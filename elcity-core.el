;;; elcity-core.el --- Core simulation for ElCity -*- lexical-binding: t; -*-
;;; Copyright (C) 2026 Vladimir Kazanov
;;; Version: 0.1.0
;;; URL: https://github.com/vkazanov/elcity
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; Deterministic, pure core simulation logic for ElCity.
;; Core functions must remain pure state transformations and be covered by tests.
;;
;; Tiles are defined by their effects and metadata via `elcity-define-tile`
;; in `elcity-tiles.el`:
;; - Effects: :context (power/connectivity), :state-effects (mutations),
;;   :accounting-effects (income/population).
;; - Metadata: :price, :pop-fn, :income-fn.
;; Effect application order:
;; 1) Context effects build power/connectivity maps.
;; 2) State effects mutate tiles (e.g., level changes).
;; 3) Accounting effects compute income/population from updated state.

;;; Code:

(require 'cl-lib)
(require 'elcity-maps)

(cl-defstruct elcity-effect
  "Effect descriptor with TYPE and DATA payload."
  type data)
(cl-defstruct elcity-tile-def
  "Tile definition: metadata and effect generators."
  type price pop-fn income-fn context-fns state-fns accounting-fns
  buildable demolishable unique token token-aliases level-token
  connects zone max-level)
(cl-defstruct elcity-turn-context
  "Per-turn context maps for power, connectivity, and zone resources."
  power-map connectivity-map pollution-map workers-map goods-map)
(cl-defstruct elcity-tile
  "Tile with TYPE and LEVEL."
  type level)
(cl-defstruct elcity-action-result
  "Action result with STATE (`elcity-state'), OK, REASON, and COST."
  state ok reason cost)
(cl-defstruct elcity-state
  "Game state: grid, resources, turn, and cursor."
  width height grid funds turn cursor-x cursor-y
  total-population total-income)

(defconst elcity-core-map-width 40
  "Default map width in tiles.")
(defconst elcity-core-map-height 25
  "Default map height in tiles.")

(defvar elcity-core--tile-registry (make-hash-table :test 'eq)
  "Registry of tile definitions keyed by tile type symbol.")
(defvar elcity-core--token-registry (make-hash-table :test 'equal)
  "Registry of normalized tokens keyed by token string.")

;; ------------------------------
;;; Public API

(defun elcity-effect-power-source (x y radius)
  "Return a power-source effect for X,Y with RADIUS."
  (make-elcity-effect :type 'power-source
                      :data (list x y radius)))

(defun elcity-effect-pollution-source (x y radius)
  "Return a pollution-source effect for X,Y with RADIUS."
  (make-elcity-effect :type 'pollution-source
                      :data (list x y radius)))

(defun elcity-effect-workers-source (x y radius)
  "Return a workers-source effect for X,Y with RADIUS."
  (make-elcity-effect :type 'workers-source
                      :data (list x y radius)))

(defun elcity-effect-goods-source (x y radius)
  "Return a goods-source effect for X,Y with RADIUS."
  (make-elcity-effect :type 'goods-source
                      :data (list x y radius)))

(defun elcity-effect-connectivity-source (x y)
  "Return a connectivity-source effect for X,Y."
  (make-elcity-effect :type 'connectivity-source
                      :data (list x y)))

(defun elcity-effect-level-delta (x y delta)
  "Return a level-delta effect for X,Y by DELTA."
  (make-elcity-effect :type 'level-delta
                      :data (list x y delta)))

(defun elcity-effect-income (amount)
  "Return an income effect for AMOUNT."
  (make-elcity-effect :type 'income
                      :data amount))

(defun elcity-effect-population (amount)
  "Return a population effect for AMOUNT."
  (make-elcity-effect :type 'population
                      :data amount))

(defun elcity-core-state-from-rows (rows &rest plist)
  "Create an `elcity-state' from ROWS with optional PLIST overrides.

ROWS are strings of 1- or 2-character tokens defined in tile
definitions.  Default tokens include \".\"/\"..\", \"~\"/\"~~\",
\"=\"/\"==\", \"P\"/\"PP\", \"H\"/\"HH\", and \"R\"/\"C\"/\"I\" with
optional levels \"0\"-\"3\"."
  (let* ((grid (elcity-core--grid-from-rows rows))
         (h (length grid))
         (w (length (aref grid 0)))
         (state (make-elcity-state
                 :width w
                 :height h
                 :grid grid
                 :funds (or (plist-get plist :funds) 1000)
                 :turn (or (plist-get plist :turn) 0)
                 :cursor-x (or (plist-get plist :cursor-x) 0)
                 :cursor-y (or (plist-get plist :cursor-y) 0)
                 :total-population (or (plist-get plist :total-population) 0)
                 :total-income (or (plist-get plist :total-income) 0))))
    state))

(defun elcity-define-tile (type &rest plist)
  "Define and register a tile definition for TYPE using PLIST.

This function registers an `elcity-tile-def' in the tile registry.
The PLIST keys describe both metadata and effect generators:

- :price (number) build cost for TYPE.
- :pop-fn (function) \\(TILE\\) -> population contribution.
- :income-fn (function) \\(TILE\\) -> income contribution.
- :context-effects (list of functions) each \\(STATE CONTEXT X Y TILE\\)
  -> list of `elcity-effect' values that build the turn context
  (power/connectivity/workers/goods/pollution).
- :state-effects (list of functions) each \\(STATE CONTEXT X Y TILE\\)
  -> list of `elcity-effect' values that mutate state (e.g., level changes).
- :accounting-effects (list of functions) each \\(STATE CONTEXT X Y TILE\\)
  -> list of `elcity-effect' values for income/population accounting.
- :buildable (bool) non-nil means building is allowed on this tile.
- :demolishable (bool) non-nil means the tile can be demolished.
- :unique (bool) non-nil means only one tile of TYPE may exist.
- :token (string) canonical map/render token for TYPE.
- :token-aliases (list) additional tokens accepted in maps.
- :level-token (bool) non-nil means tokens encode level (e.g., \"R2\").
- :connects (bool) non-nil means this tile propagates connectivity.
- :zone (bool) non-nil means this tile is counted as a zone.
- :max-level (number) maximum level for tiles of TYPE.

Callback parameters:
- STATE: current `elcity-state'.
- CONTEXT: current `elcity-turn-context' (power/connectivity maps).
- X, Y: tile coordinates (0-based).
- TILE: current `elcity-tile' at X,Y.

Effect constructors include `elcity-effect-power-source',
`elcity-effect-connectivity-source', `elcity-effect-pollution-source',
`elcity-effect-workers-source', `elcity-effect-goods-source',
`elcity-effect-level-delta', `elcity-effect-income', and
`elcity-effect-population'."
  (let* ((token (plist-get plist :token))
         (token-aliases (plist-get plist :token-aliases))
         (level-token (plist-get plist :level-token))
         (max-level (plist-get plist :max-level))
         (tile-def (make-elcity-tile-def
                    :type type
                    :price (plist-get plist :price)
                    :pop-fn (plist-get plist :pop-fn)
                    :income-fn (plist-get plist :income-fn)
                    :context-fns (plist-get plist :context-effects)
                    :state-fns (plist-get plist :state-effects)
                    :accounting-fns (plist-get plist :accounting-effects)
                    :buildable (plist-get plist :buildable)
                    :demolishable (if (plist-member plist :demolishable)
                                      (plist-get plist :demolishable)
                                    t)
                    :unique (plist-get plist :unique)
                    :token token
                    :token-aliases token-aliases
                    :level-token level-token
                    :connects (plist-get plist :connects)
                    :zone (plist-get plist :zone)
                    :max-level max-level)))
    (elcity-core--register-tile-tokens type token token-aliases level-token max-level)
    (puthash type tile-def elcity-core--tile-registry)
    tile-def))

(defun elcity-tile-def (type)
  "Return the tile definition for TYPE, or nil if missing."
  (gethash type elcity-core--tile-registry))

(defun elcity-core-make-state (&optional width height map-rows)
  "Create a new game state with optional WIDTH, HEIGHT, and MAP-ROWS.

MAP-ROWS is a list of strings using 1-character tokens."
  (let* ((w (or width elcity-core-map-width))
         (h (or height elcity-core-map-height))
         (map (or map-rows
                  (and (= w elcity-core-map-width)
                       (= h elcity-core-map-height)
                       elcity-map-default-rows)))
         grid)
    (if map
        (let* ((parsed (elcity-core--grid-from-rows map))
               (mh (length parsed))
               (mw (length (aref parsed 0))))
          (when (and width (/= width mw))
            (error "Map width mismatch: %d (expected %d)" mw width))
          (when (and height (/= height mh))
            (error "Map height mismatch: %d (expected %d)" mh height))
          (setq w mw
                h mh
                grid parsed))
      (setq grid (make-vector h nil))
      (dotimes (y h)
        (let ((row (make-vector w nil)))
          (dotimes (x w)
            (aset row x (elcity-core--make-tile 'empty 0)))
          (aset grid y row))))
    (make-elcity-state
     :width w
     :height h
     :grid grid
     :funds 1000
     :turn 0
     :cursor-x 0
     :cursor-y 0
     :total-population 0
     :total-income 0)))

(defun elcity-core-get-tile (state x y)
  "Return the tile at X,Y in `elcity-state' STATE."
  (elcity-core--grid-at (elcity-state-grid state) x y))

(defun elcity-core-set-tile (state x y tile)
  "Return a new `elcity-state' STATE with `elcity-tile' TILE at X,Y."
  (let* ((new-grid (elcity-core--grid-set (elcity-state-grid state) x y tile))
         (new (copy-elcity-state state)))
    (setf (elcity-state-grid new) new-grid)
    new))

(defun elcity-core-cost (type)
  "Return build cost for TYPE."
  (let* ((def (elcity-tile-def type))
         (price (and def (elcity-tile-def-price def))))
    (or price 0)))

(defun elcity-core-grid-at (grid x y)
  "Return the `elcity-tile' at GRID coordinate X,Y."
  (elcity-core--grid-at grid x y))

(defun elcity-core-empty-tile-p (tile)
  "Return non-nil if `elcity-tile' TILE is empty."
  (elcity-core--empty-tile-p tile))

(defun elcity-core-tile-buildable-p (tile)
  "Return non-nil if `elcity-tile' TILE can be built on."
  (let* ((def (elcity-tile-def (elcity-tile-type tile)))
         (flag (and def (elcity-tile-def-buildable def))))
    flag))

(defun elcity-core-tile-demolishable-p (tile)
  "Return non-nil if `elcity-tile' TILE can be demolished."
  (let* ((def (elcity-tile-def (elcity-tile-type tile)))
         (flag (and def (elcity-tile-def-demolishable def))))
    flag))

(defun elcity-core-zone-pop (tile)
  "Return population contribution for `elcity-tile' TILE."
  (let* ((def (elcity-tile-def (elcity-tile-type tile)))
         (pop-fn (and def (elcity-tile-def-pop-fn def))))
    (if pop-fn
        (funcall pop-fn tile)
      0)))

(defun elcity-core-tile-token (tile)
  "Return the canonical token for `elcity-tile' TILE."
  (let* ((def (elcity-tile-def (elcity-tile-type tile)))
         (token (and def (elcity-tile-def-token def))))
    (cond
     ((and def (elcity-tile-def-level-token def) token)
      (format "%s%d" token (elcity-tile-level tile)))
     (token token)
     (t "??"))))

(defun elcity-core-tile-info (state context x y)
  "Return a plist of tile info for `elcity-state' STATE at X,Y using CONTEXT.

CONTEXT is an `elcity-turn-context'.  When CONTEXT is nil, it is
computed via `elcity-core-build-context'.  The plist includes:
  :tile, :type, :level, :buildable, :demolishable, :unique, :cost,
  :connects, :zone, :population, :income, :powered, :connected, :polluted,
  :workers, :goods."
  (let* ((context (or context (elcity-core-build-context state)))
         (tile (elcity-core-get-tile state x y))
         (type (elcity-tile-type tile))
         (def (elcity-tile-def type))
         (pop-fn (and def (elcity-tile-def-pop-fn def)))
         (income-fn (and def (elcity-tile-def-income-fn def)))
         (pop (if pop-fn (funcall pop-fn tile) 0))
         (income (if income-fn (funcall income-fn tile) 0))
         (power-map (elcity-turn-context-power-map context))
         (pollution-map (elcity-turn-context-pollution-map context))
         (workers-map (elcity-turn-context-workers-map context))
         (goods-map (elcity-turn-context-goods-map context))
         (connect-map (elcity-turn-context-connectivity-map context))
         (connected (or (aref (aref connect-map y) x)
                        (elcity-core-context-connected-p context x y))))
    (list :tile tile
          :type type
          :level (elcity-tile-level tile)
          :buildable (elcity-core-tile-buildable-p tile)
          :demolishable (elcity-core-tile-demolishable-p tile)
          :unique (elcity-core-tile-unique-p type)
          :connects (elcity-core-tile-connects-p tile)
          :zone (elcity-core-tile-zone-p tile)
          :cost (elcity-core-cost type)
          :population pop
          :income income
          :powered (aref (aref power-map y) x)
          :connected connected
          :polluted (aref (aref pollution-map y) x)
          :workers (aref (aref workers-map y) x)
          :goods (aref (aref goods-map y) x))))

(defun elcity-core-tile-connects-p (tile)
  "Return non-nil when `elcity-tile' TILE propagates connectivity."
  (let ((def (elcity-tile-def (elcity-tile-type tile))))
    (and def (elcity-tile-def-connects def))))

(defun elcity-core-tile-zone-p (tile)
  "Return non-nil when `elcity-tile' TILE is a zone."
  (let ((def (elcity-tile-def (elcity-tile-type tile))))
    (and def (elcity-tile-def-zone def))))

(defun elcity-core-tile-max-level (tile)
  "Return the max level for `elcity-tile' TILE.

When a tile definition does not specify a max level, return the current
tile level."
  (let* ((def (elcity-tile-def (elcity-tile-type tile)))
         (max-level (and def (elcity-tile-def-max-level def))))
    (if (numberp max-level)
        max-level
      (elcity-tile-level tile))))

(defun elcity-core-tile-unique-p (type)
  "Return non-nil when tile TYPE is marked unique in its definition."
  (let ((def (elcity-tile-def type)))
    (and def (elcity-tile-def-unique def))))

(defun elcity-core-has-tile-type-p (state type)
  "Return non-nil when TYPE appears in `elcity-state' STATE."
  (let* ((h (elcity-state-height state))
         (w (elcity-state-width state))
         (grid (elcity-state-grid state))
         (found nil))
    (dotimes (y h)
      (dotimes (x w)
        (when (eq (elcity-tile-type (aref (aref grid y) x)) type)
          (setq found t))))
    found))

(defun elcity-core-context-powered-p (context x y)
  "Return non-nil if `elcity-turn-context' CONTEXT indicates power at X,Y."
  (aref (aref (elcity-turn-context-power-map context) y) x))

(defun elcity-core-context-connected-p (context x y)
  "Return non-nil if `elcity-turn-context' CONTEXT indicates connectivity at X,Y."
  (let ((conn (elcity-turn-context-connectivity-map context)))
    (cl-some (lambda (p)
               (let ((nx (car p)) (ny (cdr p)))
                 (and (>= nx 0) (>= ny 0)
                      (< nx (length (aref conn 0)))
                      (< ny (length conn))
                      (aref (aref conn ny) nx))))
             (elcity-core--neighbors x y))))

(defun elcity-core-context-polluted-p (context x y)
  "Return non-nil if `elcity-turn-context' CONTEXT indicates pollution at X,Y."
  (aref (aref (elcity-turn-context-pollution-map context) y) x))

(defun elcity-core-context-workers-p (context x y)
  "Return non-nil if `elcity-turn-context' CONTEXT indicates workers at X,Y."
  (aref (aref (elcity-turn-context-workers-map context) y) x))

(defun elcity-core-context-goods-p (context x y)
  "Return non-nil if `elcity-turn-context' CONTEXT indicates goods at X,Y."
  (aref (aref (elcity-turn-context-goods-map context) y) x))

(defun elcity-core-build-context (state)
  "Return an `elcity-turn-context' for `elcity-state' STATE."
  (let ((context-effects (elcity-core--collect-effects state nil 'context)))
    (elcity-core--build-context state context-effects)))

(defun elcity-core-zone-issue-counts (state)
  "Return issue counts for zones in `elcity-state' STATE.

The return value is a plist with counts for `:unpowered',
`:disconnected', and `:polluted'."
  (let* ((context (elcity-core-build-context state))
         (w (elcity-state-width state))
         (h (elcity-state-height state))
         (unpowered 0)
         (disconnected 0)
         (polluted 0))
    (dotimes (y h)
      (dotimes (x w)
        (let ((tile (elcity-core-get-tile state x y)))
          (when (elcity-core-tile-zone-p tile)
            (unless (elcity-core-context-powered-p context x y)
              (cl-incf unpowered))
            (unless (elcity-core-context-connected-p context x y)
              (cl-incf disconnected))
            (when (elcity-core-context-polluted-p context x y)
              (cl-incf polluted))))))
    (list :unpowered unpowered :disconnected disconnected :polluted polluted)))

(defun elcity-core-get-display-metrics (state)
  "Return a plist of display metrics for `elcity-state' STATE.
The plist contains:
  :funds (number) Current funds.
  :population (number) Total population.
  :income (number) Expected income for next turn.
  :unpowered-zones (number) Count of unpowered zones.
  :disconnected-zones (number) Count of disconnected zones.
  :polluted-zones (number) Count of polluted zones."
  (let* ((funds (elcity-state-funds state))
         (total-population (elcity-state-total-population state))
         (total-income (elcity-state-total-income state))
         (issue-counts (elcity-core-zone-issue-counts state)))
    (list :funds funds
          :population total-population
          :income total-income
          :unpowered-zones (plist-get issue-counts :unpowered)
          :disconnected-zones (plist-get issue-counts :disconnected)
          :polluted-zones (plist-get issue-counts :polluted))))

(defun elcity-core-simulate-turn (state)
  "Advance `elcity-state' STATE by one turn and return the new state."
  (let* ((context-effects (elcity-core--collect-effects state nil 'context))
         (context (elcity-core--build-context state context-effects))
         (state-effects (elcity-core--collect-effects state context 'state))
         (state2 (elcity-core--apply-state-effects state state-effects))
         (accounting-effects (elcity-core--collect-effects state2 context 'accounting))
         (state3 (elcity-core--apply-accounting-effects state2 accounting-effects)))
    (setf (elcity-state-turn state3) (1+ (elcity-state-turn state)))
    state3))

(defun elcity-core-apply-build-result (state type)
  "Attempt to build TYPE at the cursor in `elcity-state' STATE.

Return an `elcity-action-result' with:
- STATE: resulting `elcity-state'.
- OK: non-nil on success.
- REASON: one of `unique-already-exists', `not-buildable', or
  `insufficient-funds' on failure.
- COST: build cost for TYPE."
  (let* ((x (elcity-state-cursor-x state))
         (y (elcity-state-cursor-y state))
         (tile (elcity-core-get-tile state x y))
         (cost (elcity-core-cost type))
         (funds (elcity-state-funds state)))
    (cond
     ((and (elcity-core-tile-unique-p type)
           (elcity-core-has-tile-type-p state type))
      (make-elcity-action-result
       :state state :ok nil :reason 'unique-already-exists :cost cost))
     ((not (elcity-core-tile-buildable-p tile))
      (make-elcity-action-result
       :state state :ok nil :reason 'not-buildable :cost cost))
     ((< funds cost)
      (make-elcity-action-result
       :state state :ok nil :reason 'insufficient-funds :cost cost))
     (t
      (let* ((new-tile (elcity-core--make-tile type 0))
             (new-grid (elcity-core--grid-set (elcity-state-grid state) x y new-tile))
             (new-funds (- funds cost))
             (new (copy-elcity-state state)))
        (setf (elcity-state-grid new) new-grid)
        (setf (elcity-state-funds new) new-funds)
        (make-elcity-action-result
         :state new :ok t :reason nil :cost cost))))))

(defun elcity-core-apply-build (state type)
  "Attempt to build TYPE at the cursor in `elcity-state' STATE.

Return the resulting `elcity-state'.  Use
`elcity-core-apply-build-result' for structured details."
  (elcity-action-result-state (elcity-core-apply-build-result state type)))

(defun elcity-core-apply-demolish-result (state)
  "Demolish the tile under the cursor in `elcity-state' STATE.

Return an `elcity-action-result' with:
- STATE: resulting `elcity-state'.
- OK: non-nil on success.
- REASON: one of `empty' or `not-demolishable' on failure.
- COST: demolish cost (currently 0)."
  (let* ((x (elcity-state-cursor-x state))
         (y (elcity-state-cursor-y state))
         (tile (elcity-core-get-tile state x y))
         (cost 0))
    (cond
     ((elcity-core-empty-tile-p tile)
      (make-elcity-action-result
       :state state :ok nil :reason 'empty :cost cost))
     ((not (elcity-core-tile-demolishable-p tile))
      (make-elcity-action-result
       :state state :ok nil :reason 'not-demolishable :cost cost))
     (t
      (let* ((new-tile (elcity-core--make-tile 'empty 0))
             (new-grid (elcity-core--grid-set (elcity-state-grid state) x y new-tile))
             (new (copy-elcity-state state)))
        (setf (elcity-state-grid new) new-grid)
        (make-elcity-action-result
         :state new :ok t :reason nil :cost cost))))))

(defun elcity-core-apply-demolish (state)
  "Demolish the tile under the cursor in `elcity-state' STATE.

Return the resulting `elcity-state'.  Use
`elcity-core-apply-demolish-result' for structured details."
  (elcity-action-result-state (elcity-core-apply-demolish-result state)))

(defun elcity-core-move-cursor (state dx dy)
  "Move the cursor in `elcity-state' STATE by DX,DY and return new state."
  (let* ((x (elcity-state-cursor-x state))
         (y (elcity-state-cursor-y state))
         (nx (max 0 (min (1- (elcity-state-width state)) (+ x dx))))
         (ny (max 0 (min (1- (elcity-state-height state)) (+ y dy))))
         (new (copy-elcity-state state)))
    (setf (elcity-state-cursor-x new) nx)
    (setf (elcity-state-cursor-y new) ny)
    new))

;; ------------------------------
;;; Internal helpers

(defun elcity-core--make-tile (type &optional level)
  "Create an `elcity-tile' with TYPE and optional LEVEL."
  (make-elcity-tile :type type :level (or level 0)))

(defun elcity-core--empty-tile-p (tile)
  "Return non-nil if `elcity-tile' TILE is empty."
  (eq (elcity-tile-type tile) 'empty))

(defun elcity-core--copy-grid (grid)
  "Return a deep copy of GRID of `elcity-tile' values."
  (let* ((h (length grid))
         (new (make-vector h nil)))
    (dotimes (y h)
      (let* ((row (aref grid y))
             (w (length row))
             (new-row (make-vector w nil)))
        (dotimes (x w)
          (let ((tile (aref row x)))
            (aset new-row x (elcity-core--make-tile (elcity-tile-type tile)
                                                    (elcity-tile-level tile)))))
        (aset new y new-row)))
    new))

(defun elcity-core--grid-at (grid x y)
  "Return the `elcity-tile' at GRID coordinate X,Y."
  (aref (aref grid y) x))

(defun elcity-core--grid-set (grid x y tile)
  "Return a new GRID with `elcity-tile' TILE placed at X,Y."
  (let* ((row (aref grid y))
         (new-row (copy-sequence row)))
    (aset new-row x tile)
    (let ((new-grid (copy-sequence grid)))
      (aset new-grid y new-row)
      new-grid)))

(defun elcity-core--parse-row (row)
  "Parse ROW into a list of tokens."
  (if (string-match-p "[[:space:]]" row)
      (let ((tokens (split-string row " +" t)))
        (dolist (tok tokens)
          (unless (memq (length tok) '(1 2))
            (error "Row tokens must be 1 or 2 characters: %s" tok)))
        tokens)
    (mapcar #'char-to-string (string-to-list row))))

(defun elcity-core--token-normalize (token)
  "Return TOKEN uppercased for matching."
  (upcase token))

(defun elcity-core--register-tile-tokens (type token aliases level-token max-level)
  "Register tile TOKENS for TYPE using TOKEN, ALIASES, LEVEL-TOKEN, and MAX-LEVEL."
  (let ((tokens (elcity-core--collect-tile-tokens type token aliases level-token max-level)))
    (dolist (tok tokens)
      (let ((existing (gethash tok elcity-core--token-registry)))
        (when (and existing (not (eq existing type)))
          (error "Token collision: %s already registered to %s" tok existing))))
    (dolist (tok tokens)
      (puthash tok type elcity-core--token-registry))))

(defun elcity-core--collect-tile-tokens (type token aliases level-token max-level)
  "Return normalized tokens for TYPE from TOKEN, ALIASES, and LEVEL-TOKEN.
MAX-LEVEL controls generated level tokens."
  (let ((raw (append (when token (list token)) aliases))
        (seen (make-hash-table :test 'equal))
        tokens)
    (when level-token
      (unless (and token (= (length token) 1))
        (error "Level token requires single-character :token for %s" type))
      (unless (numberp max-level)
        (error "Level token requires :max-level for %s" type))
      (dotimes (level (1+ max-level))
        (push (format "%s%d" token level) raw)))
    (dolist (tok raw)
      (unless (memq (length tok) '(1 2))
        (error "Token length must be 1 or 2 characters: %s" tok))
      (let ((norm (elcity-core--token-normalize tok)))
        (unless (gethash norm seen)
          (puthash norm t seen)
          (push norm tokens))))
    (nreverse tokens)))

(defun elcity-core--tile-defs ()
  "Return a list of tile definitions from the registry."
  (let (defs)
    (maphash (lambda (_key def) (push def defs)) elcity-core--tile-registry)
    defs))

(defun elcity-core--token-matches-def-p (token def)
  "Return non-nil when TOKEN matches DEF token or aliases."
  (let* ((token (elcity-core--token-normalize token))
         (canonical (elcity-tile-def-token def))
         (aliases (elcity-tile-def-token-aliases def)))
    (or (and canonical
             (string= token (elcity-core--token-normalize canonical)))
        (cl-some (lambda (alias)
                   (string= token (elcity-core--token-normalize alias)))
                 aliases))))

(defun elcity-core--tile-from-level-token (token defs)
  "Return a tile for level TOKEN using DEFS, or nil if not a match."
  (let ((token (elcity-core--token-normalize token))
        (found nil))
    (dolist (def defs)
      (when (and (not found) (elcity-tile-def-level-token def))
        (let* ((prefix (elcity-tile-def-token def))
               (prefix (and prefix (elcity-core--token-normalize prefix))))
          (when prefix
            (cond
             ((string= token prefix)
              (setq found (elcity-core--make-tile (elcity-tile-def-type def) 0)))
             ((string-match (format "^%s\\([0-9]\\)$" (regexp-quote prefix)) token)
              (let* ((level (string-to-number (match-string 1 token)))
                     (max-level (elcity-tile-def-max-level def)))
                (when (and (numberp max-level) (> level max-level))
                  (error "Level token out of range: %s" token))
                (setq found (elcity-core--make-tile (elcity-tile-def-type def) level)))))))))
    found))

(defun elcity-core--tile-from-token (tok &optional defs)
  "Return a tile for TOK.

DEFS is an optional list of tile definitions to use."
  (let* ((defs (or defs (elcity-core--tile-defs)))
         (level-tile (elcity-core--tile-from-level-token tok defs)))
    (cond
     (level-tile level-tile)
     (t
      (let ((def (cl-find-if (lambda (d) (elcity-core--token-matches-def-p tok d))
                             defs)))
        (if def
            (elcity-core--make-tile (elcity-tile-def-type def) 0)
          (error "Unknown token: %s" tok)))))))

(defun elcity-core--grid-from-rows (rows)
  "Return a grid parsed from ROWS.

ROWS are strings of 1- or 2-character tokens."
  (let* ((parsed (mapcar #'elcity-core--parse-row rows))
         (defs (elcity-core--tile-defs))
         (h (length parsed))
         (w (length (car parsed)))
         (grid (make-vector h nil)))
    (dotimes (y h)
      (let ((row (make-vector w nil))
            (tokens (nth y parsed)))
        (when (/= (length tokens) w)
          (error "Row width mismatch at %d: %d (expected %d)" y (length tokens) w))
        (dotimes (x w)
          (aset row x (elcity-core--tile-from-token (nth x tokens) defs)))
        (aset grid y row)))
    grid))

(defun elcity-core--in-bounds-p (state x y)
  "Return non-nil if X,Y is within `elcity-state' STATE bounds."
  (and (>= x 0) (>= y 0)
       (< x (elcity-state-width state))
       (< y (elcity-state-height state))))

(defun elcity-core--neighbors (x y)
  "Return orthogonal neighbor coordinates of X,Y."
  (list (cons x (1- y))
        (cons x (1+ y))
        (cons (1- x) y)
        (cons (1+ x) y)))

(defun elcity-core--radius-map-from-effects (state effects effect-type)
  "Build a radius-based map for `elcity-state' STATE from EFFECTS of `effect-type'.
EFFECT-TYPE is a symbol like `power-source' or `pollution-source'."
  (let* ((w (elcity-state-width state))
         (h (elcity-state-height state))
         (map-grid (make-vector h nil)))
    (dotimes (y h)
      (aset map-grid y (make-vector w nil)))
    (dolist (eff effects)
      (when (eq (elcity-effect-type eff) effect-type)
        (pcase-let ((`(,x ,y ,radius) (elcity-effect-data eff)))
          (dotimes (dy (1+ (* 2 radius)))
            (dotimes (dx (1+ (* 2 radius)))
              (let* ((nx (+ x (- dx radius)))
                     (ny (+ y (- dy radius)))
                     (dist (+ (abs (- x nx)) (abs (- y ny)))))
                (when (and (>= nx 0) (>= ny 0)
                           (< nx w) (< ny h)
                           (<= dist radius))
                  (aset (aref map-grid ny) nx t))))))))
    map-grid))

(defun elcity-core--connects-tile-p (tile)
  "Return non-nil if TILE propagates connectivity."
  (elcity-core-tile-connects-p tile))

(defun elcity-core--connectivity-map-from-effects (state effects)
  "Build a connectivity map for `elcity-state' STATE from EFFECTS.

Connectivity is derived from `connectivity-source' effects and spreads
through adjacent tiles marked with `:connects'."
  (let* ((w (elcity-state-width state))
         (h (elcity-state-height state))
         (conn (make-vector h nil))
         (grid (elcity-state-grid state))
         (queue '()))
    (dotimes (y h)
      (aset conn y (make-vector w nil)))
    (dolist (eff effects)
      (when (eq (elcity-effect-type eff) 'connectivity-source)
        (pcase-let ((`(,x ,y) (elcity-effect-data eff)))
          (when (and (>= x 0) (>= y 0) (< x w) (< y h))
            (let ((source-tile (elcity-core--grid-at grid x y)))
              (if (elcity-core--connects-tile-p source-tile)
                  (progn
                    (aset (aref conn y) x t)
                    (push (cons x y) queue))
                (dolist (pos (elcity-core--neighbors x y))
                  (let ((nx (car pos))
                        (ny (cdr pos)))
                    (when (and (>= nx 0) (>= ny 0)
                               (< nx w) (< ny h))
                      (let ((neighbor (elcity-core--grid-at grid nx ny)))
                        (when (elcity-core--connects-tile-p neighbor)
                          (aset (aref conn ny) nx t)
                          (push (cons nx ny) queue))))))))))))
    (while queue
      (pcase-let ((`(,x . ,y) (pop queue)))
        (dolist (pos (elcity-core--neighbors x y))
          (let ((nx (car pos))
                (ny (cdr pos)))
            (when (and (>= nx 0) (>= ny 0)
                       (< nx w) (< ny h)
                       (not (aref (aref conn ny) nx)))
              (let ((tile (elcity-core--grid-at grid nx ny)))
                (when (elcity-core--connects-tile-p tile)
                  (aset (aref conn ny) nx t)
                  (push (cons nx ny) queue))))))))
    conn))

(defun elcity-core--build-context (state effects)
  "Build an `elcity-turn-context' from `elcity-state' STATE and EFFECTS."
  (make-elcity-turn-context
   :power-map (elcity-core--radius-map-from-effects state effects 'power-source)
   :connectivity-map (elcity-core--connectivity-map-from-effects state effects)
   :pollution-map (elcity-core--radius-map-from-effects state effects 'pollution-source)
   :workers-map (elcity-core--radius-map-from-effects state effects 'workers-source)
   :goods-map (elcity-core--radius-map-from-effects state effects 'goods-source)))

(defun elcity-core--tile-def-or-empty (type)
  "Return the `elcity-tile-def' for TYPE or the empty tile definition."
  (or (elcity-tile-def type) (elcity-tile-def 'empty)))

(defun elcity-core--collect-effects (state context phase)
  "Collect effects for PHASE from `elcity-state' STATE using CONTEXT.

CONTEXT is a `elcity-turn-context'."
  (let (effects)
    (dotimes (y (elcity-state-height state))
      (dotimes (x (elcity-state-width state))
        (let* ((tile (elcity-core-get-tile state x y))
               (def (elcity-core--tile-def-or-empty (elcity-tile-type tile)))
               (fns (pcase phase
                      ('context (elcity-tile-def-context-fns def))
                      ('state (elcity-tile-def-state-fns def))
                      ('accounting (elcity-tile-def-accounting-fns def))
                      (_ nil))))
          (dolist (fn fns)
            (dolist (effect (funcall fn state context x y tile))
              (push effect effects))))))
    (nreverse effects)))

(defun elcity-core--apply-state-effects (state effects)
  "Apply state EFFECTS to `elcity-state' STATE and return the new state."
  (let* ((new (copy-elcity-state state))
         (grid (elcity-core--copy-grid (elcity-state-grid state))))
    (dolist (eff effects)
      (when (eq (elcity-effect-type eff) 'level-delta)
        (pcase-let ((`(,x ,y ,delta) (elcity-effect-data eff)))
          (let* ((tile (elcity-core--grid-at grid x y))
                 (max-level (elcity-core-tile-max-level tile))
                 (new-level (max 0 (min max-level
                                        (+ (elcity-tile-level tile) delta)))))
            (setf (elcity-tile-level tile) new-level)))))
    (setf (elcity-state-grid new) grid)
    new))

(defun elcity-core--apply-accounting-effects (state effects)
  "Apply accounting EFFECTS to `elcity-state' STATE and return the new state."
  (let ((pop-from-effects 0) ;; Renamed to avoid confusion with total-population
        (income-from-effects 0) ;; Renamed to avoid confusion with total-income
        (new (copy-elcity-state state)))
    (dolist (eff effects)
      (pcase (elcity-effect-type eff)
        ('population (setq pop-from-effects (+ pop-from-effects (elcity-effect-data eff))))
        ('income (setq income-from-effects (+ income-from-effects (elcity-effect-data eff))))))

    (let* ((funds (elcity-state-funds new))
           (calculated-income-for-funds (+ (/ pop-from-effects 2) income-from-effects))
           (new-funds (+ funds calculated-income-for-funds)))
      (setf (elcity-state-funds new) new-funds)
      ;; Update the new fields in the state
      (setf (elcity-state-total-population new) pop-from-effects)
      (setf (elcity-state-total-income new) calculated-income-for-funds) ;; Store the calculated tax as total income
      new)))

;; ------------------------------

(provide 'elcity-core)

;; Local Variables:
;; package-lint-main-file: "elcity.el"
;; End:

;;; elcity-core.el ends here
