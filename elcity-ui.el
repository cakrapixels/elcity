;;; elcity-ui.el --- UI shell for ElCity -*- lexical-binding: t; -*-
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
;; Imperative UI shell for ElCity.  Renders to an Emacs buffer and handles input.
;; UI is tested only when easy and necessary; core behavior is covered by core tests.

;;; Code:

(require 'elcity-core)

;;; State

(defvar elcity--state nil
  "Current ElCity game state.")
(defvar elcity--buffer-name "*elcity*"
  "Name of the ElCity buffer.")
(defvar elcity-ui--overlay 'none
  "Current overlay mode.
One of `none', `power', `connect', `pollution', `workers', or `goods'.")
(defvar elcity-ui--selected-tool nil
  "Currently selected build tool type, or nil when none.")
(defvar elcity-ui--undo-stack nil
  "Undo stack of previous `elcity-state' values.")

(defconst elcity-ui--undo-limit 50
  "Maximum number of undo states to retain.")

(defconst elcity-ui--tool-labels
  '((residential . "Residential")
    (commercial . "Commercial")
    (industrial . "Industrial")
    (road . "Road")
    (power . "Power")
    (city-hall . "City Hall"))
  "Display labels for build tools.")

(defvar elcity-ui--crosshair t
  "When non-nil, shade the cursor row and column.")
(defvar elcity-ui--flash nil
  "Transient flash marker for the last build or demolish action.")
(defvar elcity-ui--flash-timer nil
  "Timer for clearing the transient flash marker.")

(defconst elcity-ui--flash-duration 0.18
  "Seconds to show build/demolish feedback flashes.")
(defconst elcity-ui--row-label-width 2
  "Width of map row labels in characters.")

;;; Faces

(defface elcity-cursor-face
  '((t :inverse-video t :box t :weight bold))
  "Face for the cursor tile.")

(defface elcity-crosshair-face
  '((((class color) (background dark)) :background "gray20")
    (((class color) (background light)) :background "gray90"))
  "Face for cursor crosshair shading.")

(defface elcity-empty-face
  '((((class color) (background dark)) :foreground "gray60" :background "gray12")
    (((class color) (background light)) :foreground "gray50" :background "gray95"))
  "Face for empty tiles.")

(defface elcity-road-face
  '((t :foreground "tan4" :weight bold))
  "Fallback face for roads.")

(defface elcity-road-connected-face
  '((t :foreground "tan3" :weight bold))
  "Face for connected roads.")

(defface elcity-road-disconnected-face
  '((t :foreground "gray50"))
  "Face for disconnected roads.")

(defface elcity-zone-face
  '((t :foreground "green" :box t :weight bold))
  "Fallback face for zones.")

(defface elcity-residential-l0-face
  '((t :foreground "SeaGreen4"))
  "Face for residential level 0.")

(defface elcity-residential-l1-face
  '((t :foreground "SeaGreen3"))
  "Face for residential level 1.")

(defface elcity-residential-l2-face
  '((t :foreground "SeaGreen2" :weight bold))
  "Face for residential level 2.")

(defface elcity-residential-l3-face
  '((t :foreground "SeaGreen1" :weight bold :box t))
  "Face for residential level 3.")

(defface elcity-commercial-l0-face
  '((t :foreground "SteelBlue4"))
  "Face for commercial level 0.")

(defface elcity-commercial-l1-face
  '((t :foreground "SteelBlue3"))
  "Face for commercial level 1.")

(defface elcity-commercial-l2-face
  '((t :foreground "SteelBlue2" :weight bold))
  "Face for commercial level 2.")

(defface elcity-commercial-l3-face
  '((t :foreground "SteelBlue1" :weight bold :box t))
  "Face for commercial level 3.")

(defface elcity-industrial-l0-face
  '((t :foreground "DarkGoldenrod4"))
  "Face for industrial level 0.")

(defface elcity-industrial-l1-face
  '((t :foreground "DarkGoldenrod3"))
  "Face for industrial level 1.")

(defface elcity-industrial-l2-face
  '((t :foreground "DarkGoldenrod2" :weight bold))
  "Face for industrial level 2.")

(defface elcity-industrial-l3-face
  '((t :foreground "DarkGoldenrod1" :weight bold :box t))
  "Face for industrial level 3.")

(defface elcity-power-face
  '((t :foreground "cyan4"  :box t  :weight bold))
  "Face for power plants.")

(defface elcity-city-hall-face
  '((t :foreground "orange3" :box t :weight bold))
  "Face for city hall.")

(defface elcity-water-face
  '((t :foreground "DeepSkyBlue3" :background "SkyBlue"))
  "Face for water tiles.")

(defface elcity-overlay-power-face
  '((((class color) (background dark)) :underline (:color "cyan1" :style line))
    (((class color) (background light)) :underline (:color "blue" :style line)))
  "Face for the power overlay.")

(defface elcity-overlay-connect-face
  '((((class color) (background dark)) :underline (:color "tan1" :style line))
    (((class color) (background light)) :underline (:color "tan4" :style line)))
  "Face for the connectivity overlay.")

(defface elcity-overlay-pollution-face
  '((((class color) (background dark)) :underline (:color "brown" :style line))
    (((class color) (background light)) :underline (:color "brown4" :style line)))
  "Face for the pollution overlay.")

(defface elcity-overlay-workers-face
  '((((class color) (background dark)) :underline (:color "pale green" :style line))
    (((class color) (background light)) :underline (:color "SeaGreen3" :style line)))
  "Face for the workers overlay.")

(defface elcity-overlay-goods-face
  '((((class color) (background dark)) :underline (:color "gold1" :style line))
    (((class color) (background light)) :underline (:color "goldenrod3" :style line)))
  "Face for the goods overlay.")

(defface elcity-map-frame-face
  '((t :foreground "gray50"))
  "Face for the map frame.")

(defface elcity-map-coord-face
  '((t :foreground "gray60"))
  "Face for map coordinate labels.")

(defface elcity-status-label-face
  '((t :foreground "gray60"))
  "Face for status line labels.")

(defface elcity-status-good-face
  '((t :foreground "chartreuse3" :weight bold))
  "Face for positive status values.")

(defface elcity-status-warn-face
  '((t :foreground "goldenrod2" :weight bold))
  "Face for warning status values.")

(defface elcity-status-bad-face
  '((t :foreground "IndianRed1" :weight bold))
  "Face for critical status values.")

(defface elcity-flash-build-face
  '((((class color) (background dark)) :background "DarkOliveGreen2")
    (((class color) (background light)) :background "PaleGreen1"))
  "Face for build flash feedback.")

(defface elcity-flash-demolish-face
  '((((class color) (background dark)) :background "firebrick3")
    (((class color) (background light)) :background "MistyRose1"))
  "Face for demolish flash feedback.")

;;; Rendering

(defconst elcity-ui--status-format
  "Funds: %s | Pop: %s | Income: %s | Turn: %s | Overlay: %s | Tool: %s | Unpowered: %s | Disconnected: %s | Polluted: %s\n"
  "Format string for the status line.")

(defconst elcity-ui--help-text
  (concat
   "Keys: R/C/I zone (select tool) | r road | p power | h city hall | d demolish | n next turn\n"
   "Place: SPC/RET place selected tool | u undo\n"
   "Overlay: o cycle (none/power/connectivity/pollution/workers/goods)\n"
   "Move: arrows\n"
   "Economy:\n"
   "  Build costs: Road 5, R 20, C 25, I 25, Power 200, City Hall 150\n"
   "  Income per turn: Pop/2 + (C lvl + I lvl)\n"
   "  Growth: zones gain 1 lvl if powered + road-adjacent; otherwise decay\n"
   "  Roads connect only if linked to City Hall\n"
   "  City Hall is unique and cannot be demolished\n"
   "Terrain:\n"
   "  Water tiles are not buildable or demolishable\n"
   "\n")
  "Help panel text for the UI.")

(defun elcity-ui--tile-string (tile)
  "Return the display string for TILE."
  (elcity-core-tile-token tile))

(defun elcity-ui--zone-face (type level)
  "Return a zone face for TYPE and LEVEL."
  (let* ((def (elcity-tile-def type))
         (max-level (or (and def (elcity-tile-def-max-level def)) level))
         (level (max 0 (min max-level level))))
    (pcase type
      ('residential
       (pcase level
         (0 'elcity-residential-l0-face)
         (1 'elcity-residential-l1-face)
         (2 'elcity-residential-l2-face)
         (_ 'elcity-residential-l3-face)))
      ('commercial
       (pcase level
         (0 'elcity-commercial-l0-face)
         (1 'elcity-commercial-l1-face)
         (2 'elcity-commercial-l2-face)
         (_ 'elcity-commercial-l3-face)))
      ('industrial
       (pcase level
         (0 'elcity-industrial-l0-face)
         (1 'elcity-industrial-l1-face)
         (2 'elcity-industrial-l2-face)
         (_ 'elcity-industrial-l3-face)))
      (_ 'elcity-zone-face))))

(defun elcity-ui--road-face (connected)
  "Return the road face for CONNECTED status."
  (if connected 'elcity-road-connected-face 'elcity-road-disconnected-face))

(defun elcity-ui--tile-face (info)
  "Return the display face for tile INFO."
  (let ((type (plist-get info :type))
        (level (plist-get info :level))
        (connected (plist-get info :connected)))
    (pcase type
      ('road (elcity-ui--road-face connected))
      ('residential (elcity-ui--zone-face 'residential level))
      ('commercial (elcity-ui--zone-face 'commercial level))
      ('industrial (elcity-ui--zone-face 'industrial level))
      ('power 'elcity-power-face)
      ('city-hall 'elcity-city-hall-face)
      ('water 'elcity-water-face)
      ('empty 'elcity-empty-face)
      (_ 'default))))

(defun elcity-ui--overlay-face (overlay powered connected polluted workers goods)
  "Return overlay face for OVERLAY using POWERED, CONNECTED, and POLLUTED.
WORKERS and GOODS control their respective overlay modes."
  (pcase overlay
    ('power (when powered 'elcity-overlay-power-face))
    ('connect (when connected 'elcity-overlay-connect-face))
    ('pollution (when polluted 'elcity-overlay-pollution-face))
    ('workers (when workers 'elcity-overlay-workers-face))
    ('goods (when goods 'elcity-overlay-goods-face))
    (_ nil)))

(defun elcity-ui--status-face-for-funds (funds)
  "Return a face for FUNDS in the status line."
  (cond
   ((< funds 0) 'elcity-status-bad-face)
   ((< funds 100) 'elcity-status-warn-face)
   (t 'elcity-status-good-face)))

(defun elcity-ui--status-face-for-income (income)
  "Return a face for INCOME in the status line."
  (if (< income 0) 'elcity-status-bad-face 'elcity-status-good-face))

(defun elcity-ui--status-face-for-count (count)
  "Return a face for COUNT metrics."
  (if (> count 0) 'elcity-status-bad-face 'elcity-status-label-face))

(defun elcity-ui--status-value (value face)
  "Return VALUE formatted with FACE."
  (propertize (format "%s" value) 'face face))

(defun elcity-ui--status-line (state)
  "Return a status line string for STATE."
  (let* ((metrics (elcity-core-get-display-metrics state))
         (funds (plist-get metrics :funds))
         (pop (plist-get metrics :population))
         (income (plist-get metrics :income))
         (unpowered (plist-get metrics :unpowered-zones))
         (disconnected (plist-get metrics :disconnected-zones))
         (polluted (plist-get metrics :polluted-zones))
         (overlay (elcity-ui--overlay-label elcity-ui--overlay))
         (tool (elcity-ui--selected-tool-label))
         (turn (elcity-state-turn state))
         (funds-str (elcity-ui--status-value funds (elcity-ui--status-face-for-funds funds)))
         (pop-str (elcity-ui--status-value pop 'elcity-status-label-face))
         (income-str (elcity-ui--status-value income (elcity-ui--status-face-for-income income)))
         (turn-str (elcity-ui--status-value turn 'elcity-status-label-face))
         (overlay-face (if (eq elcity-ui--overlay 'none)
                           'elcity-status-label-face
                         'elcity-status-warn-face))
         (overlay-str (elcity-ui--status-value overlay overlay-face))
         (tool-face (if elcity-ui--selected-tool
                        'elcity-status-warn-face
                      'elcity-status-label-face))
         (tool-str (elcity-ui--status-value tool tool-face))
         (unpowered-str (elcity-ui--status-value unpowered (elcity-ui--status-face-for-count unpowered)))
         (disconnected-str (elcity-ui--status-value disconnected (elcity-ui--status-face-for-count disconnected)))
         (polluted-str (elcity-ui--status-value polluted (elcity-ui--status-face-for-count polluted))))
    (format elcity-ui--status-format
            funds-str pop-str income-str turn-str overlay-str tool-str
            unpowered-str disconnected-str polluted-str)))

(defun elcity-ui--help-panel ()
  "Return a help panel string for the UI."
  elcity-ui--help-text)

(defun elcity-ui--legend-line ()
  "Return a legend line for the UI."
  (let* ((r (propertize "R" 'face (elcity-ui--zone-face 'residential 2)))
         (c (propertize "C" 'face (elcity-ui--zone-face 'commercial 2)))
         (i (propertize "I" 'face (elcity-ui--zone-face 'industrial 2)))
         (road (propertize "==" 'face 'elcity-road-connected-face))
         (p (propertize "Pwr" 'face 'elcity-overlay-power-face))
         (k (propertize "Conn" 'face 'elcity-overlay-connect-face))
         (o (propertize "Poll" 'face 'elcity-overlay-pollution-face))
         (w (propertize "Work" 'face 'elcity-overlay-workers-face))
         (g (propertize "Goods" 'face 'elcity-overlay-goods-face)))
    (concat
     "Legend: "
     r " res  "
     c " com  "
     i " ind  "
     road " road  "
     "Overlay: "
     p " "
     k " "
     o " "
     w " "
     g "\n")))

(defun elcity-ui--pretty-type (type)
  "Return a human-friendly label for TYPE."
  (capitalize (replace-regexp-in-string "-" " " (symbol-name type))))

(defun elcity-ui--tool-label (type)
  "Return a display label for tool TYPE."
  (or (cdr (assq type elcity-ui--tool-labels))
      (elcity-ui--pretty-type type)))

(defun elcity-ui--selected-tool-label ()
  "Return a display label for the selected tool."
  (if elcity-ui--selected-tool
      (elcity-ui--tool-label elcity-ui--selected-tool)
    "none"))

(defun elcity-ui--overlay-label (overlay)
  "Return a display label for OVERLAY."
  (pcase overlay
    ('none "none")
    ('power "power")
    ('connect "connect")
    ('pollution "pollution")
    ('workers "workers")
    ('goods "goods")
    (_ (symbol-name overlay))))

(defun elcity-ui--flag (value)
  "Return \"Y\" for VALUE and \"N\" otherwise."
  (if value "Y" "N"))

(defun elcity-ui--map-prefix ()
  "Return the left-side padding before the map frame."
  (make-string (1+ elcity-ui--row-label-width) ?\s))

(defun elcity-ui--col-label-line (width)
  "Return a column label line for WIDTH."
  (let ((labels (mapconcat (lambda (x) (format "%02d" x))
                           (number-sequence 0 (1- width))
                           "")))
    (concat (elcity-ui--map-prefix)
            (propertize labels 'face 'elcity-map-coord-face)
            "\n")))

(defun elcity-ui--map-border-line (width)
  "Return a map border line for WIDTH."
  (let ((line (concat "+" (make-string (* 2 width) ?-) "+")))
    (concat (elcity-ui--map-prefix)
            (propertize line 'face 'elcity-map-frame-face)
            "\n")))

(defun elcity-ui--row-label (row)
  "Return a row label string for ROW."
  (propertize (format (format "%%0%dd" elcity-ui--row-label-width) row)
              'face 'elcity-map-coord-face))

(defun elcity-ui--inspector-line (state context)
  "Return an inspector line for STATE using CONTEXT."
  (let* ((x (elcity-state-cursor-x state))
         (y (elcity-state-cursor-y state))
         (info (elcity-core-tile-info state context x y)))
    (cl-destructuring-bind (&key tile type level buildable demolishable unique
                                 cost connects zone population income
                                 powered connected polluted workers goods
                                 &allow-other-keys)
        info
      (let* ((token (elcity-core-tile-token tile))
             (label (elcity-ui--pretty-type type)))
        (concat
         (format "Cursor: (%d,%d) | Tile: %s %s | Level: %d | Build: %s | Demo: %s | Unique: %s | Connects: %s | Zone: %s | Cost: %d | Pop: %d | Inc: %d\n"
                 x y token label level
                 (elcity-ui--flag buildable)
                 (elcity-ui--flag demolishable)
                 (elcity-ui--flag unique)
                 (elcity-ui--flag connects)
                 (elcity-ui--flag zone)
                 cost population income)
         (format "Ctx: Pwr:%s Conn:%s Poll:%s Work:%s Goods:%s\n"
                 (elcity-ui--flag powered)
                 (elcity-ui--flag connected)
                 (elcity-ui--flag polluted)
                 (elcity-ui--flag workers)
                 (elcity-ui--flag goods)))))))

(defun elcity-ui--flash-at (x y face)
  "Flash FACE at X,Y for a short duration."
  (setq elcity-ui--flash (list :x x :y y :face face))
  (when elcity-ui--flash-timer
    (cancel-timer elcity-ui--flash-timer))
  (setq elcity-ui--flash-timer
        (run-at-time elcity-ui--flash-duration nil #'elcity-ui--clear-flash)))

(defun elcity-ui--clear-flash ()
  "Clear the flash marker and re-render."
  (setq elcity-ui--flash nil)
  (setq elcity-ui--flash-timer nil)
  (when elcity--state
    (elcity-ui--render elcity--state)))

(defun elcity-ui--flash-face-at (x y)
  "Return the flash face for X,Y if active."
  (when (and elcity-ui--flash
             (= x (plist-get elcity-ui--flash :x))
             (= y (plist-get elcity-ui--flash :y)))
    (plist-get elcity-ui--flash :face)))

(defun elcity-ui--render (state)
  "Render STATE into the ElCity buffer."
  (let ((inhibit-read-only t)
        (w (elcity-state-width state))
        (h (elcity-state-height state))
        (cx (elcity-state-cursor-x state))
        (cy (elcity-state-cursor-y state))
        (context (elcity-core-build-context state)))
    (erase-buffer)
    (insert (elcity-ui--status-line state))
    (insert (elcity-ui--inspector-line state context))
    (insert (elcity-ui--legend-line))
    (insert (elcity-ui--help-panel))
    (insert (elcity-ui--col-label-line w))
    (insert (elcity-ui--map-border-line w))
    (dotimes (y h)
      (insert (elcity-ui--row-label y))
      (insert (propertize "|" 'face 'elcity-map-frame-face))
      (dotimes (x w)
        (let* ((info (elcity-core-tile-info state context x y)))
          (cl-destructuring-bind (&key tile powered connected polluted workers goods
                                       &allow-other-keys)
              info
            (let* ((txt (elcity-ui--tile-string tile))
                   (base-face (elcity-ui--tile-face info))
                   (overlay-face (elcity-ui--overlay-face elcity-ui--overlay
                                                         powered connected polluted
                                                         workers goods))
                   (crosshair-face (when (and elcity-ui--crosshair
                                              (or (= x cx) (= y cy)))
                                     'elcity-crosshair-face))
                   (flash-face (elcity-ui--flash-face-at x y))
                   (cursor-face (when (and (= x cx) (= y cy))
                                  'elcity-cursor-face))
                   (face (delq nil (list base-face overlay-face crosshair-face
                                         flash-face cursor-face))))
              (insert (propertize txt 'face face))))))
      (insert (propertize "|" 'face 'elcity-map-frame-face))
      (insert "\n"))
    (insert (elcity-ui--map-border-line w))
    (goto-char (point-min))))

(defun elcity-ui--push-undo (state)
  "Push STATE onto the undo stack, trimming to `elcity-ui--undo-limit'."
  (setq elcity-ui--undo-stack (cons state elcity-ui--undo-stack))
  (when (> (length elcity-ui--undo-stack) elcity-ui--undo-limit)
    (setcdr (nthcdr (1- elcity-ui--undo-limit) elcity-ui--undo-stack) nil)))

(defun elcity-ui--apply-state (state &optional record)
  "Set UI STATE and render.

When RECORD is non-nil, push the previous state onto the undo stack."
  (when record
    (elcity-ui--push-undo elcity--state))
  (setq elcity--state state)
  (elcity-ui--render elcity--state))

(defun elcity-ui--apply (fn &rest args)
  "Apply FN with ARGS to state, then render and record history."
  (elcity-ui--apply-state (apply fn elcity--state args) t))

(defun elcity-ui--apply-no-history (fn &rest args)
  "Apply FN with ARGS to state, then render without recording history."
  (elcity-ui--apply-state (apply fn elcity--state args) nil))

(defun elcity-ui--apply-result (result)
  "Apply RESULT (an `elcity-action-result') to UI state and render."
  (elcity-ui--apply-state (elcity-action-result-state result)
                          (elcity-action-result-ok result)))

(defun elcity-ui-undo ()
  "Undo the last recorded action."
  (interactive)
  (if elcity-ui--undo-stack
      (progn
        (setq elcity--state (pop elcity-ui--undo-stack))
        (elcity-ui--render elcity--state)
        (message "Undid last action."))
    (message "Nothing to undo.")))

;;; Commands

(defun elcity-ui--select-tool (type)
  "Select build tool TYPE."
  (setq elcity-ui--selected-tool type))

(defun elcity-ui--apply-build (type &optional label)
  "Attempt to build TYPE labeled LABEL at cursor."
  (let* ((label (or label (elcity-ui--tool-label type)))
         (x (elcity-state-cursor-x elcity--state))
         (y (elcity-state-cursor-y elcity--state))
         (result (elcity-core-apply-build-result elcity--state type))
         (ok (elcity-action-result-ok result))
         (reason (elcity-action-result-reason result))
         (cost (elcity-action-result-cost result)))
    (when ok
      (elcity-ui--flash-at x y 'elcity-flash-build-face))
    (elcity-ui--apply-result result)
    (if ok
        (message "Built %s (-%d)." label cost)
      (pcase reason
        ('unique-already-exists
         (message "Cannot build %s: one already exists." label))
        ('not-buildable
         (message "Cannot build %s: tile not buildable." label))
        ('insufficient-funds
         (message "Cannot build %s: need %d, have %d."
                  label cost (elcity-state-funds elcity--state)))
        (_
         (message "Cannot build %s." label))))))

(defun elcity-ui-place-selected-tool ()
  "Place the currently selected tool at the cursor."
  (interactive)
  (if elcity-ui--selected-tool
      (elcity-ui--apply-build elcity-ui--selected-tool)
    (message "No tool selected.")))

(defun elcity-ui-build-residential ()
  "Build a residential zone at the cursor."
  (interactive)
  (elcity-ui--select-tool 'residential)
  (elcity-ui--apply-build 'residential))

(defun elcity-ui-build-commercial ()
  "Build a commercial zone at the cursor."
  (interactive)
  (elcity-ui--select-tool 'commercial)
  (elcity-ui--apply-build 'commercial))

(defun elcity-ui-build-industrial ()
  "Build an industrial zone at the cursor."
  (interactive)
  (elcity-ui--select-tool 'industrial)
  (elcity-ui--apply-build 'industrial))

(defun elcity-ui-build-road ()
  "Build a road at the cursor."
  (interactive)
  (elcity-ui--select-tool 'road)
  (elcity-ui--apply-build 'road))

(defun elcity-ui-build-power ()
  "Build a power plant at the cursor."
  (interactive)
  (elcity-ui--select-tool 'power)
  (elcity-ui--apply-build 'power))

(defun elcity-ui-build-city-hall ()
  "Build a city hall at the cursor."
  (interactive)
  (elcity-ui--select-tool 'city-hall)
  (elcity-ui--apply-build 'city-hall))

(defun elcity-ui-demolish ()
  "Demolish the tile at the cursor."
  (interactive)
  (let* ((x (elcity-state-cursor-x elcity--state))
         (y (elcity-state-cursor-y elcity--state))
         (tile (elcity-core-get-tile elcity--state x y))
         (result (elcity-core-apply-demolish-result elcity--state))
         (ok (elcity-action-result-ok result))
         (reason (elcity-action-result-reason result)))
    (when ok
      (elcity-ui--flash-at x y 'elcity-flash-demolish-face))
    (elcity-ui--apply-result result)
    (if ok
        (message "Demolished (cost 0).")
      (pcase reason
        ('empty
         (message "Nothing to demolish."))
        ('not-demolishable
         (message "Cannot demolish %s." (symbol-name (elcity-tile-type tile))))
        (_
         (message "Cannot demolish."))))))

(defun elcity-ui-next-turn ()
  "Advance the simulation by one turn."
  (interactive)
  (elcity-ui--apply #'elcity-core-simulate-turn))

(defun elcity-ui-toggle-overlay ()
  "Cycle the map overlay mode."
  (interactive)
  (setq elcity-ui--overlay
        (pcase elcity-ui--overlay
          ('none 'power)
          ('power 'connect)
          ('connect 'pollution)
          ('pollution 'workers)
          ('workers 'goods)
          (_ 'none)))
  (elcity-ui--render elcity--state)
  (message "Overlay: %s" elcity-ui--overlay))

(defun elcity-ui-move-left ()
  "Move the cursor one tile left."
  (interactive)
  (elcity-ui--apply-no-history #'elcity-core-move-cursor -1 0))

(defun elcity-ui-move-right ()
  "Move the cursor one tile right."
  (interactive)
  (elcity-ui--apply-no-history #'elcity-core-move-cursor 1 0))

(defun elcity-ui-move-up ()
  "Move the cursor one tile up."
  (interactive)
  (elcity-ui--apply-no-history #'elcity-core-move-cursor 0 -1))

(defun elcity-ui-move-down ()
  "Move the cursor one tile down."
  (interactive)
  (elcity-ui--apply-no-history #'elcity-core-move-cursor 0 1))

(defun elcity-ui-help ()
  "Show a brief help message in the minibuffer."
  (interactive)
  (message "Keys: R/C/I zone, r road, p power, h city hall, d demolish, n next turn, SPC/RET place, u undo, arrows move, o overlay"))

;;; Mode

(defvar elcity-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "R") #'elcity-ui-build-residential)
    (define-key map (kbd "C") #'elcity-ui-build-commercial)
    (define-key map (kbd "I") #'elcity-ui-build-industrial)
    (define-key map (kbd "r") #'elcity-ui-build-road)
    (define-key map (kbd "p") #'elcity-ui-build-power)
    (define-key map (kbd "h") #'elcity-ui-build-city-hall)
    (define-key map (kbd "d") #'elcity-ui-demolish)
    (define-key map (kbd "n") #'elcity-ui-next-turn)
    (define-key map (kbd "SPC") #'elcity-ui-place-selected-tool)
    (define-key map (kbd "RET") #'elcity-ui-place-selected-tool)
    (define-key map (kbd "u") #'elcity-ui-undo)
    (define-key map (kbd "o") #'elcity-ui-toggle-overlay)
    (define-key map (kbd "?") #'elcity-ui-help)
    (define-key map (kbd "<left>") #'elcity-ui-move-left)
    (define-key map (kbd "<right>") #'elcity-ui-move-right)
    (define-key map (kbd "<up>") #'elcity-ui-move-up)
    (define-key map (kbd "<down>") #'elcity-ui-move-down)
    map)
  "Keymap for `elcity-mode'.")

(define-derived-mode elcity-mode special-mode "ElCity"
  "Major mode for the simple ElCity clone."
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (setq cursor-type nil))

(provide 'elcity-ui)

;; Local Variables:
;; package-lint-main-file: "elcity.el"
;; End:

;;; elcity-ui.el ends here
