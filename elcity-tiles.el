;;; elcity-tiles.el --- Tile definitions for ElCity -*- lexical-binding: t; -*-
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
;; Static tile definitions and their effect metadata.

;;; Code:

(require 'elcity-core)

;;; Constants

(defconst elcity-tiles-power-radius 6
  "Manhattan radius for power plant coverage.")
(defconst elcity-tiles-workers-radius 4
  "Manhattan radius for residential worker coverage.")
(defconst elcity-tiles-goods-radius 4
  "Manhattan radius for industrial goods coverage.")
(defconst elcity-tiles-pollution-radius 3
  "Manhattan radius for industrial pollution coverage.")
(defconst elcity-tiles-zone-max-level 3
  "Maximum zone level.")

;;; Tile definitions

(elcity-define-tile
 'empty
 :price 0
 :buildable t
 :demolishable nil
 :token ".."
 :token-aliases '("."))

(elcity-define-tile
 'water
 :price 0
 :buildable nil
 :demolishable nil
 :token "~~"
 :token-aliases '("~"))

(elcity-define-tile
 'road
 :price 5
 :context-effects nil
 :connects t
 :token "=="
 :token-aliases '("="))

(elcity-define-tile
 'city-hall
 :price 150
 :buildable nil
 :demolishable nil
 :unique t
 :token "HH"
 :token-aliases '("H")
 :context-effects (list (lambda (_state _context x y _tile)
                          (list (elcity-effect-connectivity-source x y)))))

(elcity-define-tile
 'power
 :price 200
 :token "PP"
 :token-aliases '("P")
 :context-effects (list (lambda (_state _context x y _tile)
                          (list (elcity-effect-power-source x y elcity-tiles-power-radius)))))

(elcity-define-tile
 'residential
 :price 20
 :token "R"
 :level-token t
 :zone t
 :max-level elcity-tiles-zone-max-level
 :pop-fn (lambda (tile) (* (elcity-tile-level tile) 10))
 :income-fn (lambda (_tile) 0)
 :context-effects (list (lambda (_state _context x y tile)
                          (when (> (elcity-tile-level tile) 0)
                            (list (elcity-effect-workers-source
                                   x y elcity-tiles-workers-radius)))))
 :state-effects (list #'elcity-tiles--zone-state-effects)
 :accounting-effects (list #'elcity-tiles--zone-accounting-effects))

(elcity-define-tile
 'commercial
 :price 25
 :token "C"
 :level-token t
 :zone t
 :max-level elcity-tiles-zone-max-level
 :income-fn (lambda (tile) (max 0 (elcity-tile-level tile)))
 :state-effects (list #'elcity-tiles--zone-state-effects)
 :accounting-effects (list #'elcity-tiles--zone-accounting-effects))

(elcity-define-tile
 'industrial
 :price 25
 :token "I"
 :level-token t
 :zone t
 :max-level elcity-tiles-zone-max-level
 :income-fn (lambda (tile) (max 0 (elcity-tile-level tile)))
 :context-effects (list (lambda (_state _context x y tile)
                          (when (> (elcity-tile-level tile) 0)
                            (list (elcity-effect-goods-source
                                   x y elcity-tiles-goods-radius)
                                  (elcity-effect-pollution-source
                                   x y elcity-tiles-pollution-radius)))))
 :state-effects (list #'elcity-tiles--zone-state-effects)
 :accounting-effects (list #'elcity-tiles--zone-accounting-effects))

;;; Zone helper functions

(defun elcity-tiles--zone-next-level (tile eligible)
  "Return next level for `elcity-tile' TILE based on ELIGIBLE."
  (let* ((lvl (elcity-tile-level tile))
         (max-level (elcity-core-tile-max-level tile)))
    (cond
     (eligible
      (min max-level (1+ lvl)))
     (t (max 0 (1- lvl))))))

(defun elcity-tiles--zone-state-effects (_state context x y tile)
  "Return a level-delta effect list for TILE at X,Y using CONTEXT.

STATE is the current `elcity-state'.
CONTEXT is a `elcity-turn-context'."
  (let* ((type (elcity-tile-type tile))
         (powered (elcity-core-context-powered-p context x y))
         (connected (elcity-core-context-connected-p context x y))
         (polluted (and (eq type 'residential)
                        (elcity-core-context-polluted-p context x y)))
         (workers (elcity-core-context-workers-p context x y))
         (goods (elcity-core-context-goods-p context x y))
         (eligible (pcase type
                     ('residential (and powered connected (not polluted)))
                     ('industrial (and powered connected workers))
                     ('commercial (and powered connected workers goods))
                     (_ (and powered connected))))
         (next (elcity-tiles--zone-next-level tile eligible))
         (delta (- next (elcity-tile-level tile))))
    ;; use power
    (when (/= delta 0)
      (list (elcity-effect-level-delta x y delta)))))



(defun elcity-tiles--zone-accounting-effects (_state _context _x _y tile)
  "Return population and income effect list for a zone TILE.

CONTEXT is a `elcity-turn-context'."
  (let* ((def (elcity-tile-def (elcity-tile-type tile)))
         (pop-fn (elcity-tile-def-pop-fn def))
         (income-fn (elcity-tile-def-income-fn def))
         (pop (if pop-fn (funcall pop-fn tile) 0))
         (income (if income-fn (funcall income-fn tile) 0)))
    (append (when (> pop 0)
              (list (elcity-effect-population pop)))
            (when (> income 0)
              (list (elcity-effect-income income))))))

(provide 'elcity-tiles)

;; Local Variables:
;; package-lint-main-file: "elcity.el"
;; End:

;;; elcity-tiles.el ends here
