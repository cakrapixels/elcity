;;; test-helper.el --- Test helpers for ElCity -*- lexical-binding: t; -*-
;;; Copyright (C) 2026 Vladimir Kazanov
;;; Version: 0.1.0
;;; Package-Requires: ((emacs "30.1"))
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
;; Helper functions and macros for ElCity tests.

;;; Code:

(require 'ert)
(require 'elcity-core)
(require 'elcity-tiles)

(defun test-helper--state-at (state x y)
  "Return the tile at X,Y in STATE."
  (elcity-core-get-tile state x y))

(defun test-helper--state (rows &rest plist)
  "Create a state from ROWS with optional PLIST overrides."
  (apply #'elcity-core-state-from-rows (cons rows plist)))

(defun test-helper--assert-rows (state rows)
  "Assert that STATE matches ROWS."
  (let* ((expected-state (elcity-core-state-from-rows rows))
         (h (elcity-state-height expected-state))
         (w (elcity-state-width expected-state)))
    (should (= (elcity-state-width state) w))
    (should (= (elcity-state-height state) h))
    (dotimes (y h)
      (dotimes (x w)
        (let* ((tile (elcity-core-get-tile state x y))
               (exp (elcity-core-get-tile expected-state x y)))
          (should (eq (elcity-tile-type tile) (elcity-tile-type exp)))
          (should (= (elcity-tile-level tile) (elcity-tile-level exp))))))))

(defmacro test-helper-assert-turns (rows steps expected-rows)
  "Advance ROWS and assert EXPECTED-ROWS.

STEPS is the number of turns to advance.  ROWS and EXPECTED-ROWS are
lists of row strings as accepted by `test-helper--assert-rows'."
  `(let ((next-state (test-helper--state ,rows)))
     (dotimes (_ ,steps)
       (setq next-state (elcity-core-simulate-turn next-state)))
     (test-helper--assert-rows next-state ,expected-rows)))

(provide 'test-helper)
;;; test-helper.el ends here
