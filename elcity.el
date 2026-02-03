;;; elcity.el --- Simple ElCity city builder -*- lexical-binding: t; -*-
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
;; ElCity (Emacs Lisp City) is a small, turn-based city builder for Emacs.
;; The core is deterministic and pure; the UI renders in a dedicated buffer.

;;; Code:

(require 'elcity-core)
(require 'elcity-tiles)
(require 'elcity-ui)

;;;###autoload
(defun elcity-start (&optional map-rows)
  "Start ElCity in a dedicated buffer.

MAP-ROWS is a list of strings using 1-character tokens."
  (interactive)
  (setq elcity--state (elcity-core-make-state nil nil map-rows))
  (setq elcity-ui--overlay 'none)
  (setq elcity-ui--selected-tool nil)
  (setq elcity-ui--undo-stack nil)
  (setq elcity-ui--flash nil)
  (when (bound-and-true-p elcity-ui--flash-timer)
    (cancel-timer elcity-ui--flash-timer)
    (setq elcity-ui--flash-timer nil))
  (let ((buf (get-buffer-create elcity--buffer-name)))
    (with-current-buffer buf
      (elcity-mode)
      (elcity-ui--render elcity--state))
    (pop-to-buffer buf)
    (delete-other-windows)))

(provide 'elcity)
;;; elcity.el ends here
