;;; elcity-maps.el --- Map presets for ElCity -*- lexical-binding: t; -*-
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
;; Visual map presets for ElCity.  Each row uses 1-character tokens
;; supported by tile definitions (see `elcity-define-tile`).

;;; Code:

(defconst elcity-map-default-rows
  '("........................................"
    "........................................"
    "........................................"
    "~~~~~~~~................................"
    "~~~~~~~~~..............................."
    "~~~~~~~~................................"
    "~~~~~~.................................."
    "........................................"
    "........................................"
    "........................................"
    "..................H....................."
    "........~~~~~..........................."
    "~~~~~~~~~~~~~~~~~~~~~..................."
    ".....~~~~~~~~~~~~~~~~~~~~..............."
    "..........~~~~~~~~~~~~~~~~~~~..........."
    "..................~~~~~~~~~~~~~~~~......"
    "........................................"
    "........................................"
    "........................................"
    "........................................"
    "........................................"
    "........................................"
    "........................................"
    "........................................"
    "........................................")
  "Default map layout as rows of 1-character tokens.")

(provide 'elcity-maps)

;; Local Variables:
;; package-lint-main-file: "elcity.el"
;; End:

;;; elcity-maps.el ends here
