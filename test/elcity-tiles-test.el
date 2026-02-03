;;; elcity-tiles-test.el --- Tests for ElCity tiles -*- lexical-binding: t; -*-
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
;; ERT tests for tile definitions and tile-driven behavior.

;;; Code:

(require 'ert)

(require 'test-helper)

(ert-deftest elcity-tiles-commercial-income ()
  "Commercial tiles should generate income after growth."
  (let* ((state (test-helper--state
                 '("HH == == == == == == =="
                   "PP R1 .. C0 .. I1 .. .."
                   ".. .. .. .. .. .. .. ..")))
         (state (elcity-core-simulate-turn state)))
    (should (= (elcity-state-funds state) 1013))
    (test-helper--assert-rows
     state
     '("HH == == == == == == =="
       "PP R2 .. C1 .. I2 .. .."
       ".. .. .. .. .. .. .. .."))))

(ert-deftest elcity-tiles-industrial-income ()
  "Industrial tiles should generate income after growth."
  (let* ((state (test-helper--state
                 '("HH == == == == == == =="
                   "PP R1 .. .. .. I0 .. .."
                   ".. .. .. .. .. .. .. ..")))
         (state (elcity-core-simulate-turn state)))
    (should (= (elcity-state-funds state) 1011))
    (test-helper--assert-rows
     state
     '("HH == == == == == == =="
       "PP R2 .. .. .. I1 .. .."
       ".. .. .. .. .. .. .. .."))))

(ert-deftest elcity-tiles-industrial-needs-workers ()
  "Industrial tiles should not grow without nearby workers."
  (let* ((state (test-helper--state
                 '("HH == == == == == == =="
                   "PP .. I0 .. .. .. .. .."
                   ".. .. .. .. .. .. .. ..")))
         (state (elcity-core-simulate-turn state)))
    (should (= (elcity-state-funds state) 1000))
    (test-helper--assert-rows
     state
     '("HH == == == == == == =="
       "PP .. I0 .. .. .. .. .."
       ".. .. .. .. .. .. .. .."))))

(ert-deftest elcity-tiles-commercial-needs-goods ()
  "Commercial tiles should not grow without nearby goods."
  (let* ((state (test-helper--state
                 '("HH == == == == == == =="
                   "PP R1 C0 .. .. .. .. .."
                   ".. .. .. .. .. .. .. ..")))
         (state (elcity-core-simulate-turn state)))
    (should (= (elcity-state-funds state) 1010))
    (test-helper--assert-rows
     state
     '("HH == == == == == == =="
       "PP R2 C0 .. .. .. .. .."
       ".. .. .. .. .. .. .. .."))))

(ert-deftest elcity-tiles-residential-pollution-blocks-growth ()
  "Residential tiles should not grow when polluted."
  (let* ((state (test-helper--state
                 '("HH == == == == == == =="
                   "PP R0 I1 .. .. .. .. .."
                   ".. .. .. .. .. .. .. ..")))
         (state (elcity-core-simulate-turn state)))
    (should (= (elcity-state-funds state) 1000))
    (test-helper--assert-rows
     state
     '("HH == == == == == == =="
       "PP R0 I0 .. .. .. .. .."
       ".. .. .. .. .. .. .. .."))))

(provide 'elcity-tiles-test)
;;; elcity-tiles-test.el ends here
