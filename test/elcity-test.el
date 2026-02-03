;;; elcity-test.el --- Tests for elcity core -*- lexical-binding: t; -*-
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
;; ERT tests for the ElCity functional core.

;;; Code:

(require 'ert)

(require 'test-helper)

(ert-deftest elcity-test-core-make-state-dimensions ()
  "Create a state and verify dimensions and defaults."
  (let ((state (elcity-core-make-state 40 25)))
    (should (= (elcity-state-width state) 40))
    (should (= (elcity-state-height state) 25))
    (should (= (elcity-state-funds state) 1000))
    (should (= (elcity-state-turn state) 0))
    (should (eq (elcity-tile-type (test-helper--state-at state 0 0)) 'empty))))

(ert-deftest elcity-test-core-make-state-default-water ()
  "Default map should include water tiles."
  (let ((state (elcity-core-make-state)))
    (should (eq (elcity-tile-type (test-helper--state-at state 0 3)) 'water))
    (should (eq (elcity-tile-type (test-helper--state-at state 10 0)) 'empty))))

(ert-deftest elcity-test-core-make-state-custom-map ()
  "Custom maps should set dimensions and tiles."
  (let* ((rows '(".~"
                 "P="))
         (state (elcity-core-make-state nil nil rows)))
    (should (= (elcity-state-width state) 2))
    (should (= (elcity-state-height state) 2))
    (should (eq (elcity-tile-type (test-helper--state-at state 0 0)) 'empty))
    (should (eq (elcity-tile-type (test-helper--state-at state 1 0)) 'water))
    (should (eq (elcity-tile-type (test-helper--state-at state 0 1)) 'power))
    (should (eq (elcity-tile-type (test-helper--state-at state 1 1)) 'road))))

(ert-deftest elcity-test-core-build-costs-funds ()
  "Building deducts funds and places a tile."
  (let* ((state (test-helper--state '(".. .." ".. ..")))
         (state (elcity-core-apply-build state 'road)))
    (should (= (elcity-state-funds state) (- 1000 (elcity-core-cost 'road))))
    (test-helper--assert-rows state '("== .." ".. .."))))

(ert-deftest elcity-test-core-build-result-success ()
  "Build result should report success and updated state."
  (let* ((state (test-helper--state '(".. .." ".. ..")))
         (result (elcity-core-apply-build-result state 'road))
         (next (elcity-action-result-state result)))
    (should (elcity-action-result-ok result))
    (should-not (elcity-action-result-reason result))
    (should (= (elcity-action-result-cost result) (elcity-core-cost 'road)))
    (test-helper--assert-rows next '("== .." ".. .."))))

(ert-deftest elcity-test-core-build-result-insufficient-funds ()
  "Build result should report insufficient funds."
  (let* ((state (test-helper--state '(".. .." ".. ..") :funds 0))
         (result (elcity-core-apply-build-result state 'road))
         (next (elcity-action-result-state result)))
    (should-not (elcity-action-result-ok result))
    (should (eq (elcity-action-result-reason result) 'insufficient-funds))
    (test-helper--assert-rows next '(".. .." ".. .."))))

(ert-deftest elcity-test-core-build-only-empty ()
  "Building on occupied tiles should do nothing."
  (let* ((state (test-helper--state '("== .." ".. ..")))
         (funds (elcity-state-funds state))
         (state2 (elcity-core-apply-build state 'residential)))
    (should (= (elcity-state-funds state2) funds))
    (test-helper--assert-rows state2 '("== .." ".. .."))))

(ert-deftest elcity-test-core-build-on-city-hall-blocked ()
  "Building on City Hall tiles should be blocked."
  (let* ((state (test-helper--state '("HH .." ".. ..")))
         (funds (elcity-state-funds state))
         (state2 (elcity-core-apply-build state 'road)))
    (should (= (elcity-state-funds state2) funds))
    (test-helper--assert-rows state2 '("HH .." ".. .."))))

(ert-deftest elcity-test-core-city-hall-unique ()
  "City Hall should be unique."
  (let* ((state (test-helper--state '("HH .." ".. ..") :cursor-x 1 :cursor-y 0))
         (funds (elcity-state-funds state))
         (state2 (elcity-core-apply-build state 'city-hall)))
    (should (= (elcity-state-funds state2) funds))
    (test-helper--assert-rows state2 '("HH .." ".. .."))))

(ert-deftest elcity-test-core-build-blocked-water ()
  "Building should be blocked on water tiles."
  (let* ((state (test-helper--state '("~~ .." ".. ..") :cursor-x 0 :cursor-y 0))
         (funds (elcity-state-funds state))
         (state2 (elcity-core-apply-build state 'road)))
    (should (= (elcity-state-funds state2) funds))
    (test-helper--assert-rows state2 '("~~ .." ".. .."))))

(ert-deftest elcity-test-core-demolish ()
  "Demolish should clear a tile."
  (let* ((state (test-helper--state '("== .." ".. ..")))
         (state (elcity-core-apply-demolish state)))
    (test-helper--assert-rows state '(".. .." ".. .."))))

(ert-deftest elcity-test-core-demolish-result-empty ()
  "Demolish result should report empty tiles."
  (let* ((state (test-helper--state '(".. .." ".. ..")))
         (result (elcity-core-apply-demolish-result state))
         (next (elcity-action-result-state result)))
    (should-not (elcity-action-result-ok result))
    (should (eq (elcity-action-result-reason result) 'empty))
    (test-helper--assert-rows next '(".. .." ".. .."))))

(ert-deftest elcity-test-core-demolish-result-not-demolishable ()
  "Demolish result should report non-demolishable tiles."
  (let* ((state (test-helper--state '("~~ .." ".. ..") :cursor-x 0 :cursor-y 0))
         (result (elcity-core-apply-demolish-result state))
         (next (elcity-action-result-state result)))
    (should-not (elcity-action-result-ok result))
    (should (eq (elcity-action-result-reason result) 'not-demolishable))
    (test-helper--assert-rows next '("~~ .." ".. .."))))

(ert-deftest elcity-test-core-demolish-water ()
  "Demolish should not clear water tiles."
  (let* ((state (test-helper--state '("~~ .." ".. ..") :cursor-x 0 :cursor-y 0))
         (state (elcity-core-apply-demolish state)))
    (test-helper--assert-rows state '("~~ .." ".. .."))))

(ert-deftest elcity-test-core-growth-with-power-and-road ()
  "Zones grow when powered and road-adjacent."
  (test-helper-assert-turns
   '("HH == R0 PP"
     ".. .. .. .."
     ".. .. .. ..")
   1
   '("HH == R1 PP"
     ".. .. .. .."
     ".. .. .. ..")))

(ert-deftest elcity-test-core-decay-without-power ()
  "Zones decay when not powered."
  (test-helper-assert-turns
   '("R1 .. .."
     ".. .. .."
     ".. .. ..")
   1
   '("R0 .. .."
     ".. .. .."
     ".. .. ..")))

(ert-deftest elcity-test-core-no-growth-without-road ()
  "Zones do not grow without road adjacency."
  (test-helper-assert-turns
   '("PP .. R0"
     ".. .. .."
     ".. .. ..")
   1
   '("PP .. R0"
     ".. .. .."
     ".. .. ..")))

(ert-deftest elcity-test-core-turn-increments ()
  "Simulating a turn increments the turn counter."
  (let* ((state (test-helper--state '(".. .." ".. ..") :turn 0))
         (state (elcity-core-simulate-turn state)))
    (should (= (elcity-state-turn state) 1))))

(ert-deftest elcity-test-core-grid-at ()
  "Grid access should return the expected tile."
  (let* ((state (test-helper--state '("R1 .." ".. ..")))
         (grid (elcity-state-grid state)))
    (should (eq (elcity-tile-type (elcity-core-grid-at grid 0 0)) 'residential))
    (should (eq (elcity-tile-type (elcity-core-grid-at grid 1 0)) 'empty))))

(ert-deftest elcity-test-core-empty-tile-p ()
  "Empty tile predicate should match tile content."
  (should (elcity-core-empty-tile-p (make-elcity-tile :type 'empty :level 0)))
  (should-not (elcity-core-empty-tile-p (make-elcity-tile :type 'road :level 0))))

(ert-deftest elcity-test-core-zone-pop ()
  "Zone population should reflect tile type and level."
  (should (= (elcity-core-zone-pop (make-elcity-tile :type 'residential :level 2)) 20))
  (should (= (elcity-core-zone-pop (make-elcity-tile :type 'commercial :level 3)) 0))
  (should (= (elcity-core-zone-pop (make-elcity-tile :type 'industrial :level 1)) 0))
  (should (= (elcity-core-zone-pop (make-elcity-tile :type 'road :level 0)) 0)))

(ert-deftest elcity-test-core-tile-token ()
  "Tile tokens should reflect tile definitions."
  (should (string= (elcity-core-tile-token (make-elcity-tile :type 'empty :level 0)) ".."))
  (should (string= (elcity-core-tile-token (make-elcity-tile :type 'water :level 0)) "~~"))
  (should (string= (elcity-core-tile-token (make-elcity-tile :type 'road :level 0)) "=="))
  (should (string= (elcity-core-tile-token (make-elcity-tile :type 'power :level 0)) "PP"))
  (should (string= (elcity-core-tile-token (make-elcity-tile :type 'city-hall :level 0)) "HH"))
  (should (string= (elcity-core-tile-token (make-elcity-tile :type 'residential :level 2)) "R2"))
  (should (string= (elcity-core-tile-token (make-elcity-tile :type 'commercial :level 1)) "C1"))
  (should (string= (elcity-core-tile-token (make-elcity-tile :type 'industrial :level 3)) "I3")))

(ert-deftest elcity-test-core-token-aliases ()
  "Alias tokens should parse to the expected tiles."
  (let* ((state (test-helper--state '(". ~ = P H R C I")))
         (row (elcity-state-grid state)))
    (should (eq (elcity-tile-type (aref (aref row 0) 0)) 'empty))
    (should (eq (elcity-tile-type (aref (aref row 0) 1)) 'water))
    (should (eq (elcity-tile-type (aref (aref row 0) 2)) 'road))
    (should (eq (elcity-tile-type (aref (aref row 0) 3)) 'power))
    (should (eq (elcity-tile-type (aref (aref row 0) 4)) 'city-hall))
    (should (eq (elcity-tile-type (aref (aref row 0) 5)) 'residential))
    (should (eq (elcity-tile-type (aref (aref row 0) 6)) 'commercial))
    (should (eq (elcity-tile-type (aref (aref row 0) 7)) 'industrial))
    (should (= (elcity-tile-level (aref (aref row 0) 5)) 0))
    (should (= (elcity-tile-level (aref (aref row 0) 6)) 0))
    (should (= (elcity-tile-level (aref (aref row 0) 7)) 0))))

(ert-deftest elcity-test-core-token-collision ()
  "Defining a tile with an existing token should error."
  (should-error
   (elcity-define-tile 'elcity-test-token-collision :token "..")))

(ert-deftest elcity-test-core-level-token-validation ()
  "Level tokens require a single-character token and max level."
  (should-error
   (elcity-define-tile 'elcity-test-level-missing-max
     :token "Z"
     :level-token t))
  (should-error
   (elcity-define-tile 'elcity-test-level-long-token
     :token "ZZ"
     :level-token t
     :max-level 3)))

(ert-deftest elcity-test-core-zone-issue-counts-powered-and-connected ()
  "Issue counts should reflect powered and connected zones."
  (let* ((state (test-helper--state
                 '("HH == R0 PP"
                   ".. .. R0 .."
                   ".. .. .. ..")))
         (counts (elcity-core-zone-issue-counts state)))
    (should (= (plist-get counts :unpowered) 0))
    (should (= (plist-get counts :disconnected) 1))))

(ert-deftest elcity-test-core-zone-issue-counts-no-power ()
  "Issue counts should mark zones without power."
  (let* ((state (test-helper--state
                 '("HH == R0 .."
                   ".. .. .. .."
                   ".. .. R0 ..")))
         (counts (elcity-core-zone-issue-counts state)))
    (should (= (plist-get counts :unpowered) 2))
    (should (= (plist-get counts :disconnected) 1))))

(ert-deftest elcity-test-core-build-context-power-map ()
  "Context power map should include tiles in power radius."
  (let* ((state (test-helper--state
                 '("PP .."
                   ".. ..")))
         (context (elcity-core-build-context state))
         (power (elcity-turn-context-power-map context)))
    (should (aref (aref power 0) 0))
    (should (aref (aref power 0) 1))
    (should (aref (aref power 1) 0))
    (should (aref (aref power 1) 1))))

(ert-deftest elcity-test-core-build-context-connectivity-map ()
  "Context connectivity map should mark connected road tiles."
  (let* ((state (test-helper--state
                 '("HH =="
                   ".. ..")))
         (context (elcity-core-build-context state))
         (conn (elcity-turn-context-connectivity-map context)))
    (should-not (aref (aref conn 0) 0))
    (should (aref (aref conn 0) 1))
    (should-not (aref (aref conn 1) 0))
    (should-not (aref (aref conn 1) 1))))

(ert-deftest elcity-test-core-tile-info ()
  "Tile info should include context and tile metadata."
  (let* ((state (test-helper--state
                 '("HH == R1 PP"
                   ".. == I1 .."
                   ".. .. .. ..")))
         (context (elcity-core-build-context state))
         (r-info (elcity-core-tile-info state context 2 0))
         (i-info (elcity-core-tile-info state context 2 1)))
    (should (eq (plist-get r-info :type) 'residential))
    (should (= (plist-get r-info :level) 1))
    (should (plist-get r-info :powered))
    (should (plist-get r-info :connected))
    (should (plist-get r-info :polluted))
    (should (plist-get r-info :workers))
    (should (plist-get r-info :goods))
    (should (eq (plist-get i-info :type) 'industrial))
    (should (= (plist-get i-info :level) 1))
    (should (plist-get i-info :powered))
    (should (plist-get i-info :connected))
    (should (plist-get i-info :workers))
    (should (plist-get i-info :goods))
    (should (plist-get i-info :polluted))))

(ert-deftest elcity-test-core-tile-info-traits ()
  "Tile info should include trait flags from tile definitions."
  (let* ((state (test-helper--state '("HH == R0")))
         (context (elcity-core-build-context state))
         (hall (elcity-core-tile-info state context 0 0))
         (road (elcity-core-tile-info state context 1 0))
         (res (elcity-core-tile-info state context 2 0)))
    (should-not (plist-get hall :connects))
    (should-not (plist-get hall :zone))
    (should (plist-get road :connects))
    (should-not (plist-get road :zone))
    (should-not (plist-get res :connects))
    (should (plist-get res :zone))))


(provide 'elcity-test)
;;; elcity-test.el ends here
