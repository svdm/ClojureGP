;; Copyright (c) Stefan A. van der Meer. All rights reserved.
;; The use and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which
;; can be found in the file epl-v10.html at the root of this distribution. By
;; using this software in any fashion, you are agreeing to be bound by the
;; terms of this license. You must not remove this notice, or any other, from
;; this software.

(ns test.cljgp.test-selection
  (:use clojure.test
        test.helpers
        cljgp.selection
        [cljgp.evaluation :only (evaluate-pop)]
        [cljgp.generate :only (generate-pop)]))

; Which individuals are selected is not predictable without random seed
; mangling, making it cumbersome to test for correctness. However,
; tournament-select uses best-fitness internally, which is tested separately and
; can be assumed valid.
(deftest test-tournament-select
  (let [tsize 5
        pop (evaluate-pop (generate-pop config-maths) config-maths)
        ind (tournament-select {:size tsize} pop)]
    (is (valid-ind? ind)
        "Result should be a valid individual")))