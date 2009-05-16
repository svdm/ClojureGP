
(ns test.cljgp.test-selection
  (:use clojure.contrib.test-is
	test.helpers
	cljgp.selection
	[cljgp.evaluation :only (evaluate-pop)]
	[cljgp.breeding :only (generate-pop)]))

; Which individuals are selected is not predictable without random seed
; mangling, making it cumbersome to test for correctness. However,
; tournament-select uses best-fitness internally, which is tested separately and
; can be assumed valid.
(deftest test-tournament-select
  (let [tsize 5
	pop (evaluate-pop (generate-pop config-maths) config-maths)
	ind (tournament-select tsize pop)]
    (is (valid-ind? ind)
	"Result should be a valid individual")))