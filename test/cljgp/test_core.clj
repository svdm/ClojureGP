
(ns test.cljgp.test-core
  (:use clojure.contrib.test-is
	test.helpers
	cljgp.core
	[cljgp.breeding :only (generate-pop)]))

(deftest test-evolve-future-generations
  (let [pop-size (:population-size config-maths)
	pop (generate-pop config-maths)
	run (take 5 (evolve-future-generations pop config-maths))]
    (is (seq run))
    (is (every? #(every? valid-ind? %) run)
	"Each generation should consist only of valid individuals")
    (is (every? #(every? :fitness %) run)
	"Each generation should be fully evaluated")
    (is (every? #(= (count %) pop-size) run)
	"Each generation should be of the required size")))

; For generate-run, test-evolve-future-gens already tests the core of the
; function, otherwise it's only a combination of that and generate-pop.
; Hence, no test at this time.


