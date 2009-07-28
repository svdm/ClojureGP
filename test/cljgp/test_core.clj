;; Copyright (c) Stefan A. van der Meer. All rights reserved.
;; The use and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which
;; can be found in the file epl-v10.html at the root of this distribution. By
;; using this software in any fashion, you are agreeing to be bound by the
;; terms of this license. You must not remove this notice, or any other, from
;; this software.

(ns test.cljgp.test-core
  (:use clojure.contrib.test-is
	test.helpers
	cljgp.core
	[cljgp.generate :only [generate-pop]]))

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


