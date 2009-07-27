;; Copyright (c) Stefan A. van der Meer. All rights reserved.
;; The use and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which
;; can be found in the file epl-v10.html at the root of this distribution. By
;; using this software in any fashion, you are agreeing to be bound by the
;; terms of this license. You must not remove this notice, or any other, from
;; this software.

(ns test.cljgp.test-evaluation
  (:use clojure.test
	test.helpers
	cljgp.evaluation
	cljgp.breeding
	cljgp.util
	cljgp.random))

(defn- excepting-evaluator
  [func]
  (println "Throwing test exception...")
  (throw (RuntimeException. "Except! I should be reported on stdout!")))


; Have to copy from structmap into normal map to use dissoc in tests
; while the cljgp functions want structmaps. Bit of a hassle.
(deftest test-evaluate-individual
  (let [struct-ind (make-individual (my-tpl '(+ 1 2)) 99)
	ind (into {} struct-ind)
	given-fitness 3.33
	evaluator (fn [func] given-fitness)
	eval-ind (into {} (evaluate-individual struct-ind evaluator))

	given-score 10
	evaluator-map (fn [func] {:fitness given-fitness :raw-score 10})
	eval-map-ind (into {} (evaluate-individual struct-ind evaluator-map))]
    (testing "evaluator returning numeric value"
	     (is (= (dissoc eval-ind :fitness) (dissoc ind :fitness))
		 "Apart from fitness, individuals should be identical")
	     (is (= (:fitness eval-ind) given-fitness)
		 "Fitness should be as returned by evaluator"))
    (testing "evaluator returning map"
	     (is (= (dissoc eval-map-ind :fitness :raw-score)
		    (dissoc ind :fitness :raw-score))
		 "Apart from new keys, individuals should be identical")
	     (is (and (= (:fitness eval-map-ind) given-fitness)
		      (= (:raw-score eval-map-ind) given-score))
		 "Key values should be as returned by evaluator"))
    (is (thrown-with-msg? RuntimeException #"Except!.*"
			  (with-out-str ; silence output for cleanliness
			    (evaluate-individual struct-ind 
						 excepting-evaluator)))
	"Exceptions not caught by evaluator should be reported and re-thrown")))

(def mini-config
     {:evaluation-fn (constantly 1.33)
      :threads 2
      :rand-fns (take 2 (repeatedly #(rand-fn nextDouble 
					      (new java.util.Random))))})

(deftest test-evaluate-pop
  (let [old-pop (take 5 (repeatedly #(make-individual (my-tpl '(+ 1 2)) 99)))
	new-pop (evaluate-pop old-pop mini-config)]
    (is (seq? new-pop) 
	"Pop should be a sequence")
    (is (= (count old-pop) (count new-pop))
	"Pop size should remain constant")
    (is (empty? (filter #(not= (:fitness %) 1.33) new-pop)) 
	"Fitness should be what the evaluator returns")
    (is  (= (map #(dissoc (into {} %) :fitness) new-pop)
	    (map #(dissoc (into {} %) :fitness) old-pop))
	 "Apart from fitness, individuals should be identical")))


#_(deftest test-best-fitness
  (let [pop (evaluate-pop (generate-pop config-maths) config-maths)
	real-best (first (sort-by :fitness pop))]
    (is (= (best-fitness pop) real-best))))

