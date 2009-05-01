
;;; cljgp.tests.test_evaluation.clj

(ns cljgp.tests.test-evaluation
  (:use clojure.contrib.test-is
	cljgp.evaluation
	cljgp.breeding
	cljgp.tests.helpers
	cljgp.util
	cljgp.random))

(defn- excepting-evaluator
  [func]
  (println "Throwing test exception...")
  (throw (RuntimeException. "Except! I should be reported on stdout!")))

(deftest test-evaluate-ind
  (let [given-fitness 3.33
	ind (make-individual '(+ 1 2) 99 '[])
	evaluator (fn [func] given-fitness)
	eval-ind (evaluate-ind evaluator ind)]
    (is (= (dissoc eval-ind :fitness) (dissoc ind :fitness))
	"Apart from fitness, individuals should be identical")
    (is (= (:fitness eval-ind) given-fitness)
	"Fitness should be what evaluator returns")
    (is (thrown-with-msg? RuntimeException #"Except!.*"
			  (with-out-str ; silence output for cleanliness
			    (evaluate-ind excepting-evaluator ind)))
	"Exceptions not caught by evaluator should be reported and re-thrown")))

(def mini-config
     {:evaluation-fn (constantly 1.33)
      :threads 2
      :rand-fns (take 2 (repeatedly #(rand-fn nextDouble 
					      (new java.util.Random))))})

(deftest test-evaluate-pop
  (let [old-pop (take 5 (repeatedly #(make-individual '(+ 1 2) 99 '[])))
	new-pop (evaluate-pop old-pop mini-config)]
    (is (seq? new-pop) 
	"Pop should be a sequence")
    (is (= (count old-pop) (count new-pop))
	"Pop size should remain constant")
    (is (empty? (filter #(not= (:fitness %) 1.33) new-pop)) 
	"Fitness should be what the evaluator returns")
    (is  (= (map #(dissoc % :fitness) new-pop)
	    (map #(dissoc % :fitness) old-pop))
	 "Apart from fitness, individuals should be identical")))


#_(deftest test-best-fitness
  (let [pop (evaluate-pop (generate-pop config-maths) config-maths)
	real-best (first (sort-by :fitness pop))]
    (is (= (best-fitness pop) real-best))))

