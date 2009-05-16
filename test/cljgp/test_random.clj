
(ns test.cljgp.test-random
  (:use clojure.contrib.test-is
	cljgp.random))

(deftest test-gp-rand
  (let [r (gp-rand)]
    (is (number? r))
    (is (and (>= r 0) (< r 1)))))

(deftest test-gp-rand-int
  (let [n 10
	r (gp-rand-int n)]
    (is (integer? r))
    (is (and (>= r 0) (< r n)))
    (is (> 1
	   (apply + (take 5 (repeatedly #(gp-rand-int 1)))))) 
    "Given max should be exclusive, so we should add up zeroes here"))

(deftest test-pick-rand
  (let [c [1 2 3]
	pick (pick-rand c)]
    (is (some #(= pick %) c))
    (is (nil? (pick-rand [])))))