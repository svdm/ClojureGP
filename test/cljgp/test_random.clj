;; Copyright (c) Stefan A. van der Meer. All rights reserved.
;; The use and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which
;; can be found in the file epl-v10.html at the root of this distribution. By
;; using this software in any fashion, you are agreeing to be bound by the
;; terms of this license. You must not remove this notice, or any other, from
;; this software.

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