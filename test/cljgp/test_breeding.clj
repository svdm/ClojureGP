;; Copyright (c) Stefan A. van der Meer. All rights reserved.
;; The use and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which
;; can be found in the file epl-v10.html at the root of this distribution. By
;; using this software in any fashion, you are agreeing to be bound by the
;; terms of this license. You must not remove this notice, or any other, from
;; this software.

(ns test.cljgp.test-breeding
  (:use clojure.contrib.test-is
	cljgp.generate
	cljgp.breeding
	cljgp.evaluation
	cljgp.util)
  (:refer test.helpers))


(deftest test-parent-arg-type
  (let [p (fn [s t] (with-meta s {:gp-arg-types t}))
	tree-orig [(p '+ [:+1 :+2]) 
		   [(p '* [:*1 :*2]) (p 'x []) (p 'y [])]
		   [(p '- [:-1 :-2]) 
		    [(p 'div [:div1 :div2]) (p 'a []) (p 'b [])]
		    (p 'c [])]]]
    (is (= [:root :+1 :*1 :*2 :+2 :-1 :div1 :div2 :-2]
	   (map #(parent-arg-type % :root (make-tree-seq tree-orig))
		(range (count (make-tree-seq tree-orig))))))))

(deftest test-tree-replace
  (let [tree-orig `(+ (* (+ _1 _2) (- _3 _4)) (/ (/ _5 _1) (+ _2 _3)))]
    (doseq [i (range (count (make-tree-seq tree-orig)))]
      (is (= :test
	     (nth (make-tree-seq (tree-replace i :test tree-orig)) i))
	  "Replace should work correctly in any tree node."))))

;;; Two manually created trees for testing a specific type situation
(def typetest-tree-a
     (list (with-meta `count {:gp-type Number :gp-arg-types [:test.helpers/seq]})
	   (with-meta `TEXT {:gp-type :test.helpers/string})))

(def typetest-tree-b
     (list (with-meta `safe-nth {:gp-type Number 
				 :gp-arg-types [:test.helpers/vector Number]})
	   (with-meta `VECT {:gp-type :test.helpers/vector})
	   (with-meta `_1 {:gp-type Number})))

(deftest test-crossover-uniform
  (let [trees (crossover-uniform [(my-gen 6 :full rtype)
				  (my-gen 6 :grow rtype)]
				 rtype)]
    (is (or (= (count trees) 2) (= trees nil)))
    (doseq [tree trees]
      (full-tree-test tree)))

;;; test for a regression in typed crossover, where types with a common parent
;;; type could be picked for crossover even though the type of the first node
;;; could not satisfy the parent-arg-type of the second node, leading to
;;; invalidly typed trees
  (dotimes [_ 10] 
    (let [typetrees (crossover-uniform [typetest-tree-a
					typetest-tree-b]
				       Number)]
      (testing "Invalid types after crossover, possible regression"
	(doseq [tree typetrees]
	  (full-tree-test tree))))))

(deftest test-mutate
  (let [tree (mutate (my-gen 4 :full rtype) 17
		     func-set-maths
		     term-set-maths
		     rtype)]
    (full-tree-test tree)))

(defn test-inds
  [inds gen-old num-expected]
  (let [trees (map #(get-fn-body (:func %)) inds)]
    (is (seq inds))
    (is (= (count inds) num-expected))
    (is (every? valid-ind? inds)
	"All individuals should be maps with the required keys")
    (is (every? #(= (:gen %) (inc gen-old)) inds)
	"New individuals should have incremented :gen.")
    (doseq [tree trees]
      (full-tree-test tree))))

(deftest test-crossover-individuals
  (let [gen-old 0
	inds (crossover-individuals 
	      crossover-uniform
	      [(make-individual (my-tpl (my-gen 4 :full rtype)) 
				gen-old)
	       (make-individual (my-tpl (my-gen 4 :grow rtype)) 
				gen-old)]
	      config-maths)]
    (test-inds inds gen-old 2)))

(deftest test-mutate-ind
  (let [gen-old 0
	ind (mutate-individual 
	     (make-individual (my-tpl (my-gen 4 :full rtype)) gen-old)
	     17
	     config-maths)]
    (test-inds ind gen-old 1)))

(deftest test-reproduce-ind
  (let [gen-old 0
	ind-old (make-individual (my-tpl (my-gen 4 :full rtype)) 
				 gen-old)
	ind (reproduce-individual ind-old config-maths)]
    (test-inds ind gen-old 1)
    (is (= (dissoc (into {} ind-old) :gen) 
	   (dissoc (into {} (first ind)) :gen))
	"Reproduced individuals should be identical apart from :gen")))

; *-breeder functions not tested currently as they are very basic compositions
; of a selection function and the *-ind functions
; perhaps TODO

#_(defn fake-evaluation
  [pop]
  (map #(assoc % :fitness (rand)) pop))

(deftest test-breed-new-pop
  (let [target-size (:population-size config-maths)
	pop-evaluated (evaluate-pop (generate-pop config-maths) config-maths)
	pop-new (breed-new-pop pop-evaluated config-maths)]
    (is (seq (doall pop-new)))
    (is (= (count pop-new) target-size))))
