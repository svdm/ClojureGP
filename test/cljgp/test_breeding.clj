;; Copyright (c) Stefan A. van der Meer. All rights reserved.
;; The use and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which
;; can be found in the file epl-v10.html at the root of this distribution. By
;; using this software in any fashion, you are agreeing to be bound by the
;; terms of this license. You must not remove this notice, or any other, from
;; this software.

(ns test.cljgp.test-breeding
  (:use clojure.test
	cljgp.breeding
	cljgp.evaluation
	cljgp.util)
  (:refer test.helpers))

(def func-set-maths (:function-set config-maths))
(def term-set-maths (:terminal-set config-maths))

(defn my-tpl [tree] (list `fn 'gp-mather [] tree))

(defn my-gen
  [max-depth method root-type]
  (if-let [tree (try
		 (generate-tree max-depth
				method
				func-set-maths
				term-set-maths
				root-type)
		 (catch RuntimeException e
		   false))]
    tree
    (recur max-depth method root-type)))


(def rtype (:root-type config-maths))

; for this primitive set, trees should always return a number
(def valid-result? number?)

(defn valid-eval?
  "When evaluated, does this tree produce a valid result?"
  [tree]
  (valid-result? (eval tree)))



(defn full-tree-test
  [tree]
  (is (valid-tree? tree)
      (str "Root must be seq or result constant, tree: " tree))
  (is (valid-types? tree rtype)
      (str "Tree must be validly typed, tree: " tree))
  (is (valid-eval? tree)
      (str "Result of evaluation must be valid, tree: " tree)))

; generation function tests

(deftest test-generate-tree
  (testing "grow method"
    (let [max-depth 4
	  tree (my-gen max-depth :grow rtype)
	  depth (tree-depth tree)]
      (full-tree-test tree)
      (is (and (> depth 0) (<= depth max-depth))
	  "Grow-method trees must be of a valid size up to the limit.")
      (is (= (tree-depth (my-gen 0 :grow rtype)) 1) 
	  "For max-depth 0, must return single node.")))
  (testing "full method"
    (let [max-depth 4
	  tree (my-gen max-depth :full rtype)]
      (full-tree-test tree)
      (is (= (tree-depth tree) max-depth)
	  "Full-method trees must be the given max-depth in size.")
      (is (= (tree-depth (my-gen 0 :full rtype)) 1)
	  "For max-depth 0, must return single node."))))

(deftest test-generate-ramped
  (let [d 4
	gtor (fn [opts] (get-valid (comp not nil?) 512 
				   #(generate-ramped opts config-maths)))
	grown-tree (gtor {:max-depth d :grow-chance 1})
	full-tree (gtor {:max-depth d :grow-chance 0})
	rand-tree (gtor {:max-depth d :grow-chance 0.5})]
    (testing "generated tree validity"
	     (full-tree-test grown-tree)
	     (full-tree-test full-tree)
	     (full-tree-test rand-tree))
    (is (<= (tree-depth full-tree) d)
	"Ramped gen with 0% grow chance should result in a full tree.")
    (is (<= (tree-depth grown-tree) d)
	"Ramped gen with 100% grow chance should result in a grown tree.")))

(deftest test-generate-pop
  (let [target-size (:population-size config-maths)
	pop (doall (generate-pop config-maths))]
    (is (seq pop)
	"Generated population must be a valid seq-able.")
    (is (= (count pop) target-size)
	"Generated population should be of the specified size.")
    (is (empty? (filter #(not (valid-tree? (get-fn-body (get-func %)))) pop))
	"All generated trees must be valid.")))


; breeding function tests

(deftest test-parent-arg-type
  (let [p (fn [s t] (with-meta s {:arg-type t}))
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
     (list (with-meta `count {:type Number :arg-type [:test.helpers/seq]})
	   (with-meta `TEXT {:type :test.helpers/string})))

(def typetest-tree-b
     (list (with-meta `safe-nth {:type Number 
				 :arg-type [:test.helpers/vector Number]})
	   (with-meta `VECT {:type :test.helpers/vector})
	   (with-meta `_1 {:type Number})))

(deftest test-crossover-uniform
  (let [trees (crossover-uniform-typed [(my-gen 6 :full rtype)
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
    (let [typetrees (crossover-uniform-typed [typetest-tree-a
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

(deftest test-get-valid
  (is (nil? (get-valid true? 2 #(vector [false false false]))))
  (is (= [1 2] (get-valid number? 1 #(vector 1 2)))))

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
	      crossover-uniform-typed
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
