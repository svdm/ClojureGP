
;; cljgp.tests.test_util.clj

(ns cljgp.tests.test-util
  (:use clojure.contrib.test-is
	cljgp.util))

; mock expression tree, for the util functions the actual tree does not affect
; things
(def valid-tree '(+ 1 (* 3 4) (- 2 2)))

(deftest test-make-individual
  (let [given-args '[foo bar]
	given-gen 99
	ind (make-individual valid-tree given-gen given-args)]
    (are _
	 (map? ind)
	 (= (:func ind) `(fn ~given-args ~valid-tree))
	 (= (:gen ind) given-gen)
	 (= (:fitness ind 'not-found) nil))))

(deftest test-get-fn-body
  (is (= (get-fn-body (:func (make-individual valid-tree 0 '[a b])))
	 valid-tree))
  (is (= (get-fn-body '()) 
	 '())))

(deftest test-make-tree-seq
  (is (= (macroexpand-1 `(make-tree-seq valid-tree))
	 `(tree-seq seq? next valid-tree))))

(deftest test-tree-depth
  (is (= (tree-depth valid-tree) 3))
  (is (= (tree-depth '()) 0))
  (is (= (tree-depth '(1)) 1)))