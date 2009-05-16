
(ns test.cljgp.test-util
  (:use clojure.contrib.test-is
	cljgp.util))

; mock expression tree, for the util functions the actual tree does not affect
; things
(def valid-tree '(+ 1 (* 3 4) (- 2 2)))

(defn my-tpl [tree] (list `fn [] tree))

(deftest test-make-individual
  (let [given-gen 99
	ind (make-individual (my-tpl valid-tree) given-gen)]
    (are _
	 (map? ind)
	 (= (type ind) clojure.lang.PersistentStructMap)
	 (= (:func ind) (my-tpl valid-tree))
	 (= (:gen ind) given-gen)
	 (= (:fitness ind 'not-found) nil))))

(deftest test-get-fn-body
  (is (= (get-fn-body (my-tpl valid-tree))
	 valid-tree))
  (is (= (get-fn-body '()) 
	 '())))

(deftest test-make-tree-seq
  (is (= (macroexpand-1 `(make-tree-seq valid-tree))
	 `(tree-seq coll? next valid-tree))))

(deftest test-tree-depth
  (is (= (tree-depth valid-tree) 3))
  (is (= (tree-depth '()) 0))
  (is (= (tree-depth '(1)) 1)))

(deftest test-gp-type
  (let [sym (with-meta 'x {:type Number})]
    (is (= (type sym) (gp-type sym)))
    (is (= (type sym) (gp-type [sym 1 2])))))