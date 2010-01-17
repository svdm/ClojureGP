;; Copyright (c) Stefan A. van der Meer. All rights reserved.
;; The use and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which
;; can be found in the file epl-v10.html at the root of this distribution. By
;; using this software in any fashion, you are agreeing to be bound by the
;; terms of this license. You must not remove this notice, or any other, from
;; this software.

(ns test.cljgp.test-util
  (:use clojure.test
        cljgp.util))

; mock expression tree, for the util functions the actual tree does not affect
; things
(def valid-tree '(+ 1 (* 3 4) (- 2 2)))

(defn my-tpl [tree] (list `fn [] tree))

(deftest test-make-individual
  (let [given-gen 99
        ind (make-individual (my-tpl valid-tree) given-gen)]
    (is (map? ind))
    (is (= (type ind) clojure.lang.PersistentStructMap))
    (is (= (:func ind) (my-tpl valid-tree)))
    (is (= (:gen ind) given-gen))
    (is (= (:fitness ind 'not-found) nil))))

(deftest test-get-fn-body
  (is (= (get-fn-body (my-tpl valid-tree))
         valid-tree))
  (is (= (seq (get-fn-body '())) 
         nil)))

(deftest test-make-tree-seq
  (is (= (macroexpand-1 `(make-tree-seq valid-tree))
         `(tree-seq coll? next valid-tree))))

(deftest test-tree-depth
  (is (= (tree-depth valid-tree) 3))
  (is (= (tree-depth '()) 0))
  (is (= (tree-depth '(1)) 1)))

(deftest test-gp-type
  (let [sym (with-meta 'x {:gp-type Number})]
    (is (= (:gp-type (meta sym)) (gp-type sym)))
    (is (= (:gp-type (meta sym)) (gp-type [sym 1 2])))))