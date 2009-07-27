;; Copyright (c) Stefan A. van der Meer. All rights reserved.
;; The use and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which
;; can be found in the file epl-v10.html at the root of this distribution. By
;; using this software in any fashion, you are agreeing to be bound by the
;; terms of this license. You must not remove this notice, or any other, from
;; this software.

; Various functions and data structures used in the tests

(ns test.helpers
  (:use clojure.test
	cljgp.breeding
	cljgp.generate
	cljgp.selection
	cljgp.config
	cljgp.random
	cljgp.util))

(defmacro quiet
  [form]
  `(binding [println (constantly nil)]
     ~form))

; Validity checks

(defn valid-tree?
  "Does this tree fit the most fundamental requirements? That is, is it a seq or
  a valid result?"
  [tree]
  (or (coll? tree) 
      (number? tree)
      (symbol? tree)))

(defn valid-ind?
  "Returns whether given map contains keys required for individuals."
  [ind]
  (and (map? ind)
       (every? (set (keys ind)) [:func
				 :gen
				 :fitness])))


; A config for a simple gimped experiment

(derive ::num ::any)
(derive Number ::num)
(derive ::seq ::any)
(derive ::string ::seq)
(derive ::vector ::seq)

(def _1 1)
(def _2 2)
(def _3 3)
(def _4 4)
(def _5 5)

(def TEXT "foobar")
(def VECT [94 17 2])

(defn safe-nth
  [coll idx]
  (if-let [result (try (nth coll idx)
		       (catch RuntimeException e nil))]
    result
    0))

(def config-maths
     {:function-set [(prim `- 
			   {:gp-type Number 
			    :gp-arg-types [Number Number]})

		     (prim `+ 
			   {:gp-type Number 
			    :gp-arg-types [Number Number]})

		     (prim `* 
			   {:gp-type Number 
			    :gp-arg-types [Number Number]})

		     (prim `count 
			   {:gp-type Number
			    :gp-arg-types [::seq]})

		     (prim `safe-nth
			   {:gp-type Number
			    :gp-arg-types [::vector Number]})]

      :terminal-set [(prim `_1 {:gp-type Number})
		     (prim `_2 {:gp-type Number})
		     (prim `_3 {:gp-type Number})
		     4
		     5
		     (prim `TEXT {:gp-type ::string})
		     (prim `VECT {:gp-type ::vector})]
      :arg-list []

      :func-template-fn (make-func-template 'gp-mather [])

      :root-type Number
      
      :evaluation-fn (fn [x] (rand))
      :selection-fn (partial tournament-select {:size 3})

      :end-condition-fn (make-end 50)
      :population-size 8

      :breeders [{:prob 0.8    :breeder-fn crossover-breeder}
		 {:prob 0.1    :breeder-fn mutation-breeder}
		 {:prob 0.1    :breeder-fn reproduction-breeder}]

      :breeding-retries 5

      :validate-tree-fn identity

      :pop-generation-fn (partial generate-ramped {:max-depth 7
						   :grow-change 0.5})

      :threads 2
      :rand-seeds [0 1]			; not used
      :rand-fn-maker make-default-rand

					; supply premade rand-fns instead of
					; depending on preproc to do this
      :rand-fns (map make-default-rand 
		     (take 2 (repeatedly #(System/currentTimeMillis))))
      })


; Helpers for breeding and generate tests

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
