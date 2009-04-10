

;;; cljgp.tests.helpers.clj

; Various functions and data structures used in the tests

(ns cljgp.tests.helpers
  (:use cljgp.breeding
	cljgp.selection
	cljgp.config
	cljgp.random))

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
      (symbol? tree)))	; sufficient for test terminal set(s)

(defn valid-ind?
  "Returns whether given map contains keys required for individuals."
  [ind]
  (and (map? ind)
       (every? (set (keys ind)) [:func
				 :gen
				 :fitness])))


; A config for a simple gimped experiment

(def _1 1)
(def _2 2)
(def _3 3)
(def _4 4)
(def _5 5)

(def config-maths
     {:function-set [(prim `- {:type Number 
			       :arg-type [Number Number] 
			       :arity 2})

		     (prim `+ {:type Number 
			       :arg-type [Number Number]
			       :arity 2})

		     (prim `* {:type Number 
			       :arg-type [Number Number]
			       :arity 2})]

      :terminal-set [(prim `_1 {:type Number})
		     (prim `_2 {:type Number})
		     (prim `_3 {:type Number})
		     (prim `_4 {:type Number})
		     (prim `_5 {:type Number})]
      :arg-list []

      :evaluation-fn (fn [x] (rand))
      :selection-fn (partial tournament-select 3)

      :end-condition (make-simple-end 50)
      :population-size 8

      :breeders [{:prob 0.8    :breeder-fn crossover-breeder}
		 {:prob 0.1    :breeder-fn mutation-breeder}
		 {:prob 0.1    :breeder-fn reproduction-breeder}]

      :breeding-retries 5

      :validate-tree-fn identity

      :pop-generation-fn (partial generate-ramped 7 0.5)

      :threads 2
      :rand-seeds [0 1]			; not used
      :rand-fn-maker make-default-rand
					; supply premade rand-fns instead of depending on preproc to do this
      :rand-fns (map make-default-rand 
		     (take 2 (repeatedly #(System/currentTimeMillis))))
      })

