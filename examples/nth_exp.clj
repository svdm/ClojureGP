
(ns nth-exp
  "Example 02: An attempt at evolving the nth function, similar to the
  experiment in the article Strongly Typed Genetic Programming by Montana, D.J.,
  2002."
  (:use [cljgp.core :only (generate-run)]
	cljgp.selection
	cljgp.breeding
	cljgp.tools.logging
	cljgp.config
	cljgp.random
	cljgp.util
	[clojure.contrib.def :only (defvar)]))

;(set! *warn-on-reflection* true)


;;; Here the type hierarchy is defined.

(derive ::void ::any)
(derive ::val ::any)
(derive ::bool ::val)
(derive ::el ::val)
(derive ::number ::val)

;;; The ::seq type is derived here so that we can guarantee that the 'next
;;; function in the function set cannot be chained endlessly onto
;;; itself. Without this, the evolution process can be tempted into a (terrible)
;;; local maximum with long chains of (first (next (next (next ...). By
;;; specialising ::seq, 'next can be configured to only take ::seq-orig while
;;; returning a ::seq, which makes (next (next ..) illegal.
(derive ::seq ::val)
(derive ::seq-orig ::seq)


;;; The canonical STGP nth experiment aims to evolve an iterative solution using
;;; a mutable variable to store lists. Though not idiomatic clojure, this
;;; approach is mirrored here for the sake of the example. A nice property of
;;; the experiment is that the fitness landscape appears to be fairly smooth,
;;; with little risk of ending up in a local maximum.

(defvar var-1 nil
  "Storage var provided to the GP process, will be bound to a local atom during
  evaluation.")

(defn get-var-1
  "Retrieves the value stored in var-1."
  []
  (deref var-1))

(defn set-var-1
  "Sets the value stored in var-1."
  [lst]
  (reset! var-1 lst))

(defmacro do-times
  "Simplification of clojure.core/dotimes. Does not require a vector of
  bindings, but only the number of executions."
  [n & body]
  `(dotimes [_# ~n]
     ~@body))

(defn evaluate-stgp-nth
  "Evaluates the performance of an evolved function and returns a fitness
  value."
  [stgpnth]
  (binding [var-1 (atom [])]		; local atom storage "variable"
    (let [c (vec (range 0 50))
	  evl (fn [idx]
		(let [result (stgpnth c idx)]
		  (if (number? result)
		    (Math/abs 
		     (float (- (nth c idx)
			       result)))
		    100)))]		; nil, punish as twice as bad as worst
					; case (which would be 50 steps away)
      (reduce + (map evl 
		     (drop-last c))))))


(def config-stgp-nth 
     {
;;; cljgp can match the GP system described in STGP in a lot of ways, but
;;; polymorphic type checking is not supported. Hence, we have to define
;;; multiple versions of 'do for different types in the function set so the type
;;; checking system can guarantee the type of the second expression and the type
;;; of the return value are of the same type.
      :function-set [(prim `do
			   {:gp-type ::seq
			    :gp-arg-types [::void ::seq]})

		     (prim `do
			   {:gp-type ::el
			    :gp-arg-types [::void ::el]})

		     (prim `do
			   {:gp-type ::number
			    :gp-arg-types [::void ::number]})

		     (prim `do-times
			   {:gp-type ::void
			    :gp-arg-types [::number ::void]})

		     (prim `first
			   {:gp-type ::el
			    :gp-arg-types [::seq]})

		     (prim `next
			   {:gp-type ::seq
			    :gp-arg-types [::seq-orig]})

		     (prim `set-var-1
			   {:gp-type ::void
			    :gp-arg-types [::seq]})
		     
		     ;; get-var-1 could also be seen as a terminal as it does
		     ;; not have children, however it is a function that needs
		     ;; to be applied. A function primitive with zero arguments
		     ;; is an easy way of handling this.
		     (prim `get-var-1
			   {:gp-type ::seq-orig
			    :gp-arg-types []})]
      

      ;; The only real terminals are the two arguments to the function. Note
      ;; that these symbols are not resolved here.
      :terminal-set [(prim 'coll 
			   {:gp-type ::seq-orig})
		     
		     (prim 'index 
			   {:gp-type ::number})]
      
      ;; nth must return a list element
      :root-type ::el

      ;; We need our evolved trees to be made into functions with the right
      ;; argument list, so we use a helper to create an appropriate "templating"
      ;; function here that takes a tree and turns it into such an fn form.
      :func-template-fn (make-func-template 'stgp-nth '[coll index])
      
      :evaluation-fn evaluate-stgp-nth

      :population-size 1000

      ;; Stop after 50 generations or when a perfect individual exist.
      :end-condition-fn (make-simple-end 50)

      ;; Very deep trees are not of interest here, so we can limit the search
      ;; space a bit.
      :validate-tree-fn #(<= (tree-depth %) 7)

      ;; Some tweaking of the breeding facilities, not really required.
      :pop-generation-fn (partial generate-ramped {:max-depth 5
						   :grow-chance 0.5})

      :selection-fn (partial tournament-select {:size 14})

      :breeders [{:prob 0.8  :breeder-fn crossover-breeder}
		 {:prob 0.2  :breeder-fn (partial mutation-breeder 
					   {:max-depth 5})}]

      :breeding-retries 500

      :threads 2

      ;; Generate new seeds on each run, and report them to stdout for
      ;; later reproduction of the results.
      :rand-seeds (seeds-from-time true) 

      ;; Alternative good seeds: 
      ;; - (repeat 1243761389515) ; same for all threads
      ;; - [593221374086 825095143445]
      })



(defn run
  "Run the experiment and print the best individual at the end. The 'print-type
  parameter determines how the statistics are printed to stdout for each
  generation. See cljgp.tools.logging/print-stats for details."
  ([]
     (run :basic-trees))
  ([print-type]
     (print-best 
      (last 
       (map #(print-stats print-type %) 
	    (generate-run config-stgp-nth))))))


;;; Translation of the solution as listed in the STGP article.
(def stgp-solution
     `(fn [~'coll ~'index]
	(do
	  (do-times (do (set-var-1 ~'coll) ~'index)
	    (set-var-1 (next (get-var-1))))
	  (first (get-var-1)))))

;;; An example of a solution evolved by the experiment defined here.
(def evolved-solution
     (fn stgp-nth [coll index]
       (do
	 (set-var-1
	  (do
	    (set-var-1 (next coll))
	    (do (set-var-1 (next (get-var-1))) coll)))
	 (first
	  (do
	    (do-times index (set-var-1 (next (get-var-1))))
	    (get-var-1))))))

;;; Simplified version of evolved-solution, with redundant code removed.
(def evolved-simplified
     (fn stgp-nth [coll index]
       (do
	 (set-var-1 coll)
	 (first
	  (do
	    (do-times index 
		      (set-var-1 (next (get-var-1))))
	    (get-var-1))))))