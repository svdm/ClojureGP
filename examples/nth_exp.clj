
(ns nth-exp
  "An attempt at evolving the nth function."
  (:use [cljgp.core :only (generate-run)]
	cljgp.breeding
	cljgp.tools.logging
	cljgp.config
	cljgp.random
	cljgp.util))

;(set! *warn-on-reflection* true)

; CONFIG

(derive ::void ::any)
(derive ::val ::any)
(derive ::bool ::val)
(derive ::seq ::val)
(derive ::seq-rest ::seq)
(derive ::seq-orig ::seq)

(derive ::el ::val)

(derive ::number ::val)
(derive ::number-orig ::number)

;;; STGP NTH

(def var-1 nil)

(defn get-var-1
  []
  (deref var-1))

(defn set-var-1
  [lst]
  (reset! var-1 lst))

(defmacro do-times
  [n & body]
  `(dotimes [_# ~n]
     ~@body))

(defn evaluate-stgp-nth
  [stgpnth]
  (binding [var-1 (atom [])] ; local atom storage "variable"
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
      :function-set [(prim `do
			   {:type ::seq
			    :arg-type [::void ::seq]})

		     (prim `do
			   {:type ::el
			    :arg-type [::void ::el]})

		     (prim `do
			   {:type ::number
			    :arg-type [::void ::number]})

		     (prim `do-times
			   {:type ::void
			    :arg-type [::number ::void]})

		     (prim `first
			   {:type ::el
			    :arg-type [::seq]})

		     (prim `next
			   {:type ::seq-rest
			    :arg-type [::seq-orig]})

		     (prim `set-var-1
			   {:type ::void
			    :arg-type [::seq]})
		     
		     (prim `get-var-1
			   {:type ::seq-orig
			    :arg-type []})]
      

      :terminal-set [(prim 'coll 
			   {:type ::seq-orig})
		     
		     (prim 'index 
			   {:type ::number})]
      
      :root-type ::el

      :func-template-fn (make-func-template 'stgp-nth '[coll index])
      
      :evaluation-fn evaluate-stgp-nth

      :population-size 1000

      :end-condition-fn (make-simple-end 50)

      :validate-tree-fn #(< (tree-depth %) 8)

      :pop-generation-fn (partial generate-ramped {:max-depth 5
						   :grow-chance 0.5})

      :breeders [{:prob 0.8  :breeder-fn crossover-breeder}
		 {:prob 0.2  :breeder-fn (partial mutation-breeder 
						  {:max-depth 5})}]

      :breeding-retries 500

      :threads 1
      
      :rand-seeds (seeds-from-time true)
      })



(defn run-stgp
  ([]
     (run-stgp :basic-trees))
  ([print-type]
     (print-best 
      (last 
       (map #(print-stats print-type %) 
	    (generate-run config-stgp-nth))))))


(def stgp-solution
     `(fn [~'coll ~'index]
	(do
	  (do-times (do (set-var-1 ~'coll) ~'index)
	    (set-var-1 (next (get-var-1))))
	  (first (get-var-1)))))

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

(def evolved-simplified
     (fn stgp-nth [coll index]
       (do
	 (set-var-1 coll)
	 (first
	  (do
	    (do-times index 
		      (set-var-1 (next (get-var-1))))
	    (get-var-1))))))