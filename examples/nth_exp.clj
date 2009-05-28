
(ns nth-exp
  "An attempt at evolving the nth function."
  (:use [cljgp.core :only (generate-run)]
	cljgp.breeding
	cljgp.tools.logging
	cljgp.config
	cljgp.random
	cljgp.util))

(set! *warn-on-reflection* true)

(defn my-nth
  [coll index]
  (cond
    (= index 0) (first coll)
    (> index 0) (recur (next coll) (dec index))
    :else nil))

(defn make-wrapped-nth
  [the-nth counter limit]
  (fn [coll idx]
    (if (> (swap! counter inc)
	   limit)
      (do (the-nth coll idx)
	  (swap! counter dec))
      (throw (StackOverflowError. 
	      "GP recursion limit exceeded.")))))

(def wrapped-nth (fn [& _] (throw (Exception. "unbound wrapper"))))

(def penalty 10000)

(defn evaluate-nth
  [gpnth]
  (try
   (let [recurse-count (atom 0)
	 recurse-limit 40]
     (binding [wrapped-nth (make-wrapped-nth gpnth 
					     recurse-count 
					     recurse-limit)]
       (let [c (range 0 50)
	     evl (fn [idx]
		   (if-let [result (gpnth c idx)]
		     (Math/abs 
		      (float (- (nth c idx)
				result)))
		     penalty))]
	 (reduce + (map evl c)))))
   (catch StackOverflowError e 
     penalty)))


; CONFIG

(def ZERO 0)

;(derive ::testable ::any)
(derive ::bool ::any)
(derive ::seq ::any)
(derive ::seq-rest ::seq)
(derive ::seq-orig ::seq)

;(derive ::el-or-recur ::any)
;(derive ::el ::el-or-recur)
(derive ::el ::any)

(derive ::number ::any)

(def config-nth 
     {
      :function-set [(prim `if
			   {:type ::el
			    :arg-type [::bool
				       ::el
				       ::el]})

		     (prim `=
			   {:type ::bool
			    :arg-type [::number ::number]})

		     (prim `>
			   {:type ::bool
			    :arg-type [::number ::number]})

		     (prim `<
			   {:type ::bool
			    :arg-type [::number ::number]})

		     (prim `first
			   {:type ::el
			    :arg-type [::seq]})

		     (prim `next
			   {:type ::seq-rest
			    :arg-type [::seq-orig]})

		     (prim `inc
			   {:type ::number
			    :arg-type [::number]})
		     
		     (prim `dec
			   {:type ::number
			    :arg-type [::number]})

		     (prim `wrapped-nth
			   {:type ::el
			    :arg-type [::seq ::number]})]
      

      :terminal-set [(prim 'coll 
			   {:type ::seq-orig})
		     
		     (prim 'index 
			   {:type ::number})
		     
		     (prim `ZERO
			   {:type ::number})
		     ]
      
      :root-type ::el
      :func-template-fn (make-func-template 'gp-nth '[coll index])
      
      :evaluation-fn evaluate-nth

      :population-size 1024

      :end-condition-fn (make-simple-end 50)

      :validate-tree-fn #(< (tree-size %) 40)

      :breeders [{:prob 0.6  :breeder-fn crossover-breeder}
		 {:prob 0.4  :breeder-fn (partial mutation-breeder 
						  {:max-depth 10})}]

      :breeding-retries 50

      :threads 1
      
      :rand-seeds [(rand-int 8432894897)
		   (rand-int 2713705494)]
      })

(defn run-exp
  ([]
     (run-exp :basic-trees))
  ([print-type]
     (print-best 
      (last 
       (map #(print-stats print-type %) 
	    (generate-run config-nth))))))

(defn check-gen-types
  [gen]
  (let [check (fn [ind]
		(when (not (valid-types? (get-fn-body (get-func ind)) ::el))
		  (print-code false ind)
		  (throw (Exception. "Found invalidly typed individual."))))]
    (dorun (map check gen)))
  gen)

(defn test-types
  []
  (last
   (map check-gen-types 
	(map print-stats (generate-run config-nth)))))

(def manual-solution
     `(fn gp-nth
	[coll index]
	(if (= index ZERO)
	  (first coll)
	  (wrapped-nth (next coll) (dec index)))))