
(ns nth-exp
  "An attempt at evolving the nth function."
  (:use [cljgp.core :only (generate-run)]
	cljgp.tools.logging
	cljgp.config
	cljgp.random))

(set! *warn-on-reflection* true)

(defn my-nth
  [coll index]
  (cond
    (= index 0) (first coll)
    (> index 0) (recur (next coll) (dec index))
    :else nil))


(defn evaluate-nth
  [gpnth]
  (try
   (let [c (range 0 20)
	 evl (fn [idx]
	       (Math/abs 
		(float (- (nth c idx)
			  (gpnth c idx)))))]
     (reduce + (map evl c)))
   (catch RuntimeException e Float/MAX_VALUE)))


; CONFIG

(def ZERO 0)

(derive ::testable ::any)
(derive ::bool ::testable)
(derive ::seq ::testable)

(derive ::el-or-recur ::any)
(derive ::el ::el-or-recur)

(derive ::number ::any)

(def config-nth 
     {
      :function-set [(prim `if
			   {:type ::el
			    :arg-type [::testable
				       ::el
				       ::el-or-recur]})

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
			   {:type ::seq
			    :arg-type [::seq]})

		     (prim `inc
			   {:type ::number
			    :arg-type [::number]})
		     
		     (prim `dec
			   {:type ::number
			    :arg-type [::number]})

		     (prim 'gp-nth
			   {:type ::el-or-recur
			    :arg-type [::seq ::number]})]
      

      :terminal-set [(prim 'coll 
			   {:type ::seq})
		     
		     (prim 'index 
			   {:type ::number})
		     
		     (prim `ZERO
			   {:type ::number})
		     ]
      
      :root-type ::el
      :func-template-fn (make-func-template 'gp-nth '[coll index])
      
      :evaluation-fn evaluate-nth

      :population-size 1024

      :end-condition-fn (make-simple-end 100)

      :breeding-retries 5

      :threads 1
      
      :rand-seeds [(rand-int 8432894897)
		   (rand-int 27137054)]
      })

(defn run-exp
  []
  (reduce-to-summary 
   (map print-stats (generate-run config-nth))))


