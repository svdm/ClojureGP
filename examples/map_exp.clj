
(ns map-exp
  "Example 03: An attempt at evolving the map function."
  (:use [cljgp.core :only (generate-run)]
	cljgp.selection
	cljgp.breeding
	cljgp.tools.logging
	cljgp.config
	cljgp.random
	cljgp.util))


(def wrapped-gp-map nil)

(defn safe-inc
  [n]
  (when (number? n)
    (inc n)))

(def trials [(list)
	     (list 1)
	     (apply list (range 1 21))])
(def solutions (map #(simple-map safe-inc (seq %)) trials))

(defn evaluate-map-single
  [gp-map trial]
  (try
   (let [limit (atom 0)]
     (binding [wrapped-gp-map (fn [f c]
				(if (> @limit 40)
				  (throw (StackOverflowError. "gp"))
				  (do (swap! limit inc)
				      (let [result (gp-map f c)]
					(swap! limit dec)
					result))))]
       (gp-map safe-inc (seq trial))))
   (catch StackOverflowError e
     :overflow)))

(defn dist
  "Returns the distance of el from the position (dec el) in lst. If not found,
  returns 1000."
  [el lst]
  (let [pos (dec el)
	dst (loop [l lst
		   i 1]
	      (when-let [s (seq l)]
		(if (== (first s) el)
		  (Math/abs (- pos i))
		  (recur (next s) (inc i)))))] 
    (if (nil? dst) 
      50
      dst)))

(defn score-result
  [result solution]
  (cond
    (= result :overflow) (+ (* 2 (count solution)) 10)
    :else (+ (* 2 (Math/abs (- (count solution) (count result))))
	     (reduce #(+ %1 (dec (Math/pow 2 (dist %2 result))))
		     0
		     solution))))

(defn evaluate-map
  [gp-map]
  (let [results (map #(evaluate-map-single gp-map %) trials)
	errs (doall (map #(score-result %1 %2) results solutions))]
    (reduce + errs)))


(derive ::void ::any)
(derive ::val ::any)
(derive ::testable ::val)
(derive ::seq ::testable)
(derive ::el ::val)
(derive ::fun-passed ::val)
(derive ::seq-orig ::seq)
(derive ::seq-processed ::any)

(def func-set
     [(primitive `when
		 {:gp-type ::seq
		  :gp-arg-types [::seq-orig
			     ::seq]})

      (primitive `cons
		 {:gp-type ::seq
		  :gp-arg-types [::el ::seq]})

      (primitive `first
		 {:gp-type ::el
		  :gp-arg-types [::seq]})

      (primitive `next
		 {:gp-type ::seq
		  :gp-arg-types [::seq-orig]})

      (primitive `wrapped-gp-map
		 {:gp-type ::seq
		  :gp-arg-types [::func-passed ::seq]})

      ;; The function passed to map, applied to element
      (primitive 'f
		 {:gp-type ::el
		  :gp-arg-types [::el]})])

(def term-set
     [(primitive 'coll
		 {:gp-type ::seq-orig})

      ;; The function passed to map, passed on in self-call
      (primitive 'f
		 {:gp-type ::func-passed})])

(def breeding-options
     {:root-type ::seq
      :func-template-fn (make-func-template '[f coll])

      :validate-tree-fn #(< (tree-depth %) 7)
      :breeding-retries 256})

(def run-options
     {:evaluation-fn evaluate-map
      :population-size 1024
      :end-condition-fn (make-simple-end 100)
      :threads 2})


(def config-map
     (merge {:function-set func-set
	     :terminal-set term-set}
	    breeding-options
	    run-options))

(defn run
  "Run the experiment and print the best individual at the end. The 'print-type
  parameter determines how the statistics are printed to stdout for each
  generation. See cljgp.tools.logging/print-stats for details."
  ([]
     (run :basic-trees))
  ([print-type]
     (reduce-to-summary
      (map #(print-stats print-type %) 
	   (generate-run config-map)))))


