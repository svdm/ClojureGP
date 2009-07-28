;; Copyright (c) Stefan A. van der Meer. All rights reserved.
;; The use and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which
;; can be found in the file epl-v10.html at the root of this distribution. By
;; using this software in any fashion, you are agreeing to be bound by the
;; terms of this license. You must not remove this notice, or any other, from
;; this software.

(ns map-exp
  "Example 03: An attempt at evolving a map function.

  Shows one way of evolving a recursive function without running into stack
  overflow issues.

  Very loosely based on an experiment in the STGP article [1] also mentioned in
  nth_exp.clj.

  [1] David J. Montana. Strongly typed genetic programming. Evolutionary
      Computation, 3(2):199--230, 1995."
  (:use [cljgp.core :only [generate-run]]
	cljgp.selection
	cljgp.generate
	cljgp.breeding
	cljgp.tools.logging
	cljgp.config
	cljgp.random
	cljgp.util
	[clojure.contrib.def :only [defunbound defvar]]))


;;;; Evaluation

;;; This is the function we will map over the trials during evaluation
(defn safe-inc
  "Like standard inc, but returns nil if the arg is not a number."
  [n]
  (when (number? n)
    (inc n)))


(defvar trials [(list)
		(list 1)
		(apply list (range 1 21))]
  "The trials on which the evolved functions will be evaluated. Note that for
  the third trial, all elements are numbers that are equal to their (one-based)
  index. Based on STGP article.")

(defvar solutions (doall (map #(doall (map safe-inc (seq %))) trials))
  "The solutions to the trials of the real map function.")


(defunbound wrapped-gp-map
  "Var that will be bound to a function during evaluation. The function will
  call an evolved gp-map function while limiting the number of self-calls to
  prevent stack overflow.")

(defn evaluate-map-single
  "Applies the given 'gp-map function to the given 'trial in a safe manner. "
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

;;; This function is called quite often, and since all list elements are
;;; integers we can do some primitive coercion on both the list indices and the
;;; elements to get significant speedups.
(defn dist
  "Returns the distance of el's actual position from the position (dec el) in
  lst. If not found, returns 50."
  [el lst]
  (let [el (int el)
	pos (dec el)
	dst (loop [l lst
		   i (int 1)]
	      (when-let [fst (first l)]
		(if (== el (int fst))
		  (Math/abs (- pos i))
		  (recur (next l) (inc i)))))] 
    (if (nil? dst) 
      50
      dst)))

(defn score-result
  "Given a result list and a solution list, calculates a score for the
  result. If the result is the keyword :overflow, this is treated as an invalid
  result, and a bad score is returned.

  For any other value of result, a score is given based on a comparison of list
  length and of element location (using the fact that all elements are equal to
  their true index plus one). The measure is based on the one given in the STGP
  article."
  [result solution]
  (cond
    (= result :overflow) (+ (* 2 (count solution)) 10)
    :else (+ (* 2 (Math/abs (- (count solution) (count result))))
	     (reduce #(+ %1 (dec (Math/pow 2 (dist %2 result))))
		     0
		     solution))))

(defn evaluate-map
  "Evaluates a given map function on the trials. Returns a fitness equal to the
  sum of the scores obtained for each trial using score-result."
  [gp-map]
  (let [results (map #(evaluate-map-single gp-map %) trials)
	errs (doall (map #(score-result %1 %2) results solutions))]
    (reduce + errs)))

;;;; Type hierarchy:
;;; We have several different types of values in play here: lists/sequences,
;;; elements and functions.
(derive ::seq ::val)
(derive ::el ::val)
(derive ::func-passed ::val)
;;; As in other examples, we use the type system to avoid chains of seq
;;; functions that we know are not of interest. See also the nth-exp example.
(derive ::seq-orig ::seq)

;;;; Experiment configuration

;;; Unlike other examples, the different parts of the experiment configuration
;;; are defined in separate vars here, which some may prefer.
(def func-set
     ;; Nil-punning version of when, that very strongly suggests the basic
     ;; recursive structure we are looking for. Evolving that structure in a
     ;; more open experiment configuration can be very difficult, as there
     ;; exists no smooth path in the search space: either an evolved function
     ;; has this structure and might get a good fitness, or it does not and gets
     ;; a terrible fitness due to hitting the recursion limit.
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

      ;; The "secured" self-call, refers to unbound var that will be bound
      ;; during evaluation.
      (primitive `wrapped-gp-map
		 {:gp-type ::seq
		  :gp-arg-types [::func-passed ::seq]})

      ;; The function passed to map, applied to element
      (primitive 'f
		 {:gp-type ::el
		  :gp-arg-types [::el]})])

(def term-set
     ;; The input sequence
     [(primitive 'coll
		 {:gp-type ::seq-orig})

      ;; The function passed to map, passed on in self-call
      (primitive 'f
		 {:gp-type ::func-passed})])

(def breeding-options
     {;; Evolved functions must return a seq
      :root-type ::seq

      ;; Evolved functions must be of the form (fn [f coll] ..)
      :func-template-fn (make-func-template '[f coll])

      :validate-tree-fn #(< (tree-depth %) 7)
      :breeding-retries 256})

(def run-options
     {:evaluation-fn evaluate-map
      :population-size 1024
      :end-condition-fn (make-end 50)
      :threads 2
      :rand-seeds (seeds-from-time true)
      ;; Lucky seeds:
      ;;:rand-seeds [236245406373, 565772875052]
      })

;;; Merge everything into the final configuration (could do this inside the run
;;; function for more REPL-versatility)
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


;;; Example of an evolution result, with instances of 'wrapped-gp-map renamed to
;;; just 'gp-map.
(def evolved-solution
     (fn gp-map [f coll]
       (when coll
	 (cons
	  (f (first (when coll coll)))
	  (when coll (gp-map f (next coll)))))))

;;; Again with redundant code removed
(def evolved-simplified
     (fn gp-map [f coll]
       (when coll
	 (cons
	  (f (first coll))
	  (gp-map f (next coll))))))