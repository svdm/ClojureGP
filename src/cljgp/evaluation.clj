;; Copyright (c) Stefan A. van der Meer. All rights reserved.
;; The use and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which
;; can be found in the file epl-v10.html at the root of this distribution. By
;; using this software in any fashion, you are agreeing to be bound by the
;; terms of this license. You must not remove this notice, or any other, from
;; this software.

(ns cljgp.evaluation
  "Functionality concerning the evaluation of (populations of) individuals."
  (:use [cljgp.random :only (gp-rand)]
	cljgp.util
	[cljgp.tools.logging :only (print-code)]
	[clojure.contrib.seq-utils :only (partition-all)]))

; It's recommended that evaluator functions handle any exceptions they might
; expect themselves, for example by catching them and giving the individual a
; terrible fitness as a result. All uncaught exceptions are assumed to be bugs,
; so the below prints some potentially useful debugging info on the individual
; that caused the exception.
(defmacro report-exception
  "Reports some info on the individual that caused exception e, then throws the
  exception up."
  [e ind msg]
  `(do
     (println ~msg (str ~e))
     (println "\tCaused by:\n" (print-code ~ind))
     (throw ~e)))


(defn evaluate-individual
  "Evaluate an individual using the given evaluation function. Returns
  individual with its fitness value as returned by the 'evaluator.

  If the evaluator returns a map instead of a number, the map is merged into the
  individual. This allows storing additional evaluation details such as \"hits\"
  or raw score (eg. for analysis or customized selection)."
  [ind evaluator]
  (let [func (try 
	      (eval (get-func ind))	; compile tree
	      (catch Exception e 
		(report-exception e ind 
				  "Exception in (eval ..) of individual: ")))
	result (try 
		(evaluator func)	; execute user's evaluation
		(catch Exception e 
		  (report-exception e ind
				    "Exception during evaluation: ")))]
    (cond 
      (map? result) (conj ind result)
      (number? result) (assoc ind :fitness result)
      :else (throw (IllegalArgumentException. 
		    (str "Evaluator must return number or map. Returned: " 
			 result))))))

; The doall calls here force the future-related work to happen when (ie. now)
; and where (ie. inside bindings) we want the creation/computation to happen.
(defn evaluate-pop
  "Takes a population (collection) of individuals and returns a new seq with all
  individuals evaluated using the evaluator fn defined in 'run-config. Work is
  divided over worker threads."
  [pop {:as run-config
	:keys [threads, evaluation-fn, rand-fns]}]
  (let [per-future (Math/ceil (/ (count pop) threads))
	e-fn evaluation-fn]
    (mapcat deref
	 (doall
	  (map (fn [inds rand-fn]
		 (future
		   (binding [cljgp.random/gp-rand rand-fn]
		     (doall
		      (map #(evaluate-individual % e-fn) inds)))))
	       (partition-all per-future pop)
	       rand-fns)))))

