
;;; cljgp.evaluation.clj

(ns cljgp.evaluation
  "Functionality concerning the evaluation of (populations of) individuals."
  (:use [cljgp.random :only (gp-rand)]
	cljgp.util))

; It's recommended that evaluator functions handle any exceptions they might
; expect themselves, for example by catching them and giving the individual a
; terrible fitness as a result. All uncaught exceptions are assumed to be bugs,
; so the below prints some potentially useful debugging info on the individual
; that caused the exception.
(defmacro report-exception
  "Reports some info on the individual that caused exception e, then throws the
  exception up."
  [e ind]
  `(do
     (println "Exception during evaluation:" (str ~e))
     (println "\tCaused by:" ~ind)
     (throw ~e)))

(defn evaluate-ind
  "Evaluate an individual using the given evaluation function. Returns
  individual with its fitness value as returned by the 'evaluator.

  If the evaluator returns a map instead of a number, the map is merged into the
  individual. This allows storing additional evaluation details such as \"hits\"
  or raw score (eg. for analysis or customized selection)."
  [evaluator ind]
  (let [func (eval (get-func ind))
	result (try (evaluator func) 
		    (catch Exception e (report-exception e ind)))]
    (cond 
      (map? result) (conj ind result)
      (number? result) (assoc ind :fitness result)
      :else (throw (IllegalArgumentException. 
		    (str "Evaluator must return number or map. Returned: " 
			 result))))))

(defn evaluate-pop
  "Takes a population (collection) of individuals and returns a new seq with all
  individuals evaluated using the evaluator fn defined in 'run-config. Work is
  divided over worker threads."
  [pop run-config]
  (let [num-futures (:threads run-config)
	per-future (Math/ceil (/ (count pop) num-futures))
	e-fn (:evaluation-fn run-config)]
    (mapcat deref
	 (doall			    ; force creation of futures 
	  (map #(future
		  (binding [cljgp.random/gp-rand %2]
		    (doall	    ; force actual evaluation to occur in future
		     (map (partial evaluate-ind e-fn) %1))))
	       (partition-full per-future pop)
	       (:rand-fns run-config))))))


