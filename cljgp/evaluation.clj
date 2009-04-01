
;;; cljgp.evaluation.clj
;;;
;;; Code concerning the evaluation of (populations of) individuals
;;;

(ns cljgp.evaluation
  (:use [cljgp.random :only (gp-rand)]
	[cljgp.util :only (partition-full)]))

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
  individual with a fitness value assigned."
  [evaluator ind]
  (let [func (eval (:func ind))
	fitness (try (evaluator func) 
		     (catch Exception e (report-exception e ind)))]
    (assoc ind :fitness fitness)))

(defn evaluate-pop
  "Takes a population (collection) of individuals and returns a new seq with all
  individuals evaluated using the evaluator fn defined in run-config. Work is
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


(defn best-fitness
  "Returns the individual with the best (lowest) fitness in the population."
  [pop]
  (first (sort-by :fitness pop))) ;FIXME: replace with something faster?