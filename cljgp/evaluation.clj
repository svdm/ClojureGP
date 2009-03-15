
;;; cljgp.evaluation.clj

(ns cljgp.evaluation)

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

; TODO: threading? pmap?
(defn evaluate-pop
  "Takes a population (collection) of individuals and returns a new seq
  with all individuals evaluated using the given evaluator fn."
  [evaluator pop]
  (map (partial evaluate-ind evaluator) pop))