; TODO: headers

;;; cljgp.core.clj

(ns cljgp.core
  "Core functions that handle a GP run, creating a population, evaluating and
  breeding it until the end condition is reached."
  (:use cljgp.selection
	cljgp.breeding
	cljgp.evaluation
	[cljgp.config :only (prepare-config)]))


(defn evolve-future-gens
  "Returns lazy infinite sequence of generations (ie. consecutively evaluated
  and bred populations). See 'evaluate-pop and 'breed-new-pop for evaluation and
  breeding info.

  Take care when consuming this sequence: besides being infinite, large
  populations (ie. the seq's elements) may quickly eat up your heap space even
  for shorter seqs."
  [pop run-config]
  (lazy-seq
    (when-let [pop-seq (seq pop)]
      (comment (println "Realizing generation..."))
      (let [pop-evaluated (evaluate-pop pop-seq run-config)]
	(cons pop-evaluated
	      (evolve-future-gens (breed-new-pop pop-evaluated run-config)
				  run-config))))))

; I need the below because when using take-while with a fitness-based end
; condition predicate, it returns the sequence of generations up to and *not
; including* the sequence for which the predicate returns false. Hence, when it
; finds the first generation with perfect fitness, it stops there and doesn't
; actually cons it onto the sequence. Here I define a version that is otherwise
; identical to take-while, but includes every item in coll tested by pred.
(defn- take-while-eager
  "As take-while, but lazy sequence includes the item for which pred first
  returns false."
  [pred coll]
  (lazy-seq
    (when-let [s (seq coll)]
      (cons (first s) (when (pred (first s))
			(take-while-eager pred (rest s)))))))

(defn generate-run
  "Returns a lazy seq of successive generations in an evolution run whose
  parameters are defined in 'run-config.

  Besides validation, some preprocessing is performed on the 'run-config,
  currently only to create and initialize the RNGs on a per-run basis.

  See 'evolve-future-gens for more details on the returned lazy seq."
  [run-config]
  (let [config (prepare-config run-config)
	pop-initial (generate-pop config)
	end? (:end-condition config)]
    (take-while-eager (complement end?)
		      (evolve-future-gens pop-initial config))))



