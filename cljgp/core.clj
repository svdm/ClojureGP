; TODO: headers

;;; cljgp.core.clj

(ns cljgp.core
  "Core functions that handle a GP run, creating a population, evaluating and
  breeding it until the end condition is reached."
  (:use cljgp.selection
	cljgp.breeding
	cljgp.evaluation
	[cljgp.config :only (prepare-config)]))


(defn evolve-future-generations
  "Returns lazy infinite sequence of generations (ie. consecutively evaluated
  and bred populations). See 'evaluate-pop and 'breed-new-pop for evaluation and
  breeding info.

  Take care when consuming this sequence: besides being infinite, large
  populations/generations (ie. the seq's elements) may quickly eat up your heap
  space even for shorter seqs."
  [pop run-config]
  (lazy-seq
    (when-let [pop-seq (seq pop)]
      (let [pop-evaluated (evaluate-pop pop-seq run-config)]
	(cons pop-evaluated
	      (evolve-future-generations (breed-new-pop pop-evaluated 
							run-config)
					 run-config))))))

(defn- take-until-end
  "As take-while, but lazy sequence includes the item for which end? first
  returns true. Also, items are assumed to be seqable (populations). The item
  for which end? returns true is seq'd, and the metadata {:final true} is
  attached. This is highly useful for functions mapped over the seq of
  generations that need to perform some finalization when the run is complete,
  eg. close a log file."
  [end? coll]
  (lazy-seq
    (when-let [s (seq coll)]
      (if (end? (first s))
	(cons (with-meta (seq (first s)) {:final true}) nil)
	(cons (first s) (take-until-end end? (rest s)))))))

(defn generate-run
  "Returns a lazy seq of successive generations (= populations = collections of
  individuals) in an evolution run whose parameters are defined in 'run-config.

  Besides validation, some preprocessing is performed on the 'run-config, for
  example to create and initialize the RNGs on a per-run basis. See
  cljgp.config/prepare-config for details.

  See cljgp.core/evolve-future-gens for more details on the returned lazy seq."
  [run-config]
  (let [config (prepare-config run-config)
	pop-initial (generate-pop config)
	end? (:end-condition config)]
    (take-until-end end?
		    (evolve-future-generations pop-initial config))))



