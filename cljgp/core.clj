; TODO: headers

;;; cljgp.core.clj

(ns cljgp.core
  (:use cljgp.random
	cljgp.selection
	cljgp.breeding
	cljgp.evaluation))


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
      (let [pop-evaluated (evaluate-pop (:evaluation-fn run-config) pop-seq)]
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
  "Returns a lazy seq of successive generations in an evolution run until
  'reached-end-condition? returns true. Initial population (of size 'pop-size)
  will be generated from 'func-set and 'term-set. Evaluation will be performed
  using the 'evaluator function, and new populations will be bred using the
  breeding functions given as 'breeders.

  See 'evolve-future-gens for more details on the returned lazy seq."
  [run-config]
  (let [pop-initial (generate-pop run-config)
	end? (:end-condition run-config)]
    (take-while-eager (complement end?)
		      (evolve-future-gens pop-initial run-config))))

; TODO: move into config
(defn make-simple-end
  "Returns a simple end condition predicate that stops the evolution when the
  given max number of generations is reached, or the fitness of any individual
  in a generation is lower than (+ 0 fit-tolerance), where fit-tolerance is
  0.0001 by default."
  ([max-generations fit-tolerance]
     (fn [pop]
       (let [ind (first pop)]
	 (or (>= (:gen ind) max-generations)
	     (some #(>= fit-tolerance (:fitness %)) pop)))))
  ([max-generations] (make-simple-end max-generations 0.0001)))

