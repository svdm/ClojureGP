
;;; cljgp.selection.clj

(ns cljgp.selection
  "Default selection functionality."
  (:use cljgp.random
	[cljgp.tools.analyse :only (best-fitness)]))

(defn tournament-select
  "Selects 'tsize individuals from the given population 'pop and returns the one
  with the best (lowest) fitness. May select duplicates into a tournament."
  [tsize pop]
  (let [pop-size (count pop)
	competitors (take tsize 
			  (repeatedly #(nth pop (gp-rand-int pop-size))))]
    (best-fitness competitors)))