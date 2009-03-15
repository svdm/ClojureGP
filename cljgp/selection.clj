
;;; cljgp.selection.clj

(ns cljgp.selection
  (:use cljgp.random))

(defn tournament-select
  "Selects tsize individuals from the given population pop and returns
  the one with the best (lowest) fitness."
  [tsize pop] ;ordered such for (partial ..) use
  (let [pop-size (count pop)
	competitors (take tsize 
			  (repeatedly #(nth pop (gp-rand-int pop-size))))]
    (first (sort-by :fitness competitors))))