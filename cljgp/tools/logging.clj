
;;; cljgp.tools.logging.clj

(ns cljgp.tools.logging
  (:use cljgp.tools.analyse))

; Examples of or simple default versions of logging functions, see bottom of
; file for example usage.

(defn print-details
  "Prints basic information to *out* about given generation (= population),
  including the number of the generation, the best/worst/average fitness, and
  average tree size.

  Typical usage is mapping this function over the lazy seq returned by
  cljgp.core.generate-run. Returns given generation to allow chaining of
  multiple functions that do the same (presumably with various side effects like
  logging to files, showing graphs, etc.)."
  [generation]
  (when-let [gen-seq (seq generation)]
    (let [gen-num (:gen (first gen-seq))
	  average (fitness-average gen-seq)
	  {:keys [highest lowest]} (fitness-range gen-seq)
	  avg-tree (tree-size-average gen-seq)]
      (println 
       (format "Gen %1$03d: Best: %2$.2f -- Worst: %3$.2f -- Avg: %4$.2f"
	       gen-num (float lowest) (float highest) (float average)))
      (println (format "\t Tree size avg: %1$.2f" (float avg-tree)))))
  generation) ; let seq pass through after performing the side effects


(comment
; Example usage:
  (best-fitness
   (last (map log-details 
	      (map print-details 
		   (map graph-fitness (generate-run ...))))))
)
; The (last ..) call forces the lazy seq of generations to realize, and each
; realized generation gets passed through graph-fitness, print-details and
; log-details, before being discarded by last unless it is in fact the last
; generation. This means that side effects will be performed as soon as each
; generation is computed (which might be counter-intuitive at first) due to 
; the lazy nature of the seq of generations returned by generate-run.
