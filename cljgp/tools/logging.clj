
;;; cljgp.tools.logging.clj

(ns cljgp.tools.logging
  "Facilities for printing statistics and information relating to GP runs and
  their results. Includes examples/simple default versions of logging functions.

  Example usage of showing statistics during a run:
  (last (map <file logger fn>
             (map print-details 
                  (map <graph plotter fn>
                       (generate-run ...)))))

  The (last ..) call forces the lazy seq of generations to realize, and each
  realized generation gets passed through a graph plotter, 'print-details and
  a file logger, before being discarded by 'last unless it is in fact the last
  generation. This means that side effects will be performed as soon as each
  generation is computed (which might be counter-intuitive at first) due to the
  laziness of the seq of generations returned by generate-run."
  (:use cljgp.tools.analyse)
  (:use clojure.contrib.pprint)
  (:import [java.io Writer]))

(defn print-tree
  "Pretty-prints an evolved function/tree."
  ([writer show-ns tree]
     (write tree 
	    :dispatch *code-dispatch*
	    :suppress-namespaces (false? show-ns)
	    :pretty true
	    :stream writer))
  ([show-ns tree] 
     (print-tree true show-ns tree)))

; TODO: merge -verbose in using a :print-type arg?
(defn print-individual
  "Pretty-prints an individual as a map. Re-readable if show-ns is true (default
  false)."
  ([show-ns ind]
     (print-tree show-ns ind))

  ([ind] 
     (print-individual false ind)))

(defn print-individual-verbose
  "Prints an individual in verbose human-readable form."
  ([#^Writer writer show-ns ind]
     (let [added-data (dissoc ind :gen :fitness :func)]
       (.write writer 
	       (str "Data of individual:\n"
		    " Generation: " (:gen ind) "\n"
		    " Fitness: " (:fitness ind) "\n"
		    (when (not (empty? added-data)) 
		      (str " Additional data: "
			   (write added-data :stream nil) "\n"))
		    " Function:\n")))
     (print-tree writer show-ns (:func ind))
     (.write writer "\n")
     (.flush writer))
  ([writer ind]
     (print-individual-verbose writer false ind)))

; Neither of these are particularly pretty and should probably be replaced at
; some point.

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

; Let the caller provide a writer here, because we don't know when the run is
; over and the writer can be closed.
(defn log-details
  "Returns a function that writes basic information about a given generation to
  a given (file)writer. See 'print-details."
  ([#^Writer writer]
     (log-details writer false))
  ([#^Writer writer flush-every]
     (fn file-logger [generation]
       (when-let [gen-seq (seq generation)]
	 (let [gen-num (:gen (first gen-seq))
	       average (fitness-average gen-seq)
	       {:keys [highest lowest]} (fitness-range gen-seq)]
	   (.write writer
		   (format (str "Gen %1$03d: Best: %2$.2f\t"
				" -- Worst: %3$.2f\t"
				" -- Avg: %4$.2f\n")
			   gen-num 
			   (float lowest) 
			   (float highest) 
			   (float average)))
	   (when flush-every 
	     (.flush writer))))
       generation)))

