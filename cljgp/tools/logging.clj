
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
  (:import [java.io Writer BufferedWriter FileWriter]))

; TODO: needs evaluation of what functions are still useful, and general cleanup

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

(defn #^String stringify-ind-verbose
  "Prints data on given individual to a string. If show-ns is true, namespace
  info will be included in the printed tree."
  [show-ns ind]
  (let [added-data (dissoc ind :gen :fitness :func)]
    (str "Data of individual:\n"
	 " Generation: " (:gen ind) "\n"
	 " Fitness: " (:fitness ind) "\n"
	 (when (not (empty? added-data)) 
	   (str " Additional data: "
		(write added-data :stream nil) "\n"))
	 " Function:\n"
	 (print-tree nil show-ns (:func ind))
	 "\n")))

(defn print-ind-verbose
  "Prints individual as produced by 'stringify-ind-verbose."
  ([show-ns ind]
     (print (stringify-ind-verbose show-ns ind)))
  ([ind]
     (print-ind-verbose false ind)))

(defn setup-stats-map
  "Checks if generation has stats-map in metadata, if yes returns it. If no
  creates and adds one and returns the generation with new metadata."
  [generation]
  (let [gen-meta ^generation]
    (if (seq generation)
      (if (contains? gen-meta :stats-map)
	generation
	(with-meta generation (conj {:stats-map (make-stats-map generation)} 
				    gen-meta)))
      nil)))

(defn print-best
  "Prints best individual verbosely."
  [generation]
  (when-let [gen-seq (seq generation)]
    (let [stats (:stats-map ^(setup-stats-map gen-seq))]
      (print-ind-verbose false (get-stat stats :best-fitness)))))

(defn #^String stat-string
  "Returns a string containing basic stats/info on the given
  generation/population. Ends with a newline.

  Assumes calling function has handled 'setup-stats-map, as it is used here."
  [generation treestats?]
  (if-let [gen-seq (seq generation)]
    (let [gen-num (:gen (first gen-seq))
	  stats (:stats-map ^gen-seq)]
      (str 
       (format "Gen %1$03d: Best: %2$.2f -- Worst: %3$.2f -- Avg: %4$.2f\n"
	       gen-num 
	       (float (get-stat stats :fitness-min)) 
	       (float (get-stat stats :fitness-max)) 
	       (float (get-stat stats :fitness-avg)))
       (when treestats?
	 (format "\t Trees: avg size: %1$.2f -- avg depth: %1$.2f\n" 
		 (float (get-stat stats :tree-size-avg)) 
		 (float (get-stat stats :tree-depth-avg))))))
    "")) ; generation is nil

(defn #^String stat-string-verbose
  "Return a formatted multi-line string with stats on fitness and tree size."
  [generation]
  (if-let [gen-seq (seq generation)]
    (let [gen-num (:gen (first gen-seq))
	  stats (:stats-map ^gen-seq)]
      (str 
       (format (str 
		"=================\n"
		"Generation %1$03d\n"
		"=================\n"
		"Trees:\n"
		"  Avg size:\t%2$.4f\n"
		"  Avg depth:\t%3$.4f\n"
		"\n"
		"Fitness:\n"
		"  Best: %4$.4f\n"
		"  Worst: %5$.4f\n"
		"  Avg: %6$.4f\n"
		"\n"
		"Best individual of generation:\n")
	       gen-num 
	       (float (get-stat stats :tree-size-avg)) 
	       (float (get-stat stats :tree-depth-avg))
	       (float (get-stat stats :fitness-min)) 
	       (float (get-stat stats :fitness-max)) 
	       (float (get-stat stats :fitness-avg)))
       (stringify-ind-verbose false (get-stat stats :best-fitness))
       "\n\n"))
    ""))

(defn print-stats-basic
  "Prints basic information to *out* about given generation (= population),
  including the number of the generation and the best/worst/average fitness.

  Typical usage is mapping this function over the lazy seq returned by
  cljgp.core.generate-run. Returns given generation to allow chaining of
  multiple functions that do the same (presumably with various side effects like
  logging to files, showing graphs, etc.)."
  [generation]
  (when-let [gen-seq (seq generation)]
    (let [gen (setup-stats-map gen-seq)]
      (print (stat-string gen false))
      (flush)
      gen)))		; let seq pass through after performing side effects

(defn print-stats-full
  "Like print-stats-basic, but also includes additional stats that may be
  computationally costly in some experiments, specifically average tree size and
  depth."
  [generation]
  (when-let [gen-seq (seq generation)]
    (let [gen (setup-stats-map gen-seq)]
      (print (stat-string gen true))
      (flush)
      gen)))

; Let the caller provide a writer here, because we don't know when the run is
; over and the writer can be closed.
(defn log-stats
  "Returns a function that writes basic information about a given generation to
  a file."
  ([#^String filename]
     (log-stats filename true false))
  ([#^String filename flush-every? treestats?]
     (let [writer (BufferedWriter. (FileWriter. filename))]
       (fn file-logger [generation]
	 (let [gen (setup-stats-map (seq generation))]
	   (when (seq gen)
	     (.write writer (stat-string gen treestats?))
	     (when flush-every? 
	       (.flush writer))
	     (when (:final ^gen)
	       (.write writer "\n\nBest individual:\n")
	       (.write writer 
		       (stringify-ind-verbose false 
					      (get-stat (:stats-map ^gen) 
							:best-fitness)))
	       (.close writer))
	     gen))))))


