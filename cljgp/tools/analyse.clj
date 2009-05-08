
;;; cljgp.tools.analyse.clj

(ns cljgp.tools.analyse
  "Basic analysis functions, see cljgp.tools.logging for example usage."
  (:use cljgp.util))

; TODO: func that returns [fit-min fit-avg fit-max] as efficiently as possible

(defn pop-avg-of
  "Returns the average of the results of mapping func over pop."
  [func pop]
  (/ (reduce + (map func pop))
     (count pop)))

; pop-min-of and max-of are not very efficient, but will likely suffice for
; basic stat gathering
(defn pop-min-of
  "Returns the minimum of the results of mapping func over pop."
  [func pop]
  (apply min (map func pop)))

(defn pop-max-of
  "Returns the maximum of the results of mapping func over pop."
  [func pop]
  (apply max (map func pop)))

; First iteration used a sort-by on :fitness, second was a much faster but
; uglier manual loop-recur, this version seems to be best of both worlds.
(defn best-fitness
  "Returns the individual with the best (lowest) fitness in the population."
  [pop]
  (apply (partial min-key :fitness) pop))

(defn tree-size-ind
  "Returns number of nodes (both functions and terminals) in given individual's
  tree."
  [ind]
  (tree-size (get-fn-body (:func ind))))

(defn tree-depth-ind
  "Returns max depth of given individual's tree."
  [ind]
  (tree-depth (get-fn-body (:func ind))))

; These defs were macro'd, but this seemed to obfuscate things unnecessarily
(def fitness-avg (partial pop-avg-of :fitness))
(def fitness-min (partial pop-min-of :fitness))
(def fitness-max (partial pop-max-of :fitness))

(def tree-depth-avg (partial pop-avg-of tree-depth-ind))
(def tree-depth-min (partial pop-min-of tree-depth-ind))
(def tree-depth-max (partial pop-max-of tree-depth-ind))

(def tree-size-avg (partial pop-avg-of tree-size-ind))
(def tree-size-min (partial pop-min-of tree-size-ind))
(def tree-size-max (partial pop-max-of tree-size-ind))

(def *all-stats*
     `[fitness-avg fitness-min fitness-max
       tree-depth-avg tree-depth-min tree-depth-max
       tree-size-avg tree-size-min tree-size-max
       best-fitness])

(defmacro make-stats-map
  "Build a map of stat function names to delayed applications of those functions
  to the given population.

  By storing this map in metadata, we can avoid re-calculating the statistics
  for different logging/output functions that report on the same seq."
  [pop]
  (reduce (fn [m func] 
	     (conj m [(keyword (name func)) `(delay (~func ~pop))])) 
	   {} 
	   *all-stats*))

(defn get-stat
  "Gets a key from the stats map, which consists of delays that need forcing."
  [stats-map stat-key]
  (force (stat-key stats-map)))