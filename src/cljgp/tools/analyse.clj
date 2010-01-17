;; Copyright (c) Stefan A. van der Meer. All rights reserved.
;; The use and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which
;; can be found in the file epl-v10.html at the root of this distribution. By
;; using this software in any fashion, you are agreeing to be bound by the
;; terms of this license. You must not remove this notice, or any other, from
;; this software.

(ns cljgp.tools.analyse
  "Basic analysis functions, see cljgp.tools.logging for example usage."
  (:use cljgp.util))

(declare setup-stats-map get-stat)

(defn find-best-of-run
  "Helper fn meant for use in reduce, eg:
    (reduce find-best-of-run (map print-stats (generate-run ...)

  Given an individual 'best-yet and a 'generation, returns individual with the
  best fitness of both the generation and 'best-yet. In the reduce context, this
  is the new best-yet.

  Given two generations, returns the best individual of both. Hence, when used
  in a reduce it does not need to be given a starting val (but you can use 'nil
  if you want).

  Also see cljgp.tools.logging/reduce-to-summary."
  [best-yet generation]
  (if (not (or (map? best-yet) (nil? best-yet)))
    ;; handle reduce with no start val, ie. both initial params are generations
    (let [gen (setup-stats-map best-yet)
          stats (:stats-map (meta gen))
          best-gen (get-stat stats :best-fitness)]
      (find-best-of-run best-gen generation))
    ;; handle standard case
    (let [gen (setup-stats-map generation)
          stats (:stats-map (meta gen))
          best-gen (get-stat stats :best-fitness)
          best-new (if best-yet
                     (min-key :fitness best-yet best-gen)
                     best-gen)]
      best-new)))

(defn pop-avg-of
  "Returns the average of the results of mapping 'func over 'pop."
  [func pop]
  (/ (reduce + (map func pop))
     (count pop)))

;;; pop-min-of and max-of are not very efficient, but will likely suffice for
;;; basic stat gathering
(defn pop-min-of
  "Returns the minimum of the results of mapping 'func over 'pop."
  [func pop]
  (apply min (map func pop)))

(defn pop-max-of
  "Returns the maximum of the results of mapping 'func over 'pop."
  [func pop]
  (apply max (map func pop)))

;;; First iteration used a sort-by on :fitness, second was a much faster but
;;; uglier manual loop-recur, this version seems to be best of both worlds.
(defn best-fitness
  "Returns the individual with the best (lowest) fitness in the population."
  [pop]
  (apply (partial min-key get-fitness) pop)) ; min-key works fine with accessors

(defn tree-size-ind
  "Returns number of nodes (both functions and terminals) in given individual's
  tree."
  [ind]
  (tree-size (get-fn-body (get-func ind))))

(defn tree-depth-ind
  "Returns max depth of given individual's tree."
  [ind]
  (tree-depth (get-fn-body (get-func ind))))

(defn fitness-all
  "Returns minimum, maximum and average fitness in a single loop over the
  population. This is much faster than calling fitness-min, fitness-max, and
  fitness-avg separately."              ; of course, fast is not always pretty
  [pop]
  (loop [fit-ind (float (get-fitness (first pop)))
         poprest (rest pop)
         fit-min Float/MAX_VALUE
         fit-max Float/MIN_VALUE
         fit-tot (float 0)]
    (let [fmin (float (min fit-min fit-ind))
          fmax (float (max fit-max fit-ind))
          ftot (+ fit-tot fit-ind)]
      (if (seq poprest)
        (recur (float (get-fitness (first poprest))) 
               (rest poprest)
               fmin fmax ftot)
        {:fit-min fmin 
         :fit-max fmax 
         :fit-avg (/ ftot (count pop))}))))

; These defs were macro'd, but this seemed to obfuscate things unnecessarily
(def fitness-avg (partial pop-avg-of get-fitness))
(def fitness-min (partial pop-min-of get-fitness))
(def fitness-max (partial pop-max-of get-fitness))

(def tree-depth-avg (partial pop-avg-of tree-depth-ind))
(def tree-depth-min (partial pop-min-of tree-depth-ind))
(def tree-depth-max (partial pop-max-of tree-depth-ind))

(def tree-size-avg (partial pop-avg-of tree-size-ind))
(def tree-size-min (partial pop-min-of tree-size-ind))
(def tree-size-max (partial pop-max-of tree-size-ind))

;;;;
;;;; STATS-MAP
;;;;

; List of stats that will be present in the stats-map
(def *all-stats*
     `[fitness-avg fitness-min fitness-max fitness-all
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

(defn setup-stats-map
  "Checks if generation has stats-map in metadata, if yes returns it. If no
  creates and adds one and returns the generation with new metadata.

  Calls seq on generation and returns this seq'd generation, meaning it can be
  used directly in when-let/if-let."
  [generation]
  (when-let [gen (seq generation)]
    (let [gen-meta (meta gen)]
      (if (contains? gen-meta :stats-map)
        gen
        (with-meta gen (conj {:stats-map (make-stats-map gen)} 
                             gen-meta))))))




