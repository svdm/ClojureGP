
;;; cljgp.tools.analyse.clj

(ns cljgp.tools.analyse
  "Basic analysis functions, see cljgp.tools.logging for example usage."
  (:use cljgp.util))

; TODO: func that returns [fit-min fit-avg fit-max] as efficiently as possible

(defn pop-average-of
  "Returns the average of the results of mapping func over pop."
  [func pop]
  (/ (reduce + (map func pop))
     (count pop)))

(defn pop-min-of
  "Returns the minimum of the results of mapping func over pop."
  [func pop]
  (apply min (map func pop)))

(defn pop-max-of
  "Returns the maximum of the results of mapping func over pop."
  [func pop]
  (apply max (map func pop)))

(defn fitness-average
  "Returns average fitness in population."
  [pop]
  (pop-average-of :fitness pop))

(defn fitness-min
  "Returns lowest (best) fitness value in population."
  [pop]
  (pop-min-of :fitness pop))

(defn fitness-max
  "Returns highest (worst) fitness value in population."
  [pop]
  (pop-max-of :fitness pop))

(defn fitness-range
  "Returns map with lowest (best) and highest (worst) fitnesses in population,
  as {:lowest x, :highest y}. Bit more efficient than calling 'fitness-min and
  'fitness-max separately if you want both."
  [pop]
  (let [fs (map :fitness pop)]
    {:lowest (apply min fs)
     :highest (apply max fs)}))


(defn tree-size
  "Returns number of nodes (both functions and terminals) in given individual's
  tree."
  [ind]
  (count (make-tree-seq (get-fn-body (:func ind)))))

(defn tree-size-average
  "Average 'tree-size in population."
  [pop]
  (pop-average-of tree-size pop))

(defn tree-size-min
  "Min. 'tree-size in population."
  [pop]
  (pop-min-of tree-size pop))

(defn tree-size-max
  "Max. 'tree-size in population."
  [pop]
  (pop-max-of tree-size pop))

; Also see cljgp.util.tree-depth
; TODO: add some min/max/avg functions for tree-depth