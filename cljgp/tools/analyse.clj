
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

(defn fitness-range
  "Returns map with lowest (best) and highest (worst) fitnesses in population,
  as {:lowest x, :highest y}. Bit more efficient than calling 'fitness-min and
  'fitness-max separately if you want both."
  [pop]
  (let [fs (map :fitness pop)]
    {:lowest (apply min fs)
     :highest (apply max fs)}))

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

