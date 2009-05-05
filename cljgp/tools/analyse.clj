
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

; What terrible macros hath I wrought..

(defmacro make-pop-stat
  "Example:
    (make-pop-stat my-fun my-fun-name pop-average-of \"avg\")
  Expands to:
    (defn my-fun-name-avg
      \"Population avg of my-fun-name\"
      [pop]
      (pop-average-of my-fun pop))"
  [fsymbol fname popfn statname]
  `(defn ~(symbol (str fname "-" statname))
     ~(str "Population " statname " of " fname)
     ~['pop]
     (~popfn ~fsymbol ~'pop)))

(defmacro make-pop-stat-all
  "Defines population average/min/max functions. See make-pop-stat.

  Basically, given eg.:
    (make-pop-stat-all tree-size-ind \"tree-size\")
  Produces three functions:
    tree-size-average
    tree-size-min
    tree-size-max
  That take a population as argument and apply (pop-average-of tree-size pop),
  and the same for min and max."
  ([fsymbol]
     `(make-pop-stat-all ~fsymbol ~(name fsymbol)))
  ([fsymbol fname]
     `(do
	(make-pop-stat ~fsymbol ~fname pop-average-of "average")
	(make-pop-stat ~fsymbol ~fname pop-min-of "min")
	(make-pop-stat ~fsymbol ~fname pop-max-of "max"))))

; ... They let me be wonderfully lazy though:

; tree-depth-average, tree-depth-minimum, tree-depth-maximum
(make-pop-stat-all tree-depth-ind "tree-depth")
; tree-size-average, tree-size-minimum, tree-size-maximum
(make-pop-stat-all tree-size-ind "tree-size")
; fitness-average, fitness-minimum, fitness-maximum
(make-pop-stat-all :fitness)

