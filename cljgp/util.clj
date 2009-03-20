
;;; cljgp.util.clj

(ns cljgp.util)

(defn make-individual
  "Returns map representing an individual in the population."
  [tree gen arg-list]
  {:func `(fn ~arg-list ~tree)
   :gen gen
   :fitness nil})

(defn get-fn-body
  "Returns the body of a quoted function definition of the form (fn [..]
  body). In GP terms, returns the expression tree of an individual."
  [quoted-fn]
  (nth quoted-fn 2 '()))

(defmacro make-tree-seq
  "Returns a seq of a given S-exp-based tree using (tree-seq seq? next tree)."
  [tree]
  `(tree-seq seq? next ~tree))

(defn tree-depth
  "Returns the depth of the given tree"
  [node]
  (if (seq? node)
    (if (> (count node) 1)
      (+ 1 (apply max (map tree-depth (next node))))
      (count node))
    1))