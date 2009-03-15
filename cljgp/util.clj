
;;; cljgp.util.clj

(ns cljgp.util)

(defn get-fn-body
  "Returns the body of a quoted function definition of the form (fn [..]
  body). In GP terms, returns the expression tree of an individual."
  [quoted-fn]
  (nth quoted-fn 2 '()))

(defmacro make-tree-seq
  "Returns a seq of a given S-exp-based tree using (tree-seq seq? next tree)."
  [tree]
  `(tree-seq seq? next ~tree))
