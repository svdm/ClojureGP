
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
  "Returns a seq of a given S-exp-based tree using (tree-seq coll? next tree)."
  [tree]
  `(tree-seq coll? next ~tree))

(defn tree-depth
  "Returns the depth of the given tree"
  [node]
  (if (seq? node)
    (if (> (count node) 1)
      (+ 1 (apply max (map tree-depth (next node))))
      (count node))
    1))


; I assume there's an easier way to do this, but I can't think of any. 
(defn divide-up
  "Divides some number up into a seq of numbers of length 'pieces, with each
  element at most (ceil (/ num pieces)) in size. Essentially tries to do the
  equivalent of applying (partition ..) to the interval 0..num."
  [num pieces]
  (let [per (Math/ceil (/ num pieces))]
    (loop [s []
	   n num]
      (if (<= n 0)
	s
	(recur (conj s (min per n)) (- n per))))))

(defn partition-full
  "As (partition ..) but includes last partition if smaller than n."
  [n coll]
  (lazy-seq
    (when-let [s (seq coll)]
      (cons (take n s) (partition-full n (drop n s))))))