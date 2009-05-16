
;;; cljgp.util.clj

(ns cljgp.util
  "Utility functions and macros."
  (:use [clojure.contrib.def :only (defvar defalias)])
  (:require clojure.contrib.seq-utils)
  (:use [cljgp.random :only (pick-rand)]))


(defstruct individual :func :gen :fitness)

(defvar get-func (accessor individual :func)
  "Accessor for an individual's :func")

(defvar get-gen (accessor individual :gen)
  "Accessor for an individual's :gen")

(defvar get-fitness (accessor individual :fitness)
  "Accessor for an individual's :fitness")

(defn make-individual
  "Returns structmap representing an individual in the population. Takes the
  individual's generation and a 'fn-form that is the individual's :func. The
  fn-form should be an s-expression that evals into a function, and will
  typically look like (fn NAME [ARGS] ...). See
  cljgp.config/make-func-template."
  ([fn-form gen]
     (assert (list? fn-form))
     (struct-map individual
       :func fn-form
       :gen gen
       :fitness nil))
  ([fn-form]
     (make-individual fn-form 0)))

(defn get-fn-body
  "Returns the body of a quoted function definition of the form (fn [..]
  body). In GP terms, returns the expression tree of an individual's :func
  value."
  [quoted-fn]
  (nth quoted-fn 2 '()))

; TODO: add (comp get-fn-body get-fn) ?

(defmacro make-tree-seq
  "Returns a seq of a given S-exp-based tree using (tree-seq coll? next tree)."
  [tree]
  `(tree-seq coll? next ~tree))

(defn tree-depth
  "Returns the max depth of the given (sub)tree"
  [node]
  (if (coll? node)
    (if (> (count node) 1)
      (+ 1 (apply max (map tree-depth (next node))))
      (count node))
    1))

(defn tree-size
  "Returns number of nodes in given (sub)tree."
  [node]
  (count (make-tree-seq node)))

; I assume there's an easier way to do this, but I can't think of any. 
(defn divide-up
  "Divides some number up into a seq of numbers of length 'pieces, with each
  element at most (ceil (/ num pieces)) in size. Essentially tries to do the
  equivalent of applying (partition ..) to the numeric interval 0..num."
  [num pieces]
  (let [per (Math/ceil (/ num pieces))]
    (loop [s []
	   n num]
      (if (<= n 0)
	s
	(recur (conj s (min per n)) (- n per))))))

; Had custom partition-full fn before decision to depend on contrib was made.
; Now interning partition-all here in an attempt to isolate dependency to 
; cljgp.util and cljgp.tools.*, for now.
(defalias partition-all clojure.contrib.seq-utils/partition-all)

(defn gp-type
  "Returns type of given node, which is identical to (type (first node)) if it
  is a coll (ie. non-terminal node) and (type node) if it is a terminal."
  [node]
  (if (coll? node)
    (type (first node))
    (type node)))

(defn pick-rand-typed
  "Returns a random item from the collection that is of the given type. 

  As 'pick-rand, but first filters the seq to only include items of the given
  type 't (or a subtype of it)."
  [t coll]
  (pick-rand (filter #(isa? (type %) t) coll)))