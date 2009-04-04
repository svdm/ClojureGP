
;;; cljgp.breeding.clj

(ns cljgp.breeding
  (:require [clojure.zip :as zip])
  (:use cljgp.random
	cljgp.util))

;;
;; Tree generation
;;

(defn generate-tree
  "Returns a tree generated from given func-set and term-set, which have to be
  nth-able. If 'method is :grow, grow method will be used to generate the tree
  up to max-depth in size. Else, 'full' method will be used, resulting in a tree
  with a depth equal to max-depth in all its branches.
  
  For max-depth < 0, will return a tree of size 1."
  [func-set term-set max-depth method]
  (if (or (<= max-depth 1)
	  (and (= method :grow)
	       (< (gp-rand) (/ (count term-set)
			       (+ (count term-set) (count func-set))))))
    (pick-rand term-set)
    (let [fnode (pick-rand func-set)]
      (cons fnode (for [i (range (:arity ^fnode))]
		    (generate-tree func-set term-set 
				   (dec max-depth) method))))))

(defn generate-ramped
  "Returns a tree generated using a version of the ramped half and half
  method. Tree will be generated from func-set and term-set, up to
  max-depth (inclusive) if given (default 7). The 'grow' method will be used
  half the time by default, or with the probability given as grow-chance."
  [max-depth grow-chance func-set term-set]
  (generate-tree func-set
		 term-set
		 (inc (gp-rand-int max-depth))
		 (if (< (gp-rand) grow-chance) :grow :full)))

(defn- ind-generator-seq
  "Returns a lazy infinite sequence of individuals with generation 0 and
  expression trees generated from the function- and terminal-set, all as
  specified in the given run-config. Used internally by generate-pop."
  [run-config]
  (let [{:keys [function-set terminal-set
		pop-generation-fn
		arg-list]} run-config]
    (repeatedly #(make-individual (pop-generation-fn function-set terminal-set)
				  0 arg-list))))

; Note that it is intentional that (ind-generator-seq ..) is called inside each
; future, even though one might think it's just a producer function that is
; identical between futures. The reason is that the closure appears to inherit
; the bindings as they are at the point where it is created, as opposed to that
; where it is called. Bindings are nefarious things.
(defn generate-pop
  "Returns a population of individuals generated from scratch out of the nodes
  in the function set and terminal set, using the tree producer function also
  specified in the run-config (typically ramped half and half). The work is
  divided over worker threads."
  [run-config]
  (let [{:keys [population-size threads rand-fns]} run-config
	per-future (divide-up population-size threads)]
    (mapcat deref
	    (doall 
	     (map #(future
		     (binding [cljgp.random/gp-rand %2]
		       (doall (take %1 (ind-generator-seq run-config)))))
		  per-future
		  rand-fns)))))

;;
;; Breeding
;;

(defn tree-replace
  "Returns given tree with node at idx replaced by new-node."
  [idx new-node tree]
  (let [i (atom -1)
	r-fn (fn do-replace [node]
	       (swap! i inc)
	       (cond
		 (= @i idx) new-node
		 (> @i idx) node	; means idx has already been replaced
		 (seq? node) (cons (first node)
				   (doall (map do-replace (next node))))
		 :else node))]
    (r-fn tree)))


; old, zipper-based tree-replace, much slower, to be removed later
(comment
  (defn tree-replace-alt
    "Returns given tree with the node at idx replaced by new-node."
    [idx new-node tree]
    (let [tree-zpr (zip/seq-zip tree)]
      (loop [loc tree-zpr, n 0]
	(cond 
	  (zip/end? loc) (zip/root loc)
	  (= n idx) (zip/root (zip/replace loc new-node))
	  :else (recur (if (zip/branch? loc) 
			 (zip/next (zip/next loc)) ; skip function symbol
			 (zip/next loc))
		       (inc n)))))))




;TODO/FIXME: crossover point selection is uniform instead of the traditional
;  90/10 split between nodes and leaves respectively
;  named appropriately for now
(defn crossover-uniform
  "Performs a subtree crossover operation on the given trees. Returns vector of
  two new trees."  
  [tree-a tree-b]
  (let [seq-a (make-tree-seq tree-a)
	seq-b (make-tree-seq tree-b)
	idx-a (gp-rand-int (count seq-a))
	idx-b (gp-rand-int (count seq-b))
	pick-a (nth seq-a idx-a)
	pick-b (nth seq-b idx-b)]
    [(tree-replace idx-a pick-b tree-a)
     (tree-replace idx-b pick-a tree-b)]))

(defn mutate
  "Performs a mutation operation on the given tree. Returns the new tree."
  [tree func-set term-set]
  (let [tree-seq (make-tree-seq tree)
	idx (gp-rand-int (count tree-seq))
	;pick (nth tree-seq idx)
	; having picked a mutation point, could use type data here in the future
	subtree (generate-tree func-set term-set 17 :grow)]
    (tree-replace idx subtree tree)))


; TODO: macro-ify the breeder-result-to-individual pattern? Problem: all three
; differ in subtle ways, leaving little to generalize.

(defn crossover-inds
  "Performs a crossover operation on the given individuals of the form (fn []
  tree) using given 'crossover-fn. Returns vector of two new individuals."
  [crossover-fn ind-a ind-b arg-list]
  (let [[tree-a tree-b] (crossover-fn (get-fn-body (:func ind-a))
				      (get-fn-body (:func ind-b)))
	gen (inc (:gen ind-a))]
    [(make-individual tree-a gen arg-list)
     (make-individual tree-b gen arg-list)]))

(defn mutate-ind
  "Performs mutation on given individual's tree. Returns seq with the new
  individual as single element (for easy compatibility with crossover-ind)."
  [func-set term-set ind arg-list]
  (let [tree (mutate (get-fn-body (:func ind))
		     func-set term-set)
	gen (inc (:gen ind))]
    [(make-individual tree gen arg-list)]))


(defn reproduce-ind
  "Performs direct reproduction, ie. breeds a new individual by directly copying
  the given individual's tree. Returns seq with new individual as single
  element."
  [ind arg-list]
  (let [tree (get-fn-body (:func ind))
	gen (inc (:gen ind))]
    [(make-individual tree gen arg-list)]))


(defn crossover-breeder
  "Selects two individuals from pop by applying the selection-fn specified in
   run-config to it twice, performs crossover and returns seq of two resulting
   new trees."
  [pop run-config]
  (let [{:keys [selection-fn arg-list]} run-config]
    (crossover-inds crossover-uniform
		    (selection-fn pop) (selection-fn pop)
		    arg-list)))

(defn mutation-breeder
  "Selects an individual from pop by applying selection-fn specified in the
  run-config to it, performs mutation and returns seq with the single resulting
  tree in it. Mutation will pull nodes from the function and terminal sets
  specified in run-config."
  [pop run-config]
  (let [{:keys [terminal-set function-set selection-fn arg-list]} run-config]
    (mutate-ind function-set terminal-set (selection-fn pop) arg-list)))

(defn reproduction-breeder
  "Selects an individual from pop by applying selection-fn specified in the
  run-config to it, returns seq with a new individual whose tree is identical to
  the selected individual."
  [pop run-config]
  (let [{:keys [selection-fn arg-list]} run-config]
    (reproduce-ind (selection-fn pop) arg-list)))

(defn- setup-breeders
  "Sets up (lazy-)seq of breeders (maps with a :prob key) in a seq in
  increasing summation, eg. three sources with probabilities [0.1 0.5 0.4] will
  become [0.1 0.6 1.0]. This makes source selection fast and easy. Probabilities
  have to add up to 1.0."
  [breeders] 
  (let [pr (fn process [bs p]
	     (lazy-seq
	       (when-let [s (seq bs)]
		 (let [b (first bs)
		       p-new (+ p (:prob b))]
		   (cons (assoc b :prob p-new)
			 (process (rest s) p-new))))))]
    (pr breeders 0)))

(defn- select-breeder
  "Returns a random breeder from a collection of them according to their :prob
  values. See 'setup-breeders."
  [breeders]
  (let [p (gp-rand)]
    (first (filter #(< p (:prob %)) breeders))))

(defn breed-new-pop
  "Returns a new population of equal size bred from the given evaluated pop by
   repeatedly applying a random breeder to pop-evaluated. Work is divided over
   worker threads."
  [pop-evaluated run-config]
  (let [{:keys [breeders population-size 
		threads rand-fns]} run-config
	bs (setup-breeders breeders)
	per-future (divide-up population-size threads)
	breed-generator (fn breed-new []
			  (lazy-seq
			    (when-let [breed (:breeder-fn (select-breeder bs))]
			      (concat (breed pop-evaluated run-config) 
				      (breed-new)))))]
    (mapcat deref
	    (doall
	     (map #(future
		     (binding [cljgp.random/gp-rand %2]
		       (doall (take %1 (breed-generator)))))
		  per-future
		  rand-fns)))))

