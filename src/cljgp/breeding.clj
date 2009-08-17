;; Copyright (c) Stefan A. van der Meer. All rights reserved.
;; The use and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which
;; can be found in the file epl-v10.html at the root of this distribution. By
;; using this software in any fashion, you are agreeing to be bound by the
;; terms of this license. You must not remove this notice, or any other, from
;; this software.


(ns cljgp.breeding
  "Facilities for generating an initial population and breeding an evaluated
  one, including some standard breeding procedures."
  (:use cljgp.generate
        cljgp.random
        cljgp.util))

(declare crossover-individuals crossover-uniform
         mutate-individual reproduce-individual 
         hoist-individual point-mutate-individual
         shrink-individual)

;;;; Breeder-fns

(defn crossover-breeder
  "Selects two individuals from pop by applying the selection-fn specified in
   run-config to it twice, performs subtree crossover and returns seq of two
   resulting new trees."
  [pop {:as run-config,
        select :selection-fn}]
  (crossover-individuals crossover-uniform
                         [(select pop) (select pop)]
                         run-config))

(defn mutation-breeder
  "Selects an individual from pop by applying selection-fn specified in the
  run-config to it, performs subtree mutation and returns seq with the single
  resulting tree in it. Mutation will use nodes from the function and terminal
  sets specified in run-config. Generated (sub)tree will be at most max-depth
  deep."
  ([{max-depth :max-depth, :or {max-depth 17}} ; extra configuration
    pop {:as run-config,
         select :selection-fn}]
     (mutate-individual (select pop) max-depth run-config))
  ([pop run-config]
     (mutation-breeder nil pop run-config)))

(defn reproduction-breeder
  "Selects an individual from pop by applying selection-fn specified in the
  run-config to it, returns seq with a new individual whose tree is identical to
  the selected individual."
  [pop {:as run-config,
        select :selection-fn}]
  (reproduce-individual (select pop) run-config))

(defn hoist-breeder
  "Selects an individual from pop by applying the selection-fn specified in the
  given run-config to it. Performs a hoist mutation and returns seq with the
  single resulting individual in it.

  Hoist mutation will create a new individual which is a copy of a randomly
  selected subtree of the parent. Essentially picks a random point in the tree
  and \"hoists\" it into the root position. The aim is to reduce tree size."
  [pop {:as run-config,
        select :selection-fn}]
  (hoist-individual (select pop) run-config))

(defn point-mutation-breeder
  "Selects an individual from pop by applying the selection-fn specified in the
  given run-config to it. Performs a point mutation and returns seq with the
  single resulting individual in it.

  Point mutation will replace a node in the tree with a different one without
  modifying the rest of the tree. This is only useful if multiple function and
  terminal nodes exist with identical (gp-)types, arg counts and arg types."
  [pop {:as run-config,
        select :selection-fn}]
  (point-mutate-individual (select pop) run-config))

(defn shrink-breeder
  "Selects an individual from pop by applying the selection-fn specified in the
  given run-config to it. Performs a shrink mutation and returns seq with the
  single resulting individual in it.

  Shrink mutation will replace a non-terminal (function) node with a new
  terminal node selected from the terminal set. The aim is to shrink the tree."
  [pop {:as run-config,
        select :selection-fn}]
  (shrink-individual (select pop) run-config))

;;;; Breeder internals

(defn parent-arg-type
  "Returns the arg-type that the node at the given 'index of the 'tree
  satisfies. In other words: returns the type that a node at the index must
  satisfy in order to be valid. This type is retrieved from the parent
  node's :gp-arg-types metadata. For index 0, returns given 'root-type.

  Throws out of bounds exception if idx is out of bounds."
  [index root-type treeseq]
  (loop [typestack (list root-type)
         i (int index)
         nodes treeseq]
    (cond
      (== i 0) (first typestack)
      (seq nodes) (recur (concat (gp-arg-type (first nodes))
                                 (rest typestack))
                         (dec i)
                         (rest nodes))
      :else (throw (IndexOutOfBoundsException.
                    "Index out of bounds of treeseq.")))))

;;; Some of the low-level tree handling functions use a somewhat ugly
;;; side-effect counter while traversing a tree to keep track of the node
;;; index. This appears to be quite a bit faster than more elegant methods, and
;;; these functions are called very often. Hence the optimization. Will
;;; hopefully be improved in the future.

(defn tree-replace
  "Returns given 'tree with node at 'idx replaced by 'new-node."
  [idx new-node tree]
  (let [i (atom -1)
        r-fn (fn do-replace [node]
               (swap! i inc)
               (cond
                 (= @i idx) new-node
                 (> @i idx) node        ; means idx has already been replaced
                 (coll? node) (cons (first node)
                                    (doall (map do-replace (next node))))
                 :else node))]
    (r-fn tree)))


;;; Moved some code out of crossover-uniform into a macro, not useful on
;;; its own.
(defmacro find-valid-indices
  "Helper macro for internal use by crossover-uniform.
  Given a tree and some type information, finds all indices of nodes in the
  tree-seq representation of the tree that can be safely exchanged with a node
  whose type information is described in the replacement-type and
  replacement-parent-type arguments.

  The 'treeseq should be a vector, as nth is called on it N times, where N is
  the size of the tree (length of the treeseq)."
  [treeseq replacement-type replacement-parent-type root-type]
  `(filter (fn [idx#] (and (isa? (gp-type (nth ~treeseq idx#))
                                 ~replacement-parent-type)
                           (isa? ~replacement-type
                                 (parent-arg-type idx# ~root-type ~treeseq))))
           (range (count ~treeseq))))

(defn crossover-uniform
  "Performs a subtree crossover operation on the given trees, taking node types
  into account. The 'root-type is the type satisfied by the root nodes of both
  trees. Returns vector of two new trees, or nil if crossover failed."
  [[tree-a tree-b] root-type]
  ;; First select a node from tree A, and gather type information on it
  (let [seq-a (make-tree-seq tree-a)
        idx-a (gp-rand-int (count seq-a))
        pick-a (nth seq-a idx-a)
        type-a (gp-type pick-a)
        parent-type-a (parent-arg-type idx-a root-type seq-a)

        ;; Find indices of nodes that can safely be switched in for pick-a
        seq-b (vec (make-tree-seq tree-b))
        valid-indices (find-valid-indices seq-b type-a parent-type-a root-type)]
    (when (seq valid-indices)
      ;; Select an index and perform the switch
      (let [idx-b (pick-rand valid-indices)
            pick-b (nth seq-b idx-b)]
        [(tree-replace idx-a pick-b tree-a)
         (tree-replace idx-b pick-a tree-b)]))))

;;; Mutate used to return the unmodified tree if generating a subtree
;;; failed. However, this did not fit in well with the higher level breeder
;;; functions, as they should be the ones to decide whether to retry the
;;; mutation or return the original tree (based on eg. :breeding-retries).
(defn mutate
  "Performs a subtree mutation operation on the given tree, selecting a mutation
  point uniformly and generating a new subtree to replace it (from the given
  'func-set and 'term-set, up to 'max-depth). Also requires the
  'root-type (specifying what type the root should satisfy) as the mutation
  point may be the root.

  Returns the new tree. If no valid subtree could be generated, returns
  nil."
  [tree max-depth func-set term-set root-type]
  (let [tree-seq (make-tree-seq tree)
        idx (gp-rand-int (count tree-seq))
        pick-type (parent-arg-type idx root-type tree-seq)
        subtree (try (generate-tree max-depth :grow func-set term-set pick-type)
                     (catch RuntimeException e nil))]
    (if (nil? subtree)
      nil
      (tree-replace idx subtree tree))))

(defn hoist-mutate
  "Performs hoist mutation on the given tree, selecting a random subtree (never
  the current root) and returning it as the new tree."
  [tree root-type]
  (let [tree-seq (make-tree-seq tree)
        subtrees (filter #(isa? (gp-type %) root-type) 
                         (drop 1 tree-seq))]
    (if-let [options (seq subtrees)]
      (pick-rand subtrees)
      nil)))

(defn point-mutate
  "Performs point mutation on the given tree, selecting a random node and
  attempting to replace it with a different one, without modifying the rest of
  the tree."
  [tree function-set terminal-set root-type]
  (let [tree-seq (make-tree-seq tree)
        idx (gp-rand-int (count tree-seq))
        pick (nth tree-seq idx)
        pick-type (parent-arg-type idx root-type tree-seq)
        pick-argtypes (gp-arg-type pick)
        pick-sym (if (coll? pick) (first pick) pick)
        same-types? (fn [sym]
                      (let [sym-type (gp-type sym)
                            sym-argtypes (gp-arg-type sym)]
                        (and (not= pick-sym sym) ; never pick same symbol!
                             (isa? (gp-type sym) pick-type)
                             (== (count sym-argtypes) (count pick-argtypes))
                             (every? true? (map isa? 
                                                sym-argtypes 
                                                pick-argtypes)))))]
    (when-let [options (seq (filter same-types? (if (coll? pick)
                                                  function-set
                                                  terminal-set)))]
      (let [selected (pick-rand options)
            ;; cons the original node's subtree back on
            replacement (if (coll? pick)
                          (cons selected (next pick))
                          selected)] 
        (tree-replace idx replacement tree)))))

(defn shrink-mutate
  "Performs shrink mutation on the given tree, selecting a random node with
  children (non-terminal), and replacing it with a terminal without children."
  [tree terminal-set root-type]
  (let [tree-seq (make-tree-seq tree)
        nonterm-indices (filter number?
                                (map (fn [node idx] (when (and (coll? node)
                                                               (next node))
                                                      idx)) 
                                     tree-seq
                                     (range (count tree-seq))))
        idx (pick-rand nonterm-indices)
        pick (nth tree-seq idx)
        pick-type (parent-arg-type idx root-type tree-seq)]
    (when-let [options (seq (filter #(isa? (gp-type %) pick-type)
                                    terminal-set))]
      (tree-replace idx (pick-rand options) tree))))

;;; TODO: the mutation -individual fns have significant amounts of identical
;;; code that should be extracted

(defn shrink-individual
  "Performs shrink mutation on the given individual's tree. Returns seq with a
  new individual as its only element. The new individual's tree will have one
  non-terminal node replaced with a new terminal node."
  [ind {:as run-config,
        :keys [validate-tree-fn breeding-retries func-template-fn
               terminal-set root-type]}]
  (let [parent (get-fn-body (get-func ind))
        child (get-valid validate-tree-fn breeding-retries
                         #(shrink-mutate parent terminal-set root-type))
        tree (if (nil? child) parent child)
        gen (inc (get-gen ind))]
    [(make-individual (func-template-fn tree) gen)]))

(defn point-mutate-individual
  "Performs point mutation using the given individual's tree. Returns seq with a
  new individual as its only element. The new individual's tree will have a
  single node replaced with a different one without modifying the rest of the
  tree."
  [ind {:as run-config,
        :keys [validate-tree-fn breeding-retries func-template-fn 
               function-set terminal-set root-type]}]
  (let [parent (get-fn-body (get-func ind))
        child (get-valid validate-tree-fn breeding-retries
                         #(point-mutate parent 
                                        function-set terminal-set root-type))
        tree (if (nil? child) parent child)
        gen (inc (get-gen ind))]
    [(make-individual (func-template-fn tree) gen)]))

(defn hoist-individual
  "Performs hoist mutation using the given individual's tree. Returns seq with a
  new individual as its only element. The new individual's tree will be a
  randomly selected subtree of the parent."
  [ind {:as run-config,
        :keys [validate-tree-fn breeding-retries func-template-fn root-type]}]
  (let [parent (get-fn-body (get-func ind))
        child (get-valid validate-tree-fn breeding-retries
                         #(hoist-mutate parent root-type))
        tree (if (nil? child) parent child)
        gen (inc (get-gen ind))]
    [(make-individual (func-template-fn tree) gen)]))

(defn crossover-individuals
  "Performs a crossover operation on the given individuals using given
  'crossover-fn. Returns seq of two new individuals. If crossover-fn returns
  nil after the number of breeding retries configured in the 'run-config, then
  the given individuals are reproduced directly."
  [crossover-fn inds {:as run-config,
                      :keys [validate-tree-fn func-template-fn
                             breeding-retries root-type]}]
  (let [orig-trees (map (comp get-fn-body get-func) inds)
        new-trees (get-valid validate-tree-fn breeding-retries
                             #(crossover-fn orig-trees root-type))
        gen (inc (get-gen (first inds)))]
    (doall (map #(make-individual (func-template-fn %) gen)
                (if (nil? new-trees)
                  orig-trees            ; Crossover failed, just copy.
                  new-trees)))))        ; Successfull crossover.

(defn mutate-individual
  "Performs mutation on given individual's tree. Returns seq with the new
  individual as single element (for easy compatibility with crossover-ind)."
  [ind max-depth {:as run-config,
                  :keys [validate-tree-fn breeding-retries func-template-fn
                         function-set terminal-set root-type]}]
  (let [parent (get-fn-body (get-func ind))
        child (get-valid validate-tree-fn breeding-retries
                         #(mutate parent
                                  max-depth
                                  function-set terminal-set
                                  root-type))
        tree (if (nil? child) parent child)
        gen (inc (get-gen ind))]
    [(make-individual (func-template-fn tree) gen)]))


;;; The reason why we can't just assoc a new gen into the individual is that
;;; other keys may have been added earlier (eg. a :fitness value) that should
;;; not exist on a newly bred individual. We could explicitly unset those but
;;; that would take otherwise unnecessary maintenance if more keys turn up.
(defn reproduce-individual
  "Performs direct reproduction, ie. breeds a new individual by directly copying
  the given individual's tree. Returns seq with new individual as single
  element."
  [ind {:as run-config,
        func-tpl :func-template-fn}]
  (let [tree (get-fn-body (get-func ind))
        gen (inc (get-gen ind))]
    [(make-individual (func-tpl tree) gen)]))


;;;; Breeding infrastructure

(defn- setup-breeders
  "Sets up (lazy-)seq of breeders (maps with a :prob key) in a seq in
  increasing summation, eg. three sources with probabilities [0.1 0.5 0.4] will
  become [0.1 0.6 1.0]. This makes source selection fast and easy. Probabilities
  have to add up to 1.0."
  [breeders]
  (let [pr (fn process [bs p]
             (when-let [s (seq bs)]
               (let [b (first bs)
                     p-new (+ p (:prob b))]
                 (cons (assoc b :prob p-new)
                       (process (rest s) p-new)))))]
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
  [pop-evaluated {:as run-config,
                  :keys [breeders population-size threads rand-fns]}]
  (let [bs (setup-breeders breeders)
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

