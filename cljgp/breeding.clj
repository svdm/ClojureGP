
;;; cljgp.breeding.clj

(ns cljgp.breeding
  "Facilities for generating an initial population and breeding an evaluated
  one, including some standard breeding procedures."
  (:use cljgp.random
	cljgp.util))



;;
;; Tree generation
;;

(defn generate-tree
  "Returns a tree generated from given 'func-set and 'term-set, which have to be
  nth-able. If 'method is :grow, grow method will be used to generate the tree
  up to max-depth in size. Else, 'full' method will be used, resulting in a tree
  with a depth equal to max-depth in all its branches.
  
  For 'max-depth < 0, will return a tree of size 1.

  Throws exception if no legal tree could be generated due to type constraints."
  [func-set term-set max-depth method node-type]
  (if (or (<= max-depth 1)
	  (and (= method :grow)
	       (< (gp-rand) (/ (count term-set)
			       (+ (count term-set) (count func-set))))))
    (if-let [tnode (pick-rand-typed node-type term-set)]
      tnode
      (throw (RuntimeException. 
	      (str "No available terminal of type " node-type))))
    (if-let [fnode (pick-rand-typed node-type func-set)]
      (cons fnode (doall     ; force seq to realize inside try/catch
		   (for [cur-type (:arg-type ^fnode)]
		     (generate-tree func-set term-set 
				    (dec max-depth) method 
				    cur-type))))
      (throw (RuntimeException. 
	      (str "No available function of type " node-type))))))

; FIXME: potential infinite loop for primitive sets where no valid trees can be
;   generated
(defn generate-ramped
  "Returns a tree generated using a version of the ramped half and half
  method. Tree will be generated from 'func-set and 'term-set, up to
  'max-depth (inclusive). The 'grow' method will be used with the probability
  given as 'grow-chance."
  [max-depth grow-chance func-set term-set root-type]
  (if-let [tree (try
		 (generate-tree func-set
				term-set
				(inc (gp-rand-int max-depth))
				(if (< (gp-rand) grow-chance) :grow :full)
				root-type)
		 (catch RuntimeException e
		   false))]
    tree
    (recur max-depth grow-chance func-set term-set root-type)))

(defn get-valid
  "Returns a result of 'gen-fn (which should be a tree-generating function) that
  passes the given 'valid-tree? test. If gen-fn returns a vector of trees, all
  are tested (ie. all produced trees must be valid). If gen-fn returns anything
  that is not a vector, it is assumed to be a single tree and tested
  directly. Will retry gen-fn up to 'tries times, if by then no valid result has
  been generated, returns nil."
  [valid-tree? tries gen-fn]
  (first (filter #(if (vector? %)
		    (every? valid-tree? %)
		    (valid-tree? %))
		 (take tries (repeatedly gen-fn)))))

; FIXME: does not handle situations where valid trees are impossible to generate
(defn- ind-generator-seq
  "Returns a lazy infinite sequence of individuals with generation 0 and
  expression trees generated from the function- and terminal-set, all as
  specified in the given 'run-config. Used internally by
  cljgp.breeding/generate-pop."
  [run-config]
  (let [{:keys [function-set terminal-set
		pop-generation-fn
		validate-tree-fn
		func-template-fn
		root-type]} run-config
	generate (fn [] (get-valid validate-tree-fn Integer/MAX_VALUE 
				   #(pop-generation-fn function-set 
						       terminal-set
						       root-type)))]

    (repeatedly #(make-individual (func-template-fn (generate)) 0))))

; Note that it is intentional that (ind-generator-seq ..) is called inside each
; future, even though one might think it's just a producer function that is
; identical between futures. The reason is that the closure appears to inherit
; the bindings as they are at the point where it is created, as opposed to that
; where it is called. Dynamic bindings are nefarious things.
(defn generate-pop
  "Returns a population of individuals generated from scratch out of the nodes
  in the function set and terminal set, using the tree producer function also
  specified in the 'run-config (typically ramped half and half). The work is
  divided over worker threads, each with their own RNG."
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

; The low-level tree handling functions use a somewhat ugly side-effect counter
; while traversing a tree to keep track of the node index. This appears to be
; quite a bit faster than more elegant methods, and these functions are called
; very often. Hence the optimization. Will hopefully be improved in the future.

(defn parent-arg-type
  "Returns the arg-type that the node at the given 'index of the 'tree
  satisfies. In other words: returns the type that a node at the index must
  satisfy in order to be valid. This type is retrieved from the parent
  node's :arg-type metadata. For index 0, returns given 'root-type."
  [index tree root-type]
  (let [i (atom -1)
	pfn (fn ptype [node type]
	      (if (>= (swap! i inc) index)
		type
		(when (coll? node) 
		  (first
		   (remove nil?
			   (map ptype
				(rest node)
				(:arg-type ^(first node))))))))]
    (pfn tree root-type)))

(defn tree-replace
  "Returns given 'tree with node at 'idx replaced by 'new-node."
  [idx new-node tree]
  (let [i (atom -1)
	r-fn (fn do-replace [node]
	       (swap! i inc)
	       (cond
		 (= @i idx) new-node
		 (> @i idx) node	; means idx has already been replaced
		 (coll? node) (cons (first node)
				    (doall (map do-replace (next node))))
		 :else node))]
    (r-fn tree)))

;TODO/FIXME: crossover point selection is uniform instead of the traditional
;  90/10 split between nodes and leaves respectively

; not the prettiest, could use cleanup
(defn crossover-uniform-typed
  "Performs a subtree crossover operation on the given trees, taking node types
  into account. The 'root-type is the type satisfied by the root nodes of both
  trees. Returns vector of two new trees, or nil if crossover failed."
  [tree-a tree-b root-type]
  (let [seq-a (make-tree-seq tree-a)
	idx-a (gp-rand-int (count seq-a))
	pick-a (nth seq-a idx-a)
	type-a (parent-arg-type idx-a tree-a root-type)
	
	seq-b (vec (make-tree-seq tree-b)) ; vec for faster nth
	valid-indices (filter #(isa? (gp-type (nth seq-b %)) type-a) 
			      (range (count seq-b)))]
    (if (seq valid-indices)
      (let [idx-b (pick-rand valid-indices)
	    pick-b (nth seq-b idx-b)]
	[(tree-replace idx-a pick-b tree-a)
	 (tree-replace idx-b pick-a tree-b)])

      nil)))

(defn mutate
  "Performs a mutation operation on the given tree, selecting a mutation point
  uniformly and generating a new subtree to replace it (from the given 'func-set
  and 'term-set, up to 'max-depth). Also requires the 'root-type as the mutation
  point may be the root.

  Returns the new tree. If no valid subtree could be generated, returns
  unmodified tree."
  [func-set term-set max-depth root-type tree]
  (let [tree-seq (make-tree-seq tree)
	idx (gp-rand-int (count tree-seq))
	pick-type (parent-arg-type idx tree root-type)
	subtree (try (generate-tree func-set term-set max-depth :grow pick-type)
		     (catch RuntimeException e nil))]
    (if (nil? subtree)
      tree
      (tree-replace idx subtree tree))))


(defn crossover-individuals
  "Performs a crossover operation on the given individuals using given
  'crossover-fn. Returns vector of two new individuals. If crossover-fn returns
  nil after the number of breeding retries configured in the 'run-config, then
  the given individuals are reproduced directly."
  [crossover-fn inds run-config]
  (let [{:keys [validate-tree-fn func-template-fn
		breeding-retries root-type]} run-config
	[orig-a orig-b] (map (comp get-fn-body get-func) inds)
	[tree-a tree-b] (get-valid validate-tree-fn breeding-retries
				   #(crossover-fn orig-a orig-b root-type))
	gen (inc (get-gen (first inds)))]
    (if (not (nil? tree-a))
      [(make-individual (func-template-fn tree-a) gen) ; crossover succeeded
       (make-individual (func-template-fn tree-b) gen)]

      [(make-individual (func-template-fn orig-a) gen) ; failed, plain copy
       (make-individual (func-template-fn orig-b) gen)])))

(defn mutate-individual
  "Performs mutation on given individual's tree. Returns seq with the new
  individual as single element (for easy compatibility with crossover-ind)."
  [ind run-config max-depth]
  (let [{:keys [validate-tree-fn breeding-retries
		func-template-fn
		function-set terminal-set root-type]} run-config
	orig (get-fn-body (get-func ind))
	tree (get-valid validate-tree-fn breeding-retries
			#(mutate function-set terminal-set 
				 max-depth root-type
				 orig))
	gen (inc (get-gen ind))]
    (if (not (nil? tree))
      [(make-individual (func-template-fn tree) gen)]
      [(make-individual (func-template-fn orig) gen)])))


; The reason why we can't just assoc a new gen into the individual is that
; other keys may have been added earlier (eg. a :fitness value) that should
; not exist on a newly bred individual. We could explicitly unset those but
; that would take otherwise unnecessary maintenance if more keys turn up.
(defn reproduce-individual
  "Performs direct reproduction, ie. breeds a new individual by directly copying
  the given individual's tree. Returns seq with new individual as single
  element."
  [ind run-config]
  (let [func-tpl (:func-template-fn run-config)
	tree (get-fn-body (get-func ind))
	gen (inc (get-gen ind))]
    [(make-individual (func-tpl tree) gen)]))


(defn crossover-breeder
  "Selects two individuals from pop by applying the selection-fn specified in
   run-config to it twice, performs crossover and returns seq of two resulting
   new trees."
  [pop run-config]
  (let [select (:selection-fn run-config)]
    (crossover-individuals crossover-uniform-typed
			   [(select pop) (select pop)]
			   run-config)))

(defn mutation-breeder
  "Selects an individual from pop by applying selection-fn specified in the
  run-config to it, performs mutation and returns seq with the single resulting
  tree in it. Mutation will pull nodes from the function and terminal sets
  specified in run-config. Generated (sub)tree will be at most max-depth deep."
  ([max-depth pop run-config]
     (let [select (:selection-fn run-config)]
       (mutate-individual (select pop) run-config max-depth)))
  ([pop run-config]
     (mutation-breeder 17 pop run-config)))

(defn reproduction-breeder
  "Selects an individual from pop by applying selection-fn specified in the
  run-config to it, returns seq with a new individual whose tree is identical to
  the selected individual."
  [pop run-config]
  (let [select (:selection-fn run-config)]
    (reproduce-individual (select pop) run-config)))

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

