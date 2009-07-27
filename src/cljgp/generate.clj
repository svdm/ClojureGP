;; Copyright (c) Stefan A. van der Meer. All rights reserved.
;; The use and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which
;; can be found in the file epl-v10.html at the root of this distribution. By
;; using this software in any fashion, you are agreeing to be bound by the
;; terms of this license. You must not remove this notice, or any other, from
;; this software.

(ns cljgp.generate
  "Facilities for generating (populations of) trees.

  generate-ramped is currently the only provided population generation function
  ready for use in experiment configurations."
  (:use cljgp.random
	cljgp.util))

(declare generate-tree)

(defn generate-ramped
  "Returns a tree generated using a version of the ramped half and half
  method. Tree will be generated from the function and terminal sets, up to
  :max-depth (inclusive). The 'grow' method will be used with the probability
  given as :grow-chance.

  If a tree is constructed that ends up not being validly typed, returns
  nil. This can happen quite often in experiments with many different types. See
  cljgp.breeding/get-valid for a way to deal with this."
  [{:keys [max-depth grow-chance] :or {max-depth 7, grow-chance 0.5}} 
   {:as run-config,
    :keys [function-set terminal-set root-type]}]
  (try
   (generate-tree (inc (gp-rand-int max-depth))
		  (if (< (gp-rand) grow-chance) :grow :full)
		  function-set
		  terminal-set
		  root-type)
   (catch RuntimeException e
     nil)))

(defn generate-tree
  "Returns a tree generated from given 'func-set and 'term-set, which have to be
  nth-able. If 'method is :grow, grow method will be used to generate the tree
  up to max-depth in size. Else, 'full' method will be used, resulting in a tree
  with a depth equal to max-depth in all its branches.
  
  For 'max-depth < 0, will return a tree of size 1.

  Throws exception if no legal tree could be generated due to type constraints."
  [max-depth method func-set term-set node-type]
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
		   (for [cur-type (:gp-arg-types ^fnode)]
		     (generate-tree (dec max-depth) method 
				    func-set term-set 
				    cur-type))))
      (throw (RuntimeException. 
	      (str "No available function of type " node-type))))))

(defn get-valid
  "Returns a result of 'gen-fn (which should be a tree-generating function) that
  passes the given 'valid-tree? test. If gen-fn returns a vector of trees, all
  are tested (ie. all produced trees must be valid). If gen-fn returns anything
  that is not a vector, it is assumed to be a single tree and tested
  directly. Will retry gen-fn up to 'tries times, if by then no valid result has
  been generated, returns nil."
  [valid-tree? tries gen-fn]
  (first (filter #(if (vector? %)
		    (and (not-empty %) (every? valid-tree? %))
		    (and (not (nil? %)) (valid-tree? %)))
		 (take (inc tries) (repeatedly gen-fn)))))

(defn- individual-generator-seq
  "Returns a lazy infinite sequence of individuals with generation 0 and
  expression trees generated from the function- and terminal-set, all as
  specified in the given 'run-config. Used internally by
  cljgp.breeding/generate-pop."
  [{:as run-config,
    :keys [pop-generation-fn, validate-tree-fn, func-template-fn]}]
  (let [retries 1024			; should perhaps be configurable
	generate (fn [] 
		   (if-let [ind (get-valid validate-tree-fn retries
					   #(pop-generation-fn run-config))]
		     ind
		     (throw (RuntimeException. 
			     (str "Failed to generate a valid tree after "
				  retries " attempts. Most likely there is "
				  "an issue that makes valid trees "
				  "impossible to generate.")))))]
    (repeatedly 
     #(make-individual (func-template-fn (generate)) 0))))

;;; Note that it is intentional that (individual-generator-seq ..) is called
;;; inside each future, even though one might think it's just a producer
;;; function that is identical between futures. The reason is that the closure
;;; appears to inherit the bindings as they are at the point where it is
;;; created, as opposed to that where it is called. Dynamic bindings are
;;; nefarious things.
(defn generate-pop
  "Returns a population of individuals generated from scratch out of the nodes
  in the function set and terminal set, using the tree producer function also
  specified in the 'run-config (typically ramped half and half). The work is
  divided over worker threads, each with their own RNG instance."
  [{:keys [population-size threads rand-fns] :as run-config}]
  (let [per-future (divide-up population-size threads)]
    (mapcat deref
	    (doall 
	     (map #(future
		     (binding [cljgp.random/gp-rand %2]
		       (doall (take %1 (individual-generator-seq run-config)))))
		  per-future
		  rand-fns)))))
