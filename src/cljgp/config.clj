;; Copyright (c) Stefan A. van der Meer. All rights reserved.
;; The use and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which
;; can be found in the file epl-v10.html at the root of this distribution. By
;; using this software in any fashion, you are agreeing to be bound by the
;; terms of this license. You must not remove this notice, or any other, from
;; this software.

;;; cljgp/config.clj

(ns cljgp.config
  "Facilities for creating GP experiment (\"run\") configurations and validating
  them."
  (:use [cljgp.breeding :only (crossover-breeder
			       mutation-breeder
			       reproduction-breeder
			       generate-ramped)]
	[cljgp.selection :only (tournament-select)]
	cljgp.random
	cljgp.util

	[clojure.contrib.def :only (defalias)]))

;
; Helper fns for config creation
;

(defn primitive
  "Given a symbol and a map of its properties, returns a GP primitive for use in
  the function or terminal sets. This is simply the symbol with the properties
  as its metadata.

  Identical to clojure.core/with-meta with some extra checks."
  [sym properties]
  (assert (or (map? properties) (nil? properties)))
  (assert (symbol? sym))
  (with-meta sym properties))

(defalias prim primitive)

(defn make-simple-end
  "Returns a simple end condition predicate that stops the evolution when the
  given max number of generations is reached, or the fitness of any individual
  in a generation is lower than (+ 0 fit-tolerance), where 'fit-tolerance is
  0.0001 by default."
  ([max-generations fit-tolerance]
     (fn [pop]
       (let [ind (first pop)]
	 (or (>= (:gen ind) max-generations)
	     (some #(>= fit-tolerance (get-fitness %)) pop)))))
  ([max-generations] (make-simple-end max-generations 0.0001)))

(defn seeds-from-time
  "Returns a lazy infinite sequence of calls
  to (long (rand (System/currentTimeMillis))), the results of which can be used
  as PRNG seeds. Takes optional argument that when true enables reporting of the
  seeds used to stdout. This is meant as a very basic way of logging seeds in
  case the results need to be reproduced."
  ([report]
      (if report
	(repeatedly #(let [t (long (rand (System/currentTimeMillis)))]
		       (println "Used seed:" t)
		       t))
	(repeatedly #(long (rand (System/currentTimeMillis))))))
  ([] (seeds-from-time false)))

(defn make-func-template
  "In short, returns a fn as is expected by the :func-template-fn key in the
  run-config. Specifically, returns a function that inserts an evolved
  expression tree in a quoted fn form like:
    (fn my-name [] evolved-tree)

  With the argument vector as 'arg-list is given (else []). And with the name as
  'func-name if given (which may be useful for evolving recursive functions, as
  this func-name could be added to the function/terminal set).

  This fn form is stored in evolved individuals under the :func key, and eval'd
  during the evaluation phase."
  ([func-name arg-list]
     (assert (or (symbol? func-name) (nil? func-name)))
     (assert (vector? arg-list))
     (if (nil? func-name)		; can't just put nil in fn def
       (fn standard-template [tree] (list `fn arg-list tree))
       (fn standard-template [tree] (list `fn func-name arg-list tree))))
  ([arg-list]
     (make-func-template nil arg-list))
  ([]
     (make-func-template nil [])))

;
; Configuration validation
;

; Values used as defaults in run config when no user-defined value is present.
(def config-defaults
     {:func-template-fn (make-func-template)
      :breeders [{:prob 0.8  :breeder-fn crossover-breeder}
		 {:prob 0.1  :breeder-fn mutation-breeder}
		 {:prob 0.1  :breeder-fn reproduction-breeder}]
      :breeding-retries 1
      :selection-fn (partial tournament-select {:size 7})
      :end-condition-fn (make-simple-end 100)
      :pop-generation-fn (partial generate-ramped {:max-depth 7 
						   :grow-chance 0.5})
      :rand-fn-maker make-default-rand
      :rand-seeds (seeds-from-time)
      :validate-tree-fn identity
      :root-type nil
      :threads 1
      })

(defn valid-func-entry?
  "Returns true if the given function set entry is valid."
  [entry]
  (let [m (meta entry)]
    (and (symbol? entry)
	 (coll? (:gp-arg-types m)))))

(defn valid-term-entry?
  "Returns true if the given terminal set entry is valid."
  [entry]
  (or (symbol? entry)
      (number? entry)))

(defn valid-breeder-entry?
  "Returns true if the given map is a valid breeder specification."
  [entry]
  (and (map? entry)
       (float? (:prob entry))
       (fn? (:breeder-fn entry))))

(defn valid-breeder-probs?
  "Returns true if the :prob values of all breeders add up to 1.0 (allowing for
  floating point errors), as they should."
  [breeders]
  (< 0.99 (reduce + (map :prob breeders))))

(defn strict-every?
  "Like 'every?, but returns false on empty coll."
  [pred coll]
  (and (not-empty coll)
       (every? pred coll)))

; Map of required keys, with as their values test predicates
(def config-spec
     {:function-set #(and (coll? %) (strict-every? valid-func-entry? %))
      :terminal-set #(and (coll? %) (strict-every? valid-term-entry? %))
      :evaluation-fn fn?
      :end-condition-fn fn?
      :breeders #(and (coll? %) 
		      (strict-every? valid-breeder-entry? %)
		      (valid-breeder-probs? %))
      :breeding-retries number?
      :selection-fn fn?
      :population-size number?
      :pop-generation-fn fn?
      :threads integer?
      :rand-fn-maker fn?
      :rand-seeds coll?
      :validate-tree-fn fn?
      :func-template-fn fn?
      :root-type #(or (class? %) (keyword? %) (nil? %))
      })

(defn check-key
  "Returns a map with an :entry and an :type key.

  If 'k exists in config and passes the given test, :entry is its map entry
  and :type is :pass.

  If 'k does not exist in 'config, :entry is (find config-defaults k) and :type
  is :default.

  If 'k does not exist in 'config and no default exists, :entry is nil and :type
  is :no-default.

  If the value in (k config) exists but fails the given test, :entry is nil
  and :type is :fail."
  [k val-test config]
  (let [entry (find config k)]
    (cond
      (not (contains? config k)) 
        (if (contains? config-defaults k) 
	  {:entry (find config-defaults k) :type :default}
	  {:entry nil :type :no-default})
      (not (val-test (val entry))) 
        {:entry nil :type :fail}
      :else 
        {:entry entry :type :pass})))

(defn- add-check-result
  "Helper for check-config. Updates a given config by adding the 'rval to the
  'rkey entry list in its :check-results map, if rkey is not :pass."
  [config rkey rval]
  (if (= rkey :pass)
    config
    (update-in config [:check-results rkey] conj rval)))

(defn check-config
  "Verifies that all required keys are present in the given configuration,
  showing a warning if not, and reverting to a default if possible. If a key is
  missing and there is no default, or a value fails the test associated with a
  key, an exception is thrown. Values for which no test is defined will be
  ignored."
  [run-config]
  (loop [conf run-config
	 todo config-spec]
    (if (not (seq todo))
      conf
      (let [[k test] (first todo)
	    {checked-entry :entry etype :type} (check-key k test run-config)
	    conf-upd (add-check-result conf etype k)]
	(if (seq checked-entry)
	  (recur (conj conf-upd checked-entry)
		 (next todo))
	  (recur conf-upd
		 (next todo)))))))

(defn report-check-problems
  "Prints information on the :check-results of a given config. If any of the
  issues are fatal, throws an exception."
  [run-config]
  (when-let [results (:check-results run-config)]
    (println "Run configuration preprocessing report:")
    (when-let [defaults (:default results)]
      (println "  NOTE: The following keys were missing, using default values:")
      (println "    " defaults))
    (when-let [missing (:no-default results)]
      (println "  FATAL: The following keys were missing, no defaults exist:")
      (println "    " missing))
    (when-let [fails (:fail results)]
      (println "  FATAL: Values of the following keys were invalid:")
      (println "    " fails))
    (when-let [constraints (:constraint results)]
      (doseq [c constraints]
	(println "  FATAL: Constraint between keys violated:")
	(println "    " c)))
    (newline)
    (when (seq (filter (partial contains? results) 
		       [:fail :no-default :constraint]))
      (throw (Exception. (str "Fatal problems exist in run configuration, "
			      "run cannot proceed."))))))

; TODO: restructure to test multiple constraints when more are needed
(defn assert-constraints
  "Checks constraints between keys (ie. 'global' constraints) and adds error
  to :check-results map under :constraint key."
  [run-config]
  (let [{:keys [threads rand-seeds]} run-config]
    (cond 
      (> threads (count (take threads rand-seeds)))
        (add-check-result run-config :constraint 
			  "Insufficient seeds for the number of threads.")
      :else run-config)))


(defn preproc-config
  "Performs some convenient preprocessing that generates values for more complex
  config values that non-experts may not want to specify directly. If these
  values are already present they will not be modified here."
  [run-config]
  (let [config-get (fn ([key] (get run-config key (get config-defaults key)))
		       ([key not-found] (get run-config key not-found)))
	rand-fns {:rand-fns 
		  (config-get :rand-fns
		       ;; create and realize seq of rand-fns/seeds here
		       (take (config-get :threads)
			     (map (config-get :rand-fn-maker) 
				  (config-get :rand-seeds))))}
	tpl-fn   {:func-template-fn 
		  (config-get :func-template-fn
		       (make-func-template (config-get :arg-list [])))}]
    (merge run-config
	   rand-fns
	   tpl-fn)))

(defn prepare-config
  "First calls check-config on given run-config, then performs both
  preprocessing and checks involving multiple keys."
  [run-config]
  (let [preprocessed (preproc-config run-config)
	checked (assert-constraints (check-config preprocessed))]
    (report-check-problems checked)
    (merge preprocessed
	   checked))) ; keys changed in check-config will override run-config

