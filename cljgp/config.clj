
;;; cljgp.config.clj

(ns cljgp.config
  "Facilities for creating GP experiment (\"run\") configurations and validating
  them."
  (:use [cljgp.breeding :only (crossover-breeder
			       mutation-breeder
			       reproduction-breeder
			       generate-ramped)]
	[cljgp.selection :only (tournament-select)]
	cljgp.random
	cljgp.util))

;
; Helper fns for config creation
;

(defn prim
  "Given a symbol and a map of its properties, returns a GP primitive for use in
  the function or terminal sets. This is simply the symbol with the properties
  as its metadata."
  [sym properties]
  (assert (or (map? properties) (nil? properties)))
  (assert (symbol? sym))
  (with-meta sym properties))

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
  "Returns a lazy infinite sequence of calls to System/currentTimeMillis, which
  can be used as PRNG seeds. Takes optional argument that when true enables
  reporting of the seeds used to stdout. This is meant as a very basic way of
  logging seeds in case the results need to be reproduced."
  ([report]
      (if report
	(repeatedly #(let [t (System/currentTimeMillis)]
		       (println "Used seed:" t)
		       t))
	(repeatedly #(System/currentTimeMillis))))
  ([] (seeds-from-time false)))

(defn standard-func-template
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
     (standard-func-template nil arg-list))
  ([]
     (standard-func-template nil [])))

;
; Configuration validation
;

; Values used as defaults in run config when no user-defined value is present.
(def config-defaults
     {:func-template-fn (standard-func-template)
      :breeders [{:prob 0.8  :breeder-fn crossover-breeder}
		 {:prob 0.1  :breeder-fn mutation-breeder}
		 {:prob 0.1  :breeder-fn reproduction-breeder}]
      :breeding-retries 1
      :selection-fn (partial tournament-select 7)
      :end-condition (make-simple-end 100)
      :pop-generation-fn (partial generate-ramped 7 0.5)
      :rand-fn-maker make-default-rand
      :validate-tree-fn identity
      })

(defn valid-func-entry?
  "Returns true if the given function set entry is valid."
  [entry]
  (let [m (meta entry)]
    (and (symbol? entry)
	 (seq (:arg-type m)))))

(defn valid-term-entry?
  "Returns true if the given terminal set entry is valid."
  [entry]
  (symbol? entry))

(defn valid-breeder-entry?
  "Returns true if the given map is a valid breeder specification."
  [entry]
  (and (map? entry)
       (float? (:prob entry))
       (fn? (:breeder-fn entry))))

(defn strict-every?
  "Like 'every?, but returns false on empty coll."
  [pred coll]
  (and (not-empty coll)
       (every? pred coll)))

; Map of required keys, with as their values test predicates
(def config-spec
     {:function-set #(strict-every? valid-func-entry? %)
      :terminal-set #(strict-every? valid-term-entry? %)
      :evaluation-fn fn?
      :end-condition fn?
      :breeders #(strict-every? valid-breeder-entry? %)
      :breeding-retries number?
      :selection-fn fn?
      :population-size number?
      :pop-generation-fn fn?
      :threads integer?
      :rand-fn-maker fn?
      :validate-tree-fn fn?
      :func-template-fn fn?
      })

(defn check-key
  "If 'k does not exist in 'config, returns (k config-defaults) if any, else
  returns nil. If the value in (k config) fails the given test, returns
  nil. Else, returns (k config)."
  [k val-test config]
  (let [entry (find config k)]
    (cond
      (nil? entry) 
        (if (k config-defaults) 
	  (do (println "Note: key" k "missing from configuration,"
		       "using default.")
	      (find config-defaults k))
	  nil)
      (not (val-test (val entry))) nil
      :else entry)))

(defmacro throw-config
  [k config]
  `(throw (Exception. (str "Invalid or missing run configuration entry. "
			   "Key: " ~k " ; Value in config: " (~k ~config)))))

(defn check-config
  "Verifies that all required keys are present in the given configuration,
  showing a warning if not, and reverting to a default if possible. If a key is
  missing and there is no default, or a value fails the test associated with a
  key, an exception is thrown. Values for which no test is defined will be
  ignored."
  [run-config]
  (loop [final run-config
	 todo config-spec]
    (if (not (seq todo))
      final
      (let [[k test] (first todo)
	    checked-entry (check-key k test run-config)]
	(if (seq checked-entry)
	  (recur (conj final checked-entry)
		 (next todo))
	  (throw-config k run-config))))))

(defmacro assert-msg
  "Like assert, but with a more useful message."
  [test msg]
  `(when-not ~test
     (throw (Exception. ~msg))))

(defn assert-constraints
  "Checks constraints between keys (ie. 'global' constraints) and throws
  exception if a test fails. Returns 'run-config unmodified."
  [run-config]
  (let [{:keys [threads rand-seeds]} run-config]
    (assert-msg (<= threads (count (take threads rand-seeds)))
		"Insufficient seeds for the number of threads."))
  run-config)

(defn preproc-config
  "Performs some convenient preprocessing that generates values for more complex
  config values that non-experts may not want to specify directly. If these
  values are already present they will not be modified here."
  [run-config]
  (let [rand-fns {:rand-fns 
		  (get run-config :rand-fns
		       (map (get run-config :rand-fn-maker
				 (:rand-fn-maker config-defaults)) 
			    (get run-config :rand-seeds
				 (:rand-seeds config-defaults))))}
	tpl-fn   {:func-template-fn 
		  (get run-config :func-template-fn
		       (standard-func-template (get run-config :arg-list [])))}]
    (merge run-config
	   rand-fns
	   tpl-fn)))

(defn prepare-config
  "First calls check-config on given run-config, then performs both
  preprocessing and checks involving multiple keys."
  [run-config]
  (let [preprocessed (preproc-config run-config)
	checked (assert-constraints (check-config preprocessed))]
    (merge preprocessed
	   checked))) ; keys changed in check-config will override run-config

