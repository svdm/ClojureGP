
;;; cljgp.config.clj

(ns cljgp.config
  (:use [cljgp.breeding :only (crossover-breeder
			       mutation-breeder
			       reproduction-breeder
			       generate-ramped)]
	[cljgp.selection :only (tournament-select)]))

(defn make-simple-end
  "Returns a simple end condition predicate that stops the evolution when the
  given max number of generations is reached, or the fitness of any individual
  in a generation is lower than (+ 0 fit-tolerance), where fit-tolerance is
  0.0001 by default."
  ([max-generations fit-tolerance]
     (fn [pop]
       (let [ind (first pop)]
	 (or (>= (:gen ind) max-generations)
	     (some #(>= fit-tolerance (:fitness %)) pop)))))
  ([max-generations] (make-simple-end max-generations 0.0001)))

; Values used as defaults in run config when no user-defined value is present.
(def config-defaults
     {:arg-list []
      :breeders [{:prob 0.8  :breeder-fn crossover-breeder}
		 {:prob 0.1  :breeder-fn mutation-breeder}
		 {:prob 0.1  :breeder-fn reproduction-breeder}]
      :selection-fn (partial tournament-select 7)
      :end-condition (make-simple-end 100)
      :pop-generation-fn (partial generate-ramped 7 0.5)})

(defn valid-func-entry?
  "Returns true if the given map is a valid function set entry."
  [entry]
  (and (map? entry)
       (symbol? (:sym entry))
       (number? (:arity entry))))

(defn valid-term-entry?
  "Returns true if the given map is a valid terminal set entry."
  [entry]
  (and (map? entry)
       (or (symbol? (:sym entry))
	   (number? (:sym entry)))))

(defn valid-breeder-entry?
  [entry]
  (and (map? entry)
       (number? (:prob entry))
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
      :arg-list #(every? symbol? %) ; empty = allowed
      :evaluation-fn fn?
      :end-condition fn?
      :breeders #(strict-every? valid-breeder-entry? %)
      :selection-fn fn?
      :population-size number?
      :pop-generation-fn fn?})

(defn check-key
  "If k does not exist in config, returns (k config-defaults) if any, else
  returns nil. If the value in (k config) fails the given test, returns
  nil. Else, returns (k config)."
  [k test config]
  (let [entry (find config k)]
    (cond
      (nil? entry) 
        (if (k config-defaults) 
	  (do (println "Warning: key" k "missing from configuration,"
		       "reverted to default.")
	      (find config-defaults k))
	  nil)
      (not (test (val entry))) nil
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