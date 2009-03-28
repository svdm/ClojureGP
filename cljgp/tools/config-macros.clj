
;;; cljgp.tools.config-macros.clj

(ns cljgp.tools.config-macros.clj)

; The macros/functions below are meant as helpers for defining run parameters
; such as the function and terminal sets. Since all these parameters are maps
; with fairly self-explanatory keys, I would recommend defining those maps
; 'manually'/explicitly rather than using the positional parameter method of
; these macros. Explicit maps not hidden behind the macros will likely be
; clearer. These macros are (for now) still offered here for users who prefer
; them.

(defmacro func-set-entry
  "Returns a map of {:sym 'func-sym, :arity 'arity, :as-arg 'as-arg}. If as-arg
  is true, func-sym will not be resolved. Given arity defines the number of
  leaves/branches this node can have, ie. how many arguments it takes."
  [func-sym arity as-arg] 
  {:sym (if as-arg
	  `(quote ~func-sym)
	  (resolve func-sym))
   :arity arity
   :as-arg as-arg})

(defmacro define-function-set
  "Returns a vector of function set entries as produced by
  'func-set-entry. Entries should be collections of [function-name, arity,
  additional-properties], where function-name is a symbol naming a function of
  given arity. If among additional-properties is :as-arg, the function-name will
  not be resolved, but will have to be passed in as argument during evaluation.
  See also 'func-set-entry."
  [& entries] 
  `[~@(map (fn [entry]
	     `(func-set-entry ~(first entry)
			      ~(second entry)
			      ~(some #(= % :as-arg) entry)))
	   entries)])

(defmacro term-set-entry
  "Returns a map of {:sym 'term-sym, :as-arg 'as-arg}. If as-arg is false, will
  resolve term-sym to be fully namespace-qualified, else is left quoted and
  unresolved. In that case, the terminal is assumed to be passed as argument to
  evolved functions when they are called during evaluation."
  [term-sym as-arg]
  {:sym (if (or as-arg
		(not (symbol? term-sym))) ; for plugging in constants
	  `(quote ~term-sym)
	  (resolve term-sym))
   :as-arg as-arg})

(defmacro define-terminal-set
  "Given entries, returns a vector of terminal set entries as produced by
  'term-set-entry. Entries can simply be a single symbol, but also a collection
  of a symbol followed by additional properties, such as :as-arg."
  [& entries]
  `[~@(map (fn [entry]
	     (if (not (coll? entry))
	       `(term-set-entry ~entry nil)
	       `(term-set-entry ~(first entry) ~(some #(= % :as-arg) entry))))
	   entries)])

(defmacro breeder-entry
  "Returns a map {:prob 'prob, :func 'breeder-func} that can be used as breeder
  by the breeding functions.."
  [prob breeder-fn]
  {:prob prob
   :breeder-fn breeder-fn})

(defmacro define-breeders
  "Given collections of [probability, breederfn], defines a sequence of breeders
  as can be used by the breeding functions. 

  This primarily a shortcut for defining a sequence of {:prob, :func} maps, so
  you may prefer to do that manually for clarity. See also 'breeder-entry."
  [& pairs]
  (let [entries# (if (number? (first pairs))
		   (do (assert (even? (count pairs)))
		       (partition 2 pairs))
		   pairs)]
    `[~@(map #(breeder-entry (first %) (second %)) entries#)]))

