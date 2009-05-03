
;;; cljgp.random.clj

(ns cljgp.random
  "Default random functions, and facilities for overriding them with better
  RNGs."
  (:import java.util.Random))


(defn gp-rand
  "Identical to clojure.core/rand, but can be overridden to use a different PRNG
  without affecting clojure's rand."
  [] (. Math random))

(defn gp-rand-int
  "Identical to clojure.core/rand-int, but using gp-rand internally."
  [n]
  (int (* n (gp-rand))))

(defn pick-rand
  "Returns a random item from the given collection."
  [coll]
  (nth (seq coll) (gp-rand-int (count coll)) nil))

(defmacro rand-fn
  "Macro that takes a method name and an expression returning a new instance of
  an object, and returns a zero-arg function closing over the object and calling
  the method on it. The method should return a value between 0.0 and 1.0,
  similar to (Math/random).

  Intended usage is for creating gp-rand replacement fns with different PRNGs or
  with user-specified seeds, which can then be used in an experiment's
  run-config to replace the default time-initialised java.util.Random.

  Example: (rand-fn nextDouble (Random. 123))
  Becomes: (let [rng (Random. 123)] (fn [] (. rng nextDouble))

  Also useful, for easily creating several seeded PRNG-fns for a threaded
  experiment: (map #(rand-fn nextDouble (Random. %)) [1234 8472 9000])."
  ([method rng] 
     `(let [r# ~rng]
	(fn [] (. r# ~method))))
  ([rng]
     `(rand-fn nextDouble rng)))

(defn make-default-rand
  "Default producer function of rand functions, for use in :rand-fn-maker in
  run-config. Returns a rand-like function with the default java.util.Random
  PRNG, initialised with the given seed. See 'rand-fn."
  [seed]
  (rand-fn nextDouble (new Random seed)))
