;; Copyright (c) Stefan A. van der Meer. All rights reserved.
;; The use and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which
;; can be found in the file epl-v10.html at the root of this distribution. By
;; using this software in any fashion, you are agreeing to be bound by the
;; terms of this license. You must not remove this notice, or any other, from
;; this software.

(ns cljgp.tools.unc-math-random
  (:import [org.uncommons.maths.random MersenneTwisterRNG])
  (:use [cljgp.random :only (rand-fn)]))

; Example of how one would wrap a third party PRNG for use in cljgp experiments.

(defn long-in-bytes
  "Mangles a long into an array of 16 bytes. Probably commits several RNG-crimes
  in the process"
  [l]
  (let [bs (.toByteArray (BigInteger/valueOf l))]
    (into-array Byte/TYPE (take 16 (cycle bs)))))

(defn long-to-random-bytes
  "Turns a long into an array of 16 bytes by using it to seed java.util.Random
  and generate 16 bytes with it.

  Seems (intuitively) more random than long-in-bytes while being just as
  reproducable, though this does depend on the JVM's Random."
  [l]
  (let [rng (java.util.Random. l)
        bs (make-array Byte/TYPE 16)]
    (.nextBytes rng bs)
    bs))

(defn make-unc-math-rand
  "Returns a rand fn that uses a MersenneTwisterRNG initialised with the given
  seed. See cljgp.random.

  RNG wants 16-byte seeds (128-bit). Seed will be converted to bytes, then
  cycled to reach 16 of them."
  [seed]
  (if (number? seed)
    (make-unc-math-rand (long-to-random-bytes seed))
    (rand-fn nextDouble (new MersenneTwisterRNG #^bytes seed))))

; Where all rand-fn does is create a closure with the seeded PRNG: 
;(let [rng (new MersenneTwisterRNG seed)] 
;  (fn [] (. rng nextDouble)))
