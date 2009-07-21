;; Copyright (c) Stefan A. van der Meer. All rights reserved.
;; The use and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which
;; can be found in the file epl-v10.html at the root of this distribution. By
;; using this software in any fashion, you are agreeing to be bound by the
;; terms of this license. You must not remove this notice, or any other, from
;; this software.


(ns reg-exp
  "Example 02: evolving a solution to a simple regression problem."
  (:use [cljgp.core :only (generate-run)]
	cljgp.breeding
	cljgp.tools.logging
	cljgp.config
	cljgp.random
	cljgp.util))

(defn test-reg-once
  "Evaluate the given function on a single result, returning the abs error."
  [func]
  (let [x (gp-rand)
	y (gp-rand)
	result (func x y)
	target (+ 
		(+ (* (* x x) (* y y)) (* (* x x) (* x x)))
		(* x y))] ; z = x^2 * y^2 + x^4 + xy
    (Math/abs (float (- target result)))))

(defn evaluate-reg
  "Evaluate the given function on multiple results and return a fitness."
  [func]
  (reduce + (take 10 (repeatedly #(test-reg-once func)))))

;;; The only values we will need for regression are numbers, hence we can simply
;;; use Java's Number as our type everywhere. Using nil would also be possible.

(def config-reg
     {
      ;; Some mathematical operators
      :function-set [(prim `- {:gp-type Number 
			       :gp-arg-types [Number Number]})

		     (prim `+ {:gp-type Number 
			       :gp-arg-types [Number Number]})

		     (prim `* {:gp-type Number 
			       :gp-arg-types [Number Number]})]

      ;; The two variables in the equation
      :terminal-set [(prim 'x {:gp-type Number})

		     (prim 'y {:gp-type Number})]

      ;; Evolved functions must return a number
      :root-type Number

      ;; Basic template for a fn with our arguments
      :func-template-fn (make-func-template '[x y])

      :evaluation-fn evaluate-reg

      ;; Keep tree size sane
      :validate-tree-fn #(< (tree-depth %) 10)

      :population-size 128

      :threads 2
      })


(defn run-reg
  "Run experiment and print summary when done."
  ([]
     (run-reg :basic-trees))
  ([print-type]
     (reduce-to-summary
      (map #(print-stats print-type %)
	   (generate-run config-reg)))))