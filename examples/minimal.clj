;; Copyright (c) Stefan A. van der Meer. All rights reserved.
;; The use and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which
;; can be found in the file epl-v10.html at the root of this distribution. By
;; using this software in any fashion, you are agreeing to be bound by the
;; terms of this license. You must not remove this notice, or any other, from
;; this software.

(ns minimal
  "A minimal experiment skeleton.

  This experiment does not necessarily do a great job at showing what a typical
  experiment would look like, or even a basic experiment. It simply tries to be
  as minimal and simplistic as possible while still performing a gp run."
  (:use [cljgp.core :only (generate-run)]
	[cljgp.tools.logging :only (reduce-to-summary
				    print-stats)]
	[cljgp.tools.analyse :only (best-fitness)]
	[cljgp.config :only (prim)]))

;(set! *warn-on-reflection* true)

(defn evaluate-minimal
  "Evaluate how well a given function fits the target of returning the square of
  the number 5."
  [func]
  (let [result (func 5)
	target (Math/pow 5 2)] 
    (Math/abs (float (- target result)))))

;
(def config-minimal
     {
;;; Function arguments are essentially untyped, but we do need to indicate how
;;; many args they take, hence specify them as type nil.
      :function-set [(prim `+ {:gp-arg-types [nil nil]})
		     (prim `- {:gp-arg-types [nil nil]})]

;;; The only terminal is a non-namespaced symbol that refers to the evolved
;;; function's only argument. No type info.
      :terminal-set [(prim 'x nil)]

;;; The arg-list of the evolved function then is just a single-element vector.
;;; The config preprocessing step will turn this into the function template
;;; required by the gp process. We could do that ourselves by replacing this
;;; :arg-list option with: 
;;;   :func-template-fn (make-func-template '[x]) 
;;; Or we could specify our own custom function.
      :arg-list '[x]

;;; We already defined our trivial evaluator earlier.
      :evaluation-fn evaluate-minimal

;;; Tiny population for our extremely simple goal.
      :population-size 4

;;; Some of the things left on their defaults that typical experiments would
;;; customize are the breeders used, the end condition used, the initial
;;; population generation function used, and many more. The config preprocessing
;;; step in the cljgp.core/generate-run call will print the keys that fall back
;;; to default values.
      
      })


(defn run-min
  "Performs the minimalistic run and does basic reporting."
  []
  (reduce-to-summary
   (map print-stats 
	(generate-run config-minimal))))

(defn run-min-quiet
  "Performs minimalistic run even more minimalistically, without reporting."
  []
  (best-fitness (last (generate-run config-minimal))))

