
(set! *warn-on-reflection* true)

(ns cljgptest
  (:import java.util.Random)
  (:require [cljgp.tools.logging :as gp-log]
	    [cljgp.tools.graph :as gp-graph])
  (:use [cljgp.core :only (generate-run)]
	[cljgp.evaluation :only (best-fitness)]
	[cljgp.selection :only (tournament-select)]
	[cljgp.breeding :only (crossover-breeder 
			       mutation-breeder 
			       reproduction-breeder
			       generate-ramped)]
	cljgp.util
	cljgp.random
	cljgp.config))

;java -cp .;k:/clojure/svn-trunk/clojure.jar;k:/clojure/contrib/clojure-contrib.jar;./lib/plot.jar clojure.lang.Repl cljgptest.clj




;
; MATHS PROBLEM DEF
;

(defn sdiv [a b] (if (> b 0) (/ a b) 0))
(defn except-me [a] (throw (Exception. "I'm an exception!")))


;
; SIMPLE REGRESSION PROBLEM DEF
;

; vars we will bind during eval
(def X 0)
(def Y 0)

(def func-set-mvr
     [{:sym `- :arity 2}
      {:sym `+ :arity 2}
      {:sym `* :arity 2}
      #_{:sym `sdiv :arity 2}])

(def term-set-mvr
     [{:sym 'x :as-arg true}
      {:sym 'y :as-arg true}])


(comment
  (def select-mvr (partial tournament-select 7))

 
  (def breeders-mvr
      [{:prob 0.8    :breeder-fn (partial crossover-breeder 
					  select-mvr)}

       {:prob 0.1    :breeder-fn (partial mutation-breeder 
					  func-set-mvr term-set-mvr
					  select-mvr)}
      
       {:prob 0.1    :breeder-fn (partial reproduction-breeder
					  select-mvr)}]))


(defn test-mvr-once
  [func]
  (binding [X (gp-rand)
	    Y (gp-rand)]
      (let [result (func)
	    target (+ Y (+ (* X Y) (* Y (* X X))))] ;(x^2)y + xy + y 
	(Math/abs (float (- target result))))))

(defn test-mvr-once-args
  [func]
  (let [rx (gp-rand)
	ry (gp-rand)
	result (func rx ry)
	target (+ ry (+ (* rx ry) (* ry (* rx rx))))] ;(x^2)y + xy + y 
	(Math/abs (float (- target result)))))

(defn evaluate-mvr
  "Evaluate by testing the function on random data points and summing its
  error.

  Takes an (eval'd) function and returns a fitness value."
  [func]
  (reduce + (take 10 (repeatedly #(test-mvr-once-args func)))))

(def config-mvr
     {
      :function-set [(prim `- {:type Number 
			       :arg-type [Number Number]})

		     (prim `+ {:type Number 
			       :arg-type [Number Number]})

		     (prim `* {:type Number 
			       :arg-type [Number Number]})

		     (prim `sdiv {:type Number 
				  :arg-type [Number Number]})]

      :terminal-set [(prim 'x {:type Number
			       :as-arg true})

		     (prim 'y {:type Number
			       :as-arg true})]

      :arg-list '[x y]

      :root-type Number

      :evaluation-fn evaluate-mvr
      :end-condition (make-simple-end 50 0.0001)

      :breeders [{:prob 0.8    :breeder-fn crossover-breeder}
		 {:prob 0.1    :breeder-fn mutation-breeder}
		 {:prob 0.1    :breeder-fn reproduction-breeder}]
      :breeding-retries 5

      :validate-tree-fn #(< (tree-depth %) 20)
      :selection-fn (partial tournament-select 7)
      :pop-generation-fn (partial generate-ramped 7 0.5)

      :population-size 128

      :threads 2

      :rand-fn-maker make-default-rand
      :rand-seeds [21312 773290 4901 9928]
      })


(defn run-mvr
  []
  (best-fitness
   (last
    (map gp-log/print-details (generate-run config-mvr)))))

(defn bench-mvr
  [threads]
  (time (dorun (generate-run (assoc config-mvr
			       :threads threads)))))

(defn run-mvr-graphed
  []
  (best-fitness
   (last
    (map (gp-graph/create-fitness-plotter false)
	 (map gp-log/print-details 
	      (generate-run config-mvr))))))

;
; END MVR
;

(do
  (derive ::num ::any)
  (derive Number ::num)
  (derive ::seq ::any)
  (derive ::string ::seq)

  (def test-tree-a
       [(prim '+ {:type Number :arg-type [Number Number] :arity 2})
	[(prim '* {:type Number :arg-type [Number Number] :arity 2})
	 (prim 'x {:type Number})
	 (prim 'y {:type Number})]
	[(prim 'count {:type Number :arg-type [::seq] :arity 1})
	 [(prim 'concat {:type ::seq :arg-type [::seq ::seq] :arity 2})
	  (prim 'str-a {:type ::string})
	  (prim 'str-b {:type ::string})]]])

  (def test-tree-b
       [(prim '- {:type Number :arg-type [Number Number] :arity 2})
	[(prim 'count {:type Number :arg-type [::seq] :arity 1})
	 (prim 'str-c {:type ::string})]
	(prim 'z {:type Number})]))


(comment
  (defn run-test
    []
    (best-fitness 
     (last 
      (map (gp-graph/create-fitness-plotter true)
	   (map gp-log/print-details (generate-run func-set-maths term-set-maths 
						   evaluate-maths breeders-maths 
						   16 (make-simple-end 50))))))))
