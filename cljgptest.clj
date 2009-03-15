
(ns cljgptest
  (:require [cljgp.tools.logging :as gp-log]
	    [cljgp.tools.graph :as gp-graph])
  (:use [cljgp.core :only (generate-run 
			   make-simple-end
			   best-fitness)]
	[cljgp.selection :only (tournament-select)]
	[cljgp.breeding :only (crossover-breeder 
			       mutation-breeder 
			       reproduction-breeder
			       generate-ramped)]
	[cljgp.random :only (gp-rand)]))

;java -cp .;k:/clojure/svn-trunk/clojure.jar;./lib/plot.jar clojure.lang.Repl cljgptest.clj


(set! *warn-on-reflection* true)

;
; MATHS PROBLEM DEF
;

(defn sdiv [a b] (if (> b 0) (/ a b) 0))
(defn except-me [a] (throw (Exception. "I'm an exception!")))

(def func-set-maths 
     [{:sym '- :arity 2 :as-arg true}
      {:sym `+ :arity 2}
      {:sym `* :arity 2}
      {:sym `sdiv :arity 2}])

(def term-set-maths
     [{:sym 1}
      {:sym 2}
      {:sym 3}
      {:sym 4}
      {:sym 5}])

(def my-select (partial tournament-select 7))

(def breeders-maths
     [{:prob 0.8    :breeder-fn (partial crossover-breeder 
					 my-select)}

      {:prob 0.1    :breeder-fn (partial mutation-breeder 
					 func-set-maths term-set-maths
					 my-select)}
      
      {:prob 0.1    :breeder-fn (partial reproduction-breeder
					 my-select)}])

(defn evaluate-maths
  "Super-simple evaluation that selects for approximating 42.

  Takes an (eval'd) function and returns a fitness value."
  [func]
  (let [result (func)]
    (Math/abs (float (- 42 result)))))


(def config-maths
     {
      :function-set func-set-maths
      :terminal-set term-set-maths
      :argument-list [] ; if any as-arg

      :evaluation-fn evaluate-maths

      :end-condition (make-simple-end 100 -1) #_(make-simple-end 50)
      :population-size 16

      :breeders breeders-maths
      :pop-generation-fn (partial generate-ramped func-set-maths 
				                  term-set-maths
						  7 0.5)
      })

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
  "Evaluate by testing the function on ten random data points and summing its
  error.

  Takes an (eval'd) function and returns a fitness value."
  [func]
  (reduce + (take 5 (repeatedly #(test-mvr-once-args func)))))

(def config-mvr
     {
      :function-set [{:sym `- :arity 2}
		     {:sym `+ :arity 2}
		     {:sym `* :arity 2}]

      :terminal-set [{:sym 'x :as-arg true}
		     {:sym 'y :as-arg true}]

      :arg-list ['x 'y] ; can't generate automatically user has to know order

      :evaluation-fn evaluate-mvr
      :end-condition (make-simple-end 50)

      :breeders [{:prob 0.8    :breeder-fn crossover-breeder}
		 {:prob 0.1    :breeder-fn mutation-breeder}
		 {:prob 0.1    :breeder-fn reproduction-breeder}]

      :selection-fn (partial tournament-select 7)

      :population-size 64
      :pop-generation-fn (partial generate-ramped 7 0.5)
      })

(defn run-mvr
  []
  (best-fitness
   (last
    (map gp-log/print-details (generate-run config-mvr)))))

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

(defn run-test
    []
    (best-fitness 
     (last 
      (map gp-log/print-details (generate-run config-maths)))))



(defn time-test
  []
  (time
   (best-fitness 
    (last 
     (generate-run config-maths)))))



(comment
  (defn run-test
    []
    (best-fitness 
     (last 
      (map (gp-graph/create-fitness-plotter true)
	   (map gp-log/print-details (generate-run func-set-maths term-set-maths 
						   evaluate-maths breeders-maths 
						   16 (make-simple-end 50))))))))

; TODO:

; - source control

; - update docstrings re: config

; - random seeds for determinism
;   - is going to require 1) per-thread random gens, 2) explicit threading
;   - add thread managing function that binds an rng to each thread

; - consider naming consistency of functions and config keys

; - typed GP
;   - store type data in metadata?
;   - we need to know what type a node *satisfies* in a tree, not only what type
;     it is. Hence we either need to look back to the function, which seems not
;     that easy, or store for every node what type it satisfies for super-quick
;     reference. The meta option seems the least hack-ish and the most trans-
;     parant to non-typed gp.