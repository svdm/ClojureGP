;; Copyright (c) Stefan A. van der Meer. All rights reserved.
;; The use and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which
;; can be found in the file epl-v10.html at the root of this distribution. By
;; using this software in any fashion, you are agreeing to be bound by the
;; terms of this license. You must not remove this notice, or any other, from
;; this software.

(ns ant-exp
  "Example 03: the traditional Artifical Ant on the Santa Fe trail experiment.

  The aim is to evolve a function that through repeated execution makes the ant
  walk the entirety of the trail, eating all pieces of food on it.

  Note that for this problem one should expect a fairly low probability of
  success. It is not an area that GP performs particularly well in, even
  compared to random search, but I include it here as it is quite well known and
  included as example in established toolkits such as ECJ [1] and lil-gp [2].

  [1] http://www.cs.gmu.edu/~eclab/projects/ecj/
  [2] http://garage.cse.msu.edu/software/lil-gp/"
  (:use [cljgp.core :only [generate-run]]
        [clojure.contrib.def :only [defvar defunbound]]
        cljgp.util
        cljgp.config
        cljgp.tools.logging))


;;;; Trails

(defvar trail-santa-fe
  [[0 2 2 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
   [0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
   [0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 2 2 2 1 1 0 0]
   [0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 0 0 0 0 2 0 0]
   [0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 0 0 0 0 2 0 0]
   [0 0 0 2 2 2 2 1 2 2 2 2 2 0 0 0 0 0 0 0 1 2 2 1 1 0 0 0 0 1 0 0]
   [0 0 0 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 2 0 0]
   [0 0 0 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 1 0 0]
   [0 0 0 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 1 0 0]
   [0 0 0 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 2 0 0]
   [0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 1 0 0]
   [0 0 0 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 1 0 0]
   [0 0 0 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 2 0 0]
   [0 0 0 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 1 0 0]
   [0 0 0 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 2 0 0 1 1 1 2 2 2 1 0 0]
   [0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 1 2 1 1 1 0 0 2 0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0 0 0 0 0 2 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0 0 0 0 0 2 0 0 0 2 0 0 0 0 0 0 1 2 1 1 1 0 0 0 0]
   [0 0 0 0 0 0 0 0 0 0 0 0 2 0 0 0 2 0 0 0 0 0 0 0 0 0 0 2 0 0 0 0]
   [0 0 0 0 0 0 0 0 0 0 0 0 2 0 0 0 2 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0]
   [0 0 0 0 0 0 0 0 0 0 0 0 2 0 0 0 2 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0]
   [0 0 0 0 0 0 0 0 0 0 0 0 2 0 0 0 1 0 0 0 0 0 0 1 1 1 2 1 0 0 0 0]
   [0 0 0 0 0 0 0 0 0 0 0 0 2 0 0 0 1 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0]
   [0 1 1 2 2 1 1 2 2 2 2 2 1 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
   [0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
   [0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
   [0 2 0 0 0 0 0 1 2 2 2 2 2 2 2 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
   [0 2 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
   [0 1 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
   [0 1 2 2 2 2 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]]

  "The traditional Santa-Fe trail in 32x32 2D vector. Definition of
  characters: 0 is empty, 1 is marked as trail, 2 is food.")

;;;; Trail config
(defvar reference-trail trail-santa-fe
  "The trail used in the experiment, in its reference state, ie. with all food
  present.")

(defvar trail-size [(count (reference-trail 0)) (count reference-trail)]
  "Width and height of the trail")

(defvar trail-food 89
  "Number of food items on the trail.")

;;;; Evaluation

(defunbound ant-state
  "Var to be bound during evaluation to a map containing:
   - :trail    the trail state of the current ant
   - :pos      the position of the ant on the trail map
   - :dir      the direction in which the ant is facing
   - :moves    the number of moves made by the ant")

(defn evaluate-ant
  "Evaluates an evolved ant function by executing it 'max-moves times or until
  it has eaten all food on the trail.

  Returns a map containing:
  - :eaten    the number of food bits eaten
  - :moves    the number of actions performed
  - :fitness  the fitness value, defined as the number of food items remaining."
  [max-moves func ind]
  (binding [ant-state (atom {:trail reference-trail
                             :moves 0
                             :eaten 0
                             :pos [0 0]
                             :dir :right})]
    (while (and (< (:moves @ant-state) max-moves)
                (< (:eaten @ant-state) trail-food))
      (func))
    (let [{:keys [eaten moves]} @ant-state]
      {:eaten eaten
       :moves moves
       :fitness (- trail-food eaten)})))

;;;; Helpers

;;; These are a bit messy and too tightly integrated with the ant-state var
;;; hard to use in repl

(defn place
  "Returns value of trail map at the given coordinate."
  ([x y]
     (-> (:trail @ant-state) (nth y) (nth x)))
  ([[x y]]
     (place x y)))

(defn has-food?
  [the-place]
  (== the-place 2))

(defn pos-ahead
  "Returns the coordinates of the position that is in the given 'dir from the
  given position. The world wraps around at the edges."
  [[x y] dir]
  (let [[w h] trail-size]
    (condp = dir
      :up [x (if (<= y 0) (dec h) (dec y))]
      :down [x (rem (inc y) h)]
      :right [(rem (inc x) w) y]
      :left [(if (<= x 0) (dec w) (dec x)) y])))

(defn turn
  "Returns the direction in which the ant is facing if it turns in the given
  turning-direction while facing in the given facing-direction."
  [turning facing]
  (condp = turning
    :right (condp = facing
             :up :right
             :left :up
             :down :left
             :right :down)
    :left  (condp = facing
             :up :left
             :left :down
             :down :right
             :right :up)))

(defn perform-turn
  "Mutates the ant-state :dir to turn in the given direction."
  [direction]
  (let [state @ant-state
        newdir (turn direction (:dir state))
        newmoves (inc (:moves state))]
    (swap! ant-state
           assoc :dir newdir, :moves newmoves)))

(defn food-ahead?
  "Returns whether there is food ahead of the current position of the ant."
  []
  (let [state @ant-state]
    (has-food? (place (pos-ahead (:pos state) (:dir state))))))

;;; Primitive definitions:

(defmacro if-food-ahead
  "Like standard if, but with the condition predefined as (food-ahead?)."
  [then-expr else-expr]
  `(if (food-ahead?)
     ~then-expr
     ~else-expr))

(defn left
  "Makes the ant's direction one to the left of its current heading."
  []
  (perform-turn :left))

(defn right
  "Makes the ant's direction one to the right of its current heading."
  []
  (perform-turn :right))

(defn move
  "Moves the ant into the position ahead of it. If the position contains food
  marks it as eaten and increments :eaten count."
  []
  (let [{:keys [pos dir trail eaten moves]} @ant-state
        newpos (pos-ahead pos dir)]
    (if (has-food? (place newpos))
      (swap! ant-state assoc
             :pos newpos
             :eaten (inc eaten)
             :moves (inc moves)
             :trail (assoc-in trail (rseq newpos) 3)) ; 3 = eaten
      (swap! ant-state assoc
             :pos newpos
             :moves (inc moves)))))


;;; Experiment configuration:

;;; Simple type hierarchy purely to avoid nested ifs with unreachable branches
(derive ::cond ::any)
(derive ::action ::any)

(def config-ant
     {
      :function-set [(primitive `if-food-ahead
                                {:gp-type ::cond
                                 :gp-arg-types [::action
                                                ::action]})

                     ;; "progn2"
                     (primitive `do
                                {:gp-type ::action
                                 :gp-arg-types [::any
                                                ::any]})

                     ;; "progn3"
                     ;; Does not necessarily help in finding a solution
                     ;; (primitive `do
                     ;;            {:gp-type ::action
                     ;;             :gp-arg-types [::any
                     ;;                            ::any
                     ;;                            ::any]})

                     ;; move/left/right are side-effect functions that do not
                     ;; take arguments but do need to be applied (hence they are
                     ;; not in the terminal set)
                     (primitive `move
                                {:gp-type ::action
                                 :gp-arg-types []})

                     (primitive `left
                                {:gp-type ::action
                                 :gp-arg-types []})

                     (primitive `right
                                {:gp-type ::action
                                 :gp-arg-types []})]

      :terminal-set []

      :root-type ::any

      :evaluation-fn #(evaluate-ant 600 %1 %2)

      :population-size 1024
      :end-condition-fn (make-end 50)
      :validate-tree-fn #(and (<= (tree-depth %) 15)
                              (<= (tree-size %) 30))

      :threads 2
      :rand-seeds (seeds-from-time true)

      ;; Succesful seeds for 2 threads, without the "progn3" version of 'do in
      ;; the function set:
      ;;   [298593938264 975401385660]
      })

(defn run
  "Run the experiment and print the best individual at the end. The 'print-type
  parameter determines how the statistics are printed to stdout for each
  generation. See cljgp.tools.logging/print-stats for details."
  ([]
     (run :basic-trees))
  ([print-type]
     (reduce-to-summary
      (map #(print-stats print-type %)
           (generate-run config-ant)))))


;;;; Debugging and visualisation of results

(defn traced-move
  "Version of move that places a \"4\" marker when moving into a tile with no
  food."
  []
  (let [{:keys [pos dir trail eaten moves]} @ant-state
        newpos (pos-ahead pos dir)
        hasfood (has-food? (place newpos))]
    (swap! ant-state assoc
           :pos newpos
           :eaten (if hasfood (inc eaten) eaten)
           :moves (inc moves)
           :trail (assoc-in trail (rseq newpos) (if hasfood 3 4)))))

(defn trace-ant
  "Like evaluate-ant, but traces the ant's movements on the map by placing \"4\"
  markers on points it visits that do not contain food. The ant's route can then
  be traced by following the path of \"3\" and \"4\" markers on the map."
  ([max-moves func]
     (binding [ant-state (atom {:trail reference-trail
                                :moves 0
                                :eaten 0
                                :pos [0 0]
                                :dir :right})
               move traced-move]
       (while (and (< (:moves @ant-state) max-moves)
                   (< (:eaten @ant-state) trail-food))
         (func))
       (let [{:keys [trail eaten moves]} @ant-state]
         {:trail trail
          :eaten eaten
          :moves moves
          :fitness (- trail-food eaten)})))
  ([max-moves func ind]
     (trace-ant max-moves func)))

;;; The following program solves the problem in 600 steps. It's the solution
;;; given by Koza in his description of the experiment.
(def ideal-solution
     (fn []
       (if-food-ahead
        (move)
        (do
          (left)
          (do
            (if-food-ahead
             (move)
             (right))
            (do (right)
                (do (left)
                    (right))))
          (do (if-food-ahead
               (move)
               (left))
              (move))))))

;; The following program evolved using the above configuration (see the example
;; seeds) also solves it in 600 steps
(def evolved-solution
     (fn []
       (do
         (do
           (do
             (left)
             (if-food-ahead
              (do
                (if-food-ahead
                 (do (if-food-ahead (move)
                                    (right))
                     (move))
                 (right))
                (move))
              (do
                (if-food-ahead (move)
                               (right))
                (move))))
           (right))
         (if-food-ahead (move)
                        (left)))))


;; Simplified version of the evolved solution with excess code removed
(def evolved-simplified
     (fn []
       (do
         (left)
         (if-food-ahead
          (do
            (move)
            (move)
            (move))
          (do
            (right)
            (move)))
         (right)
         (if-food-ahead
          (move)
          (left)))))