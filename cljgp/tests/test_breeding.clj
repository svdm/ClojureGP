
;;; cljgp.tests.test_breeding.clj

(ns cljgp.tests.test-breeding
  (:use clojure.contrib.test-is
	cljgp.breeding
	cljgp.evaluation
	cljgp.tests.helpers
	cljgp.util))


(def func-set-maths (:function-set config-maths))
(def term-set-maths (:terminal-set config-maths))

(def my-gen (partial generate-tree func-set-maths term-set-maths))

; for this primitive set, trees should always return a number
(def valid-result? number?)


(defn valid-eval?
  "When evaluated, does this tree produce a valid result?"
  [tree]
  (valid-result? (eval tree)))

; generation function tests

(deftest test-generate-tree
  (testing "grow method"
    (let [max-depth 4
	  tree (my-gen max-depth :grow)
	  depth (tree-depth tree)]
      (is (valid-tree? tree)
	  "Root must either be a seq, or a valid result constant.")
      (is (and (> depth 0) (<= depth max-depth))
	  "Grow-method trees must be of a valid size up to the limit.")
      (is (= (tree-depth (my-gen 0 :grow)) 1) 
	  "For max-depth 0, must return single node.")
      (is (valid-eval? tree)
	  "Result of evaluating tree must be valid.")))
  (testing "full method"
    (let [max-depth 4
	  tree (my-gen max-depth :full)]
      (is (valid-tree? tree)
	  "Root must either be a seq, or a valid result constant.")
      (is (= (tree-depth tree) max-depth)
	  "Full-method trees must be the given max-depth in size.")
      (is (= (tree-depth (my-gen 0 :full)) 1)
	  "For max-depth 0, must return single node.")
      (is (valid-eval? tree)
	  "Result of evaluating tree must be valid."))))

(deftest test-generate-ramped
  (let [d 4
	grown-tree (generate-ramped d 1 func-set-maths term-set-maths)
	full-tree (generate-ramped d 0 func-set-maths term-set-maths)
	rand-tree (generate-ramped d 0.5 func-set-maths term-set-maths)]
    (testing "generated tree validity"
	     (testing "basic structural requirements"
		      (is (valid-tree? grown-tree))
		      (is (valid-tree? full-tree))
		      (is (valid-tree? rand-tree)))
	     (testing "evaluation requirements"
		      (is (valid-eval? grown-tree))
		      (is (valid-eval? full-tree))
		      (is (valid-eval? rand-tree))))
    (is (<= (tree-depth full-tree) d)
	"Ramped gen with 0% grow chance should result in a full tree.")
    (is (<= (tree-depth grown-tree) d)
	"Ramped gen with 100% grow chance should result in a grown tree.")))

(deftest test-generate-pop
  (let [target-size (:population-size config-maths)
	pop (doall (generate-pop config-maths))]
    (is (seq pop)
	"Generated population must be a valid seq-able.")
    (is (= (count pop) target-size)
	"Generated population should be of the specified size.")
    (is (empty? (filter #(not (valid-tree? (get-fn-body (:func %)))) pop))
	"All generated trees must be valid.")))


; breeding function tests

(deftest test-tree-replace
  (let [tree-orig '(+ (* (+ 1 2) (- 3 4)) (/ (/ 2 1) (+ 4 5)))]
    (doseq [i (range (count (make-tree-seq tree-orig)))]
      (is (= :test
	     (nth (make-tree-seq (tree-replace i :test tree-orig)) i))
	  "Replace should work correctly in any tree node."))))

(deftest test-crossover-uniform
  (let [trees (crossover-uniform '(+ (- 1 2) (* 3 4)) '(* 2 3))]
    (is (= (count trees) 2))
    (is (every? valid-tree? trees)
	"Should result in two valid trees.")
    (is (every? valid-eval? trees))))

(deftest test-mutate
  (let [tree (mutate '(+ (- 1 2) (* 3 4)) func-set-maths term-set-maths)]
    (is (valid-tree? tree))
    (is (valid-eval? tree))))


(defn test-inds
  [inds gen-old num-expected]
  (let [trees (map #(get-fn-body (:func %)) inds)]
    (is (seq inds))
    (is (= (count inds) num-expected))
    (is (every? valid-ind? inds)
	"All individuals should be maps with the required keys")
    (is (every? #(= (:gen %) (inc gen-old)) inds)
	"New individuals should have incremented :gen.")
    (is (every? valid-tree? trees)
	"New individuals should have valid trees.")
    (is (every? valid-eval? trees)
	"New inds should evaluate to valid results.")))

(deftest test-crossover-inds
  (let [gen-old 0
	inds (crossover-inds crossover-uniform 
			     (make-individual '(+ 1 2) gen-old [])
			     (make-individual '(* 3 4) gen-old [])
			     [])]
    (test-inds inds gen-old 2)))

(deftest test-mutate-ind
  (let [gen-old 0
	ind (mutate-ind func-set-maths term-set-maths
			(make-individual '(+ 1 2) gen-old [])
			[])]
    (test-inds ind gen-old 1)))

(deftest test-reproduce-ind
  (let [gen-old 0
	ind-old (make-individual '(+ 1 2) gen-old [])
	ind (reproduce-ind ind-old [])]
    (test-inds ind gen-old 1)
    (is (= (dissoc ind-old :gen) (dissoc (first ind) :gen))
	"Reproduced individuals should be identical apart from :gen")))

; *-breeder functions not tested currently as they are very basic compositions
; of a selection function and the *-ind functions
; perhaps TODO

#_(defn fake-evaluation
  [pop]
  (map #(assoc % :fitness (rand)) pop))

(deftest test-breed-new-pop
  (let [target-size (:population-size config-maths)
	pop-evaluated (evaluate-pop (generate-pop config-maths) config-maths)
	pop-new (breed-new-pop pop-evaluated config-maths)]
    (is (seq (doall pop-new)))
    (is (= (count pop-new) target-size))))
