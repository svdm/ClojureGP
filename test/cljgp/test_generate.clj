;; Copyright (c) Stefan A. van der Meer. All rights reserved.
;; The use and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which
;; can be found in the file epl-v10.html at the root of this distribution. By
;; using this software in any fashion, you are agreeing to be bound by the
;; terms of this license. You must not remove this notice, or any other, from
;; this software.

(ns test.cljgp.test-generate
  (:use clojure.contrib.test-is
        cljgp.generate
        cljgp.util)
  (:refer test.helpers))


(deftest test-generate-tree
  (testing "grow method"
    (let [max-depth 4
          tree (my-gen max-depth :grow rtype)
          depth (tree-depth tree)]
      (full-tree-test tree)
      (is (and (> depth 0) (<= depth max-depth))
          "Grow-method trees must be of a valid size up to the limit.")
      (is (= (tree-depth (my-gen 0 :grow rtype)) 1) 
          "For max-depth 0, must return single node.")))
  (testing "full method"
    (let [max-depth 4
          tree (my-gen max-depth :full rtype)]
      (full-tree-test tree)
      (is (= (tree-depth tree) max-depth)
          "Full-method trees must be the given max-depth in size.")
      (is (= (tree-depth (my-gen 0 :full rtype)) 1)
          "For max-depth 0, must return single node."))))

(deftest test-generate-ramped
  (let [d 4
        gtor (fn [opts] (get-valid (comp not nil?) 512 
                                   #(generate-ramped opts config-maths)))
        grown-tree (gtor {:max-depth d :grow-chance 1})
        full-tree (gtor {:max-depth d :grow-chance 0})
        rand-tree (gtor {:max-depth d :grow-chance 0.5})]
    (testing "generated tree validity"
             (full-tree-test grown-tree)
             (full-tree-test full-tree)
             (full-tree-test rand-tree))
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
    (is (empty? (filter #(not (valid-tree? (get-fn-body (get-func %)))) pop))
        "All generated trees must be valid.")))

(deftest test-get-valid
  (is (nil? (get-valid true? 2 #(vector [false false false]))))
  (is (= [1 2] (get-valid number? 1 #(vector 1 2)))))