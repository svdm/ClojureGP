;;; cljgp.tests.test_config.clj

(ns cljgp.tests.test-config
  (:use clojure.contrib.test-is
	cljgp.config
	cljgp.tests.helpers
	cljgp.core))

(deftest test-make-simple-end
  (let [pop [{:gen 10 :fitness 5} {:gen 10 :fitness 4.9999}]]
    (testing "generation limit"
	     (is ((make-simple-end 9) pop))
	     (is ((make-simple-end 10) pop))
	     (is (not ((make-simple-end 11) pop))))
    (testing "fitness limit"
	     (is ((make-simple-end 99 6) pop))
	     (is ((make-simple-end 99 4.9999) pop))
	     (is (not ((make-simple-end 99 4.5) pop)))
	     (is (not ((make-simple-end 99 0.1) pop))))))

(deftest test-strict-every?
  (is (strict-every? true? [true true true]))
  (is (not (strict-every? true? [true false true])))
  (is (not (strict-every? true? []))))

; Trying to be exhaustive with these tests as correct config validation can
; prevent many bugs elsewhere.
(deftest test-validators
  (testing "valid-func-entry?"
	   (let [vfe valid-func-entry?]
	     (is (vfe {:sym 'a :arity 2}))
	     (is (vfe {:sym `+ :arity 4}))
	     (is (not (vfe [])))
	     (is (not (vfe {})))
	     (is (not (vfe {:sym 'a})))
	     (is (not (vfe {:arity 2})))
	     (is (not (vfe {:sym 1 :arity 2})))
	     (is (not (vfe {:sym 'b :arity true})))))
  (testing "valid-term-entry?"
	   (let [vte valid-term-entry?]
	     (is (vte {:sym 'x}))
	     (is (vte {:sym 1}))
	     (is (not (vte {})))
	     (is (not (vte [])))
	     (is (not (vte {:sym +})))))
  (testing "valid-breeder-entry?"
	   (let [vbe valid-breeder-entry?
		 b-fn #(println "mock breeder")]
	     (is (vbe {:prob 0.1 :breeder-fn b-fn}))
	     (is (not (vbe {})))
	     (is (not (vbe [])))
	     (is (not (vbe {:prob 1})))
	     (is (not (vbe {:breeder-fn b-fn})))
	     (is (not (vbe {:prob true :breeder-fn b-fn})))
	     (is (not (vbe {:prob 1 :breeder-fn 'foo}))))))

(deftest test-check-key
  (let [conf (dissoc config-maths :arg-list)]
    (is (nil? (check-key :foo true? conf))
	"If a key does not exist and there is no default, fail.")
    (is (nil? (check-key :breeders (constantly false) conf))
	"If key exists but fails test, fail.")
    (is (= (check-key :population-size number? conf) 
	   [:population-size (:population-size conf)])
	"If key exists and val passes test, return map entry.")
    (is (= (quiet (check-key :arg-list true? conf)) 
	   (find config-defaults :arg-list))
	"If key does not exist and default does, return default entry.")))

(deftest test-check-config
  (let [conf (dissoc config-maths :arg-list)
	conf-broken (dissoc config-maths :function-set)]
    (is (= (quiet (check-config conf)) 
	   (assoc conf :arg-list (:arg-list config-defaults)))
	"Missing key should be replaced by default if one exists.")
    (is (thrown? Exception (check-config conf-broken))
	"Missing key with no default should result in exception.")
    (is (thrown? Exception 
		 (check-config (assoc conf-broken :function-set [])))
	"Invalid value for key should result in exception.")))


(deftest test-assert-constraints
  (let [conf-broken (assoc config-maths :threads 999)]
    (is (thrown? Exception (assert-constraints conf-broken))
	"Too many threads for the number of seeds should fail.")
    (is (= config-maths (assert-constraints config-maths))
	"Valid config should be returned with no exceptions thrown.")))
