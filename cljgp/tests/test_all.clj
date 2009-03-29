
;; cljgp.tests.run_tests.clj

(ns cljgp.tests.test-all
  (:use [clojure.contrib.test-is :only (run-tests)])
  (:gen-class))

; heavily borrowed from contrib.test_clojure.clj

(def test-names
     [:test-util
      :test-evaluation
      :test-random
      :test-breeding
      :test-selection
      :test-core
      :test-config])

(def test-namespaces
     (map #(symbol (str "cljgp.tests." (name %))) test-names))

(defn run
  []
  (println "Loading tests...")
  (apply require :reload-all test-namespaces)
  (apply run-tests test-namespaces))

(run)

