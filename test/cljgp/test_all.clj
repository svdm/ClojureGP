;; Copyright (c) Stefan A. van der Meer. All rights reserved.
;; The use and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which
;; can be found in the file epl-v10.html at the root of this distribution. By
;; using this software in any fashion, you are agreeing to be bound by the
;; terms of this license. You must not remove this notice, or any other, from
;; this software.

(ns cljgp.tests.test-all
  (:use [clojure.test :only (run-tests)]))

; heavily borrowed from contrib.test_clojure.clj

(def test-names
     [:test-util
      :test-evaluation
      :test-random
      :test-breeding
      :test-generate
      :test-selection
      :test-core
      :test-config])

(def test-namespaces
     (map #(symbol (str "test.cljgp." (name %))) test-names))

(defn run
  []
  (println "Loading tests...")
  (apply require :reload-all test-namespaces)
  (apply run-tests test-namespaces))

(run)
(System/exit 0)
