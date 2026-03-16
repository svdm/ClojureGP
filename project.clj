(defproject cljgp "1.0.1"
  :dependencies [[org.clojure/clojure "1.12.4"]
                 ;; For random number generator
                 [org.uncommons.maths/uncommons-maths "1.2.2a"]]

  :profiles {:dev {:dependencies [[nrepl/nrepl "1.3.0"]]}}

  :source-paths ["src" "examples"]
  :test-paths ["test" "."])
