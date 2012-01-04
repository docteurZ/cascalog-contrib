(defproject cascalog-graph "0.1.0"
  :description "Provides well-known graph analysis tools"
  :dependencies [[org.clojure/clojure "1.3.0"]
                 [cascalog "1.8.3"]]
  :dev-dependencies [[swank-clojure "1.4.0-SNAPSHOT"]
                     [org.apache.hadoop/hadoop-core "0.20.2-dev"]
                     [lein-multi "1.1.0-SNAPSHOT"]
                     [lein-marginalia "0.6.1"] ;; doc generation
                     [lein-midje "1.0.4"]      ;; for running midje tests
                     [midje-cascalog "0.3.0"]] ;; for testing: http://goo.gl/EXyEV
  :multi-deps {"1.2" [[org.clojure/clojure "1.2.1"]
                      [cascalog "1.8.3"]
                      [hadoop-util "0.2.4"]]
               "1.4" [[org.clojure/clojure "1.4.0-alpha2"]
                      [cascalog "1.8.3"]
                      [hadoop-util "0.2.4"]]})
