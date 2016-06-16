(ns yauhau.run-concurrency-test
  (:require [clojure.test :refer [deftest]]
            [com.ohua.compile :as o]
            [yauhau.concurrent-io-transform :as conc-io]
            [clojure.java.shell :refer [sh]]
            [com.ohua.logging :as l])
  (:use yauhau.util.program)
  (:import (yauhau.functions Functionality Functionality$SlowDummySource Functionality$DummyDataSource)))


(defmacro ohua [& args]
  `(o/ohua
     ~@args
     :compile-with-config {:df-transformations [
                                                ; batch rewrite
                                                ~@yauhau.ir-transform/transformations

                                                ; concurrent batch I/O rewrite
                                                conc-io/rewrite

                                                ; cache rewrite
                                                ;(partial cache/cache-rewrite "com.ohua.fetch.functions/round-persistent-cache-manager")
                                                ]}
     :run-with-config (doto (new com.ohua.engine.RuntimeProcessConfiguration)
                        (.setProperties (doto (new java.util.Properties)
                                          (.setProperty "execution-mode"
                                                        (.name (com.ohua.engine.RuntimeProcessConfiguration$Parallelism/MULTI_THREADED)))
                                          (.setProperty "core-thread-pool-size" "5")
                                          (.setProperty "max-thread-pool-size" "5")  
                                          )))
     ))

(defmacro ohua-no-conc [& args]
  `(o/ohua
     ~@args
     :compile-with-config {:df-transformations [
                                                ; batch rewrite
                                                ~@yauhau.ir-transform/transformations

                                                ; concurrent batch I/O rewrite
                                                ;conc-io/rewrite

                                                ; cache rewrite
                                                ;(partial cache/cache-rewrite "com.ohua.fetch.functions/round-persistent-cache-manager")
                                                ]}
     ;:run-with-config (doto (new com.ohua.engine.RuntimeProcessConfiguration)
     ;                   (.setProperties (doto (new java.util.Properties)
     ;                                     (.setProperty "execution-mode"
     ;                                                   (.name (com.ohua.engine.RuntimeProcessConfiguration$Parallelism/MULTI_THREADED)))
     ;                                     (.setProperty "arc-activation" "1")
     ;                                     (.setProperty "core-thread-pool-size" "5")
     ;                                     (.setProperty "max-thread-pool-size" "5") 
     ;                                     )))
     ))



(deftest concurrency-experiment
  (generate-graphs
    "-L" "Ohua"
    "-S"
    "-o" "com.ohua.fetch/test/clojure/generated/concurrency_test.clj"
    ;"-n" "1"
    "-n" "400"
    "-l" "20"
    "-s" "12345")
  (generate-graphs
    "-L" "Ohua"
    "-S"
    "-o" "com.ohua.fetch/test/clojure/generated/concurrency_test_no_rewrite.clj"
    ;"-n" "1"
    "-n" "400"
    "-l" "20"
    "-s" "12345")
  (set! Functionality$SlowDummySource/delay 10)
  (set! Functionality$DummyDataSource/delay 3)
  ;(set! (. com.ohua.engine.utils.GraphVisualizer PRINT_FLOW_GRAPH) (str "test/conc-io-rw-flow"))
  (let [functions (prepare-ns 'generated.concurrency-test #'ohua)
        results (doall (map
                         (fn [[f-name f]]
                           (let [levels (levels-from-fname f-name)]
                             (reset-counters!)
                             {"levels" (Integer/parseInt levels)
                              "time"   (do
                                         (println "Measuring runtime of function" f-name "...")
                                         (flush)
                                         (let [res (measure-exec-time (f))]
                                           (println "" res "ms")
                                           res))}))
                         functions))]
    (clojure.pprint/print-table results)
    (spit "test/concurrency-rewrite.json" (to-json results)))
  (let [functions (prepare-ns 'generated.concurrency-test-no-rewrite #'ohua-no-conc)
        results (doall (map
                         (fn [[f-name f]]
                           (let [levels (levels-from-fname f-name)]
                             (reset-counters!)
                             {"levels" (Integer/parseInt levels)
                              "time"   (do
                                         (print "Measuring runtime of function" f-name "...")
                                         (flush)
                                         (let [res (measure-exec-time (f))]
                                           (println "" res "ms")
                                           res))}))
                         functions))]
    (clojure.pprint/print-table results)
    (spit "test/concurrency-no-rewrite.json" (to-json results)))
  )
