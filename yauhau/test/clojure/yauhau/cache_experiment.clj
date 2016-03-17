;
; Copyright (c) Justus Adam, Sebastian Ertel, Andres Goens 2016. All Rights Reserved.
;
; This source code is licensed under the terms described in the associated LICENSE file.
;

(ns yauhau.cache-experiment
  (:require [clojure.test :as test :refer [deftest]]
            [com.ohua.compile :as o]
            [com.ohua.fetch.cache-transform :as cache]
            [clojure.java.io :refer [file copy]]
            [com.ohua.fetch.util.program :refer [prepare-ns levels-from-fname to-json generate-graphs]])
  (:import (yauhau.operators AccumOp Functionality Functionality$DummyDataSource Caching Functionality$GenericPayload)
           (java.lang Integer)))


(defmacro round-persistent-cache-ohua [& args]
  `(o/ohua
     ~@args
     :compile-with-config {:df-transformations [
                                                ; batch rewrite
                                                ~@com.ohua.fetch.ir-transform/transformations

                                                ; concurrent batch I/O rewrite
                                                ;conc-io/rewrite

                                                ; cache rewrite
                                                (partial cache/cache-rewrite "com.ohua.fetch.operators/round-persistent-cache-manager")
                                                ]}))


(defmacro request-persistent-cache-ohua [& args]
  `(o/ohua
     ~@args
     :compile-with-config {:df-transformations [
                                                ; batch rewrite
                                                ~@com.ohua.fetch.ir-transform/transformations

                                                ; concurrent batch I/O rewrite
                                                ;conc-io/rewrite

                                                ; cache rewrite
                                                (partial cache/cache-rewrite "com.ohua.fetch.operators/request-persistent-cache-manager")
                                                ]}))


(defmacro request-persistent-flush-cache-ohua [& args]
  `(o/ohua
     ~@args
     :compile-with-config {:df-transformations [
                                                ; batch rewrite
                                                ~@com.ohua.fetch.ir-transform/transformations

                                                ; concurrent batch I/O rewrite
                                                ;conc-io/rewrite

                                                ; cache rewrite
                                                (partial cache/cache-rewrite "com.ohua.fetch.operators/request-persistent-flush-cache-manager")
                                                ]}))


(defmacro round-persistent-flush-cache-ohua [& args]
  `(o/ohua
     ~@args
     :compile-with-config {:df-transformations [
                                                ; batch rewrite
                                                ~@com.ohua.fetch.ir-transform/transformations

                                                ; concurrent batch I/O rewrite
                                                ;conc-io/rewrite

                                                ; cache rewrite
                                                (partial cache/cache-rewrite "com.ohua.fetch.operators/round-persistent-flush-cache-manager")
                                                ]}))


(def repetition-factor 5)



(defn run-tests [type test-fns]
  (let [average (fn [i] (double (/ i repetition-factor)))]
    (into []
          (doall
            (map (fn [[f-name f]]
                   ;(println "Starting execution of function: " f-name)
                   (let [levels (levels-from-fname f-name)]
                     ;prepare
                     (set! Functionality/IO_FETCH_COUNTER 0)
                     (set! Functionality/READ_REQUEST_COUNTER 0)
                     (set! Functionality/WRITE_REQUEST_COUNTER 0)
                     (set! AccumOp/IO_ROUND_COUNTER 0)
                     (set! AccumOp/IO_FETCH_COUNTER 0)
                     ; execute the function
                     (.clear (Caching/getPersistentCacheMap))
                     (if (contains? #{"request-persistent" "request-persistent-flush"} type)
                       (do (doall (repeatedly repetition-factor f))
                           (.clear (Caching/getPersistentCacheMap))
                           {
                            ;"fetches_made"     (Functionality/IO_FETCH_COUNTER)
                            "rounds_made"     (average (AccumOp/IO_ROUND_COUNTER))
                            "levels"          (Integer. ^String levels)
                            "reads_requested" (average (Functionality/READ_REQUEST_COUNTER))
                            "write_requests"  (average (Functionality/WRITE_REQUEST_COUNTER))
                            "reads_performed" (average (AccumOp/IO_FETCH_COUNTER))})
                       (do (f)
                           {
                            ;"fetches_made"     (Functionality/IO_FETCH_COUNTER)
                            "rounds_made"     (AccumOp/IO_ROUND_COUNTER)
                            "levels"          (Integer. ^String levels)
                            "reads_requested" (Functionality/READ_REQUEST_COUNTER)
                            "write_requests"  (Functionality/WRITE_REQUEST_COUNTER)
                            "reads_performed" (AccumOp/IO_FETCH_COUNTER)}))))
                 test-fns)))))


(defn run-for-cache [name fns]
  (let [results (run-tests name fns)]
    (clojure.pprint/print-table results)
    (spit (str "test/" name ".json") (to-json results))))


(deftest cache-experiment
  (let [source-file "com.ohua.fetch/test/clojure/generated/cache_experiment.clj"
        round-persistent-file "com.ohua.fetch/test/clojure/generated/round_persistent_experiment.clj"
        request-persistent-file "com.ohua.fetch/test/clojure/generated/request_persistent_experiment.clj"
        round-persistent-flush-file "com.ohua.fetch/test/clojure/generated/round_persistent_flush_experiment.clj"
        request-persistent-flush-file "com.ohua.fetch/test/clojure/generated/request_persistent_flush_experiment.clj"]
    ;(println (Functionality$GenericPayload/testEquality))))
    (generate-graphs
      "-L" "Ohua"
      "-o" source-file
      "-n" "400"
      "-l" "20"
      "-c" "7"
      "--percentagesinks" "0.1"
      "-s" "12345")
    (copy (file source-file) (file round-persistent-file))
    (copy (file source-file) (file request-persistent-file))
    (copy (file source-file) (file round-persistent-flush-file))
    (copy (file source-file) (file request-persistent-flush-file))
    (set! Functionality$DummyDataSource/delay 0)
    (run-for-cache "round-persistent" (prepare-ns 'generated.round-persistent-experiment #'round-persistent-cache-ohua))
    (run-for-cache "request-persistent" (prepare-ns 'generated.request-persistent-experiment #'request-persistent-cache-ohua))
    (run-for-cache "round-persistent-flush" (prepare-ns 'generated.round-persistent-flush-experiment #'round-persistent-flush-cache-ohua))
    (run-for-cache "request-persistent-flush" (prepare-ns 'generated.request-persistent-flush-experiment #'request-persistent-flush-cache-ohua))))
