;
; Copyright (c) Justus Adam, Sebastian Ertel, Andres Goens 2016. All Rights Reserved.
;
; This source code is licensed under the terms described in the associated LICENSE file.
;

(ns yauhau.test-program
  (:require [com.ohua.compile :as o]
            [com.ohua.link]
            [com.ohua.logging :refer [enable-compilation-logging]]
            [com.ohua.util.gen :refer [def-sfn def-sfn-dt]]
            [yauhau.ir-transform :as trans]
            [yauhau.cache-transform :as cache]
            [yauhau.concurrent-io-transform :as conc-io]
            [clojure.test :refer [deftest]]
            [clojure.repl :refer [source]]
            [com.ohua.logging :as l])
  (:use com.ohua.fetch.util.program)
  (:import (com.ohua.fetch.operators Functionality AccumOp)))

;(defprotocol IGenericPayload
;  (equals [other-payload]))
;
;(deftype GenericPayload [#^"[Ljava.lang.Object;" args] IGenericPayload
;  (equals [this other-payload]
;    ; my generic request payload uses Java's equals method for comparison
;    (Arrays/deepEquals (.args this) (.args other-payload))))
;
;(def my-kv-map {"foo" "bar"})
;(def test-ds (reify IDataSource
;               (getIdentifier [this] "my-kv-map")
;               (fetch [this requests] (map
;                                        (fn [[key time-out & deps]]
;                                          (Thread/sleep time-out)
;                                          (get my-kv-map key))
;                                        requests))
;
;               (store [this requests] ((map
;                                         (fn [[key value time-out & deps]]
;                                           (Thread/sleep time-out)
;                                           (let [old-val (get my-kv-map key)]
;                                             (set! my-kv-map (assoc my-kv-map key value))
;                                             old-val))
;                                         requests)))))
;(def-sfn fetch [^Request request]
;         (first (.fetch (.getDataSource request) (list request))))
;
;(def-sfn store [^Request request]
;         (first (.store (.getDataSource request) (list request))))
;
;(def-sfn
;  req-generic ^Request [#^"[Ljava.lang.Object;" args]
;  (.Request (GenericPayload. args) test-ds))
;
;(def-sfn
;  compute ^Object [#^"[Ljava.lang.Object;" deps ^long time-out]
;  (Thread/sleep time-out)
;  "some-dummy-string")

;(ohua :import [com.ohua.fetch.test-program])


; switch the test namespace such that the below functions are registered there
;(in-ns 'com.ohua.fetch.level-test)

; switch back to the current namespace for executing the test
;(in-ns 'com.ohua.fetch.test-program)



(defmacro ohua [& args]
  `(o/ohua
     ~@args
     :compile-with-config {:df-transformations [
                                                ; batch rewrite
                                                ~@com.ohua.fetch.ir-transform/transformations

                                                ; concurrent batch I/O rewrite
                                                ; conc-io/rewrite

                                                ; cache rewrite
                                                ;(partial cache/cache-rewrite "com.ohua.fetch.operators/round-persistent-cache-manager")
                                                ]}))

(defmacro ohua-conc [& args]
  `(o/ohua
     ~@args
     :compile-with-config {:df-transformations [
                                                ; batch rewrite
                                                ~@com.ohua.fetch.ir-transform/transformations

                                                ; concurrent batch I/O rewrite
                                                ; conc-io/rewrite

                                                ; cache rewrite
                                                ;(partial cache/cache-rewrite "com.ohua.fetch.operators/round-persistent-cache-manager")
                                                ]}
     :run-with-config (doto (new com.ohua.engine.RuntimeProcessConfiguration)
                        (.setProperties (doto (new java.util.Properties)
                                          (.setProperty "execution-mode"
                                                        (.name (com.ohua.engine.RuntimeProcessConfiguration$Parallelism/MULTI_THREADED)))
                                          (.setProperty "core-thread-pool-size" "5")
                                          (.setProperty "max-thread-pool-size" "5")  
                                          )))
     ))


(deftest experiment-monad
  (generate-graphs
    "--percentageifs" "0"
    "-n" "400"
    "-o" "com.ohua.fetch/test/clojure/generated/yauhau_monad.clj"
    "-L" "Ohua"
    "-l" "20"
    "-s" "12345")
  (let [results (run-tests (prepare-ns 'generated.yauhau-monad #'ohua)
                           #(set! Functionality/IO_FETCH_COUNTER 0)
                           #(set! AccumOp/IO_ROUND_COUNTER 0)
                           #(Functionality/IO_FETCH_COUNTER)
                           #(AccumOp/IO_ROUND_COUNTER))]
    (clojure.pprint/print-table results)
    (spit "test/yauhau-monad.json" (to-json results))))


(deftest experiment-applicative
  (generate-graphs
    "--percentageifs" "0"
    "-n" "400"
    "-o" "com.ohua.fetch/test/clojure/generated/yauhau_applicative.clj"
    "-L" "Ohua"
    "-l" "20"
    "-s" "12345")
  (let [results (run-tests (prepare-ns 'generated.yauhau-applicative #'ohua)
                           #(set! Functionality/IO_FETCH_COUNTER 0)
                           #(set! AccumOp/IO_ROUND_COUNTER 0)
                           #(Functionality/IO_FETCH_COUNTER)
                           #(AccumOp/IO_ROUND_COUNTER))]
    (clojure.pprint/print-table results)
    (spit "test/yauhau-applicative.json" (to-json results))))

;(deftest experiment-if
;  (generate-graphs
;    "--percentageifs" "1"
;    "-n" "1"
;    "-o" "com.ohua.fetch/test/clojure/generated/yauhau_if.clj"
;    "-L" "Ohua"
;    "-l" "7")
;  (let [results (run-tests (prepare-ns 'generated.yauhau-if #'ohua-conc)
;                           #(set! Functionality/IO_FETCH_COUNTER 0)
;                           #(set! AccumOp/IO_ROUND_COUNTER 0)
;                           #(Functionality/IO_FETCH_COUNTER)
;                           #(AccumOp/IO_ROUND_COUNTER))]
;    (clojure.pprint/print-table results)
;    (spit "test/yauhau-if.json" (to-json results))))

  ;(set! (. com.ohua.engine.utils.GraphVisualizer PRINT_FLOW_GRAPH) (str "test/level-graph-flow"))
  ;(l/enable-compilation-logging)
;(let [; create a test namespace here
;      ns (create-ns 'level-test)
;      ;_ (println "created" ns)
;      ; add the above functions there in order to allow them to compile
;      ; http://stackoverflow.com/questions/20831029/how-is-it-possible-to-intern-macros-in-clojure
;      interned-get-data (intern 'level-test '^:macro get-data @#'get-data)
;      interned-compute (intern 'level-test '^:macro compute @#'compute)
;      interned-write-data (intern 'level-test '^:macro write-data @#'write-data)
;      interned-ohua-with-conf (intern 'level-test '^:macro ohua @#'ohua)
;
;      _ (in-ns 'level-test)
;      ;_ (println "level-test?" *ns*)
;      ; make sure the linker context knows the namespace of the stateful functions
;      _ (require '[clojure.core :refer :all])
;      _ (require '[com.ohua.compile])
;      _ (com.ohua.link/link ['com.ohua.fetch.operators])
;
;      _ (set! FlowGraphCompiler/SKIP_FUNCTION_SAFETY_ANALYSIS true)
;      _ (set! FlowGraphCompiler/SKIP_DEPENDENCY_ANALYSIS true)
;
;      ; finally force to load the function in the file to the namespace
;      _ (require 'level-test)
;      _ (in-ns 'com.ohua.fetch.test-program)
;      ;_ (println "test-program?" *ns*)
;
;      fns (ns-publics 'level-test)
;      test-fns (filter (fn [[f-name _]] (not (some #{f-name} '(get-data compute write-data ohua)))) fns)
;      ;_ (println test-fns)
;      results (into []
;                    (doall
;                      (map (fn [[f-name f]]
;                             (println "Starting execution of function: " f-name)
;                             (let [z (re-matcher #".*\_level([0-9]*).*" (name f-name))
;                                   found (re-find z)
;                                   levels (second found)]
;                               ;prepare
;                               (set! Functionality/IO_FETCH_COUNTER 0)
;                               (set! Functionality/READ_REQUEST_COUNTER 0)
;                               (set! Functionality/WRITE_REQUEST_COUNTER 0)
;                               (set! AccumOp/IO_ROUND_COUNTER 0)
;                               ; execute the function
;                               (f)
;                               ; collect the number of I/O calls
;                               {"fetches_made"     (Functionality/IO_FETCH_COUNTER)
;                                "rounds_performed" (AccumOp/IO_ROUND_COUNTER)
;                                "levels"           (Integer. levels)
;                                "read_requests" (Functionality/READ_REQUEST_COUNTER)
;                                "write_requests" (Functionality/WRITE_REQUEST_COUNTER)}))
;                           test-fns)))]
;  (clojure.pprint/print-table results)
;  (spit "test/output.json" results)
;  ))

;(o/ohua :import [com.ohua.fetch.operators])
;
;(deftest experiment2
;  (l/enable-compilation-logging)
;  (set! (. com.ohua.engine.utils.GraphVisualizer PRINT_FLOW_GRAPH) (str "test/level-graph-flow"))
;  (let [result (o/ohua
;                 (let [local-4 (get-data "foo" 100)
;                       local-5 (compute 100)
;                       local-6 (get-data "foo" 100)
;                       local-7 (compute 100)]
;                   (let [local-3 (compute local-4 local-5 local-7 100)]
;                     (let [local-10 (write-data "foo" 100)
;                           local-1 (write-data local-3 "foo" 100)
;                           local-2 (get-data local-3 "foo" 100)]
;                       (get-data local-1 local-2 local-6 local-10 "foo" 100))))
;                 :compile-with-config
;                 {:df-transformations [
;                                       ; batch rewrite
;                                       trans/smap-rewrite
;                                       trans/cat-redundant-smap-collects
;                                       trans/if-rewrite
;                                       trans/cat-redundant-merges
;                                       trans/cat-redundant-identities
;                                       trans/batch-rewrite
;
;                                       ; concurrent batch I/O rewrite
;                                       ; conc-io/rewrite
;
;                                       ; cache rewrite
;                                       ;(partial cache/cache-rewrite "com.ohua.fetch.operators/round-persistent-cache-manager")
;                                       ]
;                  }
;                 )
;        ]
;    (println "result:" result)
;    ))
