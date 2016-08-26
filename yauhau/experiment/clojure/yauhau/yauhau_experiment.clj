;
; Copyright (c) Justus Adam, Sebastian Ertel, Andres Goens 2016. All Rights Reserved.
;
; This source code is licensed under the terms described in the associated LICENSE file.
;

(ns yauhau.yauhau-experiment
  (:require [com.ohua.lang :as o]
            [com.ohua.link]
            [com.ohua.logging :refer [enable-compilation-logging]]
            [com.ohua.util.gen :refer [def-sfn def-sfn-dt]]
            [com.ohua.compile :refer [enable-ir-graphs]]
            [yauhau.ir-transform :as trans]
            [yauhau.cache-transform :as cache]
            [yauhau.concurrent-io-transform :as conc-io]
            [clojure.test :refer [deftest]]
            [clojure.repl :refer [source]]
            [com.ohua.logging :as l]
            [clojure.java.io :refer [file make-parents delete-file]]
            [clojure.string :as string])
  (:use yauhau.util.program)
  (:import (yauhau.functions Functionality AccumOp)))
(set! (. com.ohua.engine.utils.GraphVisualizer PRINT_FLOW_GRAPH) (str "test/level-graph-flow"))
; (enable-ir-graphs)

(defn trace [thing]
  (println thing)
  thing)

; (enable-compilation-logging)


(defmacro ohua [& args]
  `(o/ohua
     ~@args
     :compile-with-config {:df-transformations yauhau.ir-transform/transformations}))

(defmacro ohua-conc [& args]
  `(o/<-ohua
     ~@args
     :compile-with-config {:df-transformations yauhau.ir-transform/transformations}
     :run-with-config (doto (new com.ohua.engine.RuntimeProcessConfiguration)
                        (.setProperties (doto (new java.util.Properties)
                                          (.setProperty "execution-mode"
                                                        (.name (com.ohua.engine.RuntimeProcessConfiguration$Parallelism/MULTI_THREADED)))
                                          (.setProperty "core-thread-pool-size" "5")
                                          (.setProperty "max-thread-pool-size" "5") 
                                          )))
     ))



 (defn runner [namespace]
   (run-tests (prepare-ns namespace #'ohua)
              #(set! Functionality/IO_FETCH_COUNTER 0)
              #(set! AccumOp/IO_ROUND_COUNTER 0)
              #(Functionality/IO_FETCH_COUNTER)
              #(AccumOp/IO_ROUND_COUNTER)))


(def run-yauhau-experiment (partial run-experiment "yauhau" runner))

(def counter (atom 0))

(defn run-prct-exp [system runner exp-type codestyle confs]
  (let [basefolder (str "yauhau/experiment/clojure/generated/" system "/" (string/replace exp-type "-" "_") "/" codestyle "/")
        basenamespace (str "generated." system "." exp-type "." codestyle)
        _ (do
            (make-parents (str basefolder "abc")))
        results
        (mapcat
          (fn [gen-conf]
            (doseq [f (.listFiles (file basefolder))]
              (delete-file f))
            (generate-from-conf basefolder gen-conf)
            (doall
              (mapcat
                (fn [f]
                  (if-let [m (re-find #"^(.+)\.clj$" (.getName f))]
                    (let [c (swap! counter inc)
                          n (str (second m) c)
                          f2 (file (str basefolder "/" n ".clj") )]
                      (.renameTo f f2)
                      (doall
                        (map (fn [res] (assoc res :gen_conf gen-conf)) (run-one-test runner basenamespace n))))))
                (seq (.listFiles (file basefolder))))))
          confs)]
    (clojure.pprint/print-table results)
    (spit (str "test/" system "-" exp-type "-" codestyle ".json") (to-json results))))

(set! yauhau.functions.Functionality$DummyDataSource/delay 0)

(def default-gen-conf {:lang    "Ohua"
                       :#graphs 1
                       :#levels 10})

(def if-confs (for [seed [123456 234567 111111]
                    percentage [0.7 0.8 0.9 1]
                    inline [false true]]
              (merge default-gen-conf {:#levels 10 :%ifs percentage :seed seed :inline_if inline})))

(defn with-func []
  (let [confs (for [seed [123456 234567]
                    percentage [0.1 0.2 0.3 0.4]]
                (merge default-gen-conf {:%funs percentage :seed seed}))]
  (run-prct-exp "yauhau" runner "func" "monad" confs)))

(defn with-if []
  ;(reset! CATCH_EXCEPTIONS true)
  (run-prct-exp "yauhau" runner "if" "monad" if-confs))

(defn with-if-delayed []
  (run-prct-exp "yauhau" runner "if-delayed" "monad" (map #(assoc % :+slow true) if-confs)))

(defn with-map []
  (let [confs (for [seed [123456 234567]
                    percentage [0.1, 0.15, 0.2, 0.25, 0.275, 0.3, 0.325, 0.375, 0.35]]
                (merge default-gen-conf {:%maps percentage :seed seed}))]
    (run-prct-exp "yauhau" runner "map" "monad" confs)))

(defn exec-all []
  (with-map)
  (with-if)
  (with-if-delayed)
  (with-func))

(defn temp []
  (run-prct-exp "yauhau" runner "temp" "monad" [(merge default-gen-conf {:#levels 5 :%maps 0.5 :seed 123456})]))
