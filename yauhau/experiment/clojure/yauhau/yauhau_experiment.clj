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

(defn trace [thing]
  (println thing)
  thing)

; (enable-compilation-logging)


(defmacro ohua [& args]
  `(o/<-ohua
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

(defn with-if []
  (let [base-gen-conf {:%ifs 1
                       :#graphs 7
                       :#lvls 7
                       :seed 123456}]

    (doall (map (fn [style lang] (run-yauhau-experiment "if" style (assoc base-gen-conf :lang lang))) ["app" "monad"] ["OhuaApp" "Ohua"]))))

(defn with-map []
  (let [gen-conf {:%maps 0.2
                  :#graphs 3
                  :#lvls 7
                  :seed 123456
                  :lang "Ohua"}]
    (run-yauhau-experiment "map" "monad" gen-conf)))

(def counter (atom 0))

(defn run-func-exp [system runner exp-type codestyle lang]
  (let [basefolder (str "yauhau/experiment/clojure/generated/" system "/" exp-type "/" codestyle "/")
        basenamespace (str "generated." system "." exp-type "." codestyle)
        _ (do
            (println "cleaning...")
            (make-parents (str basefolder "abc")))
        results
        (apply concat
          (for [seed [123456 234567]
                percentage [0.1 0.2 0.3 0.4]]
            (do
              (doseq [f (.listFiles (file basefolder))]
                (delete-file f))
              (println "generating" seed percentage)
              (generate-graphs
                "-n" "1"
                "-l" "20"
                "-L" (str lang)
                "--percentagefuns" (str percentage)
                "-s" (str seed)
                "-o" (str basefolder))
              (println "finished generating")
              (doseq [f (.listFiles (file basefolder))]
                (println (str f)))
              (println "Starting experiments")
              (doall
                (mapcat
                  (fn [f]
                    (if-let [m (re-find #"^(.+)\.clj$" (.getName f))]
                      (let [c (swap! counter inc)
                            n (str (second m) c)
                            f2 (file (str basefolder "/" n ".clj") )]
                        (println f2)
                        (.renameTo f f2)
                        (doall
                          (map (fn [res] (assoc res "if_percentage" percentage)) (run-one-test runner basenamespace n))))))
                  (seq (.listFiles (file basefolder))))))))]
    (clojure.pprint/print-table results)
    (spit (str "test/" system "-" exp-type "-" codestyle ".json") (to-json results))))


(defn with-func []
  (let [base-gen-conf {:%ifs 0.3
                       :#graphs 7
                       :#lvls 7
                       :seed 123456
                       :%funs 0.3}]

    (doall (map (fn [style lang] (run-func-exp "yauhau" runner "func" style lang)) ["monad"] ["Ohua"]))))
