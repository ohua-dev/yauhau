;
; Copyright (c) Justus Adam, Sebastian Ertel, Andres Goens 2016. All Rights Reserved.
;
; This source code is licensed under the terms described in the associated LICENSE file.
;

(ns yauhau.muse-experiment
  (:require
    [clojure.test :refer (deftest is)]
    [promesa.core :as prom]
    [muse.core :as muse :refer :all]
    [cats.core :as m]
    [clojure.java.io :refer [file make-parents delete-file]]
    [yauhau.util.program :refer [generate-graphs run-tests to-json run-experiment run-one-test]]))

(def IO_FETCHED_COUNTER (atom 0))
(def IO_BATCHED_COUNTER (atom 0))

(defrecord GetData [id]
  DataSource
  (fetch [_]
    (prom/promise
      (fn fetch-fn [resolve reject]
        ;(println "fetching ...")
        ; (swap! IO_FETCHED_COUNTER inc)
        (swap! IO_BATCHED_COUNTER inc)
        (resolve "foo"))
      )
    )

  BatchedSource
  (fetch-multi [this requests]
    (prom/promise
      (fn fetch-multi-fn [resolve reject]
        ;(println "batched fetching ..." requests (map (fn [_] "foo") requests))
        ;(resolve (into [] (map (fn [_] "foo") requests)))
        ; (swap! IO_FETCHED_COUNTER #(+ % (count requests)))
        (swap! IO_BATCHED_COUNTER inc)
        (resolve "one")
        ))
    )

  MuseAST
  (done? [_] true)
  )

(defn get-data [& args]
  ;(println "get-data called:" args)
  (swap! IO_FETCHED_COUNTER inc)
  (GetData. args))

(defn mcompute [& args]
  ; do nothing and return a dummy
  ;(println "computing ..." args)
  (m/return "foo"))

(defn compute [& args]
  ; do nothing and return a dummy
  ;(println "computing ..." args)
  "foo")

(defn prepare-muse-ns [name compute-version]
  (let [ns (create-ns name)
        interned-get-data (intern name (with-meta 'get-data {}) @#'get-data)
        interned-compute (intern name (with-meta 'compute {}) @compute-version)
        curr-ns (str *ns*)
        _ (in-ns name)
        ;_ (println "level-test?" *ns*)
        ; make sure the linker context knows the namespace of the stateful functions
        _ (require '[clojure.core :refer :all])

        _ (require '[muse.core :refer [run!! <$> value]])
        _ (require '[cats.core :refer [mlet return]])

        ; finally force to load the function in the file to the namespace
        _ (require name)
        _ (in-ns (symbol curr-ns))
        ;_ (println "test-program?" *ns*)

        fns (ns-publics name)]
    (filter (fn [[f-name _]] (re-find #"run\_test\_level\d+(\_\d+)?" (str f-name))) fns)))


(defn monad-runner [namespace]
  (run-tests (prepare-muse-ns namespace #'mcompute)
                           #(swap! IO_FETCHED_COUNTER (fn [_] 0))
                           #(swap! IO_BATCHED_COUNTER (fn [_] 0))
                           (fn [] @IO_FETCHED_COUNTER)
                           (fn [] @IO_BATCHED_COUNTER)))



(defn applicative-runner [namespace]
  (run-tests (prepare-muse-ns namespace #'compute)
                          #(swap! IO_FETCHED_COUNTER (fn [_] 0))
                          #(swap! IO_BATCHED_COUNTER (fn [_] 0))
                          (fn [] @IO_FETCHED_COUNTER)
                          (fn [] @IO_BATCHED_COUNTER)))

(def run-muse-monad-experiment (partial run-experiment "muse" monad-runner))
(def run-muse-app-experiment (partial run-experiment "muse" applicative-runner))

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

(defn with-if []
  (let [base-gen-conf {:%ifs 1
                       :#graphs 7
                       :#lvls 7
                       :seed 123456
                       ;:gen false
                       }]
    (run-muse-monad-experiment "if" "monad" (assoc base-gen-conf :lang "MuseMonad"))
    (run-muse-app-experiment "if" "app" (assoc base-gen-conf :lang "MuseApp"))))

(defn with-func []
  (let [base-gen-conf {:%ifs 0.3
                       :#graphs 7
                       :#lvls 7
                       :seed 123456
                       :%funs 0.3}]
    (run-func-exp "muse" monad-runner "func" "monad" "MuseMonad")
    (run-func-exp "muse" applicative-runner "func" "app" "MuseApp")))

;
;
; (deftest experiment-monad
;   (println "generation result:" (generate-graphs
;     "--percentageifs" "0"
;     "-n" "400"
;     "-o" "com.ohua.fetch/test/clojure/generated/muse_monad.clj"
;     "-L" "MuseMonad"
;     "-l" "20"
;     "-s" "12345"))
;   (let [results (run-tests (prepare-muse-ns 'generated.muse-monad #'mcompute)
;                            #(swap! IO_FETCHED_COUNTER (fn [_] 0))
;                            #(swap! IO_BATCHED_COUNTER (fn [_] 0))
;                            (fn [] @IO_FETCHED_COUNTER)
;                            (fn [] @IO_BATCHED_COUNTER))]
;     (clojure.pprint/print-table results)
;     (spit "test/muse-monad.json" (to-json results))))
;
;
; (deftest experiment-applicative
;   (println "generation result:" (generate-graphs
;     "--percentageifs" "0"
;     "-n" "400"
;     "-o" "com.ohua.fetch/test/clojure/generated/muse_applicative.clj"
;     "-L" "MuseApp"
;     "-l" "20"
;     "-s" "12345"))
;   (let [results (run-tests (prepare-muse-ns 'generated.muse-applicative #'compute)
;                            #(swap! IO_FETCHED_COUNTER (fn [_] 0))
;                            #(swap! IO_BATCHED_COUNTER (fn [_] 0))
;                            (fn [] @IO_FETCHED_COUNTER)
;                            (fn [] @IO_BATCHED_COUNTER))]
;     (clojure.pprint/print-table results)
;     (spit "test/muse-applicative.json" (to-json results))))


;(deftest experiment
;         ;(set! (. com.ohua.engine.utils.GraphVisualizer PRINT_FLOW_GRAPH) (str "test/level-graph-flow"))
;         ;(l/enable-compilation-logging)
;         (let [; create a test namespace here
;               ns (create-ns 'level-test)
;               ;_ (println "created" ns)
;               ; add the above functions there in order to allow them to compile
;               ; http://stackoverflow.com/questions/20831029/how-is-it-possible-to-intern-macros-in-clojure
;               interned-get-data (intern 'level-test '^:macro get-data @#'get-data)
;               interned-write-data (intern 'level-test '^:macro compute @#'compute)
;               interned-ohua-with-conf (intern 'level-test '^:macro compute- @#'compute-)
;
;               _ (in-ns 'level-test)
;               ;_ (println "level-test?" *ns*)
;               ; make sure the linker context knows the namespace of the stateful functions
;               _ (require '[clojure.core :refer :all])
;               _ (require '[com.ohua.compile])
;               _ (com.ohua.link/link ['com.ohua.fetch.functions])
;
;               _ (set! FlowGraphCompiler/SKIP_FUNCTION_SAFETY_ANALYSIS true)
;               _ (set! FlowGraphCompiler/SKIP_DEPENDENCY_ANALYSIS true)
;
;               ; finally force to load the function in the file to the namespace
;               _ (require 'level-test)
;               _ (in-ns 'com.ohua.fetch.test-program)
;               ;_ (println "test-program?" *ns*)
;
;               fns (ns-publics 'level-test)
;               test-fns (filter (fn [[f-name _]] (not (some #{f-name} '(get-data compute write-data ohua)))) fns)
;               ;_ (println test-fns)
;               results (into []
;                             (doall
;                               (map (fn [[f-name f]]
;                                      (println "Starting execution of function: " f-name)
;                                      (let [z (re-matcher #".*\_level([0-9]*).*" (name f-name))
;                                            found (re-find z)
;                                            levels (second found)]
;                                        ;prepare
;                                        (set! Functionality/IO_FETCH_COUNTER 0)
;                                        (set! Functionality/READ_REQUEST_COUNTER 0)
;                                        (set! Functionality/WRITE_REQUEST_COUNTER 0)
;                                        (set! AccumOp/IO_ROUND_COUNTER 0)
;                                        ; execute the function
;                                        (f)
;                                        ; collect the number of I/O calls
;                                        {"fetches_made"     (Functionality/IO_FETCH_COUNTER)
;                                         "rounds_performed" (AccumOp/IO_ROUND_COUNTER)
;                                         "levels"           (Integer. levels)
;                                         "read_requests" (Functionality/READ_REQUEST_COUNTER)
;                                         "write_requests" (Functionality/WRITE_REQUEST_COUNTER)}))
;                                    test-fns)))]
;           (clojure.pprint/print-table results)
;           (spit "test/output.json" (ch/generate-string results))
;           ))
;
