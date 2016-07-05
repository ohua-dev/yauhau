;
; Copyright (c) Justus Adam, Sebastian Ertel, Andres Goens 2016. All Rights Reserved.
;
; This source code is licensed under the terms described in the associated LICENSE file.
;

(ns yauhau.util.program
  (:require [com.ohua.compile :as o]
            [com.ohua.link]
            [com.ohua.logging :refer [enable-compilation-logging]]
            [com.ohua.util.gen :refer [def-sfn def-sfn-dt]]
            [yauhau.ir-transform :as trans]
            [yauhau.cache-transform :as cache]
            [yauhau.concurrent-io-transform :as conc-io]
            [clojure.test :refer [deftest]]
            [clojure.repl :refer [source]]
            [com.ohua.logging :as l]
            [clojure.java.shell :refer [sh]]
            [cheshire.core :as ch])
  (:import
    ;(com.ohua.fetch IDataSource)
    ;       (java.util Arrays)
    (yauhau.functions Functionality AccumOp)
    (com.ohua.lang.compile FlowGraphCompiler)))

(def code-gen-executable "../rand-code-graph/.stack-work/install/x86_64-osx/lts-6.4/7.10.3/bin/random-level-graphs")


(defmacro get-data [& args]
  (if (and (< (count args) 3)
           (not (symbol? (second (reverse args)))))
    `(yauhau.functions/fetch
       (yauhau.functions/read-request ~@args))
    `(yauhau.functions/fetch
       (yauhau.functions/read-request ~@args))
    )
  )

(defmacro write-data [& args]
  (if (and (< (count args) 3)
           (not (symbol? (second (reverse args)))))
    `(yauhau.functions/store
       (yauhau.functions/write-request (into-array Object [~@args])))
    `(yauhau.functions/store
       (yauhau.functions/write-request ~@args))
    )
  )

(defmacro compute [& args]
  (if (< (count args) 2)
    `(yauhau.functions/compute ~@args)
    `(yauhau.functions/compute ~@args)
    )
  )


(defmacro slow-get-data [& args]
  (if (and (< (count args) 3)
           (not (symbol? (second (reverse args)))))
    `(yauhau.functions/fetch
       (yauhau.functions/slow-read-request (into-array Object [~@args])))
    `(yauhau.functions/fetch
       (yauhau.functions/slow-read-request ~@args))
    )
  )


(def level-name-regex #".*\_level([0-9]*).*")


(defn prepare-ns [name compiler]
  (let [ns (create-ns name)
        interned-get-data (intern name (with-meta 'get-data {:macro true}) @#'get-data)
        interned-slow-get-data (intern name (with-meta 'slow-get-data {:macro true}) @#'slow-get-data)
        interned-compute (intern name (with-meta 'compute {:macro true}) @#'compute)
        interned-write-data (intern name (with-meta 'write-data {:macro true}) @#'write-data)
        interned-ohua-with-conf (intern name (with-meta 'ohua {:macro true}) @compiler)
        curr-ns (str *ns*)
        _ (in-ns name)
        ;_ (println "level-test?" *ns*)
        ; make sure the linker context knows the namespace of the stateful functions
        _ (require '[clojure.core :refer :all])
        _ (require '[com.ohua.lang :refer [defalgo]])
        _ (com.ohua.link/link ['yauhau.functions])

        _ (set! FlowGraphCompiler/SKIP_FUNCTION_SAFETY_ANALYSIS true)
        _ (set! FlowGraphCompiler/SKIP_DEPENDENCY_ANALYSIS true)

        ; finally force to load the function in the file to the namespace
        _ (println name)
        _ (require name)
        _ (in-ns (symbol curr-ns))
        ;_ (println "test-program?" *ns*)

        fns (ns-publics name)]
    (filter (fn [[f-name _]] (re-find #"run\_test\_level\d+(\_\d+)?" (str f-name))) fns)))


(def to-json ch/generate-string)


(defn generate-graphs [& args] (apply sh code-gen-executable args))


(defn levels-from-fname [f-name]
  (second (re-find (re-matcher level-name-regex (name f-name)))))


(defmacro measure-exec-time
  [expr]
  `(let [start# (. System (nanoTime))]
     ~expr
     (/ (double (- (. System (nanoTime)) start#)) 1000000.0)))


(defn reset-counters! []
  (set! Functionality/IO_FETCH_COUNTER 0)
  (set! Functionality/READ_REQUEST_COUNTER 0)
  (set! Functionality/WRITE_REQUEST_COUNTER 0)
  (set! AccumOp/IO_ROUND_COUNTER 0))


(defn run-tests [test-fns reset-fetch-count reset-round-count read-fetch-count read-round-count]
  (into []
        (doall
          (map (fn [[f-name f]]
                 (println "Starting execution of function: " f-name)
                 (let [levels (levels-from-fname f-name)]
                   ;prepare
                   (reset-fetch-count )
                   (set! Functionality/READ_REQUEST_COUNTER 0)
                   (set! Functionality/WRITE_REQUEST_COUNTER 0)
                   (reset-round-count)
                   ; execute the function
                   (f)
                   ; collect the number of I/O calls
                   {"fetches_made"     (read-fetch-count)
                    "rounds_performed" (read-round-count)
                    "levels"           (Integer. levels)
                    "read_requests"    (Functionality/READ_REQUEST_COUNTER)
                    "write_requests"   (Functionality/WRITE_REQUEST_COUNTER)}))
               test-fns))))
