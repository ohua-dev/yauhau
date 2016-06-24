;
; Copyright (c) Justus Adam, Sebastian Ertel, Andres Goens 2016. All Rights Reserved.
;
; This source code is licensed under the terms described in the associated LICENSE file.
;
(ns yauhau.modularity
  (:require [com.ohua.lang :as o]
            [com.ohua.link]
            [com.ohua.logging :refer [enable-compilation-logging]]
            [com.ohua.util.gen :refer [def-sfn def-sfn-dt]]
            [clojure.test :refer [deftest]]
            [clojure.repl :refer [source]]
            [com.ohua.logging :as l])
  (:use yauhau.util.program)
  (:import (yauhau.functions Functionality AccumOp)))


(deftest experiment-horizontal
         (generate-graphs
           "--percentageifs" "0"
           "-n" "400"
           "-o" "com.ohua.fetch/test/clojure/generated/yauhau_madularity.clj"
           "-L" "Ohua"
           "-l" "20"
           "-s" "12345"
           ; TODO add configuration params once rand-code-graph extension completed
           )
         (let [results (run-tests (prepare-ns 'generated.yauhau-monad #'ohua)
                                  #(set! Functionality/IO_FETCH_COUNTER 0)
                                  #(set! AccumOp/IO_ROUND_COUNTER 0)
                                  #(Functionality/IO_FETCH_COUNTER)
                                  #(AccumOp/IO_ROUND_COUNTER))]
           (clojure.pprint/print-table results)
           (spit "test/yauhau-modularity.json" (to-json results))))
