;
; Copyright (c) Justus Adam, Sebastian Ertel, Andres Goens 2016. All Rights Reserved.
;
; This source code is licensed under the terms described in the associated LICENSE file.
;

(ns com.ohua.fetch.test-transform
  (:require [clojure.test :as test :refer [deftest]]
            [com.ohua.ir :as ir]
            [yauhau.ir-transform :as trans]
            [yauhau.accumulator :as acc]))


(def f1 (ir/->IRFunc nil "fetch" [] ["g"]))
(def f2 (ir/->IRFunc nil "p" [] ["h"]))


(def ir1
  [f1 f2])

(def f3 (ir/->IRFunc nil "fetch" ["h"] ["i"]))
(def f4 (ir/->IRFunc nil "func56" ["i"] ["l0"]))
(def f5 (ir/->IRFunc nil "func80" ["h"] ["llm"]))
(def f6 (ir/->IRFunc nil "fetch" ["g"] ["hgl"]))

(def ir2
  [f1 f2 f3 f4 f5 f6])


(deftest find-fetches-test
  (test/is
    (=
      (trans/find-next-fetches (ir/first-position ir1))
      [(ir/->IRGraphPosition ir1 #{} #{f1 f2} #{"h"})
       #{f1}]))
  (test/is
    (=
      (trans/find-next-fetches (ir/first-position ir2))
      [(ir/->IRGraphPosition ir2 #{} #{f1 f2 f3 f5} #{"h" "llm"})
       #{f1 f3}])))



(deftest batch-rewrite-test
  (let [trans1 (trans/batch-rewrite ir1)]
    (test/is (some #{(acc/mk-accum-for-fetches [f1])} trans1)))
  (let [trans2 (trans/batch-rewrite ir2)]
    (test/is
      (some
        #{(acc/mk-accum-for-fetches [f1 f3])
          (acc/mk-accum-for-fetches [f6])}
        trans2))))
