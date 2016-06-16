;
; Copyright (c) Justus Adam, Sebastian Ertel, Andres Goens 2016. All Rights Reserved.
;
; This source code is licensed under the terms described in the associated LICENSE file.
;

(ns yauhau.test-smap-transform
  (:require [clojure.test :as test :refer [deftest is]]
            [com.ohua.ir :as ir]
            [yauhau.ir-transform :as fetch-trans]
            [clojure.pprint :refer [pprint]]
            [clojure.set :as setlib]
            [com.ohua.context :as ctxlib]
            [monads.core :refer [return]]))


(def ir1 [(ir/mk-func 'size ['coll1] 'size1)
          (ir/mk-func 'com.ohua.lang/one-to-n ['size1 'coll1] 'a)
          (ir/mk-func 'yauhau.functions/fetch ['a] 'b)
          (ir/mk-func 'com.ohua.lang/collect ['size1 'b] 'c)])


(def ir2 [(ir/mk-func 'yauhau.functions/fetch ['a] ['b])])
(def ir2-transformed ir2)


(def ir3 [(ir/mk-func 'size ['coll1] 'size1)
          (ir/mk-func 'com.ohua.lang/one-to-n ['size1 'coll1] 'packaged1)
          (ir/mk-func 'com.ohua.lang/smap ['packaged1] 'data1)
          (ir/mk-func 'size ['data1] 'size2)
          (ir/mk-func 'com.ohua.lang/one-to-n ['size2 'data1] 'packaged2)
          (ir/mk-func 'com.ohua.lang/smap ['packaged2] 'data2)
          (ir/mk-func 'yauhau.functions/fetch ['data2] 'intermediate1)
          (ir/mk-func 'com.ohua.lang/collect ['size2 'intermediate1] 'intermediate2)
          (ir/mk-func 'com.ohua.lang/collect ['size1 'intermediate2] 'result)])


(def ir4 [(ir/mk-func 'size ['coll1] 'size1)
          (ir/mk-func 'com.ohua.lang/one-to-n ['size1 'coll1] 'a)
          (ir/mk-func 'yauhau.functions/fetch ['a] 'b)
          (ir/mk-func 'com.ohua.lang/collect ['size1 'b] 'c)
          (ir/mk-func 'size ['coll2] 'size2)
          (ir/mk-func 'com.ohua.lang/one-to-n ['size2 'coll2] 'h)
          (ir/mk-func 'yauhau.functions/fetch ['h] 'i)
          (ir/mk-func 'com.ohua.lang/collect ['size2 'i] 'j)])


(defn expect-escape [levels fetch-in fetch-out startindex transformed]
  (let [[packager & graph] (drop startindex transformed)
        input (do
                (is (= (:name packager) '__packageArgs))
                (is (= (:args packager) fetch-in))
                (:return packager))
        [graph output _]
        (reduce
          (fn [[graph input] size]
            (let [[collector tree-builder & rest] graph]
              (is (= (:name collector) 'com.ohua.lang/collect))
              (is (= (:args collector) [size input]))
              (is (= (:name tree-builder) '__mkReqBranch))
              (is (= (:args tree-builder) [(:return collector)]))
              [rest (:return tree-builder)]))
          [graph input]
          (rseq levels))
        [fetch-fun & graph] graph
        _ (is (= (:args fetch-fun) output))
        _ (is (= (:name fetch-fun) 'yauhau.functions/fetch))
        output (:return fetch-fun)
        [_ output]
        (reduce
          (fn [[graph input] size]
            (let [[one-to-n smap & rest] graph]
              (is (= (:name one-to-n) 'com.ohua.lang/one-to-n))
              (is (= (:name smap) 'com.ohua.lang/smap))
              (is (= (:args one-to-n) [size input]))
              (is (= (:args smap) [(:return one-to-n)]))
              [rest (:return smap)]))
          [graph output]
          levels)]
    (is (= fetch-out output))))


(defn smap-only-rewrite [graph]
  (first (fetch-trans/context-rewrite-with {ctxlib/smap-ctx (fetch-trans/->Rewrite fetch-trans/wrap-smap-once return)} graph)))


(deftest test-smap-trans-simple-manual
  (let [transformed (smap-only-rewrite ir1)
        size 'size1
        _ (println transformed)
        fetchf (first (filter fetch-trans/is-fetch? transformed))
        _ (println fetchf)
        fetchindex (.indexOf transformed fetchf)
        packager (nth transformed (- fetchindex 3))
        collector (nth transformed (- fetchindex 2))
        one-to-n (nth transformed (inc fetchindex))
        smap (nth transformed (+ fetchindex 2))]
    (is (= 'com.ohua.lang/collect (:name collector)))
    (is (= 'yauhau.functions/__packageArgs (:name packager)))
    (is (= size (first (:args one-to-n))))
    (is (= size (first (:args collector))))
    (is (= 'yauhau.functions/one-to-n (:name one-to-n)))
    (is (= 'com.ohua.lang/smap (:name smap)))
    (is (= (ir/get-return-vars one-to-n) (:args smap))))
  (is
    (=
      (into [] (smap-only-rewrite ir2))
      ir2-transformed)))


(deftest test-smap-trans-simple-automated
  (let [transformed (smap-only-rewrite ir1)]
    (expect-escape
      ['size1]
      ['a] 'b
      2
      transformed)))


(deftest test-smap-trans-nested
  (let [startindex 6
        transformed (smap-only-rewrite ir3)]
    (is (= (:name (first (drop startindex ir3))) 'yauhau.functions/fetch))
    (is (setlib/subset? (set (remove #(= 'yauhau.functions/fetch (:name %)) ir3)) (set transformed))
        (expect-escape
          ['size1 'size2]
          ['data2] 'intermediate1
          startindex
          transformed))))


(deftest test-smap-trans-sequential
  (let [startindex1 2
        startindex2 11
        transformed (smap-only-rewrite ir4)
        _ (pprint transformed)]
    (expect-escape ['size1] ['a] 'b startindex1 transformed)
    (expect-escape ['size2] ['h] 'i startindex2 transformed)))
