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
            [clojure.set :as setlib]))



(def ir1 [(ir/mk-func "size" ['coll1] 'size1)
          (ir/mk-func "one-to-n" ['size1 'coll1] 'a)
          (ir/mk-func "fetch" ['a] 'b)
          (ir/mk-func "collect" ['size1 'b] 'c)])


(def ir2 [(ir/mk-func "fetch" ['a] ['b])])
(def ir2-transformed ir2)


(def ir3 [(ir/mk-func "size" ['coll1] 'size1)
          (ir/mk-func "one-to-n" ['size1 'coll1] 'packaged1)
          (ir/mk-func "smap" ['packaged1] 'data1)
          (ir/mk-func "size" ['data1] 'size2)
          (ir/mk-func "one-to-n" ['size2 'data1] 'packaged2)
          (ir/mk-func "smap" ['packaged2] 'data2)
          (ir/mk-func "fetch" ['data2] 'intermediate1)
          (ir/mk-func "collect" ['size2 'intermediate1] 'intermediate2)
          (ir/mk-func "collect" ['size1 'intermediate2] 'result)])


(def ir4 [(ir/mk-func "size" ['coll1] 'size1)
          (ir/mk-func "one-to-n" ['size1 'coll1] 'a)
          (ir/mk-func "fetch" ['a] 'b)
          (ir/mk-func "collect" ['size1 'b] 'c)
          (ir/mk-func "size" ['coll2] 'size2)
          (ir/mk-func "one-to-n" ['size2 'coll2] 'h)
          (ir/mk-func "fetch" ['h] 'i)
          (ir/mk-func "collect" ['size2 'i] 'j)])


(defn expect-escape [levels fetch-in fetch-out startindex transformed]
  (let [[packager & graph] (drop startindex transformed)
        input (do
                (is (= (:name packager) "__packageArgs"))
                (is (= (:args packager) fetch-in))
                (:return packager))
        [graph output _]
        (reduce
          (fn [[graph input] size]
            (let [[collector tree-builder & rest] graph]
              (is (= (:name collector) "collect"))
              (is (= (:args collector) [size input]))
              (is (= (:name tree-builder) "__mkReqBranch"))
              (is (= (:args tree-builder) [(:return collector)]))
              [rest (:return tree-builder)]))
          [graph input]
          (rseq levels))
        [fetch-fun & graph] graph
        _ (is (= (:args fetch-fun) output))
        _ (is (= (:name fetch-fun) "fetch"))
        output (:return fetch-fun)
        [_ output]
        (reduce
          (fn [[graph input] size]
            (let [[one-to-n smap & rest] graph]
              (is (= (:name one-to-n) "one-to-n"))
              (is (= (:name smap) "smap"))
              (is (= (:args one-to-n) [size input]))
              (is (= (:args smap) [(:return one-to-n)]))
              [rest (:return smap)]))
          [graph output]
          levels)]
    (is (= fetch-out output))))


(deftest test-smap-trans-simple-manual
  (let [transformed (fetch-trans/smap-rewrite ir1)
        size 'size1
        fetchf (first (filter #(= "fetch" (:name %)) transformed))
        fetchindex (.indexOf transformed fetchf)
        packager (nth transformed (- fetchindex 3))
        collector (nth transformed (- fetchindex 2))
        one-to-n (nth transformed (inc fetchindex))
        smap (nth transformed (+ fetchindex 2))]
    (is (= "collect" (:name collector)))
    (is (= "__packageArgs" (:name packager)))
    (is (= size (first (:args one-to-n))))
    (is (= size (first (:args collector))))
    (is (= "one-to-n" (:name one-to-n)))
    (is (= "smap" (:name smap)))
    (is (= (ir/get-return-vars one-to-n) (:args smap))))
  (is
    (=
      (into [] (fetch-trans/smap-rewrite ir2))
      ir2-transformed)))


(deftest test-smap-trans-simple-automated
  (let [transformed (fetch-trans/smap-rewrite ir1)]
    (expect-escape
      ['size1]
      ['a] 'b
      2
      transformed)))


(deftest test-smap-trans-nested
  (let [startindex 6
        transformed (fetch-trans/smap-rewrite ir3)]
    (is (= (:name (first (drop startindex ir3))) "fetch"))
    (is (setlib/subset? (set (remove #(= "fetch" (:name %)) ir3)) (set transformed))
        (expect-escape
          ['size1 'size2]
          ['data2] 'intermediate1
          startindex
          transformed))))


(deftest test-smap-trans-sequential
  (let [startindex1 2
        startindex2 11
        transformed (fetch-trans/smap-rewrite ir4)
        _ (pprint transformed)]
    (expect-escape ['size1] ['a] 'b startindex1 transformed)
    (expect-escape ['size2] ['h] 'i startindex2 transformed)))
