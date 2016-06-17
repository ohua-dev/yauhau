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

(defn mk-func-w-name [name] (fn [id args return] (ir/->IRFunc id name args return)))
(def mk-size-fn (mk-func-w-name 'com.ohua.lang/size))
(def mk-one-to-n (mk-func-w-name 'com.ohua.lang/one-to-n))
(def mk-fetch (mk-func-w-name 'yauhau.functions/fetch))
(def mk-collect (mk-func-w-name 'com.ohua.lang/collect))
(def mk-smap (mk-func-w-name 'com.ohua.lang/smap-fun))
(def mk-packager (mk-func-w-name 'yauhau.functions/__packageArgs))

; (smap (algo [a] (fetch a)))
(def ir1 [(mk-size-fn 0 ['coll1] 'size1)
          (mk-one-to-n 1 ['size1 'coll1] 'smap-in)
          (mk-smap 4 ['smap-in] 'a)
          (mk-fetch 2 ['a] 'b)
          (mk-collect 3 ['size1 'b] 'c)])


(def ir1-ctx-map {0 []
                  1 []
                  4 []
                  2 [(ctxlib/->SmapStackEntry 'size1)]
                  3 []})


(def ir2 [(mk-fetch 0 ['a] ['b])])
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


(def is-packager? (partial ir/fn-name-is 'yauhau.functions/__packageArgs))
(def is-collect? (partial ir/fn-name-is 'com.ohua.lang/collect))
(def is-tree-builder? (partial ir/fn-name-is 'yauhau.functions/__mkReqTreeBranch))
(def is-fetch? (partial ir/fn-name-is 'yauhau.functions/fetch))
(def is-smap? (partial ir/fn-name-is 'com.ohua.lang/smap-fun))
(def is-one-to-n? (partial ir/fn-name-is 'com.ohua.lang/one-to-n))


(defn expect-escape [graph depth in]
  (is (< 0 depth))
  (let [[packager
         one-to-n
         collect
         tree-builder
         & rest] graph]
    (is (is-packager? packager))
    (is (= in (.-args packager)))
    (is (is-one-to-n? one-to-n))
    (is (is-collect? collect))
    (is (= (.-return packager) (second (.-args collect))))
    (is (is-tree-builder? tree-builder))
    (is (= [(.-return collect)] (.-args tree-builder)))
    (let [[out more-rest] (if (is-fetch? (first rest))
                            (do
                              (is (= 1 depth))
                              [(.-return (first rest)) (clojure.core/rest rest)])
                            (expect-escape rest (dec depth) [(.-return tree-builder)]))
          [one-to-n smap & last-rest] more-rest]
      (is (is-one-to-n? one-to-n))
      (is (= (second (.-args one-to-n)) out))
      (is (is-smap? smap))
      [(.-return smap) last-rest])))


(defn smap-only-rewrite [graph context-map]
  (first (fetch-trans/context-rewrite-with {ctxlib/smap-ctx (fetch-trans/->Rewrite fetch-trans/wrap-smap-once return)} graph context-map)))


(deftest test-smap-trans-simple-manual
  (let [transformed (smap-only-rewrite ir1 ir1-ctx-map)
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
  (let [transformed (smap-only-rewrite ir1 ir1-ctx-map)]
    (println transformed)
    (expect-escape (drop 3 transformed) 1 ['a])))


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
