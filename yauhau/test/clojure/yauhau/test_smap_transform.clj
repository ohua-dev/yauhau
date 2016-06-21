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
          (mk-one-to-n 5 ['size1 'size1] 'csize)
          (mk-collect 3 ['csize 'b] 'c)])


(def ir1-ctx-map {0 []
                  1 []
                  4 []
                  2 [(ctxlib/->SmapStackEntry 'size1)]
                  3 []
                  5 []})


(def ir2 [(mk-fetch 0 ['a] ['b])])
(def ir2-transformed ir2)


(def ir3 [(mk-size-fn 0 ['coll1] 'size1)
          (mk-one-to-n 1 ['size1 'coll1] 'packaged1)
          (mk-smap 3 ['packaged1] 'data1)
          (mk-size-fn 4 ['data1] 'size2)
          (mk-one-to-n 5 ['size2 'data1] 'packaged2)
          (mk-smap 6 ['packaged2] 'data2)
          (mk-fetch 7 ['data2] 'intermediate1)
          (mk-one-to-n 8 ['size2 'size2] 'csize2)
          (mk-collect 9 ['csize2 'intermediate1] 'intermediate2)
          (mk-one-to-n 10 ['size1 'size1] 'csize1)
          (mk-collect 11 ['csize1 'intermediate2] 'result)])


(def ir3-ctx-map
  (let [empty-ctx []
        ctx-1 [(ctxlib/->SmapStackEntry 'size1)]
        ctx-2 [(ctxlib/->SmapStackEntry 'size1)
               (ctxlib/->SmapStackEntry 'size2)]]
    {0  empty-ctx
     1  empty-ctx
     3  empty-ctx
     4  ctx-1
     5  ctx-1
     6  ctx-1
     7  ctx-2
     8  ctx-1
     9  ctx-2
     10 empty-ctx
     11 ctx-1}))


(def ir4 [(mk-size-fn 0 ['coll1] 'size1)
          (mk-one-to-n 1 ['size1 'coll1] 'a)
          (mk-smap 2 ['a] 'iterating1)
          (mk-fetch 3 ['iterating1] 'b)
          (mk-one-to-n 4 ['size1 'size1] 'csize1)
          (mk-collect 5 ['csize1 'b] 'c)
          (mk-size-fn 6 ['c] 'size2)
          (mk-one-to-n 7 ['size2 'c] 'h)
          (mk-smap 8 ['h] 'iterating2)
          (mk-fetch 9 ['iterating2] 'i)
          (mk-one-to-n 10 ['size2 'size2] 'csize2)
          (mk-collect 11 ['csize2 'i] 'j)])

(def ir4-ctx
  (let [empty-ctx []
        ctx-1 [(ctxlib/->SmapStackEntry 'size1)]
        ctx-2 [(ctxlib/->SmapStackEntry 'size2)]]
    {0  empty-ctx
     1  empty-ctx
     2  empty-ctx
     3  ctx-1
     4  empty-ctx
     5  ctx-1
     6  empty-ctx
     7  empty-ctx
     8  empty-ctx
     9  ctx-2
     10 empty-ctx
     11 ctx-2}))


(defn print-graph [fns]
  (println
    (str
      "(let ["
      (apply str (interpose "\n      "
                            (map
                              (fn [{name :name args :args return :return}]
                                (str
                                  (if (symbol? return) return (into [] return))
                                  " (" name " "
                                  (apply str (interpose " " args)) ")"))
                              fns)))
      "]\n  "
      (:return (last fns))
      ")")))


(def is-collect? (partial ir/fn-name-is 'com.ohua.lang/collect))
(def is-tree-builder? (partial ir/fn-name-is 'yauhau.functions/__mkReqTreeBranch))
(def is-fetch? (partial ir/fn-name-is 'yauhau.functions/fetch))
(def is-smap? (partial ir/fn-name-is 'com.ohua.lang/smap-fun))
(def is-one-to-n? (partial ir/fn-name-is 'com.ohua.lang/one-to-n))


(defn expect-escape [graph sizes in]
  (is (not (or (nil? sizes) (empty? sizes))))
  (let [[one-to-n
         collect
         tree-builder
         & rest] graph
        [mysize & rest-sizes] sizes]
    (is (is-one-to-n? one-to-n))
    (is (= [mysize mysize] (.-args one-to-n)))
    (is (is-collect? collect))
    (is (= in (second (.-args collect))))
    (is (is-tree-builder? tree-builder))
    (is (= [(.-return collect)] (.-args tree-builder)))
    (let [[out more-rest] (if (is-fetch? (first rest))
                            (do
                              (is (nil? rest-sizes) "Expected another smap wrapping layer")
                              [(.-return (first rest)) (clojure.core/rest rest)])
                            (expect-escape rest rest-sizes (.-return tree-builder)))
          [one-to-n smap & last-rest] more-rest]
      (is (is-one-to-n? one-to-n))
      (is (= (second (.-args one-to-n)) out))
      (is (is-smap? smap))
      [(.-return smap) last-rest])))


(defn smap-only-rewrite [graph context-map]
  (first (fetch-trans/context-rewrite-with {ctxlib/smap-ctx (fetch-trans/->Rewrite fetch-trans/wrap-smap-once return)} graph context-map)))


(deftest test-smap-trans-simple-automated
  (let [transformed (smap-only-rewrite ir1 ir1-ctx-map)]
    (pprint transformed)
    (expect-escape (drop 3 transformed) ['size1] 'a)))


(deftest test-smap-trans-nested
  (let [transformed (smap-only-rewrite ir3 ir3-ctx-map)]
    (pprint transformed)
    (expect-escape (drop 6 transformed) ['size2 'size1] 'data2)))


(deftest test-smap-trans-sequential
  (let [transformed (smap-only-rewrite ir4 ir4-ctx)
        _ (print-graph transformed)
        [_ restgraph] (expect-escape (drop 3 transformed) ['size1] 'iterating1)]
    (expect-escape (drop 5 restgraph) ['size2] 'iterating2)))
