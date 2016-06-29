;
; Copyright (c) Justus Adam, Sebastian Ertel, Andres Goens 2016. All Rights Reserved.
;
; This source code is licensed under the terms described in the associated LICENSE file.
;

(ns yauhau.visual-if-rewrite
  (:require [yauhau.ir-transform :as trans]
            [com.ohua.ir :as ir]
            [clojure.test :as test :refer [deftest is]]
            [clojure.pprint :refer [pprint]]
            [clojure.set :as set]
            [com.ohua.context :as ctxlib]
            [monads.core :refer [return]])
  (:use com.ohua.util.visual)
  (:import [com.ohua.ir IRFunc]))


(defn mk-func-w-name [name] (fn [id args return] (ir/->IRFunc id name args return)))
(def mk-if (mk-func-w-name 'com.ohua.lang/ifThenElse))
(def mk-select (mk-func-w-name 'com.ohua.lang/select))
(def mk-fetch (mk-func-w-name 'yauhau.functions/fetch))
(def mk-request (mk-func-w-name 'request))


(def empty-ctx [])
(def ir1 [(mk-if 0 [] ['a 'b])
          (mk-fetch 1 ['b] 'c)
          (mk-fetch 2 ['a] 'd)
          (mk-select 3 ['c 'd] 'e)])

(def ir1-ctx
  {0 empty-ctx
   1 [(ctxlib/->IFStackEntry 0 'b)]
   2 [(ctxlib/->IFStackEntry 0 'a)]
   3 empty-ctx})

(def ir2 [(mk-if 0 [] ['a 'b])
          (mk-fetch 1 ['b] 'c)
          (mk-select 2 ['c 'a] 'e)])

(def ir2-ctx {0 empty-ctx
              1 [(ctxlib/->IFStackEntry 0 'b)]
              2 empty-ctx})

(def ir3 [(mk-if 0 [] ['a 'b])
          (mk-request 1 ['b] 'i)
          (mk-fetch 2 ['i] 'c)
          (mk-if 3 ['a] ['f 'g])
          (mk-request 4 ['f] 'j)
          (mk-fetch 5 ['j] 'h)
          (mk-select 6 ['h 'g] 'd)
          (mk-select 7 ['c 'd] 'e)])


(def ir3-ctx
  (let [ctx-1b [(ctxlib/->IFStackEntry 0 'b)]
        ctx-1a [(ctxlib/->IFStackEntry 0 'a)]
        ctx-2f (conj ctx-1a (ctxlib/->IFStackEntry 3 'f))]
    {0 empty-ctx
     1 ctx-1b
     2 ctx-1b
     3 ctx-1a
     4 ctx-2f
     5 ctx-2f
     6 ctx-1a
     7 empty-ctx}))


(def ir4 [(mk-if 0 [] ['a 'b])
          (mk-request 1 ['b] 'j)
          (mk-fetch 2 ['j] 'c)
          (mk-request 3 ['c] 'k)
          (mk-fetch 4 ['k] 'i)
          (mk-if 6 ['a] ['f 'g])
          (mk-request 7 ['f] 'l)
          (mk-fetch 8 ['l] 'h)
          (mk-select 9 ['h 'g] 'd)
          (mk-select 10 ['i 'd] 'e)
          (ir/mk-func 11 'consumer ['e] 'p)])


(def ir4-ctx
  (let [ctx-1b [(ctxlib/->IFStackEntry 0 'b)]
        ctx-1a [(ctxlib/->IFStackEntry 0 'a)]
        ctx-2f (conj ctx-1a (ctxlib/->IFStackEntry 6 'f))]
    {0 empty-ctx
     1 ctx-1b
     2 ctx-1b
     3 ctx-1b
     4 ctx-1b
     6 ctx-1a
     7 ctx-2f
     8 ctx-2f
     9 ctx-1a
     10 empty-ctx
     11 empty-ctx}))


(def ir5 [(mk-if 0 [] ['a 'b])
          (mk-request 1 ['b] 'f)
          (mk-request 2 ['c] 'g)
          (trans/mk-fetch ['f] 'c)
          (trans/mk-fetch ['g] 'd)
          (trans/mk-select ['d 'a] 'e)])


(def to-print {;1 [ir1 ir1-ctx]
               ;2 [ir2 ir2-ctx]
               ;3 [ir3 ir3-ctx]
               4 [ir4 ir4-ctx]
               ;5 ir5
               })


(defn if-only-rewrite [graph context-map]
  (first (trans/context-rewrite-with {'if (trans/->Rewrite trans/checked-if-rewrite-one return)} graph context-map)))


(defn -main []
  (doall
    (map
      (fn [[irnum [ir ctx]]]
        (let [transformed (if-only-rewrite ir ctx)
              _ (print-graph transformed)
              ;cat-merges (trans/cat-redundant-merges transformed)
              ;cat-ids (trans/cat-redundant-identities cat-merges)
              ;coerce-merges (trans/coerce-merges cat-ids)
              ]
          (render-to-file (str "original-" irnum) ir)
          (render-to-file (str "ir-rewrite-" irnum) transformed)
          ;(render-to-file (str "cat-merges-" irnum) cat-merges)
          ;(render-to-file (str "cat-ids-" irnum) cat-ids)
          ;(render-to-file (str "coerce-merges-" irnum) coerce-merges)
          ))
      (seq to-print))))
