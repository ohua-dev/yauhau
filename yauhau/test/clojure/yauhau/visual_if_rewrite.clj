;
; Copyright (c) Justus Adam, Sebastian Ertel, Andres Goens 2016. All Rights Reserved.
;
; This source code is licensed under the terms described in the associated LICENSE file.
;

(ns com.ohua.fetch.visual-if-rewrite
  (:require [com.ohua.fetch.ir-transform :as trans]
            [com.ohua.ir :as ir]
            [clojure.test :as test :refer [deftest is]]
            [clojure.pprint :refer [pprint]]
            [clojure.set :as set])
  (:use com.ohua.util.visual)
  (:import [com.ohua.ir IRFunc]))


(def mk-if-op (trans/mk-func-maker "ifThenElse"))


(def mk-request (trans/mk-func-maker "request"))


(def ir1 [(mk-if-op [] ['a 'b])
          (trans/mk-fetch ['b] 'c)
          (trans/mk-fetch ['a] 'd)
          (trans/mk-merge ['c 'd] 'e)])

(def ir2 [(mk-if-op [] ['a 'b])
          (trans/mk-fetch ['b] 'c)
          (trans/mk-merge ['c 'a] 'e)])

(def ir3 [(mk-if-op [] ['a 'b])
          (mk-request ['b] 'i)
          (trans/mk-fetch ['i] 'c)
          (mk-if-op ['a] ['f 'g])
          (mk-request ['f] 'j)
          (trans/mk-fetch ['j] 'h)
          (trans/mk-merge ['h 'g] 'd)
          (trans/mk-merge ['c 'd] 'e)])



(def ir4 [(mk-if-op [] ['a 'b])
          (mk-request ['b] 'j)
          (trans/mk-fetch ['j] 'c)
          (mk-request ['c] 'k)
          (trans/mk-fetch ['k] 'i)
          (mk-if-op ['a] ['f 'g])
          (mk-request ['f] 'l)
          (trans/mk-fetch ['l] 'h)
          (trans/mk-merge ['h 'g] 'd)
          (trans/mk-merge ['i 'd] 'e)
          (ir/mk-func "consumer" ['e] 'p)])


(def ir5 [(mk-if-op [] ['a 'b])
          (mk-request ['b] 'f)
          (mk-request ['c] 'g)
          (trans/mk-fetch ['f] 'c)
          (trans/mk-fetch ['g] 'd)
          (trans/mk-merge ['d 'a] 'e)])


(def to-print {2 ir2
               3 ir3
               4 ir4
               5 ir5})



(doall
  (map
    (fn [[irnum ir]]
      (let [transformed (trans/if-rewrite ir)
            cat-merges (trans/cat-redundant-merges transformed)
            cat-ids (trans/cat-redundant-identities cat-merges)
            coerce-merges (trans/coerce-merges cat-ids)]
        (render-to-file "original" irnum ir)
        (render-to-file "ir-rewrite" irnum transformed)
        (render-to-file "cat-merges" irnum cat-merges)
        (render-to-file "cat-ids" irnum cat-ids)
        (render-to-file "coerce-merges" irnum coerce-merges)))
    (seq to-print)))