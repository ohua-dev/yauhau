;
; Copyright (c) Justus Adam, Sebastian Ertel, Andres Goens 2016. All Rights Reserved.
;
; This source code is licensed under the terms described in the associated LICENSE file.
;

(ns yauhau.concurrent-io-transform
  (:require [com.ohua.ir :refer [mk-func]]))

;;;
;;; The rewrite extracts the fetch execution from the batched version of the __accum-fetch function.
;;; In order to do so, we do batching and then use dataflow style programming to introduce parallelism:
;;; __accum-fetch => _accum -> dispatch -> fetch[n] -> merge -> unbatch
;;;

(defn rewrite-accum-fn [accum-fetch-fn]
  ; TODO this must be easier to write, like this:
  ;[_accum (__accum (:args accum-fetch-fn))
  ; _size (size _accum)
  ; _one-to-n (one-to-n _size _accum)
  ; _smap (smap-fun _one-to-n)
  ; _dispatches (__dispatch _smap)
  ; _fetches (map #(__batched-fetch %) dispatches) ; unclear how to express this properly
  ; _merge (merge _fetches)
  ; (:return accum-fetch-fn) (__unbatch _merge)
  ;]
  (if (> (count (:args accum-fetch-fn)) 1)
    (let [__accum (mk-func (symbol "__accum") (:args accum-fetch-fn) (gensym "accum-out"))

          ; pipe the remaining request list individually
          __size (mk-func (symbol "com.ohua.lang.operators" "size") [(:return __accum)] [(gensym "size")])
          __one-to-n (mk-func (symbol "com.ohua.lang.operators" "one-to-n") [(first (:return __size)) (:return __accum)] (gensym "one-to-n"))
          __smap (mk-func (symbol "com.ohua.lang.operators" "smap-fun") [(:return __one-to-n)] (gensym "smap"))

          __dispatch (mk-func (symbol "__dispatch") [(:return __smap)] (into []
                                                                             (map
                                                                               (fn [i]
                                                                                 ;`dispatch-out# -> generates the same var over and over again
                                                                                 (gensym (str "dispatch-out-" i "-")))
                                                                               (range (count (:args accum-fetch-fn))))))
          __fetches (map
                      #(mk-func (symbol "__batched-fetch") [%] (gensym "fetch-out"))
                      (:return __dispatch))
          __merge (mk-func (symbol "merge") (into [] (map :return __fetches)) (gensym "merge-out"))
          __unbatch (mk-func (symbol "__unbatch") [(:return __merge)] (:return accum-fetch-fn))]
      (conj __fetches __accum __size __one-to-n __smap __dispatch __merge __unbatch))
    [accum-fetch-fn]))

(defn rewrite [ir]
  (mapcat
    (fn [ir-fn]
      (if (.endsWith (name (:name ir-fn)) "__accum-fetch")
        (rewrite-accum-fn ir-fn)
        [ir-fn]))
    ir)
  )
