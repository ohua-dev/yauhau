;
; Copyright (c) Justus Adam, Sebastian Ertel, Andres Goens 2016. All Rights Reserved.
;
; This source code is licensed under the terms described in the associated LICENSE file.
;

(ns yauhau.cache-transform
  (:require [com.ohua.ir :as ir :refer [mk-func reindex-stuff fn-name-in]])
  (:import [com.ohua.ir IRFunc]))


(def is-store? (fn-name-in #{'store 'com.ohua.fetch.operators/store}))
(def is-accum? (fn-name-in #{'__accum-fetch 'com.ohua.fetch.operators/__accum-fetch}))


(defn make-cached
  "Create a new function with the name provided, the same in- and outputs of the function provided and a cache binding
  as an additional input."
  [new-name cache-sym ir-fn]
  (mk-func new-name
           (into [] (reindex-stuff :in-idx (cons cache-sym (:args ir-fn))))
           (.-return ir-fn)))


(defn cache-rewrite
  "Add a cache manager to the ir provided and turn all known cacheable funcitons into their cache enabled equivalents."
  [cache-type ir]
  (let [cache-local (gensym "a")]
    (->> (conj ir (mk-func cache-type [] [cache-local]))
         (ir/change-nodes-where is-accum? (partial make-cached "__cached-accum-fetch" cache-local))
         (ir/change-nodes-where is-store? (partial make-cached "__cached-store" cache-local)))))
