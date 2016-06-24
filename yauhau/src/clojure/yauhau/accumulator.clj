;
; Copyright (c) Justus Adam, Sebastian Ertel, Andres Goens 2016. All Rights Reserved.
;
; This source code is licensed under the terms described in the associated LICENSE file.
;

(ns yauhau.accumulator
  (:require [com.ohua.ir :as ir]))


(defn- ensure-vec [in]
  (if (vector? in)
    in
    (into [] in)))


(defn mk-accum-op [inputs outputs]
  (ir/mk-func 'yauhau.functions/__accum-fetch (ensure-vec inputs) (ensure-vec outputs)))


(defn mk-accum-for-fetches
  "Create a new accumulator where in- and outputs are the combined in and outputs of the fetches provided."
  [f]
  (mk-accum-op (mapcat :args f) (mapcat ir/get-return-vars f)))
