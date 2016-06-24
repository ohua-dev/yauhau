(ns yauhau.try-compile
  (:require [com.ohua.lang :refer [<-ohua ohua algo]]
            [clojure.test :refer [deftest is]]
            [yauhau.ir-transform]
            [com.ohua.logging :as l])
  (:import (yauhau IDataSource BuildReq IncDataSource Request)))


(l/enable-compilation-logging)

(ohua :import [yauhau.functions])


(defmacro compile [& args]
  `(<-ohua
    ~@args
    :compile-with-config {:df-transformations [~@yauhau.ir-transform/transformations]}))


;(defn mk-req [data source]
;  (Request. data source))

(deftest test-smap
  (let [data-source (IncDataSource. (int 0))
        v (map int [1 2 3])
        result (compile
                 (smap
                   (algo [a] (fetch (mk-req a data-source)))
                   v))]
    (is
      [2 3 4]
      result)
    (is
      1
      (.-counter data-source))))
