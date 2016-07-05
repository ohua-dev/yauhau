(ns yauhau.one-to-n-bug
  (:require [com.ohua.lang :refer [ohua defalgo]]
            [yauhau.ir-transform]
            [clojure.test :refer [deftest]]))

(ohua :import [yauhau.functions])

(defmacro get-data [& args]
  (if (and (< (count args) 3)
           (not (symbol? (second (reverse args)))))
    `(yauhau.functions/fetch
       (yauhau.functions/read-request ~@args))
    `(yauhau.functions/fetch
       (yauhau.functions/read-request ~@args))
    )
  )

(defmacro write-data [& args]
  (if (and (< (count args) 3)
           (not (symbol? (second (reverse args)))))
    `(yauhau.functions/store
       (yauhau.functions/write-request (into-array Object [~@args])))
    `(yauhau.functions/store
       (yauhau.functions/write-request ~@args))
    )
  )

(defmacro compute [& args]
  (if (< (count args) 2)
    `(yauhau.functions/compute ~@args)
    `(yauhau.functions/compute ~@args)
    )
  )

(defalgo ifnlocal1 []
  (let [local3 (get-data  "service-name" 3)]
    (get-data local3 "service-name" 4)))

(defn run_test_level1_0 []
  (ohua
    (smap ifnlocal1 (yauhau.functions/vector))
    :compile-with-config {:df-transformations yauhau.ir-transform/transformations}))


(deftest one-to-n-bug-test
  (run_test_level1_0))
