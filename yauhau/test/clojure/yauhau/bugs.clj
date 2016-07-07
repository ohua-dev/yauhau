(ns yauhau.bugs
  (:require [com.ohua.lang :refer [ohua defalgo]]
            [yauhau.ir-transform]
            [com.ohua.logging :as l]
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

(deftest one-to-n-bug-test
  (defalgo ifnlocal1 []
    (let [local3 (get-data  "service-name" 3)]
      (get-data local3 "service-name" 4)))

  (defn run_test_level1_0 []
    (ohua
      (smap ifnlocal1 (yauhau.functions/vector))
      :compile-with-config {:df-transformations yauhau.ir-transform/transformations}))
  (run_test_level1_0))

(deftest ghostvalues
  (defalgo ifnlocal7 [parameter-1]
    parameter-1)

  (defalgo ifnlocal2 []
    (let [local6 (get-data  "service-name" 6)]
      (ifnlocal7 local6)))

  (defn run_test_level2_0 []
    (ohua
      (let [ [local2 local3] (mvector (smap ifnlocal2 (yauhau.functions/mvector 2) ) (get-data  "service-name" 3))]
        (get-data local2 local3 "service-name" 4))
      :compile-with-config {:df-transformations yauhau.ir-transform/transformations}))

  (run_test_level2_0))
