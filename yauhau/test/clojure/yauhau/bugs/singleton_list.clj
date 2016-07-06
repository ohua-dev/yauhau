(ns yauhau.bugs.singleton-list
  (:require [com.ohua.lang :refer [ohua <-ohua defalgo]]
            [clojure.test :refer [deftest]]
            [yauhau.ir-transform]
            [com.ohua.logging :refer [enable-compilation-logging]]))

(ohua :import [yauhau.functions])

(defmacro get-data [& args]
  `(yauhau.functions/fetch
     (yauhau.functions/read-request ~@args)))

(def compute yauhau.functions/compute)

(defalgo ifnlocal25 []
  (let [local47 (get-data  "service-name" 47)]
    (get-data local47 "service-name" 48)))


(defalgo ifnlocal18 [parameter-1]
  (let [local43 parameter-1]
    (let [local42 (compute  42)]
      (get-data local42 local43 "service-name" 44))))


(defalgo ifnlocal17 []
  (let [local38 (get-data  "service-name" 38)]
    (compute local38 39)))

(defalgo ifnlocal14 [parameter-1]
  (let [local34 parameter-1]
    (let [local33 (get-data  "service-name" 33)]
      (compute local33 local34 35))))


(defalgo ifnlocal13 []
  (let [local29 (compute  29)]
    (get-data local29 "service-name" 30)))


(defalgo ifnlocal7 []
  (let [local25 (ifnlocal25)]
    (get-data local25 "service-name" 26)))


(defalgo ifnlocal6 []
  (let [local21 (get-data  "service-name" 21)]
    (get-data local21 "service-name" 22)))


(defalgo ifnlocal5 []
  (let [local17 (ifnlocal17)]
    (ifnlocal18 local17)))

(defalgo ifnlocal2 [parameter-1 parameter-2]
  (let [local12 parameter-1 local13 parameter-2]
    (let [local11 (compute local12 local13 11)]
      (ifnlocal14 local11))))

(deftest run_test_level3_0
  (<-ohua
    (let [local5 (ifnlocal5)
          local6 (ifnlocal6)
          local7 (ifnlocal7)
          local8 (compute  8)]
      (let [local2 (ifnlocal2 local5 local7)
            local3 (get-data local7 local8 "service-name" 3)
            local4 (get-data local7 "service-name" 4)]
        (get-data local2 local3 local4 local6 "service-name" 9)))
    ;:compile-with-config {:df-transformations yauhau.ir-transform/transformations}
    ))
