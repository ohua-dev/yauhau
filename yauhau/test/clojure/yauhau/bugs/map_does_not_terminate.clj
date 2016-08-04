(ns yauhau.bugs.map-does-not-terminate
  (:require [com.ohua.lang :as o :refer [defalgo]]
            [clojure.test :refer [deftest is]]
            [yauhau.ir-transform]
            [com.ohua.logging :refer [enable-compilation-logging]]))


; (enable-compilation-logging)
(set! (. com.ohua.engine.utils.GraphVisualizer PRINT_FLOW_GRAPH) (str "test/level-graph-flow"))

; This terminates

(o/ohua :import [yauhau.functions])

(defmacro get-data [& args]
  `(yauhau.functions/fetch
     (yauhau.functions/read-request ~@args)))

(def compute yauhau.functions/compute)

(defmacro ohua [& args]
  `(o/<-ohua
     ~@args
     :compile-with-config {:df-transformations yauhau.ir-transform/transformations}))

(defalgo ifnlocal2 [parameter-1]
(let [local13 parameter-1 local14 (get-data  "service-name" 14)]
(let [local9 (compute local13 9) local10 (get-data local14 "service-name" 10) local12 (get-data  "service-name" 12)]
(let [local8 (get-data local9 local12 "service-name" 8) local15 (get-data  "service-name" 15)]
(get-data local8 local10 local15 "service-name" 11)
)))
)
(defn terminates []
(ohua
(let [local4 (get-data  "service-name" 4) local5 (get-data  "service-name" 5)]
(let [local2 (count (smap ifnlocal2 (mvector local4 local5) )) local3 (get-data  "service-name" 3)]
(get-data local2 local3 "service-name" 6)
))
))

; ------------------------------------------------------------------------------

; This doesn't

(defalgo ifnlocal2 [parameter-1] (identity parameter-1))

(defn doesnt-terminate []
  (ohua
    (smap ifnlocal2 (mvector (int 5)))))

(deftest does-it-terminate?
  ; (is
  ;   ((constantly true) (terminates))
  ;   "The first one fails")
  (is
    ((constantly true) (doesnt-terminate))
    "The second one fails"))
