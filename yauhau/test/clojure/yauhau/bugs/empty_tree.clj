(ns yauhau.bugs.empty-tree
  (:require [com.ohua.lang :as o :refer [defalgo]]
            [clojure.test :refer [deftest is]]
            [yauhau.ir-transform]
            [com.ohua.logging :refer [enable-compilation-logging]])
  (:import (com.ohua.lang.compile FlowGraphCompiler)))


(enable-compilation-logging)
(set! (. com.ohua.engine.utils.GraphVisualizer PRINT_FLOW_GRAPH) (str "test/level-graph-flow"))
(set! com.ohua.engine.RuntimeProcessConfiguration/LOGGING_ENABLED true)
(set! FlowGraphCompiler/SKIP_FUNCTION_SAFETY_ANALYSIS true)
(set! FlowGraphCompiler/SKIP_DEPENDENCY_ANALYSIS true)
(set! yauhau.functions.Functionality$DummyDataSource/delay 0)
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


(defalgo ifnelselocal17 [parameter-1 parameter-2 parameter-3]
(let [local100 parameter-1]
(let [local95 (get-data local100 "service-name" 95) local96 (get-data  "service-name" 96) local97 (get-data  "service-name" 97) local99 (get-data  "service-name" 99)]
(let [local94 (compute local95 local99 94)]
(compute local94 local96 local97 98)
)))
)
(defalgo ifnthenlocal17 [parameter-1 parameter-2 parameter-3]
(let [local88 parameter-1 local89 parameter-2 local90 parameter-3]
(let [local87 (get-data local88 local90 "service-name" 87)]
(compute local87 local89 91)
))
)
(defalgo ifnelselocal16 [parameter-1 parameter-2 parameter-3]
(let [local81 parameter-1 local82 parameter-2 local83 parameter-3 local85 (get-data  "service-name" 85)]
(let [local80 (compute local81 local82 local83 local85 80)]
(get-data local80 "service-name" 84)
))
)
(defalgo ifnthenlocal16 [parameter-1 parameter-2 parameter-3]
(let [local79 parameter-1 local80 parameter-2 local81 parameter-3]
(let [local74 (compute local79 74) local75 (get-data local80 "service-name" 75) local76 (compute local81 76) local78 (get-data  "service-name" 78)]
(let [local73 (get-data local74 local76 local78 "service-name" 73)]
(get-data local73 local75 "service-name" 77)
)))
)
(defalgo ifnelselocal8 [parameter-1 parameter-2 parameter-3]
(let [local72 parameter-1 local73 parameter-2]
(let [local67 (get-data local72 "service-name" 67) local68 (compute local73 68) local69 (compute  69) local71 (get-data  "service-name" 71)]
(let [local66 (get-data local67 local68 local71 "service-name" 66)]
(get-data local66 local69 "service-name" 70)
)))
)
(defalgo ifnthenlocal8 [parameter-1 parameter-2 parameter-3]
(let [local60 parameter-1 local61 parameter-2 local62 parameter-3 local64 (get-data  "service-name" 64)]
(let [local59 (compute local64 59)]
(compute local59 local60 local61 local62 63)
))
)
(defalgo ifnelselocal7 [parameter-1 parameter-2 parameter-3]
(let [local53 parameter-1 local54 parameter-2 local55 parameter-3 local57 (get-data  "service-name" 57)]
(let [local52 (compute local57 52)]
(compute local52 local53 local54 local55 56)
))
)
(defalgo ifnthenlocal7 [parameter-1 parameter-2 parameter-3]
(let [local51 parameter-1 local52 parameter-2 local53 parameter-3]
(let [local46 (compute local51 46) local47 (compute local52 47) local48 (get-data local53 "service-name" 48) local50 (get-data  "service-name" 50)]
(let [local45 (compute local46 local50 45)]
(compute local45 local47 local48 49)
)))
)
(defalgo ifnelselocal4 [parameter-1 parameter-2 parameter-3]
(let [local44 parameter-1 local45 parameter-2]
(let [local39 (get-data local44 "service-name" 39) local40 (compute local45 40) local41 (get-data  "service-name" 41) local43 (get-data  "service-name" 43)]
(let [local38 (compute local39 local43 38)]
(get-data local38 local40 local41 "service-name" 42)
)))
)
(defalgo ifnthenlocal4 [parameter-1 parameter-2 parameter-3]
(let [local37 parameter-1 local38 parameter-2 local39 parameter-3]
(let [local32 (compute local37 32) local33 (get-data local38 "service-name" 33) local34 (get-data local39 "service-name" 34) local36 (get-data  "service-name" 36)]
(let [local31 (compute local32 local33 local34 local36 31)]
(compute local31 35)
)))
)
(defn run_test_level10_0 []
(ohua
(let [local26 (get-data  "service-name" 26) local27 (compute  27) local28 (compute  28)]
(let [local23 (get-data local28 "service-name" 23) local24 (compute local26 local28 24) local25 (get-data local26 "service-name" 25)]
(let [local20 (compute local23 local26 20) local21 (compute local25 21) local22 (compute  22)]
(let [local17 (if local21 (ifnthenlocal17 local20 local21 local23) (ifnelselocal17 local20 local21 local23)) local18 (compute local20 local24 18) local19 (compute local22 19)]
(let [local13 (get-data local17 local18 local22 local26 local27 "service-name" 13) local14 (compute local17 local18 local19 local27 local28 14) local15 (compute local18 local19 15) local16 (if local20 (ifnthenlocal16 local17 local20 local21) (ifnelselocal16 local17 local20 local21))]
(let [local10 (compute local15 local16 10) local11 (compute local15 local21 11) local12 (compute local13 local14 local15 local16 local21 local24 12)]
(let [local7 (if local12 (ifnthenlocal7 local11 local12 local16) (ifnelselocal7 local11 local12 local16)) local8 (if local16 (ifnthenlocal8 local10 local14 local16) (ifnelselocal8 local10 local14 local16)) local9 (compute local13 local14 local16 local21 9)]
(let [local4 (if local18 (ifnthenlocal4 local10 local15 local18) (ifnelselocal4 local10 local15 local18)) local5 (compute local7 local9 local15 local20 5) local6 (get-data local7 local9 local12 local15 local19 "service-name" 6)]
(let [local2 (compute local5 local6 local8 local10 2) local3 (get-data local4 local6 local7 local11 local13 local18 "service-name" 3)]
(compute local2 local3 29)
)))))))))
))

(deftest one (run_test_level10_0))
