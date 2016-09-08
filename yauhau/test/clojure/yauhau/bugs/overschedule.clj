(ns yauhau.bugs.overschedule
  (:require [com.ohua.lang :as o :refer [defalgo algo]]
            [clojure.test :refer [deftest is]]
            [yauhau.ir-transform]
            [com.ohua.logging :refer [enable-compilation-logging]])
  (:import (com.ohua.lang.compile FlowGraphCompiler)))


; (enable-compilation-logging)
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


(defalgo ifnlocal103 [parameter-1]
(let [local156 parameter-1 local157 (get-data  "service-name" 157) local158 (get-data  "service-name" 158)]
(let [local151 (get-data local156 "service-name" 151) local152 (compute local157 152) local153 (get-data local158 "service-name" 153) local155 (get-data  "service-name" 155)]
(let [local150 (get-data local152 local153 local155 "service-name" 150)]
(get-data local150 local151 "service-name" 154)
)))
)
(defalgo ifnlocal85 [parameter-1]
(let [local144 parameter-1 local145 (compute  145) local146 (compute  146) local148 (get-data  "service-name" 148)]
(let [local143 (compute local144 local145 local148 143)]
(get-data local143 local146 "service-name" 147)
))
)
(defalgo ifnlocal82 [parameter-1]
(let [local142 parameter-1 local143 (get-data  "service-name" 143)]
(let [local138 (compute local142 138) local139 (compute local143 139) local141 (get-data  "service-name" 141)]
(let [local137 (compute local141 137) local144 (get-data  "service-name" 144)]
(get-data local137 local138 local139 local144 "service-name" 140)
)))
)
(defalgo ifnlocal78 [parameter-1]
(let [local132 parameter-1 local133 (get-data  "service-name" 133) local135 (get-data  "service-name" 135)]
(let [local131 (get-data local132 local135 "service-name" 131)]
(get-data local131 local133 "service-name" 134)
))
)
(defalgo ifnlocal75 [parameter-1]
(let [local126 parameter-1 local127 (get-data  "service-name" 127) local129 (get-data  "service-name" 129)]
(let [local125 (get-data local129 "service-name" 125)]
(get-data local125 local126 local127 "service-name" 128)
))
)
(defalgo ifnlocal59 [parameter-1]
(let [local124 parameter-1 local125 (get-data  "service-name" 125)]
(let [local120 (compute local124 120) local121 (compute local125 121) local123 (get-data  "service-name" 123)]
(let [local119 (get-data local121 local123 "service-name" 119) local126 (get-data  "service-name" 126)]
(get-data local119 local120 local126 "service-name" 122)
)))
)
(defalgo ifnlocal30 [parameter-1]
(let [local118 parameter-1 local119 (get-data  "service-name" 119)]
(let [local113 (compute local118 113) local114 (compute local119 114) local115 (compute  115) local117 (get-data  "service-name" 117)]
(let [local112 (compute local117 112)]
(get-data local112 local113 local114 local115 "service-name" 116)
)))
)
(defalgo ifnlocal29 [parameter-1]
(let [local104 parameter-1 local105 (compute  105) local106 (compute  106) local107 (get-data  "service-name" 107) local108 (compute  108)]
(let [local103 (count (smap ifnlocal103 (mvector local105 local106 local108) ))]
(get-data local103 local104 local107 "service-name" 109)
))
)
(defalgo ifnlocal27 [parameter-1]
(let [local97 (assert-call-count "local97" (int 3) parameter-1) local98 (trace "get-data" (get-data  "service-name" 98)) local99 (compute  99)]
(let [local96 (compute  96)]
(compute local96 local97 local98 local99 100)
))
)
(defalgo ifnlocal23 [parameter-1]
(let [local94 parameter-1 local95 (get-data  "service-name" 95) local96 (get-data  "service-name" 96) local97 (get-data  "service-name" 97)]
(let [local86 (compute local94 86) local87 (get-data local95 "service-name" 87) local88 (get-data local96 "service-name" 88) local89 (compute local97 89) local90 (get-data  "service-name" 90) local91 (get-data  "service-name" 91) local92 (compute  92)]
(let [local85 (count (smap ifnlocal85 (mvector local88 local91 local92) ))]
(get-data local85 local86 local87 local89 local90 "service-name" 93)
)))
)
(defalgo ifnlocal21 [parameter-1]
(let [local83 parameter-1]
(let [local79 (get-data local83 "service-name" 79) local80 (get-data  "service-name" 80) local81 (compute  81)]
(let [local78 (count (smap ifnlocal78 (mvector local79 local81) ))]
(count (smap ifnlocal82 (mvector local78 local80) ))
)))
)
(defalgo ifnlocal20 [parameter-1]
(let [local77 parameter-1 local78 (get-data  "service-name" 78)]
(let [local73 (get-data local77 "service-name" 73) local74 (compute local78 74) local76 (get-data  "service-name" 76)]
(let [local72 (get-data local74 local76 "service-name" 72)]
(count (smap ifnlocal75 (mvector local72 local73) ))
)))
)
(defalgo ifnlocal18 [parameter-1]
(let [local71 parameter-1 local72 (get-data  "service-name" 72)]
(let [local67 (get-data local71 "service-name" 67) local68 (get-data local72 "service-name" 68) local70 (get-data  "service-name" 70)]
(let [local66 (get-data local70 "service-name" 66) local73 (get-data  "service-name" 73)]
(get-data local66 local67 local68 local73 "service-name" 69)
)))
)
(defalgo ifnlocal12 [parameter-1]
(let [local64 parameter-1 local65 (get-data  "service-name" 65) local66 (get-data  "service-name" 66)]
(let [local60 (compute local64 60) local61 (get-data local65 "service-name" 61) local62 (get-data local66 "service-name" 62)]
(let [local59 (count (smap ifnlocal59 (mvector local61 local62) )) local67 (get-data  "service-name" 67)]
(compute local59 local60 local67 63)
)))
)
(defalgo ifnlocal10 [parameter-1]
(let [local52 parameter-1 local53 (compute  53) local54 (compute  54) local55 (get-data  "service-name" 55) local57 (get-data  "service-name" 57)]
(let [local51 (get-data local53 local57 "service-name" 51)]
(get-data local51 local52 local54 local55 "service-name" 56)
))
)
(defalgo ifnlocal6 [parameter-1]
(let [local46 parameter-1 local47 (compute  47) local49 (get-data  "service-name" 49)]
(let [local45 (get-data local47 local49 "service-name" 45)]
(get-data local45 local46 "service-name" 48)
))
)
(defalgo ifnlocal5 [parameter-1]
(let [local44 parameter-1 local45 (get-data  "service-name" 45)]
(let [local40 (compute local44 40) local41 (compute local45 41) local43 (get-data  "service-name" 43)]
(let [local39 (compute local40 local41 local43 39)]
(get-data local39 "service-name" 42)
)))
)
(def is-three (partial = 3))
(def is-two (partial = 2))
(defn run_test_level12_0 []
(ohua
(let [local31 (assert-call-count "local 31" (int 1) (compute  31)) local32 (compute  32) local33 (assert-call-count "local33" (int 1) (get-data  "service-name" 33)) local34 (compute  34) local35 (get-data  "service-name" 35) local36 (get-data  "service-name" 36)]
(let [local27 (count (smap ifnlocal27 (assert-call-count "mvec27" (int 1) (mvector local31 local35 local36)) )) local28 (get-data local31 local32 local34 local36 "service-name" 28) local29 (count (smap ifnlocal29 (mvector local32 local33 local34 local35 local36) )) local30 (assert-op "local30" is-three (count (smap ifnlocal30 (mvector local33 local34 local35) )))]
(let [local24 (get-data local28 local29 local30 local31 "service-name" 24) local25 (get-data local27 "service-name" 25) local26 (compute local28 local30 local31 local34 26)]
(let [local22 (get-data local24 local28 "service-name" 22) local23 (count (smap ifnlocal23 (mvector local24 local27 local28 local29 local30 local31 local32) ))]
(let [local19 (get-data local22 local23 local25 local30 "service-name" 19) local20 (count (smap ifnlocal20 (mvector local23 local30) )) local21 (count (smap ifnlocal21 (mvector local22 local24 local33) ))]
(let [local15 (compute local20 local22 local25 15) local16 (get-data local19 local23 local36 "service-name" 16) local17 (compute local19 local20 local21 local22 17) local18 (count (smap ifnlocal18 (mvector local19 local21) ))]
(let [local11 (get-data local16 local17 local20 "service-name" 11) local12 (count (smap ifnlocal12 (mvector local15 local18 local21) )) local13 (compute local17 local18 13) local14 (compute local27 14)]
(let [local8 (compute local13 local14 local19 local31 8) local9 (compute local12 local13 local16 local18 local21 local25 9) local10 (count (smap ifnlocal10 (mvector local11 local13 local14 local22) ))]
(let [local6 (assert-op "local6" is-two (count (smap ifnlocal6 (mvector local10 local14) ))) local7 (get-data  "service-name" 7)]
(let [local4 (get-data local7 local12 local14 "service-name" 4) local5 (count (smap ifnlocal5 (mvector local8 local12) ))]
(let [local2 (compute local4 2) local3 (compute local5 local15 local18 3)]
(assert-call-count "end" (int 1) (get-data local2 local3 local6 local9 local26 "service-name" 37))
)))))))))))
))

;(defalgo foo [i]
; (let [a (get-data i "service-name" 1)]
;   (assert-call-count "middle" (int 12) (compute a))))
;
;(defn another []
;  (ohua
;    (let [a
;          (smap
;            (algo [i]
;                  (mvector
;                    (smap
;                      foo
;                      (mvector 1 2 3 i))
;                    (get-data "service-name" 4)))
;            (mvector 1 2 3))]
;      (assert-call-count "end" (int 1) a))))

(deftest one
  (run_test_level12_0))
