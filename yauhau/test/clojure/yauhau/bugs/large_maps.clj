(ns yauhau.bugs.large-maps
  (:require [com.ohua.lang :refer [ohua <-ohua defalgo]]
            [clojure.test :refer [deftest]]
            [yauhau.ir-transform]
            [com.ohua.logging :refer [enable-compilation-logging]]))

(set! (com.ohua.engine.utils.GraphVisualizer/PRINT_FLOW_GRAPH) (str "test/level-graph-flow"))
(ohua :import [yauhau.functions])

(defmacro get-data [& args]
  `(yauhau.functions/fetch
     (yauhau.functions/read-request ~@args)))

(def compute yauhau.functions/compute)


; (defalgo ifnlocal124 [parameter-1]
; (let [local185 parameter-1]
; (let [local180 (get-data local185 "service-name" 180) local181 (compute  181) local182 (compute  182) local184 (get-data  "service-name" 184)]
; (let [local179 (get-data local181 local182 local184 "service-name" 179)]
; (get-data local179 local180 "service-name" 183)
; )))
; )
; (defalgo ifnlocal117 [parameter-1]
; (let [local178 parameter-1 local179 (get-data  "service-name" 179)]
; (let [local172 (compute local178 172) local173 (compute local179 173) local174 (compute  174) local175 (get-data  "service-name" 175) local177 (get-data  "service-name" 177)]
; (let [local171 (compute local173 local175 local177 171)]
; (get-data local171 local172 local174 "service-name" 176)
; )))
; )
; (defalgo ifnlocal111 [parameter-1]
; (let [local166 parameter-1 local167 (compute  167)]
; (let [local165 (get-data  "service-name" 165)]
; (get-data local165 local166 local167 "service-name" 168)
; ))
; )
; (defalgo ifnlocal108 [parameter-1]
; (let [local164 parameter-1 local165 (get-data  "service-name" 165)]
; (let [local160 (get-data local164 "service-name" 160) local161 (get-data local165 "service-name" 161) local163 (get-data  "service-name" 163)]
; (let [local159 (compute local160 local163 159)]
; (get-data local159 local161 "service-name" 162)
; )))
; )
; (defalgo ifnlocal104 [parameter-1]
; (let [local158 parameter-1 local159 (get-data  "service-name" 159)]
; (let [local154 (get-data local158 "service-name" 154) local155 (get-data local159 "service-name" 155) local157 (get-data  "service-name" 157)]
; (let [local153 (compute local154 local157 153) local160 (get-data  "service-name" 160)]
; (compute local153 local155 local160 156)
; )))
; )
; (defalgo ifnlocal90 [parameter-1]
; (let [local147 parameter-1 local148 (get-data  "service-name" 148) local149 (compute  149)]
; (let [local146 (get-data local148 "service-name" 146)]
; (get-data local146 local147 local149 "service-name" 150)
; ))
; )
; (defalgo ifnlocal77 [parameter-1]
; (let [local140 parameter-1 local141 (get-data  "service-name" 141) local142 (get-data  "service-name" 142) local144 (get-data  "service-name" 144)]
; (let [local139 (get-data local141 local142 local144 "service-name" 139)]
; (get-data local139 local140 "service-name" 143)
; ))
; )
; (defalgo ifnlocal59 [parameter-1]
; (let [local134 parameter-1 local135 (compute  135) local137 (get-data  "service-name" 137)]
; (let [local133 (compute local134 local135 local137 133)]
; (compute local133 136)
; ))
; )
; (defalgo ifnlocal39 [parameter-1]
; (let [local128 parameter-1 local129 (get-data  "service-name" 129) local131 (get-data  "service-name" 131)]
; (let [local127 (get-data local128 local129 local131 "service-name" 127)]
; (compute local127 130)
; ))
; )
(defalgo ifnlocal124 [parameter-1] (compute parameter-1))
(defalgo ifnlocal117 [parameter-1] (compute parameter-1))
(defalgo ifnlocal111 [parameter-1] (compute parameter-1))
(defalgo ifnlocal108 [parameter-1] (compute parameter-1))
(defalgo ifnlocal104 [parameter-1] (compute parameter-1))
(defalgo ifnlocal90 [parameter-1] (compute parameter-1))
(defalgo ifnlocal77 [parameter-1] (compute parameter-1))
(defalgo ifnlocal59 [parameter-1] (compute parameter-1))
(defalgo ifnlocal39 [parameter-1] (compute parameter-1))


; (defalgo ifnlocal30 [parameter-1]
; (let [local126 parameter-1]
; (let [local121 (get-data local126 "service-name" 121) local122 (get-data  "service-name" 122) local123 (compute  123) local125 (get-data  "service-name" 125)]
; (let [local120 (get-data local121 local125 "service-name" 120)]
; (count (smap ifnlocal124 (mvector local120 local122 local123) ))
; )))
; )
; (defalgo ifnlocal29 [parameter-1]
; (let [local118 parameter-1]
; (let [local112 (compute local118 112) local113 (compute  113) local114 (compute  114) local115 (compute  115) local116 (get-data  "service-name" 116)]
; (let [local111 (count (smap ifnlocal111 (mvector local113 local114) ))]
; (count (smap ifnlocal117 (mvector local111 local112 local115 local116) ))
; )))
; )
; (defalgo ifnlocal27 [parameter-1]
; (let [local105 parameter-1 local106 (compute  106) local107 (get-data  "service-name" 107)]
; (let [local104 (count (smap ifnlocal104 (mvector local106 local107) ))]
; (count (smap ifnlocal108 (mvector local104 local105) ))
; ))
; )
; (defalgo ifnlocal23 [parameter-1]
; (let [local103 parameter-1]
; (let [local94 (compute local103 94) local95 (get-data  "service-name" 95) local96 (get-data  "service-name" 96) local97 (compute  97) local98 (get-data  "service-name" 98) local99 (get-data  "service-name" 99) local100 (compute  100) local102 (get-data  "service-name" 102)]
; (let [local93 (get-data local94 local97 local98 local100 local102 "service-name" 93)]
; (compute local93 local95 local96 local99 101)
; )))
; )
; (defalgo ifnlocal21 [parameter-1]
; (let [local92 parameter-1 local93 (get-data  "service-name" 93) local94 (get-data  "service-name" 94)]
; (let [local87 (compute local92 87) local88 (get-data local93 "service-name" 88) local89 (get-data local94 "service-name" 89) local91 (get-data  "service-name" 91)]
; (let [local86 (compute local89 local91 86)]
; (count (smap ifnlocal90 (mvector local86 local87 local88) ))
; )))
; )
; (defalgo ifnlocal20 [parameter-1]
; (let [local81 parameter-1 local82 (compute  82) local84 (get-data  "service-name" 84)]
; (let [local80 (get-data local84 "service-name" 80)]
; (compute local80 local81 local82 83)
; ))
; )


(defalgo ifnlocal30 [parameter-1] (compute parameter-1))
(defalgo ifnlocal29 [parameter-1] (compute parameter-1))
(defalgo ifnlocal27 [parameter-1] (compute parameter-1))
(defalgo ifnlocal23 [parameter-1] (compute parameter-1))
(defalgo ifnlocal21 [parameter-1] (compute parameter-1))
(defalgo ifnlocal20 [parameter-1] (compute parameter-1))



(defalgo ifnlocal18 [parameter-1]
(let [local79 parameter-1]
(let [local75 parameter-1 local76 (get-data  "service-name" 76) local78 (get-data  "service-name" 78)]
(let [local74 (get-data local78 "service-name" 74)]
(count (smap ifnlocal77 (mvector local74 local75 local76) ))
)))
)
(defalgo ifnlocal17 [parameter-1]
(let [local67 parameter-1 local68 (get-data  "service-name" 68) local69 (get-data  "service-name" 69) local70 (get-data  "service-name" 70) local72 (get-data  "service-name" 72)]
(let [local66 (get-data local67 local68 local69 local70 local72 "service-name" 66)]
(get-data local66 "service-name" 71)
))
)
(defalgo ifnlocal12 [parameter-1]
(let [local60 parameter-1 local61 (get-data  "service-name" 61) local62 (get-data  "service-name" 62)]
(let [local59 (count (smap ifnlocal59 (mvector local61 local62) ))]
(compute local59 local60 63)
))
)
(defalgo ifnlocal10 [parameter-1]
(let [local52 parameter-1 local53 (compute  53) local54 (compute  54) local55 (get-data  "service-name" 55)]
(let [local51 (get-data local53 "service-name" 51)]
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
(let [local43 parameter-1 local44 (get-data  "service-name" 44)]
(let [local40 (compute local43 40) local41 (compute local44 41)]
(let [local39 (count (smap ifnlocal39 (mvector local40 local41) ))]
(get-data local39 "service-name" 42)
)))
)

; (defalgo ifnlocal18 [parameter-1] (compute parameter-1))
; (defalgo ifnlocal17 [parameter-1] (compute parameter-1))
; (defalgo ifnlocal12 [parameter-1] (compute parameter-1))
; (defalgo ifnlocal10 [parameter-1] (compute parameter-1))
; (defalgo ifnlocal6 [parameter-1] (compute parameter-1))
; (defalgo ifnlocal5 [parameter-1] (compute parameter-1))


(deftest run_test_level12_0
(<-ohua
(let [local31 (compute  31) local32 (compute  32) local33 (get-data  "service-name" 33) local34 (compute  34) local35 (get-data  "service-name" 35) local36 (get-data  "service-name" 36)]
(let [local27 (count (smap ifnlocal27 (mvector local31 local35 local36) )) local28 (get-data local31 local32 local34 local36 "service-name" 28) local29 (count (smap ifnlocal29 (mvector local32 local33 local34 local35 local36) )) local30 (count (smap ifnlocal30 (mvector local33 local34 local35) ))]
(let [local24 (get-data local28 local29 local30 local31 "service-name" 24) local25 (get-data local27 "service-name" 25) local26 (compute local28 local30 local31 local34 26)]
(let [local22 (get-data local24 local28 "service-name" 22) local23 (count (smap ifnlocal23 (mvector local24 local27 local28 local29 local30 local31 local32) ))]
(let [local19 (get-data local22 local23 local25 local30 "service-name" 19) local20 (count (smap ifnlocal20 (mvector local23 local30) )) local21 (count (smap ifnlocal21 (mvector local22 local24 local33) ))]
(let [local15 (compute local20 local22 local25 15) local16 (get-data local19 local23 local36 "service-name" 16) local17 (count (smap ifnlocal17 (mvector local19 local20 local21 local22) )) local18 (count (smap ifnlocal18 (mvector local19 local21) ))]
(let [local11 (get-data local16 local17 local20 "service-name" 11) local12 (count (smap ifnlocal12 (mvector local15 local18 local21) )) local13 (compute local17 local18 13) local14 (compute local27 14)]
(let [local8 (compute local13 local14 local19 local31 8) local9 (compute local12 local13 local16 local18 local21 local25 9) local10 (count (smap ifnlocal10 (mvector local11 local13 local14 local22) ))]
(let [local6 (count (smap ifnlocal6 (mvector local10 local14) )) local7 (get-data  "service-name" 7)]
(let [local4 (get-data local7 local12 local14 "service-name" 4) local5 (count (smap ifnlocal5 (mvector local8 local12) ))]
(let [local2 (compute local4 2) local3 (compute local5 local15 local18 3)]
(get-data local2 local3 local6 local9 local26 "service-name" 37)
)))))))))))
:compile-with-config {:df-transformations yauhau.ir-transform/transformations}))
