(ns yauhau.bugs.if-debug
  (:require [com.ohua.lang :as o :refer [defalgo algo]]
            [clojure.test :refer [deftest is]]
            [yauhau.ir-transform]
            [com.ohua.logging :refer [enable-compilation-logging]]
            [com.ohua.util.visual :as visual]))


(enable-compilation-logging)
(set! (. com.ohua.engine.utils.GraphVisualizer PRINT_FLOW_GRAPH) (str "test/level-graph-flow"))
(set! (. com.ohua.engine.utils.GraphVisualizer PRINT_SECTION_GRAPH) (str "test/section-graph"))
(set! yauhau.functions.Functionality$DummyDataSource/delay 0)
(set! com.ohua.lang.compile.FlowGraphCompiler/SKIP_DEPENDENCY_ANALYSIS true)

(set! *print-meta* true)

; This terminates

(o/ohua :import [yauhau.functions])

(defmacro get-data [& args]
  `(yauhau.functions/fetch
     (yauhau.functions/read-request ~@args)))

(def compute yauhau.functions/compute)

(defmacro ohua [& args]
  `(o/ohua
     ~@args
     :compile-with-config {:df-transformations yauhau.ir-transform/transformations}))

 ; (defalgo ifnelselocal10 [parameter-1 parameter-2 parameter-3]
 ; (let [local42 parameter-1 local43 parameter-2 local44 parameter-3]
 ; (let [local37 (compute local42 37) local38 (compute local43 38) local39 (compute local44 39) local41 (get-data  "service-name" 41)]
 ; (let [local36 (compute local38 local41 36)]
 ; (compute local36 local37 local39 40)
 ; )))
 ; )
(defalgo ifnelselocal11 [parameter-1 parameter-2 parameter-3]
  (compute parameter-1 parameter-2 (trace "else" parameter-3) (trace "independent get data" (get-data "service-name" 41)) 40)
)

(defalgo ifnthenlocal11 [parameter-1 parameter-2 parameter-3]
(trace "the algo" (compute parameter-1 parameter-2 (trace "dependent get data" (get-data (trace "then" parameter-3) "service-name" 29)) 33))
)
(defn doesnt-terminate []
  (let [v
        (ohua
          (let [local24 (get-data  "service-name" 24)]
            (let [local22 (get-data "service-name" 22)
                  local23 (get-data local24 "service-name" 23)]
              (let [local15 (get-data local22 local23 "service-name" 15)
                    local16 (get-data local22 "service-name" 16)
                    local17 (compute local22 local24 17)]
                (let [local12 (get-data local15 local16 local17 local24 "service-name" 12)]
                  (trace "if-out" (if true (ifnthenlocal11 local12 local15 local17) (ifnelselocal11 local12 local15 local17))))))))]
    (println v)
    v))
; ------------------------------------------------------------------------------

; This doesn't

(deftest does-it-terminate?
  ; (is
  ;   ((constantly true) (terminates))
  ;   "The first one fails")
  (is
    ((constantly true) (doesnt-terminate))
    "The second one fails")
  )

(deftest temp
  (is 3
      (count
        (let [a [1 2 3]]
          (o/<-ohua
            (smap
              (algo [item]
                    (trace "out" (mvector (trace "in1" item) (trace "in2" 3))))
              a)
            :compile-with-config {:df-transformations
                                  [(fn [{graph :graph :as g}]
                                     (visual/render-to-file "tmp-dump" graph)
                                     g)]})))))
