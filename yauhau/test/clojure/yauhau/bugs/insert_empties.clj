(ns yauhau.bugs.insert-empties
  (:require [com.ohua.lang :as o :refer [defalgo]]
            [clojure.test :refer [deftest is]]
            [yauhau.ir-transform]
            [com.ohua.logging :refer [enable-compilation-logging]]))


(enable-compilation-logging)
(set! (. com.ohua.engine.utils.GraphVisualizer PRINT_FLOW_GRAPH) (str "test/level-graph-flow"))
(set! yauhau.functions.Functionality$DummyDataSource/delay 0)
(set! com.ohua.lang.compile.FlowGraphCompiler/SKIP_DEPENDENCY_ANALYSIS true)

; (set! *print-meta* true)

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
(get-data (trace "algo in" parameter-1) "service-name" 11)
)
(defn doesnt-terminate []
(ohua
  (compute (trace "independent get data" (get-data 5 "service-name" 5)) (trace "if out" (if true (trace "if compute" (compute 3)) (trace "algo out" (ifnlocal2 4)))))
))

; (defn terminates []
; (ohua
;   (compute (get-data 5 "service-name" 5) (if true (compute 3) (get-data 4 "service-name" 11)))
; ))

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
