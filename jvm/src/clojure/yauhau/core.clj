(ns yauhau.core
  (:require [ohua.lang]
            [ohua.util :refer [+option]]))

(defn yauhau-fn [code option] (ohua.lang/ohua-fn-with yauhau.Compiler/compileAndSpliceEnv code option))

(defmacro yauhau
  "See `ohua-fn-with`."
  ([code] (yauhau-fn code {}))
  ([code options] (yauhau-fn code options)))


(defmacro <-yauhau
  "See `ohua-fn-with`."
  ([code] (yauhau-fn code #{:capture}))
  ([code options]
    (yauhau-fn code
      (+option options :capture))))
