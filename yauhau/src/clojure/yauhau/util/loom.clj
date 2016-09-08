(ns yauhau.util.loom
  (:require [loom.graph :refer [digraph successors nodes add-edges add-nodes]]
            [loom.attr :refer [add-attr]]
            [com.ohua.ir :refer [mk-producer-map]]))


(defn- g-ir-to-loom-graph [relaxed]
  (let [f (if relaxed
            #(remove nil? %)
            (fn [a] (map #(assert (not (nil? %)) a) a)))]
    (fn [ir]
      (let [producers (mk-producer-map ir)]
        (apply add-edges
          (apply add-nodes (digraph) ir)
          (mapcat (fn [node] (map vector (f (map producers (:args node))) (repeat node))) ir))))))


(def ir-to-loom (g-ir-to-loom-graph false))

(def ir-subgr-to-loom (g-ir-to-loom-graph true))
