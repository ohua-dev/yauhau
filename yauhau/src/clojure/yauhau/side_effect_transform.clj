(ns yauhau.side-effect-transform
  (:require [com.ohua.ir :as ir]
            [loom.graph :refer [digraph successors nodes add-edges add-nodes]]
            [loom.attr :refer [add-attr]]
            [monads.core :refer [mdo return modify >>= get-state put-state]]
            [monads.state :as st]
            [clojure.set :as setlib]
            [yauhau.util.loom :refer [ir-to-loom-graph]]))


(defn is-algo? [ctx] (= 'com.ohua.lang/algo-in (:type %)))
(defn is-algo-out? [func] (= 'com.ohua.lang/algo-out (.-name func)))
(defn is-algo-in? [func] (= 'com.ohua.lang/algo-in (.-name func)))
(def is-read? (ir/fn-name-in #{'fetch 'yauhau.functions/fetch}))
(def is-write? (ir/fn-name-in #{'write 'yauhau.functions/write}))


(defn state-add-edges [& args] (modify #(apply add-edges % args)))
(defn state-add-attr [& args] (modify #(apply add-attr % args)))


(defn mapM
  "Map a computation onto seqs and then monad thread the results in order (sequence-m)"
  [comp & seqs] (sequence-m (apply map comp seqs)))


(defn reduce-graph [f ir-graph node]
  (let [arg-map (persistent!
                  (reduce
                    (fn [m x]
                      (reduce
                        #(assoc! %1 %2 (conj (get %1 %2 #{}) x))
                        m
                        (.-args x)))
                    (transient {})
                    ir-graph))
        successors (fn [node]
                      (apply setlib/union (map arg-map (.-return node))))]
    (loop [currents [node]
           nil
           visited #{node}]
      (let [all-succs (apply setlib/union (map #(successors %) currents))
            new (setlib/difference all-succs visited)
            new-visited (setlib/union new visited)
            [new-succs new-acc] (f new acc)]
        (if (empty? new-succs)
          new-acc
          (recur new-succs
                 new-acc
                 new-visited))))))


(defn get-subsequent-where [keep discard ir-graph node]
  (reduce-graph
    (fn [acc all-succs]
      (let [filtered (remove discard all-succs)
            partitioned (partition-by keep filtered)
            [algo-ins others] (case (count partitioned)
                                0 [[] []]
                                1 (if (keep (first (first partitioned)))
                                    [(first partitioned) []]
                                    [[] (first partitioned)])
                                2 partitioned
                                (throw (Exception. "Partitioning returned unexpected amount of values")))
            new-acc (concat acc algo-ins)]
        [new-acc others])
    ir-graph node)))


(def get-subsequent-algo-ins (partial get-subsequent-where is-algo-in? is-algo-out?))


(defn mk-algo-graph [{ir-graph :graph label-map :ctxt-map}]
  (let [algos (set (mapcat (partial filter is-algo?) (vals label-map)))
        algo-graph (apply add-nodes (digraph) algos)
        algo-op-id-map (apply hash-map (mapcat (fn [algo] [(:op-id algo) algo]) algos))]
    (st/run-state
      (mdo
        let algo-outs = (filter is-algo-out? ir-graph)
        (mapM
          (fn [algo-out-node]
            (let [algo-ctx (first (label-map (.-id algo-out-node)))
                  subsequent-algo-ins (get-subsequent-algo-ins ir-graph algo-out-node)]
              (mapM
                (fn [algo-in-node]
                  (state-add-edges [algo-ctx (algo-op-id-map (.-id algo-in-node))])
                subsequent-algo-ins)))
          algo-outs))
        )

      algo-graph)))


(defn does-rw [gr label-map algo-in]
  (reduce-graph
    (fn [acc_ succs]
      (let [acc (if (nil? acc_) {:does-read false :does-write false} acc_)
            does-read (or (:does-read acc) (some is-read? succs))
            does-write (or (:does-write acc) (some is-write? succs))
            new-succs (filter #(and (is-algo-out? %) (= (:op-id (first (label-map %))) (.-id algo-in))))
            new-acc {:does-read does-read :does-write does-write}]
        (if (and does-write does-read)
          [[] new-acc]
          [new-succs new-acc])))
    gr algo-in))


(defn- get-reads-and-writes [ir]
  (let [all (group-by :name
              (filter (comp #{'yauhau.function/write 'yauhau.functions/fetch 'fetch 'write} :name) ir))]
    [(concat (all 'yauhau.functions/fetch) (all 'fetch))
     (concat (all 'yauhau.functions/write) (all 'write))]))


(defn label-reads-and-writes [ir label-map]
  (let [[reads writes] (get-reads-and-writes)
        associated-algos (fn [node] (filter #(= com.ohua.lang/algo-in (:type %)) (label-map (.-id node))))
        does-read? (set (mapcat associated-algos reads))
        does-write? (set (mapcat associated-algos writes))]
    (mdo
      (mapM
        (fn [n]
          (state-add-label n :does-read (does-read? n))
          (state-add-label n :does-write (does-write? n))))
        (nodes s))
      )))


(defn side-effect-transform [{ir-graph :graph label-map :ctxt-map}]
  (st/run-state
    (mdo
      (label-reads-and-writes ir-graph label-map)
      )
    (mk-algo-graph)))
