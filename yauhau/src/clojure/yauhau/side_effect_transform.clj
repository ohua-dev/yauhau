(ns yauhau.side-effect-transform
  (:require [com.ohua.ir :as ir]
            [loom.graph :refer [digraph]]
            [loom.attr :refer [add-attr]]
            [monads.core :refer [mdo return modify >>= get-state put-state]]
            [monads.state :as st]))


(defn is-algo? [ctx] (= 'com.ohua.lang/algo-in (:type %)))
(defn is-algo-out? [func] (= 'com.ohua.lang/algo-out (.-name func)))
(defn is-algo-in? [func] (= 'com.ohua.lang/algo-in (.-name func)))
(def is-read? (ir/fn-name-in #{'fetch 'yauhau.functions/fetch}))
(def is-write? (ir/fn-name-in #{'write 'yauhau.functions/write}))


(defn state-add-edges [& args] (modify #(apply add-edges % args)))
(defn state-add-label [& args] (modify #(apply add-label % args)))


(defn mapM
  "Map a computation onto seqs and then monad thread the results in order (sequence-m)"
  [comp & seqs] (sequence-m (apply map comp seqs)))


(defn ir-to-loom-graph [ir]
  (apply add-edges
    (apply add-nodes (digraph) ir)
    (mapcat (fn [node] (map (fn [succ] [node succ]) (ir/successors node))) ir)))


(defn reduce-graph [f ir-graph node]
  (loop [currents [node]
         nil
         visited #{node}]
    (let [all-succs (set (mapcat #(ir/successors ir-graph %) currents))
          new (setlib/difference all-succs visited)
          new-visited (setlib/union new visited)
          [new-succs new-acc] (f new acc)]
      (if (empty? new-succs)
        new-acc
        (recur new-succs
               new-acc
               new-visited)))))


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
    (run-state
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


(defn label-reads-and-writes [ir label-map]
  (mdo
    s <- get-state
    let mapped-algos = (->> ir
                            (filter is-algo-in?)
                            (mapcat (fn [node] [(:id node) node])
                            (apply hash-map)))

    (mapM
      (fn [algo]
        (let [{does-read :does-read does-write :does-write} (does-rw ir label-map (mapped-algos (:op-id algo)))]
          (mdo
            (state-add-label algo :does-read does-read)
            (state-add-label algo :does-write does-write))))
      (nodes s))))


(defn side-effect-transform [{ir-graph :graph label-map :ctxt-map}]
  (run-state
    (mdo
      (label-reads-and-writes ir-graph label-map)
      )
    (mk-algo-graph)))
