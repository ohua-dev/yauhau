;
; Copyright (c) Justus Adam, Sebastian Ertel, Andres Goens 2016. All Rights Reserved.
;
; This source code is licensed under the terms described in the associated LICENSE file.
;

(ns yauhau.ir-transform
  (:require [com.ohua.ir :as ir :refer [fn-name-in fn-name-is]]
            [yauhau.accumulator :as acc]
            [clojure.set :as setlib]
            [clojure.set :as set]
            [com.ohua.util.visual :as visual]
            [com.ohua.context :as ctxlib :refer [->LabeledFunction]]
            [monads.core :refer [mdo return modify >>= get-state put-state]]
            [monads.state :as st]
            [monads.util :as mutil :refer [sequence-m fold-m lift-m*]]
            [clojure.algo.generic.functor :refer [fmap]]
            [com.ohua.logging :as l]
            [clojure.string :as string]
            [clojure.pprint :as pprint]
            [yauhau.util.loom :refer [ir-subgr-to-loom]]
            [loom.alg :refer [topsort]]
            [loom.io])
  (:import (com.ohua.ir IRFunc IRGraphPosition)
           (com.ohua.context LabeledFunction IFStackEntry SmapStackEntry)
           (clojure.lang PersistentArrayMap PersistentVector)
           (com.ohua.context LabeledFunction))
  (:use com.ohua.util.ir
        com.ohua.util.assert
        com.ohua.util.visual))

(defn trace [thing]
  (println thing)
  thing)


(def highlight-graph-parts (atom (constantly false)))


(defn trace-with [msg thing]
  (l/printline msg thing)
  thing)


(defn assert-not-nil
  ([thing] (assert (not (nil? thing))) thing)
  ([thing message]
   (assert (not (nil? thing)) message)
   thing))


(def ENABLE_VISUAL_GRAPH_TRANSFORMATION_LOGGING (atom false))

;; RECOGNIZE SPECIFIC FUNCTIONS
(def is-smap? (partial fn-name-is 'com.ohua.lang/smap))
(def is-one-to-n? (partial fn-name-is 'com.ohua.lang/one-to-n))
(def is-collect? (partial fn-name-is 'com.ohua.lang/collect))
(def is-fetch? (fn-name-in #{'fetch 'yauhau.functions/fetch}))
(def is-ite? (partial fn-name-is 'com.ohua.lang/ifThenElse))
(def is-select? (partial fn-name-is 'com.ohua.lang/select))
(def is-id-op? (partial fn-name-is 'yauhau.functions/identity))
(def is-packager? (partial fn-name-is 'yauhau.functions/__packageArgs))


;; CREATE SPECIFIC FUNCTIONS
(defn mk-func-maker [name] (fn [in out] (ir/mk-func name (into [] in) (if (seq? out) (into [] out) out))))
(def mk-select (mk-func-maker 'com.ohua.lang/select))
(def mk-fetch (mk-func-maker 'yauhau.functions/fetch))
(def mk-id-op (mk-func-maker 'yauhau.functions/identity))
(defn mk-empty-request [in out] (ir/mk-func 'yauhau.functions/__empty-request [in '__constDataSource] out))
(defn mk-const-nil [in out] (ir/mk-func 'yauhau.functions/__const-null in out))


(defmulti fn-to-id type)
(defmethod fn-to-id LabeledFunction [f] (.-id (.-function f)))
(defmethod fn-to-id IRFunc [f] (.-id f))
(defmethod fn-to-id Long [id] id)


(defn- next-var-name [name-gen]
  (loop [[name & new :as state] @name-gen]
    (if (compare-and-set! name-gen state new)
      (symbol name)
      (recur name-gen))))


(defn mk-name-gen-func
  "Create a function which every time it is called returns a new name unique in
  ir-graph"
  [ir-graph] (partial next-var-name (atom (ir/name-gen-from-graph ir-graph))))


(defn mapM
  "Map a computation onto seqs and then monad thread the results in order (sequence-m)"
  [comp & seqs] (sequence-m (apply map comp seqs)))

(defn dissoc-many
  "(Efficiently) remove all values placed at provided keys in the map provided"
  [map keys] (persistent! (reduce dissoc! (transient map) keys)))


(defn reindex-preserving
  "Reindex some meta value of each in collection 'thing' preserving previous
  values"
  [target thing]
  (first
    (st/run-state
      (mapM
        (fn [item]
          (let [prev-index (get (meta item) target)]
            (if (nil? prev-index)
              (mdo
                index <- get-state
                (modify inc)
                (return (vary-meta item assoc target index)))
              (mdo
                (put-state (inc prev-index))
                (return item)))))
        thing)
      0)))
(defn mk-func-and-index
  "Create an IR function with given parameters and also index the arguments and
  returns"
  [id name args return]
  (ir/->IRFunc id name
               (into [] (ir/reindex-stuff :in-idx args))
               (if (seq? return)
                 (into [] (ir/reindex-stuff :out-idx return))
                 (vary-meta return assoc :out-idx -1))))


(defn distinct-by
  "Select all elements of coll for which the provided function returns a
  distinct value"
  [f coll]
   (let [step (fn step [xs seen]
                (lazy-seq
                  ((fn [[elem :as xs] seen]
                     (when-let [s (seq xs)]
                       (if (contains? seen (f elem))
                         (recur (rest s) seen)
                         (cons elem (step (rest s) (conj seen (f elem)))))))
                    xs seen)))]
     (step coll #{})))


(def canonicalize-label-key fn-to-id)
(defn gets [accessor] (fmap accessor get-state))
(def get-name-gen (gets :name-gen))
(def state-mk-name (fmap (fn [a] (a)) get-name-gen))
(def state-get-label-map (gets :label-map))
(defn state-put-label
  "Place label at key in the label map saved in a composite underlying state"
  [key label]
  (modify #(assoc-in % [:label-map (canonicalize-label-key key)] label)))
(defn unsafe-modify-graph
  "this is unsafe because it only modifies the graph and not the label map"
  [f] (modify #(update % :graph f)))
(def state-gen-id
  (mdo
    [elem & gen] <- (fmap :id-gen get-state)
    (modify #(assoc % :id-gen gen))
    (return elem)))
(defn state-mk-func [name args return_] (fmap (fn [id] (mk-func-and-index id name args return_)) state-gen-id))
(defn state-mk-func-unindexed [name args return_] (fmap (fn [id] (ir/->IRFunc id name args return_)) state-gen-id))
(defn state-mk-names [n]
  (mdo
    name-gen <- (fmap :name-gen get-state)
    (return (into [] (repeatedly n name-gen)))))
(defn state-label-all-with [label fns]
  (modify (fn [state] (update state :label-map (partial merge (zipmap (map fn-to-id fns) (repeat label)))))))
(defn state-delete-label [key]
  (modify (fn [state] (update state :label-map #(dissoc % (canonicalize-label-key key))))))
(defn state-delete-labels [keys]
  (modify (fn [state] (update state :label-map #(dissoc-many % (map canonicalize-label-key keys))))))
(defn state-get-label [key]
  (fmap #(get % (canonicalize-label-key key)) state-get-label-map))
(def state-mk-select (partial state-mk-func "select")) ; TODO When select is fixed this function needs to be changed
(def state-mk-fetch (partial state-mk-func 'yauhau.functions/fetch))
(def state-mk-id (partial state-mk-func-unindexed 'yauhau.functions/identity))
(defn state-mk-empty-req [ctx-arg]
  (>>=
    state-mk-name
    (fn [empty-out]
      (state-mk-func-unindexed
        'yauhau.functions/__empty-request
        [(vary-meta ctx-arg assoc :in-idx -1)]
        empty-out))))
(defn state-delete-fn [func]
  (mdo
    (unsafe-modify-graph (partial ir/drop-node func))
    (state-delete-label func)))
(defn state-delete-fns [funcs]
  (mdo
    (unsafe-modify-graph (partial ir/drop-nodes funcs))
    (state-delete-labels funcs)))
(def get-graph (fmap :graph get-state))
(defn state-find-func [id] (fmap (fn [gr] (first (filter #(= id (.-id %)) gr))) get-graph))
(defn state-replace-node
  ([old new label]
   (mdo
     (state-delete-label old)
     (if (seq? new)
       (mdo
         (unsafe-modify-graph (partial ir/update-graph {old new}))
         (state-label-all-with label new))
       (mdo
         (unsafe-modify-graph (partial ir/replace-node old new))
         (state-put-label new label)))))
  ([old new]
   (mdo
     (state-delete-label old)
     (cond
       (not (seq? new)) (throw (Exception. "New nodes need labels"))
       (empty? new) (return nil)
       (not (seq? (first new))) (mdo
                                  let [new-node new-label] = new
                                  (unsafe-modify-graph (partial ir/replace-node old new-node))
                                  (state-put-label new-node new-label))
       :else (mdo
               (unsafe-modify-graph (partial ir/update-graph {old (map first new)}))
               (mapM (fn [[node label]] (state-put-label node label)) new))))))
(defn insert-after [node nodes graph]
  (let [nodes- (if (seq? nodes) nodes [nodes])
        [before [elem & after]] (split-with #(not= % node) graph)]
    (into [] (concat before (cons elem nodes) after))))

(defn topmost-if-frame [label-map target]
  (last (filter (fn [{t :type}] (= 'if t)) (label-map (fn-to-id target)))))


(defn- matching-if-frame [label-map if-fn target]
  (let [if-id (fn-to-id if-fn)
        {op-id :op-id :as top} (topmost-if-frame label-map target)]
    (cond
      (nil? top) nil
      (= if-id op-id) top
      :else (throw (RuntimeException. (str "Topmost if stackframe pointed to unexpected operator: " top))))))
(defn- state-matching-if-frame [if-fn target]
  (fmap (fn [map] (matching-if-frame map if-fn target)) state-get-label-map))


(defn get-largest-id
  "Obtain the largest ID present in the given ir-graph"
  [ir-graph] (apply max (remove nil? (map :id ir-graph))))


(defn serialize-unreached-nodes [{graph :ir visited :visited dependencies :dependencies}]
  (let [producers (ir/mk-producer-map graph)]
    ; (pprint/print-table producers)
    (for [{id :id name :name args :args} (remove visited graph)]
      {:id id
       :name name
       :unsatisfied-inputs
       (into [] (map (fn [i] [i (let [p (producers i)]
                                  (if (nil? p)
                                    {:id 'unknown :name 'missing}
                                    {:id (.-id p) :name (.-name p)}))]) (remove dependencies args)))
      })))


(defn find-next-fetches
  "docstring"
  [{graph :ir :as initial-graph-pos}]
  (loop [{visited :visited
          curr-nodes :curr-nodes
          :as graph-pos} initial-graph-pos
         fetch-nodes #{}]
    (if (empty? curr-nodes)
      [graph-pos fetch-nodes]
      (let [io-nodes (setlib/select is-fetch? curr-nodes)
            non-io-nodes (setlib/difference curr-nodes io-nodes)]
        (recur
          (ir/next-nodes-with (merge graph-pos {:curr-nodes non-io-nodes :visited (setlib/union visited io-nodes)}))
          (setlib/union fetch-nodes (set io-nodes)))))))


(def get-returns (partial mapcat ir/get-return-vars))


(defn wrap-smap-once [{op-id :op-id :as frame}
                      fn-id]
  (mdo
    smap-op <- (state-find-func op-id)
    graph <- get-graph
    let otn = (ir/get-producer (first (:args smap-op)) graph)
    let size-source = (vary-meta (first (.-args otn)) dissoc :in-idx)
    [collect-out
     fetch-out
     smap-in
     one-to-n-out
     new-fetch-in] <- (state-mk-names 5)
    {[f-fetch-arg & rest-fetch-args] :args
     output                          :return
     :as                             function} <- (state-find-func fn-id)
    graph <- get-graph
    label <- (state-get-label (assert-not-nil function (str "fn id" fn-id "\ngraph" graph)))
    rest-label <- (state-get-label op-id)
    let new-fetch = (-> function
                        (assoc :args (reindex-preserving :in-idx (cons new-fetch-in rest-fetch-args)))
                        (assoc :return (vary-meta fetch-out assoc :out-idx -1)))

    ; RFE optimisation
    ; In theory this operator is not necessary, we could simply reuse the
    ; output from 'otn', however Ohua currently raises an InvariantBroken
    ; exception if I do that. Therefore I must discuss with @serel whether
    ; we can abolish that invariant to reuse some results here.
    ; This would decrease the operator pressure significantly even for small graphs
    first-one-to-n <- (state-mk-func 'com.ohua.lang/one-to-n [size-source size-source] one-to-n-out)
    new-collect <- (state-mk-func 'com.ohua.lang/collect [one-to-n-out f-fetch-arg] collect-out)
    tree-builder <- (state-mk-func 'yauhau.functions/__mk-req-branch [collect-out] new-fetch-in)
    new-one-to-n <- (state-mk-func 'com.ohua.lang/one-to-n [size-source fetch-out] smap-in)
    new-smap <- (state-mk-func 'com.ohua.lang/smap-fun [smap-in] [output])

    let _ = (l/printline (meta size-source))
    _ = (l/printline (map meta (:args first-one-to-n)))

    ; update the label map
    (state-put-label first-one-to-n label)
    (state-label-all-with
      (if (nil? rest-label) [] rest-label)
      [new-collect tree-builder new-one-to-n new-smap new-fetch])
    (unsafe-modify-graph (partial ir/update-graph {function [first-one-to-n new-collect tree-builder new-fetch new-one-to-n new-smap]}))))



(defn wrap-smap [{op-id :op-id :as frame}]
  (mdo
    ir-graph <- get-graph
    label-map <- state-get-label-map
    let fetches-concerned = (->> ir-graph
                                 (map (fn [a] [a (label-map (.-id a))]))
                                 (filter #(and (is-fetch? (first %)) (not (empty? (second %)))))
                                 (filter #(some (partial = frame) (second %)))
                                 (map (comp :id first)))
    _ = (l/printline "Concerned fetches" fetches-concerned)
    (mapM (partial wrap-smap-once frame) fetches-concerned)))


(defn cat-redundant-smap-collects
  ; TODO implement
  [ir-graph]
  ir-graph)

(defn batch-rewrite
  "docstring"
  [{ir-graph :graph env-args :env-args :as df-ir}]
  (let [pos0 (ir/->IRGraphPosition ir-graph (set (filter #(every? env-args (:args %)) ir-graph)) #{} env-args)]
    (loop [position (assoc pos0 :dependencies (:env-args df-ir))
          graph ir-graph
          new-id (inc (get-largest-id ir-graph))]
      (let [[{visited :visited
              satisfied :dependencies
              :as new-position} fetch-nodes] (find-next-fetches position)]
        (if (empty? fetch-nodes)
          (do
            (when (not-every? visited graph)
              (let [unvisited (remove visited ir-graph)
                    uncreated (set (mapcat ir/get-return-vars unvisited))
                    missing (remove #(or (satisfied %) (uncreated %)) (mapcat :args unvisited))
                    producers (apply hash-map (mapcat #(mapcat vector (ir/get-return-vars %) (repeat %)) ir-graph))]
                (println "Visited")
                (pprint/print-table visited)
                (println "Unvisited")
                (pprint/print-table unvisited)
                (println "Missing data")
                (println missing)
                (println "Missing producers")
                (visual/render-to-file "cycle" unvisited)
                (pprint/print-table (map (fn [m] (let [p (producers m)] {:missing m :id (:id p) :name (:name p)})) missing))
                (System/exit 1)))
            (assoc df-ir :graph
                        (if (vector? graph)
                          graph
                          (into [] graph))))
          (let [inputs (ir/reindex-stuff :in-idx (mapcat :args fetch-nodes))
                ; TODO we might have to insert identity operators after the outputs too
                ; since the fetch operator might have destructured return
                outputs (ir/reindex-stuff :out-idx (mapcat ir/get-return-vars fetch-nodes))
                accum (mk-func-and-index new-id 'yauhau.functions/__accum-fetch inputs outputs)
                fetches-removed (remove (set fetch-nodes) graph)
                new-graph (into [] (conj fetches-removed accum))
                new-dependencies (setlib/union (.-dependencies new-position) (set outputs))
                new-visited (conj visited accum)]
            (recur
              (ir/->IRGraphPosition
                new-graph
                (ir/next-satisfied new-graph new-dependencies new-visited)
                new-visited
                new-dependencies)
              new-graph
              (inc new-id))))))))


(defn- gen-empty-for [amount in]
  (mdo
    req <- (state-mk-empty-req in)
    let req-out = (.-return req)
    inserts <- (sequence-m
                  (repeatedly
                    amount
                    (fn [] (>>= state-mk-name (partial state-mk-fetch [req-out])))))
    (return (cons req inserts))))


(defn- get-fetches-concerned [curr-if]
  (mdo
    {graph :graph label-map :label-map} <- get-state
    (return
      (filter
        (fn [func]
          (and
            (is-fetch? func)
            (let [found-if (topmost-if-frame label-map func)]
              (= (.-id (.-function curr-if)) (:op-id found-if)))))
        graph))))

; FIXME
; This algorithm assumes that fetches-concerned is a sequence of fetches
; in order as they would be called in the program.
; If we were to impose this condition on fetches-concerned the function get-fetches-concerned
; would have to be altered. Currently get-fetches-concerned returns the fetches in call order
; because the IR contains them in call order. However this property is not strongly enforced
; because the translation from IR to Operators can be done on an IR which is not in call order.
; There is even a known point where this property breaks, which is when the accumulator is inserted.
; Currently every accumulator is inserted at the top of the IR *every time* breaking call order.
; Regardles of whether the fetches-concerned have correct order or not though
; independant fetches in branches are not correcly handled.
; They should be merged rather than linearized.
(defn- mapped-fetches-for-if
  ([oid]
    (mdo
      graph <- get-graph
      label-map <- state-get-label-map
      let get-label = #(label-map (fn-to-id %))
      let mapped = (->> graph
                     (filter (fn [n] (some #(= oid (:op-id %)) (get-label n))))
                     (group-by (fn [n] (:out-var (first (filter #(= oid (:op-id %)) (get-label n)))))))
      (return (into {} (map (fn [[key fns]] [key
                                             (->> fns
                                                  ir-subgr-to-loom
                                                  topsort
                                                  (filter is-fetch?))]) mapped)))))
  ([curr-if _] (mapped-fetches-for-if curr-if)))

(defn- insert-empties [{if-op :function :as labeled-if} fetches-concerned]
  (mdo
    let _ = (assert-type LabeledFunction labeled-if (str "If op has incorrent type <" (type labeled-if) ">"))
    mapped-to-port <- (mapped-fetches-for-if (.-id (.-function labeled-if)) fetches-concerned)
    let [[short-port short-fetches] [long-port long-fetches]] = (sort-by (comp count second) (seq mapped-to-port))
    let to-gen = (- (count long-fetches) (count short-fetches))

    (if (= 0 to-gen)
      (return nil)
      (mdo
        root-if-stack <- (state-get-label if-op)
        last-stack-entry <- (state-matching-if-frame if-op (first long-fetches))
        let short-port-binding = ((.-return if-op) short-port)
        empties <- (gen-empty-for to-gen short-port-binding)
        let if-stack = (into [] (conj root-if-stack (assoc last-stack-entry :out-var short-port)))
        (state-label-all-with if-stack empties)
        (unsafe-modify-graph (partial insert-after if-op empties))
        gr <- get-graph
        let _ = (visual/render-to-file "after-insert-empty" gr @highlight-graph-parts)
        (return nil)))))


(def ^:private state-get-if-handled-fns (fmap (comp :handled-functions :if-rewrite-private-state) get-state))
(defn- state-add-if-handled-fns [fns]
  (modify (fn [state] (update-in state [:if-rewrite-private-state :handled-functions] (partial setlib/union (set (map fn-to-id fns)))))))
(def ^:private state-init-if-handled-fns
  (fmap
    (fn [state]
      (if (:handled-functions (:if-rewrite-private-state state))
        state
        (assoc-in state [:if-rewrite-private-state :handled-functions] #{})))
    get-state))
(defn state-is-fn-if-handled? [fn]
  (fmap #(contains? % fn) state-get-if-handled-fns))


(defn state-pop-matching-if-frame [if-fn target]
  (mdo
    label-map <- (gets :label-map)
    old-label <- (state-get-label target)
    let frame = (matching-if-frame label-map if-fn target)
    _ = (assert (some #(= frame %) old-label))
    (return (into [] (remove #(= frame %) old-label)))))


(defn if-rewrite-one [{if-op :op-id}]
  (mdo
    label <- (state-get-label (assert-not-nil if-op "387"))
    curr-if <- (state-find-func if-op)
    let labeled-if = (ctxlib/->LabeledFunction curr-if label)
    direct-fetches-concerned <- (get-fetches-concerned labeled-if)
    (state-add-if-handled-fns direct-fetches-concerned)
    let _ = (assert (not (nil? curr-if)) "If op not present in graph")
    (insert-empties labeled-if direct-fetches-concerned)
    let _ = (assert-type LabeledFunction labeled-if)
    mapped-to-port <- (mapped-fetches-for-if if-op)
    let fetches = [(mapped-to-port 0) (mapped-to-port 1)]
    _ = (l/printline "Mapped to port" mapped-to-port)
    if-stack = (.-label labeled-if)
    (apply mapM
           (fn [& parallel-fetches]
             (mdo
               let _ = (l/printline "Parallel fetches" parallel-fetches)
               let [head-fetch & other-fetches] = parallel-fetches
               new-label <- (state-pop-matching-if-frame if-op head-fetch)
               let all-inputs = (mapcat :args parallel-fetches)
               [merge-out fetch-out] <- (state-mk-names 2)
               merge-fn <- (state-mk-select (into [] (cons ((.-return curr-if) 0) all-inputs))
                                            merge-out)
               new-fetch <- (state-mk-fetch [merge-out] fetch-out)
               identity-ops <- (mapM
                                 (fn [{ret :return :as fun}]
                                   (mdo
                                     frame <- (state-matching-if-frame if-op fun)
                                     (state-mk-id [(vary-meta ((.-return curr-if) (:out-var frame)) assoc :in-idx -1)
                                                   (vary-meta fetch-out assoc :in-idx 0)]
                                                  (if (seq? ret)
                                                    (ir/reindex-stuff :out-idx ret)
                                                    (vary-meta ret assoc :out-idx -1)))))
                                 parallel-fetches)
               let all-fns = (concat [merge-fn new-fetch] identity-ops)
               (state-delete-fns other-fetches)
               (state-replace-node head-fetch all-fns new-label)
               (return nil)))
           fetches)
    (fmap (comp (partial assert-coll-of-type IRFunc) :graph) get-state)
    (return nil)))


(defn insert-leaf-builders [ir-graph]
  (let [name-gen (mk-name-gen-func ir-graph)]
    (into []
          (ir/update-nodes-where
            is-fetch?
            (fn [node]
              (let [tree-builder-out (name-gen)]
                [(ir/mk-func "__mkReqTreeLeaf" (.-args node) tree-builder-out)
                 (assoc node :args [tree-builder-out])]
                [node]))
            ir-graph))))


(defn cat-redundant-merges [ir-graph]
  (reduce
    (fn [graph merge-node]
      (let [predec (ir/predecessors merge-node graph)
            fetch-res (first (apply set/intersection (map (comp set :args) predec)))]
        (if (and (every? is-id-op? predec) (not (nil? fetch-res)))
          (let [fetch (ir/get-producer fetch-res graph)]
            (->> graph
                 (ir/replace-node fetch (assoc fetch :return (:return merge-node)))
                 (ir/drop-nodes predec)
                 (ir/drop-node merge-node)))
          graph)))
    ir-graph
    (filter is-select? ir-graph)))


(defn cat-redundant-identities [ir-graph]
  (reduce
    (fn [graph id-op]
      (if (not (coll? (.-return id-op)))
        (let [id-in (second (.-args id-op))
              id-out (.-return id-op)]
          (->> graph
               (ir/drop-node id-op)
               (ir/change-nodes-where
                 (fn [n] (not (empty? (filter (partial = id-out) (.-args n)))))
                 (fn [n] (update-in n [:args] (fn [args] (into [] (replace {id-out id-in} args))))))))
        graph))
    ir-graph
    (filter is-id-op? ir-graph)))


(defn cat-identities-with-no-successor [ir-graph]
  (loop [graph ir-graph]
    (let [red-identities (filter (fn [node] (and (is-id-op? node) (empty? (ir/successors node graph)))) graph)]
      (if (empty? red-identities)
        graph
        (recur (ir/drop-nodes red-identities graph))))))


(defn find-bottom-merge [merge graph]
  (loop [merge merge]
    (let [successors (ir/successors merge graph)
          more-bottom (first successors)]
      (if (and (not (empty? successors)) (is-select? more-bottom) (= 1 (count (ir/successors more-bottom graph))))
        (recur more-bottom)
        merge))))


(defn find-upstream-fns-and-merges [merge graph]
  (let [predecessors (set (ir/predecessors merge graph))
        merges (set/select is-select? predecessors)
        non-merges (set/select (comp not is-select?) predecessors)]

    (reduce
      (fn [[merges non-merges] [new-merges new-non-merges]]
        [(set/union merges new-merges)
         (set/union non-merges new-non-merges)])
      [merges non-merges]
      (map #(find-upstream-fns-and-merges % graph) merges))))


(defn coerce-merges [ir-graph]
  (let [all-merges (set (filter is-select? ir-graph))]
    (loop [merges all-merges
           graph ir-graph]
      (if (empty? merges)
        graph
        (let [curr-merge (first merges)
              bottom-merge (find-bottom-merge curr-merge graph)
              [clustered-merges non-merges] (find-upstream-fns-and-merges bottom-merge graph)
              clustered-merges-w-bott (conj clustered-merges bottom-merge)
              non-merge-ouputs (set (mapcat ir/get-return-vars non-merges))
              new-merge-inputs (set/intersection non-merge-ouputs (set (mapcat :args clustered-merges-w-bott)))
              graph (->> graph
                         (ir/replace-node bottom-merge (assoc bottom-merge :args (into [] (ir/reindex-stuff :in-idx new-merge-inputs))))
                         (ir/drop-nodes clustered-merges))]
          (recur
            (set/difference merges clustered-merges-w-bott)
            graph))))))


(defrecord Rewrite [rewrite clean])


(def transformation-map
  ; TODO add actual clean functions
  {'if                     (->Rewrite if-rewrite-one return)
   'com.ohua.lang/smap-fun (->Rewrite wrap-smap return)})


(defn- trans-map-to-trigger-map [trans-map]
  (apply hash-map
         (mapcat
           (fn [[ctx rewrite]]
             (mapcat
               (fn [marker] [marker rewrite])
               (.-begin-markers ctx)))
           trans-map)))


(defn- unwind-context- [trigger-map {ir-graph :graph label-map :ctxt-map :as df-ir}]
  (let [name-gen (mk-name-gen-func ir-graph)
        id-gen (iterate inc (inc (get-largest-id ir-graph)))

        _ (l/printline label-map)
        _ (assert-map label-map)

        fetches-in-context (->> ir-graph
                                (map (fn [a] [a (label-map (.-id a))]))
                                (filter #(and (is-fetch? (first %)) (not (empty? (second %))))))
        ;fetches-to-context
        ;(persistent!
        ;  (second
        ;    (st/run-state
        ;      (mapM
        ;        (fn [[fetch contexts]]
        ;          (sequence-m
        ;            (for [frame contexts]
        ;              (modify #(assoc! % frame (conj (% frame) (.-id fetch)))))))
        ;        fetches-in-context)
        ;      (transient {}))))

        label-stacks (->> fetches-in-context
                          (map second)
                          (distinct))
        working-order (->> (loop [stacks label-stacks
                                  order (transient [])]
                             (let [new-order (conj! order (map first stacks))
                                   new-stacks (filter (comp not empty?) (map rest stacks))]
                               (if (empty? new-stacks)
                                 new-order
                                 (recur new-stacks new-order))))
                           (persistent!)
                           (apply concat)
                           (reverse)
                           (distinct-by #(select-keys % [:type :op-id])))
        _ (do
            (l/printline "Working order" working-order))
        [_
         {new-graph  :graph
          new-labels :label-map}]
        (st/run-state
          (mapM
            (fn [stackframe]
              (l/printline "Current frame" stackframe)
              (let [{rewrite-fn  :rewrite
                     cleaning-fn :clean
                     :as         rewrite} (trigger-map (:type stackframe))]
                (mdo
                  (if (nil? rewrite)
                    (return nil)
                    (>>= (rewrite-fn stackframe) cleaning-fn))
                  graph <- get-graph
                  let _ = (l/printline "Graph after rewriting" (:type stackframe) (:op-id stackframe))
                  _ = (l/log-graph graph)
                  (return nil))))
            working-order)
          {:graph     ir-graph
           :label-map label-map
           :name-gen  name-gen
           :id-gen    id-gen})]
    (assoc df-ir :graph new-graph :label-map new-labels)))


(def context-rewrite-with unwind-context-)


(defn gen-ids [{ir-graph :graph :as ir}]
  (let [id-gen (iterate inc (inc (get-largest-id ir-graph)))
        new-graph (first
                    (st/run-state
                      (mapM
                        (fn [func]
                          (if (nil? (.-id func))
                            (fmap #(assoc func :id %) state-gen-id)
                            (return func)))
                        ir-graph)
                      {:id-gen id-gen}))]
    (assoc ir :graph new-graph)))


(defn context-rewrite [graph]
  (unwind-context- transformation-map graph))


(defn- log-transformation-at [name]
  (fn [graph]
    (if @ENABLE_VISUAL_GRAPH_TRANSFORMATION_LOGGING
      (visual/render-to-file name graph))
    graph))


(defn- validate-and-log [name]
  (comp
    (fn [graph] (ir/validate graph (str "after " name)))
    (log-transformation-at name)))

(defn is-env? [arg]
  (and (seq? arg)
       (let [[fn_ empty-vec] arg]
         (and (= 'fn fn_)
              (= [] empty-vec)))))

(defn assoc-env-args [{graph :graph :as g}]
  (let [env-args (set (mapcat #(filter is-env? (:args %)) graph))]
    (assoc g :env-args env-args)))


(def transformations
  [
   assoc-env-args
   (fn [{graph :graph :as g}]
     (l/printline "Before transformations")
     (visual/render-to-file "before-trans" graph @highlight-graph-parts)
     (l/log-graph graph)
     g)
  gen-ids
  context-rewrite
  (fn [{graph :graph :as g}]
    (visual/render-to-file "after-ctx" graph @highlight-graph-parts)
    g)
  (validate-and-log "context-rewrite")
  batch-rewrite
  (validate-and-log "batch-rewrite")
  ; (fn [{graph :graph :as g}]
  ;   (l/printline "Applied transformations")
  ;   (visual/render-to-file "after-trans" graph @highlight-graph-parts)
  ;   (l/log-graph graph)
  ;   g)
  ])


(def full-transform (apply comp (reverse transformations)))
