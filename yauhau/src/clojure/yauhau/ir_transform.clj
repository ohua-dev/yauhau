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
            [clojure.pprint :refer [pprint]]
            [com.ohua.util.visual :as visual]
            [com.ohua.context :as ctxlib :refer [->LabeledFunction]]
            [monads.core :refer [mdo return modify >>= get-state]]
            [monads.state :as st]
            [monads.util :as mutil :refer [sequence-m]]
            [clojure.algo.generic.functor :refer [fmap]]
            [clojure.pprint :as pprint])
  (:import (com.ohua.ir IRFunc IRGraphPosition)
           (com.ohua.context LabeledFunction IFStackEntry SmapStackEntry)
           (clojure.lang PersistentArrayMap)
           (com.ohua.context LabeledFunction))
  (:use com.ohua.util.ir
        com.ohua.util.assert
        com.ohua.util.visual))

(defn trace [thing]
  (println thing)
  thing)


(def ENABLE_VISUAL_GRAPH_TRANSFORMATION_LOGGING (atom false))

;; RECOGNIZE SPECIFIC FUNCTIONS
(def is-smap? (partial fn-name-is 'com.ohua.lang/smap))
(def is-one-to-n? (partial fn-name-is 'com.ohua.lang/one-to-n))
(def is-collect? (partial fn-name-is 'com.ohua.lang/collect))
(def is-fetch? (partial fn-name-is 'yauhau.functions/fetch))
(def is-ite? (partial fn-name-is 'com.ohua.lang/ifThenElse))
(def is-select? (partial fn-name-is 'com.ohua.lang/select))
(def is-id-op? (partial fn-name-is 'yauhau.functions/identity))
(def is-packager? (partial fn-name-is 'yauhau.functions/__packageArgs))


;; CREATE SPECIFIC FUNCTIONS
(defn mk-func-maker [name] (fn [in out] (ir/mk-func name (into [] in) (if (seq? out) (into [] out) out))))
(def mk-select (mk-func-maker 'com.ohua.lang/select))
(def mk-fetch (mk-func-maker 'yauhau.functions/fetch))
(def mk-id-op (mk-func-maker 'yauhau.functions/identity))
(defn mk-empty-request [in out] (ir/mk-func 'yauhau.functions/__emptyRequest [in '__constDataSource] out))
(defn mk-const-nil [in out] (ir/mk-func 'yauhau.functions/__constNull in out))


(defn- next-var-name [name-gen]
  (loop [[name & new :as state] @name-gen]
    (if (compare-and-set! name-gen state new)
      (symbol name)
      (recur name-gen))))


(defn mk-name-gen-func [ir-graph] (partial next-var-name (atom (ir/name-gen-from-graph ir-graph))))


(defn mapM [comp seq] (sequence-m (map comp seq)))

(defn dissoc-many [map keys] (persistent! (reduce dissoc! (transient map) keys)))


(defn- canonicalize-label-key [key] (if (= (type key) IRFunc) (:id key) key))
(defn state-mk-name [] (fmap (comp apply :name-gen) get-state))
(def get-label-map (fmap :label-map get-state))
(defn state-put-label [key label]
  (modify #(assoc-in % [:label-map (canonicalize-label-key key)] label)))
(defn unsafe-modify-graph
  "this is unsafe because it only modifies the graph and not the label map"
  [f] (modify #(update % :graph f)))
(def get-name-gen (fmap :name-gen get-state))
(defn state-gen-id []
  (mdo
    [elem & gen] <- (fmap :id-gen get-state)
    (modify #(assoc % :id-gen gen))
    (return elem)))
(defn state-mk-func [name args return_] (fmap (fn [id] (ir/->IRFunc id name args return_)) (state-gen-id)))
(defn state-mk-names [n]
  (mdo
    name-gen <- (fmap :name-gen get-state)
    (return (into [] (repeatedly n name-gen)))))
(defn state-label-all-with [label fns]
  (mapM #(state-put-label % label) fns))
(defn state-delete-label [key]
  (modify (fn [state] (update state :label-map #(dissoc % (canonicalize-label-key key))))))
(defn state-get-label [key]
  (fmap #(get % (canonicalize-label-key key)) get-label-map))
(def state-mk-merge (partial state-mk-func 'com.ohua.lang/merge))
(def state-mk-fetch (partial state-mk-func 'yauhau.functions/fetch))
(def state-mk-id (partial state-mk-func 'yauhau.functions/identity))
(defn state-delete-fn [func]
  (mdo
    (unsafe-modify-graph (partial ir/drop-node func))
    (state-delete-label func)))
(defn state-delete-fns [funcs]
  (mdo
    (unsafe-modify-graph (partial ir/drop-nodes funcs))
    (modify (partial update :label-map (fn [lmap] (dissoc-many lmap (map canonicalize-label-key funcs)))))))
(def get-graph (fmap :graph get-state))
(defn state-find-func [id] (fmap (partial filter (comp (partial = id) :id)) get-graph))



(defn find-next-fetches
  "docstring"
  [graph-pos]
  (loop [graph-pos graph-pos
         fetch-nodes #{}]
    (let [curr-nodes (.-curr_nodes graph-pos)]
      (if (empty? curr-nodes)
        [graph-pos fetch-nodes]
        (let [io-nodes (setlib/select is-fetch? curr-nodes)
              non-io-nodes (setlib/difference curr-nodes io-nodes)]
          (recur
            (ir/next-nodes-with (->
                                  graph-pos
                                  (assoc :visited (setlib/union (.-visited graph-pos) io-nodes))
                                  (assoc :curr-nodes non-io-nodes)))
            (setlib/union fetch-nodes (set io-nodes))))))))


(def get-returns (partial mapcat ir/get-return-vars))


(defn wrap-smap-once [{size-op :size-op}
                      fn-id]
  (mdo
    [collect-out
     fetch-out
     smap-in
     one-to-n-out
     new-fetch-in] <- (state-mk-names 5)
    [{[f-fetch-arg & rest-fetch-args] :args
     output     :return
     :as        function}] <- (state-find-func fn-id)
    [_ & rest-label :as label] <- (state-get-label function)

    let new-fetch = (-> function
                        (assoc :args (into [] (cons new-fetch-in rest-fetch-args)))
                        (assoc :return fetch-out))

    first-one-to-n <- (state-mk-func 'com.ohua.lang/one-to-n [size-op size-op] one-to-n-out)
    new-collect <- (state-mk-func 'com.ohua.lang/collect [one-to-n-out f-fetch-arg] collect-out)
    tree-builder <- (state-mk-func 'yauhau.functions/__mkReqTreeBranch [collect-out] new-fetch-in)
    new-one-to-n <- (state-mk-func 'com.ohua.lang/one-to-n [size-op fetch-out] smap-in)
    new-smap <- (state-mk-func 'com.ohua.lang/smap-fun [smap-in] output)

    ; update the label map
    (state-label-all-with label [first-one-to-n])
    (state-label-all-with
      (if (nil? rest-label) [] rest-label)
      [new-collect tree-builder new-one-to-n new-smap function])
    (state-delete-label function)
    (unsafe-modify-graph (partial ir/update-graph {function [first-one-to-n new-collect tree-builder new-fetch new-one-to-n new-smap]}))))


(defn cat-redundant-smap-collects
  ; TODO implement
  [ir-graph]
  ir-graph)


(defn batch-rewrite
  "docstring"
  [ir-graph]
  (loop [position (ir/first-position ir-graph)
         graph ir-graph]
    (let [[new-position fetch-nodes] (find-next-fetches position)]
      (if (empty? fetch-nodes)
        (if (vector? graph)
          graph
          (into [] graph))
        (let [inputs (ir/reindex-stuff :in-idx (mapcat :args fetch-nodes))
              outputs (ir/reindex-stuff :out-idx (mapcat ir/get-return-vars fetch-nodes))
              accum (acc/mk-accum-op inputs outputs)
              fetches-removed (remove (set fetch-nodes) graph)
              new-graph (into [] (conj fetches-removed accum))
              new-dependencies (setlib/union (.dependencies new-position) (set outputs))
              new-visited (conj (:visited new-position) accum)]
          (recur
            (ir/->IRGraphPosition
              new-graph
              (ir/next-satisfied new-graph new-dependencies new-visited)
              new-visited
              new-dependencies)
            new-graph))))))


(defn- gen-empty-for [amount out name-gen]
  (first
    (reduce
      (fn [[acc out] _]
        (let [fetch-in (name-gen)
              req-in (name-gen)]
          [(->> acc
                (cons (mk-fetch [fetch-in] out))
                (cons (mk-empty-request req-in fetch-in)))
           req-in]))
      ['() out]
      (range amount))))


(defn- get-fetches-concerned [labeled-functions curr-if]
  (filter
    (fn [func]
      (and
        (is-fetch? (.-function func))
        (not (empty? (.-label func)))
        (let [found-if (.-if_op (peek (.-label func)))]
          (assert-type (type curr-if) found-if)
          (= curr-if found-if))))
    labeled-functions))


(defn- mapped-fetches-for-if [labeled-graph curr-if]
  (let [fetches-concerned (get-fetches-concerned labeled-graph curr-if)
        _ (doall (map (partial assert-type LabeledFunction) labeled-graph))
        _ (assert-type LabeledFunction curr-if)]
    (if-not (empty? fetches-concerned) (assert-type LabeledFunction (first fetches-concerned)))
    (merge
      (zipmap (ir/get-return-vars (.-function curr-if)) (repeat []))
      (group-by #(.-arg_id (peek (.-label %))) fetches-concerned))))


(defn- calc-empties [if-op]
  (mdo
    label-map <- get-label-map
    name-gen <- get-name-gen
    let mapped-to-port = (mapped-fetches-for-if (vals label-map) if-op)
    longest-fetch-seq = (apply max-key count (vals mapped-to-port))
    longest-nr = (count longest-fetch-seq)
    example-if-stack = (.-label (first longest-fetch-seq))
    empty-required = (remove (comp zero? (partial - longest-nr) count second) (seq mapped-to-port))

    (-> empty-required
        (map
          (fn [[if-port fetches]]
            (let [to-gen (- longest-nr (count fetches))]
              (assert-number to-gen)
              (let [no-fetches (zero? (count fetches))
                    out-var (if no-fetches (name-gen) (.-return (.-function (last fetches))))
                    if-stack (-> example-if-stack
                                 (pop)
                                 (conj (assoc (last example-if-stack) :arg-id if-port)))
                    generated (gen-empty-for to-gen out-var name-gen)
                    empties (map #(->LabeledFunction % if-stack) generated)
                    _ (assert-coll empties)
                    _ (assert-type IRFunc (.-function (first empties)))
                    in-var (first (.-args (.-function (first empties))))
                    _ (assert-type LabeledFunction if-op)
                    to-replace (if no-fetches if-op (last fetches))

                    replacement-head
                    (if no-fetches
                      [if-op (->LabeledFunction (mk-const-nil [^{:in-idx -1} if-port] (first (.-args (first generated)))) if-stack)]
                      [(assoc-in (last fetches) [:function :return] in-var)])]
                [to-replace (into [] (concat replacement-head empties))]))))
        (map-from-coll)
        (return))))


(defn insert-empties [labeled-if]
  (mdo
    let empties-replacement-map = (calc-empties labeled-if)
    _ = (assert-map empties-replacement-map)
    (unsafe-modify-graph (partial ir/update-graph empties-replacement-map))
    (return (first (get empties-replacement-map labeled-if [labeled-if])))))


(defn if-rewrite-one [label func]
  (mdo
    let labeled-if = (ctxlib/->LabeledFunction func label)
    labeled-if <- (insert-empties labeled-if)
    all-labels <- (fmap vals get-label-map)
    let _ = (assert-type LabeledFunction labeled-if)
    curr-if = (.-function labeled-if)
    _ = (assert-type IRFunc curr-if)
    mapped-to-port = (mapped-fetches-for-if all-labels labeled-if)
    fetches = (vals mapped-to-port)
    if-stack = (.-label labeled-if)
    (mapM
      (fn [& parallel-fetches]
        (mdo
          let [head-fetch & other-fetches] = parallel-fetches
          all-inputs = (mapcat
                         (fn [fetch]
                           (let [args (.-args (.-function fetch))]
                             (assert-count 1 args)))
                         parallel-fetches)
          [merge-out fetch-out] <- (state-mk-names 2)
          merge-fn <- (state-mk-merge all-inputs merge-out)
          new-fetch <- (state-mk-fetch [merge-out] fetch-out)
          identity-ops <- (mapM
                            (fn [{{ret :return} :function
                                  fetch-label   :label}]
                              (state-mk-id [^{:out-idx -1} (.-arg_id (last fetch-label))
                                            ^{:out-idx 0} fetch-out]
                                           ret))
                            parallel-fetches)
          let all-fns = (concat [merge new-fetch] identity-ops)
          (state-delete-fns other-fetches)
          (unsafe-modify-graph (partial ir/replace-node head-fetch all-fns))
          ))
      fetches)))

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
  {ctxlib/if-ctx   (->Rewrite if-rewrite-one return)
   ctxlib/smap-ctx (->Rewrite wrap-smap-once return)})


(defn- unwind-context- [transformation-map ir-graph label-map]
  (let [name-gen (mk-name-gen-func ir-graph)
        id-gen (iterate inc (apply max (map :id ir-graph)))
        fetches-in-context (->> ir-graph
                                (map (fn [a] [a (get label-map (.-id a))]))
                                (filter #(and (is-fetch? (first %)) (not (empty? (second %))))))
        fetches-to-context
        (persistent!
          (second
            (st/run-state
              (mapM
                (fn [[fetch contexts]]
                  (sequence-m
                    (for [frame contexts]
                      (modify #(assoc! % frame (conj (% frame) (.-id fetch)))))))
                fetches-in-context)
              (transient {}))))
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
                           (distinct))
        _ (do
            (println "Working order" working-order)
            (println "Fetches to context" fetches-to-context))
        [_
         {new-graph  :graph
          new-labels :label-map}]
        (st/run-state
          (mapM
            (fn [stackframe]
              (println "Current frame" stackframe)
              (println "Relevant fetches" (fetches-to-context stackframe))
              (let [context-type (ctxlib/get-ctx stackframe)
                    {rewrite-fn  :rewrite
                     cleaning-fn :clean
                     :as         rewrite} (transformation-map context-type)]
                (if (nil? rewrite)
                  (return nil)
                  (>>= (mapM (partial rewrite-fn stackframe) (fetches-to-context stackframe)) cleaning-fn))))
            working-order)
          {:graph     ir-graph
           :label-map label-map
           :name-gen  name-gen
           :id-gen    id-gen})]
    [new-graph new-labels]))


(def context-rewrite-with unwind-context-)


(def context-rewrite (partial context-rewrite-with transformation-map))


(defn- log-transformation-at [name]
  (fn [graph]
    (if @ENABLE_VISUAL_GRAPH_TRANSFORMATION_LOGGING
      (visual/render-to-file name graph))
    graph))


(defn- validate-and-log [name]
  (comp
    (fn [graph] (ir/validate graph (str "after " name)))
    (log-transformation-at name)))


(def transformations
  [
   ; TODO change to tree builders
   ;insert-leaf-builders
   ; FIXME coerce smap and if-rewrite, they need run together
   context-rewrite
   (validate-and-log "context-rewrite")
   batch-rewrite
   (validate-and-log "batch-rewrite")])


(def full-transform (apply comp (reverse transformations)))
