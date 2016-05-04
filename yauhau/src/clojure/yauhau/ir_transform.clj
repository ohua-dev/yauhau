;
; Copyright (c) Justus Adam, Sebastian Ertel, Andres Goens 2016. All Rights Reserved.
;
; This source code is licensed under the terms described in the associated LICENSE file.
;

(ns yauhau.ir-transform
  (:require [com.ohua.ir :as ir :refer [fn-name-in]]
            [yauhau.accumulator :as acc]
            [clojure.set :as setlib]
            [clojure.set :as set]
            [clojure.pprint :refer [pprint]]
            [com.ohua.util.visual :as visual])
  (:import (com.ohua.ir IRFunc IRGraphPosition)
           (clojure.lang PersistentArrayMap))
  (:use com.ohua.util.ir
        com.ohua.util.assert
        com.ohua.util.visual))


(def ENABLE_VISUAL_GRAPH_TRANSFORMATION_LOGGING (atom false))


(defrecord IFLabeledFunction [function if-stack])
(defrecord IFStackEntry [if-op arg-id])
(defrecord SmapNestingLevel [size-source])
(defrecord SmapStackEntry [size-op])
(defrecord LabeledFunction [function label])
(defrecord ContextType
  [; A name for this context type
   name
   ; predicate whether a certain function/operator opens a context of this type
   ; type = Function -> Boolean
   trigger-open
   ; predicate whether a certain function/operator closes a context of this type
   ; type = Function -> Boolean
   trigger-close
   ; verify the context has been correctly opened and closed
   ; type = (Function, StackFrame) -> Boolean
   verify-context-wrap
   ; function for creating a context stack frame for each of the outputs
   ; type = (Graph, Function, Symbol) -> StackFrameData
   create-stack-value
   ; rewrite the graph unwiding the topmost context stack value
   ; type = (NameGen, LabeledGraph, StackFrameData) -> LabeledGraph
   unwind
   ; traverse the graph and remove any redundant operators
   ; type = Graph -> Graph
   clean-graph
   ])

(defprotocol ContextStackEntry
  (get-context [this]))


;; RECOGNIZE SPECIFIC FUNCTIONS
(def is-smap? (fn-name-in #{'smap "smap"}))
(def is-one-to-n? (fn-name-in #{'one-to-n "one-to-n"}))
(def is-collect? (fn-name-in #{'collect "collect"}))
(def is-fetch? (fn-name-in #{"yauhau.operators/fetch" 'yauhau.operators/fetch "fetch" 'fetch}))
(def is-ite? (fn-name-in #{'ifThenElse "ifThenElse"}))
(def is-merge? (fn-name-in #{'merge "merge"}))
(def is-id-op? (fn-name-in #{"identity" 'identity}))


;; CREATE SPECIFIC FUNCTIONS
(defn mk-func-maker [name] (fn [in out] (ir/mk-func name (into [] in) (if (seq? out) (into [] out) out))))
(def mk-merge (mk-func-maker "merge"))
(def mk-fetch (mk-func-maker "fetch"))
(def mk-id-op (mk-func-maker "identity"))
(defn mk-empty-request [in out] (ir/mk-func "__emptyRequest" [in '__constDataSource] out))
(defn mk-const-nil [in out] (ir/mk-func "__constNull" in out))


(defn- next-var-name [name-gen]
  (loop [[name & new :as state] @name-gen]
    (if (compare-and-set! name-gen state new)
      (symbol name)
      (recur name-gen))))


(defn mk-name-gen-func [ir-graph] (partial next-var-name (atom (ir/name-gen-from-graph ir-graph))))


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


(defn- get-size-op [smap-ir-fun ir-graph]
  (let [one-to-n-op (ir/get-producer (first (.-args smap-ir-fun)) ir-graph)
        size-op (ir/get-producer (first (.-args one-to-n-op)) ir-graph)]
    size-op))


(defn- create-collect-smap-wrap-for [levels init-in init-out name-gen]
  (reduce
    (fn [[collects smaps input output] level]
      (let [size-source (.-size_source level)
            [collect-out tree-out one-to-n-in smap-in] (repeatedly 4 name-gen)
            new-collect (ir/mk-func "collect" [size-source input] collect-out)
            tree-builder (ir/mk-func "__mkReqBranch" [collect-out] tree-out)
            new-one-to-n (ir/mk-func "one-to-n" [size-source one-to-n-in] smap-in)
            new-smap (ir/mk-func "smap" [smap-in] output)]
        [(-> collects
             (conj new-collect)
             (conj tree-builder))
         (->> smaps
              (cons new-smap)
              (cons new-one-to-n))
         tree-out
         one-to-n-in]))
    [[] [] init-in init-out]
    (if (vector? levels) (rseq levels) (reverse levels))))


(defn- find-levels [fetch-fn ir-graph]
  (loop [producers (set (remove nil? (map #(ir/get-producer % ir-graph) (:args fetch-fn))))
         levels '()
         ignores #{}]
    (if (empty? producers)
      levels
      (let [one-to-ns (filter is-one-to-n? producers)
            collects-size-ops (->> producers
                                   (filter is-collect?)
                                   (map (comp first :args))
                                   (set))
            new-level-size-ops (->> one-to-ns
                                    (map (comp first :args)) ; get the associated size operator outputs
                                    (remove (partial contains? ignores)) ; drop ignored
                                    (distinct))             ; remove the ignored size ops
            new-levels (map ->SmapNestingLevel new-level-size-ops)
            new-ignores (setlib/union ignores collects-size-ops (set new-level-size-ops))]
        (recur
          (set (remove nil? (map #(ir/get-producer % ir-graph) (mapcat :args producers))))
          (concat new-levels levels)
          new-ignores)))))


(defn- generate-smap-wrappers [fetch-fn levels name-gen]
  (if (empty? levels)
    [fetch-fn]
    (let [fetch-args (:args fetch-fn)
          packager-out (name-gen)
          [collects smaps _ fetch-out] (create-collect-smap-wrap-for levels packager-out (.-return fetch-fn) name-gen)
          new-fetch-args (repeatedly (count fetch-args) name-gen)
          ; make it a vector, because lists are weird
          new-fetch-args (into [] new-fetch-args)
          collects (into [] collects)
          smaps (into [] smaps)
          collects
          (update-in collects [(dec (count collects))] #(assoc % :return new-fetch-args))

          new-fetch-fn (-> fetch-fn
                           (assoc :args new-fetch-args)
                           (assoc :return fetch-out))

          ; adding fetch function
          collects (conj collects new-fetch-fn)

          ; Adding packaging function
          collects (cons (ir/mk-func "__packageArgs" fetch-args packager-out) collects)]
      (concat collects smaps))))


(defn cat-redundant-smap-collects
  [ir-graph]
  ir-graph)


(defn- rewrite-fetch [name-gen ir-graph fetch-fn]
  (generate-smap-wrappers fetch-fn (find-levels fetch-fn ir-graph) name-gen))


(defn smap-rewrite [ir-graph]
  (let [_ (do
            (assert-coll ir-graph)
            (doall (map (partial assert-type IRFunc) ir-graph)))
        fetches (filter is-fetch? ir-graph)
        name-gen (mk-name-gen-func ir-graph)
        fetches (distinct fetches)
        transformed-fetches
        (zipmap fetches (map (partial rewrite-fetch name-gen ir-graph) fetches))]
    (ir/update-graph transformed-fetches ir-graph)))

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


(defn- get-fetches-concerned [labeled-graph curr-if]
  (filter
    (fn [func]
      (and
        (is-fetch? (.-function func))
        (not (empty? (.-label func)))
        (let [found-if (.-if_op (peek (.-label func)))]
          (assert-type (type curr-if) found-if)
          (= curr-if found-if))))
    labeled-graph))


(defn- mapped-fetches-for-if [labeled-graph curr-if]
  (let [fetches-concerned (get-fetches-concerned labeled-graph curr-if)
        _ (doall (map (partial assert-type LabeledFunction) labeled-graph))
        _ (assert-type LabeledFunction curr-if)]
    (if-not (empty? fetches-concerned) (assert-type LabeledFunction (first fetches-concerned)))
    (merge
      (zipmap (ir/get-return-vars (.-function curr-if)) (repeat []))
      (group-by #(.-arg_id (peek (.-label %))) fetches-concerned))))


(defn- calc-empties [name-gen if-op labeled-graph]
  (let [mapped-to-port (mapped-fetches-for-if labeled-graph if-op)
        longest-fetch-seq (apply max-key count (vals mapped-to-port))
        longest-nr (count longest-fetch-seq)
        example-if-stack (.-label (first longest-fetch-seq))
        empty-required (remove (comp zero? (partial - longest-nr) count second) (seq mapped-to-port))]
    (map-from-coll
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
              [to-replace (into [] (concat replacement-head empties))])))
        empty-required))))


(defn- unlabel-graph [graph]
  (into [] (map :function graph)))


(defn insert-empties [name-gen labeled-graph labeled-if]
  (let [empties-replacement-map (calc-empties name-gen labeled-if labeled-graph)
        _ (assert-map empties-replacement-map)
        labeled-graph (ir/update-graph empties-replacement-map labeled-graph)]
    [labeled-graph
     (first (get empties-replacement-map labeled-if [labeled-if]))]))


(defn- if-rewrite-one [name-gen labeled-graph labeled-if]
  (let [[labeled-graph labeled-if] (insert-empties name-gen labeled-graph labeled-if)
        _ (doall (map (partial assert-type LabeledFunction) labeled-graph))
        curr-if (.-function labeled-if)
        _ (assert-type IRFunc curr-if)
        mapped-to-port (mapped-fetches-for-if labeled-graph labeled-if)
        fetches (vals mapped-to-port)
        if-stack (.-label labeled-if)
        replaces (apply mapcat (fn [& parallel-fetches]
                                 (let [[head-fetch & other-fetches] parallel-fetches
                                       all-inputs (mapcat
                                                    (fn [fetch]
                                                      (let [args (.-args (.-function fetch))]
                                                        (assert-count 1 args)))
                                                    parallel-fetches)
                                       merge-out (name-gen)
                                       fetch-out (name-gen)
                                       label #(->LabeledFunction % if-stack)
                                       merge (label (mk-merge all-inputs merge-out))
                                       new-fetch (label (mk-fetch [merge-out] fetch-out))
                                       identity-ops (map (fn [fetch] (label (mk-id-op [^{:out-idx -1} (.-arg_id (last (.-label fetch)))
                                                                                       ^{:out-idx 0} fetch-out]
                                                                                      (.-return (.-function fetch))))) parallel-fetches)]
                                   (concat [[head-fetch (concat [merge new-fetch] identity-ops)]] (map (fn [a] [a []]) other-fetches))))
                        fetches)
        _ (assert-coll replaces)
        _ (if-not (empty? replaces)
            (do (assert-coll (first replaces))
                (assert-type LabeledFunction (first (first replaces)))
                (assert-coll (second (first replaces)))
                (assert-type LabeledFunction (-> replaces first second first))))
        replacement-map (map-from-coll replaces)
        _ (assert-map replacement-map)]
    (ir/update-graph replacement-map labeled-graph)))


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
    (filter is-merge? ir-graph)))


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
      (if (and (not (empty? successors)) (is-merge? more-bottom) (= 1 (count (ir/successors more-bottom graph))))
        (recur more-bottom)
        merge))))


(defn find-upstream-fns-and-merges [merge graph]
  (let [predecessors (set (ir/predecessors merge graph))
        merges (set/select is-merge? predecessors)
        non-merges (set/select (comp not is-merge?) predecessors)]

    (reduce
      (fn [[merges non-merges] [new-merges new-non-merges]]
        [(set/union merges new-merges)
         (set/union non-merges new-non-merges)])
      [merges non-merges]
      (map #(find-upstream-fns-and-merges % graph) merges))))


(defn coerce-merges [ir-graph]
  (let [all-merges (set (filter is-merge? ir-graph))]
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


(def if-context
  (->ContextType
    "if"
    is-ite?
    is-merge?
    (fn [_ frame] (= IFStackEntry (type frame)))
    (fn [_ if-op arg-id] (->IFStackEntry if-op arg-id))
    (fn [name-gen graph stackframe]
      (if-rewrite-one name-gen graph stackframe))
    (comp
      cat-redundant-merges
      cat-redundant-identities
      cat-identities-with-no-successor
      coerce-merges)))


(def smap-context
  (->ContextType
    "smap"
    is-smap?
    is-collect?
    (fn [_ frame] (= SmapStackEntry (type frame)))
    (fn [graph smap-fun _]
      (->SmapStackEntry (get-size-op smap-fun graph)))
    (fn [name-gen graph]
      )
    cat-redundant-smap-collects))


(extend-protocol ContextStackEntry
  IFStackEntry
  (get-context [_] if-context)
  SmapStackEntry
  (get-context [_] smap-context))


(def context-stack [if-context smap-context])


(defn forward-label-graph [reduction-function
                           initial-state
                           initial-label-fn
                           graph]
  (let [initial-fns (->> graph
                         (filter (comp empty? :args))
                         (map #(->LabeledFunction % (initial-label-fn %))))]
    (loop [[head & queue] initial-fns
           state initial-state
           functions initial-fns]
      (let [{function :function
             label    :label} head
            successors (ir/successors function graph)
            [new-state fns]
            (reduce
              (fn [[state fns] node]
                (let [[new-state labeled-fn]
                      (reduction-function
                        state
                        head
                        node)]
                  [(if (nil? new-state) state new-state)
                   (if (nil? labeled-fn) fns (conj fns labeled-fn))]))
              [state []]
              successors)
            new-queue (concat queue fns)
            new-fns (concat functions fns)]
        (if (empty? new-queue)
          [new-state new-fns]
          (recur
            new-queue
            new-state
            new-fns))))))


(defn closes-context? [current-label-stack function]
  (some
    (fn [context]
      (let [trigger (.-trigger_close context)]
        (if (trigger function)
          (assert ((.-verify_context_wrap context) function (first current-label-stack)) (str "Context wrapping verification failed for context " (.-name context)))
          false)))
    context-stack))


; works under the assumption that any function will only ever open one new context and no function will simultaneously open and close a context
(defn graph-reduction-function
  [graph
   ^PersistentArrayMap fn-map
   {function    :function
    label-stack :label}
   node]
  (let [^LabeledFunction saved (get fn-map node)]
    (if (or (nil? saved) (> (count (.-label saved)) (count label-stack)))
      (let [opened-context? (some
                              (fn [context]
                                (if ((.-trigger_open context) node)
                                  context
                                  false))
                              context-stack)
            new-stack (cond
                        opened-context? (let [creator-fn (.-create_stack_value opened-context?)
                                              arg-id (first (set/intersection (set (.-return function)) (set (.-args node))))
                                              data (creator-fn graph node arg-id)]
                                          (conj label-stack data))
                        (closes-context? label-stack node) (pop label-stack)
                        :else label-stack)
            labeled-fn (->LabeledFunction node new-stack)]
        [(assoc fn-map node labeled-fn) labeled-fn]))))


(defn label-graph [graph]
  (forward-label-graph
    (partial graph-reduction-function graph)
    {}
    (constantly [])
    graph))


(defn- unwind-context [ir-graph labeled-graph]
  (let [name-gen (mk-name-gen-func ir-graph)
        _ (assert-coll-of-type LabeledFunction labeled-graph)
        fetches-in-context (filter #(and (is-fetch? (.-function %))
                                         (not (empty? (.-label %)))) labeled-graph)
        label-stacks (->> fetches-in-context
                          (map :label)
                          (distinct)
                          (sort-by count)
                          (reverse))
        working-order (distinct (map peek label-stacks))]

    (unlabel-graph
      (reduce
        (fn [graph ^ContextStackEntry stackframe]
          (let [^ContextType context-type (get-context stackframe)
                rewrite-fn (.-unwind context-type)
                cleaning-fn (.-clean_graph context-type)]
            (cleaning-fn
              (rewrite-fn name-gen graph stackframe))))
        labeled-graph
        working-order))))


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
   smap-rewrite
   (validate-and-log "smap-rewrite")
   cat-redundant-smap-collects
   (validate-and-log "cat-redundant-smap-collects")
   cat-redundant-merges
   (validate-and-log "cat-redundant-merges-rewrite")
   cat-redundant-identities
   (validate-and-log "cat-redundant-identifiers-rewrite")
   cat-identities-with-no-successor
   (validate-and-log "cat-identities-with-no-successor-rewrite")
   coerce-merges
   (validate-and-log "coerce-merges")
   batch-rewrite
   (validate-and-log "batch-rewrite")])


(def full-transform (apply comp (reverse transformations)))
