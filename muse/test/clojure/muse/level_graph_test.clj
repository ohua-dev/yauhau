(ns muse.level-graph-test
  (:require
    [clojure.test :refer (deftest is)]
    [promesa.core :as prom]
    [muse.core :as muse :refer :all]
    [cats.core :as m]))


;;;
;;; Data sources:
;;;

(defn set-done [this] (reset! (.-done this) true))

(defrecord GetData [id done args]
  DataSource
  (fetch [_]
    (prom/promise
      (fn [resolve reject]
        ;(assoc this fetched true)
        (println "fetching ...")
        (resolve "foo"))
      )
    )

  BatchedSource
  (fetch-multi [this requests]
    ;(throw (Exception. "here!"))
    (prom/promise
      (fn [resolve reject]
        (println "batched fetching ..." requests)

        (println resolve)
        (set-done this)
        (doall (map set-done requests))
        (println "CHANGED:" @(.-done this) this)
        (println "CHANGED:" requests)
        ;(resolve "one")
        (resolve (into [] (map (fn [_] "foo") requests)))
        ))
    )

  MuseAST
  (done? [this]
    (println "DONE?:" @(.-done this) this)
    @(.-done this))
  )

(defn get-data [id & args]
  (println "get-data called:" args)
  (GetData. id (atom false) args))

(defn compute [& args]
  ; do nothing and return a dummy
  (println "computing ..." args)
  (m/return "foo"))

(defn compute- [& args]
  ; do nothing and return a dummy
  (println "computing ..." args)
  "foo")


(deftest experiment
  (let [result (run!!
                 (m/mlet [local-0 (compute (get-data "foo" 100) (get-data "foo" 100))
                          local-4 (get-data local-0 "foo" 100)
                          local-5 (compute 100)
                          local-6 (get-data "foo" 100)
                          local-7 (compute 100)]
                         (m/mlet [local-3 (compute local-4 local-5 local-7 100)]
                                 (m/mlet [local-10 (get-data "foo" 100)
                                          local-1 (get-data local-3 "foo" 100)
                                          local-2 (get-data local-3 "foo" 100)]
                                         (get-data local-1 local-2 local-6 local-10 "foo" 100)))))]
    (println "result:" result)
    ))

(deftest experiment-batching
  (println "result:"
           (run!! (<$> compute-
                       (get-data "foo1")
                       (get-data "foo2")))))

(deftest experiment-batching-conditions
  (println "result:"
           (run!!
             (<$> compute- (if (compute- 22) (get-data "foo3" 24) (get-data "foo4" 23)) (get-data "foo5" 20))
             )))

(deftest experiment-batching-conditions-cats
  (println "result:"
           (run!!
             (m/mlet [local-1 (get-data "foo5" 20)
                      local-2 (if (compute 22) (get-data "foo3" 24) (get-data "foo4" 23))]
                     (compute local-1 local-2 100)))))

(deftest experiment-applicative
  (println "result:"
           (run!!
             (m/mlet [
                      [local-8 local-9] (<$> clojure.core/vector (<$> compute- (value 100)) (<$> compute- (value 200)))
                      [local-10 local-11] (<$> clojure.core/vector (get-data "foo" 100) (get-data "foo" 200))
                      ]
                     ;(m/return [local-10 local-11])
                     ;(m/return (compute- local-10 local-11))
                     (get-data local-10 local-11)
                     ))
  ))

(deftest experiment-one-level
  (let [result (run!!
                 (m/mlet [
                          ; works!
                          ;local-4 (GetData. 100)
                          ;local-7 (GetData. (compute local-4))

                          ; works!
                          ;local-1 (m/return 100)
                          ;local-4 (GetData. local-1)
                          ;local-5 (GetData. (compute local-4))
                          ;local-7 (GetData. (compute local-5))

                          ; works!
                          ;local-1 (m/return 100)
                          ;local-4 (get-data local-1)
                          ;local-5 (get-data (compute local-4))
                          ;local-7 (get-data (compute local-5))

                          ; works!
                          ;local-4 (get-data 100)
                          ;local-5 (m-compute local-4)
                          ;local-6 (get-data local-5)
                          ;local-7 (m-compute local-6)

                          ; works!
                          ;local-3 (get-data "foo1" 100)
                          ;local-4 (compute 100)
                          ;local-5 (get-data "foo2" 100)
                          ;local-6 (compute 200)
                          ;local-7 (get-data local-3 local-4 local-5 local-6 "foo3" 100)

                          local-3 (get-data "foo1" 100)
                          local-4 (compute 100)
                          local-5 (get-data "foo2" 100)
                          local-7 (if (compute local-3 22) (get-data "foo3" local-4 24) (get-data "foo4" local-5 23))
                          ]
                         (m/return local-7)))]
    (println "result:" result)
    ))