(ns blog
  (:require [ohua.lang :refer [ohua-require defalgo]]
            [yauhau.core :refer [yauhau]])
  (:import yauhau.IDataSource))

(ohua-require [yauhau.functions :refer :all])

(def datasource (reify IDataSource
                  (getIdentifier [this] :default)
                  (fetch [this reqs] "")
                  (store [this reqs] (throw (new Exception "store is not implemented")))))

(defalgo get-post-ids [] (fetch (request :ids datasource)))
(defalgo get-post-info [id] (fetch (request [:info id] datasource)))
(defalgo get-post-content [id] (fetch (request [:content id] datasource)))
(defalgo get-post-views [id] (fetch (request [:views id] datasource)))

(defn render-page [_ _]
  :html)

(defn render-posts [_]
  :html)

(defn render-side-pane [_ _]
  :html)

(defn render-post-list [_]
  :html)

(defn render-topics [_]
  :html)


(defalgo get-all-posts-info []
  (smap get-post-info (get-post-ids)))


(defalgo get-post-details [pid]
  (vector (get-post-info pid) (get-post-content pid)))


(defalgo topics []
  (let [posts (get-all-posts-info)
        topiccounts (frequencies (map post-topic posts))]
    (render-topics topiccounts)))

(def five (Integer. (int 5)))

(defalgo popular-posts []
  (let [pids (get-post-ids)
        views (smap get-post-views pids)
        ordered (take five (map first (sort-by (flip (comparing second)) (map vector pids views))))
        content (smap get-post-details ordered)]
    (render-post-list content)))


(defalgo left-pane []
  (render-side-pane (popular-posts) (topics)))


(defalgo main-pane []
  (let [posts (get-all-posts-info)
        ordered (take five (sort-by (comparing post-date)) posts)
        content (smap (compose get-post-content post-id) ordered)]
    (render-posts (zip ordered content))))




(defalgo blog []
  (render-page (left-pane) (main-pane)))

(println (yauhau (blog)))
