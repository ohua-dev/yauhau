(ns blog)


(defalgo get-post-ids [] (fetch [:posts]))
(defalgo get-post-info [id] (fetch [:info id]))
(defalgo get-post-content [id] (fetch [:content id]))
(defalgo get-post-views [id] (fetch [:views id]))

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


(defalgo popular-posts []
  (let [pids (get-post-ids)
        views (smap get-post-views pids)
        ordered (take 5 (map first (sort-by (flip (comparing second)) (map vector pids views))))
        content (smap get-post-details ordered)]
    (render-post-list content)))


(defalgo left-pane []
  (render-side-pane (popular-posts) (topics)))


(defalgo main-pane []
  (let [posts (get-all-posts-info)
        ordered (take 5 (sort-by (comparing post-date)) posts)
        content (smap (compose get-post-content post-id) ordered)]
    (render-posts (zip ordered content))))




(defalgo blog []
  (render-page (left-pane) (main-pane)))

