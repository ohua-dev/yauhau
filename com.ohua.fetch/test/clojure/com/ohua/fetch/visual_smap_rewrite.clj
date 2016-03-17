(ns com.ohua.fetch.visual-smap-rewrite
  (:require [com.ohua.ir :as ir]
            [com.ohua.util.visual :as visual]
            [com.ohua.fetch.ir-transform :as trans]))


(def ir3 [(ir/mk-func "size" ['coll1] 'size1)
          (ir/mk-func "one-to-n" ['size1 'coll1] 'packaged1)
          (ir/mk-func "smap" ['packaged1] 'data1)
          (ir/mk-func "size" ['data1] 'size2)
          (ir/mk-func "one-to-n" ['size2 'data1] 'packaged2)
          (ir/mk-func "smap" ['packaged2] 'data2)
          (ir/mk-func "fetch" ['data2] 'intermediate1)
          (ir/mk-func "collect" ['size2 'intermediate1] 'intermediate2)
          (ir/mk-func "collect" ['size1 'intermediate2] 'result)])


(def ir4 [(ir/mk-func "size" ['coll1] 'size1)
          (ir/mk-func "one-to-n" ['size1 'coll1] 'smap1)
          (ir/mk-func "smap" ['smap1] 'a)
          (ir/mk-func "fetch" ['a] 'b)
          (ir/mk-func "fetch" ['b] 'i)
          (ir/mk-func "collect" ['size2 'i] 'j)])


(def write-graph (partial visual/render-to-file "smap"))


(write-graph "original" 4 ir4)
(write-graph "smap-rewrite" 4 (trans/smap-rewrite ir4))
