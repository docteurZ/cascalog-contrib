(ns cascalog.graph.components
  (:use cascalog.graph.core)
  (:use cascalog.api)
  (:use cascalog.checkpoint)
  (:require [cascalog [ops :as c]]))


(defmapop add-class-field [x]
  [x (str "C_" x)])

(defn init-partition
  [edges]
  (let [nodes (enumerate-nodes edges)]
    (<-[?y ?z] (nodes ?n) (add-class-field ?n :> ?y ?z))))

(defmapop mk-best-component
  [zx zy sx sy]
  (cond
   (= sx sy) (first (sort [zx zy]))
   (> sx sy) zx
   :else zy))


(defmapcatop mk-partition
  [x y z]
  [[x z] [y z]])

(defn merge-components
  [edges part size]
  (<- [?u ?z] (edges ?x ?y) (part ?x ?zx) (part ?y ?zy)
      (size ?zx ?sx) (size ?zy ?sy)
      (mk-best-component ?zx ?zy ?sx ?sy :> ?zz)
      (mk-partition ?x ?y ?zz :> ?u ?z) (:distinct true)))

(defn size-components
  [part]
  (<- [?z ?count] (part _ ?z)  (:distinct false) (c/count :> ?count)))


(defn union-find3
  [edges]
  (let [init    (init-partition edges)
        size (size-components init)
        merge-1 (merge-components edges init size)
        size-1  (size-components merge-1)
                                        ;merge-2 (merge-components merge-1)
        ;merge-3 (merge-components merge-2)
        ;size3   (size-components merge-2)
        ;merge-4 (merge-components merge-3)
        ]
    (?- (stdout) init)
    (?- (stdout) size)
    (?- (stdout) merge-1)
    (?- (stdout) size-1)
    ;(?- (stdout) merge-2)
    ;(?- (stdout) merge-3)
    ;(?- (stdout) size3)
    ;(?- (stdout) merge-4)
    ))


(deffilterop different-compoment? [x y] (not= x y))





(defn union-find
  [edges]
  (let [part-init (init-partition edges)
        merge-init (components-to-merge  edges part-init)
        count-init (first (first (??<- [?count] (merge-init _ ?c _ _) (c/distinct-count ?c :> ?count))))
        part (loop
                 [count count-init
                  nb-iter 0
                  req   (update-partition part-init merge-init)
                  merge (components-to-merge edges part-init)]
               (if (and (> count 1) (< nb-iter 10))
                 (let [part-path  (str "/tmp/findc/partition_" nb-iter)
                       merge-path (str "/tmp/findc/component_" nb-iter)]
                   (println "Components to merge: " count)
                   (?- (hfs-textline part-path) req)
                   (?- (hfs-textline merge-path)  merge)
                   (recur (first (first (??<- [?count] (merge _ ?c _ _) (c/distinct-count ?c :> ?count))))
                          (inc nb-iter)
                          (update-partition (mk-two-fields-source part-path)
                                            (mk-two-fields-source merge-path))
                          (components-to-merge edges (mk-two-fields-source part-path))))
                 req))]
    part))





