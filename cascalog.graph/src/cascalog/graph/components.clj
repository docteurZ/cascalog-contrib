(ns cascalog.graph.components
  (:use cascalog.graph.core)
  (:use cascalog.api)
  (:use cascalog.checkpoint)
  (:require [cascalog [ops :as c]]))


(defmapop add-class-field [x]
  [x (str "class_" x)])

(defn init-partition
  [edges]
  (let [nodes (enumerate-nodes edges)]
    (<-[?y ?z] (nodes ?n) (add-class-field ?n :> ?y ?z))))


(deffilterop different-compoment? [x y] (not= x y))

(defn components-to-merge
  [edges part]
  (<- [?zu ?zv] (edges ?u ?v) (part ?u ?zu) (part ?v ?zv)
      (different-compoment? ?zu ?zv) (:distinct true)))

(defmapop get-min
  [x y]
  (if (nil? y)
    x
    (first (sort  [x y]))))

(defn mk-two-fields-source
  [dir]
  (let [source (hfs-textline dir)]
    (<- [?n ?z] (source ?line) (c/re-parse [#"[^\s]+"] ?line :> ?n ?z)
        (:distinct false))))

(defn update-partition
  [part components]
  (<- [?u ?z] (part ?u ?zu) (components !!zv ?zu)
      (get-min ?zu !!zv :> ?z) (:distinct true)))


(defn union-find
  [edges]
  (let [part-init (init-partition edges)
        merge-init (components-to-merge  edges part-init)
        count-init (first (first (??<- [?count] (merge-init ?c _) (c/distinct-count ?c :> ?count))))
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
                   (recur (first (first (??<- [?count] (merge ?c _) (c/distinct-count ?c :> ?count))))
                          (inc nb-iter)
                          (update-partition (mk-two-fields-source part-path)
                                            (mk-two-fields-source merge-path))
                          (components-to-merge edges (mk-two-fields-source part-path))))
                 req))]
    part))





