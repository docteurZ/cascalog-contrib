(ns cascalog.graph.components
  (:use cascalog.graph.core)
  (:use cascalog.api)
  (:use cascalog.checkpoint)
  (:require [cascalog [ops :as c] [io :as io]]))

(def max-iteration 20)

(defmapop add-class-field [x]
  [x (str "C_" x)])

(defn init-partition
  "At the init, each node belongs to a different component"
  [graph]
  (let [nodes (enumerate-nodes graph)]
    (<- [?y ?z] (nodes ?n) (add-class-field ?n :> ?y ?z))))


(defn init-size
  "optim for the init"
  [graph]
  (let [out (out-degree graph)]
    (<- [?z ?count] (out ?u ?count) (str "C_" ?u :> ?z))))


(deffilterop different-compoment? [x y] (not= x y))

(defn components-to-merge
  "computes the components to merge"
  [graph part]
  (<- [?zu ?zv] (graph ?u ?v) (part ?u ?zu) (part ?v ?zv)
      (different-compoment? ?zu ?zv) (:distinct true)))

(defn size-components
  "computes the size of each component"
  [part]
  (<- [?z ?count] (part _ ?z)  (:distinct false) (c/count :> ?count)))


(defbufferop sort-components
  "sort by value then by key"
  [tuples]
  (let [all (reduce (fn [acc [zu zv su sv]]
                      (assoc acc zu (parse-number su) zv (parse-number sv))) {} tuples)]
     [(first (first (reverse (sort-by
                              (fn [[k v]] [v k]) all))))]))

(defn update-partition
  "computes the node which has an update or partition"
  [part components size]
  (<- [?u  ?z] (part ?u ?zu) (components ?zu ?zv)
      (size ?zu ?su) (size ?zv ?sv)
      (sort-components ?zu ?zv ?su ?sv :> ?z)
      (:distinct true)))


(defn merge-partition
  "merge the old partition with the updates"
  [old-part updates]
  (<- [?u ?z] (old-part ?u ?zu)
      (updates ?u !!zv)
      (or-fn !!zv ?zu  :> ?z) (:distinct true)))


(defn union-find
  [edges tmp-dir]
  (let [graph (symetrize-graph edges)
        part-init (init-partition graph)
        part (loop [count (first (first (??<- [?count] (part-init ?n _)
                                              (c/distinct-count ?n :> ?count))))
                    nb-iter 0
                    size (init-size graph)
                    merge (components-to-merge graph part-init)
                    update (update-partition part-init merge size)
                    req (merge-partition part-init update)]
               (if (and (not (nil? count)) (< nb-iter max-iteration))
                 (let [size-path  (str tmp-dir "/findc/size_" nb-iter)
                       merge-path (str tmp-dir "/findc/merge_" nb-iter)
                       update-path (str tmp-dir "/findc/update_" nb-iter)
                       part-path  (str tmp-dir "/findc/partition_" nb-iter)]
                   (println "Components to merge: " count
                            " iteration: " nb-iter "/" max-iteration)
                   (?- (hfs-textline size-path) size)
                   (?- (hfs-textline merge-path)  merge)
                   (?- (hfs-textline update-path) update)
                   (?- (hfs-textline part-path) req)
                   (recur
                    (first (first (??<- [?count] (merge ?c _) (c/distinct-count ?c :> ?count))))
                    (inc nb-iter)
                    (size-components  (mk-two-fields-source part-path))
                    (components-to-merge graph (mk-two-fields-source part-path))
                    (update-partition (mk-two-fields-source part-path)
                                      (mk-two-fields-source merge-path)
                                      (mk-two-fields-source size-path))
                    (merge-partition (mk-two-fields-source part-path)
                                     (mk-two-fields-source update-path))))
                 req))]
    (io/delete-file-recursively (str tmp-dir "/findc"))
    part))






