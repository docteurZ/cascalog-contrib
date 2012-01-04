(ns cascalog.graph.pagerank
  (:use cascalog.graph.core)
  (:use cascalog.api)
  (:require [cascalog [ops :as c]]))

(def damping 0.85)
(def max-iteration 5)

(defn init-pagerank
  "initiates the pagerank vector with: 1/nb-nodes"
  [dir]
  (let [nodes (enumerate-nodes dir)
        nb-nodes (first (first (??<- [?nb-nodes] (nodes ?node) (c/count ?nb-nodes))))]
    (<- [?node ?pr] (nodes ?node) (div 1 nb-nodes :> ?pr))))


(defn mk-graph-data
  "makes the graph data structure with format: [dst src (out-d src)]"
  [dir]
  (let [edges (enumerate-edges dir)
        out_d (out-degree dir)
        graph-data (<- [?dst ?src ?out]
                       (edges ?dst ?src)
                       (out_d ?src ?out)
                       (:distinct false))]
    graph-data))

(defn or-fn
  "basic function or"
  [cl1 cl2]
  (or cl1 cl2))

(defn compute-pagerank
  "computes and normalizes pr for each nodes for a given iteration"
  [graph-data old-pr nb-nodes]
  (let [damping-out-factor (* (- 1 damping) (/ 1  nb-nodes))
        out-factor (<- [?dst ?src ?out-factor]
                       (graph-data ?dst ?src ?out)
                       (old-pr ?src ?pr-src)         
                       (div ?pr-src ?out :> ?out-factor))        
        add-out (<- [?dst ?sum-out]
                    (out-factor ?dst ?src ?out-factor)
                    (c/sum ?out-factor :>  ?sum-out))        
        pr_compute (<- [?dst ?pr]
                       (graph-data ?dst ?src ?out)
                       (add-out ?dst ?sum-out)
                       (* damping ?sum-out  :> ?sum-out-factor)
                       (+ ?sum-out-factor damping-out-factor :> ?pr))
        pr (<- [?node ?new-pr]
               (pr_compute ?node !!pr-to-merge) (old-pr ?node ?old-pr)
               (* ?old-pr (- 1 damping) :> ?old-pr-with-damping)
               (or-fn !!pr-to-merge ?old-pr-with-damping :> ?new-pr))
        sum-pr (first (first (??<- [?sum-pr] (pr ?node ?pr) (c/sum ?pr :> ?sum-pr))))]
    (<- [?node ?norm-pr] (pr ?node ?pr) (div ?pr sum-pr :>  ?norm-pr))))


(defn pagerank
  "main function to compute the pagerank"
  [dir]
  (let [graph-data (mk-graph-data dir)        
        pr-init (init-pagerank dir)
        nodes (enumerate-nodes dir)
        nb-nodes (first (first (??<- [?nb-nodes] (nodes ?node) (c/count ?nb-nodes))))
        pr (loop
               [it-nb max-iteration
                req (compute-pagerank graph-data pr-init nb-nodes)]
             (if (pos? it-nb)
               (recur (dec it-nb) (compute-pagerank graph-data req nb-nodes))
               req))]
    (?- (stdout) pr)
    ))
