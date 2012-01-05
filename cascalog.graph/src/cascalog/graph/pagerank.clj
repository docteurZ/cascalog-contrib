(ns cascalog.graph.pagerank
  (:use cascalog.graph.core)
  (:use cascalog.api)
  (:require [cascalog [ops :as c]]))

(def damping 0.85)
(def max-iteration 9)

(defn init-pagerank
  "initiates the pagerank vector with: 1/nb-nodes"
  [nodes nb-nodes]
  (<- [?node ?pr] (nodes ?node) (div 1 nb-nodes :> ?pr)))

(defn mk-graph-data
  "makes the graph data structure with format: [dst src (out-d src)]"
  [edges]
  (let [out_d (out-degree edges)]    
    (<- [?dst ?src ?out] (edges ?dst ?src) (out_d ?src ?out) (:distinct false))))

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
        pr-compute (<- [?dst ?pr]
                       (graph-data ?dst ?src ?out)
                       (add-out ?dst ?sum-out)
                       (* damping ?sum-out  :> ?sum-out-factor)
                       (+ ?sum-out-factor damping-out-factor :> ?pr))
        pr (<- [?node ?new-pr]
               (pr-compute ?node !!pr-to-merge) (old-pr ?node ?old-pr)
               (* ?old-pr (- 1 damping) :> ?old-pr-with-damping)
               (or-fn !!pr-to-merge ?old-pr-with-damping :> ?new-pr))
        sum-pr (first (first (??<- [?sum-pr] (pr ?node ?pr) (c/sum ?pr :> ?sum-pr))))]
    (<- [?node ?norm-pr] (pr ?node ?pr) (div ?pr sum-pr :>  ?norm-pr))))


(defn pagerank
  "main function to compute the pagerank"
  [edges]
  (let [nodes (enumerate-nodes edges)
        nb-nodes (first (first (??<- [?nb-nodes] (nodes ?node) (c/count ?nb-nodes))))
        pr-init (init-pagerank nodes nb-nodes)
        graph-data (mk-graph-data edges)
        
        pr (loop
               [it-nb max-iteration
                req (compute-pagerank graph-data pr-init nb-nodes)]
             (if (pos? it-nb)
               (recur (dec it-nb) (compute-pagerank graph-data req nb-nodes))
               req))
        ]
    pr))
