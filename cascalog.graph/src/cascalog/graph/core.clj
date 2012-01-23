(ns cascalog.graph.core
  (:use cascalog.api)
  (:require [cascalog [ops :as c]]))


(defn make-file-source
  "reads all the files in 'dir'"
  [dir]
  (let [source (hfs-textline dir)]
    (<- [?line] (source ?line) (:distinct false))))

(defn enumerate-edges
  "enumates all the edges where the edgelist format is 'src' 'dest' and return tuples '[dst src]'"
  [dir]
  (let [line (make-file-source dir)]
    (<- [?dst ?src] (line ?line) (c/re-parse [#"[^\s]+"] ?line :> ?src ?dst) (:distinct false))))

(defn in-degree
  "computes the in degrees"
  [edges]
  (<- [?dst ?in_d] (edges ?dst _) (:distinct false) (c/count :> ?in_d)))

(defn out-degree
  "computes the out degrees"
  [edges]
  (<- [?src ?out_d] (edges _ ?src) (:distinct false) (c/count :> ?out_d)))

(defmapcatop mk-node
  [dest src]
  [[dest] [src]])

(defn enumerate-nodes
  "enumerate the nodes"
  [edges]
  (<- [?node] (edges ?dst ?src) (mk-node ?dst ?src :> ?node) (:distinct true)))

(defn count-nodes
  "counts the number of nodes"
  [nodes]
  (<- [?nb-nodes] (nodes ?node ) (c/distinct-count ?node :> ?nb-nodes)))
