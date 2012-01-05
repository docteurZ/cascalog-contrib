(ns cascalog.graph.core
  (:use cascalog.api)
  (:require [cascalog [ops :as c]]))


(defn make-file-source
  "reads all the files in 'dir'"
  [dir]
  (let [source (hfs-textline dir)]
    (<- [?line] (source ?line) (:distinct false))))

(defn parse-line
  "parses a line where the edgelist format is: 'src' 'dest'"
  [line]
  (let [[_ src dst] (re-find #"([^\s]+)\s+([^\s]+)" line)]
    [dst src]))
    
(defn enumerate-edges
  "enumates all the edges with a return tuple: '[dst src]'"
  [dir]
  (let [line (make-file-source dir)]
    (<- [?dst ?src] (line ?line) (parse-line ?line :> ?dst ?src))))

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
  (<- [?nb-nodes] (nodes ?node) (c/count ?nb-nodes)))


