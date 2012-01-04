(ns cascalog.math.stats
  (:use cascalog.api)
  (:require [cascalog.ops :as c]))

(def variance
  "Predicate macro that calculates the variance of the supplied input
   var."
  (<- [!val :> !var]
      (* !val !val :> !squared)
      (c/sum !squared :> !square-sum)
      (c/count !count)
      (c/avg !val :> !mean)
      (* !mean !mean :> !mean-squared)
      (div !square-sum !count :> !i)
      (- !i !mean-squared :> !var)))



(defbufferop emit-first
  [tuples]
  [first (tuples)])

(defn summary2
  [src]
  (let [q (<- [?val] (src ?val))]
    (?<- (stdout) [?min ?avg ?med ?max]
        (q ?val)
        (c/min ?val :> ?min)
        (c/max ?val :> ?max)
        (c/avg ?val :> ?avg)
        (emit-first ?val :> ?med))))


(defn quantile_idx
  [count]
  (<- [?idx_first ?idx_med ?idx_last] (count ?cnt)
      (* ?cnt 0.5 :> ?idx_med_raw)
      (* ?cnt 0.25 :> ?idx_first_raw)
      (* ?cnt 0.75 :> ?idx_last_raw)
      (int ?idx_med_raw :> ?idx_med)
      (int ?idx_first_raw :> ?idx_first)
      (int ?idx_last_raw :> ?idx_last)))

  
(defn summary
  " predicate macro that calculates the .... of the supllied input var"
  [src]
  (let [q (<- [?val] (src ?val))
        q_count (<- [?cnt] (q ?val) (c/count :> ?cnt))
        q_idx (quantile_idx q_count)
        ;res (first (first (first (??- q_idx))))
        q_first (c/first-n q  :sort ["?val"] :reverse true)
        ]
    (?- (stdout) rest)
    (<- [?min ?avg  ?max]
        (src ?val)
        (c/min ?val :> ?min)
        (c/avg ?val :> ?avg)
        ;(q_first ?foo)
        ;(c/max ?foo :> ?med)                                       
        (c/max ?val :> ?max))))
