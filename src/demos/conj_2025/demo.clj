(ns demos.conj-2025.demo
  (:require [clojure.pprint :refer [cl-format]]
            [dot]
            [rte-graphviz :refer [rte-view]]
            [xymbolyco :as xym]
            [rte-construct :as rte]
            
))

(defn demo-1 []
  (let [all-int '(:* (satisfies int?))
        starts-0 '(:cat (= 0) (:* :sigma))
        termine-0 '(:cat (:* :sigma) (= 0))
        rte-even-parity '(:* (:cat :sigma :sigma))
        rte-odd-parity (rte/Cat :sigma rte-even-parity)
        r1 (rte/And starts-0
                    rte-odd-parity)
        r2 (rte/And termine-0
                 rte-even-parity)
        rte (rte/And all-int
                     (rte/Or r1
                             r2))
        dfa (rte/rte-to-dfa rte)
        ]
    (rte-view rte :title "rte")
    (dot/dfa-view dfa :title "dfa")))

(demo-1)
