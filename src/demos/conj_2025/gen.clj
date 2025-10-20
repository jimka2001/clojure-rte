(ns clojureconj-2025
  (:require 
   [clojureconj-csv :refer [statistics-resource write-csv-statistic]]
   [clojureconj-random :refer [ tree-split-rte-linear
                               tree-split-rte-gaussian
                               tree-split-rte-inv-gaussian
                               flajolet-rte-by-size
                               comb-rte]]
   [util :refer [member]]))


(def algos ["tree-split-linear"
            "tree-split-gauss"
            "tree-split-inv-gauss"
            "flajolet"
            "comb"])

(defn csv [algo prefix]
  (str statistics-resource prefix algo ".csv"))

(def gen-rte {"tree-split-linear" tree-split-rte-linear
              "tree-split-gauss"  tree-split-rte-gaussian
              "tree-split-inv-gauss" tree-split-rte-inv-gaussian
              "flajolet" flajolet-rte-by-size
              "comb" comb-rte})


(defn gen-csv-by-size [num-repetitions
                      algo
                      prefix
                      lot
                      min-leaf
                      max-leaf]
  (let [futures (for [r (range num-repetitions)
                      :let [size (+ min-leaf (rand-int (- max-leaf min-leaf)))
                            f (future (write-csv-statistic (fn [] (gen-rte algo size))
                                                           algo
                                                           (csv algo prefix)))]
                      ]
                  f)]
    (map deref futures)))


