(ns demos.conj-2025.gen
  (:require 
   [demos.conj-2025.csv :refer [statistics-resource write-csv-statistic]]
   [demos.conj-2025.random :refer [tree-split-rte-linear
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
  (assert (gen-rte algo) (format "invalid algo=%s" algo))
  (doseq [r (range num-repetitions)
          :let [size (+ min-leaf (rand-int (- max-leaf min-leaf)))
                futures (for [_ (range lot)]
                          (future (write-csv-statistic (fn [] ((gen-rte algo) size))
                                                       algo
                                                       (csv algo prefix))))]
          ]
    (doall (map deref futures))))


;;(gen-csv-by-size 1 "tree-split-linear" "" 6 20 30)
;;(write-csv-statistic (fn [] (flajolet-rte-by-size 30))
;;                     "flajolet"
;;                     "flajolet.csv")


