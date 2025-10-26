(ns demos.conj-2025.gen
  (:require 
   [demos.conj-2025.csv :refer [statistics-resource write-csv-statistic]]
   [demos.conj-2025.random :refer [tree-split-rte-linear
                                   tree-split-rte-gaussian
                                   tree-split-rte-inv-gaussian
                                   flajolet-rte-by-size
                                   comb-rte]]
   [dot]
   [lock]
   [genus :as gns]
   [rte-construct :as rte]
   [rte-graphviz :refer [rte-view]]
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
                futures (for [q (range lot)]
                          (future (do
                                    (lock/with-lock
                                      (println [:future :algo algo 
                                                :leaves size
                                                :rep (/ r num-repetitions)
                                                :iteration (/ q lot)]))
                                    (write-csv-statistic (fn [] ((gen-rte algo) size))
                                                         algo
                                                         (csv algo prefix)))))]
          ]
    (doall (map deref futures))))

(defn sample-view [num-leaves]
  (doseq [[algo gen] gen-rte
          :let [rte (gen num-leaves)
                _ (rte-view rte :title algo)
                dfa (rte/rte-to-dfa rte)]]
    (dot/dfa-view dfa algo)))
    
;;(sample-view 28)
