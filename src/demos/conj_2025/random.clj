(ns demos.conj-2025.random
  (:require [rte-construct :as rte]
            [genus :as gns]
            [clojure.math :refer [random]]
            [genus-tester :refer [*test-types*]]
            [util :refer [member weighted-case]]))



(def inhabited-types (into [] (filter (fn [td] (gns/inhabited? td false)) *test-types*)))
(def union-types (into [] (for [i (range (count inhabited-types))
                                :let [td-1 (nth inhabited-types i)]
                                j (range i (count inhabited-types))
                                :let [td-2 (nth inhabited-types j)]]
                            (gns/Or td-1 td-2))))
(def inhabited-leaves (concat [:sigma :epsilon]
                              inhabited-types
                              union-types))

(defn tree-split-rte [leaves pivot]
  (assert (< 0 leaves))
  (if (= 1 leaves)
    (rand-nth inhabited-leaves)
    (let [left-size (pivot leaves)]
      (assert (< 0 left-size))
      (letfn [(left  [] (tree-split-rte left-size pivot))
              (right [] (tree-split-rte (- leaves left-size) pivot))
              (mid   [] (tree-split-rte leaves pivot))]
        (weighted-case 20 (rte/Cat (left) (right))
                       30 (rte/And (left) (right))
                       30 (rte/Or (left) (right))
                       5  (rte/Star (mid))
                       15 (rte/Not (mid)))))))


      
(defn tree-split-rte-linear [leaves]
  (tree-split-rte leaves (fn [n] (max 1 (rand-int n)))))

(defn tree-split-rte-gaussian [leaves]
  (tree-split-rte leaves (fn [n]
                           (case n
                             (1 2 3)
                             1
                             (max 1 (+ (rand-int (quot n 2))
                                       (rand-int (quot n 2))))))))

;; (tree-split-rte-gaussian 10)


(defn tree-split-rte-inv-gaussian [leaves]
  (tree-split-rte leaves (fn [n]
                           (let [r (max 1 (rand-int (quot n 2)))]
                             (if (= 0 (rand-int 2))
                               r
                               (- n r))))))

(defn comb-rte [leaves]
  (tree-split-rte leaves (fn [n]
                           (case (1 2 3)
                             1
                             2))))

(def flajolet-probability-binary 0.9)

(defn flajolet-rte-by-size [leaves]
  (letfn [(insert [tree d]
            (if (empty? tree)
              [d nil nil]
              (let [[n left right] tree]
                (if (< d n)
                  [n (insert left d) right]
                  [n left (insert right d)]))))
          (to-rte [tree]
            (cond (> (random) flajolet-probability-binary)
                  (weighted-case 50 (rte/Not (to-rte tree))
                                 50 (rte/Star (to-rte tree)))
                  
                  (empty? tree)
                  (rand-nth inhabited-leaves)

                  :else
                  (let [[_ left right] tree]
                    (weighted-case 34 (rte/Cat (to-rte left) (to-rte right))
                                   33 (rte/Or  (to-rte left) (to-rte right))
                                   33 (rte/And (to-rte left) (to-rte right))))))]
    
    (let [skeleton (reduce (fn [acc item]
                             (insert acc item)) nil (shuffle (range (dec leaves))))]
      (to-rte skeleton))))
            
