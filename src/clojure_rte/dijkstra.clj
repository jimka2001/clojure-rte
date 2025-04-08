;; Thanks Michael Ummels, https://github.com/ummels
;; This code is taken  from https://ummels.de/2014/06/08/dijkstra-in-clojure/
;;   but several modifications have been made.

(ns clojure-rte.dijkstra
  (:require [clojure.data.priority-map :refer [priority-map]]
            [clojure-rte.util :refer [member]]))

 
(defn map-vals [m f]
  (into {} (for [[k v] m] [k (f v)])))
 
(defn remove-keys [m pred]
  (select-keys m (filter (complement pred) (keys m))))
 
(defn dijkstra
  "Computes single-source shortest path distances in a directed graph.

  Given a node n, (f n) should return a map with the successors of n
  as keys and their (non-negative) distance from n as vals.
 
  Returns a map from nodes to their distance from start."
  [start f]
  (loop [q (priority-map start 0) r {}]
    (if-let [[v d] (peek q)]
      (let [dist (-> (f v) (remove-keys r) (map-vals (partial + d)))]
        (recur (merge-with min (pop q) dist) (assoc r v d)))
      r)))

(defn dijkstra-to-final
  "Like `dijkstra` function above, except that the search aborts
  when a path is found to one of the given final states.
  Returns [s d] if the shortest path to a final state is to s, and
  where d is the shortest distance."
  [start f finals]
  (loop [q (priority-map start 0) r {}]
    (let [[v d :as found] (peek q)]
      (cond
        (not found)
        nil

        (member v finals)
        [v d]

        :otherwise
        (let [dist (-> (f v) (remove-keys r) (map-vals (partial + d)))]
          (recur (merge-with min (pop q) dist) (assoc r v d)))))))

#_((require '[clojure.data.priority-map :refer [priority-map]])

;; code suggested by Eugene Pakhomov, https://clojurians.slack.com/team/U2FRKM4TW
;; https://clojurians.slack.com/archives/C03S1KBA2/p1744060041343069?thread_ts=1744055580.190099&cid=C03S1KBA2


(defn remove-keys [m pred]
  (select-keys m (remove pred (keys m))))

(defn dijkstra
  "Computes single-source shortest path distances in a directed graph.

  Given a node n, (f n) should return a map with the successors of n
  as keys and their (non-negative) distance from n as vals.

  Returns a vector with a map from nodes to their distance from start and a map of prev nodes."
  [start f]
  (loop [q (priority-map start 0)
         r {}
         p {}]
    (if-let [[v d] (peek q)]
      (let [dist (-> (f v) (remove-keys r) (update-vals #(+ d %)))
            [q p] (reduce-kv (fn [[q p] n d]
                               (let [curr-d (q n)]
                                 (if (or (nil? curr-d) (< d curr-d))
                                   [(assoc q n d) (assoc p n v)]
                                   [q p])))
                             [(pop q) p]
                             dist)]
        (recur q (assoc r v d) p))
      [r p]))))
