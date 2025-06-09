;; Thanks Michael Ummels, https://github.com/ummels
;; This code is taken  from https://ummels.de/2014/06/08/dijkstra-in-clojure/
;;   but several modifications have been made.
;; Code enhanced by Eugene Pakhomov, https://clojurians.slack.com/team/U2FRKM4TW
;;               or https://truegrok.com/.
;; https://clojurians.slack.com/archives/C03S1KBA2/p1744060041343069?thread_ts=1744055580.190099&cid=C03S1KBA2

(ns dijkstra
  (:require [clojure.data.priority-map :refer [priority-map]]
            [util :refer [member]]))

 
(defn remove-keys [m pred]
  (select-keys m (remove pred (keys m))))

(defn find-path 
  "return a list starting with `start` ending with `v` which represents
  the shortest path from `start` to `v` as indicated in the `p` map.
  The map `p` is the precedence map created by the `dijkstra` function
  and likewise by `dijkstra-to-final`."
  [start v p]
  (loop [v v
         path (list)]
    (if (= v start)
      (conj path start)
      (recur (get p v) (conj path v)))))


(defn dijkstra
  "Computes single-source shortest path distances in a directed graph.

  Given a node n, (f n) should return a map with the successors of n
  as keys and their (non-negative) distance from n as vals.

  Returns a vector with a map from nodes to their distance from start
  and a map of vector to delay object, which will yield a path from
  start to the node in question."
  [start f]
  (letfn [(f-no-self-loop [v] (dissoc (f v) v))]
    ;; f-no-self-loop is now a function which wraps the given function, `f`
    ;;   but removes self loops from its return values
    (loop [q (priority-map start 0)
           r {}
           p {}]
      (if-let [[v d] (peek q)]
        (let [dist (-> (f-no-self-loop v) (remove-keys r) (update-vals #(+ d %)))
              [q p] (reduce-kv (fn [[q p] n d]
                                 (let [curr-d (q n)]
                                   (if (or (nil? curr-d) (< d curr-d))
                                     [(assoc q n d) (assoc p n v)]
                                     [q p])))
                               [(pop q) p]
                               dist)]
          (recur q (assoc r v d) p))
        [r (into {} (for [k (keys p)]
                          [k (delay (find-path start k p))]))]))))

(defn dijkstra-to-final
  "Like `dijkstra` function above, except that the search aborts
  when a path is found to one of the given final states.
  Returns [f d path] if the shortest path to a final state is to f, and
  where d is the shortest distance.
  path is ??? TODO document path"
  [start f finals]
  (letfn [(f-no-self-loop [v] (dissoc (f v) v))]
    (loop [q (priority-map start 0)
           r {}
           p {}]
      (let [[v d :as found] (peek q)]
        (cond
          (not found)
          nil

          (member v finals)
          [v d (find-path start v p)]

          :otherwise
          (let [dist (-> (f-no-self-loop v)
                         (remove-keys r)
                         (update-vals #(+ d %)))
                [q p] (reduce-kv (fn [[q p] n d]
                                   (let [curr-d (q n)]
                                     (if (or (nil? curr-d) (< d curr-d))
                                       [(assoc q n d) (assoc p n v)]
                                       [q p])))
                                 [(pop q) p]
                                 dist)
                ]
            (recur q (assoc r v d) p)))))))
