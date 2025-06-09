(ns xym-tester
  (:require [genus :as gns]
            [genus-tester :refer [gen-type]]
            [xymbolyco :as xym]
            [util :refer [member]]

  ))


(defn build-state-map
  ""
  [transitions initial accepting] ;; transitions is a list of triples of the form [origin-index type-designator target-index]
  (let [grouped (group-by first transitions)
        state-ids (distinct (concat (map first transitions)
                                    (map (fn [triple] (nth triple 2)) transitions)))]
    (letfn [(mutually-exclusive [pairs] ;; pairs of the form [td target-id]
              (loop [pairs pairs
                     acc []]
                (cond (empty? pairs)
                      acc

                      (empty (rest pairs))
                      (recur (rest pairs)
                             (conj acc
                                   (first pairs)))

                      :else
                  (recur (rest pairs)
                         (conj acc
                               [(apply gns/And-not (cons (first (first pairs)) (map first (rest pairs))))
                                (second (first pairs))])))))]

      (into {} (for [origin-index state-ids
                     :let [triples (get grouped origin-index [])]]
                 [origin-index
                  (xym/map->State {:index origin-index
                               :initial (= origin-index initial)
                               :accepting (member origin-index accepting)
                               :transitions (mutually-exclusive (map rest triples))})])))))

;;(let [dfa (gen-dfa 10 25 42 2)]
;;  (dot/dfa-view dfa "random")
;;  (dot/dfa-view (minimize dfa) "random-min"))

(defn gen-dfa
  "Generate a random Dfa having a specified number of states `num-states` and approximately
  `num-transitions` number of transitions."
  [num-states num-transitions exit-value type-size]
  (let [tr1 (- num-transitions num-states)
        transitions (concat (for [k (range tr1)
                                  :let [origin (rand-int (dec num-states))
                                        td (gen-type type-size)
                                        target (rand-int (dec num-states))
                                        ]]
                              [origin td target])
                            (for [id (range (dec num-states))]
                              [id (gen-type type-size) (inc id)]))]
    (xym/map->Dfa 
     {:exit-map {:default exit-value}
      :combine-labels gns/combine-labels
      ;; create map index -> State
      ;; divide transitions into two parts one transition for each state to some later state
      ;; after that transitions between randomly selected states
      :states (build-state-map transitions
                               0
                               [(dec num-states)])})))
