(ns xym.xym-tester
  (:require [genus.genus :as gns]
            [dot]
            [clojure.pprint :refer [pprint]]
            [rte-construct :refer [rte-to-dfa]]
            [rte-extract :refer [dfa-to-rte]]
            [genus.genus-tester :refer [gen-inhabited-type
                                  gen-indeterminate-type]]
            [xym.xymbolyco :as xym]
            [util :refer [member time-expr find-first]]))

(defn build-state-map
  "This is a helper function for use in `gen-dfa` and is not intended
  for general-purpose use.
  `transitions` is a list of triples of the form [origin-index type-designator target-index]
  We generate a map index->State as needed for the :states argument of map->Dfa.
  However, the given transitions might not specify non-deterministic transitions.
  Therefore we produce a new set of transitions that are deterministic.
  E.g., if we have ([0 x 1]
                    [0 y 2]
                    [0 z 3])
  Then we produce ([0 x 1]
                   [0 (:and y (:not x)) 2]
                   [0 (:and z (:not (:or x y)))])
  Sometimes this :and/:not intersection creates a type designator which is know
  to be vacuous, in which case the transition is omitted.
  For this reason the number of transitions might be reduced,
  and the number of transitions requested in the call to `gen-dfa`
  will fail to be satisfied.
  "
  [transitions initial accepting]
  (let [grouped (group-by first transitions)
        state-ids (distinct (concat (map first transitions)
                                    (map (fn [triple] (nth triple 2)) transitions)))]
    (letfn [(mutually-exclusive [pairs] ;; pairs of the form [td target-id]
              (loop [pairs pairs
                     acc []]
                (let [td (delay (apply gns/And-not (cons (first (first pairs))
                                                        (map first (rest pairs)))))]
                  (cond (empty? pairs)
                        acc

                        (gns/inhabited? @td :dont-know)
                        (recur (rest pairs)
                               (conj acc
                                     [@td (second (first pairs))]))

                        :else
                        (recur (rest pairs) acc)))))]

      (into {} (for [origin-index state-ids
                     :let [triples (get grouped origin-index [])
                           mut-ex (mutually-exclusive (map rest triples))
                           ]]
                 [origin-index
                  (xym/map->State {:index origin-index
                                   :initial (= origin-index initial)
                                   :accepting (member origin-index accepting)
                                   :transitions mut-ex})])))))

;;(let [dfa (gen-dfa 10 25 42 2)]
;;  (dot/dfa-view dfa "random")
;;  (dot/dfa-view (minimize dfa) "random-min"))

(defn gen-dfa
  "Generate a random Dfa having a specified number of states `num-states` and approximately
  `num-transitions` number of transitions.
  `num-transitions` requested number of total transitions.  This might fail to be
  satisfied in the case that unsatisfiable transitions are produced during
  the determinization loop (see `build-state-map`).
  "
  [& {:keys [num-states num-transitions exit-value type-size probability-indeterminate]
      :or {num-states 10
           num-transitions 20
           exit-value true
           type-size 2
           probability-indeterminate 0.15}}]
  (letfn [(choose-type []
            (if (< (rand) probability-indeterminate)
              (gen-indeterminate-type (* 2 type-size))
              (gen-inhabited-type type-size)
              ))]
    (let [tr1 (- num-transitions num-states)
          transitions (concat (for [k (range tr1)
                                    :let [origin (rand-int (dec num-states))
                                          ;; transition satisfiability is either true or :dont-know
                                          ;;  not false.
                                          td (choose-type)
                                          target (rand-int (dec num-states))
                                          ]]
                                [origin td target])
                              ;; generate path from 0 to (num-states - 1)
                              (for [id (range (dec num-states))]
                                [id
                                 (choose-type)
                                 (inc id)]))]
      (xym/map->Dfa 
       {:exit-map {:default exit-value}
        :combine-labels gns/combine-labels
        ;; create map index -> State
        ;; divide transitions into two parts one transition for each state to some later state
        ;; after that transitions between randomly selected states
        :states (build-state-map transitions
                                 0
                                 [(dec num-states)])}))))
