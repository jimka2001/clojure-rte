(ns xym-tester
  (:require [genus :as gns]
            [dot]
            [clojure.pprint :refer [pprint]]
            [rte-construct :refer [rte-to-dfa]]
            [rte-extract :refer [dfa-to-rte]]
            [genus-tester :refer [gen-inhabited-type
                                  gen-indeterminate-type]]
            [xymbolyco :as xym]
            [util :refer [member time-expr find-first]]

  ))


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

(defn gen-dfa-statistics [& {:keys [num-samples num-states num-transitions exit-value type-size probability-indeterminate]
                             :as options
                             :or {num-samples 100
                                  num-states 10
                                  num-transitions 30
                                  exit-value true
                                  type-size 2
                                  probability-indeterminate 0.15}}]
  (letfn [(hier-size [seq]
            (if (seq? seq)
              (apply + (map hier-size seq))
              1))]
    (dotimes [n num-samples]
      (println [:n n])
      (let [dfa (gen-dfa :num-states num-states
                         :num-transitions num-transitions
                         :exit-value exit-value
                         :type-size type-size
                         :probability-indeterminate probability-indeterminate)
            min-dfa (xym/minimize dfa)
            rte (dfa-to-rte min-dfa)
            [satisfiability path] (get (xym/find-trace-map min-dfa) true)
            size (hier-size rte)]
        (dot/dfa-view min-dfa (format "min-%d-%d" num-states num-transitions))
        (dot/dfa-view dfa (format "dfa-%d-%d" num-states num-transitions))
        (printf "%d;%d;%d;%s\n"
                num-states
                (count (:states min-dfa))
                (reduce + 0 (map (fn [st] (count (:transitions st)))
                                 (xym/states-as-seq min-dfa)))
                satisfiability)))))

(gen-dfa-statistics :num-samples 1
                    :num-states 7 :num-transitions 18 
                    :probability-indeterminate 0.3 :num-transitions 30)

;; time-expr
;; time-fn
(defn time-build-traces [max-num-states]
  (println "=========================================")
  (doseq [num-states (range 2 max-num-states)
          num-transitions (range num-states (* 2 num-states))
          :let [exit-value 42 
                type-size 2
                [dfa-1 t-1] (time-expr (gen-dfa num-states
                                                num-transitions
                                                exit-value
                                                type-size))
                [min-dfa t-2] (time-expr (xym/minimize dfa-1))
                [sm t-3] (time-expr (xym/find-trace-map min-dfa))]]
    (println "------------------------------------")
    (pprint [;;:num-states num-states
             ;;:num-transitions num-transitions 
             :num-minimized-states (count (xym/states-as-seq min-dfa))
             :sm (for [[ev [satisfiability path]] sm]
                   [ev satisfiability])
             :time (+ t-1 t-2 t-3)])
    (if (> (+ t-1 t-2 t-3) 400)
      (dot/dfa-view min-dfa (format "dfa-%d-%d" num-states num-transitions)))

))
                  

;; (time-build-traces 30)


(defn time-build-dfas
  ([max-num-states]
   (time-build-dfas 2 max-num-states))
  ([min-num-states max-num-states]
   (println "=========================================")
   (doseq [num-states (range min-num-states (inc max-num-states))
           num-transitions (range num-states (/ (* num-states num-states) 2) 5)
           :let [exit-value 42 
                 type-size 2
                 [dfa-1 t-1] (time-expr (xym/minimize (gen-dfa num-states
                                                               num-transitions
                                                               exit-value
                                                               type-size)))

                 [sm-1 t-2] (time-expr (xym/find-trace-map dfa-1))

                 [rte t-3] (time-expr (get (dfa-to-rte dfa-1) exit-value :empty-set))

                 [dfa-2 t-4] (time-expr (rte-to-dfa rte))

                 [dfa-xor t-5] (time-expr (xym/synchronized-xor dfa-1 dfa-2))

                 [sm-2 t-6] (time-expr (xym/find-trace-map dfa-xor))

                 total-time (+ t-1 t-2 t-3 t-4 t-5 t-6)
                 ]]
     (let [suffix (format "-%d-%d" num-states num-transitions)]
       (dot/dfa-view dfa-1 (str "dfa-1" suffix))
       (dot/dfa-view dfa-2 (str "dfa-2" suffix))
       (dot/dfa-view dfa-xor (str "dfa-xor" suffix)))
     (println "------------------------------------")
     (println [[ num-states num-transitions ]
               :num-minimized-states (count (xym/states-as-seq dfa-1))
               :sm-1 (for [[ev [satisfiability path]] sm-1]
                       [ev satisfiability])
               :sm-2 (for [[ev [satisfiability path]] sm-2]
                       [ev satisfiability])
               :time total-time])
     )))

;; (time-build-dfas 6 6)
