;; Copyright (c) 2020,21 EPITA Research and Development Laboratory
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without restriction,
;; including without limitation the rights to use, copy, modify, merge,
;; publish, distribute, sublicense, and/or sell copies of the Software,
;; and to permit persons to whom the Software is furnished to do so,
;; subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(ns clojure-rte.xymbolyco
  "Implementation of Sigma Complete Deterministic Finite Automata
  to represent and compute regular type expressions."
  (:refer-clojure :exclude [complement])
  (:require [clojure-rte.cl-compat :as cl]
            [clojure-rte.util :refer [fixed-point member group-by-mapped
                                      defn-memoized print-vals
                                      exists setof]]
            [clojure-rte.genus :as gns]
            [clojure.pprint :refer [cl-format]]
            [clojure-rte.bdd :as bdd]
            [clojure.set :refer [union difference intersection]]
            [backtick :refer [template]]
            ))

(defrecord State 
    ;; :index -- the index of this state in the array
    ;; :accepting - Boolean true/false indicating whether this state is a
    ;;     final/accepting state.
    ;; :initial - Boolean indicating initial state
    ;; :pattern -- the derivative value representing an rte pattern matching
    ;;     any tail of the input sequence which is accepting from this point
    ;;     onward.
    ;; :transitions -- A list of pairs, each pair is a 2 element array of the form
    ;;     [type next-state], e.g., [clojure.lang.Keyword 1]
    ;;     which means if the value at the head of the sequence is of type
    ;;     clojure.lang.Keyword, then go to state 1.  The type is some type designator
    ;;     as specified in the genus library.  the state index is some index of the state
    ;;     array representing the finite atomaton.
    [index initial accepting pattern transitions])

(defmethod print-method State [v w]
  (.write w (format "#<State %s>" (:index v))))

(defrecord Dfa [pattern canonicalized states exit-map combine-labels])

(defn record-name
  []
  Dfa)

(defn exit-value
  "Given a Dfa and either a State or state-id (integer), compute the exit value of
  the state by calling the function :exit-map in the dfa."
  [dfa state]
  (if (instance? State state)
    (exit-value dfa (:index state))
    ((:exit-map dfa) state)))

(defn state-by-index
  "Return the State object of the Dfa whose :index is the given index."
  [dfa index]
  (assert (instance? Dfa dfa))
  ((:states dfa) index))

(defn states-as-map
  "Return a map index -> state"
  [dfa]
  (assert (instance? Dfa dfa))
  (assert (map? (:states dfa)))
  (:states dfa))

(defn states-as-seq
  "Return a sequence of states which can be iterated over."
  [dfa]
  (assert (instance? Dfa dfa) (cl-format false "states-as-seq: expecting Dfa, not ~A ~A" (type dfa) dfa))
  (assert (map? (:states dfa)))
  (vals (:states dfa)))

(defn ids-as-seq
  "Return a sequence of ids of the states which can be iterated over."
  [dfa]
  (assert (instance? Dfa dfa))
  (map :index (states-as-seq dfa)))

(defn check-dfa
  "assert that no transition references an invalid state"
  [dfa]
  (assert (instance? Dfa dfa))
  (assert (:combine-labels dfa) (format "missing :combine-labels in Dfa %s" dfa))
  (assert (map? (:states dfa))  (cl-format false "states must be a map, not a ~A: ~A" (type (:states dfa)) (:states dfa)))
  (let [ids (set (ids-as-seq dfa))]
    (doseq [id (keys (states-as-map dfa))
            q (get (states-as-seq dfa) id)]
      (assert (instance? State q) (cl-format false "expecting a State, got a ~A, ~A" (type q) q))
      (assert (:index q) (format "state %s has emtpy :index" q))
      (assert (= q (state-by-index dfa (:index q)))
              (format "state %s disagrees with its index %s" q (:index q)))

      (assert (= (:transitions q)
                 (distinct (:transitions q)))
              (cl-format false "~A transitions contains a duplicate: ~A"
                         q (:transitions q)))
      (doseq [[label-1 dst-1] (:transitions q)
              [label-2 dst-2] (:transitions q)
              :when (not (= [label-1 dst-1]
                            [label-2 dst-2]))]
        (assert (bdd/type-disjoint? label-1 label-2)
                (cl-format false "overlapping types ~A vs ~A in ~A transitions ~A"
                           label-1 label-2 q (:transitions q))))
      (doseq [:let [trans-labels (map first (:transitions q))]
              [label freq] (frequencies trans-labels)]
        (assert (= 1 freq)
                (cl-format false "label ~A appears ~D times in transitions of ~A: transitions=~A"
                           label freq q (:transitions q)))))

    (doseq [q (states-as-seq dfa)
            [label dst-id] (:transitions q)]
      (assert (member dst-id ids) (format "transition %s leads to invalid state: states are %s"
                                          [label dst-id]
                                          ids)))
    dfa))

(defn make-dfa
  "Dfa factory function, which checks consistency"
  ([map]
   (make-dfa {} map))
  ([old-dfa new-attribute-map]
   (let [new-dfa (map->Dfa (merge old-dfa new-attribute-map))]
     (check-dfa new-dfa)
     new-dfa)))

(defn serialize-state
  "Serialize a State for debugging"
  [state]
  [:index (:index state)
   :accepting (:accepting state)
   :pattern (:pattern state)
   :transitions (:transitions state)])

(defn serialize-dfa
  "Serialize a Dfa for debugging"
  [dfa]
  (map serialize-state (states-as-seq dfa)))

(defn-memoized [optimized-transition-function -optimized-transition-function]
  "Given a set of transitions each of the form [type-designator state-index],
  return a indicator function which can be called with an candidate element
  of a sequence, and the function will return the state-index.  When called
  with the candidate object, will not evaluate any type predicate more than
  once."
  
  ;; TODO, if we could know whether to trust that the transitions are
  ;;  already disjoint this function could be made much faster.
  ;;  It is not necessary to know whether the transitions cover the
  ;;  universe because the indicator function has a second argument
  ;;  to return if there is no match.
  [transitions promise-disjoint default]
  (bdd/with-hash []
    (letfn [(type-intersect [t1 t2]
              (list 'and t1 t2))
            (type-and-not [t1 t2]
              (if (= :empty-set t2)
                t1
                (list 'and t1 (list 'not t2))))
            ;; local function find-duplicates
            (find-duplicates [items]
              (loop [items items
                     duplicates []]
                (cond (empty? items)
                      (distinct duplicates)

                      (member (first items) (rest items))
                      (recur (rest items)
                             (conj duplicates (first items)))

                      :else
                      (recur (rest items)
                             duplicates))))

            ;; local function gen-function
            (gen-function []
              (let [state-id->pseudo-type (into {} (for [[_type state-id] transitions
                                                         :let [tag (gensym "pseudo-")]]
                                                     [state-id (template (satisfies ~tag))]))
                    pseudos (for [[_ tag] state-id->pseudo-type]
                              tag)
                    pseudo-type->state-id (into {} (for [[state-id tag] state-id->pseudo-type]
                                                     [tag state-id]))
                    pseudo-type-functions (for [[_ [_ function-designator]] state-id->pseudo-type]
                                            function-designator)
                    old-label-< bdd/*label-<*
                    bdd (binding [gns/*pseudo-type-functions* (concat pseudo-type-functions
                                                                      gns/*pseudo-type-functions*)
                                  bdd/*label-<* (fn [t1 t2]
                                                  (cond (and (member t1 pseudos)
                                                             (member t2 pseudos))
                                                        (old-label-< t1 t2)

                                                        (member t1 pseudos)
                                                        false

                                                        (member t2 pseudos)
                                                        true

                                                        :else
                                                        (old-label-< t1 t2)))]
                          (first
                           (reduce
                            (fn [[accum-bdd previous-types] [type state-id]]
                              [(bdd/or accum-bdd
                                       (bdd/bdd (type-and-not 
                                                 (type-intersect type
                                                                 (state-id->pseudo-type state-id))
                                                 (if promise-disjoint
                                                   ;; This is an optimization
                                                   ;; see issue
                                                   ;; https://gitlab.lrde.epita.fr/jnewton/clojure-rte/-/issues/27
                                                   ;; If the given types are already promised to be disjoint,
                                                   ;;  then no need to do an expensive Bdd operation
                                                   :empty-set
                                                   ;; in case the types are not disjoint
                                                   (cons 'or previous-types)))))
                               (cons type previous-types)])
                            [false '(:empty-set)] ;; initial bdd and empty-type
                            transitions)))]
                ;;(clojure-rte.dot/bdd-to-dot bdd :title (gensym "bdd") :view true)
                (fn [candidate default]
                  (loop [bdd' bdd
                         lineage ()]
                    (cl/cl-cond
                     ((member bdd' '(true false))
                      ;; transitions not exhaustive
                      default)
                     ((pseudo-type->state-id (:label bdd') false)) ;; this is the return value of the (fn [] ...)
                     ((gns/typep candidate (:label bdd'))
                      (recur (:positive bdd')
                             (cons (:label bdd') lineage)))
                     (true
                      (recur (:negative bdd')
                             (cons (list 'not (:label bdd')) lineage))))))))
            ]
      (let [types (map first transitions)
            duplicate-types (find-duplicates types)
            inhabited-types (delay (filter (fn [td] (gns/inhabited? td false))
                                           types))
            consequents (map second transitions)]
        
        (cond
          (not= (count consequents) (count (distinct consequents)))
          ;; If there is a duplicate consequent, then the corresponding types can be unioned.
          (-optimized-transition-function (for [[consequent transitions] (group-by second transitions)]
                                            [(gns/create-or (map first transitions)) consequent])
                                          promise-disjoint
                                          default)

          (empty? duplicate-types)
          (gen-function)
          
          (not-empty (intersection duplicate-types inhabited-types))
          ;; if some duplicate types are inhbited
          (throw (ex-info (cl-format false "transitions ~A has a duplication of types: ~A"
                                     transitions (find-duplicates types))
                          {:transitions transitions
                           :duplicates (find-duplicates types)}))

          ;; if all duplicate types are empty types
          :else
          (gen-function))))))

(defn delta
  "Given a state and target-label, find the destination state (object of type State)"
  [dfa source-state target-label]
  (let [[_ index] (first (filter (fn [[label _dst-index]] (= label target-label))
                                 (:transitions source-state)))]
    (state-by-index dfa index)))

(defmethod print-method Dfa [v w]
  (.write w (format "#<Dfa %d states>" (count (states-as-seq v)))))

(defn split-eqv-class
  "Given a set of objects, return a set of subsets thereof which is a partition of
  the given set.   Every element in any set has the same value under f, and
  the value under f is different for any distinct subsets.  f is not called
  if the size of the given set is 1."
  [objects f]
  (if (= 1 (count objects))
    #{ objects}
    (set (map set (vals (group-by f objects))))))

(defn find-eqv-class
  "Given a sequence of sequences, find the leaf level
  sequence which contains the given element"
  [partition target]
  (first (filter (fn [eqv-class]
                   (member target eqv-class)) partition)))

(defn find-hopcroft-partition
  "Apply the Hopcroft partition algorithm to the states of the given
  Dfa to return a set of eqv-classes.  This set of eqv-classes is a partition
  of the initial set of states.
  Each eqv-class is a set of states.
  This function returns a sequence of sets, where each set contains
  State's, not state-id's, E.g., ( ... #{#<State 9> #<State 2>} #{#<State 0} ...),
  not (... #{9 2} #{0} ...)"
  [dfa]
  (let [[finals non-finals] (map (group-by (comp boolean :accepting)
                                           (states-as-seq dfa)) [true false])
        pi-0 (conj (split-eqv-class finals
                                    (fn [state]
                                      (exit-value dfa state)))
                   non-finals)]
    (letfn [(refine [partition]
              (letfn [(phi [source-state label]
                        ;; find the element of partition, itself an equivalence class, which
                        ;;   which contains the destination state of the transition
                        ;;   whose source and label are given
                        (find-eqv-class partition (delta dfa source-state label)))
                      (Phi' [s]
                        (for [[label _dst-id] (:transitions s)]
                          [label (phi s label)]))
                      (Phi [s]
                        (for [[k pairs] (group-by second (Phi' s))
                              :let [labels (map first pairs)
                                    label (reduce (:combine-labels dfa) labels)]]
                          [label k]))
                      (repartition [eqv-class]
                        (split-eqv-class eqv-class Phi))]
                (mapcat repartition partition)))]
      (fixed-point pi-0 refine =))))

(defn min-state
  "Compute the minimimum :index of the given set of states"
  [eqv-class] ;; a set of states
  (reduce min (map :index eqv-class)))

(defn find-sink-states
  "Find the set (as sequence) of all sink states in the given Dfa.
  A sink state is not a final state,
  is not an initial state,
  and all its transitions point to itself."
  [dfa]
  (assert (instance? Dfa dfa))
  (filter (fn [q]
            (and (not (:accepting q))
                 (not (= 0 (:index q)))
                 (every? (fn [[_label dst]]
                           (= dst (:index q)))
                         (:transitions q))))
          (states-as-seq dfa)))

(defn complete-state?
  [state]
  (let [labels (map first (:transitions state))]
    (and (not-empty labels)
         (or (member :sigma labels)
             (bdd/type-subtype? :sigma (cons 'or labels))))))

(defn find-incomplete-states
  "Return a sequence containing all the State's of the given Dfa which are not complete,
  according to the function complete-state?"
  [dfa]
  (remove complete-state? (states-as-seq dfa)))

(defn ensure-sink-state
  "Return one of the sink states of the Dfa if there is one,
  or allocate a sink state, without explicitly adding it to the Dfa.
  The calling function is responsible for inserting the newly created
  sink state into the Dfa if necessary."
  [dfa]
  (assert (instance? Dfa dfa))
  (assert (every? (fn [q] (instance? State q)) (find-sink-states dfa))
          (cl-format false "not all sink states are States: ~A: ~A"
                     (map type (find-sink-states dfa))
                     (find-sink-states dfa)))
  (or (first (find-sink-states dfa))
      (let [states (states-as-map dfa)
            num-states (count states)
            available-ids (filter (fn [id]
                                    ;; find smallest non-negative integer which is not already
                                    ;; a state-id in this Dfa
                                    (and (> id 0)
                                         (not (get states id false)))) (range 1 (+ 1 num-states)))
            sink-id (first available-ids)]
        (map->State {:index sink-id
                     :accepting false
                     :transitions (list [:sigma sink-id])}))))

(defn extend-with-sink-state
  "If the given Dfa has a sink state, then just return the Dfa, else
  allocate a sink state, and return a new Dfa which differs from the given
  Dfa on in that the sink state has been appended."
  [dfa]
  (let [sink-state (ensure-sink-state dfa)]
    (assert (instance? State sink-state) (cl-format false "expecting a State, not a ~A: ~A"
                                                    (type sink-state) sink-state))
    
    (cond
      (member sink-state (states-as-seq dfa))
      dfa

      (state-by-index dfa (:index sink-state))
      (throw (ex-info (cl-format false "ensure-sink-state failed to create a proper state index") {}))

      :else
      (make-dfa dfa
                {:states (assoc (states-as-map dfa) (:index sink-state) sink-state)}))))

(defn complete
  "Render complete the given Dfa.
  If it is already complete, then simply return it,
  otherwise compute a new Dfa which has been completed, but
  adding a sink state if necessary and add at most one transition
  per state to the sink state."
  ([dfa]
   (let [incomplete (find-incomplete-states dfa)]
     (if (empty? incomplete)
       dfa
       (complete dfa incomplete))))
  ([dfa incomplete]
   (let [sink-state (ensure-sink-state dfa)]
     (make-dfa dfa
               {:states
                (let [current-states (states-as-seq dfa)
                      extended-states (if (member sink-state current-states)
                                        current-states
                                        (conj current-states sink-state))]
                  (into {}
                        (for [q extended-states]
                          ;; allocate a pair [index state]
                          ;;   where state is either the State q, or a new State
                          ;;   derived from it by adding a transition so that the
                          ;;   union of the transition labels is now :sigma
                          [(:index q)
                           (if (member q incomplete)
                             (let [existing-labels (map first (:transitions q))
                                   new-label (gns/canonicalize-type
                                              (template (and :sigma (not (or ~@existing-labels)))) :dnf)]
                               (if (= :empty-set new-label)
                                 q
                                 (assoc q
                                        :transitions (conj (:transitions q)
                                                           [new-label (:index sink-state)]))))
                             q)])))}))))

(defn merge-parallel-transitions [transitions pretty-or]
  ;; if there are two transitions with the same src/dest, then
  ;;   combine the labels with (or ...), or (:or ...) depending on the given pretty-or
  ;; We do not, currently, try to reduce the union type.
  ;; It may be something like:
  ;; (or Long
  ;;     (and (not Long) (not String)))
  ;; which *could* be reduced to simply: Long
  (let [grouped (group-by (fn [[_ to]]
                            to)
                          transitions)]
    
    (map (fn [[to transitions]]
           [(pretty-or (map first transitions)) to])
         grouped)))

(defn minimize
  "Accepts an object of type Dfa, and returns a new object of type Dfa
  implementing the minimization of the state machine according to the
  Hopcroft minimization algorithm."
  [dfa]
  (let [pi-minimized (find-hopcroft-partition dfa)
        ids (map min-state pi-minimized)
        partitions-map (zipmap ids pi-minimized)
        ids-map (zipmap pi-minimized ids)]
    (assert (sequential? pi-minimized))
    (letfn [(merge-parallel [transitions]
              ;; if there are two transitions with the same src/dest, then
              ;;   combine the labels with (or ...)
              ;; We do not, currently, try to reduce the union type.
              ;; It may be something like:
              ;; (or Long
              ;;     (and (not Long) (not String)))
              ;; which *could* be reduced to simply: Long
              (let [grouped (group-by (fn [[from _ to]]
                                        [from to])
                                      transitions)]
                
                (map (fn [[[from to] transitions]]
                       [from (gns/create-or (map second transitions)) to])
                     grouped)))
            (new-id [state]
              (assert (instance? State state))
              (ids-map (find-eqv-class pi-minimized state)))]
      (let [new-fids (mapcat (fn [id eqv-class]
                               ;; does there exists an s in eqv-class such that (:accepting s)
                               (if (some :accepting eqv-class)
                                 (list id)
                                 nil))
                             ids pi-minimized)
            new-proto-delta (distinct (for [q (states-as-seq dfa)
                                            :let [new-src-id (new-id q)]
                                            [label dst-id] (:transitions q)
                                            ]
                                        [new-src-id
                                         label
                                         (new-id (state-by-index dfa dst-id))]))
            
            grouped (group-by (fn [[new-src-id _ _]] new-src-id) new-proto-delta)
            new-state-ids (for [id ids
                                :let [transitions (grouped id)]
                                :when (or transitions
                                          (member id new-fids)
                                          (= 0 id))
                                ]
                            id)
            new-states (for [id new-state-ids
                             :let [transitions (merge-parallel (grouped id))
                                   new-transitions (filter (fn [[_ dst-id]]
                                                             (member dst-id new-state-ids))
                                                           (map rest transitions))]
                             ]
                         
                         [id (map->State
                              {:index id
                               :initial (= 0 id)
                               ;; if any of (partitions-map id) has a :pattern, use that one,
                               ;;   it is as good as any.  Important to note here is that we
                               ;;   MUST NOT attempt to combine the patterns.
                               :pattern (some :pattern (partitions-map id))
                               :accepting (member id new-fids)
                               :transitions new-transitions})])
            ]
        (make-dfa dfa { :exit-map (into {} (map (fn [id]
                                                  [id (exit-value dfa id)])
                                                new-fids))
                       :states
                       (into {} new-states)})))))

(defn trim
  "Creates a new Dfa from the given Dfa containing only accessible and co-accessible
  states.  Warning, this removes the sink state if there is one.  The result is
  that the computed Dfa may not any longer be complete.
  Don't remove the initial state."
  [dfa]
  (assert (instance? Dfa dfa) (cl-format false "trim: expecting Dfa, not ~A ~A" (type dfa) dfa))
  (let [transition-pairs (mapcat (fn [q]
                                   (map (fn [[_ dst-id]]
                                          [(:index q) dst-id])
                                        (:transitions q)))
                                 (states-as-seq dfa))
        forward-map (group-by-mapped first second transition-pairs)
        backward-map (group-by-mapped second first transition-pairs)]
    (letfn [(trace-fb [states fb-map skip]
              (loop [states states
                     done #{}]
                (if (empty? states)
                  done
                  (let [next-states (mapcat (fn [id]
                                              (if (member id done)
                                                nil
                                                (fb-map id))) states)
                        new-next-states (difference (set next-states) done skip)]
                    (recur new-next-states (union done states))))))
            (trace-forward [states]
              (trace-fb states forward-map {}))
            (trace-backward [states skip]
              (trace-fb states backward-map skip))]
      (let [
            ;; Trace forward from initial state, collecting all states reached.
            ;; These are the accessible states.
            accessible (trace-forward #{0})
            final-accessible (clojure.set/intersection accessible
                                                       (set (map :index (filter :accepting (states-as-seq dfa)))))
            ;; trace backward starting from the set of all final states which are
            ;; co-accessible, collecting all states.  But do not traverse into
            ;; states which are not accessible.  This computes the set
            ;; of states which are both accessible and co-accessible
            useful (conj (trace-backward final-accessible (difference (set (ids-as-seq dfa))
                                                                      accessible))
                         0)
            new-fids (filter (fn [id] (:accepting (state-by-index dfa id)))
                             useful)
            ]
        (assert (not (= 0 (count useful))))
        ;; now build a new Dfa, omitting any state not in the co-accessible list
        ;; any transition going to a state which has being removed, gets
        ;; diverted to the sink state.
        (make-dfa dfa
                  {:exit-map (into {} (map (fn [id]
                                             [id (exit-value dfa id)])
                                           new-fids)) ;; map each of new-fids to the old value returned from the exit-map
                   :states
                   (into {} (map (fn [id]
                                   (let [state (state-by-index dfa id)]
                                     (assert state)
                                     [id (map->State
                                          (assoc state
                                                 :index id
                                                 :accepting (member id new-fids)
                                                 :initial (= id 0)
                                                 :transitions (filter (fn [[_label dst-id]]
                                                                        (member dst-id useful))
                                                                      (:transitions state))))]))
                                 useful))})
        ))))

(defn complement
  "Accepts an object of type Dfa, and returns a new object of type Dfa
  whose recognition languages is the complement of the given Dfa.
  This is done by replacing accepting states with non-accepting states
  and non-accepting with accepting.  The transformation looses exit-value
  information."
  [dfa]
  (let [dfa-complete (complete dfa)]
    (trim
     (make-dfa dfa
               {:states (into {} (map (fn [[_index state]]
                                        [(:index state)
                                         (map->State {:index (:index state)
                                                      :initial (:initial state)
                                                      :accepting (not (:accepting state))
                                                      :transitions (:transitions state)})])
                                      (:states dfa-complete)))
                :pattern (list 'not (:pattern dfa)) }))))

(defn intersect-labels
  ""
  [label-1 label-2]
  (gns/canonicalize-type (list 'and label-1 label-2)))

(defn cross-intersection
  "Compute a sequence of type designators corresponding to all the
  inhabited intersesection (and A B) with A coming from type-designators-1
  and B coming from type-designators-2"
  [type-designators-1 type-designators-2]
  (for [
        label-1 type-designators-1
        label-2 type-designators-2
        :when (not (bdd/type-disjoint? label-1 label-2))]
    (intersect-labels label-1 label-2)))

(defn synchronized-product
  "Assuming that the given Dfas are complete, we compute the syncronized cross product SXP
    of the two Dfas.
  f-arbitrate-accepting - binary function which accepts two Boolean values, [a1,a2]
    Then function is called when determining whether a computed state in the SXP
    should be an accepting state.  a1 indicates whether the state from dfa-1 is
    accepting.  a2 indicates whether the state from dfa-2 is accepting.
    To effectuate the intersection of dfa-1 with dfa-2, f-arbitrate-accepting should
    perform an (and a1 a2).
  f-arbitrate-exit-value - binary function called with [q1,q2].  q1 is an accepting state
    of dfa-1.  q2 is an accepting state in dfa-2.
    f-arbitrate-exit-value is called when q1 and q2 are both accepting state or
      when neither is an accepting state.   In the case that only q1 or only q2
      is an accepting state, this accepting state's exit value is used in the SXP.
      f-arbitrate-exit-value should return the exit value for the state in the SXP."
  [dfa-1 dfa-2 f-arbitrate-accepting f-arbitrate-exit-value]
  (letfn [(compute-state-transitions [state-1 state-2 state-ident-map]
            (for [[label-1 dst-1] (:transitions state-1)
                  [label-2 dst-2] (:transitions state-2)
                  :when (not (bdd/type-disjoint? label-1 label-2))
                  :let [label-sxp (intersect-labels label-1 label-2)]
                  :when (gns/inhabited? label-sxp true)
                  ]
              [label-sxp (state-ident-map [dst-1 dst-2])]))
          (accumulate-states [initial-id-pair state-ident-map ident-state-map]
            ;; Returns a sequence of pairs [id state], one for each
            ;;   state in the SXP Dfa being create.
            ;;
            ;; This local function allocates the states in the SXP Dfa.
            ;; It uses a breadth-first-search starting at the initial state [0 0].
            ;; The result is that only reachable (accessible) states get created.
            ;; However, it may create some non-co-accessible states, having no path
            ;; to a final state.  In particular, it will create at least one sink-state
            ;; if dfa-1 and dfa-2 have sink-states.
            ;; Caveat some states may appear accessible but really aren't because
            ;; they might have null-transitions leading to them.   E.g.,
            ;; There might be a type-designator which is the intersection of two
            ;; type-designators which bdd/type-disjoint? is not able to determine
            ;; are really disjoint.   We error on the side of redundancy, leaving
            ;; transitions which will never be taken at runtime.
            (loop [work-id-pairs (list initial-id-pair)
                   done-id-pairs #{}
                   acc-id-state-pairs ()]
              (cond
                (empty? work-id-pairs)
                acc-id-state-pairs

                (member (first work-id-pairs) done-id-pairs)
                (recur (rest work-id-pairs)
                       done-id-pairs
                       acc-id-state-pairs)

                :else
                (let [[[id-1 id-2] & more-pairs] work-id-pairs
                      id-sxp (state-ident-map [id-1 id-2])
                      state-1 (state-by-index dfa-1 id-1)
                      state-2 (state-by-index dfa-2 id-2)
                      new-transitions (compute-state-transitions state-1 state-2 state-ident-map)
                      ]
                  (recur (concat more-pairs (map (comp ident-state-map second) new-transitions))
                         (conj done-id-pairs [id-1 id-2])
                         (conj acc-id-state-pairs
                               [id-sxp (map->State
                                        {:index id-sxp
                                         :initial (= 0 id-sxp)
                                         :accepting (f-arbitrate-accepting
                                                     (:accepting state-1)
                                                     (:accepting state-2))
                                         :transitions new-transitions})]))))))
          ]

    (let [sxp-pairs (sort (fn [[a b] [x y]]
                            ;; sort first by sum, so that [1 0] preceeds [0 2]
                            (cond
                              (not (= (+ a b) (+ x y)))
                              (< (+ a b) (+ x y))
                              
                              (= a x)
                              (< b y)
                              
                              :else
                              (< a x)))
                          ;; all possible states in SXP, even those which
                          ;;   are not accessible or co-accessible
                          (for [id-1 (ids-as-seq dfa-1)
                                id-2 (ids-as-seq dfa-2)]
                            [id-1 id-2]))
          state-ident-map  (zipmap sxp-pairs (range)) ;; [id id] -> id
          ident-state-map  (zipmap (range) sxp-pairs) ;; id -> [id id]
          new-id-state-pairs (accumulate-states [0 0] state-ident-map
                                                ident-state-map)
          new-exit-map (for [[sxp-id new-state] new-id-state-pairs
                             :when (:accepting new-state)
                             :let [[id-1 id-2] (ident-state-map sxp-id)
                                   state-1 (state-by-index dfa-1 id-1)
                                   state-2 (state-by-index dfa-2 id-2)]]
                         [sxp-id (cond
                                   (= (boolean (:accepting state-1))
                                      (boolean (:accepting state-2)))
                                   (f-arbitrate-exit-value state-1 state-2)
                                   
                                   (:accepting state-1)
                                   (exit-value dfa-1 id-1)

                                   (:accepting state-2)
                                   (exit-value dfa-2 id-2))])
          ]
      (assert (member 0 (ids-as-seq dfa-1)))
      (assert (member 0 (ids-as-seq dfa-2)))
      (assert (= 0 (state-ident-map [0 0]))
              (cl-format false
                         "expecting [0 0] maps to 0, in ~A"
                         state-ident-map))
      (assert (= [0 0] (ident-state-map 0))
              (cl-format false
                         "expecting 0 maps to [0 0], in ~A"
                         ident-state-map))

      (bdd/with-hash []
        (make-dfa dfa-1
                  {:exit-map (into {} new-exit-map)
                   :states (into {} new-id-state-pairs)})))))

(defn synchronized-union
  "Compute the union of two Dfas.  I.e., compute a Dfa which
  will recognize sequences which either dfa-1 or dfa-2 recognizes.
  If some sequence is recognized both by dfa-1 and dfa-2, then
  the exit value is determined by dfa-1, and the exit-value of
  dfa-2 is silently ignored."
  [dfa-1 dfa-2]
  (synchronized-product dfa-1 dfa-2
                        (fn [a b]
                          (or a b))
                        (fn [q1 _q2]
                          ((:exit-map dfa-1)
                           (:index q1)))))

(defn synchronized-intersection
  "Compute the intersection of two Dfas. I.e., compute the Dfa which
  will recognized any sequence which is recognized by dfa-1 and also
  by dfa-2."
  [dfa-1 dfa-2]
  (synchronized-product dfa-1 dfa-2
                        (fn [a b]
                          (and a b))
                        (fn [q1 _q2]
                          ((:exit-map dfa-1)
                           (:index q1)))))

(defn synchronized-xor
  "Compute the xor of two Dfas. I.e., compute the Dfa which
  will recognized any sequence which is recognized by dfa-1 or
  by dfa-2 but not by both."
  [dfa-1 dfa-2]
  (synchronized-product dfa-1 dfa-2
                        (fn [a b]
                          (or (and a (not b))
                              (and b (not a))))
                        (fn [q1 _q2]
                          ((:exit-map dfa-1)
                           (:index q1)))))

(defn paths-to-accepting
  "Generate a lazy list of sequences, each sequence being a list of states
  starting at a final/accepting state of the given Dfa, and ending at the
  inital state.  Each is a path, the reverse of which represents an accepting
  computation through the dfa.  Between consecutive states there is a type
  designators (not given in the return value, but which can be found in the Dfa)
  No state appears twice in the same path, thus no loops.
  Each type designator is either is known to be non-inhabited; i.e., one (or more)
  such types might respond :dont-know to the gns/inhabited? predicate, but never
  responds false."
  [dfa]
  (letfn [(extend-path-1 [path]
            (for [[type-designator next-state-id] (:transitions (first path))
                  :when (not (exists [st path]
                                     (= next-state-id (:index st))))
                  :when (gns/inhabited? type-designator true) ;; include paths where inhabited? returns :dont-know
                  ]
              (cons (state-by-index dfa next-state-id) path)))
    
          (extend-paths-1 [paths]
            (mapcat extend-path-1 paths))
  
          (extend-paths
            [paths]
            (if (empty? paths)
              paths
              (concat (setof [p paths]
                             (:accepting (first p)))
                      (extend-paths (extend-paths-1 paths)))))]
  
  (let [initials (filter :initial (states-as-seq dfa))]
    (extend-paths (map list initials)))))

(defn dfa-equivalent?
  "Returns a Boolean indicating whether the two given Dfas
  recognize the same language."
  [dfa-1 dfa-2]
  (let [xor (synchronized-xor dfa-1 dfa-2)]
    (or (every? (comp not :accepting) (states-as-seq xor))
        ;; in case there is a non-accessiable accepting state
        (empty? (paths-to-accepting xor)))))
