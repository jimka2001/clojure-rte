;; Copyright (c) 2020 EPITA Research and Development Laboratory
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

(ns clojure-rte.dfa
  "Definition of records State and Dfa."
  (:refer-clojure :exclude [complement])
  (:require [clojure-rte.cl-compat :as cl]
            [clojure-rte.util :refer [fixed-point member group-by-mapped print-vals]]
            [clojure-rte.genus :as gns]
            [clojure.pprint :refer [cl-format]]
            [clojure-rte.bdd :as bdd]
            [clojure.set :refer [union difference intersection]]
))

(defrecord State 
    ;; :index -- the index of this state in the array
    ;; :accepting - Boolean true/false indicating whether this state is a
    ;;     final/accepting state.
    ;; :initial - Boolean indicating initial state
    ;; :pattern -- the derivative value representing an rte pattern matching
    ;;     any tail of the input sequence which is accepting from this point
    ;;     onward.
    ;; :pattern -- 
    ;; :transitions -- A list of pairs, each pair is a 2 element array of the form
    ;;     [type next-state], e.g., [clojure.lang.Keyword 1]
    ;;     which means if the value at the head of the sequence is of type
    ;;     clojure.lang.Keyword, then go to state 1.  The type is some value
    ;;     compatible with isa?.  the state index is some index of the state
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

(defn- -optimized-transition-function
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

            ;; local-function pretty-or
            (pretty-or [tds]
              (cond (empty? tds);; should not occur
                    :empty-set
                    (empty? (rest tds))
                    (first tds)
                    :else
                    (cons 'or tds)))
            
            ;; local function gen-function
            (gen-function []
              (let [state-id->pseudo-type (into {} (for [[type state-id] transitions
                                                         :let [tag (gensym "pseudo-")]]
                                                     [state-id `(~'satisfies ~tag)]))
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
            inhabited-types (delay (filter (fn [td] (gns/inhabited? td (constantly false)))
                                           types))
            consequents (map second transitions)]
        
        (cond
          (not= (count consequents) (count (distinct consequents)))
          ;; If there is a duplicate consequent, then the corresponding types can be unioned.
          (-optimized-transition-function (for [[consequent transitions] (group-by second transitions)]
                                            [(pretty-or (map first transitions)) consequent])
                                          promise-disjoint
                                          default)

          (empty? duplicate-types)
          (gen-function)
          
          (not (empty? (intersection duplicate-types inhabited-types)))
          ;; if some duplicate types are inhbited
          (throw (ex-info (cl-format false "transitions ~A has a duplication of types: ~A"
                                     transitions (find-duplicates types))
                          {:transitions transitions
                           :duplicates (find-duplicates types)}))

          ;; if all duplicate types are empty types
          :else
          (gen-function))))))

(def optimized-transition-function 
  "Given a set of transitions each of the form [type-designator state-index],
  return a function which can be called with an candidate element of a sequence,
  and the function will return the state-index.  When called with the
  candidate object, will not evaluate any type predicate more than once.
  The function assumes the types are mutually disjoint and that they partition
  the universe.  I.e., the union of all the types is :sigma, and for any two
  of the types, (a,b), a ^ b = :empty-set."
  ;;(memoize -optimized-transition-function)
  -optimized-transition-function
  )

(defn delta
  "Given a state and target-label, find the destination state (object of type State)"
  [dfa source-state target-label]
  (let [[_ index] (first (filter (fn [[label dst-index]] (= label target-label))
                                 (:transitions source-state)))]
    (state-by-index dfa index)))
  
(defmethod print-method Dfa [v w]
  (.write w (format "#<Dfa %d states>" (count (states-as-seq v)))))

(defn split-eqv-class
  "Given a set of objects, return a set of subsets thereof which is a partition of
  the given set.   Every element in any some set has the same value under f, and
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
                 (every? (fn [[label dst]]
                           (= dst (:index q)))
                         (:transitions q))))
          (states-as-seq dfa)))

(defn complete-state?
  [state]
  (let [labels (map first (:transitions state))]
    (and (not (empty? labels))
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
                                   new-label (if (empty? existing-labels)
                                               :sigma
                                               (bdd/canonicalize-type
                                                `(~'and :sigma (~'not (~'or ~@existing-labels)))))]
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
    (letfn [(pretty-or [rest-args or-keyword]
              (cl/cl-cond
               ((empty? rest-args)
                :sigma)
               ((empty? (rest rest-args))
                (first rest-args))
               (:else
                (conj rest-args or-keyword))))
            (pretty-or-rte [rest-args]
              (pretty-or rest-args :or))
            (pretty-or-type [rest-args]
              (pretty-or rest-args 'or))
            (merge-parallel [transitions]
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
                       [from (pretty-or-type (map second transitions)) to])
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
            new-exit-map (into {}
                               (mapcat (fn [id eqv-class]
                                         (if (some :accepting eqv-class)
                                           (list [id
                                                  ((:exit-map dfa) (:index (first eqv-class)))])
                                           nil))
                                       ids pi-minimized))
            ]
        (let [new-state-ids (for [id ids
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
                                 :pattern (pretty-or-rte (map :pattern (partitions-map id)))
                                 :accepting (member id new-fids)
                                 :transitions new-transitions})])]
          (make-dfa dfa { :exit-map (into {} (map (fn [id]
                                                    [id (exit-value dfa id)])
                                                  new-fids))
                         :states
                         (into {} new-states)}))))))

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
                                                 :transitions (filter (fn [[label dst-id]]
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
               {:states (into {} (map (fn [[index state]]
                                        [(:index state)
                                         (map->State {:index (:index state)
                                                      :initial (:initial state)
                                                      :accepting (not (:accepting state))
                                                      :transitions (:transitions state)})])
                                      (:states dfa-complete)))
                :pattern (list 'not (:pattern dfa)) }))))

(defn extract-rte-new
  "Accepts an object of type Dfa, and returns a map which associates
  exit values of the dfa with non-canonicalized rte patterns of the accepting
  langauge. If there are no accepting states in the Dfa, an empty map {}
  is returned."
  [dfa]
  ;; TODO - this can be done easiser
  ;;    1. trim the given dfa
  ;;    2. generate a list of transition triples [from label to]
  ;;    3. add transitions from extra-state-I to all initial states with :epsilon transition
  ;;    4. add transitions from all accepting states to extra-state-F (one per exit value) with :epsilon transition
  ;;    5. loop on each state
  ;;    6.    partition transitions into 4 groups [to-this-state loops-on-state from-state everything-else]
  ;;    7.    combine parallel transitions
  ;;    8.    n^2 iteration to-this-state x from-this-state
  ;;    9.    append new transitions in next iteration of loop 5.
  ;;    10. this reduces to one transtion per exit value, returns the map of exit-value to label
  (let [;; #2
        old-transition-triples (for [q (states-as-seq dfa)
                                     [label dst-id] (:transitions q)]
                                 [(:index q) label dst-id])
        ;; #3
        new-initial-transitions [[:I :epsion 0]]
        ;; #4
        new-final-transitions (for [q (states-as-seq dfa)
                                    :when (:accepting q)]
                                ;; we designate new final states each as [:f some-exit-value]
                                [(:index q) :epsilon [:F ((:exit-map dfa) q)]])]
    (letfn [          ;; local function
            (pretty-or [operands]
              (cond (empty? operands)
                    :empty-set
                    
                    (empty? (rest operands))
                    (first operands)
                    
                    :else
                    (cons :or operands)))

            ;; local function
            (pretty-cat [operands]
              (cond (member :epsilon operands)
                    (pretty-cat (remove (fn [o] (= o :epsilon)) operands))

                    (member '(:* :empty-set) operands)
                    (pretty-cat (remove (fn [o] (= o '(:* :empty-set))) operands))

                    (empty? operands)
                    :epsilon

                    (empty? (rest operands))
                    (first operands)

                    :else
                    (cons :cat operands)))

            ;; local function
            (extract-labels [triples]
              (map second triples))

            ;; local function
            (combine-parallel [triples]
              triples)

            ;; local function
            (eliminate-state [transition-triples q-id]
              (let [[x-to-q q-to-q q-to-x others]
                    ;; #6
                    (reduce (fn [[x-to-q q-to-q q-to-x others]
                                 [src-id label dst-id :as triple]]
                              (cond
                                (and (= src-id q-id)
                                     (= dst-id q-id))
                                ;; extend q-to-q
                                [x-to-q (cons triple q-to-q) q-to-x others]

                                (= src-id q-id)
                                ;; extend q-to-x
                                [x-to-q q-to-q (cons triple q-to-x) others]
                                
                                (= dst-id q-id)
                                ;; extend x-to-q
                                [(cons triple x-to-q) q-to-q q-to-q others]

                                :else
                                ;; extend others
                                [x-to-q q-to-q q-to-q (conj others triple)]))
                            [() () () []]
                            transition-triples)

                    ;; #7
                    self-loop-label (pretty-or (extract-labels q-to-q))
                    ;; #8
                    new-triples (for [[src pre-label _] x-to-q
                                      [_ post-label dst] q-to-x]
                                  [src
                                   (pretty-cat pre-label
                                               (list :* self-loop-label)
                                               post-label)
                                   dst])]
                (concat others new-triples)))]

      ;; #5 / #9
      (let [[[_ label _] & others :as all] (reduce eliminate-state old-transition-triples (ids-as-seq))]
        ;; one label per return value
        label))))

(defn extract-rte
  "Accepts an object of type Dfa, and returns a map which associates
  exit values of the dfa with non-canonicalized rte patterns of the accepting
  langauge. If there are no accepting states in the Dfa, an empty map {}
  is returned."
  [dfa]
  ;; TODO - this can be done easiser
  ;;    1. generate a list of transition triples [from label to]
  ;;    2. loop on each state
  ;;    3.    partition transitions into 4 groups [to-this-state loops-on-state from-state everything-else]
  ;;    4.    combine parallel transitions
  ;;    5.    n^2 iteration to-this-state x from-this-state
  ;;    6.    append new transitions in next iteration of loop 2.
  ;;    7. this reduces to one transtion, returns its label.
  (letfn [(available-ids [dfa]
            (let [states (states-as-map dfa)
                  num-states (count states)]
              (filter (fn [id]
                        ;; find smallest non-negative integer which is not already
                        ;; a state-id in this Dfa
                        (and (> id 0)
                             (not (get states id false)))) (range))))

          ;; local function
          (prepend-initial [dfa]
            (let [new-state-id (first (available-ids dfa))]
              (make-dfa dfa
                        {:states (assoc (states-as-map dfa)
                                        new-state-id (map->State {:initial true
                                                                  :index new-state-id
                                                                  :accepting false
                                                                  :transitions (for [id (ids-as-seq dfa)
                                                                                     :when (:initial (state-by-index dfa id))]
                                                                                 [:epsilon id])}))})))

          ;; local function
          (append-final [dfa]
            ;; each final state maps to an exit-value
            ;; but several final states might map to the same exit-value
            ;; we want to add one state per unique exit-value
            ;; and create an epsilon transition from the final state to that new state
            
            (let [new-states (map (fn [exit-value new-state-id]
                                    (map->State {:accepting true
                                                 :transitions ()
                                                 :exit-value exit-value
                                                 :index new-state-id
                                                 }))
                                  (keys (group-by (fn [[_ value]] value) (:exit-map dfa)))
                                  (available-ids dfa))
                  new-state-exit-map (into {} (map (fn [q] [(:exit-value q) q]) new-states))
                  ]
              (make-dfa dfa
                        {:exit-map (merge (:exit-map dfa)
                                          (into {} (map (fn [q] [(:index q) (:exit-value q)]) new-states)))
                         :states (into {}
                                       (map (fn [q]
                                              [(:index q)
                                               (cond
                                                 (member q new-states)
                                                 q
                                                 
                                                 (:accepting q) ;; but not in new-states,
                                                 ;; then create a transition to the new state corresponding
                                                 ;;  to its exit value
                                                 (map->State {:transitions (conj (:transitions q)
                                                                                 [:epsilon (:index (get new-state-exit-map
                                                                                                        (exit-value dfa q)))])
                                                              :index (:index q)})
                                                 
                                                 :else
                                                 q)])
                                            (concat new-states (states-as-seq dfa))))})))

          ;; local function
          (previous-states [dfa state]
            (let [state-id (:index state)]
              (filter (fn [q]
                        (some (fn [[_ dst-id]] (= dst-id state-id)) (:transitions q)))
                      (states-as-seq dfa))))

          ;; local function
          (next-states [dfa state]
            (for [[_ dst-id] (:transitions state)]
              (state-by-index dfa dst-id)))

          ;; local function
          (pretty-or [operands]
            (cond (empty? operands)
                  :empty-set
                  
                  (empty? (rest operands))
                  (first operands)
                  
                  :else
                  (cons :or operands)))

          ;; local function
          (pretty-cat [operands]
            (cond (member :epsilon operands)
                  (pretty-cat (remove (fn [o] (= o :epsilon)) operands))

                  (member '(:* :empty-set) operands)
                  (pretty-cat (remove (fn [o] (= o '(:* :empty-set))) operands))

                  (empty? operands)
                  :epsilon

                  (empty? (rest operands))
                  (first operands)

                  :else
                  (cons :cat operands)))
          
          ;; local function
          (find-self-loop-label [state]
            (let [state-id (:index state)]
              (pretty-or (for [[label dst-id] (:transitions state)
                               :when (= dst-id state-id)]
                           label))))

          ;; local function
          (new-transitions [previous-state state-to-eliminate]
            (let [previous-id (:index previous-state)
                  eliminate-id (:index state-to-eliminate)
                  removed (remove (fn [[_ dst-id]]
                                    (= dst-id eliminate-id))
                                  (:transitions previous-state))
                  add-these (for [[from-label dst-id-1] (:transitions previous-state)
                                  :when (= dst-id-1 eliminate-id)
                                  [to-label dst-id-2] (:transitions state-to-eliminate)
                                  :when (not= dst-id-2 eliminate-id)
                                  :let [new-label (pretty-cat (list from-label
                                                                    (list :* (find-self-loop-label state-to-eliminate))
                                                                    to-label))]
                                  ]
                              [new-label dst-id-2])]
              (concat removed
                      add-these)))
          
          ;; local function
          (eliminate-state [dfa state-to-eliminate]
            (let [from (previous-states dfa state-to-eliminate)
                  to   (next-states dfa state-to-eliminate)
                  new-states (mapcat (fn [state]
                                       (cond (= state state-to-eliminate)
                                             ()

                                             (member state from)
                                             (let [nt (new-transitions state state-to-eliminate)]
                                               (list [(:index state)
                                                      (map->State {:index (:index state)
                                                                   :transitions nt}
                                                                  )]))
                                             
                                             :else
                                             (list [(:index state) state])))
                                     (states-as-seq dfa))
                  ]
              (make-dfa dfa {:states (into {} new-states)})))
          ]
    (let [trim-dfa (trim dfa)
          i-f (reduce (fn [dfa' id]
                        (eliminate-state dfa' (state-by-index dfa' id)))
                      (append-final (prepend-initial trim-dfa))
                      (ids-as-seq trim-dfa))]
      (into {}
            (for [q (states-as-seq i-f)
                  [label dst-id] (merge-parallel-transitions (:transitions q) pretty-or)
                  :let [dst-state  (state-by-index i-f dst-id)]
                  :when (:accepting dst-state)]
              [(exit-value i-f dst-state) label])))))

(defn intersect-labels
  ""
  [label-1 label-2]
  (bdd/canonicalize-type (list 'and label-1 label-2)))

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
  [dfa-1 dfa-2 f-arbitrate-accepting f-arbitrate-exit-value]
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
  (letfn [(compute-state-transitions [state-1 state-2 state-ident-map]
            (for [[label-1 dst-1] (:transitions state-1)
                  [label-2 dst-2] (:transitions state-2)
                  :when (not (bdd/type-disjoint? label-1 label-2))
                  :let [label-sxp (intersect-labels label-1 label-2)]
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

(defn synchronized-intersection [dfa-1 dfa-2]
  "Compute the intersection of two Dfas. I.e., compute the Dfa which
  will recognized any sequence which is recognized by dfa-1 and also
  by dfa-2."
  (synchronized-product dfa-1 dfa-2
                        (fn [a b]
                          (and a b))
                        (fn [q1 _q2]
                          ((:exit-map dfa-1)
                           (:index q1)))))

(defn synchronized-xor [dfa-1 dfa-2]
  "Compute the xor of two Dfas. I.e., compute the Dfa which
  will recognized any sequence which is recognized by dfa-1 or
  by dfa-2 but not by both."
  (synchronized-product dfa-1 dfa-2
                        (fn [a b]
                          (or (and a (not b))
                              (and b (not a))))
                        (fn [q1 _q2]
                          ((:exit-map dfa-1)
                           (:index q1)))))

(defn dfa-equivalent
  "Returns a Boolean indicating whether the two given Dfas
  recognize the same language."
  [dfa-1 dfa-2]
  (every? (comp not :accepting) (states-as-seq (synchronized-xor dfa-1 dfa-2))))
