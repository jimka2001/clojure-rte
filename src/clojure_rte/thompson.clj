;; Copyright (c) 2022 EPITA Research and Development Laboratory
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

(ns clojure-rte.thompson
  "Implementation of Thompson construction of Sigma Complete Deterministic
  Finite Automata"
  (:refer-clojure :exclude [complement])
  (:require [clojure-rte.cl-compat :as cl]
            [clojure-rte.util :refer [fixed-point member group-by-mapped
                                      defn-memoized find-first
                                      partition-by-pred
                                      non-empty? exists setof trace-graph group-map]]
            [clojure-rte.rte-construct :as rte]
            [clojure-rte.genus :as gns]
            [clojure.pprint :refer [cl-format]]
            [clojure-rte.bdd :as bdd]
            [clojure.set :refer [union difference intersection]]
            [backtick :refer [template]]
            [clojure-rte.xymbolyco :as xym]
            [clojure.set :as set]
            ))

(declare accessible)
(declare coaccessible)
(declare construct-epsilon-free-transitions)
(declare trim)
(declare trace-transition-graph)
(declare find-all-states)
(declare construct-determinized-transitions)
(declare construct-epsilon-free-transitions)

(def counter (atom 0))
(defn new-state []
  (swap! counter inc))

(defn sxp [in1 outs1 transitions-1
           in2 outs2 transitions-2
           arbitrate]
  (let [grouped-1 (group-by first transitions-1)
        grouped-2 (group-by first transitions-2)]
    (letfn [(state-transitions [[q1 q2]]
              (for [[_ td1 y1] (get grouped-1 q1 [])
                    [_ td2 y2] (get grouped-2 q2 [])
                    :let [td (template (and ~td1 ~td2))]
                    :when (not= false (gns/inhabited? td :dont-know))
                    ]
                [(gns/canonicalize-type td) [y1 y2]]))]
      (let [inX [in1 in2]
            [finalsX transitionsX] (trace-transition-graph inX state-transitions
                                                           (fn [[x y]]
                                                             (arbitrate (boolean (member x outs1))
                                                                        (boolean (member y outs2)))))]
        [inX finalsX transitionsX]))))

(defn renumber-transitions 
  "We have transitions in some form other than DFA_TRANSITION, and we
   need to convert to that form.  So we assign a new Int value to each (x,_,y)
   in the transitions, and return a translated copy of transitions, as well
   as updated value of in and outs."
  ([in outs transitions]
   (renumber-transitions in outs transitions new-state))
  ([in outs transitions new-state-index]
   (let [states (concat (conj outs in)
                        (mapcat (fn [[xx _ yy]] [xx yy]) transitions))
         mapping (into {} (map (fn [qq] [qq (new-state-index)])
                               (distinct states)))]
     [(mapping in)
      (map mapping outs)
      (map (fn [[xx td yy]]
             [(mapping xx) td (mapping yy)]) transitions)])))

(defn confluxify
  "join the multiple outputs of a set of transitions (each labeled with a SimpleTypeD)
  into a single output, having epsilon transitions from the previous outputs
  to a single new output.   That output is the confluence (hence the name of the function)
  of the old output.   All the old outputs flow via epsilon transition into the new output."
  [in outs transitions]
  (let [ini (new-state)
        out (new-state)]

    [ini out (concat (conj (map (fn [f] [f :epsilon out]) outs)
                           [ini :epsilon in])
                     transitions)]))

(defn construct-and
  "The Thompson construction does not have a case for AND.
   So what we do here is take the two arguments of AND
   and perform a synchronized cross product on the two results."
  [rte-1 rte-2]
  (let [[and-1-in and-1-outs transitions-1] (construct-epsilon-free-transitions rte-1)
        [and-2-in and-2-outs transitions-2] (construct-epsilon-free-transitions rte-2)
        [sxp-in sxp-outs sxp-transitions] (sxp and-1-in and-1-outs transitions-1
                                               and-2-in and-2-outs transitions-2
                                               (fn [a b] (and a b)))
        [renum-in renum-outs renum-transitions] (renumber-transitions sxp-in sxp-outs sxp-transitions)]
    (confluxify renum-in renum-outs renum-transitions)))

(defn invert-finals 
  "Return the set of states which are not final states."
  [outs completed]
  (filter (fn [x]
            (not (member x outs)))
          (find-all-states completed)))

(defn construct-not [rte]
  (let [[in outs determinized] (construct-determinized-transitions rte)
        inverted (invert-finals outs determinized)]
    (confluxify in inverted determinized)))

(defn construct-transitions [rte]
  (rte/traverse-pattern rte
                        (assoc rte/*traversal-functions*
                               :empty-set (fn [_ _]
                                            [(new-state) (new-state) []])
                               :epsilon (fn [_ _]
                                             (let [in (new-state)
                                                   out (new-state)]
                                               [in out [[in :epsilon out]]]))
                               :sigma (fn [_ _]
                                        (let [in (new-state)
                                              out (new-state)]
                                          [in out [[in :sigma out]]]))
                               :type (fn [td _]
                                        (let [in (new-state)
                                              out (new-state)]
                                          [in out [[in td out]]]))
                               :* (fn [rte _]
                                    (let [in (new-state)
                                          out (new-state)
                                          [in-inner out-inner transitions] (construct-transitions rte)]
                                      [in out (concat [[in :epsilon in-inner]
                                                       [out-inner :epsilon out]
                                                       [in :epsilon out]
                                                       [out-inner :epsilon in-inner]]
                                                      transitions)]))
                               :cat (fn [rtes _]
                                      (cond (empty? rtes)
                                            (construct-transitions :epsilon)

                                            (empty? (rest rtes))
                                            (construct-transitions (first rtes))

                                            (empty? (rest (rest rtes)))
                                            (let [[in-1 out-1 transitions-1] (construct-transitions (first rtes))
                                                  [in-2 out-2 transitions-2] (construct-transitions (second rtes))]
                                              [in-1 out-2 (concat [[out-1 :epsilon in-2]]
                                                                  transitions-1
                                                                  transitions-2)])

                                            :else
                                            (construct-transitions (template (:cat ~(first rtes)
                                                                                   (:cat ~@(rest rtes)))))))
                                  :or (fn [rtes _]
                                        (cond (empty? rtes)
                                              (construct-transitions :empty-set)

                                              (empty? (rest rtes))
                                              (construct-transitions (first rtes))

                                              (empty? (rest (rest rtes)))
                                              (let [in (new-state)
                                                    out (new-state)
                                                    [in-1 out-1 transitions-1] (construct-transitions (first rtes))
                                                    [in-2 out-2 transitions-2] (construct-transitions (second rtes))]
                                                [in out (concat [[in :epsilon in-1]
                                                                 [in :epsilon in-2]
                                                                 [out-1 :epsilon out]
                                                                 [out-2 :epsilon out]]
                                                                transitions-1
                                                                transitions-2)])

                                              :else
                                              (construct-transitions (template (:or ~(first rtes)
                                                                                    (:or ~@(rest rtes)))))))
                                  :and (fn [rtes _]
                                        (cond (empty? rtes)
                                              (construct-transitions rte/sigma-*)

                                              (empty? (rest rtes))
                                              (construct-transitions (first rtes))

                                              (empty? (rest (rest rtes)))
                                              (construct-and (first rtes) (second rtes))

                                              :else
                                              (construct-transitions (template (:and ~(first rtes)
                                                                                     (:and ~@(rest rtes)))))))
                                  :not (fn [rte _]
                                         (construct-not rte))
                               
                               )))


(defn trim
  "remove transitions which are not accessible and are not coaccessible.
   This doesn't change the input state, but some final states may be lost."
  [in finals transitions]
  (let [[a-in a-finals a-transitions] (accessible in finals transitions)]
    (coaccessible a-in a-finals a-transitions)))

(defn remove-epsilon-transitions [in out transitions ;; List[Tuple[int, Optional[SimpleTypeD], int]]
                                  ] ;; -> Tuple[int, List[int], List[Tuple[int, SimpleTypeD, int]]]
  (let [[epsilon-transitions normal-transitions]
        (partition-by-pred (fn [[_ tr _]] (= :epsilon tr)) transitions)
        all-states (vec (find-all-states transitions))]
    (letfn [(reachable-from [q]
              (set (for [[x _ y] epsilon-transitions
                    :when (= x q)]
                y)))
            (extend-closure [m] ;; m is seq of set of int
              (for [qs m]
                (set/union qs
                           (reduce (fn [acc q]
                                     (set/union acc (reachable-from q)))
                                   #{}
                                   qs))))]
      (let [epsilon-closure (fixed-point (map (comp set list) all-states)
                                         extend-closure 
                                         =)
            transitions2 (mapcat (fn [q closure]
                                   (for [c closure
                                         [x label y] normal-transitions
                                         :when (= x c)
                                         :when (not= x q)]
                                     [q label y]))
                                 all-states epsilon-closure)
            ;; some states no longer exist after removing epsilon transitions
            updated-transitions (concat normal-transitions transitions2)
            remaining-states (find-all-states updated-transitions)
            finals (mapcat (fn [q closure]
                             (if (and (or (member q remaining-states)
                                          (= q in))
                                      (member out closure))
                               [q]))
                           all-states epsilon-closure)]
        (trim in finals updated-transitions)))))

(defn construct-epsilon-free-transitions [rte] ;; -> (int, List[int], List[Tuple[int, SimpleTypeD, int]])
  (let [[in out transitions] (construct-transitions rte)]
    (remove-epsilon-transitions in out transitions)))

(defn find-all-states [transitions]
  (set (mapcat (fn [[x _ y]] [x y]) transitions)))

(defn complete
  "Transform a sequence of transitions (either deterministic or otherwise)
   into a set of completed transitions.  I.e., the union of the types labeling
   any arbitrary transition is STop.  In the case when it cannot be determined
   whether the transitions are already locally complete, an addition transition
   is added with is the complement of the existing transitions.  This may be
   empty but if it is empty, .inhabited returns None (dont-know)."

  [in outs
   clean-transitions ;; : List[Tuple[int, SimpleTypeD, int]]
   ] ;; -> List[Tuple[int, SimpleTypeD, int]]:
  (let [all-states (find-all-states clean-transitions)
        sink (delay (new-state))
        grouped (group-by first clean-transitions)
        cf (fn [q]
             (let [trs (get grouped q [])]
               (if (empty? trs)
                 ;; for states with no exiting transitions
                 [[q :sigma @sink]]
                 ;; for states withs ome exiting transitions, find the complement of their union
                 (let [tds (map second trs)
                       remaining (template (not ~(gns/create-or tds)))]
                   ;; if either satisfiable is Some(True) or dont-know None,
                   ;; then we have to create a transition to the sink state
                   (if (= false (gns/inhabited? remaining :dont-know))
                     []
                     [[q remaining @sink]])))))
        completing-transitions (mapcat cf all-states)]
    (cond (empty? clean-transitions)
          [[in :sigma @sink]
           [@sink :sigma @sink]]

          (empty? completing-transitions)
          clean-transitions

          :else
          (concat (conj completing-transitions
                        [@sink :sigma @sink])
                  clean-transitions))))

(defn determinize [in outs
                   transitions ;; List[Tuple[int, SimpleTypeD, int]]
                   ] ;; -> Tuple[int, List[int], List[Tuple[int, SimpleTypeD, int]]]:
  ;; This can be done with a call to traceTransitionGraph.
  ;;   to generate a graph (list of transitions) whose vertices are
  ;;   each a Set[int].  Then you'll have to renumber back to
  ;;   states as int.
  ;;   BTW, you can also use traceTransitionGraph to implement
  ;;   the cartesian product needed for And, as well as to remove
  ;;   non-accessible, and non-coaccessible vertices.
  ;; Use the mdtd function to partition a set/list of types into
  ;;   a set of disjoint types.
  (let [grouped (group-by first transitions)]
    (letfn [(expand-one-state [qs] ;; qs is a set of int
              ;; Given a set of states, find all the SimpleTypeD which are labels of
              ;;   transitions leaving any of those states.
              (let [tds (set (for [q qs                                   
                                   [_ td _] (get grouped q [])]
                               td))
                    ;; The set tds of SimpleTypeD might contain some non-disjoint types.
                    ;;   So we call compute the minimum disjoint type decomposition (mdtd)
                    ;;   and create one transition per type per type from this decomposition.
                    ;;   This is done in two steps.
                    ;;   step 1. compute tr2 which is the list of (type,next-state) pairs.
                    ;;   step 2. for { (td,pairs) ...} compute the seq of next states corresponding
                    ;;      to each type.
                    ;;      The pair returned from step 2 is exactly what is needed by traceGraph
                    tr2 (for [[td factors _] (gns/mdtd tds)
                              q qs
                              [_ td1 y] (get grouped q [])
                              :when (member td1 factors)]
                          [td y])]
                (for [[td pairs] (group-by first tr2)
                      :let [next-states (map second pairs)]]
                  [td next-states])))]

      (let [inX #{in}
            [expanded-finals expanded-transitions]
            (trace-transition-graph inX expand-one-state
                                    (fn [qs]
                                      (exists [f outs]
                                              (member f qs))))]
        ;; we now have transitions which map Set[Int] to Set[Int]
        ;;   where each state (Set[Int]) represents 1 state.  We must
        ;;   now replace each Set[Int] with a corresponding Int.
        (renumber-transitions inX expanded-finals expanded-transitions)
))))

(defn construct-determinized-transitions [rte] ;; -> Tuple[int, List[int], List[Tuple[int, SimpleTypeD, int]]]
  (let [[in2 outs2 clean] (construct-epsilon-free-transitions rte)
        completed (complete in2 outs2 clean)]
    (determinize in2 outs2 completed)))

(defn trace-transition-graph [q0 ;; : V,
                              edges ;; : Callable[[V], List[Tuple[SimpleTypeD, V]]],
                              is_final ;; : Callable[[V], bool]
                              ];; -> Tuple[List[V], List[Tuple[V, SimpleTypeD, V]]]:
  (let [[qs transitions] (trace-graph q0 edges)]
    [(filter is_final qs)
     (mapcat (fn [x pairs]
              (map (fn [[label y]]
                     [x label (qs y)])
                   pairs))
            qs transitions)]))

;; remove non-accessible transitions, and non-accessible final states
(defn accessible [in outs
                  transitions;; : List[Tuple[int, SimpleTypeD, int]]
                  ] ;; -> (int, List[int], List[Tuple[int, SimpleTypeD, int]]):
  (let [grouped (group-map first
                           transitions
                           rest)
        [accessible-outs accessible-transitions]
        (trace-transition-graph in
                                (fn [q] 
                                  (get grouped q []))
                                (fn [q] (boolean (member q outs))))]
    [in accessible-outs accessible-transitions]))

(defn coaccessible [in outs transitions]
  (let [proxy (new-state)
        augmented-transitions (concat (for [q outs]
                                        [q :sigma proxy])
                                transitions)
        third (comp second rest)
        grouped (group-map third augmented-transitions (fn [[x y _]] [y x]))
        [coaccessible-outs reversed-transitions] (trace-transition-graph proxy
                                                   (fn [q] (get grouped q []))
                                                   (fn [q] (= q in)))
        coaccessible-transitions (for [[y td x] reversed-transitions
                                       :when (not= y proxy)]
                                   [x td y])]
    [in outs coaccessible-transitions]))


(defn construct-thompson-dfa [pattern ret]
  (let [[ini outs determinized] (construct-determinized-transitions pattern)
        counter (atom -1)
        new-state (fn [] 
                    (swap! counter inc))
        [ini outs determinized] (renumber-transitions ini outs determinized new-state)
        fmap (into {} (map (fn [f] {f ret}) outs))
        states (into {} (for [[x triples] (group-by (fn [[x td y]] x) determinized)]
                          [x (xym/map->State {:index x
                                              :accepting (boolean (member x outs))
                                              :initial (= ini x)
                                              :transitions (map rest triples)})]))]

    (assert (= ini 0))
    (xym/make-dfa {:pattern pattern
                   :states states
                   :exit-map fmap
                   :combine-labels (fn [td1 td2] (gns/create-or (list td1 td2)))})))

(defn simulate
  "Consider the set of transitions as a non-deterministic symbolic finite
  automaton, having in as initial state and outs as accepting/final states.
  Execute the state machine by consuming the values of stream.
  stream may be any value acceptable to reduce.
  If the given sequence (stream) is accepted, return the given exit-value;
  otherwise return false."
  [stream exit-value
   in outs transitions]
  (let [groups (group-map first transitions rest)
        computed (reduce (fn [states v]
                           (if (empty? states)
                             (reduced false)
                             (set (for [x states
                                        [td y] (get groups x [])
                                        :when (gns/typep v td)]
                                    y))))
                         #{in}
                         stream)]
    (if (exists [f computed]
                (member f outs))
      exit-value
      false)))
                
    
