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

(ns clojure-rte.api
  "This is an empty namespace to fool lein to load this file as part of
  the clojure-rte.rte-core ns.")

(in-ns 'clojure-rte.rte-core)

(defn dispatch [obj caller]
  (cond (instance? (dfa/record-name) ;; parser cannot handle dfa/Dfa
                   obj)
        :Dfa
        (sequential? obj)
        :sequential

        :else
        (throw (ex-info (format "invalid argument for %s, expecting, sequential? or Dfa, not %s"
                                caller obj)
                        {:obj obj
                         }))))

(defmulti rte-trace
  "Given a compiled rte, find a sequence of types which satisfy the corresponding pattern."
  (fn [rte]
    (dispatch rte 'rte-trace)))
  
(defmethod rte-trace :sequential
  [pattern]
  (rte-trace (rte-compile pattern)))

(defmethod rte-trace :Dfa
  [dfa]
  (let [state-vec (:states dfa)]
    (letfn [(recurring [state path lineage]
              (cond
                (:accepting (state-vec state)) path
                (some #{state} lineage) false
                :else (some (fn [[type dst-state]]
                              (recurring dst-state (conj path type) (conj lineage state)))
                            (:transitions (state-vec state))))
              )]
      (recurring 0 [] ()))))

(defmulti rte-inhabited?
  (fn [rte]
    (dispatch rte 'rte-inhabited?)))

(defmethod rte-inhabited? :sequential [pattern]
  (rte-inhabited? (rte-compile pattern)))

(defmethod rte-inhabited? :Dfa [dfa]
  (some :accepting (dfa/states-as-seq dfa)))

(defn rte-vacuous? [dfa]
  (not (rte-inhabited? dfa)))

(defmulti rte-match
  "(rte-match rte sequence :promise-disjoint true|false)
   Given an rte pattern or finite automaton generated by rte-to-dfa (or rte-compile), 
   determine whether the given sequence, items, matches the regular type expression.

   If the caller wishes to check more than one sequence against the same
   pattern, it is probably better to call rte-compile, to get an automaton, and
   use that same automaton in several calls to rte-match to avoid
   multiple conversions/look-ups, as the correspondence of pattern
   to compiled Dfa is maintained via the memoize function."

  (fn [rte _items & {:keys [promise-disjoint
                            hot-spot]}]
   (dispatch rte 'rte-match)))

(defmethod rte-match :sequential
  [pattern items & {:keys [promise-disjoint
                           hot-spot]
                    :as all-keys}]
  (apply rte-match (rte-compile pattern) items :promise-disjoint true all-keys))

(defmethod rte-match :Dfa
  [dfa items & {:keys [
                       ;; if the caller promises that now two transitions in the Dfa
                       ;;   are labeled with intersecting types, the use
                       ;;   :promise-disjoint true, in this case rte-match
                       ;;   can be more efficient and can assume that the
                       ;;   clauses can be tested in any order.  If the transitions
                       ;;   are not guaranteed disjoint, then rte-match must
                       ;;   build new type designators each one containing an and-not
                       ;;   of the previously seen types. 
                       promise-disjoint
                       ;; hot-spot = true -- lazily compile the type checks into Bdds
                       ;;    which is slow-going but becomes faster the more often you
                       ;;    re-use the same pattern, either because of loops in the
                       ;;    Dfa, or when the same Dfa is used to match different
                       ;;    input sequences.
                       ;; hot-spot = false -- always interpret the type checks rather
                       ;;    than converting them to Bdd.  This option is probably faster
                       ;;    if there are few loops in the Dfa, or if you only use the
                       ;;    pattern once to check a single input sequence.
                       hot-spot
                       ]}]
  (let [state-vec (:states dfa)
        sink-states (set (dfa/find-sink-states dfa))]
    (if (empty? sink-states)
      (rte-match (dfa/extend-with-sink-state dfa) items :promise-disjoint promise-disjoint)
      (let [sink-state-id (:index (first sink-states))]
        ;; There are two possible transition functions
        ;;   slow-transition-function -- this is faster if the caller intends to match
        ;;       the pattern only once.   The pattern is matched by an interpreter,
        ;;       and it is possible that the same type predicate will be tested multiple
        ;;       times on the same candidate objects.  If one of the type predicates
        ;;       is (satisfies slow-predicate) then that slow-predicate may be called
        ;;       multiple times, resulting in poor performance, especially if the
        ;;       pattern is used to test multiple sequences.
        ;;   fast-transition-function -- this is faster if the caller intends to match
        ;;       the pattern multiple times with different input sequences.  The
        ;;       pattern is *compiled* into a form where type-designators are converted
        ;;       to Bdds thus each type check guarantees to never check the same
        ;;       type predicate multiple times, and sometimes not at all.
        (letfn [(slow-transition-function [transitions]
                  (fn [candidate]
                    (some (fn [[type next-state-index]]
                            (if (gns/typep candidate type)
                              next-state-index
                              sink-state-id))
                          transitions)))
                (fast-transition-function [transitions]
                  (dfa/optimized-transition-function transitions promise-disjoint sink-state-id))
                (transition-function [transitions]
                  (if hot-spot
                    (fast-transition-function transitions)
                    (slow-transition-function transitions)))
                (consume [state-index item]
                  (let [state-obj (state-vec state-index)]
                    (cl/cl-cond
                     ((member state-obj sink-states)
                      (reduced false))
                     (((transition-function (:transitions state-obj)) item sink-state-id))
                     (:else (reduced false)))))]
          (let [final-state (reduce consume 0 items)]
            ;; final-state may be integer desgnating the state which was
            ;;  reached on iterating successfully through the input
            ;;  sequence, items.  Or final-state may false, if the
            ;;  iteration finished without iterating through the entire
            ;;  sequence, either because we found ourselves in a
            ;;  sink-state, or we encountered a item for which no transition
            ;;  was possible.
            (cond
              (= false final-state) false
              (:accepting (state-vec final-state)) ((:exit-map dfa) final-state)
              :else false)))))))
