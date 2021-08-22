;; Copyright (c) 2021 EPITA Research and Development Laboratory
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

(ns clojure-rte.rte-extract
  (:require [clojure.pprint :refer [cl-format]]
            [clojure-rte.rte-construct :as rte]
            [clojure-rte.xymbolyco :as xym]))

(defn extract-rte
  "Accepts an object of type Dfa, and returns a map which associates
  exit values of the dfa with canonicalized rte patterns of the accepting
  langauge. If there are no accepting states in the Dfa, an empty map {}
  is returned."
  [dfa']
  ;; TODO - this can be done easier
  ;;    1. minimize and trim the given dfa
  ;;    2. generate a list of transition triples [from label to]
  ;;    3. add transitions from extra-state-I to all initial states with :epsilon transition
  ;;    4. add transitions from all accepting states to extra-state-F (one per exit value) with :epsilon transition
  ;;    5. loop on each state
  ;;    6.    partition transitions into 4 groups [to-this-state loops-on-state from-state everything-else]
  ;;    7.    combine parallel transitions
  ;;    8.    n^2 iteration to-this-state x from-this-state
  ;;    9.    append new transitions in next iteration of loop 5.
  ;;    10. this reduces to one transition per exit value, returns the map of exit-value to label
  (let [;; #1
        dfa (xym/trim (xym/minimize dfa')) ; we must minimize otherwise the size of the returned expression can be HUGE
        ;; #2
        old-transition-triples (for [q (xym/states-as-seq dfa)
                                     [label dst-id] (:transitions q)]
                                 [(:index q) label dst-id])
        ;; #3
        new-initial-transitions [[:I :epsilon 0]]
        ;; #4
        new-final-transitions (for [q (xym/states-as-seq dfa)
                                    :when (:accepting q)]
                                ;; we designate new final states each as [:F some-exit-value]
                                [(:index q) :epsilon [:F ((:exit-map dfa) (:index q))]])]
    (letfn [          ;; local function
            (combine-parallel-labels [operands]
              (rte/canonicalize-pattern
               (rte/create-or operands)))

            ;; local function
            (extract-labels [triples]
              (map second triples))

            ;; local function
            (combine-parallel [triples]
              ;; accepts a sequence of triples, each of the form [from label to]
              ;;   groups them by common from/to, these are parallel transitions
              ;;   combines the labels of the parallel transitions, into one single label
              ;;   and collects a sequence of transitions, none of which are parallel.
              ;;   This action is important because it greatly reduces the number of transitions
              ;;   created.  The caller, the computation of new-triples, makes an NxM loop
              ;;   creating NxM new triples.   This reduces N and M by eliminating parallel
              ;;   transitions.
              (for [[[from to] triples] (group-by (fn [[from _ to]] [from to]) triples)
                    :let [label (combine-parallel-labels (extract-labels triples))]
                    ]
                [from label to]))

            ;; local function
            (eliminate-state [transition-triples q-id]
              (let [[x-to-q q-to-q q-to-x others]
                    ;; #6
                    (reduce (fn [[x-to-q q-to-q q-to-x others]
                                 [src-id _label dst-id :as triple]]
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
                                [(cons triple x-to-q) q-to-q q-to-x others]

                                :else
                                ;; extend others
                                [x-to-q q-to-q q-to-x (conj others triple)]))
                            [() () () []]
                            transition-triples)

                    ;; #7
                    self-loop-label (combine-parallel-labels (extract-labels q-to-q))
                    ;; #8
                    new-triples (for [[src pre-label _] (combine-parallel x-to-q)
                                      [_ post-label dst] (combine-parallel q-to-x)]
                                  [src
                                   (rte/create-cat (list pre-label
                                                         (list :* self-loop-label)
                                                         post-label))
                                   dst])]
                (concat others new-triples)))]

      ;; #5 / #9
      (let [new-transition-triples (reduce eliminate-state
                                           (concat new-initial-transitions
                                                   old-transition-triples
                                                   new-final-transitions)
                                           ;; TODO need to order states for faster
                                           ;;    elimination.  I.e., order by increasing
                                           ;;    product of num-inputs x num-outputs
                                           (xym/ids-as-seq dfa))
            grouped (group-by (fn [[_ _ [_ exit-value]]] exit-value) new-transition-triples)]
        (into {}
              (doall 
               (for [[exit-value triples] grouped
                     :let [pretty (combine-parallel-labels (extract-labels triples))]
                     ]
                 ;; one label per return value
                 ;; #10
                 [exit-value pretty])))))))

(defn dfa-to-rte
  "Accepts an object of type Dfa, and returns a map which associates
  exit values of the dfa with canonicalized rte patterns of the accepting
  langauge.  If there are no accepting states in the Dfa, an empty map {}
  is returned."
  [dfa]
  (assert (instance? (xym/record-name) dfa)
          (cl-format false "dfa-to-rte: expecting Dfa, not ~A ~A" (type dfa) dfa))
  (let [ret-val-rte (extract-rte dfa)]
    (if (= {} ret-val-rte)
      {true :empty-set}
      ret-val-rte)))

(defn rte-equivalent?
  "Compare two rte patterns build comparing their constructed Dfa's."
  [rte-1 rte-2]
  (xym/dfa-equivalent? (rte/rte-to-dfa rte-1)
                       (rte/rte-to-dfa rte-2)))
