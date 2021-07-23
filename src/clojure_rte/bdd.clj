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

(ns clojure-rte.bdd
  "Definition of Bdd."
  (:refer-clojure :exclude [and or not])
  (:require [clojure-rte.util :refer [call-with-collector non-empty? print-vals forall]]
            [clojure-rte.genus :as gns]
            [clojure.pprint :refer [cl-format]]
            ))

(alias 'c 'clojure.core)
(alias 'bdd 'clojure-rte.bdd)

(defrecord Bdd
    [label positive negative])

(defmethod print-method Bdd [bdd w]
  (.write w (cond
              (c/and (= true (:positive bdd))
                     (= false (:negative bdd)))
              (cl-format false "#<Bdd ~A>" (:label bdd))

              (c/and (= false (:positive bdd))
                     (= true (:negative bdd)))
              (cl-format false "#<Bdd not ~A>" (:label bdd))

              :else
              (cl-format false "#<Bdd ~A ~A ~A>" (:label bdd) (:positive bdd) (:negative bdd)))))

(defn itenf
  "Serialize a Bdd to if-then-else-normal-form (itenf)"
  [bdd]
  (case bdd
    (true) :sigma
    (false) :empty-set
    
    (let [l (:label bdd)
          p (itenf (:positive bdd))
          n (itenf (:negative bdd))]
      (assert (not= nil p))
      (assert (not= nil n))
      (gns/create-or [(gns/create-and [l p])
                      (gns/create-and [(gns/create-not l) n])]))))

(defn dnf
  "Serialize a Bdd to dnf disjunctive normal form.
  This dnf form is cleaned up so that an (and ...) or (or ...) clause contains
  no subtype/supertype pairs.  This subtype relation is determined by
  (gns/subtype? a b false).  
  "
  [bdd]
  (letfn [(supertypes [sub types]
            (filter (fn [super]
                      (c/and (not= sub super)
                             (gns/subtype? sub super false))) types))
          (check-supers [args]
            (let [args (distinct args)
                  complements (for [a args
                                    b args
                                    :when (c/or (= a (list 'not b))
                                                (= b (list 'not a)))]
                                [a b])]
              (cond
                ;; does the list contain A and (not A) ?
                (non-empty? complements)
                '(:sigma)

                ;; does the list contain A and B where A is subtype B
                :else
                (remove (fn [sub]
                          (non-empty? (supertypes sub args)))
                        args))))]

    (gns/create-or
     (check-supers
      (call-with-collector
       (fn [collect]
         (letfn [(walk [node parents]
                   (let [my-label (:label node)
                         ;; two lazy sequences created by filter.  the filter loops are
                         ;; never called unless node is differrent from true or false, i.e.
                         ;; the 3rd case of the cond.
                         disjoints (delay (filter (fn [x] (gns/disjoint? x my-label false)) parents))
                         subtypes  (delay (filter (fn [x] (gns/subtype?  x my-label false)) parents))]
                     (cond
                       (= true node) ; case #1
                       ;; we know parents ( ... A ... B ...) that B is not subtype of A, but maybe A subtype B
                       ;;   we need to remove the supertypes
                       ;;   E.g., (Long java.io.Comparable java.io.Serializable) -> (Long)
                       ;;   E.g.  (Long Number) -> Long
                       (let [term (loop [tail parents
                                         done '()]
                                    (if (empty? tail)
                                      done
                                      (let [keeping (remove (fn [b]
                                                              ;; if we don't know, then keep it.  it might
                                                              ;; be redunant, but it won't be wrong.
                                                              ;; Is (first tail) <: b ?
                                                              ;;   if yes, then omit b in recur call
                                                              ;;   if :dont-know then keep it.
                                                              (gns/subtype? (first tail) b false))
                                                            (rest tail))]
                                        (recur keeping
                                               (cons (first tail) done)))))]
                         (collect (gns/create-and term)))
                       
                       (= false node) ; case #2
                       nil ;; do not collect, and prune recursion
                       
                       (non-empty? @disjoints) ; case #3
                       ;; this means that the type-designators returned by (:label node)
                       ;;    is disjoint with something in the parents list.
                       ;;    E.g., (:label node)=String, and parents= ( ... Number ...).
                       ;;    So we can  prune the positive branch, and omit this node from the
                       ;;    parents list in the recursive call.  Why, because any object
                       ;;    which has type of everything in the parents list is
                       ;;    guaranteed to NOT have the type of node.   We could
                       ;;    add `(not ~(label :node)) to the parents list, but it
                       ;;    would be redundant---we would have (and (not String) ... Number ...),
                       ;;    so we'll keep just (and ... Number ...).
                       (walk (:negative node)
                             parents)
                       
                       (non-empty? @subtypes) ; case #4
                       ;; this means that the type-designators returned by (:label node)
                       ;;    is a supertype of something in the parents list.  E.g.,
                       ;;    (:label node) = Number and parents= (... Long ...).
                       ;;    Wo we can prune the negative branch, and omit this node from the
                       ;;    parents list in the recursive call.  Why, because any object
                       ;;    which has type of everything in the parents list is
                       ;;    guaranteed to have the type of node.   We could
                       ;;    add (label :node) to the parents list, but it
                       ;;    would be redundant---we would have (and Number ... Long ...)
                       ;;    so we'll keep just (and ... Long ...).
                       (walk (:positive node)
                             parents)
                       
                       :else ; case #5
                       (do (walk (:positive node)
                                 (cons (:label node) parents))
                           (walk (:negative node)
                                 (cons (list 'not (:label node)) parents))))))]
           (walk bdd '()))))))))

(defn satisfying-type-designators-old
  "Create a lazy list of type designators which satisfy the Bdd.  I.e.,
  one element of the list for each type corresponding to the nodes
  from the top of the Bdd to the true leaf."
  [bdd]
  (assert (clojure.core/or (instance? Boolean bdd)
                           (instance? Bdd bdd)))
  (letfn [(satisfying [node lineage]
            (let [td (delay (gns/canonicalize-type lineage :dnf))]
              (if (= :empty-set td)
                ;; if the lineage has an empty intersection, then we prune this recursion
                []
                (case node
                  (true) [@td]
                  (false) []
                  ;; otherwise, we generate a lazy conatenation of the left and right traversals
                  (concat (satisfying (:positive node)
                                      (gns/create-and [@td
                                                       (:label node)]))
                          (satisfying (:negative node)
                                      (gns/create-and [@td
                                                       (gns/create-not (:label node))])))))))]
    (satisfying bdd :sigma)))


(defn satisfying-type-designators
  "Create a lazy list of type designators which satisfy the Bdd.  I.e.,
  one element of the list for each type corresponding to the nodes
  from the top of the Bdd to the true leaf."
  [bdd]
  (assert (clojure.core/or (instance? Boolean bdd)
                           (instance? Bdd bdd)))
  (letfn [(satisfying [node lineage]
            (if (= false node)
              []  ;; if reached a false leaf-node
              (let [td (gns/canonicalize-type lineage :dnf)]
                (cond (= :empty-set td)
                      ;; if the lineage canonicalizes to an empty intersection, then we prune this recursion
                      [] 

                      (= true node)
                      ;; if reached a true leaf-node
                      [td] 

                      :else
                      ;; otherwise, we generate a lazy conatenation of the left and right traversals
                      (concat (satisfying (:positive node)
                                          (gns/create-and [td
                                                           (:label node)]))
                              (satisfying (:negative node)
                                          (gns/create-and [td
                                                           (gns/create-not (:label node))])))))))]
    (satisfying bdd :sigma)))

(def ^:dynamic *hash*
  "Hash table storing Bdd instances which have been allocated.   The idea
  is that if a new Bdd is allocated via a call to Bdd., the funciton, bdd,
  will recognize this redundant instance, and return the previously allocated
  instance."
  (atom false))

(def ^:dynamic *label-to-index*
  "Hash table mapping type-designators to integers.  Each type designator
  serves as a label for a Bdd object, but to enforce the ordering of the ROBDD
  we have to have a way to order any two type designators.  They are ordered
  according to the integers stored in this hash table."
  (atom false))

(defn call-with-hash
  "Allocations two dynamic variables for the dynamic extent of evaluating
  the given 0-ary function.  The variables are *label-to-index* and *hash*.
  This function is part of the implementation of the with-hash macro.
  "
  [thunk]
  (if (= false @*hash*)
    (binding [*label-to-index* (atom {})
              *hash* (atom {})]
      (thunk))
    ;; if call-with-hash is called recursively, don't rebind anything.
    (thunk)))

(defmacro with-hash
  "This macro wraps a piece of code which needs to allocate Bdds. The macro
  wraps a call to the function call-with-hash, which provides an environment,
  of sorts, which makes it possible to allocate and manipulate Bdd instances.
  If with-hash is called recursively (intentially or accidentally), the
  inner-most call recognizes this and does not re-bind any dynamic variables,
  thus the innter-most call is innocuous and harmless."
  [[] & body]
  `(call-with-hash (fn [] ~@body)))

(defn type-index [type-designator]
  ;; TODO make sure two differnet symbols representing the same class
  ;; result in the same index.  eg. Double vs java.lang.Double
  (c/or (@*label-to-index* type-designator)
        (do (swap! *label-to-index* assoc type-designator (count @*label-to-index*))
            (@*label-to-index* type-designator))))

(declare bdd/and) ;; bdd/and
(declare bdd/or)  ;; bdd/or
(declare bdd/not) ;; bdd/not
(declare node)

(defn bdd
  "Public interface to programmatic Bdd constructor."
  [type-designator]
  (cond
    (sequential? type-designator)
    (case (first type-designator)
      (and) (reduce bdd/and (map bdd (rest type-designator)))
      (or)  (reduce bdd/or (map bdd (rest type-designator)))
      (not) (apply bdd/not (map bdd (rest type-designator)))
      (node type-designator true false))

    (= :sigma type-designator)
    true

    (= :empty-set type-designator)
    false

    :else
    (node type-designator true false)))

(defn label-<-by-index
  "A strict ordering function on potential labels in a Bdd.
  This function enforces that a < (satifies f) whenever
  a is not a (satisfies g) type, but otherwise, a < b whenever
  a was encountered first in the computation.  Whenever a new
  label is discovered in generating a Bdd, it is given an index
  higher than any other existing index.  The ordering of the labels
  is the ordering of the index  index-of(a) < index-of(b)."
  [t1 t2]
  (let [label-index-1 (type-index t1)
        label-index-2 (type-index t2)]
    (assert (integer? label-index-1) (format "expecting integer got %s" (type label-index-1)))
    (assert (integer? label-index-2) (format "expecting integer got %s" (type label-index-2)))
    (cond (c/and (gns/satisfies? t1)
               (gns/satisfies? t2))
          (< label-index-1 label-index-2)

          (gns/satisfies? t1)
          false

          (gns/satisfies? t2)
          true

          :else
          (< label-index-1 label-index-2))))

(def ^:dynamic *label-<*
  "Dynamic variable stocking the function used to order labels in a Bdd.
  This variable must be bound do a binary function such as label-<-by-index
  (which is the default) which implements a total order relation on
  the set of potential Bdd labels.  I.e., the function must be behave
  analgous to < rather than analgous to <= .
    transitive:  a<b and b<c => a<c
    asymmetric:  a<a is false
    irreflexive: if a<b then b<a is false"
  label-<-by-index)

(defn node
  "Internal function to function `bdd`.  This function is used during the
  recursive descent of the Bdd construction algorithm.  "
  [type-designator positive negative] 
  (assert (map? @*hash*) "attempt to allocate a Bdd outside dynamically extend of call-with-hash")
  (assert (map? @*label-to-index*) "attempt to allocate a Bdd outside dynamically extend of call-with-hash")
  (assert (c/or (instance? Boolean positive)
                (instance? Bdd positive))
          (cl-format false "wrong type of positive=~A type=~A"
                     positive (type positive)))
  (assert (c/or (instance? Boolean negative)
                (instance? Bdd negative))
          (cl-format false "wrong type of negative=~A type=~A"
                     negative (type negative)))
  (assert (gns/valid-type? type-designator)
          (cl-format false "[257] invalid type-designator ~A" type-designator))
  
  (cond
    (identical? positive negative)
    positive
    :else
    (let [try-bdd (Bdd. type-designator positive negative)
          cached-bdd (@*hash* try-bdd)]
      (c/or cached-bdd
            (do (swap! *hash* assoc try-bdd try-bdd)
                (assert (c/or (instance? Boolean positive)
                              (*label-<* type-designator (:label positive)))
                        (format "parent %s must be < positive %s" type-designator (:label positive)))
                (assert (c/or (instance? Boolean negative)
                              (*label-<* type-designator (:label negative)))
                        (format "parent %s must be < negative %s" type-designator (:label negative)))
                
                try-bdd)))))

(defn binary-op
  "Bdd abstract binary operation."
  [op bdd1 bdd2]
  (if (= (:label bdd1) (:label bdd2))
    (node (:label bdd1)
          (op (:positive bdd1) (:positive bdd2))
          (op (:negative bdd1) (:negative bdd2)))
    (if (*label-<* (:label bdd1) (:label bdd2))
      (node (:label bdd1)
            (op (:positive bdd1) bdd2)
            (op (:negative bdd1) bdd2))
      (node (:label bdd2)
            (op bdd1 (:positive bdd2))
            (op bdd1 (:negative bdd2))))))

(def bdd/and
  "Perform a Boolean AND on 0 or more Bdds."
  (fn
    ([] true)
    ([bdd] bdd)
    ([bdd1 bdd2]
     (cond
       (= false bdd1) false
       (= false bdd2) false
       (= true bdd1) bdd2
       (= true bdd2) bdd1
       (identical? bdd1 bdd2) bdd1
       :else (binary-op bdd/and bdd1 bdd2)))
    ([bdd1 bdd2 & bdds]
     (reduce bdd/and (apply cons bdd1 bdd2 bdds)))))

(def bdd/or
  "Perform a Boolean OR on 0 or more Bdds."
  (fn
    ([] false)
    ([bdd] bdd)
    ([bdd1 bdd2]
     (cond
       (= false bdd1) bdd2
       (= false bdd2) bdd1
       (= true bdd1) true
       (= true bdd2) true
       (identical? bdd1 bdd2) bdd1
       :else (binary-op bdd/or bdd1 bdd2)))
    ([bdd1 bdd2 & bdds]
     (reduce bdd/or (apply cons bdd1 bdd2 bdds)))))

(def bdd/and-not
  "Perform a relative complement operation on two (or more) Bdds.
  This is not implemented for the 0-ary nor 1-ary case."
  (fn
    ([bdd1 bdd2]
     (cond
       (identical? bdd1 bdd2) false
       (= bdd1 false) false
       (= bdd2 true) false
       (= bdd2 false) bdd1
       (= bdd1 true) (node (:label bdd2)
                           (bdd/and-not true (:positive bdd2))
                           (bdd/and-not true (:negative bdd2)))
       :else (binary-op bdd/and-not bdd1 bdd2)))

    ([bdd1 bdd2 & bdds]
     (reduce bdd/and (apply cons bdd1 bdd2 bdds)))))

(def bdd/not
  "Perform a Boolean not of a given Bdd"
  (fn [bdd1]
    (bdd/and-not true bdd1)))

(def sample-types '(Long Double String
                         (= "") (member "hello" "world")
                         Boolean Character Short
                         Object Number (= 0) (member -1 0 1)
                         (member 1 2)
                         (member 0 2 4 6)
                         java.lang.CharSequence
                         java.io.Serializable
                         java.lang.Comparable))

(defn gen-random
  "Generate a random Bdd"
  ([] (gen-random 15))
  ([max-depth]
   (if (<= max-depth 0)
     (rand-nth '(true false))
     (let [r (rand-int 5)]
       (cond
         (= r 0) ;; 20% chance
         (rand-nth '(true false))
         
         (= r 1) ;; 20% chance
         (bdd (rand-nth sample-types))
         
         (= r 2) ;; 20% chance
         (bdd/not (bdd (rand-nth sample-types)))
         
         :else ;; 40% chance
         (let [bdd-1 (gen-random (dec max-depth))
               bdd-2 (gen-random (dec max-depth))
               r (rand-int 4)]
           (cond
             (= r 0) ;; 40% * 25% chance
             (bdd/and bdd-1 bdd-2)
             (= r 1) ;; 40% * 25% chance
             (bdd/and-not bdd-1 bdd-2)
             :else   ;; 40% * 50% chance
             (bdd/or bdd-1 bdd-2))))))))

(defn typep
  "Given a value in question, and a Bdd representing a type designator,
  determine whether the value is an alement of the designated type."
  [value bdd]
  (cond
    (= true bdd) true
    (= false bdd) false
    :else (typep value
                 (if (gns/typep value (:label bdd))
                   (:positive bdd)
                   (:negative bdd)))))

(defn disjoint?
  "Given two Bdds, determine whether it can be proven that the intersection of the
   types they represent is empty.
   If it cannot be proven that they are disjoint, false is returned."
  [bdd1 bdd2]
  (= :empty-set
     (dnf (and bdd1 bdd2))))

(def bdd/canonicalize-type
  "Compute a canonicalized form of a given type designator.
   The intent is that given two type designators (as possibly different
   s-expressions), if they represent the same type, then they should
   be canonicalized to equal (=) s-expressions."
  (fn [type-designator]
    (with-hash []
      (dnf (bdd type-designator)))))

(defn type-disjoint?
  "Given two type designators, use Bdds to determine whether they are disjoint.
  If it cannot be proven that they are disjoint, false is returned."
  [type-designator-1 type-designator-2]
  (with-hash []
    (= :empty-set
       (bdd/canonicalize-type (list 'and type-designator-1 type-designator-2)))))

(defn type-subtype?
  "Given two type designators, use bdds to determine whether one is a subtype of the other.
  if it cannot be proven, false is returned."
  [subtype-designator supertype-designator]
  (with-hash []
    (let [bdd-sub (bdd subtype-designator)
          bdd-sup (bdd supertype-designator)
          bdd-diff (bdd/and-not bdd-sub bdd-sup)
          satisfying (satisfying-type-designators bdd-diff)]
      (cond (= false bdd-diff)
            true
            :else
            ;; in the case the bdd is not explicitly false, some branch(s)
            ;;   from the root to the true-leaf might designate a list of
            ;;   types whose intersection is empty.
            ;;   The two given types are in a sub/super relation ONLY
            ;;   if all such intersections are empty.
            (forall [td satisfying]
                    (= :empty-set (gns/canonicalize-type td)))))))
