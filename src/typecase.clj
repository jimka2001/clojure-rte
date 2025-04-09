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

(ns typecase
  (:require [genus :as gns]
            [util :refer [setof]]
            [backtick :refer [template]]))

(defn- collect-leaf-types [tds]
  (letfn [(collect [td]
            (cond (gns/not? td)
                  (collect (second td))
                  (gns/and? td)
                  (mapcat collect (rest td))
                  (gns/or? td)
                  (mapcat collect (rest td))
                  :else
                  (list td))
            )]
    (mapcat collect tds)))

(defn- canonicalize-pairs
  "pairs is a sequence of items which are implicitly (not explicitly grouped),
  e.g., pairs = (a 100 b 200 c 300).  This function interprets "
  [pairs]
  
  (if (empty? pairs)
    ()
    (let [[td consequent & others] pairs]
      `([~(gns/canonicalize-type td) ~consequent] ~@(canonicalize-pairs others)))))

(defn- substitute-1-type
  "Helper function for substitute-type, which does the search/replace
  on one given type designators, td-domain.  This function search for
  occurances of td-search in td-domain, and if found, replaces with
  td-replace."
  [td-search td-replace td-domain]
  (cond (= td-search td-domain)
        td-replace
        (gns/not? td-domain)
        (template (not ~(substitute-1-type td-search td-replace (second td-domain))))
        (gns/and? td-domain)
        (template (and ~@(map (fn [td] (substitute-1-type td-search td-replace td)) (rest td-domain))))
        (gns/or? td-domain)
        (template (or ~@(map (fn [td] (substitute-1-type td-search td-replace td)) (rest td-domain))))
        :else
        td-domain))

(defn- substitute-type
  "pairs is a sequence of items each of the form [type-designator consequent].
  This function attempts to simplify each of the type designators, return a
  new sequence of the same length, with the consequents untouched.  Each
  type-designator has had occurances of the type designator td-old replaced
  with td-new."
  [td-old td-new pairs]
  
  (for [[td consequent] pairs]
    [(gns/canonicalize-type (substitute-1-type td-old td-new td))
     consequent]))

(defn- substitute-types
  "Helper function which performs multiple type designator substitutions."
  [td-olds td-new pairs]
  (loop [td-olds td-olds
         pairs pairs]
    (if (empty? td-olds)
      pairs
      (recur (rest td-olds)
             (substitute-type (first td-olds) td-new pairs)))))
      
(defn- prune-pairs
  "This is a helper function used in optimizing the macro expansion
  of typecase.  pairs is a sequence of objects, each of the form 
  [type-designator expression].  This function returns a new sequence
  which prunes (removes the trailing elements) the sequence at the first
  occurance of [:sigma ...], if such occurs, removes any pair
  of the form [:empty-set ...]."
  [pairs]
  (cond (empty? pairs)
        nil

        :else
        (let [[[td consequent] & others] pairs]
          (case td
            (:sigma) (list [:sigma consequent])
            (:empty-set) (prune-pairs others)
            (cons [td consequent] (prune-pairs others))))))

(defn- calc-influence [all-freq all-leaves leaf]
  (let [disjoints (setof [t all-leaves] (= true (gns/disjoint? t leaf :dont-know)))
        proper-supers    (setof [t all-leaves] (and (not= t leaf)
                                             (= true (gns/subtype? leaf t :dont-know))))
        proper-subs      (setof [t all-leaves] (and (not= t leaf)
                                                    (= true (gns/subtype? t leaf :dont-know))))
        f (fn [sum leaf] (+ sum (get all-freq leaf)))
        influence   (+ (get all-freq leaf)
                       (reduce f 0 disjoints)
                       (reduce f 0 proper-supers)
                       (reduce f 0 proper-subs))]
    [leaf influence disjoints proper-supers proper-subs]))

(defn- maximize-influence [canonicalized-pairs]
  (let [all-leaves (collect-leaf-types (map first canonicalized-pairs))
        all-freq (frequencies all-leaves)
        first-leaves (collect-leaf-types (list (first (first canonicalized-pairs))))
        ]

    ;; find the leaf in first-leaves with the maximum occurances counting disjoints, supers, and subs 
    (reduce (fn [[most-freq' influence' disjoints' supers' subs'] leaf]
               (let [[most-freq influence disjoints supers subs] (calc-influence all-freq all-leaves leaf)]
                 (if (> influence' influence)
                   [most-freq' influence' disjoints' supers' subs']
                   [most-freq  influence  disjoints  supers  subs])))
             [:ignore 0 () () ()]
             first-leaves)))

(defmacro optimized-let
  "Macro used in the macro expansion of typecase.
  This is a very simple let form which checks for a very special
  case such as (let [x x] y) and optimizes it simply to y."
  [[a b] c]
  (if (= a b)
    c
    `(let [~a ~b] ~c)))
  
(defmacro optimized-typep
  "Macro used to optimize the a call such as (typep x '(satisfies y))
  to (y x)"
  [v t]
  (if (gns/satisfies? t)
    (let [[_ pred] t]
      `(boolean (~pred ~v)))
    `(gns/typep ~v '~t)))

(defmacro typecase 
  "Takes an expression and a set of clauses
  type-descriptor consequent
  and an optional final default value.
  The type-descriptors are those as specified in clojure.rte-genus
  The given expression is evaluated a maximum of once, but in some
  optimized cases it may not be evaluated at all.  If none of the type
  designators designate a type for which the given value is a member,
  then either the default value is returned, or nil if no default is
  given.
  Otherwise the consequent is evaluated and returned corresponding
  to the first (top most) type designator which matches the value.
  These type checks are highly optimized so that no type explicitly
  mentioned is checked more than once, including (and especially)
  (satisfies ...)."
  [value & pairs]
  (if (odd? (count pairs))
    `(typecase ~value ~@(butlast pairs) :sigma ~(last pairs))
        
    (let [canonicalized-pairs (prune-pairs (canonicalize-pairs pairs))]
      (if (empty? canonicalized-pairs)
        nil
        (let [[most-freq influence disjoints proper-supers proper-subs] (maximize-influence canonicalized-pairs)]
          (cond
            (and (= 1 (count canonicalized-pairs))
                 (= :sigma (first (first canonicalized-pairs))))
            (second (first canonicalized-pairs))
            
            (= 1 influence)
            (let [[[type consequent] & others] canonicalized-pairs]
              ;; canonicalized-pairs ([type consequent] [type consequent] ...)
              (if (gns/not? type)
                `(optimized-let [value# ~value]
                                ;; if the type is (not x), then strip off the not, and swap the then/else
                                (if (optimized-typep value# ~(second type))
                                  (typecase value# ~@(mapcat identity others))
                                  ~consequent))
                `(optimized-let [value# ~value]
                                (if (optimized-typep value# ~type)
                                  ~consequent
                                  (typecase value# ~@(mapcat identity others))))))

            :else
            ;; most-freq is a leaf-level type which appears multiple times, either
            ;; appearing explicitly, or disjoint, sub/super types also appear.
            ;; we expand to two cases, one where the type is matched, and one where
            ;; the complement of the type is not matched.
            (let [if-true  (substitute-types disjoints :empty-set
                                             (substitute-types (cons most-freq proper-supers) :sigma canonicalized-pairs))
                  if-false (substitute-types (cons most-freq proper-subs) :empty-set canonicalized-pairs)]
              `(optimized-let [value# ~value]
                              (if (optimized-typep value# ~most-freq)
                                (typecase value# ~@(mapcat identity if-true))
                                (typecase value# ~@(mapcat identity if-false)))))))))))
