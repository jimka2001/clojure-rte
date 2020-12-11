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

(ns clojure-rte.typecase
  (:require [clojure-rte.genus :as gns]
            [backtick :refer [template]]))

(defn collect-leaf-types [tds]
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

(defn canonicalize-pairs [pairs]
  (if (empty? pairs)
    ()
    (let [[td consequent & others] pairs]
      `([~(gns/canonicalize-type td) ~consequent] ~@(canonicalize-pairs others)))))

(defn rev-typep [td value]
  (gns/typep value td))

(defn typecase-as-cond [value pairs]
  (let [condp-args (mapcat (fn [[td consequent]]
                             `['~td ~consequent]) pairs)]
    `(condp rev-typep ~value
       ~@condp-args)))

(defn substitute-1-type [td-search td-replace td-domain]
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
        
(defn substitute-type [td-old td-new pairs]
  (for [[td consequent] pairs
        y [(gns/canonicalize-type (substitute-1-type td-old td-new td))
           consequent]]
    y))
  
(defn prune-pairs [pairs]
  (cond (empty? pairs)
        nil

        :else
        (let [[[td consequent] & others] pairs]
          (case td
            (:sigma) (list [:sigma consequent])
            (:empty-set) (prune-pairs others)
            (cons [td consequent] (prune-pairs others))))))

(defn most-frequent [items]
  (if (empty? items)
    nil
    (apply max-key val (frequencies items))))

(defmacro typecase [value & pairs]
  (cond (odd? (count pairs))
        `(typecase ~value ~@(butlast pairs) :sigma ~(last pairs))
        
        :else
        (let [canonicalized-pairs (prune-pairs (canonicalize-pairs pairs))
              leaves (collect-leaf-types (map first canonicalized-pairs))
              [most-freq most-freq-count] (most-frequent leaves)
              ]
          (cond (empty? canonicalized-pairs)
                nil

                (and (= 1 (count canonicalized-pairs))
                     (= :sigma (first (first canonicalized-pairs))))
                (second (first canonicalized-pairs))
                
                (= 1 most-freq-count)
                (typecase-as-cond value canonicalized-pairs)

                :else
                (let [if-true  (substitute-type most-freq :sigma canonicalized-pairs)
                      if-false (substitute-type most-freq :empty-set canonicalized-pairs)]
                  `(let [value# ~value]
                     (if (gns/typep value# '~most-freq)
                       (typecase value# ~@if-true)
                       (typecase value# ~@if-false))))))))
