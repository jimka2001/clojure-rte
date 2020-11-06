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

(ns clojure-rte.genus-spec
  (:require [clojure-rte.genus :as gns]
            [clojure.spec.alpha :as s]))

(defn spec? [t]
  (and (sequential? t)
       (= 'spec (first t))))

(defmethod gns/typep 'spec [a-value [_a-type pattern]]
  (s/valid? pattern a-value))

(defmethod gns/valid-type? 'spec [[_ pattern]]
  (boolean (s/get-spec pattern)))

(defmethod gns/-inhabited? :spec [_t1]
  :dont-know)

(defmethod gns/-disjoint? :spec [t1 t2]
  (cond (not (spec? t1))
        :dont-know

        (and (= t1 t2)
             (gns/inhabited? t1 false))
        false

        :else ;; for the moment assume that spec is always disjoint with anything
        false))

(defmethod gns/-subtype? :spec [sub-designator super-designator]
  :dont-know)

(defn symbol= [x y]
  (let [ns (find-ns 'clojure-rte.genus-spec)]
    (= (ns-resolve ns x)
       (ns-resolve ns y))))

(defn recuperate-closure
  "Takes an expression something like:
  (clojure.core/fn [%] (clojure.core/> % 1000))
  and returns an equivalent function object.
  In case the function object has a free variable, it
  is not possible to recuperate a function object, so
  an Exception is thrown"
  [expr]
  {:pre [(and (sequential? expr)
              (symbol= (first expr) 'fn)
              (vector? (second expr)))]}
  (eval expr))

(defn spec-to-rte 
  [pattern]
  (cond (s/regex? pattern)
        (spec-to-rte (s/form pattern))

        (s/get-spec pattern)
        (spec-to-rte (s/form pattern))
    
        (and (symbol? pattern)
             (resolve pattern)
             (fn? (deref (resolve pattern))))
        (list 'satisfies pattern)

        (sequential? pattern)
        (letfn [(specify [operands]
                  (for [operand operands]
                    (list 'spec (strip-keys operand))))
                (transform-sequence-pattern [pattern]
                  (if (sequential? pattern)
                    (let [[tag & operands] pattern]
                      (condp symbol= tag
                        's/cat (cons :cat (transform-sequence-patterns (strip-keys operands)))
                        's/alt (cons :or (transform-sequence-patterns (strip-keys operands)))
                        's/* (cons :* (transform-sequence-patterns operands))
                        's/? (cons :? (transform-sequence-patterns operands))
                        's/+ (cons :+ (transform-sequence-patterns operands))
                        's/& (unsupported pattern)
                        (list 'spec pattern)))
                    (list 'spec pattern)))
                (transform-sequence-patterns [patterns]
                  (map transform-sequence-pattern patterns))
                (strip-keys [operands]
                  (loop [operands operands
                         stripped ()]
                    (cond (empty? operands)
                          (reverse stripped)

                          :else
                          (recur (rest (rest operands))
                                 (cons (second operands)  stripped)))))
                (unsupported [pattern]

                  (println (ex-info "unsupported pattern" {:pattern pattern}))
                  (list 'spec pattern)
                  )]
          (let [[tag & operands] pattern
                transformed (condp symbol= tag
                              'fn (list 'satisfies (recuperate-closure pattern))
                              's/and (cons 'and (specify operands))
                              's/or (cons 'or (specify (strip-keys operands)))

                              's/cat (list 'rte (transform-sequence-pattern pattern))
                              's/alt (list 'rte (transform-sequence-pattern pattern))
                              's/* (list 'rte (transform-sequence-pattern pattern))
                              's/? (list 'rte (transform-sequence-pattern pattern))
                              's/+ (list 'rte (transform-sequence-pattern pattern))
                              's/& (unsupported pattern)
                              
                              pattern)
                ]
            transformed 
            ))

        :else
        (list 'spec pattern)
        ))

(defmethod gns/-canonicalize-type 'spec
  [[_spec pattern]]
  (cond
    (s/regex? pattern)
    (spec-to-rte (s/form pattern))

    (s/get-spec pattern)
    (spec-to-rte (s/form pattern))
    
    :else
    (spec-to-rte pattern)))
