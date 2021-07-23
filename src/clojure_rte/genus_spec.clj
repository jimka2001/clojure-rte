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

(ns clojure-rte.genus-spec
  (:require [clojure-rte.genus :as gns]
            [clojure-rte.util :refer [casep defn-memoized exists seq-matcher]]
            [clojure.pprint :refer [cl-format]]
            [clojure.spec.alpha :as s]
            [backtick :refer [template]]
))

;; allow gns/ prefix even in this file.
(alias 'gs 'clojure-rte.genus-spec)

(def gs/spec?
  "Detect sequence starting with the simple symbol spec"
  (seq-matcher 'spec))

(defmethod gns/typep 'spec [a-value [_a-type pattern]]
  (assert (not= nil pattern) "gns/typep: nil is not a supported spec")
  (try (boolean (s/valid? pattern a-value))
       (catch Exception e
         (cl-format true "s/valid? failed on a-value=~A pattern=~A"
                    a-value pattern)
         (throw e))))

(defmethod gns/valid-type? 'spec [[_ pattern]]
  (boolean (s/get-spec pattern)))

(defmethod gns/-inhabited? 'spec [_t1]
  :dont-know)

(defmethod gns/-disjoint? 'spec [t1 t2]
  (cond (not (gs/spec? t1))
        :dont-know

        (and (= t1 t2)
             (gns/inhabited? t1 false))
        false

        :else ;; for the moment assume that spec is always disjoint with anything
        false))

(defmethod gns/-subtype? 'spec [_sub-designator _super-designator]
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

(defn wrap-spec 
  [pattern]
  (cond (not (sequential? pattern))
        pattern

        (empty? pattern)
        pattern
        
        (exists [k1 '(s/cat s/alt s/* s/? s/+)]
                (symbol= k1 (first pattern)))
        (list 'spec pattern)

        :else
        pattern))

(defn-memoized [spec-to-rte spec-to-rte-impl]
  "Convert spec to rte"
  [pattern]
  (cond (s/regex? pattern)
        (spec-to-rte (s/form pattern))

        (s/spec? pattern)
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
                    (list 'spec operand)))
                (transform-sequence-pattern [pattern]
                  (if (sequential? pattern)
                    (let [[tag & operands] pattern]
                      (casep symbol= tag
                        (s/cat) (cons :cat (transform-sequence-patterns (strip-keys operands)))
                        (s/alt) (cons :or (transform-sequence-patterns (strip-keys operands)))
                        (s/*)   (cons :* (transform-sequence-patterns operands))
                        (s/?)   (cons :? (transform-sequence-patterns operands))
                        (s/+)   (cons :+ (transform-sequence-patterns operands))
                        (s/&) (unsupported pattern)
                        (list 'spec pattern)))
                    (list 'spec pattern)))
                (transform-sequence-patterns [patterns]
                  (map transform-sequence-pattern patterns))
                (strip-keys [operands]
                  (assert (sequential? operands)
                          (cl-format false "expecting a sequence, not ~A" operands))
                  (let [stripped
                        (loop [operands (doall operands)
                               stripped ()]
                          (cond (empty? operands)
                                (reverse stripped)

                                :else
                                (do
                                  (when (not (keyword? (first operands)))
                                    (cl-format true "WARNING: expecting keyword, not ~A in ~A"
                                               (first operands) operands))
                                  (recur (rest (rest operands))
                                         (cons (second operands)  stripped)))))]
                    stripped))
                (unsupported [pattern]
                  (throw (ex-info "unsupported pattern" {:unsupported-pattern true
                                                         :pattern pattern
                                                         :thrown-by 'spec-to-rte})))]
          (let [[tag & operands] pattern
                transformed (casep symbol= tag
                                   (fn) (template (satisfies ~(recuperate-closure pattern)))
                                   (s/and) (template (and ~@(specify operands)))
                                   (s/or) (template (or ~@(specify (strip-keys operands))))
                                   (s/nilable) (template (or (satisfies nil?) ~@(specify operands)))
                                   (s/keys
                                    s/keys*
                                    s/coll-of) (template (spec ~(eval pattern)))
                                   
                                   (s/cat
                                    s/alt
                                    s/*
                                    s/?
                                    s/+)   (template (rte ~(transform-sequence-pattern pattern)))
                                   (s/&) (unsupported pattern)
                                   
                                   pattern)
                ]
            transformed 
            ))

        :else
        (list 'spec pattern)
        ))

(defmethod gns/-canonicalize-type 'spec
  [[_spec pattern] nf]
  (try (wrap-spec (spec-to-rte pattern))
       (catch clojure.lang.ExceptionInfo ei
         (if (:unsupported-pattern (ex-data ei))
           (do ;; if we fail to expand the pattern, then don't even try
             (cl-format true "failed to canonicalize type: ~A, at ~A~%"
                        pattern (:pattern (ex-data ei)))
             (list 'spec pattern))
           (throw ei)))))
