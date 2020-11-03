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

(defmethod gns/-inhabited? :spec [t1]
  :dont-know)

(defmethod gns/-disjoint? :rte [t1 t2]
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
  (= (resolve x)
     (resolve y)))

(defn spec-to-rte 
  [pattern]
  (cond (and (symbol? pattern)
             (resolve pattern)
             (fn? (deref (resolve pattern))))
        (list 'satisfies pattern)

        (sequential? pattern)
        (let [[tag & operands] pattern
              transformed (condp symbol= tag
                            'fn (list 'spec pattern)
                            's/and (cons 'and (for [operand operands]
                                                 (list 'spec operand)))
                            's/or (cons 'or (for [operand operands]
                                               (list 'spec operand)))
                            pattern)
              ]
          transformed 
          )

        :else
        (list 'spec pattern)
        ))

(defmethod gns/-canonicalize-type 'spec
  [[_spec pattern]]
  (let [got-spec (s/get-spec pattern)
        decompiled (delay (s/form pattern))]
    (cond
      (not got-spec)
      (spec-to-rte pattern)

      :else
      (spec-to-rte @decompiled))))
