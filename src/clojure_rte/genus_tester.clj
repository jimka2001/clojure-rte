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

(ns clojure-rte.genus-tester
  (:require [clojure-rte.genus :as gns]
            [backtick :refer [template]]))

(def ^:dynamic *test-types*
  "Some type designators used for testing"
  '((satisfies integer?)
    (satisfies int?)
    (satisfies rational?)
    (satisfies ratio?)
    (satisfies string?)
    (satisfies keyword?)
    (satisfies symbol?)
    (satisfies decimal?)
    (satisfies float?)
    (satisfies seq?)
    java.io.Serializable
    java.lang.CharSequence
    java.lang.Comparable
    java.lang.Number
    java.lang.Object
    clojure.lang.IMeta
    (= 1)
    (= 0)
    (= -1)
    (= a)
    (= [1 2 3])
    (= [])
    (member [1 2 3] [1 2] [1] [])
    (member [1 2 3] [2 1 3])
    (member a b c "a" "b" "c")
    (member a b)
    (member 1 2 3)
    (member 2 3 4)
    (member 1.5 3.5)
    (member 4.5 6.5)
    (member "a" "b" "c")
    (member "a" "b" "c" 1 2 3)
    (member 1 "a")
    :sigma
    :empty-set
    ))

(def ^:dynamic *test-values*
  "Some values used for random tests"
  `(~*in* ~*out*
    0 1 -1 2 -2
      0.0 1.0 1.1 -1.0 -1.1 1.5 3.5 4.5 6.5
      1/2
      "" "hello" "a" "b" "c"
      a b c
      12345678901234567890
      [1 2 3] [1 2] [1] [] [1 2 3 4]
    ))

(declare gen-inhabited-type)

(defn gen-type
  "Generate a type designator for testing."
  ([size]
   (gen-type size *test-types* false))
  ([size types]
   (gen-type size types false))
  ([size types limit-to-inhabited]
   (if (< 0 size)
     (case (rand-nth '(or and not :else))
       (or) (template (or ~(gen-type (dec size) types)
                          ~(gen-type (dec size) types)))
       (and) (let [t1 (if limit-to-inhabited
                        (gen-inhabited-type (dec size) (constantly true) types)
                        (gen-type (dec size) types limit-to-inhabited))
                   t2 (if limit-to-inhabited
                        (gen-inhabited-type (dec size) (constantly true) types)
                        (gen-type (dec size) types limit-to-inhabited))]
               ;; when trying to generate an inhabited type, and in the AND case
               ;;   we must make sure that both operands of (and ...) designate
               ;;   inhabited types.  If not, we can correct this now, rather than
               ;;   waiting to the top level
               (template (and ~t1 ~t2)))
       (not) (template (not ~(gen-type (dec size) types)))
       (rand-nth types))
     (rand-nth types))))

(defn gen-inhabited-type 
  "gen-type may generate a type designator which reduces to :empty-set.
  gen-inhabited-type generates a type designator which is guaranteed
  NOT to reduce to :empty-set.  If filter (a unary predicate) is
  given, gen-inhabited-type loops until it generates a type which
  satisifies the predicate.  If the predicate is never satisifed, it
  will loop forever."
  ([size]
   (gen-inhabited-type size (constantly true)))
  ([size filter]
   (gen-inhabited-type size filter *test-types*))
  ([size filter types]
   (loop []
     (let [td (gen-type size types true)
           td-inhabited (gns/inhabited? td false)]
       (if (and td-inhabited (filter td))
         td
         (recur))))))
