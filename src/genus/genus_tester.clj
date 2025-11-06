;; Copyright (c) 2021,25 EPITA Research and Development Laboratory
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

(ns genus.genus-tester
  (:require [genus.genus :as gns]
            [util :refer [rand-tree-012]]
            [backtick :refer [template]]))

(defn safe-odd? 
  "Save version of odd?.  The build-in odd? throws
  an exception if its argument is not an integer.
  safe-odd? simply returns false in this case."
  [n]
  (and (integer? n)
       (odd? n)))

(defn safe-positive? [n]
  "Save version of positive?.  The build-in positive? throws
  an exception if its argument is not an number.
  safe-positive? simply returns false in this case."
  (and (number? n)
       (> n 0)))

(def ^:dynamic *test-types*
  "Some type designators used for testing"
  (concat (template ((satisfies ~safe-odd?)
                     (satisfies ~safe-positive?)))
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
    )))

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
       (or) (template (or ~(gen-type (dec size) types limit-to-inhabited)
                          ~(gen-type (dec size) types limit-to-inhabited)))
       (and) (let [t1 (if limit-to-inhabited
                        (gen-inhabited-type (dec size) types)
                        (gen-type (dec size) types limit-to-inhabited))
                   t2 (if limit-to-inhabited
                        (gen-inhabited-type (dec size) types)
                        (gen-type (dec size) types limit-to-inhabited))]
               ;; when trying to generate an inhabited type, and in the AND case
               ;;   we must make sure that both operands of (and ...) designate
               ;;   inhabited types.  If not, we can correct this now, rather than
               ;;   waiting to the top level
               (template (and ~t1 ~t2)))
       (not) (template (not ~(gen-type (dec size) types)))
       (rand-nth types))
     (rand-nth types))))

(defn tree-012-to-type [tree types]
  (case (count tree)
    (0) (rand-nth types)
    (2) (let [[parent child] tree]
          (gns/Not (tree-012-to-type child types)))
    (3) (let [[parent left right] tree]
          (list (rand-nth '(or and))
                (tree-012-to-type left types)
                (tree-012-to-type right types)))))

(defn gen-balanced-type
  "Generate a type designator for testing."
  ([depth]
   (gen-balanced-type depth *test-types*))
  ([depth types]
   (if (< 0 depth)
     (tree-012-to-type (rand-tree-012 0.66666 depth) types)
     (rand-nth types))))

(defn gen-inhabited-type 
  "gen-type may generate a type designator which reduces to :empty-set.
  gen-inhabited-type generates a type designator which is guaranteed
  be inhabited, i.e. gns/inhabited? returns true"
  ([size]
   (gen-inhabited-type size *test-types*))
  ([size types]
   (loop []
     (let [td (gen-balanced-type size types)]
       (case (gns/inhabited? td :dont-know)
         (true)
         td

         (false :dont-know)
         (recur))))))

(defn gen-quasi-inhabited-type 
  "gen-type may generate a type designator which reduces to :empty-set.
  gen-inhabited-type generates a type designator which not provably empty.
  I.e., gns/inhabited? returns true or :dont-know"
  ([size]
   (gen-quasi-inhabited-type size *test-types*))
  ([size types]
   (loop []
     (let [td (gen-balanced-type size types) ]
       (case (gns/inhabited? td :dont-know)
         (true :dont-know)
         td

         (false)
         (recur))))))

(defn gen-indeterminate-type
  ([size]
   (gen-indeterminate-type size *test-types*))
  ([size types]
   (loop []
     (let [td (gen-balanced-type size types)]
       (case (gns/inhabited? td :dont-know)
         (:dont-know)
         td

         (true false)
         (recur))))))

;; (gen-indeterminate-type 5)
;; (gen-indeterminate-type 5)
;; (gen-quasi-inhabited-type 5)

