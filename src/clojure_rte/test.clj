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

(ns clojure-rte.genus
  (:require 
            [clojure-rte.util :refer [ member  defn-memoized
                                      defn-memoized]]
            ))

(declare canonicalize-type)
(declare inhabited?)


(declare check-disjoint check-disjoint-impl)


(defn-memoized [disjoint? disjoint?-impl]
  "Predicate to determine whether the two types overlap.
  If it cannot be determined whether the two designated types
  are disjoint, then the default value is returned."
  [t1 t2 default]
  {;;:pre [(member default '(true false :dont-know))]
   :post [(member % '(true false :dont-know))]}
  (list t1 t2 default) ;; silly return value just to use the function parameters
)

(let []
  (declare xdisjoint?)
  (defn xdisjoint?-impl [t1 t2 default]
    {:post [(member % '(true false :dont-know))]}
    (list t1 t2 default))
  (def xdisjoint?
   "Predicate to determine whether the two types overlap.\n  If it cannot be determined whether the two designated types\n  are disjoint, then the default value is returned."
   (clojure-rte.util/gc-friendly-memoize xdisjoint?-impl)))
