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

(ns clojure-rte.api-test
  (:require [clojure-rte.rte-core :refer :all :exclude [-main and? or? satisfies? member? not? =?]]
            [clojure-rte.rte-construct :refer :all]
            [clojure.test :refer :all]
            [clojure.pprint :refer [cl-format]]
            [clojure-rte.util :refer [sort-operands remove-once call-with-collector visit-permutations]]
            [clojure-rte.genus :refer [disjoint? typep inhabited?]]
            [clojure-rte.rte-core :refer :all :exclude [-main]]
            [clojure-rte.rte-tester :refer :all]))

(defn -main []
  (clojure.test/run-tests 'clojure-rte.api-test))

(deftest t-rte-match
  (testing "rte-match"
    (with-compile-env ()
      (let [rte (rte-compile '(:cat Double Number))]
        (is (not (rte-match rte [1.0]))))
      (let [rte (rte-compile '(:or (:* Number)
                                   (:cat Double Number)
                                   (:* Double)))]
        (is (rte-match rte [1.0])))
      (let [rte (rte-compile '(:or (:* Number)
                                   (:cat String Number)
                                   (:* Double)))]
        (is (rte-match rte [1.0]))
        (is (not (rte-match rte ["hello"])))
        (is (rte-match rte ["hello" 1.0]))
        (is (not (rte-match rte ["hello" 1.0 2.0]))))
      (let [rte (rte-compile '(:* (:cat clojure.lang.Keyword java.lang.Long)))]
        (is (rte-match rte '(:x 1 :y 2 :z 42)))
        (is (rte-match rte '()))
        (is (not (rte-match rte '(x 1 y 2 z 42)))))
      (let [rte (rte-compile '(:* (:cat clojure.lang.Keyword java.lang.Long)))]
        (is (rte-match rte '(:x 1 :y 2 :z 42)))
        (is (rte-match rte '()))
        (is (not (rte-match rte '(x 1 y 2 z 42)))))

      (is (rte-match '(:cat (:* (satisfies integer?)) (:? String))
                     '( 1 2 3 4 "hello")))
      (is (not (rte-match '(:cat (:* (satisfies integer?)) (:? String))
                          '( 1 2 3 4 "hello" "world"))))
      )))

(deftest t-types
  (testing "types"
    (with-compile-env ()
      (is (rte-match '(:* (satisfies int?)) [ 1 2 3]))
      (is (rte-match '(:* (satisfies number?)) [ 1 2.0 1/3]))
      (is (rte-match '(:* (satisfies symbol?))  '(a b c)))
      (is (rte-match '(:* (satisfies keyword?))  '(:a :b :c)))
      (is (not (rte-match '(:* (satisfies symbol?))  '(a :b c))))
      (is (rte-match '(:* (satisfies string?))  '("hello" "world")))
      (is (rte-match '(:* (satisfies rational?)) [ 1 2 1/3]))
      (is (rte-match '(:* (satisfies float?)) [ 1.0 2.0 3.0]))
      )))

(deftest t-not
  (testing "patterns with :not"
    (with-compile-env ()
      (is (rte-match '(:cat (:* (:cat clojure.lang.Keyword (:not java.lang.Long))))
                     '(:x 1 :y 2 :z 42 "hello" 3)))
      
      (is (not (rte-match '(:cat clojure.lang.Keyword (:not java.lang.Long))
                          '(:x 1))))
      
      (is (not (rte-match '(:* (:cat clojure.lang.Keyword (:and :sigma (:not java.lang.Long))))
                          '(:x 1))))
      (is (not (rte-match '(:* (:cat clojure.lang.Keyword (:and :sigma (:not java.lang.Long))))
                          '(:x 1 :y 2))))
      (is (not (rte-match '(:* (:cat clojure.lang.Keyword (:and :sigma (:not java.lang.Long))))
                          '(:x 1 :y 2 :z 3))))
      (is (rte-match '(:* (:cat clojure.lang.Keyword (:and :sigma (:not java.lang.Long))))
                     '(:x "hello" :y "hello" :z "hello")))
      
      ;; currently this test fails
      (is (rte-match '(:not Number) ["Hello" "world"]))
      )))
