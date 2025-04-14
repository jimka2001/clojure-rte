;; Copyright (c) 2020,25 EPITA Research and Development Laboratory
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

(ns typecase-test
  (:require [typecase :as sut]
            [genus]
            [clojure.test :as t :refer [is]]))

(defn -main []
  (clojure.test/run-tests 'typecase-test))

(t/deftest test-substitute-1-type
  (t/testing "substitute-1-type"
    (let [substitute-1-type @#'sut/substitute-1-type]
      (is (= (substitute-1-type 'a :empty-set '(or a b a c))
             '(or :empty-set b :empty-set c)))
      (is (= (substitute-1-type 'a :empty-set '(and a b a c))
             '(and :empty-set b :empty-set c)))
      (is (= (substitute-1-type 'a :empty-set '(not (and a b a c)))
             '(not (and :empty-set b :empty-set c)))))))

(t/deftest test-collect-leaf-types
  (t/testing "collect-leaf-types"
    (is (= (@#'sut/collect-leaf-types '((or a b a c)
                                        (and a b a c)
                                        (not (and a b a c))))
           '(a b a c
               a b a c
               a b a c)))))

(defn odd-int? [x]
  (and (int? x)
       (odd? x)))


(t/deftest test-typecase-optimization
  (t/testing "typcase optimization"
    (is (= (sut/typecase "hello"
             (or String Double) 42
             ;; TODO need a way to avoid using fully qualified name here.
             (and (satisfies int?) (satisfies typecase-test/odd-int?)) 43
             (and (satisfies int?) (not (satisfies typecase-test/odd-int?)) (not (satisfies neg?))) 44
             45)
           42))
    (is (= (sut/typecase 1.0
             (or String Double) 42
             (and (satisfies int?) (satisfies typecase-test/odd-int?)) 43
             (and (satisfies int?) (not (satisfies typecase-test/odd-int?)) (not (satisfies neg?))) 44
             45)
           42))
    (is (= (sut/typecase 1
             (or String Double) 42
             (and (satisfies int?) (satisfies typecase-test/odd-int?)) 43
             (and (satisfies int?) (not (satisfies typecase-test/odd-int?)) (not (satisfies neg?))) 44
             45)
           43))
    (is (= (sut/typecase 2
             (or String Double) 42
             (and (satisfies int?) (satisfies typecase-test/odd-int?)) 43
             (and (satisfies int?) (not (satisfies typecase-test/odd-int?)) (not (satisfies neg?))) 44
             45)
           44))
    (is (= (sut/typecase -2
             (or String Double) 42
             (and (satisfies int?) (satisfies typecase-test/odd-int?)) 43
             (and (satisfies int?) (not (satisfies typecase-test/odd-int?)) (not (satisfies neg?))) 44
             45)
           45))
    (is (= (sut/typecase -1
             (or String Double) 42
             (and (satisfies int?) (satisfies typecase-test/odd-int?)) 43
             (and (satisfies int?) (not (satisfies typecase-test/odd-int?)) (not (satisfies neg?))) 44
             45)
           43))
    ))            

;; (sut/typecase -1
;;   String 100
;;   (or String Number) 200
;;   Integer 300
;;    (satisfies evenp) 500)


(t/deftest test-typecase
  (t/testing "typecase"
    (is (= (sut/typecase 3)
           nil))
    (is (= (sut/typecase 3 :sigma 4)
           4))
    (is (= (sut/typecase 3 :empty-set 4)
           nil))
    (is (= (sut/typecase 3
             :empty-set 4
             :sigma 5)
           5))
    (is (= (sut/typecase "hello"
             Number 1
             String 2
             (satisfies int?) 3)
           2))

    (is (= (sut/typecase 1.0
             Number 1
             String 2
             (satisfies int?) 3)
           1))

    (is (= (sut/typecase 1.0
             (or String Number) 1
             String 2
             Number 3
             (satisfies int?) 4)
           1))
    (is (= (sut/typecase 1.0
             String 2
             (or String Number) 1
             
             Number 3
             (satisfies int?) 4)
           1))))
