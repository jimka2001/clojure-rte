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

(ns clojure-rte.util-test
  (:require [clojure-rte.rte-core]
            [clojure.pprint :refer [cl-format]]
            [clojure-rte.util :refer [sort-operands partition-by-pred
                                      call-with-collector
                                      visit-permutations
                                      remove-once fixed-point member
                                      ]]
            [clojure.test :refer [deftest testing is]]))

(defn -main []
  (clojure.test/run-tests 'clojure-rte.util-test))

(deftest t-partition-by-pred
  (testing "partition-by-pred"
    (let [[odd even] (partition-by-pred odd? (range 100))]
      (is (every? odd? odd))
      (is (every? even? even)))))

(deftest t-sort-operands-1
  (testing "sort-operands"
    (is (= (sort-operands ())
           ()))
    (is (= (sort-operands [])
           ()))
    (is (= (sort-operands '(::Cat ::Lion))
           '(::Cat ::Lion)))
    (is (= (sort-operands '(::Lion ::Cat))
           '(::Cat ::Lion)))
    (is (= (sort-operands '((:not ::Cat) (:not ::Lion)))
           '((:not ::Cat) (:not ::Lion))))
    (is (= (sort-operands '((:not ::Lion) (:not ::Cat)))
           '((:not ::Cat) (:not ::Lion))))))
    
(deftest t-sort-operands-2
  (testing "sort-operands 2"
    (sort-operands '(:cat (:not (:* (:or :epsilon :sigma)))
                          (:not (:cat :sigma (:* :sigma)))
                          (:or :epsilon :sigma)
                          :sigma
                          :sigma
                          (:* :sigma)
                          :sigma
                          (:or :epsilon :sigma)
                          (:* :sigma)
                          (:or (:and :sigma BigInteger) (:and :sigma Byte) (:and :sigma Integer) (:and :sigma Long) (:and :sigma Short) (:and :sigma clojure.lang.BigInt))
                          :sigma
                          (:* :sigma)
                          :sigma
                          :sigma))
    (sort-operands '(:cat (:not (:* (:or :epsilon :sigma)))
                          (:not (:cat :sigma (:* :sigma)))
                          (:or :epsilon :sigma)
                          :sigma
                          :sigma
                          (:* :sigma)
                          :sigma
                          (:or :epsilon :sigma)
                          (:* :sigma)
                          (:or (:and :sigma BigInteger) (:and :sigma Byte) (:and :sigma Integer) (:and :sigma Long) (:and :sigma Short) (:and :sigma clojure.lang.BigInt))
                          :sigma
                          :sigma
                          (:* :sigma)
                          :sigma))
    (sort-operands '(:cat (:not (:* (:or :epsilon :sigma)))
                          (:not (:cat :sigma (:* :sigma)))
                          (:or :epsilon :sigma)
                          :sigma
                          :sigma
                          (:* :sigma)
                          :sigma
                          :sigma
                          (:* :sigma)
                          :sigma
                          (:or :epsilon :sigma)
                          (:* :sigma)
                          (:or (:and :sigma BigInteger) (:and :sigma Byte) (:and :sigma Integer) (:and :sigma Long) (:and :sigma Short) (:and :sigma clojure.lang.BigInt))
                          :sigma))
    (sort-operands '(:cat (:not (:* (:or :epsilon :sigma)))
                          (:not (:cat :sigma (:* :sigma)))
                          (:or :epsilon :sigma)
                          :sigma
                          :sigma
                          (:* :sigma)
                          :sigma
                          (:* :sigma)
                          :sigma
                          (:or :epsilon :sigma)
                          (:* :sigma)
                          (:or (:and :sigma BigInteger) (:and :sigma Byte) (:and :sigma Integer) (:and :sigma Long) (:and :sigma Short) (:and :sigma clojure.lang.BigInt))
                          :sigma
                          :sigma))
    (sort-operands '(:cat (:not (:* (:or :epsilon :sigma)))
                          (:not (:cat :sigma (:* :sigma)))
                          (:or :epsilon :sigma)
                          :sigma
                          :sigma
                          :sigma
                          :sigma
                          (:* :sigma)
                          :sigma
                          (:or :epsilon :sigma)
                          (:* :sigma)
                          (:or (:and :sigma BigInteger) (:and :sigma Byte) (:and :sigma Integer) (:and :sigma Long) (:and :sigma Short) (:and :sigma clojure.lang.BigInt))
                          :sigma
                          (:* :sigma)))
    
    (sort-operands '(:cat (:not (:* (:or :epsilon :sigma)))
                          (:not (:cat :sigma (:* :sigma)))
                          (:or :epsilon :sigma)
                          :sigma
                          :sigma
                          :sigma
                          (:* :sigma)
                          :sigma
                          (:or :epsilon :sigma)
                          (:* :sigma)
                          (:or (:and :sigma BigInteger) (:and :sigma Byte) (:and :sigma Integer) (:and :sigma Long) (:and :sigma Short) (:and :sigma clojure.lang.BigInt))
                          :sigma
                          (:* :sigma)
                          :sigma))
    (sort-operands '(:cat (:not (:* (:or :epsilon :sigma)))
                          (:not (:cat :sigma (:* :sigma)))
                          (:or :epsilon :sigma)

                          :sigma
                          :sigma
                          :sigma
                          (:* :sigma)
                          :sigma
                          (:or :epsilon :sigma)
                          (:* :sigma)
                          (:or (:and :sigma BigInteger) (:and :sigma Byte) (:and :sigma Integer) (:and :sigma Long) (:and :sigma Short) (:and :sigma clojure.lang.BigInt))
                          :sigma
                          :sigma (:* :sigma)))
    (sort-operands '(:cat (:not (:* (:or :epsilon :sigma))) (:not (:cat :sigma (:* :sigma)))
                          (:or :epsilon :sigma) :sigma :sigma :sigma (:* :sigma)
                          :sigma :sigma (:* :sigma) :sigma (:or :epsilon :sigma)
                          (:* :sigma) (:or (:and :sigma BigInteger) (:and :sigma Byte)
                                           (:and :sigma Integer) (:and :sigma Long)
                                           (:and :sigma Short) (:and :sigma clojure.lang.BigInt))))
    (sort-operands '(:cat (:not (:* (:or :epsilon :sigma))) (:not (:cat :sigma (:* :sigma)))
                          (:or :epsilon :sigma) :sigma :sigma :sigma (:* :sigma) :sigma (:* :sigma)
                          :sigma (:or :epsilon :sigma) (:* :sigma)
                          (:or (:and :sigma BigInteger)
                               (:and :sigma Byte) (:and :sigma Integer)
                               (:and :sigma Long) (:and :sigma Short)
                               (:and :sigma clojure.lang.BigInt)) :sigma))
    (sort-operands '(:cat (:not (:* (:or :epsilon :sigma))) (:not (:cat :sigma (:* :sigma)))
                          (:or :epsilon :sigma) :sigma :sigma :sigma :sigma (:* :sigma) :sigma (:* :sigma)
                          :sigma (:or :epsilon :sigma) (:* :sigma)
                          (:or (:and :sigma BigInteger)
                               (:and :sigma Byte) (:and :sigma Integer)
                               (:and :sigma Long) (:and :sigma Short) (:and :sigma clojure.lang.BigInt))))
    (sort-operands '(:cat (:not (:* (:or :epsilon :sigma)))
                          (:not (:cat :sigma (:* :sigma)))
                          (:or :epsilon :sigma)
                          :sigma
                          :sigma
                          (:* :sigma)
                          :sigma
                          (:or :epsilon :sigma)
                          (:* :sigma)
                          (:or (:and :sigma BigInteger) (:and :sigma Byte) (:and :sigma Integer)
                               (:and :sigma Long) (:and :sigma Short) (:and :sigma clojure.lang.BigInt))
                          :sigma :sigma
                          (:* :sigma)
                          :sigma))
    (sort-operands '(:cat (:not (:* (:or :epsilon :sigma)))
                          (:not (:cat :sigma (:* :sigma)))
                          (:or :epsilon :sigma)
                          :sigma
                          :sigma
                          (:* :sigma)
                          :sigma
                          (:or :epsilon :sigma)
                          (:* :sigma)
                          (:or (:and :sigma BigInteger) (:and :sigma Byte) (:and :sigma Integer)
                               (:and :sigma Long) (:and :sigma Short) (:and :sigma clojure.lang.BigInt))
                          :sigma
                          :sigma
                          :sigma
                          (:* :sigma)))
    ))

(deftest t-sort-operands-4
  (testing "sort length"
    (is (= (sort-operands (list (list 1 2 3) (list 1 2 3 4)))
           (list (list 1 2 3) (list 1 2 3 4))))
    (is (= (sort-operands (list (list 1 2 3 4) (list 1 2 3)))
           (list (list 1 2 3) (list 1 2 3 4))))))
    

(deftest t-sort-operands-3
  (testing "sort-operands 3"
    (let [a-cons `(1 2 3) ;; clojure.lang.Cons
          b-cons `(1 2 3 4) ;; clojure.lang.Cons
          e-cons (seq `(1 2 3 4)) ;; clojure.lang.Cons
          c-cons (seq (map identity '(1 2 3))) ;; clojure.lang.Cons

          d-cons (seq (map identity [1 2 3])) ;; clojure.lang.ChunkedCons

          a-list (list 1 2 3) ;; clojure.lang.PersistentList
          b-list '(1 2 3 4) ;; clojure.lang.PersistentList
          a-vec [1 2 3] ;; clojure.lang.PersistentVector

          a-lazy (map identity (list 1 2 4)) ;; clojure.lang.LazySeq
          b-lazy (map identity [1 2 4]) ;; clojure.lang.LazySeq
          data (list a-cons  
                     c-cons d-cons 
                     a-list a-vec b-cons e-cons b-list 
                     a-lazy b-lazy)]
      (is (= (sort-operands data)
             data)))))

(deftest t-visit-permuations
  (testing "visit-permutations"
    (is (= (set (call-with-collector
                 (fn [collect]
                   (visit-permutations collect '()))))
           #{()}))
    (is (= (set (call-with-collector
                 (fn [collect]
                   (visit-permutations collect '(1)))))
           #{'(1)}))
    (is (= (set (call-with-collector
                 (fn [collect]
                   (visit-permutations collect '(1 2)))))
           (set '((1 2)
                  (2 1)))))
    (is (= (set (call-with-collector
                 (fn [collect]
                   (visit-permutations collect '(1 2 3)))))
           (set '((1 2 3)
                  (1 3 2)
                  (2 1 3)
                  (2 3 1)
                  (3 1 2)
                  (3 2 1)))))
    ))
    
(deftest t-call-with-collector
  (testing "call-with-collector"
    (is (= (call-with-collector (fn [collect]
                                  (collect 1)
                                  (collect 3)
                                  (collect 2)))
           '(2 3 1)))
    (is (= (call-with-collector (fn [_collect]
                                  ))
           ()))))

(deftest t-remove-once
  (testing "remove-once"
    (is (= (remove-once 4 '(1 2 3))
           '(1 2 3)))
    (is (= (remove-once 1 '())
           ()))

    (is (= (remove-once 1 '(1 2 3))
           '(2 3)))

    (is (= (remove-once 1 '(1 1 2 3))
           '(1 2 3)))

    (is (= (remove-once 1 '(1 2 1 3))
           '(2 1 3)))

    (is (= (remove-once 2 '(1 2 1 2 3 2))
           '(1 1 2 3 2)))
    ))


(def test-data
  '((:and (:cat (:* :sigma) (:or (:and (:not Byte) (:not Integer) (:not Long) (:not Short)) :epsilon Double Float)) (:cat (:* :sigma) :sigma) (:cat (:or (:* (:or :epsilon :sigma)) (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma)))) (:* (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma))))) :epsilon) (:and (:cat (:* :sigma) :sigma) (:cat (:or (:* (:or :epsilon :sigma)) (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma)))) (:* (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma))))) (:cat (:or (:and (:not Byte) (:not Integer) (:not Long) (:not Short)) :epsilon Double Float) (:* :sigma)) :epsilon) (:and (:cat (:* :sigma) :sigma) (:cat (:or (:* (:or :epsilon :sigma)) (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma)))) (:* (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma))))) :epsilon) (:and (:cat (:* :sigma) :sigma) (:cat (:or (:and (:not Byte) (:not Integer) (:not Long) (:not Short)) :epsilon Double Float) (:* :sigma)) :epsilon) (:and (:cat (:* :sigma) :sigma) :epsilon) (:and (:cat :sigma (:* :sigma)) (:cat (:* :sigma) (:or (:and (:not Byte) (:not Integer) (:not Long) (:not Short)) :epsilon Double Float)) :epsilon) (:and (:cat (:* :sigma) (:or (:and (:not Byte) (:not Integer) (:not Long) (:not Short)) :epsilon Double Float)) (:cat (:* :sigma) :sigma) :epsilon) (:and (:cat (:or (:* (:or :epsilon :sigma)) (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma)))) (:* (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma))))) (:cat (:or (:and (:not Byte) (:not Integer) (:not Long) (:not Short)) :epsilon Double Float) (:* :sigma)) (:cat :sigma (:* :sigma)) :epsilon) (:and (:cat (:or (:* (:or :epsilon :sigma)) (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma)))) (:* (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma))))) (:cat :sigma (:* :sigma)) (:cat (:* :sigma) (:or (:and (:not Byte) (:not Integer) (:not Long) (:not Short)) :epsilon Double Float)) :epsilon) (:and (:cat (:or (:* (:or :epsilon :sigma)) (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma)))) (:* (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma))))) (:cat :sigma (:* :sigma)) :epsilon) (:and (:cat (:or (:and (:not Byte) (:not Integer) (:not Long) (:not Short)) :epsilon Double Float) (:* :sigma)) (:cat :sigma (:* :sigma)) :epsilon) (:and (:cat :sigma (:* :sigma)) :epsilon) (:and (:cat (:* :sigma) :sigma) (:cat (:* :sigma) :sigma (:or (:and (:not Byte) (:not Integer) (:not Long) (:not Short)) :epsilon Double Float)) :epsilon) (:and (:cat (:* :sigma) :sigma) :epsilon) (:and (:cat (:* :sigma) :sigma) :epsilon (:not Byte) (:not Integer) (:not Long) (:not Short)) (:and (:cat :sigma (:* :sigma)) (:cat :sigma :sigma) :epsilon) (:and (:cat :sigma (:* :sigma)) (:cat (:* :sigma) (:or (:and (:not Byte) (:not Integer) (:not Long) (:not Short)) :epsilon Double Float) :sigma) :epsilon) (:and (:cat :sigma (:* :sigma)) :epsilon) (:and (:cat :sigma (:* :sigma)) (:cat :sigma (:* :sigma) :sigma) :epsilon) (:and (:cat :sigma (:* :sigma)) (:cat (:* :sigma) :sigma) :epsilon) (:and (:cat :sigma (:* :sigma)) :epsilon) (:and (:cat :sigma (:* :sigma)) (:cat (:* :sigma) :sigma (:or (:and (:not Byte) (:not Integer) (:not Long) (:not Short)) :epsilon Double Float)) :epsilon) (:and (:cat :sigma (:* :sigma)) :epsilon) (:and (:cat :sigma (:* :sigma)) :epsilon (:not Byte) (:not Integer) (:not Long) (:not Short)) (:and (:cat (:* :sigma) (:or (:and (:not Byte) (:not Integer) (:not Long) (:not Short)) :epsilon Double Float) :sigma) (:cat (:* :sigma) :sigma) (:cat (:or (:* (:or :epsilon :sigma)) (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma)))) (:* (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma))))) :epsilon) (:and (:cat (:* :sigma) :sigma) (:cat (:or (:* (:or :epsilon :sigma)) (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma)))) (:* (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma))))) (:cat :sigma :sigma) :epsilon) (:and (:cat (:* :sigma) :sigma) (:cat (:or (:* (:or :epsilon :sigma)) (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma)))) (:* (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma))))) :epsilon) (:and (:cat (:* :sigma) (:or (:and (:not Byte) (:not Integer) (:not Long) (:not Short)) :epsilon Double Float) :sigma) (:cat (:* :sigma) :sigma) :epsilon) (:and (:cat (:* :sigma) :sigma) (:cat :sigma :sigma) :epsilon) (:and (:cat (:* :sigma) :sigma) :epsilon) (:and (:cat (:* :sigma) (:or (:and (:not Byte) (:not Integer) (:not Long) (:not Short)) :epsilon Double Float) :sigma) (:cat (:or (:* (:or :epsilon :sigma)) (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma)))) (:* (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma))))) (:cat :sigma (:* :sigma)) :epsilon) (:and (:cat (:or (:* (:or :epsilon :sigma)) (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma)))) (:* (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma))))) (:cat :sigma (:* :sigma)) (:cat :sigma :sigma) :epsilon) (:and (:cat (:or (:* (:or :epsilon :sigma)) (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma)))) (:* (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma))))) (:cat :sigma (:* :sigma)) :epsilon) (:and (:cat (:* :sigma) :sigma) (:cat (:* :sigma) :sigma (:or (:and (:not Byte) (:not Integer) (:not Long) (:not Short)) :epsilon Double Float)) (:cat (:or (:* (:or :epsilon :sigma)) (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma)))) (:* (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma))))) :epsilon) (:and (:cat (:* :sigma) :sigma) (:cat (:or (:* (:or :epsilon :sigma)) (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma)))) (:* (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma))))) :epsilon) (:and (:cat (:* :sigma) :sigma) (:cat (:or (:* (:or :epsilon :sigma)) (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma)))) (:* (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma))))) :epsilon (:not Byte) (:not Integer) (:not Long) (:not Short)) (:and (:cat (:* :sigma) :sigma) (:cat (:or (:* (:or :epsilon :sigma)) (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma)))) (:* (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma))))) (:cat :sigma (:* :sigma)) :epsilon) (:and (:cat (:or (:* (:or :epsilon :sigma)) (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma)))) (:* (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma))))) (:cat :sigma (:* :sigma)) (:cat :sigma (:* :sigma) :sigma) :epsilon) (:and (:cat (:or (:* (:or :epsilon :sigma)) (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma)))) (:* (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma))))) (:cat :sigma (:* :sigma)) :epsilon) (:and (:cat (:* :sigma) :sigma) (:cat (:or (:* (:or :epsilon :sigma)) (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma)))) (:* (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma))))) (:cat :sigma (:* :sigma) :sigma) :epsilon) (:and (:cat (:* :sigma) :sigma) (:cat (:or (:* (:or :epsilon :sigma)) (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma)))) (:* (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma))))) :epsilon) (:and (:cat (:* :sigma) :sigma) (:cat :sigma (:* :sigma) :sigma) :epsilon) (:and (:cat (:* :sigma) :sigma) :epsilon) (:and (:cat (:* :sigma) :sigma (:or (:and (:not Byte) (:not Integer) (:not Long) (:not Short)) :epsilon Double Float)) (:cat (:or (:* (:or :epsilon :sigma)) (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma)))) (:* (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma))))) (:cat :sigma (:* :sigma)) :epsilon) (:and (:cat (:or (:* (:or :epsilon :sigma)) (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma)))) (:* (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma))))) (:cat :sigma (:* :sigma)) :epsilon) (:and (:cat (:or (:* (:or :epsilon :sigma)) (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma)))) (:* (:cat (:* (:or :epsilon :sigma)) (:* (:or :epsilon :sigma))))) (:cat :sigma (:* :sigma)) :epsilon (:not Byte) (:not Integer) (:not Long) (:not Short)) :empty-set))

(deftest t-sort-operands-5
  (testing "sort-operands 5"
    (sort-operands
     test-data)))

(deftest t-fixed-point
  (testing "fixed-point"
    (is (= (fixed-point 1 identity =) 1))
    (is (= (fixed-point 10 (fn [n] (if (> n 0) (- n 1) n)) =) 0))))

(deftest t-util-member
  (testing "util member"
    (is (member 1 '(1 2 3)))
    (is (member 2 '(1 2 3)))
    (is (member 3 '(1 2 3)))
    (is (not (member 4 '(1 2 3))))
    
    (is (member 1 [1 2 3]))
    (is (member 2 [1 2 3]))
    (is (member 3 [1 2 3]))
    (is (not (member 4 [1 2 3])))

    (is (member false '(1 false 2)))
    (is (not (member false '(1 2 3))))
    (is (not (member false '(1 nil 2 3))))

    (is (member nil '(1 nil 2)))
    (is (not (member nil '(1 false 2 3))))
    (is (not (member nil '(1 2 3))))

    (is (member true '(1 true 2)))
    (is (not (member true '(1 false 2 3))))
    (is (not (member true '(1 nil 2 3))))
    (is (not (member true '(1 2 3))))

    (is (not (member 1 [])))
    (is (not (member 1 ())))))

(deftest t-fold
  (testing "fold"
    (doseq [seq [[]
                 [1]
                 [1 2]
                 [1 2 3]
                 [1 2 3 4 5 6 7]
                 [1 2 3 4 5 6 7 8]
                 [1 2 3 4 5 6 7 8
                  10 20 30 40 50 60 70 80
                  100 200 300 400 500 600 700 800]]]
      (doseq [fold [clojure-rte.util/pairwise-fold
                    clojure-rte.util/tree-fold]]
        (is (= (reduce + 0 seq)
               (fold + 0 seq))
            (cl-format false "351: ~A" fold))
        (is (= (reduce *' 1 seq)
               (fold *' 1 seq))
            (cl-format false "354: ~A: seq=~A" fold seq))
        (is (= (reduce bit-xor 0 seq)
               (fold bit-xor 0 seq))
            (cl-format false "357: ~A" fold))

        (letfn [(range-ext [left right]
                  (range left (inc right)))]

          (doseq [n (range 100)
                  i (range n)
                  :let [seq (map / (concat (range-ext (- i) -1)
                                           (range-ext 1 i)))]]
            (is (= 0 (reduce + 0 seq)))

            (is (= (reduce + 0 seq)
                   (fold + 0 seq))
                (cl-format false "374: ~A" fold))))))))

(deftest t-fold-2
  (testing "fold 2"
    (let [s (map / [23 29 31 37 41 43 47 53 57 67
                    71 73 79 83 89 97])
          ]
      (doseq [fold [clojure-rte.util/pairwise-fold
                    clojure-rte.util/tree-fold]]

        (reduce + 0 s)
        (fold + 0 s)
        (is (= (reduce + 0 s)
               (fold + 0 s))
            (print-str fold))))))

(deftest t-fold-3
  (testing "fold 3"
    (doseq [f [reduce clojure-rte.util/pairwise-fold clojure-rte.util/tree-fold ]]
      (let [x (atom 0)
            s [1/23 1/29
               1/31 1/37
               1/41 1/43 1/47
               1/53 1/57 
               1/67
               1/71 1/73
               1/83 1/89
               1/97
               ]
            denom-digits (fn [ratio]
                           (if (zero? ratio) 0
                               (count (print-str (denominator ratio)))))
            ]
        (f (fn [acc i] 
             (let [m (max (denom-digits acc)
                          (denom-digits i))]
               (swap! x (fn [old new] (+ old (* new new new))) m)
               (println [:acc acc :i i :max-denom m :sum (deref x)])
               (+ acc i)))
           0/1 s)))))
