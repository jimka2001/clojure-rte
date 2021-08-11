;; Copyright (c) 2020,2021 EPITA Research and Development Laboratory
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

(ns clojure-rte.genus-disjoint-test
  (:require [clojure-rte.rte-core]
            [clojure-rte.genus :as gns]
            [clojure-rte.genus-tester :refer [gen-type]]
            [backtick :refer [template]]
            [clojure.pprint :refer [cl-format]]
            [clojure.test :refer [deftest is testing]]))

(defn -main []
  (clojure.test/run-tests 'clojure-rte.genus-disjoint-test))

(deftest t-discovered-case-disjoint-132
  (testing "discovered-case-disjoint-132"
    ;; t1=SAnd(SNot(SAtomic(TestB)), SOr(SAtomic(Test1), even?))
    ;; t2=SAtomic(int)
    (let [test-b 'Double
          test-1 'clojure.lang.MultiFn
          even '(satisfies even?)
          t1 (template (and (not ~test-b) (or ~test-1 ~even)))
          t2 'Integer]
      (is (= (gns/subtype? t1 (template (not ~test-b)) :dont-know) true))
      (is (= (gns/disjoint? t2 test-b :dont-know) true))
      (is (= (gns/disjoint? t2 test-1 :dont-know) true))
      (is (not= (gns/disjoint? t1 t2 :dont-know) false)))))

(deftest t-discovered-case-disjoint-134
  (testing "discovered-case-disjoint-134"
    (is (= false (gns/disjoint? 'java.lang.Number
                                'java.lang.CharSequence
                                :dont-know)))
    (is (not= true (gns/disjoint? '(and java.lang.Number)
                                  'java.lang.CharSequence
                                  :dont-know)))))

(deftest t-discovered-case-disjoint-135
  (testing "discovered-case-disjoint-135"
    (is (= (gns/inhabited? '(and (not clojure.lang.Symbol) (not java.lang.CharSequence) java.lang.Comparable)
                           :dont-know)
           true))
    (is (= (gns/disjoint? '(and (not clojure.lang.Symbol) (not java.lang.CharSequence) java.lang.Comparable)
                          'java.lang.Comparable
                                     :dont-know)
           false))))
    
(deftest t-discovered-case-134
  (testing "discovered case 134"
    (is (= (gns/inhabited? '(and (not (member 1 2 3 a b c)) (not BigDecimal)) :dont-know)
           true)
        "134/38")
    ;;disjoint? cannot decide (and (not (member 1 2 3 a b c)) (not BigDecimal)) vs java.lang.Object -- assuming not disjoint
    (is (= (gns/disjoint? '(and (not (member 1 2 3 a b c)) (not BigDecimal))
                          'java.lang.Object
                          :dont-know)
           false)
        "134/44")))

(deftest t-disjoint?
  (when (and (resolve 'java.lang.CharSequence)
             (resolve 'java.io.Serializable)
             (resolve 'java.lang.Comparable))
    (testing "disjoint?"

      (is (not (gns/disjoint? 'java.io.Serializable '(and clojure.lang.Symbol (not (member a b))) true)) "case-1")
      (is (not (gns/disjoint? 'java.lang.CharSequence 'String true)) "case-2")
      (is (not (gns/disjoint? 'java.io.Serializable 'java.lang.Comparable true)) "case-3")
      (is (gns/disjoint? 'Integer 'String false) "case-4")
      (is (not (gns/disjoint? 'java.lang.Comparable '(not java.io.Serializable) true)) "case-5")
      (is (not (gns/disjoint? '(and java.lang.Comparable (not clojure.lang.Symbol)) 'java.lang.Object true)) "case-6")

      ;; (disjoint? (and A1 A2 .. An) S)
      ;; if Ai is non empty subset of S
      (is (not (gns/disjoint? '(and Long (not (member 2 3 4))) 'java.lang.Comparable true)) "case-7")

      ;; disjoint? is not smart enough to determine the following,
      ;; must use bdd-type-disjoint? instead
      ;; (is (not (disjoint? '(and java.lang.Number (not (= 0)) (not (member a b c 1 2 3)))
      ;;                     'java.io.Serializable
      ;;                     (constantly true))) "case-8")

      ;; disjoint? is not smart enough to determine the following,
      ;; must use bdd-type-disjoint? instead
      ;; (is (not (disjoint? 'java.io.Serializable
      ;;                     '(and java.lang.Number (not (= 0)) (not (member a b c 1 2 3)))
      ;;                     (constantly true))) "case-9")

      ;; disjoint? is not smart enough to determine the following,
      ;; must use bdd-type-disjoint? instead
      ;; (is (not (disjoint? '(and Long (not (= 0)) (not (= 1)) (not (member a b c 1 2 3)))
      ;;                     'java.io.Serializable
      ;;                     (constantly true))))

      (is (= false (gns/subtype? '(or (member 1 2 3) (= 1)) '(member a 1) :dont-know)))
      (is (not= true (gns/subtype? '(and (or (member 1 2 3) (= 1))) '(member a 1) :dont-know)))

      (is (= false
             (gns/disjoint? '(and (or (member  1 2 3)
                                      (= 1)))
                            '(not (member a 1))
                            :dont-know)))

      )))

(deftest t-disjoint-not?
  (when (and (resolve 'java.lang.Number)
             (resolve 'clojure.lang.ISeq))
    (testing "disjoint not"
      (is (gns/disjoint? '(not Boolean) '(not Object) false)) ;; currently broken
      (is (gns/disjoint? 'Long 'Boolean  false))
      (is (not (gns/disjoint? '(not Long) '(not Boolean) true)))
      (is (not (gns/disjoint? 'clojure.lang.ISeq '(not java.lang.Number) true)))
      ;; because Number is a subtype of java.io.Serializable
      ;;   then Number should be disjoint from (not java.io.Serializable)
      (is (gns/disjoint? 'Number '(not java.io.Serializable) false))
      )))

(deftest t-disjoint-2-14
  (when (and (resolve 'java.lang.Comparable)
           (resolve 'clojure.lang.IMeta))   
    (testing "disjoint 2 14"
      ;; interface vs interface - never disjoint
      (is (not (gns/disjoint? 'java.lang.Comparable 'clojure.lang.IMeta true)))
      (is (not (gns/disjoint? 'clojure.lang.IMeta 'java.lang.Comparable true)))

      ;; final vs interface is superclass
      (is (not (gns/disjoint? 'Integer 'java.lang.Comparable true)))
      (is (not (gns/disjoint? 'java.lang.Comparable 'Integer true)))
      )))

(deftest t-disjoint-2
  (testing "disjoint 2"

    (is (gns/disjoint? 'Integer 'String false)) ; final vs final
    (is (gns/disjoint? 'String 'Integer false)) ; final vs final

    ;; final vs interface not a superclass
    (is (gns/disjoint? 'Integer 'java.lang.CharSequence false))
    (is (gns/disjoint? 'java.lang.CharSequence 'Integer false))

    ;; abstract vs abstract
    (is (gns/disjoint? 'Number 'clojure.lang.ASeq false))
    (is (gns/disjoint? 'clojure.lang.ASeq 'Number false))

    ;; abstract vs interface
    (is (not (gns/disjoint? 'clojure.lang.IHashEq 'clojure.lang.ASeq true)))
    (is (not (gns/disjoint? 'clojure.lang.ASeq 'clojure.lang.IHashEq true)))

    ;;clojure.lang.PersistentList java.lang.Object  java.lang.Number
    (is (not (gns/disjoint? 'clojure.lang.PersistentList 'java.lang.Object true)))
    (is (not (gns/disjoint? 'java.lang.Object  'java.lang.Number true)))
    (is (gns/disjoint? 'clojure.lang.PersistentList 'java.lang.Number false))

    (is (gns/disjoint? 'Long '(not Long) false))
    (is (gns/disjoint? '(not Long) 'Long false))

    (is (gns/disjoint? 'Long '(not java.io.Serializable) false))
    (is (gns/disjoint? '(not java.io.Serializable) 'Long false))
    ))

(deftest t-disjoint-member
  (testing "disjoint member"
    (is (gns/disjoint? '(member 1 2 3) '(member 5 6 ) false) "line 139")
    (is (not (gns/disjoint? '(member 1 2 3) '(member 3 6 ) true)) "line 140")
    (is (gns/disjoint? '(= 1) '(= 2) false) "line 141")
    (is (gns/disjoint? '(= 1) '(member 2 3 4) false) "line 142")
    (is (gns/disjoint? '(member 2 3 4) '(= 1) false) "line 143")
    (is (not (gns/disjoint? '(= 3) '(member 2 3 4) true)) "line 144")
    (is (not (gns/disjoint? '(member 2 3 4) '(= 3) true)) "line 145")

    (is (gns/disjoint? '(member 1 2 3) '(not Long) false)  "line 147")
    (is (not (gns/disjoint? '(member 1 "2" 3) '(not Long) true)) "line 148")

    (is (not (gns/disjoint? '(member a b c 1 2 3) '(not (member 1 2 3)) true)) "line 150")

    (is (gns/disjoint? '(member 1 2 3) '(not (member a b c 1 2 3)) false) "line 152")
    
    (is (gns/disjoint? 'Long '(not Long) false) "line 154")
    (is (gns/disjoint? '(not Long) 'Long false) "line 155")

    (is (gns/disjoint? 'String '(member 1 2 3) false))
    (is (gns/disjoint? '(member 1 2 3) 'String false))

    (is (not (gns/disjoint? 'String '(not (member 1 2 3))
                            true)))
    (is (not (gns/disjoint? 'Long '(not (member 1 2 3))
                            true)))
    (is (not (gns/disjoint? 'Object '(not (member 1 2 3))
                            true)))
    (is (not (gns/disjoint? 'Object '(not (= 0))
                            true)))
    (is (not (gns/disjoint? 'Long '(not (= 0))
                            true)))
    (is (not (gns/disjoint? 'java.lang.CharSequence '(not (member a b c a b c))
                            true)))
    (is (not (gns/disjoint? '(member 3 2)
                        '(member 3 4)
                        true)))
    (is (not (gns/disjoint? '(member 3 2)
                        '(not (member 3 4))
                        true)))
    (is (not (gns/disjoint? '(member [1 2 3] [1 2] [1] [])
                        '(not (member [1 2 3] [2 1 3]))
                        true)))
    (is (not (gns/disjoint? '(and String (not (member a b c 1 2 3)))
                        'java.lang.Comparable
                        true)))
    ))

(deftest t-symmetric-disjoint
  (testing "disjoint test which was failing"
    (let [t1 '(= ())
          t2 '(and clojure.lang.IPersistentList
                   clojure.lang.IPersistentVector
                   (not (rte (:* (:cat clojure.lang.IPersistentVector (:* :sigma))))))]
      (is (=
           (gns/disjoint? t1 t2 :dont-know)
           (gns/disjoint? t2 t1 :dont-know))))))

(deftest t-disjoint-and
  (testing "disjoint of intersection types"
    (is (= false (gns/disjoint? '(and BigDecimal clojure.lang.IMeta) 'java.lang.Number :dont-know)) "test 1")
    (is (= false (gns/disjoint? '(and clojure.lang.IMeta BigInteger) 'java.lang.Number :dont-know)) "test 2")
    (is (= false (gns/disjoint? '(and clojure.lang.IMeta clojure.lang.Ratio) 'java.lang.Number :dont-know)) "test 3")
    (is (= true  (gns/disjoint? '(and clojure.lang.IMeta clojure.lang.Ratio) 'String :dont-know)) "test 4")

    (is (= true  (gns/inhabited? '(and BigDecimal (not clojure.lang.IMeta)) :dont-know)) "test 5")
    (is (= false (gns/inhabited? '(not Object) :dont-know)) "test 6")
    (is (= false (gns/inhabited? '(and (not Object)) :dont-know)) "test 7")
    (is (= false (gns/disjoint? 'BigDecimal 'clojure.lang.IMeta :dont-know)) "test 8")
    (is (= false (gns/disjoint? 'BigDecimal '(not clojure.lang.IMeta) :dont-know)) "test 9")

    (is (= false (gns/disjoint? '(and BigDecimal (not clojure.lang.IMeta)) 'java.lang.Number :dont-know))
        "test 10")
    (is (= false  (gns/disjoint? '(and BigInteger (not clojure.lang.IMeta)) 'java.lang.Number :dont-know))
        "test 11")
    (is (= false (gns/disjoint? '(and clojure.lang.Ratio (not clojure.lang.IMeta)) 'java.lang.Number :dont-know))
        "test 12")

    (is (= false (gns/disjoint? 'java.io.Serializable '(not Long) :dont-know))
        "test 13")
    (is (= true (gns/disjoint? '(not java.io.Serializable) 'Long :dont-know))
        "test 14")
    ))

(deftest t-disjoint-public
  (testing "disjoint for classes which are not abstract, and not interface, and not final"
    ;; TODO finish this test
    ;; find a class which is :public and has subclasses other than Object, and test
    ;;   disjoint? with it.
    clojure.lang.PersistentVector
    ;; clojure.lang.IHashEq is a superclass of clojure.lang.PersistentVector
    BigInteger
    BigDecimal
    clojure.lang.Ratio
    
    ))
(deftest t-disjoint-commutative
  (testing "commutativity of disjoint?"
      (doseq [_ (range 200 )
              n (range 5)
              :let [td-1 (gen-type n)
                    td-2 (gen-type n)
                    d12 (gns/disjoint? td-1 td-2 :dont-know)
                    d21 (gns/disjoint? td-2 td-1 :dont-know)]]
        (is (= d12 d21)
            (cl-format false "~%td-1=~A~%td-2=~A~%disjoint? td-1 td2 = ~A~%disjoint? td-1 td2 = ~A~%"
                       td-1 td-2
                       d12 d21)))))
