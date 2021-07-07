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

(ns clojure-rte.genus-test
  (:require [clojure-rte.rte-core]
            [clojure-rte.rte-construct :refer [with-compile-env]]
            [clojure-rte.genus :as gns]
            [clojure-rte.util :refer [call-with-collector member]]
            [backtick :refer [template]]
            [clojure.pprint :refer [cl-format]]
            [clojure.test :refer [deftest is testing]]))

(defn -main []
  (clojure.test/run-tests 'clojure-rte.genus-test))

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

(deftest t-typep
  (testing "typep"
    (is (gns/typep 3 :sigma))
    (is (not (gns/typep 3 :empty-set)))
    (is (gns/typep 3 '(or Long Double)))
    (is (not (gns/typep 3 '(and Long Double))))
    (is (gns/typep 3 '(and Long (not Double))))
    (is (gns/typep 3 '(not String)))
    (is (gns/typep 3 '(= 3)))
    (is (gns/typep 3 '(member 1 2 3 4)))
    (is (gns/typep 3 '(satisfies integer?)))))

(deftest t-member
  (testing "member type"
    (is (gns/typep 3 '(member 1 2 3)) "test 0")
    (is (not (gns/typep true '(member 1 2 3))) "test 1")
    (is (not (gns/typep false '(member 1 2 3))) "test 2")
    (is (not (gns/typep nil '(member 1 2 3))) "test 3")
    (is (not (gns/typep () '(member 1 2 3))) "test 4")
    (is (gns/typep () '(member 1 () 3)) "test 5")
    (is (gns/typep true '(member 1 true 3)) "test 6")
    (is (gns/typep false '(member 1 false 3)) "test 7")
    (is (gns/typep nil '(member 1 nil 3)) "test 8")
    (is (not (gns/typep false '(member 1 nil 3))) "test 9")
    (is (not (gns/typep nil '(member 1 false 3))) "test 10")))


(deftest t-type-max
  (testing "type-max"
    (is (= 'Number (gns/type-max '(Number Integer))))
    (is (= 'Number (gns/type-max '(Integer Number))))))

(deftest t-type-min
  (testing "type-min"
    (is (= 'Integer (gns/type-min '(Number Integer))))
    (is (= 'Integer (gns/type-min '(Integer Number))))))

(deftest t-map-type-partitions
  (testing "map-type-partitions"
    (is (not (contains? (set
                         (call-with-collector (fn [collect]
                                                (gns/map-type-partitions ['Long 'Integer 'Object]
                                                                     (fn [left right]
                                                                       (collect [left right]))))))
                        ['(Object) ()])) "should not contain 1")
    (is (= (set
            (call-with-collector (fn [collect]
                                   (gns/map-type-partitions ['Long 'Integer 'Object]
                                                        (fn [left right]
                                                          (collect [left right]))))))
           #{[() '(Object)]
             ['(Integer) ()]
             ['(Long) ()]
             ['(Object) '(Integer Long)]})
        "expected content"
)))

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

(deftest t-subtype?-and
  (testing "subtype? and"
    (is (gns/subtype? '(and String (not (= "a")))
                      'String
                      false)
        "test 1")
    (is (gns/subtype? '(and String (not (member "a" "b" "c")))
                      'java.io.Serializable
                      false)
        "test 2")
    (is (gns/subtype? '(and Long (not (member 1 2)) (satisfies odd?))
                      '(and Long (satisfies odd?))
                      false)
        "test 3")

    (is (not (gns/subtype? '(and Long (not (member 0 2 4 6)))
                           '(not Long)
                           :dont-know))
        "test 4")
))

(deftest t-subtype?
  (testing "subtype?"
    ;; adding failing test, TODO need to fix
    (is (not (gns/subtype? 'Long '(not Long) :dont-know)))
    (is (gns/subtype? 'Long '(not Double) false))

    (is (not (gns/subtype? '(not Long) '(not Boolean) true)))
    (is (not (gns/subtype? '(not Boolean) '(not Long) true)))
    
    (is (not (gns/subtype? 'Long '(member 1 2 3) true)))
    (is (not (gns/subtype? 'Long '(member 1 1.2 1.3) true)))
    (is (not (gns/subtype? 'Long '(member 1.1 1.2 1.3) true)))

    (is (not (gns/subtype? 'Long '(not (member 1 2 3)) true)))
    (is (not (gns/subtype? 'Long '(not (member 1 1.2 1.3)) true)))
    (is (gns/subtype? 'Long '(not (member 1.1 1.2 1.3)) false))
    
    (is (gns/subtype? '(member 1 2) '(member 0 1 2 3) false))
    (is (not (gns/subtype? '(member 0 1 2 3) '(member 1 2) true)))
    (is (gns/subtype? '(member 1 2) 'Long false) "line 154")
    (is (not (gns/subtype? '(member 1 2) 'String true)) "line 155")
    (is (not (gns/subtype? '(member 1 2) '(not Long) true)) "line 156")
    (is (gns/subtype? '(member 1 2) '(not String) false) "line 157")))
  
(deftest t-inhabited
  (testing "inhabited?"
    (with-compile-env ()
      (is (= true (gns/inhabited? '(and Number (not (member 1 2 3))) :dont-know)))
      (is (= true (gns/inhabited? 'Long :dont-know)))
      (is (= true (gns/inhabited? '(not Long) :dont-know)))
      (is (= true (gns/inhabited? 'Object :dont-know)))
      (is (= false (gns/inhabited? '(not Object) :dont-know)))
      (is (= true (gns/inhabited? '(rte (:+ Number)) :dont-know)))
      (is (= false (gns/inhabited? '(rte (:and (:+ Number)
                                               (:+ String))) :dont-know))))))

(deftest t-expand-satisfies
  (testing "expand-satisfies"
    (is (= (gns/expand-satisfies nil)
           nil) "test 0")
    (is (= (gns/expand-satisfies 'x)
           'x) "test 1")

    (is (thrown? Exception (gns/expand-satisfies '(satisfies))) "test 2a")
    (is (thrown? Exception (gns/expand-satisfies '(satisfies f1 f2))) "test 2b")

    (is (= (gns/expand-satisfies '(satisfies no-such-function))
           '(satisfies no-such-function)) "test 3")

    (is (= (gns/expand-satisfies '(satisfies list?))
           'clojure.lang.IPersistentList) "test 4")

    (is (= (gns/expand-satisfies '(satisfies integer?))
           '(or Integer Long clojure.lang.BigInt BigInteger Short Byte))
        "test 5")
    
    (is (= (gns/expand-satisfies '(satisfies rational?))
           '(or
             (or Integer Long clojure.lang.BigInt BigInteger Short Byte)
             clojure.lang.Ratio
             BigDecimal))
        "test 6")))

(defn test-predicate [x]
  (> x 10))

(deftest t-satisfies-ns
  (testing "satifies in namespace"
    (is (not (gns/typep 3 '(satisfies clojure-rte.genus-test/test-predicate)))
        "test 0")

    (is (gns/typep 13 '(satisfies clojure-rte.genus-test/test-predicate))
        "test 1")

    ;; cannot use satisfies with undecorated function name
    ;;   from local name space.
    (is (thrown? Exception (gns/typep 13 '(satisfies test-predicate))))))
  
    
(deftest t-canonicalize-and
  (testing "canonicalize-type and"
    (is (member
         (gns/canonicalize-type '(and Double (= "a")))  '(:empty-set (member)))
        "test 0")
    (is (member (gns/canonicalize-type '(and Double (= 1.0)))
                '((= 1.0) (member 1.0))) "test 1")
    (is (= '(member 1.0 2.0)
           (gns/canonicalize-type '(and Double (member 1.0 2.0 "a" "b")))) "test 2")
    (is (= (gns/canonicalize-type '(and Number))
           'Number) "test 3")))

(deftest t-canonicalize-or
  (testing "canonicalize-type or"
    (is (=
         (gns/canonicalize-type '(or Double (member 1.0 2.0 "a" "b")))
         '(or Double (member "a" "b")))
        "test 0")))

(deftest t-symmetric-disjoint
  (testing "disjoint test which was failing"
    (let [t1 '(= ())
          t2 '(and clojure.lang.IPersistentList
                   clojure.lang.IPersistentVector
                   (not (rte (:* (:cat clojure.lang.IPersistentVector (:* :sigma))))))]
      (is (=
           (gns/disjoint? t1 t2 :dont-know)
           (gns/disjoint? t2 t1 :dont-know))))))

(deftest t-disjoint-interfaces
  (testing "disjoint interfaces"
    (is (gns/find-class 'javax.security.auth.spi.LoginModule))
    (is (gns/find-class 'java.net.http.WebSocket))
    (is (not-empty (gns/find-incompatible-members javax.security.auth.spi.LoginModule
                                                    java.net.http.WebSocket)))
    (is (gns/disjoint? 'javax.security.auth.spi.LoginModule
                       'java.net.http.WebSocket
                       false))))

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

(deftest t-inhabited-random
  (testing "checking some randomly generated types for inhabited?"
    (with-compile-env []
      (letfn [(check [type-designator]
                (let [inh (gns/inhabited? type-designator :dont-know)
                      t2 (gns/canonicalize-type type-designator)
                      inh-2 (gns/inhabited? t2 :dont-know)]
                  (cond
                    (= true inh)
                    (is (not= false inh-2)
                        (cl-format false "~A is inhabited but its canonicalized form is not ~A"
                                   type-designator
                                   t2))
                    
                    (= false inh)
                    (is (not= true inh-2)
                        (cl-format false "~A is not inhabited but its canonicalized form is ~A"
                                   type-designator
                                   t2)))))]
        (check '(and (and (and (or (member a b c 1 2 3) (= 1))
                               (not (member a b c a b c)))
                          (not (and java.lang.CharSequence
                                    (satisfies ratio?))))
                     (and (not (and (satisfies symbol?)
                                    (satisfies ratio?)))
                          (not (or (= a)
                                   (= 1))))))
        ;; (check '(and (and (or (member a b c 1 2 3) (= 1))
        ;;                   (not (member a b c a b c))
        ;;                   (not (and java.lang.CharSequence
        ;;                             (satisfies ratio?))))
        ;;              (and (not (and (satisfies symbol?)
        ;;                             (satisfies ratio?)))
        ;;                   (not (or (= a)
        ;;                            (= 1))))))
        ;; (check '(and (and (or (member a b c 1 2 3) (= 1))
        ;;                   (not (member a b c a b c))
        ;;                   (not (and java.lang.CharSequence
        ;;                             (satisfies ratio?))))
        ;;              (not (and (satisfies symbol?)
        ;;                        (satisfies ratio?)))
        ;;              (not (or (= a)
        ;;                       (= 1)))))
        ;; (check '(and (and (or (member a b c 1 2 3) (= 1))
        ;;                   (not (member a b c a b c))
        ;;                   (not (and java.lang.CharSequence
        ;;                             (satisfies ratio?))))
        ;;              (not (and (satisfies symbol?)
        ;;                        (satisfies ratio?)))
        ;;              (and (not (= a))
        ;;                   (not (= 1)))))
        ;; (check '(and (and (or (member a b c 1 2 3) (= 1))
        ;;                   (not (member a b c a b c))
        ;;                   (not (and java.lang.CharSequence
        ;;                             (satisfies ratio?))))
        ;;              (or (not (satisfies symbol?))
        ;;                  (not (satisfies ratio?)))
        ;;              (and (not (= a))
        ;;                   (not (= 1)))))
        (check '(and (and (or (member a b c 1 2 3)
                              (= 1)
                              )
                          (not (member a b c a b c))
                          (not java.lang.CharSequence))
                     
                     (and  (not (= a))
                           (not (= 1))
                           )))
       
        (dotimes [_ 1000]
          (check (gns/gen-type 4)))))))

(deftest t-canonicalize-not-member
  (testing "canonicalize (and (not (member ...)))"
    (is (= (gns/canonicalize-type '(and Long (not (member 1 2)) (not (= 12)) (not (= 13)) (not (member 3 4))))
           '(and Long (not (member 1 2 12 13 3 4)))) "line 436")
    (is (= (gns/canonicalize-type '(and Long (not (member 1 2)) (not (= "hello")) (not (= 13)) (not (member 3 4))))
           '(and Long (not (member 1 2 13 3 4)))) "line 438")
    (is (= (gns/canonicalize-type '(and Long (not (member 1 2 "world")) (not (= "hello")) (not (= 13)) (not (member 3 4))))
           '(and Long (not (member 1 2 13 3 4)))) "line 440")
    (is (= (gns/canonicalize-type '(and String (not (member 1 2 "world")) (not (= "hello")) (not (= 13)) (not (member 3 4))))
           '(and String (not (member "world" "hello")))) "line 442")
    (is (= (gns/canonicalize-type '(and Boolean (not (member 1 2 "world")) (not (= "hello")) (not (= 13)) (not (member 3 4))))
           'Boolean) "line 444")
    ))

(deftest t-random-subtype
  (testing "randomized testing of subtype?"
    (letfn [(check-subtype [td-1 td-2 comment]
              (is (not= false (gns/subtype? td-1 td-2 :dont-know)) (cl-format "~a td-1=~a td-2=~a" comment td-1 td-2))
              (is (not= false (gns/subtype? td-2 td-1 :dont-know)) (cl-format "~a td-2=~a td-1=~a" comment td-2 td-1)))]
      (for [_ (range 200)
            n (range 5)
            :let [rt (gns/gen-type n)
                  rt-can (gns/canonicalize-type rt)]]
        (do
          (check-subtype rt rt-can "rt < rt-can ?")
          (check-subtype rt-can rt "rt-can < rt ?"))))))

          
(deftest t-random-subtype-2
  (testing "randomized testing of subtype? with de morgan"
    (letfn [(check-subtype [td-1 td-2 comment]
              (is (not= false (gns/subtype? td-1 td-2 :dont-know)) (cl-format "~a td-1=~a td-2=~a" comment td-1 td-2))
              (is (not= false (gns/subtype? td-2 td-1 :dont-know)) (cl-format "~a td-2=~a td-1=~a" comment td-2 td-1)))]
    
      (for [_ (range 200)
            n (range 5)
            :let [rt-1 (gns/gen-type n)
                  rt-2 (gns/gen-type n)
                  rt-and-not (template (and (not ~rt-1) (not ~rt-2)))
                  rt-not-or (template (not (or ~rt-1) (or ~rt-2)))
                  ]]
        (do 
          (check-subtype rt-and-not 
                         rt-not-or "rt-and-not < rt-not-or")
          (check-subtype rt-not-or 
                         rt-and-not "rt-not-or < rt-and-not"))))))

(deftest t-combo-conversion-1
  (testing "combo conversion-1"
    (is (= (gns/conversion-1 '(or))
           :empty-set))
    (is (= (gns/conversion-1 '(and))
           :sigma))
    (is (= (gns/conversion-1 '(and x))
           'x))
    (is (= (gns/conversion-1 '(or x))
           'x))
    (is (= (gns/conversion-1 '(and x y))
           '(and x y)))
    (is (= (gns/conversion-1 '(or x y))
           '(or x y)))))

(deftest t-combo-conversion-2
  (testing "combo conversion-2"
    (is (= (gns/conversion-2 '(or x :sigma y))
           :sigma))
    (is (= (gns/conversion-2 '(and x :sigma y))
           '(and x :sigma y)))
    (is (= (gns/conversion-2 '(or x :empty-set y))
           '(or x :empty-set y)))
    (is (= (gns/conversion-2 '(and x :empty-set y))
           :empty-set))))

(deftest t-combo-conversion-3
  (testing "combo conversion-3"
    (is (= (gns/conversion-3 '(or x (not x)))
           :sigma))
    (is (= (gns/conversion-3 '(and x (not x)))
           :empty-set))
    (is (= (gns/conversion-3 '(or x (not y)))
           '(or x (not y))))
    (is (= (gns/conversion-3 '(and x (not y)))
           '(and x (not y))))))

(deftest t-combo-conversion-4
  (testing "combo conversion-4"
    (is (= (gns/conversion-4 '(or x :empty-set y))
           '(or x y)))
    (is (= (gns/conversion-4 '(and x :empty-set y))
           '(and x :empty-set y)))
    (is (= (gns/conversion-4 '(or x :sigma y))
           '(or x :sigma y)))
    (is (= (gns/conversion-4 '(and x :sigma y))
           '(and x y)))))

(deftest t-combo-conversion-5
  (testing "combo conversion-5"
    (is (= (gns/conversion-5 '(or x y z z y))
           '(or x z y)))
    (is (= (gns/conversion-5 '(and x y z z y))
           '(and x z y)))))

(deftest t-combo-conversion-6
  (testing "combo conversion-7"
    (is (= (gns/conversion-6 '(or x (or a b c) (or l m n) y))
           '(or x a b c l m n y)))
    (is (= (gns/conversion-6 '(and x (and a b c) (and l m n) y))
           '(and x a b c l m n y)))
    (is (= (gns/conversion-6 '(or x (and a b c) (or l m n) y))
           '(or x (and a b c) l m n y)))
    (is (= (gns/conversion-6 '(and x (or a b c) (and l m n) y))
           '(and x (or a b c) l m n y)))))

(deftest t-combo-conversion-7
  (testing "combo conversion-7"
    (is (= (gns/conversion-7 '(and w x (or a b c) y z) :dnf)
           '(or (and w x a y z)
                (and w x b y z)
                (and w x c y z))))
    (is (= (gns/conversion-7 '(and w x (or a b c) y z) :cnf)
           '(and w x (or a b c) y z)))

    (is (= (gns/conversion-7 '(or w x (and a b c) y z) :cnf)
           '(and (or w x a y z)
                 (or w x b y z)
                 (or w x c y z))))
    (is (= (gns/conversion-7 '(or w x (and a b c) y z) :cnf)
           '(or w x (and a b c) y z)))

    (is (= (gns/conversion-7 '(or w (not x) (and a (not b))) :dnf)
           '(or w (not x) (and a (not b)))))
    (is (= (gns/conversion-7 '(or w (not x) (and a (not b))) :cnf)
           ;; FIXME 
           nil))

    (is (= (gns/conversion-7 '(not (and a b c)) :dnf)
           nil ;; fixme
           ))

    (is (= (gns/conversion-7 '(not (or a b c)) :dnf)
           nil ;; fixme
           ))

    (is (= (gns/conversion-7 '(not (and a (or b c))) :dnf)
           nil ;; fixme
           ))


    (is (= (gns/conversion-7 '(not (or a (and b c))) :cnf)
           nil ;; fixme
           ))
    (is (= (gns/conversion-7 '(not (and a b c)) :cnf)
           nil ;; fixme
           ))

    (is (= (gns/conversion-7 '(not (or a b c)) :cnf)
           nil ;; fixme
           ))

    (is (= (gns/conversion-7 '(not (and a (or b c))) :cnf)
           nil ;; fixme
           ))

    (is (= (gns/conversion-7 '(not (or a (and b c))) :dnf)
           nil ;; fixme
           ))
    ))
 
