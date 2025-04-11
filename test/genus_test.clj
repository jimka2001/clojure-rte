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

(ns genus-test
  (:require [rte-core]
            [rte-construct :refer [with-compile-env]]
            [genus :as gns]
            [genus-tester :refer [gen-type *test-values*]]
            [util :refer [ member ]]
            [clojure.pprint :refer [cl-format]]
            [clojure.test :refer [deftest is]]))

(defn -main []
  (clojure.test/run-tests 'genus-test))

(def test-verbose false)

(defmacro testing
  [string & body]
  `(gns/call-with-genus-env
    (fn []
      (when test-verbose
        (println [:testing ~string :starting (java.util.Date.)]))
      (clojure.test/testing ~string ~@body)
      (when test-verbose
        (println [:finished  (java.util.Date.)])))))

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
    (is (gns/typep 3 '(satisfies integer?)))
    (is (gns/typep 3 '(? integer?)))))

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

(deftest t-inhabited-bizarre
  (testing "inhabited?"
    (is (= :abstract (gns/class-primary-flag 'Number)))
    (is (= :interface (gns/class-primary-flag 'clojure.lang.IPersistentList)))
    (is (= :public (gns/class-primary-flag 'clojure.lang.PersistentList)))
    (is (= true (gns/disjoint-classes? 'clojure.lang.PersistentList 'Number)))
    (is (= false (isa? 'Number 'clojure.lang.IPersistentList)))
    (is (= false (isa? 'clojure.lang.IPersistentList 'Number)))
    (is (= false (gns/disjoint-classes?  'Number 'clojure.lang.IPersistentList)))
    (is (= false (gns/disjoint? 'Number 'clojure.lang.IPersistentList :dont-know)))
    (is (= true (gns/inhabited? '(and Number clojure.lang.IPersistentList) :dont-know)))))

(deftest t-expand-satisfies
  (testing "expand-satisfies"
    (is (= (gns/expand-satisfies nil)
           nil) "test 0")
    (is (= (gns/expand-satisfies 'x)
           'x) "test 1")

    (is (thrown? Exception (gns/expand-satisfies '(satisfies))) "test 2a")
    (is (thrown? Exception (gns/expand-satisfies '(satisfies f1 f2))) "test 2b")

    (is (thrown? Exception (gns/expand-satisfies '(?))) "test 2c")
    (is (thrown? Exception (gns/expand-satisfies '(? f1 f2))) "test 2d")

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
    (is (not (gns/typep 3 '(satisfies genus-test/test-predicate)))
        "test 0")

    (is (not (gns/typep 3 '(? genus-test/test-predicate)))
        "test 0b")

    (is (gns/typep 13 '(satisfies genus-test/test-predicate))
        "test 1")

    (is (gns/typep 13 '(? genus-test/test-predicate))
        "test 1b")

    ;; cannot use satisfies with undecorated function name
    ;;   from local name space.
    (is (thrown? Exception (gns/typep 13 '(satisfies test-predicate))))

    ;; cannot use ? with undecorated function name
    ;;   from local name space.
    (is (thrown? Exception (gns/typep 13 '(? test-predicate))))
    ))
  

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
         '(or (member "a" "b") Double))
        "test 0")))



(deftest t-inhabited-random
  (testing "checking some randomly generated types for inhabited?"
    (with-compile-env []
      (letfn [(check [type-designator msg]
                (let [inh (gns/inhabited? type-designator :dont-know)
                      t2 (gns/canonicalize-type type-designator)
                      inh-2 (gns/inhabited? t2 :dont-know)]
                  (cond
                    (= true inh)
                    (is (not= false inh-2)
                        (cl-format false "~A: ~W~@
                                          is inhabited but its canonicalized form is not~@
                                          ~W"
                                   msg
                                   type-designator
                                   t2))

                    (= false inh)
                    (is (not= true inh-2)
                        (cl-format false "~A: ~W~@
                                          is not inhabited but its canonicalized form is~@
                                          ~W"
                                   msg
                                   type-designator
                                   t2)))))]
        (check '(and (and (and (or (member a b c 1 2 3) (= 1))
                               (not (member a b c a b c)))
                          (not (and java.lang.CharSequence
                                    (satisfies ratio?))))
                     (and (not (and (satisfies symbol?)
                                    (satisfies ratio?)))
                          (not (or (= a)
                                   (= 1)))))
               431)

        (check '(and (and (and (or (member a b c 1 2 3) (= 1))
                               (not (member a b c a b c)))
                          (not (and java.lang.CharSequence
                                    (? ratio?))))
                     (and (not (and (? symbol?)
                                    (? ratio?)))
                          (not (or (= a)
                                   (= 1)))))
               432)
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

        (check '(and (or (satisfies int?)
                         (and java.lang.Number))
                     java.lang.CharSequence)
               4655)
        (check '(and (or (? int?)
                         (and java.lang.Number))
                     java.lang.CharSequence)
               46551)
        (check '(and (or (and (satisfies int?))
                         (and java.lang.Number))
                     java.lang.CharSequence)
               4654)
        (check '(and (or (and (? int?))
                         (and java.lang.Number))
                     java.lang.CharSequence)
               46541)
        (check '(and (or (and (satisfies int?))
                         (and java.lang.Number))
                     java.lang.CharSequence)
               4653)
        (check '(and (or (and (? int?))
                         (and java.lang.Number))
                     java.lang.CharSequence)
               46531)
        (check '(and (or (and (satisfies symbol?)
                              (satisfies int?))
                         (and java.lang.Number))
                     (satisfies integer?)
                     java.lang.CharSequence)
               4652)
        (check '(and (or (and (? symbol?)
                              (? int?))
                         (and java.lang.Number))
                     (? integer?)
                     java.lang.CharSequence)
               46521)
        (check '(and (or (and (satisfies symbol?)
                              (satisfies int?))
                         (and java.lang.Number
                              java.io.Serializable))
                     (satisfies integer?)
                     java.lang.CharSequence)
               4651)
        (check '(and (or (and (? symbol?)
                              (? int?))
                         (and java.lang.Number
                              java.io.Serializable))
                     (? integer?)
                     java.lang.CharSequence)
               46511)
        (check '(and (or (and (satisfies symbol?)
                              (satisfies int?))
                         (and java.lang.Number
                              java.io.Serializable))
                     (not (not (satisfies integer?)))
                     java.lang.CharSequence)
               465)

        (check '(and (or (and (? symbol?)
                              (? int?))
                         (and java.lang.Number
                              java.io.Serializable))
                     (not (not (? integer?)))
                     java.lang.CharSequence)
               4651)

        (check '(and (and (or (and (satisfies symbol?)
                                   (satisfies int?))
                              (and java.lang.Number
                                   java.io.Serializable))
                          (not (not (satisfies integer?))))
                     java.lang.CharSequence)
               466)
        (check '(and (and (or (and (? symbol?)
                                   (? int?))
                              (and java.lang.Number
                                   java.io.Serializable))
                          (not (not (? integer?))))
                     java.lang.CharSequence)
               4661)
        (check '(and (and (or (member a b c 1 2 3)
                              (= 1))
                          (not (member a b c a b c))
                          (not java.lang.CharSequence))

                     (and  (not (= a))
                           (not (= 1))))
               475)

        (dotimes [_ 1000]
          (check (gen-type 4)
                 479))))))

(deftest t-canonicalize-not-member
  (testing "canonicalize (and (not (member ...)))"
    (is (= (gns/canonicalize-type '(and Long (not (member 1 2))
                                        (not (= 12))
                                        (not (= 13))
                                        (not (member 3 4))))
           '(and (not (member 1 2 3 4  12 13)) Long)) "line 436")
    (is (= (gns/canonicalize-type '(and Long (not (member 1 2))
                                        (not (= "hello"))
                                        (not (= 13))
                                        (not (member 3 4))))
           '(and (not (member 1 2 3 4 13)) Long)) "line 438")
    (is (= (gns/canonicalize-type '(and Long (not (member 1 2 "world"))
                                        (not (= "hello"))
                                        (not (= 13))
                                        (not (member 3 4))))
           '(and (not (member 1 2 3 4 13)) Long)) "line 440")
    (is (= (gns/canonicalize-type '(and String (not (member 1 2 "world"))
                                        (not (= "hello"))
                                        (not (= 13))
                                        (not (member 3 4))))
           '(and (not (member "hello" "world")) String)) "line 442")
    (is (= (gns/canonicalize-type '(and Boolean (not (member 1 2 "world"))
                                        (not (= "hello"))
                                        (not (= 13))
                                        (not (member 3 4))))
           'Boolean) "line 444")
    ))

(deftest t-mdtd
  (testing "mdtd"
    (with-compile-env ()
      (is (= (set (map first (gns/mdtd #{'java.lang.Exception 'clojure.lang.ExceptionInfo})))
             #{`(~'not java.lang.Exception)
               `(~'and  (~'not clojure.lang.ExceptionInfo) java.lang.Exception)
               'clojure.lang.ExceptionInfo})))))

(deftest t-type-membership
  (testing "random type membership"
    
    (doseq [depth (range 4)
            _reps (range (/ 400 (inc depth)))
            :let [nf (rand-nth [:dnf :cnf :none])
                  td (gen-type depth)
                  td-canonical (gns/canonicalize-type td nf)]
            value *test-values*]
      (is (= (gns/typep value td)
             (gns/typep value td-canonical))
          (cl-format false "~%value=~A belongs to type but not its canonicalized form~%  td=~A~%  canonicalized=~A~%  nf=~A" value td td-canonical nf)))))

(deftest t-nf-subset
  (testing ""
    ()))

;; TODO enable this
;;
;; (deftest t-compute-nf
;;   (testing "to-nf"
;;     (let [a '(:= "a")
;;           b '(:= "b")
;;           c '(:= "c")
;;           x '(:= "x")
;;           y '(:= "y")
;;           ]
          
;;       (is (= (gns/canonicalize-type (template (not (and ~a ~b ~c))) :dnf)
;;              (template (or (not ~a) (not ~b) (not ~c))))
;;           645)
;;       (is (= (gns/canonicalize-type (template (not (and a b c))) :cnf)
;;              (template (or (not a) (not b) (not c))))
;;           648)
;;       (is (= (gns/canonicalize-type '(not (or ~a ~b ~c)) :dnf)
;;              (template (and (not ~a) (not ~b) (not ~c))))
;;           651)
      
;;       (is (= (gns/canonicalize-type (template (not (or ~a ~b ~c))) :cnf)
;;              (template (and (not a) (not b) (not c))))
;;           655)
;;       (is (= (gns/canonicalize-type (template (not (and (or ~a ~b) (or ~x ~y)))) :dnf)
;;              nil)
;;           661))))


(deftest t-construction
  (testing "programmatic type construction"
    (is (gns/And Integer))
    (is (gns/And Integer String))
    (is (gns/Or Integer String))
    (is (gns/Not Integer))
    (is (gns/Not 'Integer))
    (is (gns/Satisfies even?))

    (is (thrown? Exception (gns/Not 'Integer 'String)))
))

(deftest t-valid-type
  (testing "valid-type?"
    (is (gns/valid-type? Integer))
    (is (gns/valid-type? 'Integer))
    (is (gns/valid-type? '(not Integer)))
    (is (gns/valid-type? (list 'not Integer)))
    (is (not (gns/valid-type? '(:not Integer))))
    (is (not (gns/valid-type? '(:and Integer))))
    (is (not (gns/valid-type? '(:or Integer))))
    (is (gns/valid-type? '(and)))
    (is (gns/valid-type? '(or)))
    (is (gns/valid-type? '(and Integer)))
    (is (gns/valid-type? '(or Integer)))
    (is (gns/valid-type? '(and Integer String)))
    (is (gns/valid-type? '(or Integer String)))
    (is (gns/valid-type? '(member 1 2 3)))
    (is (gns/valid-type? '(= 1)))
    (is (gns/valid-type? '(rte (:cat String :sigma))))
    (is (gns/valid-type? '(satisfies even?)))
    (is (gns/valid-type? (gns/Satisfies even?)))


))
    
