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

(ns rte-test
  (:require [rte-core]
            [rte-construct :as rte
             :refer [nullable? first-types
                     canonicalize-pattern
                     derivative derivative-1
                     with-compile-env rte-trace
                     rte-inhabited? rte-vacuous? rte-to-dfa
                     rte-combine-labels
                     with-rte]]
            [clojure.pprint :refer [cl-format]]
            [rte-extract :refer [dfa-to-rte rte-equivalent?]]
            [clojure.test :refer [deftest is] :exclude [testing]]
            [util :refer [member]]
            [genus :as gns]
            [genus-tester :refer [*test-types*]]
            [rte-tester :refer [gen-rte]]
            [dot :as dot]
            [backtick :refer [template]]
            [xymbolyco :as xym]))

(def test-verbose false)

(defmacro testing
  [string & body]
  `(with-compile-env []
     (when test-verbose
       (println [:testing 'rte-test ~string :starting (java.util.Date.)]))
     (clojure.test/testing ~string ~@body)
     (when test-verbose
       (println [:finished ' rte-test ~string (java.util.Date.)]))))

(deftest t-nullable
  (testing "nullable"
    (is (not (nullable? :sigma)) "nullable? sigma")
    (is (nullable? :epsilon) "14")
    (is (not (nullable? :empty-set)) "13")
    (is (nullable? '(:and :epsilon :epsilon)) "12")
    (is (nullable? '(:or :epsilon :empty-set)) "11")
    (is (nullable? '(:cat :epsilon :epsilon)) "10")
    (is (not (nullable? '(:cat :epsilon :empty-set))) "9")
    (is (not (nullable? '(:cat :empty-set :epsilon))) "8")
    (is (nullable? '(:cat :epsilon (:* :epsilon))) "7")
    (is (nullable? '(:+ :epsilon)) "6")
    (is (not (nullable? '(:cat :empty-set (:* :empty-set)))) "5")
    (is (not (nullable? '(:cat :empty-set :epsilon))) "4")
    (is (not (nullable? :empty-set)) "3")
    (is (not (nullable? '(:+ :empty-set))) "2")
    (is (nullable? '(:? :epsilon)) "1")))

(deftest t-first-types
  (testing "first-types"
    (is (= #{'a} (first-types 'a)))
    (is (= #{'a 'b} (first-types '(:or a b))))
    (is (= #{'a 'b} (first-types '(:and a b))))
    (is (= #{'b} (first-types '(:cat :epsilon b))))
    (is (= #{'a} (first-types '(:cat a b))))
    (is (= #{'a} (first-types '(:* a))))
    (is (= #{'a} (first-types '(:+ a))))
    (is (= #{'a} (first-types '(:? a))))
    (is (= #{'a} (first-types '(:not a))))
    (is (= #{'a 'b 'c 'e 'f 'g} (first-types '(:and (:or a b)
                                     (:cat c d)
                                     (:* e)
                                     (:+ f)
                                     (:? g)))))
    (is (= #{} (first-types :empty-set)))
    (is (= #{} (first-types :epsilon)))

    (is (= (first-types '(:cat :sigma ::Lion ::Wolf))
           #{:sigma}) "first types cat sigma")

    (is (= (first-types '(:cat))
           (first-types :epsilon)))
    ))

(deftest t-canonicalize-pattern-subtypes
  (testing "canonicalize-pattern with subtypes"
    (is (= 'Number (canonicalize-pattern '(:or Integer Number))) 
        "Number")
    (is (= '(:* :sigma) (canonicalize-pattern '(:or Number (:not Number))))
        "sigma")

    (is (= 'Integer (canonicalize-pattern '(:and Integer Number))) 
        "Integer")
    (is (= :empty-set (canonicalize-pattern '(:and Number (:not Number)))) 
        "empty-set 1")

    ;; intersection of disjoint types
    (is (= :empty-set (canonicalize-pattern '(:and String Integer))) 
        "empty-set 2")))


(deftest t-derivative
  (testing "derivative"
    (is (= (derivative-1 :empty-set java.lang.Comparable)
           :empty-set) "derivative empty-set w.r.t A")

    ;; :sigma
    (is (= (derivative-1 :sigma :sigma)
           :epsilon) "derivative sigma wrt sigma")
    (is (= (derivative-1 :sigma :epsilon)
           :sigma) "derivative sigma wrt epsilon")
    (is (= :epsilon
           (derivative-1 :sigma java.lang.Comparable)) "derivative sigma wrt A")

    ;; :epsilon
    (is (= (derivative-1 :epsilon :epsilon)
           :epsilon))
    (is (= (derivative-1 :epsilon java.lang.Comparable)
           :empty-set))
    (is (= (derivative-1 :epsilon :empty-set)
           :empty-set))
    (is (= (derivative-1 :epsilon :sigma)
           :empty-set))

    ;; :empty-set
    (is (= (derivative-1 :empty-set :epsilon)
           :empty-set))
    (is (= (derivative-1 :empty-set java.lang.Comparable)
           :empty-set))
    (is (= (derivative-1 :empty-set :empty-set)
           :empty-set))
    (is (= (derivative-1 :empty-set :sigma)
           :empty-set))

    ;; type
    (is (= (derivative-1 java.lang.Comparable java.lang.Comparable)
           :epsilon))
    (is (= (derivative-1 'Number 'String)
           :empty-set) "derivative disjoint types")

    ;; or
    
    (is (= (derivative-1 '(:or Double String)
                       'Double)
           :epsilon))

    ;; cat
    (is (thrown? clojure.lang.ExceptionInfo
                 (derivative-1 '(:cat (:or java.io.Serializable java.lang.Comparable) Long)
                             'java.io.Serializable))
        "derivative wrt overlpping type not possible")
    
    (is (= (derivative-1 '(:cat (:or Double String) Long)
                       'Double)
           'Long) "derivative cat with reduction 1")

    (is (= (derivative-1 '(:cat (:or Double String) Long)
                       'String)
           'Long) "derivative cat with reduction 2")

    (is (= (derivative-1 '(:cat Number Number Number)
                       'Number)
           '(:cat Number Number)) "line 237")

    (is (= (derivative-1 '(:cat (:or java.io.Serializable Number) Number Number)
                       'Number)
           '(:cat Number Number))  "line 277")
    (is (= (derivative-1 '(:cat (:or String Number) Number Number)
                       'String)
           '(:cat Number Number))  "line 280")
    ))

(deftest t-derivative-1
  (testing "previous failure"
    (is (= '(:* :sigma)
           (derivative-1 '(:not (:cat Boolean :sigma)) '(not Boolean)))
        "test 308")

    (is (= '(:* :sigma)
           (derivative-1 '(:not (:cat Boolean :sigma (:* :sigma))) '(not Boolean)))
        "test 312")
    
    (is (= '(:* :sigma)
           (derivative-1 '(:and (:not (:cat Boolean :sigma (:* :sigma)))
                              (:not (:cat Boolean :sigma))) '(not Boolean))))))


(deftest t-derivative-factors
  (testing "derivative factors"
    (let [z '(= 0)
          f '(= false)
          i '(satisfies int?)
          s 'String
          rt `(:and (:or (:* (:and :sigma (:not ~i)))
                         :epsilon)
                    (:* (:* (:or (:or ~z ~f) (:* ~s)))))
          ft (first-types rt)]
      (is (= ft #{'String
                  '(= false)
                  :sigma
                  'Integer
                  'Short
                  '(= 0)
                  'Long
                  'Byte}))
      (doseq [[td factors disjoints] (gns/mdtd ft)]
        (let [deriv (derivative rt td factors disjoints)
              can (canonicalize-pattern deriv)]
          nil
          ;;(cl-format true "~&~
          ;;                 ~,4@Tderiv=~A~@
          ;;                 ~,4@Tcan=~A~%"
          ;;           deriv
          ;;           can)
          )))))


(deftest t-rte-to-dfa
  (testing "rte-to-dfa"
    (is (get (methods gns/-disjoint?) 'rte) "test 367")
    (with-compile-env ()
      (is (rte-to-dfa '(:cat :epsilon (:+ (:* :epsilon)) :sigma)) "dfa 1")
      ;; (is (thrown? clojure.lang.ExceptionInfo
      ;;              (rte-to-dfa '(:permute ::Wolf (:? (:+ :empty-set)) (:+ (:* (:and)))))) "dfa 2")
      )))



(deftest t-syntax
  (testing "syntax"
    (with-compile-env ()
      (is (thrown? clojure.lang.ExceptionInfo (rte/compile '(:* :epsilon :epsilon))))
      (is (thrown? clojure.lang.ExceptionInfo (rte/compile '(:? :epsilon :epsilon))))
      (is (thrown? clojure.lang.ExceptionInfo (rte/compile '(:+ :epsilon :epsilon)))))))

(deftest t-boolean-types
  (testing "rte/match with Boolean types"
    (with-compile-env []
      (is (rte/match '(:cat (or Boolean Long)) [42]) "test 1")
      (is (rte/match '(:* (or Boolean Long)) [])  "test 2")
      (is (rte/match '(:* (or Boolean Long)) [42 ])  "test 3")
      (is (rte/match '(:* (or Boolean Long)) [42 43])  "test 4")
      (is (rte/match '(:* (or Boolean Long)) [42 43 false])  "test 5")

      (is (rte/match '(:* (and Number Long (not (= 0)))) [])  "test 6")
      (is (rte/match '(:* (and Number Long (not (= 0)))) [42])  "test 7")
      (is (rte/match '(:* (and Number Long (not (= 0)))) [42 43 ])  "test 8")
      (is (not (rte/match '(:* (:and Number
                                     Long
                                     (:and :sigma
                                           (:not (= 0)))))
                          [42 43 0 44])) "test 9a")
      (is (not (rte/match '(:* (:and Number
                                     Long
                                     ;; (:not (:and :sigma (= 0))) is all of (:* :sigma) except 0
                                     ;;   this includes :epsilon
                                     ;;   and incudes (:cat Long Long)
                                     ;; but ANDed with Long excludes (:cat Long Long)
                                     (:not (:and :sigma (= 0)))))
                          [42 43 0 44])) "test 9b")
      (is (not (rte/match '(:* (:and Number
                                     Long
                                     ;; (:not (:and :sigma (= 0))) is all of (:* :sigma) except 0
                                     ;;   this includes :epsilon
                                     ;;   and incdes (:cat Long Long)
                                     (:not (:and :sigma (= 0)))))
                          [42 43 0])) "test 9c")
      (is (not (rte/match '(:* (and Number Long (not (= 0)))) [42 43 0 44]))  "test 9d")

      (is (not (rte/match '(:* (:cat Number Long (not (= 0)))) [42 43 0 44]))  "test 9e")
      (is (rte/match '(:* (:cat Number Long (:not (= 0)))) [42 43 0 44]))  "test 9f")))

(deftest t-?
  (testing "rte :?"
    (with-compile-env []
      (is (rte/match '(:? Long) [42]))
      (is (rte/match '(:? Long) []))
      (is (not (rte/match '(:? Long) [42 43])))
      (is (not (rte/match '(:? Long) ["hello"]))))))

(deftest t-+
  (testing "rte :+"
    (with-compile-env []
      (is (rte/match '(:+ Long) [42]))
      (is (rte/match '(:+ Long) [42 43]))
      (is (rte/match '(:+ Long) [42 43 44]))
      (is (not (rte/match '(:+ Long) [])))
      (is (not (rte/match '(:+ Long) ["hello"])))
      (is (not (rte/match '(:+ Long) ["hello" "hello"])))
      (is (not (rte/match '(:+ Long) ["hello" 42])))
      (is (not (rte/match '(:+ Long) [42 "hello"]))))))

(deftest t-permute
  (testing "rte :permute"
    (with-compile-env []
      (is (rte/match '(:permute) []))
      (is (not (rte/match '(:permute) [1])))

      (is (rte/match '(:permute Long) [42]))
      (is (not (rte/match '(:permute Long) [42 43])))
      (is (not (rte/match '(:permute Long) [])))

      (is (rte/match '(:permute Long String) [42 "hello"]))
      (is (rte/match '(:permute Long String) ["hello" 42]))
      (is (not (rte/match '(:permute Long String) [42 "hello" 42])))
      (is (not (rte/match '(:permute Long String) ["hello" 42 42])))
      (is (not (rte/match '(:permute Long String) [])))
      
      (is (rte/match '(:permute Long String Boolean) [42 "hello" false]))
      (is (rte/match '(:permute Long String Boolean) [42 false "hello"]))
      (is (rte/match '(:permute Long String Boolean) [false 42 "hello"]))
      (is (rte/match '(:permute Long String Boolean) [false "hello" 42]))
      (is (rte/match '(:permute Long String Boolean) ["hello" 42 false]))
      (is (rte/match '(:permute Long String Boolean) ["hello" false 42]))
      (is (not (rte/match '(:permute Long String Boolean) [42 "hello"])))
      (is (not (rte/match '(:permute Long String Boolean) [42 false "hello" 42])))
      (is (not (rte/match '(:permute Long String Boolean) [])))
      (is (not (rte/match '(:permute Long String Boolean) [false false "hello"])))
      (is (not (rte/match '(:permute Long String Boolean) ["hello" "hello" 42])))
      (is (not (rte/match '(:permute Long String Boolean) [false]))))))

(deftest t-exp
  (testing "exp"
    (with-compile-env ()
      (is (rte/match '(:exp 3 Long) [42 42 42]))
      (is (not (rte/match '(:exp 3 Long) [42 42 42 42])))
      (is (not (rte/match '(:exp 3 5 Long) [42 42])))
      (is (rte/match '(:exp 3 5 Long) [42 42 42]))
      (is (rte/match '(:exp 3 5 Long) [42 42 42 42]))
      (is (rte/match '(:exp 3 5 Long) [42 42 42 42 42]))
      (is (not (rte/match '(:exp 3 5 Long) [42 42 42 42 42 42])))
      (map (fn [n] 
             (let [data (repeat 12 n)
                   pattern `(:cat (:exp ~n (:? Long)) (:exp ~n Long))
                   rte (rte/compile pattern)]

               (is (rte/match rte data) (format "n=%s" n))))
           (range 10)))))

(deftest t-contains-every
  (testing ":contains-every"
    (with-compile-env ()
      (is (rte/match '(:contains-every Long String Boolean) [[] [] [] 42 "hello" true [] [] []]))
      (is (rte/match '(:contains-every Long String Boolean) [[] "hello" [] 42 [] true []]))
      (is (rte/match '(:contains-every Long String Boolean) [[] true [] 42 [] "hello" []]))
      (is (not (rte/match '(:contains-every Long String Boolean) [[] true [] true [] "hello" []]))))))

(deftest t-contains-any
  (testing ":contains-any"
    (with-compile-env ()
      (is (rte/match '(:contains-any Long String Boolean) [[] [] [] 42 "hello" true [] [] []]))
      (is (rte/match '(:contains-any Long String Boolean) [[] [] [] 42 [] [] []]))
      (is (rte/match '(:contains-any Long String Boolean) [[] "hello" [] 42 []]))
      (is (rte/match '(:contains-any Long String Boolean) [[] true []]))
      (is (not (rte/match '(:contains-any Long String) [[] true [] false []]))))))


(deftest t-contains-none
  (testing ":contains-none"
    (with-compile-env ()
      (is (not (rte/match '(:contains-none Long String Boolean) [[] [] [] 42 "hello" true [] [] []])))
      (is (not (rte/match '(:contains-none Long String Boolean) [[] [] [] 42 [] [] []])))
      (is (not (rte/match '(:contains-none Long String Boolean) [[] "hello" [] 42 []])))
      (is (not (rte/match '(:contains-none Long String Boolean) [[] true []])))
      (is (rte/match '(:contains-none Long String) [[] true [] false []])))))

(deftest t-typep-rte
  (testing "typep rte"
    (with-compile-env ()
      (is (gns/typep [3 3.0 "hello" "world" 3 3.0]
                 '(rte (:cat (:+ (:cat Long Double String))
                             (:+ (:cat String Long Double)))))))))

(deftest t-rte-trace
  (testing "rte trace"
    (with-compile-env ()
      (is (rte-trace (rte/compile '(:* (rte Long))))  "test 2")
      (is (rte-trace (rte/compile '(:* (rte (:* Long)))))  "test 6")
      (is (rte-trace (rte/compile '(:cat (:+ (:cat Long Double String))
                                         (:+ (:cat String Long Double)))))  "test 7")

      (is (rte-trace  '(:* (rte Long)))  "test 12")
      (is (rte-trace  '(:* (rte (:* Long))))  "test 16")
      (is (rte-trace  '(:cat (:+ (:cat Long Double String))
                             (:+ (:cat String Long Double))))  "test 17")
      )))

(deftest t-with-rte-4
  (testing "with-rte 4"
    (with-rte [::x (:+ Long)
               ::y (:+ Double)]

      (let [pat (rte/compile '(:cat ::x  ::y))]
        ;; the same as (rte/compile '(:cat (:+ Long) (:+ Double)))
        (is (rte/match pat [1 2 3 1.2 3.4 5.6 7.8]))
        (is (not (rte/match pat [[1 2 3] [1.2 3.4 5.6 7.8]])))
        ))

    (let [pat (rte/compile '(:cat (rte (:+ Long)) (rte (:+ Double))))]
      (is (not (rte/match pat [1 2 3 1.2 3.4 5.6 7.8])))
      (is (rte/match pat [[1 2 3] [1.2 3.4 5.6 7.8]])))))

(deftest t-rte-inhabited
  (testing "rte inhabited?"
    (is (get (methods gns/-disjoint?) 'rte) "test 585")
    (with-compile-env ()

      (is (rte-inhabited? (rte-to-dfa '(:and (:* Long) (:* Double)))))
      (is (rte-vacuous? (rte-to-dfa '(:and (:+ Long) (:+ Double)))))

      (is (rte-inhabited? '(:and (:* Long) (:* Double))))
      (is (rte-vacuous? '(:and (:+ Long) (:+ Double)))))))

(deftest t-rte-with-rte
  (testing "recursive rte"
    (is (get (methods gns/-disjoint?) 'rte) "test 596")
    (with-compile-env ()

      (is (not (gns/disjoint? '(rte (:* Number))
                          '(rte (:* Double))
                          true)))
      (is (not (gns/disjoint? '(rte (:* Number))
                          '(rte (:* String))
                          true)))
      (is (gns/disjoint? '(rte (:+ Number)) 
                     '(rte (:+ String))
                     false))
      (is (rte-to-dfa '(:or (rte (:* Number)) 
                            (rte (:cat Double Number))
                            (rte (:* Double))))))))

(deftest t-issue-60
  (testing "testing issue 60"
    ;; https://gitlab.lrde.epita.fr/jnewton/clojure-rte/-/issues/60

    ;; assert the :rte methods exist for
    ;;   gns/-disjoint? :rte
    (is (get (methods gns/-disjoint?) 'rte) "test 615")
    (is (:not-rte (methods gns/-disjoint?)) "test 616")

    ;;   gns/-subtype? :rte
    (is (get (methods gns/-subtype?) 'rte) "test 619")
    
    ;;   gns/-inhabited? :rte
    (is (get (methods gns/-inhabited?) 'rte) "test 622")
    
    ;;   gns/typep 'rte
    (is (get (methods gns/typep) 'rte) "test 625")
    
    (is (= true (boolean (rte-inhabited? '(:cat String :sigma)))) "test 630")
    (is (= true (gns/inhabited? '(rte (:cat String :sigma)) :dont-know)) "test 631")
    (is (= true (gns/disjoint? '(rte (:cat String :sigma)) '(rte (:cat Character)) :dont-know)) "test 632")
    (is (= true (gns/disjoint? '(rte (:cat String :sigma)) 'String :dont-know)) "test 633")
    (is (= false (gns/inhabited? '(and (rte (:cat String :sigma)) String) :dont-know)) "test 634")
    (is (= true (gns/disjoint? '(and (rte (:cat String :sigma)) String) 'String :dont-know)) "test 635")
    ))

(deftest t-pattern-with-=-and-class
  (testing "pattern with ="
    (with-compile-env ()

      (is (rte/match '(:or Long (= 42)) [42]))
      (is (rte/match '(:or Long (= "42")) [0]))
      (is (rte/match '(:or Long (= "42")) ["42"])))))

(deftest t-pattern-with-=
  (testing "pattern with ="
    (with-compile-env ()
      (is (rte/match '(= 42) [42]))
      (is (not (rte/match '(= 42) [43])))
      (is (not (rte/match '(= 42) [42 42])))
      (is (not (rte/match '(= 42) [])))

      (is (rte/match '(:* (= 42)) []))
      (is (rte/match '(:* (= 42)) [42]))
      (is (rte/match '(:* (= 42)) [42 42 42 42]))

      (is (not (rte/match '(:+ (= 42)) [])))
      (is (rte/match '(:+ (= 42)) [42 ]))
      (is (rte/match '(:+ (= 42)) [42 42 42]))
      (is (not (rte/match '(:+ (= 42)) [42 42 42 43])))

      (is (rte/match '(:or (= 43 ) (= 42)) [42]))
      (is (rte/match '(:or (= 43 ) (= 42)) [43]))
      (is (not (rte/match '(:or (= 43 ) (= 42)) [0])))
      (is (rte/match '(:* (:or (= 43 )(= 42))) []))
      (is (rte/match '(:* (:or (= 43 )(= 42))) [42 42 42 43 42 43]))
      (is (not (rte/match '(:* (:or (= 43 ) (= 42))) [42 42 42 43 42 0 43])))

      )))

(deftest t-rte-combine-labels
  (testing "rte-combine-labels"
    (with-compile-env ()
      (is (= '(or Long String)
             (rte-combine-labels 'Long
                                 'String)))
      (is (= '(or Double Long String)
             (rte-combine-labels '(or Long String)
                                 'Double)))
      (is (= '(or Double Long String)
             (rte-combine-labels 'Double
                                 '(or Long String))))
      (is (= '(or Double Long String)
             (rte-combine-labels '(or Double String)
                                 '(or Long String)))))))

(deftest t-rte-combine-labels-b
  (testing "and/not conversion b"
    (with-compile-env ()
      (is (not= '(:not (= 0))
                (canonicalize-pattern '(not (= 0)))) "test 0")
      (is (not (rte/match '(:* (and Number Long (not (= 0)))) [0])) "test 1")
      (is (rte/match '(:* (and Number Long (not (= 0)))) [1]) "test 2")
      (is (not (rte/match '(:* (and  Long (not (= 0)))) [0])) "test 3")
      (is (rte/match '(:* (and Long (not (= 0)))) [1]) "test 4")
      (is (not (rte/match '(:* (and  Number (not (= 0)))) [0])) "test 5")
      (is (rte/match '(:* (and  Number (not (= 0)))) [1]) "test 6"))))

(deftest t-invalid-type
  (testing "for invalid type within rte"
    (with-compile-env ()
      (is (thrown? Exception (canonicalize-pattern '(not (:or String Number)))) "test 0")
      (is (thrown? Exception (canonicalize-pattern '(not (or String Number)
                                                         (or Long Number)))) "test 1")
      (is (thrown? Exception (canonicalize-pattern '(and (:or String Number)
                                                         (:or :sigma)))) "test 2")
      (is (thrown? Exception (canonicalize-pattern '(or (:and String Number)))) "test 3")
      ;; assert not an Exception
      (canonicalize-pattern '(:or :epsilon (:cat Integer (:* Integer))))
)))

(deftest t-derivative-2
  (testing "derivative previous failure"
    (is (nullable? (derivative-1 '(:and (:cat (:* :sigma))
                                     (:not (:or (:cat Boolean :sigma (:* :sigma))
                                                (:cat Boolean :sigma))))
                              '(not Boolean)))
        "derivative 2")))

(deftest t-dfa-to-rte
  (testing "dfa-to-rte"
    (is (get (methods gns/-disjoint?) 'rte) "test x14")
    (is (= '{13 (:* Integer)}
           (dfa-to-rte (rte-to-dfa '(:* Integer) 13))) "(:* Integer)")

    (is (= '{13 (:cat Integer (:* Integer))
             17 (:cat String (:* String))}
           (dfa-to-rte
            (xym/synchronized-union (rte-to-dfa '(:+ Integer) 13)
                                    (rte-to-dfa '(:+ String) 17))))
        "synchronized union a")

    (is (member (dfa-to-rte
                 (xym/synchronized-union (rte-to-dfa '(:* Integer) 13)
                                         (rte-to-dfa '(:* String) 17)))
                '({13 (:* Integer)
                   17 (:cat String (:* String))}
                  {13 (:* Integer)
                   17 (:cat (:* String) String)})
                )
        "synchronized union b")
    ))

(deftest t-rte-nullable-704
  (testing "nullable of canonicalize"
    (let [rte '(:contains-any (member a b c a b c)
                              :epsilon
                              (:+ (:or (:and (:contains-every)) :empty-set))
                              (:not (:* (member [1 2 3] [2 1 3]))))
          rte-canonicalized (canonicalize-pattern rte)]
      (is (nullable? rte) "test 1")
      (is (nullable? rte-canonicalized) "test 2"))))
    
    
(deftest t-discovered-948
  (testing "discovered test 948"
    (is (= (rte/nullable? '(member (1 2 3) (1 2) (1) []))
           (rte/nullable? '(member [] [1] [1 2] [1 2 3]))))))

(defn test-circular-dfa-rte-flow
  [rte-1]
  (let [dfa (xym/minimize (xym/trim (rte-to-dfa rte-1)))
        rte-2 (get (dfa-to-rte dfa) true)
        rte-1-2 (template (:and ~rte-1 (:not ~rte-2)))
        rte-2-1 (template (:and ~rte-2 (:not ~rte-1)))
        dfa-2-1 (xym/minimize (xym/trim (rte-to-dfa rte-1-2)))
        dfa-1-2 (xym/minimize (xym/trim (rte-to-dfa rte-2-1)))
        rte-2-1b (get (dfa-to-rte dfa-2-1) true)
        rte-1-2b (get (dfa-to-rte dfa-1-2) true)
        null-dfa (rte-to-dfa :empty-set)]
    (if (xym/dfa-equivalent? dfa-2-1 null-dfa)
      (is true)
      (do
        (dot/dfa-to-dot null-dfa :title "null" :view true)
        (dot/dfa-to-dot dfa-2-1 :title "800" :view true)
        (is (xym/dfa-equivalent? dfa-2-1 null-dfa)
            (cl-format false "800: ~%rte-1=~A~%     =~A~%rte-2=~A~%2-1=~A"
                       rte-1 (canonicalize-pattern rte-1) rte-2
                       rte-2-1b))))
    (if (xym/dfa-equivalent? dfa-1-2 null-dfa)
      (is true)
      (do (dot/dfa-to-dot null-dfa :title "null" :view true)
          (dot/dfa-to-dot dfa-1-2 :title "802" :view true)
          (is (xym/dfa-equivalent? dfa-1-2 null-dfa)
              (cl-format false "802: ~%rte-1=~A~%     =~A~%rte-2=~A~%1-2=~A"
                         rte-1 (canonicalize-pattern rte-1) rte-2
                         rte-1-2b))))))

(deftest t-circular-dfa-rte-flow
  (testing "circular dfa rte flow"
    (doseq [depth (range 1)
            _rep (range 10)]
        (test-circular-dfa-rte-flow (gen-rte depth *test-types*)))))

(deftest t-discovered-case-1261
  (test-circular-dfa-rte-flow '(:* (:contains-any))))

(deftest t-discovered-case-1262
  (test-circular-dfa-rte-flow '(:? :sigma)))

(deftest t-discovered-case-1260
  (testing "test 1260"
    (let [rte-1 '(:+ (:cat))
          rte-2 :epsilon
          dfa-1 (rte-to-dfa rte-1)
          dfa-2 (rte-to-dfa rte-2)]
      (is (xym/dfa-equivalent? dfa-1 dfa-2)
          800)
      (is (rte-equivalent? (get (dfa-to-rte dfa-1) true)
                           (get (dfa-to-rte dfa-2) true))
          801))))


(defn check-extraction-cycle
  ([rt]
   (check-extraction-cycle rt (fn [expr msg] (assert expr msg))))
  ([rt-1 check]
   (let [extracted (dfa-to-rte (rte-to-dfa rt-1 true))
         rt-2 (extracted true)
         empty (canonicalize-pattern (template (:or (:and ~rt-2 (:not ~rt-1))
                                                    (:and (:not ~rt-2) ~rt-1))))
         empty-dfa (rte-to-dfa empty)]
     (when (not (xym/dfa-vacuous? empty-dfa))
       (dot/dfa-to-dot empty-dfa :title "empty" :view true))
     (check (xym/dfa-vacuous? empty-dfa)
            (cl-format nil "~&~
                            rt-1=~W~@
                            r2-2=~W~@
                            empty=~W~%" rt-1 rt-2 empty)))))

(deftest t-extract-rte
  (testing "extract-rte"
    (doseq [depth (range 4)
            _rep (range 1000) ]
      (check-extraction-cycle (gen-rte depth *test-types*) (fn [expr message]
                                                             (is expr message))))))
            
(deftest t-valid-rte
  (testing "valid-rte?"
    (is (rte/valid-rte? :sigma))
    (is (rte/valid-rte? :epsilon))
    (is (rte/valid-rte? :empty-set))
    (is (rte/valid-rte? Integer))
    (is (rte/valid-rte? 'Integer))

    ;; :not
    (is (rte/valid-rte? '(:not Integer)))
    (is (rte/valid-rte? (list :not Integer)))
    (is (rte/valid-rte? '(not Integer)))
    (is (rte/valid-rte? '(and Integer)))
    (is (rte/valid-rte? '(or Integer)))

    ;; :cat
    (is (rte/valid-rte? '(:cat)))

    ;; :and
    (is (rte/valid-rte? '(:and)))

    ;; :or
    (is (rte/valid-rte? '(:or)))
    (is (rte/valid-rte? '(:cat Integer)))
    (is (rte/valid-rte? '(:and Integer)))
    (is (rte/valid-rte? '(:or Integer)))
    (is (rte/valid-rte? '(:cat Integer String)))
    (is (rte/valid-rte? '(:and Integer String)))
    (is (rte/valid-rte? '(:or Integer String)))
    (is (rte/valid-rte? '(member 1 2 3)))
    (is (rte/valid-rte? '(= 1)))
    (is (rte/valid-rte? 'String))
    (is (rte/valid-rte? :sigma))
    (is (rte/valid-rte? '(:cat String :sigma)))
    (is (rte/valid-rte? '(:? String)))
    (is (rte/valid-rte? '(:+ String)))
    (is (rte/valid-rte? '(:* String)))
    (is (rte/valid-rte? '(:cat (:* Number) (:+ String) (:* Boolean))))

    (is (rte/valid-rte? '(:cat String String String)))
    (is (rte/valid-rte? '(:exp 3 Integer)))
    (is (rte/valid-rte? '(:permute Integer String)))
    (is (rte/valid-rte? '(:contains-none Integer String)))
    (is (rte/valid-rte? '(:contains-any Integer String)))
    (is (rte/valid-rte? '(:contains-every Integer String)))


    (is (rte/valid-rte? '(:cat (:contains-every Integer String)
                               (:exp 3 Integer)
                               (:? String))))

))

(defn -main []
  ;; To run one test (clojure.test/test-vars [#'rte-test/the-test])
  (clojure.test/run-tests 'rte-test))
