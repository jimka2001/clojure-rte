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

(ns clojure-rte.core-test
  (:require [clojure.test :refer :all]
            [clojure.pprint :refer [cl-format]]
            [clojure-rte.cl-compat :refer [cl-cond]]
            [clojure-rte.util :refer [sort-operands remove-once call-with-collector visit-permutations]]
            [clojure-rte.type :refer [disjoint? type-intersection]]
            [clojure-rte.core :refer :all]
            [clojure-rte.rte-tester :refer :all]))

(deftest t-nullable
  (testing "nullable"
    (is (not (nullable :sigma)) "nullable sigma")
    (is (nullable :epsilon) 14)
    (is (not (nullable :empty-set)) 13)
    (is (nullable '(:and :epsilon :epsilon)) 12)
    (is (nullable '(:or :epsilon :empty-set)) 11)
    (is (nullable '(:cat :epsilon :epsilon)) 10)
    (is (not (nullable '(:cat :epsilon :empty-set))) 9)
    (is (not (nullable '(:cat :empty-set :epsilon))) 8)
    (is (nullable '(:cat :epsilon (:* :epsilon))) 7)
    (is (nullable '(:+ :epsilon)) 6)
    (is (not (nullable '(:cat :empty-set (:* :empty-set)))) 5)
    (is (not (nullable '(:cat :empty-set :epsilon))) 4)
    (is (not (nullable :empty-set)) 3)
    (is (not (nullable '(:+ :empty-set))) 2)
    (is (nullable '(:? :epsilon)) 1)))


(deftest t-first-types
  (derive ::Canine ::Animal)
  (derive ::Wolf ::Canine)
  (derive ::Fox ::Canine)
  (derive ::Dog ::Canine)
  (derive ::Feline ::Animal)
  (derive ::Cat ::Feline)
  (derive ::Lion ::Feline)
  (derive ::Cat-Lion ::Cat)
  (derive ::Cat-Lion ::Lion)

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
    ))

(deftest t-isa?
  (testing "disjoint"
   (derive ::Feline ::Animal)
   (derive ::Cat ::Feline)
   (derive ::Lion ::Feline)
   (is (isa? ::Lion ::Animal))
   (is (not (isa? ::Lion ::Cat)))))

(deftest t-sort-operands
  (testing "sort-operands"
    (is (= (sort-operands '(::Cat ::Lion))
           '(::Cat ::Lion)))
    (is (= (sort-operands '(::Lion ::Cat))
           '(::Cat ::Lion)))
    (is (= (sort-operands '((:not ::Cat) (:not ::Lion)))
           '((:not ::Cat) (:not ::Lion))))
    (is (= (sort-operands '((:not ::Lion) (:not ::Cat)))
           '((:not ::Cat) (:not ::Lion))))))
    
(deftest t-canonicalize-pattern-subtypes
  (derive ::Canine ::Animal)
  (derive ::Wolf ::Canine)
  (derive ::Fox ::Canine)
  (derive ::Dog ::Canine)
  (derive ::Feline ::Animal)
  (derive ::Cat ::Feline)
  (derive ::Lion ::Feline)
  (derive ::Cat-Lion ::Cat)
  (derive ::Cat-Lion ::Lion)

  (testing "canonicalize-pattern with subtypes"

    (is (= ::Animal (canonicalize-pattern '(:or ::Fox ::Animal))) "animal")
    (is (= :sigma (canonicalize-pattern '(:or ::Fox (:not ::Fox)))) "sigma")

    (is (= ::Fox (canonicalize-pattern '(:and ::Fox ::Animal))) "fox")
    (is (= :empty-set (canonicalize-pattern '(:and ::Fox (:not ::Fox)))) "empty-set 1")

    ;; intersection of disjoint types
    (is (= :empty-set (canonicalize-pattern '(:and ::Fox ::Lion))) "empty-set 2")
    ))


(deftest t-canonicalize-pattern
  (derive ::Canine ::Animal)
  (derive ::Wolf ::Canine)
  (derive ::Fox ::Canine)
  (derive ::Dog ::Canine)
  (derive ::Feline ::Animal)
  (derive ::Cat ::Feline)
  (derive ::Lion ::Feline)
  (derive ::Cat-Lion ::Cat)
  (derive ::Cat-Lion ::Lion)

  (derive ::Fish ::Animal)
  (derive ::Shark-a ::Fish)
  (derive ::Shark-b ::Fish)
  (derive ::Shark-c ::Fish)
  (derive ::Shark-d ::Fish)
  (derive ::Shark ::Shark-a)
  (derive ::Shark ::Shark-b)
  (derive ::Shark ::Shark-c)
  (derive ::Shark ::Shark-d)


  (testing "canonicalize-pattern"
    ;; syntax errors
    (is (thrown? clojure.lang.ExceptionInfo (canonicalize-pattern '(:*))))
    (is (thrown? clojure.lang.ExceptionInfo (canonicalize-pattern '(:not))))
    (is (thrown? clojure.lang.ExceptionInfo (canonicalize-pattern '(:?))))
    (is (thrown? clojure.lang.ExceptionInfo (canonicalize-pattern '(:+))))
    (is (thrown? clojure.lang.ExceptionInfo (canonicalize-pattern '(:rte))))

    ;; type
    (is (= ::Lion (canonicalize-pattern-once ::Lion)) "canonicalize :type")

    ;; :*
    (is (= (canonicalize-pattern-once '(:* (:* ::Fox)))
           '(:* ::Fox)) "a** -> a*")
    (is (= '(:* ::Lion) (canonicalize-pattern-once '(:* ::Lion))) "canonicalize :type *")
    (is (= :epsilon (canonicalize-pattern-once '(:* :epsilon))) ":epsilon* -> :epsilon")
    (is (= :epsilon (canonicalize-pattern-once '(:* :empty-set))) ":empty-set* -> :epsilon")
    (is (= '(:* :sigma) (canonicalize-pattern-once '(:* :sigma))) ":sigma* -> :sigma*")
    (is (= '(:* :sigma) (canonicalize-pattern-once '(:* (:* :sigma)))) ":sigma** -> :sigma*")
    (is (= '(:* :sigma) (canonicalize-pattern-once '(:* (:* (:* :sigma))))) ":sigma*** -> :sigma*")

    ;; :cat
    (is (= (canonicalize-pattern '(:cat)) :epsilon))
    (is (= ::Lion (canonicalize-pattern-once '(:cat ::Lion))) "unary :cat")
    (is (= '(:cat ::Lion ::Lion) (canonicalize-pattern-once '(:cat ::Lion ::Lion))) "binary :cat")
    (is (= '(:cat ::Lion ::Lion ::Lion) (canonicalize-pattern-once '(:cat ::Lion ::Lion ::Lion))) "3-ary :cat")
    (is (= ::Lion (canonicalize-pattern-once '(:cat (:cat ::Lion)))) "recursive cat")
    (is (= '(:cat ::Lion ::Lion) (canonicalize-pattern-once '(:cat (:cat ::Lion) (:cat ::Lion)))) "recursive cat")
    (is (= '(:cat ::Lion ::Lion) (canonicalize-pattern-once '(:cat ::Lion (:cat ::Lion)))) "recursive cat 2")
    (is (= '(:cat ::Lion ::Lion) (canonicalize-pattern-once '(:cat (:cat ::Lion) ::Lion))) "recursive cat 3")
    (is (= '(:cat ::Lion ::Lion ::Lion) (canonicalize-pattern-once '(:cat (:cat ::Lion) (:cat ::Lion) (:cat ::Lion)))) "recursive cat")
    (is (= ::Cat-Lion
           (canonicalize-pattern '(:cat :epsilon ::Cat-Lion)))
        "cat epsilon x")
    (is (= ::Cat-Lion
           (canonicalize-pattern '(:cat ::Cat-Lion :epsilon)))
        "cat x epsilon")
    (is (= :empty-set
           (canonicalize-pattern '(:cat :empty-set ::Cat-Lion)))
        "cat epsilon x")
    (is (= :empty-set
           (canonicalize-pattern '(:cat ::Cat-Lion :empty-set)))
        "cat x epsilon")
    (is (= (canonicalize-pattern '(:cat ::Fox (:* :sigma) (:* :sigma) ::Cat))
           '(:cat ::Fox (:* :sigma) ::Cat)) "cat sigma* sigma*")

    ;; :not
    (is (= :epsilon (canonicalize-pattern-once '(:not :sigma))) "not sigma")
    (is (= :empty-set (canonicalize-pattern-once '(:not (:* :sigma)))) "not sigma*")
    (is (= (canonicalize-pattern-once '(:+ :sigma))
           (canonicalize-pattern-once '(:not :epsilon))) "not epsilon")
    (is (= '(:* :sigma)
           (canonicalize-pattern-once '(:not :empty-set))) "not empty-set")
    (is (= '(:not ::Lion)
           (canonicalize-pattern-once '(:not ::Lion))) "not type")
    (is (= ::Lion
           (canonicalize-pattern-once '(:not (:not ::Lion)))) "not no type")
    (is (= '(:not ::Lion)
           (canonicalize-pattern-once '(:not (:not (:not ::Lion))))) "not not not type")
    ;; :not :and
    (is (= (canonicalize-pattern-once '(:not (:and ::Cat ::Lion)))
           (canonicalize-pattern-once '(:not (:and ::Lion ::Cat)))) "not and 1")
    (is (= '(:or (:not ::Cat) (:not ::Lion))
           (canonicalize-pattern-once '(:not (:and ::Lion ::Cat)))) "not and 2") ;;  (:not (:and A B)) --> (:or (:not A) (:not B))
    (is (= '(:or (:not ::Cat) (:not ::Lion))
           (canonicalize-pattern-once '(:not (:and ::Cat ::Lion)))) "not and 3")

    ;; :not :or
    (is (= (canonicalize-pattern '(:not (:or ::Cat ::Lion)))
           (canonicalize-pattern '(:not (:or ::Lion ::Cat)))) "not or 1")
    (is (= '(:and (:not ::Cat)
                  (:not ::Lion))
           (canonicalize-pattern '(:not (:or ::Lion ::Cat)))) "not or 2") ;;  (:not (:and A B)) --> (:or (:not A) (:not B))
    (is (= '(:and (:not ::Cat)
                  (:not ::Lion))
           (canonicalize-pattern-once '(:not (:or ::Cat ::Lion)))) "not or 3")

    (is (= '(:not ::Cat)
           (canonicalize-pattern '(:not (:or ::Cat ::Cat)))) "not or 4")

    ;; and
    (is (= (canonicalize-pattern '(:and)) :sigma))    
    (is (= ::Cat
           (canonicalize-pattern '(:and ::Cat ::Cat))) "and remove duplicate 1")
    (is (= '(:and ::Cat ::Lion)
           (canonicalize-pattern-once '(:and ::Cat ::Lion ::Cat ::Lion))) "and remove duplicate 2")

    (is (= '(:or (:and ::Shark-a ::Shark-b ::Shark-d)
                 (:and ::Shark-b ::Shark-c ::Shark-d))
    (canonicalize-pattern '(:and (:or ::Shark-a ::Shark-c) ::Shark-d ::Shark-b))) "and-distribute")

    (is (= :empty-set
           (canonicalize-pattern '(:and  ::Cat :empty-set ::Lion))) "and empty-set")
    (is (= '(:and ::Cat ::Lion)
           (canonicalize-pattern '(:and  ::Cat (:* :sigma) ::Lion))) "and sigma*")

    ;; or
    (is (= (canonicalize-pattern '(:or)) :empty-set))
    (is (= ::Cat
           (canonicalize-pattern '(:or ::Cat ::Cat))) "or remove duplicate 1")
    (is (= '(:or ::Cat ::Lion)
           (canonicalize-pattern '(:or ::Lion ::Cat ::Lion ::Cat ::Lion))) "or remove duplicate 2")
    (is (= '(:or  ::Cat ::Lion)
           (canonicalize-pattern '(:or  ::Cat :empty-set ::Lion))) "or empty-set")
    (is (= '(:* :sigma)
           (canonicalize-pattern '(:or  ::Cat (:* :sigma) ::Lion))) "or sigma*")

    ;; permute
    (is (= (canonicalize-pattern '(:permute)) :epsilon) "permute 0 arg")

    (is (= (canonicalize-pattern '(:permute ::Lion))
           ::Lion) "permute 1 arg")
    (is (= (canonicalize-pattern '(:permute ::Lion ::Cat))
           (canonicalize-pattern '(:or (:cat ::Lion ::Cat)
                                       (:cat ::Cat ::Lion)))) "permute 2 args")
    (is (= (canonicalize-pattern '(:permute ::Lion ::Cat ::Fox))
           (canonicalize-pattern '(:or (:cat ::Lion ::Cat ::Fox)
                                       (:cat ::Lion ::Fox ::Cat)
                                       (:cat ::Cat ::Lion ::Fox)
                                       (:cat ::Cat ::Fox ::Lion)
                                       (:cat ::Fox ::Cat ::Lion)
                                       (:cat ::Fox ::Lion ::Cat)))) "permute 3 args")
    
    ))

(deftest t-derivative
  (derive ::Canine ::Animal)
  (derive ::Wolf ::Canine)
  (derive ::Fox ::Canine)
  (derive ::Dog ::Canine)
  (derive ::Feline ::Animal)
  (derive ::Cat ::Feline)
  (derive ::Lion ::Feline)
  (derive ::Cat-Lion ::Cat)
  (derive ::Cat-Lion ::Lion)
  (testing "derivative"
    (is (= (derivative :empty-set ::Lion)
           :empty-set) "derivative empty-set w.r.t A")

    ;; :sigma
    (is (= (derivative :sigma :sigma)
           :epsilon) "derivative sigma wrt sigma")
    (is (= (derivative :sigma :epsilon)
           :sigma) "derivative sigma wrt epsilon")
    (is (= :epsilon
           (derivative :sigma ::Lion)) "derivative sigma wrt A")

    ;; :epsilon
    (is (= (derivative :epsilon :epsilon)
           :epsilon))
    (is (= (derivative :epsilon ::Lion)
           :empty-set))
    (is (= (derivative :epsilon :empty-set)
           :empty-set))
    (is (= (derivative :epsilon :sigma)
           :empty-set))

    ;; :empty-set
    (is (= (derivative :empty-set :epsilon)
           :empty-set))
    (is (= (derivative :empty-set ::Lion)
           :empty-set))
    (is (= (derivative :empty-set :empty-set)
           :empty-set))
    (is (= (derivative :empty-set :sigma)
           :empty-set))

    ;; type
    (is (= (derivative ::Lion ::Lion)
           :epsilon))
    (is (= (derivative ::Fox ::Wolf)
           :empty-set) "derivative disjoint types")



    ;; or
    
    (is (= (derivative '(:or ::Fox ::Lion) ::Fox)
           :epsilon))

    ;; cat
    (is (= (derivative '(:cat (:or ::Fox ::Lion) ::Cat-Lion) ::Fox)
           ::Cat-Lion) "derivative cat with reduction")

    (is (= (derivative '(:cat ::Fox ::Fox ::Fox) ::Fox)
           '(:cat ::Fox ::Fox)))

    (is (= (derivative '(:cat (:or ::Lion ::Fox) ::Fox ::Fox) ::Fox)
           '(:cat ::Fox ::Fox)))
    (is (= (derivative '(:cat (:or ::Lion ::Fox) ::Fox ::Fox) ::Lion)
           '(:cat ::Fox ::Fox)))


    ))
    
(deftest t-disjoint?
  (derive ::Canine ::Animal)
  (derive ::Wolf ::Canine)
  (derive ::Fox ::Canine)
  (derive ::Dog ::Canine)
  (derive ::Feline ::Animal)
  (derive ::Cat ::Feline)
  (derive ::Lion ::Feline)
  (derive ::Cat-Lion ::Cat)
  (derive ::Cat-Lion ::Lion)
  (testing "disjoint?"
    (is (not (disjoint? ::Fox ::Animal)))
    (is (not (disjoint? ::Cat ::Lion)))
    (is (disjoint? ::Wolf ::Fox))
    (is (= (set (type-intersection ::Cat ::Lion))
           #{::Cat-Lion}))))

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

(deftest t-call-with-collector
  (testing "call-with-collector"
    (is (= (call-with-collector (fn [collect]
                                  (collect 1)
                                  (collect 3)
                                  (collect 2)))
           '(2 3 1)))
    (is (= (call-with-collector (fn [collect]
                                  ))
           ()))))

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
    
(deftest t-rte-to-dfa
  (derive ::Canine ::Animal)
  (derive ::Wolf ::Canine)
  (derive ::Fox ::Canine)
  (derive ::Dog ::Canine)
  (derive ::Feline ::Animal)
  (derive ::Cat ::Feline)
  (derive ::Lion ::Feline)
  (derive ::Cat-Lion ::Cat)
  (derive ::Cat-Lion ::Lion)

  (testing "rte-to-dfa"
    (is (rte-to-dfa '(:cat :epsilon (:+ (:* :epsilon)) :sigma)) "dfa 1")
    ;; (is (thrown? clojure.lang.ExceptionInfo
    ;;              (rte-to-dfa '(:permute ::Wolf (:? (:+ :empty-set)) (:+ (:* (:and)))))) "dfa 2")
    ))


(deftest t-cl-cond
  (testing "cl-cond"
    (let [a 100 b 200]
      (is (= 42 (cl-cond
                 (a 42))) "cond 1")
      (is (= 42 (cl-cond
                 ((= a 1) 41)
                 ((= a 100) 42))) "cond 2")
      (is (not (cl-cond
                 ((= a 1) 41)
                 ((= a 200) 42))) "cond 2b")
      (is (= 100 (cl-cond
                  ((= a 1) 41)
                  (a)
                  (true -1))) "cond 3")

      (is (= -2 (cl-cond
                  ((= a 1) 41)
                  (true -1 -2))) "cond 4")
      )))

(deftest t-rte-match
  (testing "rte-match"
    (let [rte (rte-compile '(:* (:cat clojure.lang.Keyword java.lang.Long)))]
      (is (rte-execute rte '(:x 1 :y 2 :z 42)))
      (is (rte-execute rte '()))
      (is (not (rte-execute rte '(x 1 y 2 z 42)))))
    (let [rte (rte-compile '(:* (:cat clojure.lang.Keyword java.lang.Long)))]
      (is (rte-execute rte '(:x 1 :y 2 :z 42)))
      (is (rte-execute rte '()))
      (is (not (rte-execute rte '(x 1 y 2 z 42)))))

    (is (rte-match '(:cat (:* integer?) (:? String))
                   '( 1 2 3 4 "hello")))
    (is (not (rte-match '(:cat (:* integer?) (:? String))
                        '( 1 2 3 4 "hello" "world"))))


    ))

(deftest t-syntax
  (testing "syntax"
    (is (thrown? clojure.lang.ExceptionInfo (rte-compile '(:rte :epsilon :epsilon))))
    (is (thrown? clojure.lang.ExceptionInfo (rte-compile '(:* :epsilon :epsilon))))
    (is (thrown? clojure.lang.ExceptionInfo (rte-compile '(:? :epsilon :epsilon))))
    (is (thrown? clojure.lang.ExceptionInfo (rte-compile '(:+ :epsilon :epsilon))))))

(deftest t-types
  (testing "types"
    (is (rte-match '(:* int?) [ 1 2 3]))
    (is (rte-match '(:* number?) [ 1 2.0 1/3]))
    (is (rte-match '(:* symbol?)  '(a b c)))
    (is (rte-match '(:* keyword?)  '(:a :b :c)))
    (is (not (rte-match '(:* symbol?)  '(a :b c))))
    (is (rte-match '(:* string?)  '("hello" "world")))
    (is (rte-match '(:* rational?) [ 1 2 1/3]))
    (is (rte-match '(:* float?) [ 1.0 2.0 3.0]))
))

(deftest t-not
  (testing "patterns with :not"
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
    ))

(deftest t-mdtd
  (testing "mdtd"
    (is (= (set (mdtd #{:sigma java.lang.Exception clojure.lang.ExceptionInfo}))
           #{`(~'and :sigma (~'not ~java.lang.Exception))
             `(~'and ~java.lang.Exception (~'not ~clojure.lang.ExceptionInfo))
             clojure.lang.ExceptionInfo}))))

(deftest t-exp
  (testing "exp"
    (map (fn [n] 
           (let [data (map (constantly 12) (range n))
                 pattern `(:cat (:exp (~n (:? Long))) (:exp (~n Long)))
                 rte (rte-compile pattern)]

             (is (rte-execute rte data) (format "n=%s" n))))
         (range 10))))
