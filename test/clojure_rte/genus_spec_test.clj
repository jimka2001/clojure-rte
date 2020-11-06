;; Copyright (c) 2020 EPITA Research and Development Laboratory
;;Permission is hereby granted, free of charge, to any person
;;obtaining a copy of this software and associated documentation files
;;(the "Software"), to deal in the Software without restriction,
;;including without limitation the rights to use, copy, modify, merge,
;;publish, distribute, sublicense, and/or sell copies of the Software,
;;and to permit persons to whom the Software is furnished to do so,
;;subject to the following conditions: The above copyright notice and
;;this permission notice shall be included in all copies or
;;substantial portions of the Software.  THE SOFTWARE IS PROVIDED "AS
;;IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
;;NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
;;PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR
;;OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
;;OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
;;OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


(ns clojure-rte.genus-spec-test
  (:require [clojure-rte.rte-core]
            [clojure-rte.rte-construct :as rte]
            [clojure.pprint :refer [cl-format]]
            [clojure-rte.genus :as gns]
            [clojure.spec.alpha :as s]
            [clojure-rte.genus-spec :as gs]
            [clojure.test :as t]))

(defn -main []
  (clojure.test/run-tests 'clojure-rte.genus-spec-test))

(s/def ::test-spec-1 (s/* (s/alt :x  (s/cat :a neg? :b even?)  
                                 :y  (s/cat :c odd? :d pos?))))

(t/deftest t-spec-to-rte
  (t/testing "spec-to-rte"
    (let [rte '(rte
                (:*
                 (:or
                  (:cat (spec clojure.core/neg?) (spec clojure.core/even?))
                  (:cat (spec clojure.core/odd?) (spec clojure.core/pos?)))))]
      (t/is (= (gs/spec-to-rte (s/form (s/* (s/alt :x  (s/cat :a neg? :b even?)  
                                                   :y  (s/cat :c odd? :d pos?)))))
               rte) "test x43")
      
      (t/is (= (binding [*ns* (find-ns 'user)]
                 (gs/spec-to-rte (s/form (s/* (s/alt :x  (s/cat :a neg? :b even?)  
                                                     :y  (s/cat :c odd? :d pos?))))))
               rte) "test x43")


      (t/is (= (gs/spec-to-rte `(s/* (s/alt :x  (s/cat :a neg? :b even?)  
                                            :y  (s/cat :c odd? :d pos?))))
               rte) "test x48")

      (t/is (= (gs/spec-to-rte (s/form ::test-spec-1))
               rte) "test x51")
      )))

(t/deftest t-canonicalize-type
  (t/testing "spec canonicalize-type"
    (let [rte '(rte
                (:*
                 (:or
                  (:cat
                   (satisfies clojure.core/neg?)
                   (satisfies clojure.core/even?))
                  (:cat
                   (satisfies clojure.core/odd?)
                   (satisfies clojure.core/pos?)))))]

      (t/is (= (gns/canonicalize-type `(~'spec ~(s/* (s/alt :x  (s/cat :a neg? :b even?)  
                                                            :y  (s/cat :c odd? :d pos?)))))
               rte) "test x68")

      (t/is (= (gns/canonicalize-type '(spec ::test-spec-1))
               rte) "test x71")

      (t/is (= (gns/canonicalize-type '(spec (s/* (s/alt :x  (s/cat :a neg? :b even?)  
                                                         :y  (s/cat :c odd? :d pos?)))))
               '(rte
                 (:*
                  (:or
                   (:cat (satisfies neg?) (satisfies even?))
                   (:cat (satisfies odd?) (satisfies pos?))))))
            "test x80")
      )))

(t/deftest t-rte-match
  (t/testing "rte/match with rtes containing specs"
    (let [rte '(rte
                (:*
                 (:or
                  (:cat
                   (satisfies clojure.core/neg?)
                   (satisfies clojure.core/even?))
                  (:cat
                   (satisfies clojure.core/odd?)
                   (satisfies clojure.core/pos?)))))]

      (doseq [seq [[]
                   [-1 2]
                   [-1 2 3 1]
                   [-1 2 3 1 -3 1 -1 4]]]
        (t/is (= true (rte/match rte [seq])) (cl-format false "test x87: seq=~A pattern=~A" seq rte)))
      (t/is (= false (rte/match rte [-1 2   3 1   3 -1   -1 4])) "test x91")
      (t/is (= false (rte/match rte [[-1 2   3 1   3 -1   -1 4]])) "test x92")
      (doseq [rte2 [rte
                    `(~'spec ~(s/* (s/alt :x  (s/cat :a neg? :b even?)  
                                          :y  (s/cat :c odd? :d pos?))))
                    '(spec ::test-spec-1)
                    '(spec (s/* (s/alt :x  (s/cat :a neg? :b even?)  
                                       :y  (s/cat :c odd? :d pos?))))
                    ]
              :let [_ (t/is (= false (rte/match rte2 [seq])) "test 100")]
              seq [[]
                   [-1 2]
                   [-1 2 3 1]
                   [-1 2 3 1 -3 1 -1 4]]
              ]
        (t/is (= false (rte/match rte2 seq)) "test 105")
        (t/is (= true (rte/match rte2 [seq])) "test 106")
        ))))

;; cat - concatenation of predicates/patterns
;; alt - choice among alternative predicates/patterns
;; * - 0 or more of a predicate/pattern
;; + - 1 or more of a predicate/pattern
;; ? - 0 or 1 of a predicate/pattern
(t/deftest t-spec-sequences
  (t/testing "spec sequence operators"
    ()))

;; this example comes from https://clojure.org/guides/spec#_sequences
;; it matches any sequence of strings with even length, 0, 2, 4, ...
(s/def ::test-spec-2 (s/& (s/* string?)
                          #(even? (count %))))

(t/deftest t-rte-match-2
  (t/testing "rte/match with unsupported specs"
    ;; ::test-spec-2 uses the s/& operator which cannot be converted
    ;;   to an efficent rte.  the semantics should be maintained by the
    ;;   rte by leaving it as (spec ....)
    (let [rte '(:* (spec ::test-spec-2))]
      (t/is (= true (rte/match rte [])) "test 148")
      (t/is (= true (rte/match rte [[] [] []])) "test 149")
      (t/is (= true (rte/match rte [[]
                                    ["a" "b"]
                                    ["a" "b" "c" "d"]])) "test 150")
      (t/is (= false (rte/match rte [[]
                                    ["a" "b"]
                                    ["a" "b" "c"]])) "test 151"))))
      

(t/deftest t-expand-spec
  (t/testing "expanding unsupported form"
    (t/is (= (rte/expand '(spec ::test-spec-2) nil)
             '(spec ::test-spec-2)))))
