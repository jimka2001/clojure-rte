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

(ns clojure-rte.genus-spec-test
  (:require [clojure-rte.rte-core]
            [clojure-rte.rte-construct :as rte]
            [clojure.pprint :refer [cl-format]]
            [clojure-rte.genus :as gns]
            [clojure.spec.alpha :as s]
            [clojure-rte.genus-spec :as gs]
            [backtick :refer [template]]
            [clojure.test :as t]))

(defn -main []
  (clojure.test/run-tests 'clojure-rte.genus-spec-test))

(s/def ::test-spec-1 (s/* (s/alt :1  (s/cat :3 neg? :4 even?)  
                                 :2  (s/cat :5 odd? :6 pos?))))

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

(t/deftest t-canonicalize-rte-type
  (t/testing "spec canonicalize rte type"
    (let [rte '(rte
                (:*
                 (:or
                  (:cat
                   (satisfies clojure.core/neg?)
                   (satisfies clojure.core/even?))
                  (:cat
                   (satisfies clojure.core/odd?)
                   (satisfies clojure.core/pos?)))))]

      (t/is (= (gns/canonicalize-type (template (spec ~(s/* (s/alt :x  (s/cat :a neg? :b even?)  
                                                                   :y  (s/cat :c odd? :d pos?))))))
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
                    (template (spec ~(s/* (s/alt :x  (s/cat :a neg? :b even?)  
                                                 :y  (s/cat :c odd? :d pos?)))))
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
    (t/is (= (rte/expand '(spec ::test-spec-2) nil
                         false ; verbose=false
                         )
             '(spec ::test-spec-2)))))

(s/def ::test-spec-3 (s/or :1 string?
                           :2 (s/and int? #(even? %))))
(s/def ::test-spec-4 (s/or :1 (s/and string? #(even? (count %)))
                           :2 (s/and int? #(even? %))))

(t/deftest t-canonicalize-type
  (t/testing "canonicalize spec non-sequence types"
    (doseq [t1 (template ((spec ~(s/or :1 int? :2 number?))
                          (spec ~(s/and int? number?))
                          (spec ~(s/or :1 int? :2 double?))
                          (spec ::test-spec-3)
                          (spec ~(s/or :1 int? :2 ::test-spec-3))
                          (spec ~(s/and int? ::test-spec-3))
                          (spec ~(s/or :1 (s/and int? odd?)
                                       :2 string?))
                          (spec ~(s/or :1 (s/and int? odd?)
                                       :2 string?))))
            :let [t2 (gns/canonicalize-type t1)]
            v1 [0 1 1.0 -1 -1.0 2 3 4 5 -2 -3 -4 -5
                "hello" "" "a" "ab" "abc" "abcd"]]
      (println [:testing :t1 t1 :v1 v1 :t2 t2])
      (t/is (= (gns/typep v1 t1)
               (gns/typep v1 t2))
            (cl-format false "line 132: type-designator=~A and canonicalized=~A disagree (~A != ~A) on v1=~A"
                       t1 t2
                       (gns/typep v1 t1)
                       (gns/typep v1 t2)
                       v1)))))

