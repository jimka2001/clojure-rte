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

(ns genus-spec-test
  (:require [rte.core]
            [rte.construct :as rte]
            [clojure.pprint :refer [cl-format pprint]]
            [util.util :refer [forall human-readable-current-time]]
            [genus.genus :as gns]
            [clojure.spec.alpha :as s]
            [genus.genus-spec :as gs]
            [backtick :refer [template]]
            [clojure.test :as t]))

(s/def ::test-spec-1 (s/* (s/alt :1  (s/cat :3 neg? :4 pos-int?)  
                                 :2  (s/cat :5 pos? :6 neg-int?))))

(defmacro testing
  [string & body]
  (let [verbose false]
    `(rte/with-compile-env []
       (when ~verbose
         (println [:testing ~string :starting (human-readable-current-time)]))
       (clojure.test/testing ~string ~@body)
       (when ~verbose
         (println [:finished  (human-readable-current-time)]))
     )))

;;(pprint  (methods gns/valid-type?))

(t/deftest t-valid-rte
  (testing "valid type rte"
    (t/is (gns/valid-type? :sigma))
    (t/is (gns/valid-type? '(rte :sigma)))))
  




(t/deftest t-spec-to-rte-0
  (testing "spec-to-rte 0"
    (let [sp (s/form (s/* (s/alt :x  (s/cat :a neg? :b pos-int?)  
                                 :y  (s/cat :c pos? :d neg-int?))))]
      (t/is (gs/spec-to-rte sp) "test x0"))))

(t/deftest t-spec-to-rte
  (testing "spec-to-rte"
    (let [rte '(rte
                (:*
                 (:or
                  (:cat (spec clojure.core/neg?) (spec clojure.core/pos-int?))
                  (:cat (spec clojure.core/pos?) (spec clojure.core/neg-int?)))))]
      (t/is (= (gs/spec-to-rte (s/form (s/* (s/alt :x  (s/cat :a neg? :b pos-int?)  
                                                   :y  (s/cat :c pos? :d neg-int?)))))
               rte) "test x43")
      
      (t/is (= (binding [*ns* (find-ns 'user)]
                 (gs/spec-to-rte (s/form (s/* (s/alt :x  (s/cat :a neg? :b pos-int?)  
                                                     :y  (s/cat :c pos? :d neg-int?))))))
               rte) "test x43")


      (t/is (= (gs/spec-to-rte `(s/* (s/alt :x  (s/cat :a neg? :b pos-int?)
                                            :y  (s/cat :c pos? :d neg-int?))))
               rte) "test x48")

      (t/is (= (gs/spec-to-rte (s/form ::test-spec-1))
               rte) "test x51")

      )))

(t/deftest t-canonicalize-rte-type
  (testing "spec canonicalize rte type"
    (let [rte '(rte
                (:*
                 (:or
                  (:cat
                   (satisfies clojure.core/neg?)
                   (satisfies clojure.core/pos-int?))
                  (:cat
                   (satisfies clojure.core/pos?)
                   (satisfies clojure.core/neg-int?)))))]

      (t/is (= (gns/canonicalize-type (template (spec ~(s/* (s/alt :x  (s/cat :a neg? :b pos-int?)  
                                                                   :y  (s/cat :c pos? :d neg-int?))))))
               rte) "test x68")

      (t/is (= (gns/canonicalize-type '(spec ::test-spec-1))
               rte) "test x71")

      (t/is (= (gns/canonicalize-type '(spec (s/* (s/alt :x  (s/cat :a neg? :b pos-int?)
                                                         :y  (s/cat :c pos? :d neg-int?)))))
               '(rte
                 (:*
                  (:or
                   (:cat (satisfies neg?) (satisfies pos-int?))
                   (:cat (satisfies pos?) (satisfies neg-int?))))))
            "test x80")
      )))

(t/deftest t-rte-match-1
  (testing "1st rte/match with rtes containing specs"
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
      )))

(t/deftest t-rte-match-2
  (testing "2nd rte/match with rtes containing specs"
    (let [rte '(rte
                (:*
                 (:or
                  (:cat
                   (satisfies clojure.core/neg?)
                   (satisfies clojure.core/even?))
                  (:cat
                   (satisfies clojure.core/odd?)
                   (satisfies clojure.core/pos?)))))]

      (t/is (= false (rte/match rte [-1 2   3 1   3 -1   -1 4])) "test x91")
      (t/is (= false (rte/match rte [[-1 2   3 1   3 -1   -1 4]])) "test x92")
)))

(t/deftest t-rte-match-3
  (testing "3nd rte/match with rtes containing specs"
    (let [rte '(rte
                (:*
                 (:or
                  (:cat
                   (satisfies clojure.core/neg?)
                   (satisfies clojure.core/pos-int?))
                  (:cat
                   (satisfies clojure.core/pos?)
                   (satisfies clojure.core/neg-int?)))))]

      (doseq [rte2 [rte
                    (template (spec ~(s/* (s/alt :x  (s/cat :a neg? :b pos-int?)
                                                 :y  (s/cat :c pos? :d neg-int?)))))
                    '(spec ::test-spec-1)
                    '(spec (s/* (s/alt :x  (s/cat :a neg? :b pos-int?  )
                                       :y  (s/cat :c pos? :d neg-int?))))
                    ]
              seq [[]
                   [-1 2]
                   [-1 2 -3 1]
                   [-1 2 -3 1 3 -1 -1 4]]
              ]
        (t/is (= false (rte/match rte2 seq)) (cl-format false "test 105: seq=~A rte2=~A" seq rte2))
        (t/is (= true (rte/match rte2 [seq])) (cl-format false "test 106: seq=~A rte2=~A" seq rte2))
        ))))

;; cat - concatenation of predicates/patterns
;; alt - choice among alternative predicates/patterns
;; * - 0 or more of a predicate/pattern
;; + - 1 or more of a predicate/pattern
;; ? - 0 or 1 of a predicate/pattern
(t/deftest t-spec-sequences
  (testing "spec sequence operators"
    ()))

;; this example comes from https://clojure.org/guides/spec#_sequences
;; it matches any sequence of strings with even length, 0, 2, 4, ...
;; This cannot be converted to RTE because the s/& is not supported by RTE.
(s/def ::test-spec-2 (s/& (s/* string?)
                          #(even? (count %))))

(t/deftest t-rte-match-2b
  (testing "rte/match with unsupported specs"
    ;; ::test-spec-2 uses the s/& operator which cannot be converted
    ;;   to an efficent rte.  the semantics should be maintained by the
    ;;   rte by leaving it as (spec ....)
    (rte/with-compile-env []
      (let [rte '(:* (spec ::test-spec-2))
            rte-canonicalized (rte/canonicalize-pattern rte)]

        (t/is (not= '(:* (spec nil))
                    rte-canonicalized) "test 146")
        (t/is (= true (rte/match rte [])) "test 148")
        (t/is (= true (rte/match rte [[] [] []])) "test 149")
        (t/is (= true (rte/match rte [[]
                                      ["a" "b"]
                                      ["a" "b" "c" "d"]])) "test 150")
        (t/is (= false (rte/match rte [[]
                                       ["a" "b"]
                                       ["a" "b" "c"]])) "test 151")))))
      
(t/deftest t-expand-spec
  (testing "expanding unsupported form"
    (t/is (= (rte/expand '(spec ::test-spec-2) nil
                         false ; verbose=false
                         )
             '(spec ::test-spec-2)))))

(s/def ::test-spec-3 (s/or :1 string?
                           :2 (s/and int? #(pos-int? %))))
(s/def ::test-spec-4 (s/or :1 (s/and string? #(even? (count %)))
                           :2 (s/and int? #(pos-int? %))))

(t/deftest t-canonicalize-spec-type
  (testing "canonicalize spec non-sequence types"
    (doseq [t1 (template ((spec ~(s/or :1 int? :2 number?))
                          (spec ~(s/and int? number?))
                          (spec ~(s/or :1 int? :2 double?))
                          (spec ::test-spec-3)
                          (spec ~(s/or :1 int? :2 ::test-spec-3))
                          (spec ~(s/and int? ::test-spec-3))
                          (spec ~(s/or :1 (s/and int? neg-int?)
                                       :2 string?))
                          (spec ~(s/or :1 (s/and int? pos-int?)
                                       :2 string?))))
            :let [t2 (gns/canonicalize-type t1)]
            v1 [0 1 1.0 -1 -1.0 2 3 4 5 -2 -3 -4 -5
                "hello" "" "a" "ab" "abc" "abcd"]]
      (t/is (= (gns/typep v1 t1)
               (gns/typep v1 t2))
            (cl-format false "line 132: type-designator=~A and canonicalized=~A disagree (~A != ~A) on v1=~A"
                       t1 t2
                       (gns/typep v1 t1)
                       (gns/typep v1 t2)
                       v1)))))

(t/deftest t-canonicalize-spec-regex-type
  (testing "canonicalize spec sequence types"
    (doseq [t1 (template ((spec ~(s/* (s/or :1 int? :2 number?)))
                          (spec ~(s/+ (s/and int? number?)))
                          (spec ~(s/cat :1 (s/? int?)
                                        :2 string?))
                          (spec ~(s/alt :1 int?
                                        :2 (s/* string?)))
                          (spec ~(s/cat :3 (s/alt :1 int?
                                                  :2 (s/* string?))
                                        :4 int?))
                          (spec ~(s/+ (s/or :1 int?
                                            :2 string?)))
                          (spec ~(s/cat :1 (s/or :3 (s/* number?)
                                                 :4 boolean?)
                                        :2 (s/* string?)))
                          (spec ~(s/cat :1 (s/alt :3 (s/* number?)
                                                  :4 boolean?)
                                        :2 (s/* string?)))))
            :let [t2 (gns/canonicalize-type t1)]
            v1 [0 1 1.0 -1 -1.0 2 3 4 5 -2 -3 -4 -5
                "hello" "" "a" "ab" "abc" "abcd"
                [0 1.0 2 2.0]
                [1 2 3 -1 -2 -3]
                []
                [3]
                [3 3]
                [3 3 3 3]
                ["hello"]
                [3 "hello"]
                ["hello" 3]
                ["hello" 3 3 3]
                ["hello" 3 "hello" 3 3 "hello" 3]
                ["hello" 3 "hello" 3 "hello" "world" 3]
                [3 "hello" "world"]
                [3 "hello" "world" 3]
                ["hello" "world" 3]
                ["hello" "world" 3 3]
                ["hello" "world"]
                [true     "a" "b" "c"]
                [false     "a" "b" "c"]
                [[true]     "a" "b" "c"]
                [[false]     "a" "b" "c"]
                [()     "a" "b" "c"]
                [[]     "a" "b" "c"]
                [[1 2 3]     "a" "b" "c"]
                [1 2 3     "a" "b" "c"]
                [true true false     "a" "b" "c"]
                [[true true false]     "a" "b" "c"]
                ]]
      (t/is (= (gns/typep v1 t1)
               (gns/typep v1 t2))
            (cl-format false "line 132: type-designator=~A and canonicalized=~A disagree (~A != ~A) on v1=~A"
                       t1 t2
                       (gns/typep v1 t1)
                       (gns/typep v1 t2)
                       v1)))))

(t/deftest t-canonicalize-spec
  (testing "type vs pattern"
    (t/is (= (gns/canonicalize-type (template (spec ~(s/cat :1 number?
                                                            :2 (s/* string?)))))
             '(rte (:cat Number (:* String))))
          "line 254")

    ;; a type at the top level of an rte pattern means a sequence
    ;; for which every element matches the type.
    ;; so the rte (spec x) is not a sequence matching x
    ;; but rather a sequence of object, each of which matches (spec x)
    (t/is (= (rte/canonicalize-pattern (template (spec ~(s/cat :1 number?
                                                               :2 (s/* string?)))))
             '(rte (:cat Number (:* String))))
          "line 258")

    ;; a type at the top level of an rte pattern means a sequence
    ;; for which every element matches the type.
    ;; so the rte (spec x) is not a sequence matching x
    ;; but rather a sequence of object, each of which matches (spec x)
    (t/is (= (rte/canonicalize-pattern (template (spec ~(s/cat :1 (s/or :3 number?
                                                                        :4 boolean?)
                                                               :2 (s/* string?)))))
             '(rte (:cat (:or Boolean Number) (:* String))))
          "line 263")

    ;; match an object such as
    ;;   [true     "a" "b" "c"]
    ;;   [false     "a" "b" "c"]
    ;;   [()     "a" "b" "c"]
    ;;   [[]     "a" "b" "c"]
    ;;   [[1 2 3]     "a" "b" "c"]
    ;; not   [1 2 3     "a" "b" "c"]
    ;; not   [true true false     "a" "b" "c"]
    (t/is (= (gns/canonicalize-type (template (spec ~(s/cat :1 (s/or :3 (s/* number?)
                                                                     :4 boolean?)
                                                            :2 (s/* string?)))))
             '(rte (:cat (:or (rte (:* Number))
                              Boolean)
                         (:* String))))
          "line 305")

    (t/is (= (rte/canonicalize-pattern (template (spec ~(s/cat :1 (s/or :3 (s/* number?)
                                                                        :4 boolean?)
                                                               :2 (s/* string?)))))
             '(rte (:cat (:or (rte (:* Number))
                              Boolean)
                         (:* String))))
          "line 313")

    (t/is (= (rte/canonicalize-pattern (template (spec ~(s/cat :1 (s/alt :3 (s/* number?)
                                                                         :4 boolean?)
                                                               :2 (s/* string?)))))
             '(rte (:cat (:or (:* Number) Boolean) (:* String))))
          "line 319")

    (t/is (= (gns/canonicalize-type (template (spec ~(s/alt :1 number? :2 (s/* string?)))))
             '(rte (:or (:* String) Number)))
          "line 323")
    (t/is (= (rte/canonicalize-pattern (template (spec ~(s/alt :1 number? :2 (s/* string?)))))
             '(rte (:or (:* String) Number)))
          "line 326")))

(t/deftest t-canonicalize-spec-2
  (testing "canonicalize spec 2"
    (let [spec1 (s/alt :1 number? :2 (s/* string?))
          spec2 (s/or :1 number? :2 (s/* string?))
          t1 (template (spec ~spec1))
          t2 (template (spec ~spec2))]
      (doseq [q [[]
                 [3]
                 [3 3 3]
                 [[3] [3] [3]]
                 ["hello"]
                 ["hello" "hello"]
                 [[] [] []]
                 [["hello"]]
                 [["hello"] ["hello"] ["hello"]]
                 [["hello" "world"] ["hello" "world"] ["hello" "world"]]
                 [3 ["hello" "world"] 3 ["hello" "world"] 3 ["hello" "world"]]
                 ]]
        (t/is (forall [item q]
                      (= (gns/typep item t1)
                         (s/valid? spec1 item))) "line 278")
        (t/is (forall [item q]
                      (= (gns/typep item t2)
                         (s/valid? spec2 item))) "line 281")
        (t/is (= (gns/typep q t1)
                 (s/valid? spec1 q)) "line 283")
        (t/is (= (gns/typep q t2)
                 (s/valid? spec2 q)) "line 285")))))

(t/deftest t-canonicalize-spec-regex-type-2
  (testing "canonicalize spec sequence types"
    (let [rte1 (template (rte (:or Number (:* String))))
          rte2 (template (spec ~(s/alt :1 number? :2 (s/* string?))))]
      (doseq [rte [rte1 rte2]]
        (t/is (rte/match rte [[2]])
              (cl-format false "line 255: failed rte=~A" rte))
        (t/is (rte/match rte [[]])
              (cl-format false "line 256: failed rte=~A" rte))
        (t/is (rte/match rte [["hello"]])
              (cl-format false "line 257: failed rte=~A" rte))
        (t/is (rte/match rte [["world"]])
              (cl-format false "line 258: failed rte=~A" rte))
        (t/is (not (rte/match rte [[2 "world"]]))
              (cl-format false "line 259: failed rte=~A" rte))
        (t/is (not (rte/match rte [["world" 3]]))
              (cl-format false "line 260: failed rte=~A" rte))))

    (let [t1 (template (or Number (rte (:* String))))
          t2 (template (spec ~(s/or :1 number? :2 (s/* string?))))]
      (doseq [t [t1 t2]]
        (t/is (gns/typep 2 t)
              (cl-format false "line 265: failed t=~A" t))
        (t/is (gns/typep [] t)
              (cl-format false "line 266: failed t=~A" t))
        (t/is (gns/typep ["hello"] t)
              (cl-format false "line 267: failed t=~A" t))

        (t/is (gns/typep ["hello" "world" ] t)
              (cl-format false "line 268: failed t=~A" t))

        (t/is (not (gns/typep [2 "world"] t))
              (cl-format false "line 395: failed t=~A" t))
        (t/is (not (gns/typep [["hello" "world"]] t))
              (cl-format false "line 397: failed t=~A" t))
        (t/is (not (gns/typep [[2 "hello" "world"]] t))
              (cl-format false "line 399: failed t=~A" t))
        (t/is (not (gns/typep [["world" 3]] t))
              (cl-format false "line 401: failed t=~A" t))
        (t/is (not (gns/typep [3] t))
              (cl-format false "line 403: failed t=~A" t))
        ))))

(def email-regex #"^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,63}$")
(s/def ::email-type (s/and string? #(re-matches email-regex %)))
(t/deftest t-email-address
  (testing "email address"
    (t/is (s/valid? ::email-type "jimka.issy@gmail.com") :432)
    (t/is (not (s/valid? ::email-type "jimka.issy")) :433)

    (t/is (gns/typep "jimka.issy@gmail.com" (template (spec ~::email-type))) :435)
    (t/is (not (gns/typep "jimka.issy" (template (spec ~::email-type)))) :436)

    (t/is (gns/typep "jimka.issy@gmail.com" (gns/canonicalize-type (template (spec ~::email-type)))) :438)
    (t/is (not (gns/typep "jimka.issy" (gns/canonicalize-type (template (spec ~::email-type))))) :439)
    ))

(s/def ::acctid int?)
(s/def ::first-name string?)
(s/def ::last-name string?)
(s/def ::email ::email-type)

(s/def ::person (s/keys :req [::first-name ::last-name ::email]
                        :opt [::phone]))

(t/deftest t-person
  (testing "person example from spec guide. https://clojure.org/guides/spec#_sequences"
    (t/is (s/valid? ::person
                    {::first-name "Bugs"
                     ::last-name "Bunny"
                     ::email "bugs@example.com"}) "test 432")
    (t/is (gns/typep {::first-name "Bugs"
                      ::last-name "Bunny"
                      ::email "bugs@example.com"}
                     (template (spec ~::person))) "test 436")
     (t/is (gns/typep {::first-name "Bugs"
                      ::last-name "Bunny"
                      ::email "bugs@example.com"}
                      (gns/canonicalize-type (template (spec ~::person)))) "test 440")))

(defn -main []
  (clojure.test/run-tests 'genus-spec-test))
