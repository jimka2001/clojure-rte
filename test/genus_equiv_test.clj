;; Copyright (c) 2021 EPITA Research and Development Laboratory
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

(ns genus-equiv-test
  (:require [rte-core]
            [genus :as gns]
            [util :refer [human-readable-current-time]]
            [genus-tester :refer [gen-type]]
            [backtick :refer [template]]
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
        (println [:testing ~string :starting (human-readable-current-time)]))
      (clojure.test/testing ~string ~@body)
      (when test-verbose
        (println [:finished  (human-readable-current-time)])))))

(deftest t-type-equivalent-random
  (testing "type-equivalent? random"
    (doseq [n (range 5)
            m (range 100)
            :let [t1 (gen-type n)
                  t2 (gen-type n)]]
      (is (= true (gns/type-equivalent? t1 t1 :dont-know))
          (cl-format nil "~@
                         t1=~W~@
                         failed to find type equivalent to itself" t1))
      (is (not= true (gns/type-equivalent? t1 (gns/create-not t1) :dont-know))
          (cl-format nil "~@
                         t1=~W~@
                         failed to find type NOT equivalent to its complement" t1))
      (is (not= false (gns/type-equivalent? (template (not (and ~t1 ~t2)))
                                            (template (or (not ~t1) (not ~t2)))
                                            :dont-know))
          (cl-format nil "~@
                         t1=~W~@
                         erroniously proved type not equal to is demorganization" t1)))))

(defn test-1 [_a] true)
(defn test-2 [_a] true)

(deftest t-type-equivalent-fixed
  (testing "type-equivalent? fixed"
    (let [t1 '(satisfies test-1)
          t2 '(satisfies test-2)]
      (doseq [[a b a<b b<a a=b] [;; A < B = None   B < A = None
                                 [t1 t2 :dont-know :dont-know :dont-know]

                                 ;;  A < B = None   B < A = False
                                 ['(satisfies odd?) '(member 2) :dont-know false false]

                                 ;;  A < B = None   B < A = True
                                 [t1 :empty-set :dont-know true :dont-know]

                                 ;; A < B = True   B < A = None
                                 [:empty-set t1 true :dont-know :dont-know]

                                 ;; A < B = True   B < A = False
                                 ['(member 1 2) '(member 1 2 3) true false false]

                                 ;; A < B = True   B < A = True
                                 ['(member 1 2 3) '(member 2 1 3) true true true]
                                 ['(member 1 2 3) (template (or (member 1 2) (member 3))) true true true]

                                 ;; A < B = False  B < A = None
                                 ['(member 2) '(satisfies odd?) false :dont-know false]
                                 ;; A < B = False   B < A = False
                                 ['(member 1 2 3) '(member 2 3 4) false false false]
                                 ;; A < B = False  B < A = True
                                 ['(member 1 2 3) '(member 2 3) false true false]]]
        (is (= (gns/subtype? a b :dont-know)
               a<b)
            (cl-format nil "a=~W~@
                              b=~W~@
                              expecting a < b = ~W, got ~W"
                       a b a<b (gns/subtype? a b :dont-know)))
        (is (= (gns/subtype? b a :dont-know)
               b<a)
            (cl-format nil "a=~W~@
                              b=~W~@
                              expecting b < a = ~W, got ~W"
                       a b b<a (gns/subtype? b a :dont-know)))
        (is (= (gns/type-equivalent? a b :dont-know)
               a=b)
            (cl-format nil "a=~W~@
                              b=~W~@
                              expecting a = b = ~W, got ~W"
                       a b a=b (gns/type-equivalent? a b :dont-know)))))))
