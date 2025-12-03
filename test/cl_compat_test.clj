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

(ns cl-compat-test
  (:require [rte.core]
            [util.cl-compat :as cl]
            [util.util :refer [call-with-collector human-readable-current-time]]
            [clojure.test :refer [deftest is]]))

(defn -main []
  (clojure.test/run-tests 'cl-compat-test))

(def test-verbose false)

(defmacro testing
  [string & body]
  `(do (when test-verbose
         (println [:testing ~string :starting (human-readable-current-time)]))
       (clojure.test/testing ~string ~@body)
       (when test-verbose
         (println [:finished  (human-readable-current-time)]))))


(deftest t-cl-prog1
  (testing "cl-prog1"
    (is (= 1 (cl/prog1 1 2)))
    (is (= 1 (cl/prog1 1 2 3)))
    (is (= 1 (cl/prog1 1 2 3 4)))
    (is (= '(4 3 2 1)
           (call-with-collector (fn [collect]
                                  (cl/prog1 (collect 1)
                                            (collect 2)
                                            (collect 3)
                                            (collect 4))))))))

(deftest t-cl-prog2
  (testing "cl-prog2"
    (is (= 2 (cl/prog2 1 2)))
    (is (= 2 (cl/prog2 1 2 3)))
    (is (= 2 (cl/prog2 1 2 3 4)))
    (is (= '(4 3 2 1)
           (call-with-collector (fn [collect]
                                  (cl/prog2 (collect 1)
                                            (collect 2)
                                            (collect 3)
                                            (collect 4))))))))

(deftest t-cl-progn
  (testing "cl-progn"
    (is (= 1 (cl/progn 1)))
    (is (= 2 (cl/progn 1 2)))
    (is (= 3 (cl/progn 1 2 3)))
    (is (= 4 (cl/progn 1 2 3 4)))
    (is (= '(4 3 2 1)
           (call-with-collector (fn [collect]
                                  (cl/progn (collect 1)
                                            (collect 2)
                                            (collect 3)
                                            (collect 4))))))))
(deftest t-cl-cond
  (testing "cl-cond"
    (let [a 100]
      (is (= 42 (cl/cl-cond
                 (a 42))) "cond 1")
      (is (= 42 (cl/cl-cond
                 ((= a 1) 41)
                 ((= a 100) 42))) "cond 2")
      (is (not (cl/cl-cond
                 ((= a 1) 41)
                 ((= a 200) 42))) "cond 2b")
      (is (= 100 (cl/cl-cond
                  ((= a 1) 41)
                  (a)
                  (true -1))) "cond 3")

      (is (= -2 (cl/cl-cond
                  ((= a 1) 41)
                  (true -1 -2))) "cond 4")

      (is (= '(3 2 1) (call-with-collector (fn [collect]
                                             (cl/cl-cond
                                              ((= a 1) 41)
                                              (true (collect 1) (collect 2) (collect 3)))))) "cond 5")
      (is (= nil (cl/cl-cond)) "cond nil")
      (is (= 12 (cl/cl-cond (true 12)
                         (nil))) "cond nil termination")
      )))

(deftest t-call-with-escape
  (testing "call-with-escape"
    (is (= 42 (cl/call-with-escape (fn [ret]
                                  (ret 42)))))
    (is (= 43 (cl/call-with-escape (fn [_ret]
                                  43))))
    (is (= 44 (cl/call-with-escape (fn [ret]
                                  (ret 44)
                                  45))))
    (is (= 47 (cl/call-with-escape (fn [ret1]
                                  (cl/call-with-escape (fn [ret2]
                                                         (ret2 46)))
                                  (ret1 47)))))
    (is (= 48 (cl/call-with-escape (fn [ret1]
                                     (cl/call-with-escape (fn [_ret2]
                                                         (ret1 48)))
                                     (ret1 49)))))))

(deftest t-with-escape
  (testing "with-escape"
    (is (= 42 (cl/with-escape ret
                (ret 42))))
    (is (= 43 (cl/with-escape _ret
                43)))
    (is (= 44 (cl/with-escape ret
                (ret 44)
                45)))
    (is (= 47 (cl/with-escape ret1
                (cl/with-escape ret2
                  (ret2 46))
                (ret1 47))))
    (is (= 48 (cl/with-escape ret1
                (cl/with-escape _ret2
                  (ret1 48))
                (ret1 49))))))

(deftest t-ldiff
  (testing "ldiff"
    (is (= (let [L '(1 2 3 4)]
             (cl/ldiff L (rest (rest L))))
           '(1 2)))
    (is (= (cl/ldiff '(1 2 3 4) '(3 4))
           '(1 2)))))
