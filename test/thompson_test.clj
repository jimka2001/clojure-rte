;; Copyright (©) 2022 EPITA Research and Development Laboratory
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

(ns thompson-test
  (:require [thompson :as tom]
            [genus :as gns]
            [util :refer [human-readable-current-time]]
            [rte-core]
            [rte-construct :as rte]
            [xymbolyco :as xym]
            [rte-tester :as test]
            [rte-randomize-syntax :refer [gen-rte]]
            [clojure.test :refer [deftest is]]))


(defn -main []
  (clojure.test/run-tests 'thompson-test))

(def testing-verbose false)

(defmacro testing
  [string & body]
  `(gns/call-with-genus-env
    (fn []
      (when testing-verbose
        (println [:testing ~string :starting (human-readable-current-time)]))
      (clojure.test/testing ~string ~@body)
      (when testing-verbose
        (println [:finished  (human-readable-current-time)])))))

;; default value of num_random_tests is 1000, but you can temporarily edit this file
;;   and set it to a smaller number for a quicker run of the tests.
(def num_random_tests 1000)

(def Dfa xymbolyco.Dfa)

(deftest t-transitions
  (testing "transitions"
    (is (tom/construct-transitions :empty-set))
    (is (tom/construct-transitions :epsilon))
    (is (tom/construct-transitions :sigma))
    (is (tom/construct-transitions 'Double))
    (is (tom/construct-transitions '(:* Double)))
    (is (tom/construct-transitions '(:cat)))
    (is (tom/construct-transitions '(:cat Double)))
    (is (tom/construct-transitions '(:cat Double String)))
    (is (tom/construct-transitions '(:cat Double String Double)))
    (is (tom/construct-transitions '(:or)))
    (is (tom/construct-transitions '(:or Double)))
    (is (tom/construct-transitions '(:or Double String)))
    (is (tom/construct-transitions '(:or Double String Double)))
    (is (tom/construct-transitions '(:and)))
    (is (tom/construct-transitions '(:and Double)))
    (is (tom/construct-transitions '(:and Double String)))
    (is (tom/construct-transitions '(:and Double String Double)))
    (is (tom/construct-transitions '(:not Double)))))


(deftest t-epsilon
  (testing "epsilon"
    (let [dfa (tom/construct-thompson-dfa :epsilon 42)]
      (is (instance? Dfa dfa))
      (is dfa)
      (is (= 42 (rte/match dfa [])))
      (is (= false (rte/match dfa [1 2 3]))))))

(deftest t-sigma
  (testing "sigma"
    (let [dfa (tom/construct-thompson-dfa :sigma 42)]
      (is (instance? Dfa dfa))
      (is (= 42 (rte/match dfa [1])))
      (is (= 42 (rte/match dfa ["hello"])))
      (is (= false (rte/match dfa [1 2 3]))))))

(deftest t-SigmaStar
  (testing "sigma star"
    (let [dfa (tom/construct-thompson-dfa '(:* :sigma) 42)]
      (is (instance? Dfa dfa))
      (is (= true (xym/dfa-inhabited? dfa)))
      (is (= 42 (rte/match dfa [1])))
      (is (= 42 (rte/match dfa ["hello"])))
      (is (= 42 (rte/match dfa [1 2 3]))))))

(deftest t-EmptySet
  (testing ":empty-set"
    (let [dfa (tom/construct-thompson-dfa :empty-set, 42)]
      (is (instance? Dfa dfa))
      (is (= false (xym/dfa-inhabited? dfa))))))

(deftest t-EmptySetStar
  (testing "empty-set star"
    (let [dfa (tom/construct-thompson-dfa '(:* :empty-set) 42)]
      (is (instance? Dfa dfa))
      (is (= 42 (rte/match dfa [])))
      (is (= false (rte/match dfa [1  2  3]))))))

(deftest t-Singleton
  (testing "singleton"
    (let [dfa (tom/construct-thompson-dfa 'Double  42)]
      (is (instance? Dfa dfa))
      (is (= 42 (rte/match dfa [1.0])))
      (is (= false (rte/match dfa [1.0 2.0  3.0]))))))

(deftest t-Star
  (testing "star"
    (let [dfa (tom/construct-thompson-dfa '(:* Long) 42)]
      (is (instance? Dfa dfa))
      (is (= 42 (rte/match dfa [1])))
      (is (= 42 (rte/match dfa [1  2  3])))
      (is (= false (rte/match dfa [1  2.0  3]))))))

(deftest t-Cat
  (testing "cat"
    (let [dfa (tom/construct-thompson-dfa '(:cat Long String)
                                          42)]
      (is (instance? Dfa dfa))
      (is (= 42 (rte/match dfa [1  "hello"])))
      (is (= false (rte/match dfa [1 1  "hello"])))
      (is (= false (rte/match dfa [])))
      (is (= false (rte/match dfa [1  "hello"  "hello"])))
      (is (= false (rte/match dfa [1  2.0  3]))))))

(deftest t-Or
  (testing "or"
    (let [dfa (tom/construct-thompson-dfa '(:or Long String) 42)]
      (is (instance? Dfa dfa))
      (is (= 42 (rte/match dfa [1])))
      (is (= 42 (rte/match dfa ["hello"])))
      (is (= false (rte/match dfa [])))
      (is (= false (rte/match dfa [1  "hello"  "hello"])))
      (is (= false (rte/match dfa [1  2.0  3]))))))

(deftest t-Not
  (testing "not"
    (let [dfa (tom/construct-thompson-dfa '(:not (:or Long String))
                                          42)]
      (is (instance? Dfa dfa))
      (is (= false (rte/match dfa [1])))
      (is (= false (rte/match dfa ["hello"])))
      (is (= 42 (rte/match dfa [])))
      (is (= 42 (rte/match dfa [1  "hello"  "hello"])))
      (is (= 42 (rte/match dfa [1  2.0  3]))))))

(deftest t-And
  (testing "and"
    ;; begins with int and ends with str
    (let [dfa (tom/construct-thompson-dfa '(:and (:cat Long (:* :sigma))
                                                 (:cat (:* :sigma) String))
                                          42)]
      (is (instance? Dfa dfa))
      (is (= 42 (rte/match dfa [1  "hello"  "hello"])))
      (is (= 42 (rte/match dfa [1  2.2  2.2  "hello"  "hello"])))
      (is (= false (rte/match dfa [1  2.2  2.2])))
      (is (= false (rte/match dfa [2.2  2.2  "hello"  "hello"])))
      (is (= false (rte/match dfa []))))))

(deftest t-randomCreate
  (testing "random create"
    (doseq [depth (range 4)
            _r (range num_random_tests)
            :let [pattern (gen-rte depth)
                  dfa-thompson (tom/construct-thompson-dfa pattern  42)
                  dfa-brzozowski (rte/compile pattern 42)]]
      ;; equivalent might return :dont-know or true  but need to fail if returns false
      (is (not= false (xym/dfa-equivalent? dfa-thompson dfa-brzozowski))))))

(deftest t-accessible
  (testing "accessible"
    (let [[ini  outs  transitions] (tom/accessible 0 
                                                   [97  98  99]
                                                   [[0  :sigma  1]
                                                    [1  :sigma  97]
                                                    [1  :sigma  98] 
                                                    [2  :sigma  99]])]
      (is (= ini 0))
      (is (= (set outs) #{97  98}))
      (is (= (set transitions)  #{[0  :sigma  1]
                                  [1  :sigma  97]
                                  [1  :sigma  98]})))))

(deftest t-coaccessible
  (testing "coaccessible"
    (let [[ini  outs  transitions] (tom/coaccessible 0 
                                                   [97  98]
                                                   [[0  :sigma  1]
                                                    [0  :sigma  3]
                                                    [1  :sigma  97]
                                                    [1  :sigma  98] 
                                                    [2  :sigma  99]])]
      (is (= ini 0))
      (is (= (set outs) #{97 98}))
      (is (= (set transitions)  #{[0  :sigma  1]
                                  [1  :sigma  97]
                                  [1  :sigma  98]
                                  })))))

(deftest t-simulate
  (testing "simulate"
    (let [ini 0
          outs [1  2]
          transitions  '((0  :sigma  1)
                         (1  Long  2)
                         (1  (or String Long)  3)
                         (3  :sigma  0))]
      (is (= 42
             (tom/simulate [1  2]  42 
                           ini  outs  transitions)))
      (is (= false
             (tom/simulate [1  2  3]  42 
                           ini  outs  transitions)))
      (is (= 42
             (tom/simulate [1  2  3  4]  42 
                           ini  outs  transitions))))))
