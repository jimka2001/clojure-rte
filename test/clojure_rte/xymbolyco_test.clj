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

(ns clojure-rte.xymbolyco-test
  (:refer-clojure :exclude [complement])
  (:require [clojure-rte.rte-core ]
            [clojure-rte.rte-construct :as rte :refer [rte-to-dfa]]
            [clojure-rte.xymbolyco :refer [find-eqv-class split-eqv-class
                                           states-as-seq find-incomplete-states
                                           extend-with-sink-state synchronized-product synchronized-union
                                           cross-intersection
                                           trim complete minimize]]
            [clojure-rte.bdd :as bdd]
            [clojure.pprint :refer [cl-format]]
            [clojure-rte.util :refer [member]]
            [clojure.test :refer [deftest is] :exclude [testing]]))

(defn -main []
  (clojure.test/run-tests 'clojure-rte.xymbolyco-test))

(defmacro testing
  [string & body]
  `(do (println [:testing ~string :starting (java.util.Date.)])
       (clojure.test/testing ~string ~@body)
       (println [:finished  (java.util.Date.)])
       ))


(deftest t-split-eqv-class
  (testing "split-eqv-class"
    (is (= (split-eqv-class #{1 2 3 4 5 6 7} even?)
           #{#{1 3 5 7}
             #{2 4 6}}))
    (is (= (split-eqv-class #{1 2 3 4 5 6 7 8 9} #(mod % 3))
           #{#{2 5 8}
             #{1 4 7}
             #{3 6 9}}))))

(deftest t-find-eqv-class
  (testing "find-eqv-class"
    (is (= (find-eqv-class [#{1 2 3} #{4 5 6} #{7 8 9}]
                           7)
           #{7 8 9}))
    (is (= (find-eqv-class [#{1 2 3} #{4 5 6} #{7 8 9}]
                           8)
           #{7 8 9}))
    (is (= (find-eqv-class [#{1 2 3} #{4 5 6} #{7 8 9}]
                           2)
           #{1 2 3}))
    (is (= (find-eqv-class #{#{1 2 3} #{4 5 6} #{7 8 9}}
                           2)
           #{1 2 3}))))

(deftest t-minimize
  (testing "minimize"
    ;; first example, a dfa already minimum
    (let [dfa1 (rte-to-dfa '(:or (:* Number)
                                 (:cat String Number)
                                 (:* Double)))
          dfa2 (minimize dfa1)]
      (is (= 6 (count (states-as-seq dfa1))))
      (is (= 6 (count (states-as-seq dfa2)))))))

(deftest t-minimize-runs
  (testing "that minimize runs"
    (doseq [rte ['(:* Long)
                 '(:or (rte (:* Number)) 
                                (rte (:cat Double Number))
                                (rte (:* Double)))
                 '(:or (rte (:* Number)) 
                                (rte (:cat String Number))
                                (rte (:* Double)))]]
      (rte-to-dfa rte)
      (minimize (rte-to-dfa rte)))))

(deftest t-trim-runs
  (testing "that trim runs"
    (doseq [rte ['(:* Long)
                 '(:or (rte (:* Number)) 
                                (rte (:cat Double Number))
                                (rte (:* Double)))
                  '(:or (rte (:* Number)) 
                                (rte (:cat String Number))
                                (rte (:* Double)))]]
      (rte-to-dfa rte)
      (trim (rte-to-dfa rte))
      (trim       (minimize (rte-to-dfa rte))))))

(deftest t-rte-match-min
  (testing "same results from matching minimized"
    (let [rte '(:* Long)
          dfa (rte-to-dfa rte)
          dfa-min (minimize dfa)
          seq [4.0]]
      (is (= (rte/match dfa seq)
             (rte/match dfa-min seq))))))

(def test-seqs '([]
                 [1]
                 [3]
                 [1 2 3 4]
                 [1 2 3]
                 [2 3 4]
                 [1 2 3.0 4.0]
                 [2 3.0 4.0]

                 
                 ["hello" "world" 1 "hello" "there" "world" 2]
                 ["hello" "world" 1 "hello" "world" 2]
                 ["hello" "world" 2]
                 ["hello" "world"]
                 ["hello" 1 "world" 2]
                 ["hello" 1 "world"]
                 ["hello" 1.0 "world"]
                 ["hello" 1.0 42]
                 ["hello" 1.0]
                 ["hello" 1]
                 ["hello" 42 "world"]
                 ["hello" 42 1.0]
                 ["hello" 42]
                 ["hello"]
                 [1 "two" "3.0"]
                 [42 "two" "3.0"]
                 [1.0 "two" "3.0"]
                 ))

(def test-rtes '((:* Long)
                 (:* Short)
                 (:* (:cat String Long))
                 (:* (:cat String Short))
                 (:or (:* (:cat String Long))
                      (:* (:cat String Short)))
                 (:or (:* (:cat String Long))
                      (:not (:* (:cat String Short))))

                 (:and (:not (= 1))
                       Long)
                 (:and (:not (= 1))
                       (:* Long))
                 (:* (:and (:not (= 1))
                           Long))
                 (:+ Long)
                 (:+ Short)
                 (:+ (:cat String Long))
                 (:+ (:cat String Short))
                 (:or (:+ (:cat String Long))
                      (:* (:cat String Short)))
                 (:or (:* (:cat String Long))
                      (:+ (:cat String Short)))
                 (:or (:* (:cat String Long))
                      (:not (:* (:cat String Short))))
                 (:or (:+ (:cat String Long))
                      (:not (:* (:cat String Short))))
                 (:or (:* (:cat String Long))
                      (:not (:+ (:cat String Short))))
                 (:+ (:cat String (:? Long)))
                 (:cat (:* String) Long)
                 (:and (:+ (:cat String (:? Long)))
                       (:cat (:* String) Long))))

(deftest t-complete
  (testing "testing clojure-rte.xymbolyco/complete"
    (bdd/with-hash []
      (doseq [rte test-rtes
              :let [dfa (rte-to-dfa rte)
                    dfa-complete (complete dfa)
                    incomplete-states (find-incomplete-states dfa-complete)]]
        (is (empty? incomplete-states))))))

(defn t-acceptance-test-rte
  [rte]
  (doseq [seq-root test-seqs
          exit-value [42 true -1]
          :let [dfa (rte-to-dfa rte exit-value)
                dfa-trim (extend-with-sink-state (trim dfa))
                dfa-min (extend-with-sink-state (minimize dfa))
                dfa-min-trim (extend-with-sink-state (trim dfa-min))
                dfa-trim-min (extend-with-sink-state (minimize dfa-trim))]
          reps (range 5)
          :let [seq-long (reduce concat (repeat reps seq-root))
                match? (rte/match dfa seq-long)]
          ]
    
    (is (= match?
           (rte/match dfa-trim seq-long))
        (format "case 1: rte=%s seq=%s got %s from dfa, got %s from dfa-trim"
                rte (pr-str seq-long) match? (rte/match dfa-trim seq-long)))
    (is (= match?
           (rte/match dfa-min seq-long))
        (format "case 2: rte=%s seq=%s got %s from dfa, got %s from dfa-min"
                rte (pr-str seq-long) match? (rte/match dfa-min seq-long)))
    (is (= match?
           (rte/match dfa-min-trim seq-long))
        (format "case 3: rte=%s seq=%s got %s from dfa, got %s from dfa-min-trim"
                rte (pr-str seq-long) match? (rte/match dfa-min-trim seq-long)))
    (is (= match?
           (rte/match dfa-trim-min seq-long))
        (format "case 4: rte=%s seq=%s got %s from dfa, got %s from dfa-trim-min"
                rte (pr-str seq-long) match? (rte/match dfa-trim-min seq-long)))))
  
(deftest t-acceptance
  (testing "acceptance:  testing whether rte/match works same on dfa when trimmed and minimized."

    (t-acceptance-test-rte  '(:and (:* Long) (:not (:* Short)))) ;; this was an explicit failing test
    (let [[left-rtes right-rtes] (split-at (unchecked-divide-int (count test-rtes) 2)
                                           test-rtes)]
      (doseq [[inx rte] (reverse (map-indexed (fn [inx item] [inx item])
                                              (distinct (for [rte-1 right-rtes
                                                              rte-2 left-rtes
                                                              rte [`(:and ~rte-1 (:not ~rte-2))
                                                                   `(:or  ~rte-1 (:not ~rte-2))]]
                                                          rte))))]
        (println [:inx inx :rte rte])
        (t-acceptance-test-rte rte)))))

(deftest t-test-1
  (testing "particular case 1 which was failing"
    (let [dfa-1 (rte-to-dfa '(:+ (:cat String (:? Long)))
                            1)
          dfa-2 (rte-to-dfa  '(:cat (:* String) Long)
                             2)
          dfa-sxp (synchronized-product dfa-1 dfa-2 
                                        (fn [a b]
                                          (and a b))
                                        (fn [q1 _q2]
                                          ((:exit-map dfa-1) (:index q1))))
          dfa-sxp-trim (trim dfa-sxp)
          dfa-sxp-min (minimize dfa-sxp)
          _dfa-sxp-trim-min (minimize dfa-sxp-trim)
          _dfa-sxp-min-trim (trim dfa-sxp-min)
          seqs [[]
                [1]
                [1 2 3]
                ["hello"]
                ["hello" "world"]
                ["hello" 1]
                ["hello" 1 "world"]
                ["hello" 1 "world" 2]
                ["hello" "world" 2]
                ["hello" "world" 1 "hello" "world" 2]
                ["hello" "world" 1 "hello" "there" "world" 2]
                ]
          ]
      (doseq [s seqs
              :let [m-1 (rte/match dfa-1 s)
                    m-2 (rte/match dfa-2 s)
                    m-dfa-sxp (rte/match dfa-sxp s)]]
        (assert (= (boolean (and m-1 m-2))
                   (boolean m-dfa-sxp))
                (format "dfa-1 => %s and dfa-2 => %s but dfa-sxp => %s, on sequence %s"
                        m-1 m-2 m-dfa-sxp s))))))

(deftest t-test-2
 (testing "particular case 2 which was failing"
   (let [dfa-1 (rte-to-dfa '(:or (:* Long) 
                                 (:not (:or (:* (:cat String Long))
                                            (:* (:cat String Short)))))
                           42)
         test-seq '("hello" 1 "world" 2)
         dfa-min (minimize dfa-1)
         ]
     (is (= (rte/match dfa-1 test-seq)
            (rte/match dfa-min test-seq))))))

(deftest t-sxp
  (testing "sxp"
    (let [dfa-0 (rte-to-dfa '(:and (:* Long) (:not (:or)))
                           0)
          dfa-1 (rte-to-dfa '(:and (:* Boolean) (:not (:or (:* Long))))
                           1)
          dfa-2 (rte-to-dfa '(:and (:* String) (:not (:or (:* Boolean) (:* Long))))
                           2)
          dfa-01 (synchronized-union dfa-0 dfa-1)
          dfa-012 (synchronized-union dfa-01 dfa-2)]
      (is (= 2 (rte/match dfa-012 ["hello" "world"]))))))

(deftest t-cross-intersection
  (testing "cross-intersection"
    (let [cx (cross-intersection '((not Long)
                                   Long)
                                 '((and (not Long) (not Boolean))
                                   Long
                                   Boolean))]
      (is (member 'Long cx)
          (cl-format false "cx=~a, expecting to contain Long" cx))
      (is (member 'Boolean cx)
          (cl-format false "cx=~a, expecting to contain (and (not Long) Boolean)" cx))
      (is (member '(and (not Long) (not Boolean)) cx)
          (cl-format false "cx=~a, expecting to contain (and (not Long) (not Boolean))" cx)))))

(deftest t-missing-final
  (testing "missing final?"
    (let [dfa (rte-to-dfa '(:and (:cat :sigma (:* :sigma))
                                 (:not (:or (:cat :sigma))))
                          12)]
      (is (not-empty (filter (comp boolean :accepting) (states-as-seq dfa)))
          "missing final 1"))))

(deftest t-missing-final-2
  (testing "missing final 2?"
    (let [dfa (rte-to-dfa '(:and (:cat (:* :sigma))
                                 (:not (:or (:cat Boolean :sigma (:* :sigma))
                                            (:cat Boolean :sigma))))
                          13)]
      (is (not-empty (filter (comp boolean :accepting) (states-as-seq dfa)))
          "missing final 2"))))
