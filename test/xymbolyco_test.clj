;; Copyright (c) 2020,25 EPITA Research and Development Laboratory
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

(ns xymbolyco-test
  (:refer-clojure :exclude [complement])
  (:require [rte-core ]
            [rte-construct :as rte :refer [rte-to-dfa with-compile-env]]
            [rte-extract :refer [dfa-to-rte]]
            [xymbolyco :as xym]
            [xym-tester :refer [gen-dfa build-state-map]]
            [bdd :as bdd]
            [clojure.pprint :refer [cl-format pprint]]
            [genus :as gns]
            [util :refer [member human-readable-current-time]]
            [dot :refer [dfa-to-dot]]
            [clojure.test :refer [deftest is] :exclude [testing]]))

(def testing-view false)

(defn -main []
  (clojure.test/run-tests 'xymbolyco-test))

(def test-verbose true)
(def test-timeout-ms (* 1 60 1000 ))

(defmacro with-timeout
  [& body]
  `(let [fut# (future ~@body)
         res# (deref fut# test-timeout-ms :timeout)]
     (if (= :timeout res#)
       (throw (ex-info (format "Timed out at %s" (human-readable-current-time))
                       {:timeout-ms test-timeout-ms}))
       res#)))

(defmacro testing
  [string & body]
  `(with-compile-env []
     (when test-verbose
       (println [:testing ~string :starting (human-readable-current-time)]))
     (clojure.test/testing ~string ~@body)
     (when test-verbose
       (println [:finished  (human-readable-current-time)]))))


(deftest t-split-eqv-class
  (testing "split-eqv-class"
    (is (= (xym/split-eqv-class #{1 2 3 4 5 6 7} even?)
           #{#{1 3 5 7}
             #{2 4 6}}))
    (is (= (xym/split-eqv-class #{1 2 3 4 5 6 7 8 9} #(mod % 3))
           #{#{2 5 8}
             #{1 4 7}
             #{3 6 9}}))))

(deftest t-find-eqv-class
  (testing "find-eqv-class"
    (is (= (xym/find-eqv-class [#{1 2 3} #{4 5 6} #{7 8 9}]
                           7)
           #{7 8 9}))
    (is (= (xym/find-eqv-class [#{1 2 3} #{4 5 6} #{7 8 9}]
                           8)
           #{7 8 9}))
    (is (= (xym/find-eqv-class [#{1 2 3} #{4 5 6} #{7 8 9}]
                           2)
           #{1 2 3}))
    (is (= (xym/find-eqv-class #{#{1 2 3} #{4 5 6} #{7 8 9}}
                           2)
           #{1 2 3}))))

(deftest t-minimize
  (testing "minimize"
    (let [dfa1 (rte-to-dfa '(:or (:* Number)
                                 (:cat String Number)
                                 (:* Double)))
          dfa2 (xym/minimize dfa1)]
      (is (= 6 (count (xym/states-as-seq dfa1))) 80)
      (is (= 5 (count (xym/states-as-seq dfa2))) 81))))

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
      (xym/minimize (rte-to-dfa rte)))))

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
      (xym/trim (rte-to-dfa rte))
      (xym/trim       (xym/minimize (rte-to-dfa rte))))))

(deftest t-rte-match-min
  (testing "same results from matching minimized"
    (let [rte '(:* Long)
          dfa (rte-to-dfa rte)
          dfa-min (xym/minimize dfa)
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
  (testing "testing xymbolyco/complete"
    (bdd/with-hash []
      (doseq [rte test-rtes
              :let [dfa (rte-to-dfa rte)
                    dfa-complete (xym/complete dfa)
                    incomplete-states (xym/find-incomplete-states dfa-complete)]]
        (is (empty? incomplete-states))))))

(defn t-acceptance-test-rte
  [rte]
  (doseq [seq-root test-seqs
          exit-value [42 true -1]
          :let [dfa (rte-to-dfa rte exit-value)
                dfa-trim (xym/extend-with-sink-state (xym/trim dfa))
                dfa-min (xym/extend-with-sink-state (xym/minimize dfa))
                dfa-min-trim (xym/extend-with-sink-state (xym/trim dfa-min))
                dfa-trim-min (xym/extend-with-sink-state (xym/minimize dfa-trim))]
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
      (doseq [[_inx rte] (reverse (map-indexed (fn [inx item] [inx item])
                                              (distinct (for [rte-1 right-rtes
                                                              rte-2 left-rtes
                                                              rte [`(:and ~rte-1 (:not ~rte-2))
                                                                   `(:or  ~rte-1 (:not ~rte-2))]]
                                                          rte))))]
        ;; (println [:inx inx :rte rte])
        (t-acceptance-test-rte rte)))))

(deftest t-test-1
  (testing "particular case 1 which was failing"
    (let [dfa-1 (rte-to-dfa '(:+ (:cat String (:? Long)))
                            1)
          dfa-2 (rte-to-dfa  '(:cat (:* String) Long)
                             2)
          dfa-sxp (xym/synchronized-product dfa-1 dfa-2 
                                         (fn [a b]
                                           (and a b))
                                         (fn [q1 _q2]
                                           ((:exit-map dfa-1) (:index q1))))
          dfa-sxp-trim (xym/trim dfa-sxp)
          dfa-sxp-min (xym/minimize dfa-sxp)
          _dfa-sxp-trim-min (xym/minimize dfa-sxp-trim)
          _dfa-sxp-min-trim (xym/trim dfa-sxp-min)
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
        (is (= (boolean (and m-1 m-2))
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
         dfa-min (xym/minimize dfa-1)
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
          dfa-01 (xym/synchronized-union dfa-0 dfa-1)
          dfa-012 (xym/synchronized-union dfa-01 dfa-2)]
      (is (= 2 (rte/match dfa-012 ["hello" "world"]))))))

(deftest t-cross-intersection
  (testing "cross-intersection"
    (let [cx (xym/cross-intersection '((not Long)
                                   Long)
                                 '((and (not Long) (not Boolean))
                                   Long
                                   Boolean))]
      (is (member 'Long cx)
          (cl-format false "cx=~a, expecting to contain Long" cx))
      (is (member 'Boolean cx)
          (cl-format false "cx=~a, expecting to contain Boolean" cx))
      (is (member '(and (not Boolean) (not Long)) cx)
          (cl-format false "cx=~a, expecting to contain (and (not Boolean) (not Long))" cx)))))

(deftest t-missing-final
  (testing "missing final?"
    (let [dfa (rte-to-dfa '(:and (:cat :sigma (:* :sigma))
                                 (:not (:or (:cat :sigma))))
                          12)]
      (is (not-empty (filter (comp boolean :accepting) (xym/states-as-seq dfa)))
          "missing final 1"))))

(deftest t-missing-final-2
  (testing "missing final 2?"
    (let [dfa (rte-to-dfa '(:and (:cat (:* :sigma))
                                 (:not (:or (:cat Boolean :sigma (:* :sigma))
                                            (:cat Boolean :sigma))))
                          13)]
      (is (not-empty (filter (comp boolean :accepting) (xym/states-as-seq dfa)))
          "missing final 2"))))

(deftest t-transition-functions
  (testing "transition functions"
    (letfn [(tf1 [transitions]
              (xym/optimized-transition-function transitions true 99))
            (tf2 [transitions]
              (rte/slow-transition-function transitions 99))]
      (doseq [transitions '[;; [[td state-id] [td state-id] ...]
                            [[:sigma 0]]
                            [[(= 0) 1]
                             [(= 1) 0]]
                            [[(member 0 1 2 3) 1]
                             [(member 4 5 6 7) 0]
                             [(member -1 -2) 2]]]
              :let [f1 (tf1 transitions)
                    f2 (tf2 transitions)]
              item [ -3 -2 -1 0 1 2 3 4 5 6 7 8 9]]

        (is (= (f1 item)
               (f2 item))
            (cl-format false "~&~
                            item=~A~@
                            transitions=~A~@
                            lhs=~A~@
                            rhs=~A~%"
                       item
                       transitions
                       (f1 item)
                       (f2 item)))))))

(deftest t-inhabited
  (testing "inhabited"
    (doseq [rte ['(:cat Long Long (and Long (satisfies even?)))
                 '(:or (:cat Number Number (and Number (satisfies even?)))
                       (:cat Double (and (satisfies even?) String) Number Number))
                 ]]

      (let [dfa (rte-to-dfa rte)]
        (dfa-to-dot dfa :title (gensym "bdd") :view testing-view :verbose false)
        (is (xym/dfa-inhabited? dfa))))))

(deftest t-spanning-paths
  (testing "spanning paths"
    (let [r1 '(:cat Long String (and Long (satisfies even?)))
          r2 '(:or (:cat Number Number (and Number (satisfies even?)))
                   (:cat Double (and (satisfies even?) String) Number Number))
          r3 '(:cat String String String String)
          r4 '(:cat clojure.lang.IPersistentList clojure.lang.IPersistentList)
          dfa-1 (rte-to-dfa r1 42)
          dfa-2 (rte-to-dfa r2 -11)
          dfa-3 (rte-to-dfa r3 12)
          dfa-4 (rte-to-dfa r4 -11)
          dfa-x (reduce xym/synchronized-union [dfa-1 dfa-2 dfa-3 dfa-4])
          spanning-map       (xym/find-spanning-map dfa-x)

          ]
      (is (= #{-11 12 42} (set (keys spanning-map))))
      (is (= spanning-map 
             {-11 [:satisfiable '(0 35 6)],
              12 [:satisfiable '(0 20 13 9 28)],
             42 [:indeterminate '(0 16 45 37)]}))
       (dfa-to-dot dfa-x :draw-sink true :title "union 4" :view testing-view)
      )))


(deftest t-spanning-trace
  (testing "spanning trace"
    (let [r1 '(:cat Long String (and Long (satisfies even?)))
          r2 '(:or (:cat Number Number (and Number (satisfies even?)))
                   (:cat Double (and (satisfies even?) String) Number Number))
          r3 '(:cat String String String String)
          r4 '(:cat clojure.lang.PersistentList clojure.lang.PersistentList)
          dfa-1 (rte-to-dfa r1 42)
          dfa-2 (rte-to-dfa r2 -11)
          dfa-3 (rte-to-dfa r3 12)
          dfa-4 (rte-to-dfa r4 -11)
          dfa-x (reduce xym/synchronized-union [dfa-1 dfa-2 dfa-3 dfa-4])
          spanning-trace       (xym/find-trace-map dfa-x)

          ]
      (is (= spanning-trace
             {-11 '[:satisfiable
                   [[:satisfiable clojure.lang.PersistentList]
                    [:satisfiable clojure.lang.PersistentList]]],
              42 '[:indeterminate
                  [[:satisfiable Long]
                   [:satisfiable String]
                   [:indeterminate (and (satisfies even?)
                                         Long)]]],
              12 '[:satisfiable
                  [[:satisfiable String]
                   [:satisfiable String]
                   [:satisfiable String] 
                   [:satisfiable String]]]}))

      (dfa-to-dot dfa-x :draw-sink true :title "union 4" :view testing-view)
      )))

(deftest t-reclojure-1
  (testing "reClojure 2025"
    (is (gns/valid-type? (gns/Satisfies even?)))
    (is (rte/valid-rte? (gns/Satisfies even?)))
    (is (rte/valid-rte? Integer))
    (is (rte/valid-rte? (rte/Star String)))
    (let [pat-1 (rte/Star (rte/Cat Integer (rte/Star String) (gns/Satisfies even?)))
          pat-2 '(:* (:cat Integer (:* String) (satisfies even?)))
          rte (rte-to-dfa pat-1)]

      (is pat-1)
      (is rte)
      (dfa-to-dot pat-1 :draw-sink true :title "reClojure 1" :view testing-view)
      (dfa-to-dot pat-2 :draw-sink true :title "reClojure 2" :view testing-view))))



(deftest t-gen-dfa
  (testing "testing gen-dfa"
    (doseq [num-states (range 3 5)
            num-transitions (range num-states (+ 2 num-states))
            :let [exit-value 42 
                  type-size 2
                  dfa (gen-dfa 10 25 42 2)
                  min-dfa (xym/minimize dfa)
                  rte (get (dfa-to-rte min-dfa) exit-value :empty-set)]]
      (is rte)
      (if testing-view
        (dot/dfa-view min-dfa (format "min-dfa-%d-%d" num-states num-transitions))))))

(deftest t-gen-dfa-loop
  (testing "testing gen-dfa"
    (doseq [num-states (range 3 5)
            num-transitions (range num-states (+ 2 num-states))
            :let [exit-value 42 
                  type-size 2              
                  dfa-1 (gen-dfa 10 25 42 2)
                  min-dfa (xym/minimize dfa-1)
                  rte (get (dfa-to-rte min-dfa) exit-value :empty-set)
                  dfa-2 (with-timeout (rte-to-dfa rte exit-value))
                  ]]
      (when testing-view
        (dot/dfa-view min-dfa (format "min-dfa-%d-%d" num-states num-transitions)))
      (is (xym/dfa-equivalent? dfa-1 dfa-2 :dont-know))
      )))




;;  (dot/dfa-view dfa "random")
;;  (dot/dfa-view (minimize dfa) "random-min"))

