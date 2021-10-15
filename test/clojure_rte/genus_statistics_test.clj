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

(ns clojure-rte.genus-statistics-test
  (:require [clojure-rte.rte-core]
            [clojure-rte.genus :as gns]
            [clojure-rte.genus-tester :refer [gen-type gen-inhabited-type]]
            [clojure.test :refer [deftest is]]))

(defn -main []
  (clojure.test/run-tests 'clojure-rte.genus-statistics-test))

(defmacro testing
  [string & body]
  (let [verbose (gensym)]
    `(gns/call-with-genus-env
      (let [~verbose false]
        (fn []
          (when ~verbose (println [:testing ~string :starting (java.util.Date.)]))
          (clojure.test/testing ~string ~@body)
          (when ~verbose (println [:finished  (java.util.Date.)])))))))

(defn statistics
  "Generate a table of statics indicating the accuracy of the subtype? function."
  [nreps inh]
  (letfn [(distinct-type-pair [depth]
            (if (not inh)
              [(gen-type depth)
               (gen-type depth)]
              (loop []
                (let [t1 (gen-inhabited-type depth)
                      t2 (gen-inhabited-type depth)
                      ]
                  (if (gns/type-equivalent? t1 t2 false)
                    (recur)
                    [t1 t2])))))
          (measure-subtype-computability [n depth]
            (assert (> n 0))
            (let [m (reduce (fn [m _current-item]
                              (let [[rt1  rt2] (distinct-type-pair depth)
                                    can1 (gns/canonicalize-type rt1 :dnf)
                                    can2 (gns/canonicalize-type rt2 :dnf)
                                    s1 (gns/subtype? rt1 rt2 :dont-know)
                                    s2 (gns/subtype? can1 can2 :dont-know)]
                                (letfn [(f [key bool]
                                          [key (+ (get m key 0)
                                                  (if bool 1 0))])]
                                  (into {} [(f :inhabited
                                               (gns/inhabited? td1 false))
                                            (f :inhabited-dnf
                                               (gns/inhabited? can1 false))
                                            (f :equal
                                               (gns/type-equivalent? can1 can2 false))
                                            (f :subtype-true
                                               (= s1 true))
                                            (f :subtype-false
                                               (= s1 false))
                                            (f :subtype-dont-know
                                               (= s1 :dont-know))
                                            (f :subtype-know ;; accuracy
                                               (not= s1 :dont-know))
                                            (f :subtype-dnf-true
                                               (= s2 true))
                                            (f :subtype-dnf-false
                                               (= s2 false))
                                            (f :subtype-dnf-dont-know
                                               (= s2 :dont-know))
                                            (f :subtype-dnf-know ;; accuracy DNF
                                               (not= s2 :dont-know))
                                            (f :gained
                                               (and (= s1 :dont-know) (not= s2 :dont-know)))
                                            (f :lost
                                               (and (not= s1 :dont-know) (= s2 :dont-know)))]))))

                            {}
                            (range n))]
              (map (fn [[k v]] [k
                                (/ (* 100.0 v) n)]) m)))]
    (let [computability (measure-subtype-computability nreps 3)]
      (doall (map println computability))
      (println "--------------------")
      computability)))

(deftest t-statistics
  (testing "statistics"
    (is (statistics 1000 false))))

(deftest t-statistics-inhabited
  (testing "statistics inhabited"
    (is (statistics 1000 true))))

