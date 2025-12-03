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

(ns rte-tester-test
  (:require [rte.core]
            [util.util :refer [human-readable-current-time]]
            [clojure.pprint :refer [cl-format]]
            [rte.construct :as rte :refer [canonicalize-pattern]]
            [xym.xymbolyco :as xym]
            [rte.tester :refer [test-rte-to-dfa test-rte-not-nullable
                                test-canonicalize-pattern
                                test-rte-canonicalize-nullable
                                test-rte-not
                                rte-components
                                ]]
            [rte.randomize-syntax :refer [gen-rte *rte-keywords*]]
            [genus.genus :as gns]
            [genus.genus-tester :refer [*test-types* gen-inhabited-type]]
            [clojure.test :refer [deftest is] :exclude [testing]]))

(defn -main []
  (clojure.test/run-tests 'rte-tester-test))


(def test-verbose false)

(defmacro testing
  [string & body]
  (let [verbose test-verbose]
  `(rte/with-compile-env []
     (when ~verbose
       (println [:testing ~string :starting (human-readable-current-time)]))
     (clojure.test/testing ~string ~@body)
     (when ~verbose
       (println [:finished  ~string (human-readable-current-time)])))))

(deftest t-test-canonicalize-pattern
  (testing "test-canonicalize-pattern"
    (test-canonicalize-pattern 10 4 false (fn [expr msg] (is expr msg)))))

(deftest t-rte-keywords
  (testing "rte-keywords"
    (doseq [k *rte-keywords*]
      (gen-rte k 4 *test-types*))))

(deftest t-rte-components
  (testing "rte-components"
    (doseq [k *rte-keywords*]
      (rte-components (gen-rte k 4 *test-types*)))))

(deftest t-rte-to-dfa-random
  (testing "rte-to-dfa random"
    (test-rte-to-dfa 30 ; num-tries
                     5 ; size
                     false ; verbose
                     (fn [expr msg]
                       (is expr msg)))))

(deftest t-rte-nullable-not-random
  (testing "nullability of :not"
    ;; if an rte is nullable, then (:not rte) is not nullable
    ;; if an rte is not nullable, then (:not rte) is nullable
    (test-rte-not-nullable 1000 7 false (fn [expr msg] (is expr msg)))))

(deftest t-canonicalize-discovered-case-177
  (testing "test-rte-canonicalize-nullable-1"
    (let [rte '(:not (:contains-every :empty-set
                                      (:and (:and (:contains-any)) (:* (:contains-any)))
                                      (:or (:+ (:cat)) (satisfies string?))
))]
      (is
       (canonicalize-pattern rte)
       "failed to canonicalize")

      (rte/nullable? rte)
      
      (rte.tester/test-rte-canonicalize-nullable-1
        rte
        (fn [expr msg] (is expr msg)))
)))

(deftest t-rte-nullable-canonicalize-random
  (testing "canonicalize of :not"
    (is (= (canonicalize-pattern 
            '(:or (satisfies decimal?)
                 (:contains-any (:* (:* (:cat)))
                                (:not :epsilon)
                                (:contains-any (:* (:contains-any)) (:cat (:or))))
                 (:contains-none (:contains-any (:or (:contains-every)) (:* (member 1 a)))
                                 (:contains-any (:* (:or)) (:+ (:contains-none)))
                                 (:and (satisfies ratio?) (:+ (:cat))))
                 :sigma))
           '(:* :sigma)) "test 076")
    

    (test-rte-canonicalize-nullable 500 ; num-tries
                                    4 ; size
                                    false ;verbose
                                    (fn [expr msg] (is expr msg))
                                    )))

(deftest t-discovered-case-390
  (testing "discovered case 390"
    (let [rte-1 '(:or (= -1) 
                       (:not (:? (satisfies keyword?)))
                       (= a))
          ;; rte-2 reduces to (:or :epsilon clojure.lang.Keyword)
          rte-2 (rte/create-not rte-1)
          rte-1-complement (xym/complement (rte/rte-to-dfa rte-1))
          ]
      (is (rte/match rte-2 []))
      (is (rte/match rte-2 [:x]))
      (is (not (rte/match rte-1 [])))
      (is (not (rte/match rte-1 [:x])))
      (is (rte/match rte-1 [1 2 3]))
      (is (not (rte/match rte-2 [1 2 3])))

      (is (rte/match rte-1-complement []))
      (is (rte/match rte-1-complement [:x]))
      (is (not (rte/match rte-1-complement [1 2 3]))))))

(deftest t-rte-not
  (testing ":not discovered cases"
    (doseq [rte '(:sigma
                  :empty-set
                  (:* :sigma)
                  (:or (:and (:cat (:cat (:not :epsilon)) (member 1 a))
                             (:or (:? (:or)) (:and (satisfies keyword?)))
                             (:not :sigma))
                       :empty-set
                       (:not :sigma)
                       (:cat (:? (:* (:not (:cat))))
                             (:and :empty-set :sigma)
                             (:? (:* (:? :sigma)))))
                  (:contains-any :empty-set 
                                 (:or (:cat (:? :epsilon) 
                                            (:cat (:* (:not :sigma))))
                                      :sigma 
                                      (:contains-any :epsilon 
                                                     (:and (member (1 2 3) (2 1 3)))))
                                 :epsilon 
                                 (satisfies seq?))
                  (:cat (:* Long)
                        (member a 1) 
                        (:not  :sigma))
                  (:cat (:* (satisfies integer?))
                        (member a 1) 
                        (:not  :sigma))
                  (:cat (:* (:*
                             (:* (satisfies integer?))))
                        (:contains-every (:? :epsilon)
                                         (:not
                                          (:or (:or))) (member 1 a))
                        (:contains-any (:contains-any
                                        (:contains-any (:+ (:cat))) (:contains-any (:contains-none)))
                                       :sigma (:or (:or (:contains-every)) (member 1.5 3.5)))
                        (:contains-none :sigma (:cat (satisfies symbol?) (:* (:cat)))
                                        :sigma))
                  (:contains-every (:+ (= 1))
                                   (:+ (= 2)))
                  (:contains-every
                   (:+ (:+ (member 4.5 6.5)))
                   :epsilon
                   (:+ :epsilon)
                   (:+ (:and
                        (:contains-every (:contains-none))
                        (member 2 3 4))))
                  (:or (= -1) 
                       (:not (:? (satisfies keyword?)))
                       (= a))
                  (:or (= -1)
                       (:not (:? (satisfies keyword?)))
                       (member a b c a b c)
                       (:+ :empty-set))
                  (:or (:or (:or (:not :sigma) (= -1))
                            (:not (:? (satisfies keyword?))))
                       (member a b c a b c)
                       (:cat (:+ (:cat (:not (:* (:and)))))
                             (:+ (:and :empty-set))))
                  (:or (:or (:or (:not :sigma) (= -1))
                            (:not (:? (satisfies keyword?)))
                            (:* (:and :empty-set)))
                       (:+ (:+ (:and (:contains-any))))
                       (member a b c a b c)
                       (:cat (:+ (:cat (:not (:* (:and)))))
                             (:+ (:and :empty-set))
                             (:or (:+ (:and)) (:and (:+ (:or))))))
                  (:or (:? (:not (:cat (:contains-none)))))
                  )]
      (rte.tester/test-rte-not-1 rte (fn [expr message] (is expr message))))))

(deftest t-rte-not-random
  (testing ":not random"
    (test-rte-not 1000 4
                  false ;verbose
                  )))

(deftest t-inhabited-type
  (testing "inhabited type"
    (doseq [_ (range 100)
            n (range 5)
            :let [t1 (gen-inhabited-type n)
                  inh (gns/inhabited? t1 :dont-know)]]
      (is (= true inh)
          (cl-format nil "~@
                          t1=~W~@
                          inhabited=~W" t1 inh)))))


;; this test is not yet correctly implemented,
;;    need a good way to compare two rtes for equivalence
;; (deftest t-rte-not-not-canonicalize-random
;;   (testing "nullability of :not"
;;     (test-rte-not-not-canonicalize 1000 5 false)))

