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

(ns rte-test-memoize
  (:require [rte-core]
            [util.util :refer [human-readable-current-time]]
            [rte-construct :as rte]
            [clojure.pprint :refer [cl-format]]
            [clojure.test :refer [deftest is] :exclude [testing]]))

(def test-verbose false)

(defmacro testing
  [string & body]
  `(rte/with-compile-env []
     (when test-verbose
       (println [:testing 'rte-test ~string :starting (human-readable-current-time)]))
     (clojure.test/testing ~string ~@body)
     (when test-verbose
       (println [:finished ' rte-test ~string (human-readable-current-time)]))))

(deftest t-with-rte-5
  (testing "with-rte 5"
    (rte/with-rte [::x (:+ Long)
               ::y (:+ Double)]
      (let [pat (rte/rte-to-dfa '(:cat ::x  ::y))]
        ;; the same as (rte/compile '(:cat (:+ Long) (:+ Double)))
        (is (rte/match pat [1 2 3 1.2 3.4 5.6 7.8]))
        (is (not (rte/match pat [[1 2 3] [1.2 3.4 5.6 7.8]])))
        ))

    (rte/with-rte [::x (:+ String)
                   ::y (:+ Double)]
      ;; (cl-format true "-------- with-rte -----------~%")
      (is (= (rte/canonicalize-pattern ::x)
             '(:cat String (:* String)))
          "449 canonicalize-pattern got wrong memoized value")
      (is (= (rte/canonicalize-pattern '(:cat ::x  ::y))
             '(:cat String (:* String) Double (:* Double)))
          "452 canonicalize-pattern got wrong memoized value")
      (let [pat (rte/rte-to-dfa '(:cat ::x  ::y))]
        ;; the same as (rte/compile '(:cat (:+ String) (:+ Double)))
        (is (= (rte/canonicalize-pattern '(:cat ::x  ::y))
               '(:cat String (:* String) Double (:* Double)))
            "canonicalize-pattern got wrong memoized value")
        (is (rte/match pat ["1" "2" "3" 1.2 3.4 5.6 7.8]))

        (is (not (rte/match pat [["1" "2" "3"] [1.2 3.4 5.6 7.8]])))
        ))
    

    (let [pat (rte/rte-to-dfa '(:cat (rte (:+ Long)) (rte (:+ Double))))]
      (is (not (rte/match pat [1 2 3 1.2 3.4 5.6 7.8])))
      (is (rte/match pat [[1 2 3] [1.2 3.4 5.6 7.8]])))))

(deftest t-with-rte-5a
  (testing "with-rte 5a"
    (rte/with-rte [::x (:+ Long)]

      (let [pat (rte/rte-to-dfa '(:cat ::x))]
        ;; the same as (rte/compile '(:cat (:+ Long) (:+ Double)))
        (is (rte/match pat [1 2 3]))
        (is (not (rte/match pat [[1 2 3] [1.2 3.4 5.6 7.8]])))
        ))

    (rte/with-rte [::x (:+ String)]
      ;;(cl-format true "-------- with-rte -----------~%")
      (is (= (rte/canonicalize-pattern ::x)
             '(:cat String (:* String)))
          "449a canonicalize-pattern got wrong memoized value")
      (is (= (rte/canonicalize-pattern '(:cat ::x))
             '(:cat String (:* String)))
          "452a canonicalize-pattern got wrong memoized value"))))

(deftest t-with-rte-5b
  (testing "with-rte 5b"
    (rte/with-rte [::x (:+ Long)
                   ::y (:+ Double)]
      (let [pat (rte/rte-to-dfa '(:cat ::x  ::y))]
        ;; the same as (rte/compile '(:cat (:+ Long) (:+ Double)))
        (is (rte/match pat [1 2 3 1.2 3.4 5.6 7.8]))
        (is (not (rte/match pat [[1 2 3] [1.2 3.4 5.6 7.8]])))
        ))

    (rte/with-rte [::x (:+ String)
                   ::y (:+ Double)]
      ;;(cl-format true "-------- with-rte -----------~%")
      (is (= (rte/canonicalize-pattern ::x)
             '(:cat String (:* String)))
          "440b canonicalize-pattern got wrong memoized value")
      
      (is (= '(:cat String (:* String))
             (rte/canonicalize-pattern-once ::x))
          "446b")
      (is (= '((:cat String (:* String))
               (:cat Double (:* Double)))
             (map rte/canonicalize-pattern-once  '(::x ::y)))
          "447b")
      (is (= '(:cat (:cat String (:* String))
                    (:cat Double (:* Double)))
             (rte/create-cat (map rte/canonicalize-pattern-once '(::x ::y))))
          "448b")
      (is (= '(:cat (:cat String (:* String))
                    (:cat Double (:* Double)))
             (rte/create-cat (map rte/canonicalize-pattern-once '(::x ::y))))
          "448bb")
      (is (= '(:cat (:cat String (:* String))
                    (:cat Double (:* Double)))
             (rte/conversion-cat-99 '(:cat ::x  ::y)))
          "449b")
      (is (= (rte/canonicalize-pattern-once '(:cat ::x  ::y))
             '(:cat (:cat String (:* String)) (:cat Double (:* Double))))
          "450b")
      (is (= (rte/canonicalize-pattern '(:cat ::x  ::y))
             '(:cat String (:* String) Double (:* Double)))
          "452b canonicalize-pattern got wrong memoized value")
      (let [_pat (rte/rte-to-dfa '(:cat ::x  ::y))]
        ;; the same as (rte/compile '(:cat (:+ String) (:+ Double)))
        (is (= (rte/canonicalize-pattern '(:cat ::x  ::y))
               '(:cat String (:* String) Double (:* Double)))
            "canonicalize-pattern got wrong memoized value")
        ))))

(defn -main []
  ;; To run one test (clojure.test/test-vars [#'rte-test/the-test])
  (clojure.test/run-tests 'rte-test-memoize))


