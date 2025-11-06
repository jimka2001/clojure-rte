;; Copyright (c) 2025 EPITA Research and Development Laboratory
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

(ns statistics-test
  (:require [clojure.pprint :refer [cl-format]]
            [clojure.test :refer [deftest is] :exclude [testing]]
            [util.util :refer [human-readable-current-time]]
            [demos.vmcai-2026.statistics :as sut]
            [util.lock :as lock]
            [demos.vmcai-2026.statistics-inhabited :refer [subset-csv inhabited-csv update-inhabited-subset-csv]]
))

(def test-verbose false)

(defmacro testing
  [string & body]
  `(do
     (when test-verbose
       (println [:testing 'rte-test ~string :starting (human-readable-current-time)]))
     (clojure.test/testing ~string ~@body)
     (when test-verbose
       (println [:finished ' rte-test ~string (human-readable-current-time)]))))

(deftest t-read-csv-resource
  (testing "reading csv resource"
    (is (lock/read-resource-csv subset-csv))
    (is (lock/read-resource-csv inhabited-csv))))

(deftest t-update-resource-csv
  (testing "update inhabited subset csv resource"
    (update-inhabited-subset-csv 2 4 3)))
