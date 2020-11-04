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


(ns clojure-rte.dot-test
  (:require [clojure-rte.rte-core :refer :all :exclude [-main and? or? satisfies? member? not? =?]]
            [clojure-rte.rte-construct :refer [rte-compile with-compile-env]]
            [clojure-rte.dot :as sut]
            [clojure-rte.rte-core :refer :all :exclude [-main]]
            [clojure.test :refer :all]))

(defn -main []
  (clojure.test/run-tests 'clojure-rte.dot-test))

(deftest t-dfa-to-dot
  (testing "dfa-to-dot"
    (with-compile-env ()
      (sut/dfa-to-dot
       (rte-compile '(:and (:cat :sigma :sigma) (:cat (:not String) Long)))
       :title "Example"
       :view false))))
