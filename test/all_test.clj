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

(ns all-test
  (:require  [rte-core]
             clojure.test
             api-test
             bdd-test
             cl-compat-test
             xymbolyco-test
             dot-test
             rte-test
             rte-case-test
             rte-tester-test
             genus-test
             genus-statistics-test
             genus-subtype-test
             genus-equiv-test
             genus-conversion-test
             util-test             
             genus-spec-test
             typecase
             )
  (:gen-class))

(defn rte-run-all-test
  "Run all the tests defined in the clojure-rte project"
  []
  (clojure.test/run-tests 'api-test
                          'bdd-test
                          'cl-compat-test
                          'xymbolyco-test
                          'dot-test
                          'dijkstra-test
                          'rte-test
                          'rte-case-test
                          'rte-tester-test
                          'genus-test
                          'genus-statistics-test
                          'genus-subtype-test
                          'genus-equiv-test
                          'genus-conversion-test
                          'util-test
                          'genus-spec-test
                          'typecase
                          ))

(defn -main []
  (rte-run-all-test))



