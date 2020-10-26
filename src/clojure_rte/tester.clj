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

(ns clojure-rte.tester
  (:require [clojure.pprint :refer [cl-format]]
            [clojure-rte.util :refer [map-eagerly]])
)


(defn simplify [unary error-case gen-components]
  (try (do (unary error-case)
           error-case)
       (catch Exception e
         (do
           (cl-format true "e=~A~%" e)
           (or (some (fn [component]
                       (simplify unary component gen-components)) (gen-components error-case))
               error-case)))))

(defn de-lazify [obj]
  (cond (not (sequential? obj))
        obj

        (empty? obj)
        obj

        :else
        (map-eagerly de-lazify obj)))

(defn random-test
  "Call a testing function, unary-test-fun, on *randomly* generated values.
  unary-test-fun is a unary function which can be called with the return value of
     arg-generator as sole argument.   If the function returns, it is deemed successful.
     If the function needs to indicate failure, it should throw an exception as if by
     (assert false \"some error message\").
  arg-generator is called num-tries number of times to generate input for
  num-tries number of calls to function unary-test-fun.
  gen-components is currently unused, but in the future will be used to simply expression
      to attempt to generate a simpler test case, if test fails.
  verbose indicates whether to print verbose information about progression of tests."
  [num-tries unary-test-fun arg-generator gen-components verbose]
  (doseq [n (doall (range num-tries))
          :let [data (de-lazify (arg-generator))]]
    (when verbose
      (cl-format true "~d/~d: trying ~A~%" n num-tries data))
        
    (unary-test-fun data)))
