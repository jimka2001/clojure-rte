;; Copyright (c) 2020 EPITA Research and Development Laboratory
;;Permission is hereby granted, free of charge, to any person
;;obtaining a copy of this software and associated documentation files
;;(the "Software"), to deal in the Software without restriction,
;;including without limitation the rights to use, copy, modify, merge,
;;publish, distribute, sublicense, and/or sell copies of the Software,
;;and to permit persons to whom the Software is furnished to do so,
;;subject to the following conditions: The above copyright notice and
;;this permission notice shall be included in all copies or
;;substantial portions of the Software.  THE SOFTWARE IS PROVIDED "AS
;;IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
;;NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
;;PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR
;;OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
;;OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
;;OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


(ns clojure-rte.genus-spec-test
  (:require [clojure-rte.rte-core]
            [clojure-rte.genus :as gns]
            [clojure.spec.alpha :as s]
            [clojure-rte.genus-spec :as gs]
            [clojure.test :as t]))

(defn -main []
  (clojure.test/run-tests 'clojure-rte.genus-spec-test))


(s/def ::test-1 (s/* (s/alt :x  (s/cat :a neg? :b even?)  
                            :y  (s/cat :c odd? :d pos?))))

(t/deftest t-spec-to-rte
  (t/testing "spec-to-rte"
    (let [rte '(rte
                (:*
                 (:or
                  (:cat (spec clojure.core/neg?) (spec clojure.core/even?))
                  (:cat (spec clojure.core/odd?) (spec clojure.core/pos?)))))]
      (t/is (= (gs/spec-to-rte (s/form (s/* (s/alt :x  (s/cat :a neg? :b even?)  
                                                   :y  (s/cat :c odd? :d pos?)))))
               rte))
      

      (t/is (= (gs/spec-to-rte `(s/* (s/alt :x  (s/cat :a neg? :b even?)  
                                            :y  (s/cat :c odd? :d pos?))))
               rte))

      (t/is (= (gs/spec-to-rte (s/form ::test-1))
               rte))
      )))

(t/deftest t-canonicalize-type
  (t/testing "spec canonicalize-type"
    (let [rte '(rte
                (:*
                 (:or
                  (:cat
                   (satisfies clojure.core/neg?)
                   (satisfies clojure.core/even?))
                  (:cat
                   (satisfies clojure.core/odd?)
                   (satisfies clojure.core/pos?)))))]

      (t/is (= (gns/canonicalize-type `(~'spec ~(s/* (s/alt :x  (s/cat :a neg? :b even?)  
                                                            :y  (s/cat :c odd? :d pos?)))))
               rte))

      (t/is (= (gns/canonicalize-type '(spec ::test-1))
               rte))

      (t/is (= (gns/canonicalize-type '(spec (s/* (s/alt :x  (s/cat :a neg? :b even?)  
                                                         :y  (s/cat :c odd? :d pos?)))))
               '(rte
                 (:*
                  (:or
                   (:cat (satisfies neg?) (satisfies even?))
                   (:cat (satisfies odd?) (satisfies pos?))))))))))

    
