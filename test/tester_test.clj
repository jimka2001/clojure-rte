(ns tester-test
  (:require [rte.core]
            [util.tester :as sut]
            [clojure.test :refer [deftest testing is]]))


(defn -main []
  (clojure.test/run-tests 'tester-test))

(deftest t-random-test-verbose
  (testing "random-test-verbose"
    (sut/random-test
     10 ;; num-tries
     (fn [rnd-value] (is (= 0 rnd-value))) ;; unary-test-fun
     (constantly 0) ;; arg-generator
     nil ;; gen-components
     true ;; verbose
     )))

(deftest t-random-test
  (testing "random-test"
    (sut/random-test
     10 ;; num-tries
     (fn [rnd-value] (is (= 0 rnd-value))) ;; unary-test-fun
     (constantly 0) ;; arg-generator
     nil ;; gen-components
     false ;; verbose
     )))


(deftest t-random-test
  (testing "stack-overflow"
    (let [x (atom 0)]
      (try
        (sut/random-test
         2 ;; num-tries
         (fn rand-test [rnd-value] (rand-test rnd-value)) ;; unary-test-fun
         (constantly 0) ;; arg-generator
         nil ;; gen-components
         true ;; verbose
         )
        (catch java.lang.StackOverflowError e
          (swap! x inc)))
      (is (not= 0 @x)))))
