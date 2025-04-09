(ns dijkstra-test
  (:require [dijkstra :as sut]
            [clojure.test :refer [deftest is testing]]))

(defn -main []
  (clojure.test/run-tests 'dijkstra-test))


(deftest t-dijkstra-self-loop
  (testing "dijkstra"
    (let [[d p] (sut/dijkstra 0 (fn [v]
                               (case v
                                 (0) {1 1
                                      2 1}
                                 (1) {1 1}
                                 (2) {})))]
      (is (= d {0 0,
             1 1,
                2 1}))
      (is (= @(get p 1) '(0 1)))
      (is (= @(get p 2) '(0 2))))))

(deftest t-dijkstra
  (testing "dijkstra"
    (let [[d p] (sut/dijkstra 0 (fn [v]
                             (case v
                               (0) {1 1}
                               (1) {2 1}
                               (2) {})))]
      (is (= d {0 0,
             1 1,
             2 2}))
      (is (= @(get p 1) '(0 1)))
      (is (= @(get p 2)  '(0 1 2))))))

