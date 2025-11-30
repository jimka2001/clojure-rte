(ns conj-2025-test
  (:require [clojure.test :refer [deftest is testing]]
            [demos.conj-2025.cli :as cli]
            [demos.conj-2025.gen            :as gen]
            [demos.conj-2025.plot :as sut]))

(deftest histogram
  (testing "histogram"
    (sut/plot-histogram "" false)))

(deftest retention
  (testing "retention"
    (sut/plot-retention "" false)))

(deftest dfa-count-vs-aspect-ratio
  (testing "dfa-count-vs-aspect-ratio"
    (sut/plot-dfa-count-vs-aspect-ratio "" false)))

(deftest threshold
  (testing "threshold"
    (sut/plot-threshold "" false)))


(deftest sample-view
  (testing "sample-view"
    (gen/sample-view 28)))

(deftest gen-by-size
  (testing "gen-by-size"
    (doseq [algo gen/algos]
      (cli/test-main algo 1))))
