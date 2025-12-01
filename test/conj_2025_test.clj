(ns conj-2025-test
  (:require [clojure.test :refer [deftest is]]
            [demos.conj-2025.cli :as cli]
            [rte-construct            :as rte]
            [util.util :refer [human-readable-current-time]]
            [demos.conj-2025.gen            :as gen]
            [demos.conj-2025.plot :as sut]))

(def test-verbose true)

(defmacro testing
  [string & body]
  `(do (when test-verbose
         (println [:testing ~string :starting (human-readable-current-time)]))
       (rte/with-compile-env []
         (clojure.test/testing ~string ~@body))
       (when test-verbose
         (println [:finished  (human-readable-current-time)]))))

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
    (gen/sample-view 10)))

(deftest gen-by-size
  (testing "gen-by-size"
    (doseq [algo gen/algos]
      (cli/test-main algo 1))))
