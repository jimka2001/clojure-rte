(ns conj-2025-test
  (:require [clojure.test :refer [deftest is]]
            [demos.conj-2025.cli :as cli]
            [rte.construct            :refer [with-compile-env]]
            [util.util :refer [human-readable-current-time]]
            [demos.conj-2025.gen            :as gen]
            [demos.conj-2025.demo :as demo]
            [demos.conj-2025.plot :as sut]))

(def test-verbose true)

(defmacro testing
  [string & body]
  `(do (when test-verbose
         (println [:testing ~string :starting (human-readable-current-time)]))
       (with-compile-env []
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

(deftest demo
  (testing "demo"
    (demo/demo-1)
    (demo/missile-demo '(true true))
    (demo/missile-demo '(false "hello" 3 true true true))
    (demo/missile-demo '(true "hello"))
    (demo/missile-demo '(true 3))
    (demo/missile-demo '(true "3" 3))
    (demo/missile-demo '(true "3" 3 true))))
    
