(ns vmcai-2025-test
  (:require [clojure.test :refer [deftest is testing]]
            [demos.vmcai-2026.statistics-inhabited :as di]
            [demos.vmcai-2026.statistics-rte :as dr]))


(deftest statistics-rte
  (testing "statistics-rte"
    (dr/build-rtes-totally-balanced 0.5 3 2)

    (dr/build-rtes-partially-balanced 3 2)

    (dr/build-rtes-classic 3 2)

    (dr/plot-rte-summary)))

(deftest statistics-inhabited
  (testing "statistics-inhabited"
    (di/summarize-inhabited-subset-data )
    (di/plot-inhabited-subset-summary)
    (di/update-inhabited-subset-csv 2 5 2)
    (di/summarize-subset-data)
    (di/slurp-subset-data)
    (di/summarize-inhabited-data)))
