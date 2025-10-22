(ns demos.conj-2025.cli
  (:require [demos.conj-2025.gen :refer [gen-csv-by-size]]))

(defn test-main [algo repetitions]
  (gen-csv-by-size repetitions algo
                     "" ;; prefix
                     2 ;; lot
                     (bit-shift-left 1 5) ;; min-leaf
                     (bit-shift-left 1 6) ;; max-leaf

                     ))

(defn -main [& argv]
  (let [algo (nth argv 0)
        repetitions (Integer/parseInt (nth argv 1))]
    (test-main algo repetitions)

    (System/exit 0)
    ))

;; (test-main "comb-rte" 1)
