(ns demos.conj-2025.cli
  (:require [demos.conj-2025.gen :refer [gen-csv-by-size]]))

(defn test-main
  ([algo repetitions]
   (test-main algo repetitions 4 5))
  ([algo repetitions min-leaf max-leaf]
   (gen-csv-by-size repetitions algo
                    "" ;; prefix
                    2 ;; lot
                    (bit-shift-left 1 min-leaf)
                    (bit-shift-left 1 max-leaf)

                    )))

(defn -main [& argv]
  (let [algo (nth argv 0)
        repetitions (Integer/parseInt (nth argv 1))]
    (test-main algo repetitions)

    (System/exit 0)
    ))

;;(test-main "comb" 1)
