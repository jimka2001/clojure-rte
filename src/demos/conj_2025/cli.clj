(ns demos.conj-2025.cli
  (:require [demos.conj-205.gen :refer [gen-csv-by-size]]))

(defn -main [& argv]
  (let [algo (nth argv 0)
        repetitions (Integer/parseInt (nth argv 1))]

    (gen-csv-by-size repetitions algo
                     "" ;; prefix
                     6 ;; lot
                     (bit-shift-left 1 6) ;; min-leaf
                     (bit-shift-left 1 7) ;; max-leaf

                     )

    (System/exit 0)
    ))
