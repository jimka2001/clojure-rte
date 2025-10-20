(ns statistics
  (:require
    [view]
    [statistics-inhabited :refer [update-inhabited-subset-csv plot-inhabited-subset-summary summarize-inhabited-subset-data]]
    [statistics-rte :refer []]
))






;;;;;;; unsorted

(defn -main [& argv]
  (let [num-samples (Integer/parseInt (nth argv 0))
        center (Integer/parseInt (nth argv 1))
        radius (Integer/parseInt (nth argv 2))]
    (update-inhabited-subset-csv num-samples center radius)
    (plot-inhabited-subset-summary)
    (summarize-inhabited-subset-data)
    (System/exit 0)
    ))
