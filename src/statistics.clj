(ns statistics
  (:require
   ;; [clojure.data.csv :as csv]
   ;; [clojure.edn :as edn]
   ;; [clojure.java.io :as io]
   ;; [clojure.java.shell :refer [sh]]
   ;; [clojure.pprint :refer [pprint cl-format]]
   ;; [util :refer [member time-expr mean std-deviation read-csv-data rename-columns
   ;;               call-in-block with-timeout human-readable-current-time]]
    [view]
    [statistics-inhabited :refer [update-inhabited-subset-csv plot-inhabited-subset-summary summarize-inhabited-subset-data]]
    [statistics-rte :refer []]
   ;; [vega-plot :as vega]
   ;; [genus :as gns]
   ;; [rte-construct :refer [rte-to-dfa]]
   ;; [rte-extract :refer [dfa-to-rte]]
   ;; [rte-tester :refer [rte-depth rte-count-leaves]]
   ;; [rte-tree-partially-balanced :refer [gen-partially-balanced-rte]]
   ;; [rte-tree-totally-balanced :refer [gen-totally-balanced-rte]]
   ;; [rte-randomize-syntax :refer [gen-rte]]
   ;; [xym-tester :refer [gen-dfa]]
   ;; [xymbolyco :as xym]
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
