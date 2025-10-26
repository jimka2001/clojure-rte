(ns demos.conj-2025.csv
  (:import [java.time LocalDateTime Duration])
  (:require [lock :as lock]
            [clojure.pprint :refer [cl-format]]
            [xymbolyco :as xym]
            [rte-construct :as rte]
            [util :refer [with-timeout]]))

(def statistics-resource "resources/statistics/")


(defn write-csv-statistic [gen-rte prefix csv-file-name]
  (let [rte (gen-rte)
        leaf-count (rte/count-leaves rte)
        node-count (+ leaf-count (rte/count-internal-nodes rte))
        shortest (rte/measure-shortest-branch rte)
        longest (rte/measure-longest-branch rte)
        time-out-sec (max 60 (* 10 longest longest))
        start (LocalDateTime/now)]
    (letfn [(report [state-count transition-count min-state-count min-transition-count duration-ms]
              (lock/merge-file csv-file-name
                               (fn [out-file]
                                 (cl-format out-file "~D,~D," node-count leaf-count)
                                 (doseq [s [state-count transition-count min-state-count min-transition-count]]
                                   (if s
                                     (cl-format out-file "~D," s)
                                     (cl-format out-file "-1,")))
                                 (cl-format out-file "~D,~D,~D~%" shortest longest duration-ms))))]
      (doseq [dfa (with-timeout time-out-sec
                      (do (printf "timed out computing rte-to-dfa %s\n" rte)
                          (report nil nil nil nil time-out-sec)
                          nil)
                    [(rte/rte-to-dfa rte)])
              :let [
                    state-count (count (:states dfa))
                    transition-count (reduce + 0 (map count (map :transitions (xym/states-as-seq dfa))))
                    ]
              min-dfa (with-timeout time-out-sec
                          (do (printf "canceling DFA minimization after %d sec state-count=%d transition-count=%d"
                                      time-out-sec state-count transition-count)
                              (report state-count transition-count nil nil time-out-sec))
                        [(xym/minimize dfa)])
              :let [min-state-count (count (xym/states-as-seq min-dfa))
                    min-transition-count (reduce + 0 (map count (map :transitions (xym/states-as-seq dfa))))
                    end (LocalDateTime/now)
                    duration (.toMillis (Duration/between start end))]
              ]
        (report state-count transition-count min-state-count min-transition-count duration)))))
