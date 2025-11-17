(ns demos.conj-2025.csv
  (:import [java.time LocalDateTime Duration])
  (:require [clojure.pprint :refer [cl-format]]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [util.lock :as lock]
            [xym.xymbolyco :as xym]
            [rte-construct :as rte]
            [util.util :refer [with-timeout]]            
))

(def statistics-resource "resources/statistics/")

(defn csv-file-name [algo prefix]
  (str statistics-resource prefix algo ".csv"))

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


(defn read-csv-lines [algo prefix]
  (let [csv-file-name (csv-file-name algo prefix)
        keys [:node-count
              :leaf-count
              :state-count
              :transition-count
              :minimized-state-count
              :minimized-transition-count
              :shortest-branch
              :longest-branch
              :duration-ms]
        ]
    (with-open [reader (io/reader csv-file-name)]
      (assert reader (format "cannot read from %s" csv-file-name))
      (doall
       (for [csv-strings (csv/read-csv reader)
             :when (not-empty csv-strings)
             :when (not= \# (first (first csv-strings)))
             :let [_ (assert (= (count keys) (count csv-strings))
                             (format "invalid line %s in %s" csv-strings csv-file-name))
                   csv-line (map Integer/parseInt csv-strings)
                   parsed (into {} (map-indexed (fn [i k] [k (nth csv-line i)])
                                                keys))]]
         parsed)))))


    
