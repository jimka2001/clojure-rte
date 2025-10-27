(ns demos.conj-2025.plot
  (:require [clojure.string :as str]
            [clojure.java.shell :refer [sh]]
            [clojure.math :refer [log]]
            [demos.conj-2025.gen :refer [algos]]
            [demos.conj-2025.csv :refer [read-csv-lines]]
            [gnuplot :refer [run-gnu-plot histogram gnuplot]]
            [clojure.java.io :as io]))

(defn plot-cb [base]
  (assert (not (str/index-of base ".")))
  (let [report-dir "doc/clojure.conj/2025/gnu/"
        gnu-file-name (str report-dir base ".gnu")
        png-file-name (str report-dir base ".png")
        ]
    (assert (.isDirectory (io/file report-dir)))
    (fn [file-name-str]

      (println  (sh "cp" file-name-str gnu-file-name))
      (println  (run-gnu-plot "png" gnu-file-name png-file-name)))))



(defn plot-histogram
  ([] (plot-histogram "" true))
  ([prefix view]
   (let [buckets (for [algo algos
                       :let [csv-lines (read-csv-lines algo prefix)
                             state-counts (map :state-count csv-lines)]]
                   [algo state-counts])]
     (histogram buckets
                :base-name (if (= "" prefix)
                             "histogram"
                             (format "%s-histogram" prefix))

                :x-label "DFA state count"
                :y-label "Percentage of DFAs per state count"
                :title (format "DFA State distribution for Rte %s" prefix)
                :gnu-file-CB (plot-cb (format "plot-%s-histogram" prefix))
                :keep-if #(<= % 9)
                :other-label ">= 10"
                :view view))))

(defn imb [cl]
  (/ (:longest-branch cl)
     (/ (log (:leaf-count cl))
        (log 2))))

(defn plot-retention
  ([] (plot-retention "" true))
  ([prefix view]
   (let [descrs (for [algo algos
                      :let [csv-lines (read-csv-lines algo prefix)
                            xys (for [cl csv-lines
                                      :when (not= 0 (:node-count cl))]
                                  [(imb cl) (float (/ (:state-count cl)
                                                      (:node-count cl)))])]]
                  [(format "%s %d samples" algo (count csv-lines))
                   (shuffle xys)])]
     (gnuplot descrs
              :title (format "Retention: Ratio node count per state count %s" prefix)
              :x-axis-label "aspect ratio"
              :y-axis-label "retention"
              :y-log true
              :grid true
              :plot-with "points"
              :point-size 0.2
              :gnu-file-CB (plot-cb (format "plot-%s-aspect-ratio" prefix))
              :view view)
     (gnuplot descrs
              :title (format "Retention: Ratio node count per state count %s" prefix)
              :x-axis-label "aspect ratio"
              :y-axis-label "retention"
              :y-log true
              :grid true
              :plot-with "lines"
              :point-size 0.2
              :gnu-file-CB (plot-cb (format "plot-%s-aspect-ratio-ratsnest" prefix))
              :view view))))



;; (plot-retention "" true)
                  
(defn plot-dfa-count-vs-aspect-ratio
  ([] (plot-dfa-count-vs-aspect-ratio "" true))
  ([prefix view]
   (let [descrs (for [algo algos
                      :let [csv-lines (read-csv-lines algo prefix)
                            num-samples (count csv-lines)
                            xys (for [cl csv-lines]
                                  [(imb cl) (:state-count cl)])]]
                  [(format "%s %d samples" algo num-samples)
                   (shuffle xys)])]
     (gnuplot descrs
              :title (format "DFA state count %s vs Aspect Ratio" prefix)
              :x-axis "aspect ratio"
              :y-axis "DFA state count"
              :plot-with "points"
              :y-log true
              :point-size 0.2
              :gnu-file-CB (plot-cb (format "plot-%sdfa-state-count-vs-aspect-ratio" prefix))
              :grid true
              :view view)
     (gnuplot descrs
              :title (format "DFA state count %s vs Aspect Ratio" prefix)
              :x-axis "aspect ratio"
              :y-axis "DFA state count"
              :plot-with "lines"
              :y-log true
              :point-size 0.2
              :gnu-file-CB (plot-cb (format "plot-%sdfa-state-count-vs-aspect-ratio-ratsnest"
                                            prefix))
              :grid true
              :view view))))


(plot-dfa-count-vs-aspect-ratio "" true)

(defn -main [& argv]
  (let [view false]
    (plot-histogram "" view)
    (plot-retention "" view)
    (plot-dfa-count-vs-aspect-ratio "" view)
    
    (System/exit 0)
    ))

