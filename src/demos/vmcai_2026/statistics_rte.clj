(ns demos.vmcai-2026.statistics-rte
  (:require [clojure.data.csv :as csv]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :refer [sh]]
            [clojure.pprint :refer [pprint cl-format]]
            [util.util :refer [member time-expr mean std-deviation read-csv-data rename-columns
                          call-in-block with-timeout human-readable-current-time
                          truthy-let
                          ]]
            [graph.view :as view]
            [graph.vega-plot :as vega]
            [genus.genus :as gns]
            [rte-construct :refer [rte-to-dfa]]
            [rte-extract :refer [dfa-to-rte]]
            [rte-tester :refer [rte-depth rte-count-leaves]]
            [demos.vmcai-2026.rte-tree-partially-balanced :refer [gen-partially-balanced-rte]]
            [demos.vmcai-2026.rte-tree-totally-balanced :refer [gen-totally-balanced-rte]]
            [rte-randomize-syntax :refer [gen-rte]]
            [xym.xym-tester :refer [gen-dfa]]
            [xym.xymbolyco :as xym]
            [util.lock :as lock]
            [demos.vmcai-2026.statistics-inhabited]
            [demos.vmcai-2026.statistics-rte]
            ))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generation of RTE-based data for randomly generated RTEs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def gen-rte-classic-csv (str lock/statistics-resource "gen-rte-classic.csv"))
(def gen-rte-classic-svg (str lock/statistics-resource "gen-rte-classic.svg"))
(def gen-dfa-classic-svg (str lock/statistics-resource "gen-dfa-classic.svg"))

(def gen-rte-partially-balanced-csv (str lock/statistics-resource "gen-rte-partially-balanced.csv"))
(def gen-rte-partially-balanced-svg (str lock/statistics-resource "gen-rte-partially-balanced.svg"))
(def gen-dfa-partially-balanced-svg (str lock/statistics-resource "gen-dfa-partially-balanced.svg"))

(def gen-rte-totally-balanced-csv (str lock/statistics-resource "gen-rte-totally-balanced.csv"))
(def gen-rte-totally-balanced-svg (str lock/statistics-resource "gen-rte-totally-balanced.svg"))
(def gen-dfa-totally-balanced-svg (str lock/statistics-resource "gen-dfa-totally-balanced.svg"))

(def reduction-svg (str lock/statistics-resource "reduction.svg"))




(defn compute-balance-factors [r]
  ;; {:post [(or (println [:r r :returns %]) true)]}
  (cond (not (sequential? r))
        [0 []]

        (keyword? (first r))
        (let [[operator & operands] r
              pairs (map compute-balance-factors operands)
              heights (map first pairs)]
          (if (empty? heights)
            [0 []]
            (let [max-height (reduce max heights)
                  min-height (reduce min heights)
                  balance (- max-height min-height) ]
              [(inc max-height)
               (conj (mapcat second pairs) balance)])))

        :else
        [0 []]))


(defn tab [] (printf "\t"))

(defn build-rtes
  "Using a given genrator function `gen`, create `repetitions` many
  rte objects.  Update the specified `csv-file` with data extracted
  from the objects, one line per rte."
  [depth repetitions & {:keys [gen csv-file verbose] ;; unary function which takes depth argument and generates a random rte of that approximate depth
                        :or {gen gen-partially-balanced-rte
                             csv-file gen-rte-partially-balanced-csv
                             verbose false
                             }}]
  (let [time-out-secs 30
        max-rte-leaf-count 35536
        trivials '(:sigma (:* :sigma) (:cat :sigma (:* :sigma)) (:cat (:* :sigma) :sigma) :epsilon :empty-set)
        freqs (loop [repetitions repetitions
                     rtes nil]
                (if (not (pos? repetitions))
                  (frequencies rtes)
                  (truthy-let [rte-1 (gen depth)
                               count-rte-1-leaves (rte-count-leaves rte-1)
                               [_ balance-factors] (compute-balance-factors rte-1)
                               mean-balance (float (mean balance-factors 0))
                               max-balance (reduce max (conj balance-factors 0))
                               sigma-balance (std-deviation balance-factors 0)
                               :do (when (println [:rep repetitions
                                                   :time (human-readable-current-time)
                                                   :rte-1-count count-rte-1-leaves
                                                   :mean-balance mean-balance
                                                   :max-balance max-balance
                                                   ;; :sigma-balance sigma-balance
                                                   ]))
                               dfa-1 (when (> max-rte-leaf-count count-rte-1-leaves)
                                       (with-timeout time-out-secs
                                           (do (printf "timed out computing rte-to-dfa %s\n" rte-1)
                                               nil)
                                         (do ;;(println "starting (rte-to-dfa rte-1)\n")
                                           (rte-to-dfa rte-1))))
                               :do (when verbose (tab) (println [:dfa-1 dfa-1]))
                               dfa-min (with-timeout time-out-secs
                                           (do (printf "timed out computing minimize %s\n" dfa-1)
                                               nil)
                                         (xym/minimize dfa-1))
                               :do (when verbose (tab) (println [:dfa-min dfa-min]))
                               rte-map (with-timeout time-out-secs
                                           (do (printf "timed out computing dfa-to-rte %s\n" dfa-min)
                                               nil)
                                         (dfa-to-rte dfa-min))
                               rte-2 (when rte-map (get rte-map true :empty-set))
                               :do (if verbose
                                     (do (tab) (println [:rte-2-count (rte-count-leaves rte-2)]))
                                     (printf "skipped computation of rte-2\n"))

                               dfa-2 (when (> max-rte-leaf-count (rte-count-leaves rte-2))
                                       (with-timeout time-out-secs
                                           (do (printf "timed out computing rte-to-dfa %s\n" rte-2)
                                               nil)
                                         (rte-to-dfa rte-2)))]

                    ;; (println [:rep repetitions
                    ;;           :depth depth
                    ;;           :csv-file (cond (= csv-file gen-rte-partially-balanced-csv)
                    ;;                           "partially-balanced"
                    ;;                           (= csv-file gen-rte-totally-balanced-csv)
                    ;;                           "totally-balanced"
                    ;;                           (= csv-file gen-rte-classic-csv)
                    ;;                           "classic")
                    ;;           :date  (human-readable-current-time)])
                    (if dfa-2
                      (do (lock/merge-file csv-file
                                           (fn [out-file]
                                             ;;(cl-format out-file "# requested-depth, actual-depth, max-balance, mean-balance, sigma-balance, actual-leaf-count, minimized-depth, minimized-leaf-count, actual-state-count, minimized-state-count, minimized-rte~%")
                                             (cl-format out-file "~D, ~D, ~D" depth (rte-depth rte-1) (rte-count-leaves rte-1))
                                             (cl-format out-file ", ~A, ~A, ~A" max-balance mean-balance sigma-balance)
                                             (cl-format out-file ", ~D, ~D" (rte-depth rte-2) (rte-count-leaves rte-2))
                                             (cl-format out-file ", ~D, ~D" (count (xym/states-as-seq dfa-1)) (count (xym/states-as-seq dfa-2)))
                                             (cl-format out-file ", ~A" (if (member rte-2 trivials)
                                                                          rte-2
                                                                          :other))
                                             (cl-format out-file "~%")))
                          (recur (dec repetitions) (cons rte-2 rtes)))
                      (recur (dec repetitions) rtes)))))]
    (into {} (for [[k v] freqs
                   :when (> v 1)]
               [k v]))))


(defn build-rtes-classic
  "Interface to build-rtes using classical RTE generation."
  [depth repetitions]
  (build-rtes depth repetitions
              :verbose true
              :gen gen-rte
              :csv-file gen-rte-classic-csv))

(defn build-rtes-partially-balanced 
  "Interface to build-rtes using balanced RTE generation."
  [depth repetitions]
  (build-rtes depth repetitions
              :verbose true
              :gen gen-partially-balanced-rte
              :csv-file gen-rte-partially-balanced-csv))

(defn build-rtes-totally-balanced 
  "Interface to build-rtes using totally balanced RTE generation."
  [probability-binary depth repetitions]
  (build-rtes depth repetitions
              :verbose true
              :gen (fn [depth] (gen-totally-balanced-rte probability-binary depth))
              :csv-file gen-rte-totally-balanced-csv))


(defn slurp-rte-data [csv-file-name]
  ;; # requested-depth, actual-depth, actual-leaf-count, max-balance, mean-balance, sigma-balance, minimized-depth, minimized-leaf-count, actual-state-count, minimized-state-count, minimized-rte
  (lock/slurp-csv-data csv-file-name
                  [:requested-depth
                   :actual-depth
                   :actual-leaf-count
                   :max-balance
                   :mean-balance
                   :sigma-balance
                   :minimized-depth
                   :minimized-leaf-count
                   :actual-state-count
                   :minimized-state-count
                   :minimized-rte]))

(defn slurp-partially-balanced-rte-data []
  (slurp-rte-data gen-rte-partially-balanced-csv))

(defn slurp-totally-balanced-rte-data []
  (slurp-rte-data gen-rte-totally-balanced-csv))

(defn slurp-classic-rte-data []
  (slurp-rte-data gen-rte-classic-csv))



(defn plot-rte-summary []
  (let [partially-balanced-rte-data (slurp-partially-balanced-rte-data)
        totally-balanced-rte-data (slurp-totally-balanced-rte-data)
        classic-rte-data  (slurp-classic-rte-data)
        minimized-leaf-count (fn [data-map]
                               (min (:minimized-leaf-count data-map)
                                    (:actual-leaf-count data-map)))
        ]
    (doseq [[csv-data title plot-path-1 plot-path-2 max-x]
            [[totally-balanced-rte-data "Statistics for totally balanced generation"
              gen-rte-totally-balanced-svg gen-dfa-totally-balanced-svg 200]
             [partially-balanced-rte-data "Statistics for partially balanced generation"
              gen-rte-partially-balanced-svg gen-dfa-partially-balanced-svg 200]
             [classic-rte-data  "Statistics for classic generation"
              gen-rte-classic-svg gen-dfa-classic-svg 200]]
            
            :let [grouped (group-by :actual-leaf-count csv-data)
                  count-lines (count csv-data)
                  histogram (for [[actual-leaf-count lines] grouped
                                  :when (< actual-leaf-count max-x)]
                              [actual-leaf-count (* 10 (/ (float (count lines)) count-lines))])
                  average-minimized-leaf-count (for [[actual-leaf-count lines] grouped
                                                     :when (< actual-leaf-count max-x)]
                                                 [actual-leaf-count (float (mean (map minimized-leaf-count lines)))])
                  average-minimized-state-count (for [[actual-leaf-count lines] grouped
                                                     :when (< actual-leaf-count max-x)]
                                                  [actual-leaf-count (float (mean (map :minimized-state-count lines)))])
                  max-minimized-leaf-count (for [[actual-leaf-count lines] grouped
                                                     :when (< actual-leaf-count max-x)]
                                             [actual-leaf-count (float (reduce max 0 (map minimized-leaf-count lines)))])
                  max-minimized-state-count (for [[actual-leaf-count lines] grouped
                                                  :when (< actual-leaf-count max-x)]
                                              [actual-leaf-count (float (reduce max 0 (map :minimized-state-count lines)))])
                  image-1 (vega/series-scatter-plot (str "rte: " title)
                                                    "Starting leaf count"
                                                    "Final LEAF count"
                                                    [;; ["curve name" [[x y] [x y] [x y] ...]]
                                                     ["average" (sort average-minimized-leaf-count)]
                                                     ["maximum" (sort max-minimized-leaf-count)]
                                                     ["histogram" (sort histogram)]
                                                     ;;["minimized (avg) state count" (sort average-minimized-state-count)]
                                                     ;;["minimized (max) state count" (sort max-minimized-state-count)]
                                                     
                                                     ]
                                                    :y-scale "symlog"
                                                    :x-scale "symlog"
                                                    )
                  image-2 (vega/series-scatter-plot (str "dfa: " title)
                                                    "Starting leaf count"
                                                    "Final STATE count"
                                                    [;; ["curve name" [[x y] [x y] [x y] ...]]
                                                     ;; ["minimized (avg) leaf count" (sort average-minimized-leaf-count)]
                                                     ;; ["minimized (max) leaf count" (sort max-minimized-leaf-count)]
                                                     ["average" (sort average-minimized-state-count)]
                                                     ["maximum" (sort max-minimized-state-count)]
                                                     ["histogram" (sort histogram)]                                                     
                                                     ]
                                                    :y-scale "symlog"
                                                    :x-scale "symlog"
                                                    )]]
      (sh "cp" image-1 plot-path-1)
      (view/view-image image-1)
      (sh "cp" image-2 plot-path-2)
      (view/view-image image-2))

    (let [average-rte-reduction
          (for [[max-balance lines] (group-by :max-balance
                                              (concat totally-balanced-rte-data
                                                      partially-balanced-rte-data
                                                      classic-rte-data))
                ;; if we went from 10 leaves to 2 leaves we have 80% reduction
                :let [rte-reductions (for [line lines]
                                       (* 100.0 (if (= 0 (:actual-leaf-count line))
                                                  1
                                                  (/ (- (:actual-leaf-count line) (minimized-leaf-count line))
                                                     (:actual-leaf-count line)))))]]
            ;; compute average reduction percentage
            [max-balance (mean rte-reductions)])

          average-dfa-reduction
          (for [[max-balance lines] (group-by :max-balance
                                              (concat totally-balanced-rte-data
                                                      partially-balanced-rte-data
                                                      classic-rte-data))
                ;; if we went from 10 states to 2 states we have 80% reduction
                :let [dfa-reductions (for [line lines]
                                       (* 100.0 (if (= 0 (:actual-state-count line))
                                                  1
                                                  (/ (- (:actual-state-count line) (:minimized-state-count line))
                                                     (:actual-state-count line)))))]]
            ;; compute average reduction percentage
            [max-balance (mean dfa-reductions)])

          population-count (+ (count totally-balanced-rte-data)
                              (count partially-balanced-rte-data)
                              (count classic-rte-data))
          dfa-reduction-histogram
          (for [[max-balance lines] (group-by :max-balance
                                              (concat totally-balanced-rte-data
                                                      partially-balanced-rte-data
                                                      classic-rte-data))]
            ;; compute average reduction percentage
            [max-balance (* 100 (/ (float (count lines)) population-count))])

          image (vega/series-scatter-plot "Reduction vs Imbalance"
                                          "Imbalance"
                                          "Percentage"
                                          [["rte reduction" (sort average-rte-reduction)]
                                           ["dfa reduction" (sort average-dfa-reduction)]
                                           ["histogram" (sort dfa-reduction-histogram)]
                                           ])]
      (sh "cp" image reduction-svg)
      (view/view-image image))
      
    ))

;; (plot-rte-summary)

;;;;;;; unsorted

(defn -main [& argv]
  (let [kind (nth argv 0)
        repetitions (Integer/parseInt (nth argv 1))]

    (case kind
      ("totally-balanced")
      (build-rtes-totally-balanced 0.5 4 repetitions)

      ("partially-balanced")
      (build-rtes-partially-balanced 4 repetitions)

      ("classic")
      (build-rtes-classic 4 repetitions))

    (System/exit 0)
    ))
