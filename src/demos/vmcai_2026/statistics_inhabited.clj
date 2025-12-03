(ns demos.vmcai-2026.statistics-inhabited
  (:require [clojure.pprint :refer [pprint cl-format]]
            [clojure.java.shell :refer [sh]]
            [util.lock :as lock]
            [graph.vega-plot :as vega]
            [util.util :refer [mean std-deviation with-timeout]]
            [genus.genus :as gns]
            [xym.xymbolyco :as xym]
            [xym.xym-tester :refer [gen-dfa]]
            [rte.construct :refer [rte-to-dfa]]
            [graph.view :as view]
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generation of inhabited data for randomly generated DFAs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def subset-csv (str lock/statistics-resource "dfa-subset.csv"))
(def inhabited-csv (str lock/statistics-resource "dfa-inhabited.csv"))
(def plot-path (str lock/statistics-resource "statistics-plot.svg"))
(def statistics-tex-path (str lock/statistics-resource "statistics-constants.tex"))


(defn slurp-inhabited-data []
  ;; # num-states, num-transitions, type-size, probability-indeterminate, min-dfa-state-count, min-dfa-transitions-count, count indeterminate transitions, inhabited dfa language
  (lock/slurp-csv-data inhabited-csv [:num-states
                             :num-transitions
                             :type-size
                             :probability-indeterminate
                             :min-dfa-state-count
                             :min-dfa-transitions-count
                             :count-indeterminate-transitions
                             :inhabited-dfa-language]))  


(defn summarize-inhabited-data []
  ;; # num-states, num-transitions, type-size, probability-indeterminate, min-dfa-state-count, min-dfa-transitions-count, count indeterminate transitions, inhabited dfa language
  (let [lines (slurp-inhabited-data)]
    (letfn [(numeric [key]
              (let [population (map key lines)]
                {:min (reduce min population)
                 :max (reduce max population)
                 :mean (float (mean population))
                 :sigma (std-deviation population)}
                ))
            (symbolic [key]
              (let [population (map key lines)
                    n (count population)
                    ]
                {:distinct (distinct population)
                 :frequencies (frequencies population)
                 :count (into {} (for [x (distinct population)]
                          [x (float (/ (count (filter #(= % x) population)) n))]))
                 }))
            ]
      {:num-samples (count lines)
       :num-states (numeric :num-states)
       :num-transitions (numeric :num-transitions)
       :type-size (numeric :type-size)
       :probability-indeterminate  (numeric :probability-indeterminate )
       :min-dfa-state-count (numeric :min-dfa-state-count)
       :min-dfa-transitions-count (numeric :min-dfa-transitions-count)
       :count-indeterminate-transitions (numeric :count-indeterminate-transitions )
       :inhabited-dfa-language  (symbolic :inhabited-dfa-language)}
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generation of subset data for randomly generated DFAs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn slurp-subset-data []
  ;; num-states, num-transitions, type-size, probability-indeterminate, subset, overlap, non-trivial-overlap
  (lock/slurp-csv-data subset-csv
                  [:num-states
                   :num-transitions
                   :type-size
                   :probability-indeterminate
                   :subset
                   :overlap
                   :non-trivial-overlap
                   ]))


  
(defn summarize-subset-data []
  ;; num-states, num-transitions, type-size, probability-indeterminate, subset, overlap, non-trivial-overlap
  (let [lines (slurp-subset-data)]
    (letfn [(numeric [key]
              (let [population (map key lines)]
              {:min (reduce min population)
               :max (reduce max population)
               :mean (float (mean population))
               :sigma (std-deviation population)}
              ))
            (symbolic [key]
              (let [population (map key lines)
                    n (count population)
                    ]
                {:distinct (distinct population)
                 :frequencies (frequencies population)
                 :count (into {} (for [x (distinct population)]
                          [x (float (/ (count (filter #(= % x) population)) n))]))
                 }))
            ]
    {:num-samples (count lines)
     :num-states (numeric :num-states)
     :num-transitions (numeric :num-transitions)
     :type-size (numeric :type-size)
     :probability-indeterminate  (numeric :probability-indeterminate )
     :subset (symbolic :subset)
     :overlap (symbolic :overlap)
     :non-trivial-overlap (symbolic :non-trivila-overlap)
  }
  )))

(defn write-inhabited-subset-stats-csv 
  "Append statistics to the file `subset-csv`  resources/statistics/dfa-subset.csv
  and to `inhabited-csv` resources/statistics/dfa-inhabited.csv.
  The statistics registered in `subset-csv` specify information about subset-ness and disjoint-ness
  of two randomly selected dfas.
  The statistics registered in `inhabited-csv` specify information about inhabited-ness of
  the language of the dfa.
  "
  [& {:keys [num-states num-transitions
             type-size probability-indeterminate
             timeout-sec]
      :or {num-states 10
           num-transitions 30
           type-size 2
           probability-indeterminate 0.15
           timeout-sec (* 60 2) ;; 2 minutes
           }}]
  (let [;;csv-file-name subset-csv
        exit-value true
        ]
    (doseq [dfa-1 (with-timeout timeout-sec []
                    ;; sequence of Dfas for doseq iteration
                    [(xym/minimize (gen-dfa :num-states num-states
                                            :num-transitions num-transitions
                                            :exit-value exit-value
                                            :type-size type-size
                                            :probability-indeterminate probability-indeterminate))])
            dfa-2 (with-timeout timeout-sec []
                    ;; sequence of Dfas for doseq iteration
                    [(xym/minimize (gen-dfa :num-states num-states
                                            :num-transitions num-transitions
                                            :exit-value exit-value
                                            :type-size type-size
                                            :probability-indeterminate probability-indeterminate))])
            dfa-and (with-timeout timeout-sec []
                      ;; sequence of Dfas for doseq iteration
                      [(xym/synchronized-intersection dfa-1 dfa-2)])
            dfa-and-not (with-timeout timeout-sec []
                          ;; sequence of Dfas for doseq iteration
                          [(xym/synchronized-and-not dfa-1 dfa-2)])
            dfa-empty-word (with-timeout timeout-sec []
                             ;; sequence of Dfas for doseq iteration
                             [(rte-to-dfa :epsilon exit-value)])
            dfa-and-non-trivial (with-timeout timeout-sec []
                                  [(xym/synchronized-and-not dfa-and dfa-empty-word)])
            
            :let [subset (xym/dfa-subset? dfa-1 dfa-2)
                  overlap (xym/dfa-inhabited? dfa-and)
                  non-trivial-overlap (xym/dfa-inhabited? dfa-and-non-trivial)
                  [satisfiability path] (get (xym/find-trace-map dfa-1) exit-value)]
            ]

      (lock/merge-file inhabited-csv
                       (fn [out-file]
                         ;;(cl-format out-file "# num-states, num-transitions, type-size, probability-indeterminate, min-dfa-state-count, min-dfa-transitions-count, count indeterminate transitions, inhabited dfa language~%")
                         ;; num-states , num-transitions , type-size , probability-indeterminate ,
                         (cl-format out-file "~d,~d,~d,~f" num-states num-transitions type-size probability-indeterminate)

                         ;; min-dfa-state-count ,
                         (cl-format out-file ",~d" (count (:states dfa-1)))

                         ;; min-dfa-transitions-count ,
                         (cl-format out-file ",~d" (reduce + 0 (map (fn [st] (count (:transitions st)))
                                                                    (xym/states-as-seq dfa-1))))

                         ;; count indeterminate transitions
                         (cl-format out-file ",~d" (count (for [q (xym/states-as-seq dfa-1)
                                                                [td _] (:transitions q)
                                                                :when (= :dont-know (gns/inhabited? td :dont-know))]
                                                            1)))

                         ;; inhabited dfa language?
                         (cl-format out-file ",~a" satisfiability)

                         (cl-format out-file "~%")))

      (lock/merge-file subset-csv
                       (fn [out-file]
                         ;;(cl-format out-file "# num-states, num-transitions, type-size, probability-indeterminate, subset, overlap, non-trivial-overlap~%")
                         
                         ;; num-states , num-transitions , type-size , probability-indeterminate
                         (cl-format out-file "~d,~d,~d,~f" num-states num-transitions type-size probability-indeterminate)
                         
                         ;; subset ,
                         (cl-format out-file ",~a" subset)
                         
                         ;; overlap ,
                         (cl-format out-file ",~a" overlap)
                         
                         ;; non-trivial-overlap ,
                         (cl-format out-file ",~a" non-trivial-overlap)
                         
                         (cl-format out-file "~%")
                         )))))

(defn update-inhabited-subset-csv
  "Run a suite of simulations, appending to resources/statistics/dfa-subset.csv
  and dfa-inhabited.csv."
  [num-samples center radius]
  (doseq [nsample (range num-samples)
          :let [num-states (max 2 (+ center (- radius) (rand-int (* 2 radius))))
                delta (+ (rand-int num-states)
                         (rand-int num-states))
                num-transitions (+ num-states
                                   delta)
                type-size (+ 2 (rand-int 3))
                probability-indeterminate (/ (+ (rand 1) (rand 1)) 3)
                ]]
    
    (cl-format true "sample=~D/~D " nsample num-samples)
    (println "++++++++++++++++++++++++++++++++++++++++")
    (println [:num-states num-states
              :num-transitions num-transitions
              :type-size type-size
              :probability-indeterminate probability-indeterminate])

    (write-inhabited-subset-stats-csv :num-states num-states
                                      :num-transitions num-transitions
                                      :type-size type-size
                                      :probability-indeterminate probability-indeterminate)
    ))

(defn plot-inhabited-subset-summary []
  (let [inhabited-grouped (group-by :num-states (slurp-inhabited-data))
        inhabited-xys (for [[num-states lines] inhabited-grouped
                            :let [m (frequencies (map :inhabited-dfa-language lines))
                                  indeterminate (get m :indeterminate 0)
                                  satisfiable (get m :satisfiable 0)]
                            :when (> (+ satisfiable indeterminate) 0)
                            :let [
                                  satisfiable-percent (* 100 (float (/ satisfiable (+ satisfiable indeterminate))))
                                  ]]
                        
                        [num-states satisfiable-percent])

        inhabited-dont-know-xys (for [[num-states lines] inhabited-grouped
                                      :let [m (frequencies (map :inhabited-dfa-language lines))
                                            indeterminate (get m :indeterminate 0)
                                            satisfiable (get m :satisfiable 0)]
                                      :when (> (+ satisfiable indeterminate) 0)
                                      :let [
                                            satisfiable-percent (* 100 (float (/ indeterminate (+ satisfiable indeterminate))))
                                            ]]
                                  
                                  [num-states satisfiable-percent])

        sample-lines (slurp-subset-data)
        grouped-subset (group-by :num-states sample-lines)
        subset-true-xys (for [[num-states lines] grouped-subset
                              :let [m (frequencies (map :subset lines))
                                    count-true (get m true 0)
                                    count-false (get m false 0)
                                    count-dont-know (get m :dont-know 0)
                                    count (+ count-true count-false count-dont-know)]
                              :when (> count 0)
                              :let [
                                    true-percent (* 100 (float (/ count-true count)))
                                    ]]
                          
                          [num-states true-percent])
        subset-dont-know-xys (for [[num-states lines] grouped-subset
                                   :let [m (frequencies (map :subset lines))
                                         count-true (get m true 0)
                                         count-false (get m false 0)
                                         count-dont-know (get m :dont-know 0)
                                         count (+ count-true count-false count-dont-know)]
                                   :when (> count 0)
                                   :let [
                                         dont-know-percent (* 100 (float (/ count-dont-know count)))
                                         ]]
                               
                               [num-states dont-know-percent])
        overlap-dont-know-xys (for [[num-states lines] grouped-subset
                                    :let [m (frequencies (map :overlap lines))
                                          count-true (get m true 0)
                                          count-false (get m false 0)
                                          count-dont-know (get m :dont-know 0)
                                          count (+ count-true count-false count-dont-know)]
                                    :when (> count 0)
                                    :let [dont-know-percent (* 100 (float (/ count-dont-know count)))]
                                    ]
                                
                                [num-states dont-know-percent])
        overlap-true-xys (for [[num-states lines] grouped-subset
                               :let [m (frequencies (map :overlap lines))
                                     count-true (get m true 0)
                                     count-false (get m false 0)
                                     count-dont-know (get m :dont-know 0)
                                     count (+ count-true count-false count-dont-know)]
                               :when (> count 0)
                               :let [true-percent (* 100 (float (/ count-true count)))]
                               ]
                           
                           [num-states true-percent])

        non-trivial-overlap-true-xys (for [[num-states lines] grouped-subset
                                           :let [m (frequencies (map :non-trivial-overlap lines))
                                                 count-true (get m true 0)
                                                 count-false (get m false 0)
                                                 count-dont-know (get m :dont-know 0)
                                                 count (+ count-true count-false count-dont-know)]
                                           :when (> count 0)
                                           :let [
                                                 true-percent (* 100 (float (/ count-true count)))
                                                 ]]
                                       
                                       [num-states true-percent])

        non-trivial-overlap-dont-know-xys (for [[num-states lines] grouped-subset
                                                :let [m (frequencies (map :non-trivial-overlap lines))
                                                      count-true (get m true 0)
                                                      count-false (get m false 0)
                                                      count-dont-know (get m :dont-know 0)
                                                      count (+ count-true count-false count-dont-know)]
                                                :when (> count 0)
                                                :let [
                                                      true-percent (* 100 (float (/ count-dont-know count)))
                                                      ]]
                                            
                                            [num-states true-percent])

        population (count sample-lines)
        histogram-vmcai (for [[num-states lines] grouped-subset
                            :let [count-local (count lines)]
                            :when (> population 0)
                            :let [
                                  true-percent (* 100 (float (/ count-local population)))
                                  ]]
                        
                        [num-states true-percent])
        plot-data [

                                         ["inhabited=true" (sort inhabited-xys)]
                                         ["inhabited=dont-know" (sort inhabited-dont-know-xys)]

                                         ["subset=true" (sort subset-true-xys)]
                                         ;; ["4: subset*2=true" (sort (map (fn [[x y]] [x (* 2 y)]) subset-true-xys))]
                                         ["subset=dont-know" (sort subset-dont-know-xys)]

                                         ["overlap=true" (sort overlap-true-xys)]
                                         ["overlap=dont-know" (sort overlap-dont-know-xys)]
                                         
                                         ["state-count-histogram" (sort histogram-vmcai)]

                                         ;;["non-trivila-overlap=true" (sort non-trivial-overlap-true-xys)]
                                         ;;["non-trivial-overlap=dont-know" (sort non-trivial-overlap-dont-know-xys)]
                                         ]
        image (vega/series-scatter-plot (format "Statistics for %d samples" (count sample-lines))
                                        "state count"
                                        "probability"
                                        plot-data
                                        :sort (map first plot-data)
)]

    (sh "cp" image plot-path)
    (view/view-image image)

    image))


          

(defn summarize-inhabited-subset-data []
  (let [ssd (summarize-subset-data)
        sid (summarize-inhabited-data)
        round-2 (fn [val] (format "%3.2f" (float val)))
        ]
    (printf "---------------\n")
    (lock/with-lock
      (with-open [out-file (java.io.FileWriter. statistics-tex-path)]
        (doseq [[sym value]
                [['vmcainumsamples (:num-samples ssd)]
                 ['vmcaiminsize (get-in ssd [:num-states :min])]
                 ['vmcaimaxsize (get-in ssd [:num-states :max])]
                 ['vmcaisizemu  (round-2 (get-in ssd [:num-states :mean]))]
                 ['vmcaisizesigma (round-2 (get-in ssd [:num-states :sigma]))]

                 ['vmcaimintransitions (get-in ssd [:num-transitions :min])]
                 ['vmcaimaxtransitions (get-in ssd [:num-transitions :max])]
                 ['vmcaitransitionsmu  (round-2 (get-in ssd [:num-transitions :mean] 0))]
                 ['vmcaitransitionssigma (round-2 (get-in ssd [:num-transitions :sigma] 0))]

                 ['vmcaiminindeterminatetransitions (get-in sid [:count-indeterminate-transitions :min])]
                 ['vmcaimaxindeterminatetransitions (get-in sid [:count-indeterminate-transitions :max])]
                 ['vmcaimuindeterminatetransitions (round-2 (get-in sid [:count-indeterminate-transitions :mean] 0))]
                 ['vmcaisigmaindeterminatetransitions (round-2 (get-in sid [:count-indeterminate-transitions :sigma]))]

                 ['vmcainuminhabited (get-in sid [:inhabited-dfa-language :frequencies :satisfiable])]
                 ['vmcaipercentinhabited (round-2 (* 100 (get-in sid [:inhabited-dfa-language :count :satisfiable] 0)))]
                 ['vmcainumindeterminate (get-in sid [:inhabited-dfa-language :frequencies :indeterminate])]
                 ['vmcaipercentindeterminate
                  (round-2 (* 100 (get-in sid [:inhabited-dfa-language :count :indeterminate] 0)))]
                 ['vmcaipercentsubset (round-2 (* 100 (get-in ssd [:subset :count true] 0)))]
                 ['vmcaipercentnotsubset (round-2 (* 100 (get-in ssd [:subset :count false] 0)))]
                 ['vmcaipercentdontknowsubset (round-2 (* 100 (get-in ssd [:subset :count :dont-know] 0)))]
                 ['vmcaipercentdisjoint (round-2 (* 100 (get-in ssd [:overlap :count false] 0)))]
                 ['vmcaipercentnotdisjoint (round-2 (* 100 (get-in ssd [:overlap :count true] 0)))]
                 ['vmcaipercentdontknowdisjoint (round-2 (* 100 (get-in ssd [:overlap :count :dont-know] 0)))]
                 ]]
          (cl-format out-file "\\newcommand\\~a{~a}~%" sym value))))))

