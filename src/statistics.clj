(ns statistics
  (:require
   [clojure.data.csv :as csv]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.java.shell :refer [sh]]
   [clojure.pprint :refer [pprint cl-format]]
   [genus :as gns]
   [rte-construct :refer [rte-to-dfa]]
   [rte-extract :refer [dfa-to-rte]]
   [rte-tester :refer [gen-balanced-rte rte-depth rte-count-leaves]]
   [util :refer [member time-expr mean std-deviation call-in-block read-csv-data rename-columns]]
   [xym-tester :refer [gen-dfa]]
   [xymbolyco :as xym]
   [vega-plot :as vega]
   [view]
))


(def lock-file (str (.getPath (io/resource "statistics")) "/statistics.lockfile"))
(def subset-csv (.getPath (io/resource "statistics/dfa-subset.csv")))
(def inhabited-csv (.getPath (io/resource "statistics/dfa-inhabited.csv")))
(def plot-path (.getPath (io/resource "statistics/statistics-plot.svg")))
(def statistics-tex-path (.getPath (io/resource "statistics/statistics-constants.tex")))


(defn merge-file 
  "`csv-file-name` is a csv file in the resourse/statistics directory
  `write-record` is a unary function, callable with a file writer (from java.io.FileWriter)
  This function `merge-file`, calls `write-record` which is expected to write a line into
  the write of its argument.  Thereafter, that resulting file is merged (via sort -m) into
  the csv-file in the resource directory.
  It is safe to call this function simultaneously from two different threads or
  two different processes because the manipulation of the resource file is managed
  by `util/call-in-block`"
  [csv-file-name write-record]
  (let [tmp-1 (str "/tmp/" (random-uuid))
        tmp-2 (str "/tmp/" (random-uuid))]
    (with-open [out-file (java.io.FileWriter. tmp-1 true)]
      (write-record out-file))
    (call-in-block lock-file
                   (fn []
                     ;; open the csv file in append mode, i.e., write to the end
                     (sh "sort" "-m" tmp-1 csv-file-name "-o" tmp-2)
                     (sh "mv" tmp-2 csv-file-name)))
    (sh "trash" tmp-1)))

(defn write-stats-1-csv [& {:keys [csv-file-name num-states num-transitions
                                   exit-value type-size probability-indeterminate]
                            :or {csv-file-name inhabited-csv
                                 num-states 10
                                 num-transitions 30
                                 exit-value true
                                 type-size 2
                                 probability-indeterminate 0.15
                                 }}]
  (let [dfa (gen-dfa :num-states num-states
                     :num-transitions num-transitions
                     :exit-value exit-value
                     :type-size type-size
                     :probability-indeterminate probability-indeterminate)
        min-dfa (xym/minimize dfa)
        [satisfiability path] (get (xym/find-trace-map min-dfa) exit-value)]

    (merge-file csv-file-name
                (fn [out-file]
                  ;;(cl-format out-file "# num-states, num-transitions, type-size, probability-indeterminate, min-dfa-state-count, min-dfa-transitions-count, count indeterminate transitions, inhabited dfa language~%")
                  ;; num-states , num-transitions , type-size , probability-indeterminate ,
                  (cl-format out-file "~d,~d,~d,~f" num-states num-transitions type-size probability-indeterminate)

                  ;; min-dfa-state-count ,
                  (cl-format out-file ",~d" (count (:states min-dfa)))

                  ;; min-dfa-transitions-count ,
                  (cl-format out-file ",~d" (reduce + 0 (map (fn [st] (count (:transitions st)))
                                                             (xym/states-as-seq min-dfa))))

                  ;; count indeterminate transitions
                  (cl-format out-file ",~d" (count (for [q (xym/states-as-seq min-dfa)
                                                         [td _] (:transitions q)
                                                         :when (= :dont-know (gns/inhabited? td :dont-know))]
                                                     1)))

                  ;; inhabited dfa language?
                  (cl-format out-file ",~a" satisfiability)

                  (cl-format out-file "~%")))))

(defn slurp-inhabited-data []
  (let [csv-file-name inhabited-csv]
    ;; # num-states, num-transitions, type-size, probability-indeterminate, min-dfa-state-count, min-dfa-transitions-count, count indeterminate transitions, inhabited dfa language
    (with-open [csv-file (clojure.java.io/reader csv-file-name)]
      (doall (for [line (csv/read-csv csv-file)
                   :when (not (= \# (get (get line 0) 0)))]
               (zipmap
                [:num-states
                 :num-transitions
                 :type-size
                 :probability-indeterminate
                 :min-dfa-state-count
                 :min-dfa-transitions-count
                 :count-indeterminate-transitions
                 :inhabited-dfa-language]
                (map edn/read-string line)))))))
  

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



(defn write-stats-2-csv [& {:keys [csv-file-name num-states num-transitions
                                   exit-value type-size probability-indeterminate]
                            :or {csv-file-name subset-csv
                                 num-states 10
                                 num-transitions 30
                                 exit-value true
                                 type-size 2
                                 probability-indeterminate 0.15
                                 }}]
  (let [dfa-1 (future (xym/minimize (gen-dfa :num-states num-states
                                             :num-transitions num-transitions
                                             :exit-value exit-value
                                             :type-size type-size
                                             :probability-indeterminate probability-indeterminate)))
        dfa-2 (future (xym/minimize (gen-dfa :num-states num-states
                                             :num-transitions num-transitions
                                             :exit-value exit-value
                                             :type-size type-size
                                             :probability-indeterminate probability-indeterminate)))
        dfa-and (xym/synchronized-intersection @dfa-1 @dfa-2)
        dfa-empty-word (rte-to-dfa :epsilon exit-value)
        dfa-and-non-trivial (xym/synchronized-and-not dfa-and dfa-empty-word)
        subset (xym/dfa-inhabited? (xym/synchronized-and-not @dfa-1 @dfa-2))
        overlap (xym/dfa-inhabited? dfa-and)
        non-trivial-overlap (xym/dfa-inhabited? dfa-and-non-trivial)]
    
    (merge-file csv-file-name
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
                  ))))


(defn read-resource-csv [csv-file]
  (with-open [csv-file (clojure.java.io/reader csv-file)]
    (let [[headers lines] (read-csv-data csv-file
                                         :comment? (constantly false)
                                         :parsers {:default edn/read-string}
                                         )]
      (rename-columns headers lines {"#num-states" "num-states"}))))

(defn plot-resource-csv [csv-file x-axis y-axis]
  (let [[headers lines] (read-resource-csv csv-file)]
    (vega/series-scatter-plot "some data"
                              x-axis
                              y-axis
                              [[(format "%s vs %s" x-axis y-axis)
                                (for [line lines]
                                  [(get line x-axis)
                                   (get line y-axis)])]])))

(defn slurp-subset-data []
  (let [csv-file-name subset-csv]
    ;; num-states, num-transitions, type-size, probability-indeterminate, subset, overlap, non-trivial-overlap
    (with-open [csv-file (clojure.java.io/reader csv-file-name)]
      (doall (for [line (csv/read-csv csv-file)
                   :when (not (= \# (get (get line 0) 0)))]
               (zipmap
                [:num-states
                 :num-transitions
                 :type-size
                 :probability-indeterminate
                 :subset
                 :overlap
                 :non-trivial-overlap
                 ]
                (map edn/read-string line)))))))
  
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

(defn gen-dfa-statistics [& {:keys [num-samples num-states num-transitions
                                    exit-value type-size probability-indeterminate]
                             :as options
                             :or {num-samples 100
                                  num-states 10
                                  num-transitions 30
                                  exit-value true
                                  type-size 2
                                  probability-indeterminate 0.15}}]
  (letfn [(hier-size [seq]
            (if (seq? seq)
              (apply + (map hier-size seq))
              1))]
    (for [n (range num-samples)]
      (let [dfa (gen-dfa :num-states num-states
                         :num-transitions num-transitions
                         :exit-value exit-value
                         :type-size type-size
                         :probability-indeterminate probability-indeterminate)
            min-dfa (xym/minimize dfa)
            rte (dfa-to-rte min-dfa)
            [satisfiability path] (get (xym/find-trace-map min-dfa) exit-value)
            size (hier-size rte)]
        (dot/dfa-view min-dfa (format "min-%d-%d" num-states num-transitions))
        (dot/dfa-view dfa (format "dfa-%d-%d" num-states num-transitions))
        (printf "%d,%d,%d,%s\n"
                num-states
                (count (:states min-dfa))
                (reduce + 0 (map (fn [st] (count (:transitions st)))
                                 (xym/states-as-seq min-dfa)))
                satisfiability)
        [(count (:states min-dfa))
         (reduce + 0 (map (fn [st] (count (:transitions st)))
                          (xym/states-as-seq min-dfa)))
         satisfiability
         (xym/serialize-dfa min-dfa)
         ]
        ))))



(defn time-build-traces [max-num-states]
  (println "=========================================")
  (doseq [num-states (range 2 max-num-states)
          num-transitions (range num-states (* 2 num-states))
          :let [exit-value 42 
                type-size 2
                [dfa-1 t-1] (time-expr (gen-dfa :num-states num-states
                                                :num-transitions num-transitions
                                                :exit-value exit-value
                                                :type-size type-size))
                [min-dfa t-2] (time-expr (xym/minimize dfa-1))
                [sm t-3] (time-expr (xym/find-trace-map min-dfa))]]
    (println "------------------------------------")
    (pprint [;;:num-states num-states
             ;;:num-transitions num-transitions 
             :num-minimized-states (count (xym/states-as-seq min-dfa))
             :sm (for [[ev [satisfiability path]] sm]
                   [ev satisfiability])
             :time (+ t-1 t-2 t-3)])
    (if (> (+ t-1 t-2 t-3) 400)
      (dot/dfa-view min-dfa (format "dfa-%d-%d" num-states num-transitions)))

    ))

(defn time-build-dfas
  ([max-num-states]
   (time-build-dfas 2 max-num-states))
  ([min-num-states max-num-states]
   (println "=========================================")
   (doseq [num-states (range min-num-states (inc max-num-states))
           num-transitions (range num-states (/ (* num-states num-states) 2) 5)
           :let [exit-value 42
                 type-size 2
                 [dfa-1 t-1] (time-expr (gen-dfa :num-states num-states
                                                 :num-transitions num-transitions
                                                 :exit-value exit-value
                                                 :probability-indeterminate 0.4
                                                 :type-size type-size))

                 [sm-1 t-2] (time-expr (xym/find-trace-map dfa-1))

                 [rte t-3] (time-expr (get (dfa-to-rte dfa-1) exit-value :empty-set))

                 [dfa-2 t-4] (time-expr (rte-to-dfa rte))

                 [dfa-xor t-5] (time-expr (xym/synchronized-xor dfa-1 dfa-2))

                 [sm-2 t-6] (time-expr (xym/find-trace-map dfa-xor))

                 total-time (+ t-1 t-2 t-3 t-4 t-5 t-6)
                 ]]
     (let [suffix (format "-%d-%d" num-states num-transitions)]
       (dot/dfa-view dfa-1 (str "dfa-rand" suffix))
       (dot/dfa-view dfa-2 (str "dfa-rte" suffix))
       (dot/dfa-view dfa-xor (str "dfa-xor" suffix)))
     (println "------------------------------------")
     (println [[ num-states num-transitions ]
               :num-minimized-states (count (xym/states-as-seq dfa-1))
               :sm-1 (for [[ev [satisfiability path]] sm-1]
                       [ev satisfiability])
               :sm-2 (for [[ev [satisfiability path]] sm-2]
                       [ev satisfiability])
               :time total-time])
     )))

(defn build-dfas-balanced 
  ;; NOT YET FINISHED, gather data on gen-balanced-rte
  [depth repetitions]
  (loop [repetitions repetitions
         rtes nil]
    (let [rte-1 (gen-balanced-rte depth)
          dfa-1 (rte-to-dfa rte-1)
          rte-2 (get (dfa-to-rte dfa-1) true :empty-set)
          dfa-2 (rte-to-dfa rte-2)
          dfa-xor (xym/synchronized-xor dfa-1 dfa-2)]

      (println [:state-count [:dfa-1 (count (xym/states-as-seq dfa-1))
                              :dfa-2 (count (xym/states-as-seq dfa-2))]
                :rte [:rte-1 [:depth (rte-depth rte-1)
                              :leaves (rte-count-leaves rte-1)

                              ]
                      :rte-2 [:depth (rte-depth rte-2)
                              :leaves (rte-count-leaves rte-2)
                              ;;:rte rte-2
                              ]]
                      
                :equivalent (xym/dfa-equivalent? dfa-1 dfa-2)])

      (if (pos? repetitions)
        (recur (dec repetitions) (cons rte-2 rtes))
        (frequencies rtes)))))

;; (time-build-traces 30)
;; (time-build-dfas 6 6)

(defn update-resource-csv [num-samples center radius]
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

    (let [csv-1 (future   (write-stats-1-csv :num-states num-states
                                             :num-transitions num-transitions
                                             :type-size type-size
                                             :probability-indeterminate probability-indeterminate))
          csv-2 (future (write-stats-2-csv :num-states num-states
                                           :num-transitions num-transitions
                                           :type-size type-size
                                           :probability-indeterminate probability-indeterminate))]
      @csv-1
      @csv-2
      )
    ))

(defn plot-summary []
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
        histogram-xyz (for [[num-states lines] grouped-subset
                            :let [count-local (count lines)]
                            :when (> population 0)
                            :let [
                                  true-percent (* 100 (float (/ count-local population)))
                                  ]]
                        
                        [num-states true-percent])
        image (vega/series-scatter-plot (format "Statistics for %d samples" (count sample-lines))
                                        "state count"
                                        "probability"
                                        [

                                         ["1: inhabited=true" (sort inhabited-xys)]
                                         ["2: inhabited=dont-know" (sort inhabited-dont-know-xys)]

                                         ["3: subset=true" (sort subset-true-xys)]
                                         ;; ["4: subset*2=true" (sort (map (fn [[x y]] [x (* 2 y)]) subset-true-xys))]
                                         ["4: subset=dont-know" (sort subset-dont-know-xys)]

                                         ["5: overlap=true" (sort overlap-true-xys)]
                                         ["6: overlap=dont-know" (sort overlap-dont-know-xys)]
                                         
                                         ["7: state-count-histogram" (sort histogram-xyz)]

                                         ;;["non-trivila-overlap=true" (sort non-trivial-overlap-true-xys)]
                                         ;;["non-trivial-overlap=dont-know" (sort non-trivial-overlap-dont-know-xys)]
                                         ])]

    (sh "cp" image plot-path)
    (view/view-image image)

    image))

(defn round-2 [val]
  (format "%3.2f" (float val)))

(defn summarize-data []
  (let [ssd (summarize-subset-data)
        sid (summarize-inhabited-data)]
    (pprint [:ssd ssd])
    (pprint [:sid sid])
    (printf "---------------\n")
    (call-in-block lock-file
                   (fn []
                     (with-open [out-file (java.io.FileWriter. statistics-tex-path)]
                       (doseq [[sym value]
                               [['xyznumsamples (:num-samples ssd)]
                                ['xyzminsize (get-in ssd [:num-states :min])]
                                ['xyzmaxsize (get-in ssd [:num-states :max])]
                                ['xyzsizemu  (round-2 (get-in ssd [:num-states :mean]))]
                                ['xyzsizesigma (round-2 (get-in ssd [:num-states :sigma]))]

                                ['xyzmintransitions (get-in ssd [:num-transitions :min])]
                                ['xyzmaxtransitions (get-in ssd [:num-transitions :max])]
                                ['xyztransitionsmu  (round-2 (get-in ssd [:num-transitions :mean] 0))]
                                ['xyztransitionssigma (round-2 (get-in ssd [:num-transitions :sigma] 0))]

                                ['xyzminindeterminatetransitions (get-in sid [:count-indeterminate-transitions :min])]
                                ['xyzmaxindeterminatetransitions (get-in sid [:count-indeterminate-transitions :max])]
                                ['xyzmuindeterminatetransitions (round-2 (get-in sid [:count-indeterminate-transitions :mean] 0))]
                                ['xyzsigmaindeterminatetransitions (round-2 (get-in sid [:count-indeterminate-transitions :sigma]))]

                                ['xyznuminhabited (get-in sid [:inhabited-dfa-language :frequencies :satisfiable])]
                                ['xyzpercentinhabited (round-2 (* 100 (get-in sid [:inhabited-dfa-language :count :satisfiable] 0)))]
                                ['xyznumindeterminate (get-in sid [:inhabited-dfa-language :frequencies :indeterminate])]
                                ['xyzpercentindeterminate
                                 (round-2 (* 100 (get-in sid [:inhabited-dfa-language :count :indeterminate] 0)))]
                                ['xyzpercentsubset (round-2 (* 100 (get-in ssd [:subset :count true] 0)))]
                                ['xyzpercentnotsubset (round-2 (* 100 (get-in ssd [:subset :count false] 0)))]
                                ['xyzpercentdontknowsubset (round-2 (* 100 (get-in ssd [:subset :count :dont-know] 0)))]
                                ['xyzpercentdisjoint (round-2 (* 100 (get-in ssd [:overlap :count false] 0)))]
                                ['xyzpercentnotdisjoint (round-2 (* 100 (get-in ssd [:overlap :count true] 0)))]
                                ['xyzpercentdontknowdisjoint (round-2 (* 100 (get-in ssd [:overlap :count :dont-know] 0)))]
                                ]]
                         (cl-format out-file "\\newcommand\\~a{~a}~%" sym value)))))))


(defn -main [& argv]
  (let [num-samples (Integer/parseInt (nth argv 0))
        center (Integer/parseInt (nth argv 1))
        radius (Integer/parseInt (nth argv 2))]
    (update-resource-csv num-samples center radius)
    (plot-summary)
    (summarize-data)
    (System/exit 0)
    ))
