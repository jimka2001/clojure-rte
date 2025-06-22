(ns statistics
  (:require
   [genus :as gns]
   [clojure.java.io :as io]
   [clojure.java.shell :refer [sh]]
   [clojure.data.csv :as csv]
   [clojure.edn :as edn]
   [rte-extract :refer [dfa-to-rte]]
   [rte-construct :refer [rte-to-dfa]]
   [clojure.pprint :refer [pprint cl-format]]
   [xymbolyco :as xym]
   [xym-tester :refer [gen-dfa]]
   [util :refer [member time-expr mean std-deviation call-in-block]]
))


(def lock-file (str (.getPath (io/resource "statistics")) "/statistics.lockfile"))
(def subset-csv (.getPath (io/resource "statistics/dfa-subset.csv")))
(def inhabited-csv (.getPath (io/resource "statistics/dfa-inhabited.csv")))


(defn merge-file [csv-file-name write-record]
  (let [tmp-1 (str "/tmp/" (random-uuid))
        tmp-2 (str "/tmp/" (random-uuid))]
    ;; open the csv file in append mode, i.e., write to the end
    (with-open [out-file (java.io.FileWriter. csv-file-name true)]
      (write-record out-file))
    (sh "mv" csv-file-name tmp-2)
    (sh "sort" "-o" csv-file-name "-m" tmp-1 tmp-2)
    (sh "trash" tmp-1)
    (sh "trash" tmp-2)))

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

    (call-in-block lock-file 
                   (fn []

                     ;; open the csv file in append mode, i.e., write to the end
                     (with-open [out-file (java.io.FileWriter. csv-file-name true)]
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

                                     (cl-format out-file "~%")))
                       )))))

(defn summarize-file-1 [csv-file-name]
  ;; # num-states, num-transitions, type-size, probability-indeterminate, min-dfa-state-count, min-dfa-transitions-count, count indeterminate transitions, inhabited dfa language
  (let [lines (with-open [csv-file (clojure.java.io/reader csv-file-name)]
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
                          (map edn/read-string line)))))]
    (letfn [(numeric [key]
              (let [population (map key lines)]
              {:min (reduce min population)
               :max (reduce max population)
               :mean (mean population)
               :sigma (std-deviation population)}
              ))
            (symbolic [key]
              (let [population (map key lines)
                    n (count population)
                    ]
                {:distinct (distinct population)
                 :frequencies (frequencies population)
                 :count (for [x (distinct population)]
                          [x (/ (count (filter #(= % x) population)) n)])
                 }))
            ]
    {:num-states (numeric :num-states)
                               
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
        dfa-xor (xym/synchronized-xor @dfa-1 @dfa-2)
        dfa-empty-word (rte-to-dfa :epsilon exit-value)
        dfa-xor-non-trivial (xym/synchronized-and-not dfa-xor dfa-empty-word)
        subset (xym/dfa-inhabited? (xym/synchronized-and-not @dfa-1 @dfa-2))
        overlap (xym/dfa-inhabited? dfa-xor)
        non-trivial-overlap (xym/dfa-inhabited? dfa-xor-non-trivial)]
    
    (call-in-block lock-file

                   (fn [] 
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
                                   ))))))


(defn summarize-file-2 [csv-file-name]
  ;; num-states, num-transitions, type-size, probability-indeterminate, subset, overlap, non-trivial-overlap
  (let [lines (with-open [csv-file (clojure.java.io/reader csv-file-name)]
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
                          (map edn/read-string line)))))]
    (letfn [(numeric [key]
              (let [population (map key lines)]
              {:min (reduce min population)
               :max (reduce max population)
               :mean (mean population)
               :sigma (std-deviation population)}
              ))
            (symbolic [key]
              (let [population (map key lines)
                    n (count population)
                    ]
                {:distinct (distinct population)
                 :frequencies (frequencies population)
                 :count (for [x (distinct population)]
                          [x (/ (count (filter #(= % x) population)) n)])
                 }))
            ]
    {:num-states (numeric :num-states)
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
                [dfa-1 t-1] (time-expr (gen-dfa num-states
                                                num-transitions
                                                exit-value
                                                type-size))
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
                 [dfa-1 t-1] (time-expr (xym/minimize (gen-dfa num-states
                                                               num-transitions
                                                               exit-value
                                                               type-size)))

                 [sm-1 t-2] (time-expr (xym/find-trace-map dfa-1))

                 [rte t-3] (time-expr (get (dfa-to-rte dfa-1) exit-value :empty-set))

                 [dfa-2 t-4] (time-expr (rte-to-dfa rte))

                 [dfa-xor t-5] (time-expr (xym/synchronized-xor dfa-1 dfa-2))

                 [sm-2 t-6] (time-expr (xym/find-trace-map dfa-xor))

                 total-time (+ t-1 t-2 t-3 t-4 t-5 t-6)
                 ]]
     (let [suffix (format "-%d-%d" num-states num-transitions)]
       (dot/dfa-view dfa-1 (str "dfa-1" suffix))
       (dot/dfa-view dfa-2 (str "dfa-2" suffix))
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

;; (time-build-traces 30)
;; (time-build-dfas 6 6)

(defn -main [& argv]
  (doseq [num-samples (range 100)
          :let [num-states (+ 5 (rand-int 5) (rand-int 5) (rand-int 10) (rand-int 10))
                delta (+ (rand-int num-states)
                         (rand-int num-states))
                num-transitions (+ num-states
                                   delta)
                type-size (+ 2 (rand-int 3))
                probability-indeterminate (/ (+ (rand 1) (rand 1)) 3)
                ]]
    
    (cl-format true "num-samples=~D " num-samples)
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
    
    ;; (pprint (gen-dfa-statistics :num-samples 1
    ;;                             :num-states 7 :num-transitions 18 
    ;;                             :probability-indeterminate 0.3 :num-transitions 30))
    )
)

;; (-main)
