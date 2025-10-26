(ns gnuplot
  (:require [clojure.java.io :as io]
            [clojure.java.shell :refer [sh]]
            [util :refer [member with-outstring]]
            [view :refer [view-image]]
            ))

(defn run-gnu-plot [terminal gnu-name output-file-name]
  (assert (string? output-file-name))
  (assert (string? gnu-name))
  (assert (string? terminal))
  (sh "rm" "-f" output-file-name)
  (let [s (sh "gnuplot" "-e" (str "set terminal " terminal ";"
                                  "set output '" output-file-name "'"
                                  )
              gnu-name :in-enc "en_US.UTF-8" :out "en_US.UTF-8")]
    (println [:s s])
    (:exit s)))

(gnuplot [["plot-1" [ 1.1 2.3 6.5] [10.2 12.3 6.1]]
          ["plot-2" [ 2.1 3.3 4.5] [10.2 2.3 5.1]]
          ["plot-3" [ 1.1 4.3 9.5 10.2] [6.2 2.3 7.1 7.5]]]
         :view true
         :title "sample plots"
)

(defn gnuplot [data-to-plot
               ;; either Seq[[String Seq[Number] Seq[Number]]]
               ;; or     Seq[[String Seq[[Number Number]]]
               & {:keys [terminals title comment x-axis-label x-log y-axis-label y-log grid
                                      output-file-base-name
                                      plot-with
                                      point-size
                                      key
                                      gnu-file-CB
                                      verbose
                                      view]
                               :as gdp
                               :or {terminals #{"png"}
                                    title ""
                                    comment ""
                                    x-axis-label ""
                                    x-log false
                                    y-axis-label ""
                                    y-log false
                                    grid false
                                    output-file-base-name "curves"
                                    plot-with "linespoints"
                                    point-size 0.8
                                    key "horizontal bmargin"
                                    gnu-file-CB (fn [_str])
                                    verbose false
                                    view false}}]
  (let [curves (for [data data-to-plot] ;;  convert data-t-plot to the form Seq[(String,Seq[Double],Seq[Double])]
                 (if (= 2 (count data))
                   ;; [string [[x,y],[x,y],[x,y]...]]
                   (let [[label seq-of-pairs] data]
                     [label (map first seq-of-pairs) (map second seq-of-pairs)])
                   data))
        gnu-name (str output-file-base-name ".gnu")
        gnu (io/writer gnu-name)
        write (fn [msg]
                (.write gnu msg)
                (.write gnu "\n"))
        log-compatible (fn [projection]
                         (every? (fn [data] 
                                   (let [numbers (projection data)]
                                     (and (every? #(> % 0) numbers)
                                          (not-empty numbers)
                                          (> (reduce max numbers)
                                             (reduce min numbers)))))
                                 curves))
        ]
    (println [:curves curves])
    (assert (member plot-with ["linespoints" "points" "lines"]))
    (assert gnu)
    
    (write (str "# " comment))
    (if (and x-log
             (log-compatible #(nth % 1)))
      (write "set logscale x"))
    (if (and y-log
             (log-compatible #(nth % 2)))
      (write "set logscale y"))
    (if (not= "" x-axis-label)
      (write (format "set xlabel '%s' font ',10'" x-axis-label)))
    (if (not= "" y-axis-label)
      (write (format "set ylabel '%s' font ',10'" y-axis-label)))
    (if grid
      (write "set grid"))
    (write "set key font ',10'")
    (write "set xtics font ',10'")
    (write "set ytics font ',10'")
    (if (= "points" plot-with)
      (doseq [i (count curves)]
        (write (format "set style line %d pt 7 ps %d" (inc i) point-size))))
    
    (write (format "set key %s" key))
    (if (not= "" title)
      (write (format "set title '%s' font ',12'" title)))
    (.write gnu "plot ") ;; no \n
    (let [footer (with-outstring pr-footer
                   (let [header (with-outstring pr-header
                                  (letfn [(write-curve [curve-title i xys]
                                            (pr-header "'-' using 1:2")
                                            (pr-header (format " with %s" plot-with))
                                            (if (= "points" plot-with)
                                              (pr-header (format " ls %d" i)))
                                            (pr-header (format " title '%s'" curve-title))
                                            (pr-footer (format "# %s\n" curve-title))
                                            (doseq [[x y] xys]
                                              (pr-footer (format "%.3f %.3f\n" (float x) (float y))))
                                            (pr-footer "end\n"))]
                                    (if (not-empty curves)
                                      (do (let [[curve-title xs ys] (first curves)]
                                            (write-curve curve-title 1 (map (fn [x y] [x y]) xs ys)))
                                          (doseq [[i [curve-title xs ys]] (map-indexed vector (rest curves))]
                                            (pr-header ",\\\n    ")
                                            (write-curve curve-title (inc i) (map (fn [x y] [x y]) xs ys)))))))]
                     (write header)))]
      (write footer))
    (.close gnu)
    (gnu-file-CB gnu-name)
    
                     
    (doseq [terminal terminals
            :let [output-file-name (str output-file-base-name "." terminal)]]
      (run-gnu-plot terminal gnu-name output-file-name)
      (if verbose
        (view-image output-file-name)))))
      
