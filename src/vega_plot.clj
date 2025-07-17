(ns vega-plot
  (:require [oz.core :as oz]))

(defn series-format-plot-data 
  "encode plotting data into an hashmap ready to pass to oz/view!"
  [chart-title x-label y-label data]
  ;; data is of form [[string [(x y) (x y) (x y) ...]]
  ;;                  [string [(x y) (x y) (x y) ...]]
  ;;                  ...]

  (let [polished-data (for [[label aseq] data
                            [x y] aseq]
                        {x-label (float x)
                         y-label (float y)
                         :series label})]
    {:data {:values polished-data}
     :width 600
     :height 600
     :title {:text chart-title}
     :description chart-title
     :axes [{:orient "bottom"
             :title x-label}
            {:orient "left"
             :title y-label}]
     :encoding {:x {:field x-label
                    :type "quantitative"}
                :y {:field y-label
                    :type "quantitative"}
                :shape {:field "series"
                        :type "nominal"
                        :legend {:orient "bottom"
                                 :title "Curve shape"
                                 :direction "vertical"
                                 :labelLimit 1000}
                        }
                :color {:field "series"
                        :legend {:orient "bottom"
                                 :title "Curve color"
                                 :direction "vertical"
                                 :labelLimit 1000}
                        :type "nominal"}}
     :mark {:type "line" :point {:filled true :size 80}}}))

(defn series-scatter-plot
  "Pop up image in browser showing a graph with multiple curves.
  Each curve corresponds to a series specified in the given `data`."
  [chart-title x-label y-label data]
  ;; data is of form [[string [(x y) (x y) (x y) ...]]
  ;;                  [string [(x y) (x y) (x y) ...]]
  ;;                  ...]
  (let [tmp (.getAbsolutePath (java.io.File/createTempFile chart-title ".svg"))
        formatted-data (series-format-plot-data chart-title x-label y-label data)]

    (oz/export! formatted-data tmp)
    tmp))
