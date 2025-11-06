(ns rte-graphviz
  (:import [java.io File FileOutputStream])
  (:require [util :refer [member run-dot]]
            [genus.genus :as gns]
            [view :refer [view-image]]
            [xym.xymbolyco :as xym]
            [rte-construct :as rte])
)

;;import adjuvant.Adjuvant.openGraphicalFile
;;import genus.{SEmpty, STop, SimpleTypeD}
;;import adjuvant.GraphViz.{multiLineString, runDot}

(defn f-dummy [_ignored])
(def default-type-vector [:empty-set :sigma ])



;; returns a vector of type designators (each of tyep SimpleTypeD).
;;    this return value is useful in case the caller wants to make
;;    another call to rteToDot making sure the same t0, t1, t2 assignment
;;    is made to each type.  This helps to compare two different
;;    images, so the same type is notated the same way.
;; `habitation` controls whether the AST is colorized into 4 cases
;;    empty/vacuous = red
;;    universe = blue (equiv to Sigma *)
;;    inhabited = green
;;    dont-know = orange
(defn rte-to-dot [rte & {:keys [dot-stream 
                                title 
                                abbrev 
                                given-types 
                                habitation
                                ;; typeLegend controls whether a legend of the types
                                ;;    appears in the png file.  otherwise the legend
                                ;;    will be printed to stdout
                                type-legend
                                ]
                         :or {title ""
                              abbrev true
                              given-types default-type-vector
                              type-legend false
                              habitation true
                              }}]
  (assert rte)
  (letfn [(linearize [rte]
            (cond (keyword? rte)
                  [rte]

                  (and (sequential? rte)
                       (keyword? (first rte)))
                  (distinct (cons rte (mapcat linearize (rest rte))))

                  :else
                  [rte]))
          (children [rte]
            (cond (keyword? rte)
                  []

                  (and (sequential? rte)
                       (keyword? (first rte)))
                  (rest rte)

                  :else
                  []))
          (write [input-str]
            (doseq [c input-str
                    :let [ i (int c)]
                    :when (< i 256) ;; silently skip characters out of frange
                    ]
              (.write dot-stream (byte (int c)))))
          (type-label [td tds]
            (let [idx (get tds td nil)]
              (assert idx (format "cannot find type %s\nin given merged-types"
                                  td))
              (format "t%d" idx)))
          (node-label [expr tds]
            (cond (gns/valid-type? expr)
                  (type-label expr tds)

                  (rte/valid-rte? expr)
                  (case expr
                    (:epsilon) "()"
                    (:sigma) "&#931;" ;;  Σ
                    (:empty-set) "{}"
                    (first expr))))
          ]
    
    (write "digraph G {\n")
    ;; header
    (write (format "  # %s\n" rte))
    (write "  fontname=courier;\n")
    (write "  rankdir=TB; graph[labeljust=l,nojustify=true]\n")
    (write "  node [fontname=Arial, fontsize=25];\n")
    (write "  edge [fontsize=20];\n")

    (let [linear (linearize rte)
          used-types (distinct (filter gns/valid-type? linear))
          node-map (into {} (map-indexed (fn [idx item] [item idx]) linear))
          ;; collect all the type designators not already in given-types
          merged-types (concat given-types (filter (fn [td] (not (member td given-types))) used-types ))
          tds (into {} (map-indexed (fn [idx item] [item idx]) merged-types))
          ]
      (doseq [[r idx] node-map
              :let [label (node-label r tds)]]
        (write (format "R%s [label=\"%s\"" idx label))
        (if habitation
          (do
            (write ", style=filled, fillcolor=")
            ;; is r equiv to universe ?,
            ;; i.e. is !r equiv to emptyset ?
            (cond (xym/dfa-vacuous? (rte/rte-to-dfa (rte/Not r)) false)
                  (write "skyblue, shape=egg, orientation=180")

                  :else
                  (case (xym/dfa-inhabited? (rte/rte-to-dfa r) :dont-know)
                    (:dont-know) ;; maybe
                    (write "orange, shape=Mcircle")

                    (true) ;; inhabited
                    (write "lightgreen, shape=rect")

                    (false)       ;; empty
                    (write "lightcoral, shape=egg")))))
        (write "]\n"))

      (doseq [[parent idx] node-map
              child (children parent)
              :let [child-index (node-map child)]]
        
        (assert (int? idx))
        (assert (int? child-index))
        (write (format "R%d -> R%d\n" idx child-index)))


      (if (not= "" title)
        (do (write "  labelloc=\"t\";")
            (write "\n  label=\"")
            (write title)
            (write "\"\n")))

    (write "}\n")
    
    merged-types)))

(defn rte-to-png [rte ;; : Rte,
                  & {:keys [
                            title
                            given-types
                            dot-file-CB
                            habitation
                            ;;// typeLegend controls whether a legend of types
                            ;;//   appears in the png file.  otherwise the legend will be
                            ;;//   printed to stdout
                            type-legend
                            ]
                     :or {title ""
                          given-types default-type-vector
                          dot-file-CB f-dummy
                          habitation true
                          type-legend false
                          }}]
  (assert rte)
  (letfn [(to-png [
                   dot-path-name ;;  String,
                   _latex-path-name ;; ignored
                   title ;; : String): Vector[SimpleTypeD] = {
                   ]
            (let [dot-stream (java.io.FileOutputStream. (java.io.File. dot-path-name))
                  merged-types (rte-to-dot rte
                                           :dot-stream dot-stream
                                           :title title
                                           :given-types given-types
                                           :habitation habitation
                                           :type-legend type-legend)]
              (.close dot-stream)
              (dot-file-CB dot-path-name)
              merged-types
              ))]
    (run-dot title "rte" to-png)
  ))

(defn rte-view [rte  ;;n: Rte,
                & {:keys [title
                          given-types
                          dot-file-CB
                          habitation
                          ;;// typeLegend controls whether a legend of types
                          ;;//   appears in the png file.  otherwise the legend will be
                          ;; //   printed to stdout
                          type-legend
                          ]
                   :or {title ""
                        given-types default-type-vector
                        dot-file-CB f-dummy
                        habitation true
                        type-legend false}}]
  (assert rte)
  (let [[labels png] (rte-to-png rte
                                 :title title
                                 :given-types given-types
                                 :dot-file-CB dot-file-CB
                                 :habitation habitation
                                 :type-legend type-legend)]
    (view-image png)
    [labels png]
    ))
