(ns rte-graphviz
  (:import [java.io File FileOutputStream])
  (:require [util :refer [member run-dot]]
            [xymbolyco :as xym]
            [rte-construct :as rte])
)

;;import adjuvant.Adjuvant.openGraphicalFile
;;import genus.{SEmpty, STop, SimpleTypeD}
;;import adjuvant.GraphViz.{multiLineString, runDot}

(defn f-dummy [])
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
  (letfn [(write [str]
            (doseq [c str]
              (.write dot-stream (.toByte c))))
          (simple [str]
            (fn [_operand _functions]
              (format "%s" str)))
          (node-label [expr]
            (rte/traverse-pattern expr
                              (assoc rte/*traversal-functions*
                                     :epsilon (simple "()")
                                     :empty-set (simple "{}")
                                     :sigma (simple  "&#931;"  ;; Σ
                                                     )
                                     :type (fn [operand _functions]
                                             (format "%s" operand))
                                     :or (simple :or)
                                     :cat (simple :cat)
                                     :and (simple :and)
                                     :not (simple :not)
                                     :* (simple "*"))))
          (get-types [expr]
            (letfn [(multi [operands _functions]
                      (mapcat get-types operands))
                    (single [operand _functions]
                      (get-types operand))
                    (leaf [_operand _functions]
                      [leaf])]
              (rte/traverse-pattern expr
                                (assoc rte/*traversal-functions*
                                       :epsilon leaf
                                       :empty-set leaf
                                       :sigma leaf
                                       :type  leaf
                                       :or multi
                                       :cat multi
                                       :and multi
                                       :not single
                                       :* single))))
          ]
    
    (write "digraph G {\n")
    ;; header
    (write "  fontname=courier;\n")
    (write "  rankdir=TB; graph[labeljust=l,nojustify=true]\n")
    (write "  node [fontname=Arial, fontsize=25];\n")
    (write "  edge [fontsize=20];\n")

    (let [linear (map-indexed (fn [idx item] [item idx]) (rte/linearize rte))
          node-map (into {} linear)
          used-types (distinct (get-types rte))
          ;; collect all the type designators not already in given-types
          merged-types (concat given-types (filter (fn [td] (not (member td given-types))) used-types ))
          tds (into {} (map-indexed (fn [item idx] [idx item]) merged-types))
          ]
      
      (doseq [[r idx] linear
              :let [label (node-label r)]]
        (write (format "R%s [label=\"%s\"" idx label))
        (if habitation
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
                  (write "lightcoral, shape=egg"))))
        (write "\n"))

      (doseq [[parent idx] linear
              child (rte/children parent)
              :let [child-index (node-map child)]]
        
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
  (let [[labels png] (rte-to-png rte
                                 :title title
                                 :given-types given-types
                                 :dot-file-CB dot-file-CB
                                 :habitation habitation
                                 :type-legend type-lebend)]
    (view-image png)
    [labels png]
    ))





