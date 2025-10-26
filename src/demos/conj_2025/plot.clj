(ns demos.conj-2025.plot
  (:require [clojure.string :as str]
            [clojure.java.shell :refer [sh]]
            [clojure.java.io :as io]))

(defn plot-cb [base]
  (assert (not (str/index-of base ".")))
  (let [report-dir "doc/clojure.conj/2025/gnu/"
        gnu-file-name (str report-dir base ".gnu")
        png-file-name (str report-dir base ".png")
        ]
    (assert (.isDirectory (io/file report-dir)))
    (fn [file-name-str]
      (sh "cp" file-name-str gnu-file-name)
      (run-gnu-plot "png" gnu-file-name png-file-name))))
