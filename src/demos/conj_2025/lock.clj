(ns demos.conj-2025.lock
  (:require [clojure.java.io :as io]
            [clojure.java.shell :refer [sh]]
            [clojure.edn :as edn]
            [clojure.data.csv :as csv]
            [vega-plot :as vega]
            [util :refer [call-in-block with-timeout read-csv-data rename-columns]]
)
)



(def statistics-resource (str (.getPath (io/resource "statistics")) "/"))
(def lock-file (str statistics-resource "statistics.lockfile"))


(defmacro with-lock
  "macro version of call-in-block.
  Evaluate the given code body in a way which blocks access to
  the resource files in statistics/resources/"
  [& body]
  `(call-in-block lock-file
                  (fn [] ~@body)))

(defn merge-file
  "`csv-file-name` is a csv file in the resource/statistics directory
  `write-record` is a unary function, callable with a file writer (from java.io.FileWriter)
  This function `merge-file`, calls `write-record` which is expected to write a line into
  the writer of its argument.  Thereafter, that resulting file is merged (via sort -m) into
  the csv-file in the resource directory. Recall, sort -m, expect the file(s) to already
  be in sorted order.
  It is safe to call this function simultaneously from two different threads or
  two different processes because the manipulation of the resource file is managed
  by `util/call-in-block`"
  [csv-file-name write-record]
  (let [tmp-1 (str statistics-resource (random-uuid))
        tmp-2 (str statistics-resource (random-uuid))]
    (with-open [out-file (java.io.FileWriter. tmp-1 true)]
      (write-record out-file))
    (with-lock
      ;; open the csv file in append mode, i.e., write to the end
      (sh "sort" "-t," "-k1,3n" "-m" tmp-1 csv-file-name "-o" tmp-2)
      (sh "mv" tmp-2 csv-file-name))
    (sh "trash" tmp-1)))


(defn slurp-csv-data
  "Read the named .csv file `csv-file-name` returning a list of maps,
  each map has the keys specified in `fields`.  The fields
  name the columns in order from left to right in the csv file,
  skipping all lines which start with #."
  [csv-file-name fields]
  (with-lock
    (with-open [csv-file (clojure.java.io/reader csv-file-name)]
      (doall (for [line (csv/read-csv csv-file)
                   :when (not (= \# (get (get line 0) 0)))]
               (zipmap fields
                       (map edn/read-string line)))))))


(defn read-resource-csv 
  "Read a csv file into a list of maps.  They keys depend on the column titles
  given on the first line of the csv file."
  [csv-file]
  (with-open [csv-file (clojure.java.io/reader csv-file)]
    (let [[headers lines] (read-csv-data csv-file
                                         :comment? (constantly false)
                                         :parsers {:default edn/read-string}
                                         )]
      (rename-columns headers lines {"#num-states" "num-states"}))))
