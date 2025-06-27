(ns view
  (:require [clojure.java.shell :refer [sh]]
            [clojure.java.browse :refer [browse-url]]))

(defn browse-image [file-name]
  (browse-url (str "file://" file-name)))

(defn view-image [file-name]
  (case (System/getProperty "os.name")
    ("Mac OS X")
    (sh "open" "-a" "Google Chrome" file-name)

    :otherwise
    (browse-image file-name)))



