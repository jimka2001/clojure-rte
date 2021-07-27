(ns clojure-rte.dot
  (:require [backtick :refer [template]]
            ))

(defn test-it
  []
  (fn [td]
    (template (not (clojure.core/unquote td)))))
