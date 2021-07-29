(ns clojure-rte.rte-case )

(defmacro rte-case [& body]
  `(case ~@body))

