(ns reclojure2025
  (:require [rte-case :refer [rte-case dscase rte-case-fn]]))

(dscase '(1 2 (3 4)) ;; '(true 2 ("3" "4"))
  [[^Boolean a b] c d]
  12

  [^Boolean a [b c] ^String d]
  13

  [^Boolean a b [^String c ^String d]]
  (do
    (println "hello")
    (println "this is the 3rd case")
    14)

  [^Number  a b [c d]]
  15
)


(rte-case (for [x [1 2 true 2 3 false]
                ]
            (do (println [:generating :x x])
                x))
  (:* (:cat (:* Number) Boolean))
  1

  (:* (:cat (:* Number) (:or Boolean String)))
  2
  )

(def mf (rte-case-fn [['(:* (:cat (:* Number) Boolean))
                       (fn [] 1)]
                      ['(:* (:cat (:* Number) (:or Boolean String)))
                       (fn [] 2)]]))

(partition 2 2 '[a b c d e f g])


(mf (for [x [1 2 true 2 3 false "hello" []
             ]
          ]
      (do (println [:generating :x x])
          x)))
