(ns reclojure2025
  (:require [rte-case :refer [rte-case dscase rte-case-fn dsdef]]
            [rte-construct :as rte]
            [typecase :refer [typecase]]
            [dot])
)

(defn f
  ([[a b] c d]   12)
  ([a [b c] d]   13)
  ([a b [c d]]   14))

(defn f
  ([a]   12)
  ([^Boolean a b]   13)
  ([a b [c d]]   14))

(f 1 2)

(dsdef f 
       ([[a b] c d] 12)
       ([a [b c] d] 13)
       ([a b [c d]] 14)
       ([a b [c d]] 15))

(f 1 [10 20] 3)



(dsdef f 
       ([[a b] c d]  12)
       ([a [b c] d] 13)
       ([^Boolean a b [c d]] 14)
       ([^Number  a b [c d]] 15))

(f 1 2 3)


(rte-case [1 2 true 2 3 false 'a-symbol 'b-symbol]
  (:* (:cat (:* Number) Boolean))
  1

  (:* (:cat (:* Number) (:or Boolean String)))
  2
)

(rte-case (for [x [1 2 true 2 3 false 'a-symbol 'b-symbol]
                ]
            (do (println [:generating :x x])
                x))
  (:* (:cat (:* Number) Boolean))
  1

  (:* (:cat (:* Number) (:or Boolean String)))
  2
)


(rte-case (for [x (range 40)
                ]
            (do (println [:generating :x x])
                2.1))
  (:* Number)
  1

  (:*  (:or Boolean String))
  2
  )

(def mf (rte-case-fn [['(:* (:cat (:* Number) Boolean))
                       (fn [] 1)]
                      ['(:* (:cat (:* Number) (:or Boolean String)))
                       (fn [] 2)]]))



(mf (for [x [1 2 true 2 3 false "hello" []
             ]
          ]
      (do (println [:generating :x x])
          x)))

;; we want to match a sequence of numbers.

(def demo-seq [1 2 4/3 4 5 6.5 3 2])

(rte/match '(:* Number)
           demo-seq)

;; what about a seqence of numbers which contains a Ratio

(rte/match '(:cat (:* Number) clojure.lang.Ratio (:* Number))
           demo-seq)


;; what about a seqence of numbers which contains a Double

(rte/match '(:cat (:* Number) Double (:* Number))
           demo-seq)


;; what about a seqence of numbers which contains a Double AND a Ratio

(rte/match '(:or (:cat (:* Number) Double (:* Number) clojure.lang.Ratio (:* Number))
                 (:cat (:* Number) clojure.lang.Ratio (:* Number) Double (:* Number)))
           demo-seq)

(rte/match '(:and (:cat (:* Number) Double (:* Number))
                  (:cat (:* Number) clojure.lang.Ratio (:* Number)))
           demo-seq)


;; what about contains Double and Ratio in either order

(def rte-1 '(:and (:cat (:* Number) Double (:* Number))
                  (:cat (:* Number) clojure.lang.Ratio (:* Number))))

(dot/dfa-to-dot rte-1 :title "first dfa"
                :view true)


;; what about contains Double and Ratio in either order

(def rte-2 '(:or (:cat (:* Number) Double (:* Number) clojure.lang.Ratio (:* Number))
                 (:cat (:* Number) clojure.lang.Ratio (:* Number) Double (:* Number))))

(dot/dfa-to-dot rte-2 :title "second dfa" 
                :view true)

;; are the rtes the same?  do have match the same language?

(dot/dfa-to-dot (rte/Xor rte-1 rte-2)
                :title "xor dfa"
                :state-legend false
                :view true)                         


;; show example of expansion of (satisfies integer?)

(typecase x
  Boolean "it is a boolean"
  (satisfies integer?) "it is an integer"
  (satisfies number?) "it is a number"
  String "it is a string")


