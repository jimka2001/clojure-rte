(ns reclojure2025
  (:require [rte-case :refer [rte-case dscase rte-case-fn dsdefn]]
            [rte-construct :as rte]
            [xymbolyco :as xym]
            [typecase :refer [typecase]]
            [dot])
)

;; Definition of multiple-arity function
(defn f
  ([a]   12)
  ([a b]   13)
  ([a b [c d]]   14))


(f 0 0)



;; Vain attempt to define destructuring function
(defn f
   ([[a b] c d]   12)
   ([a [b c] d]   13)
   ([a b [c d]]   14))


(defn f
  ([a]   12)
  ([^Boolean a b]   13)
  ([a b [c d]]   14))

(f true 2)
(f 1 2)


(dsdefn f 
  ([[a b] c d] 12)
  ([a [b c] d] 13)
  ([a b [c d]] 14)
  ([a b [c d]] 15))


(dsdefn f 
  ([[a b] c d] (println 12))
  ([a [b c] d] (println 13))
  ([a b [c d]] (println 14))
  ([a b [c d]] (println 15)))

(f 1 2 [10.0 20])

(dsdefn f 
  ([[a b] c d] 12)
  ([a [b c] d] 13)
  ([a b [^Ratio c d]] 14)
  ([a b [^Integer c d]] 15)
  ([a b [^Number c d]] 16)
  ([a b [^Double c d]] 17))


(f 1 2 [10.0 20])
(f 1 2 [2/3 3])


(f 1 2 [10 20])
(instance? Integer 10)
(instance? Long 10)

(dsdefn f 
  ([[a b] c d] 12)
  ([a [b c] d] 13)
  ([a b [^Ratio c d]] 14)
  (^{c (satisfies integer?)} [a b [c d]] 14)
  ([a b [^Double c d]] 16))

(f 1 2 [10 20])




(rte-case [nil nil nil 1 nil nil nil]
  (:and (:cat (:* :sigma) Number (:* :sigma))
        (:cat (:* :sigma) String (:* :sigma)))
  0

  (:contains-any Number)
  1
  
  (:cat (:* :sigma) Number (:* :sigma))
  1

  (:cat (:* :sigma) String (:* :sigma))
  2
)

(fn rte-case-fn [] nil)


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

(println Number)

(instance? Number 10)

;; what about a seqence of numbers which contains a Ratio

(rte/match '(:cat (:* Number) Ratio (:* Number))
           demo-seq)


;; what about a seqence of numbers which contains a Double

(rte/match '(:cat (:* Number) Double (:* Number))
           demo-seq)


;; what about a seqence of numbers which contains a Double AND a Ratio

(rte/match '(:or (:cat (:* Number) Double (:* Number) Ratio (:* Number))
                 (:cat (:* Number) Ratio (:* Number) Double (:* Number)))
           demo-seq)

(rte/match '(:and (:cat (:* Number) Double (:* Number))
                  (:cat (:* Number) Ratio (:* Number)))
           demo-seq)


;; what about contains Double and Ratio in either order

(def rte-1 '(:and (:cat (:* Number) Double (:* Number))
                  (:cat (:* Number) Ratio (:* Number))))

(dot/dfa-to-dot rte-1 :title "first dfa"
                :view true
                :state-legend false)


;; what about contains Double and Ratio in either order

(def rte-2 '(:or (:cat (:* Number) Double (:* Number) Ratio (:* Number))
                 (:cat (:* Number) Ratio (:* Number) Double (:* Number))))

(dot/dfa-to-dot rte-2 :title "second dfa" 
                :view true
                :state-legend false)

;; are the rtes the same?  do have match the same language?

(dot/dfa-to-dot (rte/Xor rte-1 rte-2)
                :title "xor 1-2 dfa"
                :state-legend false
                :view true)                         



(def rte-3 '(:and (:* Number)
                  (:cat (:* :sigma) Ratio (:* :sigma))
                  (:cat (:+ :sigma) Double (:* :sigma))))

(dot/dfa-to-dot rte-3 :title "third dfa" 
                :view true
                :state-legend false)


(dot/dfa-to-dot (xym/trim (rte/rte-to-dfa (rte/And-not rte-3 rte-1)))
                :title "xor 1-3 dfa"
                :state-legend false
                :view true)

;; show example of expansion of (satisfies integer?)

(typecase 12
  Boolean "it is a boolean"
  Short "it is a short"
  (satisfies integer?) "it is an integer"
  (or Ratio Boolean) "it is a ratio"
  String "it is a string")


