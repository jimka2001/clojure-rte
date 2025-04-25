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
#_(defn f
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

;; Vain attempt
(dsdefn f 
  ([[a b] c d] 12)
  ([a [b c] d] 13)
  ([a b [^Ratio c d]] 14)
  ([a b [^integer? c d]] 15)
  ([a b [^Number c d]] 16)
  ([a b [^Double c d]] 17))

(def f
  (let [dfa__51157__auto__ (rte-case/clauses-to-dfa
                            '([0
                               (:cat
                                (rte
                                 (:cat
                                  (and :sigma :sigma)
                                  (and :sigma :sigma)))
                                (and :sigma :sigma)
                                (and :sigma :sigma))]
                              [1
                               (:cat
                                (and :sigma :sigma)
                                (rte
                                 (:cat
                                  (and :sigma :sigma)
                                  (and :sigma :sigma)))
                                (and :sigma :sigma))]
                              [2
                               (:cat
                                (and :sigma :sigma)
                                (and :sigma :sigma)
                                (rte
                                 (:cat
                                  (and Ratio :sigma)
                                  (and :sigma :sigma))))]))]
    (fn [& seq__51158__auto__]
      (let [fns__51159__auto__ [(fn conv-1 [[^integer? a ^integer? b] c d] 12)
                                (fn conv-1 [a [b c] d] 13)
                                (fn conv-1 [a b [^Ratio c ^integer? d]] 14)]
            ind__51160__auto__ (rte/match
                                dfa__51157__auto__
                                seq__51158__auto__
                                :promise-disjoint
                                true)]
        (if ind__51160__auto__
          (apply
           (fns__51159__auto__ ind__51160__auto__)
           seq__51158__auto__)
          (throw
           (ex-info
            "No pattern matching given list"
            {:sequence seq__51158__auto__})))))))

(fn [a b [c d]] nil)

(dsdefn f 
  ([[a b] c d] 12)
  ([a [b c] d] 13)
  ([a b [^Ratio c d]] 14)
  (^{c (satisfies integer?)} [a b [c d]] 15)
  ([a b [^Double c d]] 16))

(f 1 2 [10 20])


(rte-case [nil nil nil 1 nil nil nil]
  (:and (:cat (:* :sigma) Number (:* :sigma))
        (:cat (:* :sigma) String (:* :sigma)))
  0

  ;; (:contains-any Number)
  (:cat (:* :sigma) Number (:* :sigma))
  1

  ;; (:contains-any String)
  (:cat (:* :sigma) String (:* :sigma))
  2
)

(rte-case [1 2 true 2 3 false 'a-symbol 'b-symbol]
  (:* (:cat (:* Number) Boolean))
  1

  (:* (:cat (:* Number) (:or Boolean String)))
  2
)

;; we want to match a sequence of numbers.

(def demo-seq [1 2 4/3 4 5 6.5 3 2])

(rte/match '(:* Number)
           demo-seq)

(instance? Number 10)

;; what about a seqence of numbers which contains a Ratio

(rte/match '(:cat (:* Number) Ratio (:* Number))
           demo-seq)


;; what about a seqence of numbers which contains a Double

(rte/match '(:cat (:* Number) Double (:* Number))
           demo-seq)


;; what about a seqence of numbers which contains a Double AND a Ratio
;; in either order
(rte/match '(:or (:cat (:* Number) Double (:* Number) Ratio (:* Number))
                 (:cat (:* Number) Ratio (:* Number) Double (:* Number)))
           demo-seq)

(rte/match '(:and (:cat (:* Number) Double (:* Number))
                  (:cat (:* Number) Ratio (:* Number)))
           demo-seq)


;; First way to express seq of Number which contains both a Ratio
;; and a Double in either order.
(def rte-1 '(:and (:cat (:* Number) Double (:* Number))
                  (:cat (:* Number) Ratio (:* Number))))

(dot/dfa-to-dot rte-1 :title "first dfa"
                :view true
                :state-legend false)


;; Second way to express seq of Number which contains both a Ratio
;; and a Double in either order.

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


;; Third way of expression the same thing (with suble error)

(def rte-3 '(:and (:* Number)
                  (:cat (:* :sigma) Ratio (:* :sigma))
                  (:cat (:+ :sigma) Double (:* :sigma))))

(dot/dfa-to-dot rte-3 :title "third dfa" 
                :view true
                :state-legend false)

(xym/find-trace-map (rte/rte-to-dfa (rte/Xor rte-1 rte-3)))

(dot/dfa-to-dot (rte/rte-to-dfa (rte/Xor rte-1 rte-3))
                :title "xor 1-3 dfa"
                :state-legend false
                :view true)

;; a sequence of numbers which contains an odd and a Ratio
(def rte-4 '(:and (:* Number)
                  (:cat (:* :sigma) Ratio (:* :sigma))
                  (:cat (:+ :sigma) (satisfies odd?) (:* :sigma))))

(xym/find-trace-map (rte/rte-to-dfa rte-4))

(dot/dfa-to-dot rte-4
                :title "odd and ratio"
                :state-legend false
                :view true)

;; a sequence of numbers which contains an integer and a Ratio

(def rte-5 '(:and (:* Number)
                  (:cat (:* :sigma) Ratio (:* :sigma))
                  (:cat (:+ :sigma) (satisfies int?) (:* :sigma))))

(xym/find-trace-map (rte/rte-to-dfa rte-5))

(dot/dfa-to-dot rte-5
                :title "intr and ratio"
                :state-legend false
                :view true)


;; show example of expansion of (satisfies integer?)

(typecase 12
  Short "it is a short"
  Boolean "it is a boolean"
  (satisfies int?) "it is an fixed-width integer"
  (or Ratio Boolean) "it is a ratio"
  String "it is a string")


