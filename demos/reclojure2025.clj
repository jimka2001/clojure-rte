(ns reclojure2025
  (:require [rte-case :refer [rte-case dscase rte-case-fn dsdefn]]
            [rte-construct :as rte]
            [xymbolyco :as xym]
            [typecase :refer [typecase]]
            [dot])
)








;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  rte/match
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; we want to match a sequence of numbers.

(def demo-seq [1 2 4/3 4 5 6.5 3 2])

(rte/match '(:* Number)
           demo-seq)

(instance? Number 10)

(rte/match '(:* String)
           demo-seq)

(rte/match '(:* Ratio)
           demo-seq)


;; What about a seqence of numbers which contains a Ratio

(rte/match '(:cat (:* Number) Ratio (:* Number))
           demo-seq)


;; What about a seqence of numbers which contains a Double

(rte/match '(:cat (:* Number) Double (:* Number))
           demo-seq)

(rte/match '(:cat (:* Number) Float (:* Number))
           demo-seq)



;; what about a seqence of numbers which contains a Double AND a Ratio
;; in either order.  

;; First way to express seq of Number which contains both a Ratio
;; and a Double
(def rte-1 '(:and (:cat (:* Number) Double (:* Number))
                  (:cat (:* Number) Ratio (:* Number))))

(rte/match rte-1 demo-seq)


(dot/dfa-to-dot rte-1 :title "Dfa rte-1"
                :view true
                :dot-file-cb println
                :state-legend false)


;; Second way to express seq of Number which contains both a Ratio
;; and a Double in either order.

(def rte-2 '(:or (:cat (:* Number) Double (:* Number) Ratio (:* Number))
                 (:cat (:* Number) Ratio (:* Number) Double (:* Number))))

(dot/dfa-to-dot rte-2 :title "Dfa rte-2" 
                :view true
                :state-legend false)

;; are the rtes the same?  do have match the same language?

(dot/dfa-to-dot (rte/Xor rte-1 rte-2)
                :title "xor 1-2 dfa"
                :state-legend false
                :view true)                         


;; Third way of expression the same thing (with suble error)

(def rte-3 '(:and (:* Number)
                  (:cat (:* :sigma) (or Ratio Double) (:* :sigma))))

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

(xym/find-trace-map (rte/rte-to-dfa (rte/And-not rte-1 rte-4)))

(dot/dfa-to-dot (rte/And-not rte-1 rte-4)
                :title "rte and-not"
                :state-legend false
                :view true)

;; A sequence of numbers which contains an integer and a Ratio
;; In this case the int? predicate disappears because
;; the code is known and can be inlined.
(def rte-5 '(:and (:* Number)
                  (:cat (:* :sigma) Ratio (:* :sigma))
                  (:cat (:+ :sigma) (satisfies int?) (:* :sigma))))

(xym/find-trace-map (rte/rte-to-dfa rte-5))

(dot/dfa-to-dot rte-5
                :title "int and ratio"
                :state-legend false
                :view true)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Multi-arity functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A multi-branch type can can be compiled in an efficient way
;;   so as to avoid checking the same type multiple times.

;; Example of expansion of (satisfies int?)

(typecase 12
  Short "it is a short"
  Boolean "it is a boolean"
  (satisfies int?) "it is an fixed-width integer"
  (or Ratio Boolean) "it is a ratio"
  String "it is a string")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Multi-arity functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defn f
  ([a]   12)
  ([^Ratio a b]   13)
  ([a b [c d]]   14))


(defn f
  ([a]   12)
  ([a b]   13)
  ([a b [^int? c ^Ratio d]]   14))

(f 0 1 [3 52/17])       ;; --> 14
(f 0 1 ["hello" false]) ;; --> 14


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rte-case
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Functions defined by dsdefn
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(dsdefn f 
  ([[a b] c d] 12)
  ([a [b c] d] 13)
  ([a b [c d]] 14))

(f [0 1] 2 3)  ;; --> 12
(f 0 [1 2] 3)  ;; --> 13
(f 0 1 [2 3])  ;; --> 14



(dsdefn f 
  ([[a b] c d] 12)
  ([a [b c] d] 13)
  ([a b [c d]] 14)
  ([a b [c d]] (println "fourth clause") 15))


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
  ([a b [^int? c d]] 15)
  ([a b [^Number c d]] 16)
  ([a b [^Double c d]] 17))

(dsdefn f 
  ([[a b] c d] 12)
  ([a [b c] d] 13)
  ([a b [^Ratio c d]] 14)
  ([a b [^(satisfies int?) c d]] 15)
  ([a b [^Number c d]] 16)
  ([a b [^Double c d]] 17))

(dsdefn f 
  ([[a b] c d] 12)
  ([a [b c] d] 13)
  ([a b [^Ratio c d]] 14)
  (^{c (satisfies int?)} [a b [c d]] 15)
  ([a b [^Double c d]] 16))

(f 1 2 [10 20])
(f 1 2 [10.0 20])
(f 1 2 [10/3 20])
(f 1 2 [false 20])

;; In selecting which clause to evaluate, a computation is done using a
;; Dfa.  The Dfa is build by the macro expansion (when the macro expansion
;; is evaluated).  When the function f is called the Dfa is traversed
;; once to determine which piece of code to evalu

(dsdefn f 
  ([^Number a b] 14)
  (^{a Boolean
     c (satisfies int?)} [a b c d] 15)
  ( [a b ^Double c d] 16))


