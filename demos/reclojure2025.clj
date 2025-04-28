(ns reclojure2025
  (:require [rte-case :refer [rte-case dscase rte-case-fn dsdefn]]
            [rte-construct :as rte]
            [xymbolyco :as xym]
            [typecase :refer [typecase]]
            [dot])
)











;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;        _          __               _       _     
;;   _ __| |_ ___   / / __ ___   __ _| |_ ___| |__  
;;  | '__| __/ _ \ / / '_ ` _ \ / _` | __/ __| '_ \ 
;;  | |  | ||  __// /| | | | | | (_| | || (__| | | |
;;  |_|   \__\___/_/ |_| |_| |_|\__,_|\__\___|_| |_|
;;                                                
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; we want to match a sequence of numbers.

(def demo-seq-1 [1 2 4/3 4      5 6.5 3 2])
(def demo-seq-2 [1 2 4/3 "four" 5 6.5 3 2])


(rte/match '(:* Number)
           demo-seq-1)

(instance? Number 10)


(rte/match '(:* Number)
           demo-seq-2)


(rte/match '(:* String)
           demo-seq-1)

(rte/match '(:* Ratio)
           demo-seq-1)


;; What about a seqence of numbers which contains a Ratio

(rte/match '(:and (:* Number)
                  (:cat (:* :sigma) Ratio (:* :sigma)))
           demo-seq-1)

(rte/match '(:cat (:* Number) Ratio (:* Number))
           demo-seq-1)



;; What about a seqence of numbers which contains a Double

(rte/match '(:cat (:* Number) Double (:* Number))
           demo-seq-1)

(rte/match '(:cat (:* Number) Float (:* Number))
           demo-seq-1)



;; what about a seqence of numbers which contains a Double AND a Ratio
;; in either order.  

;; First way to express seq of Number which contains both a Ratio
;; and a Double
(def rte-1 '(:and (:cat (:* Number) Double (:* Number))
                  (:cat (:* Number) Ratio (:* Number))))

(rte/match rte-1 demo-seq-1)










;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       _  __                  _               
;;    __| |/ _| __ _     __   _(_) _____      __
;;   / _` | |_ / _` |____\ \ / / |/ _ \ \ /\ / /
;;  | (_| |  _| (_| |_____\ V /| |  __/\ V  V / 
;;   \__,_|_|  \__,_|      \_/ |_|\___| \_/\_/  
;;                                              
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(dot/dfa-view rte-1 "Dfa rte-1")


;; Second way to express seq of Number which contains both a Ratio
;; and a Double in either order.

(def rte-2 '(:or (:cat (:* Number) Double (:* Number) Ratio (:* Number))
                 (:cat (:* Number) Ratio (:* Number) Double (:* Number))))

(dot/dfa-view rte-2 "Dfa rte-2" )

;; are the rtes the same?  do have match the same language?

(dot/dfa-view (rte/Xor rte-1 rte-2)
              "xor 1-2 dfa")                         


;; Third way of expression the same thing (with suble error)

(def rte-3 '(:and (:* Number)
                  (:cat (:* :sigma) (or Ratio Double) (:* :sigma))))

(dot/dfa-view rte-3 "third dfa")












;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    __ _           _       _                                                  
;;   / _(_)_ __   __| |     | |_ _ __ __ _  ___ ___       _ __ ___   __ _ _ __  
;;  | |_| | '_ \ / _` |_____| __| '__/ _` |/ __/ _ \_____| '_ ` _ \ / _` | '_ \ 
;;  |  _| | | | | (_| |_____| |_| | | (_| | (_|  __/_____| | | | | | (_| | |_) |
;;  |_| |_|_| |_|\__,_|      \__|_|  \__,_|\___\___|     |_| |_| |_|\__,_| .__/ 
;;                                                                       |_|    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; describe a sequence which matches exactly one of the two RTEs


(xym/find-trace-map (rte/rte-to-dfa (rte/Xor rte-1 rte-3)))

(dot/dfa-view (rte/rte-to-dfa (rte/Xor rte-1 rte-3))
              "xor 1-3 dfa")

;; a sequence of numbers which contains an odd and a Ratio
(def rte-4 '(:and (:* Number)
                  (:cat (:* :sigma) Ratio (:* :sigma))
                  (:cat (:+ :sigma) (satisfies odd?) (:* :sigma))))

(xym/find-trace-map (rte/rte-to-dfa (rte/And-not rte-1 rte-4)))

(dot/dfa-view (rte/And-not rte-1 rte-4)
              "rte and-not")

;; A sequence of numbers which contains an integer and a Ratio
;; In this case the int? predicate disappears because
;; the code is known and can be inlined.
(def rte-5 '(:and (:* Number)
                  (:cat (:* :sigma) Ratio (:* :sigma))
                  (:cat (:+ :sigma) (satisfies int?) (:* :sigma))))

(xym/find-trace-map (rte/rte-to-dfa rte-5))

(dot/dfa-view rte-5 "int and ratio")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   _                                       
;;  | |_ _   _ _ __   ___  ___ __ _ ___  ___ 
;;  | __| | | | '_ \ / _ \/ __/ _` / __|/ _ \
;;  | |_| |_| | |_) |  __/ (_| (_| \__ \  __/
;;   \__|\__, | .__/ \___|\___\__,_|___/\___|
;;       |___/|_|                            
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A multi-branch type can can be compiled in an efficient way
;;   so as to avoid checking the same type multiple times.



;; Example of expansion of (satisfies int?)

(typecase 12
  Short "it is a short"
  Boolean "it is a boolean"
  (satisfies int?) "it is an fixed-width integer"
  (or Ratio Boolean) "it is a ratio"
  String "it is a string")





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  
;;                   _ _   _                  _ _         
;;   _ __ ___  _   _| | |_(_)       __ _ _ __(_) |_ _   _ 
;;  | '_ ` _ \| | | | | __| |_____ / _` | '__| | __| | | |
;;  | | | | | | |_| | | |_| |_____| (_| | |  | | |_| |_| |
;;  |_| |_| |_|\__,_|_|\__|_|      \__,_|_|  |_|\__|\__, |
;;                                                  |___/ 
;;  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
;;        _                                
;;   _ __| |_ ___        ___ __ _ ___  ___ 
;;  | '__| __/ _ \_____ / __/ _` / __|/ _ \
;;  | |  | ||  __/_____| (_| (_| \__ \  __/
;;  |_|   \__\___|      \___\__,_|___/\___|
;;                 
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       _         _       __       
;;    __| |___  __| | ___ / _|_ __  
;;   / _` / __|/ _` |/ _ \ |_| '_ \ 
;;  | (_| \__ \ (_| |  __/  _| | | |
;;   \__,_|___/\__,_|\___|_| |_| |_|
;;                                  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


