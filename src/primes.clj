(ns primes
  (:require [clojure.pprint :refer [pprint]]
            [clojure.math :refer [sqrt round pow]]))



(defn prime-factorization
  ([n]
   (prime-factorization n 2 (sqrt n) []))
  ([n lower upper factors]
   (cond (> lower upper)
         (conj factors n)

         (= 0 (mod n lower))
         (let [n (/ n lower)]
           (recur n lower (sqrt n) (conj factors lower)))

         :else
         (recur n (inc lower) upper factors))))

(prime-factorization (* 2 2 13 7 17 3 5 5 17))

(defn primes
  ([n]
   (assert (> n 1))
   (primes n 3 [2]))
  ([n lower found]
   (cond (> lower n)
         (into () (reverse found))

         (some (fn [p] (= 0 (mod lower p)))
               found)
         (recur n (+ 2 lower) found)

         :else
         (recur n (+ 2 lower) (conj found lower)))))


(defn encode-as-factorization [n]
  (let [factorization (frequencies (prime-factorization n))
        max-prime (reduce max (keys factorization))
        primes (primes max-prime)]
    (map #(get factorization % 0) primes)))

;; (encode-as-factorization (* 2 2 3 3 3 5 13 17))
          
  
;;  2: (1)
;;  3: (0 1)
;;  4: (2)
;;  5: (0 0 1)
;;  6: (1 1)
;;  7: (0 0 0 1)
;;  8: (3)
;;  9: (0 2)
;; 10: (1 0 1)
;; 11: (0 0 0 0 1)
;; 12: (2 1)

(defn a|b [a b]
  (cond (= a b)
        true

        (and (not-empty a)
             (not-empty b)
             (< (first a) (first b)))
        (recur (rest a) (rest b))

        (empty? a)
        true

        :else
        false))


(defn generate-constraints [n]
  (let [n-primes (primes n)
        ;; p1 < p2
        constraints-1 (map (fn [p q] (list (encode-as-factorization p)
                                           (encode-as-factorization q)
                                           :1))
                           (butlast n-primes) (rest n-primes))

        ;; primes px < py ==> px^n < px^n
        constraints-2 (for [n (range 2 10)
                            [a b & _] constraints-1]
                        (list (map #(* n %) a)
                              (map #(* n %) b)
                              :2))

        ;; a < ab
        constraints-3 (for [a (range 2 n)
                            ab (range (inc a) n)
                            :when (= 0 (mod ab a))]
                        (list (encode-as-factorization a)
                              (encode-as-factorization ab)
                              :3))
        ]
    (concat constraints-1
            ;;constraints-2
            constraints-3)))
                            
;; (pprint (generate-constraints 12))

(defn topological-sort 
  ([items a<b]
   (topological-sort items a<b []))
  ([items a<b sorted-so-far]
   (let [unconstrained (filter (fn [i] (not-any? (fn [[a b]] (= i b)) a<b)) items)
         hit-count (bounded-count 2 unconstrained)]
     (cond (empty? items)
           [sorted-so-far :sorted]

           (= 0 hit-count)
           [sorted-so-far :over-constrained] ;; no order exists

           (> 1 hit-count)
           [sorted-so-far :under-constrained] ;; multiple orders exist

           :otherwise
           (let [found (first unconstrained)
                 items (filter #(not= % found) items)
                 a<b (filter (fn [[a b]] (not= a found)) a<b)]

             (recur items a<b (conj sorted-so-far found)))))))


;; (topological-sort [1 2 3] [[1 2] [2 3] [3 4]])


(let [n 100]
  (topological-sort (shuffle (map encode-as-factorization (range 2 n)))
                    (generate-constraints n)))
