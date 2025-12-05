(ns util.strong)


(declare strong-equal?)
(declare strong-set-equal?)
(declare strong-map-equal?)
(declare strong-member?)
(declare strong-search-replace)
(declare strong-remove-element)

(defn strong-search-replace
  "Replace every occursnce of `search-for` with `replace-with`
  in the given sequence `xs`"
  [xs search-for replace-with]

  (letfn [(ssr [xs search-for replace-with]
            (cond (not (strong-member? search-for xs))
                  xs
                  
                  (empty? xs)
                  xs
                  
                  (strong-equal? (first xs) search-for)
                  (cons replace-with (ssr (rest xs) search-for replace-with))
                  
                  :otherwise
                  (cons (first xs) (ssr (rest xs) search-for replace-with))))]
    (ssr (doall xs) search-for replace-with)))

(defn strong-remove-element
  "Non-destructively remove a given element from a sequence, every time it occurs"
  [element-to-remove xs]
  (letfn [(sre [element-to-remove xs]
            #_{:pre [(if (list? xs)
                     true
                     (do (pprint [:xs xs :type-xs (type xs)]) false))]
             :post [(if (= (type xs) (type %))
                      true
                      (do (pprint [:xs xs :% % :type-xs (type xs) :type-% (type %)]) false))]}
            (loop [xs xs]
              (cond (empty? xs)
                    xs

                    (strong-equal? (first xs) element-to-remove)
                    (recur (rest xs))

                    :otherwise
                    (cons (first xs) (sre element-to-remove (rest xs))))))]
    (sre element-to-remove (doall xs))))


(defn strong-set-equal?
  "Predicate to determine whether two sets are equal using strong-equal?
  as the leaf level equal predicate."
 [s1 s2]
  (assert (set? s1))
  (assert (set? s2))
  (and (= (count s1) (count s2))
       (every? #(strong-member? % s2) s1)))

(defn strong-map-equal?
  "Predicate to determine whether two maps are equal using strong-equal?
  as the leaf level equal predicate."
  [m1 m2]
  (assert (map? m1))
  (assert (map? m2))
  (and (= (count m1) (count m2))
       (let [keys-m1 (keys m1)]
         (every? (fn [[k2 v2]]
                   (and (strong-member? k2 keys-m1)
                        (strong-equal? v2 (get m1 k2))))
                 m2))))

(defn strong-equal?
  "A stronger interpretation of equivalence than the built-in clojure =.
  The idea is that sequences of different types are not equal.
  I.e., (1 2 3) and [1 2 3] are not the same.
  But more subtile is that this test is recursive.  I.e.,
  (1 2 [3]) and (1 2 (3)) are also not equal.
  The drawback of this predicate is that sometimes clojure creates
  objects which the user might think are the same type, but are really
  different.  For example: 
    (= (type (zipmap (range 8) (range 8)))
       (type (dissoc (zipmap (range 9) (range 9)) 8))) ;; false"
  [a b]
  (letfn [(cmp-elements [a b]
            (loop [as a
                   bs b]
              (cond (and (empty? as)
                         (empty? bs))
                    true

                    (or (empty? as)
                        (empty? bs))
                    false

                    :otherwise
                    (and (strong-equal? (first as) (first bs))
                         (recur (rest as) (rest bs))))))]

    (cond (identical? a b)
          true

          (and (seq? a) (seq? b))
          (cmp-elements a b)

          (and (vector? a) (vector? b))
          (cmp-elements a b)

          (not (identical? (type a) (type b)))
          false

          (not (seqable? a))
          (= a b)

          (map? a)
          (strong-map-equal? a b)

          (set? a)
          (strong-set-equal? a b)

          :otherwise
          (cmp-elements a b))))

(defn strong-member? 
  "membership predicate based on strong-equal? rather than on clojure.core/="
  [target items]
  (boolean (some #(strong-equal? target %) items)))
                 
(defn strong-uniquify
  "returns a new sequence with duplicates remove.
   If duplicates exist, left-most is removed, right-most remains.
   Equivalent elements are judged by strong-equal?"
  [items]
  (loop [items items]
    (cond (empty? items)
          items

          (strong-member? (first items) (rest items))
          (recur (rest items))

          :otherwise
          (cons (first items) (strong-uniquify (rest items))))))
