;; Copyright (c) 2020 EPITA Research and Development Laboratory
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without restriction,
;; including without limitation the rights to use, copy, modify, merge,
;; publish, distribute, sublicense, and/or sell copies of the Software,
;; and to permit persons to whom the Software is furnished to do so,
;; subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(ns clojure-rte.util
  (:require [clojure.pprint :refer [cl-format]]))

(defn with-first-match 
  "Find the first element in the given sequence, items,
   for which the predicate, pred, returns Boolean true.
   If such is found, call the continuation with the
   element.  This type of 'finder' avoids the problem of
   deciding whether nil was the value found.  The continuation
   is only called on the found value."
  [pred items continuation]

  (loop [items items]
    (cond (empty? items)
          nil

          (pred (first items))
          (continuation (first items))

          :else
          (recur (rest items)))))

(defn remove-once 
  "Non-destructively remove the first element of the sequence which is
   = to the target value.  The list is unrolled as much as necessary,
   to remove the target value, and then the leading values are
   prepended, via concat, to the beginning of the remaining sequence.
   The tail of the sequence after finding the target is not examined,
   in case it is lazy.  If the taget does not appear in the list, a
   copy of the sequence is returned.  If the target item appears more
   than once, we have no way of knowing, and only the first such
   occurance is removed."
  [target items]

  (loop [items items
         acc ()]
    (cond
      (empty? items)
      (reverse acc)

      (= (first items) target)
      (concat (reverse acc) (rest items))

      :else
      (recur (rest items) (cons (first items) acc)))))

(defn call-with-collector
  "This function calls your given function which an argument which can be
   called to collect values.  The return value of call-with-collector is
   the list of items collected, in reverse order.  E.g.,
   (call-with-collector (fn [collect] 
                            ...body...))

   Within the body, collect is a unary function which can be called
   zero or more times.  The arguments are collected and returned as a
   in reverse order as if they were cons-ed onto an internal list.
   The caller is responsible for reversing the list if necessary."
  [unary-client]

  (with-local-vars [data '()]
    (unary-client (fn [obj]
                    (var-set data (cons obj @data))))
    @data))

(defn visit-permutations 
  "Call the given unary-client function once on each permutation
   of the given sequence of items.  Warning, there are n! many
   such permutations, so this function will be extremely slow
   if the (count items) is large.  If you want to return a list 
   of permutations, use visit-permutations in conjunction 
   with call-with-collector.
   (call-with-collector
     (fn [collect]
       (visit-permutations collect items)))"
  [unary-client items]
  
  (letfn [(visit-with-tail [remaining tail]
            (if (empty? remaining)
              (unary-client tail)
              (doseq [item remaining]
                (visit-with-tail (remove-once item remaining)
                                 (cons item tail)))))]
    (visit-with-tail items '())))

(defn rte-constantly
  "Return a binary function, similar to constanty, but the binary
   function ignores its second argument.  This function is useful as a
   callback function used to extend *traversal-functions*, as each
   such callback function must be a binary function."
  [x]
  (fn [_ _]
    x))

(defn rte-identity 
  "Similar to clojure.core.identity, except that this version is
   binary and always ignors its second argument.  This function is
   useful as a callback function used to extend *traversal-functions*,
   as each such callback function must be a binary function."
  [x _y]
  
  x)

(def problematic-operands (atom ()))

(defn sort-operands
  "Sort the given list of operands into deterministic order, making it possible
  to easily find identical elements, and to write test cases."
  [operands]
  (letfn [(cmp [a b]
            (cond
              (and (sequential? a)
                   (sequential? b))
              (loop [aa a
                     bb b]
                (cond
                  (and (empty? aa)
                       (empty? bb)) (compare (.getName (type a))
                                             (.getName (type b)))
                  (empty? aa) -1
                  (empty? bb) 1

                  (= (first aa) (first bb))   (recur (rest aa) (rest bb))
                  :else     (cmp (first aa) (first bb))))

              (= a b) (compare (.getName (type a))
                               (.getName (type b)))

              (sequential? a)
              -1

              (sequential? b)
              1
              
              (not (= (.getName (type a))
                      (.getName (type b))))
              (compare (.getName (type a))
                       (.getName (type b)))

              :else
              (compare a b)))]
    (try (sort cmp operands)
         (catch Exception e
           ;; were were getting a complaint from TIM sort.
           ;; however the error message did not give the offending list.
           ;; this code attempts to save the list in case TIM sort fails so we
           ;; can debug it.
           (swap! problematic-operands (fn [_] operands))
           (printf "saving problematic operands in *problematic-operands*\n")
           (println [:type (type operands)
                     :types (seq (map type operands))
                     :operands operands                     
                     :seq (seq operands)])
           (throw e)))))

(defn member
  "Determines whether the given target is an element of the given sequence."
  [target items]
  (boolean (cond
             (empty? items) false
             (nil? target) (some nil? items)
             (false? target) (some false? items)
             :else (reduce (fn [_acc item]
                             (if (= item target)
                               (reduced true)
                               false)) false items))))

(defn partition-by-pred 
  "Apply the predicate to every element of the sequence and return a vector of two
  values, each of which is a vector, the left vector is the set of the items
  in the original sequence for which the predicate returned a Boolean true value,
  the right vector are the other values from the given sequence."  
  [pred items]

  (reduce (fn [[good bad] item]
            (if (pred item)
              [(conj good item) bad]
              [good             (conj bad item)]))
          [[] []]
          items))

(defn find-simplifier
  "Iterate through a sequence of so-called simplifies.  Each simplifier
  is a unary function which accepts the given obj as arguments.  Each
  simplifier is expected to either _simplify_ the object or leave it as-is.
  If the simplifier returns an object which is = obj, then we assume the
  simplifier chose NOT to simplify, in which case find-simplifier proceeds
  to the next simplifier in the sequence.
  If, however, the simplifier returns an object which is not = to obj, 
  then we assume that the simplifier HAS simplified the object.
  WARNING, find-simplifier does not in any way verify that the object
  is more simple than before; it only checks whether it is different.
  Once a simplifier has _simplified_ an object, find-simplifier returns
  the newly generated object, and the remaining simplifiers are silently
  ignored."
  [obj simplifiers]
  (if (empty? simplifiers)
    obj
    (loop [[f & fs] simplifiers]
      (let [new-obj (f obj)]
        (cond
          (not= new-obj obj)
          new-obj

          (empty? fs)
          obj

          :else
          (recur fs))))))

(defn fixed-point
  "Find the fixed point of the given function starting at the given value.
  Convergence is detected when the function good-enough returns Boolean true
  when called with two consecutive values. The older of the two values
  is returned in this case."
  [value f good-enough]
  (loop [value value]
    (let [new-value (f value)]
      (if (good-enough value new-value)
        value
        (recur new-value)))))

(defn print-vals-helper ""
  [pairs]
  (let [N (count pairs)]
    (loop [val nil
           n 1
           pairs pairs]
      (if (empty? pairs)
        val
        (let [[[thunk1 e1] & pairs] pairs]
          (cl-format true "~A/~A ~A~%~T -> "
                     n N e1)
          (let [v1 (thunk1)]

            (cl-format true "~A~%"
                       v1)
            (recur v1 (inc n) pairs)))))))

(defmacro print-vals ""
  [& args]
  (let [pairs (into [] (map (fn [arg]
                              `[(fn [] ~arg) '~arg]) args))]
    `(print-vals-helper [~@pairs])))

(defn group-by-mapped
  "Like group-by but allows a second function to be mapped over each
  of the values in the computed hash map."
  [f1 f2 coll]
  (into {} (map (fn [[key value]]
                  [key (set (map f2 value))]) (group-by f1 coll))))


(defn mapc
  "Like map but does not accumulate the return values.  Returns its second argument."
  [f seq]
  (doseq [i seq]
    (f i))
  seq)

(defn assert-type 
  "Check the given value asserting it to be of the given type.
  If the type check is satisfied, the value is returned, otherwise
  an exception is throw.  This function can be used inline to
  assert types without changing the control flow."
  [type value]
  (if (instance? type value)
    value
    (throw (ex-info (cl-format false "value ~A expected to be ~A, not ~A"
                               value type (type value))
                    {:expected-type type
                     :actual-type (type value)
                     :value value}))))

(defmacro defn-memoized
  [[public-name internal-name] docstring & body]
  (assert (string? docstring))
  `(let []
     (declare ~public-name) ;; so that the internal function can call the public function if necessary
     (defn ~internal-name ~@body)
     (def ~(with-meta public-name {:dynamic true}) ~docstring (memoize ~internal-name))
     ))

(defn map-eagerly 
  "Like map, but forces non-lazy behavior"
  [& map-args]
  (doall (apply map map-args)))

(defn mapcat-eagerly 
  "Like mapcat, but forces non-lazy behavior"
  [& mapcat-args]
  (doall (apply mapcat mapcat-args)))

(defn filter-eagerly 
  "Like filter, but forces non-lazy behavior"
  [& filter-args]
  (doall (apply filter filter-args)))

(defn remove-eagerly 
  "Like remove, but forces non-lazy behavior"
  [& remove-args]
  (doall (apply remove remove-args)))

(defn concat-eagerly 
  "Like concat, but forces non-lazy behavior"
  [& concat-args]
  (doall (apply concat concat-args)))

(defn dedupe-eagerly 
  "Like dedupe, but forces non-lazy behavior"
  [& dedupe-args]
  (doall (apply dedupe dedupe-args)))

(defmacro exists
  "Test whether there exists an element of a sequence which matches a condition."
  [[var seq] & body]
  `(some (fn [~var]
           ~@body) ~seq))

(defmacro setof
  "Return a sequence of lazy elements of a given sequence which match a given condition"
  [[var seq] & body]
  `(filter (fn [~var]
             ~@body) ~seq))

(defmacro forall
  "Return true if the given body evaluates to logical true for every element of the given sequence"
   [[var seq] & body]
  `(every? (fn [~var]
             ~@body) ~seq))

(defn lazy-pairs [seq]
  (cond (empty? seq)
        ()

        :else
        (concat (for [t (rest seq)]
                  [(first seq) t])
                (lazy-pairs (rest seq)))))

(defmacro forall-pairs [[[v1 v2] seq] & body]
  `(every? (fn [[~v1 ~v2]] ~@body) (lazy-pairs ~seq)))

(defmacro exists-pair  [[[v1 v2] seq] & body]
  `(some (fn [[~v1 ~v2]]
           ~@body) (lazy-pairs ~seq)))

(defn capture-output
  "Call the given 0-ary function, returning a vector of length 2.
   The first element is the value returned from thunk.
   The second element is the string comprising all the output
   printed to *out*, or \"\" if it printed nothing."
  [thunk]
  (let [a (atom nil)]
    (reverse [(with-out-str (reset! a (thunk)))
              @a])))

(defn call-diverting-stdout
  "call the given 0-ary thunk, returning its return value.
  If anything is printed to *out* during the dynamic extent
  of thunk, it will be captured into a string.  diversion-f
  will be called on that string (once).  If nothing was printed
  then diversion-f will be called with empty string."
  [thunk diversion-f]
  (let [[value str] (capture-output thunk)]
    (diversion-f str)
    value))

(defn non-empty?
  "like not-empty, but returns boolean rather than nil, or the collection"
  [coll]
  (boolean (not-empty coll)))

(defn seq-matcher
  "Return a function, a closure, which can be used to determine whether
  its argument is a sequence whose first element is identically the
  given obj."
  [target]
  (fn [obj]
    (and (seq? obj)
         (not-empty obj)
         (= target (first obj)))))

(defn -condp-helper [test value default-f & pairs]
  (loop [pair pairs]
    (cond (empty? pairs)
          (default-f)

          :else
          (let [[keys f] (first pairs)]
            (if (exists [x keys] (test value x))
              (f)
              (recur (rest pairs)))))))

(defmacro -condp [test obj & pairs]
  (loop [pairs pairs
         default (fn [] nil)
         acc ()]
    (cond (empty? pairs)
          `(-condp-helper ~test ~obj ~default ~@(reverse acc))

          (empty? (rest pairs))
          (recur ()
                 `(fn [] ~(first pairs))
                 acc)

          :else
          (let [key (first pairs)
                value (second pairs)]
            (recur (rest (rest pairs))
                   default
                   (cons `['~key (fn [] ~value)] acc)
                 )))))
