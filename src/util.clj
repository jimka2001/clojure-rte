;; Copyright (c) 2020,21 EPITA Research and Development Laboratory
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

(ns util
  (:require [clojure.pprint :refer [cl-format]]
            [clojure.core.memoize :as m]
            [clojure.core.cache :as c]))

(defn remove-element
  "Non-destructively remove a given element from a sequence"
  [element-to-remove xs]
  (filter (fn remove-element-filter [x] (not= x element-to-remove)) xs)) 

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


              ;; if we are sorting a sequence which contains objects of type
              ;; java.lang.Class, then the underlying java function `compare`
              ;; does not implement Comparable for this type.
              (or (not (isa? (type a) Comparable))
                  (not (isa? (type b) Comparable)))
              (compare (str a) (str b))

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

(defn stacksize
  "return the depth of the stack.  This function is useful for debugging
  to detect if two recursive calls are at the same depth."
  ;; This code is copied from https://gist.github.com/devth/8865799
  ;; Thanks to https://gist.github.com/devth
  []
  (try (throw (Exception. ""))
       (catch Exception e (count (.getStackTrace e)))))

(defn find-simplifier
  "Iterate through a sequence of so-called simplifiers.  Each simplifier
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
  ([obj simplifiers]
   (find-simplifier obj simplifiers false))
  ([obj simplifiers verbose]
   ;;(cl-format true "~A ~A~%" (stacksize) obj)
   (if (empty? simplifiers)
     obj
     (loop [[f & fs] simplifiers
            i 0]
       (let [new-obj (f obj)]
         (cond
           (not= new-obj obj)
           (do
             (when verbose
               (cl-format true "[~A] ~A~%   --> ~A~%" i obj new-obj))
             new-obj)

           (empty? fs)
           obj

           :else
           (recur fs (inc i))))))))


(defn fixed-point
  "Find the fixed point of the given function starting at the given value.
  Convergence is detected when the function good-enough returns Boolean true
  when called with two consecutive values. The older of the two values
  is returned in this case."
  ([value f good-enough]
   (fixed-point value f good-enough false))
  ([value f good-enough verbose]
  (loop [value value
         old-values ()]
    (let [new-value (f value)]
      (when verbose
        (cl-format true "reducing ~A~%" value)
        (cl-format true "   to -> ~A~%" new-value))
      (cond (good-enough value new-value)
            value

            (member value old-values)
            (throw (ex-info (cl-format nil "infinite loop in fixed-point, twice encountered ~A" value)
                            {:value value
                             :old-values old-values}))

            :else
            (recur new-value (cons value old-values)))))))

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

(defn gc-friendly-memoize
  [g]
  (m/memoizer g (c/soft-cache-factory {})))

(defmacro defn-memoized
  [[public-name internal-name] docstring & body]
  (assert (string? docstring))
  `(do
     (declare ~public-name) ;; so that the internal function can call the public function if necessary
     (defn ~internal-name ~@body)
     (def ~(with-meta public-name {:dynamic true}) ~docstring (gc-friendly-memoize ~internal-name))
     ))

(def memoized-multis (atom {}))
(defmacro defmulti-memoized
  "Define a multimethod on an internal name, and a memoized function implemented
   as a dynamic variable.  Methods must be defined using defmethod-memoized, using
   the public-name."
  [[public-name internal-name] docstring dispatch-fn]

  (assert (string? docstring))
  `(do
     (declare ~public-name) ;; so that the methods can call the public function if necessary
     (defmulti ~internal-name ~dispatch-fn)
     (def ~(with-meta public-name {:dynamic true})
       ~docstring
       (gc-friendly-memoize ~internal-name))
     (swap! memoized-multis assoc '~public-name '~internal-name)))

(defmacro defmethod-memoized
  "Wrapper around defmethod which defines a method using the internal name of the given
  public name.  The pairing was presumably made in a previous call to defmulti-memoized."
  [public-name dispatch-val & fn-tail]
  (assert (get @memoized-multis public-name)
          (cl-format false "~A does not name a multimethod defined by defmulti-memoized"
                     public-name))
  `(defmethod ~(get @memoized-multis public-name) ~dispatch-val ~@fn-tail))

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
  `(some (fn ~'exists-some [~var]
           ~@body) ~seq))

(defmacro setof
  "Return a sequence of lazy elements of a given sequence which match a given condition"
  [[var seq] & body]
  `(filter (fn ~'setof-filter [~var]
             ~@body) ~seq))

(defmacro forall
  "Return true if the given body evaluates to logical true for every element of the given sequence"
   [[var seq] & body]
  `(every? (fn ~'forall-every [~var]
             ~@body) ~seq))

(defn lazy-pairs
  ;; code provided by Eugene Pakhomov
  ;; https://app.slack.com/client/T03RZGPFR/C03S1KBA2/user_profile/U2FRKM4TW
  "Generate (lazily) a sequence of pairs (a b) from the given sequence such
  that a is always to the left of b in the given sequence.   Supposing that
  the given sequence has no duplicates (1 2 3) -> ((1 2) (1 3) (2 3)).
  The order of the list of pairs is not guaranteed, however the following
  property holds.
  At the point in the sequence where [a b] occurs, every pair [x y] thereafter
  has the property that either max(x,y) >= max(a,b). E.g every pair containing
  both entries less than 13 appears before the first occurance of 13.
  If the sequence contains [4 13], then every element [a b] to the left has
  both a and b <= 13 and no [a b] after [4 13] has both a and b < 13.
  This ordering property ensures that if the given collection is a lazy
  sequence, that as few items as possible are realized.  I.e., if the first
  n items have been realized, then all pairs involving the first n items
  have been generated before the n+1'th item gets realized."
  ([coll]
   (lazy-pairs (list (first coll)) (next coll)))
  ([seen coll]
   (when (seq coll)
     (lazy-seq
       (let [x (first coll)]
         (concat (map (fn [s]
                        [s x])
                      seen)
                 (lazy-pairs (cons x seen) (next coll))))))))

(defmacro forall-pairs [[[v1 v2] seq] & body]
  `(every? (fn ~'forall-pairs-every [[~v1 ~v2]] ~@body) (lazy-pairs ~seq)))

(defmacro exists-pair  [[[v1 v2] seq] & body]
  `(some (fn ~'exists-pair-some [[~v1 ~v2]]
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

(defn -casep-helper [test value default-f & pairs]
  (loop [pairs pairs]
    (cond (empty? pairs)
          (default-f)

          :else
          (let [[keys f] (first pairs)]
            (if (exists [x keys] (test value x))
              (f)
              (recur (rest pairs)))))))

(defmacro casep [test obj & pairs]
  (loop [pairs pairs
         default (fn [] nil)
         acc ()]
    (cond (empty? pairs)
          `(-casep-helper ~test ~obj ~default ~@(reverse acc))

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

;; code thanks to https://clojurians.slack.com/archives/C053AK3F9/p1605188036049500
(defn unchunk [s]
  (when (seq s)
    (lazy-seq
      (cons (first s)
            (unchunk (next s))))))

(defn pairwise-fold
  "Similar to reduce, but iterates over the sequence
  several times, each time reducing adjacent pairs using the given
  function.  e.g., (((1 + 2) + (3 + 4)) + ((5 + 6) + (7 + 8))) ..."
  [f z coll]
  (if (empty? coll)
    z
    (letfn [(f-effective [[a b]]
              (f a b))
            (make-pairs [lazy-list]
              ;; returns either
              ;;   [vec-of-pairs ()] if lazy-list has even length
              ;;   [vec-of-paris (item)] if lazy-list has odd length
              (loop [vec []
                     lazy-list lazy-list]
                (cond (>= 1 (bounded-count 2 lazy-list))
                      [vec lazy-list]

                      :else
                      (recur (conj vec [(first lazy-list) (second lazy-list)])
                             (rest (rest lazy-list))))))]                    
      (loop [coll coll]
        (cond (= 1 (bounded-count 2 coll))
              (first coll)

              :else
              (let [[pair-vec residue] (make-pairs coll)]
                (recur (concat (map f-effective pair-vec)
                               residue))))))))

(defn tree-fold
  "Like the 3-arg version of reduce except that does not compute from left-to-right
  but rather computes by grouping in concentric groups of two. e.g.
   (+ (+ (+ 1 2) (+ 3 4)) (+ (+ 5 6) (+ 7 8))) ...
  The remaining (right-most) elements are partitioned into 2's as much as possible.
  Intermediate values become unreferenced as quickly as possible, allowing them to 
  be GCed.
  4-arg version allows function, g, which is used to *treat* incoming
  values of the collection, but it is avoided in recursive calls.  E.g.,
  to add the lengths of a collection of strings,
  (tree-fold + count 0 list-of-strings)
  is semantically eqiv to (reduce + 0 (map count list-of-strings))"
  ([f z coll]
   (tree-fold f identity z coll))
  ([f g z coll]
  (letfn [(dwindle-tree [stack]
            (if (empty? (rest stack))
              stack
              (let [[[i b1] [j b2] & tail] stack]
                (if (= i j)
                  (dwindle-tree (cons [(inc i) (f b2 b1)] tail))
                  stack))))
          (reduce-1 [stack ob]
            (dwindle-tree (cons [1 (g ob)] stack)))]

    ;; (apply (fn
    ;;          ([] z)
    ;;          ([b] b)
    ;;          ([& stack] (tree-fold f z (reverse stack))))
           
    ;;        (map second (reduce dwindle-1 () coll)))

    (let [stack (map second (reduce reduce-1 () coll))]
      (cond (empty? stack)
            z

            (empty? (rest stack))
            (first stack)

            :else
            (tree-fold f identity z (reverse stack)))))))

(defn uniquify
  "returns a new sequence with duplicates remove.
   If duplicates exist, left-most is removed, right-most remains"
  [seq]
  (reverse (distinct (reverse seq))))

(defn search-replace-splice
  "Search for all occurances of search-for in xs and replace with the elements of replace-with"
  [xs search-for replace-with]
  (mapcat (fn [x]
            (if (= x search-for)
              replace-with
              [x]))
          xs))

(defn search-replace 
  "Search for all occurances of search-for in xs and replace with replace-with"
  [xs search-for replace-with]
  (search-replace-splice xs search-for [replace-with]))

(defn count-if [pred xs]
  (count (filter pred xs)))

(defn count-if-not [pred xs]
  (count-if (fn [x] (not (pred x))) xs))

(defn assert-debug "call the assertion function as side effect, and return the given value."
  [expr assertion]
  (assertion expr)
  expr)


(defn call-with-found
  "Call the given predicate, pred, on successive elements of the collection
  until the first time pred returns a truthy value, at which time if-found
  is called with that element of the collection, and call-with-found returns
  the return value of if-found.   If no such element of collection is found
  (including if collection is empty) then the value if-not-found (defaulting
  to false) is returned."
  ([pred coll & {:keys [if-found if-not-found]
                 :or {if-found (constantly true)
                      if-not-found false}}]
   (reduce (fn [_ item]
             (if (pred item)
               (reduced (if-found item))
               if-not-found)) if-not-found coll)))

(defn with-first-match 
  "Find the first element in the given sequence, items,
   for which the predicate, pred, returns Boolean true.
   If such is found, call the continuation with the
   element.  This type of 'finder' avoids the problem of
   deciding whether nil was the value found.  The continuation
   is only called on the found value."
  [pred items continuation]
  (call-with-found pred items :if-found continuation :if-not-found nil))

(defn find-first
  ([pred coll]
   (find-first pred coll false))
  ([pred coll default]
   (call-with-found pred coll
                    :if-found identity
                    :if-not-found default)))

(defn or-else
  "Takes a (var-arg) seq of 0 or more 0-ary functions.  or-else calls the functions in-turn
  until one returns something other than :dont-know, and returns that value.  Otherwise
  :dont-know is returned"
  ([]
   :dont-know)
  ([f]
   (f))
  ([f & fs]
   (let [result (f)]
     (if (= :dont-know result)
       (apply or-else fs)
       result))))

(defn trace-graph [v0 edges]
  ;; The loop traverses the graph generated by the function, edges.
  ;; The traversal is in numerical order.  The initial vertex is 0, the next
  ;; vertex generated is 1.  Once the edges of vertex 0 are generated, several
  ;; vertices may have been generated, but vertex 1 is traversed next, then
  ;; 2, 3, ...
  ;; The variable, next-available-state, keeps track of the next available
  ;;    vertex number.  When the edges function returns a new v which
  ;;    has not yet been numbered, then it gets assigned next-available-state.
  ;; next-available-state gets incremented (via recur) whenever a new vertex
  ;;    is encountered, and associated with the current value of next-available-state.
  ;; The recursion terminates when the index of v is == to next-available-state
  ;;    because that means no new vertices have been encountered, thus
  ;;    all vertices have been visited.
  (loop [current-state-id 0 ; int
         next-available-state 1 ; int
         es (edges v0) ; seq of [L V] label vertex
         int-to-v [v0] ; vector of V, i.e. map int to V
         v-to-int {v0 0} ; map V to int
         m [[]] ; vector of seq of [L int], i.e. map of int to seq of [L int]
         ]
    (if (empty? es)
      ;; The termination condition occurs if next == next-available-state, because
      ;;   this means now new vertices have been encountered which have not
      ;;   yet been treated.
      (let [next (inc current-state-id)]
        (if (< next next-available-state)
          (let [v2 (int-to-v next)]
            (recur next next-available-state (edges v2) int-to-v v-to-int (conj m [])))
          [int-to-v m]))
      
      (let [[[label v1] & lvs] es]
        (if (not (get v-to-int v1 false))
          ;; if we are seeing v1 for the first time, then register it
          ;;   in int-to-v and in v-to-int.
          (recur current-state-id (inc next-available-state) es
                 (conj int-to-v v1) (assoc v-to-int v1 next-available-state) m)
          (recur current-state-id next-available-state lvs int-to-v v-to-int
                 (assoc m
                        current-state-id
                        (conj (m current-state-id) [label (v-to-int v1)]))))))))

(defn group-map 
  "Like group-by but allows a second function to be mapped over each
  of the values in the computed hash map."
  [f g seq]
  (into {} (for [[k vs] (group-by f seq)]
             [k (set (map g vs))])))

