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

(ns clojure-rte.rte-construct
  (:require [clojure-rte.genus :as gns]
            [clojure-rte.util :refer [member exists setof exists-pair
                                      call-with-collector defn-memoized
                                      visit-permutations fixed-point
                                      sort-operands with-first-match
                                      partition-by-pred seq-matcher
                                      rte-identity rte-constantly
                                      print-vals
                                      find-simplifier]]
            [clojure-rte.xymbolyco :as xym]
            [clojure.pprint :refer [cl-format]]
            [clojure.set :refer [union subset?]]
            [clojure-rte.cl-compat :as cl]
            [backtick :refer [template]]
            )
  (:refer-clojure :exclude [compile])
)

;; allow rte/ prefix even in this file.
(alias 'rte 'clojure-rte.rte-construct)


(declare traverse-pattern)
(declare canonicalize-pattern)
(declare match)
(declare compile)
(declare rte-inhabited?)
(declare rte-vacuous?)
(declare rte-to-dfa)
(declare canonicalize-pattern-once)
(declare -canonicalize-pattern-once)
(declare operand)
(declare operands)

(def ^:dynamic *rte-known*
  "Dynamic variable whose value is a map.
  The map associates symbols with rte expansions.
  Any tag in this table may be used in place of a type name
  in an rte pattern."
  ;; TODO - is it interesting to allow parameterized types here?
  ;;    E.g., these types would work similar to CL deftype,
  ;;    we'd need a macro expander based on quasi-quote.
  {
   'real? 'Number
   })

(defn call-with-compile-env [thunk]
  (binding [rte/compile (memoize rte-to-dfa)
            canonicalize-pattern-once (memoize -canonicalize-pattern-once)
            gns/check-disjoint (memoize gns/-check-disjoint)
            ]
    (thunk)))

(defmacro with-compile-env [[] & body]
  `(call-with-compile-env (fn [] ~@body)))

(defn call-with-rte
  "Call the given 0-ary function with 0 or more rte keys bound to rte patterns.
   with-rte is a macro API to this function.
   E.g.,
   (call-with-rte [::a '(:permute Long Long String)
                   ::b '(:permute Double Double String)]
     (fn []
      (rte/match '(:cat ::a ::b) [1 \"hello\" 2
                                  \"world\" 1.0 2.0])))"
  [bindings thunk]
  ;; TODO need to detect if every a local key is defined differently,
  ;; and if so purge the memoize cache of rte/compile.
  (with-compile-env ()
    (binding [*rte-known* (apply assoc *rte-known* bindings)]
      (thunk))))

(defmacro with-rte
  "Evaluate the given body in a dynamic extend where 0 or more keys bound to
   un-quoted rte patterns.
   E.g.,
   (with-rte [::a (:permute Long Long String)
              ::b (:permute Double Double String)]
     (rte/match '(:cat ::a ::b) [1 \"hello\" 2
                                \"world\" 1.0 2.0])))
   Warning, any rte patterns which are compiled during the dynamic extent
   of with-rte, survive the dynamic extend.  I.e., they are not compiled
   twice, rather they are memoized.   
   Any patterns which were compiled before the dynamic extend are ignored.
   Any patterns compiled within the dynamic extend are abandoned when
   the dynamic extend ends."
  [bindings & body]
  `(call-with-rte '~bindings (fn [] ~@body)))

(defn rte? [t]
  (and (sequential? t)
       (= 'rte (first t))))

(defmethod gns/typep 'rte [a-value [_a-type pattern]]
  (and (sequential? a-value)
       (rte/match pattern a-value)))

(defmethod gns/valid-type? 'rte [[_ pattern]]
  (boolean (rte/compile pattern)))

(defmethod gns/-inhabited? 'rte [t1]
  (if (rte? t1)
    (boolean (rte-inhabited? (rte/compile (second t1))))
    :dont-know))

(defmethod gns/-disjoint? :not-rte [t1 t2]
  ;; (disjoint? (not (rte ...)) clojure.lang.IPersistentVector )

  (cond (not (and (gns/not? t1)
                  (rte? (second t1))))
        :dont-know

        (and (gns/class-designator? t2)
             (or (isa? (gns/find-class t2) clojure.lang.Seqable)
                 (isa? (gns/find-class t2) clojure.lang.Sequential)))
        true

        (and (gns/class-designator? t2)
             (not (isa? (gns/find-class t2) clojure.lang.Sequential)))
        false

        :else
        :dont-know))

(defmethod gns/-disjoint? 'rte [t1 t2]
  (cond (not (rte? t1))
        :dont-know

        ;; (disjoint? (rte ...) (rte ...))
        (rte? t2)
        (let [[_ pat1] t1
              [_ pat2] t2]
          (rte-vacuous? (rte/compile `(:and ~pat1 ~pat2))))

        ;; (disjoint? (rte ...) clojure.lang.IPersistentVector )
        (and (gns/class-designator? t2)
             (or (isa? (gns/find-class t2) clojure.lang.Seqable)
                 (isa? (gns/find-class t2) clojure.lang.Sequential)))
        false
        
        (and (gns/not? t2)
             (rte? (second t2)))
        (let [[_ pat1] t1
              [_ [_ pat2]] t2]
          (rte-vacuous? (rte/compile `(:and ~pat1 (:not ~pat2)))))
        
        (and (gns/class-designator? t2)
             (isa? (gns/find-class t2) java.lang.CharSequence))
        (let [[_ pat1] t1]
          (rte-vacuous? (rte/compile `(:and ~pat1 (:* java.lang.Character)))))
        
        (and (gns/class-designator? t2)
             (not (isa? (gns/find-class t2) clojure.lang.Sequential)))
        true
        
        (and (gns/not? t2)
             (gns/class-designator? (second t2))
             (not (isa? (gns/find-class (second t2)) clojure.lang.Sequential)))
        false
        
        :else :dont-know))

(defmethod gns/-subtype? 'rte [sub-designator super-designator]
  (let [s1 (delay (gns/subtype? '(rte (:* java.lang.Character)) super-designator :dont-know))
        s2 (delay (gns/subtype? sub-designator '(rte (:* java.lang.Character)) :dont-know))]
    (cond (and (rte? sub-designator)
               (rte? super-designator))
          (let [[_ pat-sub] sub-designator
                [_ pat-super] super-designator]
            (rte-vacuous? (rte/compile `(:and ~pat-sub (:not ~pat-super)))))
          
          (and (rte? super-designator)
               (gns/class-designator? sub-designator)
               (isa? (gns/find-class sub-designator) java.lang.CharSequence)
               (member @s1 '(true false)))
          @s1
          
          (and (rte? sub-designator)
               (gns/class-designator? super-designator)
               (isa? (gns/find-class super-designator) java.lang.CharSequence)
               (member @s2 '(true false)))
          @s2
          
          (and (rte? super-designator)
               (gns/class-designator? sub-designator)
               (not (isa? (gns/find-class sub-designator) clojure.lang.Sequential)))
          false
          
          (and (rte? sub-designator)
               (gns/class-designator? super-designator)
               (not (isa? (gns/find-class super-designator) clojure.lang.Sequential)))
          false
          
          (and (rte? super-designator)
               (gns/and? sub-designator)
               (exists [and-operand (rest sub-designator)]
                       (and (rte? and-operand)
                            (gns/subtype? and-operand super-designator
                                          false))))
          true
               
          :else :dont-know)))

(def sigma-* '(:* :sigma))
(def not-sigma `(:or (:cat :sigma :sigma ~sigma-*) :epsilon))
(def not-epsilon `(:cat :sigma ~sigma-*))

(defn type-dispatch [pattern]
  (cond (sequential? pattern)
        (first pattern)

        (keyword? pattern)
        pattern

        :else
        :default))

(defmulti operands
  type-dispatch)

(defmulti operand
  type-dispatch)

(defmethod operands :or
  [pattern]
  (rest pattern))

(defmethod operands :and
  [pattern]
  (rest pattern))

(defmethod operand :*
  [pattern]
  (second pattern))

(defmethod operand :not
  [pattern]
  (second pattern))

(defmethod operands :cat
  [pattern]
  (rest pattern))


(def ^:dynamic *traversal-functions*
  "Default callbacks for walking an rte tree.
  A function which wants to perform a recursive action on an
  rte pattern, must call traverse-pattern, passing 
  (assoc *traversal-functions*
         key value key value   ...)
  as second argument, thus overriding the callbacks
  when special nodes are encountered in the rte pattern.
  For example, when (:* ...) is encountered, the function
  (*traversal-functions* :*) is called with two arguments,
  1) the list of operands given to (:* ...), and 2) the value
  value of functions, i.e., the extended value of
  *traversal-functions*"
  {:client (fn [pattern functions]
             (traverse-pattern pattern functions))
   :type (fn [tag functions]
           ((:client functions) tag functions))
   :* (fn [pattern functions]
        (cons :* ((:client functions) pattern functions)))
   :and (fn [patterns functions]
          (cons :and (map (fn [expr]
                            ((:client functions) expr functions)) patterns)))
   :or (fn [patterns functions]
         (cons :or (map (fn [expr]
                          ((:client functions) expr functions)) patterns)))
   :not (fn [pattern functions]
          (cons :not ((:client functions) pattern functions)))
   :cat (fn [patterns functions]
          (cons :cat (map (fn [expr]
                            ((:client functions) expr functions)) patterns)))
   :sigma (fn [pattern functions]
            ((:client functions) pattern functions))
   :empty-set (fn [pattern functions]
                ((:client functions) pattern functions))
   :epsilon (fn [pattern functions]
              ((:client functions) pattern functions))
   })

(defmulti expand-1
  "macro-like facility for rte.
  A call to this function expands an rte pattern (once).
  Methods are responsible for expanding as a function of the first element
  of the pattern e.g., (and ...), (spec ...), (:? ...) etc.
  Methods take two arguments [pattern functions]
  pattern is the entire rte pattern being expanded, which might be sequence or otherwise.
    E.g., pattern = (:and A B C)
    or    pather = :empty-set
  A method may return either a transformed version of the pattern,
  or may return the pattern itself.   This function is called within
  a call to fixed-point which will keep calling the function until it
  eventually _expands_ into itself.
  The :default method returns the given pattern, so as fixed-point
  continues to expand the pattern, eventually there'll be no other
  applicable method and the :default method will be called, triggering
  fixed-point to terminate."
  (fn [pattern _functions]
    (cond
      (not (sequential? pattern))
      :default
      :else
      (first pattern))))

(defn invalid-pattern [pattern functions culprit]
  (throw (ex-info (format "[134] invalid pattern %s" pattern)
                  {:error-type :rte-expand-1-error
                   :keyword (first pattern)
                   :culprit culprit
                   :pattern pattern
                   :functions functions
                   })))

(defmethod expand-1 :default [pattern _functions]
  pattern)

(defmethod expand-1 'satisfies [pattern _functions]
  (gns/expand-satisfies pattern))

(defmethod expand-1 :? [pattern functions]
  (apply (fn
           ([] (invalid-pattern pattern functions '[:? []]))
           ([operand] `(:or :epsilon ~operand))
           ([_ & _] (invalid-pattern pattern functions '[:? [_ & _]]))) 
         (rest pattern)))

(defmethod expand-1 :+ [pattern functions]
  (apply (fn
           ([] (invalid-pattern pattern functions '[:+ []]))
           ([operand] `(:cat ~operand (:* ~operand)))
           ([_ & _] (invalid-pattern pattern functions '[:+ [_ & _]])))
         (rest pattern)))

(defmethod expand-1 :permute [pattern _functions]
  (apply (fn
           ([] :epsilon)
           ([operand] operand)
           ([_ & _]
            (let [operands (rest pattern)]
              (cons :or (call-with-collector (fn [collect]
                                               (visit-permutations
                                                (fn [perm]
                                                  (collect (cons :cat perm))) operands)))))))
         (rest pattern)))

(defmethod expand-1 :contains-any [pattern _functions]
  (apply (fn
           ([] :epsilon)
           ([operand] operand)
           ([_ & _]
            (let [operands (rest pattern)]
              `(:cat ~sigma-*
                     (:or ~@operands)
                     ~sigma-*))))
         (rest pattern)))

(defmethod expand-1 :contains-every [pattern _functions]
  (apply (fn
           ([] :epsilon)
           ([operand] operand)
           ([_ & _]
            (let [wrapped (for [operand (rest pattern)]
                            `(:cat ~sigma-* ~operand ~sigma-*))]
              `(:and ~@(doall wrapped)))))
         (rest pattern)))

(defmethod expand-1 :contains-none [pattern _functions]
  ;; TODO, not sure what (:contains-none) should mean with no arguments.
  ;;    as implemented it is equivalent to (:not :epsilon) which seems wierd.
  `(:not (:contains-any ~@(rest pattern))))

(defmethod expand-1 :exp [pattern functions]
  (letfn [(expand [n m pattern]
            (assert (>= n 0) (format "pattern %s is limited to n >= 0, not %s" pattern n))
            (assert (<= n m) (format "pattern %s is limited to n <= m, got %s > %s" pattern n m))
            (let [operand pattern
                  repeated-operand (repeat n operand)
                  optional-operand (repeat (- m n) `(:? ~operand))
                  ]
              (traverse-pattern `(:cat ~@repeated-operand ~@optional-operand) functions)))]
    (apply (fn
             ([] (invalid-pattern pattern functions '[:exp []]))
             ([_] (invalid-pattern pattern functions '[:exp [_]]))
             ([n pattern]
              (expand n n pattern))
             ([n m pattern] 
              (expand n m pattern))
             ([_ _ _ & _] 
              (invalid-pattern pattern functions '[:exp [_ _ _ & _]])))
           (rest pattern))))


(defn verify-type [pattern functions]
  (if (gns/valid-type? pattern)
    pattern
    (throw (ex-info (cl-format false "[219] invalid type designator ~A" pattern)
                    {:error-type :invalid-type-designator
                     :pattern pattern
                     :functions functions}))))

(defmethod expand-1 'and [pattern functions]
  ;; convert (and a b c) => (:and a b c)
  ;;  i.e., (or (:and ...)) is not allowed, which probably means the user forgot a :
  (cons :and (rest (verify-type pattern functions))))

(defmethod expand-1 'or [pattern functions]
  ;; convert (or a b c) => (:or a b c)
  ;;  i.e., (or (:and ...)) is not allowed, which probably means the user forgot a :
  (cons :or (rest (verify-type pattern functions))))

(defmethod expand-1 'not [pattern functions]
  ;;             (not a) => (:and :sigma (:not a))
  `(:and (:not ~@(rest (verify-type pattern functions)))
         :sigma))

(defn expand ; rte/expand
  "Repeat calls to expand-1 until a fixed point is found."
  ([given-pattern functions]
   (rte/expand given-pattern functions true))
  ([given-pattern functions verbose]
   (try (fixed-point given-pattern
                     (fn [p] (expand-1 p functions))
                     ;; TODO -- after debugging, replace this (fn ...) with simply =.
                     =
                     ;; (fn [a b] 
                     ;;   (if (= a b)
                     ;;     (do (println [:fixed-point-found a]) true)
                     ;;     (do (println [:expanded :from a :to b]) false)
                     ;;     ))

                     )
        (catch clojure.lang.ExceptionInfo ei
          (if (:unsupported-pattern (ex-data ei))
            (do ;; if we fail to expand the pattern, then don't even try
              (when verbose
                (cl-format true "failed to expand pattern: ~A, at ~A~%" given-pattern (:pattern (ex-data ei))))
              
              given-pattern)
            (throw ei))
          ))))

(def traversal-depth-max 10)
(defn traverse-pattern
  "Workhorse function for walking an rte pattern.
   This function is the master of understanding the syntax of an rte
   pattern.  Any function which needs to perform a recursive operation
   such as derivative, nullable, first-types, or canonicalize-pattern
   may call traverse-pattern with an augmented map of
   *traversal-functions*, indicating the callbacks for each rte
   keyword such as :* :cat etc.  The philosophy is that no other
   function needs to understand how to walk an rte pattern."
  ([given-pattern functions]
   (traverse-pattern 0 given-pattern functions))
  ([depth given-pattern functions]
   (when (= depth traversal-depth-max)
     (cl-format false "warning traverse pattern depth reached: ~A ~A"
                depth given-pattern))
   (assert (<= depth traversal-depth-max)
           (cl-format false "traverse pattern depth exceeded: ~A ~A"
                      depth given-pattern))
   (letfn [(if-atom [pattern]
             (cond
               (member pattern '(:epsilon :empty-set :sigma))
               ((functions pattern) pattern functions)

               (*rte-known* pattern)
               (traverse-pattern (inc depth) (*rte-known* pattern) functions)

               :else
               ((:type functions) pattern functions)))
           (if-nil [_]
             ((:type functions) () functions))
           (if-singleton-list [pattern] ;; (:or)  (:and)
             (let [[keyword] pattern]
               (case keyword
                 (:or)  (traverse-pattern (inc depth) :empty-set functions)
                 (:and) (traverse-pattern (inc depth) sigma-* functions)
                 (:cat) (traverse-pattern (inc depth) :epsilon functions)
                 (:not
                  :*) (throw (ex-info (format "[264] invalid pattern %s, expecting exactly one operand" pattern)
                                      {:error-type :rte-syntax-error
                                       :keyword keyword
                                       :pattern pattern
                                       :functions functions
                                       :cause :unary-keyword
                                       }))
                 ;; case-else
                 ((:type functions) pattern functions))))
           (if-exactly-one-operand [pattern] ;; (:or Long) (:* Long)
             (let [[token operand] pattern]
               (case token
                 (:or :and :cat)
                 (traverse-pattern (inc depth) operand functions)
                 
                 (:not :*)
                 ((functions token) operand functions)

                 ;;case-else
                 ((:type functions) pattern functions))))
           (if-multiple-operands [pattern]
             (let [[token & operands] pattern]
               (case token
                 (:or
                  :and
                  :cat)
                 ((functions token) operands functions)

                 (:not :*)
                 (throw (ex-info (format "[301] invalid pattern %s, expecting exactly one operand" pattern)
                                 {:error-type :rte-syntax-error
                                  :keyword keyword
                                  :pattern pattern
                                  :functions functions
                                  :cause :unary-keyword
                                  }))

                 ;;case-else
                 ((:type functions) pattern functions))))]
     (let [pattern (expand given-pattern functions)]
       (cond (not (seq? pattern))
             (if-atom pattern)

             (empty? pattern)
             (if-nil pattern)

             (empty? (rest pattern)) ;; singleton list, (:and), (:or) etc
             (if-singleton-list pattern)

             (empty? (rest (rest pattern))) ;; (:and x)
             (if-exactly-one-operand pattern)

             ;; cond-else (:keyword args) or list-expr ;; (:and x y)
             :else (if-multiple-operands pattern))))))

(defn nullable 
  "Determine whether the given rational type expression is nullable.
  I.e., does the empty-word satisfy the expression."
  [expr]
  (boolean
   (traverse-pattern expr
                     (assoc *traversal-functions*
                            :client nullable
                            :empty-set (rte-constantly false)
                            :epsilon (rte-constantly true)
                            :sigma   (rte-constantly false)
                            :type (rte-constantly false)
                            :* (rte-constantly true)
                            :cat (fn [operands _functions]
                                   (every? nullable operands))
                            :and (fn [operands _functions]
                                   (every? nullable operands))
                            :or (fn [operands _functions]
                                  (some nullable operands))
                            :not (fn [operand _functions]
                                   (not (nullable operand)))))))

(defn first-types 
  "Return a possibly empty set of types (i.e., object which can be
  passed to isa?) which specify the possible set of first value values
  in any sequence satisfying this rational type expression."
  [expr]
  (letfn [(mr [operands _functions]
            (reduce (fn [acc next]
                      (union acc (first-types next))) #{} operands))]
    (traverse-pattern expr
                      (assoc *traversal-functions*
                             :epsilon (rte-constantly #{})
                             :empty-set (rte-constantly #{})
                             :sigma (rte-constantly #{:sigma})
                             :type (fn [operand _functions]
                                     #{operand})
                             :or mr
                             :and mr
                             :not (fn [operand _functions]
                                    (first-types operand))
                             :cat (fn [[head & tail :as operands] _functions]
                                    ;; this cond is perhaps more verbose than absolutely necessary.
                                    ;; we make special cases of empty operands and empty tail
                                    ;;  in order to avoid calling first-types on head
                                    (cond (empty? operands)
                                          (first-types :epsilon)

                                          (empty? tail)
                                          (first-types head)

                                          (nullable head)
                                          (union (first-types head)
                                                 (first-types (cons :cat tail)))

                                          :else
                                          (first-types head)))
                             :* (fn [operand _functions]
                                  (first-types operand))))))

(def rte/cat?
  "Predicate determining whether its object is of the form (:cat ...)"
  (seq-matcher :cat))

(def rte/*?
  "Predicate determining whether its object is of the form (:* ...)"
  (seq-matcher :*))

(def rte/not?
  "Predicate determining whether its object is of the form (:not ...)"
  (seq-matcher :not))

(def rte/and?
  "Predicate determining whether its object is of the form (:and ...)"
  (seq-matcher :and))

(def rte/or?
  "Predicate determining whether its object is of the form (:or ...)"
  (seq-matcher :or))

(def rte/create-or
  (fn [operands]
    (cond (empty? operands)
          :empty-set

          (= 1 (count operands))
          (first operands)

          :else
          (cons :or operands))))

(def rte/create-and
  (fn [operands]
    (cond (empty? operands)
          sigma-*

          (= 1 (count operands))
          (first operands)

          :else
          (cons :and operands))))

(def rte/create-not
  (fn [not-operand]
    (if (rte/not? not-operand)
      (operand not-operand)
      (list :not not-operand))))


(defmethod gns/-canonicalize-type 'rte
  [type-designator nf]
  ;; TODO need to pass nf to canonicalize-pattern, because if it needs to call
  ;;    gns/canonicalize-type, we'll need nf again
  (cons 'rte (map canonicalize-pattern
                  (rest type-designator))))

(defn remove-first-duplicate
  "Look through the given sequence to find two consecutive elements a,b
  for which (test a b) is a Boolean true.   If not found, return false.
  If found return a pair [prefix suffix] where prefix is a copy of the sequence
  up to but not including a, and suffix is the tail after but not including a.
  The suffix sequence starts with b.   I.e., the length of the input sequence
  is 1 more than the sum of the two output sequences, if a duplicate was found."
  [test seq]
  (loop [seq seq
         head ()]
    (cond (empty? seq)
          false

          (empty? (rest seq))
          false

          (test (first seq) (second seq))
          [(reverse head) (rest seq)]

          :else
          (recur (rest seq)
                 (cons (first seq) head)))))

(defn reduce-redundant-or [operands]
  (let [ands (filter rte/and? operands)
        xyz (setof [x ands] (exists [y operands] (member y (rest x))))
        abc (setof [and1 ands]
                   (let [and1-operands (set (rest and1)) ]
                     (exists [and2 ands]
                             (let [and2-operands (set (rest and2))]
                               (and (not (subset? and1-operands and2-operands))
                                    (subset? and2-operands and1-operands))))))
        superfluous-ands (concat xyz abc)]
    (if (empty? superfluous-ands)
      operands
      ;; remove all superfluous-ands from or-operands
      (remove (fn [to-remove] (member to-remove superfluous-ands)) operands))))

(defn disjoint?-false-warn [t1 t2]
  ;; don't complain about rte nor satisfies
  (letfn [(dont-complain [t]
            (and (sequential? t)
                 (not-empty t)
                 (member (first t) '(satisfies rte))))]
    (let [types-disjoint (gns/disjoint? t1 t2 :dont-know)]
      (cond (member types-disjoint '(true false))
            types-disjoint

            (or (dont-complain t1)
                (dont-complain t2))
            false
            
            :else
            (do
              (cl-format true "disjoint? cannot decide ~A vs ~A -- assuming not disjoint~%"
                         t1 t2)
              false)))))

(defn conversion-*1
  [self]
  (let [op (operand self)]
    (cond (= :epsilon op)
          :epsilon

          (= :empty-set op)
          :epsilon

          (rte/*? op)
          op

          :default
          self)))

(defn conversion-*2
  [self]
  (if (not (rte/cat? (operand self)))
    self
    ;; Star(Cat(...))
    (let [c (operand self)
          cat-operands (operands c)]
      (cond 
        (not (member (count cat-operands) '(2 3)))
        self

        ;; Star(Cat(x,Star(x))) -> Star(x)
        (and (= 2 (count cat-operands))
             (rte/*? (second cat-operands))
             (= (first cat-operands)
                (operand (second cat-operands))))
        (second cat-operands)

        ;; Star(Cat(Star(x),x)) -> Star(x)
        (and (= 2 (count cat-operands))
             (rte/*? (first cat-operands))
             (= (second cat-operands)
                (operand (first cat-operands))))
        (first cat-operands)

        ;; Star(Cat(Star(x),x,Star(x))) -> Star(x)
        (and (= 3 (count cat-operands))
             (rte/*? (first cat-operands))
             (= (first cat-operands) (last cat-operands))
             (= (operand (first cat-operands))
                (second cat-operands)))
        (first cat-operands)

        :else
        self))))
             

(defn conversion-*3
  [self]
  (if (not (rte/cat? (operand self)))
    self
    (let [c (operand self)
          cat-operands (operands c)]
      (cond
        ;; Star(Cat(X, Y, Z, Star(Cat(X, Y, Z))))
        ;;    -->    Star(Cat(X, Y, Z))
        (and (rte/*? (last cat-operands))
             (rte/cat? (operand (last cat-operands)))
             (= (operands (operand (last cat-operands)))
                (butlast cat-operands)))
        (last cat-operands)

        ;; Star(Cat(Star(Cat(X, Y, Z)), X, Y, Z))
        ;;   -->    Star(Cat(X, Y, Z))
        (and (rte/*? (first cat-operands))
             (rte/cat? (operand (first cat-operands)))
             (= (operands (operand (first cat-operands)))
                (rest cat-operands)))
        (first cat-operands)

        ;; Star(Cat(Star(Cat(X, Y, Z)), X, Y, Z, Star(Cat(X, Y, Z)))
        ;;   -->    Star(Cat(X, Y, Z))
        (and (<= 3 (count cat-operands))
             (rte/*? (first cat-operands))
             (rte/cat? (operand (first cat-operands)))
             (= (first cat-operands) (last cat-operands))
             (= (operands (operand (first cat-operands)))
                (butlast (rest cat-operands))))
        (first cat-operands)

        :else
        self))))

(defn conversion-*99
  [self]
  (template (:* ~(canonicalize-pattern-once (operand self)))))

(defn conversion-not-1
  [self]
  (let [op (operand self)]
    (cond (= op :sigma)
          not-sigma

          (= op sigma-*)
          :empty-set

          (= op :epsilon)
          not-epsilon

          (= op :empty-set)
          sigma-*

          :else
          self)))

(defn conversion-not-2
  [self]
  (cond (rte/not? (operand self))
        (operand (operand self))
        :else
        self))

(defn conversion-not-3
  [self]
  (let [op (operand self)]
    (cond (rte/and? op)
          (rte/create-or (map rte/create-not (operands op)))

          (rte/or? op)
          (rte/create-and (map rte/create-not (operands op)))

          :else
          self)))
                    

(defn conversion-not-99
  [self]
  (rte/create-not (canonicalize-pattern-once (operand self))))


(defn-memoized [canonicalize-pattern-once -canonicalize-pattern-once]
  "Rewrite the given rte patter to a canonical form.
  This involves recursive re-writing steps for each sub form,
  including searches for syntatical and semantical reductions.
  The API for canonicalizing a pattern is canonicalize-pattern,
  which finds a fixed-point of canonicalize-pattern-once, i.e.,
  keeps calling canonicalize-pattern-once until it finally
  stops changing."
  [re]
  (traverse-pattern re
                    (assoc *traversal-functions*
                           :type (fn [tag _functions]
                                   (gns/canonicalize-type tag))
                           :empty-set rte-identity
                           :epsilon rte-identity
                           :sigma rte-identity
                           :* (fn [operand _functions]
                                (find-simplifier (list :* operand)
                                                 [conversion-*1
                                                  conversion-*2
                                                  conversion-*3
                                                  conversion-*99]))
                           :cat (fn [operands _functions]
                                  (let [operands (map canonicalize-pattern operands)]
                                    (assert (< 1 (count operands))
                                            (format "traverse-pattern should have already eliminated this case: re=%s count=%s operands=%s"
                                                    re (count operands) operands))
                                    (cl/cl-cond
                                     ;; (:cat A (:* X) (:* X) B)
                                     ;;  --> (:cat A (:* X) B)
                                     ((let [x (remove-first-duplicate (fn [a b]
                                                                        (and (*? a)
                                                                             (= a b)))
                                                                      operands)]
                                        ;; remove-first-duplicate returns false if it didn't find a duplicate
                                        (and x
                                             (cons :cat (concat (first x) (second x))))))

                                     ;; (:cat x (:cat a b) y) --> (:cat x a b y)
                                     ((some cat? operands)
                                      (cons :cat (mapcat (fn [obj]
                                                           (if (cat? obj)
                                                             (rest obj)
                                                             (list obj))) operands)))

                                     ;; (:cat x "empty-set" y) --> :emptyset
                                     ((member :empty-set operands)
                                      :empty-set)

                                     ;; (:cat x :epsilon y) --> (:cat x y)
                                     ((member :epsilon operands)
                                      (cons :cat (remove #{:epsilon} operands)))

                                     (:else
                                      (cons :cat operands)))))
                           :not (fn [operand _functions]
                                  (find-simplifier (list :not operand)
                                                   [conversion-not-1
                                                    conversion-not-2
                                                    conversion-not-3
                                                    conversion-not-99]))
                           :and (fn [operands _functions]
                                  (let [operands (dedupe (sort-operands (map canonicalize-pattern operands)))]
                                    (cl/cl-cond
                                     ;; TODO - (:and :epsilon ...)
                                     ;;    if any of the :and arguments is not nullable,
                                     ;;    then the result is :empty-set
                                     ;;    otherwise the result is :epsilon

                                     ;; TODO (:and (:cat A B sigma-*)
                                     ;;            (:cat A B ))
                                     ;;  --> (:and (:cat A B))

                                     ;; (:and :epsilon :sigma A B C)
                                     ;; --> :empty-set, because :epsilon and :sigma are disjoint
                                     ((and (member :epsilon operands)
                                           (member :sigma operands))
                                      :empty-set)
                                     
                                     ((member :empty-set operands)
                                      :empty-set)

                                     ((some rte/and? operands)
                                      (cons :and (mapcat (fn [obj]
                                                           (if (rte/and? obj)
                                                             (rest obj)
                                                             (list obj))) operands)))

                                     ((member sigma-* operands)
                                      (cons :and (remove (fn [obj]
                                                           (= sigma-* obj)) operands)))

                                     ((some rte/or? operands)
                                      ;; (:and (:or A B) C D) --> (:or (:and A C D) (:and B C D))
                                      (with-first-match rte/or? operands
                                        (fn [or-item]
                                          (let [others (remove (fn [x] (= or-item x)) operands)]
                                            (cons :or (map (fn [x] (list* :and x others)) (rest or-item)))))))

                                     ;; (:and x (:not x)) --> :empty-set
                                     ((let [nots (filter rte/not? operands)
                                            others (remove rte/not? operands)]
                                        (when (some (fn [item]
                                                      (some #{(list :not item)} nots)) others)
                                          :empty-set)))

                                     ;; (:and of disjoint types) --> :empty-set
                                     ((let [atoms (filter (complement seq?) operands)
                                            types (filter (fn [x] (not= x :epsilon)) atoms)
                                            ]
                                        (when (exists-pair [[i1 i2] types]
                                                           (and (not= i1 i2)
                                                                (disjoint?-false-warn i1 i2)))
                                          :empty-set)))
                                     
                                     ;; (:and subtype supertype x y z) --> (:and subtype x y z)
                                     ((let [atoms (filter (complement seq?) operands)
                                            types (filter (fn [x] (not= x :epsilon)) atoms)
                                            max (gns/type-max types)
                                            ]
                                        (when max
                                          (cons :and (remove #{max} operands)))))
                                     
                                     (:else
                                      (cons :and operands))

                                     )))
                           :or (fn [operands _functions]
                                 (assert (< 1 (count operands))
                                         (format "traverse-pattern should have already eliminated this case: re=%s count=%s operands=%s" re (count operands) operands))
                                 (let [operands (dedupe
                                                 (sort-operands (map canonicalize-pattern
                                                                     (reduce-redundant-or operands))))]
                                   (cl/cl-cond
                                    ;; TODO (:or (:cat A B sigma-*)
                                    ;;           (:cat A B ))
                                    ;;  --> (:or (:cat A B sigma-*))

                                    ;; TODO (:or A B C (:* B) D)
                                    ;;  --> (:or A C (:* B) D)
                                    
                                    ;; (:or A :epsilon B (:cat X (:* X)) C)
                                    ;;   --> (:or A :epsilon B (:* X) C )
                                    ;;   --> (:or A B (:* X) C) ;; TODO remove :epsilon if there is another element which is nullable
                                    ((and (member :epsilon operands)
                                          (some (fn [obj]
                                                  (and (cat? obj)
                                                       (= 3 (count obj))
                                                       (let [[_ x y] obj]
                                                         (cond (and (*? x)
                                                                    (= y (second x)))
                                                               ;; (:or x A B C)
                                                               (cons :or (cons x (remove (fn [o] (or (= o :epsilon)
                                                                                                     (= o obj))) operands)))

                                                               (and (*? y)
                                                                    (= x (second y)))
                                                               ;; (:or y A B C)
                                                               (cons :or (cons y (remove (fn [o] (or (= o :epsilon)
                                                                                                     (= o obj))) operands)))
                                                               
                                                               :else
                                                               false))))
                                                operands)))

                                    ((some rte/or? operands)
                                     (cons :or (mapcat (fn [obj]
                                                         (if (rte/or? obj)
                                                           (rest obj)
                                                           (list obj))) operands)))

                                    ((member sigma-* operands)
                                     sigma-*)

                                    ((member :empty-set operands)
                                     (cons :or (remove #{:empty-set} operands)))

                                    ;; (:or x (:not x)) --> :sigma
                                    ((let [nots (filter rte/not? operands)
                                           others (remove rte/not? operands)]
                                       (when (some (fn [item]
                                                     (some #{(list :not item)} nots)) others)
                                         sigma-*)))

                                    ;; (:or subtype supertype x y z) --> (:and supertype x y z)
                                    ((let [atoms (filter (complement seq?) operands)
                                           min (gns/type-min atoms)
                                           ]
                                       (when min
                                         (cons :or (remove #{min} operands)))))

                                    (:else
                                     (cons :or operands))
                                    ))))))

(defn canonicalize-pattern 
  "find the fixed point of canonicalize-pattern-once"
  [pattern]
  (fixed-point pattern canonicalize-pattern-once =))

(defn compute-compound-derivative
  "wrt may be a compound type designator such as (and A (not B)).
  So to compute the derivative of B wrt (and A (not B)) we get :empty-set
  because the types are disjoint."
  [expr wrt]

  ;;(assert (not (sequential? expr)) (cl-format false "not expecting sequence expr= ~A:" expr))
  (assert (sequential? wrt) (cl-format false "expecting sequence, not ~A:" wrt))
  (assert (= 'and (first wrt)))
  (let [[_ & and-args] wrt]
    (cond
      (member (template (not ~expr)) and-args)
      :empty-set

      (some #{expr} and-args)
      :epsilon

      :else
      (throw (ex-info (format "not yet implemented: derivative of %s wrt %s"
                              expr wrt)
                      {:error-type :rte-not-yet-implemented
                       :pattern expr
                       :wrt wrt
                       })))))

(defn derivative 
  "Compute the Brzozowski rational expression derivative of the given
  rte pattern with respect to the given type wrt."
  [expr wrt]
  (letfn [(walk [patterns]
            (map (fn [p]
                   (derivative (canonicalize-pattern p) wrt))
                 patterns))]
    ;; TODO need to test deriv x wrt :sigma, should be empty-word unless x is :empty-set and :empty-set if x is :empty-set
    (canonicalize-pattern
     (cond
       (= :empty-set expr)
       :empty-set
       
       (= :epsilon wrt)
       expr ;; deriv of anything with respect to :epsilon is that thing.

       (= wrt expr)
       :epsilon

       :else
       (traverse-pattern expr
                         (assoc *traversal-functions*
                                :epsilon (rte-constantly :empty-set)
                                :empty-set (rte-constantly :empty-set)     
                                :sigma (fn [_type _functions]
                                         :epsilon)
                                :type (fn [td _functions]
                                        (cond 
                                          (disjoint?-false-warn wrt td)
                                          :empty-set

                                          (gns/subtype? wrt td false)
                                          :epsilon
                                          
                                          (gns/and? wrt)
                                          (compute-compound-derivative td wrt)

                                          :else
                                          (throw (ex-info (cl-format false
                                                                     "cannot compute derivative of overlapping types because ~A is not a subtype of ~A" wrt td)
                                                          {:error-type :derivative-undefined
                                                           :wrt wrt
                                                           :expr expr
                                                           :sub-types [{:type (template (and ~wrt ~expr))}
                                                                       {:type (template (and ~wrt (not ~expr)))}]
                                                           }))
                                          ))
                                :or (fn [operands _functions]
                                      (cons :or (walk operands)))
                                :and (fn [operands _functions]
                                       (cons :and (walk operands)))
                                :not (fn [operand _functions]
                                       (cons :not (walk (list operand))))
                                :cat (fn [[head & tail] _functions]
                                       (letfn [(term1 []
                                                 `(:cat ~(derivative head wrt)
                                                        ~@tail))
                                               (term2 []
                                                 (derivative `(:cat ~@tail) wrt))]
                                         (cond
                                           (nullable head) ;; nu = :epsilon
                                           `(:or ~(term1) ~(term2))
                                           :else
                                           (term1))))
                                :* (fn [operand _functions]
                                     `(:cat ~(derivative operand wrt) (:* ~operand)))))))))

(defn mdtd 
  "Given a set of type designators, return a newly computed list of type
  designators which implement the Maximal Disjoint Type Decomposition.
  I.e., the computed list designates a set whose union is the same as
  the given set, but all the elements are mutually disjoint."
  [type-set]
  ;; find a disjoint type
  (letfn [(independent? [t1]
            (every? (fn [t2]
                      (or (= t1 t2)
                          (disjoint?-false-warn t1 t2))) type-set))
          (count-if [pred items]
            (reduce (fn [acc item]
                      (if (pred item)
                        (inc acc)
                        acc)) 0 items))
          (collect-left-right [collect left right]
            (cond
              (and (empty? right)
                   (= 1 (count left)))
              (collect (first left))

              (and (empty? left)
                   (= 1 (count right)))
              (collect (list 'not (first right)))

              (and (empty? right)
                   (empty? left))
              :sigma

              (> (+ (count-if rte? left)
                    (count-if rte? right)) 1)
              (let [[left-rtes left] (partition-by-pred rte? left)
                    [right-rtes right] (partition-by-pred rte? right)
                    left-patterns (map second left-rtes)
                    right-patterns (map second right-rtes)]
                (cond (empty? left-rtes)
                      (let [new-rte (canonicalize-pattern `(:or ~@right-patterns))]
                        (collect-left-right collect
                                            left
                                            (cons (list 'rte new-rte) right)))

                      (empty? right-rtes)
                      (let [new-rte (canonicalize-pattern `(:and ~@left-patterns))]
                        (collect-left-right collect
                                            (cons (list 'rte new-rte) left)
                                            right))

                      :else
                      (let [new-rte (canonicalize-pattern
                                     `(:and ~@left-patterns
                                            (:not (:or ~@right-patterns))))]
                        (collect-left-right collect
                                            (cons (list 'rte new-rte) left)
                                            right))))

              :else
              (let [right (map (fn [x]
                                 (list 'not x)) right)]
                (collect (template (and ~@left ~@right))))))]

    (let [independent (filter independent? type-set)
          dependent (remove (set independent) type-set)]
      (concat independent (call-with-collector
                           (fn [collect]
                             (gns/map-type-partitions
                              (seq dependent)
                              (fn [left right]
                                (collect-left-right collect left right)))))))))

(defn find-all-derivatives 
  "Start with the given rte pattern, and compute its derivative with
  respect to all the values returned by first-types.  Continue
  computing the derivatives of each of the derivatives returned with
  respect to all of their first-types.  Continue this process until no
  more derivatives can be found.  Warning, the given pattern might not
  be an element of the return value.  I.e., the 0'th derivative is not
  guaranteed to be among the values returned."
  [pattern]
  (loop [to-do-patterns (list pattern)
         done #{}
         triples [] 
         ]
    (if (empty? to-do-patterns)
      [ triples (seq done)]
      (let [[pattern & to-do-patterns] to-do-patterns]
        (if (done pattern)
          (recur to-do-patterns done triples)
          (letfn [(xx [[acc-triples acc-derivs] wrt-type]
                    (let [triple [pattern wrt-type (derivative pattern wrt-type)]]
                      [(conj acc-triples triple)
                       (if (done (triple 2))
                         acc-derivs
                         (conj acc-derivs (triple 2)))]
                      )
                    )]
            (let [firsts (first-types pattern)
                  disjoined (mdtd (conj firsts :sigma))
                  [new-triples new-derivatives] (reduce xx [[] ()] disjoined)]
              (recur (concat new-derivatives to-do-patterns)
                     (conj done pattern)
                     (concat triples new-triples)))))))))

(defn rte-combine-labels ""
  [label1 label2]
  (cond
    (and (gns/or? label1)
         (gns/or? label2)) `(~@label1 ~@(rest label2))
    (and (gns/or? label1)
         (not (gns/or? label2))) `(~@label1 ~label2)
    (and (not (gns/or? label1))
         (gns/or? label2)) `(~(first label2) ~label1 ~@(rest label2))
    :else `(~'or ~label1 ~label2)))

(defn-memoized [rte/compile rte-to-dfa]
  "Use the Brzozowski derivative aproach to compute a finite automaton
  representing the given rte patten.  The finite automaton is in the
  form of an array of States.  The n'th State is array[n]."
  ([pattern]
   (rte-to-dfa pattern true))
  ([pattern exit-value]

   (let [given-pattern pattern
         pattern (canonicalize-pattern pattern)
         [triples derivatives] (find-all-derivatives pattern)
         derivatives (cons pattern (remove #{pattern} derivatives))
         index-map (zipmap derivatives (range (count derivatives)))
         triples (map (fn [[primative wrt deriv]]
                        [(index-map primative) wrt (index-map deriv)]
                        ) triples)
         grouped (group-by (fn [trip]
                             (trip 0)) triples)]
     (xym/extend-with-sink-state
      (xym/map->Dfa
       {:pattern given-pattern
        :canonicalized pattern
        :exit-map (constantly exit-value)
        :combine-labels rte-combine-labels
        :states
        (into {}
              (map (fn [deriv index]
                     (let [transitions (if (and (grouped index)
                                                (apply = (map (fn [[_src _wrt dst]]
                                                                dst) (grouped index))))
                                         ;; if all transitions have same dst, then don't draw
                                         ;; multiple transitions, just draw with with label = :sigma
                                         (list [:sigma ((first (grouped index)) 2)])
                                         (map (fn [[_src wrt dst]]
                                                [wrt dst]) (grouped index)))]
                       [index
                        (xym/map->State {:index index
                                         :initial (= 0 index)
                                         :accepting (nullable deriv)
                                         :pattern deriv
                                         :transitions transitions})]))
                   derivatives (range (count derivatives))))})))))

(defn dfa-to-rte
  "Accepts an object of type Dfa, and returns a map which associates
  exit values of the dfa with canonicalized rte patterns of the accepting
  langauge.  If there are no accepting states in the Dfa, an empty map {}
  is returned."
  [dfa]
  (assert (instance? (xym/record-name) dfa)
          (cl-format false "dfa-to-rte: expecting Dfa, not ~A ~A" (type dfa) dfa))
  (xym/extract-rte dfa canonicalize-pattern))


(defn dispatch [obj caller]
  (cond (instance? (xym/record-name) ;; parser cannot handle xym/Dfa
                   obj)
        :Dfa

        :else
        :pattern
))

(defmulti rte-trace
  "Given a compiled rte, find a sequence of types which satisfy the corresponding pattern."
  (fn [rte]
    (dispatch rte 'rte-trace)))

(defmethod rte-trace :pattern
  [pattern]
  (rte-trace (rte/compile pattern)))

(defmethod rte-trace :Dfa
  [dfa]
  (let [state-vec (:states dfa)]
    (letfn [(recurring [state path lineage]
              (cond
                (:accepting (state-vec state)) path
                (some #{state} lineage) false
                :else (some (fn [[type dst-state]]
                              (recurring dst-state (conj path type) (conj lineage state)))
                            (:transitions (state-vec state))))
              )]
      (recurring 0 [] ()))))

(defmulti rte-inhabited?
  (fn [rte]
    (dispatch rte 'rte-inhabited?)))

(defmethod rte-inhabited? :pattern [pattern]
  (rte-inhabited? (rte/compile pattern)))

(defmethod rte-inhabited? :Dfa [dfa]
  (some :accepting (xym/states-as-seq dfa)))

(defn rte-vacuous? [dfa]
  (not (rte-inhabited? dfa)))

(defmulti rte/match
  "(rte/match rte sequence :promise-disjoint true|false)
   Given an rte pattern or finite automaton generated by rte-to-dfa (or rte/compile), 
   determine whether the given sequence, items, matches the regular type expression.

   If the caller wishes to check more than one sequence against the same
   pattern, it is probably better to call rte/compile, to get an automaton, and
   use that same automaton in several calls to rte/match to avoid
   multiple conversions/look-ups, as the correspondence of pattern
   to compiled Dfa is maintained via the memoize function."

  (fn [rte _items & {:keys [promise-disjoint
                            hot-spot]}]
    (dispatch rte 'rte/match)))

(defmethod rte/match :pattern
  [pattern items & {:keys [promise-disjoint
                           hot-spot]}]
  (rte/match (rte/compile pattern) items :promise-disjoint true :hot-spot hot-spot))

(defmethod rte/match :Dfa
  [dfa items & {:keys [
                       ;; if the caller promises that never are two transitions in
                       ;;   the Dfa labeled with intersecting types, the use
                       ;;   :promise-disjoint true, in this case rte/match
                       ;;   can be more efficient and can assume that the
                       ;;   clauses can be tested in any order.  If the transitions
                       ;;   are not guaranteed disjoint, then rte/match must
                       ;;   build new type designators each one containing an and-not
                       ;;   of the previously seen types. 
                       promise-disjoint
                       ;; hot-spot = true -- lazily compile the type checks into Bdds
                       ;;    which is slow-going but becomes faster the more often you
                       ;;    re-use the same pattern, either because of loops in the
                       ;;    Dfa, or when the same Dfa is used to match different
                       ;;    input sequences.
                       ;; TODO -- the value of promise-disjoint should really come from
                       ;;    a slot in the dfa.  when the dfa is created, it should me
                       ;;    *marked* as being disjoint.  I believe it already is always
                       ;;    disjoint.
                       ;;    
                       ;; hot-spot = false -- always interpret the type checks rather
                       ;;    than converting them to Bdd.  This option is probably faster
                       ;;    if there are few loops in the Dfa, or if you only use the
                       ;;    pattern once to check a single input sequence.
                       hot-spot
                       ]}]
  (let [state-vec (:states dfa)
        sink-states (set (xym/find-sink-states dfa))]
    (if (empty? sink-states)
      (rte/match (xym/extend-with-sink-state dfa) items
                 :promise-disjoint promise-disjoint
                 :not-spot hot-spot)
      (let [sink-state-id (:index (first sink-states))]
        ;; There are two possible transition functions
        ;;   slow-transition-function -- this is faster if the caller intends to match
        ;;       the pattern only once.   The pattern is matched by an interpreter,
        ;;       and it is possible that the same type predicate will be tested multiple
        ;;       times on the same candidate objects.  If one of the type predicates
        ;;       is (satisfies slow-predicate) then that slow-predicate may be called
        ;;       multiple times, resulting in poor performance, especially if the
        ;;       pattern is used to test multiple sequences.
        ;;   fast-transition-function -- this is faster if the caller intends to match
        ;;       the pattern multiple times with different input sequences.  The
        ;;       pattern is *compiled* into a form where type-designators are converted
        ;;       to Bdds thus each type check guarantees to never check the same
        ;;       type predicate multiple times, and sometimes not at all.
        (letfn [(slow-transition-function [transitions]
                  (fn [candidate sink-state-id]
                    (some (fn [[type next-state-index]]
                            (if (gns/typep candidate type)
                              next-state-index
                              ;; TODO I'm not sure this is correct, do we need to return false
                              ;;   indicating no-match, or return the sink-stat-id.
                              ;;   false makes the tests pass, but sink-state-id seems more
                              ;;   logical.  need to investigate whether something more fundamental
                              ;;   is wrong.
                              false))
                          transitions)))
                (fast-transition-function [transitions]
                  (xym/optimized-transition-function transitions promise-disjoint sink-state-id))
                (transition-function [transitions]
                  (if hot-spot
                    (fast-transition-function transitions)
                    (slow-transition-function transitions)))
                (consume [state-index item]
                  (let [state-obj (state-vec state-index)]
                    (cl/cl-cond
                     ((member state-obj sink-states)
                      (reduced false))
                     (((transition-function (:transitions state-obj)) item sink-state-id))
                     (:else (reduced false)))))]
          (let [final-state (reduce consume 0 items)]
            ;; final-state may be integer desgnating the state which was
            ;;  reached on iterating successfully through the input
            ;;  sequence, items.  Or final-state may false, if the
            ;;  iteration finished without iterating through the entire
            ;;  sequence, either because we found ourselves in a
            ;;  sink-state, or we encountered a item for which no transition
            ;;  was possible.
            (cond
              (= false final-state) false
              (:accepting (state-vec final-state)) ((:exit-map dfa) final-state)
              :else false)))))))
