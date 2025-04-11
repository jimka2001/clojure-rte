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

(ns rte-construct
  (:refer-clojure :exclude [compile])
  (:require [genus :as gns]
            [util :refer [member exists setof forall
                                      call-with-collector defn-memoized defmulti-memoized defmethod-memoized
                                      visit-permutations fixed-point
                                      sort-operands
                                      seq-matcher
                                      rte-identity rte-constantly
                                      gc-friendly-memoize call-with-found find-first
                                      search-replace remove-element uniquify
                                      count-if-not find-simplifier]]
            [xymbolyco :as xym]
            [clojure.pprint :refer [cl-format]]
            [clojure.set :refer [union]]
            [cl-compat :as cl]
            [backtick :refer [template]]
            [genus-spec :as gs]
            )
)

;; allow rte/ prefix even in this file.
(alias 'rte 'rte-construct)

(declare ^:dynamic canonicalize-pattern canonicalize-pattern-impl)
(declare ^:dynamic compile rte-to-dfa)
(declare ^:dynamic canonicalize-pattern-once canonicalize-pattern-once-impl)
(declare ^:dynamic rte-inhabited? rte-inhabited?-impl)
(declare ^:dynamic nullable? nullable?-impl)

(declare traverse-pattern)
(declare match)
(declare rte-vacuous?)
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
  (gns/call-with-genus-env
   (fn []
     (binding [rte/compile (gc-friendly-memoize rte-to-dfa)
               canonicalize-pattern-once (gc-friendly-memoize canonicalize-pattern-once-impl)
               xym/optimized-transition-function (gc-friendly-memoize xym/optimized-transition-function-impl)
               canonicalize-pattern (gc-friendly-memoize canonicalize-pattern-impl)
               rte-inhabited? (gc-friendly-memoize rte-inhabited?-impl)
               nullable? (gc-friendly-memoize nullable?-impl)
               ;; rte-case-clauses-to-dfa (gc-friendly-memoize rte-case-clauses-to-dfa-impl)
               gs/spec-to-rte (gc-friendly-memoize gs/spec-to-rte-impl)]
       (thunk)))))

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
  `(with-compile-env []
     (call-with-rte '~bindings (fn ~'fn-with-rte [] ~@body))))

(defn rte? [t]
  (and (sequential? t)
       (= 'rte (first t))))

(defmethod gns/typep 'rte [a-value [_a-type pattern]]
  (and (sequential? a-value)
       (rte/match pattern a-value)))

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

        (gns/valid-type? pattern)
        :type

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
  {:client (fn traverse-client [pattern functions]
             (traverse-pattern pattern functions))
   :type (fn taverse-type [tag functions]
           ((:client functions) tag functions))
   :* (fn traverse-* [pattern functions]
        (cons :* ((:client functions) pattern functions)))
   :and (fn traverse-and [patterns functions]
          (cons :and (map (fn [expr]
                            ((:client functions) expr functions)) patterns)))
   :or (fn traverse-or [patterns functions]
         (cons :or (map (fn [expr]
                          ((:client functions) expr functions)) patterns)))
   :not (fn traverse-not [pattern functions]
          (cons :not ((:client functions) pattern functions)))
   :cat (fn traverse-cat [patterns functions]
          (cons :cat (map (fn [expr]
                            ((:client functions) expr functions)) patterns)))
   :sigma (fn traverse-sigma [pattern functions]
            ((:client functions) pattern functions))
   :empty-set (fn traverse-empty-set [pattern functions]
                ((:client functions) pattern functions))
   :epsilon (fn traverse-epsilon [pattern functions]
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

(defmethod expand-1 :default method-expand-1-default [pattern _functions]
  pattern)

(defmethod expand-1 'satisfies method-expand-1-satisfies [pattern _functions]
  (gns/expand-satisfies pattern))

(defmethod expand-1 '? method-expand-1-satisfies [pattern _functions]
  (gns/expand-satisfies pattern))

(defmethod expand-1 :? method-expand-1-question [pattern functions]
  (apply (fn
           ([] (invalid-pattern pattern functions '[:? []]))
           ([operand] `(:or :epsilon ~operand))
           ([_ & _] (invalid-pattern pattern functions '[:? [_ & _]]))) 
         (rest pattern)))

(defmethod expand-1 :+ method-expand-1-plus [pattern functions]
  (apply (fn
           ([] (invalid-pattern pattern functions '[:+ []]))
           ([operand] `(:cat ~operand (:* ~operand)))
           ([_ & _] (invalid-pattern pattern functions '[:+ [_ & _]])))
         (rest pattern)))

(defmethod expand-1 :permute method-expand-1-permute [pattern _functions]
  (apply (fn
           ([] :epsilon)
           ([operand] operand)
           ([_ & _]
            (let [operands (rest pattern)]
              (cons :or (call-with-collector (fn collect-permutation [collect]
                                               (visit-permutations
                                                (fn visit-permutation [perm]
                                                  (collect (cons :cat perm))) operands)))))))
         (rest pattern)))

(defmethod expand-1 :contains-any method-expand-1-contains-any [pattern _functions]
  (apply (fn
           ([] :epsilon)
           ([operand] operand)
           ([_ & _]
            (let [operands (rest pattern)]
              `(:cat ~sigma-*
                     (:or ~@operands)
                     ~sigma-*))))
         (rest pattern)))

(defmethod expand-1 :contains-every method-expand-1-contains-every [pattern _functions]
  (apply (fn
           ([] :epsilon)
           ([operand] operand)
           ([_ & _]
            (let [wrapped (map (fn map-wrapped [operand]
                                    `(:cat ~sigma-* ~operand ~sigma-*)) 
                                  (rest pattern))]
              `(:and ~@wrapped))))
         (rest pattern)))

(defmethod expand-1 :contains-none method-expand-1-contains-none [pattern _functions]
  ;; TODO, not sure what (:contains-none) should mean with no arguments.
  ;;    as implemented it is equivalent to (:not :epsilon) which seems wierd.
  `(:not (:contains-any ~@(rest pattern))))

(defmethod expand-1 :exp method-expand-1-exp [pattern functions]
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

(defmethod expand-1 'and method-expand-1-and [pattern functions]
  ;; convert (and a b c) => (:and a b c)
  ;;  i.e., (or (:and ...)) is not allowed, which probably means the user forgot a :
  (cons :and (rest (verify-type pattern functions))))

(defmethod expand-1 'or method-expand-1-or [pattern functions]
  ;; convert (or a b c) => (:or a b c)
  ;;  i.e., (or (:and ...)) is not allowed, which probably means the user forgot a :
  (cons :or (rest (verify-type pattern functions))))

(defmethod expand-1 'not method-expand-1-not [pattern functions]
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
   such as derivative, nullable?, first-types, or canonicalize-pattern
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

(defn-memoized [nullable? nullable?-impl]
  "Determine whether the given rational type expression is nullable.
  I.e., does the empty-word satisfy the expression."
  [expr]
  (boolean
   (traverse-pattern expr
                     (assoc *traversal-functions*
                            :client nullable?
                            :empty-set (rte-constantly false)
                            :epsilon (rte-constantly true)
                            :sigma   (rte-constantly false)
                            :type (rte-constantly false)
                            :* (rte-constantly true)
                            :cat (fn [operands _functions]
                                   (every? nullable? operands))
                            :and (fn [operands _functions]
                                   (every? nullable? operands))
                            :or (fn [operands _functions]
                                  (some nullable? operands))
                            :not (fn [operand _functions]
                                   (not (nullable? operand)))))))

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

                                          (nullable? head)
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

(def rte/catxy?
  "Predicate detecting (:cat x y z (:* (:cat x y z)))"
  (fn [r]
    (cond (not (rte/cat? r))
          false

          (< (count (operands r)) 2)
          false

          :else
          (let [right (last (operands r))
                left  (butlast (operands r))]
            (and (rte/*? right)
                 (rte/cat? (operand right))
                 (= left (operands (operand right))))))))

(def rte/plus?
  "there won't be a :+ in a pattern after expansion.
  However, this predicate indicates whether an expression is equivalent to a (:+ x)"
  (fn [r]
    (if (not (rte/cat? r))
      false
      (let [ops (operands r)]
        (and (= 2 (count ops))
             (or (and (rte/*? (nth ops 1))
                      (= (operand (nth ops 1)) (nth ops 0)))
                 (and (rte/*? (nth ops 0))
                      (= (operand (nth ops 0)) (nth ops 1)))))))))

(def rte/create-cat
  (fn [operands]
    (cond (empty? operands)
          :epsilon

          (empty? (rest operands))
          (first operands)

          :else
          (cons :cat operands))))

(def rte/create-or
  (fn [operands]
    (cond (empty? operands)
          :empty-set

          (empty? (rest operands))
          (first operands)

          :else
          (cons :or operands))))

(def rte/create-and
  (fn [operands]
    (cond (empty? operands)
          sigma-*

          (empty? (rest operands))
          (first operands)

          :else
          (cons :and operands))))

(defmulti zero
  type-dispatch)

(defmethod zero :or
  [_self]
  sigma-*)

(defmethod zero :and
  [_self]
  :empty-set)

(defmulti one
  type-dispatch)

(defmethod one :or
  [_self]
  :empty-set)

(defmethod one :and
  [_self]
  sigma-*)


(defmulti same-combination?
  (fn [self _operands]
    (type-dispatch self)))

(defmethod same-combination? :or
  [_self r]
  (rte/or? r))

(defmethod same-combination? :and
  [_self r]
  (rte/and? r))

(defmulti dual-combination?
  (fn [self _operands]
    (type-dispatch self)))

(defmethod dual-combination? :or
  [_self r]
  (rte/and? r))

(defmethod dual-combination? :and
  [_self r]
  (rte/or? r))

(defmulti set-dual-operation
  (fn [self _a _b]
    (type-dispatch self)))

(defmethod set-dual-operation :or
  ;; intersection
  [_self a b]
  (setof [x a] (member x b)))

(defmethod set-dual-operation :and
  ;; union
  [_self a b]
  (concat a (setof [x b] (not (member x a)))))

(defmulti set-operation
  (fn [self _a _b]
    (type-dispatch self)))

(defmethod set-operation :or
  ;; union
  [_self a b]
  (concat a (setof [x b] (not (member x a)))))

(defmethod set-operation :and
  ;; intersection
  [_self a b]
  (setof [x a] (member x b)))

(defmulti annihilator
  (fn [self _a _b]
    (type-dispatch self)))

(defmethod annihilator :or
  [_self a b]
  (gns/subtype? b a :dont-know))

(defmethod annihilator :and
  [_self a b]
  (gns/subtype? a b :dont-know))

(defmulti create-type-descriptor
  (fn [self _operands]
    (type-dispatch self)))

(defmethod create-type-descriptor :or
  [_self operands]
  (gns/create-or operands))

(defmethod create-type-descriptor :and
  [_self operands]
  (gns/create-and operands))

(defmulti or-invert
  (fn [self _x]
    (type-dispatch self)))

(defmethod or-invert :or
  [_self x]
  (not x))

(defmethod or-invert :and
  [_self x]
  x)

(defmulti create
  (fn [self _operands]
    (type-dispatch self)))

(defmethod create :or
  [_self operands]
  (create-or operands))

(defmethod create :and
  [_self operands]
  (create-and operands))

(def rte/create-not
  (fn [not-operand]
    (if (rte/not? not-operand)
      (operand not-operand)
      (list :not not-operand))))

(defmethod gns/-canonicalize-type 'rte method-canonicalize-type-rte
  [type-designator _nf]
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

(defn conversion-*-1
  [self]
  (let [op (operand self)]
    (cond (= :epsilon op)
          :epsilon

          (= :empty-set op)
          :epsilon

          (rte/*? op)
          op

          :else
          self)))

(defn conversion-*-2
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
             

(defn conversion-*-3
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

(defn conversion-*-99
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


(defn conversion-cat-1
  [self]
  (rte/create-cat (operands self)))

(defn conversion-cat-3
  [self]
  (if (member :empty-set (operands self))
    :empty-set
    self))

(defn conversion-cat-4
  [self]
  ;; remove  EmptyWord and flatten  Cat(Cat(...)...
  (rte/create-cat (mapcat (fn [rt]
                            (cond (= rt :epsilon)
                                  []

                                  (rte/cat? rt)
                                  (operands rt)

                                  :else
                                  [rt]))
                          (operands self))))
  
(defn conversion-cat-5
  [self]

  (letfn [;; Cat(..., x*, x, x* ...) --> Cat(..., x*, x, ...)
          (f [tail]
            (cond (< (count tail) 3)
                  tail

                  (and (= (first tail) (nth  tail 2))
                       (rte/*? (first tail))
                       (= (operand (first tail)) (second tail)))
                  (f (concat [(first tail) (second tail)]
                             (nthrest tail 3)))                          

                  :else
                  (cons (first tail) (f (rest tail)))))
          ;; and Cat(..., x*, x* ...) --> Cat(..., x*, ...)
          (g [tail]
            (cond (< (count tail) 2)
                  tail

                  (and (= (first tail) (second tail))
                       (rte/*? (first tail)))
                  (g (rest tail))

                  :else
                  (cons (first tail) (g (rest tail)))))]
    (rte/create-cat (f (g (operands self))))))


(defn conversion-cat-6
  [self]
  ;; Cat(A, B, X *, X, C, D) --> Cat(A, B, X, X *, C, D)
  ;; Cat(A, B, X *, X, X, C, D) --> Cat(A, B, X, X, X *, C, D)
  (letfn [(f [tail]
            (cond (empty? (rest tail))
                  tail

                  (and (rte/*? (first tail))
                       (= (operand (first tail)) (second tail)))
                  (cons (second tail) (f (cons (first tail)
                                               (rest (rest tail)))))

                  :else
                  (cons (first tail) (f (rest tail)))))]
    (rte/create-cat (f (operands self)))))

(defn conversion-cat-99
  [self]
  (rte/create-cat (map canonicalize-pattern-once (operands self))))

(defn conversion-combo-1
  [self]
  (create self (operands self)))

(defn conversion-combo-3
  [self]
  ;; Or(... Sigma * ....) -> Sigma *
  ;; And(... EmptySet....) -> EmptySet
  (if (member (zero self) (operands self))
    (zero self)
    self))

(defn conversion-combo-4
  [self]
  (create self (uniquify (operands self))))

(defn conversion-combo-5
  [self]
  (create self (sort-operands (operands self))))

(defn conversion-combo-6
  [self]
  ;; remove Sigma * and flatten And(And(...)...)
  ;; remove EmptySet and flatten Or(Or(...)...)
  (create self
          (mapcat (fn combo-6-mapcat [r]
                    (cond (= r (one self))
                          []

                          (same-combination? self r)
                          (operands r)

                          :else
                          [r]))
                  (operands self))))

(defn conversion-combo-7
  [self]
  ;; (:or A B (:* B) C)
  ;; --> (:or A (:* B) C)
  ;; (:and A B (:* B) C)
  ;; --> (:and A B C)
  (let [stars (filter rte/*? (operands self))]
    (if (empty? stars)
      self
      (create self
              (mapcat (fn [r]
                        (cond (and (rte/or? self)
                                   (exists [s stars]
                                           (= r (operand s))))
                              []

                              (and (rte/and? self)
                                   (rte/*? r)
                                   (member (operand r) (operands self)))
                              []

                              :else
                              [r]))
                      (operands self))))))

(defn conversion-combo-11
  [self]
  ;; And(...,x,Not(x)...) --> EmptySet
  ;; Or(...x,Not(x)...) --> SigmaStar
  (if (exists [r1 (operands self)]
              (and (rte/not? r1)
                   (member (operand r1) (operands self))))
    (zero self)
    self))

(defn conversion-combo-12
  [self]
  ;; sigmaSigmaStarSigma = Cat(Sigma, Sigma, sigmaStar)
  ;; Or(   A, B, ... Cat(Sigma,Sigma,Sigma*) ... Not(Singleton(X)) ...)
  ;;   --> Or( A, B, ... Not(Singleton(X))
  ;; This is correct because Cat(Σ,Σ,(Σ)*) is the set of all sequences of length 2 or more
  ;;    and Not(Singleton(X)) includes the set of all sequences of length 2 or more
  ;; Similarly, any Cat(...) which contains at least two non-nullables is either the
  ;;    empty set, or a set of sequences each of length 2 or more.
  ;;    And Not(Singleton(X)) contains all all sequences of length 2 or more.
  ;; So se can remove all such sequences.
  ;; E.g. Or(Singleton(SEql(1)),
  ;;         Cat(Singleton(SEql(1)),Singleton(SEql(2)),Singleton(SEql(3))),
  ;;         Not(Singleton(SEql(0))))
  ;;     we can remove Cat(...) because it contains at least 2 non-nullable items,
  ;;         and is therefore a subset of Not(Singleton(SEql(0)))
  ;; If we have  Or rather than And, then we can remove Not(Singleton(SEql(0)))
  ;;         because it is a superset of Cat(...)
  
  (let [cats (for [c (operands self)
                   :when (rte/cat? c)
                   :when (> (count-if-not nullable? (operands c)) 1)]
               c)
        not-sing (setof [n (operands self)]
                        (and (rte/not? n)
                             (gns/valid-type? (operand n))))]
    (cond (or (empty? not-sing)
              (empty? cats))
          self

          (rte/or? self)
          (create self (setof [op (operands self)]
                              (not (member op cats))))

          :else ;; (rte/and? self)
          (create self (setof [op (operands self)]
                              (not (member op not-sing)))))))

(defn conversion-combo-14
  [self]
  ;; generalization of conversionC11
  ;; Or(A,Not(B),X) -> Sigma* if B is subtype of A
  ;; And(A,Not(B),X) -> EmptySet if A is subtype of B
  (let [nots (for [r (operands self)
                   :when (rte/not? r)
                   :when (gns/valid-type? (operand r))]
               (operand r))
        singletons (filter gns/valid-type? (operands self))]
    (if (exists [sub nots]
                (exists [sup singletons]
                        (= (annihilator self sup sub) true)))
      (zero self)
      self)))

(defn conversion-combo-15
  [self]
  ;; simplify to maximum of one SMember(...) and maximum of one Not(SMember(...))
  ;; Or(<{1,2,3,4}>,<{4,5,6,7}>,Not(<{10,11,12,13}>,Not(<{12,13,14,15}>)))
  ;;   --> Or(<{1,2,3,4,6,7}>,Not(<{12,13}>))
  ;;
  ;; And(<{1,2,3,4}>,<{4,5,6,7}>,Not(<{10,11,12,13}>,Not(<{12,13,14,15}>)))
  ;;   --> And(<{3,4}>,Not(<{10,11,12,13,14,15}>))
  (let [members (filter (every-pred gns/valid-type? gns/member-or-=?) (operands self))
        not-members (for [nsm (operands self)
                          :when (rte/not? nsm)
                          :when (gns/valid-type? (operand nsm))
                          :when (gns/member-or-=? (operand nsm))]
                      nsm)]
    (if (or (empty? members)
            (empty? not-members))
      self
      (let [new-member-arglist (reduce #(set-operation self %1 %2)
                                       (gns/operands (first members))
                                       (map gns/operands members))
            new-member (if (empty? new-member-arglist)
                         (one self)
                         (gns/create-member new-member-arglist))
            new-not-member-arglist (reduce #(set-dual-operation self %1 %2)
                                           (gns/operands (operand (first not-members)))
                                           (for [nsm not-members]
                                             (gns/operands (operand nsm))))
            new-not-member (if (empty? new-not-member-arglist)
                             (one self)
                             (rte/create-not (gns/create-member new-not-member-arglist)))]
        (create self (uniquify (map (fn [op]
                                      (cond (member op members)
                                            new-member

                                            (member op not-members)
                                            new-not-member

                                            :else
                                            op))
                                    (operands self))))))))

(defn conversion-combo-16
  [self]
  ;; WARNING, this function assumes there are no repeated elements
  ;;     according to ==
  ;;     If there are repeated elements, both will be removed.
  
  ;; remove And superclasses
  ;; remove Or subclasses
  
  ;; Must be careful, e.g. if Or(A,B) with A a subset of B and B a subset of A
  ;;    but A != B, then don't remove both.
  (let [ss (filter gns/valid-type? (operands self))
        f (fn [i]
            (let [td (nth ss i)
                  right (if (exists [j (range (inc i) (count ss))]
                                    (= (annihilator self (nth ss j) td) true))
                          [td]
                          [])
                  ;; left is computed lazily, thus not at all if right is non-empty
                  left (for [j (range (inc i) (count ss))
                             :when (= (annihilator self td (nth ss j)) true)]
                         (nth ss j))]
              (if (empty? right)
                left
                right)))
        redundant (mapcat f (range (dec (count ss))))
        g (fn [op]
            (if (member op redundant)
              []
              [op]))              
        filtered (mapcat g (operands self))]
    (create self filtered)))

(defn conversion-combo-17
  [self]
  ;; And({1,2,3},Singleton(X),Not(Singleton(Y)))
  ;;  {...} selecting elements, x, for which SAnd(X,SNot(Y)).typep(x) is true
  ;; --> And({...},Singleton(X),Not(Singleton(Y)))

  ;; Or({1,2,3},Singleton(X),Not(Singleton(Y)))
  ;;  {...} deleting elements, x, for which SOr(X,SNot(Y)).typep(x) is true
  ;; --> Or({...},Singleton(X),Not(Singleton(Y)))
  (call-with-found gns/member-or-=? (operands self)
                   :if-not-found self
                   :if-found (fn [member-1]
                               (let [singletons (for [r (remove-element member-1 (operands self))
                                                      :when (or (gns/valid-type? r)
                                                                (and (rte/not? r)
                                                                     (gns/valid-type? (operand r))))]
                                                  r)
                                     f (fn [r]
                                         (cond (gns/valid-type? r)
                                               [r]

                                               (and (rte/not? r)
                                                    (gns/valid-type? (operand r)))
                                               [(gns/create-not (operand r))]

                                               :else
                                               []))
                                     looser (mapcat f singletons)]
                                 (if (empty? looser)
                                   self
                                   (let [td (create-type-descriptor self looser)
                                         rt (gns/create-member (setof [a (gns/operands member-1)]
                                                                      (or-invert self (gns/typep a td))))]
                                     (create self (search-replace (operands self)
                                                                  member-1
                                                                  rt))))))))

(defn conversion-combo-21
  [self]
  (let [singletons (mapcat (fn [r]
                             (cond (and (rte/and? self)
                                        (gns/valid-type? r))
                                   [r]

                                   (and (rte/or? self)
                                        (rte/not? r)
                                        (gns/valid-type? (operand r)))
                                   [(operand r)]

                                   :else
                                   []))
                           (operands self))]
    (if (exists [i (range (count singletons))]
                (exists [j (range (inc i) (count singletons))]
                        (gns/disjoint? (nth singletons i) (nth singletons j) false)))
      (zero self)
      self)))

(defn conversion-combo-99
  [self]
  (create self (map canonicalize-pattern-once (operands self))))

(defn conversion-and-7
  [self]
  (if (and (member :epsilon (operands self))
           (some gns/valid-type? (operands self)))
    :empty-set
    self))

(defn conversion-and-8
  [self]
  ;; if operands contains EmptyWord, then the intersection is either EmptyWord or EmptySet
  (cond (not (member :epsilon (operands self)))
        self

        (every? nullable? (operands self))
        :epsilon

        :else
        :empty-set))

(defn conversion-and-9
  [self]
  ;; if x matches only singleton then And(x,y*) -> And(x,y)
  (if (and (some gns/valid-type? (operands self))
           (some rte/*? (operands self)))
    (create self (map (fn [rt]
                        (if (rte/*? rt)
                          (operand rt)
                          rt))
                      (operands self)))
    self))
    

(defn conversion-and-10
  [self]
  ;; And(A,B,Or(X,Y,Z),C,D)
  ;; --> Or(And(A,B,   X,   C, D)),
  ;;        And(A,B,   Y,   C, D)),
  ;;        And(A,B,   Z,   C, D)))
  (call-with-found rte/or? (operands self)
                   :if-not-found self
                   :if-found (fn [ror-1]
                               (rte/create-or (for [r (operands ror-1)]
                                                (rte/create-and (search-replace (operands self) ror-1 r)))))))

(defn conversion-and-18
  [self]
  ;; if there is a singleton which is not inhabited
  (if (exists [r (operands self)]
              (and (gns/valid-type? r)
                   (= (gns/inhabited? r :dont-know) false)))
    :empty-set
    self))

(defn conversion-and-13
  [self]
  ;; if there is an explicit :sigma and also a singleton which is inhabited, then
  ;;  we can simply remove the sigma.
  (cond (not (member :sigma (operands self)))
        self

        (exists [r (operands self)]
                (and (not= :sigma r)
                     (gns/valid-type? r)
                     (gns/inhabited? r false)))
        (create self (remove-element :sigma (operands self)))

        :else
        self))

(defn conversion-and-17
  [self]
  ;; if And(...) contains a Cat(...) with at least 2 non-nullable components,
  ;;    then this Cat matches only sequences of length 2 or more.
  ;; If And(...) contains a singleton, then it matches only sequences
  ;;    of length 1, perhaps an empty set of such sequences if the singleton type
  ;;    is empty.
  ;; If both are true, then the And() matches EmptySet

  (let [vt (delay (some gns/valid-type? (operands self)))
        count-non-nullable (fn [c]
                             (count-if-not nullable? (operands c)))]
    (if (and (or (member :sigma (operands self))
                 @vt)
             (exists [c (operands self)]
                     (and (rte/cat? c)
                          (> (count-non-nullable c) 1))))
      :empty-set
      self)))

(defn conversion-and-17a
  [self]
  ;; if And(...) has more than one Cat(...) which has no nullable operand,
  ;;    then the number of non-nullables must be the same, else EmptySet.
  (let [cats (for [c (operands self)
                   :when (rte/cat? c)
                   :when (forall [td (operands c)]
                                 (not (nullable? td)))]
               (operands c))]
    (cond (empty? cats)
          self

          (= 1 (count cats))
          self

          (exists [i (range 1 (count cats))]
                  (not= (count (first cats))
                        (count (nth cats i))))
          ;; we found two Cat(...) of necessarily different lengths
          :empty-set

          :else
          self)))

(defn conversion-and-17a2
  [self]
  ;; if And(...) has more than one Cat(...) which has no nullable operand,
  ;;    We also replace the several Cat(...) (having no nullables)
  ;;    with a single Cat(...) with intersections of operands.
  ;;    And(Cat(a,b,c),Cat(x,y,z) ...)
  ;;    --> And(Cat(And(a,x),And(b,y),And(c,z),...)
  (let [cats (for [c (operands self)
                   :when (rte/cat? c)
                   :when (forall [td (operands c)]
                                 (not (nullable? td)))]
               (operands c))]
    (cond (empty? cats)
          self

          (= 1 (count cats))
          self

          (exists [i (range 1 (count cats))]
                  (not= (count (first cats))
                        (count (nth cats i))))
          ;; we found two Cat(...) of necessarily different lengths
          self

          :else
          (let [invert (for [i (range 0 (count (first cats)))]
                         (for [c cats]
                           (nth c i)))
                cat (rte/create-cat (for [r invert] (create self r)))]
            (create self (uniquify (map (fn [r]
                                          (if (and (rte/cat? r)
                                                   (member (operands r) cats))
                                            cat
                                            r))
                                        (operands self))))))))

(defn conversion-and-17b
  [self]
  ;; after 17a we know that if there are multiple Cats(...) without a nullable,
  ;;   then all such Cats(...) without a nullable have same number of operands
  ;;   have been merged into one Cat(...)
  ;;   So assure that all other Cats have no more non-nullable operands.
  (let [cats (filter rte/cat? (operands self))]
    (call-with-found (fn [c]
                       (forall [o (operands c)]
                               (not (nullable? o))))
                     cats
                     :if-not-found self
                     :if-found (fn [first-non-nullable-cat]
                                 (let [num-non-nullable (count (operands first-non-nullable-cat))
                                       count-non-nullable (fn [c]
                                                            (count-if-not nullable? (operands c)))]
                                   (if (exists [c cats]
                                               (> (count-non-nullable c) num-non-nullable))
                                     :empty-set
                                     self))))))

(defn conversion-and-17c
  [self]
  ;; if And(...) contains a Cat with no nullables, (or explicit Sigma or Singleton)
  ;;  then remove the nullables from ever other Cat with that many non-nullables.
  ;; Since 7b has run, there should be no cat with more than this many non-nullables.
  ;; find a Cat(...) with no nullables, there should be at most one because
  ;;    conversion17a as run.
  (if (not-any? rte/cat? (operands self))
    self
    (let [count-non-nullable (fn [c]
                               (count-if-not nullable? (operands c)))
          cat-non-nullable (find-first (fn [c]
                                         (and (rte/cat? c)
                                              (forall [o (operands c)]
                                                      (not (nullable? o)))))
                                       (operands self))
          num-non-nullables (cond  (member :sigma (operands self))
                                   1

                                   (some gns/valid-type? (operands self))
                                   1

                                   cat-non-nullable
                                   (count (operands cat-non-nullable))

                                   :else
                                   0)]
      (if (= 0 num-non-nullables)
        self
        (create self
                (map (fn [c]
                       (cond (not (rte/cat? c))
                             c

                             (= (count-non-nullable c) num-non-nullables)
                             (create-cat (for [o (operands c)
                                               :when (not (nullable? o))]
                                           o))

                             :else
                             c))
                     (operands self)))))))

(defn conversion-and-19
  [self]
  ;; if there is at least one singleton and zero or more Not(x) where x is a singleton
  ;;   then build a SimpleTypeD and ask whether it is inhabited.
  ;;   if it is not inhabited, then self converts to EmptySet
  (let [singletons (filter gns/valid-type? (operands self))]
    (if (empty? singletons)
      self
      (let [not-singletons (for [r (operands self)
                                 :when (rte/not? r)
                                 :when (gns/valid-type? (operand r))]
                             (gns/create-not (operand r)))
            canonicalized-singletons (gns/canonicalize-type (gns/create-and (concat singletons not-singletons))
                                                            :dnf)]
        (if (= (gns/inhabited? canonicalized-singletons :dont-know) false)
          :empty-set
          self)))))

(defn conversion-or-8
  [self]
  ;; (:or A :epsilon B (:cat X (:* X)) C)
  ;;   --> (:or A :epsilon B (:* X) C )
  ;; (:or :epsilon (:cat X (:* X)))
  ;;   --> (:or :epsilon (:* X))
  ;; (:or (:* Y) (:cat X (:* X)))
  ;;   --> (:or (:* Y) (:* X))
  (if (and (some nullable? (operands self))
           (some rte/plus? (operands self)))
    (letfn [(f [op]
              (cond (not (rte/cat? op))
                    op

                    (rte/plus? op)
                    ;; (:cat x (:* x)) -> (:* x)
                    ;; (:cat (:* x) x) -> (:* x)
                    (first (filter rte/*? (operands op)))

                    :else
                    op))]
      (create self (map f (operands self))))
    self))

(defn conversion-or-9
  [self]
  ;; (:or A :epsilon B (:cat X Y Z (:* (:cat X Y Z))) C)
  ;;   --> (:or A :epsilon B (:* (:cat X Y Z)) C )
  ;; (:or :epsilon (:cat X Y Z (:* (:cat X Y Z))))
  ;;   --> (:or :epsilon (:* (:cat X Y Z)))
  (if (and (some nullable? (operands self))
           (some catxy? (operands self)))
    (create self (map (fn [r]
                        (if (rte/catxy? r)
                          (last r)
                          r))
                      (operands self)))
    self))
    

(defn conversion-or-10
  [self]
  ;; (: or A :epsilon B (: * X) C)
  ;; --> (: or A B (: * X) C)
  (if (and (member :epsilon (operands self))
           (exists [r (operands self)]
                   (and (not= :epsilon r)
                        (nullable? r))))
    (create self (remove-element :epsilon (operands self)))
    self))
  
(defn conversion-or-11b
  [self]
  ;; if Sigma is in the operands, then filter out all singletons
  ;; Or(Singleton(A),Sigma,...) -> Or(Sigma,...)
  (if (member :sigma (operands self))
    (create self (mapcat (fn [op]
                           (cond (= op :sigma)
                                 [:sigma]

                                 (gns/valid-type? op)
                                 []

                                 :else
                                 [op]))
                         (operands self)))
    self))

(defn conversion-or-15
  [self]
  ;; Or(Not(A),B*,C) = Or(Not(A),C) if A and B  disjoint,
  ;;   i.e. remove all B* where B is disjoint from A
  (let [tds (for [r (operands self)
                  :when (rte/not? r)
                  :when (gns/valid-type? (operand r))]
              (operand r))
        stars (for [r (operands self)
                    :when (rte/*? r)
                    :when (gns/valid-type? (operand r))
                    :when (exists [a tds]
                                  (gns/disjoint? a (operand r) false))]
                r)]
    (cond (empty? tds)
          self

          (empty? stars)
          self

          :else
          (create self (for [r (operands self)
                             :when (not (member r stars))]
                         r)))))

(defmulti conversion-dual-16b
  type-dispatch)

(defmethod conversion-dual-16b :or
  [self]
  ;; Or(A, x, Not(y)) --> And(A, Not(x)) if x, y disjoint
  (let [nss (for [r (operands self)
                  :when (rte/not? r)
                  :when (gns/valid-type? (operand r))]
              ;; collect all td for each Not(Singleton(td))
              (operand r))]
    (create self (mapcat (fn [r]
                           (if (and (gns/valid-type? r)
                                    (exists [d nss]
                                            (gns/disjoint? r d false)))
                             []
                             [r]))
                         (operands self)))))

(defmethod conversion-dual-16b :and
  [self]
  ;;And(A, x, Not(y)) --> And(A, x) if x, y disjoint
  (let [ss (filter gns/valid-type? (operands self))]
    (create self (mapcat (fn [r]
                           (if (and (rte/not? r)
                                    (gns/valid-type? (operand r))
                                    (exists [d ss]
                                            (gns/disjoint? (operand r) d false)))
                             []
                             [r]))
                         (operands self)))))

(defn-memoized [canonicalize-pattern-once canonicalize-pattern-once-impl]
  "Rewrite the given rte pattern to a canonical form.
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
                                                 [conversion-*-1
                                                  conversion-*-2
                                                  conversion-*-3
                                                  conversion-*-99]))
                           :cat (fn [operands _functions]
                                  (find-simplifier (cons :cat operands)
                                                   [conversion-cat-1
                                                    conversion-cat-3
                                                    conversion-cat-4
                                                    conversion-cat-5
                                                    conversion-cat-6
                                                    conversion-cat-99]))
                           :not (fn [operand _functions]
                                  (find-simplifier (list :not operand)
                                                   [conversion-not-1
                                                    conversion-not-2
                                                    conversion-not-3
                                                    conversion-not-99]))
                           :and (fn [operands _functions]
                                  (find-simplifier (cons :and operands)
                                                   [conversion-combo-1
                                                    conversion-combo-3
                                                    conversion-combo-4
                                                    conversion-combo-6
                                                    conversion-and-7
                                                    conversion-combo-7
                                                    conversion-and-8
                                                    conversion-and-9
                                                    conversion-and-10
                                                    conversion-combo-11
                                                    conversion-combo-14
                                                    conversion-and-18
                                                    conversion-combo-12
                                                    conversion-and-13
                                                    conversion-combo-21
                                                    conversion-combo-15
                                                    conversion-combo-16
                                                    conversion-dual-16b
                                                    conversion-and-17
                                                    conversion-and-17a
                                                    conversion-and-17b
                                                    conversion-and-17c
                                                    conversion-and-19
                                                    conversion-combo-17
                                                    conversion-combo-99
                                                    conversion-combo-5
                                                    ]))
                           :or (fn [operands _functions]
                                 (find-simplifier (cons :or operands)
                                                  [conversion-combo-1
                                                   conversion-combo-3
                                                   conversion-combo-4
                                                   conversion-combo-6
                                                   conversion-combo-7
                                                   conversion-or-8
                                                   conversion-or-9
                                                   conversion-or-10
                                                   conversion-combo-11
                                                   conversion-combo-14
                                                   conversion-or-11b
                                                   conversion-combo-16
                                                   conversion-dual-16b
                                                   conversion-combo-12
                                                   conversion-or-15
                                                   conversion-combo-21
                                                   conversion-combo-15
                                                   conversion-combo-17
                                                   conversion-combo-99
                                                   conversion-combo-5])))))


;;(def count-canonicalize-pattern (atom 0))

(defn-memoized [canonicalize-pattern canonicalize-pattern-impl]
  "find the fixed point of canonicalize-pattern-once"
  [pattern]
  ;;(swap! count-canonicalize-pattern inc)
  ;;(when (zero? (mod @count-canonicalize-pattern 100))
  ;;  (prn [:count @count-canonicalize-pattern]))
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
      (member (rte/create-not expr) and-args)
      :empty-set

      (member expr and-args)
      :epsilon

      :else
      (throw (ex-info (cl-format nil "not yet implemented: derivative of ~A wrt ~A"
                              expr wrt)
                      {:error-type :rte-not-yet-implemented
                       :pattern expr
                       :wrt wrt
                       })))))

(defn derivative
  "Compute the Brzozowski rational expression derivative of the given
  rte pattern with respect to the given type wrt."
  [expr wrt factors disjoints]
  (letfn [(walk [patterns]
            (map (fn [p]
                   (derivative (canonicalize-pattern p) wrt factors disjoints))
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
                                          (member td factors)
                                          :epsilon

                                          (member td disjoints)
                                          :empty-set

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
                                                           :factors factors
                                                           :disjoints disjoints
                                                           :sub-types [{:type (template (and ~wrt ~expr))}
                                                                       {:type (template (and ~wrt (not ~expr)))}]}))))

                                :or (fn [operands _functions]
                                      (cons :or (walk operands)))
                                :and (fn [operands _functions]
                                       (cons :and (walk operands)))
                                :not (fn [operand _functions]
                                       (cons :not (walk (list operand))))
                                :cat (fn [[head & tail] _functions]
                                       (letfn [(term1 []
                                                 `(:cat ~(derivative head wrt factors disjoints)
                                                        ~@tail))
                                               (term2 []
                                                 (derivative `(:cat ~@tail) wrt factors disjoints))]
                                         (cond
                                           (nullable? head) ;; nu = :epsilon
                                           `(:or ~(term1) ~(term2))
                                           :else
                                           (term1))))
                                :* (fn [operand _functions]
                                     `(:cat ~(derivative operand wrt factors disjoints) (:* ~operand)))))))))


(defn derivative-1 [expr wrt]
  (derivative expr wrt () ()))

(defn find-all-derivatives 
  "Start with the given rte pattern, and compute its derivative with
  respect to all the values returned by first-types.  Continue
  computing the derivatives of each of the derivatives returned with
  respect to all of their first-types.  Continue this process until no
  more derivatives can be found.  Warning, the given pattern might not
  be an element of the return value.  I.e., the 0'th derivative is not
  guaranteed to be among the values returned.
  This function returns a pair [triples done]
  triples: a sequence of triples, where each triple is [rte-1 td rte-2]
      rte-2 = derivative of rte-1 with respect to td
              (derivative rte-1 td) --> rte-2
  done: a sequence of all rte-2 values
      which might or might not include rte-1
  "
  [pattern]
  (loop [to-do-patterns (list pattern)
         done #{}
         triples [] 
         ]
    (if (empty? to-do-patterns)
      [ triples (seq done)] ;; The return value of find-all-derivatives
      (let [[pattern & to-do-patterns] to-do-patterns]
        (if (done pattern)
          (recur to-do-patterns done triples)
          (letfn [(f [[acc-triples acc-derivs] [wrt-type factors disjoints]]
                    (let [triple [pattern wrt-type (derivative pattern wrt-type factors disjoints)]]
                      [(conj acc-triples triple)
                       (if (done (triple 2))
                         acc-derivs
                         (conj acc-derivs (triple 2)))]
                      )
                    )]
            (let [firsts (first-types pattern)
                  disjoined-triples (gns/mdtd firsts)
                  [new-triples new-derivatives] (reduce f [[] ()] disjoined-triples)]
              (recur (concat new-derivatives to-do-patterns)
                     (conj done pattern)
                     (concat triples new-triples)))))))))

(defn human-readable-find-all-derivatives
  "Similar to find-all-derivatives but returns a value which is easier for the
  human to understand. The return value is a map (key/value pairs)
  Each key is an rte, either the given pattern or some n'th derivative thereof.
  Each value is is a map of key/value pairs.
     Each key is a potential first-type of the rte,
     Each value is the derivative of the rte wrt that first-type."
  [pattern]
  (into {} (map (fn [[k seq-of-triples]]
                  [k 
                   (into {} (map (fn [pair] [(first pair) (second pair)])
                                 (map rest seq-of-triples)))])
                (group-by first
                          (first (rte/find-all-derivatives pattern))))))

(defn rte-combine-labels ""
  [label-1 label-2]
  (gns/canonicalize-type (gns/create-or [label-1 label-2])))

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
         grouped-by-src (group-by first triples)
         exit-value-function (constantly exit-value)]

     (xym/extend-with-sink-state
      (xym/map->Dfa
       {:pattern given-pattern
        :canonicalized pattern
        :exit-map exit-value-function
        :combine-labels rte-combine-labels
        :states
        (into {}
              (map (fn [deriv index]
                     (let [from-src (map rest (grouped-by-src index))
                           grouped-by-dst (group-by second from-src)
                           ;; grouped-by-dst is of the form
                           ;;  { dst-id-1 [(td-a dst-id-1)
                           ;;              (td-b dst-id-1)
                           ;;              (td-c dst-id-2)...],
                           ;;    dst-id-2 [(td-d dst-id-2)
                           ;;              (td-e dst-id-2) ...] ...
                           ;;  }
                           ;; derive transitions of the form
                           ;; [[td-x dst-id-1] [td-y dst-id-2] ...]
                           ;; where td-x is (or td-a td-b td-c ...) canonicalized
                           ;;   and td-y is (or td-d td-e ...) canonicalized
                           transitions (map (fn [[dst pairs]]
                                                [(gns/canonicalize-type
                                                  (gns/create-or (map first pairs)) :dnf) dst])
                                              grouped-by-dst)
                           ]
                       [index
                        (xym/map->State {:index index
                                         :initial (= 0 index)
                                         :accepting (nullable? deriv)
                                         :pattern deriv
                                         :transitions transitions})]))
                   derivatives (range (count derivatives))))})))))

(defn dispatch [obj _caller]
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

(defmulti-memoized [rte-inhabited? rte-inhabited?-impl]
  "Interface to determine whether the language of an rte is non-vacuous"
  (fn [rte]
    (dispatch rte 'rte-inhabited?)))

(defmethod-memoized rte-inhabited? :pattern [pattern]
  (rte-inhabited? (rte/compile pattern)))

(defmethod-memoized rte-inhabited? :Dfa [dfa]
  (boolean (some :accepting (xym/states-as-seq dfa))))

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

  (fn [rte _items & {_promise-disjoint :promise-disjoint
                     _hot-spot :hot-spot}]
    (dispatch rte 'rte/match)))

(defmethod rte/match :pattern
  [pattern items & {_promise-disjoint :promise-disjoint
                    hot-spot :hot-spot}]
  (rte/match (rte/compile pattern) items :promise-disjoint true :hot-spot hot-spot))

(defn slow-transition-function
  "Returns a unary function which can serve as the transfer function of a state
  in a Dfa.   The (returned) function takes an element, candidate, of an input sequence,
  and computes the id of the next state as a function of the labels of the transitions.
  The transitions and the sink-state-id are given when slow-transition-function is called."
  [transitions sink-state-id]
  (fn [candidate]
    ;; look through the sequence or [type-designator next-state-id] pairs.
    ;; if we find a type for which candidate is of that type,
    ;; then return the next-state-id,
    ;; else return the synk-state-id.
    (reduce (fn [acc [td next-state-id]]
              (if (gns/typep candidate td)
                (reduced next-state-id)
                acc)) sink-state-id transitions)))

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
        (letfn [(fast-transition-function [transitions]
                  (xym/optimized-transition-function transitions promise-disjoint sink-state-id))
                (transition-function [transitions]
                  ;; transition-function returns a function with lambda-list [item sink-state-id]
                  (if hot-spot
                    (fast-transition-function transitions)
                    (slow-transition-function transitions sink-state-id)))
                (consume [state-index item]
                  (let [state-obj (state-vec state-index)]
                    (cl/cl-cond
                     ((member state-obj sink-states)
                      (reduced false))
                     (((transition-function (:transitions state-obj)) item))
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

(defmulti valid-rte? type-dispatch)
(defmethod valid-rte? :or [[_ & args]]
  (every? valid-rte? args))
(defmethod valid-rte? :cat [[_ & args]]
  (every? valid-rte? args))
(defmethod valid-rte? :and [[_ & args]]
  (every? valid-rte? args))
(defmethod valid-rte? :or [[_ & args]]
  (every? valid-rte? args))
(defmethod valid-rte? :not [[_not & args]]
  (and (= 1 (count args))
       (valid-rte? (first args))))
(defmethod valid-rte? :* [[_ & args]]
  (and (= 1 (count args))
       (valid-rte? (first args))))
(defmethod valid-rte? :empty-word [_]
  true)
(defmethod valid-rte? :sigma [_]
  true)
(defmethod valid-rte? :empty-set [_]
  true)
(defmethod valid-rte? :default [type-designator]
  (gns/valid-type? type-designator))

(defn assert-valid-rte [rte]
  (if (valid-rte? rte)
    rte
    (throw (ex-info "invalid rte"
                    {:error-type :invalid-rte
                     :rte rte}))))

(defmethod gns/valid-type? 'rte [[_ pattern]]
  (rte/valid-rte? pattern))

;; programmatic constructors
(def rte/And
  (fn [& args]
    (assert-valid-rte (template (:and ~@args)))))

(def rte/Or
  (fn [& args]
    (assert-valid-rte (template (:or ~@args)))))

(def rte/Not
  (fn [arg]
    (assert-valid-rte (template (:not ~arg)))))

(def rte/Star
  (fn [arg]
    (assert-valid-rte (template (:* ~arg)))))

(def rte/Cat
  (fn [& args]
    (assert-valid-rte (template (:cat ~@args)))))

(def rte/Plus
  (fn [arg]
    (assert-valid-rte (template (:cat ~arg (:* ~arg))))))

(def rte/And-not
  (fn [arg & args]
    (assert-valid-rte (rte/And arg (rte/Not (apply rte/Or args))))))


(def rte/Question
  (fn [arg]
    (assert-valid-rte (template (:or :epsilon
                                     ~arg)))))
