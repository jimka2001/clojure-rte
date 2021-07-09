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

(ns clojure-rte.genus
  (:refer-clojure :exclude [satisfies?])
  (:require [clojure.set :refer [subset?]]
            [clojure.pprint :refer [cl-format]]
            [clojure.repl :refer [source-fn]]
            [clojure-rte.util :refer [exists-pair forall-pairs exists fixed-point
                                      partition-by-pred remove-element uniquify
                                      search-replace setof sort-operands
                                      seq-matcher member find-simplifier defn-memoized
                                      unchunk
                                      print-vals]]
            [clojure-rte.cl-compat :as cl]
            [clojure.reflect :as refl]
            [backtick :refer [template]]
            ))

;; allow gns/ prefix even in this file.
(alias 'gns 'clojure-rte.genus)

(declare annihilator)
(declare canonicalize-type)
(declare -canonicalize-type)
(declare check-disjoint)
(declare combinator)
(declare combo-filter)
(declare disjoint?)
(declare -disjoint?)
(declare dual-combinator)
(declare expand-satisfies)
(declare inhabited?)
(declare -inhabited?)
(declare operand)
(declare operands)
(declare subtype?)
(declare -subtype?)
(declare to-nf)
(declare type-equivalent?)
(declare type-predicate-to-type-designator)
(declare typep)
(declare unit)
(declare valid-type?)
(declare zero)

(def gns/and?
  "Detect sequence starting with the simple symbol and"
  (seq-matcher 'and))

(def gns/or? 
  "Detect sequence starting with the simple symbol or"
  (seq-matcher 'or))

(def gns/combo?
  "Detect sequence starting with the simple symbol or or and"
  (fn [td]
    (or (gns/or? td)
        (gns/and? td))))

(def gns/=?
  "Detect sequence starting with the simple symbol ="
  (seq-matcher '=))

(def gns/not?
  "Detect sequence starting with the simple symbol not"
  (seq-matcher 'not))

(def gns/member?
  "Detect sequence starting with the simple symbol member"
  (seq-matcher 'member))

(def gns/not-member-or-=?
  "Detect sequence (not (member ...)) or (not (= ...))"
  (fn [type-designator]
    (and (sequential? type-designator)
         (gns/not? type-designator)
         (or (gns/member? (second type-designator))
             (gns/=? (second type-designator))))))

(def gns/member-or-=?
  "Detect sequence (member ...) or (= ...)"
  (fn [type-designator]
    (or (gns/member? type-designator)
        (gns/=? type-designator))))

(def gns/satisfies?
  "Detect sequence starting with the simple symbol satisfies"
  (seq-matcher 'satisfies))

(defn class-designator?
  "Predicate to determine whether the given symbol designates a java class."
  [t]
  (and (symbol? t)
       (resolve t)
       (class? (resolve t))))

(defn find-class
  "Given a valid class-designator, return the (java) class or nil if not found."
  [class-name]
  (if (class-designator? class-name)
    (resolve class-name)
    nil))

(defn find-incompatible-members
  "Computes a lazy list of pairs [m1 m2] where m1 is an member of c1
  and m2 is a member of c2, where m1 and m2 have the same name
  and same parameter-types, thus c1 and c2 are incompatible."
  [c1 c2]
  {:pre [(and (or (symbol? c1) (class? c1))
              (or (symbol? c2) (class? c2)))]}
  (cond (symbol? c1)
        (find-incompatible-members (or (find-class c1)
                                       (throw (ex-info (format "find-incompatible-members: no such class %s" c1)
                                                       {:c1 c1
                                                        :c2 c2})))
                                   c2)

        (symbol? c2)
        (find-incompatible-members c1 
                                   (or (find-class c2)
                                       (throw (ex-info (format "find-incompatible-members: no such class %s" c2)
                                                       {:c1 c1
                                                        :c2 c2}))))

        :else
        (let [members-1 (:members (refl/type-reflect c1))
              members-2 (:members (refl/type-reflect c2))]
          (for [m1 members-1
                m2 members-2
                :when (and (= (:name m1) (:name m2))
                           (= (:parameter-types m1) (:parameter-types m2)))
                ]
            [m1 m2]))))

(defn compatible-members? [c1 c2]
  (empty? (find-incompatible-members c1 c2)))

(defn type-equivalent?
  "Test whether two type designators represent the same type."
  [t1 t2 default]
  {:pre [(member default '(true false :dont-know))]
   :post [(member % '(true false :dont-know))]}
  (let [sp1 (delay (subtype? t1 t2 :dont-know))
        can1 (delay (canonicalize-type :dnf t1))
        sp2 (delay (subtype? @can1 t2 :dont-know))
        can2 (delay (canonicalize-type :dnf t2))
        sp3 (delay (subtype? @can1 @can2 :dont-know))]
    ;; two types are equivalent if each is a subtype of the other.
    (cond (= t1 t2)
          true
          ;; However, if t1 is NOT a subtype of t2, don't test (subtype? t2 t1)
          ;;    as that may be compute intensive.
          (= @sp1 false)
          false

          (and (= @sp1 :dont-know)
               (= @sp2 false))
          false

          (and (= @sp1 :dont-know)
               (= @sp2 :dont-know)
               (= @sp3 false))
          false

          :else
          (subtype? @can2 @can1 default))))

(defn type-dispatch [type-designator]
  "Dispatch function for several defmulti's.  If the type-designator is a sequence,
  return its first element, else return the scaler itself."
  (if (sequential? type-designator)
    (first type-designator)
    type-designator))

(defmulti operands
  type-dispatch)

(defmulti operand
  type-dispatch)

(defn callable-designator? [f]
  (and (symbol? f)
       (resolve f)
       (fn? (deref (resolve f)))))

(def ^:dynamic *pseudo-type-functions*
  "List of function designators which will be trusted as operand of satisfies
  for the purse of valid-type?"
  ())

(defn-memoized [sort-method-keys -sort-method-keys]
  "Given a multimethod object, return a list of method keys.
  The :primary method comes first in the return list and the :default
  method has been filtered away."
  [f]
  (cons :primary (remove #{:primary :default} (keys (methods f)))))

(defn-memoized [class-primary-flag -class-primary-flag]
  "Takes a class-name and returns either :abstract, :interface, :public, or :final,
  or throws an ex-info exception."
  [t]
  (let [c (find-class t)
        r (refl/type-reflect c)
        flags (:flags r)]
    (cond
      (= c Object)
      :abstract
      (contains? flags :interface)
      :interface
      (contains? flags :final)
      :final
      (contains? flags :abstract)
      :abstract
      (= flags #{:public})
      :public
      
      :else
      (throw (ex-info (format "disjoint? type %s flags %s not yet implemented" t flags)
                      {:error-type :invalid-type-flags
                       :a-type t
                       :flags flags})))))

(defn- get-fn-source
  "Use the clojure.repl/source-fn function to extract a string representing the
  code body of the definition of the named function, fn-name.
  We then parse this string with read-string.
  If unable to get the string representing the function, nil is returned."
  [fn-name]
  (let [src-str (source-fn fn-name)]
    (cond
      (= nil src-str)
      nil

      :else
      (read-string src-str))))

(defn- extract-type-from-expression
  "After the expression representing the code body has been extracted from the code body
  of a type predicate, use several heursitics to match the code, to extract the type
  which is being implemented in the expression."
  [var-1 expr]
  (cond
    (not (sequential? expr))
    nil
    
    ;; (instance? clojure.lang.Symbol x)
    (and (= 3 (count expr))
         (= 'instance? (first expr)))
    (let [[_i type-designator var-2] expr]
      (if (and (= var-1 var-2)
               (symbol? type-designator))
        type-designator
        nil))
    
    ;; (symbol? x)
    (= 2 (count expr))
    (let [[type-predicate var-2] expr]
      (if (and (= var-1 var-2)
               (symbol? type-predicate))
        (type-predicate-to-type-designator type-predicate)
        nil))

    ;; (or ...)
    (= 'or (first expr))
    (let  [[_or & exprs] expr
           expanded (map (fn [ex]
                           (extract-type-from-expression var-1 ex))
                         exprs)
           ]
      (if (not (member nil expanded))
        (cons 'or expanded)
        nil))

    :else
    nil))

(def type-predicate-to-type-designator 
  "Look at the function definition s-expression of the named function, type-predicate,
  and apply heuristics to attempt to reverse-engineer the type being checked.  
  This works for type predicates as they are defined in core.clj."
  (fn [type-predicate]
    (let [fn-s-expression (get-fn-source type-predicate)]
      (cond
        (not (sequential? fn-s-expression))
        nil

        (empty? fn-s-expression)
        nil

        (= 3 (count fn-s-expression))
        (try (let [[_def name-1 [_fn name-2 [var-1] expr]] fn-s-expression]
               (if (and (= name-1 name-2)
                        (= _def 'def)
                        (= _fn 'fn))
                 (extract-type-from-expression var-1 expr)
                 nil))
             (catch Exception _
               nil))
        
        (= 6 (count fn-s-expression))
        (try
          (let [[_defn name doc-string attr-map [var-1] expr]
                fn-s-expression]
            (if (and (= _defn 'defn)
                     (symbol? name)
                     (string? doc-string)
                     (map? attr-map)
                     (symbol? var-1))
              (extract-type-from-expression var-1 expr)
              nil))
          (catch Exception _
            nil))

        :else
        nil))))

(defn expand-satisfies
  "Expand (satisfies rational?) to
  (or
    (or Integer Long clojure.lang.BigInt BigInteger Short Byte)
    clojure.lang.Ratio BigDecimal)
  if possible.  Otherwise expand the given type-designator simply to itself."
  [type-designator]
  (cond
   (not (sequential? type-designator))
   type-designator

   (empty? type-designator)
   type-designator

   (not= 'satisfies (first type-designator))
   type-designator

   (empty? (rest type-designator))
   (throw (ex-info "invalid type designator"
                   {:type-designator type-designator}))

   (not-empty (rest (rest type-designator)))
   (throw (ex-info "invalid type designator"
                   {:type-designator type-designator}))

   :else
   (let [[_satisfies pred] type-designator]
     (cl/cl-cond
      ((fn? pred)
       ;; If we are looking at (satisfies #function[clojure-rte.rte-core/eval16169/fn--16170])
       ;;   then just leave it as it is.
       type-designator)
      ((type-predicate-to-type-designator (second type-designator)))
      (:else
       type-designator)))))


(letfn [(type-min-max [atoms selector]
          (some (fn [sub]
                  (when (class-designator? sub)
                    (let [csub (find-class sub)]
                      (some (fn [super]
                              (when (class-designator? super)
                                (let [csuper (find-class super)]
                                  (and (not (= csub csuper))
                                       (isa? csub csuper)
                                       (selector sub super))))) atoms)))) atoms))]
  (defn type-min
    "Find an element of the given sequence which is a subtype
  of some other type and is not =.  not necessarily the global minimum."
    [atoms]
    (type-min-max atoms (fn [sub _] sub)))

  (defn type-max
    "Find an element of the given sequence which is a supertype
  of some other type and is not =.  not necessarily the global maximum"
    [atoms]
    (type-min-max atoms (fn [_ super] super))))

(defn map-type-partitions
  "Iterate through all the ways to partition types between a right and left set.
  Some care is made to prune branches which are provably empty.
  The given binary-fun will be called on all such pairs [(l1 l2 ... ln) (r1 r2 ... rm)]
  for which is provable that (and l1 l2 .. ln
                                  (not r1) (not r2) ... (not rm))
  is non-empty."
  [items binary-fun]
  (letfn [(remove-s*types [types isa]
            ;; generalization of remove-supertypes and remove-subtypes,
            ;; caller must either pass isa? or an isa? which reverse the arguments.
            (let [supers (for [t1 types
                               :when (class-designator? t1)
                               :let [c1 (find-class t1)]
                               t2 types
                               :when (and (not= t1 t2)
                                          (class-designator? t2))
                               :let [c2 (find-class t2)]
                               :when (and (not (= c1 c2))
                                          (isa c1 c2))
                               ]
                           t2)]
              (for [x types
                    :when (not (member x supers))]
                x)))
          (remove-supertypes [types]
            ;; Given a list of symbols designating types, return a new list
            ;; excluding those which are supertypes of others in the list.
            (remove-s*types types isa?))
          (remove-subtypes [types]
            ;; Given a list of symbols designating types, return a new list
            ;; excluding those which are subypes of others in the list.
            (remove-s*types types (fn [c1 c2] (isa? c2 c1))))
          (type-reduce [left right]
            (loop [left left
                   right right]
              (cond
                (and (< 1 (count left))
                     (member :sigma left))
                (recur (remove #{:sigma} left) right)

                :else [left right])))
          (recurring [items left right]
            (cl/cl-cond
             ((member :sigma right)
              ;; prune
              )
             ((and (member :sigma left)
                   right)
              (recurring items (remove #{:sigma} left) right))
             ((and left
                   (some (fn [t2]
                           (disjoint? t2 (first left) false)) (rest left)))
              ;; prune
              )
             ((and (not-empty left) (not-empty right)
                   ;; exists t2 in right such that t1 < t2
                   ;; then t1 & !t2 = nil
                   (exists [t2 right]
                           (subtype? (first left) t2 false)))
              ;; prune
              )

             ((and (not-empty left) (not-empty right)
                   ;; exists t1 in left such that t1 < t2
                   ;; then t1 & !t2 = nil
                   (exists [t1 left]
                           (subtype? t1 (first right) false)))
              ;; prune
              )

             ((empty? items)
              (let [[left right] (type-reduce (remove-supertypes left) (remove-subtypes right))]
                (binary-fun left right)))
             ;; TODO consider subsets A < B
             ;;    then A and ! B is empty
             ;;    A & B is A
             ;;    !A and !B is !B

             (:else
              (let [new-type (first items)]
                (case new-type
                  (nil) (recurring (rest items) left right)
                  (:sigma) (recurring (rest items) (cons new-type left) right)
                  (do
                    (recurring (rest items) (cons new-type left) (remove (fn [t2] (disjoint? t2 new-type false))
                                                                         right))
                    (if (some (fn [t2] (disjoint? new-type t2 false)) left)
                      (recurring (rest items) left right) ;;   Double & !Float, we can omit Float in right
                      (recurring (rest items) left (cons new-type right)))))))))]
    (recurring items () ())))

(def ^:dynamic *test-types*
  "Some type designators used for testing"
  '((satisfies integer?)
    (satisfies int?)
    (satisfies rational?)
    (satisfies ratio?)
    (satisfies string?)
    (satisfies keyword?)
    (satisfies symbol?)
    (satisfies decimal?)
    (satisfies float?)
    (satisfies seq?)
    java.io.Serializable
    java.lang.CharSequence
    java.lang.Comparable
    java.lang.Number
    java.lang.Object
    clojure.lang.IMeta
    (= 1)
    (= 0)
    (= -1)
    (= a)
    (= [1 2 3])
    (= [])
    (member [1 2 3] [1 2] [1] [])
    (member [1 2 3] [2 1 3])
    (member a b c "a" "b" "c")
    (member a b)
    (member 1 2 3)
    (member 2 3 4)
    (member 1.5 3.5)
    (member 4.5 6.5)
    (member "a" "b" "c")
    (member "a" "b" "c" 1 2 3)
    (member 1 "a")
    :sigma
    :empty-set
    ))

(defn gen-type
  "Generate a type designator for testing."
  ([size]
   (gen-type size *test-types*))
  ([size types]
   (if (< 0 size)
     (case (rand-nth '(or and not :else))
       (or) (template (or ~(gen-type (dec size) types)
                          ~(gen-type (dec size) types)))
       (and) (template (and ~(gen-type (dec size) types)
                            ~(gen-type (dec size) types)))
       (not) (template (not ~(gen-type (dec size) types)))
       (rand-nth types))
     (rand-nth types))))

(defn gen-inhabited-type 
  "gen-type may generate a type designator which reduces to :empty-set.
  gen-inhabited-type generates a type designator which is guaranteed
  NOT to reduce to :empty-set.  If filter (a unary predicate) is
  given, gen-inhabited-type loops until it generates a type which
  satisifies the predicate.  If the predicate is never satisifed, it
  will loop forever."
  ([size]
   (gen-inhabited-type size (constantly true)))
  ([size filter]
   (loop []
     (let [td (gen-type size)
           td-inhabited (inhabited? td false)]
       (if (and td-inhabited (filter td))
         td
         (recur))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; implementation of typep and its methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti typep 
  "(typep value type-descriptor)
  Like clojure.core/instance? except that the arguments are reversed, and the
  given type designator need not be a class.  The given type 
  designator may be a 1: class, 2: a symbol resolving to a class, or
  3: a CL style type designator such as
   (not A)
   (and A B)
   (or A B)
   (satisfies A)
   (= obj)
   (member a b c)
  Methods of typep should specify the symbol which distinguishes the type.
  For example, the method whose dispatch value is 'member handles the decision
  of (typep 3 '(member 1 2 3 4 5)), and the method whose dispatch value is
  :empty-set handles the decision of (typep 3 :empty-set)."
  (fn [_value type-designator]
    (type-dispatch type-designator)))

(defmethod typep :sigma [_ _]
  true)

(defmethod typep :empty-set [_ _]
  false)

(defmethod typep :default [a-value a-type]
  (cond
    (class? a-type)
    (instance? a-type a-value)
    
    (not (symbol? a-type))
    (throw (ex-info (format "typep: [178] invalid type of %s, expecting a symbol or class, (class? %s)=%s got %s"
                            a-type a-type (class? a-type) (type a-type))
                    {:error-type :invalid-type-designator
                     :a-type a-type
                     :a-value a-value
                     }))

    (not (resolve a-type))
    (throw (ex-info (format "typep: [179] invalid type %s, no resolvable value" a-type)
                    {:error-type :invalid-type-designator
                     :a-type a-type
                     :a-value a-value
                     }))

    (not (class? (resolve a-type)))
    (throw (ex-info (format "typep: [180] invalid type of %s, does not resolve to a class, got %s of type %s"
                            a-type (resolve a-type) (type (resolve a-type)))
                    {:error-type :invalid-type-designator
                     :a-type a-type
                     :a-value a-value
                     }))
    :else
    (isa? (type a-value) (resolve a-type))))

(defmethod typep 'not [a-value [_a-type t]]
  (not (typep a-value t)))

(defmethod typep 'and [a-value [_a-type & others]]
  (every? (fn [t1]
            (typep a-value t1)) others))

(defmethod typep 'satisfies [a-value [a-type f]]
  (boolean (cond (fn? f)
                 (f a-value)

                 (resolve f)
                 ((resolve f) a-value)

                 :else
                 (throw (ex-info (format "typep satisfies: unknown function %s" f)
                                 {:a-value a-value
                                  :a-type a-type
                                  :f f})))))

(defmethod typep '= [a-value [_type value]]
  (= value a-value))

(defmethod typep 'member [a-value [_type & others]]
  (member a-value others))

(defmethod typep 'or [a-value [_a-type & others]]
  (boolean (some (fn [t1]
                   (typep a-value t1)) others)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; implementation of canonicalize-typef and the methods of -canonicalize-type
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn canonicalize-type
  "Simplify the given type-designator to a stable form"
  ([type-designator]
   (canonicalize-type type-designator :dnf))
  ([type-designator nf]
   (fixed-point type-designator
                (fn [td]
                  (-canonicalize-type td nf))
                =)))

(defmulti -canonicalize-type
  "Methods of -canonicalize-type implement the behavior of canonicalize-type.
  The method of -canonicalize-type whose dispatch value is 'X handles
  the canonicalization of the type designator (X ...).
  Such a method may return the given value to indicate no further 
  canonicalization is possible, or should return a value which will be
  considered a simpler form.  For example, converting 
  (member 1 1 2 2 3) to (member 1 2 3)"
  (fn [td nf]
    (type-dispatch td)))

(defmethod -canonicalize-type :default
  [type-designator nf]
  (cond   
    (class-designator? type-designator)
    type-designator
    
    (member type-designator '(:sigma :empty-set))
    type-designator

    (not (sequential? type-designator))
    (throw (ex-info (format "-canonicalize-type: warning unknown type %s" type-designator)
                    {:error-type :not-a-sequence
                     :normal-form nf
                     :type-designator type-designator }))

    (not (valid-type? type-designator))
    (throw (ex-info (format "-canonicalize-type: warning unknown type %s" type-designator)
                    {:error-type :unknown-type
                     :normal-form nf
                     :type (type type-designator)
                     :type-designator type-designator }))

    :else
    type-designator
    ))

(defmethod -canonicalize-type 'satisfies
  [type-designator nf]
  (expand-satisfies type-designator))

(defmethod -canonicalize-type 'not
  [type-designator nf]
  (find-simplifier type-designator
                   [(fn [type-designator]
                      ;; (not (not x)) --> x
                      (if (gns/not? (second type-designator))
                        (second (second type-designator))
                        type-designator))
                    (fn [type-designator]
                      (if (= :sigma (second type-designator))
                        :empty-set
                        type-designator))
                    (fn [type-designator]
                      (if (and (class-designator? (second type-designator))
                               (= Object (find-class (second type-designator))))
                        ;; (not Object) --> :empty-set
                        :empty-set
                        type-designator))
                    (fn [type-designator]
                      (if (= :empty-set (second type-designator))
                        :sigma
                        type-designator))
                    (fn [type-designator]
                      (to-nf
                       (list 'not (-canonicalize-type (second type-designator) nf))
                       nf))]))

(defmethod -canonicalize-type 'fn*
  [type-designator nf]
  (find-simplifier type-designator
                   [(fn [type-designator]
                      ;; convert (fn* [p1__19751#] (clojure.core/even? p1__19751#))}
                      ;;     to (satisfies clojure.core/even?)
                      (cond
                        (not (and (= 3 (count type-designator))
                                  (vector (nth type-designator 1))
                                  (sequential? (nth type-designator 2))
                                  (= 2 (count (nth type-designator 2)))))
                        type-designator
                        :else
                        (let [[_fn [v1] [f1 v2]] type-designator]
                          (cond (= v1 v2)
                                (list 'satisfies f1)

                                :else
                                type-designator))))]))

(defmulti dual-combinator
  (fn [self a b]
    (type-dispatch self)))

(defmulti combinator
  (fn [self a b]
    (type-dispatch self)))

(defmethod combinator 'and
  [self a b]
  (setof [x a]
         (member x b)))

(defmethod dual-combinator 'and
  [self a b]
  (uniquify (concat a b)))

(defmethod combinator 'or
  [self a b]
  (uniquify (concat a b)))

(defmethod dual-combinator 'or
  [self a b]
  (setof [x a]
         (member x b)))
  

(defmulti combo-filter
  ""
  (fn [self pred xs]
    (type-dispatch self)))

(defmethod combo-filter 'or
  [self pred xs]
  (filter (fn [x] (not (pred x))) xs))

(defmethod combo-filter 'and
  [self pred xs]
  (filter pred xs))

(defmulti dual-combination?
  "Given this as an :or ask whether that is an :and,
  Given this as an :and ask whether that is an :or."
  (fn [this that]
    (type-dispatch this)))

(defmethod dual-combination? 'or
  [this td]
  (and (combo? td)
       (= 'and (first td))))

(defmethod dual-combination? 'and
  [this td]
  (and (combo? td)
       (= 'or (first td))))

(defmethod operands :default
  [self]
  (throw (ex-info (cl-format false "no operands for ~A" self)
                  {:self self})))

(defmethod operand :default
  [self]
  (throw (ex-info (cl-format false "no operand for ~A" self)
                  {:self self})))

(defmethod operands 'or
  [self]
  (rest self))

(defmethod operands 'and
  [self]
  (rest self))

(defmethod operands 'member
  [self]
  (rest self))

(defmethod operand '=
  [self]
  (second self))

(defmethod operand 'not
  [self]
  (second self))

(defmulti create 
  (fn [td operands]
    (type-dispatch td)))

(letfn [(cr [td operands]
          (cond (empty? operands)
                (unit td)

                (empty? (rest operands))
                (first operands)

                :else
                (cons (first td) operands)))]
  (defmethod create 'or
    [td operands]
    (cr td operands))
  (defmethod create 'and
    [td operands]
    (cr td operands)))

(defmulti create-dual
  "Given this as an :or and a list of operands, create an :and with those operands.
   Given this as an :and and a list of operands, create an :orwith those operands."
  (fn [this operands]
    (type-dispatch this)))

(defn create-member
  [operands]
  (cond (empty? operands)
        :empty-set

        (empty? (rest operands))
        (template (= ~(first operands)))

        :else
        (cons 'member operands)))

(defn create-or
  [operands]
  (create '(or) operands))

(defn create-and
  [operands]
  (create '(and) operands))

(defmethod create-dual 'or
  [this operands]
  (create-and operands))

(defmethod create-dual 'and
  [this operands]
  (create-or operands))

(defmulti compute-dnf
  "Convert to DNF"
  type-dispatch)

(defmulti compute-cnf
  "Convert to CNF"
  type-dispatch)

(defn to-nf
  ;; TODO add caching
  "Convert to DNF or CNF or leave as is"
  [td nf]
  (case nf
    (:dnf) (compute-dnf td)
    (:cnf) (compute-cnf td)
    :else td))

(defmethod compute-cnf :default
  [td]
  td)

(defmethod compute-dnf :default
  [td]
  td)

(defn compute-nf
  "it turns out SAnd.compute_dnf and SOr.compute_cnf contain
   the exact same code.  So I've factored that code here.
   convert SOr( x1, x2, SAnd(y1,y2,y3), x3, x4)
            --> td = SAnd(y1,y2,y3)
        --> SAnd(SOr(x1,x2,  y1,  x3,x4),
                 SOr(x1,x2,  y2,  x3,x4),
                 SOr(x1,x2,  y3,  x3,x4)
            )
        convert SAnd( x1, x2, SOr(y1,y2,y3), x3, x4)
           --> td = SOr(y1,y2,y3)
        --> SOr(SAnd(x1,x2,  y1,  x3,x4),
                 SAnd(x1,x2,  y2,  x3,x4),
                 SAnd(x1,x2,  y3,  x3,x4)
               )"
  [[combo & operands :as this]]
  (let [[dual-td & _ :as tds] (filter (fn [x] (dual-combination? this x))
                                      operands)]
    (if (empty? tds)
      this
      (create-dual this (map (fn [y]
                               (create this (map (fn [x]
                                                   (if (= x dual-td)
                                                     y x))
                                                 operands)))
                             (rest dual-td))))))

(defmethod compute-dnf 'and
  [td]
  (compute-nf td))

(defmethod compute-cnf 'or
  [td]
  (compute-nf td))

(defmethod compute-dnf 'not
  [[_not arg :as self]]
  (cond (gns/combo? arg)
        (create-dual arg (map (fn [td]
                                (template (not ~td)))
                              (rest arg)))

        :else
        self))

(defmethod compute-cnf 'not
  [self]
  (compute-dnf self))

(defmulti same-combination?
  ":sigma for and, :empty-set for or"
  (fn [td-1 td-2]
    (type-dispatch td-1)))

(defmethod same-combination? 'or
  [td td-2]
  (= 'or (first td-2)))

(defmethod same-combination? 'and
  [td td-2]
  (= 'and (first td-2)))

(defmulti unit 
  ":sigma for and, :empty-set for or"
  type-dispatch)

(defmethod unit 'or
  [td]
  :empty-set)

(defmethod unit 'and
  [td]
  :sigma)

(defmulti zero
  ":empty-set for and, :sigma for or"
  type-dispatch)

(defmethod zero 'and
  [td]
  :empty-set)

(defmethod zero 'or
  [td]
  :sigma)

(defmulti annihilator
  "subtype? for (and ...), supertype? for (or ...)"
  (fn [self td-1 td-2]
    (type-dispatch self)))

(defmethod annihilator 'and
  [self a b]
  (subtype? a b :dont-know))

(defmethod annihilator 'or
  [self a b]
  (subtype? b a :dont-know))


(defn conversion-1
  "(and) -> STop, unit = STop, zero = SEmpty
  (and x) -> x
  (and ...) -> (and ...)
  (or) -> SEmpty, unit = SEmpty, zero = STop
  (or x) -> x
  (or ...) -> (or ...)"
  [td]
  (create td (rest td)))

(defn conversion-2
  "(and A B SEmpty C D)-> SEmpty, unit = STop, zero = SEmpty
   (or A B STop C D) -> STop, unit = SEmpty, zero = STop"
  [[_ & operands :as td]]
  (if (member (zero td) operands)
    (zero td)
    td))

(defn conversion-3
  "(and A ( not A)) --> SEmpty, unit = STop, zero = SEmpty
   (or A ( not A)) --> STop, unit = SEmpty, zero = STop"
  [[_ & operands :as td]]
  (if (exists [n operands]
              (and (gns/not? n)
                   (member (second n) operands)))
    (zero td)
    td))

(defn conversion-4
  "SAnd(A, STop, B) == > SAnd(A, B), unit = STop, zero = SEmpty
   SOr(A, SEmpty, B) == > SOr(A, B), unit = SEmpty, zero = STop"
  [[_ & operands :as td]]
  (if (member (unit td) operands)
    (create td (remove-element (unit td) operands))
    td))

(defn conversion-5
  "(and A B A C) -> (and A B C)
   (or A B A C) -> (or A B C)"
  [[_ & operands :as td]]
  (create td (uniquify operands)))

(defn conversion-6
  "(and A ( and B C) D) --> (and A B C D)
   (or A ( or B C) D) --> (or A B C D)"
  [[_ & operands :as td]]
  (if (not (exists [td1 operands]
                   (and (gns/combo? td1)
                        (same-combination? td td1))))
    td
    (create td (mapcat (fn [td2]
                         (cond (not (gns/combo? td2))
                               [td2]
                               
                               (same-combination? td td2)
                               (rest td2)
                               
                               :else
                               [td2]))
                       operands))))
              
(defn conversion-7
  "Convert to DNF or CNF or leave as is depending on the nf argument"
  [td nf]
  (to-nf td nf))

(defn conversion-8
  "(or A (not B)) --> STop if B is subtype of A, zero = STop
   (and A (not B)) --> SEmpty if B is supertype of A, zero = SEmpty"
  [self]
  (if (exists [a (operands self)]
              (exists [n (operands self)]
                      (cl-format true "a=~A n=~A~%" a n)
                      (and (gns/not? n)
                           (= true (annihilator self a (operand n))))))
    (zero self)
    self))

(defn conversion-9
  "(A + B + C)(A + !B + C)(X) -> (A + B + C)(A + C)(X)
   (A + B +!C)(A +!B + C)(A +!B+!C) -> (A + B +!C)(A +!B + C)(A +!C)
   (A + B +!C)(A +!B + C)(A +!B+!C) -> does not reduce to(A + B +!C)(A +!B+C)(A)"
  [self]
  (let [combos (setof [td (operands self)] (gns/combo? td))
        duals (setof [td combos] (dual-combination? self td))]
    (letfn [(f [td]
              (if (not (member td duals))
                td
                (letfn [(pred [n]
                          (and (gns/not? n)
                               (exists [d duals]
                                       (= (operands d) (search-replace (operands td) n (operand n))))))]
                  ;; find to_remove=!B such that (A+!B+C) and also (A+B+C) are in the arglist
                  (let [to-remove (filter pred (operands td))]
                    (if (empty? to-remove)
                      td
                      (create td (remove-element (first to-remove) (operands td))))))))]
      (create self (map f (operands self))))))

(defn conversion-10
  "(and A B C) --> (and A C) if A is subtype of B
   (or A B C) -->  (or B C) if A is subtype of B"
  [self]
  (letfn [(pred [u]
            (exists [v (operands self)]
                    (and (not= u v)
                         (= (annihilator self u v) true))))]
    (let [subs (filter pred (operands self))]
      (if (empty? subs)
        self
        (let [sub (first subs)
              keep (setof [sup (operands self)]
                          (or (= sup sub)
                              (not= (annihilator self sub sup) true)))]
          (create self keep))))))

(defn conversion-11
  "A + !A B -> A + B
   A + !A BX + Y = (A + BX + Y)
   A + ABX + Y = (A + Y)"
  [self]
  (let [combos (filter combo? (operands self))
        duals (setof [td combos] (dual-combination? self td))]
    (letfn [(pred [a]
              (let [n (template (not ~a))]
                (exists [td duals]
                        (or (member a (operands td))
                            (member n (operands td))))))]
      (let [ao (filter pred (operands self))
            not-ao (template (not ~(first ao)))]
        (letfn [(consume [td]
                  (cond (not (combo? td))
                        [td]

                        (same-combination? self td)
                        [td]

                        (member (first ao) (operands td))
                        [] ;;  (A + ABX + Y) --> (A + Y)

                        (member not-ao (operands td))
                        ;; td is a dual, so td.create creates a dual
                        ;;    (A + !ABX + Y) --> (A + BX + Y)
                        [(create td (remove-element not-ao (operands td)))]

                        :else
                        [td]))]
          (create self (mapcat consume (operands self))))))))

(defn conversion-12
  "AXBC + !X = ABC + !X"
  [self]
  (let [combos (filter combo? (operands self))
        duals (setof [td combos] (dual-combination? self td))
        comp (filter (fn [n]
                       (and (not? n)
                            (exists [td duals]
                                    (member (operand n) (operands td)))))
                     (operands self))]
    (if (empty? comp)
      self
      (letfn [(f [td]
                (cond (gns/combo? td)
                      td

                      (not (dual-combination? self td))
                      td

                      :else
                      (create td (remove-element (operand (first comp))
                                                 (operands td)))))]
        (create self (map f (operands self)))))))


(defn conversion-13
  "multiple !member
    SOr(x,!{-1, 1},!{1, 2, 3, 4})
     --> SOr(x,!{1}) // intersection of non-member
    SAnd(x,!{-1, 1},!{1, 2, 3, 4})
     --> SOr(x,!{-1, 1, 2, 3, 4}) // union of non-member"
  [self]
  (let [not-members (setof [td (operands self)]
                           (and (gns/not? td)
                                (gns/member-or-=? (operand td))))]
    (if (< (bounded-count 2 not-members) 2)
      self
      (let [
            ;; find all the items in all the SNot(SMember(...)) elements
            ;;    this is a list of lists
            items (for [n not-members]
                    (operands (operand n)))
            ;; flatten the list of lists into a single list, either by
            ;;   union or intersection depending on SOr or SAnd
            combined (reduce (fn [x y]
                               (dual-combinator self x y))
                             items)
            new-not-member (template (not ~(create-member combined)))]
        (letfn [(f [td]
                  (if (member td not-members)
                    new-not-member
                    td))]
          ;; we replace all SNot(SMember(...)) with the newly computed
          ;;  SNot(SMember(...)), the remove duplicates.  This effectively
          ;;  replaces the right-most one, and removes all others.
          (create self (uniquify (map f (operands self)))))))))


(defn conversion-14
  "multiple member
   (or (member 1 2 3) (member 2 3 4 5)) --> (member 1 2 3 4 5)
   (or String (member 1 2 \"3\") (member 2 3 4 \"5\")) --> (or String (member 1 2 4))
   (and (member 1 2 3) (member 2 3 4 5)) --> (member 2 3)"
  [self]
  (let [members (filter gns/member-or-=? (operands self))]
    (if (< (bounded-count 2 members) 2)
      self
      (let [items (map operands members)
            combined (reduce (fn [x y]
                               (combinator self x y))
                             items)
            new-member (create-member combined)]
        (letfn [(f [td]
                  (if (member td members)
                    new-member
                    td))]
          (create self (uniquify (map f (operands self)))))))))

(defn conversion-15
  "SAnd(X, member1, not-member) --> SAnd(X,member2)
   SOr(X, member, not-member1) --> SOr(X,not-member2)
   
   after conversion13 and conversion14 there is a maximum of one SMember(...) and
   a maximum of one SNot(SMember(...))
   
   In the SAnd case we can remove the SNot(SMember(...)) and filter the SMember(...)
        to memberArgs andNot notMemberArgs.
   In the SOr  case we can remove the SMember(...) and filter the SNot(SMember(...))
        to notMemberArgs andNot memberArgs."
  [self]
  (letfn [(diff [xs ys]
            (setof [x xs]
                   (not (member x ys))))]
    (let [members (filter gns/member-or-=? (operands self))
          not-members (filter (fn [x] (and (gns/not? x)
                                           (gns/member-or-=? (operand x)))) (operands self))]
      (cond (empty? members)
            self

            (empty? not-members)
            self

            :else
            (letfn [(f [td]
                      (cond
                        ;; in the SAnd case we remove the not_member and filter the member args
                        (and (gns/and? self)
                             (= td (first not-members)))
                        []

                        (and (gns/and? self)
                             (= td (first members)))
                        [(create-member (diff (operands (first members))
                                              (operands (operand (first not-members)))))]

                        ;; in the SOr case we remove the member and filter the not-member args
                        (and (gns/or? self)
                             (= td (first members)))
                        []

                        (and (gns/or? self)
                             (= td (first not-members)))
                        [(template (not ~(create-member (diff (operands (operand (first not-members)))
                                                              (operands (first members))))))]

                        :else
                        [td]))]
              (create self (mapcat f (operands self))))))))

(defn conversion-16
  "Now(after conversions 13, 14, and 15, there is at most one SMember(...) and
   at most one SNot(SMember(...))
   (and Double (not (member 1.0 2.0 \"a\" \"b\"))) --> (and Double (not (member 1.0 2.0)))
   (or Double (member 1.0 2.0 \"a\" \"b\")) --> (and Double (member \"a\" \"b\"))"
  [self]
  (let [fewer (for [td (operands self)
                    :when (not (gns/member-or-=? td))
                    :when (not (and (gns/not? td)
                                    (gns/member-or-=? (operand td))))]
                td)
        stricter (create self fewer)]
    (letfn [(stricter-typep [x]
              (gns/typep x stricter))
            (f [td]
              (cond (gns/member-or-=? td)
                    (create-member (combo-filter self stricter-typep (operands td)))

                    (and (gns/not? td)
                         (gns/member-or-=? (operand td)))
                    (template (not ~(create-member (combo-filter self stricter-typep (operands (operand td))))))

                    :else
                    td))]
      (create self (map f (operands self))))))


(defmulti conversion-D1
  "Note this isn't consumed in SCombination:conversion16,
   conversion-16 converts SAnd(SMember(42, 43, 44, \"a\", \"b\", \"c\"), SInt)
   to SAnd(SMember(42, 43, 44), SInt)
   while conversion-D1() converts it to
   SMember(42, 43, 44)

   SAnd(SMember(42, 43, 44), A, B, C)
    == > SMember(42, 44)
   SOr(SNot(SMember(42, 43, 44, \"a\",\"b\")), String)
   == > SNot(SMember(42, 43, 44))"
  type-dispatch)

(defmethod conversion-D1 'and
  [self]
  (let [members (filter gns/member-or-=? (operands self))]
    (if (empty? members)
      self
      (create-member (setof [x (operands (first members))]
                            (gns/typep x self))))))

(defmethod conversion-D1 'or
  [self]
  ;; This is the dual of conversion-D1 'and,
  ;;   the code is a bit more complicated because we have to destructure (not (member ...))
  ;;   a couple of times.
  (let [not-members (for [td (operands self)
                          :when (gns/not? td)
                          :when (gns/member-or-=? (operand td ))]
                      td)]
    (if (empty? not-members)
      self
      (let [member-operands (for [x (operands (operand (first not-members)))
                                  :when (not (gns/typep x self))]
                              x)]
        (template (not ~(create-member member-operands)))))))

(defmulti conversion-D3
  "# discover disjoint pair
  SOr(SNot(A), SNot(B)) -> STop if A and B are disjoint
  SAnd(A, B) --> SEmpty if A and B are disjoint"
  type-dispatch)

(defmethod conversion-D3 'and
  [self]
  (if (exists-pair [[i j] (operands self)]
                   (disjoint? i j false))
    :empty-set
    self))

(defmethod conversion-D3 'or
  [self]
  (let [nots (filter gns/not? (operands self))]
    (if (exists-pair [[i j] nots]
                     (disjoint? (operand i) (operand j) false))
      :sigma
      self)))

(defn conversion-98
  "Sort the operands into deterministic order"
  [self]
  (create self (sort-operands (operands self))))

(defn conversion-99
  ""
  [td tf]
  td)


(defn combination-simplifiers [nf]
  [conversion-1
   conversion-2
   conversion-3
   conversion-4
   conversion-5
   conversion-6
   (fn [td] (conversion-7 td nf))
   conversion-8
   conversion-9
   conversion-10
   conversion-11
   conversion-12
   conversion-13
   conversion-14
   conversion-15
   conversion-16
   conversion-D1
   conversion-D3
   conversion-98
   (fn [td] (conversion-99 td nf))])

(defmethod -canonicalize-type 'and
  [type-designator nf]
  (find-simplifier type-designator
                   [(fn [type-designator]
                      (find-simplifier type-designator
                                       (combination-simplifiers nf)))
                    (fn [type-designator]
                      (if (member :empty-set (rest type-designator))
                        :empty-set
                        type-designator))
                    
                    (fn [type-designator]
                      (cons 'and (distinct (rest type-designator))))
                    
                    (fn [type-designator]
                      (cond (empty? (rest type-designator))
                            :sigma

                            (empty? (rest (rest type-designator)))
                            (second type-designator)

                            :else
                            type-designator))
                    
                    (fn [type-designator]
                      (if (some gns/=? (rest type-designator))
                        ;; (and Double (= "a")) --> (member)
                        ;; (and String (= "a")) --> (member "a")
                        (let [=-candidates (filter gns/=? (rest type-designator))
                              candidates (rest (first =-candidates))]
                          (cons 'member (filter (fn [x] (typep x type-designator)) candidates)))
                        type-designator))
                    
                    (fn [type-designator]
                      ;; (and Double (member 1.0 2.0 "a" "b")) --> (member 1.0 2.0)
                      (if (some gns/member? (rest type-designator))
                        (let [member-candidates (filter gns/member? (rest type-designator))
                              candidates (rest (first member-candidates))]
                          (cons 'member (filter (fn [x]
                                                  (typep x type-designator)) candidates)))
                        type-designator))
                    
                    (fn [type-designator]
                      (if (member :sigma (rest type-designator))
                        (cons 'and (map (fn [td] (-canonicalize-type td nf))
                                        (remove #{:sigma} (rest type-designator))))
                        type-designator))

                    ;; (and Long (not (member 1 2)) (not (member 3 4)))
                    ;;  --> (and Long (not (member 1 2 3 4)))
                    (fn [type-designator]
                      (let [not-member (filter gns/not-member-or-=? (rest type-designator))
                            remaining (remove (fn [t]
                                                (member t not-member)) (rest type-designator))
                            merged-member (mapcat (fn [[_not [_member & items]]]
                                                    items) not-member)]
                        (if (< (count not-member) 2)
                          type-designator
                          (template (and ~@remaining
                                         (not (member ~@merged-member)))))))

                    (fn [type-designator]
                      ;; (and Double (not (member 1.0 2.0 "a" "b"))) --> (and Double (not (member 1.0 2.0)))
                      ;; (and Double (not (= "a"))) --> (and Double  (not (member)))
                      (if (some gns/not-member-or-=? (rest type-designator))
                        (let [not-member (first (filter gns/not-member-or-=? (rest type-designator)))
                              [_not [_member & candidates]] not-member
                              remaining (remove (fn [t]
                                                  (= t not-member)) (rest type-designator))
                              filtered-td (template (and ~@remaining))
                              filtered-candidates (filter (fn [t2] (typep t2 filtered-td))
                                                          candidates)
                              repaired (gns/canonicalize-type (template (not (member ~@filtered-candidates))))
                              ]
                          (template (and ~@remaining ~repaired)))
                        type-designator))

                    (fn [type-designator]
                      ;; (and A (and B C) D) --> (and A B C D)
                      (if (exists [x (rest type-designator)]
                                  (gns/and? x))
                        (cons 'and
                              (mapcat (fn [x]
                                        (if (gns/and? x)
                                          (rest x)
                                          (list x))) (rest type-designator)))
                        type-designator))

                    (fn [type-designator]
                      (cons 'and (map (fn [td] (canonicalize-type nf td)) (rest type-designator))))]))

(defmethod -canonicalize-type 'member
  [type-designator nf]
  (find-simplifier type-designator
                   [(fn [type-designator]
                      (if (empty? (rest type-designator))
                        :empty-set
                        type-designator))
                    (fn [type-designator]
                      (if (empty? (rest (rest type-designator)))
                        (list '= (second type-designator))
                        type-designator))
                    (fn [type-designator]
                      (let [items (distinct (rest type-designator))]
                        (if (= items (rest type-designator))
                          type-designator
                          (cons 'member items))))]))

(defmethod -canonicalize-type 'or
  [type-designator nf]
  (find-simplifier type-designator
                   [(fn [type-designator]
                      (find-simplifier type-designator
                                       (combination-simplifiers nf)))
                    (fn [type-designator]
                      (if (member :empty-set (rest type-designator))
                        (cons 'or (map (fn [td] (-canonicalize-type td nf))
                                       (remove #{:empty-set} (rest type-designator))))
                        type-designator))

                    (fn [type-designator]
                      (cons 'or (distinct (rest type-designator))))

                    (fn [type-designator]
                      (cond (empty? (rest type-designator))
                            :empty-set

                            (empty? (rest (rest type-designator)))
                            (second type-designator)

                            :else
                            type-designator))

                    (fn [type-designator]
                      (if (member :sigma (rest type-designator))
                        :sigma
                        type-designator))

                    (fn [type-designator]
                      ;; (or (member 1 2 3) (member 2 3 4 5)) --> (member 1 2 3 4 5)
                      (if (<= 2 (bounded-count 2 (filter gns/member-or-=? (rest type-designator))))
                        (let [[members others] (partition-by-pred gns/member-or-=? (rest type-designator))
                              repaired (gns/canonicalize-type (template (member ~@(distinct (mapcat rest members)))) nf)]
                          (template (or ~repaired ~@others)))
                        type-designator))

                    (fn [type-designator]
                      ;; (or Double (member 1.0 2.0 "a" "b")) --> (or Double (member "a" "b"))
                      (if (some gns/member? (rest type-designator))
                        (let [mapped (map (fn [t]
                                            (cond
                                              (gns/member? t)
                                              (let [td2 (cons 'or (remove #{t} (rest type-designator)))]
                                                (cons 'member (filter (fn [candidate]
                                                                        (not (typep candidate td2)))
                                                                      (rest t))))
                                              :else
                                              t))
                                          (rest type-designator))]
                          (cons 'or mapped))
                        type-designator))
                    
                    (fn [type-designator]
                      (if (exists [x (rest type-designator)]
                                  (gns/or? x))
                        (cons 'or
                              (mapcat (fn [x]
                                        (if (gns/or? x)
                                          (rest x)
                                          (list x))) (rest type-designator)))
                        type-designator))

                    (fn [type-designator]
                      (to-nf (cons 'or (map (fn [td] (-canonicalize-type td nf))
                                     (rest type-designator))) nf))
                    ]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; implementation of valid-type? and its methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti valid-type?
  "Look at a type-desnignator and determine whether it is syntactically correct.
  Methods of valid-type? implement the behavior of valid-type?.
  The method of valid-type? whose dispatch value is 'X handles
  the validation the type designator (X ...).
  Such a method must return either true or false, depending on whether
  the operands are syntactically correct."
  type-dispatch)

(defmethod valid-type? :sigma [_]
  true)

(defmethod valid-type? :empty-set [_]
  true)

(defmethod valid-type? :default [type-designator]
  (boolean (find-class type-designator)))

(defmethod valid-type? 'not [[_not & type-designators]]
  (and (sequential? type-designators)
       (not-empty type-designators)
       (empty? (rest type-designators))
       (valid-type? (first type-designators))))

(defmethod valid-type? 'and [[_and & others]]
  (every? valid-type? others))

(defmethod valid-type? 'or [[_or & others]]
  (every? valid-type? others))

(defmethod valid-type? 'satisfies [[_ f]]
  (or (member f *pseudo-type-functions*)
      (callable-designator? f)))

(defmethod valid-type? '= [[_ & args]]
  (boolean (= 1 (count args))))

(defmethod valid-type? 'member [[_ & _]]
  true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; implementation of disjoint? and -disjoint?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn disjoint?
  "Predicate to determine whether the two types overlap.
  If it cannot be determined whether the two designated types
  are disjoint, then the default value is returned."
  [t1 t2 default]
  {:pre [(member default '(true false :dont-know))]
   :post [(member % '(true false :dont-know))]}
  (cond
    (not (inhabited? t1 true)) ;; if t1 is empty, t1 and t2 are disjoint
    true

    (not (inhabited? t2 true)) ;; if t2 is empty, t1 and t2 are disjoint
    true

    :else
    (let [try1 (check-disjoint t1 t2 :dont-know)]
      (if (not= :dont-know try1)
        try1
        (let [t1-simple (canonicalize-type t1 :dnf)
              t2-simple (canonicalize-type t2 :dnf)]
          (if (and (= t1-simple t1)
                   (= t2-simple t2))
            default
            (check-disjoint t1-simple t2-simple default)))))))

(defn-memoized [check-disjoint -check-disjoint]
  "Internal function used in top level disjoint? implementation."
  [t1' t2' default]
  (loop [[k & ks] (sort-method-keys -disjoint?)]
    (case ((k (methods -disjoint?)) t1' t2')
      (true) true
      (false) false
      (case ((k (methods -disjoint?)) t2' t1')
        (true) true
        (false) false
        (if ks
          (recur ks)
          default)))))

(defmulti -disjoint?
  "This function should never be called.
  Applications may install methods via (defmethod -disjoint? ...).
  The method accepts two arguments which are type-designators,
  [t1 t2],  pontentially application specific.
  The method should examine the designated types to determine whether
  the designated types are disjoint, i.e., whether they have no
  element in common, i.e., whether their intersection is empty.
  The method must return true, false, or :dont-know.
  The function, disjoint?, will call (-disjoint? t1 t2)
  and also (-disjoint? t2 t1) if necessary, therefore
  the methods need only check one or the other.
  When disjoint? (the public calling interface) is called,
  the methods of -disjoint? are called in some order
  (:primary first) until one method returns true or false,
  in which case disjoint? returns that value.
  If no method returns true or false, then disjoint?
  the function returns the given default value.

  The methods of 'disjoint? must specify a dispatch value.
  It is conventional that the method responsible for
  checking the disjointness of (X ...) vs Y, specify
  a dispatch value of 'X.  However, this is not enforced.
  For example, the implementator may wish to also check
  the disjointness of (not (X ...)), and there is already a method
  whose dispatch value is 'not.  For this reason the implementor
  might wish to add two methods, one with dispatch value 'X
  and one with dispatch value :not-X.  Any symbol or keyword
  is allowed but dispatch values must be unique.
  It is also conventional that the first thing checked
  in the method (defmethod -disjoint? 'X [t1 t2] ...)
  should be to detect whether t1 is a sequence whose
  first element is (= 'X)."
  (fn [t1 t2]
    (throw (ex-info "-disjoint? should not be called directly"
                    {:error-type :should-not-be-called-directly
                     :t1 t1
                     :t2 t2}))))

(defmethod -disjoint? :primary [t1 t2]
  (cond
    (= :empty-set t1)
    true
    
    (= :empty-set t2)
    true
    
    (= t1 t2)
    false
    
    (= :epsilon t1)
    false
    
    (= :epsilon t2)
    false
    
    (= :sigma t1)
    false
    
    (= :sigma t2)
    false
    
    (isa? t1 t2)
    false
    
    (isa? t2 t1)
    false
    
    :else
    :dont-know))

(defmethod -disjoint? :subtype [sub super]
  (cond (and (subtype? sub super false)
             (inhabited? sub false))
        false
        
        :else
        :dont-know))

(defmethod -disjoint? :classes [t1 t2]
  (if (and (class-designator? t1)
           (class-designator? t2))
    (let [c1 (find-class t1)
          c2 (find-class t2)]
      (cond (= c1 c2)
            false

            (isa? c1 c2)
            false

            (isa? c2 c1)
            false

            :else
            (let [ct1 (class-primary-flag t1)
                  ct2 (class-primary-flag t2)]
              (cond
                (or (= :final ct1)
                    (= :final ct2))
                true ;; we've already checked isa? above

                (or (= :interface ct1)
                    (= :interface ct2))
                (not (compatible-members? c1 c2))

                :else
                true))))
    
    :dont-know))

(defmethod -disjoint? 'or [t1 t2]
  (cond (not (gns/or? t1))
        :dont-know
        
        (every? (fn [t1'] (disjoint? t1' t2 false)) (rest t1))
        true

        (some (fn [t1'] (not (disjoint? t1' t2 true))) (rest t1))
        false
        
        :else
        :dont-know))

(defmethod -disjoint? 'and [t1 t2]
  (let [inhabited-t1 (delay (inhabited? t1 false))
        inhabited-t2 (delay (inhabited? t2 false))]
    (cond (not (gns/and? t1))
          :dont-know

          (exists [t (rest t1)]
                  (disjoint? t2 t false))
          ;; (disjoint? (and A B C) X)
          true

          ;; (disjoint? (and A B C) B)
          (and (member t2 (rest t1))
               @inhabited-t2
               @inhabited-t1)
          false
          
          ;; (disjoint? '(and B C) 'A)
          ;; (disjoint? '(and String (not (member a b c 1 2 3))) 'java.lang.Comparable)
          (and @inhabited-t2
               @inhabited-t1
               (exists [t1' (rest t1)]
                       (or (subtype? t1' t2 false)
                           (subtype? t2 t1' false)
                           )))
          false

          (and (class-designator? t2)
               (= (find-class t2) java.lang.Object)
               (some class-designator? (rest t1)))
          false

          ;; (disjoint? (and A B C) X)
          (and (class-designator? t2)
               (every? class-designator? (rest t1)))
          (not (forall-pairs [[a b] (conj (rest t1) t2)]
                             (or (isa? (find-class a) (find-class b))
                                 (isa? (find-class b) (find-class a))
                                 ;; TODO isn't this wrong? because () is not false
                                 (compatible-members? a b))))

          ;; I don't know the general form of this, so make it a special case for the moment.
          ;; (gns/disjoint? '(and Long (not (member 2 3 4))) 'java.lang.Comparable)
          ;;                      A   (not B)                     C
          ;; should return false
          ;; TODO generalize this special case.
          ;; If B < A and A !< B    and A < C and C !< A
          ;;   then (and A !B) is NOT disjoint from C
          (and (= 3 (count t1)) ;; t1 of the form (and x y)
               (gns/not? (first (rest (rest t1))))  ;; t1 of the form (and x (not y))
               (let [[_ A [_ B]] t1
                     C t2]
                 (and (subtype? B A false)
                      (not (subtype? A B true))
                      (subtype? A C false)
                      (not (subtype? C A true)))))
          false

          ;; (gns/disjoint? '(and String (not (member a b c 1 2 3))) 'java.lang.Comparable)
          ;;                       A     (not B)                      C
          ;;  since A and B are disjoint
          ;;  we may ask (disjoint? A C)
          (and (= 3 (count t1)) ;; t1 of the form (and x y)
               (gns/not? (nth t1 2))  ;; t1 of the form (and x (not y))
               (let [[_ A [_ B]] t1]
                 (disjoint? A B false)))
          (disjoint? (second t1) t2 :dont-know)

          :else
          :dont-know)))

(defmethod -disjoint? '= [t1 t2]
  (cond (gns/=? t1)
        (not (typep (second t1) t2))
        
        ;; (= ...) is finite, types are infinite
        ;; (disjoint? '(not (= 1)) 'Long)
        (and (gns/not? t1)
             (gns/=? (second t1))
             (class-designator? t2))
        false
        
        :else
        :dont-know))

(defmethod -disjoint? 'member [t1 t2]
  (cond (or (gns/member? t1)
            (gns/=? t1))
        (every? (fn [e1]
                  (not (typep e1 t2))) (rest t1))

        ;; (member ...) is finite, types are infinite
        ;; (disjoint? '(not (member 1 2 3)) 'Long)
        (and (gns/not-member-or-=? t1)
             (class-designator? t2))
        false
        
        (and (gns/not-member-or-=? t1)
             (gns/not-member-or-=? t2))
        false

        :else
        :dont-know))

(defmethod -disjoint? 'not [t1 t2]
  (cond
    (not (gns/not? t1))
    :dont-know
    
    ;; (disjoint? (not Object) X)
    (and (class-designator? (second t1))
         (isa? Object (find-class (second t1))))
    true

    ;; (disjoint? (not X) X)
    (= t2 (second t1))
    true
    
    ;; (disjoint? (not B) A)
    ;; (disjoint? '(not java.io.Serializable) 'Number)   as Number is a subclass of java.io.Serializable
    (and (class-designator? (second t1))
         (class-designator? t2)
         (subtype? t2 (second t1) false))
    true

    ;; (disjoint? (not B) A) ;; when A and B are disjoint
    (disjoint? t2 (second t1) false)
    false

    ;; (disjoint? '(not clojure.lang.IMeta) 'BigDecimal)
    ;;   we already know BigDecimal is not a subclass of clojure.lang.IMeta from above.
    (and (class-designator? t2)
         (class-designator? (second t1))
         (or (= :interface (class-primary-flag (second t1)))
             (= :interface (class-primary-flag t2)))
         (empty? (find-incompatible-members (second t1) t2)))
    false

    ;; (disjoint? '(not java.lang.Comparable) 'java.io.Serializable)  ;; i.e., :interface vs :interface
    ;; (disjoint? '(not java.lang.Number)     'clojure.lang.ISeq) ;; i.e. :interface vs (not :abstract)
    (and (class-designator? t2)
         (class-designator? (second t1))
         (member (class-primary-flag t2) '(:abstract :interface))
         (member (class-primary-flag (second t1)) '(:abstract :interface))
         (not (= (find-class t2) (find-class (second t1)))))
    false

    ;; (disjoint?   '(not java.io.Serializable) '(not java.lang.Comparable))
    (and (gns/not? t2)
         (class-designator? (second t1))
         (class-designator? (second t2)))
    false
    
    ;; if t2 < t1, then t2 disjoint from (not t1)
    ;; (disjoint? '(not (member a b c 1 2 3)) '(member 1 2 3) )
    (and (subtype? t2 (second t1) false)
         (not (subtype? (second t1) t2 true)))
    true

    ;; (disjoint?' (not (member 1 2 3)) '(member a b c 1 2 3) )
    (and (subtype? (second t1) t2 false)
         (not (subtype? t2 (second t1) true)))
    false

    ;; (disjoint? '(not Long) 'Number)
    (and (class-designator? t2)
         (class-designator? (second t1))
         (not (= (find-class (second t1)) (find-class t2)))
         (isa? (find-class (second t1)) (find-class t2)))
    false

    ;; (disjoint? '(not Boolean) '(not Long))
    ;; (disjoint? '(not A) '(not B))
    ;; if disjoint classes A and B
    (and (gns/not? t2)
         ;;(class-designator? (second t1))
         ;;(class-designator? (second t2))
         (disjoint? (second t1) (second t2) false))
    false

    ;; (disjoint? (not Long) (not (member 1 2 3 "a" "b" "c")))
    (and (gns/not? t2)
         (not (disjoint? (second t1) (second t2) true)))
    false         

    :else
    :dont-know))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; implementation of subtype? and -subtype?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn subtype?
  "Determine whether sub-designator specifies a type which is a subtype
  of super-designator. Sometimes this decision cannot be made/computed, in
  which case the given default value is interpreted as a binary
  function which is called, and its value returned.  The default value
  of default is one of: true, false, :dont-know or :error."
  [sub-designator super-designator default]
  {:pre [(member default '(true false :dont-know))]
   :post [(member % '(true false :dont-know))]}
  (loop [[k & ks] (sort-method-keys -subtype?)]
    (let [s ((k (methods -subtype?)) sub-designator super-designator)]
      (case s
        (true false) s
        (if ks
          (recur ks)
          default)))))

(defmulti -subtype?
  "This function should never be called.
  Applications may install methods via (defmethod -subtype? ...).
  The method accepts two arguments which are type-designators,
  [sub-designator super-designator],  pontentially application specific.
  The method should examine the designated types to determine whether
  they have a subtype relation, and return true, false, or :dont-know.
  When subtype? (the public calling interface) is called,
  the methods of -subtype? are called in some order
  (:primary first) until one method returns true or false,
  in which case subtype? returns that value.
  If no method returns true or false, then the default is
  returned.

  The methods of 'subtype? must specify a dispatch value.
  It is conventional that the method responsible for
  checking the subtypeness of (X ...) vs Y, or
  Y vs (X ...) specify a dispatch value of 'X.
  However, this is not enforced.
  For example, the implementator may wish to also check
  the subtypeness of (not (X ...)), and there is already a method
  whose dispatch value is 'not.  For this reason the implementor
  might wish to add two (or more) methods, one with dispatch value 'X
  and others whith dispatch values such as :not-X, :X-case-2."
  (fn [sub super]
    (throw (ex-info "-subtype? should not be called directly"
                    {:error-type :should-not-be-called-directly
                     :sub sub
                     :super super}))))

(defmethod -subtype? :primary [sub-designator super-designator]
  (let [super-canon (delay (gns/canonicalize-type super-designator))
        sub-canon   (delay (gns/canonicalize-type sub-designator))]
    (cond (and (class-designator? super-designator)
               (= Object (find-class super-designator)))
          true
          
          (and (class-designator? sub-designator)
               (class-designator? super-designator))
          (isa? (find-class sub-designator) (find-class super-designator))

          (= :sigma @super-canon)
          true
          
          (= :empty-set @sub-canon)
          true
          
          :else
          :dont-know)))

(defmethod -subtype? '= [sub super]
  (cond (gns/=? sub)
        (subtype? (cons 'member (rest sub)) super :dont-know)

        (gns/=? super)
        (subtype? sub (cons 'member (rest super)) :dont-know)

        :else
        :dont-know))

(defmethod -subtype? 'not [sub super]
  (cond (and (gns/not? super)  ; (subtype? 'Long '(not Double))
             (disjoint? sub (second super) false))
        true

        ;; (subtype? '(not Double) 'Long) = false
        ;; but not (subtype? '(not Double) '(or Long (not Double))) != false
        (and (gns/not? sub)
             (class-designator? super)
             (class-designator? (second sub))
             (disjoint? super (second sub) false))
        false

        (and (gns/not? sub)
             (type-equivalent? (second sub) super false))
        false

        (and (gns/not? super)
             (type-equivalent? sub (second super) false))
        false

        (and (gns/not? sub)
             (inhabited? (second sub) false)
             (subtype? (second sub) super false)
             (inhabited? (template (not ~super)) false))
        ;; TODO to test this take random tds td1, td2, and assert that (or (not td1) (not td2))
        ;;    is a subtype of (not (and td1 td2)) especially in the case that td1 and td2 are disjoint
        false
        
        (not (gns/not? sub))
        :dont-know

        (not (gns/not? super))
        :dont-know

        :else
        ;; isn't this just the same as directly returning x?
        (let [x (subtype? (second super) (second sub) :dont-know)]
          (if (= :dont-know x)
            :dont-know
            x))))

(defmethod -subtype? 'member [sub super]
  (cond (gns/member? sub)
        (every? (fn [e1]
                  (typep e1 super)) (rest sub))

        ;; (subtype? 'Long '(member 1 2 3))
        (and (gns/member? super)
             (class-designator? sub)) ;; assuming a class is infinite
        false

        ;; (subtype? 'Long '(not (member 1 2 3))) ==> false
        ;; (subtype? 'Long '(not (member 1.1 2 3))) ==> false
        ;; (subtype? 'Long '(not (member 1.1 2.2 3.3))) ==> true
        (and (gns/not? super)
             (class-designator? sub)
             (gns/member? (second super)))
        (every? (fn [e2]
                  (not (typep e2 sub))) (rest (second super)))

        :else
        :dont-know))

(defmethod -subtype? 'or [t1 t2]
  (cond
    (not (gns/or? t1))
    :dont-know

    :else
    (let [values (map (fn [t] (subtype? t t2 :dont-know))
                      (unchunk (rest t1)))]
      (cond (every? true? values)
            true

            (some false? values)
            false

            :else
            :dont-know))))

(defmethod -subtype? 'and [t1 t2]
  (cond
    (not (gns/and? t1))
    :dont-know
    
    (member t2 (rest t1))
    ;; (subtype? (and A B) A)
    true

    
    (exists [t (rest t1)] (subtype? t t2 false))
    ;; (subtype?  '(and String (not (member "a" "b" "c")))  'java.io.Serializable)
    true


    ;; (subtype? '(and A B) '(not A))
    (and (gns/not? t2)
         (exists [t (rest t1)]
                 (= t (second t2)))
         (inhabited? t1 false))
    false

    ;; (subtype? (and A B C X Y) (and A B C) )
    (and (gns/and? t2)
         (subset? (set (rest t2)) (set (rest t1))))
    true
    
    :else
    :dont-know))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; implementation of inhabite? and -inhabited?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn inhabited?
  "Given a type-designator, perhaps application specific,
  determine whether the type is inhabited, i.e., not the
  empty type."
  [type-designator default]
  {:pre [(member default '(true false :dont-know))]
   :post [(member % '(true false :dont-know))]}
  (letfn [(calc-inhabited [type-designator default]
            (loop [[k & ks] (sort-method-keys -inhabited?)]
              (case ((k (methods -inhabited?)) type-designator)
                (true) true
                (false) false
                (if ks
                  (recur ks)
                  default))))]
    (let [i (calc-inhabited type-designator :dont-know)
          td-canon (delay (canonicalize-type type-designator :dnf))]
      (cond (member i '(true false))
            i

            (= @td-canon type-designator)
            default
            
            :else
            (calc-inhabited @td-canon default)))))

(defn vacuous? 
  "Determine whether the specified type is empty, i.e., not inhabited."
  [type-designator]
  (let [inh (inhabited? type-designator :dont-know)]
    (if (= :dont-know inh)
      :dont-know
      (not inh))))

(defmulti -inhabited?
  "This function should never be called.
  Applications may install methods via (defmethod -inhabited? ...).
  The method accepts one argument which is a type-designator,
  pontentially application specific.
  The method should examine the type designator and return
  true, false, or :dont-know.
  When inhabited? (the public calling interface) is called,
  the methods of -inhabited? are called in some order
  (:primary first) until one method returns true or false,
  in which case inhabited? returns that value.

  Each method of -inhabited? must specify a dispatch value.
  Conventionally a dispatch value of 'X indicates the code
  to determine whether the type designator (X ...) is inhabited.
  However, this is not enforced.  Such code may also check, for
  example, the habitation of (not (X ...)), or the implementor
  might elect to implement a different method for this purpose.
  For this reason the code within the must must both determine
  applicability, (is the type designator one we are interested),
  and if applicable, then logic to determine habitation.
  "
  (fn [type-designator]
    (throw (ex-info "-inhabited? should not be called directly"
                    {:type-designator type-designator
                     :error-type :should-not-be-called-directly}))))

(defmethod -inhabited? :primary [type-designator]
  (cond
    (class-designator? type-designator)
    true

    (= :sigma type-designator)
    true
    
    (= :empty-set type-designator)
    false

    :else    
    :dont-know))

(defmethod -inhabited? 'or [t1]
  (cond
    (not (gns/or? t1))
    :dont-know
    
    :else
    (let [values (map (fn [t] (inhabited? t :dont-know))
                      (unchunk (rest t1)))]
      ;; we are depending on the fact that map is lazy here.
      ;; i.e. if true appears in the values list, then we
      ;; dont call inhabited? on any elements of (rest t1)
      ;; that come to the right of that element.
      (cond (some true? values)
            true

            (every? false? values)
            false

            :else
            :dont-know))))

(defmethod -inhabited? 'and [t1]
  (cond
    (not (gns/and? t1))
    :dont-know

    (and (< 2 (count t1))
         (forall-pairs [[a b] (rest t1)]
                       (and (or (class-designator? a)
                                (and (gns/not? a)
                                     (class-designator? (second a))))
                            (or (class-designator? b)
                                (and (gns/not? b)
                                     (class-designator? (second b))))
                            (not (disjoint? a b true)))))
    true

    (exists-pair [[a b] (rest t1)]
                 (and (or (class-designator? a)
                          (and (gns/not? a)
                               (class-designator? (second a))))
                      (or (class-designator? b)
                          (and (gns/not? b)
                               (class-designator? (second b))))
                      (disjoint? a b false)))
    false
    
    ;; if any of the types are empty, the intersection is empty,
    ;;   even if others are unknown.  (or A B empty D E).
    ;;   Careful, the computation here depends on laziness of map.
    ;;   Once a false is found, we don't call inhabited? on the remaining
    ;;   part of (rest t1).
    (some false? (map (fn [t] (inhabited? t :dont-know))
                      (unchunk (rest t1))))
    false
    
    ;; (and A (not (member ...))) is inhabited if A is inhabited and infinite because (member ...) is finite

    (and (not-empty (filter class-designator? (rest t1)))
         (= 1 (count (filter gns/not-member-or-=?
                             (rest t1))))
         (let [t2 (canonicalize-type (cons 'and
                                           (remove gns/not-member-or-=?
                                                   (rest t1))))]
           (inhabited? t2 false)))
    true

    ;; (and ... A ... B ...) where A and B are disjoint, then vacuous
    (exists-pair [[ta tb] (rest t1)]
                 (disjoint? ta tb false))
    false

    :else
    :dont-know))

(defmethod -inhabited? 'not [t1]
  (cond (not (gns/not? t1))
        :dont-know

        (class-designator? (second t1))
        (not= Object (find-class (second t1))) ;; (not Object) is empty, (not any-other-class) is inhabited

        (or (gns/member? (second t1))
            (gns/=? (second t1)))
        true

        :else
        :dont-know))

(defmethod -inhabited? 'member [t1]
  (cond (not (gns/member? t1))
        :dont-know

        (empty? (rest t1))
        false
        
        :else    
        true))

(defmethod -inhabited? '= [t1]
  (if (gns/=? t1)
    true
    :dont-know))
