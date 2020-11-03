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

(ns clojure-rte.genus
  (:refer-clojure :exclude [satisfies?])
  (:require [clojure.set :refer [subset?]]
            [clojure.repl :refer [source-fn]]
            [clojure.pprint :refer [cl-format]]
            [clojure-rte.util :refer [exists-pair forall-pairs exists fixed-point
                                      partition-by-pred
                                      member find-simplifier defn-memoized
                                      ]]
            [clojure-rte.cl-compat :as cl]
            [clojure.reflect :as refl]
            ))

(declare subtype?)
(declare disjoint?)
(declare inhabited?)
(declare type-equivalent?)

(defn and? [t]
  (and (sequential? t)
       (= 'and (first t))))

(defn or? [t]
  (and (sequential? t)
       (= 'or (first t))))

(defn =? [t]
  (and (sequential? t)
       (= '= (first t))
       (= (count t) 2)))

(defn not? [t]
  (and (sequential? t)
       (= 'not (first t))))

(defn member? [t]
  (and (sequential? t)
       (= 'member (first t))))

(defn satisfies? [t]
  (and (sequential? t)
       (= 'satisfies (first t))))

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
  [t1 t2 default]
  {:pre [(member default '(true false :dont-know :error))]
   :post [(fn [v] (member v '(true false :dont-know)))]}
  (if (= t1 t2)
    true
    (letfn [(conclude [t1-designator t2-designator]
              (if (= :error default)
                (throw (ex-info (format "type-equivalent? cannot decide %s vs %s" t1-designator t2-designator)
                                {:error-type :not-yet-implemented
                                 :type-designators [t1-designator t1-designator]}))
                default))]
      ;; two types are equivalent if each is a subtype of the other.
      ;; However, if t1 is NOT a subtype of t2, don't test (subtype? t2 t1)
      ;;    as that may be compute intensive.
      (let [s1 (subtype? t1 t2 :dont-know)
            s2 (delay (subtype? t2 t1 :dont-know))]
        (case s1
          (false) false
          (case @s2
            (false) false
            (if (= true s1 @s2)
              true
              (conclude t1 t2))))))))

(defmulti typep 
  "(typep value type-descriptor)
  Like clojure.core/instance? except that the arguments are reversed, and the
  given type designator need not be a class.  The given type 
  designator may be a (1) class, (2) a symbol resolving to a class, or
  (3) a CL style type designator such as
  (not A)
  (and A B)
  (or A B)
  (satisfies A)
  (= obj)
  (member a b c)"
  (fn [_value type-designator]
    (if (sequential? type-designator)
      (first type-designator)
      type-designator)))

(defmulti -canonicalize-type
  (fn [type-designator]
    (if (sequential? type-designator)
      (first type-designator)
      type-designator)))

(defn canonicalize-type
  "Simplify the given type-designator to a stable form"
  [type-designator]
  (fixed-point type-designator -canonicalize-type =))

(defmulti valid-type?
  "Look at a type-designator and determine whether it is syntactically correct"
  (fn [type-designator]
    (if (sequential? type-designator)
      (first type-designator)
      type-designator)))

(defmethod typep :sigma [_ _]
  true)

(defmethod valid-type? :sigma [_]
  true)

(defmethod typep :empty-set [_ _]
  false)

(defmethod valid-type? :empty-set [_]
  true)

(defmethod typep :default [a-value a-type]
  (cond
    (class? a-type)
    (instance? a-type a-value)
    
    (not (symbol? a-type))
    (throw (ex-info (format "typep: [178] invalid type of %s, expecting a symbol or class , got %s" a-type (type a-type))
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

(defmethod valid-type? :default [type-designator]
  (boolean (find-class type-designator)))

(defmethod typep 'not [a-value [_a-type t]]
  (not (typep a-value t)))

(defmethod valid-type? 'not [[_not & type-designators]]
  (and (sequential? type-designators)
       (not (empty? type-designators))
       (empty? (rest type-designators))
       (valid-type? (first type-designators))))

(defmethod typep 'and [a-value [_a-type & others]]
  (every? (fn [t1]
            (typep a-value t1)) others))

(defmethod valid-type? 'and [[_and & others]]
  (every? valid-type? others))

(defmethod typep 'or [a-value [_a-type & others]]
  (some (fn [t1]
          (typep a-value t1)) others))

(defmethod valid-type? 'or [[_or & others]]
  (every? valid-type? others))

(defmethod typep 'satisfies [a-value [_a-type f]]
  (if (fn? f)
    (f a-value)
    ((resolve f) a-value)))

(defn callable-designator? [f]
  (and (symbol? f)
       (resolve f)
       (fn? (deref (resolve f)))))

(def ^:dynamic *pseudo-type-functions*
  "List of function designators which will be trusted as operand of satisfies
  for the purse of valid-type?"
  ())

(defmethod valid-type? 'satisfies [[_ f]]
  (or (member f *pseudo-type-functions*)
      (callable-designator? f)))

(declare expand-satisfies)

(defmethod -canonicalize-type 'satisfies
  [type-designator]
  (expand-satisfies type-designator))

(defmethod typep '= [a-value [_type value]]
  (= value a-value))

(defmethod valid-type? '= [[_ _]]
  true)

(defmethod typep 'member [a-value [_type & others]]
  (member a-value others))

(defmethod valid-type? 'member [[_ & _]]
  true)

(defn disjoint?-false-warn [t1 t2]
  ;; don't complain about rte nor satisfies
  (letfn [(dont-complain [t]
            (and (sequential? t)
                 (not (empty? t))
                 (member (first t) '(satisfies rte))))]
    (cond (or (dont-complain t1)
              (dont-complain t2))
          false
          :else
          (do
            (cl-format true "disjoint? cannot decide ~A vs ~A -- assuming not disjoint~%" t1 t2)
            false))))

(defn inhabited?-error [type-designator]       
  (throw (ex-info (format "inhabited? cannot decide %s" type-designator)
                  {:error-type :not-yet-implemented
                   :type-designator [type-designator]})))

(def ^:dynamic *inhabited?-default*
  "doc string"
  inhabited?-error
  )

(defn-memoized [sort-method-keys -sort-method-keys]
  "Given a multimethod object, return a list of method keys.
  The :primary method comes first in the return list and the :default
  method has been filtered away."
  [f]
  (cons :primary (remove #{:primary :default} (keys (methods f)))))

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
  If no method returns true or false, then the function
  *disjoint?-default* is called, and its value returned.
  If disjoint? is called with a 3rd argument, then
  *disjoint?-default* is dynamically bound to that value."
  (fn [t1 t2]
    (throw (ex-info "-disjoint? should not be called directly"
                    {:error-type :should-not-be-called-directly
                     :t1 t1
                     :t2 t2}))))

(defn disjoint?
  "Predicate to determine whether the two types overlap."
  [t1 t2 default]
  {:pre [(member default '(true false :dont-know :warning))]
   :post [(fn [v] (member v '(true false :dont-know)))]}
  (letfn [(conclude [t1' t2']
            (if (= default :warning)
              (disjoint?-false-warn t1' t2')
              default))
          (check-disjoint [t1' t2' default]
            (loop [[k & ks] (sort-method-keys -disjoint?)]
              (case ((k (methods -disjoint?)) t1' t2')
                (true) true
                (false) false
                (case ((k (methods -disjoint?)) t2' t1')
                  (true) true
                  (false) false
                  (if ks
                    (recur ks)
                    (conclude t1' t2'))))))]
    (cond
      (not (inhabited? t1 (constantly true))) ;; if t1 is empty, t1 and t2 are disjoint
      true

      (not (inhabited? t2 (constantly true))) ;; if t2 is empty, t1 and t2 are disjoint
      true

      :else
      (let [try1 (check-disjoint t1 t2 (constantly :dont-know))]
        (if (not= :dont-know try1)
          try1
          (let [t1-simple (-canonicalize-type t1)
                t2-simple (-canonicalize-type t2)]
            (if (and (= t1-simple t1)
                     (= t2-simple t2))
              (conclude t1 t2)
              (check-disjoint t1-simple t2-simple default))))))))

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

(defmethod -disjoint? :or [t1 t2]
  (and (or? t1)
       (every? (fn [t1'] (disjoint? t1' t2 false)) (rest t1)))
  true

  :else
  :dont-know)

(defmethod -disjoint? :and [t1 t2]
  (let [inhabited-t1 (delay (inhabited? t1 (constantly false)))
        inhabited-t2 (delay (inhabited? t2 (constantly false)))]
    (cond (not (and? t1))
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
               (not? (first (rest (rest t1))))  ;; t1 of the form (and x (not y))
               (let [[_ A [_ B]] t1
                     C t2]
                 (and (subtype? B A false)
                      (not (subtype? A B true))
                      (subtype? A C false)
                      (not (subtype? C A true)))))
          false

          ;; (gns/disjoint? '(and String (not (member a b c 1 2 3))) 'java.lang.Comparable)
          ;;                       A    (not B)                     C
          ;;  since A and B are disjoint
          ;;  we may ask (disjoint? A C)
          (and (= 3 (count t1)) ;; t1 of the form (and x y)
               (not? (first (rest (rest t1))))  ;; t1 of the form (and x (not y))
               (let [[_ A [_ B]] t1
                     C t2]
                 (disjoint? A B false)))
          (disjoint? (second t1) t2 :dont-know)

          :else
          :dont-know)))

(defmethod -disjoint? := [t1 t2]
  (cond (=? t1)
        (not (typep (second t1) t2))
        
        ;; (= ...) is finite, types are infinite
        ;; (disjoint? '(not (= 1 2 3)) 'Long)
        (and (not? t1)
             (=? (second t1))
             (class-designator? t2))
        false
        
        :else
        :dont-know))

(defmethod -disjoint? :member [t1 t2]
  (cond (member? t1)
        (every? (fn [e1]
                  (not (typep e1 t2))) (rest t1))

        ;; (member ...) is finite, types are infinite
        ;; (disjoint? '(not (member 1 2 3)) 'Long)
        (and (not? t1)
             (member? (second t1))
             (class-designator? t2))
        false
        
        (and (not? t1)
             (not? t2)
             (member? (second t1))
             (member? (second t2)))
        false

        :else
        :dont-know))

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
  examined.  If the default is true, false, or :dont-know,
  then that value is returned.  If the 3rd argument is
  :error, then an exception is thrown."
  (fn [sub super]
    (throw (ex-info "-subtype? should not be called directly"
                    {:error-type :should-not-be-called-directly
                     :sub sub
                     :super super}))))

(defn subtype?
  "Determine whether sub-designator specifies a type which is a subtype
  of super-designator. Sometimes this decision cannot be made/computed, in
  which case the given default value is interpreted as a binary
  function which is called, and its value returned.  The default value
  of default is one of: true, false, :dont-know or :error."
  [sub-designator super-designator default]
  {:pre [(member default '(true false :dont-know :error))]
   :post [(fn [v] (member v '(true false :dont-know)))]}
  (letfn [(conclude [sub super]
            (if (= default :error)
              (throw (ex-info (format "subtype? cannot decide %s vs %s" sub super)
                              {:error-type :not-yet-implemented
                               :type-designators [sub super]}))
              default))]
    (loop [[k & ks] (sort-method-keys -subtype?)]
      (let [s ((k (methods -subtype?)) sub-designator super-designator)]
        (case s
          (true false) s
          (if ks
            (recur ks)
            (conclude sub-designator super-designator)))))))

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
  If no method returns true or false, then the function
  *inhabited?-default* is called, and its value returned.
  If inhabited? is called with a 3rd argument, then
  *inhabited?-default* is dynamically bound to that value."
  (fn [type-designator]
    (throw (ex-info "-inhabited? should not be called directly"
                    {:type-designator type-designator
                     :error-type :should-not-be-called-directly}))))

(defn inhabited?
  "Given a type-designator, perhaps application specific,
  determine whether the type is inhabited, i.e., not the
  empty type."
  ([type-designator]
   (inhabited? type-designator *inhabited?-default*))
  ([type-designator default]
   {:pre [(fn? default)]
    :post [(fn [v] (#{true false :dont-know} v))]}
   (binding [*inhabited?-default* default]
     (letfn [(calc-inhabited [type-designator default]
               (loop [[k & ks] (sort-method-keys -inhabited?)]
                 (case ((k (methods -inhabited?)) type-designator)
                   (true) true
                   (false) false
                   (if ks
                     (recur ks)
                     (default type-designator)))))]
       (let [i (calc-inhabited type-designator (constantly :dont-know))]
         (cond (= :dont-know i)
               (calc-inhabited (-canonicalize-type type-designator) default)

               :else
               i))))))

(defmethod -inhabited? :primary [type-designator]
  (if (class-designator? type-designator)
    true
    :dont-know))

(defn vacuous? 
  "Determine whether the specified type is empty, i.e., not inhabited."
  [type-designator]
  (let [inh (inhabited? type-designator (constantly :dont-know))]
    (if (= :dont-know inh)
      :dont-know
      (not inh))))

(defmethod -subtype? :primary [sub-designator super-designator]
  (cond (and (class-designator? super-designator)
             (= Object (find-class super-designator)))
        true
        
        (and (class-designator? sub-designator)
             (class-designator? super-designator))
        (isa? (find-class sub-designator) (find-class super-designator))
        
        :else
        :dont-know))

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

(defmethod -subtype? := [sub super]
  (cond (=? sub)
        (subtype? (cons 'member (rest sub)) super :dont-know)

        (=? super)
        (subtype? sub (cons 'member (rest super)) :dont-know)

        :else
        :dont-know))

(defmethod -subtype? :not [sub super]
  (cond (and (not? super)  ; (subtype? 'Long '(not Double))
             (disjoint? sub (second super) false))
        true

        ;; (subtype? '(not Double) 'Long)
        (and (not? sub)
             (disjoint? super (second sub) false))
        false

        (and (not? sub)
             (type-equivalent? (second sub) super false))
        false

        (and (not? super)
             (type-equivalent? sub (second super) false))
        false
        
        (not (not? sub))
        :dont-know

        (not (not? super))
        :dont-know

        :else
        (let [x (subtype? (second super) (second sub) :dont-know)]
          (if (= :dont-know x)
            :dont-know
            x))))

(defmethod -subtype? :member [sub super]
  (cond (member? sub)
        (every? (fn [e1]
                  (typep e1 super)) (rest sub))

        ;; (subtype? 'Long '(member 1 2 3))
        (and (member? super)
             (class-designator? sub)) ;; assuming a class is infinite
        false

        ;; (subtype? 'Long '(not (member 1 2 3))) ==> false
        ;; (subtype? 'Long '(not (member 1.1 2 3))) ==> false
        ;; (subtype? 'Long '(not (member 1.1 2.2 3.3))) ==> true
        (and (not? super)
             (class-designator? sub)
             (member? (second super)))
        (every? (fn [e2]
                  (not (typep e2 sub))) (rest (second super)))

        :else
        :dont-know))

(defmethod -subtype? :and [t1 t2]
  (cond
    (not (and? t1))
    :dont-know
    
    (member t2 (rest t1))
    ;; (subtype? (and A B) A)
    true

    
    (exists [t (rest t1)] (subtype? t t2 false))
    ;; (subtype?  '(and String (not (member "a" "b" "c")))  'java.io.Serializable)
    true


    ;; (subtype? '(and A B) '(not A))
    (and (not? t2)
         (exists [t (rest t1)]
                 (= t (second t2)))
         (inhabited? t1 (constantly false)))
    false

    ;; (subtype? (and A B C X Y) (and A B C) )
    (and (and? t2)
         (subset? (set (rest t2)) (set (rest t1))))
    true
    
    :else
    :dont-know))

(defmethod -disjoint? :subtype [sub super]
  (cond (and (subtype? sub super false)
             (inhabited? sub (constantly false)))
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

(defmethod -inhabited? :and [t1]
  (cond
    (not (and? t1))
    :dont-know

    (and (< 2 (count t1))
         (forall-pairs [[a b] (rest t1)]
                       (and (or (class-designator? a)
                                (not? a)
                                (class-designator? (second a)))
                            (or (class-designator? b)
                                (not? b)
                                (class-designator? (second b)))
                            (not (disjoint? a b true)))))
    true

    (exists-pair [[a b] (rest t1)]
                 (and (or (class-designator? a)
                          (not? a)
                          (class-designator? (second a)))
                      (or (class-designator? b)
                          (not? b)
                          (class-designator? (second b)))
                      (disjoint? a b false)))
    false
    
    ;; (and A (not (member ...))) is inhabited if A is inhabited and infinite because (member ...) is finite
    (exists [t (rest t1)]
            (and (not? t)
                 (member? (second t))))
    (inhabited? (-canonicalize-type (cons 'and
                                         (remove (fn [t]
                                                   (and (not? t)
                                                        (member? (second t)))) (rest t1))))
                (constantly :dont-know))

    :else
    :dont-know))

(defmethod -inhabited? :not [t1]
  (cond (not (not? t1))
        :dont-know

        (class-designator? (second t1))
        (not= Object (find-class (second t1))) ;; (not Object) is empty, (not any-other-class) is inhabited

        (member? (second t1))
        true

        :else
        :dont-know))

(defmethod -inhabited? :member [t1]
  (cond (member? t1)
        (boolean (rest t1))

        :else    
        :dont-know))

(defmethod -inhabited? := [t1]
  (if (=? t1)
    true
    :dont-know))

(defmethod -disjoint? :not [t1 t2]
  (cond
    (not (not? t1))
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
    (and (not? t2)
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
    (and (not? t2)
         ;;(class-designator? (second t1))
         ;;(class-designator? (second t2))
         (disjoint? (second t1) (second t2) false))
    false

    ;; (disjoint? (not Long) (not (member 1 2 3 "a" "b" "c")))
    (and (not? t2)
         (not (disjoint? (second t1) (second t2) true)))
    false         

    :else
    :dont-know))


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

(declare type-predicate-to-type-designator)

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
  (cl/cl-cond
   ((not (sequential? type-designator))
    type-designator)

   ((empty? type-designator)
    type-designator)

   ((not= 'satisfies (first type-designator))
    type-designator)

   ((empty? (rest type-designator))
    type-designator)

   ((not (empty? (rest (rest type-designator))))
    type-designator)

   ((type-predicate-to-type-designator (second type-designator)))

   (:else
    type-designator)))

(defmethod -canonicalize-type 'not
  [type-designator]
  (find-simplifier type-designator
                   [(fn [type-designator]
                      ;; (not (not x)) --> x
                      (if (not? (second type-designator))
                        (second (second type-designator))
                        type-designator))
                    (fn [type-designator]
                      (if (= :sigma (second type-designator))
                        :empty-set
                        type-designator))
                    (fn [type-designator]
                      (if (= :empty-set (second type-designator))
                        :sigma
                        type-designator))
                    (fn [type-designator]
                      (list 'not (-canonicalize-type (second type-designator))))]))

(defmethod -canonicalize-type 'and
  [type-designator]
  
  (find-simplifier type-designator
                   [(fn [type-designator]
                      (if (member :empty-set (rest type-designator))
                        :empty-set
                        type-designator))
                    
                    (fn [type-designator]
                      (cond (empty? (rest type-designator))
                            :sigma

                            (empty? (rest (rest type-designator)))
                            (second type-designator)

                            :else
                            type-designator))
                    
                    (fn [type-designator]
                      (if (some =? (rest type-designator))
                        ;; (and Double (= "a")) --> (member)
                        ;; (and String (= "a")) --> (member "a")
                        (let [=-candidates (filter =? (rest type-designator))
                              candidates (rest (first =-candidates))]
                          (cons 'member (filter (fn [x] (typep x type-designator)) candidates)))
                        type-designator))
                    
                    (fn [type-designator]
                      ;; (and Double (member 1.0 2.0 "a" "b")) --> (member 1.0 2.0)
                      (if (some member? (rest type-designator))
                        (let [member-candidates (filter member? (rest type-designator))
                              candidates (rest (first member-candidates))]
                          (cons 'member (filter (fn [x]
                                                  (typep x type-designator)) candidates)))
                        type-designator))
                    
                    (fn [type-designator]
                      (if (member :sigma (rest type-designator))
                        (cons 'and (map -canonicalize-type (remove #{:sigma} (rest type-designator))))
                        type-designator))

                    (fn [type-designator]
                      ;; (and Double (not (member 1.0 2.0 "a" "b"))) --> (and Double (not (member 1.0 2.0)))
                      ;; (and Double (not (= "a"))) --> (and Double  (not (member)))
                      (if (some (fn [t]
                                  (and (not? t)
                                       (or (member? (second t))
                                           (=? (second t)))))
                                (rest type-designator))
                        (cons 'and
                              (map (fn [t]
                                     (if (and (not? t)
                                              (or (member? (second t))
                                                  (=? (second t))))
                                       (let [filtered-td (remove #{t} type-designator)
                                             [_not [_member & candidates]] t
                                             filtered-candidates (filter (fn [t2] (typep t2 filtered-td))
                                                                         candidates)
                                             ]
                                         (cons 'member filtered-candidates))
                                       t
                                       ))
                                   (rest type-designator)))
                        type-designator))

                    (fn [type-designator]
                      (cons 'and (map -canonicalize-type (rest type-designator))))]))

(defmethod -canonicalize-type 'member
  [type-designator]
  (find-simplifier type-designator
                   [(fn [type-designator]
                      (if (empty? (rest type-designator))
                        :empty-set
                        type-designator))
                    (fn [type-designator]
                      (if  (empty? (rest (rest type-designator)))
                        (list '= (second type-designator))
                        type-designator))]))

(defmethod -canonicalize-type 'or
  [type-designator]
  (find-simplifier type-designator
                   [(fn [type-designator]
                      (if (member :empty-set (rest type-designator))
                        (cons 'or (map -canonicalize-type
                                       (remove #{:empty-set} (rest type-designator))))
                        type-designator))

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
                      (if (<= 2 (bounded-count 2 (filter member? (rest type-designator))))
                        (let [[members others] (partition-by-pred member? (rest type-designator))]
                          (cons 'or (cons (cons 'member (distinct (mapcat rest members)))
                                          others)))
                        type-designator))

                    (fn [type-designator]
                      ;; (or Double (member 1.0 2.0 "a" "b")) --> (or Double (member "a" "b"))
                      (if (some member? (rest type-designator))
                        (let [mapped (map (fn [t]
                                            (cond
                                              (member? t)
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
                      (cons 'or (map -canonicalize-type (rest type-designator))))
                    ]))

(defmethod -canonicalize-type :default
  [type-designator]
  (cond   
    (class-designator? type-designator)
    type-designator
    
    (member type-designator '(:sigma :empty-set))
    type-designator

    (not (sequential? type-designator))
    (throw (ex-info (format "-canonicalize-type: warning unknown type %s" type-designator)
                    {:error-type :not-a-sequence
                     :type type-designator }))

    (not (valid-type? type-designator))
    (throw (ex-info (format "-canonicalize-type: warning unknown type %s" type-designator)
                    {:error-type :unknown-type
                     :type type-designator }))

    :else
    type-designator
    ))
