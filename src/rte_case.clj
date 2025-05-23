;; Copyright (c) 2020,21,25 EPITA Research and Development Laboratory
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

(ns rte-case
  (:require [xymbolyco :as xym]
            [dot]
            [genus :as gns]
            [util :refer [defn-memoized member non-empty?]]
            [rte-construct :as rte :refer [rte-to-dfa canonicalize-pattern sigma-*
                                               ]]
            [clojure.pprint :refer [cl-format]]
            [backtick :refer [template]]
            )
  )

(def ^:dynamic *warn-on-unreachable-code* true)


(defn-memoized [clauses-to-dfa
                clauses-to-dfa-impl]
  "Returns a complete dfa which is the union of the input clauses.
  E.g.,
  (clauses-to-dfa [[0 rte-0] [1 rte-1] [3 rte-3] [2 rte-2] ...])
  returns a dfa for which (rte/match dfa some-seq) will return 0 if
  rte-0 is matched, 1 if rte-1 is matched etc...
  If none of the rtes match, then false is returned.
  "
  [pairs]
  (reduce xym/synchronized-union
          (map (fn reduce-synchronized-union [[index rte]]
                 (rte-to-dfa rte index))
               pairs)))


(defn ensure-fns-index
  "Internal function used in macro expansion of rte-case, to assure the index is in range
  or print a reasonable error message."
  [index num-fns]
  (cond (not (integer? index))
        (throw (ex-info (cl-format false "rte/match returned non-integer ~A" index)
                        {}))
        (< index 0)
        (throw (ex-info (cl-format false "rte/match returned negative integer ~A" index)
                        {}))

        (<= num-fns index)
        (throw (ex-info (cl-format false "rte/match returned index out of range ~A <= ~A"
                                   num-fns index)
                        {}))
        :else
        index))


(defn print-unreachable-warning [code-exprs]
  (binding [*out* *err*]
    (printf "Unreachable code: ")
    (if (sequential? code-exprs)
      (doseq [word (interpose " " (map str code-exprs))]
        (printf "%s" word))
      (printf "%s" code-exprs))
    (printf "\n")))

(defn rte-case-fn
  "`pairs` is a set of pairs, each of the form [rte 0-ary-function]
  This function is used in the macro expansion of rte-case but
  can also be used on its own.
  `rte-case-fn` returns a unary function which can be falled with
  a sequence.  The sequence is matched against the rtes in order
  and the first one that matches, the corresponding 0-ary-function
  is called and its value is returned.
  The sequence is traversed a maximum of once, but may not traverse
  entirely if it is determined early that no rte matches."
  [pairs code-exprs]
  (let [dfa (reduce xym/synchronized-union 
                    (for [[rte thunk] pairs]
                      (do (assert (rte/valid-rte? rte)
                                  (format "%s is not a valid rte" rte))
                          (assert (fn? thunk))
                          (rte-to-dfa rte thunk))))
        traces (xym/find-spanning-map dfa)
        ]
    (when *warn-on-unreachable-code*
      (doall (map (fn [[rte thunk] code-expr]
                    (when (not (traces thunk))
                      (print-unreachable-warning code-expr)))
                  pairs code-exprs)))
    ;; (dot/dfa-to-dot dfa :title (gensym "rte-case") :view true :draw-sink false)
    (fn f-101 [s]
      (rte/match dfa s))))

(defmacro rte-case
  "Takes an expression, and a set of clauses.
  The expression should evaluate to an object which is `sequential?`.
  Each clause is of the form
     rte consequent.
  The rte is NOT evaluated, and should not be quoted.  It should be
  a syntacticaly correct regular type expression.
  If the first rte matches the sequence, then the consequent is evaluated
  and its value is returned.   If the rte fails to match the rte, then
  the second clause is examined.   The semantics are that the rte's are
  considered in order, top to bottom, and the first one which matches
  causes the corresponding consequent to be evaluated.
  However, the sequence is traverse only once, so the matching process
  is more efficient than a sequence of consecutive calls to
  rte/match."
  [sequence & clauses]

  ;; if odd number of clauses, then don't complain if the final
  ;;   clause is unreachable
  (let [odd-clauses (odd? (count clauses))
        clauses (if odd-clauses
                  `(~@(butlast clauses)
                    ~sigma-* ~(last clauses))
                  clauses)]

    ;; This macro expansion may seem a bit more complicated that it
    ;; needs to be.  I think there's a way to avoid the complication
    ;; but I have thus far not been able to do so.
    ;; We need the Dfa to be constructed only once.  This one-time construction
    ;; is accomplished by the (let ...) in the macro expansion making
    ;; a call to rte-case-fn, which is a memoized function.
    ;; In order for the memoization to work the arguments passed to
    ;; must be all static, i.e., containing no function objects.
    ;; Therefore we cannot put (fn [...] ....) into the call rte-case-fn.
    ;; Instead, we replace the clojures with indexes into an array
    ;; of closures which also sits in the (let [...] ...)
    ;; rte-case-fn returns a function which can be called with a sequence,
    ;; and will return an index into the array.  The index is used to
    ;; grab a 0-ary function from the array and call it.
    (let [parts (partition 2 2 clauses)
          n-parts (count parts)
          n-parts-to-check (if odd-clauses
                             (dec n-parts)
                             n-parts)
          pairs (into [] (for [k (range n-parts)
                               :let [[rte expr] (nth parts k)
                                     name (symbol (format "rte-case-fn-%s" k))]]
                           ;; we have to use ~'rte-fn because we don't
                           ;; want backquote to slap a namespace on
                           ;; the symbol
                           `['~rte (fn ~name [] ~expr)]))
          code-exprs (for [[_ expr] parts]
                       expr)
          ]
      `(let [f# (rte-case-fn ~pairs '~code-exprs)
             seq# ~sequence
             thunk# (f# seq#)
             ]
         (if (fn? thunk#)
           ;; f# has returned false if the sequence didn't match
           ;; any of the rtes.  Otherwise it returned a 0-ary function,
           ;; so we can call this 0-ary function.
           (thunk#)
           (throw (ex-info "No pattern matching given sequence"
                           {:sequence seq#})))))))

(defn remove-extra-syntax 
  "lambda-list is a vector which is almost compatible with the
   lambda-list of fn.   However there might be a :allow-other-keys
   in the map immediately of the &
   E.g.,  [1 2 3 & {:keys [a b c] :allow-other-keys true}]
   If such an element is found, return a copy of the vector
      except with :allow-other-keys removed from the map
    otherwise just return the vector as is."
  [lambda-list]
  (if (not (member '& lambda-list))
    lambda-list
    (let [[before after] (split-with (fn [x] (not= x '&)) lambda-list)]
      (cond
        (<= (count after) 1)
        lambda-list
        (not (map? (nth after 1)))
        lambda-list
        :else
        (into [] (concat before
                         ['&]
                         [(dissoc (nth after 1) :allow-other-keys)]
                         (drop 2 after)))))))

(defn lambda-list-to-rte
  "Returns an rte either of one of the following forms:
    (:cat ... (:* ...)) -- if the given lambda-list contains &
    (:cat ...) -- if the given lambda list only has required parametes.
  The lambda-list itself might contain type hints such as [^Boolean a b ^Long c],
  and the types-map might contain type hints for the same or perhaps different variables
  such as {b String c (satisfies odd?)}
  If there are multiple type hints for the same variable, they both apply, e.g.,
  in this example the type designator for c would be (and Long (satisfies odd?)).
  If a type hint is given for an [& other] variable, then the type hint
  implicitly means sequence of that type.  
  e.g., [& other] {other Boolean}, means that other has type sequence of Boolean.
  This corresponds to the equivalent of an rte such as (:* Boolean)

  The type-map may have keys corresponding to variables in the lambda list,
  or keys of vectors of variables such as {a Boolean [b c] Number}.
  In such a case the type designator corresponds equally to both variables.
  In the case that a variable is mentioned stand-alone and also in a vector,
  both type designators apply (intersection type).
  "
  [lambda-list types-map]
  (assert (map? types-map))
  (letfn [(expand-multi-restrictions [types-map]
            (assert (map? types-map))
            (merge types-map
                   (into {} (for [key (keys types-map)
                                  :when (sequential? key)
                                  :let [type-1 (get types-map key)]
                                  var key
                                  :let [type-2 (get types-map var)]]
                              (if type-2
                                [var (list 'and type-1 type-2)]
                                [var type-1])))))]
    (let [types-map (expand-multi-restrictions types-map)]
      (loop [required lambda-list
             others ()
             prefix-rte []
             suffix-rte []
             parsed []]
        (cond (and (empty? required)
                   (empty? others))
              ;; finished parsing
              (if (empty? suffix-rte)
                `(:cat ~@prefix-rte)
                `(:cat ~@prefix-rte ~@suffix-rte))

              (and (not-empty required)
                   (= '& (first required)))
              ;; found & in the correct place
              (recur nil ; required
                     (rest required) ; rest
                     prefix-rte ; prefix-rte
                     suffix-rte ; suffix-rte
                     (conj parsed '&))

              (and (not-empty others)
                   (map? (first others)))
              (letfn [(make-keyword-matcher-rte [var]
                        (let [k (keyword var)
                              default-given (contains? (get (first others) :or {}) var)
                              term-1 (template (:* (:cat (:not (= ~k)) :sigma)))
                              td (gns/create-and [(get (meta var) :tag :sigma)
                                                  (get types-map var :sigma)])
                              term-2 (template (:cat (:* (:cat :sigma :sigma))
                                                     (:cat (= ~k) ~td)
                                                     (:* (:cat (:not (= ~k)) :sigma))))]
                          (list (if default-given
                                  (template (:or ~term-1
                                                 ~term-2
                                                 ))
                                  term-2))))]
                
                (assert (vector? (get (first others) :keys))
                        (cl-format false "parsing ~A expecting a vector specified for :keys, not ~A"
                                   lambda-list
                                   (get (first others) :keys)))
                
                (let [given-keys (get (first others) :keys)
                      allow-other-keys  (get (first others) :allow-other-keys false)
                      valid-key (if allow-other-keys
                                  (template (satisfies keyword?))
                                  (template (member ~@(map keyword given-keys))))]

                  (recur required
                         (rest others)
                         prefix-rte
                         (conj suffix-rte
                               ;; enforce keyword :sigma pairs
                               ;; and simultaneously a constraint for each key specified
                               (template (:and (:* (:cat ~valid-key :sigma))
                                               ~@(mapcat make-keyword-matcher-rte (:keys (first others))))))
                         (conj parsed (first others)))))
              
              (not-empty required)
              (let [var (first required)]
                (recur (rest required)
                       others
                       (conj prefix-rte
                             (cond (symbol? var)
                                   ;; parsing required section, found a var
                                   (gns/create-and [(get (meta var) :tag :sigma)
                                                    (get types-map var :sigma)])
                                   (vector? var)
                                   ;; parsing required section, found a vector
                                   (list 'rte
                                         (lambda-list-to-rte (first required) types-map))

                                   :else ;; found non-symbol non-vector
                                   (throw (ex-info (cl-format false "invalid lambda-list ~A, at ~A"
                                                              lambda-list required)
                                                   {:error-type "cannot parse prefix"
                                                    :lambda-list lambda-list
                                                    :parsed parsed
                                                    :unparsed required}))))
                       suffix-rte
                       (conj parsed var)))

              (and (not-empty others)
                   (not-empty suffix-rte))
              (throw (ex-info (cl-format false "invalid lambda-list ~A, at ~A"
                                         lambda-list others)
                              {:error-type "cannot parse suffix"
                               :lambda-list lambda-list
                               :parsed parsed
                               :unparsed others}))

              (and (not-empty others)
                   (symbol? (first others)))
              (let [var (first others)]
                (recur required
                       (rest others)
                       prefix-rte
                       (conj suffix-rte
                             (template (:* ~(gns/create-and [(get (meta var) :tag :sigma)
                                                             (get types-map var :sigma)]))))
                       (conj parsed var)))

              (not-empty others)
              (throw (ex-info (cl-format false "invalid lambda-list ~A, at ~A"
                                         lambda-list others)
                              {:error-type "cannot parse suffix"
                               :lambda-list lambda-list
                               :parsed parsed
                               :unparsed others})))))))


(defn conv-1-case-clause [[lambda-list types-map] consequences k]
  (assert (map? types-map)
          (cl-format false "expecting a map, not ~A" types-map))
  (assert (vector? lambda-list)
          (cl-format false "expecting a vector, not ~A" lambda-list))
  (let [name (symbol (format "conv-1-%d" k))]
    [(lambda-list-to-rte lambda-list types-map)
     `(fn ~name ~(remove-extra-syntax lambda-list)
        ~@consequences)]))


(defmacro destructuring-case
  "After evaluating the expression (only once) determine whether its return
  value conforms to any of the given lambda lists and type restrictions.
  If so,  bind the variables as if by let, and evaluate the corresponding
  form.
  E.g.,
  (destructuring-case '(true [\"hello\" 3] true)
     [[_a [_b _c] & _d]  {_a Boolean _b String _d Boolean}]
     1

     [[_a _b]          {_a Boolean _b (or String Boolean)}]
     2)
"
  [expr & operands]
  (cond
    (not= 0 (mod (count operands) 2))
    (throw (ex-info (cl-format false
                               "destructuring-case expects multiple of 2 number of operands after the first: not ~A, ~A"
                               (count operands) (apply list 'destructuring-case expr operands))
                    {:error-type :invalid-destructuring-case-call-site
                     :expr expr
                     :operands operands}))

    :else
    (let [pairs (partition 2 operands)
          ;; e.g. pairs =   (([[_a [_b _c] & _d]  {_a Boolean _b String _d Boolean}]
          ;;                  1)
          ;;                 ([[_a _b]          {_a Boolean _b (or String Boolean)}]
          ;;                  2))
          ]
      `(apply (-destructuring-fn-many nil ~@pairs) ~expr))))

(defmacro dscase
  "Semantically similar to destructuring-case but arguably simpler syntax.
  (dscase evaluatable-value
    lambda-list-1 consequent-1
    lambda-list-2 consequent-2
    lambda-list-3 consequent-3 ...
    )
  Any of the lambda-lists may be preceeded by meta data which maps
  variable names to type designators.

  The first consequent will be evaluated whose corresponding lambda-list
  is applicable to the value specified by evaluatable-value.
  "
  [expr & operands]
  (cond
    (not= 0 (mod (count operands) 2))
    (throw (ex-info (cl-format false
                               "dscase expects multiple of 2 number of operands after the first: not ~A, ~A"
                               (count operands) (apply list 'dscase expr operands))
                    {:error-type :invalid-dscase-call-site
                     :expr expr
                     :operands operands}))

    :else
    (let [pairs (partition 2 operands)]
      (letfn [(conv-1-pair [[lambda-list consequent]]
                (when (not (vector? lambda-list))
                  (throw (ex-info (cl-format false
                                             "dscase expecting vector not ~A" lambda-list)
                                  {:error-type :invalid-dscase-lambda-list
                                   :expr expr
                                   :operands operands})))
                (let [meta-data (meta lambda-list)]
                  (if (nil? meta-data)
                    [[lambda-list {}] consequent]
                    [[lambda-list meta-data]  consequent]))
                )]
        `(destructuring-case ~expr
                             ~@(mapcat conv-1-pair pairs))))))


(defmacro -destructuring-fn-many
  "Internal macro used in the expansion of destructuring-fn
   E.g.
  (-destructuring-fn-many
    nil
    ([[[a b] c d] {}] (list :first a b c d))
    ([[a [b c] d] {}] (list :second a b c d)))
  "
  [& args]
  (cond (empty? args)
        nil

        (and (not (symbol? (first args)))
             (not (= nil (first args))))
        `(-destructuring-fn-many nil ~@args)

        :else
        ;; e.g. given-clauses =     (([[[a b] c d] {}] (list :first a b c d))
        ;;                           ([[a [b c] d] {}] (list :second a b c d)))
        (let [[name & given-clauses] args
              parsed (map conv-1-case-clause
                          (map first given-clauses)
                          (map rest given-clauses)
                          (range (count given-clauses)))
              fns (into [] (map second parsed))
              code-exprs (into [] (map rest given-clauses))
              pairs (map-indexed (fn destr-443 [idx [[lambda-list types-map] _]]
                                   [idx (lambda-list-to-rte lambda-list types-map)])
                                 given-clauses)
              ]
          `(let [dfa# (clauses-to-dfa '~pairs)]
             ;; (dot/dfa-to-dot dfa# :title (gensym "rte") :view true :draw-sink false :dot-file-cb println :png-file-cb println)
             (when *warn-on-unreachable-code*
               (warn-unreachable dfa# '~code-exprs))
             (fn
               ~@(if name (list name) nil) ;; either name or nothing
               [& seq#]
               ;;(println [:seq seq#])
               
               ;; we must declare fns inside the (fn ...) because
               ;;    the user code might make a recrusive call to `name`
               ;;    see test case `t-dsfn-recursive` in rte_case_test.clj
               (let [fns# ~fns
                     ind# (rte/match dfa# seq# :promise-disjoint true)]

                 ;;(println [:index ind#])
                 
                 (if ind#
                   ;; get function out of vector fns# and call it
                   (apply (fns# ind#) seq#)
                   (throw (ex-info "No pattern matching given sequence"
                                   {:sequence seq#}))
                   )))))))



(defn warn-unreachable [dfa code-exprs]
  (let [traces (xym/find-spanning-map dfa)]
    (doseq [ev (range (count code-exprs))
            :when (not (traces ev))]
      (print-unreachable-warning (nth code-exprs ev)))))

(defmacro destructuring-fn
  "params => positional-params* , or positional-params* & next-param
  positional-param => binding-form
  next-param => binding-form
  name => symbol
  constr-map => a map to constrain variables by type.
  
  Defines a function.  When the function is called,  the
  clause whose parameters match the argument in structure and type
  will be evaluated;  otherwise nil is returned."
  {:forms '[(destructuring-fn name? [[params* ] constr-map] exprs*)
            (destructuring-fn name? ([[params*] constr-map ] exprs*)+)]}
  [& args]
  (cond
    (empty? args)
    (throw (IllegalArgumentException. 
            "destructuring-fn, empty argument list not supported")) 

    (not (or (symbol? (first args))
             (= nil (first args))))
    `(destructuring-fn nil ~@args)
    

    (= 1 (count args))
    (throw (IllegalArgumentException. 
            "destructuring-fn, invalid function body or clauses clauses")) 

    (vector? (second args)) ; if lambda-list is a vector
    (let [[name lambda-list & others] args]
      `(-destructuring-fn-many
        ~name
        (~lambda-list
         ~@others)))

    (every? (fn [clause]
              (and (sequential? clause)
                   (not (vector? clause))
                   (not-empty clause)
                   (vector? (first clause))))
            (rest args))
    (let [[name & clauses] args]
      `(-destructuring-fn-many
        ~name
        ~@clauses))
    
    :else
    (throw (IllegalArgumentException. 
            (cl-format false
                       "destructuring-fn, invalid argument list: ~A first non-conforming element ~A"
                       args
                       (map (fn [clause]
                              (type clause)) (rest args))
                       )))))

(defmacro dsfn
  "Syntactically easier wrapper around destructuring-fn.
  The syntax of dsfn is the same as the syntax of fn
  except that meta-data may be placed before a lambda-list which
  will be considered its constr-map.  E.g.,
  (dsfn name? (^{x Boolean} [x] ...)
              (^{x (and Number (not Long))} [x] ...))"
  [& forms]
  (letfn [(process [form]
            (if (and (sequential? form)
                     (non-empty? form))
              (let [meta-data (meta (first form))]
                (cons [(first form) (if (nil? meta-data) {} meta-data) ] (rest form)))
              form))]
    (cond
      (and (or (nil? (first forms))
               (symbol? (first forms)))
           (vector? (second forms)))
      `(dsfn ~(first forms) (~@(rest forms)))

      (vector? (first forms))
      `(dsfn (~@forms))

      :else
      `(destructuring-fn
        ~@(map process forms)))))

(defmacro dsdefn 
  "E.g.,
(dsdefn f 
  ([[a b] c d] 12)
  ([a [b c] d] 13)
  ([a b [^Ratio c d]] 14)
  ([a b [^Integer c d]] 14)
  ([a b [^Double c d]] 16))

  or

(dsdefn f 
  ([[a b] c d] 12)
  ([a [b c] d] 13)
  ([a b [^Ratio c d]] 14)
  (^{c (satisfies integer?)} [a b [c d]] 14)
  ([a b [^Double c d]] 16))
"
  [name & forms]
  `(def ~name (dsfn ~@forms)))


