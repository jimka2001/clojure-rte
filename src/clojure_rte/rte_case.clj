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

(ns clojure-rte.rte-case
  (:require [clojure-rte.xymbolyco :as xym]
            [clojure-rte.genus :as gns]
            [clojure-rte.util :refer [defn-memoized member]]
            [clojure-rte.rte-construct :as rte :refer [rte-to-dfa canonicalize-pattern sigma-*
                                               ]]
            [clojure.pprint :refer [cl-format]]
            [backtick :refer [template]]
            )
  )

(defn-memoized [memoized-rte-case-clauses-to-dfa
                rte-case-clauses-to-dfa]
  "helper function for macro-expanding rte-case.
  returns a complete dfa which is the union of the input clauses."
  [pairs]
  (reduce xym/synchronized-union
          (map (fn [[index rte]]
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
  (letfn [(compile-clauses [clauses]
            (loop [remaining-clauses clauses
                   index 0
                   used-rtes ()
                   acc-int-rte-pairs []
                   acc-fns []]
              (cond
                (empty? remaining-clauses)
                [acc-fns acc-int-rte-pairs]

                :else
                (let [[rte consequent & more] remaining-clauses]
                  (recur more
                         (inc index)
                         (cons rte used-rtes)
                         (conj acc-int-rte-pairs [index (canonicalize-pattern `(:and ~rte (:not (:or ~@used-rtes))))])
                         (conj acc-fns `(fn [] ~consequent)))))))]
    (when (odd? (count clauses))
      (throw (IllegalArgumentException. (str "rte-case, odd number of clauses is not supported. No matching clause: " (last clauses)))))
    
    (let [[fns int-rte-pairs] (compile-clauses clauses)
          num-fns (count fns)]
      `((~fns (ensure-fns-index (rte/match (memoized-rte-case-clauses-to-dfa '~int-rte-pairs) ~sequence
                                           :promise-disjoint true)
                                ~num-fns))))))

(defn- lambda-list-to-rte
  "Helper function for destructuring-case macro.
  Returns an rte either of one of the following forms:
    (:cat ... (:* ...)) -- if the given lambda-list contains &
    (:cat ...) -- if the given lambda list only has required parametes."
  [lambda-list types-map]
  (assert (map? types-map))
  
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
                           :unparsed others})))))

(defmacro destructuring-case
  "After evaluating the expression (only once) determine whether its return value
  conforms to any of the given lambda lists and type restrictions.  If so,
  bind the variables as if by let, and evaluate the corresponding form."
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
    (let [var (gensym "v")]
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
                                    [var type-1])))))
              (remove-extra-syntax [lambda-list]
                ;; lambda-list is a vector which is almost compatible with the
                ;;   lambda-list of fn.   However there might be a :allow-other-keys
                ;;   in the map immediately of the &
                ;;   E.g.,  [1 2 3 & {:keys [a b c] :allow-other-keys true}]
                ;;   If such an element is found, return a copy of the vector
                ;;      except with :allow-other-keys removed from the map
                ;;   otherwise just return the vector as is.
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
              (conv-1-case-clause [[[lambda-list types-map] consequence]]
                (assert (map? types-map)
                        (cl-format false "destructuring-case expecting a map, not ~A" types-map))
                [(lambda-list-to-rte lambda-list (expand-multi-restrictions types-map))
                 `(let [~(remove-extra-syntax lambda-list) ~var]
                    ~consequence)])]
        (let [pairs (partition 2 operands)
              cases (mapcat conv-1-case-clause pairs)]
          `(let [~var ~expr]
             (rte-case ~var ~@cases ~sigma-* nil)))))))

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
                (if (not (vector? lambda-list))
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
  "Internal macro used in the expansion of destructuring-fn"
  [& args]
  (cond (empty? args)
        nil

        (and (not (symbol? (first args)))
             (not (= nil (first args))))
        `(-destructuring-fn-many nil ~@args)

        :else
        (let [var (gensym "fn-var-")
              [name & given-clauses] args
              clauses (mapcat (fn [[structured-lambda-list & exprs]]
                                `(~structured-lambda-list (do ~@exprs))) given-clauses)
              ]
          `(fn
             ~@(if name (list name) nil) ;; either name or nothing
             [& ~var]
             (destructuring-case ~var
                                 ~@clauses)))))

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
                              (and (type clause);;(list? clause)
                                   ;;(not-empty clause)
                                   ;;(vector? (first clause))

                                   )) (rest args))
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
                     (not (empty? form)))
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
