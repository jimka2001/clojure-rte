(ns clojure-rte.util)
(intern 'clojure-rte.util 'memoized-multis)
(ns clojure-rte.cl-compat)
(intern 'clojure-rte.cl-compat 'call-with-escape)

(ns def-hook
  (:require [clj-kondo.hooks-api :as api]
))

(defn transform-def [{:keys [:ns :node]}]
  (let [[name-node & arg-nodes] (rest (:children node))
        name-sym (api/sexpr name-node)]
    (when-not (simple-symbol? name-sym)
      (let [new-node (with-meta
                       (api/list-node
                        (list*
                         (api/token-node 'def)
                         (api/token-node (symbol (name name-sym)))
                         arg-nodes))
                       (meta node))]
        {:node new-node}))))

(defmacro print-vals ""
  [& args]
  (let [pairs (into [] (map (fn [arg]
                              `[(fn [] ~arg) '~arg]) args))]
    `(print-vals-helper [~@pairs])))

(defmacro defn-memoized
  [[public-name internal-name] docstring & body]
  (assert (string? docstring))
  `(let []
     (declare ~public-name) ;; so that the internal function can call the public function if necessary
     (defn ~internal-name ~@body)
     (def ~(with-meta public-name {:dynamic true}) ~docstring (gc-friendly-memoize ~internal-name))
     ))

(defmacro defmulti-memoized
  "Define a multimethod on an internal name, and a memoized function implemented
   as a dynamic variable.  Methods must be defined using defmethod-memoized, using
   the public-name."
  [[public-name internal-name] docstring dispatch-fn]
  (assert (string? docstring))
  `(let []
     (declare ~public-name) ;; so that the methods can call the public function if necessary
     (defmulti ~internal-name ~dispatch-fn)
     (def ~(with-meta public-name {:dynamic true})
       ~docstring
       (gc-friendly-memoize ~internal-name))
     (swap! clojure-rte.util/memoized-multis assoc '~public-name '~internal-name)))

(defmacro defmethod-memoized
  [public-name dispatch-val & fn-tail]
  "Wrapper around defmethod which defines a method using the internal name of the given
  public name.  The pairing was presumably made in a previous call to defmulti-memoized."
  (assert (find-ns 'clojure-rte.util) "no ns clojure-rte.utilxxxxx")
  (intern (find-ns 'clojure-rte.util) 'memoized-multis)
  `(defmethod ~(get @clojure-rte.util/memoized-multis public-name) ~dispatch-val ~@fn-tail))

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

(defmacro forall-pairs [[[v1 v2] seq] & body]
  `(every? (fn [[~v1 ~v2]] ~@body) (lazy-pairs ~seq)))

(defmacro exists-pair  [[[v1 v2] seq] & body]
  `(some (fn [[~v1 ~v2]]
           ~@body) (lazy-pairs ~seq)))

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

(defmacro cl-cond
  "Like CL:cond.  Each operand of the cl-cond is a list of length at least 1.
   The same semantics as clojure cond, in that the return value is
   determined by the first test which returns non-false.  The
   important semantic difference is that an agument has 1, then the
   specified form is both the test and the return value, and it is
   evaluated at most once.
   Implementation from:
   https://stackoverflow.com/questions/4128993/consolidated-cond-arguments-in-clojure-cl-style"
  ([] nil)
  ([[if1 & then1] & others]
   (when (or if1 then1 others)
     (let [extra-clauses# (when others `(cl-cond ~@others))]
       (if then1
         (case if1
           (:else)
           `(do ~@then1) ;; avoid silly lint error, lein eastwood

           (false nil)
           `(do ~extra-clauses#)
           
           ;; else
           `(if ~if1 (do ~@then1) ~extra-clauses#))
         `(or ~if1 ~extra-clauses#))))))

(defmacro with-escape [ret & body]
  `(clojure-rte.cl-compat/call-with-escape (fn [~ret] ~@body)))
