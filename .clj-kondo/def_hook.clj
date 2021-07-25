(ns def-hook
  (:require [clj-kondo.hooks-api :as api]
))

(defn transform-def [{:keys [:ns :node]}]
  (println ['transform-def :node node :ns ns])
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

(defn transform-print-vals [{:keys [:ns :node]}]
  (println ['transform-print-vals :node node :ns ns])
  {:node (api/macroexpand print-vals node)})


(defmacro defn-memoized
  [[public-name internal-name] docstring & body]
  (assert (string? docstring))
  `(let []
     (declare ~public-name) ;; so that the internal function can call the public function if necessary
     (defn ~internal-name ~@body)
     (def ~(with-meta public-name {:dynamic true}) ~docstring (gc-friendly-memoize ~internal-name))
     ))

(defn transform-defn-memoized [{:keys [:node :ns]}]
  (println ['transform-dfn-memoized :node node :ns ns])
  {:node (api/macroexpand defn-memoized node)})

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

(defn transform-defmulti-memoized [{:keys [:node :ns]}]
  (println ['transform-defmulti-memoized :node node :ns ns])
  {:node (api/macroexpand defmulti-memoized node)})

(defmacro defmethod-memoized
  [public-name dispatch-val & fn-tail]
  "Wrapper around defmethod which defines a method using the internal name of the given
  public name.  The pairing was presumably made in a previous call to defmulti-memoized."
  (assert (find-ns 'clojure-rte.util) "no ns clojure-rte.utilxxxxx")
  (intern (find-ns 'clojure-rte.util) 'memoized-multis)
  `(defmethod ~(get @clojure-rte.util/memoized-multis public-name) ~dispatch-val ~@fn-tail))

(defn transform-defmethod-memoized [{:keys [:node :ns]}]
  (println ['transform-defmethod-memoized :node node :ns ns])
  (intern ns 'memoized-multis)
  {:node (api/macroexpand defmethod-memoized node)})

(defmacro exists
  "Test whether there exists an element of a sequence which matches a condition."
  [[var seq] & body]
  `(some (fn [~var]
           ~@body) ~seq))

(defn transform-exists [{:keys [:ns :node]}]
  (println ['transform-exists :node node :ns ns])
  {:node (api/macroexpand exists node)})

(defmacro setof
  "Return a sequence of lazy elements of a given sequence which match a given condition"
  [[var seq] & body]
  `(filter (fn [~var]
             ~@body) ~seq))

(defn transform-setof [{:keys [:ns :node]}]
  (println ['transform-setof :node node :ns ns])
  {:node (api/macroexpand setof node)})


(defmacro forall
  "Return true if the given body evaluates to logical true for every element of the given sequence"
   [[var seq] & body]
  `(every? (fn [~var]
             ~@body) ~seq))

(defn transform-forall [{:keys [:ns :node]}]
  (println ['transform-forall :node node :ns ns])
  {:node (api/macroexpand forall node)})

(defmacro forall-pairs [[[v1 v2] seq] & body]
  `(every? (fn [[~v1 ~v2]] ~@body) (lazy-pairs ~seq)))

(defn transform-forall-pairs [{:keys [:ns :node]}]
  (println ['transform-forall-pairs :node node :ns ns])
  {:node (api/macroexpand forall-pairs node)})

(defmacro exists-pair  [[[v1 v2] seq] & body]
  `(some (fn [[~v1 ~v2]]
           ~@body) (lazy-pairs ~seq)))

(defn transform-exists-pair [{:keys [:ns :node]}]
  (println ['transform-exists-pair :node node :ns ns])
  {:node (api/macroexpand exists-pair node)})

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

(defn transform-casep [{:keys [:ns :node]}]
  (println ['transform-casep :node node :ns ns])
  {:node (api/macroexpand casep node)})
