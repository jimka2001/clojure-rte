(ns clojure-rte.util)
(def memoized-multis (atom {}))

(defmacro print-vals ""
  [& args]
  (let [pairs (into [] (map (fn [arg]
                              `[(fn [] ~arg) '~arg]) args))]
    `(print-vals-helper [~@pairs])))

(defmacro defn-memoized
  [[public-name internal-name] docstring & body]
  ;; (println {:macro :defn-memoized})
  ;; (println {           :public-name public-name})
  ;; (println {           :internal-name internal-name})
  ;; (println {           :docstring docstring})
  ;; (println {           :body body})
  ;; (println "-------")
  (assert (string? docstring))
  (let [expansion  `(do
     (declare ~public-name) ;; so that the internal function can call the public function if necessary
     (defn ~internal-name ~@body)
     (def ~(with-meta public-name {:dynamic true}) ~docstring (clojure-rte.util/gc-friendly-memoize ~internal-name))
     )]
    ;; (prn [:expansion expansion])
    expansion
))

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
  ;;(println ["expanding forall" var seq body])
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
