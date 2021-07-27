(ns backtick
  (:refer-clojure :exclude [resolve]))

(def ^:dynamic *resolve*)

(def ^:dynamic ^:private *gensyms*)

(defn error [msg form]
  (throw (ex-info msg {:form form})))

(defn- resolve [sym]
  (let [ns (namespace sym)
        n (name sym)]
    (if (and (not ns) (= (last n) \#))
      (if-let [gs (@*gensyms* sym)]
        gs
        (let [gs (gensym (str (subs n 0 (dec (count n))) "__auto__"))]
          (swap! *gensyms* assoc sym gs)
          gs))
      (*resolve* sym))))

(defn unquote? [form]
  (and (seq? form) (or (= (first form) 'clojure.core/unquote)
                       (= (first form) 'unquote)
                       )))

(defn unquote-splicing? [form]
  (and (seq? form) (or (= (first form) 'clojure.core/unquote-splicing)
                       (= (first form) 'unquote-splicing))))

(defn inert? [x]
  (or (nil? x)
      (keyword? x)
      (number? x)
      (string? x)
      (boolean? x)
      (= () x)))

(declare quote-fn*)

(defn splice-items [coll]
  (let [xs (if (map? coll) (apply concat coll) coll)
        parts (for [x xs]
                (if (unquote-splicing? x)
                  (second x)
                  [(quote-fn* x)]))
        cat (doall `(concat ~@parts))]
    (cond
      (vector? coll) `(vec ~cat)
      (map? coll) `(apply hash-map ~cat)
      (set? coll) `(set ~cat)
      (seq? coll) `(apply list ~cat)
      :else (error "Unknown collection type" coll))))

(defn quote-items [coll]
  ((cond
     (vector? coll) vec
     (map? coll) #(into {} %)
     (set? coll) set
     (seq? coll) #(list* 'list (doall %))
     :else (error "Unknown collection type" coll))
   (map quote-fn* coll)))

(defn- quote-fn* [form]
  (cond
    (inert? form) form
    (symbol? form) `'~(resolve form)
    (unquote? form) (second form)
    (unquote-splicing? form) (error "splice not in collection" form)
    (record? form) `'~form
    (coll? form) (if (some unquote-splicing? form)
                   (splice-items form)
                   (quote-items form))
    :else `'~form))

(defn quote-fn [resolver form]
  (binding [*resolve* resolver
            *gensyms* (atom {})]
    (quote-fn* form)))

(defmacro defquote [name resolver]
  `(let [resolver# ~resolver]
     (defn ~(symbol (str name "-fn")) [form#]
       (quote-fn resolver# form#))
     (defmacro ~name [form#]
       (quote-fn resolver# form#))))

(defquote template identity)
;; (let [resolver__1274__auto__ identity]
;;   (defn template-fn [form__1275__auto__]
;;     (quote-fn resolver__1274__auto__ form__1275__auto__))
;;   (defmacro template [form__1275__auto__]
;;     (let [expansion (quote-fn resolver__1274__auto__ form__1275__auto__)]
;;       (prn [:in form__1275__auto__
;;             :out expansion])
;;       expansion
;; )))

(defn- class-symbol [ cls]
  (symbol (.getName cls)))

(defn- namespace-name [ ns]
  (name (.getName ns)))

(defn- var-namespace [ v]
  (name (.name (.ns v))))

(defn- var-name [ v]
  (name (.sym v)))

(defn- var-symbol [ v]
  (symbol (var-namespace v) (var-name v)))

(defn- ns-resolve-sym [sym]
  (try
    (let [x (ns-resolve *ns* sym)]
      (cond
        (class? x) (class-symbol x)
        (var? x) (var-symbol x)
        :else nil))
    (catch Exception _ ;; ClassNotFoundException _
      sym)))

(defn resolve-symbol [sym]
  (let [ns (namespace sym)
        nm (name sym)]
    (if (nil? ns)
      (if-let [[_ ctor-name] (re-find #"(.+)\.$" nm)]
        (symbol nil (-> (symbol nil ctor-name)
                      resolve-symbol
                      name
                      (str ".")))
        (if (or (special-symbol? sym)
                (re-find #"^\." nm)) ; method name
          sym
          (or (ns-resolve-sym sym)
              (symbol (namespace-name *ns*) nm))))
      (or (ns-resolve-sym sym) sym))))

(defquote syntax-quote resolve-symbol)
