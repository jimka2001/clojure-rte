(ns clojure-rte.rte-case
  (:require [clojure-rte.rte-construct :as rte]))


(defmacro rte-case [& body]
  `(case ~@body))

(defmacro destructuring-case [expr & operands]
  (let [var (gensym "v")
        pairs (partition 2 operands)
        body (mapcat (fn [[[lambda-list types-map] consequent]]
                       [`(rte/match (lambda-list-to-rte '~lambda-list '~types-map) ~var)
                        `((fn ~lambda-list ~consequent) ~var)]) pairs)
        expansion `(let [~var  ~expr]
                     (cond ~@body))
        ]
    expansion
    ))

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
  [& args]
  (cond
    (empty? args)
    (throw (Exception
            "destructuring-fn, empty argument list not supported")) 

    (not (or (symbol? (first args))
             (= nil (first args))))
    `(destructuring-fn nil ~@args)
    

    (= 1 (count args))
    (throw (Exception
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
    (throw (Exception
            (format
                       "destructuring-fn, invalid argument list: %s first non-conforming element %s"
                       args
                       (map (fn [clause]
                              (type clause)) (rest args))
                       )))))

(defmacro dsfn
  [& forms]
  (letfn [(process [form]
            (if (and (sequential? form)
                     (not-empty form))
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

(defmacro dscase
  [expr & operands]
  (cond
    (not= 0 (mod (count operands) 2))
    (throw Exception)

    :else
    (let [pairs (partition 2 operands)]
      (letfn [(conv-1-pair [[lambda-list consequent]]
                (when (not (vector? lambda-list))
                  (throw (ex-info (format
                                             "dscase expecting vector not %s" lambda-list)
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


    
