(ns def-hook
  (:require [clj-kondo.hooks-api :as api]))
(defn transform-def [{:keys [:node]}]
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
