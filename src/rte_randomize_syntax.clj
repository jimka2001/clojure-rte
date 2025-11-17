(ns rte-randomize-syntax
  (:require [genus.genus-tester :refer [*test-types*]])
)


(def ^:dynamic *rte-keywords*
  [:type
   :? :+ :* :not
   :and :or 
   :cat ;; :permute
   :contains-any :contains-every :contains-none
   :sigma :empty-set :epsilon])

(defn gen-rte
  ([depth]
   (gen-rte depth *test-types*))
  ([depth types]
   (let [key (rand-nth *rte-keywords*)] 
     (gen-rte key depth types)))
  ([key depth types]
   (case key
     (:type) (rand-nth types)
     (:sigma :empty-set :epsilon) key
     ;;(:permute) (gen-rte :cat depth types)
     (:and :or :cat :contains-any
           :contains-every :contains-none) (cons key (map (fn [_k] (gen-rte (dec depth) types))
                                                          (range depth)))
     (:? :+ :* :not) (list key (gen-rte (dec depth) types)))))
