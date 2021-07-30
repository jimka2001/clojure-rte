(ns clojure-rte.typecase
  (:require [clojure-rte.genus :as gns])
)


(defmacro typecase [value & pairs]
  (if (empty? pairs)
    `(do ~value nil)
    (let [var (gensym "v")
          final-pair (if (odd? (count pairs))
                       [[:sigma (last pairs)]]
                       [])
          leading-pairs (if (odd? (count pairs))
                          (partition 2 (butlast pairs))
                          (partition 2 pairs))
          pairs (concat leading-pairs final-pair)

          treated (mapcat (fn [[td consequent]]
                            [`(gns/typep ~var '~td) consequent]) pairs)
          expansion
          `(let [~var ~value]
             (cond ~@treated))]
      expansion)))


