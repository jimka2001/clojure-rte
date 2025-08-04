(ns rte-tree-totally-balanced
  (:require [statistics-lock :as lock]
            [util :refer [rand-tree-012]]
            [genus-tester :refer [*test-types*]])
)



(defn tree-012-to-rte 
  "Take a tree as created by `build-binary-tree`, insert a type at each
  leaf node, and insert a randomly selected operator at each internal node.
  If the operator is unary, then the right child is simply discarded."
  ([tree]
   (tree-012-to-rte tree *test-types*))
  ([tree types]
   (case (count tree)
     (0) (rand-nth types)
     (2) (let [[parent child] tree]
           (list (rand-nth [:? :* :not])
                 (tree-012-to-rte child types)))
     (3) (let [[parent left right] tree]
           (list (rand-nth [:and :or :cat])
                 (tree-012-to-rte left types)
                 (tree-012-to-rte right types))))))

(defn gen-totally-balanced-rte
  "Generate an RTE which corresponds (on average) in shape closely to a balanced
  binary tree.  The goal is to sample languages uniformly rather than sampling
  syntactic structure of the RTE uniformly."
  ([probability-binary depth]
   (gen-totally-balanced-rte probability-binary depth 
                             (cons :epsilon *test-types*)))
  ([probability-binary depth types]
   ;;(printf "generating tree of depth %d:  %s <= count < %s\n" depth (pow 2 depth) (pow 2 (inc depth)))
   ;; a binary tree of depth=n has 2^n <= m < 2^(n+1) leaves
   ;; so generate a random number 2^n <= rand < 2^(n+1)
   ;; i.e 2^n + (rand-int 2^(n+1) - 2^n)
   ;;    2^n + (rand-int 2^n)
   (let [tree (rand-tree-012 probability-binary depth)
         rte (tree-012-to-rte tree types)]

     rte)))
