(ns demos.vmcai-2026.rte-tree-partially-balanced
  (:require [clojure.math :refer [pow round]]
            [lock :as lock]
            [genus-tester :refer [*test-types*]])
)



(defn insert-tree 
  "nondestructively insert a value into a binary tree.  nodes are of the form [value left right],
  leaves are all empty according to the empty? function"
  [value tree]
  (cond (empty? tree)
        [value nil nil]

        :else
        (let [[top left right] tree]
          (cond (< value top)
                [top
                 (insert-tree value left)
                 right]

                (> value top)
                [top
                 left
                 (insert-tree value right)]

                :else
                tree))))


(defn build-binary-tree
  "Build a binary tree by iterating through the
  population, and inserting them one by one in the given order. "
  [population]
  (loop [tree nil
         population population]
    (if (not-empty population)
      (recur (insert-tree (first population) tree)
             (rest population))
      tree)))




(defn tree-to-rte 
  "Take a tree as created by `build-binary-tree`, insert a type designator
  (from the given `types`)  at each leaf node, and insert a randomly selected
  operator at each internal node.
  If the operator is unary, then the right child is simply discarded."
  ([tree]
   (tree-to-rte tree *test-types*))
  ([tree types]
   (cond (not-empty tree)
         (let [key (rand-nth '[:? :* :not
                               :and :or :cat])
               [value left right] tree]
           (case key
             (:? :* :not) (if (= 0 (rand-int 2)) ;; 50% choice
                            (list key (tree-to-rte left types))
                            (list key (tree-to-rte tree types)))
             (:and :or :cat) (list key
                                   (tree-to-rte left types)
                                   (tree-to-rte right types))))

         :else 
         (rand-nth types))))


(defn rand-tree 
  "Build a binary tree which has between 2^n and (2^(n+1) - 1) leaf nodes.
  The tree is built by taking a random permutation of 
  and inserting each of (range 2^n) a tree in random order
  (starting with an empty tree)"

  [depth]
  (let [num-leaves (+ (round (pow 2 depth))
                      (rand-int (round (pow 2 depth))))
        population (into () (shuffle (range num-leaves)))
        tree (build-binary-tree population)
        ]
    ;; (printf "target num-leaves=%d %s\n" num-leaves tree)
    tree
    ))



(defn gen-partially-balanced-rte
  "Generate an RTE which corresponds (on average) in shape closely to a balanced
  binary tree.  The goal is to sample languages uniformly rather than sampling
  syntactic structure of the RTE uniformly."
  ([depth]
   (gen-partially-balanced-rte depth 
                     (cons :epsilon *test-types*)))
  ([depth types]
   ;;(printf "generating tree of depth %d:  %s <= count < %s\n" depth (pow 2 depth) (pow 2 (inc depth)))
   ;; a binary tree of depth=n has 2^n <= m < 2^(n+1) leaves
   ;; so generate a random number 2^n <= rand < 2^(n+1)
   ;; i.e 2^n + (rand-int 2^(n+1) - 2^n)
   ;;    2^n + (rand-int 2^n)
   (let [tree (rand-tree depth)
         rte (tree-to-rte tree types)]

     rte)))

