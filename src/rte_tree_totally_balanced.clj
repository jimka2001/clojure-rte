(ns rte-tree-totally-balanced
  (:require [clojure.math :refer [pow round]]
            [statistics-lock :as lock]
            [genus-tester :refer [*test-types*]])
)

(defn insert-012-tree 
  "nondestructively insert a value into a binary tree.  nodes are of the form [value left right],
  leaves are all empty according to the empty? function"
  [probability-binary value tree]
  (letfn [(make-node []
            (if (< (rand 1) probability-binary)
              [value nil nil]
              [value nil]))]
    (cond (empty? tree)
          (make-node)

          ;; unary node
          (= 1 (count tree))
          (let [[parent child] tree]
            [parent (insert-012-tree probability-binary value child)])
          
          ;; binary node
          :else
          (let [[parent left right] tree]
            (cond (< value parent)
                  [parent
                   (insert-012-tree probability-binary value left)
                   right]

                  (> value parent)
                  [parent
                   left
                   (insert-012-tree probability-binary value right)]

                  ;; don't insert duplicate node
                  :else
                  tree)))))

(defn build-012-tree
  "Build a 012 tree by iterating through the
  population, and inserting them one by one in the given order. 
  A 012 tree is a tree where every node either has 0 children (i.e. leaf)
  or internal unary-1 or binary-2 node."
  [probability-binary population]
  (reduce (fn [acc-tree item]
            (insert-012-tree probability-binary item acc-tree))
          nil
          population))

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


(defn rand-tree-012
  "Build a 012 tree which has between 2^n and (2^(n+1) - 1) leaf nodes.
  The tree is built by taking a random permutation of 
  and inserting each of (range 2^n) a tree in random order
  (starting with an empty tree)"

  [probability-binary depth]
  (let [num-leaves (+ (round (pow 2 depth))
                      (rand-int (round (pow 2 depth))))
        population (into () (shuffle (range num-leaves)))
        tree (build-012-tree probability-binary population)
        ]
    ;; (printf "target num-leaves=%d %s\n" num-leaves tree)
    tree
    ))

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
