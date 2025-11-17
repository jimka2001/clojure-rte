(ns demos.conj-2025.demo
  (:require [clojure.pprint :refer [cl-format]]
            [graph.dot :as dot]
            [rte-graphviz :refer [rte-view]]
            [xym.xymbolyco :as xym]
            [rte-case :refer [rte-case destructuring-case]]
            [rte-construct :as rte]
            
))

(defn demo-1 []
  (let [all-int '(:* (satisfies int?))
        starts-0 '(:cat (= 0) (:* :sigma))
        termine-0 '(:cat (:* :sigma) (= 0))
        rte-even-parity '(:* (:cat :sigma :sigma))
        rte-odd-parity (rte/Cat :sigma rte-even-parity)
        r1 (rte/And starts-0
                    rte-odd-parity)
        r2 (rte/And termine-0
                 rte-even-parity)
        rte (rte/And all-int
                     (rte/Or r1
                             r2))
        dfa (rte/rte-to-dfa rte)
        ]
    (rte-view rte :title "rte")
    (dot/dfa-view dfa :title "dfa")))

;; (demo-1)

(defn missile-demo [input-seq]
  (destructuring-case input-seq
    [[a b]          {a Boolean b (or String Boolean)}]
    [:rename-file a b]

    [[a b]          {a Boolean b (or String (satisfies int?))}]
    [:delete-file a b]

    [[a b c & d]  {[a d] Boolean b String c (satisfies int?)}]
    [:copy-file a b c d]

    [[a b]          {a Boolean b (or String Long)}]
    [:launch-the-missiles a b]
    ))

;; (missile-demo '(true true))
;; (missile-demo '(false "hello" 3 true true true))
;; (missile-demo '(true "hello"))
;; (missile-demo '(true 3))
;; (missile-demo '(true "3" 3))
;; (missile-demo '(true "3" 3 true))
