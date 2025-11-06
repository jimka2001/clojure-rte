;; Copyright (c) 2020,21,25 EPITA Research and Development Laboratory
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without restriction,
;; including without limitation the rights to use, copy, modify, merge,
;; publish, distribute, sublicense, and/or sell copies of the Software,
;; and to permit persons to whom the Software is furnished to do so,
;; subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(ns rte-tester
  (:require [tester]
            [xym.xymbolyco :as xym]
            [clojure.pprint :refer [cl-format]]
            [clojure.math :refer [pow round]]
            [genus.genus :as gns]
            [genus.genus-tester :refer [*test-types*]]
            [rte-randomize-syntax :refer [gen-rte]]
            [rte-extract :refer [dfa-to-rte]]
            [rte-construct :refer [with-compile-env rte-to-dfa  nullable?
                                   canonicalize-pattern canonicalize-pattern-once
                                   canonicalize-pattern-once-impl]]
            ))

(defn rte-components [pattern]
  (cond
    (and (seq? pattern)
         (empty? pattern))
    ()

    (seq? pattern)
    (let [[keyword & operands] pattern]
      (case keyword
        (:* :+ :? :not
            :and :or :cat :permute) operands
        ;; case else
        ()))

    :else
    ()))



(defn rte-depth
  "compute the depth of an RTE expression"
  [r]

  (cond (not (sequential? r))
        0

        (keyword? (first r))
        (inc (reduce max 0 (map rte-depth (rest r))))

        :else
        0))

(defn rte-count-leaves 
  "Count the leaf nodes of a given RTE"
  [r]
  (cond (not (sequential? r))
        1

        (keyword? (first r))
        (reduce + 0 (map rte-count-leaves (rest r)))

        :else
        1))




(defn test-rte-to-dfa [num-tries size verbose is-fn]
  (tester/random-test num-tries (fn [rte]
                                  (with-compile-env []
                                    (is-fn (rte-to-dfa rte)
                                           (cl-format false "rte-to-dfa failed on ~A" rte))))
                      (fn [] (gen-rte size *test-types*))
                      rte-components
                      verbose))

(defn test-canonicalize-pattern [num-tries size verbose is-fn]
  (tester/random-test num-tries (fn [rte] (is-fn (canonicalize-pattern rte)
                                                 (cl-format false "canonicalize-pattern failed on ~A" rte)))
                      (fn [] (gen-rte size *test-types*))
                      rte-components verbose))


(defn test-rte-not-nullable
  "Run some tests to assure that if an rte r is nullable if and only
  if (:not r) is not nullable."
  ([num-tries size verbose]
   (test-rte-not-nullable num-tries
                          size
                          verbose
                          (fn [expr msg]
                            (assert expr msg))))
  ([num-tries size verbose is-fn]
   (tester/random-test num-tries
                       (fn [rte]
                         (let [rte-can (canonicalize-pattern rte)]
                           (if (nullable? rte)
                             (do (is-fn (not (nullable? (list :not rte)))
                                         (cl-format false
                                                    "rte ~A is nullable but its complement (:not ...) is not nullable"
                                                    rte))
                                 (is-fn (nullable? rte-can)
                                         (cl-format false
                                                    "rte ~A is nullable but its canonicalization is not: ~A"
                                                    rte rte-can)))
                             (do
                               (is-fn (nullable? (list :not rte))
                                       (cl-format false
                                                  "rte ~A is not nullable but its complement (:not ...) is nullable"
                                                  rte))
                               (is-fn (not (nullable? rte-can))
                                       (cl-format false
                                                  "rte ~A is non-nullable but its canonicalization is nullable: ~A"
                                                  rte rte-can))))))
                       (fn [] (gen-rte size *test-types*))
                       rte-components
                       verbose)))

;; this test is not yet correctly implemented,
;;    need a good way to compare two rtes for equivalence
;; (defn test-rte-not-not-canonicalize
;;   "Run some tests to assure that an rte r is equivalent to
;;   (:not (:not r))"
;;   [num-tries size verbose]
;;   (tester/random-test
;;    num-tries
;;    (fn [rte]
;;      (let [not-1 (canonicalize-pattern `(:not ~rte))
;;            not-2 (canonicalize-pattern `(:not ~not-1))
;;            a-and-not-b (canonicalize-pattern `(:and ~rte (:not ~not-2)))
;;            b-and-not-a (canonicalize-pattern `(:and ~not-2 (:not ~rte)))
;;            ]
;;        (assert (= :empty-set a-and-not-b)
;;                (cl-format false "expecting :empty-set, got a-and-not-b=~A" a-and-not-b))
;;        (assert (= :empty-set b-and-not-a)
;;                (cl-format false "expecting :empty-set, got b-and-not-a=~A" b-and-not-a))))
;;    (fn [] (gen-rte size *test-types*))
;;    rte-components
;;    verbose))


(defn test-rte-canonicalize-nullable-1 [rte is-fn]
  (with-compile-env []
    ;;(cl-format true "canonicalizing:~%")
    ;; TODO doall this lazy seq
    (binding [canonicalize-pattern-once (memoize canonicalize-pattern-once-impl)]
      (let [can (canonicalize-pattern rte)]
        ;;(cl-format true "canonicalized: ~A~%" can)
        (if (nullable? rte)
          (is-fn (nullable? can)
                  (cl-format false
                             "rte ~A is nullable but its canonicalization ~A is not"
                             rte can))
          (is-fn (not (nullable? can))
                  (cl-format false
                             "rte ~A is not nullable but its canonicalization ~A is nullable"
                             rte can)))))))

(defn test-rte-canonicalize-nullable
  "Run some tests to assure that if an rte r is nullable if and only
  if (canonicalize-pattern r) is also nullable."
  ([num-tries size verbose]
   (test-rte-canonicalize-nullable num-tries size verbose (fn [expr msg] (assert expr msg))))
  ([num-tries size verbose is-fn]
   (tester/random-test num-tries
                       (fn [rte] (test-rte-canonicalize-nullable-1 rte is-fn))
                       (fn [] (gen-rte size *test-types*))
                       rte-components
                       verbose
                       )))


(defn test-rte-not-1
  "Assert that the same result occurs from complementing a dfa
  or building a Dfa from a complemented rte."
  ([rte] (test-rte-not-1 rte (fn [expr msg]
                               (assert expr msg))))
  ([rte is-fn]
   (with-compile-env []
     ;; is (not rte) equivalent to (complement dfa) ?
     (let [dfa (rte-to-dfa rte)
           ;;dfa-complete (xym/complete dfa)
           dfa-complement (xym/complement dfa)
           dfa-not-rte (rte-to-dfa (list :not rte))
           ]

       (is-fn (xym/dfa-equivalent? dfa
                                   dfa)
              (cl-format false
                         "187: dfa not equivalent with self rte=~A" rte))

       (is-fn (xym/dfa-equivalent? dfa-not-rte
                                   dfa-not-rte)
              (cl-format false
                         "192: dfa of :not, not equivalent with self rte=~A" (list :not rte)))

       ;;(dot/dfa-to-dot dfa-complement :view true :title "dfa-complement")
       ;; (dot/dfa-to-dot dfa-not-rte :view true :title "dfa-not-rte")
       (is-fn (xym/dfa-equivalent? dfa-complement
                                   dfa-not-rte)
              (cl-format false
                         "202: !dfa != (dfa (not rte)), when rte=~A" rte))

       (let [extracted-rte-map (dfa-to-rte dfa-complement)
             extracted-rte (get extracted-rte-map true :empty-set)
             dfa-not (rte-to-dfa (list :not extracted-rte))
             ]
         ;;(dot/dfa-to-dot dfa-not :view true :title "dfa-not" :state-legend false)
         ;;(print-vals (dfa-to-rte (xym/synchronized-xor dfa dfa-not)))
         (is-fn (xym/dfa-equivalent? dfa dfa-not)
                (cl-format false
                           "211: (rte (dfa (not rte))) != dfa, when rte=~A" rte))
         )))))

(defn test-rte-not
  "Testing several functions, xym/complement, dfa-to-rte, dfa-equivalent?"
  [num-tries size verbose]
  (tester/random-test num-tries
                      test-rte-not-1
                      (fn [] (gen-rte size *test-types*))
                      rte-components
                      verbose))

(defn test-oom []
  (test-rte-canonicalize-nullable 500 ;; 500 ; num-tries
                                   4 ; size
                                   true ;verbose
                                   )
  )
