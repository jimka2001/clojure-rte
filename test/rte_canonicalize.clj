;; Copyright (c) 2021 EPITA Research and Development Laboratory
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

(ns rte-canonicalize
  (:require [rte-core]
            [rte-construct :as rte
             :refer [canonicalize-pattern with-compile-env ]]
            [clojure.pprint :refer [cl-format]]
            [clojure.test :refer [deftest is] :exclude [testing]]
            [util :refer [count-if-not human-readable-current-time]]
            [genus :as gns]))

(def test-verbose false)

(defmacro testing
  [string & body]
  `(with-compile-env []
     (when test-verbose
       (println [:testing 'rte-canonicalize ~string :starting (human-readable-current-time)]))
     (clojure.test/testing ~string ~@body)
     (when test-verbose
       (println [:finished 'rte-canonicalize ~string (human-readable-current-time)]))))

(deftest t-canonicalize-pattern-14
  (when (and (resolve 'java.lang.Comparable)
             (resolve 'clojure.lang.IMeta))

    (testing "canonicalize-pattern 14"
      (is (= '(:or (:and
                    clojure.lang.IMeta
                    clojure.lang.IReduceInit
                    java.io.Serializable)
                   (:and
                    clojure.lang.IMeta
                    clojure.lang.IReduceInit
                    java.lang.Comparable))
             (canonicalize-pattern '(:and (:or java.io.Serializable java.lang.Comparable)
                                          clojure.lang.IMeta clojure.lang.IReduceInit)))
          (cl-format false "~&~
                            and-distribute~@
                            lhs=~W~@
                            rhs=~W~%"
                     '(:or (:and
                            clojure.lang.IMeta
                            clojure.lang.IReduceInit
                            java.io.Serializable)
                           (:and
                            clojure.lang.IMeta
                            clojure.lang.IReduceInit
                            java.lang.Comparable))
                     (canonicalize-pattern '(:and (:or java.io.Serializable java.lang.Comparable)
                                                  clojure.lang.IMeta clojure.lang.IReduceInit)))))))

(deftest t-canonicalize-pattern-14b
  (when (resolve 'java.lang.Comparable)
    (testing "canonicalize-pattern 14b"
      (is (= (canonicalize-pattern '(:permute java.lang.Comparable java.io.Serializable java.lang.Comparable ))
           (canonicalize-pattern '(:or (:cat java.lang.Comparable java.io.Serializable java.lang.Comparable )
                                       (:cat java.lang.Comparable java.lang.Comparable  java.io.Serializable)
                                       (:cat java.io.Serializable java.lang.Comparable java.lang.Comparable )
                                       (:cat java.io.Serializable java.lang.Comparable  java.lang.Comparable)
                                       (:cat java.lang.Comparable  java.io.Serializable java.lang.Comparable)
                                       (:cat java.lang.Comparable java.lang.Comparable java.io.Serializable)))) "permute 3 args"))))

(deftest t-canonicalize-pattern-116
  (testing "previous failure"
    (is (= '(:* :sigma)
           (canonicalize-pattern '(:and (:* :sigma)
                                        (:* :sigma))))
        "test 116")))

(deftest t-canonicalize-pattern
  (testing "canonicalize-pattern"

    ;; syntax errors
    (is (thrown? clojure.lang.ExceptionInfo (canonicalize-pattern '(:*))))
    (is (thrown? clojure.lang.ExceptionInfo (canonicalize-pattern '(:not))))
    (is (thrown? clojure.lang.ExceptionInfo (canonicalize-pattern '(:?))))
    (is (thrown? clojure.lang.ExceptionInfo (canonicalize-pattern '(:+))))))

(def pattern-714 '(:or (:and (:not (= 2)) (:not clojure.lang.ISeq) (:not java.lang.Comparable) :sigma)
                       (:and (:not (= 2)) (:not clojure.lang.ISeq) :sigma java.lang.Comparable)
                       (:and (:not java.lang.Comparable) :sigma clojure.lang.ISeq)
                       (:and clojure.lang.ISeq java.lang.Comparable)
                       (:cat (:and (:not (= 2)) (:not clojure.lang.ISeq) (:not java.lang.Comparable) :sigma)
                             (:and (:not (= 2)) (:not java.lang.Comparable) :sigma) (:* (:and (:not (= 2)) :sigma)))
                       (:cat (:and (:not (= 2)) (:not clojure.lang.ISeq) (:not java.lang.Comparable) :sigma)
                             (:and (:not (= 2)) (:not java.lang.Comparable) :sigma) (:* (:and (:not (= 2)) :sigma))
                             (= 2) (:* (:or (:cat (:and (:not (= 2)) :sigma) (:* (:and (:not (= 2)) :sigma)) (= 2)) (= 2)))
                             (:and (:not (= 2)) :sigma) (:* (:and (:not (= 2)) :sigma)))
                       (:cat (:and (:not (= 2)) (:not clojure.lang.ISeq) (:not java.lang.Comparable) :sigma)
                             (:and (:not (= 2)) :sigma java.lang.Comparable))
                       (:cat (:and (:not (= 2)) (:not clojure.lang.ISeq) (:not java.lang.Comparable) :sigma)
                             (:and (:not (= 2)) :sigma java.lang.Comparable)
                             (:and (:not (= 2)) :sigma) (:* (:and (:not (= 2)) :sigma)))
                       (:cat (:and (:not (= 2)) (:not clojure.lang.ISeq) (:not java.lang.Comparable) :sigma)
                             (:and (:not (= 2)) :sigma java.lang.Comparable)
                             (:and (:not (= 2)) :sigma) (:* (:and (:not (= 2)) :sigma))
                             (= 2) (:* (:or (:cat (:and (:not (= 2)) :sigma) (:* (:and (:not (= 2)) :sigma)) (= 2)) (= 2)))
                             (:and (:not (= 2)) :sigma) (:* (:and (:not (= 2)) :sigma)))
                       (:cat (:and (:not (= 2)) (:not clojure.lang.ISeq) (:not java.lang.Comparable) :sigma)
                             (:and (:not (= 2)) :sigma java.lang.Comparable) (= 2)
                             (:* (:or (:cat (:and (:not (= 2)) :sigma)
                                            (:* (:and (:not (= 2)) :sigma)) (= 2)) (= 2)))
                             (:and (:not (= 2)) :sigma) (:* (:and (:not (= 2)) :sigma)))
                       (:cat (:and (:not (= 2)) (:not clojure.lang.ISeq) (:not java.lang.Comparable) :sigma)
                             (:and (= 2) java.lang.Comparable) (:and (:not (= 2)) :sigma) (:* (:and (:not (= 2)) :sigma)))
                       (:cat (:and (:not (= 2)) (:not clojure.lang.ISeq) (:not java.lang.Comparable) :sigma)
                             (:and (= 2) java.lang.Comparable) (:and (:not (= 2)) :sigma)
                             (:* (:and (:not (= 2)) :sigma)) (= 2)
                             (:* (:or (:cat (:and (:not (= 2)) :sigma)
                                            (:* (:and (:not (= 2)) :sigma)) (= 2)) (= 2))) (:and (:not (= 2)) :sigma)
                             (:* (:and (:not (= 2)) :sigma)))
                       (:cat (:and (:not (= 2)) (:not clojure.lang.ISeq) (:not java.lang.Comparable) :sigma)
                             (:and (= 2) java.lang.Comparable) (= 2)
                             (:* (:or (:cat (:and (:not (= 2)) :sigma) (:* (:and (:not (= 2)) :sigma))
                                            (= 2)) (= 2))) (:and (:not (= 2)) :sigma) (:* (:and (:not (= 2)) :sigma)))
                       (:cat (:and (:not (= 2)) (:not clojure.lang.ISeq) :sigma java.lang.Comparable)
                             (:and (:not (= 2)) (:not java.lang.Comparable) :sigma) (:* (:and (:not (= 2)) :sigma)))
                       (:cat (:and (:not (= 2)) (:not clojure.lang.ISeq) :sigma java.lang.Comparable)
                             (:and (:not (= 2)) (:not java.lang.Comparable) :sigma) (:* (:and (:not (= 2)) :sigma))
                             (= 2) (:* (:or (:cat (:and (:not (= 2)) :sigma) (:* (:and (:not (= 2)) :sigma)) (= 2)) (= 2)))
                             (:and (:not (= 2)) :sigma) (:* (:and (:not (= 2)) :sigma)))
                       (:cat (:and (:not (= 2)) (:not clojure.lang.ISeq) :sigma java.lang.Comparable)
                             (:and (:not (= 2)) :sigma java.lang.Comparable))
                       (:cat (:and (:not (= 2)) (:not clojure.lang.ISeq) :sigma java.lang.Comparable)
                             (:and (:not (= 2)) :sigma java.lang.Comparable)
                             (:and (:not (= 2)) :sigma) (:* (:and (:not (= 2)) :sigma)))
                       (:cat (:and (:not (= 2)) (:not clojure.lang.ISeq) :sigma java.lang.Comparable)
                             (:and (:not (= 2)) :sigma java.lang.Comparable) (:and (:not (= 2)) :sigma)
                             (:* (:and (:not (= 2)) :sigma)) (= 2)
                             (:* (:or (:cat (:and (:not (= 2)) :sigma) (:* (:and (:not (= 2)) :sigma)) (= 2)) (= 2)))
                             (:and (:not (= 2)) :sigma) (:* (:and (:not (= 2)) :sigma)))
                       (:cat (:and (:not (= 2)) (:not clojure.lang.ISeq) :sigma java.lang.Comparable)
                             (:and (:not (= 2)) :sigma java.lang.Comparable) (= 2)
                             (:* (:or (:cat (:and (:not (= 2)) :sigma) (:* (:and (:not (= 2)) :sigma)) (= 2)) (= 2)))
                             (:and (:not (= 2)) :sigma) (:* (:and (:not (= 2)) :sigma)))
                       (:cat (:and (:not (= 2)) (:not clojure.lang.ISeq) :sigma java.lang.Comparable)
                             (:and (= 2) java.lang.Comparable)
                             (:and (:not (= 2)) :sigma) (:* (:and (:not (= 2)) :sigma)))
                       (:cat (:and (:not (= 2)) (:not clojure.lang.ISeq) :sigma java.lang.Comparable)
                             (:and (= 2) java.lang.Comparable)
                             (:and (:not (= 2)) :sigma)
                             (:* (:and (:not (= 2)) :sigma))
                             (= 2)
                             (:* (:or (:cat (:and (:not (= 2)) :sigma) (:* (:and (:not (= 2)) :sigma)) (= 2)) (= 2)))
                             (:and (:not (= 2)) :sigma) (:* (:and (:not (= 2)) :sigma)))
                       (:cat (:and (:not (= 2)) (:not clojure.lang.ISeq) :sigma java.lang.Comparable)
                             (:and (= 2) java.lang.Comparable)
                             (= 2)
                             (:* (:or (:cat (:and (:not (= 2)) :sigma) (:* (:and (:not (= 2)) :sigma)) (= 2)) (= 2)))
                             (:and (:not (= 2)) :sigma) (:* (:and (:not (= 2)) :sigma)))
                       (:cat (:and (:not java.lang.Comparable) :sigma clojure.lang.ISeq)
                             (:and (:not (= 2)) (:not java.lang.Comparable) :sigma)
                             (:* (:and (:not (= 2)) :sigma)))
                       (:cat (:and (:not java.lang.Comparable) :sigma clojure.lang.ISeq)
                             (:and (:not (= 2)) (:not java.lang.Comparable) :sigma)
                             (:* (:and (:not (= 2)) :sigma))
                             (= 2)
                             (:* (:or (:cat (:and (:not (= 2)) :sigma) (:* (:and (:not (= 2)) :sigma)) (= 2)) (= 2)))
                             (:and (:not (= 2)) :sigma) (:* (:and (:not (= 2)) :sigma)))
                       (:cat (:and (:not java.lang.Comparable) :sigma clojure.lang.ISeq)
                             (:and (:not (= 2)) :sigma java.lang.Comparable))
                       (:cat (:and (:not java.lang.Comparable) :sigma clojure.lang.ISeq)
                             (:and (:not (= 2)) :sigma java.lang.Comparable)
                             (:and (:not (= 2)) :sigma) (:* (:and (:not (= 2)) :sigma)))
                       (:cat (:and (:not java.lang.Comparable) :sigma clojure.lang.ISeq)
                             (:and (:not (= 2)) :sigma java.lang.Comparable)
                             (:and (:not (= 2)) :sigma)
                             (:* (:and (:not (= 2)) :sigma))
                             (= 2)
                             (:* (:or (:cat (:and (:not (= 2)) :sigma) (:* (:and (:not (= 2)) :sigma)) (= 2)) (= 2)))
                             (:and (:not (= 2)) :sigma) (:* (:and (:not (= 2)) :sigma)))
                       (:cat (:and (:not java.lang.Comparable) :sigma clojure.lang.ISeq)
                             (:and (:not (= 2)) :sigma java.lang.Comparable)
                             (= 2)
                             (:* (:or (:cat (:and (:not (= 2)) :sigma) (:* (:and (:not (= 2)) :sigma)) (= 2)) (= 2)))
                             (:and (:not (= 2)) :sigma) (:* (:and (:not (= 2)) :sigma)))
                       (:cat (:and (:not java.lang.Comparable) :sigma clojure.lang.ISeq)
                             (:and (= 2) java.lang.Comparable) (:and (:not (= 2)) :sigma) (:* (:and (:not (= 2)) :sigma)))
                       (:cat (:and (:not java.lang.Comparable) :sigma clojure.lang.ISeq)
                             (:and (= 2) java.lang.Comparable)
                             (:and (:not (= 2)) :sigma)
                             (:* (:and (:not (= 2)) :sigma))
                             (= 2)
                             (:* (:or (:cat (:and (:not (= 2)) :sigma) (:* (:and (:not (= 2)) :sigma)) (= 2)) (= 2)))
                             (:and (:not (= 2)) :sigma) (:* (:and (:not (= 2)) :sigma)))
                       (:cat (:and (:not java.lang.Comparable) :sigma clojure.lang.ISeq)
                             (:and (= 2) java.lang.Comparable)
                             (= 2)
                             (:* (:or (:cat (:and (:not (= 2)) :sigma) (:* (:and (:not (= 2)) :sigma)) (= 2)) (= 2)))
                             (:and (:not (= 2)) :sigma) (:* (:and (:not (= 2)) :sigma)))
                       (:cat (:and (= 2) java.lang.Comparable) (:and (:not (= 2)) (:not java.lang.Comparable) :sigma)
                             (:* (:and (:not (= 2)) :sigma)))
                       (:cat (:and (= 2) java.lang.Comparable) (:and (:not (= 2)) (:not java.lang.Comparable) :sigma)
                             (:* (:and (:not (= 2)) :sigma)) (= 2) (:* (:or (:cat (:and (:not (= 2)) :sigma)
                                                                                  (:* (:and (:not (= 2)) :sigma)) (= 2)) (= 2)))
                             (:and (:not (= 2)) :sigma) (:* (:and (:not (= 2)) :sigma)))
                       (:cat (:and (= 2) java.lang.Comparable) (:and (:not (= 2)) :sigma java.lang.Comparable))
                       (:cat (:and (= 2) java.lang.Comparable) (:and (:not (= 2)) :sigma java.lang.Comparable)
                             (:and (:not (= 2)) :sigma) (:* (:and (:not (= 2)) :sigma)))
                       (:cat (:and (= 2) java.lang.Comparable)
                             (:and (:not (= 2)) :sigma java.lang.Comparable) (:and (:not (= 2)) :sigma)
                             (:* (:and (:not (= 2)) :sigma)) (= 2)
                             (:* (:or (:cat (:and (:not (= 2)) :sigma)
                                            (:* (:and (:not (= 2)) :sigma)) (= 2)) (= 2)))
                             (:and (:not (= 2)) :sigma) (:* (:and (:not (= 2)) :sigma)))
                       (:cat (:and (= 2) java.lang.Comparable) (:and (:not (= 2)) :sigma java.lang.Comparable) (= 2)
                             (:* (:or (:cat (:and (:not (= 2)) :sigma) (:* (:and (:not (= 2)) :sigma)) (= 2)) (= 2)))
                             (:and (:not (= 2)) :sigma) (:* (:and (:not (= 2)) :sigma)))
                       (:cat (:and (= 2) java.lang.Comparable) (:and (= 2) java.lang.Comparable)
                             (:and (:not (= 2)) :sigma) (:* (:and (:not (= 2)) :sigma)))
                       (:cat (:and (= 2) java.lang.Comparable) (:and (= 2) java.lang.Comparable)
                             (:and (:not (= 2)) :sigma) (:* (:and (:not (= 2)) :sigma)) (= 2)
                             (:* (:or (:cat (:and (:not (= 2)) :sigma) (:* (:and (:not (= 2)) :sigma)) (= 2)) (= 2)))
                             (:and (:not (= 2)) :sigma) (:* (:and (:not (= 2)) :sigma)))
                       (:cat (:and (= 2) java.lang.Comparable) (:and (= 2) java.lang.Comparable)
                             (= 2) (:* (:or (:cat (:and (:not (= 2)) :sigma) (:* (:and (:not (= 2)) :sigma))
                                                  (= 2)) (= 2))) (:and (:not (= 2)) :sigma)
                             (:* (:and (:not (= 2)) :sigma)))
                       (:cat (:and clojure.lang.ISeq java.lang.Comparable)
                             (:and (:not (= 2))
                                   (:not java.lang.Comparable) :sigma) (:* (:and (:not (= 2)) :sigma)))
                       (:cat (:and clojure.lang.ISeq java.lang.Comparable)
                             (:and (:not (= 2)) (:not java.lang.Comparable) :sigma) (:* (:and (:not (= 2)) :sigma)) (= 2)
                             (:* (:or (:cat (:and (:not (= 2)) :sigma) (:* (:and (:not (= 2)) :sigma)) (= 2)) (= 2)))
                             (:and (:not (= 2)) :sigma) (:* (:and (:not (= 2)) :sigma)))
                       (:cat (:and clojure.lang.ISeq java.lang.Comparable) (:and (:not (= 2)) :sigma java.lang.Comparable))
                       (:cat (:and clojure.lang.ISeq java.lang.Comparable) (:and (:not (= 2)) :sigma java.lang.Comparable)
                             (:and (:not (= 2)) :sigma) (:* (:and (:not (= 2)) :sigma)))
                       (:cat (:and clojure.lang.ISeq java.lang.Comparable) (:and (:not (= 2)) :sigma java.lang.Comparable)
                             (:and (:not (= 2)) :sigma) (:* (:and (:not (= 2)) :sigma)) (= 2)
                             (:* (:or (:cat (:and (:not (= 2)) :sigma)
                                            (:* (:and (:not (= 2)) :sigma)) (= 2)) (= 2)))
                             (:and (:not (= 2)) :sigma) (:* (:and (:not (= 2)) :sigma)))
                       (:cat (:and clojure.lang.ISeq java.lang.Comparable) (:and (:not (= 2)) :sigma java.lang.Comparable)
                             (= 2) (:* (:or (:cat (:and (:not (= 2)) :sigma) (:* (:and (:not (= 2)) :sigma)) (= 2)) (= 2)))
                             (:and (:not (= 2)) :sigma) (:* (:and (:not (= 2)) :sigma)))
                       (:cat (:and clojure.lang.ISeq java.lang.Comparable) (:and (= 2) java.lang.Comparable)
                             (:and (:not (= 2)) :sigma) (:* (:and (:not (= 2)) :sigma)))
                       (:cat (:and clojure.lang.ISeq java.lang.Comparable) (:and (= 2) java.lang.Comparable)
                             (:and (:not (= 2)) :sigma) (:* (:and (:not (= 2)) :sigma)) (= 2)
                             (:* (:or (:cat (:and (:not (= 2)) :sigma) (:* (:and (:not (= 2)) :sigma)) (= 2)) (= 2)))
                             (:and (:not (= 2)) :sigma) (:* (:and (:not (= 2)) :sigma)))
                       (:cat (:and clojure.lang.ISeq java.lang.Comparable) (:and (= 2) java.lang.Comparable)
                             (= 2) (:* (:or (:cat (:and (:not (= 2)) :sigma) (:* (:and (:not (= 2)) :sigma)) (= 2)) (= 2)))
                             (:and (:not (= 2)) :sigma) (:* (:and (:not (= 2)) :sigma)))
                       :epsilon))




(deftest t-canonicalize-pattern-714
  (testing "canonicalize-pattern large special case"
    (canonicalize-pattern
     pattern-714)))

(deftest t-conversion-*-1
  (testing "conversion *1"
    (is (= (rte/conversion-*-1 '(:* :epsilon))
           :epsilon))
    (is (= (rte/conversion-*-1 '(:* :empty-set))
           :epsilon))
    (is (= (rte/conversion-*-1 '(:* (:* x)))
           '(:* x)))))

(deftest t-conversion-*-2
  (testing "conversion *2"
    ;; Star(Cat(x,Star(x))) -> Star(x)
    (is (= (rte/conversion-*-2 '(:* (:cat x (:* x))))
           '(:* x)))
    ;; Star(Cat(Star(x),x)) -> Star(x)
    (is (= (rte/conversion-*-2 '(:* (:cat (:* x) x)))
           '(:* x)))
    ;; Star(Cat(Star(x),x,Star(x))) -> Star(x)
    (is (= (rte/conversion-*-2 '(:* (:cat (:* x) x (:* x))))
           '(:* x)))))

(deftest t-conversion-*-3
  (testing "conversion *3"
    
    ;; Star(Cat(X, Y, Z, Star(Cat(X, Y, Z))))
    ;;    -->    Star(Cat(X, Y, Z))
    (is (= (rte/conversion-*-3 '(:* (:cat x y z (:* (:cat x y z)))))
           '(:* (:cat x y z)))
        860)
    
    ;; Star(Cat(Star(Cat(X, Y, Z)), X, Y, Z))
    ;;   -->    Star(Cat(X, Y, Z))
    (is (= (rte/conversion-*-3 '(:* (:cat (:* (:cat x y z)) x y z)))
           '(:* (:cat x y z)))
        861)
        
    ;; Star(Cat(Star(Cat(X, Y, Z)), X, Y, Z, Star(Cat(X, Y, Z)))
    ;;   -->    Star(Cat(X, Y, Z))
    (is (= (rte/conversion-*-3 '(:* (:cat (:* (:cat x y z))
                                         x y z
                                         (:* (:cat x y z)))))
           '(:* (:cat x y z)))
        862)

    (is (= (rte/conversion-*-3 '(:* (:cat)))
           '(:* (:cat)))
        863)
    (is (= (rte/conversion-*-3 '(:* (:cat x)))
           '(:* (:cat x)))
        864)))

(deftest t-conversion-not-1
  (testing "conversion not 1"
    (is (= (rte/conversion-not-1 '(:not :sigma))
           rte/not-sigma))
    (is (= (rte/conversion-not-1 '(:not (:* :sigma)))
           :empty-set))
    (is (= (rte/conversion-not-1 '(:not :epsilon))
           rte/not-epsilon))
    (is (= (rte/conversion-not-1 '(:not :empty-set))
           rte/sigma-*))))

(deftest t-conversion-not-2
  (testing "conversion not 2"
    (is (= (rte/conversion-not-2 '(:not x))
           '(:not x)))
    (is (= (rte/conversion-not-2 '(:not (:not x)))
           'x))))

(deftest t-conversion-not-3
  (testing "conversion not 3"
    (is (= (rte/conversion-not-3 '(:not (:or x y z)))
           '(:and (:not x) (:not y) (:not z))))
    (is (= (rte/conversion-not-3 '(:not (:or (:not x) y z)))
           '(:and x (:not y) (:not z))))    
    (is (= (rte/conversion-not-3 '(:not (:and x y z)))
           '(:or (:not x) (:not y) (:not z))))
    (is (= (rte/conversion-not-3 '(:not (:and (:not x) y z)))
           '(:or x (:not y) (:not z))))))

(deftest t-conversion-cat-1
  (testing "conversion cat 1"
    (is (= (rte/conversion-cat-1 '(:cat))
           :epsilon))
    (is (= (rte/conversion-cat-1 '(:cat x))
           'x))
    (is (= (rte/conversion-cat-1 '(:cat x y z))
           '(:cat x y z)))))

(deftest t-conversion-cat-3
  (testing "conversion cat 3"
    (is (= (rte/conversion-cat-3 '(:cat x y :empty-set z))
           :empty-set))
    (is (= (rte/conversion-cat-3 '(:cat x y z))
           '(:cat x y z)))))

(deftest t-conversion-cat-4
  (testing "conversion cat 4"
    (is (= (rte/conversion-cat-4 '(:cat (:cat a b) (:cat x y)))
           '(:cat a b x y)))
    (is (= (rte/conversion-cat-4 '(:cat (:cat a b) :epsilon (:cat x y) :epsilon))
           '(:cat a b x y)))))

(deftest t-conversion-cat-5
  (testing "conversion cat 5"
    (is (= (rte/conversion-cat-5 '(:cat a b (:* x) (:* x) y z))
           '(:cat a b (:* x) y z)))
    (is (= (rte/conversion-cat-5 '(:cat a b (:* x) (:* x) (:* x) (:* x) y z))
           '(:cat a b (:* x) y z)))
    (is (= (rte/conversion-cat-5 '(:cat a b (:* x) x (:* x) y z))
           '(:cat a b (:* x) x y z)))
    (is (= (rte/conversion-cat-5 '(:cat a b (:* x) x (:* x) (:* x) y z))
           '(:cat a b (:* x) x y z)))))


(deftest t-conversion-cat-6
  (testing "conversion cat 6"
    (is (= (rte/conversion-cat-6 '(:cat a b (:* x) x c d))
           '(:cat a b x (:* x) c d)))
    (is (= (rte/conversion-cat-6 '(:cat a b (:* x) x x x c d))
           '(:cat a b x x x (:* x) c d)))))
            
(deftest t-conversion-combo-1
  (testing "conversion combo 1"
    (is (= (rte/conversion-combo-1 '(:and))
           '(:* :sigma)))
    (is (= (rte/conversion-combo-1 '(:or))
           :empty-set))
    (is (= (rte/conversion-combo-1 '(:and x))
           'x))
    (is (= (rte/conversion-combo-1 '(:or x))
           'x))
    (is (= (rte/conversion-combo-1 '(:and x y))
           '(:and x y)))
    (is (= (rte/conversion-combo-1 '(:or x y))
           '(:or x y)))))
    
(deftest t-conversion-combo-3
  (testing "conversion combo 3"
    (is (= (rte/conversion-combo-3 '(:or x y (:* :sigma) z))
           '(:* :sigma))
    (is (= (rte/conversion-combo-3 '(:and x y :empty-set z))
           :empty-set)))))
    
(deftest t-conversion-combo-4
  (testing "conversion combo 4"
    (is (= (rte/conversion-combo-4 '(:or x y y x z))
           '(:or y x z)))
    (is (= (rte/conversion-combo-4 '(:and x y y x z))
           '(:and y x z)))))
    
(deftest t-conversion-combo-5
  (testing "conversion combo 5"
    (is (= (rte/conversion-combo-5 '(:or x y y x z))
           '(:or x x y y z)))
    (is (= (rte/conversion-combo-5 '(:and x y y x z))
           '(:and x x y y z)))))
    
(deftest t-conversion-combo-6
  (testing "conversion combo 6"
    (is (= (rte/conversion-combo-6 '(:or x y :empty-set z :empty-set))
           '(:or x y z)))
    (is (= (rte/conversion-combo-6 '(:and x y (:* :sigma) z (:* :sigma)))
           '(:and x y z)))

    (is (= (rte/conversion-combo-6 '(:or x (:or a b) (:or y z)))
           '(:or x a b y z)))
    (is (= (rte/conversion-combo-6 '(:and x (:and a b) (:and y z)))
           '(:and x a b y z)))))
    
(deftest t-conversion-combo-7
  (testing "conversion combo 7"
    ;; (:or A B (:* B) C)
    ;; --> (:or A (:* B) C)
    ;; (:and A B (:* B) C)
    ;; --> (:and A B C)
    (is (= (rte/conversion-combo-7 '(:or a b (:* b) c))
           '(:or a (:* b) c)))
    (is (= (rte/conversion-combo-7 '(:and a b (:* b) c))
           '(:and a b c)))
    (is (not= (rte/conversion-combo-7 '(:and (:* (:contains-any)) (:not :epsilon)))
              '(:not :epsilon)))))

(deftest t-conversion-combo-11
  (testing "conversion combo 11"
    ;; And(...,x,Not(x)...) --> EmptySet
    ;; Or(...x,Not(x)...) --> SigmaStar
    (is (= (rte/conversion-combo-11 '(:or a b x c (:not x)))
           '(:* :sigma)))
    (is (= (rte/conversion-combo-11 '(:and a b x c (:not x)))
           :empty-set))))

(deftest t-conversion-combo-12
  (testing "conversion combo 12"
    ;; Or(   A, B, ... Cat(Sigma,Sigma,Sigma*) ... Not(Singleton(X)) ...)
    ;;   --> Or( A, B, ... Not(Singleton(X))
    (is (= 2 (count-if-not  rte/nullable? '((= 1) (= 1) (:* :sigma))))
        799)
    (is (= (rte/conversion-combo-12 '(:or a b (:cat (= 1) (= 1) (:* :sigma)) (:not (= 2))))
           ;; since (:cat (= 1) (= 1) (:* :sigma)) is already in (:not x))
           '(:or a b  (:not (= 2))))
        800)
    (is (= (rte/conversion-combo-12 '(:or a b (:cat (= 1) (:* :sigma)) (:not (= 2))))
           '(:or a b (:cat (= 1) (:* :sigma)) (:not (= 2))))
        801)
    (is (= (rte/conversion-combo-12 '(:and a b (:cat (= 1) (= 1) (:* :sigma)) (:not (= 2))))
           '(:and a b  (:cat (= 1) (= 1) (:* :sigma))))
        802)
    (is (= (rte/conversion-combo-12 '(:and a b (:cat (= 1) (:* :sigma)) (:not (= 2))))
           '(:and a b (:cat (= 1) (:* :sigma)) (:not (= 2))))
        803)))

(deftest t-conversion-combo-14
  (testing "conversion combo 14"
    ;; Or(A,Not(B),X) -> Sigma* if B is subtype of A
    ;; And(A,Not(B),X) -> EmptySet if A is subtype of B
    (is (= (rte/conversion-combo-14 '(:or (= 1) (:not (member 1 2 3))))
           '(:or (= 1) (:not (member 1 2 3))))
        1027)
    (is (gns/subtype? '(= 1) '(member 1 2 3) false))
    (is (= (rte/conversion-combo-14 '(:and (= 1) (:not (member 1 2 3))))
           :empty-set)
        1028)
    (is (= (rte/conversion-combo-14 '(:or (:not (= 1)) (member 1 2 3)))
           '(:* :sigma))
        1029)
    (is (= (rte/conversion-combo-14 '(:and (:not (= 1)) (member 1 2 3)))
           '(:and (:not (= 1)) (member 1 2 3)))
        1030)))

(deftest t-conversion-combo-15
  (testing "conversion combo 15"
    ;; simplify to maximum of one SMember(...) and maximum of one Not(SMember(...))
    ;; Or(<{1,2,3,4}>,<{4,5,6,7}>,Not(<{10,11,12,13}>,Not(<{12,13,14,15}>)))
    ;;   --> Or(<{1,2,3,4,6,7}>,Not(<{12,13}>))
    ;;
    ;; And(<{1,2,3,4}>,<{3,4,5,6,7}>,Not(<{10,11,12,13}>,Not(<{12,13,14,15}>)))
    ;;   --> And(<{3,4}>,Not(<{10,11,12,13,14,15}>))
    (is (= (rte/conversion-combo-15 '(:or (member 1 2 3 4) (member 3 4 5 6)
                                          (:not (member 10 11 12 13))
                                          (:not (member 12 13 14 15))))
           '(:or (member 1 2 3 4 5 6)
                 (:not (member 12 13))))
        800)
    (is (= (rte/conversion-combo-15 '(:and (member 1 2 3 4) (member 3 4 5 6)
                                          (:not (member 10 11 12 13))
                                          (:not (member 12 13 14 15))))
           '(:and (member 3 4)
                 (:not (member 10 11 12 13 14 15))))
        801)))

(deftest t-conversion-combo-16
  (testing "conversion combo 16"
    (is (= (rte/conversion-combo-16 '(:or (member 1 2 3) (member 3 4 5) (= 3)))
           '(:or (member 1 2 3) (member 3 4 5))))
    (is (= (rte/conversion-combo-16 '(:and (member 1 2 3) (member 3 4 5) (member 1 2 3 4 5 6)))
           '(:and (member 1 2 3) (member 3 4 5))))))
    
(deftest t-conversion-combo-22
  (testing "conversion combo 11"
    ;; (:or A (rte X) B (rte Y) C)
    ;; --> (:or (rte (:or X Y)) A B C)
    ;; (:and A (rte X) B (rte Y) C)
    ;; --> (:and (rte (:and X Y)) A B C)
    (is (= (conversion-combo-22 '(:or A (rte X) B (rte Y) C))
           '(:or (rte (:or X Y)) A B C)))
    (is (= (conversion-combo-22 '(:and A (rte X) B (rte Y) C))
           '(:and (rte (:and X Y)) A B C)))))






(deftest t-conversion-combo-17
  (testing "conversion combo 17"
    ;; And({1,2,3},Singleton(X),Not(Singleton(Y)))
    ;;  {...} selecting elements, x, for which SAnd(X,SNot(Y)).typep(x) is true
    ;; --> And({...},Singleton(X),Not(Singleton(Y)))
    (is (= (rte/conversion-combo-17 '(:and (member -2 -2.5 -1 -1.5 0 1 1.5 2 2.5 3 4 5 6)
                                           (satisfies pos?)
                                           (:not (satisfies int?))))
           '(:and (member 1.5 2.5)
                  (satisfies pos?)
                  (:not (satisfies int?))))
        800)

    ;; Or({1,2,3},Singleton(X),Not(Singleton(Y)))
    ;;  {...} deleting elements, x, for which SOr(X,SNot(Y)).typep(x) is true
    ;; --> Or({...},Singleton(X),Not(Singleton(Y)))

    (is (= (rte/conversion-combo-17 '(:or (member -2 -2.5 -1 -1.5 0 1 1.5 2 2.5 3 4 5 6)
                                           (satisfies pos?)
                                           (:not (satisfies int?))))
           '(:or (member -2  -1  0 )
                  (satisfies pos?)
                  (:not (satisfies int?))))
        801)))
    

(deftest t-conversion-combo-21
  (testing "conversion combo 21"
    (is (= (rte/conversion-combo-21 '(:and (member 1 2 3) (member 3 4 5) (member 5 6 7)))
           :empty-set)
        800)
    (is (= (rte/conversion-combo-21 '(:and (member 0 1 2 3) (member 0 3 4 5) (member 0 5 6 7)))
           '(:and (member 0 1 2 3) (member 0 3 4 5) (member 0 5 6 7)))
        801)
    (is (= (rte/conversion-combo-21 '(:or (:not (member 1 2 3)) (:not (member 3 4 5)) (:not (member 5 6 7))))
           '(:* :sigma))
        802)
    (is (= (rte/conversion-combo-21 '(:or (:not (member 0 1 2 3)) (:not (member 0 3 4 5)) (:not (member 0 5 6 7))))
           '(:or (:not (member 0 1 2 3)) (:not (member 0 3 4 5)) (:not (member 0 5 6 7))))
        803)
    (is (= (rte/conversion-combo-21 '(:or (:not (member 1 2 3)) (member 3 4 5) (:not (member 5 6 7))))
           '(:* :sigma))
        804)))

(deftest t-conversion-and-7
  (testing "conversion and 7"
    (is (= (rte/conversion-and-7 '(:and :sigma :epsilon))
           :empty-set))
    (is (= (rte/conversion-and-7 '(:and :epsilon (member 1 2 3)))
           :empty-set))
    (is (= (rte/conversion-and-7 '(:and :epsilon (:cat x y)))
           '(:and :epsilon (:cat x y))))))

(deftest t-conversion-and-8
  (testing "conversion and 8"
    ;; if operands contains EmptyWord, then the intersection is either EmptyWord or EmptySet
    (is (= (rte/conversion-and-8 '(:and a b c))
           '(:and a b c)))
    (is (= (rte/conversion-and-8 '(:and (:* a) (:* b) :epsilon))
           :epsilon))
    (is (= (rte/conversion-and-8 '(:and (:* a) (:* b) (member 1 2 3) :epsilon))
           :empty-set))))

(deftest t-conversion-and-9
  (testing "conversion and 9"
    ;; if x matches only singleton then And(x,y*) -> And(x,y)
    (is (= (rte/conversion-and-9 '(:and (= 1) (:* x)))
           '(:and (= 1) x)))))

(deftest t-conversion-and-10
  (testing "conversion and 10"
    ;; And(A,B,Or(X,Y,Z),C,D)
    ;; --> Or(And(A,B,   X,   C, D)),
    ;;        And(A,B,   Y,   C, D)),
    ;;        And(A,B,   Z,   C, D)))
    (is (= (rte/conversion-and-10 '(:and a b (:or x y z) c d))
           '(:or (:and a b x c d)
                 (:and a b y c d)
                 (:and a b z c d))))
    (is (= (rte/conversion-and-10 '(:and a b (:and x y z) c d))
           '(:and a b (:and x y z) c d)))))

(deftest t-conversion-and-18
  (testing "conversion and 18"
    ;; if there is a singleton which is not inhabited
    (is (= (rte/conversion-and-18 '(:and (and (member 1 2 3) (member 4 5 6))))
           :empty-set))))

(deftest t-conversion-and-13
  (testing "conversion and 13"
    ;; if there is an explicit :sigma and also a singleton which is inhabited, then
    ;;  we can simply remove the sigma.
    (is (= (rte/conversion-and-13 '(:and (= 1) :sigma (= 2) :sigma))
           '(:and (= 1) (= 2))))
    (is (= (rte/conversion-and-13 '(:and a b c))
           '(:and a b c)))
    (is (not= (rte/conversion-and-13 '(:and :sigma (:not (:or :epsilon :sigma))))
              '(:not (:or :epsilon :sigma))))))

(deftest t-conversion-and-17
  (testing "conversion and 17"
    ;; if And(...) contains a Cat(...) with at least 2 non-nullable components,
    ;;    then this Cat matches only sequences of length 2 or more.
    ;; If And(...) contains a singleton, then it matches only sequences
    ;;    of length 1, perhaps an empty set of such sequences if the singleton type
    ;;    is empty.
    ;; If both are true, then the And() matches EmptySet
    (is (= (rte/conversion-and-17 '(:and (= 1) (:cat (= 1) (:* x) (= 2) (:* x))))
           :empty-set))
    (is (not= (rte/conversion-and-17 '(:and (:* (= 1)) (:cat (= 1) (:* x) (= 2) (:* x))))
              :empty-set))
    (is (not= (rte/conversion-and-17 '(:and (= 1) (:cat (:* x) (= 2) (:* x))))
           :empty-set))))

(deftest t-conversion-and-17a
  (testing "conversion and 17a"
    ;; if And(...) has more than one Cat(...) which has no nullable operand,
    ;;    then the number of non-nullables must be the same, else EmptySet. 

    (is (= (rte/conversion-and-17a '(:and (:cat (= 1) (= 2) (= 3))
                                          (:cat (= 10) (= 20))))
           :empty-set)
        801)
    (is (= (rte/conversion-and-17a '(:and (:cat (= 1) (= 2) (:* (= 3)))
                                          (:cat (= 10) (= 20))))
           '(:and (:cat (= 1) (= 2) (:* (= 3)))
                  (:cat (= 10) (= 20))))
        802)))

(deftest t-conversion-and-17a2
  (testing "conversion and 17a2"
    ;; if And(...) has more than one Cat(...) which has no nullable operand,
    ;;    We also replace the several Cat(...) (having no nullables)
    ;;    with a single Cat(...) with intersections of operands.
    ;;    And(Cat(a,b,c),Cat(x,y,z) ...)
    ;;    --> And(Cat(And(a,x),And(b,y),And(c,z),...)
    (is (= (rte/conversion-and-17a2 '(:and (:cat (= 1) (= 2) (= 3))
                                          (:cat (= 10) (= 20) (= 30))))
           '(:cat (:and (= 1) (= 10))
                  (:and (= 2) (= 20))
                  (:and (= 3) (= 30))))
        800)

    (is (= (rte/conversion-and-17a2 '(:and (:cat (= 1) (= 2) (:* (= 3)))
                                          (:cat (= 10) (= 20))))
           '(:and (:cat (= 1) (= 2) (:* (= 3)))
                  (:cat (= 10) (= 20))))
        802)))

(deftest t-conversion-and-17b
  (testing "conversion and 17b"
    ;; after 17a we know that if there are multiple Cats(...) without a nullable,
    ;;   then all such Cats(...) without a nullable have same number of operands
    ;;   have been merged into one Cat(...)
    ;;   So assure that all other Cats have no more non-nullable operands.
    (is (= (rte/conversion-and-17b '(:and (:cat (= 1) (= 2)) ;; 2 operands
                                          (:cat (= 1) (= 2) (= 3) (:* (= 4))))) ;; 2 operands
           :empty-set)
        800)
    (is (not= (rte/conversion-and-17b '(:and (:cat (= 1) (= 2)) ;; 2 operands
                                             (:cat (= 1) (= 2) (:* (= 4))))) ;; 2 operands
              :empty-set)
        801)))

(deftest t-conversion-and-17c
  (testing "conversion and 17c"
    (is (= (rte/conversion-and-17c '(:and (:cat (= 1) (= 2))
                                          (:cat (:* (= 4)) (= 1) (:* (= 5)) (= 2))
                                          (:cat (:* (= 4)) (= 10) (:* (= 5)) (= 20))))
           '(:and (:cat (= 1) (= 2))
                  (:cat  (= 1)  (= 2))
                  (:cat  (= 10)  (= 20))))
        800)
    (is (= (rte/conversion-and-17c '(:and :sigma
                                          (:cat (:* (= 4)) (:* (= 1)) (:* (= 5)) (= 2))
                                          (:cat (:* (= 4)) (:* (= 10)) (:* (= 5)) (= 20))))
           '(:and :sigma
                  (= 2)
                  (= 20)))
        801)
    (is (= (rte/conversion-and-17c '(:and (= 42)
                                          (:cat (:* (= 4)) (:* (= 1)) (:* (= 5)) (= 2))
                                          (:cat (:* (= 4)) (:* (= 10)) (:* (= 5)) (= 20))))
           '(:and (= 42)
                  (= 2)
                  (= 20)))
        802)))
        
(deftest t-conversion-and-19
  (testing "conversion and 19"
    ;; if there is at least one singleton and zero or more Not(x) where x is a singleton
    ;;   then build a SimpleTypeD and ask whether it is inhabited.
    ;;   if it is not inhabited, then self converts to EmptySet
    (is (= (rte/conversion-and-19 '(:and (:not :sigma) (:not (= 0))))
           ;; does not contain singleton
           '(:and (:not :sigma) (:not (= 0))))
        800)
    (is (= (rte/conversion-and-19 '(:and (= 3) (:not :sigma) (:not (= 0))))
           :empty-set)
        801)
    (is (not= (rte/conversion-and-19 '(:and (= 3) (:not (= 4) (:not (member 10 20 30)))))
              :empty-set)
        802)))

(deftest t-conversion-or-8
  (testing "conversion or 8"
    ;; (:or A :epsilon B (:cat X (:* X)) C)
    ;;   --> (:or A :epsilon B (:* X) C )
    (is (= (rte/conversion-or-8 '(:or a :epsilon b (:cat x (:* x)) c))
           '(:or a :epsilon b (:* x) c))
        800)
    ;; (:or :epsilon (:cat X (:* X)))
    ;;   --> (:or :epsilon (:* X))
    (is (= (rte/conversion-or-8 '(:or :epsilon (:cat x (:* x))))
           '(:or :epsilon (:* x)))
        801)
    ;; (:or (:* Y) (:cat X (:* X)))
    ;;   --> (:or (:* Y) (:* X))
    (is (= (rte/conversion-or-8 '(:or (:* y) (:cat x (:* x))))
           '(:or (:* y) (:* x)))
        802)
    
    (is (= (rte/conversion-or-8 '(:or a :epsilon b (:cat (:* x) x) c))
           '(:or a :epsilon b (:* x) c))
        803)

    (is (= (rte/conversion-or-8 '(:or :epsilon (:cat (:* x) x)))
           '(:or :epsilon (:* x)))
        804)

    (is (= (rte/conversion-or-8 '(:or (:* y) (:cat (:* x) x)))
           '(:or (:* y) (:* x)))
        805)))

(deftest t-conversion-or-9
  (testing "conversion or 9"
    ;; (:or A :epsilon B (:cat X Y Z (:* (:cat X Y Z))) C)
    ;;   --> (:or A :epsilon B (:* (:cat X Y Z)) C )
    (is (= (rte/conversion-or-9 '(:or a :epsilon b (:cat x y z (:* (:cat x y z))) c))
           '(:or a :epsilon b (:* (:cat x y z)) c))
        800)
    ;; (:or :epsilon (:cat X Y Z (:* (:cat X Y Z))))
    ;;   --> (:or :epsilon (:* (:cat X Y Z)))
    (is (= (rte/conversion-or-9 '(:or :epsilon (:cat X Y Z (:* (:cat X Y Z)))))
           '(:or :epsilon (:* (:cat X Y Z))))
        801)

    (is (= (rte/conversion-or-9 '(:or :epsilon (:cat X Y Z (:* (:cat X Y)))))
           '(:or :epsilon (:cat X Y Z (:* (:cat X Y)))))
        802)

    (is (= (rte/conversion-or-9 '(:or :epsilon (:cat X Y (:* (:cat X Y Z)))))
           '(:or :epsilon (:cat X Y (:* (:cat X Y Z)))))
        803)))

(deftest t-conversion-or-10
  (testing "conversion or 10"
    ;; (: or A :epsilon B (: * X) C)
    ;; --> (: or A B (: * X) C)
    (is (= (rte/conversion-or-10 '(:or a :epsilon b (:* x) x))
           '(:or a b (:* x) x)))

    (is (= (rte/conversion-or-10 '(:or A :epsilon B :epsilon C))
           '(:or A :epsilon B :epsilon C)))))

(deftest t-conversion-or-11b
  (testing "conversion or 11b"
    ;; if Sigma is in the operands, then filter out all singletons
    ;; Or(Singleton(A),Sigma,...) -> Or(Sigma,...)
    (is (= (rte/conversion-or-11b '(:or :sigma :sigma))
           '(:or :sigma :sigma))
        800)
    (is (= (rte/conversion-or-11b '(:or :sigma (= 1) (member 1 2 3 4)))
           ':sigma))))

(deftest t-conversion-or-15
  (testing "conversion or 15"
    ;; Or(Not(A),B*,C) = Or(Not(A),C) if A and B  disjoint,
    ;;   i.e. remove all B* where B is disjoint from A
    (is (= (rte/conversion-or-15 '(:or (:* (= 1)) (:not (member 2 3 4))))
           '(:not (member 2 3 4)))
        800)
    (is (= (rte/conversion-or-15 '(:or (:* (= 1)) (:not (member 2 3 4)) (:* (= 0)) (:* (member 3 4 5))))
           '(:or  (:not (member 2 3 4)) (:* (member 3 4 5))))
        801)))

(deftest t-conversion-dual-16b
  (testing "conversion dual 16b"
    ;; And(A, x, Not(y)) --> And(A, x) if x, y disjoint
    (is (= (rte/conversion-dual-16b '(:and (member 1 2 3) (member 10 20) (:not (member 11 21))))
           '(:and (member 1 2 3) (member 10 20)))
        800)
    ;; Or(A, x, Not(y)) --> And(A, Not(x)) if x, y disjoint
    (is (= (rte/conversion-dual-16b '(:or (member 11 2 3) (member 10 20) (:not (member 11 21))))
           '(:or (member 11 2 3) (:not (member 11 21))))
        801)))

(deftest t-discovered-case-1263
  (testing "test 1263"
    (is (= (rte/canonicalize-pattern '(:and (:or :epsilon :sigma) (:not (:or :epsilon :sigma))))
           :empty-set))))

(defn -main []
  ;; To run one test (clojure.test/test-vars [#'rte-canonicalize/the-test])
  (clojure.test/run-tests 'rte-canonicalize))
