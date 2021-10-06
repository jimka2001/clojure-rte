;; Copyright (c) 2020,2021 EPITA Research and Development Laboratory
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

(ns clojure-rte.genus-conversion-test
  (:require [clojure-rte.rte-core]
            [clojure-rte.genus :as gns]
            [clojure.test :refer [deftest is testing]]))

(defn -main []
  (clojure.test/run-tests 'clojure-rte.genus-conversion-test))

(deftest t-combo-conversion-C1
  (testing "combo conversion-C1"
    (is (= (gns/conversion-C1 '(or))
           :empty-set))
    (is (= (gns/conversion-C1 '(and))
           :sigma))
    (is (= (gns/conversion-C1 '(and x))
           'x))
    (is (= (gns/conversion-C1 '(or x))
           'x))
    (is (= (gns/conversion-C1 '(and x y))
           '(and x y)))
    (is (= (gns/conversion-C1 '(or x y))
           '(or x y)))))

(deftest t-combo-conversion-C2
  (testing "combo conversion-C2"
    (is (= (gns/conversion-C2 '(or x :sigma y))
           :sigma))
    (is (= (gns/conversion-C2 '(and x :sigma y))
           '(and x :sigma y)))
    (is (= (gns/conversion-C2 '(or x :empty-set y))
           '(or x :empty-set y)))
    (is (= (gns/conversion-C2 '(and x :empty-set y))
           :empty-set))))

(deftest t-combo-conversion-C3
  (testing "combo conversion-C3"
    (is (= (gns/conversion-C3 '(or x (not x)))
           :sigma))
    (is (= (gns/conversion-C3 '(and x (not x)))
           :empty-set))
    (is (= (gns/conversion-C3 '(or x (not y)))
           '(or x (not y))))
    (is (= (gns/conversion-C3 '(and x (not y)))
           '(and x (not y))))))

(deftest t-combo-conversion-C4
  (testing "combo conversion-C4"
    (is (= (gns/conversion-C4 '(or x :empty-set y))
           '(or x y)))
    (is (= (gns/conversion-C4 '(and x :empty-set y))
           '(and x :empty-set y)))
    (is (= (gns/conversion-C4 '(or x :sigma y))
           '(or x :sigma y)))
    (is (= (gns/conversion-C4 '(and x :sigma y))
           '(and x y)))))

(deftest t-combo-conversion-C5
  (testing "combo conversion-C5"
    (is (= (gns/conversion-C5 '(or x y z z y))
           '(or x z y)))
    (is (= (gns/conversion-C5 '(and x y z z y))
           '(and x z y)))))

(deftest t-combo-conversion-C6
  (testing "combo conversion-C6"
    (is (= (gns/conversion-C6 '(or x (or a b c) (or l m n) y))
           '(or x a b c l m n y)))
    (is (= (gns/conversion-C6 '(and x (and a b c) (and l m n) y))
           '(and x a b c l m n y)))
    (is (= (gns/conversion-C6 '(or x (and a b c) (or l m n) y))
           '(or x (and a b c) l m n y)))
    (is (= (gns/conversion-C6 '(and x (or a b c) (and l m n) y))
           '(and x (or a b c) l m n y)))))

(deftest t-combo-conversion-C7
  (testing "combo conversion-C7"
    (is (= (gns/conversion-C7 '(and w x (or a b c) y z) :dnf)
           '(or (and w x a y z)
                (and w x b y z)
                (and w x c y z))))
    (is (= (gns/conversion-C7 '(and w x (or a b c) y z) :cnf)
           '(and w x (or a b c) y z)))

    (is (= (gns/conversion-C7 '(or w x (and a b c) y z) :cnf)
           '(and (or w x a y z)
                 (or w x b y z)
                 (or w x c y z)))
        618)
    (is (= (gns/conversion-C7 '(or w x (and a b c) y z) :cnf)
           '(and (or w x a y z) (or w x b y z) (or w x c y z)))
        620)

    (is (= (gns/conversion-C7 '(or w (not x) (and a (not b))) :dnf)
           '(or w (not x) (and a (not b))))
        622)
    (is (= (gns/conversion-C7 '(or w (not x) (and a (not b))) :cnf)
           '(and (or w (not x) a) (or w (not x) (not b))))
        625)
    ))
 
(deftest t-combo-conversion-C8
  (testing "combo conversion-C8"
    (is (= (gns/conversion-C8 '(and (= 1) (not (member 1 2)) (= 3)))
           :empty-set)
        635)
    (is (= (gns/conversion-C8 '(and (not (= 1)) (member 1 2) (= 3)))
           '(and (not (= 1)) (member 1 2) (= 3)))
        638)
    (is (= (gns/conversion-C8 '(or (= 1) (not (member 1 2)) (= 3)))
           '(or (= 1) (not (member 1 2)) (= 3)))
        641)
    (is (= (gns/conversion-C8 '(or (not (= 1)) (member 1 2) (= 3)))
           :sigma)
        644)))

(deftest t-combo-conversion-C9
  (testing "combo conversion-C9"
    ;; (A + B + C)(A + !B + C)(X) -> (A + B + C)(A + C)(X)
    (is (= (gns/conversion-C9 '(and (or a b c) (or a (not b) c) x))
                             '(and (or a b c) (or a c) x))
        763)
    (is (= (gns/conversion-C9 '(or (and a b c) (and a (not b) c) x))
           '(or (and a b c) (and a c) x))
        765)

    ;; (A + B +!C)(A +!B + C)(A +!B+!C) -> (A + B +!C)(A +!B + C)(A +!C)
    (is (= (gns/conversion-C9 '(and (or a b (not c)) (or a (not b) c) (or a (not b) (not c))))
           '(and (or a b (not c)) (or a (not b) c) (or a (not c))))
        769)
    (is (= (gns/conversion-C9 '(or (and a b (not c)) (and a (not b) c) (and a (not b) (not c))))
           '(or (and a b (not c)) (and a (not b) c) (and a (not c))))
        774)
    
    ;; (A + B +!C)(A +!B + C)(A +!B+!C) -> does not reduce to(A + B +!C)(A +!B+C)(A)
    (is (not= (gns/conversion-C9 '(and (or a b (not c)) (or a (not b) c) (or a (not b) (not c))))
              '(and (or a b (not c)) (or a (not b) c) a))
        779)
    (is (not= (gns/conversion-C9 '(or (and a b (not c)) (and a (not b) c) (and a (not b) (not c))))
              '(or (and a b (not c)) (and a (not b) c) a))
        782)))
    
(deftest t-combo-conversion-C10
  (testing "combo conversion-C10"
    ;; (and A B C) --> (and A C) if A is subtype of B
    ;; (or A B C) -->  (or B C) if A is subtype of B
    (is (= (gns/conversion-C10 '(and (= 1) (member 1 2) (= 3)))
           '(and (= 1) (= 3)))
        790)
    (is (= (gns/conversion-C10 '(or (= 1) (member 1 2) (= 3)))
           '(or (member 1 2) (= 3)))
        793)))

(deftest t-combo-conversion-C11
  (testing "combo conversion-C11"
    ;; A + !A B -> A + B
    (is (= (gns/conversion-C11 '(or a (and (not a) b)))
           '(or a b))
        800)
    (is (= (gns/conversion-C11 '(and a (or (not a) b)))
           '(and a b))
        801)
    
    ;; A + !A BX + Y = (A + BX + Y)
    (is (= (gns/conversion-C11 '(or a (and (not a) b x) y))
           '(or a (and b x) y))
        802)
    (is (= (gns/conversion-C11 '(and a (or (not a) b x) y))
           '(and a (or b x) y))
        803)
    
    ;; A + ABX + Y = (A + Y)
    (is (= (gns/conversion-C11 '(or a (and a b x) y))
           '(or a y))
        804)
    (is (= (gns/conversion-C11 '(and a (or a b x) y))
           '(and a y))
        805)))

(deftest t-combo-conversion-C12
  (testing "combo conversion-C12"
    ;; AXBC + !X = ABC + !X
    (is (= (gns/conversion-C12 '(or (and a x b c) (not x)))
           '(or (and a b c) (not x)))
        826)
    (is (= (gns/conversion-C12 '(and (or a x b c) (not x)))
           '(and (or a b c) (not x)))
        827)))
    
(deftest t-combo-conversion-C13
  (testing "combo conversion-C13"
    ;; multiple !member
    ;; SOr(x,!{-1, 1},!{1, 2, 3, 4})
    ;; --> SOr(x,!{1}) // intersection of non-member
    ;;  SAnd(x,!{-1, 1},!{1, 2, 3, 4})
    ;; --> SOr(x,!{-1, 1, 2, 3, 4}) // union of non-member
    (is (= (gns/conversion-C13 '(or x (not (member -1 1)) (not (member 1 2 3 4))))
           '(or x (not (= 1))))
        841)
    (is (= (gns/conversion-C13 '(and x (not (member -1 1)) (not (member 1 2 3 4))))
           '(and x (not (member -1 1 2 3 4))))
        842)))

(deftest t-combo-conversion-C14
  (testing "combo conversion-C14"
    ;; multiple member
    ;; (or (member 1 2 3) (member 2 3 4 5)) --> (member 1 2 3 4 5)
    (is (= (gns/conversion-C14 '(or (member 1 2 3) (member 2 3 4 5)))
           '(member 1 2 3 4 5))
        851)
    (is (= (gns/conversion-C14 '(or x (member 1 2 3) (member 2 3 4 5)))
           '(or x (member 1 2 3 4 5)))
        852)
    
    ;; (and (member 1 2 3) (member 2 3 4 5)) --> (member 2 3)
    (is (= (gns/conversion-C14 '(and (member 1 2 3) (member 2 3 4 5 )))
           '(member 2 3))
        859)

    (is (= (gns/conversion-C14 '(and x (member 1 2 3) (member 2 3 4 5 )))
           '(and x (member 2 3)))
        860)))
        
(deftest t-combo-conversion-C15

  (testing "combo conversion-C15"
    ;; SAnd(X, member1, not-member) --> SAnd(X,member2)
    (is (= (gns/conversion-C15 '(and x (member 1 2 3 4) (not (member 3 4 5 6))))
           '(and x (member 1 2)))
        871)
    (is (= (gns/conversion-C15 '(and x (member 1 2 3 4)))
           '(and x (member 1 2 3 4)))
        872)
    (is (= (gns/conversion-C15 '(and x (not (member 3 4 5 6))))
           '(and x (not (member 3 4 5 6))))
        873)

    ;; SOr(X, member, not-member1) --> SOr(X,not-member2)
    (is (= (gns/conversion-C15 '(or x (member 1 2 3 4) (not (member 3 4 5 6))))
           '(or x (not (member 5 6))))
        882)
    (is (= (gns/conversion-C15 '(or x (member 1 2 3 4) ))
           '(or x (member 1 2 3 4) ))
        883)
    (is (= (gns/conversion-C15 '(or x (not (member 3 4 5 6))))
           '(or x (not (member 3 4 5 6))))
        882)))
    
(deftest t-combo-conversion-C16
  (testing "combo conversion-C16"
    ;; (and Double (not (member 1.0 2.0 \"a\" \"b\"))) --> (and Double (not (member 1.0 2.0)))
    (is (= (gns/conversion-C16 '(and Double (not (member 1.0 2.0 "a" "b"))))
           '(and Double (not (member 1.0 2.0))))
        895)
    ;; (or Double (member 1.0 2.0 \"a\" \"b\")) --> (and Double (member \"a\" \"b\")
    int Double
    (is (= (gns/conversion-C16 '(or Double (member 1.0 2.0 "a" "b")))
           '(or Double (member "a" "b")))
        896)))
    

(deftest t-combo-conversion-D1
  (testing "combo conversion-D1"
    ;; SOr(SNot(SMember(42, 43, 44, "a","b")), String)
    ;; == > SNot(SMember(42, 43, 44))
    (is (= (gns/conversion-D1 '(or (not (member 42 43 44 "a" "b")) String))
           '(not (member 42 43 44)))
        909)
    ;; SAnd(SMember(42, 43, 44), A, B, C)
    ;; == > SMember(42, 44)
    (is (= (gns/conversion-D1 '(and (member 42 43 44 "a" "b") String))
           '(member "a" "b")))))

(deftest t-combo-conversion-D3
  (testing "combo conversion-D3"
    (is (= (gns/disjoint? 'Integer 'String :dont-know)
           true))
    ;; SOr(SNot(A), SNot(B)) -> STop if A and B are disjoint
    (is (= (gns/conversion-D3 '(or (not String) (not Integer)))
           :sigma)
        920)
    (is (= (gns/conversion-D3 '(or (= 1) (= "") (not String) (not Integer)))
           :sigma)
        921)
    
    ;; SAnd(A, B) --> SEmpty if A and B are disjoint"
    (is (= (gns/conversion-D3 '(and String Integer))
           :empty-set)
        922)
    (is (= (gns/conversion-D3 '(and (or String Integer) String Integer))
           :empty-set)
        923)))
    

(deftest t-combo-conversion-C98
  (testing "combo conversion-C98"
    (is (= (gns/conversion-C98 '(or z x c a b y))
           '(or a b c x y z))
        940)
    (is (= (gns/conversion-C98 '(and (or) (and)))
           '(and (and) (or)))
        941)))
