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

(ns genus-subtype-test
  (:require [rte-core]
            [genus.genus :as gns]
            [util :refer [human-readable-current-time]]
            [genus.genus-tester :refer [gen-type]]
            [backtick :refer [template]]
            [clojure.pprint :refer [cl-format]]
            [clojure.test :refer [deftest is]]))

(defn -main []
  (clojure.test/run-tests 'genus-subtype-test))

(def test-verbose false)

(defmacro testing
  [string & body]
  `(gns/call-with-genus-env
    (fn []
      (when test-verbose
        (println [:testing ~string :starting (human-readable-current-time)]))
      (clojure.test/testing ~string ~@body)
      (when test-verbose
        (println [:finished  (human-readable-current-time)])))))

(deftest t-subtype?-and
  (testing "subtype? and"
    (is (gns/subtype? '(and String (not (= "a")))
                      'String
                      false)
        "test 1")
    (is (gns/subtype? '(and String (not (member "a" "b" "c")))
                      'java.io.Serializable
                      false)
        "test 2")
    (is (gns/subtype? '(and Long (not (member 1 2)) (satisfies odd?))
                      '(and Long (satisfies odd?))
                      false)
        "test 3")

    (is (not (gns/subtype? '(and Long (not (member 0 2 4 6)))
                           '(not Long)
                           :dont-know))
        "test 4")
))

(deftest t-subtype?
  (testing "subtype?"
    (is (not (gns/subtype? 'Long '(not Long) :dont-know)))
    (is (gns/subtype? 'Long '(not Double) false))

    (is (not (gns/subtype? '(not Long) '(not Boolean) true)))
    (is (not (gns/subtype? '(not Boolean) '(not Long) true)))
    
    (is (not (gns/subtype? 'Long '(member 1 2 3) true)))
    (is (not (gns/subtype? 'Long '(member 1 1.2 1.3) true)))
    (is (not (gns/subtype? 'Long '(member 1.1 1.2 1.3) true)))

    (is (not (gns/subtype? 'Long '(not (member 1 2 3)) true)))
    (is (not (gns/subtype? 'Long '(not (member 1 1.2 1.3)) true)))
    (is (gns/subtype? 'Long '(not (member 1.1 1.2 1.3)) false))
    
    (is (gns/subtype? '(member 1 2) '(member 0 1 2 3) false))
    (is (not (gns/subtype? '(member 0 1 2 3) '(member 1 2) true)))
    (is (gns/subtype? '(member 1 2) 'Long false) "line 154")
    (is (not (gns/subtype? '(member 1 2) 'String true)) "line 155")
    (is (not (gns/subtype? '(member 1 2) '(not Long) true)) "line 156")
    (is (gns/subtype? '(member 1 2) '(not String) false) "line 157")))

(deftest t-random-subtype
  (testing "randomized testing of subtype?"
    (letfn [(check-subtype [td-1 td-2 comment]
              (is (not= false (gns/subtype? td-1 td-2 :dont-know))
                  (cl-format nil "~a td-1=~a td-2=~a" comment td-1 td-2))
              (is (not= false (gns/subtype? td-2 td-1 :dont-know))
                  (cl-format nil "~a td-2=~a td-1=~a" comment td-2 td-1)))]
      (doseq [_ (range 200)
            n (range 5)
            :let [rt (gen-type n)
                  rt-can (gns/canonicalize-type rt)]]
        (do
          (check-subtype rt rt-can "rt < rt-can ?")
          (check-subtype rt-can rt "rt-can < rt ?"))))))


(deftest t-random-subtype-2
  (testing "randomized testing of subtype? with de morgan"
    (letfn [(check-subtype [td-1 td-2 comment]
              (is (not= false (gns/subtype? td-1 td-2 :dont-know))
                  (cl-format nil "~a td-1=~a td-2=~a" comment td-1 td-2))
              (is (not= false (gns/subtype? td-2 td-1 :dont-know))
                  (cl-format nil "~a td-2=~a td-1=~a" comment td-2 td-1)))]
    
      (doseq [_ (range 200)
            n (range 5)
            :let [rt-1 (gen-type n)
                  rt-2 (gen-type n)
                  rt-and-not (template (and (not ~rt-1) (not ~rt-2)))
                  rt-not-or (template (not (or ~rt-1 ~rt-2)))
                  ]]
        (do 
          (check-subtype rt-and-not 
                         rt-not-or "rt-and-not < rt-not-or")
          (check-subtype rt-not-or 
                         rt-and-not "rt-not-or < rt-and-not"))))))
    
(deftest t-intersection-union-subtype
  (testing "intersection-union-subtype"
    (letfn [(check-subtype [rt-1 rt-2 comment]
              (is (not= false (gns/subtype? rt-1 rt-2 :dont-know))
                  (cl-format false "~A: rt-1=~A rt-2=~A" comment rt-1 rt-2)))]
      (doseq [_ (range 20 ;; 200
                       )
              n (range 5)
              :let [rt-1 (gen-type n)
                    rt-2 (gen-type n)
                    union (template (or ~rt-1 ~rt-2))
                    intersect (template (and ~rt-1 ~rt-2))]]
        (check-subtype rt-1 union "x <: x || y")
        (check-subtype rt-2 union "y <: x || y")
        (check-subtype intersect rt-1 "x&y <: x")
        (check-subtype intersect rt-2 "x&y <: y")))))


(deftest t-normalized-subtype-test
  (testing "randomized testing of subtypep with normalization"
    (letfn [(check-subtype [rt-1 rt-2 comment]
              (is (not= false (gns/subtype? rt-1 rt-2 :dont-know))
                  (cl-format false "~A: rt-1=~A rt-2=~A" comment rt-1 rt-2))
              (is (not= false (gns/subtype? rt-2 rt-1 :dont-know))
                  (cl-format false "~A: rt-2=~A rt-1=~A" comment rt-2 rt-1)))]
      (doseq [_ (range 20 ;; 200
                       )
              n (range 5)
              :let [rt (gen-type n)
                    dnf (gns/canonicalize-type rt :dnf)
                    cnf (gns/canonicalize-type rt :cnf)
                    dnf-cnf (gns/canonicalize-type dnf :cnf)
                    cnf-dnf (gns/canonicalize-type cnf :dnf)]]
        (check-subtype rt (gns/canonicalize-type rt :none) "canonicalize")
        (check-subtype rt dnf "dnf")
        (check-subtype rt cnf "cnf")
        (check-subtype rt dnf-cnf "(cnf (dnf ...))")
        (check-subtype rt cnf-dnf "(dnf (cnf ...))")))))

(deftest t-discovered-cases
  (testing "discovered cases"
    (is (= true (gns/subtype? 'Long '(not Double) :dont-know)) "Long <: not(Double)")
    (is (= false (gns/subtype? '(not Double) 'Long :dont-know)) "not(Double) !<: Long")
    (is (not= false (gns/subtype? '(not (member 1 2)) '(or (= 3) (not (member 1 2))) :dont-know))
        "simplified found in random test")
    (is (not= false (gns/subtype? '(not (member a b))
                                  '(or (= []) (not (member a b)))
                                  :dont-know)) "found in random test")))

(deftest t-double-ratio
  (testing "doubles and ratio"
    (is (= true (gns/subtype? 'Double 'Number :dont-know)))
    (is (= true (gns/subtype? 'Double '(not Ratio) :dont-know)))
    (is (= true (gns/subtype? 'Double '(and (not Ratio) Number) :dont-know)))

    (is (= true (gns/subtype? 'Double '(or (not Ratio) Number) :dont-know)))
    (is (= true (gns/subtype? 'Double '(or (not Ratio) String) :dont-know)))
    ))
