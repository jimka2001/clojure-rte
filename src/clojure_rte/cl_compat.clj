;; Copyright (c) 2020,21 EPITA Research and Development Laboratory
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

(ns clojure-rte.cl-compat)

(defn prog1 [val & _]
  val)

(defn prog2 [_ val & _]
  val)

(defn progn [& others]
  (last others))

(defmacro cl-cond
  "Like CL:cond.  Each operand of the cl-cond is a list of length at least 1.
   The same semantics as clojure cond, in that the return value is
   determined by the first test which returns non-false.  The
   important semantic difference is that an agument has 1, then the
   specified form is both the test and the return value, and it is
   evaluated at most once.
   Implementation from:
   https://stackoverflow.com/questions/4128993/consolidated-cond-arguments-in-clojure-cl-style"
  ([] nil)
  ([[if1 & then1] & others]
   (when (or if1 then1 others)
     (let [extra-clauses# (when others `(cl-cond ~@others))]
       (if then1
         (case if1
           (:else)
           `(do ~@then1) ;; avoid silly lint error, lein eastwood

           (false nil)
           `(do ~extra-clauses#)
           
           ;; else
           `(if ~if1 (do ~@then1) ~extra-clauses#))
         `(or ~if1 ~extra-clauses#))))))

(defn call-with-escape
  "A functional interface to CL block/return.
  The caller of call-with-escape provides a unary function.
  The body of that function may call the function passed as argument.
  to cause call-with-escape to return. E.g.,
  (call-with-escape
    (fn [ret1]
      ;; now ret1 is a unary function, callint ret1 with argument x
      ;; causes call-with-escape to immediately return x
      (if something
          (ret1 42) ;; return 42 from call-with-escape
         ...)))"  
  [unary]
  (letfn [(ret [v]
            (throw (ex-info "" {:data v
                                :ident ret})))]
    (try (unary ret)
         (catch clojure.lang.ExceptionInfo e
           (if (= ret (:ident (ex-data e)))
             (:data (ex-data e))
             (throw e))))))

(defmacro with-escape [ret & body]
  `(call-with-escape (fn [~ret] ~@body)))

(defn ldiff
  "Returns a copy of the first prefix of l-left whose
   tail is = (but perhaps not pointer-identical) to l-mid."
  [l-left l-mid]
  ;; we can't check pointer identity here because
  ;;  the sequence might be lazy.
  (loop [acc ()
         l-iter l-left]
    (cond (= l-mid l-iter)
          (reverse acc)

          (empty? l-iter)
          l-left

          :else
          (recur (cons (first l-iter) acc)
                 (rest l-iter)))))

(defn funcall [f & args]
  (apply f args))

