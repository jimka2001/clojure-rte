(ns clojure-rte.cl-compat)
(intern 'clojure-rte.cl-compat 'call-with-escape)


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

(defmacro with-escape [ret & body]
  `(clojure-rte.cl-compat/call-with-escape (fn [~ret] ~@body)))
