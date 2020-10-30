;; Copyright (c) 2020 EPITA Research and Development Laboratory
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

(ns clojure-rte.memoize
  "This is an empty namespace to fool lein to load this file as part of
  the clojure-rte.rte-core ns.")

(in-ns 'clojure-rte.rte-core)

(def ^:dynamic rte-compile 
  "Compile an rte pattern into a finite automaton."
  (memoize rte-to-dfa)
  )

(defn call-with-compile-env [thunk]
  (binding [rte-compile (memoize rte-to-dfa)
            canonicalize-pattern-once (memoize -canonicalize-pattern-once)
            ]
    (thunk)))

(defmacro with-compile-env [[] & body]
  `(call-with-compile-env (fn [] ~@body)))

(defn call-with-rte
  "Call the given 0-ary function with 0 or more rte keys bound to rte patterns.
   with-rte is a macro API to this function.
   E.g.,
   (call-with-rte [::a '(:permute Long Long String)
                   ::b '(:permute Double Double String)]
     (fn []
      (rte-match '(:cat ::a ::b) [1 \"hello\" 2
                                  \"world\" 1.0 2.0])))"
  [bindings thunk]
  ;; TODO need to detect if every a local key is defined differently,
  ;; and if so purge the memoize cache of rte-compile.
  (with-compile-env ()
    (binding [*rte-known* (apply assoc *rte-known* bindings)]
      (thunk))))

(defmacro with-rte
  "Evaluate the given body in a dynamic extend where 0 or more keys bound to
   un-quoted rte patterns.
   E.g.,
   (with-rte [::a (:permute Long Long String)
              ::b (:permute Double Double String)]
     (rte-match '(:cat ::a ::b) [1 \"hello\" 2
                                \"world\" 1.0 2.0])))
   Warning, any rte patterns which are compiled during the dynamic extent
   of with-rte, survive the dynamic extend.  I.e., they are not compiled
   twice, rather they are memoized.   
   Any patterns which were compiled before the dynamic extend are ignored.
   Any patterns compiled within the dynamic extend are abandoned when
   the dynamic extend ends."
  [bindings & body]
  `(call-with-rte '~bindings (fn [] ~@body)))

