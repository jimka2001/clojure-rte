(ns genus-rte-test
  (:require [rte.core]
            [rte.construct :refer [with-compile-env]]
            [genus.genus :as gns]
            [genus.genus-tester :refer [gen-type *test-values* *test-types*]]
            [util.util :refer [member human-readable-current-time]]
            [backtick :refer [template]]
            [clojure.pprint :refer [cl-format pprint]]
            [clojure.test :refer [deftest is]]))

(defn -main []
  (clojure.test/run-tests 'genus-rte-test))

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

(deftest t-inhabited
  (testing "inhabited?"
    (with-compile-env ()
      (is (= true (gns/inhabited? '(rte (:+ Number)) :dont-know)))
      (is (= false (gns/inhabited? '(rte (:and (:+ Number)
                                               (:+ String))) :dont-know))))))


(deftest t-valid-type
  (testing "valid-type?"
    (is (gns/valid-type? '(rte (:cat String :sigma))))
))
