(ns gnuplot-test
  (:require [graph.gnuplot :as sut]
            [clojure.test :refer [deftest is testing]]))

(defn -main []
  (clojure.test/run-tests 'gnuplot-test))

(deftest t-gnuplot-1
  (testing "gnuplot-1"
    (is
     (sut/gnuplot [["plot-1" [ 1.1 2.3 6.5] [10.2 12.3 6.1]]
                   ["plot-2" [ 2.1 3.3 4.5] [10.2 2.3 5.1]]
                   ["plot-3" [ 1.1 4.3 9.5 10.2] [6.2 2.3 7.1 7.5]]]
                  :view true
                  :title "sample plots"
                  ))))

