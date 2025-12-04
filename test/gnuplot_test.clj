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
                  :output-file-base-name "sample"
                  :title "sample plots"
                  ))))

(deftest t-histogram-1
  (testing "histogram-1"
    (is
     (sut/histogram '(["tree-split-linear" (2 5 86 4 2 3 2 2 2 7)]
                      ["tree-split-gauss" (6 5 3 2 15 4 14 3 4 3 2)]
                      ["tree-split-inv-gauss" (10 2 14 8 3 3 2 9 3)]
                      ["flajolet" (5 6 3 3 44 7 4 19 2 29 3 2 12 7)]
                      ["tbnl" (31 19 19 14 60 14 14 2 5 16 23 303 33 )]
                      ["comb" (2 4 6 4 2 5 2 3 15 5 4 15 2 3 2 2 3 2 3)]
                      )
                    :keep-if #(<= % 9)
                    :other-label ">= 10"
                    :view true))))

