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

(defproject clojure-rte "0.1.0-SNAPSHOT"
  :description "Regular type expressions, for Clojure"
  :url "https://gitlab.lrde.epita.fr/jnewton/clojure-rte.git"
  :license {:name "MIT"
            :url "https://opensource.org/licenses/MIT"}
  :plugins [[lein-cloverage "1.1.2"]
            [lein-ns-dep-graph "0.2.0-SNAPSHOT"]
            ]
  :jvm-opts ["-Xmx1g" "-XX:+HeapDumpOnOutOfMemoryError" "-Djdk.attach.allowAttachSelf"] ;; 
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [lein-cloverage "1.1.2"]
                 [org.clojure/data.json "1.0.0"]
                 [com.clojure-goes-fast/clj-async-profiler "0.5.1"]
                 [backtick "0.3.4"]
                 [org.clojure/core.memoize "1.0.236"]
                 [org.clojure/math.combinatorics "0.1.6"]]
  :main ^:skip-aot clojure-rte.rte-core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
