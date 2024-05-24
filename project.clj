(defproject clojure-rte "0.1.0-SNAPSHOT"
  :description "Regular type expressions, for Clojure"
  :url "https://gitlab.lrde.epita.fr/jnewton/clojure-rte.git"
  :license {:name "MIT"
            :url "https://opensource.org/licenses/MIT"}
  :plugins [[lein-cloverage "1.1.2"]
            [lein-ns-dep-graph "0.2.0-SNAPSHOT"]
            ]
  :jvm-opts ["-Xmx1g" "-XX:+HeapDumpOnOutOfMemoryError" "-Djdk.attach.allowAttachSelf"
             ;; increase stack size x6, for preventing SO errors:
             ;;   (The current default can be found with
             ;;    `java -XX:+PrintFlagsFinal -version 2>/dev/null | grep "intx ThreadStackSize"`)
             "-Xss6144k"
             ;; Prevents trivial StackOverflow errors:
             "-XX:MaxJavaStackTraceDepth=1000000"             
             ]

  
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
