(defproject clojure-rte "0.1.0-SNAPSHOT"
  :description "Regular type expressions, for Clojure"
  :url "git@gitlab.lre.epita.fr:jnewton/clojure-rte.git"
  :license {:name "MIT"
            :url "https://opensource.org/licenses/MIT"}
  :plugins [[lein-cloverage "1.2.4"]
            [lein-ns-dep-graph "0.2.0-SNAPSHOT"]
            [cider/cider-nrepl "0.55.0"]
            ]
  :jvm-opts ["-Xmx1g" "-XX:+HeapDumpOnOutOfMemoryError" "-Djdk.attach.allowAttachSelf"
             ;; increase stack size x6, for preventing SO errors:
             ;;   (The current default can be found with
             ;;    `java -XX:+PrintFlagsFinal -version 2>/dev/null | grep "intx ThreadStackSize"`)
             "-Xss6144k"
             ;; Prevents trivial StackOverflow errors:
             "-XX:MaxJavaStackTraceDepth=1000000"             

             ;; the --add-opens= is for supressing the following warnings
             ;; WARNING: An illegal reflective access operation has occurred
             ;; WARNING: Illegal reflective access by clojure.lang.InjectedInvoker/0x0000000800232040 (file:/Users/jimka/.m2/repository/org/clojure/clojure/1.10.3/clojure-1.10.3.jar) to method com.sun.xml.internal.stream.writers.XMLStreamWriterImpl.writeCharacters(java.lang.String)
             ;; WARNING: Please consider reporting this to the maintainers of clojure.lang.InjectedInvoker/0x0000000800232040
             ;; WARNING: Use --illegal-access=warn to enable warnings of further illegal reflective access operations
             ;; WARNING: All illegal access operations will be denied in a future release

             "--add-opens=java.xml/com.sun.xml.internal.stream.writers=ALL-UNNAMED"
             ]

  
  :dependencies [[org.clojure/clojure "1.12.0"]
                 [org.clojure/data.csv "1.1.0"]
                 [metasoarous/oz "2.0.0-alpha5"]
                 [lein-cloverage "1.2.4"]
                 [org.clojure/data.json "1.0.0"]
                 ;; [com.clojure-goes-fast/clj-async-profiler "0.5.1"]
                 [backtick "0.3.4"]
                 [org.clojure/core.memoize "1.0.236"]
                 ;; [org.clojure/math.combinatorics "0.1.6"]
                 ]
  :main ^:skip-aot rte.core
  :target-path "target/%s"
  :profiles {:test {:plugins [[lein-test-report-junit-xml "0.2.0"]]}
             :uberjar {:aot :all}})
