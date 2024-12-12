(defproject compiler-clojure "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [instaparse "1.5.0"]
                 [org.clojure/tools.cli "1.1.230"]
                 [meander/epsilon "0.0.650"]
                 [rewrite-clj "1.1.49"]]
  :plugins [[lein-cloverage "1.2.2"]]
  :main ^:skip-aot compiler-clojure.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
