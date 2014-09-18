(defproject com.domkm/silk "0.0.1"

  :description "Clojure[Script] Routing"

  :url "https://github.com/domkm/silk"

  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}

  :jar-exclusions [#"\.cljx|\.DS_Store"]

  :global-vars {*warn-on-reflection* true}

  :source-paths ["src" "target/src"]

  :test-paths ["spec" "target/spec"]

  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/clojurescript "0.0-2322" :scope "provided"]]

  :profiles {:dev {:dependencies [[ring-mock "0.1.5"]
                                  [speclj "3.1.0"]]}}

  :plugins [[com.keminglabs/cljx "0.4.0"]
            [lein-cljsbuild "1.0.3"]
            [lein-pdo "0.1.1"]
            [speclj "3.1.0"]]

  :aliases {"dev" ["do"
                   "clean,"
                   "cljx" "once,"
                   ["pdo"
                    "cljx" "auto,"
                    "cljsbuild" "auto,"
                    "spec" "--autotest"]]}

  :hooks [cljx.hooks]

  :cljx {:builds [{:source-paths ["src"], :output-path "target/src", :rules :clj}
                  {:source-paths ["src"], :output-path "target/src", :rules :cljs}
                  {:source-paths ["spec"], :output-path "target/spec", :rules :clj}
                  {:source-paths ["spec"], :output-path "target/spec", :rules :cljs}]}

  :cljsbuild {:test-commands {"spec" ["phantomjs" "bin/speclj" "target/js/spec.js"]}
              :builds {:spec {:source-paths ["src" "target/src" "spec" "target/spec"]
                              :compiler {:output-to "target/js/spec.js"
                                         :optimizations :whitespace
                                         :pretty-print true}
                              :notify-command ["phantomjs" "bin/speclj" "target/js/spec.js"]}}})
