(defproject com.domkm/silk "0.0.1-SNAPSHOT"

  :description "Clojure[Script] Routing"

  :url "https://github.com/domkm/silk"

  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}

  :jar-exclusions [#"\.cljx|\.DS_Store"]

  :jvm-opts ^:replace ["-server" "-Xmx4g"]

  :global-vars {*warn-on-reflection* true}

  :source-paths ["src" "target/src"]

  :test-paths ["test" "target/test"]

  :dependencies [[automat "0.1.4-SNAPSHOT"]
                 [org.clojure/clojure "1.6.0"]
                 [org.clojure/clojurescript "0.0-2322" :scope "provided"]]

  :profiles {:dev {:dependencies [[com.keminglabs/cljx "0.4.0"]
                                  [compojure "1.1.9"]
                                  [ring-mock "0.1.5"]
                                  [perforate "0.3.3"]]

                   :plugins [[com.jakemccrary/lein-test-refresh "0.5.1"]
                             [com.keminglabs/cljx "0.4.0"]
                             [lein-pdo "0.1.1"]
                             [perforate "0.3.3"]]

                   :hooks [cljx.hooks]

                   :aliases {"dev" ["do"
                                    "clean,"
                                    "cljx" "once,"
                                    ["pdo"
                                     "cljx" "auto,"
                                     "test-refresh" ":growl"]]}

                   :cljx {:builds [{:source-paths ["src"], :output-path "target/src", :rules :clj}
                                   {:source-paths ["src"], :output-path "target/src", :rules :cljs}
                                   {:source-paths ["test"], :output-path "target/test", :rules :clj}
                                   {:source-paths ["test"], :output-path "target/test", :rules :cljs}]}}})
