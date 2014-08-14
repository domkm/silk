(defproject com.domkm/silk "0.0.1-SNAPSHOT"

  :description "Clojure[Script] Routing"

  :url "https://github.com/domkm/silk"

  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}

  :jar-exclusions [#"\.cljx|\.DS_Store"]

  :global-vars {*warn-on-reflection* true}

  :dependencies [[org.clojure/clojure "1.6.0"]]

  :profiles {:dev {:plugins [[com.jakemccrary/lein-test-refresh "0.5.1"]]
                   :aliases {"dev" ["test-refresh" ":growl"]}}})
