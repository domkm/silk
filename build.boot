(set-env!
 :project 'com.domkm/silk
 :version "1.0.0-SNAPSHOT"
 :description "Routing for Clojure and ClojureScript"
 :url "https://github.com/DomKM/silk"
 :scm {:url "https://github.com/DomKM/silk"}
 :license {"Eclipse Public License" "http://www.eclipse.org/legal/epl-v10.html"}
 :source-paths #{"source" "test-source"}
 :resource-paths #{"source"}
 :test-namespaces '#{domkm.silk-test
                     domkm.silk.pattern-test})

(merge-env!
 :dependencies
 '[
   [adzerk/boot-cljs "0.0-3308-0" :scope "test"]
   [adzerk/boot-test "1.0.4" :scope "test"]
   [adzerk/bootlaces "0.1.11" :scope "test"]
   [automat "0.2.0-alpha1"]
   [boot-cljs-test/node-runner "0.1.0" :scope "test"]
   [org.clojure/clojure "1.7.0" :scope "provided"]
   [org.clojure/clojurescript "1.7.107" :classifier "aot" :scope "provided"]
   [tonsky/boot-anybar "0.1.0" :scope "test"]
   ])

(require
 '[adzerk.boot-cljs :refer [cljs]]
 '[adzerk.boot-test :as boot-test-clj]
 '[adzerk.bootlaces :as bootlaces :refer [bootlaces! build-jar push-snapshot push-release]]
 '[boot-cljs-test.node-runner :as boot-test-cljs]
 '[tonsky.boot-anybar :refer [anybar]]
 )

(task-options! pom (get-env))

(-> :version get-env bootlaces!)

(deftask test-clj
  "Run the Clojure tests."
  []
  (boot-test-clj/test :namespaces (get-env :test-namespaces)))

(deftask test-cljs
  "Run the ClojureScript tests."
  []
  (comp
   (boot-test-cljs/cljs-test-node-runner :namespaces (get-env :test-namespaces))
   (cljs :compiler-options {:warnings {:invalid-arithmetic false}} ; http://dev.clojure.org/jira/browse/CLJS-1419
         :optimizations :none
         :source-map true)
   (boot-test-cljs/run-cljs-test)))

(deftask test
  "Run the tests."
  []
  (comp
   (test-clj)
   (test-cljs)))

(deftask dev
  "Start the development process."
  []
  (comp
   (watch)
   (anybar)
   (speak)
   (test)
   (repl :server true)))
