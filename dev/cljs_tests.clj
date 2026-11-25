(require '[cljs.build.api :as build] '[clojure.java.io :as io])

(defn spec-namespace
  [{:keys [ns source-file]}]
  (let [resource (build/ns->source ns)]
    (when-not (and resource
                   (= "file" (.getProtocol resource))
                   (= (.getCanonicalFile source-file)
                      (.getCanonicalFile (io/file resource))))
      (throw (ex-info (str source-file " does not resolve as " ns)
                      {:file source-file,
                       :namespace ns,
                       :resource resource})))
    ns))

(let [runner 'domkm.silk.test-runner
      specs (->> (build/compilable->ijs (build/inputs "spec"))
                 (map spec-namespace)
                 (remove #{runner})
                 sort
                 vec)]
  (assert (seq specs) "No ClojureScript spec namespaces found under spec/")
  (build/build {:main runner,
                :optimizations :none,
                :output-dir "target/cljs-tests",
                :output-to "target/cljs-tests.js",
                :preloads specs,
                :target :nodejs}))
