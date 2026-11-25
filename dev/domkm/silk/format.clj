(ns domkm.silk.format
  (:require [clojure.java.io :as io]
            [zprint.core :as zprint])
  (:import [java.nio.file FileVisitOption Files LinkOption Path]))

(defn source-files
  [root]
  (with-open [paths (Files/walk (.toPath (io/file root))
                                (make-array FileVisitOption 0))]
    (filterv #(and (re-find #"\.(clj[sc]?|edn)$" (str %))
                   (Files/isRegularFile
                    %
                    (into-array LinkOption [LinkOption/NOFOLLOW_LINKS])))
             (iterator-seq (.iterator paths)))))

(defn formatting-change
  [^Path path]
  (let [file (.toFile path)
        source (slurp file :encoding "UTF-8")
        formatted (zprint/zprint-file-str source (str path))]
    (when (not= source formatted) [file formatted])))

(defn -main
  [& [mode :as args]]
  (when-not (and (= 1 (count args)) (#{"check" "fix"} mode))
    (binding [*out* *err*] (println "Usage: clojure -Srepro -M:fmt check|fix"))
    (System/exit 2))
  ;; Mark the configuration complete so zprint ignores user settings.
  (zprint/set-options! {:configured? true
                        :style :community
                        :width 80
                        :map {:sort? false
                              :sort-in-code? false
                              :comma? false
                              :force-nl? true}
                        :set {:sort? false
                              :sort-in-code? false}
                        :reader-cond {:sort? false
                                      :sort-in-code? false}
                        :comment {:wrap? false
                                  :smart-wrap? false}
                        :fn-map {"spec/describe" :arg1-force-nl-body
                                 "spec/context" :arg1-force-nl-body
                                 "spec/it" :arg1-force-nl-body}})
  (let [fix? (= mode "fix")
        changes (->> ["src" "spec" "dev" "deps.edn"]
                     (mapcat source-files)
                     (sort-by str)
                     (keep formatting-change)
                     vec)]
    (doseq [[file formatted] changes]
      (when fix? (spit file formatted :encoding "UTF-8"))
      (println (if fix? "Formatted:" "Needs formatting:") (str file)))
    (when (and (not fix?) (seq changes)) (System/exit 1))))
