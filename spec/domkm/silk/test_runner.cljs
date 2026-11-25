(ns domkm.silk.test-runner
  (:require [speclj.report.progress :as progress]
            [speclj.run.standard :as standard]))

(defn -main
  []
  (standard/arm)
  (let [failures (standard/run-specs :reporters
                                     [(progress/new-progress-reporter)])]
    (set! (.-exitCode js/process) (if (zero? failures) 0 1))))

(set! *main-cli-fn* -main)
