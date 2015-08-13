(ns domkm.silk.fsm-test
  (:require
   [domkm.silk.fsm :as fsm]
   #?@(:clj
       [[clojure.test :as t]]
       :cljs
       [[cljs.test :as t :include-macros true]])))

(t/deftest test-fsm
  (t/are [fsm in out] (= out
                         (-> fsm
                             fsm/fsm
                             fsm/precompile
                             fsm/compile
                             (fsm/match in)))
    "foo" "bar" nil
    "foo" "fo" nil
    "foo" "foo" {}
    (fsm/+ "a") "" nil
    (fsm/+ "a") "a" {}
    (fsm/+ "a") "aaaa" {}
    (fsm/| "a" "b") "a" {}
    (fsm/cat "a" (fsm/? "b")) "ab" {}
    (fsm/capture "foo" :foo) "foo" {:foo "foo"}
    (fsm/capture "foobar" :foobar) "foobar" {:foobar "foobar"}
    (fsm/capture (fsm/fsm "foo") :foo) "bar" nil))
