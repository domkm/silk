(ns domkm.silk.pattern-test
  (:require
   [domkm.silk.pattern :as ptrn]
   #?@(:clj
       [[clojure.test :as t]]
       :cljs
       [[cljs.test :as t :include-macros true]])))

(t/deftest test-match
  (t/are [ptrn in out] (= out
                         (-> ptrn
                             ptrn/precompile
                             ptrn/compile
                             (ptrn/match in)))
    "foo" "bar" nil
    "foo" "fo" nil
    "foo" "foo" {}
    (ptrn/+ "a") "" nil
    (ptrn/+ "a") "a" {}
    (ptrn/+ "a") "aaaa" {}
    (ptrn/| "a" "b") "a" {}
    (ptrn/cat "a" (ptrn/? "b")) "ab" {}
    (ptrn/capture "foo" :foo) "foo" {:foo "foo"}
    (ptrn/capture "foobar" :foobar) "foobar" {:foobar "foobar"}))
