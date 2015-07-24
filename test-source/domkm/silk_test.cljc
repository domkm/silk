(ns domkm.silk-test
  (:require [#?(:clj clojure.test :cljs cljs.test) :as test]))

(test/deftest test-silk
  (test/is (= 1 1)))
