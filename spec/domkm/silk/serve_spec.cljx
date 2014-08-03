(ns domkm.silk.serve-spec
  (:require [domkm.silk :as silk]
            [domkm.silk.serve :as serve]
            [speclj.core :as spec #+cljs :include-macros #+cljs true])
  #+clj
  (:import [clojure.lang ExceptionInfo]))


(spec/describe
 "serve/RequestMethodPattern"

 (spec/context
  "serve/request-method-pattern"
  (spec/it
   "matches successfully"
   (spec/should= {} (silk/match (serve/request-method-pattern :get false) :get))
   (spec/should= {} (silk/match (serve/request-method-pattern :get true) :get))
   (spec/should= {} (silk/match (serve/request-method-pattern :get true) nil)))
  (spec/it
   "matches unsuccessfully"
   (spec/should-be-nil (silk/match (serve/request-method-pattern :get false) :post))
   (spec/should-be-nil (silk/match (serve/request-method-pattern :get true) :post)))
  (spec/it
   "unmatches successfully"
   (spec/should= :get (silk/unmatch (serve/request-method-pattern :get false) {}))
   (spec/should= nil (silk/unmatch (serve/request-method-pattern :get true) {}))))

 (spec/context
  "serve/method and serve/?method"
  (spec/with-all post-ptrn (serve/request-method-pattern :post false))
  (spec/with-all ?post-ptrn (serve/request-method-pattern :post true))
  (spec/with-all get-in-url (fn [m] (get-in m [:request :request-method])))
  (spec/it
   "associates request-method-pattern into URL map"
   (spec/should= @post-ptrn
                 (@get-in-url (serve/method :post [["a" "b"]])))
   (spec/should= @?post-ptrn
                 (@get-in-url (serve/?method :post [["a" "b"]])))
   (spec/should= @?post-ptrn
                 (@get-in-url (serve/?method :post {}))))))
