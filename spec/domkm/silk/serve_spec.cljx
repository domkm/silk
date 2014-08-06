(ns domkm.silk.serve-spec
  (:require [domkm.silk :as silk]
            [domkm.silk.serve :as serve]
            #+clj [ring.mock.request :refer [request]]
            [speclj.core :as spec #+cljs :include-macros #+cljs true])
  #+clj
  (:import [clojure.lang ExceptionInfo]))


#+clj
(spec/describe
 "ring functionality"
 (spec/with-all req (request :get "http://github.com/domkm/silk/pulls?q=awesome"))
 (spec/context
  "serve/request-map->URL"
  (spec/with-all url (silk/url {:scheme "http"
                                :host ["com" "github"]
                                :port "80"
                                :path ["domkm" "silk" "pulls"]
                                :query {"q" "awesome"}}))
  (spec/it
   "parses a ring request and created a URL"
   (spec/should= (assoc @url :request @req)
                 (serve/request-map->URL @req))))
 (spec/context
  "serve/ring-handler"
  (spec/context
   "no match"
   (spec/it
    "returns nil"
    (spec/should-be-nil ((serve/ring-handler {:never-route {:path ["nope"]}})
                         @req))))
  (spec/context
   "match and `get-handler` is provided"
   (spec/with-all ring-handler (serve/ring-handler
                                {:route-name [[:username :project "pulls"]]}
                                (constantly identity)))
   (spec/it
    "associates params into request"
    (spec/should= {:username "domkm" :project "silk"}
                  (-> @req
                      (@ring-handler)
                      :params
                      (select-keys [:username :project])))
    (spec/should= @req
                  (-> @req
                      (@ring-handler)
                      (dissoc :params)))))
  (spec/context
   "match and no `get-handler` provided"
   (spec/it
    "calls the route name"
    (spec/should= "silk"
                  ((serve/ring-handler {#(get-in % [:params :project])
                                        [[:username :project "pulls"]]})
                   @req))))))

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
