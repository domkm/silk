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
   (spec/should= (silk/url (merge @req @url))
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
 (spec/it
  "matches successfully"
  (spec/should= {} (silk/match serve/GET :get))
  (spec/should= {} (silk/match (serve/GET) {:request-method :get}))
  (spec/should= {} (silk/match (serve/GET {}) {:request-method :get}))
  (spec/should= {} (silk/match serve/?GET nil)))
 (spec/it
  "matches unsuccessfully"
  (spec/should-be-nil (silk/match serve/?GET :post)))
 (spec/it
  "unmatches successfully"
  (spec/should= :get (silk/unmatch serve/GET {})))
 (spec/it
  "unmatches unsuccessfully"
  (spec/should= :get (silk/unmatch serve/GET {})))
 (spec/it
  "can be def'd"
  (spec/should-not-throw (def a-post-request-pattern (serve/POST)))))
