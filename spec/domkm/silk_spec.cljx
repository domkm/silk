(ns domkm.silk-spec
  (:require [domkm.silk :as silk]
            [speclj.core :as spec #+cljs :include-macros #+cljs true])
  #+clj
  (:import [clojure.lang ExceptionInfo]))


;;;; URL ;;;;

(spec/describe
 "URL"

 (spec/context
 "encoding/decoding"
  (spec/with-all de-string " !'()+~")
  (spec/with-all en-string "%20%21%27%28%29%2B%7E")
  (spec/it
   "encodes"
   (spec/should= @en-string (silk/encode @de-string)))
  (spec/it
   "decodes"
   (spec/should= @de-string (silk/decode @en-string))))

 (spec/context
  "path encoding/decoding"
  (spec/with-all path-str "/foo/bar")
  (spec/with-all path-vec ["foo" "bar"])
  (spec/it
   "encodes"
   (spec/should= (silk/encode-path @path-vec) @path-str)
   (spec/should= (silk/encode-path []) "/"))
  (spec/it
   "decode"
   (spec/should= (silk/decode-path @path-str) @path-vec)))

 (spec/context
 "query encoding/decoding"
  (spec/with-all query-str "foo=bar&baz=qux")
  (spec/with-all query-map {"foo" "bar", "baz" "qux"})
  (spec/it
   "encodes"
   (spec/should= (silk/encode-query @query-map) @query-str))
  (spec/it
   "decodes"
   (spec/should= (silk/decode-query @query-str) @query-map))))


;;;; Pattern ;;;;

(spec/describe
 "pattern matching/unmatching"

 (spec/context
  "string"
  (spec/it
   "matches successfully"
   (spec/should= {} (silk/match "foo" "foo")))
  (spec/it
   "matches unsuccessfully"
   (spec/should-be-nil (silk/match "foo" "bar")))
  (spec/it
   "unmatches successfully"
   (spec/should= "foo" (silk/unmatch "foo" {}))))

 (spec/context
  "keyword"
  (spec/it
   "matches successfully"
   (spec/should= {:foo "bar"} (silk/match :foo "bar")))
  (spec/it
   "unmatches successfully"
   (spec/should= "bar" (silk/unmatch :foo {:foo "bar"})))
  (spec/it
   "unmatches unsuccessfully"
   (spec/should-throw
    ExceptionInfo (silk/unmatch :foo {}))))

 (spec/context
  "vector"
  (spec/it
   "matches successfully"
   (spec/should= {} (silk/match ["foo" "bar"] ["foo" "bar"]))
   (spec/should= {:bar "bar"} (silk/match ["foo" :bar] ["foo" "bar"]))
   (spec/should= {} (silk/match [] [])))
  (spec/it
   "matches unsuccessfully"
   (spec/should-be-nil (silk/match ["foo" "bar"] ["foo" "bar" "baz"]))
   (spec/should-be-nil (silk/match ["foo" "bar" "baz"] ["foo" "bar"]))
   (spec/should-be-nil (silk/match ["foo" "baz"] ["foo" "bar"])))
  (spec/it
   "unmatches successfully"
   (spec/should= ["foo" "bar"] (silk/unmatch ["foo" :bar] {:bar "bar"}))))

 (spec/context
  "map"
  (spec/it
   "matches successfully"
   (spec/should= {} (silk/match {} {}))
   (spec/should= {} (silk/match {"foo" "bar"} {"foo" "bar"}))
   (spec/should= {:bar "bar" :b "bloop"} (silk/match {"foo" :bar "a" :b} {"foo" "bar" "a" "bloop"})))
  (spec/it
   "matches unsuccessfully"
   (spec/should-be-nil (silk/match {"foo" :bar} {"not-foo" "blah"}))))

 (spec/context
  "routes"
  (spec/with-all routes
    (silk/routes [[:id1 [nil nil {:ring {:request-method :method}}]]
                  [:id2 [["foo" "bar" :baz]]]
                  [:id3 [nil {"a" :b}]]]))
  (spec/with-all clean-params #(dissoc % :domkm.silk/route :domkm.silk/routes :domkm.silk/url))
  (spec/it
   "matches successfully"
   (spec/should= {:method :get}
                 (@clean-params
                  (silk/match @routes (silk/map->URL {:ring {:request-method :get}}))))
   (spec/should= {:baz "baz"}
                 (@clean-params
                  (silk/match @routes (silk/map->URL {:path ["foo" "bar" "baz"]}))))
   (spec/should= {:b "b"}
                 (@clean-params
                  (silk/match @routes (silk/map->URL {:query {"a" "b"}})))))
  (spec/it
   "matches unsuccessfully"
   (spec/should-be-nil (silk/match @routes (silk/map->URL {}))))
  (spec/it
   "unmatches successfully"
   (spec/should= (silk/map->URL {:query {"a" "c"}})
                 (silk/unmatch @routes {:domkm.silk/route {:id :id3} :b "c"})))
  (spec/it
   "unmatches unsuccessfully"
   (spec/should-throw ExceptionInfo
                      (silk/unmatch @routes {})))))
