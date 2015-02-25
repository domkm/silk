(ns domkm.silk-spec
  (:require [domkm.silk :as silk]
            [speclj.core :as spec #+cljs :include-macros #+cljs true])
  #+clj
  (:import [clojure.lang ExceptionInfo]))


;;;; Helpers ;;;;

#+cljs
(def ^:private AssertionError js/Error)


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
  (spec/with-all query-str "baz=qux&foo=bar")
  (spec/with-all query-map {"foo" "bar", "baz" "qux"})
  (spec/it
   "encodes"
   (spec/should= (silk/encode-query @query-map) @query-str))
  (spec/it
   "decodes"
   (spec/should= (silk/decode-query @query-str) @query-map))
  (spec/it
   "empty map produces no query-string"
   (spec/should= (silk/encode-query {}) "")))

 (spec/context
  "parsing"
  (spec/it
   "parses urls"
   (spec/should= (silk/map->URL {:path ["a"] :query {}})
                 (silk/url "/a"))
   (spec/should= (silk/map->URL {:path ["a"] :query {}})
                 (silk/url "/a?"))
   (spec/should= (silk/map->URL {:path ["a"] :query {"b" "c"}})
                 (silk/url "/a?b=c"))
   (spec/should= (silk/map->URL {:query {"b" "c"} :path []})
                 (silk/url "?b=c"))))

 (spec/context
   "toString"
   (spec/it
     "Encodes urls"
     (spec/should= "/users?q=foo"
                 (.toString (silk/map->URL {:path ["users"] :query {"q" "foo"}})))
     (spec/should= "/users"
                   (.toString (silk/map->URL {:path ["users"] :query {"q" nil}}))))))

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
   (spec/should-throw AssertionError (silk/unmatch :foo {}))))

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
  "silk/regex"
  (spec/it
   "matches successfully"
   (spec/should= {:re "foo"}
                 (silk/match (silk/regex :re #"foo") "foo"))
   (spec/should= {:re ["foobar" "bar"]}
                 (silk/match (silk/regex :re #"foo(.*)") "foobar")))
  (spec/it
   "matches unsuccessfully"
   (spec/should-be-nil (silk/match (silk/regex :re #"foo") "bar")))
  (spec/it
   "unmatches successfully"
   (spec/should= "foo"
                 (silk/unmatch (silk/regex :re #"foo") {:re "foo"}))
   (spec/should= "foobar"
                 (silk/unmatch (silk/regex :re #"foo(.*)") {:re ["foobar" "foo"]})))
  (spec/it
   "unmatches unsuccessfully"
   (spec/should-throw AssertionError
                      (silk/unmatch (silk/regex :re #"foo(.*)") {:re "a"}))
   (spec/should-throw AssertionError
                      (silk/unmatch (silk/regex :re #"foo(.*)") {:re ["bar" "foo"]}))))

 (spec/context
  "silk/bool"
  (spec/it
   "matches successfully"
   (spec/should= {:happy true}
                 (silk/match (silk/bool :happy) "true"))
   (spec/should= {:happy false}
                 (silk/match (silk/bool :happy) "false")))
  (spec/it
   "matches unsuccessfully"
   (spec/should-be-nil (silk/match (silk/bool :happy) "truth")))
  (spec/it
   "unmatches successfully"
   (spec/should= "true"
                 (silk/unmatch (silk/bool :happy) {:happy true})))
  (spec/it
   "unmatches unsuccessfully"
   (spec/should-throw AssertionError (silk/unmatch (silk/bool :happy) {}))
   (spec/should-throw AssertionError (silk/unmatch (silk/bool :happy) {:id []}))))

 (spec/context
  "silk/int"
  (spec/it
   "matches successfully"
   (spec/should= {:id 42}
                 (silk/match (silk/int :id) "42")))
  (spec/it
   "matches unsuccessfully"
   (spec/should-be-nil (silk/match (silk/int :id) "a42")))
  (spec/it
   "unmatches successfully"
   (spec/should= "42"
                 (silk/unmatch (silk/int :id) {:id 42})))
  (spec/it
   "unmatches unsuccessfully"
   (spec/should-throw AssertionError (silk/unmatch (silk/int :id) {}))
   (spec/should-throw AssertionError (silk/unmatch (silk/int :id) {:id []}))))

 (spec/context
  "silk/uuid"
  (spec/it
   "matches successfully"
   (spec/should= {:uuid #uuid "c11902f0-21b6-4645-a218-9fa40ef69333"}
                 (silk/match (silk/uuid :uuid) "c11902f0-21b6-4645-a218-9fa40ef69333")))
  (spec/it
   "matches unsuccessfully"
   (spec/should-be-nil (silk/match (silk/uuid :uuid) "c11902f0-21b6-4645")))
  (spec/it
   "unmatches successfully"
   (spec/should= "c11902f0-21b6-4645-a218-9fa40ef69333"
                 (silk/unmatch (silk/uuid :uuid) {:uuid #uuid "c11902f0-21b6-4645-a218-9fa40ef69333"})))
  (spec/it
   "unmatches unsuccessfully"
   (spec/should-throw AssertionError (silk/unmatch (silk/uuid :uuid) {}))
   (spec/should-throw AssertionError (silk/unmatch (silk/uuid :uuid) {:uuid "uuid"}))))

 (spec/context
  "silk/cat"
  (spec/it
   "matches successfully"
   (spec/should= {:answer "42"}
                 (silk/match (silk/cat "foo" :answer "bar") "foo42bar"))
   (spec/should= {:answer 42}
                 (silk/match (silk/cat "foo" (silk/int :answer) "bar") "foo42bar"))
   (spec/should= {:id 42 :this "j"}
                 (silk/match (silk/cat "user-" (silk/int :id) "-fred" (silk/? :this {:this "that"}) "s") "user-42-fredjs"))
   (spec/should= {:id 42 :this "that"}
                 (silk/match (silk/cat "user-" (silk/int :id) "-fred" (silk/? :this {:this "that"}) "s") "user-42-freds"))
   (spec/should= {:id "blah"}
                 (silk/match (silk/cat "article-" (silk/? :id {:id "not-found"})) "article-blah")))
  (spec/it
   "matches unsuccessfully"
   (spec/should-be-nil (silk/match (silk/cat "foo" :answer "bar") "foop"))
   (spec/should-be-nil (silk/match (silk/cat "pre" (silk/int :i) "post") "pre5cpost")))
  (spec/it
   "unmatches successfully"
   (spec/should= "foo42bar"
                 (silk/unmatch (silk/cat "foo" :answer "bar") {:answer "42"}))
   (spec/should= "foo42bar"
                 (silk/unmatch (silk/cat "foo" (silk/int :answer) "bar") {:answer 42})))
  (spec/it
   "unmatches unsuccessfully"
   (spec/should-throw AssertionError (silk/unmatch (silk/cat "foo" :answer "bar") {}))))

 (spec/context
  "silk/?"
  (spec/it
   "matches successfully"
   (spec/should= {:answer 42}
                 (silk/match (silk/? (silk/int :answer) {:answer 42}) nil))
   (spec/should= {:answer 42}
                 (silk/match (silk/? (silk/int :answer) {:answer 42}) "42"))
   (spec/should= {:answer 100}
                 (silk/match (silk/? (silk/int :answer) {:answer 42}) "100"))
   (spec/should= {:answer "42"}
                 (silk/match (silk/? :answer {:answer "42"}) nil)))
  (spec/it
   "matches unsuccessfully"
   (-> (silk/int :answer)
       (silk/? {:answer 42})
       (silk/match "foob")
       spec/should-be-nil))
  (spec/it
   "unmatches successfully"
   (spec/should= "42"
                 (-> (silk/int :answer)
                     (silk/? {:answer 42})
                     (silk/unmatch {:answer 42})))
   (spec/should= "42"
                 (-> (silk/int :answer)
                     (silk/? {:answer 42})
                     (silk/unmatch {}))))
  (spec/it
   "unmatches unsuccessfully"
   (spec/should-throw AssertionError (-> (silk/int :answer)
                                         (silk/? {:answer 42})
                                         (silk/unmatch {:answer "foo"})))))

 (spec/context
  "silk/routes"
  (spec/with-all routes
    (silk/routes [[:id1 [nil nil {:request-method :method}]]
                  (silk/routes [[:id2 [["foo" "bar" :baz]]]])
                  [:id3 [nil {"a" :b}]]
                  [:id4 [["search"] {"q" (silk/? :q {:q "default"})}]]
                  [:id5 [["users"] {"q" (silk/? :q)}]]]))
  (spec/with-all clean-params
    #(dissoc % :domkm.silk/routes :domkm.silk/url :domkm.silk/pattern :domkm.silk/name))
  (spec/it
   "matches successfully"
   (spec/should= {:method :get}
                 (@clean-params
                  (silk/match @routes (silk/url {:request-method :get}))))
   (spec/should= {:method :get}
                 (@clean-params
                  (silk/match @routes {:request-method :get})))
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
                 (silk/unmatch @routes {:domkm.silk/name :id3 :b "c"})))
  (spec/it
   "unmatches unsuccessfully"
   (spec/should-throw AssertionError  (silk/unmatch @routes {})))
  (spec/it
   "arrives"
   (spec/should= {:baz "baz"}
                 (@clean-params
                  (silk/arrive @routes "/foo/bar/baz")))
   (spec/should= {:b "b"}
                 (@clean-params
                  (silk/arrive @routes "/?a=b")))
   (spec/should= {:q "foo"}
                 (@clean-params
                  (silk/arrive @routes "/search?q=foo")))
   (spec/should= {:q "default"}
                 (@clean-params
                  (silk/arrive @routes "/search")))
   (spec/should= {:q nil}
                 (@clean-params
                  (silk/arrive @routes "/users")))
   (spec/should= {:q "bar"}
                 (@clean-params
                  (silk/arrive @routes "/users?q=bar"))))
  (spec/it
   "departs"
   (spec/should= "/foo/bar/bloop"
                 (silk/depart @routes :id2 {:baz "bloop"}))
   (spec/should= "/?a=bloop"
                 (silk/depart @routes :id3 {:b "bloop"}))
   (spec/should= "/search?q=default"
                 (silk/depart @routes :id4))
   (spec/should= "/search?q=foo"
                 (silk/depart @routes :id4 {:q "foo"}))
   (spec/should= "/users"
                 (silk/depart @routes :id5))
   (spec/should= "/users?q=bar"
                 (silk/depart @routes :id5 {:q "bar"})))))
