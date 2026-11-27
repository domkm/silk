(ns domkm.silk-spec
  (:require [clojure.string :as str]
            [domkm.silk :as silk]
            [speclj.core :as spec #?@(:cljs [:include-macros true])]))


;;;; Helpers ;;;;

#?(:cljs (def ^:private AssertionError js/Error))


;;;; URL ;;;;

(spec/describe "URL"
  (spec/context "encoding/decoding"
    (spec/with-all de-string " !'()+~")
    (spec/with-all en-string "%20%21%27%28%29%2B%7E")
    (spec/it "encodes"
      (spec/should= @en-string (silk/encode @de-string)))
    (spec/it "decodes"
      (spec/should= @de-string (silk/decode @en-string))))
  (spec/context "path encoding/decoding"
    (spec/with-all path-str "/foo/bar")
    (spec/with-all path-vec ["foo" "bar"])
    (spec/it "encodes"
      (spec/should= (silk/encode-path @path-vec) @path-str)
      (spec/should= (silk/encode-path []) "/"))
    (spec/it "decode"
      (spec/should= (silk/decode-path @path-str) @path-vec))
    (spec/it "decodes a literal plus according to the platform"
      (spec/should= [#?(:clj "a b"
                        :cljs "a+b") "a+b"]
                    (silk/decode-path "/a+b/a%2Bb"))))
  (spec/context "query encoding/decoding"
    (spec/with-all query-str "baz=qux&foo=bar")
    (spec/with-all query-map
                   {"baz" "qux",
                    "foo" "bar"})
    (spec/it "encodes"
      (spec/should= (silk/encode-query @query-map) @query-str))
    (spec/it "decodes"
      (spec/should= (silk/decode-query @query-str) @query-map))
    (spec/it "decodes a literal plus according to the platform"
      (spec/should= {"encoded" "a+b",
                     "q" #?(:clj "a b"
                            :cljs "a+b")}
                    (silk/decode-query "q=a+b&encoded=a%2Bb")))
    (spec/it "keeps repeated values in order"
      (spec/should= {"bar" "Linda",
                     "foo" ["Greg" "John" "Greg"]}
                    (silk/decode-query "foo=Greg&bar=Linda&foo=John&foo=Greg")))
    (spec/it "encodes each vector item as a separate query pair"
      (let [query {"q" ["a&b=c" ""]}
            encoded (silk/encode-query (assoc query "omitted" []))]
        (spec/should= "q=a%26b%3Dc&q=" encoded)
        (spec/should= query (silk/decode-query encoded))))
    (spec/it "coerces raw query vector elements to strings"
      (spec/should= "id=2&id=7" (silk/encode-query {"id" [2 7]}))
      (let [encoded (silk/encode-query {"id" ["2" nil]})]
        (spec/should= "id=2&id=" encoded)
        (spec/should= {"id" ["2" ""]} (silk/decode-query encoded))))
    (spec/it "decodes blank values and splits on the first equals sign"
      (spec/should= {"q" ["" "" "a=b"]} (silk/decode-query "q&q=&q=a=b")))
    (spec/it "ignores empty pairs but keeps explicit empty and bare keys"
      (spec/should= {} (silk/decode-query "&;&&;"))
      (spec/should= {"" "value",
                     "tag" ""}
                    (silk/decode-query ";&=value&&tag;&")))
    (spec/it "empty map produces no query-string"
      (spec/should= (silk/encode-query {}) "")))
  (spec/context "parsing"
    (spec/it "parses urls"
      (spec/should= (silk/map->URL {:path ["a"],
                                    :query {}})
                    (silk/url "/a"))
      (spec/should= (silk/map->URL {:path ["a"],
                                    :query {}})
                    (silk/url "/a?"))
      (spec/should= (silk/map->URL {:path ["a"],
                                    :query {"b" "c"}})
                    (silk/url "/a?b=c"))
      (spec/should= (silk/map->URL {:path [],
                                    :query {"b" "c"}})
                    (silk/url "?b=c")))
    (spec/it "splits the path from the query at the first question mark"
      (spec/should= (silk/map->URL {:path ["a"],
                                    :query {"q" ["a?b" "?c??"]}})
                    (silk/url "/a?q=a?b&q=?c??"))))
  (spec/context "toString"
    (spec/it "Encodes urls"
      (spec/should= "/users?q=foo"
                    (.toString (silk/map->URL {:path ["users"],
                                               :query {"q" "foo"}})))
      (spec/should= "/users"
                    (.toString (silk/map->URL {:path ["users"],
                                               :query {"q" nil}})))
      (spec/should= "/users"
                    (str (silk/map->URL {:path ["users"],
                                         :query {"q" []}}))))))

;;;; Pattern ;;;;

(spec/describe "pattern matching/unmatching"
  (spec/context "string"
    (spec/it "matches successfully"
      (spec/should= {} (silk/match "foo" "foo")))
    (spec/it "matches unsuccessfully"
      (spec/should-be-nil (silk/match "foo" "bar")))
    (spec/it "unmatches successfully"
      (spec/should= "foo" (silk/unmatch "foo" {}))))
  (spec/context "keyword"
    (spec/it "matches successfully"
      (spec/should= {:foo "bar"} (silk/match :foo "bar")))
    (spec/it "unmatches successfully"
      (spec/should= "bar" (silk/unmatch :foo {:foo "bar"})))
    (spec/it "unmatches unsuccessfully"
      (spec/should-throw AssertionError (silk/unmatch :foo {}))))
  (spec/context "vector"
    (spec/it "matches successfully"
      (spec/should= {} (silk/match ["foo" "bar"] ["foo" "bar"]))
      (spec/should= {:bar "bar"} (silk/match ["foo" :bar] ["foo" "bar"]))
      (spec/should= {} (silk/match [] [])))
    (spec/it "matches unsuccessfully"
      (spec/should-be-nil (silk/match ["foo" "bar"] ["foo" "bar" "baz"]))
      (spec/should-be-nil (silk/match ["foo" "bar" "baz"] ["foo" "bar"]))
      (spec/should-be-nil (silk/match ["foo" "baz"] ["foo" "bar"])))
    (spec/it "unmatches successfully"
      (spec/should= ["foo" "bar"] (silk/unmatch ["foo" :bar] {:bar "bar"}))))
  (spec/context "map"
    (spec/it "matches successfully"
      (spec/should= {} (silk/match {} {}))
      (spec/should= {} (silk/match {"foo" "bar"} {"foo" "bar"}))
      (spec/should= {:b "bloop",
                     :bar "bar"}
                    (silk/match {"a" :b,
                                 "foo" :bar}
                      {"a" "bloop",
                       "foo" "bar"})))
    (spec/it "matches unsuccessfully"
      (spec/should-be-nil (silk/match {"foo" :bar} {"not-foo" "blah"}))))
  (spec/context "silk/regex"
    (spec/it "matches successfully"
      (spec/should= {:re "foo"} (silk/match (silk/regex :re #"foo") "foo"))
      (spec/should= {:re ["foobar" "bar"]}
                    (silk/match (silk/regex :re #"foo(.*)") "foobar")))
    (spec/it "matches unsuccessfully"
      (spec/should-be-nil (silk/match (silk/regex :re #"foo") "bar")))
    (spec/it "unmatches successfully"
      (spec/should= "foo" (silk/unmatch (silk/regex :re #"foo") {:re "foo"}))
      (spec/should= "foobar"
                    (silk/unmatch (silk/regex :re #"foo(.*)")
                                  {:re ["foobar" "foo"]})))
    (spec/it "unmatches unsuccessfully"
      (spec/should-throw AssertionError
                         (silk/unmatch (silk/regex :re #"foo(.*)") {:re "a"}))
      (spec/should-throw AssertionError
                         (silk/unmatch (silk/regex :re #"foo(.*)")
                                       {:re ["bar" "foo"]}))))
  (spec/context "silk/bool"
    (spec/it "matches successfully"
      (spec/should= {:happy true} (silk/match (silk/bool :happy) "true"))
      (spec/should= {:happy false} (silk/match (silk/bool :happy) "false")))
    (spec/it "matches unsuccessfully"
      (spec/should-be-nil (silk/match (silk/bool :happy) "truth")))
    (spec/it "unmatches successfully"
      (spec/should= "true" (silk/unmatch (silk/bool :happy) {:happy true})))
    (spec/it "unmatches unsuccessfully"
      (spec/should-throw AssertionError (silk/unmatch (silk/bool :happy) {}))
      (spec/should-throw AssertionError
                         (silk/unmatch (silk/bool :happy) {:id []}))))
  (spec/context "silk/int"
    (spec/it "matches successfully"
      (spec/should= {:id 42} (silk/match (silk/int :id) "42")))
    (spec/it "matches unsuccessfully"
      (spec/should-be-nil (silk/match (silk/int :id) "a42")))
    (spec/it "accepts the platform maximum and rejects overflow"
      (let [pattern (silk/int :id)
            max-id #?(:clj Long/MAX_VALUE
                      :cljs js/Number.MAX_SAFE_INTEGER)
            overflow #?(:clj "9223372036854775808"
                        :cljs "9007199254740992")]
        (spec/should= {:id max-id} (silk/match pattern (str max-id)))
        (spec/should-be-nil (silk/match pattern overflow))))
    (spec/it "unmatches successfully"
      (spec/should= "42" (silk/unmatch (silk/int :id) {:id 42})))
    (spec/it "unmatches unsuccessfully"
      (spec/should-throw AssertionError (silk/unmatch (silk/int :id) {}))
      (spec/should-throw AssertionError
                         (silk/unmatch (silk/int :id) {:id []}))))
  (spec/context "silk/uuid"
    (spec/it "matches successfully"
      (spec/should= {:uuid #uuid "c11902f0-21b6-4645-a218-9fa40ef69333"}
                    (silk/match (silk/uuid :uuid)
                      "c11902f0-21b6-4645-a218-9fa40ef69333")))
    (spec/it "matches unsuccessfully"
      (spec/should-be-nil (silk/match (silk/uuid :uuid) "c11902f0-21b6-4645")))
    (spec/it "unmatches successfully"
      (spec/should= "c11902f0-21b6-4645-a218-9fa40ef69333"
                    (silk/unmatch
                     (silk/uuid :uuid)
                     {:uuid #uuid "c11902f0-21b6-4645-a218-9fa40ef69333"})))
    (spec/it "unmatches unsuccessfully"
      (spec/should-throw AssertionError (silk/unmatch (silk/uuid :uuid) {}))
      (spec/should-throw AssertionError
                         (silk/unmatch (silk/uuid :uuid) {:uuid "uuid"}))))
  (spec/context "silk/cat"
    (spec/it "matches successfully"
      (spec/should= {:answer "42"}
                    (silk/match (silk/cat "foo"
                                          :answer
                                          "bar")
                      "foo42bar"))
      (spec/should= {:answer 42}
                    (silk/match (silk/cat "foo"
                                          (silk/int :answer)
                                          "bar")
                      "foo42bar"))
      (spec/should= {:id 42,
                     :this "j"}
                    (silk/match (silk/cat "user-"
                                          (silk/int :id)
                                          "-fred"
                                          (silk/? :this {:this "that"})
                                          "s")
                      "user-42-fredjs"))
      (spec/should= {:id 42,
                     :this "that"}
                    (silk/match (silk/cat "user-"
                                          (silk/int :id)
                                          "-fred"
                                          (silk/? :this {:this "that"})
                                          "s")
                      "user-42-freds"))
      (spec/should= {:id "blah"}
                    (silk/match (silk/cat "article-"
                                          (silk/? :id {:id "not-found"}))
                      "article-blah")))
    (spec/it "matches unsuccessfully"
      (spec/should-be-nil (silk/match (silk/cat "foo"
                                                :answer
                                                "bar")
                            "foop"))
      (spec/should-be-nil (silk/match (silk/cat "pre"
                                                (silk/int :i)
                                                "post")
                            "pre5cpost")))
    (spec/it "unmatches successfully"
      (spec/should= "foo42bar"
                    (silk/unmatch (silk/cat "foo"
                                            :answer
                                            "bar")
                                  {:answer "42"}))
      (spec/should= "foo42bar"
                    (silk/unmatch (silk/cat "foo"
                                            (silk/int :answer)
                                            "bar")
                                  {:answer 42})))
    (spec/it "unmatches unsuccessfully"
      (spec/should-throw AssertionError
                         (silk/unmatch (silk/cat "foo"
                                                 :answer
                                                 "bar")
                                       {}))))
  (spec/context "silk/?"
    (spec/it "matches successfully"
      (spec/should= {:answer 42}
                    (silk/match (silk/? (silk/int :answer) {:answer 42}) nil))
      (spec/should= {:answer 42}
                    (silk/match (silk/? (silk/int :answer) {:answer 42}) "42"))
      (spec/should= {:answer 100}
                    (silk/match (silk/? (silk/int :answer) {:answer 42}) "100"))
      (spec/should= {:answer "42"}
                    (silk/match (silk/? :answer {:answer "42"}) nil)))
    (spec/it "matches unsuccessfully"
      (-> (silk/int :answer)
          (silk/? {:answer 42})
          (silk/match "foob")
          spec/should-be-nil))
    (spec/it "unmatches successfully"
      (spec/should= "42"
                    (-> (silk/int :answer)
                        (silk/? {:answer 42})
                        (silk/unmatch {:answer 42})))
      (spec/should= "42"
                    (-> (silk/int :answer)
                        (silk/? {:answer 42})
                        (silk/unmatch {}))))
    (spec/it "unmatches unsuccessfully"
      (spec/should-throw AssertionError
                         (-> (silk/int :answer)
                             (silk/? {:answer 42})
                             (silk/unmatch {:answer "foo"})))))
  (spec/context "silk/routes"
    (spec/with-all routes
                   (silk/routes
                    [[:id1 [nil nil {:request-method :method}]]
                     (silk/routes [[:id2 [["foo" "bar" :baz]]]])
                     [:id3 [nil {"a" :b}]]
                     [:id4 [["search"] {"q" (silk/? :q {:q "default"})}]]
                     [:id5 [["users"] {"q" (silk/? :q)}]]
                     [:id6 [["test"] {"q" (silk/? (silk/int :q))}]]]))
    (spec/with-all clean-params
                   #(dissoc %
                     :domkm.silk/routes
                     :domkm.silk/url
                     :domkm.silk/pattern
                     :domkm.silk/name))
    (spec/it "matches successfully"
      (spec/should= {:method :get}
                    (@clean-params
                     (silk/match @routes (silk/url {:request-method :get}))))
      (spec/should= {:method :get}
                    (@clean-params (silk/match @routes {:request-method :get})))
      (spec/should= {:baz "baz"}
                    (@clean-params
                     (silk/match @routes
                       (silk/map->URL {:path ["foo" "bar" "baz"]}))))
      (spec/should= {:b "b"}
                    (@clean-params
                     (silk/match @routes (silk/map->URL {:query {"a" "b"}})))))
    (spec/it "matches the first of two overlapping routes"
      (let [by-id [:user [["users" :id]]]
            new-user [:new-user [["users" "new"]]]]
        (spec/should=
         :user
         (silk/arrive (silk/routes [by-id new-user]) "/users/new" ::silk/name))
        (spec/should= :new-user
                      (silk/arrive (silk/routes [new-user by-id])
                                   "/users/new"
                                   ::silk/name))))
    (spec/it "matches unsuccessfully"
      (spec/should-be-nil (silk/match @routes (silk/map->URL {}))))
    (spec/it "unmatches successfully"
      (spec/should= (silk/map->URL {:query {"a" "c"}})
                    (silk/unmatch @routes
                                  {:b "c",
                                   :domkm.silk/name :id3}))
      (spec/should= (silk/map->URL {:path ["test"],
                                    :query {"q" nil}})
                    (silk/unmatch @routes {:domkm.silk/name :id6})))
    (spec/it "unmatches unsuccessfully"
      (spec/should-throw AssertionError (silk/unmatch @routes {})))
    (spec/it "arrives"
      (spec/should= {:baz "baz"}
                    (@clean-params (silk/arrive @routes "/foo/bar/baz")))
      (spec/should= {:b "b"} (@clean-params (silk/arrive @routes "/?a=b")))
      (spec/should= {:q "foo"}
                    (@clean-params (silk/arrive @routes "/search?q=foo")))
      (spec/should= {:q "default"}
                    (@clean-params (silk/arrive @routes "/search")))
      (spec/should= {:q nil} (@clean-params (silk/arrive @routes "/users")))
      (spec/should= {:q "bar"}
                    (@clean-params (silk/arrive @routes "/users?q=bar")))
      (spec/should= {:q nil} (@clean-params (silk/arrive @routes "/test"))))
    (spec/it "departs"
      (spec/should= "/foo/bar/bloop" (silk/depart @routes :id2 {:baz "bloop"}))
      (spec/should= "/?a=bloop" (silk/depart @routes :id3 {:b "bloop"}))
      (spec/should= "/search?q=default" (silk/depart @routes :id4))
      (spec/should= "/search?q=foo" (silk/depart @routes :id4 {:q "foo"}))
      (spec/should= "/users" (silk/depart @routes :id5))
      (spec/should= "/users?q=bar" (silk/depart @routes :id5 {:q "bar"}))
      (spec/should= "/test" (silk/depart @routes :id6)))))

(spec/describe "repeated query parameters"
  (spec/with-all routes
                 (silk/routes [[:tags [["tags"] {"tag" :tags}]]
                               [:items [["items"] {"id" (silk/int :id)}]]]))
  (spec/it "passes strings and vectors through plain captures"
    (spec/should= "clojure" (silk/arrive @routes "/tags?tag=clojure" :tags))
    (let [tags ["clojure" "cljs"]
          url (silk/depart @routes :tags {:tags tags})]
      (spec/should= "/tags?tag=clojure&tag=cljs" url)
      (spec/should= tags (silk/arrive @routes url :tags))))
  (spec/it "matches and generates typed values in order"
    (doseq [[id path] [[2 "/items?id=2"] [[2 7 2] "/items?id=2&id=7&id=2"]]]
      (spec/should= id (silk/arrive @routes path :id))
      (spec/should= path (silk/depart @routes :items {:id id})))
    (spec/should= "/items?id=2" (silk/depart @routes :items {:id [2]})))
  (spec/it "matches large repeated integers and rejects overflow"
    (spec/should= [2 2147483648]
                  (silk/arrive @routes "/items?id=2&id=2147483648" :id))
    (spec/should-be-nil (silk/arrive @routes
                                     "/items?id=2&id=9223372036854775808")))
  (spec/it "requires at least one value and validates every value"
    (spec/should-be-nil (silk/arrive @routes "/items"))
    (spec/should-be-nil (silk/arrive @routes "/items?id=2&id=nope"))
    (doseq [id [nil [] [2 "7"]]]
      (spec/should-throw AssertionError (silk/depart @routes :items {:id id}))))
  (spec/it "preserves optional query defaults and accepts supplied repeats"
    (let [routes (silk/routes
                  [[:items [["items"] {"id" (silk/? (silk/int :id))}]]
                   [:default
                    [["default"] {"id" (silk/? (silk/int :id) {:id 42})}]]])]
      (spec/should= :items (silk/arrive routes "/items" ::silk/name))
      (spec/should= "/items" (silk/depart routes :items))
      (spec/should= 42 (silk/arrive routes "/default" :id))
      (spec/should= "/default?id=42" (silk/depart routes :default))
      (spec/should= [2 7] (silk/arrive routes "/default?id=2&id=7" :id))
      (spec/should= "/default?id=2&id=7"
                    (silk/depart routes :default {:id [2 7]}))))
  (spec/it "prepares typed captures in whole-query optional maps"
    (let [query (silk/? {"id" (silk/? (silk/int :id) {:id 1})} {:id 42})]
      (doseq [[pattern default-url] [[query "/?id=42"]
                                     [(silk/? query {:id 43}) "/?id=43"]]]
        (let [routes (silk/routes [[:root [[] pattern]]])]
          (doseq [[id url] [[2 "/?id=2"] [[2 7] "/?id=2&id=7"]]]
            (spec/should= id (silk/arrive routes url :id))
            (spec/should= url (silk/depart routes :root {:id id})))
          (spec/should= default-url (silk/depart routes :root))
          (spec/should= 1 (silk/arrive routes "/" :id))
          (spec/should= 1 (silk/arrive routes {:path []} :id))))))
  (spec/it "uses a vector-valued optional key default as one value"
    (let [pattern (silk/regex :q
                              #"[a-z]+"
                              {:deserialize vector,
                               :serialize first,
                               :validate vector?})
          defaults {:q ["abc"]}
          routes (silk/routes [[:root [[] {"q" (silk/? pattern defaults)}]]])]
      (spec/should= "/?q=abc" (silk/depart routes :root))
      (let [q (silk/arrive routes "/" :q)]
        (spec/should= ["abc"] q)
        (spec/should= "/?q=abc" (silk/depart routes :root {:q q})))
      (spec/should= "/?q=def&q=ghi"
                    (silk/depart routes :root {:q [["def"] ["ghi"]]}))
      (spec/should-throw AssertionError (silk/depart routes :root defaults))))
  (spec/it "keeps whole-query vector defaults and their codec metadata"
    (let [codec-meta {:codec true}
          pattern (silk/regex
                   :q
                   #"[a-z]+"
                   {:deserialize #(with-meta [%] codec-meta),
                    :serialize #(when (= codec-meta (meta %)) (first %)),
                    :validate #(and (vector? %) (= codec-meta (meta %)))})
          default (with-meta ["abc"] codec-meta)
          routes (silk/routes [[:root [[] (silk/? {"q" pattern} {:q default})]]
                               [:shared
                                [[]
                                 (silk/? {"q" pattern,
                                          "r" (silk/cat pattern)}
                                         {:q default})]]])]
      (spec/should= "/?q=abc" (silk/depart routes :root))
      (spec/should= "/?q=abc&r=abc" (silk/depart routes :shared))
      (spec/should-be-nil (silk/arrive routes {:path []}))
      (spec/should= "/?q=abc&q=abc"
                    (silk/depart routes :root {:q [default default]}))))
  (spec/it "round-trips regex captures with and without groups"
    (doseq [[pattern x path]
            [[(silk/regex :q #"[a-z]+") ["abc" "def"] "/?q=abc&q=def"]
             [(silk/regex :q #"([a-z]+)") ["abc" "abc"] "/?q=abc"]
             [(silk/regex :q #"(abc)(def)?") ["abc" "abc" nil] "/?q=abc"]
             [(silk/regex :q #"([a-z]+)") [["abc" "abc"] ["def" "def"]]
              "/?q=abc&q=def"]]]
      (let [routes (silk/routes [[:root [[] {"q" pattern}]]])]
        (spec/should= x (silk/arrive routes path :q))
        (spec/should= path (silk/depart routes :root {:q x})))))
  (spec/it "uses whole-string regex validation for generation by default"
    (let [routes (silk/routes [[:root [[] {"q" (silk/regex :q #"[a-z]+")}]]])]
      (spec/should-be-nil (silk/arrive routes "/?q=def123"))
      (spec/should-be-nil (silk/arrive routes "/?q=abc&q=def123"))
      (doseq [q ["def123" ["abc" "def123"]]]
        (spec/should-throw AssertionError (silk/depart routes :root {:q q})))))
  (spec/it "uses custom regex hooks on each query value"
    (let [decoded (atom [])
          pattern (silk/regex :q
                              #"(?:[a-z]+)"
                              {:deserialize (fn [s]
                                              (swap! decoded conj s)
                                              (.toUpperCase ^String s)),
                               :serialize #(.toLowerCase ^String %),
                               :validate #(.startsWith ^String % "A")})
          routes (silk/routes [[:root [[] {"q" pattern}]]])]
      (doseq [[q url] [["ABC" "/?q=abc"] [["ABC" "ADE"] "/?q=abc&q=ade"]]]
        (spec/should= q (silk/arrive routes url :q))
        (spec/should= url (silk/depart routes :root {:q q})))
      (spec/should= ["abc" "abc" "ade"] @decoded)
      (spec/should-throw AssertionError
                         (silk/depart routes :root {:q ["ABC" "DEF"]}))))
  (spec/it "lets a custom validator override default generation validation"
    (let [pattern (silk/regex :q #"[a-z]+" {:validate string?})
          routes (silk/routes [[:root [[] {"q" pattern}]]])]
      (spec/should= "/?q=def123" (silk/depart routes :root {:q "def123"}))))
  (spec/it "uses custom grouped regex hooks on one capture vector at a time"
    (let [pattern (silk/regex :q
                              #"([a-z]+)(?:-([a-z]+))?"
                              {:serialize (fn [[s]] (.toLowerCase ^String s)),
                               :validate vector?})
          routes (silk/routes [[:root [[] {"q" pattern}]]])
          single ["abc" "abc" nil]
          repeated [single ["def-ghi" "def" "ghi"]]]
      (doseq [[q url] [[single "/?q=abc"] [repeated "/?q=abc&q=def-ghi"]]]
        (spec/should= q (silk/arrive routes url :q))
        (spec/should= url (silk/depart routes :root {:q q})))
      (spec/should-throw AssertionError
                         (silk/depart routes :root {:q [single "def"]}))))
  (spec/it "round-trips grouped regex matches deserialized to strings"
    (let [pattern (silk/regex :q
                              #"([a-z]+)"
                              {:deserialize first,
                               :serialize identity,
                               :validate #(.startsWith ^String % "a")})
          routes (silk/routes [[:root [[] {"q" pattern}]]])]
      (doseq [[value path] [["abc" "/?q=abc"] [["abc" "ade"] "/?q=abc&q=ade"]]]
        (let [q (silk/arrive routes path :q)]
          (spec/should= value q)
          (spec/should= path (silk/depart routes :root {:q q}))))))
  (spec/it "round-trips ungrouped regex matches deserialized to vectors"
    (let [pattern (silk/regex :q
                              #"[a-z]+-[a-z]+"
                              {:deserialize #(with-meta (str/split % #"-" 2)
                                                        {:codec true}),
                               :serialize #(str/join "-" %),
                               :validate #(and (vector? %)
                                               (= 2 (count %))
                                               (every? string? %)
                                               (= {:codec true} (meta %)))})
          routes (silk/routes [[:root [[] {"q" pattern}]]])]
      (doseq [[value path] [[["abc" "ade"] "/?q=abc-ade"]
                            [[["abc" "ade"] ["foo" "bar"]]
                             "/?q=abc-ade&q=foo-bar"]]]
        (let [q (silk/arrive routes path :q)]
          (spec/should= value q)
          (spec/should= path (silk/depart routes :root {:q q}))))))
  (spec/it "distinguishes equal vectors from one query value and repeats"
    (let [pattern
          (silk/regex
           :q
           #"[a-z]+(?:-[a-z]+)?"
           {:deserialize #(if (str/includes? % "-") (str/split % #"-" 2) %),
            :serialize #(if (vector? %) (str/join "-" %) %),
            :validate #(or (string? %) (and (vector? %) (every? string? %)))})
          routes (silk/routes [[:root [[] {"q" pattern}]]])
          single (silk/arrive routes "/?q=abc-ade" :q)
          repeated (silk/arrive routes "/?q=abc&q=ade" :q)]
      (spec/should= ["abc" "ade"] single)
      (spec/should= single repeated)
      (spec/should= "/?q=abc-ade" (silk/depart routes :root {:q single}))
      (spec/should= "/?q=abc&q=ade" (silk/depart routes :root {:q repeated}))
      (spec/should= "/?q=abc&q=ade"
                    (silk/depart routes :root {:q ["abc" "ade"]}))))
  (spec/it "accepts a single grouped capture with caller-supplied group slots"
    (let [pattern (silk/regex :q #"foo(.*)")
          routes (silk/routes [[:root [[] {"q" pattern}]]])]
      (spec/should= "/?q=foobar"
                    (silk/depart routes :root {:q ["foobar" "foo"]}))))
  (spec/it "exposes the original route pattern"
    (let [pattern {:path [],
                   :query {"id" (silk/int :id)}}
          route (silk/route [:root pattern])
          routes (silk/routes [route])]
      (spec/should= pattern (val route))
      (spec/should= pattern (silk/arrive routes "/?id=2&id=7" ::silk/pattern))))
  (spec/it "preserves literal query patterns"
    (let [routes (silk/routes [[:root [[] {"q" "fixed"}]]])]
      (spec/should= :root (silk/arrive routes "/?q=fixed" ::silk/name))
      (spec/should= "/?q=fixed" (silk/depart routes :root))
      (spec/should-be-nil (silk/arrive routes "/?q=fixed&q=fixed"))))
  (spec/it "preserves positional vector query patterns"
    (let [routes (silk/routes [[:root [[] {"q" ["fixed" :q]}]]])]
      (spec/should= "abc" (silk/arrive routes "/?q=fixed&q=abc" :q))
      (spec/should= "/?q=fixed&q=abc" (silk/depart routes :root {:q "abc"}))))
  (spec/it "does not lift path or standalone captures"
    (let [ptrn (silk/int :id)
          routes (silk/routes [[:item [["items" ptrn]]]])]
      (spec/should-be-nil (silk/match ptrn ["2" "7"]))
      (spec/should-be-nil (silk/arrive routes {:path ["items" ["2" "7"]]}))
      (spec/should-throw AssertionError
                         (silk/depart routes :item {:id [2 7]}))))
  (spec/it "keeps custom standalone vector captures as single values"
    (let [pattern (silk/regex :q
                              #"[a-z]+"
                              {:deserialize vector,
                               :serialize first,
                               :validate vector?})]
      (spec/should= {:q ["abc"]} (silk/match pattern "abc"))
      (spec/should= "abc" (silk/unmatch pattern {:q ["abc"]})))))
