(ns domkm.silk.uri-spec
  (:require [domkm.silk.uri :as uri]
            [speclj.core :as spec #+cljs :include-macros #+cljs true]))

(spec/describe
 "URI encoding/decoding"

 (let [de-string " !'()+~"
       en-string "%20%21%27%28%29%2B%7E"
       encoded-string (uri/encode de-string)
       decoded-string (uri/decode encoded-string)]

   (spec/it
    "encodes"
    (spec/should= encoded-string en-string))

   (spec/it
    "decodes"
    (spec/should= decoded-string de-string))))

(spec/describe
 "URI path encoding/decoding"

 (let [path-str "/foo/bar"
       path-vec ["foo" "bar"]]

   (spec/it
    "encodes"
    (spec/should= (uri/encode-path path-vec) path-str))

   (spec/it
    "decode"
    (spec/should= (uri/decode-path path-str) path-vec))))

(spec/describe
 "URI query encoding/decoding"

 (let [query-str "foo=bar&baz=qux"
       query-map {"foo" "bar", "baz" "qux"}]

   (spec/it
    "encodes"
    (spec/should= (uri/encode-query query-map) query-str))

   (spec/it
    "decodes"
    (spec/should= (uri/decode-query query-str) query-map))))
