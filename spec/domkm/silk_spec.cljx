(ns domkm.silk-spec
  (:require [domkm.silk :as silk]
            [speclj.core :as spec #+cljs :include-macros #+cljs true]
            [speclj.platform :refer [exception]]))

(spec/describe
 "url encoding/decoding"

 (let [de-string " !'()+~"
       en-string "%20%21%27%28%29%2B%7E"
       encoded-string (silk/encode de-string)
       decoded-string (silk/decode encoded-string)]

   (spec/it
    "encodes"
    (spec/should= encoded-string en-string))

   (spec/it
    "decodes"
    (spec/should= decoded-string de-string))))

(spec/describe
 "url path encoding/decoding"

 (let [path-str "/foo/bar"
       path-vec ["foo" "bar"]]

   (spec/it
    "encodes"
    (spec/should= (silk/encode-path path-vec) path-str))

   (spec/it
    "decode"
    (spec/should= (silk/decode-path path-str) path-vec))))

(spec/describe
 "url query encoding/decoding"

 (let [query-str "foo=bar&baz=qux"
       query-map {"foo" "bar", "baz" "qux"}]

   (spec/it
    "encodes"
    (spec/should= (silk/encode-query query-map) query-str))

   (spec/it
    "decodes"
    (spec/should= (silk/decode-query query-str) query-map))))


