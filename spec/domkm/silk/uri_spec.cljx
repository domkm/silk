(ns domkm.silk.uri-spec
  (:require [domkm.silk.uri :as uri]
            [speclj.core :as spec #+cljs :include-macros #+cljs true]))

(spec/describe
 "URI encoding"

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
