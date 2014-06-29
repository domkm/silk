(ns domkm.silk.uri
  (:require [clojure.string :as str]))

(defn ^String encode
  [^String s]
  (some-> s
          str
          #+clj (java.net.URLEncoder/encode "UTF-8")
          #+cljs js/encodeURIComponent
          #+clj (.replace "+" "%20")
          #+cljs (.replace #"[!'()]" js/escape)
          #+cljs (.replace "~" "%7E")
          ))

(defn ^String decode
  [^String s]
  (some-> s
          str
          #+clj (java.net.URLDecoder/decode "UTF-8")
          #+cljs js/decodeURIComponent))

(defn encode-path
  "Takes a path seqable.
  Returns a string of path segments encoded, joined with `/`, and prepended with `/`."
  [path]
  (->> path
       (map encode)
       (str/join "/")
       (str "/")))

(defn decode-path
  "Takes a path string.
  Returns a vector of decoded path segments."
  [^String s]
  (->> (str/split s #"/")
       (remove str/blank?)
       (mapv decode)))
