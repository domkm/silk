(ns domkm.silk.url
  (:require [clojure.string :as str]))


(defn ^String encode [^String s]
  (-> s
      #+clj (java.net.URLEncoder/encode "UTF-8")
      #+clj (str/replace #"\+" "%20")
      #+cljs js/encodeURIComponent
      #+cljs (str/replace #"[!'()]" js/escape)
      #+cljs (str/replace #"~" "%7E")))

(defn ^String decode [s]
  #+clj (java.net.URLDecoder/decode s "UTF-8")
  #+cljs (js/decodeURIComponent s))

(defn encode-host [v]
  (str/join "." (reverse v)))

(defn decode-host [v]
  (-> v (str/split #"\.") reverse vec))

(defn encode-path
  "Takes a path seqable.
  Returns a string of path segments encoded, joined with `/`, and prepended with `/`."
  [path]
  {:pre [(every? not-empty path)]}
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

(defn encode-query
  "Takes a query map.
  Returns a string of query pairs encoded and joined."
  [query]
  (str/join "&" (for [[k v] (sort query)]
                  (str (encode k) "=" (encode v)))))

(defn decode-query
  "Takes a query string.
  Returns a map of decoded query pairs."
  [^String s]
  (when-not (str/blank? s)
    (->> (str/split s #"[&;]")
         (reduce (fn [q pair]
                   (let [[k v] (map decode (str/split pair #"="))]
                     (assoc! q k v)))
                 (transient {}))
         persistent!)))

; TODO: handle absolute URLs and other options
(defn url-str
  ([url] (url-str url nil))
  ([{:keys [scheme user host port path query fragment]} opts]
   (str (encode-path path)
        (when query
          (str "?" (encode-query query))))))

(defrecord URL [scheme user host port path query fragment]
  Object
  (toString [this] (url-str this)))

(defn url? [x]
  (instance? URL x))

; TODO: handle absolute URLs
(defn parse-url [s]
  (let [[p q] (str/split s #"\?")]
    (map->URL {:path (decode-path p)
               :query (decode-query q)})))

(defn url [x]
  (cond
   (url? x) x
   (map? x) (map->URL x)
   (string? x) (parse-url x)))

(defn ^:private url-vector->seq [v]
  (mapcat #(cons :url/part %) v))

(defn ^:private url-map->seq [m]
  (mapcat (fn [[k v]]
            (->> v  (cons k) (cons :url/part)))
          (sort m)))

(defn url->seq [{:keys [request-method scheme host path query]}]
  (flatten [request-method
            scheme
            :url/host
            (url-vector->seq host)
            :url/path
            (url-vector->seq path)
            :url/query
            (url-map->seq query)]))
