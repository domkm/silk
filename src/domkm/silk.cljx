(ns domkm.silk
  (:require [clojure.string :as str]
            [domkm.silk.util :as util])
  #+clj
  (:import [clojure.lang Keyword PersistentArrayMap PersistentHashMap PersistentVector]))


;;;; URL ;;;;

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

(defn encode-query
  "Takes a query map.
  Returns a string of query pairs encoded and joined."
  [query]
  (->> query
       (map (fn [[k v]] (str (encode k) "=" (encode v))))
       (str/join "&")))

(defn decode-query
  "Takes a query string.
  Returns a map of decoded query pairs."
  [^String s]
  (->> (str/split s #"[&;]")
       (reduce (fn [q pair]
                 (let [[k v] (map decode (str/split pair #"="))]
                   (assoc! q k v)))
               (transient {}))
       persistent!))

; user => {:name "name" :password "pw"}
; host may need to have different implementations for IP, domains, etc.
; path and query need tuple and record types.
(defrecord URL [scheme user host port path query fragment] ; TODO: scheme, user, host, port, fragment
  Object
  (toString
   [this]
   (str (when path
          (encode-path path))
        (when query
          (str "?" (encode-query query))))))

(def url-keys [:scheme :user :host :port :path :query :fragment])

(defn url
  "Takes a map of URL parts.
  Returns a URL.
  Will try to use `:path-string` and `:query-string`
  if map does not contain `:path` or `:query` keys respectively."
  [{:keys [path query path-string query-string] :as m}]
  (let [path (or path (when path-string
                        (decode-path path-string)))
        query (or query (when query-string
                          (decode-query query-string)))]
    (map->URL (assoc m :path path :query query))))


  ;;;; Pattern ;;;;

(defprotocol Patternable
  (-match [this that])
  (-unmatch [this params]))

(defn match [pattern x]
  (-match pattern x))

(defn unmatch [pattern params]
  (-unmatch pattern params))

(defn ^:private get-param [params param]
  (if (contains? params param)
    (get params param)
    (-> (->> param
             pr-str
             (str "Missing parameter key: "))
        #+clj Exception.
        #+cljs js/Error.
        throw)))

(defn ^:private match-all [pairs]
  (loop [pairs pairs
         ret (transient {})]
    (if-let [[x y] (first pairs)]
      (when-let [mch (match x y)]
        (recur (rest pairs)
               (reduce-kv assoc! ret mch)))
      (persistent! ret))))

(defn ^:private match-all-map [patterns-map url-map]
  (->> patterns-map
       (map (fn [[k v]]
              [v (get url-map k)]))
       match-all))

(defn ^:private unmatch-all-map [patterns-map params]
  (->> patterns-map
       (reduce-kv (fn [m k pat] (assoc! m k (unmatch pat params)))
                  (transient {}))
       persistent!))

(extend-protocol Patternable

  #+clj String
  #+cljs string
  (-match [this that]
          (when (= this that)
            {}))
  (-unmatch [this _]
            this)

  Keyword
  (-match [this that]
          (when (string? that)
            {this that}))
  (-unmatch [this params]
            (get-param params this))

  PersistentVector
  (-match [this that]
          (when (== (count this)
                    (count that))
            (->> (interleave this that)
                 (partition 2)
                 match-all)))
  (-unmatch [this params]
            (mapv #(unmatch % params) this))

  PersistentArrayMap
  (-match [this that]
          (match-all-map this that))
  (-unmatch [this that]
            (unmatch-all-map this that))

  PersistentHashMap
  (-match [this that]
          (match-all-map this that))
  (-unmatch [this that]
            (unmatch-all-map this that)))

(comment "TODO: built in leaf pattern constructors"
  (defn regex [k re]) ; match the regex
  (defn uuid [k]) ; match uuid and transform to/from
  (defn integer [k]) ; match integer and transform to/from
  (defn boolean [k]) ; match boolean and transform to/from
  (defn keyword [k] [k1 k2]) ; match keyword and transform to/from
  (defn composite [patternables]) ; match all patterns
  (defn default [k patternable else]) ; if matchable is nil then return else. is this useful outside of query?
  (defn alternatives) ; should this need a key?
  )
