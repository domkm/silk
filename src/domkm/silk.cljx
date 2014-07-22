(ns domkm.silk
  (:require [clojure.string :as str])
  #+clj (:import [clojure.lang Keyword PersistentArrayMap PersistentHashMap PersistentVector]))


;;;; URL ;;;;

(defn ^String encode
  [^String s]
  (-> s
      #+clj (java.net.URLEncoder/encode "UTF-8")
      #+cljs js/encodeURIComponent
      #+clj (str/replace #"\+" "%20")
      #+cljs (str/replace #"[!'()]" js/escape)
      #+cljs (str/replace #"~" "%7E")))

(defn ^String decode
  [s]
  (-> s
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

(defrecord URL [scheme user host port path query fragment] ; TODO: scheme, user, host, port, fragment
  Object
  (toString
   [this]
   (str (encode-path path)
        (when query
          (str "?" (encode-query query))))))


  ;;;; Pattern ;;;;

(defprotocol Pattern
  (-match [this that])
  (-unmatch [this params]))

(defn pattern? [x]
  (satisfies? Pattern x))

(defn match [pattern x]
  (-match pattern x))

(defn unmatch [pattern params]
  (-unmatch pattern params))

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

(extend-protocol Pattern

  nil
  (-match [_ _]
          {})
  (-unmatch [_ _]
            nil)

  #+clj String
  #+cljs string
  (-match [this that]
          (when (= this that)
            {}))
  (-unmatch [this _]
            this)

  Keyword
  (-match [this that]
          (when-not (nil? that)
            {this that}))
  (-unmatch [this params]
            (if (contains? params this)
              (get params this)
              (->> {:parameters params
                    :key this}
                   (ex-info "missing parameter key")
                   throw)))

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


;;;; Route ;;;;

(defrecord Route [id pattern]
  Pattern
  (-match [this url]
          (when-let [params (match pattern url)]
            (assoc params ::route this)))
  (-unmatch [this params]
            (->> ::route
                 (dissoc params)
                 (unmatch pattern)
                 map->URL)))

(defn route? [x]
  (instance? Route x))

(defn route
  ([rte]
   (if (route? rte)
     rte
     (apply route rte)))
  ([id pattern]
   (->Route id
            (cond
             (map? pattern) pattern
             (vector? pattern) (let [[path query etc] pattern]
                                 (->> (assoc etc :path path :query query)
                                      (remove (fn [[k v]] (nil? v)))
                                      (into {})))))))


;;;; Routes ;;;;

(deftype Routes [routes ids]
  Pattern
  (-match [this url]
          (some (fn [route]
                  (when-let [params (match route url)]
                    (assoc params ::routes this ::url url)))
                routes))
  (-unmatch [this {{id :id} ::route :as params}]
            (if-let [route (get ids id)]
              (->> (dissoc params ::routes ::url)
                   (unmatch route))
              (->> {:routes this
                    :parameters params}
                   (ex-info "route not found")
                   throw))))

(defn routes? [x]
  (instance? Routes x))

(defn routes [rtes]
  (let [rtes (-> (fn [memo rte]
                   (if (routes? rte)
                     (reduce conj! memo (.routes rte))
                     (conj! memo (apply route rte))))
                 (reduce (transient []) rtes)
                 persistent!)
        ids (-> (fn [memo {id :id :as rte}]
                  (if (nil? id)
                    memo
                    (assoc! memo id rte)))
                (reduce (transient {}) rtes)
                persistent!)]
    (->Routes rtes ids)))
