(ns domkm.silk
  (:refer-clojure :exclude [boolean])
  (:require [#+clj clojure.core #+cljs cljs.core :as clj]
            [clojure.string :as str])
  #+clj (:import [clojure.lang Keyword PersistentArrayMap PersistentHashMap PersistentVector]
                 [java.util UUID]))


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


;;;; Native Patterns ;;;;

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


;;;; Extra Leaf Node Patterns ;;;;

(defn regex [k re]
  (reify
    Pattern
    (-match [_ s]
            (when-let [param (re-find re s)]
              {k param}))
    (-unmatch [_ params]
              (let [s (-unmatch k params)]
                (if (re-find re s)
                  s
                  (->> {:parameters params
                        :parameter s}
                       (ex-info (str "parameter does not match regex: " re))
                       throw))))))

(defn integer [k]
  (reify
    Pattern
    (-match [_ s]
            (when-let [param (re-find #"^\d+$" s)]
              #+clj {k (Integer/parseInt param)}
              #+cljs {k (js/parseInt param 10)}))
    (-unmatch [_ params]
              (let [i (-unmatch k params)]
                (if (integer? i)
                  (str i)
                  (->> {:parameters params
                        :parameter i}
                       (ex-info "parameter is not an integer")
                       throw))))))

(defn boolean [k]
  (reify
    Pattern
    (-match [_ s]
            (when-let [param (re-find #"^true$|^false$" s)]
              {k (= param "true")}))
    (-unmatch [_ params]
              (let [b (-unmatch k params)]
                (if (or (true? b)
                        (false? b))
                  (str b)
                  (->> {:parameters params
                        :parameter b}
                       (ex-info "parameter is not a boolean")
                       throw))))))

(defn uuid [k]
  (reify
    Pattern
    (-match [_ s]
            (when-let [param (re-find #"^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$" s)]
              #+clj {k (UUID/fromString s)}
              #+cljs {k (->UUID s)}))
    (-unmatch [_ params]
              (when-let [uuid (-unmatch k params)]
                (if (instance? UUID uuid)
                  (str uuid)
                  (->> {:parameters params
                        :parameter uuid}
                       (ex-info "parameter is not a UUID")
                       throw))))))

(defn composite [patterns]
  (let [re (->> patterns
                (map #(str "(" (if (string? %) % ".+") ")"))
                str/join
                re-pattern)]
    (reify Pattern
      (-match [_ s]
              (when-let [m (re-find re s)]
                (->> (rest m)
                     (interleave patterns)
                     (partition 2)
                     match-all)))
      (-unmatch [_ params]
                (str/join (map #(unmatch % params) patterns))))))

(comment "TODO"
  (defn default [k pattern else]) ; match pattern or return else
  (defn alternative [k patterns]) ; match/unmatch any of patterns
  )


;;;; Route Pattern ;;;;

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


;;;; Routes Pattern ;;;;

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
