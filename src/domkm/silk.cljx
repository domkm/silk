(ns domkm.silk
  (:refer-clojure :exclude [int cat])
  (:require [clojure.string :as str]
            #+clj [clojure.core :as core]
            #+cljs [cljs.core :as core])
  #+clj
  (:import [clojure.lang Keyword PersistentArrayMap PersistentHashMap PersistentVector]
           [java.util UUID]))


;;;; URL ;;;;

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
  (if-not (str/blank? s)
    (->> (str/split s #"[&;]")
         (reduce (fn [q pair]
                   (let [[k v] (map decode (str/split pair #"="))]
                     (assoc! q k v)))
                 (transient {}))
         persistent!)
    {}))

(defrecord URL [scheme user host port path query fragment] ; TODO: scheme, user, host, port, fragment
  Object
  (toString
   [this]
   (str (encode-path path)
        (when query
          (str "?" (encode-query query))))))

(defn url? [x]
  (instance? URL x))

(defn url [x]
  (cond
   (url? x) x
   (string? x) (let [[p q] (str/split x #"\?")]
                 (url {:path (-> p str decode-path)
                       :query (-> q str decode-query)}))
   (map? x) (map->URL x)))

(defn url-pattern [x]
  (cond
   (map? x) x
   (vector? x) (let [[path query etc] x]
                 (->> (assoc etc :path path :query query)
                      (remove (fn [[k v]] (nil? v)))
                      (into {})))))


;;;; Pattern ;;;;

(defprotocol Pattern
  (-match [this that])
  (-unmatch [this params])
  (-match-validator [this])
  (-unmatch-validators [this]))

(defn pattern? [x]
  (satisfies? Pattern x))

(defn match-validator [ptrn]
  {:pre [(pattern? ptrn)]
   :post [(fn? %)]}
  (-match-validator ptrn))

(defn unmatch-validators [ptrn]
  {:pre [(pattern? ptrn)]
   :post [(map? %)
          (every? some? (keys %))
          (every? fn? (vals %))]}
  (-unmatch-validators ptrn))

(defn match-valid? [ptrn x]
  (boolean ((match-validator ptrn) x)))

(defn unmatch-valid? [ptrn params]
  (->> ptrn
       unmatch-validators
       (map (fn [[k v]]
              (v (get params k))))
       (every? identity)))

(defn match [ptrn x]
  {:pre [(pattern? ptrn)]
   :post [(or (nil? %)
              (unmatch-valid? ptrn %))]}
  (when (match-valid? ptrn x)
    (-match ptrn x)))

(defn unmatch [ptrn params]
  {:pre [(unmatch-valid? ptrn params)]
   :post [(match-valid? ptrn %)]}
  (-unmatch ptrn params))


;;;; Native Patterns ;;;;

(extend-type #+clj String #+cljs string
  Pattern
  (-match [this that]
          (when (= this that)
            {}))
  (-unmatch [this _]
            this)
  (-match-validator [_]
                    string?)
  (-unmatch-validators [_]
                       {}))

(extend-type Keyword
  Pattern
  (-match [this that]
          {this that})
  (-unmatch [this params]
            (get params this))
  (-match-validator [_]
                    some?)
  (-unmatch-validators [this]
                       {this some?}))

(defn ^:private match-coll [ks %1s %2s]
  (loop [ks ks
         ret (transient {})]
    (if-some
     [k (first ks)]
     (when-let [m (match (get %1s k) (get %2s k))]
       (recur (rest ks)
              (reduce-kv #(assoc! %1 %2 %3) ret m)))
     (persistent! ret))))

(extend-type PersistentVector
  Pattern
  (-match [this that]
          (when (== (count this)
                    (count that))
            (match-coll (-> this count range) this that)))
  (-unmatch [this params]
            (mapv #(unmatch % params) this))
  (-match-validator [_]
                    vector?)
  (-unmatch-validators [_]
                       {}))

(defn ^:private unmatch-map [ptrn params]
  (loop [kvs (seq ptrn)
         ret (transient {})]
    (if-let [[k v] (first kvs)]
      (recur (rest kvs)
             (assoc! ret k (unmatch v params)))
      (persistent! ret))))

(extend-type PersistentArrayMap
  Pattern
  (-match [this that]
          (match-coll (keys this) this that))
  (-unmatch [this that]
            (unmatch-map this that))
  (-match-validator [_]
                    map?)
  (-unmatch-validators [_]
                       {}))

(extend-type PersistentHashMap
  Pattern
  (-match [this that]
          (match-coll (keys this) this that))
  (-unmatch [this that]
            (unmatch-map this that))
  (-match-validator [_]
                    map?)
  (-unmatch-validators [_]
                       {}))


;;;; Built-In Patterns ;;;;

; TODO: add `clojure.string/re-quote-replacement` to ClojureScript
(let [re-quote-char-map (reduce
                         #(assoc %1 %2 (str "\\" %2))
                         {}
                         "\\.*+|?()[]{}$^")]
  (defn ^:private re-quote-replacement [s]
    (str/escape s re-quote-char-map)))

(defrecord ^:private RegexPattern [param-key regex deserialize serialize validate]
  Pattern
  (-match [_ s]
          (some->> (re-matches regex s)
                   deserialize
                   (hash-map param-key)))
  (-unmatch [_ params]
            (->> param-key
                 (get params)
                 serialize))
  (-match-validator [_]
                    string?)
  (-unmatch-validators [_]
                       {param-key validate}))

(defn regex
  ([k re]
   (regex k re {}))
  ([k re {:keys [deserialize serialize validate]
          :or {deserialize identity
               serialize #(if (vector? %) (nth % 0) %)
               validate #(when-let [s (if (vector? %) (first %) %)]
                           (and (string? s)
                                (re-find re s)))}}]
   {:pre [(some? k)]}
   (map->RegexPattern {:param-key k
                       :regex re
                       :deserialize deserialize
                       :serialize serialize
                       :validate validate})))

(defn bool [k]
  (regex k
         #"true|false"
         {:deserialize #(= "true" %)
          :serialize str
          :validate #+clj #(instance? Boolean %) #+cljs #(identical? js/Boolean (type %))}))

(defn int [k]
  (regex k
         #"\d+"
         {:deserialize #+clj #(Integer/parseInt %) #+cljs #(js/parseInt % 10)
          :serialize str
          :validate integer?}))

(defn uuid [k]
  (regex k
         #"[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}"
         {:deserialize #+clj #(UUID/fromString %) #+cljs ->UUID
          :serialize str
          :validate #(instance? UUID %)}))

(defn cat [& ptrns]
  {:pre [(every? #(match-valid? % "string") ptrns)
         (let [pkeys (mapcat (comp keys unmatch-validators) ptrns)]
           (or (empty? pkeys)
               (apply distinct? pkeys)))]}
  (let [re-str #(list "("
                      (cond
                       (string? %)
                       (re-quote-replacement %),
                       (instance? RegexPattern %)
                       #+clj (-> % :regex str) ; clj (str #"re") => "re"
                       #+cljs (let [s (-> % :regex str)] ; cljs (str #"re") => "/re/"
                                (subs s 1 (-> s .-length dec))),
                       :else ".*")
                      ")")
        re (->> ptrns
                (mapcat re-str)
                str/join
                re-pattern)
        ptrns (vec ptrns)
        validator (apply merge (map unmatch-validators ptrns))]
    (reify
      Pattern
      (-match [_ s]
              (when-let [v (re-find re s)]
                (->> (subvec v 1)
                     (mapv not-empty)
                     (match ptrns))))
      (-unmatch [_ params]
                (str/join (unmatch ptrns params)))
      (-match-validator [_]
                        string?)
      (-unmatch-validators [_]
                           validator))))

(defn ? [ptrn default-params]
  {:pre [(pattern? ptrn)
         (unmatch ptrn default-params)]}
  (reify
    Pattern
    (-match [_ that]
            (if (nil? that)
              default-params
              (match ptrn that)))
    (-unmatch [_ params]
              (unmatch ptrn
                       (merge-with (fn [pval dval]
                                     (if (nil? pval)
                                       dval
                                       pval))
                                   params
                                   default-params)))
    (-match-validator [_]
                      (some-fn nil? (match-validator ptrn)))
    (-unmatch-validators [_]
                         {})))


;;;; Route Pattern ;;;;

(deftype Route [name pattern]
  Pattern
  (-match [this -url]
          (when-let [params (match pattern (url -url))]
            (assoc params ::name name ::pattern pattern)))
  (-unmatch [this params]
            (->> (dissoc params ::name ::pattern)
                 (unmatch pattern)
                 url))
  (-match-validator [_]
                    map?)
  (-unmatch-validators [_]
                       {})
  ; so much for portable code :'(
  #+clj java.util.Map$Entry
  #+clj (getKey [_] name)
  #+clj (getValue [_] pattern)
  #+cljs IMapEntry
  #+cljs (-key [_] name)
  #+cljs (-val [_] pattern))

(defn route? [x]
  (instance? Route x))

(defn route [x]
  (if (route? x)
    x
    (let [[nm ptrn] x]
      (->Route nm (url-pattern ptrn)))))


;;;; Routes Pattern ;;;;

(deftype Routes [routes-seq routes-map]
  Pattern
  (-match [this -url]
          (let [u (url -url)]
            (some (fn [route]
                    (when-let [params (match route u)]
                      (assoc params ::routes this ::url u)))
                  routes-seq)))
  (-unmatch [this {nm ::name :as params}]
            (assert (and (some? nm)
                         (contains? routes-map nm)))
            (unmatch (get routes-map nm) (dissoc params ::routes ::url)))
  (-match-validator [_]
                    map?)
  (-unmatch-validators [_]
                       {}))

(defn routes? [x]
  (instance? Routes x))

(defn routes [rtes]
  (if (routes? rtes)
    rtes
    (let [rtes-seq (mapcat #(if (routes? %)
                              (.-routes-seq %)
                              (-> % route list))
                           rtes)
          rtes-map (-> (fn [memo rte]
                         (let [rte (route rte)
                               k (key rte)]
                           (if (nil? k)
                             memo
                             (assoc! memo k rte))))
                       (reduce (transient {}) rtes-seq)
                       persistent!)]
      (->Routes rtes-seq rtes-map))))

(defn arrive
  ([rtes x]
   (arrive rtes x identity))
  ([rtes x handler]
   (->> x
        url
        (match rtes)
        handler)))

(defn depart
  ([rtes nm]
   (depart rtes nm {} str))
  ([rtes nm params]
   (depart rtes nm params str))
  ([rtes nm params handler]
   (->> (assoc params ::name nm)
        (unmatch rtes)
        handler)))
