(ns domkm.silk
  (:refer-clojure :exclude [int cat #?(:cljs uuid)])
  (:require [clojure.string :as str])
  #?(:clj (:import [clojure.lang Keyword PersistentArrayMap PersistentHashMap
                    PersistentVector]
                   [java.util UUID])))


;;;; URL ;;;;

(defn ^String encode
  [^String s]
  (-> s
      #?@(:clj [(java.net.URLEncoder/encode "UTF-8") (str/replace #"\+" "%20")]
          :cljs [js/encodeURIComponent (str/replace #"[!'()]" js/escape)
                 (str/replace #"~" "%7E")])))

(defn ^String decode
  [s]
  #?(:clj (java.net.URLDecoder/decode s "UTF-8")
     :cljs (js/decodeURIComponent s)))

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
  "Encodes a query map. A vector value emits one pair per element, in order."
  [query]
  (str/join "&"
            (for [[k v] (sort query)
                  :let [k (encode k)]
                  value (if (vector? v) v [v])]
              (str k "=" (encode value)))))

(defn decode-query
  "Decodes a query string. Repeated keys have vector values in input order."
  [^String s]
  (if-not (str/blank? s)
    (->> (str/split s #"[&;]")
         (filter seq)
         (reduce (fn [q pair]
                   (let [[k v] (str/split pair #"=" 2)
                         k (decode k)
                         v (decode (or v ""))
                         prev (get q k)]
                     (assoc! q
                             k
                             (cond (nil? prev) v
                                   (vector? prev) (conj prev v)
                                   :else [prev v]))))
                 (transient {}))
         persistent!)
    {}))

(defrecord URL [scheme user host port path query fragment] ; TODO: scheme, user, host, port, fragment
  Object
    (toString [this]
      (str (encode-path path)
           (let [query (encode-query (remove (comp nil? val) query))]
             (when (seq query) (str "?" query))))))

(defn url? [x] (instance? URL x))

(defn url
  [x]
  (cond (url? x) x
        (string? x) (let [[p q] (str/split x #"\?" 2)]
                      (url {:path (-> p
                                      str
                                      decode-path),
                            :query (-> q
                                       str
                                       decode-query)}))
        (map? x) (map->URL x)))

(defn url-pattern
  [x]
  (cond (map? x) x
        (vector? x) (let [[path query etc] x]
                      (->> (assoc etc :path path :query query)
                           (remove (fn [[k v]] (nil? v)))
                           (into {})))))


;;;; Pattern ;;;;

(defprotocol Pattern
  (-match [this that])
  (-unmatch [this params])
  (-match-validator [this])
  (-unmatch-validators [this])
  (-create-default [this v]))

(defn pattern? [x] (satisfies? Pattern x))

(defn- optional-value-is-allowed
  [validator]
  (fn [x] (if (= x ::optional-key-has-no-value) true (validator x))))

(defn match-validator
  [ptrn]
  {:post [(fn? %)],
   :pre [(pattern? ptrn)]}
  (optional-value-is-allowed (-match-validator ptrn)))

(defn unmatch-validators
  [ptrn]
  {:post [(map? %) (every? some? (keys %)) (every? fn? (vals %))],
   :pre [(pattern? ptrn)]}
  (-unmatch-validators ptrn))

(defn match-valid? [ptrn x] (boolean ((match-validator ptrn) x)))

(defn unmatch-valid?
  [ptrn params]
  (->> ptrn
       unmatch-validators
       (map (fn [[k v]] (v (get params k))))
       (every? identity)))

(defn match
  [ptrn x]
  {:post [(or (nil? %) (unmatch-valid? ptrn %))],
   :pre [(pattern? ptrn)]}
  (when (match-valid? ptrn x) (-match ptrn x)))

(defn unmatch
  [ptrn params]
  {:post [(match-valid? ptrn %)],
   :pre [(unmatch-valid? ptrn params)]}
  (-unmatch ptrn params))

(defn create-default
  ([ptrn] (create-default ptrn nil))
  ([ptrn v] (-create-default ptrn v)))


;;;; Native Patterns ;;;;

(extend-type #?(:clj String
                :cljs string)
 Pattern
   (-match [this that] (when (= this that) {}))
   (-unmatch [this _] this)
   (-match-validator [_] string?)
   (-unmatch-validators [_] {}))

(extend-type Keyword
 Pattern
   (-match [this that] {this that})
   (-unmatch [this params] (get params this))
   (-match-validator [_] some?)
   (-unmatch-validators [this] {this some?})
   (-create-default [this v] {this v}))

(defn ^:private match-coll
  [ks %1s %2s]
  (loop [ks ks
         ret (transient {})]
    (if-some [k (first ks)]
      (when-let [m (match (get %1s k) (get %2s k))]
        (recur (rest ks) (reduce-kv #(assoc! %1 %2 %3) ret m)))
      (persistent! ret))))

(extend-type PersistentVector
 Pattern
   (-match [this that]
     (when (== (count this) (count that))
       (match-coll (-> this
                       count
                       range)
                   this
                   that)))
   (-unmatch [this params] (mapv #(unmatch % params) this))
   (-match-validator [_] vector?)
   (-unmatch-validators [_] {}))

(defn ^:private unmatch-map
  [ptrn params]
  (persistent! (reduce-kv (fn [acc k v] (assoc! acc k (unmatch v params)))
                          (transient {})
                          ptrn)))

(extend-type PersistentArrayMap
 Pattern
   (-match [this that] (match-coll (keys this) this that))
   (-unmatch [this that] (unmatch-map this that))
   (-match-validator [_] map?)
   (-unmatch-validators [_] {}))

(extend-type PersistentHashMap
 Pattern
   (-match [this that] (match-coll (keys this) this that))
   (-unmatch [this that] (unmatch-map this that))
   (-match-validator [_] map?)
   (-unmatch-validators [_] {}))


;;;; Built-In Patterns ;;;;

; TODO: add `clojure.string/re-quote-replacement` to ClojureScript
(let [re-quote-char-map
      (reduce #(assoc %1 %2 (str "\\" %2)) {} "\\.*+|?()[]{}$^")]
  (defn ^:private re-quote-replacement [s] (str/escape s re-quote-char-map)))

(defrecord ^:private RegexPattern [param-key regex deserialize serialize
                                   validate]
  Pattern
    (-match [_ s]
      (some->> (re-matches regex s)
               deserialize
               (hash-map param-key)))
    (-unmatch [_ params]
      (let [v (get params param-key)]
        (if (= v ::optional-key-has-no-value) v (serialize v))))
    (-match-validator [_] string?)
    (-unmatch-validators [_] {param-key (optional-value-is-allowed validate)})
    (-create-default [_ v] (create-default param-key v)))

(defn regex
  ([k re] (regex k re {}))
  ([k re
    {:keys [deserialize serialize validate],
     :or {deserialize identity,
          serialize #(if (vector? %) (nth % 0) %),
          validate #(when-let [s (if (vector? %) (first %) %)]
                      (and (string? s) (re-matches re s)))}}]
   {:pre [(some? k)]}
   (map->RegexPattern {:deserialize deserialize,
                       :param-key k,
                       :regex re,
                       :serialize serialize,
                       :validate validate})))

(defn bool
  [k]
  (regex k
         #"true|false"
         {:deserialize #(= "true" %),
          :serialize str,
          :validate #?(:clj #(instance? Boolean %)
                       :cljs #(identical? js/Boolean (type %)))}))

(defn int
  [k]
  (regex k
         #"\d+"
         {:deserialize #?(:clj #(Integer/parseInt %)
                          :cljs #(js/parseInt % 10)),
          :serialize str,
          :validate integer?}))

(defn uuid
  [k]
  (regex k
         #"[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}"
         {:deserialize #?(:clj #(UUID/fromString %)
                          :cljs ->UUID),
          :serialize str,
          :validate #(instance? UUID %)}))

(defn cat
  [& ptrns]
  {:pre [(every? #(match-valid? % "string") ptrns)
         (let [pkeys (mapcat (comp keys unmatch-validators) ptrns)]
           (or (empty? pkeys) (apply distinct? pkeys)))]}
  (let [re-str #(list "("
                      (cond (string? %) (re-quote-replacement %)
                            (instance? RegexPattern %) #?(:clj (-> %
                                                                   :regex
                                                                   str) ; clj (str #"re") => "re"
                                                          :cljs
                                                            (let [s (-> %
                                                                        :regex
                                                                        str)] ; cljs (str #"re") => "/re/"
                                                              (subs s
                                                                    1
                                                                    (-> s
                                                                        .-length
                                                                        dec))))
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
       (-unmatch [_ params] (str/join (unmatch ptrns params)))
       (-match-validator [_] string?)
       (-unmatch-validators [_] validator))))

(deftype ^:private OptionalPattern [ptrn default-params]
  Pattern
    (-match [_ that]
      (if (nil? that)
        (or default-params (create-default ptrn))
        (match ptrn that)))
    (-unmatch [_ params]
      (let [r (unmatch ptrn
                       (merge-with (fn [pval dval] (if (nil? pval) dval pval))
                                   params
                                   (or default-params
                                       (create-default
                                        ptrn
                                        ::optional-key-has-no-value))))]
        (if-not (= ::optional-key-has-no-value r) r)))
    (-match-validator [_] (some-fn nil? (match-validator ptrn)))
    (-unmatch-validators [_] {}))

(defn ?
  [ptrn & [default-params]]
  {:pre [(pattern? ptrn)
         (or (not default-params) (unmatch ptrn default-params))]}
  (->OptionalPattern ptrn default-params))


;;;; Route Pattern ;;;;

(defn- repeated-values? [valid x] (and (vector? x) (seq x) (every? valid x)))

(defn- regex-has-groups?
  [re]
  #?(:clj (pos? (.groupCount (.matcher ^java.util.regex.Pattern re "")))
     :cljs (let [probe (js/RegExp. (str "|" (.-source re)) (.-flags re))]
             ;; The empty alternative matches without using the capture groups.
             (< 1 (.-length (.exec probe ""))))))

(defn- query-regex-pattern
  [{:as ptrn,
    :keys [param-key regex]}]
  (let [grouped? (regex-has-groups? regex)]
    (reify
     Pattern
       (-match [_ x]
         (if (vector? x)
           (when-let [values (reduce (fn [acc v]
                                       (if-let [params (match ptrn v)]
                                         (conj acc (get params param-key))
                                         (reduced nil)))
                                     []
                                     x)]
             {param-key values})
           (match ptrn x)))
       (-unmatch [_ params]
         (let [x (get params param-key)]
           (if (and (vector? x) (or (not grouped?) (empty? x) (some vector? x)))
             (mapv #(unmatch ptrn (assoc params param-key %)) x)
             (unmatch ptrn params))))
       (-match-validator [_] #(or (string? %) (repeated-values? string? %)))
       ;; The scalar pattern validates each value on match and unmatch.
       (-unmatch-validators [_] {})
       (-create-default [_ v] (create-default ptrn v)))))

(defn- query-value-pattern
  [ptrn]
  (cond (instance? RegexPattern ptrn) (query-regex-pattern ptrn)
        (instance? OptionalPattern ptrn) (->OptionalPattern
                                          (query-value-pattern (.-ptrn ptrn))
                                          (.-default-params ptrn))
        :else ptrn))

(defn- with-query-captures
  [ptrn]
  (let [query (:query ptrn)]
    (if (and (not (record? ptrn))
             (or (instance? PersistentArrayMap query)
                 (instance? PersistentHashMap query)))
      (assoc ptrn :query (update-vals query query-value-pattern))
      ptrn)))

(deftype Route [name pattern compiled-pattern]
  Pattern
    (-match [this -url]
      (let [u (url -url)]
        (when-let [params (match compiled-pattern
                            (if (nil? (:query u)) (assoc u :query {}) u))]
          (assoc params ::name name ::pattern pattern))))
    (-unmatch [this params]
      (->> (dissoc params ::name ::pattern)
           (unmatch compiled-pattern)
           url))
    (-match-validator [_] map?)
    (-unmatch-validators [_] {})
  #?@(:clj [java.util.Map$Entry (getKey [_] name) (getValue [_] pattern)]
      :cljs [IMapEntry (-key [_] name) (-val [_] pattern)]))

(defn route? [x] (instance? Route x))

(defn route
  [x]
  (if (route? x)
    x
    (let [[nm ptrn] x
          pattern (url-pattern ptrn)]
      (->Route nm pattern (with-query-captures pattern)))))


;;;; Routes Pattern ;;;;

(deftype Routes [routes-seq routes-map]
  Pattern
    (-match [this -url]
      (let [u (url -url)]
        (some (fn [route]
                (when-let [params (match route u)]
                  (assoc params ::routes this ::url u)))
              routes-seq)))
    (-unmatch [this
               {:as params,
                nm ::name}]
      (assert (and (some? nm) (contains? routes-map nm)))
      (unmatch (get routes-map nm) (dissoc params ::routes ::url)))
    (-match-validator [_] map?)
    (-unmatch-validators [_] {}))

(defn routes? [x] (instance? Routes x))

(defn routes
  [rtes]
  (if (routes? rtes)
    rtes
    (let [rtes-seq (mapcat #(if (routes? %)
                              (.-routes-seq %)
                              (-> %
                                  route
                                  list))
                    rtes)
          rtes-map (-> (fn [memo rte]
                         (let [rte (route rte)
                               k (key rte)]
                           (if (nil? k) memo (assoc! memo k rte))))
                       (reduce (transient {}) rtes-seq)
                       persistent!)]
      (->Routes rtes-seq rtes-map))))

(defn arrive
  ([rtes x] (arrive rtes x identity))
  ([rtes x handler]
   (->> x
        url
        (match rtes)
        handler)))

(defn depart
  ([rtes nm] (depart rtes nm {} str))
  ([rtes nm params] (depart rtes nm params str))
  ([rtes nm params handler]
   (->> (assoc params ::name nm)
        (unmatch rtes)
        handler)))
