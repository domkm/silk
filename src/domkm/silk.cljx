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

(defn url-pattern [x]
  (cond
   (map? x) x
   (vector? x) (let [[path query etc] x]
                 (->> (assoc etc :path path :query query)
                      (remove (fn [[k v]] (nil? v)))
                      (into {})))))


;;;; Leaf Pattern ;;;;

(def ^:private re-quote-char-map
  (reduce #(assoc %1 %2 (str "\\" %2))
          {}
          "\\.*+|?()[]{}$^"))

; TODO: add `clojure.string/re-quote-replacement` to ClojureScript
(defn ^:private re-quote-replacement [s]
  (str/escape s re-quote-char-map))

(defrecord LeafPattern [param-key optional?
                        regex validate
                        extract insert
                        deserialize serialize]
  Pattern
  (-match
   [this string]
   (let [param-val (if (and optional? (nil? string))
                     (-> nil extract deserialize)
                     (some->> string
                              (re-find regex)
                              extract
                              deserialize))]
     (when-not (nil? param-val)
       (if (validate param-val)
         (if (nil? param-key)
           {}
           {param-key param-val})
         (-> "parameter value failed validation"
             (ex-info {:pattern this
                       :string string})
             throw)))))
  (-unmatch
   [this params]
   (cond
    (nil? param-key) (insert nil)
    (contains? params param-key) (let [param-val (get params param-key)]
                                   (if (validate param-val)
                                     (-> param-val serialize insert)
                                     (-> "parameter value failed validation"
                                         (ex-info {:pattern this
                                                   :params params})
                                         throw)))
    :else (if optional?
            (insert nil)
            (-> "parameter key not found"
                (ex-info {:pattern this
                          :params params})
                throw)))))

(defn leaf-pattern? [x]
  (instance? LeafPattern x))

(defn leaf-pattern [x]
  (cond
   (leaf-pattern? x) x
   (map? x) (-> {:regex #".+"
                 :extract #(if (vector? %) (first %) %)
                 :insert identity
                 :deserialize identity
                 :serialize identity
                 :validate (constantly true)}
                (merge x)
                map->LeafPattern)
   (string? x) (->> (str "^" (re-quote-replacement x) "$")
                    re-pattern
                    (hash-map :insert (constantly x) :regex)
                    leaf-pattern)
   (keyword? x) (leaf-pattern {:param-key x})))


;;;; Extra Leaf Patterns ;;;;

(defn integer [k]
  {:pre [(keyword? k)]}
  (leaf-pattern
   {:param-key k
    :regex #"^\d+$"
    :deserialize #+clj #(Integer/parseInt %) #+cljs #(js/parseInt % 10)
    :serialize str
    :validate integer?}))

(defn boolean [k]
  {:pre [(keyword? k)]}
  (leaf-pattern
   {:param-key k
    :regex #"^true$|^false$"
    :deserialize #(= "true" %)
    :serialize str
    :validate #+clj #(instance? Boolean %) #+cljs #(identical? js/Boolean (type %))}))

(defn uuid [k]
  {:pre [(keyword? k)]}
  (leaf-pattern
   {:param-key k
    :regex #"^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$"
    :deserialize #+clj #(UUID/fromString %) #+cljs ->UUID
    :serialize str
    :validate #(instance? UUID %)}))

(defn alternative
  ([alts]
   {:pre [(every? string? alts)]} ; TODO: It would be nice if we could use any patterns, not just strings.
   (->> alts
        (map #(str "^" (re-quote-replacement %) "$"))
        (str/join "|")
        re-pattern
        (hash-map :insert (-> alts first constantly) :regex)
        leaf-pattern))
  ([k alts]
   {:pre [(keyword? k)]}
   (assoc (alternative alts)
     :param-key k
     :insert identity
     :validate (set alts))))

(defn composite
  "Takes a seqable of strings and one LeafPattern or Keyword.
  Returns a LeafPattern that matches a composite of the strings and LeafPattern."
  [strs&lp]
  {:pre [(->> strs&lp (remove string?) count (= 1))
         (apply (some-fn keyword? leaf-pattern?) strs&lp)]}
  (let [[a [_ z]] (split-with string? strs&lp)
        [a-str z-str] (map str/join [a z])]
    (merge (->> strs&lp
                (remove string?)
                first
                leaf-pattern)
           {:regex (re-pattern (str "^" a-str "(.+)" z-str "$"))
            :extract second
            :insert #(str a-str % z-str)})))

(defn option [pattern default]
  {:pre [(string? default)]}
  (let [wrap-fn (fn [f x] #(if (nil? %) x (f %)))
        lp (leaf-pattern pattern)
        lp (assoc lp
             :optional? true
             :extract (wrap-fn (:extract lp) default)
             :insert (wrap-fn (:insert lp) default))]
    (assert (let [m (match lp default)]
              (and m (unmatch lp m)))
            "default does not match")
    lp))


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
   (->Route id (url-pattern pattern))))


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
