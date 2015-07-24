(ns domkm.silk
  (:refer-clojure :exclude [int * +])
  (:require [clojure.core :as core]
            [clojure.set :as set]
            [clojure.string :as str]
            [domkm.silk.fsm :as fsm]
            [domkm.silk.url :as url])
  (:import  [clojure.lang Keyword PersistentArrayMap PersistentHashMap PersistentVector]
            [java.util UUID]))


(defprotocol Patternable
  (-pattern [this] "Converts this to an instance of Pattern."))

(defn patternable? [x]
  (satisfies? Patternable x))

(defrecord Pattern [groups automaton]
  Patternable
  (-pattern [this] this))

(defn pattern? [x]
  (instance? Pattern x))

(defn pattern [x]
  (-pattern x))

(defrecord Group [name uuid automaton]
  Patternable
  (-pattern [this]
            (map->Pattern {:groups #{this}
                           :automaton automaton})))
(defn group? [x]
  (instance? Group x))

(defn group
  ([fsm] (group nil fsm))
  ([name fsm]
   (let [uuid (UUID/randomUUID)]
     (map->Group {:name name
                  :uuid uuid
                  :automaton (if name
                               (fsm/capture fsm uuid)
                               (fsm/automaton fsm))}))))

(defn ^:private persistent-map->pattern [m]
  {:pre [(every? string? (keys m))]}
  (let [entries (map (fn [[k v]]
                       [k (pattern v)])
                     (sort m))
        any* (fsm/* fsm/any)]
    (map->Pattern {:groups (->> entries (map (comp :groups second)) (apply set/union))
                   :automaton (fsm/cat any*
                                       (interpose any*
                                                  (map (fn [[k {v :automaton}]]
                                                         (let [fsm (fsm/cat :url/part k v)]
                                                           (if (fsm/optional? v)
                                                             (fsm/* fsm)
                                                             fsm)))
                                                       entries))
                                       any*)})))

; TODO: Capturing (fsm/+ fsm/any) works correctly but capturing
; (fsm/+ anything-but-X) causes one additional input to be captured.
(def ^:private not-url-keyword-automaton
  (->> [:url/host :url/path :url/query :url/part]
       (map fsm/not)
       (apply fsm/&)))

(extend-protocol Patternable

  nil
  (-pattern [_] (-> not-url-keyword-automaton fsm/* group pattern))

  String
  (-pattern [this] (-> this seq group pattern))

  Keyword
  (-pattern [this] (->> not-url-keyword-automaton fsm/+ (group this) pattern))

  PersistentVector
  (-pattern [this]
            (let [ptrns (map pattern this)]
              (map->Pattern {:groups (->> ptrns (map :groups) (apply set/union))
                             :automaton (->> ptrns
                                             (map (comp #(let [fsm (fsm/cat :url/part %)]
                                                           (if (fsm/optional? %)
                                                             (fsm/* fsm)
                                                             fsm))
                                                        :automaton))
                                             (apply fsm/cat))})))

  PersistentArrayMap
  (-pattern [this] (persistent-map->pattern this))

  PersistentHashMap
  (-pattern [this] (persistent-map->pattern this)))

(defn ^:private wrap-patterns [f & ptrns]
  (let [ptrns (map pattern ptrns)]
    (map->Pattern {:groups (->> ptrns (map :groups) (apply set/union))
                   :automaton (->> ptrns (map :automaton) (apply f))})))

(defn ? [ptrn]
  (wrap-patterns fsm/? ptrn))

(defn * [ptrn]
  (wrap-patterns fsm/* ptrn))

(defn + [ptrn]
  (wrap-patterns fsm/+ ptrn))

(defn | [& ptrns]
  (apply wrap-patterns fsm/| ptrns))

(defn & [& ptrns]
  (apply wrap-patterns fsm/& ptrns))

(defn cat [& ptrns]
  (apply wrap-patterns fsm/cat ptrns))

(defn bool [k]
  (->> ["true" "false"]
       (map seq)
       (apply fsm/|)
       (group k)
       pattern))

(defn int [k]
  (->> (seq "0123456789")
       (apply fsm/|)
       fsm/+
       (group k)
       pattern))

(defn uuid [k]
  (let [hex-char (apply fsm/| (seq "0123456789abcdef"))]
    (->> [8 4 4 4 12]
         (map #(repeat % hex-char))
         (interpose "-")
         flatten
         (group k)
         pattern)))

(defrecord Route [name pattern]
  Patternable
  (-pattern [_] pattern))

(defn route? [x]
  (instance? Route x))

(defn route
  ([rte]
   (cond
    (route? rte) rte
    (vector? rte) (apply route rte)))
  ([name ptrn]
   (cond
    (pattern? ptrn) (->Route name ptrn)
    (nil? ptrn) (->Route name (-> fsm/any fsm/* group pattern))
    (vector? ptrn) (let [[path query etc] ptrn
                         m (assoc etc :path path :query query)]
                     (route name m))
    (map? ptrn) (let [{:keys [request-method scheme host path query]} ptrn
                      [request-method scheme] (map #(group (cond
                                                            (nil? %) fsm/any
                                                            (keyword? %) %
                                                            (set? %) (apply fsm/| %)))
                                                   [request-method scheme])
                      [host path query] (map #(if (nil? %)
                                                (group (fsm/* fsm/any))
                                                %)
                                             [host path query])]
                  (->Route name (cat request-method
                                     scheme
                                     (group :url/host)
                                     host
                                     (group :url/path)
                                     path
                                     (group :url/query)
                                     query))))))

(defrecord RouteTable [routes compiled-automaton])

(defn route-table? [x]
  (instance? RouteTable x))

(defn routes [& rtes]
  (let [rtes (->> rtes
                  (map #(if (route-table? %) (:routes %) (route %)))
                  flatten
                  vec)
        ca (->> rtes
                (map-indexed (fn [i rte]
                               (fsm/conj [(-> rte :pattern :automaton) (fsm/* fsm/any)] i)))
                (apply fsm/|)
                fsm/compile)]
    (->RouteTable rtes ca)))

(defn match [^RouteTable {:keys [routes compiled-automaton]} url]
  (when-let [[reductions accepted i] (fsm/find compiled-automaton
                                               (concat (url/url->seq url)
                                                       (-> routes count range)))]
    (reduce (fn [params {:keys [name uuid]}]
              (if (accepted uuid)
                (assoc params name (reductions uuid))
                params))
            {}
            (->> i (nth routes) :pattern :groups (filter :name)))))
