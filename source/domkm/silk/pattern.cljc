(ns domkm.silk.pattern
  (:refer-clojure :exclude [* + cat compile not])
  (:require
   [automat.compiler.core :as a.compiler]
   [automat.core :as a]
   [automat.fsm :as a.fsm]
   #?@(:clj
       [[clojure.core :as clj]]
       :cljs
       [[automat.stream :refer [InputStream]]
        [cljs.core :as clj]]))
  (:import
   #?@(:clj
       [[automat.utils InputStream]
        [java.util UUID]]
       :cljs
       [goog.string.StringBuffer])))

#?(:cljs (def ^:private StringBuilder goog.string.StringBuffer))

(defn ^:private rand-uuid []
  (-> #?(:clj (UUID/randomUUID) :cljs (random-uuid))
      str
      keyword))

(def ^:private ^:const stx
  "Start of Text"
  2)

(def ^:private ^:const etx
  "End of Text"
  3)

(defprotocol ^:private Pattern
  (^:private -fsm [_])
  (^:private -captures [_]))

(defrecord PrecompiledPattern [fsm captures]
  Pattern
  (-fsm [_] fsm)
  (-captures [_] captures))

(deftype CompiledPattern [fsm captures]
  Pattern
  (-fsm [_] fsm)
  (-captures [_] captures))

(defn ^:private validate-code-point [cp]
  (assert (< 31 cp) (str "Code points less than 32 are reserved for Silk. Got: " cp))
  cp)

(extend-type #?(:clj String :cljs string)
  Pattern
  (-fsm [s]
    (->> s
         count
         range
         (mapv (fn [^Integer i]
                 (validate-code-point
                  #?(:clj
                     (.codePointAt s i)
                     :cljs
                     (.charCodeAt s i)))))
         a.compiler/parse-automata))
  (-captures [_]
    {}))

#?(:clj
   (extend-type Character
     Pattern
     (-fsm [c]
       (-> c int validate-code-point))
     (-captures [_]
       {})))

(defn pattern? [x]
  (satisfies? Pattern x))

(defn precompiled-pattern? [x]
  (instance? PrecompiledPattern x))

(defn compiled-pattern? [x]
  (instance? CompiledPattern x))

(defn ^:private fsm
  "Returns the Automat Pattern associated with this Automaton."
  [ptrn]
  {:pre [(pattern? ptrn)]
   :post [(cond
            (precompiled-pattern? ptrn) (a.compiler/precompiled-automaton? %)
            (compiled-pattern? ptrn) (a.compiler/compiled-automaton? %)
            :else (a.fsm/automaton? %))]}
  (-fsm ptrn))

(defn ^:private captures
  "Returns a map of UUIDs to capture keys."
  [ptrn]
  {:pre [(pattern? ptrn)]
   :post [(map? %)
          (every? keyword? (keys %))]}
  (-captures ptrn))

(defn precompile [ptrn]
  {:pre [(pattern? ptrn)]
   :post [(precompiled-pattern? %)
          (fsm %)
          (captures %)]}
  (cond
    (precompiled-pattern? ptrn) ptrn
    (compiled-pattern? ptrn) (->> {:pattern ptrn}
                                  (ex-info "Cannot precompile a `CompiledPattern'.")
                                  throw)
    :else (->PrecompiledPattern (-> ptrn fsm a/precompile)
                                (captures ptrn))))

(defn ^:private build-string! [^StringBuilder sb ^long input]
  (let [sb (if (nil? sb)
             (new StringBuilder)
             sb)]
    (if (== input etx)
      (.toString sb)
      (.append sb (char input)))))

(defn ^:private reducers [ptrn]
  {:pre [(pattern? ptrn)]}
  (reduce (fn [reducers uuid]
            (assoc reducers
                   uuid
                   (fn [state ^long input]
                     (->> input
                          (build-string! (get state uuid))
                          (assoc! state uuid)))))
          {}
          (-> ptrn captures keys)))

(defn compile [ptrn]
  {:pre [(pattern? ptrn)]
   :post [(compiled-pattern? %)]}
  (cond
    (compiled-pattern? ptrn) ptrn
    (precompiled-pattern? ptrn) (->CompiledPattern
                                 (a/compile (fsm ptrn) {:backend :base
                                                        :reducers (reducers ptrn)})
                                 (captures ptrn))
    :else (-> ptrn precompile compile)))

(defn match
  "Takes a `CompiledPattern' and a `String'.
  On match, returns a nested map where the keys are capture keys and the vals are captured strings.
  Otherwise returns `nil'."
  [^CompiledPattern ptrn ^String s]
  {:pre [(compiled-pattern? ptrn)
         (string? s)]}
  (let [s (str s (char etx))
        arr #?(:clj
               (-> ^String s .codePoints .toArray)
               :cljs
               (let [len (.-length s)
                     a (make-array len)]
                 (doseq [i (range len)]
                   (aset a i (.charCodeAt s i)))
                 a))
        {:keys [accepted? value]} (a/find (fsm ptrn)
                                          (transient {})
                                          arr)]
    (when accepted?
      (let [captured (persistent! value)
            captures (captures ptrn)]
        (reduce
         (fn [m [uuid s]]
           (if (string? s)
             (let [k (get captures uuid)]
               (if (contains? m k)
                 (->> {:key k}
                      (ex-info (str "Duplicate capture key: " k))
                      throw)
                 (assoc m k s)))
             m))
         {}
         captured)))))

(defn capture
  ([k]
   (let [uuid (rand-uuid)]
     (reify
       Pattern
       (-fsm [_]
         (a.compiler/parse-automata
          [etx (a/$ uuid)]))
       (-captures [_]
         {uuid k}))))
  ([ptrn k]
   {:pre [(pattern? ptrn)
          (-> ptrn precompiled-pattern? clj/not)
          (-> ptrn compiled-pattern? clj/not)]}
   (let [uuid (rand-uuid)]
     (reify
       Pattern
       (-fsm [_]
         (a.compiler/parse-automata
          [(->> ptrn fsm (a/interpose-$ uuid))
           (-> etx a/not a/*)
           etx
           (a/$ uuid)]))
       (-captures [_]
         (assoc (captures ptrn) uuid k))))))

(defn ^:private wrap-pattern [f & ptrns]
  {:pre [(every? #(and (pattern? %)
                       (-> % precompiled-pattern? clj/not)
                       (-> % compiled-pattern? clj/not))
                 ptrns)]}
  (reify
    Pattern
    (-fsm [_]
      (->> ptrns
           (map fsm)
           (apply f)))
    (-captures [_]
      (->> ptrns
           (map captures)
           (apply merge)))))

(defn ? [ptrn]
  (wrap-pattern a/? ptrn))

(defn * [ptrn]
  (wrap-pattern a/* ptrn))

(defn + [ptrn]
  (wrap-pattern a/+ ptrn))

(defn | [& ptrns]
  (apply wrap-pattern a/or ptrns))

(defn & [& ptrns]
  (apply wrap-pattern a/and ptrns))

(defn cat [& ptrns]
  (apply wrap-pattern #(-> %& vec a.compiler/parse-automata) ptrns))

(defn not [char]
  {:pre [(or #?(:clj (char? char))
             (and (string? char)
                  (= (count char) 1)))]}
  (reify
    Pattern
    (-fsm [_]
      (-> char fsm a/not))
    (-captures [_]
      {})))
