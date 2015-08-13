(ns domkm.silk.fsm
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

(defprotocol CodePoints
  "Represents multiple code points."
  (code-point-at [obj i])
  (length [obj]))

#?(:clj
   (extend-protocol CodePoints
     String
     (code-point-at [s i]
       (.codePointAt s i))
     (length [s]
       (.length s))
     Character
     (code-point-at [c i]
       (if (= i 0)
         (int c)
         (throw (IndexOutOfBoundsException.))))
     (length [c]
       1))
   :cljs
   (extend-protocol CodePoints
     string
     (code-point-at [s i]
       (.charCodeAt s i))
     (length [s]
       (.-length s))))

(defn ^:private code-points? [x]
  (satisfies? CodePoints x))

(defn ^:private code-points
  "Takes something that satisfies `CodePoints'.
  Returns a vector of code points integers."
  [cps]
  {:pre [(code-points? cps)]}
  (mapv (partial code-point-at cps)
        (-> cps length range)))

(def ^:private ^:const stx
  "Start of Text"
  2)

(def ^:private ^:const etx
  "End of Text"
  3)

(defn ^:private automat-input-stream
  "Takes something that satisfies `CodePoints'.
  Returns an Automat `InputStream'."
  [chars]
  {:pre [(code-points? chars)]}
  (let [idx (volatile! 0)
        len (length chars)]
    (reify InputStream
      (nextInput [_ eof]
        (let [i @idx]
          (cond
            (< i len) (do
                        (vswap! idx inc)
                        (code-point-at chars i))
            (= i len) (do
                        (vswap! idx inc)
                        etx)
            (> i len) eof)))
      #?(:clj (nextNumericInput [_ eof] (.nextInput _ eof))))))

(defprotocol ^:private FSM
  (^:private -automat-fsm [fsm])
  (^:private -captures [fsm]))

(defrecord PrecompiledFSM [automat-fsm captures]
  FSM
  (-automat-fsm [_] automat-fsm)
  (-captures [_] captures))

(deftype CompiledFSM [automat-fsm captures]
  FSM
  (-automat-fsm [_] automat-fsm)
  (-captures [_] captures))

(defn fsm? [x]
  (or (satisfies? FSM x)
      (code-points? x)))

(defn precompiled-fsm? [x]
  (instance? PrecompiledFSM x))

(defn compiled-fsm? [x]
  (instance? CompiledFSM x))

(defn ^:private automat-fsm
  "Returns the Automat FSM associated with this Automaton."
  [fsm]
  {:pre [(fsm? fsm)]
   :post [(cond
            (precompiled-fsm? fsm) (a.compiler/precompiled-automaton? %)
            (compiled-fsm? fsm) (a.compiler/compiled-automaton? %)
            :else (a.fsm/automaton? %))]}
  (if (satisfies? FSM fsm)
    (-automat-fsm fsm)
    (let [cps (code-points fsm)]
      (assert (every? #(> % 31) cps) "Code points less than 32 are reserved for Silk.")
      (a.compiler/parse-automata cps))))

(defn ^:private captures
  "Returns a map of UUIDs to capture keys."
  [fsm]
  {:pre [(fsm? fsm)]
   :post [(map? %)]}
  (if (satisfies? FSM fsm)
    (-captures fsm)
    {}))

(defn precompile [fsm]
  {:pre [(fsm? fsm)]
   :post [(precompiled-fsm? %)
          (automat-fsm %)
          (captures %)]}
  (cond
    (precompiled-fsm? fsm) fsm
    (compiled-fsm? fsm) (->> {:fsm fsm}
                             (ex-info "Cannot precompile a `CompiledFSM'.")
                             throw)
    :else (->PrecompiledFSM (-> fsm automat-fsm a/precompile)
                            (captures fsm))))

(defn ^:private build-string! [^StringBuilder sb ^long input]
  (let [sb (if (nil? sb)
             (new StringBuilder)
             sb)]
    (if (== input etx)
      (.toString sb)
      (.append sb (char input)))))

(defn ^:private reducers [fsm]
  {:pre [(fsm? fsm)]}
  (reduce
   (fn [reducers uuid]
     (assoc
      reducers
      uuid
      (fn [state ^long input]
        (->> input
             (build-string! (get state uuid))
             (assoc! state uuid)))))
   {}
   (-> fsm captures keys)))

(defn compile [fsm]
  {:pre [(fsm? fsm)]
   :post [(compiled-fsm? %)]}
  (cond
    (compiled-fsm? fsm) fsm
    (precompiled-fsm? fsm) (->CompiledFSM
                            (a/compile (automat-fsm fsm) {:backend :base
                                                          :reducers (reducers fsm)})
                            (captures fsm))
    :else (-> fsm precompile compile)))

(defn match
  "Takes a `CompiledFSM' and something that satisfies `CodePoints'.
  On match, returns a nested map where the keys are capture keys and the vals are captured strings.
  Otherwise returns `nil'."
  [^CompiledFSM fsm cps]
  {:pre [(instance? CompiledFSM fsm)
         (code-points? cps)]}
  (let [{:keys [accepted? value]} (a/find (automat-fsm fsm)
                                          (transient {})
                                          (automat-input-stream cps))]
    (when accepted?
      (let [captured (persistent! value)
            captures (captures fsm)]
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
       FSM
       (-automat-fsm [_]
         (a.compiler/parse-automata
          [etx (a/$ uuid)]))
       (-captures [_]
         {uuid k}))))
  ([fsm k]
   {:pre [(fsm? fsm)
          (-> fsm precompiled-fsm? clj/not)
          (-> fsm compiled-fsm? clj/not)]}
   (let [uuid (rand-uuid)]
     (reify
       FSM
       (-automat-fsm [_]
         (a.compiler/parse-automata
          [(->> fsm automat-fsm (a/interpose-$ uuid))
           (-> etx a/not a/*)
           etx
           (a/$ uuid)]))
       (-captures [_]
         (assoc (captures fsm) uuid k))))))

(defn ^:private wrap-automat-fsm [f & fsms]
  {:pre [(every? #(and (fsm? %)
                       (-> % precompiled-fsm? clj/not)
                       (-> % compiled-fsm? clj/not))
                 fsms)]}
  (reify
    FSM
    (-automat-fsm [_]
      (->> fsms
           (map automat-fsm)
           (apply f)))
    (-captures [_]
      (->> fsms
           (map captures)
           (reduce merge)))))

(defn ? [fsm]
  (wrap-automat-fsm a/? fsm))

(defn * [fsm]
  (wrap-automat-fsm a/* fsm))

(defn + [fsm]
  (wrap-automat-fsm a/+ fsm))

(defn | [& fsms]
  (apply wrap-automat-fsm a/or fsms))

(defn & [& fsms]
  (apply wrap-automat-fsm a/and fsms))

(defn cat [& fsms]
  (apply wrap-automat-fsm #(-> %& vec a.compiler/parse-automata) fsms))

(defn not [char]
  {:pre [(code-points? char)
         (= (length char) 1)]}
  (reify
    FSM
    (-automat-fsm [_]
      (-> char automat-fsm a/not))
    (-captures [_]
      {})))
