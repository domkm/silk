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
     (class (char-array 0))
     (code-point-at [arr i]
       (Character/codePointAt arr i))
     (length [arr]
       (alength arr))
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

(def ^:private ^:const eot 4)

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
                        eot)
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
  (satisfies? FSM x))

(defn precompiled-fsm? [x]
  (instance? PrecompiledFSM x))

(defn compiled-fsm? [x]
  (instance? CompiledFSM x))

(defn ^:private automat-fsm
  "Returns the Automat FSM associated with this Automaton."
  [a]
  {:pre [(fsm? a)]
   :post [(cond
            (precompiled-fsm? a) (a.compiler/precompiled-automaton? %)
            (compiled-fsm? a) (a.compiler/compiled-automaton? %)
            :else (a.fsm/automaton? %))]}
  (-automat-fsm a))

(defn ^:private captures
  "Returns a map of UUIDs to capture keys."
  [a]
  {:pre [(fsm? a)]
   :post [(map? %)
          (every? keyword? (keys %))
          (every? sequential? (vals %))]}
  (-captures a))

(defn fsm
  ([a]
   (if (fsm? a)
     (fsm (automat-fsm a) (captures a))
     (fsm a {})))
  ([a capts]
   {:pre [(or (and (fsm? a)
                   (clj/not (precompiled-fsm? a))
                   (clj/not (compiled-fsm? a)))
              (a.fsm/automaton? a)
              (code-points? a))
          (map? capts)
          (every? keyword? (keys capts))
          (every? sequential? (vals capts))]}
   (reify
     FSM
     (-automat-fsm [_]
       (cond
         (fsm? a) (automat-fsm a)
         (code-points? a) (-> a code-points a.compiler/parse-automata)
         (a.fsm/automaton? a) a))
     (-captures [_]
       (if (fsm? a)
         (merge (captures a) capts)
         capts)))))

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
    (if (== input eot)
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
                                          (automat-input-stream cps))
        captured (persistent! value)]
    (when accepted?
      (reduce
       (fn [m [uuid ks]]
         (let [s (get captured uuid)]
           (if (string? s)
             (update-in m ks #(if %
                                (->> {:keys ks
                                      :vals [% s]}
                                     (ex-info "Capture keys must be unique.")
                                     throw)
                                s))
             m)))
       {}
       (sort-by #(-> % val count)
                #(compare %2 %1)
                (captures fsm))))))

(defn capture [a ks]
  {:pre [(sequential? ks)]}
  (let [uuid (rand-uuid)
        fsm (fsm a)]
    (reify
      FSM
      (-automat-fsm [_]
        (a.compiler/parse-automata
         [(->> fsm automat-fsm (a/interpose-$ uuid))
          (-> eot a/not a/*)
          eot
          (a/$ uuid)]))
      (-captures [_]
        (assoc (captures fsm) uuid ks)))))

(defn ^:private wrap-automat-fsm [f & as]
  (let [fsms (map fsm as)]
    (reify
      FSM
      (-automat-fsm [_]
        (apply f (map automat-fsm fsms)))
      (-captures [_]
        (reduce merge (map captures fsms))))))

(defn ? [a]
  (wrap-automat-fsm a/? a))

(defn * [a]
  (wrap-automat-fsm a/* a))

(defn + [a]
  (wrap-automat-fsm a/+ a))

(defn | [& as]
  (apply wrap-automat-fsm a/or as))

(defn & [& as]
  (apply wrap-automat-fsm a/and as))

(defn cat [& as]
  (apply wrap-automat-fsm #(-> %& vec a.compiler/parse-automata) as))

(defn not [char]
  {:pre [(code-points? chars)
         (= (length char) 1)]}
  (reify
    FSM
    (-automat-fsm [_]
      (a/not (code-point-at char 0)))
    (-captures [_]
      {})))
