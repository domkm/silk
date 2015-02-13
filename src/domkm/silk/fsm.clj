(ns domkm.silk.fsm
  (:refer-clojure :exclude [* + compile conj cons find not])
  (:require [automat.core :as auto]
            [automat.fsm :as fsm]
            [clojure.core :as core])
  (:import  [automat.fsm.IAutomaton]))


(defrecord Automaton [automaton])

(defn automaton? [x]
  (instance? Automaton x))

(defrecord CompiledAutomaton [automaton compiled-automaton])

(defn compiled-automaton? [x]
  (instance? CompiledAutomaton x))

(def ^:private valid-input?
  (some-fn nil? integer? keyword? char? string?))

(declare automaton)

(defn reducers [a]
  (dissoc (automaton a) :automaton))

(defn automaton [x]
  (cond
   (automaton? x) x
   (compiled-automaton? x) (:automaton x)
   (instance? automat.fsm.IAutomaton x) (->Automaton x)
   (sequential? x) (let [as (map automaton (flatten x))]
                     (apply merge
                            (->> as (mapv :automaton) auto/parse-automata ->Automaton)
                            (->> as (map reducers) (reduce merge))))
   :else (if (valid-input? x)
           (-> x auto/parse-automata ->Automaton)
           (->> (pr-str x)
                (str "Invalid automaton input: ")
                IllegalArgumentException.
                throw))))

(defn capture [a id]
  (let [a (automaton a)
        pre (-> id (str "-enter") keyword)
        post (-> id (str "-exit") keyword)
        in (-> id str keyword)]
    (assoc a
      :automaton (auto/parse-automata [(auto/$ pre)
                                       (auto/interpose-$ in (:automaton a))
                                       (auto/$ post)])
      pre (fn [state _]
            (-> state
                (update-in [0] assoc id [])
                (update-in [1] disj id)))
      post (fn [state _]
             (update-in state [1] core/conj id))
      in (fn [state input]
           (update-in state [0 id] core/conj input)))))

(defn conj [a i]
  (let [a (automaton a)
        k (->> i (format "conj-%s-") gensym keyword)]
    (assoc a
      :automaton (auto/parse-automata [(:automaton a) i (auto/$ k)])
      k core/conj)))

(defn ^:private wrap-automata [f & as]
  (let [as (map automaton as)]
    (apply merge
           (->> as (map :automaton) (apply f) automaton)
           (map reducers as))))

(defn ? [a]
  (wrap-automata auto/? a))

(defn * [a]
  (wrap-automata auto/* a))

(defn + [a]
  (wrap-automata auto/+ a))

(defn | [& as]
  (apply wrap-automata auto/or as))

(defn & [& as]
  (apply wrap-automata auto/and as))

(defn cat [& as]
  (automaton as))

; . is a special form :(
(def any (automaton auto/any))

; ^ is special :(
(defn not [input]
  {:pre [(valid-input? input)]}
  (-> input auto/not automaton))

(defn compile [a]
  (if (compiled-automaton? a)
    a
    (let [a (automaton a)]
      (->> {:reducers (reducers a)}
           (auto/compile (:automaton a))
           (->CompiledAutomaton a)))))

(defn find [^CompiledAutomaton {a :compiled-automaton} inputs]
  (let [{:keys [accepted? value]} (auto/greedy-find a [{} #{}] inputs)]
    (when accepted?
      value)))

(defn optional? [a]
  (let [a (-> a automaton :automaton)]
    (boolean (get (fsm/accept a) (fsm/start a)))))

(defn cons [x a]
  {:pre [(valid-input? x)]}
  (let [a (automaton a)
        fsm (:automaton a)
        optional? (get (fsm/accept fsm) (fsm/start fsm))
        accept-states]
    (assoc a
      :automaton
      (if optional?
        (condp = (-> fsm fsm/accept count)
          1 (auto/* fsm)
          2 (auto/? fsm))
        (auto/parse-automata [x fsm])))))
