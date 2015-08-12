(ns domkm.silk.serve
  (:require [clojure.string :as str]
            [domkm.silk :as silk])
  #+cljs
  (:require-macros [domkm.silk.serve :refer [def-request-methods]])
  #+clj
  (:import [clojure.lang IFn]))


#+clj
(defn request-map->URL [{:keys [scheme server-name server-port uri query-string] :as req}]
  (->> {:scheme (name scheme)
        :host (-> server-name (str/split #"\.") reverse vec)
        :port (str server-port)
        :path (silk/decode-path uri)
        :query (silk/decode-query query-string)}
       (merge req)
       silk/url))

#+clj
(defn ring-handler
  "Takes a routes data structure or instance of `domkm.silk/Routes` and a `get-handler` function.
  `get-handler` should take a route name and return a handler function for that route.
  Returns a Ring handler function that:
  * takes Ring request map
  * converts the request map to a URL
  * associates the request map into the URL
  * matches routes against the new URL
  * when a match is found, associates params into request and passes that to a route handler function"
  ([routes]
   (ring-handler routes identity))
  ([routes get-handler]
   (let [rtes (silk/routes routes)]
     (fn [req]
       (when-let [params (silk/match rtes (request-map->URL req))]
         ((-> params :domkm.silk/name get-handler)
          (assoc req :params params)))))))


;;;; Request Method Pattern ;;;;

(deftype ^:private RequestMethodPattern [method opt?]
  silk/Pattern
  (-match [_ mthd]
          (when (or (#+clj identical? #+cljs keyword-identical? method mthd)
                    (and opt? (nil? mthd)))
            {}))
  (-unmatch [_ _]
            (when-not opt?
              method))
  (-match-validator [_]
                    (if opt?
                      (some-fn nil? keyword?)
                      keyword?))
  (-unmatch-validators [_]
                       {})
  IFn
  (#+clj invoke #+cljs -invoke [this]
         {:request-method this})
  (#+clj invoke #+cljs -invoke [this url-ptrn]
         (assoc (silk/url-pattern url-ptrn) :request-method this))
  #+clj
  (applyTo [this args]
    (clojure.lang.AFn/applyToHelper this args)))


(defmacro ^:private def-request-methods [mthds]
  (cons 'do
        (for [mthd mthds
              opt? [true false]]
          `(def ~(->> mthd name str/upper-case (str (when opt? "?")) symbol)
             (~'RequestMethodPattern. ~mthd ~opt?)))))

(def-request-methods #{:delete :get :head :options :post :put})
