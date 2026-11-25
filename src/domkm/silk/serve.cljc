(ns domkm.silk.serve
  (:require [clojure.string :as str]
            [domkm.silk :as silk])
  #?(:cljs (:require-macros [domkm.silk.serve :refer [def-request-methods]]))
  #?(:clj (:import [clojure.lang IFn])))


#?(:clj (defn request-map->URL
          [{:as req,
            :keys [scheme server-name server-port uri query-string]}]
          (->> {:host (-> server-name
                          (str/split #"\.")
                          reverse
                          vec),
                :path (silk/decode-path uri),
                :port (str server-port),
                :query (silk/decode-query query-string),
                :scheme (name scheme)}
               (merge req)
               silk/url)))

#?(:clj
     (defn ring-handler
       "Takes a routes data structure or instance of `domkm.silk/Routes` and a `get-handler` function.
  `get-handler` should take a route name and return a handler function for that route.
  Returns a Ring handler function that:
  * takes Ring request map
  * converts the request map to a URL
  * associates the request map into the URL
  * matches routes against the new URL
  * when a match is found, merges route params into request params, with route
    params taking precedence, and passes the request to the route handler"
       ([routes] (ring-handler routes identity))
       ([routes get-handler]
        (let [rtes (silk/routes routes)]
          (fn [req]
            (if-let [params (silk/match rtes (request-map->URL req))]
              ((-> params
                   :domkm.silk/name
                   get-handler)
               (update req :params merge params))
              ((get-handler nil) req)))))))


;;;; Request Method Pattern ;;;;

(deftype ^:private RequestMethodPattern [method opt?]
  silk/Pattern
    (-match [_ mthd]
      (when (or (#?(:clj identical?
                    :cljs keyword-identical?)
                 method
                 mthd)
                (and opt? (nil? mthd)))
        {}))
    (-unmatch [_ _] (when-not opt? method))
    (-match-validator [_] (if opt? (some-fn nil? keyword?) keyword?))
    (-unmatch-validators [_] {})
  IFn
    (#?(:clj invoke
         :cljs -invoke)
      [this]
      {:request-method this})
    (#?(:clj invoke
         :cljs -invoke)
      [this url-ptrn]
      (assoc (silk/url-pattern url-ptrn) :request-method this))
  #?(:clj (applyTo [this args] (clojure.lang.AFn/applyToHelper this args))))


#?(:clj (defmacro ^:private def-request-methods
          [mthds]
          (cons 'do
                (for [mthd mthds
                      opt? [true false]]
                  `(def ~(->> mthd
                              name
                              str/upper-case
                              (str (when opt? "?"))
                              symbol)
                     (~'RequestMethodPattern. ~mthd ~opt?))))))

(def-request-methods #{:delete :get :head :options :post :put})
