(ns domkm.silk.serve
  (:require [clojure.string :as str]
            [domkm.silk :as silk]))


#+clj
(defn request-map->URL [{:keys [scheme server-name server-port uri query-string] :as req}]
  (-> {:scheme (name scheme)
       :host (-> server-name (str/split #"\.") reverse vec)
       :port (str server-port)
       :path (silk/decode-path uri)
       :query (silk/decode-query query-string)}
      silk/url
      (assoc :request req)))

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
  ([routes] (ring-handler routes identity))
  ([routes get-handler]
   (let [rtes (silk/routes routes)]
     (fn [req]
       (when-let [params (silk/match rtes (request-map->URL req))]
         ((-> params :domkm.silk/name get-handler)
          (assoc req :params params)))))))


;;;; Request Method Pattern ;;;;

(defrecord RequestMethodPattern [method optional?]
  silk/Pattern
  (-match [_ mthd]
          (when (or (#+clj identical? #+cljs keyword-identical? method mthd)
                    (and optional? (nil? mthd)))
            {}))
  (-unmatch [_ _]
            (when-not optional?
              method)))

(defn request-method-pattern [method optional?]
  {:pre [(#{:delete :get :head :options :post :put} method)]}
  (->RequestMethodPattern method optional?))

(defn method [mthd url-ptrn]
  (assoc-in (silk/url-pattern url-ptrn)
            [:request :request-method]
            (request-method-pattern mthd false)))

(defn ?method [mthd url-ptrn]
  (assoc-in (silk/url-pattern url-ptrn)
            [:request :request-method]
            (request-method-pattern mthd true)))
