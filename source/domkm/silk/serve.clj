(ns domkm.silk.serve
  (:require [domkm.silk.url :as url :refer [url]]))

(defn request-map->url [{:keys [request-method scheme server-name server-port uri query-string] :as req}]
  (url {:request-method request-method
        :scheme scheme
        :host (url/decode-host server-name)
        :path (url/decode-path uri)
        :query (url/decode-query query-string)}))
