(ns domkm.silk.serve
  (:require [domkm.silk :as silk]))


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
