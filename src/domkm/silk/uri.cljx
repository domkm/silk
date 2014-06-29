(ns domkm.silk.uri)

(defn ^String encode
  [^String s]
  (some-> s
          str
          #+clj (java.net.URLEncoder/encode "UTF-8")
          #+cljs js/encodeURIComponent
          #+clj (.replace "+" "%20")
          #+cljs (.replace #"[!'()]" js/escape)
          #+cljs (.replace "~" "%7E")
          ))

(defn ^String decode
  [^String s]
  (some-> s
          str
          #+clj (java.net.URLDecoder/decode "UTF-8")
          #+cljs js/decodeURIComponent))
