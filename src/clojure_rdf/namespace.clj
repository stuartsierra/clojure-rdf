(ns clojure-rdf.namespace
  (:use clojure-rdf.model))

(defn use-prefix [prefix uri]
  (assert (string? prefix))
  (assert (string? uri))
  (alter-meta! *ns* assoc ::prefix prefix ::uri uri))

(defmacro defresource [sym]
  `(def ~sym (make-resource (str (::uri (meta *ns*))
                                 ~(str sym)))))
