(ns clojure-rdf.vocabs.rdf
  (:refer-clojure :exclude (type first rest))
  (:use clojure-rdf.namespace))

(use-prefix "rdf" "http://www.w3.org/1999/02/22-rdf-syntax-ns#")

(defresource type)
(defresource subject)
(defresource predicate)
(defresource object)
(defresource first)
(defresource rest)
(defresource value)
(defresource Alt)
(defresource Bag)
(defresource Seq)
(defresource XMLLiteral)

