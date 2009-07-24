(ns clojure-rdf.vocabs.rdf
  (:refer-clojure :exclude (type first rest))
  (:use clojure-rdf.namespace clojure-rdf.model))

(use-prefix "rdf" "http://www.w3.org/1999/02/22-rdf-syntax-ns#")

;; This list is missing nil because we can't define the symbol nil
(defresources type Property Statement subject predicate object Bag Seq
  Alt value List first rest XMLLiteral)

(def NIL (make-resource "http://www.w3.org/1999/02/22-rdf-syntax-ns#nil"))
