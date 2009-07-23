(ns clojure-rdf.vocabs.dc
  (:refer-clojure :exclude (type format))
  (:use clojure-rdf.namespace))

(use-prefix "dc" "http://purl.org/dc/elements/1.1/")

(defresources contributor, coverage, creator, date, description,
format, identifier, language, publisher, relation, rights, source,
subject, title, type)
