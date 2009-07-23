(ns clojure-rdf.vocabs.rdfs
  (:refer-clojure :exclude (range comment))
  (:use clojure-rdf.namespace))

(ns-unmap *ns* 'Class)

(use-prefix "rdfs" "http://www.w3.org/2000/01/rdf-schema#")

(defresources
  Resource
  Class
  Literal
  Datatype
  range
  domain
  subClassOf
  subPropertyOf
  label
  comment
  Container
  ContainerMembershipProperty
  member
  seeAlso
  isDefinedBy)
