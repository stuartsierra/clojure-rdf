(ns clojure-rdf.rio
  (:require [clojure.contrib.duck-streams :as duck])
  (:use clojure-rdf.model)
  (:import (org.openrdf.rio StatementHandler)
           (org.openrdf.model URI BNode Literal)
           (org.openrdf.rio.ntriples NTriplesParser)
           (org.openrdf.rio.rdfxml RdfXmlParser)
           (org.openrdf.rio.turtle TurtleParser)))

(defmulti from-rio class)

(defmethod from-rio BNode [#^BNode x]
  (make-blank-node (.getID x)))

(defmethod from-rio URI [#^URI x]
  (make-resource (.toString x)))

(defmethod from-rio Literal [#^Literal x]
  (make-literal (.getLabel x) (when (.getDatatype x)
                                (.getURI (.getDatatype x)))
                (.getLanguage x)))

(defn statement-handler [graph-atom]
  (proxy [StatementHandler] []
    (handleStatement [subj pred obj]
                     (swap! graph-atom add-stmts
                            (make-stmt (from-rio subj)
                                       (from-rio pred)
                                       (from-rio obj))))))

(defn- parse-with [parser input base]
  (let [graph-atom (atom (make-graph))]
    (.setStatementHandler parser (statement-handler graph-atom))
    (.parse parser (duck/reader input) base)
    @graph-atom))

(defn load-ntriples
  ([input] (load-ntriples input ""))
  ([input base]
     (parse-with (NTriplesParser.) input base)))

(defn load-rdfxml
  ([input] (load-rdfxml input ""))
  ([input base]
     (parse-with (RdfXmlParser.) input base)))

(defn load-turtle
  ([input] (load-turtle input ""))
  ([input base]
     (parse-with (TurtleParser.) input base)))

