(ns #^{:doc "Input/Output in various RDF formats using the Rio
    libraries from openrdf.org.  

    This lib requires the Sesame 1.x JARs on the Java classpath.
    Sesame 2.x will not work!  The JARs from the Sesame 1.x
    distribution are named openrdf-model.jar, openrdf-util.jar,
    rio.jar, and sesame.jar."}
  clojure-rdf.rio
  (:require [clojure.contrib.duck-streams :as duck])
  (:use clojure-rdf.model)
  (:import (org.openrdf.rio StatementHandler)
           (org.openrdf.model URI BNode Literal)
           (org.openrdf.model.impl URIImpl LiteralImpl BNodeImpl)
           (org.openrdf.rio.ntriples NTriplesParser NTriplesWriter)
           (org.openrdf.rio.rdfxml RdfXmlParser RdfXmlWriter)
           (org.openrdf.rio.turtle TurtleParser TurtleWriter)))

(defmulti from-rio class)

(defmethod from-rio BNode [#^BNode x]
  (make-blank-node (.getID x)))

(defmethod from-rio URI [#^URI x]
  (make-resource (.toString x)))

(defmethod from-rio Literal [#^Literal x]
  (make-literal (.getLabel x) (when (.getDatatype x)
                                (.getURI (.getDatatype x)))
                (.getLanguage x)))

(defmulti to-rio :type)

(defmethod to-rio :resource [r]
  (URIImpl. (:uri r)))

(defmethod to-rio :blank-node [b]
  (BNodeImpl. (str (:id b))))

(defmethod to-rio :literal [lit]
  (cond (:datatype lit) (LiteralImpl. (:value lit) (URIImpl. (:datatype lit)))
        (:language lit) (LiteralImpl. (:value lit) (:language lit))
        :else (LiteralImpl. (:value lit))))

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

(defn- write-with [writer graph]
  (.startDocument writer)
  (doseq [stmt (:stmts graph)]
    (.writeStatement writer (to-rio (:subj stmt))
                     (to-rio (:pred stmt))
                     (to-rio (:obj stmt))))
  (.endDocument writer))

(defn write-ntriples [output graph]
  (write-with (NTriplesWriter. (duck/writer output)) graph))

(defn write-rdfxml [output graph]
  (write-with (RdfXmlWriter. (duck/writer output)) graph))

(defn write-turtle [output graph]
  (write-with (TurtleWriter. (duck/writer output)) graph))
