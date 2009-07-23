(ns clojure-rdf.jena
  (:refer-clojure :exclude (load))
  (:use clojure-rdf.model)
  (:require [clojure.contrib.duck-streams :as duck])
  (:import (com.hp.hpl.jena.rdf.arp ARP ARPHandlers StatementHandler
                                    AResource ALiteral)))

(defmulti from-jena class)

(defmethod from-jena AResource [#^AResource r]
  (if (.isAnonymous r)
    (make-blank-node (.getAnonymousID r))
    (make-resource (.getURI r))))

(defmethod from-jena ALiteral [#^ALiteral lit]
  (make-literal (.toString lit) (.getDatatypeURI lit)
                (let [lang (.getLang lit)]
                  (when-not (empty? lang) lang))))

(defn arp-statement-handler [graph-atom]
  (proxy [StatementHandler] []
    (statement [subj pred obj]
               (swap! graph-atom add-stmt (make-stmt (from-jena subj)
                                          (from-jena pred)
                                          (from-jena obj))))))

(defn load
  ([input] (load input nil))
  ([input base]
     (let [graph (atom (make-graph))
           arp (ARP.)
           handlers (.getHandlers arp)]
       (.setStatementHandler handlers (arp-statement-handler graph))
       (with-open [in (duck/reader input)]
         (if base
           (.load arp in base)
           (.load arp in)))
       @graph)))
