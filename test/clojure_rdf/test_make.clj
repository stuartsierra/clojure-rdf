(ns clojure-rdf.test-make
  (:use clojure.test clojure-rdf.model))

(def *alan* "http://dbpedia.org/resource/Alan_Turing")

(def *string* "http://www.w3.org/2001/XMLSchema#string")

(def *almaMater* "http://dbpedia.org/property/almaMater")

(def *princeton* "http://dbpedia.org/resource/Princeton_University")

(deftest t-make-resource
  (is (resource? (make-resource *alan*))))

(deftest t-make-blank-node
  (is (blank-node? (make-blank-node)))
  (is (blank-node? (make-blank-node "bnode101"))))

(deftest t-make-literal
  (is (literal? (make-literal "Hello, World!")))
  (is (literal? (make-literal "Hello, World!" *string*)))
  (is (literal? (make-literal "Hello, World!" nil "en"))))

(deftest t-make-stmt
  (is (stmt? (make-stmt (make-resource *alan*)
                        (make-resource *almaMater*)
                        (make-resource *princeton*)))))

(deftest t-make-graph
  (is (graph? (make-graph)))
  (is (graph? (make-graph #{})))
  (is (graph? (make-graph #{(make-stmt (make-resource *alan*)
                                       (make-resource *almaMater*)
                                       (make-resource *princeton*))}))))
