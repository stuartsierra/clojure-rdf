(ns clojure-rdf.model
  (:refer clojure.set)
  (:import (java.net URI)))

(defstruct resource :type :uri)

(defstruct literal :type :value :datatype :language)

(defstruct blank-node :type :id)

(defstruct stmt :type :subj :pred :obj :id)

(defstruct graph :type :stmts :uri)

(defn resource?
  "Returns true if x is a resource (not a blank node)."
  [x]
  (and (map? x) (= (:type x) :resource)))

(defn literal?
  "Returns true if x is a literal."
  [x]
  (and (map? x) (= (:type x) :literal)))

(defn blank-node?
  "Returns true if x is a blank-node."
  [x]
  (and (map? x) (= (:type x) :blank-node)))

(defn stmt?
  "Returns true if x is a statement."
  [x]
  (and (map? x) (= (:type x) :stmt)))

(defn graph?
  "Returns true if x is a graph."
  [x]
  (and (map? x) (= (:type x) :graph)))

(defn make-resource
  "Low-level resource constructor. uri is a String."
  [uri]
  (assert (string? uri))
  (struct resource :resource uri))

(defn make-literal
  "Low-level literal constructor.  All arguments are Strings.  If
  language is provided, then datatype must be nil."
  ([value]
     (assert (string? value))
     (struct literal :literal value nil nil))
  ([value datatype]
     (assert (string? value))
     (assert (or (string? datatype) (nil? datatype)))
     (struct literal :literal value datatype nil))
  ([value datatype language]
     (assert (string? value))
     (assert (or (string? datatype) (nil? datatype)))
     (assert (or (string? language) (nil? language)))
     ;; may not have both datatype & language
     (assert (or (nil? datatype) (nil? language)))
     (struct literal :literal value datatype language)))

(def *blank-node-counter* (atom 0))

(defn next-blank-node-id []
  (swap! *blank-node-counter* inc))

(defn make-blank-node
  "Blank node constructor.  Optional argument specifies a String
  identifier for the node."
  ([] (struct blank-node :blank-node (str \b (next-blank-node-id))))
  ([id] (struct blank-node :blank-node id)))

(defn make-stmt
  "Statement constructor."
  ([subj pred obj]
     (assert (or (resource? subj) (blank-node? subj)))
     (assert (resource? pred))
     (assert (or (resource? obj) (blank-node? obj) (literal? obj)))
     (struct stmt :stmt subj pred obj nil))
  ([subj pred obj id]
     (assert (or (resource? subj) (blank-node? subj)))
     (assert (or (resource? pred) (blank-node? pred)))
     (assert (or (resource? obj) (blank-node? obj) (literal? obj)))
     (assert (string? id))
     (struct stmt :stmt subj pred obj id)))

(defn make-graph
  "Graph constructor."
  ([] (struct graph :graph #{} nil))
  ([stmts]
     (assert (set? stmts))
     (assert (every? stmt? stmts))
     (struct graph :graph stmts nil))
  ([stmts uri] (struct graph :graph stmts uri)))

(defmulti
  #^{:doc "Converts a String literal value to a Java object based on
   its RDF datatype."}
  interpret-datatype (fn [datatype value] datatype))

(defmethod interpret-datatype "http://www.w3.org/2001/XMLSchema#string"
  [t value] value)
  
(defmethod interpret-datatype "http://www.w3.org/2001/XMLSchema#integer"
  [t value] 
  (let [i (read-string value)]
    (assert (integer? i))
    i))

(defn literal-data
  "Returns the typed value of the literal.  Types are interpreted by
  the multimethod interpret-datatype."
  [lit]
  (assert (literal? lit))
  (interpret-datatype (:datatype lit) (:value lit)))

(defmulti #^{:doc "Converts a Java object to an RDF literal based on its type."}
  as-literal type) 

(defmethod as-literal java.util.Map [x]
  (if (literal? x) x
      (throw (Exception. "Cannot create RDF literal from java.util.Map."))))

(defmethod as-literal String [x]
  (make-literal x "http://www.w3.org/2001/XMLSchema#string"))

(defmethod as-literal Integer [x]
  (make-literal (str x) "http://www.w3.org/2001/XMLSchema#integer"))

(defn as-resource
  "If x is a resource or blank node, returns it; otherwise tries to
  create a resource with x as the URI."
  [x]
  (if (or (resource? x) (blank-node? x)) x
      (let [u (URI. (str x))]
        (make-resource (str u)))))

(defn as-resource-or-literal
  "If x is a resource or blank node, returns it; otherwise tries to
  create a literal using as-literal."
  [x]
  (if (or (resource? x) (blank-node? x)) x
      (as-literal x)))

(defn as-graph
  "If x is a graph, returns it; otherwise if x is a set of statements,
  creates a graph."
  [x]
  (if (graph? x) x
      (if (and (coll? x) (every? stmt? x))
        (make-graph (set x))
        (throw (IllegalArgumentException.
                (str "Cannot make graph from " (class x)))))))

(defn filter-graph
  "With 2 arguments, returns a sequence of statements for which f is true.
  With 4 arguments, returns a sequence of statements for
  which (and (subj-f subject) (pred-f predicate) (obj-f object)) is
  true."
  ([graph f]
     (assert (graph? graph))
     (filter f (:stmts graph)))
  ([graph subj-f pred-f obj-f]
     (filter-graph graph (fn [stmt]
                           (and (subj-f (:subj stmt))
                                (pred-f (:pred stmt))
                                (obj-f (:obj stmt)))))))

(def #^{:doc "Function that takes any number of arguments and always
     returns true.  Used as shorthand in filter-graph."}
     any (constantly true))

(defn add-stmts
  "Returns a copy of graph with stmt added."
  [graph & stmts]
  (assert (every? stmt? stmts))
  (assert (graph? graph))
  (assoc graph :stmts (union (:stmts graph) (set stmts))))

(defn del-stmts
  "Returns a copy of graph with stmts removed."
  [graph & stmts]
  (assert (every? stmt? stmt))
  (assert (graph? graph))
  (assoc graph :stmts (difference (:stmts graph) (set stmts))))

(defn contains-stmt?
  "Returns true if graph contains stmt."
  [graph stmt]
  (assert (stmt? stmt))
  (assert (graph? graph))
  (contains? (:stmts graph) stmt))

(defn graph-size
  "Returns the number of statements in the graph."
  [graph]
  (count (:stmts graph)))

(defn graph-union
  "Returns a new graph containing the union of all statements in
  graphs."
  [& graphs]
  (make-graph (apply union (map :stmts graphs))))

(defn graph-intersection
  "Returns a new graph containing the intersection of all statements
  in graphs."
  [& graphs]
  (make-graph (apply intersection (map :stmts graphs))))

(defn graph-difference
  "Returns a new graph containing the statements in the first graph
  without the statements of the remaining graphs."
  [& graphs]
  (make-graph (apply difference (map :stmts graphs))))

(defn subject-map
  "Returns a map of properties of r, a resource.  The value for each
  key in the map is the set of objects appearing in statements with r
  as the subject and the key as the predicate."
  [graph r]
  (reduce (fn [m stmt]
            (if (= r (:subj stmt))
              (assoc m (:pred stmt) (conj (or (get m (:pred stmt)) #{})
                                          (:obj stmt)))
              m))
          {} (:stmts graph)))

(defn describe
  "Returns a new graph describing subject.  subject is converted with
  as-resource.  properties are predicate/value pairs, converted with
  as-resource and as-resource-or-literal, respectively.

  If a value is a vector, the first element becomes the object of the
  current statement, and describe is applied recursively to the
  vector and merged with the current graph."
  [subject & properties]
  (assert (even? (count properties)))
  (let [subj (as-resource subject)]
    (make-graph (apply union
                       (map (fn [[pred obj]]
                              (if (vector? obj)
                                (conj (:stmts (apply describe obj))
                                      (make-stmt subj (as-resource pred)
                                                 (as-resource-or-literal (first obj))))
                                #{(make-stmt subj (as-resource pred)
                                             (as-resource-or-literal obj))}))
                            (partition 2 properties))))))

;; bootstrap these resources befor loading clojure-rdf.vocabs.rdf
(def rdf-first (make-resource "http://www.w3.org/1999/02/22-rdf-syntax-ns#first"))
(def rdf-rest (make-resource "http://www.w3.org/1999/02/22-rdf-syntax-ns#rest"))
(def rdf-nil (make-resource "http://www.w3.org/1999/02/22-rdf-syntax-ns#nil"))

(defn rdf-list
  "Returns a pair [head s] where s is a sequence of statements
  defining an RDF List containing elements and head is the blank node
  at the head of the List."
  [& elements]
  (loop [stmts (list)
         elems elements
         head nil]
    (if (seq elems)
      (let [node (make-blank-node)]
        (if head
          (recur (cons (make-stmt node rdf-first (first elems))
                       (cons (make-stmt (:subj (first stmts)) rdf-rest node)
                               stmts))
                 (next elems)
                 head)
          (recur (cons (make-stmt node rdf-first (first elems)) stmts)
                 (next elems)
                 node)))
      ;; no more elements
      (if head
        [head (cons (make-stmt (:subj (first stmts)) rdf-rest rdf-nil) stmts)]
        [rdf-nil (list)]))))
