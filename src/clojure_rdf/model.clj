(ns clojure-rdf.model
  (:import (java.net URI)))

(defstruct resource :type :uri)

(defstruct literal :type :value :datatype :language)

(defstruct blank-node :type :id)

(defstruct stmt :type :subj :pred :obj :id)

(defstruct graph :type :stmts :uri)

(defn resource? [x]
  (and (map? x) (= (:type x) :resource)))

(defn literal? [x]
  (and (map? x) (= (:type x) :literal)))

(defn blank-node? [x]
  (and (map? x) (= (:type x) :blank-node)))

(defn stmt? [x]
  (and (map? x) (= (:type x) :stmt)))

(defn graph? [x]
  (and (map? x) (= (:type x) :graph)))

(defn make-resource [uri]
  (assert (string? uri))
  (struct resource :resource uri))

(defn make-literal
  ([value]
     (assert (string? value))
     (struct literal :literal value nil nil))
  ([value datatype]
     (assert (string? value))
     (assert (string? datatype))
     (struct literal :literal value datatype nil))
  ([value datatype language]
     (assert (string? value))
     (assert (string? datatype))
     (assert (string? language))
     (struct literal :literal value datatype language)))

(def *blank-node-counter* (atom 0))

(defn next-blank-node-id []
  (swap! *blank-node-counter* inc))

(defn make-blank-node []
  (struct blank-node :blank-node (next-blank-node-id)))

(defn make-stmt
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
  ([stmts]
     (assert (set? stmts))
     (assert (every? stmt? stmts))
     (struct graph :graph stmts nil))
  ([stmts uri] (struct graph :graph stmts uri)))

(defmulti interpret-datatype (fn [datatype value] datatype))

(defmethod interpret-datatype "http://www.w3.org/2001/XMLSchema#string"
  [t value] value)
  
(defmethod interpret-datatype "http://www.w3.org/2001/XMLSchema#integer"
  [t value] 
  (let [i (read-string value)]
    (assert (integer? i))
    i))

(defn literal-data [lit]
  (assert (literal? lit))
  (interpret-datatype (:datatype lit) (:value lit)))

(defmulti as-literal type) 

(defmethod as-literal java.util.Map [x]
  (if (literal? x) x
      (throw (Exception. "Tried to create literal from invalid Map."))))

(defmethod as-literal String [x]
  (make-literal x "http://www.w3.org/2001/XMLSchema#string"))

(defmethod as-literal Integer [x]
  (make-literal (str x) "http://www.w3.org/2001/XMLSchema#integer"))

(defn as-resource [x]
  (if (resource? x) x
      (let [u (URI. (str x))]
        (make-resource (str u)))))

(defn as-graph [x]
  (if (graph? x) x
      (if (and (coll? x) (every? stmt? x))
        (make-graph (set x))
        (throw (IllegalArgumentException.
                (str "Cannot make graph from " (class x)))))))

(defn filter-graph
  ([graph f]
     (assert (graph? graph))
     (filter f (:stmts graph)))
  ([graph subj-f pred-f obj-f]
     (filter-graph graph (fn [stmt]
                           (and (subj-f (:subj stmt))
                                (pred-f (:pred stmt))
                                (obj-f (:obj stmt)))))))

(def any (constantly true))

(defn add-stmt [graph stmt]
  (assert (stmt? stmt))
  (assert (graph? graph))
  (assoc graph :stmts (conj (:stmts graph) stmt)))

(defn del-stmt [graph stmt]
  (assert (stmt? stmt))
  (assert (graph? graph))
  (assoc graph :stmts (disj (:stmts graph) stmt)))

(defn contains-stmt? [graph stmt]
  (assert (stmt? stmt))
  (assert (graph? graph))
  (contains? (:stmts graph) stmt))
