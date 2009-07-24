(ns clojure-rdf.model
  (:refer clojure.set)
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
     (assert (or (string? datatype) (nil? datatype)))
     (struct literal :literal value datatype nil))
  ([value datatype language]
     (assert (string? value))
     (assert (or (string? datatype) (nil? datatype)))
     (assert (or (string? language) (nil? language)))
     (struct literal :literal value datatype language)))

(def *blank-node-counter* (atom 0))

(defn next-blank-node-id []
  (swap! *blank-node-counter* inc))

(defn make-blank-node
  ([] (struct blank-node :blank-node (next-blank-node-id)))
  ([id] (struct blank-node :blank-node id)))

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
  ([] (struct graph :graph #{} nil))
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

(defn literal-data
  "Returns the typed value of the literal.  Types are interpreted by
  the multimethod interpret-datatype."
  [lit]
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

(defn as-resource-or-literal [x]
  (if (resource? x) x
      (as-literal x)))

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

(def #^{:doc "Function that takes any number of arguments and always
     returns true."} any (constantly true))

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
  (make-graph (union (map :stmts graphs))))

(defn graph-intersection
  "Returns a new graph containing the intersection of all statements
  in graphs."
  [& graphs]
  (make-graph (intersection (map :stmts graphs))))

(defn graph-difference
  "Returns a new graph containing the statements in the first graph
  without the statements of the remaining graphs."
  [& graphs]
  (make-graph (difference (map :stmts graphs))))

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
