(ns sniper.graph
  "Build and maintain a dependency graph on Forms."
  (:refer-clojure :exclude [ancestors descendants])
  (:use plumbing.core)
  (:require
   [clojure.pprint :as pprint]
   [clojure.set :as set]
   [schema.core :as s]
   [sniper.core :as sniper])
  (:import
   [java.util Map HashMap LinkedHashMap Stack]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Protocol

(defprotocol PDependencyGraph
  (forms [this])
  (add-form [this form] "add a Form to the graph")
  (remove-form [this form] "remove a Form from the graph")
  (callees [this form] "all Forms that this Form directly depends on")
  (callers [this form] "all Forms that direction depend on this Form")
  (minify [this] "build a new graph (with new forms) that excludes all references to refs not defined in g.")
  (strongify [this ref] "remove all forms contribute towards generating a ref from the graph"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helpers

(s/defschema RefMap
  {sniper/Ref #{sniper/Form}})

(defn add-refs [rm form refs]
  (reduce
   (fn [rm r]
     (update rm r (fnil conj #{}) form))
   rm
   refs))

(defn remove-refs [rm form refs]
  (reduce
   (fn [rm r]
     (update rm r (fnil disj #{}) form))
   rm
   refs))

(defn dfs [init f]
  (let [visited (LinkedHashMap.)]
    ((fn visit [n]
       (when-not (.get visited n)
         (.put visited n true)
         (doseq [c (f n)]
           (visit c))))
     init)
    (vec (keys visited))))

(defn scc-graph
  "Take an edge list and return [edge-list node-set-map] for graph of sccs.
   Pretends every node has self-loop.  Clusters returned will be in topological order.

   ex: (= (scc-graph [[1 2] [2 3] [2 4] [4 2]])
          [[([0 1] [0 0] [1 2] [1 1] [2 2]) ; meta-edges
           {0 (1), 1 (2 4), 2 (3)}]])       ; meta-nodes --> old nodes"
  [edges]
  (let [edges (distinct (concat edges (map (fn [x] [x x]) (apply concat edges))))
        pe (merge (into {} (map vector (map second edges) (repeat nil)))
                  (map-vals #(map second %) (group-by first edges)))
        e  (HashMap. ^Map pe)
        re (HashMap. ^Map (map-vals #(map first %) (group-by second edges)))
        s (Stack.)]
    (while (not (.isEmpty e))
      ((fn dfs1 [n]
         (when (.containsKey e n)
           (let [nns (.get e n)]
             (.remove e n)
             (doseq [nn nns] (dfs1 nn)))
           (.push s n)))
       (first (keys e))))
    (let [sccs (into (sorted-map)
                     (indexed
                      (remove empty?
                              (for [n (reverse (seq s))]
                                ((fn dfs2 [n]
                                   (when (.containsKey re n)
                                     (let [nns (.get re n)]
                                       (.remove re n)
                                       (cons n (apply concat (doall (map dfs2 nns)))))))
                                 n)))))
          rev-sccs (into {} (for [[k vs] sccs, v vs] [v k]))]
      [(distinct
        (for [[scc nodes] sccs
              node nodes
              outgoing (get pe node)
              :let [n-scc (get rev-sccs outgoing),
                    _     (assert (<= scc n-scc))]]
          [scc n-scc]))
       sccs])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Implementation

(declare dependency-graph ancestors descendants)

(s/defrecord DependencyGraph
    [forms :- #{sniper/Form}
     definers :- RefMap
     referers :- RefMap]
  PDependencyGraph

  (forms [this]
    (sort-by (comp (juxt :file :line) :source-info) forms))

  (add-form [this f]
    (DependencyGraph.
     (conj forms f)
     (add-refs definers f (sniper/definitions f))
     (add-refs referers f (sniper/references f))))

  (remove-form [this f]
    (assert (contains? forms f)
            (str f))
    (DependencyGraph.
     (disj forms f)
     (remove-refs definers f (sniper/definitions f))
     (remove-refs referers f (sniper/references f))))

  (callees [this f]
    (->> f
         sniper/references
         (map definers)
         (apply set/union)
         (<- (disj f))))

  (callers [this f]
    (->> f
         sniper/definitions
         (map referers)
         (apply set/union)
         (<- (disj f))))

  (strongify [this r]
    (->> (definers r)
         (mapcat #(descendants this %))
         distinct
         (reduce remove-form this)))

  (minify [this]
    (let [valid-refs (->> definers (filter (comp seq val)) (map key) set)]
      (dependency-graph
       (for [f forms]
         (-> f
             (update :var-refs #(vec (filter valid-refs %)))
             (update :class-refs #(vec (filter valid-refs %)))))))))

(defn ancestors
  "depth-first graph traversal of ancestors starting at f"
  [g f]
  (dfs f #(callers g %)))

(defn descendants
  "depth-first graph traversal of descendants starting at f"
  [g f]
  (dfs f #(callees g %)))

(defn unused?
  "Is this unused, except possibly by shadow forms?"
  [g f]
  (every? sniper/shadow? (callers g f)))

(s/defn leaf-components :- [[sniper/Form]]
  "A list of strongly connected non-shadow leaf components."
  [g]
  (let [[scc-edges scc-nodes] (->> g
                                   forms
                                   (remove sniper/shadow?)
                                   (mapcat (fn [f] (for [c (cons :none (callees g f))] [f c])))
                                   scc-graph)
        non-loop-scc-edges (remove (fn [[s d]] (= s d)) scc-edges)
        leaf-sccs (apply disj (set (map first non-loop-scc-edges))
                         (map second non-loop-scc-edges))]
    (map #(safe-get scc-nodes %) leaf-sccs)))

(defn dependency-graph
  ([] (->DependencyGraph #{} {} {}))
  ([forms]
     (reduce add-form (dependency-graph) forms)))

(def pprint-graph (comp pprint/pprint forms))

(defmethod print-method DependencyGraph [g writer]
  (print-method (forms g) writer))




(comment
  (->> (sniper.snarf/ns-forms 'sniper.snarf)
       sniper.graph/dependency-graph
       sniper.graph/minify
       :forms
       (mapv sniper.core/pretty-form)
       pprint)
  )
