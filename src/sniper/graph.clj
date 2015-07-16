(ns sniper.graph
  "Build and maintain a dependency graph on Forms."
  (:refer-clojure :exclude [ancestors descendants])
  (:use plumbing.core)
  (:require
   [clojure.pprint :as pprint]
   [clojure.set :as set]
   [schema.core :as s]
   [sniper.core :as sniper]))

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
  (let [visited (java.util.LinkedHashMap.)]
    ((fn visit [n]
       (when-not (.get visited n)
         (.put visited n true)
         (doseq [c (f n)]
           (visit c))))
     init)
    (vec (keys visited))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Implementation

(declare dependency-graph ancestors descendants)

(s/defrecord DependencyGraph
    [forms :- #{sniper/Form}
     definers :- RefMap
     referers :- RefMap]
  PDependencyGraph

  (forms [this] forms)

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


(defn dependency-graph
  ([] (->DependencyGraph #{} {} {}))
  ([forms]
     (reduce add-form (dependency-graph) forms)))

(defn pretty-graph [g]
  (sort-by (comp (juxt :file :line) :source-info) (:forms g)))

(def pprint-graph (comp pprint/pprint pretty-graph))

(defmethod print-method DependencyGraph [g writer]
  (print-method (pretty-graph g) writer))


(comment
  (->> (sniper.snarf/ns-forms 'sniper.snarf)
       sniper.graph/dependency-graph
       sniper.graph/minify
       :forms
       (mapv sniper.core/pretty-form)
       pprint)
  )
