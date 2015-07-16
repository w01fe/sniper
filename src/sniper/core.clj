(ns sniper.core
  (:use plumbing.core)
  (:require
   [schema.core :as s]))

(s/defschema SourceInfo
  {:file String
   :ns clojure.lang.Symbol
   :line s/Int
   :column s/Int
   (s/optional-key :end-column) s/Int
   (s/optional-key :end-line) s/Int})

(s/defschema Klass (s/named String "class name"))
(s/defschema Var (s/named clojure.lang.Symbol "namespace-qualified symbol"))

(s/defschema Form
  {:source-info SourceInfo
   :class-defs [Klass]
   :class-refs [Klass]
   :var-defs [Var]
   :var-refs [Var]
   :shadow? Boolean})

(s/defschema Ref (s/either Var Klass))

(defn shadow? [f]
  (safe-get f :shadow?))

(defnk definitions :- [Ref]
  [class-defs var-defs :as form]
  (concat class-defs var-defs))

(defnk references :- [Ref]
  [class-refs var-refs :as form]
  (concat class-refs var-refs))
