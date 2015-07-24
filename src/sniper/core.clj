(ns sniper.core
  "Defines the core `Form` data type for sniper, which represents a top-level
   Clojure form, and its dependencies and dependents."

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
  "Represents a single clojure form.  Source-info describes where to find it,
    - class-defs and class-refs are Strings representing class names that are
      defined in and referenced in this form (respectively),
    - var-defs and var-refs are symbols representing vars that are defined
      in and referenced (respectively)
    - shadow? indicates whether this form is an form that only exists to support
      its dependents (e.g., a test).

   It is intended that class-defs and var-defs also include things that are not
   strictly defined in this form, but for which this form contributes to their
   definition (e.g. defmethod contributes to a the definition of the multimethod
   var. (Although this is not perfectly captured by the implementation currently.)"
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
