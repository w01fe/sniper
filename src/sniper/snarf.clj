(ns sniper.snarf
  "Snarf in namespaces and extract sniper.core/Forms with dependency info"
  (:use plumbing.core)
  (:require
   [clojure.java.io :as io]
   [clojure.tools.analyzer.env :as env]
   [clojure.tools.analyzer.jvm :as jvm]
   [clojure.tools.analyzer.jvm.utils :as jvm-utils]
   [clojure.tools.analyzer.ast :as ast]
   [clojure.tools.reader :as reader]
   [clojure.tools.reader.reader-types :as reader-types]
   [schema.core :as s]
   [sniper.core :as sniper]))

;; Copied from master for EOF fix., TODO delete once a new release is out
(defn analyze-ns
  "Analyzes a whole namespace, returns a vector of the ASTs for all the
   top-level ASTs of that namespace.
   Evaluates all the forms."
  ([ns] (analyze-ns ns (jvm/empty-env)))
  ([ns env] (analyze-ns ns env {}))
  ([ns env opts]
     (env/ensure (jvm/global-env)
                 (let [res ^java.net.URL (jvm-utils/ns-url ns)]
                   (assert res (str "Can't find " ns " in classpath"))
                   (let [filename (str res)
                         path     (.getPath res)]
                     (when-not (get-in (env/deref-env) [::analyzed-clj path])
                       (binding [*ns*   *ns*
                                 *file* filename]
                         (with-open [rdr (io/reader res)]
                           (let [pbr (reader-types/indexing-push-back-reader
                                      (java.io.PushbackReader. rdr) 1 filename)
                                 eof (Object.)
                                 opts {:eof eof :features #{:clj :t.a.jvm}}
                                 opts (if (.endsWith filename "cljc")
                                        (assoc opts :read-cond :allow)
                                        opts)]
                             (loop []
                               (let [form (reader/read opts pbr)]
                                 (when-not (identical? form eof)
                                   (swap! env/*env* update-in [::analyzed-clj path]
                                          (fnil conj [])
                                          (jvm/analyze+eval form (assoc env :ns (ns-name *ns*)) opts))
                                   (recur))))))))
                     (get-in @env/*env* [::analyzed-clj path]))))))

(defn class-name [^Class c] (.getName c))

(defn gen-interface-class [f]
  (when (= (first f) 'clojure.core/gen-interface)
    [(name (nth f 2))]))

(defn protocol-gen-interface-form [node]
  (when (= (:type node) :class) (-> node :raw-forms first)))

(defnk class-defs
  "TODO: Currently works for types but not interfaces or records."
  [op :as node]
  (case op
    (:deftype) [(class-name (safe-get node :class-name))]
    (:import) (-> node :raw-forms first next next first gen-interface-class) ;; possibly definterface
    (:const) (-> node protocol-gen-interface-form gen-interface-class) ;; possibly defprotocol
    nil))

(defnk class-refs [op :as node]
  (case op
    (:const) (when (= (:type node) :class) [(class-name (safe-get node :val))])
    nil))

(defn var->symbol [v]
  (letk [[ns [:name :as var-name]] (meta v)]
    (symbol (name (ns-name ns)) (name var-name))))

(defn defprotocol-vars [f ns]
  (when (= (first f) 'clojure.core/gen-interface)
    (for [[m] (nth f 4)]
      (symbol (name ns) (name m)))))

(defnk var-defs [op :as node]
  (case op
    (:def) [(var->symbol (safe-get node :var))]
    (:const) (-> node protocol-gen-interface-form (defprotocol-vars (safe-get-in node [:env :ns])))
    nil))

(defnk var-refs [op :as node]
  (case op
    (:var :the-var) [(var->symbol (safe-get node :var))]
    nil))

(s/defn ^:always-validate normalized-form :- (s/maybe sniper/Form)
  [ast-node]
  (let [nodes (ast/nodes ast-node)
        unique (fn [f] (sort-by str (distinct (mapcat f nodes))))]
    (if-not (get-in ast-node [:env :line])
      (do (assert (= (ffirst (:raw-forms ast-node)) 'comment))
          nil)
      {:source-info (select-keys (:env ast-node) [:file :ns :line :column :end-column :end-line])
       :class-defs (unique class-defs)
       :class-refs (unique class-refs)
       :var-defs (unique var-defs)
       :var-refs (unique var-refs)
       :shadow? false})))

(s/defn ns-forms :- [sniper/Form]
  [ns :- clojure.lang.Symbol]
  (keep normalized-form (analyze-ns ns)))
