(ns sniper.snarf
  "Snarf in namespaces and extract sniper.core/Forms with dependency info.
   Main entry point is `classpath-ns-forms`."
  (:use plumbing.core)
  (:require
   [clojure.java.classpath :as classpath]
   [clojure.java.io :as java-io]
   [clojure.tools.analyzer.ast :as ast]
   [clojure.tools.analyzer.env :as env]
   [clojure.tools.analyzer.jvm :as jvm]
   [clojure.tools.analyzer.jvm.utils :as jvm-utils]
   [clojure.tools.namespace.find :as namespace-find]
   [clojure.tools.reader :as reader]
   [clojure.tools.reader.reader-types :as reader-types]
   [schema.core :as s]
   [sniper.core :as sniper]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Copied and modified from tools.analyzer.jvm master

(defn try-analyze [form env opts]
  (try (jvm/analyze form env opts)
       (catch Throwable t
         (println t)
         (println "WARNING: skipping form: " form))))

(defn analyze-ns
  "Analyzes a whole namespace. returns a vector of the ASTs for all the
   top-level ASTs of that file.
   Evaluates all the forms.
   Disables wrong-tag-handler, and fixes bug with opts shadowing,
   and doesn't eval."
  ([ns] (analyze-ns ns (jvm/empty-env)))
  ([ns env] (analyze-ns ns env {:passes-opts
                                (merge
                                 jvm/default-passes-opts
                                 {:validate/wrong-tag-handler
                                  (fn [_ ast]
                                    #_(println "Wrong tag: " (-> ast :name meta :tag)
                                               " in def: " (:name ast)))})}))
  ([ns env opts]
     (println "Analyzing ns" ns)
     (env/ensure
      (jvm/global-env)
      (let [res ^java.net.URL (jvm-utils/ns-url ns)]
        (assert res (str "Can't find " ns " in classpath"))
        (let [filename (str res)
              path     (.getPath res)]
          (when-not (get-in (env/deref-env) [::analyzed-clj path])
            (binding [*ns*   (the-ns ns)
                      *file* filename]
              (with-open [rdr (java-io/reader res)]
                (let [pbr (reader-types/indexing-push-back-reader
                           (java.io.PushbackReader. rdr) 1 filename)
                      eof (Object.)
                      read-opts {:eof eof :features #{:clj :t.a.jvm}}
                      read-opts (if (.endsWith filename "cljc")
                                  (assoc read-opts :read-cond :allow)
                                  read-opts)]
                  (loop [ret []]
                    (let [form (reader/read read-opts pbr)]
                      ;;(println "\n\n" form)
                      (if (identical? form eof)
                        (remove nil? ret)
                        (recur
                         (conj ret (try-analyze form (assoc env :ns (ns-name *ns*)) opts)))))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Private helpers: extracting definitions and references from forms

(defn class-name [^Class c] (.getName c))

(defn gen-interface-class [f]
  (when (= (first f) 'clojure.core/gen-interface)
    [(name (nth f 2))]))

(defn protocol-gen-interface-form [node]
  (when (= (:type node) :class) (-> node :raw-forms first)))

(defnk class-defs
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
    (:instance-call) (when (and (= 'addMethod (:method node)) ;; defmethod is part of multi var def.
                                (= clojure.lang.MultiFn (:class node)))
                       [(var->symbol (safe-get-in node [:instance :var]))])
    nil))

(defnk var-refs [op :as node]
  (case op
    (:var :the-var) [(var->symbol (safe-get node :var))]
    nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public

(s/defn ^:always-validate normalized-form :- (s/maybe sniper/Form)
  "Convert a top-level analyzer AST node into a sniper/Form.
   May return nil for comment forms (which are often missing source info)."
  [ast-node]
  (let [nodes (ast/nodes ast-node)
        unique (fn [f] (sort-by str (distinct (mapcat f nodes))))]
    (if-not (get-in ast-node [:env :line])
      (do (assert (= (ffirst (:raw-forms ast-node)) 'comment)
                  (str "MISSING line" (:raw-forms ast-node) (:env ast-node)))
          nil)
      {:source-info (select-keys (:env ast-node) [:file :ns :line :column :end-column :end-line])
       :class-defs (unique class-defs)
       :class-refs (unique class-refs)
       :var-defs (unique var-defs)
       :var-refs (unique var-refs)
       :shadow? false})))

(def +analysis-cache-file+ ".sniper-analysis-cache.clj")

(defonce +analysis-cache+
  (atom (try (read-string (slurp +analysis-cache-file+)) (catch Exception e {}))))

(defn cached-ns-forms
  [ns]
  (let [c (slurp (jvm-utils/ns-url ns))]
    (or (@+analysis-cache+ c)
        (let [res (vec (keep normalized-form (analyze-ns ns)))]
          (swap! +analysis-cache+ assoc c res)
          res))))

(s/defn ns-forms :- [sniper/Form]
  "Get a flat sequence of forms for all namespaces in nss."
  [& nss :- [clojure.lang.Symbol]]
  (apply concat (pmap cached-ns-forms nss)))


(defn classpath-namespaces [dir-regex]
  "Get a sequence of all namespaces on classpath that match dir-regex."
  (->> (classpath/classpath)
       (mapcat file-seq)
       distinct
       (filter (fn [^java.io.File f]
                 (and (.isDirectory f)
                      (re-find dir-regex (.getPath f)))))
       (mapcat #(namespace-find/find-namespaces-in-dir %))
       distinct
       sort))

(s/defn classpath-ns-forms :- [sniper/Form]
  [dir-regex]
  "Get a flat sequence of forms for all namespaces on the classpath matching
   dir-regex."
  (let [nss (classpath-namespaces dir-regex)]
    (println "Requiring" nss)
    (apply require nss)
    (let [res (apply ns-forms nss)]
      (future (spit +analysis-cache-file+ @+analysis-cache+))
      res)))
