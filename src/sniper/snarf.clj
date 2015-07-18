(ns sniper.snarf
  "Snarf in namespaces and extract sniper.core/Forms with dependency info"
  (:use plumbing.core)
  (:require
   [clojure.java.classpath :as classpath]
   [clojure.java.io :as java-io]
   [clojure.tools.analyzer.env :as env]
   [clojure.tools.analyzer.jvm :as jvm]
   [clojure.tools.analyzer.jvm.utils :as jvm-utils]
   [clojure.tools.analyzer.passes.jvm.validate :as jvm-val]
   [clojure.tools.analyzer.ast :as ast]
   [clojure.tools.reader :as reader]
   [clojure.tools.reader.reader-types :as reader-types]
   [schema.core :as s]
   [sniper.core :as sniper]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Copied from tools.analyzer.jvm master for EOF fix.
;; also modified to take file.

(defn analyze-url
  "Analyzes a url, returns a vector of the ASTs for all the
   top-level ASTs of that file.
   Evaluates all the forms.
   Disables wrong-tag-handler, and fixes bug with opts shadowing.
"
  ([url] (println "ZZZZ") (analyze-url url (jvm/empty-env)))
  ([url env] (analyze-url url env {:passes-opts
                                   (merge
                                    jvm/default-passes-opts
                                    {:validate/wrong-tag-handler
                                     (fn [_ ast]
                                       (println "Wrong tag: " (-> ast :name meta :tag)
                                                " in def: " (:name ast)))})}))
  ([^java.net.URL url env opts]
     (println "AAA")
     (println "Analyzing file"  #_(.getPath url))
     (env/ensure (jvm/global-env)
                 (let [res url]
                   (assert res (str "Can't find " url " in classpath"))
                   (let [filename (str res)
                         path     (.getPath res)]
                     (when-not (get-in (env/deref-env) [::analyzed-clj path])
                       (binding [*ns*   (the-ns 'api.deploy) #_*ns*
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
                                 (if (identical? form eof)
                                   ret
                                   (recur
                                    (conj ret (jvm/analyze form (assoc env :ns (ns-name *ns*)) opts))))))
                             #_(loop []
                                 (let [form (reader/read read-opts pbr)]
                                   (when-not (identical? form eof)
                                     (swap! env/*env* update-in [::analyzed-clj path]
                                            (fnil conj [])
                                            (jvm/analyze+eval form (assoc env :ns (ns-name *ns*)) opts))
                                     (recur))))))))
                     #_(get-in @env/*env* [::analyzed-clj path]))))))

(defn analyze-ns
  "Analyzes a whole namespace."
  ([ns] (analyze-url (jvm-utils/ns-url ns)))
  ([ns env] (analyze-url (jvm-utils/ns-url ns) env))
  ([ns env opts]
     (analyze-url (jvm-utils/ns-url ns))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Private helpers: extracting definitions and references from forms

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public

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
  [& nss :- [clojure.lang.Symbol]]
  (keep normalized-form (mapcat analyze-ns nss)))

(s/defn file-forms :- [sniper/Form]
  [& fs :- [String]]
  (keep normalized-form (mapcat #(analyze-url (java.net.URL. (str "file://" %))) fs)))


(defn files-in-dir
  "Returns a sequence of namespaces defined in clj(s) files in directory including subdirs"
  [base-dir]
  (->> (file-seq (java-io/file base-dir))
       (remove #(.isDirectory ^java.io.File %))
       (filter #(re-find #"\.clj" (.getName ^java.io.File %)))))

(defn ns-sym->filename [ns-sym]
  (let [pubs (-> ns-sym the-ns ns-publics)
        metas (map (comp meta val) pubs)
        ns-metas (filter #(when-let [ns (:ns %)] (= (ns-name ns) ns-sym)) metas)
        f (first (keep :file ns-metas))]
    (when-not f
      (throw (RuntimeException. (format "Couldn't find file for ns %s" ns-sym))))
    f))

(defn loaded-namespaces [file-regex]
  (filter #(re-find file-regex (ns-sym->filename %)) (map ns-name (all-ns))))

(defn resolve-filename [^String f]
  (println f)
  (if (.startsWith f "/")
    f
    (.getFile (.getResource (clojure.lang.RT/baseLoader) f))))

(defn loaded-clojure-files [file-regex]
  (->> (all-ns)
       (mapcat #(vals (ns-map %)))
       (keep #(:file (meta %)))
       (remove (fn [^String f] (or (= "NO_SOURCE_FILE" f) (.startsWith f "jar:file:"))))
       distinct
       (map resolve-filename)
       (filter #(re-find file-regex %))))

(defn classpath-clojure-files [file-regex]
  (->> (classpath/classpath)
       (mapcat file-seq)
       distinct
       (remove #(.isDirectory ^java.io.File %))
       (map #(.getPath ^java.io.File %))
       (filter #(and (re-find file-regex %) (.endsWith ^String % ".clj")))))

;; TODO: get all files on classpath including tests.

;; TODO: use ordinary require and no eval, just forget about non-ns files

(comment
  (sniper.snarf/classpath-clojure-files #"^/Users/w01fe/prismatic")
  (doseq [f (sniper.snarf/classpath-clojure-files #"^/Users/w01fe/prismatic")] (try (load-file f) (catch Exception e)))
  )
