(ns sniper.scope
  (:use plumbing.core)
  (:require
   [clojure.java.shell :as shell]
   [schema.core :as s]
   [sniper.core :as sniper]
   [sniper.snarf :as snarf]
   [sniper.graph :as graph]))

;; Basic rough flow:
;;  - get all deps
;;  - flag shadow
;;  - strongify all in strong set and regex
;;  - find unused form  (show w/ shadow refs)
;;    - if user marks strong, remove from graph and add to strong set
;;      - go to next unused node
;;    - if user marks deleted,
;;      - travel through all shadow refs and enforce deletion.
;;      - if any callees have unused ancestors set, move them up
;;         (?)
;;      - if any callees are now unused, push to front.

;; todo: figure out how to handle mutating forms, currently prevent
;;   removal of anything dynamically extended.
;;   (they need to count as definitions also)
;; (this may be solved by treating them as shadow).
;; (sort-of, results in helpers called from methods being thought dead.
;;  now fixed for defmethod.)

;; TODO: shamalan-mode, coloring for strong set, depth in scc-graph.
;; for things in strong setk see centrality and roots.

;; TODO: orphaned test forms
;; TODO: untested forms.
;; TODO: see path to strong root, etc
;;  (strong is just a special 'form'
;; TODO: watch files and update
;; TODO: dead files
;; TODO: strong root list.

(defn prepare-forms [test-regex forms]
  (for [f forms]
    (if (or (empty? (sniper/definitions f))
            (re-find test-regex (safe-get-in f [:source-info :file])))
      (assoc f :shadow? true)
      f)))

(def +strong-root-var-regexes+
  [#"deploy/prod"
   #"/\$"
   #"crane.task/"])

(defonce +manual-strong-set+
  (atom #{}))

(defn strong-ref? [r]
  (or (@+manual-strong-set+ r)
      (and (symbol? r)
           (let [s (str r)]
             (some #(re-find % s) +strong-root-var-regexes+)))))

(s/defschema KillableForm
  {:type (s/enum :leaf :leaf-cycle :collatoral)
   :form sniper/Form
   (s/optional-key :cause) (s/named sniper/Form "Killed form that made this collatoral")})

(s/defschema State
  "State of the sniping process."
  {:graph sniper.graph.DependencyGraph
   :add-strong! (s/=> s/Any sniper/Form)
   :stack [KillableForm]})

(def +state+ (atom nil))

(defn aim []
  #_(map (fn-> (update :form sniper/definitions) (update-in-when [:cause] sniper/definitions))
         (:stack @+state+))
  (first (:stack @+state+)))

(defn ref-count [s]
  (letk [[exit ^String out] (shell/sh "ag" "-Q" s "/Users/w01fe/prismatic/")]
    (if (= exit 1)
      0
      (count (.split out "\n")))))

(defnk ref-counts [var-defs class-defs]
  (for-map [s (concat
               (map name var-defs)
               (map #(last (.split ^String % "\\.")) class-defs))
            :let [c (ref-count s)]
            :when (pos? c)]
    s c))

(defnk aim->el [[:form [:source-info ^String file line column] var-defs :as form] type {cause nil}]
  (assert (.startsWith file "file:"))
  (list (subs file 5)
        line
        column
        (concat [type (first var-defs) (ref-counts form)] (when cause [cause]))))

(defn start! [forms strong-ref? add-strong!]
  (let [g (graph/dependency-graph forms)
        g (graph/minify
           (reduce graph/strongify g (filter strong-ref? (keys (safe-get g :definers)))))]
    (reset!
     +state+
     {:graph g
      :add-strong! add-strong!
      :stack (keep
              (fn [forms]
                (when-let [f (first (filter #(seq (sniper/definitions %)) forms))]
                  {:form f
                   :type (if (next forms) :leaf-cycle :leaf)}))
              (graph/leaf-components g))}))
  (aim))

(defn spare! []
  (letk [[graph add-strong! stack :as state] @+state+
         form (safe-get (first stack) :form)
         defs (sniper/definitions form)]
    (doseq [d defs] (add-strong! d))
    (reset! +state+
            (assoc state
              :graph (reduce graph/strongify graph defs)
              :stack (next stack))))
  (aim))

(defn fired! []
  (letk [[graph stack :as state] @+state+
         form (:form (first stack))
         new-g (graph/remove-form graph form)]
    (reset! +state+
            (assoc state
              :graph new-g
              :stack (distinct-by
                      :form
                      (concat
                       (for [f (next (graph/ancestors graph form))]
                         {:type :collatoral
                          :form f
                          :cause form})
                       ;; TODO: this won't find new leaf cycles, for now just assume you restart
                       ;; now and again for that purpose.
                       (keep #(when (graph/unused? new-g %)
                                {:type :leaf :form %})
                             (graph/callees graph form))
                       (next stack))))))
  (aim))


(s/defn dead-namespaces :- [clojure.lang.Symbol]
  "Return all namespaces where no forms are referenced from outside"
  [forms]
  ;; TODO
  )



(comment
  (def f (vec (sniper.snarf/classpath-ns-forms #"^/Users/w01fe/prismatic")))
  (sniper.scope/start!
   (sniper.scope/prepare-forms #"/test/" f)
   sniper.scope/strong-ref?
   #(swap! sniper.scope/+manual-strong-set+ conj %))
  )
