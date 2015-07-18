(ns sniper.scope
  (:use plumbing.core)
  (:require
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

;; TODO: shamalan-mode, coloring for strong set, depth in scc-graph.
;; for things in strong setk see centrality and roots.

;; TODO: orphaned test forms

(defn prepare-forms [forms]
  (for [f forms]
    (if (empty? (sniper/definitions f))
      (assoc f :shadow? true)
      f)))

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
  (map (fn-> (update :form sniper/definitions) (update-in-when [:cause] sniper/definitions))
       (:stack @+state+))
  #_(concat (first (:stack @+state+))))

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

(comment
  (def f (mapcat sniper.snarf/ns-forms '[sniper.graph sniper.core sniper.scope]))
  (sniper.scope/start! (sniper.scope/prepare-forms f) (constantly false) (constantly nil))
  )
