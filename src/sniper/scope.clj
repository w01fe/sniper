(ns sniper.scope
  (:use plumbing.core)
  (:require
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

;; TODO: pick cycle-breakers, such that every node
;;   has some ancestor in the initial stack.
;;   (but put them after leaves, and mark them).
;; so actually we want to construct scc-graph and choose
;; a suitable element from each leaf SCC.

;; TODO: keep type of node/edge in stack.

;; TODO: shamalan-mode, coloring for strong set, depth in scc-graph.
;; for things in strong setk see centrality and roots.

;; TODO: orphaned test forms

(def +state+ (atom nil))

(defn aim []
  (map #(select-keys % [:var-defs :class-defs]) (:stack @+state+))
  #_(concat (first (:stack @+state+))))

(defn start! [forms strong-ref? add-strong!]
  (let [g (graph/dependency-graph forms)
        g (graph/minify
           (reduce graph/strongify g (filter strong-ref? (keys (safe-get g :definers)))))]
    (reset!
     +state+
     {:graph g
      :add-strong! add-strong!
      :stack (filter #(and (graph/unused? g %)
                           (not (sniper/shadow? %))
                           (seq (sniper/definitions %)))
                     (graph/forms g))}))
  (aim))

(defn spare! []
  (letk [[graph add-strong! stack :as state] @+state+
         form (first stack)
         defs (sniper/definitions form)]
    (doseq [d defs]) (add-strong! form)
    (reset! +state+
            (assoc state
              :graph (reduce graph/strongify graph defs)
              :stack (next stack))))
  (aim))

(defn fired! []
  (letk [[graph stack :as state] @+state+
         form (first stack)
         new-g (graph/remove-form graph form)]
    (reset! +state+
            (assoc state
              :graph new-g
              :stack (distinct
                      (concat
                       (next (graph/ancestors graph form))
                       (filter #(graph/unused? new-g %) (graph/callees graph form))
                       (next stack))))))
  (aim))
