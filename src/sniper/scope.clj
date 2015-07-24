(ns sniper.scope
  "Driver for exploring the dead code in a project, one form at a time.

   Stateful; intended to be used at the REPL within a project, or
   with the provided sniper.el emacs mode.

   To set things up, the basic flow accomplished by start! is:
     - You first read in a set of forms with sniper.snarf/classpath-ns-forms
     - Forms are marked as shadow if they have no definitions, or match
       a test regex.  This prevents us from considering all tests as
       dead, and from considering forms as used just because they are tested.
     - A dependency graph is constructed, an initial set of forms are
       marked as `strong` (based on regexes and symbols in the .sniper-strong.clj
       file, removing them and all dependencies from the graph, and then
       the graph is minified.
     - An initial set of potentially dead forms is identified, and a first
       form is returned.

   At this point, you can load sniper.el and M-x sniper-mode, and hit C-M-' to
   jump to the first dead form that was returned (or navigate their manually).

   Then, you repeatedly either:
    - delete the form, do any additional desired cleanup, and call
      fired! or press C-M-Backspace.  Any tests or other forms
      broken by this deletion will be added to the stack to be
      killed next (they are not spareable, and wil print as
      :collatoral).  OR,
    - don't delete the form, and call spare! or press C-M-= to mark
      it as strong.
    - At any point, you can call (aim) or press C-M-' to see the current
      target.

   If you make other modifications to the code, sniper may not pick
   them up so you may have to restart with start!.  Because of caching,
   this should generally be fast.

   Note that sniper currently makes errors, especially if you have
   some code missing from your classpath, or on forms that don't
   directly define something but instead perform a mutation on
   another definition.

   Some forms that are currently often erroneously considered dead:
    - ^:const
    - defprotocol
    - extend-schema

   Other nice-to-haves (TODO):
    - identify orphaned test forms
    - identify untested forms
    - see paths to strong roots in emacs, color forms by liveness, etc.
    - watch files and auto-update
    - find entire dead namespaces."
  (:use plumbing.core)
  (:require
   [clojure.java.shell :as shell]
   [schema.core :as s]
   [sniper.core :as sniper]
   [sniper.snarf :as snarf]
   [sniper.graph :as graph]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Private: helpers for maintaining strong set

(def +strong-set-file+
  "A file with one ref (class/var) per line, indicating forms that should never be considered
   dead.  The file is automatically updated as you interactively mark forms as strong."
  ".sniper-strong.clj")

(def +manual-strong-set+
  (atom (set (try (map read-string (.split (slurp +strong-set-file+) "\n")) (catch Exception e)))))

(defn strongify! [s]
  (swap! +manual-strong-set+ conj s)
  (spit +strong-set-file+ (str (pr-str s) "\n") :append true))

(defn strong-ref?
  "Is this reference marked strong according to the manual strong set, or one of the provided
   regexes?"
  [regexes r]
  (or (@+manual-strong-set+ r)
      (and (symbol? r)
           (let [s (str r)]
             (some #(re-find % s) regexes)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Private: maintaining scope state

(s/defschema KillableForm
  {:type (s/enum :leaf :leaf-cycle :collatoral)
   :form sniper/Form
   (s/optional-key :cause) (s/named sniper/Form "Killed form that made this collatoral")})

(s/defschema State
  "State of the sniping process."
  {:repo-root String
   :graph sniper.graph.DependencyGraph
   :stack [KillableForm]})

(def +state+ (atom nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Private: info about the next form.

(defn aim
  "Return the next form to snipe."
  []
  (first (:stack @+state+)))

(defn ref-count [s]
  (letk [[exit ^String out] (shell/sh "ag" "-Q" s (:repo-root @+state+))]
    (if (= exit 1)
      0
      (count (.split out "\n")))))

(defnk ref-counts
  "Show reference counts for literal matches to defs form using 'ag', to help judge if sniper
   missed any key references."
  [var-defs class-defs]
  (for-map [s (concat
               (map name var-defs)
               (map #(last (.split ^String % "\\.")) class-defs))
            :let [c (ref-count s)]
            :when (pos? c)]
    s c))

(defnk aim->el
  "Convert the result of `aim` into a format that can be read by sniper.el."
  [[:form [:source-info ^String file line column] var-defs :as form] type {cause nil}]
  (assert (.startsWith file "file:"))
  (list (subs file 5)
        line
        column
        (concat [type (first var-defs) (ref-counts form)] (when cause [cause]))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public: main driver loop.

(defn- prepare-forms [test-regex forms]
  (for [f forms]
    (if (or (empty? (sniper/definitions f))
            (re-find test-regex (safe-get-in f [:source-info :file])))
      (assoc f :shadow? true)
      f)))

(defn- init-stack [g]
  (->> (graph/leaf-components g)
       (keep (fn [forms]
               (when-let [f (first (filter #(seq (sniper/definitions %)) forms))]
                 {:form f
                  :type (if (next forms) :leaf-cycle :leaf)})))
       (sort-by (comp graph/sort-fn :form))
       reverse))

(defn start!
  "Start a sniper session, by identifying a repo root, set of regexes to consider strong
   from the get-go, and test-regex to identify shadow forms.

   Returns the first form to snipe.  After this, either delete the form (and do any other
   desired cleanup) then call fired!, or call spare! to mark it as strong.  Either way
   the result will be an aim at the next target.

   By default, traverses the repo bottom-up, jumping to immediately kill collatoral
   damage (e.g. tests of deleted forms)."
  [repo-root strong-regexes test-regex]
  (let [forms (prepare-forms
               test-regex
               (snarf/classpath-ns-forms (java.util.regex.Pattern/compile (str "^" repo-root))))
        g (graph/dependency-graph forms)
        g (->> (safe-get g :definers)
               keys
               (filter #(strong-ref? strong-regexes %))
               (reduce graph/strongify g)
               graph/minify)]
    (reset!
     +state+
     {:repo-root repo-root
      :graph g
      :stack (init-stack g)}))
  (aim))

(defn spare! []
  (letk [[graph stack :as state] @+state+
         [form type] (first stack)
         defs (sniper/definitions form)
         new-graph (reduce graph/strongify graph defs)]
    (assert (not= type :collatoral) "Cannot spare form (already deleted a dependency)")
    (doseq [d defs] (strongify! d))
    (reset! +state+
            (assoc state
              :graph new-graph
              :stack (vec (filter (set (graph/forms new-graph)) (next state))))))
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
  (sniper.scope/start!
   "/Users/w01fe/prismatic"
   [#"deploy/prod"
    #"/\$"
    #"crane.task/"
    #"topic-specs.admin/"
    #"plumbing.schema.generative/"
    #"data-warehouse.domain-schemas/"
    #".test-utils/"
    #"html-learn.readability/"
    #"social-scores.repl/"
    #"^hiphip."]
   #"/test/")
  )
