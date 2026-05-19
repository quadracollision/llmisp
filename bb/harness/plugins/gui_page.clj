(ns harness.plugins.gui-page
  (:require [clojure.string :as str]
            [harness.action-plugins :as action-plugins]
            [harness.protocols :as protocols]
            [harness.swing-schema :as swing-schema]))

(def use-case :gui-page)

(defn- canonical-name [value]
  (some-> value
          name
          str/lower-case
          (str/replace #"[^a-z0-9]+" "-")
          (str/replace #"(^-+|-+$)" "")))

(defn- normalize-components [components]
  (->> (or components [])
       (map canonical-name)
       (remove str/blank?)
       distinct
       vec))

(defn- normalize-persistence [persistence title]
  (when (map? persistence)
    (cond-> persistence
      (:db persistence) (update :db str)
      (not (:db persistence)) (assoc :db (str (canonical-name (or title "gui-data")) ".sqlite3"))
      (:table persistence) (update :table canonical-name)
      (not (:table persistence)) (assoc :table "submissions")
      (:target persistence) (update :target canonical-name)
      (not (:target persistence)) (assoc :target "submissions-data")
      (:columns persistence) (update :columns normalize-components))))

(defn- ensure-action-component [contract]
  (if (seq (:actions contract))
    (update contract :required_components
            (fn [components]
              (let [components (vec components)]
                (if (some #{"actions-panel"} components)
                  components
                  (conj components "actions-panel")))))
    contract))

(defn- generic-wrapper-component? [component-id]
  (boolean
   (re-find #"(^|-)main($|-)|(^|-)root($|-)|(^|-)container($|-)|(^|-)wrapper($|-)|(^|-)page($|-)|(^|-)window($|-)"
            component-id)))

(defn- prune-generic-wrappers [contract]
  (let [components (:required_components contract)
        specific (remove generic-wrapper-component? components)]
    (if (seq specific)
      (assoc contract :required_components (vec specific))
      contract)))

(defn normalize-gui-page-contract [contract]
  (when contract
    (-> (cond-> (-> contract
                    protocols/with-default-use-case
                    (assoc :use_case use-case))
          (:title contract)
          (update :title str)

          (:window_kind contract)
          (update :window_kind canonical-name)

          (:required_components contract)
          (update :required_components normalize-components)

          (:data_fields contract)
          (update :data_fields normalize-components)

          (:actions contract)
          (update :actions normalize-components)

          (:persistence contract)
          (update :persistence normalize-persistence (:title contract)))
        prune-generic-wrappers
        ensure-action-component)))

(defmethod protocols/normalize-contract :gui-page [contract]
  (normalize-gui-page-contract contract))

(defmethod protocols/validate-contract :gui-page [contract]
  (let [contract (normalize-gui-page-contract contract)]
    (cond
      (not (map? contract))
      {:exit_code 1
       :stdout ""
       :stderr "[plugin/gui-page] contract must be a map"
       :validator "gui-page-contract"}

      (str/blank? (str (:title contract)))
      {:exit_code 1
       :stdout ""
       :stderr "[plugin/gui-page] title is required"
       :validator "gui-page-contract"}

      (not (seq (:required_components contract)))
      {:exit_code 1
       :stdout ""
       :stderr "[plugin/gui-page] required_components must contain at least one component id"
       :validator "gui-page-contract"}

      :else
      (protocols/ok-check "gui-page-contract"
                          "GUI page contract OK\n"))))

(defn- action-kind [contract action-id]
  (cond
    (re-find #"(?i)\b(clear|reset)\b" action-id)
    {:kind "clear-form"}

    (and (:persistence contract) (re-find #"(?i)\bsave\b" action-id))
    (action-plugins/synthesize-action contract action-id {:plugin (get-in contract [:persistence :plugin])
                                                          :kind :insert-draft})

    (and (:persistence contract) (re-find #"(?i)\bsubmit\b" action-id))
    (action-plugins/synthesize-action contract action-id {:plugin (get-in contract [:persistence :plugin])
                                                          :kind :insert-submitted})

    (and (:persistence contract) (re-find #"(?i)\brefresh\b" action-id))
    (action-plugins/synthesize-action contract action-id {:plugin (get-in contract [:persistence :plugin])
                                                          :kind :refresh-table})

    :else
    {:kind "show-message"
     :message (str (-> action-id
                       (str/replace #"-" " ")
                       str/capitalize)
                   " requested.")}))

(defn- persistence-action-intent [action-id]
  (cond
    (re-find #"(?i)\bsave\b" action-id) {:kind :insert-draft}
    (re-find #"(?i)\bsubmit\b" action-id) {:kind :insert-submitted}
    (re-find #"(?i)\brefresh\b" action-id) {:kind :refresh-table}
    :else nil))

(defn- action-button [contract action-id]
  {:op "button"
   :id action-id
   :label (-> action-id
              (str/replace #"-" " ")
              str/capitalize)
   :action (action-kind contract action-id)})

(defn- component-panel [contract component-id]
  (if (= "actions-panel" component-id)
    {:op "panel"
     :id component-id
     :title "Actions"
     :layout "flow"
     :children (mapv #(action-button contract %) (:actions contract))}
    {:op "panel"
     :id component-id
     :title (-> component-id
                (str/replace #"-" " ")
                str/capitalize)
     :layout "flow"
     :children [{:op "label"
                 :id (str component-id "-placeholder")
                 :text (str "Placeholder for " component-id)}]}))

(defmethod protocols/assemble-skeleton-ast :gui-page [{:keys [contract]}]
  (let [contract (normalize-gui-page-contract contract)
        ast {:op "window"
             :title (:title contract)
             :width (or (:width contract) 900)
             :height (or (:height contract) 600)
             :children (mapv #(component-panel contract %) (:required_components contract))}
        check (swing-schema/validate ast)]
    (when-not (:ok check)
      (throw (ex-info "GUI skeleton failed Swing schema validation"
                      {:check check})))
    ast))

(defmethod protocols/refine-where :gui-page [{:keys [ast]}]
  ast)

(defmethod protocols/refine-columns :gui-page [{:keys [ast]}]
  ast)

(defn- collect-component-ids [node]
  (let [own-id (when-let [id (:id node)] [id])
        child-ids (mapcat collect-component-ids (:children node))
        tab-ids (mapcat (fn [tab]
                          (concat [(:id tab)]
                                  (mapcat collect-component-ids (:children tab))))
                        (:tabs node))]
    (concat own-id child-ids tab-ids)))

(defn- collect-button-ids [node]
  (let [own-id (when (and (= "button" (:op node)) (:id node)) [(:id node)])
        child-ids (mapcat collect-button-ids (:children node))
        tab-ids (mapcat (fn [tab]
                          (mapcat collect-button-ids (:children tab)))
                        (:tabs node))]
    (concat own-id child-ids tab-ids)))

(defn- collect-buttons [node]
  (let [own (when (= "button" (:op node)) [node])
        child-buttons (mapcat collect-buttons (:children node))
        tab-buttons (mapcat (fn [tab]
                              (mapcat collect-buttons (:children tab)))
                            (:tabs node))]
    (concat own child-buttons tab-buttons)))

(defn- collect-table-ids [node]
  (let [own (when (= "table" (:op node)) [(:id node)])
        child-ids (mapcat collect-table-ids (:children node))
        tab-ids (mapcat (fn [tab]
                          (mapcat collect-table-ids (:children tab)))
                        (:tabs node))]
    (concat own child-ids tab-ids)))

(defn- plugin-reference-errors [ast]
  (let [ids (set (collect-component-ids ast))
        table-ids (set (collect-table-ids ast))]
    (->> (collect-buttons ast)
         (map :action)
         (filter :plugin)
         (mapcat #(action-plugins/reference-errors {:ids ids
                                                    :table-ids table-ids}
                                                   %))
         vec)))

(defmethod protocols/completeness-check :gui-page [{:keys [ast contract]}]
  (let [contract (normalize-gui-page-contract contract)
        ids (set (collect-component-ids ast))
        missing (->> (:required_components contract)
                     (remove ids)
                     vec)
        button-ids (set (collect-button-ids ast))
        missing-actions (->> (:actions contract)
                             (remove button-ids)
                             vec)
        buttons-by-id (->> (collect-buttons ast)
                           (map (juxt :id :action))
                           (into {}))
        persistence-action-errors (when (:persistence contract)
                                    (->> (:actions contract)
                                         (keep (fn [action-id]
                                                 (when-let [intent (persistence-action-intent action-id)]
                                                   (let [action (get buttons-by-id action-id)
                                                         plugin (get-in contract [:persistence :plugin])]
                                                     (when-not (action-plugins/matches-intent?
                                                                {}
                                                                action
                                                                (assoc intent :plugin plugin))
                                                       action-id)))))
                                         vec))
        plugin-reference-errors (plugin-reference-errors ast)]
    (cond
      (seq missing)
      {:exit_code 1
       :stdout ""
       :stderr (str "[plugin/gui-page] missing required components: " (pr-str missing))
       :validator "gui-page-completeness"
       :missing_components missing}

      (seq missing-actions)
      {:exit_code 1
       :stdout ""
       :stderr (str "[plugin/gui-page] missing required action buttons: " (pr-str missing-actions))
       :validator "gui-page-completeness"
       :missing_actions missing-actions}

      (seq persistence-action-errors)
      {:exit_code 1
       :stdout ""
       :stderr (str "[plugin/gui-page] persistence actions do not match their plugin intents: "
                    (pr-str persistence-action-errors))
       :validator "gui-page-completeness"
       :invalid_persistence_actions persistence-action-errors}

      (seq plugin-reference-errors)
      {:exit_code 1
       :stdout ""
       :stderr (str "[plugin/gui-page] invalid plugin action references: "
                    (pr-str plugin-reference-errors))
       :validator "gui-page-completeness"
       :plugin_reference_errors plugin-reference-errors}

      :else
      (protocols/ok-check "gui-page-completeness"
                          "GUI page completeness OK\n"))))
