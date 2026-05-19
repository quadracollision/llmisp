(ns harness.swing-schema
  (:require [harness.common :as common]
            [harness.action-plugins :as action-plugins]
            [malli.core :as m]
            [malli.error :as me]))

(defn- valid-id? [value]
  (and (string? value)
       (boolean (re-matches #"[a-z][a-z0-9-]*" value))))

(defn- positive-int? [value]
  (and (int? value) (pos? value)))

(def ^:private id-schema
  [:fn {:error/message "expected kebab-case id"} valid-id?])

(def ^:private positive-int-schema
  [:fn {:error/message "expected positive integer"} positive-int?])

(def ^:private non-empty-string
  [:and string? [:fn {:error/message "must not be blank"} #(not (clojure.string/blank? %))]])

(def ^:private tab-schema
  [:map {:closed true}
   [:id id-schema]
   [:title non-empty-string]
   [:children [:vector {:min 1} [:ref ::node]]]])

(def ^:private scalar-schema
  [:or string? int? double? boolean? nil?])

(def ^:private action-schema
  [:or
   [:multi {:dispatch :kind}
    ["show-message" [:map {:closed true}
                     [:kind [:= "show-message"]]
                     [:message non-empty-string]]]
    ["clear-form" [:map {:closed true}
                   [:kind [:= "clear-form"]]]]]
   action-plugins/plugin-action-shell-schema])

(def ^:private registry
  {::node
   [:multi {:dispatch :op}
    ["window" [:map {:closed true}
               [:op [:= "window"]]
               [:title non-empty-string]
               [:width positive-int-schema]
               [:height positive-int-schema]
               [:children [:vector {:min 1} [:ref ::node]]]]]
    ["panel" [:map {:closed true}
              [:op [:= "panel"]]
              [:id id-schema]
              [:title {:optional true} non-empty-string]
              [:layout {:optional true} [:enum "flow" "border" "grid" "row"]]
              [:children [:vector {:min 1} [:ref ::node]]]]]
    ["label" [:map {:closed true}
              [:op [:= "label"]]
              [:id {:optional true} id-schema]
              [:text non-empty-string]]]
    ["button" [:map {:closed true}
               [:op [:= "button"]]
               [:id id-schema]
               [:label non-empty-string]
               [:action {:optional true} action-schema]]]
    ["text-field" [:map {:closed true}
                   [:op [:= "text-field"]]
                   [:id id-schema]
                   [:label {:optional true} non-empty-string]
                   [:columns {:optional true} positive-int-schema]]]
    ["text-area" [:map {:closed true}
                  [:op [:= "text-area"]]
                  [:id id-schema]
                  [:label {:optional true} non-empty-string]
                  [:rows {:optional true} positive-int-schema]
                  [:columns {:optional true} positive-int-schema]]]
    ["combo-box" [:map {:closed true}
                  [:op [:= "combo-box"]]
                  [:id id-schema]
                  [:label {:optional true} non-empty-string]
                  [:options [:vector {:min 1} non-empty-string]]]]
    ["check-box" [:map {:closed true}
                  [:op [:= "check-box"]]
                  [:id id-schema]
                  [:label non-empty-string]]]
    ["table" [:map {:closed true}
              [:op [:= "table"]]
              [:id id-schema]
              [:columns [:vector {:min 1} id-schema]]
              [:rows {:optional true} [:vector [:map-of id-schema scalar-schema]]]]]
    ["tabs" [:map {:closed true}
             [:op [:= "tabs"]]
             [:id id-schema]
             [:tabs [:vector {:min 1} tab-schema]]]]]})

(def schema
  [:schema {:registry registry} ::node])

(defn- collect-ids [node]
  (let [own-id (when-let [id (:id node)] [id])
        child-ids (mapcat collect-ids (:children node))
        tab-ids (mapcat (fn [tab]
                          (concat [(:id tab)]
                                  (mapcat collect-ids (:children tab))))
                        (:tabs node))]
    (concat own-id child-ids tab-ids)))

(defn- duplicate-id-explain [ast]
  (let [duplicates (->> (collect-ids ast)
                        frequencies
                        (filter (fn [[_ n]] (> n 1)))
                        (map first)
                        sort
                        vec)]
    (when (seq duplicates)
      {:validator "swing-duplicate-ids"
       :duplicate_ids duplicates})))

(defn- collect-nodes [node]
  (concat [node]
          (mapcat collect-nodes (:children node))
          (mapcat (fn [tab]
                    (mapcat collect-nodes (:children tab)))
                  (:tabs node))))

(defn- plugin-action-explain [ast]
  (let [actions (keep :action (collect-nodes ast))
        errors (->> actions
                    (filter :plugin)
                    (mapcat #(action-plugins/action-errors {} %))
                    vec)]
    (when (seq errors)
      {:validator "swing-plugin-actions"
       :errors (common/json-safe errors)})))

(defn explain [ast]
  (when-let [explanation (m/explain schema ast)]
    {:validator "swing-malli"
     :humanized (me/humanize explanation)
     :errors (common/json-safe
              (mapv #(select-keys % [:path :in :value :type])
                    (:errors explanation)))}))

(defn validate [ast]
  (cond
    (explain ast)
    (let [explanation (explain ast)]
      {:ok false
       :exit_code 1
       :stdout ""
       :stderr (str "[gui-ast/malli-invalid] Swing GUI AST failed Malli validation\n"
                    (pr-str (:humanized explanation)))
       :malli explanation})

    (duplicate-id-explain ast)
    (let [explanation (duplicate-id-explain ast)]
      {:ok false
       :exit_code 1
       :stdout ""
       :stderr (str "[gui-ast/duplicate-ids] Swing GUI AST contains duplicate ids: "
                    (pr-str (:duplicate_ids explanation)))
       :duplicate_ids (:duplicate_ids explanation)
       :validator "swing-duplicate-ids"})

    (plugin-action-explain ast)
    (let [explanation (plugin-action-explain ast)]
      {:ok false
       :exit_code 1
       :stdout ""
       :stderr (str "[gui-ast/plugin-action-invalid] GUI plugin actions failed validation\n"
                    (pr-str (:errors explanation)))
       :plugin_action_errors (:errors explanation)
       :validator "swing-plugin-actions"})

    :else
    {:ok true
     :exit_code 0
     :stdout "Swing GUI AST Malli OK\n"
     :stderr ""}))
