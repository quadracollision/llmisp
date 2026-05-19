(ns harness.action-plugins)

(def plugin-action-shell-schema
  [:map {:closed false}
   [:plugin string?]
   [:kind string?]])

(defmulti action-schema :plugin)
(defmulti action-errors (fn [_ctx action] (:plugin action)))
(defmulti action-form (fn [_ctx action] (:plugin action)))
(defmulti action-helper-forms identity)
(defmulti synthesize-action (fn [_contract _action-id intent] (:plugin intent)))
(defmulti reference-errors (fn [_ctx action] (:plugin action)))
(defmulti rewrite-action (fn [_ctx action] (:plugin action)))
(defmulti matches-intent? (fn [_ctx action _intent] (:plugin action)))

(defmethod action-schema :default [_]
  plugin-action-shell-schema)

(defmethod action-errors :default [_ctx _action]
  [])

(defmethod action-form :default [_ctx action]
  (throw (ex-info "No action plugin lowerer registered"
                  {:plugin (:plugin action)
                   :kind (:kind action)})))

(defmethod action-helper-forms :default [_plugin]
  [])

(defmethod synthesize-action :default [_contract _action-id _intent]
  nil)

(defmethod reference-errors :default [_ctx _action]
  [])

(defmethod rewrite-action :default [_ctx action]
  action)

(defmethod matches-intent? :default [_ctx _action _intent]
  false)
