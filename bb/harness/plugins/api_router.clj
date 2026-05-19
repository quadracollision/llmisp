(ns harness.plugins.api-router
  (:require [clojure.string :as str]
            [harness.protocols :as protocols]))

(def use-case :api-router)

(defn- normalize-route [route]
  (when (map? route)
    (cond-> route
      (:method route) (update :method #(-> % str str/lower-case keyword))
      (:path route) (update :path str))))

(defn normalize-api-router-contract [contract]
  (when contract
    (-> contract
        protocols/with-default-use-case
        (assoc :use_case use-case)
        (update :routes #(mapv normalize-route (or % []))))))

(defmethod protocols/normalize-contract :api-router [contract]
  (normalize-api-router-contract contract))

(defmethod protocols/validate-contract :api-router [contract]
  (let [contract (normalize-api-router-contract contract)
        routes (:routes contract)]
    (cond
      (not (map? contract))
      {:exit_code 1
       :stdout ""
       :stderr "[plugin/api-router] contract must be a map"
       :validator "api-router-contract"}

      (not (sequential? routes))
      {:exit_code 1
       :stdout ""
       :stderr "[plugin/api-router] routes must be an array"
       :validator "api-router-contract"}

      (some #(not (and (:method %) (:path %))) routes)
      {:exit_code 1
       :stdout ""
       :stderr "[plugin/api-router] each route must include method and path"
       :validator "api-router-contract"}

      :else
      (protocols/ok-check "api-router-contract"
                          "API router contract OK\n"))))

(defn- unsupported [lifecycle]
  (assoc (protocols/unsupported-check use-case lifecycle)
         :hint "This is a blueprint plugin. Implement this lifecycle method in an extension before using :api-router for generation."))

(defmethod protocols/assemble-skeleton-ast :api-router [_ctx]
  (throw (ex-info "[plugin/api-router] skeleton assembly is not implemented yet"
                  (unsupported "assemble-skeleton-ast"))))

(defmethod protocols/refine-where :api-router [{:keys [ast]}]
  (if ast
    ast
    (throw (ex-info "[plugin/api-router] where refinement is not implemented yet"
                    (unsupported "refine-where")))))

(defmethod protocols/refine-columns :api-router [{:keys [ast]}]
  (if ast
    ast
    (throw (ex-info "[plugin/api-router] column refinement is not implemented yet"
                    (unsupported "refine-columns")))))

(defmethod protocols/completeness-check :api-router [_ctx]
  (unsupported "completeness-check"))
