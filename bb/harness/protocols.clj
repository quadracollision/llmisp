(ns harness.protocols
  (:require [clojure.string :as str]))

(def default-use-case :data-transformation)

(defn normalize-use-case [value]
  (cond
    (keyword? value) value
    (string? value) (-> value
                        str/trim
                        str/lower-case
                        (str/replace #"_" "-")
                        keyword)
    (nil? value) default-use-case
    :else (keyword (str value))))

(defn with-default-use-case [contract]
  (when contract
    (assoc contract :use_case (normalize-use-case (:use_case contract)))))

(defn ok-check
  ([validator]
   (ok-check validator "OK\n"))
  ([validator stdout]
   {:exit_code 0
    :stdout stdout
    :stderr ""
    :validator validator}))

(defn unsupported-check [use-case lifecycle]
  {:exit_code 1
   :stdout ""
   :stderr (str "[plugin/" (name use-case) "] " lifecycle " is not implemented")
   :validator (str (name use-case) "-plugin")})

(defmulti normalize-contract
  (fn [contract]
    (normalize-use-case (:use_case contract))))

(defmulti validate-contract
  (fn [contract]
    (normalize-use-case (:use_case contract))))

(defmulti assemble-skeleton-ast
  (fn [ctx]
    (normalize-use-case (get-in ctx [:contract :use_case]))))

(defmulti refine-where
  (fn [ctx]
    (normalize-use-case (get-in ctx [:contract :use_case]))))

(defmulti refine-columns
  (fn [ctx]
    (normalize-use-case (get-in ctx [:contract :use_case]))))

(defmulti completeness-check
  (fn [ctx]
    (normalize-use-case (get-in ctx [:contract :use_case]))))

(defmethod normalize-contract :default [contract]
  (with-default-use-case contract))

(defmethod validate-contract :default [contract]
  (let [contract (with-default-use-case contract)]
    (if (:use_case contract)
      (ok-check "contract-use-case" "Contract use case OK\n")
      {:exit_code 1
       :stdout ""
       :stderr "[plugin/contract] missing use_case"
       :validator "contract-use-case"})))

(defmethod assemble-skeleton-ast :default [ctx]
  (throw (ex-info "skeleton assembly is not implemented for use case"
                  {:use_case (normalize-use-case (get-in ctx [:contract :use_case]))})))

(defmethod refine-where :default [{:keys [ast contract]}]
  (if ast
    ast
    (throw (ex-info "where refinement requires an AST"
                    {:use_case (normalize-use-case (:use_case contract))}))))

(defmethod refine-columns :default [{:keys [ast contract]}]
  (if ast
    ast
    (throw (ex-info "column refinement requires an AST"
                    {:use_case (normalize-use-case (:use_case contract))}))))

(defmethod completeness-check :default [{:keys [contract]}]
  (unsupported-check (normalize-use-case (:use_case contract)) "completeness-check"))
