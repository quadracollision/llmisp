(ns harness.semantic
  (:require [clojure.edn :as edn]
            [clojure.set]
            [harness.common :as common]
            [harness.sqlite :as sqlite]))

(defn- invoke-symbol [qualified-symbol args]
  (let [resolved (requiring-resolve qualified-symbol)]
    (when-not resolved
      (throw (ex-info "semantic function not found" {:function (str qualified-symbol)})))
    (apply @resolved args)))

(defn- map-diff [expected actual]
  (let [expected-keys (set (keys expected))
        actual-keys (set (keys actual))
        shared-keys (sort-by str (clojure.set/intersection expected-keys actual-keys))
        wrong-values (->> shared-keys
                          (keep (fn [k]
                                  (when-not (= (get expected k) (get actual k))
                                    {:key k
                                     :expected (get expected k)
                                     :actual (get actual k)})))
                          vec)]
    (cond-> {}
      (seq (clojure.set/difference expected-keys actual-keys))
      (assoc :missing_keys (sort-by str (clojure.set/difference expected-keys actual-keys)))

      (seq (clojure.set/difference actual-keys expected-keys))
      (assoc :extra_keys (sort-by str (clojure.set/difference actual-keys expected-keys)))

      (seq wrong-values)
      (assoc :wrong_values wrong-values))))

(defn semantic-diff [expected actual]
  (cond
    (and (vector? expected) (vector? actual))
    (let [count-diff (when-not (= (count expected) (count actual))
                       {:expected_count (count expected) :actual_count (count actual)})
          item-diffs (->> (map-indexed (fn [idx [expected-item actual-item]]
                                          (when-not (= expected-item actual-item)
                                            (cond-> {:index idx
                                                     :expected expected-item
                                                     :actual actual-item}
                                              (and (map? expected-item) (map? actual-item))
                                              (assoc :map_diff (map-diff expected-item actual-item)))))
                                        (map vector expected actual))
                          (remove nil?)
                          vec)]
      (cond-> {}
        count-diff (assoc :count count-diff)
        (seq item-diffs) (assoc :items item-diffs)))

    (and (map? expected) (map? actual))
    (map-diff expected actual)

    :else
    {:expected expected :actual actual}))

(defn run-semantic-tests [clj-path test-path]
  (try
    (let [suite (edn/read-string (slurp test-path))
          ns-name (:namespace suite)
          fn-name (:function suite)
          cases (:cases suite)
          qualified (symbol (str ns-name) (str fn-name))]
      (load-file clj-path)
      (let [results (mapv (fn [case]
                            (let [actual (invoke-symbol qualified (:args case))
                                  expected (:expected case)
                                  pass? (= expected actual)]
                              (cond-> {:name (:name case)
                                       :pass pass?
                                       :expected expected
                                       :actual actual}
                                (not pass?) (assoc :diff (semantic-diff expected actual))
                                (:description case) (assoc :description (:description case)))))
                          cases)
            failed (remove :pass results)]
        (if (seq failed)
          {:exit_code 1
           :stdout ""
           :stderr (str "[semantic/failure] " (count failed) " semantic case(s) failed")
           :validator "semantic"
           :semantic {:test_file test-path
                      :function (str qualified)
                      :cases (common/json-safe results)}}
          {:exit_code 0
           :stdout (str "Semantic OK (" (count results) " case(s))\n")
           :stderr ""
           :validator "semantic"
           :semantic {:test_file test-path
                      :function (str qualified)
                      :cases (common/json-safe results)}})))
    (catch Throwable ex
      {:exit_code 1
       :stdout ""
       :stderr (str "[semantic/error] " (.getMessage ex))
       :validator "semantic"
       :semantic {:test_file test-path
                  :error (str ex)
                  :data (common/json-safe (ex-data ex))}})))

(defn- trace-spy-form [db-path session-id attempt-index current-case]
  `(defn ~(symbol "__harness-spy") [tag# data#]
     (harness.sqlite/save-trace! ~db-path
                                 ~session-id
                                 ~attempt-index
                                 @~current-case
                                 tag#
                                 (harness.common/json-safe data#))
     data#))

(defn run-semantic-forms
  ([forms test-path]
   (run-semantic-forms forms test-path {}))
  ([forms test-path {:keys [db-path session-id attempt-index trace?]
                     :or {trace? false}}]
   (let [suite (edn/read-string (slurp test-path))
         ns-name (:namespace suite)
         fn-name (:function suite)
         cases (:cases suite)
         qualified (symbol (str ns-name) (str fn-name))
         current-case (gensym "current-case")]
     (try
       (remove-ns ns-name)
       (binding [*ns* *ns*]
         (doseq [form (cond-> forms
                        trace? (-> vec
                                   (subvec 0 1)
                                   (conj `(def ~current-case (atom nil))
                                         (trace-spy-form (str db-path) session-id attempt-index current-case))
                                   (into (rest forms))))]
           (eval form)))
       (let [target-var (ns-resolve ns-name fn-name)]
         (when-not target-var
           (throw (ex-info "semantic function not found" {:function (str qualified)})))
         (let [case-state (when trace? (ns-resolve ns-name current-case))
               results (mapv (fn [case]
                               (when case-state
                                 (reset! @case-state (:name case)))
                               (let [actual (apply @target-var (:args case))
                                     expected (:expected case)
                                     pass? (= expected actual)]
                                 (cond-> {:name (:name case)
                                          :pass pass?
                                          :expected expected
                                          :actual actual}
                                   (not pass?) (assoc :diff (semantic-diff expected actual))
                                   (:description case) (assoc :description (:description case)))))
                             cases)
               failed (remove :pass results)]
           (if (seq failed)
             {:exit_code 1
              :stdout ""
              :stderr (str "[semantic/failure] " (count failed) " semantic case(s) failed")
              :validator "semantic-eval"
              :semantic {:test_file test-path
                         :function (str qualified)
                         :cases (common/json-safe results)}}
             {:exit_code 0
              :stdout (str "Semantic OK (" (count results) " case(s))\n")
              :stderr ""
              :validator "semantic-eval"
              :semantic {:test_file test-path
                         :function (str qualified)
                         :cases (common/json-safe results)}})))
       (catch Throwable ex
         {:exit_code 1
          :stdout ""
          :stderr (str "[semantic/eval-error] " (.getMessage ex))
          :validator "semantic-eval"
          :semantic {:test_file test-path
                     :function (str qualified)
                     :error (str ex)
                     :data (common/json-safe (ex-data ex))}})
       (finally
         (remove-ns ns-name))))))
