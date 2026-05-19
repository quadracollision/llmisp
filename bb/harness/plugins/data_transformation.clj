(ns harness.plugins.data-transformation
  (:require [clojure.string :as str]
            [harness.protocols :as protocols]
            [harness.validation :as validation]))

(def use-case protocols/default-use-case)

(defn- canonical-name [value]
  (some-> value
          name
          str/lower-case
          (str/replace #"[^a-z0-9]+" "-")
          (str/replace #"(^-+|-+$)" "")))

(defn- canonical-field [field]
  (cond
    (map? field)
    (cond-> field
      (:name field) (update :name canonical-name)
      (:field field) (update :field canonical-name))

    (keyword? field) (canonical-name field)
    (string? field) (canonical-name field)
    :else field))

(defn- canonical-fields-map [fields]
  (into {}
        (map (fn [[field-name field-shape]]
               [(canonical-name field-name) field-shape]))
        fields))

(defn- canonical-input-shape [shape]
  (if (map? shape)
    (cond-> shape
      (:name shape) (update :name canonical-name)
      (:field shape) (update :field canonical-name)
      (sequential? (:fields shape)) (update :fields #(mapv canonical-field %))
      (map? (:fields shape)) (update :fields canonical-fields-map)
      (map? (:items shape)) (update :items canonical-fields-map))
    shape))

(defn- canonical-output-shapes [shapes]
  (when shapes
    (into {}
          (map (fn [[output-key shape]]
                 [(keyword (canonical-name output-key)) shape]))
          shapes)))

(defn normalize-data-transformation-contract [contract]
  (when contract
    (cond-> (-> contract
                protocols/with-default-use-case
                (assoc :use_case use-case))
      (:required_output_keys contract)
      (update :required_output_keys #(mapv canonical-name %))

      (:output_key_shapes contract)
      (update :output_key_shapes canonical-output-shapes)

      (:input_shapes contract)
      (update :input_shapes #(mapv canonical-input-shape %))

      (get-in contract [:order_hint :key])
      (update-in [:order_hint :key] canonical-name))))

(defmethod protocols/normalize-contract :data-transformation [contract]
  (normalize-data-transformation-contract contract))

(defmethod protocols/validate-contract :data-transformation [contract]
  (let [contract (normalize-data-transformation-contract contract)]
    (cond
      (nil? contract)
      (protocols/ok-check "data-transformation-contract"
                          "Data transformation contract SKIPPED (none)\n")

      (not (map? contract))
      {:exit_code 1
       :stdout ""
       :stderr "[plugin/data-transformation] contract must be a map"
       :validator "data-transformation-contract"}

      (and (:required_output_keys contract)
           (not (sequential? (:required_output_keys contract))))
      {:exit_code 1
       :stdout ""
       :stderr "[plugin/data-transformation] required_output_keys must be an array when present"
       :validator "data-transformation-contract"}

      (and (:input_shapes contract)
           (not (sequential? (:input_shapes contract))))
      {:exit_code 1
       :stdout ""
       :stderr "[plugin/data-transformation] input_shapes must be an array when present"
       :validator "data-transformation-contract"}

      :else
      (protocols/ok-check "data-transformation-contract"
                          "Data transformation contract OK\n"))))

(defn- kebab [value]
  (some-> value
          str
          str/lower-case
          (str/replace #"[^a-z0-9]+" "-")
          (str/replace #"(^-+|-+$)" "")))

(defn- contract-function-name [task contract]
  (or (some-> (:function contract) str (str/split #"/") last kebab)
      (second (re-find #"(?i)main function\s+([a-z][a-z0-9-]*)" task))
      "main"))

(defn- contract-module-name [task contract]
  (or (second (re-find #"(?i)module must be named\s+([a-z][a-z0-9-]*(?:\.[a-z][a-z0-9-]*)*)" task))
      (when (and (:function contract) (str/includes? (str (:function contract)) "/"))
        (-> (:function contract) str (str/split #"/") first (str/replace #"_" "-")))
      "demo.generated"))

(defn- collection-name [task contract]
  (or (some-> (:input_shapes contract) first :name kebab)
      (second (re-find #"(?i)receives a collection named\s+([a-z][a-z0-9-]*)" task))
      (second (re-find #"(?i)accepts\s+([a-z][a-z0-9-]*)" task))
      "items"))

(defn- item-name [task]
  (or (second (re-find #"(?i)query over [a-z][a-z0-9-]* as\s+([a-z][a-z0-9-]*)" task))
      "item"))

(defn- input-field-names [contract]
  (let [metadata-keys #{:name :field :type :collection :arg_index :fields :field_shapes}]
    (->> (:input_shapes contract)
         (mapcat (fn [shape]
                   (let [fields (:fields shape)]
                     (cond
                       (and (map? shape) (= "collection" (:type shape)) (map? (:items shape)))
                       (keys (:items shape))
                       (map? fields) (keys fields)
                       (sequential? fields) (map #(or (:name %) (:field %) %) fields)
                       (:field shape) [(:field shape)]
                       (map? shape) (remove metadata-keys (keys shape))
                       :else []))))
         (map name)
         set)))

(defn- passthrough-entry [output-key item]
  {:key output-key
   :expr {:op "field"
          :target {:op "var" :name item}
          :field output-key}})

(defn- nil-entry [output-key]
  {:key output-key
   :expr {:op "nil"}})

(defmethod protocols/assemble-skeleton-ast :data-transformation [{:keys [task contract]}]
  (let [module-name (contract-module-name task contract)
        function-name (contract-function-name task contract)
        collection (collection-name task contract)
        item (item-name task)
        output-keys (mapv name (:required_output_keys contract))
        input-fields (input-field-names contract)
        sort-key (get-in contract [:order_hint :key])
        sort-direction (or (get-in contract [:order_hint :direction]) "asc")]
    {:module module-name
     :defs [{:name function-name
             :args [collection]
             :expr (cond-> {:op "query"
                            :rationale "Skeleton query: column-pass fills projection logic."
                            :collection {:op "var" :name collection}
                            :item item
                            :where []
                            :select {:op "map"
                                     :entries (mapv (fn [output-key]
                                                      (if (contains? input-fields output-key)
                                                        (passthrough-entry output-key item)
                                                        (nil-entry output-key)))
                                                    output-keys)}}
                     sort-key
                     (assoc :sort {:expr {:op "field"
                                          :target {:op "var" :name item}
                                          :field (name sort-key)}
                                   :direction sort-direction}))}]}))

(defmethod protocols/refine-where :data-transformation [{:keys [ast legacy-refine-where] :as ctx}]
  (if legacy-refine-where
    (legacy-refine-where ctx)
    ast))

(defmethod protocols/refine-columns :data-transformation [{:keys [ast legacy-refine-columns] :as ctx}]
  (if legacy-refine-columns
    (legacy-refine-columns ctx)
    ast))

(defn- some-indexed [f coll]
  (first (keep-indexed f coll)))

(declare query-select-entries-path)

(defn- child-query-select-path [expr path]
  (when (map? expr)
    (case (:op expr)
      "query" (or (when (= "map" (get-in expr [:select :op]))
                    (conj path :select :entries))
                  (query-select-entries-path (:select expr) (conj path :select)))
      "map" (some-indexed (fn [idx entry]
                            (query-select-entries-path (:expr entry) (conj path :entries idx :expr)))
                          (:entries expr))
      "vector" (some-indexed (fn [idx item]
                               (query-select-entries-path item (conj path :items idx)))
                             (:items expr))
      "let" (or (some-indexed (fn [idx binding]
                                (query-select-entries-path (:expr binding) (conj path :bindings idx :expr)))
                              (:bindings expr))
                (query-select-entries-path (:body expr) (conj path :body)))
      "if" (or (query-select-entries-path (:then expr) (conj path :then))
               (query-select-entries-path (:else expr) (conj path :else)))
      "cond" (or (some-indexed (fn [idx clause]
                                 (query-select-entries-path (:expr clause) (conj path :clauses idx :expr)))
                               (:clauses expr))
                 (query-select-entries-path (:else expr) (conj path :else)))
      nil)))

(defn- query-select-entries-path [expr path]
  (child-query-select-path expr path))

(defn- ast-select-entries-path [ast]
  (some-indexed (fn [idx defn-ast]
                  (query-select-entries-path (:expr defn-ast) [:defs idx :expr]))
                (:defs ast)))

(defn- select-entries [ast]
  (when-let [entries-path (ast-select-entries-path ast)]
    (get-in ast entries-path)))

(defn- literal-nil-output-keys [ast required-output-keys]
  (let [required (set (map name required-output-keys))
        entries-by-key (->> (select-entries ast)
                            (map (fn [entry] [(:key entry) entry]))
                            (into {}))]
    (->> required
         (filter (fn [output-key]
                   (= "nil" (get-in entries-by-key [output-key :expr :op]))))
         sort
         vec)))

(defn- refinement-completeness-check [ast contract]
  (let [nil-keys (literal-nil-output-keys ast (:required_output_keys contract))]
    (if (seq nil-keys)
      {:exit_code 1
       :stdout ""
       :stderr (str "[json-ast/refinement-failed] Column pass failed to generate valid logic for keys: "
                    (pr-str (mapv keyword nil-keys))
                    ". All model attempts for these keys were rejected by static semantic linters or left as skeleton placeholders.")
       :validator "refinement-completeness"
       :refinement_failed {:literal_nil_keys (mapv keyword nil-keys)}}
      {:exit_code 0
       :stdout "Refinement completeness OK\n"
       :stderr ""
       :validator "refinement-completeness"})))

(defmethod protocols/completeness-check :data-transformation [{:keys [stage ast contract]}]
  (case stage
    :post-where (validation/validate-where-completeness contract ast)
    :post-columns (refinement-completeness-check ast contract)
    (protocols/ok-check "data-transformation-completeness"
                        "Data transformation completeness SKIPPED\n")))
