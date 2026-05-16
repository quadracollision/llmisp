(ns harness.validation
  (:require [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.set]
            [clojure.string :as str]
            [harness.common :as common]
            [malli.core :as m]
            [malli.error :as me]))

(defn- valid-name? [value]
  (and (string? value)
       (boolean (re-matches #"[a-z][a-z0-9-]*" value))))

(defn- valid-module-name? [value]
  (and (string? value)
       (boolean (re-matches #"[a-z][a-z0-9-]*(\.[a-z][a-z0-9-]*)*" value))))

(def ^:private name-schema
  [:fn {:error/message "expected kebab-case name"} valid-name?])

(def ^:private module-name-schema
  [:fn {:error/message "expected dotted kebab-case module name"} valid-module-name?])

(def ^:private ast-registry
  {::expr
   [:multi {:dispatch :op}
    ["string" [:map {:closed true} [:op [:= "string"]] [:value string?]]]
    ["int" [:map {:closed true} [:op [:= "int"]] [:value int?]]]
    ["float" [:map {:closed true} [:op [:= "float"]] [:value number?]]]
    ["bool" [:map {:closed true} [:op [:= "bool"]] [:value boolean?]]]
    ["nil" [:map {:closed true} [:op [:= "nil"]]]]
    ["var" [:map {:closed true} [:op [:= "var"]] [:name name-schema]]]
    ["field" [:map {:closed true}
              [:op [:= "field"]]
              [:target [:ref ::expr]]
              [:field name-schema]]]
    ["call" [:map {:closed true}
             [:op [:= "call"]]
             [:fn name-schema]
             [:args [:vector [:ref ::expr]]]]]
    ["map" [:map {:closed true}
            [:op [:= "map"]]
            [:entries [:vector [:map {:closed true}
                                [:key name-schema]
                                [:expr [:ref ::expr]]]]]]]
    ["vector" [:map {:closed true}
               [:op [:= "vector"]]
               [:items [:vector [:ref ::expr]]]]]
    ["let" [:map {:closed true}
            [:op [:= "let"]]
            [:bindings [:vector [:map {:closed true}
                                  [:name name-schema]
                                  [:expr [:ref ::expr]]]]]
            [:body [:ref ::expr]]]]
    ["if" [:map {:closed true}
           [:op [:= "if"]]
           [:condition [:ref ::expr]]
           [:then [:ref ::expr]]
           [:else [:ref ::expr]]]]
    ["cond" [:map {:closed true}
             [:op [:= "cond"]]
             [:rationale {:optional true} string?]
             [:clauses [:vector {:min 1}
                        [:map {:closed true}
                         [:condition [:ref ::expr]]
                         [:expr [:ref ::expr]]]]]
             [:else [:ref ::expr]]]]
    ["query" [:map {:closed true}
              [:op [:= "query"]]
              [:rationale {:optional true} string?]
              [:collection [:ref ::expr]]
              [:item name-schema]
              [:where [:vector [:ref ::expr]]]
              [:sort {:optional true}
               [:map {:closed true}
                [:expr [:ref ::expr]]
                [:direction [:enum "asc" "desc"]]]]
              [:select [:ref ::expr]]]]
    ["=" [:map {:closed true}
          [:op [:= "="]]
          [:left [:ref ::expr]]
          [:right [:ref ::expr]]]]
    ["not=" [:map {:closed true}
             [:op [:= "not="]]
             [:left [:ref ::expr]]
             [:right [:ref ::expr]]]]
    [">" [:map {:closed true}
          [:op [:= ">"]]
          [:left [:ref ::expr]]
          [:right [:ref ::expr]]]]
    ["<" [:map {:closed true}
          [:op [:= "<"]]
          [:left [:ref ::expr]]
          [:right [:ref ::expr]]]]
    [">=" [:map {:closed true}
           [:op [:= ">="]]
           [:left [:ref ::expr]]
           [:right [:ref ::expr]]]]
    ["<=" [:map {:closed true}
           [:op [:= "<="]]
           [:left [:ref ::expr]]
           [:right [:ref ::expr]]]]
    ["and" [:map {:closed true}
            [:op [:= "and"]]
            [:args [:vector {:min 1} [:ref ::expr]]]]]
    ["or" [:map {:closed true}
           [:op [:= "or"]]
           [:args [:vector {:min 1} [:ref ::expr]]]]]
    ["not" [:map {:closed true}
            [:op [:= "not"]]
            [:expr [:ref ::expr]]]]
    ["nil?" [:map {:closed true}
             [:op [:= "nil?"]]
             [:expr [:ref ::expr]]]]
    ["some?" [:map {:closed true}
              [:op [:= "some?"]]
              [:expr [:ref ::expr]]]]]

   ::def
   [:map {:closed true}
    [:name name-schema]
    [:args [:vector name-schema]]
    [:expr [:ref ::expr]]]

   ::module
   [:map {:closed true}
    [:module module-name-schema]
    [:defs [:vector {:min 1} [:ref ::def]]]]})

(def ^:private ast-schema
  [:schema {:registry ast-registry} ::module])

(def json-safe common/json-safe)

(defn- malli-explain [ast]
  (when-let [explanation (m/explain ast-schema ast)]
    {:validator "malli"
     :humanized (me/humanize explanation)
     :errors (json-safe
              (mapv #(select-keys % [:path :in :value :type])
                    (:errors explanation)))}))

(defn validate-ast-shape [ast]
  (if-let [explanation (malli-explain ast)]
    {:ok false
     :exit_code 1
     :stdout ""
     :stderr (str "[json-ast/malli-invalid] JSON AST failed Malli validation\n"
                  (pr-str (:humanized explanation)))
     :malli explanation}
    {:ok true
     :exit_code 0
     :stdout "Malli OK\n"
     :stderr ""}))

(defn- value-shape [value]
  (cond
    (vector? value) {:type "vector"
                     :count (count value)
                     :item_keys (->> value
                                     (filter map?)
                                     (mapcat keys)
                                     set
                                     (sort-by str))}
    (map? value) {:type "map"
                  :keys (sort-by str (keys value))}
    :else {:type (cond
                   (nil? value) "nil"
                   (string? value) "string"
                   (number? value) "number"
                   (boolean? value) "boolean"
                   :else (pr-str (type value)))}))

(defn- scalar-shape? [shape]
  (or (#{"nil" "string" "number" "boolean" "scalar"} (:type shape))
      (boolean (re-find #"(?i)\b(string|number|boolean)\b.*nullable" (str (:type shape))))
      (boolean (re-find #"(?i)\b(string|number|boolean)\b.*missing" (str (:type shape))))
      (boolean (re-find #"(?i)\b(string|number|boolean)[_/-]or[_/-]missing\b" (str (:type shape))))
      (boolean (re-find #"(?i)\b(string|number|boolean)[_/-]missing\b" (str (:type shape))))
      (and (:nullable shape) (#{"string" "number" "boolean"} (:type shape)))))

(defn- merge-observed-shapes [shapes]
  (let [shapes (vec (remove nil? shapes))
        types (set (map :type shapes))
        non-nil-types (disj types "nil")]
    (cond
      (empty? shapes) {:type "unknown"}
      (= 1 (count types)) (first shapes)
      (and (contains? types "nil") (= 1 (count non-nil-types)))
      {:type (first non-nil-types) :nullable true}
      (every? scalar-shape? shapes) {:type "scalar" :observed shapes}
      :else {:type "mixed" :observed shapes})))

(defn- field-shapes [values]
  (->> values
       (filter map?)
       (mapcat (fn [row] (map (fn [[k v]] [k (value-shape v)]) row)))
       (reduce (fn [acc [k shape]]
                 (update acc k (fnil conj []) shape))
               {})
       (map (fn [[k shapes]] [k (merge-observed-shapes shapes)]))
       (into {})))

(defn- output-key-shapes [cases]
  (->> cases
       (mapcat (fn [case]
                 (let [expected (:expected case)]
                   (cond
                     (and (vector? expected) (every? map? expected))
                     (mapcat (fn [row] (map (fn [[k v]] [k (value-shape v)]) row)) expected)

                     (map? expected)
                     (map (fn [[k v]] [k (value-shape v)]) expected)

                     :else []))))
       (reduce (fn [acc [k shape]]
                 (update acc k (fnil conj []) shape))
               {})
       (map (fn [[k shapes]]
              [k (merge-observed-shapes shapes)]))
       (into {})))

(defn- scalar-value? [value]
  (or (nil? value) (string? value) (number? value) (boolean? value)))

(defn- output-key-values [cases]
  (->> cases
       (mapcat (fn [case]
                 (let [expected (:expected case)]
                   (cond
                     (and (vector? expected) (every? map? expected))
                     (mapcat (fn [row] (filter (fn [[_ v]] (scalar-value? v)) row)) expected)

                     (map? expected)
                     (filter (fn [[_ v]] (scalar-value? v)) expected)

                     :else []))))
       (reduce (fn [acc [k value]]
                 (update acc k (fnil conj []) value))
               {})
       (map (fn [[k values]]
              [k {:values (->> values distinct (sort-by pr-str) vec)}]))
       (into {})))

(defn- input-shapes [cases]
  (->> cases
       (mapcat (fn [case]
                 (map-indexed
                  (fn [idx arg]
                    (cond
                      (and (vector? arg) (every? map? arg))
                      {:arg_index idx
                       :collection true
                       :fields (->> arg (mapcat keys) set (sort-by str) vec)
                       :field_shapes (field-shapes arg)}

                      (map? arg)
                      {:arg_index idx
                       :collection false
                       :fields (->> arg keys (sort-by str) vec)
                       :field_shapes (field-shapes [arg])}

                      :else nil))
                  (:args case))))
       (remove nil?)
       (reduce (fn [acc {:keys [arg_index collection fields field_shapes]}]
                 (update acc arg_index
                         (fn [existing]
                           {:arg_index arg_index
                            :collection (or collection (:collection existing))
                            :fields (->> (concat fields (:fields existing))
                                         set
                                         (sort-by str)
                                         vec)
                            :field_shapes (merge (:field_shapes existing) field_shapes)})))
               {})
       vals
       (sort-by :arg_index)
       vec))

(defn- expected-output-keys [cases]
  (->> cases
       (mapcat (fn [case]
                 (let [expected (:expected case)]
                   (cond
                     (and (vector? expected) (every? map? expected)) (mapcat keys expected)
                     (map? expected) (keys expected)
                     :else []))))
       set
       (sort-by str)
       vec))

(defn- order-hint [cases]
  (some (fn [case]
          (let [expected (:expected case)]
            (when (and (vector? expected) (every? map? expected) (seq expected))
              (some (fn [k]
                      (let [values (mapv #(get % k) expected)]
                        (cond
                          (= values (sort values)) {:key k :direction "asc"}
                          (= values (reverse (sort values))) {:key k :direction "desc"}
                          :else nil)))
                    (keys (first expected))))))
        cases))

(defn- contract [test-path {:keys [include-output-values? include-expected?]
                            :or {include-output-values? false
                                 include-expected? false}}]
  (when test-path
    (let [suite (edn/read-string (slurp test-path))
          cases (:cases suite)]
      (cond-> {:test_file test-path
               :function (str (:namespace suite) "/" (:function suite))
               :required_output_keys (expected-output-keys cases)
               :output_key_shapes (output-key-shapes cases)
               :forbidden_output_keys "any key not listed in required_output_keys"
               :input_shapes (input-shapes cases)
               :order_hint (order-hint cases)
               :expected_examples (mapv (fn [case]
                                          (cond-> {:name (:name case)
                                                   :expected_shape (value-shape (:expected case))}
                                            include-expected?
                                            (assoc :expected (:expected case))))
                                        cases)}
        include-output-values?
        (assoc :output_key_values (output-key-values cases))))))

(defn semantic-contract [test-path]
  (contract test-path {:include-output-values? false
                       :include-expected? false}))

(defn validation-contract [test-path]
  (contract test-path {:include-output-values? true
                       :include-expected? true}))

(defn sanitize-contract [contract]
  (when contract
    (let [allowed-keys [:test_file
                        :function
                        :required_output_keys
                        :output_key_shapes
                        :forbidden_output_keys
                        :input_shapes
                        :order_hint
                        :filter_required
                        :filter_rules
                        :filtering_rules
                        :where_rules
                        :filter_conditions
                        :filtering_conditions
                        :filtering_logic
                        :expected_examples]]
      (cond-> (select-keys contract allowed-keys)
        (:expected_examples contract)
        (assoc :expected_examples
               (mapv (fn [example]
                       (select-keys example [:name :expected_shape]))
                     (:expected_examples contract)))))))

(defn format-semantic-contract [contract]
  (when contract
    (str "Semantic contract from runtime fixture:\n"
         (pr-str (json-safe contract))
         "\nFollow this contract exactly. Output maps must contain required_output_keys and no other keys.\n\n")))

(defn- kw-set [xs]
  (set (map keyword xs)))

(defn- mapcat-indexed [f coll]
  (apply concat (map-indexed f coll)))

(defn- map-entry-keys [select-expr]
  (when (= "map" (:op select-expr))
    (mapv (comp keyword :key) (:entries select-expr))))

(def ^:private predicate-ops #{"=" "not=" ">" "<" ">=" "<=" "and" "or" "not" "nil?" "some?"})

(defn- predicate-expr? [expr]
  (and (map? expr) (contains? predicate-ops (:op expr))))

(declare collect-field-errors validate-predicate-positions)

(def ^:private input-shape-metadata-keys #{:name :field :type :collection :arg_index :fields :field_shapes})

(defn- normalize-shape [shape]
  (cond
    (map? shape) (if (:type shape)
                   shape
                   {:type (or (get shape "type") "unknown")})
    (string? shape) {:type shape}
    (keyword? shape) {:type (name shape)}
    :else {:type "unknown"}))

(defn- shape-field-pair [field]
  (cond
    (map? field) (when-let [field-name (or (:name field) (:field field))]
                   [(keyword field-name) (normalize-shape field)])
    field [(keyword field) {:type "unknown"}]
    :else nil))

(defn- input-shape-fields [shape]
  (let [fields (:fields shape)]
    (cond
      (and (map? shape) (= "collection" (:type shape)) (map? (:items shape)))
      (set (map keyword (keys (:items shape))))
      (map? fields) (set (map keyword (keys fields)))
      (sequential? fields) (->> fields (keep shape-field-pair) (map first) set)
      (:field shape) #{(keyword (:field shape))}
      (map? shape) (->> shape keys (remove input-shape-metadata-keys) (map keyword) set)
      :else #{})))

(defn- input-shape-field-shapes [shape]
  (let [explicit-field-shapes (->> (:field_shapes shape)
                                   (map (fn [[k v]] [(keyword k) (normalize-shape v)]))
                                   (into {}))
        fields (:fields shape)
        fields-shapes (cond
                        (and (= "collection" (:type shape)) (map? (:items shape)))
                        (->> (:items shape)
                             (map (fn [[k v]] [(keyword k) (normalize-shape v)]))
                             (into {}))
                        (map? fields) (->> fields
                                           (map (fn [[k v]] [(keyword k) (normalize-shape v)]))
                                           (into {}))
                        (sequential? fields) (->> fields
                                                  (keep shape-field-pair)
                                                  (into {}))
                        :else {})
        descriptor-shapes (when (:field shape)
                            {(keyword (:field shape)) (normalize-shape shape)})
        direct-shapes (if (map? shape)
                        (->> shape
                             (remove (fn [[k _]] (contains? input-shape-metadata-keys k)))
                             (map (fn [[k v]] [(keyword k) (normalize-shape v)]))
                             (into {}))
                        {})]
    (merge direct-shapes descriptor-shapes fields-shapes explicit-field-shapes)))

(defn- query-input-shape* [contract]
  (let [shapes (:input_shapes contract)]
    (or (->> shapes
             (filter :collection)
             first)
        (when (and (seq shapes) (every? :field shapes))
          {:collection true
           :fields (mapv (fn [shape]
                            {:name (:field shape)
                             :type (:type shape)})
                          shapes)})
        (first shapes))))

(defn- query-input-fields [contract]
  (input-shape-fields (query-input-shape* contract)))

(defn- query-input-shape [contract]
  (when-let [shape (query-input-shape* contract)]
    (assoc shape
           :fields (vec (sort-by name (input-shape-fields shape)))
           :field_shapes (input-shape-field-shapes shape))))

(defn- field-errors [expr item-name valid-fields path]
  (let [target (:target expr)
        target-var (when (= "var" (:op target)) (:name target))
        field (keyword (:field expr))]
    (when (and (= item-name target-var)
               (seq valid-fields)
               (not (contains? valid-fields field)))
      [{:path path
        :field field
        :valid_fields (sort-by str valid-fields)
        :message (str "field " field " does not exist on input item " item-name)}])))

(defn collect-field-errors [expr env path]
  (when (map? expr)
    (let [op (:op expr)
          item-name (:item-name env)
          valid-fields (:valid-fields env)]
      (vec
       (concat
        (when (= "field" op)
          (field-errors expr item-name valid-fields path))
        (case op
          "field" (collect-field-errors (:target expr) env (str path ".target"))
          "call" (mapcat-indexed (fn [idx arg] (collect-field-errors arg env (str path ".args[" idx "]"))) (:args expr))
          "map" (mapcat-indexed (fn [idx entry] (collect-field-errors (:expr entry) env (str path ".entries[" idx "].expr"))) (:entries expr))
          "vector" (mapcat-indexed (fn [idx item] (collect-field-errors item env (str path ".items[" idx "]"))) (:items expr))
          "let" (concat
                 (mapcat-indexed (fn [idx binding] (collect-field-errors (:expr binding) env (str path ".bindings[" idx "].expr"))) (:bindings expr))
                 (collect-field-errors (:body expr) env (str path ".body")))
          "if" (concat
                (collect-field-errors (:condition expr) env (str path ".condition"))
                (collect-field-errors (:then expr) env (str path ".then"))
                (collect-field-errors (:else expr) env (str path ".else")))
          "cond" (concat
                  (mapcat-indexed (fn [idx clause]
                                    (concat
                                     (collect-field-errors (:condition clause) env (str path ".clauses[" idx "].condition"))
                                     (collect-field-errors (:expr clause) env (str path ".clauses[" idx "].expr"))))
                                  (:clauses expr))
                  (collect-field-errors (:else expr) env (str path ".else")))
          "query" (let [query-env (assoc env
                                         :item-name (:item expr)
                                         :valid-fields (or (query-input-fields (:contract env)) valid-fields))]
                    (concat
                     (collect-field-errors (:collection expr) env (str path ".collection"))
                     (mapcat-indexed (fn [idx where] (collect-field-errors where query-env (str path ".where[" idx "]"))) (:where expr))
                     (when-let [sort (:sort expr)]
                       (collect-field-errors (:expr sort) query-env (str path ".sort.expr")))
                     (collect-field-errors (:select expr) query-env (str path ".select"))))
          ("=" "not=" ">" "<" ">=" "<=") (concat
                                           (collect-field-errors (:left expr) env (str path ".left"))
                                           (collect-field-errors (:right expr) env (str path ".right")))
          ("and" "or") (mapcat-indexed (fn [idx arg] (collect-field-errors arg env (str path ".args[" idx "]"))) (:args expr))
          ("not" "nil?" "some?") (collect-field-errors (:expr expr) env (str path ".expr"))
          nil))))))

(defn- projection-errors [expr contract path]
  (when (map? expr)
    (let [op (:op expr)
          required (kw-set (:required_output_keys contract))]
      (vec
       (concat
        (when (and (= "query" op) (= "map" (get-in expr [:select :op])) (seq required))
          (let [actual (set (map-entry-keys (:select expr)))
                missing (clojure.set/difference required actual)
                extra (clojure.set/difference actual required)]
            (when (or (seq missing) (seq extra))
              [{:path (str path ".select.entries")
                :message "query select map keys must exactly match semantic required_output_keys"
                :required_keys (sort-by str required)
                :actual_keys (sort-by str actual)
                :missing_keys (sort-by str missing)
                :extra_keys (sort-by str extra)}])))
        (case op
          "field" (projection-errors (:target expr) contract (str path ".target"))
          "call" (mapcat-indexed (fn [idx arg] (projection-errors arg contract (str path ".args[" idx "]"))) (:args expr))
          "map" (mapcat-indexed (fn [idx entry] (projection-errors (:expr entry) contract (str path ".entries[" idx "].expr"))) (:entries expr))
          "vector" (mapcat-indexed (fn [idx item] (projection-errors item contract (str path ".items[" idx "]"))) (:items expr))
          "let" (concat
                 (mapcat-indexed (fn [idx binding] (projection-errors (:expr binding) contract (str path ".bindings[" idx "].expr"))) (:bindings expr))
                 (projection-errors (:body expr) contract (str path ".body")))
          "if" (concat
                (projection-errors (:condition expr) contract (str path ".condition"))
                (projection-errors (:then expr) contract (str path ".then"))
                (projection-errors (:else expr) contract (str path ".else")))
          "cond" (concat
                  (mapcat-indexed (fn [idx clause]
                                    (concat
                                     (projection-errors (:condition clause) contract (str path ".clauses[" idx "].condition"))
                                     (projection-errors (:expr clause) contract (str path ".clauses[" idx "].expr"))))
                                  (:clauses expr))
                  (projection-errors (:else expr) contract (str path ".else")))
          "query" (concat
                   (projection-errors (:collection expr) contract (str path ".collection"))
                   (mapcat-indexed (fn [idx where] (projection-errors where contract (str path ".where[" idx "]"))) (:where expr))
                   (when-let [sort (:sort expr)]
                     (projection-errors (:expr sort) contract (str path ".sort.expr")))
                   (projection-errors (:select expr) contract (str path ".select")))
          ("=" "not=" ">" "<" ">=" "<=") (concat
                                           (projection-errors (:left expr) contract (str path ".left"))
                                           (projection-errors (:right expr) contract (str path ".right")))
          ("and" "or") (mapcat-indexed (fn [idx arg] (projection-errors arg contract (str path ".args[" idx "]"))) (:args expr))
          ("not" "nil?" "some?") (projection-errors (:expr expr) contract (str path ".expr"))
          nil))))))

(defn- duplicate-key-errors [expr path]
  (when (map? expr)
    (let [op (:op expr)]
      (vec
       (concat
        (when (= "map" op)
          (let [keys (mapv :key (:entries expr))
                duplicates (->> keys frequencies (filter (fn [[_ n]] (> n 1))) (map first) (sort) vec)]
            (when (seq duplicates)
              [{:path (str path ".entries")
                :message "map entries must not contain duplicate keys"
                :duplicate_keys duplicates}])))
        (case op
          "field" (duplicate-key-errors (:target expr) (str path ".target"))
          "call" (mapcat-indexed (fn [idx arg] (duplicate-key-errors arg (str path ".args[" idx "]"))) (:args expr))
          "map" (mapcat-indexed (fn [idx entry] (duplicate-key-errors (:expr entry) (str path ".entries[" idx "].expr"))) (:entries expr))
          "vector" (mapcat-indexed (fn [idx item] (duplicate-key-errors item (str path ".items[" idx "]"))) (:items expr))
          "let" (concat
                 (mapcat-indexed (fn [idx binding] (duplicate-key-errors (:expr binding) (str path ".bindings[" idx "].expr"))) (:bindings expr))
                 (duplicate-key-errors (:body expr) (str path ".body")))
          "if" (concat
                (duplicate-key-errors (:condition expr) (str path ".condition"))
                (duplicate-key-errors (:then expr) (str path ".then"))
                (duplicate-key-errors (:else expr) (str path ".else")))
          "cond" (concat
                  (mapcat-indexed (fn [idx clause]
                                    (concat
                                     (duplicate-key-errors (:condition clause) (str path ".clauses[" idx "].condition"))
                                     (duplicate-key-errors (:expr clause) (str path ".clauses[" idx "].expr"))))
                                  (:clauses expr))
                  (duplicate-key-errors (:else expr) (str path ".else")))
          "query" (concat
                   (duplicate-key-errors (:collection expr) (str path ".collection"))
                   (mapcat-indexed (fn [idx where] (duplicate-key-errors where (str path ".where[" idx "]"))) (:where expr))
                   (when-let [sort (:sort expr)]
                     (duplicate-key-errors (:expr sort) (str path ".sort.expr")))
                   (duplicate-key-errors (:select expr) (str path ".select")))
          ("=" "not=" ">" "<" ">=" "<=") (concat
                                           (duplicate-key-errors (:left expr) (str path ".left"))
                                           (duplicate-key-errors (:right expr) (str path ".right")))
          ("and" "or") (mapcat-indexed (fn [idx arg] (duplicate-key-errors arg (str path ".args[" idx "]"))) (:args expr))
          ("not" "nil?" "some?") (duplicate-key-errors (:expr expr) (str path ".expr"))
          nil))))))

(defn- duplicate-cond-predicate-errors [expr path]
  (when (map? expr)
    (let [op (:op expr)]
      (vec
       (concat
        (when (= "cond" op)
          (let [conditions (mapv :condition (:clauses expr))
                duplicates (->> conditions
                                frequencies
                                (filter (fn [[_ n]] (> n 1)))
                                (map first)
                                vec)]
            (when (seq duplicates)
              [{:path (str path ".clauses")
                :message "cond block contains structurally duplicated clause conditions"
                :duplicate_conditions (json-safe duplicates)
                :hint "Each branch in a cond block must evaluate a completely unique condition. Use else for the final fallback instead of repeating a previous predicate."}])))
        (case op
          "field" (duplicate-cond-predicate-errors (:target expr) (str path ".target"))
          "call" (mapcat-indexed (fn [idx arg] (duplicate-cond-predicate-errors arg (str path ".args[" idx "]"))) (:args expr))
          "map" (mapcat-indexed (fn [idx entry] (duplicate-cond-predicate-errors (:expr entry) (str path ".entries[" idx "].expr"))) (:entries expr))
          "vector" (mapcat-indexed (fn [idx item] (duplicate-cond-predicate-errors item (str path ".items[" idx "]"))) (:items expr))
          "let" (concat
                 (mapcat-indexed (fn [idx binding] (duplicate-cond-predicate-errors (:expr binding) (str path ".bindings[" idx "].expr"))) (:bindings expr))
                 (duplicate-cond-predicate-errors (:body expr) (str path ".body")))
          "if" (concat
                (duplicate-cond-predicate-errors (:condition expr) (str path ".condition"))
                (duplicate-cond-predicate-errors (:then expr) (str path ".then"))
                (duplicate-cond-predicate-errors (:else expr) (str path ".else")))
          "cond" (concat
                  (mapcat-indexed (fn [idx clause]
                                    (concat
                                     (duplicate-cond-predicate-errors (:condition clause) (str path ".clauses[" idx "].condition"))
                                     (duplicate-cond-predicate-errors (:expr clause) (str path ".clauses[" idx "].expr"))))
                                  (:clauses expr))
                  (duplicate-cond-predicate-errors (:else expr) (str path ".else")))
          "query" (concat
                   (duplicate-cond-predicate-errors (:collection expr) (str path ".collection"))
                   (mapcat-indexed (fn [idx where] (duplicate-cond-predicate-errors where (str path ".where[" idx "]"))) (:where expr))
                   (when-let [sort (:sort expr)]
                     (duplicate-cond-predicate-errors (:expr sort) (str path ".sort.expr")))
                   (duplicate-cond-predicate-errors (:select expr) (str path ".select")))
          ("=" "not=" ">" "<" ">=" "<=") (concat
                                           (duplicate-cond-predicate-errors (:left expr) (str path ".left"))
                                           (duplicate-cond-predicate-errors (:right expr) (str path ".right")))
          ("and" "or") (mapcat-indexed (fn [idx arg] (duplicate-cond-predicate-errors arg (str path ".args[" idx "]"))) (:args expr))
          ("not" "nil?" "some?") (duplicate-cond-predicate-errors (:expr expr) (str path ".expr"))
          nil))))))

(def ^:private static-literal-ops #{"string" "int" "float" "bool" "nil"})
(def ^:private truth-value-ops #{"=" "not=" ">" "<" ">=" "<=" "and" "or" "not"})

(defn- literal-predicate-errors [expr path]
  (when (map? expr)
    (let [op (:op expr)]
      (vec
       (concat
        (when (and (#{"nil?" "some?" "not"} op)
                   (contains? static-literal-ops (get-in expr [:expr :op])))
          [{:path path
            :message (str "Semantic Logic Error: Predicate '" op "' cannot evaluate a static literal value.")
            :predicate op
            :literal_op (get-in expr [:expr :op])
            :hint "Evaluate a dynamic field path or predicate expression instead of a hardcoded literal."}])
        (when (and (#{"nil?" "some?"} op)
                   (contains? truth-value-ops (get-in expr [:expr :op])))
          [{:path path
            :message (str "Semantic Logic Error: Predicate '" op "' cannot test the nil-ness of a truth-value expression.")
            :predicate op
            :child_op (get-in expr [:expr :op])
            :hint "Use the truth-value expression directly as a condition, or test nil?/some? against a dynamic field path."}])
        (case op
          "field" (literal-predicate-errors (:target expr) (str path ".target"))
          "call" (mapcat-indexed (fn [idx arg] (literal-predicate-errors arg (str path ".args[" idx "]"))) (:args expr))
          "map" (mapcat-indexed (fn [idx entry] (literal-predicate-errors (:expr entry) (str path ".entries[" idx "].expr"))) (:entries expr))
          "vector" (mapcat-indexed (fn [idx item] (literal-predicate-errors item (str path ".items[" idx "]"))) (:items expr))
          "let" (concat
                 (mapcat-indexed (fn [idx binding] (literal-predicate-errors (:expr binding) (str path ".bindings[" idx "].expr"))) (:bindings expr))
                 (literal-predicate-errors (:body expr) (str path ".body")))
          "if" (concat
                (literal-predicate-errors (:condition expr) (str path ".condition"))
                (literal-predicate-errors (:then expr) (str path ".then"))
                (literal-predicate-errors (:else expr) (str path ".else")))
          "cond" (concat
                  (mapcat-indexed (fn [idx clause]
                                    (concat
                                     (literal-predicate-errors (:condition clause) (str path ".clauses[" idx "].condition"))
                                     (literal-predicate-errors (:expr clause) (str path ".clauses[" idx "].expr"))))
                                  (:clauses expr))
                  (literal-predicate-errors (:else expr) (str path ".else")))
          "query" (concat
                   (literal-predicate-errors (:collection expr) (str path ".collection"))
                   (mapcat-indexed (fn [idx where] (literal-predicate-errors where (str path ".where[" idx "]"))) (:where expr))
                   (when-let [sort (:sort expr)]
                     (literal-predicate-errors (:expr sort) (str path ".sort.expr")))
                   (literal-predicate-errors (:select expr) (str path ".select")))
          ("=" "not=" ">" "<" ">=" "<=") (concat
                                           (literal-predicate-errors (:left expr) (str path ".left"))
                                           (literal-predicate-errors (:right expr) (str path ".right")))
          ("and" "or") (mapcat-indexed (fn [idx arg] (literal-predicate-errors arg (str path ".args[" idx "]"))) (:args expr))
          ("not" "nil?" "some?") (literal-predicate-errors (:expr expr) (str path ".expr"))
          nil))))))

(defn- scalar-base-type [shape]
  (let [type-text (str/lower-case (str (:type shape)))]
    (cond
      (re-find #"\bstring\b" type-text) "string"
      (re-find #"\bnumber\b" type-text) "number"
      (re-find #"\bboolean\b" type-text) "boolean"
      (#{"string" "number" "boolean"} (:type shape)) (:type shape)
      :else nil)))

(defn- nullable-scalar-shape? [shape]
  (let [type-text (str/lower-case (str (:type shape)))]
    (boolean
     (and (scalar-base-type shape)
          (or (:nullable shape)
              (re-find #"nullable|missing|_or_|/or/|\s+or\s+" type-text))))))

(defn- strict-scalar-shape? [shape]
  (boolean
   (and (scalar-base-type shape)
        (not (nullable-scalar-shape? shape))
        (#{"string" "number" "boolean"} (:type shape)))))

(defn- collapse-nullable-fallback-shape [shapes fallback-shape]
  (when (strict-scalar-shape? fallback-shape)
    (let [fallback-base (scalar-base-type fallback-shape)
          bases (set (keep scalar-base-type shapes))]
      (when (and (= #{fallback-base} bases)
                 (some nullable-scalar-shape? shapes)
                 (some strict-scalar-shape? shapes))
        {:type fallback-base}))))

(defn- merge-branch-shapes
  ([shapes]
   (merge-branch-shapes shapes nil))
  ([shapes fallback-shape]
   (let [shapes (vec (remove nil? shapes))
         types (set (map :type shapes))
         collapsed (collapse-nullable-fallback-shape shapes fallback-shape)]
    (cond
      (empty? shapes) {:type "unknown"}
      collapsed collapsed
      (= 1 (count types)) (first shapes)
      (every? scalar-shape? shapes) {:type "scalar"}
      :else {:type "mixed" :observed shapes}))))

(declare expr-shape)

(defn- expr-shape [expr env]
  (if-not (map? expr)
    {:type "unknown"}
    (let [op (:op expr)]
      (case op
        "string" {:type "string"}
        "int" {:type "number"}
        "float" {:type "number"}
        "bool" {:type "boolean"}
        "nil" {:type "nil"}
        "var" (or (get-in env [:vars (:name expr)]) {:type "unknown"})
        "field" (let [target (:target expr)
                      target-var (when (= "var" (:op target)) (:name target))
                      field (keyword (:field expr))]
                  (if (= target-var (:item-name env))
                    (or (get (:field-shapes env) field) {:type "unknown"})
                    {:type "unknown"}))
        "call" {:type "unknown"}
        "map" {:type "map"}
        "vector" {:type "vector"}
        "query" {:type "vector"}
        ("=" "not=" ">" "<" ">=" "<=" "and" "or" "not" "nil?" "some?") {:type "boolean"}
        "if" (let [then-shape (expr-shape (:then expr) env)
                   else-shape (expr-shape (:else expr) env)]
               (merge-branch-shapes [then-shape else-shape] else-shape))
        "cond" (let [else-shape (expr-shape (:else expr) env)]
                 (merge-branch-shapes
                  (concat
                   (map (fn [clause] (expr-shape (:expr clause) env)) (:clauses expr))
                   [else-shape])
                  else-shape))
        "let" (let [binding-env
                    (reduce (fn [acc binding]
                              (assoc-in acc [:vars (:name binding)] (expr-shape (:expr binding) acc)))
                            env
                            (:bindings expr))]
                (expr-shape (:body expr) binding-env))
        {:type "unknown"}))))

(defn- scalar-compatible-shape? [expected actual]
  (cond
    (not (scalar-shape? expected)) true
    (scalar-shape? actual) true
    (= "scalar" (:type actual)) true
    (= "unknown" (:type actual)) true
    :else false))

(defn- non-nullable-shape? [shape]
  (and (map? shape)
       (not (:nullable shape))
       (not (str/includes? (str (:type shape)) "nullable"))
       (not= "nil" (:type shape))
       (not= "unknown" (:type shape))))

(defn- comparison-type-error [expr env path]
  (let [left-shape (expr-shape (:left expr) env)
        right-shape (expr-shape (:right expr) env)]
    (when-not (and (= "number" (:type left-shape))
                   (= "number" (:type right-shape)))
      [{:path path
        :message (str "Type Error: Operator '" (:op expr) "' expects numbers, but received '"
                      (:type left-shape) "' and '" (:type right-shape) "'")
        :operator (:op expr)
        :expected ["number" "number"]
        :actual [(:type left-shape) (:type right-shape)]
        :left_shape left-shape
        :right_shape right-shape}])))

(defn- numeric-literal-value [expr]
  (when (map? expr)
    (case (:op expr)
      "int" (:value expr)
      "float" (:value expr)
      nil)))

(defn- field-expr-name [expr env]
  (when (map? expr)
    (let [target (:target expr)
          target-var (when (= "var" (:op target)) (:name target))]
      (when (and (= "field" (:op expr))
                 (= target-var (:item-name env)))
        (name (:field expr))))))

(defn- threshold-invariant-for [contract field-name value]
  (some (fn [invariant]
          (when (and (= (name (:field invariant)) field-name)
                     (= (:value invariant) value))
            invariant))
        (:threshold_invariants contract)))

(defn- threshold-direction-error [expr contract env path]
  (when (#{"<" ">" "<=" ">="} (:op expr))
    (let [left-field (field-expr-name (:left expr) env)
          right-field (field-expr-name (:right expr) env)
          left-value (numeric-literal-value (:left expr))
          right-value (numeric-literal-value (:right expr))
          field-name (or left-field right-field)
          value (or right-value left-value)
          generated-op (if left-field
                         (:op expr)
                         ({">" "<" "<" ">" ">=" "<=" "<=" ">="} (:op expr)))
          invariant (when (and field-name (some? value))
                      (threshold-invariant-for contract field-name value))]
      (when (and invariant
                 (not= (:operator invariant) generated-op))
        [{:path path
          :message (str "Directional Threshold Inversion: spec/plan rules mandate operator '"
                        (:operator invariant) "' for " field-name " against threshold value "
                        value ", but generated code used '" generated-op "'.")
          :field field-name
          :threshold value
          :expected_operator (:operator invariant)
          :actual_operator generated-op
          :source (:source invariant)
          :hint "Use the threshold direction from the text spec or planner truth table; do not invert greater-than and less-than boundaries."}]))))

(defn- threshold-direction-errors
  ([expr contract path]
   (threshold-direction-errors expr contract path {}))
  ([expr contract path env]
   (when (map? expr)
     (let [op (:op expr)]
       (vec
        (concat
         (threshold-direction-error expr contract env path)
         (case op
           "field" (threshold-direction-errors (:target expr) contract (str path ".target") env)
           "call" (mapcat-indexed (fn [idx arg] (threshold-direction-errors arg contract (str path ".args[" idx "]") env)) (:args expr))
           "map" (mapcat-indexed (fn [idx entry] (threshold-direction-errors (:expr entry) contract (str path ".entries[" idx "].expr") env)) (:entries expr))
           "vector" (mapcat-indexed (fn [idx item] (threshold-direction-errors item contract (str path ".items[" idx "]") env)) (:items expr))
           "let" (concat
                  (mapcat-indexed (fn [idx binding] (threshold-direction-errors (:expr binding) contract (str path ".bindings[" idx "].expr") env)) (:bindings expr))
                  (threshold-direction-errors (:body expr) contract (str path ".body") env))
           "if" (concat
                 (threshold-direction-errors (:condition expr) contract (str path ".condition") env)
                 (threshold-direction-errors (:then expr) contract (str path ".then") env)
                 (threshold-direction-errors (:else expr) contract (str path ".else") env))
           "cond" (concat
                   (mapcat-indexed (fn [idx clause]
                                     (concat
                                      (threshold-direction-errors (:condition clause) contract (str path ".clauses[" idx "].condition") env)
                                      (threshold-direction-errors (:expr clause) contract (str path ".clauses[" idx "].expr") env)))
                                   (:clauses expr))
                   (threshold-direction-errors (:else expr) contract (str path ".else") env))
           "query" (let [input-shape (query-input-shape contract)
                         query-env (assoc env
                                          :item-name (:item expr)
                                          :field-shapes (:field_shapes input-shape))]
                     (concat
                      (threshold-direction-errors (:collection expr) contract (str path ".collection") env)
                      (mapcat-indexed (fn [idx where] (threshold-direction-errors where contract (str path ".where[" idx "]") query-env)) (:where expr))
                      (when-let [sort (:sort expr)]
                        (threshold-direction-errors (:expr sort) contract (str path ".sort.expr") query-env))
                      (threshold-direction-errors (:select expr) contract (str path ".select") query-env)))
           ("=" "not=" ">" "<" ">=" "<=") (concat
                                            (threshold-direction-errors (:left expr) contract (str path ".left") env)
                                            (threshold-direction-errors (:right expr) contract (str path ".right") env))
           ("and" "or") (mapcat-indexed (fn [idx arg] (threshold-direction-errors arg contract (str path ".args[" idx "]") env)) (:args expr))
           ("not" "nil?" "some?") (threshold-direction-errors (:expr expr) contract (str path ".expr") env)
           nil)))))))

(defn- type-flow-errors
  ([expr contract path]
   (type-flow-errors expr contract path {}))
  ([expr contract path env]
   (when (map? expr)
     (let [op (:op expr)]
       (vec
        (concat
         (when (#{"<" ">" "<=" ">="} op)
           (comparison-type-error expr env path))
         (case op
           "field" (type-flow-errors (:target expr) contract (str path ".target") env)
           "call" (mapcat-indexed (fn [idx arg] (type-flow-errors arg contract (str path ".args[" idx "]") env)) (:args expr))
           "map" (mapcat-indexed (fn [idx entry] (type-flow-errors (:expr entry) contract (str path ".entries[" idx "].expr") env)) (:entries expr))
           "vector" (mapcat-indexed (fn [idx item] (type-flow-errors item contract (str path ".items[" idx "]") env)) (:items expr))
           "let" (let [binding-env
                       (reduce (fn [acc binding]
                                 (let [binding-path (str path ".bindings[" (:idx binding) "].expr")]
                                   (assoc-in acc [:vars (:name binding)]
                                             (expr-shape (:expr binding) acc))))
                               env
                               (map-indexed (fn [idx binding] (assoc binding :idx idx)) (:bindings expr)))]
                   (concat
                    (mapcat-indexed (fn [idx binding] (type-flow-errors (:expr binding) contract (str path ".bindings[" idx "].expr") env)) (:bindings expr))
                    (type-flow-errors (:body expr) contract (str path ".body") binding-env)))
           "if" (concat
                 (type-flow-errors (:condition expr) contract (str path ".condition") env)
                 (type-flow-errors (:then expr) contract (str path ".then") env)
                 (type-flow-errors (:else expr) contract (str path ".else") env))
           "cond" (concat
                   (mapcat-indexed (fn [idx clause]
                                     (concat
                                      (type-flow-errors (:condition clause) contract (str path ".clauses[" idx "].condition") env)
                                      (type-flow-errors (:expr clause) contract (str path ".clauses[" idx "].expr") env)))
                                   (:clauses expr))
                   (type-flow-errors (:else expr) contract (str path ".else") env))
           "query" (let [input-shape (query-input-shape contract)
                         query-env (assoc env
                                          :item-name (:item expr)
                                          :field-shapes (:field_shapes input-shape))]
                     (concat
                      (type-flow-errors (:collection expr) contract (str path ".collection") env)
                      (mapcat-indexed (fn [idx where] (type-flow-errors where contract (str path ".where[" idx "]") query-env)) (:where expr))
                      (when-let [sort (:sort expr)]
                        (type-flow-errors (:expr sort) contract (str path ".sort.expr") query-env))
                      (type-flow-errors (:select expr) contract (str path ".select") query-env)))
           ("=" "not=" ">" "<" ">=" "<=") (concat
                                            (type-flow-errors (:left expr) contract (str path ".left") env)
                                            (type-flow-errors (:right expr) contract (str path ".right") env))
           ("and" "or") (mapcat-indexed (fn [idx arg] (type-flow-errors arg contract (str path ".args[" idx "]") env)) (:args expr))
           ("not" "nil?" "some?") (type-flow-errors (:expr expr) contract (str path ".expr") env)
           nil)))))))

(defn- output-shape-errors
  ([expr contract path]
   (output-shape-errors expr contract path {}))
  ([expr contract path env]
   (when (map? expr)
     (let [op (:op expr)
           expected-shapes (:output_key_shapes contract)]
       (vec
        (concat
         (when (= "map" op)
           (mapcat-indexed
            (fn [idx entry]
              (let [entry-key (keyword (:key entry))
                    expected (get expected-shapes entry-key)
                    actual (expr-shape (:expr entry) env)]
                (concat
                 (when (and (= "nil" (get-in entry [:expr :op]))
                            (non-nullable-shape? expected))
                   [{:path (str path ".entries[" idx "].expr")
                     :key entry-key
                     :message "projection value cannot be literal nil for a non-nullable output key"
                     :expected_shape expected
                     :actual_shape actual
                     :hint "Generate a valid fallback value or dynamic field expression for this output key."}])
                 (when-not (scalar-compatible-shape? expected actual)
                   [{:path (str path ".entries[" idx "].expr")
                     :key entry-key
                     :message "projection value shape does not match semantic fixture"
                     :expected_shape expected
                     :actual_shape actual
                     :hint "scalar output keys may use cond, if, or let only when their returned branches are scalar"}]))))
            (:entries expr)))
         (case op
           "field" (output-shape-errors (:target expr) contract (str path ".target") env)
           "call" (mapcat-indexed (fn [idx arg] (output-shape-errors arg contract (str path ".args[" idx "]") env)) (:args expr))
           "map" (mapcat-indexed (fn [idx entry] (output-shape-errors (:expr entry) contract (str path ".entries[" idx "].expr") env)) (:entries expr))
           "vector" (mapcat-indexed (fn [idx item] (output-shape-errors item contract (str path ".items[" idx "]") env)) (:items expr))
           "let" (concat
                  (mapcat-indexed (fn [idx binding] (output-shape-errors (:expr binding) contract (str path ".bindings[" idx "].expr") env)) (:bindings expr))
                  (output-shape-errors (:body expr) contract (str path ".body") env))
           "if" (concat
                 (output-shape-errors (:condition expr) contract (str path ".condition") env)
                 (output-shape-errors (:then expr) contract (str path ".then") env)
                 (output-shape-errors (:else expr) contract (str path ".else") env))
           "cond" (concat
                   (mapcat-indexed (fn [idx clause]
                                     (concat
                                      (output-shape-errors (:condition clause) contract (str path ".clauses[" idx "].condition") env)
                                      (output-shape-errors (:expr clause) contract (str path ".clauses[" idx "].expr") env)))
                                   (:clauses expr))
                   (output-shape-errors (:else expr) contract (str path ".else") env))
           "query" (let [input-shape (query-input-shape contract)
                         query-env (assoc env
                                          :item-name (:item expr)
                                          :field-shapes (:field_shapes input-shape))]
                     (concat
                      (output-shape-errors (:collection expr) contract (str path ".collection") env)
                      (mapcat-indexed (fn [idx where] (output-shape-errors where contract (str path ".where[" idx "]") query-env)) (:where expr))
                      (when-let [sort (:sort expr)]
                        (output-shape-errors (:expr sort) contract (str path ".sort.expr") query-env))
                      (output-shape-errors (:select expr) contract (str path ".select") query-env)))
           ("=" "not=" ">" "<" ">=" "<=") (concat
                                            (output-shape-errors (:left expr) contract (str path ".left") env)
                                            (output-shape-errors (:right expr) contract (str path ".right") env))
           ("and" "or") (mapcat-indexed (fn [idx arg] (output-shape-errors arg contract (str path ".args[" idx "]") env)) (:args expr))
           ("not" "nil?" "some?") (output-shape-errors (:expr expr) contract (str path ".expr") env)
           nil)))))))

(defn- literal-expr-value [expr]
  (when (map? expr)
    (case (:op expr)
      "string" {:known true :value (:value expr)}
      "int" {:known true :value (:value expr)}
      "float" {:known true :value (:value expr)}
      "bool" {:known true :value (:value expr)}
      "nil" {:known true :value nil}
      nil)))

(defn- invalid-output-literal-error [expr output-key allowed-values path]
  (when-let [{:keys [value]} (literal-expr-value expr)]
    (when-not (contains? allowed-values value)
      [{:path path
        :key output-key
        :message "projection literal value is not valid for output key"
        :value value
        :valid_values (sort-by pr-str allowed-values)
        :hint "Do not reuse literal labels from other output keys; choose a value observed for this output key in the semantic fixture."}])))

(defn- result-literal-errors [expr output-key allowed-values path]
  (when (map? expr)
    (let [op (:op expr)]
      (vec
       (concat
        (invalid-output-literal-error expr output-key allowed-values path)
        (case op
          "if" (concat
                (result-literal-errors (:then expr) output-key allowed-values (str path ".then"))
                (result-literal-errors (:else expr) output-key allowed-values (str path ".else")))
          "cond" (concat
                  (mapcat-indexed (fn [idx clause]
                                    (result-literal-errors (:expr clause)
                                                           output-key
                                                           allowed-values
                                                           (str path ".clauses[" idx "].expr")))
                                  (:clauses expr))
                  (result-literal-errors (:else expr) output-key allowed-values (str path ".else")))
          "let" (result-literal-errors (:body expr) output-key allowed-values (str path ".body"))
          "map" (mapcat-indexed (fn [idx entry]
                                  (result-literal-errors (:expr entry)
                                                         output-key
                                                         allowed-values
                                                         (str path ".entries[" idx "].expr")))
                                (:entries expr))
          "vector" (mapcat-indexed (fn [idx item]
                                     (result-literal-errors item output-key allowed-values (str path ".items[" idx "]")))
                                   (:items expr))
          "query" (result-literal-errors (:select expr) output-key allowed-values (str path ".select"))
          nil))))))

(defn- output-value-errors
  ([expr contract path]
   (output-value-errors expr contract path {}))
  ([expr contract path env]
   (when (map? expr)
     (let [op (:op expr)
           output-values (:output_key_values contract)]
       (vec
        (concat
         (when (= "map" op)
           (mapcat-indexed
            (fn [idx entry]
              (let [entry-key (keyword (:key entry))
                    allowed-values (set (get-in output-values [entry-key :values]))]
                (when (seq allowed-values)
                  (result-literal-errors (:expr entry)
                                         entry-key
                                         allowed-values
                                         (str path ".entries[" idx "].expr")))))
            (:entries expr)))
         (case op
           "field" (output-value-errors (:target expr) contract (str path ".target") env)
           "call" (mapcat-indexed (fn [idx arg] (output-value-errors arg contract (str path ".args[" idx "]") env)) (:args expr))
           "map" (mapcat-indexed (fn [idx entry] (output-value-errors (:expr entry) contract (str path ".entries[" idx "].expr") env)) (:entries expr))
           "vector" (mapcat-indexed (fn [idx item] (output-value-errors item contract (str path ".items[" idx "]") env)) (:items expr))
           "let" (concat
                  (mapcat-indexed (fn [idx binding] (output-value-errors (:expr binding) contract (str path ".bindings[" idx "].expr") env)) (:bindings expr))
                  (output-value-errors (:body expr) contract (str path ".body") env))
           "if" (concat
                 (output-value-errors (:condition expr) contract (str path ".condition") env)
                 (output-value-errors (:then expr) contract (str path ".then") env)
                 (output-value-errors (:else expr) contract (str path ".else") env))
           "cond" (concat
                   (mapcat-indexed (fn [idx clause]
                                     (concat
                                      (output-value-errors (:condition clause) contract (str path ".clauses[" idx "].condition") env)
                                      (output-value-errors (:expr clause) contract (str path ".clauses[" idx "].expr") env)))
                                   (:clauses expr))
                   (output-value-errors (:else expr) contract (str path ".else") env))
           "query" (concat
                    (output-value-errors (:collection expr) contract (str path ".collection") env)
                    (mapcat-indexed (fn [idx where] (output-value-errors where contract (str path ".where[" idx "]") env)) (:where expr))
                    (when-let [sort (:sort expr)]
                      (output-value-errors (:expr sort) contract (str path ".sort.expr") env))
                    (output-value-errors (:select expr) contract (str path ".select") env))
           ("=" "not=" ">" "<" ">=" "<=") (concat
                                            (output-value-errors (:left expr) contract (str path ".left") env)
                                            (output-value-errors (:right expr) contract (str path ".right") env))
           ("and" "or") (mapcat-indexed (fn [idx arg] (output-value-errors arg contract (str path ".args[" idx "]") env)) (:args expr))
           ("not" "nil?" "some?") (output-value-errors (:expr expr) contract (str path ".expr") env)
           nil)))))))

(defn validate-predicate-positions [expr path]
  (when (map? expr)
    (let [op (:op expr)]
      (vec
       (concat
        (case op
          "if" (concat
                (when-not (predicate-expr? (:condition expr))
                  [{:path (str path ".condition")
                    :message "if condition must use a predicate op"
                    :op (:op (:condition expr))
                    :allowed_ops (sort predicate-ops)}])
                (validate-predicate-positions (:condition expr) (str path ".condition"))
                (validate-predicate-positions (:then expr) (str path ".then"))
                (validate-predicate-positions (:else expr) (str path ".else")))
          "cond" (concat
                  (mapcat-indexed (fn [idx clause]
                                    (concat
                                     (when-not (predicate-expr? (:condition clause))
                                       [{:path (str path ".clauses[" idx "].condition")
                                         :message "cond clause condition must use a predicate op"
                                         :op (:op (:condition clause))
                                         :allowed_ops (sort predicate-ops)}])
                                     (validate-predicate-positions (:condition clause) (str path ".clauses[" idx "].condition"))
                                     (validate-predicate-positions (:expr clause) (str path ".clauses[" idx "].expr"))))
                                  (:clauses expr))
                  (validate-predicate-positions (:else expr) (str path ".else")))
          "query" (concat
                   (validate-predicate-positions (:collection expr) (str path ".collection"))
                   (mapcat-indexed (fn [idx where]
                                     (concat
                                      (when-not (predicate-expr? where)
                                        [{:path (str path ".where[" idx "]")
                                          :message "query where expression must use a predicate op"
                                          :op (:op where)
                                          :allowed_ops (sort predicate-ops)}])
                                      (validate-predicate-positions where (str path ".where[" idx "]"))))
                                   (:where expr))
                   (when-let [sort (:sort expr)]
                     (validate-predicate-positions (:expr sort) (str path ".sort.expr")))
                   (validate-predicate-positions (:select expr) (str path ".select")))
          "field" (validate-predicate-positions (:target expr) (str path ".target"))
          "call" (mapcat-indexed (fn [idx arg] (validate-predicate-positions arg (str path ".args[" idx "]"))) (:args expr))
          "map" (mapcat-indexed (fn [idx entry] (validate-predicate-positions (:expr entry) (str path ".entries[" idx "].expr"))) (:entries expr))
          "vector" (mapcat-indexed (fn [idx item] (validate-predicate-positions item (str path ".items[" idx "]"))) (:items expr))
          "let" (concat
                 (mapcat-indexed (fn [idx binding] (validate-predicate-positions (:expr binding) (str path ".bindings[" idx "].expr"))) (:bindings expr))
                 (validate-predicate-positions (:body expr) (str path ".body")))
          ("=" "not=" ">" "<" ">=" "<=") (concat
                                           (validate-predicate-positions (:left expr) (str path ".left"))
                                           (validate-predicate-positions (:right expr) (str path ".right")))
          ("and" "or") (mapcat-indexed (fn [idx arg]
                                         (concat
                                          (when-not (predicate-expr? arg)
                                            [{:path (str path ".args[" idx "]")
                                              :message (str op " argument must use a predicate op")
                                              :op (:op arg)
                                              :allowed_ops (sort predicate-ops)}])
                                          (validate-predicate-positions arg (str path ".args[" idx "]"))))
                                       (:args expr))
          "not" (concat
                 (when-not (predicate-expr? (:expr expr))
                   [{:path (str path ".expr")
                     :message "not argument must use a predicate op"
                     :op (:op (:expr expr))
                     :allowed_ops (sort predicate-ops)
                     :hint "Use some? to test that a field is present and nil? to test that a field is missing; do not use not over a raw field."}])
                 (validate-predicate-positions (:expr expr) (str path ".expr")))
          ("nil?" "some?") (validate-predicate-positions (:expr expr) (str path ".expr"))
          nil))))))

(defn validate-semantic-structure [ast contract]
  (if-not contract
    {:ok true}
    (let [defs (:defs ast)
          projection-errors (mapcat-indexed (fn [idx defn-ast]
                                              (projection-errors (:expr defn-ast) contract (str "$.defs[" idx "].expr")))
                                            defs)
          field-errors (mapcat-indexed (fn [idx defn-ast]
                                         (collect-field-errors (:expr defn-ast)
                                                               {:contract contract}
                                                               (str "$.defs[" idx "].expr")))
                                       defs)
          duplicate-key-errors (mapcat-indexed (fn [idx defn-ast]
                                                 (duplicate-key-errors (:expr defn-ast) (str "$.defs[" idx "].expr")))
                                               defs)
          duplicate-cond-predicate-errors (mapcat-indexed (fn [idx defn-ast]
                                                            (duplicate-cond-predicate-errors (:expr defn-ast) (str "$.defs[" idx "].expr")))
                                                          defs)
          predicate-errors (mapcat-indexed (fn [idx defn-ast]
                                             (validate-predicate-positions (:expr defn-ast) (str "$.defs[" idx "].expr")))
                                           defs)
          literal-predicate-errors (mapcat-indexed (fn [idx defn-ast]
                                                     (literal-predicate-errors (:expr defn-ast) (str "$.defs[" idx "].expr")))
                                                   defs)
          output-shape-errors (mapcat-indexed (fn [idx defn-ast]
                                                (output-shape-errors (:expr defn-ast) contract (str "$.defs[" idx "].expr")))
                                              defs)
          type-flow-errors (mapcat-indexed (fn [idx defn-ast]
                                             (type-flow-errors (:expr defn-ast) contract (str "$.defs[" idx "].expr")))
                                           defs)
          threshold-direction-errors (mapcat-indexed (fn [idx defn-ast]
                                                       (threshold-direction-errors (:expr defn-ast) contract (str "$.defs[" idx "].expr")))
                                                     defs)
          output-value-errors (mapcat-indexed (fn [idx defn-ast]
                                                (output-value-errors (:expr defn-ast) contract (str "$.defs[" idx "].expr")))
                                              defs)
          errors (vec (concat projection-errors
                              field-errors
                              duplicate-key-errors
                              duplicate-cond-predicate-errors
                              predicate-errors
                              literal-predicate-errors
                              output-shape-errors
                              type-flow-errors
                              threshold-direction-errors
                              output-value-errors))]
      (if (seq errors)
        {:ok false
         :exit_code 1
         :stdout ""
         :stderr (str "[json-ast/semantic-structure] " (count errors) " semantic structure error(s)")
         :validator "semantic-structure"
         :semantic_structure {:errors (json-safe errors)}}
        {:ok true
         :exit_code 0
         :stdout "Semantic structure OK\n"
         :stderr ""
         :validator "semantic-structure"}))))

(def ^:private where-failed-message
  "[json-ast/where-failed] Specification demands filtering criteria, but the where-pass returned an empty constraint vector []. All model predicate generation attempts were rejected or bypassed.")

(defn- required-filter-text? [value]
  (boolean
   (and value
        (re-find #"(?i)\b(filter|where|keep only|keeps only|include only|exclude|omit|drop|remove|only rows|rows where|records where|must have|should have|without)\b"
                 (str value)))))

(defn- filter-required? [contract]
  (boolean
   (or (:filter_required contract)
       (seq (:filter_rules contract))
       (seq (:filtering_rules contract))
       (seq (:where_rules contract))
       (seq (:filter_conditions contract))
       (seq (:filtering_conditions contract))
       (required-filter-text? (:filter_rule contract))
       (required-filter-text? (:filtering_rule contract))
       (required-filter-text? (:where_rule contract))
       (required-filter-text? (:filtering_logic contract))
       (some required-filter-text? (:rules contract))
       (some required-filter-text? (:conditions contract)))))

(defn- collect-query-wheres [expr path]
  (when (map? expr)
    (let [op (:op expr)]
      (vec
       (concat
        (when (= "query" op)
          [{:path (str path ".where")
            :where (:where expr)}])
        (case op
          "field" (collect-query-wheres (:target expr) (str path ".target"))
          "call" (mapcat-indexed (fn [idx arg] (collect-query-wheres arg (str path ".args[" idx "]"))) (:args expr))
          "map" (mapcat-indexed (fn [idx entry] (collect-query-wheres (:expr entry) (str path ".entries[" idx "].expr"))) (:entries expr))
          "vector" (mapcat-indexed (fn [idx item] (collect-query-wheres item (str path ".items[" idx "]"))) (:items expr))
          "let" (concat
                 (mapcat-indexed (fn [idx binding] (collect-query-wheres (:expr binding) (str path ".bindings[" idx "].expr"))) (:bindings expr))
                 (collect-query-wheres (:body expr) (str path ".body")))
          "if" (concat
                (collect-query-wheres (:condition expr) (str path ".condition"))
                (collect-query-wheres (:then expr) (str path ".then"))
                (collect-query-wheres (:else expr) (str path ".else")))
          "cond" (concat
                  (mapcat-indexed (fn [idx clause]
                                    (concat
                                     (collect-query-wheres (:condition clause) (str path ".clauses[" idx "].condition"))
                                     (collect-query-wheres (:expr clause) (str path ".clauses[" idx "].expr"))))
                                  (:clauses expr))
                  (collect-query-wheres (:else expr) (str path ".else")))
          "query" (concat
                   (collect-query-wheres (:collection expr) (str path ".collection"))
                   (mapcat-indexed (fn [idx where] (collect-query-wheres where (str path ".where[" idx "]"))) (:where expr))
                   (when-let [sort (:sort expr)]
                     (collect-query-wheres (:expr sort) (str path ".sort.expr")))
                   (collect-query-wheres (:select expr) (str path ".select")))
          ("=" "not=" ">" "<" ">=" "<=") (concat
                                           (collect-query-wheres (:left expr) (str path ".left"))
                                           (collect-query-wheres (:right expr) (str path ".right")))
          ("and" "or") (mapcat-indexed (fn [idx arg] (collect-query-wheres arg (str path ".args[" idx "]"))) (:args expr))
          ("not" "nil?" "some?") (collect-query-wheres (:expr expr) (str path ".expr"))
          nil))))))

(defn validate-where-completeness [contract ast]
  (let [query-wheres (vec (mapcat-indexed (fn [idx defn-ast]
                                            (collect-query-wheres (:expr defn-ast)
                                                                 (str "$.defs[" idx "].expr")))
                                          (:defs ast)))
        empty-where? (or (empty? query-wheres)
                         (every? #(empty? (:where %)) query-wheres))]
    (if (and (filter-required? contract) empty-where?)
      {:ok false
       :exit_code 1
       :stdout ""
       :stderr where-failed-message
       :validator "where-completeness"
       :where_failed {:filter_required true
                      :query_wheres (json-safe query-wheres)}}
      {:ok true
       :exit_code 0
       :stdout "Where completeness OK\n"
       :stderr ""
       :validator "where-completeness"})))

(defn- map-entry-array-schema? [schema]
  (and (= "array" (get schema "type"))
       (map? (get schema "items"))
       (= #{"key" "expr"} (set (get-in schema ["items" "required"])))))

(defn- value-expr-def-name [key-name]
  (str "valueExpr-" (name key-name)))

(defn- string-output-values [output-values key-name]
  (let [values (get-in output-values [(keyword key-name) :values])]
    (->> values (filter string?) distinct sort vec)))

(defn- value-expr-ref [key-name]
  {"$ref" (str "#/$defs/" (value-expr-def-name key-name))})

(defn- exact-map-entry-prefix-items [entry-schema allowed-keys output-values]
  (mapv (fn [key-name]
          (let [entry-schema (assoc-in entry-schema ["properties" "key"] {"const" (name key-name)})]
            (if (seq (string-output-values output-values key-name))
              (assoc-in entry-schema ["properties" "expr"] (value-expr-ref key-name))
              entry-schema)))
        allowed-keys))

(defn- restrict-map-entry-keys [schema allowed-keys output-values]
  (cond
    (and (map? schema)
         (= ["#/$defs" "name"] (some-> (get schema "$ref") (str/split #"/"))))
    schema

    (map? schema)
    (let [path-key-schema? (and (contains? schema "properties")
                                (contains? (get schema "properties") "key")
                                (contains? (get schema "properties") "expr"))
          entry-array-schema? (map-entry-array-schema? schema)
          required-count (count allowed-keys)]
      (cond-> (into {} (map (fn [[k v]] [k (restrict-map-entry-keys v allowed-keys output-values)]) schema))
        path-key-schema?
        (assoc-in ["properties" "key"] {"enum" (mapv name allowed-keys)})

        (and entry-array-schema? (pos? required-count))
        (-> (assoc "prefixItems" (exact-map-entry-prefix-items (restrict-map-entry-keys (get schema "items") allowed-keys output-values)
                                                                allowed-keys
                                                                output-values))
            (dissoc "items")
            (assoc "minItems" required-count "maxItems" required-count))))

    (vector? schema)
    (mapv #(restrict-map-entry-keys % allowed-keys output-values) schema)

    :else schema))

(defn- expr-alt-op [alt]
  (let [op-schema (get-in alt ["properties" "op"])]
    (or (get op-schema "const")
        (when-let [values (get op-schema "enum")]
          (set values)))))

(defn- value-expr-schema [base-expr-schema key-name allowed-strings]
  (let [self-ref (value-expr-ref key-name)]
    (assoc base-expr-schema
           "oneOf"
           (mapv (fn [alt]
                   (let [op (expr-alt-op alt)]
                     (cond
                       (= "string" op)
                       (assoc-in alt ["properties" "value"] {"enum" allowed-strings})

                       (= "if" op)
                       (-> alt
                           (assoc-in ["properties" "then"] self-ref)
                           (assoc-in ["properties" "else"] self-ref))

                       (= "cond" op)
                       (-> alt
                           (assoc-in ["properties" "clauses" "items" "properties" "expr"] self-ref)
                           (assoc-in ["properties" "else"] self-ref))

                       (= "let" op)
                       (assoc-in alt ["properties" "body"] self-ref)

                       :else alt)))
                 (get base-expr-schema "oneOf")))))

(defn- add-value-expr-defs [schema allowed-keys output-values]
  (let [base-expr-schema (get-in schema ["$defs" "expr"])]
    (reduce (fn [acc key-name]
              (let [allowed-strings (string-output-values output-values key-name)]
                (if (seq allowed-strings)
                  (assoc-in acc
                            ["$defs" (value-expr-def-name key-name)]
                            (value-expr-schema base-expr-schema key-name allowed-strings))
                  acc)))
            schema
            allowed-keys)))

(defn schema-for-request [grammar semantic-contract]
  (let [schema (json/parse-string (slurp (:file grammar)))
        required-keys (:required_output_keys semantic-contract)
        output-values (:output_key_values semantic-contract)]
    (if (seq required-keys)
      (-> schema
          (restrict-map-entry-keys required-keys output-values)
          (add-value-expr-defs required-keys output-values))
      schema)))

(defn column-entry-schema
  ([output-key]
   (column-entry-schema "schemas/clojure_ast.schema.json" output-key))
  ([schema-path output-key]
   (let [schema (json/parse-string (slurp schema-path))]
     {"$schema" "https://json-schema.org/draft/2020-12/schema"
      "$defs" (get schema "$defs")
      "type" "object"
      "additionalProperties" false
      "required" ["translation_note" "key" "expr"]
      "properties" {"translation_note" {"type" "string"
                                         "minLength" 20}
                    "key" {"const" (name output-key)}
                    "expr" {"$ref" "#/$defs/expr"}}})))

(defn where-predicate-array-schema
  ([]
   (where-predicate-array-schema "schemas/clojure_ast.schema.json"))
  ([schema-path]
   (let [schema (json/parse-string (slurp schema-path))]
     {"$schema" "https://json-schema.org/draft/2020-12/schema"
      "$defs" (get schema "$defs")
      "type" "array"
      "items" {"$ref" "#/$defs/predicateExpr"}})))

(defn deconstructed-column-schema []
  {"$schema" "https://json-schema.org/draft/2020-12/schema"
   "type" "array"
   "items" {"type" "object"
            "additionalProperties" false
            "required" ["condition_text" "output_value"]
            "properties" {"condition_text" {"type" "string" "minLength" 1}
                          "output_value" {"type" ["string" "number" "boolean" "null"]}}}})

(defn atomic-predicate-schema
  ([]
   (atomic-predicate-schema "schemas/clojure_ast.schema.json"))
  ([schema-path]
   (let [schema (json/parse-string (slurp schema-path))]
     {"$schema" "https://json-schema.org/draft/2020-12/schema"
      "$defs" (get schema "$defs")
      "$ref" "#/$defs/predicateExpr"})))

(defn column-entry-request-constraint [schema-path output-key]
  (fn [grammar _semantic-contract]
    (case (:mode grammar)
      "gbnf" {:grammar (slurp (:file grammar))}
      "json-schema-gbnf" {:response_format {:type "json_schema"
                                             :json_schema {:schema (column-entry-schema schema-path output-key)}}}
      {:response_format {:type "json_object"}})))

(defn where-predicate-request-constraint [schema-path]
  (fn [grammar _semantic-contract]
    (case (:mode grammar)
      "gbnf" {:grammar (slurp (:file grammar))}
      "json-schema-gbnf" {:response_format {:type "json_schema"
                                             :json_schema {:schema (where-predicate-array-schema
                                                                    (or schema-path "schemas/clojure_ast.schema.json"))}}}
      {:response_format {:type "json_object"}})))

(defn deconstructed-column-request-constraint []
  (fn [grammar _semantic-contract]
    (case (:mode grammar)
      "gbnf" {:grammar (slurp (:file grammar))}
      "json-schema-gbnf" {:response_format {:type "json_schema"
                                             :json_schema {:schema (deconstructed-column-schema)}}}
      {:response_format {:type "json_object"}})))

(defn atomic-predicate-request-constraint [schema-path]
  (fn [grammar _semantic-contract]
    (case (:mode grammar)
      "gbnf" {:grammar (slurp (:file grammar))}
      "json-schema-gbnf" {:response_format {:type "json_schema"
                                             :json_schema {:schema (atomic-predicate-schema
                                                                    (or schema-path "schemas/clojure_ast.schema.json"))}}}
      {:response_format {:type "json_object"}})))

(defn request-constraint [grammar semantic-contract]
  (case (:mode grammar)
    "gbnf" {:grammar (slurp (:file grammar))}
    "json-schema-gbnf" {:response_format {:type "json_schema"
                                           :json_schema {:schema (schema-for-request grammar semantic-contract)}}}
    {:response_format {:type "json_object"}}))
