(ns harness.plugins.sqlite
  (:require [clojure.string :as str]
            [harness.action-plugins :as action-plugins]))

(def action-schema
  [:multi {:dispatch :kind}
   ["sqlite-insert" [:map {:closed true}
                     [:plugin [:= "sqlite"]]
                     [:kind [:= "sqlite-insert"]]
                     [:db [:fn {:error/message "expected local sqlite db path"} string?]]
                     [:table [:fn {:error/message "expected safe table name"} string?]]
                     [:values [:map-of [:or string? keyword?]
                               [:multi {:dispatch :source}
                                ["control" [:map {:closed true}
                                            [:source [:= "control"]]
                                            [:id string?]]]
                                ["literal" [:map {:closed true}
                                            [:source [:= "literal"]]
                                            [:value [:or string? int? double? boolean? nil?]]]]
                                ["now" [:map {:closed true}
                                        [:source [:= "now"]]]]]]]
                     [:message {:optional true} string?]]]
   ["sqlite-refresh-table" [:map {:closed true}
                            [:plugin [:= "sqlite"]]
                            [:kind [:= "sqlite-refresh-table"]]
                            [:db [:fn {:error/message "expected local sqlite db path"} string?]]
                            [:table [:fn {:error/message "expected safe table name"} string?]]
                            [:target string?]
                            [:columns [:vector {:min 1} string?]]
                            [:message {:optional true} string?]]]])

(defn safe-db-path? [db]
  (and (string? db)
       (not (str/blank? db))
       (str/ends-with? db ".sqlite3")
       (not (str/includes? db "/"))
       (not (str/includes? db "\\"))))

(defn safe-ident? [value]
  (let [s (cond
            (keyword? value) (name value)
            (string? value) value
            :else nil)]
    (and s
         (boolean (re-matches #"[a-z][a-z0-9-]*" s)))))

(defn sqlite-action? [action]
  (= "sqlite" (:plugin action)))

(defn action-errors [action]
  (let [db (:db action)
        table (:table action)
        columns (case (:kind action)
                  "sqlite-insert" (map name (keys (:values action)))
                  "sqlite-refresh-table" (:columns action)
                  [])]
    (vec
     (concat
      (when-not (safe-db-path? db)
        [{:message "SQLite db path must be a local .sqlite3 filename"
          :value db}])
      (when-not (safe-ident? table)
        [{:message "SQLite table name must be kebab-case"
          :value table}])
      (for [column columns
            :when (not (safe-ident? column))]
        {:message "SQLite column name must be kebab-case"
         :value column})))))

(defn- persistence-columns [contract]
  (or (seq (get-in contract [:persistence :columns]))
      (seq (:data_fields contract))
      []))

(defn- persistence-values [contract status]
  (->> (persistence-columns contract)
       (map (fn [column]
              [column
               (cond
                 (= "submitted-at" column) {:source "now"}
                 (= "status" column) {:source "literal" :value status}
                 :else {:source "control" :id column})]))
       (into {})))

(defn- insert-action [contract action-id status]
  {:plugin "sqlite"
   :kind "sqlite-insert"
   :db (get-in contract [:persistence :db])
   :table (get-in contract [:persistence :table])
   :values (persistence-values contract status)
   :message (str (-> action-id
                     (str/replace #"-" " ")
                     str/capitalize)
                 " saved.")})

(defn- refresh-action [contract]
  {:plugin "sqlite"
   :kind "sqlite-refresh-table"
   :db (get-in contract [:persistence :db])
   :table (get-in contract [:persistence :table])
   :target (get-in contract [:persistence :target])
   :columns (vec (persistence-columns contract))
   :message "Submissions refreshed."})

(defmethod action-plugins/action-schema "sqlite" [_action]
  action-schema)

(defmethod action-plugins/action-errors "sqlite" [_ctx action]
  (action-errors action))

(defmethod action-plugins/synthesize-action "sqlite" [contract action-id intent]
  (case (:kind intent)
    :insert-draft (insert-action contract action-id "draft")
    :insert-submitted (insert-action contract action-id "submitted")
    :refresh-table (refresh-action contract)
    nil))

(defmethod action-plugins/reference-errors "sqlite" [{:keys [ids table-ids]} action]
  (case (:kind action)
    "sqlite-insert"
    (for [[_ spec] (:values action)
          :when (and (= "control" (:source spec))
                     (not (contains? ids (:id spec))))]
      {:message "SQLite insert references unknown control id"
       :value (:id spec)})

    "sqlite-refresh-table"
    (when-not (contains? table-ids (:target action))
      [{:message "SQLite refresh references unknown table target"
        :value (:target action)}])

    []))

(defmethod action-plugins/rewrite-action "sqlite" [{:keys [table-ids]} action]
  (if (and (= "sqlite-refresh-table" (:kind action))
           (= 1 (count table-ids))
           (not (contains? table-ids (:target action))))
    (assoc action :target (first table-ids))
    action))

(defmethod action-plugins/matches-intent? "sqlite" [_ctx action intent]
  (case (:kind intent)
    :insert-draft (= "sqlite-insert" (:kind action))
    :insert-submitted (= "sqlite-insert" (:kind action))
    :refresh-table (= "sqlite-refresh-table" (:kind action))
    false))

(defn value-form [registry-sym value-spec]
  (case (:source value-spec)
    "control" `(~'control-value (get @~registry-sym ~(:id value-spec)))
    "literal" (:value value-spec)
    "now" `(~'now-string)
    nil))

(defmethod action-plugins/action-form "sqlite" [{:keys [registry-sym]} action]
  (case (:kind action)
    "sqlite-insert"
    (let [values (:values action)]
      `(do
         (~'sqlite-insert! ~(:db action)
                           ~(:table action)
                           ~(mapv name (keys values))
                           ~(mapv (fn [[_ value-spec]]
                                    (value-form registry-sym value-spec))
                                  values))
         ~(when-let [message (:message action)]
            `(javax.swing.JOptionPane/showMessageDialog nil ~message))))

    "sqlite-refresh-table"
    `(do
       (~'sqlite-refresh-table! ~(:db action)
                                ~(:table action)
                                ~(:columns action)
                                (get @~registry-sym ~(:target action)))
       ~(when-let [message (:message action)]
          `(javax.swing.JOptionPane/showMessageDialog nil ~message)))

    nil))

(defmethod action-plugins/action-helper-forms "sqlite" [_plugin]
  [`(defn ~'sqlite-ident [value#]
      (let [s# (str value#)]
        (when-not (re-matches #"[a-z][a-z0-9-]*" s#)
          (throw (IllegalArgumentException. (str "unsafe sqlite identifier: " s#))))
        (str "\"" s# "\"")))
   `(defn ~'now-string []
      (.format (java.time.format.DateTimeFormatter/ISO_INSTANT)
               (java.time.Instant/now)))
   `(defn ~'sqlite-connect [db#]
      (Class/forName "org.sqlite.JDBC")
      (java.sql.DriverManager/getConnection (str "jdbc:sqlite:" db#)))
   `(defn ~'sqlite-create-table! [conn# table# columns#]
      (let [sql# (str "CREATE TABLE IF NOT EXISTS "
                      (~'sqlite-ident table#)
                      " ("
                      (clojure.string/join ", "
                                           (map (fn [column#]
                                                  (str (~'sqlite-ident column#) " TEXT"))
                                                columns#))
                      ")")]
        (with-open [stmt# (.createStatement conn#)]
          (.execute stmt# sql#))))
   `(defn ~'sqlite-insert! [db# table# columns# values#]
      (with-open [conn# (~'sqlite-connect db#)]
        (~'sqlite-create-table! conn# table# columns#)
        (let [sql# (str "INSERT INTO "
                        (~'sqlite-ident table#)
                        " ("
                        (clojure.string/join ", " (map ~'sqlite-ident columns#))
                        ") VALUES ("
                        (clojure.string/join ", " (repeat (count columns#) "?"))
                        ")")]
          (with-open [stmt# (.prepareStatement conn# sql#)]
            (doseq [[idx# value#] (map-indexed vector values#)]
              (.setString stmt# (inc idx#) (some-> value# str)))
            (.executeUpdate stmt#)))))
   `(defn ~'sqlite-refresh-table! [db# table# columns# target#]
      (with-open [conn# (~'sqlite-connect db#)]
        (~'sqlite-create-table! conn# table# columns#)
        (let [sql# (str "SELECT "
                        (clojure.string/join ", " (map ~'sqlite-ident columns#))
                        " FROM "
                        (~'sqlite-ident table#)
                        " ORDER BY rowid DESC")
              rows# (with-open [stmt# (.createStatement conn#)
                                rs# (.executeQuery stmt# sql#)]
                      (loop [acc# []]
                        (if (.next rs#)
                          (recur (conj acc#
                                       (mapv (fn [column#]
                                               (.getString rs# column#))
                                             columns#)))
                          acc#)))
              table-component# (~'table-component target#)]
          (when table-component#
            (.setModel table-component#
                       (javax.swing.table.DefaultTableModel.
                        (to-array-2d rows#)
                        (into-array String columns#)))))))])
