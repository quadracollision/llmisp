(ns harness.sqlite
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [cheshire.core :as json]))

(defn sqlite! [op payload]
  (let [script (str (fs/path "bb" "sqlite_memory.py"))
        proc (p/process ["python3" script op]
                        {:in (json/generate-string payload)
                         :out :string
                         :err :string})
        result @proc]
    (when-not (zero? (:exit result))
      (throw (ex-info (str "sqlite memory op failed: " op)
                      {:op op
                       :exit (:exit result)
                       :stderr (:err result)})))
    (json/parse-string (:out result) true)))

(defn init-session! [db-path model task]
  (:session_id (sqlite! "init-session" {:db (str db-path) :model model :task task})))

(defn save-artifact! [db-path session-id kind content]
  (sqlite! "save-artifact"
           {:db (str db-path)
            :session_id session-id
            :kind kind
            :content content})
  nil)

(defn save-attempt! [db-path session-id phase attempt-index task prompt raw-output parsed source-text check]
  (sqlite! "save-attempt"
           {:db (str db-path)
            :session_id session-id
            :phase phase
            :attempt_index attempt-index
            :spec_text task
            :prompt prompt
            :raw_output raw-output
            :parsed parsed
            :source_text source-text
            :check check})
  nil)

(defn save-trace! [db-path session-id attempt-index case-name tag data]
  (sqlite! "save-trace"
           {:db (str db-path)
            :session_id session-id
            :attempt_index attempt-index
            :case_name case-name
            :tag tag
            :data data})
  nil)
