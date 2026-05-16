(ns llmisp.diagnostics
  (:require [clojure.string :as str]))

(defn diagnostic-message [{:keys [phase code message source line]}]
  (str
   (when phase
     (str "[" (name phase) "/" (name code) "] "))
   (when source
     (str source " "))
   (when line
     (str "Line " line ": "))
   message))

(defn raise!
  ([diagnostic]
   (throw (ex-info (diagnostic-message diagnostic) diagnostic)))
  ([phase code message]
   (raise! {:phase phase
            :code code
            :message message}))
  ([phase code message data]
   (raise! (merge {:phase phase
                   :code code
                   :message message}
                  data))))

(defn- format-value [value]
  (cond
    (string? value) value
    :else (pr-str value)))

(defn format-diagnostic [{:keys [phase code source line message] :as diagnostic}]
  (let [lines [(str "ERROR " (name phase) "/" (name code))
               (str "message: " message)]
        lines (cond-> lines
                source (conj (str "source: " source))
                line (conj (str "line: " line)))
        detail-keys (remove #{:phase :code :source :line :message} (keys diagnostic))
        lines (into lines
                    (for [k (sort detail-keys)]
                      (str (name k) ": " (format-value (get diagnostic k)))))]
    (str/join "\n" lines)))

(defn ex->diagnostic [ex]
  (let [data (ex-data ex)]
    (cond
      (and (map? data) (:phase data) (:code data))
      data

      (map? data)
      (assoc data
             :phase (or (:phase data) :runtime)
             :code (or (:code data) :exception)
             :message (or (:message data) (.getMessage ex)))

      :else
      {:phase :runtime
       :code :exception
       :message (.getMessage ex)})))

(defn render-exception [ex]
  (format-diagnostic (ex->diagnostic ex)))

(defn render-exception-explained [ex explain-context]
  (let [diagnostic (ex->diagnostic ex)]
    (str (format-diagnostic diagnostic)
         "\n\nEXPLAIN\n"
         (pr-str explain-context))))
