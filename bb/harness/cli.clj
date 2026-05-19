(ns harness.cli
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [harness.action-plugins :as action-plugins]
            [harness.common :as common]
            [harness.gui-smoke :as gui-smoke]
            [harness.model :as model-io]
            [harness.plugin-loader :as plugin-loader]
            [harness.protocols :as protocols]
            [harness.prompt :as prompt]
            [harness.semantic :as semantic]
            [harness.sqlite :as sqlite]
            [harness.swing-lowerer :as swing-lowerer]
            [harness.swing-schema :as swing-schema]
            [harness.validation :as validation]
            [llmisp.clj-ast :as clj-ast]))

(plugin-loader/load-configured-plugins!)

(def usage
  (str/join
   "\n"
   ["Usage:"
    "  bb json-check <candidate.ast.json>"
    "  bb json-compile <candidate.ast.json> <candidate.clj> [--force]"
    "  bb semantic-check <candidate.clj> <semantic-test.edn>"
    "  bb json-smoke [project-dir]"
    "  bb gui-smoke [project-dir]"
    "  bb gui-run --contract-file <contract.edn|contract.json> --project-dir <dir>"
    "  bb gui-run --self-plan --task-file <spec.txt> --project-dir <dir> [--endpoint <url> --model <name> | --gguf <model.gguf> --llama-server-bin <bin>]"
    "  bb gui-open <candidate.clj>"
    "  bb gui-jar <candidate.clj> <app.jar>"
    "  bb json-dry-run <project-dir>"
    "  bb json-run --task-file <spec.txt> --project-dir <dir> [--self-plan] [--skeleton-first --where-pass --column-pass] [--semantic-test-file <test.edn> --semantic-oracle-source user|benchmark|assistant|unknown] [--auto-semantic-test] [--allow-semantic-guidance] [--endpoint <url> --model <name> | --gguf <model.gguf> --llama-server-bin <bin>] [--grammar-file <ast.gbnf> | --json-schema-file <ast.schema.json>]"
    "  bb chat-ping <endpoint> <model>"]))

(defn- fail! [message]
  (binding [*out* *err*]
    (println message))
  (System/exit 1))

(defn- ensure-args! [args n]
  (when-not (= n (count args))
    (fail! usage)))

(def json-safe common/json-safe)

(defn- parse-args [args]
  (loop [remaining args
         opts {}]
    (if-let [arg (first remaining)]
      (case arg
        "--task-file" (recur (nnext remaining) (assoc opts :task-file (second remaining)))
        "--task-text" (recur (nnext remaining) (assoc opts :task-text (second remaining)))
        "--contract-file" (recur (nnext remaining) (assoc opts :contract-file (second remaining)))
        "--project-dir" (recur (nnext remaining) (assoc opts :project-dir (second remaining)))
        "--endpoint" (recur (nnext remaining) (assoc opts :endpoint (second remaining)))
        "--model" (recur (nnext remaining) (assoc opts :model (second remaining)))
        "--gguf" (recur (nnext remaining) (assoc opts :gguf (second remaining)))
        "--llama-server-bin" (recur (nnext remaining) (assoc opts :llama-server-bin (second remaining)))
        "--llama-port" (recur (nnext remaining) (assoc opts :llama-port (Long/parseLong (second remaining))))
        "--llama-host" (recur (nnext remaining) (assoc opts :llama-host (second remaining)))
        "--llama-ctx-size" (recur (nnext remaining) (assoc opts :llama-ctx-size (Long/parseLong (second remaining))))
        "--llama-gpu-layers" (recur (nnext remaining) (assoc opts :llama-gpu-layers (Long/parseLong (second remaining))))
        "--grammar-file" (recur (nnext remaining) (assoc opts :grammar-file (second remaining)))
        "--json-schema-file" (recur (nnext remaining) (assoc opts :json-schema-file (second remaining)))
        "--semantic-test-file" (recur (nnext remaining) (assoc opts :semantic-test-file (second remaining)))
        "--semantic-oracle-source" (recur (nnext remaining) (assoc opts :semantic-oracle-source (second remaining)))
        "--auto-semantic-test" (recur (next remaining) (assoc opts :auto-semantic-test true))
        "--allow-semantic-guidance" (recur (next remaining) (assoc opts :allow-semantic-guidance true))
        "--self-plan" (recur (next remaining) (assoc opts :self-plan true))
        "--component-pass" (recur (next remaining) (assoc opts :component-pass true))
        "--skeleton-first" (recur (next remaining) (assoc opts :skeleton-first true))
        "--where-pass" (recur (next remaining) (assoc opts :where-pass true))
        "--column-pass" (recur (next remaining) (assoc opts :column-pass true))
        "--planner-max-tokens" (recur (nnext remaining) (assoc opts :planner-max-tokens (Long/parseLong (second remaining))))
        "--where-max-tokens" (recur (nnext remaining) (assoc opts :where-max-tokens (Long/parseLong (second remaining))))
        "--column-max-tokens" (recur (nnext remaining) (assoc opts :column-max-tokens (Long/parseLong (second remaining))))
        "--repair-attempts" (recur (nnext remaining) (assoc opts :repair-attempts (Long/parseLong (second remaining))))
        "--max-tokens" (recur (nnext remaining) (assoc opts :max-tokens (Long/parseLong (second remaining))))
        "--temperature" (recur (nnext remaining) (assoc opts :temperature (Double/parseDouble (second remaining))))
        (fail! (str "unknown option: " arg "\n" usage)))
      opts)))

(defn- try-json-extract [content]
  (let [visible (str/replace content #"(?is)<think(?:ing)?>.*?</think(?:ing)?>" "")
        fenced (second (re-find #"(?is)```json\s*(.*?)```" visible))
        candidate (or fenced visible)]
    (loop [idx 0]
      (let [start (str/index-of candidate "{" idx)]
        (when start
          (if-let [parsed (try
                            (json/parse-string (subs candidate start) true)
                            (catch Exception _ nil))]
            parsed
            (recur (inc start))))))))

(defn- try-json-array-extract [content]
  (let [visible (str/replace content #"(?is)<think(?:ing)?>.*?</think(?:ing)?>" "")
        fenced (second (re-find #"(?is)```json\s*(.*?)```" visible))
        candidate (or fenced visible)]
    (loop [idx 0]
      (let [start (str/index-of candidate "[" idx)]
        (when start
          (if-let [parsed (try
                            (let [value (json/parse-string (subs candidate start) true)]
                              (when (sequential? value) (vec value)))
                            (catch Exception _ nil))]
            parsed
            (recur (inc start))))))))

(defn- json-extract [content]
  (or (try-json-extract content)
      (fail! (str "model did not return a JSON object:\n" content))))

(defn- task-text [opts]
  (cond
    (:task-text opts) (:task-text opts)
    (:task-file opts) (slurp (:task-file opts))
    :else (fail! "--task-file or --task-text is required")))

(defn- project-paths [opts]
  (let [project-dir (fs/path (or (:project-dir opts) "tmp/bb_json_ast_run"))
        ast-path (fs/path project-dir "candidate.ast.json")
        clj-path (fs/path project-dir "candidate.clj")
        report-path (fs/path project-dir "report.json")
        db-path (fs/path project-dir "session.sqlite3")
        log-path (fs/path project-dir "llama-server.log")]
    (fs/create-dirs project-dir)
    {:project-dir project-dir
     :ast-path ast-path
     :clj-path clj-path
     :report-path report-path
     :db-path db-path
     :log-path log-path}))

(defn- gui-project-paths [opts]
  (let [project-dir (fs/path (or (:project-dir opts) "tmp/bb_gui_run"))
        gui-path (fs/path project-dir "candidate.gui.json")
        clj-path (fs/path project-dir "candidate.clj")
        report-path (fs/path project-dir "report.json")
        db-path (fs/path project-dir "session.sqlite3")
        log-path (fs/path project-dir "llama-server.log")]
    (fs/create-dirs project-dir)
    {:project-dir project-dir
     :gui-path gui-path
     :clj-path clj-path
     :report-path report-path
     :db-path db-path
     :log-path log-path}))

(defn- read-contract-file [path]
  (let [text (slurp path)]
    (if (str/ends-with? (str/lower-case path) ".json")
      (json/parse-string text true)
      (edn/read-string text))))

(defn- load-check-clojure-source [clj-path]
  (let [load-check (p/shell {:out :string
                             :err :string
                             :continue true}
                            "clojure" "-M" "-e"
                            (str "(load-file " (pr-str (str clj-path)) ") (println :loaded)"))]
    (if (zero? (:exit load-check))
      {:exit_code 0
       :stdout (:out load-check)
       :stderr (:err load-check)
       :validator "clojure-source-load"}
      {:exit_code (:exit load-check)
       :stdout (:out load-check)
       :stderr (:err load-check)
       :validator "clojure-source-load"})))

(defn- gui-self-plan! [{:keys [task endpoint model temperature max-tokens db-path session-id]}]
  (let [messages [{:role "user" :content (prompt/gui-planner-prompt task)}]
        content (model-io/chat-completion {:endpoint endpoint
                                           :model model
                                           :messages messages
                                           :temperature temperature
                                           :max-tokens max-tokens})
        parsed (json-extract content)
        contract (:sanitized_contract parsed)
        artifact {:raw_output content
                  :parsed parsed
                  :sanitized_contract contract}]
    (sqlite/save-artifact! db-path session-id "gui_self_plan"
                           (json/generate-string (json-safe artifact) {:pretty true}))
    artifact))

(defn- validate-gui-node [node]
  (let [wrapped {:op "window"
                 :title "Validation Window"
                 :width 100
                 :height 100
                 :children [node]}]
    (swing-schema/validate wrapped)))

(defn- component-root-id [node]
  (:id node))

(defn- gui-node-ids [node]
  (let [own-id (when-let [id (:id node)] [id])
        child-ids (mapcat gui-node-ids (:children node))
        tab-ids (mapcat (fn [tab]
                          (concat [(:id tab)]
                                  (mapcat gui-node-ids (:children tab))))
                        (:tabs node))]
    (concat own-id child-ids tab-ids)))

(defn- validate-component-boundary [contract component-id node]
  (let [reserved-other-ids (disj (set (:required_components contract)) component-id)
        nested-reserved (->> (gui-node-ids node)
                             rest
                             (filter reserved-other-ids)
                             distinct
                             vec)]
    (if (seq nested-reserved)
      {:exit_code 1
       :stdout ""
       :stderr (str "[gui/component-boundary] component " component-id
                    " must not define other required component ids: "
                    (pr-str nested-reserved))
       :validator "gui-component-boundary"
       :ok false
       :nested_required_components nested-reserved}
      {:exit_code 0
       :stdout "GUI component boundary OK\n"
       :stderr ""
       :validator "gui-component-boundary"
       :ok true})))

(defn- replace-gui-component [node component-id replacement]
  (cond
    (and (map? node) (= component-id (:id node)))
    replacement

    (map? node)
    (cond-> node
      (:children node)
      (update :children #(mapv (fn [child]
                                  (replace-gui-component child component-id replacement))
                                %))
      (:tabs node)
      (update :tabs #(mapv (fn [tab]
                              (update tab :children
                                      (fn [children]
                                        (mapv (fn [child]
                                                (replace-gui-component child component-id replacement))
                                              children))))
                            %)))

    :else node))

(defn- gui-component-attempt! [{:keys [task contract component-id endpoint model temperature max-tokens previous-output error-text]}]
  (let [messages [{:role "user"
                   :content (prompt/gui-component-prompt task contract component-id previous-output error-text)}]
        content (model-io/chat-completion {:endpoint endpoint
                                           :model model
                                           :messages messages
                                           :temperature temperature
                                           :max-tokens max-tokens})
        raw-parsed (json-extract content)
        parsed (if (and (map? raw-parsed)
                        (:id raw-parsed)
                        (not= component-id (:id raw-parsed)))
                 (assoc raw-parsed :id component-id)
                 raw-parsed)
        id-check (if (= component-id (component-root-id parsed))
                   {:exit_code 0 :stdout "GUI component id OK\n" :stderr "" :validator "gui-component-id" :ok true}
                   {:exit_code 1
                    :stdout ""
                    :stderr (str "[gui/component-id] expected root id " component-id
                                 ", got " (pr-str (component-root-id parsed)))
                    :validator "gui-component-id"
                    :ok false})
        shape-check (when (zero? (:exit_code id-check))
                      (validate-gui-node parsed))
        boundary-check (when (and shape-check (:ok shape-check))
                         (validate-component-boundary contract component-id parsed))
        check (cond
                (not (zero? (:exit_code id-check))) id-check
                (not (:ok shape-check)) shape-check
                (not (:ok boundary-check)) boundary-check
                :else boundary-check)]
    {:prompt messages
     :raw_output content
     :raw_parsed raw-parsed
     :parsed parsed
     :check check}))

(defn- gui-component! [{:keys [ast component-id db-path session-id] :as opts}]
  (let [max-repairs (or (:repair-attempts opts) 1)]
    (loop [attempt 0
         previous-output nil
         error-text nil]
      (let [result (gui-component-attempt! (assoc opts
                                                  :previous-output previous-output
                                                  :error-text error-text))
            artifact (assoc result
                            :component_id component-id
                            :attempt attempt)]
        (sqlite/save-artifact! db-path session-id (str "gui_component_" component-id "_" attempt)
                               (json/generate-string (json-safe artifact) {:pretty true}))
        (if (:ok (:check result))
          (replace-gui-component ast component-id (:parsed result))
          (if (< attempt max-repairs)
            (recur (inc attempt)
                   (:raw_output result)
                   (:stderr (:check result)))
            (throw (ex-info "GUI component generation failed"
                            {:check (:check result)
                             :component_id component-id}))))))))

(defn- refine-gui-components! [{:keys [ast contract] :as opts}]
  (reduce (fn [acc component-id]
            (if (= "actions-panel" component-id)
              acc
              (gui-component! (assoc opts
                                     :ast acc
                                     :component-id component-id))))
          ast
          (:required_components contract)))

(defn- gui-control-node? [node]
  (contains? #{"text-field" "text-area" "combo-box" "check-box"} (:op node)))

(defn- gui-table-node? [node]
  (= "table" (:op node)))

(defn- gui-collect [pred node]
  (concat (when (pred node) [node])
          (mapcat #(gui-collect pred %) (:children node))
          (mapcat (fn [tab]
                    (mapcat #(gui-collect pred %) (:children tab)))
                  (:tabs node))))

(defn- persistence-meta-field? [field-name]
  (contains? #{"submitted-at" "status"} field-name))

(defn- missing-gui-data-fields [contract ast]
  (let [ids (set (gui-node-ids ast))]
    (->> (:data_fields contract)
         (remove persistence-meta-field?)
         (remove ids)
         vec)))

(defn- data-field-row [field-name]
  {:op "panel"
   :id (str field-name "-row")
   :layout "row"
   :children [{:op "label"
               :id (str field-name "-label")
               :text (-> field-name
                         (str/replace #"-" " ")
                         str/capitalize)}
              {:op "text-field"
               :id field-name
               :label (-> field-name
                          (str/replace #"-" " ")
                          str/capitalize)}]})

(defn- form-panel? [node]
  (and (= "panel" (:op node))
       (or (str/includes? (str (:id node)) "form")
           (some gui-control-node? (:children node)))))

(defn- add-fields-to-first-form [node fields]
  (let [done? (atom false)]
    (letfn [(walk [current]
              (cond
                (and (not @done?) (form-panel? current))
                (do
                  (reset! done? true)
                  (update current :children into (mapv data-field-row fields)))

                (map? current)
                (cond-> current
                  (:children current) (update :children #(mapv walk %))
                  (:tabs current) (update :tabs
                                          #(mapv (fn [tab]
                                                   (update tab :children
                                                           (fn [children]
                                                             (mapv walk children))))
                                                 %)))

                :else current))]
      (let [updated (walk node)]
        (if @done?
          updated
          (update updated :children conj {:op "panel"
                                          :id "generated-fields-form"
                                          :title "Additional Fields"
                                          :layout "flow"
                                          :children (mapv data-field-row fields)}))))))

(defn- gui-table-ids [ast]
  (->> (gui-collect gui-table-node? ast)
       (keep :id)
       distinct
       vec))

(defn- rewrite-plugin-actions [ast]
  (let [table-ids (->> (gui-collect gui-table-node? ast)
                       (keep :id)
                       distinct
                       vec)
        ctx {:table-ids (set table-ids)}]
    (letfn [(walk [node]
              (if (map? node)
                (cond-> (cond-> node
                          (get-in node [:action :plugin])
                          (update :action #(action-plugins/rewrite-action ctx %)))
                  (:children node) (update :children #(mapv walk %))
                  (:tabs node) (update :tabs
                                       #(mapv (fn [tab]
                                                (update tab :children
                                                        (fn [children]
                                                          (mapv walk children))))
                                              %)))
                node))]
      (walk ast))))

(defn- prune-non-action-panel-action-buttons [contract ast]
  (let [action-ids (set (:actions contract))]
    (if-not (and (seq action-ids)
                 (some #{"actions-panel"} (:required_components contract)))
      ast
      (letfn [(walk [inside-actions-panel? node]
                (if-not (map? node)
                  node
                  (let [inside-actions-panel? (or inside-actions-panel?
                                                  (= "actions-panel" (:id node)))]
                    (if (and (not inside-actions-panel?)
                             (= "button" (:op node))
                             (contains? action-ids (:id node)))
                      nil
                      (cond-> node
                        (:children node)
                        (update :children (fn [children]
                                            (->> children
                                                 (keep #(walk inside-actions-panel? %))
                                                 vec)))
                        (:tabs node)
                        (update :tabs
                                (fn [tabs]
                                  (mapv (fn [tab]
                                          (update tab :children
                                                  (fn [children]
                                                    (->> children
                                                         (keep #(walk inside-actions-panel? %))
                                                         vec))))
                                        tabs))))))))]
        (walk false ast)))))

(defn- finalize-gui-ast [contract ast]
  (let [missing-fields (missing-gui-data-fields contract ast)]
    (cond->> ast
      true (prune-non-action-panel-action-buttons contract)
      (seq missing-fields) (#(add-fields-to-first-form % missing-fields))
      true rewrite-plugin-actions)))

(defn- default-semantic-test-path [task-file]
  (when task-file
    (let [path (fs/path task-file)
          file-name (str (fs/file-name path))
          semantic-name (cond
                          (str/ends-with? file-name "_spec.txt")
                          (str (subs file-name 0 (- (count file-name) (count "_spec.txt"))) "_semantic.edn")

                          (str/ends-with? file-name ".txt")
                          (str (subs file-name 0 (- (count file-name) 4)) "_semantic.edn")

                          :else
                          (str file-name ".semantic.edn"))]
      (str (fs/path (fs/parent path) semantic-name)))))

(defn- semantic-test-path [opts]
  (let [path (or (:semantic-test-file opts)
                 (when (:auto-semantic-test opts)
                   (default-semantic-test-path (:task-file opts))))]
    (when (and path (fs/exists? path))
      (str path))))

(def ^:private oracle-sources #{"none" "user" "benchmark" "assistant" "unknown" "auto"})

(defn- semantic-oracle [opts semantic-path]
  (let [source (cond
                 (not semantic-path) "none"
                 (:semantic-oracle-source opts) (:semantic-oracle-source opts)
                 (:auto-semantic-test opts) "auto"
                 :else "unknown")]
    (when-not (contains? oracle-sources source)
      (fail! (str "--semantic-oracle-source must be one of " (str/join ", " (sort oracle-sources)))))
    {:path semantic-path
     :source source
     :clean_benchmark? (contains? #{"user" "benchmark" "none"} source)
     :note (case source
             "none" "No semantic oracle supplied; semantic correctness is unverified."
             "user" "User-supplied oracle; clean if the assistant did not author it."
             "benchmark" "Benchmark-supplied oracle; clean if fixed before this run."
             "assistant" "Assistant-authored oracle; not clean benchmark evidence."
             "auto" "Auto-discovered oracle; legacy/dev convenience, not clean benchmark evidence."
             "unknown" "Oracle source is unknown; not clean benchmark evidence.")}))

(defn check-json [args]
  (ensure-args! args 1)
  (let [path (first args)
        ast (json/parse-string (slurp path) true)
        shape-check (validation/validate-ast-shape ast)]
    (when-not (:ok shape-check)
      (fail! (:stderr shape-check)))
    (clj-ast/forms ast)
    (println "OK")))

(defn compile-json [args]
  (let [force? (boolean (some #{"--force"} args))
        positional (vec (remove #{"--force"} args))]
    (ensure-args! positional 2)
    (let [[json-path output-path] positional]
      (let [shape-check (validation/validate-ast-shape (json/parse-string (slurp json-path) true))]
        (when-not (:ok shape-check)
          (fail! (:stderr shape-check))))
      (println (clj-ast/compile-json-file json-path output-path {:overwrite? force?})))))

(defn semantic-check [args]
  (ensure-args! args 2)
  (let [[clj-path test-path] args
        check (semantic/run-semantic-tests clj-path test-path)]
    (if (zero? (:exit_code check))
      (println "OK")
      (fail! (str (:stderr check) "\n" (pr-str (:semantic check)))))))

(def minimal-ast
  "{\"module\":\"demo.bb-json\",\"defs\":[{\"name\":\"add-one\",\"args\":[\"x\"],\"expr\":{\"op\":\"call\",\"fn\":\"inc\",\"args\":[{\"op\":\"var\",\"name\":\"x\"}]}}]}")

(defn smoke [args]
  (let [project-dir (fs/path (or (first args) "tmp/bb_json_ast_smoke"))
        ast-path (fs/path project-dir "candidate.ast.json")
        clj-path (fs/path project-dir "candidate.clj")
        db-path (fs/path project-dir "session.sqlite3")]
    (fs/create-dirs project-dir)
    (spit (str ast-path) minimal-ast)
    (let [session-id (sqlite/init-session! db-path "smoke" "minimal JSON AST smoke")]
      (sqlite/save-artifact! db-path session-id "json_ast" minimal-ast))
    (let [shape-check (validation/validate-ast-shape (json/parse-string minimal-ast true))]
      (when-not (:ok shape-check)
        (fail! (:stderr shape-check))))
    (clj-ast/compile-json-file (str ast-path) (str clj-path) {:overwrite? true})
    (println "AST:" (str ast-path))
    (println "CLJ:" (str clj-path))
    (println "DB:" (str db-path))
    (println "OK")))

(defn gui-smoke [args]
  (let [project-dir (fs/path (or (first args) "tmp/bb_gui_smoke"))
        gui-path (fs/path project-dir "candidate.gui.json")
        clj-path (fs/path project-dir "candidate.clj")
        report-path (fs/path project-dir "report.json")
        db-path (fs/path project-dir "session.sqlite3")
        ast gui-smoke/sample-ast
        ast-text (json/generate-string (json-safe ast) {:pretty true})]
    (fs/create-dirs project-dir)
    (spit (str gui-path) ast-text)
    (let [session-id (sqlite/init-session! db-path "gui-smoke" "deterministic Swing GUI smoke")]
      (sqlite/save-artifact! db-path session-id "gui_ast" ast-text)
      (let [shape-check (swing-schema/validate ast)]
        (if (:ok shape-check)
          (do
            (swing-lowerer/write-source! (str clj-path) ast)
            (let [load-check (load-check-clojure-source clj-path)]
              (when-not (zero? (:exit_code load-check))
                (throw (ex-info "generated Swing Clojure source failed to load"
                                {:check load-check}))))
            (spit (str report-path)
                  (json/generate-string
                   {:project_dir (str project-dir)
                    :session_id session-id
                    :db (str db-path)
                    :gui_ast (str gui-path)
                    :clj (str clj-path)
                    :ok true
                    :smoke true
                    :check {:exit_code 0
                            :stdout "Swing GUI AST Malli OK\nSwing Clojure source load OK\n"
                            :stderr ""
                            :validator "swing-malli+source-load"}}
                   {:pretty true}))
            (println "GUI:" (str gui-path))
            (println "CLJ:" (str clj-path))
            (println "REPORT:" (str report-path))
            (println "DB:" (str db-path))
            (println "OK"))
          (do
            (spit (str report-path)
                  (json/generate-string
                   {:project_dir (str project-dir)
                    :session_id session-id
                    :db (str db-path)
                    :gui_ast (str gui-path)
                    :ok false
                    :smoke true
                    :check shape-check}
                   {:pretty true}))
            (fail! (:stderr shape-check))))))))

(defn gui-run [args]
  (let [opts (parse-args args)
        _ (when-not (or (:contract-file opts) (:self-plan opts))
            (fail! "--contract-file or --self-plan is required for gui-run"))
        _ (when (and (:contract-file opts) (:self-plan opts))
            (fail! "gui-run accepts either --contract-file or --self-plan, not both"))
        _ (when (and (:self-plan opts) (not (:task-file opts)))
            (fail! "gui-run --self-plan requires --task-file"))
        {:keys [project-dir gui-path clj-path report-path db-path log-path]} (gui-project-paths opts)
        host (or (:llama-host opts) "127.0.0.1")
        port (or (:llama-port opts) 18080)
        endpoint (or (:endpoint opts)
                     (when (:gguf opts) (model-io/local-endpoint host port)))
        model (model-io/model-name opts)
        max-tokens (or (:planner-max-tokens opts) (:max-tokens opts) 2048)
        session-id (sqlite/init-session! db-path "gui-run"
                                         (if (:self-plan opts)
                                           (str "GUI self-plan run: " (:task-file opts))
                                           (str "GUI contract run: " (:contract-file opts))))
        proc (when (and (:self-plan opts) (:gguf opts))
               (model-io/launch-llama! opts log-path))]
    (try
      (when (and (:self-plan opts) (not endpoint))
        (fail! "gui-run --self-plan requires --endpoint or --gguf"))
      (let [task (when (:self-plan opts) (task-text opts))
            plan-artifact (when (:self-plan opts)
                            (gui-self-plan! {:task task
                                             :endpoint endpoint
                                             :model model
                                             :temperature (:temperature opts)
                                             :max-tokens max-tokens
                                             :db-path db-path
                                             :session-id session-id}))
            raw-contract (if (:self-plan opts)
                           (:sanitized_contract plan-artifact)
                           (read-contract-file (:contract-file opts)))
            contract (protocols/normalize-contract raw-contract)
            contract-check (protocols/validate-contract contract)]
        (sqlite/save-artifact! db-path session-id "raw_contract"
                               (json/generate-string (json-safe raw-contract) {:pretty true}))
        (sqlite/save-artifact! db-path session-id "normalized_contract"
                               (json/generate-string (json-safe contract) {:pretty true}))
        (if-not (zero? (:exit_code contract-check))
          (do
            (spit (str report-path)
                  (json/generate-string
                   {:project_dir (str project-dir)
                    :session_id session-id
                    :db (str db-path)
                    :contract_file (:contract-file opts)
                    :task_file (:task-file opts)
                    :self_plan (boolean (:self-plan opts))
                    :ok false
                    :check contract-check}
                   {:pretty true}))
            (fail! (:stderr contract-check)))
          (let [skeleton-ast (protocols/assemble-skeleton-ast {:contract contract :opts opts})
                ast (if (:component-pass opts)
                      (try
                        (refine-gui-components! {:ast skeleton-ast
                                                 :contract contract
                                                 :task task
                                                 :endpoint endpoint
                                                 :model model
                                                 :temperature (:temperature opts)
                                                 :max-tokens (or (:column-max-tokens opts) (:max-tokens opts) 2048)
                                                 :repair-attempts (or (:repair-attempts opts) 1)
                                                 :db-path db-path
                                                 :session-id session-id})
                        (catch clojure.lang.ExceptionInfo ex
                          (let [check (or (:check (ex-data ex))
                                          {:exit_code 1
                                           :stdout ""
                                           :stderr (.getMessage ex)
                                           :validator "gui-component-pass"})]
                            (spit (str report-path)
                                  (json/generate-string
                                   {:project_dir (str project-dir)
                                    :session_id session-id
                                    :db (str db-path)
                                    :contract_file (:contract-file opts)
                                    :task_file (:task-file opts)
                                    :self_plan (boolean (:self-plan opts))
                                    :component_pass true
                                    :contract (json-safe contract)
                                    :ok false
                                    :check check}
                                   {:pretty true}))
                            (fail! (:stderr check)))))
                      skeleton-ast)
                ast (finalize-gui-ast contract ast)
                ast-text (json/generate-string (json-safe ast) {:pretty true})
                shape-check (swing-schema/validate ast)
                completeness-check (when (:ok shape-check)
                                     (protocols/completeness-check {:contract contract
                                                                    :ast ast
                                                                    :stage :post-skeleton}))
                check (cond
                        (not (:ok shape-check)) shape-check
                        (not (zero? (:exit_code completeness-check))) completeness-check
                        :else nil)]
            (spit (str gui-path) ast-text)
            (sqlite/save-artifact! db-path session-id "gui_ast" ast-text)
            (if check
              (do
                (spit (str report-path)
                      (json/generate-string
                       {:project_dir (str project-dir)
                        :session_id session-id
                        :db (str db-path)
                        :contract_file (:contract-file opts)
                        :task_file (:task-file opts)
                        :self_plan (boolean (:self-plan opts))
                        :contract (json-safe contract)
                        :gui_ast (str gui-path)
                        :ok false
                        :check check}
                       {:pretty true}))
                (fail! (:stderr check)))
              (do
                (swing-lowerer/write-source! (str clj-path) ast)
                (let [load-check (load-check-clojure-source clj-path)
                      ok? (zero? (:exit_code load-check))
                      final-check (if ok?
                                    {:exit_code 0
                                     :stdout (str (:stdout contract-check)
                                                  (:stdout shape-check)
                                                  (:stdout completeness-check)
                                                  "Swing Clojure source load OK\n")
                                     :stderr ""
                                     :validator "gui-page+swing-malli+source-load"}
                                    load-check)]
                  (sqlite/save-artifact! db-path session-id "final_check"
                                         (json/generate-string (json-safe final-check) {:pretty true}))
                  (spit (str report-path)
                        (json/generate-string
                         {:project_dir (str project-dir)
                          :session_id session-id
                          :db (str db-path)
                          :contract_file (:contract-file opts)
                          :task_file (:task-file opts)
                          :self_plan (boolean (:self-plan opts))
                          :component_pass (boolean (:component-pass opts))
                          :contract (json-safe contract)
                          :gui_ast (str gui-path)
                          :clj (str clj-path)
                          :ok ok?
                          :deterministic (not (:self-plan opts))
                          :check final-check}
                         {:pretty true}))
                  (if ok?
                    (do
                      (println "GUI:" (str gui-path))
                      (println "CLJ:" (str clj-path))
                      (println "REPORT:" (str report-path))
                      (println "DB:" (str db-path))
                      (println "OK"))
                    (fail! (:stderr final-check)))))))))
      (finally
        (model-io/stop-process! proc)))))

(defn gui-open [args]
  (ensure-args! args 1)
  (let [clj-path (first args)]
    (when-not (fs/exists? clj-path)
      (fail! (str "GUI source file does not exist: " clj-path)))
    (println "Opening Swing GUI from" clj-path)
    (println "This command intentionally launches a desktop window.")
    (let [result @(p/process ["clojure" "-M" "-i" clj-path "-m" "generated.swing-gui"]
                             {:out :inherit
                              :err :inherit})]
      (when-not (zero? (:exit result))
        (fail! (str "gui-open failed with exit code " (:exit result)))))))

(defn- run-process! [label args opts]
  (let [result @(p/process args (merge {:out :string :err :string} opts))]
    (when-not (zero? (:exit result))
      (throw (ex-info (str label " failed")
                      {:label label
                       :args args
                       :exit (:exit result)
                       :stdout (:out result)
                       :stderr (:err result)})))
    result))

(defn- path-separator []
  (System/getProperty "path.separator"))

(defn- classpath-entries []
  (let [result (run-process! "clojure classpath" ["clojure" "-Spath"] {})]
    (->> (str/split (str/trim (:out result)) (re-pattern (java.util.regex.Pattern/quote (path-separator))))
         (remove str/blank?)
         (map fs/path)
         vec)))

(defn- copy-generated-source! [candidate-clj src-dir]
  (let [target (fs/path src-dir "generated" "swing_gui.clj")]
    (fs/create-dirs (fs/parent target))
    (spit (str target) (slurp (str candidate-clj)))
    target))

(defn- remove-jar-signatures! [staging-dir]
  (let [meta-inf (fs/path staging-dir "META-INF")]
    (when (fs/exists? meta-inf)
      (doseq [path (fs/list-dir meta-inf)
              :let [file-name (str/upper-case (str (fs/file-name path)))]
              :when (or (str/ends-with? file-name ".SF")
                        (str/ends-with? file-name ".DSA")
                        (str/ends-with? file-name ".RSA"))]
        (fs/delete-if-exists path)))))

(defn- merge-classpath-entry! [staging-dir entry]
  (cond
    (and (fs/regular-file? entry) (str/ends-with? (str/lower-case (str entry)) ".jar"))
    (run-process! "dependency jar extract" ["jar" "xf" (str entry)] {:dir (str staging-dir)})

    (fs/directory? entry)
    nil

    :else nil))

(defn- compile-gui-main! [base-cp src-dir classes-dir]
  (let [cp (str/join (path-separator) (map str (concat base-cp [src-dir classes-dir])))
        compile-form (str "(binding [*compile-path* " (pr-str (str classes-dir)) "] "
                          "(compile 'generated.swing-gui))")]
    (run-process! "generated Swing AOT compile"
                  ["java" "-cp" cp "clojure.main" "-e" compile-form]
                  {})))

(defn- verify-gui-jar! [jar-path]
  (run-process! "generated Swing jar load"
                ["java" "-cp" (str jar-path)
                 "clojure.main" "-e"
                 "(require 'generated.swing-gui) (println :loaded)"]
                {}))

(defn gui-jar [args]
  (ensure-args! args 2)
  (let [[candidate-clj output-jar] (map fs/path args)
        output-jar (fs/absolutize output-jar)
        build-dir (fs/path (fs/parent output-jar) ".gui-jar-build")
        src-dir (fs/path build-dir "src")
        classes-dir (fs/path build-dir "classes")
        staging-dir (fs/path build-dir "staging")
        manifest-path (fs/path build-dir "MANIFEST.MF")]
    (when-not (fs/exists? candidate-clj)
      (fail! (str "GUI source file does not exist: " candidate-clj)))
    (fs/create-dirs (fs/parent output-jar))
    (when (fs/exists? build-dir)
      (fs/delete-tree build-dir))
    (fs/create-dirs classes-dir)
    (fs/create-dirs staging-dir)
    (copy-generated-source! candidate-clj src-dir)
    (let [base-cp (classpath-entries)]
      (compile-gui-main! base-cp src-dir classes-dir)
      (doseq [entry base-cp]
        (merge-classpath-entry! staging-dir entry))
      (remove-jar-signatures! staging-dir)
      (fs/copy-tree classes-dir staging-dir {:replace-existing true})
      (spit (str manifest-path) "Manifest-Version: 1.0\nMain-Class: generated.swing_gui\n\n")
      (run-process! "generated Swing jar build"
                    ["jar" "cfm" (str output-jar) (str manifest-path) "-C" (str staging-dir) "."]
                    {})
      (verify-gui-jar! output-jar)
      (println "JAR:" (str output-jar))
      (println "Main-Class: generated.swing_gui")
      (println "Load check: OK"))))

(defn dry-run [args]
  (ensure-args! args 1)
  (let [{:keys [project-dir ast-path clj-path report-path db-path]} (project-paths {:project-dir (first args)})
        session-id (sqlite/init-session! db-path "dry-run" "minimal JSON AST dry run")]
    (spit (str ast-path) minimal-ast)
    (sqlite/save-artifact! db-path session-id "json_ast" minimal-ast)
    (let [shape-check (validation/validate-ast-shape (json/parse-string minimal-ast true))]
      (when-not (:ok shape-check)
        (fail! (:stderr shape-check))))
    (clj-ast/compile-json-file (str ast-path) (str clj-path) {:overwrite? true})
    (spit (str report-path)
          (json/generate-string
           {:project_dir (str project-dir)
            :session_id session-id
            :db (str db-path)
            :ast (str ast-path)
            :clj (str clj-path)
            :ok true
            :dry_run true}
           {:pretty true}))
    (println (str report-path))))

(defn chat-completion [opts]
  (model-io/chat-completion (update opts :request-constraint-fn #(or % validation/request-constraint))))

(defn- self-plan! [{:keys [endpoint model task temperature planner-max-tokens db-path session-id]}]
  (let [messages [{:role "user" :content (prompt/planner-prompt task)}]
        content (chat-completion {:endpoint endpoint
                                  :model model
                                  :messages messages
                                  :temperature temperature
                                  :max-tokens (or planner-max-tokens 4096)
                                  :grammar nil
                                  :semantic-contract nil})
        parsed (json-extract content)
        contract (validation/sanitize-contract (:sanitized_contract parsed))
        artifact {:raw_output content
                  :parsed parsed
                  :sanitized_contract contract}]
    (sqlite/save-artifact! db-path session-id "self_plan" (json/generate-string (json-safe artifact) {:pretty true}))
    artifact))

(defn- generation-guidance [opts semantic-path self-plan-artifact]
  (cond
    (:allow-semantic-guidance opts)
    {:mode "semantic_assisted"
     :semantic_contract (validation/semantic-contract semantic-path)
     :note "Sanitized semantic fixture contract is injected into prompt/schema. Expected values and examples are withheld."}

    (:self-plan opts)
    {:mode "self_planned"
     :semantic_contract (:sanitized_contract self-plan-artifact)
     :note "Model-derived sanitized contract from a planner pass is injected into prompt/schema. This is self-generated guidance, not independent validation."}

    :else
    {:mode "text_only"
     :semantic_contract nil
     :note "Semantic fixture, if present, is used only after generation for validation."}))

(defn- kebab [value]
  (some-> value
          str
          str/lower-case
          (str/replace #"[^a-z0-9]+" "-")
          (str/replace #"(^-+|-+$)" "")))

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

(defn- input-field-types [contract]
  (let [metadata-keys #{:name :field :type :collection :arg_index :fields :field_shapes}
        normalize-type (fn [value]
                         (cond
                           (map? value) (or (:type value) "unknown")
                           (keyword? value) (name value)
                           (string? value) value
                           :else "unknown"))
        from-field (fn [field]
                     (when (map? field)
                       (when-let [field-name (or (:name field) (:field field))]
                         [(name field-name) (normalize-type field)])))]
    (->> (:input_shapes contract)
         (mapcat (fn [shape]
                   (let [fields (:fields shape)]
                     (concat
                      (when (and (map? shape) (= "collection" (:type shape)) (map? (:items shape)))
                        (map (fn [[k v]] [(name k) (normalize-type v)]) (:items shape)))
                      (when (map? (:field_shapes shape))
                        (map (fn [[k v]] [(name k) (normalize-type v)]) (:field_shapes shape)))
                      (cond
                        (map? fields) (map (fn [[k v]] [(name k) (normalize-type v)]) fields)
                        (sequential? fields) (keep from-field fields)
                        (:field shape) [[(name (:field shape)) (normalize-type shape)]]
                        (map? shape) (->> shape
                                          (remove (fn [[k _]] (contains? metadata-keys k)))
                                          (map (fn [[k v]] [(name k) (normalize-type v)])))
                        :else [])))))
         (reduce (fn [acc [field-name field-type]]
                   (assoc acc field-name field-type))
                 {})
         (sort-by first)
         (map (fn [[field-name field-type]] (str field-name ": " field-type)))
         (str/join ", "))))

(defn- inferred-field-types-from-task [task]
  (let [number-pattern #"(?i)\b([a-z][a-z0-9-]*)\b(?:\s+is)?\s+(?:less than|greater than|at least|at most|below|above|over|under|>=|<=|>|<)\s+\d+(?:\.\d+)?"
        string-pattern #"(?i)\b([a-z][a-z0-9-]*)\b(?:\s+is| equals| equal to)?\s+\"[^\"]+\""]
    (merge
     (->> (re-seq number-pattern task)
          (map second)
          (map (fn [field-name] [(kebab field-name) "number"]))
          (into {}))
     (->> (re-seq string-pattern task)
          (map second)
          (map (fn [field-name] [(kebab field-name) "string"]))
          (into {})))))

(defn- threshold-invariants-from-task [task]
  (let [patterns [{:operator "<" :regex #"(?i)\b([a-z][a-z0-9-]*)\b(?:\s+is)?\s+(?:less than|below|under)\s+(\d+(?:\.\d+)?)"}
                  {:operator ">" :regex #"(?i)\b([a-z][a-z0-9-]*)\b(?:\s+is)?\s+(?:greater than|above|over)\s+(\d+(?:\.\d+)?)"}
                  {:operator ">=" :regex #"(?i)\b([a-z][a-z0-9-]*)\b(?:\s+is)?\s+(?:at least)\s+(\d+(?:\.\d+)?)"}
                  {:operator "<=" :regex #"(?i)\b([a-z][a-z0-9-]*)\b(?:\s+is)?\s+(?:at most)\s+(\d+(?:\.\d+)?)"}
                  {:operator ">" :regex #"(?i)\b([a-z][a-z0-9-]*)\b\s*>\s*(\d+(?:\.\d+)?)"}
                  {:operator "<" :regex #"(?i)\b([a-z][a-z0-9-]*)\b\s*<\s*(\d+(?:\.\d+)?)"}
                  {:operator ">=" :regex #"(?i)\b([a-z][a-z0-9-]*)\b\s*>=\s*(\d+(?:\.\d+)?)"}
                  {:operator "<=" :regex #"(?i)\b([a-z][a-z0-9-]*)\b\s*<=\s*(\d+(?:\.\d+)?)"}]
        parse-number (fn [value]
                       (if (str/includes? value ".")
                         (Double/parseDouble value)
                         (Long/parseLong value)))]
    (->> patterns
         (mapcat (fn [{:keys [operator regex]}]
                   (map (fn [[_ field-name value]]
                          {:field (kebab field-name)
                           :operator operator
                           :value (parse-number value)
                           :source "text-spec"})
                        (re-seq regex task))))
         distinct
         vec)))

(defn- filter-rules-from-task [task]
  (let [sentences (->> (str/split (str task) #"(?<=[.!?])\s+|\n+")
                       (map str/trim)
                       (remove str/blank?))
        filter-pattern #"(?i)\b(filter|where|keep only|keeps only|include only|exclude|omit|drop|remove|only rows|rows where|records where)\b"]
    (->> sentences
         (filter #(re-find filter-pattern %))
         vec)))

(defn- enrich-contract-field-types [task contract]
  (if-not contract
    contract
    (let [inferred-types (inferred-field-types-from-task task)
          filter-rules (filter-rules-from-task task)
          enrich-field (fn [field]
                         (cond
                           (map? field)
                           (let [field-name (some-> (or (:name field) (:field field)) kebab)
                                 field-type (or (:type field)
                                                (get inferred-types field-name))]
                             (cond-> field
                               field-type (assoc :type field-type)))

                           field
                           (let [field-name (kebab field)
                                 field-type (get inferred-types field-name)]
                             (if field-type
                               {:name field-name :type field-type}
                               field))

                           :else field))
          enrich-shape (fn [shape]
                         (cond
                           (and (map? shape) (:field shape))
                           (enrich-field shape)

                           (and (map? shape) (sequential? (:fields shape)))
                           (update shape :fields #(mapv enrich-field %))

                           (and (map? shape) (map? (:fields shape)))
                           (update shape :fields
                                   (fn [fields]
                                     (reduce-kv (fn [acc field-name field-shape]
                                                  (let [field-key (kebab field-name)
                                                        inferred-type (get inferred-types field-key)]
                                                    (assoc acc field-name
                                                           (if (map? field-shape)
                                                             (cond-> field-shape
                                                               (and inferred-type (not (:type field-shape)))
                                                               (assoc :type inferred-type))
                                                             (or field-shape inferred-type)))))
                                                {}
                                                fields)))

                           :else shape))]
      (cond-> (update contract :input_shapes #(mapv enrich-shape %))
        (seq (threshold-invariants-from-task task))
        (assoc :threshold_invariants (threshold-invariants-from-task task))

        (seq filter-rules)
        (assoc :filter_required true
               :filter_rules filter-rules)))))

(defn- passthrough-entry [output-key item]
  {:key output-key
   :expr {:op "field"
          :target {:op "var" :name item}
          :field output-key}})

(defn- some-indexed [f coll]
  (first (keep-indexed f coll)))

(declare query-select-entries-path)

(declare query-path)

(defn- child-query-path [expr path]
  (when (map? expr)
    (case (:op expr)
      "query" path
      "map" (some-indexed (fn [idx entry]
                            (query-path (:expr entry) (conj path :entries idx :expr)))
                          (:entries expr))
      "vector" (some-indexed (fn [idx item]
                               (query-path item (conj path :items idx)))
                             (:items expr))
      "let" (or (some-indexed (fn [idx binding]
                                (query-path (:expr binding) (conj path :bindings idx :expr)))
                              (:bindings expr))
                (query-path (:body expr) (conj path :body)))
      "if" (or (query-path (:then expr) (conj path :then))
               (query-path (:else expr) (conj path :else)))
      "cond" (or (some-indexed (fn [idx clause]
                                 (query-path (:expr clause) (conj path :clauses idx :expr)))
                               (:clauses expr))
                 (query-path (:else expr) (conj path :else)))
      nil)))

(defn- query-path [expr path]
  (child-query-path expr path))

(defn- ast-query-path [ast]
  (some-indexed (fn [idx defn-ast]
                  (query-path (:expr defn-ast) [:defs idx :expr]))
                (:defs ast)))

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

(defn- replace-map-entry [ast entries-path entry]
  (let [entry-key (:key entry)
        entries (vec (get-in ast entries-path))
        idx (first (keep-indexed (fn [idx existing]
                                   (when (= entry-key (:key existing)) idx))
                                 entries))
        updated (if idx
                  (assoc entries idx entry)
                  (conj entries entry))]
    (assoc-in ast entries-path updated)))

(defn- scoped-column-contract [contract output-key]
  (-> contract
      (assoc :required_output_keys [output-key])
      (update :output_key_shapes #(select-keys % [(keyword output-key) output-key]))))

(defn- scoped-column-candidate [ast entries-path entry]
  (assoc-in ast entries-path [entry]))

(defn- query-item-name [ast]
  (letfn [(walk [expr]
            (when (map? expr)
              (case (:op expr)
                "query" (:item expr)
                "map" (some #(walk (:expr %)) (:entries expr))
                "vector" (some walk (:items expr))
                "let" (or (some #(walk (:expr %)) (:bindings expr))
                          (walk (:body expr)))
                "if" (or (walk (:then expr)) (walk (:else expr)))
                "cond" (or (some #(walk (:expr %)) (:clauses expr))
                           (walk (:else expr)))
                nil)))]
    (some #(walk (:expr %)) (:defs ast))))

(defn- normalize-field-targets [expr item-name]
  (if-not (map? expr)
    expr
    (let [recur-expr #(normalize-field-targets % item-name)
          op (:op expr)]
      (case op
        "field" (-> expr
                    (assoc :target {:op "var" :name item-name})
                    (update :target recur-expr))
        "call" (update expr :args #(mapv recur-expr %))
        "map" (update expr :entries #(mapv (fn [entry] (update entry :expr recur-expr)) %))
        "vector" (update expr :items #(mapv recur-expr %))
        "let" (-> expr
                  (update :bindings #(mapv (fn [binding] (update binding :expr recur-expr)) %))
                  (update :body recur-expr))
        "if" (-> expr
                 (update :condition recur-expr)
                 (update :then recur-expr)
                 (update :else recur-expr))
        "cond" (-> expr
                   (update :clauses #(mapv (fn [clause]
                                             (-> clause
                                                 (update :condition recur-expr)
                                                 (update :expr recur-expr)))
                                           %))
                   (update :else recur-expr))
        "query" (-> expr
                    (update :collection recur-expr)
                    (update :where #(mapv recur-expr (or % [])))
                    (cond-> (get-in expr [:sort :expr])
                      (update-in [:sort :expr] recur-expr))
                    (update :select recur-expr))
        ("=" "not=" ">" "<" ">=" "<=") (-> expr
                                             (update :left recur-expr)
                                             (update :right recur-expr))
        ("and" "or") (update expr :args #(mapv recur-expr %))
        ("not" "nil?" "some?") (update expr :expr recur-expr)
        expr))))

(defn- normalize-column-entry [entry output-key item-name]
  (let [entry (if (and (not (:expr entry)) (:op entry))
                {:key output-key :expr entry}
                (assoc entry :key output-key))
        entry (dissoc entry :translation_note)]
    (if (and item-name (:expr entry))
      (update entry :expr normalize-field-targets item-name)
      entry)))

(defn- fallback-condition? [condition-text]
  (boolean
   (re-find #"(?i)\b(otherwise|else|default|fallback|none|no business condition)\b"
            (str condition-text))))

(defn- literal-expr [value]
  (cond
    (nil? value) {:op "nil"}
    (boolean? value) {:op "bool" :value value}
    (integer? value) {:op "int" :value value}
    (number? value) {:op "float" :value value}
    :else {:op "string" :value (str value)}))

(defn- scalar-output-value [value output-key]
  (if (map? value)
    (or (get value (keyword output-key))
        (get value output-key)
        (when (= 1 (count value))
          (first (vals value)))
        (str value))
    value))

(defn- output-expr [value output-key item-name input-fields]
  (let [value (scalar-output-value value output-key)
        value-name (when (string? value) (kebab value))]
    (if (and value-name (contains? input-fields value-name))
      {:op "field"
       :target {:op "var" :name item-name}
       :field value-name}
      (literal-expr value))))

(defn- normalize-rule [rule]
  (when (map? rule)
    {:condition_text (str (or (:condition_text rule) (:condition-text rule) ""))
     :output_value (or (:output_value rule) (:output-value rule))}))

(defn- deconstruct-column! [{:keys [endpoint model task temperature column-max-tokens generation-contract output-key db-path session-id attempt-index grammar]}]
  (let [messages [{:role "user"
                   :content (prompt/deconstruct-column-prompt task generation-contract output-key)}]
        content (chat-completion {:endpoint endpoint
                                  :model model
                                  :messages messages
                                  :temperature temperature
                                  :max-tokens (or column-max-tokens 2048)
                                  :grammar grammar
                                  :semantic-contract nil
                                  :request-constraint-fn (validation/deconstructed-column-request-constraint)})
        rules (->> (or (try-json-array-extract content) [])
                   (keep normalize-rule)
                   (filter #(seq (:condition_text %)))
                   vec)]
    (sqlite/save-artifact! db-path
                           session-id
                           (str "column_rules_" attempt-index "_" output-key)
                           (json/generate-string {:raw_output content
                                                  :rules (json-safe rules)}
                                                 {:pretty true}))
    rules))

(defn- assemble-cond-ast [rules-with-predicates output-key item-name input-fields]
  (let [fallback (or (last (filter #(fallback-condition? (:condition_text %)) rules-with-predicates))
                     (last rules-with-predicates))
        fallback-expr (output-expr (:output_value fallback) output-key item-name input-fields)
        clauses (->> rules-with-predicates
                     (remove #(identical? % fallback))
                     (remove #(fallback-condition? (:condition_text %)))
                     (keep (fn [{:keys [predicate output_value]}]
                             (when predicate
                               {:condition predicate
                                :expr (output-expr output_value output-key item-name input-fields)})))
                     vec)]
    (if (seq clauses)
      {:op "cond"
       :rationale "Programmatically assembled from micro-stepped column rules."
       :clauses clauses
       :else fallback-expr}
      fallback-expr)))

(defn- atomic-predicate-candidate [ast entries-path output-key predicate output-value]
  (scoped-column-candidate ast
                           entries-path
                           {:key output-key
                            :expr {:op "cond"
                                   :clauses [{:condition predicate
                                        :expr (literal-expr (scalar-output-value output-value output-key))}]
                                   :else (literal-expr (scalar-output-value output-value output-key))}}))

(defn- validate-atomic-predicate [ast entries-path generation-contract output-key predicate output-value]
  (let [candidate (atomic-predicate-candidate ast entries-path output-key predicate output-value)
        shape-check (validation/validate-ast-shape candidate)]
    (if-not (:ok shape-check)
      shape-check
      (validation/validate-semantic-structure candidate
                                             (scoped-column-contract generation-contract output-key)))))

(defn- atomic-predicate! [{:keys [endpoint model task temperature column-max-tokens generation-contract current-ast output-key db-path session-id attempt-index grammar schema-path entries-path condition-text output-value]}]
  (let [item-name (query-item-name current-ast)
        max-attempts 2
        atomic-max-tokens (max 2048 (or column-max-tokens 2048))]
    (loop [attempt 0
           error-text nil]
      (let [messages [{:role "user"
                       :content (prompt/atomic-predicate-prompt task
                                                                generation-contract
                                                                (input-field-types generation-contract)
                                                                item-name
                                                                condition-text
                                                                nil
                                                                error-text)}]
            content (chat-completion {:endpoint endpoint
                                      :model model
                                      :messages messages
                                      :temperature temperature
                                      :max-tokens atomic-max-tokens
                                      :grammar grammar
                                      :semantic-contract nil
                                      :request-constraint-fn (validation/atomic-predicate-request-constraint
                                                              (or schema-path "schemas/clojure_ast.schema.json"))})
            parsed (some-> (try-json-extract content)
                           (normalize-field-targets item-name))
            check (if parsed
                    (validate-atomic-predicate current-ast entries-path generation-contract output-key parsed output-value)
                    {:ok false
                     :exit_code 1
                     :stderr "model did not return a predicate JSON object"})]
        (sqlite/save-artifact! db-path
                               session-id
                               (str "atomic_predicate_" attempt-index "_" output-key "_" attempt)
                               (json/generate-string {:condition_text condition-text
                                                      :output_value output-value
                                                      :raw_output content
                                                      :predicate (json-safe parsed)
                                                      :check (json-safe check)}
                                                     {:pretty true}))
        (if (:ok check)
          parsed
          (if (< attempt max-attempts)
            (recur (inc attempt) (:stderr check))
            nil))))))

(defn- micro-column-entry! [{:keys [current-ast output-key entries-path] :as opts}]
  (let [rules (deconstruct-column! opts)
        item-name (query-item-name current-ast)
        input-fields (input-field-names (:generation-contract opts))
        fallback? #(fallback-condition? (:condition_text %))
        compiled (loop [remaining rules
                        acc []]
                   (if-let [rule (first remaining)]
                     (if (fallback? rule)
                       (recur (rest remaining) (conj acc rule))
                       (if-let [predicate (atomic-predicate! (assoc opts
                                                                    :condition-text (:condition_text rule)
                                                                    :output-value (:output_value rule)))]
                         (recur (rest remaining) (conj acc (assoc rule :predicate predicate)))
                         acc))
                     acc))
        expr (when (and (seq rules)
                        (= (count rules) (count compiled)))
               (assemble-cond-ast compiled output-key item-name input-fields))
        entry (when expr
                (normalize-column-entry {:key output-key :expr expr}
                                        output-key
                                        item-name))]
    (sqlite/save-artifact! (:db-path opts)
                           (:session-id opts)
                           (str "column_micro_entry_" (:attempt-index opts) "_" output-key)
                           (json/generate-string {:rules (json-safe rules)
                                                  :compiled (json-safe compiled)
                                                  :entry (json-safe entry)}
                                                 {:pretty true}))
    (or entry {:key output-key :expr {}})))

(defn- column-entry! [{:keys [endpoint model task temperature column-max-tokens generation-contract current-ast output-key db-path session-id attempt-index grammar schema-path]}]
  (let [current-ast-text (json/generate-string current-ast)
        messages [{:role "user"
                   :content (prompt/column-entry-prompt task generation-contract current-ast-text output-key)}]
        content (chat-completion {:endpoint endpoint
                                  :model model
                                  :messages messages
                                  :temperature temperature
                                  :max-tokens (or column-max-tokens 2048)
                                  :grammar grammar
                                  :semantic-contract nil
                                  :request-constraint-fn (validation/column-entry-request-constraint
                                                          (or schema-path "schemas/clojure_ast.schema.json")
                                                          output-key)})
        item-name (query-item-name current-ast)
        parsed (try-json-extract content)
        entry (if parsed
                (normalize-column-entry parsed output-key item-name)
                {:key output-key :expr {}})]
    (sqlite/save-artifact! db-path
                           session-id
                           (str "column_entry_" attempt-index "_" output-key)
                           (json/generate-string {:raw_output content
                                                  :translation_note (:translation_note parsed)
                                                  :entry (json-safe entry)}
                                                 {:pretty true}))
    entry))

(defn- rule-text [rule]
  (cond
    (string? rule) rule
    (map? rule) (or (when (and (:field rule) (:operator rule) (contains? rule :value))
                      (str (:field rule) " " (:operator rule) " " (:value rule)))
                    (:condition_text rule)
                    (:condition-text rule)
                    (:condition rule)
                    (:rule rule)
                    (:text rule)
                    (:logic rule)
                    (:filter_logic rule)
                    (:filtering_logic rule)
                    (first (keep (fn [[_ value]]
                                   (when (string? value) value))
                                 rule)))
    :else nil))

(defn- raw-filter-rules [contract]
  (->> (concat (:filtering_conditions contract)
               (:filter_conditions contract)
               (:filtering_rules contract)
               (:filter_rules contract)
               (when-let [logic (:filter_logic contract)] [logic])
               (when-let [logic (:filtering_logic contract)] [logic]))
       (keep rule-text)
       (map str/trim)
       (remove str/blank?)
       vec))

(defn- condition-like-text? [text]
  (boolean
   (and (not (re-find #"(?i)^do not include\b.*\boutside the filter\b" (str/trim text)))
        (re-find #"(?i)(>|<|=|\bgreater than\b|\bless than\b|\bat least\b|\bat most\b|\babove\b|\bbelow\b|\bover\b|\bunder\b|\bequals?\b|\bis\b|\bpresent\b|\bmissing\b|\bnil\b|\btrue\b|\bfalse\b)"
                 text))))

(defn- normalize-filter-text [text]
  (-> (str text)
      (str/replace #"^\s*[-*]\s*" "")
      (str/replace #"(?i)^filter(?:ing)?\s+(?:condition|rule|criteria)\s*:\s*" "")
      (str/replace #"(?i)^keep(?:s)?(?:\s+only)?\s+[a-z][a-z0-9-]*\s+where\s+" "")
      (str/replace #"(?i)^include(?:\s+only)?\s+[a-z][a-z0-9-]*\s+where\s+" "")
      (str/replace #"(?i)^only\s+[a-z][a-z0-9-]*\s+where\s+" "")
      (str/replace #"(?i)^where\s+" "")
      (str/replace #"[.;]\s*$" "")
      str/trim))

(defn- split-filter-rule [text]
  (let [text (normalize-filter-text text)
        splitter (cond
                   (re-find #"(?i)\s+\bor\b\s+" text) #"(?i)\s+\bor\b\s+"
                   (re-find #"(?i)\s+\band\b\s+" text) #"(?i)\s+\band\b\s+"
                   :else nil)]
    (->> (if splitter (str/split text splitter) [text])
         (map normalize-filter-text)
         (filter condition-like-text?)
         vec)))

(defn- filter-condition-texts [contract]
  (->> (raw-filter-rules contract)
       (mapcat split-filter-rule)
       distinct
       vec))

(defn- filter-join-op [contract condition-texts]
  (let [text (str/join "\n" (concat (raw-filter-rules contract)
                                    (:rules contract)
                                    (keep identity [(:filter_logic contract)
                                                    (:filtering_logic contract)])))]
    (cond
      (re-find #"(?i)\bOR\b|\bor\b|alternatives?" text) "or"
      (re-find #"(?i)\bAND\b|\band\b|all\s+of" text) "and"
      (> (count condition-texts) 1) "or"
      :else "and")))

(defn- where-predicate-candidate [ast query-path predicate]
  (assoc-in ast (conj query-path :where) [predicate]))

(defn- where-validation-contract [generation-contract]
  (dissoc generation-contract :output_key_shapes :output_key_values))

(defn- validate-where-predicate [ast query-path generation-contract predicate]
  (let [candidate (where-predicate-candidate ast query-path predicate)
        shape-check (validation/validate-ast-shape candidate)]
    (if-not (:ok shape-check)
      shape-check
      (validation/validate-semantic-structure candidate
                                             (where-validation-contract generation-contract)))))

(defn- atomic-where-predicate! [{:keys [endpoint model task temperature where-max-tokens generation-contract current-ast db-path session-id attempt-index grammar schema-path query-path condition-text rule-index]}]
  (let [item-name (query-item-name current-ast)
        max-attempts 2
        atomic-max-tokens (max 2048 (or where-max-tokens 2048))]
    (loop [attempt 0
           error-text nil]
      (let [messages [{:role "user"
                       :content (prompt/atomic-predicate-prompt task
                                                                generation-contract
                                                                (input-field-types generation-contract)
                                                                item-name
                                                                condition-text
                                                                nil
                                                                error-text)}]
            content (chat-completion {:endpoint endpoint
                                      :model model
                                      :messages messages
                                      :temperature temperature
                                      :max-tokens atomic-max-tokens
                                      :grammar grammar
                                      :semantic-contract nil
                                      :request-constraint-fn (validation/atomic-predicate-request-constraint
                                                              (or schema-path "schemas/clojure_ast.schema.json"))})
            parsed (some-> (try-json-extract content)
                           (normalize-field-targets item-name))
            check (if parsed
                    (validate-where-predicate current-ast query-path generation-contract parsed)
                    {:ok false
                     :exit_code 1
                     :stderr "model did not return a predicate JSON object"})]
        (sqlite/save-artifact! db-path
                               session-id
                               (str "where_atomic_predicate_" attempt-index "_" rule-index "_" attempt)
                               (json/generate-string {:condition_text condition-text
                                                      :raw_output content
                                                      :predicate (json-safe parsed)
                                                      :check (json-safe check)}
                                                     {:pretty true}))
        (if (:ok check)
          parsed
          (if (< attempt max-attempts)
            (recur (inc attempt) (:stderr check))
            nil))))))

(defn- assemble-where-vector [join-op predicates]
  (cond
    (empty? predicates) []
    (= 1 (count predicates)) [(first predicates)]
    :else [{:op join-op :args (vec predicates)}]))

(defn- where-predicates! [{:keys [generation-contract current-ast db-path session-id attempt-index query-path] :as opts}]
  (let [condition-texts (filter-condition-texts generation-contract)
        join-op (filter-join-op generation-contract condition-texts)
        compiled (loop [remaining (map-indexed vector condition-texts)
                        acc []]
                   (if-let [[rule-index condition-text] (first remaining)]
                     (if-let [predicate (atomic-where-predicate! (assoc opts
                                                                        :condition-text condition-text
                                                                        :rule-index rule-index))]
                       (recur (rest remaining) (conj acc {:condition_text condition-text
                                                          :predicate predicate}))
                       acc)
                     acc))
        predicates (mapv :predicate compiled)
        where-vector (when (= (count condition-texts) (count compiled))
                       (assemble-where-vector join-op predicates))]
    (sqlite/save-artifact! db-path
                           session-id
                           (str "where_micro_predicates_" attempt-index)
                           (json/generate-string {:raw_rules (json-safe (raw-filter-rules generation-contract))
                                                  :condition_texts condition-texts
                                                  :join_op join-op
                                                  :compiled (json-safe compiled)
                                                  :where (json-safe where-vector)}
                                                 {:pretty true}))
    (or where-vector [])))

(defn- refine-where! [{:keys [ast generation-contract] :as opts}]
  (if-let [query-path (ast-query-path ast)]
    (let [predicates (where-predicates! (assoc opts
                                               :current-ast ast
                                               :query-path query-path))
          candidate (assoc-in ast (conj query-path :where) predicates)
          shape-check (validation/validate-ast-shape candidate)
          semantic-check (when (:ok shape-check)
                           (validation/validate-semantic-structure
                            candidate
                            (where-validation-contract generation-contract)))
          accepted? (and (:ok shape-check) (:ok semantic-check))]
      (if accepted?
        candidate
        (do
          (sqlite/save-artifact! (:db-path opts)
                                 (:session-id opts)
                                 (str "where_predicates_rejected_" (:attempt-index opts))
                                 (json/generate-string {:predicates (json-safe predicates)
                                                        :status "rejected"
                                                        :reason (if (:ok shape-check)
                                                                  "semantic structure validation failed"
                                                                  "shape validation failed")
                                                        :check (json-safe (if (:ok shape-check)
                                                                            semantic-check
                                                                            shape-check))}
                                                       {:pretty true}))
          ast)))
    ast))

(defn- refine-columns! [{:keys [ast generation-contract] :as opts}]
  (let [output-keys (mapv name (:required_output_keys generation-contract))
        input-fields (input-field-names generation-contract)
        entries-path (ast-select-entries-path ast)]
    (if (and (seq output-keys) entries-path)
      (reduce (fn [acc output-key]
                (if (contains? input-fields output-key)
                  (do
                    (sqlite/save-artifact! (:db-path opts)
                                           (:session-id opts)
                                           (str "column_entry_deterministic_" (:attempt-index opts) "_" output-key)
                                           (json/generate-string {:key output-key
                                                                  :status "accepted"
                                                                  :reason "output key matches input field"
                                                                  :entry (json-safe (passthrough-entry output-key (query-item-name acc)))}
                                                                 {:pretty true}))
                    acc)
                  (let [entry (micro-column-entry! (assoc opts
                                                          :current-ast acc
                                                          :output-key output-key
                                                          :entries-path entries-path))
                      candidate (replace-map-entry acc entries-path entry)
                      scoped-candidate (scoped-column-candidate acc entries-path entry)
                      shape-check (validation/validate-ast-shape candidate)
                      semantic-check (when (:ok shape-check)
                                       (validation/validate-semantic-structure
                                        scoped-candidate
                                        (scoped-column-contract generation-contract output-key)))
                      accepted? (and (:ok shape-check) (:ok semantic-check))]
                    (if accepted?
                      candidate
                      (do
                        (sqlite/save-artifact! (:db-path opts)
                                               (:session-id opts)
                                               (str "column_entry_rejected_" (:attempt-index opts) "_" output-key)
                                               (json/generate-string {:entry (json-safe entry)
                                                                      :status "rejected"
                                                                      :reason (if (:ok shape-check)
                                                                                "semantic structure validation failed"
                                                                                "shape validation failed")
                                                                      :check (json-safe (if (:ok shape-check)
                                                                                          semantic-check
                                                                                          shape-check))}
                                                                     {:pretty true}))
                        acc)))))
              ast
              output-keys)
      ast)))

(defn check-and-compile
  ([ast-path clj-path semantic-path]
   (check-and-compile ast-path clj-path semantic-path {}))
  ([ast-path clj-path semantic-path {:keys [db-path session-id attempt-index trace?]
                                     :or {trace? false}}]
   (try
     (fs/delete-if-exists clj-path)
     (let [ast (json/parse-string (slurp (str ast-path)) true)
           semantic-contract (validation/validation-contract semantic-path)
           shape-check (validation/validate-ast-shape ast)
           semantic-structure-check (when (:ok shape-check)
                                      (validation/validate-semantic-structure ast semantic-contract))
           clean-forms (when (and (:ok shape-check) (:ok semantic-structure-check))
                         (clj-ast/forms ast))
           eval-forms (when clean-forms
                        (if (and semantic-path trace?)
                          (clj-ast/forms ast {:trace-fn (symbol "__harness-spy")})
                          clean-forms))]
       (when-not (:ok shape-check)
         (throw (ex-info "Malli validation failed" {:check shape-check})))
       (when-not (:ok semantic-structure-check)
         (throw (ex-info "Semantic structure validation failed" {:check semantic-structure-check})))
       (if semantic-path
         (let [semantic-check (semantic/run-semantic-forms eval-forms semantic-path
                                                           {:db-path db-path
                                                            :session-id session-id
                                                            :attempt-index attempt-index
                                                            :trace? trace?})]
           (if (zero? (:exit_code semantic-check))
             (do
               (clj-ast/write-forms! (str clj-path) clean-forms)
               {:exit_code 0
                :stdout (str "Malli OK\nClojure AST eval OK\nClojure AST codegen OK\n" (:stdout semantic-check))
                :stderr ""
                :validator "malli+clj-ast-eval+codegen+semantic"
                :semantic (:semantic semantic-check)})
             semantic-check))
         (do
           (clj-ast/write-forms! (str clj-path) clean-forms)
           {:exit_code 0
            :stdout "Malli OK\nClojure AST codegen OK\nSemantic SKIPPED (no fixture)\n"
            :stderr ""
            :validator "malli+clj-ast"
            :semantic {:skipped true :reason "no semantic test fixture"}})))
     (catch clojure.lang.ExceptionInfo ex
       (if-let [check (:check (ex-data ex))]
         check
         {:exit_code 1
          :stdout ""
          :stderr (str (.getMessage ex) "\n" (pr-str (ex-data ex)))
          :validator "clj-ast"}))
     (catch Exception ex
       {:exit_code 1
        :stdout ""
        :stderr (str (.getMessage ex))
        :validator "harness"}))))

(defn- diagnostic-text [check]
  (let [semantic (:semantic check)]
    (str (:stderr check)
         (when semantic
           (str "\nSemantic details:\n"
                (pr-str semantic))))))

(defn run-json [args]
  (let [opts (parse-args args)
        task (task-text opts)
        {:keys [project-dir ast-path clj-path report-path db-path log-path]} (project-paths opts)
        host (or (:llama-host opts) "127.0.0.1")
        port (or (:llama-port opts) 18080)
        endpoint (or (:endpoint opts)
                     (when (:gguf opts) (model-io/local-endpoint host port)))
        model (model-io/model-name opts)
        repair-attempts (or (:repair-attempts opts) 3)
        max-tokens (or (:max-tokens opts) 8192)
        temperature (or (:temperature opts) 0.2)
        grammar (model-io/constrained-decoding opts)
        semantic-path (semantic-test-path opts)
        validation-contract (validation/validation-contract semantic-path)
        session-id (sqlite/init-session! db-path model task)
        proc (model-io/launch-llama! opts log-path)]
    (try
      (when-not endpoint
        (fail! "--endpoint or --gguf is required"))
      (when (and (:self-plan opts) (:allow-semantic-guidance opts))
        (fail! "--self-plan and --allow-semantic-guidance are mutually exclusive"))
      (when (and (:skeleton-first opts) (not (:self-plan opts)))
        (fail! "--skeleton-first requires --self-plan"))
      (when (and (:skeleton-first opts) (not (:column-pass opts)))
        (fail! "--skeleton-first requires --column-pass"))
      (let [self-plan-artifact (when (:self-plan opts)
                                 (self-plan! {:endpoint endpoint
                                              :model model
                                              :task task
                                              :temperature temperature
                                              :planner-max-tokens (:planner-max-tokens opts)
                                              :db-path db-path
                                              :session-id session-id}))
            oracle (semantic-oracle opts semantic-path)
            guidance (generation-guidance opts semantic-path self-plan-artifact)
            generation-contract (some->> (:semantic_contract guidance)
                                         (enrich-contract-field-types task)
                                         protocols/normalize-contract)
            guidance (assoc guidance :semantic_contract generation-contract)
            semantic-assisted? (= "semantic_assisted" (:mode guidance))]
      (when (and (:column-pass opts)
                 (not (seq (:required_output_keys generation-contract))))
        (spit (str report-path)
              (json/generate-string
               {:project_dir (str project-dir)
                :session_id session-id
                :db (str db-path)
                :generation_guidance (dissoc guidance :semantic_contract)
                :semantic_contract_used_for_generation (json-safe generation-contract)
                :semantic_oracle oracle
                :check {:exit_code 1
                        :stdout ""
                        :stderr "[json-ast/planning-failed] Column pass requires required_output_keys, but the planner did not produce a usable sanitized contract."
                        :validator "planning-contract"}
                :ok false}
               {:pretty true}))
        (println (str report-path))
        (System/exit 1))
      (loop [attempt 0
             previous-ast nil
             error-text nil
             repairs []]
        (let [messages [{:role "user" :content (prompt/ast-prompt task generation-contract previous-ast error-text)}]
              skeleton? (and (:skeleton-first opts) (zero? attempt))
              content (if skeleton?
                        (json/generate-string
                         (protocols/assemble-skeleton-ast {:task task
                                                           :opts opts
                                                           :contract generation-contract})
                         {:pretty true})
                        (chat-completion {:endpoint endpoint
                                          :model model
                                          :messages messages
                                          :temperature temperature
                                          :max-tokens max-tokens
                                          :grammar grammar
                                          :semantic-contract generation-contract}))
              raw-ast (if skeleton?
                        (json/parse-string content true)
                        (json-extract content))
              pre-pass-shape-check (validation/validate-ast-shape raw-ast)
              pass-base (if (and (or (:where-pass opts) (:column-pass opts))
                                 generation-contract
                                 (:ok pre-pass-shape-check))
                          (do
                            (sqlite/save-artifact! db-path
                                                   session-id
                                                   (if skeleton?
                                                     (str "skeleton_json_ast_" attempt)
                                                     (str "pre_pass_json_ast_" attempt))
                                                   (json/generate-string raw-ast {:pretty true}))
                            raw-ast)
                          raw-ast)
              where-ast (if (and (:where-pass opts)
                                 generation-contract
                                 (:ok pre-pass-shape-check))
                          (protocols/refine-where {:ast pass-base
                                                   :endpoint endpoint
                                                   :model model
                                                   :task task
                                                   :temperature temperature
                                                   :where-max-tokens (:where-max-tokens opts)
                                                   :grammar grammar
                                                   :schema-path (:json-schema-file opts)
                                                   :generation-contract generation-contract
                                                   :contract generation-contract
                                                   :db-path db-path
                                                   :session-id session-id
                                                   :attempt-index attempt
                                                   :legacy-refine-where refine-where!})
                          pass-base)
              where-completeness-check (when (and (:where-pass opts)
                                                  generation-contract
                                                  (:ok pre-pass-shape-check))
                                         (protocols/completeness-check {:stage :post-where
                                                                        :task task
                                                                        :opts opts
                                                                        :ast where-ast
                                                                        :contract generation-contract
                                                                        :db-path db-path
                                                                        :session-id session-id
                                                                        :attempt-index attempt}))
              where-failed? (and where-completeness-check
                                  (not (zero? (:exit_code where-completeness-check))))
              _ (when where-completeness-check
                  (sqlite/save-artifact! db-path
                                         session-id
                                         (str "where_completeness_" attempt)
                                         (json/generate-string (json-safe where-completeness-check)
                                                               {:pretty true})))
              ast (if (and (:column-pass opts)
                           generation-contract
                           (:ok pre-pass-shape-check)
                           (not where-failed?))
                    (protocols/refine-columns {:ast where-ast
                                               :endpoint endpoint
                                               :model model
                                               :task task
                                               :temperature temperature
                                               :column-max-tokens (:column-max-tokens opts)
                                               :grammar grammar
                                               :schema-path (:json-schema-file opts)
                                               :generation-contract generation-contract
                                               :contract generation-contract
                                               :db-path db-path
                                               :session-id session-id
                                               :attempt-index attempt
                                               :legacy-refine-columns refine-columns!})
                    where-ast)
              ast-text (json/generate-string ast {:pretty true})]
          (spit (str ast-path) ast-text)
          (sqlite/save-artifact! db-path session-id "raw_model_output" content)
          (sqlite/save-artifact! db-path session-id "json_ast" ast-text)
          (let [refinement-check (when (and (:column-pass opts) generation-contract)
                                   (protocols/completeness-check {:stage :post-columns
                                                                  :task task
                                                                  :opts opts
                                                                  :ast ast
                                                                  :contract generation-contract
                                                                  :db-path db-path
                                                                  :session-id session-id
                                                                  :attempt-index attempt}))
                attempt-semantic-path (when semantic-assisted? semantic-path)
                check (cond
                        where-failed? where-completeness-check
                        (and refinement-check (not (zero? (:exit_code refinement-check)))) refinement-check
                        :else (check-and-compile ast-path clj-path attempt-semantic-path
                                                 {:db-path db-path
                                                  :session-id session-id
                                                  :attempt-index attempt
                                                  :trace? true}))
                structural-ok? (zero? (:exit_code check))
                final-check (if (and structural-ok? semantic-path (not semantic-assisted?))
                              (check-and-compile ast-path clj-path semantic-path
                                                 {:db-path db-path
                                                  :session-id session-id
                                                  :attempt-index attempt
                                                  :trace? true})
                              check)
                ok? (zero? (:exit_code final-check))]
            (sqlite/save-attempt! db-path session-id "implementation" attempt task messages content ast ast-text (json-safe check))
            (if (or ok?
                    (and structural-ok? (not ok?))
                    (>= attempt repair-attempts))
              (do
                (sqlite/save-artifact! db-path session-id "final_check" (json/generate-string (json-safe final-check) {:pretty true}))
                (spit (str report-path)
                      (json/generate-string
                       {:project_dir (str project-dir)
                        :session_id session-id
                        :db (str db-path)
                        :constrained_decoding grammar
                        :generation_guidance (dissoc guidance :semantic_contract)
                        :skeleton_first {:enabled (boolean (:skeleton-first opts))
                                         :note "When enabled, the base AST is a deterministic skeleton from the sanitized contract; column-pass fills projection logic."}
                        :where_pass {:enabled (boolean (:where-pass opts))
                                     :note "When enabled, a focused pass generates query.where predicate expressions and injects them before column-pass."}
                        :column_pass {:enabled (boolean (:column-pass opts))
                                      :note "When enabled, each projected output map entry is regenerated in a focused model pass after the full AST is structurally valid."}
                        :self_plan (when self-plan-artifact
                                     (json-safe (select-keys self-plan-artifact [:parsed :sanitized_contract])))
                        :semantic_oracle oracle
                        :semantic_test semantic-path
                        :semantic_contract_used_for_generation (json-safe generation-contract)
                        :semantic_contract_used_for_validation (json-safe validation-contract)
                        :ast (str ast-path)
                        :clj (str clj-path)
                        :endpoint endpoint
                        :model model
                        :repair_attempts repair-attempts
                        :repairs repairs
                        :check (json-safe final-check)
                        :ok ok?}
                       {:pretty true}))
                (println (str report-path))
                (when-not ok? (System/exit 1)))
              (recur (inc attempt)
                     ast-text
                     (diagnostic-text check)
                     (conj repairs {:check (json-safe check)})))))))
      (finally
        (model-io/stop-process! proc)))))

(defn chat-ping [args]
  (ensure-args! args 2)
  (let [[endpoint model] args
        content (chat-completion {:endpoint endpoint
                                  :model model
                                  :messages [{:role "user" :content "Reply with JSON: {\"ok\":true}"}]
                                  :temperature 0
                                  :max-tokens 64
                                  :grammar nil})]
    (println content)))
