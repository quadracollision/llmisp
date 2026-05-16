(ns harness.model
  (:require [babashka.fs :as fs]
            [babashka.http-client :as http]
            [babashka.process :as p]
            [cheshire.core :as json]
            [clojure.string :as str]))

(declare stop-process!)

(defn model-name [opts]
  (or (:model opts)
      (some-> (:gguf opts) fs/file-name str (str/replace #"\.gguf$" ""))
      (System/getenv "MODEL")
      "local-gemma"))

(defn local-endpoint [host port]
  (str "http://" host ":" port "/v1/chat/completions"))

(defn- http-ready? [url]
  (try
    (let [response (http/get url {:throw false})]
      (< (:status response) 500))
    (catch Exception _ false)))

(defn wait-for-server! [host port timeout-seconds]
  (let [deadline (+ (System/currentTimeMillis) (* 1000 timeout-seconds))]
    (loop []
      (when (> (System/currentTimeMillis) deadline)
        (throw (ex-info (str "timed out waiting for llama server on " host ":" port)
                        {:host host :port port})))
      (if (or (http-ready? (str "http://" host ":" port "/health"))
              (http-ready? (str "http://" host ":" port "/v1/models")))
        true
        (do (Thread/sleep 500)
            (recur))))))

(defn- gpu-fallback-log? [log-path]
  (let [log-text (try
                   (slurp (str log-path))
                   (catch Exception _ ""))]
    (boolean
     (or (re-find #"failed to initialize CUDA" log-text)
         (re-find #"no usable GPU found" log-text)))))

(defn constrained-decoding [opts]
  (if-let [grammar-file (:grammar-file opts)]
    {:mode "gbnf"
     :file grammar-file
     :llama_arg "--grammar-file"}
    {:mode "json-schema-gbnf"
     :file (or (:json-schema-file opts) "schemas/clojure_ast.schema.json")}))

(defn launch-llama! [opts log-path]
  (when-let [gguf (:gguf opts)]
    (let [server-bin (or (:llama-server-bin opts) "llama-server")
          host (or (:llama-host opts) "127.0.0.1")
          port (or (:llama-port opts) 18080)
          ctx (or (:llama-ctx-size opts) 16384)
          gpu-layers (or (:llama-gpu-layers opts) 999)
          grammar (constrained-decoding opts)
          cmd (cond-> [server-bin "-m" gguf "-c" (str ctx) "--host" host "--port" (str port) "-ngl" (str gpu-layers)]
                (:llama_arg grammar) (conj (:llama_arg grammar) (:file grammar)))
          log-file (java.io.File. (str log-path))
          proc (p/process cmd {:out log-file :err :out})]
      (Thread/sleep 1000)
      (when-not (.isAlive (:proc proc))
        (throw (ex-info (str "llama server exited during startup; see " log-path)
                        {:log_path (str log-path)})))
      (when (and (pos? gpu-layers) (gpu-fallback-log? log-path))
        (stop-process! proc)
        (throw (ex-info (str "llama server did not get a usable GPU; see " log-path)
                        {:log_path (str log-path)
                         :gpu_layers gpu-layers})))
      (wait-for-server! host port 120)
      (when-not (.isAlive (:proc proc))
        (throw (ex-info (str "llama server exited after readiness check; see " log-path)
                        {:log_path (str log-path)})))
      proc)))

(defn stop-process! [proc]
  (when proc
    (try
      (p/destroy proc)
      (catch Exception _ nil))))

(defn chat-completion [{:keys [endpoint model messages temperature max-tokens grammar semantic-contract request-constraint-fn]}]
  (let [request-constraint-fn (or request-constraint-fn
                                  (fn [_ _] {:response_format {:type "json_object"}}))
        body (merge {:model model
                     :messages messages
                     :temperature (or temperature 0.2)
                     :max_tokens (or max-tokens 256)}
                    (request-constraint-fn grammar semantic-contract))
        response (http/post endpoint
                            {:headers {"Content-Type" "application/json"}
                             :body (json/generate-string body)
                             :throw false})
        status (:status response)
        body (:body response)]
    (when-not (<= 200 status 299)
      (throw (ex-info (str "chat endpoint failed: HTTP " status)
                      {:status status :body body})))
    (get-in (json/parse-string body true) [:choices 0 :message :content])))
