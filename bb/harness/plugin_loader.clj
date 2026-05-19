(ns harness.plugin-loader
  (:require [babashka.fs :as fs]
            [clojure.edn :as edn]))

(def default-config-path "plugins.edn")

(defonce loaded-plugins (atom #{}))

(defn read-config
  ([] (read-config default-config-path))
  ([path]
   (if (fs/exists? path)
     (edn/read-string (slurp (str path)))
     {:plugins []})))

(defn load-plugin! [plugin-ns]
  (let [plugin-sym (if (symbol? plugin-ns)
                     plugin-ns
                     (symbol (str plugin-ns)))]
    (when-not (@loaded-plugins plugin-sym)
      (try
        (require plugin-sym)
        (swap! loaded-plugins conj plugin-sym)
        (catch Throwable t
          (throw (ex-info (str "Failed to load plugin namespace " plugin-sym)
                          {:plugin plugin-sym}
                          t)))))
    plugin-sym))

(defn load-configured-plugins!
  ([] (load-configured-plugins! default-config-path))
  ([path]
   (let [config (read-config path)
         plugins (:plugins config)]
     (when-not (sequential? plugins)
       (throw (ex-info "plugins.edn must contain :plugins vector/list"
                       {:path path
                        :plugins plugins})))
     (mapv load-plugin! plugins))))
