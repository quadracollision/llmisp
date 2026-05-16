(ns harness.common)

(defn json-safe [value]
  (cond
    (nil? value) nil
    (string? value) value
    (number? value) value
    (true? value) true
    (false? value) false
    (keyword? value) (name value)
    (symbol? value) (str value)
    (map? value) (into {} (map (fn [[k v]] [(json-safe k) (json-safe v)]) value))
    (sequential? value) (mapv json-safe value)
    :else (pr-str value)))
