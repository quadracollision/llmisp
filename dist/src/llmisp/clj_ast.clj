(ns llmisp.clj-ast
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [clojure.string :as str]
            [llmisp.diagnostics :as diag]))

(defn- ast-error [message data]
  (diag/raise! :clj-ast :invalid-clj-ast message data))

(defn- kget [m k]
  (or (get m k) (get m (name k))))

(defn- required [m k ctx]
  (if (contains? m k)
    (get m k)
    (if (contains? m (name k))
      (get m (name k))
      (ast-error (str "missing required key " (name k)) {:context ctx :key (name k) :object m}))))

(defn- expect-map [x ctx]
  (if (map? x) x (ast-error "expected object" {:context ctx :value x})))

(defn- expect-vector [x ctx]
  (if (vector? x) x (ast-error "expected array" {:context ctx :value x})))

(defn- expect-string [x ctx]
  (if (string? x) x (ast-error "expected string" {:context ctx :value x})))

(defn- valid-name? [value]
  (and (string? value)
       (boolean (re-matches #"[a-z][a-z0-9-]*" value))))

(defn- valid-module-name? [value]
  (and (string? value)
       (boolean (re-matches #"[a-z][a-z0-9-]*(\.[a-z][a-z0-9-]*)*" value))))

(defn- name-symbol [value ctx]
  (let [value (expect-string value ctx)]
    (when-not (valid-name? value)
      (ast-error "expected kebab-case name" {:context ctx :value value}))
    (symbol value)))

(defn- namespace-symbol [value ctx]
  (let [value (expect-string value ctx)]
    (when-not (valid-module-name? value)
      (ast-error "expected dotted kebab-case module name" {:context ctx :value value}))
    (symbol value)))

(declare expr->form)

(def ^:dynamic *trace-fn* nil)

(defn- trace-step [pipeline tag]
  (if *trace-fn*
    (conj pipeline `(~*trace-fn* ~tag))
    pipeline))

(defn- field-form [x ctx]
  (let [target (expr->form (required x :target ctx) (str ctx ".target"))
        field (expect-string (required x :field ctx) (str ctx ".field"))]
    `(~(keyword field) ~target)))

(defn- map-form [x ctx]
  (let [entries (expect-vector (required x :entries ctx) (str ctx ".entries"))]
    (into {}
          (map (fn [entry]
                 (let [entry (expect-map entry (str ctx ".entries[]"))
                       key-name (expect-string (required entry :key ctx) (str ctx ".entries[].key"))]
                   [(keyword key-name)
                    (expr->form (required entry :expr ctx) (str ctx ".entries[].expr"))])))
          entries)))

(defn- call-form [x ctx]
  (let [fn-name (name-symbol (required x :fn ctx) (str ctx ".fn"))
        args (mapv #(expr->form % (str ctx ".args[]"))
                   (expect-vector (kget x :args) (str ctx ".args")))]
    (cons fn-name args)))

(defn- cond-form [x ctx]
  (let [clauses (expect-vector (required x :clauses ctx) (str ctx ".clauses"))
        pairs (mapcat (fn [clause]
                        (let [clause (expect-map clause (str ctx ".clauses[]"))]
                          [(expr->form (required clause :condition ctx) (str ctx ".clauses[].condition"))
                           (expr->form (required clause :expr ctx) (str ctx ".clauses[].expr"))]))
                      clauses)]
    (apply list 'cond (concat pairs [:else (expr->form (required x :else ctx) (str ctx ".else"))]))))

(defn- let-form [x ctx]
  (let [bindings (expect-vector (required x :bindings ctx) (str ctx ".bindings"))
        body (expr->form (required x :body ctx) (str ctx ".body"))
        binding-forms (mapcat (fn [binding]
                                (let [binding (expect-map binding (str ctx ".bindings[]"))]
                                  [(name-symbol (required binding :name ctx) (str ctx ".bindings[].name"))
                                   (expr->form (required binding :expr ctx) (str ctx ".bindings[].expr"))]))
                              bindings)]
    `(let [~@binding-forms] ~body)))

(defn- query-form [x ctx]
  (let [collection (expr->form (required x :collection ctx) (str ctx ".collection"))
        item (name-symbol (required x :item ctx) (str ctx ".item"))
        where (expect-vector (or (kget x :where) []) (str ctx ".where"))
        select (expr->form (required x :select ctx) (str ctx ".select"))
        sort (kget x :sort)
        base (trace-step ['->> collection] "1-initial-data")
        with-filter (if (seq where)
                      (trace-step
                       (conj base `(filter (fn [~item] ~(if (= 1 (count where))
                                                          (expr->form (first where) (str ctx ".where[]"))
                                                          (cons 'and (map #(expr->form % (str ctx ".where[]")) where))))))
                       "2-after-filter")
                      base)
        with-sort (if sort
                    (let [sort (expect-map sort (str ctx ".sort"))
                          sort-expr (expr->form (required sort :expr ctx) (str ctx ".sort.expr"))
                          direction (or (kget sort :direction) "asc")
                          sorted (conj with-filter `(sort-by (fn [~item] ~sort-expr)))]
                      (trace-step
                       (if (= direction "desc")
                         (conj sorted 'reverse)
                         sorted)
                       "3-after-sort"))
                    with-filter)]
    (apply list (trace-step (conj with-sort `(mapv (fn [~item] ~select))) "4-final-projection"))))

(defn expr->form [x ctx]
  (let [x (expect-map x ctx)
        op (required x :op ctx)]
    (case op
      "string" (expect-string (required x :value ctx) (str ctx ".value"))
      "int" (long (required x :value ctx))
      "float" (double (required x :value ctx))
      "bool" (boolean (required x :value ctx))
      "nil" nil
      "var" (name-symbol (required x :name ctx) (str ctx ".name"))
      "field" (field-form x ctx)
      "map" (map-form x ctx)
      "vector" (vec (map-indexed (fn [idx item] (expr->form item (str ctx ".items[" idx "]")))
                                 (expect-vector (required x :items ctx) (str ctx ".items"))))
      "call" (call-form x ctx)
      "let" (let-form x ctx)
      "if" `(if ~(expr->form (required x :condition ctx) (str ctx ".condition"))
              ~(expr->form (required x :then ctx) (str ctx ".then"))
              ~(expr->form (required x :else ctx) (str ctx ".else")))
      "cond" (cond-form x ctx)
      "query" (query-form x ctx)
      "=" `(= ~(expr->form (required x :left ctx) (str ctx ".left"))
              ~(expr->form (required x :right ctx) (str ctx ".right")))
      "not=" `(not= ~(expr->form (required x :left ctx) (str ctx ".left"))
                    ~(expr->form (required x :right ctx) (str ctx ".right")))
      ">" `(> ~(expr->form (required x :left ctx) (str ctx ".left"))
              ~(expr->form (required x :right ctx) (str ctx ".right")))
      "<" `(< ~(expr->form (required x :left ctx) (str ctx ".left"))
              ~(expr->form (required x :right ctx) (str ctx ".right")))
      ">=" `(>= ~(expr->form (required x :left ctx) (str ctx ".left"))
                ~(expr->form (required x :right ctx) (str ctx ".right")))
      "<=" `(<= ~(expr->form (required x :left ctx) (str ctx ".left"))
                ~(expr->form (required x :right ctx) (str ctx ".right")))
      "and" (cons 'and (mapv #(expr->form % (str ctx ".args[]"))
                             (expect-vector (required x :args ctx) (str ctx ".args"))))
      "or" (cons 'or (mapv #(expr->form % (str ctx ".args[]"))
                           (expect-vector (required x :args ctx) (str ctx ".args"))))
      "not" `(not ~(expr->form (required x :expr ctx) (str ctx ".expr")))
      "nil?" `(nil? ~(expr->form (required x :expr ctx) (str ctx ".expr")))
      "some?" `(some? ~(expr->form (required x :expr ctx) (str ctx ".expr")))
      (ast-error (str "unknown expression op " op) {:context ctx :op op}))))

(defn- def-form [x ctx]
  (let [x (expect-map x ctx)
        name (name-symbol (required x :name ctx) (str ctx ".name"))
        args (mapv #(name-symbol % (str ctx ".args[]"))
                   (expect-vector (required x :args ctx) (str ctx ".args")))
        expr (expr->form (required x :expr ctx) (str ctx ".expr"))]
    `(defn ~name ~args ~expr)))

(defn forms
  ([ast]
   (forms ast {}))
  ([ast {:keys [trace-fn]}]
   (let [ast (expect-map ast "$")
         module (namespace-symbol (required ast :module "$.module") "$.module")
         defs (expect-vector (required ast :defs "$.defs") "$.defs")]
     (binding [*trace-fn* trace-fn]
       (vec (concat
             [(list 'ns module '(:require [clojure.string :as string]))]
             (mapv #(def-form % "$.defs[]") defs)))))))

(defn emit [ast]
  (->> (forms ast)
       (map pr-str)
       (str/join "\n")))

(defn write-forms! [output-path forms]
  (when-let [parent (.getParentFile (io/file output-path))]
    (.mkdirs parent))
  (with-open [w (io/writer output-path)]
    (binding [*out* w]
      (doseq [form forms]
        (pp/pprint form)
        (println)))))

(defn parse-json [json-text]
  (json/parse-string json-text true))

(defn emit-json [json-text]
  (emit (parse-json json-text)))

(defn compile-json-file [json-path output-path {:keys [overwrite?] :or {overwrite? false}}]
  (when (and (.exists (io/file output-path)) (not overwrite?))
    (ast-error "output file already exists" {:path output-path}))
  (when-let [parent (.getParentFile (io/file output-path))]
    (.mkdirs parent))
  (write-forms! output-path (forms (parse-json (slurp json-path))))
  output-path)
