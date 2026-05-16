(ns harness.prompt
  (:require [harness.validation :as validation]))

(defn planner-prompt [task]
  (str
   "Read the spec and produce one strict JSON object only. No Markdown. No raw Clojure.\n"
   "This is a planning/oracle pass for a local coding agent. Do not invent sample rows or expected output rows.\n"
   "Infer only the contract that is directly stated by the text spec.\n\n"
   "Task:\n" task "\n\n"
   "Before emitting the JSON object, construct the plan internally as strict business invariants, not loose prose.\n"
   "For every derived column or business decision, build a truth table in reasoning_summary:\n"
   "1. List all raw fields involved in the calculation.\n"
   "2. Identify the explicit data type for each field, such as number, string, boolean, or nullable string.\n"
   "3. Enumerate every relevant state, including numeric boundaries such as > 10000 and <= 10000, string values, present/missing states, and fallback states.\n"
   "4. Verify mutually exclusive brackets. If one condition is a subset of another branch, state the strict priority order.\n"
   "5. Define the explicit fallback value when no business condition matches. Do not assume nil unless the spec explicitly requests nil.\n\n"
   "Return exactly this top-level JSON shape:\n"
   "{\"reasoning_summary\":\"truth-table summary...\",\"rules\":[\"...\"],\"sanitized_contract\":{...}}\n\n"
   "The sanitized_contract may contain only these keys:\n"
   "- function: string if stated or inferable from the spec\n"
   "- required_output_keys: array of output map keys requested by the spec\n"
   "- forbidden_output_keys: string rule for extra output keys\n"
   "- input_shapes: array describing input collection fields if stated by the spec\n"
   "- output_key_shapes: object mapping output keys to coarse shapes like {\"type\":\"string\"} or {\"type\":\"number\"}\n"
   "- order_hint: object like {\"key\":\"id\",\"direction\":\"asc\"} if sorting is stated\n"
   "- filter_required: boolean true only if the spec requires excluding rows from the input collection\n"
   "- filter_rules: array of plain text filter rules only if the spec states row inclusion/exclusion criteria\n"
   "- expected_examples: array of {\"name\":\"text-spec-shape\",\"expected_shape\":...} only, never expected data\n\n"
   "Forbidden in this planner response:\n"
   "- no expected output rows\n"
   "- no concrete input rows\n"
   "- no output_key_values\n"
   "- no literal enum lists unless they are classification labels explicitly named in the text spec\n"
   "- no solution AST\n\n"
   "Focus on business rules, filter conditions, sort condition, output keys, fallback rules, and derived classifications."))

(defn ast-prompt [task semantic-contract previous-ast error-text]
  (str
   "Emit one strict JSON object only. No Markdown. No LLmisp source. No raw Clojure. No prose.\n"
   "This JSON AST lowers directly to Clojure forms. It is a closed schema; do not invent keys or op names.\n\n"
   "Task:\n" task "\n\n"
   (validation/format-semantic-contract semantic-contract)
   (when previous-ast
     (str "Previous invalid JSON AST:\n" previous-ast "\n\n"))
   (when error-text
     (str "Validation error:\n" error-text "\n\n"))
   "Allowed top-level keys exactly: module, defs.\n"
   "Each def has exactly: name, args, expr. There are no statements and no return op.\n"
   "All names must be kebab-case, not snake_case. Field/key names are kebab-case strings.\n\n"
   "Top-level shape:\n"
   "{\"module\":\"demo.name\",\"defs\":[{\"name\":\"main\",\"args\":[\"items\"],\"expr\":{...}}]}\n\n"
   "Expression ops allowed exactly: string,int,float,bool,nil,var,field,call,map,vector,let,if,cond,query,=,not=,>,<,>=,<=,and,or,not,nil?,some?.\n"
   "Do not use return, body, uses, phrases, is, is-not, eq, greater, less, not-nil, is-nil, literal, fn, or raw Clojure.\n\n"
   "String literal: {\"op\":\"string\",\"value\":\"open\"}\n"
   "Variable: {\"op\":\"var\",\"name\":\"task\"}\n"
   "Field: {\"op\":\"field\",\"target\":{\"op\":\"var\",\"name\":\"task\"},\"field\":\"status\"}\n"
   "Call helper: {\"op\":\"call\",\"fn\":\"safe-owner\",\"args\":[{\"op\":\"var\",\"name\":\"task\"}]}\n"
   "Equality: {\"op\":\"=\",\"left\":FIELD_EXPR,\"right\":{\"op\":\"string\",\"value\":\"open\"}}\n"
   "Non-nil: {\"op\":\"some?\",\"expr\":FIELD_EXPR}. Nil: {\"op\":\"nil?\",\"expr\":FIELD_EXPR}.\n"
   "Do not use {\"op\":\"not\",\"expr\":FIELD_EXPR} to test non-nil. Use some? for present values and nil? for missing values.\n"
   "Greater than: {\"op\":\">\",\"left\":FIELD_EXPR,\"right\":OTHER_EXPR}. Less than uses op \"<\".\n"
   "Query expression: {\"op\":\"query\",\"rationale\":\"why this filter/sort/select matches the spec\",\"collection\":{\"op\":\"var\",\"name\":\"tasks\"},\"item\":\"task\",\"where\":[CONDITION],\"sort\":{\"expr\":FIELD_EXPR,\"direction\":\"asc\"},\"select\":MAP_EXPR}\n"
   "Map expression: {\"op\":\"map\",\"entries\":[{\"key\":\"id\",\"expr\":FIELD_EXPR}]}\n"
   "Cond expression: {\"op\":\"cond\",\"rationale\":\"why these branches cover the requested classification\",\"clauses\":[{\"condition\":CONDITION,\"expr\":EXPR}],\"else\":EXPR}\n"
   "Let expression: {\"op\":\"let\",\"bindings\":[{\"name\":\"x\",\"expr\":EXPR}],\"body\":EXPR}\n\n"
   "The function must be general; it must process the input collection dynamically using query, filter, and map.\n"
   "Every value in the output map must be a transformation of a field from the input item, a conditional result derived from input fields, or a calculated let binding.\n"
   "Static returns or hardcoded values matching test case names are prohibited. Hardcoded literal labels are allowed only when the text spec explicitly defines classification labels.\n"
   "Every query and cond should include a short rationale string explaining the business rule it implements.\n"
   "Every cond must cover the full data spectrum. Use else for the meaningful default label from the spec, such as standard, monitor, low-risk, or unassigned. Do not use nil as a cond else unless the spec explicitly requests nil.\n"
   "Projection rule: each requested output key must be its own map entry. Do not combine unrelated fields in one cond expression.\n"
   "If semantic feedback reports missing_keys, add exactly those map entries. If it reports extra_keys, remove exactly those unrequested map entries.\n"
   "Never add a map key named after the function. Never put the whole result collection inside each projected row.\n"
   "Rules: for missing fallback use condition nil? -> fallback, else original value. Use > and < for greater/less specs. Put sort only in query.sort, never as a map entry unless requested. For decisions, compare let-bound classifier variables, not raw input fields."))

(defn column-entry-prompt [task semantic-contract current-ast output-key]
  (str
   "Emit one strict compact JSON object only. No Markdown. No raw Clojure. No prose.\n"
   "Generate exactly one JSON AST map entry for one output column.\n\n"
   "Task:\n" task "\n\n"
   (validation/format-semantic-contract semantic-contract)
   "Current compact JSON AST context:\n"
   current-ast "\n\n"
   "Output key to generate: " output-key "\n\n"
   "Return exactly this shape:\n"
   "{\"translation_note\":\"I am now translating the rule for " output-key " from my plan into the AST. The dynamic field path I must access is ..., and the condition is ...\",\"key\":\"" output-key "\",\"expr\":EXPR}\n\n"
   "Rules:\n"
   "- translation_note is required. Use it to state the exact rule being translated, the dynamic field path or paths you must access, and the condition/fallback you are implementing.\n"
   "- The translation_note is not code; it is a compact reasoning checkpoint before the AST expression.\n"
   "- The returned key must be exactly \"" output-key "\".\n"
   "- Generate only this one output key; do not include other map entries.\n"
   "- Use compact JSON with no pretty printing and no repeated context.\n"
   "- Use the query item variable already used in the current AST.\n"
   "- Every expr must be derived from input fields, conditional logic over input fields, or a calculated let binding.\n"
   "- For pass-through columns, use a field expression.\n"
   "- For classification columns, use cond with a rationale and a meaningful else default from the spec.\n"
   "- Never use nil as cond else unless the spec explicitly asks for nil.\n"
   "- Do not mix business rules for other output keys into this key.\n"
   "- Do not return raw Clojure or a full module; return only the map entry object.\n\n"
   "Absolute compilation invariants for this isolated output key:\n"
   "1. Single-Look Rule: inside one cond expression, do not check the exact same field condition more than once. Every clause must represent a uniquely different logical branch.\n"
   "2. Positive and Negative Mapping: if you check absence with nil?, immediately account for presence with some? or use a non-nil fallback so the field path is fully resolved.\n"
   "3. Strict Scale Isolation: when evaluating numeric thresholds, arrange branches from highest constraint value to lowest constraint value so lower values are not short-circuited incorrectly.\n"
   "4. Total Matrix Coverage: the else expression must contain a valid classification label or fallback value from the spec. It must not be nil unless the spec explicitly says nil.\n"
   "5. Type Match Check: compare numeric fields to int/float literals, and string fields to string literals.\n"
   "6. Branch Isolation: do not repeat an earlier condition as a later fallback clause. Use else for the final fallback."))

(defn deconstruct-column-prompt [task semantic-contract output-key]
  (str
   "Emit one strict compact JSON array only. No Markdown. No raw Clojure. No AST.\n"
   "Deconstruct the business rule for exactly one output column into flat text rules.\n\n"
   "Task:\n" task "\n\n"
   (validation/format-semantic-contract semantic-contract)
   "Target output key: " output-key "\n\n"
   "Return exactly this shape:\n"
   "[{\"condition_text\":\"plain text condition from the spec\",\"output_value\":\"literal result\"},{\"condition_text\":\"otherwise\",\"output_value\":\"fallback literal\"}]\n\n"
   "Rules:\n"
   "- Return a flat JSON array. Do not nest arrays or objects inside condition_text.\n"
   "- Each item must have exactly condition_text and output_value.\n"
   "- output_value must be a scalar JSON string, number, boolean, or null. Never return an object or array as output_value.\n"
   "- Preserve the branch order from the spec, highest priority first.\n"
   "- Include the final otherwise/default/fallback rule as the last item.\n"
   "- condition_text must be plain English, not Clojure and not JSON AST.\n"
   "- output_value must be the exact literal label/value requested by the spec.\n"
   "- Do not include rules for any output key except " output-key "."))

(defn atomic-predicate-prompt [task semantic-contract input-fields item-name condition-text previous-output error-text]
  (str
   "Emit one strict compact JSON object only. No Markdown. No raw Clojure. No prose.\n"
   "Compile exactly one plain-text condition into exactly one JSON AST predicate node.\n\n"
   "Task:\n" task "\n\n"
   (validation/format-semantic-contract semantic-contract)
   "Input fields and types:\n"
   input-fields "\n\n"
   "Query item variable: " item-name "\n"
   "Condition text to compile: " condition-text "\n\n"
   (when previous-output
     (str "Previous invalid predicate output:\n" previous-output "\n\n"))
   (when error-text
     (str "Validation error for that one predicate:\n" error-text "\n\n"))
   "Allowed predicate ops exactly: =, not=, >, <, >=, <=, and, or, not, some?, nil?.\n"
   "Allowed value/field ops inside predicates: string, int, float, bool, nil, var, field.\n"
   "Field reference example: {\"op\":\"field\",\"target\":{\"op\":\"var\",\"name\":\"" item-name "\"},\"field\":\"amount\"}\n"
   "Numeric comparison example: {\"op\":\">\",\"left\":FIELD_EXPR,\"right\":{\"op\":\"int\",\"value\":10000}}\n"
   "String equality example: {\"op\":\"=\",\"left\":FIELD_EXPR,\"right\":{\"op\":\"string\",\"value\":\"overdue\"}}\n"
   "Boolean equality example: {\"op\":\"=\",\"left\":FIELD_EXPR,\"right\":{\"op\":\"bool\",\"value\":true}}\n\n"
   "Constraints:\n"
   "- Return only the predicate object. No top-level key, no cond, no map entry, no array.\n"
   "- Compile only Condition text to compile. Ignore unrelated filter rules, output rules, and other columns from the larger task.\n"
   "- If this condition is a query filter clause, emit only the one atomic inclusion/exclusion predicate for that clause.\n"
   "- For '<field> is present', emit {\"op\":\"some?\",\"expr\":FIELD_EXPR}. For '<field> is missing', emit {\"op\":\"nil?\",\"expr\":FIELD_EXPR}.\n"
   "- Compare numbers to int/float literals, strings to string literals, booleans to bool literals.\n"
   "- Use only fields listed in Input fields and types.\n"
   "- Do not use nil? or some? around static literals or around truth-value expressions.\n"
   "- Do not invent op names, helper functions, raw Clojure, comments, or output labels."))

(defn where-predicate-prompt [task semantic-contract input-fields query-name item-name]
  (str
   "Emit one strict compact JSON array only. No Markdown. No raw Clojure. No prose.\n"
   "Targeting the where clause for the " query-name " query.\n\n"
   "Task:\n" task "\n\n"
   (validation/format-semantic-contract semantic-contract)
   "Input fields and types:\n"
   input-fields "\n\n"
   "Query item variable: " item-name "\n\n"
   "Goal: emit a single JSON array of predicate expressions that filters the input collection according to the spec.\n"
   "Output format: [PREDICATE_EXPR, ...]. No top-level keys.\n\n"
   "You are compiling the filter array for a database query.\n"
   "Perform an Exhaustive Exclusion check before output:\n"
   "- Target specific, non-overlapping segments of the input collection.\n"
   "- If the spec requires a compound condition such as Condition A OR Condition B, emit a clear logical tree using an or operator block.\n"
   "- Do not combine independent filtering vectors into a single token. Every filtering criterion specified in the text must have its own dedicated predicate object inside the root array or inside the explicit and/or block that represents the compound condition.\n"
   "- Type Match Check: ensure numbers compare to numbers and strings compare to strings.\n\n"
   "Allowed predicate ops exactly: =, not=, >, <, >=, <=, and, or, not, some?, nil?.\n"
   "Allowed value/field ops inside predicates: string, int, float, bool, nil, var, field.\n"
   "Field reference example: {\"op\":\"field\",\"target\":{\"op\":\"var\",\"name\":\"" item-name "\"},\"field\":\"health-score\"}\n"
   "Number comparison example: {\"op\":\"<\",\"left\":FIELD_EXPR,\"right\":{\"op\":\"int\",\"value\":60}}\n"
   "String comparison example: {\"op\":\"=\",\"left\":FIELD_EXPR,\"right\":{\"op\":\"string\",\"value\":\"overdue\"}}\n"
   "OR example: {\"op\":\"or\",\"args\":[PREDICATE_EXPR,PREDICATE_EXPR]}\n\n"
   "Constraints:\n"
   "- Compare numbers to int/float literals, never string literals.\n"
   "- Compare strings to string literals.\n"
   "- Use only fields listed in Input fields and types.\n"
   "- Do not invent op names, keys, raw Clojure, comments, functions, or map entries.\n"
   "- If the spec does not request filtering, return an empty array []."))
