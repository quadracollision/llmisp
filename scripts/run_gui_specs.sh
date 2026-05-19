#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
GGUF="${GGUF:-/home/jacob/src/dsl/gemma.gguf}"
LLAMA_SERVER_BIN="${LLAMA_SERVER_BIN:-/home/jacob/src/StrawberryCode5/llama.cpp/build/bin/llama-server}"
PORT_START="${PORT_START:-19200}"
OUT_DIR="${OUT_DIR:-tmp/gui_specs}"
LIMIT="${LIMIT:-0}"
LLAMA_CTX_SIZE="${LLAMA_CTX_SIZE:-4096}"
LLAMA_GPU_LAYERS="${LLAMA_GPU_LAYERS:-24}"
PLANNER_MAX_TOKENS="${PLANNER_MAX_TOKENS:-2048}"
COMPONENT_MAX_TOKENS="${COMPONENT_MAX_TOKENS:-2048}"
REPAIR_ATTEMPTS="${REPAIR_ATTEMPTS:-2}"

mkdir -p "$ROOT/$OUT_DIR"

summary="$ROOT/$OUT_DIR/summary.jsonl"
: > "$summary"

idx=0
for spec in "$ROOT"/specs/gui/*_spec.txt; do
  idx=$((idx + 1))
  if [[ "$LIMIT" != "0" && "$idx" -gt "$LIMIT" ]]; then
    break
  fi

  name="$(basename "$spec" .txt)"
  project="$OUT_DIR/$name"
  port=$((PORT_START + idx))

  echo "==> $name"
  status=0
  (
    cd "$ROOT"
    bb gui-run \
      --self-plan \
      --component-pass \
      --gguf "$GGUF" \
      --llama-server-bin "$LLAMA_SERVER_BIN" \
      --llama-port "$port" \
      --llama-ctx-size "$LLAMA_CTX_SIZE" \
      --llama-gpu-layers "$LLAMA_GPU_LAYERS" \
      --task-file "$spec" \
      --project-dir "$project" \
      --planner-max-tokens "$PLANNER_MAX_TOKENS" \
      --column-max-tokens "$COMPONENT_MAX_TOKENS" \
      --repair-attempts "$REPAIR_ATTEMPTS"
  ) || status=$?

  python3 - "$ROOT/$project/report.json" "$name" "$status" >> "$summary" <<'PY'
import json, os, sys
report_path, name, status = sys.argv[1], sys.argv[2], int(sys.argv[3])
if os.path.exists(report_path):
    with open(report_path) as f:
        report = json.load(f)
else:
    report = {"ok": False, "check": {"stderr": "report.json missing"}}
check = report.get("check", {})
print(json.dumps({
    "name": name,
    "process_status": status,
    "ok": report.get("ok"),
    "self_plan": report.get("self_plan"),
    "component_pass": report.get("component_pass"),
    "stdout": check.get("stdout", ""),
    "stderr": check.get("stderr", ""),
    "project_dir": report.get("project_dir")
}))
PY
done

echo "summary: $summary"
