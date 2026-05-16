#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
GGUF="${GGUF:-/home/jacob/src/dsl/gemma.gguf}"
LLAMA_SERVER_BIN="${LLAMA_SERVER_BIN:-/home/jacob/src/StrawberryCode5/llama.cpp/build/bin/llama-server}"
PORT_START="${PORT_START:-18200}"
OUT_DIR="${OUT_DIR:-tmp/blind_specs}"
LIMIT="${LIMIT:-0}"
LLAMA_CTX_SIZE="${LLAMA_CTX_SIZE:-16384}"
LLAMA_GPU_LAYERS="${LLAMA_GPU_LAYERS:-999}"

mkdir -p "$ROOT/$OUT_DIR"

summary="$ROOT/$OUT_DIR/summary.jsonl"
: > "$summary"

idx=0
for spec in "$ROOT"/specs/blind/*_spec.txt; do
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
    bb json-run \
      --self-plan \
      --skeleton-first \
      --where-pass \
      --column-pass \
      --gguf "$GGUF" \
      --llama-server-bin "$LLAMA_SERVER_BIN" \
      --llama-port "$port" \
      --llama-ctx-size "$LLAMA_CTX_SIZE" \
      --llama-gpu-layers "$LLAMA_GPU_LAYERS" \
      --task-file "$spec" \
      --project-dir "$project" \
      --repair-attempts 0 \
      --max-tokens 8192 \
      --where-max-tokens 2048 \
      --column-max-tokens 3072
  ) || status=$?

  python3 - "$ROOT/$project/report.json" "$name" >> "$summary" <<'PY'
import json, sys
report_path, name = sys.argv[1], sys.argv[2]
with open(report_path) as f:
    report = json.load(f)
check = report.get("check", {})
print(json.dumps({
    "name": name,
    "ok": report.get("ok"),
    "semantic_oracle": report.get("semantic_oracle", {}).get("source"),
    "clean_benchmark": report.get("semantic_oracle", {}).get("clean_benchmark?"),
    "stdout": check.get("stdout", ""),
    "stderr": check.get("stderr", ""),
    "refinement_failed": check.get("refinement_failed"),
    "project_dir": report.get("project_dir")
}))
PY
done

echo "summary: $summary"
