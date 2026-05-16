#!/usr/bin/env python3
import json
import pathlib
import sqlite3
import sys
import time


SCHEMA = """
create table if not exists sessions (
  id integer primary key,
  created_at integer not null,
  model text not null,
  task text not null
);
create table if not exists artifacts (
  session_id integer not null,
  kind text not null,
  content text not null,
  created_at integer not null
);
create table if not exists model_attempts (
  id integer primary key,
  session_id integer not null,
  phase text not null,
  attempt_index integer not null,
  spec_text text not null,
  prompt_json text not null,
  raw_output text not null,
  parsed_json text,
  source_text text,
  check_json text,
  created_at integer not null
);
create table if not exists recall_notes (
  id integer primary key,
  session_id integer not null,
  phase text not null,
  label text not null,
  when_needed text not null,
  content text not null,
  created_at integer not null,
  unique (session_id, label, content)
);
create table if not exists execution_traces (
  id integer primary key,
  session_id integer not null,
  attempt_index integer,
  case_name text,
  tag text not null,
  data_json text not null,
  created_at integer not null
);
"""


def connect(path: str) -> sqlite3.Connection:
    db_path = pathlib.Path(path)
    db_path.parent.mkdir(parents=True, exist_ok=True)
    conn = sqlite3.connect(db_path)
    conn.row_factory = sqlite3.Row
    conn.executescript(SCHEMA)
    return conn


def init_session(payload: dict) -> dict:
    with connect(payload["db"]) as conn:
        session_id = conn.execute(
            "insert into sessions (created_at, model, task) values (?, ?, ?)",
            (int(time.time()), payload["model"], payload["task"]),
        ).lastrowid
        return {"session_id": session_id}


def save_artifact(payload: dict) -> dict:
    with connect(payload["db"]) as conn:
        conn.execute(
            "insert into artifacts values (?, ?, ?, ?)",
            (
                payload["session_id"],
                payload["kind"],
                payload.get("content") or "",
                int(time.time()),
            ),
        )
        return {"ok": True}


def save_attempt(payload: dict) -> dict:
    with connect(payload["db"]) as conn:
        conn.execute(
            """
            insert into model_attempts
            (session_id, phase, attempt_index, spec_text, prompt_json, raw_output,
             parsed_json, source_text, check_json, created_at)
            values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
            """,
            (
                payload["session_id"],
                payload["phase"],
                payload["attempt_index"],
                payload["spec_text"],
                json.dumps(payload.get("prompt") or [], indent=2),
                payload.get("raw_output") or "",
                json.dumps(payload["parsed"], indent=2) if payload.get("parsed") is not None else None,
                payload.get("source_text"),
                json.dumps(payload["check"], indent=2) if payload.get("check") is not None else None,
                int(time.time()),
            ),
        )
        return {"ok": True}


def save_trace(payload: dict) -> dict:
    with connect(payload["db"]) as conn:
        conn.execute(
            """
            insert into execution_traces
            (session_id, attempt_index, case_name, tag, data_json, created_at)
            values (?, ?, ?, ?, ?, ?)
            """,
            (
                payload["session_id"],
                payload.get("attempt_index"),
                payload.get("case_name"),
                payload["tag"],
                json.dumps(payload.get("data"), indent=2),
                int(time.time()),
            ),
        )
        return {"ok": True}


OPS = {
    "init-session": init_session,
    "save-artifact": save_artifact,
    "save-attempt": save_attempt,
    "save-trace": save_trace,
}


def main() -> int:
    if len(sys.argv) != 2 or sys.argv[1] not in OPS:
        print("usage: sqlite_memory.py <init-session|save-artifact|save-attempt|save-trace>", file=sys.stderr)
        return 2
    payload = json.load(sys.stdin)
    result = OPS[sys.argv[1]](payload)
    print(json.dumps(result))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
