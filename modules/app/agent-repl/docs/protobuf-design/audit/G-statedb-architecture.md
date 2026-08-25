# G — statedb architecture map (2026-08-25)

- `daemon/internal/statedb` is an IN-PROCESS LIBRARY, not a service: one
  SQLite file (`~/.cache/agent-repl/ssm/state.db`), opened once by main
  (main.go:462), one connection, one writer, WAL (statedb.go:60-75); the
  handle is shared into ssm, registry, workspace/geometry and the statedb
  store types. No socket serves it; the only external consumer is the
  token-utilization-audit CLI opening the file directly.
- Tables: statedb owns prompt_receipt, terminal_failure_card,
  keep_alive_window, token_utilization(+quarantine), turn_accounting,
  shutdown_schedule(+hold_prompt); co-tenants own workspace_state,
  conversation_reader_position, turn_lifecycle_claim, turn_interruption,
  session_connectivity, session_fault, merge_lease, workspace_merged,
  compaction_gate (ssm/db.go), session_record/conversation_checkpoint
  (registry), workspace_merge_geometry (geometry).
- durable.proto's ONLY stored uses: state.v1.TokenUtilization (one blob row
  per API response, tokenutilization.go:97) and state.v1.TurnAccounting
  (turnaccounting.go:56), written by sessioncontroller (sinks.go:1497,
  terminalsettlement.go:215), replayed by durablereplay.go:227. Everything
  else in statedb is plain columns, no proto.
- The single-file design exists so cursor + replay floor + identity move in
  ONE transaction; a service split would cross that boundary (ssm/registry/
  geometry share raw *sql.DB and multi-table transactions).
