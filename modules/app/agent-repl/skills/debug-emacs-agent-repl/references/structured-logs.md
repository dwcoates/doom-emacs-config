# Structured logs

Use this playbook for historical ordering, branch outcomes, errors, and
cross-runtime correlation. The normative persistence and schema contract is
`../../../logging-contract.md`.

## Canonical persistence

Workspace-owned records use canonical symlinks:

- `<workspace>/.claude/emacs/emacs.log`.
- `<workspace>/.claude/emacs/daemon.log`.
- `<workspace>/.claude/emacs/shim.log`.
- `<workspace>/.claude/emacs/webapp.log`.
- `<workspace>/.claude/emacs/sidecar.log`.

Each line is one JSON object. Workspace paths point to runtime-owned temporary
targets. Always enter through the canonical workspace path or the resolver.
Do not search arbitrary temporary files.

Global logs exist only for records that are conceptually unrelated to every
workspace and agent. Difficulty resolving a known workspace is an invariant
violation, not permission to use a global sink.

## Resolve before reading

List canonical paths:

```sh
modules/app/agent-repl/scripts/agent-repl-log-discovery.sh \
  --workspace /absolute/workspace
```

Select one runtime:

```sh
modules/app/agent-repl/scripts/agent-repl-log-discovery.sh \
  --workspace /absolute/workspace \
  --runtime daemon
```

List genuine global logs:

```sh
modules/app/agent-repl/scripts/agent-repl-log-discovery.sh --global
```

The resolver derives the default global Emacs sink from `TMPDIR`. If
`agent-repl-log-file-name` is customized, resolve its live value through
`/runtime-eval-code` and pass that exact path through
`AGENT_REPL_EMACS_GLOBAL_LOG`.

Filter strict JSONL:

```sh
modules/app/agent-repl/scripts/agent-repl-log-discovery.sh \
  --workspace /absolute/workspace \
  --session "$SESSION_ID" \
  --tail 2000
```

Use `--pid` to isolate one process. Combine `--runtime` with `--session` or
`--pid` to narrow before parsing. The resolver fails loudly on malformed
selected JSONL.

## Record shape

Every persisted record carries:

- `timestamp`.
- `runtime`.
- `level`.
- `verbosity`.
- `operation`.
- `message`.
- Structured `context`.

Process runtimes carry `pid`. Webapp records carry `connection_id`.
Workspace records carry `workspace_dir` and `workspace_id`. Known identities
belong in `agent_repl_session_id`, `claude_session_id`, and `request_id`.

Inspect fields directly:

```sh
modules/app/agent-repl/scripts/agent-repl-log-discovery.sh \
  --workspace /absolute/workspace \
  --session "$SESSION_ID" \
  --tail 2000 |
jq -c '{timestamp,runtime,level,verbosity,operation,message,workspace_id,agent_repl_session_id,claude_session_id,pid,connection_id,request_id,context}'
```

## Runtime ownership

- Emacs persists workspace records to `emacs.log`.
- The daemon persists daemon records to `daemon.log`.
- The shim writes directly to `shim.log` through its inherited descriptor.
- The daemon persists forwarded browser records to `webapp.log`.
- The daemon persists session-bound sidecar diagnostics to `sidecar.log`.
- Store lifecycle, database, protocol, and sink failures are genuinely global.

Do not look for shim or webapp records solely inside `daemon.log`. Terminal
mirrors are not the durable workspace record.

## Emacs evidence

Workspace-associated Emacs calls produce JSONL in the workspace's
`emacs.log`. Calls with no conceptual workspace use the configured global
Emacs sink under the OS temporary directory.

### Emacs log volume: two independent knobs

These are routinely confused, and the confusion runs in both directions —
turning the wrong one off does not shrink the log, and a missing record gets
blamed on the wrong setting.

| Variable | Governs | Does NOT govern |
|---|---|---|
| `agent-repl-debug` | `*Messages*` / terminal visibility. `nil`, `t`, `verbose`. | Anything written to disk. |
| `agent-repl-log-file-level` | The durable sink: file and workspace log buffer. `verbose` < `debug` < `info` < `warn` < `error`. Default `debug`. | Echo-area or `*Messages*` visibility. |
| `agent-repl-log-to-file` | All-or-nothing kill-switch for file writes. | Severity; it discards warnings and errors too. |

Consequences for evidence:

- Absence of a **verbose** record is expected at the default. `agent-repl-log-file-level`
  is `debug`, which drops the `agent-repl--log-verbose` rung from disk. Raise it
  to `verbose` before concluding a hot path is not executing.
- Absence of a record is **never** explained by `agent-repl-debug`. That knob has
  never gated persistence.
- A record ranks by its `verbosity` field when that field is `verbose`, regardless
  of the `level` it carries — verbose records are stamped `debug`, so ranking by
  `level` alone would misread them.

To change the durable level at runtime, with effect on the very next record and
no restart or reload:

```
M-x agent-repl-debug/set-log-file-level      ;; or SPC <agent-repl prefix> L
```

Raise it to `verbose` to capture a reproduction, then lower it again. Leaving it
at `verbose` is expensive: an ordinary working day produced ~350k verbose records
and over 140 MB, against ~14k records at every other rung combined.

`M-x agent-repl-debug/toggle-logging` (`D`) is the *visibility* toggle and will
not reduce a log file.

`<workspace>/.claude/emacs/memory-state.el` is a point-in-time snapshot of
Emacs-owned workspace data. Use it for buffer, process, timer, prompt, and
lifecycle fields. Do not use it to derive rendered workspace color. The SSM
database owns resolved state.

Use `/runtime-eval-code` when:

- The needed value exists only in live Emacs memory.
- A third-party package wrote only to `*Messages*`.
- The loaded definition must be compared with source.

Use neither `*Messages*` nor a live side buffer as a substitute for missing
agent-repl durable logging.

## Reading chronology

For a cross-runtime timeline:

1. Resolve the identity spine.
2. Select a bounded time window or tail.
3. Sort or compare RFC 3339 `timestamp` values.
4. Follow dedicated identity fields.
5. Compare stable `operation` names and structured context.
6. Look for an expected start and completion around every boundary call.
7. Treat an unmatched start as a possible hang or lost completion signal.

Distinguish:

- An explicit error record.
- An explicit branch or no-op record.
- A missing record.
- A filtered-out record.
- A record written by stale deployed code.

Only the first two are affirmative evidence about the path's outcome.

## Log safety

- Never mutate or truncate logs during diagnosis.
- Never follow an untrusted workspace-provided sink target manually.
- Never substitute message-text parsing for available structured fields.
- Never interpret silence before checking readiness, identity, time bounds,
  selected runtime, and the observability audit.
