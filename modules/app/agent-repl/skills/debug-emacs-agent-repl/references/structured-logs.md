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

### Emacs log verbosity: three sinks, three knobs

The three are routinely confused, and the confusion runs both ways — turning the
wrong one off does not shrink the log, and a missing record gets blamed on the
wrong setting. Each sink is gated independently ON PURPOSE, so a quiet buffer
never costs a complete file.

| Sink | Variable | Default | What it is for |
|---|---|---|---|
| `*Messages*` / terminal | `agent-repl-debug` | `nil` | Visibility only. `nil`, `t`, `verbose`. Has NEVER gated persistence. |
| Per-workspace log buffers | `agent-repl-log-buffer-level` | `warn` | Read live by a human. Strictest of the three: these show what went WRONG. |
| Log file (durable) | `agent-repl-log-file-level` | `debug` | Forensic record. Keeps every ordinary line, drops the verbose rung. |
| Log file, master switch | `agent-repl-log-to-file` | `t` | All-or-nothing. Discards warnings and errors too — prefer the level. |

Levels, least to most severe: `verbose` < `debug` < `info` < `warn` < `error`.
A record is written when its own rung is at or above the sink's threshold.

### Commands for changing them at runtime

All take effect on the very next record — no restart, no reload.

| Command | Key | Effect |
|---|---|---|
| `agent-repl-debug/toggle-verbose-to-disk` | `V` | Flips `agent-repl-log-file-level` between `debug` and `verbose`. THE ONE TO USE when you need hot-path chatter on disk for a reproduction. |
| `agent-repl-debug/set-log-file-level` | `L` | Sets `agent-repl-log-file-level` to any named rung. |
| `agent-repl-debug/toggle-logging` | `D` | Flips `agent-repl-debug` (`*Messages*` visibility). Will NOT shrink a log file. |
| `agent-repl-debug/toggle-log-to-file` | — | Flips the all-or-nothing file kill-switch. |

The per-workspace buffer threshold has no dedicated command; set
`agent-repl-log-buffer-level` directly (`setq`, or via `/runtime-eval-code`)
when a workspace buffer needs to show ordinary or verbose activity.

### Debugging workflow

1. Before provoking a reproduction that needs hot-path evidence, run
   `agent-repl-debug/toggle-verbose-to-disk` to turn verbose ON.
2. Provoke it, then read the file.
3. Turn verbose back OFF with the same command. A working day left on runs to
   ~350k verbose records and well over a hundred megabytes.

### Consequences for evidence

- Absence of a **verbose** record on disk is EXPECTED at the default. Turn
  verbose on before concluding a hot path is not executing.
- Absence of a **debug** record in a workspace log BUFFER is expected at the
  default (`warn`). The file still has it. Check the file before lowering the
  buffer threshold.
- Absence of a record is **never** explained by `agent-repl-debug`. That knob has
  never gated persistence, in either sink.
- A record ranks by its `verbosity` field when that field is `verbose`,
  regardless of the `level` it carries — verbose records are stamped `debug`, so
  ranking by `level` alone misreads them.
- An unrecognized level ranks at the TOP, so a threshold is never what silently
  loses a record.

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

## Shim entrypoint fatal query

`shim.main.fatal` is an operation-only query boundary. It contains only
unrecoverable shim entrypoint termination records at `error` level. Each
record carries the classified cause and exit outcome in `context`.

Do not query shim entrypoint failures by matching a `message` prefix such as
`fatal:`. Normal startup, lock, signal, and shutdown records use
`shim.main.lifecycle` instead.
