# Logging — the contract

The owner's ruling, 2026-09-10. Every agent-repl system logs the same way, to
the same place, and one command reads it back per workspace. This document is
the contract; each system's AGENTS.md states how it meets it.

## One function per codebase

Every log site in a codebase calls ONE logging function (or one object with
one method per level). Nothing else writes a record: no `fmt.Println`, no
`log.Printf`, no `console.*`, no `message`/`princ` to stderr, no `error`
signalled as a substitute for a record. A lint or test per codebase fails on a
bypass. The function is:

| system | function |
|---|---|
| daemon (Go) | `dlog.Logger` (`daemon/internal/dlog`) |
| shim-store (Go) | `agentrepl/logging` (`agent-shim/logging/go`) |
| shim-claude-sidecar (Go) | `agentrepl/logging` (`agent-shim/logging/go`) |
| shim (TypeScript) | `agentrepl/logging` (`agent-shim/logging/ts`) |
| webapp (TypeScript) | the page logger, which forwards through the daemon's `ClientLog` rpc |
| Emacs (lisp) | `agent-repl--log` with a level argument (warn/error variants call it) |

## What every record guarantees

One JSON object per line, with exactly these fields; the daemon's `dlog`
record (`daemon/internal/dlog/record.go`) is the reference schema and every
other system emits the same field names:

| field | meaning |
|---|---|
| `timestamp` | RFC 3339 with offset and sub-second precision, the writer's wall clock |
| `runtime` | `daemon`, `shim`, `store`, `sidecar`, `webapp`, `emacs` |
| `level` | `debug`, `info`, `warn`, `error` |
| `verbosity` | `normal` or `verbose` (the record's own class; see below) |
| `operation` | a stable dotted name, `system.component.event`, never prose |
| `message` | one sentence for a human |
| `context` | an object of typed fields specific to the event |
| `pid` | the writer's pid |
| `workspace_id` | the workspace this record belongs to, when it belongs to one |
| `workspace_dir` | its directory, when known |
| `agent_repl_session_id`, `claude_session_id`, `request_id` | when the event has them |

A record with no `workspace_id` is a CENTRAL record. A record about a
workspace without `workspace_id` is a defect: the logger takes the workspace
from its scope (a per-workspace child logger), so a site cannot forget it.

Emacs `*Messages*` is not a log; the module's records go through
`agent-repl--log` into the module log file in this format, and `*Messages*`
only ever shows what the user should read.

## Levels and coverage

- `error`: something failed and the user or a later reader must know; never
  swallowed, always carries the error text.
- `warn`: something is wrong but the system continued; every warn is a defect
  or a decision to be justified by name.
- `info`: the lifecycle edges a reader needs to follow a run (bring-up, link
  up, turn start/end, restart, adoption, shutdown), one record per edge.
- `debug`: everything else worth having when something is wrong: request
  boundaries, state transitions, decisions taken and why. Volume is fine at
  debug; it is off by default and switched on per system without a rebuild.

`verbosity: verbose` marks records a reader wants only when tracing; `normal`
records are the run's story. Every system honors the same switch:
`AGENT_REPL_LOG_LEVEL` (`debug|info|warn|error`, default `info`).

## Where records go

One root, `~/.claude-emacs/logs/`:

```
~/.claude-emacs/logs/
  central/<runtime>.log            records with no workspace_id, one file per runtime
  workspaces/<workspace_id>/
    <runtime>.log                  that workspace's records, one file per runtime
    meta.json                      workspace_dir, first/last seen, the ids it has held
```

Every file rotates at a size cap with N generations (`agentrepl/logging`'s
`OpenRotating`, already the daemon's and store's rule). A process that writes
for several workspaces (the daemon, the store, the sidecar) opens the
workspace's file on first use and keeps it open; the writer, not a reader,
does the fan-out, so retrieval never parses a central file to find a
workspace. The launchd services' stdout/stderr paths receive only bootstrap
errors (`NewDurableOnly`).

The Emacs side writes `emacs.log` under the same root: `agent-repl--log`
resolves the current workspace from the buffer or explicit argument and
writes to that workspace's directory; central otherwise.

## Reading it back

`bin/logs.sh` is the one reader:

```
bin/logs.sh --workspace <id|dir|name> [--since <ts|duration>] [--level warn]
            [--runtime daemon,shim] [--follow] [--json]
bin/logs.sh --central [...]
bin/logs.sh --all [...]            # every workspace and central, merged
bin/logs.sh --harvest <from> <to>  # every warn/error in the window, attributed, as a table
```

It merges the chosen files by `timestamp`, filters by level and runtime,
and prints one line per record (`time level runtime operation message` plus
context) or raw JSON. `--harvest` is what a realtest uses as its remediation
bar. The name or directory forms resolve through `workspaces/*/meta.json`.

## Codified

Each system's AGENTS.md carries: the function to call, the record schema
reference (this file), where its records land, its level switch, and the lint
that fails a bypass. The module's AGENTS.md carries the root layout and
`bin/logs.sh`. A change to this contract is a change to this file first.
