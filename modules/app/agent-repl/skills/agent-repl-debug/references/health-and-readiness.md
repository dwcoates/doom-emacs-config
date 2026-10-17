# Health and readiness

Use this playbook to distinguish four different questions:

1. Is the service registered or running?
2. Can the expected socket accept a connection?
3. Is the deployed artifact current with source?
4. Is the running process serving the deployed artifact?

A positive answer to one question does not answer the others.

## Runtime health sweep

Run from the repository root:

```sh
modules/app/agent-repl/scripts/agent-shim-doctor.sh
```

Use JSON when the result needs programmatic inspection:

```sh
modules/app/agent-repl/scripts/agent-shim-doctor.sh --json
```

The doctor is read-only. It checks:

- Store socket presence as a filesystem fact.
- Correlated store protocol health through `HealthCheck` and the matching
  healthy `HealthStatus`, including request ID, latency, component, reason,
  and exact failure class.
- Store and sidecar launchd state.
- Daemon frontend socket presence.
- Per-session shim sockets.
- Store and sidecar service-log presence.
- Store database presence.
- Store database read-only openability.
- Store database integrity through `PRAGMA integrity_check` when the database
  is at most 1 GiB.

For a larger database the routine sweep reports `store-db-integrity` as
`SKIP`, including the measured size and automatic-scan limit, rather than
blocking indefinitely on a deep page scan. Run the explicit maintenance-window
probe when a full scan is required:

```sh
modules/app/agent-repl/scripts/agent-shim-doctor.sh --deep-integrity
```

Interpret each result:

- `PASS` means the named invariant passed.
- `FAIL` means the named invariant failed.
- `SKIP` means an optional dependency prevented evaluation. It is not a pass.
- Text mode exits nonzero when any check fails.
- JSON mode carries the same per-check status.

The doctor does not prove that the running binary matches current source. Run
the readiness report separately.

## Deploy and running readiness

Run:

```sh
modules/app/agent-repl/bin/readiness-report.sh
```

The report covers `daemon`, `shim`, `webapp`, `shim-store`, and
`shim-claude-sidecar`. For each system inspect:

- `deployed_sha`: revision recorded by the artifact's build stamp.
- `deployed_dirty`: whether the artifact was built from a dirty tree.
- `source_sha`: newest commit affecting that system.
- `commits_behind` and `minutes_behind`.
- `running.pid` and `running.stale_binary` when the system has one long-lived
  process.
- `ready`.
- `error`.

Exit `0` means a JSON report was produced. It does not mean every system is
ready. Inspect every system's `ready` and `error` fields.

Elisp is deliberately absent. Loading Elisp is runtime state that only the
live Emacs process can report honestly. Use `/runtime-eval-code` when the
investigation requires proof of which definition is loaded.

## Reading the combined baseline

Classify findings without collapsing them:

- Doctor failure plus readiness success means the expected artifact is current
  but the runtime is unhealthy.
- Doctor success plus readiness failure means a healthy process may be serving
  stale code.
- Missing build stamps mean readiness is unknown. Do not infer freshness from
  mtimes.
- A present launchd service without a connectable socket is not healthy.
- An old service log may mean idleness. Correlate it with socket and process
  evidence before deciding.

## Restart safety

Do not restart `shim-store` or `shim-claude-sidecar` without explicit user
permission. They carry the file plane for every live session.

When the user authorizes restarting both:

1. Restart the store.
2. Wait until `~/.cache/agent-repl/sock/store.sock` exists.
3. Restart the sidecar.
4. Re-run the doctor.
5. Confirm the sidecar reports an established store link.

Never restart both concurrently. Cursor recovery is connection-scoped and the
sidecar must not begin against an unavailable store.

The daemon follows the bounce policy in `../../../AGENTS.md`. Do not bounce it in
the middle of an active turn.

## Escalation

Load `identity-correlation.md` when a global health failure must be tied to one
workspace or session. Load `structured-logs.md` when the doctor names a failed
surface but not the cause. Load `observability-gaps.md` when a health or
readiness field cannot answer a necessary question.
