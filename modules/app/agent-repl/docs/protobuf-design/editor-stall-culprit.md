# Editor stall culprits

The editor's `editor_stall` latency said how long its command loop was held,
never by what: after the 2026-11-25 12:32 deploy Emacs hitched 92 times in a
minute and no record named the work. Each stall now carries the work that held
the loop, so the stall is attributable from the shared log and the metrics.

## Assumptions (autonomous design)

- Objective: name, per stall, the timer, command, daemon connection handler,
  garbage collection or untimed remainder that held the loop longest.
- Systems: Emacs (measures and reports), the daemon (validates and records).
  No frontend component.
- Non-additive changes: none. `EDITOR_LATENCY_KIND_STALL` stays accepted.

## Landed changes

### `agentrepl.v1.ReportedEditorStall` and `ReportLatenciesRequest.editor_stalls`

- WHAT: a new repeated field of the request carries each stall with an
  `EditorStallCulprit`, a oneof of the kind of work (timer, process filter,
  process sentinel, command, garbage collection, unattributed). Timer and
  command name an `EditorStallFunction` (named symbol, or anonymous); filter
  and sentinel name the AgentRepl rpc method.
- WHY a separate message rather than a culprit field on
  `ReportedEditorLatency`: the culprit means nothing for the other editor
  latency kinds (an elisp reload, an input wait), so a field beside the kind
  enum would fail the adjacent-exclusivity test.
- WHY `EDITOR_LATENCY_KIND_STALL` is kept: Emacs and the daemon reload apart.
  An Emacs running the older stall.el keeps sending it to a newer daemon, and
  a stall queued by the older stall.el before a hot reload is still in the
  queue the reloaded latency-report.el sends. Both are recorded as the same
  `editor_stall` latency, without a culprit. It can be removed once no editor
  runs the older code.
- WHY the label vocabulary is closed this way: the rpc method is validated
  against the AgentRepl service descriptor; a function label names only
  agent-repl's own functions (prefix `agent-repl`), folding every other into
  `other`. Emacs's own record carries every name, so a foreign culprit is
  still diagnosable from the log.
- Consequence accepted: a set-but-unassigned culprit (a newer editor's arm)
  is recorded under source `unknown` rather than refused.
