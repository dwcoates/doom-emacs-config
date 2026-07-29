---
name: debug-emacs-agent-repl
description: Investigate the agent-repl system across Emacs, daemon, shim, webapp, sidecar, store, state, deployment, logging, and test coverage. Use for current or historical workspace failures, wrong colors or stuck states, missing conversation data, service-health questions, source-versus-running readiness, workspace/session/process correlation, structured-log analysis, SSM or store SQL investigation, coverage questions, and missing telemetry.
---

# Debug agent-repl

Use this skill as the single front door for agent-repl investigations. Do not
assume the failing service from the visible symptom. Derive an evidence plan,
load only the relevant references, and revise the plan when evidence crosses a
runtime boundary.

## Sources of authority

Keep facts in one place:

1. `../../logging-contract.md` owns durable log routing, JSONL shape, identity
   fields, and runtime ownership.
2. Executable scripts own their accepted arguments and report semantics.
3. Component `AGENTS.md` files own contributor requirements for logging,
   testing, and coverage.
4. This skill and its references own operator workflows. They must point to
   the authorities above rather than redefine incompatible contracts.

When prose conflicts with an authority, follow the authority and report the
documentation defect through the observability review.

## Investigation controller

### 1. Frame the case

Write a short internal investigation brief before opening evidence:

- Symptom and affected user-visible object.
- Absolute workspace directory when one exists.
- Time or time window.
- Known identifiers: workspace ID, agent-repl session ID, vendor session ID,
  PID, connection ID, request ID, task ID, and sequence number.
- Initial hypotheses, phrased as questions rather than conclusions.
- Evidence expected for each hypothesis.

Do not invent missing identifiers. Resolve them through
`references/identity-correlation.md`.

### 2. Select the initial playbooks

Read every selected reference completely before following its workflow.
Select the smallest set that can answer the case, but compose references when
the symptom crosses planes.

| Investigative question | Read first | Common companions |
|---|---|---|
| Is a service or socket healthy? | `references/health-and-readiness.md` | identity, logs |
| Is deployed or running code stale? | `references/health-and-readiness.md` | logs |
| Which workspace, session, or process is this? | `references/identity-correlation.md` | logs |
| What happened across runtimes? | `references/structured-logs.md` | identity |
| Why is a workspace this color or state? | `references/state-investigation.md` | identity, logs |
| Why is conversation data missing or malformed? | `references/conversation-investigation.md` | identity, logs, health |
| What tests or coverage exercise this path? | `references/testing-coverage.md` | observability |
| Is the evidence itself inadequate? | `references/observability-gaps.md` | the active playbooks |

For an unexplained live or historical runtime failure, begin with health,
readiness, and identity before narrowing to a data source. For a pure
test-coverage question, begin with `references/testing-coverage.md` and do not
run live-service diagnostics without a runtime reason.

### 3. Establish the baseline

For runtime investigations:

1. Run the read-only health sweep from
   `references/health-and-readiness.md`.
2. Run the readiness report to distinguish source, deployed artifacts, and
   running processes.
3. Resolve the identity spine from workspace through session and process.
4. Read canonical logs through the resolver rather than guessing paths.
5. Query SQLite only when the question concerns resolved state or persisted
   conversation data.

Record `FAIL`, `SKIP`, `ready: false`, and per-system `error` values as
evidence. A command exiting successfully does not make every reported
component healthy or ready.

### 4. Follow and revise the evidence plan

Keep facts, inferences, and missing evidence distinct. When a finding points
to another plane, add that plane's playbook and state why. Do not load every
reference merely because it exists.

Use `/runtime-eval-code` when the needed fact exists only inside the live Emacs
process or in third-party `*Messages*` output. Use `/profile` for performance
and hitching. Neither handoff replaces the evidence already collected here.

### 5. Audit observability before concluding

Always complete `references/observability-gaps.md` before the final diagnosis.
For each surviving hypothesis, compare expected evidence with available
evidence. Never treat a missing log line, database row, health field, or
correlation identifier as proof that an event did not occur.

Surface every material blind spot to the user with:

- The affected runtime, function, branch, or boundary when resolvable.
- The evidence that should have existed.
- The evidence actually available.
- The uncertainty the gap creates.
- The canonical logging, identity, health, readiness, or test support needed
  to close it.

Recommend source changes when telemetry is inadequate. Do not make those
changes during a read-only investigation unless the user also asked for an
implementation.

## Safety

- Keep investigations read-only unless the user explicitly asks for a change.
- Query both SQLite databases with `sqlite3 -readonly`.
- Never run an unbounded store query.
- Never restart `shim-store` or `shim-claude-sidecar` without explicit user
  permission.
- Never mutate logs, snapshots, registries, sockets, or runtime state merely
  to simplify an investigation.
- Never substitute a global log when a record is conceptually workspace-owned.
- Never fill missing telemetry with speculation.
