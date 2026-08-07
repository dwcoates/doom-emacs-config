---
name: debug-emacs-agent-repl
description: Investigate the agent-repl system across Emacs, daemon, shim, webapp, sidecar, store, state, deployment, logging, and test coverage. Use for current or historical workspace failures, wrong colors or stuck states, missing conversation data, service-health questions, source-versus-running readiness, workspace/session/process correlation, structured-log analysis, SSM or store SQL investigation, coverage questions, missing telemetry, iterative critical-path instrumentation, reload, reproduction, and log-analysis loops, and orchestrated bounce-observe-remediate loops that drive the stack to a healthy steady state.
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

### 2. Select the initial runbooks

Read every selected reference completely before following its workflow.
Select the smallest set that can answer the case, but compose references when
the symptom crosses planes.

| Runbook | Investigative question | Read first | Common companions |
|---|---|---|---|
| Health and readiness | Is a service, socket, deployed artifact, or running process healthy and current? | `references/health-and-readiness.md` | identity, logs |
| Identity correlation | Which workspace, session, process, connection, or request is this? | `references/identity-correlation.md` | logs |
| Structured logs | What happened across runtimes and in what order? | `references/structured-logs.md` | identity |
| State investigation | Why is a workspace this color or state? | `references/state-investigation.md` | identity, logs |
| Conversation investigation | Why is conversation data missing or malformed? | `references/conversation-investigation.md` | identity, logs, health |
| Performance investigation | Where is the time going — latency, stalls, hitching? | `references/performance-investigation.md` | logs, identity, observability |
| Testing and coverage | What tests or coverage exercise this path? | `references/testing-coverage.md` | observability |
| Observability gaps | Is the evidence itself adequate for a diagnosis? | `references/observability-gaps.md` | the active runbooks |
| Critical-path observability loop | Must the path be instrumented, reloaded, provoked, and inspected iteratively to expose and verify the bug? | `references/critical-path-observability-loop.md` | logs, observability, testing |
| Iterative fix-verify loop | Is the system being driven to a healthy steady state through repeated bounce, observe, and remediate iterations? | `references/iterative-fix-verify-loop.md` | health, logs, identity, critical path, observability |

Apply an ambiguity gate before choosing:

1. If exactly one runbook unambiguously matches, select it and continue.
2. If two or more runbooks plausibly fit, the symptom crosses strategies, or
   the best starting strategy is uncertain, stop before selecting or running
   diagnostics.
3. Surface the plausible runbooks to the user. For each, state in one line
   what question it answers, which runtime or evidence plane it touches, and
   whether it is read-only or may require source or runtime mutation.
4. Ask the user to select the starting runbook or combination. Wait for that
   selection; do not silently choose a primary runbook or companion set.

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
to another plane, add that plane's runbook and state why. Apply the ambiguity
gate again when more than one next runbook is plausible. Do not load every
reference merely because it exists.

Use `/runtime-eval-code` when the needed fact exists only inside the live Emacs
process or in third-party `*Messages*` output. For latency, stalls, or
hitching, enter through `references/performance-investigation.md`, which
decides between log-derived history and live sampling and hands off to
`/profile` as the sampling authority. Neither handoff replaces the evidence
already collected here.

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

## Adding a runbook

A runbook codifies one operator workflow. Add one when a workflow has been
worked out in practice and will be repeated, not to record a single
investigation.

To add one:

1. Write it as `references/<kebab-name>.md` in the repository skill directory,
   `modules/app/agent-repl/skills/debug-emacs-agent-repl/`. The installed
   skill is a symlink to that directory; never edit the installed copy.
2. Register it in the runbook-selection table above with its investigative
   question, its read-first path, and its common companions. An unregistered
   reference is unreachable, because selection happens only through that
   table.
3. Write it in the operator-workflow style the other references use: the
   investigative question and scope up top, when to select it and when not to,
   the ordered workflow, and a closing pointer to the runbooks it composes
   with.
4. Defer to the Sources of authority. A runbook points at the logging
   contract, the executable scripts, and the component `AGENTS.md` files; it
   never restates a contract those own, and never states one that contradicts
   them.
5. Do not duplicate what a sibling runbook owns. Point to it instead.

### Keeping a new runbook current within its session

A runbook written during a working session codifies the loop or process as it
stood at that moment. Subsequent discussion in the same session frequently
revises it implicitly — a new gate, a changed threshold, a step reordered, a
criterion added or dropped — without anyone saying "update the runbook."

Detect those implicit revisions and offer them. When later instruction or
agreement in the session diverges from what the runbook says, state the
divergence and offer the update as a proposed diff or a short summary for the
user to accept or decline.

Two failure modes are equally unacceptable: silently drifting from the written
runbook while claiming to follow it, and silently rewriting the runbook to
match what just happened. The revision is the user's call; surfacing it is not.

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
