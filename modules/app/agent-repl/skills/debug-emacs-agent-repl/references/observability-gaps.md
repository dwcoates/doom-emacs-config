# Observability gaps

Run this audit before every final diagnosis. Its purpose is to determine
whether the available evidence can actually support the conclusion.

## Evidence adequacy ledger

For each surviving hypothesis, record:

| Item | Question |
|---|---|
| Expected signal | What log, row, readiness field, health result, or test should exist? |
| Actual signal | What evidence was observed? |
| Identity | Can the evidence be tied to the correct workspace, session, process, request, task, or sequence? |
| Freshness | Was the relevant source deployed and running? |
| Branch visibility | Does the evidence show which meaningful branch ran? |
| Error ownership | Is an error recorded exactly once with cause and resolved inputs? |
| Sufficiency | Can the evidence prove or disprove the hypothesis? |

Keep `absent`, `filtered out`, `not emitted`, `not persisted`, `not correlated`,
and `not queryable` distinct.

## Rule out false telemetry gaps

Before declaring missing instrumentation, verify:

1. The relevant artifact was deployed and is running.
2. The correct workspace and identifier namespace were selected.
3. The time range and tail bound include the event.
4. The correct runtime log was selected.
5. The canonical symlink resolves.
6. JSONL parsing did not fail.
7. A database query used the correct session namespace.
8. The expected event is a persisted class.
9. The signal is not owned by a different runtime or boundary.

Verbose terminal visibility does not control durable persistence. Do not blame
an absent durable record on a disabled verbose console.

## Classify the deficiency

Use one or more categories:

- Missing function-entry instrumentation.
- Missing branch or early-return instrumentation.
- Missing state-transition cause.
- Missing error record.
- Duplicate error ownership.
- Missing structured context.
- Missing workspace, session, process, connection, request, task, or sequence
  identity.
- Missing health probe.
- Missing source, deployed, or running readiness evidence.
- Missing queryable state.
- Missing focused unit test.
- Missing error-path test.
- Missing percentage-based coverage tooling.
- Contradictory or stale diagnostic documentation.
- Diagnostic tool unable to expose an implemented signal.

## Materiality

Surface a deficiency when it:

- Blocks a confident diagnosis.
- Leaves two materially different hypotheses indistinguishable.
- Hides a critical branch or state transition.
- Makes an error unattributable.
- Prevents workspace-to-runtime correlation.
- Makes deployed and source behavior impossible to distinguish.
- Leaves a critical or error path without focused test coverage.

Do not report every optional diagnostic enhancement. The audit is about
missing vision that affects correctness or confidence.

## User-facing report

State each material gap in this form:

1. Affected runtime, function, branch, or boundary.
2. Expected evidence.
3. Available evidence.
4. Diagnostic consequence.
5. Recommended canonical support.

Example:

> The daemon accepted the workspace command, but no canonical record identifies
> the selected session or the branch that rejected delivery. The existing trace
> cannot distinguish identity mismatch from shim unavailability. Add one
> daemon-owned structured branch record carrying `workspace_dir`,
> `agent_repl_session_id`, `request_id`, outcome, and cause, plus an error-path
> unit test asserting that record.

Do not soften the uncertainty with speculation. Say explicitly when the
evidence is insufficient.

## Recommended remediation

Match the gap to the canonical owner:

- Runtime behavior: the runtime's normal or verbose logging API.
- Persistent state: an SSM row when the value genuinely determines later
  resolved state.
- Cross-runtime linkage: dedicated identity fields rather than message text.
- Service liveness: a read-only doctor check.
- Artifact freshness: a readiness-report field.
- Code behavior: focused unit tests and component coverage.
- Error behavior: one owning log record plus deterministic error-path tests.
- Tool visibility: extend the existing diagnostic script and its focused
  harness rather than adding an ad hoc probe.

If the user requested diagnosis only, surface the remediation and stop before
editing. If the user requested a fix, include the observability repair in the
same body of work and verify it through the corresponding tests.
