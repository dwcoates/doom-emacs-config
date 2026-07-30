# Critical-path observability loop

Use this runbook when source inspection and existing evidence cannot explain a
reproducible runtime bug with high confidence, and the user has asked for an
implementation or explicitly authorized source and runtime changes. The loop
maps the user-visible effect to its producers, repairs instrumentation, reloads
the affected runtime, provokes the path under bounded diagnostics, reads the
canonical logs, and repeats until the root cause and fix are evidenced.

For a diagnosis-only request, perform the read-only path and observability
audit. If the next iteration requires source instrumentation or runtime
mutation, report the exact proposed change and wait for authorization.

## Define the loop contract

Before editing or mutating runtime state, record:

- The visible symptom and the precise success condition.
- The smallest deterministic action that provokes the symptom.
- The action that deprovokes it and restores the prior runtime state.
- The affected workspace and known identity fields.
- The source revision and deployed or loaded revision.
- The critical path from input through each decision and boundary to the
  rendered, persisted, or transmitted output.
- The canonical log destination for every runtime on that path.
- The diagnostic time bound, such as a ten-second capture window.

Do not begin with an unbounded reproduction. Preserve enough baseline state to
restore toggles, variables, hooks, frame parameters, timers, and user-visible
state after every capture.

## Map the critical path

Trace the path in source before reading isolated log lines:

1. Identify the entry point that receives the provoking action.
2. Follow every formatter, renderer, resolver, cache, hook, advice, timer, and
   runtime boundary that can change the result.
3. Identify the final consumer that presents or persists the result.
4. List meaningful early returns, branches, mutations, cache hits, and errors.
5. Mark third-party integration boundaries and their agent-repl wrappers.
6. Mark which definitions must be reloaded for source edits to take effect.

Keep a path ledger:

| Stage | Inputs | Branch or mutation | Expected output | Existing canonical evidence |
|---|---|---|---|---|
| entry | resolved arguments and identity | selected route | downstream call | operation and inputs |
| decision | computed flags and state | chosen branch | branch result | decision inputs and outcome |
| boundary | request and target identity | success or error | response | start, completion, duration, error |
| consumer | final payload and dimensions | render or persist | visible result | final computed output |

An entry log without the decision inputs or final computed output does not make
the path observable.

## Audit and repair instrumentation

Read `structured-logs.md` and `observability-gaps.md` completely when this
runbook is selected. Compare each ledger stage with the canonical evidence.
Verify that the running artifact is current before classifying a missing
record as missing instrumentation.

For each hole, add or repair canonical instrumentation that records:

- Workspace and session identity whenever derivable.
- Resolved paths, dimensions, limits, flags, and state.
- Cache keys, hit or miss outcome, and invalidation reason.
- The selected branch, including meaningful early returns.
- Mutation target, prior value, requested value, and resulting value.
- Boundary start, completion, duration, and owned error.
- The final value delivered to the next stage or user-visible consumer.

Use the runtime's normal canonical logger. Use verbose records for
merely-frequent paths. For paths firing more than about once per second, use a
bounded diagnostic gate, state-change deduplication, or sampling so the capture
cannot bury other evidence. Never use a dedicated buffer or ad hoc file as the
sole diagnostic record.

Add focused tests for the repaired instrumentation and the suspected behavior.
Run focused tests during the loop and the unified verifier before handoff.

## Reload and prove loaded state

Reload the changed runtime yourself. Do not ask the user to perform the reload.

For Emacs:

1. Use `/runtime-eval-code`; never use an ad hoc `emacsclient` probe.
2. Load the owning agent-repl module or the complete module configuration
   required by the dependency graph.
3. Capture reload success or failure in the canonical log.
4. Read back the loaded function, variable, frame, hook, advice, or timer state
   that proves the new definition is active.
5. Treat a queued eval as incomplete until the verification read returns.

For daemon, shim, webapp, sidecar, or store changes, follow the component's
deploy and restart policy. Never restart the store or sidecar without explicit
permission. Re-run readiness after a deploy or restart.

A reload error is part of the bug investigation. Instrument and fix it before
continuing; never reason from source that is not proven loaded.

## Provoke, deprovoke, and capture

Run one bounded capture:

1. Record the start timestamp, workspace, identity spine, and pre-capture
   runtime state.
2. Enable only the verbose, tracing, sampling, or diagnostic gates needed for
   the mapped path.
3. Provoke the critical path with the smallest deterministic action.
4. Allow the asynchronous path to settle within the declared capture window.
   Prefer an observable completion condition; otherwise use the bounded window.
5. Disable every diagnostic gate.
6. Deprovoke the feature and restore the captured runtime state when the
   provoking action changed it.
7. Verify that diagnostics are disabled and restored values match the baseline.
8. Resolve and read the canonical logs for the bounded time and identity range.

Arrange cleanup so errors cannot leave verbose echo, advice, hooks, timers, or
diagnostic gates enabled. Durable verbose records do not depend on echo-area
verbosity; toggles should control capture volume or live visibility, not
whether canonical evidence survives.

## Reassess and iterate

After each capture, update the path ledger:

- Which stages emitted the expected records?
- Which exact inputs and branch outcomes are now proven?
- Where does the first divergence from the expected path occur?
- Is the remaining silence a filter, identity, freshness, routing, or
  instrumentation problem?
- Can the evidence distinguish every surviving root-cause hypothesis?

If the capture does not establish a root cause:

1. Re-open the source path at the first ambiguous stage.
2. Add or correct the minimum instrumentation needed to distinguish the
   surviving hypotheses.
3. Add or update focused tests.
4. Reload and prove the new source is active.
5. Repeat the bounded provoke, deprovoke, and log capture.

Do not stop merely because one plausible explanation fits. Continue until the
canonical trace identifies the failing decision or mutation and carries the
inputs needed to reproduce why it failed.

## Verify the fix and close the loop

Reach high confidence only when all are true:

- The canonical pre-fix trace identifies the root cause.
- The source fix addresses that exact decision, mutation, or boundary.
- Focused tests cover the behavior and its meaningful error paths.
- A clean runtime reload activates the final source.
- One final bounded reproduction shows the expected user-visible behavior.
- The canonical post-fix trace records the corrected inputs, branch, and final
  output.
- The unified local verifier passes.

Finish with diagnostics at normal settings. Remove temporary runtime advice,
hooks, timers, and toggles; retain purposeful source instrumentation. Report
the root cause, evidence, fix, verification, any remaining uncertainty, and
which runtime or disk changes persist after unloading and reloading.
