# Iterative fix-verify loop

Use this runbook when the question is not "why did this one thing fail" but
"is the whole stack being driven to a healthy steady state?" The loop bounces
the full stack to current builds, bounces Emacs, observes startup and one
probe, root-causes every issue it finds, fans the fixes out to parallel
implementation subagents, merges them, and repeats until the exit criteria
hold.

This runbook owns the orchestration. It owns no contract: the baseline sweep
belongs to `health-and-readiness.md`, the evidence belongs to
`structured-logs.md` and `identity-correlation.md`, the escalation when
evidence is inadequate belongs to `critical-path-observability-loop.md`, and
the pre-conclusion audit belongs to `observability-gaps.md`.

## When to select it

Select this runbook when:

- The user asks for the system to be driven to a working, healthy, or steady
  state rather than for one diagnosis.
- Startup is slow, partially wrong, or inconsistent across workspaces, and the
  suspected causes are plural.
- A deploy has just landed a wide change set and the whole stack must be
  re-proven.

Do not select it for a single reproducible bug with a known symptom. That is
`critical-path-observability-loop.md`. Do not select it for a read-only
diagnosis: this loop mutates source, deployed artifacts, and runtime state,
and it requires the user to have asked for an implementation.

## 1. Bounce the stack to current builds

Run the full deploy chain so that no later observation can be blamed on a
stale artifact:

```sh
modules/app/agent-repl/bin/deploy-all.sh
```

The script owns its own ordering and its own failure semantics. In summary it
regenerates protobufs, runs `build-frontend.sh`, forces the daemon build,
builds and kickstarts the store and then the sidecar with a wait on
`store.sock` between them, bounces the daemon through `emacsclient`, and
refreshes the mounted webviews. Never reorder the store and sidecar by hand;
see the restart-safety section of `health-and-readiness.md`.

Then bounce Emacs itself. A daemon bounce does not re-prove Emacs-side
startup, and Emacs-side startup is what this loop measures.

## 2. Observe the startup

Watch two sinks while Emacs comes up:

- The global Emacs agent-repl log, resolved through
  `agent-repl-log-discovery.sh --global`.
- The daemon log.

Resolve both through the discovery script rather than guessing paths, per
`structured-logs.md`.

Measure per-workspace startup latency. The target is one second or less per
workspace, with no workspace taking more than a couple of seconds. Record the
per-workspace numbers; a mean hides the one workspace that is actually broken.

Then run one probe. Choose a workspace at random, invoke `SPC o c`
(`agent-repl-simple`), and inspect:

- The daemon-log delta produced by the probe.
- The webapp console for identity rejections, warnings, and connectivity
  failure cards.

Use `identity-correlation.md` to tie any rejection back to a workspace,
session, or connection before treating it as a class of failure.

## 3. Cap every wait at five minutes

Never wait more than five minutes for startup. Reaching five minutes is not a
reason to keep waiting; it is the evidence that something is wrong. Stop
waiting and mine the logs.

If the logs do not make the cause apparent, the deficiency is in the evidence,
not in the wait. Escalate into `structured-logs.md` and
`critical-path-observability-loop.md` and iterate on the instrumentation until
the logs can name the failing stage. Improving the evidence is loop work, not
a detour from it.

## 4. Exit criteria

Iterate until all of the following hold in a single iteration:

| Criterion | Test |
|---|---|
| Quick startup | Every workspace starts in about one second; none takes more than a couple of seconds. |
| Correct attach | Existing shims are identified and reattached rather than replaced. |
| Correct spawn | A new shim is spawned if and only if no shim exists for that workspace. |
| Correct resume | An existing transcript is always detected and resumed; a fresh conversation where a transcript exists is a failure, not a cosmetic difference. |
| Clean probe | The probe opens cleanly, with zero connectivity or failure cards in the session view. |
| No warnings observed | Under `--address-warnings` or `--address-warnings-first` only: zero warning records in the startup log sweep and the probe delta. Not a criterion by default; see Invocation modifiers below. |

A criterion that holds only because the evidence for it is missing does not
hold. Complete `observability-gaps.md` before declaring an iteration clean.

### Invocation modifiers

The loop takes one optional modifier, named by the operator when the loop is
started. It is the same modifier pair the
`interaction-replay-remediation.md` runbook defines, with the observation
window read as this loop's own: the startup log sweep of step 2 plus the delta
the single probe produces. The modifier decides whether WARNINGS in those
windows are loop-critical — gating the next iteration and the exit — or
non-gating, in the sense step 7 gives those words.

| Modifier | Warnings gate the exit | Order |
|---|---|---|
| *(none)* | No | Errors, timing, and cards only. |
| `--address-warnings` | Yes | Errors, timing, and cards first, then warnings. |
| `--address-warnings-first` | Yes | Warnings first, then errors, timing, and cards. |

**Default, no modifier.** Warnings observed in the startup sweep or the probe
delta are recorded and reported to the user each iteration, but they are not
loop-critical: they never gate an iteration and never hold the exit open. Only
the criteria in the table above do.

**`--address-warnings`.** The exit criteria widen to require zero warnings in
the observation windows as well. The loop runs in two phases, and the order is
a mandate rather than a preference:

1. Phase 1 iterates on the errors, timing, and cards of the exit table alone,
   exactly as the default does. Warnings are recorded and left alone. Do not
   dispatch a warning fix in this phase.
2. Phase 2 begins only once a full iteration satisfies that table, and iterates
   until the windows are warning-clean too.

Errors come first because a warning emitted downstream of an error is usually
the error's consequence. Remediating it first spends an iteration on a line
that the error's fix would have removed, and leaves a change in the tree whose
justification no longer exists.

**`--address-warnings-first`.** Same widened exit criteria, opposite order:

1. Phase 1 iterates until the startup sweep and the probe delta are
   warning-clean. Errors, timing, and cards are recorded and left alone.
2. Phase 2 then iterates on the errors, timing, and cards of the exit table.

Select this when the warnings are suspected of naming the cause the errors only
report the effect of, or when the warning volume is drowning the error records
in the sweep.

The two modifiers are MUTUALLY EXCLUSIVE — they state opposite orderings of the
same two phases. If both are specified, refuse to start and ask the user which
ordering they meant. Do not pick one, and do not silently run the phases
concurrently.

Under either modifier the exit requires ALL criteria to hold in ONE iteration,
so phase-2 work must not have reintroduced what phase 1 cleared. A phase-2
iteration that clears warnings while a failure card reappears has not exited;
it has moved back into phase 1.

### Addressing a warning means fixing its cause

Under either modifier, a warning is closed by removing what provoked it.
Silencing the warning itself is not remediation: do not suppress it, downgrade
it to debug, filter it out of the startup sweep or the probe delta, or delete
the emit site. A window that is warning-clean because the warnings were muted
satisfies no criterion; it only destroys the evidence the next iteration needed.

Silencing is permitted only when there is a very good reason and that reason is
stated explicitly to the user — and only when the warning is not hinting at a
structural invariant being violated or eroded. If it plausibly is, the
invariant is what gets investigated and fixed, and the warning stays in place
as its sentinel.

## 5. Remediate without user mediation

Every issue the observation surfaces is root-caused, not symptom-patched. A
change that makes the card disappear without naming the decision that produced
it does not close an issue.

Once root-caused, the orchestrator proceeds on its own from assessment through
remediation plan, subagent fanout determination, and dispatch. The user does
not mediate the steps between finding an issue and dispatching its fix.

Report each dispatched fix to the user as exactly three one-liners:

1. Problem: the root cause, named at the decision or boundary that failed.
2. User-visible effect: what the user experienced because of it.
3. Fix: what the dispatched agent is changing.

## 6. Fanout mechanics

Dispatch fixes as parallel implementation subagents under these rules:

- Each agent works in its own git worktree on its own branch.
- Each agent is required to fast-forward from the main checkout first.
  Worktrees materialize stale, and an agent that skips this silently rebuilds
  against an old tree.
- Each agent typically runs the affected test suites as its own evidence;
  suite invocations are documented in the component `AGENTS.md` files.
- The orchestrator never edits code itself. Its only writes are merges and the
  conflict resolutions those merges require, which are exclusively its job and
  are never delegated back to an agent.

## 7. Gate only on loop-critical fixes

Only remediation agents whose fixes are critical to the exit criteria — the
startup-critical ones — gate the next iteration.

Non-critical fixers and user-requested feature agents run in parallel and are
not waited on. They merge into whichever later deploy comes next. Blocking the
loop on work that cannot change an exit criterion is the common way this loop
stalls.

## 8. Merge and redeploy

As each agent lands:

1. Merge its branch into master, resolving conflicts in the orchestrator.
2. Run the affected suites on the merged result.
3. When the loop-critical set is fully merged, redeploy and begin the next
   iteration at step 1.

Do not redeploy mid-merge. An iteration that observes a partially merged tree
cannot attribute its findings to any revision.

## Composition

- `health-and-readiness.md` for the baseline doctor and readiness sweep, and
  for the store-before-sidecar restart order.
- `structured-logs.md` and `identity-correlation.md` for the observation step.
- `critical-path-observability-loop.md` when the five-minute cap fires and the
  logs cannot name the cause.
- `observability-gaps.md` before concluding any iteration is clean.
- `performance-investigation.md` when startup latency is the surviving issue
  and the log record cannot localize the time.
- `interaction-replay-remediation.md` when the probe of step 2 cannot provoke
  the issue and a recorded interaction must stand in for it; it defines the
  same warning modifiers over a replay window instead of the startup sweep.
