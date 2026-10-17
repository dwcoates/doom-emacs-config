# Interaction replay remediation

Use this runbook when the question is "can the user's own interaction sequence
reproduce the issue, and can it be replayed hands-free until it runs clean?"
The user performs the reproduction exactly once, into a recorder. Every
iteration after that replays the recording instead of asking the user to do it
again, so the fix-verify loop runs autonomously.

This runbook owns one thing: turning a human reproduction into a replayable
artifact and folding that artifact into an existing loop. It owns no contract
and no cycle discipline of its own. The iteration mechanics belong to
`iterative-fix-verify-loop.md`, the evidence belongs to `structured-logs.md`,
and the pre-conclusion audit belongs to `observability-gaps.md`.

## When to select it

Select this runbook when:

- The issue is only reproducible through a specific sequence of user actions,
  and `SPC o c` alone does not provoke it.
- The user can demonstrate the failure but cannot sit through repeated
  verification cycles.
- A fix must be verified against the same interaction sequence every time, so
  that a difference between iterations is attributable to the code rather than
  to how the reproduction was performed.

Do not select it when the symptom appears at startup or on the single `SPC o c`
probe; `iterative-fix-verify-loop.md` already provokes those without a
recording. Do not select it for a read-only diagnosis: it mutates runtime state
from the first step, and the loop it feeds mutates source and deployed
artifacts.

Do not select it when the interaction being investigated lives inside the
webview. See Fidelity limits.

## 1. Start Emacs with recording live, then hand over

Bounce Emacs with the recorder enabled:

```sh
AGENT_REPL_RECORD_INTERACTIONS=1 emacs
```

The env var is read at module load, so recording is live before the user's
first keystroke. `M-x agent-repl-interaction-record-mode` is the interactive
equivalent for a session already running; prefer the env var, because a mode
toggled partway through leaves the beginning of the reproduction unrecorded.

Then TELL the user recording is live and hand control over. Say explicitly:

- Recording is on and capturing every command.
- They should use Emacs freely until they are satisfied they have reproduced
  the issue or issues.
- They must tell the agent when they are done, because the agent cannot see
  the reproduction happen.

Do not narrate, poll, or drive Emacs during this step. Any command the agent
issues lands in the recording and will be replayed as if the user had typed it.

## 2. Wait for the user

The user owns this step end to end. Wait for their word that the reproduction
is complete. Do not infer completion from log activity going quiet.

## 3. Save the recording and report its path

```sh
emacsclient -e '(agent-repl-interaction-record-save)'
```

The call returns the absolute path of the saved file, under
`~/.claude-emacs/interaction-recordings/`. Report that path to the user; it is
the artifact the rest of the loop runs on, and the user may want to keep,
inspect, or re-record it.

An empty recording is an error, not an empty file. If the call errors, the
recorder was not armed during the reproduction — re-arm and repeat from step 1
rather than proceeding with nothing to replay.

Read the saved file once before relying on it. It is data, not code: each
event carries its offset, its key sequence, its key description, and the
command symbol. Confirm the commands look like what the user described. A
recording whose transcript does not match the reported reproduction is the
cheapest failure to catch here, before it costs an iteration.

## 4. Iterate autonomously on the replay

From here the loop is `iterative-fix-verify-loop.md`, unchanged, with one
substitution: the replay stands in for the all-workspace `SPC o c` probe sweep
of its step 2, either replacing it or following it when the startup probe sweep
is still worth measuring.

Each iteration:

1. Clear the observation logs before the bounce, per "Clear the observation
   logs first" in `iterative-fix-verify-loop.md` step 1: the global Emacs
   agent-repl log and the daemon log, plus any further sink this case's replay
   window is mined on in step 4. The previous iteration's findings are already
   recorded, and an empty file makes the begin and end markers bound a window
   with nothing before them to reason about. That runbook also owns the
   carve-out this step depends on — iteration-start clearing is a
   user-directed exception to the skill's "never mutate logs" Safety rule,
   valid inside these two loops' iterations and nowhere else — and the
   instruction to STOP clearing and surface it to the user the moment clearing
   costs something, such as a forensic trail that was still needed or a
   comparison across replays that became impossible.
2. Bounce the stack and Emacs per `iterative-fix-verify-loop.md` step 1.
   Recording must be OFF for these bounces; replay refuses to run while the
   recorder is armed, so a bounce that re-reads
   `AGENT_REPL_RECORD_INTERACTIONS=1` will block the replay.
3. Replay the saved sequence:

   ```sh
   emacsclient -e '(agent-repl-interaction-replay "/absolute/path/to/recording.el")'
   ```

   The call returns a replay id and returns IMMEDIATELY — the events are
   scheduled, not executed inline. Wait for the end marker before mining, and
   cap that wait per `iterative-fix-verify-loop.md` step 3.

   An optional second argument scales the recorded delays
   (`(agent-repl-interaction-replay FILE 2.0)` replays twice as fast). Leave it
   at the default while the bug is unexplained: compressing the gaps changes
   the timing the bug may depend on.
4. Bound the replay window in the logs. The replay logs a begin line and an end
   line carrying the returned replay id:

   - `interaction-replay: begin replay_id=<id> ...`
   - `interaction-replay: end replay_id=<id> events=<n> failures=<n>`

   Resolve log paths through the discovery script per `structured-logs.md`;
   never guess a path. The two markers are what make "attributable to the
   replayed window" a fact rather than an assumption.
5. Mine that window for errors, warnings, and slowdowns, across the Emacs,
   daemon, shim, webapp, and sidecar sinks the case implicates. A nonzero
   `failures=` count on the end line names events that could not be executed at
   all; per-event failures are logged individually with their index and
   command. Record the warnings whether or not they gate this run — which they
   do is decided by the modifier in step 5.
6. Root-cause, fan out, merge, and redeploy exactly per
   `iterative-fix-verify-loop.md` steps 5 through 8, dispatching only the
   findings the current phase owns (step 5). The user does not mediate between
   finding an issue and dispatching its fix.

## 5. Invocation modifiers

The loop takes one optional modifier, named when it is started. The modifiers
decide whether WARNINGS in the replay window are loop-critical — gating the
next iteration and the exit — or non-gating, in the sense
`iterative-fix-verify-loop.md` step 7 gives those words.

| Modifier | Warnings gate the exit | Order |
|---|---|---|
| *(none)* | No | Errors and slowdowns only. |
| `--address-warnings` | Yes | Errors and slowdowns first, then warnings. |
| `--address-warnings-first` | Yes | Warnings first, then errors and slowdowns. |

**Default, no modifier.** Warnings observed in the replay window are recorded
and reported to the user, but they are not loop-critical: they never gate an
iteration and never hold the exit open. Only errors and slowdowns do.

**`--address-warnings`.** The exit criterion widens to require zero warnings in
the replay window as well. The loop runs in two phases, and the order is a
mandate rather than a preference:

1. Phase 1 iterates on errors and slowdowns alone, exactly as the default does.
   Warnings are recorded and left alone. Do not dispatch a warning fix in this
   phase.
2. Phase 2 begins only once a full replay is error-clean and slowdown-clean,
   and iterates until the window is warning-clean too.

Errors come first because a warning emitted downstream of an error is usually
the error's consequence. Remediating it first spends an iteration on a line
that the error's fix would have removed, and leaves a change in the tree whose
justification no longer exists.

**`--address-warnings-first`.** Same widened exit criterion, opposite order:

1. Phase 1 iterates until the replay window is warning-clean. Errors are
   recorded and left alone.
2. Phase 2 then iterates on errors and slowdowns.

Select this when the warnings are suspected of naming the cause the errors only
report the effect of, or when the warning volume is drowning the error records
in the window.

The two modifiers are MUTUALLY EXCLUSIVE — they state opposite orderings of the
same two phases. If both are specified, refuse to start and ask the user which
ordering they meant. Do not pick one, and do not silently run the phases
concurrently.

Under either modifier, a warning is closed by removing what provoked it, never
by silencing the warning: no suppressing it, no downgrading it to debug, no
filtering it out of the replay window, no deleting the emit site. A window that
is warning-clean because the warnings were muted satisfies no criterion. The
narrow exception — a very good reason, stated explicitly, and only where the
warning is not hinting at a structural invariant being violated or eroded — is
defined once under "Addressing a warning means fixing its cause" in
`iterative-fix-verify-loop.md` step 4, which this runbook follows unchanged.

## 6. Exit criteria

Iterate until, in a single iteration:

| Criterion | Test |
|---|---|
| Every event ran | The end line reports `failures=0`. |
| No errors in the window | No error records between the begin and end markers on any implicated sink. |
| No heavy slowdown | Per-event handling latency meets the latency expectations of `iterative-fix-verify-loop.md` step 2 and its exit table. |
| Startup still clean | The exit table of `iterative-fix-verify-loop.md` still holds for the same iteration. |
| No warnings in the window | Under `--address-warnings` or `--address-warnings-first` only: no warning records between the begin and end markers. Not a criterion by default. |

Under either modifier the exit requires ALL criteria to hold in ONE iteration,
which means the phase-2 work must not have reintroduced what phase 1 cleared.
A phase-2 iteration that fixes warnings while an error reappears has not
exited; it has moved back into phase 1.

Measure per-event handling latency, NOT the wall-clock duration of the replay.
The recording preserves the user's think-time, so a replay of a leisurely
reproduction takes just as long as the reproduction did, and long gaps in the
record are the user pausing rather than the system stalling. Compare each
event's handling against the loop runbook's expectations; do not invent a
threshold here, and do not treat the replay's total duration as a metric.

A criterion that holds only because the evidence for it is missing does not
hold. Complete `observability-gaps.md` before declaring an iteration clean.

## Fidelity limits

State these to the user rather than letting a clean replay imply more than it
proves:

- **Webview interactions are not captured.** The GUI runs in a webkit widget.
  Clicks, scrolls, and typing inside it never reach the Emacs command loop, so
  they are absent from the recording and absent from the replay. A
  reproduction that lives inside the webview cannot be recorded by this
  mechanism at all; do not select this runbook for it.
- **Out-of-Emacs events are not captured.** Daemon activity, launchd, vendor
  behavior, and anything the user did outside Emacs are not in the file. The
  replay reproduces the user's half of the interaction only.
- **Replay depends on starting workspace state.** The recording is a sequence
  of key sequences, not a description of intent. Replayed against a different
  workspace roster, a different buffer layout, or a different point in a
  conversation, the same keys drive different commands. Bounce to a comparable
  starting state before each replay.
- **A recording ages.** Rebound keys, renamed commands, and a changed workspace
  roster all make an old file drive something other than what the user did.
  When the replay stops resembling the reported reproduction — unexpected
  commands in the log, failures on events that used to run — tell the operator
  to re-record from step 1 rather than debugging the recording.

Report which of these applied whenever the loop exits clean. "The recorded
sequence runs clean" is a claim about the recorded half of the interaction, and
saying so plainly is the difference between a verified fix and one that merely
was not contradicted.

## Composition

- `iterative-fix-verify-loop.md` owns the bounce, the wait cap, the fanout
  mechanics, the merge discipline, and the latency expectations this runbook
  defers to.
- `structured-logs.md` for resolving sinks and reading the replay window.
- `observability-gaps.md` before declaring any iteration clean.
- `performance-investigation.md` when a per-event slowdown survives and the log
  record cannot localize the time.
