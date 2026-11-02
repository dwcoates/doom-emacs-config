# Flaky tests: where they come from, and what removes them (2026-11-02)

Workspace `flaky-test-elimination`. Covers every agent-repl suite: Go unit,
integration and e2e (the Emacs layer and Doom tier included), vitest, ERT and
the shell harnesses.

## How the evidence was gathered

- **Gate history.** Every merge-queue gate archive under
  `~/.claude-emacs/merge-logs/` (183 test-run archives, 2026-10-30 to
  2026-11-02; older ones are pruned) was parsed for failing cases: Go
  `--- FAIL`, ERT `FAILED n/m`, vitest `FAIL file > test`, script `FAIL`, and
  units that could not start. Each failure was paired with its own message and
  its branch, and with the outcome of the same test in the merge's later
  rounds.
- **Static sweep.** Every test and test-support file was read for time-based
  synchronization (sleeps, sleep loops, polls ending only on a bound) and for
  waits that ignore their dependency's failure.
- **Reproduction.** Suspects with no surviving artifacts were repeated alone
  on the tip with `agent-repld test run -runs N`.

## What the gate history shows

- Most gate failures are DETERMINISTIC: a branch broke a structural check
  (`TestNoPackageHandRollsItsOwnLineSplit`, `TestProductionDiagnosticsDoNotBypassDlog`,
  `layout3Undo has no inverse for migration step N`, the files-under and
  hand-rolled-recorder checks), a proto field the fake daemon did not know
  (`unknown field "queueLabel"`), or a fake daemon that did not build. They
  fail every round until the branch is fixed. Those are not flakes.
- The flakes are waits. Every case that failed across unrelated branches, or
  failed one round and passed the next with nothing changed, was a wait that
  ran out its bound:

  | Test | Branches failed | Cause recorded | State on the tip |
  |---|---|---|---|
  | `TestEmacsKillCloseLeavesTheTabOrderAlone` | 5 | a page booted, was hidden by the next registration's show, never called `AdoptWebWorkspace`; its tab never answered open | root cause fixed (`9592d7de2a`, a lazy import made a Vite chunk cycle and the page hung silently); boot stages now logged (`ccf1815828`) |
  | `TestEmacsTabsRehydrateFromTheRosterOnConnect` | 4 | both pages `load-finished`, neither reported drawn | same signature as above (a page that loaded and never drew); repeated on the tip below |
  | `TestHandoverTransfersAFreeWorkspaceThroughTheAdoptionRendezvous` | 2 | the test read a `not_yet_adopted` gap the successor closes on its own | fixed structurally (`a2919872e8`, a seam holds the adoption for its participants) |
  | `TestASelectOnTheSuccessor...`, `TestAHeadHandedOver...`, `TestDuringAHandover...` | 1 | same gap | fixed by the same seam |
  | `TestTwoMergesOfARepositoryAlternateTheTestTurn...` | 2 | merge chain races | fixed (`3126446013`, `ff10ce334b`, `34d6861399`) |
  | `TestEmacsUnfocusedTurnEndRaisesTheDaemonsBanner` | 1 | a page never reported drawn | open in `org/todo.md`; the drawn report is now at info |
  | `TestDoom/*` (4 steps, one gate) | 1 | `await the workspace ... to appear in Emacs's registry: never satisfied within 5s`, CPU wait under 1s | the registration failed and nothing said so: the wait reported a bound, not the cause |

- Since the scenario consolidation (`c2c46badc5`, 2026-11-01 18:07), 49 gate
  archives show no Emacs-layer flake; every Emacs-layer failure in them is a
  branch's deterministic error (`no models are known yet`, `Cannot split side
  window`).

## The one structural defect behind every flake

**A wait learns that its dependency failed only when its bound runs out.**
Every flake above was a dependency that had already failed (a page that hung,
a registration that was refused, a daemon that took the other branch of a
race) while the test kept polling for the state it wanted. The bound was the
only channel the failure had, so the failure read as "slow", was judged
"load", and was retried.

The harnesses already do the right thing in a few places, which shows the
shape:

| Wait | Ends early on its dependency's failure |
|---|---|
| `daemon/integration/harness/daemon.go` `awaitFile` (the daemon's addr file) | yes: the daemon's exit, with its stderr |
| `daemon/integration/harness/shimctl.go` `connectOrDeparted` | yes: the shim's pid died |
| `e2e/world_test.go` `Store.awaitReady` | yes: the store's exit, with its log tail |
| `e2e/emacs_test.go` `awaitDoom`, `AwaitEvalFor` | yes: Emacs exited or wedged |
| `agent-shim/shim-store/integration` `awaitCondition` | yes: the store exited |
| shim vitest `spawnShim` | yes: the shim exited before serving |

and the wrong thing everywhere else:

| Wait | Ends only on its bound |
|---|---|
| `daemon/integration/harness/logs.go` `awaitLogRecordAfter` and its whole `Await*` family, the serving wait, `AwaitFileGone`/`AwaitFileExists` | the daemon's exit, an ERROR record and an unexpected WARN are seen only at teardown (`assertNoUnexpectedWarnings`) |
| `daemon/integration/harness/shimctl.go` `connect` | bounded by the whole run budget, not one wait; no shim or daemon exit check |
| `e2e/emacs_test.go` `AwaitEvalFor` | an elisp error in the polled form is kept as `lastErr` and polled until the bound |
| `e2e/world_test.go` `awaitBuildReport`, `awaitCursors`; `e2e/doomworld_test.go` `AwaitCall` | the sidecar's, store's or fake daemon's exit |
| `agent-shim/claude/shim-sidecar/integration` every `await*` | the sidecar's exit (`p.done` exists and is not selected) |
| shim vitest `LogTail.record` | the shim's exit, an ERROR record |
| `lisp/test-integration-helpers.el` `agent-repl-itest--wait-until` (about 293 callers) | the fake daemon's death, unless the caller's predicate checks it |
| `vi.waitFor` (54 sites) | anything but its bound |

## Time-based synchronization still in the suites

- Go sleeps standing in for an event (9): `daemon/internal/rollout/spawn_test.go:196,226`,
  `daemon/internal/server/page_test.go:587,910`, `e2e/emacs_test.go:3424`,
  `e2e/emacs_teardown_test.go:300,397`, `e2e/webapplayer_e2e_test.go:1080,1101`.
- TypeScript sleeps (6): `webapp/test/topbar/topbar.test.ts:362`,
  `webapp/test/main-boot-logger.test.ts:120`, `webapp/test/webkit/containment-page.ts:236`,
  `agent-shim/claude/shim/scripts/capture/capture.test.mjs:1037,1050,1063`.
- Elisp (1 sleep, 2 unbounded polls): `lisp/test-daemon.el:2640,2651`,
  `lisp/test-test-helpers.el:539`.
- Shell (3): `bin/test-profile-agent-repl.sh:134,180`, `bin/profile-agent-repl.sh:452`;
  and two blind lock polls in `e2e/sandbox/bin/e2e-sandbox.sh:252,787`.
- About 70 `await new Promise(r => setTimeout(r, 0))` flushes in the webapp
  tests: no wall clock, but each assumes one macrotask drains the work it
  awaits.

## Initialization repeated for things that are not the subject

- Every Go program is already built once per process (`testenv.ProgramOr`).
- Per daemon (per integration test): the prompts copy, the fake webapp dist
  and five fake scripts are rebuilt (`daemon/integration/harness/daemon.go`
  `StartDaemon`); only the fake git is hard-linked.
- Per e2e `NewWorld` (45 call sites against 25 `SharedWorld`): a real store and
  a real sidecar, about 2.7s of a 3.4s step.
- Per Emacs world (10 `NewEmacsWorld`, 6 `NewDoomWorld`): Xvfb, a `cp -a` of
  Doom's `.local`, a full Doom boot, and again a store and a sidecar.
- `e2e`'s `TestMain` builds the fake shim it never uses.
- The webapp integration suite cold-boots once per file (`bootColdOnce`) under
  `isolate: false`.

## The fix

1. **One fault latch per spawned dependency, selected by every wait.** Each
   harness that spawns a process owns a latch that closes, with its cause,
   when the process exits unrequested or writes a record that fails the test
   (ERROR, or a WARN the test did not declare). Every wait selects on it beside
   its event and its bound, so the test fails at the instant the dependency
   failed, naming the dependency's own record. The bound stays as the fallback
   for a hang that writes nothing.
2. **No sleep stands in for an event.** Each site above waits on the event it
   was approximating (a pipe line, a process exit, a subscriber signal, a
   deferred).
3. **A non-subject resource is built once per process**, shared as the
   existing `harness.Suite()`/`SharedWorld` pattern shares the e2e World, and a
   case owns only its own slice.

The rules are in the `agent-repl-tests` skill, "Synchronization: events, never
time".
