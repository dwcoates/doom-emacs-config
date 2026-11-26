# Daemon refactor plan

Status: a living plan. Nothing in it is implemented unless a section says so.
Paths are under `~/.config/doom/modules/app/agent-repl/daemon/` unless they start elsewhere.
Line numbers were read on 2026-11-26 and drift; the named symbol is the anchor.

## 1. Direction

The daemon is split into in-process modules, each with a small public surface that is kept small.
No module becomes a separate process at first.
A module with a small surface can later move behind the same surface into its own program, and one with a wide surface cannot.

### 1.1 Why not separate processes first

- The daemon is about 335,000 source lines in 205 packages, and its coupling is wide: `internal/wsm` is imported by 58 packages, and `internal/server` imports 73.
- A process split made today would keep that coupling and add a socket to it.
- Shared database transactions would become calls that can fail halfway.

### 1.2 Rules every module follows

- One public package plus its own nested `internal/` tree, so the compiler refuses an import of its insides.
- It owns its tables and its schema changes. No other module reads or writes them.
- Two tests pin the boundaries: one holds the allowed import graph between modules, and one holds each module's exported surface in a checked-in file, so a wider surface is a reviewed diff.
- Its recovery after a daemon death is its ordinary boot path over its own tables (section 3).
- Its decisions are returned as values, never only written as log text (item 4.4).

### 1.3 Where the modules come from

Modules follow the mechanisms the daemon carries, not today's package names.
The first step is a mechanism ledger: each mechanism with its code, its tests, its rules in the `AGENTS.md` files, its commit count, and a verdict of remove, replace, move out, or keep.
The ledger is not written yet.

Candidates named so far:

| Candidate | Size | Note |
| --- | --- | --- |
| Merge queue with deploys and pull request watching | about 40,000 lines | the strongest first module, and the first candidate to leave the process later |
| Feed, footer, sidebar and topbar resolvers | about 45,000 lines | |
| Daemon handover | | simplified by the handoff design in section 2 |
| `internal/wsm` | 115 tables under one schema version | split by the module that owns each table |

### 1.4 How a module moves

Moving files conflicts with every open branch, so each module moves in one landing at a quiet moment.
The first cut of a surface will be too wide. Shrinking it afterwards is where the simplification happens.

## 2. Deploy and handoff

These points were settled in discussion and are not implemented.

- A kernel lock decides which daemon serves. The new daemon starts and prepares, the old one exits, and the lock passes when the old process dies. A crash and a deploy take the same path.
- A holder of the listening socket keeps requests queued across the change.
- The new daemon never waits for a turn to end. Shims already write everything to the store, and a daemon reads forward from it.
- Schema and contract landings are add-only by default, so the previous binary can read what the new one wrote and a rollback needs no undo. A cleanup landing that removes something is deliberate, backs the database up first, and replaces every shim. The merge gate refuses a breaking change in any other landing.
- A small supervisor that rarely changes starts the daemon, keeps the current and previous binaries, and rolls back when the new daemon does not prove it serves within a bound. It knows nothing of workspaces, shims or the database.

Gaps this design must close:

- Queue state held only in memory (queued clear and compact acts, the running cut, the cold gate) is written to the database as it changes.
- Long-running daemon work (merges, test runs, fix rounds) is restartable, or leaves the daemon.
- Each mutating request carries an id made by the client, stored in the same transaction as its effect, so a request cut off by a daemon death can be repeated safely.

Not yet verified: whether a shim announces a pending permission or question again to a new daemon; whether each merge step resumes after a daemon death; which mutating verbs already carry such an id.

## 3. Crash recovery is testable inside one process

Today a test of crash or handover behavior kills a whole daemon, because recovery exists only as a second process booting over what the first left.

Requirement on every module that owns durable state:

- Recovery is the module's ordinary boot path, reading its own tables. A unit test seeds those tables and calls it.
- Mid-operation cases use named crash points at the module's transaction boundaries. A test stops the module at a point and boots a second instance over the same database.
- A whole-process kill remains only for a test that asserts an operating system fact: the serving lock passing at death, the socket queue holding requests, a log target staying inside the state directory.

## 4. Design causes that hold tests at the integration tier

A read-only audit of the 549 daemon integration tests judged 381 to be at the wrong tier.
The workspace `daemon-integration-tier-reorg` moves the tests. This section records the design causes, each as a refactor item.
Each claim below was checked against the code on 2026-11-26, and the check's result is stated.

### 4.1 Test-only knobs in production code

Production code reads environment variables that exist only to hold a timing window open for an integration test. Each use costs a private daemon.

- `internal/workspace/watcherhold.go`, `EnvTestHoldFirstWatcherStart`. Confirmed.
- `cmd/agent-repld/main.go`, `envFeedTailRetention` and `envFooterMomentaryDwell`. Confirmed.
- `internal/rollout/api.go`, `EnvTestAdoptAwaitsParticipants`. Confirmed.
- `cmd/agent-repld/graph.go`, `MergeTerminalPauseEnv`. Confirmed, and `integration/merge_shutdown_test.go` writes the variable's name by hand.
- Not confirmed: that a unit test in `internal/promptqueue/submit_test.go` already pins the watcher-start window.

Refactor: each window becomes an injected hook or clock on the module's own constructor, and the environment variable is removed.
Moves down: every test that boots a private daemon only to hold one of these windows becomes a unit test of the owning module.

### 4.2 Turn ordering is composed only in the composition root

`internal/sessionwatcher`, `internal/promptqueue` and the feed resolver meet only where `cmd/agent-repld/graph.go` calls `promptqueue.New` with its `Deps`. Confirmed.
`graph.go` is 3,195 lines, so the only way to exercise the ordering between them is to boot the daemon.
The audit also names one frame fanning out to the feed, footer and banner, and the roster bring-up walk. Those two were not checked.

Refactor: the turn path becomes a module whose constructor takes the store and a shim client, so a test builds the watcher, queue and resolvers in one process without the rest of the graph.
Moves down: ordering tests between a prompt, its turn's frames and what the resolvers publish.

### 4.3 Shutdown order lives in defer order

`cmd/agent-repld/run.go` already names its teardown steps (`teardown.step("state_close", ...)`, `"drain_merges"`, `"server_close"` and others), but their order is the reverse of the order the `defer` statements were registered in, explained by comments. Confirmed.

Refactor: the steps become one ordered list of named steps that `run` executes, with each step's reason beside it.
Moves down: a unit test pins the order, replacing integration tests that shut a daemon down to observe it.

### 4.4 Decisions visible only as log text

Tests boot a daemon to read a log record because the decision is returned nowhere else.

- `internal/workspace/sessions.go`, `Fleet.classifySource`. It does return a `source` value, so the test's reason for reading logs needs a closer look. Partly confirmed.
- `internal/sessionwatcher/watcher.go`, the `turn_open_at_attach` record. Confirmed as log-only, and unit tests in `internal/sessionwatcher/watcher_test.go` already read it, so the integration copy looks redundant.
- The rollout reconcile's disposition per session, the watcher's opening kind, and the merge workspace's owed and covered order. Not checked.

Refactor: each decision is a typed value its function returns, and the log record is written from that value.
Moves down: every test that asserts one of these decisions becomes a unit test on the returned value.

### 4.5 Every merge test pays for a workspace creation

- `internal/merge/orchestrator.go`, `layoutFor`, refuses a workspace that has no creation job. Confirmed.
- `integration/merge_test.go`, `durableQueueOrder`, stops the daemon and runs SQL on `merge_queue` to read the queue's order. Confirmed.
- `integration/prompt_test.go`, `promptMergeFixture`, is the only way to take a merge lease. Exists, cost not measured.

Refactor: the merge module takes the layout as an input to its own surface, and its surface answers the queue's durable order and grants a lease.
Moves down: merge ordering, lease and refusal tests run against the merge module with a seeded database.
This item is a reason to make the merge queue the first module.

### 4.6 Process-wide options force a private daemon

- `IdleCutoffMS` is a boot option, and five integration call sites set it to 50. Confirmed in part: the audit counted seven hibernation tests, and five sites were found after the test moves began.
- `MULTI_REPO_ROOT` and account roots are read only at boot (`integration/session_lifecycle_test.go`). Confirmed for `MULTI_REPO_ROOT`.
- `integration/support_test.go`, `newOpenedOwingSummary`. Exists.
- `integration/scenariogoldens_test.go` gives each scenario its own daemon because its expected warnings are read from everything the daemon recorded. Confirmed.

Refactor: an option that only one workspace needs is set per workspace, and warnings are attributed to a workspace so one daemon serves many scenarios.
Moves down: these tests share the canonical daemon, and the hibernation rule itself becomes a unit test with a clock.

### 4.7 Restarts used to fake session state

A test that needs a cold or cut session restarts the daemon to get one. Not checked against specific tests.

Refactor: session state is constructed from rows, which the recovery rule in section 3 already requires.
Moves down: tests of cold and cut sessions seed the rows and run in one process.

### 4.8 A state that production never writes

The terminal kind `"deleted"` is declared three times (`internal/wsm/sessions.go`, `internal/workspace/register.go`, `internal/server/host.go`) and is read by refusals, including `Fleet.classifySource`.
No write of it was found outside tests in `internal/wsm` or `internal/workspace`. Confirmed by search, not by reading every writer.

Refactor: decide whether the kind exists. Either a production path writes it, or the kind and its refusals are removed. This removes error handling, so it needs the owner's sign-off.
Moves down: the test that corrupts a row and boots a successor daemon is deleted or becomes a unit test.

### 4.9 Branches with no unit test

- `internal/server/admin.go`, `clientLevel`. No test calls it directly. Confirmed.
- `internal/server/refuse.go`, `setArm`, which maps an arm to a response by reflection. No test calls it directly. Confirmed.
- `internal/server/streams.go`, the Emacs arm of `WatchDaemon`. The branch exists; its coverage was not checked.
- The attention clear when an ask settles (`internal/sessionwatcher/route.go` to `internal/workspace/notify.go`). Not checked.

This item is missing tests, not a design cause. It is recorded here so the test work picks it up.

### 4.10 The boot tests were moved only halfway

`cmd/agent-repld/run_test.go` has `runIn`, which runs the daemon's boot inside the test process, and 80 tests. `integration/boot_test.go` still has 18. Confirmed.

Refactor: finish the move, which the ordered boot and teardown lists (4.3) make simpler.
Moves down: the boot refusals that `run_test.go` already proves leave the integration tier.

### 4.11 One act observed by several daemons

The audit names tests that repeat one costly act once per observer: `integration/exit_streams_test.go`, the `CrashBoot` pair, the fresh-boot log observers, four pairs of footer and topbar tests, and two vendor-guard create tests.
`integration/exit_streams_test.go` now holds one test, so that case appears fixed. The `CrashBoot` tests still stand in `integration/session_lifecycle_test.go`. The rest were not checked.

This item is test structure, not daemon design: one act, then each observation as its own named case.

### 4.12 Stale comments and allowances

- `integration/drain_rollout_test.go` still says a test is expected to be red, though `controller.Republish` exists in `internal/drain/controller.go`. Confirmed.
- The audit's stale `daemon.refusal.unlanded_arm` allowance in `integration/health_clientlog_login_test.go` is gone. One allowance of that record remains in `integration/prompt_test.go` and was not judged.

This item is cleanup for the test work, not a refactor item.

## 5. Open points

- The mechanism ledger and the module map (each module, the state it owns, its surface) are not written.
- The unverified points listed in section 2.
- Whether the deploy supervisor is built, and whether cleanup landings get a skill.
