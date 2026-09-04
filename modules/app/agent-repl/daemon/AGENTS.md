# daemon/ — the rebuilt agent-repl daemon (Go)

Read `ARCHITECTURE.md` first: the package map, the seams, the conventions.
`integration/SPEC.md` is the integration suite's specification;
`ERROR-ARMS.md` is the ledger of refusal arms not yet landed in the contract.

## Build and test

- `make test` — `go build ./... && go vet ./... && go test ./... -count=1` from
  this directory. Measured 2.2s wall; slowest package 1.21s
  (`internal/gitclient`), slowest test 0.19s (`TestConfigDirForRouting`).
  Nothing in it is parallelized within a package, because at those figures
  per-test parallelism buys nothing measurable.
- `make integration` — the integration suite:
  `TMPDIR=/tmp go test -tags integration ./integration/... -timeout 180s -count=1 -parallel 8`
  (a real daemon subprocess against a fake shim.v1 server, fake git repos and a
  temp state root; the fake shim binary is `integration/fakeshim`).
- **EVERY TEST IN THE INTEGRATION SUITE CALLS `t.Parallel()`, and a new one
  must too.** The suite's fixture is per-test and shares nothing: each test
  gets its own `t.TempDir()` root, its own short socket root under `/tmp`, its
  own `StateDir`, `LockDir` and config roots, an ephemeral loopback port, its
  own fake `git` first on `PATH`, and its own fake-git world keyed on its own
  `*testing.T`. The package globals (`daemonBinary`, `fakeshimBinary`,
  `gitBinary`, `pinnedCheckout`) are written once in `harness.MainAt` before
  `m.Run()` and only read thereafter, and no test sets an environment variable
  or changes directory. Serial execution was therefore a choice, and it cost
  118.3s against 18.4s at `-parallel 8`. A `t.Run` subtest that builds its OWN
  daemon fixture calls `t.Parallel()` too; one that reads state an earlier
  sibling wrote (`register_select_test.go`'s re-registration table) does not.
- `GOTEST_PARALLEL` bounds the concurrency; see the Makefile for the measured
  basis. Do not raise it without re-measuring: every test owns a real daemon
  process plus a fake shim, and at 16 that load pushed ordinary daemon steps
  past `harness.DefaultTimeout` in tests that are green at 8.
  `TMPDIR=/tmp` IS REQUIRED on macOS: `t.TempDir()` otherwise roots the state
  under `/var/folders/...`, and `<state>/sock/<workspace-id>.sock` then exceeds
  the 103-byte unix socket path limit, so the daemon refuses the state root at
  boot before anything else runs.
- Every test process exports `AGENT_REPL_FORBID_VENDOR_CALLS=1`. No test
  ever calls the vendor.
- **Wait bounds are TIGHT, on purpose.** Every wait the harness performs is
  bounded, never a `time.Sleep`. Teamlead run 8 measured 443 passing tests at
  a 0.2s median and 1.7s max wall time each, so a wait that only ever needs to
  observe ORDINARY daemon behavior does not need anywhere near 30s to prove
  itself — and a red test that DOES time out should burn seconds, not 30 of
  them, or a run with a double-digit number of reds turns into a five-minute
  wait for nothing new.

  | bound | value | where | reason |
  | --- | --- | --- | --- |
  | `harness.DefaultTimeout` | 5s | every `harness.Daemon`'s context, unless overridden | ~3x run 8's observed 1.7s max; the shared budget for one daemon process's whole test. RE-MEASURED at `-parallel 8`: the suite's slowest test is 2.84s and its p99 leaf is under 1s, so 5s is still ~1.8x the observed max under the concurrency the suite now runs at |
  | `harness.HandoverChainTimeout` (`Opts.Timeout`) | 15s (3x default) | the handful of tests whose ONE daemon context must span an entire self-reload handover — a merge landing, the rollout trigger, a SECOND real `claude-repld`'s full boot and adoption, and the incumbent's orderly exit, all on the incumbent's own budget rather than a fresh one | structurally two real process lifecycles sharing one budget, not one; run 8 already saw this chain finish inside 1.7s, so 15s is headroom, not a measured need |
  | `harness.ProbeWindow` | 500ms | `harness.ExpectNoPush`, `harness.Daemon.ExpectFileUnchanged` | negative assertions that must wait out a bound rather than an event, so unlike every other row here it is paid IN FULL on a green run, at 27 sites. MEASURED BASIS (`AwaitView` arrival times over the whole suite at `-parallel 8`, 472 samples): p50 0.4ms, p90 5.8ms, p95 47ms, p97 99ms, max 294ms. The 294ms is `commandfile_test.go`'s ingress, which the daemon polls every 250ms and which is itself one of the negative-probe sites; the only slower arrivals in the run were the two gated by the footer's own 1.5s dwell. 500ms is ~1.7x that measured maximum, so it is NOT shrinkable on this evidence — shortening it would make the command-file and handover probes report "nothing came" about a push that was still on its way |
  | `shortTimeout` (integration/support_session_test.go) | 200ms | `TestSessionSurvivesADaemonRestart`-style old-PID-gone probes | a structural "is it already true" check that should fail fast rather than ride the whole test's deadline |
  | inline `context.WithTimeout` (drain_rollout_test.go, the drain-schedule-survives-a-restart test) | 2s | asserting the drain banner does NOT reappear after a restart | an expected-to-time-out negative probe, deliberately tighter than `DefaultTimeout` |

  A wait that needs longer than `DefaultTimeout` gets one of the rows above —
  never a bigger default. Package `-timeout 180s` leaves ample margin over the
  measured 16.5s full-suite wall time at `-parallel 8`, all green (118.3s
  serial), plus build/link time; it existed only as Go's implicit 10-minute
  default before this bound table, which let a run with many reds run
  needlessly long.

  | production window | value | override | why it is overridable |
  | --- | --- | --- | --- |
  | `footer.DefaultMomentaryDwell` | 1500ms | `--footer-momentary-dwell`, `AGENT_REPL_FOOTER_MOMENTARY_DWELL` (the environment beats the flag; a malformed or non-positive value is REFUSED, never ignored), `harness.Opts.FooterMomentaryDwell` | the window is sized for a PERSON to read a momentary status, so it is a real product window and not a bound to be tightened. The two tests whose subject is its RETIREMENT read no clock, so they run the daemon at 150ms — ~25x the measured p90 push arrival, wide enough that the status and its successor stay two separately observed pushes — and cost 0.39s each instead of 1.74s |

## Command line (binding spellings; Go's flag package accepts one or two dashes)

Emacs launches `daemon/bin/claude-repld` with NO argv — state comes from the
environment. Every flag is optional.

| flag | meaning | default |
| --- | --- | --- |
| `--state-dir <dir>` | the state root | `$AGENT_REPL_STATE_DIR`, else `~/.claude-emacs` |
| `--fake` | force the shim's offline scripted SDK (`--fake`) onto every session, and the `-fake` classifier | off |
| `--joining <addr>` | start as the blue-green SUCCESSOR of the incumbent at `<addr>`: bind a fresh port, own no workspace, write `daemon.addr` only once every workspace is adopted | absent = incumbent |
| `--store-socket <uds>` | the store socket passed to every shim | `$AGENT_REPL_STORE_SOCKET`, else `~/.cache/agent-repl/sock/store.sock` |
| `--shim-main <path>` | the shim entry (`agent-shim/claude/shim/dist/main.js`) | resolved from the checkout the binary was deployed from |
| `--node <bin>` | the node binary that runs the shim | `node` on PATH |
| `--webapp-dist <dir>` | the webapp's built assets to serve at `/` | `webapp/dist` in the checkout |
| `--prompts-dir <dir>` | the prompts directory | `$AGENT_REPL_PROMPTS_DIR`, else `modules/app/agent-repl/prompts` in the checkout |
| `--default-config-dir <dir>` | the default account root | the CLI's default (`~/.claude`) |
| `--multi-repo-config-dir <dir>` | the account root for workspaces under `$MULTI_REPO_ROOT` | unset = the default root |
| `--idle-cutoff <duration>` | hibernate a session idle this long | the keep-alive idle cutoff |
| `--pprof <unix path or 127.0.0.1:port>` | opt-in local profiling surface, opened BEFORE any dependency; a wildcard or routable bind is refused, not opened | off |
| `--no-browser` | this daemon has NO external browser: `OpenExternal` answers `no_browser_configured` and nothing is launched. Without it the browser is still absent on a host where neither `$AGENT_REPL_BROWSER_CMD` nor the pinned default launcher exists | off |
| `--feed-tail-retention <rows>` | how many published rows one feed retains for a tail's replay, which is what makes WatchFeed's `token_expired` refusal reachable | `$AGENT_REPL_FEED_TAIL_RETENTION`, else the resolver's `DefaultTailRetention` (4096) |
| `--footer-momentary-dwell <duration>` | how long a MOMENTARY footer status (`interrupted`, `loading`) stands before the daemon's own successor push retires it | `$AGENT_REPL_FOOTER_MOMENTARY_DWELL`, else the resolver's `DefaultMomentaryDwell` (1.5s) |
| `--self-repo <dir>` | override the daemon's own checkout identity, which is what the merge orchestrator's two methods key on | the checkout the binary was deployed from |

## Run and boot order (binding; `cmd/claude-repld`)

`run` performs exactly this sequence, and every step's failure is fatal:

1. the four environment contracts, with `--fake` and `--state-dir` applied over
   them;
2. the state root's layout — every directory created, then the socket path
   budget checked, so an overlong root is refused here rather than at the first
   shim spawn;
3. the log surfaces; the run log's open failure is a BOOT FATAL, because a
   daemon that cannot write its own narrative cannot report what it then does
   wrong;
4. `--pprof`, BEFORE any dependency, so a boot wedged on one is still
   diagnosable through it;
5. the ONE loopback listener, bound FIRST as the boot-exclusivity claim (an
   exclusive kernel lock on `daemon.lock` beside `daemon.addr`); an UNFLAGGED
   second daemon loses there and exits without touching the incumbent's
   listener or its advertisement;
6. `daemon.addr`, written atomically by an incumbent; a `--joining` successor
   DEFERS it and instead writes `joining.addr` where the incumbent that spawned
   it is waiting, and `daemon.addr` is written only once every workspace is
   adopted (the rollout's `WriteDaemonAddr` hook);
7. the state client — `wsm.Open`, or `wsm.OpenReadOnly` for a joining daemon,
   which owns no workspace and must not be a second writer. That handle is
   PROMOTED IN PLACE (`wsm.DB.Promote`) at the successor's first adoption:
   adopting a workspace is the moment it starts writing that workspace's rows,
   and the incumbent stopped writing them at its transfer notice, so the
   one-writer invariant holds across the swap;
8. the component graph, then `boot.Sequence.Run`: adopt the shims whose
   workspace lock is still held (never kill-and-restart), reconcile the intent
   manifest (all four dispositions persisted as faults), restore the holds
   all-or-nothing, close the orphaned turns of the CLIENT-LESS workspaces in one
   transaction each (an adopted workspace's in-flight turns are re-opened by its
   sessionwatcher instead), recover the in-flight merges, and — for a successor
   — `rollout.Controller.Join`;
9. `server.New` behind `server.H2C` on the claimed listener;
10. an orderly exit on SIGINT/SIGTERM: the advertisement is withdrawn, the
    streams are closed, and the state client and the log sinks are closed.

A lock probe that could NOT TELL is never read as free: such a workspace is
neither adopted nor orphan-closed, and the boot report names it.

## Environment (process contracts and test knobs)

| variable | scope | meaning |
| --- | --- | --- |
| `AGENT_REPL_STATE_DIR` | contract | the one state root shared with Emacs, skills and tests |
| `AGENT_REPL_FAKE` | contract | the whole stack's fake mode: shims spawn with `--fake` and the classifier is scripted. `--fake` overrides it; the flag can only turn it ON |
| `AGENT_REPL_FORBID_VENDOR_CALLS` | contract | every vendor exec site refuses (classifier, login pty with the default binary, shim spawn without `--fake`) |
| `AGENT_REPL_OWNED=1` | contract | propagated into every shim so vendor hooks recognize our processes |
| (shim spawn env) | contract | the daemon's OWN environment passed through, with CLAUDE_CONFIG_DIR, AGENT_REPL_OWNED, AGENT_REPL_STATE_DIR, SHIM_BUILD_SHA, AGENT_REPL_SESSION_ID (the HostSessionId, log correlation only) set/overridden; the store socket rides argv — never a curated allowlist |
| `AGENT_REPL_STORE_SOCKET` | contract | the store socket (a flag beats it) |
| `MULTI_REPO_ROOT` | contract | a workspace whose main repo is under it uses the multi-repo account root |
| `AGENT_REPL_SELF_REPO_DIR` | test only | overrides the daemon's own-checkout identity for the merge-method split; the self-reload trigger stays ON (test safety comes from `AGENT_REPL_DEPLOY_SCRIPT` naming a fake deploy script, so landed range → rollout trigger → deploy is assertable end to end) |
| `AGENT_REPL_FAKE_SHIMS` | test only | forces every shim spawn into the shim's offline scripted SDK WITHOUT putting the whole stack in fake mode, so a suite can exercise a REAL vendor call site (the classifier's headless run) against a live session. It can only turn fake ON |
| `AGENT_REPL_HIBERNATE_IDLE_CUTOFF_MS` | test only | compresses the idle cutoff |
| `AGENT_REPL_FEED_TAIL_RETENTION` | test only | compresses the feed's tail retention (a whole number of rows). It BEATS `--feed-tail-retention`. A malformed or non-positive value is a BOOT REFUSAL, never a fall-through to the default |
| `AGENT_REPL_HOLDOUT_WARN_EVERY` | test only | compresses the rollout's never-free holdout warning cadence (a Go duration; the default is ten minutes). A malformed or non-positive value is a BOOT REFUSAL, never a fall-through to the default |
| `AGENT_REPL_LOCK_DIR` | test only | overrides `~/.cache/agent-repl/run` for the kernel-lock probes (the fake shim honors it too) |
| `AGENT_REPL_BROWSER_CMD` | operator/test | the external browser launcher command for OpenExternal |
| `AGENT_REPL_CLAUDE_BIN` | test only | the `claude` binary for the login pty and the real classifier (a fake script in tests) |
| `AGENT_REPL_DEPLOY_SCRIPT` | test only | overrides `bin/deploy-all.sh` for the self-reload trigger |
| `AGENT_REPL_TEST_ALL_SCRIPT` | test only | overrides `bin/test-all.sh` for the merge test gate (invoked as `bash <script> --suites <a,b>` in the merge TARGET worktree; exit 0 = pass; per-suite state parsed from the script's own `<suite>: passed in <N>s` / `<suite> failed after <N>s with exit code <rc>` lines; output archived under `<state>/merge-logs/`) |
| `AGENT_REPL_PROMPTS_DIR` | operator | the prompts directory (the `--prompts-dir` flag beats it) |
| `SHIM_BUILD_SHA` | operator/test | the bundle sha every shim spawn is stamped with. In production it is read from the shim's own build stamp, `agent-shim/claude/shim/dist/.built-sha` beneath the resolved checkout; this variable answers only when that stamp does NOT exist (a checkout that has not built the shim, and every test harness, whose fake shim has no bundle). A present-but-blank stamp is a refusal, never a fall-through. With neither source the daemon REFUSES TO BOOT, naming both — an unstamped spawn cannot be checked for staleness. It has no flag |
| `AGENT_REPL_CHECKOUT` | operator | the agent-repl module root (`modules/app/agent-repl`) the binary was deployed from. It is resolved without this: the executable's own ancestors are walked first, and the path this daemon's source was COMPILED from answers when the binary was built outside the tree (`go build -o <tmp>`, which every test harness does). `--shim-main`, `--webapp-dist` and `--prompts-dir` default beneath it; `proto/vocab/` (the render colors and paint classes) and `daemon/bin/.built-sha` are read from it and have NO flag |

## The `-fake` classifier (deterministic)

A held prompt whose first word is `stop` or `abort` (the explicit-interrupt
fast path, also without `--fake`) or whose text contains `[interject]` is
classified `interject`; every other prompt is `hold_for_turn_end`.

## Kernel locks (shim-held; the daemon only probes)

`~/.cache/agent-repl/run/workspace-<md5hex(clean abs dir)[:8]>.lock` (probed
with `open + flock(LOCK_EX|LOCK_NB)`, released at once) and
`session-<vendor session id>.lock` (never probed by the daemon).

BOTH ARE TAKEN INSIDE `StartSession`, before the SDK is touched, and released
together on a kill or a stand-down (project-lead ruling). Nothing is locked at
the shim's startup, which is what lets the rollout's relaunch engine PRELAUNCH
an inert shim beside a live one: an inert shim holds neither lock. A workspace
another shim already holds answers `StartSessionFailure.conversation_owned`,
which the daemon relays as an intended arm (ERROR-ARMS.md).

## State root layout

See ARCHITECTURE.md "State root layout": `daemon.addr`, `wsm.db`,
`logs/daemon.run.log`, the per-workspace sink targets in `logs/`,
`sock/<workspace-id>.sock`, `intent/manifest.json`,
`output/workspace_commands_*.json`, `merge-logs/`.

## Wiring (wave 3: the graph is complete)

`cmd/claude-repld`'s `buildGraph` builds every component and returns the
server's and the boot sequence's dependencies, the LATE BINDINGS and the
background loops. `graph.go`'s `unwired` list is EMPTY: every Deps field has a
landed producer. The list stays so a future dependency with no producer is
declared there and fails the boot loudly, naming it, rather than being filled
with a stand-in at the composition root.

Two edges point backwards and are closed with FORWARDERS in
`cmd/claude-repld/forward.go`, bound by `run` immediately after `server.New`
and before anything is served: the rollout's and the drain's pushes
(`WorkspacePusher`, `ParticipantSource`, the announcers) and the workspace
verbs' `HostRelay` (`srv.Relay()`). The merge orchestrator's guidance route and
the queue's parked route read the orchestrator out of a forwarder for the same
reason. The background loops — the drain sweep and the command-file ingress —
start after the bindings, because each of them can push.

The one collaborator with NO PRODUCER is the feed's image origin: nothing in
the daemon serves an image reference as a fetchable `src`, so the resolver is
wired with `feed.UnproducedImageResolver`, which refuses loudly and names the
missing producer. `/todos` and `/mcp` have no producer either, and `/agents`
and `/help` are ruled unproduced: `server.Panels` answers `/context` (the
topbar resolver's context tree) and `/status` (the daemon's build stamp plus
the resolver's spliced account/model/mode facts) and fails loudly for every
other recognized panel command.

## Deploy chain

`bin/deploy-all.sh` is the ONE chain, in the order proto → bindings → shim →
webapp → daemon → store/sidecar; its step 5 evaluates
`(agent-repl-runtime-restart-await)` in `lisp/services.el` via emacsclient (the
old `agent-repl-frontend-daemon-restart-await` is dead), and
`bin/build-frontend.sh` builds `daemon/bin/claude-repld` from
`./cmd/claude-repld`. The rollout invokes the same chain with `--no-bounce` and
never a second build path. `agent-shim/wire` is DELETED: nothing in the rebuilt
daemon imports it, and its `bin/test-all.sh` roster entry is gone.

## Logging

Only `internal/dlog`. Every logical branch logs (DEBUG ordinary, WARN
warnings, ERROR errors) with `operation = daemon.<package>.<verb>` and
structured context, per `../logging-contract.md`. Workspace-bound records
go to `<workspace>/.claude/emacs/daemon.log`; failing to resolve the
workspace is an invariant violation, never a global write. That canonical path
is a SYMLINK, and its target is minted under `<state>/logs/`, never the OS temp
dir — the state root owns the daemon's durable logs.

## Conventions

Table-driven tests, Arrange/Act/Assert, one test file per source file, one
edge case per test, no `time.Sleep` for synchronization. GIT IS NEVER CALLED
DURING TESTING (user directive): every package above the git client tests
against a fake `gitclient.Git`; the integration harness scripts every git
fact (commits, conflicts, landed ranges, worktree lists) as fixture data;
the git-client leaf's own tests exercise its one spawn point against a
scripted fake `git` executable placed first on PATH (recording argv/env,
answering from a fixture table); the merge test gate is a scripted fake
script in tests. No `git init`, no temp repositories, anywhere in tests. Unlanded refusal
arms are answered at the transport as `intended arm: <Rpc>Error.<arm>: …`,
logged at WARNING under `daemon.refusal.unlanded_arm`, and recorded in
`ERROR-ARMS.md`.

## Coverage deliberately not attainable under the no-git-in-tests directive

The git client's tests pin argv, env scrubbing, `-C` selection and output
parsing against a scripted fake `git`; they can no longer prove git's OWN
behavior: that a `--no-ff` merge yields a two-parent commit, that the
landed range equals the source branch, that a conflicted merge leaves
unmerged index entries and MERGE_HEAD, that a revert removes the content
in one commit, that `worktree prune` clears a stale registration, the
exact `status --porcelain` markers, that git honors GIT_DIR over `-C`, and
real-git version compatibility (`rev-list --no-commit-header` needs
git >= 2.33). Those are e2e facts now (the project lead's suite).
