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
  | `harness.DefaultTimeout` | 5s | ONE wait's failure bound: every `Daemon.waitCtx` child, `harness.AwaitView`, and the per-call contexts the e2e suite derives for a single rpc | ~3x run 8's observed 1.7s max. RE-MEASURED at `-parallel 8`: the suite's slowest test is 2.84s and its p99 leaf is under 1s, so 5s is still ~1.8x the observed max under the concurrency the suite now runs at |
  | the `harness.Daemon` context (`DefaultTimeout * runBudgetWaits`) | 35s | the WHOLE-RUN budget one daemon process's test shares, and the lifetime of every watch stream held across it | **it used to be `DefaultTimeout` itself**, so a test's whole run was as short as its single longest permitted wait, and the LAST call in a test answered `deadline_exceeded` for budget the earlier ones had spent. That is what made `TestHostRequestedStopLeavesNoProcessBehind` fail ~1 in 20 at ~5.09s on the stop -- a step measured at 9ms p50 and 15ms max across 104 runs. Sized as five `DefaultTimeout` waits (25s) plus one `drain.DefaultStandBound` stand-down (6s) = 31s, rounded up to the next whole multiple (7 x 5s = 35s); it is a MULTIPLE so an `Opts.Timeout` override widens the run in the same proportion it widens the wait |
  | `harness.HandoverChainTimeout` (`Opts.Timeout`) | 15s wait bound (3x default), 90s run budget | the handful of tests whose ONE daemon context must span an entire self-reload handover — a merge landing, the rollout trigger, a SECOND real `claude-repld`'s full boot and adoption, and the incumbent's orderly exit, all on the incumbent's own budget rather than a fresh one | structurally two real process lifecycles sharing one budget, not one; run 8 already saw this chain finish inside 1.7s, so 15s is headroom, not a measured need |
  | `harness.ProbeWindow` | 500ms | `harness.ExpectNoPush`, `harness.Daemon.ExpectFileUnchanged` | negative assertions that must wait out a bound rather than an event, so unlike every other row here it is paid IN FULL on a green run, at 27 sites. MEASURED BASIS (`AwaitView` arrival times over the whole suite at `-parallel 8`, 472 samples): p50 0.4ms, p90 5.8ms, p95 47ms, p97 99ms, max 294ms. The 294ms is `commandfile_test.go`'s ingress, which the daemon polls every 250ms and which is itself one of the negative-probe sites; the only slower arrivals in the run were the two gated by the footer's own 1.5s dwell. 500ms is ~1.7x that measured maximum, so it is NOT shrinkable on this evidence — shortening it would make the command-file and handover probes report "nothing came" about a push that was still on its way |
  | `shortTimeout` (integration/support_session_test.go) | 200ms | `TestSessionSurvivesADaemonRestart`-style old-PID-gone probes | a structural "is it already true" check that should fail fast rather than ride the whole test's deadline |
  | inline `context.WithTimeout` (drain_rollout_test.go, the drain-schedule-survives-a-restart test) | 2s | asserting the drain banner does NOT reappear after a restart | an expected-to-time-out negative probe, deliberately tighter than `DefaultTimeout` |
  | `loopJoinBound` (cmd/claude-repld/run.go) | 2s | the orderly exit's wait for the background loops, and for the prompt queue's own goroutines, to leave their cancelled serving context | NOT a shutdown budget: every loop is a ticker whose iteration is itself bounded, so this covers one in-flight iteration finishing. An overrun is reported at ERROR and the teardown proceeds, because an unbounded wait is a daemon that does not exit |

  A wait that needs longer than `DefaultTimeout` gets one of the rows above —
  never a bigger default. And a wait's bound is never the run's: `Daemon.Ctx()`
  is the run budget, so anything that hands it straight to one rpc or one poll
  loop gives that call whatever the run happened to have left. Take a
  `DefaultTimeout` child instead. Package `-timeout 180s` leaves ample margin over the
  measured 16.5s full-suite wall time at `-parallel 8`, all green (118.3s
  serial), plus build/link time; it existed only as Go's implicit 10-minute
  default before this bound table, which let a run with many reds run
  needlessly long.

  | production window | value | override | why it is overridable |
  | --- | --- | --- | --- |
  | `footer.DefaultMomentaryDwell` | 1500ms | `--footer-momentary-dwell`, `AGENT_REPL_FOOTER_MOMENTARY_DWELL` (the environment beats the flag; a malformed or non-positive value is REFUSED, never ignored), `harness.Opts.FooterMomentaryDwell` | the window is sized for a PERSON to read a momentary status, so it is a real product window and not a bound to be tightened. The two tests whose subject is its RETIREMENT read no clock, so they run the daemon at 150ms — ~25x the measured p90 push arrival, wide enough that the status and its successor stay two separately observed pushes — and cost 0.39s each instead of 1.74s |

### An answer has left when the socket took it

`server.WriteBarrier` (`internal/server/writebarrier.go`) is what both the
daemon's orderly exit and the integration fake shim's stand-down wait on before
ending the process. READ ITS DOC BEFORE REACHING FOR A SIMPLER SIGNAL: neither
"the handler returned" nor "the request's context ended" means the answer has
been written, because `golang.org/x/net/http2`'s `serverConn.runHandler` cancels
that context in a defer that runs BEFORE the same defer produces the response's
final frames. Both beliefs have been in this tree and both cost a run.

| window | value | why |
| --- | --- | --- |
| `server.barrierSettle` | 1ms | how long the barrier lets pass with no write completing before it calls a connection quiet. NOT a delay: the loop ends on the condition and the ordinary case spends two of these. What it covers is one `write(2)` of a few dozen bytes onto a unix or loopback socket by an already-runnable goroutine — microseconds — so it is three orders of magnitude over the work |
| `server.answerWriteBound` | 250ms | how long ONE answer has to reach the socket after its handler's goroutine has finished with it. A last resort, deliberately well under the exit's own 2s grace so a single stuck answer cannot spend all of it |
| `fakeshim.killAnswerWriteBound` | 250ms | the same last resort on the fake's `KillSession` exit, far under the daemon's stand-down window so an overrun reads as a fake that took a moment rather than one that hung |
| `writesQuietBound` (cmd/claude-repld/run.go) | 250ms | how long the exit gives the CONNECTIONS to stop writing once every counted call has been answered. `Serving.counted` skips `standingStreamPaths` on purpose, so `AwaitQuiet` sees nothing in flight while the daemon's LAST PUSH — `DaemonShutdownAnnounced`, sent on every WatchDaemon stream one line before `drain.fire` calls Exit — is still on its way to the socket. Same sizing and same last-resort role as `answerWriteBound`: the barrier ends on quiescence, so the ordinary exit spends about two `barrierSettle` ticks here |

The barrier settles true on quiet OR on a connection that is still writing past
the mark — one h2 connection multiplexes the standing pushes with the unary
calls, and calling a busy link a lost answer would put an ERROR record, and a
warning-sweep failure, against a healthy run. FALSE means one thing: nothing at
all was written since the mark.

### The shim link's requests own the bytes they promise

`shimclient.ownedRequestBody` (`internal/shimclient/transport.go`) copies a
declared-length request body inside `RoundTrip`, and it is not defensive. Over
HTTP/2 `Do` returns on the response HEAD while the body is still being written,
connect-go releases its pooled payload the instant `Do` returns, and the shim
answers a streaming head THE MOMENT IT ACCEPTS THE STREAM — so the daemon
announced `content-length: 5` and then closed the stream with an empty DATA
frame, which nghttp2 correctly RST_STREAMs with PROTOCOL_ERROR. Four to seven
e2e tests per sandbox run read that as a severed shim link.

Do not "fix" a peer's reset by retrying past it: connect-go's streaming request
body is a pipe that cannot be replayed, and a request that contradicts its own
length must not be sent at all.

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
    streams are closed, the BACKGROUND LOOPS and the PROMPT QUEUE's own
    goroutines are joined (both bounded by `loopJoinBound`, 2s, and an overrun
    is reported, never waited on), and then
    the state client and the log sinks are closed. The join is registered last
    so it runs FIRST of every teardown: a loop's iteration reads and writes the
    state client off its own goroutine — the drain sweep's hibernation releases
    the lease and then tells the prompt queue — and a SIGTERM landing inside one
    left `daemon.promptqueue.lease_changed: could not read the standing holds —
    sql: database is closed` in the log of an ORDERLY exit. The queue's
    asynchronous classification verdicts and background revivals are the same
    class (`daemon.promptqueue.tray` / `daemon.promptqueue.classify` on a closed
    database) and are joined through `promptqueue.Queue.Drain`.

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

The feed's image origin IS produced, by `internal/imageorigin` mounted at
`/feed-images/`: `feed.PathImageResolver` registers a prompt's
`ImageBlock{path}` with it and draws the origin's URL. Registering is the only
way a path becomes servable, so the origin serves exactly the images some
conversation carried. The SAME resolver is required by `promptqueue`, which
mirrors the same row live -- both draws go through `feed.DrawUserBlocks`, and a
mirror that drew fewer blocks than the resolver is what once made an attached
image invisible for a whole live session. `/todos` and `/mcp` have no producer,
and `/agents`
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

`internal/dlog` owns the daemon's logging function and exposes `Logger.Debug`,
`Logger.Info`, `Logger.Warn`, and `Logger.Error`. Every logical branch records
`operation = daemon.<package>.<verb>` plus structured context, per
`../logging-contract.md`. `AGENT_REPL_LOG_LEVEL` selects the minimum persisted
and terminal-mirrored level (`debug`, `info`, `warn`, or `error`) and defaults
to `info`; any other value is a boot error.

Global records land in `<state>/logs/daemon.run.log`. The run log appends
across process restarts and rotates at 64 MiB through
`agentrepl/logging.OpenRotating`, retaining `logging.DefaultBackups`
generations. Workspace-bound daemon records go to
`<workspace>/.claude/emacs/daemon.log`; the shim writes `shim.log` through its
inherited descriptor, and forwarded webapp and sidecar records go to
`webapp.log` and `sidecar.log`. Each canonical workspace path is a symlink to
a daemon-owned target under `<state>/logs/`. Failing to resolve a workspace is
an invariant violation, never a global write.

`daemon.log`, `webapp.log`, and `sidecar.log` rotate synchronously at 64 MiB
through `agentrepl/logging.OpenRotating`, retain `logging.DefaultBackups`
generations, and atomically refresh their canonical symlink after each roll.
An already-open reader remains on the retired inode. `shim.log` cannot rotate
under the shim because descriptor `3` is inherited and the shim never receives
a path. The cap scanner therefore marks it at 64 MiB; `ShimSink` rotates the
marked target when the next replacement shim is prelaunched. At 110% the
daemon records one workspace error and sends a `ShimRollRequest` to the
freeness-aware `rollout.Controller`, which forces that process roll at the next
turn boundary. Reaching either threshold is not sink poison; failures while
checking or rotating are.

`logging_bypass_test.go` is the bypass lint. It fails any direct `fmt.Print*`,
`log.Print*`, or `os.Stderr.Write*` call in production code outside the
explicitly counted bootstrap, terminal-mirror self-report, and injected CLI
reporting sites. New diagnostics go through `internal/dlog`, never by growing
that allowlist.

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

## A BROWSER page holds ONE stream; Emacs holds its own

Every standing watch a webview holds is a SUBSCRIPTION on that page's single
`WatchPage` stream (`internal/server/page.go`). Emacs is unaffected: it dials
the dedicated `WatchDaemon`, `WatchHostWorkspace` and `WatchWorkspaceRoster`
rpcs directly, and those are unchanged.

THE REASON IS THE BROWSER'S, NOT THIS DAEMON'S. This daemon serves h2c, but no
browser negotiates cleartext HTTP/2, so a webview talks HTTP/1.1 — which caps
a page at SIX connections per host. Measured against this daemon in the e2e
sandbox: standing streams 1-6 arrived within 9ms and were logged as accepted;
streams 7, 8 and 9 produced NO record at all, and a plain same-origin `GET`
issued while six were held timed out in the browser after 5s. The webapp held
six panel watches before opening its feed tail, so the tail QUEUED forever and
the root feed never drew a live row.

Two rules follow for anything added here:

- **A NEW STANDING STREAM THE WEBAPP WILL HOLD NEEDS AN ARM IN
  `SubscribePageRequest` AND `PageFrame`, NOT JUST AN RPC.** A dedicated rpc a
  page dials directly is one more connection against a budget of six, and the
  page cannot know which of its watches will be the one over the line.
- **A WATCH BODY WRITES TO `streamSink[R]`, NEVER TO A `connect.ServerStream`
  DIRECTLY.** That is what lets the SAME body serve the dedicated rpc and the
  page mux — the subscription invariant, `WatchFeed`'s token pin, the
  acceptance and the refusals are one implementation rather than two kept in
  step by hand. `*connect.ServerStream[R]` satisfies the interface as it
  stands.

Acceptance for a muxed subscription is `SubscribePage`'s own ANSWER: the body
signals through the notifier on its context (`acceptStream`, `accept.go`) at
exactly the point a dedicated stream flushes status 200, and the unary is
withheld until it does. So a view published after `SubscribePage` returns
cannot be missed, which is the same guarantee the header flush gives.

## Production stop bounds NEST; they are never equal

A stop is a SEQUENCE of promises, and each outer one must strictly contain
every inner one with room left over for what follows it. Two bounds that
happen to carry the same number are two different promises colliding: the
outer can never observe what the inner does, so it gives up at the exact
instant the inner one acted and reports the thing it was still doing as
leaked. `drain.DefaultStandBound` and `shimclient.defaultKillGrace` were both
5s, set independently, and that is exactly the defect they produced on the
GRACEFUL path -- the idle sweep's `KillSession(..., false)`.

The graceful stand-down's nesting, outermost first:

| bound | value | contains | derivation |
| --- | --- | --- | --- |
| `drain.DefaultStandBound` | 6s | the whole `KillSession(..., false)`: the shim's own teardown inside the rpc, then the process stop | `shimTeardownWorstCase` (4s) + `shimclient.GracefulKillBound` (1.5s) + `standBoundMargin` (0.5s). A SUM of what it contains, never a round number chosen next to them |
| `shimclient.GracefulKillBound` | 1.5s | one graceful `Client.Kill`, end to end | `DefaultKillGrace` + `EscalationBound` |
| `shimclient.DefaultKillGrace` | 1250ms | the shim's SIGTERM stand-down before the SIGKILL | the shim's own single-stage last resort (`WATCHER_CONCLUSION_BUDGET_MS`, 1s) + 250ms. MEASURED: a healthy shim leaves on SIGTERM in 4.24ms p50 / 6.69ms max (real Node shim, 20 spawns) and 0.61ms p50 / 0.71ms max (the integration fake, 30 spawns), so this is ~187x the observed worst case |
| `shimclient.EscalationBound` | 250ms | the SIGKILL and the exit decode after it | measured sub-millisecond on every kill in the package suite; two orders of magnitude of headroom |

Rules that fall out of it, and that a change to any of these numbers must keep:

- **A wait takes a context, and the context is honoured.** `Client.Kill` took
  none and blocked unconditionally on the reap, which made every bound its
  callers held a fiction. Its two waits -- the grace, and the wait for the exit
  decode -- both select against ctx now.
- **A caller's bound expiring escalates; it never abandons.** "Your time is up"
  cannot mean "leave a SIGTERMed shim standing", so a ctx that ends inside the
  grace sends the SIGKILL before it returns, and its error names the escalation
  and the cause.
- **The reap is not cancellable, and that is not the same as its wait.**
  `cmd.Wait` runs on the client's own reaper goroutine, owned by nothing a
  caller holds. ctx ends this daemon's WAIT for the wait status, never the
  collection of it -- which is the only reason an abandoned kill leaves no
  zombie behind.
- **`now` FORCES**, so a forced kill skips the grace and pays only
  `EscalationBound`. That derivation is separate and is not sized by the table
  above.

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
