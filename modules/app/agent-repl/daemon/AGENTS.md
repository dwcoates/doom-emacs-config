# daemon/ — the rebuilt agent-repl daemon (Go)

Read `ARCHITECTURE.md` first: the package map, the seams, the conventions.
`integration/SPEC.md` is the integration suite's specification;
`ERROR-ARMS.md` is the ledger of refusal arms not yet landed in the contract.

## Build and test

- Every target below runs under `$(BACKGROUND)` (`../bin/background.sh`), at
  background priority; see the module AGENTS.md. The integration harness
  refuses a run without that helper's marker, so a raw
  `go test -tags integration` must be prefixed with `../bin/background.sh`.
- `make test` — `go build ./... && go vet ./... && test -z "$(gofmt -l .)" && go test ./... -count=1`
  from this directory. Measured 2.2s wall; slowest package 1.21s
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
  **Every run lives under ONE owner-locked run root** (`/tmp/arrun*`,
  `integration/harness/runroot.go`): the built binaries, every state root,
  and every `t.TempDir` (the harness points `TMPDIR` into it). The run holds an
  flock on `<root>/owner.lock` for its whole life, and the kernel drops it
  however the run ends, so the NEXT run reclaims a dead run's root and SIGKILLs
  whatever is still running out of it — a `-timeout` panic or a SIGKILL skips
  every defer and `t.Cleanup`, and before this a day of such runs filled the
  disk and left daemons spinning. The root is under `/tmp` and short, so the
  103-byte socket path budget holds without a `TMPDIR=/tmp` override.
- **A daemon's teardown FREEZES its process group before killing it**
  (`harness.Daemon.Kill`): SIGSTOP to the group, the kernel's own report that
  the leader is stopped, then SIGKILL. A bare `kill(-pgid, SIGKILL)` is walked
  member by member and can be preempted between them, so under load the
  daemon outlived its in-flight git (or a shim still in its group between fork
  and setpgid) and logged that death at ERROR, failing the warning sweep on a
  record the teardown itself caused. Never add a signal path to the harness
  that lets a member of the daemon's group die while the daemon can run.
  Reclaim tests make their dead roots in a private `runRootSpace`, never in
  `hostRunRoots`, which every other run on the host reclaims.
- **The suite bounds its own load**: `harness.DefaultDaemonSlots` (8,
  `AGENT_REPL_ITEST_DAEMON_SLOTS`) top-level tests hold a live daemon at once,
  whatever `-p`/`-parallel` the run was invoked with, because
  `go test -tags integration ./...` otherwise runs up to `nproc x nproc`
  daemons and every wait bound above was measured at eight.
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
  | `boot.DefaultAdoptBound` | 10s | `AGENT_REPL_BOOT_ADOPT_BOUND` (malformed or non-positive is REFUSED, never ignored), the suite runs it at 300ms | THE BOUND THAT KEEPS THE DAEMON SERVING AT ALL. The listener is bound and daemon.addr published at boot steps 5 and 6 and `http.Server.Serve` is not reached until the reconciliation has finished, so every instant one workspace's adoption spends is an instant the kernel is queueing connections onto a socket nobody accepts on. `shimclient.bringUp` redials a shim whose lock reads HELD forever — correctly, a held flock is a living process — so a survivor whose socket path is gone never resolves: pid 31984 listened for TEN HOURS with its accept queue at 128/128. A healthy adoption is a local AF_UNIX connect (itself bounded by `shimsocket.DialTimeout`, 2s) plus the shim's first pushed diagnostics frame, so 10s is ~5x one bounded connect. An overrun is ERROR and the workspace is UNDETERMINED — neither adopted nor orphan-closed — and the boot goes on |
  | `bootStallBound` (cmd/claude-repld/run.go) | 90s | `hooks.BootStall` | the LAST RESORT on the WHOLE reconciliation, for a step whose own bound somebody forgot. Its expiry dumps every goroutine into the run log at ERROR and ends the process, because Emacs respawns a daemon that exited and cannot tell one that is listening and dead from a healthy one. Sized above the worst LEGITIMATE boot: a handful of workspaces adopted serially at `boot.DefaultAdoptBound` apiece plus the manifest, the holds, the orphan closes and the merge recovery, all local sqlite work in milliseconds. Never paid on a healthy boot |
  | `workspace.DefaultStartSessionBound` | 60s | `AGENT_REPL_START_SESSION_BOUND` (malformed or non-positive is REFUSED, never ignored), the integration suite runs it short | ONE `StartSession` call, and the LAST step of the bring-up that had no bound at all: the dial ladder is bounded, an adoption is bounded by `boot.DefaultAdoptBound`, and then the rpc that actually starts the session could take forever -- holding the workspace's START GATE, and so every other route to starting that workspace, with it. MEASURED, realtest run 2026-09-13T16:20:34: workspace `2b81f45a724642ef`'s shim accepted the start, logged `shim.convert.hooks: a hook blocked the gated action` on `SessionStart:resume`, and never answered; three daemon generations sat in that call for 35s, 3m30s and 8m30s, each ended only by the NEXT deploy's SIGTERM. It is a PRODUCT window, not a bound on the daemon's own work -- what it covers is the shim's vendor resume plus whatever `SessionStart` hooks the user configured, which are arbitrary commands -- so it is sized the way the adopt bound is: every healthy bring-up in that same run (spawn, ready, StartSession and the watcher, end to end) ran 116ms, 245ms and 254ms, and 60s is ~240x the slowest of them. An overrun is ERROR naming the unanswered call, and the bring-up fails rather than waiting |
  | `server.acceptBackoffFloor` / `acceptBackoffCeiling` | 5ms doubling to 1s | none | how long `server.RetryAccept` waits between retries of a TRANSIENT accept failure (EMFILE, ENFILE, ENOBUFS, ENOMEM, ECONNABORTED, ECONNRESET, EINTR, EAGAIN — named explicitly, never read off the deprecated `net.Error.Temporary`). Nothing waits here unless Accept actually failed; what the ceiling covers is descriptor exhaustion, where retrying at full speed spins a core and writes a record per attempt without making a descriptor available. `net.ErrClosed` is the daemon's OWN shutdown closing the listener: DEBUG and returned, never an ERROR against a clean exit |
| `drain.HibernateRetryCeiling` | one sweep cadence doubling per consecutive failure, capped at 1h | none | how long the idle sweep waits before asking a workspace whose hibernation FAILED (a failed or unreadable directive, a warned refusal, a failed stand-down or record) again. An ordinary deferral (lease held, turn in flight, compaction under way, session gone) is not backed off and resets it. Before it, a failure was retried on every pass forever, and the pass cadence is the idle cutoff when that is shorter than 5m: an orphaned integration daemon at a 50ms cutoff re-ran the directive, the lease round trip, the host republish and an ERROR twenty times a second |
| `stateRootCheckEvery` / `stateRootCheckCeiling` (cmd/claude-repld/rootwatch.go) | 1s; a check that cannot tell backs off doubling to 1m | `hooks.StateCheckEvery` | how often a SERVING daemon verifies what it owns on disk (`daemonaddr.Claim.Verify`): the state root is the directory it bound in, `daemon.lock` is the inode its flock holds, and `daemon.addr` (while published) names its address. A loss is ERROR `daemon.cmd.state_root` naming what vanished, the serving lifetime ends, and the process exits non-zero with `stood down: <loss>`. A daemon whose root is deleted otherwise serves on forever, and a second daemon booting on a recreated root takes a `daemon.lock` inode the first one's flock does not conflict with. The integration suite measures the exit at ~1.1s |
| `rootLossShimStopBound` (cmd/claude-repld/shimstop.go) | `drain.DefaultStandBound`, per forced stop | none | on a STATE-ROOT-LOSS stand-down ONLY (`standDownReason` `standDownStateRootLost`), the daemon force-stops every shim it supervises before exiting: each held session through `Fleet.KillSession(force)` (walked from `Fleet.Workspaces`, the in-memory map, never the store), then `Supervisor.StandDownEverySpawn` for spawns still in flight, the latch raised first; each stop gets this bound. Nothing can adopt a shim whose root is gone, because adoption goes through the store and state under that root. The outcome is INFO `daemon.cmd.state_root` with `stopped`, or ERROR naming every shim that would not stop. An orderly exit (`standDownOrderly`: drain deadline, signal) and a handover (`standDownHandover`) leave the shims for adoption, unchanged. The integration suite holds the fakeshim's kernel exit event inside 200ms of the daemon's exit |
| `rollout.advertiseBackoffCeiling` / `advertiseRefusedBound` | 25ms doubling to 1s; ERROR once after 30s | none | a SUCCESSOR's retry of `daemon.addr` while the incumbent still holds the boot claim. Only `daemonaddr.ErrClaimed` is retried; any other failure is ERROR and ends the retry, and the retry ends with the serving lifetime |

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
   adopted (the rollout's `WriteDaemonAddr` hook). Its payload is the bare
   `host:port` on the first line and `pid=<n>` — this daemon's process id — on
   the second (`daemonaddr.ParseAdvertisement` / `ReadAdvertisement`); a reader
   that needs only the address takes the first line, which is exactly what a
   legacy daemon wrote, and a file with no `pid=<n>` line reads as pid-unknown.
   The pid lets Emacs retire an advertisement whose advertiser is dead without
   a dial — a predecessor that died without withdrawing is ABSENT, not a
   transport fault. See `logging-contract.md` "Daemon address advertisement";
   `joining.addr` stays a bare address;
7. the state client — `wsm.Open`, or `wsm.OpenReadOnly` for a joining daemon,
   which owns no workspace and must not be a second writer. That handle is
   PROMOTED IN PLACE (`wsm.DB.Promote`) at the successor's first adoption:
   adopting a workspace is the moment it starts writing that workspace's rows,
   and the incumbent stopped writing them at its transfer notice, so the
   one-writer invariant holds across the swap;
8. the component graph, then `boot.Sequence.Run`: BIND every registered
   workspace whose directory exists on the per-workspace resolvers
   (`workspace.Verbs.BindViews`, which writes nothing, so a joining successor
   runs it too) BEFORE any step can raise or close a fault, because a footer,
   topbar or hold-tray record for an unbound workspace is an invariant
   violation (stated once per workspace at ERROR `daemon.<resolver>.unbound_workspace`);
   a manifest entry for a workspace the registry no longer holds or whose
   directory is gone opens no fault at all, only an INFO record; CLOSE every open workspace
   whose directory is gone (a row naming a path that is not there is a tab
   Emacs cannot serve; counted as `missing_dir_closed`, and a stat that does
   not say "not exist" is never read as gone), adopt the shims whose
   workspace lock is still held (never kill-and-restart, and EVERY survivor is
   dialled concurrently so one adoption bound covers the whole boot), reconcile the intent
   manifest (all four dispositions persisted as faults; a manifest is CONSUMED
   ONCE — removed as soon as every disposition it names is durably recorded,
   kept only while a record failed or is deferred behind a read-only handle,
   and an incumbent removes any leftover before it spawns a successor, so no
   later boot and no joining successor ever reads an earlier bounce's intent),
   restore the holds
   all-or-nothing, close the orphaned turns of the CLIENT-LESS workspaces in one
   transaction each (an adopted workspace's in-flight turns are re-opened by its
   sessionwatcher instead, and any the shim's `turn_in_flight` no longer names
   is closed orphaned at INFO), recover the in-flight merges, and — for a successor
   — `rollout.Controller.Join`. A JOINING SUCCESSOR RECONCILES NOTHING, so the
   missing-directory close is also done by `verbs.PublishRegistry`, the walk
   that publishes the opening roster;
9. `server.New` behind `server.H2C` on the claimed listener;
10. an orderly exit on SIGINT/SIGTERM: the advertisement is withdrawn (only
    while it still names THIS daemon's address, so a handover successor's
    advertisement survives its predecessor's exit; recorded at INFO either
    way, and a boot that finds an address nobody answers records the stale one
    at WARN before overwriting it), the
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

### A SPAWNED SHIM'S PID IS DURABLE IN THE REGISTRY FROM THE INSTANT IT IS SPAWNED

The two kernel facts do not cover the STARTING WINDOW. A shim takes its
conversation locks inside `StartSession` and Node binds its socket ~110ms after
the fork, so between those instants a spawned shim holds no lock and answers no
dial -- indistinguishable, to both kernel probes, from a workspace nothing ever
served. Measured 2026-09-13: shim spawned at T+0, its daemon SIGKILLed at
T+60ms, the successor booting at T+65ms with `daemon.boot.adopt` "no shim
survives for this workspace", spawning its own at T+80ms; the survivor bound at
T+110ms and the newcomer died at T+190ms with `shim.main.fatal:
<state>/sock/<ws>.sock already has a live listener; refusing to start a second
shim on one session socket`.

So the spawn itself is recorded:

- `workspaces.spawned_shim_pid` (layout 8, `wsm.SetSpawnedShimPID`) is written
  the INSTANT the fork returns, through `shimclient.Spec.Spawned` -- a callback
  the supervisor invokes between `cmd.Start` and the bring-up it then blocks
  on. Every spawn site supplies it: the fleet's bring-up and the rollout's
  `Prelaunch`. It is NOT `sessions.shim_pid`: that one names the shim SERVING A
  SESSION, and a registered workspace whose first shim is being forked has no
  session row to write at all.
- It is CLEARED when this daemon knows the process is gone: a failed start's
  stop (`workspace/failedstart.go`) and the drain sweep's hibernation.
- `boot.sequence.adopt` and the fleet's `bringUpClient`, on LOCK FREE + NO LIVE
  SOCKET, read it before concluding no shim survives. A pid that is dead or
  absent proceeds as before (the socket path is cleared and a shim is spawned).
  A pid that is ALIVE is waited out by `internal/startingshim`, bounded by the
  same adoption bound and driven by an injected clock rather than a sleep,
  until the socket goes live -- and the survivor is then taken through the
  ORDINARY inert-survivor adoption, on whichever generation of the path
  answered.
- The records are one INFO for the wait (pid and bound), one INFO for the
  announcement, and one ERROR only when the bound expires. An expired bound is
  UNDETERMINED, exactly as an unreadable lock is: a live process that may bind
  the path at any instant is what a second shim must not race.

A predecessor's shim is not this daemon's own spawn, so
`refuseAdoptingOurOwnSpawn` does not fire on it: the supervisor's `SpawnedFor`
ledger is the in-memory `held` set of one process and holds nothing across a
restart.

### A shim that dies on its own is brought back

An unordered departure with no bounce registered revives the session at once
(`promptqueue.reviveAfterDeath`, the prompt's own revival): same vendor session,
a prompt sent meanwhile joins it. The cut turn ends as `query_died`. One
unattended revival until a turn ends; a second death is WARN and left down.

## Environment (process contracts and test knobs)

| variable | scope | meaning |
| --- | --- | --- |
| `AGENT_REPL_STATE_DIR` | contract | the one state root shared with Emacs, skills and tests |
| `AGENT_REPL_FAKE` | contract | the whole stack's fake mode: shims spawn with `--fake` and the classifier is scripted. `--fake` overrides it; the flag can only turn it ON |
| `AGENT_REPL_FORBID_VENDOR_CALLS` | contract | every REAL vendor exec site refuses (classifier, login pty with the default binary). A SHIM SPAWN IS NOT REFUSED: the shim is our own process and has a fake mode, so the guard FORCES `--fake` on every spawn instead (`shimclient.fakeMode`) and a guarded daemon still creates, forks and prompts workspaces against the scripted SDK |
| `AGENT_REPL_OWNED=1` | contract | propagated into every shim so vendor hooks recognize our processes |
| (shim spawn env) | contract | the daemon's OWN environment passed through, with CLAUDE_CONFIG_DIR, AGENT_REPL_OWNED, AGENT_REPL_STATE_DIR, SHIM_BUILD_SHA, AGENT_REPL_SESSION_ID (the HostSessionId, log correlation only) set/overridden; the store socket rides argv — never a curated allowlist |
| `AGENT_REPL_STORE_SOCKET` | contract | the store socket (a flag beats it) |
| `MULTI_REPO_ROOT` | contract | a workspace whose main repo is under it uses the multi-repo account root |
| `AGENT_REPL_SELF_REPO_DIR` | test only | overrides the daemon's own-checkout identity for the merge-method split; a landing still runs its ONE deploy (test safety comes from `AGENT_REPL_DEPLOY_BUILDER` naming the harness's fake build, so landing → deploy is assertable end to end) |
| `AGENT_REPL_FAKE_SHIMS` | test only | forces every shim spawn into the shim's offline scripted SDK WITHOUT putting the whole stack in fake mode, so a suite can exercise a REAL vendor call site (the classifier's headless run) against a live session. It can only turn fake ON. It is NOT needed merely to get fake shims under the vendor guard -- the guard implies them |
| `AGENT_REPL_HIBERNATE_IDLE_CUTOFF_MS` | test only | compresses the idle cutoff |
| `AGENT_REPL_FEED_TAIL_RETENTION` | test only | compresses the feed's tail retention (a whole number of rows). It BEATS `--feed-tail-retention`. A malformed or non-positive value is a BOOT REFUSAL, never a fall-through to the default |
| `AGENT_REPL_HOLDOUT_WARN_EVERY` | test only | compresses the rollout's never-free holdout warning cadence (a Go duration; the default is ten minutes). A malformed or non-positive value is a BOOT REFUSAL, never a fall-through to the default |
| `AGENT_REPL_WORKTREE_REAP_IDLE` | operator | the landed-worktree reaper's idle threshold (a Go duration; default `24h`): a worktree with any sign of activity newer than this is never judged. A malformed or non-positive value is a BOOT REFUSAL. See "The landed-worktree reaper" |
| `AGENT_REPL_WORKTREE_REAP_START_DELAY` / `AGENT_REPL_WORKTREE_REAP_EVERY` | test only | compress the reaper's schedule (defaults `5m` after start, then `24h`). Same refusal rule |
| `AGENT_REPL_LOCK_DIR` | test only | overrides `~/.cache/agent-repl/run` for the kernel-lock probes (the fake shim honors it too) |
| `AGENT_REPL_BROWSER_CMD` | operator/test | the external browser launcher command for OpenExternal |
| `AGENT_REPL_CLAUDE_BIN` | test only | the `claude` binary every one of the daemon's OWN calls execs: the login pty, and `internal/headless`'s runs (the classifier's routing question and the workspace naming call). A fake script in tests. Naming it EXPLICITLY is also what makes those spawns legal under `AGENT_REPL_FORBID_VENDOR_CALLS`: the guard refuses only the bare default `claude`, since an explicit path is by definition not a call to the real CLI |
| `AGENT_REPL_DEPLOY_BUILDER` | test only | ONE executable a deploy runs as `<exe> --out <staging>` in place of the real build (`make -C proto all`, then `bin/build-frontend.sh --out <staging> <target>` per target). The integration harness's fake stages what runs, a stale component, or a failure (`integration/harness/deploybuild.go`) |
| `AGENT_REPL_LAUNCHCTL` | operator/test | the launchctl a deploy's store/sidecar restart drives (default: `launchctl` on PATH); the harness points it at a recorder so no test reaches launchd |
| `AGENT_REPL_LAUNCH_AGENTS_DIR` | operator/test | where the services' plists are installed (default `~/Library/LaunchAgents`); a store restart bootstraps the sidecar back from it |
| `AGENT_REPL_TEST_ALL_SCRIPT` | test only | overrides `bin/test-all.sh` for the merge test gate (invoked as `bash <script> --suites <a,b>` in the merge TARGET worktree; exit 0 = pass; per-suite state parsed from the script's own `<suite>: passed in <N>s` / `<suite> failed after <N>s with exit code <rc>` lines; output archived under `<state>/merge-logs/`) |
| `AGENT_REPL_PROMPTS_DIR` | operator | the prompts directory (the `--prompts-dir` flag beats it) |
| `SHIM_BUILD_SHA` | operator/test | the shim build stated for a checkout with NO bundle on disk. In production every spawn is stamped with the CONTENT HASH (sha256) of the installed bundle it runs, held from the hash to the shim's answer so an install cannot swap the bytes between them (`buildid.ShimBundle`); this variable answers only while no bundle exists at `--shim-main`. With neither, the SPAWN refuses (`spawn_failed`), naming both — an unstamped shim cannot be judged for staleness. It has no flag |
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
which the daemon relays as an intended arm (ERROR-ARMS.md). A shim whose own
`shim-lock` holder FAILED (spawn, exit, signal, wrong line, silence) answers
`StartSessionFailure.lock_holder_unavailable {failure}` instead — nobody owns
the conversation then — and the daemon relays that `LockHolderFailure` whole on
`OpenWorkspaceError.lock_holder_unavailable`, never as `conversation_owned`.

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

### The footer and the roster take ONE live-work set

`sessionwatcher`'s `LiveWorkSet` is the SINGLE AUTHORITY for detached-work
liveness on both surfaces: `publishLiveWorkLocked` hands the same value to the
roster (`SidebarSink.OnLiveWorkChanged`, which retires `idle_async`) and to the
footer (`FooterSink.OnLiveWorkChanged`, which retires a chip row and the
`background` arm). The watcher reaps each item's watch at its own terminal and
is the only party that knows an item has ENDED.

NEITHER SURFACE MAY COUNT LIVENESS FOR ITSELF. The footer's frames supply a
chip row's DESCRIPTION — label, command, tokens, start instant — and never its
liveness; `background()` answers from the set, so an empty set is never a
background arm. A footer that kept its own ledger reported a background task
for a workspace the roster and the tab-bar both called ready, and the ledger
had already been patched per delivery path twice before that.

THE SAME SET IS WHERE A LAUNCH IS SEEN. `FooterView.focus` is minted in
`resolve/footer/focus.go` when a set lists an id the PREVIOUS SET did not
(never off reconcile's `added`: the announcement opens the row first), less
ids the watcher ADOPTED (an `OnDetachedWork` with no announcer, the one shape
adoption takes) and ids already retired (a replay). A re-take of the same set
mints nothing, and crons, tasks and workflows are never in the set.

### A footer jump names the entry THE FEED drew

Every detached-work row the footer publishes states a `FooterJump`: the
entry's FeedId, or `unresolved(not_drawn)` until the feed has announced one
(`no_feed_entry` is retired: every kind draws an entry). The FeedId is never
composed by the footer. The feed resolver announces each subagent bubble's,
shell head's and Monitor call's tool-call card's FeedId the moment it first
draws it, and again when it changes (`feed.Deps.EntryPlaced`, wired in
`graph.go` to `footer.OnEntryPlaced`). A subagent of a subagent lives on its
parent's sub-feed, and only the feed knows that. The call runs under the feed's
lock and takes the footer's, which is the same feed-then-footer order the fault
path already takes. The footer never calls back into the feed. Each row's
resolution change is recorded as `daemon.footer.jump_resolution`.

A MONITOR's entry is its call's ordinary tool-call card (owner ruling,
2026-09-23), drawn in its owner's feed at the call's position and keyed by the
monitor's unit, which is also its detached-work id. The monitor is detached from
birth, so its detachment announcement continues the card as it stands (never a
shell head), its `ended` and `failure` arms restate the call so a replay draws
the card alone, and a card still running when the live set drops the monitor is
settled there (`settleMonitorsLeftLive`, `daemon.feed.monitor_left_live`).

## History is replayed ONLY on a workspace open or a transcript select

Owner rule, 2026-09-23: history is replayed only when a workspace is OPENED or
a transcript is SELECTED (`SPC j c`, BindWorkspaceSession), and only the FIRST
PAGE. A watch or StartTurn opened without `known_through` is answered with the
agent's first page, and serving it onto a feed that already holds those rows
re-fires the history effects (`clear_confirmed`, `delivery_bound_moved`,
`subagent_without_start`, `detached_unknown_unit`) and redraws the webapp.
The rule is structural:

- `sessionwatcher.Session.Opening` is REQUIRED and `Start` refuses the zero
  value at ERROR (`daemon.sessionwatcher.start_refused`).
  `sessionwatcher.WorkspaceOpened` and `sessionwatcher.TranscriptSelected`
  are the only two replays, and each has ONE production caller,
  `workspace.(*Fleet).openingFor`. Every other watcher is
  `sessionwatcher.ResumeFrom` the retired watcher's `Pointers()`.
- The fleet decides the opening in `openingFor`, and every watcher it starts
  goes through `startWatcher`: a transcript selected since the last watcher
  replays; a workspace this process never watched, or whose session comes up
  FRESH (a new book), replays; a restart, a revival, a rollout relaunch and a
  cold-gate re-open resume. This is correct because the feed keeps a
  workspace's rows across every bring-up but a bind (`ResetWorkspace`). The
  `daemon.workspace.bring_up` INFO record states `opening` and
  `replays_history` for every watcher.
- A turn opening never replays: `StartTurn` asks for ONE entry (R15 makes it
  the turn's own prompt row), bounded by the live main watch's
  `MainKnownThrough`. The main watch serves everything the turn writes live.
- Inside one watcher, `known` is never forgotten, so a re-opened watch (a
  relink, a repeated announcement) always states its pointer, and a detached
  handle retired at its terminal is never re-admitted by a re-served
  announcement.
- `workspace/opening_test.go`'s `TestFirstPageRequestsHaveNamedSitesOnly`
  fails the moment another production site builds a replay opening, a
  `WatchAgentRequest`, a `StartTurnRequest` or a `ReadHistory` first-page
  request.

A successor daemon's adoption (handover, crash boot) is that process's first
open of the workspace: its feed holds nothing, so it still replays the first
page. Whether it should instead carry the incumbent's feed is an owner
question, recorded in `docs/REMEDIATION-CHANGELOG.md` (`replay-first-page-only`).

## A settle stands alone, and a bare one is graded by its producer's stamp

A unit's start and settle upsert one store row, so a replay serves the settle
alone. Every settle arm restates what its start carried, and the feed draws from
the restatement: the input line, a send's address and summary, a spawn's
commission and created agent (so its sub-feed is addressable), an artifact
call's act, and the start instant on the settle instant
(`AgentActivitySettledAt.started_at`), which is what a replayed card's runtime
counts from when no start was held. A start this process held outranks the
restated start, so a live clock never jumps.

A settle that restates nothing is graded by `AgentActivity.contract`, the stamp
each producer writes at its ONE activity constructor (`feed.standsAlone`):

- STAMPED (`SETTLES_STAND_ALONE`): an invariant violation. ERROR
  `daemon.feed.settle_not_restated` when a held start draws it, ERROR
  `daemon.feed.activity_undrawable` (`errSettleNotRestated`) when nothing does.
- UNSTAMPED: a row written before the contract, expected old data. INFO
  `daemon.feed.settle_predates_contract`, drawn exactly as before (from a held
  start, or not at all via `errSettlePredatesContract`).

The stamp is the discriminator because it cannot misgrade a new defect as old:
it is written by the envelope constructor, never by the per-kind code that
restates, so a new arm that forgets to restate is still stamped. A missing
restated start costs a card only its runtime chip, so the card still draws. A
subagent hold of nothing but pre-contract frames retires at INFO; one holding a
stamped frame keeps the WARN `daemon.feed.subagent_without_start`.

## Deploy (`internal/deploy`, `internal/buildid`)

THE DAEMON OWNS DEPLOYS; there is no deploy script. `Deploy{force}` (and the
`claude-repld deploy [-force]` verb and Emacs's `agent-repl-deploy`, both thin
callers of it) and a self-merge landing (`merge.Trigger.Landed`, ONCE per
landing however many commits it carries) run `deploy.Deployer.Deploy`:

1. build into `<state>/deploy/staging/<nonce>` (`ScriptBuilder`: `make -C
   proto all`, then `bin/build-frontend.sh --out <staging> <target>` for
   shim, webapp, daemon, store, sidecar, lock). A failure is `build_failed`
   with the step, the tail of its output and the archived log; NOTHING is
   installed or restarted.
2. hash every staged artifact (`buildid`: binaries and the shim bundle by
   sha256, the webapp by its entry bundle's hash, elisp by the module-set hash
   held to `proto/vocab/elisp-build.json`);
3. install atomically (copy beside, rename over; the shim bundle only while no
   spawn holds it);
4. decide, component by component, against each RUNNING process's reported
   build: store/sidecar by their build report (`agentrepl/logging/buildreport`)
   and restart them in the recorded safe order (`Restarter`); Emacs streams by
   `WatchDaemon.elisp_build` and push `reload_elisp` to the stale ones; a stale
   daemon (its own boot-time binary hash) hands over and DEFERS shim and webapp
   to its successor; otherwise each live shim is judged by its last
   `SessionDiagnostics.shim_build` and a stale one goes to the prompt queue's
   bounce registry (`rollout.CheckStaleness`), and each workspace whose webview
   reported an older `webapp_build` gets `reload_webapp`.

One deploy runs at a time (`already_deploying`); a landing that arrives while
one runs is covered by ONE follow-up deploy. Every decision is a record under
`daemon.deploy.run` / `daemon.deploy.decide` / `daemon.deploy.install` /
`daemon.deploy.services` / `daemon.deploy.build` / `daemon.deploy.landing`.
`bin/build-frontend.sh` without `--out` stays Emacs's cold-start build (a
daemon must exist before it can deploy anything). `agent-shim/wire` is
DELETED: nothing in the rebuilt daemon imports it.

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

THE WORKSPACE ID ON A RECORD AND IN A SINK NAME IS THE DAEMON-MINTED
`ids.WorkspaceID` (16 hex characters, `wsm.IDLength`) -- the same id the shim,
the webapp and the store state, so a reader grouping by `workspace_id` sees
ONE group per workspace. `run.go` binds the lookup
(`Surfaces.BindWorkspaceIDs`, `db.WorkspaceByDir`) the moment the state client
is open and before any workspace-owned record; a workspace the roster cannot
name is REFUSED, never attributed to anything derived from the path. A newly
minted target is `logs/agent-repl-<minted id>-<sink>-*.log`, and an INFO
(`daemon.dlog.sink_opened`) states the scheme on every sink open. Registration
therefore resolves its workspace sink only AFTER `RegisterWorkspace` mints the
row; the git derivations before it go to the run log with the announced
directory on them.

`dlog.WorkspaceDirHash` (md5hex(clean abs dir)[:8]) is the SHIM-HELD KERNEL
LOCK FILE's derivation and stays recorded, as the ordinary context key
`workspace_dir_hash`, so an operator can grep a record against a lock file
name. It is never a `workspace_id`, and the lock file naming is untouched.
MIGRATION: a target an older daemon minted under that hash is APPENDED TO
where the canonical link still names it (the standing-target rule); nothing is
renamed and no history is orphaned. Only a new target gets the minted name.

A WORKTREE THE DAEMON REMOVES IS DETACHED FROM ITS SINKS FIRST.
`gitclient.RemoveWorktree` calls `Surfaces.DetachDir` before `git worktree
remove`, and `CreateWorktree` calls `AttachDir` after `git worktree add`. A
detached directory's sinks create, re-point and read nothing inside it; their
records keep landing in the same targets under `<state>/logs/`. Before this, a
late sidecar record opening the merged workspace's first `sidecar.log` re-created
`<worktree>/.claude/emacs` one instant after the removal, and the teardown's
postcondition logged "still present after removal" at ERROR. The mark is taken
under the mutex every sink open holds, so the race is closed, not narrowed.

`daemon.log`, `webapp.log`, and `sidecar.log` rotate synchronously at 64 MiB
through `agentrepl/logging.OpenRotating`, retain `logging.DefaultBackups`
generations, and atomically refresh their canonical symlink after each roll.
An already-open reader remains on the retired inode.

A NEW DAEMON INSTANCE APPENDS TO THE TARGET THE CANONICAL LINK ALREADY NAMES,
so one file spans instances and rotation happens only at the cap. Retargeting
the link on every boot made `bin/logs.sh --workspace` show the current
instance alone, and the previous daemon's boot -- its adoption records
included -- sat on an inode nothing named any more. The standing target is
joined only when the canonical path is a symlink naming a regular file
directly inside `<state>/logs/` and that file is under the cap; a
workspace-provided regular file, a foreign symlink, a swept target and a
target at the cap are each displaced by a fresh one, as before. `shim.log` cannot rotate
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

Read daemon records and harvest run windows through `../bin/logs.sh`; the full
path, rotation, attribution, and level-switch table is in `../AGENTS.md`.

## Workspace-mutation progress (stages on WatchDaemon)

A workspace mutation's stages are pushed on the daemon-level `WatchDaemon`
stream as `WorkspaceMutationProgress`, keyed on a CLIENT-MINTED `op_id` the
request carried. That stream and not the per-workspace one, because a
create's stages precede the workspace's existence; the stream is a broadcast,
so the id is the only thing that lets a client tell its own operation apart.

Two mutations report stages today and they do NOT work the same way:

- **Create.** An `op_id` opts the create into option B: the rpc ACKS at once,
  the work detaches, and the stages AND the terminal outcome (`succeeded` /
  `failed`) both ride the stream. A create with no `op_id` keeps the legacy
  synchronous contract.
- **Open.** An `op_id` changes nothing about how the rpc answers -- it stays
  synchronous, and its success and every typed refusal reach the caller on
  `OpenWorkspaceResponse`. Only the STAGES ride the stream, because the wait
  inside the rpc is exactly what the answer cannot carry. There is no terminal
  step on `WorkspaceOpenProgress`, deliberately: two terminal reports for one
  operation could disagree.

Each verb takes its reporter as a proto-free interface (`CreateProgress`,
`OpenProgress`) so the verb layer never names a wire type; the server maps the
verb's own stage vocabulary onto the enum, and an unmapped stage is logged at
ERROR and NOT relayed rather than sent as UNSPECIFIED. A stage is reported
only when the work it names actually runs -- an already-live session emits no
bring-up stage -- because a stage announcing work that is not happening is
worse than no stage at all.

## Final answer: landed, not landed, not timely

A turn's terminal NAMES the response that answered it, and the feed draws that
row with the green final-answer border. There are three outcomes, and only the
first is silent (`internal/resolve/feed/finalanswer.go`, `turnended.go`).

1. **LANDED.** The terminal (`AgentSuccess_Completed`) names an answer activity
   id AND the resolver resolves it to a DRAWN, NON-THINKING response row. That
   row is published `final_answer=true` — live and on history replay, which
   walks the same terminal path (`recordFinalAnswer` / `restampFinalAnswer`).
2. **NOT LANDED.** The terminal arrives and either (a) names no answer while
   this turn drew response prose, or (b) names an answer with no resolvable
   drawn row. The daemon records it at ERROR
   (`daemon.feed.final_answer_unresolved`, with `turn`, `unit` and `why`) and
   raises the `final_answer_unresolved` fault, which STANDS UNTIL THE NEXT TURN
   STARTS. Both turn-start sites retire it, because a prompt replayed from
   history opens a turn without passing `OnTurnOpened`.
3. **NOT TIMELY.** An open response fold that has received no frame and no
   terminal for `DefaultAnswerStall` (90s) raises the SAME fault kind with
   `why` = `stalled`, cleared the instant a frame arrives ON THAT FOLD — a
   sibling block of the same turn paying out says nothing about the one that
   stopped — or the turn's terminal arrives, which answers every fold at once.
   The window is the injected `Deps.AfterFunc`, so tests advance a virtual
   clock and never sleep.
4. **Presentation is the FOOTER ONLY.** The bubble is never marked: the prose on
   screen is exactly what the agent said, and a turn whose answer the workspace
   cannot POINT AT is a fact about the workspace, not about the prose. The
   fault draws through the fault chip every other kind draws through
   (`FooterStatusActivityFault`, kind + a terse detail line composed by
   `answerFaultLine`); no new visual treatment. The kind is NON-ESCALATING
   (`health/footer.go`): the session is serving, and `disconnected` would close
   the composer over a session that is perfectly healthy. Its three cases are
   told apart by `why`, carried in
   `SessionFaultFinalAnswerUnresolved`, never by the footer's substatus cell —
   a substatus is legal only under a status the fault claims, and neither
   claimable status would be true here.

A CONTEXT-CUT DIRECTIVE, a turn that drew no prose at all, and a turn whose
only drawn prose is a VENDOR-SYNTHESIZED NOTICE (`AgentResponseSuccess.authorship
= synthesized_notice`, e.g. "API Error: Can't reach the API server", which the
feed already draws in the notice register) are excluded from (2): none ever had
an answer to lose. The settled whole decides a block's authorship, so a block
re-settled as the model's prose counts again; model prose beside a notice still
raises.

**THE ALIASING RULE.** One prose block can reach the resolver under two
divergent activity ids (the two store planes disagreeing on
`<message.id>:<block index>`). The reconciliation keeps ONE row and retires the
other fold — and the producer is free to name EITHER id as the turn's answer,
because it knows nothing about which one this resolver kept. So a retired unit
is **ALIASED onto the surviving row** (`aliasAnswerRow`), never deleted from
`answerRows`: both ids name the same block, the surviving row is that block's
row, and a lookup through either id lands on it. Deleting it is what made 24 of
199 named answers in one 40-hour window resolve to nothing. The alias also means
`answerRows[unit]` may name a row the named unit's own fold does not own, so
`recordFinalAnswer` and `restampFinalAnswer` resolve the row's OWNING fold
(`foldOfAnswer`) before reading its markdown or re-stamping it.

The integration fakes conclude turns through `pushConcludedTurn`, which pushes
the answering response block BEFORE the terminal that names it: a real producer
never names an answer it did not emit, and a fake that pushes the terminal alone
trips rule 2(b) — correctly.

## A thinking row is superseded once a later response lands in its feed

Owner rule, 2026-09-23. `FeedResponse.superseded` is set on a THINKING row
exactly when some response row (thinking, prose or a final answer) sorts after
it in the SAME feed; tool calls, prompts and every other row kind never count.
It is decided in one place, `internal/resolve/feed/superseded.go`:

- The feed keeps a per-feed record, updated only at the two structural edges
  (a response row PLACED, a response row RETIRED), and `upsert` stamps the flag
  from it on every draw, so a later fragment of a superseded fold keeps it.
- Placing a response supersedes the nearest earlier thinking row and RE-PUSHES
  it after the new row's own publication; a thinking row placed above a later
  response (older history over live rows) is placed superseded. Retiring the
  only later response re-pushes the thinking row un-superseded.
- Pages serve the stored rows, so replay, pages and live agree; a sub-feed
  follows the rule within itself and never across feeds.

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

## A forced kill is the user's to ask for

An interrupt ends only the synchronous turn: `KillTurn(..., false)`. Detached
work (background agents, shells, monitors, workflows) ends only by its own
per-task stop, or by a forced kill the user explicitly asked for. The daemon
never forces a kill on the user's behalf for a merge, a rollout or any
other act of its own; a holder that needs the session quiet waits on the
fleet's watcher-driven freeness — an admitted merge through `Fleet.AwaitFree`,
and every shim bounce and handover transfer through the prompt queue's bounce
registry, which takes it on the freeness edge. A FORCED deploy or restart is
the user's explicit ask, and only it bounces over work in flight.

`forced_kill_guard_test.go` enforces it. Every `KillTurn` call whose force is
not the literal `false`, and every `KillTurnRequest` literal whose `Force` is
not the literal `false`, must be listed there with the user request that
authorizes it: today the confirm challenge (`interruptTurn`), a restart
(`forceEndTurn`), and the two adapters that forward their caller's force. A new
forced site fails the unit suite until it is listed with its reason.

## The roster row's viewed mode is cleared by the RENDER, not by a setter

`RosterRowViewed` is the row's display mode: present is PARTIAL (the client
recedes the row's NAME), absent is FULL. `sidebar.Resolver.SetViewed` raises it
— the editor's `MarkWorkspaceViewed` is the only caller, because dwell is an
editor fact — and there is deliberately NO setter that lowers it.

It is lowered in exactly one place: `wsState.noteArm`, called from `row()` as
each row's status arm is resolved for publication, which clears the marker
whenever the arm differs from the one last published for that workspace.
**Hanging the clear off the render rather than off any particular fact-setter
is the point**: every origin of a status change — an accepted turn, a shim
frame, a merge, a dead session — reaches the row through that one funnel, so
"any status change restores FULL" holds without every present and future setter
having to remember to clear anything.

The mode is in-memory only and writes no durable record: it is a view fact and
must not outlive a restart. The editor's tab-bar draws the same mode from its
own latch, on the same reset rule; the module-root AGENTS.md section "The
viewed mode" owns the cross-surface invariant.

## THE LIVE-SHIM INVARIANT: a workspace whose shim is live carries NO terminal session record

Owner ruling, 2026-09-20. A workspace's session row records a terminal — its
cause of death — and three surfaces compose off it; the roster RECEDES a row
whose session reads `killed`, and Emacs gives a tab only to a row that is not
receded (`lisp/roster.el`'s `agent-repl-roster-desired-tabs`). So a stale
terminal is not a cosmetic wrong: it is a workspace the user cannot reach.

Nothing ever RETIRED one. The only write that cleared a terminal was a
successful `PutSession`, which clears it incidentally, because the row it
composes carries none. A bring-up that parks at a standing cold gate records no
session facts at all — so workspace `3e2d9cadc6794e13`, killed on 2026-09-15,
came up on 2026-09-20 with a live shim behind a standing gate and a record that
still read `killed`: `OpenWorkspace` saw `Sessions.Live` and answered in under
two milliseconds, the roster receded the row, no tab was drawn, and the gate the
user had to answer lived in a workspace with no tab.

The invariant is enforced STRUCTURALLY, at every place the daemon begins
holding a live client, through ONE helper — `workspace.retireTerminalRecord`
(`internal/workspace/terminalrecord.go`):

- `Fleet.hold` (`internal/workspace/sessions.go`) is the ONLY way an entry
  enters `Fleet.sessions` on the bring-up paths — the adopted shim, the session
  parked at its cold gate, and the started session — and it retires the record
  in the same breath. A restatement of the SAME client (sessionUp states its
  entry twice, once before the watcher opens and once after) writes nothing.
- `Fleet.Install` (`internal/workspace/fleet_rollout.go`) covers the ROTATION
  path, because both things that follow an install can leave the record
  untouched: an adoption records no facts, and the relaunch's `Resume` can park
  at a cold gate.
- `OpenWorkspace` RECONCILES rather than trusting the liveness it read: a live
  session is the reason the verb starts nothing, and it was also the reason the
  record was never revisited.

The store write is `wsm.DB.ClearSessionTerminal`, stated as a POSTCONDITION —
the workspace carries no terminal afterwards — so a workspace with no session
row at all is not a refusal. **A DELETED session is never resurrected**: the
store refuses with `ErrSessionDeleted`, and the helper treats that refusal as an
outcome (recorded at INFO) rather than a failure, because a live client is not
evidence against a deletion. Every other store failure is recorded at ERROR and
returned, and it fails the bring-up or the open.

`sidebar.recedes` is UNCHANGED: a genuinely killed session still recedes. The
fix is that the record became accurate.

## SelectWorkspace: the selection first, then at most one revival per workspace

Owner ruling, 2026-09-19. `Select` (internal/workspace/select.go) does its work
in a fixed order, and the order is the contract:

1. **The selection, at once.** `selectCurrent` reads the current workspace,
   stamps `SetCurrent`, clears the attention marker, republishes the registry
   and calls `Sidebar.SetSelected`, all under the verbs' `selection` lock, so
   concurrent selects land WHOLE in the order they took it. Nothing after this
   step touches `current`: **the stamped selection reflects REQUEST order, and
   a revival completing never re-stamps it.** Before this ruling the revival
   ran first, the sidebar lagged every switch by the bring-up (~0.75s each,
   serialized), and concurrent selects stamped current in the order their
   revivals finished, overwriting the user's last switch.
2. **Then, if parked, the revival** (`reviveIfParked`, internal/workspace/
   revive.go), with the roster row's `RosterRowReviving` marker raised for the
   duration of `Sessions.Start` and lowered however Start returned. A failed
   revival still fails the select: the error reaches the rpc caller, and the
   selection it already made stands.

**INVARIANT: AT MOST ONE RESTART IN FLIGHT PER WORKSPACE.** `reviveIfParked` is
single-flight keyed by workspace (`revivalFlights`): the park check and the
start run in the leader only, and every caller arriving while a revival is in
flight JOINS it and answers the leader's outcome, a failure included, without
calling `Sessions.Start` itself. A joiner whose own context ends stops waiting
with that error; the leader carries on. A caller arriving after the flight
retired leads a fresh one, which re-reads the park. Any new revive caller goes
through `reviveIfParked` and inherits this; nothing in the verbs calls
`Sessions.Start` for a parked workspace any other way. Covered by
`TestConcurrentRevivalsOfOneWorkspaceStartOneSession` and
`TestConcurrentSelectsOfAParkedWorkspaceStartOneSession` (a gated fake Start,
N callers, exactly one Start), and the request-order landing by
`TestARevivalFinishingNeverRestampsTheSelection`.

## A held-prompt edit is a claim the queue owns under its delivery lock

`EditHeldPrompt` (owner spec, 2026-09-23; `internal/promptqueue/edit.go`).

- **THE CLAIM IS WRITTEN AND READ UNDER `wsState.drain`**, the mutex every delivery of a standing hold is decided under: a turn end, a lease change, a revival's release, a Release, and a Submit that would go straight to the shim. So "delivered mid-edit" cannot be scheduled: a begin that lands during a delivery waits for it and then finds the prompt delivered.
- **ONE CLAIM PER WORKSPACE.** While it stands the edited prompt and every prompt queued after it (the tray's order: queued_at, then the turn id) are withheld from `nextDeliverable`, the semantic head included; a Release of one is `release_refused`; a submission with nothing running is held behind it. `deliverHeld` refuses a withheld hold at ERROR as the backstop.
- **A COMMIT REPLACES IN PLACE** (`ReplaceHeldPromptSaid` keeps `queued_at`, clears the verdict and the acceptance), retires the claim and reclassifies through `classifyHeld`, so an interject then runs the ordinary interject. A content EPOCH bumped under `wsState.verdicts` makes a verdict judged about the replaced words settle as a discard; lock order is `drain`, then `verdicts`.
- **THE CLAIM IS SCOPED TO THE EDITOR'S HOST STREAM, never a timer.** A begin with no `WatchHostWorkspace` stream is `no_editor`; the last one closing calls `Queue.EditorGone`, which retires the claim as a cancel would. The claim is in-memory, so a restart retires it with the stream.

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

## The landed-worktree reaper (`internal/worktreereap`)

The merge queue retires the worktrees it merges. Agents also cut worktrees
for themselves, and a branch can land by a squash or a cherry-pick the queue
never saw, so the daemon sweeps for them itself. The decision is PROGRAMMATIC;
nothing about it is agentic.

**Schedule.** One background loop (`worktree_reaper` in `graph.go`): the first
sweep `5m` after start, then one every `24h`, so a daemon that restarts daily
still sweeps daily. Time comes from an injected `Clock`; the unit suite fires
the schedule by hand and never sleeps. **One sweep at a time**: in process a
`TryLock` refuses a second (`ErrSweepRunning`), and across processes the sweep
holds `<lock dir>/worktree-reap.lock` (`internal/flock`) for its whole run,
so an incumbent and its handover successor never sweep together; the loser
records INFO and sweeps nothing.

**Which repositories.** Every row of `wsm.ListRepositories` -- the
repositories the daemon registered for its workspaces. The "default branch"
is whatever `gitclient.DefaultBranch` resolves for the repository at sweep
time (never a hardcoded `master`); it is resolved ONCE per repository to a
commit and its tree, and every worktree is judged against that pair.

**The landed rule.** A worktree has landed iff `git merge-tree --write-tree
--no-messages <default commit> <HEAD>` exits 0 with EXACTLY the default
commit's tree: merging the branch into the default branch would change
nothing. It is about CHANGES, not commits, so a merge, a cherry-pick and a
squash all count; a conflict (exit 1) or any other tree is not landed. It
needs git >= 2.38.

**The gates, in the order they are asked** (each keep is DEBUG
`daemon.worktreereap.keep` with its `reason`, and counted in the summary):

1. the main worktree (git's first listing entry, or the repository's own
   dir) and a bare entry -- never judged;
2. `locked` -- kept; `prunable` -- NOT removed: the repository gets one `git
   worktree prune` (INFO `daemon.worktreereap.prune` naming every entry);
3. the checkout of a registered workspace that is NOT CLOSED
   (`open_workspace`), the checkout of any workspace the fleet holds a LIVE
   SESSION for (`live_session`, `Fleet.Workspaces`), and a branch an open
   workspace was cut from (`parent_of_open_workspace`: a nested workspace
   merges into its parent's worktree);
4. a linked worktree ON the default branch (removing it would delete the
   default branch), an unborn HEAD, a directory that is not there;
5. **the idle gate** (`recently_active`): the newest of the worktree's admin
   files' mtimes (`<git dir>/worktrees/<name>/{HEAD,index,logs/HEAD}`, from
   `rev-parse --absolute-git-dir`; HEAD is required, the other two may be
   absent), the HEAD commit's COMMITTER time, and -- for a registered
   workspace -- the record's `created_at`, `last_activity_at`,
   `last_selected_at` and `merged_at`, must be older than the threshold
   (`AGENT_REPL_WORKTREE_REAP_IDLE`, default `24h`). The admin files move on
   every checkout, add, commit, reset and rebase step; the committer time
   covers a commit written elsewhere; the record covers the daemon's own
   knowledge that somebody was there. The merge stamp is also what keeps a
   worktree the merge queue is retiring out of the sweep: the queue stamps
   `merged_at` before `closed` and before its own removal. Files in the tree
   are not read: a tree only qualifies when it is clean, and a clean tree is
   what its HEAD records. Activity is read BEFORE any probe, and
   `gitclient.IsClean` runs `git --no-optional-locks status`, so nothing the
   sweep runs can look like work;
6. dirty (`git status --porcelain` non-empty: modified OR untracked; ignored
   files do not count);
7. the landed rule (`merge_conflicts`, `unlanded`).

**Removal.** `gitclient.RemoveCleanWorktree` -- the same door
`RemoveWorktree` is (the log sinks are detached first, the prune runs, the
postcondition decides) but WITHOUT `--force`, so a tree that became dirty
after it was judged is refused by git and kept (and re-attached to its
sinks). Only after the removal succeeds is the branch deleted, by
`gitclient.DeleteBranchAt` -- `git update-ref -d refs/heads/<b> <judged
head>`, git's compare-and-delete rather than `branch -D`, so a branch that
moved after it was judged survives with the commit nobody judged. A registered
workspace's row is left as it is (closed), exactly as the merge queue leaves
the workspaces it merges.

**Records.** INFO `daemon.worktreereap.remove` per removal (worktree, branch,
head, default branch and commit, `why`, `last_activity`,
`last_activity_signal`, `idle_for`) and INFO `daemon.worktreereap.branch` per
deleted branch; ERROR for any step that fails (`daemon.worktreereap.repo`,
`.judge`, `.remove`, `.branch`, `.prune`), after which the sweep goes on with
the next worktree or repository; INFO `daemon.worktreereap.sweep` for the
start and the summary (repositories, worktrees judged, removed and their dirs,
branches deleted, pruned, kept by reason, failures). A git the daemon's own
exit cancelled is INFO, never a failure. A registered repository whose main
worktree is gone is INFO and skipped.

**Priority.** The sweep takes no lock an interactive or prompt path takes: it
reads the registry (two short queries) and the fleet's live set (one read-lock
snapshot) once per sweep, and everything else is its own git children. Those
children are NOT niced: the daemon has no idiom for lowering a child's
priority (`bin/background.sh` is the test suites'), and git inherits the
daemon's own.

## Coverage deliberately not attainable under the no-git-in-tests directive

The git client's tests pin argv, env scrubbing, `-C` selection and output
parsing against a scripted fake `git`; they can no longer prove git's OWN
behavior: that a `--no-ff` merge yields a two-parent commit, that the
landed range equals the source branch, that a conflicted merge leaves
unmerged index entries and MERGE_HEAD, that a revert removes the content
in one commit, that `worktree prune` clears a stale registration, the
exact `status --porcelain` markers, that git honors GIT_DIR over `-C`, that
`merge-tree --write-tree` of a squash-landed branch answers the default
branch's own tree, that `update-ref -d <ref> <old>` refuses a moved ref, and
real-git version compatibility (`rev-list --no-commit-header` needs
git >= 2.33, `merge-tree --write-tree` needs git >= 2.38, `worktree list -z`
needs git >= 2.36). Those are e2e facts now (the project lead's suite).
