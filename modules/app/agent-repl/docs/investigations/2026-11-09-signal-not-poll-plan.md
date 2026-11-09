# Signals, never timers: the daemon and its tests (2026-11-09)

Owner request (main workspace): remove every place, in production or test
code, where time stands in for synchronization; consolidate the daemon test
harness so far fewer daemons start. Implemented by `main (doom)` itself in
workspace `daemon-signal-not-poll`, landed with `/emergency-merge-queue-bypass`
once everything is done (owner instruction, repeated 2026-11-09).

## Done (uncommitted until each batch passes)

- `internal/kernelevent`: process exit (kqueue/pidfd), path watch
  (kqueue vnode/inotify), `Follow`, `Merge`, `Source`.
- harness exit waits moved onto kernelevent (pin test).
- `wsm.DB.Changes` (WAL follow for files, commit signal in memory).
- `procgroup.AwaitGone`, `sessionlock.AwaitFree`, `publish.Changes`,
  `publish.Topic.AwaitSubscribers`, `dlog.TestLogger.Await`.
- Production polls removed: heldingress (250ms), deploy service restarts
  (1s), rollout manifest/arm/serving-release/adoption/carry follow, spawn
  address report, readiness probe (single probe on the listen backlog),
  shimclient adopted exits, workspace lock release, root watch (1s),
  startingshim (20ms; shim now binds `.<stem>.b`, listens, links into place).
- Rollout controller signals ServesIntake changes (`IntakeChanged`, every
  unlock through `c.unlock()`, pin test).
- Harness polls removed: every log wait (`awaitChange` + `followFiles`),
  awaitFile, AwaitFileExists/Gone, fault latch, coverage strays,
  AwaitProcessGone (now `Daemon.AwaitProcessExit`).
- Unit-test polls removed: page_test sleeps, spawn_test sleeps and pidExited,
  supervisor busy spin, workspace awaitRecord, rollout awaitRecord spin.

## Option A progress

- `internal/inflight`: Tracker (Begin/Go/AwaitIdle), Standing, Queue; pin
  test: every production `go` is inflight.Go, inflight.Standing, or a bare
  `go` marked `// inflight: joined` (132 sites classified).
- `publish.Mailbox` (the subscriber queue, exported) counts each value from
  enqueue until the consumer's Ack; `Topic/Events.SubscribeCounted`. Every
  stream handler (serveTopicWith, host, web, daemon, feed) and the internal
  followers (host asks, composer entries) ack after writing/handling. The
  feed tail now uses Mailbox (its duplicate queue removed).
- Fixed on the way: rollout `claimArmed` now begins the run under the claim
  (a participant could read an earlier failed run's cleared outcome).

- AwaitIdle rpc (agentrepl.v1 `endpoint_await_idle.proto`), served only with
  `AGENT_REPL_TEST_BARRIER=1` (the harness sets it). It answers each TAGGED
  stream's frames written: the client opens a stream with an
  `Agent-Repl-Stream-Tag` request header (harness tagInterceptor), the
  accept writer counts Connect envelopes per tag (server/streamframes.go).
- Harness: `Daemon.AwaitIdle`, every stream tagged and counting its frames
  read; ExpectNoPush/ExpectNoPushMatching/ExpectFileUnchanged/expectRPCCount/
  expectNoRPC wait on the barrier then check at once. ProbeWindow is gone.
- Queue PAUSED 2026-11-09 20:37 UTC (owner instruction) for this work's
  test runs; it is resumed by the bypass landing (step 6 of the skill).

### Option A still to do

- Known gaps to close or document: the daemon stream's `elisp` reload channel,
  `publish.Changes` (heldingress fleet wake), kernel-event wakes, shim frames
  in transit (a test awaits the daemon's record of a frame before the
  barrier), and long merge runs marked Standing.
- Replace the ~40 negative probes with AwaitIdle + an immediate check.

## Remaining

1. Option A, the owner's choice for the ~40 negative probes (ProbeWindow,
   ExpectNoPush, ExpectNoPushMatching, ExpectFileUnchanged, expectRPCCount,
   expectNoRPC, bringUpProbeWindow): an in-flight work tracker
   (`internal/inflight`, a counter whose zero closes a channel) and a
   test-only barrier rpc that blocks on it. Every consequence of a request is
   counted inside that request; a task that starts more counts it before it
   finishes; a push counts until written to its stream; a shim call until
   answered. Standing loops and external events are not counted.
   - 132 `go` statements to classify (counted / standing / joined), with a
     pin test.
   - ALSO every hand-off of work to a standing worker (queues, channels) must
     count the item until processed; no mechanical pin covers that.
   - Pushes: the barrier must be ordered with the stream the test reads
     (per-stream frame counts, or a barrier frame on the stream).
2. Production: sessionwatcher reopen timer (needs a shim.v1 `expected` flag on
   WatchBash/WatchAgent so the shim waits for the first row), persistentwifi
   5s, retryAdoption backoff, handover reoffer delay, writebarrier 1ms quiet
   ticks.
3. Tests: fakeshim control-socket connect polls (shimctl.go:336,367),
   pushSessionUpdateWhenSubscribed (shimctl.go:537), openFeedOnceCarrying,
   drainAwaitAddrFileChange, awaitNewAdvertisement, ftAwaitTrue,
   awaitRouted, awaitSessionFault, WatchSession count loop,
   workspace/rollout capture-logger spins left.
4. Daemon starts: scenario goldens on one daemon (~124 boots), the 183
   tier-audit removals (docs/investigations/2026-10-14-test-tier-audit-
   integration-nzx/removals-daemon.txt), wsm template database per test.
5. Docs: daemon/AGENTS.md bound tables (ProbeWindow, stateRootCheckEvery,
   heldingress 250ms, readyProbeEvery, spawn Poll) and ARCHITECTURE.md.
6. Commit in clean pieces (extractions separate), then the bypass landing.
