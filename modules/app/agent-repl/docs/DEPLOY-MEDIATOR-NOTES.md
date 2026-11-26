# Deploy mediator's notes

The deploy mediator's own standing notes on log records that need judgement:
what it has learned about a record that a rule in the store cannot express.
Its brief names this file, and it is the mediator's to write.

- A kind of record that is ignorable whatever the circumstances does not belong
  here. Record it in the store, which drops it before every check:
  `agent-repld merge-queue mediation ignore -operation <operation> [-message <text>] -why "<one sentence>"`.
- A note here is for a record that is ignorable only in some circumstance, or
  whose meaning took an investigation to learn. Say the operation, the message,
  the circumstance, and why.

## Notes

- A race finding's ref is `suite/test`, where the suite is `daemon` and the
  test is `integration.Test…` (and may hold a slash for a subtest); since
  2026-11-26 the brief names the two apart, as suite `daemon`, test
  `integration.Test…`, and a dispatch copies each verbatim.
- Handover groups that are older than the three most recent deploys and
  recover on their own: `daemon.server.await_streams_ended`, `go_away`,
  `streams_unwritten`, `await_quiet` are the predecessor's exit-deadline
  records (first 2026-11-18); `daemon.handover.quiesce`,
  `daemon.rollout.transfer`, `daemon.promptqueue.bounce` with "lease already
  held by merge" (first 2026-11-20) is a handover hold refused by a merge
  lease, and the workspace is re-offered and taken up by the successor;
  `daemon.logsites.pass` cut by `context canceled` at the predecessor's exit
  is expected and is INFO since 2026-11-26. A reader's `ReportUserInteraction`
  answered `notYetAdopted` mid-rendezvous is the handover's lifecycle state
  and is INFO (`feed.interaction-handover`) since 2026-11-26; any other arm
  of that call is still an ERROR refusal.
- `daemon.sidebar.merge_queue`, "the deploy mediator is one the registry does
  not carry": ignorable when the user has removed the deploy mediator
  workspace. The daemon makes a new mediator at the next deploy. In any other
  circumstance it is a real fault.
- `daemon.merge.mediation`, "the intake's turn could not be awaited" with the
  session watcher closed: ignorable right after the user removed the deploy
  mediator workspace, for the same reason. Otherwise it is a real fault that
  holds the queue.
- `shim.engine.skill_watch`, "did not come live within its bound": logged
  twice at 2026-11-23T20:41Z with the raised bound (`bound_ms` 10600, 951 and
  974 probes written, no event seen). A watch that sees none of about 950
  probes in 10.6s is not slow, it is not delivering, and a larger bound will
  not fix it. If it recurs after a deploy, diagnose the platform watch itself
  (the probe's watch or the stream setup), not the bound.
- `daemon.lockwatch.stall` on `sessionwatcher.views` at a new daemon's boot:
  the session watcher holds its views lock while a callback waits for the
  daemon's single workspace-database connection (the `wsm` store,
  `SetMaxOpenConns(1)` in `daemon/internal/wsm/open.go`; stacks show
  `wsm.(*store).write` under `routeSessionUpdateArmLocked`). Logged 43 times in
  the three days before 2026-11-23T20:29Z.
- The `*Compile-Log*` buffer holds two old lines about
  `elisp.commands.change-spec` expanded outside a file load; `lisp/commands.el`
  now passes the owner. Only a new line with different text or a growing count
  is a finding.
- `elisp.page-report.held-without-reason`: logged for every page the running
  Emacs held when `lisp/page-report.el` was first hot-loaded at the
  2026-11-24T06:08Z deploy, because those pages were loaded before reasons
  were recorded and the table started empty. It repeats on each host-stream
  subscribe (each handover) for a page until that page is next loaded or
  Emacs restarts. No code path can produce the state again: a cold start
  loads every page through `agent-repl-page-unload-load`, and a hot reload
  keeps the table. The contract's reason vocabulary has no arm for an
  unrecorded reason, so the repair is seeding the table once in the running
  Emacs or an Emacs restart, which is the owner's to allow.
- `daemon.sidebar.merge_queue` "the hold was broken" (2026-11-24): a landed
  merge still deploying holds its queue's head from its teardown (since
  00b3580ae3). A broken hold logged right after a handover can be one the
  predecessor's older build admitted before it had the hold; check which
  daemon pid logged `daemon.merge.start` for the admitted merge. On a build
  with the hold, the record is a real broken invariant and is dispatched.
- `elisp.connect.unary-failure`, `elisp.rpc.transport-failure` and
  `elisp.host.page-state-failed` on `ReportPageState` with `invalid_argument`
  (2026-11-25): when a landing renames a page-state wire arm, the deploy's
  handover window logs one such trio from the old elisp speaking to the new
  daemon during the roughly 3s before the elisp reload. That trio is the
  handover's contract skew, not a defect to fix. The same records after the
  elisp reload are a real fault.
- `daemon.promptqueue.interject` "the interrupt was refused; the jump is
  stripped and the prompt waits for the turn to end" with cause
  `not_the_open_turn` (2026-11-25): the user's interject raced the end of the
  turn it targeted, and a new turn had opened by the time the shim was asked
  to kill it. One such record is the user's own race, drawn on the strip so
  they know why the prompt did not jump. Many in one window would show the
  interject reading a stale open turn, and would be dispatched.
- `daemon.merge.production` WARN "the span's log sites were pruned before its
  mediator read them" (2026-11-25): expected once per span that was owed
  before the retention fix (dbde96d191) landed and whose rows were already
  pruned; eight such spans stood due on 2026-11-25. A record of it for a span
  owed after that landing is a retention defect and is dispatched.
- Merge-gate races the queue hands over (2026-11-25): a race's seeded artifact
  is the gate log of its FIRST failure and holds only the failing test's log
  tail at info and above, so a cause that settled at debug cannot be read from
  it. Before fixing, check whether the site already handles the quoted cause
  (`git log --since` on the file): on 2026-11-25 six of nine open races had
  been closed at their sites by landings after their last failure
  (merge.progress exiting/cut, workspace.bring_up abandoned, merge.live_bubbles
  recordFailed, rollout.relaunch closedUnder, promptqueue.submit ErrNotFound,
  cachelapse.drive ErrNotFound, and rollout 1269af6bca for the late-facts
  re-adoption). A race whose cause the tail cannot name gets the records that
  would name it, at info, on the path the tail shows went silent.
- `TestLanding/DisplacedTurnCapturedEndedThenResubmittedExactlyOnce` (e2e): the
  merge-queued park is asked at the session-up edge of a `skills_changed`
  bounce, deferred for the bounce, re-asked at `BounceEnded`, and that re-ask
  settled with no record at info (2026-11-23 20:30:59). The settles that were
  at debug are now at info for a merge-scoped park; the next failure's tail
  names the gate or the session state it read.
- Emacs unary timeouts at a handover (`elisp.connect.unary-timeout`,
  `elisp.host.page-state-failed`, `elisp.host.evaluation-answer-failed`, all
  "timed out after 10s" against the successor at 127.0.0.1:49152) and the
  sidecar's `workspace-locate-failed` (LocateWorkspace deadline exceeded) are
  one problem: the successor's one state-database connection is starved while
  it adopts a hundred workspaces. `daemon.wsm.conn_pressure` at info names the
  holders; on the 2026-11-25 23:57 handover the heaviest was
  `daemon.wsm.list_workspaces<-server.(*server).LocateWorkspace 12×1170ms`: the
  sidecar's own locate calls, each reading every workspace row, held the
  connection for most of the second and starved each other out to their
  deadline. The Emacs side of the group was first logged before 2026-11-25
  12:00Z (`conn.go` dates it to 2026-10-24), so it is older than the
  three-deploy window; the sidecar locate is owned by
  `sidecar-locate-times-out-at-handover` (prod-mediator's one-shot child,
  2026-11-25). Read `conn_pressure` first when either recurs.
- `daemon.topbar.usage_contradiction` "it reported two different usages"
  (2026-11-26): before `TokenUsage.provisional` landed, every streamed
  response logged it once, because the stream plane's in-flight snapshot and
  the file plane's settled usage of the same unit looked like two reports.
  After the landing, a record of it means two SETTLED reports of one unit
  differ, which is a real producer defect: read the unit's write ledger
  (`sqlite3 ~/.cache/agent-repl/store/events.db "select * from write_ledger
  where upsert_key='activity:<unit>'"`) to see which producer wrote each.
  "it reports more thinking tokens than output tokens" with `taken: false`
  is a provisional report arriving after the settled one, not a change to the
  breakdown.
- Scenario goldens (2026-11-26): the shim's writer
  (`AGENT_REPL_SCENARIO_GOLDENS_WRITE=1` on `test/fake/goldens.test.ts`, run
  under `bin/background.sh`) removes each golden's directory whole, so it
  deletes the daemon's `feed.json` halves; those are rewritten by the same
  switch on `daemon/internal/scenariogolden` (`TestEveryScenarioGoldenProjectsToItsFeedGolden`).
  `agent-repld test run` does not pass the switch through, so both rewrites
  run by hand. The in-process writer and the real daemon disagree on
  `compact-failed`: the writer leaves `daemon.footer.on_context_cut` out of
  the feed golden's warnings while `TestScenarioGoldensRouteThroughARealDaemon`
  sees the daemon record it, so that file was kept at its committed version;
  the drift between the in-process projection and the daemon is a harness
  defect still to fix.
- A `daemon.deploy.run` record "a staged artifact could not be hashed" whose
  cause names a path in `staging/*/cache-bin/` that the staged tree lacks
  means a landing changed the staged layout, which the running daemon, built
  from the previous revision, reads (2026-11-26T09:02:38Z: the launcher
  renamed to `Zettel.app.staged`). The fix restores the staged path, never a
  second landing.
- A `daemon.feed.durable` record "a durable row carries fields this build has
  no descriptor for" after a landing that retires a proto field means that
  landing's layout row edit missed a stored shape; the fix is a NEW additive
  layout step with a row edit (the stamp already stands past the landing's
  step), never an edit of the landed step. The same holds for a
  `daemon.wsm.turn_starts` refusal naming a retired `PROMPT_ORIGIN_*`: the
  `turns.origin` text is carried onto the surviving origin closest in meaning
  (2026-11-26: `PROMPT_ORIGIN_MERGE_PICK` onto
  `PROMPT_ORIGIN_MERGE_CONFLICT_REPAIR`, layout 223). The live file can be
  checked read-only with sqlite before deciding.
- `daemon.wsm.*` "refused a write on a read-only handle" at a deploy's boot
  means a new writer runs on the joining daemon before its promotion; the fix
  is the store's held-write pattern (hold under `heldMu`, write in `Promote`),
  never a wait or a retry in the caller.
- A webapp `rpc.stream-frame-undecodable` or `footer.verdict-redraw-undecodable`
  whose path ends in `.tone` with the cause `'' is no color of
  render-colors.json` means a daemon writer composed a row before the sidebar
  decided the workspace's arm (the registry sink fires before any render); the
  fix is in the writer, which lists the row only once the arm is known, and the
  footer fails hard on an empty tone (2026-11-26, the children tracker).
- A webapp `feed.root-open-refused` with arm `notYetAdopted` right after a
  handover means a stream reopened on the successor before the page's in-place
  adoption answered; since 2026-11-26 every reopen waits behind the context's
  adoption latch (`DaemonContext.awaitAdopted`), so one seen again is a defect
  in that latch, never a reason to re-level the record.
- A `daemon.merge.remediate` error `has no live session to await turn` on a
  successor within seconds of its takeover, preceded by `daemon.merge.review`
  "takes up a merge no handover gave this daemon" for the same workspace and
  followed by `daemon.rollout.adopt` "adopted the workspace", means the
  takeover's queue review took up a merge an adoption was still bringing here;
  since 2026-11-26 the review leaves such merges to their adoption
  (`rollout.controller.adoptionHandsMerges`), so one seen again is a defect in
  that predicate, never a reason to re-level the record.
- A `daemon.wifirpc.client.watch` error "ended before it served a standing; it
  is not reopened" whose cause is `connect: no such file or directory`, at a
  boot and followed by `daemon.holder.ensure` installing
  `socket-holder-persistent-wifi`, means the standing watch dialed before the
  boot's standalone step had installed the holder that holds the service's
  socket; since 2026-11-26 the loop waits on that step's gate
  (`cmd/agent-repld` `standalones` service gate), so one seen again is a
  holder that could not be installed, whose own error record stands beside it,
  never a reason to re-level the record.
- A `daemon.deploy.services` "a service is not loaded and has no definition"
  record for a service whose stderr log then says its binary does not exist
  (launchd exit 127 every throttle interval) means a service landed whose
  first deploy was run by a daemon built before the service existed, so that
  deploy installed no binary; since 2026-11-26 the boot builds and installs
  the binary through `requirements.sh ensure-service` before the service is
  defined and started, so one seen again is a defect in that verb or its
  caller (`deploy.Restarter.ensureLoaded`).
- An Emacs `elisp.wire.contract-breach message=<Message> field=<arm>
  reason=unknown field` right after a landing that added that arm to the proto
  means the landing taught the daemon the arm and not Emacs: the elisp codec
  is hand-written (`lisp/wire-verbs.el`, `lisp/wire-host.el`), so a proto arm
  added to a message Emacs decodes lands with its codec change and tests, and
  the record is fixed by adding the arm, never by re-leveling.
