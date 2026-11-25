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
