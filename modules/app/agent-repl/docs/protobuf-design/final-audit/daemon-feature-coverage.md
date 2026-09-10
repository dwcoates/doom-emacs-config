# Daemon feature-coverage audit — `docs/overhaul/daemon.md` vs. the existing daemon

METHOD. `docs/overhaul/daemon.md` was read in full. The existing daemon
(`daemon/cmd/**`, all 38 packages under `daemon/internal/**`, ~106k non-test
lines) was swept in five clusters. Each behavior-bearing feature was checked
against the plan for accounting of ANY kind — a prescribed port, a
replacement, a DEAD-BY-DESIGN / DEAD-BY-RULING kill, a WONT-DO, an entry in
the OPEN triage backlog, or implicit subsumption by a prescribed component's
stated responsibilities. Trivial internals are NOT findings: the plan's
standing PRESCRIPTION DEPTH rule (daemon.md:11-16) leaves them to the
implementing orchestrator. Items already in the plan's OPEN section are
treated as ACCOUNTED (consciously tracked, awaiting a ruling).

A FINDING below is a real, behavior-bearing feature with NO accounting at
all. Paths are relative to `modules/app/agent-repl/`.

---

## FINDINGS

### 1. Interactive vendor account LOGIN — no way to ever log back in
- FEATURE: a full-screen Claude login TUI hosted on a daemon-owned pty,
  one per account config root, fanned to attached viewers with scrollback
  replay, 400-column geometry so the OAuth URL never hard-wraps, idempotent
  open so a second click cannot race a second OAuth flow, and closed at
  daemon shutdown so no TUI is stranded on a pty.
- EVIDENCE: `daemon/internal/login/login.go:1-22` (package doc: "the daemon
  owns the pty, the webapp renders it, and no code anywhere parses the
  TUI"), `:36-58`, `:69` (`StartFunc` keyed by `CLAUDE_CONFIG_DIR`), `:114`
  (`Attach`), `:381` (`Manager.Open`); transport at
  `daemon/internal/server/server.go:883` (`handleLogin`), `:905`
  (`handleLoginTerminal`, duplex + resize control frames), `:975`
  (`handleLoginClose`), `:226-228`; pty at `daemon/internal/pty/pty.go:45`,
  `:112` (`Setsize`); wiring at `daemon/cmd/claude-repld/main.go:589`,
  `:1355-1358`.
- WHY UNACCOUNTED: the plan's only account clauses are the topbar's
  ("logged-out is a drawn state, never blank", daemon.md:473-475) and the
  INVARIANT that the account is DETERMINED, never selected (daemon.md:485-491).
  Together they prescribe DRAWING the logged-out state and nothing that can
  ever RESOLVE it. The `agentrepl/v1` package map (daemon.md:744-763) carries
  no login verb, no terminal stream, and no section one would belong to. Not
  covered, not ruled dead, not OPEN.
- CONFIDENCE: **high** (independently found by three of five cluster sweeps).

### 2. The fresh-conversation PROOF GATE — the rule that a conversation is never silently replaced
- FEATURE: a shim may be started without `--resume` only against a proof
  object the daemon constructs from evidence that the workspace never had a
  conversation at all. A proof object rather than a boolean, because "start
  fresh" was once reachable from four unrelated places.
- EVIDENCE: `daemon/internal/server/freshgate.go:1-30` (the ruling verbatim:
  "A workspace's Claude conversation is not replaceable… the abandonment is
  silent and irreversible, while every alternative — resume it, restore it,
  or refuse loudly — is recoverable"), `:144-148` (`proveFreshEligible`, the
  only constructor), `:181` (`spawnShim` requires the proof); sibling
  disposition `daemon/internal/server/resumevanished.go:1-12`.
- WHY UNACCOUNTED: the plan's session-lifecycle section (daemon.md:810-844)
  covers resume, the cold gate, and hibernation but never states the
  no-fresh-spawn-over-an-existing-conversation rule, and the shim-client
  entry explicitly hands the decision away — "when to call StartSession is
  the implementing orchestrator's" (daemon.md:53-55) — without carrying the
  constraint with it. The most consequential silent-data-loss guard in the
  old daemon is delegated as an unconstrained choice.
- CONFIDENCE: **high**

### 3. Transcript backup and restore — the rung below the resume refusal
- FEATURE: copies of the vendor `.jsonl` taken at every turn end and at
  vendor-uuid rotation, stored beside the work at
  `<ws>/.claude/emacs/claude-session-backups/`, pruned, and restorable —
  so finding 2's refusal is the LAST rung, not the only one.
- EVIDENCE: `daemon/internal/server/transcriptbackup.go:1-30` (rationale
  verbatim), `:153` (`Capture`), `:170` (`CaptureConversation`), `:209`
  (`prune`), `:346` (`newestBackupConversation`); registrar wiring at
  `daemon/cmd/claude-repld/main.go:778-786` ("the registrar is the first
  thing to hear both boundaries worth copying at").
- WHY UNACCOUNTED: WSM's durable-fact inventory (daemon.md:78-105) and the
  git client (daemon.md:593-624) name no one who protects the one artifact
  the daemon cannot regenerate. The plan touches transcripts only for
  account-switch PORTING (daemon.md:481-484) and account probing
  (daemon.md:103). Under the rebuild a lost transcript becomes terminal.
- CONFIDENCE: **high**

### 4. Conversation replay for a SHIM-LESS workspace ("a read must cost a read")
- FEATURE: a hibernated, parked, or post-bounce workspace's conversation is
  served from the durable record with NO vendor process spawned — "a
  frontend mounting is not a reason to spawn a vendor process."
- EVIDENCE: `daemon/internal/sessioncontroller/durablereplay.go:1-45`, `:79`
  (`resyncFromDurableHistory`), `:205` (`stampDurableReplayUnwired`), `:263`
  (`durableConsumer`), `:335-337`; store-side reader
  `daemon/internal/storehistory/storehistory.go:1-22`.
- WHY UNACCOUNTED: the plan forbids the daemon from importing `store/v1` at
  all — an isolation gate (daemon.md:784-788) — and routes ALL history
  through `shim.v1 ReadHistory`, which requires a live shim. The only nearby
  clause is "parked (shim-less) workspaces need nothing — the next implicit
  revival spawns the new binary" (daemon.md:418), which is precisely the
  behavior this code refuses. No prescribed component can answer "show me
  this workspace's conversation" without starting a session.
- CONFIDENCE: **high**

### 5. The workspace command-file protocol's NON-MERGE verbs
- FEATURE: a durable file-based ingress by which scripts, skills, and any
  non-Emacs caller drive a workspace: `prompt`, `send`, `finish`, `close`,
  `open`, `switch`, `eval`, `clipboard`, `fold`, `set-view`, and the whole
  `task-*` family, with enqueue/drain/ack lifecycle.
- EVIDENCE: `daemon/internal/workspace/create/inbox.go:421` (the verb
  switch), `daemon/internal/workspace/create/store.go:160`
  (`EnqueueHostAction`), `:178`, `:191`, `:207`, `:228`;
  `daemon/internal/workspace/create/manager.go:981` (`DrainHostActions`),
  `:1005` (`CompleteHostAction`); emitter
  `daemon/internal/workspacecmd/workspacecmd.go:41-235`.
- WHY UNACCOUNTED: the plan deletes the host command loop
  (daemon.md:940) and rules on exactly ONE file-route verb — merge
  (daemon.md:224-226). The daemon→Emacs DIRECTION is correctly killed, but
  the file route's other verbs are an INGRESS, not a command loop, and they
  vanish with no replacement and no wont-do.
- CONFIDENCE: **high**

### 6. Creation-time configured merge actions have no PRODUCER
- FEATURE: `before_ws_merge` and `postprocessing_prompt` recorded on the
  creation job and read back by the merge run.
- EVIDENCE: `daemon/internal/workspace/create/types.go:130-131`;
  `daemon/internal/workspace/create/beforeaction.go:23`,
  `postprocessing.go:27`; consumers
  `daemon/internal/workspace/merge/coordinator.go:1568` (`runBeforeAction`),
  `:1087` (`runAfterAction`).
- WHY UNACCOUNTED: this is a plan-internal gap, not an omission of
  awareness. Entry 6 REQUIRES the facts ("both are read from the creation-job
  facts", daemon.md:205-208; "the configured prompts run for EVERY merge on
  EVERY ingress", daemon.md:288-291) and entry 3 stores them
  (daemon.md:84-86) — but the frozen `CreateWorkspaceRequest` carries only
  repository / initial_prompt / base_ref
  (`proto/src/agentrepl/v1/endpoint_create_workspace.proto:19-28`) and no
  other create ingress is ruled. The facts are prescribed as readable with
  nothing able to write them.
- CONFIDENCE: **high**

### 7. Durable KEEP-ALIVE WINDOWS — the fact that keeps a ping from rendering as a user turn
- FEATURE: `keep_alive_window(turn_id, workspace, started_at_ms,
  ended_at_ms)`, written BEFORE the ping reaches the shim and re-stamped
  with the vendor-clock instant, so live push and years-later replay reach
  the same verdict about which records belong to plumbing.
- EVIDENCE: `daemon/internal/statedb/keepalivewindow.go:9-14` ("THE DURABLE
  LEDGER OF WHEN THE DAEMON WAS PINGING… that decision must come out the
  same way on a live [push and a replay]"), `:78-84`, `:99-107`; exclusion
  consumers `daemon/internal/sessioncontroller/keepaliveexclude.go:1-20`.
- WHY UNACCOUNTED: the plan moves keep-alive SUBMISSION to the shim but
  explicitly keeps them "visible on the record plane… so paged history must
  tolerate them" (daemon.md:868-870) — it states the requirement and then
  removes every means of meeting it: WSM's inventory lists no keep-alive
  fact, and rule 15 forbids resolvers from persisting anything
  (daemon.md:633-643). Nothing durable can distinguish a ping on replay.
- CONFIDENCE: **high**

### 8. Vendor-record CURATION — the withholding family
- FEATURE: one chokepoint that suppresses records which must not become feed
  rows: CLI-intercepted slash-command bookkeeping, SKILL.md bodies folded
  into the Skill card, `<task-notification>` envelopes, the synthetic
  "No response requested." assistant record (observed 118 records across 76
  sessions), and context-cut exclusions.
- EVIDENCE: `daemon/internal/sessioncontroller/withhold.go:1-25` (the shared
  filter), `machinery.go:1-30`, `skillbody.go:1-25`,
  `tasknotification.go:1-30`, `noresponse.go:1-20`,
  `contextcutexclude.go:1-30`, `userrecord.go:1-20`.
- WHY UNACCOUNTED: the plan gives resolvers a FORMATTING mandate
  (daemon.md:308-312, 540-548) which does not obviously subsume SUPPRESSING
  records, and names the chokepoint nowhere. History pages still come from
  vendor records, so the user-visible defects these fix (page-long fake
  prompt bubbles, raw markup bubbles) return by default.
- CONFIDENCE: **med-high**

### 9. Kernel lock OWNERSHIP IS INVERTED, and the two-key scheme is unaccounted
- FEATURE: the SHIM takes two exclusive flocks at startup — one keyed by
  session id, one by workspace dir — because on a fresh daemon boot a
  surviving shim "may not have dialled in yet", so connection tracking
  answers NO when the truth is NOT YET. Only the workspace key catches two
  daemon session ids over one transcript. A platform without `O_EXLOCK`
  fails LOUDLY rather than reading as free.
- EVIDENCE: `daemon/internal/sessionlock/sessionlock.go:1-37` (rationale
  verbatim), `:62` (`Path`), `:77` (`WorkspaceKey`), `:34-37`.
- WHY UNACCOUNTED: the plan has the DAEMON hold and hand off the workspace
  lock ("releases the workspace's kernel lock" / "claims the lock",
  daemon.md:346-351; "arbitrated by the workspace kernel lock",
  daemon.md:448). A daemon-held lock structurally cannot answer the
  boot-window question the shim-held lock exists for, and the
  session-vs-workspace two-key distinction has no home. The plan correctly
  adopts the MECHANISM (self-releasing kernel locks) while silently
  reassigning the HOLDER.
- CONFIDENCE: **high**

### 10. The daemon is the webapp's ASSET ORIGIN, with the cache rule that makes hot swap observable
- FEATURE: the daemon serves the SPA and widget assets over HTTP;
  `index.html` is re-stat'd per request so a rebuild self-corrects with no
  restart; `Cache-Control: no-store` on the HTML entry point ONLY (the fix
  for webviews pinning a deleted bundle); a 503 diagnostic body naming cause
  and fix instead of a bare 404.
- EVIDENCE: `daemon/cmd/claude-repld/main.go:96-125`, `:137-157`, `:1300-1307`.
- WHY UNACCOUNTED: the rollout controller prescribes "webapp → hot asset
  swap" and the `reload_webapp` push (daemon.md:326, 384-389) without ever
  saying the daemon is where the assets come from, and the entry-point
  no-store rule — the thing that makes a hot swap actually take effect — has
  no counterpart. The prescribed rollout is unimplementable as stated.
- CONFIDENCE: **high**

### 11. Proactive WARM COMPACTION on the cache clock
- FEATURE: the daemon schedules a compaction a margin BEFORE cache expiry so
  the cold gate never fires expensively; motivated by an observed
  1.5M-uncached-token revival compaction.
- EVIDENCE: `daemon/internal/sessioncontroller/warmcompact.go:1-25`, `:105`
  (`warmCompactEligibleLocked`), `:176` (`SubmitWarmCompaction`); inputs
  `contextsize.go:1-20`, `turnresultcost.go:1-20`.
- WHY UNACCOUNTED: the plan has only the REACTIVE cold gate ("a cold context
  … is REFUSED with its cost named", daemon.md:826-829). Worse, it relocates
  keep-alives wholly to the shim (daemon.md:869-870), so the daemon no
  longer owns the cache clock this schedule rides on. Nothing prescribed
  schedules anything against cache lifetime.
- CONFIDENCE: **high**

### 12. Durable conversation PROVENANCE (user vs. merge) for replay
- FEATURE: each conversation item's source is decided against the merge
  lease's durable LEDGER by its own timestamp — "a LEDGER, not a flag" —
  so a replay years later reaches the verdict the live push reached.
- EVIDENCE: `daemon/internal/ssm/db.go:169-197`,
  `daemon/internal/ssm/mergelease.go:475` (`ConversationSourceAt`);
  `daemon/internal/sessioncontroller/provenance.go:1-28`, `:30`
  (`stampConversationProvenance`), `:54`.
- WHY UNACCOUNTED: the plan replaces this with the generic OUTPUT ADDRESS
  `{target feed, parent row}` "set on lease acquisition, updated per tab,
  cleared on release" (daemon.md:247-252) — purely LIVE state — plus "merge
  phase history is not stored state… feed content the daemon synthesizes on
  the fly" (daemon.md:296-297, 918-919). At replay time the address is gone
  and nothing says which historical rows belonged to a past merge bubble.
- CONFIDENCE: **high**

### 13. BOUNCE ACCOUNTABILITY by pid identity
- FEATURE: the outgoing daemon records which pid it leaves per session and
  what it intended; the incoming daemon judges that against the kernel's
  actual lock holders, with PRESERVED / ROLLED / DIED / **UNKNOWN** never
  collapsed. It exists because a total five-session fleet loss
  (2026-08-10 19:41) passed for a clean restart under process counting,
  while the outgoing daemon logged "every session shim is PRESERVED".
- EVIDENCE: `daemon/internal/bounceledger/bounceledger.go:1-29` (rationale
  verbatim), `daemon/internal/inflight/manifest.go:1-20`, `:127`
  (`Reconcile`), `:184` (`Tally`), `:213` (`Append`), `:252` (`Read`),
  `daemon/internal/inflight/nofollow_unix.go:1-14`; wiring at
  `daemon/cmd/claude-repld/main.go:678-700`, `:1212-1246` (manifest written
  BEFORE the ledger so a mid-write death cannot read as "interrupted
  nothing").
- WHY UNACCOUNTED: the plan's handover is graceful, freeness-gated, and
  lock-arbitrated (daemon.md:337-355, 442-445), which removes most of the
  loss CLASS — but nothing records the intent, and after a CRASH (explicitly
  contemplated at daemon.md:56-58, 443-445) or the force-kill failure
  disposition (daemon.md:419-421), nothing names which sessions silently
  died. "Counting is not accounting" has no successor.
- CONFIDENCE: **med-high**

### 14. Durable FAULTS and the resolved-at instant (the card that reopens forever)
- FEATURE: per-generation open/closed session faults carrying
  component/fault_type/impact, plus a persisted resolution instant — which
  exists because "without a persisted instant the card reopens, unresolved,
  on every boot forever" (the observed 22-open-blue-cards defect).
- EVIDENCE: `daemon/internal/ssm/db.go:147-166`,
  `daemon/internal/registry/registry.go:100-110` (`DeathResolvedAtMs`);
  window/resolution/reconciliation at
  `daemon/internal/server/deathwindow.go:1-12`, `deathresolve.go:1-12`,
  `deathreconcile.go:37` (`ReconcileOpenDeaths`), `supersedepresent.go:1-13`;
  boot-sweep door `daemon/internal/ssm/bootsweepverdict.go:1-33`.
- WHY UNACCOUNTED: the plan routes faults as typed diagnostics pushes into
  in-memory resolvers (daemon.md:512-514) and declares failure classification
  a non-component (daemon.md:626-632). WSM records "death/terminality with
  cause" (daemon.md:88) but nothing owns the CLOSING edge or a durable
  resolved instant, so a re-derived-every-push card that never resolves is
  reconstructible.
- CONFIDENCE: **high**

### 15. Boot EXCLUSIVITY claim — duplicate-daemon arbitration
- FEATURE: a claim taken on the TCP address before any unix socket is
  touched, so an accidental second daemon exits (code 3) with the
  incumbent's sockets untouched rather than unlinking them.
- EVIDENCE: `daemon/cmd/claude-repld/bootclaim.go:40-61`
  (`errIncumbentDaemon`), `daemon/cmd/claude-repld/main.go:379-397`
  (distinct exit codes 1/2/3).
- WHY UNACCOUNTED: the plan DELIBERATELY runs two daemons at once
  (daemon.md:330-355) with per-workspace kernel locks, and never says how an
  accidental duplicate — as opposed to a joining one — is refused, nor what
  ordering keeps a loser from destroying the incumbent's listeners. The
  rollout makes this harder, not easier.
- CONFIDENCE: **high**

### 16. The restart announcement loses cause, bound, and stop-shims
- FEATURE: a validated announcement carrying CAUSE, `stop_shims`, a BOUNDED
  expected outage (clamped at 5 min), and a minted-at instant so a late
  client SHORTENS rather than restarts its quiet window; fanned to all
  transports, with "nowhere to send it" a loud failure. It exists because a
  deliberate bounce was indistinguishable from a crash and painted the
  severed-link banner every time.
- EVIDENCE: `daemon/internal/restartannounce/restartannounce.go:1-27`
  (rationale verbatim), `:48`, `:50-53` (`ErrNoSinks`), `:55-77`; emission
  `daemon/cmd/claude-repld/restartannouncement.go:24-44`,
  `daemon/cmd/claude-repld/main.go:1349`.
- WHY UNACCOUNTED: the plan's wire is `shutdown_announced { address }` on
  `WatchDaemon` (daemon.md:356-358) — address only. No cause, no bound, no
  stop-shims flag, no loud no-sinks failure, and no arm at all for a PLAIN
  restart that is not a blue-green handover. The announcement survives; its
  entire content does not.
- CONFIDENCE: **med-high**

### 17. Fan-wide detached-agent CANCEL and the interrupt confirmation challenge
- FEATURE: `CancelDetachedAgents` as its own verb with a typed outcome, and
  a two-phase interrupt: `InterruptConfirmRequired` returns ok=false with
  failure and error UNSET, answered by resending the interrupt with
  `confirm_agents:true`.
- EVIDENCE: `daemon/internal/frontend/commands.go:40`, `:372-391`; handlers
  `daemon/internal/server/frontendcmd.go:942`, `:1093`.
- WHY UNACCOUNTED: the FEED section lists only `Interrupt`
  (daemon.md:751) and the contract states "narrow stops stay single-target"
  (daemon.md:820-822). The fan-wide stop and its are-you-sure round trip
  have no verb and no derived error arm.
- CONFIDENCE: **med-high**

### 18. REWIND / undo lineage — three durable columns and a corruption rule
- FEATURE: `rewind_previous_vendor_session_id`, `rewind_retained_leaf_uuid`,
  `rewind_dropped_turn_ids`, with a PARTIAL lineage refused rather than
  carried (the shim rejects an empty dropped-turn list).
- EVIDENCE: `daemon/internal/registry/schema.go:54-57`, `:233-241`;
  `daemon/internal/sessioncontroller/rewind.go:1-25`,
  `daemon/internal/session/rewind.go:1-12`.
- WHY UNACCOUNTED: daemon.md mentions rewind ONLY as the shim's keep-alive
  discard mechanic (daemon.md:869). A user-facing undo feature with three
  durable columns and a refuse-on-partial rule has no entry, no inventory
  line, and no DEAD ruling.
- CONFIDENCE: **med-high**

### 19. Permission DECLINE = STOP semantics
- FEATURE: declining a permission ends the turn, pauses the queue, opens the
  interrupt window, and lets nothing further reach the SDK — deliberately
  NOT a DENY the agent routes around.
- EVIDENCE: `daemon/internal/sessioncontroller/permdecline.go:1-30`, `:98`
  (`declinePermissions`), `:149` (`declinePendingPermissionsForPrompt`),
  `:174` (`releaseParkedPermissionsOnStop`).
- WHY UNACCOUNTED: the plan lists `AnswerPermission` as a verb
  (daemon.md:748) and says "the landed permission/question API (fully
  webapp-side) is the only path" (daemon.md:659-661). The turn-ending
  CONSEQUENCE of a decline — the semantic that was once wrong — is stated
  nowhere.
- CONFIDENCE: **med-high**

### 20. The disposition of the existing `state.db` is never decided
- FEATURE / FACT: on disk today are an SSM schema at v7 with a
  non-idempotent stamp-gated data migration, an independently stamped
  registry schema at v1, a once-stamped legacy JSON import, and a seeding
  migration whose absence makes every existing session immortal.
- EVIDENCE: `daemon/internal/ssm/db.go:26`, `:281-300`, `:344-397`
  (`normalizeSessionIdentity`); `daemon/internal/registry/registry.go:49`;
  `daemon/internal/registry/schema.go:88-105`, `:145-167`;
  `daemon/internal/registry/legacy.go:14-18`.
- WHY UNACCOUNTED: the plan nukes the STORE explicitly and repeatedly
  (daemon.md:590-591, 999) and says nothing about the daemon's OWN database,
  while simultaneously requiring a layout version and an
  older-daemon-refuses-newer-file rule (daemon.md:126-129) that implies
  continuity. Nuke-vs-carry is a binding decision the plan does not make.
- CONFIDENCE: **med-high**

### 21. No RETENTION or growth bound on any WSM table
- FEATURE: terminal records retained to a cap (128) with live records never
  pruned, and a measured bring-up cost behind the design.
- EVIDENCE: `daemon/internal/registry/registry.go:51-54`
  (`TerminalRetention`), `:795` (prune site), `:521-523`;
  `daemon/internal/registry/schema.go:300-315` (the 2.9ms x 4000 measurement).
- WHY UNACCOUNTED: WSM is prescribed with seven tables, a layout version,
  and several INVARIANTs (daemon.md:61-140) but no retention, pruning, or
  growth bound for anything.
- CONFIDENCE: **med-high**

### 22. Per-workspace log TARGETS and the daemon run-log durability plane
- FEATURE: shared-fd per-workspace log targets with a 64 MiB cap and 30s
  maintenance, eviction on workspace close, and a non-closeable BORROW
  (because one caller's Close poisons the inode for every other writer and
  the next spawn inherits a closed fd 3); plus a restart-scoped run log with
  5 retained backups and a 1 GiB in-run cap, whose failure to open is a boot
  fatal.
- EVIDENCE: `daemon/internal/dlog/dlog.go:597-641`, `:912`
  (`MaintainSizeCaps`), `:1012` (`EvictWorkspace`);
  `daemon/internal/dlog/logtarget.go:9-33`;
  `daemon/internal/replog/replog.go:1-15`, `:31-39`, `:45`, `:84`;
  `daemon/cmd/claude-repld/main.go:296-306`, `:339-343`, `:1560-1578`;
  client-log landing `daemon/internal/server/frontendcmd.go:1897`, `:1910`,
  `:1979`, `:2006`, `:2016`, `:1415`.
- WHY UNACCOUNTED: the plan's only logging clauses are an EMISSION policy —
  "every logical branch gets a DEBUG log; warnings are remediated to zero"
  (daemon.md:1002) and rate-limited refusal logging (daemon.md:317-319) —
  never a log SURFACE architecture. `ClientLog` is named as an rpc
  (daemon.md:754) with nothing said about where a client line lands.
- CONFIDENCE: **med-high**

### 23. Terminal-mirror decoupling from the durable sink (the 6957ms boot pathology)
- FEATURE: one shared terminal sink keeping a single FIFO order with a
  flushing Close, so a pty Emacs that stops draining cannot block every
  emitter behind one held mutex.
- EVIDENCE: `daemon/cmd/claude-repld/main.go:307-330`,
  `daemon/internal/dlog/terminal.go`.
- WHY UNACCOUNTED: a latency-CORRECTNESS property of daemon logging sitting
  on the command critical path, with no clause anywhere in the plan.
- CONFIDENCE: **med-high**

### 24. Skill-scoped detached WINDOWS (temporal-membership bubbles)
- FEATURE: bubbles whose membership is TEMPORAL, not stream-keyed — the
  session's own top-level records between a Skill invocation and the user's
  next prompt or interrupt, folded and settled at those edges.
- EVIDENCE: `daemon/internal/sessioncontroller/detachedwindows.go:1-30`
  ("two kinds, one rule; membership is TEMPORAL"), `:184`
  (`openSkillWindow`), `:494` (`foldWindows`), `:632`
  (`settleWindowsOnPrompt`), `:649` (`settleWindowsOnInterrupt`); preview
  routing `foldedtyping.go:1-25`.
- WHY UNACCOUNTED: the plan's detached model is strictly stream-keyed —
  "one stream per detached item; zero item streams structurally IS 'no
  detached work in flight'" (daemon.md:863-865), with the sessionwatcher
  opening one watch per live item (daemon.md:497-501). A window has no
  stream and no key. The plan's OPEN list carries only "the agent-driven
  merge-skill detached window" (daemon.md:670); the GENERIC every-Skill
  window is unaccounted.
- CONFIDENCE: **med-high**

### 25. `/open-external` — the pinned external-browser escape hatch
- FEATURE: the daemon opens clicked links in a pinned Chrome profile,
  because a click inside the xwidget would otherwise navigate the webview
  away; activate-then-hand-off ordering with three distinct errors.
- EVIDENCE: `daemon/internal/externalbrowser/externalbrowser.go:1-25`,
  `:36-48` (pinned `Profile 6`, must match `lisp/external-browser.el`),
  `:96-117`; route
  `daemon/internal/server/server.go:557-580`, `:547`, `:513`.
- WHY UNACCOUNTED: the `agentrepl/v1` section map (daemon.md:744-763) has no
  link-open verb and no section one would belong to. Comment states it "is
  the ONLY thing standing between a clicked link and nothing happening at
  all."
- CONFIDENCE: **med**

### 26. The agent-authored widget / chess-game capability surface
- FEATURE: a path-confined reader for payloads an agent writes into
  `.claude/emacs/cee-web-widget`, plus a capabilities endpoint advertising
  `widget_assets`, `widget_assets_dir`, `widget_bundle_present`.
- EVIDENCE: `daemon/internal/server/server.go:640-642`, `:813`
  (`handleChessGameFile`), `:1092` (`handleCapabilities`), `:205-208`.
- WHY UNACCOUNTED: no plan clause serves agent-authored files to the webview
  or advertises optional daemon capabilities; the `frontend/v1` file list
  (daemon.md:764-768) has no arm for it.
- CONFIDENCE: **med**

### 27. `add-support` — turning a CLI-refused slash command into a workspace
- FEATURE: the daemon composes a brief and requests a workspace whose job is
  to build a graphical rendering for a slash command the CLI refuses,
  validating the name against the CLI's own charset.
- EVIDENCE: `daemon/internal/addsupport/addsupport.go:1-14`, `:36-70`;
  `daemon/internal/server/server.go:644-706` (`handleAddSupport`, emits a
  `workspacecmd.NewCreate`).
- WHY UNACCOUNTED: the slash-panel list (daemon.md:767) enumerates the
  SUPPORTED panels and never says what happens to an unsupported command;
  the flow's ingress dies with finding 5's file route and daemon.md:940.
- CONFIDENCE: **med**

### 28. Boot-sweep verdicts SURFACED, not merely logged
- FEATURE: two boot probes (parked connection, session lock) whose verdicts
  are fanned onto the pushed state — "where a boot-sweep verdict STOPS BEING
  A LOG LINE" — so a row is not left on the anonymous `daemon_restart` cause.
- EVIDENCE: `daemon/internal/server/bootsweep.go:1-30`,
  `daemon/internal/server/bootsweepverdict.go:1-12`;
  `daemon/cmd/claude-repld/main.go:1428-1470`.
- WHY UNACCOUNTED: the CRASH BOOT clause (daemon.md:56-58) covers ADOPTING
  survivors. It says nothing about the sessions the sweep finishes and
  cannot wire, nor about telling the user why.
- CONFIDENCE: **med**

### 29. Opt-in local-only PPROF surface
- FEATURE: a profiling surface on a unix socket or an explicitly-loopback
  TCP address (wildcard binds refused at construction), opened BEFORE
  dependency boot so a WEDGED boot is still profilable, with both on and off
  states recorded and a configured-but-unbindable surface fatal.
- EVIDENCE: `daemon/internal/pprofsurface/pprofsurface.go:1-33`;
  `daemon/cmd/claude-repld/main.go:369`, `:436-441`, `:1483-1512`.
- WHY UNACCOUNTED: the plan carries a `DaemonHealth` wedged-publisher
  watchdog (daemon.md:961) and no debug or profiling surface anywhere — the
  wedge is detectable but not diagnosable.
- CONFIDENCE: **med**

### 30. The three runtime-free CLI subcommands
- FEATURE: `create-workspace`, `list-workspaces`, `list-transcripts`,
  dispatched before any daemon bootstrap, with their own flags and a stated
  safety argument for bypassing job persistence.
- EVIDENCE: `daemon/cmd/claude-repld/workspace_create_cli.go:30`, `:68-85`,
  `:216`; `daemon/cmd/claude-repld/workspace_list_cli.go:43`;
  `daemon/cmd/claude-repld/workspace_transcripts_cli.go:51`;
  `daemon/cmd/claude-repld/main.go:277`.
- WHY UNACCOUNTED: the plan describes the daemon PROCESS and its rpc surface
  and never mentions that the binary has a command-line surface at all.
- CONFIDENCE: **med**

### 31. User-editable automatic-prompt FILES
- FEATURE: the daemon's synthesized agent briefs — conflict resolution,
  test-fix with its escalation marker, add-support — live in
  `modules/app/agent-repl/prompts` and are read at USE time, so they can be
  customized without a rebuild, with a missing file or typo'd placeholder
  loud.
- EVIDENCE: `daemon/internal/prompts/prompts.go:1-20`, `:33-38`
  (`AGENT_REPL_PROMPTS_DIR`);
  `daemon/internal/workspace/merge/conflictresolver.go:66-88` ("moved to
  prompts/ so it can be customized without a rebuild");
  `daemon/internal/workspace/merge/testfailureresolver.go:80-105`.
- WHY UNACCOUNTED: entry 6 prescribes the remediation loops and the agent's
  escalation record (daemon.md:209-213) but never that the briefs are
  user-editable files; the merge's CONFIGURED prompts come from per-workspace
  creation-job facts (daemon.md:205-208), which is a different plane. A
  rebuild would hard-code them, silently removing a customization surface.
- CONFIDENCE: **med**

### 32. VANISHED-RESUME fence and RESUME IDENTITY verification
- FEATURE: (a) a resume whose vendor transcript file was DELETED is refused
  before any process exists; (b) the resumed query is verified to have
  landed on the exact conversation asked for, with `/clear` discharging the
  commitment.
- EVIDENCE: `daemon/internal/sessioncontroller/vanishedresume.go:1-30`,
  `:69` (`vanishedResumeRefusal`), `:98` (`fenceVanishedResume`), `:229`,
  `:290`; `daemon/internal/sessioncontroller/resumeidentity.go:1-24`, `:39`,
  `:83`, `:151-160` (`resumeIdentityMismatchError`); ladder context
  `bringupescape.go:1-63`, `bringupretry.go:1-40`.
- WHY UNACCOUNTED: the shim-client ruling makes give-up EVIDENCE-based — "a
  dead process stops the redial and surfaces" (daemon.md:46-49) — but a
  vanished transcript produces NO death evidence, so the prescribed ladder
  never runs and the redial-forever rule yields an unbounded loop over an
  unchangeable fact. WSM's "a deleted session REFUSES resurrection"
  (daemon.md:89-90) is about a session RECORD, not the file. Resume coverage
  in the plan stops at model and permission mode (daemon.md:839-844).
- CONFIDENCE: **med**

### 33. Git ENVIRONMENT hygiene — `-C dir` as the only repository selector
- FEATURE: one place strips inherited repository-selecting env vars
  (`GIT_DIR` and family), because git hooks export them into children.
- EVIDENCE: `daemon/internal/gitexec/gitexec.go:1-6` (the guarantee stated
  as the package's purpose), `:16-41` (`StrippedVars`, `Command`,
  `StripEnv`).
- WHY UNACCOUNTED: entry 14a prescribes the git client's ACTIONS in detail
  (daemon.md:593-624) and never the env boundary. An inherited `GIT_DIR`
  makes the leaf client operate on the wrong repository — a correctness
  invariant, not a trivial internal.
- CONFIDENCE: **med**

### 34. Feed page position: the plan's keying CONTRADICTS the working rule
- FEATURE: today the position is keyed PER READER per workspace ("two tabs
  scrolled to different depths cannot read each other's place"), carries the
  controller generation so a rotation invalidates structurally, and every
  row is DROPPED AT OPEN because a surviving row could be inherited by an
  unrelated later connection.
- EVIDENCE: `daemon/internal/ssm/readerposition.go:9-40`;
  `daemon/internal/ssm/ssm.go:375-378`; cursor/session binding
  `daemon/internal/sessioncontroller/pagecursor.go:1-25`, `:33-45`.
- WHY UNACCOUNTED: the plan's inventory says the opposite — persist per
  WORKSPACE and SURVIVE the restart (daemon.md:91-96) — and asserts one
  webview per workspace (daemon.md:994). The inherited-position hazard the
  drop-at-Open rule exists for, and the invalidation of an outstanding walk
  by `/clear` or compaction, are both unaddressed.
- CONFIDENCE: **med**

### 35. `turn_interruption` — the replay tombstone for a daemon-killed turn
- FEATURE: keyed by STORE coordinate (`event_session_id`, `turn_id`) rather
  than claimant, because a re-minted session replays the same stream and
  would otherwise reconstruct the dead turn as a fresh open claim.
- EVIDENCE: `daemon/internal/ssm/db.go:120-135`,
  `daemon/internal/ssm/turninterruption.go:1-31`;
  `daemon/internal/ssm/turnclaims.go:709` (`recordTurnStart` admits and
  closes in one statement).
- WHY UNACCOUNTED: the plan makes liveness structural via open watches
  (daemon.md:497-501) and rules the lifecycle log dead (daemon.md:132-138),
  yet the daemon still replays history from durable sources on restart
  (daemon.md:817-819). The replay-resurrects-a-killed-turn class has no
  structural answer.
- CONFIDENCE: **med**

### 36. TYPING-CUT — retiring a preview nothing will ever complete
- FEATURE: an explicit CUT so a client retires a permanently-spinning
  "streaming input…" card when a torn-down query will never send a terminal.
- EVIDENCE: `daemon/internal/sessioncontroller/typingcut.go:1-40`, `:42`
  (`notePreviewOpened`), `:57` (`cutOpenPreviews`); destination
  `foldedtyping.go:1-25`.
- WHY UNACCOUNTED: the plan's self-correction story requires a terminal —
  "the terminal arm restates the WHOLE text, so a lost fragment
  self-corrects" (daemon.md:853-855) — and calls a terminal-less stream "the
  transport failure" (daemon.md:873) while assigning no one the job of
  emitting the cut. The plan also denies the client a timer, so nothing on
  either side can retire the card.
- CONFIDENCE: **med**

### 37. Creation-request facts with no home: PRIORITY, FORK-FROM, and the ungated-permission consent flag
- FEATURE: (a) a `p05|p1|p2|p3` priority label; (b) creating a workspace
  whose session forks/resumes another conversation; (c) a refusal of any
  session whose permission mode disables `canUseTool` unless the caller
  explicitly set `allow_ungated`.
- EVIDENCE: `daemon/internal/workspace/create/types.go:77-101`, `:111`
  (priority); `:112-113` and `manager.go:752`, `types.go:388` (fork);
  `:129` and `daemon/internal/server/server.go:1381-1394` (the consent gate).
- WHY UNACCOUNTED: entry 14a's CREATION requirements cover slug, branch, and
  base ref only (daemon.md:602-608), the WSM creation-job inventory
  (daemon.md:84-86) lists none of the three, and the plan's resume story is
  restart-resume, never fork. The consent gate is a SAFETY refusal whose
  underlying fact has no ingress.
- CONFIDENCE: **med** (priority/fork), **med** (consent gate)

### 38. Layer-2 wire-version handshake
- FEATURE: a protocol version carried in every hello and in the
  `GET /sessions` envelope so a client surfaces a mismatch instead of
  mis-parsing.
- EVIDENCE: `daemon/internal/protocol/layer2.go:1-8`.
- WHY UNACCOUNTED: the plan versions only the DATABASE file
  (daemon.md:126-129). The blue-green handover deliberately runs two daemon
  BUILDS against one Emacs simultaneously (daemon.md:321-460) — exactly the
  case this check exists for — with no client/daemon version surface.
- CONFIDENCE: **med-low**

### 39. "Corrupt durable state ⇒ refuse to serve" as a GENERAL rule
- FEATURE: every registry read path logs "refusing to serve" and returns an
  error — an unnameable hibernation cause, an empty session id, or a partial
  rewind lineage each refuses the whole load rather than fabricating an
  empty.
- EVIDENCE: `daemon/internal/registry/schema.go:181-249`;
  `daemon/internal/registry/registry.go:598` (`sticky`).
- WHY UNACCOUNTED: the plan states this for HELD PROMPTS only (ALL-OR-NOTHING
  HOLD RESTORE, daemon.md:122-125). The other six tables get no such rule,
  so the rebuild's default is a silent empty.
- CONFIDENCE: **med**

### 40. Model-observation ORDERING (the two-authority race)
- FEATURE: `Model` has two writers — the shim's confirmation and the SDK's
  `SystemInit` re-announcement — and carrying generation + ordinal + a
  "true as of" instant is what makes "an in-flight submit silently reverts
  the user's model change and the next respawn pins it" unrepresentable.
- EVIDENCE: `daemon/internal/registry/modelobservation.go:1-27`; bare
  `/model` readback at
  `daemon/internal/sessioncontroller/modelreadback.go:1-30`, `:41`.
- WHY UNACCOUNTED: the plan routes `/model` and `SetModel` to one queue
  meeting point (daemon.md:157-164), which resolves COMMAND-vs-PICKER
  divergence — a different race from CONFIRMATION-vs-STREAM-RE-ANNOUNCEMENT.
  The argument-less `/model` form, which opens the CLI's own picker and
  names nothing, has no readback path either.
- CONFIDENCE: **med**

### 41. Durable prompt ORIGIN and closing edges for unwatched daemon-submitted turns
- FEATURE: `prompt_origin` persisted on every parked prompt, and each
  machine origin (merge-resume, workspace-create initial prompt, keep-alive
  ping) given its OWN closing edge — because "the daemon submits turns that
  nothing watches" and the claim otherwise stands forever.
- EVIDENCE: `daemon/internal/statedb/shutdownschedule.go:79-90`;
  `daemon/internal/ssm/turnorigin.go:8-30`.
- WHY UNACCOUNTED: the prompt queue is "the ONE path for ALL session-bound
  deliveries — prompts from every origin" (daemon.md:157-164) but records no
  origin, and no prescribed component supplies a terminal for a turn no
  frontend is watching.
- CONFIDENCE: **med**

### 42. The deploy chain's ordering constraints and the `proto` component
- FEATURE: reload deliberately owns NONE of the build, delegating to
  `bin/deploy-all.sh` — including the recorded store-before-sidecar ordering
  and the Emacs-not-running deferral — specifically to avoid two drifting
  deploy paths; classification covers SIX components including `shim-store`,
  `shim-claude-sidecar`, and `proto` (whose regeneration feeds the others).
- EVIDENCE: `daemon/internal/reload/reload.go:11-23`;
  `daemon/internal/reload/classify.go:17-28`.
- WHY UNACCOUNTED: the rollout controller enumerates four handled subsystems
  and rules store/sidecar UNHANDLED (daemon.md:321-328) but never says WHO
  rebuilds, in what ORDER, or what a proto-only change means — while entry 6
  requires the self-reload to "classif[y] the landed range by changed
  subsystem prefixes and restart ONLY what changed" (daemon.md:238-241).
- CONFIDENCE: **med-low**

### 43. Durable terminal cards, owed-work receipts, and conversation checkpoints
- FEATURE: (a) one durable standing terminal failure card per session,
  replaced so a re-fence is idempotent in history; (b) pending-resumption
  receipts for a turn owed a re-drive across a planned bounce, with a
  claim/discharge state machine; (c) a replay cursor keyed by CONVERSATION
  identity (`config_dir, cwd, claude_session_id`) that outlives its session
  record, with a never-downgrade backfill ladder.
- EVIDENCE: `daemon/internal/statedb/terminalcard.go:73-85`;
  `daemon/internal/statedb/promptreceipt.go:28-60`, `:181`
  (`RecordPendingResumption`), `:292`, `:321`, `:359`;
  `daemon/internal/registry/schema.go:62-71`;
  `daemon/internal/registry/registry.go:684-688`.
- WHY UNACCOUNTED: (a) failure classification is "NOT a component" with
  entry-correlated failures on the row's own error arm
  (daemon.md:626-632, 944-949) — no durable card, no re-fence idempotence.
  (b) teardown never interrupts the vendor (daemon.md:316-319) covers the
  normal path, but the force-kill FAILURE DISPOSITION (daemon.md:419-421)
  destroys an in-flight turn with no durable owed-work record. (c) the plan
  has "its persisted opaque history pointer" (daemon.md:818) and the
  same-uuid-under-two-accounts rule (daemon.md:103-104), but no fact that
  survives record pruning and no backfill ladder.
- CONFIDENCE: **med**

### 44. Operator-facing surfaces and boot observability
- FEATURE: `GET /healthz` as an out-of-band readiness probe distinct from
  the `DaemonHealth` rpc, deliberately ordered ahead of reconciliation;
  boot-phase timing marks including deferred post-ready phases (so a boot
  that outlasts Emacs's startup-restore budget names its phase without a
  bisect); the listener bind-before-dependency-boot split (~2s of every
  bounce, so a redialling Emacs lands in the accept backlog); daemon-binary
  staleness reported from a boot-time mtime snapshot; the
  `token-utilization-audit` audit/quarantine/delete tool with its
  `-apply` gate and `OpenExisting` write-mode boundary.
- EVIDENCE: `daemon/cmd/claude-repld/main.go:62-84`, `:1288`, `:1428-1434`;
  `daemon/cmd/claude-repld/bootphase.go` and the `phases.Mark` chain at
  `main.go:398-401,434,517,553,921,1023,1191,1327`, `:1401,1422,1452`;
  `main.go:404-435`, `:1315-1326`; `main.go:159-175`, `:357`, `:1184`;
  `daemon/cmd/token-utilization-audit/main.go:1-3`, `:26-31`, `:53-59`;
  `daemon/internal/statedb/statedb.go:53-66` (`OpenExisting`).
- WHY UNACCOUNTED: the plan prescribes `DaemonHealth` as an rpc
  (daemon.md:754, 961) and a read-only inspection open (daemon.md:126-129),
  and nothing else in this family. Note the SHIM half of staleness IS
  accounted ("the build-staleness bounce, one engine two triggers",
  daemon.md:390-391); the DAEMON half — a daemon stale for any reason other
  than a self-merge — is not.
- CONFIDENCE: **med-low**

### 45. Whole-daemon modes and guards: `-fake`, the vendor-call guard, `AGENT_REPL_STATE_DIR`, `AGENT_REPL_OWNED`
- FEATURE: an offline scripted-SDK mode forced onto every session including
  respawns; a hard refusal at every vendor exec site under
  `AGENT_REPL_FORBID_VENDOR_CALLS`; a cross-process state-root contract that
  Emacs watches and skills write into (a daemon resolving it differently
  drops messages SILENTLY); and the `AGENT_REPL_OWNED` marker propagated
  through the shim spawn env so vendor hook scripts can recognize
  agent-repl-owned processes.
- EVIDENCE: `daemon/cmd/claude-repld/main.go:364`, `:855-857`, `:1186`
  (`-fake`); `daemon/internal/vendorguard/vendorguard.go:21-25` and its use
  at `daemon/internal/login/login.go:346-356`;
  `daemon/internal/stateroot/stateroot.go:1-13`, `:22-26` with
  `main.go:293,966-971,1005`; `daemon/internal/shim/proc.go:495-500`.
- WHY UNACCOUNTED: none of the four appears in the plan. The state root
  matters most: the plan places durable state in WSM/SQLite
  (daemon.md:718-721) and never names the filesystem root shared with Emacs
  and the skills, whose divergence fails silently.
- CONFIDENCE: **med-low**

### 46. Merge terminal-word durability and the post-merge teardown disposition
- FEATURE: (a) a merge whose TERMINAL could not be published survives the
  bounce and is re-said exactly once (mark / pending / watermark /
  bounded replay-and-eject); (b) post-merge teardown HIBERNATES the merged
  workspace's session rather than deleting it, because "its conversation is
  still the account of how the work got written."
- EVIDENCE: `daemon/internal/workspace/merge/queue.go:503` (`MarkTerminal`),
  `:543` (`PendingTerminal`), `:465` (`RecordStatusWatermark`);
  `daemon/internal/workspace/merge/coordinator.go:936`
  (`keepForTerminalReplay`), `:1378` (`replayTerminal`), `:1434`
  (`ejectTerminal`);
  `daemon/internal/sessioncontroller/mergedteardown.go:1-20`.
- WHY UNACCOUNTED: (a) is arguably subsumed by "an in-flight merge across a
  daemon restart is resumed or LOUDLY failed" (daemon.md:296-299), but the
  exactly-once re-say guarantee loses its store when phase history becomes
  feed content (daemon.md:296-297). (b) the plan's teardown removes the
  worktree and joins the close blockers (daemon.md:270-272) and never says
  what becomes of the SESSION.
- CONFIDENCE: **med-low**

### 47. Workspace-key CANONICALIZATION as a durable-identity rule
- FEATURE: a trailing separator once minted a PHANTOM workspace with its own
  session that "enumerates as live, runs keep-alive machinery, squats the
  workspace's locks", requiring an idempotent boot consolidation.
- EVIDENCE: `daemon/internal/registry/phantomkey.go:12-30`.
- WHY UNACCOUNTED: "a path is never an identity — Register/Create return the
  id" (daemon.md:795-797) plus `RegisterWorkspace` idempotent-by-dir
  (daemon.md:757) subsumes the PREVENTION — but "idempotent by dir" is only
  true under a stated canonicalization, and none is stated.
- CONFIDENCE: **low-med**

### 48. Additive-DDL doctrine and two schema stamps with different semantics
- FEATURE: SSM stamps at v7 and refuses newer AND runs a gated data
  migration when older; the registry stamps independently at v1 and refuses
  newer only; and an explicit doctrine that additive
  `CREATE TABLE IF NOT EXISTS` tables and nullable-defaulted columns need NO
  version bump.
- EVIDENCE: `daemon/internal/ssm/db.go:26`, `:281-300`;
  `daemon/internal/registry/registry.go:49`;
  `daemon/internal/registry/schema.go:88-105`;
  `daemon/internal/statedb/promptreceipt.go:125-131`,
  `keepalivewindow.go:68-73`, `shutdownschedule.go:31-35`.
- WHY UNACCOUNTED: the plan says "the database file carries its layout
  version" (daemon.md:126-129) — singular, one owner, and with no
  additive-change doctrine, which is the rule that decides when a bump is
  even owed.
- CONFIDENCE: **med-low**

### 49. Command ack-latency instrumentation and the boot-time stale-rebase sweep
- FEATURE: (a) per-workspace command ack-latency recording against an
  env-configured warn threshold, armed at boot and fatal on a bad knob — the
  instrumentation that found finding 23; (b) a boot sweep of stale merge
  rebase worktrees (~190 leftover dirs observed).
- EVIDENCE: `daemon/cmd/claude-repld/main.go:1033-1053`
  (`frontend.AckWarnFromEnv`, `CommandAckDeadline`);
  `daemon/internal/workspace/merge/rebasesweep.go:119-147`
  (`SweepOrphanRebaseWorktrees`), `daemon/internal/workspace/merge/merge.go:574`.
- WHY UNACCOUNTED: (a) `DaemonHealth` is a wedged-PUBLISHER watchdog
  (daemon.md:961), not per-command latency. (b) the rebase temp-worktree
  CLASS is correctly dead under "never cherry-pick, never rebase"
  (daemon.md:610-611) — listed only because the plan never states that the
  janitorial guarantee retires with it, and nothing sweeps the new landing's
  residue.
- CONFIDENCE: **low-med**

### 50. Restart-EPOCH exclusion from wall-clock failure bounds
- FEATURE: every failure bound in the session package is a wall-clock
  comparison, and "a wall clock does not know a bounce happened" — so a
  planned replacement window is excluded from all of them.
- EVIDENCE: `daemon/internal/sessioncontroller/restartepoch.go:1-45`.
- WHY UNACCOUNTED: the blue-green handover forbids a daemon↔daemon channel
  and shares no clock (daemon.md:329-355, 442-445); the successor inherits
  durable claims and instants. Freeness-gated transfer removes most spurious
  overdue cases, which is why this ranks last, but the general rule has no
  home.
- CONFIDENCE: **low-med**

---

## CHECKED AND ACCOUNTED

Verified as covered by the plan — explicitly ported, explicitly replaced,
explicitly ruled dead, or necessarily subsumed by a prescribed component.

**Ruled DEAD BY DESIGN / DEAD BY RULING (confirmed present in the old tree,
confirmed killed in the plan)**
- Compaction-gate instants (`ssm/db.go:222-239`, `ssm/compactiongate.go`,
  `sessioncontroller/compactiongate.go`) — daemon.md:97-98.
- Append-only multi-axis `workspace_state` lifecycle log (`ssm/db.go:66-91`,
  `ssm/resolve.go`) — daemon.md:132-138.
- The SECOND held-prompt store, `session_record.queued_prompts`
  (`registry/schema.go:44`) — daemon.md:130-133.
- Create-time explicit account selection / `ConfigDirOverride`
  (`registry/registry.go:75-93`) — daemon.md:485-491.
- The `doom-multi-repo-mode` toggle — daemon.md:676-679.
- Boot-time repair of missing merge geometry (`geometry/derive.go:15`,
  `geometry/backfill.go:19`, `main.go:936-963,1396-1411`) — daemon.md:674-677.
- Cherry-pick / rebase landing and the branch-ref move
  (`merge/merge.go:1632`, `:1711`) — daemon.md:610-611.
- The cherry-pick-annotation landed-range walk (`reload/landed.go:10-25`) —
  daemon.md:292-294.
- Merge flake re-run (`merge/merge.go:944`) — daemon.md:686 (DECLINED).
- The enqueuing-phase boot sweep (`merge/coordinator.go:472`) —
  daemon.md:268-269.
- The daemon→Emacs host-action worker DIRECTION (`main.go:1268-1273`) —
  daemon.md:940.
- Shim seq cursors, pins, bounded replay (`shimclient/replay.go:115`,
  `client.go:1246`, `events.go:282`) — daemon.md:786-788, 816-820.
- The shim LISTENER (shims dialing the daemon)
  (`shimlisten/shimlisten.go:1-24`) — daemon.md:33-35.
- Engagement declaration at the intake funnel
  (`sessioncontroller/engagementturn.go:379+`) — daemon.md:656-661 (DROPPED).
- Metaprompt directive machinery (`sessioncontroller/metaprompt.go:1-16`) —
  already retired to the shim's system prompt.
- Durable `session_connectivity` (`ssm/db.go:136-146`,
  `ssm/ssm.go:411-420`) — superseded by INVARIANT 11, daemon.md:532-538.
- Host materialization round-trip: re-request cadence, escalation,
  abandonment (`create/manager.go:562,509`, `types.go:213-250`) —
  `endpoint_create_workspace.proto:1-7`.
- Durable in-daemon accounting rows as a persisted plane
  (`statedb/turnaccounting.go`, `statedb/tokenutilization.go`) —
  daemon.md:633-643 ("never persisted"), daemon.md:998.

**Ported or replaced by a prescribed component**
- SQLite open settings — WAL, `busy_timeout(5000)`, `_txlock=immediate`,
  `SetMaxOpenConns(1)`, eager `Ping`, `:memory:` refused
  (`statedb/statedb.go:69-80,85-110`; `main.go:456-470`) — daemon.md:106-112.
- Read-only inspection open (`statedb/statedb.go:39-51`) — daemon.md:126-129.
- Refuse-a-newer-schema (`ssm/db.go:281-283`, `registry/schema.go:100-104`)
  — daemon.md:126-129.
- Merge geometry recorded at creation, refused when absent
  (`geometry/geometry.go:22-24,125-133,163,231`) — daemon.md:79-83, 605-607.
- Displaced user turn captured durably, resubmitted exactly once
  (`ssm/db.go:180-189`, `ssm/mergelease.go:241,271`, `merge/contract.go:60-65`)
  — daemon.md:218-220.
- At-most-one-open merge lease (`ssm/db.go:200-205`) — daemon.md:69-77.
- Lock self-release on death, no stale pid state
  (`sessionlock/sessionlock.go:24-27`) — daemon.md:70-72, 442-445.
- Deleted-session resurrection refusal (`registry/registry.go:643`) —
  daemon.md:88-90.
- Shutdown schedule and drain holds (`statedb/shutdownschedule.go:72-91`,
  `server/shutdownschedule.go:20-30`) — daemon.md:66-68, 74-77, 753.
- Parked-ledger semantics (`sessioncontroller/parkedledger.go:1-32`,
  `main.go:1158-1170`) — daemon.md:183-187 (verbatim adoption).
- Prompt classification, interject, uninterruptible cut (`classify.go`,
  `verdict.go`, `uninterruptibleturn.go`, `queue.go`) — daemon.md:170-183,
  896-897.
- Session-command recognition and the panel class (`sessioncommand.go`,
  `promptdispatch.go`, `protocmd/protocmd.go:1-25`) — daemon.md:149-152,
  732-735.
- `/model` and `SetModel` converging on one path
  (`shimclient/events.go:82`, `control.go:206,249`) — daemon.md:160-164.
- Held tray force/accept/cancel (`frontendcmd.go:1820-1846`) —
  daemon.md:183-187, 890-898.
- Hibernation policy, engagement clock, single transition
  (`hibernation.go`, `engagementturn.go`, `main.go:365`) — daemon.md:314,
  832-833, 88.
- Rate-limited hibernation/drain refusal logging
  (`hibernationrefusallog.go:1-25`) — daemon.md:317-319.
- Revive modes and the cold gate (`revive.go:1-46`, `server/gate.go`) —
  daemon.md:826-831.
- Context size from the vendor's own answer (`contextsize.go:1-20`) —
  daemon.md:469-471.
- Connectivity state machine (`connectivitystate.go:1-30`,
  `ssm/connectivity.go`) — daemon.md:532-538 (INVARIANT 11), 502-505.
- Phantom-task reconciliation (`phantomtask.go:1-30`) — daemon.md:860-862,
  497-501.
- Phantom / undriven / unsubstantiated turn machinery (`phantomturn.go`,
  `undriventurn.go`, `unsubstantiatedturn.go`, `durableturnevidence.go`) —
  relocated to the shim's `GetLiveWork`, daemon.md:879-882.
- Live-task identity set vs. count (`ssm/livetasks.go`, `ssm/reconcile.go`)
  — daemon.md:132-138, 880-882.
- Surviving-shim spawn gate (`survivingshim.go:1-25`) — daemon.md:113-116,
  446-450, 56-58.
- Crash-surviving shim adoption (`shimlisten/shimlisten.go:361,466`,
  `main.go:1428-1470`) — daemon.md:55-58 and entry 10's adopt rendezvous.
- Build refresh, turn-boundary and async deferral (`buildrefresh.go`,
  `turnboundaryrefresh.go`, `asyncrefresh.go`) — daemon.md:390-424.
- Shim bundle `.built-sha` staleness bounce (`main.go:1614-1633`) —
  daemon.md:390-391.
- Shim spawn/pgid/kill-attribution/reap/stderr ring
  (`shim/proc.go:63,165,459,551,582`) — daemon.md:36-40.
- Spawn-death-vs-connect correlation (`shimclient/spawndeath.go:15,22,42`) —
  daemon.md:38-40.
- Redial with backoff, readiness as the health answer
  (`shimclient/client.go:853`, `shimclient/health.go`) — daemon.md:46-55.
- Heartbeat monitor and the degraded window
  (`shimclient/client.go:1083-1171`) — daemon.md:873, INVARIANT 11.
- Stop-cause attribution and the sealed stop funnel (`stopcause.go`,
  `turnstop.go`) — daemon.md:36-40, 406-409.
- Narrow single-target stops (`shimclient/control.go:130`,
  `detachedcancel.go`) — daemon.md:820-822.
- Detached publish/settle/store (`detachedpublish.go`,
  `detachedcontrolsettle.go`, `detachedworkstore.go`, `frontend/detachedwork.go`,
  `frontend/detachedsplit.go`) — daemon.md:523-528, 874-878, 10b.
- Conversation paging: history/store/live page and admission
  (`historypage.go`, `storepage.go`, `livepage.go`, `historyadmission.go`,
  `storehistory/messagepage.go`) — daemon.md:91-96, 993-995.
- Repull / bounded backfill (`repull.go:1-20`) — daemon.md:816-819.
- Permission rendezvous, no auto-answer, re-armable (`perms.go`,
  `permstate.go`) — daemon.md:509-510, 748.
- Turn record / latch / lifecycle ledger (`turnrecord.go`, `turnlatch.go`,
  `turnlifecycle.go`) — daemon.md:510-512, 886-888.
- Turn resumption after a bounce (`turnresumption.go`) — designed away by
  daemon.md:316-319, 397-400, 419-424.
- Keep-alive submit/deadline/cold-ping/residue-rewind (`keepalivesubmit.go`,
  `keepalivedeadline.go`, `keepalivecold.go`, `keepaliveresidue.go`,
  `keepalive/keepalive.go`) — relocated to the shim, daemon.md:868-870.
  (Note a plan-INTERNAL tension, not a finding: daemon.md:903-904 still
  lists "keep-alive turn" among the four genuine daemon holds.)
- Accounting hold and terminal settlement (`accountinghold.go`,
  `terminalsettlement.go`, `accounting.go`) — daemon.md:633-643.
- Token utilization/usage COMPUTATION and validation
  (`tokenutilization/*`, `tokenusage/*`, `frontend/tokenbreakdown.go`) —
  daemon.md:633-643 (in-memory accumulation).
- Topbar resolution, connectivity glyph, model catalog (`frontend/topbar.go`,
  `frontend/modelcatalog.go`) — daemon.md:462-476, 653-655.
- Progress/footer ticking (`progress/progress.go`) — daemon.md:871-873,
  932-936 (clients tick locally).
- Roster fan-out (`frontend/roster.go`) — daemon.md:748-749
  (`WatchWorkspaceRoster`).
- Transport machinery: lanes, outbox coalescing, pacing, snapshot batching,
  close causes, command tickets (`frontend/lanes.go`, `outbox.go`,
  `pacing.go`, `snapshotbatch.go`, `closecause.go`, `ticket.go`) —
  daemon.md:557-562 (DISCRETIONARY) + INVARIANT 13.
- Scope / workspace-key routing (`frontend/scope.go`, `scopequery.go`,
  `workspacekey.go`) — daemon.md:782-783, 759-761.
- Record categorization, lineage, ownership root, durability
  (`frontend/recordcategory.go`, `lineage.go`, `ownershiproot.go`,
  `durability.go`) — daemon.md:796-800 (`FeedId` encode/decode).
- Failure synthesis for store-degraded / query-died
  (`frontend/failuresynthesis.go`) — daemon.md:944-957, 513-514.
- Task-output byte-cursor tail (`frontend/taskoutput.go`) — daemon.md:985-988.
- `errclass` taxonomy (`errclass/errclass.go:1-29`, `kind.go`, `facts.go`) —
  daemon.md:626-632 + the reference-material carve-out at daemon.md:8.
- `.claude.json` account read (`account/account.go:14-22`) — daemon.md:473-475.
- Two-root account roster and transcript probing (`main.go:368`,
  `session/accountroute.go:28,77`, `server/accountresolve.go:11,102`) —
  daemon.md:99-105.
- Account switch porting the transcript between roots
  (`server/server.go:1140`) — daemon.md:481-484.
- Context-cut turn-id prefixes (`daemonturn/daemonturn.go:1-30`) —
  daemon.md:826-831, 866-870.
- Signal handling and the graceful shutdown sequence (`main.go:1330-1385`) —
  daemon.md:314-319, 117-121.
- Idle-timeout sweep (`main.go:365`) — daemon.md:314, 832-833.
- Workspace open / never-blue / open-ack coalescing (`server/workspaceopen.go`,
  `openbringup.go`, `createestablish.go:1-20`) — daemon.md:83-86, 48-55, 750.
- Creation job durable state machine, materialization state, resolved base
  (`create/manager.go:723`, `create/types.go:147-172`) — daemon.md:84-86.
- Merge queue per-repo, positions, enqueue/pause/evict (`merge/queue.go`,
  `merge/repokey.go:14-26`) — daemon.md:196-198, 617-619.
- Suite selection by blast radius and archived output
  (`merge/suiteselect.go:20,159`, `suiterunner.go:336`) — daemon.md:209-213.
- Escalation-record exit of the fix loop (`merge/merge.go:1245`) —
  daemon.md:212.
- Conflict handed to the agent once, then parked
  (`merge/coordinator.go:1707`) — daemon.md:214-220, 254-264.
- Sessionless merge, `ErrNoSession`, deleted-session refusal
  (`merge/pipeline.go:47-63`, `coordinator.go:1509`) — daemon.md:216-217,
  288-291.
- Session bring-up before the lease (`merge/pipeline.go:15-45`) —
  daemon.md:290 (revival-is-implicit).
- After-action cannot fail the run; its error rides the terminal
  (`merge/coordinator.go:1037-1044`) — daemon.md:206-208.
- Post-merge worktree removal (`merge/coordinator.go:1143`) —
  daemon.md:270-272, 613-615.
- Post-merge hook triggering the stack rebuild/restart
  (`merge/posthook.go:18-37`, `server/postmergehook.go`,
  `server/mergedispatch.go`) — daemon.md:234-241 + entry 10.
- Self-repo identity by git common-dir with siblings excluded
  (`reload/selfrepo.go:17-29`) — daemon.md:234-236, 617-619.
- Read-only git probes through one selection boundary (`reload/git.go:15-25`)
  — daemon.md:622-623.
- Store/sidecar excluded from the rollout (`reload/classify.go:21-27`) —
  daemon.md:326-328.
- Inbox MERGE route: project_dir key, unknown-workspace rejection,
  quarantine (`create/inbox.go:266-320`) — daemon.md:224-226, 266-269.
- Merge dequeue offer, Standing, AbortRunning (`merge/dequeue.go:63,107,147`,
  `frontendcmd.go:898,1021`) — entry 6; offer TIMING and cross-repo
  membership are in OPEN, daemon.md:669-671.
- Merge-skill classification (`frontend/mergeskill.go`) — named OPEN,
  daemon.md:670.
- Merge dequeue offer held in memory (`ssm/mergedequeue.go:21-27`) —
  daemon.md:552-555.
- In-flight manifest reconciliation at the shim-relaunch boundary
  (`server/inflightmanifest.go`) — daemon.md:117-121, 419-424 (the OPERATOR
  ledger is separately finding 13).
- Restart-pending announcement (`server/restartpending.go`) —
  daemon.md:356-364.
- Store-direct read for unwired workspaces as a TRANSPORT
  (`storehistory/storehistory.go:1-22`) — replaced by the `shim.v1`
  isolation gate, daemon.md:784-788 (the shim-LESS case is finding 4).
- Keep-alive env policy resolved once, fatal on a bad knob
  (`main.go:611-625`) — generic no-fallback discipline.
- Diagnostic dedupe, generation ids, withhold filter MECHANICS
  (`diagnosticdedupe.go`, `generation.go`) — trivial internals, deliberately
  unprescribed per daemon.md:11-16.
