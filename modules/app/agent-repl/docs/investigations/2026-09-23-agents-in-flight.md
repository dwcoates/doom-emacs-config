# Agents in flight — session of 2026-09-23

Kept by the lead by hand. The runtime has been losing background agents, so this
file is the record of what SHOULD be running. It is updated at every dispatch,
landing and loss.

Recovery rule: for any row still `running` whose agent is gone (not in
`ListAgents`, or stopped in the logs as "a person stopped the detached run"),
check the worktree for partial work and dispatch a fresh agent to finish it.

## Running

| Branch | Worktree | Task | Agent id | Dispatched |
|---|---|---|---|---|

## Still waiting on the owner

- Retiring the `keepalive` kind in the PROTO (`reserved`) is a breaking change; the store refuses it for now.
- `HostFault.bounce_died` is no longer opened by anything (a DIED shim with a free lock resolves at INFO). Want an open record kept?
- On replay, a terminal is charged to a turn by position (a stored terminal carries no turn id). Add a turn identity to the terminal frame or `HistoryEntryAt`? (proto)
- An adopted session's open turn rows aren't checked against what its shim reports. Which component should own closing such a row?
- Lock-holder failures other than a failed spawn (exits with a non-3 code, a signal, a wrong ready line) still read as `conversation_owned`, although the shim's AGENTS.md says they never should. Split them too?
- MCP tools (`mcp__*`) are unmodeled: WARN `daemon.sessionwatcher.unmodeled_activity` for each (102 in one session) and no card. Model them generically?
- `AGENT_REPL_STORE_PPROF_ADDR` is set via `launchctl setenv`, so the store WARNs `store.pprof.enabled` at every start. Keep profiling on?

## Landed on master (this session, since the 09-21 compaction)

- The revive WARN came from the fake vendor, which replayed an `is_backgrounded` patch for work already in the background; captures show the real vendor never does. A silent lock holder is killed and refused `lock_holder_unavailable` after 5 s: the agent's 250 ms off the bench was widened by the lead, because a false expiry refuses a real session start on a loaded host (`fix/revive-warn-and-lock-bound`; shim 6103 unit + 347 integration).
- The killed-shim "hang" was a test artifact: the detached announcement could land on the watch's opening page, while the test waited only on the tail. The new `awaitAgentEntry` searches the page, then the tail, with a regression test (`fix/killed-shim-revive-hang`; shim integration 346 on master).
- Post-bootstrap log defects (`fix/post-bootstrap-log-defects`): a retired keep-alive row is superseded by its real record, with no nuke (the 3 parked subagent transcripts were re-read at 14:26, 3 INFO supersedes, 0 producer-defects); the shim scopes keep-alives by descent; the stand-down manifest is consumed once and a DIED shim with a free lock resolves; the script runner leaves the verdict to its callers and launchctl 113 is not-loaded; a notice-only turn is not a defect; workspace directories are canonical on-disk spellings (the empty case-duplicate is forgotten); the orphan-log sweep spares closed workspaces. Deployed 14:25; all suites green on master except one shim integration hang, now dispatched.
- A query death's turn terminal is self-describing: `AgentFailure.query_died` (tag 19, carrying the pushed `SessionQueryDied`) replaces the `execution_error` stand-in, so the feed, footer and replay draw the death whichever statement lands first (`fix/query-death-terminal-race`; the e2e flake `TestWebappLayerQueryDeath` passed 3 of 3 full runs). CONTRACT ADDITION, lead-approved as a defect fix; the owner may revisit it.
- A lock holder that can't be spawned answers `lock_holder_unavailable {binary, os_error}` (typed `StartSessionFailure` and `OpenWorkspaceError` arms, drawn truthfully by Emacs and the webapp); the shared node store repairs a broken entry in place (`bin/lib-node-store.sh`: a per-entry mkdir lock, a fresh tree, an atomic symlink swap), and `ensure-deps` goes private only if the repair fails (`fix/lock-spawn-refusal-and-store-self-heal`). Realtest 7 (`SPC TAB f`): the fork works with real keys; the run failed only on log findings, which were handed to the log-defects branch.
- The system default scrollbar everywhere (custom `::-webkit-scrollbar` rules and the 8px token gone), `scrollbar-gutter: stable` on the bubble scroll box, and the tree budget measures the real gutter (`fix/system-scrollbar-stable-gutter`, webapp 5679 unit + 1773 integration). Supersedes the 09-14 always-visible bar. Deployed 13:53 (a clean handover, no WARN or ERROR).
- 09-24: the BOOTSTRAP onto daemon-owned deploys is done (lead, owner-authorized: stop, Emacs restart, deploy restarted store+sidecar, a second deploy all up-to-date); the ONE-TIME AGENTS.md section is deleted. The daily landed-worktree reaper landed (`feat/daemon-reaps-landed-worktrees`, daemon unit+integration green on master). Owner rulings: no terminal overtake; no successor-retraction arm.
- 09-24 lead: the webapp node-store entry `webapp-4153e127492d6543` was repopulated (owner approved); 86 landed worktrees pruned (clean, and `merge-tree` into master was a no-op); 37 dirty or unlanded ones kept. Stray reaping by argv path is kept (owner ruling).
- e2e load flakes fixed at their sources: the sidecar holds a rotated transcript until a record names its book; registry reads are ordered against Close; worktree log sinks detach inside `RemoveWorktree` and re-attach in `CreateWorktree` (master's `Surfaces.Retire` removed as the duplicate); one adoption per rendezvous; the restarting notice waits for a dropped stream to read again; `WatchBash` waits for an announced run's rows; store `unknown_run` at INFO (`fix/e2e-load-flakes`, e2e 3x green on the branch). Open: `forkSession` rotations aren't held; a `/clear` outside the shim in a shared directory is held forever (INFO once).
- A handover owns one successor slot: every post-spawn failure stops (TERM, KILL, reap) its successor and disarms the rendezvous; an unstoppable successor keeps the slot claimed. A shim's departure is a freeness edge: a dead shim in an open workspace is relaunched at once, an ordered or closed-workspace departure unregisters the bounce (INFO) (`fix/handover-leak-and-dead-shim-freeness`, daemon unit and integration green on master). Open for the owner: no proto arm retracts a successor announcement after a manifest-write failure.
- The shim's in-flight tool registry holds only calls its stream can settle (background agents' calls are never held; every result releases; handoffs are never cut; an agent's end releases its stream; every turn and query end drains it); reaching 512 is an ERROR. All 4,490 live evictions were already-settled calls (`fix/shim-open-call-leak`; shim unit 6073, integration 341 on master).
- Settles restate the rest: subagent failures (prompt, created agent), artifact failures (the start's act), and `AgentActivitySettledAt.started_at` for runtime. `AgentActivity.contract` stamps bound producers; unstamped old rows that restate nothing log INFO `settle_predates_contract`, while stamped ones stay ERROR (`fix/restate-rest-and-old-row-level`, all suites green, e2e with Emacs tests skipped).
- DAEMON-OWNED DEPLOYS plus the keep-alive hold removed (`integrate/deploys-and-keepalive` → `b14afc9b4`; test-all green, e2e twice). `deploy-all.sh` is gone; `claude-repld deploy [-force]`; one deploy per landing. BOOTSTRAP (one-time, owner): AGENTS.md "ONE-TIME: moving the live runtime onto the daemon-owned deploy". `ensure-deps` never installs through the shared node-store.
- A Monitor call draws the ordinary tool-call card in its owner's feed; its footer row centers and rings it; the monitor's settles restate its call (`no_feed_entry` reserved) (`feat/monitor-feed-card`, all suites green).
- Shell output is stored as one rolling tail at the shared 16 KiB cap (`AgentBashTail`; `update` retired); live and replay draw the same body; old delta rows are skipped at INFO (`fix/shell-output-tail-only`).
- Store: checkpoints are a bulk-tier job (autocheckpoint off, `journal_size_limit` 16 MiB), and the page cache is 64/16 MiB plus a 256 MiB mmap (`fix/store-checkpoint-and-cache`).
- The shim's store writer never drops a row: bounded batches, backpressure on the vendor loop, persistent failures held and loud, batches ending at every prompt or terminal (`fix/shim-writer-never-drops`, 5734 unit and 339 integration tests pass).
- The fake-git SIGKILL flake: the test's own teardown `kill(-pgid)` isn't atomic, so the daemon saw its child die first. The harness now freezes the group, then kills. Reclaim tests use private spaces, and strays match whole paths (`fix/fake-git-killed-flake`; 10/10 concurrent integration runs green twice).
- A selected entry is ringed on its own card (one `.entry-selected` for footer jumps and reply selection), and "Release" reads "Send now" (`fix/selected-mark-and-send-now`, 5695 tests pass).
- Keep-alive rows are stored by neither plane (nothing that runs or rewinds a keep-alive reads them). The sidecar classifies keep-alive records by promptId and parent chain, primed across restarts by reading earlier bytes (a one-time pass of up to 166 MB per resumed transcript). Known gap: a keep-alive turn that spawns a subagent (`fix/keepalive-rows-unstored`).
- Held-prompt badges carry daemon-written short labels plus an expand-only detail (`HeldPrompt.badges`), drawn verbatim; an "editing" badge too (`feat/held-badge-short-labels`, 5672 webapp tests).
- Detached work is drawn only in its owner's feed, at its spawning call's row (the announcement names the owner). Unplaceable work draws nothing and raises an ERROR plus a topbar warning; the root fallbacks are gone. Also: the sidecar reads a shell launch from the vendor's sentence when `toolUseResult` is missing, and a caller-cancelled open-faults read is DEBUG (`fix/detached-work-in-owning-feed`, daemon suite green on master).
- The footer token cell is the in-flight turn's growth of the main context (the topbar's `SessionContextUsage`), refreshed after each main API response. Subagents are per agent in the panel, idle shows `--`, a mid-turn cut rebases, and the alarm stays on whole-turn uncached input (`fix/footer-turn-context-delta`, all suites green).
- Editing a held prompt: Edit claims it under the delivery lock (it and everything after it stay held); Emacs takes it into the input (existing text saved to history); a send replaces and reclassifies; the claim ends with the editor's host stream; cancel is `C-c C-c` (`feat/edit-held-prompt`, daemon/webapp/ERT green on master).
- Store: one writer takes interactive before bulk (bulk granted after 8 interactive grants in a row); bulk is split at 64 rows, 1 MiB or 100ms; ledger sweeps are paged (they had held the writer 137s and 848s); an unclassified write is refused; the shim writes interactive and the sidecar bulk (`fix/store-interactive-writes-first`). DEPLOY NOTE: the store, shim and sidecar must deploy together, because the new store refuses unclassified writes.
- Every test entry point runs at `nice -n 19` through `bin/background.sh`, and suites refuse to start without it. Webapp integration passes 1765/1765 twice under it, in about 22s (`fix/tests-background-priority`). CLAUDE.md's ERT line is updated. Still open: plain `go test` in shim-store, shim-sidecar and shim-lock isn't enforced (it needs a `TestMain` gate).
- Sidecar: it never reads a spool nothing renders (unclaimed, unmapped, or transcript symlinks); writes are bounded at 1 MiB and 128 frames; terminals keep the 16 KiB spool cap (`fix/sidecar-no-unrendered-spools`, sidecar/store/daemon/e2e green).
- Held prompts: half width, 2 lines plus status badges when collapsed, the rest expand-only; toned badges from one table (waiting red, interrupting green); a first draw parks and follows (`promptHeld`) (`feat/held-prompt-compact-badges`, 5611 tests pass).
- Settles restate what their start carried: SendMessage's address and summary, plus the Read/Write/Edit/Grep/Glob/Bash/Skill failure inputs, so a replayed card draws from its settle alone (`fix/sendmessage-restates-summary`). The daemon suite is green on master after the merge.
- e2e green (`-count=3`, 0 failures on the branch): ClientLog resolves against the registry during a handover, the idle sweep defers while a held prompt revives, a detached row's kind is final, reconcile walks the whole book, and a late start never un-settles a card (`fix/e2e-all-green`).
- A click on the feed background clears an active reply selection through the daemon, and the cleared push parks and follows (`fix/selection-click-and-stable-gutter`). `scrollbar-gutter: stable` was NOT landed (see the owner decisions).
- Footer and async bubbles: the detached-work id on async heads; a footer click either selects (centered) or shows and logs not-on-screen; the panel caps at 4 rows; a collapsed sub-feed is wiped and reopens on its last page; nested subagents credit their own rows (the zero-token rows fixed) (`fix/footer-rows-and-work-ids`).
- Integration first-test flakes: each file's cold boot is paid in one bounded `beforeAll`, with a guard (`fix/integration-cold-first-test`).
- Keep-alive turns are scoped by the SDK's uuid echo and nothing they produce is served (`fix/shim-hides-keepalive`).
- Follow mode also turns on whenever the latest feed entry is visible, so scrolling back down resumes it. It's held off while a reply selection is active (`fix/follow-when-latest-visible`, 5443 tests pass).
- A thinking bubble shows in full until superseded by the next agent response in its feed. The daemon's `FeedResponse.superseded` collapses it to 2 lines, a user expansion survives, and a collapse above the viewport never moves the reader (`fix/thinking-collapses-when-superseded`, 5405 tests pass).
- The border ladder spreads: thinking is nearer red (#e3a008), mid-turn prose nearer yellow (#b0b00c), and a hue-monotonic test covers it (lead, 5383 tests pass).
- Bubbles: the fade alone (no chevron), and `has-more` only when the body truly overflows. User prompts always purple, agent-to-agent prompts amber, held prompts unbordered. Trees wrap to the expanded width (`fix/bubble-borders-fade-wrap`, 5382 tests pass).
- The `vite build` chunk-size warning is fixed by giving the protobuf code, runtime and Connect client their own chunks (`fix/webapp-bundle-chunks`, 5313 tests pass).
- Usage corner: the right gap equals the top gap, a two-phase hover, one size and baseline, and the reserve holds the hovered footprint (`fix/usage-corner-gap-animation`, 5356 tests pass).
- The collapsed usage token sits at a static inset, and hover slides it by exactly the duration's width.
- Wrapping happens only at max width: no 105-column fallback, no horizontal scrolling, emoji exactly 2 columns.
- A store restart no longer ends a running turn (turn-end truth).
- A page replace or a sent prompt parks the feed at its tail.
- The compaction line can't outlive its act.
- Every card title folds to 2 lines.
- One block state per stream (no duplicate or split bubbles).
- An interrupt ends only the synchronous turn (`perTaskStopAffordance`).
- Foreground work is never detached work.
- `GetLiveWork` is scoped to the caller's session.
- A context cut reaches the footer wherever it is first served.
- A compaction's cut is released on its own summary record.
- No forced kill without the user's explicit ask.
- The user owns the scroll: six named feed-scroll causes in `scroll.ts`, no other writer, and in-place bubble repaints (`fix/user-owns-scroll`, 5129 tests pass).
- History replays only on a workspace open or transcript select. StartTurn's page is the prompt row alone, and a retired handle is never re-admitted (`fix/replay-first-page-only`).
- One `drawBubble` for every blue and purple bubble, with grey held prompts capped at 2 lines and the compaction summary bordered like its divider (`refactor/one-bubble`, 5292 tests pass).

## Lost and recovered

- A session restart (new Claude Code session) stopped 8 agents. Their transcripts belong to the old session, so they couldn't be resumed, and all 8 were relaunched fresh from their worktrees.

- ~17:25: the owner's full bounce killed the three running agents (deploys, e2e, bundle chunks). The footer showed them stalled at 387.9k / 350.6k / 334.5k tokens. None had landed, and all three were revived from their worktrees at 17:50.

- 13:23:48: two agents (foreground Bash, turn-end truth) were stopped by a classifier interrupt. Both were redispatched or finished, and both landed.
- 14:37:37 and 14:49:23: four agents were stopped by interrupts. All were redispatched and landed.
- 14:56:00: five doom subagents were marked ended by the `iterm-2` shim's unscoped reconcile, though the agents kept running. The cause is fixed by `GetLiveWork` scoping.
