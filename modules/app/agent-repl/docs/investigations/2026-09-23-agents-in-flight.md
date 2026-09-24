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
| `fix/e2e-load-flakes` | `~/.config/doom-worktrees/e2e-load-flakes` | Root-cause the e2e flakes `TestWebappLayerRoster` (ClientLog/lease ERRORs during a drain) and `TestClearRotatesIdentity` (the separation row timed out; `final_answer_unresolved`). | `a3cc32b3e6611f35f` | 22:55 |
| `fix/restate-rest-and-old-row-level` | `~/.config/doom-worktrees/restate-rest-and-old-row-level` | Pre-contract unrestated rows log INFO (a new defect stays ERROR). Subagent and artifact failures restate; every settle carries `started_at`. | `aa95f06fc74ba5cf4` | 23:05 |
| `fix/shim-open-call-leak` | `~/.config/doom-worktrees/shim-open-call-leak` | The shim's in-flight tool registry leaks (full at 512; an interrupt cut 493 phantom calls). Settle and remove every call on its settling path; reaching the bound is an ERROR. | `a90cb0143868c171b` | 23:55 |
| `fix/handover-leak-and-dead-shim-freeness` | `~/.config/doom-worktrees/handover-leak-and-dead-shim` | A failed handover stops its successor (a single-successor slot); a dead shim's death drives the bounce registry. | `a0fae6cd54268b49c` | 00:40 |

## Still waiting on the owner

- The shared `~/.cache/agent-repl/node-store` webapp entry was emptied by a worktree's `npm ci`. Repopulating it is outside the project: the owner runs it, or approves.
- The shim reports "another process owns this conversation" when it can't even spawn its lock holder. A new refusal arm?
- Ordering contract: a turn's ending still waits behind every row produced before it (the store gets rows in exact production order, which subagent consumers rely on). Should a terminal ever overtake? That's an ordering-contract change.
- Harness stray reaping still selects by argv path, not a kernel mark (a session id would break handover successors and Emacs-launched e2e daemons). Keep it as is?
- `DaemonFault.deploy_script_failed` removal is folded into the deploy branch's finishing agent.
- `SPC TAB f` retry: needs a deploy, and the owner must OK the bounce.
- The scrollbar gutter: WebKit reserves the SYSTEM scrollbar's width (0 with overlay scrollbars, 14px with "always"), not our 8px. Should we fix it, and how?
- A retry of `SPC TAB f`, which now logs.

## Landed on master (this session, since the 09-21 compaction)

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
