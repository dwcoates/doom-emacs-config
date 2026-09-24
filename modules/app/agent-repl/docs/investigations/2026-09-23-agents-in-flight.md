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
| `feat/daemon-owned-deploys` | `~/.config/doom-worktrees/daemon-owned-deploys` | REVIVED (13 commits plus uncommitted work before the bounce): daemon-owned builds and deploys, bounce registry, Deploy{force}, one deploy per merge, `deploy-all.sh` removed. | `a47129c3ca68414a3` (the e2e port in `feat/dod-e2e`, then it merges into this branch) | 21:xx (REVIVED after the session restart) |
| `fix/shim-writer-never-drops` | `~/.config/doom-worktrees/shim-writer-never-drops` | The shim's store writer never drops writes (it was "DROPPING store writes" at 256 batches); it uses backpressure, bounded batches, and latency-critical frames not stuck behind a backlog. | `a81508fcefcb60065` | 22:10 |
| `fix/store-checkpoint-and-cache` | `~/.config/doom-worktrees/store-checkpoint-and-cache` | Checkpoints become bulk-tier jobs (autocheckpoint off, `journal_size_limit`), plus a larger page cache (and maybe mmap), measured before and after. | `a959c0a4401ddeef2` | 22:10 |
| `fix/shell-output-tail-only` | `~/.config/doom-worktrees/shell-output-tail-only` | Shell output is stored as a rolling tail at the renderer's 16 KiB cap (one shared constant) instead of contiguous deltas from 0; live and replay show what they show today. | `a11d136b2405a419b` | 22:10 |
| `fix/keepalive-rows-unstored` | `~/.config/doom-worktrees/keepalive-rows-unstored` | Stop storing keep-alive rows (shim and sidecar) if running and rewinding them don't need it; the purpose table decides. | `a7a271a7c6c5b3e92` | 22:10 |
| `fix/fake-git-killed-flake` | `~/.config/doom-worktrees/fake-git-killed-flake` | Root-cause the flake `TestSubmitPromptDuringAMergeLeaseAnswersMergingRefusal` (fake git SIGKILLed under load; suspect cross-run stray reaping). Make ownership-scoped reaping structural. | `af0db7ff97ed4c775` | 22:35 |

## Queued for dispatch once the load drops (found by the deploy agent)

- `rollout/handover.go` `beginHandover`: if `served()` fails after the spawn, the successor is leaked and the next deploy spawns a second one, which races for the manifest. If `writeManifest` fails, the latch never releases. `SuccessorSpawner` needs a stop handle.
- A shim that dies with work recorded in flight produces no freeness edge, so its registered bounce waits until the workspace is revived.

## Still waiting on the owner

- Old store rows: settles written before the restate change now log an ERROR and draw nothing on replay, instead of an empty card. That's ERROR noise until the store ages out. Accept it, or treat pre-change rows at a lower level?
- Restating for a Subagent failure (prompt and created agent id) and an Artifact failure (the publish act) needs a contract shape. Should every settle also carry `started_at`, so replayed cards show their runtime?
- The proto keeps a `DaemonFault.deploy_script_failed` arm with no raise site now that the script is gone. Remove it?
- The scrollbar gutter: WebKit reserves the SYSTEM scrollbar's width (0 with overlay scrollbars, 14px with "always"), not our 8px. Should we fix it, and how?
- Background priority vs. the tight test timeouts: under `taskpolicy -b` on a loaded machine, suites time out (900ms tests, 1800ms cold boot). Which gives way?
- Proto: footer rows retired field tag 1 (`FeedId target`), a removal rather than an addition. Confirm.
- Monitor rows are now clickable and always show "not on screen" (a small visible change). Confirm.
- The selected-row mark is a 3px bar at the feed's far left, away from a centered card. Should it be restyled?
- The keep-alive hold is still visible to the daemon through the proto-ruled `turn_already_open.keepalive` refusal. Remove it (a proto change)?
- The sidecar still classifies keep-alive rows by a marker, so the two planes disagree (41 kind-change refusals today). Fix it on the sidecar side?
- The new name for "Release" (suggested: "Send now").
- A retry of `SPC TAB f`, which now logs.

## Landed on master (this session, since the 09-21 compaction)

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
