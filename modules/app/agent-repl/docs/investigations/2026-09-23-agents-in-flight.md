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
| `feat/daemon-owned-deploys` | `~/.config/doom-worktrees/daemon-owned-deploys` | REVIVED (13 commits plus uncommitted work before the bounce): daemon-owned builds and deploys, bounce registry, Deploy{force}, one deploy per merge, `deploy-all.sh` removed. | (agent done; waiting on the child `ad454b522817243f4` porting e2e in `feat/dod-e2e`, which must merge into this branch before landing) | 17:50 |
| `fix/e2e-all-green` | `~/.config/doom-worktrees/e2e-all-green` | REVIVED (4 commits plus uncommitted work): e2e to green. | `a915f8a8d5e7424ee` | 17:50 |
| `fix/detached-work-in-owning-feed` | `~/.config/doom-worktrees/detached-work-in-owning-feed` | Detached work is drawn only in its owner's feed (main to root, a subagent's to its sub-feed), at the spawning call's position. No root fallback: an unplaceable item logs an ERROR and a topbar warning. Also why a subagent's shell output isn't in the store. | `a68a3514a8fb89b75` | 18:05 |
| `fix/footer-turn-context-delta` | `~/.config/doom-worktrees/footer-turn-context-delta` | The footer token cell is the in-flight turn's growth of the MAIN context window, from the topbar's source. No subagent usage (that's in the clickable panel). Idle shows `--` and stays clickable. | (agent completed twice with NO report; 6 commits, clean; the lead verifies the suites, then merges) | 18:20 |
| `fix/sendmessage-restates-summary` | `~/.config/doom-worktrees/sendmessage-restates-summary` | The SendMessage settle restates `summary` and `addressed_to` (proto, shim, sidecar, daemon), plus an audit of other settles that don't restate their start. | `a7ce629aa3694c20d` | 18:30 |
| `feat/held-prompt-compact-badges` | `~/.config/doom-worktrees/held-prompt-compact-badges` | Held prompts collapse to 2 lines, with details and buttons only when expanded, at half the normal max width. Statuses are colored badges (waiting red, interrupting green, the rest mapped). ADDED: a held prompt landing jumps the feed to the bottom and follows, like a sent prompt. | `afb5bccb8f702f5d3` | 18:55 |
| `fix/sidecar-no-unrendered-spools` | `~/.config/doom-worktrees/sidecar-no-unrendered-spools` | The sidecar stops storing unclaimed task spools and duplicate transcript symlinks as residue. Rendered spools are bounded. | `a8d17efda472949bd` | 19:05 |
| `fix/tests-background-priority` | `~/.config/doom-worktrees/tests-background-priority` | Every test entry point runs under `taskpolicy -b` through one helper, with a source-scan guard. The live runtime is untouched. | `a8e7822fe0f97a214` | 19:05 |
| `fix/store-interactive-writes-first` | `~/.config/doom-worktrees/store-interactive-writes-first` | A two-tier store writer (interactive shim writes before bulk sidecar writes), bounded bulk batches, per-class metrics, and a look at the 163s write. | `a708d3073a6905327` | 19:05 |
| `feat/edit-held-prompt` | `~/.config/doom-worktrees/edit-held-prompt` | An Edit button on the held card. The editing claim holds that prompt and everything after it. The content goes to the Emacs input (existing text saved to history), and a send replaces it and reclassifies. | `ac1c20bc246a27fba` | 19:05 |

## Queued for dispatch once the load drops (found by the deploy agent)

- `rollout/handover.go` `beginHandover`: if `served()` fails after the spawn, the successor is leaked and the next deploy spawns a second one, which races for the manifest. If `writeManifest` fails, the latch never releases. `SuccessorSpawner` needs a stop handle.
- The idle sweep hibernates a session that is being revived for a pending prompt. This is a production race behind the integration flake `TestAParkedWorkspacesFooterIsIdle…`.
- A shim that dies with work recorded in flight produces no freeness edge, so its registered bounce waits until the workspace is revived.

## Still waiting on the owner

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

- ~17:25: the owner's full bounce killed the three running agents (deploys, e2e, bundle chunks). The footer showed them stalled at 387.9k / 350.6k / 334.5k tokens. None had landed, and all three were revived from their worktrees at 17:50.

- 13:23:48: two agents (foreground Bash, turn-end truth) were stopped by a classifier interrupt. Both were redispatched or finished, and both landed.
- 14:37:37 and 14:49:23: four agents were stopped by interrupts. All were redispatched and landed.
- 14:56:00: five doom subagents were marked ended by the `iterm-2` shim's unscoped reconcile, though the agents kept running. The cause is fixed by `GetLiveWork` scoping.
