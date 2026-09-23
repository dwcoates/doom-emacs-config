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
| `feat/daemon-owned-deploys` | `~/.config/doom-worktrees/daemon-owned-deploys` | REVIVED (13 commits plus uncommitted work before the bounce): daemon-owned builds and deploys, bounce registry, Deploy{force}, one deploy per merge, `deploy-all.sh` removed. | `a9939024139796b9d` | 17:50 |
| `fix/e2e-all-green` | `~/.config/doom-worktrees/e2e-all-green` | REVIVED (4 commits plus uncommitted work): e2e to green. | `a915f8a8d5e7424ee` | 17:50 |
| `fix/bubble-borders-fade-wrap` | `~/.config/doom-worktrees/bubble-borders-fade-wrap` | Fade only, never a chevron. has-more only when content is really hidden. User prompts always bordered purple, agent-to-agent prompts (and peer messages) amber, held prompts unbordered. Wrap as if expanded. Empty agent-prompt body. | `a9cf9a1637a84bcb4` | 17:50 |
| `fix/shim-hides-keepalive` | `~/.config/doom-worktrees/shim-hides-keepalive` | The shim never serves a keep-alive turn's prompt, reply, usage or terminal. | `a5f0d85cf57c43d07` | 17:50 |
| `fix/footer-rows-and-work-ids` | `~/.config/doom-worktrees/footer-rows-and-work-ids` | The detached-work id in every async bubble header. A footer row click either selects its entry or shows and logs "not on screen", never neither. The 0-token rows. The footer capped at 4 rows. ADDED: an async bubble shows only the last page, streams while open, and is WIPED on collapse. | `a0ac08889b72fae26` | 17:50 |
| `fix/thinking-collapses-when-superseded` | `~/.config/doom-worktrees/thinking-collapses-when-superseded` | A thinking bubble shows in full until the next agent response in its feed. The daemon's `superseded` flag then collapses it to 2 lines. A user expansion is kept, and the collapse never moves the reader. | `a46d90f097aab0cae` | 17:55 |

## Still waiting on the owner

- The new name for "Release" (suggested: "Send now").
- A retry of `SPC TAB f`, which now logs.

## Landed on master (this session, since the 09-21 compaction)

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
