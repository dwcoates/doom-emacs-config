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
| `fix/e2e-all-green` | `~/.config/doom-worktrees/e2e-all-green` | Drive e2e to green: 15 failing tests, the handover-ordering race, subagent rows mislabelled `kind=bash`, 2 uncovered fake scenarios. | `a8e05498e73a172bd` | 16:55 |
| `feat/daemon-owned-deploys` | `~/.config/doom-worktrees/daemon-owned-deploys` | The daemon owns builds and deploys, with a shim bounce registry, a forced RPC and one deploy per complete change. `deploy-all.sh` is removed. | `a148f02f6f00b1311` | 16:05 |
| `fix/webapp-bundle-chunks` | `~/.config/doom-worktrees/webapp-bundle-chunks` | The `vite build` chunk-size warning, fixed by splitting or trimming the bundle (not by raising the limit). | `a0d3f5e7dcd7eff3a` | 17:10 |

## Pending after the full bounce (owner asked ~17:20; nothing below dispatched yet)

The bounce ends this Claude session's turn and kills every background agent. So
after it:

1. Check the three `running` rows above (e2e-all-green, daemon-owned-deploys,
   webapp-bundle-chunks). Look at each worktree's partial work, redispatch fresh
   agents to finish, and record each one here.
2. Find the three subagents the footer shows stuck at 387.9k / 350.6k / 334.5k
   tokens (all named "subagent"), and check whether their work landed.
3. The async-work bubble header shows the detached-work id for ALL async work,
   so every item can be referred to by a stable id.
4. Structural invariant for clicking a footer detached-work row: if the feed
   entry is known it is selected, and if not, "not on screen" is shown and
   logged for remediation. Never neither (one subagent's click did nothing).
5. Two footer subagents show 0 tokens and "not on screen": find out what they
   are, and make the "not on screen" path log enough to investigate.
6. The expanded footer shows at most 4 rows, and scrolls beyond that.
7. Reassess every unaddressed request since the last two compactions (except
   wiping the logs and circling back), and dispatch each at once.

## Landed on master (this session, since the 09-21 compaction)

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

- 13:23:48: two agents (foreground Bash, turn-end truth) were stopped by a classifier interrupt. Both were redispatched or finished, and both landed.
- 14:37:37 and 14:49:23: four agents were stopped by interrupts. All were redispatched and landed.
- 14:56:00: five doom subagents were marked ended by the `iterm-2` shim's unscoped reconcile, though the agents kept running. The cause is fixed by `GetLiveWork` scoping.
