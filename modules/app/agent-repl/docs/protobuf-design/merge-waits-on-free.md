# A merge waits for a free workspace, and work learned at its end is never live

Owner rulings, 2026-10-08, after the `doom` workspace's incident: the agent asked
for a merge at 18:10:11 while two background subagents ran. The merge was
admitted at 18:10:22, its bubble placed and the workspace drawn `merging`, and
it then waited for the workspace to fall free. Each subagent's completion woke
the agent, and those turns drew BELOW the bubble. A shell one of the subagents
launched was first announced by the shim at its conclusion (18:15:27), admitted
live, its watch refused (the store held no rows yet), and never re-opened, so
the workspace never fell free, the merge never started, and the owner's prompt
was held behind it. The owner does not want to be consulted on proto shape;
this records what changed and why.

## 1. `conversation.v1.AgentDetachedWork.concluded`

A new optional message field, `concluded` (tag 7, `DetachedWorkConcluded`, no
fields), set when the producer makes the announcement at the work's conclusion:
the shim's `task_notification` upsert. Unset on every announcement made while
the work runs.

- The shim sets it on the notification's upsert, which is the only
  announcement it makes at a conclusion (`convert/detached.ts`).
- The daemon (`sessionwatcher.concludedFirstLocked`): work it does not already
  hold live or pending is recorded concluded (retired) and never admitted, so
  it never counts toward freeness. A shell's record is still read to its end
  through a watch that is not live work, so the feed draws its outcome; the
  terminal reaps that watch.
- Work already live is unchanged: its own terminal concludes it. A running
  shell's stream is never ended early on the restatement, because a shell's
  end still has one writer, the sidecar's terminal.

Additive: an older shim never sets it, and the daemon reads its announcements
exactly as before.

## 2. `frontend.v1.FooterSubStatusMergingAwaitingFree` and `RosterRowStatusMergeWaiting`

The owner's invariant: "while merging, the merge bubble is necessarily the last
thing in the feed, because things only land outside of it after the merge has
finished". A feed row's order key is minted at its first draw, so the bubble is
now drawn only when the merge STARTS, once the workspace is free.

- `FooterStatusMerging.awaiting_free` (tag 22): the merge holds the workspace
  (its lease holds every prompt) and waits for it to fall free. Fields: whether
  a turn is in flight, the subagents, shells, monitors and pending items
  running, and the other workspace's name when the merge waits on the one whose
  worktree it rebases in. The daemon's footer reads them from its own state of
  the awaited workspace, so they follow every change.
- `RosterRow.merge_waiting` (tag 57): the roster's projection of it.
- `render-colors.json`: `merge_waiting` is PURPLE on the roster, a colored merge
  arm, glyph `await`. Purple because the daemon holds the workspace exactly as
  during `merging` and `merge_queued`; the glyph alone says it has not started.

## 3. `agentrepl.v1.RestartWorkspaceError.relaunch_failed`

A restart answers its OUTCOME: success once the bounce finished, or
`relaunch_failed` (tag 6, `detail`) when it did not. The old success meant "the
bounce has begun", and a relaunch that then failed told the user nothing
(2026-10-08 18:55:03, workspace `doom`: the restart-pending hold collided with a
merge's lease, the prelaunched shim was killed, and Emacs had already said
"restart under way").
