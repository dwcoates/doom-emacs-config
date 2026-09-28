# Plan: the merge queue becomes the one path to master

Status: PLANNED, not started. The owner asked (2026-09-28) to circle back to it once the
outstanding work lands. Written by the lead from its analysis of the first one-shot
merge (workspace `prompt-bubble-height`, lease `c8a3a664006f46c1`, 2026-09-28 14:08–14:20).

## What happened (evidence)

1. **The gate could never pass.**
   - `daemon.merge.tests` exited 127 in all three rounds.
     - The archive `~/.claude-emacs/merge-logs/c8a3a664006f46c1-tests-1.log` shows it ran
       `…/modules/app/agent-repl/modules/app/agent-repl/bin/test-all.sh`.
   - The running daemon's default SelfRepo was the module root, and `merge.TestCommandFor`
     appended `modules/app/agent-repl` a second time.
   - The source fix is `bcf9f91ec` (`checkout.RepoRoot` + `resolveSelfRepo`). It is on master
     but NOT deployed.
2. **The requesting turn killed itself: "the turn was interrupted".**
   - The agent asked for the merge from inside its own turn, through a command file.
   - The lease acquisition runs `workspace.Fleet.CaptureDisplaced` (`fleet_rollout.go` ~172).
     It kills the in-flight turn unforced and marks it for resubmission when the lease is
     released.
   - In a one-shot workspace, that in-flight turn IS the requester.
3. **Three repair rounds on a broken gate.**
   - The fix agent was asked to repair "failing tests" three times, and on round 3 escalated
     correctly.
   - Round 1 committed a real daemon source change (the gate fix) as
     "fix(merge): repair the suite…".
4. **The parked run swallows guidance, so the agent stops responding.**
   - `merge.run.fixes()` calls `park()`, which blocks on `r.guidance`, then DISCARDS the
     guidance it returns and reports `parked=true`.
   - `merge.run.run()` then returns ("a parked merge keeps the lease and the lock"), so NO
     goroutine is left to call `deliverGuidance` or answer `r.answered`.
   - Result:
     - The owner's first prompt (14:19:28) was consumed by `park()` and never answered.
     - Their second (14:20:18) had no receiver.
     - Both ended as `daemon.promptqueue.submit … context canceled` after Emacs' 10 s
       timeout, and were then held locally as `:outage`.
5. **The whole repo's queue is blocked.**
   - `orchestrator.running[repo]` still holds the dead run, and it keeps the repo lock.
   - `admitFront` answers busy for every later merge into `/Users/dodgecoates/.config/doom`.
6. **Dequeue doesn't release a running merge.**
   - `dropQueued` only removes a queue entry. It logged "a merge left the queue without
     running", which was false.
   - The lease (`wsm.db` `leases`: `c8a3…`, policy parked), the `merge_ledger` row and the repo
     lock all stay in place.
7. **The queue writes into the live master checkout.**
   - The merge commit `770fd9efa` and the fix commit `bcf9f91ec` landed directly on master.
   - The lead's manual merge (`5c38b5b24`) landed on top at 14:18, mid-run. Nothing else
     honors the queue's lock.
   - The untracked `.agent-repl-merge-escalation` is left in the master checkout.
8. **An undecodable merge feed row.**
   - At 14:13:05.603 the webapp reported `rpc.stream-frame-undecodable`:
     `WatchFeedResponse.row`, a non-optional message field unset. That filed a
     frameUndecodable warning chip.
9. **Structural gap.** A defect in the gate cannot be repaired from inside the merge the gate
   is running, because the gate runs in the already-running daemon binary.

Master is green with the queue's commits included: webapp 6000/6000, daemon unit and
integration.

## Fixes (in order; each an agent branch, merged on green)

1. **Park properly.**
   - A parked run stays alive until it is resolved.
   - Every guidance is delivered (`deliverGuidance`) and answered.
   - Resolution is resume-with-guidance, abandon, or dequeue, and every exit releases the
     lease, the ledger and the repo lock through ONE release path. Dequeue of a RUNNING merge
     is an abandon, never a silent entry removal.
   - Summaries must be true (no "without running").
2. **One-shot merges never kill the requester.**
   - When the lease's requester is the workspace's own in-flight turn, the lease waits for
     that turn to end instead of `CaptureDisplaced`.
   - Nothing is marked for resubmission, and no "interrupted" row appears.
3. **A broken gate is not a test failure.**
   - A gate exit that means the gate itself failed to run (127, or a missing script)
     parks at once with a plain line: "the test gate itself failed to run".
   - No repair rounds.
   - Consider a separate gate-health preflight.
4. **The queue owns master.**
   - The queue merges in a worktree of its own and fast-forwards master only after the gate
     passes.
   - master never carries a half-finished merge, and concurrent writers can't interleave.
5. **Fix the undecodable merge feed row** (`WatchFeedResponse.row`, a required field unset).
6. **Skill: agents enqueue their own branch merges.**
   - A new skill enqueues a branch into the merge queue (through the daemon's command-file
     ingress) and waits for the outcome: landed, parked or failed.
   - It handles parked and failed by reporting, never by merging by hand.
   - Subagent branches flow through the same sequence.
7. **Metaprompt rule** (always loaded): "merge into master only through the merge-queue
   skill", pointing at the skill.
8. **Git hook enforcing it.**
   - Refuse any merge or commit onto master not made by the queue, recognized by a marker the
     queue sets, for example an environment variable.
   - The refusal message names the skill.
   - There is an owner escape hatch.
   - This makes bypassing the queue impossible, not merely discouraged.

Order: 1–5 first (the queue must be trustworthy before everything depends on it), then
6+7, then 8.

## UX end state

- **Success:**
  - The one-shot agent's turn ends normally.
  - The merge bubble and footer walk queued → merging → tests → landed.
  - master only moves forward after the gate passes.
  - Subagent branches use the same path.
- **Test failure:** repair rounds as today. A broken gate parks at once with a plain reason.
- **Parked:**
  - The bubble shows the agent's reason.
  - Typing in the workspace reaches the merge agent and gets an answer.
  - Resume, abandon or dequeue each release everything.
- **Can't happen:**
  - a direct merge into master (the hook refuses it);
  - two writers on master at once;
  - the warning chip from the undecodable merge row.

## Owner decision (settled 2026-09-28)

- Does a parked merge block the rest of its repo's queue?
- RULED NO (owner agreed with the recommendation). Later merges proceed. A parked merge, when resumed, rebases onto the
  new master and re-runs the gate.
- The alternative, strict order, is simpler but lets one stuck merge stall everyone.

## Live state to clean up (needs the owner; no bounces without asking)

- Lease `c8a3a664006f46c1` (parked), its `merge_ledger` row, and the in-memory run keep the
  doom repo's queue blocked until the daemon restarts.
- The untracked `.agent-repl-merge-escalation` is in the master checkout. Ask before deleting
  it.
- The owner's two prompts to `prompt-bubble-height` are held locally in Emacs as `:outage`.
