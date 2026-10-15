# Async merges attempted from agent-repl-test-slimming, and why they failed

A running ledger kept by the `agent-repl-test-slimming` workspace, which
coordinates the test-slimming child workspaces. One entry per async merge it
requested or landed for a child. Failed entries carry a brief diagnosis, with
the log records that show it, so the failures can be fixed at their source.

## Summary

| # | Requested (local time) | Branch | Merge workspace | Outcome |
|---|---|---|---|---|
| 1 | 2026-10-14 23:29 | `test-tier-rule` | `test-tier-rule-land` | waiting in line (not failed); an error record on every rebase, see entry 1 |
| 2 | 2026-10-14 23:41 | `test-fixture-table-rules` | `test-fixture-table-rules-land` | FAILED: the merge workspace could not be forked |
| 3 | 2026-10-15 | `cross-product-additions` (from `test-cross-product-audit-psv`, tip `44eee96334`) | `xprod-additions-merge` | requested, pending; a first request was refused `name_taken`, see entry 3 |
| 4 | 2026-10-15 (approved) | step 1 of `test-tier-audit-integration-nzx` (41 additive commits) | requested by the child itself | pending |

## 1. `test-tier-rule` (waiting in line)

- Carries `d5637e2f78` "a test lives at the cheapest tier that can prove it".
- Handed to its merge workspace at 23:29:59 (`daemon.merge.detached`), then put in line.
- Not a failure so far: it rebased onto each new master tip at 23:36, 23:41 and 23:51 and kept its place.
- Anomaly worth fixing at the source: every one of those rebases also logged an ERROR,
  `daemon.merge.publish` "a merge in line or in flight was published with no queue instant"
  (state `merging`), at 23:36:45, 23:36:46, 23:41:47, 23:51:56 and 23:51:57.
  - A merge in line should always carry its queue instant. This looks like the
    merge's published state losing that instant across a rebase-while-waiting.
  - Not yet traced further.

## 2. `test-fixture-table-rules` (FAILED: fork refused)

- Carries `316ae0bf59` (shared-fixture rule) and `bbf1d4e2cd` (table cases rule), on top of `d5637e2f78`.
- Outcome reported to the requester: "the merge workspace could not be created: ... the fork could not be made: the child's shim refused it: the fork point's record `9c37bcb0-f395-527a-9e6d-8a3269bc2573` is not in the parent transcript."
- Records:
  - shim, 23:41:15.091, `shim.engine.fork_session` ERROR "the fork was refused: the fork point is not in the parent transcript", `point: throughRecord`, parent transcript `~/.claude/projects/-Users-dodgecoates--config-doom-worktrees-agent-repl-test-slimming/4393df14-642f-4751-b00a-072a473941b5.jsonl`.
  - daemon, 23:41:15.105, `daemon.merge.detached` ERROR "could not fork the merge workspace for a detached merge; the request is dropped and the requester told".
- Diagnosis (confirmed from the transcript):
  - `9c37bcb0-…` is the record id of a prompt the owner sent MID-TURN.
  - The vendor never wrote that prompt as its own record. It folded it into an `attachment` record of type `queued_command` (transcript line 1270, that record's own `uuid` is `885d9276-f1f3-46b7-a206-5719a09fc9f4`), which names the prompt only as `attachment.source_uuid = 9c37bcb0-…`.
  - The daemon chose the latest prompt's id as the fork point, and the shim looks a fork point up by record `uuid` only, so it found nothing and refused.
- Where to fix it (to be decided):
  - the shim's fork-point lookup also resolves a `queued_command` attachment's `source_uuid` to that attachment record; or
  - the daemon names the fork point by a record that is actually in the transcript.
  - Either way, a prompt delivered mid-turn must never make a later fork impossible. Any workspace whose user typed during a turn hits this until it is fixed.
- The work is NOT landed. Nothing was retried or landed another way.

## 3. `cross-product-additions` (requested, pending)

- Requested by the `test-cross-product-audit` workspace for its additive work: the kind invariant through `protoarm.Must`, the 9-state footer/roster test, and the webapp vocabulary gate rows.
- The first request was refused `name_taken`, because the merge workspace was named after the branch. The re-request under the name `xprod-additions-merge` was accepted.
  - Not a defect: the refusal is documented, and the skill says to retry under another name.
  - Possible source fix: say in the refusal that the name collides with the BRANCH, since that is easy to trip over.
