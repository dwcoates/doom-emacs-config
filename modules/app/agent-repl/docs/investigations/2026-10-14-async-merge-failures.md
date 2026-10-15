# Async merges attempted from agent-repl-test-slimming, and why they failed

A running ledger kept by the `agent-repl-test-slimming` workspace, which
coordinates the test-slimming child workspaces. One entry per async merge it
requested or landed for a child. Failed entries carry a brief diagnosis, with
the log records that show it, so the failures can be fixed at their source.

## Summary

| # | Requested (local time) | Branch | Merge workspace | Outcome |
|---|---|---|---|---|
| 1 | 2026-10-14 23:29 | `test-tier-rule` | `test-tier-rule-land` | LANDED (`3e33b5963d`); an error record on every rebase while waiting, see entry 1 |
| 2 | 2026-10-14 23:41 | `test-fixture-table-rules` | `test-fixture-table-rules-land` | FAILED: the merge workspace could not be forked |
| 3 | 2026-10-15 | `cross-product-additions` (from `test-cross-product-audit-psv`, tip `44eee96334`) | `xprod-additions-merge` | LANDED (`3841cbba64`), deployed; a first request was refused `name_taken`, see entry 3 |
| 4 | 2026-10-15 | `test-tier-audit-integration-step1` (tip `3e73dfd165`, 41 additive commits) | `tier-step1-units` | LANDED (`98f4bee425`), deployed |
| 5 | 2026-10-15 | `test-fixture-table-rules-2` (entry 2's two rule commits, rebuilt on master `3e33b5963d`) | `fixture-rules-retry` | LANDED (`13f99f54a2`), deployed; the retry of entry 2 forked fine from a turn whose prompt is a normal transcript record, which supports entry 2's diagnosis |
| 6 | 2026-10-15 | `tier-audit-e2e-step-a` (tip `75e781c362`, e2e audit report plus lower-tier tests) | `tier-audit-e2e-step-a-merge` | LANDED (`f330c112a4`), deployed |
| 7 | 2026-10-15 | `test-tier-audit-integration-step3` (tip `6ff96ad124`, shim-lock stand-in plus removal lists) | `tier-step3-shim-lock` | requested, pending |

## 1. `test-tier-rule` (landed)

- Carries `d5637e2f78` "a test lives at the cheapest tier that can prove it".
- Handed to its merge workspace at 23:29:59 (`daemon.merge.detached`), then put in line.
- Landed as `3e33b5963d` (merge of `test-tier-rule-land`, rule commit `4a5aa3ee02`) after waiting in line.
- While waiting it rebased onto each new master tip at 23:36, 23:41 and 23:51 and kept its place.
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
- Second defect found 2026-10-15: the failed create left an orphan. The daemon logged `daemon.workspace.create` ERROR "the session did not come up" at 23:41:15 and dropped the merge, yet a session named `test-fixture-table-rules-land-8b` is running idle after the later restart, with no merge attached to it.
  - A create that fails should leave no workspace behind, or the restart should not bring up a workspace whose merge was dropped.

## 3. `cross-product-additions` (landed as `3841cbba64`)

- Requested by the `test-cross-product-audit` workspace for its additive work: the kind invariant through `protoarm.Must`, the 9-state footer/roster test, and the webapp vocabulary gate rows.
- The first request was refused `name_taken`, because the merge workspace was named after the branch. The re-request under the name `xprod-additions-merge` was accepted.
  - Not a defect: the refusal is documented, and the skill says to retry under another name.
  - Possible source fix: say in the refusal that the name collides with the BRANCH, since that is easy to trip over.
