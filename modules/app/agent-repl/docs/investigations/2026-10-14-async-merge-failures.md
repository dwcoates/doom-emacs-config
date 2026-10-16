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
| 7 | 2026-10-15 | `test-tier-audit-integration-step3` (tip `6ff96ad124`, shim-lock stand-in plus removal lists) | `tier-step3-shim-lock` | LANDED (`ec6fbaa1ce`), deployed |
| 8 | 2026-10-15 | `tier-audit-e2e-step-c` (tip `32d6f55b9e`, instruction: rebase onto master keeping only the two step-(c) commits) | `tier-audit-e2e-step-c-merge` | FAILED: the e2e-emacs gate hit host file-table exhaustion from Docker's VM, see entry 8 |
| 9 | 2026-10-15 | `sandbox-no-host-mounts` (tip `a54186877d`, the structural fix for entry 8, at the FRONT) | `sandbox-no-host-mounts-merge` | LANDED (`55fd63fe5d`), deployed |
| 10 | 2026-10-15 | `test-suite-skip-hdv-land` (35 commits, suite reuse) | `test-suite-reuse-merge` | LANDED (`a980538e8b`), deployed |
| 11 | 2026-10-15 | `test-timer-waits-land` (tip `087c0e4da4`) | `timer-waits-landing` | LANDED (`9076bd2ecf`), deployed |
| 12 | 2026-10-15 | `redundancy-tool` (tip `764923476a`, the redundancy tool and pinned baseline, minor) | `redundancy-tool-land` | LANDED (`c31781c36b`), deployed; landed the branch as it stood when the merge forked, missing six later fixes, see entry 12 |
| 13 | 2026-10-15 | `sessions-test-slim` (one commit: 117 redundant cases removed from `sessions_test.go`, -0.04 coverage points) | `sessions-test-slim-land` | LANDED (`af0895c8c1`), deployed |
| 14 | 2026-10-15 | `tier-audit-e2e-step-c2` (tip `4b99b4a9cc`, step (c) rebased onto the fd fix; the retry of entry 8) | `tier-audit-e2e-step-c2-merge` | LANDED (`a3691b9b8b`), deployed |
| 15 | 2026-10-15 | `redundancy-tool-fixes` (the six fixes entry 12 missed, cut from master) | `redundancy-tool-fixes-land` | LANDED (`116084f27a`) |
| 16 | 2026-10-15 | `redundancy-preflight` (gomut panic skip, mutant pre-flight, extraction) | `redundancy-preflight-land` | requested, pending; `redundancy-dropsite` follows it, since the queue takes one merge per workspace |

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

## 8. `tier-audit-e2e-step-c` (FAILED: host file table exhausted by Docker's VM)

- The e2e-emacs gate's sandbox staging died: rsync "Too many open files in system (23)", 188 "file has vanished" lines, `entrypoint.sh` exited 23, and no test ran. The branch did not cause it.
- Measured 2026-10-15 01:08: `kern.num_files` 59,871 of `kern.maxfiles` 65,536. Docker's VM process (`com.apple.Virtualization.VirtualMachine`, pid 24424, up about 53 min) holds 51,851 of them.
  - About 45,500 of those are files inside agent-repl checkouts, about 4,500 per checkout across 10 checkouts (measured by the merge workspace `tier-audit-e2e-step-c-merge`).
- Cause: every e2e-emacs run bind-mounts its live checkout and rsyncs it inside the container, and Docker's file sharing never releases the host files it served. Each run permanently adds about 4,500 descriptors, so the table fills after about a dozen runs. Every e2e-emacs gate on the host fails from then on.
- Immediate relief needs the owner: restarting Docker Desktop releases the descriptors. Agents may not cycle Docker. Docker's VM had in fact restarted by about 01:40 (new pid), and the table fell back to about 7,800.
- Structural fix (entry 9, by `test-tier-audit-e2e`): no host path is mounted at all. `e2e-sandbox.sh` lists the allowlist host-side and streams it as one tar on `docker run -i` stdin; the entrypoint refuses a mounted checkout; artifacts leave through a VM-side volume and `docker cp`. Verified: the VM held 49 descriptors before and after each of three sandbox runs, staging 4,531 files. Pinned by `TestSandboxRunStreamsTheWorkingCopyAndMountsNoHostPath` and an in-container check rejecting host-shared file systems. Earlier plan: stream the staged allowlist into the container through the Docker API (a tar over `docker run -i`, or `docker cp`), so the VM never opens host files and the leak is impossible. A test pins that no checkout path is bind-mounted.
- The merge workspace was left open at `~/.config/doom-worktrees/tier-audit-e2e-step-c-merge`. Step (c) is re-requested unchanged after the fix lands.

## Host incident, 2026-10-15 ~01:30: the shared Go build cache is corrupted, so the queue is PAUSED

- Every Go build through `~/Library/Caches/go-build` (8.0G) fails with "package bytes is not in std" and "package crypto/internal/fips140only is not in std", even though GOROOT (`/opt/homebrew/Cellar/go@1.24/1.24.6/libexec`, go1.24.6) is intact.
  - Verified: `go vet ./internal/merge/` in the daemon fails with the shared cache and passes with a fresh `GOCACHE`.
- Likely cause: the owner's partial manual deletion of cache files earlier in the night left the cache's index pointing at missing entries.
- Fix (owner's call, because it touches files outside the project): `go clean -cache`, or removing `~/Library/Caches/go-build`.
- Every merge gate's Go suites would fail to build, burning attempts and handing merges back. So I paused the doom repository's merge queue at about 01:30 (`/merge-queue pause ~/.config/doom`, outcome `paused`).
  - Resume with `/merge-queue resume ~/.config/doom` after the cache is cleaned.
- Children verify locally with a private `GOCACHE` in the meantime.
- Resolved 2026-10-15 ~10:00: the owner ran `go clean -cache`, and a `go vet` through the shared cache passed again. My resume request was refused `not_paused`, because the queue had already been resumed by then (by the owner or by `agent-repl-perf-regression`, which had asked about the pause).

## 12. `redundancy-tool` (landed as `c31781c36b`, without six later fixes)

- The merge landed the branch's tip as of the moment the merge workspace forked. The child kept committing to the same branch afterwards (the branch moved from `620766ed4d` to `764923476a` while the merge waited), and those commits did not land.
- Six fixes were missing, found with `git cherry`: real-path `FindRoot`, integration-tier pins, the ruled pin clauses, the daemon scratch-repository race fix, naming every case that fails with no mutant, and the per-package overlay fix.
- Not a queue defect as such. A waiting async merge pins its branch at fork time, which is reasonable. But nothing told the requester that its later commits were left out. Possible source fix: when a merge lands and the branch has moved past what landed, report the commits left behind.
- The child cut `redundancy-tool-fixes` from master with the six fixes and requested it as entry 15.

## `ert-and-dist-fixes` (FAILED 2026-10-15 12:35: merge workspace had no live session)

- Branch `fix-ert-and-dist-land` (from `test-tier-audit-e2e`). The queue reported: workspace `6fc59458a3ed456d` "has no live session to await turn `76ef7e9a4d394225` on". The gate never ran.
- Daemon at 12:35:25: `daemon.merge.fault` "a merge could not continue past an error; it fails", then `ReadHistory refused: unknown_agent: no session has been started on this shim and the workspace holds no persisted main agent`.
- So the merge workspace's shim came up with no session started, and the queue tried to drive a turn on it. Not traced further. Re-requested under a new name.
- Recurred 2026-10-15 ~15:50 on `matrix-halves-2` after its 4th gate attempt: "workspace `776c48e13c694622` has no live session to await turn `ed95c4f47c3f452f` on". The same fault as above, now twice. Re-requested as `halves-and-removals`.

## Landings reported by the children since 2026-10-16 00:30

- `redundancy-stale-rows-land` (merge workspace `redundancy-stale-rows`), from `test-redundancy-measure`: LANDED as `bcb2acd6ad`.
- `redundancy-discovery-bound-land` (merge workspace `redundancy-discovery-bound`), from `test-redundancy-measure`: LANDED as `39b88e175b`.
- `slim-integration-deletions` (merge workspace `slim-integration-land`), made by this workspace as the owner's proxy: LANDED as `dfbf5e9d53`. It deletes 260 integration tests (162 daemon, 20 store, 78 sidecar) and 52 dead helpers. Coverage was measured before landing, with no system dropping more than 0.05 points.
- `e2e-step-d-land`, from `test-tier-audit-e2e`: LANDED as `0c312d41fa`. It deletes 51 host e2e tests and 22 e2e-emacs tests, with no coverage loss.
- `test-shared-setup`'s shared-World work (22 commits): LANDED as `615c85c01a`. No system lost coverage beyond 0.07 points.
- `redundancy-procs-land` (merge workspace `redundancy-procs`), from `test-redundancy-measure`: LANDED as `6c98fa4ca2`.
- `test-shared-setup-d1-land`, from `test-shared-setup`: LANDED as `a159524c28`. Two identical merge tests became one act with an observer subtest each.
