# Resume file: bring realtest 9 home (written 2026-09-13 22:00, before a compaction)

Read this first after compaction. Then read `docs/REMEDIATION-CHANGELOG.md`
(regression watch), the tail of `docs/REALTEST-JUDGEMENT-CALLS.md` (every
ruling of 2026-09-13), `docs/REALTEST-PLAN.md` "Status", and
`docs/FOOTER-TOPOLOGY-AUDIT.md`.

## Standing orders in force

- Realtest 9 and the Conversation section: the lead authors, runs and
  remediates to completion WITHOUT the owner. Corrective UI changes only.
- Zero warnings/errors in any system's log; "pre-existing" is not a
  category; the gap scan between sweeps counts.
- Diagnoses are the lead's own work; edits go to opus agents on branches
  (`git worktree add -b <branch> <scratchpad>/wt-<name> master`), merged
  `--no-ff` by the lead; doc conflicts resolved by keeping both sides.
- Only the lead runs realtests: `AGENT_REPL_REALTEST_TAKEOVER=1
  AGENT_REPL_REALTEST_STOP_DAEMON=1 bin/realtest.sh` (whole sweep) or
  `-run TestRealtest...` (one). Run in the background; read the task file.
- After every merge that changes a deployed system: `bin/deploy-all.sh`.
  The sweep DECLINES if the deployed stack is not at the checkout.
- Docker: quit gracefully (`osascript -e 'quit app "Docker"'`), never
  `pkill -9`. Sandbox Emacs layer: `e2e/sandbox/bin/e2e-sandbox.sh build`
  then `... run --dir e2e go test ./ -run TestEmacs -count=1`, then quit.
- Screen: `caffeinate -d -i -u` is running (pid 9630), screensaver idle 0,
  screenLock off. Do not rely on it after a reboot; re-check with
  `swift e2e/realtest/keydriver.swift --session` (screenLocked=no).
- Webapp gates ALWAYS include `npm run test:integration`.
- Footer fault-kind proto additions are pre-approved (reuse statuses;
  substatus buckets onto activities). Nothing profound or cross-cutting.

## State at the compaction

- master tip 501312564. Realtests 1-8 twice-green on 2026-09-13 under the
  once-per-sweep focus policy; last full sweep (rt-run33) all green with
  the gap scan down to 2 records, both the shim start contract (below).
- The owner's editor: guard-free Emacs + guard-free daemon are restored at
  every sweep end (`the owner's editor was restored` line).
- Store: schema 7, single serialized writer, read pool, ledger retention,
  no residue persisted, shape catalog (`make -C agent-shim/shim-store shapes`).

## State after the sixth sweep with 9 (rt-run39, 2026-09-14 00:10)

- Landed and deployed: sidecar boot walk scoped, poll pass sliced, identity
  negative cache (master c46785d03). Sidecar steady-state CPU 2% (was 100%),
  new transcripts picked up ~1s after they appear.
- rt-run39: 1-8 green, gap scan clean; realtest 9 fails ONLY on the answer
  text: the answer bubble IS drawn (final-answer-marked styled_bubble=true)
  but the response renderer records nothing, and the harness waited for a
  `feed.draw-text-block` that only prompt blocks emit.

## Agents in flight

1. `fix/feed-response-record` (wt-resp): `feed.draw-response` INFO with
   characters/blocks; harness matches it and measures the phase.
2. `fix/sidecar-workspace-cache` (wt-wscache): the sidecar's dir→workspace
   ref cache is never invalidated; a re-registered dir carried the stale
   id (forward refused "no longer registered").

Then: merge both, deploy, rerun to twice-green, test-all, plan status.

## The loop for realtest 9

1. Merge the three branches above (in the order they land), deploy.
2. Run the whole sweep (1-9). Collect EVERY finding (harvest + gap scan)
   before remediating any; group by (runtime, level, operation).
3. Dispatch remediation in parallel on branches (one agent per system);
   the lead diagnoses first when a finding's cause is unclear.
4. Merge, deploy, rerun. Realtest 9 twice-green with a clean gap scan,
   1-8 still green.
5. `bin/test-all.sh` once as the section gate (Docker for the sandbox
   layer, then quit it). Update docs/REALTEST-PLAN.md status.
6. Then realtest 10 (interrupt), same loop.

## Known open items (not blocking 9)

- Footer audit rows N4 (drawn elsewhere, not footer/feed) — owner's call.
- `swept_up` lost-policy reason level (sidecar) — unruled.
- Stream-plane write-ledger rows are never pruned — unruled.
- Stale worktrees under `/private/tmp/claude-501/wt-*` and the scratchpad
  from earlier sessions; leave them.
- The gns-cowork plugin 9.10.0 ships PowerShell SessionStart hooks; they
  were stripped locally in both account roots' installed copies
  (backups `hooks.json.orig-with-powershell`); upstream still has them.
