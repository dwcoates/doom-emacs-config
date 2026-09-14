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

## 2026-09-14 daytime: owner-driven live fixes (all merged, deployed, gates green)

- Real-vendor defects found by the owner's own prompts: the hibernate loop
  (compaction every 5 min), the keep-alive rewind anchor taken from
  non-assistant uuids (vendor died on the second real prompt), cold-gate
  feedback and typed refusal. All landed; the keep-alive rewind now falls
  back to a plain resume and never loses the prompt.
- UI rulings landed: topbar title centered BETWEEN the flanks (equal clear
  space), chips tight (0.3rem), row spans its header, branch==name dropped;
  prompt bubble waves from draw to final answer; card timers stop on
  settle; auto permission mode (no `default`); bubbles +10% wide/long with
  edge scrollbars visible iff overflow; feed begins at the newest
  separation; sidebar hover panel (no chevron); footer letter wave on
  progress statuses; no pseudo workspaces; every text copyable; `SPC j .`
  registers a repository from a file.
- OPEN, awaiting the owner: async work (detached subagent/shell) must be
  bubbles at the response rail AND featured in the footer; three questions
  asked (dashed lane divider? "moved" duplicate card? clock/stop where?).
  Realtest 10 interrupt-key ruling still open. Hibernation compactions do
  not narrate to the footer (terminal phase "hibernated" unruled).
- Realtest 9 remains twice-green as of 00:3x; today's changes have NOT been
  re-swept. Next sweep should run 1-9 again before any new section.

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
