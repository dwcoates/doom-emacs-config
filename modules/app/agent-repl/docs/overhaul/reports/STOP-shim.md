# STOPPING POINT — shim (wind-down 2026-08-31, shim teamlead)

## Where things stand

- Branch `overhaul/shim`, worktree `~/.config/doom-overhaul/shim`. Last code
  commit: `ba1281377` (merge of remediation #1); this file's commit is the tip.
- No shim agent worktrees or branches remain; no agents are running. One
  uncommitted scratch file (`test/integration/zz-fm.test.ts`, remediation #1's
  fast-mode probe) was discarded with its worktree.
- Verified AT THE TIP: `npm run typecheck` clean; `npm test` 89 files /
  2270 tests green; `npm run build` green; `npm run smoke` green (real --fake
  StartSession). Every test environment exports
  `AGENT_REPL_FORBID_VENDOR_CALLS=1`; the vendor was never called.
- Landings 1–5 merged; every proto ask answered (failure arms, StartTurn
  page, not_deliverable/unsupported, StoreLineAt, WatchBashRun, lost arms,
  rate_limit_status, context_budget_warning page line, not_observed).

## What is DONE (merged and green)

- Process shell + Connect service (17 rpcs; workflow trio Unimplemented;
  preface-sniffed h1+h2c on one UDS; streaming header flush; validation).
- Engine: StartSession fresh/resume, cold gate, keep-alive (4 min; fake-only
  env override) + resumeSessionAt rewind, compaction, backups, R9 identity
  (+ rotation link files), one-turn-in-flight, permission gate, detached
  work (DetachedWorkId == tool_use_id), KillTurn/KillSession scoping,
  Hibernate, WatchSession fan-out (immediate diagnostics; context_usage;
  rate_limit_status; fast_mode; unsolicited model_changed; converter-defect
  faults + degraded windows), SIGTERM stand-down, SIGINT refusal.
- Record plane: the full SDK→conversation.v1 fold (21 tool kinds, terminals,
  session updates, residue with ruled keys), store client (WriteBatch retry
  buffer, WatchBashRun, per-delta bash keys, typed-arm-aware reads,
  reconciliation with created-origin live_work and lost.swept_up).
- Mocked vendor: 140+ `!scenario` roster (table in agent-shim/claude/shim/
  AGENTS.md, asserted both ways); vendor-shaped files on disk (layout in
  shim.md's mock section); capture harness ready (scripts/capture; the Q5
  capture run itself is still pending on the operator).
- Integration suite: seven suites (~193 tests incl. 2 remaining todos) +
  harness under test/integration[-support]; `npm run test:integration`.

## Failure inventory and disposition

- Run #2 (tree 731aa5f00, BEFORE remediation #1): 99 failed / 90 passed /
  4 todo; log at the session scratchpad `itest-run1.log`/`itest-run2.log`
  (may be reaped; numbers preserved here).
- A read-only analysis bucketed the 99 into 12 root causes (full text in
  the ledger section below); remediation #1 has ALREADY fixed parts of
  bucket 3 (fast_mode, model_changed) and buckets addressed by items a–m.
  The 12 buckets, condensed, with disposition:
  1. Harness spawn-wait flake (~29, fs.watch lost events; fix: bounded
     re-drain or logPipe delivery) — OPEN, harness-side, fix FIRST.
  2. KillSession never exits the process (9; engine tears down but only
     signal handlers call process.exit) — OPEN, engine.
  3. Owned-arms filter drops fold-produced session arms; probes never
     re-run (9–10) — PARTLY FIXED (fast_mode, model_changed landed);
     accountUsage/mcpServer re-probe still OPEN.
  4. Fake store fanOut drops upserts of rows predating the watch (~8–12;
     pin compares the row's original pointer) — OPEN, test fake.
  5. StartTurn's R15 writeDurable lets PersistenceError escape as
     Code.Internal; WatchBash/StopBash same gap (7) — OPEN, engine/routes.
  6. Fake store never refuses reads (3; typed arms exist in the protos and
     the fake must serve them) — OPEN, test fake.
  7. SHIM_BUILD_SHA baked by esbuild define; spawn env ignored (2) — OPEN,
     build-identity/build.mjs.
  8. DetachForeground has no foreground-unit table (three refusals collapse
     to unknownUnit) and the mock's backgroundTasks mutates-then-emits so
     the engine reads a stale flag (4) — OPEN, engine + mock.
  9. Mock writes subagent files under its task id while the shim's AgentId
     is the spawning tool_use_id (1 ENOENT + 2) — OPEN, mock.
  10. Permission-gate arms/ordering (deny-user still runs the tool;
      undecidable→policy; mismatch→no_open_ask; KillTurn noTurnOpen with
      live detached work) (4) — OPEN, engine.
  11. Residue never lands as vendor_specific unserved rows (2) — OPEN,
      converter/writer.
  12. Singletons (5): poisoned log sink kills the shim; SetSessionModel
      resolves early; usage carrier absent; bash readability unset; and ONE
      TEST-VS-DESIGN CONTRADICTION — "a fresh session's opening page is
      EMPTY" vs R15 (the prompt row is durable before the page is read, so
      the page contains it): needs a lead ruling at resumption.

## Remediation #2 — full queued scope (NOT dispatched; opus-low when resumed)

1. The 12 buckets above (order: 1, 2, 3-remainder, 4, then the rest).
2. The triaged audit at docs/overhaul/reports/shim-audit-1.md: 26 untested
   obligations (A), 15 weak assertions (B; B2's typed-arm reader fix is
   DONE), 12 edge cases (C), todo dispositions (D; D1–D3 unblocked by
   remediation #1), 8 flakiness/internals fixes (E).
3. Capture-checklist questions (relay to the project lead's capture run):
   the real context-budget-warning attachment carrier (context_tip is NOT
   it); the failed-subagent transcript shape; the /clear rotation record;
   the declared-only toolUseResult shapes listed in shim.md.
4. After remediation #2: a fresh integration run, then the next
   fresh-context fable audit; loop until clean per TEAMLEAD.md.

## Dispatch order on resumption

1. opus-low remediation #2 in a hand-made worktree `overhaul/shim-<slug>`
   (brief = this file + shim-audit-1.md + a fresh run log).
2. Fresh integration run; iterate.
3. Fable auditor #2 (fresh context) once green-ish; loop.
4. Report to the project lead per COMMON.md (must include: the R9 rule +
   evidence — in shim.md; the mock file layout — shim.md mock section; the
   prompt→scenario table location — agent-shim/claude/shim/AGENTS.md).

## Where everything is written down

- docs/overhaul/shim-fanout.md — the lead plan, every adopted ruling, and
  the LIVE LEDGER (agent ids, landings, rulings R1–R15 + relays).
- docs/overhaul/shim.md — contract context + kickoff/landing relays + the
  mock section (file layout, R9 settled, cross-plane rulings).
- docs/overhaul/reports/shim-audit-1.md — the triaged audit (remediation
  #2 input).
- The project lead is reached by SendMessage to `main`.
