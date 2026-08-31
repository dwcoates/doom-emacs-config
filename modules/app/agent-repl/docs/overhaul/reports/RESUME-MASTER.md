# RESUME-MASTER — agent-repl overhaul, paused 2026-08-31

The single entry point for resuming the overhaul. Written by the project
lead at the wind-down; read this before anything else, then each system's
STOP file. The pause was deliberate (multi-day); expect cold caches and a
compacted project-lead context — this file plus the STOP files carry
everything needed, so re-derive nothing.

## Resumption rules (user rulings, binding)

- RECREATED TEAMLEADS run **Fable at LOW effort** (`.claude/agents/fable-low.md`),
  never high — contracts are settled; the work is orchestration from the
  STOP files. The original fable-high tier applies to nothing new.
- ALL implementation agents: **Opus at LOW effort** (`opus-low`). Write
  offloads to `sonnet-medium` at the implementer's judgment, offloader
  accountable.
- Any lead that compacts drops itself to low effort immediately and says so.
- No real git in any test, anywhere. No pushes/PRs; work merges via the
  workspace-merge flow.
- A silent agent is not a dead agent: verify with a message round-trip
  before reaping a worktree.

## Where everything stands (all tips verified green at the pause)

| System | Branch @ tip | STOP file (on that branch) | Left in place |
|---|---|---|---|
| Daemon | overhaul/daemon @ e2694b20f | docs/overhaul/reports/STOP-daemon.md | worktrees daemon-agents/{hostside 87e3ca5f1, integration-tests 438c81e12} unmerged; stale paintvocab/promptflow to delete |
| Shim | overhaul/shim @ 6669d9f37 | docs/overhaul/reports/STOP-shim.md | none |
| Webapp | overhaul/webapp @ b8aeaabaa | docs/overhaul/reports/STOP-webapp.md (+ reports/webapp-briefs/ incl. UX-LIST.md) | worktrees webapp-agents/{topbar-login, merge-bubble} |
| Elisp | overhaul/elisp @ 3d3ea3a2c | docs/overhaul/reports/STOP-elisp.md | none |
| Store+sidecar | overhaul/store @ 00edf4d4c | docs/overhaul/reports/STOP-store.md | none (carries a merge of overhaul/shim 731aa5f00 — expected) |
| Cross-system e2e | overhaul/e2e @ 9dc3b148e | STOPPING-POINT section in the e2e README | suite has NEVER run |
| Contract | overhaul/integration @ f13ca50ec | this file; PROTO-CHANGES.md (landings 1–5) | landing-6 material pending |

Directives: ~/.config/doom-overhaul/directives/{COMMON,DAEMON,SHIM,WEBAPP,ELISP,STORE}.md.
The shim's 138-scenario mock table lives on overhaul/shim (AGENTS.md) and
has NOT reached overhaul/integration yet.

## Project lead's own queue, in order

1. Answer the shim's parked ruling: the "empty opening page" test vs R15's
   prompt-row-before-page contradiction (STOP-shim.md).
2. Recreate the five teamleads (fable-low) with: their STOP file path, the
   resumption rules above, and their recorded dispatch queues. The old
   subagent ids are in the project ledger but treat them as unreachable.
3. Landing 6 when the daemon's wave 3 lands: six pending ERROR-ARMS rows +
   wave-3 server arms; retire landed-but-unused arms; FeedPermissionArguments'
   wire source; consider FeedFindingsRow identity (position-keyed folds).
4. As each lead finishes: merge its branch into overhaul/integration
   (expect the vocab and shim.md merges to be non-trivial), delete worktrees.
5. Run the cross-system e2e suite (~/.config/doom-overhaul/e2e worktree;
   only the project lead runs it): merge integration in, build, expect the
   README's red list, attribute failures to leads, drive the loop.
6. Adversarial e2e audit, playtests via /debug-emacs-agent-repl, full
   verifier bin/test-all.sh.
7. Put the UX lists to the user (webapp-briefs/UX-LIST.md + elisp STOP §16
   pointers; Interrupt placement; InterruptAllAgents affordance).

## Open user decisions (parked, ask again at resume)

1. CAPTURE RUN permission: `node scripts/capture/capture.mjs --config-root
   ~/.claude` is classifier-denied; options were /permissions allowlist,
   `!`-prefix run, or defer. The capture checklist now carries: the real
   budget-warning attachment spelling (`context_tip` is NOT it), the
   failed-subagent transcript shape, a real compaction-summary line, the
   api_error taxonomy fixtures.
2. Hide vs show-and-refuse this wave (default: show + refuse honestly):
   the subagent bubble composer (`not_deliverable`) and the Ctrl-B detach
   control (`unsupported`).

## Progress at the pause

~65–70% done overall. Written: ~146k lines production, ~195k test (branch
tips, generated bindings excluded). Biggest remaining block: daemon wave 3
(server/boot/cmd wiring, deploy chain, wire deletion) — the daemon is 20+
green packages that do not yet form a runnable binary. Then the three
integration suites' first-ever runs (webapp 587 blocks, shim ~185 tests,
e2e 176 tests) and their remediation loops, playtests, verifier.
