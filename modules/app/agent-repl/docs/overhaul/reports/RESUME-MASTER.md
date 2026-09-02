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
| Daemon | overhaul/daemon @ (PENDING final report; last known 8c5b2fd23, itest 289/342, final pass + audit 1 running) | docs/overhaul/reports/STOP-daemon.md | see STOP |
| Shim | overhaul/shim @ f9dcbaa80 (STOP-shim.md; code 032a9f0ab) — RESOLVED 2026-09-02: itest 302/0/3, unit 3806, landing 6 merged, dead-code lists STOP §7, capture-checklist answers in STOP | docs/overhaul/reports/STOP-shim.md | none |
| Webapp | overhaul/webapp @ a2bc62c88 (STOP-webapp.md; code 3c2c632fb) — RESOLVED 2026-09-02: integration 13 files/1601/0, unit 3279, landing 6 merged, dead-code done | docs/overhaul/reports/STOP-webapp.md (+ webapp-briefs/UX-LIST.md for the user) | none |
| Elisp | overhaul/elisp @ 53bc5b96e (STOP-elisp.md; code 05c49d574) — RESOLVED 2026-09-02: 3517/3518 (one order-dependent red documented), audits 2+3, dead-code lists elisp-fanout.md §17, user toss-ups STOP §Toss-ups | docs/overhaul/reports/STOP-elisp.md | none |
| Store+sidecar | overhaul/store @ 00b8be0c1 (STOP-store.md; code 6e2795771) — RESOLVED 2026-09-01: gates green -race, staticcheck zero, landing 6 merged, dead-code lists in STOP | docs/overhaul/reports/STOP-store.md | none |
| Cross-system e2e | overhaul/e2e @ 9dc3b148e | STOPPING-POINT section in the e2e README | suite has NEVER run |
| Contract | overhaul/integration @ dc65c084f (landing 6 landed: protos d46e601e7, bindings 8a98e4fca) | this file; PROTO-CHANGES.md (landings 1–6 + lock ruling) | landing-7 list below |

Directives: ~/.config/doom-overhaul/directives/{COMMON,DAEMON,SHIM,WEBAPP,ELISP,STORE}.md.
A recreated lead's mandatory reading order: TEAMLEAD.md → its system PLAN DOC
(docs/overhaul/<system>.md — the durable rulings record, current through
landing 5, incl. every "Landing N relay" section) → its STOP file → the
directives. The plan docs are authoritative over memory of any prior session.
The shim's 138-scenario mock table lives on overhaul/shim (AGENTS.md) and
has NOT reached overhaul/integration yet.

## Landing 7 — LANDED 2026-09-02 (protos ab7e681f2, bindings c10714a41, docs d67a34ab4)

Adaptation dispatched 2026-09-02 by the project lead (no leads): opus-low
implementers in <sys>-agents/landing7 worktrees on branches
overhaul/<sys>-landing7 (cut from each lead tip, overhaul/integration merged
in) for shim, webapp, elisp, store; the daemon's share is queued on the
running opus-medium finisher after its dead-code merge. Project lead merges
each branch into overhaul/<sys> on report, then removes worktree + branch.
Store tightwaits merged 7c49a7d77; elisp tightwaits still running.

Original list (all landed, none deferred beyond the two noted):

- agentrepl SubmitPromptError.bubble_refused{kind: not_deliverable|agent_busy}
- shim.v1 StartSessionFresh.model optional (SDK default)
- shim.v1 WatchSession push arm session_started (re-announce on every new watch; adoption = pure attach)
- shim.v1 UpdateAgentFailure.agent_busy
- agentrepl CloseWorkspaceBlocked composed-reason fields
- store.v1 OpenAgentSessionFailure.unknown_agent (cross-plane; store lead must agree)
- deferred: AgentToolFailure denied marker (only if the permission-id join is awkward); OpenWorkspaceTranscriptMissing composed text

## Cross-system rulings 2026-09-01/02 (no proto; recorded in PROTO-CHANGES.md and the plan docs)

- Workspace kernel lock taken in StartSession, not at shim startup (rollout prelaunch).
- Denied tool: eager start kept; unit settles failure with content UNSET, drawn denied via the permission unit (id == activity id).
- /clear rotation: new id = second system:init's session_id; conversation_reset.new_conversation_id never adopted.
- Watch* refusals are transport-closed by design; logged INFO, never WARNING.
- Webapp: no pull-driven health surface; session faults via pushed topbar warnings; UpdateMergeQueue has no webapp surface.
- Compaction summary line still ungrounded (cheap-model capture never compacted); needs a user-approved longer-history capture.

## E2E cleanup orchestration (2026-09-02)

Steps 2-6 of reports/E2E-SIDECAR-PLAN.md are orchestrated by ONE opus-low
agent that dispatches sonnet-medium writers only, never runs the e2e or
integration suites (compile gate only), and makes NO production code changes
(fake SDK and e2e harness/tests are test tooling and in scope; anything
needing production change is reported back undone). Branches:
overhaul/shim-fakesdk (worktree shim-agents/fakesdk) and overhaul/e2e-cleanup
(worktree doom-overhaul/e2e-cleanup, off integration). Steps 5-6 wait for
the project lead's "merge landed" message. Project lead resumes at step 7:
run the e2e suite, dispatch remediation, then audit gaps, coverage, hardening.

## USER RULING 2026-09-02: e2e runs a REAL sidecar; no test writes the store

daemon/e2e tests that hand-write sidecar events into the store are wrong.
They write vendor JSONL and a real shim-sidecar ingests it. Plan and steps:
reports/E2E-SIDECAR-PLAN.md. Done by sonnet-medium agents after the five-way
merge, before coverage hardening. Also settled: daemon/integration = real
daemon binary against fakes of every neighbor; daemon/e2e = the cross-system
suite (real shim + store + sidecar, hosted daemon, no frontends). Both
survive the merge.

## USER RULING 2026-09-02: no playtests; e2e coverage hardening instead

The final playtest step is DROPPED. After the cross-system e2e suite passes
on overhaul/integration, a HARDENING step runs: measure coverage of the e2e
run (daemon `-coverpkg=./... -coverprofile` restricted to ./e2e; shim
vitest coverage over its mock-scenario suite; sidecar TestMockScenarios),
then shore up gaps. Standard: high, weighted toward important and complex
areas (permission asks, interrupts, compaction and session rotation,
subagents, detached bash, merge and hold flows, failure and refusal arms);
simple text-turn paths rank low. Ahead of it: a sonnet audit maps every
mock-golden capture scenario (~70 at ~/.config/doom-overhaul/captures) to
the e2e/integration test that exercises it (scratchpad
e2e-scenario-coverage.md); uncovered high-rank scenarios become e2e tests
dispatched to opus-low implementers.

## USER RULING 2026-09-02: leads are finished once parked

A parked lead is DONE. The project lead takes it home: landing 7, the
five-way merge into overhaul/integration, the e2e suite, playtests, and
EVERY remediation are dispatched by the project lead directly to opus-low
(or sonnet-medium for rote) implementers in worktrees off
overhaul/integration. No lead is re-woken for any of it.

## Project lead's own queue, in order

1. DONE 2026-09-01 — shim's parked ruling: R15 WINS. On a fresh session the
   page StartTurn returns contains exactly the just-delivered AgentPrompt
   row (durable before the page is read; StartTurn submits AND paints).
   The "a fresh session's opening page is EMPTY" test is wrong and is
   amended; only a WatchAgent opened before any turn legitimately opens
   with an empty page. Goes into the shim lead's recreation brief.
2. Recreate the five teamleads (fable-low) with: their STOP file path, the
   resumption rules above, and their recorded dispatch queues. The old
   subagent ids are in the project ledger but treat them as unreachable.
   2026-09-01: daemon, webapp, elisp, store leads RECREATED and running;
   shim lead recreated later the same day once the sweep finished (68
   goldens; its brief carries the captures directory, the ruling above,
   the store relays, and two harness defects to fix).
3. DONE 2026-09-01 — Landing 6 (protos d46e601e7, bindings 8a98e4fca):
   command_acted, duplicate_submission, unknown_repository; turn_already_open
   retired. FeedPermissionArguments' wire source and FeedFindingsRow identity
   deferred to the e2e loop (no producer asked).
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

1. CAPTURE RUN: RESOLVED 2026-09-01. User granted the run; one prose-streamed
   golden validated end to end at ~/.config/doom-overhaul/captures/ (stream,
   meta, transcript copy; account root left clean). Three harness fixes were
   needed and are committed on overhaul/shim (0369dffe3 config-root at the
   default root must leave CLAUDE_CONFIG_DIR unset; c50feea4d realpath the
   scratch world; 98f23d9c7 slug underscores). Invocation that works:
   `unset AGENT_REPL_FORBID_VENDOR_CALLS && node scripts/capture/capture.mjs
   --i-am-the-project-lead-capture-run --config-root ~/.claude --out
   ~/.config/doom-overhaul/captures [--only ...]`. Remaining: the full
   sweep (project lead only; may run concurrently with the fanout, it is an
   input the shim lead consumes, not a gate), and the MANUAL scenarios
   (api-error-classes, model-refusal) which need non-prompt provocation.
   The capture checklist still carries: the real budget-warning attachment
   spelling (`context_tip` is NOT it), the failed-subagent transcript
   shape, a real compaction-summary line, the api_error taxonomy fixtures.
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
