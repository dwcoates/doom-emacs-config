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
| Daemon | overhaul/daemon @ 892fab33e (FINAL 2026-09-02; opus-medium finisher; itest 539/0/4 skip) | docs/overhaul/reports/STOP-daemon.md | see STOP |
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

## USER RULING 2026-09-02: flaky tests are unacceptable; rerunning is not a strategy

Every intermittent, order-dependent or load-sensitive failure is a defect to
be root-caused and fixed at its SOURCE: production code when it reflects a
real race or contention there; test code only when it is truly a test
artifact. "Passes on rerun" is evidence of a race, not a resolution.
"Pre-existing, left alone" is not an acceptable disposition. Never widen a
bound to hide it. Known items to squash before close-out: daemon
scheduled-drain under load; two daemon detached-shell tests at the 5 s
bound under load; sidecar swept-up-terminal; webapp harness boot transport
race (parallel runs); elisp order-dependent integration tests (verbs refusal
context, composer command-refused, composer transferring-away, link bounce
indicator, composer merge-parked badge).

## USER RULING 2026-09-02: only OUR systems run for real; e2e mocks git and the SDK

Reverses the project lead's same-day ruling that e2e merge tests use real
git. In every suite, including e2e, only claude-repld, the shim, shim-store,
shim-sidecar (and the frontends where applicable) run for real; git is the
scripted fake git, the SDK is the fake SDK, nothing external executes. Tests
must be fast. After each suite run the worst offenders by duration get an
opus-low root-cause pass: real external dependency executing, production
timer ridden, or misbehavior; findings feed remediation.

## E2E SUITE REBUILT AND MERGED 2026-09-02 (overhaul/integration 780ec2f6c)

modules/app/agent-repl/e2e: own Go module importing the daemon integration
harness (three additive seams: MainAt, Opts.ShimNode/ShimMain/StoreSocket).
20 area files, 98 tests, 69/69 goldens mapped (67 assert, 2 ctrl-b skips
ruled out of scope, 1 KillTurn-not-open skip lacking a black-box trigger).
Real claude-repld + shim + shim-store + shim-sidecar per test; fake SDK and
scripted fake git only; tests write nothing; grep gate in TestMain. NEVER
RUN YET (compile gate only) — step 7 is the first execution. SPEC.md in the
package; coverage table in reports/E2E-SCENARIO-COVERAGE.md (with a
manifest/registry scenario-name drift section for a later shim pass).
Landing 8 merged for daemon (5214cf70c) and webapp (45376d65b); elisp pending.

## LANDING 8 (2026-09-02, user-approved; protos 1fdf85e63, bindings 3791cd630)

FeedSessionSeparation.compaction_failed and five FeedTurnEndedErrored arms
(max_turns, max_budget, execution_error, turn_failed, stop_hook_prevented).
Adaptation: opus-low implementers off overhaul/integration for daemon, webapp,
elisp (worktrees integration-agents/landing8-<sys>); e2e writers update the
compaction and turn-lifecycle assertions to the new arms.

## STEP 7 PROCEDURE (user, 2026-09-02): run once, table first, then remediation

The project lead runs the rebuilt e2e suite ONCE with `-v -json` redirected to
a file in the scratchpad (never read raw output into context), derives a
per-test `test | duration | result` table sorted by duration into a second
file, and shows the user that table plus totals BEFORE any remediation is
dispatched. Then an opus-low agent root-causes the worst offenders by
duration (real external dependency executing, production timer ridden, or
misbehavior in production or in the test). HARD STOP (user, 2026-09-02): the
project lead summarizes that report to the user and the two decide together
before ANY remediation is dispatched; slow tests are a defect class of their
own ("slow tests mean slow development, and are highly suggestive of bad
behavior"). Only after that are failures dispatched to opus-low implementers
(all collected in one run, never fail-fast).

## USER DECISION 2026-09-02: REBUILD the cross-system e2e suite

Approved ("go for it"). New top-level package modules/app/agent-repl/e2e,
owned by the project lead (only the project lead runs it). Basis: the daemon
integration harness (real claude-repld binary), with the REAL shim, store and
sidecar in place of its fakes; the fake SDK is the only mock and the only
writer of vendor files; tests write nothing. Spec inputs: E2E-EVENT-INVENTORY
(what the old suite asserted), the 69 goldens and E2E-SCENARIO-COVERAGE gaps,
the cross-system rulings in the plan docs. Orchestrated by the existing
e2e-cleanup opus-low orchestrator (sonnet-medium writers; compile gate only;
no production changes) on branch overhaul/e2e-cleanup (worktree
doom-overhaul/e2e-cleanup, now at the merged integration tip).

## FIVE-WAY MERGE LANDED 2026-09-02 (overhaul/integration)

Merged in order daemon 892fab33e, shim d8236003d, webapp e5cd9002c, store
92bb708e4, elisp b80504704 (two docs conflicts resolved: agent-shim/AGENTS.md
takes the store text with `wire/` marked DELETED; wire/AGENTS.md deleted with
the package). Everything builds: proto check-generated, daemon build + vet
(both tag sets), store, sidecar, shim typecheck, webapp typecheck. Elisp's
tightwaits and shared-fake-daemon branches are test-only and merge later.

FINDING: daemon/e2e NO LONGER EXISTS. The daemon lead rebuilt the daemon from
scratch (23cc6a672, 2026-08-29) and deleted the whole old tree including the
83-file e2e suite, which targeted the OLD daemon's internals. The cross-system
e2e suite must be REBUILT against the new daemon; E2E-SIDECAR-PLAN steps 5-6
(rewriting the old files) are moot; the step-2 inventory becomes part of the
new suite's spec. Decision owed by the user (see the ledger).

Daemon final: 892fab33e, STOP-daemon.md; owed items: abandoned-merge cause has
no producer (behavior decision), displaced-turn test needs a freeze hook,
~10 subscribe-after-trigger race sites in merge_test.go, KillTurn-after-capture
confirmed as ruled (daemon.md wording to update).

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
