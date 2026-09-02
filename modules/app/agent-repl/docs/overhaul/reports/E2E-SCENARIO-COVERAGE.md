# E2E scenario coverage audit (daemon/e2e only)

Scope: does a test in `daemon/e2e` — and only `daemon/e2e` — drive each of
the 69 named golden-capture scenarios through the real shim and the real
store, and assert on daemon-rendered frames? Re-derived from source; see
Methodology for what counted and what did not. This audit does not run any
test suite; `daemon/e2e` is read-only source analysis.

## Headline finding

**0 of 69 scenarios are covered.** The precondition the question assumes —
a fake vendor that can be told "run scenario X by name" — does not exist in
this worktree.

`daemon/e2e` builds and drives the shim FROM SOURCE at
`/Users/dodgecoates/.config/doom-overhaul/e2ecleanup-coverage/modules/app/agent-repl/agent-shim/claude/shim`.
That shim's fake vendor is
`agent-shim/claude/shim/src/fake-query.ts` (869 lines) — there is no
`src/fake/` package, no `registry.ts`, no per-scenario files, and none of
the 69 golden-capture names appear anywhere in this worktree's shim source
or git history (`git log --all -- .../src/fake/index.ts` returns nothing on
this branch). `fake-query.ts`'s entire vocabulary is eight generic tokens:
`!tool <cmd>`, `!hold`, `!md`, `!rotate`, `!query-eof`, `!agent <name>`,
`!bg <cmd>`, `!usage-subagent`, plus the `FAIL_TURN_MARKER` substring and a
turn-gate env-var hook. None of those tokens is, or unambiguously invokes,
any of the 69 named scenarios in
`shim-agents/fakesdk/.../shim/scripts/capture/prompts.json` — that richer
registry (`src/fake/index.ts` + `src/fake/scenarios/*.ts`, ~140 scenario
names such as `bash-detach`, `perm-allow-once`, `hook-blocked`) lives only in
the separate `shim-agents/fakesdk` worktree and has never been part of this
branch.

This matches the plan doc at
`/Users/dodgecoates/.config/doom-overhaul/e2ecleanup-coverage/modules/app/agent-repl/docs/overhaul/reports/E2E-SIDECAR-PLAN.md`,
which states the named-scenario fake SDK is to be wired into `daemon/e2e`
only after "the five-way merge lands" and describes today's `daemon/e2e` as
30 files that call `sidecar*Event(...)` / `ingestTranscriptAsSidecar(...)` —
i.e. writing store facts by hand rather than producing them by driving the
shim. Those hand-written-event tests are exactly the pattern the audit brief
excludes from credit.

Where a `daemon/e2e` test's *behavior* resembles a golden scenario (e.g.
`rotation_e2e_test.go`'s `!rotate` plus an injected `ContextCleared` event,
which the file's own header explains stands in for
`identity-rotation-clear`'s `/clear`), the test says outright that it is
substituting a hand-written event because the sidecar cannot produce the
real shape in fake mode. That is the disallowed pattern by name, not an
unambiguous equivalent invocation of the golden scenario, so it is marked
"no" below with a citation to the substitution.

## Per-scenario table

All 69 scenarios: not covered by the audit's definition. The "closest
existing test" column names a `daemon/e2e` test that touches the same
feature area for context — none of them qualify as coverage, per Methodology.

| Scenario | Covered? | Test function : file:line, or dash | Closest existing test (does not count) |
|---|---|---|---|
| account-usage | no | — | `TestE2E...` accounting tests in `usageaccounting_e2e_test.go` drive `!usage-subagent` (a distinct fixture for per-turn usage-observation plumbing), not the golden scenario's `accountInfo`/`usage_EXPERIMENTAL` controls |
| artifact-publish-and-list | no | — | none |
| bash-detached | no | — | `detachedworkanchor_e2e_test.go:19` `TestE2EALaunchAnchorsItsDetachedWorkInTheFeedWithLiveLiveness` drives `!bg <cmd>`, the generic detached-shell primitive, not the named scenario |
| bash-foreground-completed | no | — | generic `!tool` turns throughout the suite exercise a foreground Bash round, but none invoke this named golden capture |
| bash-image-output | no | — | none (fake-query.ts has no image-bearing tool result path) |
| bash-interrupted-by-timeout | no | — | `interrupt_e2e_test.go` covers turn interrupt generically via `!tool sleep ...`, not a Bash-timeout-specific shape |
| bash-nonzero-exit | no | — | none |
| bash-partial-output-with-spill | no | — | none |
| compaction-directed | no | — | `clearcompact_e2e_test.go` (11 `sidecar*Event`/`ingestTranscriptAsSidecar` call sites) hand-writes compaction facts into the store instead of driving a compaction scenario through the shim |
| context-budget-warning | no | — | `contextcostalert_e2e_test.go` — not inspected line-by-line but the plan doc names this scenario's "attachment spelling" as a known fake-writer defect still to be fixed (E2E-SIDECAR-PLAN.md step 4b) |
| context-injected-memory | no | — | none |
| context-injected-skills | no | — | none |
| context-usage | no | — | `tokenutilization_e2e_test.go` (4 hand-written-event call sites) synthesizes usage facts rather than driving the scenario |
| cron-create-list-delete | no | — | none |
| ctrl-b-detach-of-foreground-subagent | no | — | none |
| ctrl-b-detach-of-foreground-work | no | — | none |
| diagnostics | no | — | none |
| edit | no | — | none (fake-query.ts has no Edit-tool round) |
| fan-wide-cancel | no | — | `detachedcancel_e2e_test.go:128` `TestE2ECancelStopsARunningDetachedAgent` cancels one detached `!agent`/`!bg` turn via the generic primitive, not the named fan-wide-cancel shape; the plan doc separately flags this scenario's agent-spool `EXIT=` terminator as a still-open fake-writer defect (step 4b) |
| fast-mode | no | — | none |
| glob | no | — | none |
| grep-content-files-count | no | — | none |
| held-turn-gate | no | — | permission tests in `permission_e2e_test.go` (e.g. `TestE2EPendingPermissionResolvesThePermissionState` at line 83) hold a `!tool` turn open at its `canUseTool` gate using the generic primitive, which is the same *idea* as held-turn-gate but not an invocation of that named scenario (no delayed permission_script, no getContextUsage-while-held control) |
| hook-blocked | no | — | none (fake-query.ts has no PreToolUse/PostToolUse hook simulation at all) |
| hook-cancelled | no | — | none |
| hook-failed | no | — | none |
| hook-succeeded | no | — | none |
| ide-diagnostics-after-edit | no | — | none |
| identity-rotation-clear | no | — | `rotation_e2e_test.go` drives `!rotate` (fake-query.ts's `runRotateTurn`) then injects a hand-written `sidecarClearEvent`/`ContextCleared` fact, which the file's own header documents as a stand-in because the sidecar cannot produce a real `/clear` transcript in `--fake` mode — the excluded pattern by the file's own admission |
| interrupt | no | — | `interrupt_e2e_test.go` interrupts a `!hold`/`!tool` turn generically; not an invocation of the golden `interrupt` scenario (which specifically interrupts mid text-stream after a `text_delta`) |
| max-tokens | no | — | none |
| mcp-server-healths | no | — | none |
| mcp-unmodeled-tool | no | — | none |
| model-changed | no | — | none |
| monitor-deadline | no | — | none |
| monitor-persistent | no | — | none |
| permission-allow-once | no | — | `permission_e2e_test.go` exercises the allow/decline transitions generically over `!tool`, not the named scenario's specific script |
| permission-allow-standing | no | — | same as above |
| permission-denied-by-policy | no | — | same as above |
| permission-denied-by-user | no | — | `permission_e2e_test.go:155` `TestE2EDeclinedPermissionStopsTheTurn` is the closest generic analog; not the named scenario |
| permission-mode-changed | no | — | `permissionmode_e2e_test.go` tests mode plumbing (auto/explicit at create), not a mode-changed-mid-session scenario |
| permission-undecidable-parked | no | — | `bouncepromptpark_e2e_test.go` / `parkedledger_e2e_test.go` test prompt parking across daemon bounces generically, not this named permission scenario |
| plan-mode-enter-exit | no | — | none |
| prose-streamed | no | — | every `!md`/default-echo turn in the suite streams prose, but none names or reproduces this scenario's specific four-block (withheld-thinking + visible-thinking + two text blocks) shape |
| push-notification-not-sent | no | — | none |
| push-notification-sent | no | — | none |
| question-free-text | no | — | none (fake-query.ts has no question/ask-tool simulation) |
| question-multi-select | no | — | none |
| question-multiple-in-one-batch | no | — | none |
| question-single-select | no | — | none |
| question-unanswered | no | — | none |
| read-whole-head-range | no | — | none |
| report-findings | no | — | none |
| schedule-wakeup-schedule-and-stop | no | — | none |
| send-message-queued-and-resumed | no | — | none |
| skill-invocation | no | — | `skillbody_e2e_test.go` (13 hand-written-event call sites, the heaviest in the suite) hand-writes skill-body facts into the store instead of driving a skill-invocation turn through the shim |
| subagent-detached | no | — | `subagentrouting_e2e_test.go:28` `TestE2EASubagentResponseIsRoutedToItsDetachedWorkAndNeverToTheFeed` drives `!agent <name>` generically, not the named scenario |
| subagent-sync-nested-activity | no | — | none (fake-query.ts's `!agent` path has no nested/synchronous-activity shape) |
| task-acts-create-change-reject | no | — | none |
| turn-stop-error-during-execution | no | — | `mergeactions_e2e_test.go:58` `TestE2EMergePipelineFailsTheRunWhenTheBeforeActionErrors` drives `FAIL_TURN_MARKER` generically (documented in fake-query.ts as existing solely so a turn can fail offline), not the named turn-stop scenario with its specific stop-reason arm |
| turn-stop-hook-stop | no | — | none (no hook simulation exists to produce a hook-stop arm) |
| turn-stop-max-budget-usd | no | — | none |
| turn-stop-max-structured-output-retries | no | — | none |
| turn-stop-max-turns | no | — | none |
| vendor-answered-slash-commands | no | — | `slashdurability_e2e_test.go` / `slashdurabilityctor_e2e_test.go` (6 hand-written-event call sites) hand-write slash-command durability facts rather than driving a vendor-answered-slash-command turn |
| web-fetch | no | — | none |
| web-search | no | — | none |
| worktree-enter-exit-kept-and-removed | no | — | none |
| write-created-and-updated | no | — | none |

## Ranked gap list

All 69 scenarios are gaps. Ranked by the audit brief's HIGH-priority
families first, then by how far the current suite is from being able to
close the gap (a shim with zero support for the mechanism ranks above one
already faking a related, wrong-named shape).

1. **Permissions** — `permission-allow-once`, `permission-allow-standing`,
   `permission-denied-by-user`, `permission-denied-by-policy`,
   `permission-undecidable-parked`, `held-turn-gate`.
   - `fake-query.ts`'s `!tool` path can only ever ask a single generic
     `canUseTool`; it has no scripted arms (default decision, per-tool
     delay, allow-once vs allow-standing vs undecidable) — the fake needs
     new scenario options before any of these six can be driven by name.

2. **Interrupts** — `interrupt`, `bash-interrupted-by-timeout`.
   - Generic interrupt plumbing exists (`interrupt_e2e_test.go`), but
     nothing reproduces the golden scenario's specific triggers (interrupt
     exactly after a `text_delta`; a Bash tool's own timeout ending the
     call while the vendor is not itself interrupted).

3. **Compaction / rotation** — `compaction-directed`,
   `identity-rotation-clear`, `context-budget-warning`.
   - These are the worst-instrumented in a specific way: `daemon/e2e`
     *believes* it covers them (11 and 3+ hand-written-event call sites
     respectively in `clearcompact_e2e_test.go` and `rotation*_e2e_test.go`)
     but by construction those events never pass through the shim or
     sidecar, so a regression in the real `/clear` or `/compact` path in
     the shim itself is invisible to this suite. `E2E-SIDECAR-PLAN.md`
     already schedules this remediation.

4. **Subagents** — `subagent-detached`, `subagent-sync-nested-activity`.
   - `!agent <name>` exists but only spawns a flat detached agent; there is
     no synchronous/nested-activity shape at all.

5. **Detached bash** — `bash-detached`, `ctrl-b-detach-of-foreground-work`,
   `ctrl-b-detach-of-foreground-subagent`.
   - `!bg <cmd>` covers "start detached"; nothing models a live foreground
     turn being detached in place (the ctrl-b mid-flight conversion), which
     is the harder and more valuable half of this family.

6. **Merge / hold** — `fan-wide-cancel`.
   - `detachedcancel_e2e_test.go` cancels one agent at a time; nothing
     drives a fan-wide (multi-agent) cancel. The plan doc separately flags
     a real fake-writer defect here (missing `EXIT=` terminator on agent
     spools) blocking this even after scenario support lands.

7. **Failure arms** — `turn-stop-error-during-execution`,
   `turn-stop-hook-stop`, `turn-stop-max-budget-usd`,
   `turn-stop-max-structured-output-retries`, `turn-stop-max-turns`.
   - `FAIL_TURN_MARKER` produces exactly one generic failure shape
     (`error_during_execution`, informally); the other four stop-reason arms
     have no fake path to reach at all.

8. **Hooks** — `hook-succeeded`, `hook-blocked`, `hook-failed`,
   `hook-cancelled`. Zero support in `fake-query.ts`; no hook lifecycle is
   modeled offline.

9. **Skill invocation / vendor slash commands** — `skill-invocation`,
   `vendor-answered-slash-commands`. Both have heavy hand-written-event
   coverage (`skillbody_e2e_test.go`: 13 sites; `slashdurability*`: 6 sites)
   that gives a false sense of security for the same reason as item 3.

10. **File-tool scenarios** — `edit`, `glob`, `grep-content-files-count`,
    `read-whole-head-range`, `write-created-and-updated`,
    `ide-diagnostics-after-edit`. Zero support: `fake-query.ts` only ever
    runs a Bash-shaped tool call, never Edit/Read/Glob/Grep, so this whole
    family is unreachable until those tool shapes are added to the fake.

Everything else in the 69 (web-fetch/web-search, questions, artifacts,
cron, monitor, push-notification, task-acts, plan-mode, context-injected-*,
diagnostics, model-changed, mcp-*, max-tokens, fast-mode,
schedule-wakeup, send-message-queued-and-resumed, report-findings,
worktree-enter-exit) has no support in `fake-query.ts` and no `daemon/e2e`
test even attempting an adjacent shape.

## Scenarios I could not fully determine

None. Every scenario resolved cleanly to "not covered," either because
`fake-query.ts` has no mechanism to produce the shape at all (checked by
reading the full dispatch table, lines ~750-869, and the whole file's
documented token list) or because the one plausibly-adjacent test names its
own hand-written-event substitution in its header comment (rotation,
compaction, skill-body, slash-durability, token-utilization families).

## Methodology

**Counted as coverage** (none found): a `daemon/e2e` test that sends a
prompt selecting one of the 69 golden-capture scenarios by its literal name
or an unambiguous equivalent invocation, through the real shim built from
source in this worktree, into the real store, with assertions on frames the
daemon renders.

**Excluded, and confirmed excluded in every case checked**:
- Tests using `fake-query.ts`'s eight generic DSL tokens (`!tool`, `!hold`,
  `!md`, `!rotate`, `!query-eof`, `!agent`, `!bg`, `!usage-subagent`) or the
  `FAIL_TURN_MARKER` substring, none of which is, or maps 1:1 to, a named
  golden scenario — verified by reading `fake-query.ts` in full and grepping
  its dispatch block (lines 750-869).
- Tests that call `sidecar*Event(...)` / `ingestTranscriptAsSidecar(...)` or
  otherwise write `protocolv1.Event`s straight into the store by hand
  (21 files, confirmed by grep; heaviest: `skillbody_e2e_test.go` 13,
  `mergewindow_e2e_test.go` 11, `clearcompact_e2e_test.go` 11,
  `machinery_e2e_test.go` 8, `hibernationharness_test.go` 7,
  `slashdurability_e2e_test.go` 6).
- Substring/keyword matches of a scenario's name inside unrelated code
  (checked and discarded for every one of the 69 — e.g. `edit`, `glob`,
  `interrupt` all appear literally in the suite's source, but as unrelated
  identifiers or generic-behavior tests, never as a scenario invocation).
- Coverage in `shim-sidecar`, the shim's own test suites, `daemon/integration`,
  or the webapp. None of these were credited even where found, per the
  audit brief.

**Structural finding that shaped every row**: this worktree's shim
(`agent-shim/claude/shim/src/fake-query.ts`) is not the same fake vendor
the 69 golden-capture scenarios in
`shim-agents/fakesdk/.../scripts/capture/prompts.json` are named against
(`shim-agents/fakesdk/.../src/fake/index.ts` and
`src/fake/scenarios/*.ts`). The richer, named-scenario fake vendor has never
existed on this branch (`git log --all` on this worktree for
`src/fake/index.ts` returns nothing). `E2E-SIDECAR-PLAN.md` confirms this is
expected: the named-scenario fake SDK is to be merged in and wired into
`daemon/e2e` in a later remediation step, after which "the scenario→e2e
audit is rerun against daemon/e2e alone." This audit is that first,
pre-remediation baseline.

**Not run**: no test suite was executed (daemon/e2e, daemon/integration, or
otherwise). All findings are from reading `daemon/e2e`'s Go sources, the
shim's `fake-query.ts`, the golden-capture `prompts.json`, the fakesdk
worktree's `src/fake/registry.ts` and `scenarios/*.ts` (for contrast only),
and `E2E-SIDECAR-PLAN.md`. `daemon/e2e` not compiling was not tested and is
consistent with the task's stated expectation.
