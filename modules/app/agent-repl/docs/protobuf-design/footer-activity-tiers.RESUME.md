# RESUME STATE — footer combined model (written 2026-09-30, before a compaction)

Read this first, then `footer-activity-tiers.md` ("THE PLAN", "Detached-work
liveness", "Held-queue") for the full rules. Worktree:
`~/.config/doom-worktrees/footer-activity-updates`, branch
`footer-activity-updates`, rebased onto LOCAL `master` (was `db91df803`).
Work uninterrupted, no questions, in-session (no implementation subagents).

## Done (committed on the branch)

- Rebase onto local master (the branch was squashed per system first, with the
  owner's OK; the four standalone fixes kept their own commits).
- Combined footer model, end to end: proto (`2ee755d80`), daemon (`e3813ba7e`),
  webapp (`d6750af2f`). Daemon unit + integration, webapp typecheck, lint,
  unit, integration, webkit all green at those commits.
  - Quiet tier (`FooterActivityQuietStretch` with `at`), in
    `FooterActivityTransientOverQuietOverEnduring` for working/background.
  - Shared salient kinds on every arm: `rate_limit`, `notification`,
    `context_budget` (daemon `resolve/footer/salient.go`, `fillShared`).
    Ends: notification at the next prompt (`SetTurn`/`OnSubmission`); budget at
    a successful cut, /clear, concluded compaction, a session switch
    (`OnSessionStarted` with a new vendor id), or a subagent's own terminal;
    rate-limit at an `allowed` event for the same window.
    THIS FIXES THE OWNER'S STUCK "compaction failed" LINE (master never
    cleared it on a successful cut).
  - Enduring `oneof line { usage; context_window; unobserved }`, 80% rule
    (`contextClaimsEnduring`).
  - `submitting` stages (held with position/queued, classifying, interjecting,
    coalesced, delivered); prompt queue reports them (`reportHeld`,
    `OnSubmission`). `coalesced` is declared but NOT yet emitted — the
    held-queue fix emits it.
  - Tails (thinking/response transients) deleted.
  - Cold gate: `ColdGateAnswer.Progress` announces the concluded compaction;
    `coldGateSettled` retires the gate before clearing the answer (no flash).
- Owner side requests, committed: composer 20% shorter (`1c3d0ba29`);
  compaction "▸ summary" toggle 2x size, white (`2171c9ebf`); expanding ANY
  feed item centers its row (`itemExpanded`, `9382bf7ae`); expanded
  non-prompt/non-response items capped at `--feed-item-max-h: 80cqh`
  (`c4281be1d`, WebKit-measured).

## In progress (UNCOMMITTED in the worktree)

- e2e module (`modules/app/agent-repl/e2e`) moved onto the combined model:
  new shared reader `footeractivity_e2e_test.go`; rewritten
  `sessionfacts_e2e_test.go` (rate-limit tests now assert the salient line;
  usage tests read the enduring usage via `sfAwaitUsage`),
  `accounting_e2e_test.go`, `compaction_e2e_test.go`,
  `producerfaults_e2e_test.go`, `remainder_e2e_test.go`. `go vet ./` passes.
  NEXT: run the touched scenarios —
  `cd e2e && TMPDIR=/tmp ../bin/background.sh go test ./ -count=1 -run 'TestRateLimit|TestAccountUsage|TestSampled|Compact|ContextTip|TokensReminder|QueryEof|QueryFail|Push' -timeout 30m`
  — fix failures, then commit ("test(e2e): the footer scenarios read the
  combined model").

## Remaining, in order

0. DONE since (liveness fix: proto `e9631134f` `8033c452d`, store `8ef7a2ee1`,
   shim `92e590ff6`, sidecar `ea6bf3c73`, daemon test `e1c6d558d`, record
   landed change 4): e2e committed (`7fa4407c0`), landed change 3 recorded
   (`70aa479c3`), footer percent gradient (owner request: green <40 →
   yellow 70 → orange 90, red >=90; `7239864e0` extraction, `66f672507`).
1. (done) Finish the e2e step above.
2. (done) landed change 3.
2. Record the combined model's contract decisions in
   `footer-activity-tiers.md` "Landed changes" (a new "3. The combined model"
   entry: quiet tier message + container, shared salient kinds, enduring
   oneof + unobserved arm, submitting stages, retired transient tags 4/5/11/12,
   `ColdGateAnswer.Progress`), and note the owner's 2026-09-30 requests
   (centering, 80% ceiling, toggle style, composer height).
3. (done) DETACHED-WORK LIVENESS FIX (design in `footer-activity-tiers.md`): shim
   writes the claim from SDK `task_started` to the store; sidecar claims held
   spools from store claims (and the prose matcher also matches the timeout
   wording, `launch.go:83` `backgroundSentence`); `WatchBash` on an announced
   run with no rows WAITS for the first row instead of `not_found`; daemon
   keeps the watch to the terminal; shim `convert.detached` names the vendor
   timeout instead of "by hand". Tests per the record. Record contract changes.
4. HELD-QUEUE FIX (rules in `footer-activity-tiers.md` "Held-queue"). Found:
   `/compact` typed while a turn runs goes `prompthandler.act` →
   `promptqueue.SubmitSessionAct` → in-memory `wsState.acts` (NOT the durable
   held-prompt store, so never on the tray), and `drainActs` runs acts BEFORE
   popped held prompts (overtaking). Fix: every act that must wait becomes a
   durable held entry (FIFO with prompts, visible, never classified —
   `neverJudged`/`sessionActVerdict` already exist for a held prompt whose text
   is `/compact`/`/clear`); `/model` and permission-mode acts need a
   structured act on `wsm.HeldPrompt` (schema migration) or text
   recognition; classification judges only the item immediately ahead; an
   interrupt verdict against a still-QUEUED prompt COALESCES (fold content,
   one drawer entry marked `coalesced` — needs a daemon_hold.proto/tray
   field; emit footer `StageCoalesced`); explicit interrupts still stop an
   act. Integration test of the owner's worked example.
4a. OWNER REQUESTS QUEUED 2026-09-30 (implement when convenient, before the
    docs and green pass so they land in the same cherry-pick):
    - Response bubble's top-right token figure (`webapp/src/feed/cards/
      response.ts` `drawFeedResponseUsageStamp`, `.usage-stamp`) colored by
      the SAME subroutine/gradient spec the footer's token count uses
      (`footer/strip.ts` `footerTokensHeatColor`, `--token-heat-0..3`,
      position shipped by the daemon as `FooterTokensCellInputHeat`). Needs
      the heat position on the response stamp (`FeedResponseUsageStamp`
      proto field, daemon computes it with the footer's rule) — one shared
      daemon rule and one shared client color function, no re-derivation.
    - Scroll auto-collapse of an expanded feed item fires ONLY once no part
      of the item is visible in the feed viewport (today it fires earlier).
      Find the auto-collapse-on-scroll owner in `webapp/src/feed/` /
      `scroll.ts`; test with the item partly visible (stays open) and fully
      scrolled out (collapses), both directions.
4b. SPLIT THE INTERRUPT VERDICT (owner, 2026-09-30; lands WITH the
    held-queue fix, same unit of work). Verdicts: `queue` (wait for the turn's
    end), `after_tool_call` (reaches the running turn at its next tool
    boundary; nothing is cut, no rejection text), `interrupt` (cut now,
    because the prompt invalidates the work; rare). Unsure between the last
    two: `after_tool_call`. A real `interrupt` delivers the prompt with a
    daemon-attached note saying the work was cut because this prompt changes
    it (follow it, don't stop and wait), naming the classifier's reason.
    Coalescing unchanged (either non-`queue` verdict against a still-queued
    prompt folds into it). Footer: the interjecting stage splits into "after
    this tool call" and "interrupting" (new arm on
    `FooterActivityTransientSubmitting.stage`); the drawer keeps a pending
    after-tool-call prompt until delivered; a turn blocked on a permission or
    question shows the prompt as waiting on that answer. Check whether
    `wsm.ArmInterject` is persisted (schema migration). Classifier prompt and
    parsing to three verdicts.
    MECHANISM, SETTLED BY A LIVE PROBE (SDK 0.3.280, Haiku, 2026-09-30): a
    user message pushed into the streaming input while a Bash call ran was
    folded into the RUNNING turn right after the tool result (one result,
    `user_message_uuids` listing both). Documented in `sdk.d.ts`: "queued user
    message folded into the running turn between tool rounds". So the shim
    delivers `after_tool_call` by pushing the prompt into its input stream
    with NO interrupt; the `PostToolUse` fallback is not needed. The daemon
    binds delivery by `user_message_uuids`.
    CONTEXT FOR ME: the owner's interrupts in this session were additions,
    never rejections; the vendor's "tool use was rejected" text on an
    interjection is exactly what this item removes.
5. Docs: `AGENTS.md` footer section matches what is built (four tiers, shared
   salient kinds, 80% rule, submitting stages); prompt-queue act handling;
   one line per landed fix in `docs/REMEDIATION-CHANGELOG.md` (combined model,
   compaction-failed clearing, cold-gate flash, composer height, toggle style,
   item centering, expanded ceiling, footer percent gradient, liveness fix,
   held-queue fix).
6. Green everywhere, CHECK EXIT CODES (`make test` hides a gofmt failure
   behind a 2-line output): daemon `make test`, `make integration`; shim
   typecheck/lint/test/test:integration (AGENT_REPL_FORBID_VENDOR_CALLS=1,
   via bin/background.sh); webapp typecheck/lint/test/test:integration/
   test:webkit; proto `make validate`; the touched e2e scenarios.
7. Consolidation sweep over `git diff master..HEAD`; extractions as their own
   behavior-preserving commits with tests.
8. LAND THROUGH THE MERGE QUEUE (owner, 2026-09-30, supersedes the
   cherry-pick plan): the daemon's merge-queue handling is fixed, so the
   finished branch merges via the merge-queue skill (`/merge-queue`,
   `.claude/skills/merge-queue/SKILL.md`), NOT by cherry-pick. Run the final
   consolidation sweep first. On a parked, failed or refused merge, report
   the reason and stop.
9. Bounce all systems: `bin/build-frontend.sh` + restart claude-repld; build
   shim-store and shim-claude-sidecar into `~/.cache/agent-repl/bin` and
   `launchctl kickstart`; verify `bin/readiness-report.sh` and
   `scripts/agent-shim-doctor.sh`; hot-load touched `.el` (lisp/panels.el) into
   the main Emacs (never test-*.el).

## Leftovers to mention in the final report (out of scope)

- `/tmp` CEE worktree directory left behind; CEE `test-skill.sh` 14 failures.
- Shim latent late-resume bug (a late delivery can clear a newer wait).
- Light theme: the compaction toggle is now white (#ffffff), invisible on a
  white background.
