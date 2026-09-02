# STOPPING POINT — EMACS (elisp) overhaul (2026-09-02, project pause)

Branch `overhaul/elisp`; STOP tip is the commit that adds this file (its parent
05c49d574 is the last code/ledger commit). Zero agent worktrees under
~/.config/doom-overhaul/elisp-agents; no agents running; nothing queued for
dispatch (parked per the user's pause directive). The live ledger is
docs/overhaul/elisp-fanout.md §0c (every ruling of this session is recorded
there in dispatch order); §17 carries the dead-code lists; the audit records are
reports/elisp-suite-audit-{1,2,3}.md.

## Board at 05c49d574 (2026-09-02 00:47, all standalone runs)

- connect 32/32, link 44/44, host 90/90, roster 71/71, daemon 30/30,
  verbs 99/99, composer 74/74 — every integration suite green standalone.
- All-suite `lisp/test-agent-repl.el`: 3517/3518. ONE SURVIVING RED, order/
  load-dependent only: `agent-repl-itest-link-bounce-indicator-clears-once-
  reconnected` (test-integration-link.el:1193; audit-3 #10) timed out
  "waiting for 1 subscriber(s) on the daemon stream" after 132 s in the
  all-suite order; green standalone. First candidate cause: the bounce
  reconnect's quiet window plus a prior scenario's daemon.addr state in the
  shared fixture; second: the same `--await-subscribers` before acceptance
  shape R-STABILITY fixed elsewhere. Owner at resume: a small opus-low brief.
- test-helpers load of config.el: zero load errors. Fake daemon
  (lisp/testsupport/fakedaemon): go build + go test ok, offline.
- Landing 6 merged (93e071ef0) and adapted (command_acted,
  duplicate_submission, turn_already_open retired, unknown_repository).

## What this session landed (3d3ea3a2c → 05c49d574, 153 commits)

R-VERBS-SUITE, R-HANDOVER, R-ROSTER, R-COMPOSER, R-POLISH (the STOP queue);
R-STABILITY (+R-LOGFORGET); R-DEADCODE (opus-low — DEVIATION from the later
amended rule) and R-DEADCODE-2 (sonnet-medium); audit 2 → R-SUITE-2 +
R-AUDIT2-PROD; R-REGRESS (per-workspace log link repaired on the reuse path);
R-LANDING6; audit 3 → R-SUITE-3 + R-AUDIT3-PROD (+ panel-selection origin) +
R-CLOSE + R-RED-3A/3B. Production defects found by the audits and fixed:
roster re-subscribe on promotion; SubmitPrompt handover arms held and
replayed under the same key; hook containment; dead-conn drain refusal; rename
re-keys host state and the standing stream follows the rename; input buffer
named at creation; explicit-text sends keep the composer draft; reopened rows
revive tombstones; restart sequencing; cold-start provenance log; fault kind
rendered; log-link repair; `:project-dir` on roster-opened tabs; magit GitHub
URL builder.

## Rulings made by the teamlead this session (all in §0c; review at resume)

- Hook containment: a consumer's error inside link up/down/handover/promote
  runs is logged ERROR and the rest still run (never swallowed silently).
- Promote hook `agent-repl-link-promote-functions` (OLD NEW); roster and the
  prompt-queue drain register on it.
- Connect finding: a server-streaming REFUSAL arrives as HTTP 200 + error end
  frame; ON-OPEN legitimately fires once before it; "subscribed" is keyed on
  acceptance AND the close outcome.
- `agent-repl-install-commit-emoji-hook` stays (interactive provisioning of a
  blessed feature; no install.sh exists) — the 2026-08-29 removal targets the
  AUTO-installer.
- Frontend gui send/interrupt plumbing (leaf in deleted frontend-client.el)
  deleted; the composer is host-native; Emacs calls no interrupt rpc.
- Panels region-selection send uses `:user-sent` (no panel-selection
  PromptOrigin exists; existing UX kept).
- Audit-3 #51: only a composer-sourced send erases the composer.
- Audit-3 #30: daemon-link's guard wins — a `transferring_away` naming an
  address other than the standing successor is logged ERROR, not dialed.
- Audit-3 #41: the finish-edge banner passes ACTIVATE nil (the backend's
  workspace-activation default).
- `not_yet_adopted` retry paced by `agent-repl-host-handover-retry-delay`
  (implementation-detail override of "retry on acceptance"; the acceptance
  path stays for the no-successor case).
- The webview buffer name stays a lookup key; naming.title renames the INPUT
  buffer.

## Toss-ups for the user (project lead relays)

- #51: should an explicit-text command send (update-pr, explain, rebase)
  leave the composer draft alone (ruled yes) or clear it as before?
- Should a PromptOrigin exist for the panels region-selection send (today
  `:user-sent`)? Proto change if yes.
- Should the daemon-side workspace rename be surfaced to the user at all
  (today: tab and buffers rename silently)?
- Interrupt placement / InterruptAllAgents (RESUME-MASTER's item; Emacs
  calls no interrupt rpc today).
- The commit-emoji hook installer: keep as an interactive command (ruled) or
  move provisioning into an install.sh that does not exist yet?

## Follow-ups recorded (not dispatched; resume queue in order)

1. The surviving all-suite red above (link bounce indicator, order-dependent).
2. test-roster's `--with-editor` stub writes `:project-dir` itself and masked a
   gap; decide whether the stub should stop (touches every test in the file).
3. Cosmetic: test-integration-composer.el:28 `declare-function
   agent-repl--meta-wrap "prompts"` should name "agent-repl-core".
4. A suite-wide `agent-repl--prompt-queue` cleanup helper (cross-test residue).
5. Adversarial audit 4 (fable, fresh context) → loop per TEAMLEAD.md, then a
   sonnet-medium dead-code rescan over everything landed since R-DEADCODE-2.
6. The create-or-update-workspace skill's `status` verb lost its
   workspace-status.json source (outside the wave).

## Standing facts

Tabs = closed=false rows in roster walk order; finish edge running→settled;
composer on none/terminal/unknown sends; foreign daemon adopted, never killed;
drain indicator = global-mode-string segment; HostWorkspaceTransferred is
empty — the successor address is always WatchDaemon's announcement; the
webview reloads at the successor and self-adopts; Ctrl-B detach shows and
refuses honestly. AGENT_REPL_FORBID_VENDOR_CALLS=1 on every test invocation;
no real git in tests; nothing deployed, hot-loaded or pushed.
