# Elisp overhaul — fanout spec (teamlead-authored, binding for every elisp agent)

This is the shared contract the parallel implementation agents code against.
Function names and representations below are FIXED so agents in separate
worktrees agree without seeing each other's code. Behavior comes from the
protos (authoritative) and docs/overhaul/elisp.md; this file only pins names,
shapes, seams and ownership. When a name here conflicts with a proto comment,
the proto wins and the agent reports the conflict.

## 0. Ground rules (every agent)

- Work only in your assigned worktree; verify `git rev-parse --show-toplevel`
  and `git branch --show-current` first. Never touch ~/.config/doom, never
  hot-load into any running Emacs. Batch tests only.
- Every test process exports `AGENT_REPL_FORBID_VENDOR_CALLS=1`. No real
  vendor call ever; `prompt-summary.el` (a `claude -p` exec site) refuses
  under that variable.
- Logging: every logical branch of production code logs through core.el's
  canonical API: `agent-repl--log` (debug), `agent-repl--info`,
  `agent-repl--warn` (WARNING), `agent-repl--error` (ERROR; a PURE LOGGER
  that never signals — ruled at the pre-pass). REFUSALS that must abort use
  `agent-repl--fatal` (record at ERROR, then signal — the pre-existing
  behavior, renamed) or `user-error` for interactive refusals; never swallow
  a signal. Operation names: `elisp.<module>.<operation>`.
  Dynamic values go in the context, never only in the message.
- Validation invariant: a push or response missing a non-optional field, an
  unset oneof, a oneof with two arms set, or an unknown field/arm is a
  contract breach: signal `agent-repl-wire-error` and log ERROR. Requests are
  built only from complete values; an incomplete request errors before send.
- Tests: ERT, one test file per source module (`lisp/test-<module>.el`),
  table-driven, AAA, ONE edge case per test. Old tests are not truth.
- Commit atomically on your branch; tests ride with the change they cover.
- Surface (do not improvise) any UX/API gap you hit; the teamlead rules or
  escalates.
- Proto→code mapping: one base decode/encode function per message with
  validation once; one dedicated function per non-primitive use site
  delegating to the child's base; primitives get no wrappers.

## 0b. Dispatch policy (user ruling, binding from 2026-08-29 evening)

- Every NEW implementation dispatch is Opus at LOW effort (`opus-low`; if
  the type is not offered, `subagent_type: "claude"` with `model: "opus"`
  and the effort stated in the brief). Agents already running or resumed
  keep their tier.
- Implementers MAY offload mechanical, fully specified writes (boilerplate,
  tests from a settled table, rote conversions, doc sections) to Sonnet at
  MEDIUM effort (`sonnet-medium`; both types are offered in this session). The
  offloading agent stays accountable: it reviews the output, runs the
  suites, and reports every offload in its completion report.
- Adversarial auditors stay `claude` + `model: "fable"`, fresh context.

## 0c. Teamlead live ledger (update at every dispatch / merge / ruling)

COMPACTION RULE (user ruling): if the teamlead's context is compacted, it
drops to LOW effort at once (`/effort low` if offered, else explicitly:
no re-derivation, no exploration; act on this ledger + the docs + the
summary) and says "compacted" in its next message to the project lead.

STATE as of 2026-08-29 evening (tip after 6d97fe768):
- Merged and green at the tip: connect/rpc; wire-common/host/roster/verbs;
  pre-pass; W2-A (daemon-link, host, daemon, services, notifications);
  W2-B (roster, status, popup, workspace, session, frontend,
  webview-recovery, open-progress, panels, window); integration suite +
  Go fake daemon (lisp/testsupport/fakedaemon). Landings 1–3 merged.
- W2-C MERGED (74b1445df): composer + verbs + worktree slimming +
  doctor; every unit suite green. RUNNING (opus-medium tier): remediation-1
  (R-ACCEPT, R-QUESTION, R-STREAMCLOSE) in elisp-agents/remed1;
  R-DAEMON in elisp-agents/remed2. Resume by SendMessage to the existing
  agent, never re-dispatch.
- PRE-CUT, unassigned: elisp-agents/suite2 (overhaul/elisp-suite2) for
  R-SUITE-1 (the 92 audit findings, docs/overhaul/reports/
  elisp-suite-audit-1.md) — dispatch as `opus-low` when a slot frees.
- LANDING 4 merged (41d7c3321): typed `<Rpc>Error` arms on every rpc
  Emacs calls (cross-cutting unknown_workspace / workspace_ref_mismatch
  {registry_dir} / transferring_away{address} / not_yet_adopted + per-rpc
  arms) and HostFault.kind's eight arms. The codec refuses them until
  R-ARMS lands (test-wire-verbs 109/111 by design).
- R-DAEMON MERGED (e5fdae538): cold-start suite 12/12. Findings: the
  integration log reader could not find warn/error records because
  core.el derives `operation` from the severity-prefixed format string
  (R-LOGOP, assigned to the R-ARMS agent, fixes core.el at the source);
  cold-start boundary functions are restored per scenario by the harness;
  daemon.el now logs own-/foreign-adopted and releases its in-flight flag
  on a signal. AGENTS.md owes a line on restoring a boundary in
  integration tests (R-SUITE-1 carries it). R-PUSHINVALID may be moot
  after the reader fix — re-run host/roster before dispatching it.
- R-ARMS + R-LOGOP MERGED (b00b2e58b): every `<Rpc>Error` arm set,
  HostFault/SessionFault/DaemonFault kinds decoded and pinned; core.el
  derives `operation` from the bare format string. Treatments still owed
  (R-HANDOVER: transferring_away/not_yet_adopted in host.el).
- R-VERBS MERGED (1f433f54d): integration-verbs 30/30 (oneof shapes built
  at the verb boundary; verbs fall back to `agent-repl-link-connect`).
  REVERSED deviation in flight (same agent, remed5): verb records must
  stay WORKSPACE-owned per logging-contract.md; the harness reader now
  searches the workspace sinks too; link teardown added to the fixture.
- R-VERBS follow-up MERGED (33b3d2e81): workspace-owned verb records
  restored; harness reader searches global + workspace sinks; fixture
  tears the link down. integration-verbs 30/30, daemon 12/12.
- LANDING 5 merged (d597d9970): UpdateMergeQueuePause/Resume gain
  `optional repository` (unset = every repository); the encoder field and
  verbs.el's pause/resume argument are R-NOTIFY's added scope (own
  commits); R-SUITE-1 extends finding #79 to both scopes.
- R-SUITE-1 MERGED (336ace8cd): all 92 findings pinned; composer fixture
  fixed (empty arm `[]`→`{}`); fake gains raw-body recording + /_fake/gate;
  verbs suite repaired to landed signatures. Known-red pins awaiting
  remediations: #11 (daemon-link drain cause text ignores DrainReason) and
  #52 (banner must read "Agent ready: <name>") — BOTH ADDED to R-HANDOVER's
  brief; #79-scoped awaits R-NOTIFY's encoder; host 20/22/25/33/36 await
  remediation-1.
- WIND-DOWN (user directive): nothing new dispatches or is queued for
  launch. R-HANDOVER, the second adversarial audit, and the verbs-suite
  triage are CANCELLED FOR THE PAUSE — written entries only, re-decided at
  resume. Running implementers (remediation-1, R-NOTIFY) finish naturally
  and merge; their two worktrees are the only ones left.
- VERBS-SUITE REGRESSION diagnosed (post-R-SUITE-1 merge, 29/61): the
  suite's repaired calls pass ready-made oneof plists to verbs that take
  BARE keywords and wrap internally (create refuses "unknown creation
  form (:arm :standard ...)"; set-priority double-wraps). Fix on resume:
  re-repair the suite's verb calls to the landed signatures (bare form
  arm + keyword facts; bare level keyword; flat actions), then triage the
  21 timeout-class failures. Composer 37/42 (5 = input.el production
  gaps: image blocks, posthooks, defer path, #66 key reuse); daemon 14/15
  (#90 default command/no-argv).
- RUNNING: R-NOTIFY (`opus-low`, remed4);
  remediation-1 (opus-medium, remed1). NEXT: R-HANDOVER after
  remediation-1 merges (cut its worktree off that tip).
- VOCAB SEAM CLOSED: integration carries the trimmed vocabulary +
  footer_allowance verbatim (f1132d3a7); merged, resolved to theirs; the
  files are identical on both branches. HostWorkspaceTransferred stays
  EMPTY by design — the successor address is always WatchDaemon's
  announcement; the webview reload uses that address.
- RE-RUN after the reader fix: host 24/32, roster 19/20, link 14/18,
  connect 12/16, daemon 12/12.
- FIRST RUN composer 2/21 (17 on the suite's malformed host-push literal —
  R-SUITE-1 fixes the fixture), verbs 10/30. R-VERBS (queued, worktree
  elisp-agents/remed5): verbs.el hands the codec BARE keywords where §2
  requires oneof plists — e.g. priority `:p05` must be `(:arm :p05 :value
  nil)`; the same applies to create's form/finish/priority arms,
  shutdown's action/reason arms, merge-queue's action arm. Fix verbs.el
  (and the suite where it passes bare keywords), keep the codec as is;
  also the close-success tab teardown and the transport-failure /
  merge-refused log assertions. R-PUSHINVALID is RETIRED (the reader was
  the cause); its residue, if any, folds into R-NOTIFY.
- QUEUE, in order, one slot each, all `opus-low`: R-ARMS (codec: every
  new error arm on the rpcs Emacs calls + HostFault.kind, decoded per §2,
  arm sets pinned against the regenerated Go bindings; unit tests only —
  treatments live in host.el (remediation-1) and verbs.el (W2-C)) →
  R-SUITE-1 →
  R-PUSHINVALID → R-NOTIFY (incl. R-CLICK, gate `:unknown`) → R-HANDOVER
  (re-run link/host first; remediate the remainder; MUST cover the webview
  redial after adoption per §7 HANDOVER REDIAL) → second adversarial
  audit (fable, fresh context) → loop until green.
- PER-MERGE ROUTINE: `git merge overhaul/elisp-<slug>` into overhaul/elisp
  (worktree /Users/dodgecoates/.config/doom-overhaul/elisp); resolve
  config.el/core.el/test-agent-repl.el seams keeping both sides; verify
  `load-errors=nil` and the touched suites; `git worktree remove
  elisp-agents/<slug>` + `git branch -d`; re-run the integration suites
  (`emacs -batch -Q -l ert -l lisp/test-integration-<m>.el
  -f ert-run-tests-batch-and-exit`, AGENT_REPL_FORBID_VENDOR_CALLS=1,
  collect every failure); update this ledger.
- CAP: at most three running agents (auditors count).
- FINAL REPORT owes: commit range, every suite + result, overrides of
  prescribed details, escalations outstanding, what was left out, the UX
  gaps filled from the API (one line each), the toss-ups for the user.
- RESUMED 2026-09-01 (recreated teamlead, fable-low): worktrees cut off
  3d3ea3a2c and dispatched (`opus-low`, cap three): R-VERBS-SUITE
  (elisp-agents/verbs-suite), R-HANDOVER (elisp-agents/handover; the
  successor address is always WatchDaemon's announcement — Transferred is
  empty on the wire), R-ROSTER (elisp-agents/roster). Queued behind them:
  R-COMPOSER, R-POLISH, then adversarial audit 2.
- R-HANDOVER MERGED (d482b3648): link 26/26; host transferred family
  green (host 48/51; #38 closed as fall-out — the webview mount called a
  deleted mode function); bounce indicator names the DrainReason (#11);
  transferred adopts the recorded successor unconditionally. Harness fix:
  `agent-repl-itest--start-daemon` waits for daemon.addr to CHANGE, not
  merely exist (a second daemon in one state root used to return the
  incumbent's address). R-POLISH dispatched (elisp-agents/polish, off
  d482b3648): daemon #90, host #33/#30.
- R-ROSTER MERGED (0a8286ca1): roster 47/47. Production: session.el banner
  "Agent ready: <name>" (#52); core.el `agent-repl--current-ws-p` no longer
  signals on a nil current name. Suite: the roster fixtures copied before
  deletion (a shared literal was being mutated), reactions restore the one
  consumer they test (R-NOTIFY pattern confirmed adequate), the harness
  notifier now uses production's `agent-repl-notify-make-fake-backend`
  (the hand-rolled one had drifted in arity). Follow-up noted: sweep
  test-integration-helpers.el for other hand-rolled duplicates of
  production seams. R-COMPOSER dispatched (elisp-agents/composer, off
  0a8286ca1). Running: verbs-suite, polish, composer.
- R-POLISH MERGED (f68700841): daemon 15/15, host 51/51; no production
  change — all three reds were stale suite payloads relative to the
  landing-4 codec (unset RegisterWorkspaceError.cause, HostFault without
  kind, module root read from load-file-name inside an ERT body). Lesson
  for the remaining board: check payloads against the codec before
  dispatching production work. Running: verbs-suite, composer.
- R-COMPOSER MERGED (ceaf5f716): composer 42/42. Production (#66 only):
  `agent-repl--input-submit` accepts an existing idempotency key; the
  outage queue stores the failed attempt's key and the drain resends under
  it (deferrals mint fresh). Image blocks, posthooks and the defer path
  were already correct — four fixture bugs fixed. Follow-up noted: a
  suite-wide `agent-repl--prompt-queue` cleanup helper (one entry of
  cross-test residue). Running: verbs-suite only.
- NEW STANDING RULE (user ruling, TEAMLEAD.md on integration @ 7deb982cc,
  "Dead code is hunted programmatically"): once the seven suites are green
  and before the final report, an `opus-low` R-DEADCODE pass finds dead
  code with tools (byte-compile with unused-lexical warnings as errors;
  elisp-refs/grep for defuns with no caller outside their file and no
  test). Every uncalled production defun is deleted or named in the final
  report with its live reason (interactive, hook target, autoload) and
  pinned by a test; any ruled-dead file (elisp.md 2026-08-29 list) still on
  disk at the final tip is a defect. Audit-2 triage applies R-POLISH's
  lesson: check a red's payload against the codec before calling it a
  production gap. Queue after R-VERBS-SUITE: R-DEADCODE and audit 2 in
  parallel, then the loop.
- R-VERBS-SUITE MERGED (0677708d1): verbs 61/61 (21 call-convention
  repairs, 5 log-history orphaning reads fixed in the fixture, 2 kill/nuke
  teardown assertions corrected to liveness, 2 stale payloads, 1 health
  buffer order dependence, 1 successor-address wait — same fix as
  R-HANDOVER, conflict resolved keeping HEAD). Production: create's
  standard form gained merge_actions (fanout §5; the encoder already
  carried it). SURFACED for the teamlead: workspace.el's teardown forgets
  the workspace's durable log target and keeps logging, re-pointing the
  canonical <ws>/.claude/emacs/emacs.log link and detaching the
  pre-teardown history → R-LOGFORGET (defer the forget until the
  teardown's own records are written). No agent worktrees remain.
- FULL RUN at 0677708d1: connect 21, host 51, roster 47, daemon 15,
  verbs 61, composer 42 all green; link 25/26 standalone (test 9 asserts
  link-up before acceptance) and a different link test times out only in
  the all-suite run (host.el's link-up hook never fires after other suites
  — leaked state); unit 3303/3304 (that same itest). Dispatched
  (`opus-low`, off db67a1467): R-STABILITY (elisp-agents/stability: the
  two link flakes + R-LOGFORGET), R-DEADCODE (elisp-agents/deadcode: the
  programmatic dead-code rule); audit 2 (fable, fresh context, read-only
  against the teamlead worktree) runs alongside.
- AUDIT 2 DONE (reports/elisp-suite-audit-2.md, 46 findings, all
  accepted). Production defects it exposed: #1 roster does not re-subscribe
  after promotion (live defect: tabs stop reconciling after a blue-green
  rollout); #32 input.el treats transferring_away / not_yet_adopted on
  SubmitPrompt as unknown arms — RULED: route to
  `agent-repl-host-handle-refusal` exactly as verbs.el does; #26 verify the
  fork-without-parent guard. R-SUITE-2 dispatched (`opus-low`,
  elisp-agents/suite2 off f193fb0e8; writes tests, never runs the suite).
  R-AUDIT2-PROD (the three defects) queued behind R-STABILITY + R-DEADCODE.
  Running: stability, deadcode, suite2.
- DEAD-CODE RULE AMENDED (user ruling, TEAMLEAD.md on integration @
  25bf69341): the pass is orchestrated, never performed by the teamlead,
  and dispatches to `sonnet-medium`. R-DEADCODE was already running as
  `opus-low` when the amendment arrived; it finishes as is (no churn) — a
  DEVIATION to carry into the final report. Any follow-up dead-code pass
  (e.g. the candidates deferred from the R-STABILITY files) goes to
  `sonnet-medium`.
- R-DEADCODE MERGED (c74bf7534; ran as `opus-low` — deviation from the
  amended rule, noted). Deleted 13 uncalled defuns (wire-common int64/
  uint32/bool encoders, wire-verbs encode-empty, roster row-dir/
  unsubscribe, three frontend helpers, panels non-agent-panel-window-p,
  history make-instantiation-from-plist, session effective-model/
  refresh-magit-status) + 15 orphaned tests; kept-with-reason list pinned
  (magit github commands, commit-emoji hook installer, prompt-summary
  attach-all, runtime-eval entry points, interactive commands). Whole-
  module byte-compile clean for unused lexicals. Ruled-dead files: none on
  disk. TEAMLEAD RULINGS: (a) `agent-repl-install-commit-emoji-hook`
  stays — the 2026-08-29 removal targets the AUTO-installer at load;
  an interactive, autoloaded install command is the blessed
  "commit-emoji + hook" feature's provisioning path (no install.sh exists
  in the module); (b) frontend.el's `:send-fn`/`:interrupt-fn` registry
  points at functions of the deleted frontend-client.el — the dispatchers
  and registry entries are dead (the composer is host-native; Emacs
  calls no interrupt rpc) → delete in R-DEADCODE-2; (c) magit GitHub URL
  builder bug (ssh prefix regexp eats the colon, no slash) → fix with
  tests in R-DEADCODE-2. R-DEADCODE-2 (`sonnet-medium`, after
  R-STABILITY merges): the deferred core.el/workspace.el/host.el
  candidates (definitely-dead: `--reset-warn-once-state`,
  `--ws-advise-kill-before`, `--ws-materialize-daemon-workspace`,
  `--ws-registered-dir-owner`, `--ws-repo-folded-p`; test-only helpers
  reviewed one by one), (b), (c), and the stale explain-config comment
  mentions in frontend.el.
- R-STABILITY MERGED (928f4d744): both link flakes root-caused (fixtures
  awaited the daemon's subscriber registry, a hop before ON-OPEN; the
  composer suite leaked a prompt-queue entry + a host entry on a dead
  conn, whose drain signalled out of `agent-repl-link-up-functions` and
  cancelled every consumer behind it); R-LOGFORGET landed (forget the log
  target as the LAST act of `agent-repl--ws-del`, 3 unit tests).
  TEAMLEAD RULING on the surfaced production question: a consumer
  signalling inside `agent-repl-link-up-functions` (and the down/handover/
  finish hook runs alike) must not cancel the consumers behind it — the
  runner contains each consumer's error, logs it at ERROR
  (`elisp.link.up-hook-failed`, consumer + error in context; never
  swallowed silently), and continues; additionally the outage drain skips
  a dead conn and re-queues. → R-AUDIT2-PROD. Dispatched off 928f4d744:
  R-AUDIT2-PROD (`opus-low`, elisp-agents/audit2-prod: audit-2 #1, #32,
  #26 + the hook-containment ruling) and R-DEADCODE-2 (`sonnet-medium`,
  elisp-agents/deadcode2). Running: suite2, audit2-prod, deadcode2.
- SLOW-DOWN DIRECTIVE (user, 2026-09-01 evening): usage limits near. No
  new agent of any kind dispatches until the project lead's explicit
  resume. Already running and allowed to finish + merge: R-SUITE-2
  (elisp-agents/suite2), R-AUDIT2-PROD (elisp-agents/audit2-prod),
  R-DEADCODE-2 (elisp-agents/deadcode2). RESUME QUEUE after those merge:
  (1) run the seven integration suites + all-suite at the merged tip and
  remediate R-SUITE-2's expected-red tests that R-AUDIT2-PROD did not
  close; (2) adversarial audit 3 (fable, fresh context) → loop per
  TEAMLEAD.md until no critiques; (3) final report with the dead-code
  deletion / kept-with-reason lists, rulings, deviations (R-DEADCODE ran
  opus-low), UX gaps filled, toss-ups for the user.
- RESUMED (user go signal). R-AUDIT2-PROD MERGED + R-SUITE-2 MERGED
  (b4cb2bc0e): promote hook + roster re-subscribe (#1), SubmitPrompt
  handover arms route to the host (#32), fork-without-parent refused
  (#26), hook containment + dead-conn drain refusal; all 46 audit-2
  findings pinned, fake records request headers. R-SUITE-2 surfaced three
  more production defects (see its report; follow-up R-AUDIT2-PROD-2).
  R-DEADCODE-2 resumed by message (five commits landed). Worktrees left:
  deadcode2 only.
- RUN at ed395b804: connect 25/25, link 31/31, host 62/68, roster 55/56,
  daemon 20/21, verbs 72/73, composer 52/54; all-suite 3395/3406 (the same
  11, no order dependence). Dispatched (`opus-low`, off ed395b804):
  R-RED-HOST (elisp-agents/red-host: the six host reds — not_yet_adopted
  retry, webview redial at the successor, naming.title renames the input
  buffer, select refusal does not record the selection, transferring_away
  redial via Select, transferring_away without address is a breach) and
  R-RED-MISC (elisp-agents/red-misc: roster tab carries the row's own
  ref; daemon restart-then-ensure; verbs fork-without-parent refuses
  before send; composer merge_parked badge + merging-refusal mode-line
  flash). Triage rule: payload against the codec first. Running:
  deadcode2, red-host, red-misc.
- R-DEADCODE-2 MERGED (567bcf8fd; `sonnet-medium` per the amended rule):
  rulings (b) dead gui send/interrupt frontend plumbing deleted, (c) magit
  GitHub URL builder fixed (ssh remote slash) with table-driven tests;
  prompts.el deleted (file-backed prompt loader had no caller left) with
  its test and load line; uncalled core.el/workspace.el/host.el helpers
  deleted; `agent-repl-host-forget` kept — R-SUITE-2's composer/host
  suites call it for teardown. Whole-suite green, byte-compile clean, no
  removal-list files present. Full lists in the agent's report (carried
  into the final report). Worktrees left: red-host, red-misc.
- REGRESSION at 567bcf8fd (all-suite 3302/3318): five NEW reds beyond the
  eleven assigned — composer no-session-refusal / turn-already-open (log
  `elisp.input.unknown-error-arm` not found), submit-log-carries-the-origin,
  unknown-gate-sends-and-logs-info, verbs close-blocked-messages (the
  `message` capture received a log line instead of the user message). All
  are log-routing symptoms first seen after R-DEADCODE-2's core.el
  deletions merged onto R-SUITE-2's tests (that agent branched before
  suite2 landed). R-REGRESS dispatched (`opus-low`, elisp-agents/regress
  off eb2d55238). Running: red-host, red-misc, regress.
- R-RED-MISC MERGED: roster 56/56, daemon 21/21, verbs 73/73, composer
  54/54. Production: roster `--open-tab` writes `:project-dir` from the
  ref's dir unconditionally; `agent-repl-frontend-daemon-restart` sequences
  stop ack → teardown → await daemon.addr removal → ensure (it used to
  re-adopt the departing daemon). Suite: `format-mode-line` renders empty
  in batch — the notice helper reads the `:eval` segment function; fork
  guard signals `user-error` (verbs-layer pre-send guard per §9). Surfaced:
  composer suite has a load-correlated `--await-log` flake (the same three
  tests R-REGRESS holds — likely one cause); test-roster's `--with-editor`
  stub writes `:project-dir` itself and masked the gap (teamlead: leave the
  stub; the new test overrides it locally — noted as a follow-up).
- R-RED-HOST MERGED (1deaa97a3): host 68/68. Production: last-selected-id
  stamped on Select success only; Select/Adopt error arms route
  transferring_away / not_yet_adopted through `agent-repl-host--on-refused`
  → `agent-repl-host-handle-refusal`; empty transferring_away address is
  the breach; `not_yet_adopted` retry paced by
  `agent-repl-host-handover-retry-delay` (0.2 s defcustom) — accepted as an
  implementation-detail override of §7's "retry on acceptance" (the
  acceptance path stays for the no-successor case); `naming.title` renames
  the INPUT buffer (`agent-repl--input-buffer-name`, identity segment +
  title); the webview buffer name stays a lookup key (ruled: correct — the
  contract asks for a name in every standing, not for every buffer to
  carry it). Worktrees left: regress.
- RUN at d8a074c49: connect 25, link 31, host 68, roster 56, daemon 21,
  verbs 73 all green; composer 52/54 standalone (submit-log-carries-the-
  origin, turn-already-open-refusal-names-its-arm — the load-correlated
  `--await-log` family R-REGRESS holds) but 54/54 inside the all-suite run
  (3348/3348 green). Only R-REGRESS in flight.
- LANDING 6 MERGED (93e071ef0, from overhaul/integration: protos
  d46e601e7, bindings 8a98e4fca, docs bc0a07bae). Relay (elisp.md
  "Landing 6 relay"): SubmitPromptSuccess.command_acted = third non-turn
  success arm (composer clears, INFO `elisp.input.command-answered`);
  SubmitPromptError.duplicate_submission keeps the text and says the key
  was already accepted; SubmitPromptError.turn_already_open RETIRED (drop
  from the decoder's arm table; the composer suite's turn-already-open
  test retargets to another declared arm); UpdateMergeQueueError.
  unknown_repository on pause/resume/evict → WARN + message naming the
  repository. R-LANDING6 dispatched (`opus-low`, elisp-agents/landing6).
  Running: regress, landing6.
- PAUSE DIRECTIVE (user, 2026-09-01 late): the project pauses once every
  lead has resolved. Elisp finishes its recorded queue — merge R-REGRESS
  and R-LANDING6, full run, adversarial audit 3 (fable) + remediation of
  real findings, final report with the §17 dead-code lists — then parks:
  no dispatch after the report; STOP-elisp.md rewritten with the
  resumption state; the teamlead stays resident.
- AUDIT 3 dispatched (fable, fresh context, read-only at 44476838d; told
  the landing-6 adaptations and the composer `--await-log` flake are in
  flight). Running: regress, landing6, audit 3 (cap).
- R-REGRESS MERGED: the "regression" was load-correlated, not a
  deletion. Production: the per-workspace log link is verified and
  re-installed on the REUSE path too (it used to be installed only at
  mint, so a stolen link sent readers to another file);
  `agent-repl--install-workspace-log-link` shared by mint and repair; four
  test-core pins. Harness: `agent-repl--workspace-log-targets` bound per
  scenario (was the one process-wide registry). Suite: the close-blocked
  test waits for the message under test, not any message. Follow-up:
  `verbs-merge-success-messages-merge-enqueued` has the same racy wait
  predicate — fix at the next remediation touching that suite.
- R-LANDING6 MERGED: command_acted `(:arm :command-acted :value nil)`
  (composer clears), duplicate_submission (text kept, WARN + message +
  flash "already submitted", never queued), turn_already_open dropped
  (unknown arm → wire error; stand-in tests retargeted to
  `:feed-undecodable`), unknown_repository on merge-queue verbs (WARN +
  message naming the requested repository; evict names nil by design).
  No agent worktrees remain. Only audit 3 in flight.

## 1. Module map (final tree of lisp/)

New files:
- `connect.el` — Connect-over-HTTP/1.1 transport (curl subprocess).
- `wire-common.el`, `wire-host.el`, `wire-roster.el`, `wire-verbs.el` — codec.
- `rpc.el` — one function per agentrepl.v1 rpc Emacs calls.
- `daemon-link.el` — connection lifecycle, WatchDaemon, handover, drain.
- `host.el` — Register/Select/WatchHostWorkspace/Adopt, treatments.
- `roster.el` — WatchWorkspaceRoster consumer: tabs, paint, finish edge.
- `verbs.el` — workspace + admin verbs as thin wrappers, health output.
- `popup.el` — the one shared editor-popup subroutine.
- `notes.el` — org notes (extracted from tasks.el).
- `testsupport/fakedaemon/` (Go) + `test-integration-*.el` — the suite.

Deleted by the pre-pass (with their `test-*.el`): rename, codex, backend,
explain-config, readiness, recovery-slo, connection-notice,
hide-project-dirs, memory-state, output-nav, sentinel, install,
external-browser, workspace-status-export, tasks (org notes → notes.el),
open-fence, failure, frontend-uds, frontend-state, frontend-client, sidebar,
permission, transcripts, context, context-cost, ai-title.

Kept and adapted (owner in §15): core, prompts, workspace, frontends,
notifications, history, status, autosave, input, clipboard-image, commands,
session, daemon, prompt-queue, services, frontend, webview-recovery,
prompt-summary, window, sibling-popup, panels, open-progress, worktree,
keybindings, magit, emoji, prevent-select, close-panels-on-open,
interaction-record. `merge-handlers.el` and `workspace-create-client.el`
are deleted by the verbs agent once verbs.el replaces them.

## 2. Wire representation (binding)

- Parse: `(json-parse-string s :object-type 'alist :array-type 'list
  :null-object :null :false-object :false)` → alists keyed by SYMBOLS
  spelled exactly as on the wire (protojson lowerCamel: `atMs`,
  `shimAttached`, `shutdownAnnounced`, `reloadWebapp`, `idleAsync`).
- Serialize: `json-serialize` on alists with symbol keys (lowerCamel);
  `t` / `:false` for bools; integers for int64 (Go accepts numbers);
  vectors for repeated fields (`[]` when empty); an EMPTY MESSAGE is `nil`
  (serializes to `{}`), so an empty oneof arm encodes as `(arm . nil)`.
- Decoding int64: accept an integer OR a decimal string (Go protojson emits
  strings for int64). Optional scalars absent → nil; non-optional scalars
  absent → the proto3 default (protojson omits defaults); non-optional
  MESSAGE fields absent → `agent-repl-wire-error`, except where the proto
  comment states presence is the fact (RosterRowDetail's three lines;
  RosterRowWhen's oneof may be unset).
- Decoded elisp shape: a plist with kebab-case keywords per field
  (`:at-ms`, `:shim-attached`); a oneof decodes to `(:arm KEYWORD :value V)`
  where KEYWORD is the arm's kebab keyword (`:shutdown-announced`,
  `:merge-parked`, `:idle-async`) and V is the decoded arm message (nil for
  an empty arm); repeated → list; optional message absent → nil.
- Unknown keys anywhere → `agent-repl-wire-error` (unknown-field refusal,
  the same strictness generated clients have). Unknown oneof arm → error.
- Naming: `agent-repl-wire-decode-<message-kebab>` /
  `agent-repl-wire-encode-<message-kebab>` (base, validation lives here);
  `agent-repl-wire-decode-<message-kebab>-<field-kebab>` per non-primitive
  use site. Examples: `agent-repl-wire-decode-host-workspace`,
  `agent-repl-wire-decode-host-session-live-composer`,
  `agent-repl-wire-encode-create-workspace-request`.
- `agent-repl-wire-error` is a `define-error` in wire-common.el with data
  `(MESSAGE-NAME FIELD REASON)`.

## 3. connect.el (transport)

- Discovery: `(agent-repl-connect-daemon-addr-file)` = `<state
  dir>/daemon.addr` using core.el's existing state-dir resolver
  (`$AGENT_REPL_STATE_DIR`, default `~/.claude-emacs`);
  `(agent-repl-connect-read-daemon-addr)` → `"127.0.0.1:PORT"` or nil when
  the file is absent (the legal no-daemon state); malformed content signals
  `agent-repl-connect-error`.
- `(agent-repl-connect-open ADDRESS)` → a connection object (cl-defstruct
  `agent-repl-connect-connection`: address, streams, alive-p).
- Unary: `(agent-repl-connect-unary CONN METHOD JSON-STRING &key
  on-response on-failure timeout)` async; `on-response` receives the parsed
  alist; `on-failure` receives an `agent-repl-connect-error` datum. Also
  `(agent-repl-connect-unary-sync CONN METHOD JSON-STRING &optional
  TIMEOUT)` → alist or signal. Default timeout `agent-repl-connect-unary-
  timeout-seconds` (10). METHOD is the bare rpc name ("RegisterWorkspace");
  path `/agentrepl.v1.AgentRepl/<METHOD>`; headers `Content-Type:
  application/json`, `Connect-Protocol-Version: 1`. Any non-200 → parse the
  Connect error body `{code,message}` → failure. Never retried here.
- Streaming: `(agent-repl-connect-stream CONN METHOD JSON-STRING ON-PUSH
  ON-CLOSE)` → stream object (`agent-repl-connect-stream`: process, method,
  conn). Request `Content-Type: application/connect+json`, body = one
  envelope (flag byte 0x00, u32 big-endian length, JSON). Response =
  envelopes; flag 0x02 = EndStreamResponse with JSON `{}` or
  `{"error":{...}}`. No compression negotiated. `ON-PUSH` receives each
  parsed alist. `ON-CLOSE` receives `(:cancelled)`, `(:ended)` (end frame
  without error — for a standing stream the caller treats it as a failure),
  or `(:error DETAIL)` (end frame with error, HTTP failure, or process death
  without an end frame — logged ERROR here). `(agent-repl-connect-stream-
  cancel STREAM)` kills the curl process; that is the graceful close.
- Mechanism: `curl --http1.1 -sS --no-buffer` via `make-process` with a
  filter; envelope parsing is a pure function `(agent-repl-connect-envelope-
  feed PARSER BYTES)` → list of `(FLAGS . PAYLOAD-STRING)` frames, unit-
  tested without processes. HTTP status is read from `-D -` headers or an
  equivalent; the implementer chooses and documents.
- ON-PUSH exceptions are caught at the filter boundary: log ERROR with the
  payload in context; the stream stays open.
- STANDING-STREAM ACCEPTANCE (project-lead contract rule): the daemon
  flushes response headers on accept, so a stream is ACCEPTED the moment its
  HTTP 200 header block arrives — before any frame; connect.el exposes that
  instant (`ON-OPEN`/an accepted flag) and daemon-link/host/roster key
  "subscribed" on it, never on a first frame. A client ends a watch only by
  killing its transport (`agent-repl-connect-stream-cancel`); a standing
  stream never ends on its own, so an end frame or process death on a
  standing stream is always a failure. The fake daemon flushes headers on
  accept too.
- LANDED SHAPES (connect.el as merged): the failure datum handed to
  `:on-failure` and carried in `(:error DETAIL)` is the plist
  `(:kind K :code CODE :status STATUS :message MSG)` with `:kind` one of
  `:http`, `:transport`, `:timeout`, `:malformed`, `:malformed-addr`,
  `:no-end-frame`. `(agent-repl-connect-close CONN)` marks the connection
  dead and cancels every standing stream as `(:cancelled)` — daemon-link's
  teardown primitive. `agent-repl-connect--spawn-curl` is the single spawn
  point, registered in `agent-repl--external-boundary-functions`. The HTTP
  status is read from `curl -D -` header blocks for unary and streams alike.
- LANDED SHAPES (rpc.el as merged): request encoders are called for EMPTY
  request messages too (`agent-repl-wire-encode-watch-daemon-request`,
  `-watch-workspace-roster-request`, `-daemon-health-request` receive nil);
  `agent-repl-rpc-watch-host-workspace` hands its encoder `(:workspace REF)`.

## 4. rpc.el

- `agent-repl-rpc-<method-kebab>`: unary verbs take CONN plus the decoded
  request plist, with `&key on-response on-failure`; `on-response` receives
  the DECODED response plist `(:arm :success :value ...)` /
  `(:arm :error :value ...)`. A `-sync` variant exists for each (tests,
  doctor). Streams: `(agent-repl-rpc-watch-host-workspace CONN REF ON-PUSH
  ON-CLOSE)`, `(agent-repl-rpc-watch-daemon CONN ON-PUSH ON-CLOSE)`,
  `(agent-repl-rpc-watch-workspace-roster CONN ON-PUSH ON-CLOSE)`; ON-PUSH
  receives the decoded push plist. A push that fails decoding is logged
  ERROR (`elisp.rpc.push-invalid`) with the raw JSON in context and dropped;
  the stream continues.
- REF is the decoded WorkspaceRef plist `(:id "..." :dir "...")`, echoed
  verbatim; never constructed from a path.

## 5. Codec scopes

- wire-common.el: WorkspaceRef, RepositoryRef (both directions); UserSaid,
  UserContent, UserContentBlock, TextBlock, ImageBlock(+Path/Url,
  media_type) (ENCODE; Emacs produces, never decodes); PromptOrigin (ENCODE
  as the enum's string name, e.g. `"PROMPT_ORIGIN_USER_SENT"`; the elisp
  value is the kebab keyword `:user-sent`; UNSPECIFIED is refused before
  send); DrainReason + arms (both); WorkspacePriority (encode; decode not
  needed); TurnId (decode); shared helpers (oneof, int64, uint32, optional,
  unknown-key check); `agent-repl-wire-error`.
- wire-host.el: RegisterWorkspace{Request,Response,Success,Error};
  SelectWorkspace{...}; WatchHostWorkspaceRequest, WatchHostWorkspaceResponse
  with the whole HostWorkspace tree (session none|existing; existing.id +
  standing live|terminal; live: generation, shim_attached, vendor_info
  claude, backfill 4 arms, composer 5 arms, faults; naming), Host
  WorkspaceNotification + HostNotificationKind arms, Transferred,
  ReloadWebapp, and the landed arm `open_in_editor {path, optional uint32
  line}` (decoded to `(:path P :line L-or-nil)`); AdoptHostWorkspace{...}; WatchDaemonRequest,
  WatchDaemonResponse (shutdown_announced with cause arms, drain_scheduled,
  drain_cancelled).
- wire-roster.el: WatchWorkspaceRosterRequest/Response and the whole
  frontend.v1 WorkspaceRoster tree: RosterRepositoryView, RosterTaskView,
  RosterRepoSection (RosterRepoKey), RosterTaskSection (RosterTaskKey,
  RosterTaskSectionHeader, RosterTaskDone), RosterMergedSection,
  RosterSectionHeader, RosterLabel, RosterRows, RosterRow (workspace,
  optional attention, optional priority badge, name, the 23 status arms,
  current, children recursive, when with 2 arms or unset, detail with
  presence-optional lines, closed), RosterCurrentWorkspace.
- wire-verbs.el: CreateWorkspace (request with both forms, parent+fork,
  model, priority, allow_ungated, merge_actions, base_ref, name,
  initial_prompt; response); Open/Close(blocked arm)/Kill/Nuke/Merge/
  Restart(force)/SetWorkspacePriority (absent priority = clear); SubmitPrompt
  (request said + idempotency_key + REQUIRED origin + REQUIRED workspace
  (WorkspaceRef, landing 2: every Emacs submit sends it), feed omitted;
  response: success turn {TurnId} | command_panel — decode only the ARM
  KEYWORD and keep the panel payload as the raw alist |
  command_refused{command} (decoded `(:command "/agents")`) | error
  merging);
  UpdateShutdownSchedule (3 arms); UpdateMergeQueue (3 arms);
  DaemonHealth (healthy | unhealthy{faults[{detail}]}); SessionHealth.
- Empty error messages decode to `(:arm :error :value nil)`; a future arm
  is an unknown key → loud error (by design: the teamlead threads new arms).
- LANDED SHAPES (wire-verbs.el as merged): presence-only fields (`fork`,
  `allow_ungated`) are passed as `t` on the elisp side (nil = absent, since
  nil is also the value of a set empty message); `force`, `self_certified`
  and `add_to_merge_queue` are always encoded explicitly, false included;
  an unset one-shot `finish` is refused before send; an unset
  `CloseWorkspaceError.cause` is a decode breach; merge-action fields are
  UserSaid values. REMEDIATION OWED (teamlead loop): wire-verbs.el wraps
  `agent-repl--error` in a `condition-case` that swallows a signal — once
  core.el's non-signaling `agent-repl--error` lands, that wrapper is
  removed (never swallow errors).

## 6. daemon-link.el

- `(agent-repl-link-connect)`: read daemon.addr; nil → run
  `agent-repl-link-no-daemon-functions` (daemon.el hooks cold start) and
  return nil; else open the conn, start WatchDaemon; on the stream's first
  successful open run `agent-repl-link-up-functions` with CONN.
- `(agent-repl-link-primary)`, `(agent-repl-link-successor)`,
  `(agent-repl-link-up-p)`.
- The WatchDaemon stream closing other than by cancel = link down: run
  `agent-repl-link-down-functions` (CONN), then reconnect: poll daemon.addr
  every `agent-repl-link-reconnect-interval-seconds` (1, backing off to 5)
  until a conn opens and WatchDaemon stands; then the up hooks run again
  (host.el re-registers and re-subscribes; roster.el re-subscribes).
- `shutdown_announced` with address: open the successor conn, WatchDaemon on
  it, record it, run `agent-repl-link-handover-functions` (OLD NEW). When
  the old conn's daemon stream later closes after a handover, PROMOTE the
  successor to primary silently (no down/up hooks: workspaces were
  adopted) and run `agent-repl-link-promote-functions` (OLD NEW) — the
  one hook that fires on promotion; roster.el registers on it and
  re-subscribes WatchWorkspaceRoster on NEW (audit-2 #1). Every hook run in
  daemon-link (up/down/handover/promote) goes through
  `agent-repl-link--run-hook`, which contains a consumer's error (logged at
  ERROR `elisp.link.hook-consumer-failed`, never swallowed silently) and
  still runs the consumers behind it. Without address (plain bounce): set the quiet-until instant
  `minted_at_ms + expected_outage_ms` (ms epoch, compared against
  `(* 1000 (float-time))`); the reconnect loop waits until then before
  polling; the indicator reads "daemon restarting (<cause>)".
- `drain_scheduled` / `drain_cancelled`: keep `agent-repl-link-drain` (nil or
  `(:at-ms N :reason PLIST)`), run `agent-repl-link-drain-functions`, and
  draw the standing indicator as a `global-mode-string` segment
  `agent-repl-link-drain-segment`: "drain HH:MM · deploy" / "· maintenance"
  / "· <operator note>". This is the teamlead's choice of indicator.
- daemon-link never issues Register/Select/verbs.

## 7. host.el

- State: `agent-repl-host--by-name` hash WS-NAME → plist `(:ref REF :conn
  CONN :stream STREAM :host HOST-PLIST)`. Accessors:
  `(agent-repl-host-ref WS)`, `(agent-repl-host-conn WS)`,
  `(agent-repl-host-state WS)`, `(agent-repl-host-backfill WS)` → arm
  keyword or nil, `(agent-repl-host-faults WS)`.
- `(agent-repl-host-register CONN DIR ON-DONE)`: RegisterWorkspace; success
  → ON-DONE receives REF; error arm → `agent-repl--error` and ON-DONE nil.
- `(agent-repl-host-select WS)`: SelectWorkspace on the conn owning WS;
  records `agent-repl-host-last-selected-id`. Called from workspace.el's
  perspective-activated hook (`agent-repl--ws-add-activated-hook`).
- `(agent-repl-host-subscribe CONN WS REF)` / `(agent-repl-host-unsubscribe
  WS)`: one WatchHostWorkspace per open workspace; unsubscribe cancels.
- `(agent-repl-host-composer-gate WS)` → `:open` | `:merge-parked` |
  `:merging` | `:draining` | `:restarting` | `:no-session` | `:terminal` |
  `:unknown` (no push yet). Fixed treatments (input.el enforces): `:open`
  send; `:merge-parked` send, with the input mode-line badge "merge parked —
  prompts go to the resolution agent"; `:merging` refuse "composer closed: a
  merge owns this session"; `:draining` refuse "composer closed: daemon
  draining"; `:restarting` refuse "composer closed: restarting";
  `:no-session` / `:terminal` SEND (ruled: SubmitPrompt has no
  precondition; the daemon starts or revives the session implicitly);
  `:unknown` (no host push yet) SEND as well, logging INFO — the daemon is
  the authority and answers with its own refusal arms.
- Naming: tab label = the ROSTER row name (roster.el); buffer titles use
  `naming.title`, else `naming.slug`, else the row name. `(agent-repl-host-
  display-title WS)` exposes it.
- `agent-repl-host-update-functions` (WS HOST-PLIST) runs after every host
  push. shim_attached=false has NO treatment (parked is invisible by design).
- Notification policy, at the arm, per the proto comment: Emacs unfocused
  (`agent-repl--emacs-focused-p` nil) → `agent-repl--notify` with the text;
  its click raises the frame and `agent-repl--ws-switch`es to WS. Focused
  and WS not the selected tab → `(agent-repl-status-blink-tab WS)`. Selected
  → log only. `permission_requested` follows the same policy (the text is
  daemon-composed; tool_name goes into the log context).
- `transferred`: NEW = `(agent-repl-link-successor)`; nil → ERROR log
  (`elisp.host.transferred-without-successor`), keep the stream. Else call
  AdoptHostWorkspace on NEW; success → cancel the old stream, subscribe on
  NEW, update `:conn`; error arm → ERROR log, keep the old stream.
- `reload_webapp` → `(agent-repl-frontend-reload-webview WS)`.
- HANDOVER REDIAL (project-lead ruling): on every successful adoption —
  the `transferred` push and the `transferring_away{address}` refusal
  (`agent-repl-host-handle-refusal WS ARM-PLIST`) — host.el updates the
  workspace's `:conn` to the successor FIRST and then calls
  `(agent-repl-frontend-reload-webview WS)`, so the webview navigates to
  `http://<successor address>/?workspace=<id>&dir=<dir>`. The webapp only
  draws a "moved" notice and stops its streams; it never reconnects
  itself — the host owns the redial. `not_yet_adopted` → INFO, retry the
  adopt once the successor's WatchDaemon is accepted.
- FINAL HANDOVER SEQUENCE (project lead): on `transferred{address}` (the
  push now carries the successor address — decode it; when absent, fall
  back to `agent-repl-link-successor`) and on `transferring_away{address}`:
  (a) AdoptHostWorkspace on a connection to ADDRESS (reuse the link's
  successor when its address matches, else open one and WatchDaemon it);
  (b) reload the workspace's webview to `http://<address>/?workspace=<id>
  &dir=<dir>` — the reloaded page adopts itself (AdoptWebWorkspace at
  boot); Emacs does nothing else for the web side. Roster and daemon-link
  then follow the successor's address as the current daemon (promotion).
- `open_in_editor` (Q2 ruling) → `(agent-repl-popup-open PATH LINE)` — the
  ONE shared subroutine; a directory opens in dired. Log INFO with the path.
- On `agent-repl-link-up-functions`: for every live workspace
  (`agent-repl--live-ws-names`) register its dir, then subscribe. On link
  down: mark streams gone; keep the last host state.

## 8. roster.el and status.el

- `(agent-repl-roster-subscribe CONN)`; `agent-repl-roster-view` holds the
  last decoded roster; `agent-repl-roster-update-functions` (ROSTER).
- LANDED SHAPES (wire-roster.el as merged): the decoded roster keeps the
  contract's nesting — a row's ref is `(plist-get (plist-get ROW :workspace)
  :workspace)`, a repo section's ref is under `:key` then `:repository`, the
  roster's current is `(plist-get (plist-get ROSTER :current) :workspace)`;
  an optional EMPTY message (RosterRowAttention, priority badge presence) decodes
  to `t` when present and nil when absent; `agent-repl-wire-roster-row-status-
  keywords` exports the 23 arm keywords in proto order for status.el's table
  and its assertion tests.
- Tab reconciliation `(agent-repl-roster-reconcile ROSTER)`: walk
  `repository.sections` in order and rows depth-first (row, then its
  children), then `recently_merged.rows`. A row with `closed` false → ensure
  a tab exists (workspace.el creates it with `:ref`, `:dir` = ref.dir,
  `:name`); `closed` true → ensure no tab (teardown is idempotent). Tab
  order = walk order, strictly; no local ordering. The task view is ignored
  (the same rows regrouped). A row whose ref id matches an existing tab is
  the same workspace whatever its name; a rename of the row renames the tab.
- Tab naming: the row's name text; on collision within the roster, append
  "·<repo label>".
- `current`: if `current.workspace.id` differs from the selected tab's ref
  id AND from `agent-repl-host-last-selected-id`, switch to that tab (R8;
  the resulting SelectWorkspace is idempotent, no loop).
- Paint: `(agent-repl-status-tab-state WS)` → the row's status arm keyword.
  `agent-repl-status-color-table` maps every one of the 23 arms to exactly
  one of blue/purple/red/yellow/green/none and is ASSERTED row for row
  against `proto/vocab/render-colors.json` (landed on this branch):
  `roster_status` (arm → color) composed with
  `surface_overrides.emacs_tab_bar` (merge_enqueuing, merge_queued, merging
  → purple; vendor_blocked → blue), `merge_glyphs` (queue / recycle /
  conflict / failed / check → the tab glyph), and `precedence` (blue purple
  red yellow green). Teal and RENDER_STATE_* are gone from the file and
  from elisp. inactive → none with a "?" glyph. Attention present → blink
  once (below) then a steady marker until the marker leaves the row.
  Priority badge label draws before the name. Teal, hibernated, the local
  state machine, poll timers, git ticks, spread and stale thresholds are
  deleted; the ready-shout-then-fade dwell and bracket-only paint stay as
  local modifiers.
- Blink: `(agent-repl-status-blink-tab WS)` implements frontend.v1
  RosterRowAttention's cadence exactly — marker on at 0 ms, off at 500,
  on at 1000, off at 1500, steady on from 2000 — and its docstring cites
  the message. The test asserts the exact timer schedule.
- Finish edge: RUNNING = {submitting thinking clearing compacting
  permission}; SETTLED = {ready done interrupted idle-async}. A row moving
  RUNNING → SETTLED runs `agent-repl-roster-finish-functions` (WS) once.
  Registered reactions: (1) unfocused desktop banner "Agent ready: <name>";
  (2) cross-workspace echo `message` when WS is not the selected tab;
  (3) magit-status refresh for the dir; (4) deferred-prompt drain
  (prompt-queue.el registers this one).
- The render-colors.json assertion test reads the file at test time and
  fails on any row divergence or any arm missing on either side.

## 9. verbs.el

- `(agent-repl-verb-close WS)`, `-kill`, `-nuke`, `-open` (REF of a closed
  row), `-merge`, `(agent-repl-verb-restart WS FORCE)`,
  `(agent-repl-verb-create REPO-REF FORM &rest FACTS)`,
  `(agent-repl-verb-set-priority WS PRIORITY-OR-NIL)`,
  `(agent-repl-verb-shutdown-schedule ACTION)`,
  `(agent-repl-verb-merge-queue ACTION)`. Each resolves REF via
  `agent-repl-host-ref` (nil → `user-error`), CONN via `agent-repl-host-
  conn` (falls back to `agent-repl-link-primary`), sends async, and on the
  ack: success → the editor-state update; error arm → `agent-repl--warn` +
  `message`; transport failure → `agent-repl--error` + `message`.
- Editor-state updates: Close success → tear the tab down (workspace.el;
  idempotent against the roster's reconciliation); Close `blocked` → log
  INFO + `message "close blocked — see the workspace footer"`, no dialog;
  Kill/Nuke success → tear the tab down; Merge success → `message "merge
  enqueued"`, nothing else (Emacs holds no merge state); Restart success →
  `message`; Create success → nothing (the roster push opens the tab).
- Interactive commands and keys: `agent-repl-close-workspace` (SPC j x),
  `agent-repl-kill-workspace`, `agent-repl-nuke-workspace` (y/n confirm:
  data destruction), `agent-repl-open-workspace` (completing-read over
  closed rows), `agent-repl-merge-workspace` (SPC TAB M),
  `agent-repl-restart-workspace` (SPC o C-c; prefix arg = force),
  `agent-repl-create-workspace` (repo from the roster's sections, default
  the current workspace's; prompt, optional name/base_ref; prefix arg =
  child of the current workspace; fork/model/priority via arguments),
  `agent-repl-create-oneshot-workspace` and variants replacing the
  doom/explanation-engine one-shot commands (self_merge, open_pr with
  self_certified/add_to_merge_queue; model from
  `agent-repl-oneshot-model-candidates`), `agent-repl-set-priority`,
  `agent-repl-daemon-shutdown-schedule` / `-cancel` / `-now`,
  `agent-repl-merge-queue-pause` / `-resume` / `-evict`,
  `agent-repl-daemon-health` and `agent-repl-session-health` (render into
  `*agent-repl-health*`: verdict, each fault's detail, plus the host
  stream's standing faults for the workspace).
- worktree.el is slimmed to what has no wire successor and is blessed:
  the eval helpers (`agent-repl--eval-code-string`, reachable via
  emacsclient for /runtime-eval-code), print-branch helpers; every creation
  flavor, git removal, gns close, clipboard, PGN, profiler and heartbeat
  code dies. workspace-create-client.el and merge-handlers.el are deleted.

## 10. input.el (composer) and commands.el

- `agent-repl--send` pipeline: text → `agent-repl--prepare-input`
  (metaprompt prepend with the sentinel markers via `agent-repl--meta-wrap`,
  prefix/postfix variants) → `(agent-repl-host-composer-gate WS)` treatment
  → UserSaid `(:content (:blocks (...)))` where the blocks are the text
  block(s) plus one `(:arm :image :value (:location (:arm :path :value
  (:path P)) :media-type M))` per image attached through clipboard-image.el
  (ruled: pasted images travel as ImageBlock{path}; the composer keeps a
  per-buffer list of attached images and their MIME types, drawn as the
  existing thumbnail overlay, cleared on a successful send) →
  `agent-repl-rpc-submit-prompt` with `(:said SAID :idempotency-key
  (agent-repl--uuid) :origin ORIGIN :workspace (agent-repl-host-ref WS))` (RFC 4122 v4 from `random`; ORIGIN
  is the send site's keyword, REQUIRED) → success `:turn` →
  clear the input, push history, run `agent-repl-send-posthooks`; success
  `:command-panel` or `:command-refused` → "answered, nothing to await":
  log INFO (`elisp.input.command-answered` with the arm), clear the input
  (the webapp draws the panel or refusal as feed rows; Q3 ruling); error `:merging` → keep the text, `message` + a
  mode-line flash "refused: merge in flight"; transport failure → keep the
  text and offer it to prompt-queue.el (drained on link-up).
- Prompt origins ride the wire (landed: SubmitPromptRequest.origin is
  REQUIRED, never UNSPECIFIED): each send site passes its own keyword,
  also logged in the submit's context. Sites: user-sent, user-sent-and-hide,
  user-sent-with-metaprompt, user-sent-with-postfix, user-sent-with-prefix,
  metaprompt-read, command-diff-analysis, command-explain-context,
  command-explain-prompt, command-update-pr, command-rebase,
  command-create-or-update-pr, deferred-prompt. Dead sites get no constant.
- Composer slash/skill completion dies. The output-nav bindings die. The
  input history (persistence, fuzzy search, glyph) stays.
- prompt-queue.el keeps two roles: explicit deferral
  (`agent-repl-queue-deferred-prompt`, drained on the finish edge through
  `agent-repl-roster-finish-functions`) and the outage queue (drained on
  `agent-repl-link-up-functions`). Its liveness gate is
  `agent-repl-link-up-p` and the composer gate.
- commands.el: the canned-prompt families (explain / diff / PR / rebase /
  tests / lint) compose text and send through the composer pipeline with
  their site's origin; `agent-repl-link-code` opens via
  `agent-repl-popup-open`; snapshot save/load/archive, push/pull tab,
  paste-clipboard, switch-to-N and the restore machinery die.
- clipboard-image.el keeps capturing the pasteboard image to the workspace
  dir and drawing the thumbnail, but registers the file as an attached
  ImageBlock{path, media_type} on the input buffer instead of inserting a
  path token. prompt-summary.el stays with the FORBID guard.

## 11. daemon.el and services.el (cold start)

- `(agent-repl-daemon-ensure &optional ON-READY)`: hooked on
  `agent-repl-link-no-daemon-functions` and run at Emacs startup when
  `agent-repl-frontend-auto-start`. Sequence: daemon.addr present → probe
  DaemonHealth over a fresh conn; any ANSWER (healthy or unhealthy) = a
  daemon is there → adopt it (INFO `elisp.daemon.foreign-adopted` when this
  Emacs did not spawn it; unhealthy faults go to `*agent-repl-health*` as a
  WARNING); a transport failure = a stale file → WARNING
  `elisp.daemon.stale-addr`, treated as absent. Absent → build via
  `agent-repl-daemon-build-script` (bin/build-frontend.sh, its own
  build-if-stale) → start `agent-repl-daemon-command` (default the module's
  `daemon/bin/claude-repld`, no argv) with `AGENT_REPL_STATE_DIR` exported
  explicitly → wait for daemon.addr up to `agent-repl-daemon-boot-timeout-
  seconds` (30) polling with a timer, no sleeps → `agent-repl-link-connect`.
  Build failure → `*agent-repl-build-frontend*` shown, WARNING, `message`,
  the mode-line segment "daemon: build failed"; no automatic retry;
  `agent-repl-frontend-daemon-ensure` (interactive) retries.
- Emacs never kills a daemon that answers. `agent-repl-frontend-daemon-stop`
  = UpdateShutdownSchedule{now, operator "emacs"}; `-restart` = stop then
  ensure. Legacy `agent-repl-daemon-addr`, sentinel-era expected-restart
  bookkeeping and all UDS probing die.
- services.el keeps launchd management of store/sidecar with UDS/readiness
  references removed; `agent-repl-runtime-restart` = build script + store/
  sidecar bounce + daemon stop + ensure.

## 12. frontend.el, webview-recovery.el, open-progress.el, panels.el, popup.el

- `(agent-repl-frontend-webview-url WS)` =
  `http://<address of (agent-repl-host-conn WS)>/?workspace=<url-hexify
  id>&dir=<url-hexify dir>` — both values verbatim from the WorkspaceRef
  (RegisterWorkspace's answer / the roster). Nothing else rides the URL
  (no composer flag).
- The pool: pre-creation and staggering stay (`agent-repl-webview-precreate-
  stagger-seconds`), scheduled after link-up; each webview is bound to its
  workspace buffer for life; `agent-repl-frontend-rescue-webview` (SPC o L)
  survives. `(agent-repl-frontend-reload-webview WS)` navigates the widget
  to the current URL. Every `xwidget-webkit-execute-script` call and every
  script helper (tail, chess, close-menus, text-size, copy-selection,
  recovery probe) is deleted; the stale-webview sweep dies.
- open-progress.el stages: `:requested` (verb sent) → `:acked` → `:host-
  state` (first host push for WS) → `:loaded` (xwidget load finished); the
  stall diagnosis names the first missing stage.
- panels.el's `agent-repl` entry: ensure the current workspace is registered
  and subscribed (host.el), mount its webview, show the input buffer.
- popup.el: `(agent-repl-popup-open PATH &optional LINE)` — directory →
  dired; file → `find-file-noselect` then goto LINE; shown with
  `display-buffer-in-side-window` on the right at half the frame width.
  ONE implementation; commands.el's link-code and notes.el call it. No
  wire caller until Q2.

## 13. Dead pre-pass

Deletes the files in §1 and their tests; removes their entries from
config.el's load list and doctor.el; removes dead keybindings
(rename SPC TAB r, hibernate SPC o z, explain-config SPC j h c/C/n,
hide-project-dirs SPC o H, push/pull tab SPC TAB p/P, switch-to-N SPC TAB
1..0, the whole agent-repl-debug/* family and dump renderer, output-nav
bindings); extracts tasks.el's org-notes helpers into notes.el
(`agent-repl-notes-open` keyed by workspace name, popup via
`agent-repl-popup-open` once it exists — until then `find-file`); adds
`agent-repl--error` to core.el (level "error", persisted and displayed like
`agent-repl--warn`) with tests; removes core.el's symlink migration helpers
and UDS probes; prunes `agent-repl--external-boundary-functions` and
test-helpers.el's clean-state macro and stubs of deleted symbols; rewrites
magit.el's browse-url consumers to plain `browse-url`; removes teal/
hibernated from status.el's tables and faces; updates test-agent-repl.el to
list exactly the surviving suites. Definition of done: `emacs -batch -Q -l
ert -l lisp/test-helpers.el` loads config.el with zero load errors,
test-agent-repl.el references only existing files, test-core.el and
test-notes.el pass. Other surviving suites may be red (their owners rewrite
them).

## 14. Integration suite (fake daemon)

- `lisp/testsupport/fakedaemon/` — a Go program (module
  `agentrepl/fakedaemon`; `replace agentrepl/proto => ../../../proto/gen/go`;
  `connectrpc.com/connect v1.17.0`; `golang.org/x/net v0.43.0` (h2c);
  `google.golang.org/protobuf v1.36.11`; `go 1.23` directive; builds
  OFFLINE with `GOFLAGS=-mod=mod GOPROXY=off go build`). It binds
  127.0.0.1:0, writes `$AGENT_REPL_STATE_DIR/daemon.addr` exactly per the
  common contract (address plus newline, atomic replace; removed on orderly
  exit), serves agentrepl.v1 through the generated Connect handler
  (JSON codec, HTTP/1.1 and h2c), and records every request. Control plane
  on the same mux under `/_fake/`: `POST /_fake/script` sets canned unary
  responses per method (protojson bodies, validated by unmarshalling into
  the generated types); `POST /_fake/push` `{stream, workspace_id?,
  message}` pushes a protojson message to matching open subscribers
  (WatchHostWorkspace by workspace id; WatchDaemon; WatchWorkspaceRoster);
  `POST /_fake/end` `{stream, workspace_id?, error?, abort?}` ends a
  stream with an end frame (optionally carrying a Connect error) or, with
  `abort`, drops the TCP connection without one; `GET /_fake/calls` returns
  the recorded requests in order `[{method, body}]`; `GET /_fake/subscribers`
  lists open streams; `POST /_fake/exit` exits orderly. Unknown fields in
  elisp-sent requests are refused by protojson (that is the round-trip
  check). Two instances can run at once (handover scenarios).
- `lisp/test-integration-helpers.el` (batch-only, like test-helpers.el):
  builds the binary once per run, starts an instance with a private state
  dir, exports `AGENT_REPL_FORBID_VENDOR_CALLS=1`, and provides
  `agent-repl-itest--with-fake-daemon`, `--script`, `--push`, `--end`,
  `--calls`, `--wait-until` (polls with `accept-process-output` under a
  deadline; no sleeps), `--start-second-daemon`. A missing `go` toolchain
  FAILS the suite loudly (no skip). Fake webview factory and fake notifier
  backend record calls.
- Suites, one per production module: `test-integration-connect.el`,
  `-host.el`, `-link.el`, `-roster.el`, `-composer.el` (input.el),
  `-verbs.el`, `-daemon.el`. Scenarios (from the API):
  1. daemon.addr read; RegisterWorkspace round-trip; camelCase keys; id
     echoed verbatim; error arm → loud error; unknown-field refusal.
  2. Re-register + re-subscribe after a daemon restart (second instance on
     a new port rewrites daemon.addr).
  3. Host stream: snapshot then pushes; every composer arm → gate value;
     naming → display title; faults → health buffer; terminal / none →
     blocked; shim_attached false → still open.
  4. notification: unfocused → notifier called with the text and the click
     selects the tab; focused+unselected → the exact blink schedule;
     selected → nothing; permission_requested same policy.
  5. Handover: shutdown_announced{address} → dual attach; transferred →
     AdoptHostWorkspace on the new instance, then the old stream is
     cancelled (old instance observes the disconnect), then re-subscribe on
     the new; order asserted; old stream closing promotes the successor.
  6. Plain bounce (no address): no dual attach; quiet window honored;
     reconnect after daemon.addr reappears.
  7. reload_webapp → the webview reload is called for that workspace only.
  8. drain_scheduled → indicator text per reason arm; drain_cancelled →
     removed; a late subscriber receives the standing schedule.
  9. Roster: all 23 arms decode; unknown arm, unset oneof, two arms → ERROR
     logged, push dropped, stream continues.
  10. Tab reconciliation: rows appear → tabs in walk order; closed → tab
      torn down; reorder → tabs reorder; daemon-originated current change →
      tab switch; Emacs's own switch → exactly one SelectWorkspace.
  11. Finish edge: thinking→done fires the four reactions once;
      permission→thinking does not.
  12. SubmitPrompt: text→UserSaid; uuid key; success → history push and
      cleared input; merging error → text preserved; gate merging → no rpc;
      merge_parked → rpc sent; metaprompt markers; prefix/postfix.
  13. Verbs: each sends the right request echoing the ref; Close blocked →
      no dialog, tab stays; Restart{force}; Create standard and both
      one-shot forms with parent/fork/model/priority; SetWorkspacePriority
      clear omits the field; schedule requires a reason; DaemonHealth
      unhealthy → faults printed.
  14. Cold start: no daemon.addr → build script (stub) invoked → daemon
      command (stub that starts the fake) → link up; stale addr → treated as
      absent; build failure → buffer + WARNING, no start; an answering
      daemon → adopted, no build.
  15. Validation: a HostWorkspace push without `naming` → ERROR, dropped.
  16. Streams: producer close without end frame → ERROR + reconnect; end
      frame with error → ERROR.

## 15. Ownership and seams

| Agent | Owns | Defines (others call) | Calls (others define) |
|---|---|---|---|
| pre-pass | deletions, core.el `agent-repl--error`, notes.el, keybindings prune, test-helpers prune | `agent-repl--error` | — |
| connect | connect.el, rpc.el | §3, §4 | wire-* (by name) |
| wire-a | wire-common.el, wire-host.el, wire-roster.el | §5 | — |
| wire-b | wire-verbs.el | §5 | wire-common (by name) |
| link | daemon-link.el | §6 | rpc, connect |
| host | host.el, notifications.el adaptation | §7 | rpc, link, status blink, frontend reload, workspace.el |
| roster | roster.el, status.el, workspace.el, session.el (finish reactions) | §8 | rpc, host, workspace.el |
| composer | input.el, commands.el, history.el, prompt-queue.el, prompts.el, clipboard-image.el, prompt-summary.el | §10 | rpc, host gate, popup |
| verbs | verbs.el, worktree.el, doctor.el, delete merge-handlers.el + workspace-create-client.el | §9 | rpc, host, link, workspace.el |
| cold-start | daemon.el, services.el, startup wiring in config.el/panels.el boot | §11 | rpc (DaemonHealth), link |
| webview | frontend.el, webview-recovery.el, open-progress.el, panels.el, window.el, popup.el | §12 | host, link |
| integration | testsupport/fakedaemon, test-integration-*.el | — | everything (by name) |

Keybindings.el is shared: each owner edits only its own commands' lines; the
teamlead resolves merge seams.

WAVE-2 REGROUP (project-lead concurrency cap: at most three running agents per
lead): the seven wave-2 rows above are dispatched as THREE briefs, in this
critical-path order — W2-A "stream core" = link + host (+ notifications
adaptation) + cold-start; W2-B "tabs and views" = roster (+ status,
workspace, session finish reactions) + webview (frontend, webview-recovery,
open-progress, panels, window, popup); W2-C "user commands" = composer +
verbs (+ worktree slimming, doctor, deleting merge-handlers.el and
workspace-create-client.el). Seams between the three are exactly the §6–§12
names; each brief owns every file listed for its constituent rows.

## 15b. Remediation queue (teamlead loop; dispatched as slots free)

- R-ACCEPT (from the standing-stream rule): `agent-repl-connect-stream`
  gains an optional ON-OPEN callback, invoked exactly once when the header
  reader parses an HTTP 200 status for the stream (before any frame); a
  non-200 or a transport death before headers never calls it. daemon-link.el
  keys `agent-repl-link-up-functions` (first connect AND reconnect) and the
  successor's readiness on ON-OPEN instead of on spawn; host.el and roster.el
  log `elisp.host.subscribed` / `elisp.roster.subscribed` on ON-OPEN. Tests:
  connect (on-open once, not on non-200, not on death-before-headers),
  daemon-link (link-up only after acceptance; reconnect likewise), and the
  integration link suite's "WatchDaemon with no pushes yields link-up" case
  keeps passing.

- R-QUESTION (landing 3): `HostNotificationKind` gains
  `question_asked {header}`. wire-host.el's kind decoder accepts the arm
  (decoded `(:arm :question-asked :value (:header H))`), pinned against the
  Go bindings' arm set; host.el applies the SAME notification policy as
  permission_requested (unfocused → banner; focused + tab not selected →
  blink; selected → log only), with `header` in the log context. Tests: one
  per decode edge (present, missing header = proto3 default "", unknown
  sibling arm still refused) and one per policy branch.

- R-CLICK (integration suite finding): the desktop notification's click
  has no activation channel — `agent-repl--notify` takes `(WS TITLE
  MESSAGE)` and host.el passes no callback, so "click raises the frame and
  selects the workspace's tab" is unimplemented. notifications.el gains a
  per-notification activation (the existing emacsclient click round-trip
  carrying the workspace name → `agent-repl--notification-activate` raises
  the frame and `agent-repl--ws-switch`es), host.el's unfocused branch uses
  it for every notification kind. Tests: notifications (activation selects
  the tab; unknown workspace → WARNING, no switch), host (unfocused branch
  passes the workspace), the integration host suite's click case.

- FIRST INTEGRATION RUN (tip 942659cd6 + landing 3): connect 12/16, link
  14/18, host 21/32, roster 17/20, daemon 1/12; composer/verbs not run
  (W2-C pending). Clusters, dispatched in this order as slots free:
  R-DAEMON (the cold-start suite, 11 timeouts — independent of every other
  cluster); R-PUSHINVALID (host ×2, roster ×2: an invalid push must log
  `elisp.rpc.push-invalid` at ERROR and leave the stream standing, and the
  suite's log reader must find the record — verify where core.el writes
  global records versus where test-integration-helpers reads them);
  R-NOTIFY (host banner/blink/click + roster attention blink + R-CLICK +
  the `:unknown` gate before any push — after the R-QUESTION agent leaves
  host.el); R-HANDOVER (link dual attach/promotion, host transferred ×4 —
  re-run after R-ACCEPT/R-STREAMCLOSE land; remediate what remains).

- R-SUITE-1 (adversarial audit 1, docs/overhaul/reports/elisp-suite-audit-1.md):
  92 findings, all accepted; a suite-extension agent implements them
  (MISSING first, then WEAK), never running the suite itself. Ruling folded
  in: a drained resend reuses the failed attempt's idempotency key.

## 16. Escalations sent to the project lead (defaults in force meanwhile)

- E1 RESOLVED: SubmitPromptRequest.origin landed, REQUIRED.
- E2 RESOLVED: the trimmed vocabulary landed (cherry-pick of 24740ae4f);
  §8 keys from it.
- E3 RULED as the default: `daemon/bin/claude-repld`, no required argv,
  state root via env; adopt any answering daemon; never kill one.
- E4 CONFIRMED: permission.el dies; the notification policy is the whole
  reaction.
- E5 RULED: merged, closed and killed rows carry closed=true; nuked rows
  leave the roster; tabs derive from closed=false rows in roster order.
- E6 CONFIRMED: no task verbs in Emacs; org notes stay local.
- Soft spots RULED: submitting on a none/terminal session simply submits;
  pasted images travel as ImageBlock{path}.
- E7 transcripts.el (resume choice), ai-title.el (naming.title replaces),
  workspace-status-export.el (the roster stream replaces; the
  create-or-update-workspace skill's status source dies) removed by API
  absence.

## 17. Dead-code passes (programmatic; user rule) — carried into the final report

Pass 1 (R-DEADCODE, opus-low — deviation; the rule now says sonnet-medium):
DELETED wire-common `--encode-int64/--encode-uint32/--encode-bool`;
wire-verbs `--encode-empty`; roster `agent-repl-roster-row-dir`,
`agent-repl-roster-unsubscribe`; frontend `--frontend-live-webview-buffers`,
`--frontend-parent-ws-name`, `--frontend-webview-workspace`; panels
`--non-agent-panel-window-p`; history `--make-instantiation-from-plist`;
session `--effective-model`, `--refresh-magit-status`; 15 orphaned tests.
Pass 2 (R-DEADCODE-2, sonnet-medium): DELETED core.el `--active-inst`,
`--diagnostic-fingerprint`, `--git-root`, `--grey-hex`, `--latch-settled-p`,
`--log-on-transition`, `--non-agent-buffers`, `--reset-warn-once-state`,
`--resolve-current-git-root`, `--user-message-for-error`; workspace.el
`--folded-repo-keys`, `--main-worktree-dir`, `--reorder-workspace-next-to`,
`--repo-label`, `--workspace-for-buffer`, `--ws-advise-kill-before`,
`--ws-error`, `--ws-exists-p`, `--ws-frame-switch`, `--ws-known-projects`,
`--ws-materialize-daemon-workspace`, `--ws-names-cache-usable-p`, `--ws-new`,
`--ws-nil-name`, `--ws-protected-p`, `--ws-registered-dir-owner`,
`--ws-repo-folded-p`, `--ws-run-switch-project-function`,
`--ws-tombstoned-names`, `--ws-unregister-project`; host.el
`agent-repl-host-session-id`; frontends/frontend.el the gui
send/interrupt dispatchers, struct slots, declare-functions and registry
entries (ruling b); prompts.el + test-prompts.el whole (`agent-repl--prompt`
chain; zero production callers — one-shot prompt composition is the
daemon's); one straggler test in test-core.el.
KEPT WITH REASON (each pinned): `+dwc/magit-open-commit-in-github`,
`+dwc/magit-copy-commit-link` (bound in magit.el's map!; test-magit.el);
`agent-repl-install-commit-emoji-hook` (autoloaded interactive; the blessed
commit-emoji hook's provisioning path — teamlead ruling a; test-emoji.el);
`agent-repl-prompt-summary-attach-all` (manual recovery, declared in-file;
test-prompt-summary.el); `agent-repl-link-code`, `agent-repl--eval-format-prompt`
(/runtime-eval-code emacsclient entry points; test-commands.el,
test-worktree.el); `agent-repl-refresh-webviews`, `agent-repl-frontend-close-panel`,
`agent-repl-runtime-restart`, `agent-repl-tabbar-apply-row-count`,
`agent-repl-restart` (interactive commands; own tests);
`agent-repl-status-tab-color` (its test pins every-arm-has-a-colour);
`agent-repl--prompt`… gone (see above); `agent-repl--message-has-emoji-prefix-p`
(commit-emoji surface); `agent-repl--workspace-log-target-entry` (core.el;
test-introspection helper behind 18+ log-routing assertions);
`agent-repl-host-forget` (host.el; integration-suite teardown hygiene;
test-host.el). Cosmetic residue: test-integration-composer.el:28
`declare-function agent-repl--meta-wrap "prompts"` should name
"agent-repl-core"; `:explain-config` in wire-common.el is a frozen proto
enum arm and stays. Unused-lexical byte-compile: clean whole-module.
