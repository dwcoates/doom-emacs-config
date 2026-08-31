# STOPPING POINT — EMACS (elisp) overhaul (2026-08-31, wind-down directive)

Branch `overhaul/elisp`; STOP tip is the commit that adds this file (its
parent 798760ff6 is the last code commit). Zero agent worktrees remain under
~/.config/doom-overhaul/elisp-agents; no agents running; nothing queued for
dispatch. The live ledger is docs/overhaul/elisp-fanout.md §0c (compaction
rule included); the audit record is reports/elisp-suite-audit-1.md.

## Merged inventory (all green at unit level, load-errors=nil)

Foundation → landings 1–5 merged (last: 081dbbba8; vocab reconciled at
f1132d3a7, files identical to integration). Waves, all merged, worktrees
removed: connect+rpc; wire-common/host/roster/verbs; dead-code pre-pass
(+notes.el, agent-repl--error/--fatal split); W2-A (daemon-link, host,
daemon cold start, services, notifications); W2-B (roster, status, popup,
workspace, session, frontend, webview-recovery, open-progress, panels,
window); W2-C (input composer, commands, verbs, worktree slimming, doctor);
integration suite + Go fake daemon (testsupport/fakedaemon; raw-body
recording, /_fake/gate, headers flushed on accept); R-DAEMON; R-ARMS +
R-LOGOP; R-VERBS (+ log-routing reversal); R-SUITE-1 (all 92 audit findings
pinned); remediation-1 (R-ACCEPT stream-acceptance-on-headers, R-QUESTION,
R-STREAMCLOSE, host refusal handler + redial-and-reload); R-NOTIFY
(notification policy, blink cadence, R-CLICK activation, :unknown gate,
landing-5 merge-queue repository scope).

Unit suites at the tip (last verified counts): core 400, wire-common 67,
wire-host 104, wire-roster 48, wire-verbs 237, connect 71, rpc 23,
daemon-link 68, host 94, roster 36, status 229, workspace 266, session 15,
frontend 86, webview-recovery 16, open-progress 33, panels 221, window 58,
notes 17, notifications 83, verbs 82, input 62, commands 50, worktree 38,
prompt-queue 30, prompt-summary 25, history 127, clipboard-image 15,
keybindings 18, config 23, magit 63, emoji 75, frontends 29, test-helpers
31, render-colors 18 — all green. Go fake daemon build+test green offline.

## Integration board at 798760ff6 (per-suite; each red's cause/owner)

- connect 21/21 GREEN.
- link 20/26. 6 red, all R-HANDOVER: dual attach on an announced address,
  handover hooks, bounce reconnect once addr returns, down/up/handover log
  pins, old-stream-close promotion, and audit #11 (drain cause text must
  name the DrainReason carried by scheduled_drain/immediate).
- host 40/51. 11 red: the transferred family ×7 (adopt on successor /
  ordering / cancel / resubscribe / conn update / real-link path / adopt
  error keeps old stream) → R-HANDOVER; register-error on-done nil (#33),
  standing faults exposure + health buffer (#30 pair), webview URL
  carries-only-workspace-and-dir (#38) → R-POLISH.
- roster 34/47. 13 red → R-ROSTER: the finish-edge family (fires on
  running→settled, not running→running or settled→settled; the four
  reactions observable: banner "Agent ready: <name>" (#52), echo, magit
  refresh, deferred drain), walk order (two repo sections, children
  depth-first, task view ignored), row rename keeps the workspace,
  roster-missing-repository refused, roster-without-current no-op.
- daemon 14/15. 1 red → R-POLISH: #90 default command path + no argv.
- verbs 29/61. 32 red → R-VERBS-SUITE (suite-side, DIAGNOSED): R-SUITE-1
  repaired the pre-existing tests to the PRE-R-VERBS convention — they pass
  ready-made oneof plists to verbs that take bare keywords and wrap
  internally (create refuses "unknown creation form (:arm :standard …)";
  set-priority double-wraps). Fix: re-repair the suite's verb calls to the
  landed signatures (bare form arm + keyword facts; bare level keyword;
  flat actions), then triage the ~21 timeout-class failures that remain
  (log-ack and teardown assertions). The two landing-5 scoped merge-queue
  tests should go green with the call repair (the encoder landed with
  R-NOTIFY).
- composer 37/42. 5 red → R-COMPOSER (input.el production gaps): attached
  image as ImageBlock{path} + media type on the wire (2), success-turn
  posthooks, transport-failure defer path, #66 outage drain reusing the
  failed attempt's idempotency key (ruled: same key).

## Queued briefs for resume, in dispatch order (all cancelled for the pause)

1. R-VERBS-SUITE — the diagnosed suite-side call repair + timeout triage.
2. R-HANDOVER — link+host transferred family per §7 FINAL HANDOVER
   SEQUENCE (adopt at the announced address, conn update, webview reload,
   page self-adopts, promotion), plus #11 drain-cause text and #52 banner
   format.
3. R-ROSTER — finish-edge semantics + reactions wiring + walk order +
   rename + the two roster validation pins.
4. R-COMPOSER — the five input.el gaps (#66 per the ruling).
5. R-POLISH — daemon #90; host #33/#30/#38.
6. Adversarial audit 2 (fable, fresh context) once the board is green;
   then the loop per TEAMLEAD.md.

## Open questions / decisions parked

- R-NOTIFY's surfaced fixture-hook decision: how the harness installs the
  fake notifier/effects consumer (its final commit "the new attention
  scenarios restore the consumer they test", 10a107963, carries the
  current answer; revisit when R-ROSTER touches the same harness hooks).
- Q4 (HeldPrompt accept) and Q5 (capture run) remain the project lead's;
  neither blocks Emacs work. Q1 /agents+/help panels: daemon does not
  recognize them; Emacs already treats command_refused as "answered".
- The create-or-update-workspace skill's `status` verb lost its
  workspace-status.json source (recorded follow-up outside the wave).

## UX gaps filled from the API (standing; also in the ledger)

Tab set = closed=false rows in roster walk order; merged/closed/killed rows
carry closed=true (ruled). Finish edge = running {submitting thinking
clearing compacting permission} → settled {ready done interrupted
idle-async}. Tab-name collision → "name·repo label". Composer on
none/terminal/unknown SENDS (ruled). Foreign daemon adopted, never killed
(ruled). Drain indicator = global-mode-string segment. Notes keyed per
workspace name. transcripts.el / ai-title.el / workspace-status-export.el
removed by API absence. Buffer titles: naming.title > slug > roster row
name. Deferred queue: explicit deferral drains on the finish edge, outage
queue on link-up, same-key resend.

## Worktrees / state

None left. AGENT_REPL_FORBID_VENDOR_CALLS=1 on every test invocation; no
real vendor call anywhere; nothing deployed, nothing hot-loaded, nothing
pushed.
