;;; recovery-slo.el --- the 3s workspace recovery budget -*- lexical-binding: t; -*-

;;; Commentary:

;; WHAT THIS MEASURES, AND WHY IT IS A CONJUNCTION.
;;
;; A daemon bounce severs three things at once, and each comes back on its
;; own clock: Emacs's applied view of the workspace, the page's stream, and
;; the workspace's traffic over the UDS link.  Every signal this system
;; already had answers only one of them, and each of them is optimistic on
;; its own — an open socket is not a delivered frame, a rendered status is
;; not a fresh one, and a link that carries a heartbeat is not a link
;; carrying THIS workspace's data.  So recovery here is the CONJUNCTION of
;; three independently stamped facts, and a workspace is recovered at the
;; moment the LAST of them lands:
;;
;;   emacs  — the daemon's WorkspaceState landed and was applied to this
;;            workspace's view (`agent-repl--frontend-apply-workspace-state'),
;;            so what the status line and the modeline draw is the new
;;            daemon's ruling and not the cached one from before the bounce;
;;   webapp — the page RE-ADOPTED a snapshot *and* ingested at least one
;;            content frame since the recovery epoch opened.  Asked of the
;;            page itself through `agentReplRecoveryProbe'
;;            (webapp/src/recovery-probe.ts), which refuses to answer yes on
;;            an open socket alone — that refusal is the point of it;
;;   wire   — a frame carrying THIS workspace crossed the UDS link and was
;;            dispatched (`agent-repl--uds-dispatch-frame'), which is the
;;            only one of the three that is evidence about the transport
;;            rather than about something downstream of it.
;;
;; MEASURED EVIDENCE FOR THE BUDGET.  From the 18:52 bounce in
;; ~/.claude-emacs/claude-repld.log: the daemon serves a workspace's connect
;; snapshot in under a millisecond of the page's socket opening (253 serves,
;; every one at delta<1ms).  ALL of the recovery latency is therefore in WHEN
;; the page reconnects, not in what the daemon does afterwards — which rules
;; out the daemon-side suspects (history replay, snapshot lease cadence) and
;; leaves the client edge.  Three workspaces reconnected 1.0s after the
;; daemon reported ready; the rest took 24.3s, 32.6s and 80.8s.  The host
;; sweep that exists to repair them (webview-recovery.el) fires ONCE, on the
;; link-up edge, and has no idea whether it worked; a page that was mid-boot,
;; mid-reload or not yet mounted when it fired simply never got another one.
;;
;; SO THIS MODULE IS THE RETRY THAT EDGE NEVER HAD, and it is armed by
;; measurement rather than by hope: a workspace whose conjunction is not
;; satisfied within `agent-repl-recovery-slo-budget-ms' is warned about BY
;; NAME with WHICH signal is outstanding, force-recovered through the
;; machinery that already exists — the webview sweep for the page half, the
;; ensure/reattach path for the session half — and then RE-VERIFIED against
;; the same conjunction.  The forced path never claims success; it reports
;; whatever the re-verification found.
;;
;; ONE RECORD PER WORKSPACE PER RECOVERY carries all of it, at the voice of
;; its neighbours and through the same logging helpers, so the SLO's evidence
;; is greppable in a single pass on `recovery-slo:'.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)

(declare-function agent-repl--log "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--log-verbose "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--warn "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--live-ws-names "agent-repl-workspace" ())
(declare-function agent-repl--ws-get "agent-repl-workspace" (ws key))
(declare-function agent-repl--frontend-webview-read-script
                  "agent-repl-frontend" (buf script callback))
(declare-function agent-repl--frontend-ensure-workspace "agent-repl-frontend-client" (ws))
(declare-function agent-repl--webview-recovery-sweep
                  "agent-repl-webview-recovery" (reason &optional force))

(defvar agent-repl-uds-snapshot-applied-functions)

(defcustom agent-repl-recovery-slo-budget-ms 3000
  "Milliseconds a workspace has to satisfy the whole recovery conjunction.
Counted from the snapshot-applied edge — the first instant at which the
daemon's state of the world has landed and recovery is answerable at all.
A workspace still outstanding at the budget is warned about and
force-recovered; it is NOT a hard failure, because the forced path may
still bring it back inside the following moments and the record says
which of the two outcomes actually happened."
  :type 'integer
  :group 'agent-repl)

(defcustom agent-repl-recovery-slo-poll-ms 500
  "Milliseconds between two evaluations of a workspace's conjunction.
The emacs and wire signals arrive by push and need no polling at all;
this cadence exists for the page, which can only be ASKED.

WHY 500 AND NOT THE 250 THIS MODULE FIRST SHIPPED WITH.  The cadence
buys exactly one thing — resolution on a 3000ms budget — and 500ms
already spends six samples inside it, which is finer than the quantity
being measured is meaningful to: recovery gaps observed in the logs are
tens of SECONDS, and no decision this module makes changes on a quarter
second.  What the cadence COSTS is script injections into live WebKit
views, multiplied by every mounted webview: at 250ms a bounce with the
seventeen webviews this host actually runs was driving roughly seventy
evaluateJavaScript calls per second, each one an async round trip whose
reply lands on the main thread.  Halving the rate halves that pressure
for no loss of anything the record reports, so 500 is the cheapest
cadence that still resolves the budget honestly.  This is a rate the
module chooses, NOT a safety mechanism: the crash this rate once
amplified is prevented structurally in
`agent-repl--frontend-webview-read-script', and lowering the cadence
alone would only have made it rarer."
  :type 'integer
  :group 'agent-repl)

(defcustom agent-repl-recovery-slo-reverify-ms 2000
  "Milliseconds the forced path is given before its re-verification rules.
The forced recovery drives a page reload or a fresh ensure, neither of
which is instantaneous; re-verifying immediately would report the
failure the force was issued to repair.  The re-verification is still a
verification and not a wait: it reads the same conjunction, and reports
whatever it finds."
  :type 'integer
  :group 'agent-repl)

(defconst agent-repl-recovery-slo-signals '(emacs webapp wire)
  "The conjunction, in the order a record names its deltas.
Every one of these MUST be stamped for a workspace to count as
recovered.  Adding a fourth signal is adding a column to the record;
removing one is weakening the SLO and must not be done quietly.")

(defvar agent-repl--recovery-slo-attempts (make-hash-table :test 'equal)
  "Open recovery attempts, keyed by workspace name.
Each value is a plist of `:started-at' (float-time) and one stamp per
signal under `:emacs' / `:webapp' / `:wire', plus `:forced' once the
budget has been breached and the forced path driven.  A workspace with
no entry has no attempt open, and its signals are dropped rather than
stamped: a stamp with no attempt to belong to is a measurement of
nothing.

EVERY VALUE IN HERE IS A SCALAR — a string, a number, or t — and that is
an invariant with a crash behind it rather than a preference.  This table
is global and long-lived, and its contents reach `format' through the
record and through every warning that prints an attempt; anything
xwidget-derived stored here would eventually be PRINTED, and printing a
widget whose page has gone away is the use-after-free that took this
module's first version down.  Storing only scalars means there is nothing
in this table that printing could ever follow into freed memory.")

(defvar agent-repl--recovery-slo-timer nil
  "The repeating timer driving `agent-repl--recovery-slo-tick', or nil.")

;;;; ---- The conjunction ---------------------------------------------------

(defun agent-repl--recovery-slo-outstanding (attempt)
  "Return the signals ATTEMPT still lacks, in `agent-repl-recovery-slo-signals' order.
nil means the conjunction is satisfied.  Order is fixed so two warnings
about the same shortfall read identically."
  (cl-remove-if (lambda (signal)
                  (plist-get attempt (intern (format ":%s" signal))))
                agent-repl-recovery-slo-signals))

(defun agent-repl--recovery-slo-delta-ms (attempt signal)
  "Return SIGNAL's delta in ATTEMPT in whole milliseconds, or -1 when unstamped.
-1 rather than 0 or nil: an unstamped signal is a MISSING measurement,
and a record that printed it as zero would read as the fastest possible
recovery of exactly the thing that never happened."
  (let ((at (plist-get attempt (intern (format ":%s" signal))))
        (start (plist-get attempt :started-at)))
    (if (and at start) (round (* 1000 (- at start))) -1)))

(defun agent-repl--recovery-slo-total-ms (attempt)
  "Return ATTEMPT's total gap in ms — its LAST signal — or -1 when incomplete.
The total is the last signal and not the first, because the conjunction
is what recovery means: a workspace whose page came back instantly is
not recovered while its wire is still silent."
  (if (agent-repl--recovery-slo-outstanding attempt)
      -1
    (let ((deltas (mapcar (lambda (s) (agent-repl--recovery-slo-delta-ms attempt s))
                          agent-repl-recovery-slo-signals)))
      (apply #'max deltas))))

;;;; ---- The canonical record ----------------------------------------------

(defun agent-repl--recovery-slo-emit (ws attempt outcome)
  "Emit THE record for WS's ATTEMPT under OUTCOME, returning its total gap.

ONE record per workspace per recovery, carrying every per-signal delta
and the total, so the SLO's whole evidence is one `recovery-slo:' grep.
Emitted at `agent-repl--log' for an outcome inside budget and at
`agent-repl--warn' otherwise — a breach is not a debug detail, and the
record itself names which signal was outstanding when it happened."
  (let* ((outstanding (agent-repl--recovery-slo-outstanding attempt))
         (total (agent-repl--recovery-slo-total-ms attempt))
         (line (concat "recovery-slo: ws=%s outcome=%s emacs_ms=%d webapp_ms=%d "
                       "wire_ms=%d total_ms=%d budget_ms=%d forced=%s outstanding=%s"))
         (args (list ws outcome
                     (agent-repl--recovery-slo-delta-ms attempt 'emacs)
                     (agent-repl--recovery-slo-delta-ms attempt 'webapp)
                     (agent-repl--recovery-slo-delta-ms attempt 'wire)
                     total
                     agent-repl-recovery-slo-budget-ms
                     (if (plist-get attempt :forced) "yes" "no")
                     (if outstanding
                         (mapconcat #'symbol-name outstanding ",")
                       "none"))))
    (if outstanding
        (apply #'agent-repl--warn ws line args)
      (apply #'agent-repl--log ws line args))
    total))

;;;; ---- Opening and stamping ----------------------------------------------

(defun agent-repl--recovery-slo-open (ws)
  "Open a recovery attempt for WS, discarding any attempt already open.

Discarding rather than keeping: a second snapshot-applied edge is a
SECOND outage, and carrying the first one's stamps into it would date a
recovery from a link that has since died."
  (puthash ws (list :started-at (float-time)) agent-repl--recovery-slo-attempts)
  (agent-repl--log-verbose ws "recovery-slo: attempt opened ws=%s budget_ms=%d"
                           ws agent-repl-recovery-slo-budget-ms))

(defun agent-repl--recovery-slo-note (ws signal)
  "Stamp SIGNAL for WS, if an attempt is open and SIGNAL is unstamped.
FIRST stamp wins: what the SLO asks is when the signal came BACK, and a
later frame of the same kind measures liveness rather than recovery.
A workspace with no open attempt is ignored — see
`agent-repl--recovery-slo-attempts'."
  (let ((attempt (gethash ws agent-repl--recovery-slo-attempts))
        (key (intern (format ":%s" signal))))
    (when (and attempt (null (plist-get attempt key)))
      (puthash ws (plist-put attempt key (float-time))
               agent-repl--recovery-slo-attempts))))

(defun agent-repl--recovery-slo-note-emacs (ws)
  "Stamp WS's emacs-side signal: its applied view is the new daemon's."
  (agent-repl--recovery-slo-note ws 'emacs))

(defun agent-repl--recovery-slo-note-wire (ws)
  "Stamp WS's wire-side signal: a frame for it crossed the UDS link."
  (agent-repl--recovery-slo-note ws 'wire))

;;;; ---- Asking the page ---------------------------------------------------

(defconst agent-repl-frontend-recovery-probe-hook "agentReplRecoveryProbe"
  "Name of the webapp global that reports the page's recovery evidence.
The webapp plants it on `window' at boot (`RECOVERY_PROBE_HOOK' in
webapp/src/recovery-probe.ts) — the two names are one contract and MUST
match.  It returns a JSON string.")

(defun agent-repl--recovery-slo-probe-script (ws)
  "Return the JS asking the page for its recovery report ON BEHALF OF WS.
Guarded on the hook existing: a webview mid-boot or running a bundle
that predates the probe has not planted it, which is an expected state
answered with the empty string rather than a thrown reference error.

WS RIDES IN THE SCRIPT AND COMES BACK IN THE REPLY, and that is what
makes this module crash-safe rather than merely tidy.  The obvious way
to attribute a reply to a workspace is a closure over WS, and that is
exactly what must never be handed to `xwidget-webkit-execute-script' on
the NS port — see `agent-repl--frontend-webview-read-script', which
refuses anything but a symbol for precisely this reason.  So the
correlation travels through the PAGE: the workspace name is baked into
the script as a JSON literal, echoed back in the reply, and the reply
handler is one permanently-interned symbol shared by every workspace.

It is deliberately the host's OWN name for the workspace that is echoed,
not the page's `workspace' field: the page reports the daemon's `cwd',
and making the SLO's routing depend on those two strings agreeing would
put an unrelated contract between a measurement and the thing it
measures."
  (format "(function(){var r=(window.%s?window.%s():\"\");\
return JSON.stringify({ws:%s,report:r});})()"
          agent-repl-frontend-recovery-probe-hook
          agent-repl-frontend-recovery-probe-hook
          (json-encode-string ws)))

(defun agent-repl--recovery-slo-probe-reply (raw)
  "Handle one page reply RAW, stamping the webapp signal for the WS it names.

THE CALLBACK, AND IT IS A TOP-LEVEL NAMED FUNCTION ON PURPOSE.  This
symbol is what gets handed across the xwidget boundary, where the NS port
captures it into a GC-invisible Objective-C block; an interned symbol
survives that indefinitely where a closure would be collected out from
under the block and crash Emacs when the reply arrived.  Every workspace
shares this one function, and the workspace a reply belongs to is read
out of the reply itself (`agent-repl--recovery-slo-probe-script').

A reply that is not a string, not JSON, or names no workspace stamps
nothing: a page mid-boot legitimately has nothing to say yet, and the
budget is what rules on a page that never starts saying it."
  (when (stringp raw)
    (let* ((envelope (condition-case nil
                         (json-parse-string raw :object-type 'alist)
                       (error nil)))
           (ws (and envelope (alist-get 'ws envelope)))
           (report (and envelope (alist-get 'report envelope))))
      (when (and (stringp ws) (not (string-empty-p ws))
                 (agent-repl--recovery-slo-probe-satisfied-p report))
        (agent-repl--recovery-slo-note ws 'webapp)))))

(defun agent-repl--recovery-slo-probe-satisfied-p (raw)
  "Return non-nil when RAW, the page's JSON report, proves REAL data landed.

Read STRICTLY: the page's own `satisfied' is not trusted on its own,
because a bundle could regress it to a socket-open flag and this end
would never notice.  Both underlying facts are re-checked here — an
adoption AND at least one content frame — so socket-open alone cannot
satisfy the webapp signal from either side of the contract.

A malformed or empty report is NOT satisfied and is not an error: a page
mid-boot legitimately has nothing to say yet, and the budget is what
rules on a page that never starts saying it."
  (and (stringp raw)
       (not (string-empty-p raw))
       (let ((report (condition-case nil
                         (json-parse-string raw :object-type 'alist)
                       (error nil))))
         (and report
              (eq t (alist-get 'adopted report))
              (numberp (alist-get 'realDataFrames report))
              (> (alist-get 'realDataFrames report) 0)))))

(defun agent-repl--recovery-slo-poll-webapp (ws)
  "Ask WS's page for its recovery report, stamping the webapp signal on proof.

NOTHING ABOUT THE WEBVIEW IS REMEMBERED BETWEEN TICKS.  The buffer is
looked up from the workspace and the widget is re-resolved from that
buffer on EVERY tick, inside
`agent-repl--frontend-webview-read-script', which validates liveness
immediately before it injects.  A workspace whose buffer was killed, or
whose page was re-navigated or remounted since the last tick, therefore
gets a fresh widget or no injection at all — a widget this module held
across a timer tick would be a use-after-free the moment the page it
named went away, and the only way to be sure it never holds one is never
to store one.

A workspace with no live webview buffer is not asked: there is no page
to answer, and the budget will report the webapp signal outstanding,
which is the honest reading of a workspace whose page does not exist."
  (let ((buf (agent-repl--ws-get ws :frontend-buffer)))
    (when (buffer-live-p buf)
      (condition-case err
          (agent-repl--frontend-webview-read-script
           buf (agent-repl--recovery-slo-probe-script ws)
           #'agent-repl--recovery-slo-probe-reply)
        (error (agent-repl--warn ws "recovery-slo: ws=%s probe failed err=%S" ws err))))))

;;;; ---- Forcing ------------------------------------------------------------

(defun agent-repl--recovery-slo-force (ws)
  "Force WS through the recovery machinery that already exists.

TWO HALVES, because the conjunction has two halves that can fail apart:
the host-driven webview sweep repairs the PAGE (and is forced past its
debounce — see `agent-repl--webview-recovery-sweep' — because this sweep
is issued on measured evidence that the debounced one did not work), and
`agent-repl--frontend-ensure-workspace' repairs the SESSION through the
ordinary ensure/reattach path.  Neither is a new recovery mechanism, and
deliberately so: a second one would drift from the first.

Each half is guarded on its own, so a failure in one still leaves the
other driven, and the failure is warned about rather than swallowed."
  (condition-case err
      (agent-repl--webview-recovery-sweep "recovery_slo_force" t)
    (error (agent-repl--warn ws "recovery-slo: ws=%s force sweep failed err=%S" ws err)))
  (condition-case err
      (agent-repl--frontend-ensure-workspace ws)
    (error (agent-repl--warn ws "recovery-slo: ws=%s force ensure failed err=%S" ws err))))

(defun agent-repl--recovery-slo-breach (ws attempt)
  "Report WS's budget breach, force it, and arm the re-verification.
The breach record is emitted BEFORE the force so the evidence of the
failure survives whatever the force then does, and `:forced' is set so
the re-verification's record cannot be mistaken for the first one."
  (agent-repl--recovery-slo-emit ws attempt "budget-breach")
  (puthash ws (plist-put attempt :forced t) agent-repl--recovery-slo-attempts)
  (agent-repl--recovery-slo-force ws)
  (run-at-time (/ agent-repl-recovery-slo-reverify-ms 1000.0) nil
               #'agent-repl--recovery-slo-reverify ws))

(defun agent-repl--recovery-slo-reverify (ws)
  "Re-read WS's conjunction after a forced recovery and report the outcome.
NEVER claims recovery on the force's say-so: the outcome is whatever the
same three signals report now.  The attempt is closed either way —
`forced-recovered' when the conjunction is finally satisfied,
`forced-unrecovered' when it is not, and the latter is a warning naming
what is still missing."
  (let ((attempt (gethash ws agent-repl--recovery-slo-attempts)))
    (when attempt
      (agent-repl--recovery-slo-poll-webapp ws)
      ;; Re-read: the poll's callback may have stamped the page in place.
      (let* ((attempt (gethash ws agent-repl--recovery-slo-attempts))
             (outcome (if (agent-repl--recovery-slo-outstanding attempt)
                          "forced-unrecovered"
                        "forced-recovered")))
        (agent-repl--recovery-slo-emit ws attempt outcome)
        (remhash ws agent-repl--recovery-slo-attempts)))))

;;;; ---- The tick -----------------------------------------------------------

(defun agent-repl--recovery-slo-check (ws)
  "Advance WS's open attempt by one evaluation of the conjunction.

Returns `recovered', `breached', `pending', or nil when no attempt is
open.  A workspace already `:forced' is left entirely to its
re-verification: it must not be breached a second time, which is what
would turn one outage into an unbounded force loop."
  (let ((attempt (gethash ws agent-repl--recovery-slo-attempts)))
    (cond
     ((null attempt) nil)
     ((plist-get attempt :forced) 'pending)
     (t
      (agent-repl--recovery-slo-poll-webapp ws)
      (let* ((attempt (gethash ws agent-repl--recovery-slo-attempts))
             (elapsed-ms (round (* 1000 (- (float-time)
                                           (plist-get attempt :started-at))))))
        (cond
         ((null (agent-repl--recovery-slo-outstanding attempt))
          (agent-repl--recovery-slo-emit ws attempt "recovered")
          (remhash ws agent-repl--recovery-slo-attempts)
          'recovered)
         ((>= elapsed-ms agent-repl-recovery-slo-budget-ms)
          (agent-repl--recovery-slo-breach ws attempt)
          'breached)
         (t 'pending)))))))

(defun agent-repl--recovery-slo-tick ()
  "Evaluate every open attempt, disarming the timer once none are left.
A workspace whose check signals is warned about and the tick continues:
one workspace's fault must not strand the SLO for every other."
  (let ((open (hash-table-count agent-repl--recovery-slo-attempts)))
    (if (zerop open)
        (agent-repl--recovery-slo-disarm)
      (dolist (ws (hash-table-keys agent-repl--recovery-slo-attempts))
        (condition-case err
            (agent-repl--recovery-slo-check ws)
          (error (agent-repl--warn ws "recovery-slo: ws=%s check failed err=%S" ws err)))))))

(defun agent-repl--recovery-slo-arm ()
  "Start the tick timer, if it is not already running."
  (unless (timerp agent-repl--recovery-slo-timer)
    (setq agent-repl--recovery-slo-timer
          (run-at-time (/ agent-repl-recovery-slo-poll-ms 1000.0)
                       (/ agent-repl-recovery-slo-poll-ms 1000.0)
                       #'agent-repl--recovery-slo-tick))))

(defun agent-repl--recovery-slo-disarm ()
  "Stop the tick timer.  Idempotent."
  (when (timerp agent-repl--recovery-slo-timer)
    (cancel-timer agent-repl--recovery-slo-timer))
  (setq agent-repl--recovery-slo-timer nil))

(defun agent-repl--recovery-slo-on-link-up ()
  "Open an attempt for every live workspace when the daemon link comes back.
Subscriber for `agent-repl-uds-snapshot-applied-functions' — the same
edge the host webview sweep runs on, so the SLO measures exactly the
recovery that sweep is trying to perform."
  (dolist (ws (agent-repl--live-ws-names))
    (agent-repl--recovery-slo-open ws))
  (when (> (hash-table-count agent-repl--recovery-slo-attempts) 0)
    (agent-repl--recovery-slo-arm)))

;; ARMED AFTER the webview sweep's own subscriber, so the attempt a
;; workspace opens is measured against a sweep that has already been issued
;; rather than one still queued behind it.
(add-hook 'agent-repl-uds-snapshot-applied-functions
          #'agent-repl--recovery-slo-on-link-up t)

(provide 'recovery-slo)
;;; recovery-slo.el ends here
