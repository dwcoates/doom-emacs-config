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
;; WHEN THE CLOCK STARTS, AND THE RULE THAT KEEPS IT HONEST.  There are two
;; independent pieces of evidence that a workspace's link went away, and a
;; real bounce produces one, the other, or both:
;;
;;   the ANNOUNCEMENT — the daemon says it is bouncing (or Emacs orders the
;;            bounce itself), which opens the expected-restart window
;;            (lisp/daemon.el).  This module subscribes to
;;            `agent-repl-frontend-expected-restart-armed-functions';
;;   the DOWN EDGE — an established UDS link drops (the sentinel in
;;            lisp/frontend-uds.el).
;;
;; BOTH must arm, because either can be the only one that happens.  An
;; ANNOUNCED restart is precisely the case that produces no down-edge WARNING
;; — the window demotes it to "uds-link: down for the %s restart" — and it is
;; also the case the verification loop measures, so arming on the down edge
;; alone measured nothing at all for the bounce that mattered most.  An
;; UNANNOUNCED drop, symmetrically, has no announcement to arm from and must
;; still be measured.
;;
;; THE RULE, and it is one sentence: ONE WORKSPACE HAS AT MOST ONE IN-FLIGHT
;; BUDGET, AND ITS START INSTANT IS THE FIRST EVIDENCE THE LINK WENT AWAY —
;; whichever of the two arrived earlier.  So the two paths cannot double-arm
;; (a second arming for a workspace that already has an attempt open never
;; creates a second one) and cannot reset each other's clock (a LATER piece
;; of evidence never moves the start forward; an EARLIER one — an
;; announcement decoded after the drop it describes — moves it BACK, because
;; the outage began then).  Stamps already collected are kept across either,
;; since they are measurements of the same outage.  A workspace's attempt
;; ends only when its record is emitted.
;;
;; THE ARMING MUST ALSO PRECEDE THE STAMPS, which is the other half of why
;; the announcement path matters: the emacs signal is stamped while the
;; reconnect snapshot is being APPLIED (lisp/frontend-state.el), and the
;; link-up hook this module also subscribes to runs AFTER that same apply
;; finishes.  A workspace armed only there therefore drops the very stamp its
;; reconnect produced — a stamp with no attempt to belong to is discarded by
;; design — and could never satisfy the conjunction from the reconnect that
;; opened it.  Arming from the announcement and from the down edge puts the
;; attempt in place BEFORE the snapshot lands, so the stamp has somewhere to
;; go.  The link-up arming is kept as the backstop for a workspace that had
;; neither piece of evidence attributed to it (one registered mid-outage,
;; say), never as the primary edge.
;;
;; WHO IS MEASURED, AND WHEN THE CLOCK ACTUALLY STARTS.  Two things the
;; first live bounce settled, both of them by producing records that said
;; nothing:
;;
;;   SCOPE.  Only a RECOVERABLE workspace is armed — one that had a page or
;;   a live session when the outage began (`agent-repl--recovery-slo-exclusion',
;;   which reads the eligibility and drain-gate predicates that already own
;;   those questions).  A workspace with neither cannot satisfy a conjunction
;;   about its page and its wire, so it breached by construction and was
;;   forced for nothing.  It is now recorded once as `outcome=not-measured'
;;   with the reason, so a reader can tell NOT MEASURED from MEASURED AND FINE;
;;
;;   THE ANCHOR.  The budget is counted from `:answerable-at' — the link-open
;;   edge — and not from the arming, because an announced restart arms before
;;   the daemon has even died and nothing about recovery is answerable until
;;   frames flow again (`agent-repl--recovery-slo-base').  The outage span
;;   itself is not discarded: it is reported as `outage_ms'.
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
(declare-function agent-repl--frontend-precreate-refusal "agent-repl-frontend" (ws))
(declare-function agent-repl--frontend-session-controller-live-p
                  "agent-repl-frontend" (ws))
(declare-function agent-repl--uds-connected-p "agent-repl-frontend-uds" ())
(declare-function agent-repl--webview-recovery-sweep
                  "agent-repl-webview-recovery" (reason &optional force))
(declare-function agent-repl--webview-recovery-repair-workspace
                  "agent-repl-webview-recovery" (ws reason))

(defvar agent-repl-uds-snapshot-applied-functions)
(defvar agent-repl-uds-connected-functions)
(defvar agent-repl-frontend-expected-restart-armed-functions)

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

(defvar agent-repl--recovery-slo-excluded (make-hash-table :test 'equal)
  "Workspaces last recorded as NOT MEASURED, mapped to when that was said.
Kept so an exclusion is stated ONCE per outage rather than once per
arming path — three armings of the same bounce must not print the same
non-measurement three times — while a LATER outage still restates it.
The staleness window is the module's own budget plus its re-verification,
i.e. the longest a single measured recovery can last, so an entry can
only go stale after the outage that wrote it is over.")

(defvar agent-repl-recovery-slo-link-up-function
  (lambda () (agent-repl--uds-connected-p))
  "Called with no args; non-nil when the UDS link is carrying frames.
The one place this module asks whether recovery is answerable YET — see
`agent-repl--recovery-slo-base'.  A variable so a test can drive the
outage window without a socket, never so a caller can redefine what
`connected' means.")

(defun agent-repl--recovery-slo-link-up-p ()
  "Return non-nil when the link is up, through the injected seam."
  (funcall agent-repl-recovery-slo-link-up-function))

;;;; ---- Scope: which workspaces are recoverable at all ---------------------

(defun agent-repl--recovery-slo-exclusion (ws)
  "Return the keyword naming why WS is NOT measurable, or nil when it is.

WHY SCOPE IS PART OF THE INSTRUMENT.  The conjunction asks when a
workspace's recovery COMPLETED, and a workspace with nothing to recover
can never satisfy it: it has no page to re-adopt a snapshot and no
session to carry frames, so it is outstanding on every signal for as
long as the budget lasts and then gets forced for no reason.  Measured
live, that was the bulk of this module's output — ten breach records
with every delta unstamped, from workspaces that were hibernated, merged
away, or never had a page at all.  Those records said nothing about
recovery, and they buried the ones that did.

RECOVERABLE MEANS: the workspace had a page or a session at the moment
the outage began.  Both halves are read through the predicates that
already own those questions, never re-derived here:

  - `agent-repl--frontend-precreate-refusal' is THE eligibility answer
    for whether a workspace is owed a page at all, shared with the mount
    and the webview recovery sweep.  Its `:already-mounted' refusal is
    the one that means YES here — a workspace is refused a NEW page
    precisely because it already has one, which is the strongest
    evidence there is that something is there to recover.  Every other
    refusal (`:not-live', `:not-gui', `:merge-completed', `:open-fenced',
    `:no-xwidget') is a workspace that could not have a page now and
    could not have had one during the outage either;

  - `agent-repl--frontend-session-controller-live-p' is the drain gate
    the prompt queue rules on (`agent-repl-prompt-queue-revived-function'):
    the daemon holding a live session controller for the workspace.  A
    workspace with no page but a live session still has a wire and a
    view to recover, so it is measured.

Answering with the refusal keyword itself rather than a boolean is what
makes the drift guard possible: the reason this module records is the
reason the eligibility source gave."
  (let ((refusal (agent-repl--frontend-precreate-refusal ws)))
    (cond
     ((eq refusal :already-mounted) nil)
     (refusal refusal)
     ((agent-repl--frontend-session-controller-live-p ws) nil)
     (t :no-live-session))))

(defun agent-repl--recovery-slo-exclusion-fresh-p (ws)
  "Return non-nil when WS's non-measurement was already stated this outage."
  (let ((last (gethash ws agent-repl--recovery-slo-excluded)))
    (and last
         (< (- (float-time) last)
            (/ (float (+ agent-repl-recovery-slo-budget-ms
                         agent-repl-recovery-slo-reverify-ms))
               1000.0)))))

(defun agent-repl--recovery-slo-record-exclusion (ws reason)
  "Record once that WS is out of scope for REASON, and return non-nil if said.

NOT SILENT, and that is the whole point of this function: a reader of
the log must be able to tell `not measured' from `measured and fine',
and a workspace that simply vanished from the records is indistinguishable
from one the instrument forgot about.  The record shares the
`recovery-slo:' prefix and the ws= field of the real one so a single grep
returns the complete population, and it is logged rather than warned —
an out-of-scope workspace is a precondition, not a failure."
  (unless (agent-repl--recovery-slo-exclusion-fresh-p ws)
    (puthash ws (float-time) agent-repl--recovery-slo-excluded)
    (agent-repl--log ws "recovery-slo: ws=%s outcome=not-measured reason=%s"
                     ws (substring (symbol-name reason) 1))
    t))

;;;; ---- The conjunction ---------------------------------------------------

(defun agent-repl--recovery-slo-outstanding (attempt)
  "Return the signals ATTEMPT still lacks, in `agent-repl-recovery-slo-signals' order.
nil means the conjunction is satisfied.  Order is fixed so two warnings
about the same shortfall read identically."
  (cl-remove-if (lambda (signal)
                  (plist-get attempt (intern (format ":%s" signal))))
                agent-repl-recovery-slo-signals))

(defun agent-repl--recovery-slo-base (attempt)
  "Return the instant ATTEMPT's deltas and budget are counted from, or nil.

THE BUDGET MEASURES RECOVERY, NOT THE OUTAGE.  An attempt is armed at
the first evidence the link went away — for an ANNOUNCED restart that is
before the daemon has even died — and nothing about recovery is
answerable until the link is carrying frames again.  Counting the 3s
from the announcement therefore charges the SLO for however long the
daemon took to come back: measured live, every workspace on the host
breached with all three signals unstamped roughly three seconds into a
thirteen-second daemon restart, was force-recovered pointlessly, and
then recovered inside 700ms on the attempt the link-up backstop opened.
The fast path was never the exception — it was the SAME recovery,
measured from an instant at which it could actually be measured.

So the clock is re-based on `:answerable-at', stamped at the link-open
edge, and `:started-at' is kept for the outage span the record also
reports.  An attempt armed while the link is already up has the two
equal, which is exactly the fast path this module already proved out:
its deltas are unchanged by any of this.

nil means the link has not come back yet — the attempt is pending, and
an outage longer than the budget is not a recovery that missed it."
  (plist-get attempt :answerable-at))

(defun agent-repl--recovery-slo-outage-ms (attempt)
  "Return how long ATTEMPT waited for the link to be answerable, or -1.
-1 when the link has not come back, on the same reading as an unstamped
signal: a missing measurement, never a zero."
  (let ((base (agent-repl--recovery-slo-base attempt))
        (start (plist-get attempt :started-at)))
    (if (and base start) (round (* 1000 (- base start))) -1)))

(defun agent-repl--recovery-slo-delta-ms (attempt signal)
  "Return SIGNAL's delta in ATTEMPT in whole milliseconds, or -1 when unstamped.
-1 rather than 0 or nil: an unstamped signal is a MISSING measurement,
and a record that printed it as zero would read as the fastest possible
recovery of exactly the thing that never happened."
  (let ((at (plist-get attempt (intern (format ":%s" signal))))
        (base (agent-repl--recovery-slo-base attempt)))
    (if (and at base) (round (* 1000 (- at base))) -1)))

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
                       "wire_ms=%d total_ms=%d outage_ms=%d budget_ms=%d "
                       "forced=%s probe=%s outstanding=%s"))
         (args (list ws outcome
                     (agent-repl--recovery-slo-delta-ms attempt 'emacs)
                     (agent-repl--recovery-slo-delta-ms attempt 'webapp)
                     (agent-repl--recovery-slo-delta-ms attempt 'wire)
                     total
                     (agent-repl--recovery-slo-outage-ms attempt)
                     agent-repl-recovery-slo-budget-ms
                     (if (plist-get attempt :forced) "yes" "no")
                     ;; WHY THE PAGE SIGNAL IS MISSING, not merely that it
                     ;; is.  `webapp_ms=-1' with `probe=absent' is a stale
                     ;; deploy — a document that FINISHED loading without the
                     ;; hook; with `probe=loading' it is a page that was still
                     ;; building its document when the budget ran out; with
                     ;; `probe=silent' it is a page that never answered at all;
                     ;; with `probe=present' it is a real, measured recovery
                     ;; that has not finished.
                     (or (plist-get attempt :probe) "silent")
                     (if outstanding
                         (mapconcat #'symbol-name outstanding ",")
                       "none"))))
    (if outstanding
        (apply #'agent-repl--warn ws line args)
      (apply #'agent-repl--log ws line args))
    total))

;;;; ---- Opening and stamping ----------------------------------------------

(defun agent-repl--recovery-slo-open (ws &optional at)
  "Open WS's recovery attempt, dated AT (`float-time'; nil means now).

ONE WORKSPACE, ONE IN-FLIGHT BUDGET, STARTED AT THE FIRST EVIDENCE THE
LINK WENT AWAY — see this file's commentary.  A workspace that already
has an attempt open does NOT get a second one: the existing attempt is
kept with every stamp it has collected, and only its `:started-at' can
change, and only ever BACKWARDS, when AT proves the outage began earlier
than the evidence that armed it.  A later piece of evidence about the
same outage therefore cannot shorten a budget that is already running,
and neither arming path can reset the other's clock.

Returns non-nil when this call opened a NEW attempt."
  (let ((existing (gethash ws agent-repl--recovery-slo-attempts))
        (at (or at (float-time))))
    (cond
     ((null existing)
      (puthash ws (append (list :started-at at)
                          ;; Armed while the link is already carrying frames:
                          ;; recovery is answerable from this same instant, so
                          ;; the two anchors coincide and the deltas are the
                          ;; ones the fast path already reports.
                          (when (agent-repl--recovery-slo-link-up-p)
                            (list :answerable-at at)))
              agent-repl--recovery-slo-attempts)
      (agent-repl--log-verbose ws "recovery-slo: attempt opened ws=%s budget_ms=%d"
                               ws agent-repl-recovery-slo-budget-ms)
      t)
     ((< at (plist-get existing :started-at))
      (agent-repl--log-verbose
       ws "recovery-slo: attempt ws=%s start moved back by %dms — earlier evidence"
       ws (round (* 1000 (- (plist-get existing :started-at) at))))
      (puthash ws (plist-put existing :started-at at)
               agent-repl--recovery-slo-attempts)
      nil)
     (t
      (agent-repl--log-verbose
       ws "recovery-slo: attempt ws=%s already open — evidence kept, clock unchanged" ws)
      nil))))

(defun agent-repl--recovery-slo-open-all (at reason)
  "Open an attempt dated AT for every live workspace, arming the tick.
REASON names the evidence in the log.  Shared by every arming path so the
one-budget-per-workspace rule cannot drift between them — two copies of
this loop is exactly how one of them quietly starts double-arming."
  (let ((names (agent-repl--live-ws-names)))
    (agent-repl--log-verbose nil "recovery-slo: arming reason=%s workspaces=%d"
                             reason (length names))
    (dolist (ws names)
      ;; SCOPE FIRST.  A workspace with nothing to recover is not armed at
      ;; all — it could only breach by construction — but it is never
      ;; silently dropped either: see
      ;; `agent-repl--recovery-slo-record-exclusion'.  An attempt already
      ;; open is left alone regardless, because the scope question was
      ;; answered when the outage began and re-answering it mid-outage
      ;; would read a workspace's TORN-DOWN state as proof it never had
      ;; anything to recover.
      (let ((exclusion (and (null (gethash ws agent-repl--recovery-slo-attempts))
                            (agent-repl--recovery-slo-exclusion ws))))
        (if exclusion
            (agent-repl--recovery-slo-record-exclusion ws exclusion)
          (agent-repl--recovery-slo-open ws at))))
    (when (> (hash-table-count agent-repl--recovery-slo-attempts) 0)
      (agent-repl--recovery-slo-arm))))

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
measures.

THE REPLY REPORTS WHETHER THE HOOK EXISTS, SEPARATELY FROM WHAT IT SAID.
A page running a bundle that predates the probe and a page whose probe
answers `not yet recovered' are two completely different failures — one
is a stale deploy, the other is a slow recovery — and the record used to
print both as `webapp_ms=-1' with nothing to tell them apart.  `present'
is therefore its own field, computed from `typeof' rather than from the
report being non-empty, so an absent hook is a fact the reply carries
rather than something inferred from silence.

AND THE DOCUMENT'S OWN LOADING STATE RIDES BACK WITH IT, because without
it a MISSING hook has two completely different meanings that the record
printed identically.  Measured on this host by driving `document.readyState'
beside `typeof window.agentReplRecoveryProbe' into a live webview across a
re-navigation: for the first ~150ms the page reports
readyState=\"interactive\" with the hook `undefined', and from then on
readyState=\"complete\" with the hook a `function'.  A page the host just
re-navigated is therefore GUARANTEED to look hookless for a beat, and
calling that a stale bundle is a lie about the one thing `absent' exists
to say.  So `rs' comes back and the reader distinguishes them."
  (format "(function(){var h=window.%s;var p=(typeof h===\"function\");\
var r=(p?h():\"\");return JSON.stringify({ws:%s,present:p,rs:document.readyState,report:r});})()"
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
      (when (and (stringp ws) (not (string-empty-p ws)))
        ;; THE PAGE ANSWERED, so whatever it said is now a recorded fact
        ;; about the probe rather than an absence.  Recorded BEFORE the
        ;; satisfaction test: a page carrying no hook can never satisfy the
        ;; signal, and `probe=absent' is the only thing that distinguishes
        ;; that stale deploy from a page that simply is not back yet.
        (agent-repl--recovery-slo-note-probe
         ws (agent-repl--recovery-slo-probe-state envelope))
        (when (agent-repl--recovery-slo-probe-satisfied-p report)
          (agent-repl--recovery-slo-note ws 'webapp))))))

(defun agent-repl--recovery-slo-probe-state (envelope)
  "Return the `probe=' state ENVELOPE, one page reply, proves.

THE VERDICT IS NEVER LATCHED ON A PAGE THAT HAD NOT FINISHED LOADING.
A document the host re-navigated has no globals for the first beat of
its life, so `present' false ALONE says nothing about which bundle the
page runs; it is `absent' only once the document says it is
`complete', which is the state in which a missing hook really is a
bundle older than webapp/src/recovery-probe.ts.  Anything else — a
document still parsing, or one whose readyState this reply did not
carry — is `loading', which ranks BELOW `absent' so the very next tick
inside the budget can still overwrite it with the truth."
  (cond
   ((eq t (alist-get 'present envelope)) "present")
   ((equal (alist-get 'rs envelope) "complete") "absent")
   (t "loading")))

(defconst agent-repl-recovery-slo-probe-states '("silent" "loading" "absent" "present")
  "Every value the record's `probe=' field can take, worst first.
Strings, not symbols, because every value in
`agent-repl--recovery-slo-attempts' is a scalar the record prints — see
that table's own docstring for the crash behind that invariant.
`silent' is the initial state of an attempt: the page was asked and has
said nothing at all, which is what a webview that never boots — or one
whose evaluation never completes at all, as a page mid-navigation's does
not — looks like.  `loading' is a page that ANSWERED and was still
building its document, so it has no globals yet and nothing about its
bundle is known.  `absent' is a page that answered, finished loading,
and carries no probe hook — a running bundle older than
webapp/src/recovery-probe.ts, and the only genuine stale-bundle signal.
`present' is a page whose probe answered, whether or not it was satisfied
yet.")

(defun agent-repl--recovery-slo-note-probe (ws state)
  "Record STATE as WS's probe presence, never downgrading what was seen.

MONOTONIC ALONG `agent-repl-recovery-slo-probe-states', because the
question the field answers is `did this page EVER prove it carries the
probe', and a page that answered once and was then re-navigated must not
have that proof erased by the silence that follows.  A workspace with no
open attempt is ignored, exactly as a signal stamp is."
  (let ((attempt (gethash ws agent-repl--recovery-slo-attempts)))
    (when attempt
      (let* ((current (or (plist-get attempt :probe) "silent"))
             (rank (lambda (s) (or (cl-position s agent-repl-recovery-slo-probe-states
                                                :test #'equal)
                                   0))))
        (when (> (funcall rank state) (funcall rank current))
          (puthash ws (plist-put attempt :probe state)
                   agent-repl--recovery-slo-attempts))))))

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
the host-driven webview repair fixes the PAGE
(`agent-repl--webview-recovery-repair-workspace'), and
`agent-repl--frontend-ensure-workspace' repairs the SESSION through the
ordinary ensure/reattach path.  Neither is a new recovery mechanism, and
deliberately so: a second one would drift from the first.

IT REPAIRS THIS WORKSPACE'S PAGE AND NOBODY ELSE'S, which is a defect fix
rather than tidiness.  This used to drive the whole-host sweep past its
debounce, so a bounce in which several workspaces breached issued one
FULL sweep per breach — and every sweep re-navigated every page whose
bundle did not match the deployed one, throwing away documents that were
mid-boot from the previous force and resetting the page-side recovery
epoch each time.  Live records show the result: a host-wide
`webapp_ms=-1' with `probe=absent' or `probe=silent'.  The build-id
comparison inside the repair is unchanged — a page already on the
deployed bundle is DRIVEN in place through the recovery hook, and only a
genuinely stale bundle is re-navigated — so the force no longer destroys
the very page it is measuring, nor anyone else's.

Each half is guarded on its own, so a failure in one still leaves the
other driven, and the failure is warned about rather than swallowed."
  (condition-case err
      (agent-repl--webview-recovery-repair-workspace ws "recovery_slo_force")
    (error (agent-repl--warn ws "recovery-slo: ws=%s force repair failed err=%S" ws err)))
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
what is still missing.

IT DOES NOT POLL THE PAGE HERE, AND THAT IS THE WHOLE FIX.  It used to
call `agent-repl--recovery-slo-poll-webapp' and then re-read the attempt
on the next line, on the belief that the poll's callback `may have
stamped the page in place'.  It cannot have: the reply crosses the
xwidget boundary as an INPUT EVENT, so it is delivered by the command
loop strictly after this function returns, and the webapp signal was
therefore unreadable by construction at exactly the moment the forced
outcome was decided.  Live records show the consequence — every
`forced-unrecovered' carried `webapp_ms=-1' whatever the page was doing.
The polling now belongs to the tick alone (`agent-repl--recovery-slo-check'),
which keeps asking a forced attempt, so each reply lands in the attempt
before a later tick or this re-verification reads it."
  (let ((attempt (gethash ws agent-repl--recovery-slo-attempts)))
    (when attempt
      (let* ((outcome (if (agent-repl--recovery-slo-outstanding attempt)
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
     ;; A FORCED ATTEMPT IS STILL POLLED.  It must not be breached a second
     ;; time — that is what would turn one outage into an unbounded force
     ;; loop — but its re-verification is a READ, and the only thing that
     ;; produces the page's answer to read is a poll issued at least one
     ;; command-loop turn earlier.  Stopping the polling here is what left
     ;; every forced outcome blind to the page.
     ((plist-get attempt :forced)
      (agent-repl--recovery-slo-poll-webapp ws)
      'pending)
     ;; The link has not come back yet, so there is nothing to ask and
     ;; nothing to rule on: no page can have re-adopted a snapshot that has
     ;; not been sent.  Breaching here would report the DAEMON's downtime as
     ;; this workspace's recovery failure and force it against a socket that
     ;; does not exist — which is exactly what the live records showed.
     ((null (agent-repl--recovery-slo-base attempt)) 'pending)
     (t
      (agent-repl--recovery-slo-poll-webapp ws)
      (let* ((attempt (gethash ws agent-repl--recovery-slo-attempts))
             (elapsed-ms (round (* 1000 (- (float-time)
                                           (agent-repl--recovery-slo-base
                                            attempt))))))
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

(defun agent-repl--recovery-slo-on-restart-announcement (armed-at)
  "Arm every live workspace because an expected-restart window opened at ARMED-AT.

Subscriber for `agent-repl-frontend-expected-restart-armed-functions',
which covers BOTH kinds of announcement — the daemon's own
(`agent-repl-frontend-note-restart-announcement') and the deploy-initiated
one — because both open the same window and both are the same outage.

ARMED-AT IS THE WINDOW'S INSTANT, NOT THIS FUNCTION'S.  An announced
restart's clock starts when the outage begins, and dating it from the
moment the announcement finished being decoded and dispatched would
quietly hand the SLO back however long that took."
  (agent-repl--recovery-slo-open-all armed-at "restart-announcement"))

(defun agent-repl--recovery-slo-on-link-down ()
  "Arm every live workspace because an ESTABLISHED link just dropped.
Called from the UDS sentinel's down transition (lisp/frontend-uds.el).
Dated NOW, which is the drop.  An unannounced drop has no announcement to
arm from and must still be measured; an announced one reaches here too and
is absorbed by the one-budget rule in `agent-repl--recovery-slo-open'."
  (agent-repl--recovery-slo-open-all (float-time) "link-down"))

(defun agent-repl--recovery-slo-on-link-open ()
  "Mark every open attempt answerable: the link is carrying frames again.

Subscriber for `agent-repl-uds-connected-functions', the sentinel's OPEN
transition — deliberately that edge and not the snapshot-applied one,
because the emacs signal is stamped DURING the apply and an anchor set
after it would date that stamp before the clock it belongs to.

FIRST OPEN WINS, per attempt.  A reconnect ladder that opens, drops and
opens again inside one outage must not keep pushing the instant recovery
became answerable forward — the workspace has been trying to recover
since the first of them."
  (let ((now (float-time)))
    (dolist (ws (hash-table-keys agent-repl--recovery-slo-attempts))
      (let ((attempt (gethash ws agent-repl--recovery-slo-attempts)))
        (unless (plist-get attempt :answerable-at)
          (puthash ws (plist-put attempt :answerable-at now)
                   agent-repl--recovery-slo-attempts))))))

(defun agent-repl--recovery-slo-on-link-up ()
  "Arm any live workspace still without an attempt when the link comes back.
Subscriber for `agent-repl-uds-snapshot-applied-functions' — the same
edge the host webview sweep runs on, so the SLO measures exactly the
recovery that sweep is trying to perform.

THE BACKSTOP, NOT THE PRIMARY EDGE.  A workspace armed only here has
already missed the emacs stamp of the snapshot whose application ran this
hook, so it could not satisfy the conjunction from that reconnect; the
announcement and down-edge paths are what put the attempt in place first.
This one still runs so a workspace with neither piece of evidence
attributed to it is measured rather than ignored, and it cannot disturb
an attempt already open."
  (agent-repl--recovery-slo-open-all (float-time) "link-up"))

(add-hook 'agent-repl-frontend-expected-restart-armed-functions
          #'agent-repl--recovery-slo-on-restart-announcement)

;; The answerability anchor, on the sentinel's own open edge — BEFORE the
;; snapshot lands, so the emacs stamp that the apply produces belongs to a
;; clock that is already running.
(add-hook 'agent-repl-uds-connected-functions
          #'agent-repl--recovery-slo-on-link-open)

;; ARMED AFTER the webview sweep's own subscriber, so the attempt a
;; workspace opens is measured against a sweep that has already been issued
;; rather than one still queued behind it.
(add-hook 'agent-repl-uds-snapshot-applied-functions
          #'agent-repl--recovery-slo-on-link-up t)

(provide 'recovery-slo)
;;; recovery-slo.el ends here
