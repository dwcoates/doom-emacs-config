;;; host.el --- the agentrepl.v1 HOST section, from the editor's seat -*- lexical-binding: t; -*-

;;; Commentary:

;; EMACS IS A HOST, NOT AN AUTHOR.  Its whole workspace contribution is
;; two verbs — REGISTER a directory (the daemon mints the identity) and
;; SELECT a workspace (the user switched tabs) — plus one subscription per
;; open workspace.  Everything else about a workspace (parentage, branch,
;; repo, naming, status) the daemon derives and pushes.  This file is that
;; seat: register, select, subscribe, unsubscribe, adopt, and the FIXED
;; TREATMENTS Emacs draws per pushed arm.
;;
;; FIXED TREATMENTS, NEVER VALUE MAPPING.  Every consumer below switches on
;; the ARM and nothing else.  The composer gate IS the resolved arm; the
;; naming precedence is title, then slug, then the roster row name; the
;; notification policy is three cases and no fourth.  There is no composed
;; sentence anywhere and no derived state: a push whole-replaces what came
;; before it.
;;
;; THE NOTIFICATION POLICY IS EMACS'S ALONE, because Emacs alone holds the
;; knowledge.  The daemon publishes the FACT and never asks whether Emacs
;; is focused; `endpoint_watch_host_workspace.proto' states the policy at
;; the `notification' arm and this file implements exactly it:
;;
;;   Emacs unfocused           → the OS desktop banner, whose click raises
;;                               the frame and selects this workspace's tab
;;   focused, tab not selected → blink the tab-bar entry, per THE CANONICAL
;;                               BLINK CADENCE on frontend.v1
;;                               `RosterRowAttention'
;;   tab selected              → nothing (the footer's activity line has it)
;;
;; The attention marker's own lifecycle is the DAEMON's: it is set when the
;; notification fires and cleared by SelectWorkspace, so an ordinary tab
;; switch is the acknowledgement and no ack verb exists.
;;
;; REFS ARE ECHO TOKENS.  A `WorkspaceRef' comes from RegisterWorkspace's
;; success or from the roster and is echoed verbatim; nothing here builds
;; one from a path, and `dir' is never used as a key.

;;; Code:

;; Every log call is a log-site macro (log-site.el), needed where this
;; file is expanded; config.el loads it first.
(require 'log-site)

;; Cross-file forward declarations.  These sources load in the dependency
;; order config.el establishes and resolve each other's calls at call time,
;; so the declarations below exist for the byte-compiler alone.
(declare-function agent-repl--input-buffer-name "core")
(declare-function agent-repl-roster-interaction-standing-p "roster" (ws))
(declare-function agent-repl-roster-open-entry-named "roster" (name))
(declare-function agent-repl--input-selection-changed "input" (ws))
(declare-function agent-repl-link-successor-pending-p "daemon-link")
(declare-function agent-repl-link-successor-at "daemon-link")
(declare-function agent-repl-link-on-successor-accepted "daemon-link")

(require 'cl-lib)
(require 'subr-x)


(declare-function agent-repl-connect-stream-cancel "connect" (stream))
(declare-function agent-repl-connect-refused-arm "connect" (detail))
(declare-function agent-repl-connect-refusal-address "connect" (detail))
(declare-function agent-repl-connect-connection-alive-p "connect" (conn))

(declare-function agent-repl-rpc-register-workspace "rpc" (conn request &rest keys))
(declare-function agent-repl-rpc-register-workspaces "rpc" (conn request &rest keys))
(declare-function agent-repl-rpc-select-workspace "rpc" (conn request &rest keys))
(declare-function agent-repl-rpc-report-user-interaction "rpc" (conn request &rest keys))
(declare-function agent-repl-rpc-report-page-unloading "rpc" (conn request &rest keys))
(declare-function agent-repl-page-report-restate "page-report" (ws))
(declare-function agent-repl-rpc-adopt-host-workspace "rpc" (conn request &rest keys))
(declare-function agent-repl-rpc-answer-editor-evaluation "rpc" (conn request &rest keys))
(declare-function agent-repl-evaluate-text "evaluate" (text))
(declare-function agent-repl-link-dial-successor "daemon-link" (address))
(declare-function agent-repl-connect-connection-address "connect" (conn))
(declare-function agent-repl-connect-connection-at-p "connect" (conn address))
(declare-function agent-repl-connect-failure-unreached-p "connect" (detail))
(declare-function agent-repl--with-log-context "core" (workspace request-id function))
(declare-function agent-repl-rpc-watch-host-workspace "rpc"
                  (conn ref on-push on-close &optional on-open))

(declare-function agent-repl-link-primary "daemon-link" ())
(declare-function agent-repl-link-live "daemon-link" ())
(declare-function agent-repl-link-promotions "daemon-link" ())
(declare-function agent-repl-link-successor "daemon-link" ())
(declare-function agent-repl-link-serving-conn "daemon-link" (address))
(declare-function agent-repl-link-departing-p "daemon-link" (live &optional ended-planned-on))
(defvar agent-repl-link-down-planned)
(defvar agent-repl-link-promote-functions)

(declare-function agent-repl--ws-get "workspace" (ws key))
(declare-function agent-repl--ws-put "workspace" (ws key val))
(declare-function agent-repl--ws-switch "workspace" (ws &rest args))
(declare-function agent-repl--ws-current-name "workspace" ())
(declare-function agent-repl--live-ws-names "workspace" ())
(declare-function agent-repl--ws-by-ref-id "workspace" (id))
(declare-function agent-repl--ws-add-activated-hook "workspace" (fn))
(defvar agent-repl--eager-open-in-progress)
(defvar agent-repl--global-log-scope)
(defvar agent-repl-roster-following)

(declare-function agent-repl--notification-activate "notifications" (ref))
(declare-function agent-repl-input-focus-for-reply "input" (ws))
(declare-function agent-repl-input-text-highlighted "input" (ws))
(declare-function agent-repl-input-compose-entry "input" (ws label))
(declare-function agent-repl-input-response-question "input" (ws change))

;; W2-B's surfaces.  NAMED HERE, NEVER DEFINED HERE: status.el owns the
;; blink (whose cadence is frontend.v1 `RosterRowAttention''s), frontend.el
;; owns the webview, popup.el owns the one shared editor popup.
(declare-function agent-repl-webhost-downtime-start "webhost" (ws))
(declare-function agent-repl-latency-report-editor "latency-report" (kind seconds))
(declare-function agent-repl--now-unix-ms "core" ())
(declare-function agent-repl-frontend-reload-webapp "frontend" (ws build))
(declare-function agent-repl-popup-open "popup" (path &optional line))
(declare-function agent-repl-magit-log-commit "magit" (ws dir branch commit))
(declare-function agent-repl-terminal-run "terminal" (ws command))

;;;; ---- State ----

(defvar agent-repl-host--by-name (make-hash-table :test 'equal)
  "Workspace name → plist `(:ref REF :conn CONN :stream STREAM :host HOST)'.
REF is the daemon-minted `WorkspaceRef' plist, CONN the connection whose
daemon currently owns the workspace, STREAM its `WatchHostWorkspace'
subscription, HOST the last decoded `HostWorkspace' push (nil until the
first one arrives).")

(defvar agent-repl-host-last-selected-id nil
  "The `WorkspaceRef' id of the last workspace Emacs itself selected.
Recorded when the daemon acknowledged it, when a leaving daemon answered
`standing_down', when nobody answered at all, and when there was no
daemon to send it to: in every case it is the
selection the next link-up re-asserts (`agent-repl-host--selected-dir').
A refusal records nothing, because the daemon said it stamped nothing.")

(defvar agent-repl-host-reselect-pending nil
  "The project dir Emacs is re-asserting as the user's selection, or nil.
EMACS OWNS THE USER'S SELECTION ACROSS A DAEMON RESTART.  A relaunched
daemon has no memory of `current' — `endpoint_watch_workspace_roster.proto'
carries `current' as the daemon's stamp, and `endpoint_select_workspace.proto'
is the only verb that sets it — so the first thing it stamps is whatever
Emacs re-registered first, which is an arbitrary order and not a choice
the user made.  This is set for the length of a re-registration and
cleared when the re-select is acknowledged; while it stands,
`agent-repl-roster-apply-current' does not move the frame, so a roster
push carrying that arbitrary `current' cannot drag the user onto the
other workspace's panel.")

(defvar agent-repl-host-update-functions nil
  "Functions run with (WS HOST-PLIST) after every `host' push for WS.")

(defvar agent-repl-host-reattached-functions nil
  "Functions run with (WS) once WS is re-attached to a daemon.
Runs from `agent-repl-host--reattached', the end of the one reattach walk,
whatever started it: a transfer, a lost stream, a dead connection, a
promotion, a link-up.  From here on `agent-repl-host-conn' names the
daemon that serves WS, so work held for WS while it had none may go.")

;;;; ---- Accessors ----

(defun agent-repl-host--entry (ws)
  "Return the bookkeeping plist for workspace WS, or nil."
  (gethash ws agent-repl-host--by-name))

(defun agent-repl-host--held-name (id)
  "Return the name of the workspace this editor holds whose ref id is ID, or nil.
Read from the host table (`agent-repl-host--by-name'), the one record of
which workspaces hold a host stream here: a workspace the roster does not
mark visible (frontend.v1.RosterRowVisibility) has no entry, so it answers nil."
  (let ((found nil))
    (maphash (lambda (name entry)
               (when (and (null found) (equal (plist-get (plist-get entry :ref) :id) id))
                 (setq found name)))
             agent-repl-host--by-name)
    found))

(defun agent-repl-host--put (ws key value)
  "Set KEY to VALUE in workspace WS's bookkeeping plist."
  (puthash ws (plist-put (or (gethash ws agent-repl-host--by-name) (list))
                         key value)
           agent-repl-host--by-name))

(defun agent-repl-host-ref (ws)
  "Return the daemon-minted `WorkspaceRef' plist for WS, or nil.
The ref is an ECHO TOKEN: it is obtained from RegisterWorkspace or the
roster and handed back verbatim.  Nil means the workspace has not been
registered on the current daemon yet."
  (plist-get (agent-repl-host--entry ws) :ref))

(defun agent-repl-host-take-ref (ws ref)
  "Record REF as workspace WS's daemon-minted `WorkspaceRef', in both its homes.
The ref WS echoes on every per-workspace call lives in TWO places: the
workspace plist (`agent-repl--ws-get' WS `:ref'), which the roster and
every module that holds a workspace name reads, and this file's host
entry (`agent-repl-host-ref'), which every daemon verb echoes.  This is
THE ONE WRITER of both, so neither copy can fall behind the other.

Two writers once diverged: after a rename moved a workspace's directory
\(`agent-repl--ws-move-dir'), the roster wrote the new ref onto the plist
alone, the host entry kept the ref naming the old directory, and on
2026-11-25 a handover's adopt echoed that stale ref and the daemon
refused it (`workspace_ref_mismatch') for two workspaces, leaving both
unadopted until a promotion re-registered them.

The host entry is written only when WS has one: a workspace whose host
stream was never started has no entry, and `agent-repl-host--attach'
writes it through here when the stream starts.  An entry whose ref
changes is recorded once, with the directory it named before."
  (agent-repl--ws-put ws :ref ref)
  (when-let* ((entry (agent-repl-host--entry ws)))
    (let ((was (plist-get entry :ref)))
      (when (and was (not (equal was ref)))
        (agent-repl--info ws "elisp.host.ref-taken ws={ws} id={id} dir={dir} was-dir={was_dir}"
                          :ws ws :id (prin1-to-string (plist-get ref :id))
                          :dir (prin1-to-string (plist-get ref :dir))
                          :was_dir (prin1-to-string (plist-get was :dir)))))
    (agent-repl-host--put ws :ref ref)))

(defun agent-repl-host--conn-id (conn)
  "Return a short identity for CONN, telling connections at one address apart.
Every daemon of a handover serves the one shared address, so the address
names no connection; this identity does, in the records of the walks and
the dead-connection check.  Nil CONN answers \"none\"."
  (if conn (format "%x" (sxhash-eq conn)) "none"))

(defun agent-repl-host--recorded-conn (ws)
  "Return the connection recorded for WS, dead or alive, or nil.
This file\='s own bookkeeping reads it; every consumer that is about to
send asks `agent-repl-host-conn', which never answers a dead one."
  (plist-get (agent-repl-host--entry ws) :conn))

(defun agent-repl-host-conn (ws)
  "Return the connection whose LIVE daemon serves WS, or nil.
THE ONE RESOLUTION every per-workspace call, subscription and page URL is
addressed by, so none of them can disagree about which daemon serves WS.
During a blue-green handover the two daemons own different workspaces,
and this is the one that owns THIS workspace.  Nil when WS has none yet.

A CONNECTION KNOWN DEAD IS NEVER ANSWERED.  Regression, 2026-09-27: a
daemon exited without transferring a workspace, the link was promoted
onto its successor, and every SelectWorkspace and ReportUserInteraction for
that workspace kept going to the dead daemon\='s address
\(`elisp.connect.call-on-closed-connection') while its webview stayed on
the dead origin.  A dead recorded connection starts WS\='s reattach onto
the live daemon (`agent-repl-host--follow-live-daemon'), and the answer is
the live daemon -- or nil when no link stands, which every caller already
treats as having no connection."
  (let ((conn (agent-repl-host--recorded-conn ws)))
    (if (or (null conn) (agent-repl-connect-connection-alive-p conn))
        conn
      (agent-repl--info ws "elisp.host.dead-connection ws={ws} recorded={recorded} address={address} live={live}"
                        :ws ws :recorded (agent-repl-host--conn-id conn) :address (prin1-to-string (agent-repl-connect-connection-address conn)) :live (agent-repl-host--conn-id (agent-repl-link-live)))
      (agent-repl-host--follow-live-daemon ws "dead-connection")
      (let ((now (agent-repl-host--recorded-conn ws)))
        (if (and now (agent-repl-connect-connection-alive-p now))
            now
          (agent-repl-link-live))))))

(defun agent-repl-host-detached-p (ws)
  "Non-nil while WS has lost its daemon and no walk has re-attached it yet.
Set by the walks that find WS without a daemon
\(`agent-repl-host--follow-live-daemon', a failed registration) and
cleared only by a finished walk (`agent-repl-host--reattached')."
  (plist-get (agent-repl-host--entry ws) :detached))

(defun agent-repl-host-page-conn (ws)
  "Return the connection WS\='s page is loaded from, or nil.
WS\='s own (`agent-repl-host-conn') while it has one.  A DETACHED WORKSPACE
LOADS ITS PAGE FROM THE LIVE LINK (`agent-repl-link-live'), the daemon its
pending walk is re-attaching it to: the ref\='s id is the durable one the
registry keeps, so the page\='s URL there is the URL it has once the walk
lands, and the walk\='s own reload (`agent-repl-host--register-settled')
reloads it in place.  Regression, 2026-11-03: a switch to a detached
workspace selected it and loaded no page, every switch recording
`elisp.frontend.webview-url: no connection'.  Nil only when no link
stands at all."
  (or (agent-repl-host-conn ws)
      (and (agent-repl-host-detached-p ws)
           (let ((live (agent-repl-link-live)))
             (when live
               (agent-repl--info ws "elisp.host.page-conn-from-live-link ws={ws} address={address} walk={walk}"
                                 :ws ws :address (prin1-to-string (agent-repl-connect-connection-address live))
                                 :walk (if (plist-get (agent-repl-host--entry ws) :reattach)
                                     "in-flight" "none")))
             live))))

(defun agent-repl-host-conn-or-primary (ws)
  "Return the connection a per-workspace call for WS goes out on, or nil.
WS\='s own serving connection (`agent-repl-host-conn') when it has one, and
otherwise the primary link -- a workspace not yet attached to a daemon is
still addressed on the link every registration rides.  A nil WS (no
current workspace) is addressed on the primary link alone.  Nil only when
neither stands, which every caller treats as having no connection."
  (or (and ws (agent-repl-host-conn ws)) (agent-repl-link-primary)))

(defun agent-repl-host-stream (ws)
  "Return WS's standing `WatchHostWorkspace' stream, or nil."
  (plist-get (agent-repl-host--entry ws) :stream))

(defun agent-repl-host-state (ws)
  "Return the last decoded `HostWorkspace' pushed for WS, or nil."
  (plist-get (agent-repl-host--entry ws) :host))

(defun agent-repl-host-session-id (ws)
  "Return WS's daemon-minted agent-repl session id, or nil.
The id is an echo token published on the `existing' session arm.  Nil is the
ordinary state before the first host push and when WS has no session."
  ;; Called while every workspace log record is being constructed.  Logging
  ;; here would recurse, and this path can fire more than once per second.
  (let* ((host (agent-repl-host-state ws))
         (session (plist-get host :session)))
    (when (eq (plist-get session :arm) :existing)
      (let ((id (plist-get (plist-get (plist-get session :value) :id) :value)))
        (unless (equal id "") id)))))

(defun agent-repl-host--live (ws)
  "Return WS's `HostSessionLive' plist, or nil when the session is not live.
The one place the session tree is walked: `session' oneof → `existing' →
`standing' oneof → `live'.  Every accessor below reads through here so no
two of them disagree about what live means."
  (let* ((host (agent-repl-host-state ws))
         (session (plist-get host :session)))
    (when (eq (plist-get session :arm) :existing)
      (let ((standing (plist-get (plist-get session :value) :standing)))
        (when (eq (plist-get standing :arm) :live)
          (plist-get standing :value))))))

(defun agent-repl-host-standing-ask (ws)
  "Return WS's standing `HostAsk' plist off the last host push, or nil.
The root feed's oldest open question or permission card, as the daemon
projects it for an answer by number: `(:row FEEDID :kind (:arm
:question|:permission :value V))'.  Nil is the ordinary state whenever no
such card stands, and before the first push."
  (plist-get (agent-repl-host-state ws) :ask))

(defun agent-repl-host-unanswered-entries (ws)
  "Return the labels of WS's composer entries that answer nothing, or nil.
As the daemon read the entries the composer last reported
\(`HostComposerEntries'), in the composer's order.  Nil before the first
push and whenever every reported label targets a response."
  (plist-get (plist-get (agent-repl-host-state ws) :composer-entries) :unanswered))

(defun agent-repl-host-backfill (ws)
  "Return WS's backfill arm keyword, or nil.
One of `:none', `:pending', `:done', `:failed'.  Nil when there is no
live session — backfill lives on the LIVE arm and nowhere else."
  (plist-get (plist-get (agent-repl-host--live ws) :backfill) :arm))

(defun agent-repl-host-faults (ws)
  "Return WS's standing `HostFault' plists, or nil.
Generation-scoped: a fault window dies with its generation, which is why
these live on the live arm alone."
  (plist-get (agent-repl-host--live ws) :faults))

(defun agent-repl-host-generation (ws)
  "Return WS's live generation id string, or nil when the session is not live.
The generation ROLLS whenever the shim is relaunched, so a changed value
is the one fact that says a restart\='s bounce actually happened."
  (plist-get (plist-get (agent-repl-host--live ws) :generation) :value))

(defun agent-repl-host--live-composer-arm (ws)
  "Return the RAW composer arm off WS\='s live push, or nil.
Unvetted on purpose: this is the arm as pushed, with no vocabulary check
and no local hold folded in, so the settle below can read what the daemon
actually said without logging a second breach for the gate\='s own read."
  (plist-get (plist-get (agent-repl-host--live ws) :composer) :arm))

(defun agent-repl-host-vendor-session-id (ws)
  "Return WS's VENDOR conversation id, or nil.

This is the durable id — the claude session uuid a resume replays — and
it is read off the live arm's `vendor_info' oneof, which is the only
place the daemon publishes it to Emacs.  It is NOT
`agent-repl-host-state\='s session `id', which is the daemon-minted echo
token that rotates under one workspace.

Nil is the ORDINARY early state, not a failure: the oneof stays unset
until a vendor conversation exists (the wire decoder reads an absent arm
as nil), and a workspace with no live session has no live arm at all.

Only the `claude' arm carries an id; any other vendor arm that lands
later answers nil here until it is threaded through deliberately."
  (let ((vendor (plist-get (agent-repl-host--live ws) :vendor-info)))
    (when (eq (plist-get vendor :arm) :claude)
      (plist-get (plist-get vendor :value) :session-id))))

(defun agent-repl-host--pushed-composer-gate (ws)
  "Return the composer gate WS's last host push spells, with no local hold.

`:open' `:merging' `:draining' `:restarting' come straight
off the LIVE arm's `composer' oneof — the resolved arm IS the gate.  The
other standings are blocked by their own nature and answer `:no-session'
or `:terminal'; `:unknown' means no host push has arrived yet."
  (let* ((host (agent-repl-host-state ws))
         (session (plist-get host :session)))
    (cond
     ((null host)
      (agent-repl--log ws "elisp.host.gate ws={ws} gate=unknown reason=no-push" :ws ws)
      :unknown)
     ((eq (plist-get session :arm) :none) :no-session)
     ((eq (plist-get session :arm) :existing)
      (let ((standing (plist-get (plist-get session :value) :standing)))
        (pcase (plist-get standing :arm)
          (:terminal :terminal)
          (:live
           (let ((arm (plist-get (plist-get (plist-get standing :value) :composer) :arm)))
             (if (memq arm '(:open :merging :draining :restarting))
                 arm
               (agent-repl--error ws "elisp.host.gate-unknown-composer-arm ws={ws} arm={arm}" :ws ws :arm (prin1-to-string arm))
               :unknown)))
          (arm
           (agent-repl--error ws "elisp.host.gate-unknown-standing ws={ws} arm={arm}" :ws ws :arm (prin1-to-string arm))
           :unknown))))
     (t
      (agent-repl--error ws "elisp.host.gate-unset-session ws={ws} host={host}" :ws ws :host (prin1-to-string host))
      :unknown))))

(defun agent-repl-host-take-restart-hold (ws)
  "Close WS's composer LOCALLY from the instant a restart is sent.

THE SEND IS THE EDGE, NOT THE PUSH.  RestartWorkspace is sent
asynchronously and answers success once the daemon has taken the work on;
the bounce it schedules -- prelaunch, stand-down, reap, relaunch -- runs
after that answer again.  The `restarting' composer arm therefore arrives
on the WatchHostWorkspace stream some time LATER, over a different stream
than the one that carried the ack, and nothing orders the two.  A caller
that reads the gate the instant it asked for a restart reads the arm the
PREVIOUS generation left there -- `:open' -- and a prompt submitted on the
strength of that reading is refused a few hundred milliseconds later when
the real `:restarting' lands.  That is exactly the race a headless run of
the real editor hit.

So the hold makes the local fact structural: the accepted restart CLOSES
the gate here, and nothing reopens it but the daemon.  The generation
current at the moment of the hold is recorded because it is what the
release below compares against.

The hold is taken for EVERY restart: a restart is always immediate, so
the bounce is always coming."
  (let ((generation (agent-repl-host-generation ws)))
    (agent-repl-host--put ws :restart-hold (list :generation generation))
    (agent-repl--info ws "elisp.host.restart-hold-taken ws={ws} generation={generation}"
                      :ws ws :generation (or generation "-"))))

(defun agent-repl-host-release-restart-hold (ws reason)
  "Drop WS's local restart hold, recording REASON for why it ended."
  (when (plist-get (agent-repl-host--entry ws) :restart-hold)
    (agent-repl-host--put ws :restart-hold nil)
    (agent-repl--info ws "elisp.host.restart-hold-released ws={ws} reason={reason}"
                      :ws ws :reason reason)))

(defun agent-repl-host--settle-restart-hold (ws)
  "Release WS's restart hold once the DAEMON's own push resolves it.

Two edges end the hold, and a restart is guaranteed to produce one
of them: the daemon publishes `restarting' -- it now owns the fact, and
the pushed arm is the gate again -- or the live generation ROLLS, which is
the relaunched shim reporting itself and means the bounce is already over.
Neither edge can be missed by a push that coalesces them, because a rolled
generation is read off the same state as the arm."
  (let ((hold (plist-get (agent-repl-host--entry ws) :restart-hold)))
    (when hold
      (let ((arm (agent-repl-host--live-composer-arm ws))
            (generation (agent-repl-host-generation ws)))
        (cond
         ((eq arm :restarting)
          (agent-repl-host-release-restart-hold ws "daemon-says-restarting"))
         ((and generation (not (equal generation (plist-get hold :generation))))
          (agent-repl-host-release-restart-hold ws "generation-rolled")))))))

(defun agent-repl-host-composer-gate (ws)
  "Return the composer gate for WS as one keyword of the fixed vocabulary.

The pushed arm IS the gate, save for one local fact the daemon has not
had time to publish yet: an accepted restart holds the gate at
`:restarting' until the daemon's own push resolves it (see
`agent-repl-host-take-restart-hold').

The gate is ADVISORY about intent, never a precondition: `:no-session',
`:terminal' and `:unknown' all still SEND (ruled -- SubmitPrompt has no
precondition, and the daemon starts or revives the session implicitly),
while `:merging', `:draining' and `:restarting' are refusals input.el
draws.  Emacs enforces it because the composer is host-native."
  (if (plist-get (agent-repl-host--entry ws) :restart-hold)
      :restarting
    (agent-repl-host--pushed-composer-gate ws)))

(defun agent-repl-host-display-title (ws)
  "Return the name WS's buffers are titled from.
Precedence, fixed: the vendor-supplied `naming.title', else the
daemon-derived `naming.slug', else WS itself — which IS the roster row's
name, since tabs are named from the row (§8).  Both naming fields are
unset until the daemon derives them, so the fallback is the ordinary
early state and not a failure."
  (let ((naming (plist-get (agent-repl-host-state ws) :naming)))
    (or (plist-get naming :title)
        (plist-get naming :slug)
        ws)))

;;;; ---- Register ----

(defun agent-repl-host-register (conn dir on-done &optional workspace on-refused)
  "Register DIR with the daemon on CONN; call ON-DONE with the minted ref.
IDEMPOTENT BY DIR — re-registering after a reconnect or a daemon restart
is the normal path, never an error.  A daemon-authored error arm and a
transport failure are different facts and are logged as such; both answer
ON-DONE with nil so the caller never waits on a callback that will not come.
WORKSPACE names an existing workspace being re-registered.  Its absence means
DIR is being registered before any workspace identity exists.

ON-REFUSED, when given, OWNS a daemon-authored refusal: it is called with
the decoded `RegisterWorkspaceError' INSTEAD of ON-DONE, to tell the user
why, and the refusal is recorded at INFO -- an answer the caller reports,
not a fault.  Without it a refusal is recorded at ERROR, since nobody is
there to say it."
  (let ((log-scope (if workspace
                       workspace
                     '(:agent-repl-central
                       "initial registration precedes workspace ownership"))))
    (agent-repl--info log-scope "elisp.host.register dir={dir}" :dir (prin1-to-string dir))
    (agent-repl-rpc-register-workspace
     conn (list :dir dir)
     :on-response
     (lambda (response)
       (pcase (plist-get response :arm)
         (:success
          (funcall on-done (agent-repl-host--record-registration
                            log-scope dir :registered (plist-get response :value) workspace)))
         (:error
          (if on-refused
              (progn
                (agent-repl--info log-scope "elisp.host.register-refused dir={dir} error={error}"
                                  :dir (prin1-to-string dir) :error (prin1-to-string (plist-get response :value)))
                (funcall on-refused (plist-get response :value)))
            (funcall on-done (agent-repl-host--record-registration
                              log-scope dir :refused (plist-get response :value) workspace))))
         (arm
          (agent-repl--error log-scope "elisp.host.register-unknown-arm dir={dir} arm={arm}" :dir (prin1-to-string dir) :arm (prin1-to-string arm))
          (funcall on-done nil))))
     :on-failure
     (lambda (detail)
       (funcall on-done (agent-repl-host--record-registration log-scope dir :failed detail))))))

(defun agent-repl-host--record-registration (scope dir arm value &optional ws)
  "Record on SCOPE how DIR\='s registration ended; answer its ref, or nil.
ARM is `:registered' (VALUE the decoded `RegisterWorkspaceSuccess'),
`:refused' (VALUE the daemon\='s `RegisterWorkspaceError') or `:failed'
\(VALUE what broke).
A registration of WS refused `transferring_away' is HANDOVER NEWS, not a
failure: the daemon handed WS to the successor it names, so it is
recorded at INFO and handed to `agent-repl-host-handle-refusal', which
walks WS onto that successor; it still answers nil, since this daemon
registered nothing.
THE ONE RECORD of a registration\='s fate, a lone RegisterWorkspace
\(`agent-repl-host-register') and each dir of a RegisterWorkspaces
\(`agent-repl-host--outcome-ref') alike, so a reader asks for one
registration\='s outcome the same way however it was sent."
  (pcase arm
    (:registered
     (let ((ref (plist-get value :workspace)))
       (agent-repl--info scope "elisp.host.registered dir={dir} id={id}"
                         :dir (prin1-to-string dir) :id (prin1-to-string (plist-get ref :id)))
       ref))
    (:refused
     (let ((cause (plist-get value :cause)))
       (if (and ws (eq (plist-get cause :arm) :transferring-away))
           (progn
             (agent-repl--info scope "elisp.host.register-transferring-away dir={dir} fields={fields}"
                               :dir (prin1-to-string dir) :fields (prin1-to-string (plist-get cause :value)))
             (agent-repl-host-handle-refusal ws cause))
         (agent-repl--error scope "elisp.host.register-refused dir={dir} error={error}" :dir (prin1-to-string dir) :error (prin1-to-string value))))
     nil)
    (:failed
     (agent-repl--error scope "elisp.host.register-failed dir={dir} detail={detail}" :dir (prin1-to-string dir) :detail (prin1-to-string value))
     nil)
    (_ (agent-repl--fatal scope "elisp.host.record-registration-unknown-arm dir={dir} arm={arm}"
                          :dir (prin1-to-string dir) :arm (prin1-to-string arm)))))

(defun agent-repl-host-register-all (conn dirs on-done)
  "Register every dir of DIRS on CONN in ONE call; call ON-DONE with the outcomes.
ON-DONE receives the decoded `RegisterWorkspacesOutcome' plists, one per
dir in DIRS\=' order, or a list of nil, one per dir, when the call failed
in transport (recorded here at ERROR).  Each outcome is read by
`agent-repl-host--outcome-ref', which records it on its workspace.

THE CALL ARMS NO UNARY TIMER.  The daemon registers the dirs one after
another, so the answer\='s length scales with how many workspaces Emacs
holds, and the 10s budget a single call has would be a bound on the
workspace count rather than on a fault.  A dead daemon still ends it:
the connection\='s close is a transport failure, and the link\='s down and
up edges walk every workspace again.

AN ANSWER THAT DOES NOT PAIR WITH DIRS IS A BROKEN CONTRACT, and fails
hard: one outcome per dir, in order, each echoing its dir."
  (let ((scope '(:agent-repl-central "a batched registration spans every workspace")))
    (agent-repl--info scope "elisp.host.register-all dirs={dirs} address={address}"
                      :dirs (length dirs) :address (prin1-to-string (agent-repl-connect-connection-address conn)))
    (agent-repl-rpc-register-workspaces
     conn (list :dirs dirs)
     :timeout :none
     :on-response
     (lambda (response)
       (let ((outcomes (plist-get response :outcomes)))
         (unless (and (= (length outcomes) (length dirs))
                      (cl-every (lambda (outcome dir) (equal (plist-get outcome :dir) dir))
                                outcomes dirs))
           (agent-repl--fatal scope "elisp.host.register-all-unpaired dirs={dirs} outcomes={outcomes}"
                              :dirs (prin1-to-string dirs) :outcomes (prin1-to-string outcomes)))
         (agent-repl--info scope "elisp.host.registered-all dirs={dirs}" :dirs (length dirs))
         (funcall on-done outcomes)))
     :on-failure
     (lambda (detail)
       (agent-repl--error scope "elisp.host.register-all-failed dirs={dirs} address={address} detail={detail}"
                          :dirs (length dirs) :address (prin1-to-string (agent-repl-connect-connection-address conn)) :detail (prin1-to-string detail))
       (funcall on-done (make-list (length dirs) nil))))))

(defun agent-repl-host--outcome-ref (ws dir outcome)
  "Return the ref OUTCOME registered WS\='s DIR as, or nil; record it on WS.
OUTCOME is one `RegisterWorkspacesOutcome' plist, or nil when the batch
failed in transport (already recorded by `agent-repl-host-register-all').
The record is `agent-repl-host--record-registration\='s, a lone
registration\='s own."
  (let* ((arm (plist-get outcome :outcome))
         (value (plist-get arm :value)))
    (pcase (plist-get arm :arm)
      ('nil nil)
      (:registered
       (agent-repl-host--record-registration ws dir :registered value ws))
      (:refused
       (agent-repl-host--record-registration ws dir :refused value ws))
      (:failed
       (agent-repl-host--record-registration ws dir :failed (plist-get value :cause)))
      (other
       (agent-repl--fatal ws "elisp.host.register-unknown-arm dir={dir} arm={arm}" :dir (prin1-to-string dir) :arm (prin1-to-string other))))))

(defconst agent-repl-host--handover-arms '(:transferring-away :not-yet-adopted)
  "Refusal arms that are HANDOVER NEWS rather than user-facing failures.
Both are declared on every per-workspace rpc's error type, so one list
serves them all; they are handed to `agent-repl-host-handle-refusal' and
nothing is drawn for either.")

(defcustom agent-repl-host-handover-retry-delay 0.2
  "Seconds before a `not_yet_adopted' refusal re-walks the adopt.
The refusing daemon is the very one still finishing its takeover, so the
retry is SCHEDULED rather than issued from inside the answer: a straight
re-adopt from the response handler would retry as fast as the round trip
allows for as long as the takeover lasts."
  :type 'number
  :group 'agent-repl)

(defvar agent-repl-host--reselect-on-reattach nil
  "The workspace whose selection a handover refused, or nil.
Its `not_yet_adopted' named the daemon that serves it, so WS is re-attached
there, and the selection is sent again when that walk finishes
\(`agent-repl-host--reattached').")

(defun agent-repl-host-serving-address (arm-plist)
  "Return the daemon address ARM-PLIST\='s `not_yet_adopted' names, or nil.
The arm names the daemon that serves the workspace until the joining
daemon adopts it; nil when it names none, or is another arm."
  (and (eq (plist-get arm-plist :arm) :not-yet-adopted)
       (let ((address (plist-get (plist-get arm-plist :value) :address)))
         (and (stringp address) (not (string-empty-p address)) address))))

(defun agent-repl-host-view-where-served (ws address trigger)
  "Re-attach WS to the daemon at ADDRESS, which serves it during a handover.
A joining daemon refused WS `not_yet_adopted' and named ADDRESS as the
daemon serving it meanwhile, so WS is registered there (idempotent by
dir) and its stream, page and calls follow: viewing WS never waits for the
handover.  The incumbent\='s own `transferred' push, later, re-homes WS onto
the successor through the adopt walk.  TRIGGER is recorded.  Answers
non-nil when a walk was started, nil when WS already stands there."
  (let ((conn (agent-repl-link-serving-conn address))
        (current (agent-repl-host-conn ws)))
    (if (and (agent-repl-connect-connection-at-p current address)
             (agent-repl-connect-connection-alive-p current))
        (progn
          (agent-repl--info ws "elisp.host.served-here-already ws={ws} address={address} trigger={trigger}"
                            :ws ws :address (prin1-to-string address) :trigger trigger)
          nil)
      (agent-repl--info ws "elisp.host.view-where-served ws={ws} address={address} trigger={trigger}"
                        :ws ws :address (prin1-to-string address) :trigger trigger)
      (agent-repl-host--reattach ws conn :register trigger)
      t)))

(defun agent-repl-host--on-refused (ws op value)
  "Report a daemon-authored refusal VALUE of OP for WS, or route the handover.
THE SLUG NAMES THE RPC (`elisp.host.<op>-refused'), so a reader can ask
for every refusal of one verb.  The two handover arms are not refusals of
the verb at all — they are the daemon telling Emacs where the workspace
went — so they are INFO and go to `agent-repl-host-handle-refusal', the
one place the handover walk lives."
  (let* ((arm (plist-get value :cause))
         (keyword (plist-get arm :arm)))
    (cond
     ((memq keyword agent-repl-host--handover-arms)
      (agent-repl--info ws "elisp.host.{op}-handover-refusal ws={ws} arm={arm} fields={fields}" :op op
                        :ws ws :arm (prin1-to-string keyword) :fields (prin1-to-string (plist-get arm :value)))
      ;; A SELECTION A HANDOVER REFUSED IS STILL THE USER\='S: it is sent
      ;; again once WS stands on the daemon that serves it.  Marked BEFORE
      ;; the routing below, whose walk may finish at once.
      (when (and (equal op "select") (agent-repl-host-serving-address arm))
        (setq agent-repl-host--reselect-on-reattach ws))
      ;; A `not_yet_adopted' NAMING THE DAEMON THAT SERVES WS is routed at
      ;; once: WS is viewed there, so nothing waits on the takeover.  One
      ;; naming none is the takeover still settling, retried after a pause.
      (if (and (eq keyword :not-yet-adopted)
               (not (agent-repl-host-serving-address arm)))
          (run-at-time agent-repl-host-handover-retry-delay nil
                       #'agent-repl-host-handle-refusal ws arm)
        (agent-repl-host-handle-refusal ws arm)))
     ;; THE INCUMBENT KEPT WS, and nothing changes until it is free: the
     ;; successor refused the mid-work adoption and the incumbent took it back.
     ;; WS stays on the incumbent (the adopt walk already put `:conn' and the
     ;; webview back) and NOTHING is scheduled -- the incumbent's next
     ;; `transferred' push walks the adopt again.  Retrying it as a
     ;; `not_yet_adopted' flipped the webview between the two daemons about
     ;; three times a second for minutes (2026-10-11).
     ((eq keyword :transfer-deferred)
      (agent-repl--info ws "elisp.host.{op}-transfer-deferred ws={ws}" :op op :ws ws))
     (t
      (agent-repl--error ws "elisp.host.{op}-refused ws={ws} error={error}" :op op
                         :ws ws :error (prin1-to-string value))))))

;;;; ---- Select ----

;; THE DAEMON IS THE ONE PLACE A WORKSPACE SWITCH HAPPENS.  Every switch
;; trigger Emacs owns -- the adjacent and slot chords, the pickers, a
;; notification click -- REQUESTS the switch here
;; (`agent-repl-host-request-switch') and moves nothing; the frame moves only
;; when the roster's `current' names a workspace other than the one shown
;; (`agent-repl-roster-apply-current'), exactly as a sidebar click does.
;;
;; Regression, 2026-10-02: Emacs used to switch first and select after, and
;; the roster handler skipped every `current' it took for "Emacs's own" -- a
;; list that grew to every workspace visited, so a sidebar click to any of
;; them was silently ignored.  With Emacs never leading there is nothing of
;; its own to recognize, and that bookkeeping is gone; watch for it
;; returning.

(defvar agent-repl-host--select-in-flight nil
  "The `WorkspaceRef' whose SelectWorkspace awaits its answer, or nil.
A REF, NEVER A NAME: a workspace this editor does not hold (a folded or
backlogged one, frontend.v1.RosterRowVisibility) is selected through the
same one queue as a held one, and it has no name here until its tab opens.
EMACS SENDS ONE SELECTION AT A TIME.  Every unary call rides its own
socket, so two selects sent back to back -- `s-}' pressed twice -- reach
the daemon in whatever order its accepts are scheduled, and the older one
could land LAST, leaving `current' on a workspace the user had already
walked past.  With one in flight, the daemon receives Emacs's selections
in the order the user made them, and the last press is the last stamp.")

(defvar agent-repl-host--select-queued nil
  "The newest selection waiting behind the one in flight, or nil.
A list (REF ON-SETTLED KEEP-HISTORY SWITCH-STARTED-MS), the arguments of the
`agent-repl-host-select-ref' it will be sent as.  Only the newest is kept: a
selection the user has already moved past is never sent, because the daemon
would stamp it `current' only for the next one to overwrite it.  The one it
replaces is settled `:superseded' so its caller is not left waiting.")

(defvar agent-repl-host--confirming nil
  "A selection the daemon accepted whose roster has not arrived, or nil.
A cons (REF . REVISION): the daemon answered REF's SelectWorkspace with the
roster revision the selection is in (agentrepl.v1
SelectWorkspaceSuccess.roster_revision).  THE ANSWER AND THE ROSTER RIDE
SEPARATE STREAMS, so the roster Emacs holds when the answer lands can be
one from before the selection; followed, it moved the frame back to the
workspace the user had left, and the next push moved it forward again
\(measured 2026-10-16: three passes per switch).  While this stands REF is
the pending selection (`agent-repl-host-pending-selection'), so such a
roster is deferred; `agent-repl-roster-apply-current' releases it once the
roster it holds is at REVISION or later.")

(declare-function agent-repl-roster-apply-current "roster" ())
(declare-function agent-repl-roster-move-frame "roster" (name))
(declare-function agent-repl-webhost-has-page-p "webhost" (ws))
(declare-function agent-repl-webhost-show "webhost" (ws))
(declare-function agent-repl-webhost-note-switch "webhost" (ws))
(declare-function agent-repl--frontend-fit-webviews "frontend" (&optional frame))
(declare-function agent-repl-open-tabs-holds-p "open-tabs" (id))
(declare-function agent-repl-link-up-p "daemon-link" ())
(declare-function agent-repl-page-unload-load-requested "page-unload" (ws))

(defun agent-repl-host--target-scope (ref)
  "Return the log scope of a selection of REF: its workspace\='s name when held.
A workspace this editor does not hold has no sink of its own here, so its
records go to the central sink, naming its id in the message."
  (or (agent-repl-host--held-name (plist-get ref :id))
      '(:agent-repl-central "a selection of a workspace this editor does not hold has no workspace sink")))

(defun agent-repl-host-pending-selection-ref ()
  "Return the `WorkspaceRef' most recently REQUESTED and not yet answered, or nil.
The queued selection when there is one, else the one in flight.
`agent-repl-roster-apply-current' defers any `current' naming another
workspace until it is answered.  An answered selection whose roster has
not arrived is pending too (`agent-repl-host--confirming')."
  (or (car agent-repl-host--select-queued)
      agent-repl-host--select-in-flight
      (car agent-repl-host--confirming)))

(defun agent-repl-host-pending-selection ()
  "Return the name of the workspace pending selection, or nil.
The workspace `agent-repl-host-pending-selection-ref' names, when this
editor holds it: adjacent keys step from it while it stands, so rapid
presses walk before the roster catches up.  Nil when nothing is pending,
or when the pending workspace is one this editor does not hold yet."
  (let ((ref (agent-repl-host-pending-selection-ref)))
    (and ref (agent-repl-host--held-name (plist-get ref :id)))))

(defun agent-repl-host-release-confirmed (revision)
  "Release the answered selection once the roster held is at REVISION.
REVISION is the revision of the roster Emacs holds; the selection awaiting
its roster (`agent-repl-host--confirming') is released when REVISION has
reached the one its answer named."
  (when (and agent-repl-host--confirming
             (>= revision (cdr agent-repl-host--confirming)))
    (let ((ref (car agent-repl-host--confirming)))
      (agent-repl--log (agent-repl-host--target-scope ref)
                       "elisp.host.select-confirmed id={id} revision={revision} awaited={awaited}"
                       :id (plist-get ref :id) :revision revision :awaited (cdr agent-repl-host--confirming)))
    (setq agent-repl-host--confirming nil)))

(defun agent-repl-host-forget-confirming (reason)
  "Stop awaiting an answered selection's roster, for REASON.
The roster stream that would have carried it has ended, and the next
daemon's revisions start afresh: awaiting one of this daemon's would hold
the selection pending forever."
  (when agent-repl-host--confirming
    (let ((ref (car agent-repl-host--confirming)))
      (agent-repl--info (agent-repl-host--target-scope ref)
                        "elisp.host.select-unconfirmed id={id} awaited={awaited} reason={reason}"
                        :id (plist-get ref :id) :awaited (cdr agent-repl-host--confirming) :reason reason))
    (setq agent-repl-host--confirming nil)))

(defun agent-repl-host-select (ws &optional on-settled keep-history switch-started-ms)
  "Tell the daemon the user switched to workspace WS.
KEEP-HISTORY non-nil marks the switch as a step of the recency walk
\(`agent-repl-switch-older'): the daemon stamps `current' and leaves the
row's last-selected instant standing (agentrepl.v1
SelectWorkspaceRequest.keep_selection_history).
SWITCH-STARTED-MS, when given, is the Unix-millisecond instant the user's
switch began (`agent-repl-host--count-editor-switch'); the daemon hands it
to the sidebar with the selection, which measures its paint from it
\(agentrepl.v1 SelectWorkspaceRequest.switch_started_at_ms).
ON-SETTLED, when given, is called with `:success', `:error',
`:standing-down' (the daemon is leaving or already gone: the next daemon
is asked again) or `:failure' once the call has an outcome
— the one moment a caller holding state on the selection\='s behalf
\(the link-up re-assertion) may let go of it, whichever way it went —
and with `:superseded' when a
newer selection replaced it before it was sent.
Idempotent by contract — re-selecting the current workspace succeeds —
and the daemon's own act of stamping `current' also CLEARS the
workspace's attention marker, which is why no ack verb exists.

While another selection is in flight this one WAITS
\(`agent-repl-host--select-in-flight'), replacing any older one already
waiting, and is sent when the one in flight is answered.  Answers t when
the selection was sent or queued, nil when it was not sent: WS has no ref
yet (an unregistered workspace has no identity to select, and it is
skipped), or there is no connection to send it on (it is recorded for the
next link-up to re-assert, and settled `:standing-down').

A SELECTION OF A DETACHED WORKSPACE STARTS ITS WALK onto the live daemon
\(`agent-repl-host--follow-live-daemon'), so a workspace the user turns to
is never left without one: selection is one of the three edges a failed
walk is armed on (`agent-repl-host--register-settled')."
  (when (agent-repl-host-detached-p ws)
    (agent-repl-host--follow-live-daemon ws "select"))
  (let ((ref (agent-repl-host-ref ws)))
    (if (null ref)
        (progn
          (agent-repl--log ws "elisp.host.select-skipped ws={ws} reason=no-ref" :ws ws)
          nil)
      (agent-repl-host-select-ref ref on-settled keep-history switch-started-ms))))

(defun agent-repl-host-select-ref (ref &optional on-settled keep-history switch-started-ms)
  "Tell the daemon the user switched to the workspace REF names.
`agent-repl-host-select', by the workspace\='s identity: the one way to
select a workspace this editor does not hold (frontend.v1.RosterRowVisibility),
whose roster push then opens it and lands the frame on it
\(`agent-repl-roster-apply-current').  ON-SETTLED, KEEP-HISTORY and
SWITCH-STARTED-MS are as `agent-repl-host-select' documents; so is the one
selection in flight, behind which this one waits."
  (if agent-repl-host--select-in-flight
      (let ((older agent-repl-host--select-queued))
        (setq agent-repl-host--select-queued (list ref on-settled keep-history switch-started-ms))
        (agent-repl--info (agent-repl-host--target-scope ref)
                          "elisp.host.select-queued id={id} behind={behind} superseded={superseded}"
                          :id (plist-get ref :id) :behind (plist-get agent-repl-host--select-in-flight :id) :superseded (or (plist-get (car older) :id) "none"))
        (when (and older (cadr older))
          (funcall (cadr older) :superseded))
        t)
    (agent-repl-host--send-select ref on-settled keep-history switch-started-ms)))

(defun agent-repl-host--select-conn (ref)
  "Return the connection REF\='s selection is sent on, or nil.
A held workspace\='s own connection (`agent-repl-host-conn-or-primary');
for one this editor does not hold, the primary link, the daemon whose
roster will open it."
  (let ((ws (agent-repl-host--held-name (plist-get ref :id))))
    (if ws
        (agent-repl-host-conn-or-primary ws)
      (and (fboundp 'agent-repl-link-primary) (agent-repl-link-primary)))))

(defun agent-repl-host--send-select (ref on-settled keep-history switch-started-ms)
  "Send REF\='s SelectWorkspace now, holding the in-flight slot until it answers.
ON-SETTLED, KEEP-HISTORY and SWITCH-STARTED-MS are as
`agent-repl-host-select' documents."
  (let ((conn (agent-repl-host--select-conn ref))
        (scope (agent-repl-host--target-scope ref)))
    (cond
     ((null conn)
      ;; A SELECTION WITH NO DAEMON TO SEND IT TO IS STILL THE USER'S, as
      ;; one nobody answered is (`agent-repl-host--select-failed'): it is
      ;; recorded as the selection the next link-up re-asserts, and its
      ;; caller is settled.  Dropping it lost the user's newest choice
      ;; whenever the link went down between a select and the one queued
      ;; behind it: the drained queued select found no connection, the
      ;; in-flight one's standing-down answer was the last id recorded, and
      ;; the next daemon was told the selection the user had already left
      ;; (2026-10-14, e2e TestEmacsRestartKeepsTheSelectedWorkspace).
      (setq agent-repl-host-last-selected-id (plist-get ref :id))
      (agent-repl--info scope "elisp.host.select-deferred id={id} reason=no-connection reassert=next-link-up"
                        :id (prin1-to-string (plist-get ref :id)))
      (when on-settled (funcall on-settled :standing-down))
      nil)
     (t
      (agent-repl--info scope "elisp.host.select id={id} keep-history={keep_history} started-ms={started_ms}"
                        :id (prin1-to-string (plist-get ref :id)) :keep_history (and keep-history t) :started_ms switch-started-ms)
      (setq agent-repl-host--select-in-flight ref)
      (condition-case err
          (agent-repl-rpc-select-workspace
           conn (append (list :workspace ref)
                        (and keep-history (list :keep-selection-history t))
                        (and switch-started-ms (list :switch-started-at-ms switch-started-ms)))
           :on-response
           (lambda (response)
             (agent-repl-host--select-settled
              ref (lambda () (agent-repl-host--select-answered ref response on-settled))))
           :on-failure
           (lambda (detail)
             (agent-repl-host--select-settled
              ref (lambda () (agent-repl-host--select-failed ref detail on-settled)))))
        (error
         ;; The call never went out, so nothing will ever answer it: the
         ;; slot is released here or no later selection is ever sent.
         (setq agent-repl-host--select-in-flight nil)
         (agent-repl--error scope "elisp.host.select-unsent id={id} error={error}"
                            :id (prin1-to-string (plist-get ref :id)) :error (prin1-to-string err))
         (signal (car err) (cdr err))))
      t))))

(defun agent-repl-host--select-settled (ref handle)
  "Release REF\='s in-flight selection slot, run HANDLE, then drain.
The slot is released FIRST, so a selection HANDLE itself starts (a
handover\='s re-attach selects the current workspace) is sent at once
rather than queued behind an answer that has already arrived.  The
queued selection, if any, is sent next.  When nothing is left pending,
the roster's `current' is applied again: a push that arrived while the
selection was pending was DEFERRED (`agent-repl-roster-apply-current'),
and this is the moment it is judged -- the last push wins."
  (setq agent-repl-host--select-in-flight nil)
  (agent-repl--log (agent-repl-host--target-scope ref) "elisp.host.select-settled id={id} queued={queued}"
                   :id (plist-get ref :id) :queued (or (plist-get (car agent-repl-host--select-queued) :id) "none"))
  (unwind-protect
      (funcall handle)
    (if agent-repl-host--select-queued
        (let ((next agent-repl-host--select-queued))
          (setq agent-repl-host--select-queued nil)
          (apply #'agent-repl-host-select-ref next))
      (agent-repl-roster-apply-current))))

(defun agent-repl-host--select-failed (ref detail on-settled)
  "Act on the selection of REF failing with DETAIL before any answer.
ON-SETTLED is as `agent-repl-host-select' documents.

A SELECTION NOBODY ANSWERED IS STILL THE USER\='S, so it is ALWAYS recorded
as the selection the next link-up re-asserts
\(`agent-repl-host--selected-dir'), whatever the link\='s state when the
failure lands.  A daemon stopping under the call answers it with no
response, or with the HTTP layer\='s 503, before any handler can say
`standing_down', and that failure can land before OR after the link-down
edge -- deciding by the link\='s state left the user\='s choice to that
order, and the dead daemon\='s last `current' won whenever the failure came
first (2026-10-07, e2e TestEmacsRestartKeepsTheSelectedWorkspace).  The
link\='s state decides only how the failure is told: with the link down the
daemon is gone and the loss is expected (INFO, settled `:standing-down'),
with it up the failure is the error it is (ERROR, settled `:failure')."
  (setq agent-repl-host-last-selected-id (plist-get ref :id))
  (if (agent-repl-link-up-p)
      (progn
        (agent-repl--error (agent-repl-host--target-scope ref)
                           "elisp.host.select-failed id={id} reassert=next-link-up detail={detail}"
                           :id (prin1-to-string (plist-get ref :id)) :detail (prin1-to-string detail))
        (when on-settled (funcall on-settled :failure)))
    (agent-repl--info (agent-repl-host--target-scope ref)
                      "elisp.host.select-daemon-gone id={id} reassert=next-daemon detail={detail}"
                      :id (prin1-to-string (plist-get ref :id)) :detail (prin1-to-string detail))
    (when on-settled (funcall on-settled :standing-down))))

(defun agent-repl-host--select-answered (ref response on-settled)
  "Act on the daemon\='s RESPONSE to the selection of REF.
ON-SETTLED is as `agent-repl-host-select' documents."
  (let ((ws (agent-repl-host--held-name (plist-get ref :id)))
        (scope (agent-repl-host--target-scope ref)))
  (pcase (plist-get response :arm)
    (:success
     ;; RECORDED ONLY ON THE ACK.  `agent-repl-host-last-selected-id' is
     ;; what Emacs believes the daemon stamped as `current'; a refusal is
     ;; the daemon saying it stamped nothing, and recording the id anyway
     ;; would leave Emacs disagreeing with the daemon about which
     ;; workspace is current.
     (setq agent-repl-host-last-selected-id (plist-get ref :id))
     ;; THE ROSTER CARRYING THE SELECTION IS AWAITED, or one resolved before
     ;; it would move the frame back (`agent-repl-host--confirming').
     (setq agent-repl-host--confirming
           (cons ref (plist-get (plist-get response :value) :roster-revision)))
     (agent-repl--log scope "elisp.host.selected id={id} roster-revision={roster_revision}"
                      :id (prin1-to-string (plist-get ref :id)) :roster_revision (cdr agent-repl-host--confirming))
     (when on-settled (funcall on-settled :success)))
    ((and :error
          (guard (eq (plist-get (plist-get (plist-get response :value) :cause) :arm)
                     :standing-down)))
     ;; A DAEMON THAT IS LEAVING STAMPED THE SELECTION and started no
     ;; session for it.  It is an expected answer, not a refusal: the id is
     ;; recorded as Emacs's selection exactly as an ack records it, because
     ;; the daemon did stamp it, and that is what the next link-up
     ;; re-asserts on the daemon that serves next
     ;; (`agent-repl-host--selected-dir', `agent-repl-host--reassert-selection'),
     ;; which revives the workspace.
     (setq agent-repl-host-last-selected-id (plist-get ref :id))
     (agent-repl--info scope "elisp.host.select-standing-down id={id} reassert=next-daemon"
                       :id (prin1-to-string (plist-get ref :id)))
     (when on-settled (funcall on-settled :standing-down)))
    ((and :error (guard ws))
     (agent-repl-host--on-refused ws "select" (plist-get response :value))
     (when on-settled (funcall on-settled :error)))
    (:error
     (agent-repl-host--unheld-select-refused ref (plist-get response :value))
     (when on-settled (funcall on-settled :error)))
    (arm
     (agent-repl--error scope "elisp.host.select-unknown-arm id={id} arm={arm}" :id (prin1-to-string (plist-get ref :id)) :arm (prin1-to-string arm))
     (when on-settled (funcall on-settled :error))))))

(defun agent-repl-host--unheld-select-refused (ref value)
  "Report the daemon refusing the selection of REF, a workspace not held here.
VALUE is the decoded error.  A held workspace\='s refusal walks its handover
\(`agent-repl-host--on-refused'); one this editor does not hold has no
stream to walk, so its refusal is told: in the echo area, and at ERROR, or
at INFO for a handover arm, the daemon leaving under the click.  The user
picks the workspace again on the daemon that serves now."
  (let* ((cause (plist-get value :cause))
         (keyword (plist-get cause :arm))
         (handover (memq keyword agent-repl-host--handover-arms))
         (name (or (plist-get ref :dir) (plist-get ref :id))))
    (if handover
        (agent-repl--info '(:agent-repl-central "a selection of a workspace this editor does not hold has no workspace sink")
                          "elisp.host.select-unheld-handover id={id} arm={arm} fields={fields}"
                          :id (prin1-to-string (plist-get ref :id)) :arm (prin1-to-string keyword) :fields (prin1-to-string (plist-get cause :value)))
      (agent-repl--error '(:agent-repl-central "a selection of a workspace this editor does not hold has no workspace sink")
                         "elisp.host.select-unheld-refused id={id} arm={arm} fields={fields}"
                         :id (prin1-to-string (plist-get ref :id)) :arm (prin1-to-string keyword) :fields (prin1-to-string (plist-get cause :value))))
    (message "[agent-repl] The switch to %s did not happen (%s): pick it again"
             name (if handover "the daemon was handing over" keyword))))

(defvar agent-repl--recency-walk-landing)
(defvar agent-repl--recency-walk-origin)
(declare-function agent-repl--end-recency-walk "agent-repl-workspace" (log-ws reason))

(defvar agent-repl-host-last-request nil
  "The newest switch request, as (WS . FLOAT-TIME), or nil.
Read by the roster's follow record, which states how long the frame took
to arrive after the request: the end-to-end latency of a keyboard switch,
from one log line.")

(defvar agent-repl-host--editor-switches 0
  "How many workspace switches the user has made IN EMACS, ever.
Only grows.  A landing on a workspace the user asked Emacs to make
snapshots it when the gesture is made, and lands only if it has not moved
since (`agent-repl-verbs-select-minted'): a user who switched away while
the workspace was being minted is not dragged back to it.

ONLY EMACS COUNTS.  A switch an agent made reaches Emacs as the roster's
`current' and moves the frame as a FOLLOW
\(`agent-repl-roster-following'), which is never counted; neither is a
transient background activation (`agent-repl--eager-open-in-progress') nor
a move Emacs makes on its own account (`agent-repl-host-automatic-switch').
What is counted is every switch request a gesture of the user\='s makes
\(`agent-repl-host-request-switch') -- a key, a picker, a banner click, or
a sidebar row\='s click, which the daemon relays to Emacs
\(`agent-repl-host-select-requested') -- and every perspective activation
an Emacs command made directly (`agent-repl-host--on-workspace-activated').")

(defvar agent-repl-host-automatic-switch nil
  "Non-nil while Emacs moves the frame on its own account, not the user\='s.
Bound around the landing on a freshly minted workspace and the link-up
re-select, whose activations must not count as a switch the user made
\(`agent-repl-host--editor-switches').")

(defun agent-repl-host-editor-switch-count ()
  "Return how many workspace switches the user has made in Emacs.
See `agent-repl-host--editor-switches'."
  agent-repl-host--editor-switches)

(defun agent-repl-host--count-editor-switch (ws origin)
  "Count a workspace switch to WS the user made in Emacs, through ORIGIN.
ORIGIN is the request trigger or `activation', for the record.  Answers
the switch's instant in Unix milliseconds, nil when WS is nil: the
caller hands it to the switch's select, so the sidebar measures the
switch from the same instant the page does."
  (setq agent-repl-host--editor-switches (1+ agent-repl-host--editor-switches))
  ;; The switch's own instant rides the webhost's show, so the page measures
  ;; the switch the user felt (webhost.el).
  (let ((started (and ws (agent-repl-webhost-note-switch ws))))
    (agent-repl--info (or ws '(:agent-repl-central "a switch to a perspective without a workspace has no workspace sink"))
                      "elisp.host.editor-switch ws={ws} origin={origin} count={count} started-ms={started_ms}"
                      :ws ws :origin origin :count agent-repl-host--editor-switches :started_ms started)
    started))

(defun agent-repl-host-request-switch (ws trigger)
  "Ask the daemon to make WS the current workspace, and move the frame now.
TRIGGER names what the user did (a symbol such as `cycle', `slot',
`picker', `notification'), for the record.

THE FRAME MOVES ON THE REQUEST, not after the daemon's answer and roster
push, which took some 50ms of every switch (2026-10-15 switch-bench).  The
roster stays the selection's one source: while the request is pending
every push is deferred (`agent-repl-roster-apply-current'), and its answer
re-applies the roster, which moves the frame back if the daemon refused.
A workspace with no tab yet is not moved to here; the roster's follow
lands on it once its tab opens.

Every trigger but `startup' is a switch the user made in Emacs and is
counted (`agent-repl-host--editor-switches'); the startup\='s selection of
its first tab is Emacs's own.

THE `recency-walk' TRIGGER KEEPS THE SELECTION HISTORY: a step of the walk
\(`agent-repl-switch-older', `agent-repl-switch-newer') is not a choice, so
the daemon leaves the last-selected instant standing, and the walk\='s first
step records where it started (`agent-repl--recency-walk-origin'), which
leads the recency order while the walk runs.  Every other trigger is a
choice, and ends any walk the frame stood in (`agent-repl--end-recency-walk').

A request that cannot be sent is LOUD: a switch the user asked for that
goes nowhere is reported in the echo area and at WARN, never dropped.
Answers t when the request was sent or queued, nil otherwise."
  (let ((unheld (and (null (agent-repl-host-ref ws))
                     (fboundp 'agent-repl-roster-open-entry-named)
                     (agent-repl-roster-open-entry-named ws))))
    (if unheld
        ;; A WORKSPACE THIS EDITOR HOLDS NO TAB FOR (a fold hides it, so it is
        ;; not visible) is selected by its ref: the daemon draws it once it is
        ;; the selection, and the roster's follow lands the frame on it.
        (agent-repl-host-request-switch-ref (plist-get unheld :ref) trigger ws)
      (agent-repl-host--request-switch-held ws trigger))))

(defun agent-repl-host--note-walk (ws trigger shown)
  "Record a switch to WS by TRIGGER against the recency walk, from SHOWN.
Set BEFORE the frame moves: the move's own activation reads it.  A walk\='s
first step records where it started, and every step where it lands; any
other trigger is a choice and ends the walk (`agent-repl--end-recency-walk')."
  (if (eq trigger 'recency-walk)
      (progn
        (unless agent-repl--recency-walk-landing
          (setq agent-repl--recency-walk-origin shown))
        (setq agent-repl--recency-walk-landing ws))
    (agent-repl--end-recency-walk ws trigger)))

(defun agent-repl-host--request-switch-held (ws trigger)
  "`agent-repl-host-request-switch' for WS, a workspace this editor holds."
  (let* ((shown (agent-repl--ws-current-name))
         (walk (eq trigger 'recency-walk))
         (started (unless (eq trigger 'startup)
                    (agent-repl-host--count-editor-switch ws trigger))))
    (agent-repl-host--note-walk ws trigger shown)
    (setq agent-repl-host-last-request (cons ws (float-time)))
    (agent-repl--info ws "elisp.host.switch-requested ws={ws} trigger={trigger} shown={shown} pending={pending}"
                      :ws ws :trigger trigger :shown shown :pending (or (agent-repl-host-pending-selection) "none"))
    (cond
     ((null (agent-repl-host-ref ws))
      (agent-repl--warn ws "elisp.host.switch-request-unsent ws={ws} trigger={trigger} reason=no-ref"
                        :ws ws :trigger trigger)
      (message "[agent-repl] Cannot switch to %s: the daemon has not registered it" ws)
      nil)
     ((null (agent-repl-host-conn-or-primary ws))
      (agent-repl--warn ws "elisp.host.switch-request-unsent ws={ws} trigger={trigger} reason=no-connection"
                        :ws ws :trigger trigger)
      (message "[agent-repl] Cannot switch to %s: no daemon connection" ws)
      nil)
     (t
      (prog1
          (agent-repl-host-select
           ws (lambda (outcome)
                (unless (eq outcome :success)
                  (agent-repl--info ws "elisp.host.switch-request-settled ws={ws} outcome={outcome}"
                                    :ws ws :outcome (prin1-to-string outcome))))
           walk started)
        (agent-repl-host--move-on-request ws shown))))))

(defun agent-repl-host-request-switch-ref (ref trigger &optional name)
  "Ask the daemon to make the workspace REF names current, whether held or not.
TRIGGER is as `agent-repl-host-request-switch' takes it.  A workspace this
editor holds is switched to by name, the frame moving at once.  One it
does not hold (frontend.v1.RosterRowVisibility) has no perspective to move to:
the daemon is asked to select it (`agent-repl-host-select-ref'), the
roster push that marks it held opens it, and the roster\='s follow lands
the frame on it (`agent-repl-roster-apply-current', owner ruling
2026-11-21).  Either way the switch is counted as the user\='s.  A step of
the recency walk keeps the walk and the selection history, as a step to a
held workspace does, landing on NAME (its tab name once it is held); any
other trigger ends the walk.  Answers t when the request was sent or
queued."
  (let ((ws (agent-repl-host--held-name (plist-get ref :id))))
    (if ws
        (agent-repl-host--request-switch-held ws trigger)
      (let ((scope (agent-repl-host--target-scope ref))
            (walk (eq trigger 'recency-walk))
            (started (agent-repl-host--count-editor-switch nil trigger)))
        (if walk
            (agent-repl-host--note-walk name trigger (agent-repl--ws-current-name))
          (agent-repl--end-recency-walk scope trigger))
        (setq agent-repl-host-last-request nil)
        (agent-repl--info scope "elisp.host.switch-requested-unheld id={id} trigger={trigger} shown={shown}"
                          :id (plist-get ref :id) :trigger trigger :shown (agent-repl--ws-current-name))
        (agent-repl-host-select-ref
         ref (lambda (outcome)
               (unless (eq outcome :success)
                 (agent-repl--info scope "elisp.host.switch-request-settled id={id} outcome={outcome}"
                                   :id (plist-get ref :id) :outcome (prin1-to-string outcome))))
         walk started)))))

(defun agent-repl-host--move-on-request (ws shown)
  "Move the frame to WS for Emacs\='s own switch request, unless it cannot.
SHOWN is the workspace on screen when the request was made.  Every
decision is recorded at INFO, as the roster's follow records its own."
  (let ((id (plist-get (agent-repl-host-ref ws) :id)))
    (cond
     ((equal ws shown)
      (agent-repl--info ws "elisp.host.move-on-request ws={ws} decision=already-shown" :ws ws))
     ((or (agent-repl-open-tabs-holds-p id)
          (not (equal (agent-repl--ws-by-ref-id id) ws)))
      (agent-repl--info ws "elisp.host.move-on-request ws={ws} decision=no-tab-yet" :ws ws))
     (t
      ;; THE MOVE IS TIMED, GC APART.  A switch a focus gain's claim asks for
      ;; took 140-190ms from its select to its move where a keyboard one took
      ;; 3-5ms (2026-10-23), and the log could not say whether the select's
      ;; send, a collection or the move itself spent it.  The record carries
      ;; all three: the time since the request (which the select's send is
      ;; inside), the move's own, and the collections during the move.
      (let ((started (float-time))
            (gcs gcs-done)
            (gc-seconds gc-elapsed))
        (agent-repl-roster-move-frame ws)
        ;; THE WEBVIEW TAKES ITS NEW SIZE BEFORE THE PAGE IS SHOWN, so WebKit
        ;; paints the page once, at the size it is shown at.  The frame move
        ;; laid out WS's windows, and a workspace whose gate hides the input
        ;; gives the webview the input's space too; fitted only by the
        ;; redisplay after the command, the size reached WebKit after the
        ;; show, and the page painted at the old height, the Emacs window
        ;; showing below it, then painted again at the new one some 50ms later
        ;; (owner, 2026-10-16).  Nothing is painted that the switch would not
        ;; paint: rapid presses show each page, and WebKit draws only the one
        ;; shown at each frame.
        (agent-repl--frontend-fit-webviews)
        (let ((page (agent-repl-webhost-has-page-p ws)))
          (when page (agent-repl-webhost-show ws))
          (agent-repl--info ws "elisp.host.move-on-request ws={ws} decision=moved from={from} page={page} since-request-ms={since_request_ms} move-ms={move_ms} gcs={gcs} gc-ms={gc_ms}"
                            :ws ws :from shown :page (if page "shown" "none")
                            :since_request_ms (round (* 1000 (- (float-time) (cdr agent-repl-host-last-request))))
                            :move_ms (round (* 1000 (- (float-time) started)))
                            :gcs (- gcs-done gcs)
                            :gc_ms (round (* 1000 (- gc-elapsed gc-seconds))))))))))

(defun agent-repl-host--send-subject-verb (ws op rpc on-success on-not-sent &optional extra)
  "Send workspace WS\='s ref through the workspace-subject verb RPC.
EXTRA, a plist, rides the request beside the ref.
OP is the verb\='s record slug (`elisp.host.<op>\=', `<op>-skipped\=',
`<op>-refused\=', `<op>-unknown-arm\=', `<op>-failed\=').  ON-SUCCESS is
called with no argument once the daemon answers success, and ON-NOT-SENT
with no argument on every other outcome: no ref or no connection yet (WS
is not sent), a refusal (recorded by `agent-repl-host--on-refused\='), an
arm the verb does not know, or a transport failure (both at ERROR).
Answers t when the call was made, nil when it was not."
  (let ((ref (agent-repl-host-ref ws))
        (conn (agent-repl-host-conn-or-primary ws)))
    (cond
     ((or (null ref) (null conn))
      (agent-repl--log ws "elisp.host.{op}-skipped ws={ws} reason={reason}" :op op
                       :ws ws :reason (if ref "no-connection" "no-ref"))
      (funcall on-not-sent)
      nil)
     (t
      (agent-repl--log ws "elisp.host.{op} ws={ws} id={id}" :op op :ws ws :id (prin1-to-string (plist-get ref :id)))
      (funcall
       rpc conn (append (list :workspace ref) extra)
       :on-response
       (lambda (response)
         (pcase (plist-get response :arm)
           (:success (funcall on-success))
           (:error
            (agent-repl-host--on-refused ws op (plist-get response :value))
            (funcall on-not-sent))
           (arm
            (agent-repl--error ws "elisp.host.{op}-unknown-arm ws={ws} arm={arm}" :op op :ws ws :arm (prin1-to-string arm))
            (funcall on-not-sent))))
       :on-failure
       (lambda (detail)
         (agent-repl--error ws "elisp.host.{op}-failed ws={ws} detail={detail}"
                            :op op :ws ws :detail (prin1-to-string detail))
         (funcall on-not-sent)))
      t))))

(defun agent-repl-host-report-page-unloading (ws on-reported on-not-reported)
  "Tell the daemon Emacs is about to unload workspace WS\='s page on purpose.
ON-REPORTED is called with no argument once the daemon has answered
success: only then may the page be unloaded, so the daemon knows the end
of the page\='s web stream is deliberate before it can end
\(endpoint_report_page_unloading.proto).  Any other outcome calls
ON-NOT-REPORTED with no argument and the page stays loaded: unloading is a
performance measure, and a page kept loaded costs nothing the user sees.
The outcomes are recorded by `agent-repl-host--send-subject-verb\='."
  (agent-repl-host--send-subject-verb
   ws "page-unloading" #'agent-repl-rpc-report-page-unloading
   (lambda ()
     (agent-repl--log ws "elisp.host.page-unloading-reported ws={ws}" :ws ws)
     (funcall on-reported))
   on-not-reported))

(defun agent-repl-host-report-interaction (ws kind)
  "Tell the daemon the user interacted with workspace WS, KIND naming how.
KIND is `:composer-typed' or `:feed-navigated': the acts Emacs sees (the
page reports the user\='s scrolling itself).  The daemon draws WS\='s row
viewed on the status it stands on and stamps its interaction recency
\(`agent-repl-rpc-report-user-interaction').

A REPORT THAT WOULD CHANGE NOTHING IS NOT SENT: when the roster already
draws WS viewed and as the workspace interacted with most recently
\(`agent-repl-roster-interaction-standing-p'), another keystroke tells
the daemon nothing new.  And ONE REPORT IS IN FLIGHT PER WORKSPACE: an
interaction made while WS\='s report is unanswered is the same news, so
it is dropped, not held.

FIRE AND FORGET otherwise: every outcome is recorded by
`agent-repl-host--send-subject-verb', and a report that never lands
leaves the row as it was until the user\='s next interaction."
  ;; THE USER ACTING ON THE WORKSPACE A RECENCY WALK STANDS ON ENDS THE WALK:
  ;; they stopped there (`agent-repl--end-recency-walk').
  (when (equal ws agent-repl--recency-walk-landing)
    (agent-repl--end-recency-walk ws "interaction"))
  (cond
   ;; Both skips run per keystroke, so they are hot-path records.
   ((agent-repl-roster-interaction-standing-p ws)
    (agent-repl--log-verbose ws "elisp.host.interaction-standing ws={ws} kind={kind}" :ws ws :kind (prin1-to-string kind)))
   ((plist-get (agent-repl-host--entry ws) :interaction-in-flight)
    (agent-repl--log-verbose ws "elisp.host.interaction-in-flight ws={ws} kind={kind}" :ws ws :kind (prin1-to-string kind)))
   (t
    ;; THE MARK STANDS BEFORE THE CALL, so an answer however soon it comes
    ;; (or a report never sent) is what lowers it.
    (agent-repl-host--put ws :interaction-in-flight t)
    (let ((answered (lambda () (agent-repl-host--put ws :interaction-in-flight nil))))
      (agent-repl-host--send-subject-verb
       ws "interaction"
       (lambda (conn request &rest args)
         (apply #'agent-repl-rpc-report-user-interaction conn
                (append request (list :interaction kind)) args))
       (lambda ()
         (agent-repl--log ws "elisp.host.interaction-reported ws={ws} kind={kind}" :ws ws :kind (prin1-to-string kind))
         (funcall answered))
       answered)))))

(defun agent-repl-host--on-workspace-activated (&rest _)
  "Tell the daemon about a perspective activation Emacs made on its own.
Registered on workspace.el's perspective-activation boundary.  The user's
switch triggers never land here first -- they REQUEST the switch
\(`agent-repl-host-request-switch'), which moves the frame as a follow
would, and the roster's follow moves it too: activations this hook leaves
alone (`agent-repl-roster-following').  What
remains is a switch Emacs made itself, which the daemon is told about
after the fact so its `current' matches the frame.

A TRANSIENT BACKGROUND ACTIVATION IS NOT A TAB SWITCH, and reporting one
as such told the daemon the user had chosen a workspace they never
looked at.  `agent-repl--call-in-background-workspace' activates a
workspace\='s perspective to build into its own frame -- the webview mount
and the link-up re-point both go through it -- and binds
`agent-repl--eager-open-in-progress\=' around the whole switch-in /
build / switch-back.  Measured on the link-up edge: re-pointing the
UNSELECTED workspace\='s webview activated its perspective, this hook
selected it with the fresh daemon, the daemon stamped it `current\=',
and the roster push carrying that stamp took the frame off the workspace
the user was standing in.  It is the same flag the other two
activation-reactive hooks consult for the same reason (see its
docstring in `core.el\=')."
  (let ((ws (agent-repl--ws-current-name)))
    (cond
     (agent-repl--eager-open-in-progress
      (agent-repl--log ws "elisp.host.select-skipped reason=background-activation"))
     ;; THE ROSTER MOVED THE FRAME, so the daemon already holds this
     ;; selection: echoing it back would race a newer request -- a sidebar
     ;; click landing between the follow and the echo was overwritten by the
     ;; echo and the frame went back.
     (agent-repl-roster-following
      (agent-repl--log ws "elisp.host.select-skipped ws={ws} reason=followed-roster" :ws ws))
     (t
      ;; A switch Emacs made itself (a Doom command, a teardown landing, a
      ;; file placed in its workspace) is the user's, unless Emacs made it
      ;; on its own account.
      (let ((started (if agent-repl-host-automatic-switch
                         (progn
                           (agent-repl--log ws "elisp.host.editor-switch-uncounted ws={ws} reason=automatic" :ws ws)
                           nil)
                       (agent-repl-host--count-editor-switch ws 'activation))))
        ;; The daemon is told after the fact.
        (when (and ws (agent-repl-host--entry ws))
          (agent-repl--info ws "elisp.host.select-local-activation ws={ws} command={command}"
                            :ws ws :command (prin1-to-string this-command))
          (agent-repl-host-select ws nil nil started)))))))

;;;; ---- Subscribe ----

(defun agent-repl-host--attach (ws conn ref)
  "Record REF and CONN as workspace WS's identity and owner.
The ref is stored BOTH in this file's table and on workspace.el's plist
under `:ref', through their one writer `agent-repl-host-take-ref', so any
module can read the identity from whichever it already holds; neither
copy is ever derived from a path.

THE SELECTION THE ACTIVATION HOOK COULD NOT MAKE IS MADE HERE.
`agent-repl-host--on-workspace-activated' issues the SelectWorkspace on
every perspective activation, but a NEWLY REGISTERED workspace activates
its perspective BEFORE the daemon has minted its ref -- Doom switches to
the new perspective and the registration answers afterwards -- so that
activation finds no ref and skips (`elisp.host.select-skipped
reason=no-ref').  Nothing came back for it: the daemon therefore never
stamped `current', every roster row it resolved carried
`RosterRowCurrent.current' false, and no sidebar row in any webview was
ever drawn as the selected one.

Attaching the ref is exactly the moment the only condition that blocked
that selection stops holding, so the repair belongs here rather than in a
retry somewhere: the workspace has an identity now, and if it is the one
the user is standing in, the daemon is told.  Selecting is idempotent by
the verb\='s own contract -- re-selecting the current workspace succeeds and
re-stamps nothing -- so a re-subscribe after a reconnect or a revival
costs a round trip and changes no view."
  (agent-repl-host--put ws :conn conn)
  (agent-repl-host-take-ref ws ref)
  ;; THE USER'S WORKSPACE, NOT A TRANSIENTLY ACTIVATED ONE.  `--ws-current-name'
  ;; answers the perspective that is active right now, and
  ;; `agent-repl--call-in-background-workspace' makes a BACKGROUND workspace's
  ;; perspective active for the length of a mount, so an attach landing inside
  ;; that window would report the user as having switched to a workspace they
  ;; never looked at.  Same flag, same reason as
  ;; `agent-repl-host--on-workspace-activated'.
  (when (and (equal ws (agent-repl--ws-current-name))
             (not agent-repl--eager-open-in-progress))
    (agent-repl-host-select ws)))

(defun agent-repl-host-subscribe (conn ws ref)
  "Open WS's `WatchHostWorkspace' subscription on CONN, echoing REF.
One subscription per OPEN workspace: a snapshot arrives first, then whole
replacements.  Returns the stream.

SUBSCRIBED IS AN ACCEPTANCE, not a spawn: `elisp.host.subscribed' is
written from the transport's ON-OPEN — the daemon's HTTP 200 header block
— so the record never claims a workspace is being watched by a daemon
that never answered.

THE CALLBACKS RESOLVE THE NAME AT CALL TIME, never the one captured
here.  A stream outlives renames (§8 makes a roster row's changed name a
rename of the tab), and host state is keyed on the NAME, so a callback
closed over the subscribe-time name would keep updating the OLD key
after `agent-repl-host-rename' moved the entry — the renamed tab's gate
would then never advance again.  The REF ID is the tab identity, so the
current name is looked up from it; the subscribe-time name is the
fallback for the one case the id no longer resolves (a closed or
tombstoned workspace), where it is the best name the record has."
  ;; NO WORKSPACE EVER HOLDS TWO STREAMS.  The roster's tab-open and the
  ;; panels' arrival both subscribe, and a second subscribe that overwrote
  ;; `:stream' left the first one running unreferenced: nothing could cancel
  ;; it, and its frames kept arriving after the workspace was forgotten.
  (when (agent-repl-host-stream ws)
    (agent-repl--info ws "elisp.host.subscribe-replaces-standing ws={ws}" :ws ws)
    (agent-repl-host-unsubscribe ws))
  (agent-repl-host--attach ws conn ref)
  (let* ((id (plist-get ref :id))
         (current (lambda () (or (agent-repl--ws-by-ref-id id) ws)))
         (stream nil))
    ;; THE CLOSE NAMES ITS OWN STREAM, so a close that lands after the
    ;; workspace already moved to another stream is recognized as stale
    ;; rather than taken for the loss of the stream now standing.
    (setq stream (agent-repl-rpc-watch-host-workspace
                  conn ref
                  (lambda (push)
                    (agent-repl-host--handle-push (funcall current) push stream))
                  (lambda (outcome)
                    (agent-repl-host--handle-close (funcall current) outcome stream))
                  (lambda ()
                    (let ((now (funcall current)))
                      (agent-repl--info now "elisp.host.subscribed ws={ws} method={method} id={id}"
                                        :ws now :method (prin1-to-string "WatchHostWorkspace") :id (prin1-to-string id))
                      ;; THE DAEMON HOLDS THE PAGE STATE ONLY WHILE THIS
                      ;; STREAM STANDS (endpoint_report_page_state.proto), so
                      ;; it is restated on every subscribe.
                      (agent-repl-page-report-restate now)))))
    (agent-repl-host--put ws :stream stream)
    (agent-repl--info ws "elisp.host.subscribe-opened ws={ws} id={id}" :ws ws :id (prin1-to-string (plist-get ref :id)))
    stream))

(defun agent-repl-host-unsubscribe (ws)
  "Cancel WS's host subscription.  Cancelling IS the graceful close.
Idempotent: a workspace with no standing stream is already unsubscribed."
  (let ((stream (agent-repl-host-stream ws)))
    (if (null stream)
        (agent-repl--log ws "elisp.host.unsubscribe-noop ws={ws}" :ws ws)
      (agent-repl--info ws "elisp.host.unsubscribe ws={ws}" :ws ws)
      (agent-repl-host--put ws :stream nil)
      (agent-repl-connect-stream-cancel stream))))

(defun agent-repl-host-rename (old new)
  "Re-key OLD's host bookkeeping onto NEW and return non-nil when it moved.
Host state is keyed on the WORKSPACE NAME (fanout §7), and §8 makes a
roster row's changed name a RENAME of the tab -- so a rename that moved
only `agent-repl--workspaces' would strand every fact this file holds
under a name nothing asks about again: `agent-repl-host-ref' for NEW
would answer nil (the composer and every verb refuse), a teardown by NEW
would unsubscribe nothing and leak the stream, and NEW would hold no
stream to cancel.  The standing stream's own callbacks do not depend on
this move: they resolve the current name from the ref id at call time
(see `agent-repl-host-subscribe'), so a push lands on NEW whether or not
the entry moved -- but with no entry under NEW there is nothing for it
to update.

The whole entry moves -- `:ref' `:conn' `:stream' `:host' -- because it is
the SAME workspace under a new name and none of those facts changed.
Nothing to move is not a failure: a workspace whose host stream was never
started has no entry, and a rename before the first subscribe is
ordinary.  A NEW that already carries an entry is refused loudly and
nothing moves: two workspaces would otherwise share one stream."
  (let ((entry (gethash old agent-repl-host--by-name)))
    (cond
     ((null entry)
      (agent-repl--log new "elisp.host.rename-noop old={old} new={new} reason=no-entry"
                       :old old :new new)
      nil)
     ((gethash new agent-repl-host--by-name)
      (agent-repl--error new
                         "elisp.host.rename-target-occupied old={old} new={new} ref={ref}"
                         :old old :new new :ref (prin1-to-string (plist-get entry :ref)))
      nil)
     (t
      ;; Primitive hash mutations with no Lisp call between them, so no
      ;; observer can run against a half-moved entry.
      (puthash new entry agent-repl-host--by-name)
      (remhash old agent-repl-host--by-name)
      (agent-repl--info new
                        "elisp.host.renamed old={old} new={new} id={id} stream={stream} conn={conn} host-push={host_push}"
                        :old old :new new :id (plist-get (plist-get entry :ref) :id)
                        :stream (and (plist-get entry :stream) t)
                        :conn (and (plist-get entry :conn) t)
                        :host_push (and (plist-get entry :host) t))
      t))))

(defun agent-repl-host-forget (ws)
  "Drop every trace of WS from this file, cancelling its stream first.
Called when a workspace's tab is torn down; the daemon-side session is
untouched, because closing a tab is a VIEW act."
  (agent-repl-host-unsubscribe ws)
  ;; A call held for WS is the user's: it goes before WS is forgotten.
  (agent-repl-host--release-held ws "forgotten")
  (remhash ws agent-repl-host--by-name)
  (agent-repl--info ws "elisp.host.forgotten ws={ws}" :ws ws))

(defun agent-repl-host--note-ending (ws stream)
  "Record that STREAM, WS\='s host stream, carried the planned ending.
The daemon is standing down in a PLANNED exit and this is the stream\='s
last frame (`DaemonStreamEnding'), so the clean end that follows is
expected, not lost: `agent-repl-host--handle-close' reads the mark.
STREAM nil means the stream standing for WS now."
  (agent-repl-host--put ws :ending-stream (or stream (agent-repl-host-stream ws)))
  (agent-repl--info ws "elisp.host.stream-ending ws={ws}" :ws ws))

(defun agent-repl-host--planned-end-p (ws outcome stream)
  "Return non-nil when STREAM closing with OUTCOME is WS\='s planned end.
Only a CLEAN end (`(:ended)') of the very stream that carried the
planned-ending frame is planned; an error after the ending, or the end of
a stream that never carried it, is a loss."
  (let ((marked (plist-get (agent-repl-host--entry ws) :ending-stream)))
    (and (eq (car outcome) :ended)
         marked
         (eq marked (or stream (agent-repl-host-stream ws))))))

(defun agent-repl-host--refused-arm (outcome)
  "Return the arm a refused host stream's OUTCOME names, or nil.
Only a `failed_precondition' close carries a handover refusal
\(`agent-repl-connect-refused-arm')."
  (and (eq (car outcome) :error)
       (equal (plist-get (cadr outcome) :code) "failed_precondition")
       (agent-repl-connect-refused-arm (cadr outcome))))

(defun agent-repl-host--handle-close (ws outcome &optional stream)
  "React to WS's host STREAM closing with OUTCOME.
`(:cancelled)' is Emacs's own unsubscribe and is normal.  A clean end
after the stream carried the planned ending (`agent-repl-host--note-ending')
is the daemon standing down on purpose: it is recorded at INFO and WS
follows the live daemon with trigger `planned-ending'.  Anything else is
the producer dropping a STANDING stream, which the contract calls a
transport failure: it is recorded at ERROR, the dead stream is dropped,
and WS FOLLOWS THE LIVE DAEMON (`agent-repl-host--follow-live-daemon').

Regression, 2026-09-27: this used to leave the recovery to the link's own
reconnect.  But the link does not go down when ONE daemon of a handover
exits -- it is promoted onto the successor, and a promotion rebuilds
nothing -- so a workspace the exiting daemon never transferred kept its
dead connection for good: every later call went to that address and its
webview stayed blank on the dead origin.

A close of a stream that is no longer WS\='s standing one is STALE: WS
already moved on, and dropping the stream that stands now would strand
it.  STREAM nil means the caller did not name the stream."
  (cond
   ((and stream (not (eq stream (agent-repl-host-stream ws))))
    (agent-repl--log ws "elisp.host.stale-stream-close ws={ws} outcome={outcome}" :ws ws :outcome (prin1-to-string outcome)))
   ((eq (car outcome) :cancelled)
    (agent-repl--log ws "elisp.host.stream-cancelled ws={ws}" :ws ws))
   ;; THE STREAM STANDS ON THE OUTGOING DAEMON ON PURPOSE, its move owed to
   ;; the promotion (`agent-repl-host--adopt-settled'): its end is that
   ;; daemon exiting, which the promotion answers with the one move, so no
   ;; walk of its own starts here.
   ((plist-get (agent-repl-host--entry ws) :stream-due)
    (agent-repl--info ws "elisp.host.stream-ended-awaiting-promotion ws={ws} outcome={outcome}" :ws ws :outcome (prin1-to-string outcome))
    (agent-repl-host--put ws :stream nil))
   ((agent-repl-host--planned-end-p ws outcome stream)
    ;; THE DAEMON SAID SO FIRST.  Its last frame was the planned ending, so
    ;; this clean end is a stand-down, not a fault: INFO, and the same
    ;; single walk onto the live daemon a loss takes.
    (agent-repl--info ws "elisp.host.stream-ended-planned ws={ws}" :ws ws)
    (agent-repl-host-release-restart-hold ws "planned-ending")
    (agent-repl-host--put ws :ending-stream nil)
    (agent-repl-host--put ws :stream nil)
    (agent-repl-host--follow-live-daemon ws "planned-ending"))
   ((equal (agent-repl-host--refused-arm outcome) "transferring_away")
    ;; THE DAEMON HANDED WS ON, AND SAID SO.  This is the `transferred'
    ;; notice read off a refused open, not a loss: WS adopts onto the
    ;; successor.  Taken for a loss it waited on the promotion, which comes
    ;; only once the incumbent exits -- and an incumbent still serving another
    ;; workspace never did (2026-10-26: a fresh Emacs, linked by daemon.addr
    ;; to the incumbent, drew all 45 of its workspaces severed).
    ;;
    ;; THE REFUSAL NAMES THE SUCCESSOR (`agent-repl-connect-refusal-address'),
    ;; so WS walks there as a unary `transferring_away' walks it, rather than
    ;; onto whichever successor the link recorded.
    (let ((address (agent-repl-connect-refusal-address (cadr outcome))))
      (agent-repl-host--put ws :ending-stream nil)
      (agent-repl--info ws "elisp.host.stream-refused-transferred ws={ws} address={address}" :ws ws :address (prin1-to-string address))
      (agent-repl-host-release-restart-hold ws "stream-refused-transferred")
      (agent-repl-host--put ws :stream nil)
      (agent-repl-host--put ws :detached t)
      (if address
          (agent-repl-host-handle-refusal
           ws (list :arm :transferring-away :value (list :address address)))
        (agent-repl-host--transferred ws))))
   ((equal (agent-repl-host--refused-arm outcome) "not_yet_adopted")
    ;; THE DAEMON HAS NOT TAKEN WS OVER YET, AND SAID SO.  Nothing is lost:
    ;; a standing successor is the one to adopt onto, and otherwise WS
    ;; follows the live daemon, which registers it once the link settles
    ;; (2026-10-26: the promoted successor refused a workspace its
    ;; predecessor still held at its exit, and that was drawn as a loss).
    ;;
    ;; A REFUSAL NAMING THE DAEMON THAT SERVES WS MEANWHILE is viewed there
    ;; (`agent-repl-host-view-where-served'), as a unary `not_yet_adopted'
    ;; is: nothing waits on the takeover.  One naming the very daemon that
    ;; refused walks nothing there, and takes the walk below.
    (let ((address (agent-repl-connect-refusal-address (cadr outcome))))
      (agent-repl-host--put ws :ending-stream nil)
      (agent-repl--info ws "elisp.host.stream-refused-not-yet-adopted ws={ws} serving={serving}" :ws ws :serving (prin1-to-string address))
      (agent-repl-host-release-restart-hold ws "stream-refused-not-yet-adopted")
      (agent-repl-host--put ws :stream nil)
      (agent-repl-host--put ws :detached t)
      ;; ONE SHARED ADDRESS: the daemon it reaches is adopting WS, and the
      ;; adopt goes out on the primary link (agent-repl-host-handle-refusal).
      (agent-repl-host-handle-refusal ws (list :arm :not-yet-adopted))))
   (t
    (agent-repl-host--put ws :ending-stream nil)
    (agent-repl--error ws "elisp.host.stream-lost ws={ws} outcome={outcome}" :ws ws :outcome (prin1-to-string outcome))
    ;; NOTHING CAN RESOLVE THE HOLD ANY MORE.  The hold is a bet that the
    ;; daemon's next push settles it; a dropped standing stream means no
    ;; such push is coming, so keeping the composer shut would wedge it
    ;; until the link's reconnect happened to land a new one.
    (agent-repl-host-release-restart-hold ws "stream-lost")
    (agent-repl-host--put ws :stream nil)
    (agent-repl-host--follow-live-daemon ws "stream-lost"))))

;;;; ---- Pushes ----

(defun agent-repl-host--handle-push (ws push &optional stream)
  "Dispatch one decoded `WatchHostWorkspace' PUSH for workspace WS.
STREAM is the stream PUSH arrived on; nil means the caller did not name
it, and the stream standing for WS now is meant.

A PUSH ON A STREAM THAT NO LONGER STANDS FOR WS IS DROPPED.  A cancel is
asynchronous, so frames the daemon sent before it can still arrive; and a
forgotten workspace has no entry left to update.  Applying either would
re-create bookkeeping for a workspace whose tab is gone (measured
2026-10-06: a merged workspace's `:host' push landed 10ms after
`elisp.host.forgotten' and re-created its entry).  Recorded at INFO."
  (let ((arm (plist-get push :arm))
        (value (plist-get push :value)))
    (cond
     ((and stream (null (agent-repl-host--entry ws)))
      (agent-repl--info ws "elisp.host.push-dropped ws={ws} arm={arm} reason=forgotten"
                        :ws ws :arm arm))
     ((and stream (not (eq stream (agent-repl-host-stream ws))))
      (agent-repl--info ws "elisp.host.push-dropped ws={ws} arm={arm} reason=stale-stream"
                        :ws ws :arm arm))
     (t
      (agent-repl-host--dispatch-push ws arm value stream)))))

(defun agent-repl-host--dispatch-push (ws arm value stream)
  "Apply one host push of ARM carrying VALUE to WS; STREAM carried it."
  (pcase arm
    (:host (agent-repl-host--apply-state ws value))
    (:selection (agent-repl-host--apply-selection ws value))
    (:mentions (agent-repl-host--apply-mentions ws value))
    (:composer-selections (agent-repl-host--apply-composer-selections ws value))
    (:mention-targets (agent-repl-host--apply-mention-targets ws value))
    (:final-response-clicked (agent-repl-host--reply-target-picked ws "final-response-clicked"))
    (:chat-text-highlighted (agent-repl-host--reply-target-picked ws "chat-text-highlighted"))
    (:response-text-highlighted (agent-repl-host--reply-target-picked ws "response-text-highlighted"))
    (:load-page-requested (agent-repl-host--load-page-requested ws))
    (:compose-entry (agent-repl-host--compose-entry ws value))
    (:response-question (agent-repl-host--response-question ws value))
    (:transferred (agent-repl-host--transferred ws value))
    (:transfer-withdrawn (agent-repl-host--transfer-withdrawn ws))
    (:reload-webapp (agent-repl-host--reload-webapp ws value))
    (:open-in-editor (agent-repl-host--open-in-editor ws value))
    (:open-commit-log (agent-repl-host--open-commit-log ws value))
    (:run-in-terminal (agent-repl-host--run-in-terminal ws value))

    (:ending (agent-repl-host--note-ending ws stream))
    (_ (agent-repl--error ws "elisp.host.unknown-push ws={ws} arm={arm} value={value}"
                          :ws ws :arm (prin1-to-string arm) :value (prin1-to-string value)))))

(defun agent-repl-host--apply-naming (ws)
  "Rename WS's input buffer so its name carries WS's display title.
fanout §7: buffer titles use `naming.title', else `naming.slug', else the
row name — TITLES NAME THE BUFFERS, so a display-title accessor that
answers the right string while every buffer keeps its old name is not the
contract.  `agent-repl--input-buffer-name' keeps the name matching
`agent-repl--input-buffer-re', so a titled composer is still an agent
panel to every predicate and its identity segment is still recoverable.

The WEBVIEW buffer is deliberately left alone: its name is a lookup key
(`agent-repl--frontend-webview-buffer-name') that callers derive from the
workspace name without ever seeing the title.

Silent and inert when WS has no live input buffer — a workspace whose
composer has not been created yet has no name to write the title into.
panels.el calls THIS function at creation for exactly that reason, so a
composer born after the last `naming' push is named from the title the
daemon has by then rather than wearing the bare canonical name forever."
  (let ((buffer (agent-repl--ws-get ws :input-buffer)))
    (when (buffer-live-p buffer)
      (let ((want (agent-repl--input-buffer-name ws (agent-repl-host-display-title ws))))
        (unless (equal (buffer-name buffer) want)
          (with-current-buffer buffer
            ;; UNIQUE-OK: two workspaces may be handed the same vendor title,
            ;; and a rename that ERRORED on the collision would strand the
            ;; second composer under the old name with no way back.
            (rename-buffer want t))
          (agent-repl--info ws "elisp.host.buffer-renamed ws={ws} buffer={buffer} title={title}"
                            :ws ws :buffer (prin1-to-string (buffer-name buffer))
                            :title (prin1-to-string (agent-repl-host-display-title ws))))))))

(defun agent-repl-host--apply-state (ws host)
  "Whole-replace WS's host state with HOST and run the update hooks.
`shim_attached' false gets NO treatment: a parked workspace presents as
live and unwired, and the frontend cannot tell parked from idle, on
purpose.

A push that changes WS's vendor conversation id (see
`agent-repl-host-vendor-session-id') is recorded at INFO: the id is the
durable identity a resume replays, and the moment Emacs learns it is
otherwise invisible in the logs."
  (let ((vendor-before (agent-repl-host-vendor-session-id ws)))
    (agent-repl-host--put ws :host host)
    (let ((vendor-after (agent-repl-host-vendor-session-id ws)))
      (unless (equal vendor-before vendor-after)
        (agent-repl--info ws "elisp.host.vendor-session ws={ws} from={from} to={to}"
                          :ws ws :from (prin1-to-string vendor-before) :to (prin1-to-string vendor-after)))))
  (agent-repl-host--settle-restart-hold ws)
  (agent-repl-host--apply-naming ws)
  (agent-repl--log ws "elisp.host.state ws={ws} gate={gate} backfill={backfill} faults={faults}"
                   :ws ws :gate (prin1-to-string (agent-repl-host-composer-gate ws))
                   :backfill (prin1-to-string (agent-repl-host-backfill ws))
                   :faults (length (agent-repl-host-faults ws)))
  (run-hook-with-args 'agent-repl-host-update-functions ws host))

;;;; ---- The feed selection ----

(defun agent-repl-host--apply-selection (ws selection)
  "Record SELECTION as what WS's feed has selected, and hand it to the composer.
SELECTION is the decoded `HostWorkspaceSelection' plist `(:kind KIND)':
KIND is `:none', `:response', `:prompt' or `:bubble'.  It is the host
watch's `selection' push: state, not an event, so a late subscriber is sent
the selection in force first.  The daemon holds the selection itself; Emacs
keeps the kind, for the composer's escape-twice clear and its `RET'.  The
composer turns its selection mode on or off from it
\(`agent-repl--input-selection-changed')."
  (let ((kind (plist-get selection :kind)))
    (agent-repl-host--put ws :selection kind)
    (agent-repl--info ws "elisp.host.selection ws={ws} kind={kind}" :ws ws :kind (prin1-to-string kind))
    (agent-repl--input-selection-changed ws)))

(defun agent-repl-host-selection (ws)
  "Return the kind of row WS's feed has selected, or nil.
One of `:none', `:response', `:prompt' and `:bubble', as the host watch
last pushed it.  Nil means no selection push has arrived yet, which is
nothing selected."
  (plist-get (agent-repl-host--entry ws) :selection))

;;;; ---- The composer mentions ----

(defun agent-repl-host--apply-mentions (ws mentions)
  "Record MENTIONS as the names WS's own final responses mentioned.
MENTIONS is the decoded `HostWorkspaceMentions' plist `(:commands
:workspaces)', each a list of names, the most recently mentioned first.  It
is the host watch's `mentions' push: state, whole-replaced, and sent first
on every subscription, so Emacs's record is the daemon's after a restart of
either side.  The composer orders its completions by it
\(completion-order.el)."
  (agent-repl-host--put ws :mentions mentions)
  (agent-repl--log ws "elisp.host.mentions ws={ws} commands={commands} workspaces={workspaces}"
                   :ws ws :commands (length (plist-get mentions :commands))
                   :workspaces (length (plist-get mentions :workspaces))))

(defun agent-repl-host-mentions (ws kind)
  "Return the names of KIND WS's own final responses mentioned, newest first.
KIND is `command' (names without their slash) or `workspace'.  Nil before
the host watch's first `mentions' push, which is a workspace that has
mentioned nothing."
  (plist-get (plist-get (agent-repl-host--entry ws) :mentions)
             (pcase kind
               ('command :commands)
               ('workspace :workspaces)
               (_ (error "agent-repl: unknown mention kind %S" kind)))))

;;;; ---- The composer selections ----

(defvar agent-repl-host--composer-selections nil
  "Every candidate the reader took from a composer's completion, or nil.
The decoded `HostComposerSelections' plist `(:commands :workspaces
:channels)', each a list of names, the most recently taken first, as the
daemon last pushed it.  The record is the user's, not a workspace's, so it
is one for all of Emacs: every workspace's host watch pushes the same one.
Nil before the first push, which is nothing taken.")

(defun agent-repl-host--apply-composer-selections (ws selections)
  "Record SELECTIONS, the daemon's record of the composer's taken completions.
SELECTIONS is the decoded `HostComposerSelections' plist.  It is the host
watch's `composer_selections' push, arriving on WS's watch: state,
whole-replaced, and sent first on every subscription, so Emacs's record is
the daemon's after a restart of either side.  The composer orders its
completions by it (completion-order.el)."
  (setq agent-repl-host--composer-selections selections)
  (agent-repl--log ws "elisp.host.composer-selections ws={ws} commands={commands} workspaces={workspaces} channels={channels}"
                   :ws ws :commands (length (plist-get selections :commands))
                   :workspaces (length (plist-get selections :workspaces))
                   :channels (length (plist-get selections :channels))))

(defun agent-repl-host-composer-selections (kind)
  "Return the names of KIND the reader took from a composer's completion.
The most recently taken first.  KIND is `command' (names without their
slash), `workspace' or `channel' (names without their hash).  Nil before
the first `composer_selections' push, which is nothing taken."
  (plist-get agent-repl-host--composer-selections
             (pcase kind
               ('command :commands)
               ('workspace :workspaces)
               ('channel :channels)
               (_ (error "agent-repl: unknown composer selection kind %S" kind)))))

;;;; ---- The mention targets ----

(defvar agent-repl-host-mention-targets-functions nil
  "Functions called with WS after WS's mention targets changed.
The composer re-marks WS's composer through it (workspace-completion.el).")

(defun agent-repl-host--apply-mention-targets (ws targets)
  "Record TARGETS as the workspace names WS's composer completes.
TARGETS is the decoded `WorkspaceMentionTargets', a list of plists
\(:name :workspace :repository), the same set the daemon serves WS's page
as the names its feed links.  It is the host watch's `mention_targets'
push: state, whole-replaced, and sent first on every subscription.  Runs
`agent-repl-host-mention-targets-functions' with WS once recorded."
  (agent-repl-host--put ws :mention-targets targets)
  (agent-repl--log ws "elisp.host.mention-targets ws={ws} targets={targets}" :ws ws :targets (length targets))
  (run-hook-with-args 'agent-repl-host-mention-targets-functions ws))

(defun agent-repl-host-mention-targets (ws)
  "Return the workspace names WS's composer completes, as target plists.
Each is (:name NAME :workspace REF :repository REPOSITORY), REPOSITORY
nil when the daemon stated none.  Nil before the host watch's first
`mention_targets' push, and for a set with no targets: either way the
composer completes no workspace name."
  (plist-get (agent-repl-host--entry ws) :mention-targets))

;;;; ---- The reply target picked in the webview ----

(defun agent-repl-host--reply-target-picked (ws event)
  "Focus WS's composer: the reader picked in its webview what to reply to.
EVENT names the push, recorded as `elisp.host.EVENT': a final response's
click (`final-response-clicked'), words highlighted in a chat answer
\(`chat-text-highlighted') or in a final response
\(`response-text-highlighted').  Each way the next prompt answers the pick
and the keyboard is still in the webview, so the composer takes it in
insert state (`agent-repl-input-focus-for-reply').  A highlight first
sets the composer's draft aside for the prompt about it
\(`agent-repl-input-text-highlighted').  All are events, never replayed,
so a reconnect never moves the keyboard or clears the composer."
  (agent-repl--info ws "elisp.host.{event} ws={ws}" :event event :ws ws)
  (when (member event '("chat-text-highlighted" "response-text-highlighted"))
    (agent-repl-input-text-highlighted ws))
  (agent-repl-input-focus-for-reply ws))

(defun agent-repl-host--response-question (ws change)
  "Follow WS's question about a final response changing as CHANGE says.
CHANGE is the decoded `HostWorkspaceResponseQuestion', whose arm names the
transition; the composer makes the edit it asks for
\(`agent-repl-input-response-question').  An event, never replayed, so a
reconnect never edits the composer again."
  (agent-repl--info ws "elisp.host.response-question ws={ws} change={change}" :ws ws :change (prin1-to-string (plist-get change :arm)))
  (agent-repl-input-response-question ws (plist-get change :arm)))

;;;; ---- The rail's row click ----

(defun agent-repl-host-select-requested (ref)
  "Switch to the workspace REF names: the user clicked its row in a page\='s rail.
THE SAME SWITCH AS A KEY\='S (`agent-repl-host-request-switch-ref'), so the
frame moves on the click rather than after the roster push that a select
made by the page provoked, which waited on the daemon re-reading its
whole registry: some 350ms, where a key took 10 (2026-10-23).  The daemon
relays the click on the daemon stream (SelectInEditor), because the row
can be a workspace this editor does not hold.  An event, never replayed,
so a reconnect never switches."
  (agent-repl--info (agent-repl-host--target-scope ref) "elisp.host.select-requested id={id}" :id (plist-get ref :id))
  (agent-repl-host-request-switch-ref ref 'sidebar))

;;;; ---- The load button ----

(defun agent-repl-host--load-page-requested (ws)
  "Load WS\='s held-back page: the user clicked the webhost\='s load button.
A cold-gated workspace\='s page is held back when the user switches to it
\(`agent-repl-page-unload-note-shown'), and the webhost draws a load
button where it would be; the daemon relays the click on WS\='s own host
stream (LoadPageInEditor), because Emacs owns which pages the host holds.
An event, never replayed, so a reconnect never loads a page."
  (agent-repl--info ws "elisp.host.load-page-requested ws={ws}" :ws ws)
  (agent-repl-page-unload-load-requested ws))

;;;; ---- The branch click ----

(defun agent-repl-host--compose-entry (ws value)
  "Open a blank entry in WS's composer for the branch the reader clicked.
VALUE is (:label LABEL): the click on a response's node selected the
response, and the composer opens an entry answering LABEL
\(`agent-repl-input-compose-entry').  An event, never replayed, so a
reconnect never opens another."
  (agent-repl--info ws "elisp.host.compose-entry ws={ws} label={label}" :ws ws :label (plist-get value :label))
  (agent-repl-input-compose-entry ws (plist-get value :label)))

;;;; ---- The notification click ----

(defun agent-repl-host-notification-clicked (ref)
  "Switch to the workspace REF names: the user clicked its desktop banner.
The daemon posted the banner and read the click back, on the daemon
stream, because a banner can name a workspace this editor does not hold;
raising the frame and switching is `agent-repl--notification-activate'."
  (agent-repl--info (agent-repl-host--target-scope ref) "elisp.host.notification-clicked id={id}" :id (plist-get ref :id))
  (agent-repl--notification-activate ref))

;;;; ---- Reattach: the ONE walk that moves a workspace onto a daemon ----

(defvar agent-repl-host--reattach-tokens 0
  "The last reattach token minted; every walk takes the next one.")

(defun agent-repl-host--reattach (ws new claim trigger &optional on-settled)
  "Re-attach workspace WS to the daemon on connection NEW, claiming it by CLAIM.

THE ONE REATTACH PATH.  Every way a workspace comes to be served by a
daemon other than the one its stream stood on goes through here, so no
two of them can disagree about the walk:

  `:adopt'     the handover rendezvous -- a `transferred' push, a
               `transferring_away' or `not_yet_adopted' refusal.
               AdoptHostWorkspace on NEW (`agent-repl-host--adopt-onto').
  `:register'  WS lost its daemon -- a stream lost, a call that found its
               connection closed, a promotion that carried no transfer for
               WS, a link that came back up.  RegisterWorkspace on NEW,
               idempotent by dir (`agent-repl-host--register-onto').

Both end in `agent-repl-host--reattached': the old stream cancelled and
the host stream re-subscribed on NEW, which moves `:conn' and `:ref' --
and with them the webview, which each claim reloads onto NEW's origin at
the moment its own contract requires.  TRIGGER names what started the
walk, for the records.  ON-SETTLED, a `:register' walk's continuation, is
called with no arguments once that walk has an outcome, whichever it was.

EACH WALK TAKES A TOKEN, and only the newest walk for WS may finish: a
promotion or link-up that starts a walk onto the live daemon supersedes
one still waiting on a daemon that has since died, and the older walk\='s
late answer is dropped rather than moving WS back."
  (let ((token (agent-repl-host--begin-walk ws new claim trigger)))
    (pcase claim
      (:adopt (agent-repl-host--adopt-onto ws new token trigger))
      (:register (agent-repl-host--register-onto ws new token trigger on-settled))
      (_ (agent-repl--fatal ws "elisp.host.reattach-unknown-claim ws={ws} claim={claim} trigger={trigger}"
                            :ws ws :claim (prin1-to-string claim) :trigger trigger)))))

(defun agent-repl-host--begin-walk (ws new claim trigger)
  "Record WS\='s newest reattach walk onto NEW by CLAIM, and answer its token.
Every walk starts here, a lone one (`agent-repl-host--reattach') and each
of a batch (`agent-repl-host--reattach-all') alike, so a walk is recorded
and superseded one way.  TRIGGER is recorded."
  (let ((token (setq agent-repl-host--reattach-tokens
                     (1+ agent-repl-host--reattach-tokens)))
        (was (plist-get (agent-repl-host--entry ws) :reattach)))
    ;; THE WALK RECORDS THE PROMOTION EPOCH IT BEGAN IN, so its outcome can
    ;; read whether a promotion passed while it was in flight
    ;; (`agent-repl-host--register-settled').
    ;; A WALK OF A WORKSPACE THAT STOOD ON A DAEMON IS THE START OF ITS
    ;; DOWNTIME: the page, handed the instant with its next load, measures
    ;; from it to its conversation drawn (`viewed_downtime').  A first
    ;; attachment has no recorded daemon, so it is no downtime.
    ;; THE START LIVES IN THE WALK, never in a table of its own: a walk
    ;; superseded by a newer one hands its start on (one outage, its first
    ;; instant), and a walk that ends any other way takes its start with it.
    (let ((downtime (or (plist-get was :downtime-ms)
                        (and (agent-repl-host--recorded-conn ws)
                             (agent-repl-webhost-downtime-start ws)))))
      (agent-repl-host--put ws :reattach (list :token token :conn new :claim claim :trigger trigger
                                               :promotions (agent-repl-link-promotions)
                                               :downtime-ms downtime)))
    ;; A NEWER WALK THAT IS NOT AN ADOPT ONTO THE SAME DAEMON ENDS THE HOLD:
    ;; the superseded adopt's answer is dropped, so nothing else would send
    ;; what it held.
    (unless (and (eq claim :adopt) (eq (plist-get was :conn) new))
      (agent-repl-host--release-held ws "superseded"))
    (agent-repl--info ws "elisp.host.reattach ws={ws} address={address} claim={claim} trigger={trigger} conn={conn} from={from}"
                      :ws ws :address (prin1-to-string (agent-repl-connect-connection-address new)) :claim claim :trigger trigger :conn (agent-repl-host--conn-id new) :from (agent-repl-host--conn-id (agent-repl-host--recorded-conn ws)))
    token))

(defun agent-repl-host--reattach-all (names new trigger &optional on-settled)
  "Re-attach every workspace of NAMES onto NEW in ONE registration call.
THE LINK EDGES\=' WALK.  A link that came up, or a promotion onto a
successor, re-attaches every workspace Emacs holds, and as one
RegisterWorkspace each that was dozens of concurrent calls racing a
booting daemon\='s ONE state-database connection: each registration
republished the roster, the calls queued behind each other\='s
republications past the unary budget, and every workspace whose call
timed out was left detached (2026-11-03, `elisp.host.link-up-register-failed'
for nearly every workspace at 16:21:14).  So the batch registers every
directory in one RegisterWorkspaces (`agent-repl-host-register-all'),
which the daemon answers one dir after another with the roster
republished once, and nothing about the outcome can be a contention
timeout.

Each workspace still walks as its own: it takes its own token
\(`agent-repl-host--begin-walk'), so a newer walk supersedes it alone, and
its own outcome finishes it (`agent-repl-host--register-settled').  A
workspace with no directory takes the lone walk, which records why it
cannot register.  ON-SETTLED is called once, after every outcome."
  (let (walks)
    (dolist (ws names)
      (let ((dir (agent-repl--ws-get ws :project-dir)))
        (if dir
            (push (list ws dir (agent-repl-host--begin-walk ws new :register trigger)) walks)
          (agent-repl-host--reattach ws new :register trigger))))
    (setq walks (nreverse walks))
    (if (null walks)
        (when on-settled (funcall on-settled))
      (agent-repl-host-register-all
       new (mapcar #'cadr walks)
       (lambda (outcomes)
         (cl-loop for (ws dir token) in walks
                  for outcome in outcomes
                  do (agent-repl-host--register-settled
                      ws new token trigger dir
                      (agent-repl-host--outcome-ref ws dir outcome)))
         (when on-settled (funcall on-settled)))))))

(defun agent-repl-host-adopting-p (ws)
  "Non-nil while WS\='s reattach walk is an adopt still waiting on its answer.
Its page is then a participant the successor\='s rendezvous waits on, so
the web host loads it at once rather than in a lane\='s turn
\(`agent-repl--webhost-replay')."
  (eq :adopt (plist-get (plist-get (agent-repl-host--entry ws) :reattach) :claim)))

(defun agent-repl-host-handover-awaits-page-p (ws)
  "Non-nil while the attached successor\='s rendezvous still awaits WS\='s page.
THE RENDEZVOUS AWAITS A WORKSPACE NOT YET ADOPTED ONTO THE SUCCESSOR: its
expected participants are the streams that stood on WS at the
announcement, carried to its transfer (`daemon/internal/rollout/adopt.go',
`rendezvousCall'), so WS\='s page is awaited while WS still stands on the
outgoing daemon, while its transfer waits for the successor\='s acceptance
\(`:adopt-pending'), and while its adopt is unanswered
\(`agent-repl-host-adopting-p').  A workspace adopted onto the successor
has had its rendezvous, and a page with no successor attached is awaited by
nobody.  Regression, 2026-11-10: every page was held while ANY successor
was attached, and a stalled handover grew the loaded pages to 78."
  (let ((successor (agent-repl-link-successor)))
    (and successor
         (or (agent-repl-host-adopting-p ws)
             (plist-get (agent-repl-host--entry ws) :adopt-pending)
             (not (eq (agent-repl-host--recorded-conn ws) successor)))
         t)))

(defun agent-repl-host--reattach-current-p (ws token)
  "Return non-nil when TOKEN is WS\='s newest reattach walk."
  (equal token (plist-get (plist-get (agent-repl-host--entry ws) :reattach) :token)))

;;;; ---- The viewed workspace's outage: one episode, ended once ----

(defvar agent-repl-host--landing-downtime nil
  "(WS . MS) while WS\='s walk lands, MS its unclaimed downtime start or nil.
Bound only for the extent of `agent-repl-host--landed', whose reattach
loads WS\='s page: the load takes MS from here, the walk having been
cleared by then.")

(defun agent-repl-host-take-downtime (ws)
  "WS\='s downtime start in epoch ms, or nil; given up once.
Read from the landing walk when one is landing for WS, else from WS\='s
walk in flight (an adopt\='s page loads beside the adopt, before it lands)."
  (if (and agent-repl-host--landing-downtime
           (equal (car agent-repl-host--landing-downtime) ws))
      (prog1 (cdr agent-repl-host--landing-downtime)
        (setcdr agent-repl-host--landing-downtime nil))
    (let ((walk (plist-get (agent-repl-host--entry ws) :reattach)))
      (prog1 (plist-get walk :downtime-ms)
        (when walk
          (agent-repl-host--put ws :reattach (plist-put walk :downtime-ms nil)))))))

(defun agent-repl-host--landed (ws trigger settle)
  "Land WS\='s walk by calling SETTLE, and end its outage exactly once.
SETTLE moves the stream and loads the page (a reattach, or an adopt\='s
settling that may defer the move); TRIGGER is recorded.
The outage ends in one of two ways: a page load during the landing takes
its start and the page measures it (`viewed_downtime'), or the stream comes
back with the page kept and Emacs measures it here
\(`agent-repl-host--measure-downtime')."
  (let* ((walk (plist-get (agent-repl-host--entry ws) :reattach))
         (agent-repl-host--landing-downtime (cons ws (plist-get walk :downtime-ms))))
    (funcall settle)
    (agent-repl-host--measure-downtime ws trigger (cdr agent-repl-host--landing-downtime))))

(defun agent-repl-host--measure-downtime (ws trigger started)
  "Report WS\='s outage from STARTED (epoch ms, or nil) to now.
No page loaded to carry STARTED, so Emacs measures it."
  (when started
    (let ((seconds (/ (- (agent-repl--now-unix-ms) started) 1000.0)))
      (agent-repl--info ws "elisp.host.downtime-measured ws={ws} trigger={trigger} seconds={seconds}"
                        :ws ws :trigger trigger :seconds (format "%.3f" seconds))
      (agent-repl-latency-report-editor :viewed-reattach seconds))))

(defun agent-repl-host--reattached (ws new ref trigger)
  "Finish WS\='s reattach onto NEW with REF: move the stream, then say so.
The stream on the daemon WS is leaving is cancelled BEFORE the new one is
opened, so no workspace ever holds two, and the subscribe is what moves
`:conn' and `:ref' onto NEW.  TRIGGER is recorded."
  (agent-repl-host--move-stream ws new ref)
  (agent-repl-host--reattach-finished ws new trigger))

(defun agent-repl-host--move-stream (ws new ref)
  "Cancel WS\='s standing stream and subscribe it on NEW with REF.
THE ONE UNSUBSCRIBE, RESUBSCRIBE AND SNAPSHOT a walk costs the viewed
workspace, so a handover must spend it once, on the connection that stays
\(`agent-repl-host--adopt-settled')."
  (agent-repl-host-unsubscribe ws)
  (agent-repl-host-subscribe new ws ref))

(defun agent-repl-host--reattach-finished (ws new trigger)
  "Close WS\='s walk onto NEW, which TRIGGER started, and say so.
Clears the walk and the detached mark and runs
`agent-repl-host-reattached-functions'."
  (agent-repl-host--put ws :reattach nil)
  (agent-repl-host--put ws :detached nil)
  (agent-repl--info ws "elisp.host.reattached ws={ws} address={address} trigger={trigger} conn={conn}"
                    :ws ws :address (prin1-to-string (agent-repl-connect-connection-address new)) :trigger trigger :conn (agent-repl-host--conn-id new))
  (run-hook-with-args 'agent-repl-host-reattached-functions ws)
  (agent-repl-host--reselect-after-reattach ws))

(defun agent-repl-host--retiring-conn-p (conn)
  "Return non-nil when CONN is the link\='s primary and no successor stands on it.
Every daemon of a handover serves one shared address, so an adopt goes out
on the primary link, the OUTGOING daemon\='s connection, which the
successor\='s promotion closes (`agent-repl-link--promote-successor').  A
workspace recorded on it is recorded on a connection that is about to die."
  (and conn
       (eq conn (agent-repl-link-primary))
       (not (eq conn (agent-repl-link-successor)))))

(defun agent-repl-host--adopt-settled (ws new ref trigger)
  "Settle WS\='s adopt onto NEW, which answered with REF, at TRIGGER.
A HANDOVER MOVES WS\='S STREAM ONCE, ONTO THE CONNECTION THAT STAYS.  NEW
is usually the outgoing daemon\='s primary connection, which the promotion
closes seconds later: moving the stream there is a walk the promotion walks
again, and a recorded `:conn' that dies later is a third.  So when NEW is
retiring (`agent-repl-host--retiring-conn-p') the adopt ends the walk but
leaves the stream where it stands, and marks WS `:stream-due', which the
promotion answers with the one move onto the promoted connection
\(`agent-repl-host--move-due-streams').  Any other NEW is the connection
WS stays on, and the stream moves there now."
  (if (agent-repl-host--retiring-conn-p new)
      (progn
        (agent-repl-host--put ws :stream-due ref)
        (agent-repl--info ws "elisp.host.stream-move-deferred ws={ws} conn={conn} address={address}"
                          :ws ws :conn (agent-repl-host--conn-id new) :address (prin1-to-string (agent-repl-connect-connection-address new)))
        (agent-repl-host--reattach-finished ws new trigger))
    (agent-repl-host--reattached ws new ref trigger)))

(defun agent-repl-host--move-due-streams (old new)
  "Move every stream a settled adopt left on OLD onto NEW, the promoted connection.
The one walk of a handover for each such workspace
\(`agent-repl-host--adopt-settled'); answers the names moved, so the
promotion walks none of them again.  A mark with no standing stream on OLD
is an invariant breach, failed hard."
  (let (moved)
    (maphash
     (lambda (ws entry)
       (let ((ref (plist-get entry :stream-due)))
         (when ref
           (unless (eq (plist-get entry :conn) old)
             (agent-repl--fatal ws "elisp.host.stream-due-off-old ws={ws} recorded={recorded} old={old}"
                                :ws ws :recorded (agent-repl-host--conn-id (plist-get entry :conn)) :old (agent-repl-host--conn-id old)))
           (push ws moved))))
     agent-repl-host--by-name)
    (dolist (ws moved)
      (let ((ref (plist-get (agent-repl-host--entry ws) :stream-due)))
        (agent-repl-host--put ws :stream-due nil)
        (agent-repl-host--move-stream ws new ref)
        (agent-repl--info ws "elisp.host.stream-moved ws={ws} from={from} conn={conn} address={address}"
                          :ws ws :from (agent-repl-host--conn-id old) :conn (agent-repl-host--conn-id new) :address (prin1-to-string (agent-repl-connect-connection-address new)))))
    moved))

(defun agent-repl-host--reselect-after-reattach (ws)
  "Send WS\='s selection again when a handover refused it and WS now stands.
See `agent-repl-host--reselect-on-reattach'."
  (when (equal ws agent-repl-host--reselect-on-reattach)
    (setq agent-repl-host--reselect-on-reattach nil)
    (agent-repl--info ws "elisp.host.reselect-where-served ws={ws} address={address}"
                      :ws ws :address (prin1-to-string (let ((conn (agent-repl-host-conn ws)))
                           (and conn (agent-repl-connect-connection-address conn)))))
    (agent-repl-host-select ws)))

(defun agent-repl-host--register-onto (ws new token trigger on-settled)
  "The `:register' claim of reattach TOKEN: register WS\='s dir on NEW.
Registration is idempotent by dir, so the daemon that now serves Emacs
hands back the ref it knows WS by, whether it adopted WS\='s shim from a
daemon that died, took it over at a promotion, or is a fresh daemon after
an outage.  On success the walk finishes (`agent-repl-host--reattached')
and the webview is reloaded AFTER the subscribe, because frontend.el
derives the page URL from `agent-repl-host-conn' and the subscribe is what
moved it.  A refusal or a transport failure leaves WS detached with its
next walk armed (`agent-repl-host--register-settled').  TRIGGER is recorded;
ON-SETTLED is called once the walk has an outcome."
  (let ((dir (agent-repl--ws-get ws :project-dir))
        (settle (lambda () (when on-settled (funcall on-settled)))))
    (if (null dir)
        (progn
          (agent-repl-host--put ws :reattach nil)
          (agent-repl--error ws "elisp.host.reattach-without-dir ws={ws} trigger={trigger}" :ws ws :trigger trigger)
          (funcall settle))
      (agent-repl-host-register
       new dir
       (lambda (ref)
         (agent-repl-host--register-settled ws new token trigger dir ref)
         (funcall settle))
       ws))))

(defun agent-repl-host--register-settled (ws new token trigger dir ref)
  "Finish WS\='s `:register' walk TOKEN onto NEW, which answered DIR with REF.
REF nil is a registration that did not land (a refusal, a fault, a
transport failure), already recorded by whoever read it.  Shared by the
lone walk (`agent-repl-host--register-onto') and each workspace of a
batch (`agent-repl-host--reattach-all').

A WALK THAT FAILED LEAVES WS DETACHED WITH ITS NEXT WALK ARMED.  WS is
marked `:detached', and every `:detached' workspace is walked again by
each of three edges it is certain to meet before it is used: the next
link-up (`agent-repl-host-on-link-up'), the next promotion
\(`agent-repl-host-on-link-promote') and its own selection
\(`agent-repl-host-select').  Regression, 2026-11-03: a failed walk
cleared its walk and left nothing armed, and seven workspaces stayed
severed until a hand-triggered walk.

A WALK THAT FAILED AFTER A PROMOTION PASSED WALKS AT ONCE onto the live
daemon (`agent-repl-host--follow-live-daemon'): the promotion edge it
would be armed for has already run, and walked only the workspaces
detached or standing on the old daemon then, which WS, mid-walk, was
neither.  Whether a promotion passed is read from the link\='s promotion
epoch the walk recorded when it began (`agent-repl-link-promotions').
Regression, 2026-11-06: a `not_yet_adopted' re-registration onto the
incumbent failed after the successor was promoted, and `main
\(explanation-engine)' stood with no host stream for 20 minutes."
  (let ((walk (plist-get (agent-repl-host--entry ws) :reattach)))
    (cond
     ((not (agent-repl-host--reattach-current-p ws token))
      (agent-repl--log ws "elisp.host.reattach-superseded ws={ws} trigger={trigger}" :ws ws :trigger trigger))
     ;; THE DAEMON HANDED WS TO ITS SUCCESSOR, and the refusal's walk is
     ;; waiting for that successor to be accepted (`:adopt-pending',
     ;; the adopt on the primary link).  WS is not detached: the
     ;; acceptance walks it, so this walk ends without arming another.  A
     ;; successor already accepted took a NEW walk at once, and the branch
     ;; above dropped this one as superseded.
     ((and (null ref) (plist-get (agent-repl-host--entry ws) :adopt-pending))
      (agent-repl-host--put ws :reattach nil)
      (agent-repl--info ws "elisp.host.{trigger}-register-handed-over ws={ws} dir={dir} address={address}" :trigger trigger
                        :ws ws :dir (prin1-to-string dir) :address (prin1-to-string (agent-repl-connect-connection-address new))))
     ;; THE SLUG NAMES THE TRIGGER (`elisp.host.link-up-register-failed',
     ;; `elisp.host.stream-lost-registered', ...), a closed
     ;; vocabulary, so a reader can ask for one edge's walks alone.
     ((null ref)
      (agent-repl-host--put ws :reattach nil)
      (agent-repl-host--put ws :detached t)
      (agent-repl--error ws "elisp.host.{trigger}-register-failed ws={ws} dir={dir} address={address}" :trigger trigger
                         :ws ws :dir (prin1-to-string dir) :address (prin1-to-string (agent-repl-connect-connection-address new)))
      (if (agent-repl-host--promoted-since-p ws walk)
          (progn
            (agent-repl--info ws "elisp.host.reattach-after-promotion ws={ws} trigger={trigger} promotions={began}..{now}"
                              :ws ws :trigger trigger :began (plist-get walk :promotions) :now (agent-repl-link-promotions))
            (agent-repl-host--follow-live-daemon ws "promotion-passed"))
        (agent-repl--info ws "elisp.host.reattach-armed ws={ws} trigger={trigger} edges=link-up,promotion,select"
                          :ws ws :trigger trigger)))
     ;; THE PAGE IS LEFT AS IT IS: every daemon serves one shared address,
     ;; so the page's own streams reopen there by themselves.
     (t
      (agent-repl-host--landed
       ws trigger (lambda () (agent-repl-host--reattached ws new ref trigger)))
      (agent-repl--info ws "elisp.host.{trigger}-registered ws={ws} address={address}" :trigger trigger
                        :ws ws :address (prin1-to-string (agent-repl-connect-connection-address new)))))))

(defun agent-repl-host--promoted-since-p (ws walk)
  "Return non-nil when a promotion passed since WS\='s reattach WALK began.
WALK is the plist `agent-repl-host--begin-walk' recorded, whose
`:promotions' is the link\='s promotion epoch then.  A walk with no epoch
recorded is an invariant breach, failed hard."
  (let ((began (plist-get walk :promotions)))
    (unless (integerp began)
      (agent-repl--fatal ws "elisp.host.walk-without-promotion-epoch ws={ws} walk={walk}" :ws ws :walk (prin1-to-string walk)))
    (/= began (agent-repl-link-promotions))))

(defun agent-repl-host--follow-live-daemon (ws trigger)
  "Re-attach WS, which lost its daemon, to the LIVE one -- or wait for it.
TRIGGER names how the loss was found (`stream-lost', `dead-connection',
`planned-ending').

THE LIVE DAEMON IS THE LINK\='S (`agent-repl-link-live'), the one source
the primary itself is resolved from, so a workspace never follows an
address of its own.  WS is marked `:detached' until a walk finishes, and
the link\='s edges walk every detached workspace:

  - no link stands: nothing is dialed and nothing polls; the link-up edge
    re-attaches WS (`agent-repl-host-on-link-up');
  - a walk onto the live daemon, or onto the successor it is handing over
    to, is already in flight: nothing more.  A handover\='s adopt moves
    WS onto the successor before the old daemon\='s stream ends planned,
    and that ending must not register WS back on the daemon that is
    leaving;
  - the live daemon IS the one WS lost, and it is handing over: its
    successor\='s promotion re-attaches WS (`agent-repl-host-on-link-promote')
    -- registering on a daemon that is leaving would only fail;
  - the live daemon announced its OWN ending -- WS\='s stream on it ended
    planned, or the link\='s `WatchDaemon' carried the ending
    (`agent-repl-link-departing-p') -- with no successor standing: the
    link-down edge that ending is owed, then the link-up edge onto the next
    daemon, re-attaches WS (`agent-repl-host-on-link-up').  The streams end
    in no fixed order, so a workspace\='s planned end routinely arrives while
    the link still names the departing daemon;
  - otherwise WS is walked onto the live daemon now.

Regression, 2026-10-03: a bounce\='s announced stand-down registered every
workspace on the daemon that had just said it was leaving, and each one
failed (`elisp.host.planned-ending-register-failed', with the connect
and rpc failures under it) a millisecond before the link went down.
Regression, 2026-10-22: the same failure, for a workspace whose adopt onto
the successor was already in flight when the old stream ended planned."
  (let ((live (agent-repl-link-live))
        (walk (plist-get (agent-repl-host--entry ws) :reattach)))
    (agent-repl-host--put ws :detached t)
    (cond
     ((null live)
      (agent-repl--info ws "elisp.host.reattach-awaiting-link ws={ws} trigger={trigger}" :ws ws :trigger trigger))
     ((and walk (memq (plist-get walk :conn)
                      (delq nil (list live (agent-repl-link-successor)))))
      (agent-repl--log ws "elisp.host.reattach-in-flight ws={ws} trigger={trigger} address={address}"
                       :ws ws :trigger trigger :address (prin1-to-string (agent-repl-connect-connection-address live))))
     ((and (eq live (agent-repl-host--recorded-conn ws))
           (or (agent-repl-link-successor) (agent-repl-link-successor-pending-p)))
      (agent-repl--info ws "elisp.host.reattach-awaiting-promotion ws={ws} trigger={trigger} address={address}"
                        :ws ws :trigger trigger :address (prin1-to-string (agent-repl-connect-connection-address live))))
     ((agent-repl-link-departing-p
       live (and (equal trigger "planned-ending") (agent-repl-host--recorded-conn ws)))
      (agent-repl--info ws "elisp.host.reattach-awaiting-link-edge ws={ws} trigger={trigger} address={address}"
                        :ws ws :trigger trigger :address (prin1-to-string (agent-repl-connect-connection-address live))))
     (t
      (agent-repl-host--reattach ws live :register trigger)))))

;;;; ---- The handover, per workspace ----

;;;; ---- The call gate: a workspace's calls wait for its adopt's answer ----
;;
;; WHILE WS\='S ADOPT ONTO THE SUCCESSOR IS UNANSWERED, ITS CALLS ARE HELD.
;; The adopt walk moves `:conn' to the successor BEFORE AdoptHostWorkspace
;; is answered (`agent-repl-host--adopt-onto'), so every call about WS made
;; in that window reached a daemon that had not adopted WS yet and was
;; refused `not_yet_adopted', and what it reported was lost: in the
;; 2026-11-13 14:42 deploy, ReportPageUnloading,
;; ReportUserInteraction and SelectWorkspace for the four workspaces with a
;; loaded page, 1-3s before their adopt was answered (24 WARN records at the
;; successor).  So every async unary call goes through ONE gate
;; \=(`agent-repl-host-gate-call', from rpc.el\='s `agent-repl-rpc--unary'):
;; a call that names WS and is addressed to the daemon WS\='s outstanding
;; adopt is on is queued on WS, and the queue is sent, IN ORDER, from the
;; adopt\='s own answer (`agent-repl-host--release-held').
;;
;; NOTHING WAITS.  A held call returns at once and its callbacks run when
;; it is sent, so the editor and its own UI act immediately; only the
;; daemon hears later.  THE HOLD IS BOUNDED BY THE ADOPT\='S OWN DEADLINE:
;; a refused, failed or timed-out adopt still sends every held call, at the
;; daemon WS stands on afterwards, and the failure is the adopt\='s own
;; ERROR record.

(defconst agent-repl-host--never-held-methods '("AdoptHostWorkspace")
  "Methods the call gate never holds: the adopt, whose answer ends the hold.")

(defun agent-repl-host-gate-call (conn method request send fail)
  "Send REQUEST\='s call to METHOD on CONN now, or hold it for its workspace.
THE ONE GATE every async unary call passes (rpc.el `agent-repl-rpc--unary').
SEND is a function of the connection that sends the call; FAIL is a
function of a transport failure plist that answers the call\='s ON-FAILURE.
A call naming a workspace (`:workspace' ref) whose adopt onto CONN is
unanswered is held (`agent-repl-host--hold-call'); every other call is sent
at once, and a send that signals signals to the caller, as it always did."
  (let ((ws (and (not (member method agent-repl-host--never-held-methods))
                 (agent-repl-host--adopt-holding
                  conn (plist-get (plist-get request :workspace) :id)))))
    (if ws
        (agent-repl-host--hold-call ws method send fail)
      (funcall send conn))))

(defun agent-repl-host--adopt-holding (conn id)
  "Return the workspace whose ref is ID and whose adopt onto CONN is unanswered.
Nil when ID is nil, or no workspace\='s outstanding adopt walk is on CONN."
  (when id
    (catch 'held
      (maphash
       (lambda (ws entry)
         (let ((walk (plist-get entry :reattach)))
           (when (and (equal (plist-get (plist-get entry :ref) :id) id)
                      (eq (plist-get walk :claim) :adopt)
                      (eq (plist-get walk :conn) conn))
             (throw 'held ws))))
       agent-repl-host--by-name)
      nil)))

(defun agent-repl-host--hold-call (ws method send fail)
  "Queue METHOD\='s SEND and FAIL on WS until its adopt is answered."
  (let ((held (append (plist-get (agent-repl-host--entry ws) :held-calls)
                      (list (list :method method :send send :fail fail)))))
    (agent-repl-host--put ws :held-calls held)
    (agent-repl--info ws "elisp.host.call-held ws={ws} method={method} held={held}" :ws ws :method method :held (length held))))

(defun agent-repl-host--release-held (ws outcome &optional to)
  "Send every call held for WS, in the order it was made.
Its adopt is OUTCOME, which names how the hold ended (`adopted',
`refused', `failed', `superseded', `withdrawn', `forgotten').  Each call
goes to TO when given \(a retrying adopt names the daemon that still
serves WS), and otherwise to the daemon WS stands on NOW
\(`agent-repl-host-conn-or-primary'): the successor once WS is adopted,
the daemon it was put back on otherwise.  A call with no daemon to go
to, and a send that signals, answers its own ON-FAILURE, at ERROR, and
the calls after it are still sent."
  (let ((held (plist-get (agent-repl-host--entry ws) :held-calls)))
    (when held
      (agent-repl-host--put ws :held-calls nil)
      (agent-repl--info ws "elisp.host.held-calls-sent ws={ws} count={count} outcome={outcome}"
                        :ws ws :count (length held) :outcome outcome)
      (dolist (call held)
        (let ((conn (or to (agent-repl-host-conn-or-primary ws)))
              (method (plist-get call :method)))
          (if (null conn)
              (progn
                (agent-repl--error ws "elisp.host.held-call-unsent ws={ws} method={method} outcome={outcome} reason=no-connection"
                                   :ws ws :method method :outcome outcome)
                (funcall (plist-get call :fail)
                         (list :kind :transport :code nil :status nil
                               :message (format "%s: no daemon connection for the held call" method))))
            (condition-case err
                (funcall (plist-get call :send) conn)
              (agent-repl-connect-error
               (agent-repl--error ws "elisp.host.held-call-unsent ws={ws} method={method} outcome={outcome} error={error}"
                                  :ws ws :method method :outcome outcome :error (prin1-to-string err))
               (funcall (plist-get call :fail) (cadr err))))))))))

(defun agent-repl-host--adopt-onto (ws new token trigger)
  "The `:adopt' claim of reattach TOKEN: adopt WS onto the NEW daemon.
Moves WS\='s webview there and re-subscribes on the adopt\='s success.
Shared by the `transferred' push and by a per-workspace rpc's
`transferring_away' refusal — both say the same thing, and a second copy
of this order would be a second contract.  TRIGGER is recorded.

THE ORDER IS THE CONTRACT.  `:conn' is moved to NEW and the webview is
reloaded FIRST; then AdoptHostWorkspace is issued on the NEW connection;
then, on its success, the old stream is cancelled and the subscription is
opened on NEW.  `:conn' MUST move before the reload because frontend.el
derives the page URL from `agent-repl-host-conn' — reloading first would
navigate the webview straight back at the daemon that just released the
workspace.

THE REDIAL MUST NOT WAIT ON THE ADOPT'S ANSWER, AND THAT WAS A DEADLOCK.
The successor's adopt is a RENDEZVOUS: it completes only when every
participant its predecessor snapshotted at announcement has called, and
for an OPEN workspace those are the host AND the web page
\(`daemon/internal/rollout/adopt.go', `rendezvousCall').  The web
participant is the RELOADED page — the webapp never redials a successor
of its own, so the host is the only actor that can produce it.  Issuing
AdoptHostWorkspace first and reloading only on its success therefore made
the host's call wait for a page that only the host's call returning could
create.  Measured in the sandbox, on a handover of a workspace with a
live panel: `AdoptHostWorkspace timed out after 10s', the successor
recorded `the caller gave up before the rendezvous completed', and the
outgoing daemon then sat out its whole 30s adoption window before exiting
— so the promotion Emacs makes on the primary stream's close never came.
A headless workspace never showed it: it has no web participant, so the
rendezvous was satisfied by the host's call alone.

So the reload is issued BESIDE the adopt rather than after it, and the
two participants call concurrently, which is what the rendezvous
documents it requires.  `agent-repl-frontend-reload-webview' answers nil
for a workspace with no live webview, so a headless adoption walks
exactly as it did.

NOTHING IS LEFT MOVED BY A FAILED ADOPT.  The old stream is kept standing
on every failure path — a workspace whose old stream was dropped and
whose adopt did not land would be served by nobody — and `:conn' and the
webview are put back where they were, so a refusal or a transport failure
leaves the workspace on the daemon that still serves it.

A TRANSPORT FAILURE IS RETRIED, AND NOT RETRYING IT COST THE WHOLE
ADOPTION WINDOW.  The adopt is a unary call under its own bound,
`agent-repl-host-adopt-timeout-seconds', while the
rendezvous it joins completes only when EVERY participant has called —
so on a loaded box this call can expire on a rendezvous that is merely
still waiting for the page, and a dropped socket does the same thing.
Left there, the workspace was never adopted by anybody: the successor
adopts headless workspaces itself and RETRIES them
\(`daemon/internal/rollout/adopt.go', `retryHeadless'), but a workspace
with participants has no retry of its own — the participant IS the retry.
The outgoing daemon then waits out its full `DefaultAdoptionWindow' (30s)
for it, which delays its exit, which delays the primary stream close
Emacs promotes on — `TestEmacsHandoverTransfersAtFreeness' missing a 21s
bound on a promotion healthy runs make in ~2s.

So a transport failure re-issues the adopt CALL after
`agent-repl-host-handover-retry-delay' (`agent-repl-host--adopt-call'),
exactly as a `not_yet_adopted' refusal already does.  A RETRY RELOADS
NOTHING: WS stays moved onto NEW, page and all, because the page there IS
the rendezvous\='s web participant; only the walk\='s end puts `:conn' and
the page back, once.  Regression, 2026-11-15: each attempt restored the
page and the next reloaded it again, two reloads per attempt for every
workspace, ~800k reloads that kept Emacs too busy to read the very stream
ends that would have ended the loop.  The retry STOPS
\(`agent-repl-host--adopt-retryable-p') when NEW is no longer the standing
successor (a promotion or a lost successor stream clears it), when NEW\='s
connection is closed, and when the dial was refused outright: no daemon
listens at NEW then, and there is nobody left to adopt onto.  A refusal
is not retried here — the daemon answered, and its arms have their own
walk."
  (let ((old (agent-repl-host--recorded-conn ws)))
    (cond
     ((not (agent-repl-host--reattach-current-p ws token))
      ;; A walk a newer one has since superseded.
      (agent-repl--log ws "elisp.host.reattach-superseded ws={ws} trigger={trigger}" :ws ws :trigger trigger))
     ((null (agent-repl-host-ref ws))
      (agent-repl-host--put ws :reattach nil)
      (agent-repl--error ws "elisp.host.adopt-without-ref ws={ws}" :ws ws))
     (t
      ;; THE WALK REMEMBERS WHERE IT CAME FROM, so a withdrawn transfer can
      ;; put WS back while this adopt is still unanswered
      ;; (`agent-repl-host--transfer-withdrawn').
      (agent-repl-host--put ws :reattach
                            (plist-put (plist-get (agent-repl-host--entry ws) :reattach)
                                       :from old))
      (agent-repl-host--put ws :conn new)
      (agent-repl-host--adopt-call ws new token trigger old)))))

(defun agent-repl-host--adopt-retryable-p (new detail)
  "Answer t when an adopt onto NEW that failed with DETAIL is retried.
Otherwise answer why it stops: `not-primary' when NEW is no longer the
primary link (every adopt goes out on the one shared address), `closed'
when NEW\='s connection is closed, and `unreached' when the dial was
refused, so no daemon listens at NEW."
  (cond
   ((not (eq new (agent-repl-link-primary))) 'not-primary)
   ((not (agent-repl-connect-connection-alive-p new)) 'closed)
   ((agent-repl-connect-failure-unreached-p detail) 'unreached)
   (t t)))

(defconst agent-repl-host-adopt-timeout-seconds 30
  "How long an AdoptHostWorkspace call may wait for the daemon's answer.
The adopt joins a rendezvous the successor answers only once every
participant has called and the workspace is adopted, so it waits as long
as the handover's adoption does.  Measured from each `reattach' to its
`adopted' over the twelve handovers of 2026-11-14 and 2026-11-15 (114 to
142 workspaces each): p90 3.5 to 11.1s, the worst 13.6s (11-14 18:13).
The generic `agent-repl-connect-unary-timeout-seconds' (10s) sat below
that tail, so every handover timed the slow adopts out at the instant the
daemon answered them, recorded each at ERROR and adopted it again (36
workspaces at 12:55, 2026-11-15).  30s is a little over twice the worst.")

(defun agent-repl-host--adopt-call (ws new token trigger old)
  "Issue AdoptHostWorkspace for WS on NEW, for reattach walk TOKEN.
WS already stands moved onto NEW (`agent-repl-host--adopt-onto'); OLD is
the daemon it came from, which every ending but success puts it back on.
A transport failure retries THIS call and nothing more.  The call runs in
WS\='s own log context, so its transport records land in WS\='s log, not
in whichever workspace is current when a retry timer fires (2026-11-15:
every workspace\='s retries filled one bystander\='s rotation).  TRIGGER
is recorded."
  (let ((ref (agent-repl-host-ref ws))
        (address (agent-repl-connect-connection-address new))
        (restore
         (lambda ()
           (agent-repl-host--put ws :conn old)
           (agent-repl--info ws "elisp.host.restored ws={ws} address={address}"
                             :ws ws :address (prin1-to-string (and old (agent-repl-connect-connection-address old)))))))
    (if (not (agent-repl-host--reattach-current-p ws token))
        (agent-repl--log ws "elisp.host.reattach-superseded ws={ws} trigger={trigger} at=adopt" :ws ws :trigger trigger)
      (agent-repl--with-log-context
       ws nil
       (lambda ()
         (agent-repl-rpc-adopt-host-workspace
          new (list :workspace ref)
          :timeout agent-repl-host-adopt-timeout-seconds
          :on-response
          (lambda (response)
            (cond
             ((not (agent-repl-host--reattach-current-p ws token))
              (agent-repl--log ws "elisp.host.reattach-superseded ws={ws} trigger={trigger} at=adopt-answered" :ws ws :trigger trigger))
             (t
              (pcase (plist-get response :arm)
                (:success
                 (agent-repl--info ws "elisp.host.adopted ws={ws} address={address} conn={conn}"
                                   :ws ws :address (prin1-to-string address) :conn (agent-repl-host--conn-id new))
                 (agent-repl-host--landed
                  ws trigger (lambda () (agent-repl-host--adopt-settled ws new ref trigger)))
                 (agent-repl-host--release-held ws "adopted"))
                (:error
                 (funcall restore)
                 (agent-repl-host--put ws :reattach nil)
                 (agent-repl-host--on-refused ws "adopt" (plist-get response :value))
                 (agent-repl-host--release-held ws "refused"))
                (arm
                 (funcall restore)
                 (agent-repl-host--put ws :reattach nil)
                 (agent-repl--error ws "elisp.host.adopt-unknown-arm ws={ws} arm={arm}" :ws ws :arm (prin1-to-string arm))
                 (agent-repl-host--release-held ws "refused"))))))
          :on-failure
          (lambda (detail)
            (if (not (agent-repl-host--reattach-current-p ws token))
                (agent-repl--log ws "elisp.host.reattach-superseded ws={ws} trigger={trigger} at=adopt-failed" :ws ws :trigger trigger)
              (let ((retry (agent-repl-host--adopt-retryable-p new detail)))
                (cond
                 ((eq retry t)
                  ;; THE HOLD ENDS WITH THE ADOPT'S DEADLINE: the calls held
                  ;; for WS go now, to the daemon that still serves it, while
                  ;; the retry keeps WS and its page on NEW.
                  (agent-repl-host--release-held
                   ws "failed" (and old (agent-repl-connect-connection-alive-p old) old))
                  (agent-repl--error ws "elisp.host.adopt-failed ws={ws} detail={detail} retrying=t"
                                     :ws ws :detail (prin1-to-string detail))
                  (run-at-time agent-repl-host-handover-retry-delay nil
                               #'agent-repl-host--adopt-call ws new token trigger old))
                 (t
                  (funcall restore)
                  (agent-repl-host--release-held ws "failed")
                  (let ((primary (agent-repl-link-primary)))
                    (cond
                     ;; THE PRIMARY WAS PROMOTED WHILE THIS ADOPT WAS IN
                     ;; FLIGHT: the shared address's redial reached the next
                     ;; daemon, and NEW, the link the adopt went out on, is
                     ;; the old one the promotion closes.  Restored onto it,
                     ;; WS was stranded with no walk left (2026-10-25: six
                     ;; workspaces).  The primary serves every workspace now,
                     ;; so WS registers there.
                     ((and primary (not (eq new primary)))
                      (agent-repl--error ws "elisp.host.adopt-failed ws={ws} detail={detail} retrying=nil registering=t"
                                         :ws ws :detail (prin1-to-string detail))
                      (agent-repl-host--reattach ws primary :register "adopt-after-promotion"))
                     (t
                      (agent-repl-host--put ws :reattach nil)
                      (agent-repl--error ws "elisp.host.adopt-failed ws={ws} detail={detail} retrying=nil stopped={stopped}"
                                         :ws ws :detail (prin1-to-string detail) :stopped retry)))))))))))))))

(defun agent-repl-host--transferred (ws &optional _value)
  "Adopt WS onto the daemon its old one handed it to, after the release.

`HostWorkspaceTransferred' NAMES NO DAEMON and carries nothing.

EVERY DAEMON SERVES ONE SHARED ADDRESS, the one its socket holder keeps
\=(docs/investigations/2026-11-18-stable-sockets-design.md), so the adopt
goes out on the primary link at once: the daemon that accepts it forwards
it to the successor, and no second link is dialed or waited on.

A HANDOVER TOUCHES NO PAGE (owner ruling, 2026-11-22): WS\='s page is
never reloaded, loaded or dropped for it.  The page moves itself on its
own transfer notice (`WebWorkspaceTransferred'): on the successor\='s
webapp build it adopts WS in place, and on another it goes quiet until the
deploy reloads the host.

`transferred\=' is a PUSH, never a terminal frame, so the old stream stays
standing until the adopt lands and the shared walk cancels it."
  (agent-repl--info ws "elisp.host.transferred ws={ws} adopting=t" :ws ws)
  (agent-repl-host--reattach ws (agent-repl-link-primary) :adopt "transferred"))

;;;; ---- Handover refusals answered by a per-workspace rpc ----

(defun agent-repl-host--transfer-withdrawn (ws)
  "Keep WS on the daemon that took it back: its transfer was withdrawn.

THE OUTGOING DAEMON TOOK WS BACK, so there is nothing to adopt until its
next `transferred' push.  The adopt walk that notice began is ended, so a
scheduled retry and a late answer both find themselves superseded, and
WS\='s connection goes back to where the walk took it from; its page,
which no handover touches, re-adopts by itself.
Regression, 2026-10-26: never told, Emacs re-adopted on the successor
every 10s for as long as the workspace\='s turn ran, reloading its page
each time and recording an ERROR for every adopt."
  (let* ((walk (plist-get (agent-repl-host--entry ws) :reattach))
         (from (plist-get walk :from))
         (pending (plist-get (agent-repl-host--entry ws) :adopt-pending)))
    (agent-repl-host--put ws :adopt-pending nil)
    (cond
     ((eq (plist-get walk :claim) :adopt)
      (agent-repl-host--put ws :reattach nil)
      (when (and from (not (eq from (agent-repl-host--recorded-conn ws))))
        (agent-repl-host--put ws :conn from))
      (agent-repl-host--release-held ws "withdrawn")
      (agent-repl--info ws "elisp.host.transfer-withdrawn ws={ws} address={address} adopt=ended"
                        :ws ws :address (prin1-to-string (and from (agent-repl-connect-connection-address from)))))
     (pending
      (agent-repl--info ws "elisp.host.transfer-withdrawn ws={ws} adopt=never-sent" :ws ws))
     (t
      (agent-repl--info ws "elisp.host.transfer-withdrawn ws={ws} adopt=none" :ws ws)))))

(defun agent-repl-host-route-handover (ws arm slug)
  "Hand ARM, a per-workspace rpc's refusal for WS, to the handover walk.
Answers non-nil when ARM is one of the two handover arms
\(`agent-repl-host--handover-arms'), after logging it at INFO under the
operation SLUG with its keyword and fields and calling
`agent-repl-host-handle-refusal'; answers nil, doing nothing, for any
other arm, which stays the caller's to report.  THE ONE ROUTING every
verb's refusal handler shares, so none hand-rolls the membership test."
  (let ((keyword (plist-get arm :arm)))
    (when (memq keyword agent-repl-host--handover-arms)
      (agent-repl--info ws "{slug} ws={ws} arm={arm} fields={fields}"
                        :slug slug :ws ws :arm (prin1-to-string keyword)
                        :fields (prin1-to-string (plist-get arm :value)))
      (agent-repl-host-handle-refusal ws arm)
      t)))

(defun agent-repl-host-handle-refusal (ws arm-plist)
  "React to a per-workspace rpc's handover refusal ARM-PLIST for WS.
The daemon answers the VERB with the handover, so the refusal is news, not
an error.  EVERY DAEMON SERVES ONE SHARED ADDRESS (docs/investigations/
2026-11-18-stable-sockets-design.md), so either arm means one thing: WS
is being adopted by the daemon the shared address reaches, and the adopt
goes out on the primary link, which forwards it there.
`(:arm :transferring-away :value (:address A))' names the shared address;
one naming none is a contract breach.  `(:arm :not-yet-adopted)' is the
adopting daemon answering before its adoption landed.  Any other arm is
not a handover refusal and is a contract breach.

The adopt\='s own answer comes back through the adopt path\='s arms, never
through here, so a repeated refusal cannot recurse into a loop."
  (let ((arm (plist-get arm-plist :arm))
        (value (plist-get arm-plist :value)))
    (pcase arm
      (:transferring-away
       (let ((address (plist-get value :address)))
         ;; `address' is a PLAIN string on the wire, so the daemon's zero value
         ;; decodes to the empty string rather than to an absence: both mean
         ;; "no address rode the arm" and both are the same breach.
         (if (or (null address) (string-empty-p address))
             (agent-repl--error ws "elisp.host.transferring-away-without-address ws={ws} address={address}"
                                :ws ws :address (prin1-to-string address))
           (agent-repl--info ws "elisp.host.transferring-away ws={ws} address={address}" :ws ws :address (prin1-to-string address))
           (agent-repl-host--reattach ws (agent-repl-link-primary) :adopt "transferring-away"))))
      (:not-yet-adopted
       (agent-repl--info ws "elisp.host.not-yet-adopted ws={ws}" :ws ws)
       (agent-repl-host--reattach ws (agent-repl-link-primary) :adopt "not-yet-adopted"))
      (_
       (agent-repl--error ws "elisp.host.unknown-refusal-arm ws={ws} arm={arm}" :ws ws :arm (prin1-to-string arm))))))

;;;; ---- The two relayed acts ----

(defun agent-repl-host--reload-webapp (ws value)
  "Reload WS's page against the SAME daemon; VALUE is the arm `(:webapp-build)'.
No address rides the arm, because the daemon is not changing.  A combined
daemon+webapp rollout never sends it — the handover's fresh attach pulls
the new assets by itself.  A deploy names the build it brings, and every
page lives in the one host web view, so the host reloads once per build;
a restart names none and reloads WS's page alone
\(`agent-repl-frontend-reload-webapp')."
  (let ((build (plist-get value :webapp-build)))
    (agent-repl--info ws "elisp.host.reload-webapp ws={ws} build={build}" :ws ws :build build)
    (agent-repl-frontend-reload-webapp ws build)))

(defun agent-repl-host--open-in-editor (ws target)
  "Open TARGET's path through the ONE shared editor-popup subroutine.
A fact, not a command: nothing acks it.  The line is optional and its
absence means the file's top — or a directory, which the popup opens in
dired."
  (let ((path (plist-get target :path))
        (line (plist-get target :line)))
    (agent-repl--info ws "elisp.host.open-in-editor ws={ws} path={path} line={line}" :ws ws :path (prin1-to-string path) :line (prin1-to-string line))
    (agent-repl-popup-open path line)))

(defun agent-repl-host--open-commit-log (ws target)
  "Open TARGET's branch log with point on its commit, for WS.
The click on a landed merge's commit; a fact, not a command, exactly as
`agent-repl-host--open-in-editor' is."
  (let ((dir (plist-get target :repository-dir))
        (branch (plist-get target :branch))
        (commit (plist-get target :commit)))
    (agent-repl--info ws "elisp.host.open-commit-log ws={ws} dir={dir} branch={branch} commit={commit}"
                      :ws ws :dir (prin1-to-string dir) :branch (prin1-to-string branch) :commit (prin1-to-string commit))
    (agent-repl-magit-log-commit ws dir branch commit)))

(defun agent-repl-host--run-in-terminal (ws target)
  "Run TARGET's command in the terminal popup, for WS.
The click on a shell code block's run control; a fact, not a command,
exactly as `agent-repl-host--open-in-editor' is."
  (agent-repl-terminal-run ws (plist-get target :command)))

;;;; ---- Editor evaluation ----

(defun agent-repl-host--evaluation-buffer (ref)
  "Return the buffer an evaluation for the workspace REF names runs in.
The workspace\='s composer when this editor holds it and its composer is
live, the buffer the agent\='s workspace types into.  Otherwise the
editor\='s own evaluation buffer, whose `default-directory' is the
workspace\='s directory: a workspace this editor does not hold
\(frontend.v1.RosterRowVisibility) has no buffer of its own, and the
evaluation must not run in whatever buffer the user happens to be in."
  (let* ((ws (agent-repl-host--held-name (plist-get ref :id)))
         (composer (and ws (agent-repl--ws-get ws :input-buffer))))
    (if (buffer-live-p composer)
        composer
      (let ((buffer (get-buffer-create " *agent-repl-evaluation*"))
            (dir (plist-get ref :dir)))
        (with-current-buffer buffer
          (setq default-directory (file-name-as-directory
                                   (or (and dir (not (string-empty-p dir)) dir) "~/"))))
        buffer))))

(defun agent-repl-host-evaluate (conn request)
  "Evaluate REQUEST's elisp for an agent and answer on CONN with its token.
REQUEST is the decoded `DaemonEvaluate' plist (:workspace REF :token TOKEN
:text TEXT), pushed on CONN\='s daemon stream for the workspace REF names,
whether or not this editor holds it.  The text is evaluated by the
editor\='s one handler (`agent-repl-evaluate-text') in the buffer
`agent-repl-host--evaluation-buffer' chooses, and the record says which.
The answer goes to CONN, the daemon that minted the token."
  (let* ((ref (plist-get request :workspace))
         (scope (agent-repl-host--target-scope ref))
         (token (plist-get request :token))
         (buffer (agent-repl-host--evaluation-buffer ref))
         (outcome (with-current-buffer buffer
                    (agent-repl-evaluate-text (plist-get request :text))))
         (arm (plist-get (plist-get outcome :outcome) :arm)))
    (agent-repl--info scope "elisp.host.evaluate id={id} token={token} buffer={buffer} outcome={outcome} messages={messages}"
                      :id (plist-get ref :id) :token (plist-get token :value) :buffer (prin1-to-string (buffer-name buffer)) :outcome arm :messages (length (plist-get outcome :messages)))
    (agent-repl-rpc-answer-editor-evaluation
     conn (list :workspace ref :token token :outcome outcome)
     :on-response
     (lambda (response)
       (pcase (plist-get response :arm)
         (:success
          (agent-repl--log scope "elisp.host.evaluation-answered id={id} token={token}"
                           :id (plist-get ref :id) :token (plist-get token :value)))
         (:error
          (agent-repl--error scope "elisp.host.evaluation-answer-refused id={id} token={token} cause={cause}"
                             :id (plist-get ref :id) :token (plist-get token :value) :cause (prin1-to-string (plist-get (plist-get (plist-get response :value) :cause) :arm))))
         (other
          (agent-repl--error scope "elisp.host.evaluation-answer-unknown-arm id={id} token={token} arm={arm}"
                             :id (plist-get ref :id) :token (plist-get token :value) :arm (prin1-to-string other)))))
     :on-failure
     (lambda (detail)
       (agent-repl--error scope "elisp.host.evaluation-answer-failed id={id} token={token} detail={detail}"
                          :id (plist-get ref :id) :token (plist-get token :value) :detail (prin1-to-string detail))))))

;;;; ---- Link lifecycle ----

(defun agent-repl-host--selected-dir ()
  "Return the project dir the user\='s selection stands on, or nil.
THE DIR, NOT THE ID, IS WHAT SURVIVES A DAEMON RESTART: a relaunched
daemon re-mints every `WorkspaceRef\=' id, so the id Emacs held before the
link went down names nothing afterwards, while the directory the user
stood in is the same directory the re-registration hands back.

THE NEWEST REQUEST WINS: a selection still pending
\(`agent-repl-host-pending-selection') is the user\='s newest choice, and
it is looked up FIRST.  A select nobody has answered yet is recorded as
the selection whichever way it ends (`agent-repl-host--select-failed'),
but its failure can land AFTER the link-up that reads this, and reading
only the recorded id then re-asserted the selection the user had already
left (2026-10-15, e2e TestEmacsRestartKeepsTheSelectedWorkspace: the
select of the workspace the user stood on was still in flight when the
daemon stopped).  The id Emacs last had recorded comes next, and the tab
the frame is on is the fallback."
  (let ((pending (agent-repl-host-pending-selection-ref)))
    (if pending
        ;; A PENDING WORKSPACE THIS EDITOR DOES NOT HOLD names its own dir.
        (let ((ws (agent-repl-host--held-name (plist-get pending :id))))
          (if ws (agent-repl--ws-get ws :project-dir) (plist-get pending :dir)))
      (let ((ws (or (and agent-repl-host-last-selected-id
                         (agent-repl--ws-by-ref-id agent-repl-host-last-selected-id))
                    (agent-repl--ws-current-name))))
        (and ws (agent-repl--ws-get ws :project-dir))))))

(defun agent-repl-host--ws-for-dir (dir)
  "Return the live workspace registered at DIR, or nil when none came back.
A ref is required: a workspace the new daemon refused to register has no
identity to select."
  (and dir
       (seq-find (lambda (ws)
                   (and (agent-repl-host-ref ws)
                        (equal dir (agent-repl--ws-get ws :project-dir))))
                 (agent-repl--live-ws-names))))

(defun agent-repl-host--reassert-selection (dir)
  "Re-assert DIR as the user\='s selection once re-registration has settled.
The daemon that came back stamped `current\=' on whichever workspace
re-registered first — an order Emacs happens to walk in, never a choice
the user made — so Emacs says again what the user chose.  Clearing
`agent-repl-host-reselect-pending\=' is what re-arms
`agent-repl-roster-apply-current\=', and every arm below clears it: a
suppression that outlived its re-select would deafen Emacs to the user\='s
next sidebar click."
  (cond
   ((null dir)
    (setq agent-repl-host-reselect-pending nil)
    (agent-repl--log '(:agent-repl-central "the link has no workspace selection")
                     "elisp.host.link-up-reselect-skipped reason=no-selection"))
   ((null (agent-repl-host--ws-for-dir dir))
    ;; The workspace the user stood in did not come back.  The selection
    ;; stays wherever it is — a live workspace — and the loss is reported
    ;; rather than papered over with an arbitrary substitute.
    (setq agent-repl-host-reselect-pending nil)
    (agent-repl--warn '(:agent-repl-central
                        "the lost selection has no registered workspace sink")
                      "elisp.host.link-up-reselect-lost dir={dir} kept={kept}"
                      :dir (prin1-to-string dir) :kept (prin1-to-string (agent-repl--ws-current-name))))
   (t
    (let ((ws (agent-repl-host--ws-for-dir dir)))
      (agent-repl--info ws "elisp.host.link-up-reselect ws={ws} dir={dir}" :ws ws :dir (prin1-to-string dir))
      (unless (equal ws (agent-repl--ws-current-name))
        ;; Restoring the user's own selection is not a new switch by them.
        (let ((agent-repl-host-automatic-switch t))
          (agent-repl--ws-switch ws)))
      (agent-repl-host-select
       ws (lambda (outcome)
            (setq agent-repl-host-reselect-pending nil)
            (agent-repl--log ws "elisp.host.link-up-reselected ws={ws} outcome={outcome}"
                             :ws ws :outcome (prin1-to-string outcome))))))))

(defun agent-repl-host-on-link-up (conn)
  "Re-register, re-subscribe and re-point every live workspace on CONN.
This is the NORMAL path after a daemon restart, not an error recovery:
registration is idempotent by dir, streams are \"now\" and never \"since\",
and no resume token exists anywhere.

THE WEBVIEW IS RE-POINTED TOO, and leaving it out stranded every open
panel.  A daemon Emacs launches listens on a FRESH PORT, and the page\='s
url names the port it was opened at — so after a stop and an ensure the
tab bar healed, the roster came back and the composer worked, while the
webview went on dialing a daemon that had exited and drew a permanent
`daemonUnreachable\=' card over the last feed it had.  Measured in the e2e
sandbox: with both workspaces re-registered and the link up, the page
still reported `failureArms=[daemonUnreachable]\=' at a url naming the
dead daemon\='s port.  The only way out was the user reaching for
`SPC o l\=' — a manual step for a recovery the product otherwise makes on
its own.

The reload runs AFTER the subscribe, because `agent-repl-host-subscribe\='
is what moves `:conn\=' to the new daemon (`agent-repl-host--attach\=') and
frontend.el derives the page url from `agent-repl-host-conn\='.  It is a
no-op for a workspace with no live webview, so a headless one costs
nothing."
  (let* (;; persp-mode's OWN perspectives -- `persp-nil-name' ("none") and
         ;; Doom's initial "main" -- are not workspaces, own no project
         ;; directory and never will, so walking them warned
         ;; `link-up-skipped ws=none reason=no-dir' on every single link-up:
         ;; a warning about a condition that is not a fault, which is the
         ;; fastest way to teach a reader to ignore the ones that are.  They
         ;; are filtered BEFORE the per-workspace refusal, never inside it --
         ;; now at the SOURCE: `agent-repl--live-ws-names' excludes every
         ;; pseudo perspective, so this walk cannot see one.
         (names (agent-repl--live-ws-names))
         (wanted (agent-repl-host--selected-dir))
         (eligible (seq-filter (lambda (ws) (agent-repl--ws-get ws :project-dir)) names)))
    (setq agent-repl-host-reselect-pending wanted)
    (agent-repl--info '(:agent-repl-central "link recovery spans every workspace")
                      "elisp.host.link-up workspaces={workspaces} selection={selection}"
                      :workspaces (length names) :selection (prin1-to-string wanted))
    (dolist (ws names)
      (unless (agent-repl--ws-get ws :project-dir)
        ;; A name with no registered project directory owns no durable
        ;; workspace sink.  This refusal belongs to process-wide link
        ;; repair; preserve the candidate name in the record body.
        (agent-repl--warn '(:agent-repl-central
                            "a link-repair candidate without a directory owns no sink")
                          "elisp.host.link-up-skipped ws={ws} reason=no-dir" :ws ws)))
    ;; ONE BATCHED WALK for every workspace (`agent-repl-host--reattach-all'),
    ;; and the selection is re-asserted once, after every outcome -- at
    ;; once when no workspace had a dir to re-register.
    (agent-repl-host--reattach-all
     eligible conn "link-up"
     (lambda () (agent-repl-host--reassert-selection wanted)))))

(defun agent-repl-host-on-link-down (conn)
  "Forget the streams CONN carried, keeping every workspace's last state.
The pushed state is the newest fact Emacs has; discarding it on a link
death would blank the editor for the length of an outage the reconnect
covers by itself.

THE LEVEL FOLLOWS WHETHER THE DAEMON SAID SO FIRST.  A daemon that
announced its own ending (`agent-repl-link-down-planned') stood down on
purpose, and its link going down is that plan arriving: INFO.  A link that
died with no word from its daemon is a loss the editor has to ride out:
WARN, as it always was."
  (let ((affected 0))
    (maphash
     (lambda (ws entry)
       (when (eq (plist-get entry :conn) conn)
         (setq affected (1+ affected))
         (agent-repl-host--put ws :stream nil)
         (agent-repl-host--put ws :stream-due nil)
         (agent-repl-host--put ws :conn nil)))
     agent-repl-host--by-name)
    (if agent-repl-link-down-planned
        (agent-repl--info '(:agent-repl-central "link loss spans every workspace")
                          "elisp.host.link-down workspaces={workspaces} planned=t" :workspaces affected)
      (agent-repl--warn '(:agent-repl-central "link loss spans every workspace")
                        "elisp.host.link-down workspaces={workspaces} planned=nil" :workspaces affected))))

(defun agent-repl-host-on-link-promote (old new)
  "Re-attach onto NEW every workspace the promotion left behind on OLD.
A promotion rebuilds nothing, because every workspace the old daemon
TRANSFERRED was already adopted onto the successor.  But a daemon can go
without transferring a workspace -- measured 2026-09-27: a restart that
ran in the transfer\='s place, and a daemon that exited with the workspace
still its own -- and such a workspace is still on OLD, whose connection
this promotion is about to close.  Each one, and each one already
detached, is walked onto NEW in one batched registration
\(`agent-repl-host--reattach-all').  Runs from
`agent-repl-link-promote-functions', BEFORE OLD is closed."
  (let* ((moved (agent-repl-host--move-due-streams old new))
         left)
    (maphash (lambda (ws entry)
               (when (and (or (eq (plist-get entry :conn) old)
                              (plist-get entry :detached))
                          (not (member ws moved)))
                 (push ws left)))
             agent-repl-host--by-name)
    (agent-repl--info '(:agent-repl-central "link promotion spans every workspace")
                      "elisp.host.link-promote left-behind={left_behind} moved={moved} old={old} conn={conn} address={address}"
                      :left_behind (length left) :moved (length moved) :old (agent-repl-host--conn-id old) :conn (agent-repl-host--conn-id new) :address (prin1-to-string (agent-repl-connect-connection-address new)))
    (agent-repl-host--reattach-all (nreverse left) new "promotion")))

;; A TORN-DOWN WORKSPACE LEAVES NOTHING BEHIND HERE.  Every teardown of a
;; tab tombstones it through `agent-repl--ws-del', so that is where its host
;; entry dies too: an entry kept past the teardown is re-registered on the
;; next promotion (`agent-repl-host-on-link-promote'), and the daemon refuses
;; a workspace it closed, killed or nuked.  MEASURED, 2026-09-29T17:15:29: two
;; workspaces nuked hours earlier were re-registered on a deploy's promotion
;; and refused with `not-a-worktree'.
(add-hook 'agent-repl-ws-del-hook #'agent-repl-host-forget)
(add-hook 'agent-repl-link-up-functions #'agent-repl-host-on-link-up)
(add-hook 'agent-repl-link-down-functions #'agent-repl-host-on-link-down)
(add-hook 'agent-repl-link-promote-functions #'agent-repl-host-on-link-promote)
(agent-repl--ws-add-activated-hook #'agent-repl-host--on-workspace-activated)

(provide 'host)

;;; host.el ends here
