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

;; Cross-file forward declarations.  These sources load in the dependency
;; order config.el establishes and resolve each other's calls at call time,
;; so the declarations below exist for the byte-compiler alone.
(declare-function agent-repl--input-buffer-name "core")
(declare-function agent-repl-link-successor-pending-p "daemon-link")

(require 'cl-lib)
(require 'subr-x)

(declare-function agent-repl--log "core" (ws fmt &rest args))
(declare-function agent-repl--info "core" (ws fmt &rest args))
(declare-function agent-repl--warn "core" (ws fmt &rest args))
(declare-function agent-repl--error "core" (ws fmt &rest args))
(declare-function agent-repl--fatal "core" (ws fmt &rest args))

(declare-function agent-repl-connect-stream-cancel "connect" (stream))
(declare-function agent-repl-connect-connection-alive-p "connect" (conn))

(declare-function agent-repl-rpc-register-workspace "rpc" (conn request &rest keys))
(declare-function agent-repl-rpc-select-workspace "rpc" (conn request &rest keys))
(declare-function agent-repl-rpc-mark-workspace-viewed "rpc" (conn request &rest keys))
(declare-function agent-repl-rpc-adopt-host-workspace "rpc" (conn request &rest keys))
(declare-function agent-repl-link-dial-successor "daemon-link" (address))
(declare-function agent-repl-connect-connection-address "connect" (conn))
(declare-function agent-repl-rpc-watch-host-workspace "rpc"
                  (conn ref on-push on-close &optional on-open))

(declare-function agent-repl-link-primary "daemon-link" ())
(declare-function agent-repl-link-live "daemon-link" ())
(declare-function agent-repl-link-successor "daemon-link" ())
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

(declare-function agent-repl--notification-activate "notifications" (ws))
(declare-function agent-repl--notification-activate "notifications" (ws))

;; W2-B's surfaces.  NAMED HERE, NEVER DEFINED HERE: status.el owns the
;; blink (whose cadence is frontend.v1 `RosterRowAttention''s), frontend.el
;; owns the webview, popup.el owns the one shared editor popup.
(declare-function agent-repl-frontend-reload-webview "frontend" (ws))
(declare-function agent-repl-popup-open "popup" (path &optional line))

;;;; ---- State ----

(defvar agent-repl-host--by-name (make-hash-table :test 'equal)
  "Workspace name → plist `(:ref REF :conn CONN :stream STREAM :host HOST)'.
REF is the daemon-minted `WorkspaceRef' plist, CONN the connection whose
daemon currently owns the workspace, STREAM its `WatchHostWorkspace'
subscription, HOST the last decoded `HostWorkspace' push (nil until the
first one arrives).")

(defvar agent-repl-host-last-selected-id nil
  "The `WorkspaceRef' id of the last workspace Emacs itself selected.
roster.el compares a daemon-originated `current' change against this so
Emacs's own tab switch is not mistaken for a switch REQUEST (ruling R8).")

(defvar agent-repl-host-reselect-pending nil
  "The project dir Emacs is re-asserting as the user's selection, or nil.
EMACS OWNS THE USER'S SELECTION ACROSS A DAEMON RESTART.  A relaunched
daemon has no memory of `current' — `endpoint_watch_workspace_roster.proto'
carries `current' as the daemon's stamp, and `endpoint_select_workspace.proto'
is the only verb that sets it — so the first thing it stamps is whatever
Emacs re-registered first, which is an arbitrary order and not a choice
the user made.  This is set for the length of a re-registration and
cleared when the re-select is acknowledged; while it stands,
`agent-repl-roster-react-to-current' does not move the frame, so a roster
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
onto its successor, and every SelectWorkspace and MarkWorkspaceViewed for
that workspace kept going to the dead daemon\='s address
\(`elisp.connect.call-on-closed-connection') while its webview stayed on
the dead origin.  A dead recorded connection starts WS\='s reattach onto
the live daemon (`agent-repl-host--follow-live-daemon'), and the answer is
the live daemon -- or nil when no link stands, which every caller already
treats as having no connection."
  (let ((conn (agent-repl-host--recorded-conn ws)))
    (if (or (null conn) (agent-repl-connect-connection-alive-p conn))
        conn
      (agent-repl-host--follow-live-daemon ws "dead-connection")
      (let ((now (agent-repl-host--recorded-conn ws)))
        (if (and now (agent-repl-connect-connection-alive-p now))
            now
          (agent-repl-link-live))))))

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
      (agent-repl--log ws "elisp.host.gate ws=%s gate=unknown reason=no-push" ws)
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
               (agent-repl--error ws "elisp.host.gate-unknown-composer-arm ws=%s arm=%S" ws arm)
               :unknown)))
          (arm
           (agent-repl--error ws "elisp.host.gate-unknown-standing ws=%s arm=%S" ws arm)
           :unknown))))
     (t
      (agent-repl--error ws "elisp.host.gate-unset-session ws=%s host=%S" ws host)
      :unknown))))

(defun agent-repl-host-take-restart-hold (ws)
  "Close WS's composer LOCALLY from the instant a forced restart is accepted.

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

The hold is taken for FORCED restarts only.  A graceful restart is
SCHEDULED -- the daemon defers the bounce until the turn settles -- and
the composer stays open until the daemon itself says otherwise."
  (let ((generation (agent-repl-host-generation ws)))
    (agent-repl-host--put ws :restart-hold (list :generation generation))
    (agent-repl--info ws "elisp.host.restart-hold-taken ws=%s generation=%s"
                      ws (or generation "-"))))

(defun agent-repl-host-release-restart-hold (ws reason)
  "Drop WS's local restart hold, recording REASON for why it ended."
  (when (plist-get (agent-repl-host--entry ws) :restart-hold)
    (agent-repl-host--put ws :restart-hold nil)
    (agent-repl--info ws "elisp.host.restart-hold-released ws=%s reason=%s"
                      ws reason)))

(defun agent-repl-host--settle-restart-hold (ws)
  "Release WS's restart hold once the DAEMON's own push resolves it.

Two edges end the hold, and a forced restart is guaranteed to produce one
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
had time to publish yet: an accepted forced restart holds the gate at
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

(defun agent-repl-host-register (conn dir on-done &optional workspace)
  "Register DIR with the daemon on CONN; call ON-DONE with the minted ref.
IDEMPOTENT BY DIR — re-registering after a reconnect or a daemon restart
is the normal path, never an error.  A daemon-authored error arm and a
transport failure are different facts and are logged as such; both answer
ON-DONE with nil so the caller never waits on a callback that will not come.
WORKSPACE names an existing workspace being re-registered.  Its absence means
DIR is being registered before any workspace identity exists."
  (let ((log-scope (if workspace
                       workspace
                     '(:agent-repl-central
                       "initial registration precedes workspace ownership"))))
    (agent-repl--info log-scope "elisp.host.register dir=%S" dir)
    (agent-repl-rpc-register-workspace
     conn (list :dir dir)
     :on-response
     (lambda (response)
       (pcase (plist-get response :arm)
         (:success
          (let ((ref (plist-get (plist-get response :value) :workspace)))
            (agent-repl--info log-scope "elisp.host.registered dir=%S id=%S" dir
                              (plist-get ref :id))
            (funcall on-done ref)))
         (:error
          (agent-repl--error log-scope "elisp.host.register-refused dir=%S error=%S"
                             dir (plist-get response :value))
          (funcall on-done nil))
         (arm
          (agent-repl--error log-scope "elisp.host.register-unknown-arm dir=%S arm=%S" dir arm)
          (funcall on-done nil))))
     :on-failure
     (lambda (detail)
       (agent-repl--error log-scope "elisp.host.register-failed dir=%S detail=%S" dir detail)
       (funcall on-done nil)))))

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
      (agent-repl--info ws (format "elisp.host.%s-handover-refusal ws=%%s arm=%%S fields=%%S" op)
                        ws keyword (plist-get arm :value))
      (if (eq keyword :not-yet-adopted)
          (run-at-time agent-repl-host-handover-retry-delay nil
                       #'agent-repl-host-handle-refusal ws arm)
        (agent-repl-host-handle-refusal ws arm)))
     (t
      (agent-repl--error ws (format "elisp.host.%s-refused ws=%%s error=%%S" op)
                         ws value)))))

;;;; ---- Select ----

(defun agent-repl-host-select (ws &optional on-settled)
  "Tell the daemon the user switched to workspace WS.
ON-SETTLED, when given, is called with `:success', `:error' or
`:failure' once the call has an outcome — the one moment a caller
holding state on the selection\='s behalf (the link-up re-assertion) may
let go of it, whichever way it went.
Idempotent by contract — re-selecting the current workspace succeeds —
and the daemon's own act of stamping `current' also CLEARS the
workspace's attention marker, which is why no ack verb exists.  Answers
nil without calling anything when WS has no ref yet: an unregistered
workspace has no identity to select."
  (let ((ref (agent-repl-host-ref ws))
        (conn (or (agent-repl-host-conn ws) (agent-repl-link-primary))))
    (cond
     ((null ref)
      (agent-repl--log ws "elisp.host.select-skipped ws=%s reason=no-ref" ws)
      nil)
     ((null conn)
      (agent-repl--warn ws "elisp.host.select-skipped ws=%s reason=no-connection" ws)
      nil)
     (t
      (agent-repl--info ws "elisp.host.select ws=%s id=%S" ws (plist-get ref :id))
      (agent-repl-rpc-select-workspace
       conn (list :workspace ref)
       :on-response
       (lambda (response)
         (pcase (plist-get response :arm)
           (:success
            ;; RECORDED ONLY ON THE ACK.  `agent-repl-host-last-selected-id' is
            ;; what Emacs believes the daemon stamped as `current'; a refusal is
            ;; the daemon saying it stamped nothing, and recording the id anyway
            ;; would leave Emacs disagreeing with the daemon about which
            ;; workspace is current.
            (setq agent-repl-host-last-selected-id (plist-get ref :id))
            (agent-repl--log ws "elisp.host.selected ws=%s id=%S"
                             ws (plist-get ref :id))
            (when on-settled (funcall on-settled :success)))
           (:error
            (agent-repl-host--on-refused ws "select" (plist-get response :value))
            (when on-settled (funcall on-settled :error)))
           (arm
            (agent-repl--error ws "elisp.host.select-unknown-arm ws=%s arm=%S" ws arm)
            (when on-settled (funcall on-settled :error)))))
       :on-failure
       (lambda (detail)
         (agent-repl--error ws "elisp.host.select-failed ws=%s detail=%S" ws detail)
         (when on-settled (funcall on-settled :failure))))
      t))))

(defun agent-repl-host-mark-viewed (ws)
  "Tell the daemon the user has now SEEN workspace WS.
The editor half of the FULL/PARTIAL display mode: the tab bar has just
demoted WS\='s tab to PARTIAL after the view dwell, and this is what puts
the same mode on the workspace\='s sidebar row, so the two surfaces never
disagree about what the user has seen.

FIRE AND FORGET, and deliberately: the mode is presentation, the daemon
clears it itself on the row\='s next status change, and a workspace whose
report never lands simply keeps a FULL sidebar row until the next one
does.  So a refusal is RECORDED and nothing is retried or rolled back —
there is no local state to roll back to.

Answers nil without calling anything when WS has no ref or no connection
yet: an unregistered workspace has no row to mark."
  (let ((ref (agent-repl-host-ref ws))
        (conn (or (agent-repl-host-conn ws) (agent-repl-link-primary))))
    (cond
     ((null ref)
      (agent-repl--log ws "elisp.host.mark-viewed-skipped ws=%s reason=no-ref" ws)
      nil)
     ((null conn)
      (agent-repl--log ws "elisp.host.mark-viewed-skipped ws=%s reason=no-connection" ws)
      nil)
     (t
      (agent-repl--log ws "elisp.host.mark-viewed ws=%s id=%S" ws (plist-get ref :id))
      (agent-repl-rpc-mark-workspace-viewed
       conn (list :workspace ref)
       :on-response
       (lambda (response)
         (pcase (plist-get response :arm)
           (:success
            (agent-repl--log ws "elisp.host.marked-viewed ws=%s" ws))
           (:error
            (agent-repl-host--on-refused ws "mark-viewed" (plist-get response :value)))
           (arm
            (agent-repl--error ws "elisp.host.mark-viewed-unknown-arm ws=%s arm=%S"
                               ws arm))))
       :on-failure
       (lambda (detail)
         (agent-repl--error ws "elisp.host.mark-viewed-failed ws=%s detail=%S"
                            ws detail)))
      t))))

(defun agent-repl-host--on-workspace-activated (&rest _)
  "Select the newly activated perspective's workspace with the daemon.
Registered on workspace.el's perspective-activation boundary: an ordinary
tab switch IS the SelectWorkspace, and it is also what clears the
workspace's attention marker.

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
    (if agent-repl--eager-open-in-progress
        (agent-repl--log ws "elisp.host.select-skipped reason=background-activation")
      (when (and ws (agent-repl-host--entry ws))
        (agent-repl-host-select ws)))))

;;;; ---- Subscribe ----

(defun agent-repl-host--attach (ws conn ref)
  "Record REF and CONN as workspace WS's identity and owner.
The ref is stored BOTH in this file's table and on workspace.el's plist
under `:ref', so any module can read the identity from whichever it
already holds; neither copy is ever derived from a path.

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
  (agent-repl-host--put ws :ref ref)
  (agent-repl-host--put ws :conn conn)
  (agent-repl--ws-put ws :ref ref)
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
                      (agent-repl--info now "elisp.host.subscribed ws=%s method=%S id=%S"
                                        now "WatchHostWorkspace" id)))))
    (agent-repl-host--put ws :stream stream)
    (agent-repl--info ws "elisp.host.subscribe-opened ws=%s id=%S" ws (plist-get ref :id))
    stream))

(defun agent-repl-host-unsubscribe (ws)
  "Cancel WS's host subscription.  Cancelling IS the graceful close.
Idempotent: a workspace with no standing stream is already unsubscribed."
  (let ((stream (agent-repl-host-stream ws)))
    (if (null stream)
        (agent-repl--log ws "elisp.host.unsubscribe-noop ws=%s" ws)
      (agent-repl--info ws "elisp.host.unsubscribe ws=%s" ws)
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
      (agent-repl--log new "elisp.host.rename-noop old=%s new=%s reason=no-entry"
                       old new)
      nil)
     ((gethash new agent-repl-host--by-name)
      (agent-repl--error new
                         "elisp.host.rename-target-occupied old=%s new=%s ref=%S"
                         old new (plist-get entry :ref))
      nil)
     (t
      ;; Primitive hash mutations with no Lisp call between them, so no
      ;; observer can run against a half-moved entry.
      (puthash new entry agent-repl-host--by-name)
      (remhash old agent-repl-host--by-name)
      (agent-repl--info new
                        "elisp.host.renamed old=%s new=%s id=%s stream=%s conn=%s host-push=%s"
                        old new (plist-get (plist-get entry :ref) :id)
                        (and (plist-get entry :stream) t)
                        (and (plist-get entry :conn) t)
                        (and (plist-get entry :host) t))
      t))))

(defun agent-repl-host-forget (ws)
  "Drop every trace of WS from this file, cancelling its stream first.
Called when a workspace's tab is torn down; the daemon-side session is
untouched, because closing a tab is a VIEW act."
  (agent-repl-host-unsubscribe ws)
  (remhash ws agent-repl-host--by-name)
  (agent-repl--info ws "elisp.host.forgotten ws=%s" ws))

(defun agent-repl-host--note-ending (ws stream)
  "Record that STREAM, WS\='s host stream, carried the planned ending.
The daemon is standing down in a PLANNED exit and this is the stream\='s
last frame (`DaemonStreamEnding'), so the clean end that follows is
expected, not lost: `agent-repl-host--handle-close' reads the mark.
STREAM nil means the stream standing for WS now."
  (agent-repl-host--put ws :ending-stream (or stream (agent-repl-host-stream ws)))
  (agent-repl--info ws "elisp.host.stream-ending ws=%s" ws))

(defun agent-repl-host--planned-end-p (ws outcome stream)
  "Return non-nil when STREAM closing with OUTCOME is WS\='s planned end.
Only a CLEAN end (`(:ended)') of the very stream that carried the
planned-ending frame is planned; an error after the ending, or the end of
a stream that never carried it, is a loss."
  (let ((marked (plist-get (agent-repl-host--entry ws) :ending-stream)))
    (and (eq (car outcome) :ended)
         marked
         (eq marked (or stream (agent-repl-host-stream ws))))))

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
    (agent-repl--log ws "elisp.host.stale-stream-close ws=%s outcome=%S" ws outcome))
   ((eq (car outcome) :cancelled)
    (agent-repl--log ws "elisp.host.stream-cancelled ws=%s" ws))
   ((agent-repl-host--planned-end-p ws outcome stream)
    ;; THE DAEMON SAID SO FIRST.  Its last frame was the planned ending, so
    ;; this clean end is a stand-down, not a fault: INFO, and the same
    ;; single walk onto the live daemon a loss takes.
    (agent-repl--info ws "elisp.host.stream-ended-planned ws=%s" ws)
    (agent-repl-host-release-restart-hold ws "planned-ending")
    (agent-repl-host--put ws :ending-stream nil)
    (agent-repl-host--put ws :stream nil)
    (agent-repl-host--follow-live-daemon ws "planned-ending"))
   (t
    (agent-repl-host--put ws :ending-stream nil)
    (agent-repl--error ws "elisp.host.stream-lost ws=%s outcome=%S" ws outcome)
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
it, and the stream standing for WS now is meant."
  (let ((arm (plist-get push :arm))
        (value (plist-get push :value)))
    (pcase arm
      (:host (agent-repl-host--apply-state ws value))
      (:notification-clicked (agent-repl-host--notification-clicked ws))
      (:selection (agent-repl-host--apply-selection ws value))
      (:transferred (agent-repl-host--transferred ws value))
      (:reload-webapp (agent-repl-host--reload-webapp ws))
      (:open-in-editor (agent-repl-host--open-in-editor ws value))
      (:ending (agent-repl-host--note-ending ws stream))
      (_ (agent-repl--error ws "elisp.host.unknown-push ws=%s arm=%S push=%S"
                            ws arm push)))))

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
          (agent-repl--info ws "elisp.host.buffer-renamed ws=%s buffer=%S title=%S"
                            ws (buffer-name buffer)
                            (agent-repl-host-display-title ws)))))))

(defun agent-repl-host--apply-state (ws host)
  "Whole-replace WS's host state with HOST and run the update hooks.
`shim_attached' false gets NO treatment: a parked workspace presents as
live and unwired, and the frontend cannot tell parked from idle, on
purpose."
  (agent-repl-host--put ws :host host)
  (agent-repl-host--settle-restart-hold ws)
  (agent-repl-host--apply-naming ws)
  (agent-repl--log ws "elisp.host.state ws=%s gate=%S backfill=%S faults=%d"
                   ws (agent-repl-host-composer-gate ws)
                   (agent-repl-host-backfill ws)
                   (length (agent-repl-host-faults ws)))
  (run-hook-with-args 'agent-repl-host-update-functions ws host))

;;;; ---- The feed selection ----

(defun agent-repl-host--apply-selection (ws kind)
  "Record KIND as the kind of row WS's feed has selected.
KIND is `:none', `:response' or `:prompt', from the host watch's
`selection' push: state, not an event, so a late subscriber is sent the
selection in force first.  The daemon holds the selection itself; Emacs
keeps only the kind, for the composer's escape-twice clear."
  (agent-repl-host--put ws :selection kind)
  (agent-repl--info ws "elisp.host.selection ws=%s kind=%S" ws kind))

(defun agent-repl-host-selection (ws)
  "Return the kind of row WS's feed has selected, or nil.
One of `:none', `:response' and `:prompt', as the host watch last pushed
it.  Nil means no selection push has arrived yet, which is nothing
selected."
  (plist-get (agent-repl-host--entry ws) :selection))

;;;; ---- The notification click ----

(defun agent-repl-host--notification-clicked (ws)
  "Select WS's tab: the user clicked WS's desktop banner.
The daemon posted the banner and read the click back; raising the frame
and selecting the tab is `agent-repl--notification-activate'."
  (agent-repl--info ws "elisp.host.notification-clicked ws=%s" ws)
  (agent-repl--notification-activate ws))

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
  (let ((token (setq agent-repl-host--reattach-tokens
                     (1+ agent-repl-host--reattach-tokens))))
    (agent-repl-host--put ws :reattach (list :token token :conn new :trigger trigger))
    (agent-repl--info ws "elisp.host.reattach ws=%s address=%S claim=%s trigger=%s"
                      ws (agent-repl-connect-connection-address new) claim trigger)
    (pcase claim
      (:adopt (agent-repl-host--adopt-onto ws new token trigger))
      (:register (agent-repl-host--register-onto ws new token trigger on-settled))
      (_ (agent-repl--fatal ws "elisp.host.reattach-unknown-claim ws=%s claim=%S trigger=%s"
                            ws claim trigger)))))

(defun agent-repl-host--reattach-current-p (ws token)
  "Return non-nil when TOKEN is WS\='s newest reattach walk."
  (equal token (plist-get (plist-get (agent-repl-host--entry ws) :reattach) :token)))

(defun agent-repl-host--reattached (ws new ref trigger)
  "Finish WS\='s reattach onto NEW with REF: move the stream, then say so.
The stream on the daemon WS is leaving is cancelled BEFORE the new one is
opened, so no workspace ever holds two, and the subscribe is what moves
`:conn' and `:ref' onto NEW.  TRIGGER is recorded."
  (agent-repl-host-unsubscribe ws)
  (agent-repl-host-subscribe new ws ref)
  (agent-repl-host--put ws :reattach nil)
  (agent-repl-host--put ws :detached nil)
  (agent-repl--info ws "elisp.host.reattached ws=%s address=%S trigger=%s"
                    ws (agent-repl-connect-connection-address new) trigger)
  (run-hook-with-args 'agent-repl-host-reattached-functions ws))

(defun agent-repl-host--register-onto (ws new token trigger on-settled)
  "The `:register' claim of reattach TOKEN: register WS\='s dir on NEW.
Registration is idempotent by dir, so the daemon that now serves Emacs
hands back the ref it knows WS by, whether it adopted WS\='s shim from a
daemon that died, took it over at a promotion, or is a fresh daemon after
an outage.  On success the walk finishes (`agent-repl-host--reattached')
and the webview is reloaded AFTER the subscribe, because frontend.el
derives the page URL from `agent-repl-host-conn' and the subscribe is what
moved it.  A refusal or a transport failure leaves WS detached: the next
link edge (`agent-repl-host-on-link-up', `agent-repl-host-on-link-promote')
or the next call that finds its connection dead walks it again.  TRIGGER
is recorded; ON-SETTLED is called once the walk has an outcome."
  (let ((dir (agent-repl--ws-get ws :project-dir))
        (settle (lambda () (when on-settled (funcall on-settled)))))
    (if (null dir)
        (progn
          (agent-repl-host--put ws :reattach nil)
          (agent-repl--error ws "elisp.host.reattach-without-dir ws=%s trigger=%s" ws trigger)
          (funcall settle))
      (agent-repl-host-register
       new dir
       (lambda (ref)
         (cond
          ((not (agent-repl-host--reattach-current-p ws token))
           (agent-repl--log ws "elisp.host.reattach-superseded ws=%s trigger=%s" ws trigger))
          ;; THE SLUG NAMES THE TRIGGER (`elisp.host.link-up-register-failed',
          ;; `elisp.host.stream-lost-webview-repointed', ...), a closed
          ;; vocabulary, so a reader can ask for one edge's walks alone.
          ((null ref)
           (agent-repl-host--put ws :reattach nil)
           (agent-repl--error ws (format "elisp.host.%s-register-failed ws=%%s dir=%%S address=%%S" trigger)
                              ws dir (agent-repl-connect-connection-address new)))
          (t
           (agent-repl-host--reattached ws new ref trigger)
           (agent-repl-frontend-reload-webview ws)
           (agent-repl--info ws (format "elisp.host.%s-webview-repointed ws=%%s address=%%S" trigger)
                             ws (agent-repl-connect-connection-address new))))
         (funcall settle))
       ws))))

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
  - a walk onto the live daemon is already in flight: nothing more;
  - the live daemon IS the one WS lost, and it is handing over: its
    successor\='s promotion re-attaches WS (`agent-repl-host-on-link-promote')
    -- registering on a daemon that is leaving would only fail;
  - otherwise WS is walked onto the live daemon now."
  (let ((live (agent-repl-link-live))
        (walk (plist-get (agent-repl-host--entry ws) :reattach)))
    (agent-repl-host--put ws :detached t)
    (cond
     ((null live)
      (agent-repl--info ws "elisp.host.reattach-awaiting-link ws=%s trigger=%s" ws trigger))
     ((and walk (eq (plist-get walk :conn) live))
      (agent-repl--log ws "elisp.host.reattach-in-flight ws=%s trigger=%s address=%S"
                       ws trigger (agent-repl-connect-connection-address live)))
     ((and (eq live (agent-repl-host--recorded-conn ws))
           (or (agent-repl-link-successor) (agent-repl-link-successor-pending-p)))
      (agent-repl--info ws "elisp.host.reattach-awaiting-promotion ws=%s trigger=%s address=%S"
                        ws trigger (agent-repl-connect-connection-address live)))
     (t
      (agent-repl-host--reattach ws live :register trigger)))))

;;;; ---- The handover, per workspace ----

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
ADOPTION WINDOW.  The adopt is an ordinary unary call under
`agent-repl-connect-unary-timeout-seconds' (10s, connect.el), while the
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

So a failure re-walks the adopt after `agent-repl-host-handover-retry-delay',
exactly as a `not_yet_adopted' refusal already does, and STOPS when the
successor NEW is no longer the standing one: a promotion or a lost
successor stream clears it, and there is then nothing left to adopt onto.
A refusal is not retried here — the daemon answered, and its arms have
their own walk."
  (let ((ref (agent-repl-host-ref ws))
        (address (agent-repl-connect-connection-address new))
        (old (agent-repl-host--recorded-conn ws)))
    (cond
     ((not (agent-repl-host--reattach-current-p ws token))
      ;; A scheduled retry of a walk a newer one has since superseded.
      (agent-repl--log ws "elisp.host.reattach-superseded ws=%s trigger=%s" ws trigger))
     ((null ref)
      (agent-repl-host--put ws :reattach nil)
      (agent-repl--error ws "elisp.host.adopt-without-ref ws=%s" ws))
     (t
      (let ((restore
             (lambda ()
               (agent-repl-host--put ws :conn old)
               (agent-repl-frontend-reload-webview ws)
               (agent-repl--info ws "elisp.host.webview-restored ws=%s address=%S"
                                 ws (and old (agent-repl-connect-connection-address old))))))
        (agent-repl-host--put ws :conn new)
        (agent-repl-frontend-reload-webview ws)
        (agent-repl--info ws "elisp.host.webview-redialed ws=%s address=%S"
                          ws address)
        (agent-repl-rpc-adopt-host-workspace
         new (list :workspace ref)
         :on-response
         (lambda (response)
           (cond
            ((not (agent-repl-host--reattach-current-p ws token))
             (agent-repl--log ws "elisp.host.reattach-superseded ws=%s trigger=%s" ws trigger))
            (t
             (pcase (plist-get response :arm)
               (:success
                (agent-repl--info ws "elisp.host.adopted ws=%s address=%S" ws address)
                (agent-repl-host--reattached ws new ref trigger))
               (:error
                (funcall restore)
                (agent-repl-host--put ws :reattach nil)
                (agent-repl-host--on-refused ws "adopt" (plist-get response :value)))
               (arm
                (funcall restore)
                (agent-repl-host--put ws :reattach nil)
                (agent-repl--error ws "elisp.host.adopt-unknown-arm ws=%s arm=%S" ws arm))))))
         :on-failure
         (lambda (detail)
           (cond
            ((not (agent-repl-host--reattach-current-p ws token))
             (agent-repl--log ws "elisp.host.reattach-superseded ws=%s trigger=%s" ws trigger))
            (t
             (funcall restore)
             (if (eq new (agent-repl-link-successor))
                 (progn
                   (agent-repl--error ws "elisp.host.adopt-failed ws=%s detail=%S retrying=t"
                                      ws detail)
                   (run-at-time agent-repl-host-handover-retry-delay nil
                                #'agent-repl-host--adopt-onto ws new token trigger))
               (agent-repl-host--put ws :reattach nil)
               (agent-repl--error ws "elisp.host.adopt-failed ws=%s detail=%S retrying=nil"
                                  ws detail)))))))))))

(defun agent-repl-host--transferred (ws &optional _value)
  "Adopt WS onto the successor daemon after the old one released it.

`HostWorkspaceTransferred' IS EMPTY ON THE WIRE, so the push names no
daemon and _VALUE carries nothing to read.  THE SUCCESSOR IS THE ONE THE
LINK RECORDED: `shutdown_announced{address}' is the single place an
address for the joining daemon ever comes from, and daemon-link dialed
and accepted it there — a per-workspace release cannot introduce a
daemon the link has never heard of.

`transferred' is a PUSH, never a terminal frame, so the old stream stays
standing until the adopt lands and the shared walk cancels it.

A SUCCESSOR THAT IS ONLY PENDING IS NOT A BREACH, AND WAS THE DEFECT.
The outgoing daemon announces the stand-down and then transfers every
FREE workspace immediately — measured at ~1 ms later, which is before
this Emacs has even read the announcement off its own `WatchDaemon'
stream, let alone dialed the successor and been ACCEPTED by it.  So the
`transferred' push routinely arrives while `agent-repl-link-successor'
still answers nil because the dial is in flight, and treating that nil as
the missing-successor breach dropped the notice on the floor: no adopt
was ever sent, the outgoing daemon sat out its whole adoption window and
recorded the workspace\='s own fault, and the successor was promoted only
by the old stream dying underneath it.  The window is WAITED IN instead,
on daemon-link\='s own acceptance seam — the same one the
`not_yet_adopted' refusal already uses — so the adopt goes out the
instant the successor proves it is listening.

WITH NEITHER A STANDING NOR A PENDING SUCCESSOR, THE ANNOUNCEMENT HAS
NOT BEEN READ YET, AND THAT IS THE SECOND HALF OF THE SAME DEFECT.  The
pending branch above closes the window between the dial going out and the
successor accepting it; it does NOT close the window before the dial goes
out at all, because `agent-repl-link-successor-pending-p' only becomes
true inside the announcement handler.  The two pushes are in flight
together and arrive on different streams, so which one this Emacs
processes first is a coin toss on the transport.

Measured, in the e2e sandbox: the outgoing daemon announced the stand-down
at 16:43:37.310 and pushed the transfer notices at .314; Emacs decoded the
two `WatchHostWorkspaceResponse' pushes at .321 and the
`DaemonShutdownAnnounced' at .322.  On the earlier order this branch
called the ordering a breach and dropped the notice, no adopt was ever
sent, the outgoing daemon sat out its whole adoption window, and the
successor was promoted only by the old stream dying underneath it -- which
is `TestEmacsHandoverTransfersAtFreeness' waiting out 21s for a promotion
that healthy runs make in about two.

The daemon sends the announcement BEFORE the transfer, always
(`rollout.handover': announce, snapshot the participants, write the
manifest, then transfer each free workspace).  So a transfer with no
successor yet is a notice that overtook its own announcement, not a
missing one, and it WAITS on the same acceptance seam the pending branch
uses.  A successor that genuinely never arrives simply never wakes it, and
the workspace keeps the old stream throughout -- exactly as before.

It is an INFO record: the order is seen in the log, and nothing is wrong,
since the announcement is on its way.  It was once a WARNING, which the
standing rule forbids for ordinary operation (a deploy on 2026-09-29
recorded seven of them, one per busy-free workspace handed over)."
  (let ((new (agent-repl-link-successor)))
    (cond
     (new
      (agent-repl--info ws "elisp.host.transferred ws=%s address=%S adopting=t"
                        ws (agent-repl-connect-connection-address new))
      (agent-repl-host--reattach ws new :adopt "transferred"))
     ((agent-repl-link-successor-pending-p)
      (agent-repl--info ws "elisp.host.transferred-awaiting-successor ws=%s" ws)
      (agent-repl-host--adopt-on-acceptance ws))
     (t
      (agent-repl--info ws "elisp.host.transferred-before-the-announcement ws=%s" ws)
      (agent-repl-host--adopt-on-acceptance ws)))))

;;;; ---- Handover refusals answered by a per-workspace rpc ----

(defun agent-repl-host--redial-successor (ws address)
  "Return the successor connection at ADDRESS for WS, dialing it if needed.
A successor already standing at ADDRESS is returned as is.  Otherwise the
dial is made through `agent-repl-link-dial-successor', which is idempotent
for the matching address; when a successor is standing at a DIFFERENT
address DAEMON-LINK'S GUARD WINS (audit-3 #30 ruling) — it logs
`elisp.link.successor-address-changed' at ERROR and dials nothing, so no
adopt is sent to an address the link never attached.
Answers nil while the dial has not been ACCEPTED yet — the caller must
then wait for acceptance rather than adopt onto an unproven daemon."
  (let ((standing (agent-repl-link-successor)))
    (if (and standing
             (equal address (agent-repl-connect-connection-address standing)))
        standing
      (agent-repl--info ws "elisp.host.redial ws=%s address=%S standing=%S"
                        ws address
                        (and standing
                             (agent-repl-connect-connection-address standing)))
      (agent-repl-link-dial-successor address))))

(defun agent-repl-host--adopt-on-acceptance (ws)
  "Adopt WS onto the successor as soon as its `WatchDaemon' is ACCEPTED.
NO BUSY LOOP AND NO POLL: the handover hooks are daemon-link's acceptance
seam, so this hangs ONE self-removing function there and is woken by the
acceptance itself.  A successor that never arrives simply never wakes it."
  (let (retry)
    (setq retry
          (lambda (_old new)
            (remove-hook 'agent-repl-link-handover-functions retry)
            (agent-repl--info ws "elisp.host.adopt-retry ws=%s" ws)
            (agent-repl-host--reattach ws new :adopt "successor-accepted")))
    (add-hook 'agent-repl-link-handover-functions retry)))

(defun agent-repl-host-handle-refusal (ws arm-plist)
  "React to a per-workspace rpc's handover refusal ARM-PLIST for WS.
The daemon answers the VERB with the handover, so the refusal is news, not
an error: `(:arm :transferring-away :value (:address A))' means this
daemon has released WS to the daemon at A and is the same fact the
`transferred' push carries — Emacs dials A when it is not already the
standing successor (`elisp.host.redial'), then walks the one adopt path.
`(:arm :not-yet-adopted :value nil)' means the NEW daemon has not taken WS
over yet: nothing is wrong, so it is INFO, and the adopt is retried once
the successor's `WatchDaemon' is accepted.  Any other arm is not a
handover refusal and is a contract breach.

The RETRY's own answer comes back through the adopt path's arms, never
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
             (agent-repl--error ws "elisp.host.transferring-away-without-address ws=%s address=%S"
                                ws address)
           (agent-repl--info ws "elisp.host.transferring-away ws=%s address=%S" ws address)
           (let ((new (agent-repl-host--redial-successor ws address)))
             (if new
                 (agent-repl-host--reattach ws new :adopt "transferring-away")
               ;; The dial stands but is not accepted yet; adopting onto an
               ;; unproven daemon is exactly what the acceptance gate forbids.
               (agent-repl--info ws "elisp.host.awaiting-successor ws=%s address=%S"
                                 ws address)
               (agent-repl-host--adopt-on-acceptance ws))))))
      (:not-yet-adopted
       (agent-repl--info ws "elisp.host.not-yet-adopted ws=%s" ws)
       (let ((new (agent-repl-link-successor)))
         (if new
             (agent-repl-host--reattach ws new :adopt "not-yet-adopted")
           (agent-repl-host--adopt-on-acceptance ws))))
      (_
       (agent-repl--error ws "elisp.host.unknown-refusal-arm ws=%s arm=%S" ws arm)))))

;;;; ---- The two relayed acts ----

(defun agent-repl-host--reload-webapp (ws)
  "Reload WS's webview against the SAME daemon.
Empty by design: no address rides the arm, because the daemon is not
changing.  A combined daemon+webapp rollout never sends it — the
handover's fresh attach pulls the new assets by itself."
  (agent-repl--info ws "elisp.host.reload-webapp ws=%s" ws)
  (agent-repl-frontend-reload-webview ws))

(defun agent-repl-host--open-in-editor (ws target)
  "Open TARGET's path through the ONE shared editor-popup subroutine.
A fact, not a command: nothing acks it.  The line is optional and its
absence means the file's top — or a directory, which the popup opens in
dired."
  (let ((path (plist-get target :path))
        (line (plist-get target :line)))
    (agent-repl--info ws "elisp.host.open-in-editor ws=%s path=%S line=%S" ws path line)
    (agent-repl-popup-open path line)))

;;;; ---- Link lifecycle ----

(defun agent-repl-host--selected-dir ()
  "Return the project dir the user\='s selection stands on, or nil.
THE DIR, NOT THE ID, IS WHAT SURVIVES A DAEMON RESTART: a relaunched
daemon re-mints every `WorkspaceRef\=' id, so the id Emacs held before the
link went down names nothing afterwards, while the directory the user
stood in is the same directory the re-registration hands back.  The id is
still where the answer is looked up FIRST — it is the selection Emacs
last had acknowledged — and the tab the frame is on is the fallback."
  (let ((ws (or (and agent-repl-host-last-selected-id
                     (agent-repl--ws-by-ref-id agent-repl-host-last-selected-id))
                (agent-repl--ws-current-name))))
    (and ws (agent-repl--ws-get ws :project-dir))))

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
`agent-repl-roster-react-to-current\=', and every arm below clears it: a
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
                      "elisp.host.link-up-reselect-lost dir=%S kept=%S"
                      dir (agent-repl--ws-current-name)))
   (t
    (let ((ws (agent-repl-host--ws-for-dir dir)))
      (agent-repl--info ws "elisp.host.link-up-reselect ws=%s dir=%S" ws dir)
      (unless (equal ws (agent-repl--ws-current-name))
        (agent-repl--ws-switch ws))
      (agent-repl-host-select
       ws (lambda (outcome)
            (setq agent-repl-host-reselect-pending nil)
            (agent-repl--log ws "elisp.host.link-up-reselected ws=%s outcome=%S"
                             ws outcome)))))))

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
         (eligible (seq-filter (lambda (ws) (agent-repl--ws-get ws :project-dir)) names))
         (outstanding (length eligible))
         (settle (lambda ()
                   (setq outstanding (1- outstanding))
                   ;; EXACTLY zero, once: a register callback that answers
                   ;; synchronously walks this counter down past every
                   ;; workspace, and a re-assertion fired twice would select
                   ;; twice for one relink.
                   (when (zerop outstanding)
                     (agent-repl-host--reassert-selection wanted)))))
    (setq agent-repl-host-reselect-pending wanted)
    (agent-repl--info '(:agent-repl-central "link recovery spans every workspace")
                      "elisp.host.link-up workspaces=%d selection=%S"
                      (length names) wanted)
    (dolist (ws names)
      (let ((dir (agent-repl--ws-get ws :project-dir)))
        (if (null dir)
            ;; A name with no registered project directory owns no durable
            ;; workspace sink.  This refusal belongs to process-wide link
            ;; repair; preserve the candidate name in the record body.
            (agent-repl--warn '(:agent-repl-central
                                "a link-repair candidate without a directory owns no sink")
                              "elisp.host.link-up-skipped ws=%s reason=no-dir" ws)
          ;; THE ONE REATTACH PATH, the same walk a lost stream takes.
          (agent-repl-host--reattach ws conn :register "link-up" settle))))
    ;; Nothing to wait for: no workspace had a dir to re-register, so the
    ;; selection is settled here rather than in a callback that never runs.
    (when (null eligible)
      (agent-repl-host--reassert-selection wanted))))

(defun agent-repl-host-on-link-down (conn)
  "Forget the streams CONN carried, keeping every workspace's last state.
The pushed state is the newest fact Emacs has; discarding it on a link
death would blank the editor for the length of an outage the reconnect
covers by itself."
  (let ((affected 0))
    (maphash
     (lambda (ws entry)
       (when (eq (plist-get entry :conn) conn)
         (setq affected (1+ affected))
         (agent-repl-host--put ws :stream nil)
         (agent-repl-host--put ws :conn nil)))
     agent-repl-host--by-name)
    (agent-repl--warn '(:agent-repl-central "link loss spans every workspace")
                      "elisp.host.link-down workspaces=%d" affected)))

(defun agent-repl-host-on-link-promote (old new)
  "Re-attach onto NEW every workspace the promotion left behind on OLD.
A promotion rebuilds nothing, because every workspace the old daemon
TRANSFERRED was already adopted onto the successor.  But a daemon can go
without transferring a workspace -- measured 2026-09-27: a restart that
ran in the transfer\='s place, and a daemon that exited with the workspace
still its own -- and such a workspace is still on OLD, whose connection
this promotion is about to close.  Each one, and each one already
detached, is walked onto NEW through the one reattach path.  Runs from
`agent-repl-link-promote-functions', BEFORE OLD is closed."
  (let (left)
    (maphash (lambda (ws entry)
               (when (or (eq (plist-get entry :conn) old)
                         (plist-get entry :detached))
                 (push ws left)))
             agent-repl-host--by-name)
    (agent-repl--info '(:agent-repl-central "link promotion spans every workspace")
                      "elisp.host.link-promote left-behind=%d address=%S"
                      (length left) (agent-repl-connect-connection-address new))
    (dolist (ws (nreverse left))
      (agent-repl-host--reattach ws new :register "promotion"))))

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
