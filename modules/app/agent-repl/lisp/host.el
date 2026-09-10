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

(declare-function agent-repl-connect-stream-cancel "connect" (stream))
(declare-function agent-repl-connect-connection-alive-p "connect" (conn))

(declare-function agent-repl-rpc-register-workspace "rpc" (conn request &rest keys))
(declare-function agent-repl-rpc-select-workspace "rpc" (conn request &rest keys))
(declare-function agent-repl-rpc-adopt-host-workspace "rpc" (conn request &rest keys))
(declare-function agent-repl-link-dial-successor "daemon-link" (address))
(declare-function agent-repl-connect-connection-address "connect" (conn))
(declare-function agent-repl-rpc-watch-host-workspace "rpc"
                  (conn ref on-push on-close &optional on-open))

(declare-function agent-repl-link-primary "daemon-link" ())
(declare-function agent-repl-link-successor "daemon-link" ())

(declare-function agent-repl--ws-get "workspace" (ws key))
(declare-function agent-repl--ws-put "workspace" (ws key val))
(declare-function agent-repl--ws-switch "workspace" (ws &rest args))
(declare-function agent-repl--ws-current-name "workspace" ())
(declare-function agent-repl--live-ws-names "workspace" ())
(declare-function agent-repl--ws-by-ref-id "workspace" (id))
(declare-function agent-repl--ws-add-activated-hook "workspace" (fn))

(declare-function agent-repl--notify "notifications" (ws title message &optional activate))
(declare-function agent-repl--notification-activate "notifications" (ws))
(declare-function agent-repl--emacs-focused-p "notifications" (&optional ws))

;; W2-B's surfaces.  NAMED HERE, NEVER DEFINED HERE: status.el owns the
;; blink (whose cadence is frontend.v1 `RosterRowAttention''s), frontend.el
;; owns the webview, popup.el owns the one shared editor popup.
(declare-function agent-repl-status-blink-tab "status" (ws))
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

(defvar agent-repl-host-update-functions nil
  "Functions run with (WS HOST-PLIST) after every `host' push for WS.")

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

(defun agent-repl-host-conn (ws)
  "Return the connection whose daemon owns WS, or nil."
  (plist-get (agent-repl-host--entry ws) :conn))

(defun agent-repl-host-stream (ws)
  "Return WS's standing `WatchHostWorkspace' stream, or nil."
  (plist-get (agent-repl-host--entry ws) :stream))

(defun agent-repl-host-state (ws)
  "Return the last decoded `HostWorkspace' pushed for WS, or nil."
  (plist-get (agent-repl-host--entry ws) :host))

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

(defun agent-repl-host-composer-gate (ws)
  "Return the composer gate for WS as one keyword of the fixed vocabulary.

`:open' `:merge-parked' `:merging' `:draining' `:restarting' come straight
off the LIVE arm's `composer' oneof — the resolved arm IS the gate.  The
other standings are blocked by their own nature and answer `:no-session'
or `:terminal'; `:unknown' means no host push has arrived yet.

The gate is ADVISORY about intent, never a precondition: `:no-session',
`:terminal' and `:unknown' all still SEND (ruled — SubmitPrompt has no
precondition, and the daemon starts or revives the session implicitly),
while `:merging', `:draining' and `:restarting' are refusals input.el
draws.  Emacs enforces it because the composer is host-native."
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
             (if (memq arm '(:open :merging :draining :restarting :merge-parked))
                 arm
               (agent-repl--error ws "elisp.host.gate-unknown-composer-arm ws=%s arm=%S" ws arm)
               :unknown)))
          (arm
           (agent-repl--error ws "elisp.host.gate-unknown-standing ws=%s arm=%S" ws arm)
           :unknown))))
     (t
      (agent-repl--error ws "elisp.host.gate-unset-session ws=%s host=%S" ws host)
      :unknown))))

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

(defun agent-repl-host-register (conn dir on-done)
  "Register DIR with the daemon on CONN; call ON-DONE with the minted ref.
IDEMPOTENT BY DIR — re-registering after a reconnect or a daemon restart
is the normal path, never an error.  A daemon-authored error arm and a
transport failure are different facts and are logged as such; both answer
ON-DONE with nil so the caller never waits on a callback that will not
come."
  (agent-repl--info nil "elisp.host.register dir=%S" dir)
  (agent-repl-rpc-register-workspace
   conn (list :dir dir)
   :on-response
   (lambda (response)
     (pcase (plist-get response :arm)
       (:success
        (let ((ref (plist-get (plist-get response :value) :workspace)))
          (agent-repl--info nil "elisp.host.registered dir=%S id=%S" dir
                            (plist-get ref :id))
          (funcall on-done ref)))
       (:error
        (agent-repl--error nil "elisp.host.register-refused dir=%S error=%S"
                           dir (plist-get response :value))
        (funcall on-done nil))
       (arm
        (agent-repl--error nil "elisp.host.register-unknown-arm dir=%S arm=%S" dir arm)
        (funcall on-done nil))))
   :on-failure
   (lambda (detail)
     (agent-repl--error nil "elisp.host.register-failed dir=%S detail=%S" dir detail)
     (funcall on-done nil))))

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

(defun agent-repl-host-select (ws)
  "Tell the daemon the user switched to workspace WS.
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
                             ws (plist-get ref :id)))
           (:error (agent-repl-host--on-refused ws "select" (plist-get response :value)))
           (arm (agent-repl--error ws "elisp.host.select-unknown-arm ws=%s arm=%S" ws arm))))
       :on-failure
       (lambda (detail)
         (agent-repl--error ws "elisp.host.select-failed ws=%s detail=%S" ws detail)))
      t))))

(defun agent-repl-host--on-workspace-activated (&rest _)
  "Select the newly activated perspective's workspace with the daemon.
Registered on workspace.el's perspective-activation boundary: an ordinary
tab switch IS the SelectWorkspace, and it is also what clears the
workspace's attention marker."
  (let ((ws (agent-repl--ws-current-name)))
    (when (and ws (agent-repl-host--entry ws))
      (agent-repl-host-select ws))))

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
  (when (equal ws (agent-repl--ws-current-name))
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
         (stream (agent-repl-rpc-watch-host-workspace
                  conn ref
                  (lambda (push)
                    (agent-repl-host--handle-push (funcall current) push))
                  (lambda (outcome)
                    (agent-repl-host--handle-close (funcall current) outcome))
                  (lambda ()
                    (let ((now (funcall current)))
                      (agent-repl--info now "elisp.host.subscribed ws=%s method=%S id=%S"
                                        now "WatchHostWorkspace" id))))))
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

(defun agent-repl-host--handle-close (ws outcome)
  "React to WS's host stream closing with OUTCOME.
`(:cancelled)' is Emacs's own unsubscribe and is normal.  Anything else
is the producer dropping a STANDING stream, which the contract calls a
transport failure — the link's own reconnect owns the recovery, so this
only records the fact and drops the dead stream."
  (pcase (car outcome)
    (:cancelled (agent-repl--log ws "elisp.host.stream-cancelled ws=%s" ws))
    (_
     (agent-repl--error ws "elisp.host.stream-lost ws=%s outcome=%S" ws outcome)
     (agent-repl-host--put ws :stream nil))))

;;;; ---- Pushes ----

(defun agent-repl-host--handle-push (ws push)
  "Dispatch one decoded `WatchHostWorkspace' PUSH for workspace WS."
  (let ((arm (plist-get push :arm))
        (value (plist-get push :value)))
    (pcase arm
      (:host (agent-repl-host--apply-state ws value))
      (:notification (agent-repl-host--notify ws value))
      (:transferred (agent-repl-host--transferred ws value))
      (:reload-webapp (agent-repl-host--reload-webapp ws))
      (:open-in-editor (agent-repl-host--open-in-editor ws value))
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
  (agent-repl-host--apply-naming ws)
  (agent-repl--log ws "elisp.host.state ws=%s gate=%S backfill=%S faults=%d"
                   ws (agent-repl-host-composer-gate ws)
                   (agent-repl-host-backfill ws)
                   (length (agent-repl-host-faults ws)))
  (run-hook-with-args 'agent-repl-host-update-functions ws host))

;;;; ---- The notification policy ----

(defun agent-repl-host--notification-context (note)
  "Return a log-context string naming NOTE's typed kind.
The composed text is PRESENTATION; the kind arm is the programmatic
semantics, and a permission ask's gated tool name — or a question batch's
chip header — belongs in the log context rather than in any drawn line
Emacs composes itself."
  (let* ((kind (plist-get note :kind))
         (arm (plist-get kind :arm)))
    (pcase arm
      (:permission-requested
       (format "kind=permission-requested tool=%S"
               (plist-get (plist-get kind :value) :tool-name)))
      (:question-asked
       (format "kind=question-asked header=%S"
               (plist-get (plist-get kind :value) :header)))
      (:agent-addressed "kind=agent-addressed")
      (_ (format "kind=%S" arm)))))

(defun agent-repl-host--notify (ws note)
  "Apply Emacs's notification policy to NOTE for workspace WS.
Exactly the policy stated at the `notification' arm of
`endpoint_watch_host_workspace.proto'; every kind, `permission_requested'
included, follows the same three cases.  The daemon publishes the fact and
never asks whether Emacs is focused — that knowledge is only here."
  (let ((text (plist-get note :text))
        (context (agent-repl-host--notification-context note))
        (selected (equal ws (agent-repl--ws-current-name))))
    (cond
     ((not (agent-repl--emacs-focused-p ws))
      (agent-repl--info ws "elisp.host.notification-desktop ws=%s %s at-ms=%S"
                        ws context (plist-get note :at-ms))
      ;; The click is R-CLICK: the banner carries THIS workspace, and
      ;; activating it raises the frame and selects that tab.  Decider and
      ;; actor are one process, so the activation is plain elisp.
      (agent-repl--notify ws (agent-repl-host-display-title ws) text
                          (lambda () (agent-repl--notification-activate ws))))
     ((not selected)
      (agent-repl--info ws "elisp.host.notification-blink ws=%s %s at-ms=%S"
                        ws context (plist-get note :at-ms))
      (agent-repl-status-blink-tab ws))
     (t
      (agent-repl--info ws "elisp.host.notification-selected ws=%s %s at-ms=%S text=%S"
                        ws context (plist-get note :at-ms) text)))))

;;;; ---- The handover, per workspace ----

(defun agent-repl-host--adopt-onto (ws new)
  "Adopt WS onto the NEW daemon, move its webview there, and re-subscribe.
THE ONE WALK, shared by the `transferred' push and by a per-workspace
rpc's `transferring_away' refusal — both say the same thing, and a second
copy of this order would be a second contract.

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
leaves the workspace on the daemon that still serves it."
  (let ((ref (agent-repl-host-ref ws))
        (address (agent-repl-connect-connection-address new))
        (old (agent-repl-host-conn ws)))
    (if (null ref)
        (agent-repl--error ws "elisp.host.adopt-without-ref ws=%s" ws)
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
           (pcase (plist-get response :arm)
             (:success
              (agent-repl--info ws "elisp.host.adopted ws=%s address=%S" ws address)
              (agent-repl-host-unsubscribe ws)
              (agent-repl-host-subscribe new ws ref))
             (:error
              (funcall restore)
              (agent-repl-host--on-refused ws "adopt" (plist-get response :value)))
             (arm
              (funcall restore)
              (agent-repl--error ws "elisp.host.adopt-unknown-arm ws=%s arm=%S" ws arm))))
         :on-failure
         (lambda (detail)
           (funcall restore)
           (agent-repl--error ws "elisp.host.adopt-failed ws=%s detail=%S" ws detail)))))))

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

It is a WARNING rather than an INFO because the overtaking order is
unusual and worth seeing in a log, and never an ERROR because nothing is
wrong: the announcement is on its way."
  (let ((new (agent-repl-link-successor)))
    (cond
     (new
      (agent-repl--info ws "elisp.host.transferred ws=%s address=%S adopting=t"
                        ws (agent-repl-connect-connection-address new))
      (agent-repl-host--adopt-onto ws new))
     ((agent-repl-link-successor-pending-p)
      (agent-repl--info ws "elisp.host.transferred-awaiting-successor ws=%s" ws)
      (agent-repl-host--adopt-on-acceptance ws))
     (t
      (agent-repl--warn ws "elisp.host.transferred-before-the-announcement ws=%s" ws)
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
            (agent-repl-host--adopt-onto ws new)))
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
                 (agent-repl-host--adopt-onto ws new)
               ;; The dial stands but is not accepted yet; adopting onto an
               ;; unproven daemon is exactly what the acceptance gate forbids.
               (agent-repl--info ws "elisp.host.awaiting-successor ws=%s address=%S"
                                 ws address)
               (agent-repl-host--adopt-on-acceptance ws))))))
      (:not-yet-adopted
       (agent-repl--info ws "elisp.host.not-yet-adopted ws=%s" ws)
       (let ((new (agent-repl-link-successor)))
         (if new
             (agent-repl-host--adopt-onto ws new)
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
  (let ((names (agent-repl--live-ws-names)))
    (agent-repl--info nil "elisp.host.link-up workspaces=%d" (length names))
    (dolist (ws names)
      (let ((dir (agent-repl--ws-get ws :project-dir)))
        (if (null dir)
            (agent-repl--warn ws "elisp.host.link-up-skipped ws=%s reason=no-dir" ws)
          (agent-repl-host-register
           conn dir
           (lambda (ref)
             (if (null ref)
                 (agent-repl--error ws "elisp.host.link-up-register-failed ws=%s dir=%S" ws dir)
               (agent-repl-host-subscribe conn ws ref)
               (agent-repl-frontend-reload-webview ws)
               (agent-repl--info ws "elisp.host.link-up-webview-repointed ws=%s address=%S"
                                 ws (agent-repl-connect-connection-address conn))))))))))

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
    (agent-repl--warn nil "elisp.host.link-down workspaces=%d" affected)))

(add-hook 'agent-repl-link-up-functions #'agent-repl-host-on-link-up)
(add-hook 'agent-repl-link-down-functions #'agent-repl-host-on-link-down)
(agent-repl--ws-add-activated-hook #'agent-repl-host--on-workspace-activated)

(provide 'host)

;;; host.el ends here
