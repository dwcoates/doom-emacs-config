;;; notifications.el --- Emacs's three parts in the daemon's desktop notifications -*- lexical-binding: t; -*-

;;; Commentary:

;; THE DAEMON POSTS EVERY DESKTOP BANNER.  It decides whether to post, runs
;; the platform's banner program, and reads the click back.  Emacs owns
;; exactly three parts in that, and this file is all of them:
;;
;;   - WHETHER EMACS IS FOCUSED.  The daemon decides every banner on it: a
;;     focused Emacs (whatever workspace is open in it) gets none.  Emacs is
;;     the one process that can see its own focus on every platform, so it
;;     reports it -- on the WatchDaemon request it connects with (rpc.el) and
;;     by `ReportEditorFocus' on every change after (`agent-repl--focus-report').
;;
;;   - WHAT A CLICK DOES, on Linux.  The host stream's `notification_clicked'
;;     push asks Emacs to raise its frame and select the clicked workspace's
;;     tab (`agent-repl--notification-activate').  On macOS the banner
;;     program brings Emacs forward and the click's own SelectWorkspace moves
;;     the roster's `current', which the frame follows like any other switch,
;;     so no push reaches Emacs.
;;
;;   - WHAT COMING FORWARD DOES.  The report that Emacs GAINED focus, however
;;     the user brought it forward (the dock, an application switcher),
;;     claims the newest banner the daemon holds IN THE SAME CALL: its answer
;;     names the banner's workspace, which Emacs selects
;;     (`agent-repl--focus-claim-answered').  One round trip from coming
;;     forward to the switch, and no report can land between the gain and
;;     its claim.
;;
;; The tab BLINK is none of them: it is drawn from the roster's attention marker
;; (status.el), not from any notification.

;;; Code:

(require 'seq)

(declare-function agent-repl-host-request-switch "host" (ws trigger))
(declare-function agent-repl--ws-by-ref-id "workspace" (id))
(declare-function agent-repl--log "core" (ws fmt &rest args))
(declare-function agent-repl-serial-create "core" (&rest slots))
(declare-function agent-repl-serial-send "core" (serial report send))
(declare-function agent-repl--info "core" (ws fmt &rest args))
(declare-function agent-repl--log-verbose "core" (ws fmt &rest args))
(declare-function agent-repl--warn "core" (ws fmt &rest args))
(declare-function agent-repl--error "core" (ws fmt &rest args))
(declare-function agent-repl-link-live "daemon-link" ())
(declare-function agent-repl-rpc-report-editor-focus "rpc" (conn request &rest keys))
(declare-function agent-repl-wire-editor-focus "wire-host" (focused))
(defvar agent-repl--global-log-scope)
(defvar agent-repl-link-up-functions)
(defvar agent-repl-link-promote-functions)

;;;; ---- Emacs's focus ----

(defun agent-repl--emacs-focused-p (&optional ws)
  "Return non-nil when Emacs is the focused desktop application.
Emacs owns desktop focus when ANY of its live frames holds OS input
focus, so this scans `frame-focus-state' across every frame rather than
only the selected one — a focused-but-not-selected frame still means
Emacs is frontmost.  A frame whose focus is `unknown' counts as focused
too: the daemon suppresses a banner when Emacs is possibly focused.
Returns nil under `noninteractive' (batch/ERT), where no window-system
frame can hold focus.

WS attributes the observation to the workspace that asked for it.  DESKTOP
FOCUS IS A GLOBAL FACT, so a caller with no workspace to name gets the
CENTRAL sink explicitly rather than an unattributed workspace-owned
record: the routing rung records the latter as `log-routing-error' at
ERROR, and rightly -- missing attribution at the call site is a defect no
directory can supply.  Here there is nothing missing to supply."
  (let ((scope (or ws agent-repl--global-log-scope)))
    (if noninteractive
        (progn
          (agent-repl--log-verbose scope "emacs-focused-p noninteractive=t focused=nil")
          nil)
      (let* ((frames (frame-list))
             (focused (seq-some #'frame-focus-state frames)))
        (agent-repl--log-verbose scope "emacs-focused-p noninteractive=nil frame-count=%s focused=%s"
                                 (length frames) focused)
        focused))))

;;;; ---- Reporting the focus ----

(defconst agent-repl--focus-log-scope
  '(:agent-repl-central "desktop focus is Emacs-wide")
  "The log scope every focus report records under.")

(defvar agent-repl--focus-serial (agent-repl-serial-create)
  "The focus reports' serializer (`agent-repl-serial-send').
Two reports in flight at once may land in either order, and the daemon
would keep whichever arrived last rather than the focus Emacs holds now;
one at a time, the current focus sent once the one in flight is answered,
is what makes the daemon's focus end on Emacs's.")

(defvar agent-repl--focus-report-timer nil
  "The pending coalesced report, or nil.
Moving focus between two Emacs frames reports out-then-in within one
command loop; the report is taken once that settles, so the daemon is
told where focus came to rest rather than every step on the way.")

(defun agent-repl--focus-report ()
  "Tell the live daemon whether Emacs is focused now.
With no link standing there is nothing to tell: the next WatchDaemon
Emacs opens states its focus itself.  While a report is in flight, the
new one is OWED and sent when that one is answered
\(`agent-repl--focus-serial')."
  (let ((conn (agent-repl-link-live)))
    (if (null conn)
        (agent-repl--log agent-repl--focus-log-scope
                         "elisp.notifications.focus-report skipped reason=no-link")
      (when (eq (agent-repl-serial-send
                 agent-repl--focus-serial #'agent-repl--focus-report
                 (lambda (done)
                   (let ((focused (and (agent-repl--emacs-focused-p) t)))
                     (agent-repl--info agent-repl--focus-log-scope
                                       "elisp.notifications.focus-report send focused=%s" focused)
                     (agent-repl-rpc-report-editor-focus
                      conn (list :focus (agent-repl-wire-editor-focus focused))
                      :on-response (lambda (response) (agent-repl--focus-report-answered focused response done))
                      :on-failure (lambda (detail) (agent-repl--focus-report-failed focused detail done))))))
                :owed)
        (agent-repl--log agent-repl--focus-log-scope
                         "elisp.notifications.focus-report owed reason=in-flight")))))

(defun agent-repl--focus-report-answered (focused response done)
  "Record the daemon's RESPONSE to a report of FOCUSED, then end it with DONE.
A success carries what the report claimed (`agent-repl--focus-claim-answered')."
  (pcase (plist-get response :arm)
    (:success
     (agent-repl--focus-claim-answered
      focused (plist-get (plist-get response :value) :claim)))
    (:error
     ;; NO EMACS STREAM STANDS on that daemon: the link is between streams.
     ;; The stream that comes back states its focus itself, so nothing is
     ;; lost, and it is recorded as the ordinary reconnect it is.
     (agent-repl--info agent-repl--focus-log-scope
                       "elisp.notifications.focus-report refused focused=%s cause=%S"
                       focused (plist-get (plist-get response :value) :cause)))
    (arm
     (agent-repl--error agent-repl--focus-log-scope
                        "elisp.notifications.focus-report unknown-arm focused=%s arm=%S"
                        focused arm)))
  (funcall done))

(defun agent-repl--focus-report-failed (focused detail done)
  "Record a report of FOCUSED that failed in transport with DETAIL.
DONE ends the report."
  (agent-repl--error agent-repl--focus-log-scope
                     "elisp.notifications.focus-report failed focused=%s detail=%S"
                     focused detail)
  (funcall done))

(defvar agent-repl-focus-settled-hook nil
  "Run once Emacs's desktop focus has settled after a change.
Everything that follows Emacs's focus rides this one hook, so a move
between two Emacs frames (out, then in, in one command loop) reaches each
of them once, as where the focus came to rest: the daemon's report
\(`agent-repl--focus-report') and the webhost's unfocused shade
\(`agent-repl-webhost-sync-editor-focus').  Each asks
`agent-repl--emacs-focused-p' itself.")

(defun agent-repl--focus-changed ()
  "Run `agent-repl-focus-settled-hook' once the change that called this has settled.
Runs from `after-focus-change-function'."
  (unless (timerp agent-repl--focus-report-timer)
    (setq agent-repl--focus-report-timer
          (run-at-time 0 nil (lambda ()
                               (setq agent-repl--focus-report-timer nil)
                               (run-hooks 'agent-repl-focus-settled-hook))))))

(defun agent-repl--focus-report-on-link (&rest _connections)
  "Report Emacs's focus to a daemon that just became the live one.
A WatchDaemon states the focus Emacs held when it was BUILT; focus that
moved before the stream was accepted (a report then is refused: no
stream stood) is told here, once the stream stands."
  (agent-repl--focus-report))

(add-function :after after-focus-change-function #'agent-repl--focus-changed)
(add-hook 'agent-repl-focus-settled-hook #'agent-repl--focus-report)
(add-hook 'agent-repl-link-up-functions #'agent-repl--focus-report-on-link)
(add-hook 'agent-repl-link-promote-functions #'agent-repl--focus-report-on-link)

;;;; ---- Coming forward ----

(defun agent-repl--focus-claim-answered (focused claim)
  "Select the workspace CLAIM names, the claim a report of FOCUSED made.
CLAIM is the success's `claim' oneof: `:banner' is a FOCUS GAIN that
claimed the newest banner the daemon held, whose workspace Emacs selects
like any other switch; `:nothing-held' is a gain with no banner held, and
`:no-gain' any other report, so nothing moves."
  (pcase (plist-get claim :arm)
    (:banner
     (let* ((id (plist-get (plist-get (plist-get claim :value) :workspace) :id))
            (ws (agent-repl--ws-by-ref-id id)))
       (if ws
           (progn
             (agent-repl--info ws "elisp.notifications.claim banner ws=%s id=%s" ws id)
             (agent-repl-host-request-switch ws 'notification-focus))
         ;; THE BANNER'S WORKSPACE HAS NO TAB in this Emacs (closed or hidden
         ;; since the banner was made): there is nowhere to land, so the user
         ;; stays where they are.
         (agent-repl--info agent-repl--focus-log-scope
                           "elisp.notifications.claim banner-without-tab id=%s" id))))
    (:nothing-held
     (agent-repl--log agent-repl--focus-log-scope
                      "elisp.notifications.focus-report ok focused=%s claim=nothing-held" focused))
    (:no-gain
     (agent-repl--log agent-repl--focus-log-scope
                      "elisp.notifications.focus-report ok focused=%s claim=no-gain" focused))
    (arm
     (agent-repl--error agent-repl--focus-log-scope
                        "elisp.notifications.claim unknown-arm focused=%s arm=%S"
                        focused arm))))

;;;; ---- The click ----

(defun agent-repl--notification-activate (ws)
  "Raise the Emacs frame and select workspace WS's tab.
THE CLICK ACTION of the host stream's `notification_clicked' push: the
daemon posted the banner and read the click back.  Like every switch
trigger it REQUESTS the switch (`agent-repl-host-request-switch') and the
frame follows the roster's `current'; the daemon clears the attention
marker on the selection.  The frame is raised and focused at once, so the
click brings Emacs forward.  A nil or empty WS still focuses Emacs rather than
attempting a bogus jump."
  (agent-repl--log ws "elisp.notifications.activate ws=%s" ws)
  (let ((navigable (and ws (stringp ws) (not (string-empty-p ws)))))
    (if navigable
        (progn
          (agent-repl--log ws "elisp.notifications.activate-navigable ws=%s navigable=t" ws)
          (agent-repl-host-request-switch ws 'notification))
      ;; An unknown workspace is a WARNING and NO switch: a click that
      ;; cannot name where to go still brings Emacs forward, but guessing a
      ;; destination would move the user somewhere nobody asked for.
      (agent-repl--warn ws "elisp.notifications.activate-unknown-workspace ws=%S" ws))
    (select-frame-set-input-focus (selected-frame))))

(provide 'notifications)

;;; notifications.el ends here
