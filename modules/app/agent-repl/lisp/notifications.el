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

;; Every log call is a log-site macro (log-site.el), needed where this
;; file is expanded; config.el loads it first.
(require 'log-site)

(require 'seq)

(declare-function agent-repl-host-request-switch-ref "host" (ref trigger))
(declare-function agent-repl-roster-row-of-id "roster" (id))
(declare-function agent-repl-roster-row-closed-p "roster" (row))
(declare-function agent-repl--ws-by-ref-id "workspace" (id))
(declare-function agent-repl-serial-create "core" (&rest slots))
(declare-function agent-repl-serial-send-to-newest "core" (serial report scope label send))
(declare-function agent-repl-rpc-report-editor-focus "rpc" (conn request &rest keys))
(declare-function agent-repl-wire-editor-focus "wire-host" (focused))
(defvar agent-repl--global-log-scope)
(defvar agent-repl-link-up-functions)
(defvar agent-repl-link-handover-functions)
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
        (agent-repl--log-verbose scope "emacs-focused-p noninteractive=nil frame-count={frame_count} focused={focused}"
                                 :frame_count (length frames) :focused focused)
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
  "Tell the daemon that serves next whether Emacs is focused now.
Focus is daemon-wide, so it goes to `agent-repl-link-newest', never to a
daemon a handover is leaving.  With none to take it there is nothing to
tell: the link edge that answers the next daemon reports again
\(`agent-repl--focus-report-on-link'), and a WatchDaemon states the focus
Emacs held when it was built.  While a report is in flight, the
new one is OWED and sent when that one is answered
\(`agent-repl--focus-serial')."
  (agent-repl-serial-send-to-newest
   agent-repl--focus-serial #'agent-repl--focus-report
   agent-repl--focus-log-scope "notifications.focus-report"
   (lambda (conn done)
     (let ((focused (and (agent-repl--emacs-focused-p) t)))
       (agent-repl--info agent-repl--focus-log-scope
                         "elisp.notifications.focus-report send focused={focused}" :focused focused)
       (agent-repl-rpc-report-editor-focus
        conn (list :focus (agent-repl-wire-editor-focus focused))
        :on-response (lambda (response) (agent-repl--focus-report-answered focused response done))
        :on-failure (lambda (detail) (agent-repl--focus-report-failed focused detail done)))))))

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
                       "elisp.notifications.focus-report refused focused={focused} cause={cause}"
                       :focused focused :cause (prin1-to-string (plist-get (plist-get response :value) :cause))))
    (arm
     (agent-repl--error agent-repl--focus-log-scope
                        "elisp.notifications.focus-report unknown-arm focused={focused} arm={arm}"
                        :focused focused :arm (prin1-to-string arm))))
  (funcall done))

(defun agent-repl--focus-report-failed (focused detail done)
  "Record a report of FOCUSED that failed in transport with DETAIL.
DONE ends the report."
  (agent-repl--error agent-repl--focus-log-scope
                     "elisp.notifications.focus-report failed focused={focused} detail={detail}"
                     :focused focused :detail (prin1-to-string detail))
  (funcall done))

(defvar agent-repl-focus-settled-hook nil
  "Run once Emacs's desktop focus has settled after a change.
Everything that follows Emacs's focus rides this one hook, so a move
between two Emacs frames (out, then in, in one command loop) reaches each
of them once, as where the focus came to rest: the daemon's report
\(`agent-repl--focus-report') and the orange title bar
\(`agent-repl-titlebar-sync').  Each asks
`agent-repl--emacs-focused-p' itself.")

(defun agent-repl--focus-changed ()
  "Run `agent-repl-focus-settled-hook' once the calling change has settled.
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
(add-hook 'agent-repl-link-handover-functions #'agent-repl--focus-report-on-link)
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
     (let* ((ref (plist-get (plist-get claim :value) :workspace))
            (id (plist-get ref :id))
            (row (agent-repl-roster-row-of-id id)))
       (if (and row (not (agent-repl-roster-row-closed-p row)))
           ;; AN OPEN WORKSPACE IS LANDED ON, held here or not: a folded or
           ;; backlogged one is opened by the roster push its selection brings
           ;; (`agent-repl-host-request-switch-ref').
           (progn
             (agent-repl--info agent-repl--focus-log-scope
                               "elisp.notifications.claim banner id={id} held={held}"
                               :id id :held (and (agent-repl--ws-by-ref-id id) t))
             (agent-repl-host-request-switch-ref ref 'notification-focus))
         ;; THE BANNER'S WORKSPACE IS CLOSED, or gone from the roster, since the
         ;; banner was made: there is nowhere to land, so the user stays where
         ;; they are.
         (agent-repl--info agent-repl--focus-log-scope
                           "elisp.notifications.claim banner-without-tab id={id}" :id id))))
    (:nothing-held
     (agent-repl--log agent-repl--focus-log-scope
                      "elisp.notifications.focus-report ok focused={focused} claim=nothing-held" :focused focused))
    (:no-gain
     (agent-repl--log agent-repl--focus-log-scope
                      "elisp.notifications.focus-report ok focused={focused} claim=no-gain" :focused focused))
    (arm
     (agent-repl--error agent-repl--focus-log-scope
                        "elisp.notifications.claim unknown-arm focused={focused} arm={arm}"
                        :focused focused :arm (prin1-to-string arm)))))

;;;; ---- The click ----

(defun agent-repl--notification-activate (ref)
  "Raise the Emacs frame and switch to the workspace REF names.
THE CLICK ACTION of the daemon stream's `notification_clicked' push: the
daemon posted the banner and read the click back.  Like every switch
trigger it REQUESTS the switch (`agent-repl-host-request-switch-ref'),
held by this editor or not, and the frame follows the roster's `current';
the daemon clears the attention marker on the selection.  The frame is
raised and focused at once, so the click brings Emacs forward."
  (agent-repl--log '(:agent-repl-central "a banner click is resolved to its workspace by the switch")
                   "elisp.notifications.activate id={id}" :id (plist-get ref :id))
  (agent-repl-host-request-switch-ref ref 'notification)
  (select-frame-set-input-focus (selected-frame)))

(provide 'notifications)

;;; notifications.el ends here
