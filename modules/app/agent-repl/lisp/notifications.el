;;; notifications.el --- Emacs's two parts in the daemon's desktop notifications -*- lexical-binding: t; -*-

;;; Commentary:

;; THE DAEMON POSTS EVERY DESKTOP BANNER.  It decides whether to post, runs
;; the platform's banner program, and reads the click back.  Emacs owns
;; exactly two facts in that, and this file is both:
;;
;;   - WHETHER EMACS IS FOCUSED.  The daemon decides every banner on it: a
;;     focused Emacs (whatever workspace is open in it) gets none.  Emacs is
;;     the one process that can see its own focus on every platform, so it
;;     reports it -- on the WatchDaemon request it connects with (rpc.el) and
;;     by `ReportEditorFocus' on every change after (`agent-repl--focus-report').
;;
;;   - WHAT A CLICK DOES.  The host stream's `notification_clicked' push asks
;;     Emacs to raise its frame and select the clicked workspace's tab
;;     (`agent-repl--notification-activate').
;;
;; The tab BLINK is neither: it is drawn from the roster's attention marker
;; (status.el), not from any notification.

;;; Code:

(require 'seq)

(declare-function agent-repl-host-request-switch "host" (ws trigger))
(declare-function agent-repl--log "core" (ws fmt &rest args))
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

(defvar agent-repl--focus-report-in-flight nil
  "Non-nil while a `ReportEditorFocus' call is unanswered.
REPORTS ARE SERIALIZED: two unary calls in flight at once may land in
either order, and the daemon would keep whichever arrived last rather
than the focus Emacs holds now.  One call at a time, and a change during
it (`agent-repl--focus-report-owed') sends the CURRENT focus once it is
answered, is what makes the daemon's focus end on Emacs's.")

(defvar agent-repl--focus-report-owed nil
  "Non-nil when the focus may have changed while a report was in flight.")

(defvar agent-repl--focus-report-timer nil
  "The pending coalesced report, or nil.
Moving focus between two Emacs frames reports out-then-in within one
command loop; the report is taken once that settles, so the daemon is
told where focus came to rest rather than every step on the way.")

(defun agent-repl--focus-report ()
  "Tell the live daemon whether Emacs is focused now.
With no link standing there is nothing to tell: the next WatchDaemon
Emacs opens states its focus itself.  While a report is in flight, the
new one is OWED and sent when that one is answered."
  (let ((conn (agent-repl-link-live)))
    (cond
     ((null conn)
      (agent-repl--log agent-repl--focus-log-scope
                       "elisp.notifications.focus-report skipped reason=no-link"))
     (agent-repl--focus-report-in-flight
      (setq agent-repl--focus-report-owed t)
      (agent-repl--log agent-repl--focus-log-scope
                       "elisp.notifications.focus-report owed reason=in-flight"))
     (t
      (let ((focused (and (agent-repl--emacs-focused-p) t)))
        (setq agent-repl--focus-report-in-flight t
              agent-repl--focus-report-owed nil)
        (agent-repl--info agent-repl--focus-log-scope
                          "elisp.notifications.focus-report send focused=%s" focused)
        (agent-repl-rpc-report-editor-focus
         conn (list :focus (agent-repl-wire-editor-focus focused))
         :on-response (lambda (response) (agent-repl--focus-report-answered focused response))
         :on-failure (lambda (detail) (agent-repl--focus-report-failed focused detail))))))))

(defun agent-repl--focus-report-answered (focused response)
  "Record the daemon's RESPONSE to a report of FOCUSED, and send what is owed."
  (pcase (plist-get response :arm)
    (:success
     (agent-repl--log agent-repl--focus-log-scope
                      "elisp.notifications.focus-report ok focused=%s" focused))
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
  (agent-repl--focus-report-settle))

(defun agent-repl--focus-report-failed (focused detail)
  "Record a report of FOCUSED that failed in transport with DETAIL."
  (agent-repl--error agent-repl--focus-log-scope
                     "elisp.notifications.focus-report failed focused=%s detail=%S"
                     focused detail)
  (agent-repl--focus-report-settle))

(defun agent-repl--focus-report-settle ()
  "End the in-flight report, and send the owed one if focus moved meanwhile."
  (setq agent-repl--focus-report-in-flight nil)
  (when agent-repl--focus-report-owed
    (agent-repl--focus-report)))

(defun agent-repl--focus-changed ()
  "Report Emacs's focus once the change that called this has settled.
Runs from `after-focus-change-function'."
  (unless (timerp agent-repl--focus-report-timer)
    (setq agent-repl--focus-report-timer
          (run-at-time 0 nil (lambda ()
                               (setq agent-repl--focus-report-timer nil)
                               (agent-repl--focus-report))))))

(defun agent-repl--focus-report-on-link (&rest _connections)
  "Report Emacs's focus to a daemon that just became the live one.
A WatchDaemon states the focus Emacs held when it was BUILT; focus that
moved before the stream was accepted (a report then is refused: no
stream stood) is told here, once the stream stands."
  (agent-repl--focus-report))

(add-function :after after-focus-change-function #'agent-repl--focus-changed)
(add-hook 'agent-repl-link-up-functions #'agent-repl--focus-report-on-link)
(add-hook 'agent-repl-link-promote-functions #'agent-repl--focus-report-on-link)

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
