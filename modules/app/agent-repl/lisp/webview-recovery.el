;;; webview-recovery.el --- paced webview pre-creation -*- lexical-binding: t; -*-

;;; Commentary:

;; WEBVIEW PRE-CREATION, STAGGERED.  Mounting a WKWebView and loading the
;; webapp is the slow part of opening a workspace; doing it at first look
;; puts that cost in front of the user every time.  So every eligible
;; workspace's page is built ahead of the look, and the mounts are paced one
;; per tick so a link-up owing ten of them does not spawn ten WebKit content
;; processes in one command-loop turn.  Each webview is then BOUND TO ITS
;; WORKSPACE BUFFER FOR LIFE.
;;
;; THE STALE-WEBVIEW SWEEP IS GONE, and so is every script it drove.  A
;; rebuilt webapp reaches a live page through the daemon's own
;; `reload_webapp' push (frontend.el navigates the widget), and a daemon
;; handover re-attaches the page as a side effect — neither needs Emacs to
;; go looking for pages to repair, and neither needs a JavaScript hook.
;; There is no `window.agentRepl*' surface at all: the webview is purely
;; daemon-driven.

;;; Code:


(require 'cl-lib)
(require 'url-util)

(declare-function agent-repl--log"agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--log-verbose "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--warn "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--live-ws-names "agent-repl-workspace" ())
(declare-function agent-repl--ws-get "agent-repl-workspace" (ws key))
(declare-function agent-repl--frontend-precreate-webview "agent-repl-frontend" (ws))
(declare-function agent-repl--frontend-precreate-refusal "agent-repl-frontend" (ws))
(declare-function agent-repl--ws-live-p "agent-repl-workspace" (ws))
(declare-function agent-repl--ws-gui-frontend-p "agent-repl-frontends" (ws))
(declare-function agent-repl--info "agent-repl-core" (ws fmt &rest args))

(defvar agent-repl-link-up-functions)


(defcustom agent-repl-webview-precreate-stagger-seconds 0.02
  "Seconds between two paced webview pre-creations.
Each pre-creation spawns a WebKit content process, and a startup sweep
can be owed ten of them at once.  The timer chain exists so eligibility
is re-checked between mounts and the command loop is handed back after
each one; the interval is deliberately near-zero because a full burst is
brief (each mount itself is milliseconds -- the heavy work happens in
WebKit's own processes) and a fast rollout is preferred over a stretched
one.  Raise this if a burst ever produces a visible hitch."
  :type 'number
  :group 'agent-repl)

(defvar agent-repl--webview-precreate-queue nil
  "Workspaces still owed a paced pre-creation, in creation order.")

(defvar agent-repl--webview-precreate-timer nil
  "The timer draining `agent-repl--webview-precreate-queue', or nil.")

(defun agent-repl--webview-precreate-needed-p (ws)
  "Return non-nil when WS is owed a webview and has none.

Answered by `agent-repl--frontend-precreate-refusal' — the SAME
eligibility the mount itself applies — so the queue can never hold a
workspace the mount would then refuse, nor skip one it would accept.
It is re-asked here as well as at the mount because a workspace can be
closed, killed, merged or fenced during the seconds a paced queue takes
to drain, and a page for it must not appear afterwards."
  (null (agent-repl--frontend-precreate-refusal ws)))

(defun agent-repl--webview-precreate-drain ()
  "Pre-create the queue's next workspace, then re-arm for the one after.
One mount per tick.  Eligibility is RE-CHECKED at the tick rather than
trusted from when the workspace was queued, so a workspace closed mid
drain gets no page.  A mount that signals is warned about and the drain
continues: one workspace's failure must not strand the rest of the queue."
  (setq agent-repl--webview-precreate-timer nil)
  (let ((ws (pop agent-repl--webview-precreate-queue)))
    (when ws
      (condition-case err
          (when (agent-repl--webview-precreate-needed-p ws)
            (agent-repl--frontend-precreate-webview ws))
        (error (agent-repl--warn ws "webview-precreate: ws=%s outcome=failed err=%S"
                                 ws err))))
    (when agent-repl--webview-precreate-queue
      (setq agent-repl--webview-precreate-timer
            (run-at-time agent-repl-webview-precreate-stagger-seconds nil
                         #'agent-repl--webview-precreate-drain)))))

(defun agent-repl--webview-precreate-schedule (workspaces)
  "Queue WORKSPACES for paced pre-creation, returning how many were queued.
Appends rather than replaces, so a sweep landing while an earlier queue
is draining cannot drop the workspaces that queue still owes.  A
workspace already queued is not queued twice."
  (let ((added 0))
    (dolist (ws workspaces)
      (unless (member ws agent-repl--webview-precreate-queue)
        (setq agent-repl--webview-precreate-queue
              (append agent-repl--webview-precreate-queue (list ws)))
        (setq added (1+ added))))
    (when (and agent-repl--webview-precreate-queue
               (null agent-repl--webview-precreate-timer))
      (setq agent-repl--webview-precreate-timer
            (run-at-time agent-repl-webview-precreate-stagger-seconds nil
                         #'agent-repl--webview-precreate-drain)))
    added))

(defun agent-repl--webview-precreate-missing ()
  "Return every live workspace owed a webview it does not have."
  (cl-remove-if-not #'agent-repl--webview-precreate-needed-p
                    (agent-repl--live-ws-names)))

(defun agent-repl-webview-precreate-all ()
  "Queue every live workspace owed a webview, returning how many were queued.
The whole of what a link-up needs: a workspace whose page is already
mounted is not queued again, and one that becomes ineligible while the
queue drains is skipped at its tick."
  (let ((queued (agent-repl--webview-precreate-schedule
                 (agent-repl--webview-precreate-missing))))
    (agent-repl--info '(:agent-repl-central "webview recovery spans workspaces") "elisp.webview-recovery.precreate-all: queued=%d" queued)
    queued))

(defun agent-repl--webview-precreate-on-link-up (&optional _conn)
  "Pre-create the pages the link-up made buildable.
Registered on `agent-repl-link-up-functions': before the link is up
there is no daemon address to build a URL from, so a workspace's page
cannot be mounted, and this edge is the first moment every open
workspace is buildable."
  (agent-repl--log '(:agent-repl-central "webview recovery spans workspaces") "elisp.webview-recovery.link-up: pre-creating")
  (agent-repl-webview-precreate-all))

(add-hook 'agent-repl-link-up-functions #'agent-repl--webview-precreate-on-link-up)

(provide 'webview-recovery)
;;; webview-recovery.el ends here
