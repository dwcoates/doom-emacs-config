;;; webview-recovery.el --- every workspace's page, made ahead of the look -*- lexical-binding: t; -*-

;;; Commentary:

;; PAGES ARE MADE AHEAD OF THE LOOK.  Every eligible workspace gets its
;; webview slot and its page in the host web view (webhost.el) the moment it
;; is buildable -- on the link-up, on every roster push, when its tab is
;; created -- so a switch to it shows a page that is already live and drawn.
;; A slot is a buffer and a page is one call to the host, so every owed page
;; is made in one immediate pass; nothing is staggered or queued.
;;
;; THE HOST IS MADE REGARDLESS OF FOCUS (owner ruling, 2026-10-12): a page
;; is loaded when its workspace is asked open, whether or not Emacs is the
;; focused app.  A HIDDEN workspace (hidden, or in a collapsed repository
;; section) is owed no page: it runs nothing (open-tabs.el).

;; THE STALE-WEBVIEW SWEEP IS GONE, and so is every script it drove.  A
;; rebuilt webapp reaches every page through the daemon's own
;; `reload_webapp' push (frontend.el reloads the host), and a daemon handover
;; re-points a page as a side effect.

;;; Code:


(require 'cl-lib)

(declare-function agent-repl--panels-mount-awaited-view "panels" (ws))
(declare-function agent-repl--with-deferred-quit "agent-repl-core")
(declare-function agent-repl--log "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--warn "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--info "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--live-ws-names "agent-repl-workspace" ())
(declare-function agent-repl--frontend-precreate-refusal "agent-repl-frontend" (ws))

(defvar agent-repl-link-up-functions)
(defvar agent-repl-roster-update-functions)

(defconst agent-repl--webview-precreate-central
  '(:agent-repl-central "webview recovery spans workspaces")
  "The log scope of a pre-creation record.")

(defun agent-repl--webview-precreate-needed-p (ws)
  "Return non-nil when WS is owed a webview slot and page and has none.
Answered by `agent-repl--frontend-precreate-refusal' -- the SAME
eligibility the mount itself applies -- so a pass can never make a page the
mount would refuse, nor skip one it would accept."
  (null (agent-repl--frontend-precreate-refusal ws)))

(defun agent-repl--webview-precreate-missing ()
  "Return every live workspace owed a webview slot and page it does not have."
  (cl-remove-if-not #'agent-repl--webview-precreate-needed-p
                    (agent-repl--live-ws-names)))

(defun agent-repl--webview-precreate-pass ()
  "Make every owed workspace's slot and page now; return how many were made.
The body of `agent-repl-webview-precreate-all'.  A workspace whose mount
signals is warned about and the pass goes on: one workspace's failure must
not cost the rest their pages."
  (let ((made 0))
    (dolist (ws (agent-repl--webview-precreate-missing))
      (condition-case err
          (progn
            ;; The workspace the user stands on may have declined its
            ;; panel restore for want of exactly this view.
            (agent-repl--panels-mount-awaited-view ws)
            (setq made (1+ made))
            (agent-repl--info agent-repl--webview-precreate-central
                              "elisp.webview-recovery.precreate-created ws=%s" ws))
        (error (agent-repl--warn ws "webview-precreate: ws=%s outcome=failed err=%S"
                                 ws err))))
    made))

(defun agent-repl-webview-precreate-all ()
  "Make every live workspace's owed slot and page, returning how many were made.

A PASS IS A CRITICAL SECTION FOR QUITS.  A pass can run at the same
instant a keypress arrives; a `C-g' landing here
found Emacs BUSY rather than waiting in `read_char', so it could only arm
`quit-flag', which a checkpoint inside the pass then took inside a standing
minibuffer's own recursive edit.  So the pass runs under
`agent-repl--with-deferred-quit', which holds the quit off and LEAVES IT
ARMED for Emacs's own input wait to turn into the `C-g' the read aborts on."
  (let ((made (agent-repl--with-deferred-quit "webview-precreate-pass"
                (agent-repl--webview-precreate-pass))))
    (when (> made 0)
      (agent-repl--info agent-repl--webview-precreate-central
                        "elisp.webview-recovery.precreate-all: made=%d" made))
    made))

(defun agent-repl--webview-precreate-on-link-up (&optional _conn)
  "Make the pages the link-up made buildable.
Registered on `agent-repl-link-up-functions': before the link is up there
is no daemon address to build a URL from, and this edge is the first
moment every open workspace is buildable."
  (agent-repl-webview-precreate-all))

(add-hook 'agent-repl-link-up-functions #'agent-repl--webview-precreate-on-link-up)

(defun agent-repl--webview-precreate-on-roster-update (&optional _roster)
  "Make the pages a roster push made buildable.
Registered on `agent-repl-roster-update-functions'.  A COLD start has no
workspaces registered at link-up -- their `WorkspaceRef's arrive on the
first roster push, and until then every workspace refuses with `:no-ref'
-- so this edge is the first moment a cold workspace is buildable.  A
workspace that already has its page is not owed one, so running on both
hooks makes nothing twice."
  (agent-repl-webview-precreate-all))

(add-hook 'agent-repl-roster-update-functions #'agent-repl--webview-precreate-on-roster-update)

(provide 'webview-recovery)
;;; webview-recovery.el ends here
