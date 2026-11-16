;;; stale-sink.el --- confirm a sinkless workspace with the daemon -*- lexical-binding: t; -*-

;;; Commentary:

;; A REGISTERED WORKSPACE WHOSE DIRECTORY IS GONE is a stale registration
;; only when the daemon still holds it open.  The daemon removes a landed or
;; closed workspace's worktree itself, and its roster push saying so can come
;; after Emacs's own trailing records for that workspace: read alone, every
;; landing looked like a registry row that outlived its worktree, and the
;; user was shown a warning for an ordinary departure (owner ruling,
;; 2026-11-16).  So the log router routes the record centrally, at INFO, and
;; asks here; this module asks the daemon (`GetWorkspaceStatus') and warns,
;; with the popup, only when the daemon answers that the workspace is open.
;;
;; The ask waits for the link: a workspace met before Emacs has a daemon (a
;; startup) is asked once the link is up (`agent-repl-link-up-functions').

;;; Code:

(require 'cl-lib)

(declare-function agent-repl--info "core" (ws fmt &rest args))
(declare-function agent-repl--warn "core" (ws fmt &rest args))
(declare-function agent-repl--error "core" (ws fmt &rest args))
(declare-function agent-repl--ws-get "core" (ws key))
(declare-function agent-repl-link-live "daemon-link" ())
(declare-function agent-repl-rpc-get-workspace-status "rpc" (conn request &rest keys))
(defvar agent-repl--global-log-scope)
(defvar agent-repl-link-up-functions)

(defvar agent-repl-stale-sink--waiting nil
  "Workspaces met with no link standing, as (WS . DIR), newest first.
Each is asked once the link is up.")

(defun agent-repl-stale-sink-confirm (ws dir)
  "Ask the daemon whether WS, registered at DIR, which is gone, is still open.
Asked now when a link stands, else once the link is up."
  (let ((conn (agent-repl-link-live)))
    (if conn
        (agent-repl-stale-sink--ask conn ws dir)
      (agent-repl--info agent-repl--global-log-scope
                       "elisp.stale-sink.waiting workspace=%S registered-dir=%s; no daemon to ask yet, so it is asked once the link is up"
                       ws dir)
      (push (cons ws dir) agent-repl-stale-sink--waiting))))

(defun agent-repl-stale-sink-flush (conn)
  "Ask CONN's daemon of every workspace met before the link was up."
  (let ((waiting (reverse agent-repl-stale-sink--waiting)))
    (setq agent-repl-stale-sink--waiting nil)
    (dolist (entry waiting)
      (agent-repl-stale-sink--ask conn (car entry) (cdr entry)))))

(defun agent-repl-stale-sink--ask (conn ws dir)
  "Ask CONN's daemon for WS's status, WS registered at DIR."
  (let ((ref (agent-repl--ws-get ws :ref)))
    (if (not (and (plist-get ref :id) (plist-get ref :dir)))
        ;; NO REF, NO ASK: the daemon names a workspace by the ref it minted,
        ;; and a workspace Emacs holds no ref for was never one the daemon
        ;; told it of.
        (agent-repl--info agent-repl--global-log-scope
                         "elisp.stale-sink.no-ref workspace=%S registered-dir=%s; the daemon never named this workspace, so it is not asked and nothing is warned"
                         ws dir)
      (agent-repl-rpc-get-workspace-status
       conn (list :workspace ref)
       :on-response (lambda (response) (agent-repl-stale-sink--answered ws dir response))
       :on-failure (lambda (detail) (agent-repl-stale-sink--failed ws dir detail))))))

(defun agent-repl-stale-sink--answered (ws dir response)
  "Act on the daemon's RESPONSE about WS, registered at DIR."
  (pcase (plist-get response :arm)
    (:success
     (let ((standing (plist-get (plist-get (plist-get response :value) :status) :standing)))
       (if (eq (plist-get standing :arm) :open)
           (agent-repl-stale-sink--warn ws dir)
         (agent-repl--info agent-repl--global-log-scope
                          "elisp.stale-sink.departed workspace=%S registered-dir=%s; the daemon says it is closed, so its gone directory is its departure"
                          ws dir))))
    (:error
     (pcase (plist-get (plist-get response :value) :arm)
       (:unknown-workspace
        (agent-repl--info agent-repl--global-log-scope
                         "elisp.stale-sink.departed workspace=%S registered-dir=%s; the daemon no longer knows it, so its gone directory is its departure"
                         ws dir))
       (arm
        (agent-repl--error agent-repl--global-log-scope
                         "elisp.stale-sink.refused workspace=%S registered-dir=%s arm=%S; the daemon refused the status ask, so the gone directory is unexplained"
                         ws dir arm))))))

(defun agent-repl-stale-sink--failed (ws dir detail)
  "Record that the status ask about WS, registered at DIR, failed with DETAIL."
  (agent-repl--error agent-repl--global-log-scope
                   "elisp.stale-sink.ask-failed workspace=%S registered-dir=%s detail=%S; the gone directory is unexplained"
                   ws dir detail))

(defun agent-repl-stale-sink--warn (ws dir)
  "Warn that WS is open but its registered DIR is gone."
  (agent-repl--warn agent-repl--global-log-scope
                   "elisp.stale-sink.open-without-directory workspace=%S registered-dir=%s; the daemon says it is open, but its directory is gone"
                   ws dir)
  (display-warning
   'agent-repl
   (format "workspace %S is open, but its directory %s is gone; its records are written centrally"
           ws dir)
   :warning))

(add-hook 'agent-repl-link-up-functions #'agent-repl-stale-sink-flush)

(provide 'agent-repl-stale-sink)
;;; stale-sink.el ends here
