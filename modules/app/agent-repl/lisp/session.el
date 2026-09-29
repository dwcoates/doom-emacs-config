;;; session.el --- the finish-edge reactions and the model preferences -*- lexical-binding: t; -*-

;;; Commentary:

;; WHAT IS LEFT OF SESSIONS IN EMACS: nothing.  The DAEMON owns the session
;; lifecycle whole — which account a workspace runs against, which model,
;; which permission mode, whether the conversation is fresh or resumed, how
;; the vendor process is spawned, when it dies and when it comes back.  None
;; of that is expressible from here any more: session facts travel only in
;; the daemon's own StartSession, and Emacs never assembles a command line.
;;
;; So the command assembly, the config-dir and permission-flag computation,
;; the display-state save/load, the session-id bookkeeping and the local
;; liveness predicates are all gone with the machinery that produced them.
;;
;; TWO THINGS REMAIN HERE, and they are here because they are genuinely
;; Emacs's:
;;
;; 1. THE FINISH-EDGE REACTION'S HELPER.  The finish edge itself is the
;;    roster row's RUNNING -> SETTLED transition (roster.el owns it and runs
;;    `agent-repl-roster-finish-functions'), and the magit refresh needs
;;    knowledge only this process has: which magit buffers are looking at
;;    the workspace's worktree.  The desktop banner a turn end earns is the
;;    daemon's (it posts every banner, decided on Emacs's reported focus).
;;
;; 2. THE MODEL PREFERENCES.  Emacs still has an opinion about which model a
;;    workspace it CREATES should run under, and it travels on the wire as a
;;    field of CreateWorkspace rather than as a flag on a command line.  The
;;    preference is a user setting, so it lives in Emacs; the enforcement is
;;    the daemon's.

;;; Code:

(declare-function agent-repl--log "core" (ws format-string &rest args))
(declare-function agent-repl--log-verbose "core" (ws format-string &rest args))
(declare-function agent-repl--info "core" (ws format-string &rest args))
(declare-function agent-repl--warn "core" (ws format-string &rest args))
(declare-function agent-repl--error "core" (ws format-string &rest args))
(declare-function agent-repl--path-canonical "core" (path))
(declare-function magit-refresh "ext:magit" ())

;;;; ---- Model preferences ------------------------------------------------
;;
;; WHICH MODEL a workspace runs under is a CreateWorkspace field now, not a
;; `--model' flag: the daemon starts the session and Emacs supplies the
;; preference the user set.  These are read by the creation commands
;; (verbs.el) and by nothing else.

(defcustom agent-repl-interactive-model "opus"
  "Model alias Emacs asks for when it creates an interactive workspace.
Travels as CreateWorkspace's model field; nil asks for no particular
model and lets the daemon pick.  It is the INITIAL INPUT of verbs.el's
model picker (`agent-repl-verbs--read-model'), so this setting is what
a bare RET sends and the picker still takes any override.  Does NOT
affect the headless prompt summariser, which has its own model
variable."
  :type '(choice (const :tag "Let the daemon choose" nil)
                 (string :tag "Model alias"))
  :group 'agent-repl)

;; `agent-repl-oneshot-model-candidates' lives in verbs.el, beside the
;; one-shot creation commands that are its only consumer (ruled at the
;; wave-2 seam).  It is deliberately NOT a model preference of the
;; interactive session, which is what this section otherwise holds.

;;;; ---- The magit refresh ------------------------------------------------

(defun agent-repl--refresh-magit-status-for-dir (dir &optional ws)
  "Refresh any magit-status buffer whose `default-directory' canonicalizes to DIR.

REACTION (2) OF THE FINISH EDGE.  The turn ended, so the worktree the
user has a status buffer open on has probably changed underneath it, and
only this process knows which buffers those are.

WS is used for the log context only; a directory-keyed caller passes
nil.  No-op when DIR is nil or nothing is looking at it."
  (if-let* ((canonical (and dir (agent-repl--path-canonical dir))))
      (let ((refreshed 0))
        (dolist (buf (buffer-list))
          (when (and (buffer-live-p buf)
                     (with-current-buffer buf
                       (and (eq major-mode 'magit-status-mode)
                            (equal (agent-repl--path-canonical default-directory)
                                   canonical))))
            (agent-repl--log ws "elisp.session.magit-refresh: dir=%s buffer=%s"
                             canonical (buffer-name buf))
            (with-current-buffer buf (magit-refresh))
            (setq refreshed (1+ refreshed))))
        (agent-repl--log ws "elisp.session.magit-refresh: complete dir=%s refreshed=%d"
                         canonical refreshed))
    (agent-repl--log ws "elisp.session.magit-refresh: skipped dir=%S reason=no-directory" dir)))

(provide 'agent-repl-session)
;;; session.el ends here
