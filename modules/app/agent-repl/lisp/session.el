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
;; 1. THE FINISH-EDGE REACTIONS' HELPERS.  The finish edge itself is the
;;    roster row's RUNNING -> SETTLED transition (roster.el owns it and runs
;;    `agent-repl-roster-finish-functions'), but two of the reactions need
;;    knowledge only this process has: whether Emacs is the focused desktop
;;    application, and which magit buffers are looking at the workspace's
;;    worktree.  The daemon publishes the FACT; each surface applies the
;;    policy it alone can.
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
(declare-function agent-repl--ws-get "workspace" (ws key))
(declare-function agent-repl--ws-put "workspace" (ws key val))
(declare-function agent-repl--emacs-focused-p "notifications" (&optional ws))
(declare-function agent-repl--notify "notifications" (ws title message))
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
model and lets the daemon pick.  Does NOT affect the headless prompt
summariser, which has its own model variable."
  :type '(choice (const :tag "Let the daemon choose" nil)
                 (string :tag "Model alias"))
  :group 'agent-repl)

;; `agent-repl-oneshot-model-candidates' lives in verbs.el, beside the
;; one-shot creation commands that are its only consumer (ruled at the
;; wave-2 seam).  It is deliberately NOT a model preference of the
;; interactive session, which is what this section otherwise holds.

;;;; ---- The unfocused banner ---------------------------------------------

(defcustom agent-repl-notify-debounce-seconds 2.0
  "Minimum seconds between desktop notifications for the same workspace."
  :type 'number
  :group 'agent-repl)

(defcustom agent-repl-notify-delay 0.1
  "Seconds to delay before sending a desktop notification."
  :type 'number
  :group 'agent-repl)

(defun agent-repl--maybe-notify-finished (ws)
  "Post the \"Agent ready\" desktop banner for WS, when policy allows.

REACTION (1) OF THE FINISH EDGE, called from
`agent-repl-roster-finish-functions'.  EMACS OWNS THIS PRESENTATION
POLICY because Emacs owns the knowledge: the daemon publishes that the
turn finished and never asks whether Emacs is focused.  A banner is only
useful when the user is looking elsewhere, so a focused Emacs posts
none.

Debounced per workspace: the edge can be observed more than once in a
breath when a roster push lands beside another reaction, and two
identical banners for one turn is a defect the user would blame on the
agent."
  (let ((last (agent-repl--ws-get ws :last-notify-time))
        (now (float-time))
        (focused (agent-repl--emacs-focused-p)))
    (cond
     (focused
      (agent-repl--log ws "elisp.session.notify-finished: skipped ws=%s reason=emacs-focused" ws))
     ((and last (<= (- now last) agent-repl-notify-debounce-seconds))
      (agent-repl--log ws "elisp.session.notify-finished: skipped ws=%s reason=debounce elapsed=%.2f"
                       ws (- now last)))
     (t
      (agent-repl--ws-put ws :last-notify-time now)
      ;; THE BANNER TEXT IS "Agent ready: <name>", frozen: the workspace name
      ;; is the fact the user is scanning for in a stack of notifications, and
      ;; it reads there under one fixed prefix rather than as a bare prefix of
      ;; its own.
      (run-at-time agent-repl-notify-delay nil #'agent-repl--notify ws "Agent REPL"
                   (format "Agent ready: %s" ws))
      (agent-repl--info ws "elisp.session.notify-finished: scheduled ws=%s delay=%.2f"
                        ws agent-repl-notify-delay)))))

;;;; ---- The magit refresh ------------------------------------------------

(defun agent-repl--refresh-magit-status-for-dir (dir &optional ws)
  "Refresh any magit-status buffer whose `default-directory' canonicalizes to DIR.

REACTION (3) OF THE FINISH EDGE.  The turn ended, so the worktree the
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
