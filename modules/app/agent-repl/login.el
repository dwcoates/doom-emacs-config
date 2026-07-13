;;; login.el --- interactive Claude login for the gui frontend -*- lexical-binding: t; -*-

;; The gui frontend's topbar carries a login button.  This file is the
;; host end of that button.
;;
;; WHY EMACS RUNS THE LOGIN
;;
;; The Claude OAuth flow is an interactive TUI: it prints a URL, opens a
;; browser, and waits for a code to be pasted back.  It therefore needs a
;; controlling terminal.  Neither end of the gui frontend has one — the
;; browser obviously not, and the daemon speaks to its shims over pipes.
;; Emacs has vterm, so Emacs is the only place in the system where the
;; login CAN run, and the button is necessarily a REQUEST to Emacs rather
;; than something the webapp or daemon does itself.
;;
;; The path is: webapp button -> POST /sessions/{id}/login -> the daemon
;; writes a `login_request_<sid>' sentinel naming the session's cwd ->
;; sentinel.el's watcher dispatches it here.  The sentinel channel is used
;; because it is the daemon's ESTABLISHED reverse channel to Emacs (the
;; same one carrying permission requests and shim deaths for gui
;; sessions), and because it is file-notify driven rather than polled, so
;; a click opens a terminal immediately.
;;
;; WHICH ACCOUNT
;;
;; The login must target the account the workspace's agent actually runs
;; as, or it would cheerfully log into the wrong one and leave the real
;; problem in place.  That account is `agent-repl--compute-config-dir' of
;; the workspace's project dir — ~/.claude-chesscom under $MULTI_REPO_ROOT,
;; ~/.claude otherwise — the SAME resolver the vterm start command and the
;; gui session-create payload both use.  There is exactly one account
;; resolver in this module and all three callers share it.

;;; Code:

(require 'cl-lib)

(defvar agent-repl-startup-prefix)
(declare-function agent-repl--compute-config-dir "session" (project-dir))
(declare-function agent-repl--create-buffer "core" (ws &optional suffix))
(declare-function agent-repl--info "core" (ws fmt &rest args))
(declare-function agent-repl--log "core" (ws fmt &rest args))
(declare-function agent-repl--warn "core" (ws fmt &rest args))
(declare-function agent-repl--ws-dir "status" (ws))
(declare-function vterm-mode "vterm" ())
(declare-function vterm-send-return "vterm" ())
(declare-function vterm-send-string "vterm" (string &optional paste-p))

(defcustom agent-repl-login-command "claude /login"
  "Command run in the login terminal, sans the CLAUDE_CONFIG_DIR prefix.
`agent-repl--login-cmd' prepends the account's config dir, so this is
only the claude invocation itself.  Defaults to `claude /login', which
drops straight into the OAuth flow; set it to `claude' to land in a
plain session and drive the login by hand."
  :type 'string
  :group 'agent-repl)

(defun agent-repl--login-buffer-name (ws)
  "Return the name of WS's login terminal buffer."
  (format "*agent-login-%s*" ws))

(defun agent-repl--login-cmd (project-dir)
  "Return the shell command that logs in the account PROJECT-DIR resolves to.
Prepends CLAUDE_CONFIG_DIR exactly as `agent-repl--assemble-cmd' does for
the vterm start command, so the login lands on the same account the
workspace's agent runs as.  A nil config dir (the personal account, whose
root is the CLI's own default) yields the bare command with no prefix —
exporting an empty CLAUDE_CONFIG_DIR would name a config root literally
called \"\"."
  (let ((config-dir (agent-repl--compute-config-dir project-dir)))
    (if config-dir
        (format "CLAUDE_CONFIG_DIR=%s %s"
                (shell-quote-argument config-dir)
                agent-repl-login-command)
      agent-repl-login-command)))

(defun agent-repl--login-open-terminal (ws project-dir)
  "Open WS's login terminal for the account PROJECT-DIR resolves to.
Reuses WS's existing login buffer when one is already live: a second
click (or a second sentinel for the same click) must surface the terminal
already waiting for a paste, never race a second OAuth flow against it.
Returns the buffer.

The terminal runs from `temporary-file-directory', NOT from PROJECT-DIR.
The login CLI fires the global Claude Code hooks like any other, and
those hooks are keyed on CWD — running it inside the workspace would
attribute its `session_start'/`stop_'/`prompt_submit_' sentinels to the
workspace and walk that workspace's tab through agent states belonging to
a login prompt.  The established dodge for exactly this is to spawn from
the temp dir, where the sentinel dispatcher's non-git branch drops the
files (see the headless `claude -p' calls in prompt-summary.el and
worktree.el).  Nothing is lost: PROJECT-DIR selects the ACCOUNT, and the
account is already baked into the command's CLAUDE_CONFIG_DIR — the login
itself is account-scoped, never project-scoped."
  (let* ((name (agent-repl--login-buffer-name ws))
         (existing (get-buffer name)))
    (if (and existing (buffer-live-p existing) (get-buffer-process existing))
        (progn
          (agent-repl--log ws "login-open-terminal: reusing live login terminal for ws=%s" ws)
          (pop-to-buffer existing)
          existing)
      (when existing (kill-buffer existing))
      (let* ((default-directory temporary-file-directory)
             (cmd (agent-repl--login-cmd project-dir))
             (buf (agent-repl--create-buffer ws "-login")))
        ;; `agent-repl--create-buffer' names by role suffix; the login
        ;; terminal is a fourth role alongside vterm/input, so rename it
        ;; onto the login name the reuse check above looks for.
        (with-current-buffer buf
          (rename-buffer name t)
          (vterm-mode)
          (agent-repl--log ws "login-open-terminal: ws=%s account-from=%s run-in=%s cmd=%s"
                           ws project-dir default-directory cmd)
          (vterm-send-string (concat agent-repl-startup-prefix cmd))
          (vterm-send-return))
        (pop-to-buffer buf)
        (agent-repl--info ws "Claude login opened for ws=%s (%s)" ws cmd)
        buf))))

(defun agent-repl--on-login-request-event (ws dir)
  "Open the interactive Claude login for workspace WS rooted at DIR.
Sentinel callback for `login_request' files, which the daemon writes when
the gui topbar's login button is clicked.

DIR is the session's cwd as the daemon knows it.  The workspace's own
registered `:project-dir' is preferred when it has one, since that is the
value every other account decision in this module is made from; DIR is
the fallback for a workspace with no registration (a plain project
perspective).  A login opened against the wrong root would resolve the
wrong account, which is the exact failure this button exists to fix."
  (let ((project-dir (or (ignore-errors (agent-repl--ws-dir ws)) dir)))
    (agent-repl--log ws "on-login-request-event: ws=%s sentinel-dir=%s project-dir=%s"
                     ws dir project-dir)
    (agent-repl--login-open-terminal ws project-dir)))

(provide 'agent-repl-login)
;;; login.el ends here
