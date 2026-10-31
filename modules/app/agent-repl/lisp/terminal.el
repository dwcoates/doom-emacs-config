;;; terminal.el --- run a shell code block in the terminal popup -*- lexical-binding: t; -*-

;;; Commentary:

;; A SHELL CODE BLOCK'S RUN CONTROL RUNS ITS COMMAND IN THE TERMINAL POPUP
;; (owner request, 2026-10-30).  The webview relays the click through the
;; daemon as the `run_in_terminal' push arm of `agentrepl.v1'
;; WatchHostWorkspace (`OpenInEditorTerminalCommand {command}'), and
;; `agent-repl-terminal-run' answers it exactly as the person would by hand:
;;
;;   1. open the terminal popup `SPC o t' toggles (`+vterm/toggle'), at the
;;      workspace's project root, unless it is already shown;
;;   2. paste the command into it and press return;
;;   3. close the popup once the command exits, as a second `SPC o t' would.
;;
;; THE SHELL SAYS WHEN THE COMMAND EXITED, NOT A TIMER OR A PROMPT GUESS.  The
;; command is sent wrapped in a group followed by a `printf' of vterm's
;; elisp-evaluation escape (OSC 51;E), which names
;; `agent-repl-terminal-finished' with the run's token and the group's exit
;; status.  vterm evaluates only functions listed in `vterm-eval-cmds', so
;; this file adds its own one entry there.
;;
;; ONE RUN PER TERMINAL BUFFER IS AWAITED.  A second click while a run is
;; outstanding replaces it, because an interrupted command (C-c) never prints
;; its finish and must not leave the terminal unclosable; the stale token's
;; finish, should it ever arrive, closes nothing and is recorded.
;;
;; THE POPUP CLOSES WHATEVER THE EXIT STATUS, as asked.  A non-zero status is
;; echoed in the minibuffer and recorded, and the output stays in the
;; terminal's buffer for the next `SPC o t' to show.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function agent-repl--error "core")
(declare-function agent-repl--info "core")
(declare-function agent-repl--ws-dir "status")
(declare-function agent-repl--ws-get "workspace")
(declare-function +vterm/toggle "autoload" (arg))
(declare-function vterm-send-string "vterm" (string &optional paste-p))
(declare-function vterm-send-return "vterm")

(defvar vterm-eval-cmds)

(defconst agent-repl-terminal-finished-command "agent-repl-terminal-finished"
  "The name the shell's finish escape calls, as `vterm-eval-cmds' lists it.")

(defvar agent-repl-terminal-toggle-function #'+vterm/toggle
  "The function `SPC o t' calls: toggles the terminal popup, answering its buffer.")

(defvar agent-repl-terminal-send-function #'agent-repl-terminal--send-to-vterm
  "The function that types a wrapped command into a terminal buffer and runs it.")

(defvar agent-repl-terminal--runs (make-hash-table :test 'eq)
  "Each terminal buffer's awaited run: a plist (:ws :token).")

(defvar agent-repl-terminal--next-token 0
  "The counter every run's token is minted from.")

(defun agent-repl-terminal-wrap (command token)
  "COMMAND as the shell text that runs it whole, then reports TOKEN's finish.
The command runs inside a `{ ... }' group, so a multi-line command, one
ending in a comment, or one ending in `&' cannot swallow the report; the
report is vterm's OSC 51;E escape calling
`agent-repl-terminal-finished-command' with TOKEN and the group's status."
  (format "{ %s\n}; printf '\\033]51;E%s \"%%s\" \"%%s\"\\033\\\\' %s \"$?\""
          (string-trim-right command)
          agent-repl-terminal-finished-command
          token))

(defun agent-repl-terminal--send-to-vterm (buffer text)
  "Paste TEXT into vterm BUFFER as one bracketed paste, then press return."
  (with-current-buffer buffer
    (vterm-send-string text t)
    (vterm-send-return)))

(defun agent-repl-terminal--show (dir)
  "Show the terminal popup for project DIR as `SPC o t' does; answer its buffer.
A popup already shown stays shown: toggling it would close it."
  (let* ((default-directory dir)
         (buffer (funcall agent-repl-terminal-toggle-function nil)))
    (unless (and (buffer-live-p buffer) (get-buffer-window buffer))
      (setq buffer (funcall agent-repl-terminal-toggle-function nil)))
    (unless (and (buffer-live-p buffer) (get-buffer-window buffer))
      (error "The terminal popup toggle showed no terminal (buffer %S)" buffer))
    buffer))

(defun agent-repl-terminal-run (ws command)
  "Run COMMAND for workspace WS in the terminal popup, closing it on exit.
WS's worktree is the project root a newly opened terminal starts in."
  (let* ((dir (agent-repl--ws-dir ws))
         (buffer (condition-case err
                     (agent-repl-terminal--show (file-name-as-directory dir))
                   (error
                    (agent-repl--error ws "elisp.terminal.show-failed ws=%s dir=%S cause=%S"
                                       ws dir err)
                    nil))))
    (when buffer
      (let ((token (format "%d" (cl-incf agent-repl-terminal--next-token)))
            (replaced (gethash buffer agent-repl-terminal--runs)))
        (puthash buffer (list :ws ws :token token) agent-repl-terminal--runs)
        (with-current-buffer buffer
          (add-hook 'kill-buffer-hook #'agent-repl-terminal--forget nil t))
        (agent-repl--info ws "elisp.terminal.run ws=%s buffer=%S token=%s characters=%d replaced=%s"
                          ws (buffer-name buffer) token (length command)
                          (plist-get replaced :token))
        (funcall agent-repl-terminal-send-function buffer
                 (agent-repl-terminal-wrap command token))))))

(defun agent-repl-terminal--forget ()
  "Forget the current terminal buffer's awaited run: the buffer is dying."
  (remhash (current-buffer) agent-repl-terminal--runs))

(defun agent-repl-terminal-finished (token status)
  "Close the terminal popup whose awaited run is TOKEN; it exited with STATUS.
Called by vterm from the shell's finish escape, with the terminal's buffer
current, so TOKEN and STATUS arrive as strings."
  (let* ((buffer (current-buffer))
         (run (gethash buffer agent-repl-terminal--runs))
         (ws (plist-get run :ws)))
    (if (not (equal (plist-get run :token) token))
        (agent-repl--info ws "elisp.terminal.stale-finish buffer=%S token=%s awaited=%s status=%s"
                          (buffer-name buffer) token (plist-get run :token) status)
      (remhash buffer agent-repl-terminal--runs)
      (agent-repl--info ws "elisp.terminal.finished ws=%s buffer=%S token=%s status=%s"
                        ws (buffer-name buffer) token status)
      (unless (equal status "0")
        (message "agent-repl: the command exited with status %s; SPC o t shows its output" status))
      (when-let* ((window (get-buffer-window buffer)))
        (delete-window window)))))

(defun agent-repl-terminal-install ()
  "Let the shell's finish escape call `agent-repl-terminal-finished'."
  (add-to-list 'vterm-eval-cmds
               (list agent-repl-terminal-finished-command #'agent-repl-terminal-finished)))

(with-eval-after-load 'vterm
  (agent-repl-terminal-install))

(provide 'agent-repl-terminal)

;;; terminal.el ends here
