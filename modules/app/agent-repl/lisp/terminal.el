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
;;   3. close the popup once the command exits, as a second `SPC o t' would;
;;   4. send the command's outcome to the workspace's agent (owner request,
;;      2026-11-08): the output itself when it is under
;;      `agent-repl-terminal-inline-word-limit' words, else the path of a
;;      unique temp file holding it, with its size and the exit status.  Success
;;      and failure alike are sent; a long successful output is sent as a path
;;      the agent is told it need not read.
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
;; THE OUTPUT IS TEED TO A FILE, NOT SCRAPED FROM THE SCREEN.  The group's
;; output goes through `tee' into a temp file Emacs created, so the capture is
;; complete and exact however the terminal redraws; the pipeline's end is the
;; point the finish escape prints, so the file is whole by then.  The group's
;; exit status travels through a second temp file because a pipeline's own
;; status is the last stage's.  Output piped this way is not a tty to the
;; command; the person still sees it stream, as `tee' prints it.
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
(declare-function agent-repl--send "input")
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

(defconst agent-repl-terminal-inline-word-limit 500
  "Output of fewer words than this reaches the agent inline; more goes by path.")

(defvar agent-repl-terminal--runs (make-hash-table :test 'eq)
  "Each terminal buffer's awaited run: a plist (:ws :token :command :output :status).")

(defvar agent-repl-terminal--next-token 0
  "The counter every run's token is minted from.")

(defun agent-repl-terminal-wrap (command token output-file status-file)
  "COMMAND as the shell text that runs it whole, then reports TOKEN's finish.
The command runs inside a `( ... )' subshell, so an `exit' in it ends only
the command, and a multi-line command, one ending in a comment, or one ending
in `&' cannot swallow the report.  The
group's output, both streams, is teed into OUTPUT-FILE and its exit status
written to STATUS-FILE; the report is vterm's OSC 51;E escape calling
`agent-repl-terminal-finished-command' with TOKEN and that status, printed
once the pipeline, and so the tee, has ended."
  (format "{ ( %s\n); echo $? >%s; } 2>&1 | tee %s; printf '\\033]51;E%s \"%%s\" \"%%s\"\\033\\\\' %s \"$(cat %s)\""
          (string-trim-right command)
          (shell-quote-argument status-file)
          (shell-quote-argument output-file)
          agent-repl-terminal-finished-command
          token
          (shell-quote-argument status-file)))

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
      (let* ((token (format "%d" (cl-incf agent-repl-terminal--next-token)))
             (replaced (gethash buffer agent-repl-terminal--runs))
             (output (make-temp-file "agent-repl-terminal-" nil ".out"))
             (status (make-temp-file "agent-repl-terminal-" nil ".status")))
        (when replaced (agent-repl-terminal--discard replaced))
        (puthash buffer (list :ws ws :token token :command command
                              :output output :status status)
                 agent-repl-terminal--runs)
        (with-current-buffer buffer
          (add-hook 'kill-buffer-hook #'agent-repl-terminal--forget nil t))
        (agent-repl--info ws "elisp.terminal.run ws=%s buffer=%S token=%s characters=%d replaced=%s"
                          ws (buffer-name buffer) token (length command)
                          (plist-get replaced :token))
        (funcall agent-repl-terminal-send-function buffer
                 (agent-repl-terminal-wrap command token output status))))))

(defun agent-repl-terminal--discard (run &rest keep)
  "Delete RUN's temp files, except the ones named by the plist keys in KEEP."
  (dolist (key '(:output :status))
    (unless (memq key keep)
      (delete-file (plist-get run key)))))

(defun agent-repl-terminal--forget ()
  "Forget the current terminal buffer's awaited run: the buffer is dying."
  (when-let* ((run (gethash (current-buffer) agent-repl-terminal--runs)))
    (agent-repl-terminal--discard run))
  (remhash (current-buffer) agent-repl-terminal--runs))

(defun agent-repl-terminal--word-count (text)
  "The number of whitespace-separated words in TEXT."
  (length (split-string text)))

(defun agent-repl-terminal--read (file)
  "FILE's contents, ANSI escape sequences stripped, trailing newlines trimmed."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (while (re-search-forward "\e\\[[0-9;?]*[A-Za-z]" nil t)
      (replace-match ""))
    (string-trim-right (buffer-string) "\n+")))

(defun agent-repl-terminal-report (command status output-file)
  "The agent-facing report of COMMAND, which exited STATUS (a string).
OUTPUT-FILE holds its output.  Output under
`agent-repl-terminal-inline-word-limit' words is inlined; anything longer is
referred to by OUTPUT-FILE's path, its size and word count.  Answers
a cons (REPORT . INLINED): INLINED is non-nil when OUTPUT-FILE may be deleted."
  (let* ((output (agent-repl-terminal--read output-file))
         (words (agent-repl-terminal--word-count output))
         (success (equal status "0"))
         (head (format "I ran this shell command from your response in the terminal. It %s (exit status %s).\n\nCommand:\n```sh\n%s\n```\n\n"
                       (if success "succeeded" "failed") status
                       (string-trim-right command))))
    (if (< words agent-repl-terminal-inline-word-limit)
        (cons (concat head (if (string-empty-p output)
                               "It printed no output."
                             (format "Output:\n```\n%s\n```" output)))
              t)
      (cons (concat head
                    (format "Its output is large: %d words, %d bytes, saved to %s\n\n%s"
                            words (file-attribute-size (file-attributes output-file))
                            output-file
                            (if success
                                "The command succeeded, so you do not need to read the file."
                              "The command failed; read the file to find out why.")))
            nil))))

(defun agent-repl-terminal-finished (token status)
  "Close the terminal popup whose awaited run is TOKEN; it exited with STATUS.
Called by vterm from the shell's finish escape, with the terminal's buffer
current, so TOKEN and STATUS arrive as strings.  The run's outcome is then
sent to its workspace's agent, whatever STATUS."
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
        (delete-window window))
      (agent-repl-terminal--deliver run status))))

(defun agent-repl-terminal--deliver (run status)
  "Send RUN's outcome, exited with STATUS, to its workspace's agent.
A report that cannot be built is an error in the log and nothing is sent."
  (let ((ws (plist-get run :ws))
        (output-file (plist-get run :output))
        (inlined nil))
    (condition-case err
        (let ((report (agent-repl-terminal-report (plist-get run :command) status output-file)))
          (setq inlined (cdr report))
          (agent-repl--info ws "elisp.terminal.deliver ws=%s token=%s status=%s inline=%s output=%s"
                            ws (plist-get run :token) status inlined output-file)
          (agent-repl--send :command-terminal-output (car report) ws))
      (error
       (agent-repl--error ws "elisp.terminal.deliver-failed ws=%s token=%s status=%s output=%s cause=%S"
                          ws (plist-get run :token) status output-file err)))
    (agent-repl-terminal--discard run (unless inlined :output))))

(defun agent-repl-terminal-install ()
  "Let the shell's finish escape call `agent-repl-terminal-finished'."
  (add-to-list 'vterm-eval-cmds
               (list agent-repl-terminal-finished-command #'agent-repl-terminal-finished)))

(with-eval-after-load 'vterm
  (agent-repl-terminal-install))

(provide 'agent-repl-terminal)

;;; terminal.el ends here
