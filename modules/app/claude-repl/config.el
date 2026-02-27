;;; config.el --- Claude REPL for Doom Emacs -*- lexical-binding: t; -*-

;; Author: Dodge Coates
;; URL: https://github.com/dodgecoates
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; A Claude Code REPL integration for Doom Emacs with workspace-aware
;; session management, input history, and status indicators.

;;; Code:

;; Cancel all previously registered timers on re-eval so we don't accumulate.
(defvar claude-repl--timers nil
  "List of active timers created by claude-repl.
Cancelled and reset whenever this file is re-evaluated.")

(dolist (timer claude-repl--timers)
  (when (timerp timer)
    (cancel-timer timer)))
(setq claude-repl--timers nil)

(defun claude-repl-debug/cancel-timers ()
  "Cancel all claude-repl timers."
  (interactive)
  (dolist (timer claude-repl--timers)
    (when (timerp timer)
      (cancel-timer timer)))
  (setq claude-repl--timers nil)
  (message "Cancelled all claude-repl timers."))

(defgroup claude-repl nil
  "Claude Code REPL integration for Doom Emacs."
  :group 'tools
  :prefix "claude-repl-")

;; Workspace identity
(defvar-local claude-repl--project-root nil
  "Buffer-local git root for Claude REPL buffers.
Set when vterm/input buffers are created so workspace-id works from them.")

(defun claude-repl--git-root (&optional dir)
  "Find the git root by walking up from DIR (default `default-directory').
Checks for both .git directory and .git file (worktrees)."
  (let ((dir (or dir default-directory)))
    (locate-dominating-file dir
      (lambda (d)
        (let ((git (expand-file-name ".git" d)))
          (or (file-directory-p git) (file-regular-p git)))))))

(defun claude-repl--workspace-id ()
  "Return a short identifier for the current git workspace.
Uses an MD5 hash of the git root path.  Falls back to the buffer-local
`claude-repl--project-root' and then `default-directory'."
  (let ((root (claude-repl--resolve-root)))
    (when root
      (substring (md5 root) 0 8))))

;; Single hash table for all per-workspace state.
(defvar claude-repl--workspaces (make-hash-table :test 'equal)
  "Hash table mapping workspace name → state plist.
Keys: :vterm-buffer :input-buffer :saved-window-config
      :return-window :prefix-counter :status :activity-time")

(defun claude-repl--ws-get (ws key)
  "Get KEY from workspace WS's plist."
  (plist-get (gethash ws claude-repl--workspaces) key))

(defun claude-repl--ws-put (ws key val)
  "Set KEY to VAL in workspace WS's plist in `claude-repl--workspaces'.
Internally uses plist-put (which returns a new list) threaded into puthash."
  (puthash ws (plist-put (gethash ws claude-repl--workspaces) key val)
           claude-repl--workspaces))

(defun claude-repl--ws-del (ws)
  "Remove all state for workspace WS."
  (remhash ws claude-repl--workspaces))

(defvar claude-repl--fullscreen-config nil
  "Saved window configuration before fullscreen toggle.")

(defvar claude-repl--hide-overlay-refcount 0
  "Reference count for the hide overlay advice.
Only add advice when going from 0 to 1; only remove when going from 1 to 0.")

(defvar-local claude-repl-hide-overlay nil
  "Overlay used to hide Claude CLI input box.")

(defvar-local claude-repl--owning-workspace nil
  "Workspace name that owns this claude session.
Set when the user sends a message; used to correctly target workspace
state changes regardless of which persp the buffer drifts into.")

;; Input history
(defvar-local claude-repl--input-history nil
  "List of previous inputs, most recent first.")

(defvar-local claude-repl--history-index -1
  "Current position in history. -1 means not browsing.")

(defvar-local claude-repl--history-stash nil
  "Stashed in-progress text saved when history browsing begins.")

(defvar-local claude-repl--history-navigating nil
  "Non-nil while history navigation is replacing buffer text.")

(defcustom claude-repl-hide-input-box nil
  "When non-nil, hide the Claude CLI input box in the vterm buffer."
  :type 'boolean
  :group 'claude-repl)

(defcustom claude-repl-debug nil
  "When non-nil, emit debug messages to *Messages*."
  :type 'boolean
  :group 'claude-repl)

(defun claude-repl--log (fmt &rest args)
  "Log a timestamped debug message when `claude-repl-debug' is non-nil.
FMT and ARGS are passed to `message', prefixed with timestamp and [claude-repl]."
  (when claude-repl-debug
    (apply #'message (concat (format-time-string "%H:%M:%S.%3N") " [claude-repl] " fmt) args)))

(defcustom claude-repl-skip-permissions t
  "When non-nil, start Claude with --dangerously-skip-permissions and prepend the command prefix metaprompt."
  :type 'boolean
  :group 'claude-repl)

(defcustom claude-repl-prefix-period 7
  "Number of prompts between metaprompt prefix injections.
The prefix is sent on the first prompt and every Nth prompt thereafter."
  :type 'integer
  :group 'claude-repl)


(defcustom claude-repl-send-postfix "\n what do you think? spit it back to me."
  "String appended to input when sending via `claude-repl-send-with-postfix'."
  :type 'string
  :group 'claude-repl)

(defvar claude-repl-paste-delay 0.1
  "Seconds to wait after pasting before sending Return.
Used by `claude-repl--send-input-to-vterm' for large inputs.")

(defcustom claude-repl-command-prefix "DO NOT run any mutating git commands (push, reset, checkout, etc) without EXPLICIT PERMISSION from ME. Do not INSTALL or UNINSTALL anything without my EXPLICIT PERMISSION. Do not operate on any files OUTSIDE OF PROJECT without MY EXPLICIT PERMISSION. Do not take any actions unless it FOLLOWS DIRECTLY from an action EXPLICITLY REQUESTED in the following prompt "
  "When non-nil, this string is prepended (with a newline) before every input sent to Claude."
  :type 'string
  :group 'claude-repl)

(defvar claude-repl--command-prefix (format "<<*this is a metaprompt. I will periodically prefix my prompts with this to remind you of our restrictions for freely making changes. Do not be alarmed, this is merely a periodic reminder*: %s *metaprompt over* (rest is actual user request that you should respond to directly)>>\n\n" claude-repl-command-prefix)
  "Formatted metaprompt string prepended before every input when `claude-repl-skip-permissions' is non-nil.")


;; Set to t once Claude has set its terminal title (meaning it's ready).
(defvar-local claude-repl--ready nil
  "Non-nil once Claude Code has finished starting up.")

(defun claude-repl--buffer-name (&optional suffix)
  "Return a project-specific buffer name like *claude-HASH* or *claude-input-HASH*.
SUFFIX, if provided, is inserted before the hash (e.g. \"-input\")."
  (let ((id (claude-repl--workspace-id)))
    (format "*claude%s-%s*" (or suffix "") (or id "default"))))


;; Override vterm--get-color so claude vterm buffers get a black background.
;; Solaire-mode advises this function and face-background ignores buffer-local
;; remaps, so we hardcode black for claude buffers' default background case.
(after! vterm
  (advice-add 'vterm--get-color :around
              (lambda (fn index &rest args)
                (let ((result (apply fn index args)))
                  (if (and (not (member :foreground args))
                           (= index -1)
                           (not (member :inverse-video args))
                           (claude-repl--claude-buffer-p))
                      (claude-repl--grey 15)
                    result)))))

(defun claude-repl--live-return-window ()
  "Return the return-window for the current workspace if live, else `selected-window'."
  (let ((win (claude-repl--ws-get (+workspace-current-name) :return-window)))
    (if (and win (window-live-p win)) win (selected-window))))

;; Manual window layout: vterm on the right (full height), input below vterm.
(defun claude-repl--show-panels ()
  "Display vterm and input panels to the right of the current window.
Splits right for vterm (60% width to work window), then splits vterm bottom for input (15%)."
  (let* ((ws (+workspace-current-name))
         (vterm-buf (claude-repl--ws-get ws :vterm-buffer))
         (input-buf (claude-repl--ws-get ws :input-buffer)))
    (claude-repl--log "show-panels vterm=%s input=%s"
                      (and vterm-buf (buffer-name vterm-buf))
                      (and input-buf (buffer-name input-buf)))
    (let* ((work-win (claude-repl--live-return-window))
           (vterm-win (split-window work-win (round (* 0.6 (window-total-width work-win))) 'right))
           (input-win (split-window vterm-win (round (* -0.15 (window-total-height vterm-win))) 'below)))
      (claude-repl--refresh-vterm)
      (set-window-buffer vterm-win vterm-buf)
      (set-window-buffer input-win input-buf)
      (set-window-dedicated-p vterm-win t)
      (set-window-dedicated-p input-win t)
      ;; Hide from other-window, display-buffer, etc. so magit and friends
      ;; never try to reuse these windows.
      (set-window-parameter vterm-win 'no-other-window t)
      (set-window-parameter input-win 'no-other-window t)
      ;; Lock width to prevent resize-triggered reflow in vterm
      (set-window-parameter vterm-win 'window-size-fixed 'width)))
  (claude-repl--update-all-workspace-states))

(defun claude-repl--focus-input-panel ()
  "Focus the input panel window and enter insert state."
  (claude-repl--log "focus-input-panel")
  (when-let ((buf (claude-repl--ws-get (+workspace-current-name) :input-buffer))
             (win (get-buffer-window buf)))
    (select-window win)
    (evil-insert-state)))

(defun claude-repl--grey (n)
  "Return a hex color string for greyscale value N (0=black, 255=white)."
  (format "#%02x%02x%02x" n n n))

(defun claude-repl--claude-buffer-p (&optional buf)
  "Return non-nil if BUF (default: current buffer) is a Claude vterm buffer."
  (string-match-p "^\\*claude-[0-9a-f]+\\*$"
                  (buffer-name (or buf (current-buffer)))))

(defun claude-repl--resolve-root ()
  "Return the project root directory.
Tries git root, then buffer-local project root, then `default-directory'."
  (or (claude-repl--git-root)
      claude-repl--project-root
      default-directory))

(defun claude-repl--vterm-live-p ()
  "Return non-nil if the Claude vterm buffer for the current workspace exists and is live."
  (let ((buf (claude-repl--ws-get (+workspace-current-name) :vterm-buffer)))
    (and buf (buffer-live-p buf))))

(defun claude-repl--set-buffer-background (grey-level)
  "Set default and fringe background to greyscale GREY-LEVEL in current buffer."
  (face-remap-add-relative 'default :background (claude-repl--grey grey-level))
  (face-remap-add-relative 'fringe :background (claude-repl--grey grey-level)))

(defun claude-repl--start-claude ()
  "Send the claude startup command to the current vterm buffer."
  (claude-repl--log "start-claude skip-permissions=%s" claude-repl-skip-permissions)
  (vterm-send-string (concat "clear && claude -c"
                             (when claude-repl-skip-permissions
                               " --dangerously-skip-permissions")))
  (vterm-send-return))

;; Instructions bar face
(defface claude-repl-header-line
  '((t :background "white" :foreground "black" :weight bold))
  "Face for the Claude Input header line.")

;; Input mode
(define-derived-mode claude-input-mode fundamental-mode "Claude Input"
  "Major mode for Claude REPL input buffer."
  (setq-local header-line-format
              "RET: send | C-RET: send+postfix | C-c C-c: clear+save | C-c C-k: interrupt | <up>/<down>: history")
  (face-remap-add-relative 'header-line 'claude-repl-header-line)
  (claude-repl--set-buffer-background 37)
  (visual-line-mode 1)
  (add-hook 'after-change-functions #'claude-repl--history-on-change nil t))

(defun claude-repl-discard-input ()
  "Save current input to history, clear the buffer, and enter insert state."
  (interactive)
  (claude-repl--log "discard-input")
  (claude-repl--history-push)
  (claude-repl--history-reset)
  (erase-buffer)
  (evil-insert-state))

(defun claude-repl-scroll-down ()
  "Scroll the Claude vterm buffer down."
  (interactive)
  (claude-repl--log "scroll-down")
  (when (claude-repl--vterm-live-p)
    (with-current-buffer (claude-repl--ws-get (+workspace-current-name) :vterm-buffer)
      (vterm-send-down))))

(defun claude-repl-scroll-up ()
  "Scroll the Claude vterm buffer up."
  (interactive)
  (claude-repl--log "scroll-up")
  (when (claude-repl--vterm-live-p)
    (with-current-buffer (claude-repl--ws-get (+workspace-current-name) :vterm-buffer)
      (vterm-send-up))))

(map! :map claude-input-mode-map
      :ni "RET"       #'claude-repl-send
      :ni "S-RET"     #'newline
      :ni "C-RET"     #'claude-repl-send-with-postfix
      :ni "C-c C-k"   #'claude-repl-interrupt
      :ni "C-c C-c"   #'claude-repl-discard-input
      :ni "C-c y"     (cmd! (claude-repl-send-char "y"))
      :ni "C-c n"     (cmd! (claude-repl-send-char "n"))
      :ni "C-c r"     #'claude-repl-restart
      :ni "C-c q"     #'claude-repl-kill
      :ni "C-S-m"     #'claude-repl-cycle
      :ni "C-h"       #'evil-window-left
      :n  "C-n"       #'claude-repl-scroll-down
      :n  "C-p"       #'claude-repl-scroll-up
      :ni "C-v"       #'claude-repl-paste-to-vterm
      :ni "<up>"        #'claude-repl--history-prev
      :ni "<down>"      #'claude-repl--history-next)

;; C-S-0 through C-S-9: send digit to Claude
(dotimes (i 10)
  (let ((char (number-to-string i)))
    (define-key claude-input-mode-map (kbd (format "C-S-%s" char))
      (lambda () (interactive) (claude-repl-send-char char)))))

;; Input history persistence
(defun claude-repl--history-file ()
  "Return the path to the history file for the current project."
  (expand-file-name ".claude-repl-history" (claude-repl--resolve-root)))

(defun claude-repl--history-save (&optional ws)
  "Write input history to disk.
Resolves the project root from the input buffer's local variable
rather than `default-directory' of whatever buffer is current.
WS defaults to the current workspace name."
  (claude-repl--log "history-save")
  (let* ((ws (or ws (+workspace-current-name)))
         (buf (and ws (claude-repl--ws-get ws :input-buffer))))
    (when (and buf (buffer-live-p buf))
      (let* ((root (buffer-local-value 'claude-repl--project-root buf))
             (history (buffer-local-value 'claude-repl--input-history buf))
             (file (expand-file-name ".claude-repl-history" (or root default-directory))))
        (when history
          (with-temp-file file
            (prin1 history (current-buffer))))))))

(defun claude-repl--history-restore ()
  "Load input history from disk into the current buffer."
  (claude-repl--log "history-restore")
  (let ((file (claude-repl--history-file)))
    (when (file-exists-p file)
      (setq claude-repl--input-history
            (with-temp-buffer
              (insert-file-contents file)
              (read (current-buffer)))))))

;; Input history functions
(defun claude-repl--history-push ()
  "Save current input buffer text to history.
Skips empty strings and duplicates of the most recent entry."
  (let ((text (string-trim (buffer-string))))
    (unless (string-empty-p text)
      (unless (equal text (car claude-repl--input-history))
        (push text claude-repl--input-history)))))

(defun claude-repl--history-reset ()
  "Reset history browsing index to the default (not browsing) state."
  (setq claude-repl--history-index -1))

(defun claude-repl--history-prev ()
  "Navigate to the previous (older) history entry."
  (interactive)
  (claude-repl--log "history-prev index=%d" claude-repl--history-index)
  (when claude-repl--input-history
    (let ((next-index (1+ claude-repl--history-index)))
      (when (< next-index (length claude-repl--input-history))
        ;; Stash current text on first navigation
        (when (= claude-repl--history-index -1)
          (setq claude-repl--history-stash (buffer-string)))
        (setq claude-repl--history-index next-index)
        (let ((claude-repl--history-navigating t))
          (erase-buffer)
          (insert (nth claude-repl--history-index claude-repl--input-history)))))))

(defun claude-repl--history-next ()
  "Navigate to the next (newer) history entry, or restore stashed text."
  (interactive)
  (claude-repl--log "history-next index=%d" claude-repl--history-index)
  (when (>= claude-repl--history-index 0)
    (let ((next-index (1- claude-repl--history-index)))
      (let ((claude-repl--history-navigating t))
        (erase-buffer)
        (if (< next-index 0)
            ;; Past newest entry — restore stash
            (progn
              (setq claude-repl--history-index -1)
              (when claude-repl--history-stash
                (insert claude-repl--history-stash)))
          (setq claude-repl--history-index next-index)
          (insert (nth claude-repl--history-index claude-repl--input-history)))))))

(defun claude-repl--history-on-change (&rest _)
  "Reset history browsing when the user edits the buffer directly."
  (unless claude-repl--history-navigating
    (when (>= claude-repl--history-index 0)
      (claude-repl--log "history-on-change resetting from index=%d" claude-repl--history-index))
    (setq claude-repl--history-index -1)))

;; Core functions
(defvar claude-repl-metaprompt-exempt-strings
  '("/clear" "/usage" "/login" "/logout")
  "Inputs that should never have the metaprompt prepended.
Compared exactly against the trimmed input.")

(defun claude-repl--skip-metaprompt-p (raw)
  "Return non-nil if RAW input should never have the metaprompt prepended.
Matches `claude-repl-metaprompt-exempt-strings' and bare numerals,
ignoring trailing whitespace."
  (let ((trimmed (string-trim-right raw)))
    (or (member trimmed claude-repl-metaprompt-exempt-strings)
        (string-match-p "^[0-9]+$" trimmed))))

(defvar claude-repl-send-posthooks
  '(("^/clear$" . claude-repl--posthook-reset-prefix-counter))
  "Alist of (PATTERN . FUNCTION) posthooks run after input is sent.
PATTERN is a string or regexp matched against the raw input (trimmed).
FUNCTION is called with (WS RAW) where WS is the workspace name and RAW is the input.")

(defun claude-repl--posthook-reset-prefix-counter (ws _raw)
  "Reset the metaprompt prefix counter to zero for workspace WS."
  (claude-repl--ws-put ws :prefix-counter 0))

(defun claude-repl--run-send-posthooks (ws raw)
  "Run posthooks matching RAW input for workspace WS."
  (let ((trimmed (string-trim-right raw)))
    (dolist (hook claude-repl-send-posthooks)
      (when (string-match-p (car hook) trimmed)
        (claude-repl--log "posthook matched pattern=%s" (car hook))
        (funcall (cdr hook) ws raw)))))

(defun claude-repl--prepare-input (ws)
  "Read and return input from the input buffer for workspace WS.
Periodically prepends the metaprompt prefix when `claude-repl-skip-permissions' is on."
  (let* ((input-buf (claude-repl--ws-get ws :input-buffer))
         (counter (or (claude-repl--ws-get ws :prefix-counter) 0))
         (raw (with-current-buffer input-buf (buffer-string))))
    (claude-repl--log "prepare-input counter=%d period=%d" counter claude-repl-prefix-period)
    (if (and claude-repl-skip-permissions claude-repl-command-prefix
             (not (claude-repl--skip-metaprompt-p raw))
             (zerop (mod counter claude-repl-prefix-period)))
        (concat claude-repl--command-prefix raw)
      raw)))

(defun claude-repl--send-input-to-vterm (vterm-buf input)
  "Send INPUT string to VTERM-BUF.
Uses paste mode for large inputs to avoid truncation."
  (claude-repl--log "send-input-to-vterm len=%d mode=%s" (length input)
                    (if (> (length input) 200) "paste" "direct"))
  (with-current-buffer vterm-buf
    (if (> (length input) 200)
        (let ((buf (current-buffer)))
          (vterm-send-string input t)
          (run-at-time claude-repl-paste-delay nil
                       (lambda ()
                         (when (buffer-live-p buf)
                           (with-current-buffer buf
                             (vterm-send-return)
                             (claude-repl--refresh-vterm))))))
      (vterm-send-string input)
      (vterm-send-return)
      (claude-repl--refresh-vterm))))

(defun claude-repl--mark-ws-thinking (ws)
  "Mark workspace WS as thinking: set state and record activity."
  (claude-repl--log "mark-ws-thinking ws=%s" ws)
  (claude-repl--ws-set ws :thinking)
  (claude-repl--touch-activity ws))

(defun claude-repl--clear-input (ws)
  "Push current input to history, reset browsing, and clear the input buffer for workspace WS."
  (claude-repl--log "clear-input")
  (let ((input-buf (claude-repl--ws-get ws :input-buffer)))
    (when input-buf
      (with-current-buffer input-buf
        (claude-repl--history-push)
        (claude-repl--history-reset)
        (erase-buffer)))))

(defun claude-repl-send ()
  "Send input buffer contents to Claude, clear buffer, and return to previous window."
  (interactive)
  (let* ((ws (+workspace-current-name)))
    (unless ws (error "claude-repl-send: no active workspace"))
    (let* ((input-buf (claude-repl--ws-get ws :input-buffer))
           (vterm-buf (claude-repl--ws-get ws :vterm-buffer)))
      (when (and input-buf (claude-repl--vterm-live-p))
        (let* ((raw (with-current-buffer input-buf (buffer-string)))
               (input (claude-repl--prepare-input ws)))
        (claude-repl--log "send ws=%s len=%d" ws (length input))
        (claude-repl--ws-put ws :prefix-counter
                             (1+ (or (claude-repl--ws-get ws :prefix-counter) 0)))
        ;; Pin the owning workspace on the vterm buffer so title-change
        ;; clears the correct workspace even if the buffer drifts between persps.
        (when vterm-buf
          (with-current-buffer vterm-buf
            (setq-local claude-repl--owning-workspace ws)))
        (claude-repl--mark-ws-thinking ws)
        (claude-repl--send-input-to-vterm vterm-buf input)
        (claude-repl--clear-input ws)
        (claude-repl--run-send-posthooks ws raw)
        (let ((ret (claude-repl--ws-get ws :return-window)))
          (when (and ret (window-live-p ret))
            (select-window ret))))))))

(defun claude-repl--remember-return-window ()
  "Save the current window as the return target, unless we're in the input buffer."
  (let ((ws (+workspace-current-name)))
    (unless (eq (current-buffer) (claude-repl--ws-get ws :input-buffer))
      (claude-repl--log "remember-return-window %s" (selected-window))
      (claude-repl--ws-put ws :return-window (selected-window)))))

(defun claude-repl--restore-layout ()
  "Restore the window layout from before panels were shown.
Falls back to hiding panels and selecting the return window."
  (let* ((ws (+workspace-current-name))
         (saved (claude-repl--ws-get ws :saved-window-config)))
    (claude-repl--log "restore-layout saved-config=%s" (if saved "yes" "no"))
    (if saved
        (progn
          (set-window-configuration saved)
          (claude-repl--ws-put ws :saved-window-config nil))
      (claude-repl--hide-panels)
      (let ((ret (claude-repl--ws-get ws :return-window)))
        (when (and ret (window-live-p ret))
          (select-window ret))))))

(defun claude-repl-send-and-hide ()
  "Send input to Claude and hide both panels."
  (interactive)
  (claude-repl--log "send-and-hide")
  (claude-repl-send)
  (claude-repl--restore-layout))

(defun claude-repl-send-char (char)
  "Send a single character to Claude."
  (claude-repl--log "send-char %s" char)
  (when (claude-repl--vterm-live-p)
    (let ((vterm-buf (claude-repl--ws-get (+workspace-current-name) :vterm-buffer)))
      (with-current-buffer vterm-buf
        (vterm-send-string char)
        (vterm-send-return)))))

(defun claude-repl--ensure-session ()
  "Ensure a Claude session exists (vterm + input + overlay).
No-op if already running."
  (unless (claude-repl--vterm-running-p)
    (claude-repl--log "ensure-session: starting new session")
    (let ((ws (+workspace-current-name)))
      (unless ws (error "claude-repl--ensure-session: no active workspace"))
      (claude-repl--ensure-vterm-buffer ws)
      (claude-repl--ensure-input-buffer ws)
      (claude-repl--ws-put ws :prefix-counter 0))
    (claude-repl--enable-hide-overlay)))

(defun claude-repl--send-to-claude (text)
  "Send TEXT to Claude, starting it if needed."
  (claude-repl--log "send-to-claude len=%d" (length text))
  (let ((ws (+workspace-current-name)))
    (claude-repl--ensure-session)
    (let ((vterm-buf (claude-repl--ws-get ws :vterm-buffer)))
      (with-current-buffer vterm-buf
        (vterm-send-string text)
        (vterm-send-return)))))

(defun claude-repl--rel-path ()
  "Return the current file path relative to the project root."
  (let ((file (buffer-file-name)))
    (unless file
      (user-error "Buffer %s is not visiting a file" (buffer-name)))
    (file-relative-name file (claude-repl--resolve-root))))

(defun claude-repl-explain ()
  "Ask Claude to explain the selected region, current line, or current file.
With active region: sends file path and line range.
In magit hunk: sends file path and hunk line range.
Without region: sends file path and current line."
  (interactive)
  (let* ((msg
          (cond
           ;; Active region in any buffer
           ((use-region-p)
            (let ((rel (claude-repl--rel-path))
                  (start-line (line-number-at-pos (region-beginning)))
                  (end-line (line-number-at-pos (region-end))))
              (deactivate-mark)
              (format "please explain %s:%d-%d" rel start-line end-line)))
           ;; Magit hunk section
           ((and (derived-mode-p 'magit-diff-mode 'magit-status-mode
                                 'magit-revision-mode)
                 (magit-section-match 'hunk))
            (let* ((section (magit-current-section))
                   (file (magit-file-at-point))
                   (range (oref section to-range))
                   (start (car range))
                   (len (cadr range))
                   (end (+ start len -1))
                   (rel (file-relative-name
                         (expand-file-name file (magit-toplevel))
                         (claude-repl--resolve-root))))
              (format "please explain %s:%d-%d" rel start end)))
           ;; Default: current line
           (t
            (format "please explain %s:%d"
                    (claude-repl--rel-path) (line-number-at-pos (point)))))))
    (claude-repl--log "explain %s" msg)
    (claude-repl--send-to-claude msg)))

(defun claude-repl-interrupt ()
  "Send Escape to interrupt Claude."
  (interactive)
  (claude-repl--log "interrupt")
  (when (claude-repl--vterm-live-p)
    (let ((vterm-buf (claude-repl--ws-get (+workspace-current-name) :vterm-buffer)))
      (with-current-buffer vterm-buf
        (vterm-send-key "<escape>")
        (vterm-send-key "<escape>")))))

;; Git diff analysis commands
(defconst claude-repl--test-quality-prompt
  "please analyze tests to ensure they are following AAA standards for testing. They should be employing DRY principle for refactoring as well (extract repeated code into helpers, use builder pattern to facilitate test DSL). We should only be testing one thing per test (can extract tests into subtests to ensure this)")

(defconst claude-repl--test-coverage-prompt
  "please analyze test coverage in depth for recent changes: start by enumerating all edge cases introduced or modified, and THEN, afterwards, analyze in depth our current coverage to check for missing coverage of those edge cases.")

(defun claude-repl--send-diff-analysis (change-spec prompt)
  "Send a diff analysis request to Claude.
CHANGE-SPEC describes which changes (e.g. \"unstaged changes (git diff)\").
PROMPT is the analysis instruction."
  (let ((msg (format "for the %s, %s" change-spec prompt)))
    (claude-repl--log "diff-analysis: %s" change-spec)
    (claude-repl--send-to-claude msg)))

(defun claude-repl-test-quality-worktree ()
  "Analyze test quality for unstaged changes."
  (interactive)
  (claude-repl--send-diff-analysis "unstaged changes (git diff)" claude-repl--test-quality-prompt))

(defun claude-repl-test-quality-staged ()
  "Analyze test quality for staged changes."
  (interactive)
  (claude-repl--send-diff-analysis "staged changes (git diff --cached)" claude-repl--test-quality-prompt))

(defun claude-repl-test-quality-uncommitted ()
  "Analyze test quality for all uncommitted changes."
  (interactive)
  (claude-repl--send-diff-analysis "uncommitted changes (git diff HEAD)" claude-repl--test-quality-prompt))

(defun claude-repl-test-quality-head ()
  "Analyze test quality for the last commit."
  (interactive)
  (claude-repl--send-diff-analysis "last commit (git show HEAD)" claude-repl--test-quality-prompt))

(defun claude-repl-test-coverage-worktree ()
  "Analyze test coverage for unstaged changes."
  (interactive)
  (claude-repl--send-diff-analysis "unstaged changes (git diff)" claude-repl--test-coverage-prompt))

(defun claude-repl-test-coverage-staged ()
  "Analyze test coverage for staged changes."
  (interactive)
  (claude-repl--send-diff-analysis "staged changes (git diff --cached)" claude-repl--test-coverage-prompt))

(defun claude-repl-test-coverage-uncommitted ()
  "Analyze test coverage for all uncommitted changes."
  (interactive)
  (claude-repl--send-diff-analysis "uncommitted changes (git diff HEAD)" claude-repl--test-coverage-prompt))

(defun claude-repl-test-coverage-head ()
  "Analyze test coverage for the last commit."
  (interactive)
  (claude-repl--send-diff-analysis "last commit (git show HEAD)" claude-repl--test-coverage-prompt))

;; Hide overlay functions
(defun claude-repl--create-hide-overlay ()
  "Create a new hide overlay covering the bottom 4 lines of the current buffer."
  (when (and claude-repl-hide-input-box (> (point-max) 1))
    (let* ((end (point-max))
           (start (save-excursion
                    (goto-char end)
                    (forward-line -4)
                    (line-beginning-position))))
      (when (< start end)
        (setq claude-repl-hide-overlay (make-overlay start end nil t nil))
        (overlay-put claude-repl-hide-overlay 'display "")
        (overlay-put claude-repl-hide-overlay 'evaporate t)))))

(defun claude-repl--update-hide-overlay ()
  "Update overlay to hide bottom lines of Claude vterm buffer.
When called from within a claude buffer, updates it directly.
Otherwise looks up the vterm buffer for the current workspace."
  (let ((buf (if (claude-repl--claude-buffer-p)
                 (current-buffer)
               (let ((ws (+workspace-current-name)))
                 (and ws (claude-repl--ws-get ws :vterm-buffer))))))
    (when (and buf (buffer-live-p buf))
      (with-current-buffer buf
        (claude-repl--delete-hide-overlay)
        (claude-repl--create-hide-overlay)))))

(defun claude-repl-toggle-hide-input-box ()
  "Toggle hiding of Claude CLI's input box in the vterm buffer."
  (interactive)
  (claude-repl--log "toggle-hide-input-box -> %s" (not claude-repl-hide-input-box))
  (setq claude-repl-hide-input-box (not claude-repl-hide-input-box))
  (claude-repl--update-hide-overlay)
  (message "Claude input box hiding %s" (if claude-repl-hide-input-box "enabled" "disabled")))

;; Notifications
(defun claude-repl--notify-terminal-notifier (title message)
  "Send a desktop notification via terminal-notifier."
  (call-process "terminal-notifier" nil 0 nil
                "-title" title
                "-message" message))

(defun claude-repl--notify-osascript (title message)
  "Send a desktop notification via osascript."
  (start-process "claude-notify" nil
                 "osascript" "-e"
                 (format "display notification %S with title %S sound name \"default\""
                         message title)))

(defvar claude-repl--notify-fn
  (if (executable-find "terminal-notifier")
      #'claude-repl--notify-terminal-notifier
    #'claude-repl--notify-osascript)
  "Function used to send desktop notifications.")

(defun claude-repl--notify (title message)
  "Send a desktop notification with TITLE and MESSAGE."
  (claude-repl--log "notify title=%s msg=%s" title message)
  (funcall claude-repl--notify-fn title message))

;; Workspace tab indicator for Claude status

(defcustom claude-repl-stale-minutes 60
  "Minutes after last input before a workspace tab stops showing as stale."
  :type 'integer
  :group 'claude-repl)

(defun claude-repl--ws-state (ws)
  "Return the current status keyword for workspace WS.
Returns one of: :thinking, :done, :permission, :stale, or nil."
  (let ((plist (gethash ws claude-repl--workspaces)))
    (or (plist-get plist :status)
        (when-let ((t0 (plist-get plist :activity-time)))
          (when (< (- (float-time) t0) (* claude-repl-stale-minutes 60))
            :stale)))))

(defun claude-repl--ws-set (ws state)
  "Set workspace WS to STATE.
STATE is one of: :thinking, :done, :permission."
  (unless ws (error "claude-repl--ws-set: ws is nil"))
  (claude-repl--log "state %s -> %s" ws state)
  (claude-repl--ws-put ws :status state)
  (force-mode-line-update t))

(defun claude-repl--ws-clear (ws state)
  "Clear a single STATE for workspace WS.
STATE is one of: :thinking, :done, :permission, :stale."
  (unless ws (error "claude-repl--ws-clear: ws is nil"))
  (claude-repl--log "clear %s %s" ws state)
  (pcase state
    ((or :thinking :done :permission)
     (when (eq (claude-repl--ws-get ws :status) state)
       (claude-repl--ws-put ws :status nil)))
    (:stale (claude-repl--ws-put ws :activity-time nil)))
  (force-mode-line-update t))

(defun claude-repl--workspace-clean-p (ws)
  "Return non-nil if workspace WS has no unstaged changes to tracked files."
  (ignore-errors
    (let* ((persp (persp-get-by-name ws))
           (bufs (and persp (not (symbolp persp)) (persp-buffers persp)))
           (dir (cl-loop for buf in bufs
                         when (buffer-live-p buf)
                         return (buffer-local-value 'default-directory buf))))
      (when dir
        (let ((default-directory dir))
          (= 0 (process-file "git" nil nil nil "diff" "--quiet")))))))

(defun claude-repl--touch-activity (ws)
  "Record current time as last input for workspace WS.
Only sets stale if the workspace has no unstaged changes to tracked files."
  (unless ws (error "claude-repl--touch-activity: ws is nil"))
  (if (claude-repl--workspace-clean-p ws)
      (claude-repl--ws-put ws :activity-time (float-time))
    (claude-repl--ws-put ws :activity-time nil)))

(defun claude-repl--workspace-for-buffer (buf)
  "Return the workspace name that contains BUF, or nil."
  (when (bound-and-true-p persp-mode)
    (cl-loop for persp in (persp-persps)
             when (persp-contain-buffer-p buf persp)
             return (safe-persp-name persp))))

(defface claude-repl-tab-thinking
  '((t :background "#cc3333" :foreground "white" :weight bold))
  "Face for workspace tabs where Claude is thinking (red).")

(defface claude-repl-tab-done
  '((t :background "#1a7a1a" :foreground "black" :weight bold))
  "Face for workspace tabs where Claude is done (green).")

(defface claude-repl-tab-permission
  '((t :background "#1a7a1a" :foreground "black" :weight bold))
  "Face for workspace tabs where Claude needs permission (green + emoji).")

(defface claude-repl-tab-stale
  '((t :background "#cc8800" :foreground "black" :weight bold))
  "Face for workspace tabs you've worked in recently but aren't viewing (orange).")

(defun claude-repl--tabline-advice (&optional names)
  "Override for `+workspace--tabline' to color tabs by Claude status."
  (let* ((names (or names (+workspace-list-names)))
         (current-name (+workspace-current-name)))
    (mapconcat
     #'identity
     (cl-loop for name in names
              for i to (length names)
              collect
              (let* ((num (number-to-string (1+ i)))
                     (selected (equal current-name name))
                     (state (claude-repl--ws-state name))
                     (claude-face (pcase state
                                   ;; Permission: always visible (even when selected), green + ❓
                                   (:permission 'claude-repl-tab-permission)
                                   ;; Remaining states only on background tabs
                                   ((guard selected) nil)
                                   (:done       'claude-repl-tab-done)
                                   (:thinking   'claude-repl-tab-thinking)
                                   (:stale      'claude-repl-tab-stale)))
                     (label (pcase state
                              (:permission "❓")
                              (_ num)))
                     (base-face (if (and selected (not claude-face))
                                    '+workspace-tab-selected-face
                                  '+workspace-tab-face))
                     (face (or claude-face base-face)))
                (propertize (format " [%s] %s " label name) 'face face)))
     " ")))

(advice-add '+workspace--tabline :override #'claude-repl--tabline-advice)

;; Suppress the echo area flash when switching workspaces.
;; Doom calls (+workspace/display) after switch/cycle/new/load, which uses
;; (message ...) to show the tabline in the echo area.  Since tabs are
;; already visible at the top, the bottom flash is redundant.
(advice-add '+workspace/display :override #'ignore)

;; Periodically redraw invisible claude vterm buffers in :thinking workspaces.
;; This catches missed title transitions (e.g., Claude finished while we were
;; on a different workspace and vterm never redrawed).
(defun claude-repl--poll-thinking-workspaces ()
  "Redraw invisible claude vterm buffers whose workspace is :thinking."
  (dolist (buf (buffer-list))
    (when (and (buffer-live-p buf)
               (string-match-p "^\\*claude-[0-9a-f]+\\*$" (buffer-name buf))
               (not (get-buffer-window buf))  ;; invisible
               (with-current-buffer buf (eq major-mode 'vterm-mode)))
      (let ((ws (or (buffer-local-value 'claude-repl--owning-workspace buf)
                     (claude-repl--workspace-for-buffer buf))))
        (when (and ws (eq (claude-repl--ws-state ws) :thinking))
          (claude-repl--log "poll-thinking: redrawing %s (ws=%s)" (buffer-name buf) ws)
          (with-current-buffer buf
            (claude-repl--do-refresh)))))))

(push (run-with-timer 5 5 #'claude-repl--poll-thinking-workspaces)
      claude-repl--timers)

;; Periodically update all workspace states (catches git changes, etc.)
(push (run-with-timer 1 1 #'claude-repl--update-all-workspace-states)
      claude-repl--timers)

;; Title-based "Claude is done" detection.
;; Claude Code sets the terminal title to "<spinner> Claude Code" while thinking
;; and plain "Claude Code" when idle.  We poll via vterm--set-title advice.
(defvar-local claude-repl--title-thinking nil
  "Non-nil when the vterm title indicates Claude is thinking.
Buffer-local so multiple Claude sessions don't interfere.")

(defun claude-repl--title-has-spinner-p (title)
  "Return non-nil if TITLE contains a spinner (i.e. not the idle ✳ icon)."
  (and (> (length title) 0)
       (not (string-prefix-p "✳" title))
       (string-match-p "^[^[:ascii:]]" title)))

(defun claude-repl--detect-title-transition (title)
  "Classify a vterm TITLE change into a transition type.
Returns a plist with:
  :thinking    - non-nil if the new title has a spinner
  :transition  - one of 'started, 'finished, or nil (no change)
  :ws          - the owning workspace name for the current buffer, or nil"
  (let* ((thinking (claude-repl--title-has-spinner-p title))
         (ws (or claude-repl--owning-workspace
                 (claude-repl--workspace-for-buffer (current-buffer))))
         (transition (cond
                      ((and thinking (not claude-repl--title-thinking)) 'started)
                      ((and (not thinking) claude-repl--title-thinking) 'finished)
                      (t nil))))
    (list :thinking thinking :transition transition :ws ws)))

(defun claude-repl--swap-placeholder ()
  "Replace the loading placeholder window with the real vterm buffer.
Called once when Claude sets its first terminal title (meaning it's ready)."
  (claude-repl--log "swap-placeholder buf=%s" (buffer-name))
  (let ((buf (current-buffer))
        (placeholder (get-buffer " *claude-loading*")))
    (when placeholder
      (run-at-time 0 nil
                   (lambda ()
                     (when (buffer-live-p buf)
                       (dolist (win (get-buffer-window-list placeholder nil t))
                         (set-window-dedicated-p win nil)
                         (set-window-buffer win buf)
                         (set-window-dedicated-p win t))
                       (kill-buffer placeholder)))))))

(defun claude-repl--maybe-notify-finished (ws)
  "Send a desktop notification that Claude finished in WS, if frame is unfocused."
  (claude-repl--log "maybe-notify-finished ws=%s focused=%s" ws (if (frame-focus-state) "yes" "no"))
  (unless (frame-focus-state)
    (run-at-time 0.1 nil #'claude-repl--notify "Claude REPL"
                 (format "%s: Claude ready" ws))))

(defun claude-repl--on-claude-finished (ws)
  "Handle Claude finishing work in workspace WS.
Sets done state if buffer is hidden, refreshes display."
  (claude-repl--log "on-claude-finished ws=%s visible=%s focused=%s"
                    ws (if (get-buffer-window (current-buffer) t) "yes" "no")
                    (if (frame-focus-state) "yes" "no"))
  (unless (get-buffer-window (current-buffer) t)
    (claude-repl--ws-set ws :done))
  (claude-repl--refresh-vterm)
  (claude-repl--update-hide-overlay)
  (claude-repl--maybe-notify-finished ws)
  (unless (string= ws (+workspace-current-name))
    (message "Claude finished in workspace: %s" ws)
    (run-at-time 0.1 nil #'claude-repl--notify "Claude REPL"
                 (format "%s: Claude ready" ws))))

(defun claude-repl--handle-first-ready ()
  "Handle the first terminal title set — Claude is now ready.
Swaps the loading placeholder for the real vterm buffer."
  (unless claude-repl--ready
    (claude-repl--log "first-ready buf=%s" (buffer-name))
    (setq claude-repl--ready t)
    (claude-repl--swap-placeholder)))

(defun claude-repl--on-title-change (title)
  "Detect thinking->idle transition from vterm title changes."
  (when (claude-repl--claude-buffer-p)
    (claude-repl--handle-first-ready)
    (let* ((info (claude-repl--detect-title-transition title))
           (thinking (plist-get info :thinking))
           (transition (plist-get info :transition))
           (ws (plist-get info :ws)))
      ;; Update buffer-local thinking state before side effects so
      ;; re-entrant vterm--redraw calls see the current value and
      ;; detect no transition (prevents infinite recursion).
      (setq claude-repl--title-thinking thinking)
      (when transition
        (if (not ws)
            (claude-repl--log "on-title-change: ws nil for buffer %s, transition %s skipped"
                              (buffer-name) transition)
          (claude-repl--log "title transition=%s ws=%s" transition ws)
          (pcase transition
            ('started  (claude-repl--ws-set ws :thinking))
            ('finished (claude-repl--ws-clear ws :thinking)))
          (when (eq transition 'finished)
            (claude-repl--on-claude-finished ws))
          (claude-repl--update-all-workspace-states))))))


(after! vterm
  (advice-add 'vterm--set-title :before #'claude-repl--on-title-change))

;; Permission prompt detection via file-notify watcher.
;; A Claude Code Notification hook writes the CWD to a sentinel file;
;; we watch for it and set the permission state for the matching workspace.
(defun claude-repl--ws-for-dir (dir)
  "Return the workspace name for a Claude session rooted at DIR, or nil."
  (when-let* ((root (claude-repl--git-root dir))
              (hash (substring (md5 root) 0 8))
              (buf (get-buffer (format "*claude-%s*" hash))))
    (claude-repl--workspace-for-buffer buf)))

(defun claude-repl--on-permission-notify (event)
  "Handle file-notify event for permission prompt sentinel file."
  (let ((action (nth 1 event))
        (file (nth 2 event)))
    (when (and (memq action '(created changed))
               (string-match-p "permission_prompt$" file))
      (let ((ws (claude-repl--ws-for-dir
                 (string-trim (with-temp-buffer
                                (insert-file-contents file)
                                (buffer-string))))))
        (claude-repl--log "permission notify ws=%s" ws)
        (unless ws
          (error "claude-repl--on-permission-notify: no workspace for dir in %s" file))
        (claude-repl--ws-set ws :permission)
        (delete-file file)))))

(require 'filenotify)
(let ((dir (expand-file-name "~/.claude/workspace-notifications")))
  (make-directory dir t)
  (file-notify-add-watch dir '(change) #'claude-repl--on-permission-notify))

(defun claude-repl--do-refresh ()
  "Low-level refresh of the current vterm buffer.
Must be called with a vterm-mode buffer current."
  (let ((inhibit-read-only t))
    (when vterm--term
      (vterm--redraw vterm--term)))
  (redisplay t))

(defun claude-repl--fix-vterm-scroll (buf)
  "Briefly select the vterm window for BUF to fix Emacs scroll position."
  (let ((vterm-win (get-buffer-window buf))
        (orig-win (selected-window)))
    (when (and vterm-win (not (eq vterm-win orig-win)))
      (select-window vterm-win 'norecord)
      (select-window orig-win 'norecord))))

(defun claude-repl--refresh-vterm ()
  "Refresh the claude vterm display.
Works from any buffer or from within the vterm buffer itself."
  (let ((buf (if (eq major-mode 'vterm-mode)
                 (current-buffer)
               (let ((ws (+workspace-current-name)))
                 (and ws (claude-repl--ws-get ws :vterm-buffer))))))
    (when (and buf (buffer-live-p buf))
      (with-current-buffer buf
        (when (eq major-mode 'vterm-mode)
          (claude-repl--do-refresh)))
      (claude-repl--fix-vterm-scroll buf))))

;; Walk saved window-configuration tree to find claude buffers.
(defun claude-repl--wconf-has-claude-p (wconf)
  "Return non-nil if WCONF (a `window-state-get' tree) contains a claude buffer."
  (when (and wconf (proper-list-p wconf))
    (let ((buf-entry (alist-get 'buffer wconf)))
      (if (and buf-entry (stringp (car-safe buf-entry))
               (string-match-p "^\\*claude-[0-9a-f]+\\*$" (car buf-entry)))
          t
        (cl-some #'claude-repl--wconf-has-claude-p
                 (cl-remove-if-not #'proper-list-p wconf))))))

(defun claude-repl--ws-claude-open-p (ws-name)
  "Return non-nil if workspace WS-NAME has a claude buffer in its window layout.
For the current workspace, checks live windows.
For background workspaces, inspects the saved persp window configuration."
  (if (equal ws-name (+workspace-current-name))
      ;; Current workspace: check live windows
      (cl-some (lambda (buf)
                 (and (buffer-live-p buf)
                      (claude-repl--claude-buffer-p buf)
                      (get-buffer-window buf)))
               (buffer-list))
    ;; Background workspace: inspect saved window config
    (let* ((persp (persp-get-by-name ws-name))
           (wconf (and persp (not (symbolp persp)) (persp-window-conf persp))))
      (claude-repl--wconf-has-claude-p wconf))))

(defun claude-repl--update-ws-state (ws)
  "Update workspace WS state according to claude visibility and git status.
State table:
  :thinking  → unchanged (never touch)
  :done      + clean → :stale  |  :done      + dirty → :done
  :permission         → unchanged
  :stale     + dirty → :done   |  :stale     + clean → :stale
  nil        + dirty → :done   |  nil        + clean → nil"
  (let ((state (claude-repl--ws-state ws))
        (dirty (not (claude-repl--workspace-clean-p ws))))
    (pcase (cons state dirty)
      ;; :done + clean → :stale
      (`(:done . nil)
       (claude-repl--log "update-ws-state ws=%s :done->:stale" ws)
       (claude-repl--ws-clear ws :done)
       (claude-repl--touch-activity ws))
      ;; :stale + dirty → :done
      (`(:stale . t)
       (claude-repl--log "update-ws-state ws=%s :stale->:done" ws)
       (claude-repl--ws-clear ws :stale)
       (claude-repl--ws-set ws :done))
      ;; nil + dirty → :done
      (`(nil . t)
       (claude-repl--log "update-ws-state ws=%s nil->:done" ws)
       (claude-repl--ws-set ws :done))
      ;; :thinking, :permission, :done+dirty, :stale+clean, nil+clean → no-op
      (_ nil))))

(defun claude-repl--update-all-workspace-states ()
  "Update state for all workspaces based on claude visibility and git status."
  (when (bound-and-true-p persp-mode)
    (dolist (ws (+workspace-list-names))
      (if (claude-repl--ws-claude-open-p ws)
          (claude-repl--update-ws-state ws)
        ;; Claude not open on this workspace → clear all state
        (let ((state (claude-repl--ws-state ws)))
          (when (and state (not (eq state :thinking)))
            (claude-repl--log "update-all: ws=%s clearing %s (claude not open)" ws state)
            (claude-repl--ws-clear ws state)))))))

(defun claude-repl--on-frame-focus ()
  "Refresh claude vterm and update all workspace states when Emacs regains focus."
  (when (frame-focus-state)
    (claude-repl--log "on-frame-focus")
    (claude-repl--refresh-vterm)
    (claude-repl--update-all-workspace-states)))

(add-function :after after-focus-change-function #'claude-repl--on-frame-focus)

;; Refresh vterm on workspace switch
(defun claude-repl--on-workspace-switch ()
  "Handle workspace switch: update all workspace states, refresh vterm, reset cursors."
  (claude-repl--log "workspace-switch ws=%s" (+workspace-current-name))
  (claude-repl--update-all-workspace-states)
  (claude-repl--refresh-vterm)
  (claude-repl--reset-vterm-cursors))

;; Save window state for current workspace before switching away,
;; so update-all-workspace-states can inspect the saved config.
(when (modulep! :ui workspaces)
  (add-hook 'persp-before-deactivate-functions
            (lambda (&rest _)
              (ignore-errors (persp-frame-save-state))))
  (add-hook 'persp-activated-functions
            (lambda (&rest _)
              (run-at-time 0 nil #'claude-repl--on-workspace-switch))))

(defvar claude-repl--in-redraw-advice nil
  "Non-nil while the vterm redraw advice is executing, to prevent recursion.")

(defun claude-repl--after-vterm-redraw (&rest _)
  "Apply hide overlay after vterm redraws."
  (unless claude-repl--in-redraw-advice
    (let ((claude-repl--in-redraw-advice t))
      (when (claude-repl--claude-buffer-p)
        (claude-repl--update-hide-overlay)))))

(defun claude-repl--enable-hide-overlay ()
  "Enable the hide overlay advice.  Uses reference counting so
multiple sessions don't clobber each other."
  (claude-repl--log "enable-hide-overlay refcount=%d" claude-repl--hide-overlay-refcount)
  (when (zerop claude-repl--hide-overlay-refcount)
    (advice-add 'vterm--redraw :after #'claude-repl--after-vterm-redraw))
  (cl-incf claude-repl--hide-overlay-refcount))

(defun claude-repl--delete-hide-overlay ()
  "Delete the hide overlay if it exists and nil the variable."
  (when (and claude-repl-hide-overlay
             (overlay-buffer claude-repl-hide-overlay))
    (delete-overlay claude-repl-hide-overlay))
  (setq claude-repl-hide-overlay nil))

(defun claude-repl--disable-hide-overlay ()
  "Disable the hide overlay advice and clean up any existing overlay.
Uses reference counting so the advice is only removed when the last
session releases it."
  (cl-decf claude-repl--hide-overlay-refcount)
  (when (< claude-repl--hide-overlay-refcount 0)
    (setq claude-repl--hide-overlay-refcount 0))
  (claude-repl--log "disable-hide-overlay refcount=%d" claude-repl--hide-overlay-refcount)
  (when (zerop claude-repl--hide-overlay-refcount)
    (advice-remove 'vterm--redraw #'claude-repl--after-vterm-redraw))
  (claude-repl--delete-hide-overlay))

(defun claude-repl--vterm-running-p ()
  "Return t if Claude vterm buffer for the current workspace exists with a live process."
  (let ((buf (claude-repl--ws-get (+workspace-current-name) :vterm-buffer)))
    (and buf (buffer-live-p buf) (get-buffer-process buf))))

(defun claude-repl--input-visible-p ()
  "Return t if input buffer for the current workspace is visible in a window."
  (let ((buf (claude-repl--ws-get (+workspace-current-name) :input-buffer)))
    (and buf (buffer-live-p buf) (get-buffer-window buf))))

(defun claude-repl--vterm-visible-p ()
  "Return t if vterm buffer for the current workspace is visible in a window."
  (let ((buf (claude-repl--ws-get (+workspace-current-name) :vterm-buffer)))
    (and buf (buffer-live-p buf) (get-buffer-window buf))))

(defun claude-repl--panels-visible-p ()
  "Return t if both panels are visible."
  (and (claude-repl--input-visible-p)
       (claude-repl--vterm-visible-p)))

(defun claude-repl--hide-panels ()
  "Hide both Claude panels without killing buffers."
  (claude-repl--log "hide-panels")
  (let* ((ws (+workspace-current-name))
         (input-buf (claude-repl--ws-get ws :input-buffer))
         (vterm-buf (claude-repl--ws-get ws :vterm-buffer)))
    (when-let ((win (and input-buf (get-buffer-window input-buf))))
      (delete-window win))
    (when-let ((win (and vterm-buf (get-buffer-window vterm-buf))))
      (delete-window win))))

;; Auto-close orphaned panels: if one is closed, close the other.
;; Also refresh the hide overlay in case a window change invalidated it.
(defun claude-repl--orphaned-vterm-p (name)
  "Return non-nil if NAME is a claude vterm buffer with no matching input window."
  (and (string-match "^\\*claude-\\([0-9a-f]+\\)\\*$" name)
       (not claude-repl--fullscreen-config)
       (not (one-window-p))
       (not (get-buffer-window
             (format "*claude-input-%s*" (match-string 1 name))))))

(defun claude-repl--orphaned-input-p (name)
  "Return non-nil if NAME is a claude input buffer with no matching vterm window."
  (and (string-match "^\\*claude-input-\\([0-9a-f]+\\)\\*$" name)
       (not (one-window-p))
       (not (get-buffer-window
             (format "*claude-%s*" (match-string 1 name))))
       (not (get-buffer " *claude-loading*"))))

(defun claude-repl--sync-panels ()
  "Close any Claude panel whose partner is no longer visible."
  (dolist (win (window-list))
    (let ((name (buffer-name (window-buffer win))))
      (when (or (claude-repl--orphaned-vterm-p name)
                (claude-repl--orphaned-input-p name))
        (claude-repl--log "sync-panels closing orphaned %s" name)
        (delete-window win)))))

;; Keep visible Claude vterm buffers scrolled to the cursor.
;; Skips the selected window so clicking into vterm to read/copy isn't disrupted.
(defun claude-repl--refresh-vterm-window (win)
  "Refresh the Claude vterm buffer shown in WIN.
Resets cursor, redraws, and syncs window point."
  (let ((buf (window-buffer win)))
    (when (and buf (buffer-live-p buf) (claude-repl--claude-buffer-p buf))
      (with-current-buffer buf
        (when (and (eq major-mode 'vterm-mode)
                   (fboundp 'vterm-reset-cursor-point))
          (let ((inhibit-read-only t))
            (vterm-reset-cursor-point)
            (when vterm--term
              (vterm--redraw vterm--term))
            (vterm-reset-cursor-point)
            (set-window-point win (point))))))))

(defun claude-repl--reset-vterm-cursors ()
  "Refresh every visible Claude vterm window except the selected one."
  (let ((sel (selected-window)))
    (dolist (win (window-list))
      (unless (eq win sel)
        (claude-repl--refresh-vterm-window win)))))

(defvar claude-repl--sync-timer nil
  "Timer for debounced window-change handler.")

(defvar claude-repl--cursor-reset-timer nil
  "Timer for debounced cursor reset.")

(defun claude-repl--on-window-change ()
  "Deferred handler for window configuration changes.
Syncs orphaned panels, refreshes overlay, and resets cursors."
  (claude-repl--log "on-window-change")
  (claude-repl--sync-panels)
  (claude-repl--update-hide-overlay)
  (claude-repl--reset-vterm-cursors))

(defmacro claude-repl--deferred (timer-var fn)
  "Return a lambda that debounces calls to FN via TIMER-VAR."
  `(lambda (&rest _)
     (when ,timer-var
       (cancel-timer ,timer-var))
     (setq ,timer-var (run-at-time 0 nil ,fn))))

(add-hook 'window-configuration-change-hook
          (claude-repl--deferred claude-repl--sync-timer #'claude-repl--on-window-change))

(let ((debounced (claude-repl--deferred claude-repl--cursor-reset-timer #'claude-repl--reset-vterm-cursors)))
  (add-hook 'window-selection-change-functions debounced)
  (add-hook 'buffer-list-update-hook debounced))

;; Redirect focus from vterm output to input buffer (keyboard only, not mouse)
(defun claude-repl--redirect-to-input (_frame)
  "If the selected window shows a Claude vterm buffer, jump to its input window.
Only redirects for keyboard navigation; mouse clicks are allowed through
so the user can select and copy text from the output."
  (unless (mouse-event-p last-input-event)
    (let ((name (buffer-name (window-buffer (selected-window)))))
      (when (string-match "^\\*claude-\\([0-9a-f]+\\)\\*$" name)
        (let* ((id (match-string 1 name))
               (input-buf (get-buffer (format "*claude-input-%s*" id))))
          (when-let ((input-win (and input-buf (get-buffer-window input-buf))))
            (claude-repl--log "redirect-to-input from %s" name)
            (select-window input-win)
            (when (bound-and-true-p evil-mode)
              (evil-insert-state))))))))

(add-hook 'window-selection-change-functions #'claude-repl--redirect-to-input)

(defun claude-repl--ensure-input-buffer (ws)
  "Create input buffer for workspace WS if needed, put in claude-input-mode."
  (claude-repl--log "ensure-input-buffer")
  (let* ((root (claude-repl--resolve-root))
         (input-buf (get-buffer-create (claude-repl--buffer-name "-input"))))
    (claude-repl--ws-put ws :input-buffer input-buf)
    (with-current-buffer input-buf
      (setq-local claude-repl--project-root root)
      (unless (eq major-mode 'claude-input-mode)
        (claude-input-mode)
        (claude-repl--history-restore)))))

(defun claude-repl--kill-stale-vterm ()
  "Kill the Claude vterm buffer if it exists but has no live process."
  (when-let ((existing (get-buffer (claude-repl--buffer-name))))
    (unless (get-buffer-process existing)
      (claude-repl--log "killing stale vterm %s" (buffer-name existing))
      (kill-buffer existing))))

(defun claude-repl--ensure-vterm-buffer (ws)
  "Create vterm buffer for workspace WS running claude if needed. Starts claude from the git root."
  (let* ((root (claude-repl--resolve-root))
         (default-directory root))
    (claude-repl--kill-stale-vterm)
    (let ((vterm-buf (get-buffer-create (claude-repl--buffer-name))))
      (claude-repl--ws-put ws :vterm-buffer vterm-buf)
      (with-current-buffer vterm-buf
        (setq-local claude-repl--project-root root)
        (setq-local claude-repl--owning-workspace ws)
        (unless (eq major-mode 'vterm-mode)
          (vterm-mode)
          (setq-local truncate-lines nil)
          (setq-local word-wrap t)
          (claude-repl--set-buffer-background 15)
          (claude-repl--log "ensure-vterm created %s root=%s" (buffer-name vterm-buf) root)
          (claude-repl--start-claude))))))

(defun claude-repl--show-panels-with-placeholder ()
  "Show panels using a loading placeholder in the vterm slot.
The placeholder is swapped for the real vterm buffer once Claude is ready."
  (claude-repl--log "show-panels-with-placeholder")
  (let* ((ws (+workspace-current-name))
         (real-vterm (claude-repl--ws-get ws :vterm-buffer))
         (placeholder (get-buffer-create " *claude-loading*")))
    (with-current-buffer placeholder
      (setq-local mode-line-format nil)
      (claude-repl--set-buffer-background 15))
    (claude-repl--ws-put ws :vterm-buffer placeholder)
    (claude-repl--show-panels)
    (claude-repl--focus-input-panel)
    (claude-repl--ws-put ws :vterm-buffer real-vterm)))

(defun claude-repl--start-fresh ()
  "Start a new Claude session with placeholder panels."
  (claude-repl--log "start-fresh")
  (let ((ws (+workspace-current-name)))
    (unless ws (error "claude-repl--start-fresh: no active workspace"))
    (claude-repl--touch-activity ws)
    (claude-repl--ws-put ws :saved-window-config (current-window-configuration)))
  (delete-other-windows (claude-repl--live-return-window))
  (claude-repl--ensure-session)
  (claude-repl--show-panels-with-placeholder)
  (message "Starting Claude..."))

(defun claude-repl--show-existing-panels ()
  "Show panels for an already-running Claude session.
Demotes indicators, refreshes display, and restores panel layout."
  (claude-repl--log "show-existing-panels")
  (let ((ws (+workspace-current-name)))
    (unless ws (error "claude-repl--show-existing-panels: no active workspace"))
    (claude-repl--touch-activity ws)
    (claude-repl--refresh-vterm)
    (claude-repl--ws-put ws :saved-window-config (current-window-configuration))
    (delete-other-windows (claude-repl--live-return-window))
    (claude-repl--ensure-input-buffer ws)
    (claude-repl--show-panels)
    (claude-repl--focus-input-panel)
    (claude-repl--update-hide-overlay)))

;; Entry point - smart toggle
(defun claude-repl ()
  "Toggle Claude REPL panels.
If text is selected: send it directly to Claude.
If not running: start Claude and show both panels.
If panels visible: hide both panels.
If panels hidden: show both panels."
  (interactive)
  (claude-repl--remember-return-window)
  (let ((vterm-running (claude-repl--vterm-running-p))
        (panels-visible (claude-repl--panels-visible-p))
        (selection (when (use-region-p)
                    (buffer-substring-no-properties (region-beginning) (region-end)))))
    (claude-repl--log "claude-repl running=%s visible=%s selection=%s"
                      vterm-running panels-visible (if selection "yes" "no"))
    (cond
     ;; Text selected - send directly to Claude
     (selection
      (deactivate-mark)
      (claude-repl--send-to-claude selection))
     ;; Nothing running - start fresh with placeholder until Claude is ready
     ((not vterm-running)
      (claude-repl--start-fresh))
     ;; Panels visible - hide both, restore window layout
     (panels-visible
      (let ((ws (+workspace-current-name)))
        (unless ws (error "claude-repl: no active workspace when hiding panels"))
        (claude-repl--ws-clear ws :thinking)
        (claude-repl--ws-clear ws :done)
        (claude-repl--ws-clear ws :permission)
        (claude-repl--ws-clear ws :stale))
      (claude-repl--restore-layout))
     ;; Panels hidden - show both
     (t
      (claude-repl--show-existing-panels)))))

(defun claude-repl--close-buffer-windows (&rest bufs)
  "Close windows displaying any of BUFS."
  (claude-repl--log "close-buffer-windows %s" (mapcar (lambda (b) (and b (buffer-name b))) bufs))
  (dolist (buf bufs)
    (when (and buf (buffer-live-p buf))
      (when-let ((win (get-buffer-window buf)))
        (ignore-errors (delete-window win))))))

(defun claude-repl--kill-placeholder ()
  "Close and kill the loading placeholder buffer if it exists."
  (claude-repl--log "kill-placeholder exists=%s" (if (get-buffer " *claude-loading*") "yes" "no"))
  (when-let ((placeholder (get-buffer " *claude-loading*")))
    (when-let ((win (get-buffer-window placeholder)))
      (ignore-errors (delete-window win)))
    (kill-buffer placeholder)))

(defun claude-repl--schedule-sigkill (proc)
  "Schedule a SIGKILL for PROC after 0.5s if it's still alive."
  (run-at-time 0.5 nil
               (lambda ()
                 (when (process-live-p proc)
                   (claude-repl--log "sigkill fallback for lingering process")
                   (signal-process proc 'SIGKILL)))))

(defun claude-repl--kill-vterm-process (buf)
  "Kill the vterm buffer BUF and its process."
  (claude-repl--log "kill-vterm-process buf=%s" (and buf (buffer-name buf)))
  (when (and buf (buffer-live-p buf))
    (let ((proc (get-buffer-process buf)))
      (when proc
        (set-process-query-on-exit-flag proc nil))
      (kill-buffer buf)
      (when proc
        (claude-repl--schedule-sigkill proc)))))

(defun claude-repl--teardown-session-state (ws)
  "Save history, disable overlay, cancel timers, and clear session state for workspace WS."
  (claude-repl--log "teardown-session-state")
  (ignore-errors (claude-repl--history-save ws))
  (ignore-errors (claude-repl--disable-hide-overlay))
  (when claude-repl--sync-timer
    (cancel-timer claude-repl--sync-timer)
    (setq claude-repl--sync-timer nil))
  (claude-repl--ws-put ws :vterm-buffer nil)
  (claude-repl--ws-put ws :input-buffer nil)
  (claude-repl--ws-put ws :saved-window-config nil))

(defun claude-repl--destroy-session-buffers (vterm-buf input-buf)
  "Close windows and kill VTERM-BUF, INPUT-BUF, and any placeholder."
  (claude-repl--log "destroy-session-buffers")
  (claude-repl--close-buffer-windows vterm-buf input-buf)
  (claude-repl--kill-placeholder)
  (claude-repl--kill-vterm-process vterm-buf)
  (when (and input-buf (buffer-live-p input-buf))
    (kill-buffer input-buf)))

(defun claude-repl-kill ()
  "Kill Claude REPL buffers and windows for the current workspace."
  (interactive)
  (claude-repl--log "kill")
  (let* ((ws (+workspace-current-name))
         (vterm-buf (claude-repl--ws-get ws :vterm-buffer))
         (input-buf (claude-repl--ws-get ws :input-buffer)))
    (unless ws (error "claude-repl-kill: no active workspace"))
    (claude-repl--ws-put ws :status nil)
    (claude-repl--ws-put ws :activity-time nil)
    (force-mode-line-update t)
    (claude-repl--teardown-session-state ws)
    (claude-repl--destroy-session-buffers vterm-buf input-buf)))

(defun claude-repl-restart ()
  "Kill Claude REPL and restart with `claude -c` to continue session."
  (interactive)
  (claude-repl--log "restart")
  (let ((ws (+workspace-current-name)))
    (claude-repl-kill)
    (claude-repl--ensure-vterm-buffer ws)
    (claude-repl--ensure-input-buffer ws)
    (claude-repl--enable-hide-overlay)
    (claude-repl--show-panels)
    (claude-repl--focus-input-panel)))

(defun claude-repl-focus-input ()
  "Focus the Claude input buffer, or return to previous window if already there.
If Claude isn't running, start it (same as `claude-repl')."
  (interactive)
  (let ((ws (+workspace-current-name)))
    (cond
     ;; Already in the input buffer — jump back
     ((eq (current-buffer) (claude-repl--ws-get ws :input-buffer))
      (claude-repl--log "focus-input branch=jump-back")
      (let ((ret (claude-repl--ws-get ws :return-window)))
        (if (and ret (window-live-p ret))
            (select-window ret)
          (evil-window-left 1))))
     ;; Not running — start fresh
     ((not (claude-repl--vterm-running-p))
      (claude-repl--log "focus-input branch=start-fresh")
      (claude-repl))
     ;; Running but panels hidden — show them
     (t
      (claude-repl--log "focus-input branch=show-or-focus")
      (unless (claude-repl--panels-visible-p)
        (claude-repl--ws-put ws :saved-window-config (current-window-configuration))
        (claude-repl--ensure-input-buffer ws)
        (claude-repl--show-panels))
      (claude-repl--ws-put ws :return-window (selected-window))
      (when-let ((win (get-buffer-window (claude-repl--ws-get ws :input-buffer))))
        (select-window win)
        (when (bound-and-true-p evil-mode)
          (evil-insert-state)))))))

(defun claude-repl-toggle-fullscreen ()
  "Toggle the Claude vterm buffer fullscreen.
If already fullscreen, restore the previous window layout."
  (interactive)
  (claude-repl--log "toggle-fullscreen currently=%s" (if claude-repl--fullscreen-config "fullscreen" "normal"))
  (cond
   ;; Already fullscreen — restore
   (claude-repl--fullscreen-config
    (set-window-configuration claude-repl--fullscreen-config)
    (setq claude-repl--fullscreen-config nil))
   ;; Not fullscreen — go fullscreen if vterm exists
   ((claude-repl--vterm-live-p)
    (let ((vterm-buf (claude-repl--ws-get (+workspace-current-name) :vterm-buffer)))
      (setq claude-repl--fullscreen-config (current-window-configuration))
      (delete-other-windows)
      (set-window-dedicated-p (selected-window) nil)
      (switch-to-buffer vterm-buf)
      (claude-repl--reset-vterm-cursors)))
   (t (message "No Claude vterm buffer for this workspace."))))

(defun claude-repl-cycle ()
  "Send backtab to Claude vterm to cycle through options."
  (interactive)
  (claude-repl--log "cycle")
  (when (claude-repl--vterm-live-p)
    (with-current-buffer (claude-repl--ws-get (+workspace-current-name) :vterm-buffer)
      (vterm-send-key "<backtab>"))))

;; Global bindings
(map! :nvi "C-S-m" #'claude-repl-cycle)
(map! :i "C-S-f" #'claude-repl-toggle-fullscreen)
(map! :leader :prefix "w" :n "c" #'claude-repl-toggle-fullscreen)

(defun claude-repl-copy-reference ()
  "Copy the current file and line reference to the clipboard.
With active region: copies file:startline-endline.
Without region: copies file:line."
  (interactive)
  (let* ((rel (claude-repl--rel-path))
         (ref (if (use-region-p)
                  (let ((start-line (line-number-at-pos (region-beginning)))
                        (end-line (line-number-at-pos (region-end))))
                    (deactivate-mark)
                    (format "%s:%d-%d" rel start-line end-line))
                (format "%s:%d" rel (line-number-at-pos (point))))))
    (kill-new ref)
    (message "Copied: %s" ref)))

;; Keybindings
(map! :leader
      :desc "Claude REPL" "o c" #'claude-repl
      :desc "Claude input" "o v" #'claude-repl-focus-input
      :desc "Kill Claude" "o C" #'claude-repl-kill
      :desc "Claude explain" "o e" #'claude-repl-explain
      :desc "Claude interrupt" "o x" #'claude-repl-interrupt
      :desc "Copy file reference" "o r" #'claude-repl-copy-reference
      ;; Test quality analysis (AAA/DRY)
      :desc "Tests: worktree" "o w" #'claude-repl-test-quality-worktree
      :desc "Tests: staged" "o i" #'claude-repl-test-quality-staged
      :desc "Tests: uncommitted" "o u" #'claude-repl-test-quality-uncommitted
      :desc "Tests: HEAD" "o h" #'claude-repl-test-quality-head
      ;; Test coverage analysis
      :desc "Coverage: worktree" "o W" #'claude-repl-test-coverage-worktree
      :desc "Coverage: staged" "o I" #'claude-repl-test-coverage-staged
      :desc "Coverage: uncommitted" "o U" #'claude-repl-test-coverage-uncommitted
      :desc "Coverage: HEAD" "o H" #'claude-repl-test-coverage-head)

(dotimes (i 10)
  (let ((char (number-to-string i)))
    (define-key doom-leader-map (kbd (format "o %s" char))
      (lambda () (interactive) (claude-repl-send-char char)))))

;; FIXME: fix magit handling

;; C-v paste forwarding to vterm
(defun claude-repl-paste-to-vterm ()
  "Forward a Ctrl-V keystroke to the Claude vterm buffer.
This lets Claude CLI handle paste natively, including images."
  (interactive)
  (when (claude-repl--vterm-live-p)
    (with-current-buffer (claude-repl--ws-get (+workspace-current-name) :vterm-buffer)
      (vterm-send-key "v" nil nil t))))

;;; Debug helpers — interactive commands for diagnosing workspace state issues.
;;; Call via M-x claude-repl-debug/...

(defun claude-repl-debug/workspace-states ()
  "Display all workspace states."
  (interactive)
  (let ((states (mapcar (lambda (name)
                          (cons name (claude-repl--ws-state name)))
                        (+workspace-list-names))))
    (message "Workspace states:\n%s"
             (mapconcat (lambda (s) (format "  %s: %s" (car s) (or (cdr s) "nil")))
                        states "\n"))))

(defun claude-repl-debug/buffer-info ()
  "Display all claude vterm buffers with their owning and persp workspaces."
  (interactive)
  (let (result)
    (dolist (buf (buffer-list))
      (when (string-match-p "^\\*claude-[0-9a-f]+\\*$" (buffer-name buf))
        (push (format "  %s  owning=%s  persp=%s  thinking=%s"
                      (buffer-name buf)
                      (or (buffer-local-value 'claude-repl--owning-workspace buf) "nil")
                      (or (claude-repl--workspace-for-buffer buf) "nil")
                      (buffer-local-value 'claude-repl--title-thinking buf))
              result)))
    (message "Claude buffers:\n%s"
             (if result (mapconcat #'identity (nreverse result) "\n") "  (none)"))))

(defun claude-repl-debug/clear-state (ws)
  "Clear all states for workspace WS without killing buffers."
  (interactive
   (list (completing-read "Workspace: " (+workspace-list-names) nil t)))
  (claude-repl--ws-clear ws :thinking)
  (claude-repl--ws-clear ws :done)
  (claude-repl--ws-clear ws :permission)
  (claude-repl--ws-clear ws :stale)
  (message "Cleared all states for %s" ws))

(defun claude-repl-debug/obliterate (ws)
  "Completely remove workspace WS from all claude-repl tracking.
Kills claude buffers, closes windows, and removes all state."
  (interactive
   (list (completing-read "Obliterate workspace: " (+workspace-list-names) nil t)))
  ;; Find and destroy any claude buffers owned by this workspace
  (dolist (buf (buffer-list))
    (when (and (buffer-live-p buf)
               (string-match-p "^\\*claude-\\(input-\\)?[0-9a-f]+\\*$" (buffer-name buf))
               (equal ws (buffer-local-value 'claude-repl--owning-workspace buf)))
      (when-let ((win (get-buffer-window buf)))
        (ignore-errors (delete-window win)))
      (let ((proc (get-buffer-process buf)))
        (when proc (set-process-query-on-exit-flag proc nil)))
      (kill-buffer buf)))
  ;; Remove all state for this workspace from the single hash table
  (claude-repl--ws-del ws)
  (message "Obliterated all claude-repl state for %s" ws))

(defun claude-repl-debug/set-owning-workspace ()
  "Set the owning workspace for a claude vterm buffer."
  (interactive)
  (let* ((bufs (cl-remove-if-not
                (lambda (b) (string-match-p "^\\*claude-[0-9a-f]+\\*$" (buffer-name b)))
                (buffer-list)))
         (buf-name (completing-read "Buffer: " (mapcar #'buffer-name bufs) nil t))
         (ws (completing-read "Owning workspace: " (+workspace-list-names) nil t)))
    (with-current-buffer buf-name
      (setq-local claude-repl--owning-workspace ws))
    (message "Set %s owning workspace to %s" buf-name ws)))

(defun claude-repl-debug/toggle-logging ()
  "Toggle debug logging."
  (interactive)
  (setq claude-repl-debug (not claude-repl-debug))
  (message "Claude REPL debug logging: %s" (if claude-repl-debug "ON" "OFF")))

(defun claude-repl-debug/toggle-metaprompt ()
  "Toggle the metaprompt prefix injection."
  (interactive)
  (setq claude-repl-skip-permissions (not claude-repl-skip-permissions))
  (message "Claude REPL metaprompt: %s" (if claude-repl-skip-permissions "ON" "OFF")))

(defun claude-repl-debug/prefix-counter ()
  "Show the current metaprompt prefix counter, period, and workspace."
  (interactive)
  (let* ((ws (+workspace-current-name))
         (counter (or (claude-repl--ws-get ws :prefix-counter) 0)))
    (message "[%s] Prefix counter: %d  period: %d  next metaprompt in: %d sends"
             ws counter claude-repl-prefix-period
             (- claude-repl-prefix-period (mod counter claude-repl-prefix-period)))))

(defun claude-repl--git-branch-for-workspace (ws-name)
  "Return the git branch for workspace WS-NAME, or nil.
Uses the first live buffer's default-directory and runs
git rev-parse --abbrev-ref HEAD (works for both repos and worktrees)."
  (ignore-errors
    (let* ((persp (persp-get-by-name ws-name))
           (bufs (and persp (not (symbolp persp)) (persp-buffers persp)))
           (dir (cl-loop for buf in bufs
                         when (buffer-live-p buf)
                         return (buffer-local-value 'default-directory buf))))
      (when dir
        (let ((default-directory dir))
          (with-temp-buffer
            (when (= 0 (process-file "git" nil t nil "rev-parse" "--abbrev-ref" "HEAD"))
              (string-trim (buffer-string)))))))))

(defun claude-repl--ws-base-name (ws-name)
  "Strip any existing branch suffix from WS-NAME.
E.g. \"my-project (main)\" → \"my-project\"."
  (if (string-match "\\(.+?\\) (.*?)$" ws-name)
      (match-string 1 ws-name)
    ws-name))

(defun claude-repl-debug/ws-set-branch (ws-name)
  "Rename workspace WS-NAME to include its current git branch.
Strips any existing branch suffix first, then appends \" (branch)\"."
  (interactive
   (list (completing-read "Workspace: " (+workspace-list-names) nil t
                          nil nil (+workspace-current-name))))
  (let* ((base (claude-repl--ws-base-name ws-name))
         (branch (claude-repl--git-branch-for-workspace ws-name))
         (new-name (if branch (format "%s (%s)" base branch) base)))
    (if (equal ws-name new-name)
        (message "Workspace %s: already up to date" ws-name)
      (+workspace-rename ws-name new-name)
      (message "Renamed: %s → %s" ws-name new-name))))

(defun claude-repl-debug/workspace-clean-p (ws-name)
  "Show whether workspace WS-NAME has unstaged changes to tracked files.
Uses `claude-repl--workspace-clean-p' — the same function used in production."
  (interactive
   (list (completing-read "Workspace: " (+workspace-list-names) nil t
                          nil nil (+workspace-current-name))))
  (let ((clean (claude-repl--workspace-clean-p ws-name)))
    (message "Workspace %s: %s" ws-name (if clean "clean" "dirty"))))

(defun claude-repl-debug/refresh-state (ws-name)
  "Force a full state refresh for workspace WS-NAME.
Runs the same logic as the periodic `update-all-workspace-states' timer:
checks claude visibility, git dirty status, and applies the state table.
Reports comprehensive diagnostics."
  (interactive
   (list (completing-read "Workspace: " (+workspace-list-names) nil t
                          nil nil (+workspace-current-name))))
  (let* ((before (claude-repl--ws-state ws-name))
         (open (claude-repl--ws-claude-open-p ws-name))
         (dirty (not (claude-repl--workspace-clean-p ws-name)))
         ;; Find the claude vterm buffer for this workspace
         (persp (persp-get-by-name ws-name))
         (persp-bufs (and persp (not (symbolp persp)) (persp-buffers persp)))
         (vterm-buf (cl-loop for buf in persp-bufs
                             when (and (buffer-live-p buf)
                                       (claude-repl--claude-buffer-p buf))
                             return buf))
         ;; Read buffer-local state from the vterm buffer
         (title-thinking (and vterm-buf
                              (buffer-local-value 'claude-repl--title-thinking vterm-buf)))
         (proc (and vterm-buf (get-buffer-process vterm-buf)))
         (proc-alive (and proc (process-live-p proc)))
         (owning-ws (and vterm-buf
                         (buffer-local-value 'claude-repl--owning-workspace vterm-buf)))
         (has-window (and vterm-buf (get-buffer-window vterm-buf t))))
    ;; Apply the state update
    (if open
        (claude-repl--update-ws-state ws-name)
      (let ((state (claude-repl--ws-state ws-name)))
        (when (and state (not (eq state :thinking)))
          (claude-repl--ws-clear ws-name state))))
    (let ((after (claude-repl--ws-state ws-name)))
      (force-mode-line-update t)
      (message (concat "Workspace %s:\n"
                       "  vterm-buf=%s process=%s\n"
                       "  owning-ws=%s title-thinking=%s has-window=%s\n"
                       "  claude-open=%s dirty=%s\n"
                       "  state=%s -> %s")
               ws-name
               (and vterm-buf (buffer-name vterm-buf))
               (if proc-alive "alive" "dead/nil")
               (or owning-ws "nil") title-thinking (if has-window "yes" "no")
               (if open "yes" "no") (if dirty "yes" "no")
               (or before "nil") (or after "nil")))))

(provide 'claude-repl)
;;; config.el ends here
