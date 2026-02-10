;;; config.el --- Claude REPL for Doom Emacs -*- lexical-binding: t; -*-

;; Author: Dodge Coates
;; URL: https://github.com/dodgecoates
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; A Claude Code REPL integration for Doom Emacs with workspace-aware
;; session management, input history, and status indicators.

;;; Code:

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

(defvar claude-repl-vterm-buffer nil
  "Current Claude vterm buffer.  Swapped per-project by `claude-repl--load-session'.")

(defvar claude-repl-input-buffer nil
  "Current Claude input buffer.  Swapped per-project by `claude-repl--load-session'.")

(defvar claude-repl-return-window nil
  "Window to return to after closing Claude panels.")

(defvar claude-repl--saved-window-config nil
  "Saved window configuration before panels were shown.")

(defvar claude-repl--fullscreen-config nil
  "Saved window configuration before fullscreen toggle.")

(defvar claude-repl--hide-overlay-refcount 0
  "Reference count for the hide overlay advice.
Only add advice when going from 0 to 1; only remove when going from 1 to 0.")

(defvar-local claude-repl-hide-overlay nil
  "Overlay used to hide Claude CLI input box.")

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

(defcustom claude-repl-prefix-period 5
  "Number of prompts between metaprompt prefix injections.
The prefix is sent on the first prompt and every Nth prompt thereafter."
  :type 'integer
  :group 'claude-repl)

(defvar claude-repl--prefix-counter 0
  "Counts prompts since the last prefix injection.")

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

;; Per-project session storage
(defvar claude-repl--sessions (make-hash-table :test 'equal)
  "Hash table mapping workspace IDs to session plists.")

(defun claude-repl--buffer-name (&optional suffix)
  "Return a project-specific buffer name like *claude-HASH* or *claude-input-HASH*.
SUFFIX, if provided, is inserted before the hash (e.g. \"-input\")."
  (let ((id (claude-repl--workspace-id)))
    (format "*claude%s-%s*" (or suffix "") (or id "default"))))

(defun claude-repl--load-session ()
  "Load the current project's session state into global vars.
Sets `claude-repl-vterm-buffer', `claude-repl-input-buffer' from buffer names,
and restores window config from the sessions hash table."
  (let* ((id (claude-repl--workspace-id))
         (vterm-name (claude-repl--buffer-name))
         (input-name (claude-repl--buffer-name "-input"))
         (session (gethash id claude-repl--sessions)))
    (setq claude-repl-vterm-buffer (get-buffer vterm-name)
          claude-repl-input-buffer (get-buffer input-name)
          claude-repl--saved-window-config (plist-get session :saved-window-config)
          claude-repl-return-window (plist-get session :return-window)
          claude-repl--prefix-counter (or (plist-get session :prefix-counter) 0))
    (claude-repl--log "load-session id=%s vterm=%s input=%s" id
                      (and claude-repl-vterm-buffer (buffer-name claude-repl-vterm-buffer))
                      (and claude-repl-input-buffer (buffer-name claude-repl-input-buffer)))))

(defun claude-repl--save-session ()
  "Save the current global state back to the sessions hash table."
  (let ((id (claude-repl--workspace-id)))
    (claude-repl--log "save-session id=%s" id)
    (puthash id
             (list :saved-window-config claude-repl--saved-window-config
                   :return-window claude-repl-return-window
                   :prefix-counter claude-repl--prefix-counter)
             claude-repl--sessions)))

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
  "Return `claude-repl-return-window' if it is live, else `selected-window'."
  (if (and claude-repl-return-window (window-live-p claude-repl-return-window))
      claude-repl-return-window
    (selected-window)))

;; Manual window layout: vterm on the right (full height), input below vterm.
(defun claude-repl--show-panels ()
  "Display vterm and input panels to the right of the current window.
Splits right for vterm (60% width to work window), then splits vterm bottom for input (15%)."
  (claude-repl--log "show-panels vterm=%s input=%s"
                    (and claude-repl-vterm-buffer (buffer-name claude-repl-vterm-buffer))
                    (and claude-repl-input-buffer (buffer-name claude-repl-input-buffer)))
  (let* ((work-win (claude-repl--live-return-window))
         (vterm-win (split-window work-win (round (* 0.6 (window-total-width work-win))) 'right))
         (input-win (split-window vterm-win (round (* -0.15 (window-total-height vterm-win))) 'below)))
    (claude-repl--refresh-vterm)
    (set-window-buffer vterm-win claude-repl-vterm-buffer)
    (set-window-buffer input-win claude-repl-input-buffer)
    (set-window-dedicated-p vterm-win t)
    (set-window-dedicated-p input-win t)
    ;; Lock width to prevent resize-triggered reflow in vterm
    (set-window-parameter vterm-win 'window-size-fixed 'width)))

(defun claude-repl--focus-input-panel ()
  "Focus the input panel window and enter insert state."
  (claude-repl--log "focus-input-panel")
  (when-let ((win (get-buffer-window claude-repl-input-buffer)))
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
  "Return non-nil if the Claude vterm buffer exists and is live."
  (and claude-repl-vterm-buffer
       (buffer-live-p claude-repl-vterm-buffer)))

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
              "C-RET: send+hide | C-c C-c: clear+save | C-c C-k: interrupt | <up>/<down>: history")
  (face-remap-add-relative 'header-line 'claude-repl-header-line)
  (claude-repl--set-buffer-background 37)
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
  (claude-repl--load-session)
  (when (claude-repl--vterm-live-p)
    (with-current-buffer claude-repl-vterm-buffer (vterm-send-down))))

(defun claude-repl-scroll-up ()
  "Scroll the Claude vterm buffer up."
  (interactive)
  (claude-repl--log "scroll-up")
  (claude-repl--load-session)
  (when (claude-repl--vterm-live-p)
    (with-current-buffer claude-repl-vterm-buffer (vterm-send-up))))

(map! :map claude-input-mode-map
      :ni "RET"       #'claude-repl-send
      :ni "S-RET"     #'newline
      :ni "C-RET"     #'claude-repl-send-and-hide
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

(defun claude-repl--history-save ()
  "Write input history to disk.
Resolves the project root from the input buffer's local variable
rather than `default-directory' of whatever buffer is current."
  (claude-repl--log "history-save")
  (when (and claude-repl-input-buffer
             (buffer-live-p claude-repl-input-buffer))
    (let* ((root (buffer-local-value 'claude-repl--project-root claude-repl-input-buffer))
           (history (buffer-local-value 'claude-repl--input-history claude-repl-input-buffer))
           (file (expand-file-name ".claude-repl-history" (or root default-directory))))
      (when history
        (with-temp-file file
          (prin1 history (current-buffer)))))))

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
(defun claude-repl--prepare-input ()
  "Read and return input from the input buffer.
Periodically prepends the metaprompt prefix when `claude-repl-skip-permissions' is on."
  (claude-repl--log "prepare-input counter=%d period=%d" claude-repl--prefix-counter claude-repl-prefix-period)
  (let ((raw (with-current-buffer claude-repl-input-buffer
               (buffer-string))))
    (if (and claude-repl-skip-permissions claude-repl-command-prefix
             (zerop (mod claude-repl--prefix-counter claude-repl-prefix-period)))
        (concat claude-repl--command-prefix raw)
      raw)))

(defun claude-repl--schedule-failed-check (ws)
  "Schedule a 5s check: if WS is still :thinking without a title spinner, mark :failed."
  (claude-repl--log "schedule-failed-check ws=%s" ws)
  (let ((check-ws ws)
        (check-buf claude-repl-vterm-buffer))
    (run-at-time 5 nil
                 (lambda ()
                   (claude-repl--log "failed-check firing ws=%s state=%s title-thinking=%s"
                                     check-ws (claude-repl--ws-state check-ws)
                                     (and (buffer-live-p check-buf)
                                          (buffer-local-value 'claude-repl--title-thinking check-buf)))
                   (when (and (buffer-live-p check-buf)
                              (eq (claude-repl--ws-state check-ws) :thinking)
                              (not (buffer-local-value 'claude-repl--title-thinking check-buf)))
                     (claude-repl--ws-set check-ws :failed))))))

(defun claude-repl--send-input-to-vterm (input)
  "Send INPUT string to the Claude vterm buffer.
Uses paste mode for large inputs to avoid truncation."
  (claude-repl--log "send-input-to-vterm len=%d mode=%s" (length input)
                    (if (> (length input) 200) "paste" "direct"))
  (with-current-buffer claude-repl-vterm-buffer
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
  "Mark workspace WS as thinking: set state, record activity, schedule failed check."
  (claude-repl--log "mark-ws-thinking ws=%s" ws)
  (claude-repl--ws-set ws :thinking)
  (claude-repl--touch-activity ws)
  (claude-repl--schedule-failed-check ws))

(defun claude-repl--clear-input ()
  "Push current input to history, reset browsing, and clear the input buffer."
  (claude-repl--log "clear-input")
  (with-current-buffer claude-repl-input-buffer
    (claude-repl--history-push)
    (claude-repl--history-reset)
    (erase-buffer)))

(defun claude-repl-send ()
  "Send input buffer contents to Claude, clear buffer, and return to previous window."
  (interactive)
  (claude-repl--load-session)
  (when (claude-repl--vterm-live-p)
    (let ((ws (+workspace-current-name))
          (input (claude-repl--prepare-input)))
      (unless ws (error "claude-repl-send: no active workspace"))
      (claude-repl--log "send ws=%s len=%d" ws (length input))
      (setq claude-repl--prefix-counter (1+ claude-repl--prefix-counter))
      (claude-repl--mark-ws-thinking ws)
      (claude-repl--send-input-to-vterm input)
      (claude-repl--clear-input)
      (when (and claude-repl-return-window (window-live-p claude-repl-return-window))
        (select-window claude-repl-return-window)))))

(defun claude-repl--remember-return-window ()
  "Save the current window as the return target, unless we're in the input buffer."
  (unless (eq (current-buffer) claude-repl-input-buffer)
    (claude-repl--log "remember-return-window %s" (selected-window))
    (setq claude-repl-return-window (selected-window))))

(defun claude-repl--restore-layout ()
  "Restore the window layout from before panels were shown.
Falls back to hiding panels and selecting the return window."
  (claude-repl--log "restore-layout saved-config=%s" (if claude-repl--saved-window-config "yes" "no"))
  (if claude-repl--saved-window-config
      (progn
        (set-window-configuration claude-repl--saved-window-config)
        (setq claude-repl--saved-window-config nil))
    (claude-repl--hide-panels)
    (when (and claude-repl-return-window (window-live-p claude-repl-return-window))
      (select-window claude-repl-return-window))))

(defun claude-repl-send-and-hide ()
  "Send input to Claude and hide both panels."
  (interactive)
  (claude-repl--log "send-and-hide")
  (claude-repl--load-session)
  (claude-repl-send)
  (claude-repl--restore-layout)
  (claude-repl--save-session))

(defun claude-repl-send-char (char)
  "Send a single character to Claude."
  (claude-repl--log "send-char %s" char)
  (claude-repl--load-session)
  (when (claude-repl--vterm-live-p)
    (with-current-buffer claude-repl-vterm-buffer
      (vterm-send-string char)
      (vterm-send-return))))

(defun claude-repl--ensure-session ()
  "Ensure a Claude session exists (vterm + input + overlay).
No-op if already running."
  (unless (claude-repl--vterm-running-p)
    (claude-repl--log "ensure-session: starting new session")
    (claude-repl--ensure-vterm-buffer)
    (claude-repl--ensure-input-buffer)
    (claude-repl--enable-hide-overlay)))

(defun claude-repl--send-to-claude (text)
  "Send TEXT to Claude, starting it if needed."
  (claude-repl--log "send-to-claude len=%d" (length text))
  (claude-repl--load-session)
  (claude-repl--ensure-session)
  (with-current-buffer claude-repl-vterm-buffer
    (vterm-send-string text)
    (vterm-send-return))
  (claude-repl--save-session))

(defun claude-repl--rel-path ()
  "Return the current file path relative to the project root."
  (let ((file (buffer-file-name)))
    (unless file
      (user-error "Buffer %s is not visiting a file" (buffer-name)))
    (file-relative-name file (claude-repl--resolve-root))))

(defun claude-repl-explain ()
  "Ask Claude to explain the selected region, current line, or current file.
With active region: sends file path and line range.
Without region: sends file path and current line."
  (interactive)
  (claude-repl--load-session)
  (let* ((rel (claude-repl--rel-path))
         (msg (if (use-region-p)
                  (let ((start-line (line-number-at-pos (region-beginning)))
                        (end-line (line-number-at-pos (region-end))))
                    (deactivate-mark)
                    (format "please explain %s:%d-%d" rel start-line end-line))
                (format "please explain %s:%d" rel (line-number-at-pos (point))))))
    (claude-repl--log "explain %s" msg)
    (claude-repl--send-to-claude msg)))

(defun claude-repl-interrupt ()
  "Send Escape to interrupt Claude."
  (interactive)
  (claude-repl--log "interrupt")
  (claude-repl--load-session)
  (when (claude-repl--vterm-live-p)
    (with-current-buffer claude-repl-vterm-buffer
      (vterm-send-key "<escape>")
      (vterm-send-key "<escape>"))))

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
  "Update overlay to hide bottom lines of Claude vterm buffer."
  (when (claude-repl--vterm-live-p)
    (with-current-buffer claude-repl-vterm-buffer
      (claude-repl--delete-hide-overlay)
      (claude-repl--create-hide-overlay))))

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
(defvar claude-repl--done-workspaces (make-hash-table :test 'equal)
  "Hash table of workspaces where Claude has finished.")

(defvar claude-repl--thinking-workspaces (make-hash-table :test 'equal)
  "Hash table of workspaces where Claude is thinking.")

(defvar claude-repl--permission-workspaces (make-hash-table :test 'equal)
  "Hash table of workspaces where Claude needs permission.")

(defvar claude-repl--activity-times (make-hash-table :test 'equal)
  "Hash table of workspace name to last-activity float-time.")

(defvar claude-repl--failed-workspaces (make-hash-table :test 'equal)
  "Hash table of workspaces where Claude failed to start thinking.")

(defcustom claude-repl-stale-minutes 60
  "Minutes after last input before a workspace tab stops showing as stale."
  :type 'integer
  :group 'claude-repl)

(defun claude-repl--ws-state (ws)
  "Return the current status keyword for workspace WS.
Returns one of: :thinking, :done, :permission, :failed, :stale, or nil."
  (cond
   ((gethash ws claude-repl--thinking-workspaces)   :thinking)
   ((gethash ws claude-repl--permission-workspaces)  :permission)
   ((gethash ws claude-repl--failed-workspaces)      :failed)
   ((gethash ws claude-repl--done-workspaces)        :done)
   ((when-let ((last-time (gethash ws claude-repl--activity-times)))
      (< (- (float-time) last-time) (* claude-repl-stale-minutes 60)))
    :stale)))

(defun claude-repl--ws-set (ws state)
  "Set workspace WS to STATE, clearing any conflicting states.
STATE is one of: :thinking, :done, :permission, :failed."
  (unless ws (error "claude-repl--ws-set: ws is nil"))
  (claude-repl--log "state %s -> %s" ws state)
  (remhash ws claude-repl--thinking-workspaces)
  (remhash ws claude-repl--done-workspaces)
  (remhash ws claude-repl--permission-workspaces)
  (remhash ws claude-repl--failed-workspaces)
  (pcase state
    (:thinking   (puthash ws t claude-repl--thinking-workspaces))
    (:done       (puthash ws t claude-repl--done-workspaces))
    (:permission (puthash ws t claude-repl--permission-workspaces))
    (:failed     (puthash ws t claude-repl--failed-workspaces))))

(defun claude-repl--ws-clear (ws state)
  "Clear a single STATE for workspace WS.
STATE is one of: :thinking, :done, :permission, :failed."
  (unless ws (error "claude-repl--ws-clear: ws is nil"))
  (claude-repl--log "clear %s %s" ws state)
  (pcase state
    (:thinking   (remhash ws claude-repl--thinking-workspaces))
    (:done       (remhash ws claude-repl--done-workspaces))
    (:permission (remhash ws claude-repl--permission-workspaces))
    (:failed     (remhash ws claude-repl--failed-workspaces))))

(defun claude-repl--touch-activity (ws)
  "Record current time as last input for workspace WS."
  (unless ws (error "claude-repl--touch-activity: ws is nil"))
  (puthash ws (float-time) claude-repl--activity-times))

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

(defface claude-repl-tab-failed
  '((t :background "#1a7a1a" :foreground "black" :weight bold))
  "Face for workspace tabs where Claude failed to start thinking (green + ❌).")

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
                                   (:failed     'claude-repl-tab-failed)
                                   (:thinking   'claude-repl-tab-thinking)
                                   (:stale      'claude-repl-tab-stale)))
                     (label (pcase state
                              (:permission "❓")
                              (:failed "❌")
                              (_ num)))
                     (base-face (if (and selected (not claude-face))
                                    '+workspace-tab-selected-face
                                  '+workspace-tab-face))
                     (face (or claude-face base-face)))
                (propertize (format " [%s] %s " label name) 'face face)))
     " ")))

(advice-add '+workspace--tabline :override #'claude-repl--tabline-advice)

;; Periodically refresh tabline so stale indicators appear on time.
(run-with-timer 0.5 60
                (lambda ()
                  (when (bound-and-true-p persp-mode)
                    (+workspace/display))))

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
  :ws          - the workspace name for the current buffer, or nil"
  (let* ((thinking (claude-repl--title-has-spinner-p title))
         (ws (claude-repl--workspace-for-buffer (current-buffer)))
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
  (claude-repl--maybe-notify-finished ws))

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
      (when ws
        (when transition
          (claude-repl--log "title transition=%s ws=%s" transition ws))
        (pcase transition
          ('started  (claude-repl--ws-set ws :thinking))
          ('finished (claude-repl--ws-clear ws :thinking)))
        (when (eq transition 'finished)
          (claude-repl--on-claude-finished ws))))))

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
        (when ws
          (claude-repl--ws-set ws :permission))
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
Works from any buffer (loads session) or from within the vterm buffer itself."
  (let ((buf (if (eq major-mode 'vterm-mode)
                 (current-buffer)
               (claude-repl--load-session)
               claude-repl-vterm-buffer)))
    (when (and buf (buffer-live-p buf))
      (with-current-buffer buf
        (when (eq major-mode 'vterm-mode)
          (claude-repl--do-refresh)))
      (claude-repl--fix-vterm-scroll buf))))

;; Refresh vterm on frame focus
(defun claude-repl--clear-done-if-visible ()
  "Clear :done state for the current vterm's workspace if the vterm is visible."
  (when (and (claude-repl--vterm-live-p) (claude-repl--vterm-visible-p))
    (let ((ws (claude-repl--workspace-for-buffer claude-repl-vterm-buffer)))
      (when ws
        (claude-repl--log "clear-done-if-visible ws=%s" ws)
        (claude-repl--ws-clear ws :done)))))

(defun claude-repl--on-frame-focus ()
  "Refresh claude vterm and clear done indicator when Emacs regains focus."
  (when (frame-focus-state)
    (claude-repl--log "on-frame-focus")
    (claude-repl--load-session)
    (claude-repl--refresh-vterm)
    (claude-repl--clear-done-if-visible)))

(add-function :after after-focus-change-function #'claude-repl--on-frame-focus)

;; Refresh vterm on workspace switch
(defun claude-repl--on-workspace-switch ()
  "Handle workspace switch: clear done state, refresh vterm, reset cursors."
  (claude-repl--load-session)
  (claude-repl--log "workspace-switch ws=%s" (+workspace-current-name))
  (claude-repl--clear-done-if-visible)
  (claude-repl--refresh-vterm)
  (claude-repl--reset-vterm-cursors))

(when (modulep! :ui workspaces)
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
        (claude-repl--load-session)
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
  "Return t if Claude vterm buffer exists with a live process."
  (and (claude-repl--vterm-live-p)
       (get-buffer-process claude-repl-vterm-buffer)))

(defun claude-repl--input-visible-p ()
  "Return t if input buffer is visible in a window."
  (and claude-repl-input-buffer
       (buffer-live-p claude-repl-input-buffer)
       (get-buffer-window claude-repl-input-buffer)))

(defun claude-repl--vterm-visible-p ()
  "Return t if vterm buffer is visible in a window."
  (and (claude-repl--vterm-live-p)
       (get-buffer-window claude-repl-vterm-buffer)))

(defun claude-repl--panels-visible-p ()
  "Return t if both panels are visible."
  (and (claude-repl--input-visible-p)
       (claude-repl--vterm-visible-p)))

(defun claude-repl--hide-panels ()
  "Hide both Claude panels without killing buffers."
  (claude-repl--log "hide-panels")
  (when-let ((win (get-buffer-window claude-repl-input-buffer)))
    (delete-window win))
  (when-let ((win (get-buffer-window claude-repl-vterm-buffer)))
    (delete-window win)))

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
  (claude-repl--load-session)
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

(defun claude-repl--ensure-input-buffer ()
  "Create input buffer if needed, put in claude-input-mode."
  (claude-repl--log "ensure-input-buffer")
  (let ((root (claude-repl--resolve-root)))
    (setq claude-repl-input-buffer (get-buffer-create (claude-repl--buffer-name "-input")))
    (with-current-buffer claude-repl-input-buffer
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

(defun claude-repl--ensure-vterm-buffer ()
  "Create vterm buffer running claude if needed. Starts claude from the git root."
  (let* ((root (claude-repl--resolve-root))
         (default-directory root))
    (claude-repl--kill-stale-vterm)
    (setq claude-repl-vterm-buffer (get-buffer-create (claude-repl--buffer-name)))
    (with-current-buffer claude-repl-vterm-buffer
      (setq-local claude-repl--project-root root)
      (unless (eq major-mode 'vterm-mode)
        (vterm-mode)
        (setq-local truncate-lines nil)
        (setq-local word-wrap t)
        (claude-repl--set-buffer-background 15)
        (claude-repl--log "ensure-vterm created %s root=%s" (buffer-name claude-repl-vterm-buffer) root)
        (claude-repl--start-claude)))))

(defun claude-repl--show-panels-with-placeholder ()
  "Show panels using a loading placeholder in the vterm slot.
The placeholder is swapped for the real vterm buffer once Claude is ready."
  (claude-repl--log "show-panels-with-placeholder")
  (let ((real-vterm claude-repl-vterm-buffer)
        (placeholder (get-buffer-create " *claude-loading*")))
    (with-current-buffer placeholder
      (setq-local mode-line-format nil)
      (claude-repl--set-buffer-background 15))
    (setq claude-repl-vterm-buffer placeholder)
    (claude-repl--show-panels)
    (claude-repl--focus-input-panel)
    (setq claude-repl-vterm-buffer real-vterm)))

(defun claude-repl--start-fresh ()
  "Start a new Claude session with placeholder panels."
  (claude-repl--log "start-fresh")
  (setq claude-repl--saved-window-config (current-window-configuration))
  (delete-other-windows (claude-repl--live-return-window))
  (claude-repl--ensure-session)
  (claude-repl--show-panels-with-placeholder)
  (claude-repl--save-session)
  (message "Starting Claude..."))

(defun claude-repl--show-existing-panels ()
  "Show panels for an already-running Claude session.
Clears done state, refreshes display, and restores panel layout."
  (claude-repl--log "show-existing-panels")
  (claude-repl--refresh-vterm)
  (claude-repl--clear-done-if-visible)
  (setq claude-repl--saved-window-config (current-window-configuration))
  (delete-other-windows (claude-repl--live-return-window))
  (claude-repl--ensure-input-buffer)
  (claude-repl--show-panels)
  (claude-repl--focus-input-panel)
  (claude-repl--update-hide-overlay)
  (claude-repl--save-session))

;; Entry point - smart toggle
(defun claude-repl ()
  "Toggle Claude REPL panels.
If text is selected: send it directly to Claude.
If not running: start Claude and show both panels.
If panels visible: hide both panels.
If panels hidden: show both panels."
  (interactive)
  (claude-repl--load-session)
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
      (claude-repl--send-to-claude selection)
      (claude-repl--save-session))
     ;; Nothing running - start fresh with placeholder until Claude is ready
     ((not vterm-running)
      (claude-repl--start-fresh))
     ;; Panels visible - hide both, restore window layout
     (panels-visible
      (claude-repl--restore-layout)
      (claude-repl--save-session))
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

(defun claude-repl--teardown-session-state ()
  "Save history, disable overlay, cancel timers, and nil out session globals."
  (claude-repl--log "teardown-session-state")
  (ignore-errors (claude-repl--history-save))
  (ignore-errors (claude-repl--disable-hide-overlay))
  (when claude-repl--sync-timer
    (cancel-timer claude-repl--sync-timer)
    (setq claude-repl--sync-timer nil))
  (setq claude-repl-vterm-buffer nil
        claude-repl-input-buffer nil
        claude-repl--saved-window-config nil)
  (claude-repl--save-session))

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
  (claude-repl--load-session)
  (let ((vterm-buf claude-repl-vterm-buffer)
        (input-buf claude-repl-input-buffer))
    (claude-repl--teardown-session-state)
    (claude-repl--destroy-session-buffers vterm-buf input-buf)))

(defun claude-repl-restart ()
  "Kill Claude REPL and restart with `claude -c` to continue session."
  (interactive)
  (claude-repl--log "restart")
  (claude-repl--load-session)
  (claude-repl-kill)
  (claude-repl--ensure-vterm-buffer)
  (claude-repl--ensure-input-buffer)
  (claude-repl--enable-hide-overlay)
  (claude-repl--show-panels)
  (claude-repl--focus-input-panel)
  (claude-repl--save-session))

(defun claude-repl-focus-input ()
  "Focus the Claude input buffer, or return to previous window if already there.
If Claude isn't running, start it (same as `claude-repl')."
  (interactive)
  (claude-repl--load-session)
  (cond
   ;; Already in the input buffer — jump back
   ((eq (current-buffer) claude-repl-input-buffer)
    (claude-repl--log "focus-input branch=jump-back")
    (if (and claude-repl-return-window (window-live-p claude-repl-return-window))
        (select-window claude-repl-return-window)
      (evil-window-left 1)))
   ;; Not running — start fresh
   ((not (claude-repl--vterm-running-p))
    (claude-repl--log "focus-input branch=start-fresh")
    (claude-repl))
   ;; Running but panels hidden — show them
   (t
    (claude-repl--log "focus-input branch=show-or-focus")
    (unless (claude-repl--panels-visible-p)
      (setq claude-repl--saved-window-config (current-window-configuration))
      (claude-repl--ensure-input-buffer)
      (claude-repl--show-panels)
      (claude-repl--save-session))
    (setq claude-repl-return-window (selected-window))
    (when-let ((win (get-buffer-window claude-repl-input-buffer)))
      (select-window win)
      (when (bound-and-true-p evil-mode)
        (evil-insert-state))))))

(defun claude-repl-toggle-fullscreen ()
  "Toggle the Claude vterm buffer fullscreen.
If already fullscreen, restore the previous window layout."
  (interactive)
  (claude-repl--log "toggle-fullscreen currently=%s" (if claude-repl--fullscreen-config "fullscreen" "normal"))
  (claude-repl--load-session)
  (cond
   ;; Already fullscreen — restore
   (claude-repl--fullscreen-config
    (set-window-configuration claude-repl--fullscreen-config)
    (setq claude-repl--fullscreen-config nil))
   ;; Not fullscreen — go fullscreen if vterm exists
   ((claude-repl--vterm-live-p)
    (setq claude-repl--fullscreen-config (current-window-configuration))
    (delete-other-windows)
    (set-window-dedicated-p (selected-window) nil)
    (switch-to-buffer claude-repl-vterm-buffer)
    (claude-repl--reset-vterm-cursors))
   (t (message "No Claude vterm buffer for this workspace."))))

(defun claude-repl-cycle ()
  "Send backtab to Claude vterm to cycle through options."
  (interactive)
  (claude-repl--log "cycle")
  (claude-repl--load-session)
  (when (claude-repl--vterm-live-p)
    (with-current-buffer claude-repl-vterm-buffer
      (vterm-send-key "<backtab>"))))

;; Global bindings
(map! :nvi "C-S-m" #'claude-repl-cycle)
(map! :i "C-S-f" #'claude-repl-toggle-fullscreen)
(map! :leader :prefix "w" :n "c" #'claude-repl-toggle-fullscreen)

;; Keybindings
(map! :leader
      :desc "Claude REPL" "o c" #'claude-repl
      :desc "Claude input" "o v" #'claude-repl-focus-input
      :desc "Kill Claude" "o C" #'claude-repl-kill
      :desc "Claude explain" "o e" #'claude-repl-explain
      :desc "Claude interrupt" "o i" #'claude-repl-interrupt)

(dotimes (i 10)
  (let ((char (number-to-string i)))
    (define-key doom-leader-map (kbd (format "o %s" char))
      (lambda () (interactive) (claude-repl-send-char char)))))

;; FIXME: fix magit handling

;; Exclude claude-repl buffers from Doom's buffer switching (SPC b b, SPC b p, etc.)
(defun claude-repl--doom-unreal-buffer-p (buf)
  "Return non-nil if BUF is a claude-repl buffer."
  (string-match-p "^\\*claude-\\(input-\\)?[0-9a-f]+\\*$" (buffer-name buf)))

(add-hook 'doom-unreal-buffer-functions #'claude-repl--doom-unreal-buffer-p)

;; C-v paste forwarding to vterm
(defun claude-repl-paste-to-vterm ()
  "Forward a Ctrl-V keystroke to the Claude vterm buffer.
This lets Claude CLI handle paste natively, including images."
  (interactive)
  (claude-repl--load-session)
  (when (claude-repl--vterm-live-p)
    (with-current-buffer claude-repl-vterm-buffer
      (vterm-send-key "v" nil nil t))))

(provide 'claude-repl)
;;; config.el ends here
