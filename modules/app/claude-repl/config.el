;;; app/claude-repl/config.el -*- lexical-binding: t; -*-

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

;; Transient globals — always set by `claude-repl--load-session' before use.
;; No defvar needed; these are just scratch space swapped per-project.
;; claude-repl-vterm-buffer, claude-repl-input-buffer, claude-repl-return-window,
;; claude-repl--saved-window-config

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

(setq claude-repl-hide-input-box nil)

(defvar claude-repl-skip-permissions t
  "When non-nil, start Claude with --dangerously-skip-permissions and prepend the command prefix metaprompt.")

(defvar claude-repl-prefix-period 5
  "Number of prompts between metaprompt prefix injections.
The prefix is sent on the first prompt and every Nth prompt thereafter.")

(defvar claude-repl--prefix-counter 0
  "Counts prompts since the last prefix injection.")

(defvar claude-repl-command-prefix "DO NOT run any mutating git commands (push, reset, checkout, etc) without EXPLICIT PERMISSION from ME. Do not INSTALL or UNINSTALL anything without my EXPLICIT PERMISSION. Do not operate on any files OUTSIDE OF PROJECT without MY EXPLICIT PERMISSION. Do not take any actions unless it FOLLOWS DIRECTLY from an action EXPLICITLY REQUESTED in the following prompt "
  "When non-nil, this string is prepended (with a newline) before every input sent to Claude.")

(defvar claude-repl--command-prefix (format "<<*this is a metaprompt. I will periodically prefix my prompts with this to remind you of our restrictions for freely making changes. Do not be alarmed, this is merely a periodic reminder*: %s *metaprompt over* (rest is actual user request that you should respond to directly)>>\n\n" claude-repl-command-prefix)
  "Formatted metaprompt string prepended before every input when `claude-repl-skip-permissions' is non-nil.")


;; Set to t once Claude has set its terminal title (meaning it's ready).
(defvar-local claude-repl--ready nil
  "Non-nil once Claude Code has finished starting up.")

;; Per-project session storage
(setq claude-repl--sessions (or (bound-and-true-p claude-repl--sessions)
                                (make-hash-table :test 'equal)))

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
          claude-repl-return-window (plist-get session :return-window))))

(defun claude-repl--save-session ()
  "Save the current global state back to the sessions hash table."
  (let ((id (claude-repl--workspace-id)))
    (puthash id
             (list :saved-window-config claude-repl--saved-window-config
                   :return-window claude-repl-return-window)
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

;; Manual window layout: vterm on the right (full height), input below vterm.
(defun claude-repl--show-panels ()
  "Display vterm and input panels to the right of the current window.
Splits right for vterm (55% of frame), then splits vterm bottom for input (30%)."
  (let* ((work-win (or claude-repl-return-window (selected-window)))
         (vterm-win (split-window work-win (round (* 0.6 (window-total-width work-win))) 'right))
         (input-win (split-window vterm-win (round (* -0.15 (window-total-height vterm-win))) 'below)))
    (claude-repl--refresh-vterm)
    (set-window-buffer vterm-win claude-repl-vterm-buffer)
    (set-window-buffer input-win claude-repl-input-buffer)
    (set-window-dedicated-p vterm-win t)
    (set-window-dedicated-p input-win t)
    ;; Lock width to prevent resize-triggered reflow in vterm
    (set-window-parameter vterm-win 'window-size-fixed 'width)
    (select-window input-win)
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

(map! :map claude-input-mode-map
      :ni "RET"       #'claude-repl-send
      :ni "S-RET"     #'newline
      :ni "C-RET"     #'claude-repl-send-and-hide
      :ni "C-c C-k"   #'claude-repl-interrupt
      :ni "C-c C-c"   (cmd! (claude-repl--history-push) (erase-buffer) (evil-insert-state))
      :ni "C-c y"     (cmd! (claude-repl-send-char "y"))
      :ni "C-c n"     (cmd! (claude-repl-send-char "n"))
      :ni "C-c r"     #'claude-repl-restart
      :ni "C-c q"     #'claude-repl-kill
      :ni "C-S-m"     #'claude-repl-cycle
      :ni "C-h"       #'evil-window-left
      :n  "C-n"       (cmd! (claude-repl--load-session)
                            (when (claude-repl--vterm-live-p)
                              (with-current-buffer claude-repl-vterm-buffer (vterm-send-down))))
      :n  "C-p"       (cmd! (claude-repl--load-session)
                            (when (claude-repl--vterm-live-p)
                              (with-current-buffer claude-repl-vterm-buffer (vterm-send-up))))
      :ni "<up>"        #'claude-repl--history-prev
      :ni "<down>"      #'claude-repl--history-next)

;; C-S-0 through C-S-9: send digit to Claude
(dotimes (i 10)
  (let ((char (number-to-string i)))
    (map! :map claude-input-mode-map
          :ni (format "C-S-%s" char) (cmd! (claude-repl-send-char char)))))

;; Input history persistence
(defun claude-repl--history-file ()
  "Return the path to the history file for the current project."
  (expand-file-name ".claude-repl-history" (claude-repl--resolve-root)))

(defun claude-repl--history-save ()
  "Write input history to disk."
  (when claude-repl-input-buffer
    (let ((history (buffer-local-value 'claude-repl--input-history claude-repl-input-buffer))
          (file (claude-repl--history-file)))
      (when history
        (with-temp-file file
          (prin1 history (current-buffer)))))))

(defun claude-repl--history-restore ()
  "Load input history from disk into the current buffer."
  (let ((file (claude-repl--history-file)))
    (when (file-exists-p file)
      (setq claude-repl--input-history
            (with-temp-buffer
              (insert-file-contents file)
              (read (current-buffer)))))))

;; Input history functions
(defun claude-repl--history-push ()
  "Save current input buffer text to history.
Skips empty strings and duplicates of the most recent entry.
Resets history browsing index."
  (let ((text (string-trim (buffer-string))))
    (unless (string-empty-p text)
      (unless (equal text (car claude-repl--input-history))
        (push text claude-repl--input-history))))
  (setq claude-repl--history-index -1))

(defun claude-repl--history-prev ()
  "Navigate to the previous (older) history entry."
  (interactive)
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
    (setq claude-repl--history-index -1)))

;; Core functions
(defun claude-repl--prepare-input ()
  "Read and return input from the input buffer.
Periodically prepends the metaprompt prefix when `claude-repl-skip-permissions' is on."
  (let ((raw (with-current-buffer claude-repl-input-buffer
               (buffer-string))))
    (prog1
        (if (and claude-repl-skip-permissions claude-repl-command-prefix
                 (zerop (mod claude-repl--prefix-counter claude-repl-prefix-period)))
            (concat claude-repl--command-prefix raw)
          raw)
      (setq claude-repl--prefix-counter (1+ claude-repl--prefix-counter)))))

(defun claude-repl--schedule-failed-check (ws)
  "Schedule a 5s check: if WS is still :thinking without a title spinner, mark :failed."
  (let ((check-ws ws)
        (check-buf claude-repl-vterm-buffer))
    (run-at-time 5 nil
                 (lambda ()
                   (when (and (buffer-live-p check-buf)
                              (eq (claude-repl--ws-state check-ws) :thinking)
                              (not claude-repl--title-thinking))
                     (claude-repl--ws-set check-ws :failed))))))

(defun claude-repl--send-input-to-vterm (input)
  "Send INPUT string to the Claude vterm buffer.
Uses paste mode for large inputs to avoid truncation."
  (with-current-buffer claude-repl-vterm-buffer
    (if (> (length input) 200)
        (let ((buf (current-buffer)))
          (vterm-send-string input t)
          (run-at-time 0.1 nil
                       (lambda ()
                         (when (buffer-live-p buf)
                           (with-current-buffer buf
                             (vterm-send-return)
                             (claude-repl--refresh-vterm))))))
      (vterm-send-string input)
      (vterm-send-return)
      (claude-repl--refresh-vterm))))

(defun claude-repl-send ()
  "Send input buffer contents to Claude, clear buffer, and return to previous window."
  (interactive)
  (claude-repl--load-session)
  (when (claude-repl--vterm-live-p)
    (let ((ws (+workspace-current-name))
          (input (claude-repl--prepare-input)))
      (unless ws (error "claude-repl-send: no active workspace"))
      (claude-repl--ws-set ws :thinking)
      (claude-repl--touch-activity ws)
      (claude-repl--schedule-failed-check ws)
      (claude-repl--send-input-to-vterm input)
      (with-current-buffer claude-repl-input-buffer
        (claude-repl--history-push)
        (erase-buffer))
      (when (and claude-repl-return-window (window-live-p claude-repl-return-window))
        (select-window claude-repl-return-window)))))

(defun claude-repl--restore-layout ()
  "Restore the window layout from before panels were shown.
Falls back to hiding panels and selecting the return window."
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
  (claude-repl--load-session)
  (claude-repl-send)
  (claude-repl--restore-layout)
  (claude-repl--save-session))

(defun claude-repl-send-char (char)
  "Send a single character to Claude."
  (claude-repl--load-session)
  (when (claude-repl--vterm-live-p)
    (with-current-buffer claude-repl-vterm-buffer
      (vterm-send-string char)
      (vterm-send-return))))

(defun claude-repl--send-to-claude (text)
  "Send TEXT to Claude, starting it if needed."
  (claude-repl--load-session)
  (unless (claude-repl--vterm-running-p)
    (claude-repl--ensure-vterm-buffer)
    (claude-repl--ensure-input-buffer)
    (claude-repl--enable-hide-overlay))
  (with-current-buffer claude-repl-vterm-buffer
    (vterm-send-string text)
    (vterm-send-return))
  (claude-repl--save-session))

(defun claude-repl--rel-path ()
  "Return the current file path relative to the project root."
  (file-relative-name (buffer-file-name) (claude-repl--resolve-root)))

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
    (claude-repl--send-to-claude msg)))

(defun claude-repl-interrupt ()
  "Send Escape to interrupt Claude."
  (interactive)
  (claude-repl--load-session)
  (when (claude-repl--vterm-live-p)
    (with-current-buffer claude-repl-vterm-buffer
      (vterm-send-key "<escape>")
      (vterm-send-key "<escape>"))))

;; Hide overlay functions
(defun claude-repl--update-hide-overlay ()
  "Update overlay to hide bottom lines of Claude vterm buffer."
  (when (claude-repl--vterm-live-p)
    (with-current-buffer claude-repl-vterm-buffer
      ;; Remove old overlay if it exists
      (when (and claude-repl-hide-overlay
                 (overlay-buffer claude-repl-hide-overlay))
        (delete-overlay claude-repl-hide-overlay))
      ;; Only create new overlay if enabled and buffer has content
      (when (and claude-repl-hide-input-box (> (point-max) 1))
        (let* ((end (point-max))
               (start (save-excursion
                        (goto-char end)
                        (forward-line -4)
                        (line-beginning-position))))
          (when (< start end)
            (setq claude-repl-hide-overlay (make-overlay start end nil t nil))
            (overlay-put claude-repl-hide-overlay 'display "")
            (overlay-put claude-repl-hide-overlay 'evaporate t)))))))

(defun claude-repl-toggle-hide-input-box ()
  "Toggle hiding of Claude CLI's input box in the vterm buffer."
  (interactive)
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

(setq claude-repl--notify-fn
      (if (executable-find "terminal-notifier")
          #'claude-repl--notify-terminal-notifier
        #'claude-repl--notify-osascript))

(defun claude-repl--notify (title message)
  "Send a desktop notification with TITLE and MESSAGE."
  (funcall claude-repl--notify-fn title message))

;; Workspace tab indicator for Claude status
(setq claude-repl--done-workspaces (or (bound-and-true-p claude-repl--done-workspaces)
                                       (make-hash-table :test 'equal)))
(setq claude-repl--thinking-workspaces (or (bound-and-true-p claude-repl--thinking-workspaces)
                                           (make-hash-table :test 'equal)))
(setq claude-repl--permission-workspaces (or (bound-and-true-p claude-repl--permission-workspaces)
                                             (make-hash-table :test 'equal)))
(setq claude-repl--activity-times (or (bound-and-true-p claude-repl--activity-times)
                                      (make-hash-table :test 'equal)))
(setq claude-repl--failed-workspaces (or (bound-and-true-p claude-repl--failed-workspaces)
                                         (make-hash-table :test 'equal)))

(defvar claude-repl-stale-minutes 60
  "Minutes after last input before a workspace tab stops showing as stale.")

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
(setq claude-repl--title-thinking nil)

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

(defun claude-repl--on-claude-finished (ws)
  "Handle Claude finishing work in workspace WS.
Sets done state if buffer is hidden, refreshes display, sends notification."
  (unless (get-buffer-window (current-buffer) t)
    (claude-repl--ws-set ws :done))
  (claude-repl--refresh-vterm)
  (claude-repl--update-hide-overlay)
  (unless (frame-focus-state)
    (run-at-time 0.1 nil #'claude-repl--notify "Claude REPL"
                 (format "%s: Claude ready" ws))))

(defun claude-repl--on-title-change (title)
  "Detect thinking->idle transition from vterm title changes.
On first title change, reveal panels (Claude is ready)."
  (when (claude-repl--claude-buffer-p)
    (unless claude-repl--ready
      (setq claude-repl--ready t)
      (claude-repl--swap-placeholder))
    (let* ((info (claude-repl--detect-title-transition title))
           (thinking (plist-get info :thinking))
           (transition (plist-get info :transition))
           (ws (plist-get info :ws)))
      (unless ws (error "claude-repl--on-title-change: no workspace for buffer %s" (buffer-name)))
      (pcase transition
        ('started  (claude-repl--ws-set ws :thinking))
        ('finished (claude-repl--ws-clear ws :thinking)))
      (when (eq transition 'finished)
        (claude-repl--on-claude-finished ws))
      (setq claude-repl--title-thinking thinking))))

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
  (when vterm--term
    (vterm--redraw vterm--term))
  (redisplay t))

(defun claude-repl--refresh-vterm ()
  "Refresh the claude vterm display.
Works from any buffer (loads session) or from within the vterm buffer itself."
  (let ((buf (if (eq major-mode 'vterm-mode)
                 (current-buffer)
               (claude-repl--load-session)
               claude-repl-vterm-buffer)))
    (when (and buf (buffer-live-p buf))
      (let ((vterm-win (get-buffer-window buf))
            (orig-win (selected-window)))
        (with-current-buffer buf
          (when (eq major-mode 'vterm-mode)
            (claude-repl--do-refresh)))
        ;; Briefly select the vterm window to fix scroll position
        (when (and vterm-win (not (eq vterm-win orig-win)))
          (select-window vterm-win 'norecord)
          (select-window orig-win 'norecord))))))

;; Refresh vterm on frame focus
(defun claude-repl--clear-done-if-visible ()
  "Clear :done state for the current vterm's workspace if the vterm is visible."
  (when (and (claude-repl--vterm-live-p) (claude-repl--vterm-visible-p))
    (let ((ws (claude-repl--workspace-for-buffer claude-repl-vterm-buffer)))
      (when ws (claude-repl--ws-clear ws :done)))))

(defun claude-repl--on-frame-focus ()
  "Refresh claude vterm and clear done indicator when Emacs regains focus."
  (when (frame-focus-state)
    (claude-repl--load-session)
    (claude-repl--refresh-vterm)
    (claude-repl--clear-done-if-visible)))

(add-function :after after-focus-change-function #'claude-repl--on-frame-focus)

;; Refresh vterm on workspace switch
(defun claude-repl--on-workspace-switch ()
  "Handle workspace switch: clear done state, refresh vterm, reset cursors."
  (claude-repl--load-session)
  (claude-repl--clear-done-if-visible)
  (claude-repl--refresh-vterm)
  (claude-repl--reset-vterm-cursors))

(when (modulep! :ui workspaces)
  (add-hook 'persp-activated-functions
            (lambda (&rest _)
              (run-at-time 0 nil #'claude-repl--on-workspace-switch))))

(defun claude-repl--after-vterm-redraw (&rest _)
  "Apply hide overlay after vterm redraws."
  (when (claude-repl--claude-buffer-p)
    (claude-repl--load-session)
    (claude-repl--update-hide-overlay)))

(defun claude-repl--enable-hide-overlay ()
  "Enable the hide overlay advice."
  (advice-add 'vterm--redraw :after #'claude-repl--after-vterm-redraw))

(defun claude-repl--disable-hide-overlay ()
  "Disable the hide overlay advice and clean up."
  (advice-remove 'vterm--redraw #'claude-repl--after-vterm-redraw)
  (when (and claude-repl-hide-overlay
             (overlay-buffer claude-repl-hide-overlay))
    (delete-overlay claude-repl-hide-overlay))
  (setq claude-repl-hide-overlay nil))

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
  (when-let ((win (get-buffer-window claude-repl-input-buffer)))
    (delete-window win))
  (when-let ((win (get-buffer-window claude-repl-vterm-buffer)))
    (delete-window win)))

;; Auto-close orphaned panels: if one is closed, close the other.
;; Also refresh the hide overlay in case a window change invalidated it.
(defun claude-repl--sync-panels ()
  "Close any Claude panel whose partner is no longer visible."
  (dolist (win (window-list))
    (let ((name (buffer-name (window-buffer win))))
      (cond
       ;; Vterm visible, input missing -> close vterm
       ;; (but not during fullscreen or if it's the sole window)
       ((and (string-match "^\\*claude-\\([0-9a-f]+\\)\\*$" name)
             (not claude-repl--fullscreen-config)
             (not (one-window-p))
             (not (get-buffer-window
                   (format "*claude-input-%s*" (match-string 1 name)))))
        (delete-window win))
       ;; Input visible, vterm missing -> close input
       ;; (but not if placeholder is showing — Claude is still starting)
       ((and (string-match "^\\*claude-input-\\([0-9a-f]+\\)\\*$" name)
             (not (one-window-p))
             (not (get-buffer-window
                   (format "*claude-%s*" (match-string 1 name))))
             (not (get-buffer " *claude-loading*")))
        (delete-window win))))))

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

(setq claude-repl--sync-timer nil)

(defun claude-repl--on-window-change ()
  "Deferred handler for window configuration changes.
Syncs orphaned panels, refreshes overlay, and resets cursors."
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
            (select-window input-win)
            (when (bound-and-true-p evil-mode)
              (evil-insert-state))))))))

(add-hook 'window-selection-change-functions #'claude-repl--redirect-to-input)

(defun claude-repl--ensure-input-buffer ()
  "Create input buffer if needed, put in claude-input-mode."
  (let ((root (claude-repl--resolve-root)))
    (setq claude-repl-input-buffer (get-buffer-create (claude-repl--buffer-name "-input")))
    (with-current-buffer claude-repl-input-buffer
      (setq-local claude-repl--project-root root)
      (unless (eq major-mode 'claude-input-mode)
        (claude-input-mode)
        (claude-repl--history-restore)))))

(defun claude-repl--ensure-vterm-buffer ()
  "Create vterm buffer running claude if needed.
Kills any stale buffer (no live process) first. Starts claude from the git root."
  (let* ((root (claude-repl--resolve-root))
         (default-directory root)
         (existing (get-buffer (claude-repl--buffer-name))))
    ;; Kill stale buffer (exists but process died)
    (when (and existing (not (get-buffer-process existing)))
      (kill-buffer existing))
    (setq claude-repl-vterm-buffer (get-buffer-create (claude-repl--buffer-name)))
    (with-current-buffer claude-repl-vterm-buffer
      (setq-local claude-repl--project-root root)
      (unless (eq major-mode 'vterm-mode)
        (vterm-mode)
        (setq-local truncate-lines nil)
        (setq-local word-wrap t)
        (claude-repl--set-buffer-background 15)
        (claude-repl--start-claude)))))

(defun claude-repl--start-fresh ()
  "Start a new Claude session with placeholder panels.
Shows a loading placeholder in the output slot until Claude is ready."
  (setq claude-repl--saved-window-config (current-window-configuration))
  (delete-other-windows (or claude-repl-return-window (selected-window)))
  (claude-repl--ensure-vterm-buffer)
  (claude-repl--ensure-input-buffer)
  (claude-repl--enable-hide-overlay)
  (let ((real-vterm claude-repl-vterm-buffer)
        (placeholder (get-buffer-create " *claude-loading*")))
    (with-current-buffer placeholder
      (setq-local mode-line-format nil)
      (claude-repl--set-buffer-background 15))
    (setq claude-repl-vterm-buffer placeholder)
    (claude-repl--show-panels)
    (setq claude-repl-vterm-buffer real-vterm))
  (claude-repl--save-session)
  (message "Starting Claude..."))

(defun claude-repl--show-existing-panels ()
  "Show panels for an already-running Claude session.
Clears done state, refreshes display, and restores panel layout."
  (claude-repl--refresh-vterm)
  (claude-repl--clear-done-if-visible)
  (setq claude-repl--saved-window-config (current-window-configuration))
  (delete-other-windows (or claude-repl-return-window (selected-window)))
  (claude-repl--ensure-input-buffer)
  (claude-repl--show-panels)
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
  ;; Save current window to return to after hiding
  (unless (eq (current-buffer) claude-repl-input-buffer)
    (setq claude-repl-return-window (selected-window)))
  (let ((vterm-running (claude-repl--vterm-running-p))
        (panels-visible (claude-repl--panels-visible-p))
        (selection (when (use-region-p)
                    (buffer-substring-no-properties (region-beginning) (region-end)))))
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

(defun claude-repl--close-session-windows (&rest bufs)
  "Close windows displaying any of BUFS, plus the loading placeholder."
  (dolist (buf bufs)
    (when (and buf (buffer-live-p buf))
      (when-let ((win (get-buffer-window buf)))
        (ignore-errors (delete-window win)))))
  (when-let ((placeholder (get-buffer " *claude-loading*")))
    (when-let ((win (get-buffer-window placeholder)))
      (ignore-errors (delete-window win)))
    (kill-buffer placeholder)))

(defun claude-repl--kill-vterm-process (buf)
  "Kill the vterm process in BUF, with a delayed SIGKILL fallback."
  (when (and buf (buffer-live-p buf))
    (let ((proc (get-buffer-process buf)))
      (when proc
        (set-process-query-on-exit-flag proc nil))
      (kill-buffer buf)
      (when proc
        (run-at-time 0.5 nil
                     (lambda ()
                       (when (process-live-p proc)
                         (signal-process proc 'SIGKILL))))))))

(defun claude-repl-kill ()
  "Kill Claude REPL buffers and windows for the current workspace."
  (interactive)
  (claude-repl--load-session)
  (let ((vterm-buf claude-repl-vterm-buffer)
        (input-buf claude-repl-input-buffer))
    (ignore-errors (claude-repl--history-save))
    (ignore-errors (claude-repl--disable-hide-overlay))
    (when claude-repl--sync-timer
      (cancel-timer claude-repl--sync-timer)
      (setq claude-repl--sync-timer nil))
    ;; Nil out globals before deleting windows (prevents sync hook from
    ;; operating on dead buffers)
    (setq claude-repl-vterm-buffer nil
          claude-repl-input-buffer nil
          claude-repl--saved-window-config nil)
    (claude-repl--save-session)
    (claude-repl--close-session-windows vterm-buf input-buf)
    (claude-repl--kill-vterm-process vterm-buf)
    (when (and input-buf (buffer-live-p input-buf))
      (kill-buffer input-buf))))

(defun claude-repl-restart ()
  "Kill Claude REPL and restart with `claude -c` to continue session."
  (interactive)
  (claude-repl--load-session)
  (claude-repl-kill)
  (claude-repl--ensure-vterm-buffer)
  (claude-repl--ensure-input-buffer)
  (claude-repl--enable-hide-overlay)
  (claude-repl--show-panels)
  (claude-repl--save-session))

(defun claude-repl-focus-input ()
  "Focus the Claude input buffer, or return to previous window if already there.
If Claude isn't running, start it (same as `claude-repl')."
  (interactive)
  (claude-repl--load-session)
  (cond
   ;; Already in the input buffer — jump back
   ((eq (current-buffer) claude-repl-input-buffer)
    (if (and claude-repl-return-window (window-live-p claude-repl-return-window))
        (select-window claude-repl-return-window)
      (evil-window-left 1)))
   ;; Not running — start fresh
   ((not (claude-repl--vterm-running-p))
    (claude-repl))
   ;; Running but panels hidden — show them
   (t
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

(defvar claude-repl--fullscreen-config nil
  "Saved window configuration before fullscreen toggle.")

(defun claude-repl-toggle-fullscreen ()
  "Toggle the Claude vterm buffer fullscreen.
If already fullscreen, restore the previous window layout."
  (interactive)
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
    (map! :leader
          :desc (format "Send %s to Claude" char)
          (format "o %s" char)
          (cmd! (claude-repl-send-char char)))))
