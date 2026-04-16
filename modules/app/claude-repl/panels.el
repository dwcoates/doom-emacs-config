;;; panels.el --- panel/window management and entry point -*- lexical-binding: t; -*-

;;; Code:

;;;; Panel visibility predicates

(defun claude-repl--ws-buffer-visible-p (key)
  "Return non-nil if the buffer stored at KEY in current workspace is visible."
  (let* ((buf (claude-repl--ws-get (+workspace-current-name) key))
         (result (and buf (buffer-live-p buf) (get-buffer-window buf))))
    (claude-repl--log-verbose nil "ws-buffer-visible-p: key=%s result=%s" key (if result "visible" "hidden"))
    result))

(defun claude-repl--input-visible-p ()
  "Return t if input buffer for the current workspace is visible in a window."
  (claude-repl--ws-buffer-visible-p :input-buffer))

(defun claude-repl--vterm-visible-p ()
  "Return t if vterm buffer for the current workspace is visible in a window."
  (claude-repl--ws-buffer-visible-p :vterm-buffer))

(defun claude-repl--panels-visible-p ()
  "Return t if both panels are visible."
  (let ((result (and (claude-repl--input-visible-p)
                     (claude-repl--vterm-visible-p))))
    (claude-repl--log-verbose nil "panels-visible-p: result=%s" (if result "visible" "hidden"))
    result))

;;;; Panel display and hide

(defun claude-repl--safe-buffer-name (b)
  "Return the name of buffer B if non-nil, otherwise nil."
  (and b (buffer-name b)))

(defun claude-repl--close-buffer-window (buf)
  "Close the window displaying BUF, ignoring errors."
  (when-let ((win (get-buffer-window buf)))
    (ignore-errors (delete-window win))))

(defun claude-repl--close-buffer-windows (&rest bufs)
  "Close windows displaying any of BUFS."
  (claude-repl--log nil "close-buffer-windows %s" (mapcar #'claude-repl--safe-buffer-name bufs))
  (dolist (buf bufs)
    (when (and buf (buffer-live-p buf))
      (claude-repl--close-buffer-window buf))))

(defun claude-repl--configure-vterm-window (win)
  "Configure WIN as a dedicated, navigation-skipped, width-locked vterm window.
Marks the window as dedicated, hides it from `other-window'/windmove so
keyboard navigation (C-l etc.) skips it, and locks its width to prevent
resize-triggered reflow in vterm."
  (claude-repl--log nil "configure-vterm-window: win=%s" win)
  (set-window-dedicated-p win t)
  (set-window-parameter win 'no-other-window t)
  (set-window-parameter win 'window-size-fixed 'width))

;; Manual window layout: vterm on the right (full height), input below vterm.
(defun claude-repl--show-panels ()
  "Display vterm and input panels to the right of the current window.
Splits right for vterm (60% width to work window), then splits vterm bottom for input (15%)."
  (let* ((ws (+workspace-current-name))
         (vterm-buf (claude-repl--ws-get ws :vterm-buffer))
         (input-buf (claude-repl--ws-get ws :input-buffer)))
    (claude-repl--log ws "show-panels vterm=%s input=%s"
                      (claude-repl--safe-buffer-name vterm-buf)
                      (claude-repl--safe-buffer-name input-buf))
    (let* ((work-win (selected-window))
           (vterm-win (split-window work-win (round (* 0.6 (window-total-width work-win))) 'right))
           (input-win (split-window vterm-win (round (* -0.15 (window-total-height vterm-win))) 'below)))
      (claude-repl--log ws "show-panels: work-win=%s vterm-win=%s input-win=%s" work-win vterm-win input-win)
      (claude-repl--refresh-vterm)
      (set-window-buffer vterm-win vterm-buf)
      (set-window-buffer input-win input-buf)
      (claude-repl--configure-vterm-window vterm-win)
      (set-window-dedicated-p input-win t)))
  (claude-repl--update-all-workspace-states))

(defun claude-repl--focus-input-panel ()
  "Focus the input panel window and enter insert state."
  (claude-repl--log nil "focus-input-panel")
  (when-let ((buf (claude-repl--ws-get (+workspace-current-name) :input-buffer))
             (win (get-buffer-window buf)))
    (select-window win)
    (evil-insert-state)))

(defun claude-repl--show-panels-and-focus ()
  "Display both Claude panels and focus the input panel.
Convenience wrapper combining `claude-repl--show-panels' and
`claude-repl--focus-input-panel'."
  (claude-repl--show-panels)
  (claude-repl--focus-input-panel))

;;;; Vterm refresh

(defun claude-repl--vterm-redraw ()
  "Redraw the current vterm buffer with read-only suppressed.
Assumes the current buffer is in vterm-mode."
  (claude-repl--log-verbose nil "vterm-redraw: buf=%s" (buffer-name))
  (let ((inhibit-read-only t))
    (when vterm--term
      (vterm--redraw vterm--term))))

(defun claude-repl--do-refresh ()
  "Low-level refresh of the current vterm buffer.
Must be called with a vterm-mode buffer current."
  (claude-repl--log-verbose nil "do-refresh: buf=%s" (buffer-name))
  (claude-repl--vterm-redraw)
  (redisplay t))

(defun claude-repl--fix-vterm-scroll (buf)
  "Briefly select the vterm window for BUF to fix Emacs scroll position."
  (let ((vterm-win (get-buffer-window buf))
        (orig-win (selected-window)))
    (if (and vterm-win (not (eq vterm-win orig-win)))
        (progn
          (claude-repl--log-verbose nil "fix-vterm-scroll: fixing scroll for buf=%s" (buffer-name buf))
          (select-window vterm-win 'norecord)
          (select-window orig-win 'norecord))
      (claude-repl--log-verbose nil "fix-vterm-scroll: skipped buf=%s vterm-win=%s same-win=%s"
                                (buffer-name buf) (if vterm-win "yes" "no")
                                (if (eq vterm-win orig-win) "yes" "no")))))

(defun claude-repl--resolve-vterm-buffer ()
  "Return the vterm buffer to refresh.
Uses the current buffer if it is in vterm-mode, otherwise looks up the
workspace's vterm buffer."
  (if (eq major-mode 'vterm-mode)
      (progn
        (claude-repl--log-verbose nil "resolve-vterm-buffer: path=vterm-mode buf=%s" (buffer-name))
        (current-buffer))
    (when-let ((ws (+workspace-current-name)))
      (let ((buf (claude-repl--ws-get ws :vterm-buffer)))
        (claude-repl--log-verbose nil "resolve-vterm-buffer: path=workspace-lookup ws=%s buf=%s"
                                  ws (claude-repl--safe-buffer-name buf))
        buf))))

(defun claude-repl--refresh-vterm ()
  "Refresh the claude vterm display.
Works from any buffer or from within the vterm buffer itself."
  (let ((buf (claude-repl--resolve-vterm-buffer)))
    (cond
     ((not buf)
      (claude-repl--log-verbose nil "refresh-vterm: no buffer found"))
     ((not (buffer-live-p buf))
      (claude-repl--log-verbose nil "refresh-vterm: buffer not live buf=%s" (buffer-name buf)))
     (t
      (with-current-buffer buf
        (if (eq major-mode 'vterm-mode)
            (claude-repl--do-refresh)
          (claude-repl--log-verbose nil "refresh-vterm: buf=%s not vterm-mode (mode=%s)"
                                    (buffer-name buf) major-mode)))
      (claude-repl--fix-vterm-scroll buf)))))

(defun claude-repl--mark-viewed (ws)
  "If WS is :inactive, re-activate to :done (without :viewed, so it persists).
If WS is :done, mark :viewed so the next update-ws-state can transition to :inactive."
  (pcase (claude-repl--ws-get ws :status)
    (:inactive
     (claude-repl--log ws "mark-viewed: ws=%s branch=inactive->done (no :viewed yet)" ws)
     (claude-repl--ws-set ws :done))
    (:done
     (claude-repl--log ws "mark-viewed: ws=%s branch=done->viewed" ws)
     (claude-repl--ws-put ws :viewed t))
    (_
     (claude-repl--log ws "mark-viewed: ws=%s branch=no-op status=%s"
                       ws (claude-repl--ws-get ws :status)))))

(defun claude-repl--drain-pending-show-panels (ws)
  "Open panels for WS if a preemptive prompt queued a :pending-show-panels flag.
Clears the flag and calls `claude-repl' to display the panels."
  (if (claude-repl--ws-get ws :pending-show-panels)
      (progn
        (claude-repl--log ws "drain-pending-show-panels: ws=%s branch=had-pending draining" ws)
        (claude-repl--ws-put ws :pending-show-panels nil)
        (claude-repl))
    (claude-repl--log ws "drain-pending-show-panels: ws=%s branch=no-pending no-op" ws)))

;; Refresh vterm on workspace switch
(defun claude-repl--on-workspace-switch ()
  "Handle workspace switch: update all workspace states, refresh vterm, reset cursors.
Also opens panels for workspaces that were created with a preemptive prompt.
Marks the switched-to workspace as :viewed so :done→:inactive can proceed.
If switching to an :inactive workspace, re-activates it to :done."
  (let ((ws (+workspace-current-name)))
    (claude-repl--log ws "workspace-switch ws=%s" ws)
    (when ws
      (claude-repl--mark-viewed ws))
    (claude-repl--update-all-workspace-states)
    (claude-repl--refresh-vterm)
    (claude-repl--reset-vterm-cursors)
    (claude-repl--drain-pending-show-panels ws)))

;; Save window state for current workspace before switching away,
;; so update-all-workspace-states can inspect the saved config.

(defun claude-repl--non-claude-panel-window-p (w)
  "Return non-nil if window W does not display a Claude panel buffer."
  (not (claude-repl--claude-panel-buffer-p (window-buffer w))))

(defun claude-repl--redirect-from-claude-before-save ()
  "If the selected window shows a Claude buffer, select a real window instead.
Called before persp saves window state so the saved config has a non-claude
buffer selected. This prevents Doom's `+workspace/kill' from trying to display
its fallback buffer in a dedicated window on workspace restore, which would
otherwise split the first real window (e.g. a repl) and show a doom buffer.
Skips redirect if claude is the only window (fullscreen case)."
  (when (claude-repl--claude-panel-buffer-p (window-buffer (selected-window)))
    (when-let ((target (cl-find-if
                        #'claude-repl--non-claude-panel-window-p
                        (window-list))))
      (select-window target))))

(defun claude-repl--before-persp-deactivate (&rest _)
  "Save window state before perspective deactivation.
Redirects away from Claude buffers and saves frame state."
  (claude-repl--log nil "before-persp-deactivate: entry")
  (claude-repl--redirect-from-claude-before-save)
  (ignore-errors (persp-frame-save-state)))

(defun claude-repl--after-persp-activated (&rest _)
  "Handle perspective activation by scheduling a workspace switch."
  (claude-repl--log nil "after-persp-activated: entry")
  (run-at-time 0 nil #'claude-repl--on-workspace-switch))

(when (modulep! :ui workspaces)
  (add-hook 'persp-before-deactivate-functions
            #'claude-repl--before-persp-deactivate)
  (add-hook 'persp-activated-functions
            #'claude-repl--after-persp-activated))

(defun claude-repl--hide-panels ()
  "Hide both Claude panels without killing buffers."
  (let* ((ws (+workspace-current-name))
         (input-buf (claude-repl--ws-get ws :input-buffer))
         (vterm-buf (claude-repl--ws-get ws :vterm-buffer)))
    (claude-repl--log ws "hide-panels")
    (claude-repl--close-buffer-windows input-buf vterm-buf)))

;;;; Window synchronization

;; Auto-close orphaned panels: if one is closed, close the other.
;; Also refresh the hide overlay in case a window change invalidated it.
(defun claude-repl--extract-panel-hex (name)
  "Extract the hex identifier from a Claude panel buffer NAME.
Returns the hex string, or nil if NAME is not a Claude panel buffer."
  (cond
   ((string-match-p claude-repl--vterm-buffer-re name)
    (substring name (length "*claude-") (- (length name) (length "*"))))
   ((string-match-p claude-repl--input-buffer-re name)
    (substring name (length "*claude-input-") (- (length name) (length "*"))))))

(defun claude-repl--partner-buffer-name (name hex)
  "Return the partner buffer name for Claude panel NAME with identifier HEX.
For a vterm buffer, the partner is the input buffer, and vice versa."
  (if (string-match-p claude-repl--vterm-buffer-re name)
      (format "*claude-input-%s*" hex)
    (format "*claude-%s*" hex)))

(defun claude-repl--orphaned-panel-p (name)
  "Return non-nil if NAME is a Claude panel buffer whose partner is not visible.
Ignores single-window frames.  Input buffers are not orphaned while the
loading placeholder exists (the vterm has not been swapped in yet)."
  (when-let ((hex (claude-repl--extract-panel-hex name)))
    (let ((partner (claude-repl--partner-buffer-name name hex))
          (result (and (not (one-window-p))
                       (not (get-buffer-window (claude-repl--partner-buffer-name name hex)))
                       ;; Input panels are not orphaned while loading placeholder is live
                       (or (string-match-p claude-repl--vterm-buffer-re name)
                           (not (get-buffer " *claude-loading*"))))))
      (when result
        (claude-repl--log-verbose nil "orphaned-panel-p: name=%s partner=%s is-orphaned" name partner))
      result)))

(defun claude-repl--sync-panels ()
  "Close any Claude panel whose partner is no longer visible."
  (claude-repl--log-verbose nil "sync-panels: entry windows=%d" (length (window-list)))
  (let ((orphan-count 0))
    (dolist (win (window-list))
      (let ((name (buffer-name (window-buffer win))))
        (when (claude-repl--orphaned-panel-p name)
          (setq orphan-count (1+ orphan-count))
          (claude-repl--log nil "sync-panels closing orphaned %s" name)
          (delete-window win))))
    (claude-repl--log-verbose nil "sync-panels: closed %d orphans" orphan-count)))

;; Keep visible Claude vterm buffers scrolled to the cursor.
;; Skips the selected window so clicking into vterm to read/copy isn't disrupted.
(defun claude-repl--refresh-vterm-window (win)
  "Refresh the Claude vterm buffer shown in WIN.
Resets cursor, redraws, and syncs window point."
  (let ((buf (window-buffer win)))
    (when (and buf (buffer-live-p buf) (claude-repl--claude-buffer-p buf))
      (claude-repl--log-verbose nil "refresh-vterm-window: win=%s buf=%s" win (buffer-name buf))
      (with-current-buffer buf
        (when (and (eq major-mode 'vterm-mode)
                   (fboundp 'vterm-reset-cursor-point))
          (condition-case nil
              (progn
                (vterm-reset-cursor-point)
                (claude-repl--vterm-redraw)
                (vterm-reset-cursor-point)
                (set-window-point win (point)))
            (end-of-buffer nil)))))))

(defun claude-repl--reset-vterm-cursors ()
  "Refresh every visible Claude vterm window except the selected one."
  (claude-repl--log-verbose nil "reset-vterm-cursors: entry")
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
  (claude-repl--log-verbose nil "on-window-change")
  (condition-case nil
      (claude-repl--sync-panels)
    (error nil))
  (claude-repl--update-hide-overlay)
  (claude-repl--reset-vterm-cursors))

(defmacro claude-repl--deferred (timer-var fn)
  "Return a lambda that debounces calls to FN via TIMER-VAR.
Cancels any pending timer and schedules FN to run at next idle."
  `(lambda (&rest _)
     (when ,timer-var
       (cancel-timer ,timer-var))
     (setq ,timer-var (run-at-time 0 nil ,fn))))

(defalias 'claude-repl--debounced-on-window-change
  (claude-repl--deferred claude-repl--sync-timer #'claude-repl--on-window-change)
  "Debounced handler for `window-configuration-change-hook'.
Cancels any pending timer and schedules `claude-repl--on-window-change'.")

(defalias 'claude-repl--debounced-cursor-reset
  (claude-repl--deferred claude-repl--cursor-reset-timer #'claude-repl--reset-vterm-cursors)
  "Debounced handler for cursor reset hooks.
Cancels any pending timer and schedules `claude-repl--reset-vterm-cursors'.")

(add-hook 'window-configuration-change-hook
          #'claude-repl--debounced-on-window-change)

(add-hook 'window-selection-change-functions #'claude-repl--debounced-cursor-reset)
(add-hook 'buffer-list-update-hook #'claude-repl--debounced-cursor-reset)

;; Redirect keyboard navigation away from the vterm output window.
;; Mouse clicks (checked via last-input-event) are allowed through so the
;; user can still click into the output when needed.
(defun claude-repl--bounce-from-vterm (_frame)
  "If the selected window is a no-other-window vterm, redirect to the input window.
Allows mouse-initiated selection through."
  (let ((win (selected-window)))
    (if (and (window-parameter win 'no-other-window)
             (not (mouse-event-p last-input-event)))
        (let* ((ws (+workspace-current-name))
               (input-buf (and ws (claude-repl--ws-get ws :input-buffer)))
               (input-win (and input-buf (get-buffer-window input-buf))))
          (if input-win
              (progn
                (claude-repl--log-verbose nil "bounce-from-vterm: bouncing to input-win=%s" input-win)
                (select-window input-win))
            (claude-repl--log-verbose nil "bounce-from-vterm: no input-win to bounce to")))
      (claude-repl--log-verbose nil "bounce-from-vterm: skipped no-other-window=%s mouse=%s"
                                (window-parameter win 'no-other-window)
                                (if (mouse-event-p last-input-event) "yes" "no")))))

(add-hook 'window-selection-change-functions #'claude-repl--bounce-from-vterm)

;;;; Buffer creation

(defun claude-repl--ensure-input-buffer (ws)
  "Create input buffer for workspace WS if needed, put in claude-input-mode."
  (claude-repl--log ws "ensure-input-buffer")
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
  (let ((existing (get-buffer (claude-repl--buffer-name))))
    (if (not existing)
        (claude-repl--log nil "kill-stale-vterm: no existing buffer")
      (if (get-buffer-process existing)
          (claude-repl--log nil "kill-stale-vterm: buf=%s has live process no-op" (buffer-name existing))
        (claude-repl--log nil "kill-stale-vterm: killing stale buf=%s" (buffer-name existing))
        (kill-buffer existing)))))

(defun claude-repl--initialize-new-vterm (ws root)
  "Set up a freshly created vterm buffer and start Claude.
WS is the workspace name.  ROOT is the project root directory.
Assumes the current buffer is the new vterm buffer that has not yet
entered vterm-mode."
  (vterm-mode)
  (setq-local truncate-lines nil)
  (setq-local word-wrap t)
  (claude-repl--set-buffer-background claude-repl--vterm-background-grey)
  (claude-repl--log ws "ensure-vterm created %s root=%s" (buffer-name) root)
  (claude-repl--start-claude))

(defun claude-repl--ensure-vterm-buffer (ws)
  "Create vterm buffer for workspace WS running claude if needed.
Starts claude from the git root.  Reuses an existing vterm buffer if one
is already in vterm-mode."
  (let* ((root (claude-repl--resolve-root))
         (default-directory root))
    (claude-repl--log ws "ensure-vterm-buffer ws=%s root=%s default-directory=%s" ws root default-directory)
    (claude-repl--kill-stale-vterm)
    (let ((vterm-buf (get-buffer-create (claude-repl--buffer-name))))
      (claude-repl--ws-put ws :vterm-buffer vterm-buf)
      (with-current-buffer vterm-buf
        (if (eq major-mode 'vterm-mode)
            (progn
              (claude-repl--log ws "ensure-vterm REUSING existing buffer %s for ws=%s (no --start-claude)" (buffer-name vterm-buf) ws))
          (claude-repl--initialize-new-vterm ws root))
        (setq-local claude-repl--project-root root)
        (setq-local claude-repl--owning-workspace ws)))))

;;;; Panel show/hide strategies

(defun claude-repl--show-loading-panels ()
  "Show panels using a loading placeholder in the vterm slot.
The placeholder is swapped for the real vterm buffer once Claude is ready."
  (let* ((ws (+workspace-current-name))
         (real-vterm (claude-repl--ws-get ws :vterm-buffer))
         (placeholder (get-buffer-create " *claude-loading*")))
    (claude-repl--log ws "show-loading-panels")
    (with-current-buffer placeholder
      (setq-local mode-line-format nil)
      (claude-repl--set-buffer-background claude-repl--vterm-background-grey))
    (claude-repl--ws-put ws :vterm-buffer placeholder)
    (claude-repl--show-panels-and-focus)
    (claude-repl--ws-put ws :vterm-buffer real-vterm)))

(defun claude-repl--start-fresh ()
  "Start a new Claude session with placeholder panels."
  (let ((ws (+workspace-current-name)))
    (claude-repl--log ws "start-fresh")
    (unless ws (error "claude-repl--start-fresh: no active workspace"))
    (delete-other-windows)
    (claude-repl--ensure-session)
    (claude-repl--show-loading-panels)
    (let ((start-cmd (claude-repl-instantiation-start-cmd (claude-repl--active-inst ws))))
      (message "Starting Claude... ws=%s ws-id=%s dir=%s cmd=%s"
               ws
               (claude-repl--workspace-id)
               (claude-repl--resolve-root)
               (or start-cmd "?")))))

(defun claude-repl--show-existing-panels ()
  "Show panels for an already-running Claude session.
Demotes indicators, refreshes display, and restores panel layout."
  (let ((ws (+workspace-current-name)))
    (claude-repl--log ws "show-existing-panels")
    (unless ws (error "claude-repl--show-existing-panels: no active workspace"))
    (claude-repl--refresh-vterm)
    (delete-other-windows)
    (claude-repl--ensure-input-buffer ws)
    (claude-repl--show-panels-and-focus)
    (claude-repl--update-hide-overlay)))

(defun claude-repl--show-hidden-panels ()
  "Restore hidden panels, re-activate :inactive workspaces, and mark :viewed."
  (let ((ws (+workspace-current-name)))
    (claude-repl--log ws "showing panels ws=%s state=%s (restoring status)"
                      ws (claude-repl--ws-state ws))
    (claude-repl--ws-put ws :panels-hidden nil)
    (claude-repl--mark-viewed ws))
  (claude-repl--show-existing-panels))

(defun claude-repl--hide-and-preserve-status ()
  "Hide Claude panels while preserving the current workspace status.
Marks :panels-hidden so the panels can be restored later without losing
the :thinking/:done/:permission state."
  (let ((ws (+workspace-current-name)))
    (unless ws (error "claude-repl--hide-and-preserve-status: no active workspace"))
    (claude-repl--log ws "hiding panels ws=%s state=%s (preserving status)"
                      ws (claude-repl--ws-state ws))
    (claude-repl--ws-put ws :panels-hidden t))
  (claude-repl--hide-panels))

;;;; Entry point

(defun claude-repl ()
  "Toggle Claude REPL panels.
If text is selected: send it directly to Claude.
If not running: start Claude and show both panels.
If panels visible: hide both panels.
If panels hidden: show both panels."
  (interactive)
  (let* ((ws (+workspace-current-name))
         (vterm-running (claude-repl--vterm-running-p))
         (session-starting (claude-repl--session-starting-p))
         (panels-visible (claude-repl--panels-visible-p))
         (selection (when (use-region-p)
                     (buffer-substring-no-properties (region-beginning) (region-end)))))
    (claude-repl--log ws "claude-repl running=%s starting=%s visible=%s selection=%s"
                      vterm-running session-starting panels-visible (if selection "yes" "no"))
    (cond
     ;; Text selected - send directly to Claude
     (selection
      (deactivate-mark)
      (claude-repl--send-to-claude selection))
     ;; Nothing running - start fresh with placeholder until Claude is ready
     ((not vterm-running)
      (claude-repl--start-fresh))
     ;; Vterm alive but Claude not yet ready - hold off, panels will open automatically
     (session-starting
      (message "Claude is loading…"))
     ;; Panels visible - hide both, preserve status
     (panels-visible
      (claude-repl--hide-and-preserve-status))
     ;; Panels hidden - show both
     (t
      (claude-repl--show-hidden-panels)))))

;;;; Session cleanup

(defun claude-repl--kill-placeholder ()
  "Close and kill the loading placeholder buffer if it exists."
  (claude-repl--log nil "kill-placeholder exists=%s" (if (get-buffer " *claude-loading*") "yes" "no"))
  (when-let ((placeholder (get-buffer " *claude-loading*")))
    (claude-repl--close-buffer-window placeholder)
    (kill-buffer placeholder)))

(defun claude-repl--sigkill-if-alive (proc)
  "Send SIGKILL to PROC if it is still alive."
  (when (process-live-p proc)
    (claude-repl--log nil "sigkill fallback for lingering process")
    (signal-process proc 'SIGKILL)))

(defun claude-repl--schedule-sigkill (proc)
  "Schedule a SIGKILL for PROC after 0.5s if it's still alive."
  (claude-repl--log nil "schedule-sigkill: scheduling for proc=%s" proc)
  (run-at-time 0.5 nil #'claude-repl--sigkill-if-alive proc))

(defun claude-repl--kill-vterm-process (buf)
  "Kill the vterm buffer BUF and its process."
  (claude-repl--log nil "kill-vterm-process buf=%s" (claude-repl--safe-buffer-name buf))
  (when (and buf (buffer-live-p buf))
    (let ((proc (get-buffer-process buf)))
      (when proc
        (set-process-query-on-exit-flag proc nil))
      (kill-buffer buf)
      (when proc
        (claude-repl--schedule-sigkill proc)))))

(defun claude-repl--teardown-session-state (ws)
  "Save history, disable overlay, cancel timers, and clear session state for workspace WS."
  (claude-repl--log ws "teardown-session-state ws=%s env=%s (setting had-session t)"
                    ws (claude-repl--ws-get ws :active-env))
  (ignore-errors (claude-repl--disable-hide-overlay))
  (when claude-repl--sync-timer
    (cancel-timer claude-repl--sync-timer)
    (setq claude-repl--sync-timer nil))
  ;; Update instantiation and persist state BEFORE clearing buffer refs,
  ;; since state-save needs the vterm buffer to resolve the project root.
  (let ((inst (claude-repl--active-inst ws)))
    (setf (claude-repl-instantiation-start-cmd inst) nil)
    (setf (claude-repl-instantiation-had-session inst) t))
  (claude-repl--state-save ws)
  (claude-repl--ws-put ws :vterm-buffer nil)
  (claude-repl--ws-put ws :input-buffer nil))

(defun claude-repl--destroy-session-buffers (vterm-buf input-buf)
  "Close windows and kill VTERM-BUF, INPUT-BUF, and any placeholder."
  (claude-repl--log nil "destroy-session-buffers")
  (claude-repl--close-buffer-windows vterm-buf input-buf)
  (claude-repl--kill-placeholder)
  (claude-repl--kill-vterm-process vterm-buf)
  (when (and input-buf (buffer-live-p input-buf))
    (kill-buffer input-buf)))

(defun claude-repl--kill-session (ws)
  "Cancel timers, tear down state, and destroy buffers for workspace WS.
Captures the current buffer references before teardown clears them."
  (claude-repl--log ws "kill-session: ws=%s" ws)
  (let ((vterm-buf (claude-repl--ws-get ws :vterm-buffer))
        (input-buf (claude-repl--ws-get ws :input-buffer)))
    (claude-repl--cancel-ready-timer ws)
    (claude-repl--teardown-session-state ws)
    (claude-repl--destroy-session-buffers vterm-buf input-buf)))

;;;; User commands

(defun claude-repl-kill ()
  "Kill Claude REPL buffers and windows for the current workspace."
  (interactive)
  (let ((ws (+workspace-current-name)))
    (claude-repl--log ws "kill")
    (unless ws (error "claude-repl-kill: no active workspace"))
    (claude-repl--ws-put ws :status nil)
    (claude-repl--ws-put ws :panels-hidden nil)
    (force-mode-line-update t)
    (claude-repl--kill-session ws)))

(defun claude-repl-restart ()
  "Kill Claude REPL and restart with `claude -c` to continue session."
  (interactive)
  (let ((ws (+workspace-current-name)))
    (claude-repl--log ws "restart")
    (claude-repl-kill)
    (claude-repl--ensure-vterm-buffer ws)
    (claude-repl--ensure-input-buffer ws)
    (claude-repl--enable-hide-overlay)
    (claude-repl--show-panels-and-focus)))

(defun claude-repl-focus-input ()
  "Focus the Claude input buffer, or return to previous window if already there.
If Claude isn't running, start it (same as `claude-repl')."
  (interactive)
  (let ((ws (+workspace-current-name)))
    (cond
     ;; Already in the input buffer — jump back
     ((eq (current-buffer) (claude-repl--ws-get ws :input-buffer))
      (claude-repl--log ws "focus-input branch=jump-back")
      (evil-window-left 1))
     ;; Not running — start fresh
     ((not (claude-repl--vterm-running-p))
      (claude-repl--log ws "focus-input branch=start-fresh")
      (claude-repl))
     ;; Running but panels hidden — show them
     (t
      (claude-repl--log ws "focus-input branch=show-or-focus")
      (unless (claude-repl--panels-visible-p)
        (claude-repl--ensure-input-buffer ws)
        (claude-repl--show-panels))
      (when-let ((win (get-buffer-window (claude-repl--ws-get ws :input-buffer))))
        (select-window win)
        (when (bound-and-true-p evil-mode)
          (evil-insert-state)))))))

(defun claude-repl--delete-non-panel-windows (vterm-buf input-buf)
  "Delete all windows not showing VTERM-BUF or INPUT-BUF."
  (claude-repl--log nil "delete-non-panel-windows: window-count=%d" (length (window-list)))
  (dolist (win (window-list))
    (unless (memq (window-buffer win) (list vterm-buf input-buf))
      (ignore-errors (delete-window win)))))

(defun claude-repl-toggle-fullscreen ()
  "Toggle fullscreen for the Claude REPL vterm and input windows.
Saves the current window configuration per-workspace and expands the
Claude panels to fill the frame.  Calling again restores the layout."
  (interactive)
  (let* ((ws (+workspace-current-name))
         (saved (claude-repl--ws-get ws :fullscreen-config)))
    (claude-repl--log ws "toggle-fullscreen ws=%s currently=%s" ws (if saved "fullscreen" "normal"))
    (cond
     ;; Already fullscreen — restore
     (saved
      (set-window-configuration saved)
      (claude-repl--ws-put ws :fullscreen-config nil))
     ;; Not fullscreen — go fullscreen if panels are visible
     ((claude-repl--vterm-live-p)
      (let* ((vterm-buf (claude-repl--ws-get ws :vterm-buffer))
             (input-buf (claude-repl--ws-get ws :input-buffer)))
        (unless (and vterm-buf input-buf
                     (get-buffer-window vterm-buf)
                     (get-buffer-window input-buf))
          (user-error "Claude REPL panels are not visible"))
        (claude-repl--ws-put ws :fullscreen-config (current-window-configuration))
        (claude-repl--delete-non-panel-windows vterm-buf input-buf)))
     (t (message "No Claude vterm buffer for this workspace.")))))

(defun claude-repl-cycle ()
  "Send backtab to Claude vterm to cycle through options."
  (interactive)
  (claude-repl--log nil "cycle")
  (when (claude-repl--vterm-live-p)
    (with-current-buffer (claude-repl--ws-get (+workspace-current-name) :vterm-buffer)
      (vterm-send-key "<backtab>"))))

(defun claude-repl--validate-env-switch (ws new-env worktree-p session-id)
  "Validate that workspace WS can switch to NEW-ENV.
WORKTREE-P and SESSION-ID describe the current workspace state.
Signals `user-error' if any precondition is not met."
  (claude-repl--log ws "validate-env-switch: ws=%s new-env=%s" ws new-env)
  (unless worktree-p
    (user-error "Sandbox switching requires a worktree workspace"))
  (unless session-id
    (user-error "No session ID captured yet — session may still be starting"))
  (when (claude-repl--ws-get ws :thinking)
    (user-error "Cannot switch environment while Claude is thinking"))
  (when (and (eq new-env :sandbox)
             (not (claude-repl--resolve-sandbox-config
                   (claude-repl--git-root (claude-repl--ws-get ws :project-dir)))))
    (user-error "No sandbox configuration found for this workspace")))

(defun claude-repl--seed-new-env-session (ws new-env session-id)
  "Ensure the NEW-ENV instantiation for WS has a session-id.
If this is the first switch, seeds the new environment's session-id
from SESSION-ID so --resume carries the conversation across."
  (let ((new-inst (or (claude-repl--ws-get ws new-env)
                      (make-claude-repl-instantiation))))
    (if (claude-repl-instantiation-session-id new-inst)
        (claude-repl--log ws "seed-new-env-session: ws=%s new-env=%s reusing existing session-id" ws new-env)
      (claude-repl--log ws "seed-new-env-session: ws=%s new-env=%s seeding new session-id=%s"
                        ws new-env session-id)
      (setf (claude-repl-instantiation-session-id new-inst) session-id))
    (claude-repl--ws-put ws new-env new-inst)))

(defun claude-repl-switch-environment ()
  "Switch the current workspace between Docker sandbox and bare-metal.
Kills the current Claude process and resumes it in the other environment.
On the first switch, the new environment seeds its session-id from the
current one so --resume carries the conversation across.  On subsequent
switches each environment resumes its own prior session independently.
Requires a worktree workspace with a captured session ID."
  (interactive)
  (let* ((ws (+workspace-current-name))
         (active-env (claude-repl--ws-get ws :active-env))
         (_ (claude-repl--log ws "switch-environment: ws=%s active-env=%s" ws active-env))
         (worktree-p (claude-repl--ws-get ws :worktree-p))
         (inst (claude-repl--active-inst ws))
         (session-id (claude-repl-instantiation-session-id inst))
         (new-env (if (eq active-env :sandbox) :bare-metal :sandbox)))
    (claude-repl--validate-env-switch ws new-env worktree-p session-id)
    (claude-repl--seed-new-env-session ws new-env session-id)
    (claude-repl--kill-session ws)
    (claude-repl--ws-put ws :active-env new-env)
    (message "Switching to %s (resuming session %s...)"
             (if (eq new-env :sandbox) "Docker sandbox" "bare-metal")
             (substring session-id 0 8))
    (claude-repl--ensure-session ws)
    (claude-repl--show-panels-and-focus)))
