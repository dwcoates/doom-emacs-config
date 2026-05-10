;;; panels.el --- panel/window management and entry point -*- lexical-binding: t; -*-

;;; Code:

(defcustom claude-repl-vterm-width-fraction 0.50
  "Fraction of the work window's width allocated to the vterm panel."
  :type 'number
  :group 'claude-repl)

(defcustom claude-repl-input-height-fraction 0.23
  "Fraction of the vterm window's height allocated to the input panel."
  :type 'number
  :group 'claude-repl)

(defcustom claude-repl-loading-placeholder-name " *claude-loading*"
  "Buffer name for the loading placeholder shown while Claude starts."
  :type 'string
  :group 'claude-repl)

(defcustom claude-repl-sigkill-delay 0.5
  "Seconds to wait before sending SIGKILL to a lingering Claude process."
  :type 'number
  :group 'claude-repl)

(defcustom claude-repl-autoselect-input-on-workspace-switch t
  "When non-nil, auto-select the Claude input window on workspace switch.
If the input panel is visible after switching to a workspace, the input
window is selected so the user can start typing immediately."
  :type 'boolean
  :group 'claude-repl)

(defcustom claude-repl-session-id-display-length 8
  "Number of characters of session ID to display in messages."
  :type 'integer
  :group 'claude-repl)

;;;; Panel visibility predicates

(defun claude-repl--ws-buffer-visible-p (key)
  "Return non-nil if the buffer stored at KEY in current workspace is visible."
  (let* ((buf (claude-repl--ws-get (+workspace-current-name) key))
         (result (and buf (buffer-live-p buf) (get-buffer-window buf))))
    (claude-repl--log-verbose (+workspace-current-name) "ws-buffer-visible-p: key=%s result=%s" key (if result "visible" "hidden"))
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
    (claude-repl--log-verbose (+workspace-current-name) "panels-visible-p: result=%s" (if result "visible" "hidden"))
    result))

;;;; Panel display and hide

(defun claude-repl--safe-buffer-name (b)
  "Return the name of buffer B if non-nil, otherwise nil."
  (and b (buffer-name b)))

(defun claude-repl--close-buffer-window (buf)
  "Close the window displaying BUF.  Warns if the window cannot be closed."
  (when-let ((win (get-buffer-window buf)))
    (condition-case err
        (delete-window win)
      (error (message "[claude-repl] could not close window for %s: %S"
                      (claude-repl--safe-buffer-name buf) err)))))

(defun claude-repl--close-buffer-windows (&rest bufs)
  "Close windows displaying any of BUFS."
  (claude-repl--log (+workspace-current-name) "close-buffer-windows %s" (mapcar #'claude-repl--safe-buffer-name bufs))
  (dolist (buf bufs)
    (when (and buf (buffer-live-p buf))
      (claude-repl--close-buffer-window buf))))

(defun claude-repl--configure-vterm-window (win)
  "Configure WIN as a dedicated, width-locked, protected vterm window.
Marks the window as dedicated (so `display-buffer' can't repurpose it),
locks its width to prevent resize-triggered reflow in vterm, and sets
`no-delete-other-windows' so commands like `magit-status' that call
`delete-other-windows' cannot kill the panel.

Keyboard-navigation isolation is handled dynamically by
`claude-repl--bounce-from-vterm' rather than by a static
`no-other-window' parameter — that way windmove/`other-window' can see
vterm, but any non-mouse selection gets auto-corrected back to the
input panel (or a warning if the input isn't displayed)."
  (claude-repl--log (+workspace-current-name) "configure-vterm-window: win=%s" win)
  (set-window-dedicated-p win t)
  (set-window-parameter win 'window-size-fixed 'width)
  (set-window-parameter win 'no-delete-other-windows t))

;; Manual window layout: vterm on the right (full height), input below vterm.
(defun claude-repl--show-panels ()
  "Display vterm and input panels to the right of the current window.
Splits right for vterm (60% width to work window), then splits vterm bottom for input (15%).
If a window exists above the current one, selects it first so panels
are not split from a bottom popup (e.g. a regular vterm)."
  (when-let ((above (window-in-direction 'above)))
    (select-window above))
  (let* ((ws (+workspace-current-name))
         (vterm-buf (claude-repl--ws-get ws :vterm-buffer))
         (input-buf (claude-repl--ws-get ws :input-buffer)))
    (claude-repl--log ws "show-panels vterm=%s input=%s"
                      (claude-repl--safe-buffer-name vterm-buf)
                      (claude-repl--safe-buffer-name input-buf))
    (let* ((work-win (selected-window))
           (vterm-win (split-window work-win (round (* claude-repl-vterm-width-fraction (window-total-width work-win))) 'right))
           (input-win (split-window vterm-win (round (* (- claude-repl-input-height-fraction) (window-total-height vterm-win))) 'below)))
      (claude-repl--log ws "show-panels: work-win=%s vterm-win=%s input-win=%s" work-win vterm-win input-win)
      (claude-repl--refresh-vterm)
      (set-window-buffer vterm-win vterm-buf)
      (set-window-buffer input-win input-buf)
      (claude-repl--configure-vterm-window vterm-win)
      (set-window-dedicated-p input-win t)
      (set-window-parameter input-win 'window-size-fixed 'height)
      (set-window-parameter input-win 'no-delete-other-windows t)
      ;; `window-size-fixed' alone is bypassed by `window--resize-mini-window'
      ;; (ignore=t), so a multi-line echo area shrinks the input.  The
      ;; preserved-size parameter is only bypassed by ignore='preserved', so
      ;; preserving here steers mini-window shrink onto vterm/work-win instead.
      (window-preserve-size input-win nil t)))
  (claude-repl--update-all-workspace-states))

(defun claude-repl--focus-input-panel ()
  "Focus the input panel window and enter insert state.
Signals an error if the input buffer or its window cannot be found —
callers should ensure panels are displayed before calling this."
  (claude-repl--log (+workspace-current-name) "focus-input-panel")
  (let ((ws (+workspace-current-name)))
    (let ((buf (claude-repl--ws-get ws :input-buffer)))
      (unless buf
        (error "claude-repl--focus-input-panel: no :input-buffer for workspace %s" ws))
      (let ((win (get-buffer-window buf)))
        (unless win
          (error "claude-repl--focus-input-panel: input buffer %s is not displayed in any window" (buffer-name buf)))
        (select-window win)
        (evil-insert-state)))))

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
  (claude-repl--log-verbose (+workspace-current-name) "vterm-redraw: buf=%s" (buffer-name))
  (let ((inhibit-read-only t))
    (when vterm--term
      (vterm--redraw vterm--term))))

(defun claude-repl--do-refresh ()
  "Low-level refresh of the current vterm buffer.
Must be called with a vterm-mode buffer current."
  (claude-repl--log-verbose (+workspace-current-name) "do-refresh: buf=%s" (buffer-name))
  (claude-repl--vterm-redraw)
  (redisplay t))

(defun claude-repl--fix-vterm-scroll (buf)
  "Briefly select the vterm window for BUF to fix Emacs scroll position."
  (let ((vterm-win (get-buffer-window buf))
        (orig-win (selected-window)))
    (if (and vterm-win (not (eq vterm-win orig-win)))
        (progn
          (claude-repl--log-verbose (+workspace-current-name) "fix-vterm-scroll: fixing scroll for buf=%s" (buffer-name buf))
          (select-window vterm-win 'norecord)
          (select-window orig-win 'norecord))
      (claude-repl--log-verbose (+workspace-current-name) "fix-vterm-scroll: skipped buf=%s vterm-win=%s same-win=%s"
                                (buffer-name buf) (if vterm-win "yes" "no")
                                (if (eq vterm-win orig-win) "yes" "no")))))

(defun claude-repl--resolve-vterm-buffer ()
  "Return the vterm buffer to refresh.
Uses the current buffer if it is in vterm-mode, otherwise looks up the
workspace's vterm buffer."
  (if (eq major-mode 'vterm-mode)
      (progn
        (claude-repl--log-verbose (+workspace-current-name) "resolve-vterm-buffer: path=vterm-mode buf=%s" (buffer-name))
        (current-buffer))
    (when-let ((ws (+workspace-current-name)))
      (let ((buf (claude-repl--ws-get ws :vterm-buffer)))
        (claude-repl--log-verbose (+workspace-current-name) "resolve-vterm-buffer: path=workspace-lookup ws=%s buf=%s"
                                  ws (claude-repl--safe-buffer-name buf))
        buf))))

(defun claude-repl--refresh-vterm ()
  "Refresh the claude vterm display.
Works from any buffer or from within the vterm buffer itself."
  (let ((buf (claude-repl--resolve-vterm-buffer)))
    (cond
     ((not buf)
      (claude-repl--log-verbose (+workspace-current-name) "refresh-vterm: no buffer found"))
     ((not (buffer-live-p buf))
      (claude-repl--log-verbose (+workspace-current-name) "refresh-vterm: buffer not live buf=%s" (buffer-name buf)))
     (t
      (with-current-buffer buf
        (if (eq major-mode 'vterm-mode)
            (claude-repl--do-refresh)
          (claude-repl--log-verbose (+workspace-current-name) "refresh-vterm: buf=%s not vterm-mode (mode=%s)"
                                    (buffer-name buf) major-mode)))
      (claude-repl--fix-vterm-scroll buf)))))

(defun claude-repl--drain-pending-show-panels (ws)
  "Open panels for WS if a preemptive prompt queued a :pending-show-panels flag.
When Claude is ready, clears the flag and shows panels.  When Claude
is still starting, leaves the flag set so `on-session-start-event' can
re-drain via `open-panels-after-ready' once ready — avoids displaying
an unloaded vterm window."
  (cond
   ((not (claude-repl--ws-get ws :pending-show-panels))
    (claude-repl--log-verbose ws "drain-pending-show-panels: ws=%s branch=no-pending no-op" ws))
   ((claude-repl--session-starting-p ws)
    (claude-repl--log ws "drain-pending-show-panels: ws=%s branch=had-pending session-starting — deferring" ws))
   (t
    (claude-repl--log ws "drain-pending-show-panels: ws=%s branch=had-pending draining" ws)
    (claude-repl--ws-put ws :pending-show-panels nil)
    (claude-repl--show-hidden-panels))))

(defun claude-repl--drain-pending-magit (ws)
  "Open `magit-status' for WS if it was created with `:pending-magit' set.
Reads the worktree path from `:project-dir', clears the flag, and removes
the Doom dashboard so magit is the sole main buffer in the new workspace."
  (if (claude-repl--ws-get ws :pending-magit)
      (let ((path (claude-repl--ws-get ws :project-dir)))
        (claude-repl--log ws "drain-pending-magit: ws=%s branch=had-pending path=%s draining" ws path)
        (claude-repl--ws-put ws :pending-magit nil)
        (when path
          (magit-status path)
          (claude-repl--remove-doom-dashboard)))
    (claude-repl--log-verbose ws "drain-pending-magit: ws=%s branch=no-pending no-op" ws)))

(defun claude-repl--drain-pending-initial-buffers (ws)
  "Open configured initial buffers for WS if `:pending-initial-buffers' is set.
Reads the worktree path from `:project-dir' and clears the flag.  Deferred
from `finalize-worktree-workspace' so `find-file-noselect' runs while WS is
the current perspective, preventing the opened buffers from leaking into
the caller's workspace."
  (if (claude-repl--ws-get ws :pending-initial-buffers)
      (let ((path (claude-repl--ws-get ws :project-dir)))
        (claude-repl--log ws "drain-pending-initial-buffers: ws=%s branch=had-pending path=%s draining" ws path)
        (claude-repl--ws-put ws :pending-initial-buffers nil)
        (when path
          (claude-repl--open-initial-buffers ws path)))
    (claude-repl--log-verbose ws "drain-pending-initial-buffers: ws=%s branch=no-pending no-op" ws)))

;; Refresh vterm on workspace switch
(defun claude-repl--maybe-autoselect-input (ws)
  "Select the Claude input window for WS if visible and autoselect is enabled.
Respects `claude-repl-autoselect-input-on-workspace-switch'."
  (when claude-repl-autoselect-input-on-workspace-switch
    (when-let ((buf (claude-repl--ws-get ws :input-buffer))
               (win (and (buffer-live-p buf) (get-buffer-window buf))))
      (claude-repl--log ws "maybe-autoselect-input: selecting input-win=%s" win)
      (select-window win))))

(defun claude-repl--on-workspace-switch (&optional ws)
  "Handle workspace switch: update all workspace states, refresh vterm, reset cursors.
WS is the workspace name to operate on; when nil, falls back to
`(+workspace-current-name)' at call time.  Callers from
`--after-persp-activated' pass the ws captured at hook-fire time so
the deferred call operates on the workspace that was just switched
to, even if another switch raced ahead before the timer fired.

Also opens panels for workspaces that were created with a preemptive
prompt, and auto-selects the input window if visible.

If the newly-active workspace has `:claude-state :done', sets
`:done-acked' to t so the decay timer can clear :done → :idle on the
next tick.  This implements \"only decay after the user has viewed
the workspace.\"

Also runs `claude-repl--maybe-sweep-hidden-on-switch' so workspaces
marked `:hidden' (via `SPC o C') are persp-killed when hide-mode is on
— the persp-level enforcement of hide-mode."
  (let ((ws (or ws (+workspace-current-name))))
    (claude-repl--log-verbose ws "workspace-switch ws=%s" ws)
    (when (eq (claude-repl--ws-claude-state ws) :done)
      (claude-repl--ws-put ws :done-acked t))
    (claude-repl--maybe-sweep-hidden-on-switch ws)
    (claude-repl--update-all-workspace-states)
    (claude-repl--refresh-vterm)
    (claude-repl--reset-vterm-cursors)
    (claude-repl--drain-pending-magit ws)
    (claude-repl--drain-pending-initial-buffers ws)
    (claude-repl--drain-pending-show-panels ws)
    (claude-repl--maybe-autoselect-input ws)
    ;; Flip the emacs-side bit on the fully-loaded latch.  If
    ;; --on-session-start-event has also fired, this fires the
    ;; ws-fully-loaded hook; otherwise we just record the bit and wait
    ;; for claude-ready.  Guarded on ws non-nil so the nil-ws fallback
    ;; (test envs, persp init) doesn't poison the hash table.
    (when ws
      (claude-repl--latch-and-maybe-fire-loaded ws :ws-loaded))))

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
Redirects away from Claude buffers and saves frame state.
Logs `persp-names-cache' so cache mutations across persp lifecycle
events (kill, switch, add) are traceable."
  (claude-repl--log (+workspace-current-name) "before-persp-deactivate: entry cache=%S"
                    (if (boundp 'persp-names-cache) persp-names-cache "(unbound)"))
  (claude-repl--redirect-from-claude-before-save)
  (condition-case err
      (persp-frame-save-state)
    (error (message "[claude-repl] WARNING: persp-frame-save-state failed: %S" err)
           (claude-repl--log (+workspace-current-name) "before-persp-deactivate: persp-frame-save-state error: %S" err))))

(defun claude-repl--after-persp-activated (&rest _)
  "Handle perspective activation by scheduling a workspace switch.
Captures `(+workspace-current-name)' at hook-fire time and passes it
to the deferred `--on-workspace-switch' so the call operates on the
workspace that just activated, not whatever happens to be current
when the run-at-time-0 timer eventually fires (rapid back-to-back
switches would otherwise have every deferred call resolve to the
latest ws, dropping bookkeeping on the intermediate ones).

Logs `persp-names-cache' so cache mutations across persp lifecycle
events (kill, switch, add) are traceable."
  (claude-repl--log (+workspace-current-name) "after-persp-activated: entry cache=%S"
                    (if (boundp 'persp-names-cache) persp-names-cache "(unbound)"))
  (let ((ws (+workspace-current-name)))
    (run-at-time 0 nil #'claude-repl--on-workspace-switch ws)))

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

(defun claude-repl--save-tab-index (ws)
  "Persist WS's current tab-bar index to its plist as `:saved-tab-index'.
Read on reopen by `claude-repl--restore-tab-index' so the workspace
returns to its prior slot in the tab-bar after a close-deprio cycle.
Reads positions from `persp-names-current-frame-fast-ordered'; no-op
when that helper is unavailable (e.g. test envs without persp-mode).

Also writes the index to disk via `--state-save' so a deprioritized ws
that the user closes Emacs on still returns to its saved slot on
restart (without this, `:saved-tab-index' is in-memory only and the ws
loses its prior position across an Emacs lifecycle)."
  (when (fboundp 'persp-names-current-frame-fast-ordered)
    (when-let ((idx (cl-position ws (persp-names-current-frame-fast-ordered)
                                 :test #'string=)))
      (claude-repl--log ws "save-tab-index ws=%s index=%d" ws idx)
      (claude-repl--ws-put ws :saved-tab-index idx)
      (claude-repl--state-save ws))))

(defun claude-repl--restore-tab-index (ws)
  "Move WS back to its persisted `:saved-tab-index' slot, if any.
Clears `:saved-tab-index' after restoring so each close-deprio cycle
captures a fresh baseline.  No-op when no index is saved or when persp
helpers are unavailable.  Index is clamped to the current names list
length so a saved index past the new tail is handled gracefully.

Drives `claude-repl--force-tab-bar-redraw' (NOT `+dwc/refresh-tab-bar')
so the trailing-space-toggle is flipped — the tab-bar's string-equality
cache otherwise risks holding the pre-restore order if the new tabline
string happens to compare equal under propertized-string semantics."
  (when-let ((idx (claude-repl--ws-get ws :saved-tab-index)))
    (when (and (fboundp 'persp-names-current-frame-fast-ordered)
               (fboundp 'persp-update-names-cache))
      (let* ((names (persp-names-current-frame-fast-ordered))
             (without-ws (remove ws names))
             (clamped (min idx (length without-ws)))
             (head (cl-subseq without-ws 0 clamped))
             (tail (cl-subseq without-ws clamped))
             (reordered (append head (list ws) tail)))
        (claude-repl--log ws "restore-tab-index ws=%s saved-idx=%d clamped=%d"
                          ws idx clamped)
        (persp-update-names-cache reordered)
        (claude-repl--ws-put ws :saved-tab-index nil)
        ;; Persist the cleared index so a future restart doesn't see a
        ;; stale value and re-restore (the ws is no longer deprioritized
        ;; once we've reseated it).
        (claude-repl--state-save ws)
        (when (fboundp 'claude-repl--force-tab-bar-redraw)
          (claude-repl--force-tab-bar-redraw))))))

(defun claude-repl--on-simple-close (&optional ws)
  "Bookkeep + hide panels; do NOT touch tab-bar order.
Sets `:repl-state :inactive' on WS (`:claude-state' untouched so an
in-flight :thinking / :permission survives the close), then hides
the panel windows.  No save-tab-index, no push-to-back, no flash —
this is the simple-close audit point that `SPC o c' is bound to."
  (let ((ws (or ws (+workspace-current-name))))
    (claude-repl--log ws "on-simple-close: CALLED this-command=%s last-command=%s"
                      this-command last-command)
    (when ws
      (claude-repl--log ws "on-simple-close ws=%s claude-state=%s -> repl-state=:inactive"
                        ws (claude-repl--ws-claude-state ws))
      (claude-repl--ws-set-repl-state ws :inactive))
    (claude-repl--hide-panels)))

(defun claude-repl--on-close (&optional ws)
  "Full close: bookkeep + hide + deprio + save tab index.
Sets WS's `:repl-state' to `:hidden' (NOT `:inactive' like the
simple-close path) so the workspace becomes a kill candidate for the
next sweep when `claude-repl-hide-mode-enabled' is on.  See
`claude-repl--ws-set-repl-state' for the `:hidden' contract.  Then
hides panels and pushes WS to the second-to-last tab position via
`+dwc/workspace-push-to-back', snapshotting the tab index first via
`claude-repl--save-tab-index' so a future reopen can restore the
position.

Bound to `SPC o C' (the deprio toggle); also fires from
`claude-repl-send-and-hide' since send-and-hide is semantically
\"I'm done with this prompt, move on\".  Push-to-back is guarded by
`fboundp' so the module remains usable when the helper is not loaded.

WS defaults to the current workspace; when WS is nil the function still
hides panels but skips the bookkeeping write and the tab shuffle."
  (let ((ws (or ws (+workspace-current-name))))
    (claude-repl--log ws "on-close: CALLED this-command=%s last-command=%s"
                      this-command last-command)
    (when ws
      (claude-repl--log ws "on-close ws=%s claude-state=%s -> repl-state=:hidden"
                        ws (claude-repl--ws-claude-state ws))
      (claude-repl--ws-set-repl-state ws :hidden))
    (claude-repl--hide-panels)
    (when (and ws
               (equal ws (+workspace-current-name))
               (fboundp '+dwc/workspace-push-to-back))
      (claude-repl--save-tab-index ws)
      (claude-repl--log ws "on-close: pushing ws=%s to second-to-last" ws)
      (+dwc/workspace-push-to-back))))

(defun claude-repl--unhide-workspace (ws)
  "Reverse `claude-repl--on-close' for WS by setting `:repl-state' to `:active'.
A no-op when WS is nil or has a `:repl-state' other than `:hidden' —
non-hidden workspaces don't need unhiding, and overwriting
`:inactive' / `:dead' / nil with `:active' would lie about lifecycle
state.  Does NOT re-show panels or re-shuffle tab order; that side of
the close is reversible only by an explicit panel-show."
  (when ws
    (let ((rstate (claude-repl--ws-get ws :repl-state)))
      (claude-repl--log ws "unhide-workspace: ws=%s repl-state=%s" ws rstate)
      (when (eq rstate :hidden)
        (claude-repl--ws-set-repl-state ws :active)))))

;;;; Window synchronization

;; Auto-close orphaned panels: if one is closed, close the other.
;; Also refresh the hide overlay in case a window change invalidated it.
(defun claude-repl--extract-panel-id (name)
  "Extract the workspace identifier from a Claude panel buffer NAME.
Returns the identifier string, or nil if NAME is not a Claude panel buffer.
Input-buffer check comes first since `claude-repl--vterm-buffer-re' is a
superset that also matches input-buffer names."
  (cond
   ((string-match-p claude-repl--input-buffer-re name)
    (substring name (length "*claude-panel-input-") (- (length name) (length "*"))))
   ((string-match-p claude-repl--vterm-buffer-re name)
    (substring name (length "*claude-panel-") (- (length name) (length "*"))))))

(defun claude-repl--partner-buffer-name (name id)
  "Return the partner buffer name for Claude panel NAME with identifier ID.
For a vterm buffer, the partner is the input buffer, and vice versa.
Checks input-re first since vterm-re is a superset that also matches inputs."
  (if (string-match-p claude-repl--input-buffer-re name)
      (format "*claude-panel-%s*" id)
    (format "*claude-panel-input-%s*" id)))

(defun claude-repl--orphaned-panel-p (name)
  "Return non-nil if NAME is a Claude panel buffer whose partner is not visible.
Ignores single-window frames.  Input buffers are not orphaned while the
loading placeholder exists (the vterm has not been swapped in yet)."
  (when-let ((id (claude-repl--extract-panel-id name)))
    (let* ((is-input (string-match-p claude-repl--input-buffer-re name))
           (partner (claude-repl--partner-buffer-name name id))
           (result (and (not (one-window-p))
                        (not (get-buffer-window partner))
                        ;; Input panels are not orphaned while loading placeholder is live
                        (or (not is-input)
                            (not (get-buffer claude-repl-loading-placeholder-name))))))
      (when result
        (claude-repl--log-verbose (+workspace-current-name) "orphaned-panel-p: name=%s partner=%s is-orphaned" name partner))
      result)))

(defun claude-repl--sync-panels ()
  "Close any Claude panel whose partner is no longer visible."
  (claude-repl--log-verbose (+workspace-current-name) "sync-panels: entry windows=%d" (length (window-list)))
  (let ((orphan-count 0))
    (dolist (win (window-list))
      (let ((name (buffer-name (window-buffer win))))
        (when (claude-repl--orphaned-panel-p name)
          (setq orphan-count (1+ orphan-count))
          (claude-repl--log (+workspace-current-name) "sync-panels closing orphaned %s" name)
          (delete-window win))))
    (claude-repl--log-verbose (+workspace-current-name) "sync-panels: closed %d orphans" orphan-count)))

;; Keep visible Claude vterm buffers scrolled to the cursor.
;; Skips the selected window so clicking into vterm to read/copy isn't disrupted.
(defun claude-repl--refresh-vterm-window (win)
  "Refresh the Claude vterm buffer shown in WIN.
Resets cursor, redraws, and syncs window point."
  (let ((buf (window-buffer win)))
    (when (and buf (buffer-live-p buf) (claude-repl--claude-buffer-p buf))
      (claude-repl--log-verbose (+workspace-current-name) "refresh-vterm-window: win=%s buf=%s" win (buffer-name buf))
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
  (claude-repl--log-verbose (+workspace-current-name) "reset-vterm-cursors: entry")
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
  (claude-repl--log-verbose (+workspace-current-name) "on-window-change")
  (claude-repl--sync-panels)
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
  "If the selected window shows a Claude vterm buffer, redirect to the input window.
Allows mouse-initiated selection through so clicking into the output to
scroll or copy works.  When no input window is currently displayed
\(e.g. panels are hidden), emits a warning via `message' rather than
leaving point stranded silently.

Predicate is buffer-identity (`claude-repl--claude-buffer-p' — vterm-only,
excludes input buffers) rather than the `no-other-window' parameter, so
this bounce alone is sufficient to keep keyboard nav out of vterm."
  (let ((win (selected-window)))
    (if (and (claude-repl--claude-buffer-p (window-buffer win))
             (not (mouse-event-p last-input-event)))
        (let* ((ws (+workspace-current-name))
               (input-buf (and ws (claude-repl--ws-get ws :input-buffer)))
               (input-win (and input-buf (get-buffer-window input-buf))))
          (if input-win
              (progn
                (claude-repl--log-verbose (+workspace-current-name) "bounce-from-vterm: bouncing to input-win=%s" input-win)
                (select-window input-win))
            (message "[claude-repl] keyboard navigation landed in Claude vterm but input panel isn't visible — stuck here until you click out or reopen panels")
            (claude-repl--log (+workspace-current-name) "bounce-from-vterm: no input-win to bounce to (warned)")))
      (claude-repl--log-verbose (+workspace-current-name) "bounce-from-vterm: skipped vterm-buffer=%s mouse=%s"
                                (if (claude-repl--claude-buffer-p (window-buffer win)) "yes" "no")
                                (if (mouse-event-p last-input-event) "yes" "no")))))

(add-hook 'window-selection-change-functions #'claude-repl--bounce-from-vterm)

;;;; Buffer creation

(defun claude-repl--initialize-input-buffer (ws)
  "Create the Claude input buffer for workspace WS and enable claude-input-mode.
Errors if the buffer is already initialized (already in `claude-input-mode')."
  (claude-repl--log ws "initialize-input-buffer")
  (let ((input-buf (claude-repl--create-buffer ws "-input")))
    (claude-repl--ws-put ws :input-buffer input-buf)
    (with-current-buffer input-buf
      (when (eq major-mode 'claude-input-mode)
        (error "claude-repl--initialize-input-buffer: already initialized ws=%s" ws))
      (claude-input-mode)
      (claude-repl--history-restore ws))))

(defun claude-repl--kill-stale-vterm (&optional ws)
  "Kill the Claude vterm buffer for WS if it exists but has no live process.
WS defaults to the current workspace."
  (let ((existing (get-buffer (claude-repl--buffer-name nil ws))))
    (if (not existing)
        (claude-repl--log (+workspace-current-name) "kill-stale-vterm: no existing buffer")
      (if (get-buffer-process existing)
          (claude-repl--log (+workspace-current-name) "kill-stale-vterm: buf=%s has live process no-op" (buffer-name existing))
        (claude-repl--log (+workspace-current-name) "kill-stale-vterm: killing stale buf=%s" (buffer-name existing))
        (kill-buffer existing)))))

;;;; Panel show/hide strategies

(defun claude-repl--show-loading-panels ()
  "Show panels using a loading placeholder in the vterm slot.
The placeholder is swapped for the real vterm buffer once Claude is ready."
  (let* ((ws (+workspace-current-name))
         (real-vterm (claude-repl--ws-get ws :vterm-buffer))
         (placeholder (get-buffer-create claude-repl-loading-placeholder-name)))
    (claude-repl--log ws "show-loading-panels")
    (with-current-buffer placeholder
      (setq-local mode-line-format nil)
      (claude-repl--set-buffer-background claude-repl--vterm-background-grey))
    (claude-repl--ws-put ws :vterm-buffer placeholder)
    (claude-repl--show-panels-and-focus)
    (claude-repl--ws-put ws :vterm-buffer real-vterm)))

(defun claude-repl--initialize-claude (&optional ws project-dir-hint active-env-hint)
  "Initialize a Claude session for WS.
Calls `initialize-ws-env' with PROJECT-DIR-HINT and ACTIVE-ENV-HINT
(creation paths — worktree setup or new-workspace — pass known values
here; regular `SPC o c' passes nil and lets the helper derive from
the state file or the current buffer's git-root).  Then creates the
output vterm buffer, launches the Claude CLI inside it, creates the
input buffer, enables the hide-overlay, marks `:claude-state' as
`:init', and announces the startup.  Errors if Claude is already
running for WS.

Writes `:claude-state :init' immediately after launching the vterm
process (documented lifecycle exception to the sentinel-only-writes
rule — no hook fires between process launch and session-start, so
Emacs is the only observer of \"Claude process exists, not ready yet\").
Panels are deliberately NOT opened here — `on-session-start-event'
opens them once `:claude-state' transitions from `:init' to `:idle'.
During that window the user sees the blue `:init' tab and the
echo-area message below."
  (let ((ws (or ws (+workspace-current-name))))
    (unless ws (error "claude-repl--initialize-claude: no active workspace"))
    (when (claude-repl--claude-running-p ws)
      (error "claude-repl--initialize-claude: already running ws=%s" ws))
    (claude-repl--log ws "initialize-claude: starting new session for ws=%s" ws)
    (claude-repl--initialize-ws-env ws project-dir-hint active-env-hint)
    (let* ((root (claude-repl--ws-dir ws))
           (default-directory root))
      (claude-repl--kill-stale-vterm ws)
      (let* ((vterm-buf (claude-repl--create-buffer ws))
             (start-info (claude-repl--build-start-cmd ws))
             (cmd         (plist-get start-info :cmd))
             (inst        (plist-get start-info :inst)))
        (claude-repl--ws-put ws :vterm-buffer vterm-buf)
        (setf (claude-repl-instantiation-start-cmd inst) cmd)
        (when (plist-get start-info :fork-session-id)
          (claude-repl--log ws "initialize-claude: clearing fork-session-id for ws=%s" ws)
          (claude-repl--ws-put ws :fork-session-id nil))
        (claude-repl--log-session-start ws start-info)
        (with-current-buffer vterm-buf
          (when (eq major-mode 'vterm-mode)
            (error "claude-repl--initialize-claude: vterm buffer already initialized ws=%s" ws))
          (vterm-mode)
          (setq-local truncate-lines nil)
          (setq-local word-wrap t)
          (claude-repl--set-buffer-background claude-repl--vterm-background-grey)
          (setq-local mode-line-format
                      (claude-repl--workspace-mode-line ws))
          (setq-local claude-repl--ready nil)
          (claude-repl--log ws "initialize-claude: vterm=%s sending cmd len=%d"
                            (buffer-name) (length cmd))
          (vterm-send-string (concat claude-repl-startup-prefix cmd))
          (vterm-send-return))
        (claude-repl--schedule-ready-timer ws)
        (claude-repl--initialize-input-buffer ws)
        (claude-repl--ws-put ws :prefix-counter 0)
        (claude-repl--enable-hide-overlay)
        (claude-repl--ws-set-claude-state ws :init)
        (message "Starting Claude... ws=%s ws-id=%s dir=%s cmd=%s"
                 ws (claude-repl--workspace-id) root (or cmd "?"))
        (claude-repl--state-save ws)))))

(defun claude-repl--show-existing-panels ()
  "Show panels for an already-running Claude session.
Demotes indicators, refreshes display, and restores panel layout.
Sets `:repl-state :active' now that panels are visible and the
session is in use.

Tab-bar bookkeeping happens FIRST (before any window manipulation) so
the persp-names reorder is in place before `delete-other-windows' /
`show-panels-and-focus' trigger redisplay — otherwise the intermediate
paint can lock the pre-restore order into the tab-bar's cache.  After
panels are up, pulses the tab via `claude-repl-flash-tab' so the user
can track its return to the prior slot — symmetric with the
deprio-on-close flash."
  (let ((ws (+workspace-current-name)))
    (claude-repl--log ws "show-existing-panels")
    (unless ws (error "claude-repl--show-existing-panels: no active workspace"))
    (claude-repl--ws-set-repl-state ws :active)
    (claude-repl--restore-tab-index ws)
    (claude-repl--refresh-vterm)
    (delete-other-windows)
    (claude-repl--show-panels-and-focus)
    (claude-repl--update-hide-overlay)
    (claude-repl--flash-current-tab)))

(defun claude-repl--show-hidden-panels ()
  "Restore hidden panels.  `show-existing-panels' writes :repl-state :active.
`:claude-state' is untouched; rendering follows the same rule whether
panels are visible or hidden."
  (let ((ws (+workspace-current-name)))
    (claude-repl--log ws "showing panels ws=%s claude-state=%s"
                      ws (claude-repl--ws-claude-state ws)))
  (claude-repl--show-existing-panels))

(defun claude-repl--hide-and-preserve-status ()
  "Hide Claude panels with full deprio + tab-bar shuffle (the `SPC o C' path).
Thin wrapper around `claude-repl--on-close' that enforces the invariant
that a workspace is active.  See `claude-repl--simple-hide-and-preserve-status'
for the no-tab-bar-update variant bound to `SPC o c'."
  (let ((ws (+workspace-current-name)))
    (unless ws (error "claude-repl--hide-and-preserve-status: no active workspace"))
    (claude-repl--on-close ws)))

(defun claude-repl--simple-hide-and-preserve-status ()
  "Hide Claude panels with NO tab-bar update (the `SPC o c' path).
Thin wrapper around `claude-repl--on-simple-close' that enforces the
invariant that a workspace is active.  See
`claude-repl--hide-and-preserve-status' for the deprio + flash variant
bound to `SPC o C'."
  (let ((ws (+workspace-current-name)))
    (unless ws (error "claude-repl--simple-hide-and-preserve-status: no active workspace"))
    (claude-repl--on-simple-close ws)))

;;;; Entry point

(cl-defun claude-repl--toggle (close-fn &key always-close)
  "Generic toggle.  CLOSE-FN handles the visible-panels case.
Open / start / send-selection paths are shared.  Used by both
`claude-repl' (deprio close) and `claude-repl-simple' (plain close).

When ALWAYS-CLOSE is non-nil, every non-selection branch routes to
CLOSE-FN regardless of running / starting / panel-visibility state —
the workspace is hidden even if Claude isn't visible (or isn't running
at all).  This is the `SPC o C' contract: pressing it again on a
workspace that is already hidden / never-started should still mark it
`:hidden' and push it to the back, not re-show or launch Claude."
  (let* ((ws (+workspace-current-name))
         (vterm-running (claude-repl--claude-running-p))
         (session-starting (claude-repl--session-starting-p))
         (panels-visible (claude-repl--panels-visible-p))
         (selection (when (use-region-p)
                     (buffer-substring-no-properties (region-beginning) (region-end)))))
    (claude-repl--log ws "claude-repl running=%s starting=%s visible=%s selection=%s always-close=%s"
                      vterm-running session-starting panels-visible
                      (if selection "yes" "no") (if always-close "yes" "no"))
    (cond
     (selection
      (deactivate-mark)
      (claude-repl--send-to-claude selection))
     (always-close
      (funcall close-fn))
     ((not vterm-running)
      (claude-repl--initialize-claude))
     (session-starting
      (message "Claude is loading…"))
     (panels-visible
      (funcall close-fn))
     (t
      (claude-repl--show-hidden-panels)))))

(defun claude-repl ()
  "Hide Claude REPL panels and deprio the workspace.
If text is selected: send it directly to Claude (orthogonal to hide).
Otherwise: mark the workspace `:repl-state :hidden', hide both panels
\(no-op if already hidden), and push the workspace tab to the back.
Always hides, regardless of whether Claude is running or panels are
currently visible — if hide-mode is on, the next workspace switch will
persp-kill the workspace via `claude-repl--sweep-hidden-workspaces'.
Bound to `SPC o C'.  See `claude-repl-simple' for the no-tab-bar variant."
  (interactive)
  (claude-repl--toggle #'claude-repl--hide-and-preserve-status :always-close t))

(defun claude-repl-simple ()
  "Toggle Claude REPL panels with a plain close (no tab-bar update).
Same dispatch as `claude-repl' except the close branch only hides the
panels and sets `:repl-state :inactive' — no save-tab-index, no
push-to-back, no flash.  Bound to `SPC o c'."
  (interactive)
  (claude-repl--toggle #'claude-repl--simple-hide-and-preserve-status))

;;;; Session cleanup

(defun claude-repl--kill-placeholder ()
  "Close and kill the loading placeholder buffer if it exists."
  (claude-repl--log (+workspace-current-name) "kill-placeholder exists=%s" (if (get-buffer claude-repl-loading-placeholder-name) "yes" "no"))
  (when-let ((placeholder (get-buffer claude-repl-loading-placeholder-name)))
    (claude-repl--close-buffer-window placeholder)
    (kill-buffer placeholder)))

(defun claude-repl--sigkill-if-alive (proc)
  "Send SIGKILL to PROC if it is still alive."
  (when (process-live-p proc)
    (claude-repl--log (+workspace-current-name) "sigkill fallback for lingering process")
    (signal-process proc 'SIGKILL)))

(defun claude-repl--schedule-sigkill (proc)
  "Schedule a SIGKILL for PROC after 0.5s if it's still alive."
  (claude-repl--log (+workspace-current-name) "schedule-sigkill: scheduling for proc=%s" proc)
  (run-at-time claude-repl-sigkill-delay nil #'claude-repl--sigkill-if-alive proc))

(defun claude-repl--kill-vterm-process (buf)
  "Kill the vterm buffer BUF and its process.
Suppresses both the standard process-exit query (via
`set-process-query-on-exit-flag') and any other
`kill-buffer-query-functions' (e.g., vterm's own kill query) so the
nuke path never prompts about the claude process."
  (claude-repl--log (+workspace-current-name) "kill-vterm-process buf=%s" (claude-repl--safe-buffer-name buf))
  (when (and buf (buffer-live-p buf))
    (let ((proc (get-buffer-process buf))
          (kill-buffer-query-functions nil))
      (when proc
        (set-process-query-on-exit-flag proc nil))
      (kill-buffer buf)
      (when proc
        (claude-repl--schedule-sigkill proc)))))

(defun claude-repl--teardown-session-state (ws)
  "Save history, disable overlay, cancel timers, and clear session state for workspace WS."
  (claude-repl--log ws "teardown-session-state ws=%s env=%s"
                    ws (claude-repl--ws-get ws :active-env))
  (condition-case err
      (claude-repl--disable-hide-overlay)
    (error (message "[claude-repl] WARNING: disable-hide-overlay failed during teardown: %S" err)))
  (when claude-repl--sync-timer
    (cancel-timer claude-repl--sync-timer)
    (setq claude-repl--sync-timer nil))
  ;; Update instantiation and persist state BEFORE clearing buffer refs,
  ;; since state-save needs the vterm buffer to resolve the project root.
  (let ((inst (claude-repl--active-inst ws)))
    (setf (claude-repl-instantiation-start-cmd inst) nil))
  (claude-repl--state-save ws)
  (claude-repl--ws-put ws :vterm-buffer nil)
  (claude-repl--ws-put ws :input-buffer nil))

(defun claude-repl--destroy-session-buffers (vterm-buf input-buf)
  "Close windows and kill VTERM-BUF, INPUT-BUF, and any placeholder."
  (claude-repl--log (+workspace-current-name) "destroy-session-buffers")
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

(defun claude-repl--kill-workspace-buffers (ws)
  "Kill every buffer (and attached process) belonging to persp WS.
Idempotent: no-op when persp-mode is inactive, the persp does not
exist, or the persp slot holds a symbol sentinel rather than a real
perspective.  Each buffer is killed inside its own `condition-case' so
one bad buffer cannot block the rest.  File-visiting buffers are
marked unmodified before killing so `kill-buffer' does not prompt —
the user has already confirmed the destructive nuke."
  (when (and (bound-and-true-p persp-mode)
             (fboundp 'persp-get-by-name)
             (fboundp 'persp-buffers))
    (when-let ((persp (persp-get-by-name ws)))
      (unless (symbolp persp)
        (let ((bufs (persp-buffers persp))
              (kill-buffer-query-functions nil))
          (claude-repl--log ws "kill-workspace-buffers: count=%d" (length bufs))
          (dolist (buf bufs)
            (condition-case err
                (when (buffer-live-p buf)
                  (when-let ((proc (get-buffer-process buf)))
                    (set-process-query-on-exit-flag proc nil)
                    (ignore-errors (delete-process proc))
                    (claude-repl--schedule-sigkill proc))
                  (with-current-buffer buf
                    (set-buffer-modified-p nil))
                  (kill-buffer buf))
              (error
               (claude-repl--log ws "kill-workspace-buffers: error on %s: %S"
                                 (claude-repl--safe-buffer-name buf) err)))))))))

;;;; User commands

(defun claude-repl-kill ()
  "Kill Claude REPL buffers and windows for the current workspace."
  (interactive)
  (let ((ws (+workspace-current-name)))
    (claude-repl--log ws "kill")
    (unless ws (error "claude-repl-kill: no active workspace"))
    ;; Lifecycle-reset: kill destroys the session, so both state axes are
    ;; reset to nil.  (Documented exception to "sentinel-only writes
    ;; claude-state" — see analysis/12.)  :repl-state nil means "no panels
    ;; and no particular inactive/dead designation"; the workspace returns
    ;; to a pristine no-Claude state awaiting the next initialize-claude.
    (claude-repl--ws-put ws :claude-state nil)
    (claude-repl--ws-put ws :repl-state nil)
    (force-mode-line-update t)
    (claude-repl--kill-session ws)))

(defun claude-repl-restart ()
  "Hard restart Claude for the current workspace.
Kills the process, windows, and buffers for the current session and
re-initializes. The Claude state file on disk is preserved so the new
process resumes via `--continue'. Panels reopen once the new session
signals ready."
  (interactive)
  (let ((ws (+workspace-current-name)))
    (claude-repl--log ws "restart")
    (claude-repl-kill)
    (claude-repl--initialize-claude ws)))

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
     ((not (claude-repl--claude-running-p))
      (claude-repl--log ws "focus-input branch=initialize-claude")
      (claude-repl))
     ;; Running but panels hidden — show them
     (t
      (claude-repl--log ws "focus-input branch=show-or-focus")
      (unless (claude-repl--panels-visible-p)
        (claude-repl--show-panels))
      (when-let ((win (get-buffer-window (claude-repl--ws-get ws :input-buffer))))
        (select-window win)
        (when (bound-and-true-p evil-mode)
          (evil-insert-state)))))))

(defun claude-repl--delete-non-panel-windows (vterm-buf input-buf)
  "Delete all windows not showing VTERM-BUF or INPUT-BUF."
  (claude-repl--log (+workspace-current-name) "delete-non-panel-windows: window-count=%d" (length (window-list)))
  (dolist (win (window-list))
    (unless (memq (window-buffer win) (list vterm-buf input-buf))
      (condition-case err
          (delete-window win)
        (error (message "[claude-repl] could not close non-panel window: %S" err))))))

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

(defvar claude-repl--window-fullscreen-config nil
  "Saved window configuration for non-Claude fullscreen toggle.
Set when `claude-repl-fullscreen-and-focus' maximizes a non-Claude window,
cleared on restore.")

(defun claude-repl-fullscreen-and-focus ()
  "Toggle fullscreen for Claude panels and focus the input window.
When in a Claude panel buffer, maximizes both Claude windows and moves
point to the input buffer, or restores the layout.
When not in a Claude panel buffer, maximizes the current window and
saves the layout; calling again restores it."
  (interactive)
  (if (claude-repl--claude-panel-buffer-p)
      (progn
        (claude-repl-toggle-fullscreen)
        (let* ((ws (+workspace-current-name))
               (input-buf (claude-repl--ws-get ws :input-buffer))
               (input-win (and input-buf (get-buffer-window input-buf))))
          (when input-win
            (select-window input-win)
            (when (bound-and-true-p evil-mode)
              (evil-insert-state)))))
    (if claude-repl--window-fullscreen-config
        (progn
          (set-window-configuration claude-repl--window-fullscreen-config)
          (setq claude-repl--window-fullscreen-config nil))
      (setq claude-repl--window-fullscreen-config (current-window-configuration))
      (delete-other-windows))))

(defun claude-repl-cycle ()
  "Send backtab to Claude vterm to cycle through options."
  (interactive)
  (claude-repl--log (+workspace-current-name) "cycle")
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
                   (claude-repl--ws-get ws :project-dir))))
    (user-error "No sandbox configuration found for this workspace")))

(defun claude-repl--seed-new-env-session (ws new-env session-id)
  "Ensure the NEW-ENV instantiation for WS has a session-id.
If this is the first switch, seeds the new environment's session-id
from SESSION-ID.  The value signals to `compute-claude-flags' that
the env should emit `--continue' on start — which picks up the most
recent session in the worktree's cwd (i.e. the one we just left)."
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
current one so `--continue' in the other env picks up the conversation.
On subsequent switches each environment resumes its own prior session
independently.  Requires a worktree workspace with a captured session ID."
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
             (substring session-id 0 claude-repl-session-id-display-length))
    (claude-repl--initialize-claude ws)
    (claude-repl--show-panels-and-focus)))
