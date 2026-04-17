;;; status.el --- workspace status state machine and tab bar rendering -*- lexical-binding: t; -*-

;;; Code:

;;; Priority badge images
;;
;; Each image is a small PNG loaded from the module's images/ directory and
;; scaled to fit the tab-bar line height.

(defvar claude-repl--priority-images nil
  "Alist mapping priority strings (\"p05\" \"p1\" \"p2\" \"p3\") to Emacs image specs.")

(defun claude-repl--load-priority-images ()
  "Load priority badge PNGs from the module images/ directory.
Populates `claude-repl--priority-images' with display-ready image specs."
  (let* ((dir (file-name-directory (or load-file-name buffer-file-name)))
         (img-dir (expand-file-name "images/" dir))
         (names '("p05" "p1" "p2" "p3"))
         (height (frame-char-height)))
    (setq claude-repl--priority-images
          (cl-loop for name in names
                   for file = (expand-file-name (concat name ".png") img-dir)
                   when (file-exists-p file)
                   collect (cons name (create-image file 'png nil
                                                    :height height
                                                    :ascent 'center))))
    (claude-repl--log nil "load-priority-images: loaded=%d" (length claude-repl--priority-images))))

(when (image-type-available-p 'png)
  (claude-repl--load-priority-images))

(defun claude-repl--priority-image (priority)
  "Return the Emacs image spec for PRIORITY string, or nil."
  (cdr (assoc priority claude-repl--priority-images)))

;;; Workspace state accessors ------------------------------------------------

;; --- Two-axis state model (analysis #8) ---
;;
;; Workspace state is split into two orthogonal plist keys:
;;   :claude-state — Claude-owned lifecycle.  Values: nil | :init |
;;                   :idle | :thinking | :done | :permission.
;;                   Written primarily by hook sentinels; narrow
;;                   Emacs-side exceptions at lifecycle boundaries
;;                   (start-fresh writes :init; kill clears).
;;   :repl-state   — Emacs-owned panel-visibility flag.  Values: nil |
;;                   :inactive.  Does not contribute to tab color.

(defun claude-repl--ws-state (ws)
  "Return the current :claude-state keyword for workspace WS, or nil.
Compat shim: equivalent to `claude-repl--ws-claude-state', retained for
test callers that have not yet migrated."
  (claude-repl--ws-get ws :claude-state))

(defun claude-repl--ws-claude-state (ws)
  "Return the current :claude-state keyword for workspace WS, or nil."
  (claude-repl--ws-get ws :claude-state))

(defun claude-repl--ws-repl-state (ws)
  "Return the current :repl-state keyword for workspace WS, or nil."
  (claude-repl--ws-get ws :repl-state))

(defun claude-repl--ws-set-claude-state (ws state)
  "Set workspace WS's :claude-state to STATE.
STATE is one of: nil, :init, :idle, :thinking, :done, :permission."
  (unless ws (error "claude-repl--ws-set-claude-state: ws is nil"))
  (claude-repl--log ws "claude-state %s -> %s" ws state)
  (claude-repl--ws-put ws :claude-state state)
  (force-mode-line-update t))

(defun claude-repl--ws-set-repl-state (ws state)
  "Set workspace WS's :repl-state to STATE.
STATE is one of: nil, :inactive.  Independent of the Claude-state axis."
  (unless ws (error "claude-repl--ws-set-repl-state: ws is nil"))
  (claude-repl--log ws "repl-state %s -> %s" ws state)
  (claude-repl--ws-put ws :repl-state state)
  (force-mode-line-update t))

(defun claude-repl--ws-claude-state-clear-if (ws state)
  "Clear WS's :claude-state when it currently equals STATE.
Compare-and-clear: no-op if the current value is not STATE."
  (unless ws (error "claude-repl--ws-claude-state-clear-if: ws is nil"))
  (if (eq (claude-repl--ws-get ws :claude-state) state)
      (progn
        (claude-repl--log ws "claude-state-clear-if %s %s -> nil" ws state)
        (claude-repl--ws-put ws :claude-state nil)
        (force-mode-line-update t))
    (claude-repl--log-verbose ws
                              "claude-state-clear-if ws=%s state=%s no-op (current=%s)"
                              ws state (claude-repl--ws-get ws :claude-state))))

;; Legacy APIs below delegate into the typed setters.  Call sites migrate
;; to the typed names in a later commit; retained here for the duration
;; of the migration so every existing caller keeps working.

(defun claude-repl--ws-set (ws state)
  "Set workspace WS to STATE.
Thin wrapper around `claude-repl--ws-set-claude-state' preserved for
callers that have not yet migrated to the typed setter.
STATE is one of: :thinking, :done, :permission, :inactive."
  (claude-repl--ws-set-claude-state ws state))

(defun claude-repl--ws-clear-if-status (ws state)
  "Clear status for workspace WS only if it currently equals STATE.
Thin wrapper around `claude-repl--ws-claude-state-clear-if'."
  (claude-repl--ws-claude-state-clear-if ws state))

(defalias 'claude-repl--ws-clear #'claude-repl--ws-clear-if-status
  "Backward-compatible alias for `claude-repl--ws-clear-if-status'.")

(defun claude-repl--ws-dir (ws)
  "Return the project root directory for workspace WS.
Reads :project-dir from the workspace plist.  Errors if not set."
  (or (claude-repl--ws-get ws :project-dir)
      (error "claude-repl--ws-dir: no :project-dir for workspace %s" ws)))

;;; Git status (async) -------------------------------------------------------

(defun claude-repl--workspace-clean-p (ws)
  "Return non-nil if workspace WS has no unstaged changes to tracked files.
Reads from a cached value updated asynchronously by
`claude-repl--async-refresh-git-status'.  Defaults to non-nil (clean) when
the cache has not yet been populated."
  (let ((result (not (eq (claude-repl--ws-get ws :git-clean) 'dirty))))
    (claude-repl--log-verbose ws "workspace-clean-p ws=%s result=%s" ws result)
    result))

(defun claude-repl--git-check-in-progress-p (ws)
  "Return non-nil if a git-diff process is already running for workspace WS."
  (let ((result (when-let ((proc (claude-repl--ws-get ws :git-proc)))
                  (process-live-p proc))))
    (claude-repl--log-verbose ws "git-check-in-progress-p ws=%s result=%s" ws result)
    result))

(defun claude-repl--git-diff-sentinel (ws proc _event)
  "Process sentinel for `git diff --quiet' in workspace WS.
When PROC finishes, records `:git-clean' as `clean' or `dirty' and
triggers a state update via `claude-repl--update-ws-state'.
_EVENT is ignored."
  (unless (process-live-p proc)
    (let* ((exit-code (process-exit-status proc))
           (clean-result (if (= 0 exit-code) 'clean 'dirty)))
      (claude-repl--log-verbose ws "git-diff-sentinel: ws=%s exit-code=%s result=%s" ws exit-code clean-result)
      (claude-repl--ws-put ws :git-clean clean-result)
      (claude-repl--ws-put ws :git-proc nil)
      (claude-repl--update-ws-state ws))))

(defun claude-repl--async-refresh-git-status (ws)
  "Asynchronously refresh the git cleanliness cache for workspace WS.
Starts `git diff --quiet' in WS's directory.  On exit, sets `:git-clean'
to `clean' or `dirty' in the workspace plist and calls
`claude-repl--update-ws-state' to apply any resulting state transition.
A no-op if a check is already in progress for WS."
  (when-let ((dir (claude-repl--ws-dir ws)))
    (if (claude-repl--git-check-in-progress-p ws)
        (claude-repl--log-verbose ws "async-refresh-git-status: ws=%s skipped (already in progress)" ws)
      (claude-repl--log-verbose ws "async-refresh-git-status: ws=%s starting git diff" ws)
      (let* ((default-directory dir)
             (proc (make-process
                    :name (format "claude-repl-git-%s" ws)
                    :command '("git" "diff" "--quiet")
                    :connection-type 'pipe
                    :noquery t
                    :buffer nil
                    :sentinel (apply-partially
                               #'claude-repl--git-diff-sentinel ws))))
        (claude-repl--ws-put ws :git-proc proc)))))

;;; Buffer/workspace resolution -----------------------------------------------

(defun claude-repl--workspace-for-buffer (buf)
  "Return the workspace name that contains BUF, or nil."
  (when (bound-and-true-p persp-mode)
    (cl-loop for persp in (persp-persps)
             when (persp-contain-buffer-p buf persp)
             return (safe-persp-name persp))))

;;; Tab-bar rendering ---------------------------------------------------------

;; Canonical color palette and display properties for status states.
;; :bg is used for unselected-tab face backgrounds.
;; :fg is the contrasting text color on that background.
;; :selected-fg is the foreground color used on the selected tab's bracket
;; (rendered against a light grey #c0c0c0 background, so brighter variants
;; are used where needed for readability).
;; :face is the defface symbol for unselected tabs.
;; :label, when present, overrides the numeric tab index.
(defconst claude-repl--status-colors
  '((:init       :bg "#3366cc" :fg "white" :selected-fg "#3366cc"
                 :face claude-repl-tab-init)
    (:thinking   :bg "#cc3333" :fg "white" :selected-fg "#cc3333"
                 :face claude-repl-tab-thinking)
    (:done       :bg "#1a7a1a" :fg "black" :selected-fg "#2a8c2a"
                 :face claude-repl-tab-done)
    (:permission :bg "#1a7a1a" :fg "black" :selected-fg "#2a8c2a"
                 :face claude-repl-tab-permission :label "❓")
    (:inactive   :bg "#cc8800" :fg "black" :selected-fg "#cc8800"
                 :face claude-repl-tab-inactive))
  "Alist mapping claude-state keywords to color and display properties.
`:idle' intentionally has no entry — an idle workspace renders with the
default tab face.  `:inactive' remains for the duration of the state-axis
migration and is removed once no writer produces it.")

(defun claude-repl--status-color (state prop)
  "Return property PROP for STATE from `claude-repl--status-colors'.
PROP is one of :bg, :fg, :selected-fg, :face, or :label.
Returns nil if STATE has no entry or PROP is not defined for it."
  (plist-get (alist-get state claude-repl--status-colors) prop))

(defmacro claude-repl--define-status-face (status doc)
  "Define a face `claude-repl-tab-STATUS' using colors from the status palette.
STATUS is an unquoted symbol (e.g. thinking, done).  DOC is the docstring."
  (let* ((face-name (intern (format "claude-repl-tab-%s" status)))
         (kw        (intern (format ":%s" status))))
    `(defface ,face-name
       `((t :background ,(claude-repl--status-color ,kw :bg)
            :foreground ,(claude-repl--status-color ,kw :fg)
            :weight bold))
       ,doc)))

(claude-repl--define-status-face init
  "Face for workspace tabs where Claude is initializing (blue).")

(claude-repl--define-status-face thinking
  "Face for workspace tabs where Claude is thinking (red).")

(claude-repl--define-status-face done
  "Face for workspace tabs where Claude is done (green).")

(claude-repl--define-status-face permission
  "Face for workspace tabs where Claude needs permission (green + emoji).")

(claude-repl--define-status-face inactive
  "Face for workspace tabs you've viewed but whose panels are closed (orange).")

(defun claude-repl--render-tab (name separator-face bracket-face name-face label img-str)
  "Render a tab string for workspace NAME.
SEPARATOR-FACE is applied to the leading space.
BRACKET-FACE is applied to the [LABEL] portion.
NAME-FACE is applied to the workspace name text.
IMG-STR, when non-nil, is inserted between bracket and name."
  (concat (propertize " " 'face separator-face)
          (propertize (format "[%s]" label) 'face bracket-face)
          (when img-str (concat " " img-str))
          (propertize (format " %s " name) 'face name-face)))

(defun claude-repl--render-selected-tab (name label state face img-str)
  "Render a selected tab for workspace NAME.
LABEL is the bracket content (number or emoji), STATE is the
status keyword (or nil), FACE is the resolved face for the tab
name, and IMG-STR is an optional priority image string."
  (let* ((bracket-fg (or (claude-repl--status-color state :selected-fg) "black"))
         (bg "#c0c0c0")
         (text-fg "black"))
    (claude-repl--render-tab
     name
     `(:background unspecified :foreground ,text-fg :weight bold)
     `(:foreground ,bracket-fg :weight bold :background ,bg)
     face
     label img-str)))

(defun claude-repl--render-unselected-tab (name label face img-str)
  "Render an unselected tab for workspace NAME.
LABEL is the bracket content (number or emoji), FACE is the
resolved face for the tab name, and IMG-STR is an optional
priority image string."
  (claude-repl--render-tab
   name
   '(:background unspecified)
   '(:foreground "#4477cc" :background unspecified :weight bold)
   face label img-str))

(defun claude-repl--tab-label (state index)
  "Return the tab label for STATE and numeric INDEX.
Uses a status-specific label when defined, otherwise the index as a string."
  (or (claude-repl--status-color state :label)
      (number-to-string index)))

(defun claude-repl--tab-face (state selected)
  "Return the face for a tab with STATE and SELECTED flag.
For selected tabs, only :permission overrides the default selected face;
other statuses (e.g. :thinking) are suppressed so the selected tab looks
normal.  For unselected tabs, returns the status face from
`claude-repl--status-colors', falling back to `+workspace-tab-face'."
  (if selected
      (if (eq state :permission)
          (claude-repl--status-color state :face)
        '+workspace-tab-selected-face)
    (or (claude-repl--status-color state :face)
        '+workspace-tab-face)))

(defun claude-repl--tab-priority-image-str (name)
  "Return a propertized image string for workspace NAME's priority, or nil."
  (when-let ((priority (claude-repl--ws-get name :priority)))
    (when-let ((img (claude-repl--priority-image priority)))
      (propertize " " 'display img))))

(defun claude-repl--composed-state (claude _repl)
  "Project CLAUDE (and optionally _REPL) onto the palette's display key.
Per user direction, `:repl-state' contributes no color — tab color is a
pure function of `:claude-state'.  The second argument is retained so
that callers can keep the pair-based call convention while we migrate,
and so a future feature can hook back into rendering without a signature
change.
Rule:
  :thinking   → :thinking                (red)
  :permission → :permission               (green + ❓)
  :inactive   → :inactive                 (legacy orange — kept until no
                                          writer produces it; see N20+)
  :init       → :init                     (blue — Claude starting)
  :done       → :done                     (green — unacknowledged work)
  :idle / nil / other → nil               (default face)"
  (cond
   ((eq claude :thinking)   :thinking)
   ((eq claude :permission) :permission)
   ((eq claude :inactive)   :inactive)
   ((eq claude :init)       :init)
   ((eq claude :done)       :done)
   (t                       nil)))

(defun claude-repl--ws-display-state (ws)
  "Return the palette display key for WS.
Reads `:claude-state' (the source of truth for tab color)."
  (claude-repl--composed-state (claude-repl--ws-claude-state ws)
                               (claude-repl--ws-repl-state ws)))

(defun claude-repl--render-tab-entry (name current-name index)
  "Render a single tab entry for workspace NAME.
CURRENT-NAME is the active workspace name.  INDEX is the 1-based
tab position.  The display state is composed from the two per-axis
keys via `claude-repl--ws-display-state'."
  (let* ((selected (equal current-name name))
         (state (claude-repl--ws-display-state name))
         (label (claude-repl--tab-label state index))
         (face (claude-repl--tab-face state selected))
         (img-str (claude-repl--tab-priority-image-str name)))
    (if selected
        (claude-repl--render-selected-tab name label state face img-str)
      (claude-repl--render-unselected-tab name label face img-str))))

(cl-defun claude-repl--tabline-advice (&optional (names nil names-supplied-p))
  "Override for `+workspace--tabline' to color tabs by Claude status."
  (let* ((names (if names-supplied-p names (+workspace-list-names)))
         (current-name (+workspace-current-name))
         (states (mapcar (lambda (n)
                           (cons n (claude-repl--ws-display-state n)))
                         names)))
    (claude-repl--log-verbose nil "tabline-advice: current=%s states=%S"
                              current-name states)
    (mapconcat
     #'identity
     (cl-loop for name in names
              for i from 1
              collect (claude-repl--render-tab-entry name current-name i))
     " ")))

(advice-add '+workspace--tabline :override #'claude-repl--tabline-advice)

;; Suppress the echo area flash when switching workspaces.
;; Doom calls (+workspace/display) after switch/cycle/new/load, which uses
;; (message ...) to show the tabline in the echo area.  Since tabs are
;; already visible at the top, the bottom flash is redundant.
(advice-add '+workspace/display :override #'ignore)

;;; Claude panel visibility ---------------------------------------------------

;; Walk saved window-configuration tree to find claude buffers.
(defun claude-repl--wconf-has-claude-p (wconf)
  "Return non-nil if WCONF (a `window-state-get' tree) contains a claude buffer."
  (when (and wconf (proper-list-p wconf))
    (let ((buf-entry (alist-get 'buffer wconf)))
      (if (and buf-entry (stringp (car-safe buf-entry))
               (string-match-p claude-repl--vterm-buffer-re (car buf-entry)))
          t
        (cl-some #'claude-repl--wconf-has-claude-p
                 (cl-remove-if-not #'proper-list-p wconf))))))

(defun claude-repl--visible-claude-buffer-p (buf)
  "Return non-nil if BUF is a live, visible Claude buffer."
  (and (buffer-live-p buf)
       (claude-repl--claude-buffer-p buf)
       (get-buffer-window buf)))

(defun claude-repl--claude-visible-in-current-ws-p ()
  "Return non-nil if a claude buffer is visible in the current workspace."
  (cl-some #'claude-repl--visible-claude-buffer-p
           (buffer-list)))

(defun claude-repl--claude-in-saved-wconf-p (ws-name)
  "Return non-nil if background workspace WS-NAME has a claude buffer in its saved config."
  (let* ((persp (persp-get-by-name ws-name))
         (wconf (and persp (not (symbolp persp)) (persp-window-conf persp))))
    (claude-repl--wconf-has-claude-p wconf)))

(defun claude-repl--ws-claude-open-p (ws-name)
  "Return non-nil if workspace WS-NAME has a claude buffer in its window layout.
For the current workspace, checks live windows.
For background workspaces, inspects the saved persp window configuration."
  (if (equal ws-name (+workspace-current-name))
      (claude-repl--claude-visible-in-current-ws-p)
    (claude-repl--claude-in-saved-wconf-p ws-name)))

;;; State machine ------------------------------------------------------------

(defun claude-repl--update-ws-state (ws)
  "Decay WS's claude-state from :done to :idle when the worktree is clean.

This is the sole transition the timer drives on the claude-state axis.
Every other transition is sentinel-owned (see the hook handlers in
`sentinel.el').  When Claude finishes a turn the Stop hook writes
`:done'; if the worktree is clean (nothing was changed, or the user
has staged/committed), there is nothing outstanding and the tab decays
to `:idle' (default face, no attention needed).  If the worktree is
dirty, the tab stays green until the user stages or commits.

State table:
  :done + clean → :idle         (this function)
  :done + dirty → unchanged     (wait for user to stage/commit)
  anything else → unchanged     (sentinel-owned or already terminal)"
  (let ((state (claude-repl--ws-claude-state ws))
        (dirty (not (claude-repl--workspace-clean-p ws))))
    (cond
     ((and (eq state :done) (not dirty))
      (claude-repl--log ws "update-ws-state ws=%s :done->:idle (clean)" ws)
      (claude-repl--ws-set-claude-state ws :idle))
     (t
      (claude-repl--log-verbose ws "update-ws-state ws=%s state=%s dirty=%s no-op"
                                ws state dirty)))))

(defun claude-repl--update-all-workspace-states ()
  "Update state for claude-repl workspaces based on visibility and git status.
Only iterates workspaces registered in `claude-repl--workspaces' (not all
persp workspaces), since non-claude workspaces have no state to manage.
Uses cached git status (`:git-clean') and kicks off async refreshes.
Also polls for orphaned sentinel files that file-notify may have missed.
State machine runs whenever a live vterm process exists, regardless of
panel visibility (panels may be hidden via `SPC o c')."
  (claude-repl--poll-workspace-notifications)
  (let ((ws-names (hash-table-keys claude-repl--workspaces)))
    (claude-repl--log-verbose nil "update-all-workspace-states: count=%d" (length ws-names))
    (dolist (ws ws-names)
      (if (claude-repl--vterm-running-p ws)
          (progn
            (claude-repl--update-ws-state ws)
            (claude-repl--async-refresh-git-status ws))
        ;; No live vterm process → clear non-thinking state
        (claude-repl--maybe-clear-stale-state ws)))))

;; Periodically update all workspace states (catches git changes, etc.)
(push (run-with-timer 1 1 #'claude-repl--update-all-workspace-states)
      claude-repl--timers)

(defun claude-repl--maybe-clear-stale-state (ws)
  "Clear WS's claude-state when no vterm process is running.
Preserves :thinking (which is managed by the input/sentinel lifecycle).
Scheduled for replacement with a :repl-state :dead transition — see
follow-up commit."
  (let ((state (claude-repl--ws-claude-state ws)))
    (if (and state (not (eq state :thinking)))
        (progn
          (claude-repl--log ws "maybe-clear-stale-state: ws=%s clearing %s (no vterm)" ws state)
          (claude-repl--ws-put ws :claude-state nil)
          (force-mode-line-update t))
      (claude-repl--log-verbose ws "maybe-clear-stale-state: ws=%s no-op (state=%s)" ws state))))

;;; Frame focus handler -------------------------------------------------------

(defun claude-repl--on-frame-focus ()
  "Refresh claude vterm and update all workspace states when Emacs regains focus."
  (if (frame-focus-state)
      (progn
        (claude-repl--log nil "on-frame-focus: focused")
        (claude-repl--refresh-vterm)
        (claude-repl--update-all-workspace-states))
    (claude-repl--log-verbose nil "on-frame-focus: not focused")))

(add-function :after after-focus-change-function #'claude-repl--on-frame-focus)


