;;; status.el --- workspace status state machine and tab bar rendering -*- lexical-binding: t; -*-

;;; Code:

;;; Priority badge images
;;
;; Each image is a small PNG loaded from the module's images/ directory and
;; scaled to fit the tab-bar line height.

(defcustom claude-repl-priority-levels '("p05" "p1" "p2" "p3")
  "List of recognized priority level strings for workspace badges."
  :type '(repeat string)
  :group 'claude-repl)

(defcustom claude-repl-tab-bracket-format "[%s]"
  "Format string for tab bracket labels.
%s is replaced with the tab index number or emoji."
  :type 'string
  :group 'claude-repl)

(defcustom claude-repl-tab-name-padding " %s "
  "Format string for tab workspace name padding."
  :type 'string
  :group 'claude-repl)

(defcustom claude-repl-state-poll-interval 1
  "Seconds between workspace state update polls."
  :type 'integer
  :group 'claude-repl)

(defvar claude-repl--priority-images nil
  "Alist mapping priority strings (\"p05\" \"p1\" \"p2\" \"p3\") to Emacs image specs.")

;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;; !! DO NOT REMOVE `claude-repl--tabline-space-toggle' OR ITS USAGE   !!
;; !! IN `claude-repl--tabline-advice' AND                             !!
;; !! `claude-repl--update-all-workspace-states'.                      !!
;; !!                                                                  !!
;; !! The tab-bar will NOT repaint unless the string it displays       !!
;; !! actually changes between ticks.  Toggling a trailing space on    !!
;; !! every poll cycle forces the tab-bar to detect a "new" string     !!
;; !! and re-render, giving us real-time visual updates.  Without      !!
;; !! this, state-color changes (thinking → done, etc.) are invisible  !!
;; !! until the user manually triggers a redisplay.                    !!
;; !!                                                                  !!
;; !! This has been accidentally removed multiple times.  DO NOT       !!
;; !! remove it again.  It is NOT dead code.  It is NOT cosmetic.     !!
;; !! It is the mechanism that makes tab-bar updates work.             !!
;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
(defvar claude-repl--tabline-space-toggle nil
  "Non-nil means append an extra trailing space to the tabline string.
Flipped every poll cycle by `claude-repl--update-all-workspace-states'.
Read by `claude-repl--tabline-advice' to produce an alternating string
that forces the tab-bar to repaint.  DO NOT REMOVE — see comment above.")

(defun claude-repl--load-priority-images ()
  "Load priority badge PNGs from the module images/ directory.
Populates `claude-repl--priority-images' with display-ready image specs."
  (let* ((dir (file-name-directory (or load-file-name buffer-file-name)))
         (img-dir (expand-file-name "images/" dir))
         (names claude-repl-priority-levels)
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
;;                   (initialize-claude writes :init; kill clears).
;;   :repl-state   — Emacs-owned session-lifecycle flag.  Values:
;;                     nil       — workspace registered, no Claude
;;                                 session has ever been attached.
;;                     :active   — panels open, session running.
;;                     :inactive — panels closed, session preserved.
;;                     :dead     — vterm process has died.
;;                   Only :dead contributes to tab display (❌ badge);
;;                   other values are bookkeeping only.

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
STATE is one of:
  nil        — freshly killed / no session
  :active    — panels displayed, session alive
  :inactive  — panels hidden, session alive
  :dead      — vterm process gone
  :viewed    — panels displayed AND user has selected this workspace
               since `:claude-state' entered `:done'.  Acts as a gate
               for decay :done → :idle in `update-ws-state'."
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

(defun claude-repl--ws-dir (ws)
  "Return the project root directory for workspace WS.
Reads :project-dir from the workspace plist.  Errors if not set."
  (or (claude-repl--ws-get ws :project-dir)
      (error "claude-repl--ws-dir: no :project-dir for workspace %s" ws)))

;;; Git status (async) -------------------------------------------------------

(defun claude-repl--workspace-clean-p (ws)
  "Return non-nil if workspace WS has no unstaged changes to tracked files.
Reads from a cached value updated asynchronously by
`claude-repl--async-refresh-git-status'.  Signals an error if the cache
has not yet been populated — callers must ensure the async git check has
completed before consulting this predicate."
  (let ((status (claude-repl--ws-get ws :git-clean)))
    (unless status
      (error "claude-repl--workspace-clean-p: :git-clean not populated for workspace %s" ws))
    (let ((result (eq status 'clean)))
      (claude-repl--log-verbose ws "workspace-clean-p ws=%s status=%s result=%s" ws status result)
      result)))

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
           (clean-result (cond
                          ((= 0 exit-code) 'clean)
                          ((= 1 exit-code) 'dirty)
                          (t (message "[claude-repl] WARNING: git diff --quiet exited with code %d for ws=%s (git error, not dirty)"
                                      exit-code ws)
                             (claude-repl--log ws "git-diff-sentinel: unexpected exit-code=%d for ws=%s" exit-code ws)
                             nil))))
      (claude-repl--log-verbose ws "git-diff-sentinel: ws=%s exit-code=%s result=%s" ws exit-code clean-result)
      (when clean-result
        (claude-repl--ws-put ws :git-clean clean-result))
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
;;
;; Appearance is described by a small pyramid:
;;
;;   1. Named constants — every color / label / font-weight literal lives
;;      in a `claude-repl--color-*' / `--label-*' / `--tab-weight' defconst.
;;   2. `claude-repl--tab-default' and `claude-repl--tab-palette' — the
;;      two defconsts that compose those named values into per-state
;;      appearance specs.  No palette row contains a string literal.
;;   3. Faces — four `defface' forms that reference the same named
;;      constants (Doom theming hook).
;;   4. Renderers — take a spec, emit a propertized string.
;;
;; Palette shape (per-state):
;;   :face       — defface name for unselected tabs.
;;   :label      — optional bracket content override (e.g. the permission
;;                 glyph).
;;   :unselected — plist describing unselected appearance.
;;   :selected   — plist describing selected appearance.  May include
;;                 :face-override to replace the NAME face in selected
;;                 form (used by :permission to keep its green badge
;;                 visible on the selected tab).
;;
;; Spec plist keys:
;;   :bg          — bracket (and separator) background.
;;   :fg          — separator foreground.
;;   :bracket-fg  — [LABEL] foreground.
;;   :bracket-bg  — [LABEL] background (optional; falls back to :bg).
;;   :weight      — font weight (default `bold').
;;
;; Use `unspecified' (the symbol) for "inherit from frame default".

;; --- Named color / style constants --- ;;

(defconst claude-repl--color-init-blue        "#3366cc"
  "Blue used for the :init claude-state tab background.")

(defconst claude-repl--color-thinking-red     "#cc3333"
  "Red used for the :thinking claude-state tab background.")

(defconst claude-repl--color-done-green       "#1a7a1a"
  "Dark green used for :done and :permission tab backgrounds.")

(defconst claude-repl--color-idle-orange      "#d97706"
  "Orange used for the :idle claude-state tab background.
:idle means \"session alive, awaiting prompt or decayed from :done\" — an
explicit palette entry (not a fallback) so idle workspaces are
visually distinct from states that have no palette mapping.")

(defconst claude-repl--color-done-green-bright "#2a8c2a"
  "Brighter green used for :done / :permission bracket-fg on selected
tabs; readable against `claude-repl--color-selected-bg'.")

(defconst claude-repl--color-default-bracket  "white"
  "White used for bracket numerals on unselected tabs of any state.")

(defconst claude-repl--color-selected-bg      "#c0c0c0"
  "Grey used for the background of selected tabs.")

(defconst claude-repl--color-light            "white"
  "Light foreground for dark state backgrounds.")

(defconst claude-repl--color-dark             "black"
  "Dark foreground for light state backgrounds.")

(defconst claude-repl--label-permission       "❓"
  "Bracket label shown adjacent to the numeric index when Claude is
asking for a permission decision.")

(defconst claude-repl--label-dead             "❌"
  "Bracket label shown adjacent to the numeric index when the vterm
process has died.")

(defconst claude-repl--tab-weight             'bold
  "Font weight applied to every tab face.")

;; --- Appearance palette --- ;;

(defconst claude-repl--tab-default
  `(:unselected (:bg unspecified
                 :fg unspecified
                 :bracket-fg ,claude-repl--color-default-bracket
                 :weight ,claude-repl--tab-weight)
    :selected   (:bg ,claude-repl--color-selected-bg
                 :fg ,claude-repl--color-dark
                 :bracket-fg ,claude-repl--color-dark
                 :weight ,claude-repl--tab-weight))
  "Default tab-appearance spec for states absent from `claude-repl--tab-palette'.")

(defconst claude-repl--tab-palette
  `((:init
     :face       claude-repl-tab-init
     :unselected (:bg ,claude-repl--color-init-blue
                  :fg ,claude-repl--color-light
                  :bracket-fg ,claude-repl--color-default-bracket
                  :weight ,claude-repl--tab-weight)
     :selected   (:bg ,claude-repl--color-selected-bg
                  :fg ,claude-repl--color-dark
                  :bracket-bg ,claude-repl--color-init-blue
                  :bracket-fg ,claude-repl--color-light
                  :weight ,claude-repl--tab-weight))
    (:thinking
     :face       claude-repl-tab-thinking
     :unselected (:bg ,claude-repl--color-thinking-red
                  :fg ,claude-repl--color-light
                  :bracket-fg ,claude-repl--color-default-bracket
                  :weight ,claude-repl--tab-weight)
     :selected   (:bg ,claude-repl--color-selected-bg
                  :fg ,claude-repl--color-dark
                  :bracket-bg ,claude-repl--color-thinking-red
                  :bracket-fg ,claude-repl--color-light
                  :weight ,claude-repl--tab-weight))
    (:done
     :face       claude-repl-tab-done
     :unselected (:bg ,claude-repl--color-done-green
                  :fg ,claude-repl--color-dark
                  :bracket-fg ,claude-repl--color-default-bracket
                  :weight ,claude-repl--tab-weight)
     :selected   (:bg ,claude-repl--color-selected-bg
                  :fg ,claude-repl--color-dark
                  :bracket-bg ,claude-repl--color-done-green
                  :bracket-fg ,claude-repl--color-light
                  :weight ,claude-repl--tab-weight))
    (:permission
     :face       claude-repl-tab-permission
     :label      ,claude-repl--label-permission
     :unselected (:bg ,claude-repl--color-done-green
                  :fg ,claude-repl--color-dark
                  :bracket-fg ,claude-repl--color-default-bracket
                  :weight ,claude-repl--tab-weight)
     :selected   (:bg ,claude-repl--color-selected-bg
                  :fg ,claude-repl--color-dark
                  :bracket-bg ,claude-repl--color-done-green
                  :bracket-fg ,claude-repl--color-light
                  :weight ,claude-repl--tab-weight
                  :face-override claude-repl-tab-permission))
    (:idle
     :face       claude-repl-tab-idle
     :unselected (:bg ,claude-repl--color-idle-orange
                  :fg ,claude-repl--color-dark
                  :bracket-fg ,claude-repl--color-default-bracket
                  :weight ,claude-repl--tab-weight)
     :selected   (:bg ,claude-repl--color-selected-bg
                  :fg ,claude-repl--color-dark
                  :bracket-bg ,claude-repl--color-idle-orange
                  :bracket-fg ,claude-repl--color-light
                  :weight ,claude-repl--tab-weight))
    (:dead
     :label      ,claude-repl--label-dead))
  "Per-state tab-appearance palette.
Each entry fully describes both selected and unselected looks for a
claude-state keyword via nested `:unselected' and `:selected' plists.
`:repl-state :inactive' does not contribute to color (it is bookkeeping
only).")

(defun claude-repl--tab-spec (state selected)
  "Return the appearance spec (plist) for STATE with SELECTED flag.
Falls back to `claude-repl--tab-default' when STATE has no palette entry.
Keys in the returned plist: :bg :fg :bracket-fg :bracket-bg :weight
and optionally :face-override."
  (let* ((row (alist-get state claude-repl--tab-palette))
         (key (if selected :selected :unselected)))
    (or (plist-get row key)
        (plist-get claude-repl--tab-default key))))

;; --- defface forms referencing the named constants --- ;;
;; Each `:unselected' palette row has the same colors these forms read,
;; by construction.  Kept as explicit defface calls so Doom users can
;; customize via `customize-face' (the Doom theming hook).

(defface claude-repl-tab-init
  `((t :background ,claude-repl--color-init-blue
       :foreground ,claude-repl--color-light
       :weight ,claude-repl--tab-weight))
  "Face for workspace tabs where Claude is initializing (blue).")

(defface claude-repl-tab-thinking
  `((t :background ,claude-repl--color-thinking-red
       :foreground ,claude-repl--color-light
       :weight ,claude-repl--tab-weight))
  "Face for workspace tabs where Claude is thinking (red).")

(defface claude-repl-tab-done
  `((t :background ,claude-repl--color-done-green
       :foreground ,claude-repl--color-dark
       :weight ,claude-repl--tab-weight))
  "Face for workspace tabs where Claude is done (green).")

(defface claude-repl-tab-permission
  `((t :background ,claude-repl--color-done-green
       :foreground ,claude-repl--color-dark
       :weight ,claude-repl--tab-weight))
  "Face for workspace tabs where Claude needs permission (green + emoji).")

(defface claude-repl-tab-idle
  `((t :background ,claude-repl--color-idle-orange
       :foreground ,claude-repl--color-dark
       :weight ,claude-repl--tab-weight))
  "Face for workspace tabs where Claude is idle (orange).")

(defun claude-repl--render-tab (name spec label name-face img-str)
  "Render a tab string for workspace NAME from SPEC.
SPEC is a plist with keys :bg :fg :bracket-fg :weight (see
`claude-repl--tab-palette' docstring).  NAME-FACE is applied to the
workspace-name portion.  LABEL is the bracket content (number or
emoji).  IMG-STR, when non-nil, is inserted between bracket and name."
  (let* ((bg         (or (plist-get spec :bg)         'unspecified))
         (fg         (or (plist-get spec :fg)         'unspecified))
         (bracket-bg (or (plist-get spec :bracket-bg) bg))
         (bracket-fg (or (plist-get spec :bracket-fg) 'unspecified))
         (weight     (or (plist-get spec :weight)     'normal))
         (separator-face `(:background unspecified :foreground ,fg :weight ,weight))
         (bracket-face   `(:background ,bracket-bg  :foreground ,bracket-fg :weight ,weight)))
    (concat (propertize " " 'face separator-face)
            (propertize (format claude-repl-tab-bracket-format label) 'face bracket-face)
            (when img-str (concat " " img-str))
            (propertize (format claude-repl-tab-name-padding name) 'face name-face))))

(defun claude-repl--tab-label (state index)
  "Return the tab label for STATE and numeric INDEX.
When the palette defines a `:label' for STATE (e.g. \"❓\" for permission,
\"❌\" for dead), the suffix is appended after the numeric index
\(e.g. \"1❓\").  Otherwise returns the index as a plain string."
  (let ((suffix (plist-get (alist-get state claude-repl--tab-palette) :label)))
    (if suffix
        (concat (number-to-string index) suffix)
      (number-to-string index))))

(defun claude-repl--tab-face (state selected)
  "Return the face symbol for the NAME portion of a tab.
For unselected tabs, uses the palette row's `:face' or falls back to
`+workspace-tab-face'.  For selected tabs, uses the state's
`:selected :face-override' when present (e.g. `:permission' keeps its
green badge visible), or falls back to `+workspace-tab-selected-face'."
  (let ((row (alist-get state claude-repl--tab-palette)))
    (if selected
        (or (plist-get (plist-get row :selected) :face-override)
            '+workspace-tab-selected-face)
      (or (plist-get row :face)
          '+workspace-tab-face))))

(defun claude-repl--tab-priority-image-str (name)
  "Return a propertized image string for workspace NAME's priority, or nil."
  (when-let ((priority (claude-repl--ws-get name :priority)))
    (when-let ((img (claude-repl--priority-image priority)))
      (propertize " " 'display img))))

(defun claude-repl--composed-state (claude repl &optional ws)
  "Project CLAUDE and REPL onto the palette's display key.
Tab color is primarily a function of `:claude-state'.  The one
`:repl-state' value that contributes is `:dead' — when the vterm
process has died (`:claude-state' nil, `:repl-state' :dead), the tab
shows an ❌ badge.

Optional WS is threaded through for diagnostic logging only; it does
not affect the mapping.

Every known claude-state is mapped explicitly.  Unknown states error
hard — no silent fallback.

Rule:
  :thinking   → :thinking                (red)
  :permission → :permission               (green + ❓)
  :init       → :init                     (blue — Claude starting)
  :done       → :done                     (green — unacknowledged work)
  :idle       → :idle                     (orange)
  nil + :dead → :dead                     (default + ❌)
  nil         → nil                       (no session / unborn)"
  (cond
   ((eq claude :thinking)   :thinking)
   ((eq claude :permission) :permission)
   ((eq claude :init)       :init)
   ((eq claude :done)       :done)
   ((eq claude :idle)       :idle)
   ((and (null claude) (eq repl :dead)) :dead)
   ((null claude)           nil)
   (t
    ;; DIAG: we expected this branch to be unreachable — every writer of
    ;; :claude-state goes through the typed setter and passes one of the
    ;; documented keywords. If we hit this, some other path is leaking an
    ;; unexpected value into the hashmap. Log once per value and return
    ;; nil so redisplay doesn't blank the tab-bar while we investigate.
    (claude-repl--log ws
                      "composed-state: UNKNOWN claude-state %S (type=%s) — returning nil"
                      claude (type-of claude))
    nil)))

(defun claude-repl--ws-display-state (ws)
  "Return the palette display key for WS.
Reads `:claude-state' (the source of truth for tab color)."
  (claude-repl--composed-state (claude-repl--ws-claude-state ws)
                               (claude-repl--ws-repl-state ws)
                               ws))

(defun claude-repl--render-tab-entry (name current-name index)
  "Render a single tab entry for workspace NAME.
CURRENT-NAME is the active workspace name.  INDEX is the 1-based
tab position.  The display state is composed from the two per-axis
keys via `claude-repl--ws-display-state'; the appearance spec is
resolved via `claude-repl--tab-spec'."
  (let* ((selected (equal current-name name))
         (state    (claude-repl--ws-display-state name))
         (spec     (claude-repl--tab-spec state selected))
         (label    (claude-repl--tab-label state index))
         (face     (claude-repl--tab-face state selected))
         (img-str  (claude-repl--tab-priority-image-str name)))
    (claude-repl--render-tab name spec label face img-str)))

(cl-defun claude-repl--tabline-advice (&optional (names nil names-supplied-p))
  "Override for `+workspace--tabline' to color tabs by Claude status."
  (let* ((names (if names-supplied-p names (+workspace-list-names)))
         (current-name (+workspace-current-name))
         (states (mapcar (lambda (n)
                           (cons n (claude-repl--ws-display-state n)))
                         names)))
    (claude-repl--log-verbose nil "tabline-advice: current=%s states=%S"
                              current-name states)
    (concat
     (mapconcat
      #'identity
      (cl-loop for name in names
               for i from 1
               collect (claude-repl--render-tab-entry name current-name i))
      " ")
     ;; Trailing space toggle — DO NOT REMOVE.  See the block comment
     ;; above `claude-repl--tabline-space-toggle' for why this exists.
     (if claude-repl--tabline-space-toggle " " ""))))

(advice-add '+workspace--tabline :override #'claude-repl--tabline-advice)

;; Suppress the echo area flash when switching workspaces.
;; Doom calls (+workspace/display) after switch/cycle/new/load, which uses
;; (message ...) to show the tabline in the echo area.  Since tabs are
;; already visible at the top, the bottom flash is redundant.
(advice-add '+workspace/display :override #'ignore)

;;; Claude panel visibility ---------------------------------------------------

;; Walk saved window-configuration tree to find claude buffers.
(defun claude-repl--wconf-has-claude-p (wconf)
  "Return non-nil if WCONF (a `window-state-get' tree) contains a claude vterm buffer.
Excludes input buffers: presence of only the input panel in a saved
config (e.g. from a placeholder layout) should not count as claude open."
  (when (and wconf (proper-list-p wconf))
    (let ((buf-entry (alist-get 'buffer wconf)))
      (if (and buf-entry (stringp (car-safe buf-entry))
               (string-match-p claude-repl--vterm-buffer-re (car buf-entry))
               (not (string-match-p claude-repl--input-buffer-re (car buf-entry))))
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
  "Decay WS's claude-state from :done to :idle when conditions are met.

This is the sole transition the timer drives on the claude-state axis.
Every other transition is sentinel-owned (see the hook handlers in
`sentinel.el').  When Claude finishes a turn the Stop hook writes
`:done'; if the worktree is clean AND the user has viewed the
workspace (`:repl-state :viewed'), there is nothing outstanding and
the tab decays to `:idle'.  If the worktree is dirty OR the user has
not yet viewed the workspace, the tab stays green.

Decay also resets `:repl-state' from `:viewed' back to `:active' so a
future :done cycle starts from a clean slate.

State table:
  :done + clean + :viewed → :idle   (this function)
  :done + clean + !viewed → unchanged (wait for user to view)
  :done + dirty           → unchanged (wait for user to stage/commit)
  anything else           → unchanged (sentinel-owned or already terminal)"
  (let ((state (claude-repl--ws-claude-state ws))
        (repl-state (claude-repl--ws-repl-state ws))
        (git-status (claude-repl--ws-get ws :git-clean)))
    (cond
     ((null git-status)
      (claude-repl--log-verbose ws "update-ws-state ws=%s state=%s git-clean not yet populated, skipping" ws state))
     ((and (eq state :done) (eq git-status 'clean) (eq repl-state :viewed))
      (claude-repl--log ws "update-ws-state ws=%s :done->:idle (clean, viewed)" ws)
      (claude-repl--ws-set-claude-state ws :idle)
      (claude-repl--ws-set-repl-state ws :active))
     (t
      (claude-repl--log-verbose ws "update-ws-state ws=%s state=%s repl-state=%s git-status=%s no-op"
                                ws state repl-state git-status)))))

(defun claude-repl--update-all-workspace-states ()
  "Update state for claude-repl workspaces based on visibility and git status.
Only iterates workspaces registered in `claude-repl--workspaces' (not all
persp workspaces), since non-claude workspaces have no state to manage.
Uses cached git status (`:git-clean') and kicks off async refreshes.
Also polls for orphaned sentinel files that file-notify may have missed.
State machine runs whenever a live vterm process exists, regardless of
panel visibility (panels may be hidden via `SPC o c')."
  ;; Flip the trailing-space toggle so the tab-bar string changes on every
  ;; tick, forcing a repaint.  DO NOT REMOVE — see the block comment above
  ;; `claude-repl--tabline-space-toggle'.
  (setq claude-repl--tabline-space-toggle (not claude-repl--tabline-space-toggle))
  (claude-repl--poll-workspace-notifications)
  (let ((ws-names (hash-table-keys claude-repl--workspaces)))
    (claude-repl--log-verbose nil "update-all-workspace-states: count=%d" (length ws-names))
    (dolist (ws ws-names)
      (if (claude-repl--claude-running-p ws)
          (progn
            (claude-repl--update-ws-state ws)
            (claude-repl--async-refresh-git-status ws))
        ;; No live vterm process → clear non-thinking state
        (claude-repl--mark-dead-vterm ws)))))

;; Periodically update all workspace states (catches git changes, etc.)
(push (run-with-timer claude-repl-state-poll-interval claude-repl-state-poll-interval #'claude-repl--update-all-workspace-states)
      claude-repl--timers)

(defun claude-repl--mark-dead-vterm (ws)
  "Record that WS's vterm process is no longer running.
Sets `:repl-state :dead' and clears `:claude-state'.  This is a
documented lifecycle-cleanup exception to the sentinel-only writer
rule: no hook will ever fire again for a dead process, so Emacs is
the only observer that can reset state.

No-op in two cases:
- `:repl-state' is already `:dead' (idempotent on the poll path).
- `:claude-state' is `:init' — Claude is starting, the vterm process
  may not have reached running state yet, and observing no process
  does not mean dead.  The session-start hook will transition away
  from `:init' shortly; until then the timer leaves things alone."
  (unless (or (eq (claude-repl--ws-repl-state ws) :dead)
              (eq (claude-repl--ws-claude-state ws) :init))
    (claude-repl--log ws "mark-dead-vterm: ws=%s claude-state=%s -> :dead"
                      ws (claude-repl--ws-claude-state ws))
    (claude-repl--ws-put ws :repl-state :dead)
    (claude-repl--ws-put ws :claude-state nil)
    (force-mode-line-update t)))

;;; Frame focus handler -------------------------------------------------------

(defun claude-repl--on-frame-focus ()
  "Refresh claude vterm and update all workspace states when Emacs regains focus."
  (if (frame-focus-state)
      (progn
        (claude-repl--log (+workspace-current-name) "on-frame-focus: focused")
        (claude-repl--refresh-vterm)
        (claude-repl--update-all-workspace-states))
    (claude-repl--log-verbose (+workspace-current-name) "on-frame-focus: not focused")))

(add-function :after after-focus-change-function #'claude-repl--on-frame-focus)


