;;; hide-project-dirs.el --- Hide workspaces whose project-dir lives under configured prefixes -*- lexical-binding: t; -*-

;;; Commentary:

;; Provides a toggleable mode that hides any workspace whose `:project-dir'
;; lives under one of the prefix directories listed in
;; `claude-repl-hide-project-dirs'.
;;
;; Hiding happens at the PERSP layer, not at render time: when the mode is
;; toggled ON every matching workspace is legitimately killed via
;; `claude-repl--nuke-one-workspace' (Claude session, buffers, and persp
;; all torn down) and tombstoned with a `:hidden-project-dir' marker.
;; Because the matching workspaces leave `persp-names-cache' entirely, the
;; surviving tab numbers stay contiguous and `SPC <n>' resolves to the
;; workspace the user actually sees — the bug a render-only filter left
;; behind (a tab nominally at position 3 still switched to a hidden
;; workspace).
;;
;; Toggling the mode OFF re-establishes every marked tombstone via
;; `claude-repl--establish-workspace' and moves the restored workspaces to
;; the front of the tab-bar list.
;;
;; The current workspace is always retained so the user can never
;; accidentally render themselves invisible to themselves.
;;
;; Both the toggle state and the per-workspace `:hidden-project-dir'
;; markers are persisted to the workspace snapshot so a session restore
;; (startup or from an archive) reconstructs the hidden set.
;;
;; Default prefix is `~/workspace/ChessCom' — chosen so day-job repos can
;; be hidden in one keystroke when paged off them.
;;
;; Toggle via `claude-repl-toggle-hide-project-dirs' (bound under `SPC o H'
;; in `keybindings.el').

;;; Code:

(require 'subr-x)
(require 'cl-lib)

;; Forward declarations: defined in earlier-loaded modules (`core.el',
;; `commands.el').  Named here so the references below read cleanly at
;; compile/load time.
(defvar claude-repl--workspaces)
(defvar claude-repl--snapshot-load-state)

;;;; Customization ----------------------------------------------------------

(defcustom claude-repl-hide-project-dirs
  (list (expand-file-name "~/workspace/ChessCom"))
  "List of directory prefixes whose workspaces are hidden when the mode is on.
A workspace is hidden when its `:project-dir' (after canonicalization
via `claude-repl--path-canonical') lives under any entry on this
list.  Path comparison is prefix-based against the canonical
directory string so both the prefix itself and any nested worktree
beneath it are matched.

Default is `~/workspace/ChessCom' — toggle the mode via
`claude-repl-toggle-hide-project-dirs' (bound to `SPC o H')."
  :type '(repeat directory)
  :group 'claude-repl)

(defvar claude-repl-hide-project-dirs-enabled nil
  "Non-nil means workspaces matching a hide prefix are killed, not shown.
Prefixes are configured via `claude-repl-hide-project-dirs'.  When the
mode is toggled ON every matching workspace is killed at the persp
layer and tombstoned with `:hidden-project-dir'; toggling OFF
re-establishes those tombstones.  This flag records which side of the
toggle the runtime is currently on and is persisted to the workspace
snapshot so a session restore reconstructs it.

Toggle via `claude-repl-toggle-hide-project-dirs'.")

;;;; Predicate --------------------------------------------------------------

(defun claude-repl--hide-project-dirs--canonical-prefixes ()
  "Return `claude-repl-hide-project-dirs' canonicalized for prefix comparison.
Each entry is expanded, run through `claude-repl--path-canonical' (so
tildes / symlinks resolve consistently with `:project-dir' values),
then has a trailing slash appended so prefix matching can't match a
sibling directory whose name happens to share the prefix's leading
characters (e.g. `~/workspace/ChessCom-archive' must not match the
`~/workspace/ChessCom' prefix)."
  (cl-loop for raw in claude-repl-hide-project-dirs
           for canonical = (ignore-errors
                             (claude-repl--path-canonical
                              (expand-file-name raw)))
           when (and canonical (not (string-empty-p canonical)))
           collect (file-name-as-directory canonical)))

(defun claude-repl--hide-project-dirs--ws-matches-p (ws)
  "Return non-nil when workspace WS's project-dir lives under a hide prefix.
Always returns nil when WS has no registered `:project-dir' — entries
without a project-dir can't be classified, so they pass through the
filter and remain visible."
  (when-let* ((dir (claude-repl--ws-get ws :project-dir))
              (canonical (ignore-errors
                           (claude-repl--path-canonical dir)))
              (with-slash (file-name-as-directory canonical)))
    (cl-some (lambda (prefix)
               (string-prefix-p prefix with-slash))
             (claude-repl--hide-project-dirs--canonical-prefixes))))

;;;; Workspace selection ----------------------------------------------------

(defun claude-repl--hide-project-dirs--matching-live-workspaces ()
  "Return live workspace names whose project-dir lives under a hide prefix.
The current workspace is excluded — hiding never kills the workspace
the user is sitting in.  Only live (non-tombstoned) workspaces are
returned, since tombstones have no persp left to kill."
  (let ((current (claude-repl--ws-current-name)))
    (cl-remove-if-not
     (lambda (ws)
       (and (not (equal ws current))
            (claude-repl--hide-project-dirs--ws-matches-p ws)))
     (claude-repl--live-ws-names))))

(defun claude-repl--hide-project-dirs--hidden-workspace-names ()
  "Return names of workspaces carrying the `:hidden-project-dir' marker.
These are the tombstones killed by a prior hide toggle — the set that
`claude-repl--hide-project-dirs--restore' brings back.  Returned in
name order so restore is deterministic.

Thin wrapper over `claude-repl--ws-hide-tombstoned-names' (the
workspace.el integration boundary for hide-reason tombstones); this
file no longer pokes `claude-repl--workspaces' directly per the
\"Workspace state encapsulation\" rule in AGENTS.md."
  (claude-repl--ws-hide-tombstoned-names))

;;;; Hide / restore ---------------------------------------------------------

(defun claude-repl--hide-project-dirs--hide ()
  "Kill every live workspace whose project-dir lives under a hide prefix.
Each match is stamped with the `:hidden-project-dir' plist marker (so
`claude-repl--hide-project-dirs--restore' can later distinguish it
from a workspace the user nuked by hand) and then torn down via
`claude-repl--nuke-one-workspace', which kills its Claude session,
buffers, and persp.  The teardown tombstones the hash entry; the
marker is not a runtime key so it survives the tombstone.

Returns the list of workspace names that were hidden."
  (let ((targets (claude-repl--hide-project-dirs--matching-live-workspaces)))
    (dolist (ws targets)
      (claude-repl--log ws "hide-project-dirs: hiding ws=%s" ws)
      (claude-repl--ws-put ws :hidden-project-dir t)
      (claude-repl--nuke-one-workspace ws))
    targets))

(defun claude-repl--hide-project-dirs--restore ()
  "Re-establish every workspace hidden by a prior hide toggle.
For each tombstone carrying `:hidden-project-dir', re-creates the
persp + Claude session via `claude-repl--establish-workspace', clears
the marker, then moves the workspace to the front of the tab-bar
list.  Restored workspaces land at the front in name order; focus
returns to the workspace that was active when the restore began.

Returns the list of restored workspace names."
  (let ((targets (claude-repl--hide-project-dirs--hidden-workspace-names))
        (origin (claude-repl--ws-current-name)))
    (dolist (ws targets)
      (let ((dir (claude-repl--ws-get ws :project-dir)))
        (claude-repl--log ws "hide-project-dirs: restoring ws=%s dir=%s" ws dir)
        (when dir
          (claude-repl--establish-workspace ws dir))
        (claude-repl--ws-put ws :hidden-project-dir nil)))
    ;; Move restored workspaces to the front of the tab-bar, in reverse
    ;; order so the first target ends up leftmost.
    (when (fboundp 'claude-repl--reorder-workspace-to-front)
      (dolist (ws (reverse targets))
        (claude-repl--reorder-workspace-to-front ws)))
    ;; Return focus to wherever the user was before the restore cascade
    ;; switched the frame through each re-established workspace.
    (when (and origin (fboundp '+workspace-switch))
      (ignore-errors (+workspace-switch origin)))
    targets))

;;;; Persistence ------------------------------------------------------------

(defun claude-repl--hide-project-dirs--persist ()
  "Persist the hide-project-dirs runtime to the workspace snapshot.
A snapshot save writes both `claude-repl-hide-project-dirs-enabled'
and the per-workspace `:hidden-project-dir' markers (carried on
tombstone entries by `claude-repl--collect-snapshot-entries'), so a
later session restore — on startup or from an archive — reconstructs
the hidden set."
  (when (fboundp 'claude-repl-save-workspace-snapshot)
    (claude-repl-save-workspace-snapshot)))

;;;; Toggle -----------------------------------------------------------------

(defun claude-repl-toggle-hide-project-dirs ()
  "Toggle `claude-repl-hide-project-dirs-enabled'.

When toggled ON, every workspace whose project-dir lives under a
prefix in `claude-repl-hide-project-dirs' (default
`~/workspace/ChessCom') is killed at the persp layer — Claude
session, buffers, and tab-bar entry all torn down — so the remaining
tab numbers stay contiguous and `SPC <n>' resolves to the workspace
the user sees.  Each killed workspace is tombstoned and marked with
`:hidden-project-dir'.  The current workspace is always retained.

When toggled OFF, every marked tombstone is re-established and moved
to the front of the tab-bar list.

The toggle state and the per-workspace markers are persisted to the
workspace snapshot so a session restore reconstructs the hidden set.

Refuses to run while a snapshot load is in progress — hiding mutates
the persp roster the loader is still rebuilding.

Forces a tab-bar repaint and a drawer refresh so the change is
visible immediately rather than waiting for the next 1Hz poll."
  (interactive)
  (when (bound-and-true-p claude-repl--snapshot-load-state)
    (user-error "claude-repl: a snapshot load is in progress — retry when it finishes"))
  (setq claude-repl-hide-project-dirs-enabled
        (not claude-repl-hide-project-dirs-enabled))
  (let ((affected (if claude-repl-hide-project-dirs-enabled
                      (claude-repl--hide-project-dirs--hide)
                    (claude-repl--hide-project-dirs--restore))))
    (claude-repl--hide-project-dirs--persist)
    (when (fboundp 'claude-repl--force-tab-bar-redraw)
      (claude-repl--force-tab-bar-redraw))
    (when (fboundp 'claude-repl-drawer--refresh-if-visible)
      (claude-repl-drawer--refresh-if-visible))
    (message "claude-repl hide-project-dirs %s (%s %d workspace(s))"
             (if claude-repl-hide-project-dirs-enabled "enabled" "disabled")
             (if claude-repl-hide-project-dirs-enabled "hid" "restored")
             (length affected))))

(provide 'claude-repl-hide-project-dirs)
;;; hide-project-dirs.el ends here
