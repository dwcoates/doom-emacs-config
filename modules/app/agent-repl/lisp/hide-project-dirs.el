;;; hide-project-dirs.el --- Hide workspaces whose project-dir lives under configured prefixes -*- lexical-binding: t; -*-

;;; Commentary:

;; Provides a toggleable mode that hides any workspace whose `:project-dir'
;; lives under one of the prefix directories listed in
;; `agent-repl-hide-project-dirs'.
;;
;; Hiding happens at the PERSP layer, not at render time: when the mode is
;; toggled ON every matching workspace is legitimately killed via
;; `agent-repl--nuke-one-workspace' (Claude session, buffers, and persp
;; all torn down) and tombstoned with a `:hidden-project-dir' marker.
;; Because the matching workspaces leave `persp-names-cache' entirely, the
;; surviving tab numbers stay contiguous and `SPC <n>' resolves to the
;; workspace the user actually sees — the bug a render-only filter left
;; behind (a tab nominally at position 3 still switched to a hidden
;; workspace).
;;
;; Toggling the mode OFF re-establishes every marked tombstone via
;; `agent-repl--establish-workspace' and moves the restored workspaces to
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
;; Toggle via `agent-repl-toggle-hide-project-dirs' (bound under `SPC o H'
;; in `keybindings.el').

;;; Code:

(require 'subr-x)
(require 'cl-lib)

;; Forward declarations: defined in earlier-loaded modules (`core.el',
;; `commands.el').  Named here so the references below read cleanly at
;; compile/load time.
(defvar agent-repl--workspaces)
(defvar agent-repl--snapshot-load-state)

;;;; Customization ----------------------------------------------------------

(defcustom agent-repl-hide-project-dirs
  (list (expand-file-name "~/workspace/ChessCom"))
  "List of directory prefixes whose workspaces are hidden when the mode is on.
A workspace is hidden when its `:project-dir' (after canonicalization
via `agent-repl--path-canonical') lives under any entry on this
list.  Path comparison is prefix-based against the canonical
directory string so both the prefix itself and any nested worktree
beneath it are matched.

Default is `~/workspace/ChessCom' — toggle the mode via
`agent-repl-toggle-hide-project-dirs' (bound to `SPC o H')."
  :type '(repeat directory)
  :group 'agent-repl)

(defvar agent-repl-hide-project-dirs-enabled nil
  "Non-nil means workspaces matching a hide prefix are killed, not shown.
Prefixes are configured via `agent-repl-hide-project-dirs'.  When the
mode is toggled ON every matching workspace is killed at the persp
layer and tombstoned with `:hidden-project-dir'; toggling OFF
re-establishes those tombstones.  This flag records which side of the
toggle the runtime is currently on and is persisted to the workspace
snapshot so a session restore reconstructs it.

Toggle via `agent-repl-toggle-hide-project-dirs'.")

;;;; Predicate --------------------------------------------------------------

(defun agent-repl--hide-project-dirs--canonical-directory (ws path kind)
  "Canonicalize PATH for KIND, logging either its result or failure.
WS supplies workspace metadata when this canonicalization belongs to a
workspace scan.  KIND distinguishes a configured prefix from a workspace
project directory in the trace."
  (condition-case err
      (let ((canonical (agent-repl--path-canonical path)))
        (agent-repl--log-verbose
         ws "hide-project-dirs: canonicalized kind=%s path=%S canonical=%S"
         kind path canonical)
        canonical)
    (error
     (agent-repl--warn
      ws "hide-project-dirs: canonicalization failed kind=%s path=%S error=%S"
      kind path err)
     nil)))

(defun agent-repl--hide-project-dirs--canonical-prefixes (&optional ws)
  "Return `agent-repl-hide-project-dirs' canonicalized for prefix comparison.
Each entry is expanded, run through `agent-repl--path-canonical' (so
tildes / symlinks resolve consistently with `:project-dir' values),
then has a trailing slash appended so prefix matching can't match a
sibling directory whose name happens to share the prefix's leading
characters (e.g. `~/workspace/ChessCom-archive' must not match the
`~/workspace/ChessCom' prefix).  WS is used only for scan diagnostics."
  (let ((prefixes
         (cl-loop for raw in agent-repl-hide-project-dirs
                  for expanded = (expand-file-name raw)
                  for canonical = (agent-repl--hide-project-dirs--canonical-directory
                                   ws expanded "prefix")
                  when (and canonical (not (string-empty-p canonical)))
                  collect (file-name-as-directory canonical))))
    (agent-repl--log-verbose
     ws "hide-project-dirs: canonical-prefix scan configured=%S usable=%S"
     agent-repl-hide-project-dirs prefixes)
    prefixes))

(defun agent-repl--hide-project-dirs--ws-matches-p (ws)
  "Return non-nil when workspace WS's project-dir lives under a hide prefix.
Always returns nil when WS has no registered `:project-dir' — entries
without a project-dir can't be classified, so they pass through the
filter and remain visible."
  (let ((dir (agent-repl--ws-get ws :project-dir)))
    (if (not dir)
        (progn
          (agent-repl--log-verbose
           ws "hide-project-dirs: match scan skipped reason=missing-project-dir")
          nil)
      (let ((canonical
             (agent-repl--hide-project-dirs--canonical-directory
              ws dir "project-dir")))
        (if (not canonical)
            (progn
              (agent-repl--log-verbose
               ws "hide-project-dirs: match scan skipped dir=%S reason=uncanonicalizable"
               dir)
              nil)
          (let* ((with-slash (file-name-as-directory canonical))
                 (prefixes (agent-repl--hide-project-dirs--canonical-prefixes ws))
                 (matched (cl-find-if (lambda (prefix)
                                        (string-prefix-p prefix with-slash))
                                      prefixes)))
            (agent-repl--log-verbose
             ws "hide-project-dirs: match scan dir=%S canonical=%S prefixes=%S matched=%S"
             dir with-slash prefixes matched)
            matched))))))

;;;; Workspace selection ----------------------------------------------------

(defun agent-repl--hide-project-dirs--matching-live-workspaces ()
  "Return live workspace names whose project-dir lives under a hide prefix.
The current workspace is excluded — hiding never kills the workspace
the user is sitting in.  Only live (non-tombstoned) workspaces are
returned, since tombstones have no persp left to kill."
  (let* ((current (agent-repl--ws-current-name))
         (live (agent-repl--live-ws-names))
         (targets
          (cl-remove-if-not
           (lambda (ws)
             (cond
              ((equal ws current)
               (agent-repl--log-verbose
                ws "hide-project-dirs: match scan excluded reason=current-workspace")
               nil)
              ((agent-repl--hide-project-dirs--ws-matches-p ws)
               (agent-repl--log-verbose
                ws "hide-project-dirs: match scan selected")
               t)
              (t
               (agent-repl--log-verbose
                ws "hide-project-dirs: match scan excluded reason=no-prefix-match")
               nil)))
           live)))
    (agent-repl--log-verbose
     current "hide-project-dirs: live-workspace scan current=%S live=%S targets=%S"
     current live targets)
    targets))

(defun agent-repl--hide-project-dirs--hidden-workspace-names (&optional ws)
  "Return names of workspaces carrying the `:hidden-project-dir' marker.
These are the tombstones killed by a prior hide toggle — the set that
`agent-repl--hide-project-dirs--restore' brings back.  Returned in
name order so restore is deterministic.

Thin wrapper over `agent-repl--ws-hide-tombstoned-names' (the
workspace.el integration boundary for hide-reason tombstones); this
file no longer pokes `agent-repl--workspaces' directly per the
\"Workspace state encapsulation\" rule in AGENTS.md."
  (let ((names (agent-repl--ws-hide-tombstoned-names)))
    (agent-repl--log-verbose
     ws "hide-project-dirs: hidden-tombstone scan targets=%S" names)
    names))

;;;; Hide / restore ---------------------------------------------------------

(defun agent-repl--hide-project-dirs--hide ()
  "Kill every live workspace whose project-dir lives under a hide prefix.
Each match is stamped with the `:hidden-project-dir' plist marker (so
`agent-repl--hide-project-dirs--restore' can later distinguish it
from a workspace the user nuked by hand) and then torn down via
`agent-repl--nuke-one-workspace', which kills its Claude session,
buffers, and persp.  The teardown tombstones the hash entry; the
marker is not a runtime key so it survives the tombstone.

Returns the list of workspace names that were hidden."
  (let ((targets (agent-repl--hide-project-dirs--matching-live-workspaces))
        (origin (agent-repl--ws-current-name))
        (agent-repl--kill-cause "hide-project-dirs sweep (hide toggle)"))
    (agent-repl--log
     origin "hide-project-dirs: hide begin origin=%S targets=%S target-count=%d"
     origin targets (length targets))
    (dolist (ws targets)
      (let ((dir (agent-repl--ws-get ws :project-dir)))
        (agent-repl--log ws "hide-project-dirs: hiding ws=%s dir=%S" ws dir)
        (agent-repl--ws-put ws :hidden-project-dir t)
        (condition-case err
            (progn
              (agent-repl--nuke-one-workspace ws)
              (agent-repl--log
               ws "hide-project-dirs: hide complete ws=%s marker-set=t" ws))
          (error
           (agent-repl--log
            ws "hide-project-dirs: hide failed ws=%s marker-set=t error=%S" ws err)
           (signal (car err) (cdr err))))))
    (agent-repl--log
     origin "hide-project-dirs: hide complete origin=%S hidden=%S hidden-count=%d"
     origin targets (length targets))
    targets))

(defun agent-repl--hide-project-dirs--restore ()
  "Re-establish every workspace hidden by a prior hide toggle.
For each tombstone carrying `:hidden-project-dir', re-creates the
persp + Claude session via `agent-repl--establish-workspace', clears
the marker, then moves the workspace to the front of the tab-bar
list.  Restored workspaces land at the front in name order; focus
returns to the workspace that was active when the restore began.

Returns the list of restored workspace names."
  (let* ((origin (agent-repl--ws-current-name))
         (targets (agent-repl--hide-project-dirs--hidden-workspace-names origin)))
    (agent-repl--log
     origin "hide-project-dirs: restore begin origin=%S targets=%S target-count=%d"
     origin targets (length targets))
    (dolist (ws targets)
      (let ((dir (agent-repl--ws-get ws :project-dir)))
        (agent-repl--log ws "hide-project-dirs: restoring ws=%s dir=%s" ws dir)
        (if dir
            (condition-case err
                (progn
                  (agent-repl--establish-workspace ws dir)
                  (agent-repl--log
                   ws "hide-project-dirs: restore established ws=%s dir=%S" ws dir))
              (error
               (agent-repl--log
                ws "hide-project-dirs: restore failed ws=%s dir=%S error=%S" ws dir err)
               (signal (car err) (cdr err))))
          (agent-repl--log
           ws "hide-project-dirs: restore skipped establishment ws=%s reason=missing-project-dir" ws))
        (agent-repl--ws-put ws :hidden-project-dir nil)
        (agent-repl--log
         ws "hide-project-dirs: restore marker-cleared ws=%s dir-present=%s"
         ws (not (null dir)))))
    ;; Move restored workspaces to the front of the tab-bar, in reverse
    ;; order so the first target ends up leftmost.
    (if (fboundp 'agent-repl--reorder-workspace-to-front)
        (progn
          (agent-repl--log
           origin "hide-project-dirs: restore reorder begin targets=%S" targets)
          (dolist (ws (reverse targets))
            (condition-case err
                (progn
                  (agent-repl--reorder-workspace-to-front ws)
                  (agent-repl--log
                   ws "hide-project-dirs: restore reordered-to-front ws=%s" ws))
              (error
               (agent-repl--log
                ws "hide-project-dirs: restore reorder failed ws=%s error=%S" ws err)
               (signal (car err) (cdr err))))))
      (agent-repl--warn
       origin "hide-project-dirs: restore reorder skipped reason=reorder-helper-unavailable"))
    ;; Return focus to wherever the user was before the restore cascade
    ;; switched the frame through each re-established workspace.
    (if origin
        (condition-case err
            (progn
              (agent-repl--ws-switch origin)
              (agent-repl--log
               origin "hide-project-dirs: restore focus-returned origin=%S" origin))
          (error
           (agent-repl--warn
            origin "hide-project-dirs: restore focus-return-failed origin=%S error=%S"
            origin err)))
      (agent-repl--log
       nil "hide-project-dirs: restore focus-return skipped reason=no-origin"))
    (agent-repl--log
     origin "hide-project-dirs: restore complete origin=%S restored=%S restored-count=%d"
     origin targets (length targets))
    targets))

;;;; Persistence ------------------------------------------------------------

(defun agent-repl--hide-project-dirs--persist (&optional ws)
  "Persist the hide-project-dirs runtime to the workspace snapshot.
A snapshot save writes both `agent-repl-hide-project-dirs-enabled'
and the per-workspace `:hidden-project-dir' markers (carried on
tombstone entries by `agent-repl--collect-snapshot-entries'), so a
later session restore — on startup or from an archive — reconstructs
the hidden set."
  (if (fboundp 'agent-repl-save-workspace-snapshot)
      (condition-case err
          (progn
            (agent-repl--log
             ws "hide-project-dirs: persist begin enabled=%s" agent-repl-hide-project-dirs-enabled)
            (agent-repl-save-workspace-snapshot)
            (agent-repl--log
             ws "hide-project-dirs: persist complete enabled=%s" agent-repl-hide-project-dirs-enabled))
        (error
         (agent-repl--log
          ws "hide-project-dirs: persist failed enabled=%s error=%S"
          agent-repl-hide-project-dirs-enabled err)
         (signal (car err) (cdr err))))
    (agent-repl--warn
     ws "hide-project-dirs: persist skipped reason=snapshot-saver-unavailable enabled=%s"
     agent-repl-hide-project-dirs-enabled)))

;;;; Toggle -----------------------------------------------------------------

(defun agent-repl-toggle-hide-project-dirs ()
  "Toggle `agent-repl-hide-project-dirs-enabled'.

When toggled ON, every workspace whose project-dir lives under a
prefix in `agent-repl-hide-project-dirs' (default
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

Forces a tab-bar repaint so the change is visible immediately rather
than waiting for the next 1Hz poll."
  (interactive)
  (let ((origin (agent-repl--ws-current-name)))
    (agent-repl--log
     origin "hide-project-dirs: toggle requested origin=%S enabled-before=%s snapshot-load-state=%S"
     origin agent-repl-hide-project-dirs-enabled agent-repl--snapshot-load-state)
    (when (bound-and-true-p agent-repl--snapshot-load-state)
      (agent-repl--log
       origin "hide-project-dirs: toggle rejected origin=%S reason=snapshot-load-in-progress state=%S"
       origin agent-repl--snapshot-load-state)
      (user-error "agent-repl: a snapshot load is in progress — retry when it finishes"))
    (setq agent-repl-hide-project-dirs-enabled
          (not agent-repl-hide-project-dirs-enabled))
    (let ((operation (if agent-repl-hide-project-dirs-enabled "hide" "restore"))
          (affected nil))
      (agent-repl--log
       origin "hide-project-dirs: toggle accepted origin=%S enabled-after=%s operation=%s"
       origin agent-repl-hide-project-dirs-enabled operation)
      (condition-case err
          (setq affected (if agent-repl-hide-project-dirs-enabled
                             (agent-repl--hide-project-dirs--hide)
                           (agent-repl--hide-project-dirs--restore)))
        (error
         (agent-repl--log
          origin "hide-project-dirs: toggle operation-failed origin=%S operation=%s enabled=%s error=%S"
          origin operation agent-repl-hide-project-dirs-enabled err)
         (signal (car err) (cdr err))))
      (agent-repl--log
       origin "hide-project-dirs: toggle operation-complete origin=%S operation=%s affected=%S affected-count=%d"
       origin operation affected (length affected))
      (agent-repl--hide-project-dirs--persist origin)
      (if (fboundp 'agent-repl--force-tab-bar-redraw)
          (progn
            (agent-repl--force-tab-bar-redraw)
            (agent-repl--log
             origin "hide-project-dirs: toggle redraw-complete origin=%S" origin))
        (agent-repl--warn
         origin "hide-project-dirs: toggle redraw-skipped reason=redraw-helper-unavailable"))
      (message "agent-repl hide-project-dirs %s (%s %d workspace(s))"
               (if agent-repl-hide-project-dirs-enabled "enabled" "disabled")
               (if agent-repl-hide-project-dirs-enabled "hid" "restored")
               (length affected)))))

(provide 'agent-repl-hide-project-dirs)
;;; hide-project-dirs.el ends here
