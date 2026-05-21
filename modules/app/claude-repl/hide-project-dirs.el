;;; hide-project-dirs.el --- Hide workspaces whose project-dir lives under configured prefixes -*- lexical-binding: t; -*-

;;; Commentary:

;; Provides a toggleable mode that hides any workspace whose `:project-dir'
;; lives under one of the prefix directories listed in
;; `claude-repl-hide-project-dirs'.  Hidden workspaces drop out of BOTH
;; the tab-bar render and the drawer's visible-workspace list.
;;
;; The current workspace is always retained so the user can never accidentally
;; render themselves invisible to themselves.
;;
;; Default prefix is `~/workspace/ChessCom' — chosen so day-job repos can
;; be hidden in one keystroke when paged off them.
;;
;; Toggle via `claude-repl-toggle-hide-project-dirs' (bound under `SPC o H'
;; in `keybindings.el').

;;; Code:

(require 'subr-x)

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
  "Non-nil means hide workspaces whose `:project-dir' matches a prefix.
Prefixes are configured via `claude-repl-hide-project-dirs'.  When
non-nil, both the tab-bar and the drawer drop matching workspaces
from their rendered lists (the current workspace is always retained).

Toggle via `claude-repl-toggle-hide-project-dirs'.")

;;;; Predicate + filter -----------------------------------------------------

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

(defun claude-repl--filter-hide-project-dir-names (names current-name)
  "Drop NAMES whose project-dir lives under a hide prefix.
CURRENT-NAME is always retained — never filter the active workspace
out from under the user.  Returns NAMES unchanged when
`claude-repl-hide-project-dirs-enabled' is nil."
  (if claude-repl-hide-project-dirs-enabled
      (cl-remove-if
       (lambda (n)
         (and (not (equal n current-name))
              (claude-repl--hide-project-dirs--ws-matches-p n)))
       names)
    names))

;;;; Toggle -----------------------------------------------------------------

(defun claude-repl-toggle-hide-project-dirs ()
  "Toggle `claude-repl-hide-project-dirs-enabled'.
When toggled ON, workspaces whose project-dir lives under any prefix
listed in `claude-repl-hide-project-dirs' (default
`~/workspace/ChessCom') drop out of both the tab-bar and the drawer.

Forces a tab-bar repaint and a drawer refresh so the change is
visible immediately rather than waiting for the next 1Hz poll."
  (interactive)
  (setq claude-repl-hide-project-dirs-enabled
        (not claude-repl-hide-project-dirs-enabled))
  (when (fboundp 'claude-repl--force-tab-bar-redraw)
    (claude-repl--force-tab-bar-redraw))
  (when (fboundp 'claude-repl-drawer--refresh-if-visible)
    (claude-repl-drawer--refresh-if-visible))
  (message "claude-repl hide-project-dirs %s"
           (if claude-repl-hide-project-dirs-enabled "enabled" "disabled")))

(provide 'claude-repl-hide-project-dirs)
;;; hide-project-dirs.el ends here
