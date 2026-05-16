;;; magit.el --- magit integration for claude-repl + user magit commands -*- lexical-binding: t; -*-

;;; Commentary:

;; All magit-related claude-repl wiring lives here.  Previously split across
;; the top-level doomdir config.el; moved here so it reloads with the module
;; (see AGENTS.md — no claude-repl code in the doomdir config.el).

;;; Code:

(defcustom claude-repl-magit-no-confirm-extras '(abort-revert abort-rebase abort-merge)
  "Extra actions to add to `magit-no-confirm'."
  :type '(repeat symbol)
  :group 'claude-repl)

(defcustom claude-repl-magit-github-ssh-prefix-regexp "^git@github.com:"
  "Regexp matching the SSH prefix in git remote URLs."
  :type 'string
  :group 'claude-repl)

(defcustom claude-repl-magit-github-base-url "https://github.com"
  "Base URL for GitHub, used when converting SSH remote URLs to HTTPS."
  :type 'string
  :group 'claude-repl)

(defcustom claude-repl-magit-github-org-regexp "github.com[:/]ChessCom/\\(.*\\)"
  "Regexp to extract the repo name from a GitHub remote URL.
Must contain one capture group for the repository name."
  :type 'string
  :group 'claude-repl)

(defcustom claude-repl-magit-github-commit-url-format "https://github.com/ChessCom/%s/commit/%s"
  "Format string for GitHub commit URLs.
First %s is the repo name, second %s is the commit SHA."
  :type 'string
  :group 'claude-repl)

(defcustom claude-repl-magit-show-tags-in-log nil
  "Whether magit commit listings include tag refs alongside branches.
When nil (the default), `tag: ...' entries are stripped from the refs
string fed to `magit-format-ref-labels' so the recent-commits, unpushed,
and other status log sections show only branches and HEAD pointers.
Toggle interactively with `+dwc/magit-toggle-tags-in-log' (bound to
`g T' in `magit-status-mode-map')."
  :type 'boolean
  :group 'claude-repl)

;;;; --- magit settings and keybindings ---------------------------------------

(defun claude-repl--magit-strip-tag-refs (args)
  "Return ARGS with `tag: NAME' entries stripped from the refs string.
ARGS is the argument list passed to `magit-format-ref-labels'; its
sole element is the comma-separated refs string git emitted via `%D'.
When `claude-repl-magit-show-tags-in-log' is non-nil, ARGS is returned
unchanged; otherwise both `tag: NAME, ' (leading) and `, tag: NAME'
\(trailing) patterns are removed so tag decorations disappear from
commit lists in magit-status and magit-log buffers while branches,
HEAD pointers, and remote refs remain intact.

Used as `:filter-args' advice on `magit-format-ref-labels' rather than
a `--decorate-refs-exclude' git arg so the filter applies uniformly to
every section that decorates commits (status, log, diff inline) without
having to thread args through each call site."
  (if (or claude-repl-magit-show-tags-in-log
          (null args)
          (not (stringp (car args))))
      args
    (let ((s (car args)))
      (setq s (replace-regexp-in-string
               "\\(?:tag: [^,]+\\(?:, \\)?\\|, tag: [^,]+\\)"
               "" s))
      (cons s (cdr args)))))

(defun +dwc/magit-toggle-tags-in-log ()
  "Toggle whether magit commit listings include tag refs.
Flips `claude-repl-magit-show-tags-in-log' and refreshes the current
magit buffer (status, log, etc.) when invoked from a `magit-mode'
buffer so the change becomes visible immediately."
  (interactive)
  (setq claude-repl-magit-show-tags-in-log
        (not claude-repl-magit-show-tags-in-log))
  (when (derived-mode-p 'magit-mode)
    (magit-refresh))
  (message "magit commit-list tags %s"
           (if claude-repl-magit-show-tags-in-log "shown" "hidden")))

(after! magit
  (setq magit-no-confirm (append magit-no-confirm claude-repl-magit-no-confirm-extras)
        magit-diff-visit-previous-blob nil)

  ;; Unfold these sections by default in magit-status
  (setq magit-section-initial-visibility-alist
        '((unpushed . show)
          (stashes  . show)
          (untracked . show)))

  ;; Strip tag refs from commit-list decorations by default (toggle via `g T').
  (advice-add 'magit-format-ref-labels :filter-args
              #'claude-repl--magit-strip-tag-refs)

  (map! :map (magit-unstaged-section-map magit-staged-section-map magit-untracked-section-map magit-mode-map)
        :desc "Jump to recent commits"
        "g r"
        #'magit-jump-to-unpushed-to-upstream))

;; Section map bindings must be done after magit-diff loads.
(after! magit-diff
  (define-key magit-file-section-map [return] #'magit-diff-visit-worktree-file)
  (define-key magit-file-section-map [C-return] #'magit-diff-visit-file)
  (define-key magit-hunk-section-map [return] #'magit-diff-visit-worktree-file)
  (define-key magit-hunk-section-map [C-return] #'magit-diff-visit-file))

;;;; --- GitHub URL helpers for magit commits -------------------------------

(defun +dwc/magit-open-commit-in-github ()
  "Open the current commit in GitHub browser."
  (interactive)
  (let* ((default-directory (claude-repl--ws-dir (+workspace-current-name)))
         (commit-sha (string-trim (shell-command-to-string "git rev-parse HEAD")))
         (remote-url (string-trim (shell-command-to-string "git config --get remote.origin.url")))
         (cleaned-url (replace-regexp-in-string claude-repl-magit-github-ssh-prefix-regexp claude-repl-magit-github-base-url
                                                (replace-regexp-in-string "\\.git$" "" remote-url)))
         (repo-name (progn
                      (if (string-match claude-repl-magit-github-org-regexp cleaned-url)
                          (match-string 1 cleaned-url)
                        (error (format "Remote URL '%s' does not match expected pattern" cleaned-url)))))
         (github-url (format claude-repl-magit-github-commit-url-format repo-name commit-sha)))
    (browse-url github-url)))

(defun +dwc/magit-copy-commit-link ()
  "Copy GitHub link for commit at point in magit buffer."
  (interactive)
  (let* ((commit-sha (magit-commit-at-point))
         (default-directory (claude-repl--ws-dir (+workspace-current-name)))
         (remote-url (string-trim (shell-command-to-string "git config --get remote.origin.url")))
         (cleaned-url (replace-regexp-in-string claude-repl-magit-github-ssh-prefix-regexp claude-repl-magit-github-base-url
                                                (replace-regexp-in-string "\\.git$" "" remote-url)))
         (repo-name (progn
                      (if (string-match claude-repl-magit-github-org-regexp cleaned-url)
                          (match-string 1 cleaned-url)
                        (error (format "Remote URL '%s' does not match expected pattern" cleaned-url)))))
         (github-url (format claude-repl-magit-github-commit-url-format repo-name commit-sha)))
    (kill-new github-url)
    (message "GitHub commit link copied to clipboard: %s" github-url)))

;;;; --- GitHub PR resolution via gh CLI ------------------------------------

(defun claude-repl--gh-pr-url-for-branch (project-dir branch)
  "Return the GitHub PR URL for BRANCH in PROJECT-DIR, or nil if none.
Resolves via `gh pr view BRANCH --json url --jq .url' run from
PROJECT-DIR.  Returns nil when no PR is associated with BRANCH or when
`gh' fails for any reason (stderr is suppressed).  Branch names are
shell-quoted, so callers can pass any branch string safely."
  (let* ((default-directory (file-name-as-directory project-dir))
         (cmd (concat (mapconcat #'shell-quote-argument
                                 (list "gh" "pr" "view" branch
                                       "--json" "url" "--jq" ".url")
                                 " ")
                      " 2>/dev/null"))
         (output (string-trim (shell-command-to-string cmd))))
    (and (string-prefix-p "http" output) output)))

(defun +dwc/open-workspace-pr-in-browser ()
  "Open the GitHub PR for the current workspace's branch in the browser.
Resolves the PR URL via `gh' against the workspace's project directory
\(see `claude-repl--ws-dir'), not the buffer's `magit-toplevel' — so this
works correctly from any buffer in the workspace.  Errors when no PR is
associated with the current branch."
  (interactive)
  (let* ((ws (+workspace-current-name))
         (project-dir (claude-repl--ws-dir ws))
         (default-directory (file-name-as-directory project-dir))
         (branch (string-trim
                  (shell-command-to-string "git rev-parse --abbrev-ref HEAD")))
         (pr-url (claude-repl--gh-pr-url-for-branch project-dir branch)))
    (unless pr-url
      (user-error "No PR found for branch '%s' in workspace '%s'" branch ws))
    (browse-url pr-url)
    (message "Opened PR: %s" pr-url)))

;;;; --- magit-status-workspace ---------------------------------------------

(defun claude-repl--magit-display-buffer-same-window (buffer)
  "Display BUFFER in the selected window.
Used as a let-bound override for `magit-display-buffer-function' so
the top-level `magit-status' call from `+dwc/magit-status-workspace'
replaces the current buffer rather than splitting or popping up a
new window.  Returns the window magit should select."
  (display-buffer buffer '(display-buffer-same-window)))

(defun +dwc/magit-status-workspace ()
  "Open magit-status for the workspace in the SELECTED window.
Always replaces the current buffer with the workspace's magit-status
— no splits, no window reuse — regardless of the prior layout or
claude-repl panel state.  Forces same-window display by let-binding
`magit-display-buffer-function' for this call only, so other magit
buffers (diffs, logs, etc.) keep their normal display behavior.

If the selected window is a side window (e.g., the workspace
drawer), first pops to the frame's main window so magit replaces the
main buffer rather than failing on the dedicated side window.

Clears the saved `:fullscreen-config' since claude is no longer
fullscreen once magit replaces the current window."
  (interactive)
  (when (window-parameter (selected-window) 'window-side)
    (select-window (window-main-window)))
  (let* ((ws (+workspace-current-name))
         (dir (claude-repl--ws-dir ws))
         (magit-display-buffer-function
          #'claude-repl--magit-display-buffer-same-window))
    (when (fboundp 'claude-repl--log)
      (claude-repl--log ws "magit-status-workspace: same-window dir=%s" dir))
    (claude-repl--ws-put ws :fullscreen-config nil)
    (magit-status dir)))

;;;; --- Hide Claude panels before magit-status RET actions -----------------

(defcustom claude-repl-magit-hide-panels-advised-fns
  '(magit-visit-thing
    magit-diff-visit-file
    magit-diff-visit-worktree-file
    magit-show-commit
    magit-show-refs
    magit-show-refs-current
    magit-show-refs-head
    magit-show-refs-other
    magit-stash-show
    magit-visit-work)
  "Magit commands that should hide Claude panels before running.
The advice only fires when the command is invoked from a buffer whose
`major-mode' is `magit-status-mode', so triggering the same commands
from a magit-diff/log buffer does not disturb panels."
  :type '(repeat symbol)
  :group 'claude-repl)

(defun claude-repl--magit-hide-panels-before-action (&rest _)
  "Hide Claude REPL panels before a magit-status RET action opens a new buffer.
No-op unless the caller's buffer is in `magit-status-mode' and both
panels are currently visible.  Routes through `claude-repl--hide-panels'
so `:repl-state'/`:claude-state' are left untouched — the panels simply
become hidden, matching the behavior of other non-user-initiated close
paths (see `claude-repl--on-close' for the user-initiated path that
transitions `:repl-state' to :inactive)."
  (when (and (eq major-mode 'magit-status-mode)
             (claude-repl--panels-visible-p))
    (claude-repl--hide-panels)))

(dolist (fn claude-repl-magit-hide-panels-advised-fns)
  (advice-add fn :before #'claude-repl--magit-hide-panels-before-action))

;;;; --- Keybindings --------------------------------------------------------

(map! :leader
      :desc "Magit status for workspace" "g g" #'+dwc/magit-status-workspace
      :desc "Magit status"               "g G" #'magit-status
      :desc "Open commit in GitHub"      "g O" #'+dwc/magit-open-commit-in-github)

(map! :leader
      (:prefix "j"
       :desc "Open workspace PR in browser" "P" #'+dwc/open-workspace-pr-in-browser))

(map! :map magit-status-mode-map
      "g c" #'+dwc/magit-copy-commit-link
      "g C" #'+dwc/magit-open-commit-in-github
      "g T" #'+dwc/magit-toggle-tags-in-log)

(map! :map (magit-status-mode-map magit-diff-section-base-map magit-diff-section-map)
      "C-<return>" #'magit-diff-visit-file-other-window)

;;; magit.el ends here
