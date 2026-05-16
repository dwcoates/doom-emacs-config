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

(defcustom claude-repl-magit-show-tags-header nil
  "Whether `magit-status' displays the tags header line.
When nil (the default), `magit-insert-tags-header' is removed from
`magit-status-headers-hook' so the \"Tags:\" header is omitted from
status buffers.  Toggle interactively inside any magit-status
buffer with `+dwc/magit-toggle-tags-header'."
  :type 'boolean
  :group 'claude-repl)

;;;; --- magit settings and keybindings ---------------------------------------

(defun claude-repl--magit-apply-tags-header-visibility ()
  "Sync `magit-status-headers-hook' to `claude-repl-magit-show-tags-header'.
Adds `magit-insert-tags-header' to the hook when the option is non-nil
and removes it otherwise — the hook is the canonical place magit reads
to assemble status buffer headers."
  (if claude-repl-magit-show-tags-header
      (add-hook 'magit-status-headers-hook #'magit-insert-tags-header t)
    (remove-hook 'magit-status-headers-hook #'magit-insert-tags-header)))

(defun +dwc/magit-toggle-tags-header ()
  "Toggle whether `magit-status' displays the tags header line.
Flips `claude-repl-magit-show-tags-header', syncs the hook via
`claude-repl--magit-apply-tags-header-visibility', and refreshes the
current buffer when invoked from a `magit-status-mode' buffer so the
change becomes visible immediately."
  (interactive)
  (setq claude-repl-magit-show-tags-header
        (not claude-repl-magit-show-tags-header))
  (claude-repl--magit-apply-tags-header-visibility)
  (when (derived-mode-p 'magit-status-mode)
    (magit-refresh))
  (message "magit-status tags header %s"
           (if claude-repl-magit-show-tags-header "shown" "hidden")))

(after! magit
  (setq magit-no-confirm (append magit-no-confirm claude-repl-magit-no-confirm-extras)
        magit-diff-visit-previous-blob nil)

  ;; Unfold these sections by default in magit-status
  (setq magit-section-initial-visibility-alist
        '((unpushed . show)
          (stashes  . show)
          (untracked . show)))

  ;; Apply the tags-header visibility preference (default: hidden).
  (claude-repl--magit-apply-tags-header-visibility)

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
      "g T" #'+dwc/magit-toggle-tags-header)

(map! :map (magit-status-mode-map magit-diff-section-base-map magit-diff-section-map)
      "C-<return>" #'magit-diff-visit-file-other-window)

;;; magit.el ends here
