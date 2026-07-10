;;; magit.el --- magit integration for agent-repl + user magit commands -*- lexical-binding: t; -*-

;;; Commentary:

;; All magit-related agent-repl wiring lives here.  Previously split across
;; the top-level doomdir config.el; moved here so it reloads with the module
;; (see AGENTS.md — no agent-repl code in the doomdir config.el).

;;; Code:

(defcustom agent-repl-magit-no-confirm-extras '(abort-revert abort-rebase abort-merge)
  "Extra actions to add to `magit-no-confirm'."
  :type '(repeat symbol)
  :group 'agent-repl)

(defcustom agent-repl-magit-github-ssh-prefix-regexp "^git@github.com:"
  "Regexp matching the SSH prefix in git remote URLs."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-magit-github-base-url "https://github.com"
  "Base URL for GitHub, used when converting SSH remote URLs to HTTPS."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-magit-github-org-regexp "github.com[:/]ChessCom/\\(.*\\)"
  "Regexp to extract the repo name from a GitHub remote URL.
Must contain one capture group for the repository name."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-magit-github-commit-url-format "https://github.com/ChessCom/%s/commit/%s"
  "Format string for GitHub commit URLs.
First %s is the repo name, second %s is the commit SHA."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-magit-show-tags-in-log nil
  "Whether magit commit listings include tag refs alongside branches.
When nil (the default), `tag: ...' entries are stripped from the refs
string fed to `magit-format-ref-labels' so the recent-commits, unpushed,
and other status log sections show only branches and HEAD pointers.
Toggle interactively with `+dwc/magit-toggle-tags-in-log' (bound to
`g T' in `magit-status-mode-map')."
  :type 'boolean
  :group 'agent-repl)

;;;; --- magit settings and keybindings ---------------------------------------

(defun agent-repl--magit-strip-tag-refs (args)
  "Return ARGS with `tag: NAME' entries stripped from the refs string.
ARGS is the argument list passed to `magit-format-ref-labels'; its
sole element is the comma-separated refs string git emitted via `%D'.
When `agent-repl-magit-show-tags-in-log' is non-nil, ARGS is returned
unchanged; otherwise both `tag: NAME, ' (leading) and `, tag: NAME'
\(trailing) patterns are removed so tag decorations disappear from
commit lists in magit-status and magit-log buffers while branches,
HEAD pointers, and remote refs remain intact.

Used as `:filter-args' advice on `magit-format-ref-labels' rather than
a `--decorate-refs-exclude' git arg so the filter applies uniformly to
every section that decorates commits (status, log, diff inline) without
having to thread args through each call site."
  (if (or agent-repl-magit-show-tags-in-log
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
Flips `agent-repl-magit-show-tags-in-log' and refreshes the current
magit buffer (status, log, etc.) when invoked from a `magit-mode'
buffer so the change becomes visible immediately."
  (interactive)
  (setq agent-repl-magit-show-tags-in-log
        (not agent-repl-magit-show-tags-in-log))
  (when (derived-mode-p 'magit-mode)
    (magit-refresh))
  (message "magit commit-list tags %s"
           (if agent-repl-magit-show-tags-in-log "shown" "hidden")))

(after! magit
  (setq magit-no-confirm (append magit-no-confirm agent-repl-magit-no-confirm-extras)
        magit-diff-visit-previous-blob nil)

  ;; Unfold these sections by default in magit-status
  (setq magit-section-initial-visibility-alist
        '((unpushed . show)
          (stashes  . show)
          (untracked . show)))

  ;; Strip tag refs from commit-list decorations by default (toggle via `g T').
  (advice-add 'magit-format-ref-labels :filter-args
              #'agent-repl--magit-strip-tag-refs)

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
  "Open the current commit in GitHub browser.
Routes git reads through `agent-repl--git-string' so the function
is mockable per AGENTS.md."
  (interactive)
  (let* ((default-directory (agent-repl--ws-dir (agent-repl--ws-current-name)))
         (commit-sha (agent-repl--git-string "rev-parse" "HEAD"))
         (remote-url (agent-repl--git-string "config" "--get" "remote.origin.url"))
         (cleaned-url (replace-regexp-in-string agent-repl-magit-github-ssh-prefix-regexp agent-repl-magit-github-base-url
                                                (replace-regexp-in-string "\\.git$" "" remote-url)))
         (repo-name (progn
                      (if (string-match agent-repl-magit-github-org-regexp cleaned-url)
                          (match-string 1 cleaned-url)
                        (error (format "Remote URL '%s' does not match expected pattern" cleaned-url)))))
         (github-url (format agent-repl-magit-github-commit-url-format repo-name commit-sha)))
    (browse-url github-url)))

(defun +dwc/magit-copy-commit-link ()
  "Copy GitHub link for commit at point in magit buffer.
Routes git reads through `agent-repl--git-string' so the function
is mockable per AGENTS.md."
  (interactive)
  (let* ((commit-sha (magit-commit-at-point))
         (default-directory (agent-repl--ws-dir (agent-repl--ws-current-name)))
         (remote-url (agent-repl--git-string "config" "--get" "remote.origin.url"))
         (cleaned-url (replace-regexp-in-string agent-repl-magit-github-ssh-prefix-regexp agent-repl-magit-github-base-url
                                                (replace-regexp-in-string "\\.git$" "" remote-url)))
         (repo-name (progn
                      (if (string-match agent-repl-magit-github-org-regexp cleaned-url)
                          (match-string 1 cleaned-url)
                        (error (format "Remote URL '%s' does not match expected pattern" cleaned-url)))))
         (github-url (format agent-repl-magit-github-commit-url-format repo-name commit-sha)))
    (kill-new github-url)
    (message "GitHub commit link copied to clipboard: %s" github-url)))

;;;; --- GitHub PR resolution via gh CLI ------------------------------------

(defun agent-repl--gh-pr-url-for-branch (project-dir branch)
  "Return the GitHub PR URL for BRANCH in PROJECT-DIR, or nil if none.
Resolves via `gh pr view BRANCH --json url --jq .url' run from
PROJECT-DIR.  Routes through `agent-repl--gh-string-quiet' (the
external boundary for the GitHub CLI) so tests mock that wrapper
rather than invoking real `gh' (see AGENTS.md \"No External
Processes or External State in Tests\")."
  (let* ((default-directory (file-name-as-directory project-dir))
         (output (agent-repl--gh-string-quiet
                  "pr" "view" branch "--json" "url" "--jq" ".url")))
    (and (string-prefix-p "http" output) output)))

(defun +dwc/open-workspace-pr-in-browser ()
  "Open the GitHub PR for the current workspace's branch in the browser.
Resolves the PR URL via `gh' against the workspace's project directory
\(see `agent-repl--ws-dir'), not the buffer's `magit-toplevel' — so this
works correctly from any buffer in the workspace.  Errors when no PR is
associated with the current branch.  Routes git reads through
`agent-repl--git-string' so the function is mockable per AGENTS.md."
  (interactive)
  (let* ((ws (agent-repl--ws-current-name))
         (project-dir (agent-repl--ws-dir ws))
         (default-directory (file-name-as-directory project-dir))
         (branch (agent-repl--git-string "rev-parse" "--abbrev-ref" "HEAD"))
         (pr-url (agent-repl--gh-pr-url-for-branch project-dir branch)))
    (unless pr-url
      (user-error "No PR found for branch '%s' in workspace '%s'" branch ws))
    (browse-url pr-url)
    (message "Opened PR: %s" pr-url)))

;;;; --- magit-status-workspace ---------------------------------------------

(defun agent-repl--magit-display-buffer-same-window (buffer)
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
agent-repl panel state.  Forces same-window display by let-binding
`magit-display-buffer-function' for this call only, so other magit
buffers (diffs, logs, etc.) keep their normal display behavior.

If the selected window is a side window (e.g., the workspace
drawer), first pops to the frame's main window so magit replaces the
main buffer rather than failing on the dedicated side window.

When the current workspace is tracked by agent-repl, uses the
workspace's `:project-dir' and clears any saved `:fullscreen-config'
(the saved pre-panel layout is moot once magit replaces the current
window).  When the workspace is NOT tracked by agent-repl (e.g.,
the main \"doom\" workspace, or a workspace whose entry has been
nuked), falls back to `default-directory' so magit still opens and
skips the `:fullscreen-config' write to avoid creating a stub entry
(see `agent-repl--ws-put' STUB-CREATE warning).

When the Claude panels are visible (they always fill the frame —
fullscreen is the sole display format), closes the input window and
un-dedicates the vterm window before opening magit so that
`display-buffer-same-window' can replace the vterm window cleanly
instead of splitting it."
  (interactive)
  (when (window-parameter (selected-window) 'window-side)
    (select-window (window-main-window)))
  (let* ((ws (agent-repl--ws-current-name))
         (tracked-dir (ignore-errors (agent-repl--ws-dir ws)))
         (dir (or tracked-dir default-directory))
         (magit-display-buffer-function
          #'agent-repl--magit-display-buffer-same-window))
    (when (fboundp 'agent-repl--log)
      (agent-repl--log ws "magit-status-workspace: same-window dir=%s tracked=%s"
                        dir (if tracked-dir "yes" "no")))
    (when tracked-dir
      ;; When the panels are visible, close the input window and un-dedicate
      ;; the vterm window so magit can replace the vterm window via same-window
      ;; display without splitting it (the vterm window is dedicated, which
      ;; blocks same-window).
      (when (agent-repl--panels-visible-p)
        (let ((input-buf (agent-repl--ws-get ws :input-buffer))
              (vterm-buf (agent-repl--ws-get ws :vterm-buffer)))
          (when input-buf
            (agent-repl--close-buffer-window input-buf))
          (when vterm-buf
            (when-let ((vterm-win (get-buffer-window vterm-buf)))
              (set-window-dedicated-p vterm-win nil)
              (select-window vterm-win)))))
      (agent-repl--ws-put ws :fullscreen-config nil))
    (magit-status dir)))

;;;; --- Hide Claude panels before magit-status RET actions -----------------

(defcustom agent-repl-magit-hide-panels-advised-fns
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
  :group 'agent-repl)

(defun agent-repl--magit-hide-panels-before-action (&rest _)
  "Hide Claude REPL panels before a magit-status RET action opens a new buffer.
No-op unless the caller's buffer is in `magit-status-mode' and both
panels are currently visible.  Routes through `agent-repl--hide-panels'
so `:repl-state'/`:claude-state' are left untouched — the panels simply
become hidden, matching the behavior of other non-user-initiated close
paths (see `agent-repl--on-close' for the user-initiated path that
transitions `:repl-state' to :inactive)."
  (when (and (eq major-mode 'magit-status-mode)
             (agent-repl--panels-visible-p))
    (agent-repl--hide-panels)))

(dolist (fn agent-repl-magit-hide-panels-advised-fns)
  (advice-add fn :before #'agent-repl--magit-hide-panels-before-action))

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
