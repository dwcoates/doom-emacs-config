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

;;;; --- magit settings and keybindings ---------------------------------------

(after! magit
  (setq magit-no-confirm (append magit-no-confirm claude-repl-magit-no-confirm-extras)
        magit-diff-visit-previous-blob nil)

  ;; Unfold these sections by default in magit-status
  (setq magit-section-initial-visibility-alist
        '((unpushed . show)
          (stashes  . show)
          (untracked . show)))

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

(defun +dwc/magit-status-workspace ()
  "Open magit-status with the canonical magit-left/claude-right layout.
Guarantees the layout regardless of starting state by first reducing
the frame to claude panels only — if claude isn't already fullscreen
but panels are visible, any non-panel windows (e.g. file buffers, an
existing magit window) are deleted first — then splitting the frame's
root window from the left and opening magit-status in the new left
window.  The saved `:fullscreen-config' is cleared since claude is no
longer fullscreen once magit shares the frame.

Falls back to a plain `magit-status' when no claude panels exist for
the workspace (nothing to anchor the layout against)."
  (interactive)
  (let* ((ws (+workspace-current-name))
         (claude-fs (claude-repl--ws-get ws :fullscreen-config))
         (vterm-buf (claude-repl--ws-get ws :vterm-buffer))
         (input-buf (claude-repl--ws-get ws :input-buffer))
         (panels-visible (and (buffer-live-p vterm-buf)
                              (buffer-live-p input-buf)
                              (get-buffer-window vterm-buf)
                              (get-buffer-window input-buf))))
    (when (fboundp 'claude-repl--log)
      (claude-repl--log ws
                        "magit-status-workspace: ENTER windows=%d claude-fs=%s panels-visible=%s"
                        (length (window-list))
                        (if claude-fs "yes" "no")
                        (if panels-visible "yes" "no")))
    (cond
     ((or claude-fs panels-visible)
      (when (fboundp 'claude-repl--log)
        (claude-repl--log ws "magit-status-workspace: BRANCH=split-left dir=%s"
                          (claude-repl--ws-dir ws)))
      ;; Step 1: ensure only claude panels are visible so the split is
      ;; deterministic.  No-op when claude is already fullscreen.
      (when (and panels-visible (not claude-fs))
        (claude-repl--delete-non-panel-windows vterm-buf input-buf))
      ;; Step 2: claude is no longer fullscreen once magit lands on the left.
      (claude-repl--ws-put ws :fullscreen-config nil)
      ;; Step 3: split the frame root from the left and open magit there.
      (let ((left-win (split-window (frame-root-window) nil 'left)))
        (select-window left-win)
        (magit-status (claude-repl--ws-dir ws))))
     (t
      (when (fboundp 'claude-repl--log)
        (claude-repl--log ws "magit-status-workspace: BRANCH=fallback-no-panels dir=%s"
                          (claude-repl--ws-dir ws)))
      (magit-status (claude-repl--ws-dir ws))))))

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
      "g C" #'+dwc/magit-open-commit-in-github)

(map! :map (magit-status-mode-map magit-diff-section-base-map magit-diff-section-map)
      "C-<return>" #'magit-diff-visit-file-other-window)

;;; magit.el ends here
