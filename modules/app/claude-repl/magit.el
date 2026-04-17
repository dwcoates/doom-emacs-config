;;; magit.el --- magit integration for claude-repl + user magit commands -*- lexical-binding: t; -*-

;;; Commentary:

;; All magit-related claude-repl wiring lives here.  Previously split across
;; the top-level doomdir config.el; moved here so it reloads with the module
;; (see AGENTS.md — no claude-repl code in the doomdir config.el).

;;; Code:

;;;; --- magit settings and keybindings ---------------------------------------

(after! magit
  (setq magit-no-confirm (append magit-no-confirm '(abort-revert abort-rebase abort-merge))
        magit-diff-visit-previous-blob nil)

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
  (let* ((commit-sha (string-trim (shell-command-to-string "git rev-parse HEAD")))
         (remote-url (string-trim (shell-command-to-string "git config --get remote.origin.url")))
         (cleaned-url (replace-regexp-in-string "^git@github.com:" "https://github.com"
                                                (replace-regexp-in-string "\\.git$" "" remote-url)))
         (repo-name (progn
                      (if (string-match "github.com[:/]ChessCom/\\(.*\\)" cleaned-url)
                          (match-string 1 cleaned-url)
                        (error (format "Remote URL '%s' does not match expected pattern" cleaned-url)))))
         (github-url (format "https://github.com/ChessCom/%s/commit/%s" repo-name commit-sha)))
    (browse-url github-url)))

(defun +dwc/magit-copy-commit-link ()
  "Copy GitHub link for commit at point in magit buffer."
  (interactive)
  (let* ((commit-sha (magit-commit-at-point))
         (default-directory (magit-toplevel))
         (remote-url (string-trim (shell-command-to-string "git config --get remote.origin.url")))
         (cleaned-url (replace-regexp-in-string "^git@github.com:" "https://github.com"
                                                (replace-regexp-in-string "\\.git$" "" remote-url)))
         (repo-name (progn
                      (if (string-match "github.com[:/]ChessCom/\\(.*\\)" cleaned-url)
                          (match-string 1 cleaned-url)
                        (error (format "Remote URL '%s' does not match expected pattern" cleaned-url)))))
         (github-url (format "https://github.com/ChessCom/%s/commit/%s" repo-name commit-sha)))
    (kill-new github-url)
    (message "GitHub commit link copied to clipboard: %s" github-url)))

;;;; --- magit-status-workspace ---------------------------------------------

(defun +dwc/magit-status-workspace ()
  "Open magit-status for the current workspace's project root.
If a magit-status buffer is already visible, switch to that window and
revert the buffer in place instead of opening a fresh one."
  (interactive)
  (let ((magit-win (cl-loop for win in (window-list)
                            when (with-current-buffer (window-buffer win)
                                   (derived-mode-p 'magit-status-mode))
                            return win)))
    (when (fboundp 'claude-repl--log)
      (claude-repl--log nil
                        "magit-status-workspace: ENTER windows=%d magit-win=%s magit-buf=%s"
                        (length (window-list))
                        (if magit-win "found" "nil")
                        (if magit-win (buffer-name (window-buffer magit-win)) "-")))
    (if magit-win
        (progn
          (when (fboundp 'claude-repl--log)
            (claude-repl--log nil "magit-status-workspace: BRANCH=reuse toplevel=%s"
                              (with-current-buffer (window-buffer magit-win)
                                (ignore-errors (magit-toplevel)))))
          (select-window magit-win)
          (revert-buffer nil t))
      (when (fboundp 'claude-repl--log)
        (claude-repl--log nil "magit-status-workspace: BRANCH=fresh-open dir=%s"
                          (claude-repl--ws-dir (+workspace-current-name))))
      (magit-status (claude-repl--ws-dir (+workspace-current-name))))))

;;;; --- Keybindings --------------------------------------------------------

(map! :leader
      :desc "Magit status for workspace" "g g" #'+dwc/magit-status-workspace
      :desc "Magit status"               "g G" #'magit-status
      :desc "Open commit in GitHub"      "g O" #'+dwc/magit-open-commit-in-github)

(map! :map magit-status-mode-map
      "g c" #'+dwc/magit-copy-commit-link
      "g C" #'+dwc/magit-open-commit-in-github)

(map! :map (magit-status-mode-map magit-diff-section-base-map magit-diff-section-map)
      "C-<return>" #'magit-diff-visit-file-other-window)

;;; magit.el ends here
