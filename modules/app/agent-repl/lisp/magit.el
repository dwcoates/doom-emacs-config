;;; magit.el --- magit integration for agent-repl + user magit commands -*- lexical-binding: t; -*-

;;; Commentary:

;; All magit-related agent-repl wiring lives here.  Previously split across
;; the top-level doomdir config.el; moved here so it reloads with the module
;; (see AGENTS.md — no agent-repl code in the doomdir config.el).

;;; Code:

;; magit is an external package and is not installable under `emacs -Q', so
;; its functions and special variables are declared here for the
;; byte-compiler.  The `defvar's matter beyond silencing a warning:
;; `agent-repl--magit-status-same-window' let-binds
;; `magit-display-buffer-function' to force same-window display, and without
;; the special declaration that binding compiles LEXICALLY and never reaches
;; `magit-status'.
(declare-function magit--insert-log "magit-log")
(declare-function magit-add-section-hook "magit-section")
(declare-function magit-diff-visit-file "magit-diff")
(declare-function magit-diff-visit-file-other-window "magit-diff")
(declare-function magit-diff-visit-worktree-file "magit-diff")
(declare-function magit-insert-heading "magit-section")
(declare-function magit-log-arguments "magit-log" (&optional mode))
(declare-function magit-log-goto-commit-section "magit-log" (rev))
(declare-function magit-log-setup-buffer "magit-log" (revs args files &optional locked focus))
(declare-function magit-section-update-highlight "magit-section" (&optional force))
(declare-function magit-insert-unpushed-to-upstream-or-recent "magit-status")
(declare-function magit-refresh "magit-mode")
(declare-function magit-status-setup-buffer "magit-status")
(defvar magit-diff-visit-previous-blob)
(defvar magit-display-buffer-function)
(defvar magit-file-section-map)
(defvar magit-hunk-section-map)
(defvar magit-no-confirm)
(defvar magit-section-initial-visibility-alist)

;; Cross-file forward declarations.  These sources load in the dependency
;; order config.el establishes and resolve each other's calls at call time,
;; so the declarations below exist for the byte-compiler alone.
(declare-function agent-repl--close-buffer-window "panels")
(declare-function agent-repl--error "core")
(declare-function agent-repl--gh-string-quiet "core")
(declare-function agent-repl--info "core")
(declare-function agent-repl--git-string "core")
(declare-function agent-repl--git-string-quiet "core")
(declare-function agent-repl--hide-panels "panels")
(declare-function agent-repl--log "core")
(declare-function agent-repl--log-verbose "core")
(declare-function agent-repl--main-worktree-for-dir "workspace")
(declare-function agent-repl--panels-visible-p "panels")
(declare-function agent-repl-popup-show "popup" (buffer))
(declare-function agent-repl--ws-current-name "workspace")
(declare-function agent-repl--ws-dir "status")
(declare-function agent-repl--ws-get "workspace")
(declare-function agent-repl--ws-put "workspace")

(declare-function magit-commit-at-point "magit-git")

(defcustom agent-repl-magit-no-confirm-extras '(abort-revert abort-rebase abort-merge)
  "Extra actions to add to `magit-no-confirm'."
  :type '(repeat symbol)
  :group 'agent-repl)

(defcustom agent-repl-magit-github-ssh-prefix-regexp "^git@github.com:"
  "Regexp matching the SSH prefix in git remote URLs."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-magit-github-base-url "https://github.com/"
  "Base URL for GitHub, used when converting SSH remote URLs to HTTPS.
Carries the trailing slash `agent-repl-magit-github-ssh-prefix-regexp'
consumes along with the SSH prefix's colon — without it, substituting
this in place of \"git@github.com:\" would glue the owner/repo path
directly onto \"github.com\" with no separator at all."
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

(defcustom agent-repl-magit-merge-base-ref nil
  "Base ref the magit-status merge-base section computes against, or nil.
When nil, the merge-base is computed against the repository's main
branch as returned by `magit-main-branch' (typically `master'), so a
workspace forked off `master' shows the commit it forked from.  When a
non-empty string, that ref is used verbatim (e.g. \"origin/master\").
The section is inserted by `agent-repl--magit-insert-merge-base' and
shows the single commit where the current branch diverged from the
base ref."
  :type '(choice (const :tag "Repository main branch" nil)
                 (string :tag "Explicit ref"))
  :group 'agent-repl)

;;;; --- magit settings and keybindings ---------------------------------------

;;;; --- magit-status merge-base section --------------------------------------

(declare-function magit-main-branch "magit-git")

(defun agent-repl--magit-merge-base-ref ()
  "Return the ref to compute HEAD's merge-base against, or nil.
Returns `agent-repl-magit-merge-base-ref' when it is a non-empty
string; otherwise the repository's main branch via `magit-main-branch'
\(nil when the repository has no recognizable main branch)."
  (let ((ws (agent-repl--ws-current-name)))
    (if (and (stringp agent-repl-magit-merge-base-ref)
             (not (string-empty-p agent-repl-magit-merge-base-ref)))
        (progn
          (agent-repl--log-verbose ws "magit-merge-base-ref: ws=%s branch=explicit base=%s"
                                    ws agent-repl-magit-merge-base-ref)
          agent-repl-magit-merge-base-ref)
      (let ((base (magit-main-branch)))
        (agent-repl--log-verbose ws "magit-merge-base-ref: ws=%s branch=main-branch configured=%S base=%s"
                                  ws agent-repl-magit-merge-base-ref base)
        base))))

(defun agent-repl--magit-merge-base-commit ()
  "Return (BASE . SHA) for HEAD's merge-base with BASE, or nil.
BASE is `agent-repl--magit-merge-base-ref'; SHA is the merge-base
commit of HEAD and BASE — the point where the current branch diverged
from BASE.  Returns nil when no base ref resolves, when BASE names the
branch HEAD is already on (a branch's merge-base with itself is not
worth showing), or when git yields no usable merge-base.  Git reads
route through `agent-repl--git-string-quiet' so tests mock that wrapper
rather than shelling out (see AGENTS.md)."
  (let* ((ws (agent-repl--ws-current-name))
         (base (agent-repl--magit-merge-base-ref)))
    (if (not base)
        (progn
          (agent-repl--log-verbose ws "magit-merge-base-commit: ws=%s branch=no-base no-section" ws)
          nil)
      (let ((head-branch (agent-repl--git-string-quiet
                          "rev-parse" "--abbrev-ref" "HEAD")))
        (if (equal base head-branch)
            (progn
              (agent-repl--log-verbose ws "magit-merge-base-commit: ws=%s branch=on-base base=%s head=%s no-section"
                                        ws base head-branch)
              nil)
          (let ((sha (agent-repl--git-string-quiet "merge-base" "HEAD" base)))
            (if (and (stringp sha)
                     (not (string-empty-p sha))
                     (not (string-prefix-p "fatal" sha)))
                (progn
                  (agent-repl--log-verbose ws "magit-merge-base-commit: ws=%s branch=resolved base=%s head=%s sha=%s"
                                            ws base head-branch sha)
                  (cons base sha))
              (agent-repl--log-verbose ws "magit-merge-base-commit: ws=%s branch=invalid-sha base=%s head=%s sha=%S no-section"
                                        ws base head-branch sha)
              nil)))))))

(defun agent-repl--magit-insert-merge-base ()
  "Insert a magit-status section showing HEAD's merge-base with the base ref.
The base ref is resolved by `agent-repl--magit-merge-base-ref' and the
section shows the single fork-point commit (where the current branch
diverged from the base) as a navigable `commit' section, so RET on it
runs `magit-show-commit'.  Inserts nothing when
`agent-repl--magit-merge-base-commit' returns nil.  Registered on
`magit-status-sections-hook' after the unpushed/recent-commits section."
  (let ((ws (agent-repl--ws-current-name)))
    (if-let* ((pair (agent-repl--magit-merge-base-commit))
              (base (car pair))
              (sha (cdr pair)))
        (progn
          (agent-repl--log-verbose ws "magit-insert-merge-base: ws=%s branch=insert base=%s sha=%s"
                                    ws base sha)
          (magit-insert-section (commit sha)
            (magit-insert-heading
              (format (propertize "Merge base with %s"
                                  'font-lock-face 'magit-section-heading)
                      base))
            (magit--insert-log nil sha '("-1"))))
      (agent-repl--log-verbose ws "magit-insert-merge-base: ws=%s branch=no-merge-base no-op" ws))))

(after! magit
  (setq magit-no-confirm (append magit-no-confirm agent-repl-magit-no-confirm-extras)
        magit-diff-visit-previous-blob nil)

  ;; Unfold these sections by default in magit-status
  (setq magit-section-initial-visibility-alist
        '((unpushed . show)
          (stashes  . show)
          (untracked . show)))

  ;; Show the current branch's merge-base with the base ref (the fork
  ;; point) as a status section, just after the unpushed/recent commits.
  (magit-add-section-hook 'magit-status-sections-hook
                          #'agent-repl--magit-insert-merge-base
                          #'magit-insert-unpushed-to-upstream-or-recent
                          t)

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

(defun agent-repl-magit-open-commit-in-github ()
  "Open the current commit in GitHub browser.
Routes git reads through `agent-repl--git-string' so the function
is mockable per AGENTS.md."
  (interactive)
  (let* ((ws (agent-repl--ws-current-name))
         (default-directory (agent-repl--ws-dir ws))
         (commit-sha (agent-repl--git-string "rev-parse" "HEAD"))
         (remote-url (agent-repl--git-string "config" "--get" "remote.origin.url"))
         (cleaned-url (replace-regexp-in-string agent-repl-magit-github-ssh-prefix-regexp agent-repl-magit-github-base-url
                                                (replace-regexp-in-string "\\.git$" "" remote-url)))
         (repo-name (progn
                      (if (string-match agent-repl-magit-github-org-regexp cleaned-url)
                          (match-string 1 cleaned-url)
                        (agent-repl--log ws "magit-open-commit: ws=%s branch=invalid-remote dir=%s remote=%s cleaned=%s regexp=%s commit=%s"
                                          ws default-directory remote-url cleaned-url
                                          agent-repl-magit-github-org-regexp commit-sha)
                        (error "Remote URL '%s' does not match expected pattern" cleaned-url))))
         (github-url (format agent-repl-magit-github-commit-url-format repo-name commit-sha)))
    (agent-repl--log ws "magit-open-commit: ws=%s branch=browse dir=%s remote=%s repo=%s commit=%s url=%s"
                      ws default-directory remote-url repo-name commit-sha github-url)
    (browse-url github-url)
    (agent-repl--log ws "magit-open-commit: ws=%s branch=opened url=%s" ws github-url)))

(defun agent-repl-magit-copy-commit-link ()
  "Copy GitHub link for commit at point in magit buffer.
Routes git reads through `agent-repl--git-string' so the function
is mockable per AGENTS.md."
  (interactive)
  (let* ((ws (agent-repl--ws-current-name))
         (commit-sha (magit-commit-at-point))
         (default-directory (agent-repl--ws-dir ws))
         (remote-url (agent-repl--git-string "config" "--get" "remote.origin.url"))
         (cleaned-url (replace-regexp-in-string agent-repl-magit-github-ssh-prefix-regexp agent-repl-magit-github-base-url
                                                (replace-regexp-in-string "\\.git$" "" remote-url)))
         (repo-name (progn
                      (if (string-match agent-repl-magit-github-org-regexp cleaned-url)
                          (match-string 1 cleaned-url)
                        (agent-repl--log ws "magit-copy-commit-link: ws=%s branch=invalid-remote dir=%s remote=%s cleaned=%s regexp=%s commit=%s"
                                          ws default-directory remote-url cleaned-url
                                          agent-repl-magit-github-org-regexp commit-sha)
                        (error "Remote URL '%s' does not match expected pattern" cleaned-url))))
         (github-url (format agent-repl-magit-github-commit-url-format repo-name commit-sha)))
    (agent-repl--log ws "magit-copy-commit-link: ws=%s branch=copy dir=%s remote=%s repo=%s commit=%s url=%s"
                      ws default-directory remote-url repo-name commit-sha github-url)
    (kill-new github-url)
    (agent-repl--log ws "magit-copy-commit-link: ws=%s branch=copied url=%s" ws github-url)
    (message "GitHub commit link copied to clipboard: %s" github-url)))

;;;; --- GitHub PR resolution via gh CLI ------------------------------------

(defun agent-repl--gh-pr-url-for-branch (project-dir branch)
  "Return the GitHub PR URL for BRANCH in PROJECT-DIR, or nil if none.
Resolves via `gh pr view BRANCH --json url --jq .url' run from
PROJECT-DIR.  Routes through `agent-repl--gh-string-quiet' (the
external boundary for the GitHub CLI) so tests mock that wrapper
rather than invoking real `gh' (see AGENTS.md \"No External
Processes or External State in Tests\")."
  (let* ((ws (agent-repl--ws-current-name))
         (default-directory (file-name-as-directory project-dir))
         (output (agent-repl--gh-string-quiet
                  "pr" "view" branch "--json" "url" "--jq" ".url"))
         (url-p (string-prefix-p "http" output)))
    (agent-repl--log ws "gh-pr-url-for-branch: ws=%s project-dir=%s branch=%s output=%S branch-result=%s"
                      ws project-dir branch output (if url-p "url" "no-url"))
    (and url-p output)))

(defun agent-repl-open-workspace-pr-in-browser ()
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
      (agent-repl--log ws "open-workspace-pr: ws=%s branch=no-pr project-dir=%s git-branch=%s"
                        ws project-dir branch)
      (user-error "No PR found for branch '%s' in workspace '%s'" branch ws))
    (agent-repl--log ws "open-workspace-pr: ws=%s branch=browse project-dir=%s git-branch=%s url=%s"
                      ws project-dir branch pr-url)
    (browse-url pr-url)
    (agent-repl--log ws "open-workspace-pr: ws=%s branch=opened url=%s" ws pr-url)
    (message "Opened PR: %s" pr-url)))

;;;; --- magit-status-workspace ---------------------------------------------

(defun agent-repl--magit-display-buffer-same-window (buffer)
  "Display BUFFER in the selected window.
Used as a let-bound override for `magit-display-buffer-function' so
the top-level `magit-status' call from `agent-repl-magit-status-workspace'
replaces the current buffer rather than splitting or popping up a
new window.  Returns the window magit should select."
  (let ((ws (agent-repl--ws-current-name)))
    (agent-repl--log-verbose ws "magit-display-buffer-same-window: ws=%s buffer=%S selected-window=%S"
                              ws buffer (selected-window))
    (display-buffer buffer '(display-buffer-same-window))))

(defun agent-repl--magit-displaying (display-fn open-fn)
  "Call OPEN-FN with magit loaded and DISPLAY-FN showing its buffers.
Every magit buffer agent-repl opens at a place of its own choosing goes
through here: magit is required (NOERROR) FIRST, and only then is
`magit-display-buffer-function' let-bound to DISPLAY-FN around OPEN-FN.
OPEN-FN receives the `require' result, for its caller's log record.

THE ORDER IS THE POINT.  When magit has not loaded yet,
`magit-display-buffer-function' is not yet special, so a `let' of it
binds LEXICALLY, and the magit entry point inside then autoloads magit,
whose own `defvar' signals (\"Defining as dynamic an already lexical
var\") and aborts magit's load halfway (see
`agent-repl--magit-status-same-window').  The require is quiet because
the magit entry point OPEN-FN calls still signals loudly when magit is
genuinely unavailable.  Returns OPEN-FN's value."
  (let ((require-result (require 'magit nil t)))
    (let ((magit-display-buffer-function display-fn))
      (funcall open-fn require-result))))

(defun agent-repl--magit-status-same-window (dir)
  "Open `magit-status' for DIR, forcing same-window display.
Binds `magit-display-buffer-function' to
`agent-repl--magit-display-buffer-same-window' so the status buffer
replaces the selected window's buffer rather than splitting.

This is the canonical door every workspace-bring-up path opens magit
through (restore via `+workspaces-switch-project-function', worktree
create via `agent-repl--drain-pending-magit').  Without the same-window
binding, Doom's
`+magit-display-buffer-fn' routes a `magit-status-mode' buffer through
`+magit--display-buffer-in-direction' (a SPLIT) whenever the selected
window already shows a DIFFERENT repo's `magit-status' buffer — so a
workspace opening its magit while the prior workspace's magit is still
current lands TWO status windows side by side, the double-magit-windows
bug.

Magit MUST be loaded before the `let' below.  When magit has not
loaded yet, `magit-display-buffer-function' is not yet special, so the
`let' binds it LEXICALLY — and the `magit-status' call inside then
autoloads magit, whose own `defvar' of the variable signals
\(\"Defining as dynamic an already lexical var\") and aborts magit's
load halfway.  That was the first-restored-workspace bug: every fresh
session's first workspace came up with no magit (Doom splash instead)
and its snapshot-load await burned the full ready-watchdog timeout.

The `require' is NOERROR on purpose: when magit is genuinely
unavailable the `magit-status-setup-buffer' call below is the canonical
entry point and still signals loudly, so no failure is swallowed — the
quiet require only exists to get magit's `defvar' evaluated before the
`let'.  (In the batch test harness magit is not installable at all;
test-helpers.el declares the variable special instead.)

IT IS `magit-status-setup-buffer', NOT `magit-status', AND THAT IS A
FIX, NOT A STYLE CHOICE.  magit marks `magit-status' `interactive-only'
and names `magit-status-setup-buffer' as the entry point Lisp callers
are to use, because `magit-status' carries the INTERACTIVE fallback: it
re-derives the toplevel, compares it to DIR with `file-equal-p', and on
a mismatch ASKS \\=`y-or-n-p' whether to create a repository there.

That question is unanswerable here and the comparison behind it is
racy.  `file-equal-p' does not compare device and inode; it stats each
path separately and compares the WHOLE `file-attributes' list, mtime,
ctime and size included (files.el, which already documents the same
fragility for Haiku's atime).  Every workspace bring-up opens magit on
a directory the daemon's registration is concurrently writing into
— `.claude/emacs/' lands in the project root — so a write landing
BETWEEN those two stats makes one identical path compare unequal to
itself.  Measured in the e2e Emacs layer at roughly one bring-up in
sixteen: magit then asked \"<dir> is a repository.  Create another in
<dir>? \" of the same directory twice, nothing could answer, the reader
entered a recursive edit and Emacs stopped answering its server socket
entirely — a permanent stall whose only witness was a native backtrace
(see e2e/EMACS-LAYER-SPEC.md).

`magit-status-setup-buffer' has no prompt in it, so the question cannot
be asked at all; a directory that is genuinely not a repository signals
loudly from magit's own refresh instead, which is what this module
wants anyway."
  (let ((ws (agent-repl--ws-current-name)))
    (agent-repl--magit-displaying
     #'agent-repl--magit-display-buffer-same-window
     (lambda (require-result)
       (agent-repl--log ws "magit-status-same-window: ws=%s dir=%s require-result=%S magit-loaded=%s selected-window=%S"
                         ws dir require-result (featurep 'magit) (selected-window))
       (magit-status-setup-buffer (file-name-as-directory (expand-file-name dir)))
       (agent-repl--log ws "magit-status-same-window: ws=%s branch=opened dir=%s selected-window=%S"
                         ws dir (selected-window))))))

(defun agent-repl-magit-log-commit (ws dir branch commit)
  "Open BRANCH's log in the repository at DIR with point on COMMIT, for WS.
The click on a landed merge's commit in the merge bubble's head
\(`agentrepl.v1' WatchHostWorkspace's `open_commit_log' arm).  The log
opens in the ONE shared agent-repl popup (`agent-repl-popup-show'), where
`q' closes it, with the log arguments magit would use itself.

A COMMIT THE LOG DOES NOT SHOW is an old landing beyond the log's commit
limit: the log stays open at its top and the echo area says so.  Returns
the log buffer."
  (agent-repl--magit-displaying
   #'agent-repl-popup-show
   (lambda (_require-result)
     (let* ((default-directory (file-name-as-directory (expand-file-name dir)))
            (arguments (magit-log-arguments))
            (buffer (magit-log-setup-buffer (list branch) (car arguments) (cadr arguments))))
       (with-current-buffer buffer
         (if (magit-log-goto-commit-section commit)
             (progn
               (magit-section-update-highlight)
               (when-let ((window (get-buffer-window buffer)))
                 (set-window-point window (point)))
               (agent-repl--info ws "elisp.magit.log-commit ws=%s dir=%s branch=%s commit=%s branch=shown"
                                 ws dir branch commit))
           (agent-repl--info ws "elisp.magit.log-commit ws=%s dir=%s branch=%s commit=%s branch=not-in-log"
                             ws dir branch commit)
           (message "agent-repl: %s is not among the commits %s's log shows" commit branch)))
       buffer))))

(defun agent-repl--magit-status-workspace-open (target-fn)
  "Open magit-status for the workspace in the SELECTED window, at TARGET-FN's dir.
The shared body of `agent-repl-magit-status-workspace' (`SPC g g') and
`agent-repl-magit-status-main-worktree' (`SPC g G').  TARGET-FN is
called with the workspace name and the workspace's directory and
returns the directory magit opens; it may signal to refuse, and it runs
before anything in the frame or the workspace state is touched, so a
refusal leaves both exactly as they were.

Always replaces the current buffer with the target's magit-status
— no splits, no window reuse — regardless of the prior layout or
agent-repl panel state.  Forces same-window display by let-binding
`magit-display-buffer-function' for this call only, so other magit
buffers (diffs, logs, etc.) keep their normal display behavior.

If the selected window is a side window, first pops to the frame's
main window so magit replaces the main buffer rather than failing on
the dedicated side window.

When the current workspace is tracked by agent-repl, the workspace's
directory is its `:project-dir' and any saved `:fullscreen-config' is
cleared (the saved pre-panel layout is moot once magit replaces the
current window).  When the workspace is NOT tracked by agent-repl (e.g.,
the main \"doom\" workspace, or a workspace whose entry has been
killed), the workspace's directory is `default-directory' so magit
still opens, and the `:fullscreen-config' write is skipped to avoid
creating a stub entry (see `agent-repl--ws-put' STUB-CREATE warning).

When the Claude panels are visible (they always fill the frame —
fullscreen is the sole display format), closes the input window and
un-dedicates the webview window before opening magit so that
`display-buffer-same-window' can replace the webview window cleanly
instead of splitting it."
  (let* ((ws (agent-repl--ws-current-name))
         (tracked-error nil)
         (tracked-dir (condition-case err
                          (agent-repl--ws-dir ws)
                        (error
                         (setq tracked-error err)
                         nil)))
         (ws-dir (or tracked-dir default-directory))
         (dir (funcall target-fn ws ws-dir)))
    (when (window-parameter (selected-window) 'window-side)
      (agent-repl--log ws "magit-status-workspace: ws=%s branch=side-window selected=%S main=%S"
                        ws (selected-window) (window-main-window))
      (select-window (window-main-window)))
    (agent-repl--log ws "magit-status-workspace: same-window ws-dir=%s dir=%s tracked=%s tracked-error=%S selected-window=%S"
                      ws-dir dir (if tracked-dir "yes" "no") tracked-error (selected-window))
    (when tracked-dir
      ;; When the panels are visible, close the input window and un-dedicate
      ;; the webview window so magit can replace the webview window via
      ;; same-window display without splitting it (the webview window is
      ;; dedicated, which blocks same-window).
      (if (agent-repl--panels-visible-p)
        (let ((input-buf (agent-repl--ws-get ws :input-buffer))
              (webview-buf (agent-repl--ws-get ws :frontend-buffer)))
          (agent-repl--log ws "magit-status-workspace: ws=%s branch=panels-visible input=%S frontend=%S"
                            ws input-buf webview-buf)
          (when input-buf
            (agent-repl--close-buffer-window input-buf))
          (when webview-buf
            (if-let ((webview-win (get-buffer-window webview-buf)))
                (progn
                  (agent-repl--log ws "magit-status-workspace: ws=%s branch=select-frontend-window frontend=%S window=%S"
                                    ws webview-buf webview-win)
                  (set-window-dedicated-p webview-win nil)
                  (select-window webview-win))
              (agent-repl--log ws "magit-status-workspace: ws=%s branch=no-frontend-window frontend=%S"
                                ws webview-buf))))
        (agent-repl--log ws "magit-status-workspace: ws=%s branch=panels-hidden" ws))
      (agent-repl--ws-put ws :fullscreen-config nil))
    (agent-repl--magit-status-same-window dir)
    (agent-repl--log ws "magit-status-workspace: ws=%s branch=complete dir=%s tracked=%s"
                      ws dir (if tracked-dir "yes" "no"))))

(defun agent-repl-magit-status-workspace ()
  "Open magit-status for the workspace's own worktree in the SELECTED window.
See `agent-repl--magit-status-workspace-open' for the window and panel
handling."
  (interactive)
  (agent-repl--magit-status-workspace-open (lambda (_ws ws-dir) ws-dir)))

(defun agent-repl--magit-main-worktree-target (ws ws-dir)
  "Return the main worktree of the repository holding WS-DIR, for WS.
Refuses with a `user-error' when WS-DIR is in no git repository, after
recording the failure through `agent-repl--error'."
  (or (agent-repl--main-worktree-for-dir ws-dir)
      (progn
        (agent-repl--error ws "magit-status-main-worktree: ws=%s ws-dir=%s cause=not-a-git-repository"
                           ws ws-dir)
        (user-error "agent-repl: %s is in no git repository" ws-dir))))

(defun agent-repl-magit-status-main-worktree ()
  "Open magit-status for the repository's MAIN worktree in the SELECTED window.
Like `agent-repl-magit-status-workspace' (`SPC g g'), but opens the main
checkout of the repository the workspace's worktree belongs to rather
than the workspace's own worktree.  See
`agent-repl--magit-status-workspace-open' for the window and panel
handling."
  (interactive)
  (agent-repl--magit-status-workspace-open #'agent-repl--magit-main-worktree-target))

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
  "Hide Agent REPL panels before a magit-status RET action opens a new buffer.
No-op unless the caller's buffer is in `magit-status-mode' and both
panels are currently visible.  Routes through `agent-repl--hide-panels'
so `:repl-state'/`:agent-state' are left untouched — the panels simply
become hidden, matching the behavior of other non-user-initiated close
paths (see `agent-repl--on-close' for the user-initiated path that
transitions `:repl-state' to :inactive)."
  (let* ((ws (agent-repl--ws-current-name))
         (in-status (eq major-mode 'magit-status-mode))
         (panels-visible (and in-status (agent-repl--panels-visible-p))))
    (agent-repl--log ws "magit-hide-panels-before-action: ws=%s mode=%S in-status=%s panels-visible=%s branch=%s"
                      ws major-mode in-status panels-visible
                      (if panels-visible "hide" "no-op"))
    (when panels-visible
      (agent-repl--hide-panels))))

(dolist (fn agent-repl-magit-hide-panels-advised-fns)
  (advice-add fn :before #'agent-repl--magit-hide-panels-before-action))

;;;; --- Keybindings --------------------------------------------------------

(map! :leader
      :desc "Magit status for workspace"     "g g" #'agent-repl-magit-status-workspace
      :desc "Magit status for main worktree" "g G" #'agent-repl-magit-status-main-worktree
      :desc "Open commit in GitHub"          "g O" #'agent-repl-magit-open-commit-in-github)

(map! :leader
      (:prefix "j"
       :desc "Open workspace PR in browser" "P" #'agent-repl-open-workspace-pr-in-browser))

(map! :map magit-status-mode-map
      "g c" #'agent-repl-magit-copy-commit-link
      "g C" #'agent-repl-magit-open-commit-in-github)

(map! :map (magit-status-mode-map magit-diff-section-base-map magit-diff-section-map)
      "C-<return>" #'magit-diff-visit-file-other-window)

;;; magit.el ends here
