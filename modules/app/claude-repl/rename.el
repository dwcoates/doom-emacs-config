;;; rename.el --- rename a claude-repl workspace -*- lexical-binding: t; -*-

;;; Commentary:

;; End-to-end workspace rename: git branch, companion tag branch,
;; worktree directory, projectile entry, perspective, vterm/input
;; buffers, owning-workspace buffer-locals, and `:source-ws-dir'
;; back-references in peer workspaces.

;;; Code:

(defun claude-repl--rename-derive-branch (old-branch new-name)
  "Derive the new branch name from OLD-BRANCH given user-supplied NEW-NAME.
When NEW-NAME contains a `/', it is treated as a fully-qualified branch
ref and used as-is.  Otherwise OLD-BRANCH's directory prefix (e.g.
\"DWC/\") is preserved and only the basename is replaced.  Returns the
fully-qualified branch ref to rename to."
  (if (string-match-p "/" new-name)
      new-name
    (let ((old-dir (file-name-directory old-branch))
          (new-bare (claude-repl--bare-workspace-name new-name)))
      (if old-dir
          (concat old-dir new-bare)
        new-bare))))

(defun claude-repl--rename-resolve-current-branch (path)
  "Return the current branch checked out in PATH, or nil on detached HEAD.
Signals `user-error' when PATH is not inside a git repo."
  (let ((branch (claude-repl--git-string
                 "-C" path "rev-parse" "--abbrev-ref" "HEAD")))
    (when (or (string-empty-p branch)
              (string-prefix-p "fatal" branch))
      (user-error "Cannot resolve current branch in %s" path))
    (unless (string= branch "HEAD")
      branch)))

(defun claude-repl--rename-validate (old-ws new-bare new-branch new-tag-branch new-path old-path)
  "Validate that OLD-WS can be renamed to NEW-BARE.
Checks for empty/identical names, existing target path, existing target
branches (main and companion tag), and existing target workspace name.
Signals `user-error' on any conflict."
  (when (string-empty-p new-bare)
    (user-error "New workspace name cannot be empty"))
  (when (string= old-ws new-bare)
    (user-error "New name is identical to current name"))
  (when (file-exists-p new-path)
    (user-error "Target path already exists: %s" new-path))
  (when (claude-repl--git-branch-exists-p old-path new-branch)
    (user-error "Branch '%s' already exists" new-branch))
  (when (and new-tag-branch
             (claude-repl--git-branch-exists-p old-path new-tag-branch))
    (user-error "Tag branch '%s' already exists" new-tag-branch))
  (when (member new-bare (+workspace-list-names))
    (user-error "Workspace '%s' already exists" new-bare)))

(defun claude-repl--rename-assert-no-pending-merge (path)
  "Signal `user-error' if PATH has an in-flight cherry-pick, merge, or rebase.
A pending operation would be silently broken by the worktree move."
  (let* ((git-dir (claude-repl--git-string
                   "-C" path "rev-parse" "--absolute-git-dir"))
         (markers '("CHERRY_PICK_HEAD" "MERGE_HEAD" "REBASE_HEAD"
                    "rebase-merge" "rebase-apply")))
    (dolist (marker markers)
      (when (file-exists-p (expand-file-name marker git-dir))
        (user-error "Workspace has in-flight %s — finish or abort before renaming"
                    marker)))))

(defun claude-repl--rename-git-branch (path old-branch new-branch)
  "Rename OLD-BRANCH to NEW-BRANCH in the repo at PATH.
Signals `error' on failure so the orchestrator can roll back."
  (let ((exit-code (claude-repl--git-exit-code
                    path "branch" "-m" old-branch new-branch)))
    (claude-repl--log nil "rename-git-branch: path=%s %s -> %s exit=%d"
                      path old-branch new-branch exit-code)
    (unless (zerop exit-code)
      (error "Failed to rename branch '%s' -> '%s' in %s (exit %d)"
             old-branch new-branch path exit-code))))

(defun claude-repl--rename-git-worktree-move (path old-path new-path)
  "Move the worktree at OLD-PATH to NEW-PATH.
PATH is the cwd for the git invocation — kept distinct so the caller
can supply a stable repo path that survives the move.
Signals `error' on failure."
  (let ((exit-code (claude-repl--git-exit-code
                    path "worktree" "move" old-path new-path)))
    (claude-repl--log nil "rename-git-worktree-move: cwd=%s %s -> %s exit=%d"
                      path old-path new-path exit-code)
    (unless (zerop exit-code)
      (error "Failed to move worktree '%s' -> '%s' (exit %d)"
             old-path new-path exit-code))))

(defun claude-repl--rename-execute-git
    (old-path new-path git-cwd old-branch new-branch old-tag-branch new-tag-branch)
  "Perform the git-level rename: branch, optional tag-branch, worktree move.
GIT-CWD is the directory passed to `git -C' — must remain valid across
the worktree move (so we use the common-dir or a sibling, not OLD-PATH).
On any failure mid-flight, attempts to roll back any already-applied
rename so the repo is left in its original state."
  (let ((branch-renamed nil)
        (tag-renamed nil))
    (condition-case err
        (progn
          (claude-repl--rename-git-branch git-cwd old-branch new-branch)
          (setq branch-renamed t)
          (when (and old-tag-branch new-tag-branch
                     (claude-repl--git-branch-exists-p git-cwd old-tag-branch))
            (claude-repl--rename-git-branch git-cwd old-tag-branch new-tag-branch)
            (setq tag-renamed t))
          (claude-repl--rename-git-worktree-move git-cwd old-path new-path))
      (error
       (claude-repl--log nil "rename-execute-git: rollback after error: %S" err)
       (when tag-renamed
         (ignore-errors
           (claude-repl--rename-git-branch git-cwd new-tag-branch old-tag-branch)))
       (when branch-renamed
         (ignore-errors
           (claude-repl--rename-git-branch git-cwd new-branch old-branch)))
       (signal (car err) (cdr err))))))

(defun claude-repl--rename-rehash-state (old-ws new-ws new-path)
  "Rehash workspace state from OLD-WS to NEW-WS, refreshing the project dir.
Copies the plist for OLD-WS to NEW-WS, drops the cached `:ws-id'
(it is derived from `:project-dir', which is changing), writes the new
`:project-dir', and removes the OLD-WS entry."
  (let* ((plist (gethash old-ws claude-repl--workspaces))
         (canonical (claude-repl--path-canonical new-path))
         (with-dir (plist-put (copy-sequence plist)
                              :project-dir canonical))
         ;; Clearing :ws-id rather than deleting — plist-put with nil
         ;; leaves the key but with a nil value, which `--ws-id-cached'
         ;; treats as missing and recomputes.
         (without-id (plist-put with-dir :ws-id nil)))
    (puthash new-ws without-id claude-repl--workspaces)
    (remhash old-ws claude-repl--workspaces)
    (claude-repl--log nil "rename-rehash-state: %s -> %s project-dir=%s"
                      old-ws new-ws canonical)))

(defun claude-repl--rename-update-source-back-refs (old-path new-path)
  "Update peer workspaces' `:source-ws-dir' from OLD-PATH to NEW-PATH.
Any workspace recorded as having OLD-PATH as its source (i.e., it was
forked off the workspace being renamed) is rewritten to point at
NEW-PATH so `SPC TAB M' continues to route the merge correctly."
  (let ((canonical-old (claude-repl--path-canonical old-path))
        (canonical-new (claude-repl--path-canonical new-path)))
    (maphash
     (lambda (ws plist)
       (when-let ((src (plist-get plist :source-ws-dir)))
         (when (string= (claude-repl--path-canonical src) canonical-old)
           (claude-repl--ws-put ws :source-ws-dir canonical-new)
           (claude-repl--log ws "rename: source-ws-dir %s -> %s" src canonical-new))))
     claude-repl--workspaces)))

(defun claude-repl--rename-buffer-safe (buf new-name)
  "Rename BUF to NEW-NAME, ignoring errors.
Returns t on success, nil on failure.  Used so a stale buffer name
collision can't abort the rename mid-flight."
  (when (and buf (buffer-live-p buf))
    (with-current-buffer buf
      (condition-case err
          (progn (rename-buffer new-name t) t)
        (error
         (claude-repl--log nil "rename-buffer-safe: failed %s -> %s: %S"
                           (buffer-name buf) new-name err)
         nil)))))

(defun claude-repl--rename-update-buffers (old-ws new-ws new-path)
  "Update vterm/input buffers tracked by NEW-WS after the hash rehash.
Renames each buffer to its NEW-WS-derived name, repoints
`default-directory' to NEW-PATH, and sets the buffer-local
`claude-repl--owning-workspace' to NEW-WS so future lookups resolve
to the new identity."
  (let ((vbuf (claude-repl--ws-get new-ws :vterm-buffer))
        (ibuf (claude-repl--ws-get new-ws :input-buffer))
        (new-dir (file-name-as-directory new-path)))
    (when vbuf
      (claude-repl--rename-buffer-safe
       vbuf (claude-repl--buffer-name nil new-ws))
      (when (buffer-live-p vbuf)
        (with-current-buffer vbuf
          (setq default-directory new-dir)
          (setq-local claude-repl--owning-workspace new-ws))))
    (when ibuf
      (claude-repl--rename-buffer-safe
       ibuf (claude-repl--buffer-name "-input" new-ws))
      (when (buffer-live-p ibuf)
        (with-current-buffer ibuf
          (setq default-directory new-dir)
          (setq-local claude-repl--owning-workspace new-ws))))
    ;; Also catch any other claude-panel-* buffers that claim OLD-WS as
    ;; owner but weren't tracked in the plist (legacy/stale entries).
    (dolist (buf (buffer-list))
      (when (and (buffer-live-p buf)
                 (equal old-ws (buffer-local-value
                                'claude-repl--owning-workspace buf)))
        (with-current-buffer buf
          (setq-local claude-repl--owning-workspace new-ws))))))

(defun claude-repl--rename-persp (old-ws new-ws)
  "Rename the persp-mode perspective from OLD-WS to NEW-WS.
No-op when `persp-rename' or `persp-get-by-name' is unbound (tests).
Signals `error' when the persp exists but the rename fails — a
persistent old-name persp would diverge from the renamed state."
  (when (and (fboundp 'persp-rename) (fboundp 'persp-get-by-name))
    (when-let ((persp (persp-get-by-name old-ws)))
      (unless (persp-rename new-ws persp)
        (error "persp-rename %s -> %s failed" old-ws new-ws)))))

(defun claude-repl--rename-update-history (old-ws new-ws)
  "Replace OLD-WS with NEW-WS in `+dwc/workspace-history'."
  (when (boundp '+dwc/workspace-history)
    (setq +dwc/workspace-history
          (mapcar (lambda (n) (if (string= n old-ws) new-ws n))
                  +dwc/workspace-history))))

(defun claude-repl--rename-update-projectile (old-path new-path)
  "Move projectile's known-project entry from OLD-PATH to NEW-PATH."
  (when (fboundp 'projectile-remove-known-project)
    (ignore-errors
      (projectile-remove-known-project (file-name-as-directory old-path))))
  (when (fboundp 'projectile-add-known-project)
    (ignore-errors
      (projectile-add-known-project (file-name-as-directory new-path)))))

(defun claude-repl--rename-git-common-cwd (old-path)
  "Return a stable directory to pass as `git -C' across the worktree move.
Resolves to the repo's common-dir parent (the main checkout) so the
location survives moving OLD-PATH itself.  Falls back to OLD-PATH when
the common-dir cannot be resolved or its parent does not exist."
  (let ((common (claude-repl--git-string-quiet
                 "-C" old-path "rev-parse" "--git-common-dir")))
    (cond
     ((or (null common) (string-empty-p common)
          (string-prefix-p "fatal" common))
      old-path)
     (t
      ;; --git-common-dir may be returned as a relative path (resolved
      ;; against OLD-PATH) or absolute.  `expand-file-name' handles both.
      ;; The result is .../.git, so its parent directory is the main
      ;; checkout.
      (let* ((abs (expand-file-name common old-path))
             (parent (file-name-directory (directory-file-name abs))))
        (if (and parent (file-directory-p parent))
            parent
          old-path))))))

(defun claude-repl--do-rename-workspace (old-ws new-name)
  "Rename workspace OLD-WS to NEW-NAME.
Renames the git branch (preserving any directory prefix when NEW-NAME
is bare), the companion tag branch (when configured), the worktree
directory, the projectile entry, the perspective, vterm/input buffers,
and any peer workspace's `:source-ws-dir' back-reference.
Signals `user-error' on validation failures and surfaces git-level
errors verbatim after attempting a best-effort rollback."
  (let* ((old-ws (claude-repl--bare-workspace-name old-ws))
         (new-bare (claude-repl--bare-workspace-name new-name))
         (old-path (claude-repl--ws-dir old-ws))
         (canonical-old (claude-repl--path-canonical old-path))
         (parent (file-name-directory (directory-file-name canonical-old)))
         (new-path (claude-repl--path-canonical
                    (expand-file-name new-bare parent)))
         (old-branch (claude-repl--rename-resolve-current-branch old-path))
         (_ (unless old-branch
              (user-error "Cannot rename a detached-HEAD worktree")))
         (new-branch (claude-repl--rename-derive-branch old-branch new-name))
         (old-tag-branch (claude-repl--tag-branch-name old-branch))
         (new-tag-branch (claude-repl--tag-branch-name new-branch))
         (git-cwd (claude-repl--rename-git-common-cwd old-path)))
    (claude-repl--log old-ws
                      "rename: old-ws=%s new-bare=%s old-path=%s new-path=%s old-branch=%s new-branch=%s git-cwd=%s"
                      old-ws new-bare old-path new-path
                      old-branch new-branch git-cwd)
    (claude-repl--rename-validate
     old-ws new-bare new-branch new-tag-branch new-path old-path)
    (claude-repl--rename-assert-no-pending-merge old-path)
    (claude-repl--rename-execute-git
     old-path new-path git-cwd
     old-branch new-branch old-tag-branch new-tag-branch)
    (claude-repl--rename-rehash-state old-ws new-bare new-path)
    (claude-repl--rename-update-source-back-refs old-path new-path)
    (claude-repl--rename-update-buffers old-ws new-bare new-path)
    (claude-repl--rename-update-history old-ws new-bare)
    (claude-repl--rename-update-projectile old-path new-path)
    (claude-repl--rename-persp old-ws new-bare)
    (when (fboundp 'claude-repl--state-save)
      (ignore-errors (claude-repl--state-save new-bare)))
    (message "Renamed workspace '%s' -> '%s'." old-ws new-bare)))

(defun claude-repl-rename-workspace (new-name)
  "Rename the current claude-repl workspace to NEW-NAME.
Interactive prompt suggests the current branch as the default so users
can lightly edit the existing name rather than retype it.
NEW-NAME may include a directory prefix (e.g. \"DWC/foo\") to set the
branch ref explicitly; a bare name preserves the current branch's prefix."
  (interactive
   (let* ((ws (+workspace-current-name))
          (path (claude-repl--ws-dir ws))
          (default (or (claude-repl--rename-resolve-current-branch path)
                       ws)))
     (list (read-string (format "Rename '%s' to: " ws) default))))
  (claude-repl--do-rename-workspace (+workspace-current-name) new-name))

(defalias '+dwc/workspace-rename #'claude-repl-rename-workspace)

(provide 'claude-repl-rename)

;;; rename.el ends here
