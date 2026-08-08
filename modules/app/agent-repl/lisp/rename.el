;;; rename.el --- rename a agent-repl workspace -*- lexical-binding: t; -*-

;;; Commentary:

;; End-to-end workspace rename: git branch, companion start tag,
;; worktree directory, projectile entry, perspective, input/webview
;; buffers, owning-workspace buffer-locals, and `:source-ws-dir'
;; back-references in peer workspaces.

;;; Code:

(declare-function agent-repl--frontend-webview-buffer-name "agent-repl-frontend" (ws))

(defun agent-repl--rename-derive-branch (old-branch new-name &optional ws)
  "Derive the new branch name from OLD-BRANCH given user-supplied NEW-NAME.
When NEW-NAME contains a `/', it is treated as a fully-qualified branch
ref and used as-is.  Otherwise OLD-BRANCH's directory prefix (e.g.
\"DWC/\") is preserved and only the basename is replaced.  Returns the
fully-qualified branch ref to rename to.
WS, when non-nil, supplies workspace metadata for diagnostics."
  (let* ((qualified (string-match-p "/" new-name))
         (old-dir (and (not qualified)
                       (file-name-directory old-branch)))
         (new-bare (and (not qualified)
                        (agent-repl--bare-workspace-name new-name)))
         (result (cond
                  (qualified new-name)
                  (old-dir (concat old-dir new-bare))
                  (t new-bare))))
    (agent-repl--log
     ws
     "rename-derive-branch: ws=%S old-branch=%S new-name=%S qualified=%s old-dir=%S result=%S"
     ws old-branch new-name (if qualified "t" "nil") old-dir result)
    result))

(defun agent-repl--rename-resolve-current-branch (path &optional ws)
  "Return the current branch checked out in PATH, or nil on detached HEAD.
Signals `user-error' when PATH is not inside a git repo.
WS, when non-nil, supplies workspace metadata for diagnostics."
  (let ((branch (agent-repl--git-string
                 "-C" path "rev-parse" "--abbrev-ref" "HEAD")))
    (agent-repl--log
     ws "rename-resolve-current-branch: ws=%S path=%S raw-branch=%S"
     ws path branch)
    (when (or (string-empty-p branch)
              (string-prefix-p "fatal" branch))
      (agent-repl--log
       ws
       "rename-resolve-current-branch: REJECT ws=%S path=%S raw-branch=%S reason=unresolved"
       ws path branch)
      (user-error "Cannot resolve current branch in %s" path))
    (if (string= branch "HEAD")
        (progn
          (agent-repl--log
           ws
           "rename-resolve-current-branch: ws=%S path=%S outcome=detached-head"
           ws path)
          nil)
      (agent-repl--log
       ws
       "rename-resolve-current-branch: ws=%S path=%S outcome=attached branch=%S"
       ws path branch)
      branch)))

(defun agent-repl--rename-validate (old-ws new-bare new-branch new-path old-path)
  "Validate that OLD-WS can be renamed to NEW-BARE.
Checks for empty/identical names, existing target path, existing target
branch, and existing target workspace name.
Signals `user-error' on any conflict."
  (when (string-empty-p new-bare)
    (agent-repl--log
     old-ws
     "rename-validate: REJECT old-ws=%S new-bare=%S new-branch=%S new-path=%S old-path=%S reason=empty-name"
     old-ws new-bare new-branch new-path old-path)
    (user-error "New workspace name cannot be empty"))
  (when (string= old-ws new-bare)
    (agent-repl--log
     old-ws
     "rename-validate: REJECT old-ws=%S new-bare=%S new-branch=%S new-path=%S old-path=%S reason=identical-name"
     old-ws new-bare new-branch new-path old-path)
    (user-error "New name is identical to current name"))
  (when (file-exists-p new-path)
    (agent-repl--log
     old-ws
     "rename-validate: REJECT old-ws=%S new-bare=%S new-branch=%S new-path=%S old-path=%S reason=target-path-exists"
     old-ws new-bare new-branch new-path old-path)
    (user-error "Target path already exists: %s" new-path))
  (when (agent-repl--git-branch-exists-p old-path new-branch)
    (agent-repl--log
     old-ws
     "rename-validate: REJECT old-ws=%S new-bare=%S new-branch=%S new-path=%S old-path=%S reason=target-branch-exists"
     old-ws new-bare new-branch new-path old-path)
    (user-error "Branch '%s' already exists" new-branch))
  (when (member new-bare (agent-repl--ws-all-names))
    (agent-repl--log
     old-ws
     "rename-validate: REJECT old-ws=%S new-bare=%S new-branch=%S new-path=%S old-path=%S reason=target-workspace-exists"
     old-ws new-bare new-branch new-path old-path)
    (user-error "Workspace '%s' already exists" new-bare))
  (agent-repl--log
   old-ws
   "rename-validate: ACCEPT old-ws=%S new-bare=%S new-branch=%S new-path=%S old-path=%S"
   old-ws new-bare new-branch new-path old-path))

(defun agent-repl--rename-assert-no-pending-merge (path &optional ws)
  "Signal `user-error' if PATH has an in-flight cherry-pick, merge, or rebase.
A pending operation would be silently broken by the worktree move.
WS, when non-nil, supplies workspace metadata for diagnostics."
  (let* ((git-dir (agent-repl--git-string
                   "-C" path "rev-parse" "--absolute-git-dir"))
         (markers '("CHERRY_PICK_HEAD" "MERGE_HEAD" "REBASE_HEAD"
                    "rebase-merge" "rebase-apply")))
    (agent-repl--log
     ws "rename-pending-operation: scan ws=%S path=%S git-dir=%S markers=%S"
     ws path git-dir markers)
    (dolist (marker markers)
      (let* ((marker-path (expand-file-name marker git-dir))
             (present (file-exists-p marker-path)))
        (agent-repl--log-verbose
         ws
         "rename-pending-operation: inspect ws=%S path=%S marker=%S marker-path=%S present=%s"
         ws path marker marker-path (if present "t" "nil"))
        (when present
          (agent-repl--log
           ws
           "rename-pending-operation: REJECT ws=%S path=%S git-dir=%S marker=%S marker-path=%S"
           ws path git-dir marker marker-path)
          (user-error "Workspace has in-flight %s — finish or abort before renaming"
                      marker))))
    (agent-repl--log
     ws "rename-pending-operation: ACCEPT ws=%S path=%S git-dir=%S"
     ws path git-dir)))

(defun agent-repl--rename-git-branch (path old-branch new-branch &optional ws)
  "Rename OLD-BRANCH to NEW-BRANCH in the repo at PATH.
Signals `error' on failure so the orchestrator can roll back."
  (let ((exit-code (agent-repl--git-exit-code
                    path "branch" "-m" old-branch new-branch)))
    (agent-repl--log ws
                     "rename-git-branch: ws=%S path=%S old-branch=%S new-branch=%S exit=%d outcome=%s"
                     ws path old-branch new-branch exit-code
                     (if (zerop exit-code) "renamed" "failed"))
    (unless (zerop exit-code)
      (error "Failed to rename branch '%s' -> '%s' in %s (exit %d)"
             old-branch new-branch path exit-code))))

(defun agent-repl--rename-git-worktree-move (path old-path new-path &optional ws)
  "Move the worktree at OLD-PATH to NEW-PATH.
PATH is the cwd for the git invocation — kept distinct so the caller
can supply a stable repo path that survives the move.
Signals `error' on failure."
  (let ((exit-code (agent-repl--git-exit-code
                    path "worktree" "move" old-path new-path)))
    (agent-repl--log
     ws
     "rename-git-worktree-move: ws=%S cwd=%S old-path=%S new-path=%S exit=%d outcome=%s"
     ws path old-path new-path exit-code
     (if (zerop exit-code) "moved" "failed"))
    (unless (zerop exit-code)
      (error "Failed to move worktree '%s' -> '%s' (exit %d)"
             old-path new-path exit-code))))

(defun agent-repl--rename-execute-git
    (old-path new-path git-cwd old-branch new-branch &optional ws)
  "Perform the git-level rename: branch, then worktree move.
GIT-CWD is the directory passed to `git -C' — must remain valid across
the worktree move (so we use the common-dir or a sibling, not OLD-PATH).
On any failure mid-flight, attempts to roll back any already-applied
rename so the repo is left in its original state."
  (let ((branch-renamed nil))
    (agent-repl--log
     ws
     "rename-execute-git: begin ws=%S old-path=%S new-path=%S git-cwd=%S old-branch=%S new-branch=%S"
     ws old-path new-path git-cwd old-branch new-branch)
    (condition-case err
        (progn
          (agent-repl--rename-git-branch
           git-cwd old-branch new-branch ws)
          (setq branch-renamed t)
          (agent-repl--log
           ws
           "rename-execute-git: branch-step ws=%S old-branch=%S new-branch=%S outcome=renamed"
           ws old-branch new-branch)
          (agent-repl--rename-git-worktree-move
           git-cwd old-path new-path ws)
          (agent-repl--log
           ws
           "rename-execute-git: complete ws=%S old-path=%S new-path=%S old-branch=%S new-branch=%S"
           ws old-path new-path old-branch new-branch))
      (error
       (agent-repl--log
        ws
        "rename-execute-git: rollback begin ws=%S old-path=%S new-path=%S git-cwd=%S old-branch=%S new-branch=%S branch-renamed=%s original-error=%S"
        ws old-path new-path git-cwd old-branch new-branch
        (if branch-renamed "t" "nil") err)
       (if branch-renamed
           (condition-case rollback-branch-err
               (progn
                 (agent-repl--rename-git-branch
                  git-cwd new-branch old-branch ws)
                 (agent-repl--log
                  ws
                  "rename-execute-git: rollback-branch ws=%S from=%S to=%S outcome=restored"
                  ws new-branch old-branch))
             (error
              (agent-repl--warn
               ws
               "rename-execute-git: rollback-branch ws=%S from=%S to=%S outcome=failed error=%S"
               ws new-branch old-branch rollback-branch-err)))
         (agent-repl--log
          ws
          "rename-execute-git: rollback-branch ws=%S old-branch=%S new-branch=%S action=skip reason=not-renamed"
          ws old-branch new-branch))
       (agent-repl--log
        ws
        "rename-execute-git: rollback complete ws=%S original-error=%S action=resignal"
        ws err)
       (signal (car err) (cdr err))))))

(defun agent-repl--rename-rehash-state (old-ws new-ws new-path)
  "Rehash workspace state from OLD-WS to NEW-WS, refreshing the project dir.
Delegates the atomic move to the workspace-state owner, which preserves
the plist, updates `:project-dir', clears cached `:ws-id', and removes
the OLD-WS entry."
  (agent-repl--log
   old-ws
   "rename-rehash-state: begin old-ws=%S new-ws=%S new-path=%S"
   old-ws new-ws new-path)
  (let ((result (agent-repl--ws-rename-state old-ws new-ws new-path)))
    ;; OLD-WS is intentionally used after the move: logging against NEW-WS
    ;; would force lazy `:ws-id' recomputation through log metadata.
    (agent-repl--log
     old-ws
     "rename-rehash-state: complete old-ws=%S new-ws=%S new-path=%S result=%S project-dir=%S ws-id=%S"
     old-ws new-ws new-path result
     (agent-repl--ws-get new-ws :project-dir)
     (agent-repl--ws-get new-ws :ws-id))
    result))

(defun agent-repl--rename-update-source-back-refs (old-path new-path &optional ws)
  "Update peer workspaces' `:source-ws-dir' from OLD-PATH to NEW-PATH.
Any workspace recorded as having OLD-PATH as its source (i.e., it was
forked off the workspace being renamed) is rewritten to point at
NEW-PATH so `SPC TAB M' continues to route the merge correctly.

Also clears each peer's `:source-ws-name' cache (the resolved
source-workspace name).  The renamed workspace is rehashed under a
new name elsewhere in the rename flow, so any cached name pointing at
the old identity is stale — the next resolution repopulates against
the new name.  Returns the number of rewritten peer workspaces."
  (let ((count
         (agent-repl--ws-rewrite-source-back-refs old-path new-path)))
    (agent-repl--log
     ws
     "rename-source-back-refs: ws=%S old-path=%S new-path=%S rewritten-count=%d"
     ws old-path new-path count)
    count))

(defun agent-repl--rename-buffer-safe (buf new-name &optional ws)
  "Rename BUF to NEW-NAME, ignoring errors.
Returns t on success, nil on failure.  Used so a stale buffer name
collision can't abort the rename mid-flight.
WS, when non-nil, supplies workspace metadata for diagnostics."
  (cond
   ((null buf)
    (agent-repl--log
     ws "rename-buffer-safe: ws=%S buffer=nil new-name=%S action=skip"
     ws new-name)
    nil)
   ((not (buffer-live-p buf))
    (agent-repl--log
     ws "rename-buffer-safe: ws=%S buffer=%S new-name=%S action=skip reason=dead"
     ws buf new-name)
    nil)
   (t
    (with-current-buffer buf
      (let ((old-name (buffer-name buf)))
        (condition-case err
            (progn
              (rename-buffer new-name t)
              (agent-repl--log
               ws
               "rename-buffer-safe: ws=%S buffer=%S old-name=%S requested-name=%S actual-name=%S outcome=renamed"
               ws buf old-name new-name (buffer-name buf))
              t)
          (error
           (agent-repl--warn
            ws
            "rename-buffer-safe: ws=%S buffer=%S old-name=%S requested-name=%S outcome=failed error=%S"
            ws buf old-name new-name err)
           nil)))))))

(defun agent-repl--rename-update-buffers (old-ws new-ws new-path)
  "Update input/webview buffers tracked by NEW-WS after the hash rehash.
Renames each buffer to its NEW-WS-derived name, repoints
`default-directory' to NEW-PATH, and sets the buffer-local
`agent-repl--owning-workspace' to NEW-WS so future lookups resolve
to the new identity.

The webview buffer (`:frontend-buffer') is named via
`agent-repl--frontend-webview-buffer-name' rather than
`agent-repl--buffer-name' — it lives in its own `*agent-frontend-WS*'
namespace, deliberately outside the `*agent-panel-WS*' scheme the
latter produces (see frontend.el's buffer-identity commentary).  This
function used to rename only the vterm/input pair, so a renamed
workspace's webview kept its old name — a pre-existing gui bug fixed
here now that the webview is one of a workspace's two buffers."
  (let ((ibuf (agent-repl--ws-get new-ws :input-buffer))
        (wbuf (agent-repl--ws-get new-ws :frontend-buffer))
        (new-dir (file-name-as-directory new-path))
        (legacy-count 0))
    (agent-repl--log
     new-ws
     "rename-update-buffers: begin old-ws=%S new-ws=%S new-path=%S new-dir=%S input-buffer=%S input-live=%s frontend-buffer=%S frontend-live=%s"
     old-ws new-ws new-path new-dir
     ibuf (if (buffer-live-p ibuf) "t" "nil")
     wbuf (if (buffer-live-p wbuf) "t" "nil"))
    (let ((input-name (agent-repl--buffer-name "-input" new-ws)))
      (agent-repl--rename-buffer-safe ibuf input-name new-ws)
      (if (buffer-live-p ibuf)
          (progn
            (with-current-buffer ibuf
              (setq default-directory new-dir)
              (setq-local agent-repl--owning-workspace new-ws))
            (agent-repl--log
             new-ws
             "rename-update-buffers: input old-ws=%S new-ws=%S buffer=%S requested-name=%S actual-name=%S default-directory=%S owner=%S outcome=updated"
             old-ws new-ws ibuf input-name (buffer-name ibuf)
             (buffer-local-value 'default-directory ibuf)
             (buffer-local-value 'agent-repl--owning-workspace ibuf)))
        (agent-repl--log
         new-ws
         "rename-update-buffers: input old-ws=%S new-ws=%S buffer=%S requested-name=%S outcome=skip reason=absent-or-dead"
         old-ws new-ws ibuf input-name)))
    (let ((frontend-name
           (agent-repl--frontend-webview-buffer-name new-ws)))
      (agent-repl--rename-buffer-safe wbuf frontend-name new-ws)
      (if (buffer-live-p wbuf)
          (progn
            (with-current-buffer wbuf
              (setq default-directory new-dir)
              (setq-local agent-repl--owning-workspace new-ws))
            (agent-repl--log
             new-ws
             "rename-update-buffers: frontend old-ws=%S new-ws=%S buffer=%S requested-name=%S actual-name=%S default-directory=%S owner=%S outcome=updated"
             old-ws new-ws wbuf frontend-name (buffer-name wbuf)
             (buffer-local-value 'default-directory wbuf)
             (buffer-local-value 'agent-repl--owning-workspace wbuf)))
        (agent-repl--log
         new-ws
         "rename-update-buffers: frontend old-ws=%S new-ws=%S buffer=%S requested-name=%S outcome=skip reason=absent-or-dead"
         old-ws new-ws wbuf frontend-name)))
    ;; Also catch any other agent-panel-* buffers that claim OLD-WS as
    ;; owner but weren't tracked in the plist (legacy/stale entries).
    (dolist (buf (buffer-list))
      (let* ((live (buffer-live-p buf))
             (owner (and live
                         (buffer-local-value
                          'agent-repl--owning-workspace buf)))
             (matches (and live (equal old-ws owner))))
        (agent-repl--log-verbose
         new-ws
         "rename-update-buffers: scan old-ws=%S new-ws=%S buffer=%S name=%S live=%s owner=%S matches-old=%s"
         old-ws new-ws buf (and live (buffer-name buf))
         (if live "t" "nil") owner (if matches "t" "nil"))
        (when matches
          (cl-incf legacy-count)
          (with-current-buffer buf
            (setq-local agent-repl--owning-workspace new-ws))
          (agent-repl--log
           new-ws
           "rename-update-buffers: legacy-owner old-ws=%S new-ws=%S buffer=%S name=%S outcome=updated"
           old-ws new-ws buf (buffer-name buf)))))
    (agent-repl--log
     new-ws
     "rename-update-buffers: complete old-ws=%S new-ws=%S new-dir=%S legacy-owner-updates=%d"
     old-ws new-ws new-dir legacy-count)))

(defun agent-repl--rename-persp (old-ws new-ws)
  "Rename the persp-mode perspective from OLD-WS to NEW-WS.
No-op when persp-mode is unloaded or when OLD-WS has no live persp.
Signals `error' when the persp exists but the rename fails — a
persistent old-name persp would diverge from the renamed state."
  (let ((renamed (agent-repl--ws-rename-persp old-ws new-ws)))
    (agent-repl--log
     new-ws
     "rename-persp: old-ws=%S new-ws=%S result=%S outcome=%s"
     old-ws new-ws renamed (if renamed "renamed-or-not-present" "failed"))
    (unless renamed
      (error "persp-rename %s -> %s failed" old-ws new-ws))))

(defun agent-repl--rename-update-history (old-ws new-ws)
  "Replace OLD-WS with NEW-WS in `agent-repl--workspace-history'."
  (let ((matches (cl-count old-ws agent-repl--workspace-history
                           :test #'string=))
        (before agent-repl--workspace-history))
    (setq agent-repl--workspace-history
          (mapcar (lambda (n) (if (string= n old-ws) new-ws n))
                  agent-repl--workspace-history))
    (agent-repl--log
     new-ws
     "rename-update-history: old-ws=%S new-ws=%S matches=%d before=%S after=%S outcome=%s"
     old-ws new-ws matches before agent-repl--workspace-history
     (if (zerop matches) "unchanged" "rewritten"))))

(defun agent-repl--rename-update-projectile (old-path new-path &optional ws)
  "Move projectile's known-project entry from OLD-PATH to NEW-PATH."
  (let ((old-dir (file-name-as-directory old-path))
        (new-dir (file-name-as-directory new-path)))
    (condition-case err
        (progn
          (agent-repl--ws-unregister-project old-dir)
          (agent-repl--log
           ws
           "rename-update-projectile: ws=%S step=unregister old-path=%S old-dir=%S outcome=complete"
           ws old-path old-dir))
      (error
       (agent-repl--warn
        ws
        "rename-update-projectile: ws=%S step=unregister old-path=%S old-dir=%S outcome=ignored-error error=%S"
        ws old-path old-dir err)))
    (condition-case err
        (progn
          (agent-repl--ws-register-project new-dir)
          (agent-repl--log
           ws
           "rename-update-projectile: ws=%S step=register new-path=%S new-dir=%S outcome=complete"
           ws new-path new-dir))
      (error
       (agent-repl--warn
        ws
        "rename-update-projectile: ws=%S step=register new-path=%S new-dir=%S outcome=ignored-error error=%S"
        ws new-path new-dir err)))))

(defun agent-repl--rename-git-common-cwd (old-path &optional ws)
  "Return a stable directory to pass as `git -C' across the worktree move.
Resolves to the repo's common-dir parent (the main checkout) so the
location survives moving OLD-PATH itself.  Signals `user-error' when
the common directory cannot be resolved to an existing parent."
  (let ((common (agent-repl--git-string-quiet
                 "-C" old-path "rev-parse" "--git-common-dir")))
    (agent-repl--log
     ws "rename-git-common-cwd: ws=%S old-path=%S raw-common-dir=%S"
     ws old-path common)
    (cond
     ((or (null common) (string-empty-p common)
          (string-prefix-p "fatal" common))
      (agent-repl--log
       ws
       "rename-git-common-cwd: REJECT ws=%S old-path=%S raw-common-dir=%S reason=unresolved"
       ws old-path common)
      (user-error "Cannot resolve stable git common directory for %s"
                  old-path))
     (t
      ;; --git-common-dir may be returned as a relative path (resolved
      ;; against OLD-PATH) or absolute.  `expand-file-name' handles both.
      ;; The result is .../.git, so its parent directory is the main
      ;; checkout.
      (let* ((abs (expand-file-name common old-path))
             (parent (file-name-directory (directory-file-name abs))))
        (if (and parent (file-directory-p parent))
            (progn
              (agent-repl--log
               ws
               "rename-git-common-cwd: ACCEPT ws=%S old-path=%S raw-common-dir=%S absolute-common-dir=%S parent=%S"
               ws old-path common abs parent)
              parent)
          (agent-repl--log
           ws
           "rename-git-common-cwd: REJECT ws=%S old-path=%S raw-common-dir=%S absolute-common-dir=%S parent=%S parent-exists=%s reason=invalid-parent"
           ws old-path common abs parent
           (if (and parent (file-directory-p parent)) "t" "nil"))
          (user-error
           "Git common directory parent is not an existing directory: %s"
           parent)))))))

(defun agent-repl--do-rename-workspace (old-ws new-name)
  "Rename workspace OLD-WS to NEW-NAME.
Renames the git branch (preserving any directory prefix when NEW-NAME
is bare), the companion start tag (when configured), the worktree
directory, the projectile entry, the perspective, input/webview
buffers, and any peer workspace's `:source-ws-dir' back-reference.
  Signals `user-error' on validation failures and surfaces git-level
errors verbatim after attempting a best-effort rollback."
  (let* ((requested-old-ws old-ws)
         (old-ws (agent-repl--bare-workspace-name old-ws))
         (new-bare (agent-repl--bare-workspace-name new-name))
         (_normalized-log
          (agent-repl--log
           old-ws
           "rename: normalize requested-old-ws=%S old-ws=%S requested-new-name=%S new-bare=%S"
           requested-old-ws old-ws new-name new-bare))
         (old-path (agent-repl--ws-dir old-ws))
         (canonical-old (agent-repl--path-canonical old-path))
         (parent (file-name-directory (directory-file-name canonical-old)))
         (new-path (agent-repl--path-canonical
                    (expand-file-name new-bare parent)))
         (old-branch
          (agent-repl--rename-resolve-current-branch old-path old-ws))
         (_ (unless old-branch
              (agent-repl--log
               old-ws
               "rename: REJECT old-ws=%S new-bare=%S old-path=%S new-path=%S reason=detached-head"
               old-ws new-bare old-path new-path)
              (user-error "Cannot rename a detached-HEAD worktree")))
         (new-branch
          (agent-repl--rename-derive-branch old-branch new-name old-ws))
         (git-cwd
          (agent-repl--rename-git-common-cwd old-path old-ws)))
    (agent-repl--log
     old-ws
     "rename: prepared old-ws=%S new-bare=%S old-path=%S canonical-old=%S parent=%S new-path=%S old-branch=%S new-branch=%S git-cwd=%S"
     old-ws new-bare old-path canonical-old parent new-path
     old-branch new-branch git-cwd)
    (agent-repl--rename-validate
     old-ws new-bare new-branch new-path old-path)
    (agent-repl--rename-assert-no-pending-merge old-path old-ws)
    (agent-repl--rename-execute-git
     old-path new-path git-cwd old-branch new-branch old-ws)
    (agent-repl--rename-rehash-state old-ws new-bare new-path)
    (agent-repl--rename-update-source-back-refs old-path new-path new-bare)
    (agent-repl--rename-update-buffers old-ws new-bare new-path)
    (agent-repl--rename-update-history old-ws new-bare)
    (agent-repl--rename-update-projectile old-path new-path new-bare)
    (agent-repl--rename-persp old-ws new-bare)
    (if (fboundp 'agent-repl--state-save)
        (condition-case err
            (progn
              (agent-repl--state-save new-bare)
              (agent-repl--log
               new-bare
               "rename: state-save old-ws=%S new-ws=%S new-path=%S outcome=saved"
               old-ws new-bare new-path))
          (error
           (agent-repl--warn
            new-bare
            "rename: state-save old-ws=%S new-ws=%S new-path=%S outcome=ignored-error error=%S"
            old-ws new-bare new-path err)))
      (agent-repl--warn
       new-bare
       "rename: state-save old-ws=%S new-ws=%S new-path=%S action=skip reason=function-unavailable"
       old-ws new-bare new-path))
    (agent-repl--log
     new-bare
     "rename: complete old-ws=%S new-ws=%S old-path=%S new-path=%S old-branch=%S new-branch=%S git-cwd=%S"
     old-ws new-bare old-path new-path old-branch new-branch git-cwd)
    (message "Renamed workspace '%s' -> '%s'." old-ws new-bare)))

(defun agent-repl-rename-workspace (new-name)
  "Rename the current agent-repl workspace to NEW-NAME.
Interactive prompt suggests the current branch as the default so users
can lightly edit the existing name rather than retype it.
NEW-NAME may include a directory prefix (e.g. \"DWC/foo\") to set the
branch ref explicitly; a bare name preserves the current branch's prefix."
  (interactive
   (let* ((ws (agent-repl--ws-current-name))
          (path (agent-repl--ws-dir ws))
          (default (agent-repl--rename-resolve-current-branch path ws)))
     (unless default
       (agent-repl--log
        ws
        "rename-interactive: REJECT ws=%S path=%S reason=detached-head"
        ws path)
       (user-error "Cannot rename a detached-HEAD worktree"))
     (agent-repl--log
      ws
      "rename-interactive: prompt ws=%S path=%S default-branch=%S"
      ws path default)
     (list (read-string (format "Rename '%s' to: " ws) default))))
  (let ((ws (agent-repl--ws-current-name)))
    (agent-repl--log
     ws "rename-command: ws=%S requested-new-name=%S" ws new-name)
    (agent-repl--do-rename-workspace ws new-name)))

(provide 'agent-repl-rename)

;;; rename.el ends here
