;;; test-rename.el --- ERT tests for rename.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for workspace rename: derive-branch heuristics, validation,
;; pending-merge guard, hash rehash, source-ws-dir back-ref update,
;; buffer rename, history rewrite, and end-to-end git+state rename.
;;
;; Per AGENTS.md "No External Processes or External State in Tests",
;; every test in this file is pure elisp: the `agent-repl--git-*'
;; wrappers (`agent-repl--git-string', `agent-repl--git-string-quiet',
;; `agent-repl--git-exit-code', `agent-repl--git-branch-exists-p')
;; are stubbed via `cl-letf' with
;; fixture return values that drive the production rename logic.
;;
;; Run with:
;;   emacs -batch -Q -l ert -l test-rename.el -f ert-run-tests-batch-and-exit

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Tests: derive-branch ----

(ert-deftest agent-repl-test-rename-derive-branch-bare-preserves-prefix ()
  "A bare new-name preserves the old branch's directory prefix."
  (should (equal (agent-repl--rename-derive-branch "DWC/foo" "bar")
                 "DWC/bar")))

(ert-deftest agent-repl-test-rename-derive-branch-slash-overrides ()
  "A new-name with a slash is used verbatim."
  (should (equal (agent-repl--rename-derive-branch "DWC/foo" "OTHER/bar")
                 "OTHER/bar")))

(ert-deftest agent-repl-test-rename-derive-branch-no-prefix-old ()
  "Old branch without a prefix + bare new-name yields just the bare name."
  (should (equal (agent-repl--rename-derive-branch "foo" "bar")
                 "bar")))

;;;; ---- Tests: validate ----

(ert-deftest agent-repl-test-rename-validate-empty-name-errors ()
  "Empty new-bare signals user-error."
  (should-error
   (agent-repl--rename-validate "foo" "" "DWC/bar" "/tmp/x" "/tmp/foo")
   :type 'user-error))

(ert-deftest agent-repl-test-rename-validate-same-name-errors ()
  "Identical old and new names signal user-error."
  (should-error
   (agent-repl--rename-validate "foo" "foo" "DWC/foo" "/tmp/foo2" "/tmp/foo")
   :type 'user-error))

(ert-deftest agent-repl-test-rename-validate-existing-path-errors ()
  "Existing target path signals user-error."
  (cl-letf (((symbol-function 'file-exists-p)
             (lambda (p) (equal p "/tmp/target")))
            ((symbol-function 'agent-repl--git-branch-exists-p)
             (lambda (&rest _) nil))
            ((symbol-function '+workspace-list-names)
             (lambda () '("other"))))
    (should-error
     (agent-repl--rename-validate "foo" "bar" "DWC/bar" "/tmp/target" "/tmp/foo")
     :type 'user-error)))

(ert-deftest agent-repl-test-rename-validate-existing-branch-errors ()
  "Existing target branch signals user-error."
  (cl-letf (((symbol-function 'agent-repl--git-branch-exists-p)
             (lambda (_root branch) (string= branch "DWC/bar")))
            ((symbol-function '+workspace-list-names)
             (lambda () '("other"))))
    (should-error
     (agent-repl--rename-validate
      "foo" "bar" "DWC/bar" "/nonexistent/path" "/tmp/foo")
     :type 'user-error)))

(ert-deftest agent-repl-test-rename-validate-existing-workspace-errors ()
  "Existing workspace name signals user-error."
  (cl-letf (((symbol-function 'agent-repl--git-branch-exists-p)
             (lambda (&rest _) nil))
            ((symbol-function '+workspace-list-names)
             (lambda () '("bar"))))
    (should-error
     (agent-repl--rename-validate
      "foo" "bar" "DWC/bar" "/nonexistent/path" "/tmp/foo")
     :type 'user-error)))

(ert-deftest agent-repl-test-rename-validate-happy-path ()
  "All checks passing returns nil without error."
  (cl-letf (((symbol-function 'agent-repl--git-branch-exists-p)
             (lambda (&rest _) nil))
            ((symbol-function '+workspace-list-names)
             (lambda () '("other"))))
    (should (null (agent-repl--rename-validate
                   "foo" "bar" "DWC/bar"
                   "/nonexistent/path" "/tmp/foo")))))

;;;; ---- Tests: assert-no-pending-merge ----

(ert-deftest agent-repl-test-rename-assert-no-pending-clean ()
  "No merge markers present → no-op."
  (cl-letf (((symbol-function 'agent-repl--git-string)
             (lambda (&rest args)
               (pcase args
                 (`("-C" "/tmp/repo" "rev-parse" "--absolute-git-dir")
                  "/tmp/repo/.git")
                 (_ (error "unmocked git-string: %S" args)))))
            ((symbol-function 'file-exists-p) (lambda (_p) nil)))
    (should (null (agent-repl--rename-assert-no-pending-merge "/tmp/repo")))))

(ert-deftest agent-repl-test-rename-assert-no-pending-cherry-pick-errors ()
  "CHERRY_PICK_HEAD present → user-error."
  (cl-letf (((symbol-function 'agent-repl--git-string)
             (lambda (&rest args)
               (pcase args
                 (`("-C" "/tmp/repo" "rev-parse" "--absolute-git-dir")
                  "/tmp/repo/.git")
                 (_ (error "unmocked git-string: %S" args)))))
            ((symbol-function 'file-exists-p)
             (lambda (p) (equal p "/tmp/repo/.git/CHERRY_PICK_HEAD"))))
    (should-error (agent-repl--rename-assert-no-pending-merge "/tmp/repo")
                  :type 'user-error)))

(ert-deftest agent-repl-test-rename-assert-no-pending-merge-head-errors ()
  "MERGE_HEAD present → user-error."
  (cl-letf (((symbol-function 'agent-repl--git-string)
             (lambda (&rest args)
               (pcase args
                 (`("-C" "/tmp/repo" "rev-parse" "--absolute-git-dir")
                  "/tmp/repo/.git")
                 (_ (error "unmocked git-string: %S" args)))))
            ((symbol-function 'file-exists-p)
             (lambda (p) (equal p "/tmp/repo/.git/MERGE_HEAD"))))
    (should-error (agent-repl--rename-assert-no-pending-merge "/tmp/repo")
                  :type 'user-error)))

;;;; ---- Tests: rehash-state ----

(ert-deftest agent-repl-test-rename-rehash-state-moves-plist ()
  "Plist is copied to new key with old key removed."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "foo" :project-dir "/old/path")
    (agent-repl--ws-put "foo" :priority "high")
    (agent-repl--rename-rehash-state "foo" "bar" "/new/path")
    (should (null (gethash "foo" agent-repl--workspaces)))
    (should (equal (agent-repl--ws-get "bar" :priority) "high"))))

(ert-deftest agent-repl-test-rename-rehash-state-updates-project-dir ()
  "Project-dir is rewritten to the canonical new path."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--path-canonical)
               (lambda (p) (concat "CANON:" p))))
      (agent-repl--ws-put "foo" :project-dir "/old/path")
      (agent-repl--rename-rehash-state "foo" "bar" "/new/path")
      (should (equal (agent-repl--ws-get "bar" :project-dir)
                     "CANON:/new/path")))))

(ert-deftest agent-repl-test-rename-rehash-state-clears-ws-id ()
  "Cached :ws-id is cleared so it gets recomputed from the new dir."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "foo" :project-dir "/old/path")
    (agent-repl--ws-put "foo" :ws-id "stale-id")
    (agent-repl--rename-rehash-state "foo" "bar" "/new/path")
    (should (null (agent-repl--ws-get "bar" :ws-id)))))

;;;; ---- Tests: source-back-refs ----

(ert-deftest agent-repl-test-rename-source-back-refs-rewrites-match ()
  "Peer workspaces pointing at the old path are updated to the new path.
Comparison is canonical (truename), so we expect the canonicalized
new path back even when the input is the raw path."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--path-canonical)
               (lambda (p) (concat "CANON:" p))))
      (agent-repl--ws-put "child" :source-ws-dir "/tmp/old")
      (agent-repl--rename-update-source-back-refs "/tmp/old" "/tmp/new")
      (should (equal (agent-repl--ws-get "child" :source-ws-dir)
                     "CANON:/tmp/new")))))

(ert-deftest agent-repl-test-rename-source-back-refs-leaves-unrelated ()
  "Peer workspaces pointing elsewhere are untouched."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--path-canonical)
               (lambda (p) (concat "CANON:" p))))
      (agent-repl--ws-put "child" :source-ws-dir "/tmp/other")
      (agent-repl--rename-update-source-back-refs "/tmp/old" "/tmp/new")
      (should (equal (agent-repl--ws-get "child" :source-ws-dir)
                     "/tmp/other")))))

(ert-deftest agent-repl-test-rename-source-back-refs-clears-source-ws-name-cache ()
  "Peers whose `:source-ws-dir' is rewritten must have their
`:source-ws-name' cache cleared.  The renamed workspace is rehashed
under a new name elsewhere in the rename flow, so any cached name
pointing at the old identity is stale and must be re-resolved on next
read."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--path-canonical)
               (lambda (p) (concat "CANON:" p))))
      (agent-repl--ws-put "child" :source-ws-dir "/tmp/old")
      (agent-repl--ws-put "child" :source-ws-name "old-name")
      (agent-repl--rename-update-source-back-refs "/tmp/old" "/tmp/new")
      (should-not (agent-repl--ws-get "child" :source-ws-name)))))

(ert-deftest agent-repl-test-rename-source-back-refs-leaves-unrelated-cache ()
  "Peers whose `:source-ws-dir' was not rewritten keep their
`:source-ws-name' cache — the sweep targets only the affected peers."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--path-canonical)
               (lambda (p) (concat "CANON:" p))))
      (agent-repl--ws-put "child" :source-ws-dir "/tmp/other")
      (agent-repl--ws-put "child" :source-ws-name "other-name")
      (agent-repl--rename-update-source-back-refs "/tmp/old" "/tmp/new")
      (should (equal (agent-repl--ws-get "child" :source-ws-name)
                     "other-name")))))

;;;; ---- Tests: update-buffers ----

(ert-deftest agent-repl-test-rename-update-buffers-renames-webview-and-input ()
  "Webview and input buffers are renamed to the new ws's panel names.
The webview buffer lives in its own `*agent-frontend-WS*' namespace
\(named via `agent-repl--frontend-webview-buffer-name', not
`agent-repl--buffer-name') — distinct from the input composer's
`*agent-panel-input-WS*' scheme.  Renaming the webview is new behavior
per rename.el's commentary: it used to rename only the vterm/input
pair, so a renamed workspace's webview kept its old name."
  (agent-repl-test--with-clean-state
    (let ((wbuf (get-buffer-create "*agent-frontend-old*"))
          (ibuf (get-buffer-create "*agent-panel-input-old*")))
      (unwind-protect
          (progn
            (puthash "new" (list :frontend-buffer wbuf
                                 :input-buffer ibuf
                                 :project-dir "/tmp/new")
                     agent-repl--workspaces)
            (agent-repl--rename-update-buffers "old" "new" "/tmp/new")
            (should (string= (buffer-name wbuf) "*agent-frontend-new*"))
            (should (string= (buffer-name ibuf) "*agent-panel-input-new*")))
        (when (buffer-live-p wbuf) (kill-buffer wbuf))
        (when (buffer-live-p ibuf) (kill-buffer ibuf))))))

(ert-deftest agent-repl-test-rename-update-buffers-rewrites-owning-workspace ()
  "Buffer-local `agent-repl--owning-workspace' is repointed to the new ws."
  (agent-repl-test--with-clean-state
    (let ((wbuf (get-buffer-create "*agent-frontend-old*")))
      (unwind-protect
          (progn
            (with-current-buffer wbuf
              (setq-local agent-repl--owning-workspace "old"))
            (puthash "new" (list :frontend-buffer wbuf
                                 :project-dir "/tmp/new")
                     agent-repl--workspaces)
            (agent-repl--rename-update-buffers "old" "new" "/tmp/new")
            (should (equal (buffer-local-value 'agent-repl--owning-workspace wbuf)
                           "new")))
        (when (buffer-live-p wbuf) (kill-buffer wbuf))))))

(ert-deftest agent-repl-test-rename-update-buffers-updates-default-directory ()
  "Webview buffer's `default-directory' is repointed to the new path."
  (agent-repl-test--with-clean-state
    (let ((wbuf (get-buffer-create "*agent-frontend-old*")))
      (unwind-protect
          (progn
            (puthash "new" (list :frontend-buffer wbuf
                                 :project-dir "/tmp/newdir")
                     agent-repl--workspaces)
            (agent-repl--rename-update-buffers "old" "new" "/tmp/newdir")
            (should (equal (buffer-local-value 'default-directory wbuf)
                           (file-name-as-directory "/tmp/newdir"))))
        (when (buffer-live-p wbuf) (kill-buffer wbuf))))))

;;;; ---- Tests: update-history ----

(ert-deftest agent-repl-test-rename-update-history-replaces-old-name ()
  "Old name in `agent-repl--workspace-history' is replaced with the new name."
  (let ((agent-repl--workspace-history '("a" "old" "b")))
    (agent-repl--rename-update-history "old" "new")
    (should (equal agent-repl--workspace-history '("a" "new" "b")))))

(ert-deftest agent-repl-test-rename-update-history-no-old-entry-noop ()
  "History without the old name passes through unchanged."
  (let ((agent-repl--workspace-history '("a" "b")))
    (agent-repl--rename-update-history "old" "new")
    (should (equal agent-repl--workspace-history '("a" "b")))))

;;;; ---- Tests: end-to-end through agent-repl--do-rename-workspace ----
;;
;; The end-to-end tests mock every `agent-repl--git-*' wrapper to
;; supply fixture exit codes / strings that drive the rename pipeline.
;; They assert on (a) the post-rename in-memory state and (b) the
;; sequence of git-wrapper calls — NOT on real filesystem side
;; effects, since no real git is invoked.

(ert-deftest agent-repl-test-rename-end-to-end-renames-branch-and-dir ()
  "Full rename pipeline rehashes state, renames branch, and moves the
worktree — verified by tracking the git-wrapper call sequence and the
resulting `agent-repl--workspaces' state."
  (agent-repl-test--with-clean-state
    (let ((exit-calls nil))
      (agent-repl--ws-put "foo" :project-dir "/tmp/parent/foo")
      (agent-repl--ws-put "foo" :worktree-p t)
      (cl-letf (((symbol-function 'agent-repl--path-canonical)
                 ;; Identity canonicalization keeps assertions readable.
                 (lambda (p) (directory-file-name p)))
                ((symbol-function 'agent-repl--git-string)
                 (lambda (&rest args)
                   (pcase args
                     (`("-C" "/tmp/parent/foo" "rev-parse" "--abbrev-ref" "HEAD")
                      "DWC/foo")
                     (`("-C" "/tmp/parent/foo" "rev-parse" "--absolute-git-dir")
                      "/tmp/parent/foo/.git")
                     (_ (error "unmocked git-string: %S" args)))))
                ((symbol-function 'agent-repl--git-string-quiet)
                 (lambda (&rest args)
                   (pcase args
                     ;; --git-common-dir for git-cwd resolution.  Returning
                     ;; an absolute path under /tmp/repo so the parent
                     ;; (/tmp/repo) is the main checkout used as git-cwd.
                     (`("-C" "/tmp/parent/foo" "rev-parse" "--git-common-dir")
                      "/tmp/repo/.git")
                     (_ (error "unmocked git-string-quiet: %S" args)))))
                ;; The git-cwd parent (/tmp/repo/, with trailing slash
                ;; after `file-name-directory') must look like an
                ;; existing directory for `agent-repl--rename-git-common-cwd'
                ;; to accept it instead of falling back to old-path.  The
                ;; target path (/tmp/parent/bar) must NOT exist or
                ;; validation will reject.
                ((symbol-function 'file-directory-p)
                 (lambda (p)
                   (member (directory-file-name p) '("/tmp/repo"))))
                ((symbol-function 'file-exists-p)
                 (lambda (p)
                   ;; No pending-merge markers, and target dir absent.
                   (cond
                    ((string-suffix-p "/CHERRY_PICK_HEAD" p) nil)
                    ((string-suffix-p "/MERGE_HEAD" p) nil)
                    ((string-suffix-p "/REBASE_HEAD" p) nil)
                    ((string-suffix-p "/rebase-merge" p) nil)
                    ((string-suffix-p "/rebase-apply" p) nil)
                    (t nil))))
                ((symbol-function 'agent-repl--git-branch-exists-p)
                 ;; Old branch yes, new branch no — both via the same
                 ;; predicate the production code calls during validation.
                 (lambda (_root branch)
                   (pcase branch
                     ("DWC/foo" t)
                     ("DWC/bar" nil)
                     (_ (error "unmocked branch-exists-p: %S" branch)))))
                ((symbol-function 'agent-repl--git-exit-code)
                 (lambda (root &rest args)
                   (push (cons root args) exit-calls)
                   0))
                ((symbol-function '+workspace-list-names)
                 (lambda () '("foo")))
                ((symbol-function 'persp-get-by-name) (lambda (_) nil))
                ((symbol-function 'persp-rename) (lambda (&rest _) t))
                ((symbol-function 'agent-repl--state-save) (lambda (_ws) nil)))
        (agent-repl--do-rename-workspace "foo" "bar"))
      ;; State is rehashed under the new name and points at the new path.
      (should (null (gethash "foo" agent-repl--workspaces)))
      (should (equal (agent-repl--ws-get "bar" :project-dir)
                     "/tmp/parent/bar"))
      ;; The git pipeline invoked (in order): branch rename, worktree
      ;; move.  We assert on the recorded argument sequence rather than
      ;; filesystem state.
      (let ((calls (nreverse exit-calls)))
        (should (equal calls
                       '(("/tmp/repo/" "branch" "-m" "DWC/foo" "DWC/bar")
                         ("/tmp/repo/" "worktree" "move"
                          "/tmp/parent/foo" "/tmp/parent/bar"))))))))

(ert-deftest agent-repl-test-rename-end-to-end-rejects-existing-branch ()
  "Rename to a name whose branch already exists aborts before touching state."
  (agent-repl-test--with-clean-state
    (let ((exit-calls nil))
      (agent-repl--ws-put "foo" :project-dir "/tmp/parent/foo")
      (cl-letf (((symbol-function 'agent-repl--path-canonical)
                 (lambda (p) (directory-file-name p)))
                ((symbol-function 'agent-repl--git-string)
                 (lambda (&rest args)
                   (pcase args
                     (`("-C" "/tmp/parent/foo" "rev-parse" "--abbrev-ref" "HEAD")
                      "DWC/foo")
                     (_ (error "unmocked git-string: %S" args)))))
                ((symbol-function 'agent-repl--git-string-quiet)
                 (lambda (&rest args)
                   (pcase args
                     ;; --git-common-dir for git-cwd resolution.
                     (`("-C" "/tmp/parent/foo" "rev-parse" "--git-common-dir")
                      "/tmp/repo/.git")
                     (_ (error "unmocked git-string-quiet: %S" args)))))
                ((symbol-function 'file-directory-p)
                 (lambda (_p) nil))
                ((symbol-function 'file-exists-p)
                 (lambda (_p) nil))
                ;; Target branch already exists → validation rejects
                ;; before any rename op runs.
                ((symbol-function 'agent-repl--git-branch-exists-p)
                 (lambda (_root branch) (string= branch "DWC/bar")))
                ((symbol-function 'agent-repl--git-exit-code)
                 (lambda (root &rest args)
                   (push (cons root args) exit-calls)
                   0))
                ((symbol-function '+workspace-list-names)
                 (lambda () '("foo")))
                ((symbol-function 'persp-get-by-name) (lambda (_) nil))
                ((symbol-function 'agent-repl--state-save) (lambda (_ws) nil)))
        (should-error (agent-repl--do-rename-workspace "foo" "bar")
                      :type 'user-error))
      ;; State is unchanged: the old workspace entry survives the
      ;; aborted rename, and no mutating git command ran.
      (should (gethash "foo" agent-repl--workspaces))
      (should (null (gethash "bar" agent-repl--workspaces)))
      (should (null exit-calls)))))

(provide 'test-rename)

;;; test-rename.el ends here
