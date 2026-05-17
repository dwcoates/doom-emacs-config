;;; test-rename.el --- ERT tests for rename.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for workspace rename: derive-branch heuristics, validation,
;; pending-merge guard, hash rehash, source-ws-dir back-ref update,
;; buffer rename, history rewrite, and end-to-end git+state rename.
;;
;; Per AGENTS.md "No External Processes or External State in Tests",
;; every test in this file is pure elisp: the `claude-repl--git-*'
;; wrappers (`claude-repl--git-string', `claude-repl--git-string-quiet',
;; `claude-repl--git-exit-code', `claude-repl--git-branch-exists-p',
;; `claude-repl--git-tag-exists-p') are stubbed via `cl-letf' with
;; fixture return values that drive the production rename logic.
;;
;; Run with:
;;   emacs -batch -Q -l ert -l test-rename.el -f ert-run-tests-batch-and-exit

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Tests: derive-branch ----

(ert-deftest claude-repl-test-rename-derive-branch-bare-preserves-prefix ()
  "A bare new-name preserves the old branch's directory prefix."
  (should (equal (claude-repl--rename-derive-branch "DWC/foo" "bar")
                 "DWC/bar")))

(ert-deftest claude-repl-test-rename-derive-branch-slash-overrides ()
  "A new-name with a slash is used verbatim."
  (should (equal (claude-repl--rename-derive-branch "DWC/foo" "OTHER/bar")
                 "OTHER/bar")))

(ert-deftest claude-repl-test-rename-derive-branch-no-prefix-old ()
  "Old branch without a prefix + bare new-name yields just the bare name."
  (should (equal (claude-repl--rename-derive-branch "foo" "bar")
                 "bar")))

;;;; ---- Tests: validate ----

(ert-deftest claude-repl-test-rename-validate-empty-name-errors ()
  "Empty new-bare signals user-error."
  (should-error
   (claude-repl--rename-validate "foo" "" "DWC/bar" nil "/tmp/x" "/tmp/foo")
   :type 'user-error))

(ert-deftest claude-repl-test-rename-validate-same-name-errors ()
  "Identical old and new names signal user-error."
  (should-error
   (claude-repl--rename-validate "foo" "foo" "DWC/foo" nil "/tmp/foo2" "/tmp/foo")
   :type 'user-error))

(ert-deftest claude-repl-test-rename-validate-existing-path-errors ()
  "Existing target path signals user-error."
  (cl-letf (((symbol-function 'file-exists-p)
             (lambda (p) (equal p "/tmp/target")))
            ((symbol-function 'claude-repl--git-branch-exists-p)
             (lambda (&rest _) nil))
            ((symbol-function '+workspace-list-names)
             (lambda () '("other"))))
    (should-error
     (claude-repl--rename-validate "foo" "bar" "DWC/bar" nil "/tmp/target" "/tmp/foo")
     :type 'user-error)))

(ert-deftest claude-repl-test-rename-validate-existing-branch-errors ()
  "Existing target branch signals user-error."
  (cl-letf (((symbol-function 'claude-repl--git-branch-exists-p)
             (lambda (_root branch) (string= branch "DWC/bar")))
            ((symbol-function '+workspace-list-names)
             (lambda () '("other"))))
    (should-error
     (claude-repl--rename-validate
      "foo" "bar" "DWC/bar" nil "/nonexistent/path" "/tmp/foo")
     :type 'user-error)))

(ert-deftest claude-repl-test-rename-validate-existing-start-tag-errors ()
  "Existing target start tag signals user-error."
  (cl-letf (((symbol-function 'claude-repl--git-branch-exists-p)
             (lambda (&rest _) nil))
            ((symbol-function 'claude-repl--git-tag-exists-p)
             (lambda (_root tag) (string= tag "start/DWC/bar")))
            ((symbol-function '+workspace-list-names)
             (lambda () '("other"))))
    (should-error
     (claude-repl--rename-validate
      "foo" "bar" "DWC/bar" "start/DWC/bar" "/nonexistent/path" "/tmp/foo")
     :type 'user-error)))

(ert-deftest claude-repl-test-rename-validate-existing-workspace-errors ()
  "Existing workspace name signals user-error."
  (cl-letf (((symbol-function 'claude-repl--git-branch-exists-p)
             (lambda (&rest _) nil))
            ((symbol-function '+workspace-list-names)
             (lambda () '("bar"))))
    (should-error
     (claude-repl--rename-validate
      "foo" "bar" "DWC/bar" nil "/nonexistent/path" "/tmp/foo")
     :type 'user-error)))

(ert-deftest claude-repl-test-rename-validate-happy-path ()
  "All checks passing returns nil without error."
  (cl-letf (((symbol-function 'claude-repl--git-branch-exists-p)
             (lambda (&rest _) nil))
            ((symbol-function 'claude-repl--git-tag-exists-p)
             (lambda (&rest _) nil))
            ((symbol-function '+workspace-list-names)
             (lambda () '("other"))))
    (should (null (claude-repl--rename-validate
                   "foo" "bar" "DWC/bar" "start/DWC/bar"
                   "/nonexistent/path" "/tmp/foo")))))

;;;; ---- Tests: assert-no-pending-merge ----

(ert-deftest claude-repl-test-rename-assert-no-pending-clean ()
  "No merge markers present → no-op."
  (cl-letf (((symbol-function 'claude-repl--git-string)
             (lambda (&rest args)
               (pcase args
                 (`("-C" "/tmp/repo" "rev-parse" "--absolute-git-dir")
                  "/tmp/repo/.git")
                 (_ (error "unmocked git-string: %S" args)))))
            ((symbol-function 'file-exists-p) (lambda (_p) nil)))
    (should (null (claude-repl--rename-assert-no-pending-merge "/tmp/repo")))))

(ert-deftest claude-repl-test-rename-assert-no-pending-cherry-pick-errors ()
  "CHERRY_PICK_HEAD present → user-error."
  (cl-letf (((symbol-function 'claude-repl--git-string)
             (lambda (&rest args)
               (pcase args
                 (`("-C" "/tmp/repo" "rev-parse" "--absolute-git-dir")
                  "/tmp/repo/.git")
                 (_ (error "unmocked git-string: %S" args)))))
            ((symbol-function 'file-exists-p)
             (lambda (p) (equal p "/tmp/repo/.git/CHERRY_PICK_HEAD"))))
    (should-error (claude-repl--rename-assert-no-pending-merge "/tmp/repo")
                  :type 'user-error)))

(ert-deftest claude-repl-test-rename-assert-no-pending-merge-head-errors ()
  "MERGE_HEAD present → user-error."
  (cl-letf (((symbol-function 'claude-repl--git-string)
             (lambda (&rest args)
               (pcase args
                 (`("-C" "/tmp/repo" "rev-parse" "--absolute-git-dir")
                  "/tmp/repo/.git")
                 (_ (error "unmocked git-string: %S" args)))))
            ((symbol-function 'file-exists-p)
             (lambda (p) (equal p "/tmp/repo/.git/MERGE_HEAD"))))
    (should-error (claude-repl--rename-assert-no-pending-merge "/tmp/repo")
                  :type 'user-error)))

;;;; ---- Tests: rehash-state ----

(ert-deftest claude-repl-test-rename-rehash-state-moves-plist ()
  "Plist is copied to new key with old key removed."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "foo" :project-dir "/old/path")
    (claude-repl--ws-put "foo" :priority "high")
    (claude-repl--rename-rehash-state "foo" "bar" "/new/path")
    (should (null (gethash "foo" claude-repl--workspaces)))
    (should (equal (claude-repl--ws-get "bar" :priority) "high"))))

(ert-deftest claude-repl-test-rename-rehash-state-updates-project-dir ()
  "Project-dir is rewritten to the canonical new path."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'claude-repl--path-canonical)
               (lambda (p) (concat "CANON:" p))))
      (claude-repl--ws-put "foo" :project-dir "/old/path")
      (claude-repl--rename-rehash-state "foo" "bar" "/new/path")
      (should (equal (claude-repl--ws-get "bar" :project-dir)
                     "CANON:/new/path")))))

(ert-deftest claude-repl-test-rename-rehash-state-clears-ws-id ()
  "Cached :ws-id is cleared so it gets recomputed from the new dir."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "foo" :project-dir "/old/path")
    (claude-repl--ws-put "foo" :ws-id "stale-id")
    (claude-repl--rename-rehash-state "foo" "bar" "/new/path")
    (should (null (claude-repl--ws-get "bar" :ws-id)))))

;;;; ---- Tests: source-back-refs ----

(ert-deftest claude-repl-test-rename-source-back-refs-rewrites-match ()
  "Peer workspaces pointing at the old path are updated to the new path.
Comparison is canonical (truename), so we expect the canonicalized
new path back even when the input is the raw path."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'claude-repl--path-canonical)
               (lambda (p) (concat "CANON:" p))))
      (claude-repl--ws-put "child" :source-ws-dir "/tmp/old")
      (claude-repl--rename-update-source-back-refs "/tmp/old" "/tmp/new")
      (should (equal (claude-repl--ws-get "child" :source-ws-dir)
                     "CANON:/tmp/new")))))

(ert-deftest claude-repl-test-rename-source-back-refs-leaves-unrelated ()
  "Peer workspaces pointing elsewhere are untouched."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'claude-repl--path-canonical)
               (lambda (p) (concat "CANON:" p))))
      (claude-repl--ws-put "child" :source-ws-dir "/tmp/other")
      (claude-repl--rename-update-source-back-refs "/tmp/old" "/tmp/new")
      (should (equal (claude-repl--ws-get "child" :source-ws-dir)
                     "/tmp/other")))))

(ert-deftest claude-repl-test-rename-source-back-refs-clears-source-ws-name-cache ()
  "Peers whose `:source-ws-dir' is rewritten must have their
`:source-ws-name' cache (populated by the drawer's fast-path)
cleared.  The renamed workspace is rehashed under a new name elsewhere
in the rename flow, so any cached name pointing at the old identity
is stale and must be re-resolved on next read."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'claude-repl--path-canonical)
               (lambda (p) (concat "CANON:" p))))
      (claude-repl--ws-put "child" :source-ws-dir "/tmp/old")
      (claude-repl--ws-put "child" :source-ws-name "old-name")
      (claude-repl--rename-update-source-back-refs "/tmp/old" "/tmp/new")
      (should-not (claude-repl--ws-get "child" :source-ws-name)))))

(ert-deftest claude-repl-test-rename-source-back-refs-leaves-unrelated-cache ()
  "Peers whose `:source-ws-dir' was not rewritten keep their
`:source-ws-name' cache — the sweep targets only the affected peers."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'claude-repl--path-canonical)
               (lambda (p) (concat "CANON:" p))))
      (claude-repl--ws-put "child" :source-ws-dir "/tmp/other")
      (claude-repl--ws-put "child" :source-ws-name "other-name")
      (claude-repl--rename-update-source-back-refs "/tmp/old" "/tmp/new")
      (should (equal (claude-repl--ws-get "child" :source-ws-name)
                     "other-name")))))

;;;; ---- Tests: update-buffers ----

(ert-deftest claude-repl-test-rename-update-buffers-renames-vterm-and-input ()
  "Vterm and input buffers are renamed to the new ws's panel names."
  (claude-repl-test--with-clean-state
    (let ((vbuf (get-buffer-create "*claude-panel-old*"))
          (ibuf (get-buffer-create "*claude-panel-input-old*")))
      (unwind-protect
          (progn
            (puthash "new" (list :vterm-buffer vbuf
                                 :input-buffer ibuf
                                 :project-dir "/tmp/new")
                     claude-repl--workspaces)
            (claude-repl--rename-update-buffers "old" "new" "/tmp/new")
            (should (string= (buffer-name vbuf) "*claude-panel-new*"))
            (should (string= (buffer-name ibuf) "*claude-panel-input-new*")))
        (when (buffer-live-p vbuf) (kill-buffer vbuf))
        (when (buffer-live-p ibuf) (kill-buffer ibuf))))))

(ert-deftest claude-repl-test-rename-update-buffers-rewrites-owning-workspace ()
  "Buffer-local `claude-repl--owning-workspace' is repointed to the new ws."
  (claude-repl-test--with-clean-state
    (let ((vbuf (get-buffer-create "*claude-panel-old*")))
      (unwind-protect
          (progn
            (with-current-buffer vbuf
              (setq-local claude-repl--owning-workspace "old"))
            (puthash "new" (list :vterm-buffer vbuf
                                 :project-dir "/tmp/new")
                     claude-repl--workspaces)
            (claude-repl--rename-update-buffers "old" "new" "/tmp/new")
            (should (equal (buffer-local-value 'claude-repl--owning-workspace vbuf)
                           "new")))
        (when (buffer-live-p vbuf) (kill-buffer vbuf))))))

(ert-deftest claude-repl-test-rename-update-buffers-updates-default-directory ()
  "Vterm buffer's `default-directory' is repointed to the new path."
  (claude-repl-test--with-clean-state
    (let ((vbuf (get-buffer-create "*claude-panel-old*")))
      (unwind-protect
          (progn
            (puthash "new" (list :vterm-buffer vbuf
                                 :project-dir "/tmp/newdir")
                     claude-repl--workspaces)
            (claude-repl--rename-update-buffers "old" "new" "/tmp/newdir")
            (should (equal (buffer-local-value 'default-directory vbuf)
                           (file-name-as-directory "/tmp/newdir"))))
        (when (buffer-live-p vbuf) (kill-buffer vbuf))))))

;;;; ---- Tests: update-history ----

(ert-deftest claude-repl-test-rename-update-history-replaces-old-name ()
  "Old name in `+dwc/workspace-history' is replaced with the new name."
  (let ((+dwc/workspace-history '("a" "old" "b")))
    (claude-repl--rename-update-history "old" "new")
    (should (equal +dwc/workspace-history '("a" "new" "b")))))

(ert-deftest claude-repl-test-rename-update-history-no-old-entry-noop ()
  "History without the old name passes through unchanged."
  (let ((+dwc/workspace-history '("a" "b")))
    (claude-repl--rename-update-history "old" "new")
    (should (equal +dwc/workspace-history '("a" "b")))))

;;;; ---- Tests: end-to-end through claude-repl--do-rename-workspace ----
;;
;; The end-to-end tests mock every `claude-repl--git-*' wrapper to
;; supply fixture exit codes / strings that drive the rename pipeline.
;; They assert on (a) the post-rename in-memory state and (b) the
;; sequence of git-wrapper calls — NOT on real filesystem side
;; effects, since no real git is invoked.

(ert-deftest claude-repl-test-rename-end-to-end-renames-branch-and-dir ()
  "Full rename pipeline rehashes state, renames branch, renames start
tag, and moves the worktree — verified by tracking the git-wrapper
call sequence and the resulting `claude-repl--workspaces' state."
  (claude-repl-test--with-clean-state
    (let ((claude-repl-worktree-start-tag-prefix "start/")
          (exit-calls nil))
      (claude-repl--ws-put "foo" :project-dir "/tmp/parent/foo")
      (claude-repl--ws-put "foo" :worktree-p t)
      (cl-letf (((symbol-function 'claude-repl--path-canonical)
                 ;; Identity canonicalization keeps assertions readable.
                 (lambda (p) (directory-file-name p)))
                ((symbol-function 'claude-repl--git-string)
                 (lambda (&rest args)
                   (pcase args
                     (`("-C" "/tmp/parent/foo" "rev-parse" "--abbrev-ref" "HEAD")
                      "DWC/foo")
                     (`("-C" "/tmp/parent/foo" "rev-parse" "--absolute-git-dir")
                      "/tmp/parent/foo/.git")
                     (_ (error "unmocked git-string: %S" args)))))
                ((symbol-function 'claude-repl--git-string-quiet)
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
                ;; existing directory for `claude-repl--rename-git-common-cwd'
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
                ((symbol-function 'claude-repl--git-branch-exists-p)
                 ;; Old branch yes, new branch no — both via the same
                 ;; predicate the production code calls during validation.
                 (lambda (_root branch)
                   (pcase branch
                     ("DWC/foo" t)
                     ("DWC/bar" nil)
                     (_ (error "unmocked branch-exists-p: %S" branch)))))
                ((symbol-function 'claude-repl--git-tag-exists-p)
                 ;; Old tag exists (so the rename flow attempts to
                 ;; rename it); new tag does not.
                 (lambda (_root tag)
                   (pcase tag
                     ("start/DWC/foo" t)
                     ("start/DWC/bar" nil)
                     (_ (error "unmocked tag-exists-p: %S" tag)))))
                ((symbol-function 'claude-repl--git-exit-code)
                 (lambda (root &rest args)
                   (push (cons root args) exit-calls)
                   0))
                ((symbol-function '+workspace-list-names)
                 (lambda () '("foo")))
                ((symbol-function 'persp-get-by-name) (lambda (_) nil))
                ((symbol-function 'persp-rename) (lambda (&rest _) t))
                ((symbol-function 'claude-repl--state-save) (lambda (_ws) nil)))
        (claude-repl--do-rename-workspace "foo" "bar"))
      ;; State is rehashed under the new name and points at the new path.
      (should (null (gethash "foo" claude-repl--workspaces)))
      (should (equal (claude-repl--ws-get "bar" :project-dir)
                     "/tmp/parent/bar"))
      ;; The git pipeline invoked (in order): branch rename, tag create,
      ;; old tag delete, worktree move.  We assert on the recorded
      ;; argument sequence rather than filesystem state.
      (let ((calls (nreverse exit-calls)))
        (should (equal (nth 0 calls)
                       '("/tmp/repo/" "branch" "-m" "DWC/foo" "DWC/bar")))
        (should (equal (nth 1 calls)
                       '("/tmp/repo/" "tag" "start/DWC/bar" "start/DWC/foo")))
        (should (equal (nth 2 calls)
                       '("/tmp/repo/" "tag" "-d" "start/DWC/foo")))
        (should (equal (nth 3 calls)
                       '("/tmp/repo/" "worktree" "move"
                         "/tmp/parent/foo" "/tmp/parent/bar")))))))

(ert-deftest claude-repl-test-rename-end-to-end-rejects-existing-branch ()
  "Rename to a name whose branch already exists aborts before touching state."
  (claude-repl-test--with-clean-state
    (let ((claude-repl-worktree-start-tag-prefix nil)
          (exit-calls nil))
      (claude-repl--ws-put "foo" :project-dir "/tmp/parent/foo")
      (cl-letf (((symbol-function 'claude-repl--path-canonical)
                 (lambda (p) (directory-file-name p)))
                ((symbol-function 'claude-repl--git-string)
                 (lambda (&rest args)
                   (pcase args
                     (`("-C" "/tmp/parent/foo" "rev-parse" "--abbrev-ref" "HEAD")
                      "DWC/foo")
                     (_ (error "unmocked git-string: %S" args)))))
                ((symbol-function 'file-directory-p)
                 (lambda (_p) nil))
                ((symbol-function 'file-exists-p)
                 (lambda (_p) nil))
                ;; Target branch already exists → validation rejects
                ;; before any rename op runs.
                ((symbol-function 'claude-repl--git-branch-exists-p)
                 (lambda (_root branch) (string= branch "DWC/bar")))
                ((symbol-function 'claude-repl--git-tag-exists-p)
                 (lambda (&rest _) nil))
                ((symbol-function 'claude-repl--git-exit-code)
                 (lambda (root &rest args)
                   (push (cons root args) exit-calls)
                   0))
                ((symbol-function '+workspace-list-names)
                 (lambda () '("foo")))
                ((symbol-function 'persp-get-by-name) (lambda (_) nil))
                ((symbol-function 'claude-repl--state-save) (lambda (_ws) nil)))
        (should-error (claude-repl--do-rename-workspace "foo" "bar")
                      :type 'user-error))
      ;; State is unchanged: the old workspace entry survives the
      ;; aborted rename, and no mutating git command ran.
      (should (gethash "foo" claude-repl--workspaces))
      (should (null (gethash "bar" claude-repl--workspaces)))
      (should (null exit-calls)))))

(provide 'test-rename)

;;; test-rename.el ends here
