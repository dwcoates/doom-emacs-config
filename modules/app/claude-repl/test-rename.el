;;; test-rename.el --- ERT tests for rename.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for workspace rename: derive-branch heuristics, validation,
;; pending-merge guard, hash rehash, source-ws-dir back-ref update,
;; buffer rename, history rewrite, and end-to-end git+state rename
;; against a real git worktree.
;;
;; Run with:
;;   emacs -batch -Q -l ert -l test-rename.el -f ert-run-tests-batch-and-exit

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Local helpers ----

(defmacro claude-repl-test-rename--with-temp-git-repo (var &rest body)
  "Create a temporary git repo, bind its path to VAR, then run BODY."
  (declare (indent 1))
  `(let ((,var (make-temp-file "claude-repl-rename-test-" t)))
     (unwind-protect
         (progn
           (call-process "git" nil nil nil "-C" ,var "init" "-q" "-b" "master")
           (call-process "git" nil nil nil "-C" ,var "config" "user.email" "test@test.com")
           (call-process "git" nil nil nil "-C" ,var "config" "user.name" "Test")
           (call-process "git" nil nil nil "-C" ,var "config" "commit.gpgsign" "false")
           ,@body)
       (delete-directory ,var t))))

(defun claude-repl-test-rename--seed-commit (repo)
  "Make one initial commit in REPO so branches can exist."
  (let ((f (expand-file-name "seed" repo)))
    (write-region "seed" nil f)
    (call-process "git" nil nil nil "-C" repo "add" "seed")
    (call-process "git" nil nil nil "-C" repo "commit" "-qm" "seed")))

(defmacro claude-repl-test-rename--with-worktree (repo wt-path branch &rest body)
  "Create a worktree at WT-PATH for BRANCH off REPO, run BODY, clean up."
  (declare (indent 3))
  `(progn
     (call-process "git" nil nil nil "-C" ,repo "worktree" "add" "-b" ,branch ,wt-path)
     (unwind-protect
         (progn ,@body)
       (ignore-errors
         (call-process "git" nil nil nil "-C" ,repo "worktree" "remove" "--force" ,wt-path))
       (when (file-directory-p ,wt-path)
         (delete-directory ,wt-path t)))))

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
  (let ((dir (make-temp-file "claude-repl-rename-existing-" t)))
    (unwind-protect
        (should-error
         (cl-letf (((symbol-function 'claude-repl--git-branch-exists-p)
                    (lambda (&rest _) nil))
                   ((symbol-function '+workspace-list-names)
                    (lambda () '("other"))))
           (claude-repl--rename-validate "foo" "bar" "DWC/bar" nil dir "/tmp/foo"))
         :type 'user-error)
      (delete-directory dir t))))

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

(ert-deftest claude-repl-test-rename-validate-existing-tag-branch-errors ()
  "Existing target tag-branch signals user-error."
  (cl-letf (((symbol-function 'claude-repl--git-branch-exists-p)
             (lambda (_root branch) (string= branch "DWC/bar-tag")))
            ((symbol-function '+workspace-list-names)
             (lambda () '("other"))))
    (should-error
     (claude-repl--rename-validate
      "foo" "bar" "DWC/bar" "DWC/bar-tag" "/nonexistent/path" "/tmp/foo")
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
            ((symbol-function '+workspace-list-names)
             (lambda () '("other"))))
    (should (null (claude-repl--rename-validate
                   "foo" "bar" "DWC/bar" "DWC/bar-tag"
                   "/nonexistent/path" "/tmp/foo")))))

;;;; ---- Tests: assert-no-pending-merge ----

(ert-deftest claude-repl-test-rename-assert-no-pending-clean ()
  "No merge markers present → no-op."
  (claude-repl-test-rename--with-temp-git-repo repo
    (claude-repl-test-rename--seed-commit repo)
    (should (null (claude-repl--rename-assert-no-pending-merge repo)))))

(ert-deftest claude-repl-test-rename-assert-no-pending-cherry-pick-errors ()
  "CHERRY_PICK_HEAD present → user-error."
  (claude-repl-test-rename--with-temp-git-repo repo
    (claude-repl-test-rename--seed-commit repo)
    (let* ((git-dir (string-trim
                     (shell-command-to-string
                      (format "git -C %s rev-parse --absolute-git-dir"
                              (shell-quote-argument repo))))))
      (write-region "deadbeef" nil (expand-file-name "CHERRY_PICK_HEAD" git-dir))
      (should-error (claude-repl--rename-assert-no-pending-merge repo)
                    :type 'user-error))))

(ert-deftest claude-repl-test-rename-assert-no-pending-merge-head-errors ()
  "MERGE_HEAD present → user-error."
  (claude-repl-test-rename--with-temp-git-repo repo
    (claude-repl-test-rename--seed-commit repo)
    (let* ((git-dir (string-trim
                     (shell-command-to-string
                      (format "git -C %s rev-parse --absolute-git-dir"
                              (shell-quote-argument repo))))))
      (write-region "deadbeef" nil (expand-file-name "MERGE_HEAD" git-dir))
      (should-error (claude-repl--rename-assert-no-pending-merge repo)
                    :type 'user-error))))

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
    (let ((dir (make-temp-file "claude-repl-rename-new-" t)))
      (unwind-protect
          (progn
            (claude-repl--ws-put "foo" :project-dir "/old/path")
            (claude-repl--rename-rehash-state "foo" "bar" dir)
            (should (equal (claude-repl--ws-get "bar" :project-dir)
                           (claude-repl--path-canonical dir))))
        (delete-directory dir t)))))

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
    (let ((old (make-temp-file "claude-repl-rename-old-" t))
          (new (make-temp-file "claude-repl-rename-new-" t)))
      (unwind-protect
          (progn
            (claude-repl--ws-put "child" :source-ws-dir old)
            (claude-repl--rename-update-source-back-refs old new)
            (should (equal (claude-repl--ws-get "child" :source-ws-dir)
                           (claude-repl--path-canonical new))))
        (delete-directory old t)
        (delete-directory new t)))))

(ert-deftest claude-repl-test-rename-source-back-refs-leaves-unrelated ()
  "Peer workspaces pointing elsewhere are untouched."
  (claude-repl-test--with-clean-state
    (let ((old (make-temp-file "claude-repl-rename-old-" t))
          (new (make-temp-file "claude-repl-rename-new-" t))
          (other (make-temp-file "claude-repl-rename-other-" t)))
      (unwind-protect
          (progn
            (claude-repl--ws-put "child" :source-ws-dir other)
            (claude-repl--rename-update-source-back-refs old new)
            (should (equal (claude-repl--ws-get "child" :source-ws-dir)
                           other)))
        (delete-directory old t)
        (delete-directory new t)
        (delete-directory other t)))))

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
    (let* ((vbuf (get-buffer-create "*claude-panel-old*"))
           (dir (make-temp-file "claude-repl-rename-newdir-" t)))
      (unwind-protect
          (progn
            (puthash "new" (list :vterm-buffer vbuf
                                 :project-dir dir)
                     claude-repl--workspaces)
            (claude-repl--rename-update-buffers "old" "new" dir)
            (should (equal (buffer-local-value 'default-directory vbuf)
                           (file-name-as-directory dir))))
        (when (buffer-live-p vbuf) (kill-buffer vbuf))
        (when (file-directory-p dir) (delete-directory dir t))))))

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

;;;; ---- Tests: end-to-end against a real git worktree ----

(ert-deftest claude-repl-test-rename-end-to-end-renames-branch-and-dir ()
  "Full rename pipeline against a real worktree renames branch, tag,
directory, and rehashes state."
  (claude-repl-test--with-clean-state
    (claude-repl-test-rename--with-temp-git-repo repo
      (claude-repl-test-rename--seed-commit repo)
      (let* ((parent (make-temp-file "claude-repl-rename-wt-parent-" t))
             (wt (expand-file-name "foo" parent))
             (claude-repl-worktree-tag-branch-suffix "-tag"))
        (unwind-protect
            (progn
              ;; Create the worktree on DWC/foo and a companion tag branch.
              (call-process "git" nil nil nil "-C" repo
                            "worktree" "add" "-b" "DWC/foo" wt)
              (call-process "git" nil nil nil "-C" repo
                            "branch" "DWC/foo-tag" "DWC/foo")
              ;; Register state mirroring how the create flow does.
              (claude-repl--ws-put "foo" :project-dir
                                   (claude-repl--path-canonical wt))
              (claude-repl--ws-put "foo" :worktree-p t)
              ;; Stub out persp/projectile so the test stays focused on git+state.
              (cl-letf (((symbol-function 'persp-get-by-name) (lambda (_) nil))
                        ((symbol-function 'persp-rename) (lambda (&rest _) t))
                        ((symbol-function '+workspace-list-names) (lambda () '("foo"))))
                (claude-repl--do-rename-workspace "foo" "bar"))
              ;; New branch + tag branch exist; old ones do not.
              (should (claude-repl--git-branch-exists-p repo "DWC/bar"))
              (should (claude-repl--git-branch-exists-p repo "DWC/bar-tag"))
              (should-not (claude-repl--git-branch-exists-p repo "DWC/foo"))
              (should-not (claude-repl--git-branch-exists-p repo "DWC/foo-tag"))
              ;; New directory exists; old does not.
              (should (file-directory-p (expand-file-name "bar" parent)))
              (should-not (file-directory-p wt))
              ;; State is rehashed to the new name and points at the new path.
              (should (null (gethash "foo" claude-repl--workspaces)))
              (should (equal (claude-repl--ws-get "bar" :project-dir)
                             (claude-repl--path-canonical
                              (expand-file-name "bar" parent)))))
          ;; Cleanup any leftover worktree on either name.
          (ignore-errors
            (call-process "git" nil nil nil "-C" repo
                          "worktree" "remove" "--force"
                          (expand-file-name "bar" parent)))
          (ignore-errors
            (call-process "git" nil nil nil "-C" repo
                          "worktree" "remove" "--force" wt))
          (when (file-directory-p parent)
            (delete-directory parent t)))))))

(ert-deftest claude-repl-test-rename-end-to-end-rejects-existing-branch ()
  "Rename to a name whose branch already exists aborts before touching state."
  (claude-repl-test--with-clean-state
    (claude-repl-test-rename--with-temp-git-repo repo
      (claude-repl-test-rename--seed-commit repo)
      (let* ((parent (make-temp-file "claude-repl-rename-wt-parent-" t))
             (wt (expand-file-name "foo" parent))
             (claude-repl-worktree-tag-branch-suffix nil))
        (unwind-protect
            (progn
              (call-process "git" nil nil nil "-C" repo
                            "worktree" "add" "-b" "DWC/foo" wt)
              ;; Pre-create the target branch so validation rejects.
              (call-process "git" nil nil nil "-C" repo
                            "branch" "DWC/bar" "master")
              (claude-repl--ws-put "foo" :project-dir
                                   (claude-repl--path-canonical wt))
              (cl-letf (((symbol-function 'persp-get-by-name) (lambda (_) nil))
                        ((symbol-function '+workspace-list-names) (lambda () '("foo"))))
                (should-error (claude-repl--do-rename-workspace "foo" "bar")
                              :type 'user-error))
              ;; State is unchanged; old branch and worktree still present.
              (should (claude-repl--git-branch-exists-p repo "DWC/foo"))
              (should (file-directory-p wt))
              (should (gethash "foo" claude-repl--workspaces)))
          (ignore-errors
            (call-process "git" nil nil nil "-C" repo
                          "worktree" "remove" "--force" wt))
          (when (file-directory-p parent)
            (delete-directory parent t)))))))

(provide 'test-rename)

;;; test-rename.el ends here
