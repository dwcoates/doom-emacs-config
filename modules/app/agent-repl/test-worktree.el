;;; test-worktree.el --- ERT tests for worktree.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for worktree management: git helpers, merge-fork computation,
;; workspace commands dispatch, and cherry-pick logic.
;;
;; Run with:
;;   emacs -batch -Q -l ert -l test-worktree.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'json)

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Tests: extract-cherry-pick-shas ----

(ert-deftest agent-repl-test-extract-cherry-pick-shas-empty ()
  "Empty log text returns nil."
  (should (equal (agent-repl--extract-cherry-pick-shas "") nil)))

(ert-deftest agent-repl-test-extract-cherry-pick-shas-no-annotations ()
  "Log text without cherry-pick annotations returns nil."
  (should (equal (agent-repl--extract-cherry-pick-shas
                  "Some commit message\n\nAnother commit")
                 nil)))

(ert-deftest agent-repl-test-extract-cherry-pick-shas-single ()
  "Single cherry-pick annotation is extracted."
  (let ((sha "abc123def456789012345678901234567890abcd"))
    (should (equal (agent-repl--extract-cherry-pick-shas
                    (format "commit msg\n\n(cherry picked from commit %s)" sha))
                   (list sha)))))

(ert-deftest agent-repl-test-extract-cherry-pick-shas-multiple ()
  "Multiple cherry-pick annotations are extracted in reverse order (pushed)."
  (let ((sha1 "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
        (sha2 "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"))
    (let ((result (agent-repl--extract-cherry-pick-shas
                   (format "msg1\n(cherry picked from commit %s)\n\nmsg2\n(cherry picked from commit %s)"
                           sha1 sha2))))
      ;; push reverses order: sha2 first, then sha1
      (should (equal result (list sha2 sha1))))))

(ert-deftest agent-repl-test-extract-cherry-pick-shas-ignores-short-hashes ()
  "Short hashes (not 40 hex chars) are not matched."
  (should (equal (agent-repl--extract-cherry-pick-shas
                  "(cherry picked from commit abc123)")
                 nil)))

;;;; ---- Tests: bare-workspace-name ----

(ert-deftest agent-repl-test-bare-workspace-name-simple ()
  "Simple name returns itself."
  (should (equal (agent-repl--bare-workspace-name "foo") "foo")))

(ert-deftest agent-repl-test-bare-workspace-name-slashed ()
  "Path-style name returns only the last component."
  (should (equal (agent-repl--bare-workspace-name "DWC/foo") "foo")))

(ert-deftest agent-repl-test-bare-workspace-name-deep-path ()
  "Deeply nested path returns only the last component."
  (should (equal (agent-repl--bare-workspace-name "DWC/CV-100/cool-branch") "cool-branch")))

(ert-deftest agent-repl-test-bare-workspace-name-trailing-slash ()
  "Trailing slash is stripped before extracting."
  (should (equal (agent-repl--bare-workspace-name "DWC/foo/") "foo")))

;;;; ---- Tests: assert-clean-worktree ----
;;
;; `agent-repl--assert-clean-worktree' calls `agent-repl--git-exit-code'
;; twice (unstaged + staged check).  Tests mock the wrapper.

(ert-deftest agent-repl-test-assert-clean-worktree-clean ()
  "Clean worktree does not signal."
  (cl-letf (((symbol-function 'agent-repl--git-exit-code)
             (lambda (&rest _args) 0)))
    ;; Should not error.
    (agent-repl--assert-clean-worktree "test-ws" "/tmp/repo")))

(ert-deftest agent-repl-test-assert-clean-worktree-unstaged ()
  "Unstaged changes signal user-error."
  (cl-letf (((symbol-function 'agent-repl--git-exit-code)
             (lambda (_root &rest args)
               (if (equal args '("diff" "--quiet")) 1 0))))
    (should-error (agent-repl--assert-clean-worktree "test-ws" "/tmp/repo")
                  :type 'user-error)))

(ert-deftest agent-repl-test-assert-clean-worktree-staged ()
  "Staged changes signal user-error."
  (cl-letf (((symbol-function 'agent-repl--git-exit-code)
             (lambda (_root &rest args)
               (if (equal args '("diff" "--cached" "--quiet")) 1 0))))
    (should-error (agent-repl--assert-clean-worktree "test-ws" "/tmp/repo")
                  :type 'user-error)))

;;;; ---- Tests: worktree-dirty-p ----
;;
;; `agent-repl--worktree-dirty-p' is the predicate counterpart to
;; `--assert-clean-worktree' — same git probes, but returns nil/t
;; instead of signaling.

(ert-deftest agent-repl-test-worktree-dirty-p-returns-nil-when-clean ()
  "Clean worktree (both git probes exit 0) returns nil."
  (cl-letf (((symbol-function 'agent-repl--git-exit-code)
             (lambda (&rest _args) 0)))
    (should-not (agent-repl--worktree-dirty-p "/tmp/repo"))))

(ert-deftest agent-repl-test-worktree-dirty-p-returns-t-when-unstaged ()
  "Unstaged changes (diff --quiet non-zero) flip the predicate to t."
  (cl-letf (((symbol-function 'agent-repl--git-exit-code)
             (lambda (_root &rest args)
               (if (equal args '("diff" "--quiet")) 1 0))))
    (should (agent-repl--worktree-dirty-p "/tmp/repo"))))

(ert-deftest agent-repl-test-worktree-dirty-p-returns-t-when-staged ()
  "Staged changes (diff --cached --quiet non-zero) flip the predicate to t."
  (cl-letf (((symbol-function 'agent-repl--git-exit-code)
             (lambda (_root &rest args)
               (if (equal args '("diff" "--cached" "--quiet")) 1 0))))
    (should (agent-repl--worktree-dirty-p "/tmp/repo"))))

;;;; ---- Tests: git-exit-code / git-branch-exists-p ----
;;
;; The wrappers themselves (`agent-repl--git-exit-code',
;; `agent-repl--git-branch-exists-p', `agent-repl--git-tag-exists-p')
;; are registered external boundaries; per AGENTS.md "No External
;; Processes or External State in Tests" / "We test lisp, not external
;; code", they are mocked in their callers' tests rather than exercised
;; in isolation.  Tests that did nothing but call these wrappers against
;; a mocked `--git-exit-code' have been removed — they only re-asserted
;; the boundary itself, which does not belong in ERT.
;;
;; The worker-side logic AROUND the boundary is lisp we own, so it IS
;; tested: worker status pass-through, and worker timeout → 124.  The
;; main-thread dispatch arm is the registered boundary itself (the
;; unmocked-boundary guard rejects calling it in ERT), so it stays
;; untested per the same policy.

(ert-deftest agent-repl-test-git-exit-code-worker-returns-wait-status ()
  "The worker implementation returns the exit status produced by
`agent-repl--wait-for-process-exit' unchanged on a normal exit."
  (cl-letf (((symbol-function 'start-process)
             (lambda (&rest _) 'fake-proc))
            ((symbol-function 'set-process-query-on-exit-flag)
             (lambda (&rest _) nil))
            ((symbol-function 'agent-repl--wait-for-process-exit)
             (lambda (&rest _) 5)))
    (should (= 5 (agent-repl--git-exit-code--worker "/repo" '("status"))))))

(ert-deftest agent-repl-test-git-exit-code-worker-maps-timeout-to-124 ()
  "The worker implementation maps the symbol `timeout' from the wait
helper to exit code 124 so numeric callers (`=', `%d' logs) never see
a symbol."
  (cl-letf (((symbol-function 'start-process)
             (lambda (&rest _) 'fake-proc))
            ((symbol-function 'set-process-query-on-exit-flag)
             (lambda (&rest _) nil))
            ((symbol-function 'agent-repl--wait-for-process-exit)
             (lambda (&rest _) 'timeout))
            ((symbol-function 'agent-repl--log)
             (lambda (&rest _) nil)))
    (should (= 124 (agent-repl--git-exit-code--worker "/repo" '("fetch"))))))

;;;; ---- Tests: parse-worktree-porcelain ----

(ert-deftest agent-repl-test-parse-worktree-porcelain-finds-master ()
  "Finds the worktree path whose branch matches the target ref."
  (let ((text (concat "worktree /repo/main\n"
                      "HEAD abc123\n"
                      "branch refs/heads/master\n"
                      "\n"
                      "worktree /repo/feature-x\n"
                      "HEAD def456\n"
                      "branch refs/heads/feature-x\n")))
    (should (equal (agent-repl--parse-worktree-porcelain text "refs/heads/master")
                   "/repo/main"))))

(ert-deftest agent-repl-test-parse-worktree-porcelain-finds-non-first-entry ()
  "Finds master even when it's not the first worktree listed."
  (let ((text (concat "worktree /repo/feature-x\n"
                      "HEAD def456\n"
                      "branch refs/heads/feature-x\n"
                      "\n"
                      "worktree /repo/main\n"
                      "HEAD abc123\n"
                      "branch refs/heads/master\n")))
    (should (equal (agent-repl--parse-worktree-porcelain text "refs/heads/master")
                   "/repo/main"))))

(ert-deftest agent-repl-test-parse-worktree-porcelain-no-match ()
  "Returns nil when no entry matches the target ref."
  (let ((text (concat "worktree /repo/feature-x\n"
                      "HEAD def456\n"
                      "branch refs/heads/feature-x\n")))
    (should (null (agent-repl--parse-worktree-porcelain text "refs/heads/master")))))

(ert-deftest agent-repl-test-parse-worktree-porcelain-ignores-detached-head ()
  "Worktrees with detached HEAD (no `branch' line) are not matched."
  (let ((text (concat "worktree /repo/detached\n"
                      "HEAD abc123\n"
                      "detached\n"
                      "\n"
                      "worktree /repo/main\n"
                      "HEAD def456\n"
                      "branch refs/heads/master\n")))
    (should (equal (agent-repl--parse-worktree-porcelain text "refs/heads/master")
                   "/repo/main"))))

(ert-deftest agent-repl-test-parse-worktree-porcelain-empty ()
  "Empty input returns nil."
  (should (null (agent-repl--parse-worktree-porcelain "" "refs/heads/master"))))

;;;; ---- Tests: master-worktree-path ----
;;
;; `agent-repl--master-worktree-path' shells out via
;; `agent-repl--git-string-quiet "-C" root "worktree" "list" "--porcelain"'
;; and parses the porcelain output for the trunk branch.  Tests mock the
;; wrapper and assert the parser/dispatch behavior, per AGENTS.md "No
;; External Processes or External State in Tests".

(ert-deftest agent-repl-test-master-worktree-path-single-worktree ()
  "In a single-worktree repo on master, returns that worktree's path."
  (let ((agent-repl-master-branch-name "master")
        (porcelain "worktree /tmp/repo\nHEAD abc\nbranch refs/heads/master\n"))
    (cl-letf (((symbol-function 'agent-repl--git-string-quiet)
               (lambda (&rest args)
                 (should (equal args '("-C" "/tmp/repo" "worktree" "list" "--porcelain")))
                 porcelain)))
      (should (equal (agent-repl--master-worktree-path "/tmp/repo")
                     "/tmp/repo")))))

(ert-deftest agent-repl-test-master-worktree-path-with-secondary ()
  "In a repo with main + secondary worktree, returns the main path."
  (let ((agent-repl-master-branch-name "master")
        (porcelain (concat "worktree /tmp/repo\nHEAD abc\nbranch refs/heads/master\n\n"
                           "worktree /tmp/wt\nHEAD def\nbranch refs/heads/feature-x\n")))
    (cl-letf (((symbol-function 'agent-repl--git-string-quiet)
               (lambda (&rest _args) porcelain)))
      ;; From the primary path.
      (should (equal (agent-repl--master-worktree-path "/tmp/repo") "/tmp/repo"))
      ;; Even when called from the secondary worktree, returns master path.
      (should (equal (agent-repl--master-worktree-path "/tmp/wt") "/tmp/repo")))))

(ert-deftest agent-repl-test-master-worktree-path-no-master ()
  "When no worktree is on the master branch, returns nil."
  (let ((agent-repl-master-branch-name "master")
        (porcelain "worktree /tmp/repo\nHEAD abc\nbranch refs/heads/feature-only\n"))
    (cl-letf (((symbol-function 'agent-repl--git-string-quiet)
               (lambda (&rest _args) porcelain)))
      (should (null (agent-repl--master-worktree-path "/tmp/repo"))))))

(ert-deftest agent-repl-test-master-worktree-path-honors-defcustom ()
  "Uses `agent-repl-master-branch-name' as the trunk branch name."
  (let ((agent-repl-master-branch-name "trunk")
        (porcelain "worktree /tmp/repo\nHEAD abc\nbranch refs/heads/trunk\n"))
    (cl-letf (((symbol-function 'agent-repl--git-string-quiet)
               (lambda (&rest _args) porcelain)))
      (should (equal (agent-repl--master-worktree-path "/tmp/repo")
                     "/tmp/repo")))))

;;;; ---- Tests: main-worktree-path ----
;;
;; `agent-repl--main-worktree-path' shells out via
;; `agent-repl--git-string-quiet "-C" root "rev-parse" "--git-common-dir"'
;; and takes the parent of the returned `.git' path.  Distinct from
;; `--master-worktree-path' (which depends on which branch is checked
;; out where) — this one identifies the original clone regardless of
;; branch state.

(ert-deftest agent-repl-test-main-worktree-path-from-main-relative-dot-git ()
  "In the main worktree itself, `git-common-dir' returns `.git' (relative)
which resolves to the worktree's parent dir."
  (let ((repo (make-temp-file "agent-repl-test-main-" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" repo) t)
          (cl-letf (((symbol-function 'agent-repl--git-string-quiet)
                     (lambda (&rest args)
                       (should (equal args (list "-C" repo "rev-parse"
                                                 "--git-common-dir")))
                       ".git")))
            (should (equal (agent-repl--main-worktree-path repo)
                           (directory-file-name repo)))))
      (delete-directory repo t))))

(ert-deftest agent-repl-test-main-worktree-path-from-linked-worktree ()
  "In a linked worktree, `git-common-dir' returns the absolute path
to the main repo's `.git', and the helper returns its parent."
  (let* ((main (make-temp-file "agent-repl-test-main-" t))
         (wt (make-temp-file "agent-repl-test-wt-" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" main) t)
          (cl-letf (((symbol-function 'agent-repl--git-string-quiet)
                     (lambda (&rest _args)
                       (expand-file-name ".git" main))))
            (should (equal (agent-repl--main-worktree-path wt)
                           (directory-file-name main)))))
      (delete-directory main t)
      (delete-directory wt t))))

(ert-deftest agent-repl-test-main-worktree-path-returns-nil-on-git-failure ()
  "When git emits a fatal/empty response, the helper returns nil."
  (cl-letf (((symbol-function 'agent-repl--git-string-quiet)
             (lambda (&rest _args) "")))
    (should (null (agent-repl--main-worktree-path "/tmp/not-a-repo")))))

(ert-deftest agent-repl-test-main-worktree-path-returns-nil-on-fatal-output ()
  "When git emits a `fatal:'-prefixed response, the helper returns nil."
  (cl-letf (((symbol-function 'agent-repl--git-string-quiet)
             (lambda (&rest _args) "fatal: not a git repository")))
    (should (null (agent-repl--main-worktree-path "/tmp/not-a-repo")))))

(ert-deftest agent-repl-test-main-worktree-path-returns-nil-when-parent-missing ()
  "If the resolved parent dir does not exist on disk, return nil."
  (cl-letf (((symbol-function 'agent-repl--git-string-quiet)
             (lambda (&rest _args)
               "/nonexistent/agent-repl-test/repo/.git")))
    (should (null (agent-repl--main-worktree-path "/tmp/whatever")))))

;;;; ---- Tests: checkout-master-in-worktree ----
;;
;; `agent-repl--checkout-master-in-worktree' shells out via
;; `agent-repl--git-string-quiet' (current branch) and
;; `agent-repl--git-exit-code' (`git checkout').  Tests mock both
;; wrappers and assert the dispatch semantics.

(ert-deftest agent-repl-test-checkout-master-already-on-master-no-op ()
  "When the worktree is already on master, no `git checkout' is run,
the function returns t, and only the rev-parse probe fires."
  (let ((agent-repl-master-branch-name "master")
        (exit-calls nil))
    (cl-letf (((symbol-function 'agent-repl--git-string-quiet)
               (lambda (&rest _args) "master"))
              ((symbol-function 'agent-repl--git-exit-code)
               (lambda (&rest args) (push args exit-calls) 0)))
      (should (eq t (agent-repl--checkout-master-in-worktree "/repo/main")))
      (should (null exit-calls)))))

(ert-deftest agent-repl-test-checkout-master-not-on-master-runs-checkout ()
  "When the worktree is on a sibling branch, `git checkout master' runs
and the function returns t on exit-code 0."
  (let ((agent-repl-master-branch-name "master")
        (checkout-args nil))
    (cl-letf (((symbol-function 'agent-repl--git-string-quiet)
               (lambda (&rest _args) "DWC/feature-x"))
              ((symbol-function 'agent-repl--git-exit-code)
               (lambda (&rest args) (setq checkout-args args) 0)))
      (should (eq t (agent-repl--checkout-master-in-worktree "/repo/main")))
      (should (equal checkout-args
                     '("/repo/main" "checkout" "master"))))))

(ert-deftest agent-repl-test-checkout-master-checkout-failure-returns-nil ()
  "When `git checkout' exits non-zero (e.g. another worktree holds master),
the function returns nil — caller decides what to do with the failure."
  (let ((agent-repl-master-branch-name "master"))
    (cl-letf (((symbol-function 'agent-repl--git-string-quiet)
               (lambda (&rest _args) "DWC/feature-x"))
              ((symbol-function 'agent-repl--git-exit-code)
               (lambda (&rest _args) 128)))
      (should (null (agent-repl--checkout-master-in-worktree "/repo/main"))))))

(ert-deftest agent-repl-test-checkout-master-honors-defcustom ()
  "Uses `agent-repl-master-branch-name' as the trunk branch name in both
the rev-parse comparison and the checkout invocation."
  (let ((agent-repl-master-branch-name "trunk")
        (checkout-args nil))
    (cl-letf (((symbol-function 'agent-repl--git-string-quiet)
               (lambda (&rest _args) "feature"))
              ((symbol-function 'agent-repl--git-exit-code)
               (lambda (&rest args) (setq checkout-args args) 0)))
      (should (eq t (agent-repl--checkout-master-in-worktree "/repo/main")))
      (should (equal checkout-args
                     '("/repo/main" "checkout" "trunk"))))))

;;;; ---- Tests: apply-workspace-properties ----

(ert-deftest agent-repl-test-apply-workspace-properties-nil-values-skipped ()
  "Nil values in the plist are not stored."
  (agent-repl-test--with-clean-state
    (agent-repl--apply-workspace-properties "ws1" :priority nil :fork-session-id "abc")
    (should (equal (agent-repl--ws-get "ws1" :fork-session-id) "abc"))
    (should (null (agent-repl--ws-get "ws1" :priority)))))

(ert-deftest agent-repl-test-apply-workspace-properties-all-non-nil ()
  "All non-nil values are stored."
  (agent-repl-test--with-clean-state
    (agent-repl--apply-workspace-properties "ws1" :priority 5 :fork-session-id "xyz")
    (should (equal (agent-repl--ws-get "ws1" :priority) 5))
    (should (equal (agent-repl--ws-get "ws1" :fork-session-id) "xyz"))))

(ert-deftest agent-repl-test-apply-workspace-properties-empty ()
  "Empty plist is a no-op."
  (agent-repl-test--with-clean-state
    (agent-repl--apply-workspace-properties "ws1")
    (should (null (gethash "ws1" agent-repl--workspaces)))))

;;;; ---- Tests: enqueue-preemptive-prompt ----

(ert-deftest agent-repl-test-enqueue-preemptive-prompt-stores ()
  "Non-empty prompt is stored as pending-prompts list."
  (agent-repl-test--with-clean-state
    (agent-repl--enqueue-preemptive-prompt "ws1" "do the thing")
    (should (equal (agent-repl--ws-get "ws1" :pending-prompts) '("do the thing")))
    (should (eq (agent-repl--ws-get "ws1" :pending-show-panels) t))))

(ert-deftest agent-repl-test-enqueue-preemptive-prompt-nil ()
  "Nil prompt does not store anything."
  (agent-repl-test--with-clean-state
    (agent-repl--enqueue-preemptive-prompt "ws1" nil)
    (should (null (agent-repl--ws-get "ws1" :pending-prompts)))))

(ert-deftest agent-repl-test-enqueue-preemptive-prompt-nil-no-show-panels ()
  "Nil prompt does not set :pending-show-panels."
  (agent-repl-test--with-clean-state
    (agent-repl--enqueue-preemptive-prompt "ws1" nil)
    (should (null (agent-repl--ws-get "ws1" :pending-show-panels)))))

(ert-deftest agent-repl-test-enqueue-preemptive-prompt-empty-string ()
  "Empty string prompt does not store anything."
  (agent-repl-test--with-clean-state
    (agent-repl--enqueue-preemptive-prompt "ws1" "")
    (should (null (agent-repl--ws-get "ws1" :pending-prompts)))))

(ert-deftest agent-repl-test-enqueue-preemptive-prompt-empty-no-show-panels ()
  "Empty string prompt does not set :pending-show-panels."
  (agent-repl-test--with-clean-state
    (agent-repl--enqueue-preemptive-prompt "ws1" "")
    (should (null (agent-repl--ws-get "ws1" :pending-show-panels)))))

;;;; ---- Tests: record-merged-in-workspace ----

(ert-deftest agent-repl-test-record-merged-in-appends-name ()
  "Records the merged workspace on the receiver's `:merged-in-workspaces'."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "parent" :project-dir "/tmp/parent/")
    (agent-repl--record-merged-in-workspace "/tmp/parent/" "child")
    (should (equal (agent-repl--ws-get "parent" :merged-in-workspaces)
                   '("child")))))

(ert-deftest agent-repl-test-record-merged-in-preserves-order ()
  "Successive records append in insertion order."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "parent" :project-dir "/tmp/parent/")
    (agent-repl--record-merged-in-workspace "/tmp/parent/" "child-a")
    (agent-repl--record-merged-in-workspace "/tmp/parent/" "child-b")
    (should (equal (agent-repl--ws-get "parent" :merged-in-workspaces)
                   '("child-a" "child-b")))))

(ert-deftest agent-repl-test-record-merged-in-dedups ()
  "Recording the same merged workspace twice stores it once."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "parent" :project-dir "/tmp/parent/")
    (agent-repl--record-merged-in-workspace "/tmp/parent/" "child")
    (agent-repl--record-merged-in-workspace "/tmp/parent/" "child")
    (should (equal (agent-repl--ws-get "parent" :merged-in-workspaces)
                   '("child")))))

(ert-deftest agent-repl-test-record-merged-in-nil-dir-noop ()
  "A nil target-dir records nothing."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "parent" :project-dir "/tmp/parent/")
    (agent-repl--record-merged-in-workspace nil "child")
    (should (null (agent-repl--ws-get "parent" :merged-in-workspaces)))))

(ert-deftest agent-repl-test-record-merged-in-unknown-dir-noop ()
  "A target-dir with no owning workspace records nothing."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "parent" :project-dir "/tmp/parent/")
    (agent-repl--record-merged-in-workspace "/tmp/nonesuch/" "child")
    (should (null (agent-repl--ws-get "parent" :merged-in-workspaces)))))

(ert-deftest agent-repl-test-record-merged-in-skips-self ()
  "A workspace is never recorded as merged into itself."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "parent" :project-dir "/tmp/parent/")
    (agent-repl--record-merged-in-workspace "/tmp/parent/" "parent")
    (should (null (agent-repl--ws-get "parent" :merged-in-workspaces)))))

;;;; ---- Tests: dispatch-prompt-command ----

(ert-deftest agent-repl-test-dispatch-prompt-enqueues-when-no-buffer ()
  "When no vterm buffer exists, prompt is enqueued on :pending-prompts."
  (agent-repl-test--with-clean-state
    (agent-repl--dispatch-prompt-command "ws1" "hello")
    (should (equal (agent-repl--ws-get "ws1" :pending-prompts) '("hello")))))

(ert-deftest agent-repl-test-dispatch-prompt-enqueues-when-not-ready ()
  "When vterm buffer exists but is not ready, prompt is enqueued."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer "*agent-panel-test-vterm*"
      (setq-local agent-repl--ready nil)
      (agent-repl--ws-put "ws1" :vterm-buffer (current-buffer))
      (agent-repl--dispatch-prompt-command "ws1" "hello")
      (should (equal (agent-repl--ws-get "ws1" :pending-prompts) '("hello"))))))

(ert-deftest agent-repl-test-dispatch-prompt-appends-to-existing ()
  "Multiple prompts are appended to :pending-prompts in order."
  (agent-repl-test--with-clean-state
    (agent-repl--dispatch-prompt-command "ws1" "first")
    (agent-repl--dispatch-prompt-command "ws1" "second")
    (should (equal (agent-repl--ws-get "ws1" :pending-prompts) '("first" "second")))))

(ert-deftest agent-repl-test-dispatch-prompt-normalizes-branch-name ()
  "Branch-style name 'DWC/foo' is normalized to 'foo'."
  (agent-repl-test--with-clean-state
    (agent-repl--dispatch-prompt-command "DWC/foo" "hello")
    (should (equal (agent-repl--ws-get "foo" :pending-prompts) '("hello")))))

;;;; ---- Tests: workspace-commands-watch-handler ----

(ert-deftest agent-repl-test-watch-handler-ignores-non-workspace-files ()
  "Files not starting with workspace_commands_ are ignored."
  (let ((called nil))
    (cl-letf (((symbol-function 'agent-repl--process-workspace-commands-file)
               (lambda (_f) (setq called t))))
      (agent-repl--workspace-commands-watch-handler
       '(descriptor changed "/tmp/some-other-file.json"))
      (should-not called))))

(ert-deftest agent-repl-test-watch-handler-dispatches-on-created ()
  "Created workspace_commands_ files trigger processing."
  (let ((captured-file nil))
    (cl-letf (((symbol-function 'agent-repl--process-workspace-commands-file)
               (lambda (f) (setq captured-file f))))
      (agent-repl--workspace-commands-watch-handler
       '(descriptor created "/tmp/workspace_commands_123.json"))
      (should (equal captured-file "/tmp/workspace_commands_123.json")))))

(ert-deftest agent-repl-test-watch-handler-dispatches-on-changed ()
  "Changed workspace_commands_ files trigger processing."
  (let ((captured-file nil))
    (cl-letf (((symbol-function 'agent-repl--process-workspace-commands-file)
               (lambda (f) (setq captured-file f))))
      (agent-repl--workspace-commands-watch-handler
       '(descriptor changed "/tmp/workspace_commands_abc.json"))
      (should (equal captured-file "/tmp/workspace_commands_abc.json")))))

(ert-deftest agent-repl-test-watch-handler-dispatches-on-renamed ()
  "Renamed events use the new-file (4th element) for workspace_commands_ files."
  (let ((captured-file nil))
    (cl-letf (((symbol-function 'agent-repl--process-workspace-commands-file)
               (lambda (f) (setq captured-file f))))
      (agent-repl--workspace-commands-watch-handler
       '(descriptor renamed "/tmp/old-name" "/tmp/workspace_commands_new.json"))
      (should (equal captured-file "/tmp/workspace_commands_new.json")))))

(ert-deftest agent-repl-test-watch-handler-ignores-delete-action ()
  "Per-file delete actions do not process and do not re-arm the watch."
  (let ((called nil) (rearmed nil))
    (cl-letf (((symbol-function 'agent-repl--process-workspace-commands-file)
               (lambda (_f) (setq called t)))
              ((symbol-function 'agent-repl--register-workspace-commands-watch)
               (lambda () (setq rearmed t))))
      (agent-repl--workspace-commands-watch-handler
       '(descriptor deleted "/tmp/workspace_commands_del.json"))
      (should-not called)
      (should-not rearmed))))

(ert-deftest agent-repl-test-watch-handler-rearms-on-stopped ()
  "A `stopped' event re-arms the watch and drains pending files."
  (let ((rearmed nil) (drained nil))
    (cl-letf (((symbol-function 'agent-repl--register-workspace-commands-watch)
               (lambda () (setq rearmed t)))
              ((symbol-function 'agent-repl--drain-workspace-commands-files)
               (lambda () (setq drained t) 0)))
      (agent-repl--workspace-commands-watch-handler
       '(descriptor stopped "/Users/x/.claude/output"))
      (should rearmed)
      (should drained))))

(ert-deftest agent-repl-test-watch-handler-rearms-on-output-dir-deleted ()
  "A `deleted' event for the output directory itself re-arms and drains."
  (let ((rearmed nil) (drained nil)
        (agent-repl-workspace-commands-output-dir "/tmp/test-output/"))
    (cl-letf (((symbol-function 'agent-repl--register-workspace-commands-watch)
               (lambda () (setq rearmed t)))
              ((symbol-function 'agent-repl--drain-workspace-commands-files)
               (lambda () (setq drained t) 0)))
      (agent-repl--workspace-commands-watch-handler
       (list 'descriptor 'deleted (expand-file-name "/tmp/test-output")))
      (should rearmed)
      (should drained))))

;;;; ---- Tests: dispatch-workspace-command ----

(ert-deftest agent-repl-test-dispatch-workspace-command-create ()
  "Create commands increment delay by stagger-seconds."
  (let ((handled nil))
    (cl-letf (((symbol-function 'agent-repl--handle-create-command)
               (lambda (cmd delay) (push (list cmd delay) handled))))
      (let ((cmd '((type . "create") (name . "test"))))
        (let ((new-delay (agent-repl--dispatch-workspace-command cmd 0)))
          (should (= new-delay agent-repl-worktree-stagger-seconds))
          (should (= (length handled) 1)))))))

(ert-deftest agent-repl-test-dispatch-workspace-command-prompt ()
  "Prompt commands do not change delay."
  (let ((handled nil))
    (cl-letf (((symbol-function 'agent-repl--handle-prompt-command)
               (lambda (cmd) (push cmd handled))))
      (let ((cmd '((type . "prompt") (workspace . "ws1") (prompt . "hello"))))
        (let ((new-delay (agent-repl--dispatch-workspace-command cmd 10)))
          (should (= new-delay 10))
          (should (= (length handled) 1)))))))

(ert-deftest agent-repl-test-dispatch-workspace-command-finish ()
  "Finish commands do not change delay."
  (let ((handled nil))
    (cl-letf (((symbol-function 'agent-repl--handle-finish-command)
               (lambda (cmd) (push cmd handled))))
      (let ((cmd '((type . "finish") (workspace . "ws1"))))
        (let ((new-delay (agent-repl--dispatch-workspace-command cmd 10)))
          (should (= new-delay 10))
          (should (= (length handled) 1)))))))

(ert-deftest agent-repl-test-dispatch-workspace-command-merge ()
  "Merge commands do not change delay."
  (let ((handled nil))
    (cl-letf (((symbol-function 'agent-repl--handle-merge-command)
               (lambda (cmd) (push cmd handled))))
      (let ((cmd '((type . "merge") (workspace . "ws1"))))
        (let ((new-delay (agent-repl--dispatch-workspace-command cmd 10)))
          (should (= new-delay 10))
          (should (= (length handled) 1)))))))

(ert-deftest agent-repl-test-dispatch-workspace-command-close ()
  "Close commands route to the close handler and do not change delay."
  (let ((handled nil))
    (cl-letf (((symbol-function 'agent-repl--handle-close-command)
               (lambda (cmd) (push cmd handled))))
      (let ((cmd '((type . "close") (workspace . "ws1"))))
        (let ((new-delay (agent-repl--dispatch-workspace-command cmd 10)))
          (should (= new-delay 10))
          (should (= (length handled) 1)))))))

(ert-deftest agent-repl-test-dispatch-workspace-command-unknown ()
  "Unknown command type does not change delay and does not error."
  (let ((new-delay (agent-repl--dispatch-workspace-command
                    '((type . "bogus")) 10)))
    (should (= new-delay 10))))

(ert-deftest agent-repl-test-dispatch-workspace-command-clipboard ()
  "Clipboard commands do not change delay and route to the handler."
  (let ((handled nil))
    (cl-letf (((symbol-function 'agent-repl--handle-clipboard-command)
               (lambda (cmd) (push cmd handled))))
      (let ((cmd '((type . "clipboard") (workspace . "ws1") (text . "hi"))))
        (let ((new-delay (agent-repl--dispatch-workspace-command cmd 10)))
          (should (= new-delay 10))
          (should (= (length handled) 1)))))))

(ert-deftest agent-repl-test-dispatch-workspace-command-send ()
  "Send commands do not change delay and route to the handler."
  (let ((handled nil))
    (cl-letf (((symbol-function 'agent-repl--handle-send-command)
               (lambda (cmd) (push cmd handled))))
      (let ((cmd '((type . "send") (workspace . "ws1") (data . "hi"))))
        (let ((new-delay (agent-repl--dispatch-workspace-command cmd 10)))
          (should (= new-delay 10))
          (should (= (length handled) 1)))))))

(ert-deftest agent-repl-test-dispatch-workspace-command-eval ()
  "Eval commands route to the eval handler and do not change delay."
  (let ((handled nil))
    (cl-letf (((symbol-function 'agent-repl--handle-eval-command)
               (lambda (cmd) (push cmd handled))))
      (let ((cmd '((type . "eval") (code . "(+ 1 2)"))))
        (let ((new-delay (agent-repl--dispatch-workspace-command cmd 10)))
          (should (= new-delay 10))
          (should (= (length handled) 1)))))))

(ert-deftest agent-repl-test-dispatch-workspace-command-open ()
  "Open commands route to the open handler and do not change delay."
  (let ((handled nil))
    (cl-letf (((symbol-function 'agent-repl--handle-open-command)
               (lambda (cmd) (push cmd handled))))
      (let ((cmd '((type . "open") (workspace . "ws1"))))
        (let ((new-delay (agent-repl--dispatch-workspace-command cmd 10)))
          (should (= new-delay 10))
          (should (= (length handled) 1)))))))

;;;; ---- Tests: profile-stop-and-collect ----

(ert-deftest agent-repl-test-profile-stop-and-collect-returns-new-buffer-text ()
  "Only buffers created by the wrapped `profiler-report' call are captured."
  (let ((old-buf (generate-new-buffer " *test-old-report*"))
        (new-buf (generate-new-buffer " *test-new-report*")))
    (unwind-protect
        (progn
          (with-current-buffer old-buf
            (setq major-mode 'profiler-report-mode)
            (insert "OLD"))
          (with-current-buffer new-buf
            (setq major-mode 'profiler-report-mode)
            (insert "NEW"))
          (cl-letf (((symbol-function 'profiler-stop) (lambda () nil))
                    ((symbol-function 'profiler-report) (lambda () nil))
                    ((symbol-function 'agent-repl--profile-report-buffers)
                     (let ((calls 0))
                       (lambda ()
                         (cl-incf calls)
                         (if (= calls 1)
                             (list old-buf)
                           (list old-buf new-buf))))))
            (let ((text (agent-repl--profile-stop-and-collect)))
              (should (string-match-p "NEW" text))
              (should-not (string-match-p "OLD" text)))))
      (when (buffer-live-p old-buf) (kill-buffer old-buf))
      (when (buffer-live-p new-buf) (kill-buffer new-buf)))))

(ert-deftest agent-repl-test-profile-stop-and-collect-empty-when-no-new-buffers ()
  "When no new profiler-report buffer is created, returns the empty string."
  (cl-letf (((symbol-function 'profiler-stop) (lambda () nil))
            ((symbol-function 'profiler-report) (lambda () nil))
            ((symbol-function 'agent-repl--profile-report-buffers) (lambda () nil)))
    (should (string= "" (agent-repl--profile-stop-and-collect)))))

(ert-deftest agent-repl-test-profile-stop-and-collect-suppresses-report-window ()
  "`profiler-report' runs with a no-window `display-buffer-overriding-action'.
Guards against regressing the user-visible behavior fix: the profiler
report buffer must still be created (so we can scrape its text) but no
window should pop up for the user, since the report is only forwarded
back to the requesting agent session."
  (let ((seen-action :unset))
    (cl-letf (((symbol-function 'profiler-stop) (lambda () nil))
              ((symbol-function 'profiler-report)
               (lambda () (setq seen-action display-buffer-overriding-action)))
              ((symbol-function 'agent-repl--profile-report-buffers) (lambda () nil)))
      (agent-repl--profile-stop-and-collect)
      (should (equal seen-action
                     '(display-buffer-no-window . ((allow-no-window . t))))))))

;;;; ---- Tests: profile-fully-expand-buffer ----

(ert-deftest agent-repl-test-profile-fully-expand-buffer-calls-expand-on-each-line ()
  "Expander walks every line and invokes `profiler-report-expand-entry' with FULL=t.
A 3-line buffer should produce 3 calls, each with non-nil arg, so the
recursive-subtree branch of `profiler-report-expand-entry' fires."
  (let ((buf (generate-new-buffer " *test-expand-walk*"))
        (calls nil))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (setq major-mode 'profiler-report-mode)
            (insert "line1\nline2\nline3\n"))
          (cl-letf (((symbol-function 'profiler-report-expand-entry)
                     (lambda (&optional full) (push full calls))))
            (agent-repl--profile-fully-expand-buffer buf))
          (should (= (length calls) 3))
          (should (cl-every #'identity calls)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest agent-repl-test-profile-fully-expand-buffer-noop-on-dead-buffer ()
  "Dead buffer is a no-op — expander returns without signaling or calling expand."
  (let ((buf (generate-new-buffer " *test-expand-dead*"))
        (called nil))
    (kill-buffer buf)
    (cl-letf (((symbol-function 'profiler-report-expand-entry)
               (lambda (&optional _) (setq called t))))
      (agent-repl--profile-fully-expand-buffer buf))
    (should-not called)))

(ert-deftest agent-repl-test-profile-fully-expand-buffer-noop-on-empty-buffer ()
  "Empty buffer is a no-op — eobp is true at point-min, loop body never runs."
  (let ((buf (generate-new-buffer " *test-expand-empty*"))
        (called nil))
    (unwind-protect
        (cl-letf (((symbol-function 'profiler-report-expand-entry)
                   (lambda (&optional _) (setq called t))))
          (agent-repl--profile-fully-expand-buffer buf))
      (when (buffer-live-p buf) (kill-buffer buf)))
    (should-not called)))

(ert-deftest agent-repl-test-profile-stop-and-collect-expands-new-buffer ()
  "`profile-stop-and-collect' fully expands each new report buffer before reading it.
The expander is invoked once per new buffer, so the captured text
reflects the post-expansion content rather than the default collapsed view."
  (let ((new-buf (generate-new-buffer " *test-collect-expand*"))
        (expand-calls nil))
    (unwind-protect
        (progn
          (with-current-buffer new-buf
            (setq major-mode 'profiler-report-mode)
            (insert "collapsed"))
          (cl-letf (((symbol-function 'profiler-stop) (lambda () nil))
                    ((symbol-function 'profiler-report) (lambda () nil))
                    ((symbol-function 'agent-repl--profile-report-buffers)
                     (let ((calls 0))
                       (lambda ()
                         (cl-incf calls)
                         (if (= calls 1) nil (list new-buf)))))
                    ((symbol-function 'agent-repl--profile-fully-expand-buffer)
                     (lambda (b)
                       (push b expand-calls)
                       (with-current-buffer b
                         (goto-char (point-max))
                         (insert " EXPANDED")))))
            (let ((text (agent-repl--profile-stop-and-collect)))
              (should (= (length expand-calls) 1))
              (should (eq (car expand-calls) new-buf))
              (should (string-match-p "collapsed EXPANDED" text)))))
      (when (buffer-live-p new-buf) (kill-buffer new-buf)))))

;;;; ---- Tests: profile-stop-and-write-file ----

(ert-deftest agent-repl-test-profile-stop-and-write-file-writes-and-returns-path ()
  "stop-and-write-file writes the collected report to the file and returns its path."
  (let ((tmp (make-temp-file "agent-repl-profile-test" nil ".txt")))
    (unwind-protect
        (cl-letf (((symbol-function 'profiler-running-p) (lambda () t))
                  ((symbol-function 'agent-repl--profile-stop-and-collect)
                   (lambda () "calltree contents"))
                  (agent-repl-profile-report-file tmp))
          (let ((path (agent-repl--profile-stop-and-write-file)))
            (should (equal path (expand-file-name tmp)))
            (should (string-match-p
                     "calltree contents"
                     (with-temp-buffer (insert-file-contents tmp) (buffer-string))))))
      (when (file-exists-p tmp) (delete-file tmp)))))

(ert-deftest agent-repl-test-profile-stop-and-write-file-nil-when-not-running ()
  "stop-and-write-file returns nil and never collects when the profiler is not running."
  (cl-letf (((symbol-function 'profiler-running-p) (lambda () nil))
            ((symbol-function 'agent-repl--profile-stop-and-collect)
             (lambda () (error "should not collect when not running"))))
    (should-not (agent-repl--profile-stop-and-write-file))))

(ert-deftest agent-repl-test-profile-stop-and-write-file-nil-on-empty-report ()
  "stop-and-write-file returns nil without writing when the collected report is empty."
  (cl-letf (((symbol-function 'profiler-running-p) (lambda () t))
            ((symbol-function 'agent-repl--profile-stop-and-collect) (lambda () "")))
    (should-not (agent-repl--profile-stop-and-write-file))))

;;;; ---- Tests: handle-clipboard-command ----

(ert-deftest agent-repl-test-handle-clipboard-command-stores-text ()
  "handle-clipboard-command stores `:text' on the workspace under `:clipboard'."
  (let ((agent-repl--workspaces (make-hash-table :test 'equal)))
    (puthash "ws1" '() agent-repl--workspaces)
    (agent-repl--handle-clipboard-command
     '((type . "clipboard") (workspace . "ws1") (text . "payload")))
    (should (equal (agent-repl--ws-get "ws1" :clipboard) "payload"))))

(ert-deftest agent-repl-test-handle-clipboard-command-missing-workspace ()
  "Missing `workspace' is logged and skipped — no error, no state change."
  (let ((agent-repl--workspaces (make-hash-table :test 'equal)))
    (agent-repl--handle-clipboard-command
     '((type . "clipboard") (text . "payload")))
    (should (= 0 (hash-table-count agent-repl--workspaces)))))

(ert-deftest agent-repl-test-handle-clipboard-command-missing-text ()
  "Missing `text' is logged and skipped — no error, slot stays nil."
  (let ((agent-repl--workspaces (make-hash-table :test 'equal)))
    (puthash "ws1" '() agent-repl--workspaces)
    (agent-repl--handle-clipboard-command
     '((type . "clipboard") (workspace . "ws1")))
    (should-not (agent-repl--ws-get "ws1" :clipboard))))

(ert-deftest agent-repl-test-handle-clipboard-command-overwrites ()
  "Successive clipboard commands overwrite the prior value."
  (let ((agent-repl--workspaces (make-hash-table :test 'equal)))
    (puthash "ws1" '() agent-repl--workspaces)
    (agent-repl--handle-clipboard-command
     '((workspace . "ws1") (text . "first")))
    (agent-repl--handle-clipboard-command
     '((workspace . "ws1") (text . "second")))
    (should (equal (agent-repl--ws-get "ws1" :clipboard) "second"))))

;;;; ---- Tests: handle-send-command ----

(ert-deftest agent-repl-test-handle-send-command-stores-data ()
  "handle-send-command stores arbitrary `data' on the workspace under `:send-data'."
  (let ((agent-repl--workspaces (make-hash-table :test 'equal)))
    (puthash "ws1" '() agent-repl--workspaces)
    (agent-repl--handle-send-command
     '((type . "send") (workspace . "ws1") (data . ((link . "https://x.test")))))
    (should (equal (agent-repl--ws-get "ws1" :send-data)
                   '((link . "https://x.test"))))))

(ert-deftest agent-repl-test-handle-send-command-missing-workspace ()
  "Missing `workspace' is logged and skipped — no error, no state change."
  (let ((agent-repl--workspaces (make-hash-table :test 'equal)))
    (agent-repl--handle-send-command
     '((type . "send") (data . "payload")))
    (should (= 0 (hash-table-count agent-repl--workspaces)))))

(ert-deftest agent-repl-test-handle-send-command-missing-data-key ()
  "Absent `data' key is logged and skipped — no error, slot stays nil."
  (let ((agent-repl--workspaces (make-hash-table :test 'equal)))
    (puthash "ws1" '() agent-repl--workspaces)
    (agent-repl--handle-send-command
     '((type . "send") (workspace . "ws1")))
    (should-not (agent-repl--ws-get "ws1" :send-data))))

(ert-deftest agent-repl-test-handle-send-command-falsey-payload-still-dispatches ()
  "A present-but-falsey `data' payload (nil) still reaches the store branch.
Gating is by KEY PRESENCE, not truthiness, so `:send-data' is written even
for nil — distinguished from the missing-key case (which never writes) by
spying on the store rather than reading back the indistinguishable nil."
  (let ((stored nil))
    (cl-letf (((symbol-function 'agent-repl--ws-put)
               (lambda (_ws key val) (push (cons key val) stored))))
      (agent-repl--handle-send-command
       '((type . "send") (workspace . "ws1") (data . nil)))
      (should (equal stored '((:send-data . nil))))
      ;; Missing data KEY must NOT write, proving the gate is key-presence.
      (setq stored nil)
      (agent-repl--handle-send-command
       '((type . "send") (workspace . "ws1")))
      (should (null stored)))))

;;;; ---- Tests: handle-send-pgn ----

(ert-deftest agent-repl-test-handle-send-pgn-creates-buffer-with-content ()
  "handle-send-pgn creates a buffer named *agent-repl-pgn:<WS>* with the PGN text."
  (let ((displayed-bufs nil)
        (board-calls nil))
    (cl-letf (((symbol-function 'pygn-mode) #'ignore)
              ((symbol-function 'display-buffer)
               (lambda (buf &rest _) (push buf displayed-bufs)))
              ((symbol-function 'pygn-mode-display-gui-board-at-pos)
               (lambda (pos) (push pos board-calls)))
              ((symbol-function 'agent-repl--ws-resolve-persp)
               (lambda (_ws) nil))
              ((symbol-function 'agent-repl--ws-current-name)
               (lambda () "ws1")))
      (let ((buf (agent-repl--handle-send-pgn
                  "ws1" "1. e4 e5 2. Nf3 *")))
        (unwind-protect
            (progn
              (should (buffer-live-p buf))
              (should (string= (buffer-name buf) "*agent-repl-pgn:ws1*"))
              (with-current-buffer buf
                (should (string= (buffer-string) "1. e4 e5 2. Nf3 *"))))
          (kill-buffer buf))))))

(ert-deftest agent-repl-test-handle-send-pgn-activates-pygn-mode ()
  "handle-send-pgn calls pygn-mode on the buffer."
  (let ((mode-calls nil))
    (cl-letf (((symbol-function 'pygn-mode)
               (lambda () (push t mode-calls)))
              ((symbol-function 'display-buffer) #'ignore)
              ((symbol-function 'pygn-mode-display-gui-board-at-pos) #'ignore)
              ((symbol-function 'agent-repl--ws-resolve-persp)
               (lambda (_ws) nil))
              ((symbol-function 'agent-repl--ws-current-name)
               (lambda () "ws1")))
      (let ((buf (agent-repl--handle-send-pgn "ws1" "1. d4 *")))
        (unwind-protect
            (should (= (length mode-calls) 1))
          (kill-buffer buf))))))

(ert-deftest agent-repl-test-handle-send-pgn-displays-buffer-when-ws-is-current ()
  "handle-send-pgn calls display-buffer only when WS is the active workspace."
  (let ((displayed nil))
    (cl-letf (((symbol-function 'pygn-mode) #'ignore)
              ((symbol-function 'display-buffer)
               (lambda (buf &rest _) (setq displayed buf)))
              ((symbol-function 'pygn-mode-display-gui-board-at-pos) #'ignore)
              ((symbol-function 'agent-repl--ws-resolve-persp)
               (lambda (_ws) nil))
              ((symbol-function 'agent-repl--ws-current-name)
               (lambda () "ws1")))
      (let ((buf (agent-repl--handle-send-pgn "ws1" "1. e4 *")))
        (unwind-protect
            (should (eq displayed buf))
          (kill-buffer buf))))))

(ert-deftest agent-repl-test-handle-send-pgn-skips-display-when-ws-is-not-current ()
  "handle-send-pgn does NOT call display-buffer when WS is not the active workspace."
  (let ((displayed nil))
    (cl-letf (((symbol-function 'pygn-mode) #'ignore)
              ((symbol-function 'display-buffer)
               (lambda (buf &rest _) (setq displayed buf)))
              ((symbol-function 'pygn-mode-display-gui-board-at-pos) #'ignore)
              ((symbol-function 'agent-repl--ws-resolve-persp)
               (lambda (_ws) nil))
              ((symbol-function 'agent-repl--ws-current-name)
               (lambda () "other-ws")))
      (let ((buf (agent-repl--handle-send-pgn "ws1" "1. e4 *")))
        (unwind-protect
            (should (null displayed))
          (kill-buffer buf))))))

(ert-deftest agent-repl-test-handle-send-pgn-adds-buffer-to-workspace-persp ()
  "handle-send-pgn attaches the buffer to the target workspace perspective."
  (let ((added-calls nil)
        (fake-persp '(fake-persp-object)))
    (cl-letf (((symbol-function 'pygn-mode) #'ignore)
              ((symbol-function 'display-buffer) #'ignore)
              ((symbol-function 'pygn-mode-display-gui-board-at-pos) #'ignore)
              ((symbol-function 'agent-repl--ws-resolve-persp)
               (lambda (_ws) fake-persp))
              ((symbol-function 'agent-repl--ws-add-buffer)
               (lambda (buf persp switch)
                 (push (list buf persp switch) added-calls)))
              ((symbol-function 'agent-repl--ws-current-name)
               (lambda () "ws1")))
      (let ((buf (agent-repl--handle-send-pgn "ws1" "1. e4 *")))
        (unwind-protect
            (progn
              (should (= (length added-calls) 1))
              (should (eq (car (car added-calls)) buf))
              (should (eq (cadr (car added-calls)) fake-persp))
              (should (null (caddr (car added-calls)))))
          (kill-buffer buf))))))

(ert-deftest agent-repl-test-handle-send-pgn-no-persp-add-when-persp-nil ()
  "handle-send-pgn does not call ws-add-buffer when resolve-persp returns nil."
  (let ((added-calls nil))
    (cl-letf (((symbol-function 'pygn-mode) #'ignore)
              ((symbol-function 'display-buffer) #'ignore)
              ((symbol-function 'pygn-mode-display-gui-board-at-pos) #'ignore)
              ((symbol-function 'agent-repl--ws-resolve-persp)
               (lambda (_ws) nil))
              ((symbol-function 'agent-repl--ws-add-buffer)
               (lambda (buf persp switch)
                 (push (list buf persp switch) added-calls)))
              ((symbol-function 'agent-repl--ws-current-name)
               (lambda () "ws1")))
      (let ((buf (agent-repl--handle-send-pgn "ws1" "1. e4 *")))
        (unwind-protect
            (should (null added-calls))
          (kill-buffer buf))))))

(ert-deftest agent-repl-test-handle-send-pgn-renders-board-at-point-min ()
  "handle-send-pgn calls pygn-mode-display-gui-board-at-pos at point-min."
  (let ((board-pos nil))
    (cl-letf (((symbol-function 'pygn-mode) #'ignore)
              ((symbol-function 'display-buffer) #'ignore)
              ((symbol-function 'pygn-mode-display-gui-board-at-pos)
               (lambda (pos) (setq board-pos pos)))
              ((symbol-function 'agent-repl--ws-resolve-persp)
               (lambda (_ws) nil))
              ((symbol-function 'agent-repl--ws-current-name)
               (lambda () "ws1")))
      (let ((buf (agent-repl--handle-send-pgn "ws1" "1. e4 e5 *")))
        (unwind-protect
            (should (= board-pos 1))
          (kill-buffer buf))))))

(ert-deftest agent-repl-test-handle-send-pgn-skips-board-render-when-ws-is-not-current ()
  "handle-send-pgn does NOT render the GUI board when WS is not the active workspace.
Regression guard: the board render is a side effect in the selected
window, so rendering it while another workspace is focused leaks the
board into the wrong window.  It must be gated on ws-active just like
display-buffer."
  (let ((board-calls nil)
        (displayed nil))
    (cl-letf (((symbol-function 'pygn-mode) #'ignore)
              ((symbol-function 'display-buffer)
               (lambda (buf &rest _) (setq displayed buf)))
              ((symbol-function 'pygn-mode-display-gui-board-at-pos)
               (lambda (pos) (push pos board-calls)))
              ((symbol-function 'agent-repl--ws-resolve-persp)
               (lambda (_ws) nil))
              ((symbol-function 'agent-repl--ws-current-name)
               (lambda () "other-ws")))
      (let ((buf (agent-repl--handle-send-pgn "ws1" "1. e4 *")))
        (unwind-protect
            (progn
              (should (null board-calls))
              (should (null displayed)))
          (kill-buffer buf))))))

(ert-deftest agent-repl-test-handle-send-pgn-reuses-existing-buffer ()
  "Calling handle-send-pgn twice for the same workspace reuses the buffer."
  (cl-letf (((symbol-function 'pygn-mode) #'ignore)
            ((symbol-function 'display-buffer) #'ignore)
            ((symbol-function 'pygn-mode-display-gui-board-at-pos) #'ignore)
            ((symbol-function 'agent-repl--ws-resolve-persp)
             (lambda (_ws) nil))
            ((symbol-function 'agent-repl--ws-current-name)
             (lambda () "ws1")))
    (let ((buf1 (agent-repl--handle-send-pgn "ws1" "1. e4 *"))
          (buf2 (agent-repl--handle-send-pgn "ws1" "1. d4 *")))
      (unwind-protect
          (progn
            (should (eq buf1 buf2))
            (with-current-buffer buf2
              (should (string= (buffer-string) "1. d4 *"))))
        (kill-buffer buf1)))))

(ert-deftest agent-repl-test-handle-send-command-dispatches-pgn ()
  "handle-send-command routes data containing a pgn key to handle-send-pgn."
  (let ((pgn-calls nil))
    (cl-letf (((symbol-function 'agent-repl--handle-send-pgn)
               (lambda (ws pgn) (push (cons ws pgn) pgn-calls))))
      (let ((agent-repl--workspaces (make-hash-table :test 'equal)))
        (puthash "ws1" '() agent-repl--workspaces)
        (agent-repl--handle-send-command
         '((type . "send") (workspace . "ws1") (data . ((pgn . "1. e4 *")))))
        (should (equal pgn-calls '(("ws1" . "1. e4 *"))))))))

(ert-deftest agent-repl-test-handle-send-command-skips-pgn-for-empty-string ()
  "handle-send-command does NOT dispatch to handle-send-pgn for an empty pgn string."
  (let ((pgn-calls nil))
    (cl-letf (((symbol-function 'agent-repl--handle-send-pgn)
               (lambda (ws pgn) (push (cons ws pgn) pgn-calls))))
      (let ((agent-repl--workspaces (make-hash-table :test 'equal)))
        (puthash "ws1" '() agent-repl--workspaces)
        (agent-repl--handle-send-command
         '((type . "send") (workspace . "ws1") (data . ((pgn . "")))))
        (should (null pgn-calls))))))

(ert-deftest agent-repl-test-handle-send-command-skips-pgn-for-non-string ()
  "handle-send-command does NOT dispatch to handle-send-pgn when pgn is not a string."
  (let ((pgn-calls nil))
    (cl-letf (((symbol-function 'agent-repl--handle-send-pgn)
               (lambda (ws pgn) (push (cons ws pgn) pgn-calls))))
      (let ((agent-repl--workspaces (make-hash-table :test 'equal)))
        (puthash "ws1" '() agent-repl--workspaces)
        (agent-repl--handle-send-command
         '((type . "send") (workspace . "ws1") (data . ((pgn . 42)))))
        (should (null pgn-calls))))))

(ert-deftest agent-repl-test-handle-send-command-no-pgn-key-no-dispatch ()
  "handle-send-command does NOT dispatch to handle-send-pgn when data lacks pgn key."
  (let ((pgn-calls nil))
    (cl-letf (((symbol-function 'agent-repl--handle-send-pgn)
               (lambda (ws pgn) (push (cons ws pgn) pgn-calls))))
      (let ((agent-repl--workspaces (make-hash-table :test 'equal)))
        (puthash "ws1" '() agent-repl--workspaces)
        (agent-repl--handle-send-command
         '((type . "send") (workspace . "ws1") (data . ((link . "https://x.test")))))
        (should (null pgn-calls))))))

(ert-deftest agent-repl-test-handle-send-command-pgn-still-stores-send-data ()
  "handle-send-command stores :send-data even when PGN dispatch occurs."
  (cl-letf (((symbol-function 'agent-repl--handle-send-pgn) #'ignore))
    (let ((agent-repl--workspaces (make-hash-table :test 'equal)))
      (puthash "ws1" '() agent-repl--workspaces)
      (agent-repl--handle-send-command
       '((type . "send") (workspace . "ws1") (data . ((pgn . "1. e4 *")))))
      (should (equal (agent-repl--ws-get "ws1" :send-data)
                     '((pgn . "1. e4 *")))))))

;;;; ---- Tests: close-workspace ----

(ert-deftest agent-repl-test-close-workspace-delegates-to-nuke ()
  "`--close-workspace' delegates to `--nuke-one-workspace' with the same ws."
  (let ((received :unset))
    (cl-letf (((symbol-function 'agent-repl--nuke-one-workspace)
               (lambda (ws &optional _preserve)
                 (setq received ws))))
      (agent-repl--close-workspace "feature-one")
      (should (equal received "feature-one")))))

(ert-deftest agent-repl-test-close-workspace-default-drops-entry ()
  "`--close-workspace' without PRESERVE-ENTRY passes nil to the nuke primitive.
Standalone close (skill dispatch path) should fully drop the registry
entry — merge's preserve-entry behavior is opt-in only."
  (let ((received-preserve :unset))
    (cl-letf (((symbol-function 'agent-repl--nuke-one-workspace)
               (lambda (_ws &optional preserve)
                 (setq received-preserve preserve))))
      (agent-repl--close-workspace "feature-one")
      (should (null received-preserve)))))

(ert-deftest agent-repl-test-close-workspace-preserve-entry-passes-through ()
  "`--close-workspace' threads PRESERVE-ENTRY to the nuke primitive.
This is the merge-completion path: the hashmap entry must survive close
so the drawer's MERGED bucket can keep rendering until explicit finish."
  (let ((received-preserve :unset))
    (cl-letf (((symbol-function 'agent-repl--nuke-one-workspace)
               (lambda (_ws &optional preserve)
                 (setq received-preserve preserve))))
      (agent-repl--close-workspace "feature-one" 'preserve-entry)
      (should (eq received-preserve 'preserve-entry)))))

;;;; ---- Tests: handle-close-command ----

(ert-deftest agent-repl-test-handle-close-command-invokes-close ()
  "`--handle-close-command' invokes `--close-workspace' with the ws from CMD."
  (let ((received :unset))
    (cl-letf (((symbol-function 'agent-repl--close-workspace)
               (lambda (ws &optional _preserve) (setq received ws))))
      (agent-repl--handle-close-command
       '((type . "close") (workspace . "feature-one")))
      (should (equal received "feature-one")))))

(ert-deftest agent-repl-test-handle-close-command-no-preserve ()
  "`--handle-close-command' does NOT pass `preserve-entry'.
Skill-invoked close fully drops the workspace; preserve-entry is the
merge-completion-only behavior owned by `--workspace-merge-do'."
  (let ((received-preserve :unset))
    (cl-letf (((symbol-function 'agent-repl--close-workspace)
               (lambda (_ws &optional preserve)
                 (setq received-preserve preserve))))
      (agent-repl--handle-close-command
       '((type . "close") (workspace . "feature-one")))
      (should (null received-preserve)))))

(ert-deftest agent-repl-test-handle-close-command-routes-through-gns-gating ()
  "`--handle-close-command' must dispatch via `--gns-sockets-close-then'
so the in-workspace agent is sent `/gns-sockets close' and given a
chance to release sockets before its vterm dies."
  (let ((gating-ws :unset)
        (gating-teardown :unset))
    (cl-letf (((symbol-function 'agent-repl--gns-sockets-close-then)
               (lambda (ws teardown-fn)
                 (setq gating-ws ws
                       gating-teardown teardown-fn))))
      (agent-repl--handle-close-command
       '((type . "close") (workspace . "feature-one")))
      (should (equal gating-ws "feature-one"))
      (should (functionp gating-teardown)))))

(ert-deftest agent-repl-test-handle-close-command-teardown-thunk-closes ()
  "The teardown thunk forwarded to `--gns-sockets-close-then' must call
`--close-workspace' with the workspace name when invoked."
  (let ((received :unset)
        (teardown-fn nil))
    (cl-letf (((symbol-function 'agent-repl--gns-sockets-close-then)
               (lambda (_ws fn) (setq teardown-fn fn)))
              ((symbol-function 'agent-repl--close-workspace)
               (lambda (ws &optional _preserve) (setq received ws))))
      (agent-repl--handle-close-command
       '((type . "close") (workspace . "feature-one")))
      (funcall teardown-fn)
      (should (equal received "feature-one")))))

;;;; ---- Tests: handle-open-command ----

(ert-deftest agent-repl-test-handle-open-command-missing-name-skips ()
  "A missing/empty workspace name skips establish without erroring."
  (let ((established nil))
    (cl-letf (((symbol-function 'agent-repl--establish-workspace)
               (lambda (&rest _) (setq established t))))
      (agent-repl--handle-open-command '((type . "open")))
      (should-not established))))

(ert-deftest agent-repl-test-handle-open-command-unresolvable-dir-skips ()
  "When no on-disk dir resolves, establish is not called."
  (let ((established nil))
    (cl-letf (((symbol-function 'agent-repl--resolve-open-workspace-dir)
               (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--establish-workspace)
               (lambda (&rest _) (setq established t))))
      (agent-repl--handle-open-command
       '((type . "open") (workspace . "DWC/foo")))
      (should-not established))))

(ert-deftest agent-repl-test-handle-open-command-establishes-with-bare-name-and-dir ()
  "A resolved dir triggers establish-workspace with the bare name and dir."
  (let ((received :unset))
    (cl-letf (((symbol-function 'agent-repl--resolve-open-workspace-dir)
               (lambda (&rest _) "/tmp/repo-worktrees/foo"))
              ((symbol-function 'agent-repl--establish-workspace)
               (lambda (ws dir) (setq received (list ws dir)))))
      (agent-repl--handle-open-command
       '((type . "open") (workspace . "DWC/foo")))
      (should (equal received '("foo" "/tmp/repo-worktrees/foo"))))))

(ert-deftest agent-repl-test-handle-open-command-passes-git-root-to-resolver ()
  "CMD's `git_root' is forwarded to the dir resolver."
  (let ((received-root :unset))
    (cl-letf (((symbol-function 'agent-repl--resolve-open-workspace-dir)
               (lambda (_name git-root) (setq received-root git-root) nil))
              ((symbol-function 'agent-repl--establish-workspace)
               (lambda (&rest _) nil)))
      (agent-repl--handle-open-command
       '((type . "open") (workspace . "DWC/foo") (git_root . "/repo")))
      (should (equal received-root "/repo")))))

;;;; ---- Tests: resolve-open-workspace-dir ----

(ert-deftest agent-repl-test-resolve-open-workspace-dir-prefers-registry ()
  "A live registry `:project-dir' wins and is returned verbatim."
  (agent-repl-test--with-clean-state
    (let ((dir (make-temp-file "agent-open-reg" t)))
      (unwind-protect
          (progn
            (puthash "foo" (list :project-dir dir) agent-repl--workspaces)
            (should (equal (agent-repl--resolve-open-workspace-dir "DWC/foo" nil)
                           dir)))
        (delete-directory dir t)))))

(ert-deftest agent-repl-test-resolve-open-workspace-dir-uses-git-root-candidate ()
  "With no registry entry, an existing candidate worktree dir resolves."
  (agent-repl-test--with-clean-state
    (let* ((base (make-temp-file "agent-open-base" t))
           (repo (expand-file-name "repo" base))
           (wt (expand-file-name "foo" (expand-file-name "repo-worktrees" base))))
      (unwind-protect
          (progn
            (make-directory (expand-file-name ".git" repo) t)
            (make-directory wt t)
            (should (equal (agent-repl--path-canonical
                            (agent-repl--resolve-open-workspace-dir "DWC/foo" repo))
                           (agent-repl--path-canonical wt))))
        (delete-directory base t)))))

(ert-deftest agent-repl-test-resolve-open-workspace-dir-nil-when-worktree-gone ()
  "No registry entry and a non-existent candidate dir resolves to nil.
Models a workspace whose worktree was removed by `finish'."
  (agent-repl-test--with-clean-state
    (let* ((base (make-temp-file "agent-open-gone" t))
           (repo (expand-file-name "repo" base)))
      (unwind-protect
          (progn
            (make-directory (expand-file-name ".git" repo) t)
            (should (null (agent-repl--resolve-open-workspace-dir "DWC/foo" repo))))
        (delete-directory base t)))))

(ert-deftest agent-repl-test-resolve-open-workspace-dir-nil-git-root-no-registry ()
  "With nil git-root and no registry entry, resolution is nil."
  (agent-repl-test--with-clean-state
    (should (null (agent-repl--resolve-open-workspace-dir "DWC/foo" nil)))))

(ert-deftest agent-repl-test-resolve-open-workspace-dir-stale-registry-falls-through ()
  "A registry `:project-dir' that no longer exists falls through to git-root."
  (agent-repl-test--with-clean-state
    (let* ((base (make-temp-file "agent-open-stale" t))
           (repo (expand-file-name "repo" base))
           (wt (expand-file-name "foo" (expand-file-name "repo-worktrees" base))))
      (unwind-protect
          (progn
            (make-directory (expand-file-name ".git" repo) t)
            (make-directory wt t)
            (puthash "foo"
                     (list :project-dir (expand-file-name "gone" base))
                     agent-repl--workspaces)
            (should (equal (agent-repl--path-canonical
                            (agent-repl--resolve-open-workspace-dir "DWC/foo" repo))
                           (agent-repl--path-canonical wt))))
        (delete-directory base t)))))

;;;; ---- Tests: gns-sockets-close-then ----

(ert-deftest agent-repl-test-gns-sockets-close-then-no-vterm-runs-teardown-directly ()
  "Without a live vterm buffer, `--gns-sockets-close-then' must run the
teardown thunk immediately — there is no agent to drain."
  (agent-repl-test--with-clean-state
    (puthash "ws" '() agent-repl--workspaces)
    (let ((called nil)
          (sent nil))
      (cl-letf (((symbol-function 'agent-repl--send)
                 (lambda (&rest _) (setq sent t))))
        (agent-repl--gns-sockets-close-then
         "ws" (lambda () (setq called t)))
        (should called)
        (should-not sent)))))

(ert-deftest agent-repl-test-gns-sockets-close-then-not-ready-runs-teardown-directly ()
  "A live vterm buffer that has not yet set `agent-repl--ready' must
still fall through to immediate teardown — the prompt would otherwise
queue on `:pending-prompts' and never drain before close."
  (agent-repl-test--with-clean-state
    (let ((buf (generate-new-buffer " *test-vterm*"))
          (called nil)
          (sent nil))
      (unwind-protect
          (progn
            (with-current-buffer buf
              (setq-local agent-repl--ready nil))
            (puthash "ws" (list :vterm-buffer buf) agent-repl--workspaces)
            (cl-letf (((symbol-function 'agent-repl--send)
                       (lambda (&rest _) (setq sent t))))
              (agent-repl--gns-sockets-close-then
               "ws" (lambda () (setq called t)))
              (should called)
              (should-not sent)))
        (kill-buffer buf)))))

(ert-deftest agent-repl-test-gns-sockets-close-then-ready-sends-prompt ()
  "With a live, ready vterm, `--gns-sockets-close-then' must dispatch
`agent-repl-gns-sockets-close-prompt' via `--send' and defer teardown."
  (agent-repl-test--with-clean-state
    (let ((buf (generate-new-buffer " *test-vterm*"))
          (sent-prompt :unset)
          (sent-ws :unset)
          (teardown-called nil))
      (unwind-protect
          (progn
            (with-current-buffer buf
              (setq-local agent-repl--ready t))
            (puthash "ws" (list :vterm-buffer buf) agent-repl--workspaces)
            (cl-letf (((symbol-function 'agent-repl--send)
                       (lambda (prompt ws &optional _force _on-settle)
                         (setq sent-prompt prompt
                               sent-ws ws)))
                      ((symbol-function 'run-at-time)
                       (lambda (&rest _) nil)))
              (agent-repl--gns-sockets-close-then
               "ws" (lambda () (setq teardown-called t)))
              (should (equal sent-prompt agent-repl-gns-sockets-close-prompt))
              (should (equal sent-ws "ws"))
              (should-not teardown-called)))
        (kill-buffer buf)))))

(ert-deftest agent-repl-test-gns-sockets-close-then-on-settle-schedules-poll ()
  "The `on-settle' callback handed to `--send' must schedule the first
`--gns-sockets-close-poll' via `run-at-time' so the prompt_submit hook
has time to fire before state is polled."
  (agent-repl-test--with-clean-state
    (let ((buf (generate-new-buffer " *test-vterm*"))
          (scheduled-fn :unset)
          (scheduled-delay :unset)
          (captured-on-settle nil))
      (unwind-protect
          (progn
            (with-current-buffer buf
              (setq-local agent-repl--ready t))
            (puthash "ws" (list :vterm-buffer buf) agent-repl--workspaces)
            (cl-letf (((symbol-function 'agent-repl--send)
                       (lambda (_prompt _ws &optional _force on-settle)
                         (setq captured-on-settle on-settle)))
                      ((symbol-function 'run-at-time)
                       (lambda (delay _repeat fn &rest _args)
                         (setq scheduled-delay delay
                               scheduled-fn fn))))
              (agent-repl--gns-sockets-close-then
               "ws" (lambda () nil))
              (should (functionp captured-on-settle))
              (funcall captured-on-settle)
              (should (equal scheduled-delay
                             agent-repl-gns-sockets-close-settle-delay))
              (should (eq scheduled-fn #'agent-repl--gns-sockets-close-poll))))
        (kill-buffer buf)))))

;;;; ---- Tests: gns-sockets-close-poll ----

(ert-deftest agent-repl-test-gns-sockets-close-poll-runs-teardown-on-done ()
  "When `:agent-state' is `:done', the poll must call TEARDOWN-FN
rather than rescheduling."
  (agent-repl-test--with-clean-state
    (puthash "ws" (list :agent-state :done) agent-repl--workspaces)
    (let ((called nil)
          (rescheduled nil))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (&rest _) (setq rescheduled t))))
        (agent-repl--gns-sockets-close-poll
         "ws" (lambda () (setq called t)) (float-time))
        (should called)
        (should-not rescheduled)))))

(ert-deftest agent-repl-test-gns-sockets-close-poll-runs-teardown-on-idle ()
  "`:idle' is also a terminal state for the poll — the workspace has
decayed from `:done' but the turn is still finished, so it is safe to
tear down."
  (agent-repl-test--with-clean-state
    (puthash "ws" (list :agent-state :idle) agent-repl--workspaces)
    (let ((called nil)
          (rescheduled nil))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (&rest _) (setq rescheduled t))))
        (agent-repl--gns-sockets-close-poll
         "ws" (lambda () (setq called t)) (float-time))
        (should called)
        (should-not rescheduled)))))

(ert-deftest agent-repl-test-gns-sockets-close-poll-reschedules-on-thinking ()
  "When the workspace is still `:thinking', the poll must reschedule
itself via `run-at-time' with the configured poll interval and must
NOT call TEARDOWN-FN."
  (agent-repl-test--with-clean-state
    (puthash "ws" (list :agent-state :thinking) agent-repl--workspaces)
    (let ((called nil)
          (rescheduled-delay :unset)
          (rescheduled-fn :unset))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (delay _repeat fn &rest _args)
                   (setq rescheduled-delay delay
                         rescheduled-fn fn))))
        (agent-repl--gns-sockets-close-poll
         "ws" (lambda () (setq called t)) (float-time))
        (should-not called)
        (should (equal rescheduled-delay
                       agent-repl-gns-sockets-close-poll-interval))
        (should (eq rescheduled-fn #'agent-repl--gns-sockets-close-poll))))))

(ert-deftest agent-repl-test-gns-sockets-close-poll-times-out ()
  "Once `agent-repl-gns-sockets-close-timeout' seconds have elapsed
without reaching `:done'/`:idle', the poll must call TEARDOWN-FN
anyway — a hung session must not stall close indefinitely."
  (agent-repl-test--with-clean-state
    (puthash "ws" (list :agent-state :thinking) agent-repl--workspaces)
    (let ((called nil)
          (rescheduled nil)
          (started-at (- (float-time)
                         (+ agent-repl-gns-sockets-close-timeout 1.0))))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (&rest _) (setq rescheduled t))))
        (agent-repl--gns-sockets-close-poll
         "ws" (lambda () (setq called t)) started-at)
        (should called)
        (should-not rescheduled)))))

;;;; ---- Tests: handle-merge-command ----

(ert-deftest agent-repl-test-handle-merge-command-literal-match ()
  "Literal workspace name with a registered :project-dir is forwarded as-is."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "feature-one" :project-dir "/tmp/feature-one")
    (let ((received :unset))
      (cl-letf (((symbol-function 'agent-repl--workspace-merge-into-source)
                 (lambda (ws &optional _silent _auto) (setq received ws))))
        (agent-repl--handle-merge-command
         '((type . "merge") (workspace . "feature-one")))
        (should (equal received "feature-one"))))))

(ert-deftest agent-repl-test-handle-merge-command-falls-back-to-tail ()
  "Branch-style \"DWC/foo\" falls back to \"foo\" when only \"foo\" is registered.
Resolves the bare tail after the last `/' so the spawning agent can send
its branch name verbatim without pre-stripping it."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "foo" :project-dir "/tmp/foo")
    (let ((received :unset))
      (cl-letf (((symbol-function 'agent-repl--workspace-merge-into-source)
                 (lambda (ws &optional _silent _auto) (setq received ws))))
        (agent-repl--handle-merge-command
         '((type . "merge") (workspace . "DWC/foo")))
        (should (equal received "foo"))))))

(ert-deftest agent-repl-test-handle-merge-command-unknown-name-no-crash ()
  "Unknown name (neither literal nor tail registered) does not invoke
the merge and does not crash."
  (agent-repl-test--with-clean-state
    (let ((called nil))
      (cl-letf (((symbol-function 'agent-repl--workspace-merge-into-source)
                 (lambda (&rest _) (setq called t))))
        ;; Must not error.
        (agent-repl--handle-merge-command
         '((type . "merge") (workspace . "bar/baz")))
        (should-not called)))))

(ert-deftest agent-repl-test-handle-merge-command-unknown-name-logs ()
  "Unknown workspace triggers an `unknown workspace' log line that
includes both the literal name and the tail that was tried."
  (agent-repl-test--with-clean-state
    (let ((logged nil))
      (cl-letf (((symbol-function 'agent-repl--log)
                 (lambda (_ws fmt &rest args)
                   (push (apply #'format fmt args) logged)))
                ((symbol-function 'agent-repl--workspace-merge-into-source)
                 (lambda (&rest _) nil)))
        (agent-repl--handle-merge-command
         '((type . "merge") (workspace . "bar/baz")))
        (should (cl-some (lambda (s)
                           (and (string-match-p "unknown workspace: bar/baz" s)
                                (string-match-p "also tried tail baz" s)))
                         logged))))))

(ert-deftest agent-repl-test-handle-merge-command-project-dir-resolves-via-registry ()
  "A merge command carrying `project_dir' resolves to the workspace whose
`:project-dir' matches that path, regardless of the `workspace' field.
This is the bare-tree-branch case: the dispatcher's `workspace' value
is the branch name (e.g. \"DC/foo\") but the registry is keyed by the
repo name (e.g. \"explanation-engine\")."
  (agent-repl-test--with-clean-state
    (let ((dir (make-temp-file "agent-repl-handle-by-dir-" t))
          (received :unset))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-repl--workspace-merge-async)
                     (lambda (ws _root &optional _onto) (setq received ws))))
            (agent-repl--ws-put "explanation-engine" :project-dir dir)
            (agent-repl--handle-merge-command
             `((type . "merge")
               (workspace . "DC/some-branch")
               (project_dir . ,dir)))
            (should (equal received "explanation-engine")))
        (delete-directory dir t)))))

(ert-deftest agent-repl-test-handle-merge-command-project-dir-miss-falls-back-to-name ()
  "When `project_dir' doesn't match any live workspace, the handler
falls back to `workspace' name resolution so partial/typo'd paths
don't block an otherwise-resolvable name."
  (agent-repl-test--with-clean-state
    (let ((received :unset))
      (cl-letf (((symbol-function 'agent-repl--workspace-merge-async)
                 (lambda (ws _root &optional _onto) (setq received ws))))
        (agent-repl--ws-put "feature-one" :project-dir "/tmp/feature-one")
        (agent-repl--handle-merge-command
         '((type . "merge")
           (workspace . "feature-one")
           (project_dir . "/nonexistent/path")))
        (should (equal received "feature-one"))))))

(ert-deftest agent-repl-test-handle-merge-command-unknown-logs-include-project-dir ()
  "When the merge can't be resolved and a `project_dir' was supplied,
the unknown-workspace log line includes the attempted path so the
operator can see both the name and the path that missed."
  (agent-repl-test--with-clean-state
    (let ((logged nil))
      (cl-letf (((symbol-function 'agent-repl--log)
                 (lambda (_ws fmt &rest args)
                   (push (apply #'format fmt args) logged)))
                ((symbol-function 'agent-repl--workspace-merge-into-source)
                 (lambda (&rest _) nil)))
        (agent-repl--handle-merge-command
         '((type . "merge")
           (workspace . "bar/baz")
           (project_dir . "/nonexistent/path")))
        (should (cl-some (lambda (s)
                           (and (string-match-p "unknown workspace: bar/baz" s)
                                (string-match-p "also tried project_dir /nonexistent/path" s)))
                         logged))))))

(ert-deftest agent-repl-test-handle-merge-command-runs-silently ()
  "Skill-invoked merges (`/workspace-merge') must pass SILENT=t to
workspace-merge-into-source so the merge does not steal user focus.
Interactive entries (`SPC TAB m'/`SPC TAB M') leave SILENT nil and
retain the old switch-to-project + magit pop behavior."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "feature-one" :project-dir "/tmp/feature-one")
    (let ((silent-arg :unset))
      (cl-letf (((symbol-function 'agent-repl--workspace-merge-into-source)
                 (lambda (_ws &optional silent _auto) (setq silent-arg silent))))
        (agent-repl--handle-merge-command
         '((type . "merge") (workspace . "feature-one")))
        (should (eq silent-arg t))))))

;;;; ---- Tests: ws-merge-routing-root ----

(ert-deftest agent-repl-test-ws-merge-routing-root-prefers-source-dir ()
  "Routing root prefers :source-ws-dir when it is a live directory."
  (agent-repl-test--with-clean-state
    (let ((src-dir (make-temp-file "agent-repl-routing-src-" t))
          (own-dir (make-temp-file "agent-repl-routing-own-" t)))
      (unwind-protect
          (progn
            (agent-repl--ws-put "ws1" :project-dir own-dir)
            (agent-repl--ws-put "ws1" :source-ws-dir src-dir)
            (should (equal (agent-repl--ws-merge-routing-root "ws1") src-dir)))
        (delete-directory src-dir t)
        (delete-directory own-dir t)))))

(ert-deftest agent-repl-test-ws-merge-routing-root-falls-back-to-project-dir ()
  "Routing root falls back to :project-dir when :source-ws-dir is nil."
  (agent-repl-test--with-clean-state
    (let ((own-dir (make-temp-file "agent-repl-routing-own-" t)))
      (unwind-protect
          (progn
            (agent-repl--ws-put "ws1" :project-dir own-dir)
            (should (equal (agent-repl--ws-merge-routing-root "ws1") own-dir)))
        (delete-directory own-dir t)))))

(ert-deftest agent-repl-test-ws-merge-routing-root-falls-back-when-source-missing ()
  "When :source-ws-dir is set but the directory doesn't exist, falls back to :project-dir."
  (agent-repl-test--with-clean-state
    (let ((own-dir (make-temp-file "agent-repl-routing-own-" t)))
      (unwind-protect
          (progn
            (agent-repl--ws-put "ws1" :project-dir own-dir)
            (agent-repl--ws-put "ws1" :source-ws-dir "/nonexistent/dir")
            (should (equal (agent-repl--ws-merge-routing-root "ws1") own-dir)))
        (delete-directory own-dir t)))))

(ert-deftest agent-repl-test-ws-merge-routing-root-nil-when-neither ()
  "When neither :source-ws-dir nor :project-dir resolves, returns nil."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir nil)
    (should-not (agent-repl--ws-merge-routing-root "ws1"))))

;;;; ---- Tests: handle-merge-command dispatch via registry ----

(ert-deftest agent-repl-test-handle-merge-command-dispatches-via-registry ()
  "handle-merge-command routes through `agent-repl--dispatch-merge-handler'.
Mocks the dispatcher and verifies it receives the resolved ws + routing root."
  (agent-repl-test--with-clean-state
    (let ((src-dir (make-temp-file "agent-repl-dispatch-src-" t))
          (captured nil))
      (unwind-protect
          (progn
            (agent-repl--ws-put "feature-one" :project-dir "/tmp/feature-one")
            (agent-repl--ws-put "feature-one" :source-ws-dir src-dir)
            (cl-letf (((symbol-function 'agent-repl--dispatch-merge-handler)
                       (lambda (ws root &optional _onto) (setq captured (list ws root)))))
              (agent-repl--handle-merge-command
               '((type . "merge") (workspace . "feature-one")))
              (should (equal (car captured) "feature-one"))
              (should (equal (cadr captured) src-dir))))
        (delete-directory src-dir t)))))

(ert-deftest agent-repl-test-handle-merge-command-routes-through-async-wrapper ()
  "Skill-invoked merges go through `agent-repl--workspace-merge-async' —
the SAME wrapper the interactive `SPC TAB M' path uses, so there is no
behavioral difference between the two callers.  Both close the workspace
UI, run the merge on a worker thread, and reopen on failure."
  (agent-repl-test--with-clean-state
    (let ((src-dir (make-temp-file "agent-repl-async-sentinel-" t))
          (async-args :unset))
      (unwind-protect
          (progn
            (agent-repl--ws-put "feature-one" :project-dir "/tmp/feature-one")
            (agent-repl--ws-put "feature-one" :source-ws-dir src-dir)
            (cl-letf (((symbol-function 'agent-repl--workspace-merge-async)
                       (lambda (ws root &optional _onto) (setq async-args (list ws root)))))
              (agent-repl--handle-merge-command
               '((type . "merge") (workspace . "feature-one")))
              (should (equal (car async-args) "feature-one"))
              (should (equal (cadr async-args) src-dir))))
        (delete-directory src-dir t)))))

(ert-deftest agent-repl-test-handle-merge-command-threads-onto-master ()
  "A merge command's `pr_was_merged' field is forwarded to
`--workspace-merge-async' (and defaults to nil when the field is absent)."
  (agent-repl-test--with-clean-state
    (let ((onto :unset))
      (cl-letf (((symbol-function 'agent-repl--workspace-merge-async)
                 (lambda (_ws _root &optional onto-master) (setq onto onto-master))))
        (agent-repl--ws-put "feature-one" :project-dir "/tmp/feature-one")
        (agent-repl--handle-merge-command
         '((type . "merge") (workspace . "feature-one") (pr_was_merged . t)))
        (should (eq onto t))
        (agent-repl--handle-merge-command
         '((type . "merge") (workspace . "feature-one")))
        (should (eq onto nil))))))

;;;; ---- Tests: resolve-merge-workspace-name ----

(ert-deftest agent-repl-test-resolve-merge-workspace-name-literal ()
  "Literal name with a :project-dir entry returns the literal name."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "foo" :project-dir "/tmp/foo")
    (should (equal (agent-repl--resolve-merge-workspace-name "foo") "foo"))))

(ert-deftest agent-repl-test-resolve-merge-workspace-name-tail-fallback ()
  "Branch-style name returns the tail when only the tail is registered."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "foo" :project-dir "/tmp/foo")
    (should (equal (agent-repl--resolve-merge-workspace-name "DWC/foo") "foo"))))

(ert-deftest agent-repl-test-resolve-merge-workspace-name-literal-wins-over-tail ()
  "When both the literal name and the tail are registered, literal wins."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "DWC/foo" :project-dir "/tmp/DWC-foo")
    (agent-repl--ws-put "foo" :project-dir "/tmp/foo")
    (should (equal (agent-repl--resolve-merge-workspace-name "DWC/foo") "DWC/foo"))))

(ert-deftest agent-repl-test-resolve-merge-workspace-name-miss ()
  "Returns nil when neither the literal name nor the tail is registered."
  (agent-repl-test--with-clean-state
    (should (null (agent-repl--resolve-merge-workspace-name "bar/baz")))))

(ert-deftest agent-repl-test-resolve-merge-workspace-name-no-slash-miss ()
  "Returns nil for an unregistered bare name (no `/' to fall back from)."
  (agent-repl-test--with-clean-state
    (should (null (agent-repl--resolve-merge-workspace-name "nope")))))

(ert-deftest agent-repl-test-resolve-merge-workspace-name-project-dir-match ()
  "When PROJECT-DIR resolves to a live workspace, it wins regardless of WS.
This is the exact case that bites bare-tree branches: the registry uses
the bare repo name (e.g. \"explanation-engine\") while the dispatcher's
WS is the branch name (e.g. \"DC/foo\"). The project-dir lookup bridges
the two without either side having to agree on a name."
  (agent-repl-test--with-clean-state
    (let ((dir (make-temp-file "agent-repl-merge-by-dir-" t)))
      (unwind-protect
          (progn
            (agent-repl--ws-put "explanation-engine" :project-dir dir)
            (should (equal (agent-repl--resolve-merge-workspace-name
                            "DC/some-feature-branch" dir)
                           "explanation-engine")))
        (delete-directory dir t)))))

(ert-deftest agent-repl-test-resolve-merge-workspace-name-project-dir-wins-over-name ()
  "PROJECT-DIR resolution beats name resolution even when both would hit.
Defensive: if a caller happens to supply both and they point at
different workspaces, the directory match is the authoritative one
(directories are unique per live workspace, names are not always)."
  (agent-repl-test--with-clean-state
    (let ((dir (make-temp-file "agent-repl-merge-by-dir-wins-" t)))
      (unwind-protect
          (progn
            (agent-repl--ws-put "by-dir" :project-dir dir)
            (agent-repl--ws-put "by-name" :project-dir "/tmp/by-name")
            (should (equal (agent-repl--resolve-merge-workspace-name "by-name" dir)
                           "by-dir")))
        (delete-directory dir t)))))

(ert-deftest agent-repl-test-resolve-merge-workspace-name-project-dir-miss-falls-back-to-name ()
  "When PROJECT-DIR is non-nil but doesn't match a registered workspace,
the resolver falls back to the WS literal/tail path so a stale or
mistyped path doesn't shadow an otherwise-resolvable name."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "foo" :project-dir "/tmp/foo")
    (should (equal (agent-repl--resolve-merge-workspace-name
                    "foo" "/nonexistent/path")
                   "foo"))))

(ert-deftest agent-repl-test-resolve-merge-workspace-name-empty-project-dir-ignored ()
  "Empty string PROJECT-DIR is treated as absent (resolver skips dir
lookup and falls straight through to name resolution).
The dispatcher emits empty strings rather than null when the cwd
detection fails; the resolver must not pass those into the dir lookup
or it would canonicalize \"\" and risk a spurious match."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "foo" :project-dir "/tmp/foo")
    (should (equal (agent-repl--resolve-merge-workspace-name "foo" "")
                   "foo"))))

(ert-deftest agent-repl-test-resolve-merge-workspace-name-project-dir-canonicalizes ()
  "PROJECT-DIR resolution canonicalizes paths so a trailing slash or
relative `..' component in the dispatcher's value still matches a
registered canonical `:project-dir'."
  (agent-repl-test--with-clean-state
    (let ((dir (make-temp-file "agent-repl-merge-canon-" t)))
      (unwind-protect
          (progn
            (agent-repl--ws-put "ws" :project-dir dir)
            (should (equal (agent-repl--resolve-merge-workspace-name
                            "ignored" (concat dir "/"))
                           "ws")))
        (delete-directory dir t)))))

;;;; ---- Tests: process-workspace-commands-file ----

(ert-deftest agent-repl-test-process-workspace-commands-file-missing ()
  "Missing file is handled gracefully (no error, just logged)."
  (agent-repl--process-workspace-commands-file "/nonexistent/file.json"))

(ert-deftest agent-repl-test-process-workspace-commands-file-creates-stagger ()
  "Multiple create commands get staggered delays."
  (let ((delays nil)
        (tmpfile (make-temp-file "ws-cmd-" nil ".json")))
    (unwind-protect
        (progn
          ;; Write a file with two create commands
          (with-temp-file tmpfile
            (insert "[{\"type\":\"create\",\"name\":\"ws1\"},{\"type\":\"create\",\"name\":\"ws2\"}]"))
          (cl-letf (((symbol-function 'agent-repl--handle-create-command)
                     (lambda (_cmd delay) (push delay delays))))
            (agent-repl--process-workspace-commands-file tmpfile))
          ;; Delays should be 0 and stagger-seconds
          (should (equal (reverse delays) (list 0 agent-repl-worktree-stagger-seconds)))
          ;; File should be deleted
          (should-not (file-exists-p tmpfile)))
      (when (file-exists-p tmpfile) (delete-file tmpfile)))))

(ert-deftest agent-repl-test-process-workspace-commands-file-mixed ()
  "Mixed create/prompt/finish commands dispatch correctly."
  (let ((create-count 0) (prompt-count 0) (finish-count 0)
        (tmpfile (make-temp-file "ws-cmd-" nil ".json")))
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (insert "[{\"type\":\"create\",\"name\":\"ws1\"},{\"type\":\"prompt\",\"workspace\":\"ws1\",\"prompt\":\"hi\"},{\"type\":\"finish\",\"workspace\":\"ws1\"}]"))
          (cl-letf (((symbol-function 'agent-repl--handle-create-command)
                     (lambda (_cmd _delay) (cl-incf create-count)))
                    ((symbol-function 'agent-repl--handle-prompt-command)
                     (lambda (_cmd) (cl-incf prompt-count)))
                    ((symbol-function 'agent-repl--handle-finish-command)
                     (lambda (_cmd) (cl-incf finish-count))))
            (agent-repl--process-workspace-commands-file tmpfile))
          (should (= create-count 1))
          (should (= prompt-count 1))
          (should (= finish-count 1)))
      (when (file-exists-p tmpfile) (delete-file tmpfile)))))

;;;; ---- Tests: random-disambiguator-suffix ----

(ert-deftest agent-repl-test-random-disambiguator-suffix-length ()
  "Suffix is exactly 3 characters."
  (should (= 3 (length (agent-repl--random-disambiguator-suffix)))))

(ert-deftest agent-repl-test-random-disambiguator-suffix-lowercase ()
  "Suffix contains only lowercase a-z."
  (let ((suffix (agent-repl--random-disambiguator-suffix)))
    (should (string-match-p "\\`[a-z]\\{3\\}\\'" suffix))))

;;;; ---- Tests: workspace-name-collides-p ----

(ert-deftest agent-repl-test-workspace-name-collides-p-fresh ()
  "Fresh name in a clean repo and empty workspaces hash → no collision."
  (let ((agent-repl--workspaces (make-hash-table :test 'equal))
        (agent-repl--workspace-names-in-flight nil)
        (agent-repl-worktree-start-tag-prefix nil))
    (cl-letf (((symbol-function 'agent-repl--candidate-worktree-path)
               (lambda (&rest _args) "/tmp/repo-worktrees/fresh"))
              ((symbol-function 'file-directory-p) (lambda (_p) nil))
              ((symbol-function 'agent-repl--git-branch-exists-p)
               (lambda (_root _branch) nil)))
      (should-not (agent-repl--workspace-name-collides-p "DWC/fresh" "/tmp/repo")))))

(ert-deftest agent-repl-test-workspace-name-collides-p-in-flight ()
  "Name already reserved in `agent-repl--workspace-names-in-flight' → collision."
  (let ((agent-repl--workspaces (make-hash-table :test 'equal))
        (agent-repl--workspace-names-in-flight
         (let ((h (make-hash-table :test 'equal)))
           (puthash "DWC/dup" t h)
           h))
        (agent-repl-worktree-start-tag-prefix nil))
    ;; The production function eagerly computes the candidate path in
    ;; its `let*' before any short-circuit check, so
    ;; `agent-repl--candidate-worktree-path' DOES run and must return
    ;; a string.  But after the in-flight hit wins, no other probe
    ;; (file-directory-p, --git-exit-code) should fire — those stay
    ;; error-on-call.
    (cl-letf (((symbol-function 'agent-repl--candidate-worktree-path)
               (lambda (&rest _args) "/tmp/repo-worktrees/dup"))
              ((symbol-function 'file-directory-p)
               (lambda (&rest _args) (error "unexpected file-directory-p call")))
              ((symbol-function 'agent-repl--git-exit-code)
               (lambda (&rest _args) (error "unexpected git-exit-code call"))))
      (should (agent-repl--workspace-name-collides-p "DWC/dup" "/tmp/repo")))))

(ert-deftest agent-repl-test-workspace-name-collides-p-workspaces-hash ()
  "Bare name present in `agent-repl--workspaces' → collision."
  (let ((agent-repl--workspaces (make-hash-table :test 'equal))
        (agent-repl--workspace-names-in-flight nil)
        (agent-repl-worktree-start-tag-prefix nil))
    ;; Hash table keyed by bare name (matches `(+workspace-current-name)' style).
    (puthash "existing" '(:project-dir "/tmp/x") agent-repl--workspaces)
    (cl-letf (((symbol-function 'agent-repl--candidate-worktree-path)
               (lambda (&rest _args) "/tmp/repo-worktrees/existing"))
              ((symbol-function 'agent-repl--git-exit-code)
               (lambda (&rest _args) (error "unexpected git-exit-code call"))))
      (should (agent-repl--workspace-name-collides-p "DWC/existing" "/tmp/repo")))))

(ert-deftest agent-repl-test-workspace-name-collides-p-on-disk-path ()
  "Existing on-disk path at the resolved worktree dir → collision."
  (let ((agent-repl--workspaces (make-hash-table :test 'equal))
        (agent-repl--workspace-names-in-flight nil)
        (agent-repl-worktree-start-tag-prefix nil))
    (cl-letf (((symbol-function 'agent-repl--candidate-worktree-path)
               (lambda (&rest _args) "/tmp/repo-worktrees/ondisk"))
              ((symbol-function 'file-directory-p)
               (lambda (p) (equal p "/tmp/repo-worktrees/ondisk")))
              ((symbol-function 'agent-repl--git-exit-code)
               (lambda (&rest _args) (error "unexpected git-exit-code call"))))
      (should (agent-repl--workspace-name-collides-p "DWC/ondisk" "/tmp/repo")))))

(ert-deftest agent-repl-test-workspace-name-collides-p-git-branch ()
  "Existing git branch in repo → collision."
  (let ((agent-repl--workspaces (make-hash-table :test 'equal))
        (agent-repl--workspace-names-in-flight nil)
        (agent-repl-worktree-start-tag-prefix nil))
    (cl-letf (((symbol-function 'agent-repl--candidate-worktree-path)
               (lambda (&rest _args) "/tmp/repo-worktrees/existing-branch"))
              ((symbol-function 'file-directory-p) (lambda (_p) nil))
              ;; Only the branch-existence probe is reached here; branch exists.
              ((symbol-function 'agent-repl--git-branch-exists-p)
               (lambda (_root _branch) t)))
      (should (agent-repl--workspace-name-collides-p "DWC/existing-branch" "/tmp/repo")))))

(ert-deftest agent-repl-test-workspace-name-collides-p-start-tag ()
  "Existing start-tag for the resolved branch → collision."
  (let ((agent-repl--workspaces (make-hash-table :test 'equal))
        (agent-repl--workspace-names-in-flight nil)
        (agent-repl-worktree-start-tag-prefix "start/"))
    (cl-letf (((symbol-function 'agent-repl--candidate-worktree-path)
               (lambda (&rest _args) "/tmp/repo-worktrees/has-tag"))
              ((symbol-function 'file-directory-p) (lambda (_p) nil))
              ;; Branch does not exist.
              ((symbol-function 'agent-repl--git-branch-exists-p)
               (lambda (_root _branch) nil))
              ;; Start tag DOES exist.
              ((symbol-function 'agent-repl--git-tag-exists-p)
               (lambda (_root _tag) t)))
      (should (agent-repl--workspace-name-collides-p "DWC/has-tag" "/tmp/repo")))))

(ert-deftest agent-repl-test-workspace-name-collides-p-tag-ignored-when-prefix-nil ()
  "When start-tag prefix is nil, a stray `start/<branch>' tag does not flag collision."
  (let ((agent-repl--workspaces (make-hash-table :test 'equal))
        (agent-repl--workspace-names-in-flight nil)
        (agent-repl-worktree-start-tag-prefix nil))
    (cl-letf (((symbol-function 'agent-repl--candidate-worktree-path)
               (lambda (&rest _args) "/tmp/repo-worktrees/no-tag-check"))
              ((symbol-function 'file-directory-p) (lambda (_p) nil))
              ;; Branch probe is the only predicate reached when prefix is nil.
              ((symbol-function 'agent-repl--git-branch-exists-p)
               (lambda (_root _branch) nil))
              ((symbol-function 'agent-repl--git-tag-exists-p)
               (lambda (&rest _args)
                 (error "start-tag check should be skipped when prefix is nil"))))
      (should-not (agent-repl--workspace-name-collides-p "DWC/no-tag-check" "/tmp/repo")))))

;;;; ---- Tests: disambiguate-workspace-name ----

(ert-deftest agent-repl-test-disambiguate-workspace-name-no-collision ()
  "When the name does not collide, it is returned unchanged (no suffix)."
  (cl-letf (((symbol-function 'agent-repl--workspace-name-collides-p)
             (lambda (&rest _args) nil)))
    (should (equal "DWC/clean"
                   (agent-repl--disambiguate-workspace-name "DWC/clean" "/tmp/repo")))))

(ert-deftest agent-repl-test-disambiguate-workspace-name-collides-appends-suffix ()
  "When the name collides, the result is `NAME-XYZ' with a 3-char suffix."
  ;; First call (bare name) collides, subsequent suffixed candidates do not.
  (let ((call-count 0))
    (cl-letf (((symbol-function 'agent-repl--workspace-name-collides-p)
               (lambda (name &rest _args)
                 (cl-incf call-count)
                 (equal name "DWC/taken"))))
      (let ((result (agent-repl--disambiguate-workspace-name "DWC/taken" "/tmp/repo")))
        (should (string-match-p "\\`DWC/taken-[a-z]\\{3\\}\\'" result))))))

(ert-deftest agent-repl-test-disambiguate-workspace-name-errors-when-max-attempts-exceeded ()
  "When every candidate keeps colliding, an error is signaled.
Simulated by stubbing `agent-repl--workspace-name-collides-p' to always
return t — the loop must exit and `error' rather than spin forever or
silently return a colliding name."
  (let ((agent-repl-workspace-name-disambiguate-max-attempts 3))
    (cl-letf (((symbol-function 'agent-repl--workspace-name-collides-p)
               (lambda (&rest _args) t)))
      (should-error (agent-repl--disambiguate-workspace-name "DWC/x" "/tmp/repo")))))

;;;; ---- Tests: handle-create-command disambiguation integration ----

(ert-deftest agent-repl-test-handle-create-command-passes-clean-name-through ()
  "When the desired name does not collide, the timer is scheduled with the original name."
  (let ((agent-repl--workspaces (make-hash-table :test 'equal))
        (agent-repl--workspace-names-in-flight (make-hash-table :test 'equal))
        (agent-repl-worktree-start-tag-prefix nil)
        (scheduled-args nil))
    (cl-letf (((symbol-function 'agent-repl--workspace-name-collides-p)
               (lambda (&rest _args) nil))
              ((symbol-function 'run-with-timer)
               (lambda (_delay _repeat _fn &rest args)
                 (setq scheduled-args args))))
      (agent-repl--handle-create-command
       `((type . "create") (name . "DWC/clean") (git_root . "/tmp/repo"))
       0)
      ;; Args are (git-root name prompt priority fork-session-id base-commit)
      (should (equal "DWC/clean" (nth 1 scheduled-args))))))

(ert-deftest agent-repl-test-handle-create-command-forwards-model-from-json ()
  "A `model' field in the create JSON is scheduled as the 8th timer arg."
  (let ((agent-repl--workspaces (make-hash-table :test 'equal))
        (agent-repl--workspace-names-in-flight (make-hash-table :test 'equal))
        (agent-repl-worktree-start-tag-prefix nil)
        (scheduled-args nil))
    (cl-letf (((symbol-function 'agent-repl--workspace-name-collides-p)
               (lambda (&rest _args) nil))
              ((symbol-function 'run-with-timer)
               (lambda (_delay _repeat _fn &rest args)
                 (setq scheduled-args args))))
      (agent-repl--handle-create-command
       `((type . "create") (name . "DWC/mdl") (git_root . "/tmp/repo") (model . "sonnet"))
       0)
      ;; Args are (git-root name prompt priority fork-session-id base-commit force-sandbox model)
      (should (equal "sonnet" (nth 7 scheduled-args))))))

(ert-deftest agent-repl-test-handle-create-command-model-nil-when-absent ()
  "When the create JSON omits `model', the scheduled model arg is nil."
  (let ((agent-repl--workspaces (make-hash-table :test 'equal))
        (agent-repl--workspace-names-in-flight (make-hash-table :test 'equal))
        (agent-repl-worktree-start-tag-prefix nil)
        (scheduled-args nil))
    (cl-letf (((symbol-function 'agent-repl--workspace-name-collides-p)
               (lambda (&rest _args) nil))
              ((symbol-function 'run-with-timer)
               (lambda (_delay _repeat _fn &rest args)
                 (setq scheduled-args args))))
      (agent-repl--handle-create-command
       `((type . "create") (name . "DWC/nomdl") (git_root . "/tmp/repo"))
       0)
      (should (null (nth 7 scheduled-args))))))

(ert-deftest agent-repl-test-handle-create-command-model-nil-when-empty-string ()
  "An empty-string `model' field is normalized to nil (falls back to default)."
  (let ((agent-repl--workspaces (make-hash-table :test 'equal))
        (agent-repl--workspace-names-in-flight (make-hash-table :test 'equal))
        (agent-repl-worktree-start-tag-prefix nil)
        (scheduled-args nil))
    (cl-letf (((symbol-function 'agent-repl--workspace-name-collides-p)
               (lambda (&rest _args) nil))
              ((symbol-function 'run-with-timer)
               (lambda (_delay _repeat _fn &rest args)
                 (setq scheduled-args args))))
      (agent-repl--handle-create-command
       `((type . "create") (name . "DWC/emptymdl") (git_root . "/tmp/repo") (model . ""))
       0)
      (should (null (nth 7 scheduled-args))))))

(ert-deftest agent-repl-test-handle-create-command-disambiguates-collision ()
  "When the desired name collides (existing branch), the timer is scheduled with a suffixed name."
  (let ((agent-repl--workspaces (make-hash-table :test 'equal))
        (agent-repl--workspace-names-in-flight (make-hash-table :test 'equal))
        (agent-repl-worktree-start-tag-prefix nil)
        (scheduled-args nil))
    (cl-letf (((symbol-function 'agent-repl--workspace-name-collides-p)
               (lambda (name &rest _args)
                 ;; Only the bare name collides; suffixed variants do not.
                 (equal name "DWC/taken")))
              ((symbol-function 'run-with-timer)
               (lambda (_delay _repeat _fn &rest args)
                 (setq scheduled-args args))))
      (agent-repl--handle-create-command
       `((type . "create") (name . "DWC/taken") (git_root . "/tmp/repo"))
       0)
      (should (string-match-p "\\`DWC/taken-[a-z]\\{3\\}\\'"
                              (nth 1 scheduled-args))))))

(ert-deftest agent-repl-test-handle-create-command-reserves-name-in-flight ()
  "After scheduling, the effective name is recorded in the in-flight hash so siblings see it."
  (let ((agent-repl--workspaces (make-hash-table :test 'equal))
        (agent-repl--workspace-names-in-flight (make-hash-table :test 'equal))
        (agent-repl-worktree-start-tag-prefix nil))
    (cl-letf (((symbol-function 'agent-repl--workspace-name-collides-p)
               (lambda (&rest _args) nil))
              ((symbol-function 'run-with-timer)
               (lambda (&rest _args) nil)))
      (agent-repl--handle-create-command
       `((type . "create") (name . "DWC/sibling") (git_root . "/tmp/repo"))
       0))
    (should (gethash "DWC/sibling" agent-repl--workspace-names-in-flight))))

(ert-deftest agent-repl-test-handle-create-command-second-sibling-gets-suffix ()
  "Two sibling creates in the same batch with the same name yield distinct effective names."
  (let ((agent-repl--workspaces (make-hash-table :test 'equal))
        (agent-repl--workspace-names-in-flight (make-hash-table :test 'equal))
        (agent-repl-worktree-start-tag-prefix nil)
        (scheduled-names nil))
    ;; First create: name is collision-free.  The handler reserves it in
    ;; `--workspace-names-in-flight'.  Second create with the same name
    ;; consults that table via the real `--workspace-name-collides-p',
    ;; so it MUST find the prior reservation and disambiguate.  We mock
    ;; only the slower path probes (path/branch/start-tag) and let the
    ;; in-flight check fall through to the real implementation.
    (cl-letf (((symbol-function 'agent-repl--candidate-worktree-path)
               (lambda (&rest _args) "/tmp/repo-worktrees/dup"))
              ((symbol-function 'file-directory-p) (lambda (_p) nil))
              ((symbol-function 'agent-repl--git-branch-exists-p)
               (lambda (_root _branch) nil))
              ((symbol-function 'run-with-timer)
               (lambda (_delay _repeat _fn &rest args)
                 (push (nth 1 args) scheduled-names))))
      (agent-repl--handle-create-command
       `((type . "create") (name . "DWC/dup") (git_root . "/tmp/repo"))
       0)
      (agent-repl--handle-create-command
       `((type . "create") (name . "DWC/dup") (git_root . "/tmp/repo"))
       5))
    (setq scheduled-names (nreverse scheduled-names))
    (should (= 2 (length scheduled-names)))
    (should (equal "DWC/dup" (nth 0 scheduled-names)))
    (should (string-match-p "\\`DWC/dup-[a-z]\\{3\\}\\'" (nth 1 scheduled-names)))
    (should-not (equal (nth 0 scheduled-names) (nth 1 scheduled-names)))))

;;;; ---- Tests: worktree-add-callback ----

(ert-deftest agent-repl-test-worktree-add-callback-failure ()
  "When git worktree add fails, callback is not called and error message is shown."
  (let ((finalized nil))
    (cl-letf (((symbol-function 'agent-repl--finalize-worktree-workspace)
               (lambda (&rest _args) (setq finalized t))))
      (agent-repl--worktree-add-callback
       "/tmp/path" "dirname" nil nil nil nil nil nil nil nil nil "git error output")
      (should-not finalized))))

(ert-deftest agent-repl-test-worktree-add-callback-success ()
  "When git worktree add succeeds, finalize is called."
  (let ((finalized nil))
    (cl-letf (((symbol-function 'agent-repl--finalize-worktree-workspace)
               (lambda (path dirname prompt priority fork-id bare-metal _cb &optional source-dir no-agent model)
                 (setq finalized (list path dirname prompt priority fork-id bare-metal source-dir no-agent model)))))
      (agent-repl--worktree-add-callback
       "/tmp/path" "dirname" "prompt" 5 "fork-123" nil nil "/src/dir" nil "sonnet" t "ok")
      (should (equal finalized '("/tmp/path" "dirname" "prompt" 5 "fork-123" nil "/src/dir" nil "sonnet"))))))

(ert-deftest agent-repl-test-worktree-add-callback-forwards-no-agent ()
  "NO-AGENT is forwarded to `agent-repl--finalize-worktree-workspace'."
  (let ((captured :unset))
    (cl-letf (((symbol-function 'agent-repl--finalize-worktree-workspace)
               (lambda (_path _dirname _prompt _priority _fork _bm _cb &optional _src no-agent _model)
                 (setq captured no-agent))))
      (agent-repl--worktree-add-callback
       "/tmp/path" "dirname" nil nil nil nil nil "/src/dir" t nil t "ok")
      (should (eq captured t)))))

(ert-deftest agent-repl-test-worktree-add-callback-forwards-model ()
  "MODEL is forwarded to `agent-repl--finalize-worktree-workspace'."
  (let ((captured :unset))
    (cl-letf (((symbol-function 'agent-repl--finalize-worktree-workspace)
               (lambda (_path _dirname _prompt _priority _fork _bm _cb &optional _src _no-agent model)
                 (setq captured model))))
      (agent-repl--worktree-add-callback
       "/tmp/path" "dirname" nil nil nil nil nil "/src/dir" nil "haiku" t "ok")
      (should (equal captured "haiku")))))

;;;; ---- Tests: worktree-fetch-callback ----

(ert-deftest agent-repl-test-worktree-fetch-callback-calls-add-fn ()
  "Fetch callback invokes the add-fn regardless of success."
  (let ((called nil))
    (agent-repl--worktree-fetch-callback (lambda () (setq called t)) nil "output")
    (should called)))

(ert-deftest agent-repl-test-worktree-fetch-callback-calls-add-fn-on-failure ()
  "Fetch callback invokes add-fn even when fetch fails."
  (let ((called nil))
    (agent-repl--worktree-fetch-callback (lambda () (setq called t)) nil "error")
    (should called)))

;;;; ---- Tests: worktree-fetch-master-callback ----

(ert-deftest agent-repl-test-worktree-fetch-master-callback-calls-ff-then-add-fn ()
  "Master fetch callback calls ff-master with git-root, then invokes add-fn."
  (let ((ff-called-with nil)
        (add-called nil))
    (cl-letf (((symbol-function 'agent-repl--maybe-fast-forward-master)
               (lambda (root) (setq ff-called-with root))))
      (agent-repl--worktree-fetch-master-callback
       (lambda () (setq add-called t)) "/some/root" t "output"))
    (should (equal ff-called-with "/some/root"))
    (should add-called)))

(ert-deftest agent-repl-test-worktree-fetch-master-callback-calls-add-fn-on-failure ()
  "Master fetch callback still calls add-fn when fetch reports failure."
  (let ((add-called nil))
    (cl-letf (((symbol-function 'agent-repl--maybe-fast-forward-master)
               (lambda (_root) nil)))
      (agent-repl--worktree-fetch-master-callback
       (lambda () (setq add-called t)) "/some/root" nil "error"))
    (should add-called)))

;;;; ---- Tests: maybe-fast-forward-master ----
;;
;; The production function calls four boundary primitives:
;; - `agent-repl--git-exit-code' for: verify origin-ref, branch-exists-p,
;;   merge-base --is-ancestor, merge --ff-only (when wt is on master), and
;;   update-ref (when wt is not on master).
;; - `agent-repl--git-string' for `rev-parse <branch>' and `rev-parse <origin>'.
;; - `agent-repl--master-worktree-path' to discover the master worktree, if any.
;; All four are stubbed so the tests exercise the dispatch logic, not git.

(ert-deftest agent-repl-test-maybe-ff-master-advances-when-behind ()
  "Local master strictly behind origin/master is fast-forwarded (no wt on master)."
  (let ((agent-repl-master-branch-name "master")
        (update-ref-args nil))
    (cl-letf (((symbol-function 'agent-repl--git-exit-code)
               (lambda (_root &rest args)
                 (pcase args
                   (`("rev-parse" "--verify" "--quiet" "origin/master") 0)
                   (`("merge-base" "--is-ancestor" "master" "origin/master") 0)
                   (`("update-ref" "refs/heads/master" "refs/remotes/origin/master")
                    (setq update-ref-args args)
                    0)
                   (_ (error "unmocked git-exit-code args: %S" args)))))
              ((symbol-function 'agent-repl--git-branch-exists-p)
               (lambda (_root _branch) t))
              ((symbol-function 'agent-repl--git-string)
               (lambda (&rest args)
                 (pcase args
                   (`("-C" "/tmp/repo" "rev-parse" "master")
                    "1111111111111111111111111111111111111111")
                   (`("-C" "/tmp/repo" "rev-parse" "origin/master")
                    "2222222222222222222222222222222222222222")
                   (_ (error "unmocked git-string args: %S" args)))))
              ((symbol-function 'agent-repl--master-worktree-path)
               (lambda (_root) nil)))
      (agent-repl--maybe-fast-forward-master "/tmp/repo")
      (should (equal update-ref-args
                     '("update-ref" "refs/heads/master" "refs/remotes/origin/master"))))))

(ert-deftest agent-repl-test-maybe-ff-master-noop-when-diverged ()
  "Local master with commits origin/master lacks is NOT reset."
  (let ((agent-repl-master-branch-name "master")
        (update-ref-called nil)
        (merge-called nil))
    (cl-letf (((symbol-function 'agent-repl--git-exit-code)
               (lambda (_root &rest args)
                 (pcase args
                   (`("rev-parse" "--verify" "--quiet" "origin/master") 0)
                   ;; Non-zero means local master is NOT an ancestor — diverged.
                   (`("merge-base" "--is-ancestor" "master" "origin/master") 1)
                   (`("update-ref" . ,_) (setq update-ref-called t) 0)
                   (`("merge" . ,_) (setq merge-called t) 0)
                   (_ (error "unmocked git-exit-code args: %S" args)))))
              ((symbol-function 'agent-repl--git-branch-exists-p)
               (lambda (_root _branch) t))
              ((symbol-function 'agent-repl--git-string)
               (lambda (&rest args) (error "unmocked git-string args: %S" args)))
              ((symbol-function 'agent-repl--master-worktree-path)
               (lambda (_root) nil)))
      (agent-repl--maybe-fast-forward-master "/tmp/repo")
      (should-not update-ref-called)
      (should-not merge-called))))

(ert-deftest agent-repl-test-maybe-ff-master-noop-when-equal ()
  "When master == origin/master, the ref is unchanged."
  (let ((agent-repl-master-branch-name "master")
        (update-ref-called nil)
        (merge-called nil))
    (cl-letf (((symbol-function 'agent-repl--git-exit-code)
               (lambda (_root &rest args)
                 (pcase args
                   (`("rev-parse" "--verify" "--quiet" "origin/master") 0)
                   (`("merge-base" "--is-ancestor" "master" "origin/master") 0)
                   (`("update-ref" . ,_) (setq update-ref-called t) 0)
                   (`("merge" . ,_) (setq merge-called t) 0)
                   (_ (error "unmocked git-exit-code args: %S" args)))))
              ((symbol-function 'agent-repl--git-branch-exists-p)
               (lambda (_root _branch) t))
              ((symbol-function 'agent-repl--git-string)
               (lambda (&rest args)
                 ;; Same SHA on both sides → equal branch.
                 (pcase args
                   (`("-C" "/tmp/repo" "rev-parse" "master")
                    "abcdef0123456789abcdef0123456789abcdef01")
                   (`("-C" "/tmp/repo" "rev-parse" "origin/master")
                    "abcdef0123456789abcdef0123456789abcdef01")
                   (_ (error "unmocked git-string args: %S" args)))))
              ((symbol-function 'agent-repl--master-worktree-path)
               (lambda (_root) nil)))
      (agent-repl--maybe-fast-forward-master "/tmp/repo")
      (should-not update-ref-called)
      (should-not merge-called))))

(ert-deftest agent-repl-test-maybe-ff-master-noop-when-origin-missing ()
  "No origin/master ref → function is a no-op (no error, master unchanged)."
  (let ((agent-repl-master-branch-name "master")
        (other-calls 0))
    (cl-letf (((symbol-function 'agent-repl--git-exit-code)
               (lambda (_root &rest args)
                 (pcase args
                   ;; Origin verify FAILS — first cond branch fires, function returns.
                   (`("rev-parse" "--verify" "--quiet" "origin/master") 128)
                   (_ (cl-incf other-calls) 0)))))
      (agent-repl--maybe-fast-forward-master "/tmp/repo")
      (should (= other-calls 0)))))

(ert-deftest agent-repl-test-maybe-ff-master-noop-when-local-master-missing ()
  "No local master branch → function is a no-op (no error)."
  (let ((agent-repl-master-branch-name "master")
        (extra-calls 0))
    (cl-letf (((symbol-function 'agent-repl--git-exit-code)
               (lambda (_root &rest args)
                 (pcase args
                   (`("rev-parse" "--verify" "--quiet" "origin/master") 0)
                   (_ (cl-incf extra-calls) 0))))
              ;; Branch-exists-p says master is missing — second cond branch.
              ((symbol-function 'agent-repl--git-branch-exists-p)
               (lambda (_root _branch) nil)))
      (agent-repl--maybe-fast-forward-master "/tmp/repo")
      (should (= extra-calls 0)))))

(ert-deftest agent-repl-test-maybe-ff-master-advances-when-checked-out ()
  "When master is checked out, ff happens via `merge --ff-only' in that worktree."
  (let ((agent-repl-master-branch-name "master")
        (merge-call nil)
        (update-ref-called nil))
    (cl-letf (((symbol-function 'agent-repl--git-exit-code)
               (lambda (root &rest args)
                 (pcase args
                   (`("rev-parse" "--verify" "--quiet" "origin/master") 0)
                   (`("merge-base" "--is-ancestor" "master" "origin/master") 0)
                   (`("merge" "--ff-only" "origin/master")
                    (setq merge-call (list root args))
                    0)
                   (`("update-ref" . ,_) (setq update-ref-called t) 0)
                   (_ (error "unmocked git-exit-code args: %S" args)))))
              ((symbol-function 'agent-repl--git-branch-exists-p)
               (lambda (_root _branch) t))
              ((symbol-function 'agent-repl--git-string)
               (lambda (&rest args)
                 (pcase args
                   (`("-C" "/tmp/repo" "rev-parse" "master")
                    "1111111111111111111111111111111111111111")
                   (`("-C" "/tmp/repo" "rev-parse" "origin/master")
                    "2222222222222222222222222222222222222222")
                   (_ (error "unmocked git-string args: %S" args)))))
              ((symbol-function 'agent-repl--master-worktree-path)
               (lambda (_root) "/tmp/master-wt")))
      (agent-repl--maybe-fast-forward-master "/tmp/repo")
      (should (equal merge-call
                     (list "/tmp/master-wt"
                           '("merge" "--ff-only" "origin/master"))))
      (should-not update-ref-called))))

(ert-deftest agent-repl-test-maybe-ff-master-honors-custom-branch-name ()
  "`agent-repl-master-branch-name' selects which local/remote pair to ff."
  (let ((agent-repl-master-branch-name "trunk")
        (update-ref-args nil))
    (cl-letf (((symbol-function 'agent-repl--git-exit-code)
               (lambda (_root &rest args)
                 (pcase args
                   (`("rev-parse" "--verify" "--quiet" "origin/trunk") 0)
                   (`("merge-base" "--is-ancestor" "trunk" "origin/trunk") 0)
                   (`("update-ref" "refs/heads/trunk" "refs/remotes/origin/trunk")
                    (setq update-ref-args args)
                    0)
                   (_ (error "unmocked git-exit-code args: %S" args)))))
              ((symbol-function 'agent-repl--git-branch-exists-p)
               (lambda (_root _branch) t))
              ((symbol-function 'agent-repl--git-string)
               (lambda (&rest args)
                 (pcase args
                   (`("-C" "/tmp/repo" "rev-parse" "trunk")
                    "1111111111111111111111111111111111111111")
                   (`("-C" "/tmp/repo" "rev-parse" "origin/trunk")
                    "2222222222222222222222222222222222222222")
                   (_ (error "unmocked git-string args: %S" args)))))
              ((symbol-function 'agent-repl--master-worktree-path)
               (lambda (_root) nil)))
      (agent-repl--maybe-fast-forward-master "/tmp/repo")
      (should (equal update-ref-args
                     '("update-ref" "refs/heads/trunk" "refs/remotes/origin/trunk"))))))

;;;; ---- Tests: validate-worktree-creation ----

(ert-deftest agent-repl-test-validate-worktree-creation-empty-name ()
  "Empty name signals user-error."
  (should-error (agent-repl--validate-worktree-creation "" "/root" "dir" "branch" "/path")
                :type 'user-error))

(ert-deftest agent-repl-test-validate-worktree-creation-existing-path ()
  "An existing directory at PATH signals user-error."
  (cl-letf (((symbol-function 'file-directory-p)
             (lambda (p) (equal p "/tmp/repo")))
            ((symbol-function 'agent-repl--git-exit-code)
             (lambda (&rest _args) (error "should not probe git when path check fires"))))
    (should-error (agent-repl--validate-worktree-creation
                   "name" "/tmp/repo" "dir" "branch" "/tmp/repo")
                  :type 'user-error)))

(ert-deftest agent-repl-test-validate-worktree-creation-existing-branch ()
  "Existing branch signals user-error."
  (cl-letf (((symbol-function 'file-directory-p) (lambda (_p) nil))
            ((symbol-function 'agent-repl--git-branch-exists-p)
             (lambda (_root _branch) t)))
    (should-error (agent-repl--validate-worktree-creation
                   "feature" "/tmp/repo" "feature" "feature" "/nonexistent")
                  :type 'user-error)))

(ert-deftest agent-repl-test-validate-worktree-creation-passes ()
  "Valid inputs do not signal."
  (let ((agent-repl-worktree-start-tag-prefix nil))
    (cl-letf (((symbol-function 'file-directory-p) (lambda (_p) nil))
              ;; Branch missing → no collision.
              ((symbol-function 'agent-repl--git-branch-exists-p)
               (lambda (_root _branch) nil)))
      ;; Should not error
      (agent-repl--validate-worktree-creation
       "new-feature" "/tmp/repo" "new-feature" "new-feature" "/nonexistent"))))

(ert-deftest agent-repl-test-validate-worktree-creation-existing-start-tag ()
  "Existing start tag (PREFIX+BRANCH) signals user-error."
  (let ((agent-repl-worktree-start-tag-prefix "start/"))
    (cl-letf (((symbol-function 'file-directory-p) (lambda (_p) nil))
              ;; Branch missing.
              ((symbol-function 'agent-repl--git-branch-exists-p)
               (lambda (_root _branch) nil))
              ;; Start tag EXISTS.
              ((symbol-function 'agent-repl--git-tag-exists-p)
               (lambda (_root _tag) t)))
      (should-error (agent-repl--validate-worktree-creation
                     "feature" "/tmp/repo" "feature" "feature" "/nonexistent")
                    :type 'user-error))))

(ert-deftest agent-repl-test-validate-worktree-creation-start-tag-disabled ()
  "When start-tag prefix is nil, an existing 'start/feature' tag does not block."
  (let ((agent-repl-worktree-start-tag-prefix nil))
    (cl-letf (((symbol-function 'file-directory-p) (lambda (_p) nil))
              ;; Branch missing.
              ((symbol-function 'agent-repl--git-branch-exists-p)
               (lambda (_root _branch) nil))
              ;; Start-tag probe MUST NOT fire when prefix is nil.
              ((symbol-function 'agent-repl--git-tag-exists-p)
               (lambda (&rest _args)
                 (error "start-tag probe should be skipped when prefix is nil"))))
      ;; Should not error
      (agent-repl--validate-worktree-creation
       "feature" "/tmp/repo" "feature" "feature" "/nonexistent"))))

(ert-deftest agent-repl-test-validate-worktree-creation-nested-under-repo ()
  "Validation passes for a non-existent path nested under another git repo.
Regression: previously used `projectile-project-p', which walks UP from
PATH and would find an ancestor `.git' (e.g. when the worktree-parent
sits inside a separate repo), incorrectly flagging the new path as an
existing worktree."
  ;; The function under test uses `file-directory-p' on PATH only — it
  ;; never walks ancestors.  Asserting that an arbitrary nested path
  ;; that does not exist passes validation is enough to pin the
  ;; non-walking behavior.
  (let ((agent-repl-worktree-start-tag-prefix nil))
    (cl-letf (((symbol-function 'file-directory-p)
               (lambda (p)
                 ;; PATH passed in does not exist; only assertion.
                 (cond ((equal p "/tmp/outer-repo/inner/new-wt") nil)
                       (t (error "unexpected file-directory-p arg: %S" p)))))
              ;; Branch missing → no collision.
              ((symbol-function 'agent-repl--git-branch-exists-p)
               (lambda (_root _branch) nil)))
      ;; Should not error
      (agent-repl--validate-worktree-creation
       "new-wt" "/tmp/outer-repo" "new-wt" "new-wt" "/tmp/outer-repo/inner/new-wt"))))

;;;; ---- Tests: merge-fork (cherry-pick-base) ----
;;
;; `agent-repl--cherry-pick-base'
;; runs three git invocations through `agent-repl--git-string':
;;   (1) `log --right-only --pretty=%H --no-merges HEAD...TARGET' — target's
;;       unique commits, newest first, newline-separated.
;;   (2) `log --left-only --pretty=%B HEAD...TARGET' — HEAD's commits' bodies,
;;       which the parser scans for `(cherry picked from commit <sha>)' lines.
;;   (3) `merge-base HEAD TARGET' — only consulted on fallback when no target
;;       commit is found in the parsed cherry-pick annotations.
;; The tests stub `--git-string' with fixture strings shaped exactly like
;; real git output, so the dispatch logic is exercised end-to-end without
;; touching git.

(ert-deftest agent-repl-test-merge-fork-no-annotations-fallback ()
  "When HEAD has no -x annotations, fork falls back to merge-base HEAD TARGET."
  (let ((sha-m "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"))
    (cl-letf (((symbol-function 'agent-repl--git-string)
               (lambda (&rest args)
                 (pcase args
                   ;; Target has one unique commit (B1) — irrelevant SHA; the
                   ;; parser only matches commits present in HEAD's log.
                   (`("-C" "/tmp/repo" "log" "--right-only" "--pretty=%H" "--no-merges" "HEAD...branch-b")
                    "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb")
                   ;; HEAD's log has no cherry-pick annotations.
                   (`("-C" "/tmp/repo" "log" "--left-only" "--pretty=%B" "HEAD...branch-b")
                    "M\n\nA1\n")
                   ;; Fallback to merge-base.
                   (`("-C" "/tmp/repo" "merge-base" "HEAD" "branch-b")
                    sha-m)
                   (_ (error "unmocked git-string args: %S" args))))))
      (should (equal (agent-repl--cherry-pick-base "/tmp/repo" "branch-b") sha-m)))))

(ert-deftest agent-repl-test-cherry-pick-base-logs-entry-and-resolved-base ()
  "`--cherry-pick-base' logs an entry breadcrumb and the resolved base
so a frozen merge's post-mortem can pin which step it reached."
  (let ((sha-m "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
        (logs nil))
    (cl-letf (((symbol-function 'agent-repl--git-string)
               (lambda (&rest args)
                 (pcase args
                   (`("-C" "/tmp/repo" "log" "--right-only" "--pretty=%H" "--no-merges" "HEAD...branch-b")
                    "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb")
                   (`("-C" "/tmp/repo" "log" "--left-only" "--pretty=%B" "HEAD...branch-b")
                    "M\n\nA1\n")
                   (`("-C" "/tmp/repo" "merge-base" "HEAD" "branch-b")
                    sha-m)
                   (_ (error "unmocked git-string args: %S" args)))))
              ((symbol-function 'agent-repl--log)
               (lambda (_ws fmt &rest args)
                 (push (apply #'format fmt args) logs))))
      (agent-repl--cherry-pick-base "/tmp/repo" "branch-b")
      (should (cl-some (lambda (l) (string-match-p "cherry-pick-base: entry" l)) logs))
      (should (cl-some (lambda (l)
                         (string-match-p (concat "resolved base=" sha-m) l))
                       logs)))))

(ert-deftest agent-repl-test-merge-fork-clean-chain ()
  "After merging B (with -x), fork for C (descends from B) is B's tip SHA."
  ;; branch-c contains B1, B2, C1 (each unique vs HEAD).  HEAD's log carries
  ;; `(cherry picked from commit B1)` and `(cherry picked from commit B2)`,
  ;; but NOT C1.  The most recent target commit also present as a cherry-pick
  ;; annotation in HEAD's log is B2 — that's the fork point.
  (let ((sha-b1 "1111111111111111111111111111111111111111")
        (sha-b2 "2222222222222222222222222222222222222222")
        (sha-c1 "3333333333333333333333333333333333333333"))
    (cl-letf (((symbol-function 'agent-repl--git-string)
               (lambda (&rest args)
                 (pcase args
                   ;; `log --right-only` yields newest-first commits.
                   (`("-C" "/tmp/repo" "log" "--right-only" "--pretty=%H" "--no-merges" "HEAD...branch-c")
                    (concat sha-c1 "\n" sha-b2 "\n" sha-b1))
                   (`("-C" "/tmp/repo" "log" "--left-only" "--pretty=%B" "HEAD...branch-c")
                    (format "A1\n\nB1\n\n(cherry picked from commit %s)\n\nB2\n\n(cherry picked from commit %s)"
                            sha-b1 sha-b2))
                   (_ (error "unmocked git-string args: %S" args))))))
      (should (equal (agent-repl--cherry-pick-base "/tmp/repo" "branch-c") sha-b2)))))

(ert-deftest agent-repl-test-merge-fork-already-fully-merged ()
  "When all TARGET commits are incorporated, fork equals TARGET tip -> empty range."
  ;; Every commit on branch-b (B1, B2) appears as a cherry-pick annotation in
  ;; HEAD's log.  Newest-first target list is B2, B1, so B2 is returned.
  (let ((sha-b1 "1111111111111111111111111111111111111111")
        (sha-b2 "2222222222222222222222222222222222222222"))
    (cl-letf (((symbol-function 'agent-repl--git-string)
               (lambda (&rest args)
                 (pcase args
                   (`("-C" "/tmp/repo" "log" "--right-only" "--pretty=%H" "--no-merges" "HEAD...branch-b")
                    (concat sha-b2 "\n" sha-b1))
                   (`("-C" "/tmp/repo" "log" "--left-only" "--pretty=%B" "HEAD...branch-b")
                    (format "A1\n\nB1\n\n(cherry picked from commit %s)\n\nB2\n\n(cherry picked from commit %s)"
                            sha-b1 sha-b2))
                   (_ (error "unmocked git-string args: %S" args))))))
      (should (equal (agent-repl--cherry-pick-base "/tmp/repo" "branch-b") sha-b2)))))

(ert-deftest agent-repl-test-merge-fork-growing-workspace ()
  "After B is merged, adding B3 to branch-b; fork stays at B2 -> only B3 is new."
  ;; branch-b's target-only commits are B3, B2, B1 (newest-first).  B3 is NOT
  ;; in HEAD's cherry-pick annotations; B2 and B1 are.  `cl-find-if' walks
  ;; the target list newest-first and returns the first match — B2.
  (let ((sha-b1 "1111111111111111111111111111111111111111")
        (sha-b2 "2222222222222222222222222222222222222222")
        (sha-b3 "4444444444444444444444444444444444444444"))
    (cl-letf (((symbol-function 'agent-repl--git-string)
               (lambda (&rest args)
                 (pcase args
                   (`("-C" "/tmp/repo" "log" "--right-only" "--pretty=%H" "--no-merges" "HEAD...branch-b")
                    (concat sha-b3 "\n" sha-b2 "\n" sha-b1))
                   (`("-C" "/tmp/repo" "log" "--left-only" "--pretty=%B" "HEAD...branch-b")
                    (format "A1\n\nB1\n\n(cherry picked from commit %s)\n\nB2\n\n(cherry picked from commit %s)"
                            sha-b1 sha-b2))
                   (_ (error "unmocked git-string args: %S" args))))))
      (should (equal (agent-repl--cherry-pick-base "/tmp/repo" "branch-b") sha-b2)))))

(ert-deftest agent-repl-test-merge-fork-deep-chain ()
  "After merging B then C, fork for D (descends from C) is C's tip SHA."
  ;; branch-d's target-only newest-first: D1, C1, B2, B1.  HEAD's annotations
  ;; cover B1, B2, C1 (but not D1).  Newest match in target list is C1.
  (let ((sha-b1 "1111111111111111111111111111111111111111")
        (sha-b2 "2222222222222222222222222222222222222222")
        (sha-c1 "3333333333333333333333333333333333333333")
        (sha-d1 "5555555555555555555555555555555555555555"))
    (cl-letf (((symbol-function 'agent-repl--git-string)
               (lambda (&rest args)
                 (pcase args
                   (`("-C" "/tmp/repo" "log" "--right-only" "--pretty=%H" "--no-merges" "HEAD...branch-d")
                    (concat sha-d1 "\n" sha-c1 "\n" sha-b2 "\n" sha-b1))
                   (`("-C" "/tmp/repo" "log" "--left-only" "--pretty=%B" "HEAD...branch-d")
                    (format "A1\n\nB1\n\n(cherry picked from commit %s)\n\nB2\n\n(cherry picked from commit %s)\n\nC1\n\n(cherry picked from commit %s)"
                            sha-b1 sha-b2 sha-c1))
                   (_ (error "unmocked git-string args: %S" args))))))
      (should (equal (agent-repl--cherry-pick-base "/tmp/repo" "branch-d") sha-c1)))))

(ert-deftest agent-repl-test-merge-fork-annotation-survives-conflict-resolution ()
  "Annotation is written even when cherry-pick required conflict resolution via --continue."
  ;; Behaviorally identical to the clean-chain case from the parser's
  ;; perspective: HEAD's log contains the `(cherry picked from commit B1)`
  ;; annotation regardless of whether B1 cherry-picked clean or via --continue.
  ;; Fork for branch-c (which contains B1, C1) is therefore B1.
  (let ((sha-b1 "1111111111111111111111111111111111111111")
        (sha-c1 "3333333333333333333333333333333333333333"))
    (cl-letf (((symbol-function 'agent-repl--git-string)
               (lambda (&rest args)
                 (pcase args
                   (`("-C" "/tmp/repo" "log" "--right-only" "--pretty=%H" "--no-merges" "HEAD...branch-c")
                    (concat sha-c1 "\n" sha-b1))
                   (`("-C" "/tmp/repo" "log" "--left-only" "--pretty=%B" "HEAD...branch-c")
                    (format "A1\n\nB1 (resolved)\n\n(cherry picked from commit %s)" sha-b1))
                   (_ (error "unmocked git-string args: %S" args))))))
      (should (equal (agent-repl--cherry-pick-base "/tmp/repo" "branch-c") sha-b1)))))

;;;; ---- Tests: detect-merge-actually-landed-p ----

(ert-deftest agent-repl-test-detect-merge-actually-landed-p-defaults-true-no-project-dir ()
  "Returns t when WS has no :project-dir — backward-compat probe must
default to landed/success rather than flipping pre-existing successes
to ❌ when the worktree dir is gone or unset."
  (agent-repl-test--with-clean-state
    (puthash "ws" '() agent-repl--workspaces)
    (should (agent-repl--detect-merge-actually-landed-p "ws"))))

(ert-deftest agent-repl-test-detect-merge-actually-landed-p-defaults-true-no-source-ws-dir ()
  "Returns t when WS has no :source-ws-dir — the probe can't reach the
parent worktree to inspect cherry-pick annotations, so it defaults to
landed/success rather than slandering a clean merge."
  (agent-repl-test--with-clean-state
    (puthash "ws" '(:project-dir "/tmp/project") agent-repl--workspaces)
    ;; Production reads :source-ws-dir; absent → defaults to t before any git call.
    (cl-letf (((symbol-function 'file-directory-p)
               (lambda (p) (equal p "/tmp/project"))))
      (should (agent-repl--detect-merge-actually-landed-p "ws")))))

(ert-deftest agent-repl-test-detect-merge-actually-landed-p-true-on-clean-merge ()
  "Returns t when every commit on WS's branch is referenced via
cherry-pick -x in the parent's HEAD log.  Simulates a successful prior
merge: parent's HEAD log carries `(cherry picked from commit <sha>)' for
every target-only commit on WS's branch."
  (agent-repl-test--with-clean-state
    (puthash "ws"
             '(:project-dir "/tmp/wt" :source-ws-dir "/tmp/repo")
             agent-repl--workspaces)
    (let ((sha-f1 "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"))
      (cl-letf (((symbol-function 'file-directory-p) (lambda (_p) t))
                ((symbol-function 'agent-repl--git-string-quiet)
                 (lambda (&rest args)
                   (pcase args
                     (`("-C" "/tmp/wt" "rev-parse" "--abbrev-ref" "HEAD")
                      "feature")
                     ;; target-only commit list (newest-first).
                     (`("-C" "/tmp/repo" "log" "--right-only" "--pretty=%H" "--no-merges" "HEAD...feature")
                      sha-f1)
                     ;; parent's HEAD log carries the cherry-pick annotation.
                     (`("-C" "/tmp/repo" "log" "--left-only" "--pretty=%B" "HEAD...feature")
                      (format "M\n\nMerged feature\n\n(cherry picked from commit %s)" sha-f1))
                     (_ (error "unmocked git-string-quiet args: %S" args))))))
        (should (agent-repl--detect-merge-actually-landed-p "ws"))))))

(ert-deftest agent-repl-test-detect-merge-actually-landed-p-false-on-missing-pick ()
  "Returns nil when WS's branch has commits that are NOT referenced via
cherry-pick -x in the parent's HEAD log — the silent-failure case the
backward-compat probe is designed to detect."
  (agent-repl-test--with-clean-state
    (puthash "ws"
             '(:project-dir "/tmp/wt" :source-ws-dir "/tmp/repo")
             agent-repl--workspaces)
    (let ((sha-f1 "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"))
      (cl-letf (((symbol-function 'file-directory-p) (lambda (_p) t))
                ((symbol-function 'agent-repl--git-string-quiet)
                 (lambda (&rest args)
                   (pcase args
                     (`("-C" "/tmp/wt" "rev-parse" "--abbrev-ref" "HEAD")
                      "feature")
                     (`("-C" "/tmp/repo" "log" "--right-only" "--pretty=%H" "--no-merges" "HEAD...feature")
                      sha-f1)
                     ;; Parent log has NO cherry-pick annotation for F1 → silent failure.
                     (`("-C" "/tmp/repo" "log" "--left-only" "--pretty=%B" "HEAD...feature")
                      "M\n\nUnrelated parent commit")
                     (_ (error "unmocked git-string-quiet args: %S" args))))))
        (should-not (agent-repl--detect-merge-actually-landed-p "ws"))))))

;;;; ---- Tests: cherry-pick-commits ----

(ert-deftest agent-repl-test-cherry-pick-commits-empty-range-returns-sentinel ()
  "When range is empty (0 commits), returns `already-incorporated'
instead of erroring — the workspace's commits are already on the
parent, so the merge is a successful no-op and the caller can proceed
to auto-finish."
  ;; Production calls `git rev-list --count BASE..TARGET' once; "0" means
  ;; empty range and short-circuits to the `already-incorporated' sentinel
  ;; before any cherry-pick attempt.
  (cl-letf (((symbol-function 'agent-repl--git-string)
             (lambda (&rest args)
               (pcase args
                 (`("-C" "/tmp/repo" "rev-list" "--count" "HEAD..feature") "0")
                 (_ (error "unmocked git-string args: %S" args)))))
            ((symbol-function 'agent-repl--git-exit-code)
             (lambda (_root &rest args)
               (pcase args
                 ;; Wrapper always issues --abort on unwind; no-op here.
                 (`("cherry-pick" "--abort") 1)
                 (_ (error "git-exit-code must not be called for empty range (got %S)" args))))))
    (should (eq (agent-repl--cherry-pick-commits "/tmp/repo" "feature" "HEAD" "feature")
                'already-incorporated))))

(ert-deftest agent-repl-test-cherry-pick-commits-success ()
  "Successful cherry-pick with no conflicts."
  (let ((sha-m "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
        (cherry-pick-called nil))
    (cl-letf (((symbol-function 'agent-repl--git-string)
               (lambda (&rest args)
                 (pcase args
                   ;; Non-empty range — one commit to pick.
                   (`("-C" "/tmp/repo" "rev-list" "--count" ,_) "1")
                   (_ (error "unmocked git-string args: %S" args)))))
              ;; The pick now seeds its progress record with the range's
              ;; commits, so the log probe is on the path.
              ((symbol-function 'agent-repl--git-string-quiet)
               (lambda (&rest _args) "abc1234\tfeat: one"))
              ;; The pick itself streams (see `--git-exit-code-streaming'),
              ;; so it is this boundary rather than `--git-exit-code'.
              ((symbol-function 'agent-repl--git-exit-code-streaming)
               (lambda (_root _filter &rest args)
                 (pcase args
                   (`("cherry-pick" "-x" ,_)
                    (setq cherry-pick-called t)
                    0)
                   (_ (error "unmocked git-exit-code-streaming args: %S" args)))))
              ((symbol-function 'agent-repl--git-exit-code)
               (lambda (_root &rest args)
                 (pcase args
                   ;; Wrapper always issues --abort on unwind; no-op here.
                   (`("cherry-pick" "--abort") 1)
                   (_ (error "unmocked git-exit-code args: %S" args)))))
              ;; No CHERRY_PICK_HEAD remains — clean cherry-pick.
              ((symbol-function 'agent-repl--cherry-pick-in-progress-p)
               (lambda (_root) nil)))
      (agent-repl-test--with-merge-state
        (should (null (agent-repl--cherry-pick-commits
                       "/tmp/repo" "feature" sha-m "feature")))
        (should cherry-pick-called)))))

(ert-deftest agent-repl-test-cherry-pick-commits-seeds-progress ()
  "The pick seeds its progress record with the commits git is about to apply,
so the drawer can name the commit being cherry-picked from the first tick."
  (let ((sha-m "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"))
    (cl-letf (((symbol-function 'agent-repl--git-string)
               (lambda (&rest _args) "2"))
              ((symbol-function 'agent-repl--git-string-quiet)
               (lambda (&rest _args) "abc1234\tfeat: one\ndef5678\tfix: two"))
              ((symbol-function 'agent-repl--git-exit-code-streaming)
               (lambda (_root _filter &rest _args) 0))
              ((symbol-function 'agent-repl--git-exit-code)
               (lambda (_root &rest _args) 1))
              ((symbol-function 'agent-repl--cherry-pick-in-progress-p)
               (lambda (_root) nil)))
      (agent-repl-test--with-merge-state
        (agent-repl--cherry-pick-commits "/tmp/repo" "ws" sha-m "feature")
        (should (equal '(("abc1234" . "feat: one") ("def5678" . "fix: two"))
                       (plist-get (agent-repl--merge-progress-get "ws")
                                  :commits)))))))

(ert-deftest agent-repl-test-cherry-pick-commits-silent-failure-returns-failed ()
  "When `git cherry-pick' exits non-zero but no CHERRY_PICK_HEAD is left
behind (silent failure — commits didn't land and no conflict resolution
is in flight), `--cherry-pick-commits' returns `failed'."
  (let ((sha-m "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"))
    (cl-letf (((symbol-function 'agent-repl--git-string)
               (lambda (&rest args)
                 (pcase args
                   (`("-C" "/tmp/repo" "rev-list" "--count" ,_) "1")
                   (_ (error "unmocked git-string args: %S" args)))))
              ((symbol-function 'agent-repl--git-string-quiet)
               (lambda (&rest _args) "abc1234\tfeat: one"))
              ((symbol-function 'agent-repl--git-exit-code-streaming)
               (lambda (_root _filter &rest args)
                 (pcase args
                   ;; Non-zero exit but no conflict-head left behind.
                   (`("cherry-pick" "-x" ,_) 128)
                   (_ (error "unmocked git-exit-code-streaming args: %S" args)))))
              ((symbol-function 'agent-repl--git-exit-code)
               (lambda (_root &rest args)
                 (pcase args
                   ;; Wrapper always issues --abort on unwind; no-op here.
                   (`("cherry-pick" "--abort") 1)
                   (_ (error "unmocked git-exit-code args: %S" args)))))
              ((symbol-function 'agent-repl--cherry-pick-in-progress-p)
               (lambda (_root) nil)))
      (agent-repl-test--with-merge-state
        (should (eq (agent-repl--cherry-pick-commits
                     "/tmp/repo" "feature" sha-m "feature")
                    'failed))))))

(ert-deftest agent-repl-test-cherry-pick-commits-conflict-signals ()
  "Cherry-pick conflict aborts the cherry-pick and signals user-error
\(no magit pop — the abort clears CHERRY_PICK_HEAD so there's nothing
left to resolve)."
  (let ((sha-m "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
        ;; Track in-progress state: t while CHERRY_PICK_HEAD exists,
        ;; flipped to nil after `--check-cherry-pick-conflict' runs.
        (in-progress t))
    (cl-letf (((symbol-function 'agent-repl--git-string)
               (lambda (&rest args)
                 (pcase args
                   (`("-C" "/tmp/repo" "rev-list" "--count" ,_) "1")
                   (_ (error "unmocked git-string args: %S" args)))))
              ;; The pick seeds progress with the range's commits, so the log
              ;; probe is now on the path.
              ((symbol-function 'agent-repl--git-string-quiet)
               (lambda (&rest _args) "abc1234\tfeat: one"))
              ;; The pick streams (`--git-exit-code-streaming') so the drawer can
              ;; follow it commit by commit; --abort still goes through the
              ;; plain, output-discarding wrapper.
              ((symbol-function 'agent-repl--git-exit-code-streaming)
               (lambda (_root _filter &rest args)
                 (pcase args
                   (`("cherry-pick" "-x" ,_) 1)
                   (_ (error "unmocked git-exit-code-streaming args: %S" args)))))
              ((symbol-function 'agent-repl--git-exit-code)
               (lambda (_root &rest args)
                 (pcase args
                   ;; Wrapper always issues --abort on unwind; no-op here.
                   (`("cherry-pick" "--abort") 1)
                   (_ (error "unmocked git-exit-code args: %S" args)))))
              ((symbol-function 'agent-repl--cherry-pick-in-progress-p)
               (lambda (_root) in-progress))
              ;; The real check-cherry-pick-conflict would call git to abort;
              ;; stub it to (a) clear the in-progress state and (b) signal.
              ((symbol-function 'agent-repl--check-cherry-pick-conflict)
               (lambda (_ws root _target-ws)
                 (setq in-progress nil)
                 (user-error "Conflict cherry-picking in %s" root))))
      (should-error (agent-repl--cherry-pick-commits
                     "/tmp/repo" "feature" sha-m "feature")
                    :type 'user-error)
      (should-not in-progress))))

;;;; ---- Tests: check-cherry-pick-conflict ----

(ert-deftest agent-repl-test-check-cherry-pick-conflict-no-conflict ()
  "When no CHERRY_PICK_HEAD exists, returns nil (no error)."
  (cl-letf (((symbol-function 'agent-repl--git-string)
             (lambda (&rest args)
               (pcase args
                 (`("-C" "/tmp/repo" "rev-parse" "--absolute-git-dir") "/tmp/repo/.git")
                 (_ (error "unmocked git-string args: %S" args)))))
            ;; CHERRY_PICK_HEAD does not exist on disk.
            ((symbol-function 'file-exists-p) (lambda (_p) nil))
            ((symbol-function 'agent-repl--git-exit-code)
             (lambda (&rest args) (error "abort should not run: %S" args))))
    (should-not (agent-repl--check-cherry-pick-conflict "test-ws" "/tmp/repo" "test-ws"))))

(ert-deftest agent-repl-test-check-cherry-pick-conflict-with-conflict ()
  "When CHERRY_PICK_HEAD exists, `git cherry-pick --abort' is run before
user-error is signaled, then the on-disk state is cleared."
  (let ((cherry-pick-head-exists t)
        (abort-called nil))
    (cl-letf (((symbol-function 'agent-repl--git-string)
               (lambda (&rest args)
                 (pcase args
                   (`("-C" "/tmp/repo" "rev-parse" "--absolute-git-dir") "/tmp/repo/.git")
                   (`("-C" "/tmp/repo" "rev-parse" "--short" "CHERRY_PICK_HEAD") "abcd123")
                   (_ (error "unmocked git-string args: %S" args)))))
              ((symbol-function 'file-exists-p)
               (lambda (p)
                 (and (equal p "/tmp/repo/.git/CHERRY_PICK_HEAD")
                      cherry-pick-head-exists)))
              ((symbol-function 'agent-repl--git-exit-code)
               (lambda (_root &rest args)
                 (pcase args
                   (`("cherry-pick" "--abort")
                    (setq abort-called t
                          ;; Abort clears CHERRY_PICK_HEAD.
                          cherry-pick-head-exists nil)
                    0)
                   (_ (error "unmocked git-exit-code args: %S" args))))))
      (should-error (agent-repl--check-cherry-pick-conflict "test-ws" "/tmp/repo" "test-ws")
                    :type 'user-error)
      (should abort-called)
      (should-not cherry-pick-head-exists))))

;;;; ---- Tests: cherry-pick-in-progress-p ----

(ert-deftest agent-repl-test-cherry-pick-in-progress-p-false-on-clean-tree ()
  "No CHERRY_PICK_HEAD → returns nil."
  (cl-letf (((symbol-function 'agent-repl--git-string)
             (lambda (&rest args)
               (pcase args
                 (`("-C" "/tmp/repo" "rev-parse" "--absolute-git-dir") "/tmp/repo/.git")
                 (_ (error "unmocked git-string args: %S" args)))))
            ((symbol-function 'file-exists-p)
             (lambda (p)
               ;; The sole probe should be for CHERRY_PICK_HEAD; report missing.
               (cond ((equal p "/tmp/repo/.git/CHERRY_PICK_HEAD") nil)
                     (t (error "unexpected file-exists-p arg: %S" p))))))
    (should-not (agent-repl--cherry-pick-in-progress-p "/tmp/repo"))))

(ert-deftest agent-repl-test-cherry-pick-in-progress-p-true-during-conflict ()
  "CHERRY_PICK_HEAD present → returns t."
  (cl-letf (((symbol-function 'agent-repl--git-string)
             (lambda (&rest args)
               (pcase args
                 (`("-C" "/tmp/repo" "rev-parse" "--absolute-git-dir") "/tmp/repo/.git")
                 (_ (error "unmocked git-string args: %S" args)))))
            ((symbol-function 'file-exists-p)
             (lambda (p) (equal p "/tmp/repo/.git/CHERRY_PICK_HEAD"))))
    (should (agent-repl--cherry-pick-in-progress-p "/tmp/repo"))))

;;;; ---- Tests: cherry-pick-conflicted-files ----

(ert-deftest agent-repl-test-cherry-pick-conflicted-files-empty ()
  "No conflict in flight → empty list."
  (cl-letf (((symbol-function 'agent-repl--git-string)
             (lambda (&rest args)
               (pcase args
                 (`("-C" "/tmp/repo" "diff" "--name-only" "--diff-filter=U") "")
                 (_ (error "unmocked git-string args: %S" args))))))
    (should-not (agent-repl--cherry-pick-conflicted-files "/tmp/repo"))))

(ert-deftest agent-repl-test-cherry-pick-conflicted-files-lists-conflicts ()
  "Conflicted file is enumerated by name (relative to repo)."
  (cl-letf (((symbol-function 'agent-repl--git-string)
             (lambda (&rest args)
               (pcase args
                 (`("-C" "/tmp/repo" "diff" "--name-only" "--diff-filter=U") "shared")
                 (_ (error "unmocked git-string args: %S" args))))))
    (should (equal (agent-repl--cherry-pick-conflicted-files "/tmp/repo")
                   '("shared")))))

;;;; ---- Tests: file-has-conflict-markers-p ----

(ert-deftest agent-repl-test-file-has-conflict-markers-p-true ()
  "File containing <<<<<<< marker is detected as conflicted."
  (let ((tmp (make-temp-file "conflict-")))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (insert "ok\n<<<<<<< HEAD\nA\n=======\nB\n>>>>>>> other\n"))
          (should (agent-repl--file-has-conflict-markers-p tmp)))
      (delete-file tmp))))

(ert-deftest agent-repl-test-file-has-conflict-markers-p-false-on-clean ()
  "File without conflict markers returns nil."
  (let ((tmp (make-temp-file "no-conflict-")))
    (unwind-protect
        (progn
          (with-temp-file tmp (insert "plain content\nline 2\n"))
          (should-not (agent-repl--file-has-conflict-markers-p tmp)))
      (delete-file tmp))))

(ert-deftest agent-repl-test-file-has-conflict-markers-p-false-on-missing ()
  "Unreadable/missing file returns nil rather than erroring."
  (should-not (agent-repl--file-has-conflict-markers-p
               "/nonexistent/path/no-such-file")))

(ert-deftest agent-repl-test-file-has-conflict-markers-p-ignores-non-anchored-marker ()
  "A `<<<<<<<' that is not at line start is not a conflict marker."
  (let ((tmp (make-temp-file "fake-marker-")))
    (unwind-protect
        (progn
          (with-temp-file tmp (insert "prefix <<<<<<< not a marker\n"))
          (should-not (agent-repl--file-has-conflict-markers-p tmp)))
      (delete-file tmp))))

;;;; ---- Tests: all-conflicts-resolved-p ----

(ert-deftest agent-repl-test-all-conflicts-resolved-p-empty-list ()
  "Empty FILES list treated as resolved — nothing left to clear."
  (cl-letf (((symbol-function 'agent-repl--file-has-conflict-markers-p)
             (lambda (&rest _) (error "should not probe files on empty list"))))
    (should (agent-repl--all-conflicts-resolved-p "/tmp/repo" nil))))

(ert-deftest agent-repl-test-all-conflicts-resolved-p-true-when-markers-gone ()
  "Returns t when every listed file is clean of markers."
  (cl-letf (((symbol-function 'agent-repl--file-has-conflict-markers-p)
             (lambda (_path) nil)))
    (should (agent-repl--all-conflicts-resolved-p "/tmp/repo" '("a" "b")))))

(ert-deftest agent-repl-test-all-conflicts-resolved-p-false-when-any-file-has-markers ()
  "Any file still containing a marker → returns nil."
  (cl-letf (((symbol-function 'agent-repl--file-has-conflict-markers-p)
             (lambda (path)
               ;; File "a" is clean; file "b" still has markers.
               (string-suffix-p "b" path))))
    (should-not (agent-repl--all-conflicts-resolved-p "/tmp/repo" '("a" "b")))))

;;;; ---- Tests: build-auto-resolve-prompt ----

(ert-deftest agent-repl-test-build-auto-resolve-prompt-mentions-workspace ()
  "Generated prompt names the workspace and commit being resolved."
  (let ((p (agent-repl--build-auto-resolve-prompt "ws1" "deadbeef" '("f1.txt"))))
    (should (string-match-p "ws1" p))
    (should (string-match-p "deadbeef" p))))

(ert-deftest agent-repl-test-build-auto-resolve-prompt-lists-files ()
  "Generated prompt enumerates each conflicted file path."
  (let ((p (agent-repl--build-auto-resolve-prompt "ws1" "abc1234"
                                                   '("dir/a.el" "b.txt"))))
    (should (string-match-p "dir/a.el" p))
    (should (string-match-p "b.txt" p))))

(ert-deftest agent-repl-test-build-auto-resolve-prompt-forbids-git-commands ()
  "Generated prompt explicitly forbids git commands and edits outside
the conflicted files — the most load-bearing constraints for safety."
  (let ((p (agent-repl--build-auto-resolve-prompt "ws1" "abc1234" '("f"))))
    (should (string-match-p "NEVER run ANY git command" p))
    (should (string-match-p "no `git add`" p))
    (should (string-match-p "no `git cherry-pick --continue`" p))))

(ert-deftest agent-repl-test-build-auto-resolve-prompt-requires-orthogonality-check ()
  "Generated prompt requires the resolver to judge orthogonality and
make no edits when uncertain."
  (let ((p (agent-repl--build-auto-resolve-prompt "ws1" "abc1234" '("f"))))
    (should (string-match-p "CONCEPTUALLY ORTHOGONAL" p))
    (should (string-match-p "make NO edits" p))))

;;;; ---- Tests: spawn + wait helpers ----

(ert-deftest agent-repl-test-extract-buffer-whole-returns-whole-contents ()
  "`agent-repl--extract-buffer-whole' returns the entire buffer
contents, including any leading `#'-style header lines.  Used as the
default extractor for `agent-repl--spawn-and-wait' when no header is
expected."
  (let ((buf (generate-new-buffer " *test-extract-whole*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (insert "# header line\nactual content\n"))
          (should (equal (agent-repl--extract-buffer-whole buf)
                         "# header line\nactual content\n")))
      (kill-buffer buf))))

(ert-deftest agent-repl-test-extract-buffer-skip-header-comments-strips-header ()
  "`agent-repl--extract-buffer-skip-header-comments' skips leading
`#'-prefixed lines and leading blank lines, returning only the
content after the header block.  The merge resolver flow uses this
to avoid logging its own decorative header alongside the resolver's
real output."
  (let ((buf (generate-new-buffer " *test-extract-skip*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (insert "# agent-repl merge resolver — ws1\n")
            (insert "# root: /tmp\n")
            (insert "# cmd: (claude -p ...)\n")
            (insert "\n")
            (insert "ACTUAL RESOLVER OUTPUT\n"))
          (should (equal
                   (agent-repl--extract-buffer-skip-header-comments buf)
                   "ACTUAL RESOLVER OUTPUT\n")))
      (kill-buffer buf))))

(ert-deftest agent-repl-test-extract-buffer-skip-header-empty-when-only-header ()
  "When the buffer contains only header lines (no real content), the
skip-header extractor returns an empty string rather than the
header text — header lines must never leak into the log."
  (let ((buf (generate-new-buffer " *test-extract-only-header*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (insert "# only header\n# nothing else\n"))
          (should (equal
                   (agent-repl--extract-buffer-skip-header-comments buf)
                   "")))
      (kill-buffer buf))))

(ert-deftest agent-repl-test-wait-for-process-exit-dispatches-main-thread-to-main-impl ()
  "On the main thread (ert tests run here) `--wait-for-process-exit'
dispatches to `--wait-for-process-exit--main' rather than the
worker-thread sentinel + condvar implementation.  Verifies the
thread-check guard is wired correctly."
  (let ((called-main nil)
        (called-worker nil))
    (cl-letf (((symbol-function 'agent-repl--wait-for-process-exit--main)
               (lambda (&rest _) (setq called-main t) 0))
              ((symbol-function 'agent-repl--wait-for-process-exit--worker)
               (lambda (&rest _) (setq called-worker t) 0)))
      (agent-repl--wait-for-process-exit nil 1))
    (should called-main)
    (should-not called-worker)))

(ert-deftest agent-repl-test-wait-for-process-exit-worker-already-exited-returns-immediately ()
  "A process that exited BEFORE the worker wait installed its sentinel
completes immediately with the real exit status instead of blocking
until the timeout.  Guards the sentinel-install race: the status-change
notification was already consumed, so the sentinel never fires and the
post-install status sample is the only completion path."
  (let ((proc (start-process "test-already-exited" nil "sh" "-c" "exit 7")))
    (while (process-live-p proc)
      (accept-process-output proc 0.05))
    ;; Drain any pending status-change so the default sentinel consumes it.
    (accept-process-output nil 0.05)
    (cl-letf (((symbol-function 'condition-wait)
               (lambda (&rest _) (error "condition-wait must not be reached"))))
      (should (= 7 (agent-repl--wait-for-process-exit--worker proc 5 nil nil))))))

(ert-deftest agent-repl-test-wait-for-process-exit-worker-already-exited-skips-timeout-timer ()
  "When the post-install status sample finds the process already dead,
no timeout timer is scheduled — there is nothing left to deadline."
  (let ((proc (start-process "test-already-exited-timer" nil "true"))
        (timer-created nil))
    (while (process-live-p proc)
      (accept-process-output proc 0.05))
    (accept-process-output nil 0.05)
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (&rest _) (setq timer-created t) nil))
              ((symbol-function 'condition-wait)
               (lambda (&rest _) nil)))
      (agent-repl--wait-for-process-exit--worker proc 5 nil nil))
    (should-not timer-created)))

(ert-deftest agent-repl-test-spawn-and-wait-kills-buffer-when-keep-buffer-nil ()
  "Default behavior of `agent-repl--spawn-and-wait' is to kill OUT-BUF
after extracting + logging output.  Callers (e.g. verify) rely on this
to avoid leaking temp buffers."
  (let* ((out-buf (generate-new-buffer " *test-spawn-and-wait-kill*"))
         (real-start (symbol-function 'start-process)))
    (cl-letf (((symbol-function 'start-process)
               (lambda (_name _buf &rest _cmd)
                 (funcall real-start "stub"
                          (generate-new-buffer " *stub*") "true")))
              ((symbol-function 'agent-repl--log) (lambda (&rest _) nil)))
      (agent-repl--spawn-and-wait
       '("true") out-buf
       :process-name "test" :timeout 5
       :log-tag "test" :log-ws nil)
      (should-not (buffer-live-p out-buf)))))

(ert-deftest agent-repl-test-spawn-and-wait-preserves-buffer-when-keep-buffer-t ()
  "When :KEEP-BUFFER is non-nil, `agent-repl--spawn-and-wait' leaves
OUT-BUF alive after return so callers can use it for live inspection
(the merge resolver side-buffer case)."
  (let* ((out-buf (generate-new-buffer " *test-spawn-and-wait-keep*"))
         (real-start (symbol-function 'start-process)))
    (cl-letf (((symbol-function 'start-process)
               (lambda (_name _buf &rest _cmd)
                 (funcall real-start "stub"
                          (generate-new-buffer " *stub*") "true")))
              ((symbol-function 'agent-repl--log) (lambda (&rest _) nil)))
      (unwind-protect
          (progn
            (agent-repl--spawn-and-wait
             '("true") out-buf
             :process-name "test" :timeout 5
             :log-tag "test" :log-ws nil
             :keep-buffer t)
            (should (buffer-live-p out-buf)))
        (when (buffer-live-p out-buf) (kill-buffer out-buf))))))

(ert-deftest agent-repl-test-spawn-and-wait-calls-on-completed-callback ()
  "When :ON-COMPLETED is supplied, `agent-repl--spawn-and-wait' invokes
it with (status output) AFTER the exit log line BEFORE buffer cleanup.
Used by the merge resolver to annotate the side buffer with the final
`# exit:' marker."
  (let* ((out-buf (generate-new-buffer " *test-spawn-and-wait-cb*"))
         (real-start (symbol-function 'start-process))
         (captured nil))
    (cl-letf (((symbol-function 'start-process)
               (lambda (_name _buf &rest _cmd)
                 (funcall real-start "stub"
                          (generate-new-buffer " *stub*") "true")))
              ((symbol-function 'agent-repl--log) (lambda (&rest _) nil)))
      (agent-repl--spawn-and-wait
       '("true") out-buf
       :process-name "test" :timeout 5
       :log-tag "test" :log-ws nil
       :on-completed (lambda (status output)
                       (setq captured (list status output)))))
    (should (numberp (car captured)))
    (should (eql (car captured) 0))
    ;; Output captured pre-cleanup; not asserting exact content because
    ;; the stub doesn't write anything meaningful to out-buf.
    (should (stringp (or (cadr captured) "")))))

;;;; ---- Tests: auto-resolve-conflicts-extra-args default ----

(ert-deftest agent-repl-test-auto-resolve-extra-args-includes-dangerously-skip-permissions ()
  "Default extra-args contain `--dangerously-skip-permissions' so the
resolver cannot stall on a permission prompt even when
`bypassPermissions' mode is insufficient."
  (should (member "--dangerously-skip-permissions"
                  (default-value 'agent-repl-auto-resolve-conflicts-extra-args))))

(ert-deftest agent-repl-test-invoke-auto-resolve-agent-passes-extra-args ()
  "`--invoke-auto-resolve-agent' includes the configured extra-args
(including `--dangerously-skip-permissions') in the spawned command,
after the base `-p --model MODEL' args."
  (let* ((captured-cmd nil)
         (real-start (symbol-function 'start-process)))
    (cl-letf (((symbol-function 'start-process)
               (lambda (_name _buf &rest cmd)
                 (setq captured-cmd cmd)
                 ;; Run a trivially-succeeding process so the live-p
                 ;; poll loop terminates immediately without spawning
                 ;; the real `claude' binary.
                 (funcall real-start "agent-auto-resolve-stub"
                          (generate-new-buffer " *stub*") "true"))))
      (agent-repl--invoke-auto-resolve-agent "/tmp" "prompt"))
    (should (member "--dangerously-skip-permissions" captured-cmd))
    (should (equal (cl-subseq captured-cmd 0 4)
                   (list agent-repl-auto-resolve-conflicts-program
                         "-p" "--model"
                         agent-repl-auto-resolve-conflicts-model)))))

(ert-deftest agent-repl-test-invoke-auto-resolve-agent-passes-prompt-as-trailing-arg ()
  "`--invoke-auto-resolve-agent' passes PROMPT as the final positional
argument to `claude -p' (that is how the non-interactive API consumes
the prompt — NOT via stdin)."
  (let* ((captured-cmd nil)
         (real-start (symbol-function 'start-process)))
    (cl-letf (((symbol-function 'start-process)
               (lambda (_name _buf &rest cmd)
                 (setq captured-cmd cmd)
                 (funcall real-start "agent-auto-resolve-stub"
                          (generate-new-buffer " *stub*") "true"))))
      (agent-repl--invoke-auto-resolve-agent "/tmp" "RESOLVE THIS"))
    (should (equal (car (last captured-cmd)) "RESOLVE THIS"))))

(ert-deftest agent-repl-test-invoke-auto-resolve-agent-separates-prompt-with-double-dash ()
  "PROMPT is preceded by `--' in the cmd so the claude CLI's variadic
`--allowedTools <tools...>' flag (which comes from extra-args) cannot
swallow the prompt as another tool name.  Without `--', claude exits
1 with `Input must be provided either through stdin or as a prompt
argument when using --print' and the resolver always fails."
  (let* ((captured-cmd nil)
         (real-start (symbol-function 'start-process)))
    (cl-letf (((symbol-function 'start-process)
               (lambda (_name _buf &rest cmd)
                 (setq captured-cmd cmd)
                 (funcall real-start "stub"
                          (generate-new-buffer " *stub*") "true"))))
      (agent-repl--invoke-auto-resolve-agent "/tmp" "MY PROMPT"))
    (let ((tail (last captured-cmd 2)))
      (should (equal (car tail) "--"))
      (should (equal (cadr tail) "MY PROMPT")))))

(ert-deftest agent-repl-test-invoke-auto-resolve-agent-logs-output ()
  "`--invoke-auto-resolve-agent' mirrors the resolver's stdout/stderr
into the logfile via `agent-repl--log'.  Without this the resolver's
response only lives in a dedicated Emacs buffer — ungreppable, lost on
session restart — and a post-mortem requires the user to know the
buffer name."
  (let* ((logged nil)
         (real-start (symbol-function 'start-process)))
    (cl-letf (((symbol-function 'start-process)
               (lambda (_name buf &rest _cmd)
                 (with-current-buffer buf
                   (insert "RESOLVER STDOUT\n"))
                 (funcall real-start "stub"
                          (generate-new-buffer " *stub*") "true")))
              ((symbol-function 'agent-repl--log)
               (lambda (_ws fmt &rest args)
                 (push (apply #'format fmt args) logged))))
      (agent-repl--invoke-auto-resolve-agent "/tmp" "prompt" "ws1"))
    (should (cl-some (lambda (l) (string-match-p "RESOLVER STDOUT" l)) logged))
    (should (cl-some (lambda (l)
                       (string-match-p "auto-resolve: exited status=" l))
                     logged))))

(ert-deftest agent-repl-test-invoke-auto-resolve-agent-log-omits-header-block ()
  "The logged output excludes the `# agent-repl merge resolver — ...'
header block we insert into the side buffer at the top.  Only the
resolver's actual stdout/stderr should appear in the log — leaking our
own header is just noise that obscures the real response."
  (let* ((logged nil)
         (real-start (symbol-function 'start-process)))
    (cl-letf (((symbol-function 'start-process)
               (lambda (_name buf &rest _cmd)
                 (with-current-buffer buf
                   (insert "ACTUAL RESOLVER OUTPUT\n"))
                 (funcall real-start "stub"
                          (generate-new-buffer " *stub*") "true")))
              ((symbol-function 'agent-repl--log)
               (lambda (_ws fmt &rest args)
                 (push (apply #'format fmt args) logged))))
      (agent-repl--invoke-auto-resolve-agent "/tmp" "prompt" "ws1"))
    (let ((output-log (cl-find-if
                       (lambda (l) (string-match-p "output follows" l))
                       logged)))
      (should output-log)
      (should (string-match-p "ACTUAL RESOLVER OUTPUT" output-log))
      (should-not (string-match-p "# agent-repl merge resolver" output-log))
      (should-not (string-match-p "# root:" output-log))
      (should-not (string-match-p "# cmd:" output-log)))))

(ert-deftest agent-repl-test-invoke-auto-resolve-agent-passes-ws-to-log ()
  "Resolver-output log entries carry TARGET-WS as the workspace tag, so
the standard `{ws=... id=...}` metadata block disambiguates resolver
runs across concurrent merges."
  (let* ((logged-ws nil)
         (real-start (symbol-function 'start-process)))
    (cl-letf (((symbol-function 'start-process)
               (lambda (_name _buf &rest _cmd)
                 (funcall real-start "stub"
                          (generate-new-buffer " *stub*") "true")))
              ((symbol-function 'agent-repl--log)
               (lambda (ws fmt &rest args)
                 (when (string-match-p "exited status="
                                       (apply #'format fmt args))
                   (push ws logged-ws)))))
      (agent-repl--invoke-auto-resolve-agent "/tmp" "prompt" "my-ws"))
    (should (member "my-ws" logged-ws))))

(ert-deftest agent-repl-test-invoke-auto-resolve-verify-logs-output ()
  "`--invoke-auto-resolve-verify' mirrors the verify command's
stdout/stderr into the logfile before the temp buffer is killed, so a
non-zero exit (which blocks the merge) can be diagnosed from the
logfile alone — the temp buffer is gone by the time anyone looks."
  (let* ((logged nil)
         (real-start (symbol-function 'start-process)))
    (cl-letf (((symbol-function 'start-process)
               (lambda (_name buf &rest _cmd)
                 (with-current-buffer buf
                   (insert "VERIFY OUTPUT\n"))
                 (funcall real-start "stub"
                          (generate-new-buffer " *stub*") "true")))
              ((symbol-function 'agent-repl--log)
               (lambda (_ws fmt &rest args)
                 (push (apply #'format fmt args) logged))))
      (agent-repl--invoke-auto-resolve-verify "/tmp" (list "true")))
    (should (cl-some (lambda (l) (string-match-p "VERIFY OUTPUT" l)) logged))
    (should (cl-some (lambda (l)
                       (string-match-p "auto-resolve-verify: exited status=" l))
                     logged))))

;;;; ---- Tests: auto-resolve-cherry-pick-conflict ----

(ert-deftest agent-repl-test-auto-resolve-returns-nil-when-no-conflicted-files ()
  "No conflicted files → resolver returns nil without spawning claude."
  (let ((invoked nil))
    (cl-letf (((symbol-function 'agent-repl--cherry-pick-conflicted-files)
               (lambda (_root) nil))
              ((symbol-function 'agent-repl--invoke-auto-resolve-agent)
               (lambda (&rest _) (setq invoked t) 0)))
      (should-not (agent-repl--auto-resolve-cherry-pick-conflict "ws" "/tmp/repo"))
      (should-not invoked))))

(ert-deftest agent-repl-test-auto-resolve-accepts-when-markers-cleared ()
  "Resolver returns t when conflicted files no longer contain markers
after the stubbed `claude -p' returns successfully."
  (cl-letf (((symbol-function 'agent-repl--cherry-pick-conflicted-files)
             (lambda (_root) '("shared")))
            ((symbol-function 'agent-repl--git-string)
             (lambda (&rest args)
               (pcase args
                 (`("-C" "/tmp/repo" "rev-parse" "--short" "CHERRY_PICK_HEAD") "abcd123")
                 (_ (error "unmocked git-string args: %S" args)))))
            ((symbol-function 'agent-repl--invoke-auto-resolve-agent)
             (lambda (&rest _) 0))
            ;; Files reported clean of markers after the resolver runs.
            ((symbol-function 'agent-repl--all-conflicts-resolved-p)
             (lambda (_root _files) t))
            ;; No verify command configured → gate accepts.
            ((symbol-function 'agent-repl--auto-resolve-verify-passes-p)
             (lambda (_ws _root) t)))
    (should (agent-repl--auto-resolve-cherry-pick-conflict "ws" "/tmp/repo"))))

(ert-deftest agent-repl-test-auto-resolve-declines-when-markers-remain ()
  "Resolver returns nil when conflict markers still exist in any file
after the stubbed `claude -p' exits."
  (cl-letf (((symbol-function 'agent-repl--cherry-pick-conflicted-files)
             (lambda (_root) '("shared")))
            ((symbol-function 'agent-repl--git-string)
             (lambda (&rest _args) "abcd123"))
            ((symbol-function 'agent-repl--invoke-auto-resolve-agent)
             (lambda (&rest _) 0))
            ;; Markers still present → decline.
            ((symbol-function 'agent-repl--all-conflicts-resolved-p)
             (lambda (_root _files) nil)))
    (should-not (agent-repl--auto-resolve-cherry-pick-conflict "ws" "/tmp/repo"))))

(ert-deftest agent-repl-test-auto-resolve-declines-on-timeout ()
  "Resolver returns nil when invoked claude -p reports timeout."
  (cl-letf (((symbol-function 'agent-repl--cherry-pick-conflicted-files)
             (lambda (_root) '("shared")))
            ((symbol-function 'agent-repl--git-string)
             (lambda (&rest _args) "abcd123"))
            ((symbol-function 'agent-repl--invoke-auto-resolve-agent)
             (lambda (&rest _) 'timeout))
            ((symbol-function 'agent-repl--all-conflicts-resolved-p)
             (lambda (&rest _args) (error "should not probe markers after timeout"))))
    (should-not (agent-repl--auto-resolve-cherry-pick-conflict "ws" "/tmp/repo"))))

(ert-deftest agent-repl-test-auto-resolve-declines-on-nonzero-exit ()
  "Resolver returns nil when invoked claude -p exits non-zero, even if
the files happen to look clean afterward — a failure exit is the only
honest signal that something went wrong inside the headless agent."
  (cl-letf (((symbol-function 'agent-repl--cherry-pick-conflicted-files)
             (lambda (_root) '("shared")))
            ((symbol-function 'agent-repl--git-string)
             (lambda (&rest _args) "abcd123"))
            ((symbol-function 'agent-repl--invoke-auto-resolve-agent)
             (lambda (&rest _) 1))
            ;; Even with clean files, the non-zero exit short-circuits.
            ((symbol-function 'agent-repl--all-conflicts-resolved-p)
             (lambda (&rest _args)
               (error "should not probe markers after non-zero exit"))))
    (should-not (agent-repl--auto-resolve-cherry-pick-conflict "ws" "/tmp/repo"))))

;;;; ---- Tests: auto-resolve-verify-cmd (config resolver) ----

(ert-deftest agent-repl-test-auto-resolve-verify-cmd-nil-config ()
  "nil config resolves to nil (skip verification)."
  (let ((agent-repl-auto-resolve-verify-command nil))
    (should-not (agent-repl--auto-resolve-verify-cmd "/tmp"))))

(ert-deftest agent-repl-test-auto-resolve-verify-cmd-list-config ()
  "List-of-strings config resolves to itself."
  (let ((agent-repl-auto-resolve-verify-command '("just" "test")))
    (should (equal (agent-repl--auto-resolve-verify-cmd "/tmp")
                   '("just" "test")))))

(ert-deftest agent-repl-test-auto-resolve-verify-cmd-function-returning-list ()
  "Function-form config: function is called with ROOT, return list is used."
  (let* ((received-root nil)
         (agent-repl-auto-resolve-verify-command
          (lambda (root) (setq received-root root) '("verify" "here"))))
    (should (equal (agent-repl--auto-resolve-verify-cmd "/tmp/wt")
                   '("verify" "here")))
    (should (equal received-root "/tmp/wt"))))

(ert-deftest agent-repl-test-auto-resolve-verify-cmd-function-returning-nil ()
  "Function-form returning nil means skip verification for this invocation."
  (let ((agent-repl-auto-resolve-verify-command (lambda (_root) nil)))
    (should-not (agent-repl--auto-resolve-verify-cmd "/tmp"))))

(ert-deftest agent-repl-test-auto-resolve-verify-cmd-function-returning-malformed ()
  "Function-form returning malformed value resolves to nil (skip), not raise."
  (let ((agent-repl-auto-resolve-verify-command (lambda (_r) 'oops)))
    (should-not (agent-repl--auto-resolve-verify-cmd "/tmp"))))

(ert-deftest agent-repl-test-auto-resolve-verify-cmd-malformed-list-config ()
  "List containing non-strings resolves to nil (skip), not raise."
  (let ((agent-repl-auto-resolve-verify-command '("just" 42)))
    (should-not (agent-repl--auto-resolve-verify-cmd "/tmp"))))

;;;; ---- Tests: invoke-auto-resolve-verify (subprocess) ----
;;
;; `agent-repl--invoke-auto-resolve-verify' spawns a process via
;; `start-process'.  Per the no-subprocess policy these tests must stub
;; that primitive — the production logic under test is the wait-loop,
;; status-dispatch, and cwd-binding behavior around the spawn, not the
;; spawn itself.  Each test replaces `start-process' (and adjacent
;; primitives the wait-loop depends on) with deterministic fakes.

(ert-deftest agent-repl-test-invoke-auto-resolve-verify-zero-exit ()
  "Verifier with exit-0 command returns 0."
  (let ((agent-repl-auto-resolve-verify-timeout 30))
    (cl-letf (((symbol-function 'start-process)
               (lambda (_name buf &rest _cmd)
                 ;; Return a fake process plist — the wait loop only consults
                 ;; `process-live-p' / `process-exit-status' / etc., which we
                 ;; stub below.  The buffer must be a real live buffer so the
                 ;; output-capture branch works.
                 (list :proc :buffer buf :status 0)))
              ((symbol-function 'set-process-query-on-exit-flag)
               (lambda (&rest _) nil))
              ((symbol-function 'process-live-p) (lambda (_p) nil))
              ((symbol-function 'process-exit-status) (lambda (_p) 0))
              ((symbol-function 'accept-process-output)
               (lambda (&rest _) nil))
              ((symbol-function 'delete-process) (lambda (_p) nil)))
      (should (eql (agent-repl--invoke-auto-resolve-verify "/tmp/repo" '("true"))
                   0)))))

(ert-deftest agent-repl-test-invoke-auto-resolve-verify-nonzero-exit ()
  "Verifier with exit-non-zero command returns the non-zero exit code."
  (let ((agent-repl-auto-resolve-verify-timeout 30))
    (cl-letf (((symbol-function 'start-process)
               (lambda (_name buf &rest _cmd) (list :proc :buffer buf)))
              ((symbol-function 'set-process-query-on-exit-flag)
               (lambda (&rest _) nil))
              ((symbol-function 'process-live-p) (lambda (_p) nil))
              ((symbol-function 'process-exit-status) (lambda (_p) 1))
              ((symbol-function 'accept-process-output) (lambda (&rest _) nil))
              ((symbol-function 'delete-process) (lambda (_p) nil)))
      (let ((rc (agent-repl--invoke-auto-resolve-verify "/tmp/repo" '("false"))))
        (should (and (numberp rc) (not (zerop rc))))))))

(ert-deftest agent-repl-test-invoke-auto-resolve-verify-timeout ()
  "Verifier with a hung command returns `timeout' when the deadline elapses."
  ;; Simulate the deadline-elapsed branch by making the process appear live
  ;; while time advances past the timeout.  `float-time' is stubbed to return
  ;; a monotonically increasing value so the wait loop fires the timeout exit.
  (let ((agent-repl-auto-resolve-verify-timeout 1)
        (now 0)
        (delete-called nil))
    (cl-letf (((symbol-function 'start-process)
               (lambda (_name buf &rest _cmd) (list :proc :buffer buf)))
              ((symbol-function 'set-process-query-on-exit-flag)
               (lambda (&rest _) nil))
              ((symbol-function 'process-live-p) (lambda (_p) t))
              ((symbol-function 'float-time)
               (lambda (&rest _)
                 (let ((t-now now))
                   (cl-incf now 10)
                   t-now)))
              ((symbol-function 'accept-process-output) (lambda (&rest _) nil))
              ((symbol-function 'delete-process)
               (lambda (_p) (setq delete-called t))))
      (should (eq (agent-repl--invoke-auto-resolve-verify
                   "/tmp/repo" '("sleep" "30"))
                  'timeout))
      (should delete-called))))

(ert-deftest agent-repl-test-invoke-auto-resolve-verify-cwd-is-root ()
  "Verifier runs with `default-directory' set to ROOT.
The production function rebinds `default-directory' around the spawn;
the stubbed `start-process' captures the binding to prove it."
  (let ((agent-repl-auto-resolve-verify-timeout 30)
        (captured-cwd nil))
    (cl-letf (((symbol-function 'start-process)
               (lambda (_name buf &rest _cmd)
                 (setq captured-cwd default-directory)
                 (list :proc :buffer buf)))
              ((symbol-function 'set-process-query-on-exit-flag)
               (lambda (&rest _) nil))
              ((symbol-function 'process-live-p) (lambda (_p) nil))
              ((symbol-function 'process-exit-status) (lambda (_p) 0))
              ((symbol-function 'accept-process-output) (lambda (&rest _) nil))
              ((symbol-function 'delete-process) (lambda (_p) nil)))
      (let ((rc (agent-repl--invoke-auto-resolve-verify
                 "/tmp/repo" '("sh" "-c" "true"))))
        (should (eql rc 0))
        (should (equal captured-cwd
                       (file-name-as-directory "/tmp/repo")))))))

;;;; ---- Tests: auto-resolve-verify-passes-p ----

(ert-deftest agent-repl-test-auto-resolve-verify-passes-p-no-command ()
  "With no verify-command configured, the gate accepts without spawning."
  (let ((agent-repl-auto-resolve-verify-command nil)
        (spawned nil))
    (cl-letf (((symbol-function 'agent-repl--invoke-auto-resolve-verify)
               (lambda (&rest _) (setq spawned t) 0)))
      (should (agent-repl--auto-resolve-verify-passes-p "ws" "/tmp"))
      (should-not spawned))))

(ert-deftest agent-repl-test-auto-resolve-verify-passes-p-zero-exit ()
  "Verifier exit=0 → gate accepts."
  (let ((agent-repl-auto-resolve-verify-command '("true")))
    (cl-letf (((symbol-function 'agent-repl--invoke-auto-resolve-verify)
               (lambda (&rest _) 0)))
      (should (agent-repl--auto-resolve-verify-passes-p "ws" "/tmp")))))

(ert-deftest agent-repl-test-auto-resolve-verify-passes-p-nonzero-exit ()
  "Verifier exit non-zero → gate declines (returns nil)."
  (let ((agent-repl-auto-resolve-verify-command '("false")))
    (cl-letf (((symbol-function 'agent-repl--invoke-auto-resolve-verify)
               (lambda (&rest _) 1)))
      (should-not (agent-repl--auto-resolve-verify-passes-p "ws" "/tmp")))))

(ert-deftest agent-repl-test-auto-resolve-verify-passes-p-timeout ()
  "Verifier timeout → gate declines (returns nil)."
  (let ((agent-repl-auto-resolve-verify-command '("hang")))
    (cl-letf (((symbol-function 'agent-repl--invoke-auto-resolve-verify)
               (lambda (&rest _) 'timeout)))
      (should-not (agent-repl--auto-resolve-verify-passes-p "ws" "/tmp")))))

;;;; ---- Tests: auto-resolve-cherry-pick-conflict with verify ----

(ert-deftest agent-repl-test-auto-resolve-declines-when-verify-fails ()
  "Even with markers cleared and resolver exit=0, a non-zero verify
exit causes `--auto-resolve-cherry-pick-conflict' to return nil.
Soundness gate: textual marker scan is necessary but not sufficient."
  (let ((agent-repl-auto-resolve-verify-command '("verify-cmd")))
    (cl-letf (((symbol-function 'agent-repl--cherry-pick-conflicted-files)
               (lambda (_root) '("shared")))
              ((symbol-function 'agent-repl--git-string)
               (lambda (&rest _args) "abcd123"))
              ((symbol-function 'agent-repl--invoke-auto-resolve-agent)
               (lambda (&rest _) 0))
              ((symbol-function 'agent-repl--all-conflicts-resolved-p)
               (lambda (_root _files) t))
              ((symbol-function 'agent-repl--invoke-auto-resolve-verify)
               (lambda (&rest _) 1)))
      (should-not (agent-repl--auto-resolve-cherry-pick-conflict "ws" "/tmp/repo")))))

(ert-deftest agent-repl-test-auto-resolve-accepts-when-verify-passes ()
  "Markers cleared AND verify exit=0 → `--auto-resolve-cherry-pick-conflict' returns t."
  (let ((agent-repl-auto-resolve-verify-command '("verify-cmd")))
    (cl-letf (((symbol-function 'agent-repl--cherry-pick-conflicted-files)
               (lambda (_root) '("shared")))
              ((symbol-function 'agent-repl--git-string)
               (lambda (&rest _args) "abcd123"))
              ((symbol-function 'agent-repl--invoke-auto-resolve-agent)
               (lambda (&rest _) 0))
              ((symbol-function 'agent-repl--all-conflicts-resolved-p)
               (lambda (_root _files) t))
              ((symbol-function 'agent-repl--invoke-auto-resolve-verify)
               (lambda (&rest _) 0)))
      (should (agent-repl--auto-resolve-cherry-pick-conflict "ws" "/tmp/repo")))))

;;;; ---- Tests: cherry-pick-commits end-to-end with verify ----

(ert-deftest agent-repl-test-cherry-pick-commits-verify-fail-aborts-and-signals ()
  "End-to-end: markers cleared by resolver but verify-fail → `cherry-pick
--commits' falls through to `--check-cherry-pick-conflict' which aborts
the cherry-pick and signals user-error."
  (let ((sha-m "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
        (in-progress t)
        (check-called nil)
        (agent-repl-auto-resolve-verify-command '("verify-cmd")))
    (cl-letf (((symbol-function 'agent-repl--git-string)
               (lambda (&rest args)
                 (pcase args
                   (`("-C" "/tmp/repo" "rev-list" "--count" ,_) "1")
                   (_ (error "unmocked git-string args: %S" args)))))
              ((symbol-function 'agent-repl--git-string-quiet)
               (lambda (&rest _args) "abc1234\tfeat: one"))
              ((symbol-function 'agent-repl--git-exit-code-streaming)
               (lambda (_root _filter &rest args)
                 (pcase args
                   (`("cherry-pick" "-x" ,_) 1)
                   (_ (error "unmocked git-exit-code-streaming args: %S" args)))))
              ((symbol-function 'agent-repl--git-exit-code)
               (lambda (_root &rest args)
                 (pcase args
                   ;; Wrapper always issues --abort on unwind; no-op here.
                   (`("cherry-pick" "--abort") 1)
                   (_ (error "unmocked git-exit-code args: %S" args)))))
              ((symbol-function 'agent-repl--cherry-pick-in-progress-p)
               (lambda (_root) in-progress))
              ;; Auto-resolve decides to decline (verify fails).
              ((symbol-function 'agent-repl--auto-resolve-cherry-pick-conflict)
               (lambda (_target-ws _root) nil))
              ;; Stub the abort path: marks the on-disk state cleared and signals.
              ((symbol-function 'agent-repl--check-cherry-pick-conflict)
               (lambda (_ws _root _target-ws)
                 (setq check-called t
                       in-progress nil)
                 (user-error "Conflict cherry-picking from feature — aborted"))))
      (agent-repl-test--with-merge-state
        (should-error (agent-repl--cherry-pick-commits
                       "/tmp/repo" "feature" sha-m "feature" t)
                      :type 'user-error)
        (should check-called)
        (should-not in-progress)))))

;;;; ---- Tests: cherry-pick-commits with auto-resolve ----

(ert-deftest agent-repl-test-cherry-pick-commits-auto-resolve-success-advances-merge ()
  "When auto-resolve clears the markers, `--cherry-pick-commits' stages
and runs `cherry-pick --continue', completing the merge cleanly.
Returns nil (clean cherry-pick), and the loop exits when CHERRY_PICK_HEAD
is gone."
  (let ((sha-m "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
        ;; First call returns t (conflict from initial cherry-pick), then
        ;; nil after the resolver runs and continue lands.
        (in-progress-states '(t nil))
        (continue-called nil))
    (cl-letf (((symbol-function 'agent-repl--git-string)
               (lambda (&rest args)
                 (pcase args
                   (`("-C" "/tmp/repo" "rev-list" "--count" ,_) "1")
                   (_ (error "unmocked git-string args: %S" args)))))
              ;; The pick seeds progress with the range's commits, so the log
              ;; probe is now on the path.
              ((symbol-function 'agent-repl--git-string-quiet)
               (lambda (&rest _args) "abc1234\tfeat: one"))
              ;; The pick streams (`--git-exit-code-streaming') so the drawer can
              ;; follow it commit by commit; --abort still goes through the
              ;; plain, output-discarding wrapper.
              ((symbol-function 'agent-repl--git-exit-code-streaming)
               (lambda (_root _filter &rest args)
                 (pcase args
                   (`("cherry-pick" "-x" ,_) 1)
                   (_ (error "unmocked git-exit-code-streaming args: %S" args)))))
              ((symbol-function 'agent-repl--git-exit-code)
               (lambda (_root &rest args)
                 (pcase args
                   ;; Wrapper always issues --abort on unwind; no-op here.
                   (`("cherry-pick" "--abort") 1)
                   (_ (error "unmocked git-exit-code args: %S" args)))))
              ((symbol-function 'agent-repl--cherry-pick-in-progress-p)
               (lambda (_root) (pop in-progress-states)))
              ((symbol-function 'agent-repl--auto-resolve-cherry-pick-conflict)
               (lambda (_target-ws _root) t))
              ((symbol-function 'agent-repl--continue-cherry-pick-after-resolve)
               (lambda (_target-ws _root) (setq continue-called t) 0)))
      (should (null (agent-repl--cherry-pick-commits
                     "/tmp/repo" "feature" sha-m "feature" t)))
      (should continue-called))))

(ert-deftest agent-repl-test-cherry-pick-commits-auto-resolve-decline-falls-back-to-magit ()
  "When auto-resolve cannot clear the markers, `--cherry-pick-commits'
falls through to `--check-cherry-pick-conflict' which aborts the
cherry-pick and signals user-error."
  (let ((sha-m "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
        (in-progress t))
    (cl-letf (((symbol-function 'agent-repl--git-string)
               (lambda (&rest args)
                 (pcase args
                   (`("-C" "/tmp/repo" "rev-list" "--count" ,_) "1")
                   (_ (error "unmocked git-string args: %S" args)))))
              ;; The pick seeds progress with the range's commits, so the log
              ;; probe is now on the path.
              ((symbol-function 'agent-repl--git-string-quiet)
               (lambda (&rest _args) "abc1234\tfeat: one"))
              ;; The pick streams (`--git-exit-code-streaming') so the drawer can
              ;; follow it commit by commit; --abort still goes through the
              ;; plain, output-discarding wrapper.
              ((symbol-function 'agent-repl--git-exit-code-streaming)
               (lambda (_root _filter &rest args)
                 (pcase args
                   (`("cherry-pick" "-x" ,_) 1)
                   (_ (error "unmocked git-exit-code-streaming args: %S" args)))))
              ((symbol-function 'agent-repl--git-exit-code)
               (lambda (_root &rest args)
                 (pcase args
                   ;; Wrapper always issues --abort on unwind; no-op here.
                   (`("cherry-pick" "--abort") 1)
                   (_ (error "unmocked git-exit-code args: %S" args)))))
              ((symbol-function 'agent-repl--cherry-pick-in-progress-p)
               (lambda (_root) in-progress))
              ((symbol-function 'agent-repl--auto-resolve-cherry-pick-conflict)
               (lambda (_target-ws _root) nil))
              ((symbol-function 'agent-repl--check-cherry-pick-conflict)
               (lambda (_ws _root _target-ws)
                 (setq in-progress nil)
                 (user-error "Conflict cherry-picking — aborted"))))
      (should-error (agent-repl--cherry-pick-commits
                     "/tmp/repo" "feature" sha-m "feature" t)
                    :type 'user-error))))

(ert-deftest agent-repl-test-cherry-pick-commits-auto-resolve-off-still-signals ()
  "With auto-resolve omitted (interactive `SPC TAB m'/`SPC TAB M' path),
conflicts abort the cherry-pick and signal user-error.  Guards against
the optional auto-resolve parameter accidentally flipping the default
for existing callers."
  (let ((sha-m "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
        (in-progress t)
        (resolver-called nil))
    (cl-letf (((symbol-function 'agent-repl--git-string)
               (lambda (&rest args)
                 (pcase args
                   (`("-C" "/tmp/repo" "rev-list" "--count" ,_) "1")
                   (_ (error "unmocked git-string args: %S" args)))))
              ;; The pick seeds progress with the range's commits, so the log
              ;; probe is now on the path.
              ((symbol-function 'agent-repl--git-string-quiet)
               (lambda (&rest _args) "abc1234\tfeat: one"))
              ;; The pick streams (`--git-exit-code-streaming') so the drawer can
              ;; follow it commit by commit; --abort still goes through the
              ;; plain, output-discarding wrapper.
              ((symbol-function 'agent-repl--git-exit-code-streaming)
               (lambda (_root _filter &rest args)
                 (pcase args
                   (`("cherry-pick" "-x" ,_) 1)
                   (_ (error "unmocked git-exit-code-streaming args: %S" args)))))
              ((symbol-function 'agent-repl--git-exit-code)
               (lambda (_root &rest args)
                 (pcase args
                   ;; Wrapper always issues --abort on unwind; no-op here.
                   (`("cherry-pick" "--abort") 1)
                   (_ (error "unmocked git-exit-code args: %S" args)))))
              ((symbol-function 'agent-repl--cherry-pick-in-progress-p)
               (lambda (_root) in-progress))
              ((symbol-function 'agent-repl--auto-resolve-cherry-pick-conflict)
               (lambda (&rest _args) (setq resolver-called t) t))
              ((symbol-function 'agent-repl--check-cherry-pick-conflict)
               (lambda (_ws _root _target-ws)
                 (setq in-progress nil)
                 (user-error "Conflict cherry-picking — aborted"))))
      ;; No auto-resolve arg passed → the resolver MUST NOT be consulted.
      (should-error (agent-repl--cherry-pick-commits
                     "/tmp/repo" "feature" sha-m "feature")
                    :type 'user-error)
      (should-not resolver-called))))

;;;; ---- Tests: silent-mode conflict surfacing ----

(ert-deftest agent-repl-test-cherry-pick-commits-silent-conflict-surfaces-not-aborts ()
  "When SILENT=t and the resolver declines, the conflict is surfaced via
`--surface-silent-merge-conflict' (switch + magit pop + signal) instead
of being aborted via `--check-cherry-pick-conflict'."
  (let ((sha-m "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
        (in-progress t)
        (surface-called nil)
        (abort-called nil))
    (cl-letf (((symbol-function 'agent-repl--git-string)
               (lambda (&rest args)
                 (pcase args
                   (`("-C" "/tmp/repo" "rev-list" "--count" ,_) "1")
                   (_ (error "unmocked git-string args: %S" args)))))
              ;; The pick seeds progress with the range's commits, so the log
              ;; probe is now on the path.
              ((symbol-function 'agent-repl--git-string-quiet)
               (lambda (&rest _args) "abc1234\tfeat: one"))
              ;; The pick streams (`--git-exit-code-streaming') so the drawer can
              ;; follow it commit by commit; --abort still goes through the
              ;; plain, output-discarding wrapper.
              ((symbol-function 'agent-repl--git-exit-code-streaming)
               (lambda (_root _filter &rest args)
                 (pcase args
                   (`("cherry-pick" "-x" ,_) 1)
                   (_ (error "unmocked git-exit-code-streaming args: %S" args)))))
              ((symbol-function 'agent-repl--git-exit-code)
               (lambda (_root &rest args)
                 (pcase args
                   ;; Wrapper issues --abort on unwind; allow it (no-op).
                   (`("cherry-pick" "--abort") 1)
                   (_ (error "unmocked git-exit-code args: %S" args)))))
              ((symbol-function 'agent-repl--cherry-pick-in-progress-p)
               (lambda (_root) in-progress))
              ((symbol-function 'agent-repl--auto-resolve-cherry-pick-conflict)
               (lambda (&rest _args) nil))
              ((symbol-function 'agent-repl--surface-silent-merge-conflict)
               (lambda (_ws _root)
                 (setq surface-called t
                       in-progress nil)
                 (user-error "surfaced")))
              ((symbol-function 'agent-repl--check-cherry-pick-conflict)
               (lambda (&rest _) (setq abort-called t))))
      (should-error (agent-repl--cherry-pick-commits
                     "/tmp/repo" "feature" sha-m "feature" t t)
                    :type 'user-error)
      (should surface-called)
      (should-not abort-called))))

(ert-deftest agent-repl-test-cherry-pick-commits-non-silent-conflict-aborts ()
  "When SILENT=nil and the resolver declines, the conflict is aborted
via `--check-cherry-pick-conflict' (existing behavior) — the surface
helper is not invoked.  Guards the interactive `SPC TAB M' path."
  (let ((sha-m "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
        (in-progress t)
        (surface-called nil))
    (cl-letf (((symbol-function 'agent-repl--git-string)
               (lambda (&rest args)
                 (pcase args
                   (`("-C" "/tmp/repo" "rev-list" "--count" ,_) "1")
                   (_ (error "unmocked git-string args: %S" args)))))
              ;; The pick seeds progress with the range's commits, so the log
              ;; probe is now on the path.
              ((symbol-function 'agent-repl--git-string-quiet)
               (lambda (&rest _args) "abc1234\tfeat: one"))
              ;; The pick streams (`--git-exit-code-streaming') so the drawer can
              ;; follow it commit by commit; --abort still goes through the
              ;; plain, output-discarding wrapper.
              ((symbol-function 'agent-repl--git-exit-code-streaming)
               (lambda (_root _filter &rest args)
                 (pcase args
                   (`("cherry-pick" "-x" ,_) 1)
                   (_ (error "unmocked git-exit-code-streaming args: %S" args)))))
              ((symbol-function 'agent-repl--git-exit-code)
               (lambda (_root &rest args)
                 (pcase args
                   ;; Wrapper always issues --abort on unwind; no-op here.
                   (`("cherry-pick" "--abort") 1)
                   (_ (error "unmocked git-exit-code args: %S" args)))))
              ((symbol-function 'agent-repl--cherry-pick-in-progress-p)
               (lambda (_root) in-progress))
              ((symbol-function 'agent-repl--auto-resolve-cherry-pick-conflict)
               (lambda (&rest _args) nil))
              ((symbol-function 'agent-repl--surface-silent-merge-conflict)
               (lambda (&rest _) (setq surface-called t)))
              ((symbol-function 'agent-repl--check-cherry-pick-conflict)
               (lambda (_ws _root _target-ws)
                 (setq in-progress nil)
                 (user-error "Conflict cherry-picking — aborted"))))
      (should-error (agent-repl--cherry-pick-commits
                     "/tmp/repo" "feature" sha-m "feature" t nil)
                    :type 'user-error)
      (should-not surface-called))))

(ert-deftest agent-repl-test-surface-silent-merge-conflict-pops-magit-and-signals ()
  "`--surface-silent-merge-conflict' switches to ROOT, pops magit-status
there, then signals `user-error'.  Does NOT call `git cherry-pick
--abort' — that would erase the conflict the user is being asked to
inspect."
  (let ((switched-to nil)
        (magit-pop-root nil)
        (abort-called nil))
    (cl-letf (((symbol-function 'agent-repl--git-string)
               (lambda (&rest args)
                 (pcase args
                   (`("-C" "/tmp/repo" "rev-parse" "--short" "CHERRY_PICK_HEAD") "abcd123")
                   (_ (error "unmocked git-string args: %S" args)))))
              ((symbol-function 'agent-repl--git-exit-code)
               (lambda (&rest _args) (setq abort-called t) 0))
              ;; The function defers UI ops; run the thunk synchronously so
              ;; the test can assert the switch and magit-status calls.
              ((symbol-function 'agent-repl--defer-to-main-thread)
               (lambda (thunk) (funcall thunk)))
              ((symbol-function 'agent-repl-switch-to-project)
               (lambda (dir) (setq switched-to dir)))
              ((symbol-function 'magit-status)
               (lambda (dir) (setq magit-pop-root dir))))
      (should-error (agent-repl--surface-silent-merge-conflict
                     "feature" "/tmp/repo")
                    :type 'user-error)
      (should (equal switched-to "/tmp/repo"))
      (should (equal magit-pop-root "/tmp/repo"))
      ;; --abort must NOT have been issued — the conflict stays inspectable.
      (should-not abort-called))))

(ert-deftest agent-repl-test-surface-silent-merge-conflict-defers-ui-ops ()
  "UI ops (perspective switch + magit-status) are routed through
`agent-repl--defer-to-main-thread'.  This is what makes the function
safe to call from the worker thread spawned by
`agent-repl--workspace-merge-async' — direct UI calls from a worker
thread are undefined behavior in Emacs."
  (let ((defer-calls 0))
    (cl-letf (((symbol-function 'agent-repl--git-string)
               (lambda (&rest _args) "abcd123"))
              ((symbol-function 'agent-repl--defer-to-main-thread)
               (lambda (_thunk) (cl-incf defer-calls))))
      (should-error (agent-repl--surface-silent-merge-conflict
                     "feature" "/tmp/repo")
                    :type 'user-error)
      (should (= defer-calls 1)))))

;;;; ---- Tests: resolver output is preserved in a side buffer ----

(ert-deftest agent-repl-test-invoke-auto-resolve-agent-preserves-output-buffer ()
  "When TARGET-WS is supplied, `--invoke-auto-resolve-agent' leaves the
side buffer alive after the process exits so the user can post-mortem
the resolver's stdout/stderr + exit code."
  (let ((agent-repl-auto-resolve-conflicts-program "true")
        (agent-repl-auto-resolve-conflicts-model "test-model")
        (agent-repl-auto-resolve-conflicts-extra-args nil)
        (agent-repl-auto-resolve-conflicts-timeout 5)
        (ws "feature-x"))
    (let ((buf-name (agent-repl--merge-resolver-buffer-name ws)))
      (when (get-buffer buf-name) (kill-buffer buf-name))
      (unwind-protect
          (let ((result (agent-repl--invoke-auto-resolve-agent
                         default-directory "prompt-body" ws)))
            (should (equal result 0))
            (should (buffer-live-p (get-buffer buf-name)))
            (with-current-buffer buf-name
              (let ((content (buffer-string)))
                (should (string-match-p "merge resolver" content))
                (should (string-match-p "feature-x" content))
                (should (string-match-p "exit: 0" content)))))
        (when (get-buffer buf-name) (kill-buffer buf-name))))))

(ert-deftest agent-repl-test-invoke-auto-resolve-agent-no-target-ws-kills-temp-buffer ()
  "Legacy callers (no TARGET-WS argument) get the old behavior: the
temp buffer is killed after the process completes, so we don't leak
anonymous \" *agent-auto-resolve*\" buffers."
  (let* ((agent-repl-auto-resolve-conflicts-program "true")
         (agent-repl-auto-resolve-conflicts-model "test-model")
         (agent-repl-auto-resolve-conflicts-extra-args nil)
         (agent-repl-auto-resolve-conflicts-timeout 5)
         (anon-p (lambda (b)
                   (string-prefix-p " *agent-auto-resolve*" (buffer-name b))))
         ;; Snapshot the exact anon buffers alive BEFORE the call and assert
         ;; only on the anon buffers this invocation NEWLY leaves alive
         ;; (set difference), never a global count.  A global pre/post count
         ;; is racy: `agent-repl--invoke-auto-resolve-agent' busy-waits via
         ;; `accept-process-output', which services pending sentinels/timers
         ;; from OTHER tests' lingering resolver processes.  If such a
         ;; callback kills a pre-existing anon buffer mid-wait, a count
         ;; comparison sees post=pre-1 and fails spuriously.  The set
         ;; difference inspects only buffers created during this call, so
         ;; unrelated concurrent kills can no longer perturb it.
         (before (cl-remove-if-not anon-p (buffer-list))))
    (let ((result (agent-repl--invoke-auto-resolve-agent
                   default-directory "prompt-body")))
      (should (equal result 0))
      (let ((leaked (cl-remove-if
                     (lambda (b) (memq b before))
                     (cl-remove-if-not anon-p (buffer-list)))))
        (should (null leaked))))))

(ert-deftest agent-repl-test-invoke-auto-resolve-agent-no-leak-despite-concurrent-anon-kill ()
  "Regression for the intermittent buffer-accounting flake: the legacy
\(no TARGET-WS) path must not leak its OWN anonymous temp buffer even
when an UNRELATED anon buffer is killed during the process wait —
exactly what a lingering resolver sentinel from another test does when
serviced by this call's `accept-process-output' busy-wait.  Asserting
on the set of anon buffers newly left alive (rather than a global
count) makes the invariant hold regardless of that concurrent churn;
the old global-count assertion would have seen post=pre-1 here and
failed."
  (let* ((agent-repl-auto-resolve-conflicts-program "true")
         (agent-repl-auto-resolve-conflicts-model "test-model")
         (agent-repl-auto-resolve-conflicts-extra-args nil)
         (agent-repl-auto-resolve-conflicts-timeout 5)
         (anon-p (lambda (b)
                   (string-prefix-p " *agent-auto-resolve*" (buffer-name b))))
         ;; A pre-existing anon buffer standing in for one leaked by another
         ;; test; captured into BEFORE so it is not itself counted as a leak.
         (decoy (generate-new-buffer " *agent-auto-resolve*"))
         (before (cl-remove-if-not anon-p (buffer-list))))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-repl--wait-for-process-exit)
                   (lambda (&rest _)
                     ;; Simulate the concurrent kill that a sibling test's
                     ;; lingering sentinel would perform mid-wait.
                     (when (buffer-live-p decoy) (kill-buffer decoy))
                     0)))
          (let ((result (agent-repl--invoke-auto-resolve-agent
                         default-directory "prompt-body")))
            (should (equal result 0))
            (let ((leaked (cl-remove-if
                           (lambda (b) (memq b before))
                           (cl-remove-if-not anon-p (buffer-list)))))
              (should (null leaked)))))
      (when (buffer-live-p decoy) (kill-buffer decoy)))))

(ert-deftest agent-repl-test-invoke-auto-resolve-agent-erases-prior-output ()
  "A second resolver invocation overwrites the prior buffer's content
\(prefixed with the new header) instead of appending — the buffer
always reflects the most recent run."
  (let ((agent-repl-auto-resolve-conflicts-program "true")
        (agent-repl-auto-resolve-conflicts-model "test-model")
        (agent-repl-auto-resolve-conflicts-extra-args nil)
        (agent-repl-auto-resolve-conflicts-timeout 5)
        (ws "feature-erase"))
    (let ((buf-name (agent-repl--merge-resolver-buffer-name ws)))
      (when (get-buffer buf-name) (kill-buffer buf-name))
      (unwind-protect
          (progn
            (with-current-buffer (get-buffer-create buf-name)
              (let ((inhibit-read-only t))
                (insert "STALE-PREVIOUS-CONTENT\n")))
            (agent-repl--invoke-auto-resolve-agent
             default-directory "prompt-body" ws)
            (with-current-buffer buf-name
              (should-not (string-match-p "STALE-PREVIOUS-CONTENT"
                                          (buffer-string)))))
        (when (get-buffer buf-name) (kill-buffer buf-name))))))

;;;; ---- Tests: handle-merge-command auto-resolve gating ----

(ert-deftest agent-repl-test-handle-merge-command-passes-auto-resolve ()
  "Skill-invoked `/workspace-merge' passes AUTO-RESOLVE=t to
workspace-merge-into-source so cherry-pick conflicts are sent to the
headless resolver — interactive paths leave it nil."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "feature-one" :project-dir "/tmp/feature-one")
    (let ((auto-arg :unset))
      (cl-letf (((symbol-function 'agent-repl--workspace-merge-into-source)
                 (lambda (_ws &optional _silent auto) (setq auto-arg auto))))
        (agent-repl--handle-merge-command
         '((type . "merge") (workspace . "feature-one")))
        (should (eq auto-arg t))))))

;;;; ---- Tests: workspace-merge-async ----

(ert-deftest agent-repl-test-workspace-merge-async-closes-workspace-with-preserve-entry ()
  "Async wrapper closes the workspace UI FIRST (with `preserve-entry') so
the user is freed from it immediately on keystroke return.  Preserve-entry
keeps `:project-dir' alive so the reopen-on-failure path can find it."
  (let ((close-args nil))
    (cl-letf (((symbol-function 'agent-repl--workspace-merge-async)
               agent-repl-test--orig-workspace-merge-async)
              ((symbol-function 'agent-repl--close-workspace)
               (lambda (ws preserve) (setq close-args (list ws preserve))))
              ((symbol-function 'make-thread)
               (lambda (_thunk &optional _name) nil)))
      (agent-repl--workspace-merge-async "ws1" "/tmp/repo"))
    (should (equal close-args (list "ws1" 'preserve-entry)))))

(ert-deftest agent-repl-test-workspace-merge-async-spawns-worker-thread ()
  "Async wrapper spawns a `make-thread' (not a synchronous call) so Emacs
stays responsive while `claude -p' runs in the merge body — threads yield
during `accept-process-output' so the main UI keeps ticking."
  (let ((thread-spawned nil))
    (cl-letf (((symbol-function 'agent-repl--workspace-merge-async)
               agent-repl-test--orig-workspace-merge-async)
              ((symbol-function 'agent-repl--close-workspace) #'ignore)
              ((symbol-function 'make-thread)
               (lambda (_thunk &optional _name) (setq thread-spawned t) nil)))
      (agent-repl--workspace-merge-async "ws1" "/tmp/repo"))
    (should thread-spawned)))

(ert-deftest agent-repl-test-workspace-merge-async-thread-runs-dispatch-handler ()
  "Inside the worker thread the wrapper invokes `--dispatch-merge-handler'
\(the standard handler-routing entry).  This is what makes the two entry
points (interactive `SPC TAB M' and `/workspace-merge' skill) equivalent —
both end up here via the same dispatch."
  (let ((dispatch-args nil))
    (cl-letf (((symbol-function 'agent-repl--workspace-merge-async)
               agent-repl-test--orig-workspace-merge-async)
              ((symbol-function 'agent-repl--close-workspace) #'ignore)
              ;; Run the thread body inline so we can observe the dispatch
              ;; call without thread-join machinery.
              ((symbol-function 'make-thread)
               (lambda (thunk &optional _name) (funcall thunk) nil))
              ((symbol-function 'agent-repl--dispatch-merge-handler)
               (lambda (ws repo-root &optional onto-master)
                 (setq dispatch-args (list ws repo-root onto-master)))))
      (agent-repl--workspace-merge-async "ws1" "/tmp/repo"))
    ;; onto-master defaults to nil when not supplied.
    (should (equal dispatch-args (list "ws1" "/tmp/repo" nil)))))

(ert-deftest agent-repl-test-workspace-merge-async-threads-onto-master ()
  "The optional ONTO-MASTER arg is forwarded verbatim to
`--dispatch-merge-handler' so the `/workspace-merge --onto-master' intent
reaches the handler-routing layer."
  (let ((dispatch-args nil))
    (cl-letf (((symbol-function 'agent-repl--workspace-merge-async)
               agent-repl-test--orig-workspace-merge-async)
              ((symbol-function 'agent-repl--close-workspace) #'ignore)
              ((symbol-function 'make-thread)
               (lambda (thunk &optional _name) (funcall thunk) nil))
              ((symbol-function 'agent-repl--dispatch-merge-handler)
               (lambda (ws repo-root &optional onto-master)
                 (setq dispatch-args (list ws repo-root onto-master)))))
      (agent-repl--workspace-merge-async "ws1" "/tmp/repo" t))
    (should (equal dispatch-args (list "ws1" "/tmp/repo" t)))))

(ert-deftest agent-repl-test-workspace-merge-async-on-error-schedules-reopen ()
  "When the dispatch handler signals (conflict or any error), the wrapper's
condition-case catches it and schedules `--reopen-workspace-from-state'
on the main thread via `run-at-time'.  Without this, a failed merge
leaves the user with a closed workspace and no way to recover.

The failure arm also re-enqueues onto the merge queue and dispatches a
agent-send prompt; this test asserts only the reopen scheduling and
stubs the rest."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-empty-merge-queue
      (let ((scheduled nil))
        (cl-letf (((symbol-function 'agent-repl--workspace-merge-async)
                   agent-repl-test--orig-workspace-merge-async)
                  ((symbol-function 'agent-repl--close-workspace) #'ignore)
                  ((symbol-function 'make-thread)
                   (lambda (thunk &optional _name) (funcall thunk) nil))
                  ((symbol-function 'agent-repl--dispatch-merge-handler)
                   (lambda (&rest _) (error "boom")))
                  ((symbol-function 'agent-repl--dispatch-prompt-command) #'ignore)
                  ((symbol-function 'agent-repl--persist-merge-queue) #'ignore)
                  ((symbol-function 'run-at-time)
                   (lambda (_when _repeat thunk)
                     (push thunk scheduled))))
          (agent-repl--workspace-merge-async "ws1" "/tmp/repo"))
        (should (= (length scheduled) 1))
        ;; Invoking the scheduled thunk should call --reopen-workspace-from-state
        ;; with the workspace name.
        (let ((reopened nil))
          (cl-letf (((symbol-function 'agent-repl--reopen-workspace-from-state)
                     (lambda (ws) (setq reopened ws)))
                    ((symbol-function 'agent-repl--dispatch-prompt-command) #'ignore))
            (funcall (car scheduled)))
          (should (equal reopened "ws1")))))))

(ert-deftest agent-repl-test-workspace-merge-async-on-success-does-not-schedule-reopen ()
  "When dispatch completes without signaling, the wrapper does NOT schedule
a reopen — the merge body's own deferred teardown is the cleanup path."
  (let ((scheduled nil))
    (cl-letf (((symbol-function 'agent-repl--workspace-merge-async)
               agent-repl-test--orig-workspace-merge-async)
              ((symbol-function 'agent-repl--close-workspace) #'ignore)
              ((symbol-function 'make-thread)
               (lambda (thunk &optional _name) (funcall thunk) nil))
              ((symbol-function 'agent-repl--dispatch-merge-handler) #'ignore)
              ((symbol-function 'run-at-time)
               (lambda (_when _repeat thunk) (push thunk scheduled))))
      (agent-repl--workspace-merge-async "ws1" "/tmp/repo"))
    (should-not scheduled)))

(ert-deftest agent-repl-test-workspace-merge-async-on-error-aborts-cherry-pick-when-in-flight ()
  "When the dispatch handler signals AND a cherry-pick is in flight at the
resolved target dir, the error arm runs `git cherry-pick --abort' before
re-enqueueing.  Without the abort the target tree would stay half-merged
on the next attempt."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-empty-merge-queue
      (let ((aborted nil))
        (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
        (agent-repl--ws-put "ws1" :resolved-target-dir "/tmp/target")
        (cl-letf (((symbol-function 'agent-repl--workspace-merge-async)
                   agent-repl-test--orig-workspace-merge-async)
                  ((symbol-function 'agent-repl--close-workspace) #'ignore)
                  ((symbol-function 'make-thread)
                   (lambda (thunk &optional _name) (funcall thunk) nil))
                  ((symbol-function 'agent-repl--dispatch-merge-handler)
                   (lambda (&rest _) (error "boom")))
                  ((symbol-function 'file-directory-p) (lambda (_) t))
                  ((symbol-function 'agent-repl--cherry-pick-in-progress-p)
                   (lambda (_) t))
                  ((symbol-function 'agent-repl--git-exit-code)
                   (lambda (dir &rest args)
                     (when (and (string= dir "/tmp/target")
                                (equal args '("cherry-pick" "--abort")))
                       (setq aborted t))
                     0))
                  ((symbol-function 'agent-repl--current-head-sha)
                   (lambda (_) "deadbeef"))
                  ((symbol-function 'run-at-time) #'ignore)
                  ((symbol-function 'agent-repl--persist-merge-queue) #'ignore))
          (agent-repl--workspace-merge-async "ws1" "/tmp/ws1"))
        (should aborted)))))

(ert-deftest agent-repl-test-workspace-merge-async-on-error-does-not-abort-when-no-cherry-pick ()
  "Pre-flight failure (no CHERRY_PICK_HEAD at target) must NOT invoke
`cherry-pick --abort'.  Calling abort when nothing is mid-flight would
emit a spurious git error."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-empty-merge-queue
      (let ((abort-called nil))
        (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
        (agent-repl--ws-put "ws1" :resolved-target-dir "/tmp/target")
        (cl-letf (((symbol-function 'agent-repl--workspace-merge-async)
                   agent-repl-test--orig-workspace-merge-async)
                  ((symbol-function 'agent-repl--close-workspace) #'ignore)
                  ((symbol-function 'make-thread)
                   (lambda (thunk &optional _name) (funcall thunk) nil))
                  ((symbol-function 'agent-repl--dispatch-merge-handler)
                   (lambda (&rest _) (error "boom")))
                  ((symbol-function 'file-directory-p) (lambda (_) t))
                  ((symbol-function 'agent-repl--cherry-pick-in-progress-p)
                   (lambda (_) nil))
                  ((symbol-function 'agent-repl--git-exit-code)
                   (lambda (_dir &rest args)
                     (when (equal args '("cherry-pick" "--abort"))
                       (setq abort-called t))
                     0))
                  ((symbol-function 'agent-repl--current-head-sha)
                   (lambda (_) "deadbeef"))
                  ((symbol-function 'run-at-time) #'ignore)
                  ((symbol-function 'agent-repl--persist-merge-queue) #'ignore))
          (agent-repl--workspace-merge-async "ws1" "/tmp/ws1"))
        (should-not abort-called)))))

(ert-deftest agent-repl-test-workspace-merge-async-on-conflict-error-reenqueues-to-back ()
  "A `agent-repl-merge-conflict-error' is the agent-rejected-the-conflict
case; the workspace re-enters the queue at the BACK so sibling workspaces
get a turn before this one is retried."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-empty-merge-queue
      (agent-repl--ws-put "ws-front" :project-dir "/tmp/wsf")
      (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
      (agent-repl--ws-put "ws1" :resolved-target-dir "/tmp/target")
      (setq agent-repl--merge-queue
            (list (list :source-ws "ws-front" :silent t :auto-resolve t)))
      (cl-letf (((symbol-function 'agent-repl--workspace-merge-async)
                 agent-repl-test--orig-workspace-merge-async)
                ((symbol-function 'agent-repl--close-workspace) #'ignore)
                ((symbol-function 'make-thread)
                 (lambda (thunk &optional _name) (funcall thunk) nil))
                ((symbol-function 'agent-repl--dispatch-merge-handler)
                 (lambda (&rest _)
                   (signal 'agent-repl-merge-conflict-error '("rejected"))))
                ((symbol-function 'file-directory-p) (lambda (_) t))
                ((symbol-function 'agent-repl--cherry-pick-in-progress-p)
                 (lambda (_) nil))
                ((symbol-function 'agent-repl--current-head-sha)
                 (lambda (_) "deadbeef"))
                ((symbol-function 'run-at-time) #'ignore)
                ((symbol-function 'agent-repl--drain-merge-queue) #'ignore)
                ((symbol-function 'agent-repl--persist-merge-queue) #'ignore))
        (agent-repl--workspace-merge-async "ws1" "/tmp/ws1"))
      (should (equal (mapcar (lambda (e) (plist-get e :source-ws))
                             agent-repl--merge-queue)
                     '("ws-front" "ws1")))
      ;; Back-pushed entry carries no halt flag — drain may continue.
      (should-not (plist-get (nth 1 agent-repl--merge-queue) :halt-until-human))
      ;; Loop-guard SHA is recorded so a same-tip retry can be detected.
      (should (equal (plist-get (nth 1 agent-repl--merge-queue)
                                :last-attempt-target-head)
                     "deadbeef")))))

(ert-deftest agent-repl-test-workspace-merge-async-on-generic-error-reenqueues-to-front-with-halt-flag ()
  "Generic (non-conflict) failure goes to the FRONT of the queue with
`:halt-until-human t' so auto-drain does not retry until a human kick.
Without the halt flag the next drain would pop the same entry right
back out and loop the same failure."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-empty-merge-queue
      (agent-repl--ws-put "ws-existing" :project-dir "/tmp/wse")
      (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
      (agent-repl--ws-put "ws1" :resolved-target-dir "/tmp/target")
      (setq agent-repl--merge-queue
            (list (list :source-ws "ws-existing" :silent t :auto-resolve t)))
      (cl-letf (((symbol-function 'agent-repl--workspace-merge-async)
                 agent-repl-test--orig-workspace-merge-async)
                ((symbol-function 'agent-repl--close-workspace) #'ignore)
                ((symbol-function 'make-thread)
                 (lambda (thunk &optional _name) (funcall thunk) nil))
                ((symbol-function 'agent-repl--dispatch-merge-handler)
                 (lambda (&rest _) (error "boom")))
                ((symbol-function 'file-directory-p) (lambda (_) t))
                ((symbol-function 'agent-repl--cherry-pick-in-progress-p)
                 (lambda (_) nil))
                ((symbol-function 'agent-repl--current-head-sha)
                 (lambda (_) "cafef00d"))
                ((symbol-function 'run-at-time) #'ignore)
                ((symbol-function 'agent-repl--drain-merge-queue) #'ignore)
                ((symbol-function 'agent-repl--persist-merge-queue) #'ignore))
        (agent-repl--workspace-merge-async "ws1" "/tmp/ws1"))
      (should (equal (mapcar (lambda (e) (plist-get e :source-ws))
                             agent-repl--merge-queue)
                     '("ws1" "ws-existing")))
      (should (plist-get (car agent-repl--merge-queue) :halt-until-human))
      (should (equal (plist-get (car agent-repl--merge-queue)
                                :last-attempt-target-head)
                     "cafef00d")))))

(ert-deftest agent-repl-test-workspace-merge-async-on-error-dispatches-agent-send-with-analyze-directive ()
  "The deferred main-thread thunk calls
`agent-repl--dispatch-prompt-command' with a prompt that embeds the
error and ends with the analyze-only directive.  Without this the
workspace's agent has no in-band signal that a merge failed."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-empty-merge-queue
      (let ((deferred-thunks nil)
            (dispatched-prompt nil))
        (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
        (agent-repl--ws-put "ws1" :resolved-target-dir "/tmp/target")
        (cl-letf (((symbol-function 'agent-repl--workspace-merge-async)
                   agent-repl-test--orig-workspace-merge-async)
                  ((symbol-function 'agent-repl--close-workspace) #'ignore)
                  ((symbol-function 'make-thread)
                   (lambda (thunk &optional _name) (funcall thunk) nil))
                  ((symbol-function 'agent-repl--dispatch-merge-handler)
                   (lambda (&rest _) (error "boom")))
                  ((symbol-function 'file-directory-p) (lambda (_) t))
                  ((symbol-function 'agent-repl--cherry-pick-in-progress-p)
                   (lambda (_) nil))
                  ((symbol-function 'agent-repl--current-head-sha)
                   (lambda (_) "deadbeef"))
                  ((symbol-function 'agent-repl--reopen-workspace-from-state)
                   #'ignore)
                  ((symbol-function 'agent-repl--dispatch-prompt-command)
                   (lambda (_ws prompt) (setq dispatched-prompt prompt)))
                  ((symbol-function 'run-at-time)
                   (lambda (_when _repeat thunk) (push thunk deferred-thunks)))
                  ((symbol-function 'agent-repl--drain-merge-queue) #'ignore)
                  ((symbol-function 'agent-repl--persist-merge-queue) #'ignore))
          (agent-repl--workspace-merge-async "ws1" "/tmp/ws1")
          (should (= 1 (length deferred-thunks)))
          (funcall (car deferred-thunks)))
        (should (stringp dispatched-prompt))
        (should (string-match-p "merge attempt for this workspace just failed"
                                dispatched-prompt))
        (should (string-match-p "/workspace-merge"
                                dispatched-prompt))
        (should (string-match-p "boom" dispatched-prompt))))))

(ert-deftest agent-repl-test-workspace-merge-async-on-conflict-error-drains-queue ()
  "Conflict-rejection is recoverable; the wrapper calls
`--drain-merge-queue' so a sibling workspace can attempt its own merge
while the rejecting workspace waits at the back."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-empty-merge-queue
      (let ((drained nil))
        (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
        (agent-repl--ws-put "ws1" :resolved-target-dir "/tmp/target")
        (cl-letf (((symbol-function 'agent-repl--workspace-merge-async)
                   agent-repl-test--orig-workspace-merge-async)
                  ((symbol-function 'agent-repl--close-workspace) #'ignore)
                  ((symbol-function 'make-thread)
                   (lambda (thunk &optional _name) (funcall thunk) nil))
                  ((symbol-function 'agent-repl--dispatch-merge-handler)
                   (lambda (&rest _)
                     (signal 'agent-repl-merge-conflict-error '("rejected"))))
                  ((symbol-function 'file-directory-p) (lambda (_) t))
                  ((symbol-function 'agent-repl--cherry-pick-in-progress-p)
                   (lambda (_) nil))
                  ((symbol-function 'agent-repl--current-head-sha)
                   (lambda (_) "deadbeef"))
                  ((symbol-function 'run-at-time) #'ignore)
                  ((symbol-function 'agent-repl--drain-merge-queue)
                   (lambda () (setq drained t)))
                  ((symbol-function 'agent-repl--persist-merge-queue) #'ignore))
          (agent-repl--workspace-merge-async "ws1" "/tmp/ws1"))
        (should drained)))))

(ert-deftest agent-repl-test-workspace-merge-async-on-generic-error-does-not-drain ()
  "Generic failure must NOT trigger an auto-drain — the front entry's
`:halt-until-human' flag would block drain anyway, but skipping the
call is cheaper and makes the asymmetry explicit at the call site."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-empty-merge-queue
      (let ((drained nil))
        (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
        (agent-repl--ws-put "ws1" :resolved-target-dir "/tmp/target")
        (cl-letf (((symbol-function 'agent-repl--workspace-merge-async)
                   agent-repl-test--orig-workspace-merge-async)
                  ((symbol-function 'agent-repl--close-workspace) #'ignore)
                  ((symbol-function 'make-thread)
                   (lambda (thunk &optional _name) (funcall thunk) nil))
                  ((symbol-function 'agent-repl--dispatch-merge-handler)
                   (lambda (&rest _) (error "boom")))
                  ((symbol-function 'file-directory-p) (lambda (_) t))
                  ((symbol-function 'agent-repl--cherry-pick-in-progress-p)
                   (lambda (_) nil))
                  ((symbol-function 'agent-repl--current-head-sha)
                   (lambda (_) "deadbeef"))
                  ((symbol-function 'run-at-time) #'ignore)
                  ((symbol-function 'agent-repl--drain-merge-queue)
                   (lambda () (setq drained t)))
                  ((symbol-function 'agent-repl--persist-merge-queue) #'ignore))
          (agent-repl--workspace-merge-async "ws1" "/tmp/ws1"))
        (should-not drained)))))

;;;; ---- Tests: reenqueue-and-redrive-on-failure (shared failure core) ----

(ert-deftest agent-repl-test-reenqueue-and-redrive-on-conflict-reenqueues-to-back ()
  "A `agent-repl-merge-conflict-error' re-enqueues WS to the BACK with no
halt flag, so siblings get a turn before this one retries."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-empty-merge-queue
      (agent-repl--ws-put "ws-front" :project-dir "/tmp/wsf")
      (agent-repl--ws-put "ws1" :resolved-target-dir "/tmp/target")
      (setq agent-repl--merge-queue
            (list (list :source-ws "ws-front" :silent t :auto-resolve t)))
      (cl-letf (((symbol-function 'agent-repl--abort-cherry-pick-if-in-flight) #'ignore)
                ((symbol-function 'agent-repl--current-head-sha) (lambda (_) "deadbeef"))
                ((symbol-function 'agent-repl--drain-merge-queue) #'ignore)
                ((symbol-function 'agent-repl--persist-merge-queue) #'ignore))
        (agent-repl--reenqueue-and-redrive-on-failure
         "ws1" '(agent-repl-merge-conflict-error "rejected")))
      (should (equal (mapcar (lambda (e) (plist-get e :source-ws))
                             agent-repl--merge-queue)
                     '("ws-front" "ws1")))
      (should-not (plist-get (nth 1 agent-repl--merge-queue) :halt-until-human)))))

(ert-deftest agent-repl-test-reenqueue-and-redrive-on-conflict-drains ()
  "Conflict-rejection re-drives `--drain-merge-queue' so a sibling can try."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-empty-merge-queue
      (let ((drained nil))
        (agent-repl--ws-put "ws1" :resolved-target-dir "/tmp/target")
        (cl-letf (((symbol-function 'agent-repl--abort-cherry-pick-if-in-flight) #'ignore)
                  ((symbol-function 'agent-repl--current-head-sha) (lambda (_) "deadbeef"))
                  ((symbol-function 'agent-repl--drain-merge-queue) (lambda () (setq drained t)))
                  ((symbol-function 'agent-repl--persist-merge-queue) #'ignore))
          (agent-repl--reenqueue-and-redrive-on-failure
           "ws1" '(agent-repl-merge-conflict-error "rejected")))
        (should drained)))))

(ert-deftest agent-repl-test-reenqueue-and-redrive-on-generic-reenqueues-front-with-halt ()
  "A generic (non-conflict) failure re-enqueues WS to the FRONT with
`:halt-until-human' set."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-empty-merge-queue
      (agent-repl--ws-put "ws-existing" :project-dir "/tmp/wse")
      (agent-repl--ws-put "ws1" :resolved-target-dir "/tmp/target")
      (setq agent-repl--merge-queue
            (list (list :source-ws "ws-existing" :silent t :auto-resolve t)))
      (cl-letf (((symbol-function 'agent-repl--abort-cherry-pick-if-in-flight) #'ignore)
                ((symbol-function 'agent-repl--current-head-sha) (lambda (_) "cafef00d"))
                ((symbol-function 'agent-repl--drain-merge-queue) #'ignore)
                ((symbol-function 'agent-repl--persist-merge-queue) #'ignore))
        (agent-repl--reenqueue-and-redrive-on-failure "ws1" '(error "boom")))
      (should (equal (mapcar (lambda (e) (plist-get e :source-ws))
                             agent-repl--merge-queue)
                     '("ws1" "ws-existing")))
      (should (plist-get (car agent-repl--merge-queue) :halt-until-human)))))

(ert-deftest agent-repl-test-reenqueue-and-redrive-on-generic-does-not-drain ()
  "A generic failure must NOT auto-drain — the halted front entry blocks
the queue until a human kick."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-empty-merge-queue
      (let ((drained nil))
        (agent-repl--ws-put "ws1" :resolved-target-dir "/tmp/target")
        (cl-letf (((symbol-function 'agent-repl--abort-cherry-pick-if-in-flight) #'ignore)
                  ((symbol-function 'agent-repl--current-head-sha) (lambda (_) "cafef00d"))
                  ((symbol-function 'agent-repl--drain-merge-queue) (lambda () (setq drained t)))
                  ((symbol-function 'agent-repl--persist-merge-queue) #'ignore))
          (agent-repl--reenqueue-and-redrive-on-failure "ws1" '(error "boom")))
        (should-not drained)))))

(ert-deftest agent-repl-test-reenqueue-and-redrive-aborts-in-flight-cherry-pick ()
  "The helper aborts any in-flight cherry-pick at WS's resolved target dir
so a conflict cannot leave CHERRY_PICK_HEAD wedged and freeze the queue."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-empty-merge-queue
      (let ((aborted-dir 'unset))
        (agent-repl--ws-put "ws1" :resolved-target-dir "/tmp/target")
        (cl-letf (((symbol-function 'agent-repl--abort-cherry-pick-if-in-flight)
                   (lambda (_ws dir) (setq aborted-dir dir)))
                  ((symbol-function 'agent-repl--current-head-sha) (lambda (_) "deadbeef"))
                  ((symbol-function 'agent-repl--drain-merge-queue) #'ignore)
                  ((symbol-function 'agent-repl--persist-merge-queue) #'ignore))
          (agent-repl--reenqueue-and-redrive-on-failure
           "ws1" '(agent-repl-merge-conflict-error "rejected")))
        (should (equal aborted-dir "/tmp/target"))))))

;;;; ---- Tests: drain-merge-queue routes failures through shared recovery ----

(ert-deftest agent-repl-test-drain-merge-queue-on-failure-runs-shared-recovery ()
  "When a drained merge fails, `--drain-merge-queue' routes the error
through `--reenqueue-and-redrive-on-failure' — the same recovery the
async-dispatch path uses — so a drained conflict cannot leave the queue
wedged (the historical drain-path bug)."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-empty-merge-queue
      (let ((recovered nil))
        (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
        (setq agent-repl--merge-queue
              (list (list :source-ws "ws1" :target-dir "/tmp/ws1"
                          :silent t :auto-resolve t)))
        (cl-letf (((symbol-function 'agent-repl--cherry-pick-in-progress-p)
                   (lambda (_) nil))
                  ((symbol-function 'agent-repl--current-head-sha) (lambda (_) nil))
                  ((symbol-function 'agent-repl--persist-merge-queue) #'ignore)
                  ;; A drain refreshes the queued entries' commit lookahead,
                  ;; since the target just moved underneath them.
                  ((symbol-function 'agent-repl--git-string-quiet)
                   (lambda (&rest _args) "master"))
                  ((symbol-function 'agent-repl--workspace-merge-into-source)
                   (lambda (&rest _)
                     (signal 'agent-repl-merge-conflict-error '("rejected"))))
                  ((symbol-function 'agent-repl--reenqueue-and-redrive-on-failure)
                   (lambda (ws _err) (setq recovered ws))))
          (agent-repl--drain-merge-queue))
        (should (equal recovered "ws1"))))))

;;;; ---- Tests: reopen-workspace-from-state ----

(ert-deftest agent-repl-test-reopen-workspace-from-state-establishes-from-project-dir ()
  "Reopen wraps `agent-repl--establish-workspace' with the preserved
`:project-dir' so a workspace closed with preserve-entry can be brought
back without callers having to know the snapshot/establish protocol."
  (agent-repl-test--with-clean-state
    (let ((established nil))
      (agent-repl--ws-put "ws1" :project-dir "/tmp/saved-dir/")
      (cl-letf (((symbol-function 'agent-repl--establish-workspace)
                 (lambda (ws dir) (setq established (list ws dir)))))
        (agent-repl--reopen-workspace-from-state "ws1"))
      (should (equal established (list "ws1" "/tmp/saved-dir/"))))))

(ert-deftest agent-repl-test-reopen-workspace-from-state-noops-without-project-dir ()
  "When the workspace plist has no `:project-dir' (entry was finalized or
never preserved), reopen is a no-op — `--establish-workspace' is not
called, no error is signaled.  This is the safe path for callers that
might invoke reopen on a workspace whose state was already swept."
  (agent-repl-test--with-clean-state
    (let ((established nil))
      ;; Don't put :project-dir; entry is empty.
      (agent-repl--ws-put "ws1" :some-other-key 'something)
      (cl-letf (((symbol-function 'agent-repl--establish-workspace)
                 (lambda (&rest _) (setq established t))))
        (agent-repl--reopen-workspace-from-state "ws1"))
      (should-not established))))

(ert-deftest agent-repl-test-reopen-workspace-from-state-normalizes-branchy-name ()
  "Branch-style name like `DWC/foo' normalizes to the bare `foo' before
lookup — the registry is keyed by bare names so the lookup must agree."
  (agent-repl-test--with-clean-state
    (let ((established nil))
      (agent-repl--ws-put "foo" :project-dir "/tmp/foo-dir/")
      (cl-letf (((symbol-function 'agent-repl--establish-workspace)
                 (lambda (ws dir) (setq established (list ws dir)))))
        (agent-repl--reopen-workspace-from-state "DWC/foo"))
      (should (equal established (list "foo" "/tmp/foo-dir/"))))))

;;;; ---- Tests: finish-workspace ----

(ert-deftest agent-repl-test-finish-workspace-non-worktree ()
  "Finishing a non-worktree workspace tombstones state and kills persp.
Post-tombstone-refactor, finish-workspace no longer removes the hash
entry — it stamps `:nuked-at' via `--ws-del'.  This test pins both the
persp-kill and the resulting tombstone marker."
  (agent-repl-test--with-clean-state
    (let ((persp-killed nil))
      (agent-repl--ws-put "ws1" :project-dir "/tmp/fake")
      (cl-letf (((symbol-function 'agent-repl--kill-vterm-process) (lambda (_b) nil))
                ((symbol-function '+workspace-list-names) (lambda () '("ws1" "ws2")))
                ((symbol-function 'persp-kill) (lambda (ws) (setq persp-killed ws))))
        (agent-repl--finish-workspace "ws1")
        (should (equal persp-killed "ws1"))
        ;; Tombstoned: entry survives with `:nuked-at', not live.
        (should (agent-repl--ws-get "ws1" :nuked-at))
        (should-not (agent-repl--ws-live-p "ws1"))))))

(ert-deftest agent-repl-test-finish-workspace-with-worktree ()
  "Finishing a worktree workspace removes the git worktree."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "ws-test-" t))
          (removed nil))
      (unwind-protect
          (progn
            (agent-repl--ws-put "ws1" :worktree-p t)
            (agent-repl--ws-put "ws1" :project-dir (file-name-as-directory tmpdir))
            (cl-letf (((symbol-function 'agent-repl--kill-vterm-process) (lambda (_b) nil))
                      ((symbol-function '+workspace-list-names) (lambda () '("ws1")))
                      ((symbol-function 'persp-kill) (lambda (_ws) nil))
                      ((symbol-function 'agent-repl--remove-git-worktree)
                       (lambda (dir) (setq removed dir))))
              (agent-repl--finish-workspace "ws1")
              (should (equal removed (file-name-as-directory tmpdir)))))
        (when (file-directory-p tmpdir)
          (delete-directory tmpdir t))))))

(ert-deftest agent-repl-test-finish-workspace-normalizes-name ()
  "Branch-style name 'DWC/foo' is normalized to 'foo' before tombstoning.
The post-refactor invariant is that the hash entry is tombstoned (not
removed); we pin both the name normalization and the resulting
liveness flip."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "foo" :project-dir "/tmp/fake")
    (cl-letf (((symbol-function 'agent-repl--kill-vterm-process) (lambda (_b) nil))
              ((symbol-function '+workspace-list-names) (lambda () '("foo")))
              ((symbol-function 'persp-kill) (lambda (_ws) nil)))
      (agent-repl--finish-workspace "DWC/foo")
      (should-not (agent-repl--ws-live-p "foo"))
      (should (agent-repl--ws-get "foo" :nuked-at)))))

(ert-deftest agent-repl-test-finish-workspace-kills-vterm ()
  "Vterm buffer process is killed when present."
  (agent-repl-test--with-clean-state
    (let ((killed-buf nil))
      (agent-repl-test--with-temp-buffer "*agent-panel-test-vterm*"
        (agent-repl--ws-put "ws1" :vterm-buffer (current-buffer))
        (cl-letf (((symbol-function 'agent-repl--kill-vterm-process)
                   (lambda (b) (setq killed-buf b)))
                  ((symbol-function '+workspace-list-names) (lambda () nil))
                  ((symbol-function 'persp-kill) (lambda (_ws) nil)))
          (agent-repl--finish-workspace "ws1")
          (should (equal killed-buf (get-buffer "*agent-panel-test-vterm*"))))))))

(ert-deftest agent-repl-test-finish-workspace-no-persp-kill-if-not-listed ()
  "If workspace is not in +workspace-list-names, persp-kill is not called."
  (agent-repl-test--with-clean-state
    (let ((persp-killed nil))
      (agent-repl--ws-put "ws1" :project-dir "/tmp/fake")
      (cl-letf (((symbol-function 'agent-repl--kill-vterm-process) (lambda (_b) nil))
                ((symbol-function '+workspace-list-names) (lambda () '("other")))
                ((symbol-function 'persp-kill) (lambda (ws) (setq persp-killed ws))))
        (agent-repl--finish-workspace "ws1")
        (should-not persp-killed)))))

;;;; ---- Tests: resolve-worktree-paths ----

(ert-deftest agent-repl-test-resolve-worktree-paths-uses-passed-git-root ()
  "Uses the GIT-ROOT argument, not `default-directory' or any cached variable."
  (let ((tmpdir (agent-repl--path-canonical
                 (make-temp-file "resolve-wt-test-" t))))
    (unwind-protect
        (let* ((fake-root (expand-file-name "my-repo" tmpdir)))
          (make-directory fake-root t)
          (make-directory (expand-file-name ".git" fake-root) t)
          (let ((default-directory "/nonexistent/should-not-matter/"))
            (let ((result (agent-repl--resolve-worktree-paths
                           (file-name-as-directory fake-root)
                           "new-feature")))
              (should (equal (plist-get result :git-root)
                             (agent-repl--path-canonical fake-root))))))
      (delete-directory tmpdir t))))

(ert-deftest agent-repl-test-resolve-worktree-paths-inside-worktree ()
  "Inside a worktree (.git is a file), new worktree is a sibling directory."
  ;; Canonicalize tmpdir up front: the function under test canonicalizes
  ;; its git-root, so the test's expected paths must also be canonical or
  ;; they'll mismatch on platforms with firmlinks (macOS /var -> /private/var).
  (let ((tmpdir (agent-repl--path-canonical
                 (make-temp-file "resolve-wt-test-" t))))
    (unwind-protect
        (let* ((fake-root (expand-file-name "existing-wt" tmpdir)))
          (make-directory fake-root t)
          ;; Simulate worktree: .git is a regular file, not a directory
          (write-region "gitdir: /some/other/.git/worktrees/existing-wt"
                        nil (expand-file-name ".git" fake-root))
          (let ((result (agent-repl--resolve-worktree-paths
                         (file-name-as-directory fake-root)
                         "new-feature")))
            ;; :in-worktree should be t
            (should (eq (plist-get result :in-worktree) t))
            ;; :worktree-parent should be the parent of fake-root (i.e. tmpdir)
            (should (equal (plist-get result :worktree-parent)
                           (file-name-directory (directory-file-name fake-root))))
            ;; :path should be sibling
            (should (equal (plist-get result :path)
                           (expand-file-name "new-feature" tmpdir)))))
      (delete-directory tmpdir t))))

(ert-deftest agent-repl-test-resolve-worktree-paths-normal-repo ()
  "Normal repo (.git is a directory) creates a -worktrees sibling directory."
  (let ((tmpdir (agent-repl--path-canonical
                 (make-temp-file "resolve-wt-test-" t))))
    (unwind-protect
        (let* ((fake-root (expand-file-name "my-repo" tmpdir)))
          (make-directory fake-root t)
          ;; Simulate normal repo: .git is a directory
          (make-directory (expand-file-name ".git" fake-root) t)
          (let ((result (agent-repl--resolve-worktree-paths
                         (file-name-as-directory fake-root)
                         "new-feature")))
            ;; :in-worktree should be nil
            (should-not (plist-get result :in-worktree))
            ;; :worktree-parent should be <parent>/my-repo-worktrees/
            (let ((expected-parent (expand-file-name "my-repo-worktrees" tmpdir)))
              (should (equal (plist-get result :worktree-parent) expected-parent))
              ;; The -worktrees directory should have been created
              (should (file-directory-p expected-parent)))
            ;; :path should be inside the -worktrees directory
            (should (equal (plist-get result :path)
                           (expand-file-name "new-feature"
                                             (expand-file-name "my-repo-worktrees" tmpdir))))))
      (delete-directory tmpdir t))))

(ert-deftest agent-repl-test-resolve-worktree-paths-nested-name-extracts-dirname ()
  "Nested branch name like DWC/CV-100/cool-branch extracts only 'cool-branch' as dirname."
  (let ((tmpdir (agent-repl--path-canonical
                 (make-temp-file "resolve-wt-test-" t))))
    (unwind-protect
        (let* ((fake-root (expand-file-name "my-repo" tmpdir)))
          (make-directory fake-root t)
          (make-directory (expand-file-name ".git" fake-root) t)
          (let ((result (agent-repl--resolve-worktree-paths
                         (file-name-as-directory fake-root)
                         "DWC/CV-100/cool-branch")))
            (should (equal (plist-get result :dirname) "cool-branch"))
            (should (equal (plist-get result :branch-name) "DWC/CV-100/cool-branch"))
            (should (equal (plist-get result :git-root)
                           (agent-repl--path-canonical fake-root)))))
      (delete-directory tmpdir t))))

;;;; ---- Tests: workspace-branch ----

(ert-deftest agent-repl-test-workspace-branch-no-project-dir ()
  "When workspace has no :project-dir, returns nil."
  (agent-repl-test--with-clean-state
    ;; ws1 has no :project-dir set
    (should (null (agent-repl--workspace-branch "ws1")))))

(ert-deftest agent-repl-test-workspace-branch-git-fails ()
  "When git rev-parse returns a fatal message, returns nil."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/tmp/fake-dir/")
    (cl-letf (((symbol-function 'agent-repl--git-string)
               (lambda (&rest _args) "fatal: not a git repository")))
      (should (null (agent-repl--workspace-branch "ws1"))))))

(ert-deftest agent-repl-test-workspace-branch-detached-head ()
  "When branch is 'HEAD' (detached), returns the SHA from rev-parse HEAD."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/tmp/fake-dir/")
    (let ((call-count 0))
      (cl-letf (((symbol-function 'agent-repl--git-string)
                 (lambda (&rest args)
                   (cl-incf call-count)
                   (if (member "--abbrev-ref" args)
                       "HEAD"
                     "abc123def456"))))
        (should (equal (agent-repl--workspace-branch "ws1") "abc123def456"))))))

(ert-deftest agent-repl-test-workspace-branch-normal-branch ()
  "When git returns a normal branch name, returns it directly."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/tmp/fake-dir/")
    (cl-letf (((symbol-function 'agent-repl--git-string)
               (lambda (&rest _args) "DWC/my-feature")))
      (should (equal (agent-repl--workspace-branch "ws1") "DWC/my-feature")))))

(ert-deftest agent-repl-test-workspace-branch-empty-string ()
  "When git returns an empty string, returns nil."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/tmp/fake-dir/")
    (cl-letf (((symbol-function 'agent-repl--git-string)
               (lambda (&rest _args) "")))
      (should (null (agent-repl--workspace-branch "ws1"))))))

;;;; ---- Tests: fork-worktree-workspace ----

(ert-deftest agent-repl-test-fork-worktree-workspace-no-session-id ()
  "When current workspace has no session ID, fork signals user-error."
  (agent-repl-test--with-clean-state
    (let ((inst (make-agent-repl-instantiation :session-id nil)))
      (cl-letf (((symbol-function 'agent-repl--active-inst)
                 (lambda (_ws) inst))
                ((symbol-function '+workspace-current-name)
                 (lambda () "test-ws"))
                ((symbol-function 'agent-repl--resolve-current-git-root)
                 (lambda () "/tmp/cur-repo/")))
        (should-error (agent-repl-fork-worktree-workspace nil)
                      :type 'user-error)))))

(ert-deftest agent-repl-test-fork-worktree-workspace-with-session-id-passes-fork-from ()
  "When session ID exists, fork dispatches with FORK-FROM = current workspace.
The new flow no longer threads the session ID through the interactive
entry; it threads the workspace NAME (`fork_from`) into the
workspace-generation prompt, and the file-watcher resolves the session
ID later.  This test covers the entry's only remaining job: surfacing
the right fork-from name."
  (agent-repl-test--with-clean-state
    (let ((inst (make-agent-repl-instantiation :session-id "sess-abc-123"))
          (captured-fork-from :unset))
      (cl-letf (((symbol-function 'agent-repl--active-inst)
                 (lambda (_ws) inst))
                ((symbol-function '+workspace-current-name)
                 (lambda () "test-ws"))
                ((symbol-function 'agent-repl--resolve-current-git-root)
                 (lambda () "/tmp/cur-repo/"))
                ((symbol-function 'read-string)
                 (lambda (&rest _) "do the thing"))
                ((symbol-function 'agent-repl--spawn-workspace-generation)
                 (lambda (_raw _prefixed _git-root _base fork-from)
                   (setq captured-fork-from fork-from))))
        (agent-repl-fork-worktree-workspace nil)
        (should (equal captured-fork-from "test-ws"))))))

(ert-deftest agent-repl-test-fork-worktree-workspace-source-ws-forks-its-name ()
  "When SOURCE-WS is given, fork-from is that workspace's name."
  (agent-repl-test--with-clean-state
    (let ((source-inst (make-agent-repl-instantiation :session-id "sess-source"))
          (current-inst (make-agent-repl-instantiation :session-id "sess-current"))
          (captured-fork-from :unset))
      (cl-letf (((symbol-function 'agent-repl--active-inst)
                 (lambda (ws)
                   (cond ((equal ws "source-ws") source-inst)
                         ((equal ws "test-ws") current-inst)
                         (t (error "unexpected ws: %s" ws)))))
                ((symbol-function '+workspace-current-name)
                 (lambda () "test-ws"))
                ((symbol-function 'agent-repl--ws-dir)
                 (lambda (_ws) "/tmp/source-repo/"))
                ((symbol-function 'read-string)
                 (lambda (&rest _) "do the thing"))
                ((symbol-function 'agent-repl--spawn-workspace-generation)
                 (lambda (_raw _prefixed _git-root _base fork-from)
                   (setq captured-fork-from fork-from))))
        (agent-repl-fork-worktree-workspace "source-ws")
        (should (equal captured-fork-from "source-ws"))))))

(ert-deftest agent-repl-test-fork-worktree-workspace-source-ws-passes-git-root ()
  "When SOURCE-WS is given, its project-dir is threaded through as git-root."
  (agent-repl-test--with-clean-state
    (let ((source-inst (make-agent-repl-instantiation :session-id "sess-source"))
          (captured-git-root :unset))
      (cl-letf (((symbol-function 'agent-repl--active-inst)
                 (lambda (_ws) source-inst))
                ((symbol-function '+workspace-current-name)
                 (lambda () "test-ws"))
                ((symbol-function 'agent-repl--ws-dir)
                 (lambda (ws)
                   (if (equal ws "source-ws") "/tmp/source-repo/"
                     (error "unexpected ws: %s" ws))))
                ((symbol-function 'read-string)
                 (lambda (&rest _) "do the thing"))
                ((symbol-function 'agent-repl--spawn-workspace-generation)
                 (lambda (_raw _prefixed git-root _base _fork-from)
                   (setq captured-git-root git-root))))
        (agent-repl-fork-worktree-workspace "source-ws")
        (should (equal captured-git-root "/tmp/source-repo/"))))))

(ert-deftest agent-repl-test-fork-worktree-workspace-no-source-ws-resolves-ambient-git-root ()
  "With no SOURCE-WS and no ws-dir, git-root falls back to `resolve-current-git-root'.
Unlike the old flow (which passed nil and let `do-create' resolve later), the
new flow needs an explicit git-root to inject into the workspace-generation
JSON, so it eagerly resolves at entry-point time."
  (agent-repl-test--with-clean-state
    (let ((current-inst (make-agent-repl-instantiation :session-id "sess-current"))
          (captured-git-root :unset))
      (cl-letf (((symbol-function 'agent-repl--active-inst)
                 (lambda (ws)
                   (if (equal ws "test-ws") current-inst
                     (error "unexpected ws: %s" ws))))
                ((symbol-function '+workspace-current-name)
                 (lambda () "test-ws"))
                ((symbol-function 'agent-repl--ws-dir)
                 (lambda (_ws) nil))
                ((symbol-function 'agent-repl--resolve-current-git-root)
                 (lambda () "/tmp/ambient-repo/"))
                ((symbol-function 'read-string)
                 (lambda (&rest _) "do the thing"))
                ((symbol-function 'agent-repl--spawn-workspace-generation)
                 (lambda (_raw _prefixed git-root _base _fork-from)
                   (setq captured-git-root git-root))))
        (agent-repl-fork-worktree-workspace nil)
        (should (equal captured-git-root "/tmp/ambient-repo/"))))))

(ert-deftest agent-repl-test-fork-worktree-workspace-passes-head-as-base ()
  "Fork always passes BASE-COMMIT = \"HEAD\" to the spawn helper."
  (agent-repl-test--with-clean-state
    (let ((inst (make-agent-repl-instantiation :session-id "sess-abc-123"))
          (captured-base :unset))
      (cl-letf (((symbol-function 'agent-repl--active-inst)
                 (lambda (_ws) inst))
                ((symbol-function '+workspace-current-name)
                 (lambda () "test-ws"))
                ((symbol-function 'agent-repl--resolve-current-git-root)
                 (lambda () "/tmp/cur-repo/"))
                ((symbol-function 'read-string)
                 (lambda (&rest _) "do the thing"))
                ((symbol-function 'agent-repl--spawn-workspace-generation)
                 (lambda (_raw _prefixed _git-root base _fork-from)
                   (setq captured-base base))))
        (agent-repl-fork-worktree-workspace nil)
        (should (equal captured-base "HEAD"))))))

;;;; ---- Tests: git-root threading from interactive entry points ----

;; The new flow eagerly resolves a single git-root at entry-point time and
;; injects it into the workspace-generation JSON.  The downstream
;; `--create-worktree-from-command' uses that same git-root as both
;; git-root and source-dir on the new workspace, so source-dir threading
;; collapses into git-root threading at the entry-point layer.

(ert-deftest agent-repl-test-create-worktree-workspace-uses-current-ws-dir ()
  "Without explicit SOURCE-WS, git-root falls back to the current ws's :project-dir."
  (agent-repl-test--with-clean-state
    (let ((captured-git-root :unset))
      (cl-letf (((symbol-function '+workspace-current-name)
                 (lambda () "ambient-ws"))
                ((symbol-function 'agent-repl--ws-dir)
                 (lambda (ws)
                   (if (equal ws "ambient-ws") "/tmp/ambient-repo/"
                     (error "unexpected ws: %s" ws))))
                ((symbol-function 'read-string)
                 (lambda (&rest _) "do the thing"))
                ((symbol-function 'agent-repl--spawn-workspace-generation)
                 (lambda (_raw _prefixed git-root _base _fork-from)
                   (setq captured-git-root git-root))))
        (agent-repl-create-worktree-workspace 'head)
        (should (equal captured-git-root "/tmp/ambient-repo/"))))))

(ert-deftest agent-repl-test-create-worktree-workspace-uses-explicit-source-ws-dir ()
  "With explicit SOURCE-WS, git-root is that workspace's :project-dir."
  (agent-repl-test--with-clean-state
    (let ((captured-git-root :unset))
      (cl-letf (((symbol-function 'agent-repl--ws-dir)
                 (lambda (ws)
                   (if (equal ws "explicit-ws") "/tmp/explicit-repo/"
                     (error "unexpected ws: %s" ws))))
                ((symbol-function 'read-string)
                 (lambda (&rest _) "do the thing"))
                ((symbol-function 'agent-repl--spawn-workspace-generation)
                 (lambda (_raw _prefixed git-root _base _fork-from)
                   (setq captured-git-root git-root))))
        (agent-repl-create-worktree-workspace 'head "explicit-ws")
        (should (equal captured-git-root "/tmp/explicit-repo/"))))))

(ert-deftest agent-repl-test-fork-worktree-workspace-uses-fork-ws-dir ()
  "Fork's git-root is the fork-ws's :project-dir."
  (agent-repl-test--with-clean-state
    (let ((source-inst (make-agent-repl-instantiation :session-id "sess-source"))
          (captured-git-root :unset))
      (cl-letf (((symbol-function 'agent-repl--active-inst)
                 (lambda (_ws) source-inst))
                ((symbol-function 'agent-repl--ws-dir)
                 (lambda (ws)
                   (if (equal ws "fork-source") "/tmp/fork-source-repo/"
                     (error "unexpected ws: %s" ws))))
                ((symbol-function 'read-string)
                 (lambda (&rest _) "do the thing"))
                ((symbol-function 'agent-repl--spawn-workspace-generation)
                 (lambda (_raw _prefixed git-root _base _fork-from)
                   (setq captured-git-root git-root))))
        (agent-repl-fork-worktree-workspace "fork-source")
        (should (equal captured-git-root "/tmp/fork-source-repo/"))))))

(ert-deftest agent-repl-test-create-worktree-from-command-records-git-root-as-source-dir ()
  "The commands flow records GIT-ROOT as source-dir on the new ws."
  (agent-repl-test--with-clean-state
    (let ((captured-source-dir :unset))
      (cl-letf (((symbol-function 'agent-repl--do-create-worktree-workspace)
                 (lambda (_name _bare _fork _prompt _cb _priority _base &optional _git-root source-dir _no-agent _model)
                   (setq captured-source-dir source-dir))))
        (agent-repl--create-worktree-from-command "/tmp/cmd-repo/" "name" "prompt" 5)
        (should (equal captured-source-dir "/tmp/cmd-repo/"))))))

(ert-deftest agent-repl-test-create-worktree-from-command-forwards-force-sandbox ()
  "FORCE-SANDBOX from the JSON command flows through to
`agent-repl--do-create-worktree-workspace' as the second positional arg."
  (agent-repl-test--with-clean-state
    (let ((captured-force-sandbox :unset))
      (cl-letf (((symbol-function 'agent-repl--do-create-worktree-workspace)
                 (lambda (_name force-sandbox &rest _)
                   (setq captured-force-sandbox force-sandbox))))
        (agent-repl--create-worktree-from-command "/tmp/repo/" "name" "prompt" 5 nil nil t)
        (should captured-force-sandbox)))))

(ert-deftest agent-repl-test-create-worktree-from-command-forwards-model ()
  "MODEL flows through to `agent-repl--do-create-worktree-workspace' as the
11th positional arg."
  (agent-repl-test--with-clean-state
    (let ((captured-model :unset))
      (cl-letf (((symbol-function 'agent-repl--do-create-worktree-workspace)
                 (lambda (_name _fs _fork _prompt _cb _priority _base _git _src _no-agent &optional model)
                   (setq captured-model model))))
        (agent-repl--create-worktree-from-command
         "/tmp/repo/" "name" "prompt" 5 nil nil nil "opus")
        (should (equal captured-model "opus"))))))

(ert-deftest agent-repl-test-create-worktree-from-command-passes-nil-model-when-absent ()
  "When MODEL is not supplied, `agent-repl--do-create-worktree-workspace'
receives nil so the session uses the interactive-model default."
  (agent-repl-test--with-clean-state
    (let ((captured-model :unset))
      (cl-letf (((symbol-function 'agent-repl--do-create-worktree-workspace)
                 (lambda (_name _fs _fork _prompt _cb _priority _base _git _src _no-agent &optional model)
                   (setq captured-model model))))
        (agent-repl--create-worktree-from-command "/tmp/repo/" "name" "prompt" 5)
        (should (null captured-model))))))

(ert-deftest agent-repl-test-create-worktree-from-command-passes-nil-force-sandbox-when-absent ()
  "When FORCE-SANDBOX is not supplied, `agent-repl--do-create-worktree-workspace'
receives nil so the workspace uses bare-metal by default."
  (agent-repl-test--with-clean-state
    (let ((captured-force-sandbox :unset))
      (cl-letf (((symbol-function 'agent-repl--do-create-worktree-workspace)
                 (lambda (_name force-sandbox &rest _)
                   (setq captured-force-sandbox force-sandbox))))
        (agent-repl--create-worktree-from-command "/tmp/repo/" "name" "prompt" 5)
        (should (null captured-force-sandbox))))))

(ert-deftest agent-repl-test-finalize-worktree-workspace-stores-source-ws-dir ()
  "Finalize persists :source-ws-dir on the new workspace's plist."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--register-projectile-project)
               (lambda (&rest _) nil))
              ((symbol-function '+workspace-new) (lambda (_ws) nil))
              ((symbol-function 'agent-repl--setup-worktree-session)
               (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--path-canonical) #'identity)
              ((symbol-function 'agent-repl--git-string-quiet) (lambda (&rest _) "")))
      (agent-repl--finalize-worktree-workspace
       "/tmp/new-wt" "new-ws" nil nil nil nil nil "/tmp/source-repo/")
      (should (equal (agent-repl--ws-get "new-ws" :source-ws-dir)
                     "/tmp/source-repo/")))))

(ert-deftest agent-repl-test-finalize-worktree-workspace-omits-source-ws-dir-when-nil ()
  "When source-dir is nil, :source-ws-dir is not stored (apply-workspace-properties skips nil)."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--register-projectile-project)
               (lambda (&rest _) nil))
              ((symbol-function '+workspace-new) (lambda (_ws) nil))
              ((symbol-function 'agent-repl--setup-worktree-session)
               (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--path-canonical) #'identity)
              ((symbol-function 'agent-repl--git-string-quiet) (lambda (&rest _) "")))
      (agent-repl--finalize-worktree-workspace
       "/tmp/new-wt" "new-ws" nil nil nil nil nil nil)
      (should (null (agent-repl--ws-get "new-ws" :source-ws-dir))))))

(ert-deftest agent-repl-test-finalize-worktree-workspace-stores-model ()
  "Finalize persists :model on the new workspace's plist."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--register-projectile-project)
               (lambda (&rest _) nil))
              ((symbol-function '+workspace-new) (lambda (_ws) nil))
              ((symbol-function 'agent-repl--setup-worktree-session)
               (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--path-canonical) #'identity)
              ((symbol-function 'agent-repl--git-string-quiet) (lambda (&rest _) "")))
      (agent-repl--finalize-worktree-workspace
       "/tmp/new-wt" "new-ws" nil nil nil nil nil nil nil "sonnet")
      (should (equal (agent-repl--ws-get "new-ws" :model) "sonnet")))))

(ert-deftest agent-repl-test-finalize-worktree-workspace-omits-model-when-nil ()
  "When model is nil, :model is not stored (apply-workspace-properties skips nil)."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--register-projectile-project)
               (lambda (&rest _) nil))
              ((symbol-function '+workspace-new) (lambda (_ws) nil))
              ((symbol-function 'agent-repl--setup-worktree-session)
               (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--path-canonical) #'identity)
              ((symbol-function 'agent-repl--git-string-quiet) (lambda (&rest _) "")))
      (agent-repl--finalize-worktree-workspace
       "/tmp/new-wt" "new-ws" nil nil nil nil nil nil nil nil)
      (should (null (agent-repl--ws-get "new-ws" :model))))))

(ert-deftest agent-repl-test-finalize-worktree-workspace-forwards-no-agent ()
  "Finalize forwards NO-AGENT to `agent-repl--setup-worktree-session'."
  (agent-repl-test--with-clean-state
    (let ((captured :unset))
      (cl-letf (((symbol-function 'agent-repl--register-projectile-project)
                 (lambda (&rest _) nil))
                ((symbol-function '+workspace-new) (lambda (_ws) nil))
                ((symbol-function 'agent-repl--setup-worktree-session)
                 (lambda (_ws-id _path _ws _force-sandbox &optional no-agent)
                   (setq captured no-agent)))
                ((symbol-function 'agent-repl--path-canonical) #'identity)
                ((symbol-function 'agent-repl--git-string-quiet) (lambda (&rest _) "")))
        (agent-repl--finalize-worktree-workspace
         "/tmp/new-wt" "new-ws" nil nil nil nil nil "/tmp/source-repo/" t)
        (should (eq captured t))))))

;;;; ---- Tests: setup-worktree-session no-agent branch ----

(defmacro agent-repl-test--with-worktree-boot-stubs (bindings &rest body)
  "Run BODY with the worktree boot's collaborators stubbed, plus BINDINGS.

Stubs the env hydration faithfully — the real
`agent-repl--initialize-ws-env' is the sole writer of `:active-env',
and `agent-repl--frontend-boot-session' RESOLVES THE FRONTEND against
that value, so a stub that dropped it would hand every test a
bare-metal workspace and quietly defeat the sandbox cases.

BINDINGS are extra `cl-letf' bindings and are spliced in FIRST, ahead
of the defaults: when one `cl-letf' binds the same place twice, the
EARLIER binding is the one in force for the body, so a caller's
override must precede the default it replaces."
  (declare (indent 1))
  `(agent-repl-test--with-clean-state
     (cl-letf (,@bindings
               ((symbol-function 'agent-repl--register-worktree-ws)
                (lambda (&rest _) nil))
               ((symbol-function 'agent-repl--initialize-ws-env)
                (lambda (ws &optional _dir env)
                  (agent-repl--ws-put ws :active-env (or env :bare-metal))))
               ((symbol-function 'agent-repl--initialize-agent)
                (lambda (&rest _) nil))
               ((symbol-function 'agent-repl--gui-boot)
                (lambda (&rest _) nil))
               ((symbol-function 'agent-repl--active-inst)
                (lambda (_ws) (make-agent-repl-instantiation :start-cmd "claude"))))
       ,@body)))

(ert-deftest agent-repl-test-setup-worktree-session-boots-through-the-default-frontend ()
  "Without NO-AGENT, a new worktree boots under `agent-repl-default-frontend' (gui)."
  ;; Arrange
  (let ((gui-booted nil)
        (vterm-booted nil))
    (agent-repl-test--with-worktree-boot-stubs
        (((symbol-function 'agent-repl--gui-boot)
          (lambda (ws &rest _) (setq gui-booted ws)))
         ((symbol-function 'agent-repl--initialize-agent)
          (lambda (&rest _) (setq vterm-booted t))))
      ;; Act
      (agent-repl--setup-worktree-session "id" "/tmp/wt/" "ws" nil)
      ;; Assert — the generated workspace is born in the gui, not the vterm.
      (should (equal gui-booted "ws"))
      (should-not vterm-booted))))

(ert-deftest agent-repl-test-setup-worktree-session-boots-vterm-for-a-vterm-workspace ()
  "A workspace that chose vterm boots the vterm, not the default gui."
  ;; Arrange
  (let ((vterm-booted nil)
        (gui-booted nil))
    (agent-repl-test--with-worktree-boot-stubs
        (((symbol-function 'agent-repl--initialize-agent)
          (lambda (ws &rest _) (setq vterm-booted ws)))
         ((symbol-function 'agent-repl--gui-boot)
          (lambda (&rest _) (setq gui-booted t))))
      (agent-repl--ws-choose-frontend "ws" 'vterm)
      ;; Act
      (agent-repl--setup-worktree-session "id" "/tmp/wt/" "ws" nil)
      ;; Assert
      (should (equal vterm-booted "ws"))
      (should-not gui-booted))))

(ert-deftest agent-repl-test-setup-worktree-session-force-sandbox-boots-vterm ()
  "A force-sandbox worktree boots the vterm even though the gui is the default.
The gui daemon spawns the agent on the host, so presenting a sandboxed
workspace through it would run it outside the container it asked for."
  ;; Arrange
  (let ((vterm-booted nil)
        (gui-booted nil))
    (agent-repl-test--with-worktree-boot-stubs
        (((symbol-function 'agent-repl--initialize-agent)
          (lambda (ws &rest _) (setq vterm-booted ws)))
         ((symbol-function 'agent-repl--gui-boot)
          (lambda (&rest _) (setq gui-booted t))))
      ;; Act — force-sandbox = t.
      (agent-repl--setup-worktree-session "id" "/tmp/wt/" "ws" t)
      ;; Assert
      (should (equal vterm-booted "ws"))
      (should-not gui-booted))))

(ert-deftest agent-repl-test-setup-worktree-session-init-error-does-not-escape ()
  "A boot failure (e.g. sandbox image not built) is caught,
so it cannot escape and crash the `--async-git-sentinel' that calls this."
  (agent-repl-test--with-worktree-boot-stubs
      (((symbol-function 'agent-repl--gui-boot)
        (lambda (&rest _) (user-error "Sandbox image not built")))
       ((symbol-function 'agent-repl--ws-set-agent-state) (lambda (&rest _) nil))
       ((symbol-function 'message) (lambda (&rest _) nil)))
    ;; Returns normally rather than signaling.
    (should (progn (agent-repl--setup-worktree-session "id" "/tmp/wt/" "ws" nil) t))))

(ert-deftest agent-repl-test-setup-worktree-session-init-error-marks-start-failed ()
  "A caught boot failure sets :agent-state :start-failed so
the tab/drawer surface the failure instead of it vanishing silently."
  (let ((recorded nil))
    (agent-repl-test--with-worktree-boot-stubs
        (((symbol-function 'agent-repl--gui-boot)
          (lambda (&rest _) (user-error "Sandbox image not built")))
         ((symbol-function 'agent-repl--ws-set-agent-state)
          (lambda (ws state) (setq recorded (cons ws state))))
         ((symbol-function 'message) (lambda (&rest _) nil)))
      (agent-repl--setup-worktree-session "id" "/tmp/wt/" "ws" nil)
      (should (equal recorded '("ws" . :start-failed))))))

(ert-deftest agent-repl-test-mark-start-failed-sets-start-failed-state ()
  "mark-start-failed records :start-failed for the workspace."
  (let ((recorded nil))
    (cl-letf (((symbol-function 'agent-repl--ws-set-agent-state)
               (lambda (ws state) (setq recorded (cons ws state))))
              ((symbol-function 'message) (lambda (&rest _) nil)))
      (agent-repl--mark-start-failed "ws" '(user-error "boom"))
      (should (equal recorded '("ws" . :start-failed))))))

(ert-deftest agent-repl-test-setup-worktree-session-no-agent-skips-boot ()
  "With NO-AGENT, setup hydrates env via `initialize-ws-env' and never boots the agent."
  (let ((init-agent-called nil)
        (init-env-called nil))
    (cl-letf (((symbol-function 'agent-repl--register-worktree-ws)
               (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--initialize-agent)
               (lambda (&rest _) (setq init-agent-called t)))
              ((symbol-function 'agent-repl--initialize-ws-env)
               (lambda (&rest _) (setq init-env-called t))))
      (agent-repl--setup-worktree-session "id" "/tmp/wt/" "ws" nil t)
      (should-not init-agent-called)
      (should init-env-called))))

(ert-deftest agent-repl-test-setup-worktree-session-no-agent-still-registers-worktree ()
  "With NO-AGENT, the workspace is still registered as a worktree workspace."
  (let ((registered nil))
    (cl-letf (((symbol-function 'agent-repl--register-worktree-ws)
               (lambda (_ws-id &optional _ws) (setq registered t)))
              ((symbol-function 'agent-repl--initialize-agent)
               (lambda (&rest _) (error "should not boot Claude")))
              ((symbol-function 'agent-repl--initialize-ws-env)
               (lambda (&rest _) nil)))
      (agent-repl--setup-worktree-session "id" "/tmp/wt/" "ws" nil t)
      (should registered))))

(ert-deftest agent-repl-test-finalize-worktree-workspace-calls-reorder-by-priority ()
  "Finalize invokes reorder-workspace-by-priority after applying properties.
Reorder must run after `apply-workspace-properties' so the new workspace's
`:priority' is already on the plist when the cache is rewritten."
  (agent-repl-test--with-clean-state
    (let ((reorder-called-with :unset))
      (cl-letf (((symbol-function 'agent-repl--register-projectile-project)
                 (lambda (&rest _) nil))
                ((symbol-function '+workspace-new) (lambda (_ws) nil))
                ((symbol-function 'agent-repl--setup-worktree-session)
                 (lambda (&rest _) nil))
                ((symbol-function 'agent-repl--path-canonical) #'identity)
                ((symbol-function 'agent-repl--git-string-quiet) (lambda (&rest _) ""))
                ((symbol-function 'agent-repl--reorder-workspace-by-priority)
                 (lambda (ws)
                   (setq reorder-called-with
                         (cons ws (agent-repl--ws-get ws :priority))))))
        (agent-repl--finalize-worktree-workspace
         "/tmp/new-wt" "new-ws" nil "p1" nil nil nil nil)
        (should (equal reorder-called-with '("new-ws" . "p1")))))))

(ert-deftest agent-repl-test-finalize-tags-workspace-project ()
  "Finalize creates the persp via --ws-create, tagging +workspace-project
with the canonical project dir.  Without this tag, a later `SPC p p' into
the worktree falls into Doom's uniquify-by-parent-dir branch and recreates
the workspace under a parent-dir-prefixed name (the bug under test)."
  (agent-repl-test--with-clean-state
    (let (param-call)
      (cl-letf (((symbol-function 'agent-repl--register-projectile-project)
                 (lambda (&rest _) nil))
                ((symbol-function 'agent-repl--repo-default-priority-for-path)
                 (lambda (&rest _) nil))
                ((symbol-function 'agent-repl--path-canonical)
                 (lambda (_p) "/canonical/new-wt"))
                ((symbol-function 'persp-add-new) (lambda (_ws) 'a-persp))
                ((symbol-function 'set-persp-parameter)
                 (lambda (key val persp) (setq param-call (list key val persp))))
                ((symbol-function 'agent-repl--setup-worktree-session)
                 (lambda (&rest _) nil))
                ((symbol-function 'agent-repl--git-string-quiet) (lambda (&rest _) "")))
        (agent-repl--finalize-worktree-workspace
         "/tmp/new-wt" "new-ws" nil nil nil nil nil nil)
        (should (equal param-call
                       '(+workspace-project "/canonical/new-wt" a-persp)))))))

;;;; ---- Tests: inherit-priority-from-source ----

(ert-deftest agent-repl-test-inherit-priority-explicit-wins ()
  "When PRIORITY is non-nil, it is returned unchanged regardless of SOURCE-DIR."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "parent" :project-dir "/tmp/parent/")
    (agent-repl--ws-put "parent" :priority "p1")
    (cl-letf (((symbol-function 'agent-repl--path-canonical) #'identity))
      (should (equal (agent-repl--inherit-priority-from-source "p2" "/tmp/parent/")
                     "p2")))))

(ert-deftest agent-repl-test-inherit-priority-nil-source-dir ()
  "When SOURCE-DIR is nil, returns nil even with no priority set."
  (should-not (agent-repl--inherit-priority-from-source nil nil)))

(ert-deftest agent-repl-test-inherit-priority-unknown-source-dir ()
  "When SOURCE-DIR does not resolve to any workspace, returns nil."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--path-canonical) #'identity))
      (should-not (agent-repl--inherit-priority-from-source nil "/tmp/nowhere/")))))

(ert-deftest agent-repl-test-inherit-priority-source-without-priority ()
  "Source workspace exists but has no :priority — returns nil."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "parent" :project-dir "/tmp/parent/")
    (cl-letf (((symbol-function 'agent-repl--path-canonical) #'identity))
      (should-not (agent-repl--inherit-priority-from-source nil "/tmp/parent/")))))

(ert-deftest agent-repl-test-inherit-priority-source-has-priority ()
  "Source workspace has :priority — returns it when PRIORITY is nil."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "parent" :project-dir "/tmp/parent/")
    (agent-repl--ws-put "parent" :priority "p05")
    (cl-letf (((symbol-function 'agent-repl--path-canonical) #'identity))
      (should (equal (agent-repl--inherit-priority-from-source nil "/tmp/parent/")
                     "p05")))))

;;;; ---- Tests: finalize-worktree-workspace child inherits parent priority ----

(ert-deftest agent-repl-test-finalize-child-inherits-parent-priority ()
  "When PRIORITY is nil and SOURCE-DIR points at a workspace, child inherits its priority."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "parent" :project-dir "/tmp/parent/")
    (agent-repl--ws-put "parent" :priority "p1")
    (cl-letf (((symbol-function 'agent-repl--register-projectile-project)
               (lambda (&rest _) nil))
              ((symbol-function '+workspace-new) (lambda (_ws) nil))
              ((symbol-function 'agent-repl--setup-worktree-session)
               (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--reorder-workspace-by-priority)
               (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--path-canonical) #'identity)
              ((symbol-function 'agent-repl--git-string-quiet) (lambda (&rest _) "")))
      (agent-repl--finalize-worktree-workspace
       "/tmp/new-wt" "child-ws" nil nil nil nil nil "/tmp/parent/")
      (should (equal (agent-repl--ws-get "child-ws" :priority) "p1")))))

(ert-deftest agent-repl-test-finalize-child-explicit-priority-wins ()
  "When PRIORITY is provided, it overrides any source workspace priority."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "parent" :project-dir "/tmp/parent/")
    (agent-repl--ws-put "parent" :priority "p1")
    (cl-letf (((symbol-function 'agent-repl--register-projectile-project)
               (lambda (&rest _) nil))
              ((symbol-function '+workspace-new) (lambda (_ws) nil))
              ((symbol-function 'agent-repl--setup-worktree-session)
               (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--reorder-workspace-by-priority)
               (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--path-canonical) #'identity)
              ((symbol-function 'agent-repl--git-string-quiet) (lambda (&rest _) "")))
      (agent-repl--finalize-worktree-workspace
       "/tmp/new-wt" "child-ws" nil "p3" nil nil nil "/tmp/parent/")
      (should (equal (agent-repl--ws-get "child-ws" :priority) "p3")))))

(ert-deftest agent-repl-test-finalize-no-parent-priority-stays-nil ()
  "When PRIORITY is nil and source workspace has no priority, child has no priority."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "parent" :project-dir "/tmp/parent/")
    (cl-letf (((symbol-function 'agent-repl--register-projectile-project)
               (lambda (&rest _) nil))
              ((symbol-function '+workspace-new) (lambda (_ws) nil))
              ((symbol-function 'agent-repl--setup-worktree-session)
               (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--reorder-workspace-by-priority)
               (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--repo-default-priority-for-path)
               (lambda (_path) nil))
              ((symbol-function 'agent-repl--path-canonical) #'identity)
              ((symbol-function 'agent-repl--git-string-quiet) (lambda (&rest _) "")))
      (agent-repl--finalize-worktree-workspace
       "/tmp/new-wt" "child-ws" nil nil nil nil nil "/tmp/parent/")
      (should-not (agent-repl--ws-get "child-ws" :priority)))))

;;;; ---- Tests: finalize-worktree-workspace falls back to repo-default priority ----

(ert-deftest agent-repl-test-finalize-falls-back-to-repo-default ()
  "When PRIORITY is nil and source-workspace has no priority, falls back to repo-default."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "parent" :project-dir "/tmp/parent/")
    (cl-letf (((symbol-function 'agent-repl--register-projectile-project)
               (lambda (&rest _) nil))
              ((symbol-function '+workspace-new) (lambda (_ws) nil))
              ((symbol-function 'agent-repl--setup-worktree-session)
               (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--reorder-workspace-by-priority)
               (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--repo-default-priority-for-path)
               (lambda (path)
                 (when (equal path "/tmp/new-wt") "p3")))
              ((symbol-function 'agent-repl--path-canonical) #'identity)
              ((symbol-function 'agent-repl--git-string-quiet) (lambda (&rest _) "")))
      (agent-repl--finalize-worktree-workspace
       "/tmp/new-wt" "child-ws" nil nil nil nil nil "/tmp/parent/")
      (should (equal (agent-repl--ws-get "child-ws" :priority) "p3")))))

(ert-deftest agent-repl-test-finalize-explicit-priority-wins-over-repo-default ()
  "Explicit PRIORITY wins over the repo-default fallback."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--register-projectile-project)
               (lambda (&rest _) nil))
              ((symbol-function '+workspace-new) (lambda (_ws) nil))
              ((symbol-function 'agent-repl--setup-worktree-session)
               (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--reorder-workspace-by-priority)
               (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--repo-default-priority-for-path)
               (lambda (_path) "p3"))
              ((symbol-function 'agent-repl--path-canonical) #'identity)
              ((symbol-function 'agent-repl--git-string-quiet) (lambda (&rest _) "")))
      (agent-repl--finalize-worktree-workspace
       "/tmp/new-wt" "child-ws" nil "p1" nil nil nil nil)
      (should (equal (agent-repl--ws-get "child-ws" :priority) "p1")))))

(ert-deftest agent-repl-test-finalize-parent-priority-wins-over-repo-default ()
  "Source-workspace priority wins over the repo-default fallback."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "parent" :project-dir "/tmp/parent/")
    (agent-repl--ws-put "parent" :priority "p2")
    (cl-letf (((symbol-function 'agent-repl--register-projectile-project)
               (lambda (&rest _) nil))
              ((symbol-function '+workspace-new) (lambda (_ws) nil))
              ((symbol-function 'agent-repl--setup-worktree-session)
               (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--reorder-workspace-by-priority)
               (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--repo-default-priority-for-path)
               (lambda (_path) "p3"))
              ((symbol-function 'agent-repl--path-canonical) #'identity)
              ((symbol-function 'agent-repl--git-string-quiet) (lambda (&rest _) "")))
      (agent-repl--finalize-worktree-workspace
       "/tmp/new-wt" "child-ws" nil nil nil nil nil "/tmp/parent/")
      (should (equal (agent-repl--ws-get "child-ws" :priority) "p2")))))

;;;; ---- Tests: new-workspace applies repo-default priority ----

(ert-deftest agent-repl-test-new-workspace-applies-repo-default-priority ()
  "`--new-workspace' writes the repo-default priority onto the new ws plist."
  (agent-repl-test--with-clean-state
    (let ((ws-name "new-ws")
          (reorder-called nil))
      (cl-letf (((symbol-function 'agent-repl--git-root) (lambda (&rest _) "/tmp/ee/"))
                ((symbol-function '+workspace/new) (lambda () nil))
                ((symbol-function '+workspace-current-name) (lambda () ws-name))
                ((symbol-function 'agent-repl--initialize-ws-env)
                 (lambda (&rest _) nil))
                ((symbol-function 'agent-repl--repo-default-priority-for-path)
                 (lambda (path) (when (equal path "/tmp/ee/") "p3")))
                ((symbol-function 'agent-repl--reorder-workspace-by-priority)
                 (lambda (_ws) (setq reorder-called t)))
                ((symbol-function 'magit-status) (lambda (&rest _) nil))
                ((symbol-function 'agent-repl--remove-doom-dashboard)
                 (lambda (&rest _) nil)))
        (agent-repl--new-workspace)
        (should (equal (agent-repl--ws-get ws-name :priority) "p3"))
        (should reorder-called)))))

(ert-deftest agent-repl-test-new-workspace-no-default-leaves-priority-unset ()
  "`--new-workspace' leaves :priority unset when no repo-default applies."
  (agent-repl-test--with-clean-state
    (let ((ws-name "new-ws")
          (reorder-called nil))
      (cl-letf (((symbol-function 'agent-repl--git-root) (lambda (&rest _) "/tmp/other/"))
                ((symbol-function '+workspace/new) (lambda () nil))
                ((symbol-function '+workspace-current-name) (lambda () ws-name))
                ((symbol-function 'agent-repl--initialize-ws-env)
                 (lambda (&rest _) nil))
                ((symbol-function 'agent-repl--repo-default-priority-for-path)
                 (lambda (_path) nil))
                ((symbol-function 'agent-repl--reorder-workspace-by-priority)
                 (lambda (_ws) (setq reorder-called t)))
                ((symbol-function 'magit-status) (lambda (&rest _) nil))
                ((symbol-function 'agent-repl--remove-doom-dashboard)
                 (lambda (&rest _) nil)))
        (agent-repl--new-workspace)
        (should-not (agent-repl--ws-get ws-name :priority))
        (should-not reorder-called)))))

(ert-deftest agent-repl-test-new-workspace-priority-set-before-initialize-ws-env ()
  "`--new-workspace' writes :priority BEFORE calling `--initialize-ws-env'.
This matters because `--initialize-ws-env' reads `:priority' off the plist as
a fallback when no saved state exists, persisting the repo-default into the
initial state file."
  (agent-repl-test--with-clean-state
    (let ((ws-name "new-ws")
          (priority-at-init nil))
      (cl-letf (((symbol-function 'agent-repl--git-root) (lambda (&rest _) "/tmp/ee/"))
                ((symbol-function '+workspace/new) (lambda () nil))
                ((symbol-function '+workspace-current-name) (lambda () ws-name))
                ((symbol-function 'agent-repl--initialize-ws-env)
                 (lambda (ws &rest _)
                   (setq priority-at-init (agent-repl--ws-get ws :priority))))
                ((symbol-function 'agent-repl--repo-default-priority-for-path)
                 (lambda (_path) "p3"))
                ((symbol-function 'agent-repl--reorder-workspace-by-priority)
                 (lambda (_ws) nil))
                ((symbol-function 'magit-status) (lambda (&rest _) nil))
                ((symbol-function 'agent-repl--remove-doom-dashboard)
                 (lambda (&rest _) nil)))
        (agent-repl--new-workspace)
        (should (equal priority-at-init "p3"))))))

(ert-deftest agent-repl-test-new-workspace-honors-explicit-root ()
  "`--new-workspace' uses an explicitly-passed ROOT instead of resolving one
from `default-directory' via `agent-repl--git-root'."
  (agent-repl-test--with-clean-state
    (let ((ws-name "new-ws")
          (init-root nil)
          (priority-path nil))
      (cl-letf (((symbol-function 'agent-repl--git-root)
                 (lambda (&rest _) (error "should not resolve root when ROOT given")))
                ((symbol-function '+workspace/new) (lambda () nil))
                ((symbol-function '+workspace-current-name) (lambda () ws-name))
                ((symbol-function 'agent-repl--initialize-ws-env)
                 (lambda (_ws root) (setq init-root root)))
                ((symbol-function 'agent-repl--repo-default-priority-for-path)
                 (lambda (path) (setq priority-path path) nil))
                ((symbol-function 'agent-repl--reorder-workspace-by-priority)
                 (lambda (_ws) nil))
                ((symbol-function 'magit-status) (lambda (&rest _) nil))
                ((symbol-function 'agent-repl--remove-doom-dashboard)
                 (lambda (&rest _) nil)))
        (agent-repl--new-workspace "/tmp/explicit/")
        (should (equal init-root "/tmp/explicit/"))
        (should (equal priority-path "/tmp/explicit/"))))))

;;;; ---- Tests: setup-worktree-session ----

(ert-deftest agent-repl-test-setup-worktree-session-passes-sandbox-hint-when-forced ()
  "When force-sandbox is t, the env hydration receives :sandbox as the env hint."
  ;; Arrange
  (let ((captured-env nil))
    (agent-repl-test--with-worktree-boot-stubs
        (((symbol-function 'agent-repl--initialize-ws-env)
          (lambda (ws &optional _dir env)
            (setq captured-env env)
            (agent-repl--ws-put ws :active-env env))))
      ;; Act
      (agent-repl--setup-worktree-session "abc123" "/tmp/path" "ws1" t)
      ;; Assert
      (should (eq captured-env :sandbox)))))

(ert-deftest agent-repl-test-setup-worktree-session-passes-bare-metal-hint-by-default ()
  "When force-sandbox is nil, the env hydration receives :bare-metal as the env hint."
  ;; Arrange
  (let ((captured-env nil))
    (agent-repl-test--with-worktree-boot-stubs
        (((symbol-function 'agent-repl--initialize-ws-env)
          (lambda (ws &optional _dir env)
            (setq captured-env env)
            (agent-repl--ws-put ws :active-env env))))
      ;; Act
      (agent-repl--setup-worktree-session "abc123" "/tmp/path" "ws1" nil)
      ;; Assert
      (should (eq captured-env :bare-metal)))))

(ert-deftest agent-repl-test-setup-worktree-session-passes-path-hint ()
  "The env hydration receives the worktree PATH as the project-dir hint."
  ;; Arrange
  (let ((captured-dir-hint nil))
    (agent-repl-test--with-worktree-boot-stubs
        (((symbol-function 'agent-repl--initialize-ws-env)
          (lambda (ws &optional dir env)
            (setq captured-dir-hint dir)
            (agent-repl--ws-put ws :active-env (or env :bare-metal)))))
      ;; Act
      (agent-repl--setup-worktree-session "abc123" "/tmp/my-worktree" "ws1" nil)
      ;; Assert
      (should (equal captured-dir-hint "/tmp/my-worktree")))))

(ert-deftest agent-repl-test-setup-worktree-session-passes-path-hint-to-the-frontend-boot ()
  "The frontend's boot capability receives the worktree PATH as its hint."
  ;; Arrange
  (let ((captured-dir-hint nil))
    (agent-repl-test--with-worktree-boot-stubs
        (((symbol-function 'agent-repl--gui-boot)
          (lambda (_ws &optional dir _env) (setq captured-dir-hint dir))))
      ;; Act
      (agent-repl--setup-worktree-session "abc123" "/tmp/my-worktree" "ws1" nil)
      ;; Assert
      (should (equal captured-dir-hint "/tmp/my-worktree")))))

(ert-deftest agent-repl-test-setup-worktree-session-binds-default-directory ()
  "During the boot, default-directory is bound to the worktree path."
  ;; Arrange
  (let ((captured-dir nil))
    (agent-repl-test--with-worktree-boot-stubs
        (((symbol-function 'agent-repl--gui-boot)
          (lambda (&rest _) (setq captured-dir default-directory))))
      ;; Act
      (agent-repl--setup-worktree-session "abc123" "/tmp/my-worktree" "ws1" nil)
      ;; Assert
      (should (equal captured-dir "/tmp/my-worktree/")))))

;;;; ---- Tests: async-git-sentinel ----

(ert-deftest agent-repl-test-async-git-sentinel-exit-success ()
  "Exit with code 0 calls callback with (t output)."
  (let ((captured-ok nil)
        (captured-output nil)
        (proc-buf (generate-new-buffer " *test-sentinel-ok*")))
    (unwind-protect
        (progn
          (with-current-buffer proc-buf
            (insert "  git output here  "))
          (let ((proc (start-process "test-sentinel" proc-buf "true")))
            ;; Prevent default sentinel from inserting status text into buffer
            (set-process-sentinel proc #'ignore)
            ;; Wait for process to finish
            (while (process-live-p proc)
              (accept-process-output proc 0.1))
            (process-put proc 'agent-repl-callback
                         (lambda (ok output)
                           (setq captured-ok ok
                                 captured-output output)))
            (agent-repl--async-git-sentinel proc "finished\n")
            (should (eq captured-ok t))
            (should (equal captured-output "git output here"))))
      (when (buffer-live-p proc-buf)
        (kill-buffer proc-buf)))))

(ert-deftest agent-repl-test-async-git-sentinel-exit-failure ()
  "Exit with non-zero code calls callback with (nil output)."
  (let ((captured-ok 'not-set)
        (captured-output nil)
        (proc-buf (generate-new-buffer " *test-sentinel-fail*")))
    (unwind-protect
        (progn
          (with-current-buffer proc-buf
            (insert "fatal: error message"))
          (let ((proc (start-process "test-sentinel" proc-buf "false")))
            ;; Prevent default sentinel from inserting status text into buffer
            (set-process-sentinel proc #'ignore)
            ;; Wait for process to finish
            (while (process-live-p proc)
              (accept-process-output proc 0.1))
            (process-put proc 'agent-repl-callback
                         (lambda (ok output)
                           (setq captured-ok ok
                                 captured-output output)))
            (agent-repl--async-git-sentinel proc "finished\n")
            (should (eq captured-ok nil))
            (should (equal captured-output "fatal: error message"))))
      (when (buffer-live-p proc-buf)
        (kill-buffer proc-buf)))))

(ert-deftest agent-repl-test-async-git-sentinel-signal ()
  "Signaled process also invokes the callback."
  (let ((captured-ok 'not-set)
        (captured-output nil)
        (proc-buf (generate-new-buffer " *test-sentinel-signal*")))
    (unwind-protect
        (progn
          (with-current-buffer proc-buf
            (insert "partial output"))
          (let ((proc (start-process "test-sentinel" proc-buf "sleep" "60")))
            (process-put proc 'agent-repl-callback
                         (lambda (ok output)
                           (setq captured-ok ok
                                 captured-output output)))
            ;; Kill the process to produce a signal
            (kill-process proc)
            ;; Wait for process to be fully dead
            (while (process-live-p proc)
              (accept-process-output proc 0.1))
            (agent-repl--async-git-sentinel proc "killed\n")
            (should (not (eq captured-ok 'not-set)))
            (should (stringp captured-output))))
      (when (buffer-live-p proc-buf)
        (kill-buffer proc-buf)))))

(ert-deftest agent-repl-test-async-git-sentinel-kills-process-buffer ()
  "Process buffer is killed after callback is invoked."
  (let ((proc-buf (generate-new-buffer " *test-sentinel-bufkill*")))
    (with-current-buffer proc-buf
      (insert "output"))
    (let ((proc (start-process "test-sentinel" proc-buf "true")))
      ;; Wait for process to finish
      (while (process-live-p proc)
        (accept-process-output proc 0.1))
      (process-put proc 'agent-repl-callback (lambda (_ok _output) nil))
      (agent-repl--async-git-sentinel proc "finished\n")
      (should-not (buffer-live-p proc-buf)))))

;;;; ---- Tests: open-initial-buffers (moved from core.el) ----

(ert-deftest agent-repl-test-open-initial-buffers-no-persp ()
  "open-initial-buffers should return nil when persp-get-by-name returns nil."
  (cl-letf (((symbol-function 'persp-get-by-name) (lambda (_name) nil)))
    (let ((agent-repl-workspace-initial-buffers '(("." . ("file.txt")))))
      ;; Should not error
      (should-not (agent-repl--open-initial-buffers "ws1" "/tmp/")))))

(ert-deftest agent-repl-test-open-initial-buffers-no-matching-pattern ()
  "open-initial-buffers should do nothing when no patterns match."
  (let ((add-called nil))
    (cl-letf (((symbol-function 'persp-get-by-name) (lambda (_name) 'fake-persp))
              ((symbol-function 'persp-add-buffer)
               (lambda (&rest _) (setq add-called t))))
      (let ((agent-repl-workspace-initial-buffers '(("^/specific/path" . ("file.txt")))))
        (agent-repl--open-initial-buffers "ws1" "/different/path")
        (should-not add-called)))))

(ert-deftest agent-repl-test-open-initial-buffers-missing-file-warns ()
  "open-initial-buffers should warn for missing files, not error."
  (let ((warned nil))
    (cl-letf (((symbol-function 'persp-get-by-name) (lambda (_name) 'fake-persp))
              ((symbol-function 'agent-repl--log)
               (lambda (_ws fmt &rest _args)
                 (when (string-match-p "not found" fmt)
                   (setq warned t)))))
      (let ((agent-repl-workspace-initial-buffers
             '(("." . ("nonexistent-file-12345.txt")))))
        (agent-repl--open-initial-buffers "ws1" "/tmp/")
        (should warned)))))

(ert-deftest agent-repl-test-open-initial-buffers-existing-file ()
  "open-initial-buffers should add existing file to perspective."
  (let ((tmpdir (make-temp-file "test-init-buf-" t))
        (added-buffers nil))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "found.txt" tmpdir)
            (insert "content"))
          (cl-letf (((symbol-function 'persp-get-by-name) (lambda (_name) 'fake-persp))
                    ((symbol-function 'persp-add-buffer)
                     (lambda (buf &rest _) (push buf added-buffers))))
            (let ((agent-repl-workspace-initial-buffers
                   (list (cons "." '("found.txt")))))
              (agent-repl--open-initial-buffers "ws1" tmpdir)
              (should (= (length added-buffers) 1)))))
      ;; cleanup: kill the file buffer if it was created
      (let ((fb (get-buffer "found.txt")))
        (when fb (kill-buffer fb)))
      (delete-directory tmpdir t))))

(ert-deftest agent-repl-test-open-initial-buffers-multiple-patterns ()
  "open-initial-buffers should match multiple patterns and add files for each."
  (let ((tmpdir (make-temp-file "test-init-multi-" t))
        (added-count 0))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "a.txt" tmpdir) (insert "a"))
          (with-temp-file (expand-file-name "b.txt" tmpdir) (insert "b"))
          (cl-letf (((symbol-function 'persp-get-by-name) (lambda (_name) 'fake-persp))
                    ((symbol-function 'persp-add-buffer)
                     (lambda (_buf &rest _) (cl-incf added-count))))
            (let ((agent-repl-workspace-initial-buffers
                   (list (cons "." '("a.txt"))
                         (cons "." '("b.txt")))))
              (agent-repl--open-initial-buffers "ws1" tmpdir)
              (should (= added-count 2)))))
      (dolist (name '("a.txt" "b.txt"))
        (let ((fb (get-buffer name)))
          (when fb (kill-buffer fb))))
      (delete-directory tmpdir t))))

(ert-deftest agent-repl-test-open-initial-buffers-empty-files-list ()
  "open-initial-buffers with empty FILES list should be a no-op."
  (let ((add-called nil))
    (cl-letf (((symbol-function 'persp-get-by-name) (lambda (_name) 'fake-persp))
              ((symbol-function 'persp-add-buffer)
               (lambda (&rest _) (setq add-called t))))
      (let ((agent-repl-workspace-initial-buffers '(("." . ()))))
        (agent-repl--open-initial-buffers "ws1" "/tmp/")
        (should-not add-called)))))

;;;; ---- Tests: resolve-fork-session-id ----

(ert-deftest agent-repl-test-resolve-fork-session-id-nil ()
  "resolve-fork-session-id returns nil when fork-from is nil."
  (should-not (agent-repl--resolve-fork-session-id nil)))

(ert-deftest agent-repl-test-resolve-fork-session-id-known-ws ()
  "resolve-fork-session-id returns session ID for a known workspace."
  (agent-repl-test--with-clean-state
    (let ((inst (make-agent-repl-instantiation :session-id "abc-123")))
      (agent-repl--ws-put "my-ws" :active-env :bare-metal)
      (agent-repl--ws-put "my-ws" :bare-metal inst)
      (should (equal (agent-repl--resolve-fork-session-id "my-ws") "abc-123")))))

(ert-deftest agent-repl-test-resolve-fork-session-id-normalizes-branch ()
  "resolve-fork-session-id normalizes DWC/my-ws to my-ws."
  (agent-repl-test--with-clean-state
    (let ((inst (make-agent-repl-instantiation :session-id "def-456")))
      (agent-repl--ws-put "my-ws" :active-env :bare-metal)
      (agent-repl--ws-put "my-ws" :bare-metal inst)
      (should (equal (agent-repl--resolve-fork-session-id "DWC/my-ws") "def-456")))))

(ert-deftest agent-repl-test-resolve-fork-session-id-no-session-errors ()
  "resolve-fork-session-id signals error when workspace has no session ID."
  (agent-repl-test--with-clean-state
    (let ((inst (make-agent-repl-instantiation)))
      (agent-repl--ws-put "my-ws" :active-env :bare-metal)
      (agent-repl--ws-put "my-ws" :bare-metal inst)
      (should-error (agent-repl--resolve-fork-session-id "my-ws") :type 'error))))

(ert-deftest agent-repl-test-resolve-fork-session-id-unknown-ws-errors ()
  "resolve-fork-session-id signals error for an unknown workspace."
  (agent-repl-test--with-clean-state
    (should-error (agent-repl--resolve-fork-session-id "nonexistent") :type 'error)))

;;;; ---- Tests: handle-create-command with fork_from ----

(ert-deftest agent-repl-test-handle-create-command-with-fork-from ()
  "handle-create-command should resolve fork_from and pass fork-session-id."
  (agent-repl-test--with-clean-state
    (let ((inst (make-agent-repl-instantiation :session-id "fork-sid-789")))
      (agent-repl--ws-put "source-ws" :active-env :bare-metal)
      (agent-repl--ws-put "source-ws" :bare-metal inst)
      (let ((captured-args nil))
        (cl-letf (((symbol-function 'agent-repl--workspace-name-collides-p)
                   (lambda (&rest _) nil))
                  ((symbol-function 'run-with-timer)
                   (lambda (_delay _repeat fn &rest args)
                     (setq captured-args args))))
          (agent-repl--handle-create-command
           '((type . "create") (name . "DWC/new-ws")
             (git_root . "/fake/root") (fork_from . "source-ws"))
           0)
          ;; captured-args = (git-root name prompt priority fork-session-id)
          (should (equal (nth 0 captured-args) "/fake/root/"))
          (should (equal (nth 1 captured-args) "DWC/new-ws"))
          (should (equal (nth 4 captured-args) "fork-sid-789")))))))

(ert-deftest agent-repl-test-handle-create-command-without-fork-from ()
  "handle-create-command without fork_from should pass nil fork-session-id."
  (agent-repl-test--with-clean-state
    (let ((captured-args nil))
      (cl-letf (((symbol-function 'agent-repl--workspace-name-collides-p)
                 (lambda (&rest _) nil))
                ((symbol-function 'run-with-timer)
                 (lambda (_delay _repeat fn &rest args)
                   (setq captured-args args))))
        (agent-repl--handle-create-command
         '((type . "create") (name . "DWC/new-ws") (git_root . "/fake/root"))
         0)
        ;; captured-args = (git-root name prompt priority fork-session-id)
        (should (equal (nth 0 captured-args) "/fake/root/"))
        (should (equal (nth 1 captured-args) "DWC/new-ws"))
        (should-not (nth 4 captured-args))))))

(ert-deftest agent-repl-test-handle-create-command-fork-from-no-session-aborts ()
  "handle-create-command with fork_from but no session should refuse to create workspace."
  (agent-repl-test--with-clean-state
    (let ((inst (make-agent-repl-instantiation)))
      (agent-repl--ws-put "source-ws" :active-env :bare-metal)
      (agent-repl--ws-put "source-ws" :bare-metal inst)
      (let ((timer-scheduled nil))
        (cl-letf (((symbol-function 'run-with-timer)
                   (lambda (_delay _repeat fn &rest args)
                     (setq timer-scheduled t))))
          (agent-repl--handle-create-command
           '((type . "create") (name . "DWC/new-ws") (fork_from . "source-ws"))
           0)
          ;; Timer must NOT be scheduled -- workspace creation was refused.
          (should-not timer-scheduled))))))

(ert-deftest agent-repl-test-handle-create-command-fork-from-unknown-ws-aborts ()
  "handle-create-command with fork_from referencing unknown workspace should refuse to create."
  (agent-repl-test--with-clean-state
    (let ((timer-scheduled nil))
      (cl-letf (((symbol-function 'run-with-timer)
                 (lambda (_delay _repeat fn &rest args)
                   (setq timer-scheduled t))))
        (agent-repl--handle-create-command
         '((type . "create") (name . "DWC/new-ws") (fork_from . "nonexistent"))
         0)
        (should-not timer-scheduled)))))

(ert-deftest agent-repl-test-handle-create-command-uses-explicit-git-root ()
  "handle-create-command with git_root in cmd should use it verbatim and skip ambient resolution."
  (agent-repl-test--with-clean-state
    (let ((captured-args nil)
          (resolve-calls 0))
      (cl-letf (((symbol-function 'agent-repl--resolve-current-git-root)
                 (lambda () (cl-incf resolve-calls) "/ambient/root/"))
                ((symbol-function 'agent-repl--workspace-name-collides-p)
                 (lambda (&rest _) nil))
                ((symbol-function 'run-with-timer)
                 (lambda (_delay _repeat _fn &rest args)
                   (setq captured-args args))))
        (agent-repl--handle-create-command
         '((type . "create") (name . "DWC/new-ws") (git_root . "/explicit/root"))
         0)
        ;; captured-args = (git-root name prompt priority fork-session-id)
        (should (equal (nth 0 captured-args) "/explicit/root/"))
        (should (equal resolve-calls 0)))))

(ert-deftest agent-repl-test-handle-create-command-passes-force-sandbox-true ()
  "handle-create-command with force_sandbox: true in cmd forwards it as the
last positional arg to `agent-repl--create-worktree-from-command'."
  (agent-repl-test--with-clean-state
    (let ((captured-args nil))
      (cl-letf (((symbol-function 'agent-repl--workspace-name-collides-p)
                 (lambda (&rest _) nil))
                ((symbol-function 'run-with-timer)
                 (lambda (_delay _repeat _fn &rest args)
                   (setq captured-args args))))
        (agent-repl--handle-create-command
         '((type . "create") (name . "DWC/new-ws")
           (git_root . "/fake/root") (force_sandbox . t))
         0)
        ;; captured-args = (git-root name prompt priority fork-session-id base-commit force-sandbox)
        (should (nth 6 captured-args))))))

(ert-deftest agent-repl-test-handle-create-command-passes-force-sandbox-nil-when-absent ()
  "handle-create-command with no force_sandbox field passes nil so the
workspace defaults to bare-metal."
  (agent-repl-test--with-clean-state
    (let ((captured-args nil))
      (cl-letf (((symbol-function 'agent-repl--workspace-name-collides-p)
                 (lambda (&rest _) nil))
                ((symbol-function 'run-with-timer)
                 (lambda (_delay _repeat _fn &rest args)
                   (setq captured-args args))))
        (agent-repl--handle-create-command
         '((type . "create") (name . "DWC/new-ws") (git_root . "/fake/root"))
         0)
        ;; captured-args = (git-root name prompt priority fork-session-id base-commit force-sandbox)
        (should (null (nth 6 captured-args))))))))

(ert-deftest agent-repl-test-handle-create-command-expands-tilde-in-git-root ()
  "handle-create-command should expand `~' in an explicit git_root before dispatch."
  (agent-repl-test--with-clean-state
    (let ((captured-args nil)
          (home (expand-file-name "~")))
      (cl-letf (((symbol-function 'agent-repl--resolve-current-git-root)
                 (lambda () "/ambient/root/"))
                ((symbol-function 'agent-repl--workspace-name-collides-p)
                 (lambda (&rest _) nil))
                ((symbol-function 'run-with-timer)
                 (lambda (_delay _repeat _fn &rest args)
                   (setq captured-args args))))
        (agent-repl--handle-create-command
         '((type . "create") (name . "DWC/new-ws") (git_root . "~/some/repo"))
         0)
        (should (equal (nth 0 captured-args)
                       (file-name-as-directory (expand-file-name "~/some/repo"))))
        ;; Sanity: the expanded value is rooted at HOME, not the literal tilde.
        (should (string-prefix-p home (nth 0 captured-args)))))))

(ert-deftest agent-repl-test-handle-create-command-empty-git-root-refuses ()
  "handle-create-command with an empty git_root string must refuse — no ambient fallback."
  (agent-repl-test--with-clean-state
    (let ((timer-scheduled nil)
          (resolve-calls 0))
      (cl-letf (((symbol-function 'agent-repl--resolve-current-git-root)
                 (lambda () (cl-incf resolve-calls) "/ambient/root/"))
                ((symbol-function 'run-with-timer)
                 (lambda (_delay _repeat _fn &rest _args)
                   (setq timer-scheduled t))))
        (agent-repl--handle-create-command
         '((type . "create") (name . "DWC/new-ws") (git_root . ""))
         0)
        (should-not timer-scheduled)
        (should (equal resolve-calls 0))))))

(ert-deftest agent-repl-test-handle-create-command-missing-git-root-refuses ()
  "handle-create-command with no git_root key must refuse — no ambient fallback."
  (agent-repl-test--with-clean-state
    (let ((timer-scheduled nil)
          (resolve-calls 0))
      (cl-letf (((symbol-function 'agent-repl--resolve-current-git-root)
                 (lambda () (cl-incf resolve-calls) "/ambient/root/"))
                ((symbol-function 'run-with-timer)
                 (lambda (_delay _repeat _fn &rest _args)
                   (setq timer-scheduled t))))
        (agent-repl--handle-create-command
         '((type . "create") (name . "DWC/new-ws"))
         0)
        (should-not timer-scheduled)
        (should (equal resolve-calls 0))))))

(ert-deftest agent-repl-test-handle-create-command-missing-name-refuses ()
  "handle-create-command with no `name' key must refuse — a missing name
would otherwise leak a phantom \"none\" / \"nil\" workspace into the
registry once `--bare-workspace-name' is called on it downstream."
  (agent-repl-test--with-clean-state
    (let ((timer-scheduled nil))
      (cl-letf (((symbol-function 'run-with-timer)
                 (lambda (_delay _repeat _fn &rest _args)
                   (setq timer-scheduled t))))
        (agent-repl--handle-create-command
         '((type . "create") (git_root . "/fake/root"))
         0)
        (should-not timer-scheduled)))))

(ert-deftest agent-repl-test-handle-create-command-null-name-refuses ()
  "handle-create-command with JSON `null' name (parsed as `:null') must refuse."
  (agent-repl-test--with-clean-state
    (let ((timer-scheduled nil))
      (cl-letf (((symbol-function 'run-with-timer)
                 (lambda (_delay _repeat _fn &rest _args)
                   (setq timer-scheduled t))))
        (agent-repl--handle-create-command
         '((type . "create") (name . :null) (git_root . "/fake/root"))
         0)
        (should-not timer-scheduled)))))

(ert-deftest agent-repl-test-handle-create-command-empty-name-refuses ()
  "handle-create-command with an empty-string `name' must refuse."
  (agent-repl-test--with-clean-state
    (let ((timer-scheduled nil))
      (cl-letf (((symbol-function 'run-with-timer)
                 (lambda (_delay _repeat _fn &rest _args)
                   (setq timer-scheduled t))))
        (agent-repl--handle-create-command
         '((type . "create") (name . "") (git_root . "/fake/root"))
         0)
        (should-not timer-scheduled)))))

(ert-deftest agent-repl-test-handle-create-command-persp-nil-name-refuses ()
  "handle-create-command with a bare `name' equal to `persp-nil-name' must
refuse.  The headless `/workspace-generation' flow occasionally emits
\"none\" (or \"DWC/none\") when there is no slug material; without this
guard, the downstream `+workspace-new' would collide with the
nil-perspective sentinel and the entry would surface in the drawer and
nuke prompts as a stray \"none\" workspace."
  (agent-repl-test--with-clean-state
    (let ((timer-scheduled nil)
          (persp-nil-name "none"))
      (cl-letf (((symbol-function 'run-with-timer)
                 (lambda (_delay _repeat _fn &rest _args)
                   (setq timer-scheduled t))))
        (agent-repl--handle-create-command
         '((type . "create") (name . "none") (git_root . "/fake/root"))
         0)
        (should-not timer-scheduled)))))

(ert-deftest agent-repl-test-handle-create-command-dwc-persp-nil-name-refuses ()
  "handle-create-command must refuse a `DWC/none' name because the bare
form collides with `persp-nil-name'."
  (agent-repl-test--with-clean-state
    (let ((timer-scheduled nil)
          (persp-nil-name "none"))
      (cl-letf (((symbol-function 'run-with-timer)
                 (lambda (_delay _repeat _fn &rest _args)
                   (setq timer-scheduled t))))
        (agent-repl--handle-create-command
         '((type . "create") (name . "DWC/none") (git_root . "/fake/root"))
         0)
        (should-not timer-scheduled)))))

(ert-deftest agent-repl-test-handle-create-command-passes-base-commit-when-given ()
  "handle-create-command threads a non-empty `base_commit' field through to the
timer callback — letting the workspace-generation flow request HEAD without forking."
  (agent-repl-test--with-clean-state
    (let ((captured-args nil))
      (cl-letf (((symbol-function 'agent-repl--workspace-name-collides-p)
                 (lambda (&rest _) nil))
                ((symbol-function 'run-with-timer)
                 (lambda (_delay _repeat _fn &rest args)
                   (setq captured-args args))))
        (agent-repl--handle-create-command
         '((type . "create") (name . "DWC/new-ws")
           (git_root . "/fake/root") (base_commit . "HEAD"))
         0)
        ;; captured-args = (git-root name prompt priority fork-session-id base-commit)
        (should (equal (nth 5 captured-args) "HEAD"))))))

(ert-deftest agent-repl-test-handle-create-command-empty-base-commit-passes-nil ()
  "An empty base_commit string is normalized to nil so the downstream default applies."
  (agent-repl-test--with-clean-state
    (let ((captured-args nil))
      (cl-letf (((symbol-function 'run-with-timer)
                 (lambda (_delay _repeat _fn &rest args)
                   (setq captured-args args))))
        (agent-repl--handle-create-command
         '((type . "create") (name . "DWC/new-ws")
           (git_root . "/fake/root") (base_commit . ""))
         0)
        (should (null (nth 5 captured-args)))))))

(ert-deftest agent-repl-test-handle-create-command-missing-base-commit-passes-nil ()
  "An absent base_commit field passes nil so the downstream default applies
(HEAD for forks, `agent-repl-worktree-default-base' otherwise)."
  (agent-repl-test--with-clean-state
    (let ((captured-args nil))
      (cl-letf (((symbol-function 'run-with-timer)
                 (lambda (_delay _repeat _fn &rest args)
                   (setq captured-args args))))
        (agent-repl--handle-create-command
         '((type . "create") (name . "DWC/new-ws") (git_root . "/fake/root"))
         0)
        (should (null (nth 5 captured-args)))))))

(ert-deftest agent-repl-test-create-worktree-from-command-threads-base-commit ()
  "`--create-worktree-from-command' forwards BASE-COMMIT to `--do-create-worktree-workspace'."
  (agent-repl-test--with-clean-state
    (let ((captured-base :unset))
      (cl-letf (((symbol-function 'agent-repl--do-create-worktree-workspace)
                 (lambda (_name _bare _fork _prompt _cb _priority base &rest _)
                   (setq captured-base base))))
        (agent-repl--create-worktree-from-command
         "/tmp/cmd-repo/" "name" "prompt" 5 nil "HEAD")
        (should (equal captured-base "HEAD"))))))

(ert-deftest agent-repl-test-create-worktree-from-command-nil-base-commit-passes-nil ()
  "When BASE-COMMIT is nil, `--do-create-worktree-workspace' receives nil and applies its default."
  (agent-repl-test--with-clean-state
    (let ((captured-base :unset))
      (cl-letf (((symbol-function 'agent-repl--do-create-worktree-workspace)
                 (lambda (_name _bare _fork _prompt _cb _priority base &rest _)
                   (setq captured-base base))))
        (agent-repl--create-worktree-from-command
         "/tmp/cmd-repo/" "name" "prompt" 5 nil nil)
        (should (null captured-base))))))

(ert-deftest agent-repl-test-create-worktree-from-command-master-base-uses-master-worktree-as-source-dir ()
  "BASE-COMMIT = `master' resolves source-dir via `--master-worktree-path'.
For `SPC TAB N', the new workspace's `:source-ws-dir' must be the master
worktree of the repo, not the calling workspace — otherwise the drawer
nests it under a parent that shares no commits with it."
  (agent-repl-test--with-clean-state
    (let ((captured-source-dir :unset)
          (master-lookup-root :unset)
          (agent-repl-master-branch-name "master"))
      (cl-letf (((symbol-function 'agent-repl--master-worktree-path)
                 (lambda (root)
                   (setq master-lookup-root root)
                   "/tmp/master/"))
                ((symbol-function 'agent-repl--do-create-worktree-workspace)
                 (lambda (_name _bare _fork _prompt _cb _priority _base _git source-dir &rest _)
                   (setq captured-source-dir source-dir))))
        (agent-repl--create-worktree-from-command
         "/tmp/calling-ws/" "name" "prompt" 5 nil "master")
        (should (equal master-lookup-root "/tmp/calling-ws/"))
        (should (equal captured-source-dir "/tmp/master/"))))))

(ert-deftest agent-repl-test-create-worktree-from-command-master-base-nil-master-yields-nil-source-dir ()
  "BASE-COMMIT = `master' with no master worktree yields nil `:source-ws-dir'.
When the repo has no worktree on master (e.g. main checkout itself is on
a feature branch), the new workspace must not fall back to the calling
workspace as its parent.  Nil leaves it parentless in the drawer — the
correct outcome since `SPC TAB N' branches off master, not the caller."
  (agent-repl-test--with-clean-state
    (let ((captured-source-dir :unset)
          (agent-repl-master-branch-name "master"))
      (cl-letf (((symbol-function 'agent-repl--master-worktree-path)
                 (lambda (_root) nil))
                ((symbol-function 'agent-repl--do-create-worktree-workspace)
                 (lambda (_name _bare _fork _prompt _cb _priority _base _git source-dir &rest _)
                   (setq captured-source-dir source-dir))))
        (agent-repl--create-worktree-from-command
         "/tmp/calling-ws/" "name" "prompt" 5 nil "master")
        (should (null captured-source-dir))))))

(ert-deftest agent-repl-test-create-worktree-from-command-head-base-uses-git-root-as-source-dir ()
  "BASE-COMMIT = `HEAD' yields source-dir == git-root (calling workspace).
`SPC TAB n' is a child-of-current operation; its drawer parent must be
the calling workspace, captured as GIT-ROOT at enqueue time."
  (agent-repl-test--with-clean-state
    (let ((captured-source-dir :unset)
          (master-path-called nil))
      (cl-letf (((symbol-function 'agent-repl--master-worktree-path)
                 (lambda (_root)
                   (setq master-path-called t)
                   "/tmp/master/"))
                ((symbol-function 'agent-repl--do-create-worktree-workspace)
                 (lambda (_name _bare _fork _prompt _cb _priority _base _git source-dir &rest _)
                   (setq captured-source-dir source-dir))))
        (agent-repl--create-worktree-from-command
         "/tmp/calling-ws/" "name" "prompt" 5 nil "HEAD")
        (should-not master-path-called)
        (should (equal captured-source-dir "/tmp/calling-ws/"))))))

(ert-deftest agent-repl-test-create-worktree-from-command-nil-base-commit-uses-git-root-as-source-dir ()
  "Absent BASE-COMMIT yields source-dir == git-root.
With no base hint, source-dir defaults to the calling workspace dir —
the master special case only kicks in for an explicit `master' value."
  (agent-repl-test--with-clean-state
    (let ((captured-source-dir :unset)
          (master-path-called nil))
      (cl-letf (((symbol-function 'agent-repl--master-worktree-path)
                 (lambda (_root)
                   (setq master-path-called t)
                   "/tmp/master/"))
                ((symbol-function 'agent-repl--do-create-worktree-workspace)
                 (lambda (_name _bare _fork _prompt _cb _priority _base _git source-dir &rest _)
                   (setq captured-source-dir source-dir))))
        (agent-repl--create-worktree-from-command
         "/tmp/calling-ws/" "name" "prompt" 5 nil nil)
        (should-not master-path-called)
        (should (equal captured-source-dir "/tmp/calling-ws/"))))))

(ert-deftest agent-repl-test-create-worktree-from-command-origin-master-base-uses-git-root-as-source-dir ()
  "BASE-COMMIT = `origin/master' (or any non-master ref) yields source-dir == git-root.
Only the literal `agent-repl-master-branch-name' triggers master-worktree
lookup; arbitrary refs like `origin/master' or a SHA route through the
default path so the parent is the originating workspace."
  (agent-repl-test--with-clean-state
    (let ((captured-source-dir :unset)
          (master-path-called nil)
          (agent-repl-master-branch-name "master"))
      (cl-letf (((symbol-function 'agent-repl--master-worktree-path)
                 (lambda (_root)
                   (setq master-path-called t)
                   "/tmp/master/"))
                ((symbol-function 'agent-repl--do-create-worktree-workspace)
                 (lambda (_name _bare _fork _prompt _cb _priority _base _git source-dir &rest _)
                   (setq captured-source-dir source-dir))))
        (agent-repl--create-worktree-from-command
         "/tmp/calling-ws/" "name" "prompt" 5 nil "origin/master")
        (should-not master-path-called)
        (should (equal captured-source-dir "/tmp/calling-ws/"))))))

(ert-deftest agent-repl-test-create-worktree-from-command-master-base-honors-custom-branch-name ()
  "Source-dir resolution honors a custom `agent-repl-master-branch-name'.
When the trunk is named `trunk' rather than `master', BASE-COMMIT = `trunk'
triggers the master-worktree lookup."
  (agent-repl-test--with-clean-state
    (let ((captured-source-dir :unset)
          (master-path-called nil)
          (agent-repl-master-branch-name "trunk"))
      (cl-letf (((symbol-function 'agent-repl--master-worktree-path)
                 (lambda (_root)
                   (setq master-path-called t)
                   "/tmp/trunk/"))
                ((symbol-function 'agent-repl--do-create-worktree-workspace)
                 (lambda (_name _bare _fork _prompt _cb _priority _base _git source-dir &rest _)
                   (setq captured-source-dir source-dir))))
        (agent-repl--create-worktree-from-command
         "/tmp/calling-ws/" "name" "prompt" 5 nil "trunk")
        (should master-path-called)
        (should (equal captured-source-dir "/tmp/trunk/"))))))

;;;; ---- Tests: build-preemptive-prompt ----

(ert-deftest agent-repl-test-build-preemptive-prompt-marks-the-preamble ()
  "The autonomous preamble is bracketed as a harness-injected span."
  (should (string-prefix-p
           (agent-repl--meta-wrap agent-repl--autonomous-prompt-prefix)
           (agent-repl--build-preemptive-prompt "do the thing"))))

(ert-deftest agent-repl-test-build-preemptive-prompt-leaves-user-text-unmarked ()
  "The user's own words carry no markers — they are what the gui bubble shows."
  (should (string-suffix-p "do the thing"
                           (agent-repl--build-preemptive-prompt "do the thing"))))

(ert-deftest agent-repl-test-build-preemptive-prompt-marks-the-suffix ()
  "The success-gated wrap-up suffix is bracketed as a harness-injected span."
  (should (string-suffix-p
           (agent-repl--meta-wrap "\n\nWRAP-UP GATE")
           (agent-repl--build-preemptive-prompt "do the thing" "\n\nWRAP-UP GATE"))))

(ert-deftest agent-repl-test-build-preemptive-prompt-omits-absent-suffix ()
  "With no suffix, nothing follows the user's text (no empty marked span)."
  (should (equal (agent-repl--build-preemptive-prompt "do the thing")
                 (concat (agent-repl--meta-wrap agent-repl--autonomous-prompt-prefix)
                         "do the thing"))))

(ert-deftest agent-repl-test-build-preemptive-prompt-unmarks-to-the-agent-message ()
  "Unmarking the composed prompt yields exactly the message the agent must read.
The markers are annotation, never content: dropping them must leave the
preamble + task + gate concatenation the agent always received."
  (should (equal (agent-repl--meta-unmark
                  (agent-repl--build-preemptive-prompt "do the thing" "\n\nWRAP-UP GATE"))
                 (concat agent-repl--autonomous-prompt-prefix
                         "do the thing"
                         "\n\nWRAP-UP GATE"))))

;;;; ---- Tests: autonomous-prompt-prefix content ----

(ert-deftest agent-repl-test-autonomous-prompt-prefix-keeps-plan-framing ()
  "The prefix retains the autonomous plan + task framing that is workspace-
creation specific (the bit that the metaprompt does NOT cover)."
  (should (string-match-p
           (regexp-quote "Do not wait for further instructions.")
           agent-repl--autonomous-prompt-prefix))
  (should (string-match-p
           (regexp-quote "Come up with a plan and then immediately execute on it.")
           agent-repl--autonomous-prompt-prefix))
  (should (string-suffix-p "Here is the task:\n\n"
                           agent-repl--autonomous-prompt-prefix)))

(ert-deftest agent-repl-test-autonomous-prompt-prefix-omits-commit-policy ()
  "The commit policy (commit-often, tests-before-commit, no-other-mutating-git)
has been migrated to the metaprompt and MUST NOT be duplicated in the
prefix — otherwise the two sources can drift.  Each forbidden token
guards a specific historical phrase from the old prefix."
  (dolist (forbidden '("Commit freely"
                       "do not commit before corresponding tests"
                       "Never rebase"
                       "mutating git commands"))
    (should-not (string-match-p (regexp-quote forbidden)
                                agent-repl--autonomous-prompt-prefix))))

;;;; ---- Tests: workspace-generation prompt construction ----

(ert-deftest agent-repl-test-workspace-generation-prompt-includes-raw-and-prefixed ()
  "The generated prompt contains both the raw description (for naming) and the
prefixed prompt (for the new workspace's session)."
  (let* ((raw "fix login flow")
         (prefixed (concat agent-repl--autonomous-prompt-prefix raw))
         (out (agent-repl--workspace-generation-prompt
               raw prefixed "/tmp/repo/" "HEAD" nil)))
    (should (string-match-p (regexp-quote raw) out))
    (should (string-match-p (regexp-quote prefixed) out))))

(ert-deftest agent-repl-test-workspace-generation-prompt-emits-required-fields ()
  "The prompt instructs the model to emit `type', `git_root', and `base_commit'
with the exact values supplied."
  (let ((out (agent-repl--workspace-generation-prompt
              "raw" "prefixed" "/tmp/repo/" "origin/master" nil)))
    (should (string-match-p "\"type\": \"create\"" out))
    (should (string-match-p "\"git_root\": \"/tmp/repo/\"" out))
    (should (string-match-p "\"base_commit\": \"origin/master\"" out))))

(ert-deftest agent-repl-test-workspace-generation-prompt-omits-fork-from-when-nil ()
  "When FORK-FROM is nil, no `fork_from' line is emitted in the prompt."
  (let ((out (agent-repl--workspace-generation-prompt
              "raw" "prefixed" "/tmp/repo/" "HEAD" nil)))
    (should-not (string-match-p "fork_from" out))))

(ert-deftest agent-repl-test-workspace-generation-prompt-includes-fork-from-when-set ()
  "When FORK-FROM is set, the prompt instructs the model to emit a matching
`fork_from' field."
  (let ((out (agent-repl--workspace-generation-prompt
              "raw" "prefixed" "/tmp/repo/" "HEAD" "source-ws")))
    (should (string-match-p "\"fork_from\": \"source-ws\"" out))))

(ert-deftest agent-repl-test-workspace-generation-prompt-tells-model-not-to-ask ()
  "The prompt instructs the model not to request permission — in headless
`-p' mode there is no one to approve, and the spawn previously died
emitting only the permission question."
  (let ((out (agent-repl--workspace-generation-prompt
              "raw" "prefixed" "/tmp/repo/" "HEAD" nil)))
    (should (string-match-p "[Dd]o NOT ask for permission" out))))

(ert-deftest agent-repl-test-workspace-generation-prompt-isolates-inner-prompt ()
  "The headless prompt explicitly tells the model the inner string is the
USER PROMPT for a separate spawned agent and is NOT instructions for the
headless model itself.  Without this, the headless model can read a
suffix like `invoke /workspace-merge' inside the inner prompt and run it
itself instead of just emitting it verbatim into the JSON."
  (let ((out (agent-repl--workspace-generation-prompt
              "raw" "prefixed" "/tmp/repo/" "HEAD" nil)))
    (should (string-match-p "NOT instructions for you" out))
    (should (string-match-p "verbatim" out))))

(ert-deftest agent-repl-test-workspace-generation-prompt-requires-array-top-level ()
  "The prompt explicitly tells the model the JSON top-level must be an array
even for a single workspace.  Previously the model emitted a bare object
`{...}' and the elisp parser crashed with `listp, (type . \"create\")'
when `dolist' iterated the alist's cons cells."
  (let ((out (agent-repl--workspace-generation-prompt
              "raw" "prefixed" "/tmp/repo/" "HEAD" nil)))
    (should (string-match-p "MUST be an array" out))))

(ert-deftest agent-repl-test-workspace-generation-prompt-emits-force-sandbox-when-set ()
  "When FORCE-SANDBOX is non-nil the prompt instructs the model to emit
`\"force_sandbox\": true' so the spawned workspace runs in the sandbox."
  (let ((out (agent-repl--workspace-generation-prompt
              "raw" "prefixed" "/tmp/repo/" "HEAD" nil t)))
    (should (string-match-p "\"force_sandbox\": true" out))))

(ert-deftest agent-repl-test-workspace-generation-prompt-omits-force-sandbox-when-nil ()
  "When FORCE-SANDBOX is nil the prompt does not mention force_sandbox
at all — no false field emitted for non-sandboxed repos."
  (let ((out (agent-repl--workspace-generation-prompt
              "raw" "prefixed" "/tmp/repo/" "HEAD" nil nil)))
    (should-not (string-match-p "force_sandbox" out))))

(ert-deftest agent-repl-test-workspace-generation-prompt-slug-uses-prefix-when-set ()
  "The slug instruction includes the `<prefix>/' form when
CLAUDE_WORKSPACE_PREFIX is set, so generated names carry the prefix."
  (cl-letf (((symbol-function 'getenv)
             (lambda (k) (and (equal k "CLAUDE_WORKSPACE_PREFIX") "DWC"))))
    (let ((out (agent-repl--workspace-generation-prompt
                "raw" "prefixed" "/tmp/repo/" "HEAD" nil)))
      (should (string-match-p "Generate the `name' field as DWC/<short-slug>" out)))))

(ert-deftest agent-repl-test-workspace-generation-prompt-slug-bare-when-prefix-unset ()
  "The slug instruction omits any prefix when CLAUDE_WORKSPACE_PREFIX is
unset, instructing a bare `<short-slug>' with no leading slash."
  (cl-letf (((symbol-function 'getenv) (lambda (_) nil)))
    (let ((out (agent-repl--workspace-generation-prompt
                "raw" "prefixed" "/tmp/repo/" "HEAD" nil)))
      (should (string-match-p "Generate the `name' field as <short-slug>" out))
      (should-not (string-match-p "<short-slug> prefix" out)))))

;;;; ---- Tests: workspace-commands JSON normalization ----

(ert-deftest agent-repl-test-normalize-workspace-commands-vector-becomes-list ()
  "A JSON array (parsed as vector) is normalized to a list."
  (let* ((parsed (vector '((type . "create") (name . "a"))
                         '((type . "create") (name . "b"))))
         (out (agent-repl--normalize-workspace-commands parsed)))
    (should (listp out))
    (should (equal (length out) 2))
    (should (equal (alist-get 'name (car out)) "a"))
    (should (equal (alist-get 'name (cadr out)) "b"))))

(ert-deftest agent-repl-test-normalize-workspace-commands-bare-object-wrapped ()
  "A bare JSON object (parsed as alist) is wrapped into a one-element list.
Without this, `dolist' iterates the alist's cons cells and dispatch
crashes with `Wrong type argument: listp, (type . \"create\")'."
  (let* ((parsed '((type . "create") (name . "solo") (git_root . "/g")))
         (out (agent-repl--normalize-workspace-commands parsed)))
    (should (equal (length out) 1))
    (should (equal (alist-get 'name (car out)) "solo"))
    (should (equal (alist-get 'type (car out)) "create"))))

(ert-deftest agent-repl-test-normalize-workspace-commands-empty-vector-empty-list ()
  "An empty JSON array produces an empty list (no commands to dispatch)."
  (should (equal (agent-repl--normalize-workspace-commands (vector)) nil)))

(ert-deftest agent-repl-test-normalize-workspace-commands-nil-empty-list ()
  "Malformed/nil input produces an empty list — caller skips dispatch."
  (should (equal (agent-repl--normalize-workspace-commands nil) nil)))

(ert-deftest agent-repl-test-normalize-workspace-commands-scalar-empty-list ()
  "A scalar JSON value (string/number) produces an empty list."
  (should (equal (agent-repl--normalize-workspace-commands "oops") nil))
  (should (equal (agent-repl--normalize-workspace-commands 42) nil)))

(ert-deftest agent-repl-test-spawn-workspace-generation-appends-extra-args ()
  "The spawn command list includes `agent-repl-workspace-generation-extra-args'
after the base `-p --model X' args.  Without these, the headless model
hits the permission prompt and dies emitting only its question."
  (let ((captured-cmd nil)
        (agent-repl-workspace-generation-extra-args
         '("--permission-mode" "bypassPermissions")))
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest plist)
                 (setq captured-cmd (plist-get plist :command))
                 ;; Return a dummy proc-like object — we only care about
                 ;; the command list captured above.  The lambda treats
                 ;; nil as a failure, so do nothing more.
                 (make-marker)))
              ((symbol-function 'process-send-string) (lambda (&rest _) nil))
              ((symbol-function 'process-send-eof) (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--log) (lambda (&rest _) nil)))
      (agent-repl--spawn-workspace-generation "raw" "prefixed" "/tmp/repo/" "HEAD" nil)
      (should (member "--permission-mode" captured-cmd))
      (should (member "bypassPermissions" captured-cmd))
      ;; Sanity: extra-args come after the default backend's base
      ;; `<binary> -p --model MODEL' prefix.
      (should (equal (cl-subseq captured-cmd 0 4)
                     (list (agent-repl-backend-binary (agent-repl--default-backend))
                           "-p" "--model" agent-repl-workspace-generation-model))))))

(ert-deftest agent-repl-test-spawn-workspace-generation-empty-extra-args ()
  "When `agent-repl-workspace-generation-extra-args' is nil, no extra args appear."
  (let ((captured-cmd nil)
        (agent-repl-workspace-generation-extra-args nil))
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest plist)
                 (setq captured-cmd (plist-get plist :command))
                 (make-marker)))
              ((symbol-function 'process-send-string) (lambda (&rest _) nil))
              ((symbol-function 'process-send-eof) (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--log) (lambda (&rest _) nil)))
      (agent-repl--spawn-workspace-generation "raw" "prefixed" "/tmp/repo/" "HEAD" nil)
      (should (equal captured-cmd
                     (list (agent-repl-backend-binary (agent-repl--default-backend))
                           "-p" "--model" agent-repl-workspace-generation-model))))))

(ert-deftest agent-repl-test-spawn-workspace-generation-binds-temporary-default-directory ()
  "Spawn must invoke `make-process' with `default-directory' rebound to
`temporary-file-directory'.  Without this, the headless claude inherits
the caller's cwd, its hooks fire with that cwd, and the sentinel watcher
misattributes them to whichever workspace owns that project-dir — flipping
:agent-state to :done."
  (let ((captured-cwd nil))
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest _plist)
                 (setq captured-cwd default-directory)
                 (make-marker)))
              ((symbol-function 'process-send-string) (lambda (&rest _) nil))
              ((symbol-function 'process-send-eof) (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--log) (lambda (&rest _) nil)))
      (agent-repl--spawn-workspace-generation "raw" "prefixed" "/tmp/repo/" "HEAD" nil)
      (should (equal (file-name-as-directory captured-cwd)
                     (file-name-as-directory temporary-file-directory))))))

;;;; ---- Tests: workspace-generation logging helpers ----

(ert-deftest agent-repl-test-workspace-generation-id-returns-non-empty-hex ()
  "The correlation-ID generator returns a non-empty hex string."
  (let ((id (agent-repl--workspace-generation-id)))
    (should (stringp id))
    (should (> (length id) 0))
    (should (string-match-p "\\`[0-9a-f]+\\'" id))))

(ert-deftest agent-repl-test-workspace-generation-truncate-leaves-short-strings ()
  "Strings within the cap are returned unchanged."
  (should (equal (agent-repl--workspace-generation-truncate "hello" 100) "hello")))

(ert-deftest agent-repl-test-workspace-generation-truncate-truncates-long-strings ()
  "Strings beyond the cap get a `...[truncated]' suffix."
  (let ((out (agent-repl--workspace-generation-truncate "0123456789" 4)))
    (should (string-prefix-p "0123" out))
    (should (string-suffix-p "...[truncated]" out))))

(ert-deftest agent-repl-test-workspace-generation-truncate-nil-cap-passes-through ()
  "A nil cap disables truncation entirely."
  (let ((s (make-string 10000 ?x)))
    (should (equal (agent-repl--workspace-generation-truncate s nil) s))))

(ert-deftest agent-repl-test-workspace-generation-truncate-nil-input-yields-empty ()
  "A nil input is treated as the empty string."
  (should (equal (agent-repl--workspace-generation-truncate nil 100) "")))

(ert-deftest agent-repl-test-workspace-generation-finalize-logs-correlation-id ()
  "Finalize includes the correlation ID in its log line so spawns can be matched."
  (let ((logged nil))
    (cl-letf (((symbol-function 'agent-repl--log)
               (lambda (_ws fmt &rest args)
                 (setq logged (apply #'format fmt args))))
              ((symbol-function 'message) (lambda (&rest _) nil)))
      (agent-repl--workspace-generation-finalize "abc123" 0 "finished\n" "ok")
      (should (string-match-p "\\[abc123\\]" logged)))))

(ert-deftest agent-repl-test-workspace-generation-finalize-logs-stdout-snippet ()
  "Finalize includes the stdout content (truncated) in the log line — so
failed spawns can be debugged without the buffer."
  (let ((logged nil))
    (cl-letf (((symbol-function 'agent-repl--log)
               (lambda (_ws fmt &rest args)
                 (setq logged (apply #'format fmt args))))
              ((symbol-function 'message) (lambda (&rest _) nil)))
      (agent-repl--workspace-generation-finalize "id" 1 "exit" "model-error-text")
      (should (string-match-p "model-error-text" logged)))))

(ert-deftest agent-repl-test-workspace-generation-finalize-truncates-long-stdout ()
  "Finalize honors the stdout cap so the log line stays bounded."
  (let ((logged nil)
        (agent-repl-workspace-generation-stdout-log-cap 8))
    (cl-letf (((symbol-function 'agent-repl--log)
               (lambda (_ws fmt &rest args)
                 (setq logged (apply #'format fmt args))))
              ((symbol-function 'message) (lambda (&rest _) nil)))
      (agent-repl--workspace-generation-finalize "id" 1 "exit" "0123456789abcdef")
      (should (string-match-p "0123" logged))
      (should (string-match-p "truncated" logged))
      (should-not (string-match-p "abcdef" logged)))))

(ert-deftest agent-repl-test-workspace-generation-finalize-no-message-on-success ()
  "On status=0, finalize does not surface a user-facing failure message."
  (let ((messaged nil))
    (cl-letf (((symbol-function 'agent-repl--log) (lambda (&rest _) nil))
              ((symbol-function 'message)
               (lambda (&rest args) (setq messaged args))))
      (agent-repl--workspace-generation-finalize "id" 0 "finished" "ok")
      (should-not messaged))))

(ert-deftest agent-repl-test-workspace-generation-finalize-message-includes-id ()
  "On non-zero status, the user-facing message includes the correlation ID
so the user can grep the log for the matching spawn."
  (let ((messaged nil))
    (cl-letf (((symbol-function 'agent-repl--log) (lambda (&rest _) nil))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq messaged (apply #'format fmt args)))))
      (agent-repl--workspace-generation-finalize "id-xyz" 2 "exit" "")
      (should (string-match-p "id-xyz" messaged)))))

(ert-deftest agent-repl-test-workspace-generation-finalize-non-numeric-status-is-failure ()
  "A non-numeric status (e.g. nil from a malformed signal) is treated as failure."
  (let ((messaged nil))
    (cl-letf (((symbol-function 'agent-repl--log) (lambda (&rest _) nil))
              ((symbol-function 'message)
               (lambda (&rest args) (setq messaged args))))
      (agent-repl--workspace-generation-finalize "id" nil "killed" "")
      (should messaged))))

;;;; ---- Tests: create-worktree-workspace (interactive) ----

(ert-deftest agent-repl-test-resolve-worktree-base-head ()
  "`head' resolves to HEAD."
  (should (equal (agent-repl--resolve-worktree-base 'head) "HEAD")))

(ert-deftest agent-repl-test-resolve-worktree-base-master ()
  "`master' resolves to local `master' (not `origin/master').
The fetch happens at worktree-creation time as a freshness gesture, but
the new branch is rooted in local master so local-only commits carry over."
  (should (equal (agent-repl--resolve-worktree-base 'master) "master")))

(ert-deftest agent-repl-test-resolve-worktree-base-unknown-errors ()
  "An unknown base symbol signals a `user-error' rather than silently
passing through."
  (should-error (agent-repl--resolve-worktree-base 'bogus)
                :type 'user-error))

(ert-deftest agent-repl-test-worktree-preemptive-prompt-head ()
  "BASE = `head' (`SPC TAB n') prompt names the current worktree."
  (should (equal (agent-repl--worktree-preemptive-prompt 'head)
                 "Preemptive prompt from current worktree (empty to name plain ws): ")))

(ert-deftest agent-repl-test-worktree-preemptive-prompt-master ()
  "BASE = `master' (`SPC TAB N') prompt names the main worktree."
  (should (equal (agent-repl--worktree-preemptive-prompt 'master)
                 "Preemptive prompt from main worktree (empty to name plain ws): ")))

(ert-deftest agent-repl-test-worktree-preemptive-prompt-differ ()
  "The `SPC TAB n' and `SPC TAB N' prompts are visibly distinct."
  (should-not (equal (agent-repl--worktree-preemptive-prompt 'head)
                     (agent-repl--worktree-preemptive-prompt 'master))))

(ert-deftest agent-repl-test-worktree-preemptive-prompt-unknown-errors ()
  "An unknown base signals an error rather than a mislabeled prompt."
  (should-error (agent-repl--worktree-preemptive-prompt 'bogus)))

(ert-deftest agent-repl-test-create-worktree-workspace-head-base ()
  "BASE = `head' branches off HEAD (the current worktree)."
  (agent-repl-test--with-clean-state
    (let ((captured-base nil))
      (cl-letf (((symbol-function 'agent-repl--resolve-current-git-root)
                 (lambda () "/tmp/repo/"))
                ((symbol-function 'read-string)
                 (lambda (&rest _) "do the thing"))
                ((symbol-function 'agent-repl--spawn-workspace-generation)
                 (lambda (_raw _prefixed _git-root base _fork-from)
                   (setq captured-base base))))
        (agent-repl-create-worktree-workspace 'head)
        (should (equal captured-base "HEAD"))))))

(ert-deftest agent-repl-test-create-worktree-workspace-master-base ()
  "BASE = `master' branches off LOCAL `master' (not `origin/master').
The downstream worktree-creation flow still fetches `origin master' first
as a freshness gesture, but the new branch is rooted in local master."
  (agent-repl-test--with-clean-state
    (let ((captured-base nil))
      (cl-letf (((symbol-function 'agent-repl--resolve-current-git-root)
                 (lambda () "/tmp/repo/"))
                ((symbol-function 'agent-repl--master-worktree-path)
                 (lambda (_root) nil))
                ((symbol-function 'read-string)
                 (lambda (&rest _) "do the thing"))
                ((symbol-function 'agent-repl--spawn-workspace-generation)
                 (lambda (_raw _prefixed _git-root base _fork-from)
                   (setq captured-base base))))
        (agent-repl-create-worktree-workspace 'master)
        (should (equal captured-base "master"))))))

(ert-deftest agent-repl-test-create-worktree-workspace-master-base-does-not-anchor-spawn-git-root ()
  "BASE = `master' passes the calling-ws git-root to spawn (no master anchoring).
Source-dir resolution now happens at receive time in
`agent-repl--create-worktree-from-command' based on BASE-COMMIT, so the
spawn-side no longer special-cases `master' for the git-root.  Keeping
git-root anchored to calling-ws means the JSON command file's `git_root'
reflects the user's actual context, with no master-worktree-path lookup
to fail."
  (agent-repl-test--with-clean-state
    (let ((captured-git-root :unset)
          (master-path-called nil))
      (cl-letf (((symbol-function '+workspace-current-name)
                 (lambda () "calling-ws"))
                ((symbol-function 'agent-repl--ws-dir)
                 (lambda (ws)
                   (if (equal ws "calling-ws") "/tmp/calling-ws/"
                     (error "unexpected ws: %s" ws))))
                ((symbol-function 'agent-repl--master-worktree-path)
                 (lambda (_root)
                   (setq master-path-called t)
                   "/tmp/master/"))
                ((symbol-function 'read-string)
                 (lambda (&rest _) "do the thing"))
                ((symbol-function 'agent-repl--spawn-workspace-generation)
                 (lambda (_raw _prefixed git-root _base _fork-from)
                   (setq captured-git-root git-root))))
        (agent-repl-create-worktree-workspace 'master)
        (should-not master-path-called)
        (should (equal captured-git-root "/tmp/calling-ws/"))))))

(ert-deftest agent-repl-test-create-worktree-workspace-head-base-passes-calling-ws-git-root ()
  "BASE = `head' passes calling-ws git-root and never consults master resolver."
  (agent-repl-test--with-clean-state
    (let ((captured-git-root :unset)
          (master-path-called nil))
      (cl-letf (((symbol-function '+workspace-current-name)
                 (lambda () "calling-ws"))
                ((symbol-function 'agent-repl--ws-dir)
                 (lambda (ws)
                   (if (equal ws "calling-ws") "/tmp/calling-ws/"
                     (error "unexpected ws: %s" ws))))
                ((symbol-function 'agent-repl--master-worktree-path)
                 (lambda (_root)
                   (setq master-path-called t)
                   "/tmp/master/"))
                ((symbol-function 'read-string)
                 (lambda (&rest _) "do the thing"))
                ((symbol-function 'agent-repl--spawn-workspace-generation)
                 (lambda (_raw _prefixed git-root _base _fork-from)
                   (setq captured-git-root git-root))))
        (agent-repl-create-worktree-workspace 'head)
        (should-not master-path-called)
        (should (equal captured-git-root "/tmp/calling-ws/"))))))

(ert-deftest agent-repl-test-create-worktree-workspace-passes-no-fork-from ()
  "Plain create (non-fork) passes FORK-FROM = nil."
  (agent-repl-test--with-clean-state
    (let ((captured-fork-from :unset))
      (cl-letf (((symbol-function 'agent-repl--resolve-current-git-root)
                 (lambda () "/tmp/repo/"))
                ((symbol-function 'read-string)
                 (lambda (&rest _) "do the thing"))
                ((symbol-function 'agent-repl--spawn-workspace-generation)
                 (lambda (_raw _prefixed _git-root _base fork-from)
                   (setq captured-fork-from fork-from))))
        (agent-repl-create-worktree-workspace 'head)
        (should (null captured-fork-from))))))

(ert-deftest agent-repl-test-create-worktree-workspace-from-origin-master-delegates-with-master-symbol ()
  "`SPC TAB N' wrapper delegates to the main command with BASE = `master'."
  (let ((captured-base :unset)
        (captured-source :unset))
    (cl-letf (((symbol-function 'agent-repl-create-worktree-workspace)
               (lambda (base &optional source-ws)
                 (setq captured-base base)
                 (setq captured-source source-ws))))
      (agent-repl-create-worktree-workspace-from-origin-master)
      (should (eq captured-base 'master))
      (should (null captured-source)))))

(ert-deftest agent-repl-test-create-worktree-workspace-from-origin-master-forwards-source-ws ()
  "`SPC TAB N' wrapper forwards SOURCE-WS to the main command."
  (let ((captured-source :unset))
    (cl-letf (((symbol-function 'agent-repl-create-worktree-workspace)
               (lambda (_base &optional source-ws)
                 (setq captured-source source-ws))))
      (agent-repl-create-worktree-workspace-from-origin-master "other-ws")
      (should (equal captured-source "other-ws")))))

(ert-deftest agent-repl-test-create-worktree-workspace-source-ws-passes-git-root ()
  "When SOURCE-WS is given, its :project-dir is threaded through as git-root."
  (agent-repl-test--with-clean-state
    (let ((captured-git-root :unset))
      (cl-letf (((symbol-function 'agent-repl--ws-dir)
                 (lambda (ws)
                   (if (equal ws "source-ws") "/tmp/source-repo/"
                     (error "unexpected ws: %s" ws))))
                ((symbol-function 'read-string)
                 (lambda (&rest _) "do the thing"))
                ((symbol-function 'agent-repl--spawn-workspace-generation)
                 (lambda (_raw _prefixed git-root _base _fork-from)
                   (setq captured-git-root git-root))))
        (agent-repl-create-worktree-workspace 'head "source-ws")
        (should (equal captured-git-root "/tmp/source-repo/"))))))

(ert-deftest agent-repl-test-create-worktree-workspace-no-source-ws-resolves-ambient-git-root ()
  "When SOURCE-WS is nil and current ws has no :project-dir, falls back to `resolve-current-git-root'."
  (agent-repl-test--with-clean-state
    (let ((captured-git-root :unset))
      (cl-letf (((symbol-function '+workspace-current-name)
                 (lambda () "ambient-ws"))
                ((symbol-function 'agent-repl--ws-dir)
                 (lambda (_ws) nil))
                ((symbol-function 'agent-repl--resolve-current-git-root)
                 (lambda () "/tmp/ambient-repo/"))
                ((symbol-function 'read-string)
                 (lambda (&rest _) "do the thing"))
                ((symbol-function 'agent-repl--spawn-workspace-generation)
                 (lambda (_raw _prefixed git-root _base _fork-from)
                   (setq captured-git-root git-root))))
        (agent-repl-create-worktree-workspace 'head nil)
        (should (equal captured-git-root "/tmp/ambient-repo/"))))))

(ert-deftest agent-repl-test-create-worktree-workspace-prefixes-preemptive-prompt ()
  "The preemptive prompt is prefixed with the autonomous instruction before being
handed to the spawn helper as PREFIXED-PROMPT."
  (agent-repl-test--with-clean-state
    (let ((captured-prefixed nil))
      (cl-letf (((symbol-function 'agent-repl--resolve-current-git-root)
                 (lambda () "/tmp/repo/"))
                ((symbol-function 'read-string)
                 (lambda (&rest _) "do the thing"))
                ((symbol-function 'agent-repl--spawn-workspace-generation)
                 (lambda (_raw prefixed _git-root _base _fork-from)
                   (setq captured-prefixed prefixed))))
        (agent-repl-create-worktree-workspace 'head)
        (should (string-prefix-p (agent-repl--meta-wrap agent-repl--autonomous-prompt-prefix)
                                 captured-prefixed))
        (should (string-suffix-p "do the thing" captured-prefixed))))))

(ert-deftest agent-repl-test-create-worktree-workspace-passes-raw-prompt-unprefixed ()
  "RAW-PROMPT given to the spawn helper is the user's original input,
unprefixed — the prefix is reserved for the new ws session, not for naming."
  (agent-repl-test--with-clean-state
    (let ((captured-raw nil))
      (cl-letf (((symbol-function 'agent-repl--resolve-current-git-root)
                 (lambda () "/tmp/repo/"))
                ((symbol-function 'read-string)
                 (lambda (&rest _) "do the thing"))
                ((symbol-function 'agent-repl--spawn-workspace-generation)
                 (lambda (raw _prefixed _git-root _base _fork-from)
                   (setq captured-raw raw))))
        (agent-repl-create-worktree-workspace 'head)
        (should (equal captured-raw "do the thing"))
        (should-not (string-prefix-p agent-repl--autonomous-prompt-prefix captured-raw))))))

(ert-deftest agent-repl-test-create-worktree-workspace-blank-prompt-prompts-for-name ()
  "An empty preemptive prompt prompts for the workspace name with a second
minibuffer read rather than erroring or spawning name generation."
  (agent-repl-test--with-clean-state
    (let ((prompts nil))
      (cl-letf (((symbol-function 'agent-repl--resolve-current-git-root)
                 (lambda () "/tmp/repo/"))
                ((symbol-function 'read-string)
                 (lambda (prompt &rest _)
                   (push prompt prompts)
                   (if (= (length prompts) 1) "" "my-ws")))
                ((symbol-function 'agent-repl--do-create-worktree-workspace)
                 (lambda (&rest _) nil))
                ((symbol-function 'agent-repl--spawn-workspace-generation)
                 (lambda (&rest _) (error "should not be called"))))
        (agent-repl-create-worktree-workspace 'head)
        (should (= (length prompts) 2))
        (should (string-match-p "Workspace name" (car prompts)))))))

(ert-deftest agent-repl-test-create-worktree-workspace-blank-prompt-creates-worktree-no-agent ()
  "An empty preemptive prompt creates the worktree via
`agent-repl--do-create-worktree-workspace' with NO-AGENT = t (the agent
not auto-booted) instead of spawning name generation."
  (agent-repl-test--with-clean-state
    (let ((captured-name nil)
          (captured-no-agent :unset))
      (cl-letf (((symbol-function 'agent-repl--resolve-current-git-root)
                 (lambda () "/tmp/repo/"))
                ((symbol-function 'read-string)
                 (let ((n 0))
                   (lambda (&rest _)
                     (setq n (1+ n))
                     (if (= n 1) "" "my-ws"))))
                ((symbol-function 'agent-repl--do-create-worktree-workspace)
                 (lambda (name &optional _fs _fork _prompt _cb _prio _base _root _src no-agent)
                   (setq captured-name name)
                   (setq captured-no-agent no-agent)))
                ((symbol-function 'agent-repl--spawn-workspace-generation)
                 (lambda (&rest _) (error "should not be called"))))
        (agent-repl-create-worktree-workspace 'head)
        (should (equal captured-name "my-ws"))
        (should (eq captured-no-agent t))))))

(ert-deftest agent-repl-test-create-worktree-workspace-blank-prompt-empty-name-errors ()
  "An empty preemptive prompt followed by an empty workspace name signals a
`user-error' rather than creating a worktree."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--resolve-current-git-root)
               (lambda () "/tmp/repo/"))
              ((symbol-function 'read-string)
               (lambda (&rest _) "   "))
              ((symbol-function 'agent-repl--do-create-worktree-workspace)
               (lambda (&rest _) (error "should not be called")))
              ((symbol-function 'agent-repl--spawn-workspace-generation)
               (lambda (&rest _) (error "should not be called"))))
      (should-error (agent-repl-create-worktree-workspace 'head)
                    :type 'user-error))))

(ert-deftest agent-repl-test-create-worktree-workspace-blank-prompt-passes-git-root ()
  "The empty-prompt named-worktree path roots the new worktree at the
resolved GIT-ROOT (so a source-ws selection is honored)."
  (agent-repl-test--with-clean-state
    (let ((captured-root :unset))
      (cl-letf (((symbol-function 'agent-repl--resolve-current-git-root)
                 (lambda () "/tmp/repo/"))
                ((symbol-function 'read-string)
                 (let ((n 0))
                   (lambda (&rest _)
                     (setq n (1+ n))
                     (if (= n 1) "" "my-ws"))))
                ((symbol-function 'agent-repl--do-create-worktree-workspace)
                 (lambda (_name &optional _fs _fork _prompt _cb _prio _base git-root &rest _)
                   (setq captured-root git-root)))
                ((symbol-function 'agent-repl--spawn-workspace-generation)
                 (lambda (&rest _) (error "should not be called"))))
        (agent-repl-create-worktree-workspace 'head)
        (should (equal captured-root "/tmp/repo/"))))))

(ert-deftest agent-repl-test-create-worktree-workspace-blank-prompt-head-source-dir-is-git-root ()
  "For BASE = `head' the empty-prompt path passes GIT-ROOT as the worktree's
source-dir (drawer parent), never the master worktree."
  (agent-repl-test--with-clean-state
    (let ((captured-src :unset)
          (master-called nil))
      (cl-letf (((symbol-function 'agent-repl--resolve-current-git-root)
                 (lambda () "/tmp/repo/"))
                ((symbol-function 'agent-repl--master-worktree-path)
                 (lambda (_root) (setq master-called t) "/tmp/master/"))
                ((symbol-function 'read-string)
                 (let ((n 0))
                   (lambda (&rest _)
                     (setq n (1+ n))
                     (if (= n 1) "" "my-ws"))))
                ((symbol-function 'agent-repl--do-create-worktree-workspace)
                 (lambda (_name &optional _fs _fork _prompt _cb _prio _base _root src &rest _)
                   (setq captured-src src)))
                ((symbol-function 'agent-repl--spawn-workspace-generation)
                 (lambda (&rest _) (error "should not be called"))))
        (agent-repl-create-worktree-workspace 'head)
        (should-not master-called)
        (should (equal captured-src "/tmp/repo/"))))))

(ert-deftest agent-repl-test-create-worktree-workspace-blank-prompt-master-source-dir-is-master-worktree ()
  "For BASE = `master' the empty-prompt path derives the worktree's source-dir
from the master worktree (mirroring `--create-worktree-from-command')."
  (agent-repl-test--with-clean-state
    (let ((captured-src :unset))
      (cl-letf (((symbol-function 'agent-repl--resolve-current-git-root)
                 (lambda () "/tmp/repo/"))
                ((symbol-function 'agent-repl--master-worktree-path)
                 (lambda (_root) "/tmp/master/"))
                ((symbol-function 'read-string)
                 (let ((n 0))
                   (lambda (&rest _)
                     (setq n (1+ n))
                     (if (= n 1) "" "my-ws"))))
                ((symbol-function 'agent-repl--do-create-worktree-workspace)
                 (lambda (_name &optional _fs _fork _prompt _cb _prio _base _root src &rest _)
                   (setq captured-src src)))
                ((symbol-function 'agent-repl--spawn-workspace-generation)
                 (lambda (&rest _) (error "should not be called"))))
        (agent-repl-create-worktree-workspace 'master)
        (should (equal captured-src "/tmp/master/"))))))

(ert-deftest agent-repl-test-create-worktree-workspace-blank-prompt-switches-focus ()
  "The empty-prompt path passes the switch callback so focus jumps to the
newly created worktree."
  (agent-repl-test--with-clean-state
    (let ((captured-cb :unset))
      (cl-letf (((symbol-function 'agent-repl--resolve-current-git-root)
                 (lambda () "/tmp/repo/"))
                ((symbol-function 'read-string)
                 (let ((n 0))
                   (lambda (&rest _)
                     (setq n (1+ n))
                     (if (= n 1) "" "my-ws"))))
                ((symbol-function 'agent-repl--do-create-worktree-workspace)
                 (lambda (_name &optional _fs _fork _prompt cb &rest _)
                   (setq captured-cb cb)))
                ((symbol-function 'agent-repl--spawn-workspace-generation)
                 (lambda (&rest _) (error "should not be called"))))
        (agent-repl-create-worktree-workspace 'head)
        (should (eq captured-cb #'agent-repl--worktree-creation-switch-callback))))))

(ert-deftest agent-repl-test-create-worktree-workspace-logs-entry-before-prompt-read ()
  "An ENTRY log line is emitted BEFORE `read-string' so a cancelled minibuffer
or empty prompt still leaves a trace that the keybinding fired."
  (agent-repl-test--with-clean-state
    (let ((logs nil)
          (read-string-called nil))
      (cl-letf (((symbol-function 'agent-repl--log)
                 (lambda (_ws fmt &rest args)
                   (push (apply #'format fmt args) logs)))
                ((symbol-function 'agent-repl--resolve-current-git-root)
                 (lambda () "/tmp/repo/"))
                ((symbol-function 'read-string)
                 (lambda (&rest _)
                   (setq read-string-called t)
                   (signal 'quit nil)))
                ((symbol-function 'agent-repl--spawn-workspace-generation)
                 (lambda (&rest _) (error "should not be called"))))
        (condition-case _err
            (agent-repl-create-worktree-workspace 'head)
          (quit nil))
        (should read-string-called)
        (should (cl-some (lambda (s)
                           (string-match-p "create-worktree-workspace: ENTRY" s))
                         logs))))))

(ert-deftest agent-repl-test-create-worktree-workspace-logs-named-worktree-on-empty-prompt ()
  "Empty preemptive prompt logs the named-worktree creation path, so the
no-prompt branch is visible in the log."
  (agent-repl-test--with-clean-state
    (let ((logs nil))
      (cl-letf (((symbol-function 'agent-repl--log)
                 (lambda (_ws fmt &rest args)
                   (push (apply #'format fmt args) logs)))
                ((symbol-function 'agent-repl--resolve-current-git-root)
                 (lambda () "/tmp/repo/"))
                ((symbol-function 'read-string)
                 (let ((n 0))
                   (lambda (&rest _)
                     (setq n (1+ n))
                     (if (= n 1) "" "my-ws"))))
                ((symbol-function 'agent-repl--do-create-worktree-workspace)
                 (lambda (&rest _) nil))
                ((symbol-function 'agent-repl--spawn-workspace-generation)
                 (lambda (&rest _) (error "should not be called"))))
        (agent-repl-create-worktree-workspace 'head)
        (should (cl-some (lambda (s)
                           (string-match-p "empty preemptive prompt, creating worktree" s))
                         logs))))))

(ert-deftest agent-repl-test-create-worktree-workspace-from-origin-master-logs-entry ()
  "`SPC TAB N' wrapper logs an ENTRY line before delegating, so the keybinding
firing is visible in the log even if the inner command bails."
  (let ((logs nil))
    (cl-letf (((symbol-function 'agent-repl--log)
               (lambda (_ws fmt &rest args)
                 (push (apply #'format fmt args) logs)))
              ((symbol-function 'agent-repl-create-worktree-workspace)
               (lambda (&rest _) nil)))
      (agent-repl-create-worktree-workspace-from-origin-master)
      (should (cl-some (lambda (s)
                         (string-match-p
                          "create-worktree-workspace-from-origin-master: ENTRY" s))
                       logs)))))

(ert-deftest agent-repl-test-fork-worktree-workspace-prefixes-preemptive-prompt ()
  "Fork's preemptive prompt is prefixed with the autonomous instruction."
  (agent-repl-test--with-clean-state
    (let ((inst (make-agent-repl-instantiation :session-id "sess-abc"))
          (captured-prefixed nil))
      (cl-letf (((symbol-function 'agent-repl--active-inst)
                 (lambda (_ws) inst))
                ((symbol-function '+workspace-current-name)
                 (lambda () "test-ws"))
                ((symbol-function 'agent-repl--resolve-current-git-root)
                 (lambda () "/tmp/repo/"))
                ((symbol-function 'read-string)
                 (lambda (&rest _) "do the thing"))
                ((symbol-function 'agent-repl--spawn-workspace-generation)
                 (lambda (_raw prefixed _git-root _base _fork-from)
                   (setq captured-prefixed prefixed))))
        (agent-repl-fork-worktree-workspace nil)
        (should (string-prefix-p (agent-repl--meta-wrap agent-repl--autonomous-prompt-prefix)
                                 captured-prefixed))
        (should (string-suffix-p "do the thing" captured-prefixed))))))

(ert-deftest agent-repl-test-fork-worktree-workspace-blank-prompt-errors ()
  "An empty preemptive prompt to fork signals user-error."
  (agent-repl-test--with-clean-state
    (let ((inst (make-agent-repl-instantiation :session-id "sess-abc")))
      (cl-letf (((symbol-function 'agent-repl--active-inst)
                 (lambda (_ws) inst))
                ((symbol-function '+workspace-current-name)
                 (lambda () "test-ws"))
                ((symbol-function 'agent-repl--resolve-current-git-root)
                 (lambda () "/tmp/repo/"))
                ((symbol-function 'read-string)
                 (lambda (&rest _) ""))
                ((symbol-function 'agent-repl--spawn-workspace-generation)
                 (lambda (&rest _) (error "should not be called"))))
        (should-error (agent-repl-fork-worktree-workspace nil)
                      :type 'user-error)))))

;;;; ---- Tests: do-create-worktree-workspace base-commit + fetch ----

(ert-deftest agent-repl-test-do-create-base-commit-default-no-fork-fetches-origin-master ()
  "When BASE-COMMIT is nil and no FORK-SESSION-ID, the default is local
`master' AND a `git fetch origin master' is still scheduled.
Preserves the programmatic worktree-creation path used by
`create-worktree-from-command' (Slack/command-file workspace creation):
local master keeps any local-only commits, while the fetch keeps
`origin/master' fresh as a side benefit."
  (let ((add-args nil))
    (cl-letf (((symbol-function 'agent-repl--resolve-current-git-root)
               (lambda () "/g/"))
              ((symbol-function 'agent-repl--resolve-worktree-paths)
               (lambda (_git-root _name) (list :git-root "/g" :dirname "d" :branch-name "b"
                                               :in-worktree nil :path "/g/d")))
              ((symbol-function 'agent-repl--validate-worktree-creation)
               (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--workspace-id) (lambda () "id"))
              ((symbol-function '+workspace-current-name) (lambda () "ws"))
              ((symbol-function 'agent-repl--async-git)
               (lambda (_label _root args _cb) (setq add-args args)))
              ((symbol-function 'agent-repl--async-worktree-add)
               (lambda (&rest _) nil)))
      (agent-repl--do-create-worktree-workspace "name" nil nil nil nil nil nil)
      ;; Fetch is still scheduled for origin master even though the branch
      ;; will be rooted in local master.
      (should (equal add-args '("fetch" "origin" "master"))))))

(ert-deftest agent-repl-test-do-create-local-master-base-passed-to-worktree-add ()
  "When BASE-COMMIT is the local trunk (e.g. \"master\"), the ref passed
to `git worktree add' is the LOCAL ref (\"master\"), not \"origin/master\".
The fetch still runs as a freshness gesture but does not change what the
new branch is rooted in."
  (let ((add-base nil))
    (cl-letf (((symbol-function 'agent-repl--resolve-current-git-root)
               (lambda () "/g/"))
              ((symbol-function 'agent-repl--resolve-worktree-paths)
               (lambda (_git-root _name) (list :git-root "/g" :dirname "d" :branch-name "b"
                                               :in-worktree nil :path "/g/d")))
              ((symbol-function 'agent-repl--validate-worktree-creation)
               (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--workspace-id) (lambda () "id"))
              ((symbol-function '+workspace-current-name) (lambda () "ws"))
              ;; Stub out the async-fetch so the callback fires immediately
              ;; and we can capture what got passed to worktree-add.
              ((symbol-function 'agent-repl--async-git)
               (lambda (_label _root _args cb) (funcall cb t "ok")))
              ;; The fetch callback fast-forwards local master; that path is
              ;; not under test here, so stub it out before it reaches git.
              ((symbol-function 'agent-repl--maybe-fast-forward-master)
               (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--async-worktree-add)
               (lambda (_root _branch _path base &rest _) (setq add-base base))))
      (agent-repl--do-create-worktree-workspace "name" nil nil nil nil nil "master")
      (should (equal add-base "master")))))

(ert-deftest agent-repl-test-do-create-local-master-base-fetches-origin-counterpart ()
  "When BASE-COMMIT equals `agent-repl-master-branch-name', a fetch of the
corresponding origin ref is scheduled even though the base is local.
This is the freshness-gesture path: the user picked local master as the
branching point, so the worktree-add proper uses local master, but
`origin/master' is updated alongside since fetching costs ~nothing."
  (let ((fetch-args nil))
    (cl-letf (((symbol-function 'agent-repl--resolve-current-git-root)
               (lambda () "/g/"))
              ((symbol-function 'agent-repl--resolve-worktree-paths)
               (lambda (_git-root _name) (list :git-root "/g" :dirname "d" :branch-name "b"
                                               :in-worktree nil :path "/g/d")))
              ((symbol-function 'agent-repl--validate-worktree-creation)
               (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--workspace-id) (lambda () "id"))
              ((symbol-function '+workspace-current-name) (lambda () "ws"))
              ((symbol-function 'agent-repl--async-git)
               (lambda (_label _root args _cb) (setq fetch-args args)))
              ((symbol-function 'agent-repl--async-worktree-add)
               (lambda (&rest _) nil)))
      (agent-repl--do-create-worktree-workspace "name" nil nil nil nil nil "master")
      (should (equal fetch-args '("fetch" "origin" "master"))))))

(ert-deftest agent-repl-test-do-create-base-commit-default-with-fork-is-head ()
  "When BASE-COMMIT is nil and FORK-SESSION-ID is set, the default is HEAD.
Fork workflows need the session's tip; fetching origin/master would
reset that context."
  (let ((add-base nil))
    (cl-letf (((symbol-function 'agent-repl--resolve-current-git-root)
               (lambda () "/g/"))
              ((symbol-function 'agent-repl--resolve-worktree-paths)
               (lambda (_git-root _name) (list :git-root "/g" :dirname "d" :branch-name "b"
                                               :in-worktree nil :path "/g/d")))
              ((symbol-function 'agent-repl--validate-worktree-creation)
               (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--workspace-id) (lambda () "id"))
              ((symbol-function '+workspace-current-name) (lambda () "ws"))
              ((symbol-function 'agent-repl--async-worktree-add)
               (lambda (_root _branch _path base &rest _) (setq add-base base))))
      (agent-repl--do-create-worktree-workspace "name" nil "sid-1" nil nil nil nil)
      (should (equal add-base "HEAD")))))

(ert-deftest agent-repl-test-do-create-base-commit-explicit-wins ()
  "Explicit BASE-COMMIT overrides the fork-derived default.
This is the path `agent-repl-create-worktree-workspace' uses to
force HEAD even without a fork-session-id."
  (let ((add-base nil))
    (cl-letf (((symbol-function 'agent-repl--resolve-current-git-root)
               (lambda () "/g/"))
              ((symbol-function 'agent-repl--resolve-worktree-paths)
               (lambda (_git-root _name) (list :git-root "/g" :dirname "d" :branch-name "b"
                                               :in-worktree nil :path "/g/d")))
              ((symbol-function 'agent-repl--validate-worktree-creation)
               (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--workspace-id) (lambda () "id"))
              ((symbol-function '+workspace-current-name) (lambda () "ws"))
              ((symbol-function 'agent-repl--async-worktree-add)
               (lambda (_root _branch _path base &rest _) (setq add-base base))))
      (agent-repl--do-create-worktree-workspace "name" nil nil nil nil nil "HEAD")
      (should (equal add-base "HEAD")))))

(ert-deftest agent-repl-test-do-create-skips-fetch-when-base-is-head ()
  "No fetch runs when BASE-COMMIT is HEAD — nothing to pull from origin."
  (let ((fetch-called nil)
        (add-called nil))
    (cl-letf (((symbol-function 'agent-repl--resolve-current-git-root)
               (lambda () "/g/"))
              ((symbol-function 'agent-repl--resolve-worktree-paths)
               (lambda (_git-root _name) (list :git-root "/g" :dirname "d" :branch-name "b"
                                               :in-worktree nil :path "/g/d")))
              ((symbol-function 'agent-repl--validate-worktree-creation)
               (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--workspace-id) (lambda () "id"))
              ((symbol-function '+workspace-current-name) (lambda () "ws"))
              ((symbol-function 'agent-repl--async-git)
               (lambda (&rest _) (setq fetch-called t)))
              ((symbol-function 'agent-repl--async-worktree-add)
               (lambda (&rest _) (setq add-called t))))
      (agent-repl--do-create-worktree-workspace "name" nil nil nil nil nil "HEAD")
      (should-not fetch-called)
      (should add-called))))

(ert-deftest agent-repl-test-do-create-fetch-ref-parsed-from-base ()
  "Fetch uses the ref name parsed from BASE-COMMIT after the origin/ prefix.
Supports bases other than origin/master without hard-coding the ref."
  (let ((fetch-args nil))
    (cl-letf (((symbol-function 'agent-repl--resolve-current-git-root)
               (lambda () "/g/"))
              ((symbol-function 'agent-repl--resolve-worktree-paths)
               (lambda (_git-root _name) (list :git-root "/g" :dirname "d" :branch-name "b"
                                               :in-worktree nil :path "/g/d")))
              ((symbol-function 'agent-repl--validate-worktree-creation)
               (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--workspace-id) (lambda () "id"))
              ((symbol-function '+workspace-current-name) (lambda () "ws"))
              ((symbol-function 'agent-repl--async-git)
               (lambda (_label _root args _cb) (setq fetch-args args))))
      (agent-repl--do-create-worktree-workspace
       "name" nil nil nil nil nil "origin/develop")
      (should (equal fetch-args '("fetch" "origin" "develop"))))))

(ert-deftest agent-repl-test-do-create-fork-skips-fetch-regardless-of-base ()
  "FORK-SESSION-ID always skips fetch (agent session-restore flow).
Even if someone passed an origin/ base-commit by mistake, the fork
path short-circuits to avoid disturbing the fork source's refs."
  (let ((fetch-called nil))
    (cl-letf (((symbol-function 'agent-repl--resolve-current-git-root)
               (lambda () "/g/"))
              ((symbol-function 'agent-repl--resolve-worktree-paths)
               (lambda (_git-root _name) (list :git-root "/g" :dirname "d" :branch-name "b"
                                               :in-worktree nil :path "/g/d")))
              ((symbol-function 'agent-repl--validate-worktree-creation)
               (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--workspace-id) (lambda () "id"))
              ((symbol-function '+workspace-current-name) (lambda () "ws"))
              ((symbol-function 'agent-repl--async-git)
               (lambda (&rest _) (setq fetch-called t)))
              ((symbol-function 'agent-repl--async-worktree-add)
               (lambda (&rest _) nil)))
      (agent-repl--do-create-worktree-workspace
       "name" nil "sid-1" nil nil nil "origin/master")
      (should-not fetch-called))))

(ert-deftest agent-repl-test-do-create-uses-explicit-git-root-and-skips-resolver ()
  "When GIT-ROOT is passed explicitly, `--resolve-current-git-root' is NOT called.
This matters for the commands-file flow, which captures git-root at
enqueue and must not have it re-resolved at timer-fire time."
  (let ((resolver-called nil)
        (resolve-paths-root nil))
    (cl-letf (((symbol-function 'agent-repl--resolve-current-git-root)
               (lambda () (setq resolver-called t) "/SHOULD-NOT-BE-USED/"))
              ((symbol-function 'agent-repl--resolve-worktree-paths)
               (lambda (git-root _name)
                 (setq resolve-paths-root git-root)
                 (list :git-root git-root :dirname "d" :branch-name "b"
                       :in-worktree nil :path "/g/d")))
              ((symbol-function 'agent-repl--validate-worktree-creation)
               (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--workspace-id) (lambda () "id"))
              ((symbol-function '+workspace-current-name) (lambda () "ws"))
              ((symbol-function 'agent-repl--async-worktree-add)
               (lambda (&rest _) nil)))
      (agent-repl--do-create-worktree-workspace
       "name" nil nil nil nil nil "HEAD" "/explicit/root/")
      (should-not resolver-called)
      (should (equal resolve-paths-root "/explicit/root/")))))

(ert-deftest agent-repl-test-do-create-resolves-git-root-when-omitted ()
  "When GIT-ROOT is nil, `--resolve-current-git-root' is called exactly once
and its result is threaded into `--resolve-worktree-paths'."
  (let ((resolver-calls 0)
        (resolve-paths-root nil))
    (cl-letf (((symbol-function 'agent-repl--resolve-current-git-root)
               (lambda () (cl-incf resolver-calls) "/resolved/root/"))
              ((symbol-function 'agent-repl--resolve-worktree-paths)
               (lambda (git-root _name)
                 (setq resolve-paths-root git-root)
                 (list :git-root git-root :dirname "d" :branch-name "b"
                       :in-worktree nil :path "/g/d")))
              ((symbol-function 'agent-repl--validate-worktree-creation)
               (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--workspace-id) (lambda () "id"))
              ((symbol-function '+workspace-current-name) (lambda () "ws"))
              ((symbol-function 'agent-repl--async-worktree-add)
               (lambda (&rest _) nil)))
      (agent-repl--do-create-worktree-workspace
       "name" nil nil nil nil nil "HEAD")
      (should (= resolver-calls 1))
      (should (equal resolve-paths-root "/resolved/root/")))))

(ert-deftest agent-repl-test-create-worktree-from-command-forwards-git-root ()
  "`--create-worktree-from-command' forwards GIT-ROOT as the 8th arg to
`--do-create-worktree-workspace', preserving the value captured at enqueue."
  (let ((forwarded-args nil))
    (cl-letf (((symbol-function 'agent-repl--do-create-worktree-workspace)
               (lambda (&rest args) (setq forwarded-args args))))
      (agent-repl--create-worktree-from-command
       "/captured/root/" "ws-name" "some prompt" :high "fork-sid")
      ;; args: (name force-bare fork-sid prompt cb priority base-commit git-root)
      (should (equal (nth 0 forwarded-args) "ws-name"))
      (should (equal (nth 2 forwarded-args) "fork-sid"))
      (should (equal (nth 3 forwarded-args) "some prompt"))
      (should (equal (nth 5 forwarded-args) :high))
      (should (equal (nth 7 forwarded-args) "/captured/root/")))))

;;;; ---- Tests: async-worktree-add base-commit ----

(ert-deftest agent-repl-test-async-worktree-add-uses-base-commit ()
  "async-worktree-add passes BASE-COMMIT as the final `git worktree add' arg.
Covers the full call the interactive `SPC TAB n' path builds up."
  (let ((captured-args nil))
    (cl-letf (((symbol-function 'agent-repl--async-git)
               (lambda (_label _root args _cb) (setq captured-args args))))
      (agent-repl--async-worktree-add
       "/git-root" "my-branch" "/path" "HEAD"
       nil "dirname" nil nil nil nil)
      (should (equal captured-args
                     '("worktree" "add" "-b" "my-branch" "/path" "HEAD"))))))

;;;; ---- Tests: start-tag-name ----

(ert-deftest agent-repl-test-start-tag-name-default-prefix ()
  "Default prefix `start/' produces start/<branch>."
  (let ((agent-repl-worktree-start-tag-prefix "start/"))
    (should (equal (agent-repl--start-tag-name "DC/feature") "start/DC/feature"))))

(ert-deftest agent-repl-test-start-tag-name-nil-prefix ()
  "Nil prefix means start tags are disabled — returns nil."
  (let ((agent-repl-worktree-start-tag-prefix nil))
    (should (null (agent-repl--start-tag-name "any")))))

(ert-deftest agent-repl-test-start-tag-name-empty-prefix ()
  "Empty prefix is treated as disabled — returns nil."
  (let ((agent-repl-worktree-start-tag-prefix ""))
    (should (null (agent-repl--start-tag-name "any")))))

;;;; ---- Tests: create-start-tag ----

(ert-deftest agent-repl-test-create-start-tag-creates-at-base-commit ()
  "create-start-tag invokes `git tag <prefix><branch> <base-commit>' in GIT-ROOT."
  (let ((sha "abc123def456abc123def456abc123def4567890")
        (agent-repl-worktree-start-tag-prefix "start/")
        (captured-call nil))
    (cl-letf (((symbol-function 'agent-repl--git-exit-code)
               (lambda (root &rest args)
                 (setq captured-call (cons root args))
                 0)))
      (agent-repl--create-start-tag "/tmp/repo" "feature" sha)
      (should (equal captured-call
                     (list "/tmp/repo" "tag" "start/feature" sha))))))

(ert-deftest agent-repl-test-create-start-tag-disabled-no-op ()
  "When prefix is nil, no tag is created (no git call)."
  (let ((agent-repl-worktree-start-tag-prefix nil)
        (git-called nil))
    (cl-letf (((symbol-function 'agent-repl--git-exit-code)
               (lambda (&rest _args) (setq git-called t) 0)))
      (agent-repl--create-start-tag "/tmp/repo" "feature" "HEAD")
      (should-not git-called))))

(ert-deftest agent-repl-test-create-start-tag-signals-on-failure ()
  "create-start-tag signals an error when git tag fails (non-zero exit)."
  (let ((agent-repl-worktree-start-tag-prefix "start/"))
    (cl-letf (((symbol-function 'agent-repl--git-exit-code)
               (lambda (&rest _args) 128)))
      (should-error (agent-repl--create-start-tag "/tmp/repo" "feature" "HEAD")))))

;;;; ---- Tests: async-worktree-add start-tag integration ----

(ert-deftest agent-repl-test-async-worktree-add-creates-start-tag-on-success ()
  "After successful worktree add, the start tag is created."
  (let ((captured-tag-args nil))
    (cl-letf (((symbol-function 'agent-repl--async-git)
               ;; Simulate immediate success: invoke callback with ok=t.
               (lambda (_label _root _args cb) (funcall cb t "ok")))
              ((symbol-function 'agent-repl--worktree-add-callback)
               (lambda (&rest _args) nil))
              ((symbol-function 'agent-repl--create-start-tag)
               (lambda (git-root branch-name base-commit)
                 (setq captured-tag-args (list git-root branch-name base-commit)))))
      (agent-repl--async-worktree-add
       "/git-root" "my-branch" "/path" "HEAD"
       nil "dirname" nil nil nil nil)
      (should (equal captured-tag-args '("/git-root" "my-branch" "HEAD"))))))

(ert-deftest agent-repl-test-async-worktree-add-skips-start-tag-on-failure ()
  "On worktree add failure, the start tag is NOT created."
  (let ((tag-called nil))
    (cl-letf (((symbol-function 'agent-repl--async-git)
               ;; Simulate failure: invoke callback with ok=nil.
               (lambda (_label _root _args cb) (funcall cb nil "git error")))
              ((symbol-function 'agent-repl--worktree-add-callback)
               (lambda (&rest _args) nil))
              ((symbol-function 'agent-repl--create-start-tag)
               (lambda (&rest _args) (setq tag-called t))))
      (agent-repl--async-worktree-add
       "/git-root" "my-branch" "/path" "HEAD"
       nil "dirname" nil nil nil nil)
      (should-not tag-called))))

;;;; ---- Tests: workspace-merge default selection ----

(ert-deftest agent-repl-test-workspace-merge-defaults-to-last-visited-claude-ws ()
  "workspace-merge pre-selects the most recently visited agent workspace."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "current" :project-dir "/tmp/cur")
    (agent-repl--ws-put "ws-a" :project-dir "/tmp/a")
    (agent-repl--ws-put "ws-b" :project-dir "/tmp/b")
    (let ((agent-repl--workspace-history '("ws-b" "ws-a" "current"))
          (captured-default nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "current"))
                ((symbol-function '+workspace-list-names)
                 (lambda () '("current" "ws-a" "ws-b")))
                ((symbol-function 'agent-repl--assert-clean-worktree) #'ignore)
                ((symbol-function 'completing-read)
                 (lambda (_prompt _coll &optional _pred _req _init _hist default &rest _)
                   (setq captured-default default)
                   "ws-a"))
                ((symbol-function 'agent-repl--workspace-merge-do) #'ignore))
        (agent-repl-workspace-merge)
        (should (equal captured-default "ws-b"))))))

(ert-deftest agent-repl-test-workspace-merge-skips-non-agent-ws ()
  "workspace-merge skips workspaces not registered in agent-repl--workspaces."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "current" :project-dir "/tmp/cur")
    ;; Only ws-b is an agent workspace; ws-a is a plain workspace.
    (agent-repl--ws-put "ws-b" :project-dir "/tmp/b")
    (let ((agent-repl--workspace-history '("ws-a" "ws-b" "current"))
          (captured-default nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "current"))
                ((symbol-function '+workspace-list-names)
                 (lambda () '("current" "ws-a" "ws-b")))
                ((symbol-function 'agent-repl--assert-clean-worktree) #'ignore)
                ((symbol-function 'completing-read)
                 (lambda (_prompt _coll &optional _pred _req _init _hist default &rest _)
                   (setq captured-default default)
                   "ws-b"))
                ((symbol-function 'agent-repl--workspace-merge-do) #'ignore))
        (agent-repl-workspace-merge)
        (should (equal captured-default "ws-b"))))))

(ert-deftest agent-repl-test-workspace-merge-no-default-when-history-empty ()
  "workspace-merge passes nil default when no history matches."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "current" :project-dir "/tmp/cur")
    (agent-repl--ws-put "ws-a" :project-dir "/tmp/a")
    (let ((agent-repl--workspace-history nil)
          (captured-default nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "current"))
                ((symbol-function '+workspace-list-names)
                 (lambda () '("current" "ws-a")))
                ((symbol-function 'agent-repl--assert-clean-worktree) #'ignore)
                ((symbol-function 'completing-read)
                 (lambda (_prompt _coll &optional _pred _req _init _hist default &rest _)
                   (setq captured-default default)
                   "ws-a"))
                ((symbol-function 'agent-repl--workspace-merge-do) #'ignore))
        (agent-repl-workspace-merge)
        (should (null captured-default))))))

(ert-deftest agent-repl-test-workspace-merge-skips-current-ws-in-history ()
  "workspace-merge does not default to the current workspace even if most recent."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "current" :project-dir "/tmp/cur")
    (agent-repl--ws-put "ws-a" :project-dir "/tmp/a")
    (let ((agent-repl--workspace-history '("current" "ws-a"))
          (captured-default nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "current"))
                ((symbol-function '+workspace-list-names)
                 (lambda () '("current" "ws-a")))
                ((symbol-function 'agent-repl--assert-clean-worktree) #'ignore)
                ((symbol-function 'completing-read)
                 (lambda (_prompt _coll &optional _pred _req _init _hist default &rest _)
                   (setq captured-default default)
                   "ws-a"))
                ((symbol-function 'agent-repl--workspace-merge-do) #'ignore))
        (agent-repl-workspace-merge)
        ;; current is removed from other-ws, so ws-a should be the default
        (should (equal captured-default "ws-a"))))))

;;;; ---- Tests: workspace-merge-do reloads config ----

(ert-deftest agent-repl-test-workspace-merge-do-reloads-config ()
  "workspace-merge-do calls load-file on agent-repl--config-file after cherry-picking."
  (let ((loaded-file nil))
    (cl-letf* (((symbol-function '+workspace-current-name) (lambda () "current"))
               ((symbol-function 'agent-repl--workspace-branch) (lambda (_ws) "branch-x"))
               ((symbol-function 'agent-repl--ws-dir) (lambda (_ws) "/tmp/fake"))
               ((symbol-function 'agent-repl--git-branch-exists-p) (lambda (_dir _br) t))
               ((symbol-function 'agent-repl--cherry-pick-base) (lambda (_dir _br) "abc123"))
               ((symbol-function 'agent-repl--cherry-pick-commits) (lambda (_dir _ws _base _br &optional _auto _silent) nil))
               ((symbol-function 'agent-repl--tag-merge-completion) #'ignore)
               ((symbol-function 'agent-repl-drawer--refresh-detail-cache) #'ignore)
               ((symbol-function 'agent-repl--nuke-one-workspace) (lambda (&rest _) nil))
               ((symbol-function 'load-file) (lambda (f) (setq loaded-file f))))
      (agent-repl--workspace-merge-do "other-ws")
      (should (equal loaded-file agent-repl--config-file)))))

(ert-deftest agent-repl-test-workspace-merge-do-never-pops-magit-status ()
  "`--workspace-merge-do' must NEVER call `magit-status' to POP a new
magit-status buffer — post-merge buffer presentation (window selection,
buffer creation) is purely the caller's (`--workspace-merge-into-source')
workspace-switch responsibility.  Exercises both the SILENT and
non-SILENT call shapes to assert this is unconditional, not gated on
SILENT.

Note: a magit-refresh of any already-open status buffer for the merge
target IS called at the end of the merge (see
`agent-repl-test-workspace-merge-do-refreshes-target-magit-after-close')
— this test only asserts that no NEW buffer is created/popped."
  (agent-repl-test--with-clean-state
    (puthash "other-ws" '() agent-repl--workspaces)
    (let ((magit-status-called nil))
      (cl-letf* (((symbol-function '+workspace-current-name) (lambda () "current"))
                 ((symbol-function 'agent-repl--workspace-branch) (lambda (_ws) "branch-x"))
                 ((symbol-function 'agent-repl--ws-dir) (lambda (_ws) "/tmp/fake"))
                 ((symbol-function 'agent-repl--git-branch-exists-p) (lambda (_dir _br) t))
                 ((symbol-function 'agent-repl--cherry-pick-base) (lambda (_dir _br) "abc123"))
                 ((symbol-function 'agent-repl--cherry-pick-commits) (lambda (_dir _ws _base _br &optional _auto _silent) nil))
                 ((symbol-function 'agent-repl--tag-merge-completion) #'ignore)
                 ((symbol-function 'agent-repl--close-workspace) #'ignore)
                 ((symbol-function 'agent-repl-drawer--refresh-detail-cache) #'ignore)
                 ((symbol-function 'agent-repl--refresh-magit-status-for-dir) #'ignore)
                 ((symbol-function 'load-file) #'ignore)
                 ((symbol-function 'magit-status) (lambda (&rest _) (setq magit-status-called t))))
        ;; Non-silent path.
        (agent-repl--workspace-merge-do "other-ws")
        (should-not magit-status-called)
        ;; Silent path.
        (agent-repl--workspace-merge-do "other-ws" "/tmp/fake" t)
        (should-not magit-status-called)))))

;;;; ---- Tests: tag-merge-completion ----

(ert-deftest agent-repl-test-tag-merge-completion-creates-correct-tag ()
  "tag-merge-completion runs `git tag -f merge/<source-ws> HEAD' in
the project root."
  (let ((captured-args nil))
    (cl-letf (((symbol-function 'agent-repl--git-exit-code)
               (lambda (root &rest args)
                 (setq captured-args (cons root args))
                 0)))
      (agent-repl--tag-merge-completion "/tmp/repo" "feat-foo")
      (should (equal captured-args
                     (list "/tmp/repo" "tag" "-f" "merge/feat-foo" "HEAD"))))))

(ert-deftest agent-repl-test-tag-merge-completion-uses-force ()
  "tag-merge-completion passes `-f' so re-running the merge for the
same workspace updates an existing tag rather than erroring."
  (let ((captured-args nil))
    (cl-letf (((symbol-function 'agent-repl--git-exit-code)
               (lambda (_root &rest args) (setq captured-args args) 0)))
      (agent-repl--tag-merge-completion "/tmp/repo" "feat-foo")
      (should (member "-f" captured-args)))))

(ert-deftest agent-repl-test-tag-merge-completion-does-not-propagate-error ()
  "A non-zero git tag exit code must not propagate (the cherry-pick
already succeeded; a tag-write failure shouldn't undo that)."
  (cl-letf (((symbol-function 'agent-repl--git-exit-code)
             (lambda (_root &rest _args) 128)))
    ;; Should not error.
    (agent-repl--tag-merge-completion "/tmp/repo" "feat-foo")
    (should t)))

;;;; ---- Tests: workspace-merge-do tags after cherry-pick ----

(ert-deftest agent-repl-test-workspace-merge-do-tags-after-cherry-pick ()
  "workspace-merge-do calls tag-merge-completion after a successful
cherry-pick, with the project-root and source workspace name."
  (let ((tagged nil))
    (cl-letf* (((symbol-function '+workspace-current-name) (lambda () "current"))
               ((symbol-function 'agent-repl--workspace-branch) (lambda (_ws) "branch-x"))
               ((symbol-function 'agent-repl--ws-dir) (lambda (_ws) "/tmp/fake"))
               ((symbol-function 'agent-repl--git-branch-exists-p) (lambda (_dir _br) t))
               ((symbol-function 'agent-repl--cherry-pick-base) (lambda (_dir _br) "abc123"))
               ((symbol-function 'agent-repl--cherry-pick-commits) (lambda (_dir _ws _base _br &optional _auto _silent) nil))
               ((symbol-function 'agent-repl-drawer--refresh-detail-cache) #'ignore)
               ((symbol-function 'agent-repl--nuke-one-workspace) (lambda (&rest _) nil))
               ((symbol-function 'load-file) #'ignore)
               ((symbol-function 'agent-repl--tag-merge-completion)
                (lambda (root ws) (setq tagged (cons root ws)))))
      (agent-repl--workspace-merge-do "other-ws")
      (should (equal tagged (cons "/tmp/fake" "other-ws"))))))

(ert-deftest agent-repl-test-workspace-merge-do-skips-tag-on-cherry-pick-error ()
  "When cherry-pick-commits signals user-error (e.g., a conflict),
tag-merge-completion is NOT invoked and the error is re-signaled to
the caller.  The empty-range case no longer goes through this path —
it returns the `already-incorporated' sentinel and proceeds to the
tag + finish steps (see `…-already-incorporated-still-finishes')."
  (agent-repl-test--with-clean-state
    (puthash "other-ws" '() agent-repl--workspaces)
    (let ((tagged nil))
      (cl-letf* (((symbol-function '+workspace-current-name) (lambda () "current"))
                 ((symbol-function 'agent-repl--workspace-branch) (lambda (_ws) "branch-x"))
                 ((symbol-function 'agent-repl--ws-dir) (lambda (_ws) "/tmp/fake"))
                 ((symbol-function 'agent-repl--git-branch-exists-p) (lambda (_dir _br) t))
                 ((symbol-function 'agent-repl--cherry-pick-base) (lambda (_dir _br) "abc123"))
                 ((symbol-function 'agent-repl--cherry-pick-commits)
                  (lambda (_dir _ws _base _br &optional _auto _silent)
                    (user-error "Conflict cherry-picking — resolve in magit")))
                 ((symbol-function 'agent-repl--nuke-one-workspace) (lambda (&rest _) nil))
                 ((symbol-function 'load-file) #'ignore)
                 ((symbol-function 'agent-repl--tag-merge-completion)
                  (lambda (_root _ws) (setq tagged t))))
        (should-error (agent-repl--workspace-merge-do "other-ws") :type 'user-error)
        (should-not tagged)))))

;;;; ---- Tests: workspace-merge-do success/failure plist effects ----

(ert-deftest agent-repl-test-workspace-merge-do-sets-merge-completed-on-success ()
  "After a successful cherry-pick, `:merge-completed' t is recorded on
the target workspace before the auto-finish tear-down runs.  Stubs
`--finish-workspace' so the plist entry survives the assertion."
  (agent-repl-test--with-clean-state
    (puthash "other-ws" '() agent-repl--workspaces)
    (cl-letf* (((symbol-function '+workspace-current-name) (lambda () "current"))
               ((symbol-function 'agent-repl--workspace-branch) (lambda (_ws) "branch-x"))
               ((symbol-function 'agent-repl--ws-dir) (lambda (_ws) "/tmp/fake"))
               ((symbol-function 'agent-repl--git-branch-exists-p) (lambda (_dir _br) t))
               ((symbol-function 'agent-repl--cherry-pick-base) (lambda (_dir _br) "abc123"))
               ((symbol-function 'agent-repl--cherry-pick-commits) (lambda (_dir _ws _base _br &optional _auto _silent) nil))
               ((symbol-function 'agent-repl-drawer--refresh-detail-cache) #'ignore)
               ((symbol-function 'agent-repl--tag-merge-completion) #'ignore)
               ((symbol-function 'agent-repl--nuke-one-workspace) #'ignore)
               ((symbol-function 'load-file) #'ignore))
      (agent-repl--workspace-merge-do "other-ws" "/tmp/fake" t)
      (should (eq (agent-repl--ws-get "other-ws" :merge-completed) t)))))

(ert-deftest agent-repl-test-workspace-merge-do-sets-merge-failed-on-silent-failure ()
  "When `--cherry-pick-commits' returns `failed' (silent failure: exit
non-zero, no CHERRY_PICK_HEAD), `--workspace-merge-do' flips
`:repl-state' to `:merge-failed' and records `:merge-failed t' so the
drawer surfaces the ❌ badge.  `:merge-completed' is NOT set —
commits did not land, so the workspace stays in its normal (alive)
bucket rather than routing into MERGED."
  (agent-repl-test--with-clean-state
    (puthash "other-ws" '() agent-repl--workspaces)
    (cl-letf* (((symbol-function '+workspace-current-name) (lambda () "current"))
               ((symbol-function 'agent-repl--workspace-branch) (lambda (_ws) "branch-x"))
               ((symbol-function 'agent-repl--ws-dir) (lambda (_ws) "/tmp/fake"))
               ((symbol-function 'agent-repl--git-branch-exists-p) (lambda (_dir _br) t))
               ((symbol-function 'agent-repl--cherry-pick-base) (lambda (_dir _br) "abc123"))
               ((symbol-function 'agent-repl--cherry-pick-commits)
                (lambda (_dir _ws _base _br &optional _auto _silent) 'failed))
               ((symbol-function 'agent-repl-drawer--refresh-detail-cache) #'ignore)
               ((symbol-function 'agent-repl--tag-merge-completion) #'ignore)
               ((symbol-function 'agent-repl--nuke-one-workspace) #'ignore)
               ((symbol-function 'load-file) #'ignore))
      (agent-repl--workspace-merge-do "other-ws" "/tmp/fake" t)
      (should (eq (agent-repl--ws-get "other-ws" :repl-state) :merge-failed))
      (should-not (agent-repl--ws-get "other-ws" :merge-completed))
      (should (eq (agent-repl--ws-get "other-ws" :merge-failed) t)))))

(ert-deftest agent-repl-test-workspace-merge-do-does-not-close-on-silent-failure ()
  "On silent cherry-pick failure (`failed' sentinel), the workspace is
NOT torn down: `--close-workspace' (and its underlying
`--nuke-one-workspace') must not run.  The user keeps the live
session/perspective/buffers so they can investigate and retry — the
whole point of the SPC TAB M failure path is to preserve in-flight
work, not auto-finish a workspace whose commits never landed."
  (agent-repl-test--with-clean-state
    (puthash "other-ws" '() agent-repl--workspaces)
    (let ((close-called nil)
          (nuke-called nil))
      (cl-letf* (((symbol-function '+workspace-current-name) (lambda () "current"))
                 ((symbol-function 'agent-repl--workspace-branch) (lambda (_ws) "branch-x"))
                 ((symbol-function 'agent-repl--ws-dir) (lambda (_ws) "/tmp/fake"))
                 ((symbol-function 'agent-repl--git-branch-exists-p) (lambda (_dir _br) t))
                 ((symbol-function 'agent-repl--cherry-pick-base) (lambda (_dir _br) "abc123"))
                 ((symbol-function 'agent-repl--cherry-pick-commits)
                  (lambda (_dir _ws _base _br &optional _auto _silent) 'failed))
                 ((symbol-function 'agent-repl-drawer--refresh-detail-cache) #'ignore)
                 ((symbol-function 'agent-repl--tag-merge-completion) #'ignore)
                 ((symbol-function 'agent-repl--close-workspace)
                  (lambda (&rest _) (setq close-called t)))
                 ((symbol-function 'agent-repl--nuke-one-workspace)
                  (lambda (&rest _) (setq nuke-called t)))
                 ((symbol-function 'load-file) #'ignore))
        (agent-repl--workspace-merge-do "other-ws" "/tmp/fake" t)
        (should-not close-called)
        (should-not nuke-called)))))

(ert-deftest agent-repl-test-workspace-merge-do-skips-tag-on-silent-failure ()
  "When the cherry-pick silently failed, HEAD has not advanced to include
the target workspace's work — `--tag-merge-completion' MUST NOT run, or
the `merge/<ws>' tag would mislabel an unrelated commit."
  (agent-repl-test--with-clean-state
    (puthash "other-ws" '() agent-repl--workspaces)
    (let ((tagged nil))
      (cl-letf* (((symbol-function '+workspace-current-name) (lambda () "current"))
                 ((symbol-function 'agent-repl--workspace-branch) (lambda (_ws) "branch-x"))
                 ((symbol-function 'agent-repl--ws-dir) (lambda (_ws) "/tmp/fake"))
                 ((symbol-function 'agent-repl--git-branch-exists-p) (lambda (_dir _br) t))
                 ((symbol-function 'agent-repl--cherry-pick-base) (lambda (_dir _br) "abc123"))
                 ((symbol-function 'agent-repl--cherry-pick-commits)
                  (lambda (_dir _ws _base _br &optional _auto _silent) 'failed))
                 ((symbol-function 'agent-repl-drawer--refresh-detail-cache) #'ignore)
                 ((symbol-function 'agent-repl--tag-merge-completion)
                  (lambda (_root _ws) (setq tagged t)))
                 ((symbol-function 'agent-repl--nuke-one-workspace) #'ignore)
                 ((symbol-function 'load-file) #'ignore))
        (agent-repl--workspace-merge-do "other-ws" "/tmp/fake" t)
        (should-not tagged)))))

(ert-deftest agent-repl-test-workspace-merge-do-clears-merge-failed-on-success ()
  "A successful merge must explicitly clear `:merge-failed' (in case a
prior attempt set it).  Without this, a re-run from a silent-failure
state would leave `:merge-failed t' sticky and the drawer would keep
showing ❌ despite the latest run landing cleanly."
  (agent-repl-test--with-clean-state
    (puthash "other-ws" '(:merge-failed t) agent-repl--workspaces)
    (cl-letf* (((symbol-function '+workspace-current-name) (lambda () "current"))
               ((symbol-function 'agent-repl--workspace-branch) (lambda (_ws) "branch-x"))
               ((symbol-function 'agent-repl--ws-dir) (lambda (_ws) "/tmp/fake"))
               ((symbol-function 'agent-repl--git-branch-exists-p) (lambda (_dir _br) t))
               ((symbol-function 'agent-repl--cherry-pick-base) (lambda (_dir _br) "abc123"))
               ((symbol-function 'agent-repl--cherry-pick-commits) (lambda (_dir _ws _base _br &optional _auto _silent) nil))
               ((symbol-function 'agent-repl-drawer--refresh-detail-cache) #'ignore)
               ((symbol-function 'agent-repl--tag-merge-completion) #'ignore)
               ((symbol-function 'agent-repl--nuke-one-workspace) #'ignore)
               ((symbol-function 'load-file) #'ignore))
      (agent-repl--workspace-merge-do "other-ws" "/tmp/fake" t)
      (should-not (agent-repl--ws-get "other-ws" :merge-failed))
      (should (eq (agent-repl--ws-get "other-ws" :repl-state) :merged)))))

(ert-deftest agent-repl-test-workspace-merge-do-sets-repl-state-merged-on-success ()
  "After a successful cherry-pick, `:repl-state' is set to `:merged'
so the 🔀 badge survives the post-nuke poll cycle that would
otherwise mark the (now-vterm-less) workspace `:dead'."
  (agent-repl-test--with-clean-state
    (puthash "other-ws" '() agent-repl--workspaces)
    (cl-letf* (((symbol-function '+workspace-current-name) (lambda () "current"))
               ((symbol-function 'agent-repl--workspace-branch) (lambda (_ws) "branch-x"))
               ((symbol-function 'agent-repl--ws-dir) (lambda (_ws) "/tmp/fake"))
               ((symbol-function 'agent-repl--git-branch-exists-p) (lambda (_dir _br) t))
               ((symbol-function 'agent-repl--cherry-pick-base) (lambda (_dir _br) "abc123"))
               ((symbol-function 'agent-repl--cherry-pick-commits) (lambda (_dir _ws _base _br &optional _auto _silent) nil))
               ((symbol-function 'agent-repl-drawer--refresh-detail-cache) #'ignore)
               ((symbol-function 'agent-repl--tag-merge-completion) #'ignore)
               ((symbol-function 'agent-repl--nuke-one-workspace) #'ignore)
               ((symbol-function 'load-file) #'ignore))
      (agent-repl--workspace-merge-do "other-ws" "/tmp/fake" t)
      (should (eq (agent-repl--ws-get "other-ws" :repl-state) :merged)))))

(ert-deftest agent-repl-test-workspace-merge-do-already-incorporated-still-tears-down ()
  "When cherry-pick-commits returns `already-incorporated' (commits
already on the parent), workspace-merge-do still tags and tears down
via `--nuke-one-workspace' with `preserve-entry' so the drawer's
MERGED bucket picks it up.  `--finish-workspace' is intentionally
NOT called — that runs only when the user explicitly presses `x'."
  (agent-repl-test--with-clean-state
    (puthash "other-ws" '() agent-repl--workspaces)
    (let ((tagged nil)
          (nuked-ws nil)
          (nuked-preserve nil))
      (cl-letf* (((symbol-function '+workspace-current-name) (lambda () "current"))
                 ((symbol-function 'agent-repl--workspace-branch) (lambda (_ws) "branch-x"))
                 ((symbol-function 'agent-repl--ws-dir) (lambda (_ws) "/tmp/fake"))
                 ((symbol-function 'agent-repl--git-branch-exists-p) (lambda (_dir _br) t))
                 ((symbol-function 'agent-repl--cherry-pick-base) (lambda (_dir _br) "abc123"))
                 ((symbol-function 'agent-repl--cherry-pick-commits)
                  (lambda (_dir _ws _base _br &optional _auto _silent) 'already-incorporated))
                 ((symbol-function 'agent-repl-drawer--refresh-detail-cache) #'ignore)
                 ((symbol-function 'agent-repl--tag-merge-completion)
                  (lambda (_root _ws) (setq tagged t)))
                 ((symbol-function 'agent-repl--nuke-one-workspace)
                  (lambda (ws &optional preserve)
                    (setq nuked-ws ws)
                    (setq nuked-preserve preserve)))
                 ((symbol-function 'load-file) #'ignore))
        (agent-repl--workspace-merge-do "other-ws" "/tmp/fake" t)
        (should tagged)
        (should (equal nuked-ws "other-ws"))
        (should nuked-preserve)))))

(ert-deftest agent-repl-test-workspace-merge-do-refreshes-target-magit-after-close ()
  "Successful merge refreshes any open magit-status buffer for the
cherry-pick target directory AFTER `--close-workspace' completes.
This is the deferred magit update: the cherry-pick can run for many
seconds (auto-resolve loop, multi-commit replay) during which magit's
own auto-revert may have last fired mid-flight, leaving the buffer
stuck on an intermediate state.  The trailing refresh guarantees the
final post-merge state is visible without the user pressing `g'.

Asserts:
  1. `--refresh-magit-status-for-dir' is called with PROJECT-ROOT.
  2. It is called AFTER `--close-workspace' — the order matters so
     any close-time buffer churn is settled before the refresh fires."
  (agent-repl-test--with-clean-state
    (puthash "other-ws" '() agent-repl--workspaces)
    (let ((events nil))
      (cl-letf* (((symbol-function '+workspace-current-name) (lambda () "current"))
                 ((symbol-function 'agent-repl--workspace-branch) (lambda (_ws) "branch-x"))
                 ((symbol-function 'agent-repl--ws-dir) (lambda (_ws) "/tmp/master"))
                 ((symbol-function 'agent-repl--git-branch-exists-p) (lambda (_dir _br) t))
                 ((symbol-function 'agent-repl--cherry-pick-base) (lambda (_dir _br) "abc123"))
                 ((symbol-function 'agent-repl--cherry-pick-commits) (lambda (_dir _ws _base _br &optional _auto _silent) nil))
                 ((symbol-function 'agent-repl-drawer--refresh-detail-cache) #'ignore)
                 ((symbol-function 'agent-repl--tag-merge-completion) #'ignore)
                 ((symbol-function 'load-file) #'ignore)
                 ((symbol-function 'agent-repl--close-workspace)
                  (lambda (&rest _) (push 'close events)))
                 ((symbol-function 'agent-repl--refresh-magit-status-for-dir)
                  (lambda (dir &optional _ws)
                    (push (list 'refresh dir) events))))
        (agent-repl--workspace-merge-do "other-ws" "/tmp/master" t)
        (setq events (nreverse events))
        (should (equal events
                       '(close (refresh "/tmp/master"))))))))

(ert-deftest agent-repl-test-workspace-merge-do-skips-target-magit-refresh-on-silent-failure ()
  "Silent cherry-pick failure (sentinel `failed') must NOT refresh the
target's magit-status — the merge didn't land, the worktree is in its
pre-merge state, and the user keeps the source workspace alive to
investigate.  Forcing a refresh of the (unchanged) target dir would
be a misleading nudge that something landed."
  (agent-repl-test--with-clean-state
    (puthash "other-ws" '() agent-repl--workspaces)
    (let ((refresh-called nil))
      (cl-letf* (((symbol-function '+workspace-current-name) (lambda () "current"))
                 ((symbol-function 'agent-repl--workspace-branch) (lambda (_ws) "branch-x"))
                 ((symbol-function 'agent-repl--ws-dir) (lambda (_ws) "/tmp/master"))
                 ((symbol-function 'agent-repl--git-branch-exists-p) (lambda (_dir _br) t))
                 ((symbol-function 'agent-repl--cherry-pick-base) (lambda (_dir _br) "abc123"))
                 ((symbol-function 'agent-repl--cherry-pick-commits)
                  (lambda (_dir _ws _base _br &optional _auto _silent) 'failed))
                 ((symbol-function 'agent-repl--tag-merge-completion) #'ignore)
                 ((symbol-function 'agent-repl--close-workspace) #'ignore)
                 ((symbol-function 'agent-repl-drawer--refresh-detail-cache) #'ignore)
                 ((symbol-function 'load-file) #'ignore)
                 ((symbol-function 'agent-repl--refresh-magit-status-for-dir)
                  (lambda (&rest _) (setq refresh-called t))))
        (agent-repl--workspace-merge-do "other-ws" "/tmp/master" t)
        (should-not refresh-called)))))

(ert-deftest agent-repl-test-workspace-merge-do-skips-target-magit-refresh-on-conflict ()
  "Cherry-pick conflict (`agent-repl-merge-conflict-error') must NOT
refresh the target's magit.  The conflict resolution path opens magit
on the target itself via `--surface-silent-merge-conflict' (when
silent) or aborts the cherry-pick — running our trailing refresh on
top would either fight the conflict UI or refresh state that was
just aborted."
  (agent-repl-test--with-clean-state
    (puthash "other-ws" '() agent-repl--workspaces)
    (let ((refresh-called nil))
      (cl-letf* (((symbol-function '+workspace-current-name) (lambda () "current"))
                 ((symbol-function 'agent-repl--workspace-branch) (lambda (_ws) "branch-x"))
                 ((symbol-function 'agent-repl--ws-dir) (lambda (_ws) "/tmp/master"))
                 ((symbol-function 'agent-repl--git-branch-exists-p) (lambda (_dir _br) t))
                 ((symbol-function 'agent-repl--cherry-pick-base) (lambda (_dir _br) "abc123"))
                 ((symbol-function 'agent-repl--cherry-pick-commits)
                  (lambda (_dir _ws _base _br &optional _auto _silent)
                    (signal 'agent-repl-merge-conflict-error '("conflict"))))
                 ((symbol-function 'agent-repl--mark-merge-conflict) #'ignore)
                 ((symbol-function 'agent-repl--clear-in-flight-merge) #'ignore)
                 ((symbol-function 'agent-repl--drain-merge-queue) #'ignore)
                 ((symbol-function 'load-file) #'ignore)
                 ((symbol-function 'agent-repl--refresh-magit-status-for-dir)
                  (lambda (&rest _) (setq refresh-called t))))
        (ignore-errors
          (agent-repl--workspace-merge-do "other-ws" "/tmp/master" t))
        (should-not refresh-called)))))

(ert-deftest agent-repl-test-workspace-merge-do-routes-close-through-gns-gating ()
  "Successful merge must dispatch the editor-side close via
`--gns-sockets-close-then' so the in-workspace agent is sent
`/gns-sockets close' before its vterm dies.  The teardown thunk
forwarded to the gate must call `--close-workspace' with
`preserve-entry'."
  (agent-repl-test--with-clean-state
    (puthash "other-ws" '() agent-repl--workspaces)
    (let ((gating-ws :unset)
          (gating-teardown nil)
          (closed-ws :unset)
          (closed-preserve :unset))
      (cl-letf* (((symbol-function '+workspace-current-name) (lambda () "current"))
                 ((symbol-function 'agent-repl--workspace-branch) (lambda (_ws) "branch-x"))
                 ((symbol-function 'agent-repl--ws-dir) (lambda (_ws) "/tmp/fake"))
                 ((symbol-function 'agent-repl--git-branch-exists-p) (lambda (_dir _br) t))
                 ((symbol-function 'agent-repl--cherry-pick-base) (lambda (_dir _br) "abc123"))
                 ((symbol-function 'agent-repl--cherry-pick-commits) (lambda (_dir _ws _base _br &optional _auto _silent) nil))
                 ((symbol-function 'agent-repl-drawer--refresh-detail-cache) #'ignore)
                 ((symbol-function 'agent-repl--tag-merge-completion) #'ignore)
                 ((symbol-function 'load-file) #'ignore)
                 ((symbol-function 'agent-repl--gns-sockets-close-then)
                  (lambda (ws fn)
                    (setq gating-ws ws
                          gating-teardown fn)))
                 ((symbol-function 'agent-repl--close-workspace)
                  (lambda (ws &optional preserve)
                    (setq closed-ws ws
                          closed-preserve preserve))))
        (agent-repl--workspace-merge-do "other-ws" "/tmp/fake" t)
        (should (equal gating-ws "other-ws"))
        (should (functionp gating-teardown))
        (funcall gating-teardown)
        (should (equal closed-ws "other-ws"))
        (should (eq closed-preserve 'preserve-entry))))))

(ert-deftest agent-repl-test-workspace-merge-do-tears-down-on-success ()
  "Successful merge nukes the target workspace's session/persp/buffers
with `preserve-entry' so the hash entry survives for the drawer's
MERGED bucket.  The git worktree on disk is left in place — only an
explicit drawer `x' (`--finish-workspace') removes it."
  (agent-repl-test--with-clean-state
    (puthash "other-ws" '() agent-repl--workspaces)
    (let ((nuked-ws nil)
          (nuked-preserve nil))
      (cl-letf* (((symbol-function '+workspace-current-name) (lambda () "current"))
                 ((symbol-function 'agent-repl--workspace-branch) (lambda (_ws) "branch-x"))
                 ((symbol-function 'agent-repl--ws-dir) (lambda (_ws) "/tmp/fake"))
                 ((symbol-function 'agent-repl--git-branch-exists-p) (lambda (_dir _br) t))
                 ((symbol-function 'agent-repl--cherry-pick-base) (lambda (_dir _br) "abc123"))
                 ((symbol-function 'agent-repl--cherry-pick-commits) (lambda (_dir _ws _base _br &optional _auto _silent) nil))
                 ((symbol-function 'agent-repl-drawer--refresh-detail-cache) #'ignore)
                 ((symbol-function 'agent-repl--tag-merge-completion) #'ignore)
                 ((symbol-function 'load-file) #'ignore)
                 ((symbol-function 'agent-repl--nuke-one-workspace)
                  (lambda (ws &optional preserve)
                    (setq nuked-ws ws)
                    (setq nuked-preserve preserve))))
        (agent-repl--workspace-merge-do "other-ws" "/tmp/fake" t)
        (should (equal nuked-ws "other-ws"))
        (should nuked-preserve)))))

(ert-deftest agent-repl-test-workspace-merge-do-defers-success-teardown ()
  "Success-path teardown (gns-sockets-close-then -> close-workspace) is
routed through `agent-repl--defer-to-main-thread' so the perspective
kill, vterm kill, and buffer cleanup all run on the main thread.  This
is what makes `--workspace-merge-do' safe to execute from the worker
thread spawned by `agent-repl--workspace-merge-async'.

Pinned with a stub that captures the defer call — the test fixture's
default override would otherwise invoke the thunk immediately, hiding
whether the defer call was ever made."
  (agent-repl-test--with-clean-state
    (puthash "other-ws" '() agent-repl--workspaces)
    (let ((defer-calls 0))
      (cl-letf* (((symbol-function '+workspace-current-name) (lambda () "current"))
                 ((symbol-function 'agent-repl--workspace-branch) (lambda (_ws) "branch-x"))
                 ((symbol-function 'agent-repl--ws-dir) (lambda (_ws) "/tmp/fake"))
                 ((symbol-function 'agent-repl--git-branch-exists-p) (lambda (_dir _br) t))
                 ((symbol-function 'agent-repl--cherry-pick-base) (lambda (_dir _br) "abc123"))
                 ((symbol-function 'agent-repl--cherry-pick-commits) (lambda (_dir _ws _base _br &optional _auto _silent) nil))
                 ((symbol-function 'agent-repl-drawer--refresh-detail-cache) #'ignore)
                 ((symbol-function 'agent-repl--tag-merge-completion) #'ignore)
                 ((symbol-function 'load-file) #'ignore)
                 ((symbol-function 'agent-repl--defer-to-main-thread)
                  (lambda (_thunk) (cl-incf defer-calls))))
        (agent-repl--workspace-merge-do "other-ws" "/tmp/fake" t)
        (should (= defer-calls 1))))))

(ert-deftest agent-repl-test-workspace-merge-do-does-not-call-finish-workspace ()
  "Successful merge must NOT call `--finish-workspace' — that's reserved
for the drawer `x' path and removes the git worktree, which is exactly
what we want to defer until the user explicitly chooses."
  (agent-repl-test--with-clean-state
    (puthash "other-ws" '() agent-repl--workspaces)
    (let ((finish-called nil))
      (cl-letf* (((symbol-function '+workspace-current-name) (lambda () "current"))
                 ((symbol-function 'agent-repl--workspace-branch) (lambda (_ws) "branch-x"))
                 ((symbol-function 'agent-repl--ws-dir) (lambda (_ws) "/tmp/fake"))
                 ((symbol-function 'agent-repl--git-branch-exists-p) (lambda (_dir _br) t))
                 ((symbol-function 'agent-repl--cherry-pick-base) (lambda (_dir _br) "abc123"))
                 ((symbol-function 'agent-repl--cherry-pick-commits) (lambda (_dir _ws _base _br &optional _auto _silent) nil))
                 ((symbol-function 'agent-repl-drawer--refresh-detail-cache) #'ignore)
                 ((symbol-function 'agent-repl--tag-merge-completion) #'ignore)
                 ((symbol-function 'agent-repl--nuke-one-workspace) #'ignore)
                 ((symbol-function 'load-file) #'ignore)
                 ((symbol-function 'agent-repl--finish-workspace)
                  (lambda (&rest _) (setq finish-called t))))
        (agent-repl--workspace-merge-do "other-ws" "/tmp/fake" t)
        (should-not finish-called)))))

(ert-deftest agent-repl-test-workspace-merge-do-records-merge-completed-at ()
  "Successful merge stamps `:merge-completed-at' on the target so the
drawer can render an age/timestamp once that surfaces in the UI."
  (agent-repl-test--with-clean-state
    (puthash "other-ws" '() agent-repl--workspaces)
    (cl-letf* (((symbol-function '+workspace-current-name) (lambda () "current"))
               ((symbol-function 'agent-repl--workspace-branch) (lambda (_ws) "branch-x"))
               ((symbol-function 'agent-repl--ws-dir) (lambda (_ws) "/tmp/fake"))
               ((symbol-function 'agent-repl--git-branch-exists-p) (lambda (_dir _br) t))
               ((symbol-function 'agent-repl--cherry-pick-base) (lambda (_dir _br) "abc123"))
               ((symbol-function 'agent-repl--cherry-pick-commits) (lambda (_dir _ws _base _br &optional _auto _silent) nil))
               ((symbol-function 'agent-repl-drawer--refresh-detail-cache) #'ignore)
               ((symbol-function 'agent-repl--tag-merge-completion) #'ignore)
               ((symbol-function 'agent-repl--nuke-one-workspace) #'ignore)
               ((symbol-function 'load-file) #'ignore))
      (agent-repl--workspace-merge-do "other-ws" "/tmp/fake" t)
      (should (numberp (agent-repl--ws-get "other-ws" :merge-completed-at))))))

(ert-deftest agent-repl-test-workspace-merge-do-marks-dead-on-cherry-pick-error ()
  "GENERIC cherry-pick failure (non-conflict `user-error') flips the
target workspace to `:repl-state :dead' (and clears `:agent-state')
so the drawer shows the ❌ badge.  The error is still re-signaled.
Conflict-specific errors go through a different path — see
`agent-repl-test-workspace-merge-do-marks-merge-conflict-on-conflict-error'."
  (agent-repl-test--with-clean-state
    (puthash "other-ws" '(:agent-state :thinking) agent-repl--workspaces)
    (cl-letf* (((symbol-function '+workspace-current-name) (lambda () "current"))
               ((symbol-function 'agent-repl--workspace-branch) (lambda (_ws) "branch-x"))
               ((symbol-function 'agent-repl--ws-dir) (lambda (_ws) "/tmp/fake"))
               ((symbol-function 'agent-repl--git-branch-exists-p) (lambda (_dir _br) t))
               ((symbol-function 'agent-repl--cherry-pick-base) (lambda (_dir _br) "abc123"))
               ((symbol-function 'agent-repl--cherry-pick-commits)
                (lambda (_dir _ws _base _br &optional _auto _silent) (user-error "Generic failure")))
               ((symbol-function 'agent-repl--nuke-one-workspace) #'ignore)
               ((symbol-function 'load-file) #'ignore))
      (should-error (agent-repl--workspace-merge-do "other-ws" "/tmp/fake" t)
                    :type 'user-error)
      (should (eq (agent-repl--ws-get "other-ws" :repl-state) :dead))
      (should (null (agent-repl--ws-get "other-ws" :agent-state))))))

(ert-deftest agent-repl-test-workspace-merge-do-marks-merge-conflict-on-conflict-error ()
  "When the cherry-pick raises `agent-repl-merge-conflict-error', the
target workspace flips to `:repl-state :merge-conflict' (not `:dead')
so the drawer renders the 💥 badge.  `:agent-state' is preserved
because the vterm is still alive — the user can keep typing after
resolving the conflict externally."
  (agent-repl-test--with-clean-state
    (puthash "other-ws" '(:agent-state :thinking) agent-repl--workspaces)
    (cl-letf* (((symbol-function '+workspace-current-name) (lambda () "current"))
               ((symbol-function 'agent-repl--workspace-branch) (lambda (_ws) "branch-x"))
               ((symbol-function 'agent-repl--ws-dir) (lambda (_ws) "/tmp/fake"))
               ((symbol-function 'agent-repl--git-branch-exists-p) (lambda (_dir _br) t))
               ((symbol-function 'agent-repl--cherry-pick-base) (lambda (_dir _br) "abc123"))
               ((symbol-function 'agent-repl--cherry-pick-commits)
                (lambda (_dir _ws _base _br &optional _auto _silent)
                  (signal 'agent-repl-merge-conflict-error '("Conflict"))))
               ((symbol-function 'agent-repl--nuke-one-workspace) #'ignore)
               ((symbol-function 'load-file) #'ignore))
      (should-error (agent-repl--workspace-merge-do "other-ws" "/tmp/fake" t)
                    :type 'agent-repl-merge-conflict-error)
      (should (eq (agent-repl--ws-get "other-ws" :repl-state) :merge-conflict))
      ;; vterm-alive workspace should keep its agent-state through a conflict
      (should (eq (agent-repl--ws-get "other-ws" :agent-state) :thinking)))))

(ert-deftest agent-repl-test-workspace-merge-do-clears-prior-merge-conflict-on-retry ()
  "A retry of a previously-conflicted merge clears the stale 💥 badge
before re-entering the cherry-pick so the drawer reflects in-flight
state, not stale failure state.  Only `:merge-conflict' is cleared —
other repl-states are preserved."
  (agent-repl-test--with-clean-state
    (puthash "other-ws" '(:repl-state :merge-conflict) agent-repl--workspaces)
    (cl-letf* (((symbol-function '+workspace-current-name) (lambda () "current"))
               ((symbol-function 'agent-repl--workspace-branch) (lambda (_ws) "branch-x"))
               ((symbol-function 'agent-repl--ws-dir) (lambda (_ws) "/tmp/fake"))
               ((symbol-function 'agent-repl--git-branch-exists-p) (lambda (_dir _br) t))
               ((symbol-function 'agent-repl--cherry-pick-base) (lambda (_dir _br) "abc123"))
               ((symbol-function 'agent-repl--cherry-pick-commits)
                (lambda (_dir _ws _base _br &optional _auto _silent) nil))
               ((symbol-function 'agent-repl-drawer--refresh-detail-cache) #'ignore)
               ((symbol-function 'agent-repl--nuke-one-workspace) #'ignore)
               ((symbol-function 'agent-repl--tag-merge-completion) #'ignore)
               ((symbol-function 'agent-repl--gns-sockets-close-then)
                (lambda (_ws thunk) (funcall thunk)))
               ((symbol-function 'agent-repl--close-workspace) #'ignore)
               ((symbol-function 'load-file) #'ignore))
      (agent-repl--workspace-merge-do "other-ws" "/tmp/fake")
      ;; After a successful retry, the workspace ends up :merged — but
      ;; the assertion we care about is that the initial :merge-conflict
      ;; was cleared (it would NOT be `:merge-conflict' here regardless).
      (should-not (eq (agent-repl--ws-get "other-ws" :repl-state)
                      :merge-conflict)))))

(ert-deftest agent-repl-test-mark-merge-conflict-sets-state ()
  "Direct invariants of `agent-repl--mark-merge-conflict':
`:repl-state' → `:merge-conflict', `:merging' cleared,
`:merge-completed' cleared, `:agent-state' NOT touched."
  (agent-repl-test--with-clean-state
    (puthash "ws" '(:agent-state :thinking
                    :merging t
                    :merge-completed t)
             agent-repl--workspaces)
    (agent-repl--mark-merge-conflict "ws" '(error "test"))
    (should (eq (agent-repl--ws-get "ws" :repl-state) :merge-conflict))
    (should (null (agent-repl--ws-get "ws" :merging)))
    (should (null (agent-repl--ws-get "ws" :merge-completed)))
    ;; :agent-state must remain — vterm is still alive on a conflict
    (should (eq (agent-repl--ws-get "ws" :agent-state) :thinking))))

(ert-deftest agent-repl-test-workspace-merge-do-does-not-set-merge-completed-on-error ()
  "A failed cherry-pick must leave `:merge-completed' nil so the
workspace cannot accidentally surface in MERGED on the strength of an
earlier partial success."
  (agent-repl-test--with-clean-state
    (puthash "other-ws" '(:merge-completed nil) agent-repl--workspaces)
    (cl-letf* (((symbol-function '+workspace-current-name) (lambda () "current"))
               ((symbol-function 'agent-repl--workspace-branch) (lambda (_ws) "branch-x"))
               ((symbol-function 'agent-repl--ws-dir) (lambda (_ws) "/tmp/fake"))
               ((symbol-function 'agent-repl--git-branch-exists-p) (lambda (_dir _br) t))
               ((symbol-function 'agent-repl--cherry-pick-base) (lambda (_dir _br) "abc123"))
               ((symbol-function 'agent-repl--cherry-pick-commits)
                (lambda (_dir _ws _base _br &optional _auto _silent) (user-error "Conflict")))
               ((symbol-function 'agent-repl--nuke-one-workspace) #'ignore)
               ((symbol-function 'load-file) #'ignore))
      (ignore-errors
        (agent-repl--workspace-merge-do "other-ws" "/tmp/fake" t))
      (should-not (agent-repl--ws-get "other-ws" :merge-completed)))))

(ert-deftest agent-repl-test-workspace-merge-do-refreshes-detail-cache-on-success ()
  "After a successful merge, the drawer's `:detail-*' cache for the
target workspace is refreshed.  The cache populated pre-merge (e.g.,
`:detail-master-ahead' showing the soon-to-be-merged commit count)
would otherwise linger in the MERGED bucket's expanded view — the
post-merge refresh ensures the rendered values reflect current git
state, not stale pre-merge snapshots."
  (agent-repl-test--with-clean-state
    (puthash "other-ws" '(:detail-master-ahead 99) agent-repl--workspaces)
    (let ((refreshed-ws nil))
      (cl-letf* (((symbol-function '+workspace-current-name) (lambda () "current"))
                 ((symbol-function 'agent-repl--workspace-branch) (lambda (_ws) "branch-x"))
                 ((symbol-function 'agent-repl--ws-dir) (lambda (_ws) "/tmp/fake"))
                 ((symbol-function 'agent-repl--git-branch-exists-p) (lambda (_dir _br) t))
                 ((symbol-function 'agent-repl--cherry-pick-base) (lambda (_dir _br) "abc123"))
                 ((symbol-function 'agent-repl--cherry-pick-commits) (lambda (_dir _ws _base _br &optional _auto _silent) nil))
                 ((symbol-function 'agent-repl--tag-merge-completion) #'ignore)
                 ((symbol-function 'agent-repl--nuke-one-workspace) #'ignore)
                 ((symbol-function 'load-file) #'ignore)
                 ((symbol-function 'agent-repl-drawer--refresh-detail-cache)
                  (lambda (ws) (setq refreshed-ws ws))))
        (agent-repl--workspace-merge-do "other-ws" "/tmp/fake" t)
        (should (equal refreshed-ws "other-ws"))))))

(ert-deftest agent-repl-test-workspace-merge-do-refreshes-detail-cache-after-nuke ()
  "The post-merge `:detail-*' refresh runs after `--nuke-one-workspace'
so the cache reflects the fully settled MERGED-bucket state — the nuke
preserves the hash entry and the worktree on disk, so the refresh's
synchronous git calls still resolve."
  (agent-repl-test--with-clean-state
    (puthash "other-ws" '() agent-repl--workspaces)
    (let ((call-order nil))
      (cl-letf* (((symbol-function '+workspace-current-name) (lambda () "current"))
                 ((symbol-function 'agent-repl--workspace-branch) (lambda (_ws) "branch-x"))
                 ((symbol-function 'agent-repl--ws-dir) (lambda (_ws) "/tmp/fake"))
                 ((symbol-function 'agent-repl--git-branch-exists-p) (lambda (_dir _br) t))
                 ((symbol-function 'agent-repl--cherry-pick-base) (lambda (_dir _br) "abc123"))
                 ((symbol-function 'agent-repl--cherry-pick-commits) (lambda (_dir _ws _base _br &optional _auto _silent) nil))
                 ((symbol-function 'agent-repl--tag-merge-completion) #'ignore)
                 ((symbol-function 'load-file) #'ignore)
                 ((symbol-function 'agent-repl--nuke-one-workspace)
                  (lambda (&rest _) (push 'nuke call-order)))
                 ((symbol-function 'agent-repl-drawer--refresh-detail-cache)
                  (lambda (_ws) (push 'refresh call-order))))
        (agent-repl--workspace-merge-do "other-ws" "/tmp/fake" t)
        (should (equal (nreverse call-order) '(nuke refresh)))))))

(ert-deftest agent-repl-test-workspace-merge-do-skips-detail-refresh-when-unbound ()
  "The post-merge cache refresh is guarded by `fboundp' so a load-order
oddity (drawer not yet loaded) cannot break the merge.  Verifies the
merge completes normally when `--refresh-detail-cache' is unbound."
  (agent-repl-test--with-clean-state
    (puthash "other-ws" '() agent-repl--workspaces)
    (cl-letf* (((symbol-function '+workspace-current-name) (lambda () "current"))
               ((symbol-function 'agent-repl--workspace-branch) (lambda (_ws) "branch-x"))
               ((symbol-function 'agent-repl--ws-dir) (lambda (_ws) "/tmp/fake"))
               ((symbol-function 'agent-repl--git-branch-exists-p) (lambda (_dir _br) t))
               ((symbol-function 'agent-repl--cherry-pick-base) (lambda (_dir _br) "abc123"))
               ((symbol-function 'agent-repl--cherry-pick-commits) (lambda (_dir _ws _base _br &optional _auto _silent) nil))
               ((symbol-function 'agent-repl--tag-merge-completion) #'ignore)
               ((symbol-function 'agent-repl--nuke-one-workspace) #'ignore)
               ((symbol-function 'load-file) #'ignore)
               ((symbol-function 'fboundp)
                (lambda (sym) (not (eq sym 'agent-repl-drawer--refresh-detail-cache)))))
      ;; Should complete without error and still record :merge-completed.
      (agent-repl--workspace-merge-do "other-ws" "/tmp/fake" t)
      (should (eq (agent-repl--ws-get "other-ws" :merge-completed) t)))))

(ert-deftest agent-repl-test-workspace-merge-do-skips-detail-refresh-on-failure ()
  "A failed cherry-pick must NOT refresh the detail cache — the merge
didn't complete, so there's no fresh post-merge state to capture, and
running the refresh on a workspace headed for `:dead' just wastes git
calls."
  (agent-repl-test--with-clean-state
    (puthash "other-ws" '() agent-repl--workspaces)
    (let ((refresh-called nil))
      (cl-letf* (((symbol-function '+workspace-current-name) (lambda () "current"))
                 ((symbol-function 'agent-repl--workspace-branch) (lambda (_ws) "branch-x"))
                 ((symbol-function 'agent-repl--ws-dir) (lambda (_ws) "/tmp/fake"))
                 ((symbol-function 'agent-repl--git-branch-exists-p) (lambda (_dir _br) t))
                 ((symbol-function 'agent-repl--cherry-pick-base) (lambda (_dir _br) "abc123"))
                 ((symbol-function 'agent-repl--cherry-pick-commits)
                  (lambda (_dir _ws _base _br &optional _auto _silent) (user-error "Conflict")))
                 ((symbol-function 'agent-repl--nuke-one-workspace) #'ignore)
                 ((symbol-function 'load-file) #'ignore)
                 ((symbol-function 'agent-repl-drawer--refresh-detail-cache)
                  (lambda (_ws) (setq refresh-called t))))
        (ignore-errors
          (agent-repl--workspace-merge-do "other-ws" "/tmp/fake" t))
        (should-not refresh-called)))))

(ert-deftest agent-repl-test-workspace-merge-do-clears-merging-on-success ()
  "After a successful cherry-pick, `:merging' is cleared on the target
workspace.  Asserts the in-flight workflow flag does not linger past
the success transition — the workspace must leave the MERGING bucket
and enter MERGED in the same operation."
  (agent-repl-test--with-clean-state
    (puthash "other-ws" '() agent-repl--workspaces)
    (cl-letf* (((symbol-function '+workspace-current-name) (lambda () "current"))
               ((symbol-function 'agent-repl--workspace-branch) (lambda (_ws) "branch-x"))
               ((symbol-function 'agent-repl--ws-dir) (lambda (_ws) "/tmp/fake"))
               ((symbol-function 'agent-repl--git-branch-exists-p) (lambda (_dir _br) t))
               ((symbol-function 'agent-repl--cherry-pick-base) (lambda (_dir _br) "abc123"))
               ((symbol-function 'agent-repl--cherry-pick-commits) (lambda (_dir _ws _base _br &optional _auto _silent) nil))
               ((symbol-function 'agent-repl-drawer--refresh-detail-cache) #'ignore)
               ((symbol-function 'agent-repl--tag-merge-completion) #'ignore)
               ((symbol-function 'agent-repl--nuke-one-workspace) #'ignore)
               ((symbol-function 'load-file) #'ignore))
      (agent-repl--workspace-merge-do "other-ws" "/tmp/fake" t)
      (should-not (agent-repl--ws-get "other-ws" :merging)))))

(ert-deftest agent-repl-test-workspace-merge-do-clears-merging-on-failure ()
  "A failed cherry-pick must leave `:merging' nil so the workspace
exits the MERGING bucket — the dead/❌ badge from
`--mark-merge-failed' takes over, and the in-flight flag must not
linger and falsely suggest the merge is still running."
  (agent-repl-test--with-clean-state
    (puthash "other-ws" '() agent-repl--workspaces)
    (cl-letf* (((symbol-function '+workspace-current-name) (lambda () "current"))
               ((symbol-function 'agent-repl--workspace-branch) (lambda (_ws) "branch-x"))
               ((symbol-function 'agent-repl--ws-dir) (lambda (_ws) "/tmp/fake"))
               ((symbol-function 'agent-repl--git-branch-exists-p) (lambda (_dir _br) t))
               ((symbol-function 'agent-repl--cherry-pick-base) (lambda (_dir _br) "abc123"))
               ((symbol-function 'agent-repl--cherry-pick-commits)
                (lambda (_dir _ws _base _br &optional _auto _silent) (user-error "Conflict")))
               ((symbol-function 'agent-repl--nuke-one-workspace) #'ignore)
               ((symbol-function 'load-file) #'ignore))
      (ignore-errors
        (agent-repl--workspace-merge-do "other-ws" "/tmp/fake" t))
      (should-not (agent-repl--ws-get "other-ws" :merging)))))

(ert-deftest agent-repl-test-workspace-merge-do-sets-merging-during-cherry-pick ()
  "`:merging' t is observable on the target workspace while the
cherry-pick is running.  Probed via a stubbed cherry-pick that
captures the plist mid-flight — asserts the flag is set before the
cherry-pick begins, not after."
  (agent-repl-test--with-clean-state
    (puthash "other-ws" '() agent-repl--workspaces)
    (let ((merging-mid-flight nil))
      (cl-letf* (((symbol-function '+workspace-current-name) (lambda () "current"))
                 ((symbol-function 'agent-repl--workspace-branch) (lambda (_ws) "branch-x"))
                 ((symbol-function 'agent-repl--ws-dir) (lambda (_ws) "/tmp/fake"))
                 ((symbol-function 'agent-repl--git-branch-exists-p) (lambda (_dir _br) t))
                 ((symbol-function 'agent-repl--cherry-pick-base) (lambda (_dir _br) "abc123"))
                 ((symbol-function 'agent-repl--cherry-pick-commits)
                  (lambda (_dir _ws _base _br &optional _auto _silent)
                    (setq merging-mid-flight
                          (agent-repl--ws-get "other-ws" :merging))
                    nil))
                 ((symbol-function 'agent-repl-drawer--refresh-detail-cache) #'ignore)
                 ((symbol-function 'agent-repl--tag-merge-completion) #'ignore)
                 ((symbol-function 'agent-repl--nuke-one-workspace) #'ignore)
                 ((symbol-function 'load-file) #'ignore))
        (agent-repl--workspace-merge-do "other-ws" "/tmp/fake" t)
        (should (eq merging-mid-flight t))))))

;;;; ---- Tests: ws-merge-in-progress-p ----

(ert-deftest agent-repl-test-ws-merge-in-progress-p-true-when-set ()
  "Returns t when `:merging' is explicitly t."
  (agent-repl-test--with-clean-state
    (puthash "ws" '(:merging t) agent-repl--workspaces)
    (should (agent-repl--ws-merge-in-progress-p "ws"))))

(ert-deftest agent-repl-test-ws-merge-in-progress-p-nil-when-absent ()
  "Returns nil when `:merging' is not set — workspace must default
away from MERGING."
  (agent-repl-test--with-clean-state
    (puthash "ws" '() agent-repl--workspaces)
    (should-not (agent-repl--ws-merge-in-progress-p "ws"))))

(ert-deftest agent-repl-test-ws-merge-in-progress-p-nil-when-other-truthy ()
  "Only the symbol t qualifies as in-flight.  Guards against a future
caller storing a truthy-but-non-t value (e.g. a start timestamp) and
unintentionally placing the workspace into MERGING."
  (agent-repl-test--with-clean-state
    (puthash "ws" '(:merging "1970") agent-repl--workspaces)
    (should-not (agent-repl--ws-merge-in-progress-p "ws"))))

;;;; ---- Tests: ws-merge-completed-p ----

(ert-deftest agent-repl-test-ws-merge-completed-p-true-when-set ()
  "Returns t when `:merge-completed' is explicitly t."
  (agent-repl-test--with-clean-state
    (puthash "ws" '(:merge-completed t) agent-repl--workspaces)
    (should (agent-repl--ws-merge-completed-p "ws"))))

(ert-deftest agent-repl-test-ws-merge-completed-p-nil-when-absent ()
  "Returns nil on cache miss — drawer must default such workspaces away
from MERGED."
  (agent-repl-test--with-clean-state
    (puthash "ws" '() agent-repl--workspaces)
    (should-not (agent-repl--ws-merge-completed-p "ws"))))

(ert-deftest agent-repl-test-ws-merge-completed-p-nil-when-other-truthy ()
  "Only the symbol t qualifies as completed.  This blocks a future
caller from mistakenly storing a truthy-but-non-t marker (e.g. a
timestamp) and getting an accidental MERGED bucket placement."
  (agent-repl-test--with-clean-state
    (puthash "ws" '(:merge-completed "1970") agent-repl--workspaces)
    (should-not (agent-repl--ws-merge-completed-p "ws"))))

;;;; ---- Tests: workspace-merge-current-into-source ----

(ert-deftest agent-repl-test-merge-current-into-source-routes-through-async-wrapper ()
  "Interactive `SPC TAB M' routes through `agent-repl--workspace-merge-async'
\(same path `/workspace-merge' skill takes — there is no behavioral diff
between the two callers), passing the current workspace name and its
resolved merge-routing-root.  The async wrapper handles close-then-spawn-
then-reopen-on-failure; tests of that lifecycle live near the helper."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-merge-async-" t))
          (async-args :unset))
      (unwind-protect
          (progn
            (agent-repl--ws-put "wt-ws" :project-dir "/tmp/wt-dir/")
            (agent-repl--ws-put "wt-ws" :source-ws-dir tmpdir)
            (cl-letf (((symbol-function '+workspace-current-name)
                       (lambda () "wt-ws"))
                      ((symbol-function 'agent-repl--workspace-merge-async)
                       (lambda (ws repo-root)
                         (setq async-args (list ws repo-root)))))
              (agent-repl-workspace-merge-current-into-source)
              (should (equal (car async-args) "wt-ws"))
              (should (equal (cadr async-args) tmpdir))))
        (delete-directory tmpdir t)))))

(ert-deftest agent-repl-test-merge-into-source-routes-to-recorded-source-dir ()
  "When :source-ws-dir points at an existing dir, --workspace-merge-do receives it
as the resolved target.  The interactive entry point now routes through the
cherry-pick handler (silent=t auto-resolve=t), so `switch-to-project' is NOT
called on the happy path — the assertion is on merge-do's TARGET-DIR arg."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-merge-src-" t))
          (switch-called nil)
          (merge-do-args :unset))
      (unwind-protect
          (progn
            (agent-repl--ws-put "wt-ws" :project-dir "/tmp/wt-dir/")
            (agent-repl--ws-put "wt-ws" :source-ws-dir tmpdir)
            (cl-letf (((symbol-function '+workspace-current-name) (lambda () "wt-ws"))
                      ((symbol-function 'agent-repl--master-worktree-path)
                       (lambda (_root) nil))
                      ((symbol-function 'agent-repl--main-worktree-path)
                       (lambda (dir) dir))
                      ((symbol-function 'agent-repl--cherry-pick-in-progress-p)
                       (lambda (_) nil))
                      ((symbol-function 'agent-repl--assert-clean-worktree)
                       (lambda (&rest _) nil))
                      ((symbol-function 'agent-repl-switch-to-project)
                       (lambda (&rest _) (setq switch-called t)))
                      ((symbol-function 'agent-repl--git-branch-of-dir)
                       (lambda (_) nil))
                      ((symbol-function 'agent-repl--workspace-merge-do)
                       (lambda (&rest args) (setq merge-do-args args))))
              (agent-repl-workspace-merge-current-into-source)
              (should-not switch-called)
              (should (equal merge-do-args (list "wt-ws" tmpdir t t)))))
        (delete-directory tmpdir t)))))

(ert-deftest agent-repl-test-merge-into-source-falls-back-to-master-when-recorded-dir-gone ()
  "If :source-ws-dir refers to a missing directory, fall back to master worktree path.
Assertion is on merge-do's TARGET-DIR arg (post-handler-routing, silent=t skips
the `switch-to-project' call)."
  (agent-repl-test--with-clean-state
    (let ((merge-do-args :unset))
      (agent-repl--ws-put "wt-ws" :project-dir "/tmp/wt-dir/")
      (agent-repl--ws-put "wt-ws" :source-ws-dir "/no/such/dir/")
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "wt-ws"))
                ((symbol-function 'agent-repl--master-worktree-path)
                 (lambda (_root) "/tmp/master-fallback/"))
                ((symbol-function 'agent-repl--cherry-pick-in-progress-p)
                 (lambda (_) nil))
                ((symbol-function 'agent-repl--assert-clean-worktree)
                 (lambda (&rest _) nil))
                ((symbol-function 'agent-repl-switch-to-project) #'ignore)
                ((symbol-function 'agent-repl--workspace-merge-do)
                 (lambda (&rest args) (setq merge-do-args args))))
        (agent-repl-workspace-merge-current-into-source)
        (should (equal merge-do-args (list "wt-ws" "/tmp/master-fallback/" t t)))))))

(ert-deftest agent-repl-test-merge-into-source-falls-back-to-master-when-no-recorded-source ()
  "Legacy workspace with no :source-ws-dir falls back to master worktree path.
Assertion is on merge-do's TARGET-DIR arg (post-handler-routing, silent=t skips
the `switch-to-project' call)."
  (agent-repl-test--with-clean-state
    (let ((merge-do-args :unset))
      (agent-repl--ws-put "wt-ws" :project-dir "/tmp/wt-dir/")
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "wt-ws"))
                ((symbol-function 'agent-repl--master-worktree-path)
                 (lambda (_root) "/tmp/master-fallback/"))
                ((symbol-function 'agent-repl--cherry-pick-in-progress-p)
                 (lambda (_) nil))
                ((symbol-function 'agent-repl--assert-clean-worktree)
                 (lambda (&rest _) nil))
                ((symbol-function 'agent-repl-switch-to-project) #'ignore)
                ((symbol-function 'agent-repl--workspace-merge-do)
                 (lambda (&rest args) (setq merge-do-args args))))
        (agent-repl-workspace-merge-current-into-source)
        (should (equal merge-do-args (list "wt-ws" "/tmp/master-fallback/" t t)))))))

(ert-deftest agent-repl-test-merge-into-source-silent-skips-switch-to-project ()
  "When SILENT is non-nil, --workspace-merge-into-source must NOT call
`agent-repl-switch-to-project'.  This is the path used by
`agent-repl--handle-merge-command' for skill-invoked merges so that
background-triggered /workspace-merge does not yank the user's focus."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-merge-silent-no-switch-" t))
          (switch-called nil)
          (merge-do-args :unset))
      (unwind-protect
          (progn
            (agent-repl--ws-put "wt-ws" :project-dir "/tmp/wt-dir/")
            (agent-repl--ws-put "wt-ws" :source-ws-dir tmpdir)
            (cl-letf (((symbol-function '+workspace-current-name) (lambda () "other-ws"))
                      ((symbol-function 'agent-repl--master-worktree-path)
                       (lambda (_root) nil))
                      ((symbol-function 'agent-repl--cherry-pick-in-progress-p)
                       (lambda (_) nil))
                      ((symbol-function 'agent-repl--assert-clean-worktree)
                       (lambda (&rest _) nil))
                      ((symbol-function 'agent-repl-switch-to-project)
                       (lambda (&rest _) (setq switch-called t)))
                      ((symbol-function 'agent-repl--git-branch-of-dir)
                       (lambda (_) nil))
                      ((symbol-function 'agent-repl--workspace-merge-do)
                       (lambda (&rest args) (setq merge-do-args args))))
              (agent-repl--workspace-merge-into-source "wt-ws" t)
              (should-not switch-called)
              (should (equal merge-do-args (list "wt-ws" tmpdir t nil)))))
        (delete-directory tmpdir t)))))

(ert-deftest agent-repl-test-merge-into-source-errors-when-no-source-and-no-master ()
  "user-errors when neither a recorded source nor a master worktree can be found."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "wt-ws" :project-dir "/tmp/wt-dir/")
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "wt-ws"))
              ((symbol-function 'agent-repl--master-worktree-path)
               (lambda (_root) nil)))
      (should-error (agent-repl-workspace-merge-current-into-source)
                    :type 'user-error))))

(ert-deftest agent-repl-test-merge-into-source-errors-when-already-on-source ()
  "user-errors when the resolved target equals the current workspace's dir."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-merge-self-" t)))
      (unwind-protect
          (progn
            (agent-repl--ws-put "self-ws" :project-dir tmpdir)
            (agent-repl--ws-put "self-ws" :source-ws-dir tmpdir)
            (agent-repl-test--with-mocked-git-probes
              (cl-letf (((symbol-function '+workspace-current-name) (lambda () "self-ws"))
                        ((symbol-function 'agent-repl--master-worktree-path)
                         (lambda (_root) nil))
                        ((symbol-function 'agent-repl--assert-clean-worktree)
                         (lambda (&rest _) nil)))
                (should-error (agent-repl-workspace-merge-current-into-source)
                              :type 'user-error))))
        (delete-directory tmpdir t)))))

(ert-deftest agent-repl-test-workspace-merge-into-source-accepts-explicit-ws ()
  "workspace-merge-into-source operates on the named workspace, not (+workspace-current-name)."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-merge-explicit-" t))
          (target-arg :unset)
          (merge-do-args :unset))
      (unwind-protect
          (progn
            (agent-repl--ws-put "named-ws" :project-dir "/tmp/named-dir/")
            (agent-repl--ws-put "named-ws" :source-ws-dir tmpdir)
            (cl-letf (((symbol-function '+workspace-current-name) (lambda () "other-ws"))
                      ((symbol-function 'agent-repl--master-worktree-path)
                       (lambda (_root) nil))
                      ((symbol-function 'agent-repl--cherry-pick-in-progress-p)
                       (lambda (_) nil))
                      ((symbol-function 'agent-repl--assert-clean-worktree)
                       (lambda (&rest _) nil))
                      ((symbol-function 'agent-repl-switch-to-project)
                       (lambda (target) (setq target-arg target)))
                      ((symbol-function 'agent-repl--git-branch-of-dir)
                       (lambda (_) nil))
                      ((symbol-function 'agent-repl--workspace-merge-do)
                       (lambda (&rest args) (setq merge-do-args args))))
              (agent-repl--workspace-merge-into-source "named-ws")
              (should (equal target-arg tmpdir))
              (should (equal merge-do-args (list "named-ws" tmpdir nil nil)))))
        (delete-directory tmpdir t)))))

(ert-deftest agent-repl-test-workspace-merge-into-source-normalizes-branchy-name ()
  "Branch-style names like \"DWC/feature-one\" are normalized to the bare workspace name."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-merge-branchy-" t))
          (merge-do-args :unset))
      (unwind-protect
          (progn
            (agent-repl--ws-put "feature-one" :project-dir "/tmp/feature-one/")
            (agent-repl--ws-put "feature-one" :source-ws-dir tmpdir)
            (cl-letf (((symbol-function '+workspace-current-name) (lambda () "other-ws"))
                      ((symbol-function 'agent-repl--master-worktree-path)
                       (lambda (_root) nil))
                      ((symbol-function 'agent-repl--cherry-pick-in-progress-p)
                       (lambda (_) nil))
                      ((symbol-function 'agent-repl--assert-clean-worktree)
                       (lambda (&rest _) nil))
                      ((symbol-function 'agent-repl-switch-to-project) #'ignore)
                      ((symbol-function 'agent-repl--git-branch-of-dir)
                       (lambda (_) nil))
                      ((symbol-function 'agent-repl--workspace-merge-do)
                       (lambda (&rest args) (setq merge-do-args args))))
              (agent-repl--workspace-merge-into-source "DWC/feature-one")
              (should (equal (car merge-do-args) "feature-one"))))
        (delete-directory tmpdir t)))))

(ert-deftest agent-repl-test-workspace-merge-into-source-errors-on-unknown-ws ()
  "user-errors when the named workspace is not registered in the workspaces hash."
  (agent-repl-test--with-clean-state
    (should-error (agent-repl--workspace-merge-into-source "no-such-ws")
                  :type 'user-error)))

;;;; ---- Tests: resolve-merge-into-source-target ----

(ert-deftest agent-repl-test-resolve-merge-target-nil-parent ()
  "Returns nil when parent-dir is nil."
  (should (null (agent-repl--resolve-merge-into-source-target nil "/m/"))))

(ert-deftest agent-repl-test-resolve-merge-target-nil-master ()
  "Returns parent unchanged when master-dir is nil."
  (should (equal (agent-repl--resolve-merge-into-source-target "/p/" nil)
                 "/p/")))

(ert-deftest agent-repl-test-resolve-merge-target-parent-is-master ()
  "Returns parent unchanged when parent-dir == master-dir."
  (let ((tmp (make-temp-file "test-resolve-master-" t)))
    (unwind-protect
        (should (equal (agent-repl--resolve-merge-into-source-target tmp tmp)
                       tmp))
      (delete-directory tmp t))))

(ert-deftest agent-repl-test-resolve-merge-target-parent-already-merged ()
  "Returns master-dir when parent != master and parent's branch is in master."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--branch-merged-into-p)
               (lambda (_s _t) t)))
      (should (equal (agent-repl--resolve-merge-into-source-target "/p/" "/m/")
                     "/m/")))))

(ert-deftest agent-repl-test-resolve-merge-target-parent-not-merged ()
  "Returns parent-dir when parent's branch is not yet in master."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--branch-merged-into-p)
               (lambda (_s _t) nil)))
      (should (equal (agent-repl--resolve-merge-into-source-target "/p/" "/m/")
                     "/p/")))))

(ert-deftest agent-repl-test-resolve-merge-target-walks-merged-grandparent ()
  "Walks the source-ws-dir chain when intermediate ancestors are merged.
parent merged into grandparent, grandparent merged into master ⇒ returns master."
  (agent-repl-test--with-clean-state
    (puthash "p-ws" '(:project-dir "/p/" :source-ws-dir "/g/")
             agent-repl--workspaces)
    (puthash "g-ws" '(:project-dir "/g/" :source-ws-dir nil)
             agent-repl--workspaces)
    (cl-letf (((symbol-function 'agent-repl--branch-merged-into-p)
               (lambda (_s _t) t))
              ((symbol-function 'file-directory-p) (lambda (_) t))
              ((symbol-function 'agent-repl--path-canonical) #'identity))
      (should (equal (agent-repl--resolve-merge-into-source-target "/p/" "/m/")
                     "/m/")))))

(ert-deftest agent-repl-test-resolve-merge-target-stops-at-unmerged-grandparent ()
  "When parent is merged but grandparent is not, returns the grandparent dir."
  (agent-repl-test--with-clean-state
    (puthash "p-ws" '(:project-dir "/p/" :source-ws-dir "/g/")
             agent-repl--workspaces)
    (puthash "g-ws" '(:project-dir "/g/" :source-ws-dir nil)
             agent-repl--workspaces)
    (let ((calls 0))
      (cl-letf (((symbol-function 'agent-repl--branch-merged-into-p)
                 (lambda (_s _t)
                   (setq calls (1+ calls))
                   ;; First call (p→g) merged; second call (g→m) not.
                   (= calls 1)))
                ((symbol-function 'file-directory-p) (lambda (_) t))
                ((symbol-function 'agent-repl--path-canonical) #'identity))
        (should (equal (agent-repl--resolve-merge-into-source-target "/p/" "/m/")
                       "/g/"))))))

(ert-deftest agent-repl-test-resolve-merge-target-cycle-cap ()
  "Self-referential `:source-ws-dir' chain terminates at the depth cap.
Defense-in-depth — should never happen in practice, but the resolver
must not infinite-loop if it does."
  (agent-repl-test--with-clean-state
    (puthash "p-ws" '(:project-dir "/p/" :source-ws-dir "/p/")
             agent-repl--workspaces)
    (let ((agent-repl-merge-resolve-max-depth 4))
      (cl-letf (((symbol-function 'agent-repl--branch-merged-into-p)
                 (lambda (_s _t) t))
                ((symbol-function 'file-directory-p) (lambda (_) t))
                ((symbol-function 'agent-repl--path-canonical) #'identity))
        ;; Should return without hanging; exact value is the candidate
        ;; held when the depth cap fires.
        (should (agent-repl--resolve-merge-into-source-target "/p/" "/m/"))))))

;;;; ---- Tests: branch-merged-into-p (generalized predicate) ----

(ert-deftest agent-repl-test-branch-merged-into-p-nil-args ()
  "Returns nil when either dir is nil."
  (should (null (agent-repl--branch-merged-into-p nil "/m/")))
  (should (null (agent-repl--branch-merged-into-p "/p/" nil))))

(ert-deftest agent-repl-test-branch-merged-into-p-same-branch ()
  "Returns nil when source and target have the same current branch."
  (cl-letf (((symbol-function 'agent-repl--git-string-quiet)
             (lambda (&rest _) "main")))
    (should-not (agent-repl--branch-merged-into-p "/a/" "/b/"))))

(ert-deftest agent-repl-test-merge-base-ancestor-args-bails-on-same-sha ()
  "Returns nil when both branches resolve to the same tip SHA.
A freshly created child worktree starts at its parent's HEAD commit, so
the two branches are commit-identical even though their names differ —
the ancestry check would trivially succeed and mis-bucket the empty
child as merged.  The helper must bail before that point."
  (let ((same-sha "abc123def456abc123def456abc123def4567890"))
    (cl-letf (((symbol-function 'agent-repl--git-string-quiet)
               (lambda (&rest args)
                 (pcase args
                   ;; Distinct branch names but identical tip SHAs.
                   (`("-C" "/tmp/child-wt" "rev-parse" "--abbrev-ref" "HEAD") "child")
                   (`("-C" "/tmp/repo" "rev-parse" "--abbrev-ref" "HEAD") "master")
                   (`("-C" "/tmp/child-wt" "rev-parse" "HEAD") same-sha)
                   (`("-C" "/tmp/repo" "rev-parse" "HEAD") same-sha)
                   (_ (error "unmocked git-string-quiet args: %S" args))))))
      (should (null (agent-repl--merge-base-ancestor-args
                     "/tmp/child-wt" "/tmp/repo"))))))

;;;; ---- Tests: branch-merged async cache ----

(ert-deftest agent-repl-test-ws-merge-parent-dir-uses-source-when-live ()
  "`--ws-merge-parent-dir' returns `:source-ws-dir' when it is a live directory."
  (agent-repl-test--with-clean-state
    (let ((tmp (make-temp-file "merge-parent-" t)))
      (unwind-protect
          (progn
            (agent-repl--ws-put "ws" :project-dir "/anything/")
            (agent-repl--ws-put "ws" :source-ws-dir tmp)
            (should (equal (agent-repl--ws-merge-parent-dir "ws") tmp)))
        (delete-directory tmp t)))))

(ert-deftest agent-repl-test-ws-merge-parent-dir-falls-back-to-master ()
  "Falls back to the master worktree path when `:source-ws-dir' is missing."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws" :project-dir "/some/repo/")
    (cl-letf (((symbol-function 'agent-repl--ws-dir)
               (lambda (_) "/some/repo/"))
              ((symbol-function 'agent-repl--master-worktree-path)
               (lambda (_) "/master/dir/")))
      (should (equal (agent-repl--ws-merge-parent-dir "ws") "/master/dir/")))))

(ert-deftest agent-repl-test-ws-merge-parent-dir-caches-positive-result ()
  "Second call returns the cached path without re-invoking master-worktree-path."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws" :project-dir "/some/repo/")
    (let ((call-count 0))
      (cl-letf (((symbol-function 'agent-repl--ws-dir)
                 (lambda (_) "/some/repo/"))
                ((symbol-function 'agent-repl--master-worktree-path)
                 (lambda (_) (cl-incf call-count) "/master/dir/")))
        (agent-repl--ws-merge-parent-dir "ws")
        (agent-repl--ws-merge-parent-dir "ws")
        (should (= call-count 1))))))

(ert-deftest agent-repl-test-ws-merge-parent-dir-caches-negative-result ()
  "Nil resolution is cached as `unresolved' so master-worktree-path is not re-shelled.
Regression: with no `:source-ws-dir' and a nil-returning master fallback,
the prior implementation skipped the cache write and re-shelled
`git worktree list --porcelain' on every poll tick — the dominant cost
on workspace switch in repos with many worktrees."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws" :project-dir "/some/repo/")
    (let ((call-count 0))
      (cl-letf (((symbol-function 'agent-repl--ws-dir)
                 (lambda (_) "/some/repo/"))
                ((symbol-function 'agent-repl--master-worktree-path)
                 (lambda (_) (cl-incf call-count) nil)))
        (should (null (agent-repl--ws-merge-parent-dir "ws")))
        (should (null (agent-repl--ws-merge-parent-dir "ws")))
        (should (= call-count 1))
        (should (eq (agent-repl--ws-get "ws" :merge-parent-dir) 'unresolved))))))

(ert-deftest agent-repl-test-branch-merge-sentinel-merged-on-zero-exit ()
  "Sentinel records `merged' when git merge-base exits 0."
  (agent-repl-test--with-clean-state
    (let ((proc (make-pipe-process :name "test-merge" :buffer nil :noquery t)))
      (cl-letf (((symbol-function 'process-live-p) (lambda (_) nil))
                ((symbol-function 'process-exit-status) (lambda (_) 0)))
        (agent-repl--branch-merge-sentinel "ws" proc "finished\n")
        (should (eq (agent-repl--ws-get "ws" :branch-merged) 'merged))
        (should (null (agent-repl--ws-get "ws" :merge-proc))))
      (delete-process proc))))

(ert-deftest agent-repl-test-branch-merge-sentinel-not-merged-on-one-exit ()
  "Sentinel records `not-merged' when git merge-base exits 1."
  (agent-repl-test--with-clean-state
    (let ((proc (make-pipe-process :name "test-merge" :buffer nil :noquery t)))
      (cl-letf (((symbol-function 'process-live-p) (lambda (_) nil))
                ((symbol-function 'process-exit-status) (lambda (_) 1)))
        (agent-repl--branch-merge-sentinel "ws" proc "finished\n")
        (should (eq (agent-repl--ws-get "ws" :branch-merged) 'not-merged)))
      (delete-process proc))))

(ert-deftest agent-repl-test-branch-merge-sentinel-leaves-cache-on-error ()
  "Unexpected exit codes leave `:branch-merged' untouched."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws" :branch-merged 'merged)
    (let ((proc (make-pipe-process :name "test-merge" :buffer nil :noquery t)))
      (cl-letf (((symbol-function 'process-live-p) (lambda (_) nil))
                ((symbol-function 'process-exit-status) (lambda (_) 128)))
        (agent-repl--branch-merge-sentinel "ws" proc "fatal\n")
        (should (eq (agent-repl--ws-get "ws" :branch-merged) 'merged)))
      (delete-process proc))))

(ert-deftest agent-repl-test-branch-merge-check-in-progress-detects-live-proc ()
  "`--branch-merge-check-in-progress-p' returns non-nil when `:merge-proc' is alive."
  (agent-repl-test--with-clean-state
    (let ((proc (make-pipe-process :name "test-live" :buffer nil :noquery t)))
      (agent-repl--ws-put "ws" :merge-proc proc)
      (cl-letf (((symbol-function 'process-live-p) (lambda (p) (eq p proc))))
        (should (agent-repl--branch-merge-check-in-progress-p "ws")))
      (delete-process proc))))

(ert-deftest agent-repl-test-async-refresh-branch-merged-skips-when-in-progress ()
  "No new process is spawned when one is already live for the workspace.
Stubs the registered wrapper `agent-repl--make-process-git' rather
than raw `make-process' — production code now routes async git
invocations through the wrapper, and the runtime guards installed by
test-helpers.el would otherwise fire UNMOCKED if the production code
were reached without the wrapper stub."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws" :project-dir "/some/")
    ;; Pre-seed the merge-parent-dir cache so resolution succeeds without
    ;; shelling out through `--master-worktree-path' / `--git-string-quiet'.
    (agent-repl--ws-put "ws" :merge-parent-dir "/some-parent/")
    (let ((spawned nil))
      (cl-letf (((symbol-function 'agent-repl--branch-merge-check-in-progress-p)
                 (lambda (_) t))
                ((symbol-function 'agent-repl--make-process-git)
                 (lambda (&rest _) (setq spawned t) :proc)))
        (agent-repl--async-refresh-branch-merged "ws")
        (should-not spawned)))))

;;;; ---- Tests: finalize-worktree-workspace branch caching ----

(ert-deftest agent-repl-test-finalize-worktree-workspace-caches-branch-name ()
  "Finalize stores the worktree's current branch as :branch-name on the plist."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--register-projectile-project)
               (lambda (&rest _) nil))
              ((symbol-function '+workspace-new) (lambda (_ws) nil))
              ((symbol-function 'agent-repl--setup-worktree-session)
               (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--path-canonical) #'identity)
              ((symbol-function 'agent-repl--git-string-quiet)
               (lambda (&rest args)
                 (pcase args
                   (`("-C" "/tmp/new-wt" "rev-parse" "--abbrev-ref" "HEAD") "my-branch")
                   (_ "")))))
      (agent-repl--finalize-worktree-workspace
       "/tmp/new-wt" "new-ws" nil nil nil nil nil nil)
      (should (equal (agent-repl--ws-get "new-ws" :branch-name) "my-branch")))))

(ert-deftest agent-repl-test-finalize-worktree-workspace-caches-parent-branch-name ()
  "Finalize stores the parent dir's branch as :parent-branch-name when source-dir given."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--register-projectile-project)
               (lambda (&rest _) nil))
              ((symbol-function '+workspace-new) (lambda (_ws) nil))
              ((symbol-function 'agent-repl--setup-worktree-session)
               (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--path-canonical) #'identity)
              ((symbol-function 'agent-repl--git-string-quiet)
               (lambda (&rest args)
                 (pcase args
                   (`("-C" "/tmp/new-wt" "rev-parse" "--abbrev-ref" "HEAD") "child")
                   (`("-C" "/tmp/source/" "rev-parse" "--abbrev-ref" "HEAD") "master")
                   (_ "")))))
      (agent-repl--finalize-worktree-workspace
       "/tmp/new-wt" "new-ws" nil nil nil nil nil "/tmp/source/")
      (should (equal (agent-repl--ws-get "new-ws" :branch-name) "child"))
      (should (equal (agent-repl--ws-get "new-ws" :parent-branch-name) "master")))))

(ert-deftest agent-repl-test-finalize-worktree-workspace-skips-parent-branch-when-nil ()
  "No :parent-branch-name set when source-dir is nil."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--register-projectile-project)
               (lambda (&rest _) nil))
              ((symbol-function '+workspace-new) (lambda (_ws) nil))
              ((symbol-function 'agent-repl--setup-worktree-session)
               (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--path-canonical) #'identity)
              ((symbol-function 'agent-repl--git-string-quiet)
               (lambda (&rest args)
                 (pcase args
                   (`("-C" "/tmp/new-wt" "rev-parse" "--abbrev-ref" "HEAD") "my-branch")
                   (_ "")))))
      (agent-repl--finalize-worktree-workspace
       "/tmp/new-wt" "new-ws" nil nil nil nil nil nil)
      (should-not (agent-repl--ws-get "new-ws" :parent-branch-name)))))

;;;; ---- Tests: merge-base-ancestor-args branch hints ----

(ert-deftest agent-repl-test-merge-base-ancestor-args-uses-branch-hints ()
  "When valid branch hints are supplied, skips the rev-parse --abbrev-ref calls."
  (let ((git-calls nil))
    (cl-letf (((symbol-function 'agent-repl--git-string-quiet)
               (lambda (&rest args)
                 (push args git-calls)
                 (pcase args
                   ;; SHA calls still expected
                   (`("-C" "/tmp/child" "rev-parse" "HEAD") "aaa111")
                   (`("-C" "/tmp/parent" "rev-parse" "HEAD") "bbb222")
                   (_ (error "unexpected git call: %S" args))))))
      (let ((result (agent-repl--merge-base-ancestor-args
                     "/tmp/child" "/tmp/parent" "child-branch" "master")))
        (should (equal result '("child-branch" . "master")))
        ;; Only the two SHA calls fired; no branch-name rev-parse calls
        (should (= (length git-calls) 2))
        (should (cl-every (lambda (c) (member "HEAD" c)) git-calls))))))

(ert-deftest agent-repl-test-merge-base-ancestor-args-falls-back-without-hints ()
  "Without branch hints, all four rev-parse calls still fire."
  (let ((git-calls nil))
    (cl-letf (((symbol-function 'agent-repl--git-string-quiet)
               (lambda (&rest args)
                 (push args git-calls)
                 (pcase args
                   (`("-C" "/tmp/child" "rev-parse" "--abbrev-ref" "HEAD") "child")
                   (`("-C" "/tmp/parent" "rev-parse" "--abbrev-ref" "HEAD") "master")
                   (`("-C" "/tmp/child" "rev-parse" "HEAD") "aaa111")
                   (`("-C" "/tmp/parent" "rev-parse" "HEAD") "bbb222")
                   (_ (error "unexpected git call: %S" args))))))
      (agent-repl--merge-base-ancestor-args "/tmp/child" "/tmp/parent")
      (should (= (length git-calls) 4)))))

;;;; ---- Tests: async-refresh-branch-merged passes cached branches ----

(ert-deftest agent-repl-test-async-refresh-branch-merged-passes-cached-branches ()
  "Passes :branch-name and :parent-branch-name plist values as hints to ancestor-args."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws" :project-dir "/some/")
    (agent-repl--ws-put "ws" :branch-name "feature")
    (agent-repl--ws-put "ws" :parent-branch-name "master")
    (let ((captured-hints nil))
      (cl-letf (((symbol-function 'agent-repl--ws-dir)
                 (lambda (_) "/some/"))
                ((symbol-function 'agent-repl--ws-merge-parent-dir)
                 (lambda (_) "/parent/"))
                ((symbol-function 'agent-repl--branch-merge-check-in-progress-p)
                 (lambda (_) nil))
                ((symbol-function 'agent-repl--merge-base-ancestor-args)
                 (lambda (_src _tgt sb tb)
                   (setq captured-hints (list sb tb))
                   (cons sb tb)))
                ((symbol-function 'agent-repl--make-process-git)
                 (lambda (&rest _) :proc)))
        (agent-repl--async-refresh-branch-merged "ws")
        (should (equal captured-hints '("feature" "master")))))))

;;;; ---- Tests: ws-name-for-dir (reverse lookup) ----

(ert-deftest agent-repl-test-ws-name-for-dir-nil-arg ()
  "Returns nil for nil DIR."
  (should (null (agent-repl--ws-name-for-dir nil))))

(ert-deftest agent-repl-test-ws-name-for-dir-finds-match ()
  "Returns the workspace name whose `:project-dir' canonicalizes to DIR."
  (agent-repl-test--with-clean-state
    (puthash "alpha" '(:project-dir "/repo-a/") agent-repl--workspaces)
    (puthash "beta"  '(:project-dir "/repo-b/") agent-repl--workspaces)
    (cl-letf (((symbol-function 'agent-repl--path-canonical) #'identity))
      (should (equal (agent-repl--ws-name-for-dir "/repo-b/") "beta")))))

(ert-deftest agent-repl-test-ws-name-for-dir-returns-nil-on-miss ()
  "Returns nil when no workspace's `:project-dir' matches DIR."
  (agent-repl-test--with-clean-state
    (puthash "alpha" '(:project-dir "/repo-a/") agent-repl--workspaces)
    (cl-letf (((symbol-function 'agent-repl--path-canonical) #'identity))
      (should (null (agent-repl--ws-name-for-dir "/missing/"))))))

;;;; ---- Tests: ws-merged-p ----

(ert-deftest agent-repl-test-ws-merged-p-true-when-cached-merged ()
  "Returns t when WS's `:branch-merged' cache is `merged'."
  (agent-repl-test--with-clean-state
    (puthash "ws" '(:branch-merged merged) agent-repl--workspaces)
    (should (agent-repl--ws-merged-p "ws"))))

(ert-deftest agent-repl-test-ws-merged-p-nil-when-cached-not-merged ()
  "Returns nil when WS's `:branch-merged' cache is `not-merged'."
  (agent-repl-test--with-clean-state
    (puthash "ws" '(:branch-merged not-merged) agent-repl--workspaces)
    (should-not (agent-repl--ws-merged-p "ws"))))

(ert-deftest agent-repl-test-ws-merged-p-nil-when-cache-absent ()
  "Returns nil on cache miss — drawer should treat unknown as :main."
  (agent-repl-test--with-clean-state
    (puthash "ws" '() agent-repl--workspaces)
    (should-not (agent-repl--ws-merged-p "ws"))))

;;;; ---- Tests: merge-into-source re-routes when parent merged into master ----

(ert-deftest agent-repl-test-merge-into-source-reroutes-to-master-when-parent-already-merged ()
  "When parent worktree's branch is already in master, merge-do receives master-dir
as the resolved target.  The interactive entry point routes through the
cherry-pick handler (silent=t auto-resolve=t), so the rerouting decision is
visible in merge-do's TARGET-DIR arg rather than in `switch-to-project'."
  (agent-repl-test--with-clean-state
    (let ((parent-dir (make-temp-file "test-reroute-parent-" t))
          (master-dir (make-temp-file "test-reroute-master-" t))
          (merge-do-args :unset))
      (unwind-protect
          (progn
            (agent-repl--ws-put "wt-ws" :project-dir "/tmp/wt-dir/")
            (agent-repl--ws-put "wt-ws" :source-ws-dir parent-dir)
            (cl-letf (((symbol-function '+workspace-current-name) (lambda () "wt-ws"))
                      ((symbol-function 'agent-repl--master-worktree-path)
                       (lambda (_root) master-dir))
                      ((symbol-function 'agent-repl--main-worktree-path)
                       (lambda (dir) dir))
                      ((symbol-function 'agent-repl--branch-merged-into-p)
                       (lambda (_s _t) t))
                      ((symbol-function 'agent-repl--cherry-pick-in-progress-p)
                       (lambda (_) nil))
                      ((symbol-function 'agent-repl--assert-clean-worktree)
                       (lambda (&rest _) nil))
                      ((symbol-function 'agent-repl-switch-to-project) #'ignore)
                      ((symbol-function 'agent-repl--git-branch-of-dir)
                       (lambda (_) nil))
                      ((symbol-function 'agent-repl--workspace-merge-do)
                       (lambda (&rest args) (setq merge-do-args args))))
              (agent-repl-workspace-merge-current-into-source)
              (should (equal merge-do-args (list "wt-ws" master-dir t t)))))
        (delete-directory parent-dir t)
        (delete-directory master-dir t)))))

(ert-deftest agent-repl-test-merge-into-source-stays-on-parent-when-not-yet-merged ()
  "When parent worktree's branch has unmerged commits, keep parent as the target.
Routes through the cherry-pick handler (silent=t auto-resolve=t); the
target-dir decision shows up in merge-do's args."
  (agent-repl-test--with-clean-state
    (let ((parent-dir (make-temp-file "test-stay-parent-" t))
          (master-dir (make-temp-file "test-stay-master-" t))
          (merge-do-args :unset))
      (unwind-protect
          (progn
            (agent-repl--ws-put "wt-ws" :project-dir "/tmp/wt-dir/")
            (agent-repl--ws-put "wt-ws" :source-ws-dir parent-dir)
            (cl-letf (((symbol-function '+workspace-current-name) (lambda () "wt-ws"))
                      ((symbol-function 'agent-repl--master-worktree-path)
                       (lambda (_root) master-dir))
                      ((symbol-function 'agent-repl--main-worktree-path)
                       (lambda (dir) dir))
                      ((symbol-function 'agent-repl--branch-merged-into-p)
                       (lambda (_s _t) nil))
                      ((symbol-function 'agent-repl--cherry-pick-in-progress-p)
                       (lambda (_) nil))
                      ((symbol-function 'agent-repl--assert-clean-worktree)
                       (lambda (&rest _) nil))
                      ((symbol-function 'agent-repl-switch-to-project) #'ignore)
                      ((symbol-function 'agent-repl--git-branch-of-dir)
                       (lambda (_) nil))
                      ((symbol-function 'agent-repl--workspace-merge-do)
                       (lambda (&rest args) (setq merge-do-args args))))
              (agent-repl-workspace-merge-current-into-source)
              (should (equal merge-do-args (list "wt-ws" parent-dir t t)))))
        (delete-directory parent-dir t)
        (delete-directory master-dir t)))))

;;;; ---- Tests: workspace-merge-do project-root override ----

(ert-deftest agent-repl-test-workspace-merge-do-uses-project-root-override ()
  "When PROJECT-ROOT-OVERRIDE is non-nil, cherry-pick lands there (not at current ws's dir)."
  (let ((cherry-pick-dir nil))
    (cl-letf* (((symbol-function '+workspace-current-name) (lambda () "current"))
               ((symbol-function 'agent-repl--workspace-branch) (lambda (_ws) "branch-x"))
               ((symbol-function 'agent-repl--ws-dir) (lambda (_ws) "/should/not/be/used/"))
               ((symbol-function 'agent-repl--git-branch-exists-p) (lambda (_dir _br) t))
               ((symbol-function 'agent-repl--cherry-pick-base) (lambda (_dir _br) "abc123"))
               ((symbol-function 'agent-repl--cherry-pick-commits)
                (lambda (dir _ws _base _br &optional _auto _silent) (setq cherry-pick-dir dir)))
               ((symbol-function 'agent-repl--tag-merge-completion) #'ignore)
               ((symbol-function 'agent-repl-drawer--refresh-detail-cache) #'ignore)
               ((symbol-function 'agent-repl--nuke-one-workspace) (lambda (&rest _) nil))
               ((symbol-function 'load-file) #'ignore))
      (agent-repl--workspace-merge-do "other-ws" "/explicit/target/")
      (should (equal cherry-pick-dir "/explicit/target/")))))

;;;; ---- Tests: remove-doom-dashboard ----

(ert-deftest agent-repl-test-remove-doom-dashboard-removes-existing-buffer ()
  "Dashboard buffer is removed from the workspace when it exists."
  (let ((removed nil)
        (+doom-dashboard-buffer-name "*doom*"))
    (agent-repl-test--with-temp-buffer "*doom*"
      (cl-letf (((symbol-function 'persp-remove-buffer)
                 (lambda (buf) (setq removed buf)))
                ((symbol-function '+workspace-current-name) (lambda () "test-ws")))
        (agent-repl--remove-doom-dashboard)
        (should removed)
        (should (equal (buffer-name removed) "*doom*"))))))

(ert-deftest agent-repl-test-remove-doom-dashboard-noop-when-no-buffer ()
  "No error when the dashboard buffer does not exist."
  (let ((removed nil)
        (+doom-dashboard-buffer-name "*doom-nonexistent-xyz*"))
    (cl-letf (((symbol-function 'persp-remove-buffer)
               (lambda (buf) (setq removed buf)))
              ((symbol-function '+workspace-current-name) (lambda () "test-ws")))
      (agent-repl--remove-doom-dashboard)
      (should-not removed))))

(ert-deftest agent-repl-test-remove-doom-dashboard-noop-when-unbound ()
  "No error when `+doom-dashboard-buffer-name' is unbound."
  (let ((removed nil))
    (cl-letf (((symbol-function 'persp-remove-buffer)
               (lambda (buf) (setq removed buf)))
              ((symbol-function '+workspace-current-name) (lambda () "test-ws")))
      ;; Temporarily unbind the variable
      (let ((had-binding (boundp '+doom-dashboard-buffer-name)))
        (when had-binding (makunbound '+doom-dashboard-buffer-name))
        (unwind-protect
            (progn
              (agent-repl--remove-doom-dashboard)
              (should-not removed))
          (when had-binding
            (setq +doom-dashboard-buffer-name "*doom*")))))))

(ert-deftest agent-repl-test-worktree-callback-only-switches ()
  "worktree-creation-switch-callback only switches workspace.
Magit-status and dashboard removal are handled by finalize-worktree-workspace."
  (let ((call-order nil))
    (cl-letf (((symbol-function 'agent-repl--switch-to-workspace)
               (lambda (_ws) (push 'switch call-order)))
              ((symbol-function 'agent-repl--flash-current-tab) #'ignore)
              ((symbol-function '+workspace-current-name) (lambda () "test-ws")))
      (agent-repl--worktree-creation-switch-callback "/tmp/fake" "test-ws")
      (should (equal (reverse call-order) '(switch))))))

(ert-deftest agent-repl-test-worktree-callback-flashes-destination-tab ()
  "worktree-creation-switch-callback flashes the destination tab.
Symmetric with the project-picker (`SPC p p') and reopen paths so every
identity-based jump pulses uniformly."
  (let ((flashed nil))
    (cl-letf (((symbol-function 'agent-repl--switch-to-workspace) #'ignore)
              ((symbol-function 'agent-repl--flash-current-tab)
               (lambda () (setq flashed t)))
              ((symbol-function '+workspace-current-name) (lambda () "test-ws")))
      (agent-repl--worktree-creation-switch-callback "/tmp/fake" "test-ws")
      (should flashed))))

;;;; ---- Tests: agent-repl-jump-to-workspace ----

(ert-deftest agent-repl-test-jump-to-workspace-delegates-to-switch ()
  "agent-repl-jump-to-workspace forwards WS to the raw switch primitive."
  (let ((switched-ws nil))
    (cl-letf (((symbol-function 'agent-repl--switch-to-workspace)
               (lambda (ws) (setq switched-ws ws)))
              ((symbol-function 'agent-repl--flash-current-tab) #'ignore))
      (agent-repl-jump-to-workspace "target-ws")
      (should (equal switched-ws "target-ws")))))

(ert-deftest agent-repl-test-jump-to-workspace-flashes-by-default ()
  "Without NO-FLASH, the jumper pulses the destination tab — flash is inherent."
  (let ((flashed nil))
    (cl-letf (((symbol-function 'agent-repl--switch-to-workspace) #'ignore)
              ((symbol-function 'agent-repl--flash-current-tab)
               (lambda () (setq flashed t))))
      (agent-repl-jump-to-workspace "target-ws")
      (should flashed))))

(ert-deftest agent-repl-test-jump-to-workspace-no-flash-suppresses-pulse ()
  "Passing NO-FLASH non-nil skips the pulse — escape hatch for bulk callers."
  (let ((flashed nil))
    (cl-letf (((symbol-function 'agent-repl--switch-to-workspace) #'ignore)
              ((symbol-function 'agent-repl--flash-current-tab)
               (lambda () (setq flashed t))))
      (agent-repl-jump-to-workspace "target-ws" t)
      (should-not flashed))))

(ert-deftest agent-repl-test-new-workspace-removes-dashboard ()
  "new-workspace calls remove-doom-dashboard after magit."
  (let ((call-order nil)
        (+doom-dashboard-buffer-name "*doom*"))
    (agent-repl-test--with-temp-buffer "*doom*"
      (agent-repl-test--with-clean-state
        (cl-letf (((symbol-function 'agent-repl--git-root)
                   (lambda () "/tmp/fake-root"))
                  ((symbol-function '+workspace/new)
                   (lambda (&rest _) (push 'ws-new call-order)))
                  ((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                  ((symbol-function 'agent-repl--initialize-ws-env)
                   (lambda (_ws _root) (push 'init-env call-order)))
                  ((symbol-function 'magit-status)
                   (lambda (_path) (push 'magit call-order)))
                  ((symbol-function 'persp-remove-buffer)
                   (lambda (_buf) (push 'remove-dash call-order))))
          (agent-repl--new-workspace)
          (should (equal (reverse call-order) '(ws-new init-env magit remove-dash))))))))

;;;; ---- Tests: finalize-worktree-workspace defers magit via :pending-magit ----

(ert-deftest agent-repl-test-finalize-sets-pending-magit ()
  "finalize-worktree-workspace sets :pending-magit and does not call magit-status.
The drain happens on workspace activation; calling magit-status synchronously
here would open it in the caller's workspace layout, not the new one."
  (let ((magit-called nil))
    (agent-repl-test--with-clean-state
      (cl-letf (((symbol-function 'agent-repl--register-projectile-project) #'ignore)
                ((symbol-function 'agent-repl--path-canonical) #'identity)
                ((symbol-function 'agent-repl--repo-default-priority-for-path)
                 (lambda (&rest _) nil))
                ((symbol-function 'agent-repl--git-string-quiet) (lambda (&rest _) ""))
                ((symbol-function 'persp-add-new) (lambda (_ws) 'a-persp))
                ((symbol-function 'set-persp-parameter) #'ignore)
                ((symbol-function 'magit-status)
                 (lambda (&rest _) (setq magit-called t)))
                ((symbol-function 'agent-repl--remove-doom-dashboard)
                 (lambda (&rest _) (setq magit-called t)))
                ((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'agent-repl--open-initial-buffers) #'ignore)
                ((symbol-function 'agent-repl--enqueue-preemptive-prompt) #'ignore)
                ((symbol-function 'agent-repl--apply-workspace-properties) #'ignore)
                ((symbol-function 'agent-repl--git-string-quiet)
                 (lambda (&rest _) "DWC/test-ws"))
                ((symbol-function 'agent-repl--setup-worktree-session) #'ignore))
        (agent-repl--finalize-worktree-workspace
         "/tmp/fake" "test-ws" nil nil nil nil nil)
        (should (agent-repl--ws-get "test-ws" :pending-magit))
        (should-not magit-called)))))

(ert-deftest agent-repl-test-finalize-sets-pending-magit-with-preemptive-prompt ()
  "finalize-worktree-workspace sets :pending-magit even when a preemptive prompt is set."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--register-projectile-project) #'ignore)
              ((symbol-function 'agent-repl--path-canonical) #'identity)
              ((symbol-function 'agent-repl--repo-default-priority-for-path)
               (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--git-string-quiet) (lambda (&rest _) ""))
              ((symbol-function 'persp-add-new) (lambda (_ws) 'a-persp))
              ((symbol-function 'set-persp-parameter) #'ignore)
              ((symbol-function 'magit-status) #'ignore)
              ((symbol-function 'agent-repl--remove-doom-dashboard) #'ignore)
              ((symbol-function '+workspace-current-name) (lambda () "test-ws"))
              ((symbol-function 'agent-repl--open-initial-buffers) #'ignore)
              ((symbol-function 'agent-repl--enqueue-preemptive-prompt) #'ignore)
              ((symbol-function 'agent-repl--apply-workspace-properties) #'ignore)
              ((symbol-function 'agent-repl--git-string-quiet)
               (lambda (&rest _) "DWC/test-ws"))
              ((symbol-function 'agent-repl--setup-worktree-session) #'ignore))
      (agent-repl--finalize-worktree-workspace
       "/tmp/fake" "test-ws" "do something" nil nil nil nil)
      (should (agent-repl--ws-get "test-ws" :pending-magit)))))

;;;; ---- Tests: finalize-worktree-workspace defers initial buffers ----

(ert-deftest agent-repl-test-finalize-sets-pending-initial-buffers ()
  "finalize-worktree-workspace sets :pending-initial-buffers and does not call open-initial-buffers.
The drain happens on workspace activation; calling open-initial-buffers
synchronously here uses `find-file-noselect' in the caller's perspective,
leaking the opened buffers into the wrong workspace."
  (let ((open-called nil))
    (agent-repl-test--with-clean-state
      (cl-letf (((symbol-function 'agent-repl--register-projectile-project) #'ignore)
                ((symbol-function 'agent-repl--path-canonical) #'identity)
                ((symbol-function 'agent-repl--repo-default-priority-for-path)
                 (lambda (&rest _) nil))
                ((symbol-function 'agent-repl--git-string-quiet) (lambda (&rest _) ""))
                ((symbol-function 'persp-add-new) (lambda (_ws) 'a-persp))
                ((symbol-function 'set-persp-parameter) #'ignore)
                ((symbol-function 'magit-status) #'ignore)
                ((symbol-function 'agent-repl--remove-doom-dashboard) #'ignore)
                ((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'agent-repl--open-initial-buffers)
                 (lambda (&rest _) (setq open-called t)))
                ((symbol-function 'agent-repl--enqueue-preemptive-prompt) #'ignore)
                ((symbol-function 'agent-repl--apply-workspace-properties) #'ignore)
                ((symbol-function 'agent-repl--git-string-quiet)
                 (lambda (&rest _) "DWC/test-ws"))
                ((symbol-function 'agent-repl--setup-worktree-session) #'ignore))
        (agent-repl--finalize-worktree-workspace
         "/tmp/fake" "test-ws" nil nil nil nil nil)
        (should (agent-repl--ws-get "test-ws" :pending-initial-buffers))
        (should-not open-called)))))

;;;; ---- Tests: agent-repl--with-preserved-focus ----

(ert-deftest agent-repl-test-with-preserved-focus-restores-after-persp-switch ()
  "Macro restores the caller's workspace when BODY changes the current persp.
Workspace-creation finalize must not leak focus changes to the user; the
macro is the contract that guarantees it."
  (let* ((current-persp "caller-ws")
         (restored-with nil))
    (cl-letf (((symbol-function '+workspace-current-name)
               (lambda () current-persp))
              ((symbol-function '+workspace-switch)
               (lambda (name &optional _auto)
                 (setq current-persp name)))
              ((symbol-function 'agent-repl--restore-focus)
               (lambda (orig-persp orig-window orig-buffer)
                 (setq restored-with (list orig-persp orig-window orig-buffer)))))
      (agent-repl--with-preserved-focus
        (+workspace-switch "intruder-ws"))
      (should (equal (nth 0 restored-with) "caller-ws")))))

(ert-deftest agent-repl-test-with-preserved-focus-restores-on-error ()
  "Macro restores focus even when BODY signals — `unwind-protect' contract."
  (let ((restored-count 0))
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "caller-ws"))
              ((symbol-function 'agent-repl--restore-focus)
               (lambda (&rest _) (cl-incf restored-count))))
      (should-error
       (agent-repl--with-preserved-focus
         (error "body fail")))
      (should (= restored-count 1)))))

(ert-deftest agent-repl-test-restore-focus-switches-back-when-persp-drifted ()
  "Restore helper calls `+workspace-switch' when current persp differs from ORIG-PERSP."
  (let ((switched-to nil))
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "drifted-ws"))
              ((symbol-function '+workspace-switch)
               (lambda (name &optional _auto) (setq switched-to name))))
      (agent-repl--restore-focus "caller-ws" (selected-window) (current-buffer))
      (should (equal switched-to "caller-ws")))))

(ert-deftest agent-repl-test-restore-focus-no-switch-when-persp-stable ()
  "Restore helper skips `+workspace-switch' when current persp matches ORIG-PERSP.
Avoids re-firing the deactivate/activate hook chain when no drift actually happened."
  (let ((switch-called nil))
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "caller-ws"))
              ((symbol-function '+workspace-switch)
               (lambda (&rest _) (setq switch-called t))))
      (agent-repl--restore-focus "caller-ws" (selected-window) (current-buffer))
      (should-not switch-called))))

(ert-deftest agent-repl-test-restore-focus-tolerates-switch-failure ()
  "When `+workspace-switch' signals, restore logs and continues instead of re-signaling.
The macro's job is best-effort focus restoration, not error propagation —
a failing switch must not poison the `unwind-protect' chain or hide the
underlying BODY error from the caller."
  (cl-letf (((symbol-function '+workspace-current-name) (lambda () "drifted-ws"))
            ((symbol-function '+workspace-switch)
             (lambda (&rest _) (error "switch boom"))))
    ;; Must not signal — and must return normally.
    (agent-repl--restore-focus "caller-ws" (selected-window) (current-buffer))))

(ert-deftest agent-repl-test-finalize-preserves-caller-persp ()
  "finalize-worktree-workspace restores the caller's persp even when an internal
side effect (here, a stubbed `+workspace-new') flips the current workspace.
This is the user-visible contract: the sentinel-driven workspace generation
must not steal focus away from whatever the user is currently doing."
  (let* ((current-persp "caller-ws")
         (switch-log nil))
    (agent-repl-test--with-clean-state
      (cl-letf (((symbol-function 'agent-repl--register-projectile-project) #'ignore)
                ((symbol-function 'agent-repl--path-canonical) #'identity)
                ((symbol-function 'agent-repl--repo-default-priority-for-path)
                 (lambda (_path) nil))
                ((symbol-function 'agent-repl--git-string-quiet) (lambda (&rest _) ""))
                ;; Simulate the bug: workspace creation (now `persp-add-new'
                ;; via --ws-create) switches the current persp away from the
                ;; caller's workspace.
                ((symbol-function 'persp-add-new)
                 (lambda (name) (setq current-persp name) 'a-persp))
                ((symbol-function 'set-persp-parameter) #'ignore)
                ((symbol-function '+workspace-current-name)
                 (lambda () current-persp))
                ((symbol-function '+workspace-switch)
                 (lambda (name &optional _auto)
                   (push name switch-log)
                   (setq current-persp name)))
                ((symbol-function 'magit-status) #'ignore)
                ((symbol-function 'agent-repl--remove-doom-dashboard) #'ignore)
                ((symbol-function 'agent-repl--open-initial-buffers) #'ignore)
                ((symbol-function 'agent-repl--enqueue-preemptive-prompt) #'ignore)
                ((symbol-function 'agent-repl--apply-workspace-properties) #'ignore)
                ((symbol-function 'agent-repl--reorder-workspace-by-priority) #'ignore)
                ((symbol-function 'agent-repl--setup-worktree-session) #'ignore))
        (agent-repl--finalize-worktree-workspace
         "/tmp/fake" "test-ws" nil nil nil nil nil)
        (should (equal current-persp "caller-ws"))
        (should (member "caller-ws" switch-log))))))

(ert-deftest agent-repl-test-finalize-runs-callback-outside-preserved-focus ()
  "The optional CALLBACK runs OUTSIDE the focus-preservation wrapper, so
callers that deliberately switch to the new workspace (e.g. interactive
worktree creation that pulses the destination tab) are not silently undone
by the restore step."
  (let* ((current-persp "caller-ws")
         (callback-final-persp nil))
    (agent-repl-test--with-clean-state
      (cl-letf (((symbol-function 'agent-repl--register-projectile-project) #'ignore)
                ((symbol-function 'agent-repl--path-canonical) #'identity)
                ((symbol-function 'agent-repl--repo-default-priority-for-path)
                 (lambda (_path) nil))
                ((symbol-function 'agent-repl--git-string-quiet) (lambda (&rest _) ""))
                ((symbol-function '+workspace-new) #'ignore)
                ((symbol-function '+workspace-current-name)
                 (lambda () current-persp))
                ((symbol-function '+workspace-switch)
                 (lambda (name &optional _auto) (setq current-persp name)))
                ((symbol-function 'magit-status) #'ignore)
                ((symbol-function 'agent-repl--remove-doom-dashboard) #'ignore)
                ((symbol-function 'agent-repl--open-initial-buffers) #'ignore)
                ((symbol-function 'agent-repl--enqueue-preemptive-prompt) #'ignore)
                ((symbol-function 'agent-repl--apply-workspace-properties) #'ignore)
                ((symbol-function 'agent-repl--reorder-workspace-by-priority) #'ignore)
                ((symbol-function 'agent-repl--setup-worktree-session) #'ignore))
        (agent-repl--finalize-worktree-workspace
         "/tmp/fake" "test-ws" nil nil nil nil
         (lambda (_path dirname)
           ;; Callback deliberately switches to the new ws.
           (+workspace-switch dirname)
           (setq callback-final-persp (+workspace-current-name))))
        ;; The callback's switch must survive — i.e. NOT be undone by the
        ;; wrapper's restore step.
        (should (equal callback-final-persp "test-ws"))
        (should (equal current-persp "test-ws"))))))

(ert-deftest agent-repl-test-finalize-preserves-focus-on-signal ()
  "When finalize body errors mid-setup, focus still restores via `unwind-protect'.
A failing `--setup-worktree-session' (e.g. claude binary missing) must not
leave the user stranded on a half-built workspace."
  (let* ((current-persp "caller-ws"))
    (agent-repl-test--with-clean-state
      (cl-letf (((symbol-function 'agent-repl--register-projectile-project) #'ignore)
                ((symbol-function 'agent-repl--path-canonical) #'identity)
                ((symbol-function 'agent-repl--repo-default-priority-for-path)
                 (lambda (_path) nil))
                ((symbol-function '+workspace-new)
                 (lambda (name) (setq current-persp name)))
                ((symbol-function '+workspace-current-name)
                 (lambda () current-persp))
                ((symbol-function '+workspace-switch)
                 (lambda (name &optional _auto) (setq current-persp name)))
                ((symbol-function 'magit-status) #'ignore)
                ((symbol-function 'agent-repl--remove-doom-dashboard) #'ignore)
                ((symbol-function 'agent-repl--open-initial-buffers) #'ignore)
                ((symbol-function 'agent-repl--enqueue-preemptive-prompt) #'ignore)
                ((symbol-function 'agent-repl--apply-workspace-properties) #'ignore)
                ((symbol-function 'agent-repl--reorder-workspace-by-priority) #'ignore)
                ((symbol-function 'agent-repl--setup-worktree-session)
                 (lambda (&rest _) (error "setup boom"))))
        (should-error
         (agent-repl--finalize-worktree-workspace
          "/tmp/fake" "test-ws" nil nil nil nil nil))
        (should (equal current-persp "caller-ws"))))))

;;;; ---- Tests: agent-repl-create-doom-oneshot-workspace ----

(ert-deftest agent-repl-test-create-doom-oneshot-pins-git-root-to-doom-config ()
  "doom-oneshot pins git-root to `~/.config/doom' regardless of the current
workspace's project, so the binding can be invoked from anywhere and still
edit the doom config."
  (agent-repl-test--with-clean-state
    (let ((captured-git-root :unset))
      (cl-letf (((symbol-function '+workspace-current-name)
                 (lambda () "unrelated-ws"))
                ((symbol-function 'agent-repl--ws-dir)
                 (lambda (_ws) "/tmp/unrelated-repo/"))
                ((symbol-function 'read-from-minibuffer)
                 (lambda (&rest _) "tweak the modeline"))
                ((symbol-function 'agent-repl--spawn-workspace-generation)
                 (lambda (_raw _prefixed git-root _base _fork-from &optional _force-sandbox)
                   (setq captured-git-root git-root))))
        (agent-repl-create-doom-oneshot-workspace)
        (should (equal captured-git-root agent-repl--doom-config-dir))
        (should (equal captured-git-root
                       (file-name-as-directory
                        (expand-file-name "~/.config/doom"))))))))

(ert-deftest agent-repl-test-create-doom-oneshot-uses-master-base ()
  "doom-oneshot branches off local `master', mirroring `SPC TAB N'."
  (agent-repl-test--with-clean-state
    (let ((captured-base :unset))
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (&rest _) "tweak the modeline"))
                ((symbol-function 'agent-repl--spawn-workspace-generation)
                 (lambda (_raw _prefixed _git-root base _fork-from &optional _force-sandbox)
                   (setq captured-base base))))
        (agent-repl-create-doom-oneshot-workspace)
        (should (equal captured-base "master"))))))

(ert-deftest agent-repl-test-create-doom-oneshot-appends-merge-suffix-to-prefixed ()
  "The merge-on-success suffix is included in the PREFIXED prompt (the
spawned agent's first message) so the inner agent knows to invoke
`/workspace-merge' after a successful, tested implementation."
  (agent-repl-test--with-clean-state
    (let ((captured-prefixed :unset))
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (&rest _) "tweak the modeline"))
                ((symbol-function 'agent-repl--spawn-workspace-generation)
                 (lambda (_raw prefixed _git-root _base _fork-from &optional _force-sandbox)
                   (setq captured-prefixed prefixed))))
        (agent-repl-create-doom-oneshot-workspace)
        (should (string-match-p "/workspace-merge" captured-prefixed))
        (should (string-match-p
                 (regexp-quote agent-repl--oneshot-merge-suffix)
                 captured-prefixed))))))

(ert-deftest agent-repl-test-create-doom-oneshot-keeps-raw-prompt-clean ()
  "The merge suffix is NOT appended to the raw prompt — raw is used purely
for slug generation and should not get polluted with skill names like
`/workspace-merge', which would derail the workspace-name slug."
  (agent-repl-test--with-clean-state
    (let ((captured-raw :unset))
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (&rest _) "tweak the modeline"))
                ((symbol-function 'agent-repl--spawn-workspace-generation)
                 (lambda (raw _prefixed _git-root _base _fork-from &optional _force-sandbox)
                   (setq captured-raw raw))))
        (agent-repl-create-doom-oneshot-workspace)
        (should (equal captured-raw "tweak the modeline"))
        (should-not (string-match-p "/workspace-merge" captured-raw))))))

(ert-deftest agent-repl-test-create-doom-oneshot-prefixed-includes-autonomous-prefix ()
  "The prefixed prompt still starts with the standard autonomous-prompt
prefix so the spawned agent runs autonomously without waiting."
  (agent-repl-test--with-clean-state
    (let ((captured-prefixed :unset))
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (&rest _) "tweak the modeline"))
                ((symbol-function 'agent-repl--spawn-workspace-generation)
                 (lambda (_raw prefixed _git-root _base _fork-from &optional _force-sandbox)
                   (setq captured-prefixed prefixed))))
        (agent-repl-create-doom-oneshot-workspace)
        (should (string-prefix-p (agent-repl--meta-wrap agent-repl--autonomous-prompt-prefix)
                                 captured-prefixed))))))

(ert-deftest agent-repl-test-create-doom-oneshot-rejects-empty-prompt ()
  "An empty/whitespace prompt is rejected — there is nothing to slug or
implement, and we do not want to spawn a useless workspace."
  (agent-repl-test--with-clean-state
    (let ((spawned nil))
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (&rest _) "   "))
                ((symbol-function 'agent-repl--spawn-workspace-generation)
                 (lambda (&rest _) (setq spawned t))))
        (should-error (agent-repl-create-doom-oneshot-workspace)
                      :type 'user-error)
        (should-not spawned)))))

(ert-deftest agent-repl-test-create-doom-oneshot-passes-no-fork-from ()
  "doom-oneshot is not a fork — fork-from must be nil so the new workspace
starts a fresh agent session rather than resuming someone else's."
  (agent-repl-test--with-clean-state
    (let ((captured-fork-from :unset))
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (&rest _) "tweak the modeline"))
                ((symbol-function 'agent-repl--spawn-workspace-generation)
                 (lambda (_raw _prefixed _git-root _base fork-from &optional _force-sandbox)
                   (setq captured-fork-from fork-from))))
        (agent-repl-create-doom-oneshot-workspace)
        (should (null captured-fork-from))))))

(ert-deftest agent-repl-test-create-doom-oneshot-passes-force-sandbox-t ()
  "doom-oneshot passes force-sandbox = t so the spawned workspace runs in
the Docker sandbox rather than bare-metal."
  (agent-repl-test--with-clean-state
    (let ((captured-force-sandbox :unset))
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (&rest _) "tweak the modeline"))
                ((symbol-function 'agent-repl--spawn-workspace-generation)
                 (lambda (_raw _prefixed _git-root _base _fork-from &optional force-sandbox)
                   (setq captured-force-sandbox force-sandbox))))
        (agent-repl-create-doom-oneshot-workspace)
        (should captured-force-sandbox)))))

(ert-deftest agent-repl-test-create-doom-oneshot-from-current-branch-passes-force-sandbox-t ()
  "The current-branch variant also passes force-sandbox = t — both doom
oneshot flavours run in the sandbox, regardless of which base ref is used."
  (agent-repl-test--with-clean-state
    (let ((captured-force-sandbox :unset))
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (&rest _) "tweak the modeline"))
                ((symbol-function 'agent-repl--spawn-workspace-generation)
                 (lambda (_raw _prefixed _git-root _base _fork-from &optional force-sandbox)
                   (setq captured-force-sandbox force-sandbox))))
        (agent-repl-create-doom-oneshot-workspace-from-current-branch)
        (should captured-force-sandbox)))))

(ert-deftest agent-repl-test-oneshot-merge-suffix-mentions-stop-on-ambiguity ()
  "The merge suffix tells the spawned agent to STOP (not push on) when it
hits genuine ambiguity it cannot resolve — explicitly required so a
faulty one-shot implementation isn't auto-merged."
  (should (string-match-p "STOP" agent-repl--oneshot-merge-suffix))
  (should (string-match-p "ambiguity" agent-repl--oneshot-merge-suffix)))

(ert-deftest agent-repl-test-oneshot-merge-suffix-mentions-tests-and-commits ()
  "Merge is gated on implementation, tests, AND commits — the suffix must
spell that out so the spawned agent doesn't merge half-finished work."
  (should (string-match-p "tests" agent-repl--oneshot-merge-suffix))
  (should (string-match-p "[Cc]ommit" agent-repl--oneshot-merge-suffix)))

;;;; ---- Tests: agent-repl-create-doom-oneshot-workspace-from-current-branch ----

(ert-deftest agent-repl-test-create-doom-oneshot-from-current-branch-uses-head-base ()
  "doom-oneshot-from-current-branch branches off HEAD (current branch of
the doom-config repo) rather than `master', so the one-shot builds on
top of in-flight doom-config work."
  (agent-repl-test--with-clean-state
    (let ((captured-base :unset))
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (&rest _) "tweak the modeline"))
                ((symbol-function 'agent-repl--spawn-workspace-generation)
                 (lambda (_raw _prefixed _git-root base _fork-from &optional _force-sandbox)
                   (setq captured-base base))))
        (agent-repl-create-doom-oneshot-workspace-from-current-branch)
        (should (equal captured-base "HEAD"))))))

(ert-deftest agent-repl-test-create-doom-oneshot-from-current-branch-pins-git-root-to-doom-config ()
  "The current-branch variant still pins git-root to `~/.config/doom'
regardless of the calling workspace's project — only the base ref
changes from `master' to HEAD."
  (agent-repl-test--with-clean-state
    (let ((captured-git-root :unset))
      (cl-letf (((symbol-function '+workspace-current-name)
                 (lambda () "unrelated-ws"))
                ((symbol-function 'agent-repl--ws-dir)
                 (lambda (_ws) "/tmp/unrelated-repo/"))
                ((symbol-function 'read-from-minibuffer)
                 (lambda (&rest _) "tweak the modeline"))
                ((symbol-function 'agent-repl--spawn-workspace-generation)
                 (lambda (_raw _prefixed git-root _base _fork-from &optional _force-sandbox)
                   (setq captured-git-root git-root))))
        (agent-repl-create-doom-oneshot-workspace-from-current-branch)
        (should (equal captured-git-root agent-repl--doom-config-dir))))))

(ert-deftest agent-repl-test-create-doom-oneshot-from-current-branch-appends-merge-suffix ()
  "The current-branch variant must also append the merge-on-success suffix
to the prefixed prompt — the spawned agent still needs to know to invoke
`/workspace-merge' after a successful implementation."
  (agent-repl-test--with-clean-state
    (let ((captured-prefixed :unset))
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (&rest _) "tweak the modeline"))
                ((symbol-function 'agent-repl--spawn-workspace-generation)
                 (lambda (_raw prefixed _git-root _base _fork-from &optional _force-sandbox)
                   (setq captured-prefixed prefixed))))
        (agent-repl-create-doom-oneshot-workspace-from-current-branch)
        (should (string-match-p "/workspace-merge" captured-prefixed))
        (should (string-match-p
                 (regexp-quote agent-repl--oneshot-merge-suffix)
                 captured-prefixed))))))

(ert-deftest agent-repl-test-create-doom-oneshot-from-current-branch-keeps-raw-prompt-clean ()
  "The merge suffix must not pollute the raw prompt used for slug
generation — same constraint as the master variant."
  (agent-repl-test--with-clean-state
    (let ((captured-raw :unset))
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (&rest _) "tweak the modeline"))
                ((symbol-function 'agent-repl--spawn-workspace-generation)
                 (lambda (raw _prefixed _git-root _base _fork-from &optional _force-sandbox)
                   (setq captured-raw raw))))
        (agent-repl-create-doom-oneshot-workspace-from-current-branch)
        (should (equal captured-raw "tweak the modeline"))
        (should-not (string-match-p "/workspace-merge" captured-raw))))))

(ert-deftest agent-repl-test-create-doom-oneshot-from-current-branch-rejects-empty-prompt ()
  "An empty/whitespace prompt is rejected for the current-branch variant
too — there is nothing to slug or implement."
  (agent-repl-test--with-clean-state
    (let ((spawned nil))
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (&rest _) "   "))
                ((symbol-function 'agent-repl--spawn-workspace-generation)
                 (lambda (&rest _) (setq spawned t))))
        (should-error
         (agent-repl-create-doom-oneshot-workspace-from-current-branch)
         :type 'user-error)
        (should-not spawned)))))

(ert-deftest agent-repl-test-create-doom-oneshot-default-base-is-master ()
  "Calling the parent function with no BASE arg still defaults to `master'
— preserves backwards compatibility for the existing `SPC j o' binding
and existing call sites that pass no arguments."
  (agent-repl-test--with-clean-state
    (let ((captured-base :unset))
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (&rest _) "tweak the modeline"))
                ((symbol-function 'agent-repl--spawn-workspace-generation)
                 (lambda (_raw _prefixed _git-root base _fork-from &optional _force-sandbox)
                   (setq captured-base base))))
        (agent-repl-create-doom-oneshot-workspace)
        (should (equal captured-base "master"))))))

;;;; ---- Tests: merge queue ----

(defmacro agent-repl-test--with-empty-merge-queue (&rest body)
  "Run BODY with `agent-repl--merge-queue' freshly empty.
The queue is a top-level defvar, so tests that enqueue MUST scrub it
afterwards or later tests inherit stale state."
  (declare (indent 0))
  `(let ((agent-repl--merge-queue nil))
     (unwind-protect (progn ,@body)
       (setq agent-repl--merge-queue nil))))

(ert-deftest agent-repl-test-ws-merge-queued-p-true-when-marker-set ()
  "WS with `:repl-state :merge-queued' is detected as queued."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws" :project-dir "/tmp/ws")
    (agent-repl--ws-put "ws" :repl-state :merge-queued)
    (should (agent-repl--ws-merge-queued-p "ws"))))

(ert-deftest agent-repl-test-ws-merge-queued-p-nil-when-unmarked ()
  "WS without the queued marker is not detected as queued."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws" :project-dir "/tmp/ws")
    (should-not (agent-repl--ws-merge-queued-p "ws"))))

(ert-deftest agent-repl-test-ws-merge-queued-p-nil-for-other-repl-states ()
  "Other `:repl-state' values are not mistaken for queued."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws" :project-dir "/tmp/ws")
    (agent-repl--ws-put "ws" :repl-state :merged)
    (should-not (agent-repl--ws-merge-queued-p "ws"))))

(ert-deftest agent-repl-test-enqueue-merge-appends-to-queue ()
  "`--enqueue-merge' appends a plist describing the request to the FIFO,
tagged with the canonical `:target-dir' bucket key."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-empty-merge-queue
      (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
      (cl-letf (((symbol-function 'agent-repl--path-canonical) #'identity))
        (agent-repl--enqueue-merge "ws1" t t "/tmp/target"))
      (should (equal agent-repl--merge-queue
                     '((:source-ws "ws1" :silent t :auto-resolve t
                        :target-dir "/tmp/target")))))))

(ert-deftest agent-repl-test-enqueue-merge-canonicalizes-target-dir ()
  "`--enqueue-merge' stores the CANONICAL target dir so two spellings of
the same destination land in the same bucket."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-empty-merge-queue
      (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
      (cl-letf (((symbol-function 'agent-repl--path-canonical)
                 (lambda (_) "/canon/target")))
        (agent-repl--enqueue-merge "ws1" t t "/tmp/target/"))
      (should (equal (plist-get (car agent-repl--merge-queue) :target-dir)
                     "/canon/target")))))

(ert-deftest agent-repl-test-enqueue-merge-marks-repl-state ()
  "`--enqueue-merge' flips the workspace's `:repl-state' to `:merge-queued'
so the drawer can route it under MERGING."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-empty-merge-queue
      (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
      (agent-repl--enqueue-merge "ws1" nil nil "/tmp/target")
      (should (eq (agent-repl--ws-get "ws1" :repl-state) :merge-queued)))))

(ert-deftest agent-repl-test-enqueue-merge-clears-agent-state ()
  "Stale `:agent-state' is cleared so the state glyph reflects queued."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-empty-merge-queue
      (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
      (agent-repl--ws-put "ws1" :agent-state :thinking)
      (agent-repl--enqueue-merge "ws1" nil nil "/tmp/target")
      (should (null (agent-repl--ws-get "ws1" :agent-state))))))

(ert-deftest agent-repl-test-enqueue-merge-preserves-fifo-order ()
  "Multiple enqueues land in arrival order — the drain must pop oldest first."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-empty-merge-queue
      (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
      (agent-repl--ws-put "ws2" :project-dir "/tmp/ws2")
      (agent-repl--ws-put "ws3" :project-dir "/tmp/ws3")
      (agent-repl--enqueue-merge "ws1" t t "/tmp/target")
      (agent-repl--enqueue-merge "ws2" nil t "/tmp/target")
      (agent-repl--enqueue-merge "ws3" t nil "/tmp/target")
      (should (equal (mapcar (lambda (e) (plist-get e :source-ws))
                             agent-repl--merge-queue)
                     '("ws1" "ws2" "ws3"))))))

(ert-deftest agent-repl-test-ws-in-merge-queue-p-nil-when-empty ()
  "`--ws-in-merge-queue-p' returns nil against an empty queue."
  (agent-repl-test--with-empty-merge-queue
    (should-not (agent-repl--ws-in-merge-queue-p "ws1"))))

(ert-deftest agent-repl-test-ws-in-merge-queue-p-true-when-parked ()
  "`--ws-in-merge-queue-p' detects a ws that has an entry in the queue."
  (agent-repl-test--with-empty-merge-queue
    (setq agent-repl--merge-queue
          '((:source-ws "ws1" :silent t :auto-resolve t)))
    (should (agent-repl--ws-in-merge-queue-p "ws1"))))

(ert-deftest agent-repl-test-ws-in-merge-queue-p-nil-for-absent-ws ()
  "`--ws-in-merge-queue-p' returns nil for a ws not present among queued entries."
  (agent-repl-test--with-empty-merge-queue
    (setq agent-repl--merge-queue
          '((:source-ws "ws1" :silent t :auto-resolve t)))
    (should-not (agent-repl--ws-in-merge-queue-p "ws2"))))

(ert-deftest agent-repl-test-enqueue-merge-dedupes-duplicate-ws ()
  "Re-enqueuing a ws already in the queue does not append a second entry."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-empty-merge-queue
      (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
      (cl-letf (((symbol-function 'agent-repl--path-canonical) #'identity))
        (agent-repl--enqueue-merge "ws1" t t "/tmp/target")
        (agent-repl--enqueue-merge "ws1" t t "/tmp/target"))
      (should (equal agent-repl--merge-queue
                     '((:source-ws "ws1" :silent t :auto-resolve t
                        :target-dir "/tmp/target")))))))

(ert-deftest agent-repl-test-enqueue-merge-dedupe-keeps-distinct-ws ()
  "Dedup is keyed on `:source-ws' only — a distinct ws still appends."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-empty-merge-queue
      (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
      (agent-repl--ws-put "ws2" :project-dir "/tmp/ws2")
      (agent-repl--enqueue-merge "ws1" t t "/tmp/target")
      (agent-repl--enqueue-merge "ws1" t t "/tmp/target")
      (agent-repl--enqueue-merge "ws2" t t "/tmp/target")
      (should (equal (mapcar (lambda (e) (plist-get e :source-ws))
                             agent-repl--merge-queue)
                     '("ws1" "ws2"))))))

(ert-deftest agent-repl-test-dequeue-merge-removes-entry ()
  "`--dequeue-merge' pulls the matching ws's entry out of the FIFO."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-empty-merge-queue
      (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
      (agent-repl--enqueue-merge "ws1" t t "/tmp/target")
      (agent-repl--dequeue-merge "ws1")
      (should (null agent-repl--merge-queue)))))

(ert-deftest agent-repl-test-dequeue-merge-clears-repl-state-marker ()
  "`--dequeue-merge' clears the `:repl-state :merge-queued' marker on the ws."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-empty-merge-queue
      (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
      (agent-repl--enqueue-merge "ws1" t t "/tmp/target")
      (agent-repl--dequeue-merge "ws1")
      (should (null (agent-repl--ws-get "ws1" :repl-state))))))

(ert-deftest agent-repl-test-dequeue-merge-returns-t-when-removed ()
  "`--dequeue-merge' returns non-nil when it removed an entry."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-empty-merge-queue
      (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
      (agent-repl--enqueue-merge "ws1" t t "/tmp/target")
      (should (agent-repl--dequeue-merge "ws1")))))

(ert-deftest agent-repl-test-dequeue-merge-noop-when-ws-not-queued ()
  "`--dequeue-merge' returns nil and leaves the queue intact when the ws
has no parked entry."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-empty-merge-queue
      (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
      (cl-letf (((symbol-function 'agent-repl--path-canonical) #'identity))
        (agent-repl--enqueue-merge "ws1" t t "/tmp/target"))
      (should-not (agent-repl--dequeue-merge "ws2"))
      (should (equal agent-repl--merge-queue
                     '((:source-ws "ws1" :silent t :auto-resolve t
                        :target-dir "/tmp/target")))))))

(ert-deftest agent-repl-test-dequeue-merge-noop-when-ws-nil ()
  "`--dequeue-merge' returns nil for a nil ws without touching the queue."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-empty-merge-queue
      (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
      (cl-letf (((symbol-function 'agent-repl--path-canonical) #'identity))
        (agent-repl--enqueue-merge "ws1" t t "/tmp/target"))
      (should-not (agent-repl--dequeue-merge nil))
      (should (equal agent-repl--merge-queue
                     '((:source-ws "ws1" :silent t :auto-resolve t
                        :target-dir "/tmp/target")))))))

(ert-deftest agent-repl-test-dequeue-merge-preserves-other-entries ()
  "`--dequeue-merge' removes only the matching ws, leaving siblings in
FIFO order."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-empty-merge-queue
      (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
      (agent-repl--ws-put "ws2" :project-dir "/tmp/ws2")
      (agent-repl--ws-put "ws3" :project-dir "/tmp/ws3")
      (agent-repl--enqueue-merge "ws1" t t "/tmp/target")
      (agent-repl--enqueue-merge "ws2" nil t "/tmp/target")
      (agent-repl--enqueue-merge "ws3" t nil "/tmp/target")
      (agent-repl--dequeue-merge "ws2")
      (should (equal (mapcar (lambda (e) (plist-get e :source-ws))
                             agent-repl--merge-queue)
                     '("ws1" "ws3"))))))

(ert-deftest agent-repl-test-dequeue-merge-leaves-other-repl-state-untouched ()
  "`--dequeue-merge' does not clear a non-`:merge-queued' `:repl-state'."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-empty-merge-queue
      ;; Hand-place an entry without the marker so the repl-state under
      ;; test is a foreign value the dequeue must leave alone.
      (setq agent-repl--merge-queue
            '((:source-ws "ws1" :silent t :auto-resolve t :target-dir "/tmp/target")))
      (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
      (agent-repl--ws-put "ws1" :repl-state :merged)
      (agent-repl--dequeue-merge "ws1")
      (should (eq (agent-repl--ws-get "ws1" :repl-state) :merged)))))

;;;; ---- Tests: merge-queue bucket helpers ----

(ert-deftest agent-repl-test-merge-target-dir-for-ws-uses-recorded-source ()
  "`--merge-target-dir-for-ws' resolves through the recorded source dir."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
    (agent-repl--ws-put "ws1" :source-ws-dir "/tmp/parent")
    (cl-letf (((symbol-function 'file-directory-p) (lambda (_) t))
              ((symbol-function 'agent-repl--master-worktree-path)
               (lambda (_) "/tmp/master"))
              ((symbol-function 'agent-repl--resolve-merge-into-source-target)
               (lambda (parent _master) parent)))
      (should (equal (agent-repl--merge-target-dir-for-ws "ws1") "/tmp/parent")))))

(ert-deftest agent-repl-test-merge-target-dir-for-ws-nil-when-no-project-dir ()
  "`--merge-target-dir-for-ws' returns nil for a ws with no `:project-dir'
\(it cannot resolve a destination)."
  (agent-repl-test--with-clean-state
    (should (null (agent-repl--merge-target-dir-for-ws "ghost-ws")))))

(ert-deftest agent-repl-test-merge-queue-entry-target-dir-prefers-stored ()
  "`--merge-queue-entry-target-dir' returns the canonical stored
`:target-dir' without resolving from the source ws."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--path-canonical) #'identity)
              ((symbol-function 'agent-repl--merge-target-dir-for-ws)
               (lambda (_) (error "must not resolve when :target-dir present"))))
      (should (equal (agent-repl--merge-queue-entry-target-dir
                      '(:source-ws "ws1" :target-dir "/tmp/target"))
                     "/tmp/target")))))

(ert-deftest agent-repl-test-merge-queue-entry-target-dir-falls-back-to-resolution ()
  "`--merge-queue-entry-target-dir' resolves from the source ws when the
entry carries no `:target-dir' (legacy/recovery entry)."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--path-canonical) #'identity)
              ((symbol-function 'agent-repl--merge-target-dir-for-ws)
               (lambda (ws) (and (equal ws "ws1") "/tmp/resolved"))))
      (should (equal (agent-repl--merge-queue-entry-target-dir
                      '(:source-ws "ws1"))
                     "/tmp/resolved")))))

(ert-deftest agent-repl-test-merge-queue-target-dirs-distinct-first-appearance ()
  "`--merge-queue-target-dirs' returns distinct bucket keys in first-seen
order."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-empty-merge-queue
      (cl-letf (((symbol-function 'agent-repl--path-canonical) #'identity))
        (setq agent-repl--merge-queue
              '((:source-ws "w1" :target-dir "/tmp/a")
                (:source-ws "w2" :target-dir "/tmp/b")
                (:source-ws "w3" :target-dir "/tmp/a")))
        (should (equal (agent-repl--merge-queue-target-dirs)
                       '("/tmp/a" "/tmp/b")))))))

(ert-deftest agent-repl-test-merge-queue-front-for-target-returns-oldest ()
  "`--merge-queue-front-for-target' returns the FIFO front of a bucket."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-empty-merge-queue
      (cl-letf (((symbol-function 'agent-repl--path-canonical) #'identity))
        (setq agent-repl--merge-queue
              '((:source-ws "w1" :target-dir "/tmp/a")
                (:source-ws "w2" :target-dir "/tmp/b")
                (:source-ws "w3" :target-dir "/tmp/a")))
        (should (equal (plist-get
                        (agent-repl--merge-queue-front-for-target "/tmp/a")
                        :source-ws)
                       "w1"))))))

(ert-deftest agent-repl-test-drain-merge-queue-noop-when-empty ()
  "Empty queue → drain does nothing, no error."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-empty-merge-queue
      (let ((called nil))
        (cl-letf (((symbol-function 'agent-repl--workspace-merge-into-source)
                   (lambda (&rest _) (setq called t))))
          (agent-repl--drain-merge-queue)
          (should-not called))))))

(ert-deftest agent-repl-test-drain-merge-queue-noop-when-target-bucket-busy ()
  "A bucket whose TARGET worktree still has CHERRY_PICK_HEAD is left
untouched — a later drain re-enters once that target's cherry-pick clears."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-empty-merge-queue
      (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
      (cl-letf (((symbol-function 'agent-repl--path-canonical) #'identity))
        (agent-repl--enqueue-merge "ws1" t t "/tmp/target"))
      (cl-letf (;; Block: this target's worktree has a cherry-pick in flight.
                ((symbol-function 'agent-repl--cherry-pick-in-progress-p)
                 (lambda (root) (equal root "/tmp/target")))
                ((symbol-function 'agent-repl--path-canonical) #'identity))
        (let ((called nil))
          (cl-letf (((symbol-function 'agent-repl--workspace-merge-into-source)
                     (lambda (&rest _) (setq called t))))
            (agent-repl--drain-merge-queue)
            (should-not called)
            (should (= 1 (length agent-repl--merge-queue)))))))))

(ert-deftest agent-repl-test-drain-merge-queue-pops-oldest-first-within-bucket ()
  "Within a single target bucket, drain dispatches the oldest entry (FIFO)
and leaves the rest of that bucket parked."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-empty-merge-queue
      (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
      (agent-repl--ws-put "ws2" :project-dir "/tmp/ws2")
      (cl-letf (((symbol-function 'agent-repl--path-canonical) #'identity))
        (agent-repl--enqueue-merge "ws1" t t "/tmp/target")
        (agent-repl--enqueue-merge "ws2" nil nil "/tmp/target"))
      (let ((dispatched nil))
        (cl-letf (((symbol-function 'agent-repl--path-canonical) #'identity)
                  ((symbol-function 'agent-repl--cherry-pick-in-progress-p)
                   (lambda (_) nil))
                  ((symbol-function 'agent-repl--current-head-sha)
                   (lambda (_) nil))
                  ((symbol-function 'agent-repl--workspace-merge-into-source)
                   (lambda (ws &optional silent auto)
                     (push (list ws silent auto) dispatched))))
          (agent-repl--drain-merge-queue)
          (should (equal dispatched '(("ws1" t t))))
          (should (equal (mapcar (lambda (e) (plist-get e :source-ws))
                                 agent-repl--merge-queue)
                         '("ws2"))))))))

(ert-deftest agent-repl-test-drain-merge-queue-drains-distinct-targets-concurrently ()
  "Two merges whose destinations are DIFFERENT target worktrees both
dispatch in a single drain — neither bucket blocks the other."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-empty-merge-queue
      (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
      (agent-repl--ws-put "ws2" :project-dir "/tmp/ws2")
      (cl-letf (((symbol-function 'agent-repl--path-canonical) #'identity))
        (agent-repl--enqueue-merge "ws1" t t "/tmp/target-a")
        (agent-repl--enqueue-merge "ws2" t t "/tmp/target-b"))
      (let ((dispatched nil))
        (cl-letf (((symbol-function 'agent-repl--path-canonical) #'identity)
                  ((symbol-function 'agent-repl--cherry-pick-in-progress-p)
                   (lambda (_) nil))
                  ((symbol-function 'agent-repl--current-head-sha)
                   (lambda (_) nil))
                  ((symbol-function 'agent-repl--workspace-merge-into-source)
                   (lambda (ws &rest _) (push ws dispatched))))
          (agent-repl--drain-merge-queue)
          (should (equal (sort (copy-sequence dispatched) #'string<)
                         '("ws1" "ws2")))
          (should (null agent-repl--merge-queue)))))))

(ert-deftest agent-repl-test-drain-merge-queue-busy-bucket-does-not-block-free-bucket ()
  "When one target's worktree has a live cherry-pick, its bucket is skipped
but a different target's bucket still drains — the core independence
guarantee."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-empty-merge-queue
      (agent-repl--ws-put "ws-busy" :project-dir "/tmp/ws-busy")
      (agent-repl--ws-put "ws-free" :project-dir "/tmp/ws-free")
      (cl-letf (((symbol-function 'agent-repl--path-canonical) #'identity))
        (agent-repl--enqueue-merge "ws-busy" t t "/tmp/target-busy")
        (agent-repl--enqueue-merge "ws-free" t t "/tmp/target-free"))
      (let ((dispatched nil))
        (cl-letf (((symbol-function 'agent-repl--path-canonical) #'identity)
                  ((symbol-function 'agent-repl--cherry-pick-in-progress-p)
                   (lambda (root) (equal root "/tmp/target-busy")))
                  ((symbol-function 'agent-repl--current-head-sha)
                   (lambda (_) nil))
                  ((symbol-function 'agent-repl--workspace-merge-into-source)
                   (lambda (ws &rest _) (push ws dispatched))))
          (agent-repl--drain-merge-queue)
          (should (equal dispatched '("ws-free")))
          (should (equal (mapcar (lambda (e) (plist-get e :source-ws))
                                 agent-repl--merge-queue)
                         '("ws-busy"))))))))

(ert-deftest agent-repl-test-drain-merge-queue-resolves-target-for-legacy-entry ()
  "An entry carrying no `:target-dir' (legacy/recovery) is bucketed by
lazily resolving its destination via `--merge-target-dir-for-ws'."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-empty-merge-queue
      (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
      (setq agent-repl--merge-queue
            (list (list :source-ws "ws1" :silent t :auto-resolve t)))
      (let ((dispatched nil))
        (cl-letf (((symbol-function 'agent-repl--path-canonical) #'identity)
                  ((symbol-function 'agent-repl--merge-target-dir-for-ws)
                   (lambda (_) "/tmp/resolved-target"))
                  ((symbol-function 'agent-repl--cherry-pick-in-progress-p)
                   (lambda (_) nil))
                  ((symbol-function 'agent-repl--current-head-sha)
                   (lambda (_) nil))
                  ((symbol-function 'agent-repl--workspace-merge-into-source)
                   (lambda (ws &rest _) (push ws dispatched))))
          (agent-repl--drain-merge-queue)
          (should (equal dispatched '("ws1")))
          (should (null agent-repl--merge-queue)))))))

(ert-deftest agent-repl-test-drain-merge-queue-skips-unresolvable-target-entry ()
  "An entry whose destination cannot be resolved (nil bucket) stays parked
rather than being dispatched blind."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-empty-merge-queue
      (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
      (setq agent-repl--merge-queue
            (list (list :source-ws "ws1" :silent t :auto-resolve t)))
      (let ((called nil))
        (cl-letf (((symbol-function 'agent-repl--merge-target-dir-for-ws)
                   (lambda (_) nil))
                  ((symbol-function 'agent-repl--workspace-merge-into-source)
                   (lambda (&rest _) (setq called t))))
          (agent-repl--drain-merge-queue)
          (should-not called)
          (should (= 1 (length agent-repl--merge-queue))))))))

(ert-deftest agent-repl-test-drain-merge-queue-clears-queued-marker ()
  "Drain clears the dispatched workspace's `:merge-queued' marker so the
re-entered `--workspace-merge-into-source' can flip `:merging' t
without precedence collisions."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-empty-merge-queue
      (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
      (cl-letf (((symbol-function 'agent-repl--path-canonical) #'identity))
        (agent-repl--enqueue-merge "ws1" t t "/tmp/target"))
      (cl-letf (((symbol-function 'agent-repl--path-canonical) #'identity)
                ((symbol-function 'agent-repl--cherry-pick-in-progress-p)
                 (lambda (_) nil))
                ((symbol-function 'agent-repl--current-head-sha)
                 (lambda (_) nil))
                ((symbol-function 'agent-repl--workspace-merge-into-source)
                 (lambda (&rest _) nil)))
        (agent-repl--drain-merge-queue)
        (should-not (eq (agent-repl--ws-get "ws1" :repl-state)
                        :merge-queued))))))

(ert-deftest agent-repl-test-drain-merge-queue-catches-deferred-error ()
  "Errors from a deferred merge are caught so a single bad entry does
not leave the queue stuck — drain returns normally, no signal."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-empty-merge-queue
      (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
      (cl-letf (((symbol-function 'agent-repl--path-canonical) #'identity))
        (agent-repl--enqueue-merge "ws1" t t "/tmp/target"))
      (cl-letf (((symbol-function 'agent-repl--path-canonical) #'identity)
                ((symbol-function 'agent-repl--cherry-pick-in-progress-p)
                 (lambda (_) nil))
                ((symbol-function 'agent-repl--current-head-sha)
                 (lambda (_) nil))
                ((symbol-function 'agent-repl--workspace-merge-into-source)
                 (lambda (&rest _) (error "boom"))))
        ;; Must not raise.
        (agent-repl--drain-merge-queue)))))

(ert-deftest agent-repl-test-enqueue-merge-persists-snapshot ()
  "`--enqueue-merge' triggers `agent-repl-save-workspace-snapshot' so a
restart restores the queue.  Stubs out the saver to confirm the call."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-empty-merge-queue
      (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
      (let ((save-calls 0))
        (cl-letf (((symbol-function 'agent-repl-save-workspace-snapshot)
                   (lambda () (cl-incf save-calls))))
          (agent-repl--enqueue-merge "ws1" t t "/tmp/target")
          (should (= 1 save-calls)))))))

(ert-deftest agent-repl-test-drain-merge-queue-persists-snapshot ()
  "`--drain-merge-queue' triggers a snapshot save AFTER popping the next
entry so the persisted queue reflects the post-pop length — a crash
mid-merge does not resurrect an already-dispatched entry."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-empty-merge-queue
      (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
      (cl-letf (((symbol-function 'agent-repl--path-canonical) #'identity))
        (agent-repl--enqueue-merge "ws1" t t "/tmp/target"))
      (let ((save-calls 0)
            (queue-len-at-save nil))
        (cl-letf (((symbol-function 'agent-repl--path-canonical) #'identity)
                  ((symbol-function 'agent-repl--cherry-pick-in-progress-p)
                   (lambda (_) nil))
                  ((symbol-function 'agent-repl--current-head-sha)
                   (lambda (_) nil))
                  ((symbol-function 'agent-repl-save-workspace-snapshot)
                   (lambda ()
                     (cl-incf save-calls)
                     (setq queue-len-at-save (length agent-repl--merge-queue))))
                  ((symbol-function 'agent-repl--workspace-merge-into-source)
                   (lambda (&rest _) nil)))
          (agent-repl--drain-merge-queue)
          (should (= 1 save-calls))
          ;; Drain pops before saving, so the persisted queue has 0 entries.
          (should (= 0 queue-len-at-save)))))))

(ert-deftest agent-repl-test-persist-merge-queue-tolerates-missing-saver ()
  "`--persist-merge-queue' is a no-op when the saver isn't fboundp, so
test fixtures and partial-load environments don't crash on enqueue/drain."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-empty-merge-queue
      (cl-letf (((symbol-function 'fboundp)
                 (lambda (sym)
                   (if (eq sym 'agent-repl-save-workspace-snapshot)
                       nil
                     (funcall (symbol-function 'fboundp) sym)))))
        ;; Must not raise.
        (agent-repl--persist-merge-queue)))))

(ert-deftest agent-repl-test-persist-merge-queue-swallows-save-errors ()
  "Errors from the saver are caught and logged so a write failure does
not propagate into the queue mutator and stall the merge flow."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-empty-merge-queue
      (cl-letf (((symbol-function 'agent-repl-save-workspace-snapshot)
                 (lambda () (error "disk full"))))
        ;; Must not raise.
        (agent-repl--persist-merge-queue)))))

(ert-deftest agent-repl-test-workspace-merge-into-source-enqueues-when-target-cherry-pick-in-flight ()
  "When a cherry-pick is in progress in the resolved TARGET worktree, the
new merge request is parked on the queue (tagged with that target) rather
than running."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-empty-merge-queue
      (agent-repl--ws-put "ws-pending" :project-dir "/tmp/ws-pending")
      (agent-repl--ws-put "ws-pending" :source-ws-dir "/tmp/parent")
      (cl-letf (((symbol-function 'file-directory-p) (lambda (_) t))
                ((symbol-function 'agent-repl--path-canonical) #'identity)
                ((symbol-function 'agent-repl--master-worktree-path)
                 (lambda (_) "/tmp/master"))
                ((symbol-function 'agent-repl--resolve-merge-into-source-target)
                 (lambda (parent _master) parent))
                ((symbol-function 'agent-repl--git-branch-of-dir)
                 (lambda (_) nil))
                ;; Enqueueing refreshes the queued entry's commit lookahead.
                ((symbol-function 'agent-repl--git-string-quiet)
                 (lambda (&rest _args) "master"))
                ;; The resolved target ("/tmp/parent") has a cherry-pick in flight.
                ((symbol-function 'agent-repl--cherry-pick-in-progress-p)
                 (lambda (root) (equal root "/tmp/parent"))))
        (let ((merge-do-called nil))
          (cl-letf (((symbol-function 'agent-repl--workspace-merge-do)
                     (lambda (&rest _) (setq merge-do-called t))))
            (agent-repl--workspace-merge-into-source "ws-pending" t t)
            (should-not merge-do-called)
            (should (= 1 (length agent-repl--merge-queue)))
            (should (equal (plist-get (car agent-repl--merge-queue) :target-dir)
                           "/tmp/parent"))
            (should (eq (agent-repl--ws-get "ws-pending" :repl-state)
                        :merge-queued))))))))

(ert-deftest agent-repl-test-workspace-merge-into-source-proceeds-when-different-target-busy ()
  "A cherry-pick in flight in an UNRELATED worktree does not defer a merge
whose resolved target is a different worktree — the per-target gate only
inspects this merge's own destination."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-empty-merge-queue
      (agent-repl--ws-put "ws-pending" :project-dir "/tmp/ws-pending")
      (agent-repl--ws-put "ws-pending" :source-ws-dir "/tmp/parent")
      (cl-letf (((symbol-function 'file-directory-p) (lambda (_) t))
                ((symbol-function 'agent-repl--path-canonical) #'identity)
                ((symbol-function 'agent-repl--master-worktree-path)
                 (lambda (_) "/tmp/master"))
                ((symbol-function 'agent-repl--resolve-merge-into-source-target)
                 (lambda (parent _master) parent))
                ((symbol-function 'agent-repl--assert-clean-worktree) #'ignore)
                ((symbol-function 'agent-repl--git-branch-of-dir)
                 (lambda (_) nil))
                ;; An unrelated worktree is busy, but NOT this merge's target.
                ((symbol-function 'agent-repl--cherry-pick-in-progress-p)
                 (lambda (root) (equal root "/tmp/unrelated"))))
        (let ((merge-do-args nil))
          (cl-letf (((symbol-function 'agent-repl--workspace-merge-do)
                     (lambda (ws target &rest _) (setq merge-do-args (list ws target)))))
            (agent-repl--workspace-merge-into-source "ws-pending" t t)
            (should (equal merge-do-args '("ws-pending" "/tmp/parent")))
            (should (null agent-repl--merge-queue))))))))

;;;; ---- Tests: drain-merge-queue loop guards ----

(ert-deftest agent-repl-test-drain-merge-queue-halts-on-halt-until-human ()
  "A bucket whose front entry carries `:halt-until-human t' must NOT be
popped during an auto-drain — that bucket stays untouched until a human
kicks it via `agent-repl-drain-merge-queue'."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-empty-merge-queue
      (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
      (setq agent-repl--merge-queue
            (list (list :source-ws "ws1" :silent t :auto-resolve t
                        :target-dir "/tmp/target" :halt-until-human t)))
      (let ((called nil))
        (cl-letf (((symbol-function 'agent-repl--path-canonical) #'identity)
                  ((symbol-function 'agent-repl--cherry-pick-in-progress-p)
                   (lambda (_) nil))
                  ((symbol-function 'agent-repl--current-head-sha)
                   (lambda (_) nil))
                  ((symbol-function 'agent-repl--workspace-merge-into-source)
                   (lambda (&rest _) (setq called t))))
          (agent-repl--drain-merge-queue)
          (should-not called)
          (should (= 1 (length agent-repl--merge-queue))))))))

(ert-deftest agent-repl-test-drain-merge-queue-halts-on-matching-target-head ()
  "Loop guard: a bucket front whose recorded `:last-attempt-target-head'
equals the current HEAD of its target dir is skipped — nothing on that
branch has advanced, so retrying would just re-fail."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-empty-merge-queue
      (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
      (setq agent-repl--merge-queue
            (list (list :source-ws "ws1" :silent t :auto-resolve t
                        :target-dir "/tmp/target"
                        :last-attempt-target-head "abc123")))
      (let ((called nil))
        (cl-letf (((symbol-function 'agent-repl--path-canonical) #'identity)
                  ((symbol-function 'agent-repl--cherry-pick-in-progress-p)
                   (lambda (_) nil))
                  ((symbol-function 'agent-repl--current-head-sha)
                   (lambda (_) "abc123"))
                  ((symbol-function 'agent-repl--workspace-merge-into-source)
                   (lambda (&rest _) (setq called t))))
          (agent-repl--drain-merge-queue)
          (should-not called)
          (should (= 1 (length agent-repl--merge-queue))))))))

(ert-deftest agent-repl-test-drain-merge-queue-proceeds-when-target-head-advanced ()
  "Loop guard releases when the target HEAD has changed since the failed
attempt — a sibling workspace successfully advanced the branch, so this
workspace's retry is no longer redundant."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-empty-merge-queue
      (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
      (setq agent-repl--merge-queue
            (list (list :source-ws "ws1" :silent t :auto-resolve t
                        :target-dir "/tmp/target"
                        :last-attempt-target-head "abc123")))
      (let ((called nil))
        (cl-letf (((symbol-function 'agent-repl--path-canonical) #'identity)
                  ((symbol-function 'agent-repl--cherry-pick-in-progress-p)
                   (lambda (_) nil))
                  ((symbol-function 'agent-repl--current-head-sha)
                   (lambda (_) "def456"))
                  ((symbol-function 'agent-repl--workspace-merge-into-source)
                   (lambda (ws &rest _) (setq called ws))))
          (agent-repl--drain-merge-queue)
          (should (equal called "ws1"))
          (should (= 0 (length agent-repl--merge-queue))))))))

(ert-deftest agent-repl-test-drain-merge-queue-proceeds-when-no-recorded-head ()
  "An entry that has never been attempted (no recorded
`:last-attempt-target-head') must not trip the loop guard — that field
is only set by `--reenqueue-merge-on-failure', not by normal first-time
enqueues."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-empty-merge-queue
      (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
      (cl-letf (((symbol-function 'agent-repl--path-canonical) #'identity))
        (agent-repl--enqueue-merge "ws1" t t "/tmp/target"))
      (let ((called nil))
        (cl-letf (((symbol-function 'agent-repl--path-canonical) #'identity)
                  ((symbol-function 'agent-repl--cherry-pick-in-progress-p)
                   (lambda (_) nil))
                  ((symbol-function 'agent-repl--current-head-sha)
                   (lambda (_) "abc123"))
                  ((symbol-function 'agent-repl--workspace-merge-into-source)
                   (lambda (ws &rest _) (setq called ws))))
          (agent-repl--drain-merge-queue)
          (should (equal called "ws1")))))))

;;;; ---- Tests: agent-repl-drain-merge-queue (interactive kick) ----

(ert-deftest agent-repl-test-interactive-drain-clears-halt-flag-then-drains ()
  "The interactive `agent-repl-drain-merge-queue' is the human signal
that re-dispatch should proceed.  It clears `:halt-until-human' on each
bucket's front entry and then runs the drain — the same drain that would
otherwise have halted on the flag."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-empty-merge-queue
      (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
      (setq agent-repl--merge-queue
            (list (list :source-ws "ws1" :silent t :auto-resolve t
                        :target-dir "/tmp/target" :halt-until-human t)))
      (let ((called nil))
        (cl-letf (((symbol-function 'agent-repl--path-canonical) #'identity)
                  ((symbol-function 'agent-repl--cherry-pick-in-progress-p)
                   (lambda (_) nil))
                  ((symbol-function 'agent-repl--current-head-sha)
                   (lambda (_) nil))
                  ((symbol-function 'agent-repl--workspace-merge-into-source)
                   (lambda (ws &rest _) (setq called ws))))
          (agent-repl-drain-merge-queue)
          (should (equal called "ws1"))
          (should (= 0 (length agent-repl--merge-queue))))))))

(ert-deftest agent-repl-test-interactive-drain-clears-halt-on-every-bucket ()
  "The human kick clears `:halt-until-human' on the front of EVERY target
bucket so all halted buckets become drainable in one kick."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-empty-merge-queue
      (agent-repl--ws-put "ws-a" :project-dir "/tmp/ws-a")
      (agent-repl--ws-put "ws-b" :project-dir "/tmp/ws-b")
      (setq agent-repl--merge-queue
            (list (list :source-ws "ws-a" :silent t :auto-resolve t
                        :target-dir "/tmp/target-a" :halt-until-human t)
                  (list :source-ws "ws-b" :silent t :auto-resolve t
                        :target-dir "/tmp/target-b" :halt-until-human t)))
      (let ((dispatched nil))
        (cl-letf (((symbol-function 'agent-repl--path-canonical) #'identity)
                  ((symbol-function 'agent-repl--cherry-pick-in-progress-p)
                   (lambda (_) nil))
                  ((symbol-function 'agent-repl--current-head-sha)
                   (lambda (_) nil))
                  ((symbol-function 'agent-repl--workspace-merge-into-source)
                   (lambda (ws &rest _) (push ws dispatched))))
          (agent-repl-drain-merge-queue)
          (should (equal (sort (copy-sequence dispatched) #'string<)
                         '("ws-a" "ws-b")))
          (should (null agent-repl--merge-queue)))))))

(ert-deftest agent-repl-test-interactive-drain-leaves-non-halted-front-untouched ()
  "When the front entry has no `:halt-until-human' flag, the interactive
drain runs as-is — it must not spuriously rewrite the entry."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-empty-merge-queue
      (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
      (cl-letf (((symbol-function 'agent-repl--path-canonical) #'identity))
        (agent-repl--enqueue-merge "ws1" t t "/tmp/target"))
      (let ((entry-before (car agent-repl--merge-queue))
            (dispatched nil))
        (cl-letf (((symbol-function 'agent-repl--path-canonical) #'identity)
                  ((symbol-function 'agent-repl--cherry-pick-in-progress-p)
                   (lambda (_) nil))
                  ((symbol-function 'agent-repl--current-head-sha)
                   (lambda (_) nil))
                  ((symbol-function 'agent-repl--workspace-merge-into-source)
                   (lambda (&rest _) (setq dispatched t))))
          (agent-repl-drain-merge-queue)
          (should dispatched)
          (should-not (plist-get entry-before :halt-until-human)))))))

;;;; ---- Tests: helpers for merge-async failure handling ----

(ert-deftest agent-repl-test-current-head-sha-returns-nil-for-nil-dir ()
  "Helper is robust to nil — the failure arm may have no resolved target."
  (should (null (agent-repl--current-head-sha nil))))

(ert-deftest agent-repl-test-current-head-sha-returns-nil-for-missing-dir ()
  "Helper is robust to a path that doesn't exist on disk."
  (cl-letf (((symbol-function 'file-directory-p) (lambda (_) nil)))
    (should (null (agent-repl--current-head-sha "/nonexistent/dir/xyz")))))

(ert-deftest agent-repl-test-current-head-sha-returns-rev-parse-output ()
  "Helper returns the trimmed `git rev-parse HEAD' output via the wrapper.
Mocks `--git-string' rather than spawning a real git subprocess (per
AGENTS.md `No External Processes or External State in Tests')."
  (cl-letf (((symbol-function 'file-directory-p) (lambda (_) t))
            ((symbol-function 'agent-repl--git-string)
             (lambda (&rest _)
               "deadbeefcafef00ddeadbeefcafef00ddeadbeef")))
    (should (equal (agent-repl--current-head-sha "/tmp/repo")
                   "deadbeefcafef00ddeadbeefcafef00ddeadbeef"))))

(ert-deftest agent-repl-test-format-merge-failure-prompt-embeds-error ()
  "Prompt includes the error tuple via `%S' so the agent sees the full shape
\(symbol + data) for analysis."
  (let ((prompt (agent-repl--format-merge-failure-prompt
                 '(error "boom"))))
    (should (string-match-p "boom" prompt))
    (should (string-match-p "error" prompt))))

(ert-deftest agent-repl-test-format-merge-failure-prompt-contains-workspace-merge-retry-directive ()
  "Prompt directs the agent to run /workspace-merge again — the skill's
rebase step is more likely to resolve conflicts than a raw retry."
  (let ((prompt (agent-repl--format-merge-failure-prompt
                 '(error "boom"))))
    (should (string-match-p "/workspace-merge" prompt))
    (should (string-match-p "rebase directive" prompt))))

(ert-deftest agent-repl-test-abort-cherry-pick-if-in-flight-noop-on-nil-dir ()
  "Robustness: nil dir → no-op (no call to git, no error)."
  (let ((git-called nil))
    (cl-letf (((symbol-function 'agent-repl--git-exit-code)
               (lambda (&rest _) (setq git-called t) 0)))
      (agent-repl--abort-cherry-pick-if-in-flight "ws1" nil)
      (should-not git-called))))

(ert-deftest agent-repl-test-abort-cherry-pick-if-in-flight-noop-when-no-head ()
  "When CHERRY_PICK_HEAD is absent, the helper must not call abort —
calling abort with nothing in flight emits a spurious git error."
  (let ((git-called nil))
    (cl-letf (((symbol-function 'file-directory-p) (lambda (_) t))
              ((symbol-function 'agent-repl--cherry-pick-in-progress-p)
               (lambda (_) nil))
              ((symbol-function 'agent-repl--git-exit-code)
               (lambda (&rest _) (setq git-called t) 0)))
      (agent-repl--abort-cherry-pick-if-in-flight "ws1" "/tmp/target")
      (should-not git-called))))

(ert-deftest agent-repl-test-abort-cherry-pick-if-in-flight-runs-abort-when-head-exists ()
  "When CHERRY_PICK_HEAD exists at dir, helper invokes
`git -C dir cherry-pick --abort'."
  (let ((args nil))
    (cl-letf (((symbol-function 'file-directory-p) (lambda (_) t))
              ((symbol-function 'agent-repl--cherry-pick-in-progress-p)
               (lambda (_) t))
              ((symbol-function 'agent-repl--git-exit-code)
               (lambda (dir &rest a) (setq args (cons dir a)) 0)))
      (agent-repl--abort-cherry-pick-if-in-flight "ws1" "/tmp/target")
      (should (equal args '("/tmp/target" "cherry-pick" "--abort"))))))

(ert-deftest agent-repl-test-reenqueue-merge-on-failure-back-on-conflict-rejection ()
  "Conflict-rejection re-enqueue appends to the BACK of the queue and
does NOT set `:halt-until-human' — auto-drain may proceed to siblings."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-empty-merge-queue
      (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
      (setq agent-repl--merge-queue
            (list (list :source-ws "ws-existing" :silent t :auto-resolve t)))
      (cl-letf (((symbol-function 'agent-repl--current-head-sha)
                 (lambda (_) "abc"))
                ((symbol-function 'agent-repl--persist-merge-queue) #'ignore))
        (agent-repl--reenqueue-merge-on-failure "ws1" t "/tmp/target"))
      (should (equal (mapcar (lambda (e) (plist-get e :source-ws))
                             agent-repl--merge-queue)
                     '("ws-existing" "ws1")))
      (should-not (plist-get (nth 1 agent-repl--merge-queue)
                             :halt-until-human))
      (should (equal (plist-get (nth 1 agent-repl--merge-queue)
                                :last-attempt-target-head)
                     "abc")))))

(ert-deftest agent-repl-test-reenqueue-merge-on-failure-front-on-generic ()
  "Generic failure re-enqueue prepends to the FRONT and sets
`:halt-until-human t' so auto-drain stops there."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-empty-merge-queue
      (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
      (setq agent-repl--merge-queue
            (list (list :source-ws "ws-existing" :silent t :auto-resolve t)))
      (cl-letf (((symbol-function 'agent-repl--current-head-sha)
                 (lambda (_) "abc"))
                ((symbol-function 'agent-repl--persist-merge-queue) #'ignore))
        (agent-repl--reenqueue-merge-on-failure "ws1" nil "/tmp/target"))
      (should (equal (mapcar (lambda (e) (plist-get e :source-ws))
                             agent-repl--merge-queue)
                     '("ws1" "ws-existing")))
      (should (plist-get (car agent-repl--merge-queue) :halt-until-human)))))

(ert-deftest agent-repl-test-reenqueue-merge-on-failure-marks-ws-merge-queued ()
  "Re-enqueue marks the workspace with `:repl-state :merge-queued' so the
drawer routes it under MERGING with the queued badge."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-empty-merge-queue
      (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
      (agent-repl--ws-put "ws1" :agent-state :thinking)
      (cl-letf (((symbol-function 'agent-repl--current-head-sha)
                 (lambda (_) nil))
                ((symbol-function 'agent-repl--persist-merge-queue) #'ignore))
        (agent-repl--reenqueue-merge-on-failure "ws1" t "/tmp/target"))
      (should (eq (agent-repl--ws-get "ws1" :repl-state) :merge-queued))
      ;; Agent-state cleared so the queued badge wins the glyph precedence.
      (should (null (agent-repl--ws-get "ws1" :agent-state))))))

;;;; ---- Tests: workspace-merge-into-source stashes resolved target dir ----

(ert-deftest agent-repl-test-workspace-merge-into-source-stashes-resolved-target-dir ()
  "After successful target resolution, `--workspace-merge-into-source'
stashes the target dir on the source workspace plist so the failure
handler in `--workspace-merge-async' can run `cherry-pick --abort' and
record the loop-guard head without redoing target resolution from the
worker thread."
  (agent-repl-test--with-clean-state
    (let ((merge-do-args nil))
      (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
      (agent-repl--ws-put "ws1" :source-ws-dir "/tmp/parent")
      (cl-letf (((symbol-function 'agent-repl--master-worktree-path)
                 (lambda (_) "/tmp/master"))
                ((symbol-function 'agent-repl--resolve-merge-into-source-target)
                 (lambda (parent _master) parent))
                ((symbol-function 'agent-repl--path-canonical) #'identity)
                ((symbol-function 'file-directory-p) (lambda (_) t))
                ((symbol-function 'agent-repl--cherry-pick-in-progress-p)
                 (lambda (_) nil))
                ((symbol-function 'agent-repl--assert-clean-worktree) #'ignore)
                ((symbol-function 'agent-repl--git-branch-of-dir)
                 (lambda (_) "DWC/parent-branch"))
                ((symbol-function 'agent-repl--workspace-merge-do)
                 (lambda (ws target &rest _)
                   (push (list ws target) merge-do-args))))
        (agent-repl--workspace-merge-into-source "ws1" t t)
        (should (equal (agent-repl--ws-get "ws1" :resolved-target-dir)
                       "/tmp/parent"))
        (should (equal (agent-repl--ws-get "ws1" :merge-target-name)
                       "DWC/parent-branch"))
        (should (equal merge-do-args '(("ws1" "/tmp/parent"))))))))

;;;; ---- Tests: agent-repl-create-explanation-engine-oneshot-workspace ----

(ert-deftest agent-repl-test-explanation-engine-oneshot-pins-git-root-to-explanation-engine ()
  "The explanation-engine one-shot pins git-root to
`~/workspace/ChessCom/explanation-engine' regardless of the calling
workspace's project, so SPC j O always dispatches into that repo."
  (agent-repl-test--with-clean-state
    (let ((captured-git-root :unset))
      (cl-letf (((symbol-function '+workspace-current-name)
                 (lambda () "unrelated-ws"))
                ((symbol-function 'agent-repl--ws-dir)
                 (lambda (_ws) "/tmp/unrelated-repo/"))
                ((symbol-function 'read-from-minibuffer)
                 (lambda (&rest _) "add caching to thing"))
                ((symbol-function 'agent-repl--spawn-workspace-generation)
                 (lambda (_raw _prefixed git-root _base _fork-from &optional _force-sandbox)
                   (setq captured-git-root git-root))))
        (agent-repl-create-explanation-engine-oneshot-workspace)
        (should (equal captured-git-root
                       agent-repl--explanation-engine-dir))
        (should (equal captured-git-root
                       (file-name-as-directory
                        (expand-file-name
                         "~/workspace/ChessCom/explanation-engine"))))))))

(ert-deftest agent-repl-test-explanation-engine-oneshot-uses-master-base ()
  "The explanation-engine one-shot branches off local `master'
(equivalent to `SPC TAB N' in that repo)."
  (agent-repl-test--with-clean-state
    (let ((captured-base :unset))
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (&rest _) "add caching to thing"))
                ((symbol-function 'agent-repl--spawn-workspace-generation)
                 (lambda (_raw _prefixed _git-root base _fork-from &optional _force-sandbox)
                   (setq captured-base base))))
        (agent-repl-create-explanation-engine-oneshot-workspace)
        (should (equal captured-base "master"))))))

(ert-deftest agent-repl-test-explanation-engine-oneshot-appends-create-pr-suffix-to-prefixed ()
  "The create-PR-on-success suffix is included in the PREFIXED prompt so
the spawned agent knows to invoke
`agent-repl--oneshot-create-pr-command' on success — this replaces the
`/workspace-merge' instruction used by the doom one-shot."
  (agent-repl-test--with-clean-state
    (let ((captured-prefixed :unset))
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (&rest _) "add caching to thing"))
                ((symbol-function 'agent-repl--spawn-workspace-generation)
                 (lambda (_raw prefixed _git-root _base _fork-from &optional _force-sandbox)
                   (setq captured-prefixed prefixed))))
        (agent-repl-create-explanation-engine-oneshot-workspace)
        (should (string-match-p
                 (regexp-quote agent-repl--oneshot-create-pr-command)
                 captured-prefixed))
        (should (string-match-p
                 (regexp-quote agent-repl--oneshot-create-pr-suffix)
                 captured-prefixed))))))

(ert-deftest agent-repl-test-explanation-engine-oneshot-chains-workspace-merge-after-create-pr ()
  "The explanation-engine one-shot chains `/workspace-merge' AFTER
`/create-or-update-pr' as a second-stage teardown — the prefixed prompt
must mention `/workspace-merge', and it must appear textually AFTER the
`/create-or-update-pr' reference so the chain reads chronologically
(implement → PR → CICD → workspace-merge)."
  (agent-repl-test--with-clean-state
    (let ((captured-prefixed :unset))
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (&rest _) "add caching to thing"))
                ((symbol-function 'agent-repl--spawn-workspace-generation)
                 (lambda (_raw prefixed _git-root _base _fork-from &optional _force-sandbox)
                   (setq captured-prefixed prefixed))))
        (agent-repl-create-explanation-engine-oneshot-workspace)
        (let ((pr-pos (string-match
                       (regexp-quote agent-repl--oneshot-create-pr-command)
                       captured-prefixed))
              (merge-pos (string-match "/workspace-merge" captured-prefixed)))
          (should pr-pos)
          (should merge-pos)
          (should (< pr-pos merge-pos)))))))

(ert-deftest agent-repl-test-explanation-engine-oneshot-keeps-raw-prompt-clean ()
  "The create-PR suffix is NOT appended to the raw prompt — raw is used
purely for slug generation and should not get polluted with slash
commands like `/create-or-update-pr', which would derail the slug."
  (agent-repl-test--with-clean-state
    (let ((captured-raw :unset))
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (&rest _) "add caching to thing"))
                ((symbol-function 'agent-repl--spawn-workspace-generation)
                 (lambda (raw _prefixed _git-root _base _fork-from &optional _force-sandbox)
                   (setq captured-raw raw))))
        (agent-repl-create-explanation-engine-oneshot-workspace)
        (should (equal captured-raw "add caching to thing"))
        (should-not (string-match-p "/create-or-update-pr"
                                    captured-raw))))))

(ert-deftest agent-repl-test-explanation-engine-oneshot-prefixed-includes-autonomous-prefix ()
  "The prefixed prompt still starts with the standard autonomous-prompt
prefix so the spawned agent runs autonomously without waiting."
  (agent-repl-test--with-clean-state
    (let ((captured-prefixed :unset))
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (&rest _) "add caching to thing"))
                ((symbol-function 'agent-repl--spawn-workspace-generation)
                 (lambda (_raw prefixed _git-root _base _fork-from &optional _force-sandbox)
                   (setq captured-prefixed prefixed))))
        (agent-repl-create-explanation-engine-oneshot-workspace)
        (should (string-prefix-p (agent-repl--meta-wrap agent-repl--autonomous-prompt-prefix)
                                 captured-prefixed))))))

(ert-deftest agent-repl-test-explanation-engine-oneshot-rejects-empty-prompt ()
  "An empty/whitespace prompt is rejected — there is nothing to slug or
implement, and we do not want to spawn a useless workspace."
  (agent-repl-test--with-clean-state
    (let ((spawned nil))
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (&rest _) "   "))
                ((symbol-function 'agent-repl--spawn-workspace-generation)
                 (lambda (&rest _) (setq spawned t))))
        (should-error
         (agent-repl-create-explanation-engine-oneshot-workspace)
         :type 'user-error)
        (should-not spawned)))))

(ert-deftest agent-repl-test-explanation-engine-oneshot-passes-no-fork-from ()
  "The explanation-engine one-shot is not a fork — fork-from must be nil
so the new workspace starts a fresh agent session rather than resuming
someone else's."
  (agent-repl-test--with-clean-state
    (let ((captured-fork-from :unset))
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (&rest _) "add caching to thing"))
                ((symbol-function 'agent-repl--spawn-workspace-generation)
                 (lambda (_raw _prefixed _git-root _base fork-from &optional _force-sandbox)
                   (setq captured-fork-from fork-from))))
        (agent-repl-create-explanation-engine-oneshot-workspace)
        (should (null captured-fork-from))))))

(ert-deftest agent-repl-test-explanation-engine-oneshot-passes-no-force-sandbox ()
  "The explanation-engine one-shot does NOT pass force-sandbox — that repo
uses bare-metal Claude, not the Docker sandbox."
  (agent-repl-test--with-clean-state
    (let ((captured-force-sandbox :unset))
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (&rest _) "add caching to thing"))
                ((symbol-function 'agent-repl--spawn-workspace-generation)
                 (lambda (_raw _prefixed _git-root _base _fork-from &optional force-sandbox)
                   (setq captured-force-sandbox force-sandbox))))
        (agent-repl-create-explanation-engine-oneshot-workspace)
        (should (null captured-force-sandbox))))))

(ert-deftest agent-repl-test-oneshot-create-pr-command-has-expected-flags ()
  "The PR command string must match exactly what the user specified for
the explanation-engine one-shot: `/create-or-update-pr --patch
--add-to-merge-queue --rebase' (no --self-certified, no `commit'
subcommand)."
  (should (equal agent-repl--oneshot-create-pr-command
                 "/create-or-update-pr --patch --add-to-merge-queue --rebase")))

(ert-deftest agent-repl-test-oneshot-create-pr-suffix-mentions-stop-on-ambiguity ()
  "The create-PR suffix tells the spawned agent to STOP (not push on)
when it hits genuine ambiguity it cannot resolve — same safety property
as the doom-oneshot merge suffix, so a faulty implementation isn't
auto-PRed."
  (should (string-match-p "STOP" agent-repl--oneshot-create-pr-suffix))
  (should (string-match-p "ambiguity"
                          agent-repl--oneshot-create-pr-suffix)))

(ert-deftest agent-repl-test-oneshot-create-pr-suffix-mentions-tests-and-commits ()
  "PR creation is gated on implementation, tests, AND commits — the
suffix must spell that out so the spawned agent doesn't PR half-finished
work."
  (should (string-match-p "tests"
                          agent-repl--oneshot-create-pr-suffix))
  (should (string-match-p "[Cc]ommit"
                          agent-repl--oneshot-create-pr-suffix)))

(ert-deftest agent-repl-test-explanation-engine-dir-points-to-chesscom-explanation-engine ()
  "Sanity check: the explanation-engine dir constant resolves to
`~/workspace/ChessCom/explanation-engine' with a trailing slash."
  (should (equal agent-repl--explanation-engine-dir
                 (file-name-as-directory
                  (expand-file-name
                   "~/workspace/ChessCom/explanation-engine")))))

;;;; ---- Tests: agent-repl--oneshot-create-pr-then-merge-followup ----

(ert-deftest agent-repl-test-oneshot-create-pr-then-merge-followup-mentions-workspace-merge ()
  "The follow-up clause must reference `/workspace-merge' — that's the
slash command the spawned agent invokes once CICD passes."
  (should (string-match-p "/workspace-merge"
                          agent-repl--oneshot-create-pr-then-merge-followup)))

(ert-deftest agent-repl-test-oneshot-create-pr-then-merge-followup-gates-on-check-cicd-pass ()
  "The follow-up clause must explicitly gate `/workspace-merge' on
`/check-cicd' returning PASS — without this gate the agent could tear
down the workspace even after a failing CI run."
  (should (string-match-p "/check-cicd"
                          agent-repl--oneshot-create-pr-then-merge-followup))
  (should (string-match-p "PASS"
                          agent-repl--oneshot-create-pr-then-merge-followup)))

(ert-deftest agent-repl-test-oneshot-create-pr-then-merge-followup-stops-on-check-cicd-fail ()
  "On CICD FAIL the follow-up clause must tell the agent to STOP and NOT
invoke `/workspace-merge' — otherwise a failing CI could still lead to a
workspace teardown that loses the editor state without the change landing."
  (should (string-match-p "FAIL"
                          agent-repl--oneshot-create-pr-then-merge-followup))
  (should (string-match-p "STOP"
                          agent-repl--oneshot-create-pr-then-merge-followup))
  ;; The "do NOT invoke /workspace-merge" instruction must appear so the
  ;; agent doesn't mis-read STOP as merely "stop the implementation" and
  ;; still fire the teardown.
  (should (string-match-p "NOT invoke `/workspace-merge`"
                          agent-repl--oneshot-create-pr-then-merge-followup)))

(ert-deftest agent-repl-test-oneshot-create-pr-then-merge-followup-references-create-pr-command ()
  "The follow-up clause must name the create-PR command it chains off —
otherwise the agent has to guess which prior invocation's CICD result
gates the workspace-merge."
  (should (string-match-p
           (regexp-quote agent-repl--oneshot-create-pr-command)
           agent-repl--oneshot-create-pr-then-merge-followup)))

;;;; ---- Tests: chained suffix integration ----

(ert-deftest agent-repl-test-oneshot-create-pr-suffix-includes-followup ()
  "The composed create-PR suffix must include the workspace-merge
follow-up clause — otherwise the chain is half-wired and the agent only
gets the first-stage gate."
  (should (string-match-p
           (regexp-quote agent-repl--oneshot-create-pr-then-merge-followup)
           agent-repl--oneshot-create-pr-suffix)))

(ert-deftest agent-repl-test-oneshot-create-pr-suffix-followup-comes-after-build-suffix ()
  "The follow-up clause must appear AFTER the build-oneshot-success-suffix
output, not before — order is load-bearing because the follow-up gates
on the first-stage invocation's CICD result."
  (let* ((first-stage (agent-repl--build-oneshot-success-suffix
                       (concat "`" agent-repl--oneshot-create-pr-command "`")
                       "push and queue this branch for merge"))
         (first-pos (string-match (regexp-quote first-stage)
                                  agent-repl--oneshot-create-pr-suffix))
         (followup-pos (string-match
                        (regexp-quote
                         agent-repl--oneshot-create-pr-then-merge-followup)
                        agent-repl--oneshot-create-pr-suffix)))
    (should first-pos)
    (should followup-pos)
    (should (< first-pos followup-pos))))

;;;; ---- Tests: agent-repl--build-oneshot-success-suffix ----

(ert-deftest agent-repl-test-build-oneshot-success-suffix-interpolates-invocation-twice ()
  "INVOCATION appears in BOTH the 'invoke X to Y' action sentence and the
'Only invoke X when ...' gate sentence — the helper must wire it through
both clauses, otherwise the gate dangles."
  (let ((suffix (agent-repl--build-oneshot-success-suffix
                 "the /foo skill" "do the foo thing")))
    (with-temp-buffer
      (insert suffix)
      (goto-char (point-min))
      (should (search-forward "invoke the /foo skill to do the foo thing"
                              nil t))
      (should (search-forward "Only invoke the /foo skill when"
                              nil t)))))

(ert-deftest agent-repl-test-build-oneshot-success-suffix-mentions-stop-on-ambiguity ()
  "Every success-suffix MUST carry the STOP-on-ambiguity safety clause —
otherwise a one-shot can auto-merge / auto-PR a faulty implementation."
  (let ((suffix (agent-repl--build-oneshot-success-suffix
                 "the /foo skill" "do the foo thing")))
    (should (string-match-p "STOP" suffix))
    (should (string-match-p "ambiguity" suffix))))

(ert-deftest agent-repl-test-build-oneshot-success-suffix-gates-on-tests-and-commits ()
  "The gate clause must require implementation AND tests AND commits, not
just implementation — the helper hard-codes this gate so every variant
inherits it."
  (let ((suffix (agent-repl--build-oneshot-success-suffix
                 "the /foo skill" "do the foo thing")))
    (should (string-match-p "tests" suffix))
    (should (string-match-p "[Cc]ommit" suffix))))

;;;; ---- Tests: agent-repl--create-pinned-oneshot-workspace ----

(ert-deftest agent-repl-test-create-pinned-oneshot-uses-tag-in-minibuffer-prompt ()
  "TAG is interpolated into the minibuffer prompt so distinct one-shot
variants are visually distinguishable when the user is typing the
preemptive prompt."
  (agent-repl-test--with-clean-state
    (let ((captured-mb-prompt :unset))
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (prompt &rest _)
                   (setq captured-mb-prompt prompt)
                   "do a thing"))
                ((symbol-function 'agent-repl--spawn-workspace-generation)
                 (lambda (&rest _) nil)))
        (agent-repl--create-pinned-oneshot-workspace
         "/tmp/repo/" 'master "SUFFIX" "test-tag")
        (should (equal captured-mb-prompt "One-shot test-tag prompt: "))))))

(ert-deftest agent-repl-test-create-pinned-oneshot-rejects-empty-prompt ()
  "Empty/whitespace prompt is rejected at the helper level so every
variant inherits the validation — no caller can accidentally skip it."
  (agent-repl-test--with-clean-state
    (let ((spawned nil))
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (&rest _) "   "))
                ((symbol-function 'agent-repl--spawn-workspace-generation)
                 (lambda (&rest _) (setq spawned t))))
        (should-error
         (agent-repl--create-pinned-oneshot-workspace
          "/tmp/repo/" 'master "SUFFIX" "test-tag")
         :type 'user-error)
        (should-not spawned)))))

(ert-deftest agent-repl-test-create-pinned-oneshot-passes-git-root-through ()
  "GIT-ROOT flows verbatim through to `agent-repl--spawn-workspace-generation'
— a caller pinning a non-default repo must see exactly that path."
  (agent-repl-test--with-clean-state
    (let ((captured-git-root :unset))
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (&rest _) "do a thing"))
                ((symbol-function 'agent-repl--spawn-workspace-generation)
                 (lambda (_raw _prefixed git-root _base _fork-from &optional _force-sandbox)
                   (setq captured-git-root git-root))))
        (agent-repl--create-pinned-oneshot-workspace
         "/tmp/some-pinned-repo/" 'master "SUFFIX" "test-tag")
        (should (equal captured-git-root "/tmp/some-pinned-repo/"))))))

(ert-deftest agent-repl-test-create-pinned-oneshot-appends-suffix-to-prefixed-only ()
  "SUFFIX is appended to the PREFIXED prompt (agent's first message) but
NOT to the RAW prompt (used for slug generation) — keeps the
workspace-name slug clean across every one-shot variant."
  (agent-repl-test--with-clean-state
    (let ((captured-raw :unset)
          (captured-prefixed :unset))
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (&rest _) "do a thing"))
                ((symbol-function 'agent-repl--spawn-workspace-generation)
                 (lambda (raw prefixed _git-root _base _fork-from &optional _force-sandbox)
                   (setq captured-raw raw)
                   (setq captured-prefixed prefixed))))
        (agent-repl--create-pinned-oneshot-workspace
         "/tmp/repo/" 'master "::SENTINEL-SUFFIX::" "test-tag")
        (should-not (string-match-p "::SENTINEL-SUFFIX::" captured-raw))
        (should (string-match-p "::SENTINEL-SUFFIX::"
                                captured-prefixed))))))

;;;; ---- Tests: one-shot prompt C-RET no-action suffix ----

(ert-deftest agent-repl-test-oneshot-no-action-suffix-value ()
  "The C-RET no-action suffix is exactly \". dont take action\" — the
leading period+space close off the user's typed prompt before the
instruction."
  (should (equal agent-repl--oneshot-no-action-suffix ". dont take action")))

(ert-deftest agent-repl-test-oneshot-prompt-insert-no-action-suffix-appends ()
  "The insert helper appends the no-action suffix to the current buffer's
contents."
  (with-temp-buffer
    (insert "do the thing")
    (agent-repl--oneshot-prompt-insert-no-action-suffix)
    (should (equal (buffer-string) "do the thing. dont take action"))))

(ert-deftest agent-repl-test-oneshot-prompt-insert-no-action-suffix-at-end ()
  "The insert helper appends at point-max even when point is not at the
end of the buffer — the suffix must land after the whole prompt."
  (with-temp-buffer
    (insert "do the thing")
    (goto-char (point-min))
    (agent-repl--oneshot-prompt-insert-no-action-suffix)
    (should (equal (buffer-string) "do the thing. dont take action"))))

(ert-deftest agent-repl-test-oneshot-prompt-submit-no-action-is-command ()
  "`agent-repl--oneshot-prompt-submit-no-action' is an interactive
command so it can be bound directly in the minibuffer keymap."
  (should (commandp 'agent-repl--oneshot-prompt-submit-no-action)))

(ert-deftest agent-repl-test-oneshot-prompt-submit-no-action-appends-suffix ()
  "The submit command appends the no-action suffix before exiting the
minibuffer."
  (with-temp-buffer
    (cl-letf (((symbol-function 'exit-minibuffer) #'ignore))
      (insert "fix the bug")
      (agent-repl--oneshot-prompt-submit-no-action)
      (should (equal (buffer-string) "fix the bug. dont take action")))))

(ert-deftest agent-repl-test-oneshot-prompt-submit-no-action-exits-minibuffer ()
  "The submit command calls `exit-minibuffer' so the suffixed prompt is
dispatched immediately."
  (with-temp-buffer
    (let ((exited nil))
      (cl-letf (((symbol-function 'exit-minibuffer)
                 (lambda () (setq exited t))))
        (insert "fix the bug")
        (agent-repl--oneshot-prompt-submit-no-action)
        (should exited)))))

(ert-deftest agent-repl-test-oneshot-prompt-map-binds-c-ret ()
  "`C-RET' in the one-shot prompt keymap dispatches the no-action submit
command."
  (should (eq (lookup-key agent-repl--oneshot-prompt-map (kbd "C-RET"))
              #'agent-repl--oneshot-prompt-submit-no-action)))

(ert-deftest agent-repl-test-oneshot-prompt-map-ret-inherits-normal-submit ()
  "`RET' in the one-shot prompt keymap resolves to the inherited
`minibuffer-local-map' binding — plain RET still submits normally."
  (should (eq (lookup-key agent-repl--oneshot-prompt-map (kbd "RET"))
              (lookup-key minibuffer-local-map (kbd "RET")))))

(ert-deftest agent-repl-test-oneshot-prompt-map-parent-is-minibuffer-local-map ()
  "The one-shot prompt keymap inherits `minibuffer-local-map' so all the
standard minibuffer editing/submit bindings remain available."
  (should (eq (keymap-parent agent-repl--oneshot-prompt-map)
              minibuffer-local-map)))

(ert-deftest agent-repl-test-create-pinned-oneshot-reads-with-oneshot-prompt-map ()
  "The preemptive prompt is read with `agent-repl--oneshot-prompt-map'
so `C-RET' is reachable while typing the one-shot prompt."
  (agent-repl-test--with-clean-state
    (let ((captured-keymap :unset))
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (_prompt _initial keymap &rest _)
                   (setq captured-keymap keymap)
                   "do a thing"))
                ((symbol-function 'agent-repl--spawn-workspace-generation)
                 (lambda (&rest _) nil)))
        (agent-repl--create-pinned-oneshot-workspace
         "/tmp/repo/" 'master "SUFFIX" "test-tag")
        (should (eq captured-keymap agent-repl--oneshot-prompt-map))))))

(ert-deftest agent-repl-test-create-pinned-oneshot-c-ret-suffix-reaches-prefixed ()
  "When the minibuffer is dispatched with `C-RET', the resulting prompt
carries the no-action suffix all the way into the prefixed prompt sent
to the spawned agent."
  (agent-repl-test--with-clean-state
    (let ((captured-prefixed :unset))
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (&rest _)
                   (concat "do a thing"
                           agent-repl--oneshot-no-action-suffix)))
                ((symbol-function 'agent-repl--spawn-workspace-generation)
                 (lambda (_raw prefixed _git-root _base _fork-from &optional _force-sandbox)
                   (setq captured-prefixed prefixed))))
        (agent-repl--create-pinned-oneshot-workspace
         "/tmp/repo/" 'master "SUFFIX" "test-tag")
        (should (string-match-p
                 (regexp-quote agent-repl--oneshot-no-action-suffix)
                 captured-prefixed))))))

(ert-deftest agent-repl-test-create-pinned-oneshot-forwards-force-sandbox ()
  "FORCE-SANDBOX passed to `agent-repl--create-pinned-oneshot-workspace'
is forwarded verbatim to `agent-repl--spawn-workspace-generation'."
  (agent-repl-test--with-clean-state
    (let ((captured-force-sandbox :unset))
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (&rest _) "do a thing"))
                ((symbol-function 'agent-repl--spawn-workspace-generation)
                 (lambda (_raw _prefixed _git-root _base _fork-from &optional force-sandbox)
                   (setq captured-force-sandbox force-sandbox))))
        (agent-repl--create-pinned-oneshot-workspace
         "/tmp/repo/" 'master "SUFFIX" "test-tag" t)
        (should captured-force-sandbox)))))

(ert-deftest agent-repl-test-create-pinned-oneshot-no-force-sandbox-by-default ()
  "When FORCE-SANDBOX is omitted, `agent-repl--spawn-workspace-generation'
receives nil — the default is bare-metal for repos that do not opt in."
  (agent-repl-test--with-clean-state
    (let ((captured-force-sandbox :unset))
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (&rest _) "do a thing"))
                ((symbol-function 'agent-repl--spawn-workspace-generation)
                 (lambda (_raw _prefixed _git-root _base _fork-from &optional force-sandbox)
                   (setq captured-force-sandbox force-sandbox))))
        (agent-repl--create-pinned-oneshot-workspace
         "/tmp/repo/" 'master "SUFFIX" "test-tag")
        (should (null captured-force-sandbox))))))

;;;; ---- Tests: eval-code-string ----

(ert-deftest agent-repl-test-eval-code-string-returns-value-string ()
  "Successful eval populates `:value-string' with `prin1-to-string' of result."
  (let ((result (agent-repl--eval-code-string "(+ 1 2)")))
    (should (equal "3" (plist-get result :value-string)))
    (should (null (plist-get result :error)))))

(ert-deftest agent-repl-test-eval-code-string-captures-printed-output ()
  "`princ' inside the form is captured into `:printed'."
  (let ((result (agent-repl--eval-code-string "(princ \"hello\")")))
    (should (string-match-p "hello" (plist-get result :printed)))))

(ert-deftest agent-repl-test-eval-code-string-evaluates-multiple-forms ()
  "Multiple top-level forms evaluate in order; `:value-string' tracks the last."
  (let ((result (agent-repl--eval-code-string "(princ \"a\") (+ 10 20)")))
    (should (equal "30" (plist-get result :value-string)))
    (should (string-match-p "a" (plist-get result :printed)))))

(ert-deftest agent-repl-test-eval-code-string-traps-error ()
  "Errors are trapped into `:error' instead of propagating."
  (let ((result (agent-repl--eval-code-string "(error \"boom\")")))
    (should (null (plist-get result :value-string)))
    (should (stringp (plist-get result :error)))
    (should (string-match-p "boom" (plist-get result :error)))))

(ert-deftest agent-repl-test-eval-code-string-error-preserves-prior-output ()
  "When form N raises, prior forms' printed output is still in `:printed'."
  (let ((result (agent-repl--eval-code-string "(princ \"first\") (error \"boom\")")))
    (should (stringp (plist-get result :error)))
    (should (string-match-p "first" (plist-get result :printed)))))

(ert-deftest agent-repl-test-eval-code-string-empty-input-yields-nil-value ()
  "Whitespace-only code yields a `nil' value string and no error."
  (let ((result (agent-repl--eval-code-string "   ")))
    (should (equal "nil" (plist-get result :value-string)))
    (should (null (plist-get result :error)))))

;;;; ---- Tests: eval-format-prompt ----

(ert-deftest agent-repl-test-eval-format-prompt-success-includes-result ()
  "Success path includes the `;; result:' section with the prin1 value."
  (let ((text (agent-repl--eval-format-prompt "(+ 1 2)" nil "" "3" nil)))
    (should (string-match-p ";; code:" text))
    (should (string-match-p ";; result:" text))
    (should (string-match-p "3" text))
    (should-not (string-match-p ";; error:" text))))

(ert-deftest agent-repl-test-eval-format-prompt-error-includes-error-section ()
  "Error path omits `;; result:' and includes `;; error:' instead."
  (let ((text (agent-repl--eval-format-prompt "(error \"boom\")" nil "" nil "boom")))
    (should (string-match-p ";; error:" text))
    (should (string-match-p "boom" text))
    (should-not (string-match-p ";; result:" text))))

(ert-deftest agent-repl-test-eval-format-prompt-omits-empty-printed-section ()
  "Empty `printed' output is not echoed back as a `;; printed:' section."
  (let ((text (agent-repl--eval-format-prompt "(+ 1 2)" nil "" "3" nil)))
    (should-not (string-match-p ";; printed:" text))))

(ert-deftest agent-repl-test-eval-format-prompt-includes-printed-when-present ()
  "Non-empty `printed' output renders as a `;; printed:' section."
  (let ((text (agent-repl--eval-format-prompt
               "(princ \"hi\")" nil "hi" "\"hi\"" nil)))
    (should (string-match-p ";; printed:" text))
    (should (string-match-p "hi" text))))

(ert-deftest agent-repl-test-eval-format-prompt-note-renders-in-header ()
  "A non-empty `note' is appended to the header line."
  (let ((text (agent-repl--eval-format-prompt
               "(+ 1 2)" "warmup" "" "3" nil)))
    (should (string-match-p "note: warmup" text))))

(ert-deftest agent-repl-test-eval-format-prompt-error-header-says-error ()
  "Error variant uses `Elisp eval ERROR' header rather than `Elisp eval result'."
  (let ((text (agent-repl--eval-format-prompt
               "(error \"x\")" nil "" nil "x")))
    (should (string-match-p "Elisp eval ERROR" text))
    (should-not (string-match-p "Elisp eval result" text))))

;;;; ---- Tests: eval-truncate ----

(ert-deftest agent-repl-test-eval-truncate-under-cap-returns-unchanged ()
  "Text shorter than the cap passes through verbatim."
  (let ((agent-repl-eval-output-max-chars 100))
    (should (equal "short" (agent-repl--eval-truncate "short")))))

(ert-deftest agent-repl-test-eval-truncate-over-cap-clips-and-annotates ()
  "Text longer than the cap is clipped and gets a `[truncated to N chars]' marker."
  (let* ((agent-repl-eval-output-max-chars 5)
         (out (agent-repl--eval-truncate "abcdefghij")))
    (should (string-prefix-p "abcde" out))
    (should (string-match-p "truncated to 5 chars" out))))

(ert-deftest agent-repl-test-eval-truncate-zero-cap-disables-truncation ()
  "A cap of 0 returns the input unchanged regardless of length."
  (let ((agent-repl-eval-output-max-chars 0))
    (should (equal "abcdefghij"
                   (agent-repl--eval-truncate "abcdefghij")))))

;;;; ---- Tests: handle-eval-command ----

(ert-deftest agent-repl-test-handle-eval-command-sends-result-to-workspace ()
  "`workspace' field routes the formatted result back via `agent-repl--send'."
  (let ((sent nil))
    (cl-letf (((symbol-function 'agent-repl--send)
               (lambda (prompt ws &rest _) (push (cons ws prompt) sent))))
      (agent-repl--handle-eval-command
       '((type . "eval") (code . "(+ 1 2)") (workspace . "ws1"))))
    (should (= 1 (length sent)))
    (should (equal "ws1" (car (car sent))))
    (should (string-match-p ";; result:" (cdr (car sent))))
    (should (string-match-p "3" (cdr (car sent))))))

(ert-deftest agent-repl-test-handle-eval-command-no-workspace-no-send ()
  "Without `workspace', the result is computed but never sent."
  (let ((sent nil))
    (cl-letf (((symbol-function 'agent-repl--send)
               (lambda (&rest args) (push args sent))))
      (agent-repl--handle-eval-command
       '((type . "eval") (code . "(+ 1 2)"))))
    (should-not sent)))

(ert-deftest agent-repl-test-handle-eval-command-empty-workspace-no-send ()
  "Empty-string `workspace' is treated as absent — no send is dispatched."
  (let ((sent nil))
    (cl-letf (((symbol-function 'agent-repl--send)
               (lambda (&rest args) (push args sent))))
      (agent-repl--handle-eval-command
       '((type . "eval") (code . "(+ 1 2)") (workspace . ""))))
    (should-not sent)))

(ert-deftest agent-repl-test-handle-eval-command-missing-code-skips ()
  "Missing `code' field skips evaluation entirely — no send, no crash."
  (let ((sent nil))
    (cl-letf (((symbol-function 'agent-repl--send)
               (lambda (&rest args) (push args sent))))
      (agent-repl--handle-eval-command
       '((type . "eval") (workspace . "ws1"))))
    (should-not sent)))

(ert-deftest agent-repl-test-handle-eval-command-empty-code-skips ()
  "Empty-string `code' is treated as missing — no send is dispatched."
  (let ((sent nil))
    (cl-letf (((symbol-function 'agent-repl--send)
               (lambda (&rest args) (push args sent))))
      (agent-repl--handle-eval-command
       '((type . "eval") (code . "   ") (workspace . "ws1"))))
    (should-not sent)))

(ert-deftest agent-repl-test-handle-eval-command-error-sends-error-prompt ()
  "An error inside the evaluated code is reported back as the `;; error:' section."
  (let ((sent nil))
    (cl-letf (((symbol-function 'agent-repl--send)
               (lambda (prompt ws &rest _) (push (cons ws prompt) sent))))
      (agent-repl--handle-eval-command
       '((type . "eval") (code . "(error \"boom\")") (workspace . "ws1"))))
    (should (= 1 (length sent)))
    (should (string-match-p "Elisp eval ERROR" (cdr (car sent))))
    (should (string-match-p "boom" (cdr (car sent))))))

(ert-deftest agent-repl-test-handle-eval-command-note-passed-through ()
  "Optional `note' field is echoed back in the response header."
  (let ((sent nil))
    (cl-letf (((symbol-function 'agent-repl--send)
               (lambda (prompt ws &rest _) (push (cons ws prompt) sent))))
      (agent-repl--handle-eval-command
       '((type . "eval") (code . "(+ 1 2)") (workspace . "ws1") (note . "tick"))))
    (should (string-match-p "note: tick" (cdr (car sent))))))

;;;; ---- Tests: amended-oneshot tracking + queue ----

(defmacro agent-repl-test--with-oneshot-tracking-state (&rest body)
  "Execute BODY with empty global amended-oneshot tracking + queue.
Rebinds `agent-repl--oneshot-last-ws' and
`agent-repl--oneshot-amended-prompts' to nil so each test starts from a
known-clean baseline regardless of any leftover state from a prior test.

Also rebinds `agent-repl-oneshot-generation-backstop-seconds' to nil
so the default reset path doesn't `run-at-time' a real 600s timer in
the test run (that would leak persistent timers across the suite).
Tests that exercise the backstop scheduling explicitly set it
themselves AND stub `run-at-time' to observe the schedule call."
  (declare (indent 0))
  `(let ((agent-repl--oneshot-last-ws nil)
         (agent-repl--oneshot-amended-prompts nil)
         (agent-repl-oneshot-generation-backstop-seconds nil))
     ,@body))

(ert-deftest agent-repl-test-oneshot-flavor-for-git-root-doom ()
  "`--oneshot-flavor-for-git-root' returns `:doom' for the pinned doom dir."
  (should (eq (agent-repl--oneshot-flavor-for-git-root
               agent-repl--doom-config-dir)
              :doom)))

(ert-deftest agent-repl-test-oneshot-flavor-for-git-root-explanation-engine ()
  "`--oneshot-flavor-for-git-root' returns `:explanation-engine' for the
pinned explanation-engine dir."
  (should (eq (agent-repl--oneshot-flavor-for-git-root
               agent-repl--explanation-engine-dir)
              :explanation-engine)))

(ert-deftest agent-repl-test-oneshot-flavor-for-git-root-unrelated-nil ()
  "Unrelated git roots return nil — only the two pinned dirs are recognized."
  (should-not (agent-repl--oneshot-flavor-for-git-root "/tmp/unrelated/")))

(ert-deftest agent-repl-test-oneshot-flavor-for-git-root-nil ()
  "Passing nil returns nil rather than signaling — defensive against
unset git-root args from upstream callers."
  (should-not (agent-repl--oneshot-flavor-for-git-root nil)))

(ert-deftest agent-repl-test-oneshot-reset-flavor-sets-generating ()
  "`--oneshot-reset-flavor' sets the flavor's last-ws entry to `:generating'."
  (agent-repl-test--with-oneshot-tracking-state
    (agent-repl--oneshot-reset-flavor :doom)
    (should (eq (plist-get agent-repl--oneshot-last-ws :doom) :generating))))

(ert-deftest agent-repl-test-oneshot-reset-flavor-clears-queue ()
  "Reset clears any previously queued amended prompts for the flavor —
each new `SPC j o' starts with a fresh queue."
  (agent-repl-test--with-oneshot-tracking-state
    (setq agent-repl--oneshot-amended-prompts
          (plist-put agent-repl--oneshot-amended-prompts :doom '("old")))
    (agent-repl--oneshot-reset-flavor :doom)
    (should (null (plist-get agent-repl--oneshot-amended-prompts :doom)))))

(ert-deftest agent-repl-test-oneshot-reset-flavor-nil-is-noop ()
  "Reset is a no-op when flavor is nil (e.g. caller passed an
unrecognized git-root) — does not corrupt the plist."
  (agent-repl-test--with-oneshot-tracking-state
    (agent-repl--oneshot-reset-flavor nil)
    (should (null agent-repl--oneshot-last-ws))
    (should (null agent-repl--oneshot-amended-prompts))))

(ert-deftest agent-repl-test-oneshot-track-workspace-records-dirname ()
  "`--oneshot-track-workspace' replaces the `:generating' sentinel with
the new workspace's dirname when path matches the flavor."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-oneshot-tracking-state
      (agent-repl--oneshot-reset-flavor :doom)
      (agent-repl--oneshot-track-workspace agent-repl--doom-config-dir "doom-ws-1")
      (should (equal (plist-get agent-repl--oneshot-last-ws :doom) "doom-ws-1")))))

(ert-deftest agent-repl-test-oneshot-track-workspace-drains-amended ()
  "Track drains queued amended prompts onto the workspace's
`:pending-prompts' and clears the global queue."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-oneshot-tracking-state
      (agent-repl--oneshot-reset-flavor :doom)
      (setq agent-repl--oneshot-amended-prompts
            (plist-put agent-repl--oneshot-amended-prompts
                       :doom '("amend-1" "amend-2")))
      (agent-repl--ws-put "doom-ws-1" :pending-prompts '("preemptive"))
      (agent-repl--oneshot-track-workspace agent-repl--doom-config-dir "doom-ws-1")
      (should (equal (agent-repl--ws-get "doom-ws-1" :pending-prompts)
                     '("preemptive" "amend-1" "amend-2")))
      (should (null (plist-get agent-repl--oneshot-amended-prompts :doom))))))

(ert-deftest agent-repl-test-oneshot-track-workspace-unrelated-path-noop ()
  "Track is a no-op when path is not a pinned-oneshot dir — non-oneshot
worktrees flowing through `--finalize-worktree-workspace' do not
clobber the tracker."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-oneshot-tracking-state
      (agent-repl--oneshot-reset-flavor :doom)
      (agent-repl--oneshot-track-workspace "/tmp/unrelated/" "other-ws")
      (should (eq (plist-get agent-repl--oneshot-last-ws :doom) :generating)))))

(ert-deftest agent-repl-test-oneshot-track-workspace-not-generating-noop ()
  "Track is a no-op when the flavor isn't currently `:generating' (e.g.
a non-oneshot worktree was created inside the pinned doom dir) — the
previously recorded dirname is left intact."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-oneshot-tracking-state
      (setq agent-repl--oneshot-last-ws
            (plist-put agent-repl--oneshot-last-ws :doom "prev-ws"))
      (agent-repl--oneshot-track-workspace agent-repl--doom-config-dir "intruder")
      (should (equal (plist-get agent-repl--oneshot-last-ws :doom) "prev-ws")))))

;;;; ---- Tests: --oneshot-clear-flavor-on-failure + backstop timer ----

(ert-deftest agent-repl-test-oneshot-clear-flavor-on-failure-resets-generating ()
  "Clears `:generating' state when the flavor is in-flight."
  (agent-repl-test--with-oneshot-tracking-state
    (setq agent-repl--oneshot-last-ws
          (plist-put agent-repl--oneshot-last-ws :doom :generating))
    (agent-repl--oneshot-clear-flavor-on-failure :doom :agent-p-failed)
    (should (null (plist-get agent-repl--oneshot-last-ws :doom)))))

(ert-deftest agent-repl-test-oneshot-clear-flavor-on-failure-drops-queued-amends ()
  "Drops any prompts queued under `--oneshot-amended-prompts' for the
failed flavor — their workspace will never exist, so they must not
later leak onto an unrelated workspace for the same flavor."
  (agent-repl-test--with-oneshot-tracking-state
    (setq agent-repl--oneshot-last-ws
          (plist-put agent-repl--oneshot-last-ws :doom :generating))
    (setq agent-repl--oneshot-amended-prompts
          (plist-put agent-repl--oneshot-amended-prompts :doom '("queued-a" "queued-b")))
    (agent-repl--oneshot-clear-flavor-on-failure :doom :agent-p-failed)
    (should (null (plist-get agent-repl--oneshot-amended-prompts :doom)))))

(ert-deftest agent-repl-test-oneshot-clear-flavor-on-failure-noop-on-non-generating ()
  "No-op when the flavor is not currently `:generating' — either the
success path already moved it to a real dirname (don't clobber the
recorded workspace) or the slot was already nil."
  (agent-repl-test--with-oneshot-tracking-state
    (setq agent-repl--oneshot-last-ws
          (plist-put agent-repl--oneshot-last-ws :doom "real-ws"))
    (agent-repl--oneshot-clear-flavor-on-failure :doom :backstop-timeout)
    (should (equal (plist-get agent-repl--oneshot-last-ws :doom) "real-ws"))))

(ert-deftest agent-repl-test-oneshot-clear-flavor-on-failure-nil-flavor-is-noop ()
  "Passing nil FLAVOR is a no-op — defensive against
`--oneshot-flavor-for-git-root' returning nil for non-pinned dirs."
  (agent-repl-test--with-oneshot-tracking-state
    (setq agent-repl--oneshot-last-ws
          (plist-put agent-repl--oneshot-last-ws :doom :generating))
    (agent-repl--oneshot-clear-flavor-on-failure nil :agent-p-failed)
    (should (eq (plist-get agent-repl--oneshot-last-ws :doom) :generating))))

(ert-deftest agent-repl-test-oneshot-reset-flavor-schedules-backstop ()
  "When `agent-repl-oneshot-generation-backstop-seconds' is non-nil,
`--oneshot-reset-flavor' calls `run-at-time' with that delay and the
clear-on-failure callback so a lost spawn cannot wedge the flavor."
  (agent-repl-test--with-oneshot-tracking-state
    (let ((scheduled nil)
          (agent-repl-oneshot-generation-backstop-seconds 600))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (delay _repeat fn &rest args)
                   (setq scheduled (list :delay delay :fn fn :args args)))))
        (agent-repl--oneshot-reset-flavor :doom)
        (should (= 600 (plist-get scheduled :delay)))
        (should (eq #'agent-repl--oneshot-clear-flavor-on-failure
                    (plist-get scheduled :fn)))
        (should (equal '(:doom :backstop-timeout) (plist-get scheduled :args)))))))

(ert-deftest agent-repl-test-oneshot-reset-flavor-nil-backstop-disables-timer ()
  "When the backstop is `nil', reset-flavor does NOT call `run-at-time'."
  (agent-repl-test--with-oneshot-tracking-state
    (let ((scheduled nil)
          (agent-repl-oneshot-generation-backstop-seconds nil))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (&rest _args) (setq scheduled t))))
        (agent-repl--oneshot-reset-flavor :doom)
        (should-not scheduled)))))

;;;; ---- Tests: --workspace-generation-finalize failure → flavor clear ----

(ert-deftest agent-repl-test-workspace-generation-finalize-nonzero-clears-matching-flavor ()
  "Non-zero exit + GIT-ROOT matching a pinned oneshot dir → eagerly clear
the flavor's `:generating' state via `--oneshot-clear-flavor-on-failure'."
  (agent-repl-test--with-oneshot-tracking-state
    (setq agent-repl--oneshot-last-ws
          (plist-put agent-repl--oneshot-last-ws :doom :generating))
    (cl-letf (((symbol-function 'message) #'ignore))
      (agent-repl--workspace-generation-finalize
       "gen-id-1" 1 "exited abnormally\n" "" agent-repl--doom-config-dir))
    (should (null (plist-get agent-repl--oneshot-last-ws :doom)))))

(ert-deftest agent-repl-test-workspace-generation-finalize-zero-exit-leaves-flavor ()
  "Zero exit (success) must NOT clear the flavor — the workspace
will materialize and the track-workspace success path handles the
transition out of `:generating'."
  (agent-repl-test--with-oneshot-tracking-state
    (setq agent-repl--oneshot-last-ws
          (plist-put agent-repl--oneshot-last-ws :doom :generating))
    (agent-repl--workspace-generation-finalize
     "gen-id-1" 0 "finished\n" "" agent-repl--doom-config-dir)
    (should (eq (plist-get agent-repl--oneshot-last-ws :doom) :generating))))

(ert-deftest agent-repl-test-workspace-generation-finalize-nonzero-unrelated-root-noop ()
  "Non-zero exit but GIT-ROOT not a pinned oneshot dir → no flavor
clear (the spawn was for a non-oneshot creation; no flavor state to
touch).  Confirms the failure-clear is gated on
`--oneshot-flavor-for-git-root' returning non-nil."
  (agent-repl-test--with-oneshot-tracking-state
    (setq agent-repl--oneshot-last-ws
          (plist-put agent-repl--oneshot-last-ws :doom :generating))
    (cl-letf (((symbol-function 'message) #'ignore))
      (agent-repl--workspace-generation-finalize
       "gen-id-1" 1 "exited abnormally\n" "" "/tmp/unrelated/"))
    (should (eq (plist-get agent-repl--oneshot-last-ws :doom) :generating))))

(ert-deftest agent-repl-test-workspace-generation-finalize-nil-git-root-noop ()
  "Omitted GIT-ROOT (legacy / test callers) → no flavor clear,
preserves the old single-arg-form behavior for callers that don't
care about oneshot bookkeeping."
  (agent-repl-test--with-oneshot-tracking-state
    (setq agent-repl--oneshot-last-ws
          (plist-put agent-repl--oneshot-last-ws :doom :generating))
    (cl-letf (((symbol-function 'message) #'ignore))
      (agent-repl--workspace-generation-finalize
       "gen-id-1" 1 "exited abnormally\n" "" nil))
    (should (eq (plist-get agent-repl--oneshot-last-ws :doom) :generating))))

;;;; ---- Tests: --oneshot-amend ----

(ert-deftest agent-repl-test-oneshot-amend-errors-on-empty-prompt ()
  "Empty/whitespace prompt is rejected up front."
  (agent-repl-test--with-oneshot-tracking-state
    (should-error (agent-repl--oneshot-amend :doom "   ")
                  :type 'user-error)
    (should-error (agent-repl--oneshot-amend :doom "")
                  :type 'user-error)
    (should-error (agent-repl--oneshot-amend :doom nil)
                  :type 'user-error)))

(ert-deftest agent-repl-test-oneshot-amend-errors-when-no-tracking ()
  "Amend signals when no oneshot has been created for the flavor —
better than silently dropping the prompt."
  (agent-repl-test--with-oneshot-tracking-state
    (should-error (agent-repl--oneshot-amend :doom "hello")
                  :type 'user-error)))

(ert-deftest agent-repl-test-oneshot-amend-queues-while-generating ()
  "While the workspace is still being generated, amend pushes the
prompt onto `agent-repl--oneshot-amended-prompts' (FIFO)."
  (agent-repl-test--with-oneshot-tracking-state
    (agent-repl--oneshot-reset-flavor :doom)
    (agent-repl--oneshot-amend :doom "first")
    (agent-repl--oneshot-amend :doom "second")
    (should (equal (plist-get agent-repl--oneshot-amended-prompts :doom)
                   '("first" "second")))))

(ert-deftest agent-repl-test-oneshot-amend-dispatches-when-ws-exists ()
  "When a real workspace dirname is recorded, amend routes through
`--dispatch-prompt-command' rather than touching the global queue."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-oneshot-tracking-state
      (setq agent-repl--oneshot-last-ws
            (plist-put agent-repl--oneshot-last-ws :doom "doom-ws-1"))
      (let ((dispatched nil))
        (cl-letf (((symbol-function 'agent-repl--dispatch-prompt-command)
                   (lambda (ws prompt) (push (cons ws prompt) dispatched)))
                  ((symbol-function '+workspace-list-names)
                   (lambda () '("doom-ws-1"))))
          (agent-repl--oneshot-amend :doom "go")
          (should (equal dispatched '(("doom-ws-1" . "go"))))
          (should (null (plist-get agent-repl--oneshot-amended-prompts :doom))))))))

(ert-deftest agent-repl-test-oneshot-amend-errors-when-ws-gone ()
  "When the recorded workspace dirname no longer exists (no vterm
buffer AND not in the perspective list), amend surfaces a user-error
rather than persisting a ghost `:pending-prompts' entry."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-oneshot-tracking-state
      (setq agent-repl--oneshot-last-ws
            (plist-put agent-repl--oneshot-last-ws :doom "dead-ws"))
      (cl-letf (((symbol-function '+workspace-list-names)
                 (lambda () '("other"))))
        (should-error (agent-repl--oneshot-amend :doom "go")
                      :type 'user-error)))))

;;;; ---- Tests: --create-pinned-oneshot-workspace resets flavor ----

(ert-deftest agent-repl-test-create-pinned-oneshot-resets-flavor ()
  "Invoking the pinned-oneshot helper resets the corresponding flavor's
last-ws to `:generating' and clears any queued amended prompts —
before the headless spawn begins, so amend invocations enqueue onto
the new oneshot."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-oneshot-tracking-state
      (setq agent-repl--oneshot-amended-prompts
            (plist-put agent-repl--oneshot-amended-prompts :doom '("stale"))
            agent-repl--oneshot-last-ws
            (plist-put agent-repl--oneshot-last-ws :doom "old-ws"))
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (&rest _) "tweak the modeline"))
                ((symbol-function 'agent-repl--spawn-workspace-generation)
                 (lambda (&rest _) nil)))
        (agent-repl-create-doom-oneshot-workspace)
        (should (eq (plist-get agent-repl--oneshot-last-ws :doom) :generating))
        (should (null (plist-get agent-repl--oneshot-amended-prompts :doom)))))))

;;;; ---- Tests: amend commands route to correct flavor ----

(ert-deftest agent-repl-test-amend-doom-oneshot-uses-doom-flavor ()
  "`agent-repl-amend-doom-oneshot-prompt' routes through
`--oneshot-amend' with `:doom'."
  (agent-repl-test--with-oneshot-tracking-state
    (let ((captured-flavor :unset)
          (captured-prompt :unset))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) "amend me"))
                ((symbol-function 'agent-repl--oneshot-amend)
                 (lambda (flavor prompt)
                   (setq captured-flavor flavor
                         captured-prompt prompt))))
        (agent-repl-amend-doom-oneshot-prompt)
        (should (eq captured-flavor :doom))
        (should (equal captured-prompt "amend me"))))))

(ert-deftest agent-repl-test-amend-explanation-engine-oneshot-uses-ee-flavor ()
  "`agent-repl-amend-explanation-engine-oneshot-prompt' routes through
`--oneshot-amend' with `:explanation-engine'."
  (agent-repl-test--with-oneshot-tracking-state
    (let ((captured-flavor :unset))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) "amend me"))
                ((symbol-function 'agent-repl--oneshot-amend)
                 (lambda (flavor _prompt) (setq captured-flavor flavor))))
        (agent-repl-amend-explanation-engine-oneshot-prompt)
        (should (eq captured-flavor :explanation-engine))))))

;;;; ---- Tests: in-flight merge bookkeeping ----

(defmacro agent-repl-test--with-empty-in-flight-merges (&rest body)
  "Run BODY with `agent-repl--in-flight-merges' freshly empty.
Top-level defvar — tests that push MUST scrub it afterwards or later
tests inherit stale state."
  (declare (indent 0))
  `(let ((agent-repl--in-flight-merges nil))
     (unwind-protect (progn ,@body)
       (setq agent-repl--in-flight-merges nil))))

(ert-deftest agent-repl-test-push-in-flight-merge-adds-entry ()
  "Pushing a fresh entry appends a plist with the recorded ws/dir/timestamp."
  (agent-repl-test--with-empty-in-flight-merges
    (cl-letf (((symbol-function 'agent-repl--persist-merge-queue) #'ignore))
      (agent-repl--push-in-flight-merge "ws-a" "/tmp/a")
      (should (= 1 (length agent-repl--in-flight-merges)))
      (let ((entry (car agent-repl--in-flight-merges)))
        (should (equal (plist-get entry :source-ws) "ws-a"))
        (should (equal (plist-get entry :target-dir) "/tmp/a"))
        (should (numberp (plist-get entry :started-at)))))))

(ert-deftest agent-repl-test-push-in-flight-merge-replaces-prior-entry-for-same-ws ()
  "A second push for the same source-ws replaces the prior entry — the
bookkeeping must never stack on retries."
  (agent-repl-test--with-empty-in-flight-merges
    (cl-letf (((symbol-function 'agent-repl--persist-merge-queue) #'ignore))
      (agent-repl--push-in-flight-merge "ws-a" "/tmp/a-old")
      (agent-repl--push-in-flight-merge "ws-a" "/tmp/a-new")
      (should (= 1 (length agent-repl--in-flight-merges)))
      (should (equal (plist-get (car agent-repl--in-flight-merges) :target-dir)
                     "/tmp/a-new")))))

(ert-deftest agent-repl-test-push-in-flight-merge-noop-on-nil-args ()
  "Defensive: nil source-ws or nil target-dir → no entry added."
  (agent-repl-test--with-empty-in-flight-merges
    (cl-letf (((symbol-function 'agent-repl--persist-merge-queue) #'ignore))
      (agent-repl--push-in-flight-merge nil "/tmp/a")
      (agent-repl--push-in-flight-merge "ws-a" nil)
      (should (= 0 (length agent-repl--in-flight-merges))))))

(ert-deftest agent-repl-test-push-in-flight-merge-persists ()
  "Push must call `--persist-merge-queue' so the entry reaches disk
before the cherry-pick has a chance to be interrupted."
  (agent-repl-test--with-empty-in-flight-merges
    (let ((persist-called 0))
      (cl-letf (((symbol-function 'agent-repl--persist-merge-queue)
                 (lambda () (cl-incf persist-called))))
        (agent-repl--push-in-flight-merge "ws-a" "/tmp/a")
        (should (= 1 persist-called))))))

(ert-deftest agent-repl-test-clear-in-flight-merge-removes-entry ()
  "Clearing an existing entry removes it from the live list."
  (agent-repl-test--with-empty-in-flight-merges
    (cl-letf (((symbol-function 'agent-repl--persist-merge-queue) #'ignore))
      (setq agent-repl--in-flight-merges
            (list (list :source-ws "ws-a" :target-dir "/tmp/a" :started-at 1)
                  (list :source-ws "ws-b" :target-dir "/tmp/b" :started-at 2)))
      (agent-repl--clear-in-flight-merge "ws-a")
      (should (equal (mapcar (lambda (e) (plist-get e :source-ws))
                             agent-repl--in-flight-merges)
                     '("ws-b"))))))

(ert-deftest agent-repl-test-clear-in-flight-merge-noop-when-absent ()
  "Clearing a workspace that has no entry leaves the list untouched."
  (agent-repl-test--with-empty-in-flight-merges
    (cl-letf (((symbol-function 'agent-repl--persist-merge-queue) #'ignore))
      (setq agent-repl--in-flight-merges
            (list (list :source-ws "ws-b" :target-dir "/tmp/b" :started-at 2)))
      (agent-repl--clear-in-flight-merge "ws-a")
      (should (equal (mapcar (lambda (e) (plist-get e :source-ws))
                             agent-repl--in-flight-merges)
                     '("ws-b"))))))

(ert-deftest agent-repl-test-clear-in-flight-merge-persists ()
  "Clear always persists — even on no-op — so the on-disk state mirrors
the in-memory state regardless of the path taken."
  (agent-repl-test--with-empty-in-flight-merges
    (let ((persist-called 0))
      (cl-letf (((symbol-function 'agent-repl--persist-merge-queue)
                 (lambda () (cl-incf persist-called))))
        (agent-repl--clear-in-flight-merge "ws-a") ; no-op clear
        (should (= 1 persist-called))))))

;;;; ---- Tests: main-thread heartbeat ----

(defmacro agent-repl-test--with-clean-heartbeat (&rest body)
  "Run BODY with the heartbeat timer fresh — uninstall any preexisting
timer, run BODY, then uninstall again on the way out.  Test isolation
matters because the heartbeat is a global timer."
  (declare (indent 0))
  `(let ((agent-repl--debug-heartbeat-timer nil))
     (unwind-protect (progn ,@body)
       (agent-repl--debug-heartbeat-uninstall))))

(ert-deftest agent-repl-test-debug-heartbeat-install-schedules-timer ()
  "Install schedules a `run-with-timer' at the configured interval."
  (agent-repl-test--with-clean-heartbeat
    (let ((scheduled nil))
      (cl-letf (((symbol-function 'run-with-timer)
                 (lambda (delay repeat fn)
                   (setq scheduled (list :delay delay :repeat repeat :fn fn))
                   :timer-handle))
                (agent-repl-debug-heartbeat-interval 5))
        (agent-repl--debug-heartbeat-install)
        (should (equal 5 (plist-get scheduled :delay)))
        (should (equal 5 (plist-get scheduled :repeat)))
        (should (eq #'agent-repl--debug-heartbeat-tick
                    (plist-get scheduled :fn)))
        (should (eq :timer-handle agent-repl--debug-heartbeat-timer))))))

(ert-deftest agent-repl-test-debug-heartbeat-install-is-idempotent ()
  "A second install while a timer is already active is a no-op — no
double-schedule."
  (agent-repl-test--with-clean-heartbeat
    (let ((call-count 0))
      (cl-letf (((symbol-function 'run-with-timer)
                 (lambda (&rest _args) (cl-incf call-count) :timer))
                (agent-repl-debug-heartbeat-interval 5))
        (agent-repl--debug-heartbeat-install)
        (agent-repl--debug-heartbeat-install)
        (should (= 1 call-count))))))

(ert-deftest agent-repl-test-debug-heartbeat-install-nil-interval-disables ()
  "When `agent-repl-debug-heartbeat-interval' is nil, install is a no-op
\(no timer scheduled).  Lets the user disable heartbeats globally."
  (agent-repl-test--with-clean-heartbeat
    (let ((scheduled nil))
      (cl-letf (((symbol-function 'run-with-timer)
                 (lambda (&rest _args) (setq scheduled t) :timer))
                (agent-repl-debug-heartbeat-interval nil))
        (agent-repl--debug-heartbeat-install)
        (should-not scheduled)
        (should (null agent-repl--debug-heartbeat-timer))))))

(ert-deftest agent-repl-test-debug-heartbeat-uninstall-cancels-timer ()
  "Uninstall calls `cancel-timer' on the active timer and clears the slot."
  (agent-repl-test--with-clean-heartbeat
    (let ((cancelled nil))
      (cl-letf (((symbol-function 'timerp) (lambda (_) t))
                ((symbol-function 'cancel-timer)
                 (lambda (timer) (setq cancelled timer))))
        (setq agent-repl--debug-heartbeat-timer :a-timer)
        (agent-repl--debug-heartbeat-uninstall)
        (should (eq :a-timer cancelled))
        (should (null agent-repl--debug-heartbeat-timer))))))

(ert-deftest agent-repl-test-debug-heartbeat-uninstall-noop-when-nil ()
  "Uninstall is safe when no timer is active — no `cancel-timer' call."
  (agent-repl-test--with-clean-heartbeat
    (let ((cancelled nil))
      (cl-letf (((symbol-function 'cancel-timer)
                 (lambda (_) (setq cancelled t))))
        (setq agent-repl--debug-heartbeat-timer nil)
        (agent-repl--debug-heartbeat-uninstall)
        (should-not cancelled)))))

;;;; ---- Tests: git-branch-of-dir ----

(ert-deftest agent-repl-test-git-branch-of-dir-returns-branch ()
  "Returns the abbreviated branch name for a valid dir."
  (cl-letf (((symbol-function 'file-directory-p) (lambda (_p) t))
            ((symbol-function 'agent-repl--git-string)
             (lambda (&rest _args) "DWC/parent-branch")))
    (should (equal (agent-repl--git-branch-of-dir "/tmp/x")
                   "DWC/parent-branch"))))

(ert-deftest agent-repl-test-git-branch-of-dir-nil-when-missing-dir ()
  "Returns nil when DIR does not exist."
  (cl-letf (((symbol-function 'file-directory-p) (lambda (_p) nil)))
    (should (null (agent-repl--git-branch-of-dir "/nope")))))

(ert-deftest agent-repl-test-git-branch-of-dir-nil-when-detached ()
  "Returns nil for a detached HEAD (git reports literal \"HEAD\")."
  (cl-letf (((symbol-function 'file-directory-p) (lambda (_p) t))
            ((symbol-function 'agent-repl--git-string)
             (lambda (&rest _args) "HEAD")))
    (should (null (agent-repl--git-branch-of-dir "/tmp/x")))))

(ert-deftest agent-repl-test-git-branch-of-dir-nil-when-fatal ()
  "Returns nil when git emits a fatal error string."
  (cl-letf (((symbol-function 'file-directory-p) (lambda (_p) t))
            ((symbol-function 'agent-repl--git-string)
             (lambda (&rest _args) "fatal: not a git repository")))
    (should (null (agent-repl--git-branch-of-dir "/tmp/x")))))

(ert-deftest agent-repl-test-git-branch-of-dir-nil-when-empty ()
  "Returns nil when git emits an empty string."
  (cl-letf (((symbol-function 'file-directory-p) (lambda (_p) t))
            ((symbol-function 'agent-repl--git-string)
             (lambda (&rest _args) "")))
    (should (null (agent-repl--git-branch-of-dir "/tmp/x")))))

;;;; ---- Tests: cherry-pick progress filter ----
;;
;; The filter is a pure string -> progress-plist fold, so these need no git.

(defun agent-repl-test--feed (ws &rest chunks)
  "Feed CHUNKS through a fresh cherry-pick filter for WS; return its progress."
  (let ((filter (agent-repl--make-cherry-pick-filter ws)))
    (dolist (chunk chunks)
      (funcall filter nil chunk)))
  (agent-repl--merge-progress-get ws))

(ert-deftest agent-repl-test-cherry-pick-filter-applied-advances-index ()
  "An applied-commit line advances `:commit-index'."
  (agent-repl-test--with-merge-state
    (agent-repl--merge-progress-begin "ws" '(("aaa1111" . "one") ("bbb2222" . "two")))
    (let ((progress (agent-repl-test--feed "ws" "[master aaa1111] one\n")))
      (should (= 1 (plist-get progress :commit-index))))))

(ert-deftest agent-repl-test-cherry-pick-filter-applied-resets-clock ()
  "An applied-commit line restarts the per-commit clock."
  (agent-repl-test--with-merge-state
    (agent-repl--merge-progress-begin "ws" '(("aaa1111" . "one")))
    (agent-repl--merge-progress-put "ws" :commit-started-at 0.0)
    (let ((progress (agent-repl-test--feed "ws" "[master aaa1111] one\n")))
      (should (> (plist-get progress :commit-started-at) 0.0)))))

(ert-deftest agent-repl-test-cherry-pick-filter-buffers-split-line ()
  "A boundary line split across two chunks is buffered, not dropped.
A process filter receives arbitrary chunks, so matching per-chunk would
silently lose commits and desync the index."
  (agent-repl-test--with-merge-state
    (agent-repl--merge-progress-begin "ws" '(("aaa1111" . "one")))
    (let ((progress (agent-repl-test--feed "ws" "[master aaa11" "11] one\n")))
      (should (= 1 (plist-get progress :commit-index))))))

(ert-deftest agent-repl-test-cherry-pick-filter-partial-line-not-counted-early ()
  "A line with no newline yet is held back rather than counted."
  (agent-repl-test--with-merge-state
    (agent-repl--merge-progress-begin "ws" '(("aaa1111" . "one")))
    (let ((progress (agent-repl-test--feed "ws" "[master aaa1111] one")))
      (should (= 0 (plist-get progress :commit-index))))))

(ert-deftest agent-repl-test-cherry-pick-filter-two-commits-one-chunk ()
  "Two boundary lines arriving in one chunk both advance the index."
  (agent-repl-test--with-merge-state
    (agent-repl--merge-progress-begin "ws" '(("a" . "1") ("b" . "2") ("c" . "3")))
    (let ((progress (agent-repl-test--feed
                     "ws" "[master aaa1111] one\n[master bbb2222] two\n")))
      (should (= 2 (plist-get progress :commit-index))))))

(ert-deftest agent-repl-test-cherry-pick-filter-conflict-commit ()
  "A `could not apply' line records the conflicting SHA and subject."
  (agent-repl-test--with-merge-state
    (agent-repl--merge-progress-begin "ws" '(("dec4a97" . "feat: one")))
    (let ((progress (agent-repl-test--feed
                     "ws" "error: could not apply dec4a97... feat: one\n")))
      (should (equal "dec4a97" (plist-get progress :conflict-sha)))
      (should (equal "feat: one" (plist-get progress :conflict-subject))))))

(ert-deftest agent-repl-test-cherry-pick-filter-conflict-file ()
  "A CONFLICT line appends the conflicted path."
  (agent-repl-test--with-merge-state
    (agent-repl--merge-progress-begin "ws" '(("a" . "1")))
    (let ((progress (agent-repl-test--feed
                     "ws" "CONFLICT (content): Merge conflict in src/f.txt\n")))
      (should (equal '("src/f.txt") (plist-get progress :conflict-files))))))

(ert-deftest agent-repl-test-cherry-pick-filter-conflict-file-deduped ()
  "The same conflicted path reported twice is recorded once."
  (agent-repl-test--with-merge-state
    (agent-repl--merge-progress-begin "ws" '(("a" . "1")))
    (let ((progress (agent-repl-test--feed
                     "ws"
                     "CONFLICT (content): Merge conflict in f.txt\n"
                     "CONFLICT (content): Merge conflict in f.txt\n")))
      (should (equal '("f.txt") (plist-get progress :conflict-files))))))

(ert-deftest agent-repl-test-cherry-pick-filter-ignores-chatter ()
  "Unrecognized git chatter leaves the progress record untouched."
  (agent-repl-test--with-merge-state
    (agent-repl--merge-progress-begin "ws" '(("a" . "1")))
    (let ((progress (agent-repl-test--feed
                     "ws" "Auto-merging f.txt\nhint: after resolving\n")))
      (should (= 0 (plist-get progress :commit-index)))
      (should (null (plist-get progress :conflict-sha))))))

;;;; ---- Tests: merge progress record ----

(ert-deftest agent-repl-test-merge-progress-begin-starts-at-zero ()
  "Progress starts at index 0: git is applying the FIRST commit immediately."
  (agent-repl-test--with-merge-state
    (agent-repl--merge-progress-begin "ws" '(("a" . "1") ("b" . "2")))
    (should (= 0 (plist-get (agent-repl--merge-progress-get "ws") :commit-index)))))

(ert-deftest agent-repl-test-merge-progress-clear-removes-entry ()
  "Clearing progress drops the entry entirely."
  (agent-repl-test--with-merge-state
    (agent-repl--merge-progress-begin "ws" '(("a" . "1")))
    (agent-repl--merge-progress-clear "ws")
    (should (null (agent-repl--merge-progress-get "ws")))))

(ert-deftest agent-repl-test-merge-progress-put-bumps-seq ()
  "Every progress write bumps the render counter the drawer's signature reads."
  (agent-repl-test--with-merge-state
    (let ((before agent-repl--merge-progress-seq))
      (agent-repl--merge-progress-put "ws" :commit-index 1)
      (should (> agent-repl--merge-progress-seq before)))))

;;;; ---- Tests: range-commits ----

(ert-deftest agent-repl-test-range-commits-parses-log ()
  "Tab-separated log output parses to oldest-first (SHA . SUBJECT) pairs."
  (cl-letf (((symbol-function 'agent-repl--git-string-quiet)
             (lambda (&rest _args) "aaa1111\tfeat: one\nbbb2222\tfix: two")))
    (should (equal '(("aaa1111" . "feat: one") ("bbb2222" . "fix: two"))
                   (agent-repl--range-commits "/tmp/x" "master" "HEAD")))))

(ert-deftest agent-repl-test-range-commits-empty-range ()
  "An empty range yields nil rather than a bogus entry."
  (cl-letf (((symbol-function 'agent-repl--git-string-quiet)
             (lambda (&rest _args) "")))
    (should (null (agent-repl--range-commits "/tmp/x" "master" "HEAD")))))

(ert-deftest agent-repl-test-range-commits-subject-with-tabs-preserved ()
  "Only the first tab separates SHA from subject; the subject keeps the rest."
  (cl-letf (((symbol-function 'agent-repl--git-string-quiet)
             (lambda (&rest _args) "aaa1111\tfeat: a\tb")))
    (should (equal '(("aaa1111" . "feat: a\tb"))
                   (agent-repl--range-commits "/tmp/x" "master" "HEAD")))))

;;; test-worktree.el ends here
