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
;; `agent-repl--git-branch-exists-p')
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

;;;; ---- Tests: main-worktree-p ----
;;
;; `agent-repl--main-worktree-p' is a pure filesystem check: the main
;; worktree's `.git' is a directory; a linked worktree's `.git' is a
;; `gitdir:' pointer FILE.

(ert-deftest agent-repl-test-main-worktree-p-dot-git-directory ()
  "A dir whose `.git' is a directory is the main worktree."
  (let ((repo (make-temp-file "agent-repl-test-mwp-" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" repo) t)
          (should (agent-repl--main-worktree-p repo)))
      (delete-directory repo t))))

(ert-deftest agent-repl-test-main-worktree-p-dot-git-file ()
  "A dir whose `.git' is a plain pointer file is a LINKED worktree, not main."
  (let ((wt (make-temp-file "agent-repl-test-mwp-wt-" t)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name ".git" wt)
            (insert "gitdir: /somewhere/.git/worktrees/wt\n"))
          (should-not (agent-repl--main-worktree-p wt)))
      (delete-directory wt t))))

(ert-deftest agent-repl-test-main-worktree-p-no-dot-git ()
  "A dir with no `.git' entry at all is not the main worktree."
  (let ((dir (make-temp-file "agent-repl-test-mwp-none-" t)))
    (unwind-protect
        (should-not (agent-repl--main-worktree-p dir))
      (delete-directory dir t))))

(ert-deftest agent-repl-test-main-worktree-p-nil-dir ()
  "A nil directory is not the main worktree (and does not error)."
  (should-not (agent-repl--main-worktree-p nil)))

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

(ert-deftest agent-repl-test-dispatch-prompt-enqueues-when-no-buffer ()
  "When the workspace has no state at all (never registered), prompt is
enqueued on :pending-prompts."
  (agent-repl-test--with-clean-state
    (agent-repl--dispatch-prompt-command "ws1" "hello")
    (should (equal (agent-repl--ws-get "ws1" :pending-prompts) '("hello")))))

(ert-deftest agent-repl-test-dispatch-prompt-enqueues-when-not-ready ()
  "When the workspace has no live running agent session, the prompt is enqueued."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :frontend 'gui)
    (agent-repl--dispatch-prompt-command "ws1" "hello")
    (should (equal (agent-repl--ws-get "ws1" :pending-prompts) '("hello")))))

(ert-deftest agent-repl-test-dispatch-prompt-sends-immediately-when-running ()
  "When the workspace has a live running agent session, the prompt is sent
immediately via `agent-repl--send' rather than enqueued.

Regression: the old body gated on the vterm-only buffer-local
`agent-repl--ready', which is always nil for a gui workspace (no
`:vterm-buffer' is ever set), so it ALWAYS enqueued -- a gui workspace's
prompt could never be delivered immediately no matter how long its
session had been running.  This test's workspace has a genuinely live
session (`:frontend-session-id' bound) and fails against that old body."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :frontend 'gui)
    (agent-repl--ws-put "ws1" :frontend-session-id "sid-1")
    (let (sent-prompt sent-ws)
      (cl-letf (((symbol-function 'agent-repl--send)
                 (lambda (prompt ws) (setq sent-prompt prompt sent-ws ws))))
        (agent-repl--dispatch-prompt-command "ws1" "hello")
        (should (equal sent-prompt "hello"))
        (should (equal sent-ws "ws1"))
        (should-not (agent-repl--ws-get "ws1" :pending-prompts))))))

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
so the merged workspace stays registered until explicit finish."
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
chance to release sockets before its session dies."
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

(ert-deftest agent-repl-test-handle-close-command-normalizes-full-branch-for-gns ()
  "A full-branch `workspace' (e.g. \"DWC/feature-one\") is normalized to
its bare name before `--gns-sockets-close-then' so the gating targets
the persp/registry key rather than the never-registered branch name."
  (let ((gating-ws :unset))
    (cl-letf (((symbol-function 'agent-repl--gns-sockets-close-then)
               (lambda (ws _teardown-fn) (setq gating-ws ws))))
      (agent-repl--handle-close-command
       '((type . "close") (workspace . "DWC/feature-one")))
      (should (equal gating-ws "feature-one")))))

(ert-deftest agent-repl-test-handle-close-command-normalizes-full-branch-for-teardown ()
  "A full-branch `workspace' is normalized to its bare name before the
teardown thunk calls `--close-workspace', so the tab and session close."
  (let ((received :unset)
        (teardown-fn nil))
    (cl-letf (((symbol-function 'agent-repl--gns-sockets-close-then)
               (lambda (_ws fn) (setq teardown-fn fn)))
              ((symbol-function 'agent-repl--close-workspace)
               (lambda (ws &optional _preserve) (setq received ws))))
      (agent-repl--handle-close-command
       '((type . "close") (workspace . "DWC/feature-one")))
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

(ert-deftest agent-repl-test-gns-sockets-close-then-unregistered-ws-runs-teardown-directly ()
  "For a bare, unregistered workspace, `--gns-sockets-close-then' must run
the teardown thunk immediately — there is no agent to drain.

Runs through the REAL (unstubbed) `agent-repl--agent-running-p', which is
exactly the regression this function pins: the old body gated on the
vterm-only buffer-local `agent-repl--ready' and so happened to reach the
same answer here, but for the wrong reason -- see the `-gui-running'
tests below for the fixture where the old and new bodies actually
disagree."
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

(ert-deftest agent-repl-test-gns-sockets-close-then-gui-not-running-runs-teardown-directly ()
  "A gui workspace with no live daemon session binding must still fall
through to immediate teardown — the prompt would otherwise queue on
`:pending-prompts' and never drain before close."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws" :frontend 'gui)
    (let ((called nil)
          (sent nil))
      (cl-letf (((symbol-function 'agent-repl--send)
                 (lambda (&rest _) (setq sent t))))
        (agent-repl--gns-sockets-close-then
         "ws" (lambda () (setq called t)))
        (should called)
        (should-not sent)))))

(ert-deftest agent-repl-test-gns-sockets-close-then-gui-running-sends-prompt ()
  "With a live, running gui session, `--gns-sockets-close-then' must
dispatch `agent-repl-gns-sockets-close-prompt' via `--send' and defer
teardown.

Regression: the old body gated on the vterm-only buffer-local
`agent-repl--ready', which is always nil for a gui workspace (no
`:vterm-buffer' is ever set), so it ALWAYS took the immediate-teardown
branch and silently skipped the GNS socket drain for every gui
workspace close.  This test's workspace has a genuinely live session
\(`:frontend-session-id' bound) and fails against that old body."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws" :frontend 'gui)
    (agent-repl--ws-put "ws" :frontend-session-id "sid-1")
    (let ((sent-prompt :unset)
          (sent-ws :unset)
          (teardown-called nil))
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
        (should-not teardown-called)))))

(ert-deftest agent-repl-test-gns-sockets-close-then-on-settle-schedules-poll ()
  "The `on-settle' callback handed to `--send' must schedule the first
`--gns-sockets-close-poll' via `run-at-time' so the prompt_submit hook
has time to fire before state is polled."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws" :frontend 'gui)
    (agent-repl--ws-put "ws" :frontend-session-id "sid-1")
    (let ((scheduled-fn :unset)
          (scheduled-delay :unset)
          (captured-on-settle nil))
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
        (should (eq scheduled-fn #'agent-repl--gns-sockets-close-poll))))))

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

(ert-deftest agent-repl-test-kill-process-safely-main-thread-deletes-directly ()
  "On the main thread the deletion happens inline (no deferral needed)."
  ;; Arrange
  (let ((deleted nil) (deferred nil))
    (cl-letf (((symbol-function 'process-live-p) (lambda (_p) t))
              ((symbol-function 'delete-process) (lambda (p) (setq deleted p)))
              ((symbol-function 'agent-repl--defer-to-main-thread)
               (lambda (_thunk) (setq deferred t))))
      ;; Act — ert runs on the main thread.
      (agent-repl--kill-process-safely 'proc)
      ;; Assert
      (should (eq deleted 'proc))
      (should-not deferred))))

(ert-deftest agent-repl-test-kill-process-safely-worker-thread-defers ()
  "Off the main thread the deletion is DEFERRED, never run inline.
`delete-process' can redisplay, and redisplay off-main aborts Emacs on
macOS — the deadlock of 2026-07-12."
  ;; Arrange
  (let ((deleted nil) (thunk nil))
    (cl-letf (((symbol-function 'current-thread) (lambda () 'worker-thread))
              ((symbol-function 'process-live-p) (lambda (_p) t))
              ((symbol-function 'delete-process) (lambda (p) (setq deleted p)))
              ((symbol-function 'agent-repl--defer-to-main-thread)
               (lambda (fn) (setq thunk fn))))
      ;; Act
      (agent-repl--kill-process-safely 'proc)
      ;; Assert — nothing deleted inline; the work is queued for main.
      (should-not deleted)
      (should (functionp thunk))
      ;; And the queued thunk performs the deletion when main runs it.
      (funcall thunk)
      (should (eq deleted 'proc)))))

(ert-deftest agent-repl-test-kill-process-safely-dead-process-is-no-op ()
  "A dead (or nil) process is neither deleted nor deferred."
  ;; Arrange
  (let ((deferred nil))
    (cl-letf (((symbol-function 'process-live-p) (lambda (_p) nil))
              ((symbol-function 'agent-repl--defer-to-main-thread)
               (lambda (_thunk) (setq deferred t))))
      ;; Act / Assert
      (should-not (agent-repl--kill-process-safely nil))
      (should-not deferred))))

(ert-deftest agent-repl-test-kill-buffer-safely-main-thread-kills-directly ()
  "On the main thread the buffer is killed inline."
  ;; Arrange
  (let ((buf (generate-new-buffer " *safe-kill-main*"))
        (deferred nil))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-repl--defer-to-main-thread)
                   (lambda (_thunk) (setq deferred t))))
          ;; Act
          (agent-repl--kill-buffer-safely buf)
          ;; Assert
          (should-not (buffer-live-p buf))
          (should-not deferred))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest agent-repl-test-kill-buffer-safely-worker-thread-defers ()
  "Off the main thread the kill is DEFERRED.
`kill-buffer' implicitly deletes a live process the buffer owns, so it
carries the same redisplay-off-main abort hazard."
  ;; Arrange
  (let ((buf (generate-new-buffer " *safe-kill-worker*"))
        (thunk nil))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'current-thread) (lambda () 'worker-thread))
                    ((symbol-function 'agent-repl--defer-to-main-thread)
                     (lambda (fn) (setq thunk fn))))
            ;; Act
            (agent-repl--kill-buffer-safely buf))
          ;; Assert — still alive; the kill is queued for the main thread.
          (should (buffer-live-p buf))
          (should (functionp thunk))
          (funcall thunk)
          (should-not (buffer-live-p buf)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest agent-repl-test-kill-buffer-safely-dead-buffer-is-no-op ()
  "A dead (or nil) buffer is neither killed nor deferred."
  ;; Arrange
  (let ((deferred nil))
    (cl-letf (((symbol-function 'agent-repl--defer-to-main-thread)
               (lambda (_thunk) (setq deferred t))))
      ;; Act / Assert
      (should-not (agent-repl--kill-buffer-safely nil))
      (should-not deferred))))

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
  (let ((proc 'fake-exited-process))
    (cl-letf (((symbol-function 'set-process-sentinel) (lambda (&rest _) nil))
              ((symbol-function 'process-name) (lambda (_p) "fake-exited"))
              ((symbol-function 'process-status) (lambda (_p) 'exit))
              ((symbol-function 'process-exit-status) (lambda (_p) 7))
              ((symbol-function 'condition-wait)
               (lambda (&rest _) (error "condition-wait must not be reached"))))
      (should (= 7 (agent-repl--wait-for-process-exit--worker proc 5 nil nil))))))

(ert-deftest agent-repl-test-wait-for-process-exit-worker-already-exited-skips-timeout-timer ()
  "When the post-install status sample finds the process already dead,
no timeout timer is scheduled — there is nothing left to deadline."
  (let ((proc 'fake-exited-process)
        (timer-created nil))
    (cl-letf (((symbol-function 'set-process-sentinel) (lambda (&rest _) nil))
              ((symbol-function 'process-name) (lambda (_p) "fake-exited"))
              ((symbol-function 'process-status) (lambda (_p) 'exit))
              ((symbol-function 'process-exit-status) (lambda (_p) 0))
              ((symbol-function 'run-at-time)
               (lambda (&rest _) (setq timer-created t) nil))
              ((symbol-function 'condition-wait)
               (lambda (&rest _) nil)))
      (agent-repl--wait-for-process-exit--worker proc 5 nil nil))
    (should-not timer-created)))

;;;; ---- Tests: finish-workspace ----

(ert-deftest agent-repl-test-finish-workspace-non-worktree ()
  "Finishing a non-worktree workspace tombstones state and kills persp.
Post-tombstone-refactor, finish-workspace no longer removes the hash
entry — it stamps `:nuked-at' via `--ws-del'.  This test pins both the
persp-kill and the resulting tombstone marker."
  (agent-repl-test--with-clean-state
    (let ((persp-killed nil))
      (agent-repl--ws-put "ws1" :project-dir "/tmp/fake")
      (cl-letf (((symbol-function 'agent-repl--gui-kill) #'ignore)
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
            (cl-letf (((symbol-function 'agent-repl--gui-kill) #'ignore)
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
    (cl-letf (((symbol-function 'agent-repl--gui-kill) #'ignore)
              ((symbol-function '+workspace-list-names) (lambda () '("foo")))
              ((symbol-function 'persp-kill) (lambda (_ws) nil)))
      (agent-repl--finish-workspace "DWC/foo")
      (should-not (agent-repl--ws-live-p "foo"))
      (should (agent-repl--ws-get "foo" :nuked-at)))))

(ert-deftest agent-repl-test-finish-workspace-kills-through-frontend-registry ()
  "finish-workspace kills WS's agent session through the frontend
registry's `:kill-fn' dispatch, NOT a hardcoded vterm-process kill.

Regression: the old body only ever called `agent-repl--kill-vterm-process',
and only `when' a `:vterm-buffer' was present -- for a gui workspace that
key is never set, so a gui workspace's daemon session (and its webview)
was NEVER killed on finish, orphaning it forever.  This test fails
against that old body: a gui workspace here has no `:vterm-buffer' at
all, yet the kill must still fire, through `agent-repl--gui-kill' (the
registered gui frontend's `:kill-fn')."
  (agent-repl-test--with-clean-state
    (let ((killed-ws nil))
      (agent-repl--ws-put "ws1" :frontend 'gui)
      (agent-repl--ws-put "ws1" :frontend-session-id "sid-1")
      (cl-letf (((symbol-function 'agent-repl--gui-kill)
                 (lambda (ws) (setq killed-ws ws)))
                ((symbol-function '+workspace-list-names) (lambda () nil))
                ((symbol-function 'persp-kill) (lambda (_ws) nil)))
        (agent-repl--finish-workspace "ws1")
        (should (equal killed-ws "ws1"))))))

(ert-deftest agent-repl-test-finish-workspace-no-persp-kill-if-not-listed ()
  "If workspace is not in +workspace-list-names, persp-kill is not called."
  (agent-repl-test--with-clean-state
    (let ((persp-killed nil))
      (agent-repl--ws-put "ws1" :project-dir "/tmp/fake")
      (cl-letf (((symbol-function 'agent-repl--gui-kill) #'ignore)
                ((symbol-function '+workspace-list-names) (lambda () '("other")))
                ((symbol-function 'persp-kill) (lambda (ws) (setq persp-killed ws))))
        (agent-repl--finish-workspace "ws1")
        (should-not persp-killed)))))

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
workspace-generation prompt, and the daemon resolves the session ID when it
claims the generated intent.  This test covers the entry's only remaining
job: surfacing the right fork-from name."
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
                 (lambda (_raw _prefixed _git-root _base fork-from &optional _model)
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
                 (lambda (_raw _prefixed _git-root _base fork-from &optional _model)
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
                 (lambda (_raw _prefixed git-root _base _fork-from &optional _model)
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
                 (lambda (_raw _prefixed git-root _base _fork-from &optional _model)
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
                 (lambda (_raw _prefixed _git-root base _fork-from &optional _model)
                   (setq captured-base base))))
        (agent-repl-fork-worktree-workspace nil)
        (should (equal captured-base "HEAD"))))))

;;;; ---- Tests: git-root threading from interactive entry points ----

;; The new flow eagerly resolves a single git-root at entry-point time and
;; injects it into the workspace-generation JSON.  The downstream
;; `--create-worktree-from-command' uses that same git-root as both
;; git-root and source-dir on the new workspace, so source-dir threading
;; collapses into git-root threading at the entry-point layer.

;; Interactive daemon-create source routing is covered by test-workspace-create-client.el.

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
                 (lambda (_raw _prefixed git-root _base _fork-from &optional _model)
                   (setq captured-git-root git-root))))
        (agent-repl-fork-worktree-workspace "fork-source")
        (should (equal captured-git-root "/tmp/fork-source-repo/"))))))

;;;; ---- Tests: eager-open of a generated workspace's REPL ----

(ert-deftest agent-repl-test-eager-open-panels-drains-target-under-guard ()
  "eager-open-panels switches to WS and runs the three panel drains for it,
with `agent-repl--eager-open-in-progress' bound while they run so the
activation-reactive hooks stay suppressed."
  (agent-repl-test--with-clean-state
    (let ((switched nil) (drains nil) (guard-seen nil))
      (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "caller"))
                ((symbol-function 'agent-repl--restore-focus) (lambda (&rest _) nil))
                ((symbol-function 'agent-repl--ws-switch)
                 (lambda (ws &rest _) (push ws switched)))
                ((symbol-function 'agent-repl--drain-pending-magit)
                 (lambda (ws)
                   (push (cons :magit ws) drains)
                   (push agent-repl--eager-open-in-progress guard-seen)))
                ((symbol-function 'agent-repl--drain-pending-initial-buffers)
                 (lambda (ws) (push (cons :init ws) drains)))
                ((symbol-function 'agent-repl--drain-pending-show-panels)
                 (lambda (ws) (push (cons :show ws) drains))))
        (agent-repl--eager-open-panels "gen-ws")
        (should (equal switched '("gen-ws")))
        (should (equal (reverse drains)
                       '((:magit . "gen-ws") (:init . "gen-ws") (:show . "gen-ws"))))
        (should (equal guard-seen '(t)))))))

(ert-deftest agent-repl-test-eager-open-panels-clears-guard-after-return ()
  "eager-open-panels leaves `agent-repl--eager-open-in-progress' nil once it
returns, so a real later switch is not suppressed."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "caller"))
              ((symbol-function 'agent-repl--restore-focus) (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--ws-switch) #'ignore)
              ((symbol-function 'agent-repl--drain-pending-magit) #'ignore)
              ((symbol-function 'agent-repl--drain-pending-initial-buffers) #'ignore)
              ((symbol-function 'agent-repl--drain-pending-show-panels) #'ignore))
      (agent-repl--eager-open-panels "gen-ws")
      (should-not agent-repl--eager-open-in-progress))))

(ert-deftest agent-repl-test-eager-open-panels-restores-focus-on-drain-error ()
  "eager-open-panels restores the caller's focus even when a drain signals —
the `agent-repl--with-preserved-focus' unwind contract."
  (agent-repl-test--with-clean-state
    (let ((restored nil))
      (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "caller"))
                ((symbol-function 'agent-repl--restore-focus)
                 (lambda (&rest _) (setq restored t)))
                ((symbol-function 'agent-repl--ws-switch) #'ignore)
                ((symbol-function 'agent-repl--drain-pending-magit)
                 (lambda (&rest _) (error "boom")))
                ((symbol-function 'agent-repl--drain-pending-initial-buffers) #'ignore)
                ((symbol-function 'agent-repl--drain-pending-show-panels) #'ignore))
        (should-error (agent-repl--eager-open-panels "gen-ws"))
        (should restored)
        (should-not agent-repl--eager-open-in-progress)))))

;;;; ---- Tests: setup-worktree-session no-agent branch ----

(defmacro agent-repl-test--with-worktree-boot-stubs (bindings &rest body)
  "Run BODY with the worktree boot's collaborators stubbed, plus BINDINGS.

Stubs the env hydration faithfully — the real
`agent-repl--initialize-ws-env' is the sole writer of `:active-env',
and `agent-repl--frontend-boot-session' RESOLVES THE FRONTEND against
that value, so a stub that dropped it would leave every test's
workspace with no resolvable frontend.

BINDINGS are extra `cl-letf' bindings and are spliced in FIRST, ahead
of the defaults: when one `cl-letf' binds the same place twice, the
EARLIER binding is the one in force for the body, so a caller's
override must precede the default it replaces."
  (declare (indent 1))
  `(agent-repl-test--with-clean-state
     (cl-letf (,@bindings
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

(ert-deftest agent-repl-test-async-git-sentinel-exit-success ()
  "Exit with code 0 calls callback with (t output)."
  (let ((captured-ok nil)
        (captured-output nil)
        (proc-buf (generate-new-buffer " *test-sentinel-ok*")))
    (unwind-protect
        (progn
          (with-current-buffer proc-buf
            (insert "  git output here  "))
          (cl-letf (((symbol-function 'process-status) (lambda (_p) 'exit))
                    ((symbol-function 'process-exit-status) (lambda (_p) 0))
                    ((symbol-function 'process-buffer) (lambda (_p) proc-buf))
                    ((symbol-function 'process-name) (lambda (_p) "fake-success"))
                    ((symbol-function 'process-get)
                     (lambda (_p prop)
                       (when (eq prop 'agent-repl-callback)
                         (lambda (ok output)
                           (setq captured-ok ok
                                 captured-output output)))))
                    ((symbol-function 'agent-repl--kill-buffer-safely)
                     (lambda (buf) (kill-buffer buf))))
            (agent-repl--async-git-sentinel 'fake-process "finished\n")
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
          (cl-letf (((symbol-function 'process-status) (lambda (_p) 'exit))
                    ((symbol-function 'process-exit-status) (lambda (_p) 1))
                    ((symbol-function 'process-buffer) (lambda (_p) proc-buf))
                    ((symbol-function 'process-name) (lambda (_p) "fake-failure"))
                    ((symbol-function 'process-get)
                     (lambda (_p prop)
                       (when (eq prop 'agent-repl-callback)
                         (lambda (ok output)
                           (setq captured-ok ok
                                 captured-output output)))))
                    ((symbol-function 'agent-repl--kill-buffer-safely)
                     (lambda (buf) (kill-buffer buf))))
            (agent-repl--async-git-sentinel 'fake-process "finished\n")
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
          (cl-letf (((symbol-function 'process-status) (lambda (_p) 'signal))
                    ((symbol-function 'process-exit-status) (lambda (_p) 9))
                    ((symbol-function 'process-buffer) (lambda (_p) proc-buf))
                    ((symbol-function 'process-name) (lambda (_p) "fake-signaled"))
                    ((symbol-function 'process-get)
                     (lambda (_p prop)
                       (when (eq prop 'agent-repl-callback)
                         (lambda (ok output)
                           (setq captured-ok ok
                                 captured-output output)))))
                    ((symbol-function 'agent-repl--kill-buffer-safely)
                     (lambda (buf) (kill-buffer buf))))
            (agent-repl--async-git-sentinel 'fake-process "killed\n")
            (should (not (eq captured-ok 'not-set)))
            (should (stringp captured-output))))
      (when (buffer-live-p proc-buf)
        (kill-buffer proc-buf)))))

(ert-deftest agent-repl-test-async-git-sentinel-kills-process-buffer ()
  "Process buffer is killed after callback is invoked."
  (let ((proc-buf (generate-new-buffer " *test-sentinel-bufkill*")))
    (with-current-buffer proc-buf
      (insert "output"))
    (cl-letf (((symbol-function 'process-status) (lambda (_p) 'exit))
              ((symbol-function 'process-exit-status) (lambda (_p) 0))
              ((symbol-function 'process-buffer) (lambda (_p) proc-buf))
              ((symbol-function 'process-name) (lambda (_p) "fake-buffer-kill"))
              ((symbol-function 'process-get)
               (lambda (_p prop)
                 (when (eq prop 'agent-repl-callback)
                   (lambda (_ok _output) nil)))))
      (agent-repl--async-git-sentinel 'fake-process "finished\n")
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

(ert-deftest agent-repl-test-workspace-generation-prompt-lists-prompt-as-required-field ()
  "The MUST-emit field block names `prompt' alongside the deterministic
fields.  Prose-only instruction proved insufficient: a generation run
omitted the `prompt' field entirely, materializing a workspace that
booted idle with no first message."
  (let* ((out (agent-repl--workspace-generation-prompt
               "raw" "prefixed" "/tmp/repo/" "HEAD" nil))
         (must-block (progn
                       (should (string-match "Deterministic fields you MUST emit[^:]*:\n\\(\\(?:  .*\n\\)+\\)" out))
                       (match-string 1 out))))
    (should (string-match-p "\"prompt\"" must-block))))

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

(ert-deftest agent-repl-test-workspace-generation-prompt-omits-model-when-nil ()
  "When MODEL is nil, no `model' line is emitted — the workspace falls
back to `agent-repl-interactive-model'."
  (let ((out (agent-repl--workspace-generation-prompt
              "raw" "prefixed" "/tmp/repo/" "HEAD" nil nil)))
    (should-not (string-match-p "\"model\"" out))))

(ert-deftest agent-repl-test-workspace-generation-prompt-emits-model-when-set ()
  "When MODEL is set, the prompt instructs the model to emit a matching
`model' field so the spawned workspace boots under `--model MODEL'."
  (let ((out (agent-repl--workspace-generation-prompt
              "raw" "prefixed" "/tmp/repo/" "HEAD" nil "sonnet")))
    (should (string-match-p "\"model\": \"sonnet\"" out))))

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
suffix like `invoke /create-or-update-workspace merge' inside the inner prompt and run it
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

(ert-deftest agent-repl-test-spawn-workspace-generation-threads-model-to-prompt ()
  "The MODEL arg is forwarded to `agent-repl--workspace-generation-prompt'
so the emitted JSON carries a `model' field for the spawned workspace."
  (let ((captured-model :unset))
    (cl-letf (((symbol-function 'agent-repl--workspace-generation-prompt)
               (lambda (_raw _prefixed _git-root _base _fork-from &optional model)
                 (setq captured-model model)
                 "PROMPT"))
              ((symbol-function 'make-process) (lambda (&rest _) (make-marker)))
              ((symbol-function 'process-send-string) (lambda (&rest _) nil))
              ((symbol-function 'process-send-eof) (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--log) (lambda (&rest _) nil)))
      (agent-repl--spawn-workspace-generation
       "raw" "prefixed" "/tmp/repo/" "HEAD" nil "sonnet")
      (should (equal captured-model "sonnet")))))

(ert-deftest agent-repl-test-spawn-workspace-generation-passes-nil-model-when-absent ()
  "When MODEL is omitted, `agent-repl--workspace-generation-prompt' receives
nil so no `model' field is emitted."
  (let ((captured-model :unset))
    (cl-letf (((symbol-function 'agent-repl--workspace-generation-prompt)
               (lambda (_raw _prefixed _git-root _base _fork-from &optional model)
                 (setq captured-model model)
                 "PROMPT"))
              ((symbol-function 'make-process) (lambda (&rest _) (make-marker)))
              ((symbol-function 'process-send-string) (lambda (&rest _) nil))
              ((symbol-function 'process-send-eof) (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--log) (lambda (&rest _) nil)))
      (agent-repl--spawn-workspace-generation
       "raw" "prefixed" "/tmp/repo/" "HEAD" nil)
      (should (null captured-model)))))

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
                 "Initial prompt from current worktree (optional): ")))

(ert-deftest agent-repl-test-worktree-preemptive-prompt-master ()
  "BASE = `master' (`SPC TAB N') prompt names the main worktree."
  (should (equal (agent-repl--worktree-preemptive-prompt 'master)
                 "Initial prompt from main worktree (optional): ")))

(ert-deftest agent-repl-test-worktree-preemptive-prompt-differ ()
  "The `SPC TAB n' and `SPC TAB N' prompts are visibly distinct."
  (should-not (equal (agent-repl--worktree-preemptive-prompt 'head)
                     (agent-repl--worktree-preemptive-prompt 'master))))

(ert-deftest agent-repl-test-worktree-preemptive-prompt-unknown-errors ()
  "An unknown base signals an error rather than a mislabeled prompt."
  (should-error (agent-repl--worktree-preemptive-prompt 'bogus)))

;; Retired local create/name-generation tests moved to test-workspace-create-client.el.

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
                 (lambda (_raw prefixed _git-root _base _fork-from &optional _model)
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

(ert-deftest agent-repl-test-ws-name-for-dir-skips-tombstoned-match ()
  "Reverse lookup remains live-only after moving iteration behind workspace.el."
  (agent-repl-test--with-clean-state
    (puthash "tomb" '(:project-dir "/shared/" :nuked-at (1 2 3))
             agent-repl--workspaces)
    (puthash "live" '(:project-dir "/shared/") agent-repl--workspaces)
    (cl-letf (((symbol-function 'agent-repl--path-canonical) #'identity))
      (should (equal (agent-repl--ws-name-for-dir "/shared/") "live")))))

;;;; ---- Tests: --place-generated-workspace (child tab placement) ----

(defmacro agent-repl-test--capturing-workspace-placement (&rest body)
  "Run BODY with the two tab-placement functions stubbed to record calls.
Binds `calls' to a list that accumulates (next-to WS ANCHOR) or
(priority WS) entries in call order, and stubs `agent-repl--path-canonical'
with `identity' so `agent-repl--ws-name-for-dir' resolves literal dirs."
  (declare (indent 0) (debug t))
  `(let ((calls nil))
     (cl-letf (((symbol-function 'agent-repl--path-canonical) #'identity)
               ((symbol-function 'agent-repl--reorder-workspace-next-to)
                (lambda (ws anchor) (push (list 'next-to ws anchor) calls)))
               ((symbol-function 'agent-repl--reorder-workspace-by-priority)
                (lambda (ws) (push (list 'priority ws) calls))))
       ,@body)))

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
  "Returns nil on cache miss — callers should treat unknown as :main."
  (agent-repl-test--with-clean-state
    (puthash "ws" '() agent-repl--workspaces)
    (should-not (agent-repl--ws-merged-p "ws"))))

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
                 (lambda (_raw _prefixed git-root _base _fork-from &optional _model)
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
                 (lambda (_raw _prefixed _git-root base _fork-from &optional _model)
                   (setq captured-base base))))
        (agent-repl-create-doom-oneshot-workspace)
        (should (equal captured-base "master"))))))

(ert-deftest agent-repl-test-create-doom-oneshot-appends-merge-suffix-to-prefixed ()
  "The merge-on-success suffix is included in the PREFIXED prompt (the
spawned agent's first message) so the inner agent knows to invoke
`/create-or-update-workspace merge' after a successful, tested implementation."
  (agent-repl-test--with-clean-state
    (let ((captured-prefixed :unset))
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (&rest _) "tweak the modeline"))
                ((symbol-function 'agent-repl--spawn-workspace-generation)
                 (lambda (_raw prefixed _git-root _base _fork-from &optional _model)
                   (setq captured-prefixed prefixed))))
        (agent-repl-create-doom-oneshot-workspace)
        (should (string-match-p "/create-or-update-workspace merge" captured-prefixed))
        (should (string-match-p
                 (regexp-quote agent-repl--oneshot-merge-suffix)
                 captured-prefixed))))))

(ert-deftest agent-repl-test-create-doom-oneshot-keeps-raw-prompt-clean ()
  "The merge suffix is NOT appended to the raw prompt — raw is used purely
for slug generation and should not get polluted with skill names like
`/create-or-update-workspace merge', which would derail the workspace-name slug."
  (agent-repl-test--with-clean-state
    (let ((captured-raw :unset))
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (&rest _) "tweak the modeline"))
                ((symbol-function 'agent-repl--spawn-workspace-generation)
                 (lambda (raw _prefixed _git-root _base _fork-from &optional _model)
                   (setq captured-raw raw))))
        (agent-repl-create-doom-oneshot-workspace)
        (should (equal captured-raw "tweak the modeline"))
        (should-not (string-match-p "/create-or-update-workspace merge" captured-raw))))))

(ert-deftest agent-repl-test-create-doom-oneshot-prefixed-includes-autonomous-prefix ()
  "The prefixed prompt still starts with the standard autonomous-prompt
prefix so the spawned agent runs autonomously without waiting."
  (agent-repl-test--with-clean-state
    (let ((captured-prefixed :unset))
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (&rest _) "tweak the modeline"))
                ((symbol-function 'agent-repl--spawn-workspace-generation)
                 (lambda (_raw prefixed _git-root _base _fork-from &optional _model)
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
                 (lambda (_raw _prefixed _git-root _base fork-from &optional _model)
                   (setq captured-fork-from fork-from))))
        (agent-repl-create-doom-oneshot-workspace)
        (should (null captured-fork-from))))))

(ert-deftest agent-repl-test-create-doom-oneshot-forwards-model-to-spawn ()
  "A MODEL passed to the doom one-shot flows through to
`agent-repl--spawn-workspace-generation' so the workspace boots under it."
  (agent-repl-test--with-clean-state
    (let ((captured-model :unset))
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (&rest _) "tweak the modeline"))
                ((symbol-function 'agent-repl--spawn-workspace-generation)
                 (lambda (_raw _prefixed _git-root _base _fork-from &optional model)
                   (setq captured-model model))))
        (agent-repl-create-doom-oneshot-workspace nil "sonnet")
        (should (equal captured-model "sonnet"))))))

(ert-deftest agent-repl-test-create-doom-oneshot-passes-nil-model-by-default ()
  "The plain doom one-shot passes no model, so spawn receives nil and the
workspace falls back to `agent-repl-interactive-model'."
  (agent-repl-test--with-clean-state
    (let ((captured-model :unset))
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (&rest _) "tweak the modeline"))
                ((symbol-function 'agent-repl--spawn-workspace-generation)
                 (lambda (_raw _prefixed _git-root _base _fork-from &optional model)
                   (setq captured-model model))))
        (agent-repl-create-doom-oneshot-workspace)
        (should (null captured-model))))))

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
                 (lambda (_raw _prefixed _git-root base _fork-from &optional _model)
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
                 (lambda (_raw _prefixed git-root _base _fork-from &optional _model)
                   (setq captured-git-root git-root))))
        (agent-repl-create-doom-oneshot-workspace-from-current-branch)
        (should (equal captured-git-root agent-repl--doom-config-dir))))))

(ert-deftest agent-repl-test-create-doom-oneshot-from-current-branch-appends-merge-suffix ()
  "The current-branch variant must also append the merge-on-success suffix
to the prefixed prompt — the spawned agent still needs to know to invoke
`/create-or-update-workspace merge' after a successful implementation."
  (agent-repl-test--with-clean-state
    (let ((captured-prefixed :unset))
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (&rest _) "tweak the modeline"))
                ((symbol-function 'agent-repl--spawn-workspace-generation)
                 (lambda (_raw prefixed _git-root _base _fork-from &optional _model)
                   (setq captured-prefixed prefixed))))
        (agent-repl-create-doom-oneshot-workspace-from-current-branch)
        (should (string-match-p "/create-or-update-workspace merge" captured-prefixed))
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
                 (lambda (raw _prefixed _git-root _base _fork-from &optional _model)
                   (setq captured-raw raw))))
        (agent-repl-create-doom-oneshot-workspace-from-current-branch)
        (should (equal captured-raw "tweak the modeline"))
        (should-not (string-match-p "/create-or-update-workspace merge" captured-raw))))))

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
                 (lambda (_raw _prefixed _git-root base _fork-from &optional _model)
                   (setq captured-base base))))
        (agent-repl-create-doom-oneshot-workspace)
        (should (equal captured-base "master"))))))

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
                 (lambda (_raw _prefixed git-root _base _fork-from &optional _model)
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
                 (lambda (_raw _prefixed _git-root base _fork-from &optional _model)
                   (setq captured-base base))))
        (agent-repl-create-explanation-engine-oneshot-workspace)
        (should (equal captured-base "master"))))))

(ert-deftest agent-repl-test-explanation-engine-oneshot-appends-create-pr-suffix-to-prefixed ()
  "The create-PR-on-success suffix is included in the PREFIXED prompt so
the spawned agent knows to invoke
`agent-repl--oneshot-create-pr-command' on success — this replaces the
`/create-or-update-workspace merge' instruction used by the doom one-shot."
  (agent-repl-test--with-clean-state
    (let ((captured-prefixed :unset))
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (&rest _) "add caching to thing"))
                ((symbol-function 'agent-repl--spawn-workspace-generation)
                 (lambda (_raw prefixed _git-root _base _fork-from &optional _model)
                   (setq captured-prefixed prefixed))))
        (agent-repl-create-explanation-engine-oneshot-workspace)
        (should (string-match-p
                 (regexp-quote agent-repl--oneshot-create-pr-command)
                 captured-prefixed))
        (should (string-match-p
                 (regexp-quote agent-repl--oneshot-create-pr-suffix)
                 captured-prefixed))))))

(ert-deftest agent-repl-test-explanation-engine-oneshot-chains-workspace-merge-after-create-pr ()
  "The explanation-engine one-shot chains `/create-or-update-workspace merge' AFTER
`/create-or-update-pr' as a second-stage teardown — the prefixed prompt
must mention `/create-or-update-workspace merge', and it must appear textually AFTER the
`/create-or-update-pr' reference so the chain reads chronologically
(implement → PR → CICD → workspace-merge)."
  (agent-repl-test--with-clean-state
    (let ((captured-prefixed :unset))
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (&rest _) "add caching to thing"))
                ((symbol-function 'agent-repl--spawn-workspace-generation)
                 (lambda (_raw prefixed _git-root _base _fork-from &optional _model)
                   (setq captured-prefixed prefixed))))
        (agent-repl-create-explanation-engine-oneshot-workspace)
        (let ((pr-pos (string-match
                       (regexp-quote agent-repl--oneshot-create-pr-command)
                       captured-prefixed))
              (merge-pos (string-match "/create-or-update-workspace merge" captured-prefixed)))
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
                 (lambda (raw _prefixed _git-root _base _fork-from &optional _model)
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
                 (lambda (_raw prefixed _git-root _base _fork-from &optional _model)
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
                 (lambda (_raw _prefixed _git-root _base fork-from &optional _model)
                   (setq captured-fork-from fork-from))))
        (agent-repl-create-explanation-engine-oneshot-workspace)
        (should (null captured-fork-from))))))

(ert-deftest agent-repl-test-create-explanation-engine-oneshot-forwards-model-to-spawn ()
  "A MODEL passed to the explanation-engine one-shot flows through to
`agent-repl--spawn-workspace-generation' so the workspace boots under it."
  (agent-repl-test--with-clean-state
    (let ((captured-model :unset))
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (&rest _) "add caching to thing"))
                ((symbol-function 'agent-repl--spawn-workspace-generation)
                 (lambda (_raw _prefixed _git-root _base _fork-from &optional model)
                   (setq captured-model model))))
        (agent-repl-create-explanation-engine-oneshot-workspace "opus")
        (should (equal captured-model "opus"))))))

(ert-deftest agent-repl-test-create-explanation-engine-oneshot-passes-nil-model-by-default ()
  "The plain explanation-engine one-shot passes no model, so spawn receives
nil and the workspace falls back to `agent-repl-interactive-model'."
  (agent-repl-test--with-clean-state
    (let ((captured-model :unset))
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (&rest _) "add caching to thing"))
                ((symbol-function 'agent-repl--spawn-workspace-generation)
                 (lambda (_raw _prefixed _git-root _base _fork-from &optional model)
                   (setq captured-model model))))
        (agent-repl-create-explanation-engine-oneshot-workspace)
        (should (null captured-model))))))

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
  "The follow-up clause must reference `/create-or-update-workspace merge' — that's the
slash command the spawned agent invokes once CICD passes."
  (should (string-match-p "/create-or-update-workspace merge"
                          agent-repl--oneshot-create-pr-then-merge-followup)))

(ert-deftest agent-repl-test-oneshot-create-pr-then-merge-followup-gates-on-check-cicd-pass ()
  "The follow-up clause must explicitly gate `/create-or-update-workspace merge' on
`/check-cicd' returning PASS — without this gate the agent could tear
down the workspace even after a failing CI run."
  (should (string-match-p "/check-cicd"
                          agent-repl--oneshot-create-pr-then-merge-followup))
  (should (string-match-p "PASS"
                          agent-repl--oneshot-create-pr-then-merge-followup)))

(ert-deftest agent-repl-test-oneshot-create-pr-then-merge-followup-stops-on-check-cicd-fail ()
  "On CICD FAIL the follow-up clause must tell the agent to STOP and NOT
invoke `/create-or-update-workspace merge' — otherwise a failing CI could still lead to a
workspace teardown that loses the editor state without the change landing."
  (should (string-match-p "FAIL"
                          agent-repl--oneshot-create-pr-then-merge-followup))
  (should (string-match-p "STOP"
                          agent-repl--oneshot-create-pr-then-merge-followup))
  ;; The "do NOT invoke /create-or-update-workspace merge" instruction must appear so the
  ;; agent doesn't mis-read STOP as merely "stop the implementation" and
  ;; still fire the teardown.
  (should (string-match-p "NOT invoke `/create-or-update-workspace merge`"
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
                 (lambda (_raw _prefixed git-root _base _fork-from &optional _model)
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
                 (lambda (raw prefixed _git-root _base _fork-from &optional _model)
                   (setq captured-raw raw)
                   (setq captured-prefixed prefixed))))
        (agent-repl--create-pinned-oneshot-workspace
         "/tmp/repo/" 'master "::SENTINEL-SUFFIX::" "test-tag")
        (should-not (string-match-p "::SENTINEL-SUFFIX::" captured-raw))
        (should (string-match-p "::SENTINEL-SUFFIX::"
                                captured-prefixed))))))

(ert-deftest agent-repl-test-create-pinned-oneshot-forwards-model-to-spawn ()
  "MODEL is forwarded to `agent-repl--spawn-workspace-generation' as its
6th argument so the generated workspace boots under `--model MODEL'."
  (agent-repl-test--with-clean-state
    (let ((captured-model :unset))
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (&rest _) "do a thing"))
                ((symbol-function 'agent-repl--spawn-workspace-generation)
                 (lambda (_raw _prefixed _git-root _base _fork-from &optional model)
                   (setq captured-model model))))
        (agent-repl--create-pinned-oneshot-workspace
         "/tmp/repo/" 'master "SUFFIX" "test-tag" "sonnet")
        (should (equal captured-model "sonnet"))))))

(ert-deftest agent-repl-test-create-pinned-oneshot-passes-nil-model-when-absent ()
  "When no MODEL is supplied, spawn receives nil so the workspace falls
back to `agent-repl-interactive-model' — same as the plain one-shot."
  (agent-repl-test--with-clean-state
    (let ((captured-model :unset))
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (&rest _) "do a thing"))
                ((symbol-function 'agent-repl--spawn-workspace-generation)
                 (lambda (_raw _prefixed _git-root _base _fork-from &optional model)
                   (setq captured-model model))))
        (agent-repl--create-pinned-oneshot-workspace
         "/tmp/repo/" 'master "SUFFIX" "test-tag")
        (should (null captured-model))))))

;;;; ---- Tests: agent-repl--read-oneshot-model ----

(ert-deftest agent-repl-test-read-oneshot-model-returns-entry ()
  "The reader returns the alias typed at the minibuffer verbatim."
  (cl-letf (((symbol-function 'completing-read)
             (lambda (&rest _) "sonnet")))
    (should (equal (agent-repl--read-oneshot-model) "sonnet"))))

(ert-deftest agent-repl-test-read-oneshot-model-trims-whitespace ()
  "Surrounding whitespace is stripped from the typed alias."
  (cl-letf (((symbol-function 'completing-read)
             (lambda (&rest _) "  opus  ")))
    (should (equal (agent-repl--read-oneshot-model) "opus"))))

(ert-deftest agent-repl-test-read-oneshot-model-rejects-empty ()
  "An empty entry signals `user-error' — the model-picking variants exist
precisely to specify a model, so a blank answer is a mistake."
  (cl-letf (((symbol-function 'completing-read)
             (lambda (&rest _) "")))
    (should-error (agent-repl--read-oneshot-model) :type 'user-error)))

(ert-deftest agent-repl-test-read-oneshot-model-rejects-whitespace-only ()
  "A whitespace-only entry signals `user-error' after trimming."
  (cl-letf (((symbol-function 'completing-read)
             (lambda (&rest _) "   ")))
    (should-error (agent-repl--read-oneshot-model) :type 'user-error)))

(ert-deftest agent-repl-test-read-oneshot-model-offers-candidates ()
  "The reader seeds `completing-read' with `agent-repl-oneshot-model-candidates'
so known aliases complete."
  (let ((captured-collection :unset)
        (agent-repl-oneshot-model-candidates '("opus" "sonnet")))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _)
                 (setq captured-collection collection)
                 "opus")))
      (agent-repl--read-oneshot-model)
      (should (equal captured-collection '("opus" "sonnet"))))))

;;;; ---- Tests: model-picking one-shot commands ----

(ert-deftest agent-repl-test-doom-oneshot-with-model-reads-then-delegates ()
  "The `SPC j C-o' command reads a model, then dispatches the doom one-shot
with that model as the second arg (BASE left nil to default to master)."
  (let ((captured-base :unset)
        (captured-model :unset))
    (cl-letf (((symbol-function 'agent-repl--read-oneshot-model)
               (lambda () "sonnet"))
              ((symbol-function 'agent-repl-create-doom-oneshot-workspace)
               (lambda (&optional base model)
                 (setq captured-base base)
                 (setq captured-model model))))
      (agent-repl-create-doom-oneshot-workspace-with-model)
      (should (null captured-base))
      (should (equal captured-model "sonnet")))))

(ert-deftest agent-repl-test-explanation-engine-oneshot-with-model-reads-then-delegates ()
  "The `SPC j C-O' command reads a model, then dispatches the
explanation-engine one-shot with that model."
  (let ((captured-model :unset))
    (cl-letf (((symbol-function 'agent-repl--read-oneshot-model)
               (lambda () "opus"))
              ((symbol-function 'agent-repl-create-explanation-engine-oneshot-workspace)
               (lambda (&optional model)
                 (setq captured-model model))))
      (agent-repl-create-explanation-engine-oneshot-workspace-with-model)
      (should (equal captured-model "opus")))))

(ert-deftest agent-repl-test-doom-oneshot-with-model-aborts-on-empty-model ()
  "When the model reader aborts (empty entry -> `user-error'), the doom
one-shot flow is never dispatched."
  (let ((delegated nil))
    (cl-letf (((symbol-function 'agent-repl--read-oneshot-model)
               (lambda () (user-error "A model alias is required")))
              ((symbol-function 'agent-repl-create-doom-oneshot-workspace)
               (lambda (&rest _) (setq delegated t))))
      (should-error (agent-repl-create-doom-oneshot-workspace-with-model)
                    :type 'user-error)
      (should-not delegated))))

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
                 (lambda (_raw prefixed _git-root _base _fork-from &optional _model)
                   (setq captured-prefixed prefixed))))
        (agent-repl--create-pinned-oneshot-workspace
         "/tmp/repo/" 'master "SUFFIX" "test-tag")
        (should (string-match-p
                 (regexp-quote agent-repl--oneshot-no-action-suffix)
                 captured-prefixed))))))

;;;; ---- Tests: agent-repl--oneshot-history-push ----

(ert-deftest agent-repl-test-oneshot-history-push-adds-text ()
  "Pushing a non-empty prompt records it on the one-shot history."
  (let ((agent-repl--oneshot-prompt-history nil))
    (agent-repl--oneshot-history-push "fix the bug")
    (should (equal agent-repl--oneshot-prompt-history '("fix the bug")))))

(ert-deftest agent-repl-test-oneshot-history-push-skips-empty ()
  "An empty string is not recorded on the one-shot history."
  (let ((agent-repl--oneshot-prompt-history nil))
    (agent-repl--oneshot-history-push "")
    (should (null agent-repl--oneshot-prompt-history))))

(ert-deftest agent-repl-test-oneshot-history-push-skips-nil ()
  "A nil value is not recorded on the one-shot history."
  (let ((agent-repl--oneshot-prompt-history nil))
    (agent-repl--oneshot-history-push nil)
    (should (null agent-repl--oneshot-prompt-history))))

(ert-deftest agent-repl-test-oneshot-history-push-skips-whitespace-only ()
  "A whitespace-only string trims to empty and is not recorded."
  (let ((agent-repl--oneshot-prompt-history nil))
    (agent-repl--oneshot-history-push "   \n\t ")
    (should (null agent-repl--oneshot-prompt-history))))

(ert-deftest agent-repl-test-oneshot-history-push-trims-text ()
  "Surrounding whitespace is trimmed before the prompt is recorded."
  (let ((agent-repl--oneshot-prompt-history nil))
    (agent-repl--oneshot-history-push "  padded  ")
    (should (equal agent-repl--oneshot-prompt-history '("padded")))))

(ert-deftest agent-repl-test-oneshot-history-push-skips-duplicate-of-most-recent ()
  "A prompt identical to the most-recent entry is not recorded again."
  (let ((agent-repl--oneshot-prompt-history '("same")))
    (agent-repl--oneshot-history-push "same")
    (should (equal agent-repl--oneshot-prompt-history '("same")))))

(ert-deftest agent-repl-test-oneshot-history-push-allows-non-consecutive-duplicate ()
  "A prompt matching an older-but-not-most-recent entry is still recorded."
  (let ((agent-repl--oneshot-prompt-history '("b" "a")))
    (agent-repl--oneshot-history-push "a")
    (should (equal agent-repl--oneshot-prompt-history '("a" "b" "a")))))

(ert-deftest agent-repl-test-oneshot-history-push-prepends-most-recent-first ()
  "New prompts prepend so the history stays most-recent-first."
  (let ((agent-repl--oneshot-prompt-history '("old")))
    (agent-repl--oneshot-history-push "new")
    (should (equal agent-repl--oneshot-prompt-history '("new" "old")))))

;;;; ---- Tests: agent-repl--oneshot-capture-in-progress ----

(ert-deftest agent-repl-test-oneshot-capture-in-progress-records-contents ()
  "The capture hook records the active minibuffer's contents into the
in-progress variable."
  (let ((agent-repl--oneshot-prompt-in-progress nil))
    (cl-letf (((symbol-function 'minibuffer-contents)
               (lambda () "typed so far")))
      (agent-repl--oneshot-capture-in-progress)
      (should (equal agent-repl--oneshot-prompt-in-progress "typed so far")))))

;;;; ---- Tests: agent-repl--oneshot-read-prompt ----

(ert-deftest agent-repl-test-oneshot-read-prompt-returns-submitted-text ()
  "A normal submit returns the typed string."
  (let ((agent-repl--oneshot-prompt-history nil))
    (cl-letf (((symbol-function 'read-from-minibuffer)
               (lambda (&rest _) "do the thing")))
      (should (equal (agent-repl--oneshot-read-prompt "doom-oneshot")
                     "do the thing")))))

(ert-deftest agent-repl-test-oneshot-read-prompt-normal-submit-no-manual-push ()
  "A normal submit leaves the history untouched by the helper — the
built-in HIST mechanism records the submit, so the cleanup must not
double-push."
  (let ((agent-repl--oneshot-prompt-history nil))
    (cl-letf (((symbol-function 'read-from-minibuffer)
               (lambda (&rest _) "do the thing")))
      (agent-repl--oneshot-read-prompt "doom-oneshot"))
    (should (null agent-repl--oneshot-prompt-history))))

(ert-deftest agent-repl-test-oneshot-read-prompt-cancel-logs-partial ()
  "Cancelling the one-shot prompt pushes the entry-thus-far onto the
one-shot history so a later one-shot recalls it via up-arrow."
  (let ((agent-repl--oneshot-prompt-history nil))
    (cl-letf (((symbol-function 'read-from-minibuffer)
               (lambda (&rest _)
                 (setq agent-repl--oneshot-prompt-in-progress "partial prompt")
                 (signal 'quit nil))))
      (condition-case nil
          (agent-repl--oneshot-read-prompt "doom-oneshot")
        (quit nil)))
    (should (equal agent-repl--oneshot-prompt-history '("partial prompt")))))

(ert-deftest agent-repl-test-oneshot-read-prompt-cancel-reraises-quit ()
  "Cancelling still propagates the quit so the enclosing one-shot command
aborts exactly as before."
  (let ((agent-repl--oneshot-prompt-history nil)
        (propagated nil))
    (cl-letf (((symbol-function 'read-from-minibuffer)
               (lambda (&rest _) (signal 'quit nil))))
      (condition-case nil
          (agent-repl--oneshot-read-prompt "doom-oneshot")
        (quit (setq propagated t))))
    (should propagated)))

(ert-deftest agent-repl-test-oneshot-read-prompt-cancel-empty-no-push ()
  "Cancelling with nothing typed records nothing on the one-shot history."
  (let ((agent-repl--oneshot-prompt-history nil))
    (cl-letf (((symbol-function 'read-from-minibuffer)
               (lambda (&rest _) (signal 'quit nil))))
      (condition-case nil
          (agent-repl--oneshot-read-prompt "doom-oneshot")
        (quit nil)))
    (should (null agent-repl--oneshot-prompt-history))))

(ert-deftest agent-repl-test-oneshot-read-prompt-uses-dedicated-history ()
  "The one-shot read passes its dedicated history symbol as
`read-from-minibuffer's HIST so up-arrow cycles only one-shot prompts."
  (let ((captured-args nil))
    (cl-letf (((symbol-function 'read-from-minibuffer)
               (lambda (&rest args) (setq captured-args args) "x")))
      (agent-repl--oneshot-read-prompt "doom-oneshot"))
    (should (eq (nth 4 captured-args) 'agent-repl--oneshot-prompt-history))))

(ert-deftest agent-repl-test-oneshot-read-prompt-uses-oneshot-keymap ()
  "The one-shot read passes `agent-repl--oneshot-prompt-map' as the
minibuffer keymap so `C-RET' still appends the no-action suffix."
  (let ((captured-args nil))
    (cl-letf (((symbol-function 'read-from-minibuffer)
               (lambda (&rest args) (setq captured-args args) "x")))
      (agent-repl--oneshot-read-prompt "doom-oneshot"))
    (should (eq (nth 2 captured-args) agent-repl--oneshot-prompt-map))))

(ert-deftest agent-repl-test-oneshot-read-prompt-interpolates-tag ()
  "The minibuffer prompt interpolates TAG."
  (let ((captured-prompt nil))
    (cl-letf (((symbol-function 'read-from-minibuffer)
               (lambda (prompt &rest _) (setq captured-prompt prompt) "x")))
      (agent-repl--oneshot-read-prompt "doom-oneshot"))
    (should (equal captured-prompt "One-shot doom-oneshot prompt: "))))

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
  "When the recorded workspace dirname is not in the perspective list,
amend surfaces a user-error rather than persisting a ghost
`:pending-prompts' entry.  Unconditional check -- there is no
`:vterm-buffer'-present escape hatch that could skip it."
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

(ert-deftest agent-repl-test-resume-investigation-prompt-names-lost-session ()
  "The investigation prompt names the lost resume id."
  (let ((p (agent-repl--resume-investigation-prompt
            "3ef2f8f5-uuid" '("/cfg/projects/-w/3ef2f8f5-uuid.jsonl"))))
    (should (string-match-p "3ef2f8f5-uuid" p))))

(ert-deftest agent-repl-test-resume-investigation-prompt-searches-both-config-dirs ()
  "The investigation prompt directs the agent at BOTH config dirs."
  (let ((p (agent-repl--resume-investigation-prompt "u" nil)))
    (should (string-match-p "\\.claude`" p))
    (should (string-match-p "\\.claude-chesscom" p))))

(ert-deftest agent-repl-test-resume-investigation-prompt-lists-searched-paths ()
  "Daemon-reported searched paths appear in the prompt."
  (let ((p (agent-repl--resume-investigation-prompt "u" '("/a.jsonl" "/b.jsonl"))))
    (should (string-match-p "/a.jsonl, /b.jsonl" p))))

(ert-deftest agent-repl-test-resume-investigation-prompt-handles-no-paths ()
  "A nil searched-paths renders a placeholder rather than erroring."
  (let ((p (agent-repl--resume-investigation-prompt "u" nil)))
    (should (string-match-p "(none reported)" p))))

(ert-deftest agent-repl-test-dispatch-resume-investigation-signals-without-git-root ()
  "An unresolvable git root signals rather than creating a rootless worktree."
  (let ((agent-repl--resume-investigation-workspaces (make-hash-table :test 'equal)))
    (cl-letf (((symbol-function 'agent-repl--git-string-quiet)
               (lambda (&rest _) ""))
              ((symbol-function 'run-at-time)
               (lambda (&rest _) (error "must not schedule a create without a git root"))))
      (should-error (agent-repl--dispatch-resume-investigation "uuid-gone" nil "/w")))))

(ert-deftest agent-repl-test-merge-continue-after-resolve-sends-resume ()
  "The interactive continue command sends the resume over UDS for the current ws."
  (agent-repl-test--with-clean-state
    (let (resumed)
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
                ((symbol-function 'agent-repl--merge-resume-over-uds)
                 (lambda (ws) (setq resumed ws))))
        (agent-repl-workspace-merge-continue-after-resolve)
        (should (equal resumed "ws1"))))))

;;;; ---- Tests: workspace-merge-async is a bare daemon request ----

(ert-deftest agent-repl-test-merge-async-dispatches-over-uds ()
  "The async entry point sends the workspace straight to the daemon."
  (agent-repl-test--with-clean-state
    (let (dispatched)
      (cl-letf (((symbol-function 'agent-repl--merge-dispatch-over-uds)
                 (lambda (ws) (setq dispatched ws) "req-1")))
        (agent-repl--workspace-merge-async "ws1")
        (should (equal dispatched "ws1"))))))

(ert-deftest agent-repl-test-merge-async-returns-the-request-id ()
  "The async entry point hands back the daemon request-id."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--merge-dispatch-over-uds)
               (lambda (_ws) "req-4")))
      (should (equal (agent-repl--workspace-merge-async "ws1") "req-4")))))

(ert-deftest agent-repl-test-merge-async-does-not-pre-close-the-workspace ()
  "No pre-close: the daemon owns teardown, so the UI survives the request."
  (agent-repl-test--with-clean-state
    (let (closed)
      (cl-letf (((symbol-function 'agent-repl--merge-dispatch-over-uds)
                 (lambda (_ws) "req-1"))
                ((symbol-function 'agent-repl--close-workspace)
                 (lambda (&rest _) (setq closed t))))
        (agent-repl--workspace-merge-async "ws1")
        (should-not closed)))))

(ert-deftest agent-repl-test-merge-async-spawns-no-worker-thread ()
  "No worker thread: there is no local git work left to run off-main."
  (agent-repl-test--with-clean-state
    (let (threaded)
      (cl-letf (((symbol-function 'agent-repl--merge-dispatch-over-uds)
                 (lambda (_ws) "req-1"))
                ((symbol-function 'make-thread)
                 (lambda (&rest _) (setq threaded t) nil)))
        (agent-repl--workspace-merge-async "ws1")
        (should-not threaded)))))

(ert-deftest agent-repl-test-merge-async-propagates-a-refused-request ()
  "A refused request surfaces to the caller rather than being absorbed."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--merge-dispatch-over-uds)
               (lambda (_ws) (user-error "daemon refused"))))
      (should-error (agent-repl--workspace-merge-async "ws1")
                    :type 'user-error))))

;;;; ---- Tests: Emacs reacts to no merge state of its own ----

(ert-deftest agent-repl-test-no-merge-state-reactors-remain ()
  "Emacs runs no merge-state consequences: it only renders what is pushed."
  (dolist (sym '(agent-repl--merge-react-to-pushed-state
                 agent-repl--merge-react-merged
                 agent-repl--merge-react-conflict
                 agent-repl--merge-react-failed))
    (should-not (fboundp sym)))
  (should-not (cl-some (lambda (fn)
                         (and (symbolp fn)
                              (string-match-p "merge-react" (symbol-name fn))))
                       agent-repl-ws-state-transition-functions)))

;;; test-worktree.el ends here
