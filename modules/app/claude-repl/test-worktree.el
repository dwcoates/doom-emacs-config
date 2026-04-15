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

;;;; ---- Git test helpers ----

(defmacro claude-repl-test--with-temp-git-repo (var &rest body)
  "Create a temporary git repo, bind its path to VAR, execute BODY, then delete it."
  (declare (indent 1))
  `(let ((,var (make-temp-file "claude-repl-test-repo-" t)))
     (unwind-protect
         (progn
           (call-process "git" nil nil nil "-C" ,var "init" "-q")
           (call-process "git" nil nil nil "-C" ,var "config" "user.email" "test@test.com")
           (call-process "git" nil nil nil "-C" ,var "config" "user.name" "Test")
           (call-process "git" nil nil nil "-C" ,var "config" "commit.gpgsign" "false")
           ,@body)
       (delete-directory ,var t))))

(defun claude-repl-test--git-commit (repo msg &optional content)
  "Write CONTENT (default MSG) to a file named MSG in REPO, stage, and commit.
Using a per-commit filename ensures cherry-picks never conflict by default.
Returns the full SHA of the new commit."
  (let ((file (expand-file-name msg repo)))
    (write-region (or content msg) nil file)
    (call-process "git" nil nil nil "-C" repo "add" msg)
    (call-process "git" nil nil nil "-C" repo "commit" "-qm" msg))
  (string-trim (shell-command-to-string
                (format "git -C %s rev-parse HEAD" (shell-quote-argument repo)))))

(defun claude-repl-test--git-checkout (repo branch &optional create-p)
  "Checkout BRANCH in REPO. If CREATE-P, create it first."
  (if create-p
      (call-process "git" nil nil nil "-C" repo "checkout" "-qb" branch)
    (call-process "git" nil nil nil "-C" repo "checkout" "-q" branch)))

(defun claude-repl-test--git-cherry-pick-x (repo sha)
  "Cherry-pick SHA into REPO with -x. Returns exit code."
  (call-process "git" nil nil nil "-C" repo "cherry-pick" "-x" sha))

;;;; ---- Tests: extract-cherry-pick-shas ----

(ert-deftest claude-repl-test-extract-cherry-pick-shas-empty ()
  "Empty log text returns nil."
  (should (equal (claude-repl--extract-cherry-pick-shas "") nil)))

(ert-deftest claude-repl-test-extract-cherry-pick-shas-no-annotations ()
  "Log text without cherry-pick annotations returns nil."
  (should (equal (claude-repl--extract-cherry-pick-shas
                  "Some commit message\n\nAnother commit")
                 nil)))

(ert-deftest claude-repl-test-extract-cherry-pick-shas-single ()
  "Single cherry-pick annotation is extracted."
  (let ((sha "abc123def456789012345678901234567890abcd"))
    (should (equal (claude-repl--extract-cherry-pick-shas
                    (format "commit msg\n\n(cherry picked from commit %s)" sha))
                   (list sha)))))

(ert-deftest claude-repl-test-extract-cherry-pick-shas-multiple ()
  "Multiple cherry-pick annotations are extracted in reverse order (pushed)."
  (let ((sha1 "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
        (sha2 "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"))
    (let ((result (claude-repl--extract-cherry-pick-shas
                   (format "msg1\n(cherry picked from commit %s)\n\nmsg2\n(cherry picked from commit %s)"
                           sha1 sha2))))
      ;; push reverses order: sha2 first, then sha1
      (should (equal result (list sha2 sha1))))))

(ert-deftest claude-repl-test-extract-cherry-pick-shas-ignores-short-hashes ()
  "Short hashes (not 40 hex chars) are not matched."
  (should (equal (claude-repl--extract-cherry-pick-shas
                  "(cherry picked from commit abc123)")
                 nil)))

;;;; ---- Tests: bare-workspace-name ----

(ert-deftest claude-repl-test-bare-workspace-name-simple ()
  "Simple name returns itself."
  (should (equal (claude-repl--bare-workspace-name "foo") "foo")))

(ert-deftest claude-repl-test-bare-workspace-name-slashed ()
  "Path-style name returns only the last component."
  (should (equal (claude-repl--bare-workspace-name "DWC/foo") "foo")))

(ert-deftest claude-repl-test-bare-workspace-name-deep-path ()
  "Deeply nested path returns only the last component."
  (should (equal (claude-repl--bare-workspace-name "DWC/CV-100/cool-branch") "cool-branch")))

(ert-deftest claude-repl-test-bare-workspace-name-trailing-slash ()
  "Trailing slash is stripped before extracting."
  (should (equal (claude-repl--bare-workspace-name "DWC/foo/") "foo")))

;;;; ---- Tests: assert-clean-worktree ----

(ert-deftest claude-repl-test-assert-clean-worktree-clean ()
  "Clean worktree does not signal."
  (claude-repl-test--with-temp-git-repo repo
    (claude-repl-test--git-commit repo "initial" "content")
    ;; Should not error
    (claude-repl--assert-clean-worktree "test-ws" repo)))

(ert-deftest claude-repl-test-assert-clean-worktree-unstaged ()
  "Unstaged changes signal user-error."
  (claude-repl-test--with-temp-git-repo repo
    (claude-repl-test--git-commit repo "initial" "content")
    (write-region "modified" nil (expand-file-name "initial" repo))
    (should-error (claude-repl--assert-clean-worktree "test-ws" repo)
                  :type 'user-error)))

(ert-deftest claude-repl-test-assert-clean-worktree-staged ()
  "Staged changes signal user-error."
  (claude-repl-test--with-temp-git-repo repo
    (claude-repl-test--git-commit repo "initial" "content")
    (write-region "modified" nil (expand-file-name "initial" repo))
    (call-process "git" nil nil nil "-C" repo "add" "initial")
    (should-error (claude-repl--assert-clean-worktree "test-ws" repo)
                  :type 'user-error)))

;;;; ---- Tests: git-exit-code / git-branch-exists-p ----

(ert-deftest claude-repl-test-git-exit-code-success ()
  "Successful git command returns 0."
  (claude-repl-test--with-temp-git-repo repo
    (claude-repl-test--git-commit repo "initial" "content")
    (should (= 0 (claude-repl--git-exit-code repo "status")))))

(ert-deftest claude-repl-test-git-exit-code-failure ()
  "Failed git command returns non-zero."
  (claude-repl-test--with-temp-git-repo repo
    (claude-repl-test--git-commit repo "initial" "content")
    (should (/= 0 (claude-repl--git-exit-code repo "rev-parse" "--verify" "nonexistent-branch")))))

(ert-deftest claude-repl-test-git-branch-exists-p-true ()
  "Existing branch returns non-nil."
  (claude-repl-test--with-temp-git-repo repo
    (claude-repl-test--git-commit repo "initial" "content")
    (claude-repl-test--git-checkout repo "feature" t)
    (should (claude-repl--git-branch-exists-p repo "feature"))))

(ert-deftest claude-repl-test-git-branch-exists-p-false ()
  "Non-existent branch returns nil."
  (claude-repl-test--with-temp-git-repo repo
    (claude-repl-test--git-commit repo "initial" "content")
    (should-not (claude-repl--git-branch-exists-p repo "nonexistent"))))

;;;; ---- Tests: apply-workspace-properties ----

(ert-deftest claude-repl-test-apply-workspace-properties-nil-values-skipped ()
  "Nil values in the plist are not stored."
  (claude-repl-test--with-clean-state
    (claude-repl--apply-workspace-properties "ws1" :priority nil :fork-session-id "abc")
    (should (equal (claude-repl--ws-get "ws1" :fork-session-id) "abc"))
    (should (null (claude-repl--ws-get "ws1" :priority)))))

(ert-deftest claude-repl-test-apply-workspace-properties-all-non-nil ()
  "All non-nil values are stored."
  (claude-repl-test--with-clean-state
    (claude-repl--apply-workspace-properties "ws1" :priority 5 :fork-session-id "xyz")
    (should (equal (claude-repl--ws-get "ws1" :priority) 5))
    (should (equal (claude-repl--ws-get "ws1" :fork-session-id) "xyz"))))

(ert-deftest claude-repl-test-apply-workspace-properties-empty ()
  "Empty plist is a no-op."
  (claude-repl-test--with-clean-state
    (claude-repl--apply-workspace-properties "ws1")
    (should (null (gethash "ws1" claude-repl--workspaces)))))

;;;; ---- Tests: enqueue-preemptive-prompt ----

(ert-deftest claude-repl-test-enqueue-preemptive-prompt-stores ()
  "Non-empty prompt is stored as pending-prompts list."
  (claude-repl-test--with-clean-state
    (claude-repl--enqueue-preemptive-prompt "ws1" "do the thing")
    (should (equal (claude-repl--ws-get "ws1" :pending-prompts) '("do the thing")))
    (should (eq (claude-repl--ws-get "ws1" :pending-show-panels) t))))

(ert-deftest claude-repl-test-enqueue-preemptive-prompt-nil ()
  "Nil prompt does not store anything."
  (claude-repl-test--with-clean-state
    (claude-repl--enqueue-preemptive-prompt "ws1" nil)
    (should (null (claude-repl--ws-get "ws1" :pending-prompts)))))

(ert-deftest claude-repl-test-enqueue-preemptive-prompt-empty-string ()
  "Empty string prompt does not store anything."
  (claude-repl-test--with-clean-state
    (claude-repl--enqueue-preemptive-prompt "ws1" "")
    (should (null (claude-repl--ws-get "ws1" :pending-prompts)))))

;;;; ---- Tests: dispatch-prompt-command ----

(ert-deftest claude-repl-test-dispatch-prompt-enqueues-when-no-buffer ()
  "When no vterm buffer exists, prompt is enqueued on :pending-prompts."
  (claude-repl-test--with-clean-state
    (claude-repl--dispatch-prompt-command "ws1" "hello")
    (should (equal (claude-repl--ws-get "ws1" :pending-prompts) '("hello")))))

(ert-deftest claude-repl-test-dispatch-prompt-enqueues-when-not-ready ()
  "When vterm buffer exists but is not ready, prompt is enqueued."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-buffer "*claude-test-vterm*"
      (setq-local claude-repl--ready nil)
      (claude-repl--ws-put "ws1" :vterm-buffer (current-buffer))
      (claude-repl--dispatch-prompt-command "ws1" "hello")
      (should (equal (claude-repl--ws-get "ws1" :pending-prompts) '("hello"))))))

(ert-deftest claude-repl-test-dispatch-prompt-appends-to-existing ()
  "Multiple prompts are appended to :pending-prompts in order."
  (claude-repl-test--with-clean-state
    (claude-repl--dispatch-prompt-command "ws1" "first")
    (claude-repl--dispatch-prompt-command "ws1" "second")
    (should (equal (claude-repl--ws-get "ws1" :pending-prompts) '("first" "second")))))

(ert-deftest claude-repl-test-dispatch-prompt-normalizes-branch-name ()
  "Branch-style name 'DWC/foo' is normalized to 'foo'."
  (claude-repl-test--with-clean-state
    (claude-repl--dispatch-prompt-command "DWC/foo" "hello")
    (should (equal (claude-repl--ws-get "foo" :pending-prompts) '("hello")))))

;;;; ---- Tests: workspace-commands-watch-handler ----

(ert-deftest claude-repl-test-watch-handler-ignores-non-workspace-files ()
  "Files not starting with workspace_commands_ are ignored."
  (let ((called nil))
    (cl-letf (((symbol-function 'claude-repl--process-workspace-commands-file)
               (lambda (_f) (setq called t))))
      (claude-repl--workspace-commands-watch-handler
       '(descriptor changed "/tmp/some-other-file.json"))
      (should-not called))))

(ert-deftest claude-repl-test-watch-handler-dispatches-on-created ()
  "Created workspace_commands_ files trigger processing."
  (let ((captured-file nil))
    (cl-letf (((symbol-function 'claude-repl--process-workspace-commands-file)
               (lambda (f) (setq captured-file f))))
      (claude-repl--workspace-commands-watch-handler
       '(descriptor created "/tmp/workspace_commands_123.json"))
      (should (equal captured-file "/tmp/workspace_commands_123.json")))))

(ert-deftest claude-repl-test-watch-handler-dispatches-on-changed ()
  "Changed workspace_commands_ files trigger processing."
  (let ((captured-file nil))
    (cl-letf (((symbol-function 'claude-repl--process-workspace-commands-file)
               (lambda (f) (setq captured-file f))))
      (claude-repl--workspace-commands-watch-handler
       '(descriptor changed "/tmp/workspace_commands_abc.json"))
      (should (equal captured-file "/tmp/workspace_commands_abc.json")))))

(ert-deftest claude-repl-test-watch-handler-dispatches-on-renamed ()
  "Renamed events use the new-file (4th element) for workspace_commands_ files."
  (let ((captured-file nil))
    (cl-letf (((symbol-function 'claude-repl--process-workspace-commands-file)
               (lambda (f) (setq captured-file f))))
      (claude-repl--workspace-commands-watch-handler
       '(descriptor renamed "/tmp/old-name" "/tmp/workspace_commands_new.json"))
      (should (equal captured-file "/tmp/workspace_commands_new.json")))))

(ert-deftest claude-repl-test-watch-handler-ignores-delete-action ()
  "Delete actions are ignored even for workspace_commands_ files."
  (let ((called nil))
    (cl-letf (((symbol-function 'claude-repl--process-workspace-commands-file)
               (lambda (_f) (setq called t))))
      (claude-repl--workspace-commands-watch-handler
       '(descriptor deleted "/tmp/workspace_commands_del.json"))
      (should-not called))))

;;;; ---- Tests: dispatch-workspace-command ----

(ert-deftest claude-repl-test-dispatch-workspace-command-create ()
  "Create commands increment delay by stagger-seconds."
  (let ((handled nil))
    (cl-letf (((symbol-function 'claude-repl--handle-create-command)
               (lambda (cmd delay) (push (list cmd delay) handled))))
      (let ((cmd '((type . "create") (name . "test"))))
        (let ((new-delay (claude-repl--dispatch-workspace-command cmd 0)))
          (should (= new-delay claude-repl--worktree-stagger-seconds))
          (should (= (length handled) 1)))))))

(ert-deftest claude-repl-test-dispatch-workspace-command-prompt ()
  "Prompt commands do not change delay."
  (let ((handled nil))
    (cl-letf (((symbol-function 'claude-repl--handle-prompt-command)
               (lambda (cmd) (push cmd handled))))
      (let ((cmd '((type . "prompt") (workspace . "ws1") (prompt . "hello"))))
        (let ((new-delay (claude-repl--dispatch-workspace-command cmd 10)))
          (should (= new-delay 10))
          (should (= (length handled) 1)))))))

(ert-deftest claude-repl-test-dispatch-workspace-command-finish ()
  "Finish commands do not change delay."
  (let ((handled nil))
    (cl-letf (((symbol-function 'claude-repl--handle-finish-command)
               (lambda (cmd) (push cmd handled))))
      (let ((cmd '((type . "finish") (workspace . "ws1"))))
        (let ((new-delay (claude-repl--dispatch-workspace-command cmd 10)))
          (should (= new-delay 10))
          (should (= (length handled) 1)))))))

(ert-deftest claude-repl-test-dispatch-workspace-command-unknown ()
  "Unknown command type does not change delay and does not error."
  (let ((new-delay (claude-repl--dispatch-workspace-command
                    '((type . "bogus")) 10)))
    (should (= new-delay 10))))

;;;; ---- Tests: process-workspace-commands-file ----

(ert-deftest claude-repl-test-process-workspace-commands-file-missing ()
  "Missing file is handled gracefully (no error, just logged)."
  (claude-repl--process-workspace-commands-file "/nonexistent/file.json"))

(ert-deftest claude-repl-test-process-workspace-commands-file-creates-stagger ()
  "Multiple create commands get staggered delays."
  (let ((delays nil)
        (tmpfile (make-temp-file "ws-cmd-" nil ".json")))
    (unwind-protect
        (progn
          ;; Write a file with two create commands
          (with-temp-file tmpfile
            (insert "[{\"type\":\"create\",\"name\":\"ws1\"},{\"type\":\"create\",\"name\":\"ws2\"}]"))
          (cl-letf (((symbol-function 'claude-repl--handle-create-command)
                     (lambda (_cmd delay) (push delay delays))))
            (claude-repl--process-workspace-commands-file tmpfile))
          ;; Delays should be 0 and stagger-seconds
          (should (equal (reverse delays) (list 0 claude-repl--worktree-stagger-seconds)))
          ;; File should be deleted
          (should-not (file-exists-p tmpfile)))
      (when (file-exists-p tmpfile) (delete-file tmpfile)))))

(ert-deftest claude-repl-test-process-workspace-commands-file-mixed ()
  "Mixed create/prompt/finish commands dispatch correctly."
  (let ((create-count 0) (prompt-count 0) (finish-count 0)
        (tmpfile (make-temp-file "ws-cmd-" nil ".json")))
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (insert "[{\"type\":\"create\",\"name\":\"ws1\"},{\"type\":\"prompt\",\"workspace\":\"ws1\",\"prompt\":\"hi\"},{\"type\":\"finish\",\"workspace\":\"ws1\"}]"))
          (cl-letf (((symbol-function 'claude-repl--handle-create-command)
                     (lambda (_cmd _delay) (cl-incf create-count)))
                    ((symbol-function 'claude-repl--handle-prompt-command)
                     (lambda (_cmd) (cl-incf prompt-count)))
                    ((symbol-function 'claude-repl--handle-finish-command)
                     (lambda (_cmd) (cl-incf finish-count))))
            (claude-repl--process-workspace-commands-file tmpfile))
          (should (= create-count 1))
          (should (= prompt-count 1))
          (should (= finish-count 1)))
      (when (file-exists-p tmpfile) (delete-file tmpfile)))))

;;;; ---- Tests: worktree-add-callback ----

(ert-deftest claude-repl-test-worktree-add-callback-failure ()
  "When git worktree add fails, callback is not called and error message is shown."
  (let ((finalized nil))
    (cl-letf (((symbol-function 'claude-repl--finalize-worktree-workspace)
               (lambda (&rest _args) (setq finalized t))))
      (claude-repl--worktree-add-callback
       "/tmp/path" "dirname" nil nil nil nil nil nil "git error output")
      (should-not finalized))))

(ert-deftest claude-repl-test-worktree-add-callback-success ()
  "When git worktree add succeeds, finalize is called."
  (let ((finalized nil))
    (cl-letf (((symbol-function 'claude-repl--finalize-worktree-workspace)
               (lambda (path dirname prompt priority fork-id bare-metal cb)
                 (setq finalized (list path dirname prompt priority fork-id bare-metal)))))
      (claude-repl--worktree-add-callback
       "/tmp/path" "dirname" "prompt" 5 "fork-123" nil nil t "ok")
      (should (equal finalized '("/tmp/path" "dirname" "prompt" 5 "fork-123" nil))))))

;;;; ---- Tests: worktree-fetch-callback ----

(ert-deftest claude-repl-test-worktree-fetch-callback-calls-add-fn ()
  "Fetch callback invokes the add-fn regardless of success."
  (let ((called nil))
    (claude-repl--worktree-fetch-callback (lambda () (setq called t)) nil "output")
    (should called)))

(ert-deftest claude-repl-test-worktree-fetch-callback-calls-add-fn-on-failure ()
  "Fetch callback invokes add-fn even when fetch fails."
  (let ((called nil))
    (claude-repl--worktree-fetch-callback (lambda () (setq called t)) nil "error")
    (should called)))

;;;; ---- Tests: validate-worktree-creation ----

(ert-deftest claude-repl-test-validate-worktree-creation-empty-name ()
  "Empty name signals user-error."
  (should-error (claude-repl--validate-worktree-creation "" "/root" "dir" "branch" "/path")
                :type 'user-error))

(ert-deftest claude-repl-test-validate-worktree-creation-existing-project ()
  "Existing projectile project signals user-error."
  (cl-letf (((symbol-function 'projectile-project-p) (lambda (_path) t)))
    (should-error (claude-repl--validate-worktree-creation "name" "/root" "dir" "branch" "/path")
                  :type 'user-error)))

(ert-deftest claude-repl-test-validate-worktree-creation-existing-branch ()
  "Existing branch signals user-error."
  (claude-repl-test--with-temp-git-repo repo
    (claude-repl-test--git-commit repo "initial" "content")
    (claude-repl-test--git-checkout repo "feature" t)
    (claude-repl-test--git-checkout repo "master")
    (cl-letf (((symbol-function 'projectile-project-p) (lambda (_path) nil)))
      (should-error (claude-repl--validate-worktree-creation
                     "feature" repo "feature" "feature" "/nonexistent")
                    :type 'user-error))))

(ert-deftest claude-repl-test-validate-worktree-creation-passes ()
  "Valid inputs do not signal."
  (claude-repl-test--with-temp-git-repo repo
    (claude-repl-test--git-commit repo "initial" "content")
    (cl-letf (((symbol-function 'projectile-project-p) (lambda (_path) nil)))
      ;; Should not error
      (claude-repl--validate-worktree-creation
       "new-feature" repo "new-feature" "new-feature" "/nonexistent"))))

;;;; ---- Tests: merge-fork (cherry-pick-base) ----
;; These tests exercise claude-repl--cherry-pick-base (aliased as
;; +dwc/workspace-merge--fork) against real git repos created in temp
;; directories, since the function shells out to git entirely.

(ert-deftest claude-repl-test-merge-fork-no-annotations-fallback ()
  "When HEAD has no -x annotations, fork falls back to merge-base HEAD TARGET."
  ;; Arrange: M on branch-a, B1 on branch-b (no cherry-picks yet)
  (claude-repl-test--with-temp-git-repo repo
    (let ((sha-m (claude-repl-test--git-commit repo "M" "base")))
      (claude-repl-test--git-checkout repo "branch-b" t)
      (claude-repl-test--git-commit repo "B1" "b1")
      (claude-repl-test--git-checkout repo "master")
      ;; Act
      (let ((fork (+dwc/workspace-merge--fork repo "branch-b")))
        ;; Assert: should be merge-base = sha-m
        (should (equal fork sha-m))))))

(ert-deftest claude-repl-test-merge-fork-clean-chain ()
  "After merging B (with -x), fork for C (descends from B) is B's tip SHA."
  ;; Arrange:
  ;;   branch-b: M -> B1 -> B2
  ;;   branch-c: M -> B1 -> B2 -> C1   (branched from branch-b)
  ;;   branch-a: M -> A1 -> B1'(-x B1) -> B2'(-x B2)
  (claude-repl-test--with-temp-git-repo repo
    (claude-repl-test--git-commit repo "M" "base")
    (claude-repl-test--git-checkout repo "branch-b" t)
    (claude-repl-test--git-commit repo "B1" "b1")
    (let ((sha-b2 (claude-repl-test--git-commit repo "B2" "b2")))
      (claude-repl-test--git-checkout repo "branch-c" t)
      (claude-repl-test--git-commit repo "C1" "c1")
      (claude-repl-test--git-checkout repo "master")
      (claude-repl-test--git-checkout repo "branch-a" t)
      (claude-repl-test--git-commit repo "A1" "a1")
      ;; Cherry-pick B1 then B2 with -x onto branch-a
      (let ((sha-b1 (string-trim (shell-command-to-string
                                  (format "git -C %s rev-parse branch-b~1"
                                          (shell-quote-argument repo))))))
        (claude-repl-test--git-cherry-pick-x repo sha-b1)
        (claude-repl-test--git-cherry-pick-x repo sha-b2))
      ;; Act: compute fork for branch-c
      (let ((fork (+dwc/workspace-merge--fork repo "branch-c")))
        ;; Assert: fork should be sha-b2 (last incorporated commit in branch-c's history)
        (should (equal fork sha-b2))))))

(ert-deftest claude-repl-test-merge-fork-already-fully-merged ()
  "When all TARGET commits are incorporated, fork equals TARGET tip -> empty range."
  ;; Arrange: same as clean-chain but test against branch-b directly
  (claude-repl-test--with-temp-git-repo repo
    (claude-repl-test--git-commit repo "M" "base")
    (claude-repl-test--git-checkout repo "branch-b" t)
    (let ((sha-b1 (claude-repl-test--git-commit repo "B1" "b1"))
          (sha-b2 (claude-repl-test--git-commit repo "B2" "b2")))
      (claude-repl-test--git-checkout repo "master")
      (claude-repl-test--git-checkout repo "branch-a" t)
      (claude-repl-test--git-commit repo "A1" "a1")
      (claude-repl-test--git-cherry-pick-x repo sha-b1)
      (claude-repl-test--git-cherry-pick-x repo sha-b2)
      ;; Act
      (let* ((fork (+dwc/workspace-merge--fork repo "branch-b"))
             (range-count (string-trim
                           (shell-command-to-string
                            (format "git -C %s rev-list --count %s..branch-b"
                                    (shell-quote-argument repo)
                                    fork)))))
        ;; Assert: fork = sha-b2 (tip), range is empty
        (should (equal fork sha-b2))
        (should (equal range-count "0"))))))

(ert-deftest claude-repl-test-merge-fork-growing-workspace ()
  "After B is merged, adding B3 to branch-b; fork stays at B2 -> only B3 is new."
  (claude-repl-test--with-temp-git-repo repo
    (claude-repl-test--git-commit repo "M" "base")
    (claude-repl-test--git-checkout repo "branch-b" t)
    (let ((sha-b1 (claude-repl-test--git-commit repo "B1" "b1"))
          (sha-b2 (claude-repl-test--git-commit repo "B2" "b2")))
      (claude-repl-test--git-checkout repo "master")
      (claude-repl-test--git-checkout repo "branch-a" t)
      (claude-repl-test--git-commit repo "A1" "a1")
      (claude-repl-test--git-cherry-pick-x repo sha-b1)
      (claude-repl-test--git-cherry-pick-x repo sha-b2)
      ;; Simulate branch-b growing: add B3
      (claude-repl-test--git-checkout repo "branch-b")
      (claude-repl-test--git-commit repo "B3" "b3")
      (claude-repl-test--git-checkout repo "branch-a")
      ;; Act
      (let* ((fork (+dwc/workspace-merge--fork repo "branch-b"))
             (range-count (string-trim
                           (shell-command-to-string
                            (format "git -C %s rev-list --count %s..branch-b"
                                    (shell-quote-argument repo)
                                    fork)))))
        ;; Assert: fork = sha-b2, only B3 is in range
        (should (equal fork sha-b2))
        (should (equal range-count "1"))))))

(ert-deftest claude-repl-test-merge-fork-deep-chain ()
  "After merging B then C, fork for D (descends from C) is C's tip SHA."
  ;; branch-d: M -> B1 -> B2 -> C1 -> D1
  ;; branch-a has cherry-picked B1, B2, C1 with -x
  (claude-repl-test--with-temp-git-repo repo
    (claude-repl-test--git-commit repo "M" "base")
    (claude-repl-test--git-checkout repo "branch-b" t)
    (let ((sha-b1 (claude-repl-test--git-commit repo "B1" "b1"))
          (sha-b2 (claude-repl-test--git-commit repo "B2" "b2")))
      (claude-repl-test--git-checkout repo "branch-c" t)
      (let ((sha-c1 (claude-repl-test--git-commit repo "C1" "c1")))
        (claude-repl-test--git-checkout repo "branch-d" t)
        (claude-repl-test--git-commit repo "D1" "d1")
        (claude-repl-test--git-checkout repo "master")
        (claude-repl-test--git-checkout repo "branch-a" t)
        (claude-repl-test--git-commit repo "A1" "a1")
        (claude-repl-test--git-cherry-pick-x repo sha-b1)
        (claude-repl-test--git-cherry-pick-x repo sha-b2)
        (claude-repl-test--git-cherry-pick-x repo sha-c1)
        ;; Act
        (let ((fork (+dwc/workspace-merge--fork repo "branch-d")))
          ;; Assert: fork = sha-c1 (most recently incorporated commit in branch-d)
          (should (equal fork sha-c1)))))))

(ert-deftest claude-repl-test-merge-fork-annotation-survives-conflict-resolution ()
  "Annotation is written even when cherry-pick required conflict resolution via --continue."
  ;; Arrange: both branch-a and branch-b modify the same file explicitly -> conflict.
  ;; We use a shared "conflict-file" written by both branches, unlike the normal helpers
  ;; which use per-commit filenames to avoid conflicts.
  (claude-repl-test--with-temp-git-repo repo
    ;; M: create conflict-file with base content
    (write-region "base" nil (expand-file-name "conflict-file" repo))
    (call-process "git" nil nil nil "-C" repo "add" "conflict-file")
    (call-process "git" nil nil nil "-C" repo "commit" "-qm" "M")
    (claude-repl-test--git-checkout repo "branch-b" t)
    ;; B1: modify conflict-file on branch-b
    (write-region "branch-b-content" nil (expand-file-name "conflict-file" repo))
    (call-process "git" nil nil nil "-C" repo "add" "conflict-file")
    (call-process "git" nil nil nil "-C" repo "commit" "-qm" "B1")
    (let ((sha-b1 (string-trim (shell-command-to-string
                                (format "git -C %s rev-parse HEAD" (shell-quote-argument repo))))))
      (claude-repl-test--git-checkout repo "branch-c" t)
      (claude-repl-test--git-commit repo "C1" "c1")
      (claude-repl-test--git-checkout repo "master")
      (claude-repl-test--git-checkout repo "branch-a" t)
      ;; A1: also modify conflict-file on branch-a -> cherry-pick of B1 will conflict
      (write-region "branch-a-content" nil (expand-file-name "conflict-file" repo))
      (call-process "git" nil nil nil "-C" repo "add" "conflict-file")
      (call-process "git" nil nil nil "-C" repo "commit" "-qm" "A1")
      ;; Cherry-pick B1 -> conflict (both modified conflict-file)
      (call-process "git" nil nil nil "-C" repo "cherry-pick" "-x" sha-b1)
      ;; Resolve: write resolved content and stage
      (write-region "resolved" nil (expand-file-name "conflict-file" repo))
      (call-process "git" nil nil nil "-C" repo "add" "conflict-file")
      ;; Continue -- sequencer writes -x annotation on finalization
      (call-process "git" nil nil nil
                    "-C" repo "-c" "core.editor=true"
                    "cherry-pick" "--continue" "--no-edit")
      ;; Assert: annotation present despite conflict resolution
      (let ((log-msg (shell-command-to-string
                      (format "git -C %s log --pretty=%%B -1" (shell-quote-argument repo)))))
        (should (string-match-p
                 (format "(cherry picked from commit %s)" sha-b1)
                 log-msg)))
      ;; Fork computation for branch-c correctly identifies sha-b1 as incorporated
      (let ((fork (+dwc/workspace-merge--fork repo "branch-c")))
        (should (equal fork sha-b1))))))

;;;; ---- Tests: cherry-pick-commits ----

(ert-deftest claude-repl-test-cherry-pick-commits-empty-range ()
  "When range is empty (0 commits), signals user-error."
  (claude-repl-test--with-temp-git-repo repo
    (claude-repl-test--git-commit repo "M" "base")
    (claude-repl-test--git-checkout repo "feature" t)
    ;; HEAD and feature are at the same commit; range HEAD..feature is empty
    (should-error (claude-repl--cherry-pick-commits repo "feature" "HEAD" "feature")
                  :type 'user-error)))

(ert-deftest claude-repl-test-cherry-pick-commits-success ()
  "Successful cherry-pick with no conflicts."
  (claude-repl-test--with-temp-git-repo repo
    (let ((sha-m (claude-repl-test--git-commit repo "M" "base")))
      (claude-repl-test--git-checkout repo "feature" t)
      (claude-repl-test--git-commit repo "F1" "f1")
      (claude-repl-test--git-checkout repo "master")
      ;; Cherry-pick feature's commits onto master
      (claude-repl--cherry-pick-commits repo "feature" sha-m "feature")
      ;; Verify the commit was applied
      (let ((log (string-trim
                  (shell-command-to-string
                   (format "git -C %s log --oneline -1" (shell-quote-argument repo))))))
        (should (string-match-p "F1" log))))))

(ert-deftest claude-repl-test-cherry-pick-commits-conflict-signals ()
  "Cherry-pick conflict opens magit and signals user-error."
  (claude-repl-test--with-temp-git-repo repo
    ;; Create base with shared file
    (write-region "base" nil (expand-file-name "shared" repo))
    (call-process "git" nil nil nil "-C" repo "add" "shared")
    (call-process "git" nil nil nil "-C" repo "commit" "-qm" "M")
    (let ((sha-m (string-trim (shell-command-to-string
                               (format "git -C %s rev-parse HEAD" (shell-quote-argument repo))))))
      (claude-repl-test--git-checkout repo "feature" t)
      (write-region "feature-content" nil (expand-file-name "shared" repo))
      (call-process "git" nil nil nil "-C" repo "add" "shared")
      (call-process "git" nil nil nil "-C" repo "commit" "-qm" "F1")
      (claude-repl-test--git-checkout repo "master")
      (write-region "master-content" nil (expand-file-name "shared" repo))
      (call-process "git" nil nil nil "-C" repo "add" "shared")
      (call-process "git" nil nil nil "-C" repo "commit" "-qm" "M2")
      ;; Cherry-pick should conflict
      (cl-letf (((symbol-function 'magit-status) (lambda (&rest _) nil)))
        (should-error (claude-repl--cherry-pick-commits repo "feature" sha-m "feature")
                      :type 'user-error)))))

;;;; ---- Tests: check-cherry-pick-conflict ----

(ert-deftest claude-repl-test-check-cherry-pick-conflict-no-conflict ()
  "When no CHERRY_PICK_HEAD exists, returns nil (no error)."
  (claude-repl-test--with-temp-git-repo repo
    (claude-repl-test--git-commit repo "M" "base")
    ;; No conflict in progress
    (should-not (claude-repl--check-cherry-pick-conflict "test-ws" repo "test-ws"))))

(ert-deftest claude-repl-test-check-cherry-pick-conflict-with-conflict ()
  "When CHERRY_PICK_HEAD exists, magit-status is called and user-error is signaled."
  (claude-repl-test--with-temp-git-repo repo
    ;; Set up a conflicting cherry-pick
    (write-region "base" nil (expand-file-name "file" repo))
    (call-process "git" nil nil nil "-C" repo "add" "file")
    (call-process "git" nil nil nil "-C" repo "commit" "-qm" "M")
    (claude-repl-test--git-checkout repo "feature" t)
    (write-region "feature" nil (expand-file-name "file" repo))
    (call-process "git" nil nil nil "-C" repo "add" "file")
    (call-process "git" nil nil nil "-C" repo "commit" "-qm" "F1")
    (let ((sha-f1 (string-trim (shell-command-to-string
                                (format "git -C %s rev-parse HEAD" (shell-quote-argument repo))))))
      (claude-repl-test--git-checkout repo "master")
      (write-region "master" nil (expand-file-name "file" repo))
      (call-process "git" nil nil nil "-C" repo "add" "file")
      (call-process "git" nil nil nil "-C" repo "commit" "-qm" "M2")
      ;; Create a conflict
      (call-process "git" nil nil nil "-C" repo "cherry-pick" "-x" sha-f1)
      ;; Now CHERRY_PICK_HEAD should exist
      (cl-letf (((symbol-function 'magit-status) (lambda (&rest _) nil)))
        (should-error (claude-repl--check-cherry-pick-conflict "test-ws" repo "test-ws")
                      :type 'user-error)))))

;;;; ---- Tests: finish-workspace ----

(ert-deftest claude-repl-test-finish-workspace-non-worktree ()
  "Finishing a non-worktree workspace cleans state and kills persp."
  (claude-repl-test--with-clean-state
    (let ((persp-killed nil))
      (claude-repl--ws-put "ws1" :project-dir "/tmp/fake")
      (cl-letf (((symbol-function 'claude-repl--kill-vterm-process) (lambda (_b) nil))
                ((symbol-function '+workspace-list-names) (lambda () '("ws1" "ws2")))
                ((symbol-function 'persp-kill) (lambda (ws) (setq persp-killed ws))))
        (claude-repl--finish-workspace "ws1")
        (should (equal persp-killed "ws1"))
        ;; State should be removed
        (should (null (gethash "ws1" claude-repl--workspaces)))))))

(ert-deftest claude-repl-test-finish-workspace-with-worktree ()
  "Finishing a worktree workspace removes the git worktree."
  (claude-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "ws-test-" t))
          (removed nil))
      (unwind-protect
          (progn
            (claude-repl--ws-put "ws1" :worktree-p t)
            (claude-repl--ws-put "ws1" :project-dir (file-name-as-directory tmpdir))
            (cl-letf (((symbol-function 'claude-repl--kill-vterm-process) (lambda (_b) nil))
                      ((symbol-function '+workspace-list-names) (lambda () '("ws1")))
                      ((symbol-function 'persp-kill) (lambda (_ws) nil))
                      ((symbol-function 'claude-repl--remove-git-worktree)
                       (lambda (dir) (setq removed dir))))
              (claude-repl--finish-workspace "ws1")
              (should (equal removed (file-name-as-directory tmpdir)))))
        (when (file-directory-p tmpdir)
          (delete-directory tmpdir t))))))

(ert-deftest claude-repl-test-finish-workspace-normalizes-name ()
  "Branch-style name 'DWC/foo' is normalized to 'foo'."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "foo" :project-dir "/tmp/fake")
    (cl-letf (((symbol-function 'claude-repl--kill-vterm-process) (lambda (_b) nil))
              ((symbol-function '+workspace-list-names) (lambda () '("foo")))
              ((symbol-function 'persp-kill) (lambda (_ws) nil)))
      (claude-repl--finish-workspace "DWC/foo")
      (should (null (gethash "foo" claude-repl--workspaces))))))

(ert-deftest claude-repl-test-finish-workspace-kills-vterm ()
  "Vterm buffer process is killed when present."
  (claude-repl-test--with-clean-state
    (let ((killed-buf nil))
      (claude-repl-test--with-temp-buffer "*claude-test-vterm*"
        (claude-repl--ws-put "ws1" :vterm-buffer (current-buffer))
        (cl-letf (((symbol-function 'claude-repl--kill-vterm-process)
                   (lambda (b) (setq killed-buf b)))
                  ((symbol-function '+workspace-list-names) (lambda () nil))
                  ((symbol-function 'persp-kill) (lambda (_ws) nil)))
          (claude-repl--finish-workspace "ws1")
          (should (equal killed-buf (get-buffer "*claude-test-vterm*"))))))))

(ert-deftest claude-repl-test-finish-workspace-no-persp-kill-if-not-listed ()
  "If workspace is not in +workspace-list-names, persp-kill is not called."
  (claude-repl-test--with-clean-state
    (let ((persp-killed nil))
      (claude-repl--ws-put "ws1" :project-dir "/tmp/fake")
      (cl-letf (((symbol-function 'claude-repl--kill-vterm-process) (lambda (_b) nil))
                ((symbol-function '+workspace-list-names) (lambda () '("other")))
                ((symbol-function 'persp-kill) (lambda (ws) (setq persp-killed ws))))
        (claude-repl--finish-workspace "ws1")
        (should-not persp-killed)))))

;;;; ---- Tests: resolve-worktree-paths ----

(ert-deftest claude-repl-test-resolve-worktree-paths-not-in-git-repo ()
  "When git rev-parse returns a fatal message, signals user-error."
  (cl-letf (((symbol-function 'claude-repl--git-string)
             (lambda (&rest _args) "fatal: not a git repository")))
    (should-error (claude-repl--resolve-worktree-paths "my-branch")
                  :type 'user-error)))

(ert-deftest claude-repl-test-resolve-worktree-paths-inside-worktree ()
  "Inside a worktree (.git is a file), new worktree is a sibling directory."
  (let ((tmpdir (make-temp-file "resolve-wt-test-" t)))
    (unwind-protect
        (let* ((fake-root (expand-file-name "existing-wt" tmpdir)))
          (make-directory fake-root t)
          ;; Simulate worktree: .git is a regular file, not a directory
          (write-region "gitdir: /some/other/.git/worktrees/existing-wt"
                        nil (expand-file-name ".git" fake-root))
          (cl-letf (((symbol-function 'claude-repl--git-string)
                     (lambda (&rest _args) fake-root)))
            (let ((result (claude-repl--resolve-worktree-paths "new-feature")))
              ;; :in-worktree should be t
              (should (eq (plist-get result :in-worktree) t))
              ;; :worktree-parent should be the parent of fake-root (i.e. tmpdir)
              (should (equal (plist-get result :worktree-parent)
                             (file-name-directory (directory-file-name fake-root))))
              ;; :path should be sibling
              (should (equal (plist-get result :path)
                             (expand-file-name "new-feature" tmpdir))))))
      (delete-directory tmpdir t))))

(ert-deftest claude-repl-test-resolve-worktree-paths-normal-repo ()
  "Normal repo (.git is a directory) creates a -worktrees sibling directory."
  (let ((tmpdir (make-temp-file "resolve-wt-test-" t)))
    (unwind-protect
        (let* ((fake-root (expand-file-name "my-repo" tmpdir)))
          (make-directory fake-root t)
          ;; Simulate normal repo: .git is a directory
          (make-directory (expand-file-name ".git" fake-root) t)
          (cl-letf (((symbol-function 'claude-repl--git-string)
                     (lambda (&rest _args) fake-root)))
            (let ((result (claude-repl--resolve-worktree-paths "new-feature")))
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
                                               (expand-file-name "my-repo-worktrees" tmpdir)))))))
      (delete-directory tmpdir t))))

(ert-deftest claude-repl-test-resolve-worktree-paths-nested-name-extracts-dirname ()
  "Nested branch name like DWC/CV-100/cool-branch extracts only 'cool-branch' as dirname."
  (let ((tmpdir (make-temp-file "resolve-wt-test-" t)))
    (unwind-protect
        (let* ((fake-root (expand-file-name "my-repo" tmpdir)))
          (make-directory fake-root t)
          (make-directory (expand-file-name ".git" fake-root) t)
          (cl-letf (((symbol-function 'claude-repl--git-string)
                     (lambda (&rest _args) fake-root)))
            (let ((result (claude-repl--resolve-worktree-paths "DWC/CV-100/cool-branch")))
              (should (equal (plist-get result :dirname) "cool-branch"))
              (should (equal (plist-get result :branch-name) "DWC/CV-100/cool-branch"))
              (should (equal (plist-get result :git-root) fake-root)))))
      (delete-directory tmpdir t))))

;;;; ---- Tests: workspace-branch ----

(ert-deftest claude-repl-test-workspace-branch-no-project-dir ()
  "When workspace has no :project-dir, returns nil."
  (claude-repl-test--with-clean-state
    ;; ws1 has no :project-dir set
    (should (null (claude-repl--workspace-branch "ws1")))))

(ert-deftest claude-repl-test-workspace-branch-git-fails ()
  "When git rev-parse returns a fatal message, returns nil."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :project-dir "/tmp/fake-dir/")
    (cl-letf (((symbol-function 'claude-repl--git-string)
               (lambda (&rest _args) "fatal: not a git repository")))
      (should (null (claude-repl--workspace-branch "ws1"))))))

(ert-deftest claude-repl-test-workspace-branch-detached-head ()
  "When branch is 'HEAD' (detached), returns the SHA from rev-parse HEAD."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :project-dir "/tmp/fake-dir/")
    (let ((call-count 0))
      (cl-letf (((symbol-function 'claude-repl--git-string)
                 (lambda (&rest args)
                   (cl-incf call-count)
                   (if (member "--abbrev-ref" args)
                       "HEAD"
                     "abc123def456"))))
        (should (equal (claude-repl--workspace-branch "ws1") "abc123def456"))))))

(ert-deftest claude-repl-test-workspace-branch-normal-branch ()
  "When git returns a normal branch name, returns it directly."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :project-dir "/tmp/fake-dir/")
    (cl-letf (((symbol-function 'claude-repl--git-string)
               (lambda (&rest _args) "DWC/my-feature")))
      (should (equal (claude-repl--workspace-branch "ws1") "DWC/my-feature")))))

(ert-deftest claude-repl-test-workspace-branch-empty-string ()
  "When git returns an empty string, returns nil."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :project-dir "/tmp/fake-dir/")
    (cl-letf (((symbol-function 'claude-repl--git-string)
               (lambda (&rest _args) "")))
      (should (null (claude-repl--workspace-branch "ws1"))))))

;;;; ---- Tests: resolve-fork-session-id ----

(ert-deftest claude-repl-test-resolve-fork-session-id-raw-below-16 ()
  "When raw prefix is less than 16, returns nil (no fork requested)."
  (should (null (claude-repl--resolve-fork-session-id 4))))

(ert-deftest claude-repl-test-resolve-fork-session-id-raw-16-with-session-id ()
  "When raw >= 16 and session-id exists, returns the session-id."
  (claude-repl-test--with-clean-state
    (let ((inst (make-claude-repl-instantiation :session-id "sess-abc-123")))
      (cl-letf (((symbol-function 'claude-repl--active-inst)
                 (lambda (_ws) inst))
                ((symbol-function '+workspace-current-name)
                 (lambda () "test-ws")))
        (should (equal (claude-repl--resolve-fork-session-id 16) "sess-abc-123"))))))

(ert-deftest claude-repl-test-resolve-fork-session-id-raw-16-no-session-id ()
  "When raw >= 16 but no session-id, signals user-error."
  (claude-repl-test--with-clean-state
    (let ((inst (make-claude-repl-instantiation :session-id nil)))
      (cl-letf (((symbol-function 'claude-repl--active-inst)
                 (lambda (_ws) inst))
                ((symbol-function '+workspace-current-name)
                 (lambda () "test-ws")))
        (should-error (claude-repl--resolve-fork-session-id 16)
                      :type 'user-error)))))

(ert-deftest claude-repl-test-resolve-fork-session-id-raw-64 ()
  "When raw is 64 (C-u C-u C-u), fork is still requested."
  (claude-repl-test--with-clean-state
    (let ((inst (make-claude-repl-instantiation :session-id "sess-xyz")))
      (cl-letf (((symbol-function 'claude-repl--active-inst)
                 (lambda (_ws) inst))
                ((symbol-function '+workspace-current-name)
                 (lambda () "test-ws")))
        (should (equal (claude-repl--resolve-fork-session-id 64) "sess-xyz"))))))

;;;; ---- Tests: setup-worktree-session ----

(ert-deftest claude-repl-test-setup-worktree-session-bare-metal ()
  "When force-bare-metal is t, :active-env is set to :bare-metal."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'claude-repl--register-worktree-ws)
               (lambda (_ws-id _path _ws) nil))
              ((symbol-function 'claude-repl--ensure-session)
               (lambda (_ws) nil))
              ((symbol-function 'claude-repl--active-inst)
               (lambda (_ws) (make-claude-repl-instantiation :start-cmd "claude"))))
      (claude-repl--setup-worktree-session "abc123" "/tmp/path" "ws1" t)
      (should (eq (claude-repl--ws-get "ws1" :active-env) :bare-metal)))))

(ert-deftest claude-repl-test-setup-worktree-session-sandbox ()
  "When force-bare-metal is nil, :active-env is set to :sandbox."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'claude-repl--register-worktree-ws)
               (lambda (_ws-id _path _ws) nil))
              ((symbol-function 'claude-repl--ensure-session)
               (lambda (_ws) nil))
              ((symbol-function 'claude-repl--active-inst)
               (lambda (_ws) (make-claude-repl-instantiation :start-cmd "claude"))))
      (claude-repl--setup-worktree-session "abc123" "/tmp/path" "ws1" nil)
      (should (eq (claude-repl--ws-get "ws1" :active-env) :sandbox)))))

(ert-deftest claude-repl-test-setup-worktree-session-binds-default-directory ()
  "During ensure-session, default-directory is bound to the worktree path."
  (claude-repl-test--with-clean-state
    (let ((captured-dir nil))
      (cl-letf (((symbol-function 'claude-repl--register-worktree-ws)
                 (lambda (_ws-id _path _ws) nil))
                ((symbol-function 'claude-repl--ensure-session)
                 (lambda (_ws) (setq captured-dir default-directory)))
                ((symbol-function 'claude-repl--active-inst)
                 (lambda (_ws) (make-claude-repl-instantiation :start-cmd "claude"))))
        (claude-repl--setup-worktree-session "abc123" "/tmp/my-worktree" "ws1" nil)
        (should (equal captured-dir "/tmp/my-worktree/"))))))

(ert-deftest claude-repl-test-setup-worktree-session-creates-instantiations ()
  "Both :sandbox and :bare-metal instantiation structs are created."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'claude-repl--register-worktree-ws)
               (lambda (_ws-id _path _ws) nil))
              ((symbol-function 'claude-repl--ensure-session)
               (lambda (_ws) nil))
              ((symbol-function 'claude-repl--active-inst)
               (lambda (_ws) (make-claude-repl-instantiation :start-cmd "claude"))))
      (claude-repl--setup-worktree-session "abc123" "/tmp/path" "ws1" t)
      (should (claude-repl-instantiation-p (claude-repl--ws-get "ws1" :sandbox)))
      (should (claude-repl-instantiation-p (claude-repl--ws-get "ws1" :bare-metal))))))

;;;; ---- Tests: async-git-sentinel ----

(ert-deftest claude-repl-test-async-git-sentinel-exit-success ()
  "Exit with code 0 calls callback with (t output)."
  (let ((captured-ok nil)
        (captured-output nil)
        (proc-buf (generate-new-buffer " *test-sentinel-ok*")))
    (unwind-protect
        (progn
          (with-current-buffer proc-buf
            (insert "  git output here  "))
          (let ((proc (start-process "test-sentinel" proc-buf "true")))
            ;; Wait for process to finish
            (while (process-live-p proc)
              (accept-process-output proc 0.1))
            (process-put proc 'claude-repl-callback
                         (lambda (ok output)
                           (setq captured-ok ok
                                 captured-output output)))
            (claude-repl--async-git-sentinel proc "finished\n")
            (should (eq captured-ok t))
            (should (equal captured-output "git output here"))))
      (when (buffer-live-p proc-buf)
        (kill-buffer proc-buf)))))

(ert-deftest claude-repl-test-async-git-sentinel-exit-failure ()
  "Exit with non-zero code calls callback with (nil output)."
  (let ((captured-ok 'not-set)
        (captured-output nil)
        (proc-buf (generate-new-buffer " *test-sentinel-fail*")))
    (unwind-protect
        (progn
          (with-current-buffer proc-buf
            (insert "fatal: error message"))
          (let ((proc (start-process "test-sentinel" proc-buf "false")))
            ;; Wait for process to finish
            (while (process-live-p proc)
              (accept-process-output proc 0.1))
            (process-put proc 'claude-repl-callback
                         (lambda (ok output)
                           (setq captured-ok ok
                                 captured-output output)))
            (claude-repl--async-git-sentinel proc "finished\n")
            (should (eq captured-ok nil))
            (should (equal captured-output "fatal: error message"))))
      (when (buffer-live-p proc-buf)
        (kill-buffer proc-buf)))))

(ert-deftest claude-repl-test-async-git-sentinel-signal ()
  "Signaled process also invokes the callback."
  (let ((captured-ok 'not-set)
        (captured-output nil)
        (proc-buf (generate-new-buffer " *test-sentinel-signal*")))
    (unwind-protect
        (progn
          (with-current-buffer proc-buf
            (insert "partial output"))
          (let ((proc (start-process "test-sentinel" proc-buf "sleep" "60")))
            (process-put proc 'claude-repl-callback
                         (lambda (ok output)
                           (setq captured-ok ok
                                 captured-output output)))
            ;; Kill the process to produce a signal
            (kill-process proc)
            ;; Wait for process to be fully dead
            (while (process-live-p proc)
              (accept-process-output proc 0.1))
            (claude-repl--async-git-sentinel proc "killed\n")
            (should (not (eq captured-ok 'not-set)))
            (should (stringp captured-output))))
      (when (buffer-live-p proc-buf)
        (kill-buffer proc-buf)))))

(ert-deftest claude-repl-test-async-git-sentinel-kills-process-buffer ()
  "Process buffer is killed after callback is invoked."
  (let ((proc-buf (generate-new-buffer " *test-sentinel-bufkill*")))
    (with-current-buffer proc-buf
      (insert "output"))
    (let ((proc (start-process "test-sentinel" proc-buf "true")))
      ;; Wait for process to finish
      (while (process-live-p proc)
        (accept-process-output proc 0.1))
      (process-put proc 'claude-repl-callback (lambda (_ok _output) nil))
      (claude-repl--async-git-sentinel proc "finished\n")
      (should-not (buffer-live-p proc-buf)))))

;;;; ---- Tests: open-initial-buffers (moved from core.el) ----

(ert-deftest claude-repl-test-open-initial-buffers-no-persp ()
  "open-initial-buffers should return nil when persp-get-by-name returns nil."
  (cl-letf (((symbol-function 'persp-get-by-name) (lambda (_name) nil)))
    (let ((claude-repl-workspace-initial-buffers '(("." . ("file.txt")))))
      ;; Should not error
      (should-not (claude-repl--open-initial-buffers "ws1" "/tmp/")))))

(ert-deftest claude-repl-test-open-initial-buffers-no-matching-pattern ()
  "open-initial-buffers should do nothing when no patterns match."
  (let ((add-called nil))
    (cl-letf (((symbol-function 'persp-get-by-name) (lambda (_name) 'fake-persp))
              ((symbol-function 'persp-add-buffer)
               (lambda (&rest _) (setq add-called t))))
      (let ((claude-repl-workspace-initial-buffers '(("^/specific/path" . ("file.txt")))))
        (claude-repl--open-initial-buffers "ws1" "/different/path")
        (should-not add-called)))))

(ert-deftest claude-repl-test-open-initial-buffers-missing-file-warns ()
  "open-initial-buffers should warn for missing files, not error."
  (let ((warned nil))
    (cl-letf (((symbol-function 'persp-get-by-name) (lambda (_name) 'fake-persp))
              ((symbol-function 'message)
               (lambda (fmt &rest _args)
                 (when (string-match-p "not found" fmt)
                   (setq warned t)))))
      (let ((claude-repl-workspace-initial-buffers
             '(("." . ("nonexistent-file-12345.txt")))))
        (claude-repl--open-initial-buffers "ws1" "/tmp/")
        (should warned)))))

(ert-deftest claude-repl-test-open-initial-buffers-existing-file ()
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
            (let ((claude-repl-workspace-initial-buffers
                   (list (cons "." '("found.txt")))))
              (claude-repl--open-initial-buffers "ws1" tmpdir)
              (should (= (length added-buffers) 1)))))
      ;; cleanup: kill the file buffer if it was created
      (let ((fb (get-buffer "found.txt")))
        (when fb (kill-buffer fb)))
      (delete-directory tmpdir t))))

(ert-deftest claude-repl-test-open-initial-buffers-multiple-patterns ()
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
            (let ((claude-repl-workspace-initial-buffers
                   (list (cons "." '("a.txt"))
                         (cons "." '("b.txt")))))
              (claude-repl--open-initial-buffers "ws1" tmpdir)
              (should (= added-count 2)))))
      (dolist (name '("a.txt" "b.txt"))
        (let ((fb (get-buffer name)))
          (when fb (kill-buffer fb))))
      (delete-directory tmpdir t))))

(ert-deftest claude-repl-test-open-initial-buffers-empty-files-list ()
  "open-initial-buffers with empty FILES list should be a no-op."
  (let ((add-called nil))
    (cl-letf (((symbol-function 'persp-get-by-name) (lambda (_name) 'fake-persp))
              ((symbol-function 'persp-add-buffer)
               (lambda (&rest _) (setq add-called t))))
      (let ((claude-repl-workspace-initial-buffers '(("." . ()))))
        (claude-repl--open-initial-buffers "ws1" "/tmp/")
        (should-not add-called)))))

;;; test-worktree.el ends here
