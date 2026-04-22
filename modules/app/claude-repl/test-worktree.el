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
    (claude-repl-test--with-temp-buffer "*claude-panel-test-vterm*"
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
          (should (= new-delay claude-repl-worktree-stagger-seconds))
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
          (should (equal (reverse delays) (list 0 claude-repl-worktree-stagger-seconds)))
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
      (claude-repl-test--with-temp-buffer "*claude-panel-test-vterm*"
        (claude-repl--ws-put "ws1" :vterm-buffer (current-buffer))
        (cl-letf (((symbol-function 'claude-repl--kill-vterm-process)
                   (lambda (b) (setq killed-buf b)))
                  ((symbol-function '+workspace-list-names) (lambda () nil))
                  ((symbol-function 'persp-kill) (lambda (_ws) nil)))
          (claude-repl--finish-workspace "ws1")
          (should (equal killed-buf (get-buffer "*claude-panel-test-vterm*"))))))))

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

(ert-deftest claude-repl-test-resolve-worktree-paths-uses-passed-git-root ()
  "Uses the GIT-ROOT argument, not `default-directory' or any cached variable."
  (let ((tmpdir (claude-repl--path-canonical
                 (make-temp-file "resolve-wt-test-" t))))
    (unwind-protect
        (let* ((fake-root (expand-file-name "my-repo" tmpdir)))
          (make-directory fake-root t)
          (make-directory (expand-file-name ".git" fake-root) t)
          (let ((default-directory "/nonexistent/should-not-matter/"))
            (let ((result (claude-repl--resolve-worktree-paths
                           (file-name-as-directory fake-root)
                           "new-feature")))
              (should (equal (plist-get result :git-root)
                             (claude-repl--path-canonical fake-root))))))
      (delete-directory tmpdir t))))

(ert-deftest claude-repl-test-resolve-worktree-paths-inside-worktree ()
  "Inside a worktree (.git is a file), new worktree is a sibling directory."
  ;; Canonicalize tmpdir up front: the function under test canonicalizes
  ;; its git-root, so the test's expected paths must also be canonical or
  ;; they'll mismatch on platforms with firmlinks (macOS /var -> /private/var).
  (let ((tmpdir (claude-repl--path-canonical
                 (make-temp-file "resolve-wt-test-" t))))
    (unwind-protect
        (let* ((fake-root (expand-file-name "existing-wt" tmpdir)))
          (make-directory fake-root t)
          ;; Simulate worktree: .git is a regular file, not a directory
          (write-region "gitdir: /some/other/.git/worktrees/existing-wt"
                        nil (expand-file-name ".git" fake-root))
          (let ((result (claude-repl--resolve-worktree-paths
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

(ert-deftest claude-repl-test-resolve-worktree-paths-normal-repo ()
  "Normal repo (.git is a directory) creates a -worktrees sibling directory."
  (let ((tmpdir (claude-repl--path-canonical
                 (make-temp-file "resolve-wt-test-" t))))
    (unwind-protect
        (let* ((fake-root (expand-file-name "my-repo" tmpdir)))
          (make-directory fake-root t)
          ;; Simulate normal repo: .git is a directory
          (make-directory (expand-file-name ".git" fake-root) t)
          (let ((result (claude-repl--resolve-worktree-paths
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

(ert-deftest claude-repl-test-resolve-worktree-paths-nested-name-extracts-dirname ()
  "Nested branch name like DWC/CV-100/cool-branch extracts only 'cool-branch' as dirname."
  (let ((tmpdir (claude-repl--path-canonical
                 (make-temp-file "resolve-wt-test-" t))))
    (unwind-protect
        (let* ((fake-root (expand-file-name "my-repo" tmpdir)))
          (make-directory fake-root t)
          (make-directory (expand-file-name ".git" fake-root) t)
          (let ((result (claude-repl--resolve-worktree-paths
                         (file-name-as-directory fake-root)
                         "DWC/CV-100/cool-branch")))
            (should (equal (plist-get result :dirname) "cool-branch"))
            (should (equal (plist-get result :branch-name) "DWC/CV-100/cool-branch"))
            (should (equal (plist-get result :git-root)
                           (claude-repl--path-canonical fake-root)))))
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

;;;; ---- Tests: fork-worktree-workspace ----

(ert-deftest claude-repl-test-fork-worktree-workspace-no-session-id ()
  "When current workspace has no session ID, fork signals user-error."
  (claude-repl-test--with-clean-state
    (let ((inst (make-claude-repl-instantiation :session-id nil)))
      (cl-letf (((symbol-function 'claude-repl--active-inst)
                 (lambda (_ws) inst))
                ((symbol-function '+workspace-current-name)
                 (lambda () "test-ws")))
        (should-error (claude-repl-fork-worktree-workspace nil)
                      :type 'user-error)))))

(ert-deftest claude-repl-test-fork-worktree-workspace-with-session-id ()
  "When session ID exists, fork passes it to do-create-worktree-workspace."
  (claude-repl-test--with-clean-state
    (let ((inst (make-claude-repl-instantiation :session-id "sess-abc-123"))
          (captured-fork-sid nil))
      (cl-letf (((symbol-function 'claude-repl--active-inst)
                 (lambda (_ws) inst))
                ((symbol-function '+workspace-current-name)
                 (lambda () "test-ws"))
                ((symbol-function 'read-string)
                 (lambda (prompt &rest _)
                   (if (string-match-p "name" prompt) "my-fork"
                     "")))
                ((symbol-function 'claude-repl--do-create-worktree-workspace)
                 (lambda (_name _bare fork-sid &rest _)
                   (setq captured-fork-sid fork-sid))))
        (claude-repl-fork-worktree-workspace nil)
        (should (equal captured-fork-sid "sess-abc-123"))))))

(ert-deftest claude-repl-test-fork-worktree-workspace-force-sandbox ()
  "With prefix arg, fork passes force-sandbox as t."
  (claude-repl-test--with-clean-state
    (let ((inst (make-claude-repl-instantiation :session-id "sess-xyz"))
          (captured-sandbox nil))
      (cl-letf (((symbol-function 'claude-repl--active-inst)
                 (lambda (_ws) inst))
                ((symbol-function '+workspace-current-name)
                 (lambda () "test-ws"))
                ((symbol-function 'read-string)
                 (lambda (prompt &rest _)
                   (if (string-match-p "name" prompt) "my-fork"
                     "")))
                ((symbol-function 'claude-repl--do-create-worktree-workspace)
                 (lambda (_name sandbox _fork-sid &rest _)
                   (setq captured-sandbox sandbox))))
        (claude-repl-fork-worktree-workspace '(4))
        (should (eq captured-sandbox t))))))

;;;; ---- Tests: setup-worktree-session ----

(ert-deftest claude-repl-test-setup-worktree-session-passes-sandbox-hint-when-forced ()
  "When force-sandbox is t, initialize-claude receives :sandbox as the env hint."
  (claude-repl-test--with-clean-state
    (let ((captured-env nil))
      (cl-letf (((symbol-function 'claude-repl--register-worktree-ws)
                 (lambda (_ws-id &optional _ws) nil))
                ((symbol-function 'claude-repl--initialize-claude)
                 (lambda (_ws &optional _dir env) (setq captured-env env)))
                ((symbol-function 'claude-repl--active-inst)
                 (lambda (_ws) (make-claude-repl-instantiation :start-cmd "claude"))))
        (claude-repl--setup-worktree-session "abc123" "/tmp/path" "ws1" t)
        (should (eq captured-env :sandbox))))))

(ert-deftest claude-repl-test-setup-worktree-session-passes-bare-metal-hint-by-default ()
  "When force-sandbox is nil, initialize-claude receives :bare-metal as the env hint."
  (claude-repl-test--with-clean-state
    (let ((captured-env nil))
      (cl-letf (((symbol-function 'claude-repl--register-worktree-ws)
                 (lambda (_ws-id &optional _ws) nil))
                ((symbol-function 'claude-repl--initialize-claude)
                 (lambda (_ws &optional _dir env) (setq captured-env env)))
                ((symbol-function 'claude-repl--active-inst)
                 (lambda (_ws) (make-claude-repl-instantiation :start-cmd "claude"))))
        (claude-repl--setup-worktree-session "abc123" "/tmp/path" "ws1" nil)
        (should (eq captured-env :bare-metal))))))

(ert-deftest claude-repl-test-setup-worktree-session-passes-path-hint ()
  "initialize-claude receives the worktree PATH as the project-dir hint."
  (claude-repl-test--with-clean-state
    (let ((captured-dir-hint nil))
      (cl-letf (((symbol-function 'claude-repl--register-worktree-ws)
                 (lambda (_ws-id &optional _ws) nil))
                ((symbol-function 'claude-repl--initialize-claude)
                 (lambda (_ws &optional dir _env) (setq captured-dir-hint dir)))
                ((symbol-function 'claude-repl--active-inst)
                 (lambda (_ws) (make-claude-repl-instantiation :start-cmd "claude"))))
        (claude-repl--setup-worktree-session "abc123" "/tmp/my-worktree" "ws1" nil)
        (should (equal captured-dir-hint "/tmp/my-worktree"))))))

(ert-deftest claude-repl-test-setup-worktree-session-binds-default-directory ()
  "During initialize-claude, default-directory is bound to the worktree path."
  (claude-repl-test--with-clean-state
    (let ((captured-dir nil))
      (cl-letf (((symbol-function 'claude-repl--register-worktree-ws)
                 (lambda (_ws-id &optional _ws) nil))
                ((symbol-function 'claude-repl--initialize-claude)
                 (lambda (_ws &optional _dir _env) (setq captured-dir default-directory)))
                ((symbol-function 'claude-repl--active-inst)
                 (lambda (_ws) (make-claude-repl-instantiation :start-cmd "claude"))))
        (claude-repl--setup-worktree-session "abc123" "/tmp/my-worktree" "ws1" nil)
        (should (equal captured-dir "/tmp/my-worktree/"))))))

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
            ;; Prevent default sentinel from inserting status text into buffer
            (set-process-sentinel proc #'ignore)
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
            ;; Prevent default sentinel from inserting status text into buffer
            (set-process-sentinel proc #'ignore)
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
              ((symbol-function 'claude-repl--log)
               (lambda (_ws fmt &rest _args)
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

;;;; ---- Tests: resolve-fork-session-id ----

(ert-deftest claude-repl-test-resolve-fork-session-id-nil ()
  "resolve-fork-session-id returns nil when fork-from is nil."
  (should-not (claude-repl--resolve-fork-session-id nil)))

(ert-deftest claude-repl-test-resolve-fork-session-id-known-ws ()
  "resolve-fork-session-id returns session ID for a known workspace."
  (claude-repl-test--with-clean-state
    (let ((inst (make-claude-repl-instantiation :session-id "abc-123")))
      (claude-repl--ws-put "my-ws" :active-env :bare-metal)
      (claude-repl--ws-put "my-ws" :bare-metal inst)
      (should (equal (claude-repl--resolve-fork-session-id "my-ws") "abc-123")))))

(ert-deftest claude-repl-test-resolve-fork-session-id-normalizes-branch ()
  "resolve-fork-session-id normalizes DWC/my-ws to my-ws."
  (claude-repl-test--with-clean-state
    (let ((inst (make-claude-repl-instantiation :session-id "def-456")))
      (claude-repl--ws-put "my-ws" :active-env :bare-metal)
      (claude-repl--ws-put "my-ws" :bare-metal inst)
      (should (equal (claude-repl--resolve-fork-session-id "DWC/my-ws") "def-456")))))

(ert-deftest claude-repl-test-resolve-fork-session-id-no-session-errors ()
  "resolve-fork-session-id signals error when workspace has no session ID."
  (claude-repl-test--with-clean-state
    (let ((inst (make-claude-repl-instantiation)))
      (claude-repl--ws-put "my-ws" :active-env :bare-metal)
      (claude-repl--ws-put "my-ws" :bare-metal inst)
      (should-error (claude-repl--resolve-fork-session-id "my-ws") :type 'error))))

(ert-deftest claude-repl-test-resolve-fork-session-id-unknown-ws-errors ()
  "resolve-fork-session-id signals error for an unknown workspace."
  (claude-repl-test--with-clean-state
    (should-error (claude-repl--resolve-fork-session-id "nonexistent") :type 'error)))

;;;; ---- Tests: handle-create-command with fork_from ----

(ert-deftest claude-repl-test-handle-create-command-with-fork-from ()
  "handle-create-command should resolve fork_from and pass fork-session-id."
  (claude-repl-test--with-clean-state
    (let ((inst (make-claude-repl-instantiation :session-id "fork-sid-789")))
      (claude-repl--ws-put "source-ws" :active-env :bare-metal)
      (claude-repl--ws-put "source-ws" :bare-metal inst)
      (let ((captured-args nil))
        (cl-letf (((symbol-function 'run-with-timer)
                   (lambda (_delay _repeat fn &rest args)
                     (setq captured-args args))))
          (claude-repl--handle-create-command
           '((type . "create") (name . "DWC/new-ws")
             (git_root . "/fake/root") (fork_from . "source-ws"))
           0)
          ;; captured-args = (git-root name prompt priority fork-session-id)
          (should (equal (nth 0 captured-args) "/fake/root/"))
          (should (equal (nth 1 captured-args) "DWC/new-ws"))
          (should (equal (nth 4 captured-args) "fork-sid-789")))))))

(ert-deftest claude-repl-test-handle-create-command-without-fork-from ()
  "handle-create-command without fork_from should pass nil fork-session-id."
  (claude-repl-test--with-clean-state
    (let ((captured-args nil))
      (cl-letf (((symbol-function 'run-with-timer)
                 (lambda (_delay _repeat fn &rest args)
                   (setq captured-args args))))
        (claude-repl--handle-create-command
         '((type . "create") (name . "DWC/new-ws") (git_root . "/fake/root"))
         0)
        ;; captured-args = (git-root name prompt priority fork-session-id)
        (should (equal (nth 0 captured-args) "/fake/root/"))
        (should (equal (nth 1 captured-args) "DWC/new-ws"))
        (should-not (nth 4 captured-args))))))

(ert-deftest claude-repl-test-handle-create-command-fork-from-no-session-aborts ()
  "handle-create-command with fork_from but no session should refuse to create workspace."
  (claude-repl-test--with-clean-state
    (let ((inst (make-claude-repl-instantiation)))
      (claude-repl--ws-put "source-ws" :active-env :bare-metal)
      (claude-repl--ws-put "source-ws" :bare-metal inst)
      (let ((timer-scheduled nil))
        (cl-letf (((symbol-function 'run-with-timer)
                   (lambda (_delay _repeat fn &rest args)
                     (setq timer-scheduled t))))
          (claude-repl--handle-create-command
           '((type . "create") (name . "DWC/new-ws") (fork_from . "source-ws"))
           0)
          ;; Timer must NOT be scheduled -- workspace creation was refused.
          (should-not timer-scheduled))))))

(ert-deftest claude-repl-test-handle-create-command-fork-from-unknown-ws-aborts ()
  "handle-create-command with fork_from referencing unknown workspace should refuse to create."
  (claude-repl-test--with-clean-state
    (let ((timer-scheduled nil))
      (cl-letf (((symbol-function 'run-with-timer)
                 (lambda (_delay _repeat fn &rest args)
                   (setq timer-scheduled t))))
        (claude-repl--handle-create-command
         '((type . "create") (name . "DWC/new-ws") (fork_from . "nonexistent"))
         0)
        (should-not timer-scheduled)))))

(ert-deftest claude-repl-test-handle-create-command-uses-explicit-git-root ()
  "handle-create-command with git_root in cmd should use it verbatim and skip ambient resolution."
  (claude-repl-test--with-clean-state
    (let ((captured-args nil)
          (resolve-calls 0))
      (cl-letf (((symbol-function 'claude-repl--resolve-current-git-root)
                 (lambda () (cl-incf resolve-calls) "/ambient/root/"))
                ((symbol-function 'run-with-timer)
                 (lambda (_delay _repeat _fn &rest args)
                   (setq captured-args args))))
        (claude-repl--handle-create-command
         '((type . "create") (name . "DWC/new-ws") (git_root . "/explicit/root"))
         0)
        ;; captured-args = (git-root name prompt priority fork-session-id)
        (should (equal (nth 0 captured-args) "/explicit/root/"))
        (should (equal resolve-calls 0))))))

(ert-deftest claude-repl-test-handle-create-command-expands-tilde-in-git-root ()
  "handle-create-command should expand `~' in an explicit git_root before dispatch."
  (claude-repl-test--with-clean-state
    (let ((captured-args nil)
          (home (expand-file-name "~")))
      (cl-letf (((symbol-function 'claude-repl--resolve-current-git-root)
                 (lambda () "/ambient/root/"))
                ((symbol-function 'run-with-timer)
                 (lambda (_delay _repeat _fn &rest args)
                   (setq captured-args args))))
        (claude-repl--handle-create-command
         '((type . "create") (name . "DWC/new-ws") (git_root . "~/some/repo"))
         0)
        (should (equal (nth 0 captured-args)
                       (file-name-as-directory (expand-file-name "~/some/repo"))))
        ;; Sanity: the expanded value is rooted at HOME, not the literal tilde.
        (should (string-prefix-p home (nth 0 captured-args)))))))

(ert-deftest claude-repl-test-handle-create-command-empty-git-root-refuses ()
  "handle-create-command with an empty git_root string must refuse — no ambient fallback."
  (claude-repl-test--with-clean-state
    (let ((timer-scheduled nil)
          (resolve-calls 0))
      (cl-letf (((symbol-function 'claude-repl--resolve-current-git-root)
                 (lambda () (cl-incf resolve-calls) "/ambient/root/"))
                ((symbol-function 'run-with-timer)
                 (lambda (_delay _repeat _fn &rest _args)
                   (setq timer-scheduled t))))
        (claude-repl--handle-create-command
         '((type . "create") (name . "DWC/new-ws") (git_root . ""))
         0)
        (should-not timer-scheduled)
        (should (equal resolve-calls 0))))))

(ert-deftest claude-repl-test-handle-create-command-missing-git-root-refuses ()
  "handle-create-command with no git_root key must refuse — no ambient fallback."
  (claude-repl-test--with-clean-state
    (let ((timer-scheduled nil)
          (resolve-calls 0))
      (cl-letf (((symbol-function 'claude-repl--resolve-current-git-root)
                 (lambda () (cl-incf resolve-calls) "/ambient/root/"))
                ((symbol-function 'run-with-timer)
                 (lambda (_delay _repeat _fn &rest _args)
                   (setq timer-scheduled t))))
        (claude-repl--handle-create-command
         '((type . "create") (name . "DWC/new-ws"))
         0)
        (should-not timer-scheduled)
        (should (equal resolve-calls 0))))))

;;;; ---- Tests: create-worktree-workspace (interactive) ----

(ert-deftest claude-repl-test-create-worktree-workspace-default-base-is-head ()
  "`SPC TAB n' with no prefix arg branches off HEAD (the current worktree)."
  (claude-repl-test--with-clean-state
    (let ((captured-base nil))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (prompt &rest _)
                   (if (string-match-p "name" prompt) "my-ws" "")))
                ((symbol-function 'claude-repl--do-create-worktree-workspace)
                 (lambda (_name _bare _fork _prompt _cb _priority base)
                   (setq captured-base base))))
        (claude-repl-create-worktree-workspace nil)
        (should (equal captured-base "HEAD"))))))

(ert-deftest claude-repl-test-create-worktree-workspace-c-u-base-is-origin-master ()
  "`C-u SPC TAB n' branches off origin/master."
  (claude-repl-test--with-clean-state
    (let ((captured-base nil))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (prompt &rest _)
                   (if (string-match-p "name" prompt) "my-ws" "")))
                ((symbol-function 'claude-repl--do-create-worktree-workspace)
                 (lambda (_name _bare _fork _prompt _cb _priority base)
                   (setq captured-base base))))
        (claude-repl-create-worktree-workspace '(4))
        (should (equal captured-base "origin/master"))))))

(ert-deftest claude-repl-test-create-worktree-workspace-prefixes-preemptive-prompt ()
  "When a preemptive prompt is given, it is prefixed with the autonomous instruction."
  (claude-repl-test--with-clean-state
    (let ((captured-prompt nil))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (prompt &rest _)
                   (if (string-match-p "name" prompt) "my-ws"
                     "do the thing")))
                ((symbol-function 'claude-repl--do-create-worktree-workspace)
                 (lambda (_name _bare _fork prompt &rest _)
                   (setq captured-prompt prompt))))
        (claude-repl-create-worktree-workspace nil)
        (should (string-prefix-p claude-repl--autonomous-prompt-prefix captured-prompt))
        (should (string-suffix-p "do the thing" captured-prompt))))))

(ert-deftest claude-repl-test-create-worktree-workspace-blank-prompt-passes-nil ()
  "When preemptive prompt is blank, nil is passed (no prefix prepended)."
  (claude-repl-test--with-clean-state
    (let ((captured-prompt :unset))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (prompt &rest _)
                   (if (string-match-p "name" prompt) "my-ws" "")))
                ((symbol-function 'claude-repl--do-create-worktree-workspace)
                 (lambda (_name _bare _fork prompt &rest _)
                   (setq captured-prompt prompt))))
        (claude-repl-create-worktree-workspace nil)
        (should (null captured-prompt))))))

(ert-deftest claude-repl-test-fork-worktree-workspace-prefixes-preemptive-prompt ()
  "When a preemptive prompt is given to fork, it is prefixed with the autonomous instruction."
  (claude-repl-test--with-clean-state
    (let ((inst (make-claude-repl-instantiation :session-id "sess-abc"))
          (captured-prompt nil))
      (cl-letf (((symbol-function 'claude-repl--active-inst)
                 (lambda (_ws) inst))
                ((symbol-function '+workspace-current-name)
                 (lambda () "test-ws"))
                ((symbol-function 'read-string)
                 (lambda (prompt &rest _)
                   (if (string-match-p "name" prompt) "my-fork"
                     "do the thing")))
                ((symbol-function 'claude-repl--do-create-worktree-workspace)
                 (lambda (_name _bare _fork prompt &rest _)
                   (setq captured-prompt prompt))))
        (claude-repl-fork-worktree-workspace nil)
        (should (string-prefix-p claude-repl--autonomous-prompt-prefix captured-prompt))
        (should (string-suffix-p "do the thing" captured-prompt))))))

(ert-deftest claude-repl-test-fork-worktree-workspace-blank-prompt-passes-nil ()
  "When fork preemptive prompt is blank, nil is passed (no prefix prepended)."
  (claude-repl-test--with-clean-state
    (let ((inst (make-claude-repl-instantiation :session-id "sess-abc"))
          (captured-prompt :unset))
      (cl-letf (((symbol-function 'claude-repl--active-inst)
                 (lambda (_ws) inst))
                ((symbol-function '+workspace-current-name)
                 (lambda () "test-ws"))
                ((symbol-function 'read-string)
                 (lambda (prompt &rest _)
                   (if (string-match-p "name" prompt) "my-fork" "")))
                ((symbol-function 'claude-repl--do-create-worktree-workspace)
                 (lambda (_name _bare _fork prompt &rest _)
                   (setq captured-prompt prompt))))
        (claude-repl-fork-worktree-workspace nil)
        (should (null captured-prompt))))))

;;;; ---- Tests: do-create-worktree-workspace base-commit + fetch ----

(ert-deftest claude-repl-test-do-create-base-commit-default-no-fork-is-origin-master ()
  "When BASE-COMMIT is nil and no FORK-SESSION-ID, the default is origin/master.
Preserves the programmatic worktree-creation path used by
`create-worktree-from-command' (Slack/command-file workspace creation)."
  (let ((add-args nil))
    (cl-letf (((symbol-function 'claude-repl--resolve-current-git-root)
               (lambda () "/g/"))
              ((symbol-function 'claude-repl--resolve-worktree-paths)
               (lambda (_git-root _name) (list :git-root "/g" :dirname "d" :branch-name "b"
                                               :in-worktree nil :path "/g/d")))
              ((symbol-function 'claude-repl--validate-worktree-creation)
               (lambda (&rest _) nil))
              ((symbol-function 'claude-repl--workspace-id) (lambda () "id"))
              ((symbol-function '+workspace-current-name) (lambda () "ws"))
              ((symbol-function 'claude-repl--async-git)
               (lambda (_label _root args _cb) (setq add-args args)))
              ((symbol-function 'claude-repl--async-worktree-add)
               (lambda (&rest _) nil)))
      (claude-repl--do-create-worktree-workspace "name" nil nil nil nil nil nil)
      ;; Fetch is scheduled for origin/master
      (should (equal add-args '("fetch" "origin" "master"))))))

(ert-deftest claude-repl-test-do-create-base-commit-default-with-fork-is-head ()
  "When BASE-COMMIT is nil and FORK-SESSION-ID is set, the default is HEAD.
Fork workflows need the session's tip; fetching origin/master would
reset that context."
  (let ((add-base nil))
    (cl-letf (((symbol-function 'claude-repl--resolve-current-git-root)
               (lambda () "/g/"))
              ((symbol-function 'claude-repl--resolve-worktree-paths)
               (lambda (_git-root _name) (list :git-root "/g" :dirname "d" :branch-name "b"
                                               :in-worktree nil :path "/g/d")))
              ((symbol-function 'claude-repl--validate-worktree-creation)
               (lambda (&rest _) nil))
              ((symbol-function 'claude-repl--workspace-id) (lambda () "id"))
              ((symbol-function '+workspace-current-name) (lambda () "ws"))
              ((symbol-function 'claude-repl--async-worktree-add)
               (lambda (_root _branch _path base &rest _) (setq add-base base))))
      (claude-repl--do-create-worktree-workspace "name" nil "sid-1" nil nil nil nil)
      (should (equal add-base "HEAD")))))

(ert-deftest claude-repl-test-do-create-base-commit-explicit-wins ()
  "Explicit BASE-COMMIT overrides the fork-derived default.
This is the path `claude-repl-create-worktree-workspace' uses to
force HEAD even without a fork-session-id."
  (let ((add-base nil))
    (cl-letf (((symbol-function 'claude-repl--resolve-current-git-root)
               (lambda () "/g/"))
              ((symbol-function 'claude-repl--resolve-worktree-paths)
               (lambda (_git-root _name) (list :git-root "/g" :dirname "d" :branch-name "b"
                                               :in-worktree nil :path "/g/d")))
              ((symbol-function 'claude-repl--validate-worktree-creation)
               (lambda (&rest _) nil))
              ((symbol-function 'claude-repl--workspace-id) (lambda () "id"))
              ((symbol-function '+workspace-current-name) (lambda () "ws"))
              ((symbol-function 'claude-repl--async-worktree-add)
               (lambda (_root _branch _path base &rest _) (setq add-base base))))
      (claude-repl--do-create-worktree-workspace "name" nil nil nil nil nil "HEAD")
      (should (equal add-base "HEAD")))))

(ert-deftest claude-repl-test-do-create-skips-fetch-when-base-is-head ()
  "No fetch runs when BASE-COMMIT is HEAD — nothing to pull from origin."
  (let ((fetch-called nil)
        (add-called nil))
    (cl-letf (((symbol-function 'claude-repl--resolve-current-git-root)
               (lambda () "/g/"))
              ((symbol-function 'claude-repl--resolve-worktree-paths)
               (lambda (_git-root _name) (list :git-root "/g" :dirname "d" :branch-name "b"
                                               :in-worktree nil :path "/g/d")))
              ((symbol-function 'claude-repl--validate-worktree-creation)
               (lambda (&rest _) nil))
              ((symbol-function 'claude-repl--workspace-id) (lambda () "id"))
              ((symbol-function '+workspace-current-name) (lambda () "ws"))
              ((symbol-function 'claude-repl--async-git)
               (lambda (&rest _) (setq fetch-called t)))
              ((symbol-function 'claude-repl--async-worktree-add)
               (lambda (&rest _) (setq add-called t))))
      (claude-repl--do-create-worktree-workspace "name" nil nil nil nil nil "HEAD")
      (should-not fetch-called)
      (should add-called))))

(ert-deftest claude-repl-test-do-create-fetch-ref-parsed-from-base ()
  "Fetch uses the ref name parsed from BASE-COMMIT after the origin/ prefix.
Supports bases other than origin/master without hard-coding the ref."
  (let ((fetch-args nil))
    (cl-letf (((symbol-function 'claude-repl--resolve-current-git-root)
               (lambda () "/g/"))
              ((symbol-function 'claude-repl--resolve-worktree-paths)
               (lambda (_git-root _name) (list :git-root "/g" :dirname "d" :branch-name "b"
                                               :in-worktree nil :path "/g/d")))
              ((symbol-function 'claude-repl--validate-worktree-creation)
               (lambda (&rest _) nil))
              ((symbol-function 'claude-repl--workspace-id) (lambda () "id"))
              ((symbol-function '+workspace-current-name) (lambda () "ws"))
              ((symbol-function 'claude-repl--async-git)
               (lambda (_label _root args _cb) (setq fetch-args args))))
      (claude-repl--do-create-worktree-workspace
       "name" nil nil nil nil nil "origin/develop")
      (should (equal fetch-args '("fetch" "origin" "develop"))))))

(ert-deftest claude-repl-test-do-create-fork-skips-fetch-regardless-of-base ()
  "FORK-SESSION-ID always skips fetch (Claude session-restore flow).
Even if someone passed an origin/ base-commit by mistake, the fork
path short-circuits to avoid disturbing the fork source's refs."
  (let ((fetch-called nil))
    (cl-letf (((symbol-function 'claude-repl--resolve-current-git-root)
               (lambda () "/g/"))
              ((symbol-function 'claude-repl--resolve-worktree-paths)
               (lambda (_git-root _name) (list :git-root "/g" :dirname "d" :branch-name "b"
                                               :in-worktree nil :path "/g/d")))
              ((symbol-function 'claude-repl--validate-worktree-creation)
               (lambda (&rest _) nil))
              ((symbol-function 'claude-repl--workspace-id) (lambda () "id"))
              ((symbol-function '+workspace-current-name) (lambda () "ws"))
              ((symbol-function 'claude-repl--async-git)
               (lambda (&rest _) (setq fetch-called t)))
              ((symbol-function 'claude-repl--async-worktree-add)
               (lambda (&rest _) nil)))
      (claude-repl--do-create-worktree-workspace
       "name" nil "sid-1" nil nil nil "origin/master")
      (should-not fetch-called))))

(ert-deftest claude-repl-test-do-create-uses-explicit-git-root-and-skips-resolver ()
  "When GIT-ROOT is passed explicitly, `--resolve-current-git-root' is NOT called.
This matters for the commands-file flow, which captures git-root at
enqueue and must not have it re-resolved at timer-fire time."
  (let ((resolver-called nil)
        (resolve-paths-root nil))
    (cl-letf (((symbol-function 'claude-repl--resolve-current-git-root)
               (lambda () (setq resolver-called t) "/SHOULD-NOT-BE-USED/"))
              ((symbol-function 'claude-repl--resolve-worktree-paths)
               (lambda (git-root _name)
                 (setq resolve-paths-root git-root)
                 (list :git-root git-root :dirname "d" :branch-name "b"
                       :in-worktree nil :path "/g/d")))
              ((symbol-function 'claude-repl--validate-worktree-creation)
               (lambda (&rest _) nil))
              ((symbol-function 'claude-repl--workspace-id) (lambda () "id"))
              ((symbol-function '+workspace-current-name) (lambda () "ws"))
              ((symbol-function 'claude-repl--async-worktree-add)
               (lambda (&rest _) nil)))
      (claude-repl--do-create-worktree-workspace
       "name" nil nil nil nil nil "HEAD" "/explicit/root/")
      (should-not resolver-called)
      (should (equal resolve-paths-root "/explicit/root/")))))

(ert-deftest claude-repl-test-do-create-resolves-git-root-when-omitted ()
  "When GIT-ROOT is nil, `--resolve-current-git-root' is called exactly once
and its result is threaded into `--resolve-worktree-paths'."
  (let ((resolver-calls 0)
        (resolve-paths-root nil))
    (cl-letf (((symbol-function 'claude-repl--resolve-current-git-root)
               (lambda () (cl-incf resolver-calls) "/resolved/root/"))
              ((symbol-function 'claude-repl--resolve-worktree-paths)
               (lambda (git-root _name)
                 (setq resolve-paths-root git-root)
                 (list :git-root git-root :dirname "d" :branch-name "b"
                       :in-worktree nil :path "/g/d")))
              ((symbol-function 'claude-repl--validate-worktree-creation)
               (lambda (&rest _) nil))
              ((symbol-function 'claude-repl--workspace-id) (lambda () "id"))
              ((symbol-function '+workspace-current-name) (lambda () "ws"))
              ((symbol-function 'claude-repl--async-worktree-add)
               (lambda (&rest _) nil)))
      (claude-repl--do-create-worktree-workspace
       "name" nil nil nil nil nil "HEAD")
      (should (= resolver-calls 1))
      (should (equal resolve-paths-root "/resolved/root/")))))

(ert-deftest claude-repl-test-create-worktree-from-command-forwards-git-root ()
  "`--create-worktree-from-command' forwards GIT-ROOT as the 8th arg to
`--do-create-worktree-workspace', preserving the value captured at enqueue."
  (let ((forwarded-args nil))
    (cl-letf (((symbol-function 'claude-repl--do-create-worktree-workspace)
               (lambda (&rest args) (setq forwarded-args args))))
      (claude-repl--create-worktree-from-command
       "/captured/root/" "ws-name" "some prompt" :high "fork-sid")
      ;; args: (name force-bare fork-sid prompt cb priority base-commit git-root)
      (should (equal (nth 0 forwarded-args) "ws-name"))
      (should (equal (nth 2 forwarded-args) "fork-sid"))
      (should (equal (nth 3 forwarded-args) "some prompt"))
      (should (equal (nth 5 forwarded-args) :high))
      (should (equal (nth 7 forwarded-args) "/captured/root/")))))

;;;; ---- Tests: async-worktree-add base-commit ----

(ert-deftest claude-repl-test-async-worktree-add-uses-base-commit ()
  "async-worktree-add passes BASE-COMMIT as the final `git worktree add' arg.
Covers the full call the interactive `SPC TAB n' path builds up."
  (let ((captured-args nil))
    (cl-letf (((symbol-function 'claude-repl--async-git)
               (lambda (_label _root args _cb) (setq captured-args args))))
      (claude-repl--async-worktree-add
       "/git-root" "my-branch" "/path" "HEAD"
       nil "dirname" nil nil nil nil)
      (should (equal captured-args
                     '("worktree" "add" "-b" "my-branch" "/path" "HEAD"))))))

;;;; ---- Tests: workspace-merge default selection ----

(ert-deftest claude-repl-test-workspace-merge-defaults-to-last-visited-claude-ws ()
  "workspace-merge pre-selects the most recently visited claude workspace."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "current" :project-dir "/tmp/cur")
    (claude-repl--ws-put "ws-a" :project-dir "/tmp/a")
    (claude-repl--ws-put "ws-b" :project-dir "/tmp/b")
    (let ((+dwc/workspace-history '("ws-b" "ws-a" "current"))
          (captured-default nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "current"))
                ((symbol-function '+workspace-list-names)
                 (lambda () '("current" "ws-a" "ws-b")))
                ((symbol-function 'claude-repl--assert-clean-worktree) #'ignore)
                ((symbol-function 'completing-read)
                 (lambda (_prompt _coll &optional _pred _req _init _hist default &rest _)
                   (setq captured-default default)
                   "ws-a"))
                ((symbol-function 'claude-repl--workspace-merge-do) #'ignore))
        (claude-repl-workspace-merge)
        (should (equal captured-default "ws-b"))))))

(ert-deftest claude-repl-test-workspace-merge-skips-non-claude-ws ()
  "workspace-merge skips workspaces not registered in claude-repl--workspaces."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "current" :project-dir "/tmp/cur")
    ;; Only ws-b is a claude workspace; ws-a is a plain workspace.
    (claude-repl--ws-put "ws-b" :project-dir "/tmp/b")
    (let ((+dwc/workspace-history '("ws-a" "ws-b" "current"))
          (captured-default nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "current"))
                ((symbol-function '+workspace-list-names)
                 (lambda () '("current" "ws-a" "ws-b")))
                ((symbol-function 'claude-repl--assert-clean-worktree) #'ignore)
                ((symbol-function 'completing-read)
                 (lambda (_prompt _coll &optional _pred _req _init _hist default &rest _)
                   (setq captured-default default)
                   "ws-b"))
                ((symbol-function 'claude-repl--workspace-merge-do) #'ignore))
        (claude-repl-workspace-merge)
        (should (equal captured-default "ws-b"))))))

(ert-deftest claude-repl-test-workspace-merge-no-default-when-history-empty ()
  "workspace-merge passes nil default when no history matches."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "current" :project-dir "/tmp/cur")
    (claude-repl--ws-put "ws-a" :project-dir "/tmp/a")
    (let ((+dwc/workspace-history nil)
          (captured-default nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "current"))
                ((symbol-function '+workspace-list-names)
                 (lambda () '("current" "ws-a")))
                ((symbol-function 'claude-repl--assert-clean-worktree) #'ignore)
                ((symbol-function 'completing-read)
                 (lambda (_prompt _coll &optional _pred _req _init _hist default &rest _)
                   (setq captured-default default)
                   "ws-a"))
                ((symbol-function 'claude-repl--workspace-merge-do) #'ignore))
        (claude-repl-workspace-merge)
        (should (null captured-default))))))

(ert-deftest claude-repl-test-workspace-merge-skips-current-ws-in-history ()
  "workspace-merge does not default to the current workspace even if most recent."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "current" :project-dir "/tmp/cur")
    (claude-repl--ws-put "ws-a" :project-dir "/tmp/a")
    (let ((+dwc/workspace-history '("current" "ws-a"))
          (captured-default nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "current"))
                ((symbol-function '+workspace-list-names)
                 (lambda () '("current" "ws-a")))
                ((symbol-function 'claude-repl--assert-clean-worktree) #'ignore)
                ((symbol-function 'completing-read)
                 (lambda (_prompt _coll &optional _pred _req _init _hist default &rest _)
                   (setq captured-default default)
                   "ws-a"))
                ((symbol-function 'claude-repl--workspace-merge-do) #'ignore))
        (claude-repl-workspace-merge)
        ;; current is removed from other-ws, so ws-a should be the default
        (should (equal captured-default "ws-a"))))))

;;;; ---- Tests: workspace-merge-do reloads config ----

(ert-deftest claude-repl-test-workspace-merge-do-reloads-config ()
  "workspace-merge-do calls load-file on claude-repl--config-file after cherry-picking."
  (let ((loaded-file nil))
    (cl-letf* (((symbol-function '+workspace-current-name) (lambda () "current"))
               ((symbol-function 'claude-repl--workspace-branch) (lambda (_ws) "branch-x"))
               ((symbol-function 'claude-repl--ws-dir) (lambda (_ws) "/tmp/fake"))
               ((symbol-function 'claude-repl--git-branch-exists-p) (lambda (_dir _br) t))
               ((symbol-function 'claude-repl--cherry-pick-base) (lambda (_dir _br) "abc123"))
               ((symbol-function 'claude-repl--cherry-pick-commits) (lambda (_dir _ws _base _br) nil))
               ((symbol-function 'claude-repl--finish-workspace) (lambda (_ws) nil))
               ((symbol-function 'load-file) (lambda (f) (setq loaded-file f)))
               ((symbol-function 'magit-status) #'ignore))
      (claude-repl--workspace-merge-do "other-ws")
      (should (equal loaded-file claude-repl--config-file)))))

(ert-deftest claude-repl-test-workspace-merge-do-reloads-before-magit ()
  "workspace-merge-do reloads config before opening magit-status."
  (let ((call-order nil))
    (cl-letf* (((symbol-function '+workspace-current-name) (lambda () "current"))
               ((symbol-function 'claude-repl--workspace-branch) (lambda (_ws) "branch-x"))
               ((symbol-function 'claude-repl--ws-dir) (lambda (_ws) "/tmp/fake"))
               ((symbol-function 'claude-repl--git-branch-exists-p) (lambda (_dir _br) t))
               ((symbol-function 'claude-repl--cherry-pick-base) (lambda (_dir _br) "abc123"))
               ((symbol-function 'claude-repl--cherry-pick-commits) (lambda (_dir _ws _base _br) nil))
               ((symbol-function 'claude-repl--finish-workspace) (lambda (_ws) nil))
               ((symbol-function 'load-file) (lambda (_f) (push 'reload call-order)))
               ((symbol-function 'magit-status) (lambda (_dir) (push 'magit call-order))))
      (claude-repl--workspace-merge-do "other-ws")
      (should (equal (nreverse call-order) '(reload magit))))))

(ert-deftest claude-repl-test-workspace-merge-do-magit-receives-project-dir ()
  "workspace-merge-do passes the current workspace's project directory to magit-status."
  (let ((magit-dir nil))
    (cl-letf* (((symbol-function '+workspace-current-name) (lambda () "current"))
               ((symbol-function 'claude-repl--workspace-branch) (lambda (_ws) "branch-x"))
               ((symbol-function 'claude-repl--ws-dir) (lambda (_ws) "/tmp/fake"))
               ((symbol-function 'claude-repl--git-branch-exists-p) (lambda (_dir _br) t))
               ((symbol-function 'claude-repl--cherry-pick-base) (lambda (_dir _br) "abc123"))
               ((symbol-function 'claude-repl--cherry-pick-commits) (lambda (_dir _ws _base _br) nil))
               ((symbol-function 'claude-repl--finish-workspace) (lambda (_ws) nil))
               ((symbol-function 'load-file) #'ignore)
               ((symbol-function 'magit-status) (lambda (dir) (setq magit-dir dir))))
      (claude-repl--workspace-merge-do "other-ws")
      (should (equal magit-dir "/tmp/fake")))))

;;;; ---- Tests: remove-doom-dashboard ----

(ert-deftest claude-repl-test-remove-doom-dashboard-removes-existing-buffer ()
  "Dashboard buffer is removed from the workspace when it exists."
  (let ((removed nil)
        (+doom-dashboard-buffer-name "*doom*"))
    (claude-repl-test--with-temp-buffer "*doom*"
      (cl-letf (((symbol-function 'persp-remove-buffer)
                 (lambda (buf) (setq removed buf)))
                ((symbol-function '+workspace-current-name) (lambda () "test-ws")))
        (claude-repl--remove-doom-dashboard)
        (should removed)
        (should (equal (buffer-name removed) "*doom*"))))))

(ert-deftest claude-repl-test-remove-doom-dashboard-noop-when-no-buffer ()
  "No error when the dashboard buffer does not exist."
  (let ((removed nil)
        (+doom-dashboard-buffer-name "*doom-nonexistent-xyz*"))
    (cl-letf (((symbol-function 'persp-remove-buffer)
               (lambda (buf) (setq removed buf)))
              ((symbol-function '+workspace-current-name) (lambda () "test-ws")))
      (claude-repl--remove-doom-dashboard)
      (should-not removed))))

(ert-deftest claude-repl-test-remove-doom-dashboard-noop-when-unbound ()
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
              (claude-repl--remove-doom-dashboard)
              (should-not removed))
          (when had-binding
            (setq +doom-dashboard-buffer-name "*doom*")))))))

(ert-deftest claude-repl-test-worktree-callback-only-switches ()
  "worktree-creation-switch-callback only switches workspace.
Magit-status and dashboard removal are handled by finalize-worktree-workspace."
  (let ((call-order nil))
    (cl-letf (((symbol-function 'claude-repl--switch-to-workspace)
               (lambda (_ws) (push 'switch call-order)))
              ((symbol-function '+workspace-current-name) (lambda () "test-ws")))
      (claude-repl--worktree-creation-switch-callback "/tmp/fake" "test-ws")
      (should (equal (reverse call-order) '(switch))))))

(ert-deftest claude-repl-test-new-workspace-removes-dashboard ()
  "new-workspace calls remove-doom-dashboard after magit."
  (let ((call-order nil)
        (+doom-dashboard-buffer-name "*doom*"))
    (claude-repl-test--with-temp-buffer "*doom*"
      (claude-repl-test--with-clean-state
        (cl-letf (((symbol-function 'claude-repl--git-root)
                   (lambda () "/tmp/fake-root"))
                  ((symbol-function '+workspace/new)
                   (lambda (&rest _) (push 'ws-new call-order)))
                  ((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                  ((symbol-function 'claude-repl--initialize-ws-env)
                   (lambda (_ws _root) (push 'init-env call-order)))
                  ((symbol-function 'magit-status)
                   (lambda (_path) (push 'magit call-order)))
                  ((symbol-function 'persp-remove-buffer)
                   (lambda (_buf) (push 'remove-dash call-order))))
          (claude-repl--new-workspace)
          (should (equal (reverse call-order) '(ws-new init-env magit remove-dash))))))))

;;;; ---- Tests: finalize-worktree-workspace defers magit via :pending-magit ----

(ert-deftest claude-repl-test-finalize-sets-pending-magit ()
  "finalize-worktree-workspace sets :pending-magit and does not call magit-status.
The drain happens on workspace activation; calling magit-status synchronously
here would open it in the caller's workspace layout, not the new one."
  (let ((magit-called nil))
    (claude-repl-test--with-clean-state
      (cl-letf (((symbol-function 'claude-repl--register-projectile-project) #'ignore)
                ((symbol-function 'claude-repl--path-canonical) #'identity)
                ((symbol-function '+workspace-new) #'ignore)
                ((symbol-function 'magit-status)
                 (lambda (&rest _) (setq magit-called t)))
                ((symbol-function 'claude-repl--remove-doom-dashboard)
                 (lambda (&rest _) (setq magit-called t)))
                ((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'claude-repl--open-initial-buffers) #'ignore)
                ((symbol-function 'claude-repl--enqueue-preemptive-prompt) #'ignore)
                ((symbol-function 'claude-repl--apply-workspace-properties) #'ignore)
                ((symbol-function 'claude-repl--setup-worktree-session) #'ignore))
        (claude-repl--finalize-worktree-workspace
         "/tmp/fake" "test-ws" nil nil nil nil nil)
        (should (claude-repl--ws-get "test-ws" :pending-magit))
        (should-not magit-called)))))

(ert-deftest claude-repl-test-finalize-sets-pending-magit-with-preemptive-prompt ()
  "finalize-worktree-workspace sets :pending-magit even when a preemptive prompt is set."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'claude-repl--register-projectile-project) #'ignore)
              ((symbol-function 'claude-repl--path-canonical) #'identity)
              ((symbol-function '+workspace-new) #'ignore)
              ((symbol-function 'magit-status) #'ignore)
              ((symbol-function 'claude-repl--remove-doom-dashboard) #'ignore)
              ((symbol-function '+workspace-current-name) (lambda () "test-ws"))
              ((symbol-function 'claude-repl--open-initial-buffers) #'ignore)
              ((symbol-function 'claude-repl--enqueue-preemptive-prompt) #'ignore)
              ((symbol-function 'claude-repl--apply-workspace-properties) #'ignore)
              ((symbol-function 'claude-repl--setup-worktree-session) #'ignore))
      (claude-repl--finalize-worktree-workspace
       "/tmp/fake" "test-ws" "do something" nil nil nil nil)
      (should (claude-repl--ws-get "test-ws" :pending-magit)))))

;;; test-worktree.el ends here
