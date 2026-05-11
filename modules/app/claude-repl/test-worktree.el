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

;;;; ---- Tests: parse-worktree-porcelain ----

(ert-deftest claude-repl-test-parse-worktree-porcelain-finds-master ()
  "Finds the worktree path whose branch matches the target ref."
  (let ((text (concat "worktree /repo/main\n"
                      "HEAD abc123\n"
                      "branch refs/heads/master\n"
                      "\n"
                      "worktree /repo/feature-x\n"
                      "HEAD def456\n"
                      "branch refs/heads/feature-x\n")))
    (should (equal (claude-repl--parse-worktree-porcelain text "refs/heads/master")
                   "/repo/main"))))

(ert-deftest claude-repl-test-parse-worktree-porcelain-finds-non-first-entry ()
  "Finds master even when it's not the first worktree listed."
  (let ((text (concat "worktree /repo/feature-x\n"
                      "HEAD def456\n"
                      "branch refs/heads/feature-x\n"
                      "\n"
                      "worktree /repo/main\n"
                      "HEAD abc123\n"
                      "branch refs/heads/master\n")))
    (should (equal (claude-repl--parse-worktree-porcelain text "refs/heads/master")
                   "/repo/main"))))

(ert-deftest claude-repl-test-parse-worktree-porcelain-no-match ()
  "Returns nil when no entry matches the target ref."
  (let ((text (concat "worktree /repo/feature-x\n"
                      "HEAD def456\n"
                      "branch refs/heads/feature-x\n")))
    (should (null (claude-repl--parse-worktree-porcelain text "refs/heads/master")))))

(ert-deftest claude-repl-test-parse-worktree-porcelain-ignores-detached-head ()
  "Worktrees with detached HEAD (no `branch' line) are not matched."
  (let ((text (concat "worktree /repo/detached\n"
                      "HEAD abc123\n"
                      "detached\n"
                      "\n"
                      "worktree /repo/main\n"
                      "HEAD def456\n"
                      "branch refs/heads/master\n")))
    (should (equal (claude-repl--parse-worktree-porcelain text "refs/heads/master")
                   "/repo/main"))))

(ert-deftest claude-repl-test-parse-worktree-porcelain-empty ()
  "Empty input returns nil."
  (should (null (claude-repl--parse-worktree-porcelain "" "refs/heads/master"))))

;;;; ---- Tests: master-worktree-path ----

(ert-deftest claude-repl-test-master-worktree-path-single-worktree ()
  "In a single-worktree repo on master, returns that worktree's path."
  (claude-repl-test--with-temp-git-repo repo
    (claude-repl-test--git-commit repo "initial" "content")
    (call-process "git" nil nil nil "-C" repo "branch" "-M" "master")
    (let ((claude-repl-master-branch-name "master"))
      (should (equal (file-truename (claude-repl--master-worktree-path repo))
                     (file-truename repo))))))

(ert-deftest claude-repl-test-master-worktree-path-with-secondary ()
  "In a repo with main + secondary worktree, returns the main path."
  (claude-repl-test--with-temp-git-repo repo
    (claude-repl-test--git-commit repo "initial" "content")
    (call-process "git" nil nil nil "-C" repo "branch" "-M" "master")
    (let ((wt-dir (make-temp-file "claude-repl-test-wt-" t)))
      (unwind-protect
          (progn
            (delete-directory wt-dir)
            (call-process "git" nil nil nil "-C" repo "worktree" "add" "-b" "feature-x" wt-dir)
            (let ((claude-repl-master-branch-name "master"))
              (should (equal (file-truename (claude-repl--master-worktree-path repo))
                             (file-truename repo)))
              ;; Even when called from the secondary worktree, returns master path.
              (should (equal (file-truename (claude-repl--master-worktree-path wt-dir))
                             (file-truename repo)))))
        (when (file-directory-p wt-dir)
          (delete-directory wt-dir t))))))

(ert-deftest claude-repl-test-master-worktree-path-no-master ()
  "When no worktree is on the master branch, returns nil."
  (claude-repl-test--with-temp-git-repo repo
    (claude-repl-test--git-commit repo "initial" "content")
    (call-process "git" nil nil nil "-C" repo "branch" "-M" "feature-only")
    (let ((claude-repl-master-branch-name "master"))
      (should (null (claude-repl--master-worktree-path repo))))))

(ert-deftest claude-repl-test-master-worktree-path-honors-defcustom ()
  "Uses `claude-repl-master-branch-name' as the trunk branch name."
  (claude-repl-test--with-temp-git-repo repo
    (claude-repl-test--git-commit repo "initial" "content")
    (call-process "git" nil nil nil "-C" repo "branch" "-M" "trunk")
    (let ((claude-repl-master-branch-name "trunk"))
      (should (equal (file-truename (claude-repl--master-worktree-path repo))
                     (file-truename repo))))))

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

(ert-deftest claude-repl-test-dispatch-workspace-command-merge ()
  "Merge commands do not change delay."
  (let ((handled nil))
    (cl-letf (((symbol-function 'claude-repl--handle-merge-command)
               (lambda (cmd) (push cmd handled))))
      (let ((cmd '((type . "merge") (workspace . "ws1"))))
        (let ((new-delay (claude-repl--dispatch-workspace-command cmd 10)))
          (should (= new-delay 10))
          (should (= (length handled) 1)))))))

(ert-deftest claude-repl-test-dispatch-workspace-command-unknown ()
  "Unknown command type does not change delay and does not error."
  (let ((new-delay (claude-repl--dispatch-workspace-command
                    '((type . "bogus")) 10)))
    (should (= new-delay 10))))

(ert-deftest claude-repl-test-dispatch-workspace-command-clipboard ()
  "Clipboard commands do not change delay and route to the handler."
  (let ((handled nil))
    (cl-letf (((symbol-function 'claude-repl--handle-clipboard-command)
               (lambda (cmd) (push cmd handled))))
      (let ((cmd '((type . "clipboard") (workspace . "ws1") (text . "hi"))))
        (let ((new-delay (claude-repl--dispatch-workspace-command cmd 10)))
          (should (= new-delay 10))
          (should (= (length handled) 1)))))))

;;;; ---- Tests: handle-clipboard-command ----

(ert-deftest claude-repl-test-handle-clipboard-command-stores-text ()
  "handle-clipboard-command stores `:text' on the workspace under `:clipboard'."
  (let ((claude-repl--workspaces (make-hash-table :test 'equal)))
    (puthash "ws1" '() claude-repl--workspaces)
    (claude-repl--handle-clipboard-command
     '((type . "clipboard") (workspace . "ws1") (text . "payload")))
    (should (equal (claude-repl--ws-get "ws1" :clipboard) "payload"))))

(ert-deftest claude-repl-test-handle-clipboard-command-missing-workspace ()
  "Missing `workspace' is logged and skipped — no error, no state change."
  (let ((claude-repl--workspaces (make-hash-table :test 'equal)))
    (claude-repl--handle-clipboard-command
     '((type . "clipboard") (text . "payload")))
    (should (= 0 (hash-table-count claude-repl--workspaces)))))

(ert-deftest claude-repl-test-handle-clipboard-command-missing-text ()
  "Missing `text' is logged and skipped — no error, slot stays nil."
  (let ((claude-repl--workspaces (make-hash-table :test 'equal)))
    (puthash "ws1" '() claude-repl--workspaces)
    (claude-repl--handle-clipboard-command
     '((type . "clipboard") (workspace . "ws1")))
    (should-not (claude-repl--ws-get "ws1" :clipboard))))

(ert-deftest claude-repl-test-handle-clipboard-command-overwrites ()
  "Successive clipboard commands overwrite the prior value."
  (let ((claude-repl--workspaces (make-hash-table :test 'equal)))
    (puthash "ws1" '() claude-repl--workspaces)
    (claude-repl--handle-clipboard-command
     '((workspace . "ws1") (text . "first")))
    (claude-repl--handle-clipboard-command
     '((workspace . "ws1") (text . "second")))
    (should (equal (claude-repl--ws-get "ws1" :clipboard) "second"))))

;;;; ---- Tests: handle-merge-command ----

(ert-deftest claude-repl-test-handle-merge-command-calls-merge-into-source ()
  "handle-merge-command forwards the workspace name to workspace-merge-into-source."
  (let ((received :unset))
    (cl-letf (((symbol-function 'claude-repl--workspace-merge-into-source)
               (lambda (ws) (setq received ws))))
      (claude-repl--handle-merge-command '((type . "merge") (workspace . "DWC/feature-one")))
      (should (equal received "DWC/feature-one")))))

(ert-deftest claude-repl-test-handle-merge-command-passes-bare-name-through ()
  "handle-merge-command does not normalize the workspace name itself —
normalization is the responsibility of workspace-merge-into-source so
both interactive and command-file callers get identical handling."
  (let ((received :unset))
    (cl-letf (((symbol-function 'claude-repl--workspace-merge-into-source)
               (lambda (ws) (setq received ws))))
      (claude-repl--handle-merge-command '((type . "merge") (workspace . "feature-one")))
      (should (equal received "feature-one")))))

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
       "/tmp/path" "dirname" nil nil nil nil nil nil nil "git error output")
      (should-not finalized))))

(ert-deftest claude-repl-test-worktree-add-callback-success ()
  "When git worktree add succeeds, finalize is called."
  (let ((finalized nil))
    (cl-letf (((symbol-function 'claude-repl--finalize-worktree-workspace)
               (lambda (path dirname prompt priority fork-id bare-metal _cb &optional source-dir)
                 (setq finalized (list path dirname prompt priority fork-id bare-metal source-dir)))))
      (claude-repl--worktree-add-callback
       "/tmp/path" "dirname" "prompt" 5 "fork-123" nil nil "/src/dir" t "ok")
      (should (equal finalized '("/tmp/path" "dirname" "prompt" 5 "fork-123" nil "/src/dir"))))))

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

(ert-deftest claude-repl-test-validate-worktree-creation-existing-path ()
  "An existing directory at PATH signals user-error."
  (claude-repl-test--with-temp-git-repo repo
    (claude-repl-test--git-commit repo "initial" "content")
    (should-error (claude-repl--validate-worktree-creation
                   "name" repo "dir" "branch" repo)
                  :type 'user-error)))

(ert-deftest claude-repl-test-validate-worktree-creation-existing-branch ()
  "Existing branch signals user-error."
  (claude-repl-test--with-temp-git-repo repo
    (claude-repl-test--git-commit repo "initial" "content")
    (claude-repl-test--git-checkout repo "feature" t)
    (claude-repl-test--git-checkout repo "master")
    (should-error (claude-repl--validate-worktree-creation
                   "feature" repo "feature" "feature" "/nonexistent")
                  :type 'user-error)))

(ert-deftest claude-repl-test-validate-worktree-creation-passes ()
  "Valid inputs do not signal."
  (claude-repl-test--with-temp-git-repo repo
    (claude-repl-test--git-commit repo "initial" "content")
    ;; Should not error
    (claude-repl--validate-worktree-creation
     "new-feature" repo "new-feature" "new-feature" "/nonexistent")))

(ert-deftest claude-repl-test-validate-worktree-creation-existing-tag-branch ()
  "Existing tag-branch (BRANCH+suffix) signals user-error."
  (claude-repl-test--with-temp-git-repo repo
    (claude-repl-test--git-commit repo "initial" "content")
    ;; Pre-create the tag branch that validation should detect.
    (claude-repl-test--git-checkout repo "feature-tag" t)
    (claude-repl-test--git-checkout repo "master")
    (let ((claude-repl-worktree-tag-branch-suffix "-tag"))
      (should-error (claude-repl--validate-worktree-creation
                     "feature" repo "feature" "feature" "/nonexistent")
                    :type 'user-error))))

(ert-deftest claude-repl-test-validate-worktree-creation-tag-branch-disabled ()
  "When tag-branch suffix is nil, an existing 'feature-tag' branch does not block."
  (claude-repl-test--with-temp-git-repo repo
    (claude-repl-test--git-commit repo "initial" "content")
    (claude-repl-test--git-checkout repo "feature-tag" t)
    (claude-repl-test--git-checkout repo "master")
    (let ((claude-repl-worktree-tag-branch-suffix nil))
      ;; Should not error
      (claude-repl--validate-worktree-creation
       "feature" repo "feature" "feature" "/nonexistent"))))

(ert-deftest claude-repl-test-validate-worktree-creation-nested-under-repo ()
  "Validation passes for a non-existent path nested under another git repo.
Regression: previously used `projectile-project-p', which walks UP from
PATH and would find an ancestor `.git' (e.g. when the worktree-parent
sits inside a separate repo), incorrectly flagging the new path as an
existing worktree."
  (claude-repl-test--with-temp-git-repo outer-repo
    (claude-repl-test--git-commit outer-repo "initial" "content")
    ;; outer-repo/inner is not itself a repo; outer-repo/inner/new-wt does
    ;; not exist. With projectile-project-p this would have thrown because
    ;; outer-repo has a .git ancestor; with file-directory-p it passes.
    (let* ((inner (expand-file-name "inner" outer-repo))
           (path (expand-file-name "new-wt" inner)))
      (make-directory inner t)
      ;; Should not error
      (claude-repl--validate-worktree-creation
       "new-wt" outer-repo "new-wt" "new-wt" path))))

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

(ert-deftest claude-repl-test-fork-worktree-workspace-with-session-id-passes-fork-from ()
  "When session ID exists, fork dispatches with FORK-FROM = current workspace.
The new flow no longer threads the session ID through the interactive
entry; it threads the workspace NAME (`fork_from`) into the
workspace-generation prompt, and the file-watcher resolves the session
ID later.  This test covers the entry's only remaining job: surfacing
the right fork-from name."
  (claude-repl-test--with-clean-state
    (let ((inst (make-claude-repl-instantiation :session-id "sess-abc-123"))
          (captured-fork-from :unset))
      (cl-letf (((symbol-function 'claude-repl--active-inst)
                 (lambda (_ws) inst))
                ((symbol-function '+workspace-current-name)
                 (lambda () "test-ws"))
                ((symbol-function 'claude-repl--resolve-current-git-root)
                 (lambda () "/tmp/cur-repo/"))
                ((symbol-function 'read-string)
                 (lambda (&rest _) "do the thing"))
                ((symbol-function 'claude-repl--spawn-workspace-generation)
                 (lambda (_raw _prefixed _git-root _base fork-from)
                   (setq captured-fork-from fork-from))))
        (claude-repl-fork-worktree-workspace nil)
        (should (equal captured-fork-from "test-ws"))))))

(ert-deftest claude-repl-test-fork-worktree-workspace-source-ws-forks-its-name ()
  "When SOURCE-WS is given, fork-from is that workspace's name."
  (claude-repl-test--with-clean-state
    (let ((source-inst (make-claude-repl-instantiation :session-id "sess-source"))
          (current-inst (make-claude-repl-instantiation :session-id "sess-current"))
          (captured-fork-from :unset))
      (cl-letf (((symbol-function 'claude-repl--active-inst)
                 (lambda (ws)
                   (cond ((equal ws "source-ws") source-inst)
                         ((equal ws "test-ws") current-inst)
                         (t (error "unexpected ws: %s" ws)))))
                ((symbol-function '+workspace-current-name)
                 (lambda () "test-ws"))
                ((symbol-function 'claude-repl--ws-dir)
                 (lambda (_ws) "/tmp/source-repo/"))
                ((symbol-function 'read-string)
                 (lambda (&rest _) "do the thing"))
                ((symbol-function 'claude-repl--spawn-workspace-generation)
                 (lambda (_raw _prefixed _git-root _base fork-from)
                   (setq captured-fork-from fork-from))))
        (claude-repl-fork-worktree-workspace "source-ws")
        (should (equal captured-fork-from "source-ws"))))))

(ert-deftest claude-repl-test-fork-worktree-workspace-source-ws-passes-git-root ()
  "When SOURCE-WS is given, its project-dir is threaded through as git-root."
  (claude-repl-test--with-clean-state
    (let ((source-inst (make-claude-repl-instantiation :session-id "sess-source"))
          (captured-git-root :unset))
      (cl-letf (((symbol-function 'claude-repl--active-inst)
                 (lambda (_ws) source-inst))
                ((symbol-function '+workspace-current-name)
                 (lambda () "test-ws"))
                ((symbol-function 'claude-repl--ws-dir)
                 (lambda (ws)
                   (if (equal ws "source-ws") "/tmp/source-repo/"
                     (error "unexpected ws: %s" ws))))
                ((symbol-function 'read-string)
                 (lambda (&rest _) "do the thing"))
                ((symbol-function 'claude-repl--spawn-workspace-generation)
                 (lambda (_raw _prefixed git-root _base _fork-from)
                   (setq captured-git-root git-root))))
        (claude-repl-fork-worktree-workspace "source-ws")
        (should (equal captured-git-root "/tmp/source-repo/"))))))

(ert-deftest claude-repl-test-fork-worktree-workspace-no-source-ws-resolves-ambient-git-root ()
  "With no SOURCE-WS and no ws-dir, git-root falls back to `resolve-current-git-root'.
Unlike the old flow (which passed nil and let `do-create' resolve later), the
new flow needs an explicit git-root to inject into the workspace-generation
JSON, so it eagerly resolves at entry-point time."
  (claude-repl-test--with-clean-state
    (let ((current-inst (make-claude-repl-instantiation :session-id "sess-current"))
          (captured-git-root :unset))
      (cl-letf (((symbol-function 'claude-repl--active-inst)
                 (lambda (ws)
                   (if (equal ws "test-ws") current-inst
                     (error "unexpected ws: %s" ws))))
                ((symbol-function '+workspace-current-name)
                 (lambda () "test-ws"))
                ((symbol-function 'claude-repl--ws-dir)
                 (lambda (_ws) nil))
                ((symbol-function 'claude-repl--resolve-current-git-root)
                 (lambda () "/tmp/ambient-repo/"))
                ((symbol-function 'read-string)
                 (lambda (&rest _) "do the thing"))
                ((symbol-function 'claude-repl--spawn-workspace-generation)
                 (lambda (_raw _prefixed git-root _base _fork-from)
                   (setq captured-git-root git-root))))
        (claude-repl-fork-worktree-workspace nil)
        (should (equal captured-git-root "/tmp/ambient-repo/"))))))

(ert-deftest claude-repl-test-fork-worktree-workspace-passes-head-as-base ()
  "Fork always passes BASE-COMMIT = \"HEAD\" to the spawn helper."
  (claude-repl-test--with-clean-state
    (let ((inst (make-claude-repl-instantiation :session-id "sess-abc-123"))
          (captured-base :unset))
      (cl-letf (((symbol-function 'claude-repl--active-inst)
                 (lambda (_ws) inst))
                ((symbol-function '+workspace-current-name)
                 (lambda () "test-ws"))
                ((symbol-function 'claude-repl--resolve-current-git-root)
                 (lambda () "/tmp/cur-repo/"))
                ((symbol-function 'read-string)
                 (lambda (&rest _) "do the thing"))
                ((symbol-function 'claude-repl--spawn-workspace-generation)
                 (lambda (_raw _prefixed _git-root base _fork-from)
                   (setq captured-base base))))
        (claude-repl-fork-worktree-workspace nil)
        (should (equal captured-base "HEAD"))))))

;;;; ---- Tests: git-root threading from interactive entry points ----

;; The new flow eagerly resolves a single git-root at entry-point time and
;; injects it into the workspace-generation JSON.  The downstream
;; `--create-worktree-from-command' uses that same git-root as both
;; git-root and source-dir on the new workspace, so source-dir threading
;; collapses into git-root threading at the entry-point layer.

(ert-deftest claude-repl-test-create-worktree-workspace-uses-current-ws-dir ()
  "Without explicit SOURCE-WS, git-root falls back to the current ws's :project-dir."
  (claude-repl-test--with-clean-state
    (let ((captured-git-root :unset))
      (cl-letf (((symbol-function '+workspace-current-name)
                 (lambda () "ambient-ws"))
                ((symbol-function 'claude-repl--ws-dir)
                 (lambda (ws)
                   (if (equal ws "ambient-ws") "/tmp/ambient-repo/"
                     (error "unexpected ws: %s" ws))))
                ((symbol-function 'read-string)
                 (lambda (&rest _) "do the thing"))
                ((symbol-function 'claude-repl--spawn-workspace-generation)
                 (lambda (_raw _prefixed git-root _base _fork-from)
                   (setq captured-git-root git-root))))
        (claude-repl-create-worktree-workspace 'head)
        (should (equal captured-git-root "/tmp/ambient-repo/"))))))

(ert-deftest claude-repl-test-create-worktree-workspace-uses-explicit-source-ws-dir ()
  "With explicit SOURCE-WS, git-root is that workspace's :project-dir."
  (claude-repl-test--with-clean-state
    (let ((captured-git-root :unset))
      (cl-letf (((symbol-function 'claude-repl--ws-dir)
                 (lambda (ws)
                   (if (equal ws "explicit-ws") "/tmp/explicit-repo/"
                     (error "unexpected ws: %s" ws))))
                ((symbol-function 'read-string)
                 (lambda (&rest _) "do the thing"))
                ((symbol-function 'claude-repl--spawn-workspace-generation)
                 (lambda (_raw _prefixed git-root _base _fork-from)
                   (setq captured-git-root git-root))))
        (claude-repl-create-worktree-workspace 'head "explicit-ws")
        (should (equal captured-git-root "/tmp/explicit-repo/"))))))

(ert-deftest claude-repl-test-fork-worktree-workspace-uses-fork-ws-dir ()
  "Fork's git-root is the fork-ws's :project-dir."
  (claude-repl-test--with-clean-state
    (let ((source-inst (make-claude-repl-instantiation :session-id "sess-source"))
          (captured-git-root :unset))
      (cl-letf (((symbol-function 'claude-repl--active-inst)
                 (lambda (_ws) source-inst))
                ((symbol-function 'claude-repl--ws-dir)
                 (lambda (ws)
                   (if (equal ws "fork-source") "/tmp/fork-source-repo/"
                     (error "unexpected ws: %s" ws))))
                ((symbol-function 'read-string)
                 (lambda (&rest _) "do the thing"))
                ((symbol-function 'claude-repl--spawn-workspace-generation)
                 (lambda (_raw _prefixed git-root _base _fork-from)
                   (setq captured-git-root git-root))))
        (claude-repl-fork-worktree-workspace "fork-source")
        (should (equal captured-git-root "/tmp/fork-source-repo/"))))))

(ert-deftest claude-repl-test-create-worktree-from-command-records-git-root-as-source-dir ()
  "The commands flow records GIT-ROOT as source-dir on the new ws."
  (claude-repl-test--with-clean-state
    (let ((captured-source-dir :unset))
      (cl-letf (((symbol-function 'claude-repl--do-create-worktree-workspace)
                 (lambda (_name _bare _fork _prompt _cb _priority _base &optional _git-root source-dir)
                   (setq captured-source-dir source-dir))))
        (claude-repl--create-worktree-from-command "/tmp/cmd-repo/" "name" "prompt" 5)
        (should (equal captured-source-dir "/tmp/cmd-repo/"))))))

(ert-deftest claude-repl-test-finalize-worktree-workspace-stores-source-ws-dir ()
  "Finalize persists :source-ws-dir on the new workspace's plist."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'claude-repl--register-projectile-project)
               (lambda (&rest _) nil))
              ((symbol-function '+workspace-new) (lambda (_ws) nil))
              ((symbol-function 'claude-repl--setup-worktree-session)
               (lambda (&rest _) nil))
              ((symbol-function 'claude-repl--path-canonical) #'identity))
      (claude-repl--finalize-worktree-workspace
       "/tmp/new-wt" "new-ws" nil nil nil nil nil "/tmp/source-repo/")
      (should (equal (claude-repl--ws-get "new-ws" :source-ws-dir)
                     "/tmp/source-repo/")))))

(ert-deftest claude-repl-test-finalize-worktree-workspace-omits-source-ws-dir-when-nil ()
  "When source-dir is nil, :source-ws-dir is not stored (apply-workspace-properties skips nil)."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'claude-repl--register-projectile-project)
               (lambda (&rest _) nil))
              ((symbol-function '+workspace-new) (lambda (_ws) nil))
              ((symbol-function 'claude-repl--setup-worktree-session)
               (lambda (&rest _) nil))
              ((symbol-function 'claude-repl--path-canonical) #'identity))
      (claude-repl--finalize-worktree-workspace
       "/tmp/new-wt" "new-ws" nil nil nil nil nil nil)
      (should (null (claude-repl--ws-get "new-ws" :source-ws-dir))))))

(ert-deftest claude-repl-test-finalize-worktree-workspace-calls-reorder-by-priority ()
  "Finalize invokes reorder-workspace-by-priority after applying properties.
Reorder must run after `apply-workspace-properties' so the new workspace's
`:priority' is already on the plist when the cache is rewritten."
  (claude-repl-test--with-clean-state
    (let ((reorder-called-with :unset))
      (cl-letf (((symbol-function 'claude-repl--register-projectile-project)
                 (lambda (&rest _) nil))
                ((symbol-function '+workspace-new) (lambda (_ws) nil))
                ((symbol-function 'claude-repl--setup-worktree-session)
                 (lambda (&rest _) nil))
                ((symbol-function 'claude-repl--path-canonical) #'identity)
                ((symbol-function 'claude-repl--reorder-workspace-by-priority)
                 (lambda (ws)
                   (setq reorder-called-with
                         (cons ws (claude-repl--ws-get ws :priority))))))
        (claude-repl--finalize-worktree-workspace
         "/tmp/new-wt" "new-ws" nil "p1" nil nil nil nil)
        (should (equal reorder-called-with '("new-ws" . "p1")))))))

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

(ert-deftest claude-repl-test-handle-create-command-passes-base-commit-when-given ()
  "handle-create-command threads a non-empty `base_commit' field through to the
timer callback — letting the workspace-generation flow request HEAD without forking."
  (claude-repl-test--with-clean-state
    (let ((captured-args nil))
      (cl-letf (((symbol-function 'run-with-timer)
                 (lambda (_delay _repeat _fn &rest args)
                   (setq captured-args args))))
        (claude-repl--handle-create-command
         '((type . "create") (name . "DWC/new-ws")
           (git_root . "/fake/root") (base_commit . "HEAD"))
         0)
        ;; captured-args = (git-root name prompt priority fork-session-id base-commit)
        (should (equal (nth 5 captured-args) "HEAD"))))))

(ert-deftest claude-repl-test-handle-create-command-empty-base-commit-passes-nil ()
  "An empty base_commit string is normalized to nil so the downstream default applies."
  (claude-repl-test--with-clean-state
    (let ((captured-args nil))
      (cl-letf (((symbol-function 'run-with-timer)
                 (lambda (_delay _repeat _fn &rest args)
                   (setq captured-args args))))
        (claude-repl--handle-create-command
         '((type . "create") (name . "DWC/new-ws")
           (git_root . "/fake/root") (base_commit . ""))
         0)
        (should (null (nth 5 captured-args)))))))

(ert-deftest claude-repl-test-handle-create-command-missing-base-commit-passes-nil ()
  "An absent base_commit field passes nil so the downstream default applies
(HEAD for forks, `claude-repl-worktree-default-base' otherwise)."
  (claude-repl-test--with-clean-state
    (let ((captured-args nil))
      (cl-letf (((symbol-function 'run-with-timer)
                 (lambda (_delay _repeat _fn &rest args)
                   (setq captured-args args))))
        (claude-repl--handle-create-command
         '((type . "create") (name . "DWC/new-ws") (git_root . "/fake/root"))
         0)
        (should (null (nth 5 captured-args)))))))

(ert-deftest claude-repl-test-create-worktree-from-command-threads-base-commit ()
  "`--create-worktree-from-command' forwards BASE-COMMIT to `--do-create-worktree-workspace'."
  (claude-repl-test--with-clean-state
    (let ((captured-base :unset))
      (cl-letf (((symbol-function 'claude-repl--do-create-worktree-workspace)
                 (lambda (_name _bare _fork _prompt _cb _priority base &rest _)
                   (setq captured-base base))))
        (claude-repl--create-worktree-from-command
         "/tmp/cmd-repo/" "name" "prompt" 5 nil "HEAD")
        (should (equal captured-base "HEAD"))))))

(ert-deftest claude-repl-test-create-worktree-from-command-nil-base-commit-passes-nil ()
  "When BASE-COMMIT is nil, `--do-create-worktree-workspace' receives nil and applies its default."
  (claude-repl-test--with-clean-state
    (let ((captured-base :unset))
      (cl-letf (((symbol-function 'claude-repl--do-create-worktree-workspace)
                 (lambda (_name _bare _fork _prompt _cb _priority base &rest _)
                   (setq captured-base base))))
        (claude-repl--create-worktree-from-command
         "/tmp/cmd-repo/" "name" "prompt" 5 nil nil)
        (should (null captured-base))))))

;;;; ---- Tests: workspace-generation prompt construction ----

(ert-deftest claude-repl-test-workspace-generation-prompt-includes-raw-and-prefixed ()
  "The generated prompt contains both the raw description (for naming) and the
prefixed prompt (for the new workspace's session)."
  (let* ((raw "fix login flow")
         (prefixed (concat claude-repl--autonomous-prompt-prefix raw))
         (out (claude-repl--workspace-generation-prompt
               raw prefixed "/tmp/repo/" "HEAD" nil)))
    (should (string-match-p (regexp-quote raw) out))
    (should (string-match-p (regexp-quote prefixed) out))))

(ert-deftest claude-repl-test-workspace-generation-prompt-emits-required-fields ()
  "The prompt instructs the model to emit `type', `git_root', and `base_commit'
with the exact values supplied."
  (let ((out (claude-repl--workspace-generation-prompt
              "raw" "prefixed" "/tmp/repo/" "origin/master" nil)))
    (should (string-match-p "\"type\": \"create\"" out))
    (should (string-match-p "\"git_root\": \"/tmp/repo/\"" out))
    (should (string-match-p "\"base_commit\": \"origin/master\"" out))))

(ert-deftest claude-repl-test-workspace-generation-prompt-omits-fork-from-when-nil ()
  "When FORK-FROM is nil, no `fork_from' line is emitted in the prompt."
  (let ((out (claude-repl--workspace-generation-prompt
              "raw" "prefixed" "/tmp/repo/" "HEAD" nil)))
    (should-not (string-match-p "fork_from" out))))

(ert-deftest claude-repl-test-workspace-generation-prompt-includes-fork-from-when-set ()
  "When FORK-FROM is set, the prompt instructs the model to emit a matching
`fork_from' field."
  (let ((out (claude-repl--workspace-generation-prompt
              "raw" "prefixed" "/tmp/repo/" "HEAD" "source-ws")))
    (should (string-match-p "\"fork_from\": \"source-ws\"" out))))

(ert-deftest claude-repl-test-workspace-generation-prompt-tells-model-not-to-ask ()
  "The prompt instructs the model not to request permission — in headless
`-p' mode there is no one to approve, and the spawn previously died
emitting only the permission question."
  (let ((out (claude-repl--workspace-generation-prompt
              "raw" "prefixed" "/tmp/repo/" "HEAD" nil)))
    (should (string-match-p "[Dd]o NOT ask for permission" out))))

(ert-deftest claude-repl-test-workspace-generation-prompt-isolates-inner-prompt ()
  "The headless prompt explicitly tells the model the inner string is the
USER PROMPT for a separate spawned agent and is NOT instructions for the
headless model itself.  Without this, the headless model can read a
suffix like `invoke /workspace-merge' inside the inner prompt and run it
itself instead of just emitting it verbatim into the JSON."
  (let ((out (claude-repl--workspace-generation-prompt
              "raw" "prefixed" "/tmp/repo/" "HEAD" nil)))
    (should (string-match-p "NOT instructions for you" out))
    (should (string-match-p "verbatim" out))))

(ert-deftest claude-repl-test-workspace-generation-prompt-requires-array-top-level ()
  "The prompt explicitly tells the model the JSON top-level must be an array
even for a single workspace.  Previously the model emitted a bare object
`{...}' and the elisp parser crashed with `listp, (type . \"create\")'
when `dolist' iterated the alist's cons cells."
  (let ((out (claude-repl--workspace-generation-prompt
              "raw" "prefixed" "/tmp/repo/" "HEAD" nil)))
    (should (string-match-p "MUST be an array" out))))

;;;; ---- Tests: workspace-commands JSON normalization ----

(ert-deftest claude-repl-test-normalize-workspace-commands-vector-becomes-list ()
  "A JSON array (parsed as vector) is normalized to a list."
  (let* ((parsed (vector '((type . "create") (name . "a"))
                         '((type . "create") (name . "b"))))
         (out (claude-repl--normalize-workspace-commands parsed)))
    (should (listp out))
    (should (equal (length out) 2))
    (should (equal (alist-get 'name (car out)) "a"))
    (should (equal (alist-get 'name (cadr out)) "b"))))

(ert-deftest claude-repl-test-normalize-workspace-commands-bare-object-wrapped ()
  "A bare JSON object (parsed as alist) is wrapped into a one-element list.
Without this, `dolist' iterates the alist's cons cells and dispatch
crashes with `Wrong type argument: listp, (type . \"create\")'."
  (let* ((parsed '((type . "create") (name . "solo") (git_root . "/g")))
         (out (claude-repl--normalize-workspace-commands parsed)))
    (should (equal (length out) 1))
    (should (equal (alist-get 'name (car out)) "solo"))
    (should (equal (alist-get 'type (car out)) "create"))))

(ert-deftest claude-repl-test-normalize-workspace-commands-empty-vector-empty-list ()
  "An empty JSON array produces an empty list (no commands to dispatch)."
  (should (equal (claude-repl--normalize-workspace-commands (vector)) nil)))

(ert-deftest claude-repl-test-normalize-workspace-commands-nil-empty-list ()
  "Malformed/nil input produces an empty list — caller skips dispatch."
  (should (equal (claude-repl--normalize-workspace-commands nil) nil)))

(ert-deftest claude-repl-test-normalize-workspace-commands-scalar-empty-list ()
  "A scalar JSON value (string/number) produces an empty list."
  (should (equal (claude-repl--normalize-workspace-commands "oops") nil))
  (should (equal (claude-repl--normalize-workspace-commands 42) nil)))

(ert-deftest claude-repl-test-spawn-workspace-generation-appends-extra-args ()
  "The spawn command list includes `claude-repl-workspace-generation-extra-args'
after the base `-p --model X' args.  Without these, the headless model
hits the permission prompt and dies emitting only its question."
  (let ((captured-cmd nil)
        (claude-repl-workspace-generation-extra-args
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
              ((symbol-function 'claude-repl--log) (lambda (&rest _) nil)))
      (claude-repl--spawn-workspace-generation "raw" "prefixed" "/tmp/repo/" "HEAD" nil)
      (should (member "--permission-mode" captured-cmd))
      (should (member "bypassPermissions" captured-cmd))
      ;; Sanity: extra-args come after the base `-p --model MODEL'.
      (should (equal (cl-subseq captured-cmd 0 4)
                     (list claude-repl-workspace-generation-program
                           "-p" "--model" claude-repl-workspace-generation-model))))))

(ert-deftest claude-repl-test-spawn-workspace-generation-empty-extra-args ()
  "When `claude-repl-workspace-generation-extra-args' is nil, no extra args appear."
  (let ((captured-cmd nil)
        (claude-repl-workspace-generation-extra-args nil))
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest plist)
                 (setq captured-cmd (plist-get plist :command))
                 (make-marker)))
              ((symbol-function 'process-send-string) (lambda (&rest _) nil))
              ((symbol-function 'process-send-eof) (lambda (&rest _) nil))
              ((symbol-function 'claude-repl--log) (lambda (&rest _) nil)))
      (claude-repl--spawn-workspace-generation "raw" "prefixed" "/tmp/repo/" "HEAD" nil)
      (should (equal captured-cmd
                     (list claude-repl-workspace-generation-program
                           "-p" "--model" claude-repl-workspace-generation-model))))))

(ert-deftest claude-repl-test-spawn-workspace-generation-binds-temporary-default-directory ()
  "Spawn must invoke `make-process' with `default-directory' rebound to
`temporary-file-directory'.  Without this, the headless claude inherits
the caller's cwd, its hooks fire with that cwd, and the sentinel watcher
misattributes them to whichever workspace owns that project-dir — flipping
:claude-state to :done."
  (let ((captured-cwd nil))
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest _plist)
                 (setq captured-cwd default-directory)
                 (make-marker)))
              ((symbol-function 'process-send-string) (lambda (&rest _) nil))
              ((symbol-function 'process-send-eof) (lambda (&rest _) nil))
              ((symbol-function 'claude-repl--log) (lambda (&rest _) nil)))
      (claude-repl--spawn-workspace-generation "raw" "prefixed" "/tmp/repo/" "HEAD" nil)
      (should (equal (file-name-as-directory captured-cwd)
                     (file-name-as-directory temporary-file-directory))))))

;;;; ---- Tests: workspace-generation logging helpers ----

(ert-deftest claude-repl-test-workspace-generation-id-returns-non-empty-hex ()
  "The correlation-ID generator returns a non-empty hex string."
  (let ((id (claude-repl--workspace-generation-id)))
    (should (stringp id))
    (should (> (length id) 0))
    (should (string-match-p "\\`[0-9a-f]+\\'" id))))

(ert-deftest claude-repl-test-workspace-generation-truncate-leaves-short-strings ()
  "Strings within the cap are returned unchanged."
  (should (equal (claude-repl--workspace-generation-truncate "hello" 100) "hello")))

(ert-deftest claude-repl-test-workspace-generation-truncate-truncates-long-strings ()
  "Strings beyond the cap get a `...[truncated]' suffix."
  (let ((out (claude-repl--workspace-generation-truncate "0123456789" 4)))
    (should (string-prefix-p "0123" out))
    (should (string-suffix-p "...[truncated]" out))))

(ert-deftest claude-repl-test-workspace-generation-truncate-nil-cap-passes-through ()
  "A nil cap disables truncation entirely."
  (let ((s (make-string 10000 ?x)))
    (should (equal (claude-repl--workspace-generation-truncate s nil) s))))

(ert-deftest claude-repl-test-workspace-generation-truncate-nil-input-yields-empty ()
  "A nil input is treated as the empty string."
  (should (equal (claude-repl--workspace-generation-truncate nil 100) "")))

(ert-deftest claude-repl-test-workspace-generation-finalize-logs-correlation-id ()
  "Finalize includes the correlation ID in its log line so spawns can be matched."
  (let ((logged nil))
    (cl-letf (((symbol-function 'claude-repl--log)
               (lambda (_ws fmt &rest args)
                 (setq logged (apply #'format fmt args))))
              ((symbol-function 'message) (lambda (&rest _) nil)))
      (claude-repl--workspace-generation-finalize "abc123" 0 "finished\n" "ok")
      (should (string-match-p "\\[abc123\\]" logged)))))

(ert-deftest claude-repl-test-workspace-generation-finalize-logs-stdout-snippet ()
  "Finalize includes the stdout content (truncated) in the log line — so
failed spawns can be debugged without the buffer."
  (let ((logged nil))
    (cl-letf (((symbol-function 'claude-repl--log)
               (lambda (_ws fmt &rest args)
                 (setq logged (apply #'format fmt args))))
              ((symbol-function 'message) (lambda (&rest _) nil)))
      (claude-repl--workspace-generation-finalize "id" 1 "exit" "model-error-text")
      (should (string-match-p "model-error-text" logged)))))

(ert-deftest claude-repl-test-workspace-generation-finalize-truncates-long-stdout ()
  "Finalize honors the stdout cap so the log line stays bounded."
  (let ((logged nil)
        (claude-repl-workspace-generation-stdout-log-cap 8))
    (cl-letf (((symbol-function 'claude-repl--log)
               (lambda (_ws fmt &rest args)
                 (setq logged (apply #'format fmt args))))
              ((symbol-function 'message) (lambda (&rest _) nil)))
      (claude-repl--workspace-generation-finalize "id" 1 "exit" "0123456789abcdef")
      (should (string-match-p "0123" logged))
      (should (string-match-p "truncated" logged))
      (should-not (string-match-p "abcdef" logged)))))

(ert-deftest claude-repl-test-workspace-generation-finalize-no-message-on-success ()
  "On status=0, finalize does not surface a user-facing failure message."
  (let ((messaged nil))
    (cl-letf (((symbol-function 'claude-repl--log) (lambda (&rest _) nil))
              ((symbol-function 'message)
               (lambda (&rest args) (setq messaged args))))
      (claude-repl--workspace-generation-finalize "id" 0 "finished" "ok")
      (should-not messaged))))

(ert-deftest claude-repl-test-workspace-generation-finalize-message-includes-id ()
  "On non-zero status, the user-facing message includes the correlation ID
so the user can grep the log for the matching spawn."
  (let ((messaged nil))
    (cl-letf (((symbol-function 'claude-repl--log) (lambda (&rest _) nil))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq messaged (apply #'format fmt args)))))
      (claude-repl--workspace-generation-finalize "id-xyz" 2 "exit" "")
      (should (string-match-p "id-xyz" messaged)))))

(ert-deftest claude-repl-test-workspace-generation-finalize-non-numeric-status-is-failure ()
  "A non-numeric status (e.g. nil from a malformed signal) is treated as failure."
  (let ((messaged nil))
    (cl-letf (((symbol-function 'claude-repl--log) (lambda (&rest _) nil))
              ((symbol-function 'message)
               (lambda (&rest args) (setq messaged args))))
      (claude-repl--workspace-generation-finalize "id" nil "killed" "")
      (should messaged))))

;;;; ---- Tests: create-worktree-workspace (interactive) ----

(ert-deftest claude-repl-test-resolve-worktree-base-head ()
  "`head' resolves to HEAD."
  (should (equal (claude-repl--resolve-worktree-base 'head) "HEAD")))

(ert-deftest claude-repl-test-resolve-worktree-base-master ()
  "`master' resolves to local `master' (not `origin/master').
The fetch happens at worktree-creation time as a freshness gesture, but
the new branch is rooted in local master so local-only commits carry over."
  (should (equal (claude-repl--resolve-worktree-base 'master) "master")))

(ert-deftest claude-repl-test-resolve-worktree-base-unknown-errors ()
  "An unknown base symbol signals a `user-error' rather than silently
passing through."
  (should-error (claude-repl--resolve-worktree-base 'bogus)
                :type 'user-error))

(ert-deftest claude-repl-test-create-worktree-workspace-head-base ()
  "BASE = `head' branches off HEAD (the current worktree)."
  (claude-repl-test--with-clean-state
    (let ((captured-base nil))
      (cl-letf (((symbol-function 'claude-repl--resolve-current-git-root)
                 (lambda () "/tmp/repo/"))
                ((symbol-function 'read-string)
                 (lambda (&rest _) "do the thing"))
                ((symbol-function 'claude-repl--spawn-workspace-generation)
                 (lambda (_raw _prefixed _git-root base _fork-from)
                   (setq captured-base base))))
        (claude-repl-create-worktree-workspace 'head)
        (should (equal captured-base "HEAD"))))))

(ert-deftest claude-repl-test-create-worktree-workspace-master-base ()
  "BASE = `master' branches off LOCAL `master' (not `origin/master').
The downstream worktree-creation flow still fetches `origin master' first
as a freshness gesture, but the new branch is rooted in local master."
  (claude-repl-test--with-clean-state
    (let ((captured-base nil))
      (cl-letf (((symbol-function 'claude-repl--resolve-current-git-root)
                 (lambda () "/tmp/repo/"))
                ((symbol-function 'read-string)
                 (lambda (&rest _) "do the thing"))
                ((symbol-function 'claude-repl--spawn-workspace-generation)
                 (lambda (_raw _prefixed _git-root base _fork-from)
                   (setq captured-base base))))
        (claude-repl-create-worktree-workspace 'master)
        (should (equal captured-base "master"))))))

(ert-deftest claude-repl-test-create-worktree-workspace-passes-no-fork-from ()
  "Plain create (non-fork) passes FORK-FROM = nil."
  (claude-repl-test--with-clean-state
    (let ((captured-fork-from :unset))
      (cl-letf (((symbol-function 'claude-repl--resolve-current-git-root)
                 (lambda () "/tmp/repo/"))
                ((symbol-function 'read-string)
                 (lambda (&rest _) "do the thing"))
                ((symbol-function 'claude-repl--spawn-workspace-generation)
                 (lambda (_raw _prefixed _git-root _base fork-from)
                   (setq captured-fork-from fork-from))))
        (claude-repl-create-worktree-workspace 'head)
        (should (null captured-fork-from))))))

(ert-deftest claude-repl-test-create-worktree-workspace-from-origin-master-delegates-with-master-symbol ()
  "`SPC TAB N' wrapper delegates to the main command with BASE = `master'."
  (let ((captured-base :unset)
        (captured-source :unset))
    (cl-letf (((symbol-function 'claude-repl-create-worktree-workspace)
               (lambda (base &optional source-ws)
                 (setq captured-base base)
                 (setq captured-source source-ws))))
      (claude-repl-create-worktree-workspace-from-origin-master)
      (should (eq captured-base 'master))
      (should (null captured-source)))))

(ert-deftest claude-repl-test-create-worktree-workspace-from-origin-master-forwards-source-ws ()
  "`SPC TAB N' wrapper forwards SOURCE-WS to the main command."
  (let ((captured-source :unset))
    (cl-letf (((symbol-function 'claude-repl-create-worktree-workspace)
               (lambda (_base &optional source-ws)
                 (setq captured-source source-ws))))
      (claude-repl-create-worktree-workspace-from-origin-master "other-ws")
      (should (equal captured-source "other-ws")))))

(ert-deftest claude-repl-test-create-worktree-workspace-source-ws-passes-git-root ()
  "When SOURCE-WS is given, its :project-dir is threaded through as git-root."
  (claude-repl-test--with-clean-state
    (let ((captured-git-root :unset))
      (cl-letf (((symbol-function 'claude-repl--ws-dir)
                 (lambda (ws)
                   (if (equal ws "source-ws") "/tmp/source-repo/"
                     (error "unexpected ws: %s" ws))))
                ((symbol-function 'read-string)
                 (lambda (&rest _) "do the thing"))
                ((symbol-function 'claude-repl--spawn-workspace-generation)
                 (lambda (_raw _prefixed git-root _base _fork-from)
                   (setq captured-git-root git-root))))
        (claude-repl-create-worktree-workspace 'head "source-ws")
        (should (equal captured-git-root "/tmp/source-repo/"))))))

(ert-deftest claude-repl-test-create-worktree-workspace-no-source-ws-resolves-ambient-git-root ()
  "When SOURCE-WS is nil and current ws has no :project-dir, falls back to `resolve-current-git-root'."
  (claude-repl-test--with-clean-state
    (let ((captured-git-root :unset))
      (cl-letf (((symbol-function '+workspace-current-name)
                 (lambda () "ambient-ws"))
                ((symbol-function 'claude-repl--ws-dir)
                 (lambda (_ws) nil))
                ((symbol-function 'claude-repl--resolve-current-git-root)
                 (lambda () "/tmp/ambient-repo/"))
                ((symbol-function 'read-string)
                 (lambda (&rest _) "do the thing"))
                ((symbol-function 'claude-repl--spawn-workspace-generation)
                 (lambda (_raw _prefixed git-root _base _fork-from)
                   (setq captured-git-root git-root))))
        (claude-repl-create-worktree-workspace 'head nil)
        (should (equal captured-git-root "/tmp/ambient-repo/"))))))

(ert-deftest claude-repl-test-create-worktree-workspace-prefixes-preemptive-prompt ()
  "The preemptive prompt is prefixed with the autonomous instruction before being
handed to the spawn helper as PREFIXED-PROMPT."
  (claude-repl-test--with-clean-state
    (let ((captured-prefixed nil))
      (cl-letf (((symbol-function 'claude-repl--resolve-current-git-root)
                 (lambda () "/tmp/repo/"))
                ((symbol-function 'read-string)
                 (lambda (&rest _) "do the thing"))
                ((symbol-function 'claude-repl--spawn-workspace-generation)
                 (lambda (_raw prefixed _git-root _base _fork-from)
                   (setq captured-prefixed prefixed))))
        (claude-repl-create-worktree-workspace 'head)
        (should (string-prefix-p claude-repl--autonomous-prompt-prefix captured-prefixed))
        (should (string-suffix-p "do the thing" captured-prefixed))))))

(ert-deftest claude-repl-test-create-worktree-workspace-passes-raw-prompt-unprefixed ()
  "RAW-PROMPT given to the spawn helper is the user's original input,
unprefixed — the prefix is reserved for the new ws session, not for naming."
  (claude-repl-test--with-clean-state
    (let ((captured-raw nil))
      (cl-letf (((symbol-function 'claude-repl--resolve-current-git-root)
                 (lambda () "/tmp/repo/"))
                ((symbol-function 'read-string)
                 (lambda (&rest _) "do the thing"))
                ((symbol-function 'claude-repl--spawn-workspace-generation)
                 (lambda (raw _prefixed _git-root _base _fork-from)
                   (setq captured-raw raw))))
        (claude-repl-create-worktree-workspace 'head)
        (should (equal captured-raw "do the thing"))
        (should-not (string-prefix-p claude-repl--autonomous-prompt-prefix captured-raw))))))

(ert-deftest claude-repl-test-create-worktree-workspace-blank-prompt-errors ()
  "An empty preemptive prompt signals user-error — no fallback path."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'claude-repl--resolve-current-git-root)
               (lambda () "/tmp/repo/"))
              ((symbol-function 'read-string)
               (lambda (&rest _) "   "))
              ((symbol-function 'claude-repl--spawn-workspace-generation)
               (lambda (&rest _) (error "should not be called"))))
      (should-error (claude-repl-create-worktree-workspace 'head)
                    :type 'user-error))))

(ert-deftest claude-repl-test-fork-worktree-workspace-prefixes-preemptive-prompt ()
  "Fork's preemptive prompt is prefixed with the autonomous instruction."
  (claude-repl-test--with-clean-state
    (let ((inst (make-claude-repl-instantiation :session-id "sess-abc"))
          (captured-prefixed nil))
      (cl-letf (((symbol-function 'claude-repl--active-inst)
                 (lambda (_ws) inst))
                ((symbol-function '+workspace-current-name)
                 (lambda () "test-ws"))
                ((symbol-function 'claude-repl--resolve-current-git-root)
                 (lambda () "/tmp/repo/"))
                ((symbol-function 'read-string)
                 (lambda (&rest _) "do the thing"))
                ((symbol-function 'claude-repl--spawn-workspace-generation)
                 (lambda (_raw prefixed _git-root _base _fork-from)
                   (setq captured-prefixed prefixed))))
        (claude-repl-fork-worktree-workspace nil)
        (should (string-prefix-p claude-repl--autonomous-prompt-prefix captured-prefixed))
        (should (string-suffix-p "do the thing" captured-prefixed))))))

(ert-deftest claude-repl-test-fork-worktree-workspace-blank-prompt-errors ()
  "An empty preemptive prompt to fork signals user-error."
  (claude-repl-test--with-clean-state
    (let ((inst (make-claude-repl-instantiation :session-id "sess-abc")))
      (cl-letf (((symbol-function 'claude-repl--active-inst)
                 (lambda (_ws) inst))
                ((symbol-function '+workspace-current-name)
                 (lambda () "test-ws"))
                ((symbol-function 'claude-repl--resolve-current-git-root)
                 (lambda () "/tmp/repo/"))
                ((symbol-function 'read-string)
                 (lambda (&rest _) ""))
                ((symbol-function 'claude-repl--spawn-workspace-generation)
                 (lambda (&rest _) (error "should not be called"))))
        (should-error (claude-repl-fork-worktree-workspace nil)
                      :type 'user-error)))))

;;;; ---- Tests: do-create-worktree-workspace base-commit + fetch ----

(ert-deftest claude-repl-test-do-create-base-commit-default-no-fork-fetches-origin-master ()
  "When BASE-COMMIT is nil and no FORK-SESSION-ID, the default is local
`master' AND a `git fetch origin master' is still scheduled.
Preserves the programmatic worktree-creation path used by
`create-worktree-from-command' (Slack/command-file workspace creation):
local master keeps any local-only commits, while the fetch keeps
`origin/master' fresh as a side benefit."
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
      ;; Fetch is still scheduled for origin master even though the branch
      ;; will be rooted in local master.
      (should (equal add-args '("fetch" "origin" "master"))))))

(ert-deftest claude-repl-test-do-create-local-master-base-passed-to-worktree-add ()
  "When BASE-COMMIT is the local trunk (e.g. \"master\"), the ref passed
to `git worktree add' is the LOCAL ref (\"master\"), not \"origin/master\".
The fetch still runs as a freshness gesture but does not change what the
new branch is rooted in."
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
              ;; Stub out the async-fetch so the callback fires immediately
              ;; and we can capture what got passed to worktree-add.
              ((symbol-function 'claude-repl--async-git)
               (lambda (_label _root _args cb) (funcall cb t "ok")))
              ((symbol-function 'claude-repl--async-worktree-add)
               (lambda (_root _branch _path base &rest _) (setq add-base base))))
      (claude-repl--do-create-worktree-workspace "name" nil nil nil nil nil "master")
      (should (equal add-base "master")))))

(ert-deftest claude-repl-test-do-create-local-master-base-fetches-origin-counterpart ()
  "When BASE-COMMIT equals `claude-repl-master-branch-name', a fetch of the
corresponding origin ref is scheduled even though the base is local.
This is the freshness-gesture path: the user picked local master as the
branching point, so the worktree-add proper uses local master, but
`origin/master' is updated alongside since fetching costs ~nothing."
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
               (lambda (_label _root args _cb) (setq fetch-args args)))
              ((symbol-function 'claude-repl--async-worktree-add)
               (lambda (&rest _) nil)))
      (claude-repl--do-create-worktree-workspace "name" nil nil nil nil nil "master")
      (should (equal fetch-args '("fetch" "origin" "master"))))))

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

;;;; ---- Tests: tag-branch-name ----

(ert-deftest claude-repl-test-tag-branch-name-default-suffix ()
  "Default suffix `-tag' produces BRANCH+'-tag'."
  (let ((claude-repl-worktree-tag-branch-suffix "-tag"))
    (should (equal (claude-repl--tag-branch-name "DC/feature") "DC/feature-tag"))))

(ert-deftest claude-repl-test-tag-branch-name-nil-suffix ()
  "Nil suffix means tag branches are disabled — returns nil."
  (let ((claude-repl-worktree-tag-branch-suffix nil))
    (should (null (claude-repl--tag-branch-name "any")))))

(ert-deftest claude-repl-test-tag-branch-name-empty-suffix ()
  "Empty suffix is treated as disabled — returns nil."
  (let ((claude-repl-worktree-tag-branch-suffix ""))
    (should (null (claude-repl--tag-branch-name "any")))))

;;;; ---- Tests: create-tag-branch ----

(ert-deftest claude-repl-test-create-tag-branch-creates-at-base-commit ()
  "create-tag-branch creates BRANCH+suffix pointing at BASE-COMMIT."
  (claude-repl-test--with-temp-git-repo repo
    (let ((sha (claude-repl-test--git-commit repo "initial" "content"))
          (claude-repl-worktree-tag-branch-suffix "-tag"))
      ;; Advance HEAD so we can verify the tag points at the OLD commit, not HEAD.
      (claude-repl-test--git-commit repo "second" "more")
      (claude-repl--create-tag-branch repo "feature" sha)
      (should (claude-repl--git-branch-exists-p repo "feature-tag"))
      (let ((tag-sha (string-trim
                      (shell-command-to-string
                       (format "git -C %s rev-parse feature-tag"
                               (shell-quote-argument repo))))))
        (should (equal tag-sha sha))))))

(ert-deftest claude-repl-test-create-tag-branch-disabled-no-op ()
  "When suffix is nil, no branch is created."
  (claude-repl-test--with-temp-git-repo repo
    (claude-repl-test--git-commit repo "initial" "content")
    (let ((claude-repl-worktree-tag-branch-suffix nil))
      (claude-repl--create-tag-branch repo "feature" "HEAD")
      (should-not (claude-repl--git-branch-exists-p repo "feature-tag"))
      (should-not (claude-repl--git-branch-exists-p repo "feature")))))

(ert-deftest claude-repl-test-create-tag-branch-signals-on-failure ()
  "create-tag-branch signals an error when git branch fails (e.g. branch exists)."
  (claude-repl-test--with-temp-git-repo repo
    (claude-repl-test--git-commit repo "initial" "content")
    ;; Pre-create the tag branch so the second `git branch' attempt fails.
    (claude-repl-test--git-checkout repo "feature-tag" t)
    (claude-repl-test--git-checkout repo "master")
    (let ((claude-repl-worktree-tag-branch-suffix "-tag"))
      (should-error (claude-repl--create-tag-branch repo "feature" "HEAD")))))

;;;; ---- Tests: async-worktree-add tag-branch integration ----

(ert-deftest claude-repl-test-async-worktree-add-creates-tag-branch-on-success ()
  "After successful worktree add, the tag branch is created."
  (let ((captured-tag-args nil))
    (cl-letf (((symbol-function 'claude-repl--async-git)
               ;; Simulate immediate success: invoke callback with ok=t.
               (lambda (_label _root _args cb) (funcall cb t "ok")))
              ((symbol-function 'claude-repl--worktree-add-callback)
               (lambda (&rest _args) nil))
              ((symbol-function 'claude-repl--create-tag-branch)
               (lambda (git-root branch-name base-commit)
                 (setq captured-tag-args (list git-root branch-name base-commit)))))
      (claude-repl--async-worktree-add
       "/git-root" "my-branch" "/path" "HEAD"
       nil "dirname" nil nil nil nil)
      (should (equal captured-tag-args '("/git-root" "my-branch" "HEAD"))))))

(ert-deftest claude-repl-test-async-worktree-add-skips-tag-branch-on-failure ()
  "On worktree add failure, the tag branch is NOT created."
  (let ((tag-called nil))
    (cl-letf (((symbol-function 'claude-repl--async-git)
               ;; Simulate failure: invoke callback with ok=nil.
               (lambda (_label _root _args cb) (funcall cb nil "git error")))
              ((symbol-function 'claude-repl--worktree-add-callback)
               (lambda (&rest _args) nil))
              ((symbol-function 'claude-repl--create-tag-branch)
               (lambda (&rest _args) (setq tag-called t))))
      (claude-repl--async-worktree-add
       "/git-root" "my-branch" "/path" "HEAD"
       nil "dirname" nil nil nil nil)
      (should-not tag-called))))

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
               ((symbol-function 'claude-repl--show-and-refresh-magit-status)
                (lambda (_dir) (push 'magit call-order))))
      (claude-repl--workspace-merge-do "other-ws")
      (should (equal (nreverse call-order) '(reload magit))))))

(ert-deftest claude-repl-test-workspace-merge-do-magit-receives-project-dir ()
  "workspace-merge-do passes the current workspace's project directory to magit helper."
  (let ((magit-dir nil))
    (cl-letf* (((symbol-function '+workspace-current-name) (lambda () "current"))
               ((symbol-function 'claude-repl--workspace-branch) (lambda (_ws) "branch-x"))
               ((symbol-function 'claude-repl--ws-dir) (lambda (_ws) "/tmp/fake"))
               ((symbol-function 'claude-repl--git-branch-exists-p) (lambda (_dir _br) t))
               ((symbol-function 'claude-repl--cherry-pick-base) (lambda (_dir _br) "abc123"))
               ((symbol-function 'claude-repl--cherry-pick-commits) (lambda (_dir _ws _base _br) nil))
               ((symbol-function 'claude-repl--finish-workspace) (lambda (_ws) nil))
               ((symbol-function 'load-file) #'ignore)
               ((symbol-function 'claude-repl--show-and-refresh-magit-status)
                (lambda (dir) (setq magit-dir dir))))
      (claude-repl--workspace-merge-do "other-ws")
      (should (equal magit-dir "/tmp/fake")))))

;;;; ---- Tests: tag-merge-completion ----

(ert-deftest claude-repl-test-tag-merge-completion-creates-correct-tag ()
  "tag-merge-completion runs `git tag -f merge/<source-ws> HEAD' in
the project root."
  (let ((captured-args nil))
    (cl-letf (((symbol-function 'claude-repl--git-exit-code)
               (lambda (root &rest args)
                 (setq captured-args (cons root args))
                 0)))
      (claude-repl--tag-merge-completion "/tmp/repo" "feat-foo")
      (should (equal captured-args
                     (list "/tmp/repo" "tag" "-f" "merge/feat-foo" "HEAD"))))))

(ert-deftest claude-repl-test-tag-merge-completion-uses-force ()
  "tag-merge-completion passes `-f' so re-running the merge for the
same workspace updates an existing tag rather than erroring."
  (let ((captured-args nil))
    (cl-letf (((symbol-function 'claude-repl--git-exit-code)
               (lambda (_root &rest args) (setq captured-args args) 0)))
      (claude-repl--tag-merge-completion "/tmp/repo" "feat-foo")
      (should (member "-f" captured-args)))))

(ert-deftest claude-repl-test-tag-merge-completion-does-not-propagate-error ()
  "A non-zero git tag exit code must not propagate (the cherry-pick
already succeeded; a tag-write failure shouldn't undo that)."
  (cl-letf (((symbol-function 'claude-repl--git-exit-code)
             (lambda (_root &rest _args) 128)))
    ;; Should not error.
    (claude-repl--tag-merge-completion "/tmp/repo" "feat-foo")
    (should t)))

;;;; ---- Tests: workspace-merge-do tags after cherry-pick ----

(ert-deftest claude-repl-test-workspace-merge-do-tags-after-cherry-pick ()
  "workspace-merge-do calls tag-merge-completion after a successful
cherry-pick, with the project-root and source workspace name."
  (let ((tagged nil))
    (cl-letf* (((symbol-function '+workspace-current-name) (lambda () "current"))
               ((symbol-function 'claude-repl--workspace-branch) (lambda (_ws) "branch-x"))
               ((symbol-function 'claude-repl--ws-dir) (lambda (_ws) "/tmp/fake"))
               ((symbol-function 'claude-repl--git-branch-exists-p) (lambda (_dir _br) t))
               ((symbol-function 'claude-repl--cherry-pick-base) (lambda (_dir _br) "abc123"))
               ((symbol-function 'claude-repl--cherry-pick-commits) (lambda (_dir _ws _base _br) nil))
               ((symbol-function 'claude-repl--finish-workspace) (lambda (_ws) nil))
               ((symbol-function 'load-file) #'ignore)
               ((symbol-function 'claude-repl--show-and-refresh-magit-status) #'ignore)
               ((symbol-function 'claude-repl--tag-merge-completion)
                (lambda (root ws) (setq tagged (cons root ws)))))
      (claude-repl--workspace-merge-do "other-ws")
      (should (equal tagged (cons "/tmp/fake" "other-ws"))))))

(ert-deftest claude-repl-test-workspace-merge-do-skips-tag-on-cherry-pick-error ()
  "When cherry-pick-commits signals user-error (e.g., conflict or
all-incorporated), tag-merge-completion is NOT invoked."
  (let ((tagged nil))
    (cl-letf* (((symbol-function '+workspace-current-name) (lambda () "current"))
               ((symbol-function 'claude-repl--workspace-branch) (lambda (_ws) "branch-x"))
               ((symbol-function 'claude-repl--ws-dir) (lambda (_ws) "/tmp/fake"))
               ((symbol-function 'claude-repl--git-branch-exists-p) (lambda (_dir _br) t))
               ((symbol-function 'claude-repl--cherry-pick-base) (lambda (_dir _br) "abc123"))
               ((symbol-function 'claude-repl--cherry-pick-commits)
                (lambda (_dir _ws _base _br)
                  (user-error "All commits already incorporated")))
               ((symbol-function 'claude-repl--finish-workspace) (lambda (_ws) nil))
               ((symbol-function 'load-file) #'ignore)
               ((symbol-function 'claude-repl--show-and-refresh-magit-status) #'ignore)
               ((symbol-function 'claude-repl--tag-merge-completion)
                (lambda (_root _ws) (setq tagged t))))
      (should-error (claude-repl--workspace-merge-do "other-ws") :type 'user-error)
      (should-not tagged))))

;;;; ---- Tests: show-and-refresh-magit-status ----

(ert-deftest claude-repl-test-show-and-refresh-magit-opens-status-for-root ()
  "show-and-refresh-magit-status calls magit-status with the project root."
  (let ((magit-dir nil))
    (cl-letf (((symbol-function 'magit-status)
               (lambda (dir) (setq magit-dir dir)))
              ((symbol-function 'window-list) (lambda (&rest _) nil))
              ((symbol-function 'magit-refresh) #'ignore))
      (claude-repl--show-and-refresh-magit-status "/tmp/repo")
      (should (equal magit-dir "/tmp/repo")))))

(ert-deftest claude-repl-test-show-and-refresh-magit-selects-matching-window ()
  "When a magit-status window for the project exists, it is selected."
  (let ((selected-win nil)
        (fake-win 'fake-magit-window)
        (fake-buf (generate-new-buffer "*fake-magit*")))
    (unwind-protect
        (progn
          (with-current-buffer fake-buf
            (setq-local default-directory "/tmp/repo/")
            (setq major-mode 'magit-status-mode))
          (cl-letf (((symbol-function 'magit-status) #'ignore)
                    ((symbol-function 'window-list)
                     (lambda (&rest _) (list fake-win)))
                    ((symbol-function 'window-buffer)
                     (lambda (w) (when (eq w fake-win) fake-buf)))
                    ((symbol-function 'select-window)
                     (lambda (w) (setq selected-win w)))
                    ((symbol-function 'magit-refresh) #'ignore)
                    ((symbol-function 'claude-repl--path-canonical)
                     (lambda (p) (directory-file-name (or p "")))))
            (claude-repl--show-and-refresh-magit-status "/tmp/repo")
            (should (eq selected-win fake-win))))
      (kill-buffer fake-buf))))

(ert-deftest claude-repl-test-show-and-refresh-magit-refreshes-selected-buffer ()
  "After selecting the magit window, magit-refresh is called."
  (let ((refreshed nil)
        (fake-win 'fake-magit-window)
        (fake-buf (generate-new-buffer "*fake-magit*")))
    (unwind-protect
        (progn
          (with-current-buffer fake-buf
            (setq-local default-directory "/tmp/repo/")
            (setq major-mode 'magit-status-mode))
          (cl-letf (((symbol-function 'magit-status) #'ignore)
                    ((symbol-function 'window-list)
                     (lambda (&rest _) (list fake-win)))
                    ((symbol-function 'window-buffer)
                     (lambda (w) (when (eq w fake-win) fake-buf)))
                    ((symbol-function 'select-window) #'ignore)
                    ((symbol-function 'magit-refresh)
                     (lambda () (setq refreshed t)))
                    ((symbol-function 'claude-repl--path-canonical)
                     (lambda (p) (directory-file-name (or p "")))))
            (claude-repl--show-and-refresh-magit-status "/tmp/repo")
            (should refreshed)))
      (kill-buffer fake-buf))))

(ert-deftest claude-repl-test-show-and-refresh-magit-skips-non-matching-windows ()
  "Windows whose magit buffer points at a different repo are not selected."
  (let ((selected-win nil)
        (refreshed nil)
        (fake-win 'fake-magit-window)
        (fake-buf (generate-new-buffer "*fake-magit-other*")))
    (unwind-protect
        (progn
          (with-current-buffer fake-buf
            (setq-local default-directory "/tmp/other-repo/")
            (setq major-mode 'magit-status-mode))
          (cl-letf (((symbol-function 'magit-status) #'ignore)
                    ((symbol-function 'window-list)
                     (lambda (&rest _) (list fake-win)))
                    ((symbol-function 'window-buffer)
                     (lambda (w) (when (eq w fake-win) fake-buf)))
                    ((symbol-function 'select-window)
                     (lambda (w) (setq selected-win w)))
                    ((symbol-function 'magit-refresh)
                     (lambda () (setq refreshed t)))
                    ((symbol-function 'claude-repl--path-canonical)
                     (lambda (p) (directory-file-name (or p "")))))
            (claude-repl--show-and-refresh-magit-status "/tmp/repo")
            (should (null selected-win))
            (should (null refreshed))))
      (kill-buffer fake-buf))))

(ert-deftest claude-repl-test-show-and-refresh-magit-skips-non-magit-windows ()
  "Windows whose buffer is not in magit-status-mode are skipped."
  (let ((selected-win nil)
        (refreshed nil)
        (fake-win 'fake-window)
        (fake-buf (generate-new-buffer "*not-magit*")))
    (unwind-protect
        (progn
          (with-current-buffer fake-buf
            (setq-local default-directory "/tmp/repo/")
            (setq major-mode 'fundamental-mode))
          (cl-letf (((symbol-function 'magit-status) #'ignore)
                    ((symbol-function 'window-list)
                     (lambda (&rest _) (list fake-win)))
                    ((symbol-function 'window-buffer)
                     (lambda (w) (when (eq w fake-win) fake-buf)))
                    ((symbol-function 'select-window)
                     (lambda (w) (setq selected-win w)))
                    ((symbol-function 'magit-refresh)
                     (lambda () (setq refreshed t)))
                    ((symbol-function 'claude-repl--path-canonical)
                     (lambda (p) (directory-file-name (or p "")))))
            (claude-repl--show-and-refresh-magit-status "/tmp/repo")
            (should (null selected-win))
            (should (null refreshed))))
      (kill-buffer fake-buf))))

;;;; ---- Tests: workspace-merge-current-into-source ----

(ert-deftest claude-repl-test-merge-into-source-routes-to-recorded-source-dir ()
  "When :source-ws-dir points at an existing dir, switch-to-project is called with it."
  (claude-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-merge-src-" t))
          (target-arg :unset)
          (merge-do-args :unset))
      (unwind-protect
          (progn
            (claude-repl--ws-put "wt-ws" :project-dir "/tmp/wt-dir/")
            (claude-repl--ws-put "wt-ws" :source-ws-dir tmpdir)
            (cl-letf (((symbol-function '+workspace-current-name) (lambda () "wt-ws"))
                      ((symbol-function 'claude-repl--master-worktree-path)
                       (lambda (_root) nil))
                      ((symbol-function 'claude-repl--assert-clean-worktree)
                       (lambda (&rest _) nil))
                      ((symbol-function 'claude-repl-switch-to-project)
                       (lambda (target) (setq target-arg target)))
                      ((symbol-function 'claude-repl--workspace-merge-do)
                       (lambda (&rest args) (setq merge-do-args args))))
              (claude-repl-workspace-merge-current-into-source)
              (should (equal target-arg tmpdir))
              (should (equal merge-do-args (list "wt-ws" tmpdir)))))
        (delete-directory tmpdir t)))))

(ert-deftest claude-repl-test-merge-into-source-falls-back-to-master-when-recorded-dir-gone ()
  "If :source-ws-dir refers to a missing directory, fall back to master worktree path."
  (claude-repl-test--with-clean-state
    (let ((target-arg :unset))
      (claude-repl--ws-put "wt-ws" :project-dir "/tmp/wt-dir/")
      (claude-repl--ws-put "wt-ws" :source-ws-dir "/no/such/dir/")
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "wt-ws"))
                ((symbol-function 'claude-repl--master-worktree-path)
                 (lambda (_root) "/tmp/master-fallback/"))
                ((symbol-function 'claude-repl--assert-clean-worktree)
                 (lambda (&rest _) nil))
                ((symbol-function 'claude-repl-switch-to-project)
                 (lambda (target) (setq target-arg target)))
                ((symbol-function 'claude-repl--workspace-merge-do) #'ignore))
        (claude-repl-workspace-merge-current-into-source)
        (should (equal target-arg "/tmp/master-fallback/"))))))

(ert-deftest claude-repl-test-merge-into-source-falls-back-to-master-when-no-recorded-source ()
  "Legacy workspace with no :source-ws-dir falls back to master worktree path."
  (claude-repl-test--with-clean-state
    (let ((target-arg :unset))
      (claude-repl--ws-put "wt-ws" :project-dir "/tmp/wt-dir/")
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "wt-ws"))
                ((symbol-function 'claude-repl--master-worktree-path)
                 (lambda (_root) "/tmp/master-fallback/"))
                ((symbol-function 'claude-repl--assert-clean-worktree)
                 (lambda (&rest _) nil))
                ((symbol-function 'claude-repl-switch-to-project)
                 (lambda (target) (setq target-arg target)))
                ((symbol-function 'claude-repl--workspace-merge-do) #'ignore))
        (claude-repl-workspace-merge-current-into-source)
        (should (equal target-arg "/tmp/master-fallback/"))))))

(ert-deftest claude-repl-test-merge-into-source-errors-when-no-source-and-no-master ()
  "user-errors when neither a recorded source nor a master worktree can be found."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "wt-ws" :project-dir "/tmp/wt-dir/")
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "wt-ws"))
              ((symbol-function 'claude-repl--master-worktree-path)
               (lambda (_root) nil)))
      (should-error (claude-repl-workspace-merge-current-into-source)
                    :type 'user-error))))

(ert-deftest claude-repl-test-merge-into-source-errors-when-already-on-source ()
  "user-errors when the resolved target equals the current workspace's dir."
  (claude-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-merge-self-" t)))
      (unwind-protect
          (progn
            (claude-repl--ws-put "self-ws" :project-dir tmpdir)
            (claude-repl--ws-put "self-ws" :source-ws-dir tmpdir)
            (cl-letf (((symbol-function '+workspace-current-name) (lambda () "self-ws"))
                      ((symbol-function 'claude-repl--master-worktree-path)
                       (lambda (_root) nil)))
              (should-error (claude-repl-workspace-merge-current-into-source)
                            :type 'user-error)))
        (delete-directory tmpdir t)))))

(ert-deftest claude-repl-test-workspace-merge-into-source-accepts-explicit-ws ()
  "workspace-merge-into-source operates on the named workspace, not (+workspace-current-name)."
  (claude-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-merge-explicit-" t))
          (target-arg :unset)
          (merge-do-args :unset))
      (unwind-protect
          (progn
            (claude-repl--ws-put "named-ws" :project-dir "/tmp/named-dir/")
            (claude-repl--ws-put "named-ws" :source-ws-dir tmpdir)
            (cl-letf (((symbol-function '+workspace-current-name) (lambda () "other-ws"))
                      ((symbol-function 'claude-repl--master-worktree-path)
                       (lambda (_root) nil))
                      ((symbol-function 'claude-repl--assert-clean-worktree)
                       (lambda (&rest _) nil))
                      ((symbol-function 'claude-repl-switch-to-project)
                       (lambda (target) (setq target-arg target)))
                      ((symbol-function 'claude-repl--workspace-merge-do)
                       (lambda (&rest args) (setq merge-do-args args))))
              (claude-repl--workspace-merge-into-source "named-ws")
              (should (equal target-arg tmpdir))
              (should (equal merge-do-args (list "named-ws" tmpdir)))))
        (delete-directory tmpdir t)))))

(ert-deftest claude-repl-test-workspace-merge-into-source-normalizes-branchy-name ()
  "Branch-style names like \"DWC/feature-one\" are normalized to the bare workspace name."
  (claude-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-merge-branchy-" t))
          (merge-do-args :unset))
      (unwind-protect
          (progn
            (claude-repl--ws-put "feature-one" :project-dir "/tmp/feature-one/")
            (claude-repl--ws-put "feature-one" :source-ws-dir tmpdir)
            (cl-letf (((symbol-function '+workspace-current-name) (lambda () "other-ws"))
                      ((symbol-function 'claude-repl--master-worktree-path)
                       (lambda (_root) nil))
                      ((symbol-function 'claude-repl--assert-clean-worktree)
                       (lambda (&rest _) nil))
                      ((symbol-function 'claude-repl-switch-to-project) #'ignore)
                      ((symbol-function 'claude-repl--workspace-merge-do)
                       (lambda (&rest args) (setq merge-do-args args))))
              (claude-repl--workspace-merge-into-source "DWC/feature-one")
              (should (equal (car merge-do-args) "feature-one"))))
        (delete-directory tmpdir t)))))

(ert-deftest claude-repl-test-workspace-merge-into-source-errors-on-unknown-ws ()
  "user-errors when the named workspace is not registered in the workspaces hash."
  (claude-repl-test--with-clean-state
    (should-error (claude-repl--workspace-merge-into-source "no-such-ws")
                  :type 'user-error)))

;;;; ---- Tests: resolve-merge-into-source-target ----

(ert-deftest claude-repl-test-resolve-merge-target-nil-parent ()
  "Returns nil when parent-dir is nil."
  (should (null (claude-repl--resolve-merge-into-source-target nil "/m/"))))

(ert-deftest claude-repl-test-resolve-merge-target-nil-master ()
  "Returns parent unchanged when master-dir is nil."
  (should (equal (claude-repl--resolve-merge-into-source-target "/p/" nil)
                 "/p/")))

(ert-deftest claude-repl-test-resolve-merge-target-parent-is-master ()
  "Returns parent unchanged when parent-dir == master-dir."
  (let ((tmp (make-temp-file "test-resolve-master-" t)))
    (unwind-protect
        (should (equal (claude-repl--resolve-merge-into-source-target tmp tmp)
                       tmp))
      (delete-directory tmp t))))

(ert-deftest claude-repl-test-resolve-merge-target-parent-already-merged ()
  "Returns master-dir when parent != master and parent's branch is in master."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'claude-repl--branch-merged-into-p)
               (lambda (_s _t) t)))
      (should (equal (claude-repl--resolve-merge-into-source-target "/p/" "/m/")
                     "/m/")))))

(ert-deftest claude-repl-test-resolve-merge-target-parent-not-merged ()
  "Returns parent-dir when parent's branch is not yet in master."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'claude-repl--branch-merged-into-p)
               (lambda (_s _t) nil)))
      (should (equal (claude-repl--resolve-merge-into-source-target "/p/" "/m/")
                     "/p/")))))

(ert-deftest claude-repl-test-resolve-merge-target-walks-merged-grandparent ()
  "Walks the source-ws-dir chain when intermediate ancestors are merged.
parent merged into grandparent, grandparent merged into master ⇒ returns master."
  (claude-repl-test--with-clean-state
    (puthash "p-ws" '(:project-dir "/p/" :source-ws-dir "/g/")
             claude-repl--workspaces)
    (puthash "g-ws" '(:project-dir "/g/" :source-ws-dir nil)
             claude-repl--workspaces)
    (cl-letf (((symbol-function 'claude-repl--branch-merged-into-p)
               (lambda (_s _t) t))
              ((symbol-function 'file-directory-p) (lambda (_) t))
              ((symbol-function 'claude-repl--path-canonical) #'identity))
      (should (equal (claude-repl--resolve-merge-into-source-target "/p/" "/m/")
                     "/m/")))))

(ert-deftest claude-repl-test-resolve-merge-target-stops-at-unmerged-grandparent ()
  "When parent is merged but grandparent is not, returns the grandparent dir."
  (claude-repl-test--with-clean-state
    (puthash "p-ws" '(:project-dir "/p/" :source-ws-dir "/g/")
             claude-repl--workspaces)
    (puthash "g-ws" '(:project-dir "/g/" :source-ws-dir nil)
             claude-repl--workspaces)
    (let ((calls 0))
      (cl-letf (((symbol-function 'claude-repl--branch-merged-into-p)
                 (lambda (_s _t)
                   (setq calls (1+ calls))
                   ;; First call (p→g) merged; second call (g→m) not.
                   (= calls 1)))
                ((symbol-function 'file-directory-p) (lambda (_) t))
                ((symbol-function 'claude-repl--path-canonical) #'identity))
        (should (equal (claude-repl--resolve-merge-into-source-target "/p/" "/m/")
                       "/g/"))))))

(ert-deftest claude-repl-test-resolve-merge-target-cycle-cap ()
  "Self-referential `:source-ws-dir' chain terminates at the depth cap.
Defense-in-depth — should never happen in practice, but the resolver
must not infinite-loop if it does."
  (claude-repl-test--with-clean-state
    (puthash "p-ws" '(:project-dir "/p/" :source-ws-dir "/p/")
             claude-repl--workspaces)
    (let ((claude-repl-merge-resolve-max-depth 4))
      (cl-letf (((symbol-function 'claude-repl--branch-merged-into-p)
                 (lambda (_s _t) t))
                ((symbol-function 'file-directory-p) (lambda (_) t))
                ((symbol-function 'claude-repl--path-canonical) #'identity))
        ;; Should return without hanging; exact value is the candidate
        ;; held when the depth cap fires.
        (should (claude-repl--resolve-merge-into-source-target "/p/" "/m/"))))))

;;;; ---- Tests: branch-merged-into-p (generalized predicate) ----

(ert-deftest claude-repl-test-branch-merged-into-p-nil-args ()
  "Returns nil when either dir is nil."
  (should (null (claude-repl--branch-merged-into-p nil "/m/")))
  (should (null (claude-repl--branch-merged-into-p "/p/" nil))))

(ert-deftest claude-repl-test-branch-merged-into-p-same-branch ()
  "Returns nil when source and target have the same current branch."
  (cl-letf (((symbol-function 'claude-repl--git-string)
             (lambda (&rest _) "main")))
    (should-not (claude-repl--branch-merged-into-p "/a/" "/b/"))))

;;;; ---- Tests: branch-merged async cache ----

(ert-deftest claude-repl-test-ws-merge-parent-dir-uses-source-when-live ()
  "`--ws-merge-parent-dir' returns `:source-ws-dir' when it is a live directory."
  (claude-repl-test--with-clean-state
    (let ((tmp (make-temp-file "merge-parent-" t)))
      (unwind-protect
          (progn
            (claude-repl--ws-put "ws" :project-dir "/anything/")
            (claude-repl--ws-put "ws" :source-ws-dir tmp)
            (should (equal (claude-repl--ws-merge-parent-dir "ws") tmp)))
        (delete-directory tmp t)))))

(ert-deftest claude-repl-test-ws-merge-parent-dir-falls-back-to-master ()
  "Falls back to the master worktree path when `:source-ws-dir' is missing."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws" :project-dir "/some/repo/")
    (cl-letf (((symbol-function 'claude-repl--ws-dir)
               (lambda (_) "/some/repo/"))
              ((symbol-function 'claude-repl--master-worktree-path)
               (lambda (_) "/master/dir/")))
      (should (equal (claude-repl--ws-merge-parent-dir "ws") "/master/dir/")))))

(ert-deftest claude-repl-test-branch-merge-sentinel-merged-on-zero-exit ()
  "Sentinel records `merged' when git merge-base exits 0."
  (claude-repl-test--with-clean-state
    (let ((proc (make-pipe-process :name "test-merge" :buffer nil :noquery t)))
      (cl-letf (((symbol-function 'process-live-p) (lambda (_) nil))
                ((symbol-function 'process-exit-status) (lambda (_) 0)))
        (claude-repl--branch-merge-sentinel "ws" proc "finished\n")
        (should (eq (claude-repl--ws-get "ws" :branch-merged) 'merged))
        (should (null (claude-repl--ws-get "ws" :merge-proc))))
      (delete-process proc))))

(ert-deftest claude-repl-test-branch-merge-sentinel-not-merged-on-one-exit ()
  "Sentinel records `not-merged' when git merge-base exits 1."
  (claude-repl-test--with-clean-state
    (let ((proc (make-pipe-process :name "test-merge" :buffer nil :noquery t)))
      (cl-letf (((symbol-function 'process-live-p) (lambda (_) nil))
                ((symbol-function 'process-exit-status) (lambda (_) 1)))
        (claude-repl--branch-merge-sentinel "ws" proc "finished\n")
        (should (eq (claude-repl--ws-get "ws" :branch-merged) 'not-merged)))
      (delete-process proc))))

(ert-deftest claude-repl-test-branch-merge-sentinel-leaves-cache-on-error ()
  "Unexpected exit codes leave `:branch-merged' untouched."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws" :branch-merged 'merged)
    (let ((proc (make-pipe-process :name "test-merge" :buffer nil :noquery t)))
      (cl-letf (((symbol-function 'process-live-p) (lambda (_) nil))
                ((symbol-function 'process-exit-status) (lambda (_) 128)))
        (claude-repl--branch-merge-sentinel "ws" proc "fatal\n")
        (should (eq (claude-repl--ws-get "ws" :branch-merged) 'merged)))
      (delete-process proc))))

(ert-deftest claude-repl-test-branch-merge-check-in-progress-detects-live-proc ()
  "`--branch-merge-check-in-progress-p' returns non-nil when `:merge-proc' is alive."
  (claude-repl-test--with-clean-state
    (let ((proc (make-pipe-process :name "test-live" :buffer nil :noquery t)))
      (claude-repl--ws-put "ws" :merge-proc proc)
      (cl-letf (((symbol-function 'process-live-p) (lambda (p) (eq p proc))))
        (should (claude-repl--branch-merge-check-in-progress-p "ws")))
      (delete-process proc))))

(ert-deftest claude-repl-test-async-refresh-branch-merged-skips-when-in-progress ()
  "No new process is spawned when one is already live for the workspace."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws" :project-dir "/some/")
    (let ((spawned nil))
      (cl-letf (((symbol-function 'claude-repl--branch-merge-check-in-progress-p)
                 (lambda (_) t))
                ((symbol-function 'make-process)
                 (lambda (&rest _) (setq spawned t) :proc)))
        (claude-repl--async-refresh-branch-merged "ws")
        (should-not spawned)))))

;;;; ---- Tests: ws-name-for-dir (reverse lookup) ----

(ert-deftest claude-repl-test-ws-name-for-dir-nil-arg ()
  "Returns nil for nil DIR."
  (should (null (claude-repl--ws-name-for-dir nil))))

(ert-deftest claude-repl-test-ws-name-for-dir-finds-match ()
  "Returns the workspace name whose `:project-dir' canonicalizes to DIR."
  (claude-repl-test--with-clean-state
    (puthash "alpha" '(:project-dir "/repo-a/") claude-repl--workspaces)
    (puthash "beta"  '(:project-dir "/repo-b/") claude-repl--workspaces)
    (cl-letf (((symbol-function 'claude-repl--path-canonical) #'identity))
      (should (equal (claude-repl--ws-name-for-dir "/repo-b/") "beta")))))

(ert-deftest claude-repl-test-ws-name-for-dir-returns-nil-on-miss ()
  "Returns nil when no workspace's `:project-dir' matches DIR."
  (claude-repl-test--with-clean-state
    (puthash "alpha" '(:project-dir "/repo-a/") claude-repl--workspaces)
    (cl-letf (((symbol-function 'claude-repl--path-canonical) #'identity))
      (should (null (claude-repl--ws-name-for-dir "/missing/"))))))

;;;; ---- Tests: ws-merged-p ----

(ert-deftest claude-repl-test-ws-merged-p-true-when-cached-merged ()
  "Returns t when WS's `:branch-merged' cache is `merged'."
  (claude-repl-test--with-clean-state
    (puthash "ws" '(:branch-merged merged) claude-repl--workspaces)
    (should (claude-repl--ws-merged-p "ws"))))

(ert-deftest claude-repl-test-ws-merged-p-nil-when-cached-not-merged ()
  "Returns nil when WS's `:branch-merged' cache is `not-merged'."
  (claude-repl-test--with-clean-state
    (puthash "ws" '(:branch-merged not-merged) claude-repl--workspaces)
    (should-not (claude-repl--ws-merged-p "ws"))))

(ert-deftest claude-repl-test-ws-merged-p-nil-when-cache-absent ()
  "Returns nil on cache miss — drawer should treat unknown as :main."
  (claude-repl-test--with-clean-state
    (puthash "ws" '() claude-repl--workspaces)
    (should-not (claude-repl--ws-merged-p "ws"))))

;;;; ---- Tests: merge-into-source re-routes when parent merged into master ----

(ert-deftest claude-repl-test-merge-into-source-reroutes-to-master-when-parent-already-merged ()
  "When parent worktree's branch is already in master, switch-to-project gets master-dir."
  (claude-repl-test--with-clean-state
    (let ((parent-dir (make-temp-file "test-reroute-parent-" t))
          (master-dir (make-temp-file "test-reroute-master-" t))
          (target-arg :unset)
          (merge-do-args :unset))
      (unwind-protect
          (progn
            (claude-repl--ws-put "wt-ws" :project-dir "/tmp/wt-dir/")
            (claude-repl--ws-put "wt-ws" :source-ws-dir parent-dir)
            (cl-letf (((symbol-function '+workspace-current-name) (lambda () "wt-ws"))
                      ((symbol-function 'claude-repl--master-worktree-path)
                       (lambda (_root) master-dir))
                      ((symbol-function 'claude-repl--branch-merged-into-p)
                       (lambda (_s _t) t))
                      ((symbol-function 'claude-repl--assert-clean-worktree)
                       (lambda (&rest _) nil))
                      ((symbol-function 'claude-repl-switch-to-project)
                       (lambda (target) (setq target-arg target)))
                      ((symbol-function 'claude-repl--workspace-merge-do)
                       (lambda (&rest args) (setq merge-do-args args))))
              (claude-repl-workspace-merge-current-into-source)
              (should (equal target-arg master-dir))
              (should (equal merge-do-args (list "wt-ws" master-dir)))))
        (delete-directory parent-dir t)
        (delete-directory master-dir t)))))

(ert-deftest claude-repl-test-merge-into-source-stays-on-parent-when-not-yet-merged ()
  "When parent worktree's branch has unmerged commits, keep parent as the target."
  (claude-repl-test--with-clean-state
    (let ((parent-dir (make-temp-file "test-stay-parent-" t))
          (master-dir (make-temp-file "test-stay-master-" t))
          (target-arg :unset)
          (merge-do-args :unset))
      (unwind-protect
          (progn
            (claude-repl--ws-put "wt-ws" :project-dir "/tmp/wt-dir/")
            (claude-repl--ws-put "wt-ws" :source-ws-dir parent-dir)
            (cl-letf (((symbol-function '+workspace-current-name) (lambda () "wt-ws"))
                      ((symbol-function 'claude-repl--master-worktree-path)
                       (lambda (_root) master-dir))
                      ((symbol-function 'claude-repl--branch-merged-into-p)
                       (lambda (_s _t) nil))
                      ((symbol-function 'claude-repl--assert-clean-worktree)
                       (lambda (&rest _) nil))
                      ((symbol-function 'claude-repl-switch-to-project)
                       (lambda (target) (setq target-arg target)))
                      ((symbol-function 'claude-repl--workspace-merge-do)
                       (lambda (&rest args) (setq merge-do-args args))))
              (claude-repl-workspace-merge-current-into-source)
              (should (equal target-arg parent-dir))
              (should (equal merge-do-args (list "wt-ws" parent-dir)))))
        (delete-directory parent-dir t)
        (delete-directory master-dir t)))))

;;;; ---- Tests: workspace-merge-do project-root override ----

(ert-deftest claude-repl-test-workspace-merge-do-uses-project-root-override ()
  "When PROJECT-ROOT-OVERRIDE is non-nil, cherry-pick lands there (not at current ws's dir)."
  (let ((cherry-pick-dir nil)
        (magit-dir nil))
    (cl-letf* (((symbol-function '+workspace-current-name) (lambda () "current"))
               ((symbol-function 'claude-repl--workspace-branch) (lambda (_ws) "branch-x"))
               ((symbol-function 'claude-repl--ws-dir) (lambda (_ws) "/should/not/be/used/"))
               ((symbol-function 'claude-repl--git-branch-exists-p) (lambda (_dir _br) t))
               ((symbol-function 'claude-repl--cherry-pick-base) (lambda (_dir _br) "abc123"))
               ((symbol-function 'claude-repl--cherry-pick-commits)
                (lambda (dir _ws _base _br) (setq cherry-pick-dir dir)))
               ((symbol-function 'claude-repl--finish-workspace) (lambda (_ws) nil))
               ((symbol-function 'load-file) #'ignore)
               ((symbol-function 'magit-status) (lambda (dir) (setq magit-dir dir))))
      (claude-repl--workspace-merge-do "other-ws" "/explicit/target/")
      (should (equal cherry-pick-dir "/explicit/target/"))
      (should (equal magit-dir "/explicit/target/")))))

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
              ((symbol-function 'claude-repl--flash-current-tab) #'ignore)
              ((symbol-function '+workspace-current-name) (lambda () "test-ws")))
      (claude-repl--worktree-creation-switch-callback "/tmp/fake" "test-ws")
      (should (equal (reverse call-order) '(switch))))))

(ert-deftest claude-repl-test-worktree-callback-flashes-destination-tab ()
  "worktree-creation-switch-callback flashes the destination tab.
Symmetric with the project-picker (`SPC p p') and reopen paths so every
identity-based jump pulses uniformly."
  (let ((flashed nil))
    (cl-letf (((symbol-function 'claude-repl--switch-to-workspace) #'ignore)
              ((symbol-function 'claude-repl--flash-current-tab)
               (lambda () (setq flashed t)))
              ((symbol-function '+workspace-current-name) (lambda () "test-ws")))
      (claude-repl--worktree-creation-switch-callback "/tmp/fake" "test-ws")
      (should flashed))))

;;;; ---- Tests: claude-repl-jump-to-workspace ----

(ert-deftest claude-repl-test-jump-to-workspace-delegates-to-switch ()
  "claude-repl-jump-to-workspace forwards WS to the raw switch primitive."
  (let ((switched-ws nil))
    (cl-letf (((symbol-function 'claude-repl--switch-to-workspace)
               (lambda (ws) (setq switched-ws ws)))
              ((symbol-function 'claude-repl--flash-current-tab) #'ignore))
      (claude-repl-jump-to-workspace "target-ws")
      (should (equal switched-ws "target-ws")))))

(ert-deftest claude-repl-test-jump-to-workspace-flashes-by-default ()
  "Without NO-FLASH, the jumper pulses the destination tab — flash is inherent."
  (let ((flashed nil))
    (cl-letf (((symbol-function 'claude-repl--switch-to-workspace) #'ignore)
              ((symbol-function 'claude-repl--flash-current-tab)
               (lambda () (setq flashed t))))
      (claude-repl-jump-to-workspace "target-ws")
      (should flashed))))

(ert-deftest claude-repl-test-jump-to-workspace-no-flash-suppresses-pulse ()
  "Passing NO-FLASH non-nil skips the pulse — escape hatch for bulk callers."
  (let ((flashed nil))
    (cl-letf (((symbol-function 'claude-repl--switch-to-workspace) #'ignore)
              ((symbol-function 'claude-repl--flash-current-tab)
               (lambda () (setq flashed t))))
      (claude-repl-jump-to-workspace "target-ws" t)
      (should-not flashed))))

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

;;;; ---- Tests: finalize-worktree-workspace defers initial buffers ----

(ert-deftest claude-repl-test-finalize-sets-pending-initial-buffers ()
  "finalize-worktree-workspace sets :pending-initial-buffers and does not call open-initial-buffers.
The drain happens on workspace activation; calling open-initial-buffers
synchronously here uses `find-file-noselect' in the caller's perspective,
leaking the opened buffers into the wrong workspace."
  (let ((open-called nil))
    (claude-repl-test--with-clean-state
      (cl-letf (((symbol-function 'claude-repl--register-projectile-project) #'ignore)
                ((symbol-function 'claude-repl--path-canonical) #'identity)
                ((symbol-function '+workspace-new) #'ignore)
                ((symbol-function 'magit-status) #'ignore)
                ((symbol-function 'claude-repl--remove-doom-dashboard) #'ignore)
                ((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'claude-repl--open-initial-buffers)
                 (lambda (&rest _) (setq open-called t)))
                ((symbol-function 'claude-repl--enqueue-preemptive-prompt) #'ignore)
                ((symbol-function 'claude-repl--apply-workspace-properties) #'ignore)
                ((symbol-function 'claude-repl--setup-worktree-session) #'ignore))
        (claude-repl--finalize-worktree-workspace
         "/tmp/fake" "test-ws" nil nil nil nil nil)
        (should (claude-repl--ws-get "test-ws" :pending-initial-buffers))
        (should-not open-called)))))

;;;; ---- Tests: claude-repl-create-doom-oneshot-workspace ----

(ert-deftest claude-repl-test-create-doom-oneshot-pins-git-root-to-doom-config ()
  "doom-oneshot pins git-root to `~/.config/doom' regardless of the current
workspace's project, so the binding can be invoked from anywhere and still
edit the doom config."
  (claude-repl-test--with-clean-state
    (let ((captured-git-root :unset))
      (cl-letf (((symbol-function '+workspace-current-name)
                 (lambda () "unrelated-ws"))
                ((symbol-function 'claude-repl--ws-dir)
                 (lambda (_ws) "/tmp/unrelated-repo/"))
                ((symbol-function 'read-string)
                 (lambda (&rest _) "tweak the modeline"))
                ((symbol-function 'claude-repl--spawn-workspace-generation)
                 (lambda (_raw _prefixed git-root _base _fork-from)
                   (setq captured-git-root git-root))))
        (claude-repl-create-doom-oneshot-workspace)
        (should (equal captured-git-root claude-repl--doom-config-dir))
        (should (equal captured-git-root
                       (file-name-as-directory
                        (expand-file-name "~/.config/doom"))))))))

(ert-deftest claude-repl-test-create-doom-oneshot-uses-master-base ()
  "doom-oneshot branches off local `master', mirroring `SPC TAB N'."
  (claude-repl-test--with-clean-state
    (let ((captured-base :unset))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) "tweak the modeline"))
                ((symbol-function 'claude-repl--spawn-workspace-generation)
                 (lambda (_raw _prefixed _git-root base _fork-from)
                   (setq captured-base base))))
        (claude-repl-create-doom-oneshot-workspace)
        (should (equal captured-base "master"))))))

(ert-deftest claude-repl-test-create-doom-oneshot-appends-merge-suffix-to-prefixed ()
  "The merge-on-success suffix is included in the PREFIXED prompt (the
spawned agent's first message) so the inner agent knows to invoke
`/workspace-merge' after a successful, tested implementation."
  (claude-repl-test--with-clean-state
    (let ((captured-prefixed :unset))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) "tweak the modeline"))
                ((symbol-function 'claude-repl--spawn-workspace-generation)
                 (lambda (_raw prefixed _git-root _base _fork-from)
                   (setq captured-prefixed prefixed))))
        (claude-repl-create-doom-oneshot-workspace)
        (should (string-match-p "/workspace-merge" captured-prefixed))
        (should (string-match-p
                 (regexp-quote claude-repl--oneshot-merge-suffix)
                 captured-prefixed))))))

(ert-deftest claude-repl-test-create-doom-oneshot-keeps-raw-prompt-clean ()
  "The merge suffix is NOT appended to the raw prompt — raw is used purely
for slug generation and should not get polluted with skill names like
`/workspace-merge', which would derail the workspace-name slug."
  (claude-repl-test--with-clean-state
    (let ((captured-raw :unset))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) "tweak the modeline"))
                ((symbol-function 'claude-repl--spawn-workspace-generation)
                 (lambda (raw _prefixed _git-root _base _fork-from)
                   (setq captured-raw raw))))
        (claude-repl-create-doom-oneshot-workspace)
        (should (equal captured-raw "tweak the modeline"))
        (should-not (string-match-p "/workspace-merge" captured-raw))))))

(ert-deftest claude-repl-test-create-doom-oneshot-prefixed-includes-autonomous-prefix ()
  "The prefixed prompt still starts with the standard autonomous-prompt
prefix so the spawned agent runs autonomously without waiting."
  (claude-repl-test--with-clean-state
    (let ((captured-prefixed :unset))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) "tweak the modeline"))
                ((symbol-function 'claude-repl--spawn-workspace-generation)
                 (lambda (_raw prefixed _git-root _base _fork-from)
                   (setq captured-prefixed prefixed))))
        (claude-repl-create-doom-oneshot-workspace)
        (should (string-prefix-p claude-repl--autonomous-prompt-prefix
                                 captured-prefixed))))))

(ert-deftest claude-repl-test-create-doom-oneshot-rejects-empty-prompt ()
  "An empty/whitespace prompt is rejected — there is nothing to slug or
implement, and we do not want to spawn a useless workspace."
  (claude-repl-test--with-clean-state
    (let ((spawned nil))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) "   "))
                ((symbol-function 'claude-repl--spawn-workspace-generation)
                 (lambda (&rest _) (setq spawned t))))
        (should-error (claude-repl-create-doom-oneshot-workspace)
                      :type 'user-error)
        (should-not spawned)))))

(ert-deftest claude-repl-test-create-doom-oneshot-passes-no-fork-from ()
  "doom-oneshot is not a fork — fork-from must be nil so the new workspace
starts a fresh Claude session rather than resuming someone else's."
  (claude-repl-test--with-clean-state
    (let ((captured-fork-from :unset))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) "tweak the modeline"))
                ((symbol-function 'claude-repl--spawn-workspace-generation)
                 (lambda (_raw _prefixed _git-root _base fork-from)
                   (setq captured-fork-from fork-from))))
        (claude-repl-create-doom-oneshot-workspace)
        (should (null captured-fork-from))))))

(ert-deftest claude-repl-test-oneshot-merge-suffix-mentions-stop-on-ambiguity ()
  "The merge suffix tells the spawned agent to STOP (not push on) when it
hits genuine ambiguity it cannot resolve — explicitly required so a
faulty one-shot implementation isn't auto-merged."
  (should (string-match-p "STOP" claude-repl--oneshot-merge-suffix))
  (should (string-match-p "ambiguity" claude-repl--oneshot-merge-suffix)))

(ert-deftest claude-repl-test-oneshot-merge-suffix-mentions-tests-and-commits ()
  "Merge is gated on implementation, tests, AND commits — the suffix must
spell that out so the spawned agent doesn't merge half-finished work."
  (should (string-match-p "tests" claude-repl--oneshot-merge-suffix))
  (should (string-match-p "[Cc]ommit" claude-repl--oneshot-merge-suffix)))

;;; test-worktree.el ends here
