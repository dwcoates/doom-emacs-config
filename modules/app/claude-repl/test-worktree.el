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
;;
;; `claude-repl--assert-clean-worktree' calls `claude-repl--git-exit-code'
;; twice (unstaged + staged check).  Tests mock the wrapper.

(ert-deftest claude-repl-test-assert-clean-worktree-clean ()
  "Clean worktree does not signal."
  (cl-letf (((symbol-function 'claude-repl--git-exit-code)
             (lambda (&rest _args) 0)))
    ;; Should not error.
    (claude-repl--assert-clean-worktree "test-ws" "/tmp/repo")))

(ert-deftest claude-repl-test-assert-clean-worktree-unstaged ()
  "Unstaged changes signal user-error."
  (cl-letf (((symbol-function 'claude-repl--git-exit-code)
             (lambda (_root &rest args)
               (if (equal args '("diff" "--quiet")) 1 0))))
    (should-error (claude-repl--assert-clean-worktree "test-ws" "/tmp/repo")
                  :type 'user-error)))

(ert-deftest claude-repl-test-assert-clean-worktree-staged ()
  "Staged changes signal user-error."
  (cl-letf (((symbol-function 'claude-repl--git-exit-code)
             (lambda (_root &rest args)
               (if (equal args '("diff" "--cached" "--quiet")) 1 0))))
    (should-error (claude-repl--assert-clean-worktree "test-ws" "/tmp/repo")
                  :type 'user-error)))

;;;; ---- Tests: git-exit-code / git-branch-exists-p ----
;;
;; The wrappers themselves (`claude-repl--git-exit-code',
;; `claude-repl--git-branch-exists-p') are the external boundary; per
;; AGENTS.md "No External Processes or External State in Tests", the
;; wrappers are mocked in their callers' tests rather than exercised
;; against a real git binary.  Below we keep tests for
;; `claude-repl--git-branch-exists-p' that mock its sole dependency
;; (`claude-repl--git-exit-code') and verify the predicate logic.
;; The trivial wrapper-of-call-process functions are intentionally not
;; tested in isolation — there is nothing to test that isn't a tautology
;; against `call-process'.

(ert-deftest claude-repl-test-git-branch-exists-p-true ()
  "Existing branch returns non-nil (exit-code 0 from rev-parse --verify)."
  (cl-letf (((symbol-function 'claude-repl--git-exit-code)
             (lambda (_root &rest args)
               (should (equal args '("rev-parse" "--verify" "feature")))
               0)))
    (should (claude-repl--git-branch-exists-p "/tmp/repo" "feature"))))

(ert-deftest claude-repl-test-git-branch-exists-p-false ()
  "Non-existent branch returns nil (non-zero exit-code)."
  (cl-letf (((symbol-function 'claude-repl--git-exit-code)
             (lambda (&rest _args) 128)))
    (should-not (claude-repl--git-branch-exists-p "/tmp/repo" "nonexistent"))))

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
;;
;; `claude-repl--master-worktree-path' shells out via
;; `claude-repl--git-string-quiet "-C" root "worktree" "list" "--porcelain"'
;; and parses the porcelain output for the trunk branch.  Tests mock the
;; wrapper and assert the parser/dispatch behavior, per AGENTS.md "No
;; External Processes or External State in Tests".

(ert-deftest claude-repl-test-master-worktree-path-single-worktree ()
  "In a single-worktree repo on master, returns that worktree's path."
  (let ((claude-repl-master-branch-name "master")
        (porcelain "worktree /tmp/repo\nHEAD abc\nbranch refs/heads/master\n"))
    (cl-letf (((symbol-function 'claude-repl--git-string-quiet)
               (lambda (&rest args)
                 (should (equal args '("-C" "/tmp/repo" "worktree" "list" "--porcelain")))
                 porcelain)))
      (should (equal (claude-repl--master-worktree-path "/tmp/repo")
                     "/tmp/repo")))))

(ert-deftest claude-repl-test-master-worktree-path-with-secondary ()
  "In a repo with main + secondary worktree, returns the main path."
  (let ((claude-repl-master-branch-name "master")
        (porcelain (concat "worktree /tmp/repo\nHEAD abc\nbranch refs/heads/master\n\n"
                           "worktree /tmp/wt\nHEAD def\nbranch refs/heads/feature-x\n")))
    (cl-letf (((symbol-function 'claude-repl--git-string-quiet)
               (lambda (&rest _args) porcelain)))
      ;; From the primary path.
      (should (equal (claude-repl--master-worktree-path "/tmp/repo") "/tmp/repo"))
      ;; Even when called from the secondary worktree, returns master path.
      (should (equal (claude-repl--master-worktree-path "/tmp/wt") "/tmp/repo")))))

(ert-deftest claude-repl-test-master-worktree-path-no-master ()
  "When no worktree is on the master branch, returns nil."
  (let ((claude-repl-master-branch-name "master")
        (porcelain "worktree /tmp/repo\nHEAD abc\nbranch refs/heads/feature-only\n"))
    (cl-letf (((symbol-function 'claude-repl--git-string-quiet)
               (lambda (&rest _args) porcelain)))
      (should (null (claude-repl--master-worktree-path "/tmp/repo"))))))

(ert-deftest claude-repl-test-master-worktree-path-honors-defcustom ()
  "Uses `claude-repl-master-branch-name' as the trunk branch name."
  (let ((claude-repl-master-branch-name "trunk")
        (porcelain "worktree /tmp/repo\nHEAD abc\nbranch refs/heads/trunk\n"))
    (cl-letf (((symbol-function 'claude-repl--git-string-quiet)
               (lambda (&rest _args) porcelain)))
      (should (equal (claude-repl--master-worktree-path "/tmp/repo")
                     "/tmp/repo")))))

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

(ert-deftest claude-repl-test-dispatch-workspace-command-close ()
  "Close commands route to the close handler and do not change delay."
  (let ((handled nil))
    (cl-letf (((symbol-function 'claude-repl--handle-close-command)
               (lambda (cmd) (push cmd handled))))
      (let ((cmd '((type . "close") (workspace . "ws1"))))
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

(ert-deftest claude-repl-test-dispatch-workspace-command-profile ()
  "Profile commands route to the profile handler and do not change delay."
  (let ((handled nil))
    (cl-letf (((symbol-function 'claude-repl--handle-profile-command)
               (lambda (cmd) (push cmd handled))))
      (let ((cmd '((type . "profile") (enabled . t))))
        (let ((new-delay (claude-repl--dispatch-workspace-command cmd 10)))
          (should (= new-delay 10))
          (should (= (length handled) 1)))))))

(ert-deftest claude-repl-test-dispatch-workspace-command-eval ()
  "Eval commands route to the eval handler and do not change delay."
  (let ((handled nil))
    (cl-letf (((symbol-function 'claude-repl--handle-eval-command)
               (lambda (cmd) (push cmd handled))))
      (let ((cmd '((type . "eval") (code . "(+ 1 2)"))))
        (let ((new-delay (claude-repl--dispatch-workspace-command cmd 10)))
          (should (= new-delay 10))
          (should (= (length handled) 1)))))))

;;;; ---- Tests: parse-profile-mode ----

(ert-deftest claude-repl-test-parse-profile-mode-nil-uses-default ()
  "Nil mode returns `claude-repl-profile-default-mode'."
  (let ((claude-repl-profile-default-mode 'cpu+mem))
    (should (eq (claude-repl--parse-profile-mode nil) 'cpu+mem))))

(ert-deftest claude-repl-test-parse-profile-mode-empty-uses-default ()
  "Empty-string mode returns `claude-repl-profile-default-mode'."
  (let ((claude-repl-profile-default-mode 'cpu))
    (should (eq (claude-repl--parse-profile-mode "") 'cpu))))

(ert-deftest claude-repl-test-parse-profile-mode-cpu ()
  "\"cpu\" maps to the `cpu' symbol."
  (should (eq (claude-repl--parse-profile-mode "cpu") 'cpu)))

(ert-deftest claude-repl-test-parse-profile-mode-mem ()
  "\"mem\" maps to the `mem' symbol."
  (should (eq (claude-repl--parse-profile-mode "mem") 'mem)))

(ert-deftest claude-repl-test-parse-profile-mode-cpu+mem ()
  "\"cpu+mem\" maps to the `cpu+mem' symbol."
  (should (eq (claude-repl--parse-profile-mode "cpu+mem") 'cpu+mem)))

(ert-deftest claude-repl-test-parse-profile-mode-unknown ()
  "Unknown mode string returns nil so the handler can refuse the request."
  (should (null (claude-repl--parse-profile-mode "gpu"))))

;;;; ---- Tests: handle-profile-command ----

(ert-deftest claude-repl-test-handle-profile-command-enabled-starts ()
  "enabled=t starts the profiler with the default mode when not already running."
  (let ((started-with :unset))
    (cl-letf (((symbol-function 'profiler-running-p) (lambda () nil))
              ((symbol-function 'profiler-start)
               (lambda (mode) (setq started-with mode)))
              ((symbol-function 'profiler-stop) (lambda () (error "should not stop")))
              ((symbol-function 'profiler-report) (lambda () (error "should not report")))
              (claude-repl-profile-default-mode 'cpu+mem))
      (claude-repl--handle-profile-command '((type . "profile") (enabled . t)))
      (should (eq started-with 'cpu+mem)))))

(ert-deftest claude-repl-test-handle-profile-command-enabled-honors-mode ()
  "enabled=t with mode=\"cpu\" starts the profiler with the `cpu' symbol."
  (let ((started-with :unset))
    (cl-letf (((symbol-function 'profiler-running-p) (lambda () nil))
              ((symbol-function 'profiler-start)
               (lambda (mode) (setq started-with mode)))
              ((symbol-function 'profiler-stop) (lambda () (error "should not stop")))
              ((symbol-function 'profiler-report) (lambda () (error "should not report"))))
      (claude-repl--handle-profile-command
       '((type . "profile") (enabled . t) (mode . "cpu")))
      (should (eq started-with 'cpu)))))

(ert-deftest claude-repl-test-handle-profile-command-enabled-noop-when-running ()
  "enabled=t is a no-op when the profiler is already running — no start, no stop."
  (let ((started nil))
    (cl-letf (((symbol-function 'profiler-running-p) (lambda () t))
              ((symbol-function 'profiler-start)
               (lambda (_mode) (setq started t)))
              ((symbol-function 'profiler-stop) (lambda () (error "should not stop")))
              ((symbol-function 'profiler-report) (lambda () (error "should not report"))))
      (claude-repl--handle-profile-command '((type . "profile") (enabled . t)))
      (should-not started))))

(ert-deftest claude-repl-test-handle-profile-command-disabled-stops-and-reports ()
  "enabled=nil stops the profiler and pops up the report when running."
  (let ((stopped nil) (reported nil) (sent nil))
    (cl-letf (((symbol-function 'profiler-running-p) (lambda () t))
              ((symbol-function 'profiler-start) (lambda (_m) (error "should not start")))
              ((symbol-function 'profiler-stop) (lambda () (setq stopped t)))
              ((symbol-function 'profiler-report) (lambda () (setq reported t)))
              ((symbol-function 'claude-repl--profile-report-buffers) (lambda () nil))
              ((symbol-function 'claude-repl--send) (lambda (&rest args) (push args sent))))
      (claude-repl--handle-profile-command
       '((type . "profile") (enabled . :json-false)))
      (should stopped)
      (should reported)
      ;; No workspace in JSON, so no send is dispatched.
      (should-not sent))))

(ert-deftest claude-repl-test-handle-profile-command-disabled-noop-when-not-running ()
  "enabled=nil is a no-op when the profiler is not running — no stop, no report."
  (let ((stopped nil) (reported nil))
    (cl-letf (((symbol-function 'profiler-running-p) (lambda () nil))
              ((symbol-function 'profiler-start) (lambda (_m) (error "should not start")))
              ((symbol-function 'profiler-stop) (lambda () (setq stopped t)))
              ((symbol-function 'profiler-report) (lambda () (setq reported t))))
      (claude-repl--handle-profile-command
       '((type . "profile") (enabled . :json-false)))
      (should-not stopped)
      (should-not reported))))

(ert-deftest claude-repl-test-handle-profile-command-disabled-sends-report-to-workspace ()
  "enabled=nil with `workspace' pipes the captured report into that ws's session."
  (let* ((fake-buf (generate-new-buffer " *test-profile-report*"))
         (sent nil))
    (unwind-protect
        (progn
          (with-current-buffer fake-buf
            (setq major-mode 'profiler-report-mode)
            (insert "calltree contents"))
          (cl-letf (((symbol-function 'profiler-running-p) (lambda () t))
                    ((symbol-function 'profiler-stop) (lambda () nil))
                    ((symbol-function 'profiler-report) (lambda () nil))
                    ((symbol-function 'claude-repl--profile-report-buffers)
                     (let ((calls 0))
                       (lambda ()
                         (cl-incf calls)
                         (if (= calls 1) nil (list fake-buf)))))
                    ((symbol-function 'claude-repl--send)
                     (lambda (prompt ws &rest _) (push (cons ws prompt) sent))))
            (claude-repl--handle-profile-command
             '((type . "profile") (enabled . :json-false) (workspace . "ws1"))))
          (should (= 1 (length sent)))
          (should (equal "ws1" (car (car sent))))
          (should (string-match-p "calltree contents" (cdr (car sent))))
          (should (string-match-p "Profiler report" (cdr (car sent)))))
      (when (buffer-live-p fake-buf) (kill-buffer fake-buf)))))

(ert-deftest claude-repl-test-handle-profile-command-disabled-empty-report-no-send ()
  "enabled=nil with empty captured report does not dispatch a send."
  (let ((sent nil))
    (cl-letf (((symbol-function 'profiler-running-p) (lambda () t))
              ((symbol-function 'profiler-stop) (lambda () nil))
              ((symbol-function 'profiler-report) (lambda () nil))
              ((symbol-function 'claude-repl--profile-report-buffers) (lambda () nil))
              ((symbol-function 'claude-repl--send)
               (lambda (&rest args) (push args sent))))
      (claude-repl--handle-profile-command
       '((type . "profile") (enabled . :json-false) (workspace . "ws1")))
      (should-not sent))))

(ert-deftest claude-repl-test-handle-profile-command-disabled-empty-workspace-no-send ()
  "enabled=nil with empty-string `workspace' does not dispatch a send."
  (let* ((fake-buf (generate-new-buffer " *test-profile-report-2*"))
         (sent nil))
    (unwind-protect
        (progn
          (with-current-buffer fake-buf
            (setq major-mode 'profiler-report-mode)
            (insert "contents"))
          (cl-letf (((symbol-function 'profiler-running-p) (lambda () t))
                    ((symbol-function 'profiler-stop) (lambda () nil))
                    ((symbol-function 'profiler-report) (lambda () nil))
                    ((symbol-function 'claude-repl--profile-report-buffers)
                     (let ((calls 0))
                       (lambda ()
                         (cl-incf calls)
                         (if (= calls 1) nil (list fake-buf)))))
                    ((symbol-function 'claude-repl--send)
                     (lambda (&rest args) (push args sent))))
            (claude-repl--handle-profile-command
             '((type . "profile") (enabled . :json-false) (workspace . ""))))
          (should-not sent))
      (when (buffer-live-p fake-buf) (kill-buffer fake-buf)))))

;;;; ---- Tests: profile-stop-and-collect ----

(ert-deftest claude-repl-test-profile-stop-and-collect-returns-new-buffer-text ()
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
                    ((symbol-function 'claude-repl--profile-report-buffers)
                     (let ((calls 0))
                       (lambda ()
                         (cl-incf calls)
                         (if (= calls 1)
                             (list old-buf)
                           (list old-buf new-buf))))))
            (let ((text (claude-repl--profile-stop-and-collect)))
              (should (string-match-p "NEW" text))
              (should-not (string-match-p "OLD" text)))))
      (when (buffer-live-p old-buf) (kill-buffer old-buf))
      (when (buffer-live-p new-buf) (kill-buffer new-buf)))))

(ert-deftest claude-repl-test-profile-stop-and-collect-empty-when-no-new-buffers ()
  "When no new profiler-report buffer is created, returns the empty string."
  (cl-letf (((symbol-function 'profiler-stop) (lambda () nil))
            ((symbol-function 'profiler-report) (lambda () nil))
            ((symbol-function 'claude-repl--profile-report-buffers) (lambda () nil)))
    (should (string= "" (claude-repl--profile-stop-and-collect)))))

(ert-deftest claude-repl-test-profile-stop-and-collect-suppresses-report-window ()
  "`profiler-report' runs with a no-window `display-buffer-overriding-action'.
Guards against regressing the user-visible behavior fix: the profiler
report buffer must still be created (so we can scrape its text) but no
window should pop up for the user, since the report is only forwarded
back to the requesting Claude session."
  (let ((seen-action :unset))
    (cl-letf (((symbol-function 'profiler-stop) (lambda () nil))
              ((symbol-function 'profiler-report)
               (lambda () (setq seen-action display-buffer-overriding-action)))
              ((symbol-function 'claude-repl--profile-report-buffers) (lambda () nil)))
      (claude-repl--profile-stop-and-collect)
      (should (equal seen-action
                     '(display-buffer-no-window . ((allow-no-window . t))))))))

;;;; ---- Tests: profile-fully-expand-buffer ----

(ert-deftest claude-repl-test-profile-fully-expand-buffer-calls-expand-on-each-line ()
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
            (claude-repl--profile-fully-expand-buffer buf))
          (should (= (length calls) 3))
          (should (cl-every #'identity calls)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest claude-repl-test-profile-fully-expand-buffer-noop-on-dead-buffer ()
  "Dead buffer is a no-op — expander returns without signaling or calling expand."
  (let ((buf (generate-new-buffer " *test-expand-dead*"))
        (called nil))
    (kill-buffer buf)
    (cl-letf (((symbol-function 'profiler-report-expand-entry)
               (lambda (&optional _) (setq called t))))
      (claude-repl--profile-fully-expand-buffer buf))
    (should-not called)))

(ert-deftest claude-repl-test-profile-fully-expand-buffer-noop-on-empty-buffer ()
  "Empty buffer is a no-op — eobp is true at point-min, loop body never runs."
  (let ((buf (generate-new-buffer " *test-expand-empty*"))
        (called nil))
    (unwind-protect
        (cl-letf (((symbol-function 'profiler-report-expand-entry)
                   (lambda (&optional _) (setq called t))))
          (claude-repl--profile-fully-expand-buffer buf))
      (when (buffer-live-p buf) (kill-buffer buf)))
    (should-not called)))

(ert-deftest claude-repl-test-profile-stop-and-collect-expands-new-buffer ()
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
                    ((symbol-function 'claude-repl--profile-report-buffers)
                     (let ((calls 0))
                       (lambda ()
                         (cl-incf calls)
                         (if (= calls 1) nil (list new-buf)))))
                    ((symbol-function 'claude-repl--profile-fully-expand-buffer)
                     (lambda (b)
                       (push b expand-calls)
                       (with-current-buffer b
                         (goto-char (point-max))
                         (insert " EXPANDED")))))
            (let ((text (claude-repl--profile-stop-and-collect)))
              (should (= (length expand-calls) 1))
              (should (eq (car expand-calls) new-buf))
              (should (string-match-p "collapsed EXPANDED" text)))))
      (when (buffer-live-p new-buf) (kill-buffer new-buf)))))

;;;; ---- Tests: profile-format-prompt ----

(ert-deftest claude-repl-test-profile-format-prompt-wraps-in-fenced-block ()
  "Format prompt wraps the report text in a fenced code block with a preamble."
  (let ((formatted (claude-repl--profile-format-prompt "abc")))
    (should (string-match-p "Profiler report" formatted))
    (should (string-match-p "```\nabc\n```" formatted))))

(ert-deftest claude-repl-test-handle-profile-command-unknown-mode-refuses-start ()
  "An unknown `mode' string refuses the start — profiler-start is NOT called."
  (let ((started nil))
    (cl-letf (((symbol-function 'profiler-running-p) (lambda () nil))
              ((symbol-function 'profiler-start)
               (lambda (_m) (setq started t)))
              ((symbol-function 'profiler-stop) (lambda () (error "should not stop")))
              ((symbol-function 'profiler-report) (lambda () (error "should not report"))))
      (claude-repl--handle-profile-command
       '((type . "profile") (enabled . t) (mode . "gpu")))
      (should-not started))))

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

;;;; ---- Tests: close-workspace ----

(ert-deftest claude-repl-test-close-workspace-delegates-to-nuke ()
  "`--close-workspace' delegates to `--nuke-one-workspace' with the same ws."
  (let ((received :unset))
    (cl-letf (((symbol-function 'claude-repl--nuke-one-workspace)
               (lambda (ws &optional _preserve)
                 (setq received ws))))
      (claude-repl--close-workspace "feature-one")
      (should (equal received "feature-one")))))

(ert-deftest claude-repl-test-close-workspace-default-drops-entry ()
  "`--close-workspace' without PRESERVE-ENTRY passes nil to the nuke primitive.
Standalone close (skill dispatch path) should fully drop the registry
entry — merge's preserve-entry behavior is opt-in only."
  (let ((received-preserve :unset))
    (cl-letf (((symbol-function 'claude-repl--nuke-one-workspace)
               (lambda (_ws &optional preserve)
                 (setq received-preserve preserve))))
      (claude-repl--close-workspace "feature-one")
      (should (null received-preserve)))))

(ert-deftest claude-repl-test-close-workspace-preserve-entry-passes-through ()
  "`--close-workspace' threads PRESERVE-ENTRY to the nuke primitive.
This is the merge-completion path: the hashmap entry must survive close
so the drawer's MERGED bucket can keep rendering until explicit finish."
  (let ((received-preserve :unset))
    (cl-letf (((symbol-function 'claude-repl--nuke-one-workspace)
               (lambda (_ws &optional preserve)
                 (setq received-preserve preserve))))
      (claude-repl--close-workspace "feature-one" 'preserve-entry)
      (should (eq received-preserve 'preserve-entry)))))

;;;; ---- Tests: handle-close-command ----

(ert-deftest claude-repl-test-handle-close-command-invokes-close ()
  "`--handle-close-command' invokes `--close-workspace' with the ws from CMD."
  (let ((received :unset))
    (cl-letf (((symbol-function 'claude-repl--close-workspace)
               (lambda (ws &optional _preserve) (setq received ws))))
      (claude-repl--handle-close-command
       '((type . "close") (workspace . "feature-one")))
      (should (equal received "feature-one")))))

(ert-deftest claude-repl-test-handle-close-command-no-preserve ()
  "`--handle-close-command' does NOT pass `preserve-entry'.
Skill-invoked close fully drops the workspace; preserve-entry is the
merge-completion-only behavior owned by `--workspace-merge-do'."
  (let ((received-preserve :unset))
    (cl-letf (((symbol-function 'claude-repl--close-workspace)
               (lambda (_ws &optional preserve)
                 (setq received-preserve preserve))))
      (claude-repl--handle-close-command
       '((type . "close") (workspace . "feature-one")))
      (should (null received-preserve)))))

(ert-deftest claude-repl-test-handle-close-command-routes-through-gns-gating ()
  "`--handle-close-command' must dispatch via `--gns-sockets-close-then'
so the in-workspace Claude is sent `/gns-sockets close' and given a
chance to release sockets before its vterm dies."
  (let ((gating-ws :unset)
        (gating-teardown :unset))
    (cl-letf (((symbol-function 'claude-repl--gns-sockets-close-then)
               (lambda (ws teardown-fn)
                 (setq gating-ws ws
                       gating-teardown teardown-fn))))
      (claude-repl--handle-close-command
       '((type . "close") (workspace . "feature-one")))
      (should (equal gating-ws "feature-one"))
      (should (functionp gating-teardown)))))

(ert-deftest claude-repl-test-handle-close-command-teardown-thunk-closes ()
  "The teardown thunk forwarded to `--gns-sockets-close-then' must call
`--close-workspace' with the workspace name when invoked."
  (let ((received :unset)
        (teardown-fn nil))
    (cl-letf (((symbol-function 'claude-repl--gns-sockets-close-then)
               (lambda (_ws fn) (setq teardown-fn fn)))
              ((symbol-function 'claude-repl--close-workspace)
               (lambda (ws &optional _preserve) (setq received ws))))
      (claude-repl--handle-close-command
       '((type . "close") (workspace . "feature-one")))
      (funcall teardown-fn)
      (should (equal received "feature-one")))))

;;;; ---- Tests: gns-sockets-close-then ----

(ert-deftest claude-repl-test-gns-sockets-close-then-no-vterm-runs-teardown-directly ()
  "Without a live vterm buffer, `--gns-sockets-close-then' must run the
teardown thunk immediately — there is no Claude to drain."
  (claude-repl-test--with-clean-state
    (puthash "ws" '() claude-repl--workspaces)
    (let ((called nil)
          (sent nil))
      (cl-letf (((symbol-function 'claude-repl--send)
                 (lambda (&rest _) (setq sent t))))
        (claude-repl--gns-sockets-close-then
         "ws" (lambda () (setq called t)))
        (should called)
        (should-not sent)))))

(ert-deftest claude-repl-test-gns-sockets-close-then-not-ready-runs-teardown-directly ()
  "A live vterm buffer that has not yet set `claude-repl--ready' must
still fall through to immediate teardown — the prompt would otherwise
queue on `:pending-prompts' and never drain before close."
  (claude-repl-test--with-clean-state
    (let ((buf (generate-new-buffer " *test-vterm*"))
          (called nil)
          (sent nil))
      (unwind-protect
          (progn
            (with-current-buffer buf
              (setq-local claude-repl--ready nil))
            (puthash "ws" (list :vterm-buffer buf) claude-repl--workspaces)
            (cl-letf (((symbol-function 'claude-repl--send)
                       (lambda (&rest _) (setq sent t))))
              (claude-repl--gns-sockets-close-then
               "ws" (lambda () (setq called t)))
              (should called)
              (should-not sent)))
        (kill-buffer buf)))))

(ert-deftest claude-repl-test-gns-sockets-close-then-ready-sends-prompt ()
  "With a live, ready vterm, `--gns-sockets-close-then' must dispatch
`claude-repl-gns-sockets-close-prompt' via `--send' and defer teardown."
  (claude-repl-test--with-clean-state
    (let ((buf (generate-new-buffer " *test-vterm*"))
          (sent-prompt :unset)
          (sent-ws :unset)
          (teardown-called nil))
      (unwind-protect
          (progn
            (with-current-buffer buf
              (setq-local claude-repl--ready t))
            (puthash "ws" (list :vterm-buffer buf) claude-repl--workspaces)
            (cl-letf (((symbol-function 'claude-repl--send)
                       (lambda (prompt ws &optional _force _on-settle)
                         (setq sent-prompt prompt
                               sent-ws ws)))
                      ((symbol-function 'run-at-time)
                       (lambda (&rest _) nil)))
              (claude-repl--gns-sockets-close-then
               "ws" (lambda () (setq teardown-called t)))
              (should (equal sent-prompt claude-repl-gns-sockets-close-prompt))
              (should (equal sent-ws "ws"))
              (should-not teardown-called)))
        (kill-buffer buf)))))

(ert-deftest claude-repl-test-gns-sockets-close-then-on-settle-schedules-poll ()
  "The `on-settle' callback handed to `--send' must schedule the first
`--gns-sockets-close-poll' via `run-at-time' so the prompt_submit hook
has time to fire before state is polled."
  (claude-repl-test--with-clean-state
    (let ((buf (generate-new-buffer " *test-vterm*"))
          (scheduled-fn :unset)
          (scheduled-delay :unset)
          (captured-on-settle nil))
      (unwind-protect
          (progn
            (with-current-buffer buf
              (setq-local claude-repl--ready t))
            (puthash "ws" (list :vterm-buffer buf) claude-repl--workspaces)
            (cl-letf (((symbol-function 'claude-repl--send)
                       (lambda (_prompt _ws &optional _force on-settle)
                         (setq captured-on-settle on-settle)))
                      ((symbol-function 'run-at-time)
                       (lambda (delay _repeat fn &rest _args)
                         (setq scheduled-delay delay
                               scheduled-fn fn))))
              (claude-repl--gns-sockets-close-then
               "ws" (lambda () nil))
              (should (functionp captured-on-settle))
              (funcall captured-on-settle)
              (should (equal scheduled-delay
                             claude-repl-gns-sockets-close-settle-delay))
              (should (eq scheduled-fn #'claude-repl--gns-sockets-close-poll))))
        (kill-buffer buf)))))

;;;; ---- Tests: gns-sockets-close-poll ----

(ert-deftest claude-repl-test-gns-sockets-close-poll-runs-teardown-on-done ()
  "When `:claude-state' is `:done', the poll must call TEARDOWN-FN
rather than rescheduling."
  (claude-repl-test--with-clean-state
    (puthash "ws" (list :claude-state :done) claude-repl--workspaces)
    (let ((called nil)
          (rescheduled nil))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (&rest _) (setq rescheduled t))))
        (claude-repl--gns-sockets-close-poll
         "ws" (lambda () (setq called t)) (float-time))
        (should called)
        (should-not rescheduled)))))

(ert-deftest claude-repl-test-gns-sockets-close-poll-runs-teardown-on-idle ()
  "`:idle' is also a terminal state for the poll — the workspace has
decayed from `:done' but the turn is still finished, so it is safe to
tear down."
  (claude-repl-test--with-clean-state
    (puthash "ws" (list :claude-state :idle) claude-repl--workspaces)
    (let ((called nil)
          (rescheduled nil))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (&rest _) (setq rescheduled t))))
        (claude-repl--gns-sockets-close-poll
         "ws" (lambda () (setq called t)) (float-time))
        (should called)
        (should-not rescheduled)))))

(ert-deftest claude-repl-test-gns-sockets-close-poll-reschedules-on-thinking ()
  "When the workspace is still `:thinking', the poll must reschedule
itself via `run-at-time' with the configured poll interval and must
NOT call TEARDOWN-FN."
  (claude-repl-test--with-clean-state
    (puthash "ws" (list :claude-state :thinking) claude-repl--workspaces)
    (let ((called nil)
          (rescheduled-delay :unset)
          (rescheduled-fn :unset))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (delay _repeat fn &rest _args)
                   (setq rescheduled-delay delay
                         rescheduled-fn fn))))
        (claude-repl--gns-sockets-close-poll
         "ws" (lambda () (setq called t)) (float-time))
        (should-not called)
        (should (equal rescheduled-delay
                       claude-repl-gns-sockets-close-poll-interval))
        (should (eq rescheduled-fn #'claude-repl--gns-sockets-close-poll))))))

(ert-deftest claude-repl-test-gns-sockets-close-poll-times-out ()
  "Once `claude-repl-gns-sockets-close-timeout' seconds have elapsed
without reaching `:done'/`:idle', the poll must call TEARDOWN-FN
anyway — a hung session must not stall close indefinitely."
  (claude-repl-test--with-clean-state
    (puthash "ws" (list :claude-state :thinking) claude-repl--workspaces)
    (let ((called nil)
          (rescheduled nil)
          (started-at (- (float-time)
                         (+ claude-repl-gns-sockets-close-timeout 1.0))))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (&rest _) (setq rescheduled t))))
        (claude-repl--gns-sockets-close-poll
         "ws" (lambda () (setq called t)) started-at)
        (should called)
        (should-not rescheduled)))))

;;;; ---- Tests: handle-merge-command ----

(ert-deftest claude-repl-test-handle-merge-command-literal-match ()
  "Literal workspace name with a registered :project-dir is forwarded as-is."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "feature-one" :project-dir "/tmp/feature-one")
    (let ((received :unset))
      (cl-letf (((symbol-function 'claude-repl--workspace-merge-into-source)
                 (lambda (ws &optional _silent _auto) (setq received ws))))
        (claude-repl--handle-merge-command
         '((type . "merge") (workspace . "feature-one")))
        (should (equal received "feature-one"))))))

(ert-deftest claude-repl-test-handle-merge-command-falls-back-to-tail ()
  "Branch-style \"DWC/foo\" falls back to \"foo\" when only \"foo\" is registered.
Resolves the bare tail after the last `/' so the spawning agent can send
its branch name verbatim without pre-stripping it."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "foo" :project-dir "/tmp/foo")
    (let ((received :unset))
      (cl-letf (((symbol-function 'claude-repl--workspace-merge-into-source)
                 (lambda (ws &optional _silent _auto) (setq received ws))))
        (claude-repl--handle-merge-command
         '((type . "merge") (workspace . "DWC/foo")))
        (should (equal received "foo"))))))

(ert-deftest claude-repl-test-handle-merge-command-unknown-name-no-crash ()
  "Unknown name (neither literal nor tail registered) does not invoke
the merge and does not crash."
  (claude-repl-test--with-clean-state
    (let ((called nil))
      (cl-letf (((symbol-function 'claude-repl--workspace-merge-into-source)
                 (lambda (&rest _) (setq called t))))
        ;; Must not error.
        (claude-repl--handle-merge-command
         '((type . "merge") (workspace . "bar/baz")))
        (should-not called)))))

(ert-deftest claude-repl-test-handle-merge-command-unknown-name-logs ()
  "Unknown workspace triggers an `unknown workspace' log line that
includes both the literal name and the tail that was tried."
  (claude-repl-test--with-clean-state
    (let ((logged nil))
      (cl-letf (((symbol-function 'claude-repl--log)
                 (lambda (_ws fmt &rest args)
                   (push (apply #'format fmt args) logged)))
                ((symbol-function 'claude-repl--workspace-merge-into-source)
                 (lambda (&rest _) nil)))
        (claude-repl--handle-merge-command
         '((type . "merge") (workspace . "bar/baz")))
        (should (cl-some (lambda (s)
                           (and (string-match-p "unknown workspace: bar/baz" s)
                                (string-match-p "also tried tail baz" s)))
                         logged))))))

(ert-deftest claude-repl-test-handle-merge-command-runs-silently ()
  "Skill-invoked merges (`/workspace-merge') must pass SILENT=t to
workspace-merge-into-source so the merge does not steal user focus.
Interactive entries (`SPC TAB m'/`SPC TAB M') leave SILENT nil and
retain the old switch-to-project + magit pop behavior."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "feature-one" :project-dir "/tmp/feature-one")
    (let ((silent-arg :unset))
      (cl-letf (((symbol-function 'claude-repl--workspace-merge-into-source)
                 (lambda (_ws &optional silent _auto) (setq silent-arg silent))))
        (claude-repl--handle-merge-command
         '((type . "merge") (workspace . "feature-one")))
        (should (eq silent-arg t))))))

;;;; ---- Tests: ws-merge-routing-root ----

(ert-deftest claude-repl-test-ws-merge-routing-root-prefers-source-dir ()
  "Routing root prefers :source-ws-dir when it is a live directory."
  (claude-repl-test--with-clean-state
    (let ((src-dir (make-temp-file "claude-repl-routing-src-" t))
          (own-dir (make-temp-file "claude-repl-routing-own-" t)))
      (unwind-protect
          (progn
            (claude-repl--ws-put "ws1" :project-dir own-dir)
            (claude-repl--ws-put "ws1" :source-ws-dir src-dir)
            (should (equal (claude-repl--ws-merge-routing-root "ws1") src-dir)))
        (delete-directory src-dir t)
        (delete-directory own-dir t)))))

(ert-deftest claude-repl-test-ws-merge-routing-root-falls-back-to-project-dir ()
  "Routing root falls back to :project-dir when :source-ws-dir is nil."
  (claude-repl-test--with-clean-state
    (let ((own-dir (make-temp-file "claude-repl-routing-own-" t)))
      (unwind-protect
          (progn
            (claude-repl--ws-put "ws1" :project-dir own-dir)
            (should (equal (claude-repl--ws-merge-routing-root "ws1") own-dir)))
        (delete-directory own-dir t)))))

(ert-deftest claude-repl-test-ws-merge-routing-root-falls-back-when-source-missing ()
  "When :source-ws-dir is set but the directory doesn't exist, falls back to :project-dir."
  (claude-repl-test--with-clean-state
    (let ((own-dir (make-temp-file "claude-repl-routing-own-" t)))
      (unwind-protect
          (progn
            (claude-repl--ws-put "ws1" :project-dir own-dir)
            (claude-repl--ws-put "ws1" :source-ws-dir "/nonexistent/dir")
            (should (equal (claude-repl--ws-merge-routing-root "ws1") own-dir)))
        (delete-directory own-dir t)))))

(ert-deftest claude-repl-test-ws-merge-routing-root-nil-when-neither ()
  "When neither :source-ws-dir nor :project-dir resolves, returns nil."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :project-dir nil)
    (should-not (claude-repl--ws-merge-routing-root "ws1"))))

;;;; ---- Tests: handle-merge-command dispatch via registry ----

(ert-deftest claude-repl-test-handle-merge-command-dispatches-via-registry ()
  "handle-merge-command routes through `claude-repl--dispatch-merge-handler'.
Mocks the dispatcher and verifies it receives the resolved ws + routing root."
  (claude-repl-test--with-clean-state
    (let ((src-dir (make-temp-file "claude-repl-dispatch-src-" t))
          (captured nil))
      (unwind-protect
          (progn
            (claude-repl--ws-put "feature-one" :project-dir "/tmp/feature-one")
            (claude-repl--ws-put "feature-one" :source-ws-dir src-dir)
            (cl-letf (((symbol-function 'claude-repl--dispatch-merge-handler)
                       (lambda (ws root) (setq captured (list ws root)))))
              (claude-repl--handle-merge-command
               '((type . "merge") (workspace . "feature-one")))
              (should (equal (car captured) "feature-one"))
              (should (equal (cadr captured) src-dir))))
        (delete-directory src-dir t)))))

(ert-deftest claude-repl-test-handle-merge-command-routes-through-async-wrapper ()
  "Skill-invoked merges go through `claude-repl--workspace-merge-async' —
the SAME wrapper the interactive `SPC TAB M' path uses, so there is no
behavioral difference between the two callers.  Both close the workspace
UI, run the merge on a worker thread, and reopen on failure."
  (claude-repl-test--with-clean-state
    (let ((src-dir (make-temp-file "claude-repl-async-sentinel-" t))
          (async-args :unset))
      (unwind-protect
          (progn
            (claude-repl--ws-put "feature-one" :project-dir "/tmp/feature-one")
            (claude-repl--ws-put "feature-one" :source-ws-dir src-dir)
            (cl-letf (((symbol-function 'claude-repl--workspace-merge-async)
                       (lambda (ws root) (setq async-args (list ws root)))))
              (claude-repl--handle-merge-command
               '((type . "merge") (workspace . "feature-one")))
              (should (equal (car async-args) "feature-one"))
              (should (equal (cadr async-args) src-dir))))
        (delete-directory src-dir t)))))

;;;; ---- Tests: resolve-merge-workspace-name ----

(ert-deftest claude-repl-test-resolve-merge-workspace-name-literal ()
  "Literal name with a :project-dir entry returns the literal name."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "foo" :project-dir "/tmp/foo")
    (should (equal (claude-repl--resolve-merge-workspace-name "foo") "foo"))))

(ert-deftest claude-repl-test-resolve-merge-workspace-name-tail-fallback ()
  "Branch-style name returns the tail when only the tail is registered."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "foo" :project-dir "/tmp/foo")
    (should (equal (claude-repl--resolve-merge-workspace-name "DWC/foo") "foo"))))

(ert-deftest claude-repl-test-resolve-merge-workspace-name-literal-wins-over-tail ()
  "When both the literal name and the tail are registered, literal wins."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "DWC/foo" :project-dir "/tmp/DWC-foo")
    (claude-repl--ws-put "foo" :project-dir "/tmp/foo")
    (should (equal (claude-repl--resolve-merge-workspace-name "DWC/foo") "DWC/foo"))))

(ert-deftest claude-repl-test-resolve-merge-workspace-name-miss ()
  "Returns nil when neither the literal name nor the tail is registered."
  (claude-repl-test--with-clean-state
    (should (null (claude-repl--resolve-merge-workspace-name "bar/baz")))))

(ert-deftest claude-repl-test-resolve-merge-workspace-name-no-slash-miss ()
  "Returns nil for an unregistered bare name (no `/' to fall back from)."
  (claude-repl-test--with-clean-state
    (should (null (claude-repl--resolve-merge-workspace-name "nope")))))

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

;;;; ---- Tests: random-disambiguator-suffix ----

(ert-deftest claude-repl-test-random-disambiguator-suffix-length ()
  "Suffix is exactly 3 characters."
  (should (= 3 (length (claude-repl--random-disambiguator-suffix)))))

(ert-deftest claude-repl-test-random-disambiguator-suffix-lowercase ()
  "Suffix contains only lowercase a-z."
  (let ((suffix (claude-repl--random-disambiguator-suffix)))
    (should (string-match-p "\\`[a-z]\\{3\\}\\'" suffix))))

;;;; ---- Tests: workspace-name-collides-p ----

(ert-deftest claude-repl-test-workspace-name-collides-p-fresh ()
  "Fresh name in a clean repo and empty workspaces hash → no collision."
  (let ((claude-repl--workspaces (make-hash-table :test 'equal))
        (claude-repl--workspace-names-in-flight nil)
        (claude-repl-worktree-start-tag-prefix nil))
    (cl-letf (((symbol-function 'claude-repl--candidate-worktree-path)
               (lambda (&rest _args) "/tmp/repo-worktrees/fresh"))
              ((symbol-function 'file-directory-p) (lambda (_p) nil))
              ((symbol-function 'claude-repl--git-exit-code)
               (lambda (&rest _args) 128)))
      (should-not (claude-repl--workspace-name-collides-p "DWC/fresh" "/tmp/repo")))))

(ert-deftest claude-repl-test-workspace-name-collides-p-in-flight ()
  "Name already reserved in `claude-repl--workspace-names-in-flight' → collision."
  (let ((claude-repl--workspaces (make-hash-table :test 'equal))
        (claude-repl--workspace-names-in-flight
         (let ((h (make-hash-table :test 'equal)))
           (puthash "DWC/dup" t h)
           h))
        (claude-repl-worktree-start-tag-prefix nil))
    ;; The production function eagerly computes the candidate path in
    ;; its `let*' before any short-circuit check, so
    ;; `claude-repl--candidate-worktree-path' DOES run and must return
    ;; a string.  But after the in-flight hit wins, no other probe
    ;; (file-directory-p, --git-exit-code) should fire — those stay
    ;; error-on-call.
    (cl-letf (((symbol-function 'claude-repl--candidate-worktree-path)
               (lambda (&rest _args) "/tmp/repo-worktrees/dup"))
              ((symbol-function 'file-directory-p)
               (lambda (&rest _args) (error "unexpected file-directory-p call")))
              ((symbol-function 'claude-repl--git-exit-code)
               (lambda (&rest _args) (error "unexpected git-exit-code call"))))
      (should (claude-repl--workspace-name-collides-p "DWC/dup" "/tmp/repo")))))

(ert-deftest claude-repl-test-workspace-name-collides-p-workspaces-hash ()
  "Bare name present in `claude-repl--workspaces' → collision."
  (let ((claude-repl--workspaces (make-hash-table :test 'equal))
        (claude-repl--workspace-names-in-flight nil)
        (claude-repl-worktree-start-tag-prefix nil))
    ;; Hash table keyed by bare name (matches `(+workspace-current-name)' style).
    (puthash "existing" '(:project-dir "/tmp/x") claude-repl--workspaces)
    (cl-letf (((symbol-function 'claude-repl--candidate-worktree-path)
               (lambda (&rest _args) "/tmp/repo-worktrees/existing"))
              ((symbol-function 'claude-repl--git-exit-code)
               (lambda (&rest _args) (error "unexpected git-exit-code call"))))
      (should (claude-repl--workspace-name-collides-p "DWC/existing" "/tmp/repo")))))

(ert-deftest claude-repl-test-workspace-name-collides-p-on-disk-path ()
  "Existing on-disk path at the resolved worktree dir → collision."
  (let ((claude-repl--workspaces (make-hash-table :test 'equal))
        (claude-repl--workspace-names-in-flight nil)
        (claude-repl-worktree-start-tag-prefix nil))
    (cl-letf (((symbol-function 'claude-repl--candidate-worktree-path)
               (lambda (&rest _args) "/tmp/repo-worktrees/ondisk"))
              ((symbol-function 'file-directory-p)
               (lambda (p) (equal p "/tmp/repo-worktrees/ondisk")))
              ((symbol-function 'claude-repl--git-exit-code)
               (lambda (&rest _args) (error "unexpected git-exit-code call"))))
      (should (claude-repl--workspace-name-collides-p "DWC/ondisk" "/tmp/repo")))))

(ert-deftest claude-repl-test-workspace-name-collides-p-git-branch ()
  "Existing git branch in repo → collision."
  (let ((claude-repl--workspaces (make-hash-table :test 'equal))
        (claude-repl--workspace-names-in-flight nil)
        (claude-repl-worktree-start-tag-prefix nil))
    (cl-letf (((symbol-function 'claude-repl--candidate-worktree-path)
               (lambda (&rest _args) "/tmp/repo-worktrees/existing-branch"))
              ((symbol-function 'file-directory-p) (lambda (_p) nil))
              ((symbol-function 'claude-repl--git-exit-code)
               (lambda (_root &rest args)
                 ;; Only the branch-existence probe is reached here; return 0.
                 (if (equal args '("rev-parse" "--verify" "DWC/existing-branch"))
                     0
                   (error "unexpected git-exit-code args: %S" args)))))
      (should (claude-repl--workspace-name-collides-p "DWC/existing-branch" "/tmp/repo")))))

(ert-deftest claude-repl-test-workspace-name-collides-p-start-tag ()
  "Existing start-tag for the resolved branch → collision."
  (let ((claude-repl--workspaces (make-hash-table :test 'equal))
        (claude-repl--workspace-names-in-flight nil)
        (claude-repl-worktree-start-tag-prefix "start/"))
    (cl-letf (((symbol-function 'claude-repl--candidate-worktree-path)
               (lambda (&rest _args) "/tmp/repo-worktrees/has-tag"))
              ((symbol-function 'file-directory-p) (lambda (_p) nil))
              ((symbol-function 'claude-repl--git-exit-code)
               (lambda (_root &rest args)
                 (cond
                  ;; Branch does not exist.
                  ((equal args '("rev-parse" "--verify" "DWC/has-tag")) 128)
                  ;; Start tag DOES exist.
                  ((equal args '("rev-parse" "--verify" "refs/tags/start/DWC/has-tag")) 0)
                  (t (error "unexpected git-exit-code args: %S" args))))))
      (should (claude-repl--workspace-name-collides-p "DWC/has-tag" "/tmp/repo")))))

(ert-deftest claude-repl-test-workspace-name-collides-p-tag-ignored-when-prefix-nil ()
  "When start-tag prefix is nil, a stray `start/<branch>' tag does not flag collision."
  (let ((claude-repl--workspaces (make-hash-table :test 'equal))
        (claude-repl--workspace-names-in-flight nil)
        (claude-repl-worktree-start-tag-prefix nil))
    (cl-letf (((symbol-function 'claude-repl--candidate-worktree-path)
               (lambda (&rest _args) "/tmp/repo-worktrees/no-tag-check"))
              ((symbol-function 'file-directory-p) (lambda (_p) nil))
              ((symbol-function 'claude-repl--git-exit-code)
               (lambda (_root &rest args)
                 ;; Branch probe is the only git call reached when prefix is nil.
                 (if (equal args '("rev-parse" "--verify" "DWC/no-tag-check")) 128
                   (error "unexpected git-exit-code args (start-tag check should be skipped): %S" args)))))
      (should-not (claude-repl--workspace-name-collides-p "DWC/no-tag-check" "/tmp/repo")))))

;;;; ---- Tests: disambiguate-workspace-name ----

(ert-deftest claude-repl-test-disambiguate-workspace-name-no-collision ()
  "When the name does not collide, it is returned unchanged (no suffix)."
  (cl-letf (((symbol-function 'claude-repl--workspace-name-collides-p)
             (lambda (&rest _args) nil)))
    (should (equal "DWC/clean"
                   (claude-repl--disambiguate-workspace-name "DWC/clean" "/tmp/repo")))))

(ert-deftest claude-repl-test-disambiguate-workspace-name-collides-appends-suffix ()
  "When the name collides, the result is `NAME-XYZ' with a 3-char suffix."
  ;; First call (bare name) collides, subsequent suffixed candidates do not.
  (let ((call-count 0))
    (cl-letf (((symbol-function 'claude-repl--workspace-name-collides-p)
               (lambda (name &rest _args)
                 (cl-incf call-count)
                 (equal name "DWC/taken"))))
      (let ((result (claude-repl--disambiguate-workspace-name "DWC/taken" "/tmp/repo")))
        (should (string-match-p "\\`DWC/taken-[a-z]\\{3\\}\\'" result))))))

(ert-deftest claude-repl-test-disambiguate-workspace-name-errors-when-max-attempts-exceeded ()
  "When every candidate keeps colliding, an error is signaled.
Simulated by stubbing `claude-repl--workspace-name-collides-p' to always
return t — the loop must exit and `error' rather than spin forever or
silently return a colliding name."
  (let ((claude-repl-workspace-name-disambiguate-max-attempts 3))
    (cl-letf (((symbol-function 'claude-repl--workspace-name-collides-p)
               (lambda (&rest _args) t)))
      (should-error (claude-repl--disambiguate-workspace-name "DWC/x" "/tmp/repo")))))

;;;; ---- Tests: handle-create-command disambiguation integration ----

(ert-deftest claude-repl-test-handle-create-command-passes-clean-name-through ()
  "When the desired name does not collide, the timer is scheduled with the original name."
  (let ((claude-repl--workspaces (make-hash-table :test 'equal))
        (claude-repl--workspace-names-in-flight (make-hash-table :test 'equal))
        (claude-repl-worktree-start-tag-prefix nil)
        (scheduled-args nil))
    (cl-letf (((symbol-function 'claude-repl--workspace-name-collides-p)
               (lambda (&rest _args) nil))
              ((symbol-function 'run-with-timer)
               (lambda (_delay _repeat _fn &rest args)
                 (setq scheduled-args args))))
      (claude-repl--handle-create-command
       `((type . "create") (name . "DWC/clean") (git_root . "/tmp/repo"))
       0)
      ;; Args are (git-root name prompt priority fork-session-id base-commit)
      (should (equal "DWC/clean" (nth 1 scheduled-args))))))

(ert-deftest claude-repl-test-handle-create-command-disambiguates-collision ()
  "When the desired name collides (existing branch), the timer is scheduled with a suffixed name."
  (let ((claude-repl--workspaces (make-hash-table :test 'equal))
        (claude-repl--workspace-names-in-flight (make-hash-table :test 'equal))
        (claude-repl-worktree-start-tag-prefix nil)
        (scheduled-args nil))
    (cl-letf (((symbol-function 'claude-repl--workspace-name-collides-p)
               (lambda (name &rest _args)
                 ;; Only the bare name collides; suffixed variants do not.
                 (equal name "DWC/taken")))
              ((symbol-function 'run-with-timer)
               (lambda (_delay _repeat _fn &rest args)
                 (setq scheduled-args args))))
      (claude-repl--handle-create-command
       `((type . "create") (name . "DWC/taken") (git_root . "/tmp/repo"))
       0)
      (should (string-match-p "\\`DWC/taken-[a-z]\\{3\\}\\'"
                              (nth 1 scheduled-args))))))

(ert-deftest claude-repl-test-handle-create-command-reserves-name-in-flight ()
  "After scheduling, the effective name is recorded in the in-flight hash so siblings see it."
  (let ((claude-repl--workspaces (make-hash-table :test 'equal))
        (claude-repl--workspace-names-in-flight (make-hash-table :test 'equal))
        (claude-repl-worktree-start-tag-prefix nil))
    (cl-letf (((symbol-function 'claude-repl--workspace-name-collides-p)
               (lambda (&rest _args) nil))
              ((symbol-function 'run-with-timer)
               (lambda (&rest _args) nil)))
      (claude-repl--handle-create-command
       `((type . "create") (name . "DWC/sibling") (git_root . "/tmp/repo"))
       0))
    (should (gethash "DWC/sibling" claude-repl--workspace-names-in-flight))))

(ert-deftest claude-repl-test-handle-create-command-second-sibling-gets-suffix ()
  "Two sibling creates in the same batch with the same name yield distinct effective names."
  (let ((claude-repl--workspaces (make-hash-table :test 'equal))
        (claude-repl--workspace-names-in-flight (make-hash-table :test 'equal))
        (claude-repl-worktree-start-tag-prefix nil)
        (scheduled-names nil))
    ;; First create: name is collision-free.  The handler reserves it in
    ;; `--workspace-names-in-flight'.  Second create with the same name
    ;; consults that table via the real `--workspace-name-collides-p',
    ;; so it MUST find the prior reservation and disambiguate.  We mock
    ;; only the slower path probes (path/branch/start-tag) and let the
    ;; in-flight check fall through to the real implementation.
    (cl-letf (((symbol-function 'claude-repl--candidate-worktree-path)
               (lambda (&rest _args) "/tmp/repo-worktrees/dup"))
              ((symbol-function 'file-directory-p) (lambda (_p) nil))
              ((symbol-function 'claude-repl--git-exit-code)
               (lambda (&rest _args) 128))
              ((symbol-function 'run-with-timer)
               (lambda (_delay _repeat _fn &rest args)
                 (push (nth 1 args) scheduled-names))))
      (claude-repl--handle-create-command
       `((type . "create") (name . "DWC/dup") (git_root . "/tmp/repo"))
       0)
      (claude-repl--handle-create-command
       `((type . "create") (name . "DWC/dup") (git_root . "/tmp/repo"))
       5))
    (setq scheduled-names (nreverse scheduled-names))
    (should (= 2 (length scheduled-names)))
    (should (equal "DWC/dup" (nth 0 scheduled-names)))
    (should (string-match-p "\\`DWC/dup-[a-z]\\{3\\}\\'" (nth 1 scheduled-names)))
    (should-not (equal (nth 0 scheduled-names) (nth 1 scheduled-names)))))

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

;;;; ---- Tests: worktree-fetch-master-callback ----

(ert-deftest claude-repl-test-worktree-fetch-master-callback-calls-ff-then-add-fn ()
  "Master fetch callback calls ff-master with git-root, then invokes add-fn."
  (let ((ff-called-with nil)
        (add-called nil))
    (cl-letf (((symbol-function 'claude-repl--maybe-fast-forward-master)
               (lambda (root) (setq ff-called-with root))))
      (claude-repl--worktree-fetch-master-callback
       (lambda () (setq add-called t)) "/some/root" t "output"))
    (should (equal ff-called-with "/some/root"))
    (should add-called)))

(ert-deftest claude-repl-test-worktree-fetch-master-callback-calls-add-fn-on-failure ()
  "Master fetch callback still calls add-fn when fetch reports failure."
  (let ((add-called nil))
    (cl-letf (((symbol-function 'claude-repl--maybe-fast-forward-master)
               (lambda (_root) nil)))
      (claude-repl--worktree-fetch-master-callback
       (lambda () (setq add-called t)) "/some/root" nil "error"))
    (should add-called)))

;;;; ---- Tests: maybe-fast-forward-master ----
;;
;; The production function calls four boundary primitives:
;; - `claude-repl--git-exit-code' for: verify origin-ref, branch-exists-p,
;;   merge-base --is-ancestor, merge --ff-only (when wt is on master), and
;;   update-ref (when wt is not on master).
;; - `claude-repl--git-string' for `rev-parse <branch>' and `rev-parse <origin>'.
;; - `claude-repl--master-worktree-path' to discover the master worktree, if any.
;; All four are stubbed so the tests exercise the dispatch logic, not git.

(ert-deftest claude-repl-test-maybe-ff-master-advances-when-behind ()
  "Local master strictly behind origin/master is fast-forwarded (no wt on master)."
  (let ((claude-repl-master-branch-name "master")
        (update-ref-args nil))
    (cl-letf (((symbol-function 'claude-repl--git-exit-code)
               (lambda (_root &rest args)
                 (pcase args
                   (`("rev-parse" "--verify" "--quiet" "origin/master") 0)
                   (`("rev-parse" "--verify" "master") 0) ; via --git-branch-exists-p
                   (`("merge-base" "--is-ancestor" "master" "origin/master") 0)
                   (`("update-ref" "refs/heads/master" "refs/remotes/origin/master")
                    (setq update-ref-args args)
                    0)
                   (_ (error "unmocked git-exit-code args: %S" args)))))
              ((symbol-function 'claude-repl--git-string)
               (lambda (&rest args)
                 (pcase args
                   (`("-C" "/tmp/repo" "rev-parse" "master")
                    "1111111111111111111111111111111111111111")
                   (`("-C" "/tmp/repo" "rev-parse" "origin/master")
                    "2222222222222222222222222222222222222222")
                   (_ (error "unmocked git-string args: %S" args)))))
              ((symbol-function 'claude-repl--master-worktree-path)
               (lambda (_root) nil)))
      (claude-repl--maybe-fast-forward-master "/tmp/repo")
      (should (equal update-ref-args
                     '("update-ref" "refs/heads/master" "refs/remotes/origin/master"))))))

(ert-deftest claude-repl-test-maybe-ff-master-noop-when-diverged ()
  "Local master with commits origin/master lacks is NOT reset."
  (let ((claude-repl-master-branch-name "master")
        (update-ref-called nil)
        (merge-called nil))
    (cl-letf (((symbol-function 'claude-repl--git-exit-code)
               (lambda (_root &rest args)
                 (pcase args
                   (`("rev-parse" "--verify" "--quiet" "origin/master") 0)
                   (`("rev-parse" "--verify" "master") 0)
                   ;; Non-zero means local master is NOT an ancestor — diverged.
                   (`("merge-base" "--is-ancestor" "master" "origin/master") 1)
                   (`("update-ref" . ,_) (setq update-ref-called t) 0)
                   (`("merge" . ,_) (setq merge-called t) 0)
                   (_ (error "unmocked git-exit-code args: %S" args)))))
              ((symbol-function 'claude-repl--git-string)
               (lambda (&rest args) (error "unmocked git-string args: %S" args)))
              ((symbol-function 'claude-repl--master-worktree-path)
               (lambda (_root) nil)))
      (claude-repl--maybe-fast-forward-master "/tmp/repo")
      (should-not update-ref-called)
      (should-not merge-called))))

(ert-deftest claude-repl-test-maybe-ff-master-noop-when-equal ()
  "When master == origin/master, the ref is unchanged."
  (let ((claude-repl-master-branch-name "master")
        (update-ref-called nil)
        (merge-called nil))
    (cl-letf (((symbol-function 'claude-repl--git-exit-code)
               (lambda (_root &rest args)
                 (pcase args
                   (`("rev-parse" "--verify" "--quiet" "origin/master") 0)
                   (`("rev-parse" "--verify" "master") 0)
                   (`("merge-base" "--is-ancestor" "master" "origin/master") 0)
                   (`("update-ref" . ,_) (setq update-ref-called t) 0)
                   (`("merge" . ,_) (setq merge-called t) 0)
                   (_ (error "unmocked git-exit-code args: %S" args)))))
              ((symbol-function 'claude-repl--git-string)
               (lambda (&rest args)
                 ;; Same SHA on both sides → equal branch.
                 (pcase args
                   (`("-C" "/tmp/repo" "rev-parse" "master")
                    "abcdef0123456789abcdef0123456789abcdef01")
                   (`("-C" "/tmp/repo" "rev-parse" "origin/master")
                    "abcdef0123456789abcdef0123456789abcdef01")
                   (_ (error "unmocked git-string args: %S" args)))))
              ((symbol-function 'claude-repl--master-worktree-path)
               (lambda (_root) nil)))
      (claude-repl--maybe-fast-forward-master "/tmp/repo")
      (should-not update-ref-called)
      (should-not merge-called))))

(ert-deftest claude-repl-test-maybe-ff-master-noop-when-origin-missing ()
  "No origin/master ref → function is a no-op (no error, master unchanged)."
  (let ((claude-repl-master-branch-name "master")
        (other-calls 0))
    (cl-letf (((symbol-function 'claude-repl--git-exit-code)
               (lambda (_root &rest args)
                 (pcase args
                   ;; Origin verify FAILS — first cond branch fires, function returns.
                   (`("rev-parse" "--verify" "--quiet" "origin/master") 128)
                   (_ (cl-incf other-calls) 0)))))
      (claude-repl--maybe-fast-forward-master "/tmp/repo")
      (should (= other-calls 0)))))

(ert-deftest claude-repl-test-maybe-ff-master-noop-when-local-master-missing ()
  "No local master branch → function is a no-op (no error)."
  (let ((claude-repl-master-branch-name "master")
        (extra-calls 0))
    (cl-letf (((symbol-function 'claude-repl--git-exit-code)
               (lambda (_root &rest args)
                 (pcase args
                   (`("rev-parse" "--verify" "--quiet" "origin/master") 0)
                   ;; Branch-exists-p says master is missing — second cond branch.
                   (`("rev-parse" "--verify" "master") 128)
                   (_ (cl-incf extra-calls) 0)))))
      (claude-repl--maybe-fast-forward-master "/tmp/repo")
      (should (= extra-calls 0)))))

(ert-deftest claude-repl-test-maybe-ff-master-advances-when-checked-out ()
  "When master is checked out, ff happens via `merge --ff-only' in that worktree."
  (let ((claude-repl-master-branch-name "master")
        (merge-call nil)
        (update-ref-called nil))
    (cl-letf (((symbol-function 'claude-repl--git-exit-code)
               (lambda (root &rest args)
                 (pcase args
                   (`("rev-parse" "--verify" "--quiet" "origin/master") 0)
                   (`("rev-parse" "--verify" "master") 0)
                   (`("merge-base" "--is-ancestor" "master" "origin/master") 0)
                   (`("merge" "--ff-only" "origin/master")
                    (setq merge-call (list root args))
                    0)
                   (`("update-ref" . ,_) (setq update-ref-called t) 0)
                   (_ (error "unmocked git-exit-code args: %S" args)))))
              ((symbol-function 'claude-repl--git-string)
               (lambda (&rest args)
                 (pcase args
                   (`("-C" "/tmp/repo" "rev-parse" "master")
                    "1111111111111111111111111111111111111111")
                   (`("-C" "/tmp/repo" "rev-parse" "origin/master")
                    "2222222222222222222222222222222222222222")
                   (_ (error "unmocked git-string args: %S" args)))))
              ((symbol-function 'claude-repl--master-worktree-path)
               (lambda (_root) "/tmp/master-wt")))
      (claude-repl--maybe-fast-forward-master "/tmp/repo")
      (should (equal merge-call
                     (list "/tmp/master-wt"
                           '("merge" "--ff-only" "origin/master"))))
      (should-not update-ref-called))))

(ert-deftest claude-repl-test-maybe-ff-master-honors-custom-branch-name ()
  "`claude-repl-master-branch-name' selects which local/remote pair to ff."
  (let ((claude-repl-master-branch-name "trunk")
        (update-ref-args nil))
    (cl-letf (((symbol-function 'claude-repl--git-exit-code)
               (lambda (_root &rest args)
                 (pcase args
                   (`("rev-parse" "--verify" "--quiet" "origin/trunk") 0)
                   (`("rev-parse" "--verify" "trunk") 0)
                   (`("merge-base" "--is-ancestor" "trunk" "origin/trunk") 0)
                   (`("update-ref" "refs/heads/trunk" "refs/remotes/origin/trunk")
                    (setq update-ref-args args)
                    0)
                   (_ (error "unmocked git-exit-code args: %S" args)))))
              ((symbol-function 'claude-repl--git-string)
               (lambda (&rest args)
                 (pcase args
                   (`("-C" "/tmp/repo" "rev-parse" "trunk")
                    "1111111111111111111111111111111111111111")
                   (`("-C" "/tmp/repo" "rev-parse" "origin/trunk")
                    "2222222222222222222222222222222222222222")
                   (_ (error "unmocked git-string args: %S" args)))))
              ((symbol-function 'claude-repl--master-worktree-path)
               (lambda (_root) nil)))
      (claude-repl--maybe-fast-forward-master "/tmp/repo")
      (should (equal update-ref-args
                     '("update-ref" "refs/heads/trunk" "refs/remotes/origin/trunk"))))))

;;;; ---- Tests: validate-worktree-creation ----

(ert-deftest claude-repl-test-validate-worktree-creation-empty-name ()
  "Empty name signals user-error."
  (should-error (claude-repl--validate-worktree-creation "" "/root" "dir" "branch" "/path")
                :type 'user-error))

(ert-deftest claude-repl-test-validate-worktree-creation-existing-path ()
  "An existing directory at PATH signals user-error."
  (cl-letf (((symbol-function 'file-directory-p)
             (lambda (p) (equal p "/tmp/repo")))
            ((symbol-function 'claude-repl--git-exit-code)
             (lambda (&rest _args) (error "should not probe git when path check fires"))))
    (should-error (claude-repl--validate-worktree-creation
                   "name" "/tmp/repo" "dir" "branch" "/tmp/repo")
                  :type 'user-error)))

(ert-deftest claude-repl-test-validate-worktree-creation-existing-branch ()
  "Existing branch signals user-error."
  (cl-letf (((symbol-function 'file-directory-p) (lambda (_p) nil))
            ((symbol-function 'claude-repl--git-exit-code)
             (lambda (_root &rest args)
               ;; Branch-exists-p probe: returning 0 means the branch exists.
               (if (equal args '("rev-parse" "--verify" "feature")) 0
                 (error "unmocked git-exit-code args: %S" args)))))
    (should-error (claude-repl--validate-worktree-creation
                   "feature" "/tmp/repo" "feature" "feature" "/nonexistent")
                  :type 'user-error)))

(ert-deftest claude-repl-test-validate-worktree-creation-passes ()
  "Valid inputs do not signal."
  (let ((claude-repl-worktree-start-tag-prefix nil))
    (cl-letf (((symbol-function 'file-directory-p) (lambda (_p) nil))
              ((symbol-function 'claude-repl--git-exit-code)
               (lambda (_root &rest args)
                 ;; Branch check: 128 = missing → no collision.
                 (if (equal args '("rev-parse" "--verify" "new-feature")) 128
                   (error "unmocked git-exit-code args: %S" args)))))
      ;; Should not error
      (claude-repl--validate-worktree-creation
       "new-feature" "/tmp/repo" "new-feature" "new-feature" "/nonexistent"))))

(ert-deftest claude-repl-test-validate-worktree-creation-existing-start-tag ()
  "Existing start tag (PREFIX+BRANCH) signals user-error."
  (let ((claude-repl-worktree-start-tag-prefix "start/"))
    (cl-letf (((symbol-function 'file-directory-p) (lambda (_p) nil))
              ((symbol-function 'claude-repl--git-exit-code)
               (lambda (_root &rest args)
                 (pcase args
                   ;; Branch missing.
                   (`("rev-parse" "--verify" "feature") 128)
                   ;; Start tag EXISTS.
                   (`("rev-parse" "--verify" "refs/tags/start/feature") 0)
                   (_ (error "unmocked git-exit-code args: %S" args))))))
      (should-error (claude-repl--validate-worktree-creation
                     "feature" "/tmp/repo" "feature" "feature" "/nonexistent")
                    :type 'user-error))))

(ert-deftest claude-repl-test-validate-worktree-creation-start-tag-disabled ()
  "When start-tag prefix is nil, an existing 'start/feature' tag does not block."
  (let ((claude-repl-worktree-start-tag-prefix nil))
    (cl-letf (((symbol-function 'file-directory-p) (lambda (_p) nil))
              ((symbol-function 'claude-repl--git-exit-code)
               (lambda (_root &rest args)
                 ;; Branch missing.  Start-tag probe MUST NOT fire when prefix is nil.
                 (if (equal args '("rev-parse" "--verify" "feature")) 128
                   (error "unmocked git-exit-code args (start-tag probe should be skipped): %S" args)))))
      ;; Should not error
      (claude-repl--validate-worktree-creation
       "feature" "/tmp/repo" "feature" "feature" "/nonexistent"))))

(ert-deftest claude-repl-test-validate-worktree-creation-nested-under-repo ()
  "Validation passes for a non-existent path nested under another git repo.
Regression: previously used `projectile-project-p', which walks UP from
PATH and would find an ancestor `.git' (e.g. when the worktree-parent
sits inside a separate repo), incorrectly flagging the new path as an
existing worktree."
  ;; The function under test uses `file-directory-p' on PATH only — it
  ;; never walks ancestors.  Asserting that an arbitrary nested path
  ;; that does not exist passes validation is enough to pin the
  ;; non-walking behavior.
  (let ((claude-repl-worktree-start-tag-prefix nil))
    (cl-letf (((symbol-function 'file-directory-p)
               (lambda (p)
                 ;; PATH passed in does not exist; only assertion.
                 (cond ((equal p "/tmp/outer-repo/inner/new-wt") nil)
                       (t (error "unexpected file-directory-p arg: %S" p)))))
              ((symbol-function 'claude-repl--git-exit-code)
               (lambda (_root &rest args)
                 (if (equal args '("rev-parse" "--verify" "new-wt")) 128
                   (error "unmocked git-exit-code args: %S" args)))))
      ;; Should not error
      (claude-repl--validate-worktree-creation
       "new-wt" "/tmp/outer-repo" "new-wt" "new-wt" "/tmp/outer-repo/inner/new-wt"))))

;;;; ---- Tests: merge-fork (cherry-pick-base) ----
;;
;; `claude-repl--cherry-pick-base' (aliased as `+dwc/workspace-merge--fork')
;; runs three git invocations through `claude-repl--git-string':
;;   (1) `log --right-only --pretty=%H --no-merges HEAD...TARGET' — target's
;;       unique commits, newest first, newline-separated.
;;   (2) `log --left-only --pretty=%B HEAD...TARGET' — HEAD's commits' bodies,
;;       which the parser scans for `(cherry picked from commit <sha>)' lines.
;;   (3) `merge-base HEAD TARGET' — only consulted on fallback when no target
;;       commit is found in the parsed cherry-pick annotations.
;; The tests stub `--git-string' with fixture strings shaped exactly like
;; real git output, so the dispatch logic is exercised end-to-end without
;; touching git.

(ert-deftest claude-repl-test-merge-fork-no-annotations-fallback ()
  "When HEAD has no -x annotations, fork falls back to merge-base HEAD TARGET."
  (let ((sha-m "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"))
    (cl-letf (((symbol-function 'claude-repl--git-string)
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
      (should (equal (+dwc/workspace-merge--fork "/tmp/repo" "branch-b") sha-m)))))

(ert-deftest claude-repl-test-merge-fork-clean-chain ()
  "After merging B (with -x), fork for C (descends from B) is B's tip SHA."
  ;; branch-c contains B1, B2, C1 (each unique vs HEAD).  HEAD's log carries
  ;; `(cherry picked from commit B1)` and `(cherry picked from commit B2)`,
  ;; but NOT C1.  The most recent target commit also present as a cherry-pick
  ;; annotation in HEAD's log is B2 — that's the fork point.
  (let ((sha-b1 "1111111111111111111111111111111111111111")
        (sha-b2 "2222222222222222222222222222222222222222")
        (sha-c1 "3333333333333333333333333333333333333333"))
    (cl-letf (((symbol-function 'claude-repl--git-string)
               (lambda (&rest args)
                 (pcase args
                   ;; `log --right-only` yields newest-first commits.
                   (`("-C" "/tmp/repo" "log" "--right-only" "--pretty=%H" "--no-merges" "HEAD...branch-c")
                    (concat sha-c1 "\n" sha-b2 "\n" sha-b1))
                   (`("-C" "/tmp/repo" "log" "--left-only" "--pretty=%B" "HEAD...branch-c")
                    (format "A1\n\nB1\n\n(cherry picked from commit %s)\n\nB2\n\n(cherry picked from commit %s)"
                            sha-b1 sha-b2))
                   (_ (error "unmocked git-string args: %S" args))))))
      (should (equal (+dwc/workspace-merge--fork "/tmp/repo" "branch-c") sha-b2)))))

(ert-deftest claude-repl-test-merge-fork-already-fully-merged ()
  "When all TARGET commits are incorporated, fork equals TARGET tip -> empty range."
  ;; Every commit on branch-b (B1, B2) appears as a cherry-pick annotation in
  ;; HEAD's log.  Newest-first target list is B2, B1, so B2 is returned.
  (let ((sha-b1 "1111111111111111111111111111111111111111")
        (sha-b2 "2222222222222222222222222222222222222222"))
    (cl-letf (((symbol-function 'claude-repl--git-string)
               (lambda (&rest args)
                 (pcase args
                   (`("-C" "/tmp/repo" "log" "--right-only" "--pretty=%H" "--no-merges" "HEAD...branch-b")
                    (concat sha-b2 "\n" sha-b1))
                   (`("-C" "/tmp/repo" "log" "--left-only" "--pretty=%B" "HEAD...branch-b")
                    (format "A1\n\nB1\n\n(cherry picked from commit %s)\n\nB2\n\n(cherry picked from commit %s)"
                            sha-b1 sha-b2))
                   (_ (error "unmocked git-string args: %S" args))))))
      (should (equal (+dwc/workspace-merge--fork "/tmp/repo" "branch-b") sha-b2)))))

(ert-deftest claude-repl-test-merge-fork-growing-workspace ()
  "After B is merged, adding B3 to branch-b; fork stays at B2 -> only B3 is new."
  ;; branch-b's target-only commits are B3, B2, B1 (newest-first).  B3 is NOT
  ;; in HEAD's cherry-pick annotations; B2 and B1 are.  `cl-find-if' walks
  ;; the target list newest-first and returns the first match — B2.
  (let ((sha-b1 "1111111111111111111111111111111111111111")
        (sha-b2 "2222222222222222222222222222222222222222")
        (sha-b3 "4444444444444444444444444444444444444444"))
    (cl-letf (((symbol-function 'claude-repl--git-string)
               (lambda (&rest args)
                 (pcase args
                   (`("-C" "/tmp/repo" "log" "--right-only" "--pretty=%H" "--no-merges" "HEAD...branch-b")
                    (concat sha-b3 "\n" sha-b2 "\n" sha-b1))
                   (`("-C" "/tmp/repo" "log" "--left-only" "--pretty=%B" "HEAD...branch-b")
                    (format "A1\n\nB1\n\n(cherry picked from commit %s)\n\nB2\n\n(cherry picked from commit %s)"
                            sha-b1 sha-b2))
                   (_ (error "unmocked git-string args: %S" args))))))
      (should (equal (+dwc/workspace-merge--fork "/tmp/repo" "branch-b") sha-b2)))))

(ert-deftest claude-repl-test-merge-fork-deep-chain ()
  "After merging B then C, fork for D (descends from C) is C's tip SHA."
  ;; branch-d's target-only newest-first: D1, C1, B2, B1.  HEAD's annotations
  ;; cover B1, B2, C1 (but not D1).  Newest match in target list is C1.
  (let ((sha-b1 "1111111111111111111111111111111111111111")
        (sha-b2 "2222222222222222222222222222222222222222")
        (sha-c1 "3333333333333333333333333333333333333333")
        (sha-d1 "5555555555555555555555555555555555555555"))
    (cl-letf (((symbol-function 'claude-repl--git-string)
               (lambda (&rest args)
                 (pcase args
                   (`("-C" "/tmp/repo" "log" "--right-only" "--pretty=%H" "--no-merges" "HEAD...branch-d")
                    (concat sha-d1 "\n" sha-c1 "\n" sha-b2 "\n" sha-b1))
                   (`("-C" "/tmp/repo" "log" "--left-only" "--pretty=%B" "HEAD...branch-d")
                    (format "A1\n\nB1\n\n(cherry picked from commit %s)\n\nB2\n\n(cherry picked from commit %s)\n\nC1\n\n(cherry picked from commit %s)"
                            sha-b1 sha-b2 sha-c1))
                   (_ (error "unmocked git-string args: %S" args))))))
      (should (equal (+dwc/workspace-merge--fork "/tmp/repo" "branch-d") sha-c1)))))

(ert-deftest claude-repl-test-merge-fork-annotation-survives-conflict-resolution ()
  "Annotation is written even when cherry-pick required conflict resolution via --continue."
  ;; Behaviorally identical to the clean-chain case from the parser's
  ;; perspective: HEAD's log contains the `(cherry picked from commit B1)`
  ;; annotation regardless of whether B1 cherry-picked clean or via --continue.
  ;; Fork for branch-c (which contains B1, C1) is therefore B1.
  (let ((sha-b1 "1111111111111111111111111111111111111111")
        (sha-c1 "3333333333333333333333333333333333333333"))
    (cl-letf (((symbol-function 'claude-repl--git-string)
               (lambda (&rest args)
                 (pcase args
                   (`("-C" "/tmp/repo" "log" "--right-only" "--pretty=%H" "--no-merges" "HEAD...branch-c")
                    (concat sha-c1 "\n" sha-b1))
                   (`("-C" "/tmp/repo" "log" "--left-only" "--pretty=%B" "HEAD...branch-c")
                    (format "A1\n\nB1 (resolved)\n\n(cherry picked from commit %s)" sha-b1))
                   (_ (error "unmocked git-string args: %S" args))))))
      (should (equal (+dwc/workspace-merge--fork "/tmp/repo" "branch-c") sha-b1)))))

;;;; ---- Tests: detect-merge-actually-landed-p ----

(ert-deftest claude-repl-test-detect-merge-actually-landed-p-defaults-true-no-project-dir ()
  "Returns t when WS has no :project-dir — backward-compat probe must
default to landed/success rather than flipping pre-existing successes
to ❌ when the worktree dir is gone or unset."
  (claude-repl-test--with-clean-state
    (puthash "ws" '() claude-repl--workspaces)
    (should (claude-repl--detect-merge-actually-landed-p "ws"))))

(ert-deftest claude-repl-test-detect-merge-actually-landed-p-defaults-true-no-source-ws-dir ()
  "Returns t when WS has no :source-ws-dir — the probe can't reach the
parent worktree to inspect cherry-pick annotations, so it defaults to
landed/success rather than slandering a clean merge."
  (claude-repl-test--with-clean-state
    (puthash "ws" '(:project-dir "/tmp/project") claude-repl--workspaces)
    ;; Production reads :source-ws-dir; absent → defaults to t before any git call.
    (cl-letf (((symbol-function 'file-directory-p)
               (lambda (p) (equal p "/tmp/project"))))
      (should (claude-repl--detect-merge-actually-landed-p "ws")))))

(ert-deftest claude-repl-test-detect-merge-actually-landed-p-true-on-clean-merge ()
  "Returns t when every commit on WS's branch is referenced via
cherry-pick -x in the parent's HEAD log.  Simulates a successful prior
merge: parent's HEAD log carries `(cherry picked from commit <sha>)' for
every target-only commit on WS's branch."
  (claude-repl-test--with-clean-state
    (puthash "ws"
             '(:project-dir "/tmp/wt" :source-ws-dir "/tmp/repo")
             claude-repl--workspaces)
    (let ((sha-f1 "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"))
      (cl-letf (((symbol-function 'file-directory-p) (lambda (_p) t))
                ((symbol-function 'claude-repl--git-string-quiet)
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
        (should (claude-repl--detect-merge-actually-landed-p "ws"))))))

(ert-deftest claude-repl-test-detect-merge-actually-landed-p-false-on-missing-pick ()
  "Returns nil when WS's branch has commits that are NOT referenced via
cherry-pick -x in the parent's HEAD log — the silent-failure case the
backward-compat probe is designed to detect."
  (claude-repl-test--with-clean-state
    (puthash "ws"
             '(:project-dir "/tmp/wt" :source-ws-dir "/tmp/repo")
             claude-repl--workspaces)
    (let ((sha-f1 "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"))
      (cl-letf (((symbol-function 'file-directory-p) (lambda (_p) t))
                ((symbol-function 'claude-repl--git-string-quiet)
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
        (should-not (claude-repl--detect-merge-actually-landed-p "ws"))))))

;;;; ---- Tests: cherry-pick-commits ----

(ert-deftest claude-repl-test-cherry-pick-commits-empty-range-returns-sentinel ()
  "When range is empty (0 commits), returns `already-incorporated'
instead of erroring — the workspace's commits are already on the
parent, so the merge is a successful no-op and the caller can proceed
to auto-finish."
  ;; Production calls `git rev-list --count BASE..TARGET' once; "0" means
  ;; empty range and short-circuits to the `already-incorporated' sentinel
  ;; before any cherry-pick attempt.
  (cl-letf (((symbol-function 'claude-repl--git-string)
             (lambda (&rest args)
               (pcase args
                 (`("-C" "/tmp/repo" "rev-list" "--count" "HEAD..feature") "0")
                 (_ (error "unmocked git-string args: %S" args)))))
            ((symbol-function 'claude-repl--git-exit-code)
             (lambda (&rest _args)
               (error "git-exit-code must not be called for empty range"))))
    (should (eq (claude-repl--cherry-pick-commits "/tmp/repo" "feature" "HEAD" "feature")
                'already-incorporated))))

(ert-deftest claude-repl-test-cherry-pick-commits-success ()
  "Successful cherry-pick with no conflicts."
  (let ((sha-m "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
        (cherry-pick-called nil))
    (cl-letf (((symbol-function 'claude-repl--git-string)
               (lambda (&rest args)
                 (pcase args
                   ;; Non-empty range — one commit to pick.
                   (`("-C" "/tmp/repo" "rev-list" "--count" ,_) "1")
                   (_ (error "unmocked git-string args: %S" args)))))
              ((symbol-function 'claude-repl--git-exit-code)
               (lambda (_root &rest args)
                 (pcase args
                   (`("cherry-pick" "-x" ,_)
                    (setq cherry-pick-called t)
                    0)
                   (_ (error "unmocked git-exit-code args: %S" args)))))
              ;; No CHERRY_PICK_HEAD remains — clean cherry-pick.
              ((symbol-function 'claude-repl--cherry-pick-in-progress-p)
               (lambda (_root) nil)))
      (should (null (claude-repl--cherry-pick-commits
                     "/tmp/repo" "feature" sha-m "feature")))
      (should cherry-pick-called))))

(ert-deftest claude-repl-test-cherry-pick-commits-silent-failure-returns-failed ()
  "When `git cherry-pick' exits non-zero but no CHERRY_PICK_HEAD is left
behind (silent failure — commits didn't land and no conflict resolution
is in flight), `--cherry-pick-commits' returns `failed'."
  (let ((sha-m "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"))
    (cl-letf (((symbol-function 'claude-repl--git-string)
               (lambda (&rest args)
                 (pcase args
                   (`("-C" "/tmp/repo" "rev-list" "--count" ,_) "1")
                   (_ (error "unmocked git-string args: %S" args)))))
              ((symbol-function 'claude-repl--git-exit-code)
               (lambda (_root &rest args)
                 (pcase args
                   ;; Non-zero exit but no conflict-head left behind.
                   (`("cherry-pick" "-x" ,_) 128)
                   (_ (error "unmocked git-exit-code args: %S" args)))))
              ((symbol-function 'claude-repl--cherry-pick-in-progress-p)
               (lambda (_root) nil)))
      (should (eq (claude-repl--cherry-pick-commits
                   "/tmp/repo" "feature" sha-m "feature")
                  'failed)))))

(ert-deftest claude-repl-test-cherry-pick-commits-conflict-signals ()
  "Cherry-pick conflict aborts the cherry-pick and signals user-error
\(no magit pop — the abort clears CHERRY_PICK_HEAD so there's nothing
left to resolve)."
  (let ((sha-m "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
        ;; Track in-progress state: t while CHERRY_PICK_HEAD exists,
        ;; flipped to nil after `--check-cherry-pick-conflict' runs.
        (in-progress t))
    (cl-letf (((symbol-function 'claude-repl--git-string)
               (lambda (&rest args)
                 (pcase args
                   (`("-C" "/tmp/repo" "rev-list" "--count" ,_) "1")
                   (_ (error "unmocked git-string args: %S" args)))))
              ((symbol-function 'claude-repl--git-exit-code)
               (lambda (_root &rest args)
                 (pcase args
                   (`("cherry-pick" "-x" ,_) 1)
                   (_ (error "unmocked git-exit-code args: %S" args)))))
              ((symbol-function 'claude-repl--cherry-pick-in-progress-p)
               (lambda (_root) in-progress))
              ;; The real check-cherry-pick-conflict would call git to abort;
              ;; stub it to (a) clear the in-progress state and (b) signal.
              ((symbol-function 'claude-repl--check-cherry-pick-conflict)
               (lambda (_ws root _target-ws)
                 (setq in-progress nil)
                 (user-error "Conflict cherry-picking in %s" root))))
      (should-error (claude-repl--cherry-pick-commits
                     "/tmp/repo" "feature" sha-m "feature")
                    :type 'user-error)
      (should-not in-progress))))

;;;; ---- Tests: check-cherry-pick-conflict ----

(ert-deftest claude-repl-test-check-cherry-pick-conflict-no-conflict ()
  "When no CHERRY_PICK_HEAD exists, returns nil (no error)."
  (cl-letf (((symbol-function 'claude-repl--git-string)
             (lambda (&rest args)
               (pcase args
                 (`("-C" "/tmp/repo" "rev-parse" "--absolute-git-dir") "/tmp/repo/.git")
                 (_ (error "unmocked git-string args: %S" args)))))
            ;; CHERRY_PICK_HEAD does not exist on disk.
            ((symbol-function 'file-exists-p) (lambda (_p) nil))
            ((symbol-function 'claude-repl--git-exit-code)
             (lambda (&rest args) (error "abort should not run: %S" args))))
    (should-not (claude-repl--check-cherry-pick-conflict "test-ws" "/tmp/repo" "test-ws"))))

(ert-deftest claude-repl-test-check-cherry-pick-conflict-with-conflict ()
  "When CHERRY_PICK_HEAD exists, `git cherry-pick --abort' is run before
user-error is signaled, then the on-disk state is cleared."
  (let ((cherry-pick-head-exists t)
        (abort-called nil))
    (cl-letf (((symbol-function 'claude-repl--git-string)
               (lambda (&rest args)
                 (pcase args
                   (`("-C" "/tmp/repo" "rev-parse" "--absolute-git-dir") "/tmp/repo/.git")
                   (`("-C" "/tmp/repo" "rev-parse" "--short" "CHERRY_PICK_HEAD") "abcd123")
                   (_ (error "unmocked git-string args: %S" args)))))
              ((symbol-function 'file-exists-p)
               (lambda (p)
                 (and (equal p "/tmp/repo/.git/CHERRY_PICK_HEAD")
                      cherry-pick-head-exists)))
              ((symbol-function 'claude-repl--git-exit-code)
               (lambda (_root &rest args)
                 (pcase args
                   (`("cherry-pick" "--abort")
                    (setq abort-called t
                          ;; Abort clears CHERRY_PICK_HEAD.
                          cherry-pick-head-exists nil)
                    0)
                   (_ (error "unmocked git-exit-code args: %S" args))))))
      (should-error (claude-repl--check-cherry-pick-conflict "test-ws" "/tmp/repo" "test-ws")
                    :type 'user-error)
      (should abort-called)
      (should-not cherry-pick-head-exists))))

;;;; ---- Tests: cherry-pick-in-progress-p ----

(ert-deftest claude-repl-test-cherry-pick-in-progress-p-false-on-clean-tree ()
  "No CHERRY_PICK_HEAD → returns nil."
  (cl-letf (((symbol-function 'claude-repl--git-string)
             (lambda (&rest args)
               (pcase args
                 (`("-C" "/tmp/repo" "rev-parse" "--absolute-git-dir") "/tmp/repo/.git")
                 (_ (error "unmocked git-string args: %S" args)))))
            ((symbol-function 'file-exists-p)
             (lambda (p)
               ;; The sole probe should be for CHERRY_PICK_HEAD; report missing.
               (cond ((equal p "/tmp/repo/.git/CHERRY_PICK_HEAD") nil)
                     (t (error "unexpected file-exists-p arg: %S" p))))))
    (should-not (claude-repl--cherry-pick-in-progress-p "/tmp/repo"))))

(ert-deftest claude-repl-test-cherry-pick-in-progress-p-true-during-conflict ()
  "CHERRY_PICK_HEAD present → returns t."
  (cl-letf (((symbol-function 'claude-repl--git-string)
             (lambda (&rest args)
               (pcase args
                 (`("-C" "/tmp/repo" "rev-parse" "--absolute-git-dir") "/tmp/repo/.git")
                 (_ (error "unmocked git-string args: %S" args)))))
            ((symbol-function 'file-exists-p)
             (lambda (p) (equal p "/tmp/repo/.git/CHERRY_PICK_HEAD"))))
    (should (claude-repl--cherry-pick-in-progress-p "/tmp/repo"))))

;;;; ---- Tests: cherry-pick-conflicted-files ----

(ert-deftest claude-repl-test-cherry-pick-conflicted-files-empty ()
  "No conflict in flight → empty list."
  (cl-letf (((symbol-function 'claude-repl--git-string)
             (lambda (&rest args)
               (pcase args
                 (`("-C" "/tmp/repo" "diff" "--name-only" "--diff-filter=U") "")
                 (_ (error "unmocked git-string args: %S" args))))))
    (should-not (claude-repl--cherry-pick-conflicted-files "/tmp/repo"))))

(ert-deftest claude-repl-test-cherry-pick-conflicted-files-lists-conflicts ()
  "Conflicted file is enumerated by name (relative to repo)."
  (cl-letf (((symbol-function 'claude-repl--git-string)
             (lambda (&rest args)
               (pcase args
                 (`("-C" "/tmp/repo" "diff" "--name-only" "--diff-filter=U") "shared")
                 (_ (error "unmocked git-string args: %S" args))))))
    (should (equal (claude-repl--cherry-pick-conflicted-files "/tmp/repo")
                   '("shared")))))

;;;; ---- Tests: file-has-conflict-markers-p ----

(ert-deftest claude-repl-test-file-has-conflict-markers-p-true ()
  "File containing <<<<<<< marker is detected as conflicted."
  (let ((tmp (make-temp-file "conflict-")))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (insert "ok\n<<<<<<< HEAD\nA\n=======\nB\n>>>>>>> other\n"))
          (should (claude-repl--file-has-conflict-markers-p tmp)))
      (delete-file tmp))))

(ert-deftest claude-repl-test-file-has-conflict-markers-p-false-on-clean ()
  "File without conflict markers returns nil."
  (let ((tmp (make-temp-file "no-conflict-")))
    (unwind-protect
        (progn
          (with-temp-file tmp (insert "plain content\nline 2\n"))
          (should-not (claude-repl--file-has-conflict-markers-p tmp)))
      (delete-file tmp))))

(ert-deftest claude-repl-test-file-has-conflict-markers-p-false-on-missing ()
  "Unreadable/missing file returns nil rather than erroring."
  (should-not (claude-repl--file-has-conflict-markers-p
               "/nonexistent/path/no-such-file")))

(ert-deftest claude-repl-test-file-has-conflict-markers-p-ignores-non-anchored-marker ()
  "A `<<<<<<<' that is not at line start is not a conflict marker."
  (let ((tmp (make-temp-file "fake-marker-")))
    (unwind-protect
        (progn
          (with-temp-file tmp (insert "prefix <<<<<<< not a marker\n"))
          (should-not (claude-repl--file-has-conflict-markers-p tmp)))
      (delete-file tmp))))

;;;; ---- Tests: all-conflicts-resolved-p ----

(ert-deftest claude-repl-test-all-conflicts-resolved-p-empty-list ()
  "Empty FILES list treated as resolved — nothing left to clear."
  (cl-letf (((symbol-function 'claude-repl--file-has-conflict-markers-p)
             (lambda (&rest _) (error "should not probe files on empty list"))))
    (should (claude-repl--all-conflicts-resolved-p "/tmp/repo" nil))))

(ert-deftest claude-repl-test-all-conflicts-resolved-p-true-when-markers-gone ()
  "Returns t when every listed file is clean of markers."
  (cl-letf (((symbol-function 'claude-repl--file-has-conflict-markers-p)
             (lambda (_path) nil)))
    (should (claude-repl--all-conflicts-resolved-p "/tmp/repo" '("a" "b")))))

(ert-deftest claude-repl-test-all-conflicts-resolved-p-false-when-any-file-has-markers ()
  "Any file still containing a marker → returns nil."
  (cl-letf (((symbol-function 'claude-repl--file-has-conflict-markers-p)
             (lambda (path)
               ;; File "a" is clean; file "b" still has markers.
               (string-suffix-p "b" path))))
    (should-not (claude-repl--all-conflicts-resolved-p "/tmp/repo" '("a" "b")))))

;;;; ---- Tests: build-auto-resolve-prompt ----

(ert-deftest claude-repl-test-build-auto-resolve-prompt-mentions-workspace ()
  "Generated prompt names the workspace and commit being resolved."
  (let ((p (claude-repl--build-auto-resolve-prompt "ws1" "deadbeef" '("f1.txt"))))
    (should (string-match-p "ws1" p))
    (should (string-match-p "deadbeef" p))))

(ert-deftest claude-repl-test-build-auto-resolve-prompt-lists-files ()
  "Generated prompt enumerates each conflicted file path."
  (let ((p (claude-repl--build-auto-resolve-prompt "ws1" "abc1234"
                                                   '("dir/a.el" "b.txt"))))
    (should (string-match-p "dir/a.el" p))
    (should (string-match-p "b.txt" p))))

(ert-deftest claude-repl-test-build-auto-resolve-prompt-forbids-git-commands ()
  "Generated prompt explicitly forbids git commands and edits outside
the conflicted files — the most load-bearing constraints for safety."
  (let ((p (claude-repl--build-auto-resolve-prompt "ws1" "abc1234" '("f"))))
    (should (string-match-p "NEVER run ANY git command" p))
    (should (string-match-p "no `git add`" p))
    (should (string-match-p "no `git cherry-pick --continue`" p))))

(ert-deftest claude-repl-test-build-auto-resolve-prompt-requires-orthogonality-check ()
  "Generated prompt requires the resolver to judge orthogonality and
make no edits when uncertain."
  (let ((p (claude-repl--build-auto-resolve-prompt "ws1" "abc1234" '("f"))))
    (should (string-match-p "CONCEPTUALLY ORTHOGONAL" p))
    (should (string-match-p "make NO edits" p))))

;;;; ---- Tests: auto-resolve-conflicts-extra-args default ----

(ert-deftest claude-repl-test-auto-resolve-extra-args-includes-dangerously-skip-permissions ()
  "Default extra-args contain `--dangerously-skip-permissions' so the
resolver cannot stall on a permission prompt even when
`bypassPermissions' mode is insufficient."
  (should (member "--dangerously-skip-permissions"
                  (default-value 'claude-repl-auto-resolve-conflicts-extra-args))))

(ert-deftest claude-repl-test-invoke-auto-resolve-claude-passes-extra-args ()
  "`--invoke-auto-resolve-claude' includes the configured extra-args
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
                 (funcall real-start "claude-auto-resolve-stub"
                          (generate-new-buffer " *stub*") "true"))))
      (claude-repl--invoke-auto-resolve-claude "/tmp" "prompt"))
    (should (member "--dangerously-skip-permissions" captured-cmd))
    (should (equal (cl-subseq captured-cmd 0 4)
                   (list claude-repl-auto-resolve-conflicts-program
                         "-p" "--model"
                         claude-repl-auto-resolve-conflicts-model)))))

(ert-deftest claude-repl-test-invoke-auto-resolve-claude-passes-prompt-as-trailing-arg ()
  "`--invoke-auto-resolve-claude' passes PROMPT as the final positional
argument to `claude -p' (that is how the non-interactive API consumes
the prompt — NOT via stdin)."
  (let* ((captured-cmd nil)
         (real-start (symbol-function 'start-process)))
    (cl-letf (((symbol-function 'start-process)
               (lambda (_name _buf &rest cmd)
                 (setq captured-cmd cmd)
                 (funcall real-start "claude-auto-resolve-stub"
                          (generate-new-buffer " *stub*") "true"))))
      (claude-repl--invoke-auto-resolve-claude "/tmp" "RESOLVE THIS"))
    (should (equal (car (last captured-cmd)) "RESOLVE THIS"))))

(ert-deftest claude-repl-test-invoke-auto-resolve-claude-separates-prompt-with-double-dash ()
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
      (claude-repl--invoke-auto-resolve-claude "/tmp" "MY PROMPT"))
    (let ((tail (last captured-cmd 2)))
      (should (equal (car tail) "--"))
      (should (equal (cadr tail) "MY PROMPT")))))

(ert-deftest claude-repl-test-invoke-auto-resolve-claude-logs-output ()
  "`--invoke-auto-resolve-claude' mirrors the resolver's stdout/stderr
into the logfile via `claude-repl--log'.  Without this the resolver's
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
              ((symbol-function 'claude-repl--log)
               (lambda (_ws fmt &rest args)
                 (push (apply #'format fmt args) logged))))
      (claude-repl--invoke-auto-resolve-claude "/tmp" "prompt" "ws1"))
    (should (cl-some (lambda (l) (string-match-p "RESOLVER STDOUT" l)) logged))
    (should (cl-some (lambda (l)
                       (string-match-p "auto-resolve: claude -p exited status=" l))
                     logged))))

(ert-deftest claude-repl-test-invoke-auto-resolve-claude-log-omits-header-block ()
  "The logged output excludes the `# claude-repl merge resolver — ...'
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
              ((symbol-function 'claude-repl--log)
               (lambda (_ws fmt &rest args)
                 (push (apply #'format fmt args) logged))))
      (claude-repl--invoke-auto-resolve-claude "/tmp" "prompt" "ws1"))
    (let ((output-log (cl-find-if
                       (lambda (l) (string-match-p "output follows" l))
                       logged)))
      (should output-log)
      (should (string-match-p "ACTUAL RESOLVER OUTPUT" output-log))
      (should-not (string-match-p "# claude-repl merge resolver" output-log))
      (should-not (string-match-p "# root:" output-log))
      (should-not (string-match-p "# cmd:" output-log)))))

(ert-deftest claude-repl-test-invoke-auto-resolve-claude-passes-ws-to-log ()
  "Resolver-output log entries carry TARGET-WS as the workspace tag, so
the standard `{ws=... id=...}` metadata block disambiguates resolver
runs across concurrent merges."
  (let* ((logged-ws nil)
         (real-start (symbol-function 'start-process)))
    (cl-letf (((symbol-function 'start-process)
               (lambda (_name _buf &rest _cmd)
                 (funcall real-start "stub"
                          (generate-new-buffer " *stub*") "true")))
              ((symbol-function 'claude-repl--log)
               (lambda (ws fmt &rest args)
                 (when (string-match-p "exited status="
                                       (apply #'format fmt args))
                   (push ws logged-ws)))))
      (claude-repl--invoke-auto-resolve-claude "/tmp" "prompt" "my-ws"))
    (should (member "my-ws" logged-ws))))

(ert-deftest claude-repl-test-invoke-auto-resolve-verify-logs-output ()
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
              ((symbol-function 'claude-repl--log)
               (lambda (_ws fmt &rest args)
                 (push (apply #'format fmt args) logged))))
      (claude-repl--invoke-auto-resolve-verify "/tmp" (list "true")))
    (should (cl-some (lambda (l) (string-match-p "VERIFY OUTPUT" l)) logged))
    (should (cl-some (lambda (l)
                       (string-match-p "auto-resolve-verify: exited status=" l))
                     logged))))

;;;; ---- Tests: auto-resolve-cherry-pick-conflict ----

(ert-deftest claude-repl-test-auto-resolve-returns-nil-when-no-conflicted-files ()
  "No conflicted files → resolver returns nil without spawning claude."
  (let ((invoked nil))
    (cl-letf (((symbol-function 'claude-repl--cherry-pick-conflicted-files)
               (lambda (_root) nil))
              ((symbol-function 'claude-repl--invoke-auto-resolve-claude)
               (lambda (&rest _) (setq invoked t) 0)))
      (should-not (claude-repl--auto-resolve-cherry-pick-conflict "ws" "/tmp/repo"))
      (should-not invoked))))

(ert-deftest claude-repl-test-auto-resolve-accepts-when-markers-cleared ()
  "Resolver returns t when conflicted files no longer contain markers
after the stubbed `claude -p' returns successfully."
  (cl-letf (((symbol-function 'claude-repl--cherry-pick-conflicted-files)
             (lambda (_root) '("shared")))
            ((symbol-function 'claude-repl--git-string)
             (lambda (&rest args)
               (pcase args
                 (`("-C" "/tmp/repo" "rev-parse" "--short" "CHERRY_PICK_HEAD") "abcd123")
                 (_ (error "unmocked git-string args: %S" args)))))
            ((symbol-function 'claude-repl--invoke-auto-resolve-claude)
             (lambda (&rest _) 0))
            ;; Files reported clean of markers after the resolver runs.
            ((symbol-function 'claude-repl--all-conflicts-resolved-p)
             (lambda (_root _files) t))
            ;; No verify command configured → gate accepts.
            ((symbol-function 'claude-repl--auto-resolve-verify-passes-p)
             (lambda (_ws _root) t)))
    (should (claude-repl--auto-resolve-cherry-pick-conflict "ws" "/tmp/repo"))))

(ert-deftest claude-repl-test-auto-resolve-declines-when-markers-remain ()
  "Resolver returns nil when conflict markers still exist in any file
after the stubbed `claude -p' exits."
  (cl-letf (((symbol-function 'claude-repl--cherry-pick-conflicted-files)
             (lambda (_root) '("shared")))
            ((symbol-function 'claude-repl--git-string)
             (lambda (&rest _args) "abcd123"))
            ((symbol-function 'claude-repl--invoke-auto-resolve-claude)
             (lambda (&rest _) 0))
            ;; Markers still present → decline.
            ((symbol-function 'claude-repl--all-conflicts-resolved-p)
             (lambda (_root _files) nil)))
    (should-not (claude-repl--auto-resolve-cherry-pick-conflict "ws" "/tmp/repo"))))

(ert-deftest claude-repl-test-auto-resolve-declines-on-timeout ()
  "Resolver returns nil when invoked claude -p reports timeout."
  (cl-letf (((symbol-function 'claude-repl--cherry-pick-conflicted-files)
             (lambda (_root) '("shared")))
            ((symbol-function 'claude-repl--git-string)
             (lambda (&rest _args) "abcd123"))
            ((symbol-function 'claude-repl--invoke-auto-resolve-claude)
             (lambda (&rest _) 'timeout))
            ((symbol-function 'claude-repl--all-conflicts-resolved-p)
             (lambda (&rest _args) (error "should not probe markers after timeout"))))
    (should-not (claude-repl--auto-resolve-cherry-pick-conflict "ws" "/tmp/repo"))))

(ert-deftest claude-repl-test-auto-resolve-declines-on-nonzero-exit ()
  "Resolver returns nil when invoked claude -p exits non-zero, even if
the files happen to look clean afterward — a failure exit is the only
honest signal that something went wrong inside the headless agent."
  (cl-letf (((symbol-function 'claude-repl--cherry-pick-conflicted-files)
             (lambda (_root) '("shared")))
            ((symbol-function 'claude-repl--git-string)
             (lambda (&rest _args) "abcd123"))
            ((symbol-function 'claude-repl--invoke-auto-resolve-claude)
             (lambda (&rest _) 1))
            ;; Even with clean files, the non-zero exit short-circuits.
            ((symbol-function 'claude-repl--all-conflicts-resolved-p)
             (lambda (&rest _args)
               (error "should not probe markers after non-zero exit"))))
    (should-not (claude-repl--auto-resolve-cherry-pick-conflict "ws" "/tmp/repo"))))

;;;; ---- Tests: auto-resolve-verify-cmd (config resolver) ----

(ert-deftest claude-repl-test-auto-resolve-verify-cmd-nil-config ()
  "nil config resolves to nil (skip verification)."
  (let ((claude-repl-auto-resolve-verify-command nil))
    (should-not (claude-repl--auto-resolve-verify-cmd "/tmp"))))

(ert-deftest claude-repl-test-auto-resolve-verify-cmd-list-config ()
  "List-of-strings config resolves to itself."
  (let ((claude-repl-auto-resolve-verify-command '("just" "test")))
    (should (equal (claude-repl--auto-resolve-verify-cmd "/tmp")
                   '("just" "test")))))

(ert-deftest claude-repl-test-auto-resolve-verify-cmd-function-returning-list ()
  "Function-form config: function is called with ROOT, return list is used."
  (let* ((received-root nil)
         (claude-repl-auto-resolve-verify-command
          (lambda (root) (setq received-root root) '("verify" "here"))))
    (should (equal (claude-repl--auto-resolve-verify-cmd "/tmp/wt")
                   '("verify" "here")))
    (should (equal received-root "/tmp/wt"))))

(ert-deftest claude-repl-test-auto-resolve-verify-cmd-function-returning-nil ()
  "Function-form returning nil means skip verification for this invocation."
  (let ((claude-repl-auto-resolve-verify-command (lambda (_root) nil)))
    (should-not (claude-repl--auto-resolve-verify-cmd "/tmp"))))

(ert-deftest claude-repl-test-auto-resolve-verify-cmd-function-returning-malformed ()
  "Function-form returning malformed value resolves to nil (skip), not raise."
  (let ((claude-repl-auto-resolve-verify-command (lambda (_r) 'oops)))
    (should-not (claude-repl--auto-resolve-verify-cmd "/tmp"))))

(ert-deftest claude-repl-test-auto-resolve-verify-cmd-malformed-list-config ()
  "List containing non-strings resolves to nil (skip), not raise."
  (let ((claude-repl-auto-resolve-verify-command '("just" 42)))
    (should-not (claude-repl--auto-resolve-verify-cmd "/tmp"))))

;;;; ---- Tests: invoke-auto-resolve-verify (subprocess) ----
;;
;; `claude-repl--invoke-auto-resolve-verify' spawns a process via
;; `start-process'.  Per the no-subprocess policy these tests must stub
;; that primitive — the production logic under test is the wait-loop,
;; status-dispatch, and cwd-binding behavior around the spawn, not the
;; spawn itself.  Each test replaces `start-process' (and adjacent
;; primitives the wait-loop depends on) with deterministic fakes.

(ert-deftest claude-repl-test-invoke-auto-resolve-verify-zero-exit ()
  "Verifier with exit-0 command returns 0."
  (let ((claude-repl-auto-resolve-verify-timeout 30))
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
      (should (eql (claude-repl--invoke-auto-resolve-verify "/tmp/repo" '("true"))
                   0)))))

(ert-deftest claude-repl-test-invoke-auto-resolve-verify-nonzero-exit ()
  "Verifier with exit-non-zero command returns the non-zero exit code."
  (let ((claude-repl-auto-resolve-verify-timeout 30))
    (cl-letf (((symbol-function 'start-process)
               (lambda (_name buf &rest _cmd) (list :proc :buffer buf)))
              ((symbol-function 'set-process-query-on-exit-flag)
               (lambda (&rest _) nil))
              ((symbol-function 'process-live-p) (lambda (_p) nil))
              ((symbol-function 'process-exit-status) (lambda (_p) 1))
              ((symbol-function 'accept-process-output) (lambda (&rest _) nil))
              ((symbol-function 'delete-process) (lambda (_p) nil)))
      (let ((rc (claude-repl--invoke-auto-resolve-verify "/tmp/repo" '("false"))))
        (should (and (numberp rc) (not (zerop rc))))))))

(ert-deftest claude-repl-test-invoke-auto-resolve-verify-timeout ()
  "Verifier with a hung command returns `timeout' when the deadline elapses."
  ;; Simulate the deadline-elapsed branch by making the process appear live
  ;; while time advances past the timeout.  `float-time' is stubbed to return
  ;; a monotonically increasing value so the wait loop fires the timeout exit.
  (let ((claude-repl-auto-resolve-verify-timeout 1)
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
      (should (eq (claude-repl--invoke-auto-resolve-verify
                   "/tmp/repo" '("sleep" "30"))
                  'timeout))
      (should delete-called))))

(ert-deftest claude-repl-test-invoke-auto-resolve-verify-cwd-is-root ()
  "Verifier runs with `default-directory' set to ROOT.
The production function rebinds `default-directory' around the spawn;
the stubbed `start-process' captures the binding to prove it."
  (let ((claude-repl-auto-resolve-verify-timeout 30)
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
      (let ((rc (claude-repl--invoke-auto-resolve-verify
                 "/tmp/repo" '("sh" "-c" "true"))))
        (should (eql rc 0))
        (should (equal captured-cwd
                       (file-name-as-directory "/tmp/repo")))))))

;;;; ---- Tests: auto-resolve-verify-passes-p ----

(ert-deftest claude-repl-test-auto-resolve-verify-passes-p-no-command ()
  "With no verify-command configured, the gate accepts without spawning."
  (let ((claude-repl-auto-resolve-verify-command nil)
        (spawned nil))
    (cl-letf (((symbol-function 'claude-repl--invoke-auto-resolve-verify)
               (lambda (&rest _) (setq spawned t) 0)))
      (should (claude-repl--auto-resolve-verify-passes-p "ws" "/tmp"))
      (should-not spawned))))

(ert-deftest claude-repl-test-auto-resolve-verify-passes-p-zero-exit ()
  "Verifier exit=0 → gate accepts."
  (let ((claude-repl-auto-resolve-verify-command '("true")))
    (cl-letf (((symbol-function 'claude-repl--invoke-auto-resolve-verify)
               (lambda (&rest _) 0)))
      (should (claude-repl--auto-resolve-verify-passes-p "ws" "/tmp")))))

(ert-deftest claude-repl-test-auto-resolve-verify-passes-p-nonzero-exit ()
  "Verifier exit non-zero → gate declines (returns nil)."
  (let ((claude-repl-auto-resolve-verify-command '("false")))
    (cl-letf (((symbol-function 'claude-repl--invoke-auto-resolve-verify)
               (lambda (&rest _) 1)))
      (should-not (claude-repl--auto-resolve-verify-passes-p "ws" "/tmp")))))

(ert-deftest claude-repl-test-auto-resolve-verify-passes-p-timeout ()
  "Verifier timeout → gate declines (returns nil)."
  (let ((claude-repl-auto-resolve-verify-command '("hang")))
    (cl-letf (((symbol-function 'claude-repl--invoke-auto-resolve-verify)
               (lambda (&rest _) 'timeout)))
      (should-not (claude-repl--auto-resolve-verify-passes-p "ws" "/tmp")))))

;;;; ---- Tests: auto-resolve-cherry-pick-conflict with verify ----

(ert-deftest claude-repl-test-auto-resolve-declines-when-verify-fails ()
  "Even with markers cleared and resolver exit=0, a non-zero verify
exit causes `--auto-resolve-cherry-pick-conflict' to return nil.
Soundness gate: textual marker scan is necessary but not sufficient."
  (let ((claude-repl-auto-resolve-verify-command '("verify-cmd")))
    (cl-letf (((symbol-function 'claude-repl--cherry-pick-conflicted-files)
               (lambda (_root) '("shared")))
              ((symbol-function 'claude-repl--git-string)
               (lambda (&rest _args) "abcd123"))
              ((symbol-function 'claude-repl--invoke-auto-resolve-claude)
               (lambda (&rest _) 0))
              ((symbol-function 'claude-repl--all-conflicts-resolved-p)
               (lambda (_root _files) t))
              ((symbol-function 'claude-repl--invoke-auto-resolve-verify)
               (lambda (&rest _) 1)))
      (should-not (claude-repl--auto-resolve-cherry-pick-conflict "ws" "/tmp/repo")))))

(ert-deftest claude-repl-test-auto-resolve-accepts-when-verify-passes ()
  "Markers cleared AND verify exit=0 → `--auto-resolve-cherry-pick-conflict' returns t."
  (let ((claude-repl-auto-resolve-verify-command '("verify-cmd")))
    (cl-letf (((symbol-function 'claude-repl--cherry-pick-conflicted-files)
               (lambda (_root) '("shared")))
              ((symbol-function 'claude-repl--git-string)
               (lambda (&rest _args) "abcd123"))
              ((symbol-function 'claude-repl--invoke-auto-resolve-claude)
               (lambda (&rest _) 0))
              ((symbol-function 'claude-repl--all-conflicts-resolved-p)
               (lambda (_root _files) t))
              ((symbol-function 'claude-repl--invoke-auto-resolve-verify)
               (lambda (&rest _) 0)))
      (should (claude-repl--auto-resolve-cherry-pick-conflict "ws" "/tmp/repo")))))

;;;; ---- Tests: cherry-pick-commits end-to-end with verify ----

(ert-deftest claude-repl-test-cherry-pick-commits-verify-fail-aborts-and-signals ()
  "End-to-end: markers cleared by resolver but verify-fail → `cherry-pick
--commits' falls through to `--check-cherry-pick-conflict' which aborts
the cherry-pick and signals user-error."
  (let ((sha-m "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
        (in-progress t)
        (check-called nil)
        (claude-repl-auto-resolve-verify-command '("verify-cmd")))
    (cl-letf (((symbol-function 'claude-repl--git-string)
               (lambda (&rest args)
                 (pcase args
                   (`("-C" "/tmp/repo" "rev-list" "--count" ,_) "1")
                   (_ (error "unmocked git-string args: %S" args)))))
              ((symbol-function 'claude-repl--git-exit-code)
               (lambda (_root &rest args)
                 (pcase args
                   (`("cherry-pick" "-x" ,_) 1)
                   (_ (error "unmocked git-exit-code args: %S" args)))))
              ((symbol-function 'claude-repl--cherry-pick-in-progress-p)
               (lambda (_root) in-progress))
              ;; Auto-resolve decides to decline (verify fails).
              ((symbol-function 'claude-repl--auto-resolve-cherry-pick-conflict)
               (lambda (_target-ws _root) nil))
              ;; Stub the abort path: marks the on-disk state cleared and signals.
              ((symbol-function 'claude-repl--check-cherry-pick-conflict)
               (lambda (_ws _root _target-ws)
                 (setq check-called t
                       in-progress nil)
                 (user-error "Conflict cherry-picking from feature — aborted"))))
      (should-error (claude-repl--cherry-pick-commits
                     "/tmp/repo" "feature" sha-m "feature" t)
                    :type 'user-error)
      (should check-called)
      (should-not in-progress))))

;;;; ---- Tests: cherry-pick-commits with auto-resolve ----

(ert-deftest claude-repl-test-cherry-pick-commits-auto-resolve-success-advances-merge ()
  "When auto-resolve clears the markers, `--cherry-pick-commits' stages
and runs `cherry-pick --continue', completing the merge cleanly.
Returns nil (clean cherry-pick), and the loop exits when CHERRY_PICK_HEAD
is gone."
  (let ((sha-m "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
        ;; First call returns t (conflict from initial cherry-pick), then
        ;; nil after the resolver runs and continue lands.
        (in-progress-states '(t nil))
        (continue-called nil))
    (cl-letf (((symbol-function 'claude-repl--git-string)
               (lambda (&rest args)
                 (pcase args
                   (`("-C" "/tmp/repo" "rev-list" "--count" ,_) "1")
                   (_ (error "unmocked git-string args: %S" args)))))
              ((symbol-function 'claude-repl--git-exit-code)
               (lambda (_root &rest args)
                 (pcase args
                   (`("cherry-pick" "-x" ,_) 1)
                   (_ (error "unmocked git-exit-code args: %S" args)))))
              ((symbol-function 'claude-repl--cherry-pick-in-progress-p)
               (lambda (_root) (pop in-progress-states)))
              ((symbol-function 'claude-repl--auto-resolve-cherry-pick-conflict)
               (lambda (_target-ws _root) t))
              ((symbol-function 'claude-repl--continue-cherry-pick-after-resolve)
               (lambda (_target-ws _root) (setq continue-called t) 0)))
      (should (null (claude-repl--cherry-pick-commits
                     "/tmp/repo" "feature" sha-m "feature" t)))
      (should continue-called))))

(ert-deftest claude-repl-test-cherry-pick-commits-auto-resolve-decline-falls-back-to-magit ()
  "When auto-resolve cannot clear the markers, `--cherry-pick-commits'
falls through to `--check-cherry-pick-conflict' which aborts the
cherry-pick and signals user-error."
  (let ((sha-m "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
        (in-progress t))
    (cl-letf (((symbol-function 'claude-repl--git-string)
               (lambda (&rest args)
                 (pcase args
                   (`("-C" "/tmp/repo" "rev-list" "--count" ,_) "1")
                   (_ (error "unmocked git-string args: %S" args)))))
              ((symbol-function 'claude-repl--git-exit-code)
               (lambda (_root &rest args)
                 (pcase args
                   (`("cherry-pick" "-x" ,_) 1)
                   (_ (error "unmocked git-exit-code args: %S" args)))))
              ((symbol-function 'claude-repl--cherry-pick-in-progress-p)
               (lambda (_root) in-progress))
              ((symbol-function 'claude-repl--auto-resolve-cherry-pick-conflict)
               (lambda (_target-ws _root) nil))
              ((symbol-function 'claude-repl--check-cherry-pick-conflict)
               (lambda (_ws _root _target-ws)
                 (setq in-progress nil)
                 (user-error "Conflict cherry-picking — aborted"))))
      (should-error (claude-repl--cherry-pick-commits
                     "/tmp/repo" "feature" sha-m "feature" t)
                    :type 'user-error))))

(ert-deftest claude-repl-test-cherry-pick-commits-auto-resolve-off-still-signals ()
  "With auto-resolve omitted (interactive `SPC TAB m'/`SPC TAB M' path),
conflicts abort the cherry-pick and signal user-error.  Guards against
the optional auto-resolve parameter accidentally flipping the default
for existing callers."
  (let ((sha-m "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
        (in-progress t)
        (resolver-called nil))
    (cl-letf (((symbol-function 'claude-repl--git-string)
               (lambda (&rest args)
                 (pcase args
                   (`("-C" "/tmp/repo" "rev-list" "--count" ,_) "1")
                   (_ (error "unmocked git-string args: %S" args)))))
              ((symbol-function 'claude-repl--git-exit-code)
               (lambda (_root &rest args)
                 (pcase args
                   (`("cherry-pick" "-x" ,_) 1)
                   (_ (error "unmocked git-exit-code args: %S" args)))))
              ((symbol-function 'claude-repl--cherry-pick-in-progress-p)
               (lambda (_root) in-progress))
              ((symbol-function 'claude-repl--auto-resolve-cherry-pick-conflict)
               (lambda (&rest _args) (setq resolver-called t) t))
              ((symbol-function 'claude-repl--check-cherry-pick-conflict)
               (lambda (_ws _root _target-ws)
                 (setq in-progress nil)
                 (user-error "Conflict cherry-picking — aborted"))))
      ;; No auto-resolve arg passed → the resolver MUST NOT be consulted.
      (should-error (claude-repl--cherry-pick-commits
                     "/tmp/repo" "feature" sha-m "feature")
                    :type 'user-error)
      (should-not resolver-called))))

;;;; ---- Tests: silent-mode conflict surfacing ----

(ert-deftest claude-repl-test-cherry-pick-commits-silent-conflict-surfaces-not-aborts ()
  "When SILENT=t and the resolver declines, the conflict is surfaced via
`--surface-silent-merge-conflict' (switch + magit pop + signal) instead
of being aborted via `--check-cherry-pick-conflict'."
  (let ((sha-m "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
        (in-progress t)
        (surface-called nil)
        (abort-called nil))
    (cl-letf (((symbol-function 'claude-repl--git-string)
               (lambda (&rest args)
                 (pcase args
                   (`("-C" "/tmp/repo" "rev-list" "--count" ,_) "1")
                   (_ (error "unmocked git-string args: %S" args)))))
              ((symbol-function 'claude-repl--git-exit-code)
               (lambda (_root &rest args)
                 (pcase args
                   (`("cherry-pick" "-x" ,_) 1)
                   (_ (error "unmocked git-exit-code args: %S" args)))))
              ((symbol-function 'claude-repl--cherry-pick-in-progress-p)
               (lambda (_root) in-progress))
              ((symbol-function 'claude-repl--auto-resolve-cherry-pick-conflict)
               (lambda (&rest _args) nil))
              ((symbol-function 'claude-repl--surface-silent-merge-conflict)
               (lambda (_ws _root)
                 (setq surface-called t
                       in-progress nil)
                 (user-error "surfaced")))
              ((symbol-function 'claude-repl--check-cherry-pick-conflict)
               (lambda (&rest _) (setq abort-called t))))
      (should-error (claude-repl--cherry-pick-commits
                     "/tmp/repo" "feature" sha-m "feature" t t)
                    :type 'user-error)
      (should surface-called)
      (should-not abort-called))))

(ert-deftest claude-repl-test-cherry-pick-commits-non-silent-conflict-aborts ()
  "When SILENT=nil and the resolver declines, the conflict is aborted
via `--check-cherry-pick-conflict' (existing behavior) — the surface
helper is not invoked.  Guards the interactive `SPC TAB M' path."
  (let ((sha-m "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
        (in-progress t)
        (surface-called nil))
    (cl-letf (((symbol-function 'claude-repl--git-string)
               (lambda (&rest args)
                 (pcase args
                   (`("-C" "/tmp/repo" "rev-list" "--count" ,_) "1")
                   (_ (error "unmocked git-string args: %S" args)))))
              ((symbol-function 'claude-repl--git-exit-code)
               (lambda (_root &rest args)
                 (pcase args
                   (`("cherry-pick" "-x" ,_) 1)
                   (_ (error "unmocked git-exit-code args: %S" args)))))
              ((symbol-function 'claude-repl--cherry-pick-in-progress-p)
               (lambda (_root) in-progress))
              ((symbol-function 'claude-repl--auto-resolve-cherry-pick-conflict)
               (lambda (&rest _args) nil))
              ((symbol-function 'claude-repl--surface-silent-merge-conflict)
               (lambda (&rest _) (setq surface-called t)))
              ((symbol-function 'claude-repl--check-cherry-pick-conflict)
               (lambda (_ws _root _target-ws)
                 (setq in-progress nil)
                 (user-error "Conflict cherry-picking — aborted"))))
      (should-error (claude-repl--cherry-pick-commits
                     "/tmp/repo" "feature" sha-m "feature" t nil)
                    :type 'user-error)
      (should-not surface-called))))

(ert-deftest claude-repl-test-surface-silent-merge-conflict-pops-magit-and-signals ()
  "`--surface-silent-merge-conflict' switches to ROOT, pops magit-status
there, then signals `user-error'.  Does NOT call `git cherry-pick
--abort' — that would erase the conflict the user is being asked to
inspect."
  (let ((switched-to nil)
        (magit-pop-root nil)
        (abort-called nil))
    (cl-letf (((symbol-function 'claude-repl--git-string)
               (lambda (&rest args)
                 (pcase args
                   (`("-C" "/tmp/repo" "rev-parse" "--short" "CHERRY_PICK_HEAD") "abcd123")
                   (_ (error "unmocked git-string args: %S" args)))))
              ((symbol-function 'claude-repl--git-exit-code)
               (lambda (&rest _args) (setq abort-called t) 0))
              ;; The function defers UI ops; run the thunk synchronously so
              ;; the test can assert the switch and magit-status calls.
              ((symbol-function 'claude-repl--defer-to-main-thread)
               (lambda (thunk) (funcall thunk)))
              ((symbol-function 'claude-repl-switch-to-project)
               (lambda (dir) (setq switched-to dir)))
              ((symbol-function 'magit-status)
               (lambda (dir) (setq magit-pop-root dir))))
      (should-error (claude-repl--surface-silent-merge-conflict
                     "feature" "/tmp/repo")
                    :type 'user-error)
      (should (equal switched-to "/tmp/repo"))
      (should (equal magit-pop-root "/tmp/repo"))
      ;; --abort must NOT have been issued — the conflict stays inspectable.
      (should-not abort-called))))

(ert-deftest claude-repl-test-surface-silent-merge-conflict-defers-ui-ops ()
  "UI ops (perspective switch + magit-status) are routed through
`claude-repl--defer-to-main-thread'.  This is what makes the function
safe to call from the worker thread spawned by
`claude-repl--workspace-merge-async' — direct UI calls from a worker
thread are undefined behavior in Emacs."
  (let ((defer-calls 0))
    (cl-letf (((symbol-function 'claude-repl--git-string)
               (lambda (&rest _args) "abcd123"))
              ((symbol-function 'claude-repl--defer-to-main-thread)
               (lambda (_thunk) (cl-incf defer-calls))))
      (should-error (claude-repl--surface-silent-merge-conflict
                     "feature" "/tmp/repo")
                    :type 'user-error)
      (should (= defer-calls 1)))))

;;;; ---- Tests: resolver output is preserved in a side buffer ----

(ert-deftest claude-repl-test-invoke-auto-resolve-claude-preserves-output-buffer ()
  "When TARGET-WS is supplied, `--invoke-auto-resolve-claude' leaves the
side buffer alive after the process exits so the user can post-mortem
the resolver's stdout/stderr + exit code."
  (let ((claude-repl-auto-resolve-conflicts-program "true")
        (claude-repl-auto-resolve-conflicts-model "test-model")
        (claude-repl-auto-resolve-conflicts-extra-args nil)
        (claude-repl-auto-resolve-conflicts-timeout 5)
        (ws "feature-x"))
    (let ((buf-name (claude-repl--merge-resolver-buffer-name ws)))
      (when (get-buffer buf-name) (kill-buffer buf-name))
      (unwind-protect
          (let ((result (claude-repl--invoke-auto-resolve-claude
                         default-directory "prompt-body" ws)))
            (should (equal result 0))
            (should (buffer-live-p (get-buffer buf-name)))
            (with-current-buffer buf-name
              (let ((content (buffer-string)))
                (should (string-match-p "merge resolver" content))
                (should (string-match-p "feature-x" content))
                (should (string-match-p "exit: 0" content)))))
        (when (get-buffer buf-name) (kill-buffer buf-name))))))

(ert-deftest claude-repl-test-invoke-auto-resolve-claude-no-target-ws-kills-temp-buffer ()
  "Legacy callers (no TARGET-WS argument) get the old behavior: the
temp buffer is killed after the process completes, so we don't leak
anonymous \" *claude-auto-resolve*\" buffers."
  (let ((claude-repl-auto-resolve-conflicts-program "true")
        (claude-repl-auto-resolve-conflicts-model "test-model")
        (claude-repl-auto-resolve-conflicts-extra-args nil)
        (claude-repl-auto-resolve-conflicts-timeout 5)
        (pre-count (length (cl-remove-if-not
                            (lambda (b) (string-prefix-p " *claude-auto-resolve*"
                                                         (buffer-name b)))
                            (buffer-list)))))
    (let ((result (claude-repl--invoke-auto-resolve-claude
                   default-directory "prompt-body")))
      (should (equal result 0))
      (let ((post-count (length (cl-remove-if-not
                                 (lambda (b) (string-prefix-p " *claude-auto-resolve*"
                                                              (buffer-name b)))
                                 (buffer-list)))))
        (should (= pre-count post-count))))))

(ert-deftest claude-repl-test-invoke-auto-resolve-claude-erases-prior-output ()
  "A second resolver invocation overwrites the prior buffer's content
\(prefixed with the new header) instead of appending — the buffer
always reflects the most recent run."
  (let ((claude-repl-auto-resolve-conflicts-program "true")
        (claude-repl-auto-resolve-conflicts-model "test-model")
        (claude-repl-auto-resolve-conflicts-extra-args nil)
        (claude-repl-auto-resolve-conflicts-timeout 5)
        (ws "feature-erase"))
    (let ((buf-name (claude-repl--merge-resolver-buffer-name ws)))
      (when (get-buffer buf-name) (kill-buffer buf-name))
      (unwind-protect
          (progn
            (with-current-buffer (get-buffer-create buf-name)
              (let ((inhibit-read-only t))
                (insert "STALE-PREVIOUS-CONTENT\n")))
            (claude-repl--invoke-auto-resolve-claude
             default-directory "prompt-body" ws)
            (with-current-buffer buf-name
              (should-not (string-match-p "STALE-PREVIOUS-CONTENT"
                                          (buffer-string)))))
        (when (get-buffer buf-name) (kill-buffer buf-name))))))

;;;; ---- Tests: handle-merge-command auto-resolve gating ----

(ert-deftest claude-repl-test-handle-merge-command-passes-auto-resolve ()
  "Skill-invoked `/workspace-merge' passes AUTO-RESOLVE=t to
workspace-merge-into-source so cherry-pick conflicts are sent to the
headless resolver — interactive paths leave it nil."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "feature-one" :project-dir "/tmp/feature-one")
    (let ((auto-arg :unset))
      (cl-letf (((symbol-function 'claude-repl--workspace-merge-into-source)
                 (lambda (_ws &optional _silent auto) (setq auto-arg auto))))
        (claude-repl--handle-merge-command
         '((type . "merge") (workspace . "feature-one")))
        (should (eq auto-arg t))))))

;;;; ---- Tests: workspace-merge-async ----

(ert-deftest claude-repl-test-workspace-merge-async-closes-workspace-with-preserve-entry ()
  "Async wrapper closes the workspace UI FIRST (with `preserve-entry') so
the user is freed from it immediately on keystroke return.  Preserve-entry
keeps `:project-dir' alive so the reopen-on-failure path can find it."
  (let ((close-args nil))
    (cl-letf (((symbol-function 'claude-repl--workspace-merge-async)
               claude-repl-test--orig-workspace-merge-async)
              ((symbol-function 'claude-repl--close-workspace)
               (lambda (ws preserve) (setq close-args (list ws preserve))))
              ((symbol-function 'make-thread)
               (lambda (_thunk &optional _name) nil)))
      (claude-repl--workspace-merge-async "ws1" "/tmp/repo"))
    (should (equal close-args (list "ws1" 'preserve-entry)))))

(ert-deftest claude-repl-test-workspace-merge-async-spawns-worker-thread ()
  "Async wrapper spawns a `make-thread' (not a synchronous call) so Emacs
stays responsive while `claude -p' runs in the merge body — threads yield
during `accept-process-output' so the main UI keeps ticking."
  (let ((thread-spawned nil))
    (cl-letf (((symbol-function 'claude-repl--workspace-merge-async)
               claude-repl-test--orig-workspace-merge-async)
              ((symbol-function 'claude-repl--close-workspace) #'ignore)
              ((symbol-function 'make-thread)
               (lambda (_thunk &optional _name) (setq thread-spawned t) nil)))
      (claude-repl--workspace-merge-async "ws1" "/tmp/repo"))
    (should thread-spawned)))

(ert-deftest claude-repl-test-workspace-merge-async-thread-runs-dispatch-handler ()
  "Inside the worker thread the wrapper invokes `--dispatch-merge-handler'
\(the standard handler-routing entry).  This is what makes the two entry
points (interactive `SPC TAB M' and `/workspace-merge' skill) equivalent —
both end up here via the same dispatch."
  (let ((dispatch-args nil))
    (cl-letf (((symbol-function 'claude-repl--workspace-merge-async)
               claude-repl-test--orig-workspace-merge-async)
              ((symbol-function 'claude-repl--close-workspace) #'ignore)
              ;; Run the thread body inline so we can observe the dispatch
              ;; call without thread-join machinery.
              ((symbol-function 'make-thread)
               (lambda (thunk &optional _name) (funcall thunk) nil))
              ((symbol-function 'claude-repl--dispatch-merge-handler)
               (lambda (ws repo-root)
                 (setq dispatch-args (list ws repo-root)))))
      (claude-repl--workspace-merge-async "ws1" "/tmp/repo"))
    (should (equal dispatch-args (list "ws1" "/tmp/repo")))))

(ert-deftest claude-repl-test-workspace-merge-async-on-error-schedules-reopen ()
  "When the dispatch handler signals (conflict or any error), the wrapper's
condition-case catches it and schedules `--reopen-workspace-from-state'
on the main thread via `run-at-time'.  Without this, a failed merge
leaves the user with a closed workspace and no way to recover."
  (let ((scheduled nil))
    (cl-letf (((symbol-function 'claude-repl--workspace-merge-async)
               claude-repl-test--orig-workspace-merge-async)
              ((symbol-function 'claude-repl--close-workspace) #'ignore)
              ((symbol-function 'make-thread)
               (lambda (thunk &optional _name) (funcall thunk) nil))
              ((symbol-function 'claude-repl--dispatch-merge-handler)
               (lambda (&rest _) (error "boom")))
              ((symbol-function 'run-at-time)
               (lambda (_when _repeat thunk)
                 (push thunk scheduled))))
      (claude-repl--workspace-merge-async "ws1" "/tmp/repo"))
    (should (= (length scheduled) 1))
    ;; Invoking the scheduled thunk should call --reopen-workspace-from-state
    ;; with the workspace name.
    (let ((reopened nil))
      (cl-letf (((symbol-function 'claude-repl--reopen-workspace-from-state)
                 (lambda (ws) (setq reopened ws))))
        (funcall (car scheduled)))
      (should (equal reopened "ws1")))))

(ert-deftest claude-repl-test-workspace-merge-async-on-success-does-not-schedule-reopen ()
  "When dispatch completes without signaling, the wrapper does NOT schedule
a reopen — the merge body's own deferred teardown is the cleanup path."
  (let ((scheduled nil))
    (cl-letf (((symbol-function 'claude-repl--workspace-merge-async)
               claude-repl-test--orig-workspace-merge-async)
              ((symbol-function 'claude-repl--close-workspace) #'ignore)
              ((symbol-function 'make-thread)
               (lambda (thunk &optional _name) (funcall thunk) nil))
              ((symbol-function 'claude-repl--dispatch-merge-handler) #'ignore)
              ((symbol-function 'run-at-time)
               (lambda (_when _repeat thunk) (push thunk scheduled))))
      (claude-repl--workspace-merge-async "ws1" "/tmp/repo"))
    (should-not scheduled)))

;;;; ---- Tests: reopen-workspace-from-state ----

(ert-deftest claude-repl-test-reopen-workspace-from-state-establishes-from-project-dir ()
  "Reopen wraps `claude-repl--establish-workspace' with the preserved
`:project-dir' so a workspace closed with preserve-entry can be brought
back without callers having to know the snapshot/establish protocol."
  (claude-repl-test--with-clean-state
    (let ((established nil))
      (claude-repl--ws-put "ws1" :project-dir "/tmp/saved-dir/")
      (cl-letf (((symbol-function 'claude-repl--establish-workspace)
                 (lambda (ws dir) (setq established (list ws dir)))))
        (claude-repl--reopen-workspace-from-state "ws1"))
      (should (equal established (list "ws1" "/tmp/saved-dir/"))))))

(ert-deftest claude-repl-test-reopen-workspace-from-state-noops-without-project-dir ()
  "When the workspace plist has no `:project-dir' (entry was finalized or
never preserved), reopen is a no-op — `--establish-workspace' is not
called, no error is signaled.  This is the safe path for callers that
might invoke reopen on a workspace whose state was already swept."
  (claude-repl-test--with-clean-state
    (let ((established nil))
      ;; Don't put :project-dir; entry is empty.
      (claude-repl--ws-put "ws1" :some-other-key 'something)
      (cl-letf (((symbol-function 'claude-repl--establish-workspace)
                 (lambda (&rest _) (setq established t))))
        (claude-repl--reopen-workspace-from-state "ws1"))
      (should-not established))))

(ert-deftest claude-repl-test-reopen-workspace-from-state-normalizes-branchy-name ()
  "Branch-style name like `DWC/foo' normalizes to the bare `foo' before
lookup — the registry is keyed by bare names so the lookup must agree."
  (claude-repl-test--with-clean-state
    (let ((established nil))
      (claude-repl--ws-put "foo" :project-dir "/tmp/foo-dir/")
      (cl-letf (((symbol-function 'claude-repl--establish-workspace)
                 (lambda (ws dir) (setq established (list ws dir)))))
        (claude-repl--reopen-workspace-from-state "DWC/foo"))
      (should (equal established (list "foo" "/tmp/foo-dir/"))))))

;;;; ---- Tests: finish-workspace ----

(ert-deftest claude-repl-test-finish-workspace-non-worktree ()
  "Finishing a non-worktree workspace tombstones state and kills persp.
Post-tombstone-refactor, finish-workspace no longer removes the hash
entry — it stamps `:nuked-at' via `--ws-del'.  This test pins both the
persp-kill and the resulting tombstone marker."
  (claude-repl-test--with-clean-state
    (let ((persp-killed nil))
      (claude-repl--ws-put "ws1" :project-dir "/tmp/fake")
      (cl-letf (((symbol-function 'claude-repl--kill-vterm-process) (lambda (_b) nil))
                ((symbol-function '+workspace-list-names) (lambda () '("ws1" "ws2")))
                ((symbol-function 'persp-kill) (lambda (ws) (setq persp-killed ws))))
        (claude-repl--finish-workspace "ws1")
        (should (equal persp-killed "ws1"))
        ;; Tombstoned: entry survives with `:nuked-at', not live.
        (should (claude-repl--ws-get "ws1" :nuked-at))
        (should-not (claude-repl--ws-live-p "ws1"))))))

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
  "Branch-style name 'DWC/foo' is normalized to 'foo' before tombstoning.
The post-refactor invariant is that the hash entry is tombstoned (not
removed); we pin both the name normalization and the resulting
liveness flip."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "foo" :project-dir "/tmp/fake")
    (cl-letf (((symbol-function 'claude-repl--kill-vterm-process) (lambda (_b) nil))
              ((symbol-function '+workspace-list-names) (lambda () '("foo")))
              ((symbol-function 'persp-kill) (lambda (_ws) nil)))
      (claude-repl--finish-workspace "DWC/foo")
      (should-not (claude-repl--ws-live-p "foo"))
      (should (claude-repl--ws-get "foo" :nuked-at)))))

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

;;;; ---- Tests: inherit-priority-from-source ----

(ert-deftest claude-repl-test-inherit-priority-explicit-wins ()
  "When PRIORITY is non-nil, it is returned unchanged regardless of SOURCE-DIR."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "parent" :project-dir "/tmp/parent/")
    (claude-repl--ws-put "parent" :priority "p1")
    (cl-letf (((symbol-function 'claude-repl--path-canonical) #'identity))
      (should (equal (claude-repl--inherit-priority-from-source "p2" "/tmp/parent/")
                     "p2")))))

(ert-deftest claude-repl-test-inherit-priority-nil-source-dir ()
  "When SOURCE-DIR is nil, returns nil even with no priority set."
  (should-not (claude-repl--inherit-priority-from-source nil nil)))

(ert-deftest claude-repl-test-inherit-priority-unknown-source-dir ()
  "When SOURCE-DIR does not resolve to any workspace, returns nil."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'claude-repl--path-canonical) #'identity))
      (should-not (claude-repl--inherit-priority-from-source nil "/tmp/nowhere/")))))

(ert-deftest claude-repl-test-inherit-priority-source-without-priority ()
  "Source workspace exists but has no :priority — returns nil."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "parent" :project-dir "/tmp/parent/")
    (cl-letf (((symbol-function 'claude-repl--path-canonical) #'identity))
      (should-not (claude-repl--inherit-priority-from-source nil "/tmp/parent/")))))

(ert-deftest claude-repl-test-inherit-priority-source-has-priority ()
  "Source workspace has :priority — returns it when PRIORITY is nil."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "parent" :project-dir "/tmp/parent/")
    (claude-repl--ws-put "parent" :priority "p05")
    (cl-letf (((symbol-function 'claude-repl--path-canonical) #'identity))
      (should (equal (claude-repl--inherit-priority-from-source nil "/tmp/parent/")
                     "p05")))))

;;;; ---- Tests: finalize-worktree-workspace child inherits parent priority ----

(ert-deftest claude-repl-test-finalize-child-inherits-parent-priority ()
  "When PRIORITY is nil and SOURCE-DIR points at a workspace, child inherits its priority."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "parent" :project-dir "/tmp/parent/")
    (claude-repl--ws-put "parent" :priority "p1")
    (cl-letf (((symbol-function 'claude-repl--register-projectile-project)
               (lambda (&rest _) nil))
              ((symbol-function '+workspace-new) (lambda (_ws) nil))
              ((symbol-function 'claude-repl--setup-worktree-session)
               (lambda (&rest _) nil))
              ((symbol-function 'claude-repl--reorder-workspace-by-priority)
               (lambda (&rest _) nil))
              ((symbol-function 'claude-repl--path-canonical) #'identity))
      (claude-repl--finalize-worktree-workspace
       "/tmp/new-wt" "child-ws" nil nil nil nil nil "/tmp/parent/")
      (should (equal (claude-repl--ws-get "child-ws" :priority) "p1")))))

(ert-deftest claude-repl-test-finalize-child-explicit-priority-wins ()
  "When PRIORITY is provided, it overrides any source workspace priority."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "parent" :project-dir "/tmp/parent/")
    (claude-repl--ws-put "parent" :priority "p1")
    (cl-letf (((symbol-function 'claude-repl--register-projectile-project)
               (lambda (&rest _) nil))
              ((symbol-function '+workspace-new) (lambda (_ws) nil))
              ((symbol-function 'claude-repl--setup-worktree-session)
               (lambda (&rest _) nil))
              ((symbol-function 'claude-repl--reorder-workspace-by-priority)
               (lambda (&rest _) nil))
              ((symbol-function 'claude-repl--path-canonical) #'identity))
      (claude-repl--finalize-worktree-workspace
       "/tmp/new-wt" "child-ws" nil "p3" nil nil nil "/tmp/parent/")
      (should (equal (claude-repl--ws-get "child-ws" :priority) "p3")))))

(ert-deftest claude-repl-test-finalize-no-parent-priority-stays-nil ()
  "When PRIORITY is nil and source workspace has no priority, child has no priority."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "parent" :project-dir "/tmp/parent/")
    (cl-letf (((symbol-function 'claude-repl--register-projectile-project)
               (lambda (&rest _) nil))
              ((symbol-function '+workspace-new) (lambda (_ws) nil))
              ((symbol-function 'claude-repl--setup-worktree-session)
               (lambda (&rest _) nil))
              ((symbol-function 'claude-repl--reorder-workspace-by-priority)
               (lambda (&rest _) nil))
              ((symbol-function 'claude-repl--repo-default-priority-for-path)
               (lambda (_path) nil))
              ((symbol-function 'claude-repl--path-canonical) #'identity))
      (claude-repl--finalize-worktree-workspace
       "/tmp/new-wt" "child-ws" nil nil nil nil nil "/tmp/parent/")
      (should-not (claude-repl--ws-get "child-ws" :priority)))))

;;;; ---- Tests: finalize-worktree-workspace falls back to repo-default priority ----

(ert-deftest claude-repl-test-finalize-falls-back-to-repo-default ()
  "When PRIORITY is nil and source-workspace has no priority, falls back to repo-default."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "parent" :project-dir "/tmp/parent/")
    (cl-letf (((symbol-function 'claude-repl--register-projectile-project)
               (lambda (&rest _) nil))
              ((symbol-function '+workspace-new) (lambda (_ws) nil))
              ((symbol-function 'claude-repl--setup-worktree-session)
               (lambda (&rest _) nil))
              ((symbol-function 'claude-repl--reorder-workspace-by-priority)
               (lambda (&rest _) nil))
              ((symbol-function 'claude-repl--repo-default-priority-for-path)
               (lambda (path)
                 (when (equal path "/tmp/new-wt") "p3")))
              ((symbol-function 'claude-repl--path-canonical) #'identity))
      (claude-repl--finalize-worktree-workspace
       "/tmp/new-wt" "child-ws" nil nil nil nil nil "/tmp/parent/")
      (should (equal (claude-repl--ws-get "child-ws" :priority) "p3")))))

(ert-deftest claude-repl-test-finalize-explicit-priority-wins-over-repo-default ()
  "Explicit PRIORITY wins over the repo-default fallback."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'claude-repl--register-projectile-project)
               (lambda (&rest _) nil))
              ((symbol-function '+workspace-new) (lambda (_ws) nil))
              ((symbol-function 'claude-repl--setup-worktree-session)
               (lambda (&rest _) nil))
              ((symbol-function 'claude-repl--reorder-workspace-by-priority)
               (lambda (&rest _) nil))
              ((symbol-function 'claude-repl--repo-default-priority-for-path)
               (lambda (_path) "p3"))
              ((symbol-function 'claude-repl--path-canonical) #'identity))
      (claude-repl--finalize-worktree-workspace
       "/tmp/new-wt" "child-ws" nil "p1" nil nil nil nil)
      (should (equal (claude-repl--ws-get "child-ws" :priority) "p1")))))

(ert-deftest claude-repl-test-finalize-parent-priority-wins-over-repo-default ()
  "Source-workspace priority wins over the repo-default fallback."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "parent" :project-dir "/tmp/parent/")
    (claude-repl--ws-put "parent" :priority "p2")
    (cl-letf (((symbol-function 'claude-repl--register-projectile-project)
               (lambda (&rest _) nil))
              ((symbol-function '+workspace-new) (lambda (_ws) nil))
              ((symbol-function 'claude-repl--setup-worktree-session)
               (lambda (&rest _) nil))
              ((symbol-function 'claude-repl--reorder-workspace-by-priority)
               (lambda (&rest _) nil))
              ((symbol-function 'claude-repl--repo-default-priority-for-path)
               (lambda (_path) "p3"))
              ((symbol-function 'claude-repl--path-canonical) #'identity))
      (claude-repl--finalize-worktree-workspace
       "/tmp/new-wt" "child-ws" nil nil nil nil nil "/tmp/parent/")
      (should (equal (claude-repl--ws-get "child-ws" :priority) "p2")))))

;;;; ---- Tests: new-workspace applies repo-default priority ----

(ert-deftest claude-repl-test-new-workspace-applies-repo-default-priority ()
  "`--new-workspace' writes the repo-default priority onto the new ws plist."
  (claude-repl-test--with-clean-state
    (let ((ws-name "new-ws")
          (reorder-called nil))
      (cl-letf (((symbol-function 'claude-repl--git-root) (lambda (&rest _) "/tmp/ee/"))
                ((symbol-function '+workspace/new) (lambda () nil))
                ((symbol-function '+workspace-current-name) (lambda () ws-name))
                ((symbol-function 'claude-repl--initialize-ws-env)
                 (lambda (&rest _) nil))
                ((symbol-function 'claude-repl--repo-default-priority-for-path)
                 (lambda (path) (when (equal path "/tmp/ee/") "p3")))
                ((symbol-function 'claude-repl--reorder-workspace-by-priority)
                 (lambda (_ws) (setq reorder-called t)))
                ((symbol-function 'magit-status) (lambda (&rest _) nil))
                ((symbol-function 'claude-repl--remove-doom-dashboard)
                 (lambda (&rest _) nil)))
        (claude-repl--new-workspace)
        (should (equal (claude-repl--ws-get ws-name :priority) "p3"))
        (should reorder-called)))))

(ert-deftest claude-repl-test-new-workspace-no-default-leaves-priority-unset ()
  "`--new-workspace' leaves :priority unset when no repo-default applies."
  (claude-repl-test--with-clean-state
    (let ((ws-name "new-ws")
          (reorder-called nil))
      (cl-letf (((symbol-function 'claude-repl--git-root) (lambda (&rest _) "/tmp/other/"))
                ((symbol-function '+workspace/new) (lambda () nil))
                ((symbol-function '+workspace-current-name) (lambda () ws-name))
                ((symbol-function 'claude-repl--initialize-ws-env)
                 (lambda (&rest _) nil))
                ((symbol-function 'claude-repl--repo-default-priority-for-path)
                 (lambda (_path) nil))
                ((symbol-function 'claude-repl--reorder-workspace-by-priority)
                 (lambda (_ws) (setq reorder-called t)))
                ((symbol-function 'magit-status) (lambda (&rest _) nil))
                ((symbol-function 'claude-repl--remove-doom-dashboard)
                 (lambda (&rest _) nil)))
        (claude-repl--new-workspace)
        (should-not (claude-repl--ws-get ws-name :priority))
        (should-not reorder-called)))))

(ert-deftest claude-repl-test-new-workspace-priority-set-before-initialize-ws-env ()
  "`--new-workspace' writes :priority BEFORE calling `--initialize-ws-env'.
This matters because `--initialize-ws-env' reads `:priority' off the plist as
a fallback when no saved state exists, persisting the repo-default into the
initial state file."
  (claude-repl-test--with-clean-state
    (let ((ws-name "new-ws")
          (priority-at-init nil))
      (cl-letf (((symbol-function 'claude-repl--git-root) (lambda (&rest _) "/tmp/ee/"))
                ((symbol-function '+workspace/new) (lambda () nil))
                ((symbol-function '+workspace-current-name) (lambda () ws-name))
                ((symbol-function 'claude-repl--initialize-ws-env)
                 (lambda (ws &rest _)
                   (setq priority-at-init (claude-repl--ws-get ws :priority))))
                ((symbol-function 'claude-repl--repo-default-priority-for-path)
                 (lambda (_path) "p3"))
                ((symbol-function 'claude-repl--reorder-workspace-by-priority)
                 (lambda (_ws) nil))
                ((symbol-function 'magit-status) (lambda (&rest _) nil))
                ((symbol-function 'claude-repl--remove-doom-dashboard)
                 (lambda (&rest _) nil)))
        (claude-repl--new-workspace)
        (should (equal priority-at-init "p3"))))))

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

(ert-deftest claude-repl-test-handle-create-command-missing-name-refuses ()
  "handle-create-command with no `name' key must refuse — a missing name
would otherwise leak a phantom \"none\" / \"nil\" workspace into the
registry once `--bare-workspace-name' is called on it downstream."
  (claude-repl-test--with-clean-state
    (let ((timer-scheduled nil))
      (cl-letf (((symbol-function 'run-with-timer)
                 (lambda (_delay _repeat _fn &rest _args)
                   (setq timer-scheduled t))))
        (claude-repl--handle-create-command
         '((type . "create") (git_root . "/fake/root"))
         0)
        (should-not timer-scheduled)))))

(ert-deftest claude-repl-test-handle-create-command-null-name-refuses ()
  "handle-create-command with JSON `null' name (parsed as `:null') must refuse."
  (claude-repl-test--with-clean-state
    (let ((timer-scheduled nil))
      (cl-letf (((symbol-function 'run-with-timer)
                 (lambda (_delay _repeat _fn &rest _args)
                   (setq timer-scheduled t))))
        (claude-repl--handle-create-command
         '((type . "create") (name . :null) (git_root . "/fake/root"))
         0)
        (should-not timer-scheduled)))))

(ert-deftest claude-repl-test-handle-create-command-empty-name-refuses ()
  "handle-create-command with an empty-string `name' must refuse."
  (claude-repl-test--with-clean-state
    (let ((timer-scheduled nil))
      (cl-letf (((symbol-function 'run-with-timer)
                 (lambda (_delay _repeat _fn &rest _args)
                   (setq timer-scheduled t))))
        (claude-repl--handle-create-command
         '((type . "create") (name . "") (git_root . "/fake/root"))
         0)
        (should-not timer-scheduled)))))

(ert-deftest claude-repl-test-handle-create-command-persp-nil-name-refuses ()
  "handle-create-command with a bare `name' equal to `persp-nil-name' must
refuse.  The headless `/workspace-generation' flow occasionally emits
\"none\" (or \"DWC/none\") when there is no slug material; without this
guard, the downstream `+workspace-new' would collide with the
nil-perspective sentinel and the entry would surface in the drawer and
nuke prompts as a stray \"none\" workspace."
  (claude-repl-test--with-clean-state
    (let ((timer-scheduled nil)
          (persp-nil-name "none"))
      (cl-letf (((symbol-function 'run-with-timer)
                 (lambda (_delay _repeat _fn &rest _args)
                   (setq timer-scheduled t))))
        (claude-repl--handle-create-command
         '((type . "create") (name . "none") (git_root . "/fake/root"))
         0)
        (should-not timer-scheduled)))))

(ert-deftest claude-repl-test-handle-create-command-dwc-persp-nil-name-refuses ()
  "handle-create-command must refuse a `DWC/none' name because the bare
form collides with `persp-nil-name'."
  (claude-repl-test--with-clean-state
    (let ((timer-scheduled nil)
          (persp-nil-name "none"))
      (cl-letf (((symbol-function 'run-with-timer)
                 (lambda (_delay _repeat _fn &rest _args)
                   (setq timer-scheduled t))))
        (claude-repl--handle-create-command
         '((type . "create") (name . "DWC/none") (git_root . "/fake/root"))
         0)
        (should-not timer-scheduled)))))

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

(ert-deftest claude-repl-test-create-worktree-from-command-master-base-uses-master-worktree-as-source-dir ()
  "BASE-COMMIT = `master' resolves source-dir via `--master-worktree-path'.
For `SPC TAB N', the new workspace's `:source-ws-dir' must be the master
worktree of the repo, not the calling workspace — otherwise the drawer
nests it under a parent that shares no commits with it."
  (claude-repl-test--with-clean-state
    (let ((captured-source-dir :unset)
          (master-lookup-root :unset)
          (claude-repl-master-branch-name "master"))
      (cl-letf (((symbol-function 'claude-repl--master-worktree-path)
                 (lambda (root)
                   (setq master-lookup-root root)
                   "/tmp/master/"))
                ((symbol-function 'claude-repl--do-create-worktree-workspace)
                 (lambda (_name _bare _fork _prompt _cb _priority _base _git source-dir)
                   (setq captured-source-dir source-dir))))
        (claude-repl--create-worktree-from-command
         "/tmp/calling-ws/" "name" "prompt" 5 nil "master")
        (should (equal master-lookup-root "/tmp/calling-ws/"))
        (should (equal captured-source-dir "/tmp/master/"))))))

(ert-deftest claude-repl-test-create-worktree-from-command-master-base-nil-master-yields-nil-source-dir ()
  "BASE-COMMIT = `master' with no master worktree yields nil `:source-ws-dir'.
When the repo has no worktree on master (e.g. main checkout itself is on
a feature branch), the new workspace must not fall back to the calling
workspace as its parent.  Nil leaves it parentless in the drawer — the
correct outcome since `SPC TAB N' branches off master, not the caller."
  (claude-repl-test--with-clean-state
    (let ((captured-source-dir :unset)
          (claude-repl-master-branch-name "master"))
      (cl-letf (((symbol-function 'claude-repl--master-worktree-path)
                 (lambda (_root) nil))
                ((symbol-function 'claude-repl--do-create-worktree-workspace)
                 (lambda (_name _bare _fork _prompt _cb _priority _base _git source-dir)
                   (setq captured-source-dir source-dir))))
        (claude-repl--create-worktree-from-command
         "/tmp/calling-ws/" "name" "prompt" 5 nil "master")
        (should (null captured-source-dir))))))

(ert-deftest claude-repl-test-create-worktree-from-command-head-base-uses-git-root-as-source-dir ()
  "BASE-COMMIT = `HEAD' yields source-dir == git-root (calling workspace).
`SPC TAB n' is a child-of-current operation; its drawer parent must be
the calling workspace, captured as GIT-ROOT at enqueue time."
  (claude-repl-test--with-clean-state
    (let ((captured-source-dir :unset)
          (master-path-called nil))
      (cl-letf (((symbol-function 'claude-repl--master-worktree-path)
                 (lambda (_root)
                   (setq master-path-called t)
                   "/tmp/master/"))
                ((symbol-function 'claude-repl--do-create-worktree-workspace)
                 (lambda (_name _bare _fork _prompt _cb _priority _base _git source-dir)
                   (setq captured-source-dir source-dir))))
        (claude-repl--create-worktree-from-command
         "/tmp/calling-ws/" "name" "prompt" 5 nil "HEAD")
        (should-not master-path-called)
        (should (equal captured-source-dir "/tmp/calling-ws/"))))))

(ert-deftest claude-repl-test-create-worktree-from-command-nil-base-commit-uses-git-root-as-source-dir ()
  "Absent BASE-COMMIT yields source-dir == git-root.
With no base hint, source-dir defaults to the calling workspace dir —
the master special case only kicks in for an explicit `master' value."
  (claude-repl-test--with-clean-state
    (let ((captured-source-dir :unset)
          (master-path-called nil))
      (cl-letf (((symbol-function 'claude-repl--master-worktree-path)
                 (lambda (_root)
                   (setq master-path-called t)
                   "/tmp/master/"))
                ((symbol-function 'claude-repl--do-create-worktree-workspace)
                 (lambda (_name _bare _fork _prompt _cb _priority _base _git source-dir)
                   (setq captured-source-dir source-dir))))
        (claude-repl--create-worktree-from-command
         "/tmp/calling-ws/" "name" "prompt" 5 nil nil)
        (should-not master-path-called)
        (should (equal captured-source-dir "/tmp/calling-ws/"))))))

(ert-deftest claude-repl-test-create-worktree-from-command-origin-master-base-uses-git-root-as-source-dir ()
  "BASE-COMMIT = `origin/master' (or any non-master ref) yields source-dir == git-root.
Only the literal `claude-repl-master-branch-name' triggers master-worktree
lookup; arbitrary refs like `origin/master' or a SHA route through the
default path so the parent is the originating workspace."
  (claude-repl-test--with-clean-state
    (let ((captured-source-dir :unset)
          (master-path-called nil)
          (claude-repl-master-branch-name "master"))
      (cl-letf (((symbol-function 'claude-repl--master-worktree-path)
                 (lambda (_root)
                   (setq master-path-called t)
                   "/tmp/master/"))
                ((symbol-function 'claude-repl--do-create-worktree-workspace)
                 (lambda (_name _bare _fork _prompt _cb _priority _base _git source-dir)
                   (setq captured-source-dir source-dir))))
        (claude-repl--create-worktree-from-command
         "/tmp/calling-ws/" "name" "prompt" 5 nil "origin/master")
        (should-not master-path-called)
        (should (equal captured-source-dir "/tmp/calling-ws/"))))))

(ert-deftest claude-repl-test-create-worktree-from-command-master-base-honors-custom-branch-name ()
  "Source-dir resolution honors a custom `claude-repl-master-branch-name'.
When the trunk is named `trunk' rather than `master', BASE-COMMIT = `trunk'
triggers the master-worktree lookup."
  (claude-repl-test--with-clean-state
    (let ((captured-source-dir :unset)
          (master-path-called nil)
          (claude-repl-master-branch-name "trunk"))
      (cl-letf (((symbol-function 'claude-repl--master-worktree-path)
                 (lambda (_root)
                   (setq master-path-called t)
                   "/tmp/trunk/"))
                ((symbol-function 'claude-repl--do-create-worktree-workspace)
                 (lambda (_name _bare _fork _prompt _cb _priority _base _git source-dir)
                   (setq captured-source-dir source-dir))))
        (claude-repl--create-worktree-from-command
         "/tmp/calling-ws/" "name" "prompt" 5 nil "trunk")
        (should master-path-called)
        (should (equal captured-source-dir "/tmp/trunk/"))))))

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
                ((symbol-function 'claude-repl--master-worktree-path)
                 (lambda (_root) nil))
                ((symbol-function 'read-string)
                 (lambda (&rest _) "do the thing"))
                ((symbol-function 'claude-repl--spawn-workspace-generation)
                 (lambda (_raw _prefixed _git-root base _fork-from)
                   (setq captured-base base))))
        (claude-repl-create-worktree-workspace 'master)
        (should (equal captured-base "master"))))))

(ert-deftest claude-repl-test-create-worktree-workspace-master-base-does-not-anchor-spawn-git-root ()
  "BASE = `master' passes the calling-ws git-root to spawn (no master anchoring).
Source-dir resolution now happens at receive time in
`claude-repl--create-worktree-from-command' based on BASE-COMMIT, so the
spawn-side no longer special-cases `master' for the git-root.  Keeping
git-root anchored to calling-ws means the JSON command file's `git_root'
reflects the user's actual context, with no master-worktree-path lookup
to fail."
  (claude-repl-test--with-clean-state
    (let ((captured-git-root :unset)
          (master-path-called nil))
      (cl-letf (((symbol-function '+workspace-current-name)
                 (lambda () "calling-ws"))
                ((symbol-function 'claude-repl--ws-dir)
                 (lambda (ws)
                   (if (equal ws "calling-ws") "/tmp/calling-ws/"
                     (error "unexpected ws: %s" ws))))
                ((symbol-function 'claude-repl--master-worktree-path)
                 (lambda (_root)
                   (setq master-path-called t)
                   "/tmp/master/"))
                ((symbol-function 'read-string)
                 (lambda (&rest _) "do the thing"))
                ((symbol-function 'claude-repl--spawn-workspace-generation)
                 (lambda (_raw _prefixed git-root _base _fork-from)
                   (setq captured-git-root git-root))))
        (claude-repl-create-worktree-workspace 'master)
        (should-not master-path-called)
        (should (equal captured-git-root "/tmp/calling-ws/"))))))

(ert-deftest claude-repl-test-create-worktree-workspace-head-base-passes-calling-ws-git-root ()
  "BASE = `head' passes calling-ws git-root and never consults master resolver."
  (claude-repl-test--with-clean-state
    (let ((captured-git-root :unset)
          (master-path-called nil))
      (cl-letf (((symbol-function '+workspace-current-name)
                 (lambda () "calling-ws"))
                ((symbol-function 'claude-repl--ws-dir)
                 (lambda (ws)
                   (if (equal ws "calling-ws") "/tmp/calling-ws/"
                     (error "unexpected ws: %s" ws))))
                ((symbol-function 'claude-repl--master-worktree-path)
                 (lambda (_root)
                   (setq master-path-called t)
                   "/tmp/master/"))
                ((symbol-function 'read-string)
                 (lambda (&rest _) "do the thing"))
                ((symbol-function 'claude-repl--spawn-workspace-generation)
                 (lambda (_raw _prefixed git-root _base _fork-from)
                   (setq captured-git-root git-root))))
        (claude-repl-create-worktree-workspace 'head)
        (should-not master-path-called)
        (should (equal captured-git-root "/tmp/calling-ws/"))))))

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

;;;; ---- Tests: start-tag-name ----

(ert-deftest claude-repl-test-start-tag-name-default-prefix ()
  "Default prefix `start/' produces start/<branch>."
  (let ((claude-repl-worktree-start-tag-prefix "start/"))
    (should (equal (claude-repl--start-tag-name "DC/feature") "start/DC/feature"))))

(ert-deftest claude-repl-test-start-tag-name-nil-prefix ()
  "Nil prefix means start tags are disabled — returns nil."
  (let ((claude-repl-worktree-start-tag-prefix nil))
    (should (null (claude-repl--start-tag-name "any")))))

(ert-deftest claude-repl-test-start-tag-name-empty-prefix ()
  "Empty prefix is treated as disabled — returns nil."
  (let ((claude-repl-worktree-start-tag-prefix ""))
    (should (null (claude-repl--start-tag-name "any")))))

;;;; ---- Tests: create-start-tag ----

(ert-deftest claude-repl-test-create-start-tag-creates-at-base-commit ()
  "create-start-tag invokes `git tag <prefix><branch> <base-commit>' in GIT-ROOT."
  (let ((sha "abc123def456abc123def456abc123def4567890")
        (claude-repl-worktree-start-tag-prefix "start/")
        (captured-call nil))
    (cl-letf (((symbol-function 'claude-repl--git-exit-code)
               (lambda (root &rest args)
                 (setq captured-call (cons root args))
                 0)))
      (claude-repl--create-start-tag "/tmp/repo" "feature" sha)
      (should (equal captured-call
                     (list "/tmp/repo" "tag" "start/feature" sha))))))

(ert-deftest claude-repl-test-create-start-tag-disabled-no-op ()
  "When prefix is nil, no tag is created (no git call)."
  (let ((claude-repl-worktree-start-tag-prefix nil)
        (git-called nil))
    (cl-letf (((symbol-function 'claude-repl--git-exit-code)
               (lambda (&rest _args) (setq git-called t) 0)))
      (claude-repl--create-start-tag "/tmp/repo" "feature" "HEAD")
      (should-not git-called))))

(ert-deftest claude-repl-test-create-start-tag-signals-on-failure ()
  "create-start-tag signals an error when git tag fails (non-zero exit)."
  (let ((claude-repl-worktree-start-tag-prefix "start/"))
    (cl-letf (((symbol-function 'claude-repl--git-exit-code)
               (lambda (&rest _args) 128)))
      (should-error (claude-repl--create-start-tag "/tmp/repo" "feature" "HEAD")))))

;;;; ---- Tests: async-worktree-add start-tag integration ----

(ert-deftest claude-repl-test-async-worktree-add-creates-start-tag-on-success ()
  "After successful worktree add, the start tag is created."
  (let ((captured-tag-args nil))
    (cl-letf (((symbol-function 'claude-repl--async-git)
               ;; Simulate immediate success: invoke callback with ok=t.
               (lambda (_label _root _args cb) (funcall cb t "ok")))
              ((symbol-function 'claude-repl--worktree-add-callback)
               (lambda (&rest _args) nil))
              ((symbol-function 'claude-repl--create-start-tag)
               (lambda (git-root branch-name base-commit)
                 (setq captured-tag-args (list git-root branch-name base-commit)))))
      (claude-repl--async-worktree-add
       "/git-root" "my-branch" "/path" "HEAD"
       nil "dirname" nil nil nil nil)
      (should (equal captured-tag-args '("/git-root" "my-branch" "HEAD"))))))

(ert-deftest claude-repl-test-async-worktree-add-skips-start-tag-on-failure ()
  "On worktree add failure, the start tag is NOT created."
  (let ((tag-called nil))
    (cl-letf (((symbol-function 'claude-repl--async-git)
               ;; Simulate failure: invoke callback with ok=nil.
               (lambda (_label _root _args cb) (funcall cb nil "git error")))
              ((symbol-function 'claude-repl--worktree-add-callback)
               (lambda (&rest _args) nil))
              ((symbol-function 'claude-repl--create-start-tag)
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
               ((symbol-function 'claude-repl--cherry-pick-commits) (lambda (_dir _ws _base _br &optional _auto _silent) nil))
               ((symbol-function 'claude-repl--nuke-one-workspace) (lambda (&rest _) nil))
               ((symbol-function 'load-file) (lambda (f) (setq loaded-file f))))
      (claude-repl--workspace-merge-do "other-ws")
      (should (equal loaded-file claude-repl--config-file)))))

(ert-deftest claude-repl-test-workspace-merge-do-never-pops-magit-status ()
  "`--workspace-merge-do' must NEVER open or refresh a magit-status
buffer — post-merge buffer presentation is purely the caller's
(`--workspace-merge-into-source') workspace-switch responsibility.  The
function exercises both the SILENT and non-SILENT call shapes to assert
this is unconditional, not gated on SILENT."
  (claude-repl-test--with-clean-state
    (puthash "other-ws" '() claude-repl--workspaces)
    (let ((magit-status-called nil)
          (magit-refresh-called nil))
      (cl-letf* (((symbol-function '+workspace-current-name) (lambda () "current"))
                 ((symbol-function 'claude-repl--workspace-branch) (lambda (_ws) "branch-x"))
                 ((symbol-function 'claude-repl--ws-dir) (lambda (_ws) "/tmp/fake"))
                 ((symbol-function 'claude-repl--git-branch-exists-p) (lambda (_dir _br) t))
                 ((symbol-function 'claude-repl--cherry-pick-base) (lambda (_dir _br) "abc123"))
                 ((symbol-function 'claude-repl--cherry-pick-commits) (lambda (_dir _ws _base _br &optional _auto _silent) nil))
                 ((symbol-function 'claude-repl--tag-merge-completion) #'ignore)
                 ((symbol-function 'claude-repl--close-workspace) #'ignore)
                 ((symbol-function 'claude-repl-drawer--refresh-detail-cache) #'ignore)
                 ((symbol-function 'load-file) #'ignore)
                 ((symbol-function 'magit-status) (lambda (&rest _) (setq magit-status-called t)))
                 ((symbol-function 'magit-refresh) (lambda (&rest _) (setq magit-refresh-called t))))
        ;; Non-silent path.
        (claude-repl--workspace-merge-do "other-ws")
        (should-not magit-status-called)
        (should-not magit-refresh-called)
        ;; Silent path.
        (claude-repl--workspace-merge-do "other-ws" "/tmp/fake" t)
        (should-not magit-status-called)
        (should-not magit-refresh-called)))))

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
               ((symbol-function 'claude-repl--cherry-pick-commits) (lambda (_dir _ws _base _br &optional _auto _silent) nil))
               ((symbol-function 'claude-repl--nuke-one-workspace) (lambda (&rest _) nil))
               ((symbol-function 'load-file) #'ignore)
               ((symbol-function 'claude-repl--tag-merge-completion)
                (lambda (root ws) (setq tagged (cons root ws)))))
      (claude-repl--workspace-merge-do "other-ws")
      (should (equal tagged (cons "/tmp/fake" "other-ws"))))))

(ert-deftest claude-repl-test-workspace-merge-do-skips-tag-on-cherry-pick-error ()
  "When cherry-pick-commits signals user-error (e.g., a conflict),
tag-merge-completion is NOT invoked and the error is re-signaled to
the caller.  The empty-range case no longer goes through this path —
it returns the `already-incorporated' sentinel and proceeds to the
tag + finish steps (see `…-already-incorporated-still-finishes')."
  (claude-repl-test--with-clean-state
    (puthash "other-ws" '() claude-repl--workspaces)
    (let ((tagged nil))
      (cl-letf* (((symbol-function '+workspace-current-name) (lambda () "current"))
                 ((symbol-function 'claude-repl--workspace-branch) (lambda (_ws) "branch-x"))
                 ((symbol-function 'claude-repl--ws-dir) (lambda (_ws) "/tmp/fake"))
                 ((symbol-function 'claude-repl--git-branch-exists-p) (lambda (_dir _br) t))
                 ((symbol-function 'claude-repl--cherry-pick-base) (lambda (_dir _br) "abc123"))
                 ((symbol-function 'claude-repl--cherry-pick-commits)
                  (lambda (_dir _ws _base _br &optional _auto _silent)
                    (user-error "Conflict cherry-picking — resolve in magit")))
                 ((symbol-function 'claude-repl--nuke-one-workspace) (lambda (&rest _) nil))
                 ((symbol-function 'load-file) #'ignore)
                 ((symbol-function 'claude-repl--tag-merge-completion)
                  (lambda (_root _ws) (setq tagged t))))
        (should-error (claude-repl--workspace-merge-do "other-ws") :type 'user-error)
        (should-not tagged)))))

;;;; ---- Tests: workspace-merge-do success/failure plist effects ----

(ert-deftest claude-repl-test-workspace-merge-do-sets-merge-completed-on-success ()
  "After a successful cherry-pick, `:merge-completed' t is recorded on
the target workspace before the auto-finish tear-down runs.  Stubs
`--finish-workspace' so the plist entry survives the assertion."
  (claude-repl-test--with-clean-state
    (puthash "other-ws" '() claude-repl--workspaces)
    (cl-letf* (((symbol-function '+workspace-current-name) (lambda () "current"))
               ((symbol-function 'claude-repl--workspace-branch) (lambda (_ws) "branch-x"))
               ((symbol-function 'claude-repl--ws-dir) (lambda (_ws) "/tmp/fake"))
               ((symbol-function 'claude-repl--git-branch-exists-p) (lambda (_dir _br) t))
               ((symbol-function 'claude-repl--cherry-pick-base) (lambda (_dir _br) "abc123"))
               ((symbol-function 'claude-repl--cherry-pick-commits) (lambda (_dir _ws _base _br &optional _auto _silent) nil))
               ((symbol-function 'claude-repl--tag-merge-completion) #'ignore)
               ((symbol-function 'claude-repl--nuke-one-workspace) #'ignore)
               ((symbol-function 'load-file) #'ignore))
      (claude-repl--workspace-merge-do "other-ws" "/tmp/fake" t)
      (should (eq (claude-repl--ws-get "other-ws" :merge-completed) t)))))

(ert-deftest claude-repl-test-workspace-merge-do-sets-merge-failed-on-silent-failure ()
  "When `--cherry-pick-commits' returns `failed' (silent failure: exit
non-zero, no CHERRY_PICK_HEAD), `--workspace-merge-do' flips
`:repl-state' to `:merge-failed' and records `:merge-failed t' so the
drawer surfaces the ❌ badge.  `:merge-completed' is NOT set —
commits did not land, so the workspace stays in its normal (alive)
bucket rather than routing into MERGED."
  (claude-repl-test--with-clean-state
    (puthash "other-ws" '() claude-repl--workspaces)
    (cl-letf* (((symbol-function '+workspace-current-name) (lambda () "current"))
               ((symbol-function 'claude-repl--workspace-branch) (lambda (_ws) "branch-x"))
               ((symbol-function 'claude-repl--ws-dir) (lambda (_ws) "/tmp/fake"))
               ((symbol-function 'claude-repl--git-branch-exists-p) (lambda (_dir _br) t))
               ((symbol-function 'claude-repl--cherry-pick-base) (lambda (_dir _br) "abc123"))
               ((symbol-function 'claude-repl--cherry-pick-commits)
                (lambda (_dir _ws _base _br &optional _auto _silent) 'failed))
               ((symbol-function 'claude-repl--tag-merge-completion) #'ignore)
               ((symbol-function 'claude-repl--nuke-one-workspace) #'ignore)
               ((symbol-function 'load-file) #'ignore))
      (claude-repl--workspace-merge-do "other-ws" "/tmp/fake" t)
      (should (eq (claude-repl--ws-get "other-ws" :repl-state) :merge-failed))
      (should-not (claude-repl--ws-get "other-ws" :merge-completed))
      (should (eq (claude-repl--ws-get "other-ws" :merge-failed) t)))))

(ert-deftest claude-repl-test-workspace-merge-do-does-not-close-on-silent-failure ()
  "On silent cherry-pick failure (`failed' sentinel), the workspace is
NOT torn down: `--close-workspace' (and its underlying
`--nuke-one-workspace') must not run.  The user keeps the live
session/perspective/buffers so they can investigate and retry — the
whole point of the SPC TAB M failure path is to preserve in-flight
work, not auto-finish a workspace whose commits never landed."
  (claude-repl-test--with-clean-state
    (puthash "other-ws" '() claude-repl--workspaces)
    (let ((close-called nil)
          (nuke-called nil))
      (cl-letf* (((symbol-function '+workspace-current-name) (lambda () "current"))
                 ((symbol-function 'claude-repl--workspace-branch) (lambda (_ws) "branch-x"))
                 ((symbol-function 'claude-repl--ws-dir) (lambda (_ws) "/tmp/fake"))
                 ((symbol-function 'claude-repl--git-branch-exists-p) (lambda (_dir _br) t))
                 ((symbol-function 'claude-repl--cherry-pick-base) (lambda (_dir _br) "abc123"))
                 ((symbol-function 'claude-repl--cherry-pick-commits)
                  (lambda (_dir _ws _base _br &optional _auto _silent) 'failed))
                 ((symbol-function 'claude-repl--tag-merge-completion) #'ignore)
                 ((symbol-function 'claude-repl--close-workspace)
                  (lambda (&rest _) (setq close-called t)))
                 ((symbol-function 'claude-repl--nuke-one-workspace)
                  (lambda (&rest _) (setq nuke-called t)))
                 ((symbol-function 'load-file) #'ignore))
        (claude-repl--workspace-merge-do "other-ws" "/tmp/fake" t)
        (should-not close-called)
        (should-not nuke-called)))))

(ert-deftest claude-repl-test-workspace-merge-do-skips-tag-on-silent-failure ()
  "When the cherry-pick silently failed, HEAD has not advanced to include
the target workspace's work — `--tag-merge-completion' MUST NOT run, or
the `merge/<ws>' tag would mislabel an unrelated commit."
  (claude-repl-test--with-clean-state
    (puthash "other-ws" '() claude-repl--workspaces)
    (let ((tagged nil))
      (cl-letf* (((symbol-function '+workspace-current-name) (lambda () "current"))
                 ((symbol-function 'claude-repl--workspace-branch) (lambda (_ws) "branch-x"))
                 ((symbol-function 'claude-repl--ws-dir) (lambda (_ws) "/tmp/fake"))
                 ((symbol-function 'claude-repl--git-branch-exists-p) (lambda (_dir _br) t))
                 ((symbol-function 'claude-repl--cherry-pick-base) (lambda (_dir _br) "abc123"))
                 ((symbol-function 'claude-repl--cherry-pick-commits)
                  (lambda (_dir _ws _base _br &optional _auto _silent) 'failed))
                 ((symbol-function 'claude-repl--tag-merge-completion)
                  (lambda (_root _ws) (setq tagged t)))
                 ((symbol-function 'claude-repl--nuke-one-workspace) #'ignore)
                 ((symbol-function 'load-file) #'ignore))
        (claude-repl--workspace-merge-do "other-ws" "/tmp/fake" t)
        (should-not tagged)))))

(ert-deftest claude-repl-test-workspace-merge-do-clears-merge-failed-on-success ()
  "A successful merge must explicitly clear `:merge-failed' (in case a
prior attempt set it).  Without this, a re-run from a silent-failure
state would leave `:merge-failed t' sticky and the drawer would keep
showing ❌ despite the latest run landing cleanly."
  (claude-repl-test--with-clean-state
    (puthash "other-ws" '(:merge-failed t) claude-repl--workspaces)
    (cl-letf* (((symbol-function '+workspace-current-name) (lambda () "current"))
               ((symbol-function 'claude-repl--workspace-branch) (lambda (_ws) "branch-x"))
               ((symbol-function 'claude-repl--ws-dir) (lambda (_ws) "/tmp/fake"))
               ((symbol-function 'claude-repl--git-branch-exists-p) (lambda (_dir _br) t))
               ((symbol-function 'claude-repl--cherry-pick-base) (lambda (_dir _br) "abc123"))
               ((symbol-function 'claude-repl--cherry-pick-commits) (lambda (_dir _ws _base _br &optional _auto _silent) nil))
               ((symbol-function 'claude-repl--tag-merge-completion) #'ignore)
               ((symbol-function 'claude-repl--nuke-one-workspace) #'ignore)
               ((symbol-function 'load-file) #'ignore))
      (claude-repl--workspace-merge-do "other-ws" "/tmp/fake" t)
      (should-not (claude-repl--ws-get "other-ws" :merge-failed))
      (should (eq (claude-repl--ws-get "other-ws" :repl-state) :merged)))))

(ert-deftest claude-repl-test-workspace-merge-do-sets-repl-state-merged-on-success ()
  "After a successful cherry-pick, `:repl-state' is set to `:merged'
so the 🔀 badge survives the post-nuke poll cycle that would
otherwise mark the (now-vterm-less) workspace `:dead'."
  (claude-repl-test--with-clean-state
    (puthash "other-ws" '() claude-repl--workspaces)
    (cl-letf* (((symbol-function '+workspace-current-name) (lambda () "current"))
               ((symbol-function 'claude-repl--workspace-branch) (lambda (_ws) "branch-x"))
               ((symbol-function 'claude-repl--ws-dir) (lambda (_ws) "/tmp/fake"))
               ((symbol-function 'claude-repl--git-branch-exists-p) (lambda (_dir _br) t))
               ((symbol-function 'claude-repl--cherry-pick-base) (lambda (_dir _br) "abc123"))
               ((symbol-function 'claude-repl--cherry-pick-commits) (lambda (_dir _ws _base _br &optional _auto _silent) nil))
               ((symbol-function 'claude-repl--tag-merge-completion) #'ignore)
               ((symbol-function 'claude-repl--nuke-one-workspace) #'ignore)
               ((symbol-function 'load-file) #'ignore))
      (claude-repl--workspace-merge-do "other-ws" "/tmp/fake" t)
      (should (eq (claude-repl--ws-get "other-ws" :repl-state) :merged)))))

(ert-deftest claude-repl-test-workspace-merge-do-already-incorporated-still-tears-down ()
  "When cherry-pick-commits returns `already-incorporated' (commits
already on the parent), workspace-merge-do still tags and tears down
via `--nuke-one-workspace' with `preserve-entry' so the drawer's
MERGED bucket picks it up.  `--finish-workspace' is intentionally
NOT called — that runs only when the user explicitly presses `x'."
  (claude-repl-test--with-clean-state
    (puthash "other-ws" '() claude-repl--workspaces)
    (let ((tagged nil)
          (nuked-ws nil)
          (nuked-preserve nil))
      (cl-letf* (((symbol-function '+workspace-current-name) (lambda () "current"))
                 ((symbol-function 'claude-repl--workspace-branch) (lambda (_ws) "branch-x"))
                 ((symbol-function 'claude-repl--ws-dir) (lambda (_ws) "/tmp/fake"))
                 ((symbol-function 'claude-repl--git-branch-exists-p) (lambda (_dir _br) t))
                 ((symbol-function 'claude-repl--cherry-pick-base) (lambda (_dir _br) "abc123"))
                 ((symbol-function 'claude-repl--cherry-pick-commits)
                  (lambda (_dir _ws _base _br &optional _auto _silent) 'already-incorporated))
                 ((symbol-function 'claude-repl--tag-merge-completion)
                  (lambda (_root _ws) (setq tagged t)))
                 ((symbol-function 'claude-repl--nuke-one-workspace)
                  (lambda (ws &optional preserve)
                    (setq nuked-ws ws)
                    (setq nuked-preserve preserve)))
                 ((symbol-function 'load-file) #'ignore))
        (claude-repl--workspace-merge-do "other-ws" "/tmp/fake" t)
        (should tagged)
        (should (equal nuked-ws "other-ws"))
        (should nuked-preserve)))))

(ert-deftest claude-repl-test-workspace-merge-do-routes-close-through-gns-gating ()
  "Successful merge must dispatch the editor-side close via
`--gns-sockets-close-then' so the in-workspace Claude is sent
`/gns-sockets close' before its vterm dies.  The teardown thunk
forwarded to the gate must call `--close-workspace' with
`preserve-entry'."
  (claude-repl-test--with-clean-state
    (puthash "other-ws" '() claude-repl--workspaces)
    (let ((gating-ws :unset)
          (gating-teardown nil)
          (closed-ws :unset)
          (closed-preserve :unset))
      (cl-letf* (((symbol-function '+workspace-current-name) (lambda () "current"))
                 ((symbol-function 'claude-repl--workspace-branch) (lambda (_ws) "branch-x"))
                 ((symbol-function 'claude-repl--ws-dir) (lambda (_ws) "/tmp/fake"))
                 ((symbol-function 'claude-repl--git-branch-exists-p) (lambda (_dir _br) t))
                 ((symbol-function 'claude-repl--cherry-pick-base) (lambda (_dir _br) "abc123"))
                 ((symbol-function 'claude-repl--cherry-pick-commits) (lambda (_dir _ws _base _br &optional _auto _silent) nil))
                 ((symbol-function 'claude-repl--tag-merge-completion) #'ignore)
                 ((symbol-function 'load-file) #'ignore)
                 ((symbol-function 'claude-repl--gns-sockets-close-then)
                  (lambda (ws fn)
                    (setq gating-ws ws
                          gating-teardown fn)))
                 ((symbol-function 'claude-repl--close-workspace)
                  (lambda (ws &optional preserve)
                    (setq closed-ws ws
                          closed-preserve preserve))))
        (claude-repl--workspace-merge-do "other-ws" "/tmp/fake" t)
        (should (equal gating-ws "other-ws"))
        (should (functionp gating-teardown))
        (funcall gating-teardown)
        (should (equal closed-ws "other-ws"))
        (should (eq closed-preserve 'preserve-entry))))))

(ert-deftest claude-repl-test-workspace-merge-do-tears-down-on-success ()
  "Successful merge nukes the target workspace's session/persp/buffers
with `preserve-entry' so the hash entry survives for the drawer's
MERGED bucket.  The git worktree on disk is left in place — only an
explicit drawer `x' (`--finish-workspace') removes it."
  (claude-repl-test--with-clean-state
    (puthash "other-ws" '() claude-repl--workspaces)
    (let ((nuked-ws nil)
          (nuked-preserve nil))
      (cl-letf* (((symbol-function '+workspace-current-name) (lambda () "current"))
                 ((symbol-function 'claude-repl--workspace-branch) (lambda (_ws) "branch-x"))
                 ((symbol-function 'claude-repl--ws-dir) (lambda (_ws) "/tmp/fake"))
                 ((symbol-function 'claude-repl--git-branch-exists-p) (lambda (_dir _br) t))
                 ((symbol-function 'claude-repl--cherry-pick-base) (lambda (_dir _br) "abc123"))
                 ((symbol-function 'claude-repl--cherry-pick-commits) (lambda (_dir _ws _base _br &optional _auto _silent) nil))
                 ((symbol-function 'claude-repl--tag-merge-completion) #'ignore)
                 ((symbol-function 'load-file) #'ignore)
                 ((symbol-function 'claude-repl--nuke-one-workspace)
                  (lambda (ws &optional preserve)
                    (setq nuked-ws ws)
                    (setq nuked-preserve preserve))))
        (claude-repl--workspace-merge-do "other-ws" "/tmp/fake" t)
        (should (equal nuked-ws "other-ws"))
        (should nuked-preserve)))))

(ert-deftest claude-repl-test-workspace-merge-do-defers-success-teardown ()
  "Success-path teardown (gns-sockets-close-then -> close-workspace) is
routed through `claude-repl--defer-to-main-thread' so the perspective
kill, vterm kill, and buffer cleanup all run on the main thread.  This
is what makes `--workspace-merge-do' safe to execute from the worker
thread spawned by `claude-repl--workspace-merge-async'.

Pinned with a stub that captures the defer call — the test fixture's
default override would otherwise invoke the thunk immediately, hiding
whether the defer call was ever made."
  (claude-repl-test--with-clean-state
    (puthash "other-ws" '() claude-repl--workspaces)
    (let ((defer-calls 0))
      (cl-letf* (((symbol-function '+workspace-current-name) (lambda () "current"))
                 ((symbol-function 'claude-repl--workspace-branch) (lambda (_ws) "branch-x"))
                 ((symbol-function 'claude-repl--ws-dir) (lambda (_ws) "/tmp/fake"))
                 ((symbol-function 'claude-repl--git-branch-exists-p) (lambda (_dir _br) t))
                 ((symbol-function 'claude-repl--cherry-pick-base) (lambda (_dir _br) "abc123"))
                 ((symbol-function 'claude-repl--cherry-pick-commits) (lambda (_dir _ws _base _br &optional _auto _silent) nil))
                 ((symbol-function 'claude-repl--tag-merge-completion) #'ignore)
                 ((symbol-function 'load-file) #'ignore)
                 ((symbol-function 'claude-repl--defer-to-main-thread)
                  (lambda (_thunk) (cl-incf defer-calls))))
        (claude-repl--workspace-merge-do "other-ws" "/tmp/fake" t)
        (should (= defer-calls 1))))))

(ert-deftest claude-repl-test-workspace-merge-do-does-not-call-finish-workspace ()
  "Successful merge must NOT call `--finish-workspace' — that's reserved
for the drawer `x' path and removes the git worktree, which is exactly
what we want to defer until the user explicitly chooses."
  (claude-repl-test--with-clean-state
    (puthash "other-ws" '() claude-repl--workspaces)
    (let ((finish-called nil))
      (cl-letf* (((symbol-function '+workspace-current-name) (lambda () "current"))
                 ((symbol-function 'claude-repl--workspace-branch) (lambda (_ws) "branch-x"))
                 ((symbol-function 'claude-repl--ws-dir) (lambda (_ws) "/tmp/fake"))
                 ((symbol-function 'claude-repl--git-branch-exists-p) (lambda (_dir _br) t))
                 ((symbol-function 'claude-repl--cherry-pick-base) (lambda (_dir _br) "abc123"))
                 ((symbol-function 'claude-repl--cherry-pick-commits) (lambda (_dir _ws _base _br &optional _auto _silent) nil))
                 ((symbol-function 'claude-repl--tag-merge-completion) #'ignore)
                 ((symbol-function 'claude-repl--nuke-one-workspace) #'ignore)
                 ((symbol-function 'load-file) #'ignore)
                 ((symbol-function 'claude-repl--finish-workspace)
                  (lambda (&rest _) (setq finish-called t))))
        (claude-repl--workspace-merge-do "other-ws" "/tmp/fake" t)
        (should-not finish-called)))))

(ert-deftest claude-repl-test-workspace-merge-do-records-merge-completed-at ()
  "Successful merge stamps `:merge-completed-at' on the target so the
drawer can render an age/timestamp once that surfaces in the UI."
  (claude-repl-test--with-clean-state
    (puthash "other-ws" '() claude-repl--workspaces)
    (cl-letf* (((symbol-function '+workspace-current-name) (lambda () "current"))
               ((symbol-function 'claude-repl--workspace-branch) (lambda (_ws) "branch-x"))
               ((symbol-function 'claude-repl--ws-dir) (lambda (_ws) "/tmp/fake"))
               ((symbol-function 'claude-repl--git-branch-exists-p) (lambda (_dir _br) t))
               ((symbol-function 'claude-repl--cherry-pick-base) (lambda (_dir _br) "abc123"))
               ((symbol-function 'claude-repl--cherry-pick-commits) (lambda (_dir _ws _base _br &optional _auto _silent) nil))
               ((symbol-function 'claude-repl--tag-merge-completion) #'ignore)
               ((symbol-function 'claude-repl--nuke-one-workspace) #'ignore)
               ((symbol-function 'load-file) #'ignore))
      (claude-repl--workspace-merge-do "other-ws" "/tmp/fake" t)
      (should (numberp (claude-repl--ws-get "other-ws" :merge-completed-at))))))

(ert-deftest claude-repl-test-workspace-merge-do-marks-dead-on-cherry-pick-error ()
  "GENERIC cherry-pick failure (non-conflict `user-error') flips the
target workspace to `:repl-state :dead' (and clears `:claude-state')
so the drawer shows the ❌ badge.  The error is still re-signaled.
Conflict-specific errors go through a different path — see
`claude-repl-test-workspace-merge-do-marks-merge-conflict-on-conflict-error'."
  (claude-repl-test--with-clean-state
    (puthash "other-ws" '(:claude-state :thinking) claude-repl--workspaces)
    (cl-letf* (((symbol-function '+workspace-current-name) (lambda () "current"))
               ((symbol-function 'claude-repl--workspace-branch) (lambda (_ws) "branch-x"))
               ((symbol-function 'claude-repl--ws-dir) (lambda (_ws) "/tmp/fake"))
               ((symbol-function 'claude-repl--git-branch-exists-p) (lambda (_dir _br) t))
               ((symbol-function 'claude-repl--cherry-pick-base) (lambda (_dir _br) "abc123"))
               ((symbol-function 'claude-repl--cherry-pick-commits)
                (lambda (_dir _ws _base _br &optional _auto _silent) (user-error "Generic failure")))
               ((symbol-function 'claude-repl--nuke-one-workspace) #'ignore)
               ((symbol-function 'load-file) #'ignore))
      (should-error (claude-repl--workspace-merge-do "other-ws" "/tmp/fake" t)
                    :type 'user-error)
      (should (eq (claude-repl--ws-get "other-ws" :repl-state) :dead))
      (should (null (claude-repl--ws-get "other-ws" :claude-state))))))

(ert-deftest claude-repl-test-workspace-merge-do-marks-merge-conflict-on-conflict-error ()
  "When the cherry-pick raises `claude-repl-merge-conflict-error', the
target workspace flips to `:repl-state :merge-conflict' (not `:dead')
so the drawer renders the 💥 badge.  `:claude-state' is preserved
because the vterm is still alive — the user can keep typing after
resolving the conflict externally."
  (claude-repl-test--with-clean-state
    (puthash "other-ws" '(:claude-state :thinking) claude-repl--workspaces)
    (cl-letf* (((symbol-function '+workspace-current-name) (lambda () "current"))
               ((symbol-function 'claude-repl--workspace-branch) (lambda (_ws) "branch-x"))
               ((symbol-function 'claude-repl--ws-dir) (lambda (_ws) "/tmp/fake"))
               ((symbol-function 'claude-repl--git-branch-exists-p) (lambda (_dir _br) t))
               ((symbol-function 'claude-repl--cherry-pick-base) (lambda (_dir _br) "abc123"))
               ((symbol-function 'claude-repl--cherry-pick-commits)
                (lambda (_dir _ws _base _br &optional _auto _silent)
                  (signal 'claude-repl-merge-conflict-error '("Conflict"))))
               ((symbol-function 'claude-repl--nuke-one-workspace) #'ignore)
               ((symbol-function 'load-file) #'ignore))
      (should-error (claude-repl--workspace-merge-do "other-ws" "/tmp/fake" t)
                    :type 'claude-repl-merge-conflict-error)
      (should (eq (claude-repl--ws-get "other-ws" :repl-state) :merge-conflict))
      ;; vterm-alive workspace should keep its claude-state through a conflict
      (should (eq (claude-repl--ws-get "other-ws" :claude-state) :thinking)))))

(ert-deftest claude-repl-test-workspace-merge-do-clears-prior-merge-conflict-on-retry ()
  "A retry of a previously-conflicted merge clears the stale 💥 badge
before re-entering the cherry-pick so the drawer reflects in-flight
state, not stale failure state.  Only `:merge-conflict' is cleared —
other repl-states are preserved."
  (claude-repl-test--with-clean-state
    (puthash "other-ws" '(:repl-state :merge-conflict) claude-repl--workspaces)
    (cl-letf* (((symbol-function '+workspace-current-name) (lambda () "current"))
               ((symbol-function 'claude-repl--workspace-branch) (lambda (_ws) "branch-x"))
               ((symbol-function 'claude-repl--ws-dir) (lambda (_ws) "/tmp/fake"))
               ((symbol-function 'claude-repl--git-branch-exists-p) (lambda (_dir _br) t))
               ((symbol-function 'claude-repl--cherry-pick-base) (lambda (_dir _br) "abc123"))
               ((symbol-function 'claude-repl--cherry-pick-commits)
                (lambda (_dir _ws _base _br &optional _auto _silent) nil))
               ((symbol-function 'claude-repl--nuke-one-workspace) #'ignore)
               ((symbol-function 'claude-repl--tag-merge-completion) #'ignore)
               ((symbol-function 'claude-repl--gns-sockets-close-then)
                (lambda (_ws thunk) (funcall thunk)))
               ((symbol-function 'claude-repl--close-workspace) #'ignore)
               ((symbol-function 'load-file) #'ignore))
      (claude-repl--workspace-merge-do "other-ws" "/tmp/fake")
      ;; After a successful retry, the workspace ends up :merged — but
      ;; the assertion we care about is that the initial :merge-conflict
      ;; was cleared (it would NOT be `:merge-conflict' here regardless).
      (should-not (eq (claude-repl--ws-get "other-ws" :repl-state)
                      :merge-conflict)))))

(ert-deftest claude-repl-test-mark-merge-conflict-sets-state ()
  "Direct invariants of `claude-repl--mark-merge-conflict':
`:repl-state' → `:merge-conflict', `:merging' cleared,
`:merge-completed' cleared, `:claude-state' NOT touched."
  (claude-repl-test--with-clean-state
    (puthash "ws" '(:claude-state :thinking
                    :merging t
                    :merge-completed t)
             claude-repl--workspaces)
    (claude-repl--mark-merge-conflict "ws" '(error "test"))
    (should (eq (claude-repl--ws-get "ws" :repl-state) :merge-conflict))
    (should (null (claude-repl--ws-get "ws" :merging)))
    (should (null (claude-repl--ws-get "ws" :merge-completed)))
    ;; :claude-state must remain — vterm is still alive on a conflict
    (should (eq (claude-repl--ws-get "ws" :claude-state) :thinking))))

(ert-deftest claude-repl-test-workspace-merge-do-does-not-set-merge-completed-on-error ()
  "A failed cherry-pick must leave `:merge-completed' nil so the
workspace cannot accidentally surface in MERGED on the strength of an
earlier partial success."
  (claude-repl-test--with-clean-state
    (puthash "other-ws" '(:merge-completed nil) claude-repl--workspaces)
    (cl-letf* (((symbol-function '+workspace-current-name) (lambda () "current"))
               ((symbol-function 'claude-repl--workspace-branch) (lambda (_ws) "branch-x"))
               ((symbol-function 'claude-repl--ws-dir) (lambda (_ws) "/tmp/fake"))
               ((symbol-function 'claude-repl--git-branch-exists-p) (lambda (_dir _br) t))
               ((symbol-function 'claude-repl--cherry-pick-base) (lambda (_dir _br) "abc123"))
               ((symbol-function 'claude-repl--cherry-pick-commits)
                (lambda (_dir _ws _base _br &optional _auto _silent) (user-error "Conflict")))
               ((symbol-function 'claude-repl--nuke-one-workspace) #'ignore)
               ((symbol-function 'load-file) #'ignore))
      (ignore-errors
        (claude-repl--workspace-merge-do "other-ws" "/tmp/fake" t))
      (should-not (claude-repl--ws-get "other-ws" :merge-completed)))))

(ert-deftest claude-repl-test-workspace-merge-do-refreshes-detail-cache-on-success ()
  "After a successful merge, the drawer's `:detail-*' cache for the
target workspace is refreshed.  The cache populated pre-merge (e.g.,
`:detail-master-ahead' showing the soon-to-be-merged commit count)
would otherwise linger in the MERGED bucket's expanded view — the
post-merge refresh ensures the rendered values reflect current git
state, not stale pre-merge snapshots."
  (claude-repl-test--with-clean-state
    (puthash "other-ws" '(:detail-master-ahead 99) claude-repl--workspaces)
    (let ((refreshed-ws nil))
      (cl-letf* (((symbol-function '+workspace-current-name) (lambda () "current"))
                 ((symbol-function 'claude-repl--workspace-branch) (lambda (_ws) "branch-x"))
                 ((symbol-function 'claude-repl--ws-dir) (lambda (_ws) "/tmp/fake"))
                 ((symbol-function 'claude-repl--git-branch-exists-p) (lambda (_dir _br) t))
                 ((symbol-function 'claude-repl--cherry-pick-base) (lambda (_dir _br) "abc123"))
                 ((symbol-function 'claude-repl--cherry-pick-commits) (lambda (_dir _ws _base _br &optional _auto _silent) nil))
                 ((symbol-function 'claude-repl--tag-merge-completion) #'ignore)
                 ((symbol-function 'claude-repl--nuke-one-workspace) #'ignore)
                 ((symbol-function 'load-file) #'ignore)
                 ((symbol-function 'claude-repl-drawer--refresh-detail-cache)
                  (lambda (ws) (setq refreshed-ws ws))))
        (claude-repl--workspace-merge-do "other-ws" "/tmp/fake" t)
        (should (equal refreshed-ws "other-ws"))))))

(ert-deftest claude-repl-test-workspace-merge-do-refreshes-detail-cache-after-nuke ()
  "The post-merge `:detail-*' refresh runs after `--nuke-one-workspace'
so the cache reflects the fully settled MERGED-bucket state — the nuke
preserves the hash entry and the worktree on disk, so the refresh's
synchronous git calls still resolve."
  (claude-repl-test--with-clean-state
    (puthash "other-ws" '() claude-repl--workspaces)
    (let ((call-order nil))
      (cl-letf* (((symbol-function '+workspace-current-name) (lambda () "current"))
                 ((symbol-function 'claude-repl--workspace-branch) (lambda (_ws) "branch-x"))
                 ((symbol-function 'claude-repl--ws-dir) (lambda (_ws) "/tmp/fake"))
                 ((symbol-function 'claude-repl--git-branch-exists-p) (lambda (_dir _br) t))
                 ((symbol-function 'claude-repl--cherry-pick-base) (lambda (_dir _br) "abc123"))
                 ((symbol-function 'claude-repl--cherry-pick-commits) (lambda (_dir _ws _base _br &optional _auto _silent) nil))
                 ((symbol-function 'claude-repl--tag-merge-completion) #'ignore)
                 ((symbol-function 'load-file) #'ignore)
                 ((symbol-function 'claude-repl--nuke-one-workspace)
                  (lambda (&rest _) (push 'nuke call-order)))
                 ((symbol-function 'claude-repl-drawer--refresh-detail-cache)
                  (lambda (_ws) (push 'refresh call-order))))
        (claude-repl--workspace-merge-do "other-ws" "/tmp/fake" t)
        (should (equal (nreverse call-order) '(nuke refresh)))))))

(ert-deftest claude-repl-test-workspace-merge-do-skips-detail-refresh-when-unbound ()
  "The post-merge cache refresh is guarded by `fboundp' so a load-order
oddity (drawer not yet loaded) cannot break the merge.  Verifies the
merge completes normally when `--refresh-detail-cache' is unbound."
  (claude-repl-test--with-clean-state
    (puthash "other-ws" '() claude-repl--workspaces)
    (cl-letf* (((symbol-function '+workspace-current-name) (lambda () "current"))
               ((symbol-function 'claude-repl--workspace-branch) (lambda (_ws) "branch-x"))
               ((symbol-function 'claude-repl--ws-dir) (lambda (_ws) "/tmp/fake"))
               ((symbol-function 'claude-repl--git-branch-exists-p) (lambda (_dir _br) t))
               ((symbol-function 'claude-repl--cherry-pick-base) (lambda (_dir _br) "abc123"))
               ((symbol-function 'claude-repl--cherry-pick-commits) (lambda (_dir _ws _base _br &optional _auto _silent) nil))
               ((symbol-function 'claude-repl--tag-merge-completion) #'ignore)
               ((symbol-function 'claude-repl--nuke-one-workspace) #'ignore)
               ((symbol-function 'load-file) #'ignore)
               ((symbol-function 'fboundp)
                (lambda (sym) (not (eq sym 'claude-repl-drawer--refresh-detail-cache)))))
      ;; Should complete without error and still record :merge-completed.
      (claude-repl--workspace-merge-do "other-ws" "/tmp/fake" t)
      (should (eq (claude-repl--ws-get "other-ws" :merge-completed) t)))))

(ert-deftest claude-repl-test-workspace-merge-do-skips-detail-refresh-on-failure ()
  "A failed cherry-pick must NOT refresh the detail cache — the merge
didn't complete, so there's no fresh post-merge state to capture, and
running the refresh on a workspace headed for `:dead' just wastes git
calls."
  (claude-repl-test--with-clean-state
    (puthash "other-ws" '() claude-repl--workspaces)
    (let ((refresh-called nil))
      (cl-letf* (((symbol-function '+workspace-current-name) (lambda () "current"))
                 ((symbol-function 'claude-repl--workspace-branch) (lambda (_ws) "branch-x"))
                 ((symbol-function 'claude-repl--ws-dir) (lambda (_ws) "/tmp/fake"))
                 ((symbol-function 'claude-repl--git-branch-exists-p) (lambda (_dir _br) t))
                 ((symbol-function 'claude-repl--cherry-pick-base) (lambda (_dir _br) "abc123"))
                 ((symbol-function 'claude-repl--cherry-pick-commits)
                  (lambda (_dir _ws _base _br &optional _auto _silent) (user-error "Conflict")))
                 ((symbol-function 'claude-repl--nuke-one-workspace) #'ignore)
                 ((symbol-function 'load-file) #'ignore)
                 ((symbol-function 'claude-repl-drawer--refresh-detail-cache)
                  (lambda (_ws) (setq refresh-called t))))
        (ignore-errors
          (claude-repl--workspace-merge-do "other-ws" "/tmp/fake" t))
        (should-not refresh-called)))))

(ert-deftest claude-repl-test-workspace-merge-do-clears-merging-on-success ()
  "After a successful cherry-pick, `:merging' is cleared on the target
workspace.  Asserts the in-flight workflow flag does not linger past
the success transition — the workspace must leave the MERGING bucket
and enter MERGED in the same operation."
  (claude-repl-test--with-clean-state
    (puthash "other-ws" '() claude-repl--workspaces)
    (cl-letf* (((symbol-function '+workspace-current-name) (lambda () "current"))
               ((symbol-function 'claude-repl--workspace-branch) (lambda (_ws) "branch-x"))
               ((symbol-function 'claude-repl--ws-dir) (lambda (_ws) "/tmp/fake"))
               ((symbol-function 'claude-repl--git-branch-exists-p) (lambda (_dir _br) t))
               ((symbol-function 'claude-repl--cherry-pick-base) (lambda (_dir _br) "abc123"))
               ((symbol-function 'claude-repl--cherry-pick-commits) (lambda (_dir _ws _base _br &optional _auto _silent) nil))
               ((symbol-function 'claude-repl--tag-merge-completion) #'ignore)
               ((symbol-function 'claude-repl--nuke-one-workspace) #'ignore)
               ((symbol-function 'load-file) #'ignore))
      (claude-repl--workspace-merge-do "other-ws" "/tmp/fake" t)
      (should-not (claude-repl--ws-get "other-ws" :merging)))))

(ert-deftest claude-repl-test-workspace-merge-do-clears-merging-on-failure ()
  "A failed cherry-pick must leave `:merging' nil so the workspace
exits the MERGING bucket — the dead/❌ badge from
`--mark-merge-failed' takes over, and the in-flight flag must not
linger and falsely suggest the merge is still running."
  (claude-repl-test--with-clean-state
    (puthash "other-ws" '() claude-repl--workspaces)
    (cl-letf* (((symbol-function '+workspace-current-name) (lambda () "current"))
               ((symbol-function 'claude-repl--workspace-branch) (lambda (_ws) "branch-x"))
               ((symbol-function 'claude-repl--ws-dir) (lambda (_ws) "/tmp/fake"))
               ((symbol-function 'claude-repl--git-branch-exists-p) (lambda (_dir _br) t))
               ((symbol-function 'claude-repl--cherry-pick-base) (lambda (_dir _br) "abc123"))
               ((symbol-function 'claude-repl--cherry-pick-commits)
                (lambda (_dir _ws _base _br &optional _auto _silent) (user-error "Conflict")))
               ((symbol-function 'claude-repl--nuke-one-workspace) #'ignore)
               ((symbol-function 'load-file) #'ignore))
      (ignore-errors
        (claude-repl--workspace-merge-do "other-ws" "/tmp/fake" t))
      (should-not (claude-repl--ws-get "other-ws" :merging)))))

(ert-deftest claude-repl-test-workspace-merge-do-sets-merging-during-cherry-pick ()
  "`:merging' t is observable on the target workspace while the
cherry-pick is running.  Probed via a stubbed cherry-pick that
captures the plist mid-flight — asserts the flag is set before the
cherry-pick begins, not after."
  (claude-repl-test--with-clean-state
    (puthash "other-ws" '() claude-repl--workspaces)
    (let ((merging-mid-flight nil))
      (cl-letf* (((symbol-function '+workspace-current-name) (lambda () "current"))
                 ((symbol-function 'claude-repl--workspace-branch) (lambda (_ws) "branch-x"))
                 ((symbol-function 'claude-repl--ws-dir) (lambda (_ws) "/tmp/fake"))
                 ((symbol-function 'claude-repl--git-branch-exists-p) (lambda (_dir _br) t))
                 ((symbol-function 'claude-repl--cherry-pick-base) (lambda (_dir _br) "abc123"))
                 ((symbol-function 'claude-repl--cherry-pick-commits)
                  (lambda (_dir _ws _base _br &optional _auto _silent)
                    (setq merging-mid-flight
                          (claude-repl--ws-get "other-ws" :merging))
                    nil))
                 ((symbol-function 'claude-repl--tag-merge-completion) #'ignore)
                 ((symbol-function 'claude-repl--nuke-one-workspace) #'ignore)
                 ((symbol-function 'load-file) #'ignore))
        (claude-repl--workspace-merge-do "other-ws" "/tmp/fake" t)
        (should (eq merging-mid-flight t))))))

;;;; ---- Tests: ws-merge-in-progress-p ----

(ert-deftest claude-repl-test-ws-merge-in-progress-p-true-when-set ()
  "Returns t when `:merging' is explicitly t."
  (claude-repl-test--with-clean-state
    (puthash "ws" '(:merging t) claude-repl--workspaces)
    (should (claude-repl--ws-merge-in-progress-p "ws"))))

(ert-deftest claude-repl-test-ws-merge-in-progress-p-nil-when-absent ()
  "Returns nil when `:merging' is not set — workspace must default
away from MERGING."
  (claude-repl-test--with-clean-state
    (puthash "ws" '() claude-repl--workspaces)
    (should-not (claude-repl--ws-merge-in-progress-p "ws"))))

(ert-deftest claude-repl-test-ws-merge-in-progress-p-nil-when-other-truthy ()
  "Only the symbol t qualifies as in-flight.  Guards against a future
caller storing a truthy-but-non-t value (e.g. a start timestamp) and
unintentionally placing the workspace into MERGING."
  (claude-repl-test--with-clean-state
    (puthash "ws" '(:merging "1970") claude-repl--workspaces)
    (should-not (claude-repl--ws-merge-in-progress-p "ws"))))

;;;; ---- Tests: ws-merge-completed-p ----

(ert-deftest claude-repl-test-ws-merge-completed-p-true-when-set ()
  "Returns t when `:merge-completed' is explicitly t."
  (claude-repl-test--with-clean-state
    (puthash "ws" '(:merge-completed t) claude-repl--workspaces)
    (should (claude-repl--ws-merge-completed-p "ws"))))

(ert-deftest claude-repl-test-ws-merge-completed-p-nil-when-absent ()
  "Returns nil on cache miss — drawer must default such workspaces away
from MERGED."
  (claude-repl-test--with-clean-state
    (puthash "ws" '() claude-repl--workspaces)
    (should-not (claude-repl--ws-merge-completed-p "ws"))))

(ert-deftest claude-repl-test-ws-merge-completed-p-nil-when-other-truthy ()
  "Only the symbol t qualifies as completed.  This blocks a future
caller from mistakenly storing a truthy-but-non-t marker (e.g. a
timestamp) and getting an accidental MERGED bucket placement."
  (claude-repl-test--with-clean-state
    (puthash "ws" '(:merge-completed "1970") claude-repl--workspaces)
    (should-not (claude-repl--ws-merge-completed-p "ws"))))

;;;; ---- Tests: workspace-merge-current-into-source ----

(ert-deftest claude-repl-test-merge-current-into-source-routes-through-async-wrapper ()
  "Interactive `SPC TAB M' routes through `claude-repl--workspace-merge-async'
\(same path `/workspace-merge' skill takes — there is no behavioral diff
between the two callers), passing the current workspace name and its
resolved merge-routing-root.  The async wrapper handles close-then-spawn-
then-reopen-on-failure; tests of that lifecycle live near the helper."
  (claude-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-merge-async-" t))
          (async-args :unset))
      (unwind-protect
          (progn
            (claude-repl--ws-put "wt-ws" :project-dir "/tmp/wt-dir/")
            (claude-repl--ws-put "wt-ws" :source-ws-dir tmpdir)
            (cl-letf (((symbol-function '+workspace-current-name)
                       (lambda () "wt-ws"))
                      ((symbol-function 'claude-repl--workspace-merge-async)
                       (lambda (ws repo-root)
                         (setq async-args (list ws repo-root)))))
              (claude-repl-workspace-merge-current-into-source)
              (should (equal (car async-args) "wt-ws"))
              (should (equal (cadr async-args) tmpdir))))
        (delete-directory tmpdir t)))))

(ert-deftest claude-repl-test-merge-into-source-routes-to-recorded-source-dir ()
  "When :source-ws-dir points at an existing dir, --workspace-merge-do receives it
as the resolved target.  The interactive entry point now routes through the
cherry-pick handler (silent=t auto-resolve=t), so `switch-to-project' is NOT
called on the happy path — the assertion is on merge-do's TARGET-DIR arg."
  (claude-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-merge-src-" t))
          (switch-called nil)
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
                       (lambda (&rest _) (setq switch-called t)))
                      ((symbol-function 'claude-repl--workspace-merge-do)
                       (lambda (&rest args) (setq merge-do-args args))))
              (claude-repl-workspace-merge-current-into-source)
              (should-not switch-called)
              (should (equal merge-do-args (list "wt-ws" tmpdir t t)))))
        (delete-directory tmpdir t)))))

(ert-deftest claude-repl-test-merge-into-source-falls-back-to-master-when-recorded-dir-gone ()
  "If :source-ws-dir refers to a missing directory, fall back to master worktree path.
Assertion is on merge-do's TARGET-DIR arg (post-handler-routing, silent=t skips
the `switch-to-project' call)."
  (claude-repl-test--with-clean-state
    (let ((merge-do-args :unset))
      (claude-repl--ws-put "wt-ws" :project-dir "/tmp/wt-dir/")
      (claude-repl--ws-put "wt-ws" :source-ws-dir "/no/such/dir/")
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "wt-ws"))
                ((symbol-function 'claude-repl--master-worktree-path)
                 (lambda (_root) "/tmp/master-fallback/"))
                ((symbol-function 'claude-repl--assert-clean-worktree)
                 (lambda (&rest _) nil))
                ((symbol-function 'claude-repl-switch-to-project) #'ignore)
                ((symbol-function 'claude-repl--workspace-merge-do)
                 (lambda (&rest args) (setq merge-do-args args))))
        (claude-repl-workspace-merge-current-into-source)
        (should (equal merge-do-args (list "wt-ws" "/tmp/master-fallback/" t t)))))))

(ert-deftest claude-repl-test-merge-into-source-falls-back-to-master-when-no-recorded-source ()
  "Legacy workspace with no :source-ws-dir falls back to master worktree path.
Assertion is on merge-do's TARGET-DIR arg (post-handler-routing, silent=t skips
the `switch-to-project' call)."
  (claude-repl-test--with-clean-state
    (let ((merge-do-args :unset))
      (claude-repl--ws-put "wt-ws" :project-dir "/tmp/wt-dir/")
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "wt-ws"))
                ((symbol-function 'claude-repl--master-worktree-path)
                 (lambda (_root) "/tmp/master-fallback/"))
                ((symbol-function 'claude-repl--assert-clean-worktree)
                 (lambda (&rest _) nil))
                ((symbol-function 'claude-repl-switch-to-project) #'ignore)
                ((symbol-function 'claude-repl--workspace-merge-do)
                 (lambda (&rest args) (setq merge-do-args args))))
        (claude-repl-workspace-merge-current-into-source)
        (should (equal merge-do-args (list "wt-ws" "/tmp/master-fallback/" t t)))))))

(ert-deftest claude-repl-test-merge-into-source-silent-skips-switch-to-project ()
  "When SILENT is non-nil, --workspace-merge-into-source must NOT call
`claude-repl-switch-to-project'.  This is the path used by
`claude-repl--handle-merge-command' for skill-invoked merges so that
background-triggered /workspace-merge does not yank the user's focus."
  (claude-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-merge-silent-no-switch-" t))
          (switch-called nil)
          (merge-do-args :unset))
      (unwind-protect
          (progn
            (claude-repl--ws-put "wt-ws" :project-dir "/tmp/wt-dir/")
            (claude-repl--ws-put "wt-ws" :source-ws-dir tmpdir)
            (cl-letf (((symbol-function '+workspace-current-name) (lambda () "other-ws"))
                      ((symbol-function 'claude-repl--master-worktree-path)
                       (lambda (_root) nil))
                      ((symbol-function 'claude-repl--assert-clean-worktree)
                       (lambda (&rest _) nil))
                      ((symbol-function 'claude-repl-switch-to-project)
                       (lambda (&rest _) (setq switch-called t)))
                      ((symbol-function 'claude-repl--workspace-merge-do)
                       (lambda (&rest args) (setq merge-do-args args))))
              (claude-repl--workspace-merge-into-source "wt-ws" t)
              (should-not switch-called)
              (should (equal merge-do-args (list "wt-ws" tmpdir t nil)))))
        (delete-directory tmpdir t)))))

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
              (should (equal merge-do-args (list "named-ws" tmpdir nil nil)))))
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

(ert-deftest claude-repl-test-merge-base-ancestor-args-bails-on-same-sha ()
  "Returns nil when both branches resolve to the same tip SHA.
A freshly created child worktree starts at its parent's HEAD commit, so
the two branches are commit-identical even though their names differ —
the ancestry check would trivially succeed and mis-bucket the empty
child as merged.  The helper must bail before that point."
  (let ((same-sha "abc123def456abc123def456abc123def4567890"))
    (cl-letf (((symbol-function 'claude-repl--git-string-quiet)
               (lambda (&rest args)
                 (pcase args
                   ;; Distinct branch names but identical tip SHAs.
                   (`("-C" "/tmp/child-wt" "rev-parse" "--abbrev-ref" "HEAD") "child")
                   (`("-C" "/tmp/repo" "rev-parse" "--abbrev-ref" "HEAD") "master")
                   (`("-C" "/tmp/child-wt" "rev-parse" "HEAD") same-sha)
                   (`("-C" "/tmp/repo" "rev-parse" "HEAD") same-sha)
                   (_ (error "unmocked git-string-quiet args: %S" args))))))
      (should (null (claude-repl--merge-base-ancestor-args
                     "/tmp/child-wt" "/tmp/repo"))))))

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

(ert-deftest claude-repl-test-ws-merge-parent-dir-caches-positive-result ()
  "Second call returns the cached path without re-invoking master-worktree-path."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws" :project-dir "/some/repo/")
    (let ((call-count 0))
      (cl-letf (((symbol-function 'claude-repl--ws-dir)
                 (lambda (_) "/some/repo/"))
                ((symbol-function 'claude-repl--master-worktree-path)
                 (lambda (_) (cl-incf call-count) "/master/dir/")))
        (claude-repl--ws-merge-parent-dir "ws")
        (claude-repl--ws-merge-parent-dir "ws")
        (should (= call-count 1))))))

(ert-deftest claude-repl-test-ws-merge-parent-dir-caches-negative-result ()
  "Nil resolution is cached as `unresolved' so master-worktree-path is not re-shelled.
Regression: with no `:source-ws-dir' and a nil-returning master fallback,
the prior implementation skipped the cache write and re-shelled
`git worktree list --porcelain' on every poll tick — the dominant cost
on workspace switch in repos with many worktrees."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws" :project-dir "/some/repo/")
    (let ((call-count 0))
      (cl-letf (((symbol-function 'claude-repl--ws-dir)
                 (lambda (_) "/some/repo/"))
                ((symbol-function 'claude-repl--master-worktree-path)
                 (lambda (_) (cl-incf call-count) nil)))
        (should (null (claude-repl--ws-merge-parent-dir "ws")))
        (should (null (claude-repl--ws-merge-parent-dir "ws")))
        (should (= call-count 1))
        (should (eq (claude-repl--ws-get "ws" :merge-parent-dir) 'unresolved))))))

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
  "When parent worktree's branch is already in master, merge-do receives master-dir
as the resolved target.  The interactive entry point routes through the
cherry-pick handler (silent=t auto-resolve=t), so the rerouting decision is
visible in merge-do's TARGET-DIR arg rather than in `switch-to-project'."
  (claude-repl-test--with-clean-state
    (let ((parent-dir (make-temp-file "test-reroute-parent-" t))
          (master-dir (make-temp-file "test-reroute-master-" t))
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
                      ((symbol-function 'claude-repl-switch-to-project) #'ignore)
                      ((symbol-function 'claude-repl--workspace-merge-do)
                       (lambda (&rest args) (setq merge-do-args args))))
              (claude-repl-workspace-merge-current-into-source)
              (should (equal merge-do-args (list "wt-ws" master-dir t t)))))
        (delete-directory parent-dir t)
        (delete-directory master-dir t)))))

(ert-deftest claude-repl-test-merge-into-source-stays-on-parent-when-not-yet-merged ()
  "When parent worktree's branch has unmerged commits, keep parent as the target.
Routes through the cherry-pick handler (silent=t auto-resolve=t); the
target-dir decision shows up in merge-do's args."
  (claude-repl-test--with-clean-state
    (let ((parent-dir (make-temp-file "test-stay-parent-" t))
          (master-dir (make-temp-file "test-stay-master-" t))
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
                      ((symbol-function 'claude-repl-switch-to-project) #'ignore)
                      ((symbol-function 'claude-repl--workspace-merge-do)
                       (lambda (&rest args) (setq merge-do-args args))))
              (claude-repl-workspace-merge-current-into-source)
              (should (equal merge-do-args (list "wt-ws" parent-dir t t)))))
        (delete-directory parent-dir t)
        (delete-directory master-dir t)))))

;;;; ---- Tests: workspace-merge-do project-root override ----

(ert-deftest claude-repl-test-workspace-merge-do-uses-project-root-override ()
  "When PROJECT-ROOT-OVERRIDE is non-nil, cherry-pick lands there (not at current ws's dir)."
  (let ((cherry-pick-dir nil))
    (cl-letf* (((symbol-function '+workspace-current-name) (lambda () "current"))
               ((symbol-function 'claude-repl--workspace-branch) (lambda (_ws) "branch-x"))
               ((symbol-function 'claude-repl--ws-dir) (lambda (_ws) "/should/not/be/used/"))
               ((symbol-function 'claude-repl--git-branch-exists-p) (lambda (_dir _br) t))
               ((symbol-function 'claude-repl--cherry-pick-base) (lambda (_dir _br) "abc123"))
               ((symbol-function 'claude-repl--cherry-pick-commits)
                (lambda (dir _ws _base _br &optional _auto _silent) (setq cherry-pick-dir dir)))
               ((symbol-function 'claude-repl--nuke-one-workspace) (lambda (&rest _) nil))
               ((symbol-function 'load-file) #'ignore))
      (claude-repl--workspace-merge-do "other-ws" "/explicit/target/")
      (should (equal cherry-pick-dir "/explicit/target/")))))

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

;;;; ---- Tests: claude-repl-create-doom-oneshot-workspace-from-current-branch ----

(ert-deftest claude-repl-test-create-doom-oneshot-from-current-branch-uses-head-base ()
  "doom-oneshot-from-current-branch branches off HEAD (current branch of
the doom-config repo) rather than `master', so the one-shot builds on
top of in-flight doom-config work."
  (claude-repl-test--with-clean-state
    (let ((captured-base :unset))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) "tweak the modeline"))
                ((symbol-function 'claude-repl--spawn-workspace-generation)
                 (lambda (_raw _prefixed _git-root base _fork-from)
                   (setq captured-base base))))
        (claude-repl-create-doom-oneshot-workspace-from-current-branch)
        (should (equal captured-base "HEAD"))))))

(ert-deftest claude-repl-test-create-doom-oneshot-from-current-branch-pins-git-root-to-doom-config ()
  "The current-branch variant still pins git-root to `~/.config/doom'
regardless of the calling workspace's project — only the base ref
changes from `master' to HEAD."
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
        (claude-repl-create-doom-oneshot-workspace-from-current-branch)
        (should (equal captured-git-root claude-repl--doom-config-dir))))))

(ert-deftest claude-repl-test-create-doom-oneshot-from-current-branch-appends-merge-suffix ()
  "The current-branch variant must also append the merge-on-success suffix
to the prefixed prompt — the spawned agent still needs to know to invoke
`/workspace-merge' after a successful implementation."
  (claude-repl-test--with-clean-state
    (let ((captured-prefixed :unset))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) "tweak the modeline"))
                ((symbol-function 'claude-repl--spawn-workspace-generation)
                 (lambda (_raw prefixed _git-root _base _fork-from)
                   (setq captured-prefixed prefixed))))
        (claude-repl-create-doom-oneshot-workspace-from-current-branch)
        (should (string-match-p "/workspace-merge" captured-prefixed))
        (should (string-match-p
                 (regexp-quote claude-repl--oneshot-merge-suffix)
                 captured-prefixed))))))

(ert-deftest claude-repl-test-create-doom-oneshot-from-current-branch-keeps-raw-prompt-clean ()
  "The merge suffix must not pollute the raw prompt used for slug
generation — same constraint as the master variant."
  (claude-repl-test--with-clean-state
    (let ((captured-raw :unset))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) "tweak the modeline"))
                ((symbol-function 'claude-repl--spawn-workspace-generation)
                 (lambda (raw _prefixed _git-root _base _fork-from)
                   (setq captured-raw raw))))
        (claude-repl-create-doom-oneshot-workspace-from-current-branch)
        (should (equal captured-raw "tweak the modeline"))
        (should-not (string-match-p "/workspace-merge" captured-raw))))))

(ert-deftest claude-repl-test-create-doom-oneshot-from-current-branch-rejects-empty-prompt ()
  "An empty/whitespace prompt is rejected for the current-branch variant
too — there is nothing to slug or implement."
  (claude-repl-test--with-clean-state
    (let ((spawned nil))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) "   "))
                ((symbol-function 'claude-repl--spawn-workspace-generation)
                 (lambda (&rest _) (setq spawned t))))
        (should-error
         (claude-repl-create-doom-oneshot-workspace-from-current-branch)
         :type 'user-error)
        (should-not spawned)))))

(ert-deftest claude-repl-test-create-doom-oneshot-default-base-is-master ()
  "Calling the parent function with no BASE arg still defaults to `master'
— preserves backwards compatibility for the existing `SPC j o' binding
and existing call sites that pass no arguments."
  (claude-repl-test--with-clean-state
    (let ((captured-base :unset))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) "tweak the modeline"))
                ((symbol-function 'claude-repl--spawn-workspace-generation)
                 (lambda (_raw _prefixed _git-root base _fork-from)
                   (setq captured-base base))))
        (claude-repl-create-doom-oneshot-workspace)
        (should (equal captured-base "master"))))))

;;;; ---- Tests: merge queue ----

(defmacro claude-repl-test--with-empty-merge-queue (&rest body)
  "Run BODY with `claude-repl--merge-queue' freshly empty.
The queue is a top-level defvar, so tests that enqueue MUST scrub it
afterwards or later tests inherit stale state."
  (declare (indent 0))
  `(let ((claude-repl--merge-queue nil))
     (unwind-protect (progn ,@body)
       (setq claude-repl--merge-queue nil))))

(ert-deftest claude-repl-test-ws-merge-queued-p-true-when-marker-set ()
  "WS with `:repl-state :merge-queued' is detected as queued."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws" :project-dir "/tmp/ws")
    (claude-repl--ws-put "ws" :repl-state :merge-queued)
    (should (claude-repl--ws-merge-queued-p "ws"))))

(ert-deftest claude-repl-test-ws-merge-queued-p-nil-when-unmarked ()
  "WS without the queued marker is not detected as queued."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws" :project-dir "/tmp/ws")
    (should-not (claude-repl--ws-merge-queued-p "ws"))))

(ert-deftest claude-repl-test-ws-merge-queued-p-nil-for-other-repl-states ()
  "Other `:repl-state' values are not mistaken for queued."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws" :project-dir "/tmp/ws")
    (claude-repl--ws-put "ws" :repl-state :merged)
    (should-not (claude-repl--ws-merge-queued-p "ws"))))

(ert-deftest claude-repl-test-any-cherry-pick-in-progress-false-on-clean-tree ()
  "No CHERRY_PICK_HEAD in any registered ws dir → returns nil."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws" :project-dir "/tmp/repo")
    (cl-letf (((symbol-function 'file-directory-p)
               (lambda (p) (equal p "/tmp/repo")))
              ((symbol-function 'claude-repl--cherry-pick-in-progress-p)
               (lambda (_root) nil)))
      (should-not (claude-repl--any-cherry-pick-in-progress-p)))))

(ert-deftest claude-repl-test-any-cherry-pick-in-progress-true-during-conflict ()
  "Any registered ws dir with CHERRY_PICK_HEAD → returns t."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws" :project-dir "/tmp/repo")
    (cl-letf (((symbol-function 'file-directory-p)
               (lambda (p) (equal p "/tmp/repo")))
              ((symbol-function 'claude-repl--cherry-pick-in-progress-p)
               (lambda (root) (equal root "/tmp/repo"))))
      (should (claude-repl--any-cherry-pick-in-progress-p)))))

(ert-deftest claude-repl-test-any-cherry-pick-in-progress-skips-missing-dir ()
  "Stale :project-dir entries (dir no longer exists) are skipped, not
errored on.  Tests the defensive `file-directory-p' guard."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws" :project-dir "/nonexistent/path/missing")
    (should-not (claude-repl--any-cherry-pick-in-progress-p))))

(ert-deftest claude-repl-test-enqueue-merge-appends-to-queue ()
  "`--enqueue-merge' appends a plist describing the request to the FIFO."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-empty-merge-queue
      (claude-repl--ws-put "ws1" :project-dir "/tmp/ws1")
      (claude-repl--enqueue-merge "ws1" t t)
      (should (equal claude-repl--merge-queue
                     '((:source-ws "ws1" :silent t :auto-resolve t)))))))

(ert-deftest claude-repl-test-enqueue-merge-marks-repl-state ()
  "`--enqueue-merge' flips the workspace's `:repl-state' to `:merge-queued'
so the drawer can route it under MERGING."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-empty-merge-queue
      (claude-repl--ws-put "ws1" :project-dir "/tmp/ws1")
      (claude-repl--enqueue-merge "ws1" nil nil)
      (should (eq (claude-repl--ws-get "ws1" :repl-state) :merge-queued)))))

(ert-deftest claude-repl-test-enqueue-merge-clears-claude-state ()
  "Stale `:claude-state' is cleared so the state glyph reflects queued."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-empty-merge-queue
      (claude-repl--ws-put "ws1" :project-dir "/tmp/ws1")
      (claude-repl--ws-put "ws1" :claude-state :thinking)
      (claude-repl--enqueue-merge "ws1" nil nil)
      (should (null (claude-repl--ws-get "ws1" :claude-state))))))

(ert-deftest claude-repl-test-enqueue-merge-preserves-fifo-order ()
  "Multiple enqueues land in arrival order — the drain must pop oldest first."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-empty-merge-queue
      (claude-repl--ws-put "ws1" :project-dir "/tmp/ws1")
      (claude-repl--ws-put "ws2" :project-dir "/tmp/ws2")
      (claude-repl--ws-put "ws3" :project-dir "/tmp/ws3")
      (claude-repl--enqueue-merge "ws1" t t)
      (claude-repl--enqueue-merge "ws2" nil t)
      (claude-repl--enqueue-merge "ws3" t nil)
      (should (equal (mapcar (lambda (e) (plist-get e :source-ws))
                             claude-repl--merge-queue)
                     '("ws1" "ws2" "ws3"))))))

(ert-deftest claude-repl-test-drain-merge-queue-noop-when-empty ()
  "Empty queue → drain does nothing, no error."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-empty-merge-queue
      (let ((called nil))
        (cl-letf (((symbol-function 'claude-repl--workspace-merge-into-source)
                   (lambda (&rest _) (setq called t))))
          (claude-repl--drain-merge-queue)
          (should-not called))))))

(ert-deftest claude-repl-test-drain-merge-queue-noop-when-cherry-pick-active ()
  "If any registered ws dir still has CHERRY_PICK_HEAD, drain leaves the
queue untouched — another caller will drain after the live cherry-pick
finishes."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-empty-merge-queue
      (claude-repl--ws-put "ws-blocker" :project-dir "/tmp/repo")
      (claude-repl--ws-put "ws1" :project-dir "/tmp/ws1")
      (claude-repl--enqueue-merge "ws1" t t)
      (cl-letf (((symbol-function 'file-directory-p)
                 (lambda (p) (member p '("/tmp/repo" "/tmp/ws1"))))
                ;; Block: at least one ws dir has CHERRY_PICK_HEAD.
                ((symbol-function 'claude-repl--cherry-pick-in-progress-p)
                 (lambda (root) (equal root "/tmp/repo"))))
        (let ((called nil))
          (cl-letf (((symbol-function 'claude-repl--workspace-merge-into-source)
                     (lambda (&rest _) (setq called t))))
            (claude-repl--drain-merge-queue)
            (should-not called)
            (should (= 1 (length claude-repl--merge-queue)))))))))

(ert-deftest claude-repl-test-drain-merge-queue-pops-oldest-first ()
  "Drain pops the oldest enqueued entry (FIFO)."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-empty-merge-queue
      (claude-repl--ws-put "ws1" :project-dir "/tmp/ws1")
      (claude-repl--ws-put "ws2" :project-dir "/tmp/ws2")
      (claude-repl--enqueue-merge "ws1" t t)
      (claude-repl--enqueue-merge "ws2" nil nil)
      (let ((dispatched nil))
        (cl-letf (((symbol-function 'claude-repl--workspace-merge-into-source)
                   (lambda (ws &optional silent auto)
                     (push (list ws silent auto) dispatched))))
          (claude-repl--drain-merge-queue)
          (should (equal dispatched '(("ws1" t t))))
          (should (equal (mapcar (lambda (e) (plist-get e :source-ws))
                                 claude-repl--merge-queue)
                         '("ws2"))))))))

(ert-deftest claude-repl-test-drain-merge-queue-clears-queued-marker ()
  "Drain clears the dispatched workspace's `:merge-queued' marker so the
re-entered `--workspace-merge-into-source' can flip `:merging' t
without precedence collisions."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-empty-merge-queue
      (claude-repl--ws-put "ws1" :project-dir "/tmp/ws1")
      (claude-repl--enqueue-merge "ws1" t t)
      (cl-letf (((symbol-function 'claude-repl--workspace-merge-into-source)
                 (lambda (&rest _) nil)))
        (claude-repl--drain-merge-queue)
        (should-not (eq (claude-repl--ws-get "ws1" :repl-state)
                        :merge-queued))))))

(ert-deftest claude-repl-test-drain-merge-queue-catches-deferred-error ()
  "Errors from a deferred merge are caught so a single bad entry does
not leave the queue stuck — drain returns normally, no signal."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-empty-merge-queue
      (claude-repl--ws-put "ws1" :project-dir "/tmp/ws1")
      (claude-repl--enqueue-merge "ws1" t t)
      (cl-letf (((symbol-function 'claude-repl--workspace-merge-into-source)
                 (lambda (&rest _) (error "boom"))))
        ;; Must not raise.
        (claude-repl--drain-merge-queue)))))

(ert-deftest claude-repl-test-enqueue-merge-persists-snapshot ()
  "`--enqueue-merge' triggers `claude-repl-save-workspace-snapshot' so a
restart restores the queue.  Stubs out the saver to confirm the call."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-empty-merge-queue
      (claude-repl--ws-put "ws1" :project-dir "/tmp/ws1")
      (let ((save-calls 0))
        (cl-letf (((symbol-function 'claude-repl-save-workspace-snapshot)
                   (lambda () (cl-incf save-calls))))
          (claude-repl--enqueue-merge "ws1" t t)
          (should (= 1 save-calls)))))))

(ert-deftest claude-repl-test-drain-merge-queue-persists-snapshot ()
  "`--drain-merge-queue' triggers a snapshot save AFTER popping the next
entry so the persisted queue reflects the post-pop length — a crash
mid-merge does not resurrect an already-dispatched entry."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-empty-merge-queue
      (claude-repl--ws-put "ws1" :project-dir "/tmp/ws1")
      (claude-repl--enqueue-merge "ws1" t t)
      (let ((save-calls 0)
            (queue-len-at-save nil))
        (cl-letf (((symbol-function 'claude-repl-save-workspace-snapshot)
                   (lambda ()
                     (cl-incf save-calls)
                     (setq queue-len-at-save (length claude-repl--merge-queue))))
                  ((symbol-function 'claude-repl--workspace-merge-into-source)
                   (lambda (&rest _) nil)))
          (claude-repl--drain-merge-queue)
          (should (= 1 save-calls))
          ;; Drain pops before saving, so the persisted queue has 0 entries.
          (should (= 0 queue-len-at-save)))))))

(ert-deftest claude-repl-test-persist-merge-queue-tolerates-missing-saver ()
  "`--persist-merge-queue' is a no-op when the saver isn't fboundp, so
test fixtures and partial-load environments don't crash on enqueue/drain."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-empty-merge-queue
      (cl-letf (((symbol-function 'fboundp)
                 (lambda (sym)
                   (if (eq sym 'claude-repl-save-workspace-snapshot)
                       nil
                     (funcall (symbol-function 'fboundp) sym)))))
        ;; Must not raise.
        (claude-repl--persist-merge-queue)))))

(ert-deftest claude-repl-test-persist-merge-queue-swallows-save-errors ()
  "Errors from the saver are caught and logged so a write failure does
not propagate into the queue mutator and stall the merge flow."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-empty-merge-queue
      (cl-letf (((symbol-function 'claude-repl-save-workspace-snapshot)
                 (lambda () (error "disk full"))))
        ;; Must not raise.
        (claude-repl--persist-merge-queue)))))

(ert-deftest claude-repl-test-workspace-merge-into-source-enqueues-when-cherry-pick-in-flight ()
  "When a cherry-pick is in progress in any registered ws dir, the new
merge request is parked on the queue rather than running."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-empty-merge-queue
      (claude-repl--ws-put "ws-blocker" :project-dir "/tmp/repo")
      ;; ws-pending has a registered project-dir so the "unknown ws"
      ;; guard doesn't fire before the queue check.
      (claude-repl--ws-put "ws-pending" :project-dir "/tmp/ws-pending")
      (cl-letf (((symbol-function 'file-directory-p)
                 (lambda (p) (member p '("/tmp/repo" "/tmp/ws-pending"))))
                ((symbol-function 'claude-repl--cherry-pick-in-progress-p)
                 (lambda (root) (equal root "/tmp/repo"))))
        (let ((merge-do-called nil))
          (cl-letf (((symbol-function 'claude-repl--workspace-merge-do)
                     (lambda (&rest _) (setq merge-do-called t))))
            (claude-repl--workspace-merge-into-source "ws-pending" t t)
            (should-not merge-do-called)
            (should (= 1 (length claude-repl--merge-queue)))
            (should (eq (claude-repl--ws-get "ws-pending" :repl-state)
                        :merge-queued))))))))

;;;; ---- Tests: claude-repl-create-explanation-engine-oneshot-workspace ----

(ert-deftest claude-repl-test-explanation-engine-oneshot-pins-git-root-to-explanation-engine ()
  "The explanation-engine one-shot pins git-root to
`~/workspace/ChessCom/explanation-engine' regardless of the calling
workspace's project, so SPC j O always dispatches into that repo."
  (claude-repl-test--with-clean-state
    (let ((captured-git-root :unset))
      (cl-letf (((symbol-function '+workspace-current-name)
                 (lambda () "unrelated-ws"))
                ((symbol-function 'claude-repl--ws-dir)
                 (lambda (_ws) "/tmp/unrelated-repo/"))
                ((symbol-function 'read-string)
                 (lambda (&rest _) "add caching to thing"))
                ((symbol-function 'claude-repl--spawn-workspace-generation)
                 (lambda (_raw _prefixed git-root _base _fork-from)
                   (setq captured-git-root git-root))))
        (claude-repl-create-explanation-engine-oneshot-workspace)
        (should (equal captured-git-root
                       claude-repl--explanation-engine-dir))
        (should (equal captured-git-root
                       (file-name-as-directory
                        (expand-file-name
                         "~/workspace/ChessCom/explanation-engine"))))))))

(ert-deftest claude-repl-test-explanation-engine-oneshot-uses-master-base ()
  "The explanation-engine one-shot branches off local `master'
(equivalent to `SPC TAB N' in that repo)."
  (claude-repl-test--with-clean-state
    (let ((captured-base :unset))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) "add caching to thing"))
                ((symbol-function 'claude-repl--spawn-workspace-generation)
                 (lambda (_raw _prefixed _git-root base _fork-from)
                   (setq captured-base base))))
        (claude-repl-create-explanation-engine-oneshot-workspace)
        (should (equal captured-base "master"))))))

(ert-deftest claude-repl-test-explanation-engine-oneshot-appends-create-pr-suffix-to-prefixed ()
  "The create-PR-on-success suffix is included in the PREFIXED prompt so
the spawned agent knows to invoke
`claude-repl--oneshot-create-pr-command' on success — this replaces the
`/workspace-merge' instruction used by the doom one-shot."
  (claude-repl-test--with-clean-state
    (let ((captured-prefixed :unset))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) "add caching to thing"))
                ((symbol-function 'claude-repl--spawn-workspace-generation)
                 (lambda (_raw prefixed _git-root _base _fork-from)
                   (setq captured-prefixed prefixed))))
        (claude-repl-create-explanation-engine-oneshot-workspace)
        (should (string-match-p
                 (regexp-quote claude-repl--oneshot-create-pr-command)
                 captured-prefixed))
        (should (string-match-p
                 (regexp-quote claude-repl--oneshot-create-pr-suffix)
                 captured-prefixed))))))

(ert-deftest claude-repl-test-explanation-engine-oneshot-chains-workspace-merge-after-create-pr ()
  "The explanation-engine one-shot chains `/workspace-merge' AFTER
`/create-or-update-pr' as a second-stage teardown — the prefixed prompt
must mention `/workspace-merge', and it must appear textually AFTER the
`/create-or-update-pr' reference so the chain reads chronologically
(implement → PR → CICD → workspace-merge)."
  (claude-repl-test--with-clean-state
    (let ((captured-prefixed :unset))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) "add caching to thing"))
                ((symbol-function 'claude-repl--spawn-workspace-generation)
                 (lambda (_raw prefixed _git-root _base _fork-from)
                   (setq captured-prefixed prefixed))))
        (claude-repl-create-explanation-engine-oneshot-workspace)
        (let ((pr-pos (string-match
                       (regexp-quote claude-repl--oneshot-create-pr-command)
                       captured-prefixed))
              (merge-pos (string-match "/workspace-merge" captured-prefixed)))
          (should pr-pos)
          (should merge-pos)
          (should (< pr-pos merge-pos)))))))

(ert-deftest claude-repl-test-explanation-engine-oneshot-keeps-raw-prompt-clean ()
  "The create-PR suffix is NOT appended to the raw prompt — raw is used
purely for slug generation and should not get polluted with slash
commands like `/create-or-update-pr', which would derail the slug."
  (claude-repl-test--with-clean-state
    (let ((captured-raw :unset))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) "add caching to thing"))
                ((symbol-function 'claude-repl--spawn-workspace-generation)
                 (lambda (raw _prefixed _git-root _base _fork-from)
                   (setq captured-raw raw))))
        (claude-repl-create-explanation-engine-oneshot-workspace)
        (should (equal captured-raw "add caching to thing"))
        (should-not (string-match-p "/create-or-update-pr"
                                    captured-raw))))))

(ert-deftest claude-repl-test-explanation-engine-oneshot-prefixed-includes-autonomous-prefix ()
  "The prefixed prompt still starts with the standard autonomous-prompt
prefix so the spawned agent runs autonomously without waiting."
  (claude-repl-test--with-clean-state
    (let ((captured-prefixed :unset))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) "add caching to thing"))
                ((symbol-function 'claude-repl--spawn-workspace-generation)
                 (lambda (_raw prefixed _git-root _base _fork-from)
                   (setq captured-prefixed prefixed))))
        (claude-repl-create-explanation-engine-oneshot-workspace)
        (should (string-prefix-p claude-repl--autonomous-prompt-prefix
                                 captured-prefixed))))))

(ert-deftest claude-repl-test-explanation-engine-oneshot-rejects-empty-prompt ()
  "An empty/whitespace prompt is rejected — there is nothing to slug or
implement, and we do not want to spawn a useless workspace."
  (claude-repl-test--with-clean-state
    (let ((spawned nil))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) "   "))
                ((symbol-function 'claude-repl--spawn-workspace-generation)
                 (lambda (&rest _) (setq spawned t))))
        (should-error
         (claude-repl-create-explanation-engine-oneshot-workspace)
         :type 'user-error)
        (should-not spawned)))))

(ert-deftest claude-repl-test-explanation-engine-oneshot-passes-no-fork-from ()
  "The explanation-engine one-shot is not a fork — fork-from must be nil
so the new workspace starts a fresh Claude session rather than resuming
someone else's."
  (claude-repl-test--with-clean-state
    (let ((captured-fork-from :unset))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) "add caching to thing"))
                ((symbol-function 'claude-repl--spawn-workspace-generation)
                 (lambda (_raw _prefixed _git-root _base fork-from)
                   (setq captured-fork-from fork-from))))
        (claude-repl-create-explanation-engine-oneshot-workspace)
        (should (null captured-fork-from))))))

(ert-deftest claude-repl-test-oneshot-create-pr-command-has-expected-flags ()
  "The PR command string must match exactly what the user specified for
the explanation-engine one-shot: `/create-or-update-pr --patch
--add-to-merge-queue --skip-tests' (no --self-certified, no `commit'
subcommand)."
  (should (equal claude-repl--oneshot-create-pr-command
                 "/create-or-update-pr --patch --add-to-merge-queue --skip-tests")))

(ert-deftest claude-repl-test-oneshot-create-pr-suffix-mentions-stop-on-ambiguity ()
  "The create-PR suffix tells the spawned agent to STOP (not push on)
when it hits genuine ambiguity it cannot resolve — same safety property
as the doom-oneshot merge suffix, so a faulty implementation isn't
auto-PRed."
  (should (string-match-p "STOP" claude-repl--oneshot-create-pr-suffix))
  (should (string-match-p "ambiguity"
                          claude-repl--oneshot-create-pr-suffix)))

(ert-deftest claude-repl-test-oneshot-create-pr-suffix-mentions-tests-and-commits ()
  "PR creation is gated on implementation, tests, AND commits — the
suffix must spell that out so the spawned agent doesn't PR half-finished
work."
  (should (string-match-p "tests"
                          claude-repl--oneshot-create-pr-suffix))
  (should (string-match-p "[Cc]ommit"
                          claude-repl--oneshot-create-pr-suffix)))

(ert-deftest claude-repl-test-explanation-engine-dir-points-to-chesscom-explanation-engine ()
  "Sanity check: the explanation-engine dir constant resolves to
`~/workspace/ChessCom/explanation-engine' with a trailing slash."
  (should (equal claude-repl--explanation-engine-dir
                 (file-name-as-directory
                  (expand-file-name
                   "~/workspace/ChessCom/explanation-engine")))))

;;;; ---- Tests: claude-repl--oneshot-create-pr-then-merge-followup ----

(ert-deftest claude-repl-test-oneshot-create-pr-then-merge-followup-mentions-workspace-merge ()
  "The follow-up clause must reference `/workspace-merge' — that's the
slash command the spawned agent invokes once CICD passes."
  (should (string-match-p "/workspace-merge"
                          claude-repl--oneshot-create-pr-then-merge-followup)))

(ert-deftest claude-repl-test-oneshot-create-pr-then-merge-followup-gates-on-check-cicd-pass ()
  "The follow-up clause must explicitly gate `/workspace-merge' on
`/check-cicd' returning PASS — without this gate the agent could tear
down the workspace even after a failing CI run."
  (should (string-match-p "/check-cicd"
                          claude-repl--oneshot-create-pr-then-merge-followup))
  (should (string-match-p "PASS"
                          claude-repl--oneshot-create-pr-then-merge-followup)))

(ert-deftest claude-repl-test-oneshot-create-pr-then-merge-followup-stops-on-check-cicd-fail ()
  "On CICD FAIL the follow-up clause must tell the agent to STOP and NOT
invoke `/workspace-merge' — otherwise a failing CI could still lead to a
workspace teardown that loses the editor state without the change landing."
  (should (string-match-p "FAIL"
                          claude-repl--oneshot-create-pr-then-merge-followup))
  (should (string-match-p "STOP"
                          claude-repl--oneshot-create-pr-then-merge-followup))
  ;; The "do NOT invoke /workspace-merge" instruction must appear so the
  ;; agent doesn't mis-read STOP as merely "stop the implementation" and
  ;; still fire the teardown.
  (should (string-match-p "NOT invoke `/workspace-merge`"
                          claude-repl--oneshot-create-pr-then-merge-followup)))

(ert-deftest claude-repl-test-oneshot-create-pr-then-merge-followup-references-create-pr-command ()
  "The follow-up clause must name the create-PR command it chains off —
otherwise the agent has to guess which prior invocation's CICD result
gates the workspace-merge."
  (should (string-match-p
           (regexp-quote claude-repl--oneshot-create-pr-command)
           claude-repl--oneshot-create-pr-then-merge-followup)))

;;;; ---- Tests: chained suffix integration ----

(ert-deftest claude-repl-test-oneshot-create-pr-suffix-includes-followup ()
  "The composed create-PR suffix must include the workspace-merge
follow-up clause — otherwise the chain is half-wired and the agent only
gets the first-stage gate."
  (should (string-match-p
           (regexp-quote claude-repl--oneshot-create-pr-then-merge-followup)
           claude-repl--oneshot-create-pr-suffix)))

(ert-deftest claude-repl-test-oneshot-create-pr-suffix-followup-comes-after-build-suffix ()
  "The follow-up clause must appear AFTER the build-oneshot-success-suffix
output, not before — order is load-bearing because the follow-up gates
on the first-stage invocation's CICD result."
  (let* ((first-stage (claude-repl--build-oneshot-success-suffix
                       (concat "`" claude-repl--oneshot-create-pr-command "`")
                       "push and queue this branch for merge"))
         (first-pos (string-match (regexp-quote first-stage)
                                  claude-repl--oneshot-create-pr-suffix))
         (followup-pos (string-match
                        (regexp-quote
                         claude-repl--oneshot-create-pr-then-merge-followup)
                        claude-repl--oneshot-create-pr-suffix)))
    (should first-pos)
    (should followup-pos)
    (should (< first-pos followup-pos))))

;;;; ---- Tests: claude-repl--build-oneshot-success-suffix ----

(ert-deftest claude-repl-test-build-oneshot-success-suffix-interpolates-invocation-twice ()
  "INVOCATION appears in BOTH the 'invoke X to Y' action sentence and the
'Only invoke X when ...' gate sentence — the helper must wire it through
both clauses, otherwise the gate dangles."
  (let ((suffix (claude-repl--build-oneshot-success-suffix
                 "the /foo skill" "do the foo thing")))
    (with-temp-buffer
      (insert suffix)
      (goto-char (point-min))
      (should (search-forward "invoke the /foo skill to do the foo thing"
                              nil t))
      (should (search-forward "Only invoke the /foo skill when"
                              nil t)))))

(ert-deftest claude-repl-test-build-oneshot-success-suffix-mentions-stop-on-ambiguity ()
  "Every success-suffix MUST carry the STOP-on-ambiguity safety clause —
otherwise a one-shot can auto-merge / auto-PR a faulty implementation."
  (let ((suffix (claude-repl--build-oneshot-success-suffix
                 "the /foo skill" "do the foo thing")))
    (should (string-match-p "STOP" suffix))
    (should (string-match-p "ambiguity" suffix))))

(ert-deftest claude-repl-test-build-oneshot-success-suffix-gates-on-tests-and-commits ()
  "The gate clause must require implementation AND tests AND commits, not
just implementation — the helper hard-codes this gate so every variant
inherits it."
  (let ((suffix (claude-repl--build-oneshot-success-suffix
                 "the /foo skill" "do the foo thing")))
    (should (string-match-p "tests" suffix))
    (should (string-match-p "[Cc]ommit" suffix))))

;;;; ---- Tests: claude-repl--create-pinned-oneshot-workspace ----

(ert-deftest claude-repl-test-create-pinned-oneshot-uses-tag-in-minibuffer-prompt ()
  "TAG is interpolated into the minibuffer prompt so distinct one-shot
variants are visually distinguishable when the user is typing the
preemptive prompt."
  (claude-repl-test--with-clean-state
    (let ((captured-mb-prompt :unset))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (prompt &rest _)
                   (setq captured-mb-prompt prompt)
                   "do a thing"))
                ((symbol-function 'claude-repl--spawn-workspace-generation)
                 (lambda (&rest _) nil)))
        (claude-repl--create-pinned-oneshot-workspace
         "/tmp/repo/" 'master "SUFFIX" "test-tag")
        (should (equal captured-mb-prompt "One-shot test-tag prompt: "))))))

(ert-deftest claude-repl-test-create-pinned-oneshot-rejects-empty-prompt ()
  "Empty/whitespace prompt is rejected at the helper level so every
variant inherits the validation — no caller can accidentally skip it."
  (claude-repl-test--with-clean-state
    (let ((spawned nil))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) "   "))
                ((symbol-function 'claude-repl--spawn-workspace-generation)
                 (lambda (&rest _) (setq spawned t))))
        (should-error
         (claude-repl--create-pinned-oneshot-workspace
          "/tmp/repo/" 'master "SUFFIX" "test-tag")
         :type 'user-error)
        (should-not spawned)))))

(ert-deftest claude-repl-test-create-pinned-oneshot-passes-git-root-through ()
  "GIT-ROOT flows verbatim through to `claude-repl--spawn-workspace-generation'
— a caller pinning a non-default repo must see exactly that path."
  (claude-repl-test--with-clean-state
    (let ((captured-git-root :unset))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) "do a thing"))
                ((symbol-function 'claude-repl--spawn-workspace-generation)
                 (lambda (_raw _prefixed git-root _base _fork-from)
                   (setq captured-git-root git-root))))
        (claude-repl--create-pinned-oneshot-workspace
         "/tmp/some-pinned-repo/" 'master "SUFFIX" "test-tag")
        (should (equal captured-git-root "/tmp/some-pinned-repo/"))))))

(ert-deftest claude-repl-test-create-pinned-oneshot-appends-suffix-to-prefixed-only ()
  "SUFFIX is appended to the PREFIXED prompt (agent's first message) but
NOT to the RAW prompt (used for slug generation) — keeps the
workspace-name slug clean across every one-shot variant."
  (claude-repl-test--with-clean-state
    (let ((captured-raw :unset)
          (captured-prefixed :unset))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) "do a thing"))
                ((symbol-function 'claude-repl--spawn-workspace-generation)
                 (lambda (raw prefixed _git-root _base _fork-from)
                   (setq captured-raw raw)
                   (setq captured-prefixed prefixed))))
        (claude-repl--create-pinned-oneshot-workspace
         "/tmp/repo/" 'master "::SENTINEL-SUFFIX::" "test-tag")
        (should-not (string-match-p "::SENTINEL-SUFFIX::" captured-raw))
        (should (string-match-p "::SENTINEL-SUFFIX::"
                                captured-prefixed))))))

;;;; ---- Tests: eval-code-string ----

(ert-deftest claude-repl-test-eval-code-string-returns-value-string ()
  "Successful eval populates `:value-string' with `prin1-to-string' of result."
  (let ((result (claude-repl--eval-code-string "(+ 1 2)")))
    (should (equal "3" (plist-get result :value-string)))
    (should (null (plist-get result :error)))))

(ert-deftest claude-repl-test-eval-code-string-captures-printed-output ()
  "`princ' inside the form is captured into `:printed'."
  (let ((result (claude-repl--eval-code-string "(princ \"hello\")")))
    (should (string-match-p "hello" (plist-get result :printed)))))

(ert-deftest claude-repl-test-eval-code-string-evaluates-multiple-forms ()
  "Multiple top-level forms evaluate in order; `:value-string' tracks the last."
  (let ((result (claude-repl--eval-code-string "(princ \"a\") (+ 10 20)")))
    (should (equal "30" (plist-get result :value-string)))
    (should (string-match-p "a" (plist-get result :printed)))))

(ert-deftest claude-repl-test-eval-code-string-traps-error ()
  "Errors are trapped into `:error' instead of propagating."
  (let ((result (claude-repl--eval-code-string "(error \"boom\")")))
    (should (null (plist-get result :value-string)))
    (should (stringp (plist-get result :error)))
    (should (string-match-p "boom" (plist-get result :error)))))

(ert-deftest claude-repl-test-eval-code-string-error-preserves-prior-output ()
  "When form N raises, prior forms' printed output is still in `:printed'."
  (let ((result (claude-repl--eval-code-string "(princ \"first\") (error \"boom\")")))
    (should (stringp (plist-get result :error)))
    (should (string-match-p "first" (plist-get result :printed)))))

(ert-deftest claude-repl-test-eval-code-string-empty-input-yields-nil-value ()
  "Whitespace-only code yields a `nil' value string and no error."
  (let ((result (claude-repl--eval-code-string "   ")))
    (should (equal "nil" (plist-get result :value-string)))
    (should (null (plist-get result :error)))))

;;;; ---- Tests: eval-format-prompt ----

(ert-deftest claude-repl-test-eval-format-prompt-success-includes-result ()
  "Success path includes the `;; result:' section with the prin1 value."
  (let ((text (claude-repl--eval-format-prompt "(+ 1 2)" nil "" "3" nil)))
    (should (string-match-p ";; code:" text))
    (should (string-match-p ";; result:" text))
    (should (string-match-p "3" text))
    (should-not (string-match-p ";; error:" text))))

(ert-deftest claude-repl-test-eval-format-prompt-error-includes-error-section ()
  "Error path omits `;; result:' and includes `;; error:' instead."
  (let ((text (claude-repl--eval-format-prompt "(error \"boom\")" nil "" nil "boom")))
    (should (string-match-p ";; error:" text))
    (should (string-match-p "boom" text))
    (should-not (string-match-p ";; result:" text))))

(ert-deftest claude-repl-test-eval-format-prompt-omits-empty-printed-section ()
  "Empty `printed' output is not echoed back as a `;; printed:' section."
  (let ((text (claude-repl--eval-format-prompt "(+ 1 2)" nil "" "3" nil)))
    (should-not (string-match-p ";; printed:" text))))

(ert-deftest claude-repl-test-eval-format-prompt-includes-printed-when-present ()
  "Non-empty `printed' output renders as a `;; printed:' section."
  (let ((text (claude-repl--eval-format-prompt
               "(princ \"hi\")" nil "hi" "\"hi\"" nil)))
    (should (string-match-p ";; printed:" text))
    (should (string-match-p "hi" text))))

(ert-deftest claude-repl-test-eval-format-prompt-note-renders-in-header ()
  "A non-empty `note' is appended to the header line."
  (let ((text (claude-repl--eval-format-prompt
               "(+ 1 2)" "warmup" "" "3" nil)))
    (should (string-match-p "note: warmup" text))))

(ert-deftest claude-repl-test-eval-format-prompt-error-header-says-error ()
  "Error variant uses `Elisp eval ERROR' header rather than `Elisp eval result'."
  (let ((text (claude-repl--eval-format-prompt
               "(error \"x\")" nil "" nil "x")))
    (should (string-match-p "Elisp eval ERROR" text))
    (should-not (string-match-p "Elisp eval result" text))))

;;;; ---- Tests: eval-truncate ----

(ert-deftest claude-repl-test-eval-truncate-under-cap-returns-unchanged ()
  "Text shorter than the cap passes through verbatim."
  (let ((claude-repl-eval-output-max-chars 100))
    (should (equal "short" (claude-repl--eval-truncate "short")))))

(ert-deftest claude-repl-test-eval-truncate-over-cap-clips-and-annotates ()
  "Text longer than the cap is clipped and gets a `[truncated to N chars]' marker."
  (let* ((claude-repl-eval-output-max-chars 5)
         (out (claude-repl--eval-truncate "abcdefghij")))
    (should (string-prefix-p "abcde" out))
    (should (string-match-p "truncated to 5 chars" out))))

(ert-deftest claude-repl-test-eval-truncate-zero-cap-disables-truncation ()
  "A cap of 0 returns the input unchanged regardless of length."
  (let ((claude-repl-eval-output-max-chars 0))
    (should (equal "abcdefghij"
                   (claude-repl--eval-truncate "abcdefghij")))))

;;;; ---- Tests: handle-eval-command ----

(ert-deftest claude-repl-test-handle-eval-command-sends-result-to-workspace ()
  "`workspace' field routes the formatted result back via `claude-repl--send'."
  (let ((sent nil))
    (cl-letf (((symbol-function 'claude-repl--send)
               (lambda (prompt ws &rest _) (push (cons ws prompt) sent))))
      (claude-repl--handle-eval-command
       '((type . "eval") (code . "(+ 1 2)") (workspace . "ws1"))))
    (should (= 1 (length sent)))
    (should (equal "ws1" (car (car sent))))
    (should (string-match-p ";; result:" (cdr (car sent))))
    (should (string-match-p "3" (cdr (car sent))))))

(ert-deftest claude-repl-test-handle-eval-command-no-workspace-no-send ()
  "Without `workspace', the result is computed but never sent."
  (let ((sent nil))
    (cl-letf (((symbol-function 'claude-repl--send)
               (lambda (&rest args) (push args sent))))
      (claude-repl--handle-eval-command
       '((type . "eval") (code . "(+ 1 2)"))))
    (should-not sent)))

(ert-deftest claude-repl-test-handle-eval-command-empty-workspace-no-send ()
  "Empty-string `workspace' is treated as absent — no send is dispatched."
  (let ((sent nil))
    (cl-letf (((symbol-function 'claude-repl--send)
               (lambda (&rest args) (push args sent))))
      (claude-repl--handle-eval-command
       '((type . "eval") (code . "(+ 1 2)") (workspace . ""))))
    (should-not sent)))

(ert-deftest claude-repl-test-handle-eval-command-missing-code-skips ()
  "Missing `code' field skips evaluation entirely — no send, no crash."
  (let ((sent nil))
    (cl-letf (((symbol-function 'claude-repl--send)
               (lambda (&rest args) (push args sent))))
      (claude-repl--handle-eval-command
       '((type . "eval") (workspace . "ws1"))))
    (should-not sent)))

(ert-deftest claude-repl-test-handle-eval-command-empty-code-skips ()
  "Empty-string `code' is treated as missing — no send is dispatched."
  (let ((sent nil))
    (cl-letf (((symbol-function 'claude-repl--send)
               (lambda (&rest args) (push args sent))))
      (claude-repl--handle-eval-command
       '((type . "eval") (code . "   ") (workspace . "ws1"))))
    (should-not sent)))

(ert-deftest claude-repl-test-handle-eval-command-error-sends-error-prompt ()
  "An error inside the evaluated code is reported back as the `;; error:' section."
  (let ((sent nil))
    (cl-letf (((symbol-function 'claude-repl--send)
               (lambda (prompt ws &rest _) (push (cons ws prompt) sent))))
      (claude-repl--handle-eval-command
       '((type . "eval") (code . "(error \"boom\")") (workspace . "ws1"))))
    (should (= 1 (length sent)))
    (should (string-match-p "Elisp eval ERROR" (cdr (car sent))))
    (should (string-match-p "boom" (cdr (car sent))))))

(ert-deftest claude-repl-test-handle-eval-command-note-passed-through ()
  "Optional `note' field is echoed back in the response header."
  (let ((sent nil))
    (cl-letf (((symbol-function 'claude-repl--send)
               (lambda (prompt ws &rest _) (push (cons ws prompt) sent))))
      (claude-repl--handle-eval-command
       '((type . "eval") (code . "(+ 1 2)") (workspace . "ws1") (note . "tick"))))
    (should (string-match-p "note: tick" (cdr (car sent))))))

;;; test-worktree.el ends here
