;;; test-commands.el --- ERT tests for claude-repl commands -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for commands.el — user commands, file references, diff analysis,
;; and standalone interactive commands.
;;
;; Run with:
;;   emacs -batch -Q -l ert -l test-commands.el -f ert-run-tests-batch-and-exit

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- claude-repl--buffer-relative-path ----

(ert-deftest claude-repl-cmd-test-buffer-relative-path/non-file-buffer ()
  "buffer-relative-path signals user-error for non-file buffers."
  (claude-repl-test--with-temp-buffer " *test-no-file*"
    (should-error (claude-repl--buffer-relative-path) :type 'user-error)))

(ert-deftest claude-repl-cmd-test-buffer-relative-path/file-buffer ()
  "buffer-relative-path returns path relative to project root."
  (cl-letf (((symbol-function 'buffer-file-name)
             (lambda (&optional _buf) "/project/src/foo.el"))
            ((symbol-function 'claude-repl--ws-dir)
             (lambda (_ws) "/project/"))
            ((symbol-function '+workspace-current-name)
             (lambda () "test-ws")))
    (should (equal (claude-repl--buffer-relative-path) "src/foo.el"))))

(ert-deftest claude-repl-cmd-test-buffer-relative-path/nested-subdir ()
  "buffer-relative-path works for deeply nested paths."
  (cl-letf (((symbol-function 'buffer-file-name)
             (lambda (&optional _buf) "/project/a/b/c/deep.el"))
            ((symbol-function 'claude-repl--ws-dir)
             (lambda (_ws) "/project/"))
            ((symbol-function '+workspace-current-name)
             (lambda () "test-ws")))
    (should (equal (claude-repl--buffer-relative-path) "a/b/c/deep.el"))))

(ert-deftest claude-repl-cmd-test-buffer-relative-path/file-at-root ()
  "buffer-relative-path returns bare filename when file is at project root."
  (cl-letf (((symbol-function 'buffer-file-name)
             (lambda (&optional _buf) "/project/file.el"))
            ((symbol-function 'claude-repl--ws-dir)
             (lambda (_ws) "/project/"))
            ((symbol-function '+workspace-current-name)
             (lambda () "test-ws")))
    (should (equal (claude-repl--buffer-relative-path) "file.el"))))

(ert-deftest claude-repl-cmd-test-buffer-relative-path/root-without-trailing-slash ()
  "buffer-relative-path works when ws-dir omits trailing slash."
  (cl-letf (((symbol-function 'buffer-file-name)
             (lambda (&optional _buf) "/project/src/bar.el"))
            ((symbol-function 'claude-repl--ws-dir)
             (lambda (_ws) "/project"))
            ((symbol-function '+workspace-current-name)
             (lambda () "test-ws")))
    (should (equal (claude-repl--buffer-relative-path) "src/bar.el"))))

;;;; ---- claude-repl--format-file-ref ----

(ert-deftest claude-repl-cmd-test-format-file-ref/no-region ()
  "format-file-ref returns file:line when no region is active."
  (cl-letf (((symbol-function 'claude-repl--buffer-relative-path)
             (lambda () "src/foo.el"))
            ((symbol-function 'use-region-p)
             (lambda () nil))
            ((symbol-function 'line-number-at-pos)
             (lambda (&optional _pos) 42)))
    (should (equal (claude-repl--format-file-ref) "src/foo.el:42"))))

(ert-deftest claude-repl-cmd-test-format-file-ref/with-region ()
  "format-file-ref returns file:start-end when region is active."
  (with-temp-buffer
    (transient-mark-mode 1)
    (insert "line1\nline2\nline3\nline4\nline5\n")
    ;; Select lines 2-4
    (goto-char (point-min))
    (forward-line 1)
    (set-mark (point))
    (forward-line 2)
    (cl-letf (((symbol-function 'claude-repl--buffer-relative-path)
               (lambda () "src/foo.el")))
      (let ((result (claude-repl--format-file-ref)))
        ;; Should be file:startline-endline format
        (should (string-match "^src/foo\\.el:[0-9]+-[0-9]+$" result))
        ;; Mark should be deactivated
        (should-not (use-region-p))))))

(ert-deftest claude-repl-cmd-test-format-file-ref/single-line-region ()
  "format-file-ref with region on a single line returns same start and end."
  (with-temp-buffer
    (transient-mark-mode 1)
    (insert "line1\nline2\nline3\n")
    (goto-char (point-min))
    (forward-line 1)
    (set-mark (point))
    (end-of-line)
    (cl-letf (((symbol-function 'claude-repl--buffer-relative-path)
               (lambda () "test.el")))
      (let ((result (claude-repl--format-file-ref)))
        (should (equal result "test.el:2-2"))))))

(ert-deftest claude-repl-cmd-test-format-file-ref/first-line ()
  "format-file-ref at first line returns file:1."
  (cl-letf (((symbol-function 'claude-repl--buffer-relative-path)
             (lambda () "root.el"))
            ((symbol-function 'use-region-p)
             (lambda () nil))
            ((symbol-function 'line-number-at-pos)
             (lambda (&optional _pos) 1)))
    (should (equal (claude-repl--format-file-ref) "root.el:1"))))

;;;; ---- claude-repl--format-magit-hunk-ref ----

(ert-deftest claude-repl-cmd-test-format-magit-hunk-ref/basic ()
  "format-magit-hunk-ref returns file:start-end from magit hunk section."
  (let ((mock-section (record 'magit-section nil nil nil nil nil nil nil nil)))
    ;; Stub eieio-oref (the runtime function that `oref' expands to)
    ;; since `oref' is a macro and cannot be stubbed via cl-letf.
    (cl-letf (((symbol-function 'magit-current-section)
               (lambda () mock-section))
              ((symbol-function 'magit-file-at-point)
               (lambda () "src/main.go"))
              ((symbol-function 'eieio-oref)
               (lambda (_obj _slot) '(10 5)))
              ((symbol-function 'magit-toplevel)
               (lambda () "/project/"))
              ((symbol-function 'claude-repl--ws-dir)
               (lambda (_ws) "/project/"))
              ((symbol-function '+workspace-current-name)
               (lambda () "test-ws")))
      (should (equal (claude-repl--format-magit-hunk-ref) "src/main.go:10-14")))))

(ert-deftest claude-repl-cmd-test-format-magit-hunk-ref/single-line-hunk ()
  "format-magit-hunk-ref with a 1-line hunk returns start equal to end."
  (let ((mock-section (record 'magit-section nil nil nil nil nil nil nil nil)))
    (cl-letf (((symbol-function 'magit-current-section)
               (lambda () mock-section))
              ((symbol-function 'magit-file-at-point)
               (lambda () "file.py"))
              ((symbol-function 'eieio-oref)
               (lambda (_obj _slot) '(25 1)))
              ((symbol-function 'magit-toplevel)
               (lambda () "/repo/"))
              ((symbol-function 'claude-repl--ws-dir)
               (lambda (_ws) "/repo/"))
              ((symbol-function '+workspace-current-name)
               (lambda () "test-ws")))
      (should (equal (claude-repl--format-magit-hunk-ref) "file.py:25-25")))))

(ert-deftest claude-repl-cmd-test-format-magit-hunk-ref/different-roots ()
  "format-magit-hunk-ref computes relative path from ws-dir, not magit-toplevel."
  (let ((mock-section (record 'magit-section nil nil nil nil nil nil nil nil)))
    (cl-letf (((symbol-function 'magit-current-section)
               (lambda () mock-section))
              ((symbol-function 'magit-file-at-point)
               (lambda () "subdir/file.rs"))
              ((symbol-function 'eieio-oref)
               (lambda (_obj _slot) '(1 3)))
              ((symbol-function 'magit-toplevel)
               (lambda () "/workspace/project/"))
              ((symbol-function 'claude-repl--ws-dir)
               (lambda (_ws) "/workspace/project/"))
              ((symbol-function '+workspace-current-name)
               (lambda () "test-ws")))
      (should (equal (claude-repl--format-magit-hunk-ref) "subdir/file.rs:1-3")))))

;;;; ---- claude-repl--context-reference ----

(ert-deftest claude-repl-cmd-test-context-reference/non-magit-delegates-to-format-file-ref ()
  "context-reference delegates to format-file-ref when not in a magit mode."
  (with-temp-buffer
    (cl-letf (((symbol-function 'claude-repl--format-file-ref)
               (lambda () "src/foo.el:10")))
      (should (equal (claude-repl--context-reference) "src/foo.el:10")))))

(ert-deftest claude-repl-cmd-test-context-reference/magit-hunk-delegates-to-magit-ref ()
  "context-reference delegates to format-magit-hunk-ref in magit hunk context."
  (with-temp-buffer
    (let ((major-mode 'magit-diff-mode))
      (cl-letf (((symbol-function 'magit-section-match)
                 (lambda (_type) t))
                ((symbol-function 'claude-repl--format-magit-hunk-ref)
                 (lambda () "src/main.go:10-14"))
                ((symbol-function 'derived-mode-p)
                 (lambda (&rest _modes) t)))
        (should (equal (claude-repl--context-reference) "src/main.go:10-14"))))))

(ert-deftest claude-repl-cmd-test-context-reference/magit-non-hunk-section ()
  "context-reference falls through to format-file-ref in magit non-hunk section."
  (with-temp-buffer
    (let ((major-mode 'magit-status-mode))
      (cl-letf (((symbol-function 'magit-section-match)
                 (lambda (_type) nil))
                ((symbol-function 'claude-repl--format-file-ref)
                 (lambda () "src/bar.el:5"))
                ((symbol-function 'derived-mode-p)
                 (lambda (&rest _modes) t)))
        (should (equal (claude-repl--context-reference) "src/bar.el:5"))))))

;;;; ---- claude-repl--send-diff-analysis ----

(ert-deftest claude-repl-cmd-test-send-diff-analysis/formats-message ()
  "send-diff-analysis formats 'for the SPEC, PROMPT' and sends it."
  (let (sent-text)
    (cl-letf (((symbol-function 'claude-repl--send-to-claude)
               (lambda (text) (setq sent-text text))))
      (claude-repl--send-diff-analysis "unstaged changes (git diff)" "please explain the changes")
      (should (equal sent-text "for the unstaged changes (git diff), please explain the changes")))))

;;;; ---- claude-repl--resolve-change-spec ----

(ert-deftest claude-repl-cmd-test-resolve-change-spec/string-default ()
  "resolve-change-spec returns string default-spec when no override."
  (should (equal (claude-repl--resolve-change-spec
                  'worktree "unstaged changes (git diff)" nil)
                 "unstaged changes (git diff)")))

(ert-deftest claude-repl-cmd-test-resolve-change-spec/branch-returns-symbol ()
  "resolve-change-spec returns symbol for :use-branch-diff-spec."
  (should (eq (claude-repl--resolve-change-spec
               'branch :use-branch-diff-spec nil)
              'claude-repl-branch-diff-spec)))

(ert-deftest claude-repl-cmd-test-resolve-change-spec/override-takes-precedence ()
  "resolve-change-spec prefers override over default-spec."
  (defvar claude-repl-test--override-alist
    '((worktree . "OVERRIDDEN worktree spec")))
  (should (equal (claude-repl--resolve-change-spec
                  'worktree "default spec" 'claude-repl-test--override-alist)
                 "OVERRIDDEN worktree spec")))

(ert-deftest claude-repl-cmd-test-resolve-change-spec/override-missing-scope-falls-through ()
  "resolve-change-spec falls through to default when override has no entry for scope."
  (defvar claude-repl-test--partial-override
    '((staged . "overridden staged")))
  (should (equal (claude-repl--resolve-change-spec
                  'worktree "default worktree" 'claude-repl-test--partial-override)
                 "default worktree")))

(ert-deftest claude-repl-cmd-test-resolve-change-spec/override-branch-still-uses-symbol ()
  "resolve-change-spec returns branch symbol even with overrides that lack branch entry."
  (defvar claude-repl-test--no-branch-override
    '((worktree . "override")))
  (should (eq (claude-repl--resolve-change-spec
               'branch :use-branch-diff-spec 'claude-repl-test--no-branch-override)
              'claude-repl-branch-diff-spec)))

;;;; ---- claude-repl--send-to-claude ----

(ert-deftest claude-repl-cmd-test-send-to-claude/not-running-initializes-first ()
  "send-to-claude calls initialize-claude when Claude is not running."
  (let (init-called sent-text)
    (claude-repl-test--with-clean-state
      (let ((fake-vterm-buf (get-buffer-create " *test-vterm*")))
        (unwind-protect
            (progn
              (claude-repl--ws-put "test-ws" :vterm-buffer fake-vterm-buf)
              (cl-letf (((symbol-function '+workspace-current-name)
                         (lambda () "test-ws"))
                        ((symbol-function 'claude-repl--claude-running-p)
                         (lambda (_ws) nil))
                        ((symbol-function 'claude-repl--initialize-claude)
                         (lambda (_ws) (setq init-called t)))
                        ((symbol-function 'claude-repl--send-input-to-vterm)
                         (lambda (_buf text) (setq sent-text text))))
                (claude-repl--send-to-claude "hello claude")
                (should init-called)
                (should (equal sent-text "hello claude"))))
          (kill-buffer fake-vterm-buf))))))

(ert-deftest claude-repl-cmd-test-send-to-claude/running-skips-init ()
  "send-to-claude skips initialize-claude when Claude is already running."
  (let (init-called sent-buf sent-text)
    (claude-repl-test--with-clean-state
      (let ((fake-vterm-buf (get-buffer-create " *test-vterm*")))
        (unwind-protect
            (progn
              (claude-repl--ws-put "test-ws" :vterm-buffer fake-vterm-buf)
              (cl-letf (((symbol-function '+workspace-current-name)
                         (lambda () "test-ws"))
                        ((symbol-function 'claude-repl--claude-running-p)
                         (lambda (_ws) t))
                        ((symbol-function 'claude-repl--initialize-claude)
                         (lambda (_ws) (setq init-called t)))
                        ((symbol-function 'claude-repl--send-input-to-vterm)
                         (lambda (buf text)
                           (setq sent-buf buf sent-text text))))
                (claude-repl--send-to-claude "hello claude")
                (should-not init-called)
                (should (eq sent-buf fake-vterm-buf))
                (should (equal sent-text "hello claude"))))
          (kill-buffer fake-vterm-buf))))))

;;;; ---- claude-repl-explain ----

(ert-deftest claude-repl-cmd-test-explain/sends-context-reference ()
  "explain sends 'please explain REF' to claude."
  (let (sent-text)
    (cl-letf (((symbol-function 'claude-repl--context-reference)
               (lambda () "src/foo.el:42"))
              ((symbol-function 'claude-repl--send-to-claude)
               (lambda (text) (setq sent-text text))))
      (claude-repl-explain)
      (should (equal sent-text "please explain src/foo.el:42")))))

;;;; ---- claude-repl-explain-prompt ----

(ert-deftest claude-repl-cmd-test-explain-prompt/sends-user-input ()
  "explain-prompt sends user-provided text to claude."
  (let (sent-text)
    (cl-letf (((symbol-function 'claude-repl--context-reference)
               (lambda () "src/foo.el:42"))
              ((symbol-function 'read-string)
               (lambda (_prompt _initial) "review src/foo.el:42 for bugs"))
              ((symbol-function 'claude-repl--send-to-claude)
               (lambda (text) (setq sent-text text))))
      (claude-repl-explain-prompt)
      (should (equal sent-text "review src/foo.el:42 for bugs")))))

(ert-deftest claude-repl-cmd-test-explain-prompt/prefills-context-reference ()
  "explain-prompt pre-fills the minibuffer with the context reference."
  (let (initial-input)
    (cl-letf (((symbol-function 'claude-repl--context-reference)
               (lambda () "src/bar.el:10-20"))
              ((symbol-function 'read-string)
               (lambda (_prompt initial) (setq initial-input initial) "anything"))
              ((symbol-function 'claude-repl--send-to-claude)
               (lambda (_text))))
      (claude-repl-explain-prompt)
      (should (equal initial-input "src/bar.el:10-20")))))

(ert-deftest claude-repl-cmd-test-explain-prompt/empty-input-does-not-send ()
  "explain-prompt does not send when user provides empty input."
  (let (sent-text)
    (cl-letf (((symbol-function 'claude-repl--context-reference)
               (lambda () "src/foo.el:1"))
              ((symbol-function 'read-string)
               (lambda (_prompt _initial) ""))
              ((symbol-function 'claude-repl--send-to-claude)
               (lambda (text) (setq sent-text text))))
      (claude-repl-explain-prompt)
      (should (null sent-text)))))

;;;; ---- claude-repl--send-interrupt-escape ----

(ert-deftest claude-repl-cmd-test-send-interrupt-escape/sends-two-escapes ()
  "send-interrupt-escape sends two escape key presses to vterm buffer."
  (let ((keys-sent '()))
    (claude-repl-test--with-temp-buffer " *test-vterm-interrupt*"
      (cl-letf (((symbol-function 'vterm-send-key)
                 (lambda (key) (push key keys-sent))))
        (claude-repl--send-interrupt-escape "test-ws" (current-buffer))
        (should (equal (nreverse keys-sent) '("<escape>" "<escape>")))))))

;;;; ---- claude-repl--enter-insert-mode ----

(ert-deftest claude-repl-cmd-test-enter-insert-mode/live-buffer ()
  "enter-insert-mode sends 'i' to a live buffer."
  (let (sent-string)
    (claude-repl-test--with-temp-buffer " *test-vterm-insert*"
      (cl-letf (((symbol-function 'vterm-send-string)
                 (lambda (str) (setq sent-string str))))
        (claude-repl--enter-insert-mode (current-buffer))
        (should (equal sent-string "i"))))))

(ert-deftest claude-repl-cmd-test-enter-insert-mode/dead-buffer ()
  "enter-insert-mode is a no-op for a killed buffer."
  (let ((buf (get-buffer-create " *test-dead-buf*"))
        (send-called nil))
    (kill-buffer buf)
    (cl-letf (((symbol-function 'vterm-send-string)
               (lambda (_str) (setq send-called t))))
      (claude-repl--enter-insert-mode buf)
      (should-not send-called))))

;;;; ---- claude-repl-interrupt ----

(ert-deftest claude-repl-cmd-test-interrupt/sends-escape-when-vterm-live ()
  "interrupt sends escape keys when vterm is live."
  (let (escape-called)
    (claude-repl-test--with-clean-state
      (let ((fake-vterm-buf (get-buffer-create " *test-interrupt-vterm*")))
        (unwind-protect
            (progn
              (claude-repl--ws-put "test-ws" :vterm-buffer fake-vterm-buf)
              (cl-letf (((symbol-function 'claude-repl--vterm-live-p)
                         (lambda () t))
                        ((symbol-function '+workspace-current-name)
                         (lambda () "test-ws"))
                        ((symbol-function 'claude-repl--ws-get)
                         (lambda (_ws _key) fake-vterm-buf))
                        ((symbol-function 'claude-repl--send-interrupt-escape)
                         (lambda (_ws _buf) (setq escape-called t)))
                        ((symbol-function 'run-at-time)
                         (lambda (_time _repeat _fn _arg) nil)))
                (claude-repl-interrupt)
                (should escape-called)))
          (kill-buffer fake-vterm-buf))))))

(ert-deftest claude-repl-cmd-test-interrupt/noop-when-vterm-not-live ()
  "interrupt is a no-op when vterm is not live."
  (let (escape-called)
    (cl-letf (((symbol-function 'claude-repl--vterm-live-p)
               (lambda () nil))
              ((symbol-function 'claude-repl--send-interrupt-escape)
               (lambda (_ws _buf) (setq escape-called t))))
      (claude-repl-interrupt)
      (should-not escape-called))))

;;;; ---- claude-repl-update-pr ----

(ert-deftest claude-repl-cmd-test-update-pr/sends-prompt ()
  "update-pr sends the configured update-pr prompt to claude."
  (let (sent-text)
    (cl-letf (((symbol-function 'claude-repl--send-to-claude)
               (lambda (text) (setq sent-text text))))
      (claude-repl-update-pr)
      (should (equal sent-text claude-repl-update-pr-prompt)))))

;;;; ---- claude-repl-copy-reference ----

(ert-deftest claude-repl-cmd-test-copy-reference/copies-to-kill-ring ()
  "copy-reference puts file:line reference on kill ring."
  (cl-letf (((symbol-function 'claude-repl--format-file-ref)
             (lambda () "src/foo.el:42")))
    (claude-repl-copy-reference)
    (should (equal (car kill-ring) "src/foo.el:42"))))

;;;; ---- claude-repl--diff-command-form (macro expansion) ----

(ert-deftest claude-repl-cmd-test-diff-commands/explain-diff-worktree-exists ()
  "Macro generates claude-repl-explain-diff-worktree."
  (should (fboundp 'claude-repl-explain-diff-worktree)))

(ert-deftest claude-repl-cmd-test-diff-commands/explain-diff-branch-exists ()
  "Macro generates claude-repl-explain-diff-branch."
  (should (fboundp 'claude-repl-explain-diff-branch)))

(ert-deftest claude-repl-cmd-test-diff-commands/all-scopes-generated ()
  "Macro generates all 5 scope commands for each family."
  (dolist (family '("explain-diff" "update-pr-diff" "run-tests"
                    "run-lint" "run-all" "test-quality" "test-coverage"))
    (dolist (scope '("worktree" "staged" "uncommitted" "head" "branch"))
      (let ((fn (intern (format "claude-repl-%s-%s" family scope))))
        (should (fboundp fn))))))

(ert-deftest claude-repl-cmd-test-diff-commands/explain-diff-worktree-sends ()
  "explain-diff-worktree sends correct message."
  (let (sent-spec sent-prompt)
    (cl-letf (((symbol-function 'claude-repl--send-diff-analysis)
               (lambda (spec prompt) (setq sent-spec spec sent-prompt prompt))))
      (claude-repl-explain-diff-worktree)
      (should (equal sent-spec "unstaged changes (git diff)"))
      (should (equal sent-prompt claude-repl-explain-diff-prompt)))))

(ert-deftest claude-repl-cmd-test-diff-commands/update-pr-diff-uses-override ()
  "update-pr-diff-worktree uses scope override instead of default."
  (let (sent-spec)
    (cl-letf (((symbol-function 'claude-repl--send-diff-analysis)
               (lambda (spec _prompt) (setq sent-spec spec))))
      (claude-repl-update-pr-diff-worktree)
      ;; Should use the override from claude-repl--update-pr-diff-scopes
      (should (string-match-p "UNSTAGED" sent-spec)))))

(ert-deftest claude-repl-cmd-test-diff-commands/branch-uses-custom-var ()
  "explain-diff-branch uses the claude-repl-branch-diff-spec custom variable."
  (let (sent-spec
        (claude-repl-branch-diff-spec "custom branch spec"))
    (cl-letf (((symbol-function 'claude-repl--send-diff-analysis)
               (lambda (spec _prompt) (setq sent-spec spec))))
      (claude-repl-explain-diff-branch)
      (should (equal sent-spec "custom branch spec")))))

;;;; ---- Customization defaults ----

(ert-deftest claude-repl-cmd-test-customization-defaults ()
  "All custom prompt variables are non-empty strings."
  (dolist (var '(claude-repl-branch-diff-spec
                 claude-repl-explain-diff-prompt
                 claude-repl-update-pr-diff-prompt
                 claude-repl-update-pr-prompt
                 claude-repl-run-tests-prompt
                 claude-repl-run-lint-prompt
                 claude-repl-run-all-prompt
                 claude-repl-test-quality-prompt
                 claude-repl-test-coverage-prompt))
    (should (stringp (symbol-value var)))
    (should (> (length (symbol-value var)) 0))))

;;;; ---- claude-repl-nuke-workspace ----

(ert-deftest claude-repl-cmd-test-nuke-workspace/no-workspaces ()
  "nuke-workspace signals user-error when hashmap is empty."
  (claude-repl-test--with-clean-state
    (should-error (claude-repl-nuke-workspace) :type 'user-error)))

(ert-deftest claude-repl-cmd-test-nuke-workspace/kills-session-and-removes-hashmap ()
  "nuke-workspace kills session, kills persp workspace, and removes hashmap entry."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "doomed" :project-dir "/tmp/doomed")
    (claude-repl--ws-put "doomed" :status :done)
    (let ((session-killed nil)
          (persp-killed nil)
          (persp-mode t))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt _coll &rest _) "doomed"))
                ((symbol-function 'y-or-n-p) (lambda (_prompt) t))
                ((symbol-function 'claude-repl--kill-session)
                 (lambda (ws) (setq session-killed ws)))
                ((symbol-function '+workspace-current-name) (lambda () "other-ws"))
                ((symbol-function 'persp-get-by-name) (lambda (_n) t))
                ((symbol-function '+workspace/kill)
                 (lambda (ws) (setq persp-killed ws)))
                ((symbol-function 'force-mode-line-update) #'ignore))
        (claude-repl-nuke-workspace)
        (should (equal session-killed "doomed"))
        (should (equal persp-killed "doomed"))
        (should-not (gethash "doomed" claude-repl--workspaces))))))

(ert-deftest claude-repl-cmd-test-nuke-workspace/aborts-on-deny ()
  "nuke-workspace does nothing when user answers no."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "doomed" :project-dir "/tmp/doomed")
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt _coll &rest _) "doomed"))
              ((symbol-function 'y-or-n-p) (lambda (_prompt) nil)))
      (should-error (claude-repl-nuke-workspace) :type 'user-error)
      (should (gethash "doomed" claude-repl--workspaces)))))

(ert-deftest claude-repl-cmd-test-nuke-workspace/kills-git-proc ()
  "nuke-workspace kills an in-flight git-diff process."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "doomed" :project-dir "/tmp/doomed")
    (let ((proc-deleted nil)
          (fake-proc (start-process "fake" nil "true")))
      (claude-repl--ws-put "doomed" :git-proc fake-proc)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt _coll &rest _) "doomed"))
                ((symbol-function 'y-or-n-p) (lambda (_prompt) t))
                ((symbol-function 'claude-repl--kill-session) #'ignore)
                ((symbol-function '+workspace-current-name) (lambda () "other"))
                ((symbol-function 'persp-get-by-name) (lambda (_n) nil))
                ((symbol-function 'force-mode-line-update) #'ignore)
                ;; Stub `process-live-p' since "true" exits before the test
                ;; reaches the kill check, racing us to the assertion.
                ((symbol-function 'process-live-p) (lambda (_p) t))
                ((symbol-function 'delete-process)
                 (lambda (p) (setq proc-deleted p))))
        (claude-repl-nuke-workspace)
        (should proc-deleted)))))

(ert-deftest claude-repl-cmd-test-nuke-workspace/kills-persp-workspace ()
  "nuke-workspace calls +workspace/kill to tear down the persp workspace."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "doomed" :project-dir "/tmp/doomed")
    (let ((killed-ws nil)
          (persp-mode t))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt _coll &rest _) "doomed"))
                ((symbol-function 'y-or-n-p) (lambda (_prompt) t))
                ((symbol-function 'claude-repl--kill-session) #'ignore)
                ((symbol-function '+workspace-current-name) (lambda () "other"))
                ((symbol-function 'persp-get-by-name) (lambda (_n) t))
                ((symbol-function '+workspace/kill)
                 (lambda (ws) (setq killed-ws ws)))
                ((symbol-function 'force-mode-line-update) #'ignore))
        (claude-repl-nuke-workspace)
        (should (equal killed-ws "doomed"))))))

(ert-deftest claude-repl-cmd-test-nuke-workspace/no-persp-still-cleans-hashmap ()
  "nuke-workspace removes hashmap entry even when persp workspace doesn't exist."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ghost" :project-dir "/tmp/ghost")
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt _coll &rest _) "ghost"))
              ((symbol-function 'y-or-n-p) (lambda (_prompt) t))
              ((symbol-function 'claude-repl--kill-session) #'ignore)
              ((symbol-function 'persp-get-by-name) (lambda (_n) nil))
              ((symbol-function 'force-mode-line-update) #'ignore))
      (claude-repl-nuke-workspace)
      (should-not (gethash "ghost" claude-repl--workspaces)))))

(ert-deftest claude-repl-cmd-test-nuke-workspace/removes-hashmap-when-kill-session-errors ()
  "nuke-workspace still removes the hashmap entry when kill-session errors."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "doomed" :project-dir "/tmp/doomed")
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt _coll &rest _) "doomed"))
              ((symbol-function 'y-or-n-p) (lambda (_prompt) t))
              ((symbol-function 'claude-repl--kill-session)
               (lambda (_ws) (error "simulated kill-session failure")))
              ((symbol-function 'persp-get-by-name) (lambda (_n) nil))
              ((symbol-function 'force-mode-line-update) #'ignore))
      (claude-repl-nuke-workspace)
      (should-not (gethash "doomed" claude-repl--workspaces)))))

(ert-deftest claude-repl-cmd-test-nuke-workspace/removes-hashmap-when-workspace-kill-errors ()
  "nuke-workspace still removes the hashmap entry when +workspace/kill errors."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "doomed" :project-dir "/tmp/doomed")
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt _coll &rest _) "doomed"))
              ((symbol-function 'y-or-n-p) (lambda (_prompt) t))
              ((symbol-function 'claude-repl--kill-session) #'ignore)
              ((symbol-function '+workspace-current-name) (lambda () "other"))
              ((symbol-function 'persp-get-by-name) (lambda (_n) t))
              ((symbol-function '+workspace/kill)
               (lambda (_ws) (error "simulated workspace-kill failure")))
              ((symbol-function 'force-mode-line-update) #'ignore))
      (claude-repl-nuke-workspace)
      (should-not (gethash "doomed" claude-repl--workspaces)))))

(ert-deftest claude-repl-cmd-test-nuke-workspace/purges-state-file ()
  "nuke-workspace unlinks the .claude-repl-state file at the project root.
Without this, a subsequent workspace registered at the same root would
inherit the stale session-id via `state-restore' and launch Claude
with `--resume <stale-sid>' — the exact failure the purge closes."
  (claude-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "claude-nuke-" t)))
      (unwind-protect
          (let ((state-file (expand-file-name ".claude-repl-state" tmpdir)))
            (with-temp-file state-file (insert "(:session-id \"stale-abc\")"))
            (claude-repl--ws-put "doomed" :project-dir tmpdir)
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (_prompt _coll &rest _) "doomed"))
                      ((symbol-function 'y-or-n-p) (lambda (_prompt) t))
                      ((symbol-function 'claude-repl--kill-session) #'ignore)
                      ((symbol-function 'persp-get-by-name) (lambda (_n) nil))
                      ((symbol-function 'force-mode-line-update) #'ignore))
              (claude-repl-nuke-workspace)
              (should-not (file-exists-p state-file))))
        (delete-directory tmpdir t)))))

(ert-deftest claude-repl-cmd-test-nuke-workspace/kills-workspace-buffers ()
  "nuke-workspace invokes kill-workspace-buffers so every persp buffer is torn down."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "doomed" :project-dir "/tmp/doomed")
    (let ((kwb-arg nil))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt _coll &rest _) "doomed"))
                ((symbol-function 'y-or-n-p) (lambda (_prompt) t))
                ((symbol-function 'claude-repl--kill-session) #'ignore)
                ((symbol-function 'claude-repl--kill-workspace-buffers)
                 (lambda (ws) (setq kwb-arg ws)))
                ((symbol-function 'persp-get-by-name) (lambda (_n) nil))
                ((symbol-function 'force-mode-line-update) #'ignore))
        (claude-repl-nuke-workspace)
        (should (equal kwb-arg "doomed"))))))

(ert-deftest claude-repl-cmd-test-nuke-workspace/kills-buffers-even-when-kill-session-errors ()
  "nuke-workspace still sweeps persp buffers when kill-session throws.
kill-workspace-buffers lives in the `unwind-protect' cleanup so the
buffer sweep is not skipped by an earlier teardown failure."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "doomed" :project-dir "/tmp/doomed")
    (let ((kwb-called nil))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt _coll &rest _) "doomed"))
                ((symbol-function 'y-or-n-p) (lambda (_prompt) t))
                ((symbol-function 'claude-repl--kill-session)
                 (lambda (_ws) (error "simulated kill-session failure")))
                ((symbol-function 'claude-repl--kill-workspace-buffers)
                 (lambda (_ws) (setq kwb-called t)))
                ((symbol-function 'persp-get-by-name) (lambda (_n) nil))
                ((symbol-function 'force-mode-line-update) #'ignore))
        (claude-repl-nuke-workspace)
        (should kwb-called)))))

(ert-deftest claude-repl-cmd-test-nuke-workspace/workspace-kill-runs-after-buffer-sweep ()
  "nuke-workspace kills the persp buffers BEFORE tearing down the persp itself.
Reversing the order would make the buffer sweep a no-op because
`persp-get-by-name' returns nil once the persp is gone."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "doomed" :project-dir "/tmp/doomed")
    (let ((call-order nil)
          (persp-mode t))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt _coll &rest _) "doomed"))
                ((symbol-function 'y-or-n-p) (lambda (_prompt) t))
                ((symbol-function 'claude-repl--kill-session) #'ignore)
                ((symbol-function 'claude-repl--kill-workspace-buffers)
                 (lambda (_ws) (push 'kwb call-order)))
                ((symbol-function 'persp-get-by-name) (lambda (_n) t))
                ((symbol-function '+workspace/kill)
                 (lambda (_ws) (push 'persp-kill call-order)))
                ((symbol-function 'force-mode-line-update) #'ignore))
        (claude-repl-nuke-workspace)
        (should (equal (nreverse call-order) '(kwb persp-kill)))))))

;;;; ---- claude-repl-nuke-all-workspaces ----

(ert-deftest claude-repl-cmd-test-nuke-all/no-workspaces ()
  "nuke-all-workspaces signals user-error when hashmap is empty."
  (claude-repl-test--with-clean-state
    (should-error (claude-repl-nuke-all-workspaces) :type 'user-error)))

(ert-deftest claude-repl-cmd-test-nuke-all/aborts-on-deny ()
  "nuke-all-workspaces does nothing when user answers no."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :project-dir "/tmp/ws1")
    (claude-repl--ws-put "ws2" :project-dir "/tmp/ws2")
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) nil)))
      (should-error (claude-repl-nuke-all-workspaces) :type 'user-error)
      (should (gethash "ws1" claude-repl--workspaces))
      (should (gethash "ws2" claude-repl--workspaces)))))

(ert-deftest claude-repl-cmd-test-nuke-all/iterates-every-workspace ()
  "nuke-all-workspaces tears down every registered workspace."
  (claude-repl-test--with-clean-state
    (dolist (n '("ws1" "ws2" "ws3"))
      (claude-repl--ws-put n :project-dir (format "/tmp/%s" n)))
    (let ((torn-down nil)
          (persp-mode nil))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) t))
                ((symbol-function 'claude-repl--kill-session)
                 (lambda (ws) (push ws torn-down)))
                ((symbol-function 'force-mode-line-update) #'ignore))
        (claude-repl-nuke-all-workspaces)
        (should (= 3 (length torn-down)))
        (should (member "ws1" torn-down))
        (should (member "ws2" torn-down))
        (should (member "ws3" torn-down))
        (should (zerop (hash-table-count claude-repl--workspaces)))))))

(ert-deftest claude-repl-cmd-test-nuke-all/prompt-includes-count ()
  "nuke-all-workspaces' confirmation prompt includes the workspace count."
  (claude-repl-test--with-clean-state
    (dolist (n '("a" "b"))
      (claude-repl--ws-put n :project-dir (format "/tmp/%s" n)))
    (let ((seen-prompt nil))
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (prompt) (setq seen-prompt prompt) nil)))
        (ignore-errors (claude-repl-nuke-all-workspaces))
        (should (string-match-p "ALL 2" seen-prompt))))))

;;;; ---- Tests: workspace snapshot save/load ----

(ert-deftest claude-repl-cmd-test-save-workspace-snapshot/writes-entries ()
  "save-workspace-snapshot writes a (NAME . PROJECT-DIR) alist to the file."
  (claude-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "claude-snap-")))
      (unwind-protect
          (let ((claude-repl-workspace-snapshot-file snapshot-file))
            (claude-repl--ws-put "ws1" :project-dir "/tmp/ws1")
            (claude-repl--ws-put "ws2" :project-dir "/tmp/ws2")
            (claude-repl-save-workspace-snapshot)
            (let ((data (claude-repl--read-sexp-file snapshot-file)))
              (should (= 2 (length data)))
              (should (equal (cdr (assoc "ws1" data)) "/tmp/ws1"))
              (should (equal (cdr (assoc "ws2" data)) "/tmp/ws2"))))
        (delete-file snapshot-file)))))

(ert-deftest claude-repl-cmd-test-save-workspace-snapshot/skips-missing-project-dir ()
  "save-workspace-snapshot omits workspaces with no :project-dir."
  (claude-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "claude-snap-")))
      (unwind-protect
          (let ((claude-repl-workspace-snapshot-file snapshot-file))
            (claude-repl--ws-put "ws1" :project-dir "/tmp/ws1")
            (claude-repl--ws-put "ws2" :vterm-buffer nil)
            (claude-repl-save-workspace-snapshot)
            (let ((data (claude-repl--read-sexp-file snapshot-file)))
              (should (= 1 (length data)))
              (should (equal (cdr (assoc "ws1" data)) "/tmp/ws1"))
              (should-not (assoc "ws2" data))))
        (delete-file snapshot-file)))))

(ert-deftest claude-repl-cmd-test-load-workspace-snapshot/errors-when-file-missing ()
  "load-workspace-snapshot signals user-error when the snapshot file is absent."
  (claude-repl-test--with-clean-state
    (let ((claude-repl-workspace-snapshot-file "/nonexistent/claude-snap.el"))
      (should-error (claude-repl-load-workspace-snapshot) :type 'user-error))))

(ert-deftest claude-repl-cmd-test-load-workspace-snapshot/calls-switch-for-each-entry ()
  "load-workspace-snapshot invokes +dwc/switch-to-project once per existing entry."
  (claude-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "claude-snap-"))
          (dir-a (make-temp-file "claude-proj-a-" t))
          (dir-b (make-temp-file "claude-proj-b-" t))
          (switched nil))
      (unwind-protect
          (let ((claude-repl-workspace-snapshot-file snapshot-file))
            (claude-repl--write-sexp-file snapshot-file
                                          `(("ws-a" . ,dir-a)
                                            ("ws-b" . ,dir-b)))
            (cl-letf (((symbol-function '+dwc/switch-to-project)
                       (lambda (project) (push project switched))))
              (claude-repl-load-workspace-snapshot)
              (should (= 2 (length switched)))
              (should (member dir-a switched))
              (should (member dir-b switched))))
        (delete-file snapshot-file)
        (delete-directory dir-a t)
        (delete-directory dir-b t)))))

(ert-deftest claude-repl-cmd-test-load-workspace-snapshot/skips-missing-dirs ()
  "load-workspace-snapshot does not switch to entries whose directory is gone."
  (claude-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "claude-snap-"))
          (real-dir (make-temp-file "claude-proj-real-" t))
          (switched nil))
      (unwind-protect
          (let ((claude-repl-workspace-snapshot-file snapshot-file))
            (claude-repl--write-sexp-file snapshot-file
                                          `(("ws-real" . ,real-dir)
                                            ("ws-gone" . "/nonexistent/path")))
            (cl-letf (((symbol-function '+dwc/switch-to-project)
                       (lambda (project) (push project switched))))
              (claude-repl-load-workspace-snapshot)
              (should (equal switched (list real-dir)))))
        (delete-file snapshot-file)
        (delete-directory real-dir t)))))

(provide 'test-commands)

;;; test-commands.el ends here
