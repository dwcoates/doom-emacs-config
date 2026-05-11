;;; test-commands.el --- ERT tests for claude-repl commands -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for commands.el — user commands, file references, diff analysis,
;; and standalone interactive commands.
;;
;; Run with:
;;   emacs -batch -Q -l ert -l test-commands.el -f ert-run-tests-batch-and-exit

;;; Code:

(defvar recentf-list)

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

;;;; ---- claude-repl--exclusion-symbol-to-flag ----

(ert-deftest claude-repl-cmd-test-exclusion-symbol-to-flag/single-word ()
  "no-self-certified maps to --self-certified."
  (should (equal (claude-repl--exclusion-symbol-to-flag 'no-self-certified)
                 "--self-certified")))

(ert-deftest claude-repl-cmd-test-exclusion-symbol-to-flag/multi-word ()
  "Dashes inside the flag name are preserved."
  (should (equal (claude-repl--exclusion-symbol-to-flag 'no-add-to-merge-queue)
                 "--add-to-merge-queue")))

(ert-deftest claude-repl-cmd-test-exclusion-symbol-to-flag/missing-prefix-errors ()
  "Symbol without the `no-' prefix raises an error."
  (should-error (claude-repl--exclusion-symbol-to-flag 'self-certified)))

;;;; ---- claude-repl--build-create-or-update-pr-prompt ----

(ert-deftest claude-repl-cmd-test-build-coup-prompt/no-exclusions-keeps-all-flags ()
  "With nil EXCLUDED, the prompt contains every base flag."
  (let ((claude-repl-create-or-update-pr-base-flags
         '("--patch" "--self-certified" "--add-to-merge-queue" "--skip-tests")))
    (should (equal (claude-repl--build-create-or-update-pr-prompt nil)
                   "/create-or-update-pr --patch --self-certified --add-to-merge-queue --skip-tests"))))

(ert-deftest claude-repl-cmd-test-build-coup-prompt/single-exclusion ()
  "A single exclusion drops just that flag."
  (let ((claude-repl-create-or-update-pr-base-flags
         '("--patch" "--self-certified" "--add-to-merge-queue" "--skip-tests")))
    (should (equal (claude-repl--build-create-or-update-pr-prompt '(no-self-certified))
                   "/create-or-update-pr --patch --add-to-merge-queue --skip-tests"))))

(ert-deftest claude-repl-cmd-test-build-coup-prompt/multiple-exclusions ()
  "Multiple exclusions drop each named flag."
  (let ((claude-repl-create-or-update-pr-base-flags
         '("--patch" "--self-certified" "--add-to-merge-queue" "--skip-tests")))
    (should (equal (claude-repl--build-create-or-update-pr-prompt
                    '(no-self-certified no-add-to-merge-queue))
                   "/create-or-update-pr --patch --skip-tests"))))

(ert-deftest claude-repl-cmd-test-build-coup-prompt/unknown-exclusion-errors ()
  "Excluding a flag not present in the base list signals an error."
  (let ((claude-repl-create-or-update-pr-base-flags '("--patch" "--self-certified")))
    (should-error (claude-repl--build-create-or-update-pr-prompt '(no-add-to-merge-queue)))))

;;;; ---- claude-repl-create-or-update-pr ----

(ert-deftest claude-repl-cmd-test-create-or-update-pr/no-args-sends-default ()
  "create-or-update-pr with no args sends the prompt built from base flags."
  (let (sent-text)
    (cl-letf (((symbol-function 'claude-repl--send-to-claude)
               (lambda (text) (setq sent-text text))))
      (claude-repl-create-or-update-pr)
      (should (equal sent-text
                     (claude-repl--build-create-or-update-pr-prompt nil))))))

(ert-deftest claude-repl-cmd-test-create-or-update-pr/excluded-arg-omits-flag ()
  "create-or-update-pr called with EXCLUDED list drops those flags."
  (let (sent-text)
    (cl-letf (((symbol-function 'claude-repl--send-to-claude)
               (lambda (text) (setq sent-text text))))
      (claude-repl-create-or-update-pr '(no-self-certified))
      (should-not (string-match-p "--self-certified" sent-text)))))

(ert-deftest claude-repl-cmd-test-create-or-update-pr/prefixes-input-buffer-contents ()
  "Non-empty input buffer contents are prepended to the prompt."
  (claude-repl-test--with-clean-state
    (let (sent-text)
      (claude-repl-test--with-temp-buffer " *test-coup-input*"
        (insert "do a thing")
        (claude-repl--ws-put "test-ws" :input-buffer (current-buffer))
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                  ((symbol-function 'claude-repl--send-to-claude)
                   (lambda (text) (setq sent-text text)))
                  ((symbol-function 'claude-repl--commit-input-buffer)
                   (lambda (&rest _) nil)))
          (claude-repl-create-or-update-pr)
          (should (equal sent-text
                         (concat "do a thing "
                                 (claude-repl--build-create-or-update-pr-prompt nil)))))))))

(ert-deftest claude-repl-cmd-test-create-or-update-pr/empty-buffer-sends-bare-prompt ()
  "Empty input buffer leaves the base prompt unprefixed (no leading space)."
  (claude-repl-test--with-clean-state
    (let (sent-text)
      (claude-repl-test--with-temp-buffer " *test-coup-input*"
        (claude-repl--ws-put "test-ws" :input-buffer (current-buffer))
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                  ((symbol-function 'claude-repl--send-to-claude)
                   (lambda (text) (setq sent-text text))))
          (claude-repl-create-or-update-pr)
          (should (equal sent-text
                         (claude-repl--build-create-or-update-pr-prompt nil))))))))

(ert-deftest claude-repl-cmd-test-create-or-update-pr/trims-trailing-whitespace-on-prefix ()
  "Trailing whitespace/newlines in the input buffer are trimmed before joining."
  (claude-repl-test--with-clean-state
    (let (sent-text)
      (claude-repl-test--with-temp-buffer " *test-coup-input*"
        (insert "do a thing  \n")
        (claude-repl--ws-put "test-ws" :input-buffer (current-buffer))
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                  ((symbol-function 'claude-repl--send-to-claude)
                   (lambda (text) (setq sent-text text)))
                  ((symbol-function 'claude-repl--commit-input-buffer)
                   (lambda (&rest _) nil)))
          (claude-repl-create-or-update-pr)
          (should (equal sent-text
                         (concat "do a thing "
                                 (claude-repl--build-create-or-update-pr-prompt nil)))))))))

(ert-deftest claude-repl-cmd-test-create-or-update-pr/whitespace-only-buffer-treated-as-empty ()
  "An input buffer of only whitespace is treated as empty (no prefix, no commit)."
  (claude-repl-test--with-clean-state
    (let (sent-text commit-called)
      (claude-repl-test--with-temp-buffer " *test-coup-input*"
        (insert "   \n  ")
        (claude-repl--ws-put "test-ws" :input-buffer (current-buffer))
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                  ((symbol-function 'claude-repl--send-to-claude)
                   (lambda (text) (setq sent-text text)))
                  ((symbol-function 'claude-repl--commit-input-buffer)
                   (lambda (&rest _) (setq commit-called t))))
          (claude-repl-create-or-update-pr)
          (should (equal sent-text
                         (claude-repl--build-create-or-update-pr-prompt nil)))
          (should-not commit-called))))))

(ert-deftest claude-repl-cmd-test-create-or-update-pr/commits-input-buffer-when-prefixed ()
  "When a prefix was used, the input buffer is committed (history + clear)."
  (claude-repl-test--with-clean-state
    (let (commit-args)
      (claude-repl-test--with-temp-buffer " *test-coup-input*"
        (insert "ship it")
        (claude-repl--ws-put "test-ws" :input-buffer (current-buffer))
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                  ((symbol-function 'claude-repl--send-to-claude) (lambda (_) nil))
                  ((symbol-function 'claude-repl--commit-input-buffer)
                   (lambda (ws buf raw clear-p)
                     (setq commit-args (list ws buf raw clear-p)))))
          (claude-repl-create-or-update-pr)
          (should commit-args)
          (should (equal (nth 0 commit-args) "test-ws"))
          (should (eq (nth 3 commit-args) t)))))))

(ert-deftest claude-repl-cmd-test-create-or-update-pr/excluded-with-prefix-still-drops-flag ()
  "EXCLUDED flags are dropped even when an input buffer prefix is present."
  (claude-repl-test--with-clean-state
    (let (sent-text)
      (claude-repl-test--with-temp-buffer " *test-coup-input*"
        (insert "do a thing")
        (claude-repl--ws-put "test-ws" :input-buffer (current-buffer))
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                  ((symbol-function 'claude-repl--send-to-claude)
                   (lambda (text) (setq sent-text text)))
                  ((symbol-function 'claude-repl--commit-input-buffer)
                   (lambda (&rest _) nil)))
          (claude-repl-create-or-update-pr '(no-self-certified))
          (should (string-prefix-p "do a thing " sent-text))
          (should-not (string-match-p "--self-certified" sent-text)))))))

;;;; ---- claude-repl-create-or-update-pr-no-self-certified ----

(ert-deftest claude-repl-cmd-test-create-or-update-pr-no-self-certified/sends-prompt ()
  "no-self-certified wrapper sends a prompt that omits --self-certified."
  (let (sent-text)
    (cl-letf (((symbol-function 'claude-repl--send-to-claude)
               (lambda (text) (setq sent-text text))))
      (claude-repl-create-or-update-pr-no-self-certified)
      (should-not (string-match-p "--self-certified" sent-text)))))

;;;; ---- claude-repl-copy-reference ----

(ert-deftest claude-repl-cmd-test-copy-reference/copies-to-kill-ring ()
  "copy-reference puts file:line reference on kill ring."
  (cl-letf (((symbol-function 'claude-repl--format-file-ref)
             (lambda () "src/foo.el:42")))
    (claude-repl-copy-reference)
    (should (equal (car kill-ring) "src/foo.el:42"))))

;;;; ---- claude-repl-paste-clipboard ----

(ert-deftest claude-repl-cmd-test-paste-clipboard/inserts-at-point ()
  "paste-clipboard inserts the workspace's `:clipboard' text at point."
  (let ((claude-repl--workspaces (make-hash-table :test 'equal)))
    (puthash "ws1" (list :clipboard "hello world") claude-repl--workspaces)
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1")))
      (with-temp-buffer
        (claude-repl-paste-clipboard)
        (should (equal (buffer-string) "hello world"))))))

(ert-deftest claude-repl-cmd-test-paste-clipboard/errors-when-unset ()
  "paste-clipboard signals user-error when no clipboard text is set."
  (let ((claude-repl--workspaces (make-hash-table :test 'equal)))
    (puthash "ws1" '() claude-repl--workspaces)
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1")))
      (with-temp-buffer
        (should-error (claude-repl-paste-clipboard) :type 'user-error)))))

(ert-deftest claude-repl-cmd-test-paste-clipboard/does-not-touch-os-clipboard ()
  "paste-clipboard inserts only at point — kill-ring is left untouched."
  (let ((claude-repl--workspaces (make-hash-table :test 'equal))
        (kill-ring '("pre-existing")))
    (puthash "ws1" (list :clipboard "ws-text") claude-repl--workspaces)
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1")))
      (with-temp-buffer
        (claude-repl-paste-clipboard)
        (should (equal (car kill-ring) "pre-existing"))))))

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

(ert-deftest claude-repl-cmd-test-base-flags-default ()
  "Default base flags are a non-empty list of strings."
  (should (listp claude-repl-create-or-update-pr-base-flags))
  (should (> (length claude-repl-create-or-update-pr-base-flags) 0))
  (should (cl-every #'stringp claude-repl-create-or-update-pr-base-flags)))

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

(ert-deftest claude-repl-cmd-test-nuke-workspace/preserves-state-file ()
  "nuke-workspace MUST preserve the per-project state.el so the
captured session-id survives the in-memory teardown.  The next time
the same project is opened, `--initialize-ws-env' reads this file and
launches Claude with `--continue', resuming the prior session.  A
nuke that wipes state.el would force a fresh session each time, which
is the regression this test pins."
  (claude-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "claude-nuke-" t)))
      (unwind-protect
          (let ((state-file (claude-repl--state-file tmpdir)))
            (claude-repl-test--seed-file state-file "(:session-id \"keep-abc\")")
            (claude-repl--ws-put "doomed" :project-dir tmpdir)
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (_prompt _coll &rest _) "doomed"))
                      ((symbol-function 'y-or-n-p) (lambda (_prompt) t))
                      ;; Stub kill-session so its embedded state-save can't
                      ;; rewrite the file with empty session-id — we want
                      ;; to verify the up-front state-save (or the no-purge
                      ;; guarantee) preserves the seeded contents.
                      ((symbol-function 'claude-repl--kill-session) #'ignore)
                      ;; Stub state-save too so the seeded content is what
                      ;; the test asserts on; this isolates the "no purge"
                      ;; property from the orthogonal "save before tear
                      ;; down" property tested separately below.
                      ((symbol-function 'claude-repl--state-save) #'ignore)
                      ((symbol-function 'persp-get-by-name) (lambda (_n) nil))
                      ((symbol-function 'force-mode-line-update) #'ignore))
              (claude-repl-nuke-workspace)
              (should (file-exists-p state-file))))
        (delete-directory tmpdir t)))))

(ert-deftest claude-repl-cmd-test-nuke-workspace/saves-state-before-teardown ()
  "nuke-workspace runs `--state-save' BEFORE any teardown so session-id
is persisted even if a downstream step (kill-session, ws-del, persp
kill) signals.  Order assertion: state-save called at least once
before kill-session."
  (claude-repl-test--with-clean-state
    (let ((events nil))
      (claude-repl--ws-put "doomed" :project-dir "/tmp/whatever")
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt _coll &rest _) "doomed"))
                ((symbol-function 'y-or-n-p) (lambda (_prompt) t))
                ((symbol-function 'claude-repl--state-save)
                 (lambda (_ws) (push 'state-save events)))
                ((symbol-function 'claude-repl--kill-session)
                 (lambda (_ws) (push 'kill-session events)))
                ((symbol-function 'persp-get-by-name) (lambda (_n) nil))
                ((symbol-function 'force-mode-line-update) #'ignore))
        (claude-repl-nuke-workspace)
        ;; Reverse so events are in chronological order.
        (let ((ordered (reverse events)))
          (should (memq 'state-save ordered))
          (should (memq 'kill-session ordered))
          ;; state-save must precede kill-session.
          (should (< (cl-position 'state-save ordered)
                     (cl-position 'kill-session ordered))))))))

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

;;;; ---- claude-repl-nuke-restored-workspaces ----

(ert-deftest claude-repl-cmd-test-nuke-restored/no-restored ()
  "nuke-restored-workspaces errors when the restored set is empty."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :project-dir "/tmp/ws1")
    (should-error (claude-repl-nuke-restored-workspaces) :type 'user-error)))

(ert-deftest claude-repl-cmd-test-nuke-restored/aborts-on-deny ()
  "nuke-restored-workspaces does nothing when user answers no."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :project-dir "/tmp/ws1")
    (push "ws1" claude-repl--restored-workspaces)
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) nil)))
      (should-error (claude-repl-nuke-restored-workspaces) :type 'user-error)
      (should (claude-repl--ws-get "ws1" :project-dir)))))

(ert-deftest claude-repl-cmd-test-nuke-restored/only-restored-are-torn-down ()
  "nuke-restored-workspaces tears down only restored workspaces, sparing manual ones."
  (claude-repl-test--with-clean-state
    (dolist (n '("restored1" "restored2" "manual"))
      (claude-repl--ws-put n :project-dir (format "/tmp/%s" n)))
    (setq claude-repl--restored-workspaces '("restored1" "restored2"))
    (let ((torn-down nil)
          (persp-mode nil))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) t))
                ((symbol-function 'claude-repl--kill-session)
                 (lambda (ws) (push ws torn-down)))
                ((symbol-function 'force-mode-line-update) #'ignore))
        (claude-repl-nuke-restored-workspaces)
        (should (= 2 (length torn-down)))
        (should (member "restored1" torn-down))
        (should (member "restored2" torn-down))
        (should-not (member "manual" torn-down))
        (should (claude-repl--ws-get "manual" :project-dir))
        (should-not (claude-repl--ws-get "restored1" :project-dir))
        (should-not (claude-repl--ws-get "restored2" :project-dir))))))

(ert-deftest claude-repl-cmd-test-nuke-restored/prompt-includes-count ()
  "nuke-restored-workspaces' confirmation prompt includes the restored count."
  (claude-repl-test--with-clean-state
    (dolist (n '("a" "b" "c"))
      (claude-repl--ws-put n :project-dir (format "/tmp/%s" n)))
    (setq claude-repl--restored-workspaces '("a" "b" "c"))
    (let ((seen-prompt nil))
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (prompt) (setq seen-prompt prompt) nil)))
        (ignore-errors (claude-repl-nuke-restored-workspaces))
        (should (string-match-p "3 restored" seen-prompt))))))

(ert-deftest claude-repl-cmd-test-nuke-restored/skips-stale-names ()
  "nuke-restored-workspaces ignores names in the restored list with no live ws.
Avoids a user-error on the unprompted path when a name was removed from
the live hash (e.g., by individual nuke) but stayed on the list."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "live" :project-dir "/tmp/live")
    (setq claude-repl--restored-workspaces '("live" "stale"))
    (let ((torn-down nil)
          (persp-mode nil))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) t))
                ((symbol-function 'claude-repl--kill-session)
                 (lambda (ws) (push ws torn-down)))
                ((symbol-function 'force-mode-line-update) #'ignore))
        (claude-repl-nuke-restored-workspaces)
        (should (equal torn-down '("live")))))))

(ert-deftest claude-repl-cmd-test-nuke-one/drops-from-restored-list ()
  "Individual nuke removes the ws from `claude-repl--restored-workspaces'."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :project-dir "/tmp/ws1")
    (claude-repl--ws-put "ws2" :project-dir "/tmp/ws2")
    (setq claude-repl--restored-workspaces '("ws1" "ws2"))
    (let ((persp-mode nil))
      (cl-letf (((symbol-function 'claude-repl--kill-session) #'ignore)
                ((symbol-function 'force-mode-line-update) #'ignore))
        (claude-repl--nuke-one-workspace "ws1")
        (should (equal claude-repl--restored-workspaces '("ws2")))))))

;;;; ---- claude-repl-kill-workspace ----

(ert-deftest claude-repl-cmd-test-kill-workspace/no-workspaces ()
  "kill-workspace signals user-error when hashmap is empty."
  (claude-repl-test--with-clean-state
    (should-error (claude-repl-kill-workspace) :type 'user-error)))

(ert-deftest claude-repl-cmd-test-kill-workspace/aborts-on-deny ()
  "kill-workspace does nothing when user answers no."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "doomed" :project-dir "/tmp/doomed")
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt _coll &rest _) "doomed"))
              ((symbol-function 'y-or-n-p) (lambda (_prompt) nil)))
      (should-error (claude-repl-kill-workspace) :type 'user-error)
      (should (gethash "doomed" claude-repl--workspaces)))))

(ert-deftest claude-repl-cmd-test-kill-workspace/kills-session-and-removes-hashmap ()
  "kill-workspace kills session, kills persp workspace, and removes hashmap entry."
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
        (claude-repl-kill-workspace)
        (should (equal session-killed "doomed"))
        (should (equal persp-killed "doomed"))
        (should-not (gethash "doomed" claude-repl--workspaces))))))

(ert-deftest claude-repl-cmd-test-kill-workspace/preserves-state-file ()
  "kill-workspace must NOT unlink the .claude-repl-state file.
This is the whole point of the kill (vs nuke) split: priority and
per-environment session-id live in that file and need to survive a
kill so the workspace can be re-opened with its identity intact."
  (claude-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "claude-kill-" t)))
      (unwind-protect
          (let ((state-file (claude-repl--state-file tmpdir)))
            (claude-repl-test--seed-file state-file "(:session-id \"keep-me\")")
            (claude-repl--ws-put "doomed" :project-dir tmpdir)
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (_prompt _coll &rest _) "doomed"))
                      ((symbol-function 'y-or-n-p) (lambda (_prompt) t))
                      ((symbol-function 'claude-repl--kill-session) #'ignore)
                      ((symbol-function 'persp-get-by-name) (lambda (_n) nil))
                      ((symbol-function 'force-mode-line-update) #'ignore))
              (claude-repl-kill-workspace)
              (should (file-exists-p state-file))))
        (delete-directory tmpdir t)))))

(ert-deftest claude-repl-cmd-test-kill-workspace/kills-workspace-buffers ()
  "kill-workspace invokes kill-workspace-buffers so every persp buffer is torn down."
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
        (claude-repl-kill-workspace)
        (should (equal kwb-arg "doomed"))))))

(ert-deftest claude-repl-cmd-test-kill-workspace/kills-git-proc ()
  "kill-workspace kills an in-flight git-diff process."
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
                ((symbol-function 'process-live-p) (lambda (_p) t))
                ((symbol-function 'delete-process)
                 (lambda (p) (setq proc-deleted p))))
        (claude-repl-kill-workspace)
        (should proc-deleted)))))

;;;; ---- Tests: workspace snapshot save/load ----

(ert-deftest claude-repl-cmd-test-save-workspace-snapshot/writes-entries ()
  "save-workspace-snapshot writes `(NAME :project-dir DIR)' entries."
  (claude-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "claude-snap-")))
      (unwind-protect
          (let ((claude-repl-workspace-snapshot-file snapshot-file))
            (claude-repl--ws-put "ws1" :project-dir "/tmp/ws1")
            (claude-repl--ws-put "ws2" :project-dir "/tmp/ws2")
            (claude-repl-save-workspace-snapshot)
            (let ((data (claude-repl--read-sexp-file snapshot-file)))
              (should (= 2 (length data)))
              (should (equal (plist-get (cdr (assoc "ws1" data)) :project-dir) "/tmp/ws1"))
              (should (equal (plist-get (cdr (assoc "ws2" data)) :project-dir) "/tmp/ws2"))))
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
              (should (equal (plist-get (cdr (assoc "ws1" data)) :project-dir) "/tmp/ws1"))
              (should-not (assoc "ws2" data))))
        (delete-file snapshot-file)))))

(ert-deftest claude-repl-cmd-test-update-workspace-snapshot/forces-write-bypassing-safe-guard ()
  "update-workspace-snapshot writes the live roster even when the loader
hasn't run and the on-disk roster is larger — `save-workspace-snapshot'
aborts in that case, but the explicit update command bypasses the
safety check after a user confirmation."
  (claude-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "claude-snap-")))
      (unwind-protect
          (let ((claude-repl-workspace-snapshot-file snapshot-file)
                (claude-repl--snapshot-loaded-p nil))
            ;; Seed a richer on-disk roster than the live hash.
            (claude-repl--write-sexp-file snapshot-file
                                          '(("old-a" :project-dir "/tmp/old-a")
                                            ("old-b" :project-dir "/tmp/old-b")
                                            ("old-c" :project-dir "/tmp/old-c")))
            (claude-repl--ws-put "ws1" :project-dir "/tmp/ws1")
            (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) t)))
              (claude-repl-update-workspace-snapshot))
            (let ((data (claude-repl--read-sexp-file snapshot-file)))
              (should (= 1 (length data)))
              (should (equal (plist-get (cdr (assoc "ws1" data)) :project-dir) "/tmp/ws1"))))
        (delete-file snapshot-file)))))

(ert-deftest claude-repl-cmd-test-update-workspace-snapshot/aborts-on-shrink-decline ()
  "update-workspace-snapshot aborts when the user declines the shrink
confirmation, leaving the on-disk file untouched."
  (claude-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "claude-snap-")))
      (unwind-protect
          (let ((claude-repl-workspace-snapshot-file snapshot-file))
            (claude-repl--write-sexp-file snapshot-file
                                          '(("old-a" :project-dir "/tmp/old-a")
                                            ("old-b" :project-dir "/tmp/old-b")))
            (claude-repl--ws-put "ws1" :project-dir "/tmp/ws1")
            (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) nil)))
              (should-error (claude-repl-update-workspace-snapshot)
                            :type 'user-error))
            ;; File contents are unchanged.
            (let ((data (claude-repl--read-sexp-file snapshot-file)))
              (should (= 2 (length data)))))
        (delete-file snapshot-file)))))

(ert-deftest claude-repl-cmd-test-update-workspace-snapshot/no-prompt-when-not-shrinking ()
  "update-workspace-snapshot writes without confirmation when the live
roster is at least as large as the on-disk one."
  (claude-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "claude-snap-"))
          (prompted nil))
      (unwind-protect
          (let ((claude-repl-workspace-snapshot-file snapshot-file))
            (claude-repl--write-sexp-file snapshot-file
                                          '(("old-a" :project-dir "/tmp/old-a")))
            (claude-repl--ws-put "ws1" :project-dir "/tmp/ws1")
            (claude-repl--ws-put "ws2" :project-dir "/tmp/ws2")
            (cl-letf (((symbol-function 'y-or-n-p)
                       (lambda (_prompt) (setq prompted t) t)))
              (claude-repl-update-workspace-snapshot))
            (should-not prompted)
            (let ((data (claude-repl--read-sexp-file snapshot-file)))
              (should (= 2 (length data)))))
        (delete-file snapshot-file)))))

;;;; ---- Tests: clean-frame-foreign-windows ----

(defun claude-repl-test--make-owned-buffer (name ws)
  "Create buffer NAME with `claude-repl--owning-workspace' set to WS."
  (let ((buf (get-buffer-create name)))
    (with-current-buffer buf
      (setq-local claude-repl--owning-workspace ws))
    buf))

(ert-deftest claude-repl-cmd-test-clean-frame-foreign-windows/native-claude-buffer-kept ()
  "Window showing a buffer owned by WS is kept (native-claude case)."
  (claude-repl-test--with-clean-state
    (let ((native (claude-repl-test--make-owned-buffer "*ws-native*" "ws-a"))
          (extra-win nil))
      (unwind-protect
          (progn
            (switch-to-buffer native)
            (setq extra-win (split-window))
            (set-window-buffer extra-win native)
            (claude-repl--clean-frame-foreign-windows "ws-a")
            (should (= 2 (length (window-list nil 'nomini))))
            (dolist (win (window-list nil 'nomini))
              (should (eq (window-buffer win) native))))
        (when (and extra-win (window-live-p extra-win))
          (ignore-errors (delete-window extra-win)))
        (when (buffer-live-p native) (kill-buffer native))))))

(ert-deftest claude-repl-cmd-test-clean-frame-foreign-windows/foreign-claude-buffer-detected ()
  "Window showing a buffer owned by a different workspace is scrubbed."
  (claude-repl-test--with-clean-state
    (let ((native  (claude-repl-test--make-owned-buffer "*ws-native*"  "ws-a"))
          (foreign (claude-repl-test--make-owned-buffer "*ws-foreign*" "ws-b"))
          (foreign-win nil))
      (unwind-protect
          (progn
            (switch-to-buffer native)
            (setq foreign-win (split-window))
            (set-window-buffer foreign-win foreign)
            ;; Mark as a claude-repl-style protected panel.
            (set-window-parameter foreign-win 'no-delete-other-windows t)
            (set-window-dedicated-p foreign-win t)
            (claude-repl--clean-frame-foreign-windows "ws-a")
            (let ((wins (window-list nil 'nomini)))
              (should (= 1 (length wins)))
              (should (eq (window-buffer (car wins)) native))))
        (when (and foreign-win (window-live-p foreign-win))
          (set-window-parameter foreign-win 'no-delete-other-windows nil)
          (set-window-dedicated-p foreign-win nil)
          (ignore-errors (delete-window foreign-win)))
        (when (buffer-live-p native)  (kill-buffer native))
        (when (buffer-live-p foreign) (kill-buffer foreign))))))

(ert-deftest claude-repl-cmd-test-clean-frame-foreign-windows/non-claude-buffer-kept ()
  "A non-Claude buffer (no owning workspace) is treated as allowed."
  (claude-repl-test--with-clean-state
    (let ((regular (get-buffer-create "*regular-file*"))
          (extra-win nil))
      (unwind-protect
          (progn
            (with-current-buffer regular
              (should-not (buffer-local-value 'claude-repl--owning-workspace
                                              regular)))
            (switch-to-buffer regular)
            (setq extra-win (split-window))
            (set-window-buffer extra-win regular)
            (claude-repl--clean-frame-foreign-windows "ws-a")
            (should (= 2 (length (window-list nil 'nomini))))
            (dolist (win (window-list nil 'nomini))
              (should (eq (window-buffer win) regular))))
        (when (and extra-win (window-live-p extra-win))
          (ignore-errors (delete-window extra-win)))
        (when (buffer-live-p regular) (kill-buffer regular))))))

(ert-deftest claude-repl-cmd-test-clean-frame-foreign-windows/no-owning-workspace-buffer-kept ()
  "Mixed: native + no-owner buffer — only foreign is scrubbed; no-owner kept."
  (claude-repl-test--with-clean-state
    (let ((native  (claude-repl-test--make-owned-buffer "*ws-native*"  "ws-a"))
          (foreign (claude-repl-test--make-owned-buffer "*ws-foreign*" "ws-b"))
          (regular (get-buffer-create "*regular-file*"))
          (foreign-win nil)
          (regular-win nil))
      (unwind-protect
          (progn
            (switch-to-buffer native)
            (setq foreign-win (split-window))
            (set-window-buffer foreign-win foreign)
            (setq regular-win (split-window))
            (set-window-buffer regular-win regular)
            (claude-repl--clean-frame-foreign-windows "ws-a")
            (let* ((wins (window-list nil 'nomini))
                   (bufs (mapcar #'window-buffer wins)))
              (should (= 2 (length wins)))
              (should (memq native bufs))
              (should (memq regular bufs))
              (should-not (memq foreign bufs))))
        (dolist (w (list foreign-win regular-win))
          (when (and w (window-live-p w))
            (ignore-errors (delete-window w))))
        (when (buffer-live-p native)  (kill-buffer native))
        (when (buffer-live-p foreign) (kill-buffer foreign))
        (when (buffer-live-p regular) (kill-buffer regular))))))

(ert-deftest claude-repl-cmd-test-clean-frame-foreign-windows/swaps-when-all-foreign ()
  "When every window is foreign-claude, helper collapses to fallback."
  (claude-repl-test--with-clean-state
    (let ((foreign1 (claude-repl-test--make-owned-buffer "*ws-foreign-1*" "ws-b"))
          (foreign2 (claude-repl-test--make-owned-buffer "*ws-foreign-2*" "ws-c"))
          (fallback (get-buffer-create " *test-fallback*"))
          (extra-win nil))
      (unwind-protect
          (cl-letf (((symbol-function 'doom-fallback-buffer)
                     (lambda () fallback)))
            (switch-to-buffer foreign1)
            (set-window-parameter (selected-window) 'no-delete-other-windows t)
            (set-window-dedicated-p (selected-window) t)
            (setq extra-win (split-window))
            (set-window-buffer extra-win foreign2)
            (set-window-parameter extra-win 'no-delete-other-windows t)
            (set-window-dedicated-p extra-win t)
            (claude-repl--clean-frame-foreign-windows "ws-fresh")
            (let ((wins (window-list nil 'nomini)))
              (should (= 1 (length wins)))
              (should (eq (window-buffer (car wins)) fallback))
              ;; Surviving window must be writable for the next setup step.
              (should-not (window-dedicated-p (car wins)))))
        ;; Cleanup any leftover windows that survived a failed assertion.
        (dolist (w (window-list nil 'nomini))
          (set-window-parameter w 'no-delete-other-windows nil)
          (set-window-dedicated-p w nil))
        (when (and extra-win (window-live-p extra-win))
          (ignore-errors (delete-window extra-win)))
        (when (buffer-live-p foreign1) (kill-buffer foreign1))
        (when (buffer-live-p foreign2) (kill-buffer foreign2))))))

(ert-deftest claude-repl-cmd-test-load-workspace-snapshot/errors-when-file-missing ()
  "load-workspace-snapshot signals user-error when the snapshot file is absent."
  (claude-repl-test--with-clean-state
    (let ((claude-repl-workspace-snapshot-file "/nonexistent/claude-snap.el"))
      (should-error (claude-repl-load-workspace-snapshot) :type 'user-error))))

(ert-deftest claude-repl-cmd-test-load-workspace-snapshot/establishes-each-entry ()
  "load-workspace-snapshot delegates to `claude-repl--establish-workspace'
once per existing entry, passing the snapshot's `ws' name."
  (claude-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "claude-snap-"))
          (dir-a (make-temp-file "claude-proj-a-" t))
          (dir-b (make-temp-file "claude-proj-b-" t))
          (established nil))
      (unwind-protect
          (let ((claude-repl-workspace-snapshot-file snapshot-file))
            (claude-repl--write-sexp-file snapshot-file
                                          `(("ws-a" . ,dir-a)
                                            ("ws-b" . ,dir-b)))
            (cl-letf (((symbol-function 'claude-repl--establish-workspace)
                       (lambda (ws _dir) (push ws established)))
                      ((symbol-function 'claude-repl--snapshot-load-ws-ready-p)
                       (lambda (_ws) t)))
              (claude-repl-load-workspace-snapshot)
              (should (= 2 (length established)))
              (should (member "ws-a" established))
              (should (member "ws-b" established))))
        (delete-file snapshot-file)
        (delete-directory dir-a t)
        (delete-directory dir-b t)))))

(ert-deftest claude-repl-cmd-test-load-workspace-snapshot/tracks-restored-workspaces ()
  "load-workspace-snapshot records each successfully established entry on
`claude-repl--restored-workspaces' so a later
`claude-repl-nuke-restored-workspaces' can target only the restore batch.
Workspaces that go through the actually-establish branch (NOT the
already-ready short-circuit) must be tagged."
  (claude-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "claude-snap-"))
          (dir-a (make-temp-file "claude-proj-a-" t))
          (dir-b (make-temp-file "claude-proj-b-" t)))
      (unwind-protect
          (let ((claude-repl-workspace-snapshot-file snapshot-file))
            (claude-repl--write-sexp-file snapshot-file
                                          `(("ws-a" . ,dir-a)
                                            ("ws-b" . ,dir-b)))
            (cl-letf (((symbol-function 'claude-repl--establish-workspace) #'ignore)
                      ;; Force the establishing branch (not already-ready).
                      ((symbol-function 'claude-repl--snapshot-load-ws-ready-p)
                       (lambda (_ws) nil))
                      ;; Stub the watchdog so loader doesn't actually wait.
                      ((symbol-function 'run-with-timer)
                       (lambda (&rest _) nil)))
              (claude-repl-load-workspace-snapshot)
              (should (member "ws-a" claude-repl--restored-workspaces))
              ;; ws-b is awaited; advance it manually via the fully-loaded hook
              ;; to confirm the establishing-branch path tags it too.
              (run-hook-with-args 'claude-repl-ws-fully-loaded-functions "ws-a" nil)
              (run-hook-with-args 'claude-repl-ws-fully-loaded-functions "ws-b" nil)
              (should (member "ws-b" claude-repl--restored-workspaces))))
        (delete-file snapshot-file)
        (delete-directory dir-a t)
        (delete-directory dir-b t)))))

(ert-deftest claude-repl-cmd-test-load-workspace-snapshot/already-ready-not-tracked ()
  "A snapshot entry that hits the `already-ready' short-circuit must NOT be
tagged as restored.  Such workspaces were already alive before the loader
ran (the origin ws the user was sitting in, or any other ws claude was
already up in before the 2s idle loader fired).  Tagging them would make
`nuke-restored-workspaces' incorrectly sweep the user's pre-existing
workspace."
  (claude-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "claude-snap-"))
          (dir-origin (make-temp-file "claude-proj-origin-" t))
          (dir-new (make-temp-file "claude-proj-new-" t))
          (ready-table (make-hash-table :test 'equal)))
      (puthash "ws-origin" t ready-table)
      (unwind-protect
          (let ((claude-repl-workspace-snapshot-file snapshot-file))
            (claude-repl--write-sexp-file snapshot-file
                                          `(("ws-origin" . ,dir-origin)
                                            ("ws-new" . ,dir-new)))
            (cl-letf (((symbol-function 'claude-repl--establish-workspace) #'ignore)
                      ;; ws-origin hits the already-ready short-circuit;
                      ;; ws-new goes through the establishing branch.
                      ((symbol-function 'claude-repl--snapshot-load-ws-ready-p)
                       (lambda (ws) (gethash ws ready-table)))
                      ((symbol-function 'run-with-timer)
                       (lambda (&rest _) nil)))
              (claude-repl-load-workspace-snapshot)
              ;; Origin (already-ready) NOT tagged.
              (should-not (member "ws-origin" claude-repl--restored-workspaces))
              ;; Drive the establishing-branch entry to completion.
              (run-hook-with-args 'claude-repl-ws-fully-loaded-functions "ws-new" nil)
              ;; ws-new (actually established) IS tagged.
              (should (member "ws-new" claude-repl--restored-workspaces))))
        (delete-file snapshot-file)
        (delete-directory dir-origin t)
        (delete-directory dir-new t)))))

(ert-deftest claude-repl-cmd-test-load-workspace-snapshot/skipped-not-tracked ()
  "A snapshot entry whose project-dir is gone is NOT added to the restored list.
Only entries the loader actually established (`:loaded') are tracked —
skipped entries (`:skipped' branch) must not pollute the set, otherwise
`nuke-restored-workspaces' would try to tear down ghosts."
  (claude-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "claude-snap-"))
          (real-dir (make-temp-file "claude-proj-real-" t)))
      (unwind-protect
          (let ((claude-repl-workspace-snapshot-file snapshot-file))
            (claude-repl--write-sexp-file snapshot-file
                                          `(("ws-real" . ,real-dir)
                                            ("ws-gone" . "/nonexistent/path")))
            (cl-letf (((symbol-function 'claude-repl--establish-workspace) #'ignore)
                      ;; Force the establishing branch (not already-ready).
                      ((symbol-function 'claude-repl--snapshot-load-ws-ready-p)
                       (lambda (_ws) nil))
                      ((symbol-function 'run-with-timer)
                       (lambda (&rest _) nil)))
              (claude-repl-load-workspace-snapshot)
              ;; Advance ws-real via the fully-loaded hook.
              (run-hook-with-args 'claude-repl-ws-fully-loaded-functions "ws-real" nil)
              (should (member "ws-real" claude-repl--restored-workspaces))
              (should-not (member "ws-gone" claude-repl--restored-workspaces))))
        (delete-file snapshot-file)
        (delete-directory real-dir t)))))

(ert-deftest claude-repl-cmd-test-load-workspace-snapshot/accumulates-across-loads ()
  "Successive snapshot loads union (not replace) `claude-repl--restored-workspaces'.
Loading from-archive after a normal load must not drop the first batch's
restored names — both batches are restore-origin and the nuke-restored
path needs to see both."
  (claude-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "claude-snap-"))
          (dir-a (make-temp-file "claude-proj-a-" t))
          (dir-b (make-temp-file "claude-proj-b-" t)))
      (unwind-protect
          (let ((claude-repl-workspace-snapshot-file snapshot-file))
            (cl-letf (((symbol-function 'claude-repl--establish-workspace) #'ignore)
                      ;; Force the establishing branch (not already-ready)
                      ;; so the loader actually tags both workspaces.
                      ((symbol-function 'claude-repl--snapshot-load-ws-ready-p)
                       (lambda (_ws) nil))
                      ((symbol-function 'run-with-timer)
                       (lambda (&rest _) nil)))
              (claude-repl--write-sexp-file snapshot-file `(("ws-a" . ,dir-a)))
              (claude-repl-load-workspace-snapshot)
              (run-hook-with-args 'claude-repl-ws-fully-loaded-functions "ws-a" nil)
              (claude-repl--write-sexp-file snapshot-file `(("ws-b" . ,dir-b)))
              (claude-repl-load-workspace-snapshot)
              (run-hook-with-args 'claude-repl-ws-fully-loaded-functions "ws-b" nil)
              (should (member "ws-a" claude-repl--restored-workspaces))
              (should (member "ws-b" claude-repl--restored-workspaces))))
        (delete-file snapshot-file)
        (delete-directory dir-a t)
        (delete-directory dir-b t)))))

(ert-deftest claude-repl-cmd-test-load-workspace-snapshot/skips-missing-dirs ()
  "load-workspace-snapshot does not establish entries whose directory is gone."
  (claude-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "claude-snap-"))
          (real-dir (make-temp-file "claude-proj-real-" t))
          (established nil))
      (unwind-protect
          (let ((claude-repl-workspace-snapshot-file snapshot-file))
            (claude-repl--write-sexp-file snapshot-file
                                          `(("ws-real" . ,real-dir)
                                            ("ws-gone" . "/nonexistent/path")))
            (cl-letf (((symbol-function 'claude-repl--establish-workspace)
                       (lambda (ws _dir) (push ws established)))
                      ((symbol-function 'claude-repl--snapshot-load-ws-ready-p)
                       (lambda (_ws) t)))
              (claude-repl-load-workspace-snapshot)
              (should (equal established (list "ws-real")))))
        (delete-file snapshot-file)
        (delete-directory real-dir t)))))

(ert-deftest claude-repl-cmd-test-load-workspace-snapshot/does-not-flash ()
  "load-workspace-snapshot does NOT pulse tabs during bulk restore.
A flash storm would be noise; the loader bypasses the inherent-flash
jump path on purpose."
  (claude-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "claude-snap-"))
          (dir-a (make-temp-file "claude-proj-a-" t))
          (dir-b (make-temp-file "claude-proj-b-" t))
          (flash-calls 0))
      (unwind-protect
          (let ((claude-repl-workspace-snapshot-file snapshot-file))
            (claude-repl--write-sexp-file snapshot-file
                                          `(("ws-a" . ,dir-a)
                                            ("ws-b" . ,dir-b)))
            (cl-letf (((symbol-function 'claude-repl--establish-workspace) #'ignore)
                      ((symbol-function 'claude-repl--snapshot-load-ws-ready-p)
                       (lambda (_ws) t))
                      ((symbol-function 'claude-repl-flash-tab)
                       (lambda (&rest _) (cl-incf flash-calls)))
                      ((symbol-function 'claude-repl--flash-current-tab)
                       (lambda () (cl-incf flash-calls))))
              (claude-repl-load-workspace-snapshot)
              (should (zerop flash-calls))))
        (delete-file snapshot-file)
        (delete-directory dir-a t)
        (delete-directory dir-b t)))))

;;;; ---- Tests: snapshot startup/quit wrappers ----

(ert-deftest claude-repl-cmd-test-load-snapshot-on-startup/no-op-when-file-absent ()
  "Startup wrapper returns quietly when the snapshot file does not exist."
  (claude-repl-test--with-clean-state
    (let ((claude-repl-workspace-snapshot-file "/nonexistent/claude-snap.el")
          (called nil))
      (cl-letf (((symbol-function 'claude-repl-load-workspace-snapshot)
                 (lambda () (setq called t))))
        (claude-repl--load-workspace-snapshot-on-startup)
        (should-not called)))))

(ert-deftest claude-repl-cmd-test-load-snapshot-on-startup/invokes-load-when-file-present ()
  "Startup wrapper calls the real loader when the snapshot file exists."
  (claude-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "claude-snap-"))
          (called nil))
      (unwind-protect
          (let ((claude-repl-workspace-snapshot-file snapshot-file))
            (cl-letf (((symbol-function 'claude-repl-load-workspace-snapshot)
                       (lambda () (setq called t))))
              (claude-repl--load-workspace-snapshot-on-startup)
              (should called)))
        (delete-file snapshot-file)))))

(ert-deftest claude-repl-cmd-test-load-snapshot-on-startup/swallows-errors ()
  "Startup wrapper must not propagate errors from the loader."
  (claude-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "claude-snap-")))
      (unwind-protect
          (let ((claude-repl-workspace-snapshot-file snapshot-file))
            (cl-letf (((symbol-function 'claude-repl-load-workspace-snapshot)
                       (lambda () (error "boom"))))
              (claude-repl--load-workspace-snapshot-on-startup)
              (should t)))
        (delete-file snapshot-file)))))

;;;; ---- Tests: workspace snapshot path resolver ----

(ert-deftest claude-repl-cmd-test-workspace-snapshot-file-for-read-prefers-configured ()
  "workspace-snapshot-file-for-read returns the configured path when it exists."
  (let ((snapshot-file (make-temp-file "claude-snap-cur-")))
    (unwind-protect
        (let ((claude-repl-workspace-snapshot-file snapshot-file))
          (should (equal (claude-repl--workspace-snapshot-file-for-read)
                         snapshot-file)))
      (delete-file snapshot-file))))

(ert-deftest claude-repl-cmd-test-workspace-snapshot-file-for-read-falls-back-to-legacy ()
  "workspace-snapshot-file-for-read falls back to the legacy module-dir
path when the configured file is absent but the legacy file exists."
  (let* ((legacy (make-temp-file "claude-snap-legacy-"))
         (configured "/nonexistent/claude-snap.el"))
    (unwind-protect
        (let ((claude-repl-workspace-snapshot-file configured)
              (claude-repl--legacy-workspace-snapshot-file legacy))
          (should (equal (claude-repl--workspace-snapshot-file-for-read) legacy)))
      (delete-file legacy))))

(ert-deftest claude-repl-cmd-test-workspace-snapshot-file-for-read-defaults-to-configured ()
  "When neither the configured nor the legacy file exists, the resolver
returns the configured path so callers get a reasonable default
(e.g. for `unless (file-exists-p ...)' guards on startup)."
  (let ((claude-repl-workspace-snapshot-file "/nonexistent/configured.el")
        (claude-repl--legacy-workspace-snapshot-file "/nonexistent/legacy.el"))
    (should (equal (claude-repl--workspace-snapshot-file-for-read)
                   "/nonexistent/configured.el"))))

;;;; ---- Tests: workspace snapshot archival ----

(ert-deftest claude-repl-cmd-test-snapshot-archive/first-save-archives-prior ()
  "First save in this Emacs run copies the existing snapshot file into
the archive dir before overwriting (so the previous session's roster
is preserved)."
  (claude-repl-test--with-clean-state
    ;; Seed an existing snapshot on disk to represent the prior session.
    (let ((dir (file-name-directory claude-repl-workspace-snapshot-file)))
      (when (and dir (not (file-directory-p dir))) (make-directory dir t)))
    (with-temp-file claude-repl-workspace-snapshot-file
      (insert "((\"prior-ws\" :project-dir \"/tmp/prior\" :priority nil))"))
    (claude-repl--ws-put "new-ws" :project-dir "/tmp/new")
    (claude-repl-save-workspace-snapshot)
    (let* ((archive-dir (claude-repl--workspace-snapshot-archive-dir))
           (archives (and (file-directory-p archive-dir)
                          (directory-files archive-dir nil "\\.el\\'"))))
      (should (= 1 (length archives))))))

(ert-deftest claude-repl-cmd-test-snapshot-archive/subsequent-saves-skip ()
  "Subsequent saves in the same Emacs run do NOT create additional
archive files — the archive ran already this run."
  (claude-repl-test--with-clean-state
    (let ((dir (file-name-directory claude-repl-workspace-snapshot-file)))
      (when (and dir (not (file-directory-p dir))) (make-directory dir t)))
    (with-temp-file claude-repl-workspace-snapshot-file
      (insert "((\"prior\" :project-dir \"/tmp/prior\" :priority nil))"))
    (claude-repl--ws-put "ws" :project-dir "/tmp/ws")
    (claude-repl-save-workspace-snapshot)   ; first save: archives prior
    (claude-repl-save-workspace-snapshot)   ; second save: must NOT archive again
    (claude-repl-save-workspace-snapshot)   ; third save: still no new archive
    (let* ((archive-dir (claude-repl--workspace-snapshot-archive-dir))
           (archives (and (file-directory-p archive-dir)
                          (directory-files archive-dir nil "\\.el\\'"))))
      (should (= 1 (length archives))))))

(ert-deftest claude-repl-cmd-test-snapshot-archive/no-prior-file-is-noop ()
  "When the snapshot file does not exist, the first save is just a
write — no archive is created (nothing to preserve)."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws" :project-dir "/tmp/ws")
    (claude-repl-save-workspace-snapshot)
    (let ((archive-dir (claude-repl--workspace-snapshot-archive-dir)))
      (should-not (file-directory-p archive-dir)))))

(ert-deftest claude-repl-cmd-test-snapshot-archive/disabled-when-max-zero ()
  "Setting `claude-repl-workspace-snapshot-archive-max' to 0 disables
archival entirely — even when a prior file exists."
  (claude-repl-test--with-clean-state
    (let ((claude-repl-workspace-snapshot-archive-max 0)
          (dir (file-name-directory claude-repl-workspace-snapshot-file)))
      (when (and dir (not (file-directory-p dir))) (make-directory dir t))
      (with-temp-file claude-repl-workspace-snapshot-file
        (insert "((\"prior\" :project-dir \"/tmp/prior\" :priority nil))"))
      (claude-repl--ws-put "ws" :project-dir "/tmp/ws")
      (claude-repl-save-workspace-snapshot)
      (let ((archive-dir (claude-repl--workspace-snapshot-archive-dir)))
        (should-not (file-directory-p archive-dir))))))

(ert-deftest claude-repl-cmd-test-snapshot-archive/prunes-to-cap ()
  "Archive count is capped at `claude-repl-workspace-snapshot-archive-max'.
Older entries (lexicographically earliest filenames) are pruned."
  (claude-repl-test--with-clean-state
    (let ((claude-repl-workspace-snapshot-archive-max 2)
          (archive-dir (claude-repl--workspace-snapshot-archive-dir)))
      (make-directory archive-dir t)
      ;; Pre-seed three "old" archives.
      (dolist (name '("20200101T000000.el"
                      "20200102T000000.el"
                      "20200103T000000.el"))
        (with-temp-file (expand-file-name name archive-dir) (insert "()")))
      ;; Seed the live snapshot so the next save triggers the once-per-run
      ;; archival, which copies that file in and then prunes.
      (let ((dir (file-name-directory claude-repl-workspace-snapshot-file)))
        (when (and dir (not (file-directory-p dir))) (make-directory dir t)))
      (with-temp-file claude-repl-workspace-snapshot-file
        (insert "((\"prior\" :project-dir \"/tmp/p\" :priority nil))"))
      (claude-repl-save-workspace-snapshot)
      ;; After pruning, only the two newest entries remain.  The new
      ;; archive (named after live file's mtime) is one of them; the
      ;; oldest pre-seeded entry is gone.
      (let ((remaining (directory-files archive-dir nil "\\.el\\'")))
        (should (= 2 (length remaining)))
        (should-not (member "20200101T000000.el" remaining))))))

;;;; ---- Tests: snapshot entry normalizer ----

(ert-deftest claude-repl-cmd-test-snapshot-entry-normalize/legacy-shape ()
  "Legacy `(NAME . DIR-STRING)' entries become `(NAME :project-dir DIR)'."
  (let ((n (claude-repl--snapshot-entry-normalize '("ws" . "/tmp/proj"))))
    (should (equal (car n) "ws"))
    (should (equal (plist-get (cdr n) :project-dir) "/tmp/proj"))
    (should (null (plist-get (cdr n) :priority)))))

(ert-deftest claude-repl-cmd-test-snapshot-entry-normalize/plist-shape ()
  "Plist entries are passed through unchanged (priority retained for
back-compat reads of older snapshot files even though new saves omit it)."
  (let ((n (claude-repl--snapshot-entry-normalize
            '("ws" :project-dir "/tmp/proj" :priority "p2"))))
    (should (equal (car n) "ws"))
    (should (equal (plist-get (cdr n) :project-dir) "/tmp/proj"))
    (should (equal (plist-get (cdr n) :priority) "p2"))))

;;;; ---- Tests: save-workspace-snapshot (plist format) ----

(ert-deftest claude-repl-cmd-test-save-workspace-snapshot/omits-priority ()
  "Save deliberately omits :priority from saved entries — the per-project
state file (`<root>/.claude/emacs/state.el') is the authoritative source."
  (claude-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "claude-snap-")))
      (unwind-protect
          (let ((claude-repl-workspace-snapshot-file snapshot-file))
            (claude-repl--ws-put "ws-a" :project-dir "/tmp/a")
            (claude-repl--ws-put "ws-a" :priority "p1")
            (claude-repl-save-workspace-snapshot)
            (let* ((data (claude-repl--read-sexp-file snapshot-file))
                   (entry (assoc "ws-a" data)))
              (should entry)
              (should (equal (plist-get (cdr entry) :project-dir) "/tmp/a"))
              (should-not (plist-member (cdr entry) :priority))))
        (delete-file snapshot-file)))))

(ert-deftest claude-repl-cmd-test-save-workspace-snapshot/one-entry-per-line ()
  "Save writes each workspace entry on its own line for human-readable diffs."
  (claude-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "claude-snap-")))
      (unwind-protect
          (let ((claude-repl-workspace-snapshot-file snapshot-file))
            (claude-repl--ws-put "ws1" :project-dir "/tmp/ws1")
            (claude-repl--ws-put "ws2" :project-dir "/tmp/ws2")
            (claude-repl--ws-put "ws3" :project-dir "/tmp/ws3")
            (claude-repl-save-workspace-snapshot)
            (let* ((raw (with-temp-buffer
                          (insert-file-contents snapshot-file)
                          (buffer-string)))
                   (lines (split-string raw "\n")))
              ;; 3 entries => 3 lines (first line opens with "(", subsequent
              ;; lines start with " " and each carries exactly one entry).
              (should (= 3 (length lines)))
              (should (string-prefix-p "((" (nth 0 lines)))
              (should (string-prefix-p " (" (nth 1 lines)))
              (should (string-prefix-p " (" (nth 2 lines)))
              ;; Still a valid sexp round-trip.
              (should (= 3 (length (claude-repl--read-sexp-file snapshot-file))))))
        (delete-file snapshot-file)))))

;;;; ---- Tests: load-workspace-snapshot (back-compat + hydration + pending) ----

(ert-deftest claude-repl-cmd-test-load-workspace-snapshot/reads-legacy-format ()
  "Loader still accepts the legacy `(NAME . DIR-STRING)' shape."
  (claude-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "claude-snap-"))
          (real-dir (make-temp-file "claude-proj-" t))
          (established nil))
      (unwind-protect
          (let ((claude-repl-workspace-snapshot-file snapshot-file))
            (claude-repl--write-sexp-file snapshot-file `(("ws-legacy" . ,real-dir)))
            (cl-letf (((symbol-function 'claude-repl--establish-workspace)
                       (lambda (ws dir)
                         (push (cons ws dir) established)
                         (claude-repl--ws-put ws :project-dir dir)))
                      ((symbol-function 'claude-repl--snapshot-load-ws-ready-p)
                       (lambda (_ws) t)))
              (claude-repl-load-workspace-snapshot)
              (should (member (cons "ws-legacy" real-dir) established))
              (should (equal (claude-repl--ws-get "ws-legacy" :project-dir) real-dir))))
        (delete-file snapshot-file)
        (delete-directory real-dir t)))))

(ert-deftest claude-repl-cmd-test-load-workspace-snapshot/ignores-legacy-priority ()
  "Loader does NOT pass `:priority' to establish — priority is now
sourced from each project's state file, not the snapshot roster."
  (claude-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "claude-snap-"))
          (real-dir (make-temp-file "claude-proj-" t))
          (call-arity nil))
      (unwind-protect
          (let ((claude-repl-workspace-snapshot-file snapshot-file))
            (claude-repl--write-sexp-file
             snapshot-file
             `(("ws-pri" :project-dir ,real-dir :priority "p1")))
            (cl-letf (((symbol-function 'claude-repl--establish-workspace)
                       (lambda (&rest args) (setq call-arity (length args))))
                      ((symbol-function 'claude-repl--snapshot-load-ws-ready-p)
                       (lambda (_ws) t)))
              (claude-repl-load-workspace-snapshot)
              (should (= call-arity 2))))
        (delete-file snapshot-file)
        (delete-directory real-dir t)))))

(ert-deftest claude-repl-cmd-test-load-workspace-snapshot/returns-to-origin-workspace ()
  "Loader switches back to the workspace that was active when it began."
  (claude-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "claude-snap-"))
          (real-dir (make-temp-file "claude-proj-" t))
          (returned-to nil))
      (unwind-protect
          (let ((claude-repl-workspace-snapshot-file snapshot-file))
            (claude-repl--write-sexp-file
             snapshot-file `(("ws-a" :project-dir ,real-dir)))
            (cl-letf (((symbol-function '+workspace-current-name)
                       (lambda () "origin-ws"))
                      ((symbol-function '+workspace-exists-p) (lambda (_n) t))
                      ((symbol-function 'persp-frame-switch)
                       (lambda (name) (setq returned-to name)))
                      ((symbol-function 'claude-repl--establish-workspace) #'ignore)
                      ((symbol-function 'claude-repl--snapshot-load-ws-ready-p)
                       (lambda (_ws) t)))
              (claude-repl-load-workspace-snapshot)
              (should (equal returned-to "origin-ws"))))
        (delete-file snapshot-file)
        (delete-directory real-dir t)))))

(ert-deftest claude-repl-cmd-test-establish-workspace/starts-claude-when-not-running ()
  "establish-workspace starts claude for the workspace unless it's already running."
  (claude-repl-test--with-clean-state
    (let* ((tmp-dir (file-name-as-directory (make-temp-file "claude-repl-est-" t)))
           (started-for nil))
      (unwind-protect
          (cl-letf (((symbol-function 'claude-repl--initialize-claude)
                     (lambda (ws &rest _) (setq started-for ws)))
                    ((symbol-function 'claude-repl--claude-running-p) (lambda (&rest _) nil))
                    ((symbol-function 'persp-add-new) #'ignore)
                    ((symbol-function 'persp-frame-switch) #'ignore)
                    ((symbol-function 'projectile-add-known-project) #'ignore))
            (claude-repl--establish-workspace "test-ws" tmp-dir)
            (should (equal started-for "test-ws")))
        (delete-directory tmp-dir t)))))

(ert-deftest claude-repl-cmd-test-establish-workspace/skips-claude-when-running ()
  "establish-workspace skips claude-init when claude is already running for ws."
  (claude-repl-test--with-clean-state
    (let* ((tmp-dir (file-name-as-directory (make-temp-file "claude-repl-est-" t)))
           (started nil))
      (unwind-protect
          (cl-letf (((symbol-function 'claude-repl--initialize-claude)
                     (lambda (&rest _) (setq started t)))
                    ((symbol-function 'claude-repl--claude-running-p) (lambda (&rest _) t))
                    ((symbol-function 'persp-add-new) #'ignore)
                    ((symbol-function 'persp-frame-switch) #'ignore)
                    ((symbol-function 'projectile-add-known-project) #'ignore))
            (claude-repl--establish-workspace "test-ws" tmp-dir)
            (should-not started))
        (delete-directory tmp-dir t)))))

(ert-deftest claude-repl-cmd-test-establish-workspace/calls-switch-project-function ()
  "establish-workspace invokes `+workspaces-switch-project-function' (magit lambda)."
  (claude-repl-test--with-clean-state
    (let* ((tmp-dir (file-name-as-directory (make-temp-file "claude-repl-est-" t)))
           (called-with nil))
      (unwind-protect
          (let ((+workspaces-switch-project-function
                 (lambda (d) (setq called-with d))))
            (cl-letf (((symbol-function 'claude-repl--initialize-claude) #'ignore)
                      ((symbol-function 'claude-repl--claude-running-p) (lambda (&rest _) nil))
                      ((symbol-function 'persp-add-new) #'ignore)
                      ((symbol-function 'persp-frame-switch) #'ignore)
                      ((symbol-function 'projectile-add-known-project) #'ignore))
              (claude-repl--establish-workspace "test-ws" tmp-dir)
              (should (equal (file-name-as-directory called-with) tmp-dir))))
        (delete-directory tmp-dir t)))))

(ert-deftest claude-repl-cmd-test-establish-workspace/restores-fallback-default-directory ()
  "establish-workspace must not permanently mutate the shared fallback
buffer's `default-directory'.  The buffer is visible from every persp,
so a permanent mutation makes scratch report the last-loaded ws's project
root from every persp (a cross-persp bleed)."
  (claude-repl-test--with-clean-state
    (let* ((tmp-dir (file-name-as-directory (make-temp-file "claude-repl-est-" t)))
           (fb (get-buffer-create " *test-fb-default-dir*"))
           (sentinel-dir "/sentinel-original-dir/"))
      (unwind-protect
          (cl-letf (((symbol-function 'doom-fallback-buffer) (lambda () fb))
                    ((symbol-function 'claude-repl--initialize-claude) #'ignore)
                    ((symbol-function 'claude-repl--claude-running-p) (lambda (&rest _) nil))
                    ((symbol-function 'persp-add-new) #'ignore)
                    ((symbol-function 'persp-frame-switch) #'ignore)
                    ((symbol-function 'projectile-add-known-project) #'ignore))
            (with-current-buffer fb
              (setq default-directory sentinel-dir))
            (claude-repl--establish-workspace "test-ws" tmp-dir)
            (should (equal (buffer-local-value 'default-directory fb)
                           sentinel-dir)))
        (when (buffer-live-p fb) (kill-buffer fb))
        (delete-directory tmp-dir t)))))

(ert-deftest claude-repl-cmd-test-establish-workspace/fallback-default-dir-visible-during-hook ()
  "While `+workspaces-switch-project-function' runs, the fallback buffer's
`default-directory' must reflect the project root (some Doom hooks
depend on it).  After the hook returns it's restored."
  (claude-repl-test--with-clean-state
    (let* ((tmp-dir (file-name-as-directory (make-temp-file "claude-repl-est-" t)))
           (fb (get-buffer-create " *test-fb-default-dir-hook*"))
           (sentinel-dir "/sentinel-original-dir/")
           (observed-dir nil))
      (unwind-protect
          (let ((+workspaces-switch-project-function
                 (lambda (_dir)
                   (setq observed-dir
                         (buffer-local-value 'default-directory fb)))))
            (cl-letf (((symbol-function 'doom-fallback-buffer) (lambda () fb))
                      ((symbol-function 'claude-repl--initialize-claude) #'ignore)
                      ((symbol-function 'claude-repl--claude-running-p) (lambda (&rest _) nil))
                      ((symbol-function 'persp-add-new) #'ignore)
                      ((symbol-function 'persp-frame-switch) #'ignore)
                      ((symbol-function 'projectile-add-known-project) #'ignore))
              (with-current-buffer fb
                (setq default-directory sentinel-dir))
              (claude-repl--establish-workspace "test-ws" tmp-dir)
              (should (equal observed-dir tmp-dir))
              (should (equal (buffer-local-value 'default-directory fb)
                             sentinel-dir))))
        (when (buffer-live-p fb) (kill-buffer fb))
        (delete-directory tmp-dir t)))))

(ert-deftest claude-repl-cmd-test-establish-workspace/opens-recent-file ()
  "establish-workspace opens the most-recent file via `find-file' when one exists."
  (claude-repl-test--with-clean-state
    (let* ((tmp-dir (file-name-as-directory (make-temp-file "claude-repl-est-" t)))
           (tmp-file (expand-file-name "hello.el" tmp-dir))
           (opened nil))
      (unwind-protect
          (progn
            (with-temp-file tmp-file (insert ";; placeholder"))
            (cl-letf (((symbol-function 'claude-repl--most-recent-project-file)
                       (lambda (_d) tmp-file))
                      ((symbol-function 'find-file)
                       (lambda (f) (setq opened f)))
                      ((symbol-function 'claude-repl--initialize-claude) #'ignore)
                      ((symbol-function 'claude-repl--claude-running-p) (lambda (&rest _) nil))
                      ((symbol-function 'persp-add-new) #'ignore)
                      ((symbol-function 'persp-frame-switch) #'ignore)
                      ((symbol-function 'projectile-add-known-project) #'ignore))
              (claude-repl--establish-workspace "test-ws" tmp-dir)
              (should (equal opened tmp-file))))
        (delete-directory tmp-dir t)))))

(ert-deftest claude-repl-cmd-test-establish-workspace/skips-recent-when-gone ()
  "establish-workspace skips `find-file' when the recent file doesn't exist."
  (claude-repl-test--with-clean-state
    (let* ((tmp-dir (file-name-as-directory (make-temp-file "claude-repl-est-" t)))
           (find-file-called nil))
      (unwind-protect
          (cl-letf (((symbol-function 'claude-repl--most-recent-project-file)
                     (lambda (_d) "/nonexistent/gone.el"))
                    ((symbol-function 'find-file)
                     (lambda (&rest _) (setq find-file-called t)))
                    ((symbol-function 'claude-repl--initialize-claude) #'ignore)
                    ((symbol-function 'claude-repl--claude-running-p) (lambda (&rest _) nil))
                    ((symbol-function 'persp-add-new) #'ignore)
                    ((symbol-function 'persp-frame-switch) #'ignore)
                    ((symbol-function 'projectile-add-known-project) #'ignore))
            (claude-repl--establish-workspace "test-ws" tmp-dir)
            (should-not find-file-called))
        (delete-directory tmp-dir t)))))

(ert-deftest claude-repl-cmd-test-establish-workspace/rehydrates-priority-from-state ()
  "establish-workspace calls `--hydrate-priority-from-state' so the badge
restores from the per-project state file rather than the roster."
  (claude-repl-test--with-clean-state
    (let* ((tmp-dir (file-name-as-directory (make-temp-file "claude-repl-est-pri-" t)))
           (hydrated-with nil))
      (unwind-protect
          (cl-letf (((symbol-function 'persp-add-new) #'ignore)
                    ((symbol-function 'persp-frame-switch) #'ignore)
                    ((symbol-function 'projectile-add-known-project) #'ignore)
                    ((symbol-function 'claude-repl--initialize-claude) #'ignore)
                    ((symbol-function 'claude-repl--claude-running-p) (lambda (&rest _) nil))
                    ((symbol-function 'claude-repl--hydrate-priority-from-state)
                     (lambda (d) (setq hydrated-with d))))
            (claude-repl--establish-workspace "test-ws" tmp-dir)
            (should (equal (file-name-as-directory hydrated-with) tmp-dir)))
        (delete-directory tmp-dir t)))))

(ert-deftest claude-repl-cmd-test-establish-workspace/reorders-by-priority ()
  "establish-workspace calls `--reorder-workspace-by-priority' AFTER priority
hydration so restored workspaces appear in priority order, matching the
behavior of `claude-repl-set-priority'.  Without this, snapshot entries
sit in file order even when state.el carries priorities."
  (claude-repl-test--with-clean-state
    (let* ((tmp-dir (file-name-as-directory (make-temp-file "claude-repl-est-reorder-" t)))
           (events nil))
      (unwind-protect
          (cl-letf (((symbol-function 'persp-add-new) #'ignore)
                    ((symbol-function 'persp-frame-switch) #'ignore)
                    ((symbol-function 'projectile-add-known-project) #'ignore)
                    ((symbol-function 'claude-repl--initialize-claude) #'ignore)
                    ((symbol-function 'claude-repl--claude-running-p) (lambda (&rest _) nil))
                    ((symbol-function 'claude-repl--hydrate-priority-from-state)
                     (lambda (_d) (push 'hydrate events)))
                    ((symbol-function 'claude-repl--reorder-workspace-by-priority)
                     (lambda (_ws) (push 'reorder events))))
            (claude-repl--establish-workspace "test-ws" tmp-dir)
            (let ((ordered (reverse events)))
              (should (memq 'hydrate ordered))
              (should (memq 'reorder ordered))
              ;; Reorder must come after hydrate so it reads a real priority.
              (should (< (cl-position 'hydrate ordered)
                         (cl-position 'reorder ordered)))))
        (delete-directory tmp-dir t)))))

(ert-deftest claude-repl-cmd-test-establish-workspace/activates-persp ()
  "establish-workspace calls `persp-frame-switch' with the snapshot's ws name
so persp-mode begins capturing a window configuration for that persp."
  (claude-repl-test--with-clean-state
    (let* ((tmp-dir (file-name-as-directory (make-temp-file "claude-repl-est-" t)))
           (switched-to nil))
      (unwind-protect
          (cl-letf (((symbol-function 'persp-add-new) #'ignore)
                    ((symbol-function 'persp-frame-switch)
                     (lambda (n) (setq switched-to n)))
                    ((symbol-function 'projectile-add-known-project) #'ignore)
                    ((symbol-function 'claude-repl--initialize-claude) #'ignore)
                    ((symbol-function 'claude-repl--claude-running-p) (lambda (&rest _) nil)))
            (claude-repl--establish-workspace "DC/CV-494738/worker-suite" tmp-dir)
            (should (equal switched-to "DC/CV-494738/worker-suite")))
        (delete-directory tmp-dir t)))))

;;;; ---- Tests: snapshot-load queue driver (after-ready hook) ----

(ert-deftest claude-repl-cmd-test-snapshot-load/refuses-concurrent-invocation ()
  "Calling load while a load is in progress signals user-error."
  (claude-repl-test--with-clean-state
    (let ((claude-repl--snapshot-load-state (list :queue nil)))
      (should-error (claude-repl-load-workspace-snapshot) :type 'user-error))))

(ert-deftest claude-repl-cmd-test-snapshot-load/advances-on-ready-event ()
  "A ws-fully-loaded callback for the awaited ws advances the queue."
  (claude-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "claude-snap-"))
          (dir-a (make-temp-file "claude-proj-a-" t))
          (dir-b (make-temp-file "claude-proj-b-" t))
          (established nil))
      (unwind-protect
          (let ((claude-repl-workspace-snapshot-file snapshot-file))
            (claude-repl--write-sexp-file snapshot-file
                                          `(("ws-a" . ,dir-a) ("ws-b" . ,dir-b)))
            (cl-letf (((symbol-function 'claude-repl--establish-workspace)
                       (lambda (ws _dir) (push ws established)))
                      ;; Don't short-circuit on already-ready — force the
                      ;; hook-driven path.
                      ((symbol-function 'claude-repl--snapshot-load-ws-ready-p)
                       (lambda (_ws) nil))
                      ((symbol-function 'run-with-timer)
                       (lambda (&rest _) nil)))
              (claude-repl-load-workspace-snapshot)
              ;; First entry established; loader is awaiting ws-a.
              (should (equal established '("ws-a")))
              (should (equal "ws-a"
                             (plist-get claude-repl--snapshot-load-state :awaiting)))
              ;; Simulate ws-fully-loaded signal for ws-a (no marker = happy path).
              (run-hook-with-args 'claude-repl-ws-fully-loaded-functions "ws-a" nil)
              (should (equal (sort (copy-sequence established) #'string<)
                             '("ws-a" "ws-b")))
              ;; Simulate ws-fully-loaded for ws-b → load finishes.
              (run-hook-with-args 'claude-repl-ws-fully-loaded-functions "ws-b" nil)
              (should-not claude-repl--snapshot-load-state)))
        (delete-file snapshot-file)
        (delete-directory dir-a t)
        (delete-directory dir-b t)))))

(ert-deftest claude-repl-cmd-test-snapshot-load/ignores-foreign-ready ()
  "Ready signal for a workspace we're not awaiting does NOT advance the queue."
  (claude-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "claude-snap-"))
          (dir-a (make-temp-file "claude-proj-a-" t))
          (dir-b (make-temp-file "claude-proj-b-" t))
          (established nil))
      (unwind-protect
          (let ((claude-repl-workspace-snapshot-file snapshot-file))
            (claude-repl--write-sexp-file snapshot-file
                                          `(("ws-a" . ,dir-a) ("ws-b" . ,dir-b)))
            (cl-letf (((symbol-function 'claude-repl--establish-workspace)
                       (lambda (ws _dir) (push ws established)))
                      ((symbol-function 'claude-repl--snapshot-load-ws-ready-p)
                       (lambda (_ws) nil))
                      ((symbol-function 'run-with-timer)
                       (lambda (&rest _) nil)))
              (claude-repl-load-workspace-snapshot)
              ;; Foreign ws-fully-loaded — must NOT advance.
              (run-hook-with-args 'claude-repl-ws-fully-loaded-functions "some-other-ws" nil)
              (should (equal established '("ws-a")))
              (should (equal "ws-a"
                             (plist-get claude-repl--snapshot-load-state :awaiting)))))
        (delete-file snapshot-file)
        (delete-directory dir-a t)
        (delete-directory dir-b t)))))

(ert-deftest claude-repl-cmd-test-snapshot-load/already-ready-short-circuits ()
  "When `--snapshot-load-ws-ready-p' is t after establish, queue advances
without waiting for a hook fire."
  (claude-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "claude-snap-"))
          (dir-a (make-temp-file "claude-proj-a-" t))
          (dir-b (make-temp-file "claude-proj-b-" t))
          (established nil))
      (unwind-protect
          (let ((claude-repl-workspace-snapshot-file snapshot-file))
            (claude-repl--write-sexp-file snapshot-file
                                          `(("ws-a" . ,dir-a) ("ws-b" . ,dir-b)))
            (cl-letf (((symbol-function 'claude-repl--establish-workspace)
                       (lambda (ws _dir) (push ws established)))
                      ((symbol-function 'claude-repl--snapshot-load-ws-ready-p)
                       (lambda (_ws) t))
                      ((symbol-function 'run-with-timer)
                       (lambda (&rest _) nil)))
              (claude-repl-load-workspace-snapshot)
              (should (equal (sort established #'string<) '("ws-a" "ws-b")))
              (should-not claude-repl--snapshot-load-state)))
        (delete-file snapshot-file)
        (delete-directory dir-a t)
        (delete-directory dir-b t)))))

(ert-deftest claude-repl-cmd-test-snapshot-load/skip-missing-dir-does-not-wait ()
  "Missing-dir entries are skipped and the queue advances synchronously."
  (claude-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "claude-snap-"))
          (real-dir (make-temp-file "claude-proj-" t))
          (established nil))
      (unwind-protect
          (let ((claude-repl-workspace-snapshot-file snapshot-file))
            (claude-repl--write-sexp-file
             snapshot-file
             `(("ws-gone" . "/nonexistent/path")
               ("ws-real" . ,real-dir)))
            (cl-letf (((symbol-function 'claude-repl--establish-workspace)
                       (lambda (ws _dir) (push ws established)))
                      ((symbol-function 'claude-repl--snapshot-load-ws-ready-p)
                       (lambda (_ws) t))
                      ((symbol-function 'run-with-timer)
                       (lambda (&rest _) nil)))
              (claude-repl-load-workspace-snapshot)
              ;; ws-gone skipped; ws-real established (no wait).
              (should (equal established '("ws-real")))
              (should-not claude-repl--snapshot-load-state)))
        (delete-file snapshot-file)
        (delete-directory real-dir t)))))

(ert-deftest claude-repl-cmd-test-snapshot-load/timeout-advances-queue ()
  "Per-entry watchdog firing advances past a wedged workspace."
  (claude-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "claude-snap-"))
          (dir-a (make-temp-file "claude-proj-a-" t))
          (dir-b (make-temp-file "claude-proj-b-" t))
          (established nil)
          captured-timer-callback)
      (unwind-protect
          (let ((claude-repl-workspace-snapshot-file snapshot-file))
            (claude-repl--write-sexp-file snapshot-file
                                          `(("ws-a" . ,dir-a) ("ws-b" . ,dir-b)))
            (cl-letf (((symbol-function 'claude-repl--establish-workspace)
                       (lambda (ws _dir) (push ws established)))
                      ((symbol-function 'claude-repl--snapshot-load-ws-ready-p)
                       (lambda (_ws) nil))
                      ((symbol-function 'run-with-timer)
                       (lambda (_secs _rep fn &rest args)
                         (setq captured-timer-callback (cons fn args))
                         'fake-timer))
                      ((symbol-function 'timerp) (lambda (_t) nil))
                      ((symbol-function 'cancel-timer) #'ignore))
              (claude-repl-load-workspace-snapshot)
              (should (equal established '("ws-a")))
              ;; Fire the timeout for ws-a manually.
              (apply (car captured-timer-callback) (cdr captured-timer-callback))
              (should (equal (sort (copy-sequence established) #'string<)
                             '("ws-a" "ws-b")))))
        (delete-file snapshot-file)
        (delete-directory dir-a t)
        (delete-directory dir-b t)))))

(ert-deftest claude-repl-cmd-test-snapshot-load/timeout-fires-fully-loaded-with-marker ()
  "Watchdog timeout for ws fires `ws-fully-loaded-functions' with the
`:timed-out' marker so observers can distinguish forced advance from
the happy-path advance."
  (claude-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "claude-snap-"))
          (dir-a (make-temp-file "claude-proj-a-" t))
          (dir-b (make-temp-file "claude-proj-b-" t))
          (fired-with nil)
          captured-timer-callback)
      (unwind-protect
          (let ((claude-repl-workspace-snapshot-file snapshot-file))
            (claude-repl--write-sexp-file snapshot-file
                                          `(("ws-a" . ,dir-a) ("ws-b" . ,dir-b)))
            (cl-letf (((symbol-function 'claude-repl--establish-workspace) #'ignore)
                      ((symbol-function 'claude-repl--snapshot-load-ws-ready-p)
                       (lambda (_ws) nil))
                      ((symbol-function 'run-with-timer)
                       (lambda (_secs _rep fn &rest args)
                         (setq captured-timer-callback (cons fn args))
                         'fake-timer))
                      ((symbol-function 'timerp) (lambda (_t) nil))
                      ((symbol-function 'cancel-timer) #'ignore))
              (let ((claude-repl-ws-fully-loaded-functions
                     (list (lambda (ws marker)
                             (push (cons ws marker) fired-with))
                           ;; Plus the loader's own subscriber stays attached.
                           #'claude-repl--snapshot-load-on-loaded)))
                (claude-repl-load-workspace-snapshot)
                ;; Fire the timeout for ws-a manually.
                (apply (car captured-timer-callback) (cdr captured-timer-callback))
                ;; Hook fired for ws-a with :timed-out marker.
                (should (cl-some (lambda (e)
                                   (and (equal (car e) "ws-a")
                                        (eq (cdr e) :timed-out)))
                                 fired-with)))))
        (delete-file snapshot-file)
        (delete-directory dir-a t)
        (delete-directory dir-b t)))))

(ert-deftest claude-repl-cmd-test-snapshot-load/hook-detached-on-finish ()
  "After a successful load, `claude-repl--snapshot-load-on-loaded' is removed
from `claude-repl-ws-fully-loaded-functions'."
  (claude-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "claude-snap-"))
          (real-dir (make-temp-file "claude-proj-" t)))
      (unwind-protect
          (let ((claude-repl-workspace-snapshot-file snapshot-file))
            (claude-repl--write-sexp-file snapshot-file `(("ws-a" . ,real-dir)))
            (cl-letf (((symbol-function 'claude-repl--establish-workspace) #'ignore)
                      ((symbol-function 'claude-repl--snapshot-load-ws-ready-p)
                       (lambda (_ws) t))
                      ((symbol-function 'run-with-timer)
                       (lambda (&rest _) nil)))
              (claude-repl-load-workspace-snapshot)
              (should-not (memq #'claude-repl--snapshot-load-on-loaded
                                claude-repl-ws-fully-loaded-functions))))
        (delete-file snapshot-file)
        (delete-directory real-dir t)))))

(ert-deftest claude-repl-cmd-test-snapshot-load/establish-error-advances ()
  "If `--establish-workspace' errors, the loader logs and advances anyway."
  (claude-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "claude-snap-"))
          (dir-a (make-temp-file "claude-proj-a-" t))
          (dir-b (make-temp-file "claude-proj-b-" t))
          (attempts nil))
      (unwind-protect
          (let ((claude-repl-workspace-snapshot-file snapshot-file))
            (claude-repl--write-sexp-file snapshot-file
                                          `(("ws-a" . ,dir-a) ("ws-b" . ,dir-b)))
            (cl-letf (((symbol-function 'claude-repl--establish-workspace)
                       (lambda (ws _dir)
                         (push ws attempts)
                         (when (equal ws "ws-a")
                           (error "boom"))))
                      ((symbol-function 'claude-repl--snapshot-load-ws-ready-p)
                       (lambda (_ws) t))
                      ((symbol-function 'run-with-timer)
                       (lambda (&rest _) nil)))
              (claude-repl-load-workspace-snapshot)
              ;; Both entries attempted despite ws-a's error.
              (should (equal (sort (copy-sequence attempts) #'string<)
                             '("ws-a" "ws-b")))
              (should-not claude-repl--snapshot-load-state)))
        (delete-file snapshot-file)
        (delete-directory dir-a t)
        (delete-directory dir-b t)))))

(ert-deftest claude-repl-cmd-test-snapshot-load/establish-error-bumps-load-error-not-loaded ()
  "An establish failure increments `:load-error', NOT `:loaded'."
  (claude-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "claude-snap-"))
          (dir-a (make-temp-file "claude-proj-a-" t))
          (dir-b (make-temp-file "claude-proj-b-" t))
          finish-state)
      (unwind-protect
          (let ((claude-repl-workspace-snapshot-file snapshot-file))
            (claude-repl--write-sexp-file snapshot-file
                                          `(("ws-a" . ,dir-a) ("ws-b" . ,dir-b)))
            (cl-letf (((symbol-function 'claude-repl--establish-workspace)
                       (lambda (ws _dir)
                         (when (equal ws "ws-a") (error "boom"))))
                      ((symbol-function 'claude-repl--snapshot-load-ws-ready-p)
                       (lambda (_ws) t))
                      ((symbol-function 'run-with-timer)
                       (lambda (&rest _) nil))
                      ((symbol-function 'claude-repl--snapshot-load-finish)
                       (lambda ()
                         (setq finish-state (copy-sequence claude-repl--snapshot-load-state))
                         (setq claude-repl--snapshot-load-state nil))))
              (claude-repl-load-workspace-snapshot)
              (should (= 1 (plist-get finish-state :loaded)))
              (should (= 1 (plist-get finish-state :load-error)))
              (should (= 0 (plist-get finish-state :skipped)))))
        (delete-file snapshot-file)
        (delete-directory dir-a t)
        (delete-directory dir-b t)))))

(ert-deftest claude-repl-cmd-test-snapshot-load/establish-error-skips-watchdog ()
  "An establish failure must NOT arm the per-entry watchdog timer —
no ws is alive to wait on, advance immediately."
  (claude-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "claude-snap-"))
          (dir-a (make-temp-file "claude-proj-a-" t))
          (dir-b (make-temp-file "claude-proj-b-" t))
          (timer-calls 0))
      (unwind-protect
          (let ((claude-repl-workspace-snapshot-file snapshot-file))
            (claude-repl--write-sexp-file snapshot-file
                                          `(("ws-a" . ,dir-a) ("ws-b" . ,dir-b)))
            (cl-letf (((symbol-function 'claude-repl--establish-workspace)
                       (lambda (ws _dir)
                         (when (equal ws "ws-a") (error "boom"))))
                      ((symbol-function 'claude-repl--snapshot-load-ws-ready-p)
                       (lambda (_ws) t))
                      ((symbol-function 'run-with-timer)
                       (lambda (&rest _) (cl-incf timer-calls) 'fake-timer)))
              (claude-repl-load-workspace-snapshot)
              ;; Watchdog should NOT have been armed for ws-a (failure path)
              ;; and ws-b takes the already-ready short-circuit, so 0 timers.
              (should (= 0 timer-calls))))
        (delete-file snapshot-file)
        (delete-directory dir-a t)
        (delete-directory dir-b t)))))

(ert-deftest claude-repl-cmd-test-snapshot-load/state-omits-pre-load-window-snapshot ()
  "Loader state must not carry a captured window-configuration or
window-state for origin: persp-mode's own switch-away save (triggered
by the first `--establish-workspace's `persp-frame-switch') handles
origin's layout, so any extra capture is dead weight that risks
resurrecting foreign or dead buffers on restore."
  (claude-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "claude-snap-"))
          (real-dir (make-temp-file "claude-proj-" t))
          (state-during-establish nil))
      (unwind-protect
          (let ((claude-repl-workspace-snapshot-file snapshot-file))
            (claude-repl--write-sexp-file snapshot-file `(("ws-a" . ,real-dir)))
            (cl-letf (((symbol-function 'claude-repl--establish-workspace)
                       (lambda (&rest _)
                         (setq state-during-establish
                               (copy-sequence claude-repl--snapshot-load-state))))
                      ((symbol-function 'claude-repl--snapshot-load-ws-ready-p)
                       (lambda (_ws) t))
                      ((symbol-function 'run-with-timer)
                       (lambda (&rest _) nil)))
              (claude-repl-load-workspace-snapshot)
              (should-not (plist-member state-during-establish :origin-window-config))
              (should-not (plist-member state-during-establish :origin-window-state))))
        (delete-file snapshot-file)
        (delete-directory real-dir t)))))

(ert-deftest claude-repl-cmd-test-snapshot-load/top-level-error-finishes ()
  "An error inside `--snapshot-load-step' (outside establish-workspace) is
routed to `--snapshot-load-finish' so the hook detaches and state clears
instead of leaving a zombie loader."
  (claude-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "claude-snap-"))
          (dir-a (make-temp-file "claude-proj-a-" t)))
      (unwind-protect
          (let ((claude-repl-workspace-snapshot-file snapshot-file))
            (claude-repl--write-sexp-file snapshot-file `(("ws-a" . ,dir-a)))
            (cl-letf (((symbol-function 'claude-repl--establish-workspace) #'ignore)
                      ;; Force a signal from the ready check — covers the path
                      ;; where neither establish-workspace nor finish itself
                      ;; raised, but a helper between them did.
                      ((symbol-function 'claude-repl--snapshot-load-ws-ready-p)
                       (lambda (_ws) (error "ready-check boom")))
                      ((symbol-function 'run-with-timer)
                       (lambda (&rest _) nil)))
              (claude-repl-load-workspace-snapshot)
              (should-not claude-repl--snapshot-load-state)
              (should-not (memq #'claude-repl--snapshot-load-on-loaded
                                claude-repl-ws-fully-loaded-functions))
              (should claude-repl--snapshot-loaded-p)))
        (delete-file snapshot-file)
        (delete-directory dir-a t)))))

(ert-deftest claude-repl-cmd-test-snapshot-load/awaiting-nil-during-establish ()
  "Bug 6: `:awaiting' must remain nil for the duration of
`--establish-workspace' so a re-entrant ready event (today impossible,
latent if establish ever yields) is a no-op rather than advancing the
queue mid-call.  Asserts the contract by firing
`--snapshot-load-on-loaded' synchronously from inside the mocked
establish and verifying (a) `:awaiting' is nil at the re-entry point,
(b) the queue did NOT advance to the next ws during establish, and
(c) `:awaiting' is set to ws-a only after establish returned."
  (claude-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "claude-snap-"))
          (dir-a (make-temp-file "claude-proj-a-" t))
          (dir-b (make-temp-file "claude-proj-b-" t))
          (established nil)
          (reentry-awaiting 'unset)
          (reentry-established 'unset))
      (unwind-protect
          (let ((claude-repl-workspace-snapshot-file snapshot-file))
            (claude-repl--write-sexp-file snapshot-file
                                          `(("ws-a" . ,dir-a) ("ws-b" . ,dir-b)))
            (cl-letf (((symbol-function 'claude-repl--establish-workspace)
                       (lambda (ws _dir)
                         (push ws established)
                         (when (equal ws "ws-a")
                           ;; Capture state at the moment of re-entry, then
                           ;; synchronously fire the loaded hook for ws-a
                           ;; from inside establish.  The handler must see
                           ;; `:awaiting' nil and short-circuit.
                           (setq reentry-awaiting
                                 (plist-get claude-repl--snapshot-load-state :awaiting))
                           (claude-repl--snapshot-load-on-loaded "ws-a")
                           ;; If the re-entry had advanced the queue, ws-b
                           ;; would have been pushed onto `established' here.
                           (setq reentry-established (copy-sequence established)))))
                      ;; Force the hook-driven path (not already-ready).
                      ((symbol-function 'claude-repl--snapshot-load-ws-ready-p)
                       (lambda (_ws) nil))
                      ((symbol-function 'run-with-timer)
                       (lambda (&rest _) nil)))
              (claude-repl-load-workspace-snapshot)
              ;; (a) During re-entry, `:awaiting' was nil — the handler check
              ;; `(equal ws :awaiting)' failed, so the callback was a no-op.
              (should (null reentry-awaiting))
              ;; (b) The queue did NOT advance to ws-b while establish for
              ;; ws-a was still on the stack.
              (should (equal reentry-established '("ws-a")))
              ;; (c) After establish returned and the watchdog branch ran,
              ;; `:awaiting' is now ws-a — the loader is properly parked.
              (should (equal "ws-a"
                             (plist-get claude-repl--snapshot-load-state :awaiting)))
              ;; Sanity: ws-b was never established.
              (should (equal established '("ws-a")))))
        (delete-file snapshot-file)
        (delete-directory dir-a t)
        (delete-directory dir-b t)))))

(ert-deftest claude-repl-cmd-test-snapshot-load-finish/idempotent ()
  "Calling `--snapshot-load-finish' twice is harmless: the second call
sees nil state and short-circuits without printing bogus counters."
  (claude-repl-test--with-clean-state
    (setq claude-repl--snapshot-load-state
          (list :queue nil :origin nil :awaiting nil
                :loaded 0 :skipped 0 :total 0 :timeout-timer nil))
    (claude-repl--snapshot-load-finish)
    (should-not claude-repl--snapshot-load-state)
    ;; Second call must not error and must not re-set --snapshot-loaded-p
    ;; from a synthetic state (no state, no message).
    (claude-repl--snapshot-load-finish)
    (should-not claude-repl--snapshot-load-state)))

;;;; ---- claude-repl--hydrate-priority-from-state ----

(ert-deftest claude-repl-cmd-test-hydrate-priority/sets-priority-from-state-file ()
  "hydrate-priority reads :priority from .claude-repl-state and applies it."
  (claude-repl-test--with-clean-state
    (let ((tmp-dir (file-name-as-directory (make-temp-file "claude-repl-hydrate-" t))))
      (unwind-protect
          (progn
            (claude-repl-test--seed-file
             (claude-repl--state-file tmp-dir)
             (prin1-to-string '(:project-dir "/some/dir" :active-env :bare-metal :priority "p1")))
            (cl-letf (((symbol-function '+workspace-current-name)
                       (lambda () "test-ws"))
                      ((symbol-function 'force-mode-line-update)
                       (lambda (&optional _all) nil)))
              (claude-repl--hydrate-priority-from-state tmp-dir)
              (should (equal (claude-repl--ws-get "test-ws" :priority) "p1"))))
        (delete-directory tmp-dir t)))))

(ert-deftest claude-repl-cmd-test-hydrate-priority/no-state-file-noop ()
  "hydrate-priority is a no-op when the state file is missing."
  (claude-repl-test--with-clean-state
    (let ((tmp-dir (file-name-as-directory (make-temp-file "claude-repl-hydrate-" t))))
      (unwind-protect
          (cl-letf (((symbol-function '+workspace-current-name)
                     (lambda () "test-ws")))
            (claude-repl--hydrate-priority-from-state tmp-dir)
            (should-not (claude-repl--ws-get "test-ws" :priority)))
        (delete-directory tmp-dir t)))))

(ert-deftest claude-repl-cmd-test-hydrate-priority/state-without-priority-noop ()
  "hydrate-priority is a no-op when state file has no :priority entry."
  (claude-repl-test--with-clean-state
    (let ((tmp-dir (file-name-as-directory (make-temp-file "claude-repl-hydrate-" t))))
      (unwind-protect
          (progn
            (claude-repl-test--seed-file
             (claude-repl--state-file tmp-dir)
             (prin1-to-string '(:project-dir "/some/dir" :active-env :bare-metal)))
            (cl-letf (((symbol-function '+workspace-current-name)
                       (lambda () "test-ws")))
              (claude-repl--hydrate-priority-from-state tmp-dir)
              (should-not (claude-repl--ws-get "test-ws" :priority))))
        (delete-directory tmp-dir t)))))

;;;; ---- claude-repl-switch-to-project ----

(ert-deftest claude-repl-cmd-test-switch-to-project/switches-then-hydrates ()
  "switch-to-project switches via projectile, then hydrates priority."
  (claude-repl-test--with-clean-state
    (let ((tmp-dir (file-name-as-directory (make-temp-file "claude-repl-switch-" t)))
          switched-with)
      (unwind-protect
          (progn
            (claude-repl-test--seed-file
             (claude-repl--state-file tmp-dir)
             (prin1-to-string '(:priority "p2")))
            (cl-letf (((symbol-function 'projectile-switch-project-by-name)
                       (lambda (project) (setq switched-with project)))
                      ((symbol-function 'claude-repl--most-recent-project-file)
                       (lambda (_d) nil))
                      ((symbol-function '+workspace-current-name)
                       (lambda () "switched-ws"))
                      ((symbol-function 'force-mode-line-update)
                       (lambda (&optional _all) nil))
                      ((symbol-function 'claude-repl-flash-tab)
                       (lambda (&rest _) nil)))
              (claude-repl-switch-to-project tmp-dir)
              (should (equal switched-with tmp-dir))
              (should (equal (claude-repl--ws-get "switched-ws" :priority) "p2"))))
        (delete-directory tmp-dir t)))))

(ert-deftest claude-repl-cmd-test-switch-to-project/flashes-activated-ws ()
  "switch-to-project pulses the activated workspace tab via flash-tab."
  (claude-repl-test--with-clean-state
    (let ((tmp-dir (file-name-as-directory (make-temp-file "claude-repl-switch-" t)))
          flashed-ws)
      (unwind-protect
          (cl-letf (((symbol-function 'projectile-switch-project-by-name)
                     (lambda (_p) nil))
                    ((symbol-function 'claude-repl--most-recent-project-file)
                     (lambda (_d) nil))
                    ((symbol-function '+workspace-current-name)
                     (lambda () "switched-ws"))
                    ((symbol-function 'force-mode-line-update)
                     (lambda (&optional _all) nil))
                    ((symbol-function 'claude-repl-flash-tab)
                     (lambda (ws &rest _) (setq flashed-ws ws))))
            (claude-repl-switch-to-project tmp-dir)
            (should (equal flashed-ws "switched-ws")))
        (delete-directory tmp-dir t)))))

(ert-deftest claude-repl-cmd-test-switch-to-project/opens-most-recent-file ()
  "switch-to-project opens the most-recent project file when it exists."
  (claude-repl-test--with-clean-state
    (let* ((tmp-dir (file-name-as-directory (make-temp-file "claude-repl-switch-" t)))
           (tmp-file (expand-file-name "hello.el" tmp-dir))
           (opened nil))
      (unwind-protect
          (progn
            (with-temp-file tmp-file (insert ";; placeholder"))
            (cl-letf (((symbol-function 'projectile-switch-project-by-name)
                       (lambda (_p) nil))
                      ((symbol-function 'claude-repl--most-recent-project-file)
                       (lambda (_d) tmp-file))
                      ((symbol-function 'find-file)
                       (lambda (f) (setq opened f)))
                      ((symbol-function '+workspace-current-name)
                       (lambda () "switched-ws"))
                      ((symbol-function 'force-mode-line-update)
                       (lambda (&optional _all) nil))
                      ((symbol-function 'claude-repl-flash-tab)
                       (lambda (&rest _) nil)))
              (claude-repl-switch-to-project tmp-dir)
              (should (equal opened tmp-file))))
        (delete-directory tmp-dir t)))))

(ert-deftest claude-repl-cmd-test-switch-to-project/skips-most-recent-when-gone ()
  "switch-to-project skips find-file when the most-recent path doesn't exist."
  (claude-repl-test--with-clean-state
    (let ((tmp-dir (file-name-as-directory (make-temp-file "claude-repl-switch-" t)))
          (find-file-called nil))
      (unwind-protect
          (cl-letf (((symbol-function 'projectile-switch-project-by-name)
                     (lambda (_p) nil))
                    ((symbol-function 'claude-repl--most-recent-project-file)
                     (lambda (_d) "/nonexistent/gone.el"))
                    ((symbol-function 'find-file)
                     (lambda (&rest _) (setq find-file-called t)))
                    ((symbol-function '+workspace-current-name)
                     (lambda () "switched-ws"))
                    ((symbol-function 'force-mode-line-update)
                     (lambda (&optional _all) nil))
                    ((symbol-function 'claude-repl-flash-tab)
                     (lambda (&rest _) nil)))
            (claude-repl-switch-to-project tmp-dir)
            (should-not find-file-called))
        (delete-directory tmp-dir t)))))

;;;; ---- claude-repl--most-recent-project-file ----

(ert-deftest claude-repl-cmd-test-most-recent-project-file/returns-first-under-root ()
  "Returns the first `recentf-list' entry that lives under PROJECT-ROOT."
  (let* ((tmp-dir (file-name-as-directory (make-temp-file "claude-repl-recent-" t)))
         (in1  (expand-file-name "a.el" tmp-dir))
         (in2  (expand-file-name "b.el" tmp-dir))
         (out  (expand-file-name "elsewhere.el" temporary-file-directory)))
    (unwind-protect
        (progn
          (with-temp-file in1 (insert ""))
          (with-temp-file in2 (insert ""))
          (with-temp-file out (insert ""))
          (let ((recentf-list (list out in1 in2)))
            (should (equal (claude-repl--most-recent-project-file tmp-dir) in1))))
      (delete-directory tmp-dir t)
      (when (file-exists-p out) (delete-file out)))))

(ert-deftest claude-repl-cmd-test-most-recent-project-file/nil-when-none-match ()
  "Returns nil when no `recentf-list' entry lives under PROJECT-ROOT."
  (let* ((tmp-dir (file-name-as-directory (make-temp-file "claude-repl-recent-" t)))
         (out     (expand-file-name "elsewhere.el" temporary-file-directory)))
    (unwind-protect
        (progn
          (with-temp-file out (insert ""))
          (let ((recentf-list (list out)))
            (should-not (claude-repl--most-recent-project-file tmp-dir))))
      (delete-directory tmp-dir t)
      (when (file-exists-p out) (delete-file out)))))

(ert-deftest claude-repl-cmd-test-most-recent-project-file/boundary-safe ()
  "Does not mis-match `/p/foo' entries against project root `/p/foo-bar'."
  (let* ((parent  (file-name-as-directory (make-temp-file "claude-repl-recent-" t)))
         (foo     (file-name-as-directory (expand-file-name "foo" parent)))
         (foo-bar (file-name-as-directory (expand-file-name "foo-bar" parent)))
         (sibling (expand-file-name "x.el" foo)))
    (unwind-protect
        (progn
          (make-directory foo)
          (make-directory foo-bar)
          (with-temp-file sibling (insert ""))
          (let ((recentf-list (list sibling)))
            (should-not (claude-repl--most-recent-project-file foo-bar))))
      (delete-directory parent t))))

;;;; ---- Tests: snapshot archive picker ----

(ert-deftest claude-repl-cmd-test-snapshot-file-ws-count/counts-entries ()
  "snapshot-file-ws-count returns the number of entries in the snapshot."
  (let ((f (make-temp-file "claude-snap-count-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file f
            (insert "((\"a\" :project-dir \"/tmp/a\") (\"b\" :project-dir \"/tmp/b\") (\"c\" :project-dir \"/tmp/c\"))"))
          (should (= 3 (claude-repl--snapshot-file-ws-count f))))
      (delete-file f))))

(ert-deftest claude-repl-cmd-test-snapshot-file-ws-count/zero-for-missing ()
  "snapshot-file-ws-count returns 0 for a missing file (graceful)."
  (should (= 0 (claude-repl--snapshot-file-ws-count "/nonexistent/snap.el"))))

(ert-deftest claude-repl-cmd-test-snapshot-candidate-label/contains-count-and-date ()
  "Candidate label embeds workspace count and a YYYY-MM-DD HH:MM mtime."
  (let ((f (make-temp-file "claude-snap-label-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file f
            (insert "((\"a\" :project-dir \"/tmp/a\") (\"b\" :project-dir \"/tmp/b\"))"))
          (let ((label (claude-repl--snapshot-candidate-label f)))
            (should (string-match-p (file-name-nondirectory f) label))
            (should (string-match-p "2ws" label))
            (should (string-match-p "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\}"
                                    label))))
      (delete-file f))))

(ert-deftest claude-repl-cmd-test-snapshot-archive-candidates/current-and-archives ()
  "snapshot-archive-candidates returns the current file and every archived file."
  (let* ((dir (file-name-as-directory (make-temp-file "claude-snap-dir-" t)))
         (current (expand-file-name "workspaces.el" dir))
         (archive-dir (expand-file-name "workspaces-archive" dir))
         (archive-a (expand-file-name "20260510T184855.el" archive-dir))
         (archive-b (expand-file-name "20260505T094316.el" archive-dir)))
    (unwind-protect
        (let ((claude-repl-workspace-snapshot-file current))
          (make-directory archive-dir t)
          (with-temp-file current (insert "((\"cur\" :project-dir \"/tmp/c\"))"))
          (with-temp-file archive-a (insert "((\"a\" :project-dir \"/tmp/a\") (\"b\" :project-dir \"/tmp/b\"))"))
          (with-temp-file archive-b (insert "((\"x\" :project-dir \"/tmp/x\"))"))
          (let* ((candidates (claude-repl--snapshot-archive-candidates))
                 (paths (mapcar #'cdr candidates)))
            (should (= 3 (length candidates)))
            (should (member current paths))
            (should (member archive-a paths))
            (should (member archive-b paths))))
      (delete-directory dir t))))

(ert-deftest claude-repl-cmd-test-snapshot-archive-candidates/archives-newest-first ()
  "Archives are sorted newest-first (lexicographic on timestamped filename)."
  (let* ((dir (file-name-as-directory (make-temp-file "claude-snap-dir-" t)))
         (current (expand-file-name "workspaces.el" dir))
         (archive-dir (expand-file-name "workspaces-archive" dir))
         (older (expand-file-name "20260101T000000.el" archive-dir))
         (newer (expand-file-name "20260601T120000.el" archive-dir)))
    (unwind-protect
        (let ((claude-repl-workspace-snapshot-file current))
          (make-directory archive-dir t)
          (with-temp-file current (insert "()"))
          (with-temp-file older (insert "()"))
          (with-temp-file newer (insert "()"))
          (let* ((candidates (claude-repl--snapshot-archive-candidates))
                 (paths (mapcar #'cdr candidates)))
            ;; current first, then newer, then older
            (should (equal paths (list current newer older)))))
      (delete-directory dir t))))

(ert-deftest claude-repl-cmd-test-load-from-archive/loads-selected-file ()
  "load-from-archive invokes loader with the selected file's path."
  (let* ((dir (file-name-as-directory (make-temp-file "claude-snap-dir-" t)))
         (current (expand-file-name "workspaces.el" dir))
         (archive-dir (expand-file-name "workspaces-archive" dir))
         (chosen-archive (expand-file-name "20260510T184855.el" archive-dir))
         loaded-file)
    (unwind-protect
        (let ((claude-repl-workspace-snapshot-file current))
          (make-directory archive-dir t)
          (with-temp-file current (insert "()"))
          (with-temp-file chosen-archive (insert "()"))
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (_prompt collection &rest _)
                       ;; pick the archive entry (second, since current is first)
                       (cl-find-if (lambda (s) (string-match-p "20260510T184855" s))
                                   collection)))
                    ((symbol-function 'claude-repl-load-workspace-snapshot)
                     (lambda (file) (setq loaded-file file))))
            (claude-repl-load-workspace-snapshot-from-archive)
            (should (equal loaded-file chosen-archive))))
      (delete-directory dir t))))

(ert-deftest claude-repl-cmd-test-load-from-archive/errors-when-no-candidates ()
  "load-from-archive signals user-error when no snapshot files exist anywhere."
  (let ((claude-repl-workspace-snapshot-file "/nonexistent/snap.el")
        (claude-repl--legacy-workspace-snapshot-file "/nonexistent/legacy.el"))
    (cl-letf (((symbol-function 'claude-repl--workspace-snapshot-archive-dir)
               (lambda () "/nonexistent/archive/")))
      (should-error (claude-repl-load-workspace-snapshot-from-archive)
                    :type 'user-error))))

;;;; ---- Tests: snapshot startup-load scheduler ----

(ert-deftest claude-repl-cmd-test-schedule-snapshot-startup-load/schedules-idle-timer ()
  "schedule-snapshot-startup-load arms an idle timer with the configured delay."
  (let ((claude-repl-snapshot-startup-load-delay 1.5)
        captured-secs captured-repeat captured-fn)
    (cl-letf (((symbol-function 'run-with-idle-timer)
               (lambda (secs repeat fn &rest _)
                 (setq captured-secs secs captured-repeat repeat captured-fn fn))))
      (claude-repl--schedule-snapshot-startup-load)
      (should (= 1.5 captured-secs))
      (should-not captured-repeat)
      (should (eq captured-fn #'claude-repl--load-workspace-snapshot-on-startup)))))

(ert-deftest claude-repl-cmd-test-schedule-snapshot-startup-load/nil-delay-disables ()
  "Setting the delay to nil disables the startup load entirely."
  (let ((claude-repl-snapshot-startup-load-delay nil)
        called)
    (cl-letf (((symbol-function 'run-with-idle-timer)
               (lambda (&rest _) (setq called t))))
      (claude-repl--schedule-snapshot-startup-load)
      (should-not called))))

(ert-deftest claude-repl-cmd-test-schedule-snapshot-startup-load/installed-on-startup-hook ()
  "The scheduler is registered on `emacs-startup-hook' (module-load wires it)."
  (should (memq #'claude-repl--schedule-snapshot-startup-load
                emacs-startup-hook)))

;;;; ---- Tests: workspace snapshot save-guard (unloaded-clobber prevention) ----

(defmacro claude-repl-cmd-test--with-temp-snapshot-file (var &rest body)
  "Bind `claude-repl-workspace-snapshot-file' to a temp path and run BODY.
VAR receives the temp file path so BODY can inspect it.  Cleans up the
file and the archive directory the save path materialises beside it."
  (declare (indent 1))
  `(let* ((,var (make-temp-file "claude-repl-snap-guard-" nil ".el"))
          (claude-repl-workspace-snapshot-file ,var)
          (claude-repl--snapshot-loaded-p nil)
          (claude-repl--snapshot-archived-this-run nil))
     (unwind-protect
         (progn ,@body)
       (when (file-exists-p ,var) (delete-file ,var))
       (let ((archive (claude-repl--workspace-snapshot-archive-dir)))
         (when (file-directory-p archive) (delete-directory archive t))))))

(ert-deftest claude-repl-cmd-test-snapshot-save-safe-p/loaded-flag-passes ()
  "Once the loaded flag is set, save is always safe regardless of disk state."
  (claude-repl-cmd-test--with-temp-snapshot-file f
    (with-temp-file f
      (insert "((\"a\" :project-dir \"/tmp/a\") (\"b\" :project-dir \"/tmp/b\") (\"c\" :project-dir \"/tmp/c\"))"))
    (setq claude-repl--snapshot-loaded-p t)
    (should (claude-repl--snapshot-save-safe-p 1))))

(ert-deftest claude-repl-cmd-test-snapshot-save-safe-p/no-disk-file-passes ()
  "When no on-disk file exists, save is safe even if loader hasn't run."
  (claude-repl-cmd-test--with-temp-snapshot-file f
    (delete-file f)
    (should (claude-repl--snapshot-save-safe-p 1))))

(ert-deftest claude-repl-cmd-test-snapshot-save-safe-p/disk-smaller-passes ()
  "When loader hasn't run but on-disk roster is no larger than live, save is safe."
  (claude-repl-cmd-test--with-temp-snapshot-file f
    (with-temp-file f
      (insert "((\"a\" :project-dir \"/tmp/a\"))"))
    (should (claude-repl--snapshot-save-safe-p 1))
    (should (claude-repl--snapshot-save-safe-p 2))))

(ert-deftest claude-repl-cmd-test-snapshot-save-safe-p/unloaded-and-disk-larger-blocks ()
  "When loader hasn't run AND on-disk roster is larger than live, save is unsafe."
  (claude-repl-cmd-test--with-temp-snapshot-file f
    (with-temp-file f
      (insert "((\"a\" :project-dir \"/tmp/a\") (\"b\" :project-dir \"/tmp/b\") (\"c\" :project-dir \"/tmp/c\"))"))
    (should-not (claude-repl--snapshot-save-safe-p 1))
    (should-not (claude-repl--snapshot-save-safe-p 2))))

(ert-deftest claude-repl-cmd-test-snapshot-save/refuses-when-unloaded-and-shrinking ()
  "Save aborts (file unchanged) when loader hasn't run and write would shrink the roster."
  (claude-repl-test--with-clean-state
    (claude-repl-cmd-test--with-temp-snapshot-file f
      (let ((seed "((\"prior-a\" :project-dir \"/tmp/a\") (\"prior-b\" :project-dir \"/tmp/b\") (\"prior-c\" :project-dir \"/tmp/c\"))"))
        (with-temp-file f (insert seed))
        (claude-repl--ws-put "only-live" :project-dir "/tmp/live")
        (claude-repl-save-workspace-snapshot)
        (with-temp-buffer
          (insert-file-contents f)
          (should (equal (buffer-string) seed)))))))

(ert-deftest claude-repl-cmd-test-snapshot-save/proceeds-after-load ()
  "Save proceeds after the loader has run, even if live roster is smaller than disk."
  (claude-repl-test--with-clean-state
    (claude-repl-cmd-test--with-temp-snapshot-file f
      (with-temp-file f
        (insert "((\"prior-a\" :project-dir \"/tmp/a\") (\"prior-b\" :project-dir \"/tmp/b\"))"))
      (setq claude-repl--snapshot-loaded-p t)
      (claude-repl--ws-put "only-live" :project-dir "/tmp/live")
      (claude-repl-save-workspace-snapshot)
      (with-temp-buffer
        (insert-file-contents f)
        (should (string-match-p "only-live" (buffer-string)))
        (should-not (string-match-p "prior-a" (buffer-string)))))))

(ert-deftest claude-repl-cmd-test-snapshot-load/sets-loaded-flag-on-success ()
  "Successful load sets `claude-repl--snapshot-loaded-p' to t."
  (claude-repl-test--with-clean-state
    (claude-repl-cmd-test--with-temp-snapshot-file f
      (let ((tmp-dir (file-name-as-directory (make-temp-file "claude-repl-snap-dir-" t))))
        (unwind-protect
            (progn
              (with-temp-file f
                (prin1 (list (list "ws-a" :project-dir tmp-dir :priority nil))
                       (current-buffer)))
              (cl-letf (((symbol-function 'claude-repl--establish-workspace) #'ignore)
                        ((symbol-function 'claude-repl--snapshot-load-ws-ready-p)
                         (lambda (_ws) t)))
                (setq claude-repl--snapshot-loaded-p nil)
                (claude-repl-load-workspace-snapshot)
                (should claude-repl--snapshot-loaded-p)))
          (delete-directory tmp-dir t))))))

;;;; ---- Workspace cycling (claude-repl-switch-left/right) ----

(defmacro claude-repl-cmd-test--with-cycle-stubs (names current hidden-set
                                                  switched-to flashed protected-p
                                                  &rest body)
  "Bind `+workspace-list-names' / `-current-name' / `-switch' / flash to
fixtures.  NAMES is a list of workspace names, CURRENT is a string,
HIDDEN-SET is a list of names whose `:repl-state' is `:hidden' (the
filter target since hide-mode reimpl moved to persp-level enforcement),
SWITCHED-TO and FLASHED are place-symbols (boxed into single-cell lists)
the stubs push to.  PROTECTED-P is a boolean controlling
`+workspace--protected-p'."
  (declare (indent 7))
  `(cl-letf (((symbol-function '+workspace-list-names) (lambda () ,names))
             ((symbol-function '+workspace-current-name) (lambda () ,current))
             ((symbol-function '+workspace--protected-p)
              (lambda (_name) ,protected-p))
             ((symbol-function 'claude-repl--ws-repl-state)
              (lambda (n) (when (member n ,hidden-set) :hidden)))
             ((symbol-function '+workspace-switch)
              (lambda (name &optional _auto-create) (push name ,switched-to)))
             ((symbol-function 'claude-repl--flash-current-tab)
              (lambda () (push t ,flashed))))
     ,@body))

(ert-deftest claude-repl-cmd-test-switch-right/cycles-to-next ()
  "switch-right with hide-mode off cycles to the next workspace."
  (let ((switched (list)) (flashed (list))
        (claude-repl-hide-mode-enabled nil))
    (claude-repl-cmd-test--with-cycle-stubs
        '("a" "b" "c") "a" '() switched flashed nil
      (claude-repl-switch-right)
      (should (equal switched '("b"))))))

(ert-deftest claude-repl-cmd-test-switch-left/cycles-to-prev ()
  "switch-left with hide-mode off cycles to the previous workspace."
  (let ((switched (list)) (flashed (list))
        (claude-repl-hide-mode-enabled nil))
    (claude-repl-cmd-test--with-cycle-stubs
        '("a" "b" "c") "b" '() switched flashed nil
      (claude-repl-switch-left)
      (should (equal switched '("a"))))))

(ert-deftest claude-repl-cmd-test-switch-right/wraps-around ()
  "switch-right from the last workspace wraps to the first."
  (let ((switched (list)) (flashed (list))
        (claude-repl-hide-mode-enabled nil))
    (claude-repl-cmd-test--with-cycle-stubs
        '("a" "b" "c") "c" '() switched flashed nil
      (claude-repl-switch-right)
      (should (equal switched '("a"))))))

(ert-deftest claude-repl-cmd-test-switch-left/wraps-around ()
  "switch-left from the first workspace wraps to the last."
  (let ((switched (list)) (flashed (list))
        (claude-repl-hide-mode-enabled nil))
    (claude-repl-cmd-test--with-cycle-stubs
        '("a" "b" "c") "a" '() switched flashed nil
      (claude-repl-switch-left)
      (should (equal switched '("c"))))))

(ert-deftest claude-repl-cmd-test-switch-right/skips-hidden-when-hide-on ()
  "With hide-mode on, switch-right skips workspaces whose `:repl-state' is `:hidden'."
  (let ((switched (list)) (flashed (list))
        (claude-repl-hide-mode-enabled t))
    (claude-repl-cmd-test--with-cycle-stubs
        '("a" "b" "c") "a" '("b") switched flashed nil
      (claude-repl-switch-right)
      (should (equal switched '("c"))))))

(ert-deftest claude-repl-cmd-test-switch-left/skips-hidden-when-hide-on ()
  "With hide-mode on, switch-left skips workspaces whose `:repl-state' is `:hidden'."
  (let ((switched (list)) (flashed (list))
        (claude-repl-hide-mode-enabled t))
    (claude-repl-cmd-test--with-cycle-stubs
        '("a" "b" "c") "c" '("b") switched flashed nil
      (claude-repl-switch-left)
      (should (equal switched '("a"))))))

(ert-deftest claude-repl-cmd-test-switch-right/single-visible-no-op ()
  "When only the current workspace is visible, switch-right does not switch."
  (let ((switched (list)) (flashed (list))
        (claude-repl-hide-mode-enabled t)
        ;; condition-case-unless-debug skips its handlers when
        ;; `debug-on-error' is set, which ert turns on by default.  Bind
        ;; it off so the user-error path is observable in tests.
        (debug-on-error nil))
    (claude-repl-cmd-test--with-cycle-stubs
        '("a" "b" "c") "a" '("b" "c") switched flashed nil
      (cl-letf (((symbol-function '+workspace-error)
                 (lambda (&rest _) nil)))
        (claude-repl-switch-right)
        (should-not switched)
        (should-not flashed)))))

(ert-deftest claude-repl-cmd-test-switch-right/protected-goes-to-main ()
  "When current workspace is protected, switch-right routes to +workspaces-main."
  (let ((switched (list)) (flashed (list))
        (claude-repl-hide-mode-enabled nil)
        (+workspaces-main "main"))
    (claude-repl-cmd-test--with-cycle-stubs
        '("nil") "nil" '() switched flashed t
      (claude-repl-switch-right)
      (should (equal switched '("main")))
      (should-not flashed))))

(ert-deftest claude-repl-cmd-test-switch-right/does-not-flash-destination ()
  "switch-right does NOT flash the destination tab.
Left/right cycling is high-frequency navigation and the flash becomes
noise; only identity-based jumps (`SPC p p', priority change,
worktree jump) flash."
  (let ((switched (list)) (flashed (list))
        (claude-repl-hide-mode-enabled nil))
    (claude-repl-cmd-test--with-cycle-stubs
        '("a" "b") "a" '() switched flashed nil
      (claude-repl-switch-right)
      (should-not flashed))))

;;;; ---- Hide-mode sweep ----

(defmacro claude-repl-cmd-test--with-sweep-stubs (current killed &rest body)
  "Stub `+workspace-current-name' to return CURRENT and replace
`claude-repl--nuke-one-workspace' with a recorder that pushes the named
ws onto KILLED (a place-symbol bound to a list)."
  (declare (indent 2))
  `(cl-letf (((symbol-function '+workspace-current-name) (lambda () ,current))
             ((symbol-function 'claude-repl--nuke-one-workspace)
              (lambda (ws &rest _) (push ws ,killed))))
     ,@body))

(ert-deftest claude-repl-cmd-test-sweep-hidden/kills-non-current-hidden ()
  "sweep-hidden-workspaces persp-kills every :hidden ws except the current one."
  (claude-repl-test--with-clean-state
    (let ((killed (list)))
      (claude-repl--ws-set-repl-state "ws-a" :hidden)
      (claude-repl--ws-set-repl-state "ws-b" :hidden)
      (claude-repl--ws-set-repl-state "ws-c" :inactive)
      (claude-repl-cmd-test--with-sweep-stubs "ws-c" killed
        (claude-repl--sweep-hidden-workspaces)
        (should (equal (sort killed #'string<) '("ws-a" "ws-b")))))))

(ert-deftest claude-repl-cmd-test-sweep-hidden/skips-current ()
  "sweep-hidden-workspaces never kills the current workspace, even if hidden."
  (claude-repl-test--with-clean-state
    (let ((killed (list)))
      (claude-repl--ws-set-repl-state "ws-a" :hidden)
      (claude-repl-cmd-test--with-sweep-stubs "ws-a" killed
        (claude-repl--sweep-hidden-workspaces)
        (should (null killed))))))

(ert-deftest claude-repl-cmd-test-sweep-hidden/skips-non-hidden ()
  "sweep-hidden-workspaces ignores workspaces with non-:hidden states."
  (claude-repl-test--with-clean-state
    (let ((killed (list)))
      (claude-repl--ws-set-repl-state "ws-a" :inactive)
      (claude-repl--ws-set-repl-state "ws-b" :active)
      (claude-repl--ws-set-repl-state "ws-c" :viewed)
      (claude-repl-cmd-test--with-sweep-stubs "ws-c" killed
        (claude-repl--sweep-hidden-workspaces)
        (should (null killed))))))

(ert-deftest claude-repl-cmd-test-sweep-hidden/forwards-to-nuke ()
  "sweep-hidden-workspaces calls nuke-one-workspace for each `:hidden' ws.
nuke-one-workspace always preserves the on-disk state file, so there's
no explicit purge flag to assert — just that nuke was called with the
right ws name."
  (claude-repl-test--with-clean-state
    (let ((received-args nil))
      (claude-repl--ws-set-repl-state "ws-a" :hidden)
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws-c"))
                ((symbol-function 'claude-repl--nuke-one-workspace)
                 (lambda (&rest args) (setq received-args args))))
        (claude-repl--sweep-hidden-workspaces)
        (should (equal received-args '("ws-a")))))))

(ert-deftest claude-repl-cmd-test-sweep-hidden/except-overrides-current ()
  "Explicit EXCEPT arg takes precedence over `+workspace-current-name'."
  (claude-repl-test--with-clean-state
    (let ((killed (list)))
      (claude-repl--ws-set-repl-state "ws-a" :hidden)
      (claude-repl--ws-set-repl-state "ws-b" :hidden)
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws-c"))
                ((symbol-function 'claude-repl--nuke-one-workspace)
                 (lambda (ws &rest _) (push ws killed))))
        ;; EXCEPT="ws-a" should keep ws-a alive even though current is ws-c.
        (claude-repl--sweep-hidden-workspaces "ws-a")
        (should (equal killed '("ws-b")))))))

;;;; ---- maybe-sweep-hidden-on-switch ----

(ert-deftest claude-repl-cmd-test-maybe-sweep/runs-when-hide-on ()
  "maybe-sweep-hidden-on-switch runs the sweep when hide-mode is enabled."
  (claude-repl-test--with-clean-state
    (let ((sweep-called 0)
          (claude-repl-hide-mode-enabled t))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws-c"))
                ((symbol-function 'claude-repl--sweep-hidden-workspaces)
                 (lambda (&rest _) (cl-incf sweep-called))))
        (claude-repl--maybe-sweep-hidden-on-switch)
        (should (= sweep-called 1))))))

(ert-deftest claude-repl-cmd-test-maybe-sweep/skips-when-hide-off ()
  "maybe-sweep-hidden-on-switch is a no-op when hide-mode is disabled."
  (claude-repl-test--with-clean-state
    (let ((sweep-called 0)
          (claude-repl-hide-mode-enabled nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws-c"))
                ((symbol-function 'claude-repl--sweep-hidden-workspaces)
                 (lambda (&rest _) (cl-incf sweep-called))))
        (claude-repl--maybe-sweep-hidden-on-switch)
        (should (= sweep-called 0))))))

(ert-deftest claude-repl-cmd-test-maybe-sweep/resets-arrived-hidden-to-inactive ()
  "Arriving on a `:hidden' workspace resets it to `:inactive' so the user
actively viewing it does not get it killed.  Independent of hide-mode flag."
  (claude-repl-test--with-clean-state
    (let ((claude-repl-hide-mode-enabled nil))
      (claude-repl--ws-set-repl-state "ws-a" :hidden)
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws-a"))
                ((symbol-function 'claude-repl--sweep-hidden-workspaces)
                 (lambda (&rest _) nil)))
        (claude-repl--maybe-sweep-hidden-on-switch)
        (should (eq (claude-repl--ws-repl-state "ws-a") :inactive))))))

(ert-deftest claude-repl-cmd-test-maybe-sweep/leaves-non-hidden-current-alone ()
  "maybe-sweep-hidden-on-switch does not touch repl-state if current is not :hidden."
  (claude-repl-test--with-clean-state
    (let ((claude-repl-hide-mode-enabled nil))
      (claude-repl--ws-set-repl-state "ws-a" :active)
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws-a"))
                ((symbol-function 'claude-repl--sweep-hidden-workspaces)
                 (lambda (&rest _) nil)))
        (claude-repl--maybe-sweep-hidden-on-switch)
        (should (eq (claude-repl--ws-repl-state "ws-a") :active))))))

(ert-deftest claude-repl-cmd-test-maybe-sweep/explicit-ws-overrides-current ()
  "An explicit WS argument takes precedence over `+workspace-current-name'.
This is how `--on-workspace-switch' passes the ws captured at
hook-fire time, so the reset/sweep operate on the workspace that was
just switched to even if another switch raced ahead first."
  (claude-repl-test--with-clean-state
    (let ((swept-with nil)
          (claude-repl-hide-mode-enabled t))
      (claude-repl--ws-set-repl-state "ws-a" :hidden)
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws-c"))
                ((symbol-function 'claude-repl--sweep-hidden-workspaces)
                 (lambda (&optional except) (setq swept-with except))))
        (claude-repl--maybe-sweep-hidden-on-switch "ws-a")
        ;; ws-a was reset because it was the explicit arg, even though
        ;; +workspace-current-name returns "ws-c".
        (should (eq (claude-repl--ws-repl-state "ws-a") :inactive))
        (should (equal swept-with "ws-a"))))))

(provide 'test-commands)

;;; test-commands.el ends here
