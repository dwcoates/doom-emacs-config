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
            ((symbol-function 'claude-repl--resolve-root)
             (lambda () "/project/")))
    (should (equal (claude-repl--buffer-relative-path) "src/foo.el"))))

(ert-deftest claude-repl-cmd-test-buffer-relative-path/nested-subdir ()
  "buffer-relative-path works for deeply nested paths."
  (cl-letf (((symbol-function 'buffer-file-name)
             (lambda (&optional _buf) "/project/a/b/c/deep.el"))
            ((symbol-function 'claude-repl--resolve-root)
             (lambda () "/project/")))
    (should (equal (claude-repl--buffer-relative-path) "a/b/c/deep.el"))))

(ert-deftest claude-repl-cmd-test-buffer-relative-path/file-at-root ()
  "buffer-relative-path returns bare filename when file is at project root."
  (cl-letf (((symbol-function 'buffer-file-name)
             (lambda (&optional _buf) "/project/file.el"))
            ((symbol-function 'claude-repl--resolve-root)
             (lambda () "/project/")))
    (should (equal (claude-repl--buffer-relative-path) "file.el"))))

(ert-deftest claude-repl-cmd-test-buffer-relative-path/root-without-trailing-slash ()
  "buffer-relative-path works when resolve-root omits trailing slash."
  (cl-letf (((symbol-function 'buffer-file-name)
             (lambda (&optional _buf) "/project/src/bar.el"))
            ((symbol-function 'claude-repl--resolve-root)
             (lambda () "/project")))
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
              ((symbol-function 'claude-repl--resolve-root)
               (lambda () "/project/")))
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
              ((symbol-function 'claude-repl--resolve-root)
               (lambda () "/repo/")))
      (should (equal (claude-repl--format-magit-hunk-ref) "file.py:25-25")))))

(ert-deftest claude-repl-cmd-test-format-magit-hunk-ref/different-roots ()
  "format-magit-hunk-ref computes relative path from resolve-root, not magit-toplevel."
  (let ((mock-section (record 'magit-section nil nil nil nil nil nil nil nil)))
    (cl-letf (((symbol-function 'magit-current-section)
               (lambda () mock-section))
              ((symbol-function 'magit-file-at-point)
               (lambda () "subdir/file.rs"))
              ((symbol-function 'eieio-oref)
               (lambda (_obj _slot) '(1 3)))
              ((symbol-function 'magit-toplevel)
               (lambda () "/workspace/project/"))
              ((symbol-function 'claude-repl--resolve-root)
               (lambda () "/workspace/project/")))
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

(ert-deftest claude-repl-cmd-test-send-to-claude/calls-ensure-and-send ()
  "send-to-claude ensures session and sends text to vterm buffer."
  (let (ensured-p sent-buf sent-text)
    (claude-repl-test--with-clean-state
      (let ((fake-vterm-buf (get-buffer-create " *test-vterm*")))
        (unwind-protect
            (progn
              (claude-repl--ws-put "test-ws" :vterm-buffer fake-vterm-buf)
              (cl-letf (((symbol-function 'claude-repl--ensure-session)
                         (lambda () (setq ensured-p t)))
                        ((symbol-function '+workspace-current-name)
                         (lambda () "test-ws"))
                        ((symbol-function 'claude-repl--send-input-to-vterm)
                         (lambda (buf text)
                           (setq sent-buf buf sent-text text))))
                (claude-repl--send-to-claude "hello claude")
                (should ensured-p)
                (should (eq sent-buf fake-vterm-buf))
                (should (equal sent-text "hello claude"))))
          (kill-buffer fake-vterm-buf))))))

;;;; ---- claude-repl--ensure-session ----

(ert-deftest claude-repl-cmd-test-ensure-session/no-workspace-signals-error ()
  "ensure-session errors when no workspace is active."
  (cl-letf (((symbol-function '+workspace-current-name)
             (lambda () nil)))
    (should-error (claude-repl--ensure-session) :type 'error)))

(ert-deftest claude-repl-cmd-test-ensure-session/already-running-is-noop ()
  "ensure-session is a no-op when vterm is already running."
  (let (vterm-created-p)
    (claude-repl-test--with-clean-state
      (cl-letf (((symbol-function '+workspace-current-name)
                 (lambda () "test-ws"))
                ((symbol-function 'claude-repl--vterm-running-p)
                 (lambda (_ws) t))
                ((symbol-function 'claude-repl--ensure-vterm-buffer)
                 (lambda (_ws) (setq vterm-created-p t))))
        (claude-repl--ensure-session)
        (should-not vterm-created-p)))))

(ert-deftest claude-repl-cmd-test-ensure-session/starts-new-session ()
  "ensure-session creates vterm, input buffer, sets counter, enables overlay."
  (let (vterm-ensured input-ensured overlay-enabled counter-set)
    (claude-repl-test--with-clean-state
      (cl-letf (((symbol-function '+workspace-current-name)
                 (lambda () "test-ws"))
                ((symbol-function 'claude-repl--vterm-running-p)
                 (lambda (_ws) nil))
                ((symbol-function 'claude-repl--ensure-vterm-buffer)
                 (lambda (_ws) (setq vterm-ensured t)))
                ((symbol-function 'claude-repl--ensure-input-buffer)
                 (lambda (_ws) (setq input-ensured t)))
                ((symbol-function 'claude-repl--ws-put)
                 (lambda (ws key val)
                   (when (and (equal ws "test-ws") (eq key :prefix-counter) (= val 0))
                     (setq counter-set t))))
                ((symbol-function 'claude-repl--enable-hide-overlay)
                 (lambda () (setq overlay-enabled t))))
        (claude-repl--ensure-session "test-ws")
        (should vterm-ensured)
        (should input-ensured)
        (should counter-set)
        (should overlay-enabled)))))

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
  "nuke-workspace kills session, deletes persp, and removes hashmap entry."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "doomed" :project-dir "/tmp/doomed")
    (claude-repl--ws-put "doomed" :status :done)
    (let ((session-killed nil)
          (persp-deleted nil)
          (persp-mode t))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt _coll &rest _) "doomed"))
                ((symbol-function 'y-or-n-p) (lambda (_prompt) t))
                ((symbol-function 'claude-repl--kill-session)
                 (lambda (ws) (setq session-killed ws)))
                ((symbol-function '+workspace-current-name) (lambda () "other-ws"))
                ((symbol-function 'persp-get-by-name) (lambda (_n) t))
                ((symbol-function '+workspace/delete)
                 (lambda (ws) (setq persp-deleted ws)))
                ((symbol-function 'force-mode-line-update) #'ignore))
        (claude-repl-nuke-workspace)
        (should (equal session-killed "doomed"))
        (should (equal persp-deleted "doomed"))
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
                ((symbol-function 'delete-process)
                 (lambda (p) (setq proc-deleted p))))
        (claude-repl-nuke-workspace)
        (should proc-deleted)))))

(ert-deftest claude-repl-cmd-test-nuke-workspace/switches-away-if-current ()
  "nuke-workspace switches to another workspace before deleting if target is current."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "doomed" :project-dir "/tmp/doomed")
    (let ((switched nil)
          (persp-mode t))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt _coll &rest _) "doomed"))
                ((symbol-function 'y-or-n-p) (lambda (_prompt) t))
                ((symbol-function 'claude-repl--kill-session) #'ignore)
                ((symbol-function '+workspace-current-name) (lambda () "doomed"))
                ((symbol-function '+workspace/other) (lambda () (setq switched t)))
                ((symbol-function 'persp-get-by-name) (lambda (_n) t))
                ((symbol-function '+workspace/delete) #'ignore)
                ((symbol-function 'force-mode-line-update) #'ignore))
        (claude-repl-nuke-workspace)
        (should switched)))))

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

(provide 'test-commands)

;;; test-commands.el ends here
