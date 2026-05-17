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

;;;; ---- claude-repl-explain-config ----

(ert-deftest claude-repl-cmd-test-explain-config-build-input/embeds-raw-prompt ()
  "build-input embeds the raw prompt verbatim in the rendered output."
  (let ((rendered (claude-repl--explain-config-build-input
                   "what does SPC j RET do?")))
    (should (string-match-p "what does SPC j RET do\\?" rendered))))

(ert-deftest claude-repl-cmd-test-explain-config-build-input/preamble-marks-read-only ()
  "Preamble declares the entry point READ-ONLY and forbids mutation."
  (let ((rendered (claude-repl--explain-config-build-input "anything")))
    (should (string-match-p "READ-ONLY" rendered))
    (should (string-match-p "MUST NOT" rendered))
    (should (string-match-p "FORBIDDEN" rendered))))

(ert-deftest claude-repl-cmd-test-explain-config-build-input/preamble-instructs-refusal-on-misuse ()
  "Preamble must instruct the model to REFUSE misuse attempts."
  (let ((rendered (claude-repl--explain-config-build-input "anything")))
    (should (string-match-p "REFUSE" rendered))))

(ert-deftest claude-repl-cmd-test-explain-config-flags/contains-headless-and-skip-perms ()
  "Default flags include `-p' and `--dangerously-skip-permissions'."
  (should (member "-p" claude-repl-explain-config-flags))
  (should (member "--dangerously-skip-permissions"
                  claude-repl-explain-config-flags)))

(ert-deftest claude-repl-cmd-test-explain-config-flags/pins-haiku-model ()
  "Default flags pass `--model haiku' so the read-only Q&A run uses the
small, fast model rather than the default-tier one (explain-config is
short-form Q&A — no need for the bigger model)."
  (should (member "--model" claude-repl-explain-config-flags))
  (let* ((tail (cdr (member "--model" claude-repl-explain-config-flags))))
    (should (equal (car tail) "haiku"))))

(ert-deftest claude-repl-cmd-test-explain-config-spawn/passes-haiku-model-to-process ()
  "spawn forwards `--model haiku' through to `make-process' so the headless
claude actually runs on haiku — guards against a future flag-list edit
that drops the model pin without surfacing in the defcustom-shape test."
  (let (captured-cmd)
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest args)
                 (setq captured-cmd (plist-get args :command))
                 'fake-proc))
              ((symbol-function 'process-send-string) #'ignore)
              ((symbol-function 'process-send-eof) #'ignore)
              ((symbol-function 'display-buffer) #'ignore))
      (claude-repl--explain-config-spawn "anything")
      (should (member "--model" captured-cmd))
      (let ((tail (cdr (member "--model" captured-cmd))))
        (should (equal (car tail) "haiku"))))))

(ert-deftest claude-repl-cmd-test-explain-config-dir/defaults-to-doom-config ()
  "Default dir resolves to `~/.config/doom' (the canonical doom config)."
  (should (equal (expand-file-name claude-repl-explain-config-dir)
                 (expand-file-name "~/.config/doom"))))

(ert-deftest claude-repl-cmd-test-explain-config/empty-prompt-errors ()
  "explain-config signals user-error for an empty prompt."
  (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws"))
            ((symbol-function 'claude-repl--explain-config-spawn)
             (lambda (_p) 'fake-proc)))
    (should-error (claude-repl-explain-config "") :type 'user-error)))

(ert-deftest claude-repl-cmd-test-explain-config/whitespace-prompt-errors ()
  "explain-config signals user-error for a whitespace-only prompt."
  (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws"))
            ((symbol-function 'claude-repl--explain-config-spawn)
             (lambda (_p) 'fake-proc)))
    (should-error (claude-repl-explain-config "   \n\t  ") :type 'user-error)))

(ert-deftest claude-repl-cmd-test-explain-config/forwards-trimmed-prompt ()
  "explain-config trims surrounding whitespace before forwarding to spawn."
  (let (captured)
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws"))
              ((symbol-function 'claude-repl--explain-config-spawn)
               (lambda (p) (setq captured p) 'fake-proc)))
      (claude-repl-explain-config "  hello  ")
      (should (equal captured "hello")))))

(ert-deftest claude-repl-cmd-test-explain-config-spawn/builds-command-from-defcustoms ()
  "spawn constructs the command list from program + flags defcustoms."
  (let (captured-cmd)
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest args)
                 (setq captured-cmd (plist-get args :command))
                 'fake-proc))
              ((symbol-function 'process-send-string) #'ignore)
              ((symbol-function 'process-send-eof) #'ignore)
              ((symbol-function 'display-buffer) #'ignore))
      (claude-repl--explain-config-spawn "anything")
      (should (equal (car captured-cmd) claude-repl-explain-config-program))
      (should (member "-p" captured-cmd))
      (should (member "--dangerously-skip-permissions" captured-cmd)))))

(ert-deftest claude-repl-cmd-test-explain-config-spawn/runs-in-configured-dir ()
  "spawn sets `default-directory' to the configured dir (with trailing slash)."
  (let (captured-dir)
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest _args)
                 (setq captured-dir default-directory)
                 'fake-proc))
              ((symbol-function 'process-send-string) #'ignore)
              ((symbol-function 'process-send-eof) #'ignore)
              ((symbol-function 'display-buffer) #'ignore))
      (claude-repl--explain-config-spawn "anything")
      (should (equal captured-dir
                     (file-name-as-directory
                      (expand-file-name claude-repl-explain-config-dir)))))))

(ert-deftest claude-repl-cmd-test-explain-config-spawn/sends-wrapped-input-on-stdin ()
  "spawn sends the preamble-wrapped prompt to the process's stdin."
  (let (captured-input)
    (cl-letf (((symbol-function 'make-process) (lambda (&rest _args) 'fake-proc))
              ((symbol-function 'process-send-string)
               (lambda (_proc s) (setq captured-input s)))
              ((symbol-function 'process-send-eof) #'ignore)
              ((symbol-function 'display-buffer) #'ignore))
      (claude-repl--explain-config-spawn "what does foo do?")
      (should (string-match-p "what does foo do\\?" captured-input))
      (should (string-match-p "READ-ONLY" captured-input)))))

(ert-deftest claude-repl-cmd-test-explain-config-init-buffer/erases-and-headers ()
  "init-buffer erases prior contents and inserts a Question header."
  (let* ((claude-repl-explain-config-buffer-name " *test-explain-config*")
         (buf (get-buffer-create claude-repl-explain-config-buffer-name)))
    (unwind-protect
        (progn
          (with-current-buffer buf (insert "stale content"))
          (claude-repl--explain-config-init-buffer "the question")
          (with-current-buffer buf
            (should-not (string-match-p "stale content" (buffer-string)))
            (should (string-match-p "Question: the question" (buffer-string)))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest claude-repl-cmd-test-explain-config-format-header/includes-emoji-and-question ()
  "Header banner includes the robot emoji, question mark emoji, and the prompt."
  (let ((header (claude-repl--explain-config-format-header "the question")))
    (should (string-match-p "🤖" header))
    (should (string-match-p "❓" header))
    (should (string-match-p "📜" header))
    (should (string-match-p "Question: the question" header))))

(ert-deftest claude-repl-cmd-test-explain-config-format-header/applies-face-properties ()
  "Header text carries face properties so the buffer renders in color."
  (let* ((header (claude-repl--explain-config-format-header "anything"))
         (q-pos (string-match "Question:" header))
         (face (and q-pos (get-text-property q-pos 'face header))))
    (should q-pos)
    (should face)
    (should (eq (plist-get face :weight) 'bold))))

(ert-deftest claude-repl-cmd-test-explain-config-format-header/box-drawing-rule ()
  "Header uses unicode box-drawing characters for the top rule, not ASCII dashes."
  (let ((header (claude-repl--explain-config-format-header "anything")))
    (should (string-match-p "━" header))
    (should-not (string-match-p "^--- response" header))))

(ert-deftest claude-repl-cmd-test-explain-config-format-footer/success-uses-check-emoji ()
  "Footer for status=0 includes the check emoji and a success phrase."
  (let ((footer (claude-repl--explain-config-format-footer 0)))
    (should (string-match-p "✅" footer))
    (should (string-match-p "exited cleanly" footer))
    (should (string-match-p "status 0" footer))))

(ert-deftest claude-repl-cmd-test-explain-config-format-footer/failure-uses-cross-emoji ()
  "Footer for non-zero status uses the cross emoji and reports the code."
  (let ((footer (claude-repl--explain-config-format-footer 137)))
    (should (string-match-p "❌" footer))
    (should (string-match-p "exited with errors" footer))
    (should (string-match-p "status 137" footer))))

(ert-deftest claude-repl-cmd-test-explain-config-format-footer/success-and-failure-have-distinct-colors ()
  "Success and failure footers carry distinct foreground colors."
  (let* ((ok (claude-repl--explain-config-format-footer 0))
         (bad (claude-repl--explain-config-format-footer 1))
         (ok-pos (string-match "claude" ok))
         (bad-pos (string-match "claude" bad))
         (ok-face (get-text-property ok-pos 'face ok))
         (bad-face (get-text-property bad-pos 'face bad)))
    (should (plist-get ok-face :foreground))
    (should (plist-get bad-face :foreground))
    (should-not (equal (plist-get ok-face :foreground)
                       (plist-get bad-face :foreground)))))

(ert-deftest claude-repl-cmd-test-explain-config-sentinel/inserts-rich-footer-on-exit ()
  "Sentinel inserts the rich footer (emoji + status) when the process exits."
  (let* ((claude-repl-explain-config-buffer-name " *test-explain-config-footer*")
         (buf (get-buffer-create claude-repl-explain-config-buffer-name))
         (proc (make-symbol "fake-proc")))
    (unwind-protect
        (cl-letf (((symbol-function 'process-status) (lambda (_p) 'exit))
                  ((symbol-function 'process-exit-status) (lambda (_p) 0))
                  ((symbol-function 'process-buffer) (lambda (_p) buf)))
          (claude-repl--explain-config-sentinel proc "finished\n")
          (with-current-buffer buf
            (should (string-match-p "✅" (buffer-string)))
            (should (string-match-p "status 0" (buffer-string)))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

;;;; ---- explain-config as a right-side popup ----

(ert-deftest claude-repl-cmd-test-explain-config-display-action/uses-right-side-window ()
  "Display action routes the explain-config buffer to a right-side window."
  (should (member 'display-buffer-in-side-window
                  (car claude-repl--explain-config-display-action)))
  (should (eq (cdr (assq 'side claude-repl--explain-config-display-action))
              'right)))

(ert-deftest claude-repl-cmd-test-explain-config-display-action/sits-at-right-slot-0 ()
  "Explain-config window lives at slot 0 on the right side."
  (should (= (cdr (assq 'slot claude-repl--explain-config-display-action))
             0)))

(ert-deftest claude-repl-cmd-test-explain-config-display-action/configures-window-width ()
  "Display action specifies `window-width' (own width config), not `window-height'."
  (should (assq 'window-width claude-repl--explain-config-display-action))
  (should-not (assq 'window-height claude-repl--explain-config-display-action)))

(ert-deftest claude-repl-cmd-test-explain-config-width-fraction/defcustom-defaults-half-frame ()
  "Width-fraction defcustom exists and defaults to half the frame width."
  (should (boundp 'claude-repl-explain-config-width-fraction))
  (should (floatp claude-repl-explain-config-width-fraction))
  (should (= claude-repl-explain-config-width-fraction 0.5)))

(ert-deftest claude-repl-cmd-test-explain-config-window-width/computes-from-fraction ()
  "`--window-width' returns round(fraction × frame-width) for any window."
  (let ((claude-repl-explain-config-width-fraction 0.5))
    (cl-letf (((symbol-function 'window-frame) (lambda (_w) 'fake-frame))
              ((symbol-function 'frame-width) (lambda (_f) 200)))
      (should (= (claude-repl--explain-config-window-width 'fake-win) 100)))))

(ert-deftest claude-repl-cmd-test-explain-config-window-width/clamps-floor-at-1 ()
  "`--window-width' floors at 1 col even when fraction × width rounds to 0."
  (let ((claude-repl-explain-config-width-fraction 0.001))
    (cl-letf (((symbol-function 'window-frame) (lambda (_w) 'fake-frame))
              ((symbol-function 'frame-width) (lambda (_f) 40)))
      (should (>= (claude-repl--explain-config-window-width 'fake-win) 1)))))

(ert-deftest claude-repl-cmd-test-explain-config-show/no-op-when-buffer-missing ()
  "Show is a no-op when no explain-config buffer has ever been created."
  (let ((claude-repl-explain-config-buffer-name " *nonexistent-explain-config*")
        (claude-repl--explain-config-global-visible-p nil)
        (display-called nil))
    (cl-letf (((symbol-function 'display-buffer)
               (lambda (&rest _) (setq display-called t) nil)))
      (claude-repl--explain-config-show)
      (should-not display-called)
      ;; Flag stays nil when buffer doesn't exist — nothing to show.
      (should-not claude-repl--explain-config-global-visible-p))))

(ert-deftest claude-repl-cmd-test-explain-config-show/displays-existing-buffer ()
  "Show falls back to `display-buffer' with the side-window action when the
claude output window is not available to take over."
  (let* ((claude-repl-explain-config-buffer-name " *test-explain-show*")
         (buf (get-buffer-create claude-repl-explain-config-buffer-name))
         (claude-repl--explain-config-global-visible-p nil)
         (claude-repl--explain-config-replaced-window nil)
         (display-args nil))
    (unwind-protect
        (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) nil))
                  ((symbol-function 'claude-repl--explain-config-current-claude-output-window)
                   (lambda () nil))
                  ((symbol-function 'display-buffer)
                   (lambda (b action) (setq display-args (list b action)) nil))
                  ((symbol-function 'claude-repl--explain-config-apply-width) #'ignore))
          (claude-repl--explain-config-show)
          (should (eq (car display-args) buf))
          (should (eq (cadr display-args)
                      claude-repl--explain-config-display-action)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest claude-repl-cmd-test-explain-config-show/sets-global-visible-flag ()
  "Show sets `--global-visible-p' so the popup follows the user across persps."
  (let* ((claude-repl-explain-config-buffer-name " *test-explain-flag-set*")
         (buf (get-buffer-create claude-repl-explain-config-buffer-name))
         (claude-repl--explain-config-global-visible-p nil)
         (claude-repl--explain-config-replaced-window nil))
    (unwind-protect
        (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) nil))
                  ((symbol-function 'claude-repl--explain-config-current-claude-output-window)
                   (lambda () nil))
                  ((symbol-function 'display-buffer) (lambda (&rest _) nil))
                  ((symbol-function 'claude-repl--explain-config-apply-width) #'ignore))
          (claude-repl--explain-config-show)
          (should claude-repl--explain-config-global-visible-p))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest claude-repl-cmd-test-explain-config-show/reapplies-width-when-visible ()
  "Show reapplies the configured width when a side-window already displays the buffer."
  (let* ((claude-repl-explain-config-buffer-name " *test-explain-reapply*")
         (buf (get-buffer-create claude-repl-explain-config-buffer-name))
         (claude-repl--explain-config-global-visible-p t)
         (claude-repl--explain-config-replaced-window nil)
         (width-applied-with nil))
    (unwind-protect
        (cl-letf (((symbol-function 'get-buffer-window)
                   (lambda (&rest _) 'fake-win))
                  ((symbol-function 'window-live-p) (lambda (_w) t))
                  ((symbol-function 'display-buffer)
                   (lambda (&rest _) (error "should not redisplay")))
                  ((symbol-function 'claude-repl--explain-config-apply-width)
                   (lambda (w) (setq width-applied-with w))))
          (claude-repl--explain-config-show)
          (should (eq width-applied-with 'fake-win)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

;;;; ---- Drawer decoupling: --show must never touch the drawer ----

(ert-deftest claude-repl-cmd-test-explain-config-show/does-not-call-drawer-hide ()
  "Show must NOT call `claude-repl-drawer-hide' regardless of drawer state —
the popup and drawer are fully decoupled."
  (let* ((claude-repl-explain-config-buffer-name " *test-explain-no-drawer-hide*")
         (buf (get-buffer-create claude-repl-explain-config-buffer-name))
         (claude-repl--explain-config-global-visible-p nil)
         (claude-repl--explain-config-replaced-window nil)
         (claude-repl-drawer--global-visible-p t)
         (drawer-hide-called nil))
    (unwind-protect
        (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) nil))
                  ((symbol-function 'claude-repl--explain-config-current-claude-output-window)
                   (lambda () nil))
                  ((symbol-function 'display-buffer) (lambda (&rest _) nil))
                  ((symbol-function 'claude-repl--explain-config-apply-width) #'ignore)
                  ((symbol-function 'claude-repl-drawer-hide)
                   (lambda () (setq drawer-hide-called t))))
          (claude-repl--explain-config-show)
          (should-not drawer-hide-called))
      (when (buffer-live-p buf) (kill-buffer buf)))))

;;;; ---- Claude output window takeover ----

(ert-deftest claude-repl-cmd-test-explain-config-show/takes-over-claude-output-window-when-visible ()
  "Show reuses the live claude output window via `set-window-buffer' rather
than falling back to the side-window display action."
  (let* ((claude-repl-explain-config-buffer-name " *test-explain-takeover*")
         (buf (get-buffer-create claude-repl-explain-config-buffer-name))
         (claude-repl--explain-config-global-visible-p nil)
         (claude-repl--explain-config-replaced-window nil)
         (set-window-buffer-args nil)
         (display-buffer-called nil))
    (unwind-protect
        (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) nil))
                  ((symbol-function 'claude-repl--explain-config-current-claude-output-window)
                   (lambda () 'fake-output-win))
                  ((symbol-function 'window-buffer) (lambda (_w) 'prev-buf))
                  ((symbol-function 'set-window-dedicated-p) #'ignore)
                  ((symbol-function 'set-window-buffer)
                   (lambda (w b) (setq set-window-buffer-args (list w b))))
                  ((symbol-function 'display-buffer)
                   (lambda (&rest _) (setq display-buffer-called t) nil)))
          (claude-repl--explain-config-show)
          (should (equal set-window-buffer-args (list 'fake-output-win buf)))
          (should-not display-buffer-called))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest claude-repl-cmd-test-explain-config-show/records-replaced-window-on-takeover ()
  "Show stashes the (window . prev-buffer) cell so `--hide' can restore."
  (let* ((claude-repl-explain-config-buffer-name " *test-explain-takeover-record*")
         (buf (get-buffer-create claude-repl-explain-config-buffer-name))
         (claude-repl--explain-config-global-visible-p nil)
         (claude-repl--explain-config-replaced-window nil))
    (unwind-protect
        (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) nil))
                  ((symbol-function 'claude-repl--explain-config-current-claude-output-window)
                   (lambda () 'fake-output-win))
                  ((symbol-function 'window-buffer) (lambda (_w) 'prev-buf))
                  ((symbol-function 'set-window-dedicated-p) #'ignore)
                  ((symbol-function 'set-window-buffer) #'ignore))
          (claude-repl--explain-config-show)
          (should (equal claude-repl--explain-config-replaced-window
                         '(fake-output-win . prev-buf))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest claude-repl-cmd-test-explain-config-show/takeover-clears-window-dedication ()
  "Show clears the claude output window's dedicated flag before swapping
buffers — without this, `set-window-buffer' would error on the dedicated
window."
  (let* ((claude-repl-explain-config-buffer-name " *test-explain-takeover-dedup*")
         (buf (get-buffer-create claude-repl-explain-config-buffer-name))
         (claude-repl--explain-config-global-visible-p nil)
         (claude-repl--explain-config-replaced-window nil)
         (dedicated-cleared-with nil))
    (unwind-protect
        (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) nil))
                  ((symbol-function 'claude-repl--explain-config-current-claude-output-window)
                   (lambda () 'fake-output-win))
                  ((symbol-function 'window-buffer) (lambda (_w) 'prev-buf))
                  ((symbol-function 'set-window-dedicated-p)
                   (lambda (w flag) (setq dedicated-cleared-with (list w flag))))
                  ((symbol-function 'set-window-buffer) #'ignore))
          (claude-repl--explain-config-show)
          (should (equal dedicated-cleared-with '(fake-output-win nil))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest claude-repl-cmd-test-explain-config-hide/deletes-windows-for-buffer ()
  "Hide delegates to `claude-repl-window--delete-buffer-windows' for the buffer."
  (let* ((claude-repl-explain-config-buffer-name " *test-explain-hide*")
         (buf (get-buffer-create claude-repl-explain-config-buffer-name))
         (claude-repl--explain-config-replaced-window nil)
         (delete-called-with nil))
    (unwind-protect
        (cl-letf (((symbol-function 'claude-repl-window--delete-buffer-windows)
                   (lambda (b &rest _) (setq delete-called-with b))))
          (claude-repl--explain-config-hide)
          (should (eq delete-called-with buf)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest claude-repl-cmd-test-explain-config-hide/no-op-when-buffer-missing ()
  "Hide is a no-op when no explain-config buffer has ever been created."
  (let ((claude-repl-explain-config-buffer-name " *nonexistent-explain-hide*")
        (claude-repl--explain-config-replaced-window nil)
        (delete-called nil))
    (cl-letf (((symbol-function 'claude-repl-window--delete-buffer-windows)
               (lambda (&rest _) (setq delete-called t))))
      (claude-repl--explain-config-hide)
      (should-not delete-called))))

(ert-deftest claude-repl-cmd-test-explain-config-hide/clears-global-visible-flag ()
  "Hide clears `--global-visible-p' so the popup stays gone across persps."
  (let ((claude-repl--explain-config-global-visible-p t)
        (claude-repl--explain-config-replaced-window nil))
    (cl-letf (((symbol-function 'claude-repl-window--delete-buffer-windows) #'ignore))
      (claude-repl--explain-config-hide)
      (should-not claude-repl--explain-config-global-visible-p))))

;;;; ---- Drawer decoupling: --hide must never touch the drawer ----

(ert-deftest claude-repl-cmd-test-explain-config-hide/does-not-call-drawer-show ()
  "Hide must NOT call `claude-repl-drawer-show' — the popup never modified
the drawer in the first place, so it has nothing to restore."
  (let ((claude-repl--explain-config-global-visible-p t)
        (claude-repl--explain-config-replaced-window nil)
        (drawer-show-called nil))
    (cl-letf (((symbol-function 'claude-repl-window--delete-buffer-windows) #'ignore)
              ((symbol-function 'claude-repl-drawer-show)
               (lambda () (setq drawer-show-called t))))
      (claude-repl--explain-config-hide)
      (should-not drawer-show-called))))

;;;; ---- Claude output window restoration on --hide ----

(ert-deftest claude-repl-cmd-test-explain-config-hide/restores-prev-buffer-in-replaced-window ()
  "Hide re-displays the saved prev-buffer in the window the popup took over."
  (let* ((claude-repl-explain-config-buffer-name " *test-explain-restore*")
         (buf (get-buffer-create claude-repl-explain-config-buffer-name))
         (prev (get-buffer-create " *test-explain-restore-prev*"))
         (claude-repl--explain-config-global-visible-p t)
         (claude-repl--explain-config-replaced-window (cons 'fake-output-win prev))
         (set-window-buffer-args nil))
    (unwind-protect
        (cl-letf (((symbol-function 'window-live-p) (lambda (_w) t))
                  ((symbol-function 'set-window-buffer)
                   (lambda (w b) (setq set-window-buffer-args (list w b))))
                  ((symbol-function 'claude-repl--configure-vterm-window) #'ignore)
                  ((symbol-function 'claude-repl-window--delete-buffer-windows) #'ignore))
          (claude-repl--explain-config-hide)
          (should (equal set-window-buffer-args (list 'fake-output-win prev))))
      (when (buffer-live-p buf) (kill-buffer buf))
      (when (buffer-live-p prev) (kill-buffer prev)))))

(ert-deftest claude-repl-cmd-test-explain-config-hide/clears-replaced-window-after-restore ()
  "Hide clears `--replaced-window' after restoring, so a future show won't
double-restore an already-rehydrated claude output window."
  (let ((claude-repl--explain-config-global-visible-p t)
        (claude-repl--explain-config-replaced-window (cons 'fake-output-win 'prev-buf)))
    (cl-letf (((symbol-function 'window-live-p) (lambda (_w) nil))
              ((symbol-function 'claude-repl-window--delete-buffer-windows) #'ignore))
      (claude-repl--explain-config-hide)
      (should-not claude-repl--explain-config-replaced-window))))

(ert-deftest claude-repl-cmd-test-explain-config-hide/reapplies-claude-output-window-hardening ()
  "Hide re-applies `--configure-vterm-window' on successful restore so the
claude output window regains its dedicate/size-fix/delete-protect recipe."
  (let* ((prev (get-buffer-create " *test-explain-reharden-prev*"))
         (claude-repl--explain-config-global-visible-p t)
         (claude-repl--explain-config-replaced-window (cons 'fake-output-win prev))
         (configure-called-with nil))
    (unwind-protect
        (cl-letf (((symbol-function 'window-live-p) (lambda (_w) t))
                  ((symbol-function 'set-window-buffer) #'ignore)
                  ((symbol-function 'claude-repl--configure-vterm-window)
                   (lambda (w) (setq configure-called-with w)))
                  ((symbol-function 'claude-repl-window--delete-buffer-windows) #'ignore))
          (claude-repl--explain-config-hide)
          (should (eq configure-called-with 'fake-output-win)))
      (when (buffer-live-p prev) (kill-buffer prev)))))

(ert-deftest claude-repl-cmd-test-explain-config-hide/skips-restore-when-window-dead ()
  "Hide is a no-op on the restore path when the saved window is no longer live."
  (let ((claude-repl--explain-config-global-visible-p t)
        (claude-repl--explain-config-replaced-window (cons 'dead-win 'prev-buf))
        (set-window-buffer-called nil))
    (cl-letf (((symbol-function 'window-live-p) (lambda (_w) nil))
              ((symbol-function 'set-window-buffer)
               (lambda (&rest _) (setq set-window-buffer-called t)))
              ((symbol-function 'claude-repl-window--delete-buffer-windows) #'ignore))
      (claude-repl--explain-config-hide)
      (should-not set-window-buffer-called))))

;;;; ---- claude-repl-explain-config-close ----

(ert-deftest claude-repl-cmd-test-explain-config-close/is-interactive ()
  "Public close command is interactively callable (has interactive form)."
  (should (commandp #'claude-repl-explain-config-close)))

(ert-deftest claude-repl-cmd-test-explain-config-close/delegates-to-hide ()
  "Public close command invokes the private hide implementation."
  (let ((hide-called 0))
    (cl-letf (((symbol-function 'claude-repl--explain-config-hide)
               (lambda (&rest _) (cl-incf hide-called))))
      (claude-repl-explain-config-close)
      (should (= hide-called 1)))))

(ert-deftest claude-repl-cmd-test-explain-config-close/clears-flag-and-deletes-windows ()
  "End-to-end: public close clears the global flag AND deletes the popup windows."
  (let* ((claude-repl-explain-config-buffer-name " *test-explain-close-e2e*")
         (buf (get-buffer-create claude-repl-explain-config-buffer-name))
         (claude-repl--explain-config-global-visible-p t)
         (claude-repl--explain-config-replaced-window nil)
         (delete-called-with nil))
    (unwind-protect
        (cl-letf (((symbol-function 'claude-repl-window--delete-buffer-windows)
                   (lambda (b &rest _) (setq delete-called-with b))))
          (claude-repl-explain-config-close)
          (should-not claude-repl--explain-config-global-visible-p)
          (should (eq delete-called-with buf)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest claude-repl-cmd-test-explain-config-mode/q-binds-to-close ()
  "Minor-mode keymap binds `q' to the public close command."
  (should (eq (lookup-key claude-repl-explain-config-mode-map (kbd "q"))
              #'claude-repl-explain-config-close)))

(ert-deftest claude-repl-cmd-test-explain-config-init-buffer/enables-minor-mode ()
  "init-buffer activates `claude-repl-explain-config-mode' in the output buffer."
  (let* ((claude-repl-explain-config-buffer-name " *test-explain-config-mode*")
         (buf (get-buffer-create claude-repl-explain-config-buffer-name)))
    (unwind-protect
        (progn
          (claude-repl--explain-config-init-buffer "any question")
          (with-current-buffer buf
            (should claude-repl-explain-config-mode)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest claude-repl-cmd-test-explain-config-ensure-visible/shows-when-flag-set-and-hidden ()
  "Persp-reconciliation re-displays the popup via `--show' when flag is set
and window is missing, with no claude output window available to take over."
  (let* ((claude-repl-explain-config-buffer-name " *test-explain-persp-show*")
         (buf (get-buffer-create claude-repl-explain-config-buffer-name))
         (claude-repl--explain-config-global-visible-p t)
         (claude-repl--explain-config-replaced-window nil)
         (display-args nil))
    (unwind-protect
        (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) nil))
                  ((symbol-function 'claude-repl--explain-config-current-claude-output-window)
                   (lambda () nil))
                  ((symbol-function 'display-buffer)
                   (lambda (b action) (setq display-args (list b action)) nil))
                  ((symbol-function 'claude-repl--explain-config-apply-width) #'ignore))
          (claude-repl--explain-config-ensure-visible-on-persp-switch)
          (should (eq (car display-args) buf))
          (should (eq (cadr display-args)
                      claude-repl--explain-config-display-action)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest claude-repl-cmd-test-explain-config-ensure-visible/takes-over-new-persp-claude-output-window ()
  "Persp-reconciliation routes through `--show', which takes over the new
persp's claude output window when it is visible — no side-window fallback."
  (let* ((claude-repl-explain-config-buffer-name " *test-explain-persp-takeover*")
         (buf (get-buffer-create claude-repl-explain-config-buffer-name))
         (claude-repl--explain-config-global-visible-p t)
         (claude-repl--explain-config-replaced-window nil)
         (set-window-buffer-args nil)
         (display-buffer-called nil))
    (unwind-protect
        (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) nil))
                  ((symbol-function 'claude-repl--explain-config-current-claude-output-window)
                   (lambda () 'new-persp-output-win))
                  ((symbol-function 'window-buffer) (lambda (_w) 'new-prev-buf))
                  ((symbol-function 'set-window-dedicated-p) #'ignore)
                  ((symbol-function 'set-window-buffer)
                   (lambda (w b) (setq set-window-buffer-args (list w b))))
                  ((symbol-function 'display-buffer)
                   (lambda (&rest _) (setq display-buffer-called t) nil)))
          (claude-repl--explain-config-ensure-visible-on-persp-switch)
          (should (equal set-window-buffer-args (list 'new-persp-output-win buf)))
          (should-not display-buffer-called))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest claude-repl-cmd-test-explain-config-ensure-visible/drops-stale-replaced-window ()
  "Persp-reconciliation discards a `--replaced-window' whose window is dead
before re-showing — the cell belongs to the persp we left, not the new one."
  (let* ((claude-repl-explain-config-buffer-name " *test-explain-persp-stale*")
         (buf (get-buffer-create claude-repl-explain-config-buffer-name))
         (claude-repl--explain-config-global-visible-p t)
         (claude-repl--explain-config-replaced-window (cons 'dead-win 'old-prev)))
    (unwind-protect
        (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) nil))
                  ((symbol-function 'window-live-p)
                   (lambda (w) (not (eq w 'dead-win))))
                  ((symbol-function 'claude-repl--explain-config-current-claude-output-window)
                   (lambda () nil))
                  ((symbol-function 'display-buffer) (lambda (&rest _) nil))
                  ((symbol-function 'claude-repl--explain-config-apply-width) #'ignore))
          (claude-repl--explain-config-ensure-visible-on-persp-switch)
          ;; Stale cell is gone — neither show nor hide tried to use it.
          (should-not claude-repl--explain-config-replaced-window))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest claude-repl-cmd-test-explain-config-ensure-visible/hides-when-flag-nil-and-shown ()
  "Persp-reconciliation deletes stale popup windows when flag is nil."
  (let* ((claude-repl-explain-config-buffer-name " *test-explain-persp-hide*")
         (buf (get-buffer-create claude-repl-explain-config-buffer-name))
         (claude-repl--explain-config-global-visible-p nil)
         (delete-called-with nil))
    (unwind-protect
        (cl-letf (((symbol-function 'get-buffer-window)
                   (lambda (&rest _) 'fake-win))
                  ((symbol-function 'claude-repl-window--delete-buffer-windows)
                   (lambda (b &rest _) (setq delete-called-with b))))
          (claude-repl--explain-config-ensure-visible-on-persp-switch)
          (should (eq delete-called-with buf)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest claude-repl-cmd-test-explain-config-ensure-visible/noop-when-flag-set-and-already-visible ()
  "Persp-reconciliation skips display when flag is set and window is already live."
  (let* ((claude-repl-explain-config-buffer-name " *test-explain-persp-noop*")
         (buf (get-buffer-create claude-repl-explain-config-buffer-name))
         (claude-repl--explain-config-global-visible-p t))
    (unwind-protect
        (cl-letf (((symbol-function 'get-buffer-window)
                   (lambda (&rest _) 'fake-win))
                  ((symbol-function 'display-buffer)
                   (lambda (&rest _) (error "should not redisplay")))
                  ((symbol-function 'claude-repl-window--delete-buffer-windows)
                   (lambda (&rest _) (error "should not delete"))))
          (claude-repl--explain-config-ensure-visible-on-persp-switch)
          ;; No error means the test passed.
          (should t))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest claude-repl-cmd-test-explain-config-spawn/does-not-open-drawer ()
  "Spawn must NOT open the drawer — the popup is decoupled and stands alone."
  (let ((drawer-shown nil))
    (cl-letf (((symbol-function 'make-process) (lambda (&rest _) 'fake-proc))
              ((symbol-function 'process-send-string) #'ignore)
              ((symbol-function 'process-send-eof) #'ignore)
              ((symbol-function 'claude-repl--explain-config-show) #'ignore)
              ((symbol-function 'claude-repl-drawer-show)
               (lambda () (setq drawer-shown t))))
      (claude-repl--explain-config-spawn "anything")
      (should-not drawer-shown))))

(ert-deftest claude-repl-cmd-test-explain-config-spawn/does-not-call-show ()
  "Spawn must NOT call `--explain-config-show' — the popup is deferred until
the first model chunk arrives via `--explain-config-filter'."
  (let ((show-called nil))
    (cl-letf (((symbol-function 'make-process) (lambda (&rest _) 'fake-proc))
              ((symbol-function 'process-send-string) #'ignore)
              ((symbol-function 'process-send-eof) #'ignore)
              ((symbol-function 'claude-repl--explain-config-show)
               (lambda () (setq show-called t))))
      (claude-repl--explain-config-spawn "anything")
      (should-not show-called))))

(ert-deftest claude-repl-cmd-test-explain-config-filter/calls-show-on-chunk ()
  "Filter calls `--explain-config-show' after parsing each chunk so the popup
appears the moment the model starts streaming output."
  (let* ((claude-repl-explain-config-buffer-name " *test-explain-filter-show*")
         (buf (get-buffer-create claude-repl-explain-config-buffer-name))
         (show-called nil))
    (unwind-protect
        (cl-letf (((symbol-function 'process-buffer) (lambda (_p) buf))
                  ((symbol-function 'claude-repl--explain-config-parse-chunk) #'ignore)
                  ((symbol-function 'claude-repl--explain-config-show)
                   (lambda () (setq show-called t))))
          (claude-repl--explain-config-filter 'fake-proc "hello")
          (should show-called))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest claude-repl-cmd-test-explain-config-filter/parses-before-showing ()
  "Filter parses the chunk first, then calls `--show' — content is in the
buffer before the window is displayed."
  (let* ((claude-repl-explain-config-buffer-name " *test-explain-filter-order*")
         (buf (get-buffer-create claude-repl-explain-config-buffer-name))
         (order nil))
    (unwind-protect
        (cl-letf (((symbol-function 'process-buffer) (lambda (_p) buf))
                  ((symbol-function 'claude-repl--explain-config-parse-chunk)
                   (lambda (&rest _) (push 'parse order)))
                  ((symbol-function 'claude-repl--explain-config-show)
                   (lambda () (push 'show order))))
          (claude-repl--explain-config-filter 'fake-proc "hello")
          (should (equal (reverse order) '(parse show))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

;;;; ---- claude-repl explain-config face-markup rendering ----

(defmacro claude-repl-test--with-explain-config-buf (buf-sym &rest body)
  "Bind BUF-SYM to a fresh, parser-initialized explain-config buffer.
Clean up the buffer after BODY runs."
  (declare (indent 1))
  `(let* ((claude-repl-explain-config-buffer-name
           (format " *test-explain-config-render-%s*" (cl-gensym)))
          (,buf-sym (get-buffer-create claude-repl-explain-config-buffer-name)))
     (unwind-protect
         (progn
           (with-current-buffer ,buf-sym
             (erase-buffer)
             (setq claude-repl--explain-config-pending ""
                   claude-repl--explain-config-face-stack nil))
           ,@body)
       (when (buffer-live-p ,buf-sym) (kill-buffer ,buf-sym)))))

(ert-deftest claude-repl-cmd-test-explain-config-preamble/instructs-face-markup ()
  "Preamble documents the ⟦face⟧ tag contract instead of Markdown."
  (let ((rendered (claude-repl--explain-config-build-input "anything")))
    (should (string-match-p "EMACS FACE MARKUP" rendered))
    (should (string-match-p "DO NOT use\\s-+Markdown" rendered))
    (should (string-match-p "⟦h1⟧" rendered))
    (should (string-match-p "⟦/h1⟧" rendered))
    (should (string-match-p "⟦code⟧" rendered))
    (should (string-match-p "⟦block⟧" rendered))))

(ert-deftest claude-repl-cmd-test-explain-config-preamble/documents-raw-face-escape-hatch ()
  "Preamble documents the ⟦face PLIST⟧ raw-elisp escape hatch."
  (let ((rendered (claude-repl--explain-config-build-input "anything")))
    (should (string-match-p "ESCAPE HATCH" rendered))
    (should (string-match-p "⟦face " rendered))
    (should (string-match-p "⟦/face⟧" rendered))
    (should (string-match-p ":foreground" rendered))
    (should (string-match-p ":background" rendered))
    (should (string-match-p ":weight" rendered))))

(ert-deftest claude-repl-cmd-test-explain-config-preamble/prefers-semantic-tags ()
  "Preamble tells the model to prefer semantic tags over the escape hatch."
  (let ((rendered (claude-repl--explain-config-build-input "anything")))
    (should (string-match-p "PREFER semantic" rendered))))

(ert-deftest claude-repl-cmd-test-explain-config-parse/plain-text-passes-through ()
  "Untagged text is inserted verbatim with no face property."
  (claude-repl-test--with-explain-config-buf buf
    (claude-repl--explain-config-parse-chunk buf "hello world")
    (with-current-buffer buf
      (should (equal (buffer-string) "hello world"))
      (should-not (get-text-property 1 'face)))))

(ert-deftest claude-repl-cmd-test-explain-config-parse/single-tag-applies-face ()
  "An ⟦h1⟧…⟦/h1⟧ segment renders with the h1 face and strips the tags."
  (claude-repl-test--with-explain-config-buf buf
    (claude-repl--explain-config-parse-chunk buf "⟦h1⟧Title⟦/h1⟧")
    (with-current-buffer buf
      (should (equal (buffer-string) "Title"))
      (should (eq (get-text-property 1 'face)
                  'claude-repl-explain-config-h1)))))

(ert-deftest claude-repl-cmd-test-explain-config-parse/nested-tags-stack-faces ()
  "Nested tags produce a list face value with innermost first."
  (claude-repl-test--with-explain-config-buf buf
    (claude-repl--explain-config-parse-chunk buf "⟦h1⟧A ⟦b⟧B⟦/b⟧ C⟦/h1⟧")
    (with-current-buffer buf
      (let* ((s (buffer-string))
             (b-pos (string-match "B" s))
             (a-pos (string-match "A" s))
             (b-face (get-text-property b-pos 'face s))
             (a-face (get-text-property a-pos 'face s)))
        (should (equal s "A B C"))
        (should (eq a-face 'claude-repl-explain-config-h1))
        (should (equal b-face
                       '(claude-repl-explain-config-bold
                         claude-repl-explain-config-h1)))))))

(ert-deftest claude-repl-cmd-test-explain-config-parse/partial-tag-across-chunks ()
  "An open tag split across two chunks still styles the inner text correctly."
  (claude-repl-test--with-explain-config-buf buf
    (claude-repl--explain-config-parse-chunk buf "pre ⟦b")
    (with-current-buffer buf
      (should (equal (buffer-string) "pre "))
      (should (equal claude-repl--explain-config-pending "⟦b")))
    (claude-repl--explain-config-parse-chunk buf "⟧bold⟦/b⟧ post")
    (with-current-buffer buf
      (let* ((s (buffer-string))
             (bold-pos (string-match "bold" s))
             (post-pos (string-match "post" s)))
        (should (equal s "pre bold post"))
        (should (eq (get-text-property bold-pos 'face s)
                    'claude-repl-explain-config-bold))
        (should-not (get-text-property post-pos 'face s))))))

(ert-deftest claude-repl-cmd-test-explain-config-parse/partial-close-tag-across-chunks ()
  "A close tag split across two chunks doesn't leak the partial bytes."
  (claude-repl-test--with-explain-config-buf buf
    (claude-repl--explain-config-parse-chunk buf "⟦b⟧bold⟦/b")
    (with-current-buffer buf
      (should (equal (buffer-string) "bold"))
      (should (equal claude-repl--explain-config-pending "⟦/b")))
    (claude-repl--explain-config-parse-chunk buf "⟧ tail")
    (with-current-buffer buf
      (let* ((s (buffer-string))
             (tail-pos (string-match " tail" s)))
        (should (equal s "bold tail"))
        (should (equal claude-repl--explain-config-pending ""))
        (should-not (get-text-property tail-pos 'face s))))))

(ert-deftest claude-repl-cmd-test-explain-config-parse/unknown-tag-emits-verbatim ()
  "Unknown tag names are rendered verbatim with no face change."
  (claude-repl-test--with-explain-config-buf buf
    (claude-repl--explain-config-parse-chunk buf "x ⟦bogus⟧y⟦/bogus⟧ z")
    (with-current-buffer buf
      (should (equal (buffer-string) "x ⟦bogus⟧y⟦/bogus⟧ z"))
      (should-not (get-text-property 1 'face)))))

(ert-deftest claude-repl-cmd-test-explain-config-parse/stray-close-tag-is-noop ()
  "A close tag with no matching open is a no-op on the face stack."
  (claude-repl-test--with-explain-config-buf buf
    (claude-repl--explain-config-parse-chunk buf "before⟦/b⟧after")
    (with-current-buffer buf
      (should (equal (buffer-string) "beforeafter"))
      (should (null claude-repl--explain-config-face-stack))
      (should-not (get-text-property 1 'face)))))

(ert-deftest claude-repl-cmd-test-explain-config-parse/block-and-kbd-have-distinct-faces ()
  "Different tag names map to different defined faces."
  (claude-repl-test--with-explain-config-buf buf
    (claude-repl--explain-config-parse-chunk
     buf "⟦kbd⟧SPC j h c⟦/kbd⟧ runs ⟦sym⟧explain-config⟦/sym⟧")
    (with-current-buffer buf
      (let* ((s (buffer-string))
             (spc-pos (string-match "SPC" s))
             (sym-pos (string-match "explain-config" s)))
        (should (eq (get-text-property spc-pos 'face s)
                    'claude-repl-explain-config-kbd))
        (should (eq (get-text-property sym-pos 'face s)
                    'claude-repl-explain-config-sym))))))

;;;; ---- claude-repl explain-config raw-face escape hatch ----

(ert-deftest claude-repl-cmd-test-explain-config-parse/raw-face-applies-plist ()
  "⟦face PLIST⟧…⟦/face⟧ applies the raw plist as the text-face property."
  (claude-repl-test--with-explain-config-buf buf
    (claude-repl--explain-config-parse-chunk
     buf "⟦face :foreground \"red\" :weight bold⟧danger⟦/face⟧")
    (with-current-buffer buf
      (should (equal (buffer-string) "danger"))
      (should (equal (get-text-property 1 'face)
                     '(:foreground "red" :weight bold))))))

(ert-deftest claude-repl-cmd-test-explain-config-parse/raw-face-nested-inside-semantic ()
  "⟦face …⟧ nested inside ⟦b⟧ merges as a list of face specs."
  (claude-repl-test--with-explain-config-buf buf
    (claude-repl--explain-config-parse-chunk
     buf "⟦b⟧bold ⟦face :foreground \"red\"⟧red⟦/face⟧ end⟦/b⟧")
    (with-current-buffer buf
      (let* ((s (buffer-string))
             (red-pos (string-match "red" s))
             (red-face (get-text-property red-pos 'face s)))
        (should (equal s "bold red end"))
        (should (equal red-face
                       '((:foreground "red")
                         claude-repl-explain-config-bold)))))))

(ert-deftest claude-repl-cmd-test-explain-config-parse/raw-face-malformed-plist-verbatim ()
  "An unparseable plist inside ⟦face …⟧ emits the open tag verbatim."
  (claude-repl-test--with-explain-config-buf buf
    (claude-repl--explain-config-parse-chunk
     buf "⟦face :foreground⟧x⟦/face⟧")
    (with-current-buffer buf
      ;; Open tag verbatim, body unstyled, close tag verbatim — since
      ;; stack stays empty the matching close also lands verbatim.
      (should (equal (buffer-string) "⟦face :foreground⟧x⟦/face⟧"))
      (should-not (get-text-property 1 'face))
      (should (null claude-repl--explain-config-face-stack)))))

(ert-deftest claude-repl-cmd-test-explain-config-parse/raw-face-unknown-attr-rejected ()
  "A plist key outside the whitelist is rejected — open tag verbatim."
  (claude-repl-test--with-explain-config-buf buf
    (claude-repl--explain-config-parse-chunk
     buf "⟦face :bogus 1⟧x⟦/face⟧")
    (with-current-buffer buf
      (should (equal (buffer-string) "⟦face :bogus 1⟧x⟦/face⟧"))
      (should-not (get-text-property 1 'face)))))

(ert-deftest claude-repl-cmd-test-explain-config-parse/raw-face-partial-across-chunks ()
  "A ⟦face PLIST⟧ split across chunks buffers until the closing ⟧ arrives."
  (claude-repl-test--with-explain-config-buf buf
    (claude-repl--explain-config-parse-chunk buf "before ⟦face :foreground ")
    (with-current-buffer buf
      (should (equal (buffer-string) "before "))
      (should (equal claude-repl--explain-config-pending
                     "⟦face :foreground ")))
    (claude-repl--explain-config-parse-chunk buf "\"red\"⟧hot⟦/face⟧ tail")
    (with-current-buffer buf
      (let* ((s (buffer-string))
             (hot-pos (string-match "hot" s))
             (tail-pos (string-match " tail" s)))
        (should (equal s "before hot tail"))
        (should (equal (get-text-property hot-pos 'face s)
                       '(:foreground "red")))
        (should-not (get-text-property tail-pos 'face s))
        (should (equal claude-repl--explain-config-pending ""))))))

(ert-deftest claude-repl-cmd-test-explain-config-parse/raw-face-stray-close-verbatim ()
  "A ⟦/face⟧ with no preceding open emits verbatim, stack untouched."
  (claude-repl-test--with-explain-config-buf buf
    (claude-repl--explain-config-parse-chunk buf "x⟦/face⟧y")
    (with-current-buffer buf
      (should (equal (buffer-string) "x⟦/face⟧y"))
      (should (null claude-repl--explain-config-face-stack)))))

(ert-deftest claude-repl-cmd-test-explain-config-parse/raw-face-close-pops-only-plist-top ()
  "⟦/face⟧ pops only when the stack top is a plist, not a face symbol."
  (claude-repl-test--with-explain-config-buf buf
    ;; ⟦b⟧ pushes the bold face symbol; ⟦/face⟧ must NOT pop it.
    (claude-repl--explain-config-parse-chunk buf "⟦b⟧still⟦/face⟧bold⟦/b⟧")
    (with-current-buffer buf
      (let* ((s (buffer-string))
             (bold-pos (string-match "bold" s)))
        (should (equal s "still⟦/face⟧bold"))
        (should (eq (get-text-property bold-pos 'face s)
                    'claude-repl-explain-config-bold))
        (should (null claude-repl--explain-config-face-stack))))))

(ert-deftest claude-repl-cmd-test-explain-config-parse/raw-face-empty-attrs-verbatim ()
  "⟦face⟧ with no attrs at all is treated as an unparseable plist."
  (claude-repl-test--with-explain-config-buf buf
    (claude-repl--explain-config-parse-chunk buf "⟦face⟧x⟦/face⟧")
    (with-current-buffer buf
      (should (equal (buffer-string) "⟦face⟧x⟦/face⟧"))
      (should-not (get-text-property 1 'face)))))

(ert-deftest claude-repl-cmd-test-explain-config-parse/semantic-tag-with-stray-attrs-verbatim ()
  "A semantic tag carrying attrs (forbidden) renders the open tag verbatim.
The matching close tag is then a stray close — handled by the existing
silent-drop branch (see `stray-close-tag-is-noop'), so the close itself
does not appear in the output."
  (claude-repl-test--with-explain-config-buf buf
    (claude-repl--explain-config-parse-chunk
     buf "⟦b :foreground \"red\"⟧bold⟦/b⟧")
    (with-current-buffer buf
      (should (equal (buffer-string)
                     "⟦b :foreground \"red\"⟧bold"))
      (should-not (get-text-property 1 'face)))))

(ert-deftest claude-repl-cmd-test-explain-config-parse-face-attrs/valid-plist ()
  "A well-formed whitelisted plist is returned as-is."
  (should (equal (claude-repl--explain-config-parse-face-attrs
                  " :foreground \"red\" :weight bold")
                 '(:foreground "red" :weight bold))))

(ert-deftest claude-repl-cmd-test-explain-config-parse-face-attrs/nil-on-extra-tokens ()
  "Trailing junk after the plist sexp is rejected."
  (should (null (claude-repl--explain-config-parse-face-attrs
                 " :foreground \"red\" extra"))))

(ert-deftest claude-repl-cmd-test-explain-config-parse-face-attrs/nil-on-odd-length ()
  "An odd-length plist (key without value) is rejected."
  (should (null (claude-repl--explain-config-parse-face-attrs
                 " :foreground \"red\" :weight"))))

(ert-deftest claude-repl-cmd-test-explain-config-parse-face-attrs/nil-on-unknown-key ()
  "A plist containing a non-whitelisted key is rejected."
  (should (null (claude-repl--explain-config-parse-face-attrs
                 " :bogus 1"))))

(ert-deftest claude-repl-cmd-test-explain-config-parse-face-attrs/nil-on-empty ()
  "Empty or whitespace-only attrs return nil (no plist)."
  (should (null (claude-repl--explain-config-parse-face-attrs "")))
  (should (null (claude-repl--explain-config-parse-face-attrs " "))))

(ert-deftest claude-repl-cmd-test-explain-config-filter/routes-chunk-through-parser ()
  "Process filter forwards CHUNK to the parser using PROC's buffer."
  (claude-repl-test--with-explain-config-buf buf
    (cl-letf (((symbol-function 'process-buffer) (lambda (_p) buf)))
      (claude-repl--explain-config-filter 'fake-proc "⟦b⟧hi⟦/b⟧")
      (with-current-buffer buf
        (should (equal (buffer-string) "hi"))
        (should (eq (get-text-property 1 'face)
                    'claude-repl-explain-config-bold))))))

(ert-deftest claude-repl-cmd-test-explain-config-filter/dead-buffer-is-noop ()
  "Filter does not throw if PROC's buffer is dead."
  (let ((buf (generate-new-buffer " *tmp-dead*")))
    (kill-buffer buf)
    (cl-letf (((symbol-function 'process-buffer) (lambda (_p) buf)))
      (should (progn (claude-repl--explain-config-filter 'fake-proc "x") t)))))

(ert-deftest claude-repl-cmd-test-explain-config-init-buffer/resets-parser-state ()
  "init-buffer wipes any leftover pending bytes and face stack from prior runs."
  (let* ((claude-repl-explain-config-buffer-name " *test-explain-config-reset*")
         (buf (get-buffer-create claude-repl-explain-config-buffer-name)))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (setq claude-repl--explain-config-pending "⟦h"
                  claude-repl--explain-config-face-stack
                  '(claude-repl-explain-config-h1)))
          (claude-repl--explain-config-init-buffer "q")
          (with-current-buffer buf
            (should (equal claude-repl--explain-config-pending ""))
            (should (null claude-repl--explain-config-face-stack))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest claude-repl-cmd-test-explain-config-spawn/registers-face-filter ()
  "spawn installs the rendering filter on the new process."
  (let (captured-filter)
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest args)
                 (setq captured-filter (plist-get args :filter))
                 'fake-proc))
              ((symbol-function 'process-send-string) #'ignore)
              ((symbol-function 'process-send-eof) #'ignore)
              ((symbol-function 'display-buffer) #'ignore))
      (claude-repl--explain-config-spawn "anything")
      (should (eq captured-filter #'claude-repl--explain-config-filter)))))

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
              ;; NOTE: do NOT stub `claude-repl--ws-get' here.  A blanket
              ;; stub that returns `fake-vterm-buf' for every key collides
              ;; with the post-f2560b6 interrupt path, which calls
              ;; `--mark-claude-done' → `--backoff-retry-reset' →
              ;; `(claude-repl--ws-get ws :backoff-retry-count)' and then
              ;; `(> prev 0)' — comparing a buffer against an int errors
              ;; with wrong-type-argument number-or-marker-p.  The real
              ;; `--ws-get' reads the value `--ws-put' just stored for
              ;; `:vterm-buffer' and returns nil for unknown keys, which
              ;; is exactly what the interrupt path expects.
              (cl-letf (((symbol-function 'claude-repl--vterm-live-p)
                         (lambda () t))
                        ((symbol-function '+workspace-current-name)
                         (lambda () "test-ws"))
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

(ert-deftest claude-repl-cmd-test-interrupt/marks-claude-state-done-when-vterm-live ()
  "interrupt sets the workspace's :claude-state to :done after sending escape."
  (claude-repl-test--with-clean-state
    (let ((fake-vterm-buf (get-buffer-create " *test-interrupt-done-vterm*")))
      (unwind-protect
          (progn
            (claude-repl--ws-put "test-ws" :vterm-buffer fake-vterm-buf)
            (claude-repl--ws-set-claude-state "test-ws" :thinking)
            (cl-letf (((symbol-function '+workspace-current-name)
                       (lambda () "test-ws"))
                      ((symbol-function 'claude-repl--send-interrupt-escape)
                       (lambda (_ws _buf) nil))
                      ((symbol-function 'run-at-time)
                       (lambda (_time _repeat _fn _arg) nil)))
              (claude-repl-interrupt)
              (should (eq (claude-repl--ws-get "test-ws" :claude-state) :done))))
        (kill-buffer fake-vterm-buf)))))

(ert-deftest claude-repl-cmd-test-interrupt/clears-stop-tracking-when-vterm-live ()
  "interrupt clears :stop-received and :pending-subagents after sending escape.
The interrupted turn will never see a Stop hook, so leftover tracking
state from the previous turn must be reset by Emacs."
  (claude-repl-test--with-clean-state
    (let ((fake-vterm-buf (get-buffer-create " *test-interrupt-clear-vterm*")))
      (unwind-protect
          (progn
            (claude-repl--ws-put "test-ws" :vterm-buffer fake-vterm-buf)
            (claude-repl--ws-set-stop-received "test-ws" t)
            (claude-repl--ws-incf-pending-subagents "test-ws")
            (cl-letf (((symbol-function '+workspace-current-name)
                       (lambda () "test-ws"))
                      ((symbol-function 'claude-repl--send-interrupt-escape)
                       (lambda (_ws _buf) nil))
                      ((symbol-function 'run-at-time)
                       (lambda (_time _repeat _fn _arg) nil)))
              (claude-repl-interrupt)
              (should-not (claude-repl--ws-stop-received-p "test-ws"))
              (should (= 0 (claude-repl--ws-pending-subagents "test-ws")))))
        (kill-buffer fake-vterm-buf)))))

(ert-deftest claude-repl-cmd-test-interrupt/does-not-mark-done-when-vterm-not-live ()
  "interrupt does not mark :done when vterm is not live.
No interrupt was actually delivered, so the state should not change."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "test-ws" :vterm-buffer nil)
    (claude-repl--ws-set-claude-state "test-ws" :thinking)
    (cl-letf (((symbol-function '+workspace-current-name)
               (lambda () "test-ws"))
              ((symbol-function 'claude-repl--send-interrupt-escape)
               (lambda (_ws _buf) nil)))
      (claude-repl-interrupt)
      (should (eq (claude-repl--ws-get "test-ws" :claude-state) :thinking)))))

;;;; ---- claude-repl-update-pr ----

(ert-deftest claude-repl-cmd-test-update-pr/sends-prompt ()
  "update-pr sends the configured update-pr prompt to claude."
  (let (sent-text)
    (cl-letf (((symbol-function 'claude-repl--send-to-claude)
               (lambda (text) (setq sent-text text))))
      (claude-repl-update-pr)
      (should (equal sent-text claude-repl-update-pr-prompt)))))

;;;; ---- claude-repl-rebase-onto-origin-master ----

(ert-deftest claude-repl-cmd-test-rebase-onto-origin-master/fetches-origin-in-ws-dir ()
  "rebase-onto-origin-master invokes async-git with `fetch origin' in the workspace dir."
  (let (label-arg git-root-arg args-arg)
    (cl-letf (((symbol-function '+workspace-current-name)
               (lambda () "test-ws"))
              ((symbol-function 'claude-repl--ws-dir)
               (lambda (_ws) "/project/"))
              ((symbol-function 'claude-repl--async-git)
               (lambda (label git-root args _callback)
                 (setq label-arg label
                       git-root-arg git-root
                       args-arg args))))
      (claude-repl-rebase-onto-origin-master)
      (should (equal label-arg "rebase-fetch"))
      (should (equal git-root-arg "/project/"))
      (should (equal args-arg '("fetch" "origin"))))))

(ert-deftest claude-repl-cmd-test-rebase-onto-origin-master/sends-prompt-on-fetch-success ()
  "On fetch success, callback sends the rebase prompt to claude."
  (let (sent-text)
    (cl-letf (((symbol-function 'claude-repl--send-to-claude)
               (lambda (text) (setq sent-text text))))
      (claude-repl--rebase-onto-origin-master-callback "test-ws" t "fetch output")
      (should (equal sent-text claude-repl-rebase-onto-origin-master-prompt)))))

(ert-deftest claude-repl-cmd-test-rebase-onto-origin-master/skips-prompt-on-fetch-failure ()
  "On fetch failure, callback does NOT send the rebase prompt."
  (let ((send-called nil))
    (cl-letf (((symbol-function 'claude-repl--send-to-claude)
               (lambda (_text) (setq send-called t))))
      (claude-repl--rebase-onto-origin-master-callback "test-ws" nil "fatal: not a git repository")
      (should-not send-called))))

(ert-deftest claude-repl-cmd-test-rebase-onto-origin-master/callback-routes-through-async-git ()
  "Command's async-git callback dispatches via the named callback helper."
  (let (captured-callback sent-text)
    (cl-letf (((symbol-function '+workspace-current-name)
               (lambda () "test-ws"))
              ((symbol-function 'claude-repl--ws-dir)
               (lambda (_ws) "/project/"))
              ((symbol-function 'claude-repl--async-git)
               (lambda (_label _git-root _args callback)
                 (setq captured-callback callback)))
              ((symbol-function 'claude-repl--send-to-claude)
               (lambda (text) (setq sent-text text))))
      (claude-repl-rebase-onto-origin-master)
      (funcall captured-callback t "ok")
      (should (equal sent-text claude-repl-rebase-onto-origin-master-prompt)))))

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

;;;; ---- claude-repl-create-or-update-pr-paste ----

(ert-deftest claude-repl-cmd-test-create-or-update-pr-paste/inserts-prompt-at-point ()
  "paste variant inserts the full base prompt at point, wrapped in backticks."
  (with-temp-buffer
    (claude-repl-create-or-update-pr-paste)
    (should (equal (buffer-string)
                   (concat "`"
                           (claude-repl--build-create-or-update-pr-prompt nil)
                           "`")))))

(ert-deftest claude-repl-cmd-test-create-or-update-pr-paste/wraps-in-backticks ()
  "paste variant wraps the inserted prompt in single backticks."
  (with-temp-buffer
    (claude-repl-create-or-update-pr-paste)
    (let ((s (buffer-string)))
      (should (string-prefix-p "`" s))
      (should (string-suffix-p "`" s)))))

(ert-deftest claude-repl-cmd-test-create-or-update-pr-paste/does-not-send-to-claude ()
  "paste variant must not call `claude-repl--send-to-claude'."
  (let (send-called)
    (cl-letf (((symbol-function 'claude-repl--send-to-claude)
               (lambda (&rest _) (setq send-called t))))
      (with-temp-buffer
        (claude-repl-create-or-update-pr-paste)
        (should-not send-called)))))

(ert-deftest claude-repl-cmd-test-create-or-update-pr-paste/excluded-arg-omits-flag ()
  "paste variant honors EXCLUDED and drops the named flag from the inserted text."
  (with-temp-buffer
    (claude-repl-create-or-update-pr-paste '(no-self-certified))
    (should-not (string-match-p "--self-certified" (buffer-string)))))

(ert-deftest claude-repl-cmd-test-create-or-update-pr-paste/ignores-input-buffer-prefix ()
  "paste variant inserts the bare prompt — the workspace input buffer is not consulted."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-buffer " *test-coup-paste-input*"
      (insert "do a thing")
      (claude-repl--ws-put "test-ws" :input-buffer (current-buffer))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'claude-repl--commit-input-buffer)
                 (lambda (&rest _) (error "must not commit input buffer"))))
        (with-temp-buffer
          (claude-repl-create-or-update-pr-paste)
          (should (equal (buffer-string)
                         (concat "`"
                                 (claude-repl--build-create-or-update-pr-prompt nil)
                                 "`"))))))))

(ert-deftest claude-repl-cmd-test-create-or-update-pr-paste/inserts-at-point-not-end ()
  "paste variant inserts at point, preserving surrounding buffer content."
  (with-temp-buffer
    (insert "before-AFTER")
    (goto-char (+ (point-min) 7))
    (claude-repl-create-or-update-pr-paste)
    (let ((expected (concat "before-`"
                            (claude-repl--build-create-or-update-pr-prompt nil)
                            "`AFTER")))
      (should (equal (buffer-string) expected)))))

;;;; ---- claude-repl-create-or-update-pr-no-self-certified-paste ----

(ert-deftest claude-repl-cmd-test-create-or-update-pr-no-self-certified-paste/omits-flag ()
  "no-self-certified paste wrapper inserts a prompt without --self-certified."
  (with-temp-buffer
    (claude-repl-create-or-update-pr-no-self-certified-paste)
    (should-not (string-match-p "--self-certified" (buffer-string)))))

(ert-deftest claude-repl-cmd-test-create-or-update-pr-no-self-certified-paste/wraps-in-backticks ()
  "no-self-certified paste wrapper wraps the inserted prompt in single backticks."
  (with-temp-buffer
    (claude-repl-create-or-update-pr-no-self-certified-paste)
    (let ((s (buffer-string)))
      (should (string-prefix-p "`" s))
      (should (string-suffix-p "`" s)))))

(ert-deftest claude-repl-cmd-test-create-or-update-pr-no-self-certified-paste/does-not-send ()
  "no-self-certified paste wrapper does not invoke `claude-repl--send-to-claude'."
  (let (send-called)
    (cl-letf (((symbol-function 'claude-repl--send-to-claude)
               (lambda (&rest _) (setq send-called t))))
      (with-temp-buffer
        (claude-repl-create-or-update-pr-no-self-certified-paste)
        (should-not send-called)))))

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

(ert-deftest claude-repl-cmd-test-nuke-workspace/kills-session-and-tombstones-hashmap ()
  "nuke-workspace kills session, kills persp workspace, and tombstones hashmap entry.
Post-tombstone-refactor, the hash entry survives with `:nuked-at' stamped
rather than being removed; `--ws-live-p' is the predicate that filters
tombstones out of the drawer/picker."
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
                ((symbol-function '+workspace-exists-p) (lambda (_n) t))
                ((symbol-function '+workspace/kill)
                 (lambda (ws) (setq persp-killed ws)))
                ((symbol-function 'force-mode-line-update) #'ignore))
        (claude-repl-nuke-workspace)
        (should (equal session-killed "doomed"))
        (should (equal persp-killed "doomed"))
        (should-not (claude-repl--ws-live-p "doomed"))
        (should (claude-repl--ws-get "doomed" :nuked-at))))))

(ert-deftest claude-repl-cmd-test-nuke-workspace/no-confirmation-prompt ()
  "nuke-workspace MUST NOT prompt for confirmation.  Teardown is
immediate — persisted state.el is preserved so accidental invocations
are recoverable by reopening the project."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "doomed" :project-dir "/tmp/doomed")
    (let ((prompted nil))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt _coll &rest _) "doomed"))
                ((symbol-function 'y-or-n-p)
                 (lambda (_prompt) (setq prompted t) t))
                ((symbol-function 'yes-or-no-p)
                 (lambda (_prompt) (setq prompted t) t))
                ((symbol-function 'claude-repl--kill-session) #'ignore)
                ((symbol-function 'persp-get-by-name) (lambda (_n) nil))
                ((symbol-function 'force-mode-line-update) #'ignore))
        (claude-repl-nuke-workspace)
        (should-not prompted)
        (should-not (claude-repl--ws-live-p "doomed"))
        (should (claude-repl--ws-get "doomed" :nuked-at))))))

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
                ((symbol-function '+workspace-exists-p) (lambda (_n) t))
                ((symbol-function '+workspace/kill)
                 (lambda (ws) (setq killed-ws ws)))
                ((symbol-function 'force-mode-line-update) #'ignore))
        (claude-repl-nuke-workspace)
        (should (equal killed-ws "doomed"))))))

(ert-deftest claude-repl-cmd-test-nuke-workspace/no-persp-still-tombstones-hashmap ()
  "nuke-workspace tombstones hashmap entry even when persp workspace doesn't exist."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ghost" :project-dir "/tmp/ghost")
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt _coll &rest _) "ghost"))
              ((symbol-function 'y-or-n-p) (lambda (_prompt) t))
              ((symbol-function 'claude-repl--kill-session) #'ignore)
              ((symbol-function '+workspace-exists-p) (lambda (_n) nil))
              ((symbol-function 'force-mode-line-update) #'ignore))
      (claude-repl-nuke-workspace)
      (should-not (claude-repl--ws-live-p "ghost"))
      (should (claude-repl--ws-get "ghost" :nuked-at)))))

(ert-deftest claude-repl-cmd-test-nuke-workspace/skips-persp-kill-when-workspace-already-gone ()
  "When the persp is already gone from the cache, nuke MUST NOT call
`+workspace/kill' — that call would emit the user-visible warning
`'<ws>' workspace doesn't exist' in the echo area.

Pins the regression seen after a successful workspace merge: the
async merge flow double-closes the workspace (preemptive close in
`--workspace-merge-async', then the deferred success-callback close
in `--workspace-merge-do'), and the second close arrives with the
persp already torn down.  The existence guard MUST short-circuit on
this second pass instead of falling through to `+workspace/kill'.

Crucial detail: persp-mode's real `persp-get-by-name' returns the
keyword `:nil' (i.e. `persp-not-persp', a TRUTHY value) when the
persp is missing — so a guard that gates on `(persp-get-by-name ws)'
truthiness would NOT short-circuit.  The current implementation uses
`+workspace-exists-p' (cache membership) instead, which correctly
returns nil for a missing workspace."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ghost" :project-dir "/tmp/ghost")
    (let ((kill-called nil)
          (persp-mode t))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt _coll &rest _) "ghost"))
                ((symbol-function 'y-or-n-p) (lambda (_prompt) t))
                ((symbol-function 'claude-repl--kill-session) #'ignore)
                ;; Simulate persp-mode's actual broken-guard behavior:
                ;; persp-get-by-name returns `:nil' for a missing persp.
                ((symbol-function 'persp-get-by-name)
                 (lambda (&rest _) :nil))
                ;; Workspace not in the names cache — the real signal
                ;; for "doesn't exist" that `+workspace-exists-p' reads.
                ((symbol-function '+workspace-exists-p) (lambda (_n) nil))
                ((symbol-function '+workspace/kill)
                 (lambda (_ws) (setq kill-called t)))
                ((symbol-function 'force-mode-line-update) #'ignore))
        (claude-repl-nuke-workspace)
        (should-not kill-called)))))

(ert-deftest claude-repl-cmd-test-nuke-workspace/tombstones-hashmap-when-kill-session-errors ()
  "nuke-workspace still tombstones the hashmap entry when kill-session errors.
The teardown error must not prevent the tombstone — otherwise the entry
would stay `live' from `--ws-live-p''s perspective while its runtime
state is corrupted, leaving the drawer/picker showing a half-dead row."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "doomed" :project-dir "/tmp/doomed")
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt _coll &rest _) "doomed"))
              ((symbol-function 'y-or-n-p) (lambda (_prompt) t))
              ((symbol-function 'claude-repl--kill-session)
               (lambda (_ws) (error "simulated kill-session failure")))
              ((symbol-function '+workspace-exists-p) (lambda (_n) nil))
              ((symbol-function 'force-mode-line-update) #'ignore))
      (claude-repl-nuke-workspace)
      (should-not (claude-repl--ws-live-p "doomed"))
      (should (claude-repl--ws-get "doomed" :nuked-at)))))

(ert-deftest claude-repl-cmd-test-nuke-workspace/tombstones-hashmap-when-workspace-kill-errors ()
  "nuke-workspace still tombstones the hashmap entry when +workspace/kill errors."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "doomed" :project-dir "/tmp/doomed")
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt _coll &rest _) "doomed"))
              ((symbol-function 'y-or-n-p) (lambda (_prompt) t))
              ((symbol-function 'claude-repl--kill-session) #'ignore)
              ((symbol-function '+workspace-current-name) (lambda () "other"))
              ((symbol-function '+workspace-exists-p) (lambda (_n) t))
              ((symbol-function '+workspace/kill)
               (lambda (_ws) (error "simulated workspace-kill failure")))
              ((symbol-function 'force-mode-line-update) #'ignore))
      (claude-repl-nuke-workspace)
      (should-not (claude-repl--ws-live-p "doomed"))
      (should (claude-repl--ws-get "doomed" :nuked-at)))))

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
Reversing the order would make the buffer sweep a no-op because the
persp would already be gone before the buffer sweep ran."
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
                ((symbol-function '+workspace-exists-p) (lambda (_n) t))
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
        ;; Post-tombstone: hash entries survive with `:nuked-at' but no
        ;; entry remains live.  Use the live-name helper as the assertion.
        (should-not (claude-repl--live-ws-names))))))

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
        ;; Manual workspace must remain live with its identity intact.
        (should (claude-repl--ws-live-p "manual"))
        (should (claude-repl--ws-get "manual" :project-dir))
        ;; Restored entries are tombstoned, not removed — `:project-dir'
        ;; is preserved across tombstone (identity key), so assert the
        ;; live-p flip and the `:nuked-at' stamp instead.
        (should-not (claude-repl--ws-live-p "restored1"))
        (should-not (claude-repl--ws-live-p "restored2"))
        (should (claude-repl--ws-get "restored1" :nuked-at))
        (should (claude-repl--ws-get "restored2" :nuked-at))))))

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

(ert-deftest claude-repl-cmd-test-nuke-one/preserve-entry-keeps-hash ()
  "Calling `--nuke-one-workspace' with PRESERVE-ENTRY non-nil retains
the hash entry so the drawer's MERGED bucket can render it.  Every
other teardown step still runs."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "merged-ws" :project-dir "/tmp/merged-ws")
    (claude-repl--ws-put "merged-ws" :merge-completed t)
    (let ((persp-mode nil))
      (cl-letf (((symbol-function 'claude-repl--kill-session) #'ignore)
                ((symbol-function 'force-mode-line-update) #'ignore))
        (claude-repl--nuke-one-workspace "merged-ws" 'preserve-entry)
        (should (gethash "merged-ws" claude-repl--workspaces))
        (should (eq (claude-repl--ws-get "merged-ws" :merge-completed) t))))))

(ert-deftest claude-repl-cmd-test-nuke-one/no-preserve-tombstones-hash ()
  "Default `--nuke-one-workspace' (no PRESERVE-ENTRY) tombstones the
hash entry.  Guards against an accidental flip of the default that
would leak live ws plists past teardown.  Post-tombstone-refactor the
entry survives with `:nuked-at' stamped — `--ws-live-p' is the
predicate that keeps it out of every UI/runtime iterator."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :project-dir "/tmp/ws1")
    (let ((persp-mode nil))
      (cl-letf (((symbol-function 'claude-repl--kill-session) #'ignore)
                ((symbol-function 'force-mode-line-update) #'ignore))
        (claude-repl--nuke-one-workspace "ws1")
        (should-not (claude-repl--ws-live-p "ws1"))
        (should (claude-repl--ws-get "ws1" :nuked-at))))))

;;;; ---- register-merged-workspace + state-merge-completed-p ----

(ert-deftest claude-repl-cmd-test-register-merged-workspace/populates-hash ()
  "`--register-merged-workspace' creates a hash entry with
`:project-dir' and `:merge-completed' t even when the on-disk state
file is absent — the snapshot loader uses this to surface MERGED
entries from a snapshot whose state.el was deleted out-of-band."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'claude-repl--state-file-for-read)
               (lambda (_) "/nonexistent/state.el")))
      (claude-repl--register-merged-workspace "merged-ws" "/tmp/merged")
      (should (gethash "merged-ws" claude-repl--workspaces))
      (should (equal (claude-repl--ws-get "merged-ws" :project-dir) "/tmp/merged"))
      (should (eq (claude-repl--ws-get "merged-ws" :merge-completed) t)))))

(ert-deftest claude-repl-cmd-test-register-merged-workspace/hydrates-from-state ()
  "`--register-merged-workspace' pulls per-ws fields from state.el when
present: priority, worktree-p, merge-completed-at, etc., so the
post-restart MERGED entry shows the same metadata it had before quit."
  (claude-repl-test--with-clean-state
    (let ((tmp (make-temp-file "claude-repl-state-" nil ".el")))
      (unwind-protect
          (progn
            (with-temp-file tmp
              (prin1 '(:project-dir "/tmp/merged"
                       :priority "p1"
                       :worktree-p t
                       :merge-completed t
                       :merge-completed-at 1234567890.0
                       :last-prompt-summary "did the thing")
                     (current-buffer)))
            (cl-letf (((symbol-function 'claude-repl--state-file-for-read)
                       (lambda (_) tmp)))
              (claude-repl--register-merged-workspace "merged-ws" "/tmp/merged")
              (should (equal (claude-repl--ws-get "merged-ws" :priority) "p1"))
              (should (eq (claude-repl--ws-get "merged-ws" :worktree-p) t))
              (should (eq (claude-repl--ws-get "merged-ws" :merge-completed) t))
              (should (= (claude-repl--ws-get "merged-ws" :merge-completed-at)
                         1234567890.0))
              (should (equal (claude-repl--ws-get "merged-ws" :last-prompt-summary)
                             "did the thing"))))
        (delete-file tmp)))))

(ert-deftest claude-repl-cmd-test-register-merged-workspace/clean-sets-merged-state ()
  "When the on-disk state has `:merge-completed t' but no `:merge-failed'
\(or `:merge-failed nil') AND the backward-compat probe reports the
merge as landed, `--register-merged-workspace' sets `:repl-state' to
`:merged' and leaves `:merge-failed' clear so the drawer shows 🔀."
  (claude-repl-test--with-clean-state
    (let ((tmp (make-temp-file "claude-repl-state-" nil ".el")))
      (unwind-protect
          (progn
            (with-temp-file tmp
              (prin1 '(:project-dir "/tmp/merged"
                       :merge-completed t)
                     (current-buffer)))
            (cl-letf (((symbol-function 'claude-repl--state-file-for-read)
                       (lambda (_) tmp))
                      ((symbol-function 'claude-repl--detect-merge-actually-landed-p)
                       (lambda (_ws) t)))
              (claude-repl--register-merged-workspace "merged-ws" "/tmp/merged")
              (should (eq (claude-repl--ws-get "merged-ws" :repl-state) :merged))
              (should-not (claude-repl--ws-get "merged-ws" :merge-failed))))
        (delete-file tmp)))))

(ert-deftest claude-repl-cmd-test-register-merged-workspace/restores-persisted-merge-failed ()
  "When the on-disk state explicitly carries `:merge-failed t', the
registered workspace adopts `:repl-state :merge-failed' regardless of
the probe's current verdict — the user's prior signal is authoritative
and should not be overwritten by an opportunistic post-restart probe."
  (claude-repl-test--with-clean-state
    (let ((tmp (make-temp-file "claude-repl-state-" nil ".el")))
      (unwind-protect
          (progn
            (with-temp-file tmp
              (prin1 '(:project-dir "/tmp/merged"
                       :merge-completed t
                       :merge-failed t)
                     (current-buffer)))
            (cl-letf (((symbol-function 'claude-repl--state-file-for-read)
                       (lambda (_) tmp))
                      ((symbol-function 'claude-repl--detect-merge-actually-landed-p)
                       (lambda (_ws) t)))
              (claude-repl--register-merged-workspace "merged-ws" "/tmp/merged")
              (should (eq (claude-repl--ws-get "merged-ws" :merge-failed) t))
              (should (eq (claude-repl--ws-get "merged-ws" :repl-state) :merge-failed))))
        (delete-file tmp)))))

(ert-deftest claude-repl-cmd-test-register-merged-workspace/probe-promotes-to-merge-failed ()
  "When the on-disk state has `:merge-completed t' but no
`:merge-failed' (legacy snapshot from before the flag existed) AND the
backward-compat probe reports the merge as NOT landed, the registered
workspace is promoted to `:repl-state :merge-failed' / `:merge-failed t'
so the drawer ❌ badge appears on first load even though no prior run
recorded the failure."
  (claude-repl-test--with-clean-state
    (let ((tmp (make-temp-file "claude-repl-state-" nil ".el")))
      (unwind-protect
          (progn
            (with-temp-file tmp
              (prin1 '(:project-dir "/tmp/merged"
                       :merge-completed t)
                     (current-buffer)))
            (cl-letf (((symbol-function 'claude-repl--state-file-for-read)
                       (lambda (_) tmp))
                      ((symbol-function 'claude-repl--detect-merge-actually-landed-p)
                       (lambda (_ws) nil)))
              (claude-repl--register-merged-workspace "merged-ws" "/tmp/merged")
              (should (eq (claude-repl--ws-get "merged-ws" :merge-failed) t))
              (should (eq (claude-repl--ws-get "merged-ws" :repl-state) :merge-failed))))
        (delete-file tmp)))))

(ert-deftest claude-repl-cmd-test-state-merge-completed-p/detects-flag ()
  "`--state-merge-completed-p' returns t when state.el carries
`:merge-completed' t and nil otherwise.  Powers the snapshot loader's
route-to-register-merged branch."
  (let ((tmp-merged (make-temp-file "claude-repl-state-" nil ".el"))
        (tmp-plain  (make-temp-file "claude-repl-state-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file tmp-merged
            (prin1 '(:project-dir "/x" :merge-completed t) (current-buffer)))
          (with-temp-file tmp-plain
            (prin1 '(:project-dir "/x") (current-buffer)))
          (cl-letf (((symbol-function 'claude-repl--state-file-for-read)
                     (lambda (d) (cond ((equal d "/merged") tmp-merged)
                                       ((equal d "/plain")  tmp-plain)
                                       (t nil)))))
            (should (claude-repl--state-merge-completed-p "/merged"))
            (should-not (claude-repl--state-merge-completed-p "/plain"))))
      (delete-file tmp-merged)
      (delete-file tmp-plain))))

;;;; ---- claude-repl-kill-workspace ----

(ert-deftest claude-repl-cmd-test-kill-workspace/no-workspaces ()
  "kill-workspace signals user-error when hashmap is empty."
  (claude-repl-test--with-clean-state
    (should-error (claude-repl-kill-workspace) :type 'user-error)))

(ert-deftest claude-repl-cmd-test-kill-workspace/no-confirmation-prompt ()
  "kill-workspace MUST NOT prompt for confirmation.  Teardown is
immediate — persisted state.el is preserved so accidental invocations
are recoverable by reopening the project."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "doomed" :project-dir "/tmp/doomed")
    (let ((prompted nil))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt _coll &rest _) "doomed"))
                ((symbol-function 'y-or-n-p)
                 (lambda (_prompt) (setq prompted t) t))
                ((symbol-function 'yes-or-no-p)
                 (lambda (_prompt) (setq prompted t) t))
                ((symbol-function 'claude-repl--kill-session) #'ignore)
                ((symbol-function 'persp-get-by-name) (lambda (_n) nil))
                ((symbol-function 'force-mode-line-update) #'ignore))
        (claude-repl-kill-workspace)
        (should-not prompted)
        (should-not (claude-repl--ws-live-p "doomed"))
        (should (claude-repl--ws-get "doomed" :nuked-at))))))

(ert-deftest claude-repl-cmd-test-kill-workspace/kills-session-and-tombstones-hashmap ()
  "kill-workspace kills session, kills persp workspace, and tombstones hashmap entry.
Same tombstone semantics as nuke — `--ws-del' is the single teardown
primitive both routes through."
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
                ((symbol-function '+workspace-exists-p) (lambda (_n) t))
                ((symbol-function '+workspace/kill)
                 (lambda (ws) (setq persp-killed ws)))
                ((symbol-function 'force-mode-line-update) #'ignore))
        (claude-repl-kill-workspace)
        (should (equal session-killed "doomed"))
        (should (equal persp-killed "doomed"))
        (should-not (claude-repl--ws-live-p "doomed"))
        (should (claude-repl--ws-get "doomed" :nuked-at))))))

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
            (let ((data (plist-get (claude-repl--read-workspace-snapshot snapshot-file)
                                   :workspaces)))
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
            (let ((data (plist-get (claude-repl--read-workspace-snapshot snapshot-file)
                                   :workspaces)))
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
            (let ((data (plist-get (claude-repl--read-workspace-snapshot snapshot-file)
                                   :workspaces)))
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
            ;; File contents are unchanged — still the legacy seed layout.
            (let ((data (plist-get (claude-repl--read-workspace-snapshot snapshot-file)
                                   :workspaces)))
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
            (let ((data (plist-get (claude-repl--read-workspace-snapshot snapshot-file)
                                   :workspaces)))
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
            (let* ((data (plist-get (claude-repl--read-workspace-snapshot snapshot-file)
                                    :workspaces))
                   (entry (assoc "ws-a" data)))
              (should entry)
              (should (equal (plist-get (cdr entry) :project-dir) "/tmp/a"))
              (should-not (plist-member (cdr entry) :priority))))
        (delete-file snapshot-file)))))

(ert-deftest claude-repl-cmd-test-save-workspace-snapshot/one-entry-per-line ()
  "Save writes each workspace entry on its own line for human-readable diffs.
The current format wraps entries in a `:workspaces' key inside a top-level
plist that also carries `:merge-queue'; the workspace list portion still
puts one entry per line so per-workspace diffs stay tight."
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
                   (ws-line-count
                    (cl-count-if (lambda (line)
                                   (string-match-p "(\"ws[0-9]+\"" line))
                                 (split-string raw "\n"))))
              ;; Each of the three workspace entries appears on its own
              ;; line inside the :workspaces sub-list (order is hash-key
              ;; dependent and intentionally not asserted here).
              (should (= 3 ws-line-count))
              ;; Round-trip cleanly through the new reader.
              (let ((parsed (claude-repl--read-workspace-snapshot snapshot-file)))
                (should (= 3 (length (plist-get parsed :workspaces)))))))
        (delete-file snapshot-file)))))

;;;; ---- Tests: read-workspace-snapshot (format normalizer) ----

(ert-deftest claude-repl-cmd-test-read-workspace-snapshot/legacy-list-format ()
  "Legacy `((NAME :project-dir DIR) ...)' files normalize to a plist with
the entries under :workspaces and a nil :merge-queue."
  (let ((file (make-temp-file "claude-snap-")))
    (unwind-protect
        (progn
          (claude-repl--write-sexp-file
           file '(("ws-a" :project-dir "/tmp/a")
                  ("ws-b" :project-dir "/tmp/b")))
          (let ((parsed (claude-repl--read-workspace-snapshot file)))
            (should (= 2 (length (plist-get parsed :workspaces))))
            (should (null (plist-get parsed :merge-queue)))))
      (delete-file file))))

(ert-deftest claude-repl-cmd-test-read-workspace-snapshot/plist-format ()
  "New plist files round-trip through the reader with both keys intact."
  (let ((file (make-temp-file "claude-snap-")))
    (unwind-protect
        (progn
          (claude-repl--write-sexp-file
           file '(:workspaces (("ws-a" :project-dir "/tmp/a"))
                  :merge-queue ((:source-ws "ws-a" :silent t :auto-resolve nil))))
          (let ((parsed (claude-repl--read-workspace-snapshot file)))
            (should (equal (plist-get parsed :workspaces)
                           '(("ws-a" :project-dir "/tmp/a"))))
            (should (equal (plist-get parsed :merge-queue)
                           '((:source-ws "ws-a" :silent t :auto-resolve nil))))))
      (delete-file file))))

(ert-deftest claude-repl-cmd-test-read-workspace-snapshot/missing-file-returns-nil ()
  "Reader returns nil for a non-existent file (no error)."
  (should-not (claude-repl--read-workspace-snapshot "/nonexistent/snap.el")))

;;;; ---- Tests: write-workspace-snapshot (merge-queue persistence) ----

(ert-deftest claude-repl-cmd-test-write-workspace-snapshot/persists-merge-queue ()
  "Writer round-trips `claude-repl--merge-queue' into the snapshot file
so a later read restores the FIFO."
  (claude-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "claude-snap-")))
      (unwind-protect
          (let ((claude-repl-workspace-snapshot-file snapshot-file)
                (claude-repl--merge-queue
                 '((:source-ws "ws-a" :silent t :auto-resolve nil)
                   (:source-ws "ws-b" :silent nil :auto-resolve t))))
            (claude-repl--ws-put "ws-a" :project-dir "/tmp/a")
            (claude-repl--ws-put "ws-b" :project-dir "/tmp/b")
            (claude-repl-save-workspace-snapshot)
            (let* ((parsed (claude-repl--read-workspace-snapshot snapshot-file))
                   (mq (plist-get parsed :merge-queue)))
              (should (= 2 (length mq)))
              (should (equal (plist-get (nth 0 mq) :source-ws) "ws-a"))
              (should (eq    (plist-get (nth 0 mq) :silent) t))
              (should (eq    (plist-get (nth 0 mq) :auto-resolve) nil))
              (should (equal (plist-get (nth 1 mq) :source-ws) "ws-b"))
              (should (eq    (plist-get (nth 1 mq) :silent) nil))
              (should (eq    (plist-get (nth 1 mq) :auto-resolve) t))))
        (delete-file snapshot-file)))))

(ert-deftest claude-repl-cmd-test-write-workspace-snapshot/empty-merge-queue ()
  "An empty live queue writes an empty :merge-queue list — not omitted."
  (claude-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "claude-snap-")))
      (unwind-protect
          (let ((claude-repl-workspace-snapshot-file snapshot-file)
                (claude-repl--merge-queue nil))
            (claude-repl--ws-put "ws-a" :project-dir "/tmp/a")
            (claude-repl-save-workspace-snapshot)
            (let ((parsed (claude-repl--read-workspace-snapshot snapshot-file)))
              ;; :merge-queue is present and explicitly an empty list.
              (should (plist-member parsed :merge-queue))
              (should (null (plist-get parsed :merge-queue)))))
        (delete-file snapshot-file)))))

;;;; ---- Tests: snapshot-restore-merge-queue ----

(ert-deftest claude-repl-cmd-test-snapshot-restore-merge-queue/repopulates-live-queue ()
  "Restore copies the saved entries into `claude-repl--merge-queue' in order."
  (claude-repl-test--with-clean-state
    (let ((claude-repl--merge-queue nil))
      (claude-repl--ws-put "ws-a" :project-dir "/tmp/a")
      (claude-repl--ws-put "ws-b" :project-dir "/tmp/b")
      (claude-repl--snapshot-restore-merge-queue
       '((:source-ws "ws-a" :silent t :auto-resolve nil)
         (:source-ws "ws-b" :silent nil :auto-resolve t)))
      (should (= 2 (length claude-repl--merge-queue)))
      (should (equal (plist-get (nth 0 claude-repl--merge-queue) :source-ws) "ws-a"))
      (should (equal (plist-get (nth 1 claude-repl--merge-queue) :source-ws) "ws-b")))))

(ert-deftest claude-repl-cmd-test-snapshot-restore-merge-queue/remarks-queued-state ()
  "Restore re-applies `:repl-state :merge-queued' on each surviving ws so
the drawer's MERGING bucket shows them again post-restart."
  (claude-repl-test--with-clean-state
    (let ((claude-repl--merge-queue nil))
      (claude-repl--ws-put "ws-a" :project-dir "/tmp/a")
      (claude-repl--snapshot-restore-merge-queue
       '((:source-ws "ws-a" :silent t :auto-resolve t)))
      (should (eq :merge-queued (claude-repl--ws-get "ws-a" :repl-state))))))

(ert-deftest claude-repl-cmd-test-snapshot-restore-merge-queue/drops-vanished-ws ()
  "Entries whose `:source-ws' no longer exists in `claude-repl--workspaces'
are dropped (a workspace was removed between sessions)."
  (claude-repl-test--with-clean-state
    (let ((claude-repl--merge-queue nil))
      (claude-repl--ws-put "ws-a" :project-dir "/tmp/a")
      ;; ws-gone is not in the workspaces hash.
      (claude-repl--snapshot-restore-merge-queue
       '((:source-ws "ws-a" :silent t :auto-resolve t)
         (:source-ws "ws-gone" :silent t :auto-resolve t)))
      (should (= 1 (length claude-repl--merge-queue)))
      (should (equal (plist-get (car claude-repl--merge-queue) :source-ws) "ws-a")))))

(ert-deftest claude-repl-cmd-test-snapshot-restore-merge-queue/empty-input-noop ()
  "Restore with nil input leaves the live queue untouched."
  (claude-repl-test--with-clean-state
    (let ((claude-repl--merge-queue
           '((:source-ws "preexisting" :silent nil :auto-resolve nil))))
      (claude-repl--snapshot-restore-merge-queue nil)
      ;; Existing queue is unchanged.
      (should (= 1 (length claude-repl--merge-queue))))))

;;;; ---- Tests: load-workspace-snapshot (merge-queue restoration) ----

(ert-deftest claude-repl-cmd-test-load-workspace-snapshot/restores-merge-queue ()
  "Loader populates `claude-repl--merge-queue' from the snapshot file's
:merge-queue at the end of the load (in `--snapshot-load-finish')."
  (claude-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "claude-snap-"))
          (real-dir (make-temp-file "claude-proj-" t))
          (claude-repl--merge-queue nil))
      (unwind-protect
          (let ((claude-repl-workspace-snapshot-file snapshot-file))
            (claude-repl--write-sexp-file
             snapshot-file
             `(:workspaces (("ws-a" :project-dir ,real-dir))
               :merge-queue ((:source-ws "ws-a" :silent t :auto-resolve nil))))
            (cl-letf (((symbol-function 'claude-repl--establish-workspace)
                       (lambda (ws dir)
                         (claude-repl--ws-put ws :project-dir dir)))
                      ((symbol-function 'claude-repl--snapshot-load-ws-ready-p)
                       (lambda (_ws) t)))
              (claude-repl-load-workspace-snapshot)
              (should (= 1 (length claude-repl--merge-queue)))
              (should (equal (plist-get (car claude-repl--merge-queue) :source-ws)
                             "ws-a"))))
        (delete-file snapshot-file)
        (delete-directory real-dir t)))))

;;;; ---- Tests: drain-merge-queue interactive command ----

(ert-deftest claude-repl-cmd-test-drain-merge-queue/empty-queue-messages ()
  "With an empty queue the command emits a `message' and does not call
the internal drain."
  (claude-repl-test--with-clean-state
    (let ((claude-repl--merge-queue nil)
          (drain-called nil))
      (cl-letf (((symbol-function 'claude-repl--drain-merge-queue)
                 (lambda () (setq drain-called t)))
                ((symbol-function 'claude-repl--any-cherry-pick-in-progress-p)
                 (lambda () nil)))
        (claude-repl-drain-merge-queue)
        (should-not drain-called)))))

(ert-deftest claude-repl-cmd-test-drain-merge-queue/cherry-pick-active-errors ()
  "With a cherry-pick in progress the command refuses to drain (user-error)."
  (claude-repl-test--with-clean-state
    (let ((claude-repl--merge-queue
           '((:source-ws "ws-a" :silent t :auto-resolve t)))
          (drain-called nil))
      (cl-letf (((symbol-function 'claude-repl--drain-merge-queue)
                 (lambda () (setq drain-called t)))
                ((symbol-function 'claude-repl--any-cherry-pick-in-progress-p)
                 (lambda () t)))
        (should-error (claude-repl-drain-merge-queue) :type 'user-error)
        (should-not drain-called)))))

(ert-deftest claude-repl-cmd-test-drain-merge-queue/dispatches-when-safe ()
  "With a non-empty queue and no live cherry-pick the command calls
`claude-repl--drain-merge-queue' to dispatch the next entry."
  (claude-repl-test--with-clean-state
    (let ((claude-repl--merge-queue
           '((:source-ws "ws-a" :silent t :auto-resolve t)))
          (drain-called nil))
      (cl-letf (((symbol-function 'claude-repl--drain-merge-queue)
                 (lambda () (setq drain-called t)))
                ((symbol-function 'claude-repl--any-cherry-pick-in-progress-p)
                 (lambda () nil)))
        (claude-repl-drain-merge-queue)
        (should drain-called)))))

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

;;;; ---- claude-repl--snapshot-load-close-main ----

(ert-deftest claude-repl-cmd-test-snapshot-load-finish/closes-main-when-exists ()
  "After finish, the leftover `main' workspace artifact is killed via
`+workspace/kill' when it still exists."
  (claude-repl-test--with-clean-state
    (let ((killed nil))
      (setq claude-repl--snapshot-load-state
            (list :queue nil :origin nil :awaiting nil
                  :loaded 0 :skipped 0 :total 0 :timeout-timer nil))
      (cl-letf (((symbol-function '+workspace-exists-p)
                 (lambda (name) (equal name "main")))
                ((symbol-function '+workspace/kill)
                 (lambda (name) (setq killed name)))
                (+workspaces-main "main"))
        (claude-repl--snapshot-load-finish)
        (should (equal killed "main"))))))

(ert-deftest claude-repl-cmd-test-snapshot-load-finish/main-missing-is-noop ()
  "Finish is a no-op for the main-close step when `main' doesn't exist."
  (claude-repl-test--with-clean-state
    (let ((kill-calls 0))
      (setq claude-repl--snapshot-load-state
            (list :queue nil :origin nil :awaiting nil
                  :loaded 0 :skipped 0 :total 0 :timeout-timer nil))
      (cl-letf (((symbol-function '+workspace-exists-p) (lambda (_n) nil))
                ((symbol-function '+workspace/kill)
                 (lambda (_n) (cl-incf kill-calls)))
                (+workspaces-main "main"))
        (claude-repl--snapshot-load-finish)
        (should (= 0 kill-calls))))))

(ert-deftest claude-repl-cmd-test-snapshot-load-finish/close-main-error-swallowed ()
  "An error from `+workspace/kill' on main is logged but never propagated."
  (claude-repl-test--with-clean-state
    (setq claude-repl--snapshot-load-state
          (list :queue nil :origin nil :awaiting nil
                :loaded 0 :skipped 0 :total 0 :timeout-timer nil))
    (cl-letf (((symbol-function '+workspace-exists-p)
               (lambda (name) (equal name "main")))
              ((symbol-function '+workspace/kill)
               (lambda (_n) (error "boom")))
              (+workspaces-main "main"))
      ;; Must not signal.
      (claude-repl--snapshot-load-finish)
      (should-not claude-repl--snapshot-load-state))))

(ert-deftest claude-repl-cmd-test-snapshot-load-finish/idempotent-skips-second-close-main ()
  "A second `--snapshot-load-finish' call (state already nil) must not
re-invoke the main-close step — close-main is part of the per-load
finalization, not a standalone teardown."
  (claude-repl-test--with-clean-state
    (let ((kill-calls 0))
      (setq claude-repl--snapshot-load-state
            (list :queue nil :origin nil :awaiting nil
                  :loaded 0 :skipped 0 :total 0 :timeout-timer nil))
      (cl-letf (((symbol-function '+workspace-exists-p)
                 (lambda (name) (equal name "main")))
                ((symbol-function '+workspace/kill)
                 (lambda (_n) (cl-incf kill-calls)))
                (+workspaces-main "main"))
        (claude-repl--snapshot-load-finish)
        (should (= 1 kill-calls))
        ;; Second call: state is nil, the early `when' short-circuits before
        ;; the close-main call, so no extra kill fires.
        (claude-repl--snapshot-load-finish)
        (should (= 1 kill-calls))))))

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

;;;; ---- nuke-one-workspace :last-killed-at stamping ----

(ert-deftest claude-repl-cmd-test-nuke-one/stamps-last-killed-at-on-ws-plist ()
  "`--nuke-one-workspace' records `:last-killed-at' on the ws plist so the
project picker (`SPC p p') can surface most-recently-killed projects
to the top and color the kill-date column."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :project-dir "/tmp/ws1")
    (let ((persp-mode nil))
      (cl-letf (((symbol-function 'claude-repl--kill-session) #'ignore)
                ((symbol-function 'claude-repl--state-save) #'ignore)
                ((symbol-function 'force-mode-line-update) #'ignore))
        (claude-repl--nuke-one-workspace "ws1" 'preserve-entry)
        (should (claude-repl--ws-get "ws1" :last-killed-at))))))

(ert-deftest claude-repl-cmd-test-nuke-one/state-save-sees-last-killed-at ()
  "`--nuke-one-workspace' stamps `:last-killed-at' BEFORE the pre-teardown
state-save runs, so the on-disk state.el reflects the kill timestamp
even if downstream teardown errors before the redundant save fires."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :project-dir "/tmp/ws1")
    (let ((saw-killed-at nil)
          (persp-mode nil))
      (cl-letf (((symbol-function 'claude-repl--kill-session) #'ignore)
                ((symbol-function 'claude-repl--state-save)
                 (lambda (ws)
                   (setq saw-killed-at (claude-repl--ws-get ws :last-killed-at))))
                ((symbol-function 'force-mode-line-update) #'ignore))
        (claude-repl--nuke-one-workspace "ws1" 'preserve-entry)
        (should saw-killed-at)))))

;;;; ---- Project picker (SPC p p) helpers ----

(ert-deftest claude-repl-cmd-test-picker-status-emoji/live-mirrors-drawer-glyph ()
  "When the project has a workspace, picker mirrors the drawer's glyph
for that workspace — keeps `SPC p p' and the drawer visually consistent
per-workspace rather than collapsing every live ws to a single 🟢."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws-live" :claude-state :thinking)
    (let ((summary '(:workspace-name "ws-live" :live-p t
                     :last-killed-at (1 2 3) :has-state t)))
      (should (equal (claude-repl--picker-status-emoji summary)
                     (alist-get :thinking claude-repl-drawer-state-icons))))))

(ert-deftest claude-repl-cmd-test-picker-status-emoji/live-merge-conflict-wins ()
  "Picker mirrors `:merge-conflict' badge for a workspace mid-conflict.
Pins the drawer's repl-state precedence (💥 wins over :claude-state)
through the picker's mirror path."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws-conflict" :claude-state :thinking)
    (claude-repl--ws-put "ws-conflict" :repl-state :merge-conflict)
    (let ((summary '(:workspace-name "ws-conflict" :live-p t
                     :has-state t)))
      (should (equal (claude-repl--picker-status-emoji summary) "💥")))))

(ert-deftest claude-repl-cmd-test-picker-status-emoji/live-dominates-killed-at ()
  "A live workspace's drawer glyph wins regardless of `:last-killed-at'
data on the summary — when `:workspace-name' is set the picker reads
the drawer glyph from the cached ws plist and never consults the
non-live 📁 fallback branch."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws-respawned" :claude-state :done)
    (let ((summary '(:workspace-name "ws-respawned"
                     :last-killed-at (1 2 3))))
      (should (equal (claude-repl--picker-status-emoji summary)
                     (alist-get :done claude-repl-drawer-state-icons))))))

(ert-deftest claude-repl-cmd-test-picker-status-emoji/no-workspace-folder ()
  "Without a live workspace the picker returns the neutral 📁 — the
status ladder collapses to live-vs-not-live because the picker does
NOT read the on-disk state file to distinguish historical kill /
dormant / never-opened.  Persisted kill/has-state hints on the summary
are ignored when no live workspace is present."
  (let ((summary '(:workspace-name nil :live-p nil
                   :last-killed-at (1 2 3))))
    (should (equal (claude-repl--picker-status-emoji summary) "📁"))))

(ert-deftest claude-repl-cmd-test-picker-status-emoji/no-workspace-nil-fields ()
  "A summary with no `:workspace-name' and nil date fields still falls
back to 📁 — the live-vs-not-live branch is the only distinction the
picker draws now that it consults only in-memory cached state."
  (let ((summary '(:workspace-name nil :live-p nil
                   :last-killed-at nil)))
    (should (equal (claude-repl--picker-status-emoji summary) "📁"))))

(ert-deftest claude-repl-cmd-test-picker-format-date/formats-real-time ()
  "Picker formats a real time value via `claude-repl--picker-date-format'.
Encoded in local time so `format-time-string' (which uses the local zone
by default) reproduces the calendar date we put in — encoding in UTC
and formatting in a non-UTC local zone would shift the date by a day."
  (let* ((time (encode-time 0 0 12 16 5 2026))
         (str (claude-repl--picker-format-date
               time claude-repl--picker-date-width
               'claude-repl-picker-created-face "----------")))
    (should (equal (substring-no-properties str) "2026-05-16"))))

(ert-deftest claude-repl-cmd-test-picker-format-date/placeholder-for-nil ()
  "When time is nil, picker emits a fixed-width dash placeholder so the
column aligns with rows that have a real date."
  (let ((str (claude-repl--picker-format-date
              nil 10 'claude-repl-picker-killed-face "----------")))
    (should (equal (substring-no-properties str) "----------"))
    (should (= (length (substring-no-properties str)) 10))))

(ert-deftest claude-repl-cmd-test-picker-format-date/applies-face ()
  "Picker propertizes the date string with the supplied face so the two
columns are visually distinct in the candidate list."
  (let ((str (claude-repl--picker-format-date
              nil 10 'claude-repl-picker-killed-face "----------")))
    (should (eq (get-text-property 0 'face str)
                'claude-repl-picker-killed-face))))

(ert-deftest claude-repl-cmd-test-picker-name-width/uses-longest-basename ()
  "When the longest basename exceeds the minimum, picker pads to that
length so date columns line up across all rows."
  (let* ((roots '("/p/a-short" "/p/this-is-a-much-longer-project-name")))
    (should (= (claude-repl--picker-name-width roots)
               (length "this-is-a-much-longer-project-name")))))

(ert-deftest claude-repl-cmd-test-picker-name-width/honors-minimum ()
  "When every basename is short, picker pads to the configured minimum so
short-name-only lists still get a readable column gutter."
  (let ((roots '("/p/a" "/p/b")))
    (should (= (claude-repl--picker-name-width roots)
               claude-repl--picker-name-min-width))))

(ert-deftest claude-repl-cmd-test-picker-time-greater-p/non-nil-vs-non-nil ()
  "Picker time comparison: newer non-nil value sorts before older."
  (should (claude-repl--picker-time-greater-p '(25000 0 0 0)
                                              '(20000 0 0 0)))
  (should-not (claude-repl--picker-time-greater-p '(20000 0 0 0)
                                                  '(25000 0 0 0))))

(ert-deftest claude-repl-cmd-test-picker-time-greater-p/nil-vs-non-nil ()
  "Picker time comparison treats nil as oldest — any real time wins."
  (should (claude-repl--picker-time-greater-p '(25000 0 0 0) nil))
  (should-not (claude-repl--picker-time-greater-p nil '(25000 0 0 0))))

(ert-deftest claude-repl-cmd-test-picker-sort-key/prefers-killed-over-created ()
  "Sort key prefers `:last-killed-at' so projects sort by their most-recent
kill, falling back to creation date only when never killed."
  (let ((summary '(:created-at (10000 0 0 0) :last-killed-at (20000 0 0 0))))
    (should (equal (claude-repl--picker-sort-key summary)
                   '(20000 0 0 0)))))

(ert-deftest claude-repl-cmd-test-picker-sort-key/falls-back-to-created ()
  "When `:last-killed-at' is nil, sort key uses `:created-at' so projects
that have never been killed still sort newest-first by creation."
  (let ((summary '(:created-at (10000 0 0 0) :last-killed-at nil)))
    (should (equal (claude-repl--picker-sort-key summary)
                   '(10000 0 0 0)))))

(ert-deftest claude-repl-cmd-test-project-has-live-workspace-p/matches ()
  "Returns t when any registered workspace points at the given root."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws" :project-dir "/tmp/proj/")
    (should (claude-repl--project-has-live-workspace-p "/tmp/proj"))))

(ert-deftest claude-repl-cmd-test-project-has-live-workspace-p/no-match ()
  "Returns nil when no registered workspace matches the given root."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws" :project-dir "/tmp/other/")
    (should-not (claude-repl--project-has-live-workspace-p "/tmp/proj"))))

(ert-deftest claude-repl-cmd-test-project-has-live-workspace-p/trailing-slash ()
  "Trailing-slash differences don't cause false negatives — both the
registered `:project-dir' and the queried root are normalized."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws" :project-dir "/tmp/proj")
    (should (claude-repl--project-has-live-workspace-p "/tmp/proj/"))))

(ert-deftest claude-repl-cmd-test-project-state-summary/reads-ws-plist-fields ()
  "Summary sources `:created-at', `:last-killed-at', `:priority' from
the live workspace plist — picker reads only in-memory cached state,
never the on-disk state file."
  (claude-repl-test--with-clean-state
    (let ((tmpdir (file-name-as-directory (make-temp-file "picker-summary-" t)))
          (created '(10000 0 0 0))
          (killed  '(20000 0 0 0)))
      (unwind-protect
          (progn
            (claude-repl--ws-put "ws-a" :project-dir tmpdir)
            (claude-repl--ws-put "ws-a" :created-at created)
            (claude-repl--ws-put "ws-a" :last-killed-at killed)
            (claude-repl--ws-put "ws-a" :priority "p1")
            (let ((summary (claude-repl--project-state-summary tmpdir)))
              (should (equal (plist-get summary :created-at) created))
              (should (equal (plist-get summary :last-killed-at) killed))
              (should (equal (plist-get summary :priority) "p1"))
              (should (plist-get summary :live-p))))
        (delete-directory tmpdir t)))))

(ert-deftest claude-repl-cmd-test-project-state-summary/no-workspace ()
  "Without a live workspace pointing at the project, summary's date and
priority fields are all nil — the picker deliberately does not consult
the state file on disk, so projects without an in-memory entry surface
with placeholder columns."
  (claude-repl-test--with-clean-state
    (let ((tmpdir (file-name-as-directory (make-temp-file "picker-no-state-" t))))
      (unwind-protect
          (let ((summary (claude-repl--project-state-summary tmpdir)))
            (should-not (plist-get summary :created-at))
            (should-not (plist-get summary :last-killed-at))
            (should-not (plist-get summary :priority))
            (should-not (plist-get summary :live-p)))
        (delete-directory tmpdir t)))))

(ert-deftest claude-repl-cmd-test-project-state-summary/ignores-on-disk-state ()
  "Pins the no-disk-IO contract: even when an on-disk state file exists
with full `:created-at' / `:last-killed-at' values, the summary
returns nil for those keys unless a live workspace also points at the
project.  Regression guard against re-introducing a state-file read
in the picker hot path."
  (claude-repl-test--with-clean-state
    (let ((tmpdir (file-name-as-directory (make-temp-file "picker-ignore-disk-" t))))
      (unwind-protect
          (progn
            (claude-repl-test--seed-file
             (claude-repl--state-file tmpdir)
             (prin1-to-string '(:created-at (10000 0 0 0)
                                :last-killed-at (20000 0 0 0)
                                :priority "p-disk")))
            (let ((summary (claude-repl--project-state-summary tmpdir)))
              (should-not (plist-get summary :created-at))
              (should-not (plist-get summary :last-killed-at))
              (should-not (plist-get summary :priority))))
        (delete-directory tmpdir t)))))

(ert-deftest claude-repl-cmd-test-project-state-summary/no-disk-read-when-no-workspace ()
  "Picker's summary path makes ZERO `claude-repl--read-sexp-file' calls
when no live workspace matches the project — anchors the cached-only
contract by counting actual function invocations rather than just
inspecting the returned plist."
  (claude-repl-test--with-clean-state
    (let ((tmpdir (file-name-as-directory (make-temp-file "picker-no-read-" t)))
          (read-count 0))
      (unwind-protect
          (cl-letf* ((orig (symbol-function 'claude-repl--read-sexp-file))
                     ((symbol-function 'claude-repl--read-sexp-file)
                      (lambda (&rest args)
                        (cl-incf read-count)
                        (apply orig args))))
            (claude-repl--project-state-summary tmpdir)
            (should (= read-count 0)))
        (delete-directory tmpdir t)))))

(ert-deftest claude-repl-cmd-test-project-state-summary/workspace-name-resolves ()
  "Summary's `:workspace-name' is the registered ws whose `:project-dir'
matches the queried root — picker uses this to mirror the drawer's
per-workspace glyph."
  (claude-repl-test--with-clean-state
    (let ((tmpdir (file-name-as-directory (make-temp-file "picker-wsname-" t))))
      (unwind-protect
          (progn
            (claude-repl--ws-put "ws-x" :project-dir tmpdir)
            (let ((summary (claude-repl--project-state-summary tmpdir)))
              (should (equal (plist-get summary :workspace-name) "ws-x"))))
        (delete-directory tmpdir t)))))

(ert-deftest claude-repl-cmd-test-project-state-summary/workspace-name-nil-when-no-ws ()
  "Summary's `:workspace-name' is nil when no workspace points at the
queried root — picker falls back to the project-level emoji ladder."
  (claude-repl-test--with-clean-state
    (let ((tmpdir (file-name-as-directory (make-temp-file "picker-no-ws-" t))))
      (unwind-protect
          (let ((summary (claude-repl--project-state-summary tmpdir)))
            (should-not (plist-get summary :workspace-name)))
        (delete-directory tmpdir t)))))

(ert-deftest claude-repl-cmd-test-build-project-picker-candidates/sorted-by-killed-at ()
  "Picker sorts entries most-recently-killed first.  Sort key is sourced
from each project's live workspace plist (cached in-memory) — no
state-file reads — so older kill ranks below newer kill regardless of
created-at."
  (claude-repl-test--with-clean-state
    (let* ((tmp-old (file-name-as-directory (make-temp-file "picker-old-kill-" t)))
           (tmp-new (file-name-as-directory (make-temp-file "picker-new-kill-" t))))
      (unwind-protect
          (progn
            (claude-repl--ws-put "ws-old" :project-dir tmp-old)
            (claude-repl--ws-put "ws-old" :created-at '(30000 0 0 0))
            (claude-repl--ws-put "ws-old" :last-killed-at '(10000 0 0 0))
            (claude-repl--ws-put "ws-new" :project-dir tmp-new)
            (claude-repl--ws-put "ws-new" :created-at '(10000 0 0 0))
            (claude-repl--ws-put "ws-new" :last-killed-at '(20000 0 0 0))
            (let* ((candidates (claude-repl--build-project-picker-candidates
                                (list tmp-old tmp-new)))
                   (roots (mapcar #'cdr candidates)))
              (should (equal (car roots) tmp-new))
              (should (equal (cadr roots) tmp-old))))
        (delete-directory tmp-old t)
        (delete-directory tmp-new t)))))

(ert-deftest claude-repl-cmd-test-build-project-picker-candidates/sorted-by-created-when-no-kill ()
  "Projects with no `:last-killed-at' sort among themselves by the
workspace plist's `:created-at' (newest-first).  Source-of-truth is
the in-memory hash, not the on-disk state file."
  (claude-repl-test--with-clean-state
    (let* ((tmp-old (file-name-as-directory (make-temp-file "picker-old-create-" t)))
           (tmp-new (file-name-as-directory (make-temp-file "picker-new-create-" t))))
      (unwind-protect
          (progn
            (claude-repl--ws-put "ws-old" :project-dir tmp-old)
            (claude-repl--ws-put "ws-old" :created-at '(10000 0 0 0))
            (claude-repl--ws-put "ws-new" :project-dir tmp-new)
            (claude-repl--ws-put "ws-new" :created-at '(20000 0 0 0))
            (let* ((candidates (claude-repl--build-project-picker-candidates
                                (list tmp-old tmp-new)))
                   (roots (mapcar #'cdr candidates)))
              (should (equal (car roots) tmp-new))
              (should (equal (cadr roots) tmp-old))))
        (delete-directory tmp-old t)
        (delete-directory tmp-new t)))))

(ert-deftest claude-repl-cmd-test-build-project-picker-candidates/non-live-sorts-last ()
  "A project without a live workspace has nil sort-key (no cached
dates).  It sinks below every project with a cached created-at — the
picker surfaces live/recently-touched workspaces and pushes
never-opened or fully-killed ones to the bottom."
  (claude-repl-test--with-clean-state
    (let* ((tmp-live (file-name-as-directory (make-temp-file "picker-live-" t)))
           (tmp-none (file-name-as-directory (make-temp-file "picker-none-" t))))
      (unwind-protect
          (progn
            (claude-repl--ws-put "ws-live" :project-dir tmp-live)
            (claude-repl--ws-put "ws-live" :created-at '(10000 0 0 0))
            (let* ((candidates (claude-repl--build-project-picker-candidates
                                (list tmp-none tmp-live)))
                   (roots (mapcar #'cdr candidates)))
              (should (equal (car roots) tmp-live))
              (should (equal (cadr roots) tmp-none))))
        (delete-directory tmp-live t)
        (delete-directory tmp-none t)))))

(ert-deftest claude-repl-cmd-test-build-project-picker-candidates/display-includes-emoji ()
  "Each candidate display string starts with the status emoji prefix so
users can scan the list at a glance.  A project with no live workspace
gets the neutral 📁 (the picker no longer distinguishes
killed/dormant/never-opened — that distinction required disk I/O)."
  (claude-repl-test--with-clean-state
    (let ((tmp (file-name-as-directory (make-temp-file "picker-display-" t))))
      (unwind-protect
          (let* ((candidates (claude-repl--build-project-picker-candidates
                              (list tmp)))
                 (display (substring-no-properties (car (car candidates)))))
            (should (string-prefix-p "📁" display)))
        (delete-directory tmp t)))))

(ert-deftest claude-repl-cmd-test-build-project-picker-candidates/display-mirrors-drawer-glyph ()
  "When a workspace points at the project, the picker's candidate display
opens with the drawer's per-workspace glyph rather than the legacy 🟢
\"any live ws\" emoji — pins the consistency the picker promises with
the drawer."
  (claude-repl-test--with-clean-state
    (let ((tmp (file-name-as-directory (make-temp-file "picker-glyph-mirror-" t))))
      (unwind-protect
          (progn
            (claude-repl--ws-put "live-ws" :project-dir tmp)
            (claude-repl--ws-put "live-ws" :claude-state :idle)
            (let* ((candidates (claude-repl--build-project-picker-candidates
                                (list tmp)))
                   (display (substring-no-properties (car (car candidates)))))
              (should (string-prefix-p
                       (alist-get :idle claude-repl-drawer-state-icons)
                       display))))
        (delete-directory tmp t)))))

(ert-deftest claude-repl-cmd-test-build-project-picker-candidates/display-aligns-columns ()
  "Picker pads the project-name column to a uniform width so the date
columns line up across rows even when basenames differ in length.

Both rows in this test are never-opened (no state file), so each
display contains two `----------' placeholders — one for created, one
for last-killed.  We anchor on the first placeholder occurrence in
each row; aligned columns mean that position is identical."
  (claude-repl-test--with-clean-state
    (let* ((short (file-name-as-directory (make-temp-file "ab-" t)))
           (long  (file-name-as-directory (make-temp-file "xyz-much-longer-basename-" t))))
      (unwind-protect
          (let* ((candidates (claude-repl--build-project-picker-candidates
                              (list short long)))
                 (displays (mapcar (lambda (c) (substring-no-properties (car c)))
                                   candidates))
                 (positions (mapcar (lambda (d)
                                      (string-match "----------" d))
                                    displays)))
            (should (apply #'= positions)))
        (delete-directory short t)
        (delete-directory long t)))))

(ert-deftest claude-repl-cmd-test-read-project-via-picker/captures-cdr ()
  "Picker returns the project root (cdr of the selected candidate), never
the propertized display string, regardless of the shape ivy passes to
the action closure."
  (claude-repl-test--with-clean-state
    (let ((tmp (file-name-as-directory (make-temp-file "picker-capture-" t))))
      (unwind-protect
          (cl-letf (((symbol-function 'projectile-relevant-known-projects)
                     (lambda () (list tmp)))
                    ((symbol-function 'ivy-read)
                     (lambda (_prompt candidates &rest args)
                       ;; Simulate ivy passing the cons cell into the
                       ;; action; the closure should setq the cdr.
                       (let ((action (plist-get args :action)))
                         (funcall action (car candidates))))))
            (should (equal (claude-repl--read-project-via-picker) tmp)))
        (delete-directory tmp t)))))

(ert-deftest claude-repl-cmd-test-read-project-via-picker/string-shape ()
  "Picker also handles the ivy-shape where the action receives the
display string rather than the cons cell — it falls back to assoc on
the candidate list."
  (claude-repl-test--with-clean-state
    (let ((tmp (file-name-as-directory (make-temp-file "picker-string-" t))))
      (unwind-protect
          (cl-letf (((symbol-function 'projectile-relevant-known-projects)
                     (lambda () (list tmp)))
                    ((symbol-function 'ivy-read)
                     (lambda (_prompt candidates &rest args)
                       (let ((action (plist-get args :action)))
                         ;; Pass just the display string.
                         (funcall action (car (car candidates)))))))
            (should (equal (claude-repl--read-project-via-picker) tmp)))
        (delete-directory tmp t)))))

;;;; ---- Indexed workspace switchers (M-1..M-9, M-0) ----
;;
;; Tests cover `claude-repl--workspace-switch-by-index' (private core)
;; and each of the public `claude-repl-workspace-switch-to-*' commands.
;; The wrappers are thin persp wrappers that intentionally ignore
;; `current-prefix-arg' — that's the property under test.

(defmacro claude-repl-cmd-test--with-switch-stubs (names switched-to &rest body)
  "Bind `+workspace-list-names' to NAMES and `+workspace-switch' to push
into SWITCHED-TO so the test can assert the destination passed by the
indexed switchers."
  (declare (indent 2))
  `(cl-letf (((symbol-function '+workspace-list-names) (lambda () ,names))
             ((symbol-function '+workspace-switch)
              (lambda (name &optional _auto-create) (push name ,switched-to))))
     ,@body))

(ert-deftest claude-repl-cmd-test-switch-by-index/picks-nth-name ()
  "switch-by-index dispatches `+workspace-switch' on the nth name."
  (let ((switched (list)))
    (claude-repl-cmd-test--with-switch-stubs
        '("a" "b" "c" "d") switched
      (claude-repl--workspace-switch-by-index 2)
      (should (equal switched '("c"))))))

(ert-deftest claude-repl-cmd-test-switch-by-index/out-of-range-user-errors ()
  "switch-by-index signals `user-error' when no workspace exists at INDEX."
  (let ((switched (list)))
    (claude-repl-cmd-test--with-switch-stubs
        '("a" "b") switched
      (should-error (claude-repl--workspace-switch-by-index 5)
                    :type 'user-error)
      (should-not switched))))

(ert-deftest claude-repl-cmd-test-switch-to-0/lands-on-first ()
  "switch-to-0 routes to the 1st workspace."
  (let ((switched (list)))
    (claude-repl-cmd-test--with-switch-stubs
        '("a" "b" "c") switched
      (claude-repl-workspace-switch-to-0)
      (should (equal switched '("a"))))))

(ert-deftest claude-repl-cmd-test-switch-to-1/lands-on-second ()
  "switch-to-1 routes to the 2nd workspace."
  (let ((switched (list)))
    (claude-repl-cmd-test--with-switch-stubs
        '("a" "b" "c") switched
      (claude-repl-workspace-switch-to-1)
      (should (equal switched '("b"))))))

(ert-deftest claude-repl-cmd-test-switch-to-2/lands-on-third ()
  "switch-to-2 routes to the 3rd workspace."
  (let ((switched (list)))
    (claude-repl-cmd-test--with-switch-stubs
        '("a" "b" "c") switched
      (claude-repl-workspace-switch-to-2)
      (should (equal switched '("c"))))))

(ert-deftest claude-repl-cmd-test-switch-to-3/lands-on-fourth ()
  "switch-to-3 routes to the 4th workspace."
  (let ((switched (list)))
    (claude-repl-cmd-test--with-switch-stubs
        '("a" "b" "c" "d") switched
      (claude-repl-workspace-switch-to-3)
      (should (equal switched '("d"))))))

(ert-deftest claude-repl-cmd-test-switch-to-4/lands-on-fifth ()
  "switch-to-4 routes to the 5th workspace."
  (let ((switched (list)))
    (claude-repl-cmd-test--with-switch-stubs
        '("a" "b" "c" "d" "e") switched
      (claude-repl-workspace-switch-to-4)
      (should (equal switched '("e"))))))

(ert-deftest claude-repl-cmd-test-switch-to-5/lands-on-sixth ()
  "switch-to-5 routes to the 6th workspace."
  (let ((switched (list)))
    (claude-repl-cmd-test--with-switch-stubs
        '("a" "b" "c" "d" "e" "f") switched
      (claude-repl-workspace-switch-to-5)
      (should (equal switched '("f"))))))

(ert-deftest claude-repl-cmd-test-switch-to-6/lands-on-seventh ()
  "switch-to-6 routes to the 7th workspace."
  (let ((switched (list)))
    (claude-repl-cmd-test--with-switch-stubs
        '("a" "b" "c" "d" "e" "f" "g") switched
      (claude-repl-workspace-switch-to-6)
      (should (equal switched '("g"))))))

(ert-deftest claude-repl-cmd-test-switch-to-7/lands-on-eighth ()
  "switch-to-7 routes to the 8th workspace."
  (let ((switched (list)))
    (claude-repl-cmd-test--with-switch-stubs
        '("a" "b" "c" "d" "e" "f" "g" "h") switched
      (claude-repl-workspace-switch-to-7)
      (should (equal switched '("h"))))))

(ert-deftest claude-repl-cmd-test-switch-to-8/lands-on-ninth ()
  "switch-to-8 routes to the 9th workspace — this is the regression
target for the M-9 misbehavior where it sometimes landed on the
final workspace instead."
  (let ((switched (list)))
    (claude-repl-cmd-test--with-switch-stubs
        '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j") switched
      (claude-repl-workspace-switch-to-8)
      (should (equal switched '("i"))))))

(ert-deftest claude-repl-cmd-test-switch-to-8/ignores-prefix-arg ()
  "switch-to-8 ignores `current-prefix-arg' — pressing a prefix-arg key
beforehand must not redirect the jump to a different index."
  (let ((switched (list))
        (current-prefix-arg 99))
    (claude-repl-cmd-test--with-switch-stubs
        '("a" "b" "c" "d" "e" "f" "g" "h" "i") switched
      (claude-repl-workspace-switch-to-8)
      (should (equal switched '("i"))))))

(ert-deftest claude-repl-cmd-test-switch-to-final/lands-on-last ()
  "switch-to-final routes to the last name in the workspace list."
  (let ((switched (list)))
    (claude-repl-cmd-test--with-switch-stubs
        '("a" "b" "c" "d") switched
      (claude-repl-workspace-switch-to-final)
      (should (equal switched '("d"))))))

(ert-deftest claude-repl-cmd-test-switch-to-final/ignores-prefix-arg ()
  "switch-to-final ignores `current-prefix-arg' — this is the regression
target for the M-0 case where `+workspace/switch-to' would consult the
prefix arg and the binding sometimes fell through to `text-scale-set'
with the \"The font hasn't been resized\" message."
  (let ((switched (list))
        (current-prefix-arg 3))
    (claude-repl-cmd-test--with-switch-stubs
        '("a" "b" "c" "d" "e") switched
      (claude-repl-workspace-switch-to-final)
      (should (equal switched '("e"))))))

(ert-deftest claude-repl-cmd-test-switch-to-final/empty-list-user-errors ()
  "switch-to-final signals `user-error' when no workspaces exist."
  (let ((switched (list)))
    (claude-repl-cmd-test--with-switch-stubs
        '() switched
      (should-error (claude-repl-workspace-switch-to-final)
                    :type 'user-error)
      (should-not switched))))

(ert-deftest claude-repl-cmd-test-switch-to-N/is-interactive ()
  "Each indexed switcher is an interactive command — required for keymap
binding to invoke it via key press."
  (dolist (fn '(claude-repl-workspace-switch-to-0
                claude-repl-workspace-switch-to-1
                claude-repl-workspace-switch-to-2
                claude-repl-workspace-switch-to-3
                claude-repl-workspace-switch-to-4
                claude-repl-workspace-switch-to-5
                claude-repl-workspace-switch-to-6
                claude-repl-workspace-switch-to-7
                claude-repl-workspace-switch-to-8
                claude-repl-workspace-switch-to-final))
    (should (commandp fn))))

(provide 'test-commands)

;;; test-commands.el ends here
