;;; test-input.el --- Tests for input.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Dedicated test file for input.el: input mode, the send pipeline,
;; metaprompt preparation and posthooks, and the /wor source-workspace
;; tag injection.  The gui frontend (xwidget webview + daemon) is the
;; only frontend; input.el carries no vterm-specific sending, scrolling,
;; or slash pass-through code, so this file carries no tests for those.

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Tests: Prefix injection counter (migrated) ----

(ert-deftest agent-repl-test-prefix-injection-counter ()
  "Prefix should be injected when counter mod period is 0."
  (agent-repl-test--with-clean-state
    (let ((agent-repl-skip-permissions t)
          (agent-repl-prefix-period 3)
          (agent-repl-command-prefix "TEST")
          (agent-repl--command-prefix "PREFIX: "))
      (agent-repl-test--with-temp-buffer " *test-prefix*"
        (let ((ws "test-ws"))
          (agent-repl--ws-put ws :input-buffer (current-buffer))
          (agent-repl--ws-put ws :prefix-counter 0)
          (insert "hello")
          ;; Counter 0 mod 3 = 0 -> prefix (bracketed as a meta span)
          (should (string-prefix-p (agent-repl--meta-wrap "PREFIX: ")
                                   (agent-repl--prepare-input ws "hello")))
          ;; Counter 1 mod 3 != 0 -> no prefix
          (agent-repl--ws-put ws :prefix-counter 1)
          (should-not (string-prefix-p (agent-repl--meta-wrap "PREFIX: ")
                                       (agent-repl--prepare-input ws "hello")))
          ;; Counter 3 mod 3 = 0 -> prefix again
          (agent-repl--ws-put ws :prefix-counter 3)
          (should (string-prefix-p (agent-repl--meta-wrap "PREFIX: ")
                                   (agent-repl--prepare-input ws "hello"))))))))

(ert-deftest agent-repl-test-prefix-counter-per-workspace ()
  "Each workspace should maintain its own prefix counter independently."
  (agent-repl-test--with-clean-state
    ;; Set different counters for two workspaces
    (agent-repl--ws-put "ws-a" :prefix-counter 7)
    (agent-repl--ws-put "ws-b" :prefix-counter 42)
    ;; They should be independent
    (should (= (agent-repl--ws-get "ws-a" :prefix-counter) 7))
    (should (= (agent-repl--ws-get "ws-b" :prefix-counter) 42))
    ;; Mutating one should not affect the other
    (agent-repl--ws-put "ws-a" :prefix-counter 8)
    (should (= (agent-repl--ws-get "ws-b" :prefix-counter) 42))))

;;;; ---- Tests: Input preparation (migrated) ----

(ert-deftest agent-repl-test-prepare-input-no-prefix-when-disabled ()
  "When `agent-repl-skip-permissions' is nil, `prepare-input' returns raw text."
  (agent-repl-test--with-clean-state
    (let ((agent-repl-skip-permissions nil)
          (agent-repl-prefix-period 1))
      (agent-repl--ws-put "ws1" :prefix-counter 0)
      (should (equal (agent-repl--prepare-input "ws1" "raw text") "raw text")))))

;;;; ---- Tests: Per-workspace metaprompt path resolution ----

(ert-deftest agent-repl-test-metaprompt-file-for-prefers-ws-worktree-copy ()
  "`agent-repl--metaprompt-file-for' points at the metaprompt inside WS's own
worktree when that worktree carries the in-repo copy."
  (agent-repl-test--with-clean-state
    (let* ((root (make-temp-file "agent-repl-mp-" t))
           (sub (expand-file-name "modules/app/agent-repl" root))
           (file (expand-file-name "metaprompt.md" sub)))
      (unwind-protect
          (progn
            (make-directory sub t)
            (with-temp-file file (insert "body"))
            (agent-repl--ws-put "ws1" :project-dir root)
            (should (equal (agent-repl--metaprompt-file-for "ws1") file)))
        (delete-directory root t)))))

(ert-deftest agent-repl-test-metaprompt-file-for-falls-back-when-ws-lacks-copy ()
  "`agent-repl--metaprompt-file-for' returns the canonical file when WS's
project dir exists but does not vendor the in-repo metaprompt."
  (agent-repl-test--with-clean-state
    (let ((root (make-temp-file "agent-repl-nomp-" t)))
      (unwind-protect
          (progn
            (agent-repl--ws-put "ws1" :project-dir root)
            (should (equal (agent-repl--metaprompt-file-for "ws1")
                           agent-repl-metaprompt-file)))
        (delete-directory root t)))))

(ert-deftest agent-repl-test-metaprompt-file-for-falls-back-without-project-dir ()
  "`agent-repl--metaprompt-file-for' returns the canonical file when WS has no
:project-dir at all."
  (agent-repl-test--with-clean-state
    (should (equal (agent-repl--metaprompt-file-for "ws-none")
                   agent-repl-metaprompt-file))))

(ert-deftest agent-repl-test-command-prefix-for-reformats-with-ws-path ()
  "`agent-repl--command-prefix-for' formats the directive with WS's own
worktree metaprompt path when WS resolves to a non-canonical file."
  (agent-repl-test--with-clean-state
    (let* ((root (make-temp-file "agent-repl-mp-" t))
           (sub (expand-file-name "modules/app/agent-repl" root))
           (file (expand-file-name "metaprompt.md" sub))
           (agent-repl-command-prefix-template "read %s now"))
      (unwind-protect
          (progn
            (make-directory sub t)
            (with-temp-file file (insert "body"))
            (agent-repl--ws-put "ws1" :project-dir root)
            (should (equal (agent-repl--command-prefix-for "ws1")
                           (format "read %s now" file))))
        (delete-directory root t)))))

(ert-deftest agent-repl-test-command-prefix-for-uses-global-for-canonical ()
  "`agent-repl--command-prefix-for' returns the shared pre-formatted prefix
when WS resolves to the canonical metaprompt file, so foreign-project
workspaces stay byte-for-byte unchanged."
  (agent-repl-test--with-clean-state
    (let ((agent-repl--command-prefix "CANONICAL PREFIX"))
      ;; ws-none has no :project-dir, so it resolves to the canonical file.
      (should (equal (agent-repl--command-prefix-for "ws-none")
                     "CANONICAL PREFIX")))))

(ert-deftest agent-repl-test-prepare-input-prefix-points-at-ws-worktree ()
  "`agent-repl--prepare-input' prepends a directive pointing at WS's OWN
worktree metaprompt, not the load-time canonical copy."
  (agent-repl-test--with-clean-state
    (let* ((root (make-temp-file "agent-repl-mp-" t))
           (sub (expand-file-name "modules/app/agent-repl" root))
           (file (expand-file-name "metaprompt.md" sub))
           (agent-repl-skip-permissions t)
           (agent-repl-prefix-period 1))
      (unwind-protect
          (progn
            (make-directory sub t)
            (with-temp-file file (insert "body"))
            (agent-repl--ws-put "ws1" :project-dir root)
            (agent-repl--ws-put "ws1" :prefix-counter 0)
            (should (string-match-p (regexp-quote file)
                                    (agent-repl--prepare-input "ws1" "hello"))))
        (delete-directory root t)))))

;;;; ---- Tests: Composite state functions (migrated) ----

(ert-deftest agent-repl-test-mark-ws-thinking-composite ()
  "`agent-repl--mark-ws-thinking' should set :thinking state."
  (agent-repl-test--with-clean-state
    (agent-repl--mark-ws-thinking "ws1")
    (should (eq (agent-repl--ws-state "ws1") :thinking))))

(ert-deftest agent-repl-test-clear-input-pushes-and-clears ()
  "`agent-repl--commit-input-buffer' pushes text to history, resets index, clears buffer when clear-p."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer " *test-clear-input*"
      (setq-local agent-repl--input-history nil)
      (setq-local agent-repl--history-index 5)
      (setq-local agent-repl--history-navigating nil)
      (agent-repl--ws-put "ws1" :input-buffer (current-buffer))
      (insert "some input text")
      (cl-letf (((symbol-function 'agent-repl--history-save) #'ignore))
        (agent-repl--commit-input-buffer "ws1" (current-buffer) "some input text" t))
      (should (equal agent-repl--input-history '("some input text")))
      (should (= agent-repl--history-index -1))
      (should (equal (buffer-string) "")))))

(ert-deftest agent-repl-test-discard-input-pushes-and-clears ()
  "`agent-repl-discard-input' pushes text, clears buffer, calls `evil-insert-state'."
  (agent-repl-test--with-temp-buffer " *test-discard*"
    (setq-local agent-repl--input-history nil)
    (setq-local agent-repl--history-index 3)
    (setq-local agent-repl--history-navigating nil)
    (insert "discard me")
    (let ((evil-called nil))
      (cl-letf (((symbol-function 'evil-insert-state)
                 (lambda () (setq evil-called t)))
                ((symbol-function 'agent-repl--history-save) #'ignore))
        (agent-repl-discard-input)
        (should (equal agent-repl--input-history '("discard me")))
        (should (= agent-repl--history-index -1))
        (should (equal (buffer-string) ""))
        (should evil-called)))))

;;;; ---- Tests: Deferred prompt queue (SPC j RET) ----

(ert-deftest agent-repl-test-queue-deferred-prompt-appends-to-queue ()
  "`agent-repl-queue-deferred-prompt' appends input buffer text to `:deferred-prompts'."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer " *test-queue-deferred-1*"
      (agent-repl--ws-put "ws1" :input-buffer (current-buffer))
      (agent-repl--ws-put "ws1" :agent-state :thinking)
      (insert "queued one")
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
                ((symbol-function 'agent-repl--history-save) #'ignore))
        (agent-repl-queue-deferred-prompt)
        (should (equal (agent-repl--ws-get "ws1" :deferred-prompts)
                       '("queued one")))))))

(ert-deftest agent-repl-test-queue-deferred-prompt-fifo-order ()
  "Successive `queue-deferred-prompt' calls preserve FIFO order."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer " *test-queue-deferred-2*"
      (agent-repl--ws-put "ws1" :input-buffer (current-buffer))
      (agent-repl--ws-put "ws1" :agent-state :thinking)
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
                ((symbol-function 'agent-repl--history-save) #'ignore))
        (insert "first")
        (agent-repl-queue-deferred-prompt)
        (insert "second")
        (agent-repl-queue-deferred-prompt)
        (insert "third")
        (agent-repl-queue-deferred-prompt)
        (should (equal (agent-repl--ws-get "ws1" :deferred-prompts)
                       '("first" "second" "third")))))))

(ert-deftest agent-repl-test-queue-deferred-prompt-clears-input-buffer ()
  "Enqueue clears the input buffer after capturing its contents."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer " *test-queue-deferred-3*"
      (agent-repl--ws-put "ws1" :input-buffer (current-buffer))
      (agent-repl--ws-put "ws1" :agent-state :thinking)
      (insert "clear me")
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
                ((symbol-function 'agent-repl--history-save) #'ignore))
        (agent-repl-queue-deferred-prompt)
        (should (equal (buffer-string) ""))))))

(ert-deftest agent-repl-test-queue-deferred-prompt-empty-input-noop ()
  "Enqueue is a no-op (no queue mutation) when the input buffer is empty or whitespace."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer " *test-queue-deferred-4*"
      (agent-repl--ws-put "ws1" :input-buffer (current-buffer))
      (agent-repl--ws-put "ws1" :agent-state :thinking)
      (agent-repl--ws-put "ws1" :deferred-prompts nil)
      ;; Buffer is empty
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
                ((symbol-function 'agent-repl--history-save) #'ignore))
        (agent-repl-queue-deferred-prompt)
        (should (null (agent-repl--ws-get "ws1" :deferred-prompts)))
        ;; Whitespace-only also a no-op
        (insert "   \n  ")
        (agent-repl-queue-deferred-prompt)
        (should (null (agent-repl--ws-get "ws1" :deferred-prompts)))))))

(ert-deftest agent-repl-test-queue-deferred-prompt-drains-when-idle ()
  "When state is `:idle` at enqueue time, the queue drains immediately."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer " *test-queue-deferred-5*"
      (agent-repl--ws-put "ws1" :input-buffer (current-buffer))
      (agent-repl--ws-put "ws1" :agent-state :idle)
      (insert "fire now")
      (let ((sent nil))
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
                  ((symbol-function 'agent-repl--history-save) #'ignore)
                  ((symbol-function 'agent-repl--send)
                   (lambda (prompt ws &rest _) (setq sent (list prompt ws)))))
          (agent-repl-queue-deferred-prompt)
          (should (equal sent '("fire now" "ws1")))
          ;; Drain popped the head, queue should be empty after.
          (should (null (agent-repl--ws-get "ws1" :deferred-prompts))))))))

(ert-deftest agent-repl-test-queue-deferred-prompt-no-drain-when-thinking ()
  "When state is `:thinking` at enqueue time, the queue is NOT drained — the prompt waits."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer " *test-queue-deferred-6*"
      (agent-repl--ws-put "ws1" :input-buffer (current-buffer))
      (agent-repl--ws-put "ws1" :agent-state :thinking)
      (insert "wait for it")
      (let ((sent nil))
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
                  ((symbol-function 'agent-repl--history-save) #'ignore)
                  ((symbol-function 'agent-repl--send)
                   (lambda (prompt ws &rest _) (setq sent (list prompt ws)))))
          (agent-repl-queue-deferred-prompt)
          (should (null sent))
          (should (equal (agent-repl--ws-get "ws1" :deferred-prompts)
                         '("wait for it"))))))))

;;;; ---- Tests: Bug regressions (migrated) ----

(ert-deftest agent-repl-test-bug7-prefix-counter-persists ()
  "Bug 7: prefix counter should persist in the workspaces hash across lookups."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :prefix-counter 42)
    (should (= (agent-repl--ws-get "ws1" :prefix-counter) 42))
    ;; Incrementing via the dedicated helper should work correctly
    (agent-repl--increment-prefix-counter "ws1")
    (should (= (agent-repl--ws-get "ws1" :prefix-counter) 43))))

;;;; ---- Tests: command-prefix content ----

(ert-deftest agent-repl-test-command-prefix-houses-commit-frequency-directive ()
  "The metaprompt is the new home of the 'commit freely and often' directive
that used to live in `agent-repl--autonomous-prompt-prefix'."
  (should (string-match-p "freely and often" agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-houses-tests-before-commit-directive ()
  "The metaprompt is the new home of the tests-pass-before-commit directive
that used to live in `agent-repl--autonomous-prompt-prefix'."
  (should (string-match-p
           "applicable tests run and pass before each commit"
           agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-houses-mutating-git-restriction ()
  "The metaprompt authorizes all git operations by default and names the
specific ops included: rebase, pull, merge, etc."
  ;; The blanket authorization statement is present.
  (should (string-match-p
           "Every git operation is authorized by default"
           agent-repl-command-prefix))
  ;; The enumerated operations that are explicitly included are all listed.
  (dolist (op '("rebase" "pull" "merge" "reset" "checkout" "cherry-pick"))
    (should (string-match-p op agent-repl-command-prefix))))

(ert-deftest agent-repl-test-command-prefix-houses-fail-hard-invariants-directive ()
  "The metaprompt must forbid defensive code or default behavior for invariants,
mandating a hard failure (assertion/throw/panic) when an invariant is violated."
  ;; The section heading announcing the directive is present.
  (should (string-match-p
           "Never add defensive code or default behavior for invariants"
           agent-repl-command-prefix))
  ;; The fail-hard instruction is present.
  (should (string-match-p "fail hard" agent-repl-command-prefix))
  ;; Papering over a violated invariant with a fallback/default is forbidden.
  (should (string-match-p
           "NEVER add defensive code, a fallback value, or default behavior"
           agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-forbids-backgrounding-without-necessity ()
  "The metaprompt must forbid backgrounding a process unless concurrent commands
run in the same invocation, or backgrounding is strictly necessary."
  (should (string-match-p
           "Never background a process unless I am immediately running concurrent commands in the same invocation, unless strictly necessary"
           agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-requires-foreground-alongside-background ()
  "The metaprompt must state the corollary that a backgrounded process requires a
foregrounded process alongside it, since backgrounding all processes is never necessary."
  (should (string-match-p
           "whenever a backgrounded process exists, a foregrounded process must exist alongside it"
           agent-repl-command-prefix))
  (should (string-match-p
           "never necessary to background ALL processes"
           agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-keeps-main-process-foregrounded ()
  "The metaprompt must state the user always wants the main process's output visible
at a glance and never wants the main process backgrounded."
  (should (string-match-p
           "main process's output visible at a glance"
           agent-repl-command-prefix))
  (should (string-match-p
           "never wants the main process backgrounded"
           agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-streams-equal-backgrounded-results-concurrently ()
  "The metaprompt must state that multiple equally significant concurrent backgrounded
processes have their results streamed back concurrently."
  (should (string-match-p
           "equally significant backgrounded processes run at once, their results are streamed back concurrently"
           agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-mandates-entire-response-is-tree ()
  "`agent-repl-command-prefix' must mandate that the entire response itself be a single TLDR tree."
  (should (stringp agent-repl-command-prefix))
  (should (string-match-p "ENTIRE response should itself be a TLDR tree"
                          agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-omits-convo-tldr ()
  "`agent-repl-command-prefix' must NOT reference a 'Convo TLDR' section."
  (should-not (string-match-p "Convo TLDR" agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-omits-tldr-tldr ()
  "`agent-repl-command-prefix' must NOT reference a recursive 'Response TLDR's TLDR' section."
  (should-not (string-match-p "Response TLDR's TLDR" agent-repl-command-prefix))
  (should-not (string-match-p "TLDR's TLDR" agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-no-separate-tldr-section ()
  "The metaprompt must state there is no separate 'Response TLDR' section, since the tree IS the whole response."
  (should (string-match-p "there is no separate 'Response TLDR' section"
                          agent-repl-command-prefix))
  (should (string-match-p "the tree IS the whole response"
                          agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-nothing-outside-tree-except-header ()
  "The metaprompt must forbid any response content outside the tree except the single header line."
  (should (string-match-p
           "Nothing may appear in the response outside the tree except the single response header line"
           agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-tldr-structure-mece-numbered-ascii-tree ()
  "TLDR spec must mandate rendering as a MECE numbered ASCII tree with a dynamically determined depth in range 1-4."
  (should (string-match-p
           "MECE numbered ASCII tree whose depth is dynamically determined"
           agent-repl-command-prefix))
  (should (string-match-p
           "permitted range of 1 to 4 inclusive"
           agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-tldr-ascii-tree-connectors ()
  "TLDR spec must call out the ASCII box-drawing connectors used to render the tree."
  (should (string-match-p
           "ASCII box-drawing connectors"
           agent-repl-command-prefix))
  (should (string-match-p "├──" agent-repl-command-prefix))
  (should (string-match-p "└──" agent-repl-command-prefix))
  (should (string-match-p "│" agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-tldr-connectors-emanate-from-label-start ()
  "TLDR spec must require child connectors to emanate from the column where the parent's label begins, not from the emoji."
  (should (string-match-p
           "MUST emanate from the column where the parent's dotted hierarchical label begins"
           agent-repl-command-prefix))
  (should (string-match-p
           "rather than from the emoji"
           agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-tldr-connector-rule-does-not-affect-numbering ()
  "Regression guard: the connector-alignment clause must explicitly state it does not influence node numbering."
  (should (string-match-p
           "MUST NOT influence how any node is numbered"
           agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-tldr-child-label-is-parent-full-label-plus-index ()
  "Regression guard: the TLDR spec must state a child's dotted label is always the parent's COMPLETE dotted label plus the child's next index."
  (should (string-match-p
           "the parent's complete dotted label followed by the child's own next index"
           agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-tldr-numeral-drop-is-forbidden ()
  "Regression guard: the TLDR spec must explicitly forbid the numeral-dropped child numbering that the prior 'first number' phrasing could induce."
  (should (string-match-p
           "NEVER the numeral-dropped"
           agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-tldr-dotted-hierarchical-numbering ()
  "TLDR spec must call out dotted hierarchical numbering for the tree's node labels, with examples up through the depth-4 cap."
  (should (string-match-p
           "dotted hierarchical numbering"
           agent-repl-command-prefix))
  (should (string-match-p
           "1\\.1 \\.\\.\\."
           agent-repl-command-prefix))
  (should (string-match-p
           "1\\.1\\.1 \\.\\.\\."
           agent-repl-command-prefix))
  (should (string-match-p
           "1\\.1\\.1\\.1 \\.\\.\\."
           agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-tldr-label-has-no-trailing-dot ()
  "TLDR spec must explicitly state a label ends on its final numeral and carries NO trailing dot."
  (should (string-match-p
           "carries NO trailing dot"
           agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-tldr-label-examples-omit-trailing-dot ()
  "Regression guard: the dotted-numbering label examples must NOT carry a trailing dot after the final numeral."
  ;; Depth-4 example label must appear without a trailing dot.
  (should-not (string-match-p "1\\.1\\.1\\.1\\. " agent-repl-command-prefix))
  ;; The numeral-drop counter-example must also be trailing-dot-free.
  (should-not (string-match-p "'2\\.1\\.'" agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-tldr-cross-ref-requires-number ()
  "TLDR spec must require that a bullet referencing another tree item ALWAYS cites that item's dotted number."
  (should (string-match-p
           "references another item in the same tree, it MUST ALWAYS cite that item's dotted hierarchical number"
           agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-tldr-cross-ref-example ()
  "TLDR spec must illustrate the cross-reference requirement with the parenthesized-number example."
  (should (string-match-p
           "want me to implement the refactor (2\\.4\\.1)"
           agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-tldr-cross-ref-forbids-name-only ()
  "TLDR spec must forbid referring to another item by name alone without its number."
  (should (string-match-p
           "Never refer to another item by name or description alone without its number"
           agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-tldr-blank-line-between-parent-and-children ()
  "TLDR spec must require a blank line separating top-level entries and no line spacing between non-top-level entries."
  (should (string-match-p
           "Top-level entries in the tree should be separated by a newline"
           agent-repl-command-prefix))
  (should (string-match-p
           "Non-top-level entries should not have any line spacing between entries"
           agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-tldr-no-blank-line-between-siblings ()
  "TLDR spec must forbid line spacing between non-top-level sibling entries."
  (should (string-match-p
           "Non-top-level entries should not have any line spacing between entries"
           agent-repl-command-prefix)))


(ert-deftest agent-repl-test-command-prefix-tldr-depth-scales-with-response-length ()
  "TLDR spec must mandate that tree depth scales with the conceptual length of the response itself."
  (should (string-match-p
           "TLDR tree depth MUST scale with the conceptual length of the response itself"
           agent-repl-command-prefix))
  (should (string-match-p
           "Very simple responses use a shallow tree (depth 1 or 2)"
           agent-repl-command-prefix))
  (should (string-match-p
           "Medium-length responses use depth 3"
           agent-repl-command-prefix))
  (should (string-match-p
           "Long, multi-section, or analysis-heavy responses use depth 4"
           agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-tldr-depth-range-and-hard-cap ()
  "TLDR spec must mandate the permitted 1-4 depth range and render a terse answer as a shallow depth-1 tree rather than padding it with manufactured depth."
  (should (string-match-p
           "within the permitted range of 1 to 4 inclusive"
           agent-repl-command-prefix))
  (should (string-match-p
           "rendered as a shallow depth-1 tree of just its root branches rather than padded out with manufactured depth"
           agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-tldr-depth-may-vary-across-branches ()
  "TLDR spec must permit (not require) depth to vary across branches within the same tree."
  (should (string-match-p
           "TLDR tree's depth MAY vary across branches within the same tree"
           agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-tldr-branch-depth-reflects-warranted-explanation ()
  "TLDR spec must say per-branch depth reflects how much that branch warrants further explanation."
  (should (string-match-p
           "depth used under any given branch reflecting how much that branch warrants further explanation"
           agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-tldr-deeper-subtrees-as-visual-cue ()
  "TLDR spec must frame deeper subtrees as a visual cue that those areas deserve more attention or warrant more detail (and perhaps more complication)."
  (should (string-match-p
           "deeper subtrees act as a visual cue"
           agent-repl-command-prefix))
  (should (string-match-p
           "deserve more attention or warrant more detail"
           agent-repl-command-prefix))
  (should (string-match-p
           "perhaps involve more complication"
           agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-tldr-shallower-subtrees-signal-self-contained ()
  "TLDR spec must frame shallower subtrees as a signal that the topic is comparatively self-contained."
  (should (string-match-p
           "shallower subtrees signal a comparatively self-contained topic"
           agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-tldr-per-branch-variability-permitted-not-required ()
  "TLDR spec must say per-branch variability is permitted and encouraged wherever useful but never required for its own sake."
  (should (string-match-p
           "permitted and encouraged wherever useful but never required for its own sake"
           agent-repl-command-prefix))
  (should (string-match-p
           "forcing uniform depth across siblings defeats the purpose of using depth as a salience signal"
           agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-tldr-concise-sixteen-node-soft-cap ()
  "TLDR spec must keep verbosity bounded with a soft cap of about 16 total nodes."
  (should (string-match-p
           "SHOULD be concise: no more than about 16 nodes in total"
           agent-repl-command-prefix))
  (should (string-match-p
           "16-node cap is a soft limit that should rarely be exceeded"
           agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-tldr-node-cap-counts-all-nodes ()
  "TLDR spec must state the 16-node cap counts ALL nodes, not just leaf nodes."
  (should (string-match-p
           "counts ALL nodes in the tree (internal and leaf alike), not just leaf nodes"
           agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-tldr-no-soft-node-count-escape ()
  "Regression guard: the prior soft 'only going larger when absolutely necessary' escape hatch must be gone from the node-count rule."
  (should-not (string-match-p
               "only going larger when absolutely necessary"
               agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-tldr-defaults-to-moderate-detail ()
  "TLDR spec must direct the tree to default to moderate detail, covering the critical points plus brief context."
  (should (string-match-p
           "tree SHOULD default to moderate detail"
           agent-repl-command-prefix))
  (should (string-match-p
           "covering the critical points"
           agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-tldr-broad-tree-can-be-expanded ()
  "TLDR spec must state that a broad tree can always be expanded by the user asking for further explanation."
  (should (string-match-p
           "A broad tree can always be expanded by the user asking for further explanation"
           agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-tldr-doubt-allows-brief-inclusion ()
  "TLDR spec must allow a brief inclusion rather than automatic omission when genuinely in doubt about a detail."
  (should (string-match-p
           "a brief inclusion is acceptable rather than automatic omission"
           agent-repl-command-prefix))
  (should (string-match-p
           "When genuinely in doubt whether a detail earns its place"
           agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-tldr-per-level-concision ()
  "TLDR spec must require entries at the same level of the tree to be a bit more concise than the fuller resolution carried by their child subtrees, without sacrificing meaning."
  (should (string-match-p
           "Entries at the same level of the tree SHOULD be a bit more concise"
           agent-repl-command-prefix))
  (should (string-match-p
           "not so much more concise that meaning is shed"
           agent-repl-command-prefix))
  (should (string-match-p
           "each level reads as a quick scan of its siblings"
           agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-tldr-root-branches-as-domain-vectors ()
  "TLDR spec must say root branches are chosen by domain directions as vectors, not ad-hoc topic selection."
  (should (string-match-p
           "domain directions as vectors"
           agent-repl-command-prefix))
  (should (string-match-p
           "orthogonal decomposition axes"
           agent-repl-command-prefix))
  (should (string-match-p
           "not by ad-hoc topic selection"
           agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-tldr-mece-branches ()
  "TLDR spec must explicitly require branches to be MECE: mutually exclusive and collectively exhaustive."
  (should (string-match-p
           "branches MUST be MECE"
           agent-repl-command-prefix))
  (should (string-match-p
           "mutually exclusive"
           agent-repl-command-prefix))
  (should (string-match-p
           "collectively exhaustive"
           agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-tldr-no-effort-impact-time-prioritization ()
  "TLDR spec must forbid prioritizing content by effort/impact/time tradeoffs."
  (should (string-match-p
           "MUST NOT prioritize effort vs\\. impact or time"
           agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-tldr-completeness-ideal-outcomes ()
  "TLDR spec must direct focus to completeness and ideal future outcomes."
  (should (string-match-p
           "completeness and ideal future outcomes"
           agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-tldr-grounded-code-or-gns-leaves ()
  "TLDR spec must require leaves to be anchored by grounded pragmatic references to code (file:line) or GNS."
  (should (string-match-p
           "fully grounded pragmatic references to code"
           agent-repl-command-prefix))
  (should (string-match-p
           "GNS knowledge at the leaves"
           agent-repl-command-prefix))
  (should (string-match-p
           "(file:line)"
           agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-tldr-leaves-are-resolution-detail ()
  "TLDR spec must anchor leaves with grounded pragmatic references to code or knowledge."
  (should (string-match-p
           "Anchor leaves with fully grounded pragmatic references to code"
           agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-tldr-root-branches-emoji-after-number ()
  "TLDR spec must require an emoji prefix on each root branch, immediately after its numeric label, and forbid emoji on non-root nodes (independent of the tree's chosen depth)."
  (should (string-match-p
           "Each root branch (AKA depth-1 node"
           agent-repl-command-prefix))
  (should (string-match-p
           "MUST be prefixed with a relevant prefixing emoji"
           agent-repl-command-prefix))
  (should (string-match-p
           "immediately after its numeric label"
           agent-repl-command-prefix))
  (should (string-match-p
           "Non-root nodes are NOT emoji-prefixed"
           agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-tldr-fix-section-dedicated-root-branch ()
  "TLDR spec must require a dedicated fix section, rendered as its own root branch, whenever a fix is available."
  (should (string-match-p
           "the TLDR tree MUST contain a dedicated fix section rendered as its own root branch"
           agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-tldr-fix-section-wrench-icon ()
  "TLDR spec must require the fix section's root branch to be prefixed with the wrench icon."
  (should (string-match-p
           "prefixed with the wrench icon 🔧 immediately after its numeric label"
           agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-omits-main-response-body-concept ()
  "Now that the entire response IS the tree, the metaprompt must NOT reference a separate 'main response body'."
  (should-not (string-match-p "main response body" agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-omits-skip-when-terse ()
  "Now that the entire response IS the tree, the metaprompt must NOT carry skip-when-terse language."
  (should-not (string-match-p "Skip the Response TLDR entirely" agent-repl-command-prefix))
  (should-not (string-match-p "omit the TLDR entirely" agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-response-comma-paren-subbullet-rule ()
  "The response style rules must extend the subbullet rule to also cover commas and parenthetical asides that bolt on additional/qualifying content."
  (should (string-match-p
           "cognizant of avoiding commas wherever a comma is serving to bolt on an additional or qualifying clause"
           agent-repl-command-prefix))
  (should (string-match-p
           "same cognizance applies to parenthetical asides"
           agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-response-comma-strong-reason-exception ()
  "The paren rule must carry an explicit exception for short labels that are part of the bullet's own name or identifier."
  (should (string-match-p
           "Exception: short labels that are part of the bullet's own name or identifier"
           agent-repl-command-prefix))
  (should (string-match-p
           "may stay inline"
           agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-response-grammatical-structure-principle ()
  "The metaprompt must state the guiding principle: keep bullets short not by simplifying content but by recursively subbulleting along english grammatical structure."
  (should (string-match-p
           "keep each bullet short not by simplifying content but by subbulleting along english grammatical structure, recursively"
           agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-bullet-text-brevity ()
  "The metaprompt must require each bullet's text to be concise."
  (should (string-match-p
           "The text on each bullet SHOULD be concise"
           agent-repl-command-prefix))
  (should (string-match-p
           "Concise bullet text is preferred"
           agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-bullet-brevity-not-branch-count ()
  "The metaprompt's bullet-brevity rule must target line length, not branch count."
  (should (string-match-p
           "This brevity targets the LENGTH of each line, not the NUMBER of branches"
           agent-repl-command-prefix))
  (should (string-match-p
           "Shortening a bullet MUST never mean dropping a branch"
           agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-tldr-header-changes-annotation ()
  "TLDR spec must require the response to open with a header line indicating whether changes were made."
  (should (string-match-p
           "The response MUST open with a single header line"
           agent-repl-command-prefix))
  (should (string-match-p
           "whether changes were made in this response"
           agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-tldr-header-changes-example ()
  "TLDR spec must give concrete parenthesized examples of the header annotation with both status emojis."
  (should (string-match-p "'Response (✏️ changes made)'" agent-repl-command-prefix))
  (should (string-match-p "'Response (👀 no changes made)'" agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-tldr-header-changes-defines-changes ()
  "TLDR spec must define what 'changes' means (edits/writes/commits, not reads/analysis)."
  (should (string-match-p
           "'Changes' means any file edits, writes, or commits"
           agent-repl-command-prefix))
  (should (string-match-p
           "read-only operations, analysis, and answers do NOT count as changes"
           agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-tldr-header-changes-emoji-required ()
  "TLDR spec must mandate the ✏️ (changes) and 👀 (no changes) status emojis."
  (should (string-match-p
           "✏️ when changes were made"
           agent-repl-command-prefix))
  (should (string-match-p
           "👀 when no changes were made"
           agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-tldr-header-changes-plain-english-after-emoji ()
  "TLDR spec must allow plain english after the mandatory emoji."
  (should (string-match-p
           "Plain english after the emoji is permitted"
           agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-tldr-entries-state-clarity ()
  "TLDR spec must require each entry's implicit language to disambiguate
current vs. proposed/suggested vs. just-changed state, so the user never has
to guess whether a bullet describes how things currently work, how they will
work after a proposed change, or how they work now after a change just made."
  (should (string-match-p
           "make clear via its implicit language"
           agent-repl-command-prefix))
  (should (string-match-p
           "current/existing state"
           agent-repl-command-prefix))
  (should (string-match-p
           "proposed/suggested future state"
           agent-repl-command-prefix))
  (should (string-match-p
           "just changed in this response"
           agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-tldr-change-bullets-include-disambiguating-context ()
  "TLDR spec must require change-describing bullets to include brief disambiguating context indicating where the change landed."
  (should (string-match-p
           "When a TLDR bullet describes a change, it MUST include brief disambiguating context"
           agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-tldr-change-bullets-x-y-z-form ()
  "TLDR spec must prescribe the 'changed X about Y in Z' bullet form for change-describing bullets, in contrast to the unanchored 'changed X about Y'."
  (should (string-match-p
           "'changed X about Y in Z'"
           agent-repl-command-prefix))
  (should (string-match-p
           "'changed X about Y'"
           agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-tldr-change-bullets-nearest-ambiguity ()
  "TLDR spec must direct the assistant to resolve only the nearest/highest-level ambiguity rather than over-qualifying with redundant scopes."
  (should (string-match-p
           "nearest level of abstraction"
           agent-repl-command-prefix))
  (should (string-match-p
           "highest-level (nearest, broadest) ambiguity that actually exists"
           agent-repl-command-prefix))
  (should (string-match-p
           "rather than over-qualifying with redundant scopes"
           agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-tldr-change-bullets-scope-ladder ()
  "TLDR spec must enumerate the scope ladder for Z: codebase/repo, then filename, then function/definition, recursively to finer scopes."
  (should (string-match-p
           "codebase or repository name when work could plausibly span multiple codebases"
           agent-repl-command-prefix))
  (should (string-match-p
           "filename when work is confined to one codebase but could plausibly span multiple files"
           agent-repl-command-prefix))
  (should (string-match-p
           "function or definition name when work is confined to one file but could plausibly span multiple functions"
           agent-repl-command-prefix))
  (should (string-match-p
           "and so on recursively to finer scopes"
           agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-brevity-via-tree-depth ()
  "The metaprompt must express that the entire response is the tree and that there is no separate prose body."
  (should (string-match-p
           "ENTIRE response should itself be a TLDR tree"
           agent-repl-command-prefix))
  (should (string-match-p
           "There is no separate prose body"
           agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-header-plus-tree-is-entire-response ()
  "TLDR spec must state the header line plus the tree beneath it constitute the entire response."
  (should (string-match-p
           "this header line together with the tree beneath it constitutes the entire response"
           agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-tldr-forbids-emdashes-and-semicolons ()
  "TLDR spec must forbid emdashes and semicolons inside TLDR bullets, framing them as a sign that detail belongs in a (recursively-nested) subbullet."
  (should (string-match-p
           "TLDR bullets MUST never contain emdashes or semicolons"
           agent-repl-command-prefix))
  (should (string-match-p
           "recursively-nested) subbullet"
           agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-tldr-forbids-greek-letters ()
  "TLDR spec must forbid Greek letters inside TLDR bullets, even though the multilevel numbering invites a mathy aesthetic."
  (should (string-match-p
           "TLDR bullets MUST never use Greek letters"
           agent-repl-command-prefix))
  (should (string-match-p
           "α, β, γ"
           agent-repl-command-prefix))
  (should (string-match-p
           "multilevel numbering"
           agent-repl-command-prefix))
  (should (string-match-p
           "mathy aesthetic"
           agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-tldr-comma-awareness ()
  "TLDR spec must require bullets to be cognizant of avoiding commas that bolt on additional/qualifying clauses, preferring subbullets instead."
  (should (string-match-p
           "cognizant of avoiding commas"
           agent-repl-command-prefix))
  (should (string-match-p
           "would more cleanly live as a subbullet"
           agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-tldr-parenthetical-awareness ()
  "TLDR spec must require bullets to be cognizant of parenthetical asides that carry supplemental detail, preferring subbullets instead."
  (should (string-match-p
           "parenthetical asides inside a TLDR bullet"
           agent-repl-command-prefix))
  (should (string-match-p
           "promoted to a (recursively-nested) subbullet"
           agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-tldr-subbullets-preferred-attachment-mechanism ()
  "TLDR spec must make recursive subbullets the preferred way to attach additional/qualifying info to a TLDR bullet, permitting a second short sentence but not a third."
  (should (string-match-p
           "preferred way to attach additional or qualifying information"
           agent-repl-command-prefix))
  (should (string-match-p
           "A second short sentence inside a single bullet is permitted where it aids clarity, but a third is not"
           agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-omits-main-body-emoji-restriction ()
  "Now that every root branch of the response tree is emoji-prefixed, the
metaprompt must NOT carry the old main-body emoji restriction."
  (should-not (string-match-p "Do NOT prefix top-level bullets with emojis"
                              agent-repl-command-prefix)))

(defun agent-repl-test--count-matches (regexp string)
  "Return how many non-overlapping times REGEXP case-sensitively matches STRING."
  (let ((case-fold-search nil)
        (count 0)
        (start 0))
    (while (string-match regexp string start)
      (setq count (1+ count)
            start (max (match-end 0) (1+ (match-beginning 0)))))
    count))

(ert-deftest agent-repl-test-command-prefix-mandates-markdown-inline-code ()
  "The metaprompt must mandate wrapping every code-like reference in markdown inline code."
  (should (string-match-p
           "Every code-like reference in the response MUST be wrapped in markdown inline code"
           agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-inline-code-covers-symbols ()
  "The inline-code directive must name code symbols as code-like references."
  (should (string-match-p
           "Code symbols are code-like references"
           agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-inline-code-covers-keybindings ()
  "The inline-code directive must name keybindings as code-like references."
  (should (string-match-p
           "Keybindings are code-like references"
           agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-inline-code-covers-filenames ()
  "The inline-code directive must name filenames and paths as code-like references."
  (should (string-match-p
           "Filenames, directories, and paths are code-like references"
           agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-inline-code-covers-commands-and-literals ()
  "The inline-code directive must name shell commands, flags, and literals as code-like references."
  (should (string-match-p
           "Shell commands, flags, and literal values are code-like references"
           agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-inline-code-governs-whole-response ()
  "The inline-code directive must govern the whole response, tree bullets and header line alike."
  (should (string-match-p
           "governs the WHOLE response"
           agent-repl-command-prefix))
  (should (string-match-p
           "every bullet of the TLDR tree at every depth, and to the response header line"
           agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-inline-code-directive-lives-in-one-place ()
  "The inline-code directive must be stated in exactly one section, with no restatement elsewhere."
  (should (= 1 (agent-repl-test--count-matches
                "^### Markdown inline code for every code-like reference$"
                agent-repl-command-prefix)))
  (should (= 1 (agent-repl-test--count-matches
                "markdown inline code"
                agent-repl-command-prefix))))

(ert-deftest agent-repl-test-command-prefix-inline-code-forbids-escaped-backticks ()
  "The inline-code directive must forbid escaping backticks and wrapping plain-english concepts."
  (should (string-match-p
           "Backticks are NEVER escaped, and a plain-english concept is NEVER wrapped in them"
           agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-omits-confusing-inline-code-phrasing ()
  "The old, confusing inline-code phrasing must not return alongside the new directive."
  (should-not (string-match-p
               "standard markdown inline code spawns"
               agent-repl-command-prefix))
  (should-not (string-match-p
               "renderable inline code"
               agent-repl-command-prefix)))

;;;; ---- Tests: metaprompt auto-reload ----

(defvar agent-repl-test--input-el
  (expand-file-name "input.el"
                    (file-name-directory
                     (or load-file-name buffer-file-name)))
  "Absolute path to the input.el under test, captured at file load time.
Computed here (top level) because `load-file-name'/`buffer-file-name'
are nil at test-run time.")

(ert-deftest agent-repl-test-command-prefix-reload-refreshes-user-facing-string ()
  "Reloading input.el must refresh `agent-repl-command-prefix' to the file's value.
Without explicit setqs, `defcustom' only initializes on first load, so a
mutated `agent-repl-command-prefix' would survive a reload — breaking
the user expectation that `agent-repl-reload-config' picks up
metaprompt edits."
  (let ((orig agent-repl-command-prefix)
        (input-el agent-repl-test--input-el))
    (unwind-protect
        (progn
          (setq agent-repl-command-prefix "SENTINEL-STALE-VALUE")
          (should (equal agent-repl-command-prefix "SENTINEL-STALE-VALUE"))
          (load input-el nil t)
          (should (equal agent-repl-command-prefix orig)))
      (setq agent-repl-command-prefix orig))))

(ert-deftest agent-repl-test-command-prefix-template-reload-refreshes ()
  "Reloading input.el must refresh `agent-repl-command-prefix-template'."
  (let ((orig agent-repl-command-prefix-template)
        (input-el agent-repl-test--input-el))
    (unwind-protect
        (progn
          (setq agent-repl-command-prefix-template "SENTINEL %s SENTINEL")
          (should (equal agent-repl-command-prefix-template "SENTINEL %s SENTINEL"))
          (load input-el nil t)
          (should (equal agent-repl-command-prefix-template orig)))
      (setq agent-repl-command-prefix-template orig))))

(ert-deftest agent-repl-test-internal-command-prefix-reload-recomputes ()
  "Reloading input.el must recompute the derived `agent-repl--command-prefix'.
This is the variable actually prepended to user input, so it must
reflect any edits to `agent-repl-command-prefix' after a reload."
  (let ((orig agent-repl--command-prefix)
        (input-el agent-repl-test--input-el))
    (unwind-protect
        (progn
          (setq agent-repl--command-prefix "SENTINEL-STALE-DERIVED")
          (should (equal agent-repl--command-prefix "SENTINEL-STALE-DERIVED"))
          (load input-el nil t)
          (should (equal agent-repl--command-prefix orig)))
      (setq agent-repl--command-prefix orig))))

(ert-deftest agent-repl-test-command-prefix-reload-reflects-md-file-edit ()
  "Reload must propagate edits to the metaprompt .md file into `agent-repl-command-prefix'.
Simulates editing the canonical metaprompt source by mutating
`standard-value' (which the defcustom value form re-reads from disk on
reload) to a literal sentinel string, and verifies the reload's setq
path picks it up.  This replaces the legacy inline-content test now
that the metaprompt body lives in `agent-repl-metaprompt-file' rather
than inline in input.el."
  (let ((orig-value agent-repl-command-prefix)
        (orig-derived agent-repl--command-prefix)
        (orig-standard (get 'agent-repl-command-prefix 'standard-value))
        (input-el agent-repl-test--input-el))
    (unwind-protect
        (progn
          ;; Simulate a metaprompt.md edit by replacing the standard-value
          ;; (which defcustom re-sets on every load) with a sentinel — the
          ;; file's content as seen by the defcustom's `with-temp-buffer'.
          (put 'agent-repl-command-prefix 'standard-value
               '((funcall (lambda () "EDITED-METAPROMPT-CONTENT"))))
          ;; Test the setq path in isolation against the doctored
          ;; standard-value (a real reload would overwrite it from disk).
          (setq agent-repl-command-prefix
                (eval (car (get 'agent-repl-command-prefix 'standard-value))))
          (should (equal agent-repl-command-prefix "EDITED-METAPROMPT-CONTENT"))
          ;; A real reload restores the file's content.
          (put 'agent-repl-command-prefix 'standard-value orig-standard)
          (load input-el nil t)
          (should (equal agent-repl-command-prefix orig-value))
          (should (equal agent-repl--command-prefix orig-derived)))
      (put 'agent-repl-command-prefix 'standard-value orig-standard)
      (setq agent-repl-command-prefix orig-value)
      (setq agent-repl--command-prefix orig-derived))))

(ert-deftest agent-repl-test-internal-command-prefix-is-plain-read-directive ()
  "`agent-repl--command-prefix' must be a plain read-the-file instruction.
The metaprompt body is read by the agent from
`agent-repl-metaprompt-file'; neither the body nor the retired
wrapper markers must be embedded inline in the derived prefix string, and
the inline prefix must NOT use \"metaprompt\" as a conceptual framing so it
cannot confuse the agent into refusing to read the file (the bare filename
inside the path is allowed and unavoidable)."
  (should (stringp agent-repl--command-prefix))
  ;; Plain read-directive is present.
  (should (string-match-p "read the file at" agent-repl--command-prefix))
  ;; The retired wrapper bookends are present nowhere, the prefix included.
  (should-not (string-match-p "start of metaprompt-read-directive"
                              agent-repl--command-prefix))
  (should-not (string-match-p "metaprompt-read-directive over"
                              agent-repl--command-prefix))
  ;; Conceptual "metaprompt" framing must be absent from the inline prefix.
  (should-not (string-match-p "metaprompt directive" agent-repl--command-prefix))
  (should-not (string-match-p "metaprompt file" agent-repl--command-prefix))
  (should-not (string-match-p "the metaprompt" agent-repl--command-prefix))
  ;; Body content (a substring unique to the metaprompt body) is NOT present.
  (should-not (string-match-p "Response TLDR MUST be rendered as a MECE"
                              agent-repl--command-prefix))
  (should-not (string-match-p "MECE numbered ASCII tree"
                              agent-repl--command-prefix)))

(ert-deftest agent-repl-test-metaprompt-file-carries-the-body ()
  "`agent-repl-command-prefix' mirrors the metaprompt .md file's body.
The body is the whole point of the file: the inline prefix is only a
directive to go read it, so a prefix that does not carry the body means
the canonical source was never loaded."
  (should (string-match-p "MECE numbered ASCII tree" agent-repl-command-prefix)))

(ert-deftest agent-repl-test-metaprompt-file-carries-no-wrapper-bookends ()
  "The metaprompt .md file carries no `metaprompt-read-directive' bookends.
The bookends once wrapped the file body (having earlier been moved there out
of the inline template), and were then removed outright — the file is read
whole, so it needs no markers delimiting where it starts and ends."
  (should-not (string-match-p "metaprompt-read-directive" agent-repl-command-prefix)))

(ert-deftest agent-repl-test-internal-command-prefix-references-in-repo-file ()
  "`agent-repl--command-prefix' must embed `agent-repl-metaprompt-file'.
The wrapper's job is to point the agent at the canonical in-repo metaprompt
path, so the body is loaded from the version-controlled file itself rather
than through an out-of-tree symlink."
  (should (string-match-p
           (regexp-quote agent-repl-metaprompt-file)
           agent-repl--command-prefix)))

(ert-deftest agent-repl-test-internal-command-prefix-references-no-external-path ()
  "The read-directive must not point outside the repository.
The metaprompt lived behind `~/.config/claude/emacs/metaprompt.md' (a
symlink into this repo) before it was referenced in-repo directly; a
directive still naming that path would resurrect the indirection."
  (should-not (string-match-p (regexp-quote "/.config/claude/")
                              agent-repl--command-prefix)))

(ert-deftest agent-repl-test-internal-command-prefix-read-directive-is-unconditional ()
  "The inline directive must instruct the agent to read the file even if already loaded.
The whole point of the periodic re-injection is to force a fresh read, so
the inline prefix must explicitly tell the agent to read the file even if it
has previously done so during the session and even if it has not changed."
  (should (string-match-p "even if you have previously done so during this session"
                          agent-repl--command-prefix))
  (should (string-match-p "even if you have already done so previously"
                          agent-repl--command-prefix))
  (should (string-match-p "they have not changed since"
                          agent-repl--command-prefix)))

(ert-deftest agent-repl-test-metaprompt-file-exists ()
  "`agent-repl-metaprompt-file' must resolve to a readable file on disk."
  (should (stringp agent-repl-metaprompt-file))
  (should (file-readable-p agent-repl-metaprompt-file)))

(ert-deftest agent-repl-test-metaprompt-file-name-is-metaprompt-md ()
  "`agent-repl-metaprompt-file' must point at a file named `metaprompt.md'."
  (should (equal (file-name-nondirectory agent-repl-metaprompt-file)
                 "metaprompt.md")))

(ert-deftest agent-repl-test-metaprompt-file-lives-beside-input-el ()
  "`agent-repl-metaprompt-file' must resolve inside the agent-repl module.
The metaprompt is version-controlled alongside the code that sends it, so
the directory the agent is pointed at is the one holding input.el itself."
  (should (file-readable-p
           (expand-file-name "input.el"
                             (file-name-directory agent-repl-metaprompt-file)))))

(ert-deftest agent-repl-test-command-prefix-matches-md-file-content ()
  "`agent-repl-command-prefix' must equal the on-disk metaprompt.md content.
The defcustom's value form reads the file, and the reload-time setq
forces a fresh read on each reload.  Any drift between the variable and
the file means a stale variable that won't reflect file edits."
  (let ((file-content (with-temp-buffer
                        (insert-file-contents agent-repl-metaprompt-file)
                        (buffer-string))))
    (should (equal agent-repl-command-prefix file-content))))

(ert-deftest agent-repl-test-metaprompt-preexisting-claim-requires-adversarial-team ()
  "The metaprompt must mandate an adversarial agent team to vet any \"pre-existing\" claim.
Any temptation to declare an issue pre-existing has to trigger an
independent adversarial investigation before the claim is accepted."
  (should (string-match-p "adversarial agent team to vet"
                          agent-repl-command-prefix))
  (should (string-match-p "disprove the \"pre-existing\" claim"
                          agent-repl-command-prefix)))

(ert-deftest agent-repl-test-metaprompt-preexisting-vetting-is-mandatory ()
  "The metaprompt must make the pre-existing vetting unconditional.
The adversarial investigation happens every time such a claim arises,
not just for failing tests."
  (should (string-match-p "MANDATORY and happens EVERY time"
                          agent-repl-command-prefix))
  (should (string-match-p "ANY issue, not just failing tests"
                          agent-repl-command-prefix)))

(ert-deftest agent-repl-test-metaprompt-preexisting-claim-surfaced-in-tldr ()
  "The metaprompt must require surfacing the vetting under the TLDR pre-existing bullet.
Both the fact that the investigation took place and its verdict must
appear as subbullets of the \"is pre-existing\" claim."
  (should (string-match-p "Pre-existing claims in the tree"
                          agent-repl-command-prefix))
  (should (string-match-p "adversarial agent team investigation took place"
                          agent-repl-command-prefix))
  (should (string-match-p "records the investigation's verdict"
                          agent-repl-command-prefix)))

;;;; ---- Tests: should-prepend-metaprompt-p ----

(ert-deftest agent-repl-test-should-prepend-metaprompt-p-all-conditions ()
  "Test the full matrix of conditions for metaprompt prepending."
  ;; Enabled + prefix set + non-exempt + counter aligned -> t
  (let ((agent-repl-skip-permissions t)
        (agent-repl-command-prefix "TEST")
        (agent-repl--command-prefix "PREFIX: ")
        (agent-repl-prefix-period 3))
    (should (agent-repl--should-prepend-metaprompt-p "hello" 0))
    ;; Counter not aligned -> nil
    (should-not (agent-repl--should-prepend-metaprompt-p "hello" 1))
    (should-not (agent-repl--should-prepend-metaprompt-p "hello" 2))
    ;; Counter aligned again -> t
    (should (agent-repl--should-prepend-metaprompt-p "hello" 6))
    ;; Force bypasses counter
    (should (agent-repl--should-prepend-metaprompt-p "hello" 1 t))
    (should (agent-repl--should-prepend-metaprompt-p "hello" 2 t))))

(ert-deftest agent-repl-test-should-prepend-nil-when-skip-permissions-off ()
  "Returns nil when `agent-repl-skip-permissions' is nil."
  (let ((agent-repl-skip-permissions nil)
        (agent-repl-command-prefix "TEST")
        (agent-repl-prefix-period 1))
    (should-not (agent-repl--should-prepend-metaprompt-p "hello" 0))))

(ert-deftest agent-repl-test-should-prepend-nil-when-no-command-prefix ()
  "Returns nil when `agent-repl-command-prefix' is nil."
  (let ((agent-repl-skip-permissions t)
        (agent-repl-command-prefix nil)
        (agent-repl-prefix-period 1))
    (should-not (agent-repl--should-prepend-metaprompt-p "hello" 0))))

(ert-deftest agent-repl-test-should-prepend-nil-for-exempt-strings ()
  "Returns nil for exempt slash commands even when conditions are met."
  (let ((agent-repl-skip-permissions t)
        (agent-repl-command-prefix "TEST")
        (agent-repl-prefix-period 1))
    (dolist (exempt '("/clear" "/usage" "/login" "/logout"))
      (should-not (agent-repl--should-prepend-metaprompt-p exempt 0)))))

(ert-deftest agent-repl-test-should-prepend-nil-for-bare-numerals ()
  "Returns nil for bare numeral inputs (e.g. '1', '42')."
  (let ((agent-repl-skip-permissions t)
        (agent-repl-command-prefix "TEST")
        (agent-repl-prefix-period 1))
    (should-not (agent-repl--should-prepend-metaprompt-p "1" 0))
    (should-not (agent-repl--should-prepend-metaprompt-p "42" 0))
    (should-not (agent-repl--should-prepend-metaprompt-p "0" 0))))

;;;; ---- Tests: slash-command-p ----

(ert-deftest agent-repl-test-slash-command-p-bare-command ()
  "A lone `/name' is a slash command."
  (should (agent-repl--slash-command-p "/compact"))
  (should (agent-repl--slash-command-p "/debug-logs")))

(ert-deftest agent-repl-test-slash-command-p-command-with-args ()
  "A `/name' followed by arguments is a slash command."
  (should (agent-repl--slash-command-p "/analyze-position e4 detailed")))

(ert-deftest agent-repl-test-slash-command-p-namespaced ()
  "A plugin-namespaced `/plugin:name' is a slash command."
  (should (agent-repl--slash-command-p "/gns-cowork:gns-bootstrap")))

(ert-deftest agent-repl-test-slash-command-p-rejects-path ()
  "A Unix path is NOT a slash command: its name run is stopped by the
second slash rather than by whitespace or end."
  (should-not (agent-repl--slash-command-p "/Users/foo/bar"))
  (should-not (agent-repl--slash-command-p "/etc/hosts")))

(ert-deftest agent-repl-test-slash-command-p-rejects-leading-whitespace ()
  "A `/' preceded by whitespace is NOT a slash command, matching the CLI's
true-message-start rule."
  (should-not (agent-repl--slash-command-p "  /compact")))

(ert-deftest agent-repl-test-slash-command-p-rejects-bare-slash ()
  "A lone `/' with no name is not yet a slash command."
  (should-not (agent-repl--slash-command-p "/")))

(ert-deftest agent-repl-test-slash-command-p-rejects-mid-message-slash ()
  "A `/' that is not at the start is not a slash command."
  (should-not (agent-repl--slash-command-p "please run /compact")))

;;;; ---- Tests: skip-metaprompt-p ----

(ert-deftest agent-repl-test-skip-metaprompt-exempt-strings ()
  "`agent-repl--skip-metaprompt-p' returns non-nil for exempt commands."
  (dolist (cmd '("/clear" "/usage" "/login" "/logout"))
    (should (agent-repl--skip-metaprompt-p cmd))))

(ert-deftest agent-repl-test-skip-metaprompt-bare-numerals ()
  "`agent-repl--skip-metaprompt-p' returns non-nil for bare numerals."
  (should (agent-repl--skip-metaprompt-p "1"))
  (should (agent-repl--skip-metaprompt-p "42"))
  (should (agent-repl--skip-metaprompt-p "007")))

(ert-deftest agent-repl-test-skip-metaprompt-trailing-whitespace ()
  "`agent-repl--skip-metaprompt-p' handles trailing whitespace."
  (should (agent-repl--skip-metaprompt-p "/clear  "))
  (should (agent-repl--skip-metaprompt-p "42\n"))
  (should (agent-repl--skip-metaprompt-p "/usage\t")))

(ert-deftest agent-repl-test-skip-metaprompt-normal-input ()
  "`agent-repl--skip-metaprompt-p' returns nil for normal input."
  (should-not (agent-repl--skip-metaprompt-p "hello world"))
  (should-not (agent-repl--skip-metaprompt-p "fix the bug"))
  (should-not (agent-repl--skip-metaprompt-p "123abc")))

(ert-deftest agent-repl-test-skip-metaprompt-any-slash-command ()
  "`agent-repl--skip-metaprompt-p' skips ANY slash command, not just the
four exempt strings."
  (should (agent-repl--skip-metaprompt-p "/debug-logs"))
  (should (agent-repl--skip-metaprompt-p "/analyze-position e4"))
  (should (agent-repl--skip-metaprompt-p "/gns-cowork:gns-bootstrap")))

(ert-deftest agent-repl-test-skip-metaprompt-unknown-slash-command ()
  "A slash-prefixed token that is not a real command is still treated as a
slash command, so it too skips the metaprompt."
  ;; `/clearsomething' used to get the metaprompt (it is not `/clear'); under
  ;; the general slash-command rule it no longer does.
  (should (agent-repl--skip-metaprompt-p "/clearsomething")))

(ert-deftest agent-repl-test-skip-metaprompt-path-is-not-a-command ()
  "A Unix path that merely starts with `/' is NOT a slash command, so it
still gets the metaprompt."
  (should-not (agent-repl--skip-metaprompt-p "/Users/foo/bar.el has a bug"))
  (should-not (agent-repl--skip-metaprompt-p "/etc/hosts")))

;;;; ---- Tests: prepare-input ----

(ert-deftest agent-repl-test-prepare-input-force-metaprompt ()
  "Force-metaprompt should prepend regardless of counter."
  (agent-repl-test--with-clean-state
    (let ((agent-repl-skip-permissions t)
          (agent-repl-prefix-period 3)
          (agent-repl-command-prefix "TEST")
          (agent-repl--command-prefix "PREFIX: "))
      ;; Counter 1 normally would not prepend with period 3
      (agent-repl--ws-put "ws1" :prefix-counter 1)
      (should (string-prefix-p (agent-repl--meta-wrap "PREFIX: ")
                               (agent-repl--prepare-input "ws1" "hello" t))))))

(ert-deftest agent-repl-test-prepare-input-nil-counter ()
  "When counter is nil (fresh workspace), should treat as 0."
  (agent-repl-test--with-clean-state
    (let ((agent-repl-skip-permissions t)
          (agent-repl-prefix-period 3)
          (agent-repl-command-prefix "TEST")
          (agent-repl--command-prefix "PREFIX: "))
      ;; No :prefix-counter set -> defaults to 0 -> 0 mod 3 = 0 -> prepend
      (should (string-prefix-p (agent-repl--meta-wrap "PREFIX: ")
                               (agent-repl--prepare-input "ws1" "hello"))))))

(ert-deftest agent-repl-test-prepare-input-exempt-input ()
  "Exempt inputs should not get the prefix even when counter is aligned."
  (agent-repl-test--with-clean-state
    (let ((agent-repl-skip-permissions t)
          (agent-repl-prefix-period 1)
          (agent-repl-command-prefix "TEST")
          (agent-repl--command-prefix "PREFIX: "))
      (agent-repl--ws-put "ws1" :prefix-counter 0)
      (should (equal (agent-repl--prepare-input "ws1" "/clear") "/clear"))
      (should (equal (agent-repl--prepare-input "ws1" "42") "42")))))

;;;; ---- Tests: workspace-command detection + source-ws tag injection ----
;;
;; `agent-repl--maybe-inject-source-ws' is the re-homed successor of the
;; slash-mode-only `--slash-maybe-inject-source-ws': it now lives on the
;; single `--prepare-input' path every send goes through (frontend or
;; not), so a `/wor...' workspace-generation/update command always
;; carries its origin, regardless of how it was typed.

(ert-deftest agent-repl-test-workspace-command-p-true ()
  "`agent-repl--workspace-command-p' is non-nil for input starting with the
`/wor' prefix."
  (should (agent-repl--workspace-command-p "/workspace-generation do a thing")))

(ert-deftest agent-repl-test-workspace-command-p-false ()
  "`agent-repl--workspace-command-p' is nil for input that doesn't start
with the `/wor' prefix."
  (should-not (agent-repl--workspace-command-p "/clear"))
  (should-not (agent-repl--workspace-command-p "hello world")))

(ert-deftest agent-repl-test-workspace-command-p-ignores-leading-whitespace ()
  "`agent-repl--workspace-command-p' trims leading whitespace before
matching the prefix, so a command typed after an accidental leading
space is still recognized."
  (should (agent-repl--workspace-command-p "  /workspace-update fix the bug")))

(ert-deftest agent-repl-test-maybe-inject-source-ws-appends-tag-for-wor-command ()
  "`agent-repl--maybe-inject-source-ws' appends the
\"[source-ws:<ws> path:<project-dir>]\" tag to a /wor command."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/repo/root")
    (should (equal (agent-repl--maybe-inject-source-ws "ws1" "/workspace-generation do it")
                   "/workspace-generation do it [source-ws:ws1 path:/repo/root]"))))

(ert-deftest agent-repl-test-maybe-inject-source-ws-leaves-non-wor-input-untouched ()
  "`agent-repl--maybe-inject-source-ws' returns RAW unchanged for non-/wor input,
even when WS has no `:project-dir' — the path lookup must not fire at all
for input the tag doesn't apply to."
  (agent-repl-test--with-clean-state
    (should (equal (agent-repl--maybe-inject-source-ws "ws1" "hello there")
                   "hello there"))))

(ert-deftest agent-repl-test-maybe-inject-source-ws-errors-when-no-project-dir ()
  "`agent-repl--maybe-inject-source-ws' signals an error for a /wor command
when WS has no `:project-dir' — the skill cannot produce a valid git_root
without it, so this must fail loudly rather than silently omit the tag."
  (agent-repl-test--with-clean-state
    (should-error (agent-repl--maybe-inject-source-ws "ws1" "/workspace-generation do it"))))

(ert-deftest agent-repl-test-prepare-input-injects-source-ws-for-wor-command ()
  "`agent-repl--prepare-input' carries the source-ws tag through for a /wor
command when the metaprompt is not due to fire."
  (agent-repl-test--with-clean-state
    (let ((agent-repl-skip-permissions nil))
      (agent-repl--ws-put "ws1" :project-dir "/repo/root")
      (should (equal (agent-repl--prepare-input "ws1" "/workspace-generation do it")
                     "/workspace-generation do it [source-ws:ws1 path:/repo/root]")))))

(ert-deftest agent-repl-test-prepare-input-wor-command-gets-tag-not-metaprompt ()
  "A /wor slash command gets its source-ws tag but NEVER the metaprompt,
even when the prefix counter would otherwise trigger the metaprompt.

`/workspace-generation' is a slash command, and slash commands run a skill
that owns its own behavior, so the harness metaprompt is never prepended;
the source-ws tag the skill actually needs is still appended."
  (agent-repl-test--with-clean-state
    (let ((agent-repl-skip-permissions t)
          (agent-repl-prefix-period 1)
          (agent-repl-command-prefix "TEST")
          (agent-repl--command-prefix "PREFIX: "))
      (agent-repl--ws-put "ws1" :prefix-counter 0)
      (agent-repl--ws-put "ws1" :project-dir "/repo/root")
      (should (equal (agent-repl--prepare-input "ws1" "/workspace-generation do it")
                     "/workspace-generation do it [source-ws:ws1 path:/repo/root]")))))

(ert-deftest agent-repl-test-send-injects-source-ws-into-dispatched-input-but-not-raw ()
  "A /wor command sent via `agent-repl--send' reaches `agent-repl--do-send'
with the source-ws tag appended, while the RAW threaded to posthooks (and
saved to history) stays untagged."
  (agent-repl-test--with-clean-state
    (let ((agent-repl-skip-permissions nil)
          (dispatched-input nil)
          (dispatched-raw nil))
      (agent-repl-test--with-temp-buffer " *test-send-source-ws*"
        (setq-local agent-repl--input-history nil)
        (setq-local agent-repl--history-index 0)
        (setq-local agent-repl--history-navigating nil)
        (insert "/workspace-generation do the thing")
        (agent-repl--ws-put "ws1" :input-buffer (current-buffer))
        (agent-repl--ws-put "ws1" :project-dir "/repo/root")
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
                  ((symbol-function 'agent-repl--do-send)
                   (lambda (_ws input raw &optional _settle)
                     (setq dispatched-input input dispatched-raw raw)))
                  ((symbol-function 'agent-repl--history-save) #'ignore))
          (agent-repl--send nil "ws1")
          (should (equal dispatched-input
                         "/workspace-generation do the thing [source-ws:ws1 path:/repo/root]"))
          (should (equal dispatched-raw "/workspace-generation do the thing"))
          ;; History records the untagged raw, not the tagged input.
          (should (equal agent-repl--input-history
                         '("/workspace-generation do the thing"))))))))

;;;; ---- Tests: increment-prefix-counter ----

(ert-deftest agent-repl-test-increment-prefix-counter-from-nil ()
  "Incrementing from nil should yield 1."
  (agent-repl-test--with-clean-state
    (agent-repl--increment-prefix-counter "ws1")
    (should (= (agent-repl--ws-get "ws1" :prefix-counter) 1))))

(ert-deftest agent-repl-test-increment-prefix-counter-from-existing ()
  "Incrementing from existing value should add 1."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :prefix-counter 10)
    (agent-repl--increment-prefix-counter "ws1")
    (should (= (agent-repl--ws-get "ws1" :prefix-counter) 11))))

;;;; ---- Tests: read-input-buffer ----

(ert-deftest agent-repl-test-read-input-buffer-returns-contents ()
  "Should return the buffer contents for a live input buffer."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer " *test-read-input*"
      (insert "hello world")
      (agent-repl--ws-put "ws1" :input-buffer (current-buffer))
      (should (equal (agent-repl--read-input-buffer "ws1") "hello world")))))

(ert-deftest agent-repl-test-read-input-buffer-nil-when-no-buffer ()
  "Should return nil when no input buffer is registered."
  (agent-repl-test--with-clean-state
    (should-not (agent-repl--read-input-buffer "ws1"))))

(ert-deftest agent-repl-test-read-input-buffer-nil-when-dead ()
  "Should return nil when the input buffer has been killed."
  (agent-repl-test--with-clean-state
    (let ((buf (get-buffer-create " *test-read-dead*")))
      (agent-repl--ws-put "ws1" :input-buffer buf)
      (kill-buffer buf)
      (should-not (agent-repl--read-input-buffer "ws1")))))

;;;; ---- Tests: append-to-input-buffer ----

(ert-deftest agent-repl-test-append-to-input-buffer ()
  "Should append text to the end of the input buffer."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer " *test-append*"
      (insert "start")
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1")))
        (agent-repl--ws-put "ws1" :input-buffer (current-buffer))
        (agent-repl--append-to-input-buffer " end")
        (should (equal (buffer-string) "start end"))))))

(ert-deftest agent-repl-test-append-to-input-buffer-no-buffer ()
  "Should be a no-op when no input buffer is registered."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1")))
      ;; Should not error
      (agent-repl--append-to-input-buffer "text"))))

;;;; ---- Tests: prepend-to-input-buffer ----

(ert-deftest agent-repl-test-prepend-to-input-buffer ()
  "Should prepend text to the start of the input buffer."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer " *test-prepend*"
      (insert "end")
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1")))
        (agent-repl--ws-put "ws1" :input-buffer (current-buffer))
        (agent-repl--prepend-to-input-buffer "start ")
        (should (equal (buffer-string) "start end"))))))

(ert-deftest agent-repl-test-prepend-to-input-buffer-no-buffer ()
  "Should be a no-op when no input buffer is registered."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1")))
      ;; Should not error
      (agent-repl--prepend-to-input-buffer "text"))))

;;;; ---- Tests: commit-input-buffer ----

(ert-deftest agent-repl-test-commit-input-buffer-no-clear ()
  "Without clear-p, buffer should not be erased."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer " *test-commit-noclear*"
      (setq-local agent-repl--input-history nil)
      (setq-local agent-repl--history-index 0)
      (setq-local agent-repl--history-navigating nil)
      (insert "keep me")
      (cl-letf (((symbol-function 'agent-repl--history-save) #'ignore))
        (agent-repl--commit-input-buffer "ws1" (current-buffer) "keep me" nil))
      (should (equal agent-repl--input-history '("keep me")))
      (should (equal (buffer-string) "keep me")))))

(ert-deftest agent-repl-test-commit-input-buffer-with-clear ()
  "With clear-p, buffer should be erased after saving history."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer " *test-commit-clear*"
      (setq-local agent-repl--input-history nil)
      (setq-local agent-repl--history-index 0)
      (setq-local agent-repl--history-navigating nil)
      (insert "clear me")
      (cl-letf (((symbol-function 'agent-repl--history-save) #'ignore))
        (agent-repl--commit-input-buffer "ws1" (current-buffer) "clear me" t))
      (should (equal agent-repl--input-history '("clear me")))
      (should (equal (buffer-string) "")))))

(ert-deftest agent-repl-test-commit-input-buffer-nil-buffer ()
  "Should be a no-op for nil input buffer."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--history-save) #'ignore))
      ;; Should not error
      (agent-repl--commit-input-buffer "ws1" nil "text" t))))

(ert-deftest agent-repl-test-commit-input-buffer-dead-buffer ()
  "Should be a no-op for killed input buffer."
  (agent-repl-test--with-clean-state
    (let ((buf (get-buffer-create " *test-commit-dead*")))
      (kill-buffer buf)
      (cl-letf (((symbol-function 'agent-repl--history-save) #'ignore))
        ;; Should not error
        (agent-repl--commit-input-buffer "ws1" buf "text" t)))))

;;;; ---- Tests: posthooks ----

(ert-deftest agent-repl-test-posthook-reset-prefix-counter ()
  "`/clear' posthook resets the prefix counter to 0."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :prefix-counter 42)
    (agent-repl--posthook-reset-prefix-counter "ws1" "/clear")
    (should (= (agent-repl--ws-get "ws1" :prefix-counter) 0))))

(ert-deftest agent-repl-test-posthook-reset-prefix-counter-fires-next-send ()
  "After `/clear' reset, the next send re-injects the metaprompt.
Counter 0 satisfies the firing condition `(zerop (mod counter period))',
mirroring the first send of a freshly-initialized workspace."
  (agent-repl-test--with-clean-state
    (let ((agent-repl-skip-permissions t)
          (agent-repl-command-prefix "PREFIX")
          (agent-repl-prefix-period 3))
      (agent-repl--ws-put "ws1" :prefix-counter 42)
      (agent-repl--posthook-reset-prefix-counter "ws1" "/clear")
      (let ((counter (agent-repl--ws-get "ws1" :prefix-counter)))
        (should (agent-repl--should-prepend-metaprompt-p "hello" counter))))))

(ert-deftest agent-repl-test-run-send-posthooks-matches-clear ()
  "`agent-repl--run-send-posthooks' fires the /clear hook."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :prefix-counter 42)
    (agent-repl--run-send-posthooks "ws1" "/clear")
    (should (= (agent-repl--ws-get "ws1" :prefix-counter) 0))))

(ert-deftest agent-repl-test-run-send-posthooks-no-match ()
  "Posthooks should not fire for non-matching input."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :prefix-counter 42)
    (agent-repl--run-send-posthooks "ws1" "hello")
    (should (= (agent-repl--ws-get "ws1" :prefix-counter) 42))))

(ert-deftest agent-repl-test-run-send-posthooks-trailing-whitespace ()
  "Posthook matching should trim trailing whitespace."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :prefix-counter 42)
    (agent-repl--run-send-posthooks "ws1" "/clear  ")
    (should (= (agent-repl--ws-get "ws1" :prefix-counter) 0))))

(ert-deftest agent-repl-test-posthook-mark-done-sets-done ()
  "`agent-repl--posthook-mark-done' sets :agent-state :done for WS."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-set-agent-state "ws1" :idle)
    (agent-repl--posthook-mark-done "ws1" "/clear")
    (should (eq (agent-repl--ws-get "ws1" :agent-state) :done))))

(ert-deftest agent-repl-test-run-send-posthooks-clear-marks-done ()
  "`/clear' through the posthook runner marks :agent-state :done."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-set-agent-state "ws1" :idle)
    (agent-repl--run-send-posthooks "ws1" "/clear")
    (should (eq (agent-repl--ws-get "ws1" :agent-state) :done))))


;;;; ---- Tests: send pipeline routes through a frontend-agnostic path ----

(ert-deftest agent-repl-test-send-frontend-workspace-without-vterm ()
  "RET-level send works in a frontend workspace with no vterm buffer at all.
`agent-repl--send' collapsed to a single straight-line pipeline (prepare
-> do-send -> commit), so there is no vterm-gated branch left to swallow
a hybrid-UI or gui-only workspace's send."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :frontend (quote gui))
    (agent-repl--ws-put "ws1" :frontend-session-id "s_1")
    (let ((sent nil)
          (committed nil))
      (cl-letf (((symbol-function 'agent-repl--do-send)
                 (lambda (_ws input _raw &optional _settle) (setq sent input)))
                ((symbol-function 'agent-repl--commit-input-buffer)
                 (lambda (&rest _) (setq committed t)))
                ;; Decoration (metaprompt prepend) is prepare-input's own
                ;; concern; this test pins only the ROUTING.
                ((symbol-function 'agent-repl--prepare-input)
                 (lambda (_ws raw &optional _force) raw)))
        (agent-repl--send "hello frontend" "ws1"))
      (should (equal sent "hello frontend"))
      (should committed))))

(ert-deftest agent-repl-test-send-frontend-empty-input-is-noop ()
  "Empty input in a frontend workspace sends nothing (no bare-RET analog)."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :frontend (quote gui))
    (agent-repl--ws-put "ws1" :frontend-session-id "s_1")
    (let ((sent nil))
      (cl-letf (((symbol-function 'agent-repl--do-send)
                 (lambda (&rest _) (setq sent t))))
        (agent-repl--send "   " "ws1"))
      (should-not sent))))

;;;; ---- Tests: do-send / interrupt-agent are pure frontend dispatch ----
;;
;; `agent-repl--do-send' and `agent-repl--interrupt-agent' carry no sending
;; or interrupting logic of their own — each is a one-line delegation to the
;; frontend-registry dispatcher (`agent-repl--frontend-dispatch-send' /
;; `agent-repl--frontend-dispatch-interrupt' in frontends.el).  The counter
;; increment, :thinking flip, posthooks, prompt-summary kickoff, and
;; :last-prompt-time stamp that a bare read of `agent-repl--do-send' might
;; suggest live here now belong to each frontend's own `:send-fn'
;; (`agent-repl--gui-send-turn' in frontend-client.el, covered by
;; test-frontend-client.el) -- testing them again here would just be
;; re-testing frontend-client.el through an extra layer of indirection.

(ert-deftest agent-repl-test-do-send-delegates-to-frontend-dispatch ()
  "`agent-repl--do-send' forwards WS, INPUT, RAW, and ON-SETTLE unchanged to
`agent-repl--frontend-dispatch-send'."
  (agent-repl-test--with-clean-state
    (let ((dispatch-args nil)
          (on-settle (lambda () 'settled)))
      (cl-letf (((symbol-function 'agent-repl--frontend-dispatch-send)
                 (lambda (&rest args) (setq dispatch-args args))))
        (agent-repl--do-send "ws1" "prepared-input" "raw-text" on-settle))
      (should (equal dispatch-args (list "ws1" "prepared-input" "raw-text" on-settle))))))

(ert-deftest agent-repl-test-do-send-on-settle-defaults-to-nil ()
  "`agent-repl--do-send' passes a nil ON-SETTLE through when the caller omits it."
  (agent-repl-test--with-clean-state
    (let ((dispatch-args nil))
      (cl-letf (((symbol-function 'agent-repl--frontend-dispatch-send)
                 (lambda (&rest args) (setq dispatch-args args))))
        (agent-repl--do-send "ws1" "input" "raw"))
      (should (equal dispatch-args '("ws1" "input" "raw" nil))))))

;;;; ---- Tests: fire-metaprompt-read (standalone re-read, e.g. post-/compact) ----

(ert-deftest agent-repl-test-fire-metaprompt-read-dispatches-wrapped-directive ()
  "`agent-repl--fire-metaprompt-read' sends the meta-wrapped read-directive.
INPUT is the command prefix bracketed as a harness-injected span; RAW is
empty so the gui draws no bubble and skips the prompt summary."
  (agent-repl-test--with-clean-state
    (let ((send-args nil)
          (agent-repl-skip-permissions t)
          (agent-repl-command-prefix "BODY")
          (agent-repl--command-prefix "READ-DIRECTIVE"))
      (cl-letf (((symbol-function 'agent-repl--do-send)
                 (lambda (&rest args) (setq send-args args))))
        (agent-repl--fire-metaprompt-read "ws1"))
      (should (equal send-args
                     (list "ws1" (agent-repl--meta-wrap "READ-DIRECTIVE") ""))))))

(ert-deftest agent-repl-test-fire-metaprompt-read-resets-prefix-counter ()
  "`agent-repl--fire-metaprompt-read' resets the prefix counter to 0.
Realigns periodic re-injection to a fresh period from this send."
  (agent-repl-test--with-clean-state
    (let ((agent-repl-skip-permissions t)
          (agent-repl-command-prefix "BODY")
          (agent-repl--command-prefix "READ-DIRECTIVE"))
      (agent-repl--ws-put "ws1" :prefix-counter 9)
      (cl-letf (((symbol-function 'agent-repl--do-send) #'ignore))
        (agent-repl--fire-metaprompt-read "ws1"))
      (should (= (agent-repl--ws-get "ws1" :prefix-counter) 0)))))

(ert-deftest agent-repl-test-fire-metaprompt-read-noop-when-skip-permissions-off ()
  "`agent-repl--fire-metaprompt-read' is a no-op when the metaprompt is disabled.
`agent-repl-skip-permissions' nil means the metaprompt system is off, so the
read-directive must not be re-established behind the user's back."
  (agent-repl-test--with-clean-state
    (let ((sent nil)
          (agent-repl-skip-permissions nil)
          (agent-repl-command-prefix "BODY")
          (agent-repl--command-prefix "READ-DIRECTIVE"))
      (agent-repl--ws-put "ws1" :prefix-counter 9)
      (cl-letf (((symbol-function 'agent-repl--do-send)
                 (lambda (&rest _) (setq sent t))))
        (agent-repl--fire-metaprompt-read "ws1"))
      (should-not sent)
      (should (= (agent-repl--ws-get "ws1" :prefix-counter) 9)))))

(ert-deftest agent-repl-test-fire-metaprompt-read-noop-when-no-command-prefix ()
  "`agent-repl--fire-metaprompt-read' is a no-op when `agent-repl-command-prefix' is nil.
Mirrors the gate `agent-repl--should-prepend-metaprompt-p' applies."
  (agent-repl-test--with-clean-state
    (let ((sent nil)
          (agent-repl-skip-permissions t)
          (agent-repl-command-prefix nil)
          (agent-repl--command-prefix "READ-DIRECTIVE"))
      (cl-letf (((symbol-function 'agent-repl--do-send)
                 (lambda (&rest _) (setq sent t))))
        (agent-repl--fire-metaprompt-read "ws1"))
      (should-not sent))))

(ert-deftest agent-repl-test-interrupt-agent-delegates-to-frontend-dispatch ()
  "`agent-repl--interrupt-agent' forwards WS to
`agent-repl--frontend-dispatch-interrupt' with the `ctrl-c' gesture."
  (agent-repl-test--with-clean-state
    (let ((dispatch-args nil))
      (cl-letf (((symbol-function 'agent-repl--frontend-dispatch-interrupt)
                 (lambda (&rest args) (setq dispatch-args args))))
        (agent-repl--interrupt-agent "ws1"))
      (should (equal dispatch-args '("ws1" ctrl-c))))))

;;;; ---- Tests: discard-or-send-interrupt ----
;;
;; `agent-repl-discard-or-send-interrupt' no longer touches vterm at all —
;; the Ctrl-C gesture routes through `agent-repl--interrupt-agent' (itself
;; a pure frontend dispatch; see above), so these tests stub that single
;; seam instead of arranging a vterm process/buffer world.

(ert-deftest agent-repl-test-discard-or-send-interrupt-empty-calls-interrupt-agent ()
  "When the input buffer is empty, C-c C-c interrupts the agent via
`agent-repl--interrupt-agent' and leaves the (already empty) buffer alone."
  (agent-repl-test--with-clean-state
    (let ((interrupted nil))
      (agent-repl-test--with-temp-buffer " *test-input-discard-empty*"
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                  ((symbol-function 'agent-repl--interrupt-agent)
                   (lambda (ws) (setq interrupted ws))))
          (agent-repl-discard-or-send-interrupt)
          (should (equal interrupted "test-ws")))))))

(ert-deftest agent-repl-test-discard-or-send-interrupt-nonempty-discards-and-interrupts ()
  "When the input buffer has text, C-c C-c BOTH discards the local draft
AND interrupts the agent.
Regression: previously only cleared the local buffer, leaving the agent's
in-flight prompt line untouched."
  (agent-repl-test--with-clean-state
    (let ((interrupted nil)
          (evil-called nil))
      (agent-repl-test--with-temp-buffer " *test-input-discard-nonempty*"
        (setq-local agent-repl--input-history nil)
        (setq-local agent-repl--history-index 0)
        (setq-local agent-repl--history-navigating nil)
        (insert "some text")
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                  ((symbol-function 'evil-insert-state)
                   (lambda () (setq evil-called t)))
                  ((symbol-function 'agent-repl--history-save) #'ignore)
                  ((symbol-function 'agent-repl--interrupt-agent)
                   (lambda (ws) (setq interrupted ws))))
          (agent-repl-discard-or-send-interrupt)
          (should (equal (buffer-string) ""))
          (should evil-called)
          (should (equal interrupted "test-ws")))))))

(ert-deftest agent-repl-test-discard-or-send-interrupt-whitespace-only-clears-buffer ()
  "When input buffer contains only whitespace, C-c C-c still clears it.
Previously `string-blank-p' treated whitespace-only as empty and skipped
`erase-buffer', leaving the user's whitespace stuck in the input."
  (agent-repl-test--with-clean-state
    (let ((interrupted nil)
          (evil-called nil))
      (agent-repl-test--with-temp-buffer " *test-input-whitespace*"
        (setq-local agent-repl--input-history nil)
        (setq-local agent-repl--history-index 0)
        (setq-local agent-repl--history-navigating nil)
        (insert "   \n\t  \n")
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                  ((symbol-function 'evil-insert-state)
                   (lambda () (setq evil-called t)))
                  ((symbol-function 'agent-repl--history-save) #'ignore)
                  ((symbol-function 'agent-repl--interrupt-agent)
                   (lambda (ws) (setq interrupted ws))))
          (agent-repl-discard-or-send-interrupt)
          (should (equal (buffer-string) ""))
          (should evil-called)
          (should (equal interrupted "test-ws")))))))

(ert-deftest agent-repl-test-discard-or-send-interrupt-thinking-nonempty-suppresses-interrupt ()
  "When the agent is :thinking AND input buffer is non-empty, C-c C-c clears
the local buffer + saves history but DOES NOT interrupt the agent.
This lets the user draft a message while the agent works and discard the
draft without cancelling the agent's in-flight response."
  (agent-repl-test--with-clean-state
    (let ((interrupted nil)
          (evil-called nil))
      (agent-repl-test--with-temp-buffer " *test-input-thinking-nonempty*"
        (agent-repl--ws-set-agent-state "test-ws" :thinking)
        (setq-local agent-repl--input-history nil)
        (setq-local agent-repl--history-index 0)
        (setq-local agent-repl--history-navigating nil)
        (insert "draft while claude works")
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                  ((symbol-function 'evil-insert-state)
                   (lambda () (setq evil-called t)))
                  ((symbol-function 'agent-repl--history-save) #'ignore)
                  ((symbol-function 'agent-repl--interrupt-agent)
                   (lambda (ws) (setq interrupted ws))))
          (agent-repl-discard-or-send-interrupt)
          (should (equal (buffer-string) ""))
          (should evil-called)
          (should-not interrupted))))))

(ert-deftest agent-repl-test-discard-or-send-interrupt-thinking-empty-still-interrupts ()
  "When the agent is :thinking but the input buffer is empty, C-c C-c still
interrupts — the suppression only applies when there is local content to
discard."
  (agent-repl-test--with-clean-state
    (let ((interrupted nil))
      (agent-repl-test--with-temp-buffer " *test-input-thinking-empty*"
        (agent-repl--ws-set-agent-state "test-ws" :thinking)
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                  ((symbol-function 'agent-repl--interrupt-agent)
                   (lambda (ws) (setq interrupted ws))))
          (agent-repl-discard-or-send-interrupt)
          (should (equal interrupted "test-ws")))))))

(ert-deftest agent-repl-test-discard-or-send-interrupt-idle-nonempty-discards-and-interrupts ()
  "When the agent is :idle (not :thinking) AND input buffer is non-empty,
C-c C-c discards the buffer AND interrupts — the full-reset behavior is
preserved outside the :thinking-with-a-draft exception."
  (agent-repl-test--with-clean-state
    (let ((interrupted nil))
      (agent-repl-test--with-temp-buffer " *test-input-idle-nonempty*"
        (agent-repl--ws-set-agent-state "test-ws" :idle)
        (setq-local agent-repl--input-history nil)
        (setq-local agent-repl--history-index 0)
        (setq-local agent-repl--history-navigating nil)
        (insert "some text")
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                  ((symbol-function 'evil-insert-state) #'ignore)
                  ((symbol-function 'agent-repl--history-save) #'ignore)
                  ((symbol-function 'agent-repl--interrupt-agent)
                   (lambda (ws) (setq interrupted ws))))
          (agent-repl-discard-or-send-interrupt)
          (should (equal (buffer-string) ""))
          (should (equal interrupted "test-ws")))))))

;;;; ---- Tests: send (integration) ----

(ert-deftest agent-repl-test-send-no-workspace-errors ()
  "`agent-repl--send' should error when no workspace is available."
  (cl-letf (((symbol-function '+workspace-current-name) (lambda () nil)))
    (should-error (agent-repl--send) :type 'error)))

(ert-deftest agent-repl-test-send-reads-from-input-buffer ()
  "`agent-repl--send' reads from the input buffer when no prompt is given."
  (agent-repl-test--with-clean-state
    (let ((sent-input nil))
      (agent-repl-test--with-temp-buffer " *test-send-input*"
        (setq-local agent-repl--input-history nil)
        (setq-local agent-repl--history-index 0)
        (setq-local agent-repl--history-navigating nil)
        (insert "from buffer")
        (agent-repl--ws-put "ws1" :input-buffer (current-buffer))
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
                  ((symbol-function 'agent-repl--do-send)
                   (lambda (_ws input _raw &optional _settle) (setq sent-input input)))
                  ((symbol-function 'agent-repl--history-save) #'ignore))
          (agent-repl--send nil "ws1")
          (should (stringp sent-input))
          ;; The input buffer should be cleared
          (should (equal (with-current-buffer (agent-repl--ws-get "ws1" :input-buffer)
                           (buffer-string))
                         "")))))))

(ert-deftest agent-repl-test-send-with-explicit-prompt ()
  "`agent-repl--send' uses the given prompt and does not clear input buffer."
  (agent-repl-test--with-clean-state
    (let ((sent-input nil))
      (agent-repl-test--with-temp-buffer " *test-send-prompt-input*"
        (setq-local agent-repl--input-history nil)
        (setq-local agent-repl--history-index 0)
        (setq-local agent-repl--history-navigating nil)
        (insert "original content")
        (agent-repl--ws-put "ws1" :input-buffer (current-buffer))
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
                  ((symbol-function 'agent-repl--do-send)
                   (lambda (_ws input _raw &optional _settle) (setq sent-input input)))
                  ((symbol-function 'agent-repl--history-save) #'ignore))
          (agent-repl--send "explicit prompt" "ws1")
          (should (stringp sent-input))
          ;; Input buffer should NOT be cleared when prompt is given
          (should (equal (with-current-buffer (agent-repl--ws-get "ws1" :input-buffer)
                           (buffer-string))
                         "original content")))))))

(ert-deftest agent-repl-test-send-skips-do-send-when-nil-raw ()
  "`agent-repl--send' skips the full-send pipeline when both prompt and input buffer are nil/empty.
Regression guard: empty input must not dispatch a metaprompt-only send via
`agent-repl--do-send'."
  (agent-repl-test--with-clean-state
    (let ((do-send-called nil))
      ;; No input buffer registered.
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
                ((symbol-function 'agent-repl--do-send)
                 (lambda (&rest _) (setq do-send-called t))))
        (agent-repl--send nil "ws1")
        (should-not do-send-called)))))

(ert-deftest agent-repl-test-send-skips-do-send-when-input-buffer-empty ()
  "`agent-repl--send' skips the full-send pipeline when the input buffer is empty.
Regression: RET in an empty input buffer used to dispatch a metaprompt-only
send whenever the prefix counter aligned with the period."
  (agent-repl-test--with-clean-state
    (let ((do-send-called nil))
      (agent-repl-test--with-temp-buffer " *test-send-empty-input*"
        ;; Input buffer registered, but contains "".
        (agent-repl--ws-put "ws1" :input-buffer (current-buffer))
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
                  ((symbol-function 'agent-repl--do-send)
                   (lambda (&rest _) (setq do-send-called t))))
          (agent-repl--send nil "ws1")
          (should-not do-send-called))))))

(ert-deftest agent-repl-test-send-skips-do-send-when-input-buffer-whitespace-only ()
  "`agent-repl--send' skips the full-send pipeline when the input buffer holds only whitespace."
  (agent-repl-test--with-clean-state
    (let ((do-send-called nil))
      (agent-repl-test--with-temp-buffer " *test-send-whitespace-input*"
        (insert "  \n\t  \n")
        (agent-repl--ws-put "ws1" :input-buffer (current-buffer))
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
                  ((symbol-function 'agent-repl--do-send)
                   (lambda (&rest _) (setq do-send-called t))))
          (agent-repl--send nil "ws1")
          (should-not do-send-called))))))

(ert-deftest agent-repl-test-send-skips-do-send-when-explicit-prompt-empty ()
  "`agent-repl--send' skips the full-send pipeline when an empty PROMPT is passed explicitly."
  (agent-repl-test--with-clean-state
    (let ((do-send-called nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
                ((symbol-function 'agent-repl--do-send)
                 (lambda (&rest _) (setq do-send-called t))))
        (agent-repl--send "" "ws1")
        (should-not do-send-called)
        (agent-repl--send "   \n  " "ws1")
        (should-not do-send-called)))))

;;; agent-repl-input-mode: mode setup

(ert-deftest agent-repl-test-agent-repl-input-mode-setup ()
  "`agent-repl-input-mode' sets header-line and installs hooks."
  (agent-repl-test--with-temp-buffer " *test-input-mode*"
    (cl-letf (((symbol-function 'agent-repl--set-buffer-background) #'ignore))
      (agent-repl-input-mode))
    ;; Header line should be set
    (should (stringp header-line-format))
    ;; Match on "history" rather than a specific keybinding glyph (e.g. "C-r")
    ;; so the assertion stays decoupled from the chord chosen — the structural
    ;; invariant is that the header advertises the history recall.
    (should (string-match-p "history" header-line-format))
    ;; after-change-functions should include history-on-change
    (should (memq #'agent-repl--history-on-change after-change-functions))))

(ert-deftest agent-repl-test-agent-repl-input-mode-fill-column-150 ()
  "`agent-repl-input-mode' sets the buffer-local `fill-column' to 150.
Pins the widened composer fill width so wrapped prose reflows to 150
columns rather than the 70-column default."
  (agent-repl-test--with-temp-buffer " *test-input-mode-fill-column*"
    (cl-letf (((symbol-function 'agent-repl--set-buffer-background) #'ignore))
      (agent-repl-input-mode))
    (should (= fill-column 150))))

(ert-deftest agent-repl-test-agent-repl-input-mode-applies-configured-background ()
  "`agent-repl-input-mode' tints the buffer with `agent-repl--input-background-color'."
  (agent-repl-test--with-temp-buffer " *test-input-mode-bg*"
    (let ((applied-color nil))
      (cl-letf (((symbol-function 'agent-repl--set-buffer-background)
                 (lambda (color) (setq applied-color color))))
        (agent-repl-input-mode))
      (should (equal applied-color (agent-repl--input-background-color))))))

(ert-deftest agent-repl-test-agent-repl-input-background-shade-is-dark ()
  "The input buffer background base shade defaults dark, not merely dim.
Pins the darkened default: a base shade at or above 24 (the prior
default) would regress the slightly-darker-input-background change back
to its old, lighter look."
  (should (< agent-repl-input-background-shade 24)))

(ert-deftest agent-repl-test-agent-repl-input-background-is-blue-tinted ()
  "The input background nudges the blue channel above the grey base.
A positive blue boost is what makes the composer very slightly blue
rather than a pure neutral grey."
  (should (> agent-repl-input-background-blue-boost 0)))

(ert-deftest agent-repl-test-agent-repl-input-background-color-is-bluer-than-grey ()
  "`agent-repl--input-background-color' has a blue channel above red and green.
Pins the blue tint at the composed-hex level: the blue byte must exceed
both the red and green bytes, which are the neutral grey base."
  (let* ((hex (agent-repl--input-background-color))
         (r (string-to-number (substring hex 1 3) 16))
         (g (string-to-number (substring hex 3 5) 16))
         (b (string-to-number (substring hex 5 7) 16)))
    (should (= r g))
    (should (> b r))
    (should (> b g))))

(ert-deftest agent-repl-test-agent-repl-input-mode-no-visual-line-mode ()
  "`agent-repl-input-mode' no longer force-enables `visual-line-mode'.
The buffer's screen-line editing tuning was removed, so the mode leaves
`visual-line-mode' at its default (nil) rather than turning it on."
  (agent-repl-test--with-temp-buffer " *test-input-mode-no-vline*"
    (cl-letf (((symbol-function 'agent-repl--set-buffer-background) #'ignore))
      (agent-repl-input-mode))
    (should-not visual-line-mode)))

(ert-deftest agent-repl-test-agent-repl-input-mode-header-omits-direct-send ()
  "The header line no longer advertises the direct-send chords.
The `(ins) <slash>/<digit>/<up>/<down>: direct send' segment, and the
slash/digit passthrough machinery it once advertised, are both gone —
the header carries no \"direct send\" advertisement at all now."
  (agent-repl-test--with-temp-buffer " *test-input-mode-header*"
    (cl-letf (((symbol-function 'agent-repl--set-buffer-background) #'ignore))
      (agent-repl-input-mode))
    (should-not (string-match-p "direct send" header-line-format))))

;;; agent-repl-input-mode: no special visual-line editing tuning

(ert-deftest agent-repl-test-agent-repl-input-mode-does-not-force-respect-visual-line ()
  "`agent-repl-input-mode' no longer forces `evil-respect-visual-line-mode'.
The screen-line operator tuning was removed, so the mode leaves
`evil-respect-visual-line-mode' at its global default rather than binding
it buffer-locally to t."
  (agent-repl-test--with-temp-buffer " *test-input-mode-vline-var*"
    (cl-letf (((symbol-function 'agent-repl--set-buffer-background) #'ignore))
      (agent-repl-input-mode))
    (should-not (local-variable-p 'evil-respect-visual-line-mode))))

(ert-deftest agent-repl-test-visual-line-bindings-const-removed ()
  "The `agent-repl--visual-line-bindings' data table no longer exists.
The visual-line motion remaps (j/k/0/^/$/V and their g-prefixed logical
counterparts) were removed, so the defconst that declared them is gone."
  (should-not (boundp 'agent-repl--visual-line-bindings)))

;;; discard-input with empty buffer

(ert-deftest agent-repl-test-discard-input-empty-buffer ()
  "`agent-repl-discard-input' on an empty buffer should push empty string to history and remain empty."
  (agent-repl-test--with-temp-buffer " *test-discard-empty*"
    (setq-local agent-repl--input-history nil)
    (setq-local agent-repl--history-index 0)
    (setq-local agent-repl--history-navigating nil)
    (let ((evil-called nil))
      (cl-letf (((symbol-function 'evil-insert-state)
                 (lambda () (setq evil-called t)))
                ((symbol-function 'agent-repl--history-save) #'ignore))
        (agent-repl-discard-input)
        (should (equal (buffer-string) ""))
        (should evil-called)))))

;;; skip-metaprompt-p: leading whitespace

(ert-deftest agent-repl-test-skip-metaprompt-leading-whitespace ()
  "`agent-repl--skip-metaprompt-p' does NOT strip leading whitespace, so \" /clear\" is not exempt."
  ;; string-trim-right only trims trailing whitespace
  (should-not (agent-repl--skip-metaprompt-p "  /clear"))
  (should-not (agent-repl--skip-metaprompt-p " 42")))

;;; skip-metaprompt-p: empty string

(ert-deftest agent-repl-test-skip-metaprompt-empty-string ()
  "`agent-repl--skip-metaprompt-p' returns nil for empty string."
  (should-not (agent-repl--skip-metaprompt-p "")))

;;; skip-metaprompt-p: mixed numeral+whitespace

(ert-deftest agent-repl-test-skip-metaprompt-mixed-numeral-whitespace ()
  "`agent-repl--skip-metaprompt-p' handles numerals with trailing whitespace but not mixed content."
  ;; Numerals with trailing whitespace -> trimmed to numerals -> should skip
  (should (agent-repl--skip-metaprompt-p "42  "))
  (should (agent-repl--skip-metaprompt-p "7\n"))
  ;; Numeral with leading text -> not bare numeral
  (should-not (agent-repl--skip-metaprompt-p "abc42"))
  ;; Numeral with trailing non-whitespace text
  (should-not (agent-repl--skip-metaprompt-p "42abc")))

;;; should-prepend-metaprompt-p: empty string command-prefix

(ert-deftest agent-repl-test-should-prepend-nil-when-empty-command-prefix ()
  "Empty string `agent-repl-command-prefix' is truthy but conceptually empty."
  ;; In Emacs, "" is truthy, so this will actually return t when conditions align.
  ;; This test documents that behavior.
  (let ((agent-repl-skip-permissions t)
        (agent-repl-command-prefix "")
        (agent-repl--command-prefix "PREFIX: ")
        (agent-repl-prefix-period 1))
    ;; "" is truthy in Emacs, so the function returns t
    (should (agent-repl--should-prepend-metaprompt-p "hello" 0))))

;;; prepare-input: empty raw input

(ert-deftest agent-repl-test-prepare-input-empty-raw ()
  "`agent-repl--prepare-input' with empty raw input: empty string is not exempt."
  (agent-repl-test--with-clean-state
    (let ((agent-repl-skip-permissions t)
          (agent-repl-prefix-period 1)
          (agent-repl-command-prefix "TEST")
          (agent-repl--command-prefix "PREFIX: "))
      (agent-repl--ws-put "ws1" :prefix-counter 0)
      ;; Empty string is not in exempt list and doesn't match numeral regex
      ;; So it gets the prefix prepended (with "\n\n" separator)
      (should (equal (agent-repl--prepare-input "ws1" "")
                     (concat (agent-repl--meta-wrap "PREFIX: ") "\n\n"))))))

;;; run-send-posthooks: multiple hooks matching same input

(ert-deftest agent-repl-test-run-send-posthooks-multiple-matches ()
  "When multiple posthooks match the same input, all should fire."
  (agent-repl-test--with-clean-state
    (let* ((hook-a-called nil)
           (hook-b-called nil)
           (agent-repl-send-posthooks
            (list (cons "^/clear$" (lambda (_ws _raw) (setq hook-a-called t)))
                  (cons "clear"    (lambda (_ws _raw) (setq hook-b-called t))))))
      (agent-repl--run-send-posthooks "ws1" "/clear")
      (should hook-a-called)
      (should hook-b-called))))

;;; run-send-posthooks: empty input

(ert-deftest agent-repl-test-run-send-posthooks-empty-input ()
  "Empty input should not match /clear pattern."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :prefix-counter 42)
    (agent-repl--run-send-posthooks "ws1" "")
    ;; Counter should be unchanged -- no hooks matched
    (should (= (agent-repl--ws-get "ws1" :prefix-counter) 42))))

;;; send: force-metaprompt path

(ert-deftest agent-repl-test-send-force-metaprompt-path ()
  "`agent-repl--send' with force-metaprompt passes force=t to prepare-input."
  (agent-repl-test--with-clean-state
    (let ((prepare-force nil))
      (agent-repl-test--with-temp-buffer " *test-send-force-input*"
        (setq-local agent-repl--input-history nil)
        (setq-local agent-repl--history-index 0)
        (setq-local agent-repl--history-navigating nil)
        (insert "hello")
        (agent-repl--ws-put "ws1" :input-buffer (current-buffer))
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
                  ((symbol-function 'agent-repl--prepare-input)
                   (lambda (_ws raw &optional force)
                     (setq prepare-force force)
                     raw))
                  ((symbol-function 'agent-repl--do-send) #'ignore)
                  ((symbol-function 'agent-repl--history-save) #'ignore))
          (agent-repl--send nil "ws1" t)
          (should prepare-force))))))

;;; send-and-hide: calls send then hide-panels

(ert-deftest agent-repl-test-send-and-hide ()
  "`agent-repl-send-and-hide' calls `agent-repl--send' then `agent-repl--hide-panels'."
  (agent-repl-test--with-clean-state
    ;; A VTERM workspace: `--on-close' dispatches its view teardown through
    ;; the workspace's own frontend, so a workspace that never declares one
    ;; resolves to the gui default and puts a webview away instead.
    (agent-repl--ws-put "test-ws" :frontend 'vterm)
    (let ((calls nil))
      (cl-letf (((symbol-function 'agent-repl--send)
                 (lambda (&rest _) (push 'send calls)))
                ((symbol-function 'agent-repl--hide-panels)
                 (lambda () (push 'hide calls)))
                ;; `agent-repl-send-and-hide' delegates to
                ;; `agent-repl--on-close', which (post-extraction) calls
                ;; `agent-repl-workspace-push-to-back' unconditionally
                ;; rather than under an `fboundp' guard.  Mock it so the
                ;; test doesn't depend on persp-mode being loaded.
                ((symbol-function 'agent-repl-workspace-push-to-back) #'ignore))
        (agent-repl-send-and-hide)
        (should (equal (reverse calls) '(send hide)))))))

;;; send-with-metaprompt: calls send with force=t

(ert-deftest agent-repl-test-send-with-metaprompt ()
  "`agent-repl-send-with-metaprompt' calls `agent-repl--send' with force-metaprompt=t."
  (agent-repl-test--with-clean-state
    (let ((send-args nil))
      (cl-letf (((symbol-function 'agent-repl--send)
                 (lambda (&optional prompt ws force)
                   (setq send-args (list prompt ws force)))))
        (agent-repl-send-with-metaprompt)
        (should (equal send-args '(nil nil t)))))))

;;; send-with-postfix: appends postfix then sends

(ert-deftest agent-repl-test-send-with-postfix ()
  "`agent-repl-send-with-postfix' appends the postfix, then calls send."
  (agent-repl-test--with-clean-state
    (let ((send-called nil)
          (agent-repl-send-postfix " POSTFIX"))
      (agent-repl-test--with-temp-buffer " *test-postfix-input*"
        (insert "hello")
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1")))
          (agent-repl--ws-put "ws1" :input-buffer (current-buffer))
          (cl-letf (((symbol-function 'agent-repl--send)
                     (lambda (&rest _) (setq send-called t))))
            (agent-repl-send-with-postfix)
            ;; Postfix should have been appended
            (should (equal (buffer-string) "hello POSTFIX"))
            ;; Send should have been called
            (should send-called)))))))

;;; send-with-prefix: prepends prefix then sends

(ert-deftest agent-repl-test-send-with-prefix ()
  "`agent-repl-send-with-prefix' prepends the prefix, then calls send."
  (agent-repl-test--with-clean-state
    (let ((send-called nil)
          (agent-repl-send-prefix "PREFIX "))
      (agent-repl-test--with-temp-buffer " *test-prefix-input*"
        (insert "hello")
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1")))
          (agent-repl--ws-put "ws1" :input-buffer (current-buffer))
          (cl-letf (((symbol-function 'agent-repl--send)
                     (lambda (&rest _) (setq send-called t))))
            (agent-repl-send-with-prefix)
            ;; Prefix should have been prepended
            (should (equal (buffer-string) "PREFIX hello"))
            ;; Send should have been called
            (should send-called)))))))

(ert-deftest agent-repl-test-send-prefix-default-value ()
  "`agent-repl-send-prefix' default must be the canonical \"just answer\" string."
  (should (equal agent-repl-send-prefix "just answer, dont take action: ")))

;;; append-to-input-buffer: dead buffer for workspace

(ert-deftest agent-repl-test-append-to-input-buffer-dead-buffer ()
  "`agent-repl--append-to-input-buffer' errors when the input buffer is dead.
The ws-get returns a non-nil dead buffer, passing the `when-let' guard,
but `with-current-buffer' on a dead buffer signals an error."
  (agent-repl-test--with-clean-state
    (let ((buf (get-buffer-create " *test-append-dead*")))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1")))
        (agent-repl--ws-put "ws1" :input-buffer buf)
        (kill-buffer buf)
        ;; Dead buffer passes when-let but with-current-buffer errors
        (should-error (agent-repl--append-to-input-buffer "text"))))))

;;; prepend-to-input-buffer: dead buffer for workspace

(ert-deftest agent-repl-test-prepend-to-input-buffer-dead-buffer ()
  "`agent-repl--prepend-to-input-buffer' errors when the input buffer is dead.
Mirrors the append-to-input-buffer dead-buffer case."
  (agent-repl-test--with-clean-state
    (let ((buf (get-buffer-create " *test-prepend-dead*")))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1")))
        (agent-repl--ws-put "ws1" :input-buffer buf)
        (kill-buffer buf)
        (should-error (agent-repl--prepend-to-input-buffer "text"))))))

;;;; ---- Tests: mark-ws-thinking state overwrite edge cases (status transitions .md) ----

(ert-deftest agent-repl-test-mark-ws-thinking-overwrites-permission ()
  "mark-ws-thinking should overwrite :permission with :thinking."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-set "ws1" :permission)
    (agent-repl--mark-ws-thinking "ws1")
    (should (eq (agent-repl--ws-state "ws1") :thinking))))

(ert-deftest agent-repl-test-mark-ws-thinking-overwrites-done ()
  "mark-ws-thinking should overwrite :done with :thinking."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-set "ws1" :done)
    (agent-repl--mark-ws-thinking "ws1")
    (should (eq (agent-repl--ws-state "ws1") :thinking))))

(ert-deftest agent-repl-test-mark-ws-thinking-overwrites-inactive ()
  "mark-ws-thinking should overwrite :inactive with :thinking."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-set "ws1" :inactive)
    (agent-repl--mark-ws-thinking "ws1")
    (should (eq (agent-repl--ws-state "ws1") :thinking))))

;;;; ---- Tests: metaprompt defcustom defaults ----

(ert-deftest agent-repl-test-skip-permissions-default ()
  "`agent-repl-skip-permissions' should default to t."
  (should (eq (default-value 'agent-repl-skip-permissions) t)))

(ert-deftest agent-repl-test-prefix-period-default ()
  "`agent-repl-prefix-period' should default to 14."
  (should (= (default-value 'agent-repl-prefix-period) 14)))

(ert-deftest agent-repl-test-command-prefix-contains-text ()
  "`agent-repl--command-prefix' should contain the plain read-directive text.
Must NOT use \"metaprompt\" as a conceptual framing — that terminology
was confusing the agent into refusing to read the file, so the inline prefix
is intentionally phrased as a generic \"read this file\" instruction
(the bare filename inside the path is allowed and unavoidable)."
  (should (stringp agent-repl--command-prefix))
  (should (string-match-p "read the file at" agent-repl--command-prefix))
  (should-not (string-match-p "metaprompt directive" agent-repl--command-prefix))
  (should-not (string-match-p "metaprompt file" agent-repl--command-prefix))
  (should-not (string-match-p "the metaprompt" agent-repl--command-prefix)))

;;;; ---- Tests: slash-command completion (capf) ----

(defun agent-repl-test--capf-bounds-in (text point)
  "Return `agent-repl--skill-capf-bounds' for TEXT with point at POINT.
POINT is a 1-indexed buffer position."
  (with-temp-buffer
    (insert text)
    (goto-char point)
    (agent-repl--skill-capf-bounds)))

(ert-deftest agent-repl-test-capf-bounds-bare-command ()
  "The bounds span the `/name' token when it is at buffer start."
  ;; "/deb" with point at end (position 5) → the whole token, positions 1..5.
  (should (equal (agent-repl-test--capf-bounds-in "/deb" 5) '(1 . 5))))

(ert-deftest agent-repl-test-capf-bounds-lone-slash ()
  "A lone `/' is completable: the menu should offer everything."
  (should (equal (agent-repl-test--capf-bounds-in "/" 2) '(1 . 2))))

(ert-deftest agent-repl-test-capf-bounds-rejects-path ()
  "A path is not completed: the name run is stopped by a second slash."
  (should-not (agent-repl-test--capf-bounds-in "/Users/foo" 7)))

(ert-deftest agent-repl-test-capf-bounds-rejects-non-start-slash ()
  "A `/' that is not at buffer start is not completed."
  (should-not (agent-repl-test--capf-bounds-in "hello /deb" 11)))

(ert-deftest agent-repl-test-capf-bounds-rejects-point-past-token ()
  "Once point has moved past the command into its arguments, there is
nothing to complete."
  ;; "/deb x": point at 7 (after the space) is past the token.
  (should-not (agent-repl-test--capf-bounds-in "/deb x" 7)))

(ert-deftest agent-repl-test-capf-bounds-empty-buffer ()
  "An empty buffer has no slash-command token."
  (should-not (agent-repl-test--capf-bounds-in "" 1)))

(ert-deftest agent-repl-test-capf-candidates-carry-the-slash ()
  "Candidates include the leading slash, since the completion region does."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :slash-commands
                        '(((name . "debug-logs") (description . "d") (argumentHint . ""))))
    (should (equal (agent-repl--skill-capf-candidates "ws1") '("/debug-logs")))))

(ert-deftest agent-repl-test-capf-candidates-carry-hint-property ()
  "Each candidate carries its argument hint as a text property."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :slash-commands
                        '(((name . "compact") (description . "d") (argumentHint . "<how>"))))
    (let ((cand (car (agent-repl--skill-capf-candidates "ws1"))))
      (should (equal (get-text-property 0 'agent-repl-skill-hint cand) "<how>")))))

(ert-deftest agent-repl-test-capf-annotation-shows-hint ()
  "The annotation renders the argument hint of a command that takes one."
  (let ((cand (propertize "/compact" 'agent-repl-skill-hint "<how>")))
    (should (equal (agent-repl--skill-capf-annotation cand) " <how>"))))

(ert-deftest agent-repl-test-capf-annotation-nil-for-argless-command ()
  "A command with an empty hint annotates to nil, so no trailing space
is shown."
  (let ((cand (propertize "/debug-logs" 'agent-repl-skill-hint "")))
    (should-not (agent-repl--skill-capf-annotation cand))))

(ert-deftest agent-repl-test-capf-returns-region-and-collection ()
  "The capf returns the token region and the candidate collection when
point sits on a slash command in a workspace."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :slash-commands
                        '(((name . "debug-logs") (description . "d") (argumentHint . ""))))
    (with-temp-buffer
      (setq-local agent-repl--owning-workspace "ws1")
      (insert "/deb")
      (let ((result (agent-repl--skill-capf)))
        (should (equal (nth 0 result) 1))
        (should (equal (nth 1 result) 5))
        (should (equal (nth 2 result) '("/debug-logs")))))))

(ert-deftest agent-repl-test-capf-nil-when-not-on-a-command ()
  "The capf returns nil when point is not on a slash command."
  (agent-repl-test--with-clean-state
    (with-temp-buffer
      (setq-local agent-repl--owning-workspace "ws1")
      (insert "hello world")
      (should-not (agent-repl--skill-capf)))))

;;;; ---- Tests: slash-command menu cache ----

(ert-deftest agent-repl-test-slash-commands-refetch-caches ()
  "`agent-repl--slash-commands-for-ws' fetches once and then serves the
cache."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :frontend-session-id "s1")
    (let ((fetch-count 0))
      (cl-letf (((symbol-function 'agent-repl--frontend-fetch-commands)
                 (lambda (_id)
                   (cl-incf fetch-count)
                   '(((name . "debug-logs") (description . "d") (argumentHint . ""))))))
        ;; Act — two reads.
        (agent-repl--slash-commands-for-ws "ws1")
        (agent-repl--slash-commands-for-ws "ws1")
        ;; Assert — only the first hit the daemon.
        (should (equal fetch-count 1))))))

(ert-deftest agent-repl-test-slash-commands-refetch-tolerates-http-error ()
  "A failed fetch returns nil rather than propagating, so a broken daemon
never interrupts typing."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :frontend-session-id "s1")
    (cl-letf (((symbol-function 'agent-repl--frontend-fetch-commands)
               (lambda (_id) (error "daemon down"))))
      (should-not (agent-repl--slash-commands-for-ws "ws1")))))

(ert-deftest agent-repl-test-slash-commands-refetch-nil-without-session ()
  "With no live session there is nothing to fetch, so the menu is nil and
the daemon is never called."
  (agent-repl-test--with-clean-state
    (let ((called nil))
      (cl-letf (((symbol-function 'agent-repl--frontend-fetch-commands)
                 (lambda (_id) (setq called t) nil)))
        (should-not (agent-repl--slash-commands-for-ws "ws1"))
        (should-not called)))))

(ert-deftest agent-repl-test-slash-commands-invalidate-clears-and-refreshes ()
  "Invalidation drops the cache AND asks the daemon to re-resolve the menu."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :frontend-session-id "s1")
    (agent-repl--ws-put "ws1" :slash-commands
                        '(((name . "stale") (description . "d") (argumentHint . ""))))
    (let ((refreshed-id nil))
      (cl-letf (((symbol-function 'agent-repl--frontend-refresh-commands)
                 (lambda (id) (setq refreshed-id id) t)))
        (agent-repl--slash-commands-invalidate "ws1")
        ;; Assert — cache gone, and the daemon was told to re-probe.
        (should-not (agent-repl--ws-get "ws1" :slash-commands))
        (should (equal refreshed-id "s1"))))))

(ert-deftest agent-repl-test-slash-commands-invalidate-tolerates-refresh-error ()
  "A refresh failure during invalidation is swallowed: the stale menu
simply stands until the next change."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :frontend-session-id "s1")
    (cl-letf (((symbol-function 'agent-repl--frontend-refresh-commands)
               (lambda (_id) (error "daemon down"))))
      ;; Act + Assert — no error escapes.
      (should-not (agent-repl--slash-commands-invalidate "ws1")))))

;;;; ---- Tests: skill-directory watch dirs ----

(ert-deftest agent-repl-test-watch-dirs-includes-existing-user-and-project ()
  "Watch dirs include the user config's skills dir and the project's
.claude/skills, and exclude directories that do not exist."
  (agent-repl-test--with-clean-state
    (let* ((config-root (make-temp-file "capf-config" t))
           (project-root (make-temp-file "capf-project" t))
           (user-skills (expand-file-name "skills" config-root))
           (project-skills (expand-file-name ".claude/skills" project-root)))
      (unwind-protect
          (progn
            (make-directory user-skills t)
            (make-directory project-skills t)
            ;; Note: neither `commands' dir is created, so both must be filtered.
            (agent-repl--ws-put "ws1" :project-dir project-root)
            (let ((process-environment
                   (cons (concat "CLAUDE_CONFIG_DIR=" config-root)
                         process-environment)))
              (let ((dirs (agent-repl--slash-command-watch-dirs "ws1")))
                (should (member user-skills dirs))
                (should (member project-skills dirs))
                ;; The non-existent commands dirs are filtered out.
                (should-not (member (expand-file-name "commands" config-root) dirs)))))
        (delete-directory config-root t)
        (delete-directory project-root t)))))

(ert-deftest agent-repl-test-watch-dirs-no-project ()
  "With no project dir, only the (existing) user config dirs are watched."
  (agent-repl-test--with-clean-state
    (let* ((config-root (make-temp-file "capf-config" t))
           (user-skills (expand-file-name "skills" config-root)))
      (unwind-protect
          (progn
            (make-directory user-skills t)
            (let ((process-environment
                   (cons (concat "CLAUDE_CONFIG_DIR=" config-root)
                         process-environment)))
              (let ((dirs (agent-repl--slash-command-watch-dirs "ws1")))
                (should (equal dirs (list user-skills))))))
        (delete-directory config-root t)))))

(ert-deftest agent-repl-test-ensure-watch-noop-in-batch ()
  "Watcher install is a no-op in batch, so headless runs never leak real
file-notify watchers."
  (agent-repl-test--with-clean-state
    (with-temp-buffer
      ;; `noninteractive' is t under `emacs -batch'.
      (agent-repl--slash-commands-ensure-watch "ws1")
      (should-not agent-repl--slash-command-watchers))))

(provide 'test-input)

;;; test-input.el ends here
