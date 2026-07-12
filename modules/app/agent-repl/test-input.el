;;; test-input.el --- Tests for input.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Dedicated test file for input.el: input mode, send pipeline,
;; metaprompt preparation, slash pass-through, and vterm forwarding.

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

;;;; ---- Tests: Send functions (migrated) ----

(ert-deftest agent-repl-test-send-char-calls-vterm ()
  "`agent-repl-send-char' calls `vterm-send-string' with the char, then the shared Enter primitive.
The trailing Enter must route through
`agent-repl--vterm-send-return-key-logged' so single-char sends share
the same delivery pipeline as every other Enter sender."
  (agent-repl-test--with-clean-state
    (let ((calls nil))
      (agent-repl-test--with-temp-buffer "*agent-panel-abcd1234*"
        (agent-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                  ((symbol-function 'agent-repl--vterm-live-p) (lambda () t))
                  ((symbol-function 'vterm-send-string)
                   (lambda (s &rest _) (push (list 'send-string s) calls)))
                  ((symbol-function 'agent-repl--vterm-send-return-key-logged)
                   (lambda (label) (push (list 'send-return-key label) calls))))
          (agent-repl-send-char "y")
          (should (member '(send-string "y") (reverse calls)))
          (should (member '(send-return-key "send-char") (reverse calls))))))))

(ert-deftest agent-repl-test-scroll-down-sends-down ()
  "`agent-repl-scroll-down' calls `vterm-send-down' in the vterm buffer."
  (agent-repl-test--with-clean-state
    (let ((down-called nil))
      (agent-repl-test--with-temp-buffer "*agent-panel-abcd1234*"
        (agent-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                  ((symbol-function 'agent-repl--vterm-live-p) (lambda () t))
                  ((symbol-function 'vterm-send-down)
                   (lambda () (setq down-called t))))
          (agent-repl-scroll-down)
          (should down-called))))))

(ert-deftest agent-repl-test-scroll-up-sends-up ()
  "`agent-repl-scroll-up' calls `vterm-send-up'."
  (agent-repl-test--with-clean-state
    (let ((up-called nil))
      (agent-repl-test--with-temp-buffer "*agent-panel-abcd1234*"
        (agent-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                  ((symbol-function 'agent-repl--vterm-live-p) (lambda () t))
                  ((symbol-function 'vterm-send-up)
                   (lambda () (setq up-called t))))
          (agent-repl-scroll-up)
          (should up-called))))))

(ert-deftest agent-repl-test-interrupt-sends-escape-twice ()
  "`agent-repl-interrupt' calls `vterm-send-key' with \"<escape>\" twice."
  (agent-repl-test--with-clean-state
    (agent-repl-test--use-vterm-frontend)
    (let ((escape-count 0))
      (agent-repl-test--with-temp-buffer "*agent-panel-abcd1234*"
        (agent-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                  ((symbol-function 'agent-repl--vterm-live-p) (lambda () t))
                  ((symbol-function 'vterm-send-key)
                   (lambda (key &rest _)
                     (when (equal key "<escape>")
                       (cl-incf escape-count)))))
          (agent-repl-interrupt)
          (should (= escape-count 2)))))))

(ert-deftest agent-repl-test-cycle-sends-backtab ()
  "`agent-repl-cycle' calls `vterm-send-key' with \"<backtab>\"."
  (agent-repl-test--with-clean-state
    (let ((backtab-called nil))
      (agent-repl-test--with-temp-buffer "*agent-panel-abcd1234*"
        (agent-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                  ((symbol-function 'agent-repl--vterm-live-p) (lambda () t))
                  ((symbol-function 'vterm-send-key)
                   (lambda (key &rest _)
                     (when (equal key "<backtab>")
                       (setq backtab-called t)))))
          (agent-repl-cycle)
          (should backtab-called))))))

(ert-deftest agent-repl-test-send-input-short-uses-paste ()
  "Short input uses bracketed paste to avoid the vterm--update/process-send-string race."
  (agent-repl-test--with-clean-state
    (let ((send-string-args nil)
          (return-called nil)
          (timer-args nil))
      (agent-repl-test--with-temp-buffer "*agent-panel-abcd1234*"
        (cl-letf (((symbol-function 'vterm-send-string)
                   (lambda (s &rest args) (setq send-string-args (cons s args))))
                  ((symbol-function 'vterm-send-return)
                   (lambda () (setq return-called t)))
                  ((symbol-function 'agent-repl--refresh-vterm) #'ignore)
                  ((symbol-function 'run-at-time)
                   (lambda (&rest args) (setq timer-args args))))
          (agent-repl--send-input-to-vterm (current-buffer) "short input")
          ;; paste flag (2nd arg) should be t
          (should (equal (car send-string-args) "short input"))
          (should (equal (cadr send-string-args) t))
          ;; return should NOT have been called directly (deferred)
          (should-not return-called)
          ;; run-at-time should have been called
          (should timer-args))))))

(ert-deftest agent-repl-test-send-input-paste-mode ()
  "For input >200 chars, calls `vterm-send-string' WITH paste flag, defers return."
  (agent-repl-test--with-clean-state
    (let ((send-string-args nil)
          (return-called nil)
          (timer-args nil))
      (agent-repl-test--with-temp-buffer "*agent-panel-abcd1234*"
        (cl-letf (((symbol-function 'vterm-send-string)
                   (lambda (s &rest args) (setq send-string-args (cons s args))))
                  ((symbol-function 'vterm-send-return)
                   (lambda () (setq return-called t)))
                  ((symbol-function 'agent-repl--refresh-vterm) #'ignore)
                  ((symbol-function 'run-at-time)
                   (lambda (&rest args) (setq timer-args args))))
          (let ((long-input (make-string 201 ?x)))
            (agent-repl--send-input-to-vterm (current-buffer) long-input)
            ;; paste flag (2nd arg) should be t
            (should (equal (cadr send-string-args) t))
            ;; return should NOT have been called directly
            (should-not return-called)
            ;; run-at-time should have been called to defer
            (should timer-args)))))))

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

;;;; ---- Tests: Paste to vterm (migrated) ----

(ert-deftest agent-repl-test-paste-to-vterm ()
  "agent-repl-paste-to-vterm should call vterm-send-key with C-v args."
  (agent-repl-test--with-clean-state
    (let ((send-key-args nil))
      (agent-repl-test--with-temp-buffer "*agent-panel-abcd1234*"
        (agent-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                  ((symbol-function 'agent-repl--vterm-live-p) (lambda () t))
                  ((symbol-function 'vterm-send-key)
                   (lambda (&rest args) (setq send-key-args args))))
          (agent-repl-paste-to-vterm)
          (should (equal send-key-args '("v" nil nil t))))))))

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

(ert-deftest agent-repl-test-command-prefix-tldr-brief-ten-node-hard-limit ()
  "TLDR spec must mandate brevity with a hard cap of 10 total nodes."
  (should (string-match-p
           "MUST be BRIEF: no more than 10 nodes in total"
           agent-repl-command-prefix))
  (should (string-match-p
           "10-node cap is a HARD limit that may NEVER be exceeded for any reason"
           agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-tldr-node-cap-counts-all-nodes ()
  "TLDR spec must state the 10-node cap counts ALL nodes, not just leaf nodes."
  (should (string-match-p
           "counts ALL nodes in the tree (internal and leaf alike), not just leaf nodes"
           agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-tldr-no-soft-node-count-escape ()
  "Regression guard: the prior soft 'only going larger when absolutely necessary' escape hatch must be gone from the node-count rule."
  (should-not (string-match-p
               "only going larger when absolutely necessary"
               agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-tldr-defaults-to-minimal-detail ()
  "TLDR spec must direct the tree to default to minimal detail, covering only critical points."
  (should (string-match-p
           "tree MUST default to minimal detail"
           agent-repl-command-prefix))
  (should (string-match-p
           "covering only the critical points"
           agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-tldr-broad-tree-can-be-expanded ()
  "TLDR spec must state that a broad tree can always be expanded by the user asking for further explanation."
  (should (string-match-p
           "A broad tree can always be expanded by the user asking for further explanation"
           agent-repl-command-prefix)))

(ert-deftest agent-repl-test-command-prefix-tldr-err-toward-omission ()
  "TLDR spec must direct erring toward omission when in doubt whether a detail warrants inclusion."
  (should (string-match-p
           "Err toward omission"
           agent-repl-command-prefix))
  (should (string-match-p
           "when in doubt whether a detail warrants inclusion"
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
  "The metaprompt must require each bullet's text to be as brief as possible."
  (should (string-match-p
           "The text on each bullet MUST be as brief as possible"
           agent-repl-command-prefix))
  (should (string-match-p
           "Brevity of the bullet text is king"
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

(ert-deftest agent-repl-test-command-prefix-tldr-subbullets-only-attachment-mechanism ()
  "TLDR spec must declare recursive subbullets the only permitted way to attach additional/qualifying info to a TLDR bullet, and forbid second sentences within a single bullet."
  (should (string-match-p
           "ONLY permissible way to attach additional or qualifying information"
           agent-repl-command-prefix))
  (should (string-match-p
           "second sentences inside a single bullet are never allowed"
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
  (should-not (agent-repl--skip-metaprompt-p "/clearsomething"))
  (should-not (agent-repl--skip-metaprompt-p "123abc")))

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

;;;; ---- Tests: pin-owning-workspace ----

(ert-deftest agent-repl-test-pin-owning-workspace-sets-local ()
  "Pin should set `agent-repl--owning-workspace' as a buffer-local."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer "*agent-panel-pin-test*"
      (agent-repl--pin-owning-workspace (current-buffer) "my-ws")
      (should (equal agent-repl--owning-workspace "my-ws")))))

(ert-deftest agent-repl-test-pin-owning-workspace-nil-buf ()
  "Pinning with nil buffer should be a no-op (not error)."
  (agent-repl-test--with-clean-state
    (agent-repl--pin-owning-workspace nil "my-ws")))

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

;;;; ---- Tests: send-input-to-vterm always-bracketed-paste ----

(ert-deftest agent-repl-test-send-input-to-vterm-exact-threshold ()
  "Input at exactly the old threshold (200 chars) uses bracketed paste."
  (agent-repl-test--with-clean-state
    (let ((send-string-args nil)
          (return-called nil)
          (timer-args nil))
      (agent-repl-test--with-temp-buffer "*agent-panel-threshold*"
        (cl-letf (((symbol-function 'vterm-send-string)
                   (lambda (s &rest args) (setq send-string-args (cons s args))))
                  ((symbol-function 'vterm-send-return)
                   (lambda () (setq return-called t)))
                  ((symbol-function 'agent-repl--refresh-vterm) #'ignore)
                  ((symbol-function 'run-at-time)
                   (lambda (&rest args) (setq timer-args args))))
          (let ((exact-input (make-string 200 ?x)))
            (agent-repl--send-input-to-vterm (current-buffer) exact-input)
            (should (equal (cadr send-string-args) t))
            (should-not return-called)
            (should timer-args)))))))

(ert-deftest agent-repl-test-send-input-empty-string ()
  "Empty string uses bracketed paste (consistent with all other sends)."
  (agent-repl-test--with-clean-state
    (let ((send-string-args nil)
          (return-called nil)
          (timer-args nil))
      (agent-repl-test--with-temp-buffer "*agent-panel-empty*"
        (cl-letf (((symbol-function 'vterm-send-string)
                   (lambda (s &rest args) (setq send-string-args (cons s args))))
                  ((symbol-function 'vterm-send-return)
                   (lambda () (setq return-called t)))
                  ((symbol-function 'agent-repl--refresh-vterm) #'ignore)
                  ((symbol-function 'run-at-time)
                   (lambda (&rest args) (setq timer-args args))))
          (agent-repl--send-input-to-vterm (current-buffer) "")
          (should (equal (car send-string-args) ""))
          (should (equal (cadr send-string-args) t))
          (should-not return-called)
          (should timer-args))))))

(ert-deftest agent-repl-test-send-input-newline-uses-paste ()
  "Input containing a newline uses bracketed paste."
  (agent-repl-test--with-clean-state
    (let ((send-string-args nil)
          (return-called nil)
          (timer-args nil))
      (agent-repl-test--with-temp-buffer "*agent-panel-newline*"
        (cl-letf (((symbol-function 'vterm-send-string)
                   (lambda (s &rest args) (setq send-string-args (cons s args))))
                  ((symbol-function 'vterm-send-return)
                   (lambda () (setq return-called t)))
                  ((symbol-function 'agent-repl--refresh-vterm) #'ignore)
                  ((symbol-function 'run-at-time)
                   (lambda (&rest args) (setq timer-args args))))
          (agent-repl--send-input-to-vterm (current-buffer) "line1\nline2")
          (should (equal (cadr send-string-args) t))
          (should-not return-called)
          (should timer-args))))))

(ert-deftest agent-repl-test-send-input-trailing-newline-uses-paste ()
  "Input with only a trailing newline uses bracketed paste."
  (agent-repl-test--with-clean-state
    (let ((send-string-args nil)
          (return-called nil)
          (timer-args nil))
      (agent-repl-test--with-temp-buffer "*agent-panel-trailing-nl*"
        (cl-letf (((symbol-function 'vterm-send-string)
                   (lambda (s &rest args) (setq send-string-args (cons s args))))
                  ((symbol-function 'vterm-send-return)
                   (lambda () (setq return-called t)))
                  ((symbol-function 'agent-repl--refresh-vterm) #'ignore)
                  ((symbol-function 'run-at-time)
                   (lambda (&rest args) (setq timer-args args))))
          (agent-repl--send-input-to-vterm (current-buffer) "hello\n")
          (should (equal (cadr send-string-args) t))
          (should-not return-called)
          (should timer-args))))))

(ert-deftest agent-repl-test-send-input-no-newline-short-uses-paste ()
  "Short input without newlines uses bracketed paste (no more direct mode)."
  (agent-repl-test--with-clean-state
    (let ((send-string-args nil)
          (return-called nil)
          (timer-args nil))
      (agent-repl-test--with-temp-buffer "*agent-panel-no-nl*"
        (cl-letf (((symbol-function 'vterm-send-string)
                   (lambda (s &rest args) (setq send-string-args (cons s args))))
                  ((symbol-function 'vterm-send-return)
                   (lambda () (setq return-called t)))
                  ((symbol-function 'agent-repl--refresh-vterm) #'ignore)
                  ((symbol-function 'run-at-time)
                   (lambda (&rest args) (setq timer-args args))))
          (agent-repl--send-input-to-vterm (current-buffer) "no newlines here")
          (should (equal (cadr send-string-args) t))
          (should-not return-called)
          (should timer-args))))))

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


;;;; ---- Tests: do-send frontend backend ----

(ert-deftest agent-repl-test-do-send-frontend-routes-input-to-daemon ()
  "Frontend-backed workspaces send the PREPARED input via the daemon, not vterm.
INPUT may carry the metaprompt prefix — genuine message content that
must survive the transport swap."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :frontend (quote gui))
    (agent-repl--ws-put "ws1" :frontend-session-id "s_1")
    (agent-repl--ws-put "ws1" :prefix-counter 5)
    (let ((sent nil)
          (vterm-called nil))
      (cl-letf (((symbol-function 'agent-repl--frontend-send-user-message)
                 (lambda (ws text) (setq sent (list ws text))))
                ((symbol-function 'agent-repl--send-input-to-vterm)
                 (lambda (&rest _) (setq vterm-called t)))
                ((symbol-function 'agent-repl--run-send-posthooks) #'ignore)
                ((symbol-function 'agent-repl--kickoff-prompt-summary) #'ignore))
        (agent-repl--do-send "ws1" "prepared-input" "raw-text"))
      (should (equal sent '("ws1" "prepared-input")))
      (should-not vterm-called)
      ;; The metaprompt periodicity counter matches the vterm backend.
      (should (= (agent-repl--ws-get "ws1" :prefix-counter) 6)))))

(ert-deftest agent-repl-test-send-frontend-workspace-without-vterm ()
  "RET-level send works in a frontend workspace with NO vterm buffer.
The vterm-gated cond previously swallowed every hybrid-UI send."
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

(ert-deftest agent-repl-test-interrupt-frontend-workspace-without-vterm ()
  "agent-repl-interrupt reaches the wire for frontend workspaces.
The vterm-liveness gate previously skipped the interrupt entirely."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :frontend (quote gui))
    (agent-repl--ws-put "ws1" :frontend-session-id "s_1")
    (let ((interrupted nil))
      (cl-letf (((symbol-function 'agent-repl--gui-interrupt)
                 (lambda (_ws _kind) (setq interrupted t) t))
                ((symbol-function 'run-at-time) #'ignore))
        (agent-repl-interrupt "ws1"))
      (should interrupted)
      ;; Emacs-side state mirrors the vterm branch: turn marked done.
      (should (eq (agent-repl--ws-agent-state "ws1") :done)))))

(ert-deftest agent-repl-test-do-send-frontend-calls-on-settle ()
  "The frontend branch still honors ON-SETTLE (send is synchronous)."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :frontend (quote gui))
    (agent-repl--ws-put "ws1" :frontend-session-id "s_1")
    (let ((settled nil))
      (cl-letf (((symbol-function 'agent-repl--frontend-send-user-message) #'ignore)
                ((symbol-function 'agent-repl--run-send-posthooks) #'ignore)
                ((symbol-function 'agent-repl--kickoff-prompt-summary) #'ignore))
        (agent-repl--do-send "ws1" "input" "raw" (lambda () (setq settled t))))
      (should settled))))

(ert-deftest agent-repl-test-interrupt-agent-frontend-uses-http ()
  "Frontend-backed workspaces interrupt over the daemon route, not Ctrl-C."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :frontend (quote gui))
    (agent-repl--ws-put "ws1" :frontend-session-id "s_1")
    (let ((interrupted nil)
          (ctrl-c nil))
      (cl-letf (((symbol-function 'agent-repl--frontend-interrupt-session)
                 (lambda (id) (setq interrupted id)))
                ((symbol-function 'agent-repl--vterm-send-raw-ctrl-c)
                 (lambda () (setq ctrl-c t))))
        (agent-repl--interrupt-agent "ws1"))
      (should (equal interrupted "s_1"))
      (should-not ctrl-c))))

(ert-deftest agent-repl-test-interrupt-agent-vterm-fallback ()
  "Workspaces without a frontend session keep the raw Ctrl-C path."
  (agent-repl-test--with-clean-state
    (agent-repl-test--use-vterm-frontend)
    (let ((ctrl-c nil))
      (cl-letf (((symbol-function 'agent-repl--vterm-send-raw-ctrl-c)
                 (lambda () (setq ctrl-c t))))
        (agent-repl--interrupt-agent "ws1"))
      (should ctrl-c))))

;;;; ---- Tests: do-send ----

(ert-deftest agent-repl-test-do-send-increments-counter ()
  "`agent-repl--do-send' increments the prefix counter."
  (agent-repl-test--with-clean-state
    (agent-repl-test--use-vterm-frontend)
    (agent-repl-test--with-temp-buffer "*agent-panel-do-send*"
      (agent-repl--ws-put "ws1" :vterm-buffer (current-buffer))
      (agent-repl--ws-put "ws1" :prefix-counter 5)
      (cl-letf (((symbol-function 'agent-repl--send-input-to-vterm) #'ignore)
                ((symbol-function 'agent-repl--run-send-posthooks) #'ignore))
        (agent-repl--do-send "ws1" "input" "raw"))
      (should (= (agent-repl--ws-get "ws1" :prefix-counter) 6)))))

(ert-deftest agent-repl-test-do-send-does-not-touch-agent-state ()
  "`agent-repl--do-send' must not write :agent-state.
The :thinking transition belongs to the prompt_submit Claude Code hook
(via `on-prompt-submit-event').  Emacs-side do-send only sends bytes."
  (agent-repl-test--with-clean-state
    (agent-repl-test--use-vterm-frontend)
    (agent-repl-test--with-temp-buffer "*agent-panel-do-send-think*"
      (agent-repl--ws-put "ws1" :vterm-buffer (current-buffer))
      (cl-letf (((symbol-function 'agent-repl--send-input-to-vterm) #'ignore)
                ((symbol-function 'agent-repl--run-send-posthooks) #'ignore))
        (agent-repl--do-send "ws1" "input" "raw"))
      (should-not (agent-repl--ws-agent-state "ws1"))
      (should-not (agent-repl--ws-get "ws1" :status)))))

(ert-deftest agent-repl-test-do-send-pins-owning-workspace ()
  "`agent-repl--do-send' pins the owning workspace on the vterm buffer."
  (agent-repl-test--with-clean-state
    (agent-repl-test--use-vterm-frontend)
    (agent-repl-test--with-temp-buffer "*agent-panel-do-send-pin*"
      (agent-repl--ws-put "ws1" :vterm-buffer (current-buffer))
      (cl-letf (((symbol-function 'agent-repl--send-input-to-vterm) #'ignore)
                ((symbol-function 'agent-repl--run-send-posthooks) #'ignore))
        (agent-repl--do-send "ws1" "input" "raw"))
      (should (equal agent-repl--owning-workspace "ws1")))))

(ert-deftest agent-repl-test-do-send-runs-posthooks ()
  "`agent-repl--do-send' passes raw input to posthooks."
  (agent-repl-test--with-clean-state
    (agent-repl-test--use-vterm-frontend)
    (let ((posthook-args nil))
      (agent-repl-test--with-temp-buffer "*agent-panel-do-send-hook*"
        (agent-repl--ws-put "ws1" :vterm-buffer (current-buffer))
        (cl-letf (((symbol-function 'agent-repl--send-input-to-vterm) #'ignore)
                  ((symbol-function 'agent-repl--run-send-posthooks)
                   (lambda (ws raw) (setq posthook-args (list ws raw)))))
          (agent-repl--do-send "ws1" "decorated-input" "raw-input"))
        (should (equal posthook-args '("ws1" "raw-input")))))))

(ert-deftest agent-repl-test-do-send-records-last-prompt-time ()
  "`agent-repl--do-send' stamps :last-prompt-time with the current float-time."
  (agent-repl-test--with-clean-state
    (agent-repl-test--use-vterm-frontend)
    (agent-repl-test--with-temp-buffer "*agent-panel-do-send-time*"
      (agent-repl--ws-put "ws1" :vterm-buffer (current-buffer))
      (cl-letf (((symbol-function 'agent-repl--send-input-to-vterm) #'ignore)
                ((symbol-function 'agent-repl--run-send-posthooks) #'ignore)
                ((symbol-function 'float-time) (lambda (&rest _) 1234567.0)))
        (agent-repl--do-send "ws1" "input" "raw"))
      (should (equal (agent-repl--ws-get "ws1" :last-prompt-time)
                     1234567.0)))))

;;;; ---- Tests: discard-or-send-interrupt ----

(ert-deftest agent-repl-test-discard-or-send-interrupt-empty-sends-raw-etx ()
  "When input buffer is empty, send raw ETX (Ctrl-C) byte directly to vterm process.
Uses `process-send-string' rather than `vterm-send-key' because the latter
routes through libvterm's key translation and can dispatch SIGINT instead
of the literal ETX keystroke the agent needs to clear its input line."
  (agent-repl-test--with-clean-state
    (agent-repl-test--use-vterm-frontend)
    (let ((sent-bytes nil))
      (agent-repl-test--with-temp-buffer "*agent-panel-discard-test*"
        (setq-local vterm--process 'fake-proc)
        (agent-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
        (agent-repl-test--with-temp-buffer " *test-input-discard*"
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                    ((symbol-function 'process-send-string)
                     (lambda (proc bytes) (push (cons proc bytes) sent-bytes))))
            (agent-repl-discard-or-send-interrupt)
            (should (equal sent-bytes '((fake-proc . "\C-c"))))))))))

(ert-deftest agent-repl-test-discard-or-send-interrupt-nonempty-discards-and-clears-vterm ()
  "When input buffer has text, BOTH discard the input locally AND clear the agent's prompt.
Previously only cleared the local buffer; this regressed real-world usage
where the user pressed C-c C-c expecting a full reset."
  (agent-repl-test--with-clean-state
    (agent-repl-test--use-vterm-frontend)
    (let ((sent-bytes nil)
          (evil-called nil))
      (agent-repl-test--with-temp-buffer "*agent-panel-discard-vterm*"
        (setq-local vterm--process 'fake-proc)
        (agent-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
        (agent-repl-test--with-temp-buffer " *test-input-nonempty*"
          (setq-local agent-repl--input-history nil)
          (setq-local agent-repl--history-index 0)
          (setq-local agent-repl--history-navigating nil)
          (insert "some text")
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                    ((symbol-function 'evil-insert-state)
                     (lambda () (setq evil-called t)))
                    ((symbol-function 'agent-repl--history-save) #'ignore)
                    ((symbol-function 'process-send-string)
                     (lambda (proc bytes) (push (cons proc bytes) sent-bytes))))
            (agent-repl-discard-or-send-interrupt)
            (should (equal (buffer-string) ""))
            (should evil-called)
            (should (equal sent-bytes '((fake-proc . "\C-c"))))))))))

(ert-deftest agent-repl-test-discard-or-send-interrupt-whitespace-only-clears-buffer ()
  "When input buffer contains only whitespace, C-c C-c still clears it.
Previously `string-blank-p' treated whitespace-only as empty and skipped
`erase-buffer', leaving the user's whitespace stuck in the input."
  (agent-repl-test--with-clean-state
    (agent-repl-test--use-vterm-frontend)
    (let ((sent-bytes nil)
          (evil-called nil))
      (agent-repl-test--with-temp-buffer "*agent-panel-discard-ws*"
        (setq-local vterm--process 'fake-proc)
        (agent-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
        (agent-repl-test--with-temp-buffer " *test-input-whitespace*"
          (setq-local agent-repl--input-history nil)
          (setq-local agent-repl--history-index 0)
          (setq-local agent-repl--history-navigating nil)
          (insert "   \n\t  \n")
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                    ((symbol-function 'evil-insert-state)
                     (lambda () (setq evil-called t)))
                    ((symbol-function 'agent-repl--history-save) #'ignore)
                    ((symbol-function 'process-send-string)
                     (lambda (proc bytes) (push (cons proc bytes) sent-bytes))))
            (agent-repl-discard-or-send-interrupt)
            (should (equal (buffer-string) ""))
            (should evil-called)
            (should (equal sent-bytes '((fake-proc . "\C-c"))))))))))

(ert-deftest agent-repl-test-discard-or-send-interrupt-empty-in-slash-mode-clears-stack ()
  "When in slash mode (empty buffer, stack populated), C-c C-c exits slash mode.
The raw Ctrl-C clears the agent's prompt line; our record of direct sends
must follow so subsequent keystrokes don't continue forwarding and the
next slash-return doesn't see stale accumulated input."
  (agent-repl-test--with-clean-state
    (agent-repl-test--use-vterm-frontend)
    (let ((sent-bytes nil))
      (agent-repl-test--with-temp-buffer "*agent-panel-discard-slash*"
        (setq-local vterm--process 'fake-proc)
        (agent-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
        (agent-repl-test--with-temp-buffer " *test-input-slash-active*"
          (setq-local agent-repl--slash-stack '("r" "a" "e" "l" "c" "/"))
          (agent-repl-slash-input-mode 1)
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                    ((symbol-function 'process-send-string)
                     (lambda (proc bytes) (push (cons proc bytes) sent-bytes))))
            (agent-repl-discard-or-send-interrupt)
            (should (null agent-repl--slash-stack))
            (should-not agent-repl-slash-input-mode)
            (should (equal sent-bytes '((fake-proc . "\C-c"))))))))))

(ert-deftest agent-repl-test-discard-or-send-interrupt-thinking-nonempty-suppresses-ctrl-c ()
  "When the agent is :thinking AND input buffer is non-empty, C-c C-c clears
the local buffer + saves history but DOES NOT send raw Ctrl-C to vterm.
This lets the user draft a message while the agent works and discard the
draft without interrupting the agent's in-flight response."
  (agent-repl-test--with-clean-state
    (let ((sent-bytes nil)
          (evil-called nil))
      (agent-repl-test--with-temp-buffer "*agent-panel-thinking-nonempty*"
        (setq-local vterm--process 'fake-proc)
        (agent-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
        (agent-repl--ws-set-agent-state "test-ws" :thinking)
        (agent-repl-test--with-temp-buffer " *test-input-thinking-nonempty*"
          (setq-local agent-repl--input-history nil)
          (setq-local agent-repl--history-index 0)
          (setq-local agent-repl--history-navigating nil)
          (insert "draft while claude works")
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                    ((symbol-function 'evil-insert-state)
                     (lambda () (setq evil-called t)))
                    ((symbol-function 'agent-repl--history-save) #'ignore)
                    ((symbol-function 'process-send-string)
                     (lambda (proc bytes) (push (cons proc bytes) sent-bytes))))
            (agent-repl-discard-or-send-interrupt)
            (should (equal (buffer-string) ""))
            (should evil-called)
            (should (null sent-bytes))))))))

(ert-deftest agent-repl-test-discard-or-send-interrupt-thinking-empty-still-sends-ctrl-c ()
  "When the agent is :thinking but the input buffer is empty, C-c C-c still
sends raw Ctrl-C to vterm — the suppression only applies when there is
local content to discard."
  (agent-repl-test--with-clean-state
    (agent-repl-test--use-vterm-frontend)
    (let ((sent-bytes nil))
      (agent-repl-test--with-temp-buffer "*agent-panel-thinking-empty*"
        (setq-local vterm--process 'fake-proc)
        (agent-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
        (agent-repl--ws-set-agent-state "test-ws" :thinking)
        (agent-repl-test--with-temp-buffer " *test-input-thinking-empty*"
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                    ((symbol-function 'process-send-string)
                     (lambda (proc bytes) (push (cons proc bytes) sent-bytes))))
            (agent-repl-discard-or-send-interrupt)
            (should (equal sent-bytes '((fake-proc . "\C-c"))))))))))

(ert-deftest agent-repl-test-discard-or-send-interrupt-idle-nonempty-sends-ctrl-c ()
  "When the agent is :idle (not :thinking) AND input buffer is non-empty,
C-c C-c sends raw Ctrl-C AND clears the buffer — the historical full-reset
behavior is preserved outside the :thinking state."
  (agent-repl-test--with-clean-state
    (agent-repl-test--use-vterm-frontend)
    (let ((sent-bytes nil))
      (agent-repl-test--with-temp-buffer "*agent-panel-idle-nonempty*"
        (setq-local vterm--process 'fake-proc)
        (agent-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
        (agent-repl--ws-set-agent-state "test-ws" :idle)
        (agent-repl-test--with-temp-buffer " *test-input-idle-nonempty*"
          (setq-local agent-repl--input-history nil)
          (setq-local agent-repl--history-index 0)
          (setq-local agent-repl--history-navigating nil)
          (insert "some text")
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                    ((symbol-function 'evil-insert-state) #'ignore)
                    ((symbol-function 'agent-repl--history-save) #'ignore)
                    ((symbol-function 'process-send-string)
                     (lambda (proc bytes) (push (cons proc bytes) sent-bytes))))
            (agent-repl-discard-or-send-interrupt)
            (should (equal (buffer-string) ""))
            (should (equal sent-bytes '((fake-proc . "\C-c"))))))))))

;;;; ---- Tests: send-vterm-key ----

(ert-deftest agent-repl-test-send-vterm-key-forwards-key ()
  "`agent-repl--send-vterm-key' forwards the given key to vterm."
  (agent-repl-test--with-clean-state
    (let ((sent-key nil))
      (agent-repl-test--with-temp-buffer "*agent-panel-key-test*"
        (agent-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                  ((symbol-function 'agent-repl--vterm-live-p) (lambda () t))
                  ((symbol-function 'vterm-send-key)
                   (lambda (key &rest _) (setq sent-key key))))
          (agent-repl--send-vterm-key "<up>")
          (should (equal sent-key "<up>")))))))

(ert-deftest agent-repl-test-send-up-arrow ()
  "`agent-repl--send-up-arrow' sends <up> to vterm."
  (agent-repl-test--with-clean-state
    (let ((sent-key nil))
      (agent-repl-test--with-temp-buffer "*agent-panel-up-test*"
        (agent-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                  ((symbol-function 'agent-repl--vterm-live-p) (lambda () t))
                  ((symbol-function 'vterm-send-key)
                   (lambda (key &rest _) (setq sent-key key))))
          (agent-repl--send-up-arrow)
          (should (equal sent-key "<up>")))))))

(ert-deftest agent-repl-test-send-down-arrow ()
  "`agent-repl--send-down-arrow' sends <down> to vterm."
  (agent-repl-test--with-clean-state
    (let ((sent-key nil))
      (agent-repl-test--with-temp-buffer "*agent-panel-down-test*"
        (agent-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                  ((symbol-function 'agent-repl--vterm-live-p) (lambda () t))
                  ((symbol-function 'vterm-send-key)
                   (lambda (key &rest _) (setq sent-key key))))
          (agent-repl--send-down-arrow)
          (should (equal sent-key "<down>")))))))

;;;; ---- Tests: send (integration) ----

(ert-deftest agent-repl-test-send-no-workspace-errors ()
  "`agent-repl--send' should error when no workspace is available."
  (cl-letf (((symbol-function '+workspace-current-name) (lambda () nil)))
    (should-error (agent-repl--send) :type 'error)))

(ert-deftest agent-repl-test-send-reads-from-input-buffer ()
  "`agent-repl--send' reads from the input buffer when no prompt is given."
  (agent-repl-test--with-clean-state
    (agent-repl-test--use-vterm-frontend)
    (let ((sent-input nil))
      (agent-repl-test--with-temp-buffer " *test-send-input*"
        (setq-local agent-repl--input-history nil)
        (setq-local agent-repl--history-index 0)
        (setq-local agent-repl--history-navigating nil)
        (insert "from buffer")
        (agent-repl--ws-put "ws1" :input-buffer (current-buffer))
        (agent-repl-test--with-temp-buffer "*agent-panel-send-vterm*"
          (agent-repl--ws-put "ws1" :vterm-buffer (current-buffer))
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
                    ((symbol-function 'agent-repl--send-input-to-vterm)
                     (lambda (_buf input &optional _on-settle) (setq sent-input input)))
                    ((symbol-function 'agent-repl--history-save) #'ignore))
            (agent-repl--send nil "ws1")
            (should (stringp sent-input))
            ;; The input buffer should be cleared
            (should (equal (with-current-buffer (agent-repl--ws-get "ws1" :input-buffer)
                             (buffer-string))
                           ""))))))))

(ert-deftest agent-repl-test-send-with-explicit-prompt ()
  "`agent-repl--send' uses the given prompt and does not clear input buffer."
  (agent-repl-test--with-clean-state
    (agent-repl-test--use-vterm-frontend)
    (let ((sent-input nil))
      (agent-repl-test--with-temp-buffer " *test-send-prompt-input*"
        (setq-local agent-repl--input-history nil)
        (setq-local agent-repl--history-index 0)
        (setq-local agent-repl--history-navigating nil)
        (insert "original content")
        (agent-repl--ws-put "ws1" :input-buffer (current-buffer))
        (agent-repl-test--with-temp-buffer "*agent-panel-send-prompt-vterm*"
          (agent-repl--ws-put "ws1" :vterm-buffer (current-buffer))
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
                    ((symbol-function 'agent-repl--send-input-to-vterm)
                     (lambda (_buf input &optional _on-settle) (setq sent-input input)))
                    ((symbol-function 'agent-repl--history-save) #'ignore))
            (agent-repl--send "explicit prompt" "ws1")
            (should (stringp sent-input))
            ;; Input buffer should NOT be cleared when prompt is given
            (should (equal (with-current-buffer (agent-repl--ws-get "ws1" :input-buffer)
                             (buffer-string))
                           "original content"))))))))

(ert-deftest agent-repl-test-send-skips-do-send-when-nil-raw ()
  "`agent-repl--send' skips the full-send pipeline when both prompt and input buffer are nil/empty.
Regression guard: empty input must not dispatch a metaprompt-only send via
`agent-repl--do-send'.  Bare-RET forwarding is covered by a separate test."
  (agent-repl-test--with-clean-state
    (let ((do-send-called nil))
      (agent-repl-test--with-temp-buffer "*agent-panel-noop-vterm*"
        (agent-repl--ws-put "ws1" :vterm-buffer (current-buffer))
        ;; No input buffer registered
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
                  ((symbol-function 'agent-repl--do-send)
                   (lambda (&rest _) (setq do-send-called t)))
                  ((symbol-function 'agent-repl--vterm-send-return-key-logged) #'ignore))
          (agent-repl--send nil "ws1")
          (should-not do-send-called))))))

(ert-deftest agent-repl-test-send-skips-do-send-when-input-buffer-empty ()
  "`agent-repl--send' skips the full-send pipeline when the input buffer is empty.
Regression: RET in an empty input buffer used to dispatch a metaprompt-only
send whenever the prefix counter aligned with the period."
  (agent-repl-test--with-clean-state
    (let ((do-send-called nil))
      (agent-repl-test--with-temp-buffer " *test-send-empty-input*"
        ;; Input buffer registered, but contains "".
        (agent-repl--ws-put "ws1" :input-buffer (current-buffer))
        (agent-repl-test--with-temp-buffer "*agent-panel-empty-vterm*"
          (agent-repl--ws-put "ws1" :vterm-buffer (current-buffer))
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
                    ((symbol-function 'agent-repl--do-send)
                     (lambda (&rest _) (setq do-send-called t)))
                    ((symbol-function 'agent-repl--vterm-send-return-key-logged) #'ignore))
            (agent-repl--send nil "ws1")
            (should-not do-send-called)))))))

(ert-deftest agent-repl-test-send-skips-do-send-when-input-buffer-whitespace-only ()
  "`agent-repl--send' skips the full-send pipeline when the input buffer holds only whitespace."
  (agent-repl-test--with-clean-state
    (let ((do-send-called nil))
      (agent-repl-test--with-temp-buffer " *test-send-whitespace-input*"
        (insert "  \n\t  \n")
        (agent-repl--ws-put "ws1" :input-buffer (current-buffer))
        (agent-repl-test--with-temp-buffer "*agent-panel-whitespace-vterm*"
          (agent-repl--ws-put "ws1" :vterm-buffer (current-buffer))
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
                    ((symbol-function 'agent-repl--do-send)
                     (lambda (&rest _) (setq do-send-called t)))
                    ((symbol-function 'agent-repl--vterm-send-return-key-logged) #'ignore))
            (agent-repl--send nil "ws1")
            (should-not do-send-called)))))))

(ert-deftest agent-repl-test-send-skips-do-send-when-explicit-prompt-empty ()
  "`agent-repl--send' skips the full-send pipeline when an empty PROMPT is passed explicitly."
  (agent-repl-test--with-clean-state
    (let ((do-send-called nil))
      (agent-repl-test--with-temp-buffer "*agent-panel-empty-prompt-vterm*"
        (agent-repl--ws-put "ws1" :vterm-buffer (current-buffer))
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
                  ((symbol-function 'agent-repl--do-send)
                   (lambda (&rest _) (setq do-send-called t)))
                  ((symbol-function 'agent-repl--vterm-send-return-key-logged) #'ignore))
          (agent-repl--send "" "ws1")
          (should-not do-send-called)
          (agent-repl--send "   \n  " "ws1")
          (should-not do-send-called))))))

(ert-deftest agent-repl-test-send-forwards-bare-ret-when-input-buffer-empty ()
  "`agent-repl--send' forwards a bare RET to vterm when the input buffer is empty.
RET on an empty input should still reach the agent — useful for navigating
permission prompts, menus, and confirmations."
  (agent-repl-test--with-clean-state
    (agent-repl-test--use-vterm-frontend)
    (let ((ret-buf nil))
      (agent-repl-test--with-temp-buffer " *test-send-bare-ret-empty-input*"
        (agent-repl--ws-put "ws1" :input-buffer (current-buffer))
        (agent-repl-test--with-temp-buffer "*agent-panel-bare-ret-empty-vterm*"
          (agent-repl--ws-put "ws1" :vterm-buffer (current-buffer))
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
                    ((symbol-function 'agent-repl--vterm-send-return-key-logged)
                     (lambda (&rest _) (setq ret-buf (current-buffer)))))
            (agent-repl--send nil "ws1")
            (should (eq ret-buf (agent-repl--ws-get "ws1" :vterm-buffer)))))))))

(ert-deftest agent-repl-test-send-forwards-bare-ret-when-input-buffer-whitespace-only ()
  "`agent-repl--send' forwards a bare RET to vterm when the input buffer holds only whitespace."
  (agent-repl-test--with-clean-state
    (agent-repl-test--use-vterm-frontend)
    (let ((ret-buf nil))
      (agent-repl-test--with-temp-buffer " *test-send-bare-ret-whitespace-input*"
        (insert "  \n\t  \n")
        (agent-repl--ws-put "ws1" :input-buffer (current-buffer))
        (agent-repl-test--with-temp-buffer "*agent-panel-bare-ret-whitespace-vterm*"
          (agent-repl--ws-put "ws1" :vterm-buffer (current-buffer))
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
                    ((symbol-function 'agent-repl--vterm-send-return-key-logged)
                     (lambda (&rest _) (setq ret-buf (current-buffer)))))
            (agent-repl--send nil "ws1")
            (should (eq ret-buf (agent-repl--ws-get "ws1" :vterm-buffer)))))))))

(ert-deftest agent-repl-test-send-forwards-bare-ret-when-explicit-prompt-empty ()
  "`agent-repl--send' forwards a bare RET to vterm when an empty PROMPT is passed explicitly."
  (agent-repl-test--with-clean-state
    (agent-repl-test--use-vterm-frontend)
    (let ((ret-call-count 0))
      (agent-repl-test--with-temp-buffer "*agent-panel-bare-ret-empty-prompt-vterm*"
        (agent-repl--ws-put "ws1" :vterm-buffer (current-buffer))
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
                  ((symbol-function 'agent-repl--vterm-send-return-key-logged)
                   (lambda (&rest _) (cl-incf ret-call-count))))
          (agent-repl--send "" "ws1")
          (should (= ret-call-count 1))
          (agent-repl--send "   \n  " "ws1")
          (should (= ret-call-count 2)))))))

(ert-deftest agent-repl-test-send-forwards-bare-ret-when-nil-raw-no-input-buffer ()
  "`agent-repl--send' forwards a bare RET to vterm when no input buffer is registered.
Prompt is nil and no input buffer means raw is nil and the empty-input branch
should still forward RET so the keystroke reaches the agent."
  (agent-repl-test--with-clean-state
    (agent-repl-test--use-vterm-frontend)
    (let ((ret-buf nil))
      (agent-repl-test--with-temp-buffer "*agent-panel-bare-ret-nil-raw-vterm*"
        (agent-repl--ws-put "ws1" :vterm-buffer (current-buffer))
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
                  ((symbol-function 'agent-repl--vterm-send-return-key-logged)
                   (lambda (&rest _) (setq ret-buf (current-buffer)))))
          (agent-repl--send nil "ws1")
          (should (eq ret-buf (agent-repl--ws-get "ws1" :vterm-buffer))))))))

(ert-deftest agent-repl-test-send-bare-ret-transitions-permission-to-thinking ()
  "`agent-repl--send' transitions :permission -> :thinking on a bare-RET send.
Answering a permission prompt by pressing RET on an empty input buffer is the
only signal that the agent is now working on the permitted action.  The flip
lives inside `agent-repl--vterm-send-return-key-logged' (the lowest-level
return primitive), so the real primitive must run — `vterm--term' is set
buffer-locally so the delivered branch is taken."
  (agent-repl-test--with-clean-state
    (agent-repl-test--use-vterm-frontend)
    (agent-repl-test--with-temp-buffer " *test-send-bare-ret-perm-input*"
      (agent-repl--ws-put "ws1" :input-buffer (current-buffer))
      (agent-repl-test--with-temp-buffer "*agent-panel-bare-ret-perm-vterm*"
        (agent-repl--ws-put "ws1" :vterm-buffer (current-buffer))
        (agent-repl--ws-set-agent-state "ws1" :permission)
        (setq-local vterm--term t)
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
                  ((symbol-function 'vterm-send-key) #'ignore))
          (agent-repl--send nil "ws1")
          (should (eq (agent-repl--ws-state "ws1") :thinking)))))))

(ert-deftest agent-repl-test-send-bare-ret-leaves-non-permission-state-unchanged ()
  "`agent-repl--send' bare-RET does NOT force :thinking from a non-:permission state.
Only the :permission -> :thinking transition is owned by the Emacs-side keypress;
other states (e.g. :idle) must be left untouched on a bare RET."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer " *test-send-bare-ret-idle-input*"
      (agent-repl--ws-put "ws1" :input-buffer (current-buffer))
      (agent-repl-test--with-temp-buffer "*agent-panel-bare-ret-idle-vterm*"
        (agent-repl--ws-put "ws1" :vterm-buffer (current-buffer))
        (agent-repl--ws-set-agent-state "ws1" :idle)
        (setq-local vterm--term t)
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
                  ((symbol-function 'vterm-send-key) #'ignore))
          (agent-repl--send nil "ws1")
          (should (eq (agent-repl--ws-state "ws1") :idle)))))))

(ert-deftest agent-repl-test-send-skips-bare-ret-when-no-vterm-buffer ()
  "`agent-repl--send' does NOT forward RET when no vterm buffer is registered.
There's no terminal to receive the keystroke."
  (agent-repl-test--with-clean-state
    (let ((ret-called nil))
      (agent-repl-test--with-temp-buffer " *test-send-bare-ret-no-vterm-input*"
        (agent-repl--ws-put "ws1" :input-buffer (current-buffer))
        ;; Note: no vterm-buffer registered.
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
                  ((symbol-function 'agent-repl--vterm-send-return-key-logged)
                   (lambda (&rest _) (setq ret-called t))))
          (agent-repl--send nil "ws1")
          (should-not ret-called))))))

(ert-deftest agent-repl-test-send-skips-bare-ret-when-vterm-buffer-dead ()
  "`agent-repl--send' does NOT forward RET when the vterm buffer is dead."
  (agent-repl-test--with-clean-state
    (let ((ret-called nil)
          (dead-vterm (generate-new-buffer "*agent-panel-bare-ret-dead-vterm*")))
      (agent-repl--ws-put "ws1" :vterm-buffer dead-vterm)
      (kill-buffer dead-vterm)
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
                ((symbol-function 'agent-repl--vterm-send-return-key-logged)
                 (lambda (&rest _) (setq ret-called t))))
        (agent-repl--send nil "ws1")
        (should-not ret-called)))))

;;;; ---- Tests: bracketed paste pipeline ----

;;;; ---- Tests: vterm-send-return-logged ----

(ert-deftest agent-repl-test-vterm-send-return-logged-delivers ()
  "`agent-repl--vterm-send-return-logged' sends return when vterm--term is alive."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer "*test-return-logged*"
      (setq-local vterm--term 'fake-term)
      (let ((return-called nil))
        (cl-letf (((symbol-function 'vterm-send-return)
                   (lambda () (setq return-called t))))
          (agent-repl--vterm-send-return-logged "test-label")
          (should return-called))))))

(ert-deftest agent-repl-test-vterm-send-return-logged-nil-term ()
  "`agent-repl--vterm-send-return-logged' does NOT send when vterm--term is nil."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer "*test-return-logged-nil*"
      (setq-local vterm--term nil)
      (let ((return-called nil))
        (cl-letf (((symbol-function 'vterm-send-return)
                   (lambda () (setq return-called t))))
          (agent-repl--vterm-send-return-logged "test-label")
          (should-not return-called))))))

;;;; ---- Tests: vterm-send-return-key-logged ----

(ert-deftest agent-repl-test-vterm-send-return-key-logged-routes-via-libvterm ()
  "`agent-repl--vterm-send-return-key-logged' sends `\\C-m' via `vterm-send-key'.
Regression guard: the Enter keystroke must go through libvterm's
keyboard handler (vterm--update) on the SAME path as the arrow-key
forwards, and the key argument must be the raw CR character."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer "*test-return-key-logged*"
      (setq-local vterm--term 'fake-term)
      (let ((key-arg nil))
        (cl-letf (((symbol-function 'vterm-send-key)
                   (lambda (key &rest _) (setq key-arg key))))
          (agent-repl--vterm-send-return-key-logged "test-label")
          (should (equal key-arg "\C-m")))))))

(ert-deftest agent-repl-test-vterm-send-return-key-logged-never-sends-return-key-name ()
  "`agent-repl--vterm-send-return-key-logged' must NOT pass \"<return>\" to `vterm-send-key'.
vterm-module.c's `term_process_key' does not recognize \"<return>\" as
a key name, and unrecognized names longer than 4 bytes are silently
dropped by its UTF-8 fallthrough guard — so `vterm-send-key
\"<return>\"' is a no-op that looks like a successful send.  This is
the exact bug that made RET on an empty input buffer do nothing while
arrow keys (recognized names) worked."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer "*test-return-key-logged-name*"
      (setq-local vterm--term 'fake-term)
      (let ((key-arg nil))
        (cl-letf (((symbol-function 'vterm-send-key)
                   (lambda (key &rest _) (setq key-arg key))))
          (agent-repl--vterm-send-return-key-logged "test-label")
          (should-not (equal key-arg "<return>")))))))

(ert-deftest agent-repl-test-vterm-send-return-key-logged-nil-term ()
  "`agent-repl--vterm-send-return-key-logged' does NOT send when vterm--term is nil.
Mirrors `agent-repl-test-vterm-send-return-logged-nil-term' for the
libvterm-routed variant — a missing `vterm--term' must be a logged
warning, not a silent vterm-send-key call."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer "*test-return-key-logged-nil*"
      (setq-local vterm--term nil)
      (let ((key-called nil))
        (cl-letf (((symbol-function 'vterm-send-key)
                   (lambda (&rest _) (setq key-called t))))
          (agent-repl--vterm-send-return-key-logged "test-label")
          (should-not key-called))))))

(ert-deftest agent-repl-test-vterm-send-return-key-logged-does-not-call-vterm-send-return ()
  "`agent-repl--vterm-send-return-key-logged' never falls back to `vterm-send-return'.
The whole point of the libvterm-routed variant is to avoid the
raw process-send-string path, so this guards against the helper
accidentally being wired back to the byte-write path."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer "*test-return-key-logged-no-fallback*"
      (setq-local vterm--term 'fake-term)
      (let ((return-called nil))
        (cl-letf (((symbol-function 'vterm-send-key) #'ignore)
                  ((symbol-function 'vterm-send-return)
                   (lambda () (setq return-called t))))
          (agent-repl--vterm-send-return-key-logged "test-label")
          (should-not return-called))))))

(ert-deftest agent-repl-test-send-input-direct-calls-send-return ()
  "`agent-repl--send-input-direct' sends string then return then refreshes."
  (agent-repl-test--with-clean-state
    (let ((calls nil))
      (agent-repl-test--with-temp-buffer "*agent-panel-direct-test*"
        (cl-letf (((symbol-function 'vterm-send-string)
                   (lambda (s &rest _) (push (list 'string s) calls)))
                  ((symbol-function 'vterm-send-return)
                   (lambda () (push '(return) calls)))
                  ((symbol-function 'agent-repl--refresh-vterm)
                   (lambda () (push '(refresh) calls))))
          (agent-repl--send-input-direct (current-buffer) "hello")
          ;; Verify order: string, return, refresh
          (should (equal (reverse calls) '((string "hello") (return) (refresh)))))))))

(ert-deftest agent-repl-test-send-input-bracketed-uses-paste-flag ()
  "`agent-repl--send-input-bracketed' calls vterm-send-string with t paste flag."
  (agent-repl-test--with-clean-state
    (let ((send-args nil))
      (agent-repl-test--with-temp-buffer "*agent-panel-bracketed-test*"
        (cl-letf (((symbol-function 'vterm-send-string)
                   (lambda (s &rest args) (setq send-args (cons s args))))
                  ((symbol-function 'run-at-time)
                   (lambda (&rest _) nil)))
          (agent-repl--send-input-bracketed (current-buffer) "big input")
          (should (equal (car send-args) "big input"))
          (should (equal (cadr send-args) t)))))))

(ert-deftest agent-repl-test-send-input-direct-calls-on-settle ()
  "`agent-repl--send-input-direct' calls on-settle callback immediately."
  (agent-repl-test--with-clean-state
    (let ((settled nil))
      (agent-repl-test--with-temp-buffer "*agent-panel-direct-settle*"
        (cl-letf (((symbol-function 'vterm-send-string) #'ignore)
                  ((symbol-function 'vterm-send-return) #'ignore)
                  ((symbol-function 'agent-repl--refresh-vterm) #'ignore))
          (agent-repl--send-input-direct (current-buffer) "x"
                                          (lambda () (setq settled t)))
          (should settled))))))

(ert-deftest agent-repl-test-send-input-direct-nil-on-settle ()
  "`agent-repl--send-input-direct' works fine without on-settle."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer "*agent-panel-direct-nosettle*"
      (cl-letf (((symbol-function 'vterm-send-string) #'ignore)
                ((symbol-function 'vterm-send-return) #'ignore)
                ((symbol-function 'agent-repl--refresh-vterm) #'ignore))
        (agent-repl--send-input-direct (current-buffer) "x")))))

(ert-deftest agent-repl-test-send-input-bracketed-forwards-on-settle ()
  "`agent-repl--send-input-bracketed' threads on-settle to deferred pipeline."
  (agent-repl-test--with-clean-state
    (let ((timer-action nil))
      (agent-repl-test--with-temp-buffer "*agent-panel-bracketed-settle*"
        (cl-letf (((symbol-function 'vterm-send-string) #'ignore)
                  ((symbol-function 'run-at-time)
                   (lambda (_delay _repeat fn &rest _args)
                     (setq timer-action fn))))
          ;; Send with an on-settle callback
          (agent-repl--send-input-bracketed (current-buffer) "x"
                                             (lambda () 'settled))
          ;; A timer was scheduled (the paste-delay action)
          (should timer-action)
          ;; The action should be a lambda (wrapping bracketed-send-return
          ;; with the on-settle callback), not the bare partial application
          (should (functionp timer-action)))))))

(ert-deftest agent-repl-test-bracketed-finalize-calls-on-settle ()
  "`agent-repl--bracketed-finalize' calls on-settle after refresh."
  (let ((settled nil))
    (cl-letf (((symbol-function 'vterm-send-return) #'ignore)
              ((symbol-function 'agent-repl--refresh-vterm) #'ignore))
      (agent-repl--bracketed-finalize (lambda () (setq settled t)))
      (should settled))))

(ert-deftest agent-repl-test-bracketed-finalize-nil-on-settle ()
  "`agent-repl--bracketed-finalize' works fine without on-settle."
  (cl-letf (((symbol-function 'vterm-send-return) #'ignore)
            ((symbol-function 'agent-repl--refresh-vterm) #'ignore))
    (agent-repl--bracketed-finalize)))

;;;; ---- Tests: slash mode ----

(ert-deftest agent-repl-test-slash-try-send-and-push-success ()
  "On successful vterm forward, pushes onto stack and returns t."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer " *test-slash-push*"
      (setq-local agent-repl--slash-stack nil)
      (let ((sent nil))
        (agent-repl-test--with-temp-buffer "*agent-panel-slash-vterm*"
          (agent-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                    ((symbol-function 'agent-repl--vterm-live-p) (lambda () t))
                    ((symbol-function 'vterm-send-string)
                     (lambda (s &rest _) (push s sent))))
            (with-current-buffer " *test-slash-push*"
              (should (agent-repl--slash-try-send-and-push "a"))
              (should (equal agent-repl--slash-stack '("a")))
              (should (agent-repl--slash-try-send-and-push "b"))
              (should (equal agent-repl--slash-stack '("b" "a")))
              (should (equal (reverse sent) '("a" "b"))))))))))

(ert-deftest agent-repl-test-slash-try-send-and-push-no-vterm-refuses-push ()
  "When no live vterm: returns nil, does NOT push onto stack, surfaces error.
Regression for the stuck-stack bug: if vterm isn't live, the local stack
must not accumulate phantom entries that trap the user in slash mode."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer " *test-slash-push-fail*"
      (setq-local agent-repl--slash-stack nil)
      (let ((msg-called nil))
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                  ;; No :vterm-buffer set → --current-ws-live-vterm returns nil.
                  ((symbol-function 'message) (lambda (&rest _) (setq msg-called t)))
                  ((symbol-function 'vterm-send-string)
                   (lambda (&rest _) (error "MUST NOT be called when vterm missing"))))
          (with-current-buffer " *test-slash-push-fail*"
            (should-not (agent-repl--slash-try-send-and-push "a"))
            (should (null agent-repl--slash-stack))
            (should msg-called)))))))

(ert-deftest agent-repl-test-slash-backspace-pops-stack ()
  "Slash backspace should pop from stack and send backspace to vterm."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer " *test-slash-bs*"
      (setq-local agent-repl--slash-stack '("b" "a" "/"))
      (let ((backspace-count 0))
        (agent-repl-test--with-temp-buffer "*agent-panel-slash-bs-vterm*"
          (agent-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                    ((symbol-function 'agent-repl--vterm-live-p) (lambda () t))
                    ((symbol-function 'vterm-send-key)
                     (lambda (key &rest _)
                       (when (equal key "<backspace>")
                         (cl-incf backspace-count)))))
            (with-current-buffer " *test-slash-bs*"
              (agent-repl--slash-backspace)
              (should (equal agent-repl--slash-stack '("a" "/")))
              (should (= backspace-count 1)))))))))

(ert-deftest agent-repl-test-slash-backspace-exits-mode-when-empty ()
  "Slash backspace should exit slash mode when stack becomes empty."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer " *test-slash-exit*"
      (setq-local agent-repl--slash-stack '("/"))
      (agent-repl-slash-input-mode 1)
      (agent-repl-test--with-temp-buffer "*agent-panel-slash-exit-vterm*"
        (agent-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                  ((symbol-function 'agent-repl--vterm-live-p) (lambda () t))
                  ((symbol-function 'vterm-send-key) #'ignore))
          (with-current-buffer " *test-slash-exit*"
            (agent-repl--slash-backspace)
            (should (null agent-repl--slash-stack))
            (should-not agent-repl-slash-input-mode)))))))

(ert-deftest agent-repl-test-slash-return-exits-mode ()
  "Slash return should send return to vterm and exit slash mode."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer " *test-slash-return*"
      (setq-local agent-repl--slash-stack '("r" "a" "e" "l" "c" "/"))
      (agent-repl-slash-input-mode 1)
      (let ((return-called nil))
        (agent-repl-test--with-temp-buffer "*agent-panel-slash-return-vterm*"
          (agent-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                    ((symbol-function 'agent-repl--vterm-live-p) (lambda () t))
                    ((symbol-function 'agent-repl--vterm-send-return-key-logged)
                     (lambda (_label) (setq return-called t))))
            (with-current-buffer " *test-slash-return*"
              (agent-repl--slash-return)
              (should return-called)
              (should (null agent-repl--slash-stack))
              (should-not agent-repl-slash-input-mode))))))))

(ert-deftest agent-repl-test-slash-return-routes-submission-via-libvterm ()
  "Slash-return's no-pasted submission never uses raw `vterm-send-return'.
The raw `process-send-string' byte write is not reliably registered as
an Enter keystroke by Claude's Ink TUI, so slash commands typed
char-by-char intermittently failed to submit on RET.  The submission
must route through `agent-repl--vterm-send-return-key-logged'
\(libvterm keyboard path) — the same fix as the empty-buffer bare-RET
branch and the bracketed-paste submission Return."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer " *test-slash-return-libvterm*"
      (setq-local agent-repl--slash-stack '("c" "/"))
      (agent-repl-slash-input-mode 1)
      (let ((key-logged-label nil)
            (raw-return-called nil))
        (agent-repl-test--with-temp-buffer "*agent-panel-slash-libvterm-vterm*"
          (agent-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                    ((symbol-function 'agent-repl--vterm-live-p) (lambda () t))
                    ((symbol-function 'agent-repl--vterm-send-return-key-logged)
                     (lambda (label) (setq key-logged-label label)))
                    ((symbol-function 'vterm-send-return)
                     (lambda () (setq raw-return-called t))))
            (with-current-buffer " *test-slash-return-libvterm*"
              (agent-repl--slash-return)
              (should (equal key-logged-label "slash-return"))
              (should-not raw-return-called))))))))

(ert-deftest agent-repl-test-slash-return-runs-posthooks-on-accumulated-input ()
  "`slash-return' runs posthooks against the reconstructed slash command.
Stack is in reverse order (most recent push first); the runner sees the
concatenated forward-order string, so `/clear' typed via direct send
fires the same posthooks as a buffered-and-sent `/clear'."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer " *test-slash-return-posthooks*"
      ;; Stack stores reversed pushes: typed "/clear" → ("r" "a" "e" "l" "c" "/")
      (setq-local agent-repl--slash-stack '("r" "a" "e" "l" "c" "/"))
      (agent-repl-slash-input-mode 1)
      (let ((posthook-args nil))
        (agent-repl-test--with-temp-buffer "*agent-panel-slash-posthook-vterm*"
          (agent-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                    ((symbol-function 'agent-repl--vterm-send-return-key-logged) #'ignore)
                    ((symbol-function 'agent-repl--run-send-posthooks)
                     (lambda (ws raw) (setq posthook-args (list ws raw)))))
            (with-current-buffer " *test-slash-return-posthooks*"
              (agent-repl--slash-return)
              (should (equal posthook-args '("test-ws" "/clear"))))))))))

(ert-deftest agent-repl-test-slash-return-clear-marks-done ()
  "Direct send `/clear' via slash-return marks agent-state :done.
Covers the end-to-end path: slash-stack accumulated, RET in slash mode
fires posthooks, the /clear posthook marks :done."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer " *test-slash-return-clear-done*"
      (setq-local agent-repl--slash-stack '("r" "a" "e" "l" "c" "/"))
      (agent-repl-slash-input-mode 1)
      (agent-repl-test--with-temp-buffer "*agent-panel-slash-clear-done-vterm*"
        (agent-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
        (agent-repl--ws-set-agent-state "test-ws" :idle)
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                  ((symbol-function 'agent-repl--vterm-send-return-key-logged) #'ignore))
          (with-current-buffer " *test-slash-return-clear-done*"
            (agent-repl--slash-return)
            (should (eq (agent-repl--ws-get "test-ws" :agent-state) :done))))))))

(ert-deftest agent-repl-test-slash-return-backspaced-input-runs-posthooks-on-remaining ()
  "After backspaces, slash-return's posthooks see only the remaining stack.
Reconstructed command should reflect post-backspace state, so `/cle' typed
then truncated to `/c' fires no /clear hook."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer " *test-slash-bs-posthooks*"
      ;; Stack reflects user typing "/c" (after backspaces) — top is "c", bottom is "/"
      (setq-local agent-repl--slash-stack '("c" "/"))
      (agent-repl-slash-input-mode 1)
      (let ((posthook-args nil))
        (agent-repl-test--with-temp-buffer "*agent-panel-slash-bs-vterm*"
          (agent-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                    ((symbol-function 'agent-repl--vterm-send-return-key-logged) #'ignore)
                    ((symbol-function 'agent-repl--run-send-posthooks)
                     (lambda (ws raw) (setq posthook-args (list ws raw)))))
            (with-current-buffer " *test-slash-bs-posthooks*"
              (agent-repl--slash-return)
              (should (equal posthook-args '("test-ws" "/c"))))))))))

(ert-deftest agent-repl-test-slash-return-sends-pasted-input-buffer ()
  "When input buffer has pasted text, slash-return sends it via bracketed paste.
Pasted text bypasses slash-mode's self-insert-command remap and lands in the
input buffer; slash-return must forward it to vterm so it concatenates with the
already-forwarded direct-insert chars on the agent's prompt line."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer " *test-slash-return-paste*"
      (setq-local agent-repl--slash-stack '("c" "/"))
      (agent-repl-slash-input-mode 1)
      (insert "pasted-content")
      (agent-repl--ws-put "test-ws" :input-buffer (current-buffer))
      (let ((send-args nil)
            (return-called nil))
        (agent-repl-test--with-temp-buffer "*agent-panel-slash-paste-vterm*"
          (agent-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                    ((symbol-function 'agent-repl--vterm-live-p) (lambda () t))
                    ((symbol-function 'agent-repl--send-input-to-vterm)
                     (lambda (buf input &optional _on-settle)
                       (setq send-args (list buf input))))
                    ((symbol-function 'agent-repl--vterm-send-return-key-logged)
                     (lambda (_label) (setq return-called t))))
            (with-current-buffer " *test-slash-return-paste*"
              (agent-repl--slash-return)
              (should send-args)
              (should (equal (nth 1 send-args) "pasted-content"))
              (should-not return-called)
              (should (zerop (buffer-size)))
              (should-not agent-repl-slash-input-mode))))))))

(ert-deftest agent-repl-test-slash-return-empty-buffer-sends-bare-return ()
  "When input buffer is empty, slash-return sends only RET (no bracketed paste).
Preserves the pre-existing behavior for the typical direct-send path
\(empty buffer + slash command typed char-by-char + RET)."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer " *test-slash-return-empty*"
      (setq-local agent-repl--slash-stack '("c" "/"))
      (agent-repl-slash-input-mode 1)
      (agent-repl--ws-put "test-ws" :input-buffer (current-buffer))
      (let ((send-called nil)
            (return-called nil))
        (agent-repl-test--with-temp-buffer "*agent-panel-slash-empty-vterm*"
          (agent-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                    ((symbol-function 'agent-repl--vterm-live-p) (lambda () t))
                    ((symbol-function 'agent-repl--send-input-to-vterm)
                     (lambda (&rest _) (setq send-called t)))
                    ((symbol-function 'agent-repl--vterm-send-return-key-logged)
                     (lambda (_label) (setq return-called t))))
            (with-current-buffer " *test-slash-return-empty*"
              (agent-repl--slash-return)
              (should return-called)
              (should-not send-called))))))))

(ert-deftest agent-repl-test-slash-return-whitespace-only-buffer-sends-pasted ()
  "Whitespace-only buffer is treated as pasted content (non-empty after `string-empty-p').
Whitespace can be meaningful in agent prompts; slash-return must not silently
drop it.  Verifies the empty/non-empty check uses `string-empty-p' on raw
buffer-string, not a trimmed view."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer " *test-slash-return-ws*"
      (setq-local agent-repl--slash-stack '("c" "/"))
      (agent-repl-slash-input-mode 1)
      (insert "   ")
      (agent-repl--ws-put "test-ws" :input-buffer (current-buffer))
      (let ((send-args nil))
        (agent-repl-test--with-temp-buffer "*agent-panel-slash-ws-vterm*"
          (agent-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                    ((symbol-function 'agent-repl--vterm-live-p) (lambda () t))
                    ((symbol-function 'agent-repl--send-input-to-vterm)
                     (lambda (buf input &optional _on-settle)
                       (setq send-args (list buf input))))
                    ((symbol-function 'agent-repl--vterm-send-return-key-logged) #'ignore))
            (with-current-buffer " *test-slash-return-ws*"
              (agent-repl--slash-return)
              (should (equal (nth 1 send-args) "   ")))))))))

(ert-deftest agent-repl-test-exit-slash-mode-clears-state ()
  "`agent-repl--exit-slash-mode' clears stack and disables the minor mode."
  (agent-repl-test--with-temp-buffer " *test-exit-slash*"
    (setq-local agent-repl--slash-stack '("a" "b"))
    (agent-repl-slash-input-mode 1)
    (agent-repl--exit-slash-mode)
    (should (null agent-repl--slash-stack))
    (should-not agent-repl-slash-input-mode)))

(ert-deftest agent-repl-test-slash-tab-sends-tab ()
  "`agent-repl--slash-tab' sends a tab character and pushes to stack."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer " *test-slash-tab*"
      (setq-local agent-repl--slash-stack '("/"))
      (let ((sent nil))
        (agent-repl-test--with-temp-buffer "*agent-panel-slash-tab-vterm*"
          (agent-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                    ((symbol-function 'agent-repl--vterm-live-p) (lambda () t))
                    ((symbol-function 'vterm-send-string)
                     (lambda (s &rest _) (setq sent s))))
            (with-current-buffer " *test-slash-tab*"
              (agent-repl--slash-tab)
              (should (equal sent "\t"))
              (should (equal agent-repl--slash-stack '("\t" "/"))))))))))

;;;; ---- Tests: passthrough-start ----

(ert-deftest agent-repl-test-passthrough-start-empty-buffer ()
  "In empty buffer, passthrough-start should enter slash mode and forward char."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer " *test-passthrough-empty*"
      (setq-local agent-repl--slash-stack nil)
      (let ((sent nil))
        (agent-repl-test--with-temp-buffer "*agent-panel-pt-vterm*"
          (agent-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                    ((symbol-function 'agent-repl--vterm-live-p) (lambda () t))
                    ((symbol-function 'vterm-send-string)
                     (lambda (s &rest _) (setq sent s))))
            (with-current-buffer " *test-passthrough-empty*"
              (agent-repl--passthrough-start "/")
              (should agent-repl-slash-input-mode)
              (should (equal agent-repl--slash-stack '("/")))
              (should (equal sent "/")))))))))

(ert-deftest agent-repl-test-passthrough-start-nonempty-inserts ()
  "In non-empty buffer, passthrough-start should insert the char normally."
  (agent-repl-test--with-temp-buffer " *test-passthrough-nonempty*"
    (insert "existing")
    (cl-letf (((symbol-function 'self-insert-command)
               (lambda (_n &optional _ch) (insert "/"))))
      (agent-repl--passthrough-start "/")
      (should-not (bound-and-true-p agent-repl-slash-input-mode))
      (should (string-match-p "/" (buffer-string))))))

;;;; ---- Tests: deferred action helpers ----

(ert-deftest agent-repl-test-run-deferred-action-live-buffer ()
  "Deferred action should run when buffer is alive."
  (agent-repl-test--with-temp-buffer "*test-deferred*"
    (let ((ran nil))
      (agent-repl--run-deferred-action (current-buffer) (lambda () (setq ran t)))
      (should ran))))

(ert-deftest agent-repl-test-run-deferred-action-dead-buffer ()
  "Deferred action should NOT run when buffer is dead."
  (let ((ran nil)
        (buf (get-buffer-create "*test-deferred-dead*")))
    (kill-buffer buf)
    (agent-repl--run-deferred-action buf (lambda () (setq ran t)))
    (should-not ran)))

(ert-deftest agent-repl-test-vterm-deferred-action-calls-run-at-time ()
  "Should schedule action via run-at-time."
  (let ((timer-args nil))
    (agent-repl-test--with-temp-buffer "*test-deferred-timer*"
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (&rest args) (setq timer-args args))))
        (agent-repl--vterm-deferred-action (current-buffer) 0.5 #'ignore)
        (should timer-args)
        (should (= (car timer-args) 0.5))))))

;;;; ---- Tests: backspace intercept ----

(ert-deftest agent-repl-test-slash-intercept-backspace-in-slash-mode ()
  "In slash mode, intercept should redirect to slash-backspace."
  (agent-repl-test--with-temp-buffer " *test-intercept-slash*"
    (setq-local agent-repl-slash-input-mode t)
    (let ((this-command 'evil-delete-backward-char-and-join))
      (agent-repl--slash-intercept-backspace)
      (should (eq this-command #'agent-repl--slash-backspace)))))

(ert-deftest agent-repl-test-slash-intercept-backspace-empty-no-slash ()
  "Outside slash mode with empty buffer, should forward backspace to vterm."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer " *test-intercept-empty*"
      (setq-local agent-repl-slash-input-mode nil)
      (let ((this-command 'delete-backward-char)
            (sent-key nil))
        (agent-repl-test--with-temp-buffer "*agent-panel-intercept-vterm*"
          (agent-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                    ((symbol-function 'agent-repl--vterm-live-p) (lambda () t))
                    ((symbol-function 'vterm-send-key)
                     (lambda (key &rest _) (setq sent-key key))))
            (with-current-buffer " *test-intercept-empty*"
              (agent-repl--slash-intercept-backspace)
              (should (equal sent-key "<backspace>")))))))))

(ert-deftest agent-repl-test-slash-intercept-backspace-nonempty-noop ()
  "Outside slash mode with non-empty buffer, backspace intercept should be a no-op."
  (agent-repl-test--with-temp-buffer " *test-intercept-nonempty*"
    (setq-local agent-repl-slash-input-mode nil)
    (insert "text")
    (let ((this-command 'delete-backward-char))
      (agent-repl--slash-intercept-backspace)
      ;; Command should be unchanged
      (should (eq this-command 'delete-backward-char)))))

(ert-deftest agent-repl-test-slash-intercept-ignores-non-backspace ()
  "Intercept should ignore commands not in the backspace list."
  (agent-repl-test--with-temp-buffer " *test-intercept-other*"
    (setq-local agent-repl-slash-input-mode t)
    (let ((this-command 'self-insert-command))
      (agent-repl--slash-intercept-backspace)
      ;; Should remain unchanged since self-insert-command is not in the list
      (should (eq this-command 'self-insert-command)))))

;;;; ---- Tests: edge cases (new coverage) ----

;;; Backspace: leading whitespace in buffer (buffer-size > 0 but looks blank)

(ert-deftest agent-repl-test-slash-intercept-backspace-whitespace-only ()
  "Whitespace-only buffer has buffer-size > 0, so backspace should NOT forward to vterm."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer " *test-intercept-ws*"
      (setq-local agent-repl-slash-input-mode nil)
      (insert "   ")
      (let ((this-command 'delete-backward-char)
            (sent-key nil))
        (agent-repl-test--with-temp-buffer "*agent-panel-intercept-ws-vterm*"
          (agent-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                    ((symbol-function 'agent-repl--vterm-live-p) (lambda () t))
                    ((symbol-function 'vterm-send-key)
                     (lambda (key &rest _) (setq sent-key key))))
            (with-current-buffer " *test-intercept-ws*"
              (agent-repl--slash-intercept-backspace)
              ;; buffer-size > 0, so no forwarding
              (should-not sent-key)
              ;; Command should remain unchanged
              (should (eq this-command 'delete-backward-char)))))))))

;;; agent-repl-input-mode: mode setup

(ert-deftest agent-repl-test-agent-repl-input-mode-setup ()
  "`agent-repl-input-mode' sets header-line, visual-line-mode, and installs hooks."
  (agent-repl-test--with-temp-buffer " *test-input-mode*"
    (cl-letf (((symbol-function 'agent-repl--set-buffer-background) #'ignore))
      (agent-repl-input-mode))
    ;; Header line should be set
    (should (stringp header-line-format))
    ;; Match on "history" rather than a specific keybinding glyph (e.g. "C-r")
    ;; so the assertion stays decoupled from the chord chosen — the structural
    ;; invariant is that the header advertises the history recall.
    (should (string-match-p "history" header-line-format))
    ;; Visual line mode should be enabled
    (should visual-line-mode)
    ;; pre-command-hook should include slash-intercept-backspace
    (should (memq #'agent-repl--slash-intercept-backspace pre-command-hook))
    ;; after-change-functions should include history-on-change
    (should (memq #'agent-repl--history-on-change after-change-functions))))

(ert-deftest agent-repl-test-agent-repl-input-mode-header-omits-direct-send ()
  "The header line no longer advertises the direct-send chords.
The `(ins) <slash>/<digit>/<up>/<down>: direct send' segment was dropped from
the info panel, so the header carries no \"direct send\" advertisement even
though the underlying insert-state passthrough bindings remain live."
  (agent-repl-test--with-temp-buffer " *test-input-mode-header*"
    (cl-letf (((symbol-function 'agent-repl--set-buffer-background) #'ignore))
      (agent-repl-input-mode))
    (should-not (string-match-p "direct send" header-line-format))))

;;; agent-repl-input-mode: visual-line evil integration

(ert-deftest agent-repl-test-agent-repl-input-mode-respects-visual-line-mode ()
  "`agent-repl-input-mode' sets `evil-respect-visual-line-mode' buffer-locally to t.
This is the runtime flag that makes Evil's line-based operators (yy, dd,
cc, Y, D, C) operate on screen lines rather than logical lines, which is
what users expect when composing wrapping prose in the input buffer."
  (agent-repl-test--with-temp-buffer " *test-input-mode-vline-var*"
    (cl-letf (((symbol-function 'agent-repl--set-buffer-background) #'ignore))
      (agent-repl-input-mode))
    (should (local-variable-p 'evil-respect-visual-line-mode))
    (should (eq evil-respect-visual-line-mode t))))

;;; The bindings below are declared as data in
;;; `agent-repl--visual-line-bindings' (an alist of `(STATE KEY COMMAND)'
;;; triples) and then applied via `evil-define-key'.  The tests assert the
;;; data is well-formed and contains the intended pairs, since
;;; `evil-define-key' is a no-op stub in this test harness and cannot be
;;; queried back through `lookup-key'.

(ert-deftest agent-repl-test-visual-line-bindings-cover-three-evil-states ()
  "Each motion key in `agent-repl--visual-line-bindings' is bound in normal,
motion, and visual state.  This makes the visual-line behavior consistent
across all three states the user might trigger a motion from."
  (let ((keys '("j" "k" "0" "^" "$" "gj" "gk" "g0" "g$")))
    (dolist (key keys)
      (dolist (state '(normal motion visual))
        (should
         (cl-find-if (lambda (b)
                       (and (eq (nth 0 b) state)
                            (string= (nth 1 b) key)))
                     agent-repl--visual-line-bindings))))))

(ert-deftest agent-repl-test-visual-line-bindings-j-is-next-visual-line ()
  "`j' is bound to `evil-next-visual-line' in normal state."
  (should (member '(normal "j" evil-next-visual-line)
                  agent-repl--visual-line-bindings)))

(ert-deftest agent-repl-test-visual-line-bindings-k-is-previous-visual-line ()
  "`k' is bound to `evil-previous-visual-line' in normal state."
  (should (member '(normal "k" evil-previous-visual-line)
                  agent-repl--visual-line-bindings)))

(ert-deftest agent-repl-test-visual-line-bindings-0-is-beginning-of-visual-line ()
  "`0' is bound to `evil-beginning-of-visual-line' in normal state."
  (should (member '(normal "0" evil-beginning-of-visual-line)
                  agent-repl--visual-line-bindings)))

(ert-deftest agent-repl-test-visual-line-bindings-dollar-is-end-of-visual-line ()
  "`$' is bound to `evil-end-of-visual-line' in normal state."
  (should (member '(normal "$" evil-end-of-visual-line)
                  agent-repl--visual-line-bindings)))

(ert-deftest agent-repl-test-visual-line-bindings-caret-is-first-non-blank-of-visual-line ()
  "`^' is bound to `evil-first-non-blank-of-visual-line' in normal state."
  (should (member '(normal "^" evil-first-non-blank-of-visual-line)
                  agent-repl--visual-line-bindings)))

(ert-deftest agent-repl-test-visual-line-bindings-capital-V-is-screen-line ()
  "`V' is bound to `evil-visual-screen-line' in normal state.
This makes `V' select by screen line rather than logical line, matching
the rest of the visual-line motion family."
  (should (member '(normal "V" evil-visual-screen-line)
                  agent-repl--visual-line-bindings)))

(ert-deftest agent-repl-test-visual-line-bindings-gj-is-logical-next-line ()
  "`gj' is bound to `evil-next-line' as the logical-line escape hatch
counterpart to the rebound `j' (mirroring evil's standard
`evil-respect-visual-line-mode' integration)."
  (should (member '(normal "gj" evil-next-line)
                  agent-repl--visual-line-bindings)))

(ert-deftest agent-repl-test-visual-line-bindings-gk-is-logical-previous-line ()
  "`gk' is bound to `evil-previous-line' as the logical-line escape hatch
counterpart to the rebound `k'."
  (should (member '(normal "gk" evil-previous-line)
                  agent-repl--visual-line-bindings)))

;;; discard-input with active slash mode

(ert-deftest agent-repl-test-discard-input-exits-slash-mode ()
  "`agent-repl-discard-input' should exit slash mode when it is active."
  (agent-repl-test--with-temp-buffer " *test-discard-slash*"
    (setq-local agent-repl--input-history nil)
    (setq-local agent-repl--history-index 0)
    (setq-local agent-repl--history-navigating nil)
    (setq-local agent-repl--slash-stack '("c" "l" "/"))
    (agent-repl-slash-input-mode 1)
    (insert "text")
    (cl-letf (((symbol-function 'evil-insert-state) #'ignore)
              ((symbol-function 'agent-repl--history-save) #'ignore))
      (agent-repl-discard-input)
      ;; Slash mode should be exited
      (should-not agent-repl-slash-input-mode)
      (should (null agent-repl--slash-stack))
      ;; Buffer should be cleared
      (should (equal (buffer-string) "")))))

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

;;; discard-or-send-interrupt with whitespace-only buffer

;;; send-vterm-key with dead vterm buffer

(ert-deftest agent-repl-test-send-vterm-key-dead-vterm-noop ()
  "`agent-repl--send-vterm-key' should be a no-op when vterm buffer is dead."
  (agent-repl-test--with-clean-state
    (let ((buf (get-buffer-create "*agent-panel-dead-key*"))
          (sent nil))
      (agent-repl--ws-put "test-ws" :vterm-buffer buf)
      (kill-buffer buf)
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'vterm-send-key)
                 (lambda (&rest _) (setq sent t))))
        (agent-repl--send-vterm-key "<up>")
        (should-not sent)))))

;;; scroll-down with dead vterm buffer

(ert-deftest agent-repl-test-scroll-down-dead-vterm-noop ()
  "`agent-repl-scroll-down' should be a no-op when vterm buffer is dead."
  (agent-repl-test--with-clean-state
    (let ((buf (get-buffer-create "*agent-panel-dead-scroll-down*"))
          (called nil))
      (agent-repl--ws-put "test-ws" :vterm-buffer buf)
      (kill-buffer buf)
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'vterm-send-down)
                 (lambda () (setq called t))))
        (agent-repl-scroll-down)
        (should-not called)))))

;;; scroll-up with dead vterm buffer

(ert-deftest agent-repl-test-scroll-up-dead-vterm-noop ()
  "`agent-repl-scroll-up' should be a no-op when vterm buffer is dead."
  (agent-repl-test--with-clean-state
    (let ((buf (get-buffer-create "*agent-panel-dead-scroll-up*"))
          (called nil))
      (agent-repl--ws-put "test-ws" :vterm-buffer buf)
      (kill-buffer buf)
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'vterm-send-up)
                 (lambda () (setq called t))))
        (agent-repl-scroll-up)
        (should-not called)))))

;;; scroll-vterm-output: sets window-start LINES away from the current start

(ert-deftest agent-repl-test-scroll-vterm-output-shifts-window-start ()
  "`agent-repl--scroll-vterm-output' moves `window-start' by LINES via
`set-window-start'.  Asserts the new start matches `forward-line LINES'
from the previous start, computed in the vterm buffer."
  (agent-repl-test--with-clean-state
    (let ((set-start-args nil))
      (agent-repl-test--with-temp-buffer "*agent-panel-scroll-output*"
        ;; Seed the buffer with enough content to make forward-line meaningful.
        (insert (mapconcat (lambda (i) (format "line %d" i))
                           (number-sequence 1 50) "\n"))
        (goto-char (point-min))
        (forward-line 30)
        (let ((seed-start (point)))
          (agent-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                    ((symbol-function 'agent-repl--vterm-live-p) (lambda () t))
                    ((symbol-function 'get-buffer-window)
                     (lambda (&rest _) (selected-window)))
                    ((symbol-function 'window-start)
                     (lambda (&rest _) seed-start))
                    ((symbol-function 'set-window-start)
                     (lambda (win pos &optional noforce)
                       (setq set-start-args (list win pos noforce)))))
            (agent-repl--scroll-vterm-output -5)
            (let ((expected (with-current-buffer (current-buffer)
                              (save-excursion (goto-char seed-start)
                                              (forward-line -5)
                                              (point)))))
              (should (equal (nth 1 set-start-args) expected))
              ;; NOFORCE must be non-nil so vterm's bottom-cursor point
              ;; doesn't auto-recenter the window back to the prompt.
              (should (nth 2 set-start-args)))))))))

;;; scroll-vterm-output: no vterm window is a no-op

(ert-deftest agent-repl-test-scroll-vterm-output-no-window-noop ()
  "`agent-repl--scroll-vterm-output' is a no-op when vterm has no visible window."
  (agent-repl-test--with-clean-state
    (let ((set-start-called nil))
      (agent-repl-test--with-temp-buffer "*agent-panel-scroll-nowin*"
        (agent-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                  ((symbol-function 'agent-repl--vterm-live-p) (lambda () t))
                  ((symbol-function 'get-buffer-window)
                   (lambda (&rest _) nil))
                  ((symbol-function 'set-window-start)
                   (lambda (&rest _) (setq set-start-called t))))
          (agent-repl--scroll-vterm-output 10)
          (should-not set-start-called))))))

;;; scroll-vterm-output: must also move window-point so upward scroll
;;; can pass vterm's bottom-anchored buffer point.  Without this,
;;; redisplay snaps window-start back down to keep the recorded
;;; window-point visible, capping how far up the user can scroll.

(ert-deftest agent-repl-test-scroll-vterm-output-moves-window-point ()
  "`agent-repl--scroll-vterm-output' must call `set-window-point' on the
vterm window with the new start position, so redisplay does not snap
`window-start' back to keep vterm's bottom-anchored point visible."
  (agent-repl-test--with-clean-state
    (let ((set-point-args nil))
      (agent-repl-test--with-temp-buffer "*agent-panel-scroll-point*"
        (insert (mapconcat (lambda (i) (format "line %d" i))
                           (number-sequence 1 50) "\n"))
        (goto-char (point-min))
        (forward-line 30)
        (let ((seed-start (point)))
          (agent-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                    ((symbol-function 'agent-repl--vterm-live-p) (lambda () t))
                    ((symbol-function 'get-buffer-window)
                     (lambda (&rest _) (selected-window)))
                    ((symbol-function 'window-start)
                     (lambda (&rest _) seed-start))
                    ((symbol-function 'set-window-start) (lambda (&rest _) nil))
                    ((symbol-function 'set-window-point)
                     (lambda (win pos) (setq set-point-args (list win pos)))))
            (agent-repl--scroll-vterm-output -5)
            (let ((expected (with-current-buffer (current-buffer)
                              (save-excursion (goto-char seed-start)
                                              (forward-line -5)
                                              (point)))))
              (should set-point-args)
              (should (equal (nth 1 set-point-args) expected)))))))))

;;; scroll-vterm-output: must NOT select the vterm window (the bug fix).
;;; Selecting vterm even briefly fires window-selection-change-functions,
;;; which schedules reset-vterm-cursors that snaps vterm back to its
;;; bottom-cursor — undoing the user's scroll a moment later.

(ert-deftest agent-repl-test-scroll-vterm-output-does-not-select-vterm ()
  "`agent-repl--scroll-vterm-output' never calls `select-window' on the
vterm window.  The fix replaces a `with-selected-window' approach with
direct `set-window-start' so no selection-change hook fires."
  (agent-repl-test--with-clean-state
    (let ((selected nil))
      (agent-repl-test--with-temp-buffer "*agent-panel-scroll-noselect*"
        (agent-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                  ((symbol-function 'agent-repl--vterm-live-p) (lambda () t))
                  ((symbol-function 'get-buffer-window)
                   (lambda (&rest _) (selected-window)))
                  ((symbol-function 'select-window)
                   (lambda (win &optional _norecord) (push win selected)))
                  ((symbol-function 'set-window-start) (lambda (&rest _) nil)))
          (agent-repl--scroll-vterm-output -5)
          (should-not selected))))))

;;; scroll-output-up scrolls by negative agent-repl-scroll-lines (older content)

(ert-deftest agent-repl-test-scroll-output-up-uses-negative-lines ()
  "`agent-repl-scroll-output-up' delegates with `(- agent-repl-scroll-lines)'
so the vterm window scrolls backward toward older output."
  (agent-repl-test--with-clean-state
    (let ((delegated nil)
          (agent-repl-scroll-lines 15))
      (cl-letf (((symbol-function 'agent-repl--scroll-vterm-output)
                 (lambda (lines) (setq delegated lines))))
        (agent-repl-scroll-output-up)
        (should (equal delegated -15))))))

;;; scroll-output-down scrolls by positive agent-repl-scroll-lines (newer content)

(ert-deftest agent-repl-test-scroll-output-down-uses-positive-lines ()
  "`agent-repl-scroll-output-down' delegates with `agent-repl-scroll-lines'
so the vterm window scrolls forward toward newer output."
  (agent-repl-test--with-clean-state
    (let ((delegated nil)
          (agent-repl-scroll-lines 15))
      (cl-letf (((symbol-function 'agent-repl--scroll-vterm-output)
                 (lambda (lines) (setq delegated lines))))
        (agent-repl-scroll-output-down)
        (should (equal delegated 15))))))

;;; C-S-n / C-S-p must NOT be bound in the input map — the global
;;; drawer-nav bindings need to fall through to global-map.  S-<up>
;;; / S-<down> are the dedicated scroll keys in the input buffer.

(ert-deftest agent-repl-test-input-map-does-not-shadow-csn ()
  "`agent-repl-input-mode-map' must not bind `C-S-n' so the global drawer-nav
binding falls through.  Asserts the local key is unbound in the map."
  (should-not (lookup-key agent-repl-input-mode-map (kbd "C-S-n"))))

(ert-deftest agent-repl-test-input-map-does-not-shadow-csp ()
  "`agent-repl-input-mode-map' must not bind `C-S-p' so the global drawer-nav
binding falls through.  Asserts the local key is unbound in the map."
  (should-not (lookup-key agent-repl-input-mode-map (kbd "C-S-p"))))

(ert-deftest agent-repl-test-input-map-does-not-shadow-csj ()
  "`agent-repl-input-mode-map' must not bind `C-S-j' so the global
scroll-output binding falls through everywhere, including in the
agent input buffer."
  (should-not (lookup-key agent-repl-input-mode-map (kbd "C-S-j"))))

(ert-deftest agent-repl-test-input-map-does-not-shadow-csk ()
  "`agent-repl-input-mode-map' must not bind `C-S-k' so the global
scroll-output binding falls through everywhere, including in the
agent input buffer."
  (should-not (lookup-key agent-repl-input-mode-map (kbd "C-S-k"))))

;;; send-char with dead vterm buffer

(ert-deftest agent-repl-test-send-char-dead-vterm-noop ()
  "`agent-repl-send-char' should be a no-op when vterm buffer is dead."
  (agent-repl-test--with-clean-state
    (let ((buf (get-buffer-create "*agent-panel-dead-sendchar*"))
          (sent nil))
      (agent-repl--ws-put "test-ws" :vterm-buffer buf)
      (kill-buffer buf)
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'vterm-send-string)
                 (lambda (&rest _) (setq sent t))))
        (agent-repl-send-char "y")
        (should-not sent)))))

;;; send-y macro-generated command

(ert-deftest agent-repl-test-send-y-calls-send-char ()
  "`agent-repl--send-y' sends \"y\" via `agent-repl-send-char'."
  (agent-repl-test--with-clean-state
    (let ((char-sent nil))
      (cl-letf (((symbol-function 'agent-repl-send-char)
                 (lambda (c) (setq char-sent c))))
        (agent-repl--send-y)
        (should (equal char-sent "y"))))))

;;; send-n macro-generated command

(ert-deftest agent-repl-test-send-n-calls-send-char ()
  "`agent-repl--send-n' sends \"n\" via `agent-repl-send-char'."
  (agent-repl-test--with-clean-state
    (let ((char-sent nil))
      (cl-letf (((symbol-function 'agent-repl-send-char)
                 (lambda (c) (setq char-sent c))))
        (agent-repl--send-n)
        (should (equal char-sent "n"))))))

;;; define-send-char-command macro expansion correctness

(ert-deftest agent-repl-test-define-send-char-macro-creates-interactive-command ()
  "Macro `agent-repl--define-send-char-command' creates an interactive function."
  ;; agent-repl--send-y and agent-repl--send-n were defined via the macro
  (should (fboundp 'agent-repl--send-y))
  (should (commandp 'agent-repl--send-y))
  (should (fboundp 'agent-repl--send-n))
  (should (commandp 'agent-repl--send-n)))

;;; input-send-digit-char: extracts digit from last-command-event

(ert-deftest agent-repl-test-input-send-digit-char ()
  "`agent-repl--input-send-digit-char' extracts digit from `last-command-event' and sends it."
  (agent-repl-test--with-clean-state
    (let ((char-sent nil))
      (cl-letf (((symbol-function 'agent-repl-send-char)
                 (lambda (c) (setq char-sent c))))
        ;; Simulate C-S-3: event-basic-type strips modifiers, returning ?3
        (let ((last-command-event ?3))
          (agent-repl--input-send-digit-char)
          (should (equal char-sent "3")))))))

;;; insert-digit-or-passthrough: empty buffer -> passthrough

(ert-deftest agent-repl-test-insert-digit-or-passthrough-empty ()
  "In an empty buffer, digit key should enter passthrough mode."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer " *test-digit-empty*"
      (setq-local agent-repl--slash-stack nil)
      (let ((sent nil))
        (agent-repl-test--with-temp-buffer "*agent-panel-digit-vterm*"
          (agent-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                    ((symbol-function 'agent-repl--vterm-live-p) (lambda () t))
                    ((symbol-function 'vterm-send-string)
                     (lambda (s &rest _) (setq sent s))))
            (with-current-buffer " *test-digit-empty*"
              (let ((last-command-event ?5))
                (agent-repl--insert-digit-or-passthrough)
                ;; Should have entered slash mode
                (should agent-repl-slash-input-mode)
                ;; Should have sent "5" to vterm
                (should (equal sent "5"))
                ;; Stack should contain "5"
                (should (equal agent-repl--slash-stack '("5")))))))))))

;;; insert-digit-or-passthrough: non-empty buffer -> self-insert

(ert-deftest agent-repl-test-insert-digit-or-passthrough-nonempty ()
  "In a non-empty buffer, digit key should self-insert."
  (agent-repl-test--with-temp-buffer " *test-digit-nonempty*"
    (insert "existing")
    (let ((inserted nil))
      (cl-letf (((symbol-function 'self-insert-command)
                 (lambda (_n &optional _ch) (setq inserted t))))
        (let ((last-command-event ?7))
          (agent-repl--insert-digit-or-passthrough)
          (should inserted)
          (should-not (bound-and-true-p agent-repl-slash-input-mode)))))))

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

;;; do-send: dead vterm buffer

(ert-deftest agent-repl-test-do-send-dead-vterm ()
  "`agent-repl--do-send' should still increment counter etc. even with a dead vterm buffer."
  (agent-repl-test--with-clean-state
    (agent-repl-test--use-vterm-frontend)
    (let ((buf (get-buffer-create "*agent-panel-dead-do-send*")))
      (agent-repl--ws-put "ws1" :vterm-buffer buf)
      (agent-repl--ws-put "ws1" :prefix-counter 5)
      (kill-buffer buf)
      (cl-letf (((symbol-function 'agent-repl--send-input-to-vterm) #'ignore)
                ((symbol-function 'agent-repl--run-send-posthooks) #'ignore)
                ((symbol-function 'agent-repl--pin-owning-workspace) #'ignore))
        (agent-repl--do-send "ws1" "input" "raw"))
      ;; Counter should still be incremented
      (should (= (agent-repl--ws-get "ws1" :prefix-counter) 6)))))

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
        (agent-repl-test--with-temp-buffer "*agent-panel-send-force-vterm*"
          (agent-repl--ws-put "ws1" :vterm-buffer (current-buffer))
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
                    ((symbol-function 'agent-repl--prepare-input)
                     (lambda (_ws raw &optional force)
                       (setq prepare-force force)
                       raw))
                    ((symbol-function 'agent-repl--do-send) #'ignore)
                    ((symbol-function 'agent-repl--history-save) #'ignore))
            (agent-repl--send nil "ws1" t)
            (should prepare-force)))))))

;;; send: dead vterm buffer is a no-op

(ert-deftest agent-repl-test-send-dead-vterm-noop ()
  "`agent-repl--send' does not call do-send when vterm buffer is dead."
  (agent-repl-test--with-clean-state
    (agent-repl-test--use-vterm-frontend)
    (let ((do-send-called nil)
          (buf (get-buffer-create "*agent-panel-send-dead-vterm*")))
      (agent-repl--ws-put "ws1" :vterm-buffer buf)
      (agent-repl-test--with-temp-buffer " *test-send-dead-input*"
        (insert "hello")
        (agent-repl--ws-put "ws1" :input-buffer (current-buffer))
        (kill-buffer buf)
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
                  ((symbol-function 'agent-repl--do-send)
                   (lambda (&rest _) (setq do-send-called t))))
          (agent-repl--send nil "ws1")
          (should-not do-send-called))))))

;;; send-and-hide: calls send then hide-panels

(ert-deftest agent-repl-test-send-and-hide ()
  "`agent-repl-send-and-hide' calls `agent-repl--send' then `agent-repl--hide-panels'."
  (agent-repl-test--with-clean-state
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

;;; vterm-deferred-action: dead buffer at schedule time

(ert-deftest agent-repl-test-vterm-deferred-action-dead-buffer-schedules ()
  "`agent-repl--vterm-deferred-action' still schedules the timer even with dead buffer.
The dead-buffer check happens inside `run-deferred-action' at callback time."
  (let ((timer-args nil)
        (buf (get-buffer-create "*test-deferred-dead-sched*")))
    (kill-buffer buf)
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (&rest args) (setq timer-args args))))
      (agent-repl--vterm-deferred-action buf 0.3 #'ignore)
      ;; run-at-time should still be called (guard is in the callback)
      (should timer-args)
      (should (= (car timer-args) 0.3)))))

;;; bracketed-finalize: sends return + refreshes vterm

(ert-deftest agent-repl-test-bracketed-finalize ()
  "`agent-repl--bracketed-finalize' sends return and refreshes."
  (agent-repl-test--with-temp-buffer "*test-bracketed-fin*"
    (setq-local vterm--term 'fake-term)
    (let ((calls nil))
      (cl-letf (((symbol-function 'vterm-send-return)
                 (lambda () (push 'return calls)))
                ((symbol-function 'agent-repl--refresh-vterm)
                 (lambda () (push 'refresh calls))))
        (agent-repl--bracketed-finalize)
        (should (equal (reverse calls) '(return refresh)))))))

;;; bracketed-send-return: sends return + schedules finalize

(ert-deftest agent-repl-test-bracketed-send-return ()
  "`agent-repl--bracketed-send-return' sends return and schedules finalize via deferred action.
The submission return goes through the libvterm keyboard handler
\(`vterm-send-key \"\\C-m\"'), not the raw `process-send-string'
path — see the regression note below."
  (agent-repl-test--with-clean-state
    (let ((key-arg nil)
          (deferred-args nil))
      (agent-repl-test--with-temp-buffer "*test-bracketed-return*"
        (setq-local vterm--term 'fake-term)
        (cl-letf (((symbol-function 'vterm-send-key)
                   (lambda (key &rest _) (setq key-arg key)))
                  ((symbol-function 'agent-repl--vterm-deferred-action)
                   (lambda (&rest args) (setq deferred-args args))))
          (agent-repl--bracketed-send-return (current-buffer))
          (should (equal key-arg "\C-m"))
          ;; Deferred action should be scheduled with the buffer and 0.05 delay
          (should deferred-args)
          (should (eq (car deferred-args) (current-buffer)))
          (should (= (cadr deferred-args) 0.05)))))))

(ert-deftest agent-repl-test-bracketed-send-return-avoids-raw-pty-write ()
  "`agent-repl--bracketed-send-return' never submits via raw `vterm-send-return'.
Regression guard for the intermittent-RET bug: Claude's Ink TUI does
not always register a raw `\\C-m'/`\\C-j' byte as an Enter keystroke, so
the non-empty-input submission return must route through libvterm's
keyboard handler (`vterm-send-key') — mirroring the empty-buffer
bare-RET fix in commit 190622a7."
  (agent-repl-test--with-clean-state
    (let ((raw-called nil))
      (agent-repl-test--with-temp-buffer "*test-bracketed-return-noraw*"
        (setq-local vterm--term 'fake-term)
        (cl-letf (((symbol-function 'vterm-send-key) #'ignore)
                  ((symbol-function 'vterm-send-return)
                   (lambda () (setq raw-called t)))
                  ((symbol-function 'agent-repl--vterm-deferred-action) #'ignore))
          (agent-repl--bracketed-send-return (current-buffer))
          (should-not raw-called))))))

;;; exit-slash-mode: already disabled (idempotent)

(ert-deftest agent-repl-test-exit-slash-mode-idempotent ()
  "`agent-repl--exit-slash-mode' is safe to call when already disabled."
  (agent-repl-test--with-temp-buffer " *test-exit-slash-idem*"
    (setq-local agent-repl--slash-stack nil)
    ;; Mode is already off
    (should-not agent-repl-slash-input-mode)
    ;; Should not error
    (agent-repl--exit-slash-mode)
    (should (null agent-repl--slash-stack))
    (should-not agent-repl-slash-input-mode)))

;;; slash-vterm-send: dead vterm fails loudly (never silently)

(ert-deftest agent-repl-test-slash-vterm-send-dead-vterm-returns-nil-and-errors ()
  "`agent-repl--slash-vterm-send' must return nil, skip the send, log + message
when the workspace's recorded vterm buffer is dead.
Per AGENTS.md \"No Silent Fallbacks\": no silent no-op."
  (agent-repl-test--with-clean-state
    (let ((buf (get-buffer-create "*agent-panel-dead-slash-vterm*"))
          (sent nil)
          (msg-called nil))
      (agent-repl--ws-put "test-ws" :vterm-buffer buf)
      (kill-buffer buf)
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'message) (lambda (&rest _) (setq msg-called t)))
                ((symbol-function 'vterm-send-string)
                 (lambda (&rest _) (setq sent t))))
        (should-not (agent-repl--slash-vterm-send "a"))
        (should-not sent)
        (should msg-called)))))

;;; slash-forward-char: uses last-command-event

(ert-deftest agent-repl-test-slash-forward-char ()
  "`agent-repl--slash-forward-char' reads `last-command-event' and forwards that char."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer " *test-slash-fwd-char*"
      (setq-local agent-repl--slash-stack '("/"))
      (let ((sent nil))
        (agent-repl-test--with-temp-buffer "*agent-panel-fwd-char-vterm*"
          (agent-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                    ((symbol-function 'agent-repl--vterm-live-p) (lambda () t))
                    ((symbol-function 'vterm-send-string)
                     (lambda (s &rest _) (setq sent s))))
            (with-current-buffer " *test-slash-fwd-char*"
              (let ((last-command-event ?c))
                (agent-repl--slash-forward-char)
                (should (equal sent "c"))
                (should (equal agent-repl--slash-stack '("c" "/")))))))))))

;;; slash-backspace: already empty stack

(ert-deftest agent-repl-test-slash-backspace-empty-stack ()
  "`agent-repl--slash-backspace' with an already-empty stack exits mode and pops nil."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer " *test-slash-bs-empty*"
      (setq-local agent-repl--slash-stack nil)
      (agent-repl-slash-input-mode 1)
      (agent-repl-test--with-temp-buffer "*agent-panel-slash-bs-empty-vterm*"
        (agent-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                  ((symbol-function 'agent-repl--vterm-live-p) (lambda () t))
                  ((symbol-function 'vterm-send-key) #'ignore))
          (with-current-buffer " *test-slash-bs-empty*"
            (agent-repl--slash-backspace)
            ;; Stack was already empty; pop returns nil; mode exits
            (should (null agent-repl--slash-stack))
            (should-not agent-repl-slash-input-mode)))))))

;;; slash-start: thin wrapper over passthrough-start with "/"

(ert-deftest agent-repl-test-slash-start-delegates-to-passthrough ()
  "`agent-repl--slash-start' calls `agent-repl--passthrough-start' with \"/\"."
  (let ((passthrough-arg nil))
    (cl-letf (((symbol-function 'agent-repl--passthrough-start)
               (lambda (char) (setq passthrough-arg char))))
      (agent-repl--slash-start)
      (should (equal passthrough-arg "/")))))

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

(ert-deftest agent-repl-test-scroll-lines-default ()
  "`agent-repl-scroll-lines' should default to 15."
  (should (= (default-value 'agent-repl-scroll-lines) 15)))

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

;;;; ---- Tests: no-silent-fallback behavior in slash mode (regression suite) ----
;;
;; These tests lock in the contract introduced after the stuck-stack bug:
;; every path that "forwards to vterm" must either succeed and observably
;; reach vterm, or fail loudly (log + user-visible message) AND avoid
;; mutating local state that presumes success.  User input must never be
;; silently dropped.

(ert-deftest agent-repl-test-passthrough-start-no-vterm-inserts-char ()
  "With empty buffer + no live vterm, passthrough-start must NOT enter slash
mode and must insert CHAR as a regular self-insert so user input isn't lost."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer " *test-pt-no-vterm*"
      (setq-local agent-repl--slash-stack nil)
      (let ((msg-called nil))
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                  ;; No :vterm-buffer set → --current-ws-live-vterm returns nil.
                  ((symbol-function 'message) (lambda (&rest _) (setq msg-called t)))
                  ((symbol-function 'vterm-send-string)
                   (lambda (&rest _) (error "MUST NOT forward when vterm missing"))))
          (with-current-buffer " *test-pt-no-vterm*"
            (let ((last-command-event ?/))
              (agent-repl--passthrough-start "/"))
            (should-not agent-repl-slash-input-mode)
            (should (null agent-repl--slash-stack))
            (should (equal (buffer-string) "/"))
            (should msg-called)))))))

(ert-deftest agent-repl-test-slash-forward-char-no-vterm-exits-and-inserts ()
  "If vterm goes away mid-slash-session, forward-char must exit slash mode and
insert the keystroke into the input buffer (never silently dropped)."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer " *test-fwd-no-vterm*"
      (setq-local agent-repl--slash-stack '("/"))
      (agent-repl-slash-input-mode 1)
      (let ((msg-called nil))
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                  ((symbol-function 'message) (lambda (&rest _) (setq msg-called t)))
                  ((symbol-function 'vterm-send-string)
                   (lambda (&rest _) (error "MUST NOT forward when vterm missing"))))
          (with-current-buffer " *test-fwd-no-vterm*"
            (let ((last-command-event ?x))
              (agent-repl--slash-forward-char))
            (should-not agent-repl-slash-input-mode)
            (should (null agent-repl--slash-stack))
            (should (equal (buffer-string) "x"))
            (should msg-called)))))))

(ert-deftest agent-repl-test-slash-backspace-no-vterm-exits-loudly ()
  "Slash backspace with no live vterm must log + message AND exit slash mode,
not pop a phantom entry from the stack."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer " *test-bs-no-vterm*"
      (setq-local agent-repl--slash-stack '("a" "/"))
      (agent-repl-slash-input-mode 1)
      (let ((msg-called nil))
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                  ((symbol-function 'message) (lambda (&rest _) (setq msg-called t)))
                  ((symbol-function 'vterm-send-key)
                   (lambda (&rest _) (error "MUST NOT forward when vterm missing"))))
          (with-current-buffer " *test-bs-no-vterm*"
            (agent-repl--slash-backspace)
            (should-not agent-repl-slash-input-mode)
            (should (null agent-repl--slash-stack))
            (should msg-called)))))))

(ert-deftest agent-repl-test-slash-return-no-vterm-still-exits ()
  "Slash return with no live vterm must still exit slash mode (and surface error)."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer " *test-ret-no-vterm*"
      (setq-local agent-repl--slash-stack '("a" "/"))
      (agent-repl-slash-input-mode 1)
      (let ((msg-called nil))
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                  ((symbol-function 'message) (lambda (&rest _) (setq msg-called t)))
                  ((symbol-function 'vterm-send-return)
                   (lambda (&rest _) (error "MUST NOT forward when vterm missing"))))
          (with-current-buffer " *test-ret-no-vterm*"
            (agent-repl--slash-return)
            (should-not agent-repl-slash-input-mode)
            (should (null agent-repl--slash-stack))
            (should msg-called)))))))

(ert-deftest agent-repl-test-slash-quit-exits-without-sending ()
  "`agent-repl--slash-quit' (bound to C-g) must exit slash mode without touching vterm."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer " *test-slash-quit*"
      (setq-local agent-repl--slash-stack '("a" "b" "/"))
      (agent-repl-slash-input-mode 1)
      (cl-letf (((symbol-function 'vterm-send-string)
                 (lambda (&rest _) (error "slash-quit must not send")))
                ((symbol-function 'vterm-send-key)
                 (lambda (&rest _) (error "slash-quit must not send"))))
        (with-current-buffer " *test-slash-quit*"
          (agent-repl--slash-quit)
          (should-not agent-repl-slash-input-mode)
          (should (null agent-repl--slash-stack)))))))

;;;; ---- Tests: evil-escape inhibit + insert-state-exit hook (jk flutter fix) ----

(ert-deftest agent-repl-test-slash-mode-enables-evil-escape-inhibit ()
  "Activating slash mode must set `evil-escape-inhibit' buffer-locally.
Without this, Doom's default `jk' escape sequence (150ms delay) causes
every `j' keystroke in slash mode to flutter before reaching vterm."
  (agent-repl-test--with-temp-buffer " *test-slash-inhibit*"
    (setq-local evil-escape-inhibit nil)
    (agent-repl-slash-input-mode 1)
    (should (eq evil-escape-inhibit t))
    (should (local-variable-p 'evil-escape-inhibit))))

(ert-deftest agent-repl-test-slash-mode-disable-clears-evil-escape-inhibit ()
  "Deactivating slash mode must clear the buffer-local `evil-escape-inhibit'."
  (agent-repl-test--with-temp-buffer " *test-slash-uninhibit*"
    (agent-repl-slash-input-mode 1)
    (should (local-variable-p 'evil-escape-inhibit))
    (agent-repl-slash-input-mode -1)
    (should-not (local-variable-p 'evil-escape-inhibit))))

(ert-deftest agent-repl-test-slash-on-insert-state-exit-exits-slash ()
  "Leaving evil insert state (e.g. ESC) must exit slash mode."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer " *test-slash-esc*"
      (setq-local agent-repl--slash-stack '("a" "/"))
      (agent-repl-slash-input-mode 1)
      (with-current-buffer " *test-slash-esc*"
        (agent-repl--slash-on-insert-state-exit)
        (should-not agent-repl-slash-input-mode)
        (should (null agent-repl--slash-stack))))))

(ert-deftest agent-repl-test-slash-on-insert-state-exit-noop-when-not-in-slash ()
  "The insert-state-exit hook must be a no-op when slash mode is NOT active."
  (agent-repl-test--with-temp-buffer " *test-slash-esc-noop*"
    ;; Do not enable slash mode.
    (should-not agent-repl-slash-input-mode)
    ;; Should not error or do anything.
    (agent-repl--slash-on-insert-state-exit)
    (should-not agent-repl-slash-input-mode)))

;;;; ---- Tests: raw-ETX helper (no-silent-fallback on no vterm) ----

(ert-deftest agent-repl-test-vterm-send-raw-ctrl-c-no-vterm-errors-loudly ()
  "With no live vterm, `--vterm-send-raw-ctrl-c' returns nil, skips process-send,
and surfaces a user-visible error."
  (agent-repl-test--with-clean-state
    (let ((sent nil)
          (msg-called nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'message) (lambda (&rest _) (setq msg-called t)))
                ((symbol-function 'process-send-string)
                 (lambda (&rest _) (setq sent t))))
        (should-not (agent-repl--vterm-send-raw-ctrl-c))
        (should-not sent)
        (should msg-called)))))

;;;; ---- Tests: slash workspace injection ----

(ert-deftest agent-repl-test-slash-command-string-reconstructs ()
  "slash-command-string should reconstruct the command from the reversed stack."
  (agent-repl-test--with-temp-buffer " *test-cmd-str*"
    (setq-local agent-repl--slash-stack '("r" "k" "s" "a" "/"))
    (should (equal (agent-repl--slash-command-string) "/askr"))))

(ert-deftest agent-repl-test-slash-command-string-empty ()
  "slash-command-string returns empty string for an empty stack."
  (agent-repl-test--with-temp-buffer " *test-cmd-str-empty*"
    (setq-local agent-repl--slash-stack nil)
    (should (equal (agent-repl--slash-command-string) ""))))

(ert-deftest agent-repl-test-slash-command-string-with-tab ()
  "slash-command-string includes tab characters from the stack."
  (agent-repl-test--with-temp-buffer " *test-cmd-str-tab*"
    (setq-local agent-repl--slash-stack '("\t" "r" "o" "w" "/"))
    (should (equal (agent-repl--slash-command-string) "/wor\t"))))

(ert-deftest agent-repl-test-slash-workspace-command-p-true ()
  "slash-workspace-command-p returns non-nil for /wor prefix."
  (agent-repl-test--with-temp-buffer " *test-ws-cmd-t*"
    (setq-local agent-repl--slash-stack
                (nreverse (mapcar #'string (string-to-list "/workspace-generation"))))
    (should (agent-repl--slash-workspace-command-p))))

(ert-deftest agent-repl-test-slash-workspace-command-p-with-tab ()
  "slash-workspace-command-p returns non-nil for /wor followed by tab."
  (agent-repl-test--with-temp-buffer " *test-ws-cmd-tab*"
    (setq-local agent-repl--slash-stack '("\t" "r" "o" "w" "/"))
    (should (agent-repl--slash-workspace-command-p))))

(ert-deftest agent-repl-test-slash-workspace-command-p-false ()
  "slash-workspace-command-p returns nil for non-/wor commands."
  (agent-repl-test--with-temp-buffer " *test-ws-cmd-f*"
    (setq-local agent-repl--slash-stack '("r" "a" "e" "l" "c" "/"))
    (should-not (agent-repl--slash-workspace-command-p))))

(ert-deftest agent-repl-test-slash-return-injects-source-ws ()
  "Slash return should inject [source-ws:NAME path:DIR] for /wor commands."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer " *test-slash-inject*"
      (setq-local agent-repl--slash-stack
                  (nreverse (mapcar #'string (string-to-list "/workspace-generation"))))
      (agent-repl-slash-input-mode 1)
      (let ((sent-strings nil)
            (return-called nil))
        (agent-repl-test--with-temp-buffer "*agent-panel-slash-inject-vterm*"
          (agent-repl--ws-put "my-ws" :vterm-buffer (current-buffer))
          (agent-repl--ws-put "my-ws" :project-dir "/test/project")
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () "my-ws"))
                    ((symbol-function 'agent-repl--vterm-live-p) (lambda () t))
                    ((symbol-function 'vterm-send-string)
                     (lambda (str &rest _) (push str sent-strings)))
                    ((symbol-function 'agent-repl--vterm-send-return-key-logged)
                     (lambda (_label) (setq return-called t))))
            (with-current-buffer " *test-slash-inject*"
              (agent-repl--slash-return)
              (should return-called)
              ;; The injected string should contain both the workspace name and path.
              (should (cl-some (lambda (s) (string-match-p "\\[source-ws:my-ws path:/test/project\\]" s))
                               sent-strings)))))))))

(ert-deftest agent-repl-test-slash-return-no-inject-for-non-wor ()
  "Slash return should NOT inject [source-ws:] for non-/wor commands."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer " *test-slash-no-inject*"
      (setq-local agent-repl--slash-stack '("r" "a" "e" "l" "c" "/"))
      (agent-repl-slash-input-mode 1)
      (let ((sent-strings nil)
            (return-called nil))
        (agent-repl-test--with-temp-buffer "*agent-panel-slash-no-inject-vterm*"
          (agent-repl--ws-put "my-ws" :vterm-buffer (current-buffer))
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () "my-ws"))
                    ((symbol-function 'agent-repl--vterm-live-p) (lambda () t))
                    ((symbol-function 'vterm-send-string)
                     (lambda (str &rest _) (push str sent-strings)))
                    ((symbol-function 'agent-repl--vterm-send-return-key-logged)
                     (lambda (_label) (setq return-called t))))
            (with-current-buffer " *test-slash-no-inject*"
              (agent-repl--slash-return)
              (should return-called)
              ;; No source-ws tag should have been sent.
              (should-not (cl-some (lambda (s) (string-match-p "source-ws" s))
                                   sent-strings)))))))))

;;;; ---- Tests: Permission state clearing on send ----

(ert-deftest agent-repl-test-note-permission-answered-flips-permission-to-thinking ()
  "`agent-repl--note-permission-answered-by-send' flips :permission -> :thinking.
This is the centralized helper every send path delegates to."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-set-agent-state "ws1" :permission)
    (agent-repl--note-permission-answered-by-send "ws1")
    (should (eq (agent-repl--ws-agent-state "ws1") :thinking))))

(ert-deftest agent-repl-test-note-permission-answered-leaves-non-permission-unchanged ()
  "`agent-repl--note-permission-answered-by-send' leaves a non-:permission state alone."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-set-agent-state "ws1" :idle)
    (agent-repl--note-permission-answered-by-send "ws1")
    (should (eq (agent-repl--ws-agent-state "ws1") :idle))))

(ert-deftest agent-repl-test-do-send-transitions-permission-to-thinking ()
  "`agent-repl--do-send' transitions :permission -> :thinking after sending.
Claude Code does not emit UserPromptSubmit for permission responses.
The flip lives inside `agent-repl--send-input-to-vterm' (the
lowest-level string-send primitive), so the real primitive must run —
only the bracketed transport beneath it is stubbed.  do-send pins the
owning workspace on the vterm buffer before sending, which is what the
primitive resolves the workspace from."
  (agent-repl-test--with-clean-state
    (agent-repl-test--use-vterm-frontend)
    (agent-repl-test--with-temp-buffer "*agent-panel-do-send-perm*"
      (agent-repl--ws-put "ws1" :vterm-buffer (current-buffer))
      (agent-repl--ws-set-agent-state "ws1" :permission)
      (cl-letf (((symbol-function 'agent-repl--send-input-bracketed) #'ignore)
                ((symbol-function 'agent-repl--run-send-posthooks) #'ignore))
        (agent-repl--do-send "ws1" "y" "y"))
      (should (eq (agent-repl--ws-agent-state "ws1") :thinking)))))

(ert-deftest agent-repl-test-do-send-does-not-touch-non-permission-state ()
  "`agent-repl--do-send' only transitions :permission, not other states."
  (agent-repl-test--with-clean-state
    (agent-repl-test--use-vterm-frontend)
    (agent-repl-test--with-temp-buffer "*agent-panel-do-send-think2*"
      (agent-repl--ws-put "ws1" :vterm-buffer (current-buffer))
      (agent-repl--ws-set-agent-state "ws1" :thinking)
      (cl-letf (((symbol-function 'agent-repl--send-input-bracketed) #'ignore)
                ((symbol-function 'agent-repl--run-send-posthooks) #'ignore))
        (agent-repl--do-send "ws1" "input" "raw"))
      (should (eq (agent-repl--ws-agent-state "ws1") :thinking)))))

(ert-deftest agent-repl-test-send-char-transitions-permission-to-thinking ()
  "`agent-repl-send-char' transitions :permission -> :thinking after sending."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer "*agent-panel-sendchar-perm*"
      (agent-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
      (agent-repl--ws-set-agent-state "test-ws" :permission)
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'agent-repl--vterm-live-p) (lambda () t))
                ((symbol-function 'vterm-send-string) #'ignore)
                ((symbol-function 'vterm-send-return) #'ignore))
        (agent-repl-send-char "y"))
      (should (eq (agent-repl--ws-agent-state "test-ws") :thinking)))))

(ert-deftest agent-repl-test-send-char-forces-thinking-from-idle ()
  "`agent-repl-send-char' drives ANY prior state to :thinking, not just :permission.
A single-char send (y/n, digit) is `not directly sent' input Claude
acts on, but it never fires the UserPromptSubmit hook — so the send
itself must mark :thinking even when the prior state is :idle."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer "*agent-panel-sendchar-idle*"
      (agent-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
      (agent-repl--ws-set-agent-state "test-ws" :idle)
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'agent-repl--vterm-live-p) (lambda () t))
                ((symbol-function 'vterm-send-string) #'ignore)
                ((symbol-function 'vterm-send-return) #'ignore))
        (agent-repl-send-char "y"))
      (should (eq (agent-repl--ws-agent-state "test-ws") :thinking)))))

(ert-deftest agent-repl-test-send-char-no-vterm-keeps-permission ()
  "`agent-repl-send-char' does not transition when no vterm exists.
The char was never sent, so the permission prompt is still active."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "test-ws" :vterm-buffer nil)
    (agent-repl--ws-set-agent-state "test-ws" :permission)
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
              ((symbol-function 'agent-repl--vterm-live-p) (lambda () nil)))
      (agent-repl-send-char "y"))
    (should (eq (agent-repl--ws-agent-state "test-ws") :permission))))

(ert-deftest agent-repl-test-slash-return-transitions-permission-to-thinking ()
  "`agent-repl--slash-return' transitions :permission -> :thinking after sending.
Empty input buffer + digit press enters slash mode (passthrough); the
RET finalize then runs through slash-return.  Without this transition,
answering a permission prompt via the digit-passthrough path leaves the
tab stuck at green-❓."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer " *test-slash-return-perm*"
      (setq-local agent-repl--slash-stack '("1"))
      (agent-repl-slash-input-mode 1)
      (agent-repl-test--with-temp-buffer "*agent-panel-slash-return-perm-vterm*"
        (agent-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
        (agent-repl--ws-set-agent-state "test-ws" :permission)
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                  ((symbol-function 'agent-repl--vterm-live-p) (lambda () t))
                  ((symbol-function 'vterm-send-return) #'ignore)
                  ((symbol-function 'agent-repl--run-send-posthooks) #'ignore))
          (with-current-buffer " *test-slash-return-perm*"
            (agent-repl--slash-return))))
      (should (eq (agent-repl--ws-agent-state "test-ws") :thinking)))))

(ert-deftest agent-repl-test-slash-return-forces-thinking-from-idle ()
  "`agent-repl--slash-return' drives ANY prior state to :thinking, not just :permission.
Submitting a slash command is `not directly sent' input Claude acts on
but that never fires the UserPromptSubmit hook, so the submission itself
marks :thinking even when the prior state is :idle."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer " *test-slash-return-idle*"
      (setq-local agent-repl--slash-stack '("/" "m" "o" "d" "e" "l"))
      (agent-repl-slash-input-mode 1)
      (agent-repl-test--with-temp-buffer "*agent-panel-slash-return-idle-vterm*"
        (agent-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
        (agent-repl--ws-set-agent-state "test-ws" :idle)
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                  ((symbol-function 'agent-repl--vterm-live-p) (lambda () t))
                  ((symbol-function 'vterm-send-return) #'ignore)
                  ((symbol-function 'agent-repl--run-send-posthooks) #'ignore))
          (with-current-buffer " *test-slash-return-idle*"
            (agent-repl--slash-return))))
      (should (eq (agent-repl--ws-agent-state "test-ws") :thinking)))))

(ert-deftest agent-repl-test-slash-return-no-vterm-keeps-permission ()
  "`agent-repl--slash-return' does not transition when no vterm exists.
The return was never sent, so the permission prompt is still active."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer " *test-slash-return-no-vterm*"
      (setq-local agent-repl--slash-stack '("1"))
      (agent-repl-slash-input-mode 1)
      (agent-repl--ws-put "test-ws" :vterm-buffer nil)
      (agent-repl--ws-set-agent-state "test-ws" :permission)
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'agent-repl--current-ws-live-vterm) (lambda () nil))
                ((symbol-function 'agent-repl--run-send-posthooks) #'ignore))
        (with-current-buffer " *test-slash-return-no-vterm*"
          (agent-repl--slash-return)))
      (should (eq (agent-repl--ws-agent-state "test-ws") :permission)))))

;;;; ---- Tests: lowest-level send primitives own the :permission flip ----

(ert-deftest agent-repl-test-slash-vterm-send-flips-permission-to-thinking ()
  "`agent-repl--slash-vterm-send' flips :permission -> :thinking on a forward.
A bare digit answering a permission prompt enters passthrough mode and is
committed by the agent's dialog IMMEDIATELY — no RET ever follows — so the
char forward itself is the only observable answer signal."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer "*agent-panel-slash-send-perm*"
      (agent-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
      (agent-repl--ws-set-agent-state "test-ws" :permission)
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'vterm-send-string) #'ignore))
        (should (agent-repl--slash-vterm-send "1")))
      (should (eq (agent-repl--ws-agent-state "test-ws") :thinking)))))

(ert-deftest agent-repl-test-slash-vterm-send-forces-thinking-from-non-permission ()
  "`agent-repl--slash-vterm-send' drives ANY prior state to :thinking on a forward.
A slash/digit forward is `not directly sent' input Claude acts on but
that never fires the UserPromptSubmit hook, so the forward itself marks
:thinking even when the prior state is :done."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer "*agent-panel-slash-send-done*"
      (agent-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
      (agent-repl--ws-set-agent-state "test-ws" :done)
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'vterm-send-string) #'ignore))
        (should (agent-repl--slash-vterm-send "/")))
      (should (eq (agent-repl--ws-agent-state "test-ws") :thinking)))))

(ert-deftest agent-repl-test-slash-vterm-send-no-vterm-keeps-permission ()
  "`agent-repl--slash-vterm-send' does not transition when no live vterm exists.
The char was never forwarded, so the permission prompt is still active."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "test-ws" :vterm-buffer nil)
    (agent-repl--ws-set-agent-state "test-ws" :permission)
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
              ((symbol-function 'message) (lambda (&rest _) nil)))
      (should-not (agent-repl--slash-vterm-send "1")))
    (should (eq (agent-repl--ws-agent-state "test-ws") :permission))))

(ert-deftest agent-repl-test-passthrough-digit-flips-permission-to-thinking ()
  "Typing a digit into an empty input buffer flips :permission -> :thinking.
End-to-end over the dominant permission-answer path:
`agent-repl--passthrough-start' -> slash mode -> char forward."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer " *test-passthrough-digit-input*"
      (agent-repl-test--with-temp-buffer "*agent-panel-passthrough-digit-vterm*"
        (agent-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
        (agent-repl--ws-set-agent-state "test-ws" :permission)
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                  ((symbol-function 'vterm-send-string) #'ignore))
          (with-current-buffer " *test-passthrough-digit-input*"
            (agent-repl--passthrough-start "1")))
        (should (eq (agent-repl--ws-agent-state "test-ws") :thinking))))))

(ert-deftest agent-repl-test-send-input-to-vterm-flips-permission-to-thinking ()
  "`agent-repl--send-input-to-vterm' flips :permission -> :thinking after dispatch."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer "*agent-panel-sitv-perm*"
      (setq-local agent-repl--owning-workspace "wsA")
      (agent-repl--ws-set-agent-state "wsA" :permission)
      (cl-letf (((symbol-function 'agent-repl--send-input-bracketed) #'ignore))
        (agent-repl--send-input-to-vterm (current-buffer) "hello"))
      (should (eq (agent-repl--ws-agent-state "wsA") :thinking)))))

(ert-deftest agent-repl-test-send-input-to-vterm-resolves-ws-from-buffer-owner ()
  "`agent-repl--send-input-to-vterm' flips the OWNER workspace, not the current one.
A programmatic send can target a vterm whose workspace is not the
selected one (e.g. deferred drains); the owner pin on the
buffer is the source of truth."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer "*agent-panel-sitv-owner*"
      (setq-local agent-repl--owning-workspace "wsA")
      (agent-repl--ws-set-agent-state "wsA" :permission)
      (agent-repl--ws-set-agent-state "wsB" :permission)
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "wsB"))
                ((symbol-function 'agent-repl--send-input-bracketed) #'ignore))
        (agent-repl--send-input-to-vterm (current-buffer) "hello"))
      (should (eq (agent-repl--ws-agent-state "wsA") :thinking))
      (should (eq (agent-repl--ws-agent-state "wsB") :permission)))))

(ert-deftest agent-repl-test-send-input-direct-flips-permission-to-thinking ()
  "`agent-repl--send-input-direct' flips :permission -> :thinking after sending."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer "*agent-panel-sid-perm*"
      (setq-local agent-repl--owning-workspace "wsA")
      (agent-repl--ws-set-agent-state "wsA" :permission)
      (cl-letf (((symbol-function 'vterm-send-string) #'ignore)
                ((symbol-function 'vterm-send-return) #'ignore)
                ((symbol-function 'agent-repl--refresh-vterm) #'ignore))
        (agent-repl--send-input-direct (current-buffer) "y"))
      (should (eq (agent-repl--ws-agent-state "wsA") :thinking)))))

(ert-deftest agent-repl-test-return-key-logged-flips-permission-when-delivered ()
  "`agent-repl--vterm-send-return-key-logged' flips :permission when delivered.
With `vterm--term' set, the return reaches libvterm and the flip fires."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer "*agent-panel-rkl-perm*"
      (setq-local agent-repl--owning-workspace "wsA")
      (setq-local vterm--term t)
      (agent-repl--ws-set-agent-state "wsA" :permission)
      (cl-letf (((symbol-function 'vterm-send-key) #'ignore))
        (agent-repl--vterm-send-return-key-logged "test-label"))
      (should (eq (agent-repl--ws-agent-state "wsA") :thinking)))))

(ert-deftest agent-repl-test-return-key-logged-no-flip-when-term-nil ()
  "`agent-repl--vterm-send-return-key-logged' does NOT flip when vterm--term is nil.
The return was never delivered (warning branch), so the permission
prompt is still active and :permission must persist."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer "*agent-panel-rkl-nil*"
      (setq-local agent-repl--owning-workspace "wsA")
      (agent-repl--ws-set-agent-state "wsA" :permission)
      (agent-repl--vterm-send-return-key-logged "test-label")
      (should (eq (agent-repl--ws-agent-state "wsA") :permission)))))

(ert-deftest agent-repl-test-note-permission-answered-for-vterm-falls-back-to-current-ws ()
  "`agent-repl--note-permission-answered-for-vterm' falls back to the current ws.
When the buffer carries no owner pin, the current workspace is flipped."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer "*agent-panel-npafv-fallback*"
      (agent-repl--ws-set-agent-state "ws1" :permission)
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1")))
        (agent-repl--note-permission-answered-for-vterm (current-buffer)))
      (should (eq (agent-repl--ws-agent-state "ws1") :thinking)))))

;;;; ---- Tests: non-direct sends force :thinking (mark-send-thinking) ----

(ert-deftest agent-repl-test-mark-send-thinking-forces-thinking-from-idle ()
  "`agent-repl--mark-send-thinking' drives a non-:permission state to :thinking."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-set-agent-state "ws1" :idle)
    (agent-repl--mark-send-thinking "ws1")
    (should (eq (agent-repl--ws-agent-state "ws1") :thinking))))

(ert-deftest agent-repl-test-mark-send-thinking-forces-thinking-from-done ()
  "`agent-repl--mark-send-thinking' drives a :done state to :thinking."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-set-agent-state "ws1" :done)
    (agent-repl--mark-send-thinking "ws1")
    (should (eq (agent-repl--ws-agent-state "ws1") :thinking))))

(ert-deftest agent-repl-test-mark-send-thinking-nil-ws-is-noop ()
  "`agent-repl--mark-send-thinking' is a no-op when WS is nil (no error)."
  (agent-repl-test--with-clean-state
    (should-not (agent-repl--mark-send-thinking nil))))

(ert-deftest agent-repl-test-mark-send-thinking-for-vterm-resolves-owner ()
  "`agent-repl--mark-send-thinking-for-vterm' marks the buffer OWNER, not current ws."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer "*agent-panel-mstfv-owner*"
      (setq-local agent-repl--owning-workspace "wsA")
      (agent-repl--ws-set-agent-state "wsA" :idle)
      (agent-repl--ws-set-agent-state "wsB" :idle)
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "wsB")))
        (agent-repl--mark-send-thinking-for-vterm (current-buffer)))
      (should (eq (agent-repl--ws-agent-state "wsA") :thinking))
      (should (eq (agent-repl--ws-agent-state "wsB") :idle)))))

(ert-deftest agent-repl-test-mark-send-thinking-for-vterm-falls-back-to-current-ws ()
  "`agent-repl--mark-send-thinking-for-vterm' falls back to the current ws.
When the buffer carries no owner pin, the current workspace is marked."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer "*agent-panel-mstfv-fallback*"
      (agent-repl--ws-set-agent-state "ws1" :idle)
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1")))
        (agent-repl--mark-send-thinking-for-vterm (current-buffer)))
      (should (eq (agent-repl--ws-agent-state "ws1") :thinking)))))

(ert-deftest agent-repl-test-passthrough-digit-forces-thinking-from-idle ()
  "Typing a digit into an empty input buffer forces :thinking from :idle too.
End-to-end over `agent-repl--passthrough-start' -> slash mode -> char
forward: the digit is `not directly sent' input, so it marks :thinking
regardless of the prior state."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer " *test-passthrough-digit-idle-input*"
      (agent-repl-test--with-temp-buffer "*agent-panel-passthrough-digit-idle-vterm*"
        (agent-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
        (agent-repl--ws-set-agent-state "test-ws" :idle)
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                  ((symbol-function 'vterm-send-string) #'ignore))
          (with-current-buffer " *test-passthrough-digit-idle-input*"
            (agent-repl--passthrough-start "1")))
        (should (eq (agent-repl--ws-agent-state "test-ws") :thinking))))))

;;;; ---- Tests: vterm-send-turn meta markers ----

(ert-deftest agent-repl-test-vterm-send-turn-strips-meta-markers ()
  "The vterm paste carries the injected text WITHOUT its meta markers.
The terminal echoes the prompt to a human, and the markers exist only so
the gui frontend can hide the spans they bracket."
  (agent-repl-test--with-clean-state
    (let ((pasted nil))
      (cl-letf (((symbol-function 'agent-repl--pin-owning-workspace) #'ignore)
                ((symbol-function 'agent-repl--send-input-to-vterm)
                 (lambda (_buf input &optional _settle) (setq pasted input)))
                ((symbol-function 'agent-repl--run-send-posthooks) #'ignore)
                ((symbol-function 'agent-repl--kickoff-prompt-summary) #'ignore))
        (agent-repl--vterm-send-turn
         "ws1"
         (concat (agent-repl--meta-wrap "READ-DIRECTIVE") "\n\nhello")
         "hello"))
      (should (equal pasted "READ-DIRECTIVE\n\nhello")))))

(provide 'test-input)

;;; test-input.el ends here
