;;; test-input.el --- Tests for input.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Dedicated test file for input.el: input mode, send pipeline,
;; metaprompt preparation, slash pass-through, and vterm forwarding.

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Tests: Prefix injection counter (migrated) ----

(ert-deftest claude-repl-test-prefix-injection-counter ()
  "Prefix should be injected when counter mod period is 0."
  (claude-repl-test--with-clean-state
    (let ((claude-repl-skip-permissions t)
          (claude-repl-prefix-period 3)
          (claude-repl-command-prefix "TEST")
          (claude-repl--command-prefix "PREFIX: "))
      (claude-repl-test--with-temp-buffer " *test-prefix*"
        (let ((ws "test-ws"))
          (claude-repl--ws-put ws :input-buffer (current-buffer))
          (claude-repl--ws-put ws :prefix-counter 0)
          (insert "hello")
          ;; Counter 0 mod 3 = 0 -> prefix
          (should (string-prefix-p "PREFIX: " (claude-repl--prepare-input ws "hello")))
          ;; Counter 1 mod 3 != 0 -> no prefix
          (claude-repl--ws-put ws :prefix-counter 1)
          (should-not (string-prefix-p "PREFIX: " (claude-repl--prepare-input ws "hello")))
          ;; Counter 3 mod 3 = 0 -> prefix again
          (claude-repl--ws-put ws :prefix-counter 3)
          (should (string-prefix-p "PREFIX: " (claude-repl--prepare-input ws "hello"))))))))

(ert-deftest claude-repl-test-prefix-counter-per-workspace ()
  "Each workspace should maintain its own prefix counter independently."
  (claude-repl-test--with-clean-state
    ;; Set different counters for two workspaces
    (claude-repl--ws-put "ws-a" :prefix-counter 7)
    (claude-repl--ws-put "ws-b" :prefix-counter 42)
    ;; They should be independent
    (should (= (claude-repl--ws-get "ws-a" :prefix-counter) 7))
    (should (= (claude-repl--ws-get "ws-b" :prefix-counter) 42))
    ;; Mutating one should not affect the other
    (claude-repl--ws-put "ws-a" :prefix-counter 8)
    (should (= (claude-repl--ws-get "ws-b" :prefix-counter) 42))))

;;;; ---- Tests: Input preparation (migrated) ----

(ert-deftest claude-repl-test-prepare-input-no-prefix-when-disabled ()
  "When `claude-repl-skip-permissions' is nil, `prepare-input' returns raw text."
  (claude-repl-test--with-clean-state
    (let ((claude-repl-skip-permissions nil)
          (claude-repl-prefix-period 1))
      (claude-repl--ws-put "ws1" :prefix-counter 0)
      (should (equal (claude-repl--prepare-input "ws1" "raw text") "raw text")))))

;;;; ---- Tests: Send functions (migrated) ----

(ert-deftest claude-repl-test-send-char-calls-vterm ()
  "`claude-repl-send-char' should call `vterm-send-string' with the char, then `vterm-send-return'."
  (claude-repl-test--with-clean-state
    (let ((calls nil))
      (claude-repl-test--with-temp-buffer "*claude-abcd1234*"
        (claude-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                  ((symbol-function 'claude-repl--vterm-live-p) (lambda () t))
                  ((symbol-function 'vterm-send-string)
                   (lambda (s &rest _) (push (list 'send-string s) calls)))
                  ((symbol-function 'vterm-send-return)
                   (lambda () (push '(send-return) calls))))
          (claude-repl-send-char "y")
          (should (member '(send-string "y") (reverse calls)))
          (should (member '(send-return) (reverse calls))))))))

(ert-deftest claude-repl-test-scroll-down-sends-down ()
  "`claude-repl-scroll-down' calls `vterm-send-down' in the vterm buffer."
  (claude-repl-test--with-clean-state
    (let ((down-called nil))
      (claude-repl-test--with-temp-buffer "*claude-abcd1234*"
        (claude-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                  ((symbol-function 'claude-repl--vterm-live-p) (lambda () t))
                  ((symbol-function 'vterm-send-down)
                   (lambda () (setq down-called t))))
          (claude-repl-scroll-down)
          (should down-called))))))

(ert-deftest claude-repl-test-scroll-up-sends-up ()
  "`claude-repl-scroll-up' calls `vterm-send-up'."
  (claude-repl-test--with-clean-state
    (let ((up-called nil))
      (claude-repl-test--with-temp-buffer "*claude-abcd1234*"
        (claude-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                  ((symbol-function 'claude-repl--vterm-live-p) (lambda () t))
                  ((symbol-function 'vterm-send-up)
                   (lambda () (setq up-called t))))
          (claude-repl-scroll-up)
          (should up-called))))))

(ert-deftest claude-repl-test-interrupt-sends-escape-twice ()
  "`claude-repl-interrupt' calls `vterm-send-key' with \"<escape>\" twice."
  (claude-repl-test--with-clean-state
    (let ((escape-count 0))
      (claude-repl-test--with-temp-buffer "*claude-abcd1234*"
        (claude-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                  ((symbol-function 'claude-repl--vterm-live-p) (lambda () t))
                  ((symbol-function 'vterm-send-key)
                   (lambda (key &rest _)
                     (when (equal key "<escape>")
                       (cl-incf escape-count)))))
          (claude-repl-interrupt)
          (should (= escape-count 2)))))))

(ert-deftest claude-repl-test-cycle-sends-backtab ()
  "`claude-repl-cycle' calls `vterm-send-key' with \"<backtab>\"."
  (claude-repl-test--with-clean-state
    (let ((backtab-called nil))
      (claude-repl-test--with-temp-buffer "*claude-abcd1234*"
        (claude-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                  ((symbol-function 'claude-repl--vterm-live-p) (lambda () t))
                  ((symbol-function 'vterm-send-key)
                   (lambda (key &rest _)
                     (when (equal key "<backtab>")
                       (setq backtab-called t)))))
          (claude-repl-cycle)
          (should backtab-called))))))

(ert-deftest claude-repl-test-send-input-direct-mode ()
  "For input <=200 chars, `send-input-to-vterm' calls `vterm-send-string' without paste flag."
  (claude-repl-test--with-clean-state
    (let ((send-string-args nil)
          (return-called nil))
      (claude-repl-test--with-temp-buffer "*claude-abcd1234*"
        (cl-letf (((symbol-function 'vterm-send-string)
                   (lambda (s &rest args) (setq send-string-args (cons s args))))
                  ((symbol-function 'vterm-send-return)
                   (lambda () (setq return-called t)))
                  ((symbol-function 'claude-repl--refresh-vterm) #'ignore))
          (claude-repl--send-input-to-vterm (current-buffer) "short input")
          ;; Should have been called with just the string (no paste flag)
          (should (equal (car send-string-args) "short input"))
          (should (null (cdr send-string-args)))
          (should return-called))))))

(ert-deftest claude-repl-test-send-input-paste-mode ()
  "For input >200 chars, calls `vterm-send-string' WITH paste flag, defers return."
  (claude-repl-test--with-clean-state
    (let ((send-string-args nil)
          (return-called nil)
          (timer-args nil))
      (claude-repl-test--with-temp-buffer "*claude-abcd1234*"
        (cl-letf (((symbol-function 'vterm-send-string)
                   (lambda (s &rest args) (setq send-string-args (cons s args))))
                  ((symbol-function 'vterm-send-return)
                   (lambda () (setq return-called t)))
                  ((symbol-function 'claude-repl--refresh-vterm) #'ignore)
                  ((symbol-function 'run-at-time)
                   (lambda (&rest args) (setq timer-args args))))
          (let ((long-input (make-string 201 ?x)))
            (claude-repl--send-input-to-vterm (current-buffer) long-input)
            ;; paste flag (2nd arg) should be t
            (should (equal (cadr send-string-args) t))
            ;; return should NOT have been called directly
            (should-not return-called)
            ;; run-at-time should have been called to defer
            (should timer-args)))))))

;;;; ---- Tests: Composite state functions (migrated) ----

(ert-deftest claude-repl-test-mark-ws-thinking-composite ()
  "`claude-repl--mark-ws-thinking' should set :thinking state."
  (claude-repl-test--with-clean-state
    (claude-repl--mark-ws-thinking "ws1")
    (should (eq (claude-repl--ws-state "ws1") :thinking))))

(ert-deftest claude-repl-test-clear-input-pushes-and-clears ()
  "`claude-repl--commit-input-buffer' pushes text to history, resets index, clears buffer when clear-p."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-buffer " *test-clear-input*"
      (setq-local claude-repl--input-history nil)
      (setq-local claude-repl--history-index 5)
      (setq-local claude-repl--history-navigating nil)
      (claude-repl--ws-put "ws1" :input-buffer (current-buffer))
      (insert "some input text")
      (cl-letf (((symbol-function 'claude-repl--history-save) #'ignore))
        (claude-repl--commit-input-buffer "ws1" (current-buffer) "some input text" t))
      (should (equal claude-repl--input-history '("some input text")))
      (should (= claude-repl--history-index -1))
      (should (equal (buffer-string) "")))))

(ert-deftest claude-repl-test-discard-input-pushes-and-clears ()
  "`claude-repl-discard-input' pushes text, clears buffer, calls `evil-insert-state'."
  (claude-repl-test--with-temp-buffer " *test-discard*"
    (setq-local claude-repl--input-history nil)
    (setq-local claude-repl--history-index 3)
    (setq-local claude-repl--history-navigating nil)
    (insert "discard me")
    (let ((evil-called nil))
      (cl-letf (((symbol-function 'evil-insert-state)
                 (lambda () (setq evil-called t)))
                ((symbol-function 'claude-repl--history-save) #'ignore))
        (claude-repl-discard-input)
        (should (equal claude-repl--input-history '("discard me")))
        (should (= claude-repl--history-index -1))
        (should (equal (buffer-string) ""))
        (should evil-called)))))

;;;; ---- Tests: Paste to vterm (migrated) ----

(ert-deftest claude-repl-test-paste-to-vterm ()
  "claude-repl-paste-to-vterm should call vterm-send-key with C-v args."
  (claude-repl-test--with-clean-state
    (let ((send-key-args nil))
      (claude-repl-test--with-temp-buffer "*claude-abcd1234*"
        (claude-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                  ((symbol-function 'claude-repl--vterm-live-p) (lambda () t))
                  ((symbol-function 'vterm-send-key)
                   (lambda (&rest args) (setq send-key-args args))))
          (claude-repl-paste-to-vterm)
          (should (equal send-key-args '("v" nil nil t))))))))

;;;; ---- Tests: Bug regressions (migrated) ----

(ert-deftest claude-repl-test-bug7-prefix-counter-persists ()
  "Bug 7: prefix counter should persist in the workspaces hash across lookups."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :prefix-counter 42)
    (should (= (claude-repl--ws-get "ws1" :prefix-counter) 42))
    ;; Incrementing via the dedicated helper should work correctly
    (claude-repl--increment-prefix-counter "ws1")
    (should (= (claude-repl--ws-get "ws1" :prefix-counter) 43))))

;;;; ---- Tests: should-prepend-metaprompt-p ----

(ert-deftest claude-repl-test-should-prepend-metaprompt-p-all-conditions ()
  "Test the full matrix of conditions for metaprompt prepending."
  ;; Enabled + prefix set + non-exempt + counter aligned -> t
  (let ((claude-repl-skip-permissions t)
        (claude-repl-command-prefix "TEST")
        (claude-repl--command-prefix "PREFIX: ")
        (claude-repl-prefix-period 3))
    (should (claude-repl--should-prepend-metaprompt-p "hello" 0))
    ;; Counter not aligned -> nil
    (should-not (claude-repl--should-prepend-metaprompt-p "hello" 1))
    (should-not (claude-repl--should-prepend-metaprompt-p "hello" 2))
    ;; Counter aligned again -> t
    (should (claude-repl--should-prepend-metaprompt-p "hello" 6))
    ;; Force bypasses counter
    (should (claude-repl--should-prepend-metaprompt-p "hello" 1 t))
    (should (claude-repl--should-prepend-metaprompt-p "hello" 2 t))))

(ert-deftest claude-repl-test-should-prepend-nil-when-skip-permissions-off ()
  "Returns nil when `claude-repl-skip-permissions' is nil."
  (let ((claude-repl-skip-permissions nil)
        (claude-repl-command-prefix "TEST")
        (claude-repl-prefix-period 1))
    (should-not (claude-repl--should-prepend-metaprompt-p "hello" 0))))

(ert-deftest claude-repl-test-should-prepend-nil-when-no-command-prefix ()
  "Returns nil when `claude-repl-command-prefix' is nil."
  (let ((claude-repl-skip-permissions t)
        (claude-repl-command-prefix nil)
        (claude-repl-prefix-period 1))
    (should-not (claude-repl--should-prepend-metaprompt-p "hello" 0))))

(ert-deftest claude-repl-test-should-prepend-nil-for-exempt-strings ()
  "Returns nil for exempt slash commands even when conditions are met."
  (let ((claude-repl-skip-permissions t)
        (claude-repl-command-prefix "TEST")
        (claude-repl-prefix-period 1))
    (dolist (exempt '("/clear" "/usage" "/login" "/logout"))
      (should-not (claude-repl--should-prepend-metaprompt-p exempt 0)))))

(ert-deftest claude-repl-test-should-prepend-nil-for-bare-numerals ()
  "Returns nil for bare numeral inputs (e.g. '1', '42')."
  (let ((claude-repl-skip-permissions t)
        (claude-repl-command-prefix "TEST")
        (claude-repl-prefix-period 1))
    (should-not (claude-repl--should-prepend-metaprompt-p "1" 0))
    (should-not (claude-repl--should-prepend-metaprompt-p "42" 0))
    (should-not (claude-repl--should-prepend-metaprompt-p "0" 0))))

;;;; ---- Tests: skip-metaprompt-p ----

(ert-deftest claude-repl-test-skip-metaprompt-exempt-strings ()
  "`claude-repl--skip-metaprompt-p' returns non-nil for exempt commands."
  (dolist (cmd '("/clear" "/usage" "/login" "/logout"))
    (should (claude-repl--skip-metaprompt-p cmd))))

(ert-deftest claude-repl-test-skip-metaprompt-bare-numerals ()
  "`claude-repl--skip-metaprompt-p' returns non-nil for bare numerals."
  (should (claude-repl--skip-metaprompt-p "1"))
  (should (claude-repl--skip-metaprompt-p "42"))
  (should (claude-repl--skip-metaprompt-p "007")))

(ert-deftest claude-repl-test-skip-metaprompt-trailing-whitespace ()
  "`claude-repl--skip-metaprompt-p' handles trailing whitespace."
  (should (claude-repl--skip-metaprompt-p "/clear  "))
  (should (claude-repl--skip-metaprompt-p "42\n"))
  (should (claude-repl--skip-metaprompt-p "/usage\t")))

(ert-deftest claude-repl-test-skip-metaprompt-normal-input ()
  "`claude-repl--skip-metaprompt-p' returns nil for normal input."
  (should-not (claude-repl--skip-metaprompt-p "hello world"))
  (should-not (claude-repl--skip-metaprompt-p "fix the bug"))
  (should-not (claude-repl--skip-metaprompt-p "/clearsomething"))
  (should-not (claude-repl--skip-metaprompt-p "123abc")))

;;;; ---- Tests: prepare-input ----

(ert-deftest claude-repl-test-prepare-input-force-metaprompt ()
  "Force-metaprompt should prepend regardless of counter."
  (claude-repl-test--with-clean-state
    (let ((claude-repl-skip-permissions t)
          (claude-repl-prefix-period 3)
          (claude-repl-command-prefix "TEST")
          (claude-repl--command-prefix "PREFIX: "))
      ;; Counter 1 normally would not prepend with period 3
      (claude-repl--ws-put "ws1" :prefix-counter 1)
      (should (string-prefix-p "PREFIX: " (claude-repl--prepare-input "ws1" "hello" t))))))

(ert-deftest claude-repl-test-prepare-input-nil-counter ()
  "When counter is nil (fresh workspace), should treat as 0."
  (claude-repl-test--with-clean-state
    (let ((claude-repl-skip-permissions t)
          (claude-repl-prefix-period 3)
          (claude-repl-command-prefix "TEST")
          (claude-repl--command-prefix "PREFIX: "))
      ;; No :prefix-counter set -> defaults to 0 -> 0 mod 3 = 0 -> prepend
      (should (string-prefix-p "PREFIX: " (claude-repl--prepare-input "ws1" "hello"))))))

(ert-deftest claude-repl-test-prepare-input-exempt-input ()
  "Exempt inputs should not get the prefix even when counter is aligned."
  (claude-repl-test--with-clean-state
    (let ((claude-repl-skip-permissions t)
          (claude-repl-prefix-period 1)
          (claude-repl-command-prefix "TEST")
          (claude-repl--command-prefix "PREFIX: "))
      (claude-repl--ws-put "ws1" :prefix-counter 0)
      (should (equal (claude-repl--prepare-input "ws1" "/clear") "/clear"))
      (should (equal (claude-repl--prepare-input "ws1" "42") "42")))))

;;;; ---- Tests: increment-prefix-counter ----

(ert-deftest claude-repl-test-increment-prefix-counter-from-nil ()
  "Incrementing from nil should yield 1."
  (claude-repl-test--with-clean-state
    (claude-repl--increment-prefix-counter "ws1")
    (should (= (claude-repl--ws-get "ws1" :prefix-counter) 1))))

(ert-deftest claude-repl-test-increment-prefix-counter-from-existing ()
  "Incrementing from existing value should add 1."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :prefix-counter 10)
    (claude-repl--increment-prefix-counter "ws1")
    (should (= (claude-repl--ws-get "ws1" :prefix-counter) 11))))

;;;; ---- Tests: pin-owning-workspace ----

(ert-deftest claude-repl-test-pin-owning-workspace-sets-local ()
  "Pin should set `claude-repl--owning-workspace' as a buffer-local."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-buffer "*claude-pin-test*"
      (claude-repl--pin-owning-workspace (current-buffer) "my-ws")
      (should (equal claude-repl--owning-workspace "my-ws")))))

(ert-deftest claude-repl-test-pin-owning-workspace-nil-buf ()
  "Pinning with nil buffer should be a no-op (not error)."
  (claude-repl-test--with-clean-state
    (claude-repl--pin-owning-workspace nil "my-ws")))

;;;; ---- Tests: read-input-buffer ----

(ert-deftest claude-repl-test-read-input-buffer-returns-contents ()
  "Should return the buffer contents for a live input buffer."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-buffer " *test-read-input*"
      (insert "hello world")
      (claude-repl--ws-put "ws1" :input-buffer (current-buffer))
      (should (equal (claude-repl--read-input-buffer "ws1") "hello world")))))

(ert-deftest claude-repl-test-read-input-buffer-nil-when-no-buffer ()
  "Should return nil when no input buffer is registered."
  (claude-repl-test--with-clean-state
    (should-not (claude-repl--read-input-buffer "ws1"))))

(ert-deftest claude-repl-test-read-input-buffer-nil-when-dead ()
  "Should return nil when the input buffer has been killed."
  (claude-repl-test--with-clean-state
    (let ((buf (get-buffer-create " *test-read-dead*")))
      (claude-repl--ws-put "ws1" :input-buffer buf)
      (kill-buffer buf)
      (should-not (claude-repl--read-input-buffer "ws1")))))

;;;; ---- Tests: append-to-input-buffer ----

(ert-deftest claude-repl-test-append-to-input-buffer ()
  "Should append text to the end of the input buffer."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-buffer " *test-append*"
      (insert "start")
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1")))
        (claude-repl--ws-put "ws1" :input-buffer (current-buffer))
        (claude-repl--append-to-input-buffer " end")
        (should (equal (buffer-string) "start end"))))))

(ert-deftest claude-repl-test-append-to-input-buffer-no-buffer ()
  "Should be a no-op when no input buffer is registered."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1")))
      ;; Should not error
      (claude-repl--append-to-input-buffer "text"))))

;;;; ---- Tests: send-input-to-vterm routing ----

(ert-deftest claude-repl-test-send-input-to-vterm-exact-threshold ()
  "Input at exactly the threshold (200 chars) should use direct mode."
  (claude-repl-test--with-clean-state
    (let ((send-string-args nil)
          (return-called nil))
      (claude-repl-test--with-temp-buffer "*claude-threshold*"
        (cl-letf (((symbol-function 'vterm-send-string)
                   (lambda (s &rest args) (setq send-string-args (cons s args))))
                  ((symbol-function 'vterm-send-return)
                   (lambda () (setq return-called t)))
                  ((symbol-function 'claude-repl--refresh-vterm) #'ignore))
          (let ((exact-input (make-string 200 ?x)))
            (claude-repl--send-input-to-vterm (current-buffer) exact-input)
            ;; Exactly 200 -> direct mode (no paste flag)
            (should (null (cdr send-string-args)))
            (should return-called)))))))

(ert-deftest claude-repl-test-send-input-empty-string ()
  "Empty string should use direct mode."
  (claude-repl-test--with-clean-state
    (let ((send-string-args nil)
          (return-called nil))
      (claude-repl-test--with-temp-buffer "*claude-empty*"
        (cl-letf (((symbol-function 'vterm-send-string)
                   (lambda (s &rest args) (setq send-string-args (cons s args))))
                  ((symbol-function 'vterm-send-return)
                   (lambda () (setq return-called t)))
                  ((symbol-function 'claude-repl--refresh-vterm) #'ignore))
          (claude-repl--send-input-to-vterm (current-buffer) "")
          (should (equal (car send-string-args) ""))
          (should return-called))))))

;;;; ---- Tests: commit-input-buffer ----

(ert-deftest claude-repl-test-commit-input-buffer-no-clear ()
  "Without clear-p, buffer should not be erased."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-buffer " *test-commit-noclear*"
      (setq-local claude-repl--input-history nil)
      (setq-local claude-repl--history-index 0)
      (setq-local claude-repl--history-navigating nil)
      (insert "keep me")
      (cl-letf (((symbol-function 'claude-repl--history-save) #'ignore))
        (claude-repl--commit-input-buffer "ws1" (current-buffer) "keep me" nil))
      (should (equal claude-repl--input-history '("keep me")))
      (should (equal (buffer-string) "keep me")))))

(ert-deftest claude-repl-test-commit-input-buffer-with-clear ()
  "With clear-p, buffer should be erased after saving history."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-buffer " *test-commit-clear*"
      (setq-local claude-repl--input-history nil)
      (setq-local claude-repl--history-index 0)
      (setq-local claude-repl--history-navigating nil)
      (insert "clear me")
      (cl-letf (((symbol-function 'claude-repl--history-save) #'ignore))
        (claude-repl--commit-input-buffer "ws1" (current-buffer) "clear me" t))
      (should (equal claude-repl--input-history '("clear me")))
      (should (equal (buffer-string) "")))))

(ert-deftest claude-repl-test-commit-input-buffer-nil-buffer ()
  "Should be a no-op for nil input buffer."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'claude-repl--history-save) #'ignore))
      ;; Should not error
      (claude-repl--commit-input-buffer "ws1" nil "text" t))))

(ert-deftest claude-repl-test-commit-input-buffer-dead-buffer ()
  "Should be a no-op for killed input buffer."
  (claude-repl-test--with-clean-state
    (let ((buf (get-buffer-create " *test-commit-dead*")))
      (kill-buffer buf)
      (cl-letf (((symbol-function 'claude-repl--history-save) #'ignore))
        ;; Should not error
        (claude-repl--commit-input-buffer "ws1" buf "text" t)))))

;;;; ---- Tests: posthooks ----

(ert-deftest claude-repl-test-posthook-reset-prefix-counter ()
  "`/clear' posthook resets the prefix counter to 1."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :prefix-counter 42)
    (claude-repl--posthook-reset-prefix-counter "ws1" "/clear")
    (should (= (claude-repl--ws-get "ws1" :prefix-counter) 1))))

(ert-deftest claude-repl-test-run-send-posthooks-matches-clear ()
  "`claude-repl--run-send-posthooks' fires the /clear hook."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :prefix-counter 42)
    (claude-repl--run-send-posthooks "ws1" "/clear")
    (should (= (claude-repl--ws-get "ws1" :prefix-counter) 1))))

(ert-deftest claude-repl-test-run-send-posthooks-no-match ()
  "Posthooks should not fire for non-matching input."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :prefix-counter 42)
    (claude-repl--run-send-posthooks "ws1" "hello")
    (should (= (claude-repl--ws-get "ws1" :prefix-counter) 42))))

(ert-deftest claude-repl-test-run-send-posthooks-trailing-whitespace ()
  "Posthook matching should trim trailing whitespace."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :prefix-counter 42)
    (claude-repl--run-send-posthooks "ws1" "/clear  ")
    (should (= (claude-repl--ws-get "ws1" :prefix-counter) 1))))

;;;; ---- Tests: do-send ----

(ert-deftest claude-repl-test-do-send-increments-counter ()
  "`claude-repl--do-send' increments the prefix counter."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-buffer "*claude-do-send*"
      (claude-repl--ws-put "ws1" :vterm-buffer (current-buffer))
      (claude-repl--ws-put "ws1" :prefix-counter 5)
      (cl-letf (((symbol-function 'claude-repl--send-input-to-vterm) #'ignore)
                ((symbol-function 'claude-repl--run-send-posthooks) #'ignore))
        (claude-repl--do-send "ws1" "input" "raw"))
      (should (= (claude-repl--ws-get "ws1" :prefix-counter) 6)))))

(ert-deftest claude-repl-test-do-send-sets-thinking-state ()
  "`claude-repl--do-send' marks workspace as thinking."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-buffer "*claude-do-send-think*"
      (claude-repl--ws-put "ws1" :vterm-buffer (current-buffer))
      (cl-letf (((symbol-function 'claude-repl--send-input-to-vterm) #'ignore)
                ((symbol-function 'claude-repl--run-send-posthooks) #'ignore))
        (claude-repl--do-send "ws1" "input" "raw"))
      (should (eq (claude-repl--ws-state "ws1") :thinking)))))

(ert-deftest claude-repl-test-do-send-pins-owning-workspace ()
  "`claude-repl--do-send' pins the owning workspace on the vterm buffer."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-buffer "*claude-do-send-pin*"
      (claude-repl--ws-put "ws1" :vterm-buffer (current-buffer))
      (cl-letf (((symbol-function 'claude-repl--send-input-to-vterm) #'ignore)
                ((symbol-function 'claude-repl--run-send-posthooks) #'ignore))
        (claude-repl--do-send "ws1" "input" "raw"))
      (should (equal claude-repl--owning-workspace "ws1")))))

(ert-deftest claude-repl-test-do-send-runs-posthooks ()
  "`claude-repl--do-send' passes raw input to posthooks."
  (claude-repl-test--with-clean-state
    (let ((posthook-args nil))
      (claude-repl-test--with-temp-buffer "*claude-do-send-hook*"
        (claude-repl--ws-put "ws1" :vterm-buffer (current-buffer))
        (cl-letf (((symbol-function 'claude-repl--send-input-to-vterm) #'ignore)
                  ((symbol-function 'claude-repl--run-send-posthooks)
                   (lambda (ws raw) (setq posthook-args (list ws raw)))))
          (claude-repl--do-send "ws1" "decorated-input" "raw-input"))
        (should (equal posthook-args '("ws1" "raw-input")))))))

;;;; ---- Tests: discard-or-send-interrupt ----

(ert-deftest claude-repl-test-discard-or-send-interrupt-empty-sends-ctrl-c ()
  "When input buffer is empty, send C-c to vterm."
  (claude-repl-test--with-clean-state
    (let ((sent-key nil))
      (claude-repl-test--with-temp-buffer "*claude-discard-test*"
        (claude-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
        (claude-repl-test--with-temp-buffer " *test-input-discard*"
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                    ((symbol-function 'claude-repl--vterm-live-p) (lambda () t))
                    ((symbol-function 'vterm-send-key)
                     (lambda (key &rest args) (setq sent-key (cons key args)))))
            (claude-repl-discard-or-send-interrupt)
            (should (equal (car sent-key) "c"))))))))

(ert-deftest claude-repl-test-discard-or-send-interrupt-nonempty-discards ()
  "When input buffer has text, discard the input."
  (claude-repl-test--with-temp-buffer " *test-input-nonempty*"
    (setq-local claude-repl--input-history nil)
    (setq-local claude-repl--history-index 0)
    (setq-local claude-repl--history-navigating nil)
    (insert "some text")
    (let ((evil-called nil))
      (cl-letf (((symbol-function 'evil-insert-state)
                 (lambda () (setq evil-called t)))
                ((symbol-function 'claude-repl--history-save) #'ignore))
        (claude-repl-discard-or-send-interrupt)
        ;; Should have discarded (cleared buffer)
        (should (equal (buffer-string) ""))
        (should evil-called)))))

;;;; ---- Tests: send-vterm-key ----

(ert-deftest claude-repl-test-send-vterm-key-forwards-key ()
  "`claude-repl--send-vterm-key' forwards the given key to vterm."
  (claude-repl-test--with-clean-state
    (let ((sent-key nil))
      (claude-repl-test--with-temp-buffer "*claude-key-test*"
        (claude-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                  ((symbol-function 'claude-repl--vterm-live-p) (lambda () t))
                  ((symbol-function 'vterm-send-key)
                   (lambda (key &rest _) (setq sent-key key))))
          (claude-repl--send-vterm-key "<up>")
          (should (equal sent-key "<up>")))))))

(ert-deftest claude-repl-test-send-up-arrow ()
  "`claude-repl--send-up-arrow' sends <up> to vterm."
  (claude-repl-test--with-clean-state
    (let ((sent-key nil))
      (claude-repl-test--with-temp-buffer "*claude-up-test*"
        (claude-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                  ((symbol-function 'claude-repl--vterm-live-p) (lambda () t))
                  ((symbol-function 'vterm-send-key)
                   (lambda (key &rest _) (setq sent-key key))))
          (claude-repl--send-up-arrow)
          (should (equal sent-key "<up>")))))))

(ert-deftest claude-repl-test-send-down-arrow ()
  "`claude-repl--send-down-arrow' sends <down> to vterm."
  (claude-repl-test--with-clean-state
    (let ((sent-key nil))
      (claude-repl-test--with-temp-buffer "*claude-down-test*"
        (claude-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                  ((symbol-function 'claude-repl--vterm-live-p) (lambda () t))
                  ((symbol-function 'vterm-send-key)
                   (lambda (key &rest _) (setq sent-key key))))
          (claude-repl--send-down-arrow)
          (should (equal sent-key "<down>")))))))

;;;; ---- Tests: send (integration) ----

(ert-deftest claude-repl-test-send-no-workspace-errors ()
  "`claude-repl--send' should error when no workspace is available."
  (cl-letf (((symbol-function '+workspace-current-name) (lambda () nil)))
    (should-error (claude-repl--send) :type 'error)))

(ert-deftest claude-repl-test-send-reads-from-input-buffer ()
  "`claude-repl--send' reads from the input buffer when no prompt is given."
  (claude-repl-test--with-clean-state
    (let ((sent-input nil))
      (claude-repl-test--with-temp-buffer " *test-send-input*"
        (setq-local claude-repl--input-history nil)
        (setq-local claude-repl--history-index 0)
        (setq-local claude-repl--history-navigating nil)
        (insert "from buffer")
        (claude-repl--ws-put "ws1" :input-buffer (current-buffer))
        (claude-repl-test--with-temp-buffer "*claude-send-vterm*"
          (claude-repl--ws-put "ws1" :vterm-buffer (current-buffer))
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
                    ((symbol-function 'claude-repl--send-input-to-vterm)
                     (lambda (_buf input) (setq sent-input input)))
                    ((symbol-function 'claude-repl--history-save) #'ignore))
            (claude-repl--send nil "ws1")
            (should (stringp sent-input))
            ;; The input buffer should be cleared
            (should (equal (with-current-buffer (claude-repl--ws-get "ws1" :input-buffer)
                             (buffer-string))
                           ""))))))))

(ert-deftest claude-repl-test-send-with-explicit-prompt ()
  "`claude-repl--send' uses the given prompt and does not clear input buffer."
  (claude-repl-test--with-clean-state
    (let ((sent-input nil))
      (claude-repl-test--with-temp-buffer " *test-send-prompt-input*"
        (setq-local claude-repl--input-history nil)
        (setq-local claude-repl--history-index 0)
        (setq-local claude-repl--history-navigating nil)
        (insert "original content")
        (claude-repl--ws-put "ws1" :input-buffer (current-buffer))
        (claude-repl-test--with-temp-buffer "*claude-send-prompt-vterm*"
          (claude-repl--ws-put "ws1" :vterm-buffer (current-buffer))
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
                    ((symbol-function 'claude-repl--send-input-to-vterm)
                     (lambda (_buf input) (setq sent-input input)))
                    ((symbol-function 'claude-repl--history-save) #'ignore))
            (claude-repl--send "explicit prompt" "ws1")
            (should (stringp sent-input))
            ;; Input buffer should NOT be cleared when prompt is given
            (should (equal (with-current-buffer (claude-repl--ws-get "ws1" :input-buffer)
                             (buffer-string))
                           "original content"))))))))

(ert-deftest claude-repl-test-send-noop-when-nil-raw ()
  "`claude-repl--send' is a no-op when both prompt and input buffer are nil/empty."
  (claude-repl-test--with-clean-state
    (let ((do-send-called nil))
      (claude-repl-test--with-temp-buffer "*claude-noop-vterm*"
        (claude-repl--ws-put "ws1" :vterm-buffer (current-buffer))
        ;; No input buffer registered
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
                  ((symbol-function 'claude-repl--do-send)
                   (lambda (&rest _) (setq do-send-called t))))
          (claude-repl--send nil "ws1")
          (should-not do-send-called))))))

;;;; ---- Tests: bracketed paste pipeline ----

(ert-deftest claude-repl-test-bracketed-paste-threshold-constant ()
  "The bracketed paste threshold should be 200."
  (should (= claude-repl--bracketed-paste-threshold 200)))

(ert-deftest claude-repl-test-send-input-direct-calls-send-return ()
  "`claude-repl--send-input-direct' sends string then return then refreshes."
  (claude-repl-test--with-clean-state
    (let ((calls nil))
      (claude-repl-test--with-temp-buffer "*claude-direct-test*"
        (cl-letf (((symbol-function 'vterm-send-string)
                   (lambda (s &rest _) (push (list 'string s) calls)))
                  ((symbol-function 'vterm-send-return)
                   (lambda () (push '(return) calls)))
                  ((symbol-function 'claude-repl--refresh-vterm)
                   (lambda () (push '(refresh) calls))))
          (claude-repl--send-input-direct (current-buffer) "hello")
          ;; Verify order: string, return, refresh
          (should (equal (reverse calls) '((string "hello") (return) (refresh)))))))))

(ert-deftest claude-repl-test-send-input-bracketed-uses-paste-flag ()
  "`claude-repl--send-input-bracketed' calls vterm-send-string with t paste flag."
  (claude-repl-test--with-clean-state
    (let ((send-args nil))
      (claude-repl-test--with-temp-buffer "*claude-bracketed-test*"
        (cl-letf (((symbol-function 'vterm-send-string)
                   (lambda (s &rest args) (setq send-args (cons s args))))
                  ((symbol-function 'run-at-time)
                   (lambda (&rest _) nil)))
          (claude-repl--send-input-bracketed (current-buffer) "big input")
          (should (equal (car send-args) "big input"))
          (should (equal (cadr send-args) t)))))))

;;;; ---- Tests: slash mode ----

(ert-deftest claude-repl-test-slash-send-and-push ()
  "Slash send-and-push should forward char and push to stack."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-buffer " *test-slash-push*"
      (setq-local claude-repl--slash-stack nil)
      (let ((sent nil))
        (claude-repl-test--with-temp-buffer "*claude-slash-vterm*"
          (claude-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                    ((symbol-function 'claude-repl--vterm-live-p) (lambda () t))
                    ((symbol-function 'vterm-send-string)
                     (lambda (s &rest _) (push s sent))))
            (with-current-buffer " *test-slash-push*"
              (claude-repl--slash-send-and-push "a")
              (should (equal claude-repl--slash-stack '("a")))
              (claude-repl--slash-send-and-push "b")
              (should (equal claude-repl--slash-stack '("b" "a")))
              (should (equal (reverse sent) '("a" "b"))))))))))

(ert-deftest claude-repl-test-slash-backspace-pops-stack ()
  "Slash backspace should pop from stack and send backspace to vterm."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-buffer " *test-slash-bs*"
      (setq-local claude-repl--slash-stack '("b" "a" "/"))
      (let ((backspace-count 0))
        (claude-repl-test--with-temp-buffer "*claude-slash-bs-vterm*"
          (claude-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                    ((symbol-function 'claude-repl--vterm-live-p) (lambda () t))
                    ((symbol-function 'vterm-send-key)
                     (lambda (key &rest _)
                       (when (equal key "<backspace>")
                         (cl-incf backspace-count)))))
            (with-current-buffer " *test-slash-bs*"
              (claude-repl--slash-backspace)
              (should (equal claude-repl--slash-stack '("a" "/")))
              (should (= backspace-count 1)))))))))

(ert-deftest claude-repl-test-slash-backspace-exits-mode-when-empty ()
  "Slash backspace should exit slash mode when stack becomes empty."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-buffer " *test-slash-exit*"
      (setq-local claude-repl--slash-stack '("/"))
      (claude-slash-input-mode 1)
      (claude-repl-test--with-temp-buffer "*claude-slash-exit-vterm*"
        (claude-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                  ((symbol-function 'claude-repl--vterm-live-p) (lambda () t))
                  ((symbol-function 'vterm-send-key) #'ignore))
          (with-current-buffer " *test-slash-exit*"
            (claude-repl--slash-backspace)
            (should (null claude-repl--slash-stack))
            (should-not claude-slash-input-mode)))))))

(ert-deftest claude-repl-test-slash-return-exits-mode ()
  "Slash return should send return to vterm and exit slash mode."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-buffer " *test-slash-return*"
      (setq-local claude-repl--slash-stack '("r" "a" "e" "l" "c" "/"))
      (claude-slash-input-mode 1)
      (let ((return-called nil))
        (claude-repl-test--with-temp-buffer "*claude-slash-return-vterm*"
          (claude-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                    ((symbol-function 'claude-repl--vterm-live-p) (lambda () t))
                    ((symbol-function 'vterm-send-return)
                     (lambda () (setq return-called t))))
            (with-current-buffer " *test-slash-return*"
              (claude-repl--slash-return)
              (should return-called)
              (should (null claude-repl--slash-stack))
              (should-not claude-slash-input-mode))))))))

(ert-deftest claude-repl-test-exit-slash-mode-clears-state ()
  "`claude-repl--exit-slash-mode' clears stack and disables the minor mode."
  (claude-repl-test--with-temp-buffer " *test-exit-slash*"
    (setq-local claude-repl--slash-stack '("a" "b"))
    (claude-slash-input-mode 1)
    (claude-repl--exit-slash-mode)
    (should (null claude-repl--slash-stack))
    (should-not claude-slash-input-mode)))

(ert-deftest claude-repl-test-slash-tab-sends-tab ()
  "`claude-repl--slash-tab' sends a tab character and pushes to stack."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-buffer " *test-slash-tab*"
      (setq-local claude-repl--slash-stack '("/"))
      (let ((sent nil))
        (claude-repl-test--with-temp-buffer "*claude-slash-tab-vterm*"
          (claude-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                    ((symbol-function 'claude-repl--vterm-live-p) (lambda () t))
                    ((symbol-function 'vterm-send-string)
                     (lambda (s &rest _) (setq sent s))))
            (with-current-buffer " *test-slash-tab*"
              (claude-repl--slash-tab)
              (should (equal sent "\t"))
              (should (equal claude-repl--slash-stack '("\t" "/"))))))))))

;;;; ---- Tests: passthrough-start ----

(ert-deftest claude-repl-test-passthrough-start-empty-buffer ()
  "In empty buffer, passthrough-start should enter slash mode and forward char."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-buffer " *test-passthrough-empty*"
      (setq-local claude-repl--slash-stack nil)
      (let ((sent nil))
        (claude-repl-test--with-temp-buffer "*claude-pt-vterm*"
          (claude-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                    ((symbol-function 'claude-repl--vterm-live-p) (lambda () t))
                    ((symbol-function 'vterm-send-string)
                     (lambda (s &rest _) (setq sent s))))
            (with-current-buffer " *test-passthrough-empty*"
              (claude-repl--passthrough-start "/")
              (should claude-slash-input-mode)
              (should (equal claude-repl--slash-stack '("/")))
              (should (equal sent "/")))))))))

(ert-deftest claude-repl-test-passthrough-start-nonempty-inserts ()
  "In non-empty buffer, passthrough-start should insert the char normally."
  (claude-repl-test--with-temp-buffer " *test-passthrough-nonempty*"
    (insert "existing")
    (cl-letf (((symbol-function 'self-insert-command)
               (lambda (_n &optional _ch) (insert "/"))))
      (claude-repl--passthrough-start "/")
      (should-not (bound-and-true-p claude-slash-input-mode))
      (should (string-match-p "/" (buffer-string))))))

;;;; ---- Tests: deferred action helpers ----

(ert-deftest claude-repl-test-run-deferred-action-live-buffer ()
  "Deferred action should run when buffer is alive."
  (claude-repl-test--with-temp-buffer "*test-deferred*"
    (let ((ran nil))
      (claude-repl--run-deferred-action (current-buffer) (lambda () (setq ran t)))
      (should ran))))

(ert-deftest claude-repl-test-run-deferred-action-dead-buffer ()
  "Deferred action should NOT run when buffer is dead."
  (let ((ran nil)
        (buf (get-buffer-create "*test-deferred-dead*")))
    (kill-buffer buf)
    (claude-repl--run-deferred-action buf (lambda () (setq ran t)))
    (should-not ran)))

(ert-deftest claude-repl-test-vterm-deferred-action-calls-run-at-time ()
  "Should schedule action via run-at-time."
  (let ((timer-args nil))
    (claude-repl-test--with-temp-buffer "*test-deferred-timer*"
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (&rest args) (setq timer-args args))))
        (claude-repl--vterm-deferred-action (current-buffer) 0.5 #'ignore)
        (should timer-args)
        (should (= (car timer-args) 0.5))))))

;;;; ---- Tests: backspace intercept ----

(ert-deftest claude-repl-test-slash-intercept-backspace-in-slash-mode ()
  "In slash mode, intercept should redirect to slash-backspace."
  (claude-repl-test--with-temp-buffer " *test-intercept-slash*"
    (setq-local claude-slash-input-mode t)
    (let ((this-command 'evil-delete-backward-char-and-join))
      (claude-repl--slash-intercept-backspace)
      (should (eq this-command #'claude-repl--slash-backspace)))))

(ert-deftest claude-repl-test-slash-intercept-backspace-empty-no-slash ()
  "Outside slash mode with empty buffer, should forward backspace to vterm."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-buffer " *test-intercept-empty*"
      (setq-local claude-slash-input-mode nil)
      (let ((this-command 'delete-backward-char)
            (sent-key nil))
        (claude-repl-test--with-temp-buffer "*claude-intercept-vterm*"
          (claude-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                    ((symbol-function 'claude-repl--vterm-live-p) (lambda () t))
                    ((symbol-function 'vterm-send-key)
                     (lambda (key &rest _) (setq sent-key key))))
            (with-current-buffer " *test-intercept-empty*"
              (claude-repl--slash-intercept-backspace)
              (should (equal sent-key "<backspace>")))))))))

(ert-deftest claude-repl-test-slash-intercept-backspace-nonempty-noop ()
  "Outside slash mode with non-empty buffer, backspace intercept should be a no-op."
  (claude-repl-test--with-temp-buffer " *test-intercept-nonempty*"
    (setq-local claude-slash-input-mode nil)
    (insert "text")
    (let ((this-command 'delete-backward-char))
      (claude-repl--slash-intercept-backspace)
      ;; Command should be unchanged
      (should (eq this-command 'delete-backward-char)))))

(ert-deftest claude-repl-test-slash-intercept-ignores-non-backspace ()
  "Intercept should ignore commands not in the backspace list."
  (claude-repl-test--with-temp-buffer " *test-intercept-other*"
    (setq-local claude-slash-input-mode t)
    (let ((this-command 'self-insert-command))
      (claude-repl--slash-intercept-backspace)
      ;; Should remain unchanged since self-insert-command is not in the list
      (should (eq this-command 'self-insert-command)))))

;;;; ---- Tests: edge cases (new coverage) ----

;;; Backspace: leading whitespace in buffer (buffer-size > 0 but looks blank)

(ert-deftest claude-repl-test-slash-intercept-backspace-whitespace-only ()
  "Whitespace-only buffer has buffer-size > 0, so backspace should NOT forward to vterm."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-buffer " *test-intercept-ws*"
      (setq-local claude-slash-input-mode nil)
      (insert "   ")
      (let ((this-command 'delete-backward-char)
            (sent-key nil))
        (claude-repl-test--with-temp-buffer "*claude-intercept-ws-vterm*"
          (claude-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                    ((symbol-function 'claude-repl--vterm-live-p) (lambda () t))
                    ((symbol-function 'vterm-send-key)
                     (lambda (key &rest _) (setq sent-key key))))
            (with-current-buffer " *test-intercept-ws*"
              (claude-repl--slash-intercept-backspace)
              ;; buffer-size > 0, so no forwarding
              (should-not sent-key)
              ;; Command should remain unchanged
              (should (eq this-command 'delete-backward-char)))))))))

;;; claude-input-mode: mode setup

(ert-deftest claude-repl-test-claude-input-mode-setup ()
  "`claude-input-mode' sets header-line, visual-line-mode, and installs hooks."
  (claude-repl-test--with-temp-buffer " *test-input-mode*"
    (cl-letf (((symbol-function 'claude-repl--set-buffer-background) #'ignore))
      (claude-input-mode))
    ;; Header line should be set
    (should (stringp header-line-format))
    (should (string-match-p "RET" header-line-format))
    ;; Visual line mode should be enabled
    (should visual-line-mode)
    ;; pre-command-hook should include slash-intercept-backspace
    (should (memq #'claude-repl--slash-intercept-backspace pre-command-hook))
    ;; after-change-functions should include history-on-change
    (should (memq #'claude-repl--history-on-change after-change-functions))))

;;; discard-input with active slash mode

(ert-deftest claude-repl-test-discard-input-exits-slash-mode ()
  "`claude-repl-discard-input' should exit slash mode when it is active."
  (claude-repl-test--with-temp-buffer " *test-discard-slash*"
    (setq-local claude-repl--input-history nil)
    (setq-local claude-repl--history-index 0)
    (setq-local claude-repl--history-navigating nil)
    (setq-local claude-repl--slash-stack '("c" "l" "/"))
    (claude-slash-input-mode 1)
    (insert "text")
    (cl-letf (((symbol-function 'evil-insert-state) #'ignore)
              ((symbol-function 'claude-repl--history-save) #'ignore))
      (claude-repl-discard-input)
      ;; Slash mode should be exited
      (should-not claude-slash-input-mode)
      (should (null claude-repl--slash-stack))
      ;; Buffer should be cleared
      (should (equal (buffer-string) "")))))

;;; discard-input with empty buffer

(ert-deftest claude-repl-test-discard-input-empty-buffer ()
  "`claude-repl-discard-input' on an empty buffer should push empty string to history and remain empty."
  (claude-repl-test--with-temp-buffer " *test-discard-empty*"
    (setq-local claude-repl--input-history nil)
    (setq-local claude-repl--history-index 0)
    (setq-local claude-repl--history-navigating nil)
    (let ((evil-called nil))
      (cl-letf (((symbol-function 'evil-insert-state)
                 (lambda () (setq evil-called t)))
                ((symbol-function 'claude-repl--history-save) #'ignore))
        (claude-repl-discard-input)
        (should (equal (buffer-string) ""))
        (should evil-called)))))

;;; discard-or-send-interrupt with whitespace-only buffer

(ert-deftest claude-repl-test-discard-or-send-interrupt-whitespace-sends-ctrl-c ()
  "Whitespace-only buffer is blank-p, so should send C-c to vterm."
  (claude-repl-test--with-clean-state
    (let ((sent-key nil))
      (claude-repl-test--with-temp-buffer "*claude-discard-ws-vterm*"
        (claude-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
        (claude-repl-test--with-temp-buffer " *test-discard-ws-input*"
          (insert "   \n\t  ")
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                    ((symbol-function 'claude-repl--vterm-live-p) (lambda () t))
                    ((symbol-function 'vterm-send-key)
                     (lambda (key &rest args) (setq sent-key (cons key args)))))
            (claude-repl-discard-or-send-interrupt)
            ;; string-blank-p returns t for whitespace, so C-c is sent
            (should (equal (car sent-key) "c"))))))))

;;; send-vterm-key with dead vterm buffer

(ert-deftest claude-repl-test-send-vterm-key-dead-vterm-noop ()
  "`claude-repl--send-vterm-key' should be a no-op when vterm buffer is dead."
  (claude-repl-test--with-clean-state
    (let ((buf (get-buffer-create "*claude-dead-key*"))
          (sent nil))
      (claude-repl--ws-put "test-ws" :vterm-buffer buf)
      (kill-buffer buf)
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'vterm-send-key)
                 (lambda (&rest _) (setq sent t))))
        (claude-repl--send-vterm-key "<up>")
        (should-not sent)))))

;;; scroll-down with dead vterm buffer

(ert-deftest claude-repl-test-scroll-down-dead-vterm-noop ()
  "`claude-repl-scroll-down' should be a no-op when vterm buffer is dead."
  (claude-repl-test--with-clean-state
    (let ((buf (get-buffer-create "*claude-dead-scroll-down*"))
          (called nil))
      (claude-repl--ws-put "test-ws" :vterm-buffer buf)
      (kill-buffer buf)
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'vterm-send-down)
                 (lambda () (setq called t))))
        (claude-repl-scroll-down)
        (should-not called)))))

;;; scroll-up with dead vterm buffer

(ert-deftest claude-repl-test-scroll-up-dead-vterm-noop ()
  "`claude-repl-scroll-up' should be a no-op when vterm buffer is dead."
  (claude-repl-test--with-clean-state
    (let ((buf (get-buffer-create "*claude-dead-scroll-up*"))
          (called nil))
      (claude-repl--ws-put "test-ws" :vterm-buffer buf)
      (kill-buffer buf)
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'vterm-send-up)
                 (lambda () (setq called t))))
        (claude-repl-scroll-up)
        (should-not called)))))

;;; scroll-vterm-output: scrolls by 3 lines with visible window

(ert-deftest claude-repl-test-scroll-vterm-output-scrolls-3-lines ()
  "`claude-repl--scroll-vterm-output' calls SCROLL-FN with 3 in the vterm window."
  (claude-repl-test--with-clean-state
    (let ((scroll-arg nil))
      (claude-repl-test--with-temp-buffer "*claude-scroll-output*"
        (claude-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                  ((symbol-function 'claude-repl--vterm-live-p) (lambda () t))
                  ((symbol-function 'get-buffer-window)
                   (lambda (&rest _) (selected-window)))
                  ((symbol-function 'with-selected-window)
                   (lambda (_win &rest _) nil)))
          ;; Call directly with a lambda that captures the arg
          (cl-letf (((symbol-function 'get-buffer-window)
                     (lambda (&rest _) (selected-window))))
            (claude-repl--scroll-vterm-output
             (lambda (n) (setq scroll-arg n)))
            (should (= scroll-arg 3))))))))

;;; scroll-vterm-output: no vterm window is a no-op

(ert-deftest claude-repl-test-scroll-vterm-output-no-window-noop ()
  "`claude-repl--scroll-vterm-output' is a no-op when vterm has no visible window."
  (claude-repl-test--with-clean-state
    (let ((scroll-called nil))
      (claude-repl-test--with-temp-buffer "*claude-scroll-nowin*"
        (claude-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                  ((symbol-function 'claude-repl--vterm-live-p) (lambda () t))
                  ((symbol-function 'get-buffer-window)
                   (lambda (&rest _) nil)))
          (claude-repl--scroll-vterm-output
           (lambda (_n) (setq scroll-called t)))
          (should-not scroll-called))))))

;;; scroll-output-up calls scroll-down (counterintuitive naming)

(ert-deftest claude-repl-test-scroll-output-up-calls-scroll-down ()
  "`claude-repl-scroll-output-up' calls `scroll-down' (not `scroll-up')."
  (claude-repl-test--with-clean-state
    (let ((fn-called nil))
      (claude-repl-test--with-temp-buffer "*claude-scroll-up-test*"
        (claude-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                  ((symbol-function 'claude-repl--vterm-live-p) (lambda () t))
                  ((symbol-function 'get-buffer-window)
                   (lambda (&rest _) (selected-window)))
                  ((symbol-function 'scroll-down)
                   (lambda (n) (setq fn-called (list 'scroll-down n))))
                  ((symbol-function 'scroll-up)
                   (lambda (_n) (setq fn-called 'wrong))))
          (claude-repl-scroll-output-up)
          (should (equal fn-called '(scroll-down 3))))))))

;;; scroll-output-down calls scroll-up

(ert-deftest claude-repl-test-scroll-output-down-calls-scroll-up ()
  "`claude-repl-scroll-output-down' calls `scroll-up' (not `scroll-down')."
  (claude-repl-test--with-clean-state
    (let ((fn-called nil))
      (claude-repl-test--with-temp-buffer "*claude-scroll-down-test*"
        (claude-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                  ((symbol-function 'claude-repl--vterm-live-p) (lambda () t))
                  ((symbol-function 'get-buffer-window)
                   (lambda (&rest _) (selected-window)))
                  ((symbol-function 'scroll-up)
                   (lambda (n) (setq fn-called (list 'scroll-up n))))
                  ((symbol-function 'scroll-down)
                   (lambda (_n) (setq fn-called 'wrong))))
          (claude-repl-scroll-output-down)
          (should (equal fn-called '(scroll-up 3))))))))

;;; send-char with dead vterm buffer

(ert-deftest claude-repl-test-send-char-dead-vterm-noop ()
  "`claude-repl-send-char' should be a no-op when vterm buffer is dead."
  (claude-repl-test--with-clean-state
    (let ((buf (get-buffer-create "*claude-dead-sendchar*"))
          (sent nil))
      (claude-repl--ws-put "test-ws" :vterm-buffer buf)
      (kill-buffer buf)
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'vterm-send-string)
                 (lambda (&rest _) (setq sent t))))
        (claude-repl-send-char "y")
        (should-not sent)))))

;;; send-y macro-generated command

(ert-deftest claude-repl-test-send-y-calls-send-char ()
  "`claude-repl--send-y' sends \"y\" via `claude-repl-send-char'."
  (claude-repl-test--with-clean-state
    (let ((char-sent nil))
      (cl-letf (((symbol-function 'claude-repl-send-char)
                 (lambda (c) (setq char-sent c))))
        (claude-repl--send-y)
        (should (equal char-sent "y"))))))

;;; send-n macro-generated command

(ert-deftest claude-repl-test-send-n-calls-send-char ()
  "`claude-repl--send-n' sends \"n\" via `claude-repl-send-char'."
  (claude-repl-test--with-clean-state
    (let ((char-sent nil))
      (cl-letf (((symbol-function 'claude-repl-send-char)
                 (lambda (c) (setq char-sent c))))
        (claude-repl--send-n)
        (should (equal char-sent "n"))))))

;;; define-send-char-command macro expansion correctness

(ert-deftest claude-repl-test-define-send-char-macro-creates-interactive-command ()
  "Macro `claude-repl--define-send-char-command' creates an interactive function."
  ;; claude-repl--send-y and claude-repl--send-n were defined via the macro
  (should (fboundp 'claude-repl--send-y))
  (should (commandp 'claude-repl--send-y))
  (should (fboundp 'claude-repl--send-n))
  (should (commandp 'claude-repl--send-n)))

;;; input-send-digit-char: extracts digit from last-command-event

(ert-deftest claude-repl-test-input-send-digit-char ()
  "`claude-repl--input-send-digit-char' extracts digit from `last-command-event' and sends it."
  (claude-repl-test--with-clean-state
    (let ((char-sent nil))
      (cl-letf (((symbol-function 'claude-repl-send-char)
                 (lambda (c) (setq char-sent c))))
        ;; Simulate C-S-3: event-basic-type strips modifiers, returning ?3
        (let ((last-command-event ?3))
          (claude-repl--input-send-digit-char)
          (should (equal char-sent "3")))))))

;;; insert-digit-or-passthrough: empty buffer -> passthrough

(ert-deftest claude-repl-test-insert-digit-or-passthrough-empty ()
  "In an empty buffer, digit key should enter passthrough mode."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-buffer " *test-digit-empty*"
      (setq-local claude-repl--slash-stack nil)
      (let ((sent nil))
        (claude-repl-test--with-temp-buffer "*claude-digit-vterm*"
          (claude-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                    ((symbol-function 'claude-repl--vterm-live-p) (lambda () t))
                    ((symbol-function 'vterm-send-string)
                     (lambda (s &rest _) (setq sent s))))
            (with-current-buffer " *test-digit-empty*"
              (let ((last-command-event ?5))
                (claude-repl--insert-digit-or-passthrough)
                ;; Should have entered slash mode
                (should claude-slash-input-mode)
                ;; Should have sent "5" to vterm
                (should (equal sent "5"))
                ;; Stack should contain "5"
                (should (equal claude-repl--slash-stack '("5")))))))))))

;;; insert-digit-or-passthrough: non-empty buffer -> self-insert

(ert-deftest claude-repl-test-insert-digit-or-passthrough-nonempty ()
  "In a non-empty buffer, digit key should self-insert."
  (claude-repl-test--with-temp-buffer " *test-digit-nonempty*"
    (insert "existing")
    (let ((inserted nil))
      (cl-letf (((symbol-function 'self-insert-command)
                 (lambda (_n &optional _ch) (setq inserted t))))
        (let ((last-command-event ?7))
          (claude-repl--insert-digit-or-passthrough)
          (should inserted)
          (should-not (bound-and-true-p claude-slash-input-mode)))))))

;;; skip-metaprompt-p: leading whitespace

(ert-deftest claude-repl-test-skip-metaprompt-leading-whitespace ()
  "`claude-repl--skip-metaprompt-p' does NOT strip leading whitespace, so \" /clear\" is not exempt."
  ;; string-trim-right only trims trailing whitespace
  (should-not (claude-repl--skip-metaprompt-p "  /clear"))
  (should-not (claude-repl--skip-metaprompt-p " 42")))

;;; skip-metaprompt-p: empty string

(ert-deftest claude-repl-test-skip-metaprompt-empty-string ()
  "`claude-repl--skip-metaprompt-p' returns nil for empty string."
  (should-not (claude-repl--skip-metaprompt-p "")))

;;; skip-metaprompt-p: mixed numeral+whitespace

(ert-deftest claude-repl-test-skip-metaprompt-mixed-numeral-whitespace ()
  "`claude-repl--skip-metaprompt-p' handles numerals with trailing whitespace but not mixed content."
  ;; Numerals with trailing whitespace -> trimmed to numerals -> should skip
  (should (claude-repl--skip-metaprompt-p "42  "))
  (should (claude-repl--skip-metaprompt-p "7\n"))
  ;; Numeral with leading text -> not bare numeral
  (should-not (claude-repl--skip-metaprompt-p "abc42"))
  ;; Numeral with trailing non-whitespace text
  (should-not (claude-repl--skip-metaprompt-p "42abc")))

;;; should-prepend-metaprompt-p: empty string command-prefix

(ert-deftest claude-repl-test-should-prepend-nil-when-empty-command-prefix ()
  "Empty string `claude-repl-command-prefix' is truthy but conceptually empty."
  ;; In Emacs, "" is truthy, so this will actually return t when conditions align.
  ;; This test documents that behavior.
  (let ((claude-repl-skip-permissions t)
        (claude-repl-command-prefix "")
        (claude-repl--command-prefix "PREFIX: ")
        (claude-repl-prefix-period 1))
    ;; "" is truthy in Emacs, so the function returns t
    (should (claude-repl--should-prepend-metaprompt-p "hello" 0))))

;;; prepare-input: empty raw input

(ert-deftest claude-repl-test-prepare-input-empty-raw ()
  "`claude-repl--prepare-input' with empty raw input: empty string is not exempt."
  (claude-repl-test--with-clean-state
    (let ((claude-repl-skip-permissions t)
          (claude-repl-prefix-period 1)
          (claude-repl-command-prefix "TEST")
          (claude-repl--command-prefix "PREFIX: "))
      (claude-repl--ws-put "ws1" :prefix-counter 0)
      ;; Empty string is not in exempt list and doesn't match numeral regex
      ;; So it gets the prefix prepended
      (should (equal (claude-repl--prepare-input "ws1" "") "PREFIX: ")))))

;;; run-send-posthooks: multiple hooks matching same input

(ert-deftest claude-repl-test-run-send-posthooks-multiple-matches ()
  "When multiple posthooks match the same input, all should fire."
  (claude-repl-test--with-clean-state
    (let* ((hook-a-called nil)
           (hook-b-called nil)
           (claude-repl-send-posthooks
            (list (cons "^/clear$" (lambda (_ws _raw) (setq hook-a-called t)))
                  (cons "clear"    (lambda (_ws _raw) (setq hook-b-called t))))))
      (claude-repl--run-send-posthooks "ws1" "/clear")
      (should hook-a-called)
      (should hook-b-called))))

;;; run-send-posthooks: empty input

(ert-deftest claude-repl-test-run-send-posthooks-empty-input ()
  "Empty input should not match /clear pattern."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :prefix-counter 42)
    (claude-repl--run-send-posthooks "ws1" "")
    ;; Counter should be unchanged -- no hooks matched
    (should (= (claude-repl--ws-get "ws1" :prefix-counter) 42))))

;;; do-send: dead vterm buffer

(ert-deftest claude-repl-test-do-send-dead-vterm ()
  "`claude-repl--do-send' should still increment counter etc. even with a dead vterm buffer."
  (claude-repl-test--with-clean-state
    (let ((buf (get-buffer-create "*claude-dead-do-send*")))
      (claude-repl--ws-put "ws1" :vterm-buffer buf)
      (claude-repl--ws-put "ws1" :prefix-counter 5)
      (kill-buffer buf)
      (cl-letf (((symbol-function 'claude-repl--send-input-to-vterm) #'ignore)
                ((symbol-function 'claude-repl--run-send-posthooks) #'ignore)
                ((symbol-function 'claude-repl--pin-owning-workspace) #'ignore))
        (claude-repl--do-send "ws1" "input" "raw"))
      ;; Counter should still be incremented
      (should (= (claude-repl--ws-get "ws1" :prefix-counter) 6)))))

;;; send: force-metaprompt path

(ert-deftest claude-repl-test-send-force-metaprompt-path ()
  "`claude-repl--send' with force-metaprompt passes force=t to prepare-input."
  (claude-repl-test--with-clean-state
    (let ((prepare-force nil))
      (claude-repl-test--with-temp-buffer " *test-send-force-input*"
        (setq-local claude-repl--input-history nil)
        (setq-local claude-repl--history-index 0)
        (setq-local claude-repl--history-navigating nil)
        (insert "hello")
        (claude-repl--ws-put "ws1" :input-buffer (current-buffer))
        (claude-repl-test--with-temp-buffer "*claude-send-force-vterm*"
          (claude-repl--ws-put "ws1" :vterm-buffer (current-buffer))
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
                    ((symbol-function 'claude-repl--prepare-input)
                     (lambda (_ws raw &optional force)
                       (setq prepare-force force)
                       raw))
                    ((symbol-function 'claude-repl--do-send) #'ignore)
                    ((symbol-function 'claude-repl--history-save) #'ignore))
            (claude-repl--send nil "ws1" t)
            (should prepare-force)))))))

;;; send: dead vterm buffer is a no-op

(ert-deftest claude-repl-test-send-dead-vterm-noop ()
  "`claude-repl--send' does not call do-send when vterm buffer is dead."
  (claude-repl-test--with-clean-state
    (let ((do-send-called nil)
          (buf (get-buffer-create "*claude-send-dead-vterm*")))
      (claude-repl--ws-put "ws1" :vterm-buffer buf)
      (claude-repl-test--with-temp-buffer " *test-send-dead-input*"
        (insert "hello")
        (claude-repl--ws-put "ws1" :input-buffer (current-buffer))
        (kill-buffer buf)
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
                  ((symbol-function 'claude-repl--do-send)
                   (lambda (&rest _) (setq do-send-called t))))
          (claude-repl--send nil "ws1")
          (should-not do-send-called))))))

;;; send-and-hide: calls send then hide-panels

(ert-deftest claude-repl-test-send-and-hide ()
  "`claude-repl-send-and-hide' calls `claude-repl--send' then `claude-repl--hide-panels'."
  (claude-repl-test--with-clean-state
    (let ((calls nil))
      (cl-letf (((symbol-function 'claude-repl--send)
                 (lambda (&rest _) (push 'send calls)))
                ((symbol-function 'claude-repl--hide-panels)
                 (lambda () (push 'hide calls))))
        (claude-repl-send-and-hide)
        (should (equal (reverse calls) '(send hide)))))))

;;; send-with-metaprompt: calls send with force=t

(ert-deftest claude-repl-test-send-with-metaprompt ()
  "`claude-repl-send-with-metaprompt' calls `claude-repl--send' with force-metaprompt=t."
  (claude-repl-test--with-clean-state
    (let ((send-args nil))
      (cl-letf (((symbol-function 'claude-repl--send)
                 (lambda (&optional prompt ws force)
                   (setq send-args (list prompt ws force)))))
        (claude-repl-send-with-metaprompt)
        (should (equal send-args '(nil nil t)))))))

;;; send-with-postfix: appends postfix then sends

(ert-deftest claude-repl-test-send-with-postfix ()
  "`claude-repl-send-with-postfix' appends the postfix, then calls send."
  (claude-repl-test--with-clean-state
    (let ((send-called nil)
          (claude-repl-send-postfix " POSTFIX"))
      (claude-repl-test--with-temp-buffer " *test-postfix-input*"
        (insert "hello")
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1")))
          (claude-repl--ws-put "ws1" :input-buffer (current-buffer))
          (cl-letf (((symbol-function 'claude-repl--send)
                     (lambda (&rest _) (setq send-called t))))
            (claude-repl-send-with-postfix)
            ;; Postfix should have been appended
            (should (equal (buffer-string) "hello POSTFIX"))
            ;; Send should have been called
            (should send-called)))))))

;;; vterm-deferred-action: dead buffer at schedule time

(ert-deftest claude-repl-test-vterm-deferred-action-dead-buffer-schedules ()
  "`claude-repl--vterm-deferred-action' still schedules the timer even with dead buffer.
The dead-buffer check happens inside `run-deferred-action' at callback time."
  (let ((timer-args nil)
        (buf (get-buffer-create "*test-deferred-dead-sched*")))
    (kill-buffer buf)
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (&rest args) (setq timer-args args))))
      (claude-repl--vterm-deferred-action buf 0.3 #'ignore)
      ;; run-at-time should still be called (guard is in the callback)
      (should timer-args)
      (should (= (car timer-args) 0.3)))))

;;; bracketed-finalize: sends return + refreshes vterm

(ert-deftest claude-repl-test-bracketed-finalize ()
  "`claude-repl--bracketed-finalize' sends return and refreshes."
  (let ((calls nil))
    (cl-letf (((symbol-function 'vterm-send-return)
               (lambda () (push 'return calls)))
              ((symbol-function 'claude-repl--refresh-vterm)
               (lambda () (push 'refresh calls))))
      (claude-repl--bracketed-finalize)
      (should (equal (reverse calls) '(return refresh))))))

;;; bracketed-send-return: sends return + schedules finalize

(ert-deftest claude-repl-test-bracketed-send-return ()
  "`claude-repl--bracketed-send-return' sends return and schedules finalize via deferred action."
  (let ((return-called nil)
        (deferred-args nil))
    (claude-repl-test--with-temp-buffer "*test-bracketed-return*"
      (cl-letf (((symbol-function 'vterm-send-return)
                 (lambda () (setq return-called t)))
                ((symbol-function 'claude-repl--vterm-deferred-action)
                 (lambda (&rest args) (setq deferred-args args))))
        (claude-repl--bracketed-send-return (current-buffer))
        (should return-called)
        ;; Deferred action should be scheduled with the buffer and 0.05 delay
        (should deferred-args)
        (should (eq (car deferred-args) (current-buffer)))
        (should (= (cadr deferred-args) 0.05))))))

;;; exit-slash-mode: already disabled (idempotent)

(ert-deftest claude-repl-test-exit-slash-mode-idempotent ()
  "`claude-repl--exit-slash-mode' is safe to call when already disabled."
  (claude-repl-test--with-temp-buffer " *test-exit-slash-idem*"
    (setq-local claude-repl--slash-stack nil)
    ;; Mode is already off
    (should-not claude-slash-input-mode)
    ;; Should not error
    (claude-repl--exit-slash-mode)
    (should (null claude-repl--slash-stack))
    (should-not claude-slash-input-mode)))

;;; slash-vterm-send: dead vterm is a no-op

(ert-deftest claude-repl-test-slash-vterm-send-dead-vterm-noop ()
  "`claude-repl--slash-vterm-send' is a no-op when vterm buffer is dead."
  (claude-repl-test--with-clean-state
    (let ((buf (get-buffer-create "*claude-dead-slash-vterm*"))
          (sent nil))
      (claude-repl--ws-put "test-ws" :vterm-buffer buf)
      (kill-buffer buf)
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'vterm-send-string)
                 (lambda (&rest _) (setq sent t))))
        (claude-repl--slash-vterm-send "a")
        (should-not sent)))))

;;; slash-forward-char: uses last-command-event

(ert-deftest claude-repl-test-slash-forward-char ()
  "`claude-repl--slash-forward-char' reads `last-command-event' and forwards that char."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-buffer " *test-slash-fwd-char*"
      (setq-local claude-repl--slash-stack '("/"))
      (let ((sent nil))
        (claude-repl-test--with-temp-buffer "*claude-fwd-char-vterm*"
          (claude-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                    ((symbol-function 'claude-repl--vterm-live-p) (lambda () t))
                    ((symbol-function 'vterm-send-string)
                     (lambda (s &rest _) (setq sent s))))
            (with-current-buffer " *test-slash-fwd-char*"
              (let ((last-command-event ?c))
                (claude-repl--slash-forward-char)
                (should (equal sent "c"))
                (should (equal claude-repl--slash-stack '("c" "/")))))))))))

;;; slash-backspace: already empty stack

(ert-deftest claude-repl-test-slash-backspace-empty-stack ()
  "`claude-repl--slash-backspace' with an already-empty stack exits mode and pops nil."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-buffer " *test-slash-bs-empty*"
      (setq-local claude-repl--slash-stack nil)
      (claude-slash-input-mode 1)
      (claude-repl-test--with-temp-buffer "*claude-slash-bs-empty-vterm*"
        (claude-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                  ((symbol-function 'claude-repl--vterm-live-p) (lambda () t))
                  ((symbol-function 'vterm-send-key) #'ignore))
          (with-current-buffer " *test-slash-bs-empty*"
            (claude-repl--slash-backspace)
            ;; Stack was already empty; pop returns nil; mode exits
            (should (null claude-repl--slash-stack))
            (should-not claude-slash-input-mode)))))))

;;; slash-start: thin wrapper over passthrough-start with "/"

(ert-deftest claude-repl-test-slash-start-delegates-to-passthrough ()
  "`claude-repl--slash-start' calls `claude-repl--passthrough-start' with \"/\"."
  (let ((passthrough-arg nil))
    (cl-letf (((symbol-function 'claude-repl--passthrough-start)
               (lambda (char) (setq passthrough-arg char))))
      (claude-repl--slash-start)
      (should (equal passthrough-arg "/")))))

;;; append-to-input-buffer: dead buffer for workspace

(ert-deftest claude-repl-test-append-to-input-buffer-dead-buffer ()
  "`claude-repl--append-to-input-buffer' errors when the input buffer is dead.
The ws-get returns a non-nil dead buffer, passing the `when-let' guard,
but `with-current-buffer' on a dead buffer signals an error."
  (claude-repl-test--with-clean-state
    (let ((buf (get-buffer-create " *test-append-dead*")))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1")))
        (claude-repl--ws-put "ws1" :input-buffer buf)
        (kill-buffer buf)
        ;; Dead buffer passes when-let but with-current-buffer errors
        (should-error (claude-repl--append-to-input-buffer "text"))))))

;;;; ---- Tests: mark-ws-thinking state overwrite edge cases (status transitions .md) ----

(ert-deftest claude-repl-test-mark-ws-thinking-overwrites-permission ()
  "mark-ws-thinking should overwrite :permission with :thinking."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set "ws1" :permission)
    (claude-repl--mark-ws-thinking "ws1")
    (should (eq (claude-repl--ws-state "ws1") :thinking))))

(ert-deftest claude-repl-test-mark-ws-thinking-overwrites-done ()
  "mark-ws-thinking should overwrite :done with :thinking."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set "ws1" :done)
    (claude-repl--mark-ws-thinking "ws1")
    (should (eq (claude-repl--ws-state "ws1") :thinking))))

(ert-deftest claude-repl-test-mark-ws-thinking-overwrites-inactive ()
  "mark-ws-thinking should overwrite :inactive with :thinking."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set "ws1" :inactive)
    (claude-repl--mark-ws-thinking "ws1")
    (should (eq (claude-repl--ws-state "ws1") :thinking))))

;;;; ---- Tests: metaprompt defcustom defaults ----

(ert-deftest claude-repl-test-skip-permissions-default ()
  "`claude-repl-skip-permissions' should default to t."
  (should (eq (default-value 'claude-repl-skip-permissions) t)))

(ert-deftest claude-repl-test-prefix-period-default ()
  "`claude-repl-prefix-period' should default to 7."
  (should (= (default-value 'claude-repl-prefix-period) 7)))

(ert-deftest claude-repl-test-command-prefix-contains-text ()
  "`claude-repl--command-prefix' should contain the metaprompt text."
  (should (stringp claude-repl--command-prefix))
  (should (string-match-p "metaprompt" claude-repl--command-prefix)))

(provide 'test-input)

;;; test-input.el ends here
