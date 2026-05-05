;;; test-history.el --- ERT tests for history.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for input history, session state persistence, and file I/O helpers.
;;
;; Run with:
;;   emacs -batch -Q -l ert -l test-history.el -f ert-run-tests-batch-and-exit

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Tests: history-push ----

(ert-deftest claude-repl-test-history-push ()
  "history-push should add text, skip empty, skip duplicates."
  (claude-repl-test--with-temp-buffer " *test-hist*"
    (setq-local claude-repl--input-history nil)
    ;; Push "hello"
    (insert "hello")
    (claude-repl--history-push)
    (should (equal claude-repl--input-history '("hello")))
    ;; Duplicate should be skipped
    (erase-buffer) (insert "hello")
    (claude-repl--history-push)
    (should (equal claude-repl--input-history '("hello")))
    ;; New entry
    (erase-buffer) (insert "world")
    (claude-repl--history-push)
    (should (equal claude-repl--input-history '("world" "hello")))
    ;; Empty should be skipped
    (erase-buffer) (insert "   ")
    (claude-repl--history-push)
    (should (equal claude-repl--input-history '("world" "hello")))))

;;;; ---- Tests: history-prev / history-next ----

(ert-deftest claude-repl-test-history-prev-next ()
  "history-prev and history-next should navigate correctly."
  (claude-repl-test--with-temp-buffer " *test-hist-nav*"
    (setq-local claude-repl--input-history '("second" "first"))
    (setq-local claude-repl--history-index -1)
    (setq-local claude-repl--history-stash nil)
    (setq-local claude-repl--history-navigating nil)
    (insert "current text")
    ;; Go back once
    (claude-repl--history-prev)
    (should (equal (buffer-string) "second"))
    (should (= claude-repl--history-index 0))
    (should (equal claude-repl--history-stash "current text"))
    ;; Go back again
    (claude-repl--history-prev)
    (should (equal (buffer-string) "first"))
    (should (= claude-repl--history-index 1))
    ;; Go forward
    (claude-repl--history-next)
    (should (equal (buffer-string) "second"))
    (should (= claude-repl--history-index 0))
    ;; Go forward past newest -- restore stash
    (claude-repl--history-next)
    (should (equal (buffer-string) "current text"))
    (should (= claude-repl--history-index -1))))

;;;; ---- Tests: history-reset ----

(ert-deftest claude-repl-test-history-reset ()
  "history-reset should set index back to -1."
  (claude-repl-test--with-temp-buffer " *test-hist-reset*"
    (setq-local claude-repl--history-index 3)
    (claude-repl--history-reset)
    (should (= claude-repl--history-index -1))))

;;;; ---- Tests: history-on-change ----

(ert-deftest claude-repl-test-history-on-change-resets ()
  "Editing buffer while browsing history should reset the index."
  (claude-repl-test--with-temp-buffer " *test-hist-change*"
    (setq-local claude-repl--history-index 2)
    (setq-local claude-repl--history-navigating nil)
    (claude-repl--history-on-change)
    (should (= claude-repl--history-index -1))))

(ert-deftest claude-repl-test-history-on-change-noop-during-navigation ()
  "When `claude-repl--history-navigating' is t, `history-on-change' does NOT reset the index."
  (claude-repl-test--with-temp-buffer " *test-hist-nav-noop*"
    (setq-local claude-repl--history-index 3)
    (setq-local claude-repl--history-navigating t)
    (claude-repl--history-on-change)
    (should (= claude-repl--history-index 3))))

;;;; ---- Tests: history edge cases ----

(ert-deftest claude-repl-test-history-prev-empty-list ()
  "Calling `history-prev' with empty history does nothing."
  (claude-repl-test--with-temp-buffer " *test-hist-empty*"
    (setq-local claude-repl--input-history nil)
    (setq-local claude-repl--history-index -1)
    (setq-local claude-repl--history-navigating nil)
    (insert "original")
    (claude-repl--history-prev)
    (should (equal (buffer-string) "original"))
    (should (= claude-repl--history-index -1))))

(ert-deftest claude-repl-test-history-prev-at-oldest ()
  "When already at the last (oldest) entry, `history-prev' does nothing."
  (claude-repl-test--with-temp-buffer " *test-hist-oldest*"
    (setq-local claude-repl--input-history '("only-entry"))
    (setq-local claude-repl--history-index 0)
    (setq-local claude-repl--history-stash "stashed")
    (setq-local claude-repl--history-navigating nil)
    (insert "only-entry")
    (claude-repl--history-prev)
    ;; index should stay at 0, buffer unchanged
    (should (= claude-repl--history-index 0))
    (should (equal (buffer-string) "only-entry"))))

(ert-deftest claude-repl-test-history-next-at-start ()
  "When `claude-repl--history-index' is -1, `history-next' does nothing."
  (claude-repl-test--with-temp-buffer " *test-hist-at-start*"
    (setq-local claude-repl--input-history '("a" "b"))
    (setq-local claude-repl--history-index -1)
    (setq-local claude-repl--history-navigating nil)
    (insert "current")
    (claude-repl--history-next)
    (should (equal (buffer-string) "current"))
    (should (= claude-repl--history-index -1))))

;;;; ---- Tests: history-file path ----

(ert-deftest claude-repl-test-history-file-path ()
  "`claude-repl--history-file' returns `<root>/.claude-repl-history'."
  (should (equal (claude-repl--history-file "/test/root")
                 (expand-file-name ".claude-repl-history" "/test/root"))))

;;;; ---- Tests: history-save path resolution ----

(ert-deftest claude-repl-test-history-save-uses-ws-project-dir ()
  "history-save should compute the history file path from the workspace's
`:project-dir', not from `default-directory' or any buffer-local."
  (claude-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-hist-pdir-" t)))
      (unwind-protect
          (claude-repl-test--with-temp-buffer " *test-hist-save-pdir*"
            (setq-local claude-repl--input-history '("entry1" "entry2"))
            (claude-repl--ws-put "test-ws" :project-dir tmpdir)
            (claude-repl--ws-put "test-ws" :input-buffer (current-buffer))
            (let ((default-directory "/should-not-be-used/"))
              (claude-repl--history-save "test-ws"))
            (should (file-exists-p (expand-file-name ".claude-repl-history" tmpdir))))
        (delete-directory tmpdir t)))))

;;;; ---- Tests: history-push with explicit text argument ----

(ert-deftest claude-repl-test-history-push-explicit-text ()
  "history-push with an explicit TEXT argument uses that instead of buffer."
  (claude-repl-test--with-temp-buffer " *test-hist-push-text*"
    (setq-local claude-repl--input-history nil)
    (insert "buffer content")
    (claude-repl--history-push "explicit text")
    (should (equal claude-repl--input-history '("explicit text")))))

(ert-deftest claude-repl-test-history-push-trims-whitespace ()
  "history-push should trim leading/trailing whitespace from text."
  (claude-repl-test--with-temp-buffer " *test-hist-push-trim*"
    (setq-local claude-repl--input-history nil)
    (claude-repl--history-push "  hello  ")
    (should (equal claude-repl--input-history '("hello")))))

(ert-deftest claude-repl-test-history-push-duplicate-after-trim ()
  "history-push should skip duplicates that match after trimming."
  (claude-repl-test--with-temp-buffer " *test-hist-push-dup-trim*"
    (setq-local claude-repl--input-history '("hello"))
    (claude-repl--history-push "  hello  ")
    (should (equal claude-repl--input-history '("hello")))))

;;;; ---- Tests: history-replace-buffer-text ----

(ert-deftest claude-repl-test-history-replace-buffer-text ()
  "history-replace-buffer-text should replace buffer and suppress on-change."
  (claude-repl-test--with-temp-buffer " *test-hist-replace*"
    (setq-local claude-repl--history-navigating nil)
    (setq-local claude-repl--history-index 2)
    (insert "old text")
    ;; Add change hook to check that navigating flag suppresses reset
    (add-hook 'after-change-functions #'claude-repl--history-on-change nil t)
    (claude-repl--history-replace-buffer-text "new text")
    (should (equal (buffer-string) "new text"))
    ;; Index should NOT have been reset because navigating flag was set
    (should (= claude-repl--history-index 2))))

;;;; ---- Tests: history-show-entry ----

(ert-deftest claude-repl-test-history-show-entry-from-history ()
  "history-show-entry with non-negative index shows the history entry."
  (claude-repl-test--with-temp-buffer " *test-hist-show*"
    (setq-local claude-repl--input-history '("newest" "middle" "oldest"))
    (setq-local claude-repl--history-index -1)
    (setq-local claude-repl--history-stash nil)
    (setq-local claude-repl--history-navigating nil)
    (claude-repl--history-show-entry 1)
    (should (equal (buffer-string) "middle"))
    (should (= claude-repl--history-index 1))))

(ert-deftest claude-repl-test-history-show-entry-restores-stash ()
  "history-show-entry with negative index restores the stash."
  (claude-repl-test--with-temp-buffer " *test-hist-show-stash*"
    (setq-local claude-repl--input-history '("entry"))
    (setq-local claude-repl--history-index 0)
    (setq-local claude-repl--history-stash "my stash")
    (setq-local claude-repl--history-navigating nil)
    (claude-repl--history-show-entry -1)
    (should (equal (buffer-string) "my stash"))
    (should (= claude-repl--history-index -1))))

(ert-deftest claude-repl-test-history-show-entry-nil-stash ()
  "history-show-entry with nil stash inserts empty string."
  (claude-repl-test--with-temp-buffer " *test-hist-show-nil*"
    (setq-local claude-repl--input-history '("entry"))
    (setq-local claude-repl--history-index 0)
    (setq-local claude-repl--history-stash nil)
    (setq-local claude-repl--history-navigating nil)
    (claude-repl--history-show-entry -1)
    (should (equal (buffer-string) ""))
    (should (= claude-repl--history-index -1))))

;;;; ---- Tests: history-prev stash behavior ----

(ert-deftest claude-repl-test-history-prev-stashes-only-on-first-nav ()
  "history-prev should only stash buffer text on the first navigation (index -1 -> 0)."
  (claude-repl-test--with-temp-buffer " *test-hist-stash-once*"
    (setq-local claude-repl--input-history '("b" "a"))
    (setq-local claude-repl--history-index -1)
    (setq-local claude-repl--history-stash nil)
    (setq-local claude-repl--history-navigating nil)
    (insert "original")
    ;; First prev: stash should be set
    (claude-repl--history-prev)
    (should (equal claude-repl--history-stash "original"))
    ;; Second prev: stash should NOT be overwritten
    (claude-repl--history-prev)
    (should (equal claude-repl--history-stash "original"))))

;;;; ---- Tests: history-on-change when not browsing ----

(ert-deftest claude-repl-test-history-on-change-noop-when-not-browsing ()
  "history-on-change should do nothing when index is already -1."
  (claude-repl-test--with-temp-buffer " *test-hist-change-noop*"
    (setq-local claude-repl--history-index -1)
    (setq-local claude-repl--history-navigating nil)
    (claude-repl--history-on-change)
    (should (= claude-repl--history-index -1))))

;;;; ---- Tests: with-error-logging ----

(ert-deftest claude-repl-test-with-error-logging-success ()
  "with-error-logging returns body value on success."
  (should (equal (claude-repl--with-error-logging "test" (+ 1 2)) 3)))

(ert-deftest claude-repl-test-with-error-logging-catches-error ()
  "with-error-logging catches errors and does not propagate."
  (should-not
   (condition-case _err
       (claude-repl--with-error-logging "test"
         (error "boom"))
     ;; If the error propagated, this handler would be reached
     (error 'propagated))))

;;;; ---- Tests: read-sexp-file / write-sexp-file ----

(ert-deftest claude-repl-test-write-and-read-sexp-file ()
  "write-sexp-file followed by read-sexp-file round-trips data."
  (let ((file (make-temp-file "test-sexp-")))
    (unwind-protect
        (progn
          (claude-repl--write-sexp-file file '("hello" "world"))
          (should (equal (claude-repl--read-sexp-file file) '("hello" "world"))))
      (delete-file file))))

(ert-deftest claude-repl-test-read-sexp-file-if-exists-present ()
  "read-sexp-file-if-exists returns data when file exists."
  (let ((file (make-temp-file "test-sexp-")))
    (unwind-protect
        (progn
          (claude-repl--write-sexp-file file '(a b c))
          (should (equal (claude-repl--read-sexp-file-if-exists file) '(a b c))))
      (delete-file file))))

(ert-deftest claude-repl-test-read-sexp-file-if-exists-absent ()
  "read-sexp-file-if-exists returns nil when file does not exist."
  (should-not (claude-repl--read-sexp-file-if-exists "/nonexistent/path/file.el")))

;;;; ---- Tests: instantiation serialization ----

(ert-deftest claude-repl-test-instantiation-to-plist-nil ()
  "instantiation-to-plist returns nil for nil input."
  (should-not (claude-repl--instantiation-to-plist nil)))

(ert-deftest claude-repl-test-instantiation-to-plist-basic ()
  "instantiation-to-plist serializes session-id."
  (let ((inst (make-claude-repl-instantiation :session-id "abc-123")))
    (should (equal (claude-repl--instantiation-to-plist inst)
                   '(:session-id "abc-123")))))

(ert-deftest claude-repl-test-instantiation-to-plist-empty-struct ()
  "instantiation-to-plist serializes an empty struct with nil fields."
  (let ((inst (make-claude-repl-instantiation)))
    (should (equal (claude-repl--instantiation-to-plist inst)
                   '(:session-id nil)))))

(ert-deftest claude-repl-test-make-instantiation-from-plist-basic ()
  "make-instantiation-from-plist creates a struct with session-id."
  (let ((inst (claude-repl--make-instantiation-from-plist
               '(:session-id "xyz-789"))))
    (should (claude-repl-instantiation-p inst))
    (should (equal (claude-repl-instantiation-session-id inst) "xyz-789"))))

(ert-deftest claude-repl-test-make-instantiation-from-plist-nil ()
  "make-instantiation-from-plist with nil creates a fresh empty struct."
  (let ((inst (claude-repl--make-instantiation-from-plist nil)))
    (should (claude-repl-instantiation-p inst))
    (should-not (claude-repl-instantiation-session-id inst))))

(ert-deftest claude-repl-test-make-instantiation-from-plist-extra-keys ()
  "make-instantiation-from-plist ignores extra keys in saved plist."
  (let ((inst (claude-repl--make-instantiation-from-plist
               '(:session-id "xyz" :unknown-key "val"))))
    (should (equal (claude-repl-instantiation-session-id inst) "xyz"))))

(ert-deftest claude-repl-test-make-instantiation-from-plist-legacy-had-session ()
  "make-instantiation-from-plist ignores the legacy :had-session key in old state files."
  (let ((inst (claude-repl--make-instantiation-from-plist
               '(:session-id "legacy" :had-session t))))
    (should (claude-repl-instantiation-p inst))
    (should (equal (claude-repl-instantiation-session-id inst) "legacy"))))

;;;; ---- Tests: state-file ----

(ert-deftest claude-repl-test-state-file-with-root ()
  "state-file returns path under the given root."
  (should (equal (claude-repl--state-file "/my/project")
                 (expand-file-name ".claude-repl-state" "/my/project"))))

(ert-deftest claude-repl-test-state-file-nil-root ()
  "state-file returns nil when root is nil."
  (should-not (claude-repl--state-file nil)))

;;;; ---- Tests: ws-live-input-buffer ----

(ert-deftest claude-repl-test-ws-live-input-buffer-returns-live ()
  "ws-live-input-buffer returns a live buffer when one exists."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-buffer " *test-live-input*"
      (claude-repl--ws-put "test-ws" :input-buffer (current-buffer))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws")))
        (should (eq (claude-repl--ws-live-input-buffer nil) (current-buffer)))))))

(ert-deftest claude-repl-test-ws-live-input-buffer-returns-nil-for-dead ()
  "ws-live-input-buffer returns nil when the buffer has been killed."
  (claude-repl-test--with-clean-state
    (let ((buf (get-buffer-create " *test-dead-input*")))
      (claude-repl--ws-put "test-ws" :input-buffer buf)
      (kill-buffer buf)
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws")))
        (should-not (claude-repl--ws-live-input-buffer nil))))))

(ert-deftest claude-repl-test-ws-live-input-buffer-nil-when-no-buffer ()
  "ws-live-input-buffer returns nil when no input buffer is set."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws")))
      (should-not (claude-repl--ws-live-input-buffer nil)))))

(ert-deftest claude-repl-test-ws-live-input-buffer-explicit-ws ()
  "ws-live-input-buffer accepts an explicit workspace name."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-buffer " *test-explicit-ws*"
      (claude-repl--ws-put "my-ws" :input-buffer (current-buffer))
      (should (eq (claude-repl--ws-live-input-buffer "my-ws") (current-buffer))))))

;;;; ---- Tests: collect-env-state ----

(ert-deftest claude-repl-test-collect-env-state ()
  "collect-env-state returns plists for each environment key."
  (claude-repl-test--with-clean-state
    (let ((sandbox-inst (make-claude-repl-instantiation :session-id "s1"))
          (bare-inst (make-claude-repl-instantiation :session-id "b1")))
      (claude-repl--ws-put "ws" :sandbox sandbox-inst)
      (claude-repl--ws-put "ws" :bare-metal bare-inst)
      (let ((state (claude-repl--collect-env-state "ws")))
        (should (equal (plist-get state :bare-metal)
                       '(:session-id "b1")))
        (should (equal (plist-get state :sandbox)
                       '(:session-id "s1")))))))

(ert-deftest claude-repl-test-collect-env-state-nil-envs ()
  "collect-env-state returns nil plists when no envs are initialized."
  (claude-repl-test--with-clean-state
    (let ((state (claude-repl--collect-env-state "ws")))
      (should-not (plist-get state :bare-metal))
      (should-not (plist-get state :sandbox)))))

;;;; ---- Tests: state-save ----

(ert-deftest claude-repl-test-state-save-writes-file ()
  "state-save writes state to disk when :project-dir is set."
  (claude-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-state-" t)))
      (unwind-protect
          (progn
            (claude-repl--ws-put "ws" :project-dir tmpdir)
            (claude-repl--ws-put "ws" :active-env :bare-metal)
            (claude-repl--ws-put "ws" :bare-metal
                                 (make-claude-repl-instantiation :session-id "s1"))
            (claude-repl--ws-put "ws" :sandbox
                                 (make-claude-repl-instantiation))
            (claude-repl--state-save "ws")
            (let* ((file (expand-file-name ".claude-repl-state" tmpdir))
                   (data (claude-repl--read-sexp-file file)))
              (should (equal (plist-get data :project-dir) tmpdir))
              (should (equal (plist-get data :active-env) :bare-metal))))
        (delete-directory tmpdir t)))))

(ert-deftest claude-repl-test-state-save-skips-when-no-project-dir ()
  "state-save does not write when :project-dir is nil."
  (claude-repl-test--with-clean-state
    ;; No :project-dir set -- should not error
    (claude-repl--state-save "ws")))

(ert-deftest claude-repl-test-state-save-includes-priority ()
  "state-save serializes `:priority' so badges survive restarts."
  (claude-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-state-" t)))
      (unwind-protect
          (progn
            (claude-repl--ws-put "ws" :project-dir tmpdir)
            (claude-repl--ws-put "ws" :active-env :bare-metal)
            (claude-repl--ws-put "ws" :priority "p1")
            (claude-repl--ws-put "ws" :bare-metal (make-claude-repl-instantiation))
            (claude-repl--ws-put "ws" :sandbox (make-claude-repl-instantiation))
            (claude-repl--state-save "ws")
            (let* ((file (expand-file-name ".claude-repl-state" tmpdir))
                   (data (claude-repl--read-sexp-file file)))
              (should (equal (plist-get data :priority) "p1"))))
        (delete-directory tmpdir t)))))

(ert-deftest claude-repl-test-state-save-nil-priority ()
  "state-save writes `:priority' nil when no badge is set (no badge == nil)."
  (claude-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-state-" t)))
      (unwind-protect
          (progn
            (claude-repl--ws-put "ws" :project-dir tmpdir)
            (claude-repl--ws-put "ws" :active-env :bare-metal)
            (claude-repl--ws-put "ws" :bare-metal (make-claude-repl-instantiation))
            (claude-repl--ws-put "ws" :sandbox (make-claude-repl-instantiation))
            (claude-repl--state-save "ws")
            (let* ((file (expand-file-name ".claude-repl-state" tmpdir))
                   (data (claude-repl--read-sexp-file file)))
              (should (null (plist-get data :priority)))))
        (delete-directory tmpdir t)))))

(ert-deftest claude-repl-test-state-save-includes-source-ws-dir ()
  "state-save serializes `:source-ws-dir' so the merge target survives restarts."
  (claude-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-state-" t)))
      (unwind-protect
          (progn
            (claude-repl--ws-put "ws" :project-dir tmpdir)
            (claude-repl--ws-put "ws" :active-env :bare-metal)
            (claude-repl--ws-put "ws" :source-ws-dir "/tmp/source-repo/")
            (claude-repl--ws-put "ws" :bare-metal (make-claude-repl-instantiation))
            (claude-repl--ws-put "ws" :sandbox (make-claude-repl-instantiation))
            (claude-repl--state-save "ws")
            (let* ((file (expand-file-name ".claude-repl-state" tmpdir))
                   (data (claude-repl--read-sexp-file file)))
              (should (equal (plist-get data :source-ws-dir) "/tmp/source-repo/"))))
        (delete-directory tmpdir t)))))

(ert-deftest claude-repl-test-state-save-nil-source-ws-dir ()
  "state-save writes `:source-ws-dir' nil when no source is recorded."
  (claude-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-state-" t)))
      (unwind-protect
          (progn
            (claude-repl--ws-put "ws" :project-dir tmpdir)
            (claude-repl--ws-put "ws" :active-env :bare-metal)
            (claude-repl--ws-put "ws" :bare-metal (make-claude-repl-instantiation))
            (claude-repl--ws-put "ws" :sandbox (make-claude-repl-instantiation))
            (claude-repl--state-save "ws")
            (let* ((file (expand-file-name ".claude-repl-state" tmpdir))
                   (data (claude-repl--read-sexp-file file)))
              (should (null (plist-get data :source-ws-dir)))))
        (delete-directory tmpdir t)))))

;;;; ---- Tests: validate-ws-env ----

(ert-deftest claude-repl-test-validate-ws-env-valid ()
  "validate-ws-env passes for well-formed workspace state."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws" :active-env :bare-metal)
    (claude-repl--ws-put "ws" :bare-metal (make-claude-repl-instantiation))
    (claude-repl--ws-put "ws" :sandbox (make-claude-repl-instantiation))
    ;; Should not error
    (claude-repl--validate-ws-env "ws")))

(ert-deftest claude-repl-test-validate-ws-env-valid-with-session ()
  "validate-ws-env passes when instantiation has a string session-id."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws" :active-env :sandbox)
    (claude-repl--ws-put "ws" :bare-metal (make-claude-repl-instantiation))
    (claude-repl--ws-put "ws" :sandbox
                         (make-claude-repl-instantiation :session-id "abc"))
    (claude-repl--validate-ws-env "ws")))

(ert-deftest claude-repl-test-validate-ws-env-invalid-active-env ()
  "validate-ws-env errors on invalid :active-env."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws" :active-env :bogus)
    (claude-repl--ws-put "ws" :bare-metal (make-claude-repl-instantiation))
    (claude-repl--ws-put "ws" :sandbox (make-claude-repl-instantiation))
    (should-error (claude-repl--validate-ws-env "ws"))))

(ert-deftest claude-repl-test-validate-ws-env-nil-active-env ()
  "validate-ws-env errors when :active-env is nil."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws" :bare-metal (make-claude-repl-instantiation))
    (claude-repl--ws-put "ws" :sandbox (make-claude-repl-instantiation))
    (should-error (claude-repl--validate-ws-env "ws"))))

(ert-deftest claude-repl-test-validate-ws-env-missing-struct ()
  "validate-ws-env errors when an environment has no instantiation struct."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws" :active-env :bare-metal)
    (claude-repl--ws-put "ws" :bare-metal (make-claude-repl-instantiation))
    ;; :sandbox is missing
    (should-error (claude-repl--validate-ws-env "ws"))))

(ert-deftest claude-repl-test-validate-ws-env-invalid-session-id ()
  "validate-ws-env errors when session-id is not a string or nil."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws" :active-env :bare-metal)
    (claude-repl--ws-put "ws" :bare-metal
                         (make-claude-repl-instantiation :session-id 42))
    (claude-repl--ws-put "ws" :sandbox (make-claude-repl-instantiation))
    (should-error (claude-repl--validate-ws-env "ws"))))

;;;; ---- Tests: initialize-ws-env integration ----

(ert-deftest claude-repl-test-initialize-ws-env-restores-from-file ()
  "initialize-ws-env restores full state from disk including :active-env."
  (claude-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-state-" t)))
      (unwind-protect
          (progn
            (claude-repl--write-sexp-file
             (expand-file-name ".claude-repl-state" tmpdir)
             '(:project-dir "/restored/root"
               :active-env :sandbox
               :bare-metal (:session-id "bm-id")
               :sandbox (:session-id "sb-id")))
            (claude-repl--ws-put "ws" :project-dir tmpdir)
            (claude-repl--initialize-ws-env "ws")
            (should (eq (claude-repl--ws-get "ws" :active-env) :sandbox))
            (should (equal (claude-repl--ws-get "ws" :project-dir) "/restored/root"))
            (should (equal (claude-repl-instantiation-session-id
                            (claude-repl--ws-get "ws" :bare-metal))
                           "bm-id"))
            (should (equal (claude-repl-instantiation-session-id
                            (claude-repl--ws-get "ws" :sandbox))
                           "sb-id")))
        (delete-directory tmpdir t)))))

(ert-deftest claude-repl-test-initialize-ws-env-fresh-when-no-file ()
  "initialize-ws-env creates fresh defaults when no state file exists."
  (claude-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-state-" t)))
      (unwind-protect
          (progn
            (claude-repl--ws-put "ws" :project-dir tmpdir)
            (claude-repl--initialize-ws-env "ws")
            (should (eq (claude-repl--ws-get "ws" :active-env) :bare-metal))
            (should (claude-repl-instantiation-p (claude-repl--ws-get "ws" :bare-metal)))
            (should (claude-repl-instantiation-p (claude-repl--ws-get "ws" :sandbox)))
            (should-not (claude-repl-instantiation-session-id
                         (claude-repl--ws-get "ws" :bare-metal))))
        (delete-directory tmpdir t)))))

(ert-deftest claude-repl-test-initialize-ws-env-save-restore-round-trip ()
  "state-save followed by initialize-ws-env restores :active-env across restart."
  (claude-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-state-env-" t)))
      (unwind-protect
          (progn
            ;; Simulate pre-restart state: sandbox was active
            (claude-repl--ws-put "ws" :project-dir tmpdir)
            (claude-repl--ws-put "ws" :active-env :sandbox)
            (claude-repl--ws-put "ws" :bare-metal
                                 (make-claude-repl-instantiation :session-id "bm1"))
            (claude-repl--ws-put "ws" :sandbox
                                 (make-claude-repl-instantiation :session-id "sb1"))
            (claude-repl--state-save "ws")
            ;; Simulate post-restart: clear in-memory state
            (clrhash claude-repl--workspaces)
            (claude-repl--ws-put "ws" :project-dir tmpdir)
            (claude-repl--initialize-ws-env "ws")
            ;; :active-env should be restored to :sandbox
            (should (eq (claude-repl--ws-get "ws" :active-env) :sandbox))
            (should (equal (claude-repl-instantiation-session-id
                            (claude-repl--ws-get "ws" :sandbox))
                           "sb1")))
        (delete-directory tmpdir t)))))

(ert-deftest claude-repl-test-initialize-ws-env-restores-source-ws-dir ()
  "initialize-ws-env restores `:source-ws-dir' from the saved state file."
  (claude-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-state-src-" t)))
      (unwind-protect
          (progn
            (claude-repl--write-sexp-file
             (expand-file-name ".claude-repl-state" tmpdir)
             '(:project-dir "/restored/root"
               :active-env :bare-metal
               :source-ws-dir "/tmp/recorded-source/"
               :bare-metal (:session-id nil)
               :sandbox (:session-id nil)))
            (claude-repl--ws-put "ws" :project-dir tmpdir)
            (claude-repl--initialize-ws-env "ws")
            (should (equal (claude-repl--ws-get "ws" :source-ws-dir)
                           "/tmp/recorded-source/")))
        (delete-directory tmpdir t)))))

(ert-deftest claude-repl-test-initialize-ws-env-old-state-file-no-source-ws-dir ()
  "Old state files (no `:source-ws-dir' key) leave the value as nil — no error."
  (claude-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-state-old-" t)))
      (unwind-protect
          (progn
            (claude-repl--write-sexp-file
             (expand-file-name ".claude-repl-state" tmpdir)
             '(:project-dir "/restored/root"
               :active-env :bare-metal
               :bare-metal (:session-id nil)
               :sandbox (:session-id nil)))
            (claude-repl--ws-put "ws" :project-dir tmpdir)
            (claude-repl--initialize-ws-env "ws")
            (should (null (claude-repl--ws-get "ws" :source-ws-dir))))
        (delete-directory tmpdir t)))))

(ert-deftest claude-repl-test-initialize-ws-env-source-ws-dir-round-trip ()
  "state-save followed by initialize-ws-env restores `:source-ws-dir' across restart."
  (claude-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-state-src-rt-" t)))
      (unwind-protect
          (progn
            (claude-repl--ws-put "ws" :project-dir tmpdir)
            (claude-repl--ws-put "ws" :active-env :bare-metal)
            (claude-repl--ws-put "ws" :source-ws-dir "/tmp/source-roundtrip/")
            (claude-repl--ws-put "ws" :bare-metal (make-claude-repl-instantiation))
            (claude-repl--ws-put "ws" :sandbox (make-claude-repl-instantiation))
            (claude-repl--state-save "ws")
            ;; Simulate restart
            (clrhash claude-repl--workspaces)
            (claude-repl--ws-put "ws" :project-dir tmpdir)
            (claude-repl--initialize-ws-env "ws")
            (should (equal (claude-repl--ws-get "ws" :source-ws-dir)
                           "/tmp/source-roundtrip/")))
        (delete-directory tmpdir t)))))

;;;; ---- Tests: history-save / history-restore round-trip ----

(ert-deftest claude-repl-test-history-save-and-restore-round-trip ()
  "history-save writes and history-restore reads back the same data."
  (claude-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-hist-" t)))
      (unwind-protect
          (claude-repl-test--with-temp-buffer " *test-hist-round-trip*"
            (setq-local default-directory tmpdir)
            (setq-local claude-repl--input-history '("third" "second" "first"))
            (claude-repl--ws-put "test-ws" :project-dir tmpdir)
            (claude-repl--ws-put "test-ws" :input-buffer (current-buffer))
            ;; Save
            (claude-repl--history-save "test-ws")
            ;; Clear and restore
            (setq-local claude-repl--input-history nil)
            (claude-repl--history-restore "test-ws")
            (should (equal claude-repl--input-history '("third" "second" "first"))))
        (delete-directory tmpdir t)))))

(ert-deftest claude-repl-test-history-save-skips-nil-history ()
  "history-save should not write when history is nil."
  (claude-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-hist-" t)))
      (unwind-protect
          (claude-repl-test--with-temp-buffer " *test-hist-save-nil*"
            (setq-local claude-repl--input-history nil)
            (claude-repl--ws-put "test-ws" :project-dir tmpdir)
            (claude-repl--ws-put "test-ws" :input-buffer (current-buffer))
            (claude-repl--history-save "test-ws")
            ;; File should not exist
            (should-not (file-exists-p
                         (expand-file-name ".claude-repl-history" tmpdir))))
        (delete-directory tmpdir t)))))

(ert-deftest claude-repl-test-history-save-skips-dead-buffer ()
  "history-save should do nothing when the input buffer is dead."
  (claude-repl-test--with-clean-state
    (let ((buf (get-buffer-create " *test-hist-dead*")))
      (with-current-buffer buf
        (setq-local claude-repl--input-history '("entry")))
      (claude-repl--ws-put "test-ws" :input-buffer buf)
      (kill-buffer buf)
      ;; Should not error
      (claude-repl--history-save "test-ws"))))

;;;; ---- Tests: environment-keys constant ----

(ert-deftest claude-repl-test-environment-keys-value ()
  "environment-keys should contain :bare-metal and :sandbox."
  (should (memq :bare-metal claude-repl--environment-keys))
  (should (memq :sandbox claude-repl--environment-keys)))

;;;; ---- Tests: history-restore with no file ----

(ert-deftest claude-repl-test-history-restore-no-file ()
  "history-restore leaves history nil when no file exists."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-buffer " *test-hist-restore-none*"
      (setq-local claude-repl--input-history nil)
      (claude-repl--ws-put "test-ws" :project-dir "/nonexistent/path")
      (claude-repl--history-restore "test-ws")
      (should-not claude-repl--input-history))))

;;;; ---- Tests: with-error-logging edge cases ----

(ert-deftest claude-repl-test-with-error-logging-body-returns-nil ()
  "with-error-logging returns nil from body without confusing it with error."
  (should (eq (claude-repl--with-error-logging "test" nil) nil)))

(ert-deftest claude-repl-test-with-error-logging-multiple-forms ()
  "with-error-logging returns the value of the last form in body."
  (should (equal (claude-repl--with-error-logging "test"
                   (+ 1 2)
                   (* 3 4))
                 12)))

(ert-deftest claude-repl-test-with-error-logging-calls-log-on-error ()
  "with-error-logging calls `claude-repl--log' with label on error."
  (let ((logged-msg nil))
    (cl-letf (((symbol-function 'claude-repl--log)
               (lambda (ws fmt &rest args)
                 (setq logged-msg (apply #'format fmt args)))))
      (claude-repl--with-error-logging "my-label"
        (error "kaboom"))
      (should (stringp logged-msg))
      (should (string-match-p "my-label" logged-msg)))))

;;;; ---- Tests: read-sexp-file edge cases ----

(ert-deftest claude-repl-test-read-sexp-file-multiple-sexps ()
  "read-sexp-file returns only the first sexp from a file with multiple."
  (let ((file (make-temp-file "test-sexp-multi-")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "(first-sexp)\n(second-sexp)"))
          (should (equal (claude-repl--read-sexp-file file) '(first-sexp))))
      (delete-file file))))

(ert-deftest claude-repl-test-read-sexp-file-complex-nested ()
  "read-sexp-file handles complex nested data structures."
  (let ((file (make-temp-file "test-sexp-nested-"))
        (data '((:key1 . "val1") (:key2 . [1 2 3]) (:key3 . (:a 1 :b 2)))))
    (unwind-protect
        (progn
          (claude-repl--write-sexp-file file data)
          (should (equal (claude-repl--read-sexp-file file) data)))
      (delete-file file))))

(ert-deftest claude-repl-test-read-sexp-file-empty-file ()
  "read-sexp-file signals an error on an empty file."
  (let ((file (make-temp-file "test-sexp-empty-")))
    (unwind-protect
        (progn
          ;; File exists but is empty (make-temp-file creates empty file)
          (should-error (claude-repl--read-sexp-file file)))
      (delete-file file))))

;;;; ---- Tests: write-sexp-file edge cases ----

(ert-deftest claude-repl-test-write-sexp-file-nil-value ()
  "write-sexp-file can write nil and read it back."
  (let ((file (make-temp-file "test-sexp-nil-")))
    (unwind-protect
        (progn
          (claude-repl--write-sexp-file file nil)
          (should (eq (claude-repl--read-sexp-file file) nil)))
      (delete-file file))))

(ert-deftest claude-repl-test-write-sexp-file-special-characters ()
  "write-sexp-file handles strings with quotes and newlines."
  (let ((file (make-temp-file "test-sexp-special-"))
        (data '("a string with \"quotes\"" "and\nnewlines")))
    (unwind-protect
        (progn
          (claude-repl--write-sexp-file file data)
          (should (equal (claude-repl--read-sexp-file file) data)))
      (delete-file file))))

(ert-deftest claude-repl-test-write-sexp-file-nonexistent-parent ()
  "write-sexp-file errors when parent directory does not exist."
  (should-error
   (claude-repl--write-sexp-file "/nonexistent/parent/dir/file.el" '(data))))

;;;; ---- Tests: read-sexp-file-if-exists edge cases ----

(ert-deftest claude-repl-test-read-sexp-file-if-exists-empty-file ()
  "read-sexp-file-if-exists errors when file exists but is empty."
  (let ((file (make-temp-file "test-sexp-if-exists-empty-")))
    (unwind-protect
        (should-error (claude-repl--read-sexp-file-if-exists file))
      (delete-file file))))

;;;; ---- Tests: instantiation-to-plist edge cases ----

(ert-deftest claude-repl-test-instantiation-to-plist-omits-start-cmd ()
  "instantiation-to-plist does NOT include start-cmd in output."
  (let* ((inst (make-claude-repl-instantiation
                :session-id "abc"
                :start-cmd "claude --resume"))
         (plist (claude-repl--instantiation-to-plist inst)))
    (should (equal plist '(:session-id "abc")))
    (should-not (plist-member plist :start-cmd))))

;;;; ---- Tests: make-instantiation-from-plist edge cases ----
;; (basic, nil, extra-keys, and partial-keys tests are in the main section above)

;;;; ---- Tests: ws-live-input-buffer edge cases ----

(ert-deftest claude-repl-test-ws-live-input-buffer-both-ws-nil ()
  "ws-live-input-buffer returns nil when WS is nil and +workspace-current-name returns nil."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () nil)))
      (should-not (claude-repl--ws-live-input-buffer nil)))))

;;;; ---- Tests: history-save edge cases ----

(ert-deftest claude-repl-test-history-save-single-entry ()
  "history-save writes and reads back a single-entry history list."
  (claude-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-hist-single-" t)))
      (unwind-protect
          (claude-repl-test--with-temp-buffer " *test-hist-single*"
            (setq-local claude-repl--input-history '("only-one"))
            (claude-repl--ws-put "test-ws" :project-dir tmpdir)
            (claude-repl--ws-put "test-ws" :input-buffer (current-buffer))
            (claude-repl--history-save "test-ws")
            ;; Read back and verify
            (let ((file (expand-file-name ".claude-repl-history" tmpdir)))
              (should (equal (claude-repl--read-sexp-file file) '("only-one")))))
        (delete-directory tmpdir t)))))

;;;; ---- Tests: history-restore edge cases ----

(ert-deftest claude-repl-test-history-restore-non-list-data ()
  "history-restore sets history to whatever is in the file, even non-list."
  (claude-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-hist-nonlist-" t)))
      (unwind-protect
          (claude-repl-test--with-temp-buffer " *test-hist-nonlist*"
            (setq-local claude-repl--input-history nil)
            (claude-repl--ws-put "test-ws" :project-dir tmpdir)
            ;; Write a string (non-list) to the history file
            (claude-repl--write-sexp-file
             (expand-file-name ".claude-repl-history" tmpdir) "just-a-string")
            (claude-repl--history-restore "test-ws")
            (should (equal claude-repl--input-history "just-a-string")))
        (delete-directory tmpdir t)))))

(ert-deftest claude-repl-test-history-restore-overwrites-existing ()
  "history-restore overwrites existing history, does not append."
  (claude-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-hist-overwrite-" t)))
      (unwind-protect
          (claude-repl-test--with-temp-buffer " *test-hist-overwrite*"
            (setq-local claude-repl--input-history '("old1" "old2"))
            (claude-repl--ws-put "test-ws" :project-dir tmpdir)
            (claude-repl--write-sexp-file
             (expand-file-name ".claude-repl-history" tmpdir) '("new1"))
            (claude-repl--history-restore "test-ws")
            (should (equal claude-repl--input-history '("new1"))))
        (delete-directory tmpdir t)))))

;;;; ---- Tests: collect-env-state edge cases ----

(ert-deftest claude-repl-test-collect-env-state-partial ()
  "collect-env-state handles only one environment initialized."
  (claude-repl-test--with-clean-state
    (let ((inst (make-claude-repl-instantiation :session-id "s1")))
      (claude-repl--ws-put "ws" :sandbox inst)
      ;; :bare-metal is not set
      (let ((state (claude-repl--collect-env-state "ws")))
        (should (equal (plist-get state :sandbox)
                       '(:session-id "s1")))
        (should-not (plist-get state :bare-metal))))))

;;;; ---- Tests: state-save edge cases ----

(ert-deftest claude-repl-test-state-save-nil-active-env ()
  "state-save serializes :active-env as nil when it is unset."
  (claude-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-state-nil-env-" t)))
      (unwind-protect
          (progn
            (claude-repl--ws-put "ws" :project-dir tmpdir)
            ;; :active-env is NOT set (defaults to nil)
            (claude-repl--ws-put "ws" :bare-metal (make-claude-repl-instantiation))
            (claude-repl--ws-put "ws" :sandbox (make-claude-repl-instantiation))
            (claude-repl--state-save "ws")
            (let* ((file (expand-file-name ".claude-repl-state" tmpdir))
                   (data (claude-repl--read-sexp-file file)))
              (should (plist-member data :active-env))
              (should-not (plist-get data :active-env))))
        (delete-directory tmpdir t)))))

(ert-deftest claude-repl-test-state-save-write-error-caught ()
  "state-save catches write errors via with-error-logging."
  (claude-repl-test--with-clean-state
    ;; Point :project-dir to a nonexistent directory to trigger write error
    (claude-repl--ws-put "ws" :project-dir "/nonexistent/dir/for/test")
    (claude-repl--ws-put "ws" :bare-metal (make-claude-repl-instantiation))
    (claude-repl--ws-put "ws" :sandbox (make-claude-repl-instantiation))
    ;; Should not signal an error thanks to with-error-logging
    (claude-repl--state-save "ws")))

;;;; ---- Tests: initialize-ws-env with missing/corrupt state files ----

(ert-deftest claude-repl-test-initialize-ws-env-empty-file-creates-fresh ()
  "initialize-ws-env creates fresh state when state file is empty."
  (claude-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-state-empty-" t)))
      (unwind-protect
          (progn
            (with-temp-file (expand-file-name ".claude-repl-state" tmpdir)
              (insert ""))
            (claude-repl--ws-put "ws" :project-dir tmpdir)
            (claude-repl--initialize-ws-env "ws")
            (should (eq (claude-repl--ws-get "ws" :active-env) :bare-metal))
            (should (claude-repl-instantiation-p
                     (claude-repl--ws-get "ws" :bare-metal)))
            (should-not (claude-repl-instantiation-session-id
                         (claude-repl--ws-get "ws" :bare-metal))))
        (delete-directory tmpdir t)))))

(ert-deftest claude-repl-test-initialize-ws-env-invalid-elisp-creates-fresh ()
  "initialize-ws-env creates fresh state when state file has unreadable elisp."
  (claude-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-state-invalid-" t)))
      (unwind-protect
          (progn
            ;; Unclosed paren triggers end-of-file error in (read ...)
            (with-temp-file (expand-file-name ".claude-repl-state" tmpdir)
              (insert "(unclosed paren"))
            (claude-repl--ws-put "ws" :project-dir tmpdir)
            (claude-repl--initialize-ws-env "ws")
            (should (eq (claude-repl--ws-get "ws" :active-env) :bare-metal))
            (should (claude-repl-instantiation-p
                     (claude-repl--ws-get "ws" :bare-metal)))
            (should-not (claude-repl-instantiation-session-id
                         (claude-repl--ws-get "ws" :bare-metal))))
        (delete-directory tmpdir t)))))

(ert-deftest claude-repl-test-initialize-ws-env-missing-file-writes-state ()
  "initialize-ws-env creates state file on disk when it was missing."
  (claude-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-state-write-" t)))
      (unwind-protect
          (let ((state-path (expand-file-name ".claude-repl-state"
                                              (claude-repl--path-canonical tmpdir))))
            (claude-repl--ws-put "ws" :project-dir tmpdir)
            (should-not (file-exists-p state-path))
            (claude-repl--initialize-ws-env "ws")
            (should (file-exists-p state-path))
            ;; Verify the written file is valid and round-trips
            (let ((data (claude-repl--read-sexp-file state-path)))
              (should (eq (plist-get data :active-env) :bare-metal))
              (should (equal (plist-get data :project-dir)
                             (claude-repl--path-canonical tmpdir)))))
        (delete-directory tmpdir t)))))

;;;; ---- Tests: history-push edge cases ----

(ert-deftest claude-repl-test-history-push-internal-whitespace-differs ()
  "history-push adds entry that differs from head only in internal whitespace."
  (claude-repl-test--with-temp-buffer " *test-hist-push-internal-ws*"
    (setq-local claude-repl--input-history '("hello world"))
    (claude-repl--history-push "hello  world")
    (should (equal claude-repl--input-history '("hello  world" "hello world")))))

(ert-deftest claude-repl-test-history-push-matches-non-first ()
  "history-push adds entry matching a non-first history entry (only head is checked)."
  (claude-repl-test--with-temp-buffer " *test-hist-push-nonfirst*"
    (setq-local claude-repl--input-history '("latest" "older" "oldest"))
    (claude-repl--history-push "older")
    (should (equal claude-repl--input-history '("older" "latest" "older" "oldest")))))

;;;; ---- Tests: history-replace-buffer-text edge cases ----

(ert-deftest claude-repl-test-history-replace-buffer-text-empty-string ()
  "history-replace-buffer-text with empty string leaves buffer empty."
  (claude-repl-test--with-temp-buffer " *test-hist-replace-empty*"
    (setq-local claude-repl--history-navigating nil)
    (insert "some text")
    (claude-repl--history-replace-buffer-text "")
    (should (equal (buffer-string) ""))))

(ert-deftest claude-repl-test-history-replace-buffer-text-already-empty ()
  "history-replace-buffer-text in already-empty buffer inserts new text."
  (claude-repl-test--with-temp-buffer " *test-hist-replace-was-empty*"
    (setq-local claude-repl--history-navigating nil)
    ;; Buffer starts empty
    (should (equal (buffer-string) ""))
    (claude-repl--history-replace-buffer-text "new text")
    (should (equal (buffer-string) "new text"))))

;;;; ---- Tests: history-show-entry edge cases ----

(ert-deftest claude-repl-test-history-show-entry-last-valid-index ()
  "history-show-entry at the last valid index shows the oldest entry."
  (claude-repl-test--with-temp-buffer " *test-hist-show-last*"
    (setq-local claude-repl--input-history '("newest" "middle" "oldest"))
    (setq-local claude-repl--history-index -1)
    (setq-local claude-repl--history-stash nil)
    (setq-local claude-repl--history-navigating nil)
    (claude-repl--history-show-entry 2)
    (should (equal (buffer-string) "oldest"))
    (should (= claude-repl--history-index 2))))

;;;; ---- Tests: history-prev edge cases ----

(ert-deftest claude-repl-test-history-prev-single-entry ()
  "history-prev with single-entry history: first prev goes to 0, second stays at 0."
  (claude-repl-test--with-temp-buffer " *test-hist-prev-single*"
    (setq-local claude-repl--input-history '("only"))
    (setq-local claude-repl--history-index -1)
    (setq-local claude-repl--history-stash nil)
    (setq-local claude-repl--history-navigating nil)
    (insert "current")
    ;; First prev: goes to index 0
    (claude-repl--history-prev)
    (should (= claude-repl--history-index 0))
    (should (equal (buffer-string) "only"))
    (should (equal claude-repl--history-stash "current"))
    ;; Second prev: stays at index 0 (no older entry)
    (claude-repl--history-prev)
    (should (= claude-repl--history-index 0))
    (should (equal (buffer-string) "only"))))

;;;; ---- Tests: history-next edge cases ----

(ert-deftest claude-repl-test-history-next-from-index-zero ()
  "history-next from index 0 goes to -1 and restores stash."
  (claude-repl-test--with-temp-buffer " *test-hist-next-from-zero*"
    (setq-local claude-repl--input-history '("entry"))
    (setq-local claude-repl--history-index 0)
    (setq-local claude-repl--history-stash "my stash")
    (setq-local claude-repl--history-navigating nil)
    (insert "entry")
    (claude-repl--history-next)
    (should (= claude-repl--history-index -1))
    (should (equal (buffer-string) "my stash"))))

;;;; ---- Tests: history-on-change edge cases ----

(ert-deftest claude-repl-test-history-on-change-ignores-extra-args ()
  "history-on-change ignores extra arguments (the &rest _args)."
  (claude-repl-test--with-temp-buffer " *test-hist-change-args*"
    (setq-local claude-repl--history-index 2)
    (setq-local claude-repl--history-navigating nil)
    ;; Call with extra arguments like after-change-functions would
    (claude-repl--history-on-change 1 5 0)
    (should (= claude-repl--history-index -1))))

;;;; ---- Tests: state-purge ----

(ert-deftest claude-repl-test-state-purge-removes-state-preserves-history ()
  "state-purge deletes .claude-repl-state but preserves .claude-repl-history."
  (let ((tmpdir (make-temp-file "claude-purge-" t)))
    (unwind-protect
        (let ((state-file   (expand-file-name ".claude-repl-state"   tmpdir))
              (history-file (expand-file-name ".claude-repl-history" tmpdir)))
          (with-temp-file state-file   (insert "(:session-id \"abc\")"))
          (with-temp-file history-file (insert "(:history (\"x\"))"))
          (should (file-exists-p state-file))
          (should (file-exists-p history-file))
          (claude-repl--state-purge tmpdir)
          (should-not (file-exists-p state-file))
          (should (file-exists-p history-file)))
      (delete-directory tmpdir t))))

(ert-deftest claude-repl-test-state-purge-nil-root-noop ()
  "state-purge is a no-op when ROOT is nil."
  ;; Should not error; there's nothing else to assert — the function just returns.
  (claude-repl--state-purge nil))

(ert-deftest claude-repl-test-state-purge-idempotent ()
  "state-purge on a root with no state files is a silent no-op."
  (let ((tmpdir (make-temp-file "claude-purge-empty-" t)))
    (unwind-protect
        (progn
          (claude-repl--state-purge tmpdir)
          ;; Running again should still be fine.
          (claude-repl--state-purge tmpdir))
      (delete-directory tmpdir t))))

(ert-deftest claude-repl-test-state-purge-leaves-unrelated-files ()
  "state-purge only removes the managed per-project files."
  (let ((tmpdir (make-temp-file "claude-purge-unrelated-" t)))
    (unwind-protect
        (let ((state-file   (expand-file-name ".claude-repl-state" tmpdir))
              (unrelated    (expand-file-name "README.md"          tmpdir)))
          (with-temp-file state-file (insert "(:session-id \"abc\")"))
          (with-temp-file unrelated  (insert "# project"))
          (claude-repl--state-purge tmpdir)
          (should-not (file-exists-p state-file))
          (should (file-exists-p unrelated)))
      (delete-directory tmpdir t))))

(provide 'test-history)

;;; test-history.el ends here
