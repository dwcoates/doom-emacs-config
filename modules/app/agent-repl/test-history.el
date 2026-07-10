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

(ert-deftest agent-repl-test-history-push ()
  "history-push should add text, skip empty, skip duplicates."
  (agent-repl-test--with-temp-buffer " *test-hist*"
    (setq-local agent-repl--input-history nil)
    ;; Push "hello"
    (insert "hello")
    (agent-repl--history-push)
    (should (equal agent-repl--input-history '("hello")))
    ;; Duplicate should be skipped
    (erase-buffer) (insert "hello")
    (agent-repl--history-push)
    (should (equal agent-repl--input-history '("hello")))
    ;; New entry
    (erase-buffer) (insert "world")
    (agent-repl--history-push)
    (should (equal agent-repl--input-history '("world" "hello")))
    ;; Empty should be skipped
    (erase-buffer) (insert "   ")
    (agent-repl--history-push)
    (should (equal agent-repl--input-history '("world" "hello")))))

;;;; ---- Tests: history-prev / history-next ----

(ert-deftest agent-repl-test-history-prev-next ()
  "history-prev and history-next should navigate correctly."
  (agent-repl-test--with-temp-buffer " *test-hist-nav*"
    (setq-local agent-repl--input-history '("second" "first"))
    (setq-local agent-repl--history-index -1)
    (setq-local agent-repl--history-stash nil)
    (setq-local agent-repl--history-navigating nil)
    (insert "current text")
    ;; Go back once
    (agent-repl--history-prev)
    (should (equal (buffer-string) "second"))
    (should (= agent-repl--history-index 0))
    (should (equal agent-repl--history-stash "current text"))
    ;; Go back again
    (agent-repl--history-prev)
    (should (equal (buffer-string) "first"))
    (should (= agent-repl--history-index 1))
    ;; Go forward
    (agent-repl--history-next)
    (should (equal (buffer-string) "second"))
    (should (= agent-repl--history-index 0))
    ;; Go forward past newest -- restore stash
    (agent-repl--history-next)
    (should (equal (buffer-string) "current text"))
    (should (= agent-repl--history-index -1))))

;;;; ---- Tests: history-reset ----

(ert-deftest agent-repl-test-history-reset ()
  "history-reset should set index back to -1."
  (agent-repl-test--with-temp-buffer " *test-hist-reset*"
    (setq-local agent-repl--history-index 3)
    (agent-repl--history-reset)
    (should (= agent-repl--history-index -1))))

;;;; ---- Tests: history-on-change ----

(ert-deftest agent-repl-test-history-on-change-resets ()
  "Editing buffer while browsing history should reset the index."
  (agent-repl-test--with-temp-buffer " *test-hist-change*"
    (setq-local agent-repl--history-index 2)
    (setq-local agent-repl--history-navigating nil)
    (agent-repl--history-on-change)
    (should (= agent-repl--history-index -1))))

(ert-deftest agent-repl-test-history-on-change-noop-during-navigation ()
  "When `agent-repl--history-navigating' is t, `history-on-change' does NOT reset the index."
  (agent-repl-test--with-temp-buffer " *test-hist-nav-noop*"
    (setq-local agent-repl--history-index 3)
    (setq-local agent-repl--history-navigating t)
    (agent-repl--history-on-change)
    (should (= agent-repl--history-index 3))))

;;;; ---- Tests: history edge cases ----

(ert-deftest agent-repl-test-history-prev-empty-list ()
  "Calling `history-prev' with empty history does nothing."
  (agent-repl-test--with-temp-buffer " *test-hist-empty*"
    (setq-local agent-repl--input-history nil)
    (setq-local agent-repl--history-index -1)
    (setq-local agent-repl--history-navigating nil)
    (insert "original")
    (agent-repl--history-prev)
    (should (equal (buffer-string) "original"))
    (should (= agent-repl--history-index -1))))

(ert-deftest agent-repl-test-history-prev-at-oldest ()
  "When already at the last (oldest) entry, `history-prev' does nothing."
  (agent-repl-test--with-temp-buffer " *test-hist-oldest*"
    (setq-local agent-repl--input-history '("only-entry"))
    (setq-local agent-repl--history-index 0)
    (setq-local agent-repl--history-stash "stashed")
    (setq-local agent-repl--history-navigating nil)
    (insert "only-entry")
    (agent-repl--history-prev)
    ;; index should stay at 0, buffer unchanged
    (should (= agent-repl--history-index 0))
    (should (equal (buffer-string) "only-entry"))))

(ert-deftest agent-repl-test-history-next-at-start ()
  "When `agent-repl--history-index' is -1, `history-next' does nothing."
  (agent-repl-test--with-temp-buffer " *test-hist-at-start*"
    (setq-local agent-repl--input-history '("a" "b"))
    (setq-local agent-repl--history-index -1)
    (setq-local agent-repl--history-navigating nil)
    (insert "current")
    (agent-repl--history-next)
    (should (equal (buffer-string) "current"))
    (should (= agent-repl--history-index -1))))

;;;; ---- Tests: history-file path ----

(ert-deftest agent-repl-test-history-file-path ()
  "`agent-repl--history-file' returns the history filename joined under
the per-project data subdir."
  (should (equal (agent-repl--history-file "/test/root")
                 (expand-file-name agent-repl-history-filename
                                   (expand-file-name agent-repl-emacs-data-subdir
                                                     "/test/root")))))

(ert-deftest agent-repl-test-history-file-for-read-prefers-new ()
  "history-file-for-read returns the new path when it exists."
  (let ((tmpdir (make-temp-file "test-hist-read-new-" t)))
    (unwind-protect
        (let ((new (agent-repl--history-file tmpdir)))
          (make-directory (file-name-directory new) t)
          (with-temp-file new (insert "(\"x\")"))
          (should (equal (agent-repl--history-file-for-read tmpdir) new)))
      (delete-directory tmpdir t))))

(ert-deftest agent-repl-test-history-file-for-read-falls-back-to-legacy ()
  "history-file-for-read returns the legacy project-root path when the
new path does not exist but the legacy one does."
  (let ((tmpdir (make-temp-file "test-hist-read-legacy-" t)))
    (unwind-protect
        (let ((legacy (expand-file-name agent-repl--legacy-history-filename
                                        tmpdir)))
          (with-temp-file legacy (insert "(\"x\")"))
          (should (equal (agent-repl--history-file-for-read tmpdir) legacy)))
      (delete-directory tmpdir t))))

(ert-deftest agent-repl-test-state-file-for-read-falls-back-to-legacy ()
  "state-file-for-read returns the legacy project-root path when the
new path does not exist but the legacy one does."
  (let ((tmpdir (make-temp-file "test-state-read-legacy-" t)))
    (unwind-protect
        (let ((legacy (expand-file-name agent-repl--legacy-state-filename
                                        tmpdir)))
          (with-temp-file legacy (insert "(:project-dir nil)"))
          (should (equal (agent-repl--state-file-for-read tmpdir) legacy)))
      (delete-directory tmpdir t))))

;;;; ---- Tests: history-save path resolution ----

(ert-deftest agent-repl-test-history-save-uses-ws-project-dir ()
  "history-save should compute the history file path from the workspace's
`:project-dir', not from `default-directory' or any buffer-local."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-hist-pdir-" t)))
      (unwind-protect
          (agent-repl-test--with-temp-buffer " *test-hist-save-pdir*"
            (setq-local agent-repl--input-history '("entry1" "entry2"))
            (agent-repl--ws-put "test-ws" :project-dir tmpdir)
            (agent-repl--ws-put "test-ws" :input-buffer (current-buffer))
            (let ((default-directory "/should-not-be-used/"))
              (agent-repl--history-save "test-ws"))
            (should (file-exists-p (agent-repl--history-file tmpdir))))
        (delete-directory tmpdir t)))))

;;;; ---- Tests: history-push with explicit text argument ----

(ert-deftest agent-repl-test-history-push-explicit-text ()
  "history-push with an explicit TEXT argument uses that instead of buffer."
  (agent-repl-test--with-temp-buffer " *test-hist-push-text*"
    (setq-local agent-repl--input-history nil)
    (insert "buffer content")
    (agent-repl--history-push "explicit text")
    (should (equal agent-repl--input-history '("explicit text")))))

(ert-deftest agent-repl-test-history-push-trims-whitespace ()
  "history-push should trim leading/trailing whitespace from text."
  (agent-repl-test--with-temp-buffer " *test-hist-push-trim*"
    (setq-local agent-repl--input-history nil)
    (agent-repl--history-push "  hello  ")
    (should (equal agent-repl--input-history '("hello")))))

(ert-deftest agent-repl-test-history-push-duplicate-after-trim ()
  "history-push should skip duplicates that match after trimming."
  (agent-repl-test--with-temp-buffer " *test-hist-push-dup-trim*"
    (setq-local agent-repl--input-history '("hello"))
    (agent-repl--history-push "  hello  ")
    (should (equal agent-repl--input-history '("hello")))))

;;;; ---- Tests: history-replace-buffer-text ----

(ert-deftest agent-repl-test-history-replace-buffer-text ()
  "history-replace-buffer-text should replace buffer and suppress on-change."
  (agent-repl-test--with-temp-buffer " *test-hist-replace*"
    (setq-local agent-repl--history-navigating nil)
    (setq-local agent-repl--history-index 2)
    (insert "old text")
    ;; Add change hook to check that navigating flag suppresses reset
    (add-hook 'after-change-functions #'agent-repl--history-on-change nil t)
    (agent-repl--history-replace-buffer-text "new text")
    (should (equal (buffer-string) "new text"))
    ;; Index should NOT have been reset because navigating flag was set
    (should (= agent-repl--history-index 2))))

;;;; ---- Tests: history-show-entry ----

(ert-deftest agent-repl-test-history-show-entry-from-history ()
  "history-show-entry with non-negative index shows the history entry."
  (agent-repl-test--with-temp-buffer " *test-hist-show*"
    (setq-local agent-repl--input-history '("newest" "middle" "oldest"))
    (setq-local agent-repl--history-index -1)
    (setq-local agent-repl--history-stash nil)
    (setq-local agent-repl--history-navigating nil)
    (agent-repl--history-show-entry 1)
    (should (equal (buffer-string) "middle"))
    (should (= agent-repl--history-index 1))))

(ert-deftest agent-repl-test-history-show-entry-restores-stash ()
  "history-show-entry with negative index restores the stash."
  (agent-repl-test--with-temp-buffer " *test-hist-show-stash*"
    (setq-local agent-repl--input-history '("entry"))
    (setq-local agent-repl--history-index 0)
    (setq-local agent-repl--history-stash "my stash")
    (setq-local agent-repl--history-navigating nil)
    (agent-repl--history-show-entry -1)
    (should (equal (buffer-string) "my stash"))
    (should (= agent-repl--history-index -1))))

(ert-deftest agent-repl-test-history-show-entry-nil-stash ()
  "history-show-entry with nil stash inserts empty string."
  (agent-repl-test--with-temp-buffer " *test-hist-show-nil*"
    (setq-local agent-repl--input-history '("entry"))
    (setq-local agent-repl--history-index 0)
    (setq-local agent-repl--history-stash nil)
    (setq-local agent-repl--history-navigating nil)
    (agent-repl--history-show-entry -1)
    (should (equal (buffer-string) ""))
    (should (= agent-repl--history-index -1))))

;;;; ---- Tests: history-prev stash behavior ----

(ert-deftest agent-repl-test-history-prev-stashes-only-on-first-nav ()
  "history-prev should only stash buffer text on the first navigation (index -1 -> 0)."
  (agent-repl-test--with-temp-buffer " *test-hist-stash-once*"
    (setq-local agent-repl--input-history '("b" "a"))
    (setq-local agent-repl--history-index -1)
    (setq-local agent-repl--history-stash nil)
    (setq-local agent-repl--history-navigating nil)
    (insert "original")
    ;; First prev: stash should be set
    (agent-repl--history-prev)
    (should (equal agent-repl--history-stash "original"))
    ;; Second prev: stash should NOT be overwritten
    (agent-repl--history-prev)
    (should (equal agent-repl--history-stash "original"))))

;;;; ---- Tests: history-on-change when not browsing ----

(ert-deftest agent-repl-test-history-on-change-noop-when-not-browsing ()
  "history-on-change should do nothing when index is already -1."
  (agent-repl-test--with-temp-buffer " *test-hist-change-noop*"
    (setq-local agent-repl--history-index -1)
    (setq-local agent-repl--history-navigating nil)
    (agent-repl--history-on-change)
    (should (= agent-repl--history-index -1))))

;;;; ---- Tests: with-error-logging ----

(ert-deftest agent-repl-test-with-error-logging-success ()
  "with-error-logging returns body value on success."
  (should (equal (agent-repl--with-error-logging "test" (+ 1 2)) 3)))

(ert-deftest agent-repl-test-with-error-logging-catches-error ()
  "with-error-logging catches errors and does not propagate."
  (should-not
   (condition-case _err
       (agent-repl--with-error-logging "test"
         (error "boom"))
     ;; If the error propagated, this handler would be reached
     (error 'propagated))))

;;;; ---- Tests: read-sexp-file / write-sexp-file ----

(ert-deftest agent-repl-test-write-and-read-sexp-file ()
  "write-sexp-file followed by read-sexp-file round-trips data."
  (let ((file (make-temp-file "test-sexp-")))
    (unwind-protect
        (progn
          (agent-repl--write-sexp-file file '("hello" "world"))
          (should (equal (agent-repl--read-sexp-file file) '("hello" "world"))))
      (delete-file file))))

(ert-deftest agent-repl-test-read-sexp-file-if-exists-present ()
  "read-sexp-file-if-exists returns data when file exists."
  (let ((file (make-temp-file "test-sexp-")))
    (unwind-protect
        (progn
          (agent-repl--write-sexp-file file '(a b c))
          (should (equal (agent-repl--read-sexp-file-if-exists file) '(a b c))))
      (delete-file file))))

(ert-deftest agent-repl-test-read-sexp-file-if-exists-absent ()
  "read-sexp-file-if-exists returns nil when file does not exist."
  (should-not (agent-repl--read-sexp-file-if-exists "/nonexistent/path/file.el")))

;;;; ---- Tests: instantiation serialization ----

(ert-deftest agent-repl-test-instantiation-to-plist-nil ()
  "instantiation-to-plist returns nil for nil input."
  (should-not (agent-repl--instantiation-to-plist nil)))

(ert-deftest agent-repl-test-instantiation-to-plist-basic ()
  "instantiation-to-plist serializes session-id."
  (let ((inst (make-agent-repl-instantiation :session-id "abc-123")))
    (should (equal (agent-repl--instantiation-to-plist inst)
                   '(:session-id "abc-123")))))

(ert-deftest agent-repl-test-instantiation-to-plist-empty-struct ()
  "instantiation-to-plist serializes an empty struct with nil fields."
  (let ((inst (make-agent-repl-instantiation)))
    (should (equal (agent-repl--instantiation-to-plist inst)
                   '(:session-id nil)))))

(ert-deftest agent-repl-test-make-instantiation-from-plist-basic ()
  "make-instantiation-from-plist creates a struct with session-id."
  (let ((inst (agent-repl--make-instantiation-from-plist
               '(:session-id "xyz-789"))))
    (should (agent-repl-instantiation-p inst))
    (should (equal (agent-repl-instantiation-session-id inst) "xyz-789"))))

(ert-deftest agent-repl-test-make-instantiation-from-plist-nil ()
  "make-instantiation-from-plist with nil creates a fresh empty struct."
  (let ((inst (agent-repl--make-instantiation-from-plist nil)))
    (should (agent-repl-instantiation-p inst))
    (should-not (agent-repl-instantiation-session-id inst))))

(ert-deftest agent-repl-test-make-instantiation-from-plist-extra-keys ()
  "make-instantiation-from-plist ignores extra keys in saved plist."
  (let ((inst (agent-repl--make-instantiation-from-plist
               '(:session-id "xyz" :unknown-key "val"))))
    (should (equal (agent-repl-instantiation-session-id inst) "xyz"))))

(ert-deftest agent-repl-test-make-instantiation-from-plist-legacy-had-session ()
  "make-instantiation-from-plist ignores the legacy :had-session key in old state files."
  (let ((inst (agent-repl--make-instantiation-from-plist
               '(:session-id "legacy" :had-session t))))
    (should (agent-repl-instantiation-p inst))
    (should (equal (agent-repl-instantiation-session-id inst) "legacy"))))

;;;; ---- Tests: state-file ----

(ert-deftest agent-repl-test-state-file-with-root ()
  "state-file returns path under the given root."
  (should (equal (agent-repl--state-file "/my/project")
                 (agent-repl--state-file "/my/project"))))

(ert-deftest agent-repl-test-state-file-nil-root ()
  "state-file returns nil when root is nil."
  (should-not (agent-repl--state-file nil)))

;;;; ---- Tests: ws-live-input-buffer ----

(ert-deftest agent-repl-test-ws-live-input-buffer-returns-live ()
  "ws-live-input-buffer returns a live buffer when one exists."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer " *test-live-input*"
      (agent-repl--ws-put "test-ws" :input-buffer (current-buffer))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws")))
        (should (eq (agent-repl--ws-live-input-buffer nil) (current-buffer)))))))

(ert-deftest agent-repl-test-ws-live-input-buffer-returns-nil-for-dead ()
  "ws-live-input-buffer returns nil when the buffer has been killed."
  (agent-repl-test--with-clean-state
    (let ((buf (get-buffer-create " *test-dead-input*")))
      (agent-repl--ws-put "test-ws" :input-buffer buf)
      (kill-buffer buf)
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws")))
        (should-not (agent-repl--ws-live-input-buffer nil))))))

(ert-deftest agent-repl-test-ws-live-input-buffer-nil-when-no-buffer ()
  "ws-live-input-buffer returns nil when no input buffer is set."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws")))
      (should-not (agent-repl--ws-live-input-buffer nil)))))

(ert-deftest agent-repl-test-ws-live-input-buffer-explicit-ws ()
  "ws-live-input-buffer accepts an explicit workspace name."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer " *test-explicit-ws*"
      (agent-repl--ws-put "my-ws" :input-buffer (current-buffer))
      (should (eq (agent-repl--ws-live-input-buffer "my-ws") (current-buffer))))))

;;;; ---- Tests: collect-env-state ----

(ert-deftest agent-repl-test-collect-env-state ()
  "collect-env-state returns plists for each environment key."
  (agent-repl-test--with-clean-state
    (let ((sandbox-inst (make-agent-repl-instantiation :session-id "s1"))
          (bare-inst (make-agent-repl-instantiation :session-id "b1")))
      (agent-repl--ws-put "ws" :sandbox sandbox-inst)
      (agent-repl--ws-put "ws" :bare-metal bare-inst)
      (let ((state (agent-repl--collect-env-state "ws")))
        (should (equal (plist-get state :bare-metal)
                       '(:session-id "b1")))
        (should (equal (plist-get state :sandbox)
                       '(:session-id "s1")))))))

(ert-deftest agent-repl-test-collect-env-state-nil-envs ()
  "collect-env-state returns nil plists when no envs are initialized."
  (agent-repl-test--with-clean-state
    (let ((state (agent-repl--collect-env-state "ws")))
      (should-not (plist-get state :bare-metal))
      (should-not (plist-get state :sandbox)))))

;;;; ---- Tests: state-save ----

(ert-deftest agent-repl-test-state-save-writes-file ()
  "state-save writes state to disk when :project-dir is set."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-state-" t)))
      (unwind-protect
          (progn
            (agent-repl--ws-put "ws" :project-dir tmpdir)
            (agent-repl--ws-put "ws" :active-env :bare-metal)
            (agent-repl--ws-put "ws" :bare-metal
                                 (make-agent-repl-instantiation :session-id "s1"))
            (agent-repl--ws-put "ws" :sandbox
                                 (make-agent-repl-instantiation))
            (agent-repl--state-save "ws")
            (let* ((file (agent-repl--state-file tmpdir))
                   (data (agent-repl--read-sexp-file file)))
              (should (equal (plist-get data :project-dir) tmpdir))
              (should (equal (plist-get data :active-env) :bare-metal))))
        (delete-directory tmpdir t)))))

(ert-deftest agent-repl-test-state-save-skips-when-no-project-dir ()
  "state-save does not write when :project-dir is nil."
  (agent-repl-test--with-clean-state
    ;; No :project-dir set -- should not error
    (agent-repl--state-save "ws")))

(ert-deftest agent-repl-test-state-save-includes-priority ()
  "state-save serializes `:priority' so badges survive restarts."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-state-" t)))
      (unwind-protect
          (progn
            (agent-repl--ws-put "ws" :project-dir tmpdir)
            (agent-repl--ws-put "ws" :active-env :bare-metal)
            (agent-repl--ws-put "ws" :priority "p1")
            (agent-repl--ws-put "ws" :bare-metal (make-agent-repl-instantiation))
            (agent-repl--ws-put "ws" :sandbox (make-agent-repl-instantiation))
            (agent-repl--state-save "ws")
            (let* ((file (agent-repl--state-file tmpdir))
                   (data (agent-repl--read-sexp-file file)))
              (should (equal (plist-get data :priority) "p1"))))
        (delete-directory tmpdir t)))))

(ert-deftest agent-repl-test-state-save-nil-priority ()
  "state-save writes `:priority' nil when no badge is set (no badge == nil)."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-state-" t)))
      (unwind-protect
          (progn
            (agent-repl--ws-put "ws" :project-dir tmpdir)
            (agent-repl--ws-put "ws" :active-env :bare-metal)
            (agent-repl--ws-put "ws" :bare-metal (make-agent-repl-instantiation))
            (agent-repl--ws-put "ws" :sandbox (make-agent-repl-instantiation))
            (agent-repl--state-save "ws")
            (let* ((file (agent-repl--state-file tmpdir))
                   (data (agent-repl--read-sexp-file file)))
              (should (null (plist-get data :priority)))))
        (delete-directory tmpdir t)))))

(ert-deftest agent-repl-test-state-save-includes-last-prompt-time ()
  "state-save serializes `:last-prompt-time' so duration-since-last-message survives restarts."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-state-" t)))
      (unwind-protect
          (progn
            (agent-repl--ws-put "ws" :project-dir tmpdir)
            (agent-repl--ws-put "ws" :active-env :bare-metal)
            (agent-repl--ws-put "ws" :last-prompt-time 1700000000.5)
            (agent-repl--ws-put "ws" :bare-metal (make-agent-repl-instantiation))
            (agent-repl--ws-put "ws" :sandbox (make-agent-repl-instantiation))
            (agent-repl--state-save "ws")
            (let* ((file (agent-repl--state-file tmpdir))
                   (data (agent-repl--read-sexp-file file)))
              (should (equal (plist-get data :last-prompt-time) 1700000000.5))))
        (delete-directory tmpdir t)))))

(ert-deftest agent-repl-test-state-save-includes-source-ws-dir ()
  "state-save serializes `:source-ws-dir' so the merge target survives restarts."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-state-" t)))
      (unwind-protect
          (progn
            (agent-repl--ws-put "ws" :project-dir tmpdir)
            (agent-repl--ws-put "ws" :active-env :bare-metal)
            (agent-repl--ws-put "ws" :source-ws-dir "/tmp/source-repo/")
            (agent-repl--ws-put "ws" :bare-metal (make-agent-repl-instantiation))
            (agent-repl--ws-put "ws" :sandbox (make-agent-repl-instantiation))
            (agent-repl--state-save "ws")
            (let* ((file (agent-repl--state-file tmpdir))
                   (data (agent-repl--read-sexp-file file)))
              (should (equal (plist-get data :source-ws-dir) "/tmp/source-repo/"))))
        (delete-directory tmpdir t)))))

(ert-deftest agent-repl-test-state-save-nil-source-ws-dir ()
  "state-save writes `:source-ws-dir' nil when no source is recorded."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-state-" t)))
      (unwind-protect
          (progn
            (agent-repl--ws-put "ws" :project-dir tmpdir)
            (agent-repl--ws-put "ws" :active-env :bare-metal)
            (agent-repl--ws-put "ws" :bare-metal (make-agent-repl-instantiation))
            (agent-repl--ws-put "ws" :sandbox (make-agent-repl-instantiation))
            (agent-repl--state-save "ws")
            (let* ((file (agent-repl--state-file tmpdir))
                   (data (agent-repl--read-sexp-file file)))
              (should (null (plist-get data :source-ws-dir)))))
        (delete-directory tmpdir t)))))

(ert-deftest agent-repl-test-state-save-includes-saved-tab-index ()
  "state-save serializes `:saved-tab-index' so a deprioritized ws returns
to its prior tab-bar slot after Emacs restart."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-state-tabidx-" t)))
      (unwind-protect
          (progn
            (agent-repl--ws-put "ws" :project-dir tmpdir)
            (agent-repl--ws-put "ws" :active-env :bare-metal)
            (agent-repl--ws-put "ws" :saved-tab-index 3)
            (agent-repl--ws-put "ws" :bare-metal (make-agent-repl-instantiation))
            (agent-repl--ws-put "ws" :sandbox (make-agent-repl-instantiation))
            (agent-repl--state-save "ws")
            (let* ((file (agent-repl--state-file tmpdir))
                   (data (agent-repl--read-sexp-file file)))
              (should (eq (plist-get data :saved-tab-index) 3))))
        (delete-directory tmpdir t)))))

(ert-deftest agent-repl-test-state-save-includes-fork-session-id ()
  "state-save serializes `:fork-session-id' so a fresh fork-ws whose
claude session never started before quit can still launch with
--fork-session on the next restart."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-state-fork-" t)))
      (unwind-protect
          (progn
            (agent-repl--ws-put "ws" :project-dir tmpdir)
            (agent-repl--ws-put "ws" :active-env :bare-metal)
            (agent-repl--ws-put "ws" :fork-session-id "fork-sid-123")
            (agent-repl--ws-put "ws" :bare-metal (make-agent-repl-instantiation))
            (agent-repl--ws-put "ws" :sandbox (make-agent-repl-instantiation))
            (agent-repl--state-save "ws")
            (let* ((file (agent-repl--state-file tmpdir))
                   (data (agent-repl--read-sexp-file file)))
              (should (equal (plist-get data :fork-session-id) "fork-sid-123"))))
        (delete-directory tmpdir t)))))

(ert-deftest agent-repl-test-state-save-includes-last-prompt-summary ()
  "state-save serializes `:last-prompt-summary' so the tabline hint
survives Emacs restart."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-state-summary-" t)))
      (unwind-protect
          (progn
            (agent-repl--ws-put "ws" :project-dir tmpdir)
            (agent-repl--ws-put "ws" :active-env :bare-metal)
            (agent-repl--ws-put "ws" :last-prompt-summary "fix login bug")
            (agent-repl--ws-put "ws" :bare-metal (make-agent-repl-instantiation))
            (agent-repl--ws-put "ws" :sandbox (make-agent-repl-instantiation))
            (agent-repl--state-save "ws")
            (let* ((file (agent-repl--state-file tmpdir))
                   (data (agent-repl--read-sexp-file file)))
              (should (equal (plist-get data :last-prompt-summary) "fix login bug"))))
        (delete-directory tmpdir t)))))

(ert-deftest agent-repl-test-state-save-includes-merge-completed ()
  "state-save serializes `:merge-completed' so a merged ws auto-reappears
in the drawer's MERGED bucket after an Emacs restart, until the user
explicitly `x's it (which routes to `--finish-workspace')."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-state-merged-" t)))
      (unwind-protect
          (progn
            (agent-repl--ws-put "ws" :project-dir tmpdir)
            (agent-repl--ws-put "ws" :active-env :bare-metal)
            (agent-repl--ws-put "ws" :merge-completed t)
            (agent-repl--ws-put "ws" :merge-completed-at 1234567890.0)
            (agent-repl--ws-put "ws" :bare-metal (make-agent-repl-instantiation))
            (agent-repl--ws-put "ws" :sandbox (make-agent-repl-instantiation))
            (agent-repl--state-save "ws")
            (let* ((file (agent-repl--state-file tmpdir))
                   (data (agent-repl--read-sexp-file file)))
              (should (eq (plist-get data :merge-completed) t))
              (should (= (plist-get data :merge-completed-at) 1234567890.0))))
        (delete-directory tmpdir t)))))

(ert-deftest agent-repl-test-state-save-includes-merge-failed ()
  "state-save serializes `:merge-failed' so a silent-failure merge ws
keeps its ❌ badge in the MERGED bucket after restart instead of
regressing to the clean :merged 🔀 badge."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-state-mf-" t)))
      (unwind-protect
          (progn
            (agent-repl--ws-put "ws" :project-dir tmpdir)
            (agent-repl--ws-put "ws" :active-env :bare-metal)
            (agent-repl--ws-put "ws" :merge-completed t)
            (agent-repl--ws-put "ws" :merge-failed t)
            (agent-repl--ws-put "ws" :bare-metal (make-agent-repl-instantiation))
            (agent-repl--ws-put "ws" :sandbox (make-agent-repl-instantiation))
            (agent-repl--state-save "ws")
            (let* ((file (agent-repl--state-file tmpdir))
                   (data (agent-repl--read-sexp-file file)))
              (should (eq (plist-get data :merge-failed) t))))
        (delete-directory tmpdir t)))))

(ert-deftest agent-repl-test-state-save-includes-worktree-p ()
  "state-save serializes `:worktree-p' so a post-restart MERGED entry's
`x' (-> `--finish-workspace') still knows to remove the git worktree."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-state-wtp-" t)))
      (unwind-protect
          (progn
            (agent-repl--ws-put "ws" :project-dir tmpdir)
            (agent-repl--ws-put "ws" :active-env :bare-metal)
            (agent-repl--ws-put "ws" :worktree-p t)
            (agent-repl--ws-put "ws" :bare-metal (make-agent-repl-instantiation))
            (agent-repl--ws-put "ws" :sandbox (make-agent-repl-instantiation))
            (agent-repl--state-save "ws")
            (let* ((file (agent-repl--state-file tmpdir))
                   (data (agent-repl--read-sexp-file file)))
              (should (eq (plist-get data :worktree-p) t))))
        (delete-directory tmpdir t)))))

(ert-deftest agent-repl-test-state-save-includes-repl-state ()
  "state-save serializes `:repl-state' so panel-visibility survives restart."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-state-" t)))
      (unwind-protect
          (progn
            (agent-repl--ws-put "ws" :project-dir tmpdir)
            (agent-repl--ws-put "ws" :active-env :bare-metal)
            (agent-repl--ws-put "ws" :repl-state :inactive)
            (agent-repl--ws-put "ws" :bare-metal (make-agent-repl-instantiation))
            (agent-repl--ws-put "ws" :sandbox (make-agent-repl-instantiation))
            (agent-repl--state-save "ws")
            (let* ((file (agent-repl--state-file tmpdir))
                   (data (agent-repl--read-sexp-file file)))
              (should (eq (plist-get data :repl-state) :inactive))))
        (delete-directory tmpdir t)))))

(ert-deftest agent-repl-test-state-save-includes-model-from-config-dir ()
  "state-save serializes the session's current model resolved from the
config dir, overriding the generation `:model' (opus generated, switched
to fable mid-session), so a restore re-launches under fable."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-state-model-" t)))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-repl--model-for-ws)
                     (lambda (_ws) "claude-fable-5")))
            (agent-repl--ws-put "ws" :project-dir tmpdir)
            (agent-repl--ws-put "ws" :active-env :bare-metal)
            (agent-repl--ws-put "ws" :model "opus")
            (agent-repl--ws-put "ws" :bare-metal (make-agent-repl-instantiation))
            (agent-repl--ws-put "ws" :sandbox (make-agent-repl-instantiation))
            (agent-repl--state-save "ws")
            (let* ((file (agent-repl--state-file tmpdir))
                   (data (agent-repl--read-sexp-file file)))
              (should (equal (plist-get data :model) "claude-fable-5"))))
        (delete-directory tmpdir t)))))

(ert-deftest agent-repl-test-state-save-model-falls-back-to-generation ()
  "state-save persists the generation `:model' when the config dir yields
no session model yet (no assistant turn produced)."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-state-model-" t)))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-repl--model-for-ws)
                     (lambda (_ws) nil)))
            (agent-repl--ws-put "ws" :project-dir tmpdir)
            (agent-repl--ws-put "ws" :active-env :bare-metal)
            (agent-repl--ws-put "ws" :model "opus")
            (agent-repl--ws-put "ws" :bare-metal (make-agent-repl-instantiation))
            (agent-repl--ws-put "ws" :sandbox (make-agent-repl-instantiation))
            (agent-repl--state-save "ws")
            (let* ((file (agent-repl--state-file tmpdir))
                   (data (agent-repl--read-sexp-file file)))
              (should (equal (plist-get data :model) "opus"))))
        (delete-directory tmpdir t)))))

(ert-deftest agent-repl-test-state-save-nil-model ()
  "state-save writes `:model' nil when neither the config dir nor the
generation `:model' yields a model."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-state-model-" t)))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-repl--model-for-ws)
                     (lambda (_ws) nil)))
            (agent-repl--ws-put "ws" :project-dir tmpdir)
            (agent-repl--ws-put "ws" :active-env :bare-metal)
            (agent-repl--ws-put "ws" :bare-metal (make-agent-repl-instantiation))
            (agent-repl--ws-put "ws" :sandbox (make-agent-repl-instantiation))
            (agent-repl--state-save "ws")
            (let* ((file (agent-repl--state-file tmpdir))
                   (data (agent-repl--read-sexp-file file)))
              (should (null (plist-get data :model)))))
        (delete-directory tmpdir t)))))

(ert-deftest agent-repl-test-state-save-piggybacks-snapshot ()
  "state-save also rewrites the workspace snapshot file so the roster
survives a crash that beats kill-emacs-hook.  Roster carries only
`:project-dir' — `:priority' lives in the per-project state file."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-state-" t))
          (snapshot-file (make-temp-file "agent-snap-")))
      (unwind-protect
          (let ((agent-repl-workspace-snapshot-file snapshot-file))
            (agent-repl--ws-put "ws" :project-dir tmpdir)
            (agent-repl--ws-put "ws" :active-env :bare-metal)
            (agent-repl--ws-put "ws" :priority "p3")
            (agent-repl--ws-put "ws" :bare-metal (make-agent-repl-instantiation))
            (agent-repl--ws-put "ws" :sandbox (make-agent-repl-instantiation))
            (agent-repl--state-save "ws")
            (let ((data (plist-get (agent-repl--read-workspace-snapshot snapshot-file)
                                   :workspaces)))
              (should (equal (plist-get (cdr (assoc "ws" data)) :project-dir) tmpdir))
              (should-not (plist-member (cdr (assoc "ws" data)) :priority))))
        (delete-file snapshot-file)
        (delete-directory tmpdir t)))))

(ert-deftest agent-repl-test-state-save-snapshot-error-does-not-block-state ()
  "A snapshot-save failure must not propagate out of state-save (state file
write is the primary obligation; snapshot is the piggyback)."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-state-" t)))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-repl-save-workspace-snapshot)
                     (lambda () (error "boom"))))
            (agent-repl--ws-put "ws" :project-dir tmpdir)
            (agent-repl--ws-put "ws" :active-env :bare-metal)
            (agent-repl--ws-put "ws" :bare-metal (make-agent-repl-instantiation))
            (agent-repl--ws-put "ws" :sandbox (make-agent-repl-instantiation))
            (agent-repl--state-save "ws")
            (let* ((file (agent-repl--state-file tmpdir))
                   (data (agent-repl--read-sexp-file file)))
              (should (equal (plist-get data :project-dir) tmpdir))))
        (delete-directory tmpdir t)))))

;;;; ---- Tests: validate-ws-env ----

(ert-deftest agent-repl-test-validate-ws-env-valid ()
  "validate-ws-env passes for well-formed workspace state."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws" :active-env :bare-metal)
    (agent-repl--ws-put "ws" :bare-metal (make-agent-repl-instantiation))
    (agent-repl--ws-put "ws" :sandbox (make-agent-repl-instantiation))
    ;; Should not error
    (agent-repl--validate-ws-env "ws")))

(ert-deftest agent-repl-test-validate-ws-env-valid-with-session ()
  "validate-ws-env passes when instantiation has a string session-id."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws" :active-env :sandbox)
    (agent-repl--ws-put "ws" :bare-metal (make-agent-repl-instantiation))
    (agent-repl--ws-put "ws" :sandbox
                         (make-agent-repl-instantiation :session-id "abc"))
    (agent-repl--validate-ws-env "ws")))

(ert-deftest agent-repl-test-validate-ws-env-invalid-active-env ()
  "validate-ws-env errors on invalid :active-env."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws" :active-env :bogus)
    (agent-repl--ws-put "ws" :bare-metal (make-agent-repl-instantiation))
    (agent-repl--ws-put "ws" :sandbox (make-agent-repl-instantiation))
    (should-error (agent-repl--validate-ws-env "ws"))))

(ert-deftest agent-repl-test-validate-ws-env-nil-active-env ()
  "validate-ws-env errors when :active-env is nil."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws" :bare-metal (make-agent-repl-instantiation))
    (agent-repl--ws-put "ws" :sandbox (make-agent-repl-instantiation))
    (should-error (agent-repl--validate-ws-env "ws"))))

(ert-deftest agent-repl-test-validate-ws-env-missing-struct ()
  "validate-ws-env errors when an environment has no instantiation struct."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws" :active-env :bare-metal)
    (agent-repl--ws-put "ws" :bare-metal (make-agent-repl-instantiation))
    ;; :sandbox is missing
    (should-error (agent-repl--validate-ws-env "ws"))))

(ert-deftest agent-repl-test-validate-ws-env-invalid-session-id ()
  "validate-ws-env errors when session-id is not a string or nil."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws" :active-env :bare-metal)
    (agent-repl--ws-put "ws" :bare-metal
                         (make-agent-repl-instantiation :session-id 42))
    (agent-repl--ws-put "ws" :sandbox (make-agent-repl-instantiation))
    (should-error (agent-repl--validate-ws-env "ws"))))

;;;; ---- Tests: initialize-ws-env integration ----

(ert-deftest agent-repl-test-initialize-ws-env-restores-from-file ()
  "initialize-ws-env restores full state from disk including :active-env."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-state-" t)))
      (unwind-protect
          (progn
            (agent-repl--write-sexp-file
             (agent-repl--state-file tmpdir)
             '(:project-dir "/restored/root"
               :active-env :sandbox
               :bare-metal (:session-id "bm-id")
               :sandbox (:session-id "sb-id")))
            (agent-repl--ws-put "ws" :project-dir tmpdir)
            (agent-repl--initialize-ws-env "ws")
            (should (eq (agent-repl--ws-get "ws" :active-env) :sandbox))
            (should (equal (agent-repl--ws-get "ws" :project-dir) "/restored/root"))
            (should (equal (agent-repl-instantiation-session-id
                            (agent-repl--ws-get "ws" :bare-metal))
                           "bm-id"))
            (should (equal (agent-repl-instantiation-session-id
                            (agent-repl--ws-get "ws" :sandbox))
                           "sb-id")))
        (delete-directory tmpdir t)))))

(ert-deftest agent-repl-test-initialize-ws-env-fresh-when-no-file ()
  "initialize-ws-env creates fresh defaults when no state file exists."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-state-" t)))
      (unwind-protect
          (progn
            (agent-repl--ws-put "ws" :project-dir tmpdir)
            (agent-repl--initialize-ws-env "ws")
            (should (eq (agent-repl--ws-get "ws" :active-env) :bare-metal))
            (should (agent-repl-instantiation-p (agent-repl--ws-get "ws" :bare-metal)))
            (should (agent-repl-instantiation-p (agent-repl--ws-get "ws" :sandbox)))
            (should-not (agent-repl-instantiation-session-id
                         (agent-repl--ws-get "ws" :bare-metal))))
        (delete-directory tmpdir t)))))

(ert-deftest agent-repl-test-initialize-ws-env-save-restore-round-trip ()
  "state-save followed by initialize-ws-env restores :active-env across restart."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-state-env-" t)))
      (unwind-protect
          (progn
            ;; Simulate pre-restart state: sandbox was active
            (agent-repl--ws-put "ws" :project-dir tmpdir)
            (agent-repl--ws-put "ws" :active-env :sandbox)
            (agent-repl--ws-put "ws" :bare-metal
                                 (make-agent-repl-instantiation :session-id "bm1"))
            (agent-repl--ws-put "ws" :sandbox
                                 (make-agent-repl-instantiation :session-id "sb1"))
            (agent-repl--state-save "ws")
            ;; Simulate post-restart: clear in-memory state
            (clrhash agent-repl--workspaces)
            (agent-repl--ws-put "ws" :project-dir tmpdir)
            (agent-repl--initialize-ws-env "ws")
            ;; :active-env should be restored to :sandbox
            (should (eq (agent-repl--ws-get "ws" :active-env) :sandbox))
            (should (equal (agent-repl-instantiation-session-id
                            (agent-repl--ws-get "ws" :sandbox))
                           "sb1")))
        (delete-directory tmpdir t)))))

(ert-deftest agent-repl-test-initialize-ws-env-restores-source-ws-dir ()
  "initialize-ws-env restores `:source-ws-dir' from the saved state file."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-state-src-" t)))
      (unwind-protect
          (progn
            (agent-repl--write-sexp-file
             (agent-repl--state-file tmpdir)
             '(:project-dir "/restored/root"
               :active-env :bare-metal
               :source-ws-dir "/tmp/recorded-source/"
               :bare-metal (:session-id nil)
               :sandbox (:session-id nil)))
            (agent-repl--ws-put "ws" :project-dir tmpdir)
            (agent-repl--initialize-ws-env "ws")
            (should (equal (agent-repl--ws-get "ws" :source-ws-dir)
                           "/tmp/recorded-source/")))
        (delete-directory tmpdir t)))))

(ert-deftest agent-repl-test-initialize-ws-env-old-state-file-no-source-ws-dir ()
  "Old state files (no `:source-ws-dir' key) leave the value as nil — no error."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-state-old-" t)))
      (unwind-protect
          (progn
            (agent-repl--write-sexp-file
             (agent-repl--state-file tmpdir)
             '(:project-dir "/restored/root"
               :active-env :bare-metal
               :bare-metal (:session-id nil)
               :sandbox (:session-id nil)))
            (agent-repl--ws-put "ws" :project-dir tmpdir)
            (agent-repl--initialize-ws-env "ws")
            (should (null (agent-repl--ws-get "ws" :source-ws-dir))))
        (delete-directory tmpdir t)))))

(ert-deftest agent-repl-test-initialize-ws-env-source-ws-dir-round-trip ()
  "state-save followed by initialize-ws-env restores `:source-ws-dir' across restart."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-state-src-rt-" t)))
      (unwind-protect
          (progn
            (agent-repl--ws-put "ws" :project-dir tmpdir)
            (agent-repl--ws-put "ws" :active-env :bare-metal)
            (agent-repl--ws-put "ws" :source-ws-dir "/tmp/source-roundtrip/")
            (agent-repl--ws-put "ws" :bare-metal (make-agent-repl-instantiation))
            (agent-repl--ws-put "ws" :sandbox (make-agent-repl-instantiation))
            (agent-repl--state-save "ws")
            ;; Simulate restart
            (clrhash agent-repl--workspaces)
            (agent-repl--ws-put "ws" :project-dir tmpdir)
            (agent-repl--initialize-ws-env "ws")
            (should (equal (agent-repl--ws-get "ws" :source-ws-dir)
                           "/tmp/source-roundtrip/")))
        (delete-directory tmpdir t)))))

;;;; ---- Tests: history-save / history-restore round-trip ----

(ert-deftest agent-repl-test-history-save-and-restore-round-trip ()
  "history-save writes and history-restore reads back the same data."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-hist-" t)))
      (unwind-protect
          (agent-repl-test--with-temp-buffer " *test-hist-round-trip*"
            (setq-local default-directory tmpdir)
            (setq-local agent-repl--input-history '("third" "second" "first"))
            (agent-repl--ws-put "test-ws" :project-dir tmpdir)
            (agent-repl--ws-put "test-ws" :input-buffer (current-buffer))
            ;; Save
            (agent-repl--history-save "test-ws")
            ;; Clear and restore
            (setq-local agent-repl--input-history nil)
            (agent-repl--history-restore "test-ws")
            (should (equal agent-repl--input-history '("third" "second" "first"))))
        (delete-directory tmpdir t)))))

(ert-deftest agent-repl-test-history-save-skips-nil-history ()
  "history-save should not write when history is nil."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-hist-" t)))
      (unwind-protect
          (agent-repl-test--with-temp-buffer " *test-hist-save-nil*"
            (setq-local agent-repl--input-history nil)
            (agent-repl--ws-put "test-ws" :project-dir tmpdir)
            (agent-repl--ws-put "test-ws" :input-buffer (current-buffer))
            (agent-repl--history-save "test-ws")
            ;; File should not exist
            (should-not (file-exists-p
                         (agent-repl--history-file tmpdir))))
        (delete-directory tmpdir t)))))

(ert-deftest agent-repl-test-history-save-skips-dead-buffer ()
  "history-save should do nothing when the input buffer is dead."
  (agent-repl-test--with-clean-state
    (let ((buf (get-buffer-create " *test-hist-dead*")))
      (with-current-buffer buf
        (setq-local agent-repl--input-history '("entry")))
      (agent-repl--ws-put "test-ws" :input-buffer buf)
      (kill-buffer buf)
      ;; Should not error
      (agent-repl--history-save "test-ws"))))

;;;; ---- Tests: environment-keys constant ----

(ert-deftest agent-repl-test-environment-keys-value ()
  "environment-keys should contain :bare-metal and :sandbox."
  (should (memq :bare-metal agent-repl--environment-keys))
  (should (memq :sandbox agent-repl--environment-keys)))

;;;; ---- Tests: history-restore with no file ----

(ert-deftest agent-repl-test-history-restore-no-file ()
  "history-restore leaves history nil when no file exists."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer " *test-hist-restore-none*"
      (setq-local agent-repl--input-history nil)
      (agent-repl--ws-put "test-ws" :project-dir "/nonexistent/path")
      (agent-repl--history-restore "test-ws")
      (should-not agent-repl--input-history))))

;;;; ---- Tests: with-error-logging edge cases ----

(ert-deftest agent-repl-test-with-error-logging-body-returns-nil ()
  "with-error-logging returns nil from body without confusing it with error."
  (should (eq (agent-repl--with-error-logging "test" nil) nil)))

(ert-deftest agent-repl-test-with-error-logging-multiple-forms ()
  "with-error-logging returns the value of the last form in body."
  (should (equal (agent-repl--with-error-logging "test"
                   (+ 1 2)
                   (* 3 4))
                 12)))

(ert-deftest agent-repl-test-with-error-logging-calls-log-on-error ()
  "with-error-logging calls `agent-repl--log' with label on error."
  (let ((logged-msg nil))
    (cl-letf (((symbol-function 'agent-repl--log)
               (lambda (ws fmt &rest args)
                 (setq logged-msg (apply #'format fmt args)))))
      (agent-repl--with-error-logging "my-label"
        (error "kaboom"))
      (should (stringp logged-msg))
      (should (string-match-p "my-label" logged-msg)))))

;;;; ---- Tests: read-sexp-file edge cases ----

(ert-deftest agent-repl-test-read-sexp-file-multiple-sexps ()
  "read-sexp-file returns only the first sexp from a file with multiple."
  (let ((file (make-temp-file "test-sexp-multi-")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "(first-sexp)\n(second-sexp)"))
          (should (equal (agent-repl--read-sexp-file file) '(first-sexp))))
      (delete-file file))))

(ert-deftest agent-repl-test-read-sexp-file-complex-nested ()
  "read-sexp-file handles complex nested data structures."
  (let ((file (make-temp-file "test-sexp-nested-"))
        (data '((:key1 . "val1") (:key2 . [1 2 3]) (:key3 . (:a 1 :b 2)))))
    (unwind-protect
        (progn
          (agent-repl--write-sexp-file file data)
          (should (equal (agent-repl--read-sexp-file file) data)))
      (delete-file file))))

(ert-deftest agent-repl-test-read-sexp-file-empty-file ()
  "read-sexp-file signals an error on an empty file."
  (let ((file (make-temp-file "test-sexp-empty-")))
    (unwind-protect
        (progn
          ;; File exists but is empty (make-temp-file creates empty file)
          (should-error (agent-repl--read-sexp-file file)))
      (delete-file file))))

;;;; ---- Tests: write-sexp-file edge cases ----

(ert-deftest agent-repl-test-write-sexp-file-nil-value ()
  "write-sexp-file can write nil and read it back."
  (let ((file (make-temp-file "test-sexp-nil-")))
    (unwind-protect
        (progn
          (agent-repl--write-sexp-file file nil)
          (should (eq (agent-repl--read-sexp-file file) nil)))
      (delete-file file))))

(ert-deftest agent-repl-test-write-sexp-file-special-characters ()
  "write-sexp-file handles strings with quotes and newlines."
  (let ((file (make-temp-file "test-sexp-special-"))
        (data '("a string with \"quotes\"" "and\nnewlines")))
    (unwind-protect
        (progn
          (agent-repl--write-sexp-file file data)
          (should (equal (agent-repl--read-sexp-file file) data)))
      (delete-file file))))

(ert-deftest agent-repl-test-write-sexp-file-nonexistent-parent ()
  "write-sexp-file errors when parent directory does not exist."
  (should-error
   (agent-repl--write-sexp-file "/nonexistent/parent/dir/file.el" '(data))))

;;;; ---- Tests: read-sexp-file-if-exists edge cases ----

(ert-deftest agent-repl-test-read-sexp-file-if-exists-empty-file ()
  "read-sexp-file-if-exists errors when file exists but is empty."
  (let ((file (make-temp-file "test-sexp-if-exists-empty-")))
    (unwind-protect
        (should-error (agent-repl--read-sexp-file-if-exists file))
      (delete-file file))))

;;;; ---- Tests: instantiation-to-plist edge cases ----

(ert-deftest agent-repl-test-instantiation-to-plist-omits-start-cmd ()
  "instantiation-to-plist does NOT include start-cmd in output."
  (let* ((inst (make-agent-repl-instantiation
                :session-id "abc"
                :start-cmd "claude --resume"))
         (plist (agent-repl--instantiation-to-plist inst)))
    (should (equal plist '(:session-id "abc")))
    (should-not (plist-member plist :start-cmd))))

;;;; ---- Tests: make-instantiation-from-plist edge cases ----
;; (basic, nil, extra-keys, and partial-keys tests are in the main section above)

;;;; ---- Tests: ws-live-input-buffer edge cases ----

(ert-deftest agent-repl-test-ws-live-input-buffer-both-ws-nil ()
  "ws-live-input-buffer returns nil when WS is nil and +workspace-current-name returns nil."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () nil)))
      (should-not (agent-repl--ws-live-input-buffer nil)))))

;;;; ---- Tests: history-save edge cases ----

(ert-deftest agent-repl-test-history-save-single-entry ()
  "history-save writes and reads back a single-entry history list."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-hist-single-" t)))
      (unwind-protect
          (agent-repl-test--with-temp-buffer " *test-hist-single*"
            (setq-local agent-repl--input-history '("only-one"))
            (agent-repl--ws-put "test-ws" :project-dir tmpdir)
            (agent-repl--ws-put "test-ws" :input-buffer (current-buffer))
            (agent-repl--history-save "test-ws")
            ;; Read back and verify
            (let ((file (agent-repl--history-file tmpdir)))
              (should (equal (agent-repl--read-sexp-file file) '("only-one")))))
        (delete-directory tmpdir t)))))

;;;; ---- Tests: history-restore edge cases ----

(ert-deftest agent-repl-test-history-restore-non-list-data ()
  "history-restore sets history to whatever is in the file, even non-list."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-hist-nonlist-" t)))
      (unwind-protect
          (agent-repl-test--with-temp-buffer " *test-hist-nonlist*"
            (setq-local agent-repl--input-history nil)
            (agent-repl--ws-put "test-ws" :project-dir tmpdir)
            ;; Write a string (non-list) to the history file
            (agent-repl--write-sexp-file
             (agent-repl--history-file tmpdir) "just-a-string")
            (agent-repl--history-restore "test-ws")
            (should (equal agent-repl--input-history "just-a-string")))
        (delete-directory tmpdir t)))))

(ert-deftest agent-repl-test-history-restore-overwrites-existing ()
  "history-restore overwrites existing history, does not append."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-hist-overwrite-" t)))
      (unwind-protect
          (agent-repl-test--with-temp-buffer " *test-hist-overwrite*"
            (setq-local agent-repl--input-history '("old1" "old2"))
            (agent-repl--ws-put "test-ws" :project-dir tmpdir)
            (agent-repl--write-sexp-file
             (agent-repl--history-file tmpdir) '("new1"))
            (agent-repl--history-restore "test-ws")
            (should (equal agent-repl--input-history '("new1"))))
        (delete-directory tmpdir t)))))

;;;; ---- Tests: collect-env-state edge cases ----

(ert-deftest agent-repl-test-collect-env-state-partial ()
  "collect-env-state handles only one environment initialized."
  (agent-repl-test--with-clean-state
    (let ((inst (make-agent-repl-instantiation :session-id "s1")))
      (agent-repl--ws-put "ws" :sandbox inst)
      ;; :bare-metal is not set
      (let ((state (agent-repl--collect-env-state "ws")))
        (should (equal (plist-get state :sandbox)
                       '(:session-id "s1")))
        (should-not (plist-get state :bare-metal))))))

;;;; ---- Tests: state-save edge cases ----

(ert-deftest agent-repl-test-state-save-nil-active-env ()
  "state-save serializes :active-env as nil when it is unset."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-state-nil-env-" t)))
      (unwind-protect
          (progn
            (agent-repl--ws-put "ws" :project-dir tmpdir)
            ;; :active-env is NOT set (defaults to nil)
            (agent-repl--ws-put "ws" :bare-metal (make-agent-repl-instantiation))
            (agent-repl--ws-put "ws" :sandbox (make-agent-repl-instantiation))
            (agent-repl--state-save "ws")
            (let* ((file (agent-repl--state-file tmpdir))
                   (data (agent-repl--read-sexp-file file)))
              (should (plist-member data :active-env))
              (should-not (plist-get data :active-env))))
        (delete-directory tmpdir t)))))

(ert-deftest agent-repl-test-state-save-write-error-caught ()
  "state-save catches write errors via with-error-logging."
  (agent-repl-test--with-clean-state
    ;; Point :project-dir to a nonexistent directory to trigger write error
    (agent-repl--ws-put "ws" :project-dir "/nonexistent/dir/for/test")
    (agent-repl--ws-put "ws" :bare-metal (make-agent-repl-instantiation))
    (agent-repl--ws-put "ws" :sandbox (make-agent-repl-instantiation))
    ;; Should not signal an error thanks to with-error-logging
    (agent-repl--state-save "ws")))

;;;; ---- Tests: initialize-ws-env with missing/corrupt state files ----

(ert-deftest agent-repl-test-initialize-ws-env-empty-file-creates-fresh ()
  "initialize-ws-env creates fresh state when state file is empty."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-state-empty-" t)))
      (unwind-protect
          (progn
            (agent-repl-test--seed-file (agent-repl--state-file tmpdir) "")
            (agent-repl--ws-put "ws" :project-dir tmpdir)
            (agent-repl--initialize-ws-env "ws")
            (should (eq (agent-repl--ws-get "ws" :active-env) :bare-metal))
            (should (agent-repl-instantiation-p
                     (agent-repl--ws-get "ws" :bare-metal)))
            (should-not (agent-repl-instantiation-session-id
                         (agent-repl--ws-get "ws" :bare-metal))))
        (delete-directory tmpdir t)))))

(ert-deftest agent-repl-test-initialize-ws-env-invalid-elisp-creates-fresh ()
  "initialize-ws-env creates fresh state when state file has unreadable elisp."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-state-invalid-" t)))
      (unwind-protect
          (progn
            ;; Unclosed paren triggers end-of-file error in (read ...)
            (agent-repl-test--seed-file (agent-repl--state-file tmpdir) "(unclosed paren")
            (agent-repl--ws-put "ws" :project-dir tmpdir)
            (agent-repl--initialize-ws-env "ws")
            (should (eq (agent-repl--ws-get "ws" :active-env) :bare-metal))
            (should (agent-repl-instantiation-p
                     (agent-repl--ws-get "ws" :bare-metal)))
            (should-not (agent-repl-instantiation-session-id
                         (agent-repl--ws-get "ws" :bare-metal))))
        (delete-directory tmpdir t)))))

(ert-deftest agent-repl-test-initialize-ws-env-missing-file-writes-state ()
  "initialize-ws-env creates state file on disk when it was missing."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-state-write-" t)))
      (unwind-protect
          (let ((state-path (agent-repl--state-file (agent-repl--path-canonical tmpdir))))
            (agent-repl--ws-put "ws" :project-dir tmpdir)
            (should-not (file-exists-p state-path))
            (agent-repl--initialize-ws-env "ws")
            (should (file-exists-p state-path))
            ;; Verify the written file is valid and round-trips
            (let ((data (agent-repl--read-sexp-file state-path)))
              (should (eq (plist-get data :active-env) :bare-metal))
              (should (equal (plist-get data :project-dir)
                             (agent-repl--path-canonical tmpdir)))))
        (delete-directory tmpdir t)))))

;;;; ---- Tests: history-push edge cases ----

(ert-deftest agent-repl-test-history-push-internal-whitespace-differs ()
  "history-push adds entry that differs from head only in internal whitespace."
  (agent-repl-test--with-temp-buffer " *test-hist-push-internal-ws*"
    (setq-local agent-repl--input-history '("hello world"))
    (agent-repl--history-push "hello  world")
    (should (equal agent-repl--input-history '("hello  world" "hello world")))))

(ert-deftest agent-repl-test-history-push-matches-non-first ()
  "history-push adds entry matching a non-first history entry (only head is checked)."
  (agent-repl-test--with-temp-buffer " *test-hist-push-nonfirst*"
    (setq-local agent-repl--input-history '("latest" "older" "oldest"))
    (agent-repl--history-push "older")
    (should (equal agent-repl--input-history '("older" "latest" "older" "oldest")))))

;;;; ---- Tests: history-replace-buffer-text edge cases ----

(ert-deftest agent-repl-test-history-replace-buffer-text-empty-string ()
  "history-replace-buffer-text with empty string leaves buffer empty."
  (agent-repl-test--with-temp-buffer " *test-hist-replace-empty*"
    (setq-local agent-repl--history-navigating nil)
    (insert "some text")
    (agent-repl--history-replace-buffer-text "")
    (should (equal (buffer-string) ""))))

(ert-deftest agent-repl-test-history-replace-buffer-text-already-empty ()
  "history-replace-buffer-text in already-empty buffer inserts new text."
  (agent-repl-test--with-temp-buffer " *test-hist-replace-was-empty*"
    (setq-local agent-repl--history-navigating nil)
    ;; Buffer starts empty
    (should (equal (buffer-string) ""))
    (agent-repl--history-replace-buffer-text "new text")
    (should (equal (buffer-string) "new text"))))

;;;; ---- Tests: history-show-entry edge cases ----

(ert-deftest agent-repl-test-history-show-entry-last-valid-index ()
  "history-show-entry at the last valid index shows the oldest entry."
  (agent-repl-test--with-temp-buffer " *test-hist-show-last*"
    (setq-local agent-repl--input-history '("newest" "middle" "oldest"))
    (setq-local agent-repl--history-index -1)
    (setq-local agent-repl--history-stash nil)
    (setq-local agent-repl--history-navigating nil)
    (agent-repl--history-show-entry 2)
    (should (equal (buffer-string) "oldest"))
    (should (= agent-repl--history-index 2))))

;;;; ---- Tests: history-prev edge cases ----

(ert-deftest agent-repl-test-history-prev-single-entry ()
  "history-prev with single-entry history: first prev goes to 0, second stays at 0."
  (agent-repl-test--with-temp-buffer " *test-hist-prev-single*"
    (setq-local agent-repl--input-history '("only"))
    (setq-local agent-repl--history-index -1)
    (setq-local agent-repl--history-stash nil)
    (setq-local agent-repl--history-navigating nil)
    (insert "current")
    ;; First prev: goes to index 0
    (agent-repl--history-prev)
    (should (= agent-repl--history-index 0))
    (should (equal (buffer-string) "only"))
    (should (equal agent-repl--history-stash "current"))
    ;; Second prev: stays at index 0 (no older entry)
    (agent-repl--history-prev)
    (should (= agent-repl--history-index 0))
    (should (equal (buffer-string) "only"))))

;;;; ---- Tests: history-next edge cases ----

(ert-deftest agent-repl-test-history-next-from-index-zero ()
  "history-next from index 0 goes to -1 and restores stash."
  (agent-repl-test--with-temp-buffer " *test-hist-next-from-zero*"
    (setq-local agent-repl--input-history '("entry"))
    (setq-local agent-repl--history-index 0)
    (setq-local agent-repl--history-stash "my stash")
    (setq-local agent-repl--history-navigating nil)
    (insert "entry")
    (agent-repl--history-next)
    (should (= agent-repl--history-index -1))
    (should (equal (buffer-string) "my stash"))))

;;;; ---- Tests: history-on-change edge cases ----

(ert-deftest agent-repl-test-history-on-change-ignores-extra-args ()
  "history-on-change ignores extra arguments (the &rest _args)."
  (agent-repl-test--with-temp-buffer " *test-hist-change-args*"
    (setq-local agent-repl--history-index 2)
    (setq-local agent-repl--history-navigating nil)
    ;; Call with extra arguments like after-change-functions would
    (agent-repl--history-on-change 1 5 0)
    (should (= agent-repl--history-index -1))))

;;;; ---- Tests: history-search candidates ----

(ert-deftest agent-repl-test-history-format-candidate-basic ()
  "history-format-candidate prepends the index and preserves single-line text."
  (let ((label (agent-repl--history-format-candidate "hello" 2)))
    (should (string-match-p "\\`[[:space:]]*2" label))
    (should (string-match-p "hello\\'" label))))

(ert-deftest agent-repl-test-history-format-candidate-collapses-newlines ()
  "history-format-candidate collapses newlines so multi-line entries stay on one line."
  (let ((label (agent-repl--history-format-candidate "first\nsecond" 0)))
    (should-not (string-match-p "\n" label))
    (should (string-match-p "first" label))
    (should (string-match-p "second" label))))

(ert-deftest agent-repl-test-history-search-candidates-labels-and-indices ()
  "history-search-candidates returns (LABEL . INDEX) pairs in history order."
  (agent-repl-test--with-temp-buffer " *test-hist-search-cand*"
    (setq-local agent-repl--input-history '("newest" "older" "oldest"))
    (let ((cands (agent-repl--history-search-candidates)))
      (should (= (length cands) 3))
      (should (= (cdar cands) 0))
      (should (= (cdr (nth 1 cands)) 1))
      (should (= (cdr (nth 2 cands)) 2))
      (should (string-match-p "newest" (caar cands)))
      (should (string-match-p "oldest" (car (nth 2 cands)))))))

(ert-deftest agent-repl-test-history-search-candidates-empty ()
  "history-search-candidates returns nil for empty history."
  (agent-repl-test--with-temp-buffer " *test-hist-search-empty*"
    (setq-local agent-repl--input-history nil)
    (should-not (agent-repl--history-search-candidates))))

(ert-deftest agent-repl-test-history-search-candidates-duplicates-unique ()
  "history-search-candidates produces unique labels even for duplicate entries."
  (agent-repl-test--with-temp-buffer " *test-hist-search-dup*"
    (setq-local agent-repl--input-history '("same" "same"))
    (let* ((cands (agent-repl--history-search-candidates))
           (labels (mapcar #'car cands)))
      (should (= (length labels) (length (delete-dups (copy-sequence labels)))))) ))

;;;; ---- Tests: history-search ----

(ert-deftest agent-repl-test-history-search-replaces-buffer-with-selection ()
  "history-search replaces buffer with the chosen entry and updates the index."
  (agent-repl-test--with-temp-buffer " *test-hist-search-pick*"
    (setq-local agent-repl--input-history '("newest" "middle" "oldest"))
    (setq-local agent-repl--history-index -1)
    (setq-local agent-repl--history-stash nil)
    (setq-local agent-repl--history-navigating nil)
    (insert "draft")
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _)
                 ;; Pick the "middle" candidate (index 1)
                 (nth 1 collection))))
      (agent-repl-history-search))
    (should (equal (buffer-string) "middle"))
    (should (= agent-repl--history-index 1))))

(ert-deftest agent-repl-test-history-search-stashes-buffer-on-first-nav ()
  "history-search stashes the in-progress buffer text when called from a fresh index."
  (agent-repl-test--with-temp-buffer " *test-hist-search-stash*"
    (setq-local agent-repl--input-history '("entry"))
    (setq-local agent-repl--history-index -1)
    (setq-local agent-repl--history-stash nil)
    (setq-local agent-repl--history-navigating nil)
    (insert "in progress")
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _) (car collection))))
      (agent-repl-history-search))
    (should (equal agent-repl--history-stash "in progress"))))

(ert-deftest agent-repl-test-history-search-preserves-existing-stash ()
  "history-search does not overwrite an existing stash when already browsing."
  (agent-repl-test--with-temp-buffer " *test-hist-search-keep-stash*"
    (setq-local agent-repl--input-history '("a" "b"))
    (setq-local agent-repl--history-index 0)
    (setq-local agent-repl--history-stash "original")
    (setq-local agent-repl--history-navigating nil)
    (insert "a")
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _) (nth 1 collection))))
      (agent-repl-history-search))
    (should (equal agent-repl--history-stash "original"))
    (should (= agent-repl--history-index 1))
    (should (equal (buffer-string) "b"))))

(ert-deftest agent-repl-test-history-search-empty-history-is-noop ()
  "history-search with empty history does not call completing-read and leaves buffer untouched."
  (agent-repl-test--with-temp-buffer " *test-hist-search-empty-noop*"
    (setq-local agent-repl--input-history nil)
    (setq-local agent-repl--history-index -1)
    (setq-local agent-repl--history-stash nil)
    (insert "draft text")
    (let ((called nil))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) (setq called t) "")))
        (agent-repl-history-search))
      (should-not called))
    (should (equal (buffer-string) "draft text"))
    (should (= agent-repl--history-index -1))))

;;;; ---- Tests: state-save :created-at / :last-killed-at ----

(ert-deftest agent-repl-test-state-save-stamps-created-at-on-first-save ()
  "state-save sets `:created-at' on the ws plist and persists it when no
existing state file is present."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-state-created-" t)))
      (unwind-protect
          (progn
            (agent-repl--ws-put "ws" :project-dir tmpdir)
            (agent-repl--ws-put "ws" :active-env :bare-metal)
            (agent-repl--ws-put "ws" :bare-metal (make-agent-repl-instantiation))
            (agent-repl--ws-put "ws" :sandbox (make-agent-repl-instantiation))
            (agent-repl--state-save "ws")
            (let* ((file (agent-repl--state-file tmpdir))
                   (data (agent-repl--read-sexp-file file)))
              (should (plist-get data :created-at))
              (should (agent-repl--ws-get "ws" :created-at))))
        (delete-directory tmpdir t)))))

(ert-deftest agent-repl-test-state-save-preserves-existing-created-at ()
  "state-save keeps the `:created-at' written by an earlier save instead
of stamping a fresh timestamp on every write."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-state-created-existing-" t))
          (original '(22000 0 0 0)))
      (unwind-protect
          (progn
            (agent-repl--write-sexp-file
             (agent-repl--state-file tmpdir)
             `(:project-dir ,tmpdir :created-at ,original))
            (agent-repl--ws-put "ws" :project-dir tmpdir)
            (agent-repl--ws-put "ws" :active-env :bare-metal)
            (agent-repl--ws-put "ws" :bare-metal (make-agent-repl-instantiation))
            (agent-repl--ws-put "ws" :sandbox (make-agent-repl-instantiation))
            (agent-repl--state-save "ws")
            (let* ((file (agent-repl--state-file tmpdir))
                   (data (agent-repl--read-sexp-file file)))
              (should (equal (plist-get data :created-at) original))))
        (delete-directory tmpdir t)))))

(ert-deftest agent-repl-test-state-save-includes-last-killed-at ()
  "state-save serializes `:last-killed-at' when the ws plist carries it
\(populated by `agent-repl--nuke-one-workspace')."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-state-killed-" t))
          (killed '(23000 0 0 0)))
      (unwind-protect
          (progn
            (agent-repl--ws-put "ws" :project-dir tmpdir)
            (agent-repl--ws-put "ws" :active-env :bare-metal)
            (agent-repl--ws-put "ws" :last-killed-at killed)
            (agent-repl--ws-put "ws" :bare-metal (make-agent-repl-instantiation))
            (agent-repl--ws-put "ws" :sandbox (make-agent-repl-instantiation))
            (agent-repl--state-save "ws")
            (let* ((file (agent-repl--state-file tmpdir))
                   (data (agent-repl--read-sexp-file file)))
              (should (equal (plist-get data :last-killed-at) killed))))
        (delete-directory tmpdir t)))))

(ert-deftest agent-repl-test-state-save-preserves-existing-last-killed-at ()
  "state-save carries forward `:last-killed-at' from the on-disk file when
the ws plist has no value, so non-kill saves don't clear the field."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-state-killed-existing-" t))
          (killed '(24000 0 0 0)))
      (unwind-protect
          (progn
            (agent-repl--write-sexp-file
             (agent-repl--state-file tmpdir)
             `(:project-dir ,tmpdir :last-killed-at ,killed))
            (agent-repl--ws-put "ws" :project-dir tmpdir)
            (agent-repl--ws-put "ws" :active-env :bare-metal)
            (agent-repl--ws-put "ws" :bare-metal (make-agent-repl-instantiation))
            (agent-repl--ws-put "ws" :sandbox (make-agent-repl-instantiation))
            (agent-repl--state-save "ws")
            (let* ((file (agent-repl--state-file tmpdir))
                   (data (agent-repl--read-sexp-file file)))
              (should (equal (plist-get data :last-killed-at) killed))))
        (delete-directory tmpdir t)))))

(ert-deftest agent-repl-test-state-save-nil-last-killed-at-when-never-killed ()
  "state-save writes `:last-killed-at' nil for projects that have never
been killed (both the ws plist and the existing file lack the field)."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-state-killed-nil-" t)))
      (unwind-protect
          (progn
            (agent-repl--ws-put "ws" :project-dir tmpdir)
            (agent-repl--ws-put "ws" :active-env :bare-metal)
            (agent-repl--ws-put "ws" :bare-metal (make-agent-repl-instantiation))
            (agent-repl--ws-put "ws" :sandbox (make-agent-repl-instantiation))
            (agent-repl--state-save "ws")
            (let* ((file (agent-repl--state-file tmpdir))
                   (data (agent-repl--read-sexp-file file)))
              (should (null (plist-get data :last-killed-at)))))
        (delete-directory tmpdir t)))))

(provide 'test-history)

;;; test-history.el ends here
