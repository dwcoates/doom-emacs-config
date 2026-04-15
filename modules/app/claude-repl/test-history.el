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
  (cl-letf (((symbol-function 'claude-repl--resolve-root) (lambda () "/test/root")))
    (should (equal (claude-repl--history-file)
                   (expand-file-name ".claude-repl-history" "/test/root")))))

;;;; ---- Tests: Bug 8 - history-save uses input buffer root ----

(ert-deftest claude-repl-test-bug8-history-save-uses-input-buffer-root ()
  "Bug 8: history-save should use the input buffer's project root."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-buffer " *test-hist-save*"
      (setq-local claude-repl--project-root "/input-buffer-root")
      (setq-local claude-repl--input-history '("entry1" "entry2"))
      (claude-repl--ws-put "test-ws" :input-buffer (current-buffer))
      ;; Verify the file path computed from the input buffer's project root
      (let* ((buf (claude-repl--ws-get "test-ws" :input-buffer))
             (root (buffer-local-value 'claude-repl--project-root buf))
             (file (expand-file-name ".claude-repl-history" root)))
        (should (string-match-p "/input-buffer-root" file))))))

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
  "instantiation-to-plist serializes session-id and had-session."
  (let ((inst (make-claude-repl-instantiation
               :session-id "abc-123"
               :had-session t)))
    (should (equal (claude-repl--instantiation-to-plist inst)
                   '(:session-id "abc-123" :had-session t)))))

(ert-deftest claude-repl-test-instantiation-to-plist-empty-struct ()
  "instantiation-to-plist serializes an empty struct with nil fields."
  (let ((inst (make-claude-repl-instantiation)))
    (should (equal (claude-repl--instantiation-to-plist inst)
                   '(:session-id nil :had-session nil)))))

(ert-deftest claude-repl-test-instantiation-from-plist-restores ()
  "instantiation-from-plist restores session-id and had-session."
  (let ((inst (make-claude-repl-instantiation)))
    (claude-repl--instantiation-from-plist
     inst '(:session-id "xyz-789" :had-session t))
    (should (equal (claude-repl-instantiation-session-id inst) "xyz-789"))
    (should (eq (claude-repl-instantiation-had-session inst) t))))

(ert-deftest claude-repl-test-instantiation-from-plist-nil-inst ()
  "instantiation-from-plist does nothing when inst is nil."
  ;; Should not error
  (claude-repl--instantiation-from-plist nil '(:session-id "abc" :had-session t)))

(ert-deftest claude-repl-test-instantiation-from-plist-nil-saved ()
  "instantiation-from-plist does nothing when saved is nil."
  (let ((inst (make-claude-repl-instantiation :session-id "original")))
    (claude-repl--instantiation-from-plist inst nil)
    ;; Should remain unchanged
    (should (equal (claude-repl-instantiation-session-id inst) "original"))))

(ert-deftest claude-repl-test-instantiation-from-plist-both-nil ()
  "instantiation-from-plist does nothing when both args are nil."
  (claude-repl--instantiation-from-plist nil nil))

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
    (let ((sandbox-inst (make-claude-repl-instantiation :session-id "s1" :had-session t))
          (bare-inst (make-claude-repl-instantiation :session-id "b1" :had-session nil)))
      (claude-repl--ws-put "ws" :sandbox sandbox-inst)
      (claude-repl--ws-put "ws" :bare-metal bare-inst)
      (let ((state (claude-repl--collect-env-state "ws")))
        (should (equal (plist-get state :bare-metal)
                       '(:session-id "b1" :had-session nil)))
        (should (equal (plist-get state :sandbox)
                       '(:session-id "s1" :had-session t)))))))

(ert-deftest claude-repl-test-collect-env-state-nil-envs ()
  "collect-env-state returns nil plists when no envs are initialized."
  (claude-repl-test--with-clean-state
    (let ((state (claude-repl--collect-env-state "ws")))
      (should-not (plist-get state :bare-metal))
      (should-not (plist-get state :sandbox)))))

;;;; ---- Tests: restore-env-state ----

(ert-deftest claude-repl-test-restore-env-state ()
  "restore-env-state populates instantiation structs from saved state."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws" :bare-metal (make-claude-repl-instantiation))
    (claude-repl--ws-put "ws" :sandbox (make-claude-repl-instantiation))
    (claude-repl--restore-env-state
     "ws" '(:bare-metal (:session-id "bm1" :had-session t)
            :sandbox (:session-id "sb1" :had-session nil)))
    (let ((bm (claude-repl--ws-get "ws" :bare-metal))
          (sb (claude-repl--ws-get "ws" :sandbox)))
      (should (equal (claude-repl-instantiation-session-id bm) "bm1"))
      (should (eq (claude-repl-instantiation-had-session bm) t))
      (should (equal (claude-repl-instantiation-session-id sb) "sb1"))
      (should-not (claude-repl-instantiation-had-session sb)))))

;;;; ---- Tests: apply-restored-state ----

(ert-deftest claude-repl-test-apply-restored-state-sets-project-dir ()
  "apply-restored-state sets :project-dir from saved state."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws" :bare-metal (make-claude-repl-instantiation))
    (claude-repl--ws-put "ws" :sandbox (make-claude-repl-instantiation))
    (claude-repl--apply-restored-state
     "ws" '(:project-dir "/saved/root"
            :bare-metal (:session-id "x" :had-session nil)
            :sandbox nil))
    (should (equal (claude-repl--ws-get "ws" :project-dir) "/saved/root"))))

(ert-deftest claude-repl-test-apply-restored-state-nil-project-dir ()
  "apply-restored-state does not overwrite :project-dir when saved is nil."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws" :project-dir "/existing")
    (claude-repl--ws-put "ws" :bare-metal (make-claude-repl-instantiation))
    (claude-repl--ws-put "ws" :sandbox (make-claude-repl-instantiation))
    (claude-repl--apply-restored-state "ws" '(:project-dir nil))
    ;; :project-dir should remain "/existing" since saved was nil
    (should (equal (claude-repl--ws-get "ws" :project-dir) "/existing"))))

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

;;;; ---- Tests: state-restore ----

(ert-deftest claude-repl-test-state-restore-reads-file ()
  "state-restore reads state from disk and applies it."
  (claude-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-state-" t)))
      (unwind-protect
          (progn
            ;; Write a state file
            (claude-repl--write-sexp-file
             (expand-file-name ".claude-repl-state" tmpdir)
             '(:project-dir "/restored/root"
               :active-env :sandbox
               :bare-metal (:session-id "bm-id" :had-session t)
               :sandbox (:session-id "sb-id" :had-session nil)))
            ;; Set up workspace with project-dir pointing to tmpdir
            (claude-repl--ws-put "ws" :project-dir tmpdir)
            (claude-repl--ws-put "ws" :bare-metal (make-claude-repl-instantiation))
            (claude-repl--ws-put "ws" :sandbox (make-claude-repl-instantiation))
            (claude-repl--state-restore "ws")
            ;; Verify state was applied
            (should (equal (claude-repl--ws-get "ws" :project-dir) "/restored/root"))
            (should (equal (claude-repl-instantiation-session-id
                            (claude-repl--ws-get "ws" :bare-metal))
                           "bm-id")))
        (delete-directory tmpdir t)))))

(ert-deftest claude-repl-test-state-restore-no-file ()
  "state-restore does nothing gracefully when no state file exists."
  (claude-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-state-" t)))
      (unwind-protect
          (progn
            (claude-repl--ws-put "ws" :project-dir tmpdir)
            ;; No .claude-repl-state file exists
            (claude-repl--state-restore "ws"))
        (delete-directory tmpdir t)))))

(ert-deftest claude-repl-test-state-restore-fallback-to-git-root ()
  "state-restore falls back to git-root when :project-dir is not set."
  (claude-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-state-" t)))
      (unwind-protect
          (progn
            ;; Write state file
            (claude-repl--write-sexp-file
             (expand-file-name ".claude-repl-state" tmpdir)
             '(:project-dir "/fallback/root"))
            ;; No :project-dir on workspace -- fall back to git-root
            (cl-letf (((symbol-function 'claude-repl--git-root)
                       (lambda (&optional _d) tmpdir)))
              (claude-repl--state-restore "ws")
              (should (equal (claude-repl--ws-get "ws" :project-dir) "/fallback/root"))))
        (delete-directory tmpdir t)))))

;;;; ---- Tests: history-save / history-restore round-trip ----

(ert-deftest claude-repl-test-history-save-and-restore-round-trip ()
  "history-save writes and history-restore reads back the same data."
  (claude-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-hist-" t)))
      (unwind-protect
          (claude-repl-test--with-temp-buffer " *test-hist-round-trip*"
            (setq-local claude-repl--project-root tmpdir)
            (setq-local claude-repl--input-history '("third" "second" "first"))
            (claude-repl--ws-put "test-ws" :input-buffer (current-buffer))
            ;; Save
            (claude-repl--history-save "test-ws")
            ;; Clear and restore
            (setq-local claude-repl--input-history nil)
            (let ((default-directory tmpdir))
              (claude-repl--history-restore))
            (should (equal claude-repl--input-history '("third" "second" "first"))))
        (delete-directory tmpdir t)))))

(ert-deftest claude-repl-test-history-save-skips-nil-history ()
  "history-save should not write when history is nil."
  (claude-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-hist-" t)))
      (unwind-protect
          (claude-repl-test--with-temp-buffer " *test-hist-save-nil*"
            (setq-local claude-repl--project-root tmpdir)
            (setq-local claude-repl--input-history nil)
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
  (claude-repl-test--with-temp-buffer " *test-hist-restore-none*"
    (setq-local claude-repl--input-history nil)
    (cl-letf (((symbol-function 'claude-repl--resolve-root)
               (lambda () "/nonexistent/path")))
      (claude-repl--history-restore)
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
                :had-session t
                :start-cmd "claude --resume"))
         (plist (claude-repl--instantiation-to-plist inst)))
    (should (equal plist '(:session-id "abc" :had-session t)))
    (should-not (plist-member plist :start-cmd))))

;;;; ---- Tests: instantiation-from-plist edge cases ----

(ert-deftest claude-repl-test-instantiation-from-plist-extra-keys ()
  "instantiation-from-plist silently ignores extra keys in saved plist."
  (let ((inst (make-claude-repl-instantiation)))
    (claude-repl--instantiation-from-plist
     inst '(:session-id "xyz" :had-session t :unknown-key "val" :another 42))
    (should (equal (claude-repl-instantiation-session-id inst) "xyz"))
    (should (eq (claude-repl-instantiation-had-session inst) t))))

(ert-deftest claude-repl-test-instantiation-from-plist-partial-keys ()
  "instantiation-from-plist handles plist with only :session-id (no :had-session)."
  (let ((inst (make-claude-repl-instantiation :had-session t)))
    (claude-repl--instantiation-from-plist inst '(:session-id "partial"))
    (should (equal (claude-repl-instantiation-session-id inst) "partial"))
    ;; :had-session was not in plist, so plist-get returns nil, overwriting old value
    (should-not (claude-repl-instantiation-had-session inst))))

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
            (setq-local claude-repl--project-root tmpdir)
            (setq-local claude-repl--input-history '("only-one"))
            (claude-repl--ws-put "test-ws" :input-buffer (current-buffer))
            (cl-letf (((symbol-function 'claude-repl--resolve-root)
                       (lambda () tmpdir)))
              (claude-repl--history-save "test-ws")
              ;; Read back and verify
              (let ((file (expand-file-name ".claude-repl-history" tmpdir)))
                (should (equal (claude-repl--read-sexp-file file) '("only-one"))))))
        (delete-directory tmpdir t)))))

;;;; ---- Tests: history-restore edge cases ----

(ert-deftest claude-repl-test-history-restore-non-list-data ()
  "history-restore sets history to whatever is in the file, even non-list."
  (let ((tmpdir (make-temp-file "test-hist-nonlist-" t)))
    (unwind-protect
        (claude-repl-test--with-temp-buffer " *test-hist-nonlist*"
          (setq-local claude-repl--input-history nil)
          ;; Write a string (non-list) to the history file
          (claude-repl--write-sexp-file
           (expand-file-name ".claude-repl-history" tmpdir) "just-a-string")
          (cl-letf (((symbol-function 'claude-repl--resolve-root)
                     (lambda () tmpdir)))
            (claude-repl--history-restore)
            (should (equal claude-repl--input-history "just-a-string"))))
      (delete-directory tmpdir t))))

(ert-deftest claude-repl-test-history-restore-overwrites-existing ()
  "history-restore overwrites existing history, does not append."
  (let ((tmpdir (make-temp-file "test-hist-overwrite-" t)))
    (unwind-protect
        (claude-repl-test--with-temp-buffer " *test-hist-overwrite*"
          (setq-local claude-repl--input-history '("old1" "old2"))
          (claude-repl--write-sexp-file
           (expand-file-name ".claude-repl-history" tmpdir) '("new1"))
          (cl-letf (((symbol-function 'claude-repl--resolve-root)
                     (lambda () tmpdir)))
            (claude-repl--history-restore)
            (should (equal claude-repl--input-history '("new1")))))
      (delete-directory tmpdir t))))

;;;; ---- Tests: collect-env-state edge cases ----

(ert-deftest claude-repl-test-collect-env-state-partial ()
  "collect-env-state handles only one environment initialized."
  (claude-repl-test--with-clean-state
    (let ((inst (make-claude-repl-instantiation :session-id "s1" :had-session t)))
      (claude-repl--ws-put "ws" :sandbox inst)
      ;; :bare-metal is not set
      (let ((state (claude-repl--collect-env-state "ws")))
        (should (equal (plist-get state :sandbox)
                       '(:session-id "s1" :had-session t)))
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

;;;; ---- Tests: restore-env-state edge cases ----

(ert-deftest claude-repl-test-restore-env-state-nil-for-one-env ()
  "restore-env-state handles nil saved state for one environment."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws" :bare-metal
                         (make-claude-repl-instantiation :session-id "orig"))
    (claude-repl--ws-put "ws" :sandbox (make-claude-repl-instantiation))
    (claude-repl--restore-env-state
     "ws" '(:bare-metal nil
            :sandbox (:session-id "sb1" :had-session t)))
    ;; :bare-metal had nil saved, so instantiation-from-plist is a no-op
    (should (equal (claude-repl-instantiation-session-id
                    (claude-repl--ws-get "ws" :bare-metal))
                   "orig"))
    ;; :sandbox should be restored
    (should (equal (claude-repl-instantiation-session-id
                    (claude-repl--ws-get "ws" :sandbox))
                   "sb1"))))

(ert-deftest claude-repl-test-restore-env-state-ws-struct-nil ()
  "restore-env-state handles nil workspace struct for one environment."
  (claude-repl-test--with-clean-state
    ;; :bare-metal has no instantiation struct on the workspace
    (claude-repl--ws-put "ws" :sandbox (make-claude-repl-instantiation))
    (claude-repl--restore-env-state
     "ws" '(:bare-metal (:session-id "bm1" :had-session t)
            :sandbox (:session-id "sb1" :had-session nil)))
    ;; Should not error; :bare-metal struct is nil so from-plist is a no-op
    (should (equal (claude-repl-instantiation-session-id
                    (claude-repl--ws-get "ws" :sandbox))
                   "sb1"))))

;;;; ---- Tests: apply-restored-state edge cases ----

(ert-deftest claude-repl-test-apply-restored-state-empty-plist ()
  "apply-restored-state handles a completely empty state plist without error."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws" :bare-metal (make-claude-repl-instantiation))
    (claude-repl--ws-put "ws" :sandbox (make-claude-repl-instantiation))
    ;; Should not error with empty plist
    (claude-repl--apply-restored-state "ws" '())))

;;;; ---- Tests: state-restore edge cases ----

(ert-deftest claude-repl-test-state-restore-both-roots-nil ()
  "state-restore does nothing when both :project-dir and git-root are nil."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'claude-repl--git-root)
               (lambda (&optional _d) nil)))
      ;; No :project-dir set, git-root returns nil -> state-file returns nil
      (claude-repl--state-restore "ws"))))

(ert-deftest claude-repl-test-state-restore-corrupt-data ()
  "state-restore handles corrupt data via with-error-logging."
  (claude-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-state-corrupt-" t)))
      (unwind-protect
          (progn
            ;; Write a number (not a plist) as state data -- apply-restored-state
            ;; will try to plist-get on it, which may error
            (claude-repl--write-sexp-file
             (expand-file-name ".claude-repl-state" tmpdir) 42)
            (claude-repl--ws-put "ws" :project-dir tmpdir)
            ;; Should not propagate error
            (claude-repl--state-restore "ws"))
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

(provide 'test-history)

;;; test-history.el ends here
