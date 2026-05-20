;;; test-workspace.el --- ERT tests for claude-repl workspace.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the workspace state encapsulation API in workspace.el.
;; One edge case per test, AAA structure.
;;
;; Run with:
;;   emacs -batch -Q -l ert -l test-workspace.el -f ert-run-tests-batch-and-exit

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Tests: --ws-known-p ----

(ert-deftest claude-repl-test-ws-known-p-returns-t-for-live-entry ()
  "A workspace with a hash entry and no :nuked-at is known."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :project-dir "/tmp/x")
    (should (claude-repl--ws-known-p "ws1"))))

(ert-deftest claude-repl-test-ws-known-p-returns-t-for-tombstoned-entry ()
  "A tombstoned workspace (entry + :nuked-at set) is still known."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :project-dir "/tmp/x")
    (claude-repl--ws-del "ws1")
    (should (claude-repl--ws-known-p "ws1"))))

(ert-deftest claude-repl-test-ws-known-p-returns-nil-for-unknown ()
  "A workspace name that has never been registered is not known."
  (claude-repl-test--with-clean-state
    (should-not (claude-repl--ws-known-p "never-registered"))))

(ert-deftest claude-repl-test-ws-known-p-returns-t-for-empty-plist ()
  "A workspace whose plist is the empty list is still present."
  (claude-repl-test--with-clean-state
    (puthash "ws1" nil claude-repl--workspaces)
    (should (claude-repl--ws-known-p "ws1"))))

;;;; ---- Tests: --ws-require-known ----

(ert-deftest claude-repl-test-ws-require-known-passes-for-known ()
  "--ws-require-known returns nil (no error) when ws is known."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :project-dir "/tmp/x")
    (should-not (claude-repl--ws-require-known "ws1" "ctx"))))

(ert-deftest claude-repl-test-ws-require-known-errors-for-unknown ()
  "--ws-require-known signals user-error when ws is not known."
  (claude-repl-test--with-clean-state
    (should-error (claude-repl--ws-require-known "missing" "ctx")
                  :type 'user-error)))

(ert-deftest claude-repl-test-ws-require-known-includes-context-in-message ()
  "The error message mentions the CONTEXT argument so callers identify themselves."
  (claude-repl-test--with-clean-state
    (condition-case err
        (progn (claude-repl--ws-require-known "missing" "render-status")
               (ert-fail "expected user-error"))
      (user-error
       (should (string-match-p "render-status" (error-message-string err)))))))

;;;; ---- Tests: --ws-tombstoned-p ----

(ert-deftest claude-repl-test-ws-tombstoned-p-returns-t-after-ws-del ()
  "A workspace returns t for tombstoned after --ws-del runs on it."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :project-dir "/tmp/x")
    (claude-repl--ws-del "ws1")
    (should (claude-repl--ws-tombstoned-p "ws1"))))

(ert-deftest claude-repl-test-ws-tombstoned-p-returns-nil-for-live-entry ()
  "A live workspace (no :nuked-at) is not tombstoned."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :project-dir "/tmp/x")
    (should-not (claude-repl--ws-tombstoned-p "ws1"))))

(ert-deftest claude-repl-test-ws-tombstoned-p-returns-nil-for-unknown ()
  "An unknown workspace is not tombstoned (it is neither live nor tombstoned)."
  (claude-repl-test--with-clean-state
    (should-not (claude-repl--ws-tombstoned-p "missing"))))

(ert-deftest claude-repl-test-ws-tombstoned-p-partition-with-live-p ()
  "live and tombstoned are mutually exclusive over known workspaces."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :project-dir "/tmp/x")
    ;; Arrange: live ws.
    (should (claude-repl--ws-live-p "ws1"))
    (should-not (claude-repl--ws-tombstoned-p "ws1"))
    ;; Act: tombstone it.
    (claude-repl--ws-del "ws1")
    ;; Assert: now the inverse.
    (should-not (claude-repl--ws-live-p "ws1"))
    (should (claude-repl--ws-tombstoned-p "ws1"))))

;;;; ---- Tests: --ws-open-p ----

(ert-deftest claude-repl-test-ws-open-p-returns-t-when-in-persp-cache ()
  "A known workspace present in persp-names-cache is open."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :project-dir "/tmp/x")
    (let ((persp-names-cache '("ws1" "other")))
      (should (claude-repl--ws-open-p "ws1")))))

(ert-deftest claude-repl-test-ws-open-p-returns-nil-when-not-in-persp-cache ()
  "A known workspace NOT present in persp-names-cache is not open."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :project-dir "/tmp/x")
    (let ((persp-names-cache '("other")))
      (should-not (claude-repl--ws-open-p "ws1")))))

(ert-deftest claude-repl-test-ws-open-p-errors-for-unknown ()
  "An unknown workspace name signals user-error rather than returning nil."
  (claude-repl-test--with-clean-state
    (let ((persp-names-cache '("missing")))
      (should-error (claude-repl--ws-open-p "missing") :type 'user-error))))

(ert-deftest claude-repl-test-ws-open-p-decouples-from-tombstone ()
  "A tombstoned ws can still be `open' if persp-names-cache still lists it."
  ;; This documents the legitimate divergence between the two data
  ;; sources: tab-bar membership (persp-names-cache) and hash liveness
  ;; (--ws-live-p) are NOT the same thing.  See `--ws-open-p' docstring.
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :project-dir "/tmp/x")
    (claude-repl--ws-del "ws1")
    (let ((persp-names-cache '("ws1")))
      (should (claude-repl--ws-tombstoned-p "ws1"))
      (should (claude-repl--ws-open-p "ws1")))))

(ert-deftest claude-repl-test-ws-open-p-returns-nil-when-persp-cache-unbound ()
  "--ws-open-p returns nil rather than erroring when persp-names-cache is unbound."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :project-dir "/tmp/x")
    (let (persp-names-cache)
      ;; Unbind the symbol entirely for the duration of this test.
      (makunbound 'persp-names-cache)
      (unwind-protect
          (should-not (claude-repl--ws-open-p "ws1"))
        ;; Restore: rebind to an empty list so other tests don't trip
        ;; on the unbound state.
        (setq persp-names-cache nil)))))

(provide 'test-workspace)
;;; test-workspace.el ends here
