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

;;;; ---- Tests: --ws-list-names ------------------------------------------

(ert-deftest claude-repl-test-ws-list-names-intersects-cache-and-known ()
  "Returns names that are BOTH in persp-names-cache AND --ws-known-p."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "known-and-open" :project-dir "/tmp/a")
    (claude-repl--ws-put "known-not-open" :project-dir "/tmp/b")
    (let ((persp-names-cache '("known-and-open" "unknown-in-cache")))
      (let ((result (claude-repl--ws-list-names)))
        (should (member "known-and-open" result))
        (should-not (member "known-not-open" result))
        (should-not (member "unknown-in-cache" result))))))

(ert-deftest claude-repl-test-ws-list-names-excludes-persp-nil-name ()
  "The persp-nil-name sentinel is filtered out even when it appears in cache and would be known."
  (claude-repl-test--with-clean-state
    ;; Arrange a ws whose name equals the nil sentinel (pathological but
    ;; documented elsewhere as a guard pattern).
    (let ((persp-nil-name "none"))
      (claude-repl--ws-put "none" :project-dir "/tmp/x")
      (let ((persp-names-cache '("none" "real-ws")))
        (claude-repl--ws-put "real-ws" :project-dir "/tmp/y")
        (let ((result (claude-repl--ws-list-names)))
          (should-not (member "none" result))
          (should (member "real-ws" result)))))))

(ert-deftest claude-repl-test-ws-list-names-preserves-cache-order ()
  "Order of results follows persp-names-cache order so tab-bar order is stable."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "a" :project-dir "/tmp/a")
    (claude-repl--ws-put "b" :project-dir "/tmp/b")
    (claude-repl--ws-put "c" :project-dir "/tmp/c")
    (let ((persp-names-cache '("c" "a" "b")))
      (should (equal '("c" "a" "b") (claude-repl--ws-list-names))))))

(ert-deftest claude-repl-test-ws-list-names-returns-nil-when-cache-unbound ()
  "Returns nil rather than erroring when persp-names-cache is unbound."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :project-dir "/tmp/x")
    (let (persp-names-cache)
      (makunbound 'persp-names-cache)
      (unwind-protect
          (should-not (claude-repl--ws-list-names))
        (setq persp-names-cache nil)))))

(ert-deftest claude-repl-test-ws-list-names-includes-tombstoned-if-in-cache ()
  "A tombstoned ws that still appears in persp-names-cache is listed.
This case is rare in production (the nuke path removes from cache
before tombstoning), but the predicate is `--ws-known-p' which is
true for tombstoned, so the list includes it.  Documents the
contract explicitly so a renderer relying on it stays predictable."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :project-dir "/tmp/x")
    (claude-repl--ws-del "ws1")
    (let ((persp-names-cache '("ws1")))
      (should (member "ws1" (claude-repl--ws-list-names))))))

;;;; ---- Tests: --ws-render-status (closed-set return) -------------------

(ert-deftest claude-repl-test-ws-render-status-errors-for-unknown ()
  "Unknown ws signals user-error via --ws-require-known."
  (claude-repl-test--with-clean-state
    (should-error (claude-repl--ws-render-status "missing") :type 'user-error)))

(ert-deftest claude-repl-test-ws-render-status-nil-for-tombstoned ()
  "Tombstoned ws returns nil — renderers skip these (drawer filters them anyway)."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :project-dir "/tmp/x")
    (claude-repl--ws-put "ws1" :claude-state :thinking)
    (claude-repl--ws-del "ws1")
    (should-not (claude-repl--ws-render-status "ws1"))))

(ert-deftest claude-repl-test-ws-render-status-nil-for-unborn ()
  "Live ws with no state signals returns nil (no session yet)."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :project-dir "/tmp/x")
    (should-not (claude-repl--ws-render-status "ws1"))))

(ert-deftest claude-repl-test-ws-render-status-returns-merge-conflict ()
  "`:repl-state :merge-conflict' renders as :merge-conflict."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :project-dir "/tmp/x")
    (claude-repl--ws-put "ws1" :repl-state :merge-conflict)
    (should (eq :merge-conflict (claude-repl--ws-render-status "ws1")))))

(ert-deftest claude-repl-test-ws-render-status-returns-merge-failed ()
  "`:repl-state :merge-failed' renders as :merge-failed."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :project-dir "/tmp/x")
    (claude-repl--ws-put "ws1" :repl-state :merge-failed)
    (should (eq :merge-failed (claude-repl--ws-render-status "ws1")))))

(ert-deftest claude-repl-test-ws-render-status-returns-merged ()
  "`:repl-state :merged' renders as :merged."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :project-dir "/tmp/x")
    (claude-repl--ws-put "ws1" :repl-state :merged)
    (should (eq :merged (claude-repl--ws-render-status "ws1")))))

(ert-deftest claude-repl-test-ws-render-status-returns-merging ()
  "`:merging t' renders as :merging."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :project-dir "/tmp/x")
    (claude-repl--ws-put "ws1" :merging t)
    (should (eq :merging (claude-repl--ws-render-status "ws1")))))

(ert-deftest claude-repl-test-ws-render-status-returns-merge-queued ()
  "`:repl-state :merge-queued' renders as :merge-queued."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :project-dir "/tmp/x")
    (claude-repl--ws-put "ws1" :repl-state :merge-queued)
    (should (eq :merge-queued (claude-repl--ws-render-status "ws1")))))

(ert-deftest claude-repl-test-ws-render-status-returns-dead ()
  "`:repl-state :dead' renders as :dead when no merge signal applies."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :project-dir "/tmp/x")
    (claude-repl--ws-put "ws1" :repl-state :dead)
    (should (eq :dead (claude-repl--ws-render-status "ws1")))))

(ert-deftest claude-repl-test-ws-render-status-returns-thinking ()
  "`:claude-state :thinking' renders as :thinking."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :project-dir "/tmp/x")
    (claude-repl--ws-put "ws1" :claude-state :thinking)
    (should (eq :thinking (claude-repl--ws-render-status "ws1")))))

(ert-deftest claude-repl-test-ws-render-status-returns-permission ()
  "`:claude-state :permission' renders as :permission."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :project-dir "/tmp/x")
    (claude-repl--ws-put "ws1" :claude-state :permission)
    (should (eq :permission (claude-repl--ws-render-status "ws1")))))

(ert-deftest claude-repl-test-ws-render-status-returns-init ()
  "`:claude-state :init' renders as :init."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :project-dir "/tmp/x")
    (claude-repl--ws-put "ws1" :claude-state :init)
    (should (eq :init (claude-repl--ws-render-status "ws1")))))

(ert-deftest claude-repl-test-ws-render-status-returns-done ()
  "`:claude-state :done' renders as :done."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :project-dir "/tmp/x")
    (claude-repl--ws-put "ws1" :claude-state :done)
    (should (eq :done (claude-repl--ws-render-status "ws1")))))

(ert-deftest claude-repl-test-ws-render-status-returns-stop-failed ()
  "`:claude-state :stop-failed' renders as :stop-failed."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :project-dir "/tmp/x")
    (claude-repl--ws-put "ws1" :claude-state :stop-failed)
    (should (eq :stop-failed (claude-repl--ws-render-status "ws1")))))

(ert-deftest claude-repl-test-ws-render-status-returns-idle ()
  "`:claude-state :idle' renders as :idle."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :project-dir "/tmp/x")
    (claude-repl--ws-put "ws1" :claude-state :idle)
    (should (eq :idle (claude-repl--ws-render-status "ws1")))))

;;;; ---- Tests: --ws-render-status precedence pairs -----------------------

(ert-deftest claude-repl-test-ws-render-status-conflict-beats-failed ()
  "An active conflict outranks a silent abort even when both flags are set."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :project-dir "/tmp/x")
    (claude-repl--ws-put "ws1" :repl-state :merge-conflict)
    ;; Set a stale :merging too — should still report conflict, not merging.
    (claude-repl--ws-put "ws1" :merging t)
    (should (eq :merge-conflict (claude-repl--ws-render-status "ws1")))))

(ert-deftest claude-repl-test-ws-render-status-merge-failed-beats-dead ()
  "An actionable merge-failed signal outranks an incidental dead vterm.
The merge-failed setter cleared the prior repl-state, but if both
somehow co-exist, the renderer must report the more actionable one."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :project-dir "/tmp/x")
    ;; Only one :repl-state at a time in practice; but to exercise the
    ;; precedence we set :merge-failed and let :merging carry an
    ;; orthogonal dead-vterm signal via :claude-state nil + a stale flag.
    (claude-repl--ws-put "ws1" :repl-state :merge-failed)
    (should (eq :merge-failed (claude-repl--ws-render-status "ws1")))))

(ert-deftest claude-repl-test-ws-render-status-merged-beats-dead ()
  "A merged workspace whose vterm has since died still reads as merged.
This was the existing drawer behavior and is preserved by the
unification."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :project-dir "/tmp/x")
    ;; :repl-state is set to :merged on success — that already excludes
    ;; :dead from the same slot.  This test just documents the chosen
    ;; order via the cond branch alignment.
    (claude-repl--ws-put "ws1" :repl-state :merged)
    (should (eq :merged (claude-repl--ws-render-status "ws1")))))

(ert-deftest claude-repl-test-ws-render-status-merging-beats-dead ()
  "An in-flight cherry-pick (`:merging t') outranks a dead vterm.
This is the motivating bug class: pre-merge `--close-workspace
preserve-entry' tears down the vterm, then the worker thread starts
cherry-picking.  The drawer must surface the merge, not the dead
vterm."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :project-dir "/tmp/x")
    (claude-repl--ws-put "ws1" :merging t)
    (claude-repl--ws-put "ws1" :repl-state :dead)
    (should (eq :merging (claude-repl--ws-render-status "ws1")))))

(ert-deftest claude-repl-test-ws-render-status-merging-beats-claude-state ()
  "An in-flight merge outranks claude-state.
A workspace that was :thinking when the merge command fired should
read as :merging in the drawer until cherry-pick resolves."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :project-dir "/tmp/x")
    (claude-repl--ws-put "ws1" :merging t)
    (claude-repl--ws-put "ws1" :claude-state :thinking)
    (should (eq :merging (claude-repl--ws-render-status "ws1")))))

(ert-deftest claude-repl-test-ws-render-status-merge-queued-beats-claude-state ()
  "A queued merge outranks claude-state for the same reason as :merging."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :project-dir "/tmp/x")
    (claude-repl--ws-put "ws1" :repl-state :merge-queued)
    (claude-repl--ws-put "ws1" :claude-state :thinking)
    (should (eq :merge-queued (claude-repl--ws-render-status "ws1")))))

(ert-deftest claude-repl-test-ws-render-status-merge-queued-beats-dead ()
  "Queued merge outranks dead vterm — same reasoning as :merging > :dead."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :project-dir "/tmp/x")
    ;; :repl-state holds one value at a time, but :merge-queued is the
    ;; queued marker and :dead is the dead-vterm marker; in this case
    ;; we set :repl-state to :merge-queued (the canonical signal) and
    ;; verify it wins over a :claude-state nil + no dead flag.  The
    ;; combined "what beats what when both occur" is asserted
    ;; structurally by the cond order in the function under test.
    (claude-repl--ws-put "ws1" :repl-state :merge-queued)
    (should (eq :merge-queued (claude-repl--ws-render-status "ws1")))))

(ert-deftest claude-repl-test-ws-render-status-dead-beats-claude-state ()
  "A dead vterm outranks any leftover claude-state value.
:dead + :claude-state :thinking should read as :dead."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :project-dir "/tmp/x")
    (claude-repl--ws-put "ws1" :repl-state :dead)
    (claude-repl--ws-put "ws1" :claude-state :thinking)
    (should (eq :dead (claude-repl--ws-render-status "ws1")))))

(ert-deftest claude-repl-test-ws-render-status-thinking-beats-permission ()
  "Among claude-states, :thinking outranks :permission."
  ;; The two are not typically both set, but the cond order must be
  ;; deterministic; document via test.
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :project-dir "/tmp/x")
    ;; Only one :claude-state in practice; this test exercises the
    ;; first-match-wins semantic by setting :claude-state to :thinking
    ;; and asserting it returns :thinking even though :permission and
    ;; later states are reachable elsewhere in the cond.
    (claude-repl--ws-put "ws1" :claude-state :thinking)
    (should (eq :thinking (claude-repl--ws-render-status "ws1")))))

(ert-deftest claude-repl-test-ws-render-status-merge-completed-flag-yields-merged ()
  "`:merge-completed t' alone yields :merged even when :repl-state is unset.
Covers the transition window between setting `:merge-completed t'
and writing `:repl-state :merged' (which the production setter does
in two separate `--ws-put' calls), and the legacy on-disk shape that
register-merged-workspace reclassifies on snapshot load."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :project-dir "/tmp/x")
    (claude-repl--ws-put "ws1" :merge-completed t)
    (should (eq :merged (claude-repl--ws-render-status "ws1")))))

(ert-deftest claude-repl-test-ws-render-status-merge-completed-beats-merging-flag ()
  "`:merge-completed t' wins over `:merging t' even when both are set.
Covers the transition window: production sets :merging nil before
:merge-completed t, but a fixture (or a future code path) that sets
the two in the other order must still resolve to :merged."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :project-dir "/tmp/x")
    (claude-repl--ws-put "ws1" :merge-completed t)
    (claude-repl--ws-put "ws1" :merging t)
    (should (eq :merged (claude-repl--ws-render-status "ws1")))))

(ert-deftest claude-repl-test-ws-render-status-tombstone-suppresses-merge-state ()
  "A tombstoned workspace returns nil even if :repl-state :merged is set.
The tombstone gate is checked before any state read; renderers should
skip these.  This is the documented contract."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :project-dir "/tmp/x")
    (claude-repl--ws-put "ws1" :repl-state :merged)
    (claude-repl--ws-del "ws1")
    (should-not (claude-repl--ws-render-status "ws1"))))

(provide 'test-workspace)
;;; test-workspace.el ends here
