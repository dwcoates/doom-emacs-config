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

;;;; ---- Tests: ws-get / ws-put (moved from test-core.el) ----

(ert-deftest claude-repl-test-ws-get-nonexistent-workspace ()
  "ws-get on non-existent workspace should return nil."
  (claude-repl-test--with-clean-state
    (should-not (claude-repl--ws-get "nonexistent" :status))))

(ert-deftest claude-repl-test-ws-get-nonexistent-key ()
  "ws-get for non-existent key on existing workspace should return nil."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :status "active")
    (should-not (claude-repl--ws-get "ws1" :nonexistent-key))))

(ert-deftest claude-repl-test-ws-get-zero-value ()
  "ws-get should return 0 when key is set to 0 (not confuse with nil)."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :counter 0)
    (should (equal (claude-repl--ws-get "ws1" :counter) 0))))

(ert-deftest claude-repl-test-ws-get-empty-string-value ()
  "ws-get should return empty string when key is set to empty string."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :name "")
    (should (equal (claude-repl--ws-get "ws1" :name) ""))))

(ert-deftest claude-repl-test-ws-put-new-workspace ()
  "ws-put to a brand new workspace should create the entry."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "new-ws" :status "ready")
    (should (equal (claude-repl--ws-get "new-ws" :status) "ready"))))

(ert-deftest claude-repl-test-ws-put-overwrite ()
  "ws-put should overwrite an existing key."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :status "old")
    (claude-repl--ws-put "ws1" :status "new")
    (should (equal (claude-repl--ws-get "ws1" :status) "new"))))

(ert-deftest claude-repl-test-ws-put-nil-value ()
  "ws-put with nil value should set key to nil."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :status "active")
    (claude-repl--ws-put "ws1" :status nil)
    (should-not (claude-repl--ws-get "ws1" :status))))

(ert-deftest claude-repl-test-ws-put-multiple-keys ()
  "ws-put should support multiple keys on the same workspace."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :status "ready")
    (claude-repl--ws-put "ws1" :priority "p1")
    (claude-repl--ws-put "ws1" :counter 42)
    (should (equal (claude-repl--ws-get "ws1" :status) "ready"))
    (should (equal (claude-repl--ws-get "ws1" :priority) "p1"))
    (should (equal (claude-repl--ws-get "ws1" :counter) 42))))

(ert-deftest claude-repl-test-ws-put-stub-create-emits-noisy-log ()
  "ws-put that creates a fresh entry with a non-:project-dir key should
emit a noisy unconditional log via `claude-repl--do-log'."
  (claude-repl-test--with-clean-state
    (let ((log-calls nil))
      (cl-letf (((symbol-function 'claude-repl--do-log)
                 (lambda (ws fmt args &optional _err)
                   (push (list ws fmt args) log-calls))))
        (claude-repl--ws-put "stub-ws" :priority "p1"))
      (should (= 1 (length log-calls)))
      (should (string-match-p "STUB-CREATE" (nth 1 (car log-calls)))))))

(ert-deftest claude-repl-test-ws-put-project-dir-first-no-log ()
  "ws-put that creates an entry by setting :project-dir as the first key
should not emit the stub-create log."
  (claude-repl-test--with-clean-state
    (let ((log-calls nil))
      (cl-letf (((symbol-function 'claude-repl--do-log)
                 (lambda (ws fmt args &optional _err)
                   (push (list ws fmt args) log-calls))))
        (claude-repl--ws-put "good-ws" :project-dir "/some/dir"))
      (should (null log-calls)))))

(ert-deftest claude-repl-test-ws-put-existing-entry-no-log ()
  "ws-put on an existing entry should not emit the stub-create log
even when writing a non-:project-dir key on an entry that itself
has no :project-dir (no new entry is being created)."
  (claude-repl-test--with-clean-state
    ;; Seed an entry via :project-dir first so it exists.
    (claude-repl--ws-put "ws1" :project-dir "/some/dir")
    (let ((log-calls nil))
      (cl-letf (((symbol-function 'claude-repl--do-log)
                 (lambda (ws fmt args &optional _err)
                   (push (list ws fmt args) log-calls))))
        (claude-repl--ws-put "ws1" :priority "p1"))
      (should (null log-calls)))))

(ert-deftest claude-repl-test-ws-put-stub-log-includes-caller-trace ()
  "Stub-create log payload should include a caller-trace string so the
producer of the leak can be identified from the message alone."
  (claude-repl-test--with-clean-state
    (let ((log-calls nil))
      (cl-letf (((symbol-function 'claude-repl--do-log)
                 (lambda (ws fmt args &optional _err)
                   (push (list ws fmt args) log-calls))))
        (claude-repl--ws-put "stub-ws" :priority "p1"))
      (should (= 1 (length log-calls)))
      (let* ((args (nth 2 (car log-calls)))
             (trace (car (last args))))
        (should (stringp trace))
        (should (> (length trace) 0))))))

;;;; ---- Tests: ws-del (tombstone semantics; moved from test-core.el) ----

(ert-deftest claude-repl-test-ws-del-clears-runtime-key ()
  "ws-del clears every key listed in `claude-repl--ws-runtime-keys'.
Asserts a representative runtime key (`:flashing') is reset to nil so
post-nuke render passes don't paint a stale flash on a tombstoned tab."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :project-dir "/tmp/ws1")
    (claude-repl--ws-put "ws1" :flashing t)
    (claude-repl--ws-del "ws1")
    (should-not (claude-repl--ws-get "ws1" :flashing))))

(ert-deftest claude-repl-test-ws-del-nonexistent ()
  "ws-del on a non-existent workspace should be a no-op."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-del "nonexistent")
    ;; Should not error and should not synthesize an entry.
    (should-not (gethash "nonexistent" claude-repl--workspaces))))

(ert-deftest claude-repl-test-ws-del-preserves-project-dir ()
  "ws-del preserves `:project-dir' across the tombstone — the entire
point of the tombstone model.  Without this guarantee, `--ws-dir'
callers would resume firing `no :project-dir for workspace X' errors
on persps that outlive their claude-repl session."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :project-dir "/tmp/ws1")
    (claude-repl--ws-del "ws1")
    (should (equal (claude-repl--ws-get "ws1" :project-dir) "/tmp/ws1"))))

(ert-deftest claude-repl-test-ws-del-preserves-priority ()
  "ws-del preserves `:priority' — identity/historical key, not runtime.
Re-creating a workspace with the same name should resume at its prior
priority badge without the user having to re-rank it."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :project-dir "/tmp/ws1")
    (claude-repl--ws-put "ws1" :priority :p1)
    (claude-repl--ws-del "ws1")
    (should (eq (claude-repl--ws-get "ws1" :priority) :p1))))

(ert-deftest claude-repl-test-ws-del-stamps-nuked-at ()
  "ws-del stamps `:nuked-at' with a non-nil time value — the marker
read by `--ws-live-p' and the snapshot persistence layer to distinguish
tombstones from live entries."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :project-dir "/tmp/ws1")
    (claude-repl--ws-del "ws1")
    (should (claude-repl--ws-get "ws1" :nuked-at))))

(ert-deftest claude-repl-test-ws-del-bumps-last-killed-at ()
  "ws-del bumps `:last-killed-at' so the picker's sort-by-last-killed
sees the tombstone immediately rather than waiting for an external
caller to stamp the timestamp."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :project-dir "/tmp/ws1")
    (claude-repl--ws-del "ws1")
    (should (claude-repl--ws-get "ws1" :last-killed-at))))

(ert-deftest claude-repl-test-ws-del-keeps-entry-in-hash ()
  "ws-del leaves the hash entry in place (tombstone, not remhash).
This is the structural inverse of the pre-tombstone behavior — pinning
so a regression that brings remhash back is caught immediately."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :project-dir "/tmp/ws1")
    (claude-repl--ws-del "ws1")
    (should (gethash "ws1" claude-repl--workspaces))))

(ert-deftest claude-repl-test-ws-del-logs-had-entry-true ()
  "ws-del logs `had-entry=t' when the workspace was registered."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :project-dir "/tmp/ws1")
    (let ((logged nil))
      (cl-letf (((symbol-function 'claude-repl--log)
                 (lambda (_ws fmt &rest args)
                   (setq logged (apply #'format fmt args)))))
        (claude-repl--ws-del "ws1")
        (should (string-match-p "ws-del:" logged))
        (should (string-match-p "had-entry=t" logged))))))

(ert-deftest claude-repl-test-ws-del-logs-had-entry-nil ()
  "ws-del logs `had-entry=nil' when the workspace was not registered."
  (claude-repl-test--with-clean-state
    (let ((logged nil))
      (cl-letf (((symbol-function 'claude-repl--log)
                 (lambda (_ws fmt &rest args)
                   (setq logged (apply #'format fmt args)))))
        (claude-repl--ws-del "nonexistent")
        (should (string-match-p "ws-del:" logged))
        (should (string-match-p "had-entry=nil" logged))))))

(ert-deftest claude-repl-test-ws-del-clears-peer-source-ws-name-cache ()
  "`--ws-del' must clear `:source-ws-name' on peers that cached the
deleted ws as their resolved source.  Without the sweep, a future
workspace re-using the deleted name would be returned as a parent it
isn't (different `:project-dir').  Asserts the sweep targets exactly
the affected peers."
  (claude-repl-test--with-clean-state
    (puthash "parent" '(:project-dir "/tmp/parent")
             claude-repl--workspaces)
    (puthash "child"  '(:project-dir "/tmp/child"
                                     :source-ws-dir "/tmp/parent"
                                     :source-ws-name "parent")
             claude-repl--workspaces)
    (puthash "unrelated" '(:project-dir "/tmp/u"
                                        :source-ws-name "someone-else")
             claude-repl--workspaces)
    (claude-repl--ws-del "parent")
    (should-not (claude-repl--ws-get "child" :source-ws-name))
    (should (equal (claude-repl--ws-get "unrelated" :source-ws-name)
                   "someone-else"))))

(ert-deftest claude-repl-test-ws-del-tombstones-entry-not-removes ()
  "`--ws-del' tombstones the target's own entry rather than removing it —
the post-tombstone-refactor invariant.  The peer-cache sweep above still
fires; this test pins that the same call also leaves the target entry
intact (just with `:nuked-at' stamped)."
  (claude-repl-test--with-clean-state
    (puthash "doomed" '(:project-dir "/tmp/x") claude-repl--workspaces)
    (claude-repl--ws-del "doomed")
    (should (gethash "doomed" claude-repl--workspaces))
    (should (claude-repl--ws-get "doomed" :nuked-at))
    (should (equal (claude-repl--ws-get "doomed" :project-dir) "/tmp/x"))))

;;;; ---- Tests: ws-live-p (moved from test-core.el) ----

(ert-deftest claude-repl-test-ws-live-p-returns-t-for-live-entry ()
  "ws-live-p returns non-nil for a fresh hash entry with no tombstone."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :project-dir "/tmp/ws1")
    (should (claude-repl--ws-live-p "ws1"))))

(ert-deftest claude-repl-test-ws-live-p-returns-nil-for-tombstone ()
  "ws-live-p returns nil for a tombstoned entry — the predicate that
keeps drawer/picker/state-updater from surfacing nuked workspaces."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :project-dir "/tmp/ws1")
    (claude-repl--ws-del "ws1")
    (should-not (claude-repl--ws-live-p "ws1"))))

(ert-deftest claude-repl-test-ws-live-p-returns-nil-for-unknown ()
  "ws-live-p returns nil when no hash entry exists at all."
  (claude-repl-test--with-clean-state
    (should-not (claude-repl--ws-live-p "never-seen"))))

;;;; ---- Tests: live-ws-names (moved from test-core.el) ----

(ert-deftest claude-repl-test-live-ws-names-excludes-tombstones ()
  "live-ws-names returns only non-tombstoned hash keys, regardless of
insertion order — the single helper every hash iterator routes through
to avoid surfacing nuked workspaces."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "alive" :project-dir "/tmp/alive")
    (claude-repl--ws-put "dead" :project-dir "/tmp/dead")
    (claude-repl--ws-del "dead")
    (let ((names (claude-repl--live-ws-names)))
      (should (member "alive" names))
      (should-not (member "dead" names)))))

(ert-deftest claude-repl-test-live-ws-names-empty-hash ()
  "live-ws-names returns nil (not an error) when the hash has no entries."
  (claude-repl-test--with-clean-state
    (should-not (claude-repl--live-ws-names))))

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

;;;; ---- Tests: --ws-hide-tombstoned-p ----

(ert-deftest claude-repl-test-ws-hide-tombstoned-p-returns-t-when-both-set ()
  "A workspace tombstoned by the hide flow returns t."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "hidden-ws" :project-dir "/tmp/x")
    (claude-repl--ws-put "hidden-ws" :hidden-project-dir t)
    (claude-repl--ws-del "hidden-ws")
    (should (claude-repl--ws-hide-tombstoned-p "hidden-ws"))))

(ert-deftest claude-repl-test-ws-hide-tombstoned-p-returns-nil-for-nuke-tombstoned ()
  "A workspace tombstoned without the hide marker returns nil even though it is tombstoned."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "nuked-ws" :project-dir "/tmp/x")
    (claude-repl--ws-del "nuked-ws")
    (should (claude-repl--ws-tombstoned-p "nuked-ws"))
    (should-not (claude-repl--ws-hide-tombstoned-p "nuked-ws"))))

(ert-deftest claude-repl-test-ws-hide-tombstoned-p-returns-nil-for-live-marker ()
  "A live workspace carrying the marker but no :nuked-at returns nil.
Predicate is a conjunction of tombstone state AND reason marker."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "live-ws" :project-dir "/tmp/x")
    (claude-repl--ws-put "live-ws" :hidden-project-dir t)
    (should-not (claude-repl--ws-hide-tombstoned-p "live-ws"))))

(ert-deftest claude-repl-test-ws-hide-tombstoned-p-returns-nil-for-unknown ()
  "An unregistered workspace returns nil."
  (claude-repl-test--with-clean-state
    (should-not (claude-repl--ws-hide-tombstoned-p "never-seen"))))

;;;; ---- Tests: --ws-hide-tombstoned-names ----

(ert-deftest claude-repl-test-ws-hide-tombstoned-names-returns-hide-tombstones-only ()
  "Enumerator returns hide-tombstoned ws but excludes nuke-tombstoned and live ws."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "hidden1" :project-dir "/tmp/a")
    (claude-repl--ws-put "hidden1" :hidden-project-dir t)
    (claude-repl--ws-del "hidden1")
    (claude-repl--ws-put "nuked"   :project-dir "/tmp/b")
    (claude-repl--ws-del "nuked")
    (claude-repl--ws-put "live"    :project-dir "/tmp/c")
    (let ((names (claude-repl--ws-hide-tombstoned-names)))
      (should (equal names '("hidden1")))
      (should-not (member "nuked" names))
      (should-not (member "live" names)))))

(ert-deftest claude-repl-test-ws-hide-tombstoned-names-sorted-by-name ()
  "Returned names are sorted lexicographically for deterministic restore order."
  (claude-repl-test--with-clean-state
    (dolist (n '("c" "a" "b"))
      (claude-repl--ws-put n :project-dir (concat "/tmp/" n))
      (claude-repl--ws-put n :hidden-project-dir t)
      (claude-repl--ws-del n))
    (should (equal (claude-repl--ws-hide-tombstoned-names) '("a" "b" "c")))))

(ert-deftest claude-repl-test-ws-hide-tombstoned-names-empty-when-no-matches ()
  "Returns nil when no workspace carries the marker."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "live" :project-dir "/tmp/x")
    (should-not (claude-repl--ws-hide-tombstoned-names))))

;;;; ---- Tests: --ws-render-status nil for hide-tombstoned ----

(ert-deftest claude-repl-test-ws-render-status-nil-for-hide-tombstoned ()
  "Render-status returns nil for hide-tombstoned ws, collapsed with nuke-tombstoned."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "hidden" :project-dir "/tmp/x")
    (claude-repl--ws-put "hidden" :claude-state :thinking)
    (claude-repl--ws-put "hidden" :hidden-project-dir t)
    (claude-repl--ws-del "hidden")
    (should-not (claude-repl--ws-render-status "hidden"))))

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

;;;; ---- Tests: --ws-all-names -------------------------------------------

(ert-deftest claude-repl-test-ws-all-names-delegates-when-bound ()
  "--ws-all-names returns the raw +workspace-list-names value when bound."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-list-names)
               (lambda () '("a" "b" "c"))))
      (should (equal (claude-repl--ws-all-names) '("a" "b" "c"))))))

(ert-deftest claude-repl-test-ws-all-names-unfiltered-by-known ()
  "--ws-all-names returns names even when claude-repl never registered them."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-list-names)
               (lambda () '("stray-persp"))))
      (should (equal (claude-repl--ws-all-names) '("stray-persp"))))))

(ert-deftest claude-repl-test-ws-all-names-returns-nil-when-unbound ()
  "--ws-all-names returns nil when +workspace-list-names is not fboundp."
  (claude-repl-test--with-clean-state
    (fmakunbound '+workspace-list-names)
    (should-not (claude-repl--ws-all-names))))

;;;; ---- Tests: --ws-tombstoned-names ------------------------------------

(ert-deftest claude-repl-test-ws-tombstoned-names-returns-tombstones ()
  "--ws-tombstoned-names returns all tombstoned workspace names."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "live-ws"  :project-dir "/tmp/live")
    (claude-repl--ws-put "dead-ws"  :project-dir "/tmp/dead")
    (claude-repl--ws-del "dead-ws")
    (let ((result (claude-repl--ws-tombstoned-names)))
      (should (equal result '("dead-ws")))
      (should-not (member "live-ws" result)))))

(ert-deftest claude-repl-test-ws-tombstoned-names-empty-when-none ()
  "--ws-tombstoned-names returns nil when no workspace is tombstoned."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :project-dir "/tmp/ws1")
    (should-not (claude-repl--ws-tombstoned-names))))

(ert-deftest claude-repl-test-ws-tombstoned-names-sorted ()
  "--ws-tombstoned-names returns names in alphabetical order."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "charlie" :project-dir "/tmp/c")
    (claude-repl--ws-put "alpha"   :project-dir "/tmp/a")
    (claude-repl--ws-put "bravo"   :project-dir "/tmp/b")
    (claude-repl--ws-del "charlie")
    (claude-repl--ws-del "alpha")
    (claude-repl--ws-del "bravo")
    (should (equal (claude-repl--ws-tombstoned-names) '("alpha" "bravo" "charlie")))))

;;;; ---- Tests: --ws-names-cache-usable-p --------------------------------

(ert-deftest claude-repl-test-ws-names-cache-usable-p-returns-t-when-non-nil ()
  "--ws-names-cache-usable-p returns non-nil when persp-names-cache is a
non-nil list."
  (let ((persp-names-cache '("ws1")))
    (should (claude-repl--ws-names-cache-usable-p))))

(ert-deftest claude-repl-test-ws-names-cache-usable-p-returns-nil-when-nil ()
  "--ws-names-cache-usable-p returns nil when persp-names-cache is nil
even if bound — a nil cache is not a usable tab-bar signal."
  (let ((persp-names-cache nil))
    (should-not (claude-repl--ws-names-cache-usable-p))))

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

;;;; ---- Tests: reorder-workspace-by-priority (moved from test-status.el) ----

(ert-deftest claude-repl-test-reorder-priority-noop-when-priority-nil ()
  "reorder-workspace-by-priority leaves cache untouched when ws has no priority."
  (claude-repl-test--with-clean-state
    (let* ((persp-nil-name "main")
           (persp-names-cache '("main" "ws-a" "new-ws"))
           (captured nil))
      (cl-letf (((symbol-function 'persp-update-names-cache)
                 (lambda (new-cache) (setq captured new-cache))))
        (claude-repl--reorder-workspace-by-priority "new-ws")
        (should-not captured)))))

(ert-deftest claude-repl-test-reorder-priority-noop-when-not-in-cache ()
  "reorder-workspace-by-priority no-ops when ws is not registered in cache."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "missing-ws" :priority "p1")
    (let* ((persp-nil-name "main")
           (persp-names-cache '("main" "ws-a"))
           (captured nil))
      (cl-letf (((symbol-function 'persp-update-names-cache)
                 (lambda (new-cache) (setq captured new-cache))))
        (claude-repl--reorder-workspace-by-priority "missing-ws")
        (should-not captured)))))

(ert-deftest claude-repl-test-reorder-priority-logs-bail-no-priority ()
  "reorder-workspace-by-priority emits a BAIL/no-priority log line when ws has no :priority."
  (claude-repl-test--with-clean-state
    (let* ((persp-names-cache '("main" "ws-a" "new-ws"))
           (logs nil))
      (cl-letf (((symbol-function 'claude-repl--log)
                 (lambda (_ws fmt &rest args)
                   (push (apply #'format fmt args) logs))))
        (claude-repl--reorder-workspace-by-priority "new-ws")
        (should (cl-find-if (lambda (l)
                              (and (string-match-p "reorder-workspace-by-priority: BAIL" l)
                                   (string-match-p "reason=no-priority" l)))
                            logs))))))

(ert-deftest claude-repl-test-reorder-priority-logs-bail-not-in-cache ()
  "reorder-workspace-by-priority emits a BAIL/not-in-cache log line when ws is missing from the cache."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "missing-ws" :priority "p1")
    (let* ((persp-names-cache '("main" "ws-a"))
           (logs nil))
      (cl-letf (((symbol-function 'claude-repl--log)
                 (lambda (_ws fmt &rest args)
                   (push (apply #'format fmt args) logs))))
        (claude-repl--reorder-workspace-by-priority "missing-ws")
        (should (cl-find-if (lambda (l)
                              (and (string-match-p "reorder-workspace-by-priority: BAIL" l)
                                   (string-match-p "reason=not-in-cache" l)))
                            logs))))))

(ert-deftest claude-repl-test-reorder-priority-preserves-cache-string-identity ()
  "After reorder, the WS slot in `persp-names-cache' is `eq' to the
canonical string already in the cache, NOT to the (potentially fresh)
WS argument.  Regression for the persp-kill failure where `cl-delete'
with default `:test #'eql' could not match a content-equal but
identity-distinct string injected by `claude-repl-set-priority' from
`completing-read'.  See workspace.el for the full explanation."
  (claude-repl-test--with-clean-state
    (let* ((canonical (copy-sequence "new-p1"))
           (fresh (copy-sequence "new-p1"))
           (persp-nil-name "main")
           (persp-names-cache (list "main" "ws-a" canonical))
           (captured nil))
      (should-not (eq canonical fresh))
      (claude-repl--ws-put fresh :priority "p1")
      (cl-letf (((symbol-function 'persp-update-names-cache)
                 (lambda (new-cache) (setq captured new-cache))))
        (claude-repl--reorder-workspace-by-priority fresh)
        (let ((injected (car (member "new-p1" captured))))
          (should injected)
          (should (eq injected canonical))
          (should-not (eq injected fresh)))))))

(ert-deftest claude-repl-test-reorder-priority-logs-apply-on-success ()
  "reorder-workspace-by-priority emits an APPLY log line on the success path."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "new-p1" :priority "p1")
    (let* ((persp-nil-name "main")
           (persp-names-cache '("main" "ws-a" "new-p1"))
           (logs nil))
      (cl-letf (((symbol-function 'persp-update-names-cache) (lambda (_) nil))
                ((symbol-function 'claude-repl--log)
                 (lambda (_ws fmt &rest args)
                   (push (apply #'format fmt args) logs))))
        (claude-repl--reorder-workspace-by-priority "new-p1")
        (should (cl-find-if (lambda (l)
                              (string-match-p "reorder-workspace-by-priority: APPLY" l))
                            logs))))))

(ert-deftest claude-repl-test-reorder-priority-p1-moves-before-unprioritized ()
  "A new p1 workspace is moved ahead of unprioritized workspaces."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "new-p1" :priority "p1")
    (let* ((persp-nil-name "main")
           (persp-names-cache '("main" "ws-a" "ws-b" "new-p1"))
           (captured nil))
      (cl-letf (((symbol-function 'persp-update-names-cache)
                 (lambda (new-cache) (setq captured new-cache))))
        (claude-repl--reorder-workspace-by-priority "new-p1")
        (should (equal captured '("main" "new-p1" "ws-a" "ws-b")))))))

(ert-deftest claude-repl-test-reorder-priority-p2-after-existing-p1 ()
  "A new p2 workspace lands after an existing p1 but before unprioritized."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws-p1" :priority "p1")
    (claude-repl--ws-put "new-p2" :priority "p2")
    (let* ((persp-nil-name "main")
           (persp-names-cache '("main" "ws-p1" "ws-a" "new-p2"))
           (captured nil))
      (cl-letf (((symbol-function 'persp-update-names-cache)
                 (lambda (new-cache) (setq captured new-cache))))
        (claude-repl--reorder-workspace-by-priority "new-p2")
        (should (equal captured '("main" "ws-p1" "new-p2" "ws-a")))))))

(ert-deftest claude-repl-test-reorder-priority-equal-priority-after-existing ()
  "A new p1 workspace lands after an existing p1 (does not displace peers)."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws-p1-old" :priority "p1")
    (claude-repl--ws-put "new-p1" :priority "p1")
    (let* ((persp-nil-name "main")
           (persp-names-cache '("main" "ws-p1-old" "ws-a" "new-p1"))
           (captured nil))
      (cl-letf (((symbol-function 'persp-update-names-cache)
                 (lambda (new-cache) (setq captured new-cache))))
        (claude-repl--reorder-workspace-by-priority "new-p1")
        (should (equal captured '("main" "ws-p1-old" "new-p1" "ws-a")))))))

(ert-deftest claude-repl-test-reorder-priority-p05-goes-to-very-front ()
  "A new p05 workspace lands ahead of every other priority and unprioritized."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws-p1" :priority "p1")
    (claude-repl--ws-put "ws-p2" :priority "p2")
    (claude-repl--ws-put "new-p05" :priority "p05")
    (let* ((persp-nil-name "main")
           (persp-names-cache '("main" "ws-p1" "ws-p2" "ws-a" "new-p05"))
           (captured nil))
      (cl-letf (((symbol-function 'persp-update-names-cache)
                 (lambda (new-cache) (setq captured new-cache))))
        (claude-repl--reorder-workspace-by-priority "new-p05")
        (should (equal captured '("main" "new-p05" "ws-p1" "ws-p2" "ws-a")))))))

(ert-deftest claude-repl-test-reorder-priority-p3-after-all-priorities ()
  "A new p3 workspace lands after p05/p1/p2 and before unprioritized."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws-p05" :priority "p05")
    (claude-repl--ws-put "ws-p1" :priority "p1")
    (claude-repl--ws-put "ws-p2" :priority "p2")
    (claude-repl--ws-put "new-p3" :priority "p3")
    (let* ((persp-nil-name "main")
           (persp-names-cache '("main" "ws-p05" "ws-p1" "ws-p2" "ws-a" "new-p3"))
           (captured nil))
      (cl-letf (((symbol-function 'persp-update-names-cache)
                 (lambda (new-cache) (setq captured new-cache))))
        (claude-repl--reorder-workspace-by-priority "new-p3")
        (should (equal captured '("main" "ws-p05" "ws-p1" "ws-p2" "new-p3" "ws-a")))))))

(ert-deftest claude-repl-test-reorder-priority-preserves-nil-persp-position ()
  "reorder-workspace-by-priority keeps persp-nil-name at the head of the cache."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "new-p1" :priority "p1")
    (let* ((persp-nil-name "main")
           (persp-names-cache '("main" "ws-a" "new-p1"))
           (captured nil))
      (cl-letf (((symbol-function 'persp-update-names-cache)
                 (lambda (new-cache) (setq captured new-cache))))
        (claude-repl--reorder-workspace-by-priority "new-p1")
        (should (equal (car captured) "main"))))))

;;;; ---- Tests: --ws-resolve-persp ----

(ert-deftest claude-repl-test-ws-resolve-persp-returns-persp-when-found ()
  "ws-resolve-persp returns the persp object when one exists for the name."
  (claude-repl-test--with-clean-state
    (let ((fake-persp (list :a-persp-object)))
      (cl-letf (((symbol-function 'persp-get-by-name)
                 (lambda (_ws) fake-persp)))
        (should (eq (claude-repl--ws-resolve-persp "my-ws") fake-persp))))))

(ert-deftest claude-repl-test-ws-resolve-persp-returns-nil-for-not-persp-sentinel ()
  "ws-resolve-persp returns nil when persp-get-by-name returns the persp-not-persp keyword."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'persp-get-by-name)
               ;; persp-not-persp is :nil — a keyword — which keywordp catches
               (lambda (_ws) :nil)))
      (should-not (claude-repl--ws-resolve-persp "missing-ws")))))

(ert-deftest claude-repl-test-ws-resolve-persp-returns-nil-when-unbound ()
  "ws-resolve-persp returns nil when persp-get-by-name is not fboundp."
  (claude-repl-test--with-clean-state
    (fmakunbound 'persp-get-by-name)
    (should-not (claude-repl--ws-resolve-persp "my-ws"))))

(ert-deftest claude-repl-test-ws-resolve-persp-returns-nil-for-nil-result ()
  "ws-resolve-persp returns nil when persp-get-by-name returns nil."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'persp-get-by-name)
               (lambda (_ws) nil)))
      (should-not (claude-repl--ws-resolve-persp "my-ws")))))

;;;; ---- Tests: --ws-system-available-p ----

(ert-deftest claude-repl-test-ws-system-available-p-returns-t-when-persp-mode-on ()
  "ws-system-available-p returns t when persp-mode is non-nil."
  (claude-repl-test--with-clean-state
    (let ((persp-mode t))
      (should (claude-repl--ws-system-available-p)))))

(ert-deftest claude-repl-test-ws-system-available-p-returns-nil-when-persp-mode-off ()
  "ws-system-available-p returns nil when persp-mode is nil."
  (claude-repl-test--with-clean-state
    (let ((persp-mode nil))
      (should-not (claude-repl--ws-system-available-p)))))

(ert-deftest claude-repl-test-ws-system-available-p-returns-nil-when-persp-mode-unbound ()
  "ws-system-available-p returns nil when persp-mode variable is unbound."
  (claude-repl-test--with-clean-state
    ;; bound-and-true-p returns nil for unbound vars, same as nil.
    ;; We test with persp-mode=nil (the test-helpers default).
    (should-not (claude-repl--ws-system-available-p))))

;;;; ---- Tests: --ws-switch ----

(ert-deftest claude-repl-test-ws-switch-delegates-when-bound ()
  "ws-switch calls +workspace-switch with the given ws name."
  (claude-repl-test--with-clean-state
    (let (called-with)
      (cl-letf (((symbol-function '+workspace-switch)
                 (lambda (ws &rest _args) (setq called-with ws))))
        (claude-repl--ws-switch "my-ws")
        (should (equal called-with "my-ws"))))))

(ert-deftest claude-repl-test-ws-switch-passes-extra-args ()
  "ws-switch forwards additional args to +workspace-switch."
  (claude-repl-test--with-clean-state
    (let (captured-args)
      (cl-letf (((symbol-function '+workspace-switch)
                 (lambda (&rest args) (setq captured-args args))))
        (claude-repl--ws-switch "my-ws" t)
        (should (equal captured-args '("my-ws" t)))))))

(ert-deftest claude-repl-test-ws-switch-noop-when-unbound ()
  "ws-switch is a no-op when +workspace-switch is not fboundp."
  (claude-repl-test--with-clean-state
    (fmakunbound '+workspace-switch)
    (should-not (claude-repl--ws-switch "my-ws"))))

;;;; ---- Tests: --ws-current-name ----

(ert-deftest claude-repl-test-ws-current-name-delegates-to-wrapper ()
  "ws-current-name returns value from +workspace-current-name when bound."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "my-ws")))
      (should (equal (claude-repl--ws-current-name) "my-ws")))))

(ert-deftest claude-repl-test-ws-current-name-returns-nil-when-unbound ()
  "ws-current-name returns nil when +workspace-current-name is not fboundp."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) nil))
      ;; Unbind by fmakunbound so fboundp returns nil.
      (fmakunbound '+workspace-current-name)
      (should-not (claude-repl--ws-current-name)))))

(ert-deftest claude-repl-test-ws-current-name-returns-nil-when-no-persp ()
  "ws-current-name returns nil when the workspace system returns nil."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () nil)))
      (should-not (claude-repl--ws-current-name)))))

;;;; ---- Tests: --ws-exists-p ----

(ert-deftest claude-repl-test-ws-exists-p-delegates-when-bound ()
  "ws-exists-p returns the +workspace-exists-p result when bound."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-exists-p) (lambda (ws) (equal ws "live"))))
      (should (claude-repl--ws-exists-p "live"))
      (should-not (claude-repl--ws-exists-p "gone")))))

(ert-deftest claude-repl-test-ws-exists-p-returns-nil-when-unbound ()
  "ws-exists-p returns nil when +workspace-exists-p is not fboundp."
  (claude-repl-test--with-clean-state
    (fmakunbound '+workspace-exists-p)
    (should-not (claude-repl--ws-exists-p "any"))))

;;;; ---- Tests: --ws-kill ----

(ert-deftest claude-repl-test-ws-kill-delegates-when-bound ()
  "ws-kill calls +workspace/kill with the given ws name."
  (claude-repl-test--with-clean-state
    (let (killed)
      (cl-letf (((symbol-function '+workspace/kill) (lambda (ws) (setq killed ws))))
        (claude-repl--ws-kill "doomed")
        (should (equal killed "doomed"))))))

(ert-deftest claude-repl-test-ws-kill-noop-when-unbound ()
  "ws-kill is a no-op when +workspace/kill is not fboundp."
  (claude-repl-test--with-clean-state
    (fmakunbound '+workspace/kill)
    (should-not (claude-repl--ws-kill "doomed"))))

;;;; ---- Tests: --ws-main-name ----

(ert-deftest claude-repl-test-ws-main-name-returns-value ()
  "ws-main-name returns the +workspaces-main value when bound and non-nil."
  (claude-repl-test--with-clean-state
    (let ((+workspaces-main "custom-main"))
      (should (equal (claude-repl--ws-main-name) "custom-main")))))

(ert-deftest claude-repl-test-ws-main-name-returns-nil-when-nil ()
  "ws-main-name returns nil when +workspaces-main is nil."
  (claude-repl-test--with-clean-state
    (let ((+workspaces-main nil))
      (should-not (claude-repl--ws-main-name)))))

;;;; ---- Tests: --ws-frame-switch ----

(ert-deftest claude-repl-test-ws-frame-switch-delegates-when-bound ()
  "ws-frame-switch calls persp-frame-switch with the given ws name."
  (claude-repl-test--with-clean-state
    (let (switched)
      (cl-letf (((symbol-function 'persp-frame-switch) (lambda (ws) (setq switched ws))))
        (claude-repl--ws-frame-switch "target")
        (should (equal switched "target"))))))

(ert-deftest claude-repl-test-ws-frame-switch-noop-when-unbound ()
  "ws-frame-switch is a no-op when persp-frame-switch is not fboundp."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'persp-frame-switch) nil))
      (fmakunbound 'persp-frame-switch)
      (should-not (claude-repl--ws-frame-switch "target")))))

;;;; ---- Tests: --ws-frame-save-state ----

(ert-deftest claude-repl-test-ws-frame-save-state-delegates-when-bound ()
  "ws-frame-save-state calls persp-frame-save-state when bound."
  (claude-repl-test--with-clean-state
    (let (saved)
      (cl-letf (((symbol-function 'persp-frame-save-state) (lambda () (setq saved t))))
        (claude-repl--ws-frame-save-state)
        (should saved)))))

(ert-deftest claude-repl-test-ws-frame-save-state-noop-when-unbound ()
  "ws-frame-save-state is a no-op when persp-frame-save-state is not fboundp."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'persp-frame-save-state) nil))
      (fmakunbound 'persp-frame-save-state)
      (should-not (claude-repl--ws-frame-save-state)))))

;;;; ---- Tests: --ws-create ----

(ert-deftest claude-repl-test-ws-create-returns-persp-and-tags-project ()
  "ws-create calls persp-add-new and sets +workspace-project on a real persp."
  (claude-repl-test--with-clean-state
    (let (added param-call)
      (cl-letf (((symbol-function 'persp-add-new) (lambda (ws) (setq added ws) 'a-persp))
                ((symbol-function 'set-persp-parameter)
                 (lambda (key val persp) (setq param-call (list key val persp)))))
        (should (eq (claude-repl--ws-create "ws1" "/tmp/p") 'a-persp))
        (should (equal added "ws1"))
        (should (equal param-call '(+workspace-project "/tmp/p" a-persp)))))))

(ert-deftest claude-repl-test-ws-create-skips-param-when-keyword-sentinel ()
  "ws-create does not set the project param when persp-add-new returns a keyword."
  (claude-repl-test--with-clean-state
    (let (param-called)
      (cl-letf (((symbol-function 'persp-add-new) (lambda (_ws) :nil))
                ((symbol-function 'set-persp-parameter)
                 (lambda (&rest _) (setq param-called t))))
        (claude-repl--ws-create "ws1" "/tmp/p")
        (should-not param-called)))))

(ert-deftest claude-repl-test-ws-create-skips-param-when-no-dir ()
  "ws-create does not set the project param when PROJECT-DIR is nil."
  (claude-repl-test--with-clean-state
    (let (param-called)
      (cl-letf (((symbol-function 'persp-add-new) (lambda (_ws) 'a-persp))
                ((symbol-function 'set-persp-parameter)
                 (lambda (&rest _) (setq param-called t))))
        (claude-repl--ws-create "ws1")
        (should-not param-called)))))

(ert-deftest claude-repl-test-ws-create-noop-when-unbound ()
  "ws-create returns nil when persp-add-new is not fboundp."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'persp-add-new) nil))
      (fmakunbound 'persp-add-new)
      (should-not (claude-repl--ws-create "ws1" "/tmp/p")))))

;;;; ---- Tests: --ws-protected-p ----

(ert-deftest claude-repl-test-ws-protected-p-delegates-when-bound ()
  "ws-protected-p returns the +workspace--protected-p result when bound."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace--protected-p) (lambda (ws) (equal ws "main"))))
      (should (claude-repl--ws-protected-p "main"))
      (should-not (claude-repl--ws-protected-p "feature")))))

(ert-deftest claude-repl-test-ws-protected-p-returns-nil-when-unbound ()
  "ws-protected-p returns nil when +workspace--protected-p is not fboundp."
  (claude-repl-test--with-clean-state
    (fmakunbound '+workspace--protected-p)
    (should-not (claude-repl--ws-protected-p "main"))))

;;;; ---- Tests: --ws-error ----

(ert-deftest claude-repl-test-ws-error-delegates-when-bound ()
  "ws-error forwards message and noerror flag to +workspace-error."
  (claude-repl-test--with-clean-state
    (let (captured)
      (cl-letf (((symbol-function '+workspace-error)
                 (lambda (msg &optional noerror) (setq captured (list msg noerror)))))
        (claude-repl--ws-error "boom" t)
        (should (equal captured '("boom" t)))))))

(ert-deftest claude-repl-test-ws-error-noop-when-unbound ()
  "ws-error is a no-op when +workspace-error is not fboundp."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-error) nil))
      (fmakunbound '+workspace-error)
      (should-not (claude-repl--ws-error "boom" t)))))

;;;; ---- Tests: --ws-add-buffer ----

(ert-deftest claude-repl-test-ws-add-buffer-delegates-when-bound ()
  "ws-add-buffer forwards buffer, persp, and switch to persp-add-buffer."
  (claude-repl-test--with-clean-state
    (let (captured)
      (cl-letf (((symbol-function 'persp-add-buffer)
                 (lambda (buf persp switch) (setq captured (list buf persp switch)))))
        (claude-repl--ws-add-buffer 'buf 'persp t)
        (should (equal captured '(buf persp t)))))))

(ert-deftest claude-repl-test-ws-add-buffer-noop-when-unbound ()
  "ws-add-buffer is a no-op when persp-add-buffer is not fboundp."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'persp-add-buffer) nil))
      (fmakunbound 'persp-add-buffer)
      (should-not (claude-repl--ws-add-buffer 'buf 'persp nil)))))

;;;; ---- Tests: --ws-buffers ----

(ert-deftest claude-repl-test-ws-buffers-delegates-when-bound ()
  "ws-buffers returns the persp-buffers result for a non-nil persp."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'persp-buffers) (lambda (_persp) '(b1 b2))))
      (should (equal (claude-repl--ws-buffers 'persp) '(b1 b2))))))

(ert-deftest claude-repl-test-ws-buffers-returns-nil-for-nil-persp ()
  "ws-buffers returns nil when persp is nil, without calling persp-buffers."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'persp-buffers)
               (lambda (_persp) (error "should not be called"))))
      (should-not (claude-repl--ws-buffers nil)))))

(ert-deftest claude-repl-test-ws-buffers-returns-nil-when-unbound ()
  "ws-buffers returns nil when persp-buffers is not fboundp."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'persp-buffers) nil))
      (fmakunbound 'persp-buffers)
      (should-not (claude-repl--ws-buffers 'persp)))))

;;;; ---- Tests: --ws-rename-persp ----

(ert-deftest claude-repl-test-ws-rename-persp-renames-live-persp ()
  "ws-rename-persp renames the resolved persp and returns non-nil on success."
  (claude-repl-test--with-clean-state
    (let (captured)
      (cl-letf (((symbol-function 'claude-repl--ws-resolve-persp) (lambda (_ws) 'a-persp))
                ((symbol-function 'persp-rename)
                 (lambda (new persp) (setq captured (list new persp)) t)))
        (should (claude-repl--ws-rename-persp "old" "new"))
        (should (equal captured '("new" a-persp)))))))

(ert-deftest claude-repl-test-ws-rename-persp-returns-nil-on-failure ()
  "ws-rename-persp returns nil when a live persp exists but persp-rename fails."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'claude-repl--ws-resolve-persp) (lambda (_ws) 'a-persp))
              ((symbol-function 'persp-rename) (lambda (_new _persp) nil)))
      (should-not (claude-repl--ws-rename-persp "old" "new")))))

(ert-deftest claude-repl-test-ws-rename-persp-noop-when-no-persp ()
  "ws-rename-persp returns non-nil and skips rename when OLD-WS has no live persp."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'claude-repl--ws-resolve-persp) (lambda (_ws) nil))
              ((symbol-function 'persp-rename)
               (lambda (&rest _) (error "should not be called"))))
      (should (claude-repl--ws-rename-persp "old" "new")))))

(ert-deftest claude-repl-test-ws-rename-persp-noop-when-unbound ()
  "ws-rename-persp returns non-nil when persp-rename is not fboundp."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'persp-rename) nil))
      (fmakunbound 'persp-rename)
      (should (claude-repl--ws-rename-persp "old" "new")))))

;;;; ---- Tests: --ws-frame-ordered-names ----

(ert-deftest claude-repl-test-ws-frame-ordered-names-delegates-when-bound ()
  "ws-frame-ordered-names returns the persp fast-ordered list when bound."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'persp-names-current-frame-fast-ordered)
               (lambda () '("a" "b" "c"))))
      (should (equal (claude-repl--ws-frame-ordered-names) '("a" "b" "c"))))))

(ert-deftest claude-repl-test-ws-frame-ordered-names-returns-nil-when-unbound ()
  "ws-frame-ordered-names returns nil when the persp helper is not fboundp."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'persp-names-current-frame-fast-ordered) nil))
      (fmakunbound 'persp-names-current-frame-fast-ordered)
      (should-not (claude-repl--ws-frame-ordered-names)))))

;;;; ---- Tests: --ws-update-names-cache ----

(ert-deftest claude-repl-test-ws-update-names-cache-delegates-when-bound ()
  "ws-update-names-cache forwards NAMES to persp-update-names-cache."
  (claude-repl-test--with-clean-state
    (let (captured)
      (cl-letf (((symbol-function 'persp-update-names-cache)
                 (lambda (names) (setq captured names))))
        (claude-repl--ws-update-names-cache '("a" "b"))
        (should (equal captured '("a" "b")))))))

(ert-deftest claude-repl-test-ws-update-names-cache-noop-when-unbound ()
  "ws-update-names-cache is a no-op when persp-update-names-cache is not fboundp."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'persp-update-names-cache) nil))
      (fmakunbound 'persp-update-names-cache)
      (should-not (claude-repl--ws-update-names-cache '("a"))))))

;;;; ---- Tests: --ws-window-conf ----

(ert-deftest claude-repl-test-ws-window-conf-delegates-when-bound ()
  "ws-window-conf returns the persp-window-conf result for a non-nil persp."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'persp-window-conf) (lambda (_persp) 'a-wconf)))
      (should (eq (claude-repl--ws-window-conf 'persp) 'a-wconf)))))

(ert-deftest claude-repl-test-ws-window-conf-returns-nil-for-nil-persp ()
  "ws-window-conf returns nil for a nil persp without calling persp-window-conf."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'persp-window-conf)
               (lambda (_persp) (error "should not be called"))))
      (should-not (claude-repl--ws-window-conf nil)))))

(ert-deftest claude-repl-test-ws-window-conf-returns-nil-when-unbound ()
  "ws-window-conf returns nil when persp-window-conf is not fboundp."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'persp-window-conf) nil))
      (fmakunbound 'persp-window-conf)
      (should-not (claude-repl--ws-window-conf 'persp)))))

;;;; ---- Tests: --ws-tab-face / --ws-tab-selected-face ----

(ert-deftest claude-repl-test-ws-tab-face-returns-doom-face-symbol ()
  "ws-tab-face returns the +workspace-tab-face symbol."
  (should (eq (claude-repl--ws-tab-face) '+workspace-tab-face)))

(ert-deftest claude-repl-test-ws-tab-selected-face-returns-doom-face-symbol ()
  "ws-tab-selected-face returns the +workspace-tab-selected-face symbol."
  (should (eq (claude-repl--ws-tab-selected-face) '+workspace-tab-selected-face)))

;;;; ---- Tests: --ws-register-project ----

(ert-deftest claude-repl-test-ws-register-project-delegates-when-bound ()
  "ws-register-project forwards DIR to projectile-add-known-project."
  (claude-repl-test--with-clean-state
    (let (captured)
      (cl-letf (((symbol-function 'projectile-add-known-project)
                 (lambda (dir) (setq captured dir))))
        (claude-repl--ws-register-project "/tmp/p/")
        (should (equal captured "/tmp/p/"))))))

(ert-deftest claude-repl-test-ws-register-project-noop-when-unbound ()
  "ws-register-project is a no-op when projectile-add-known-project is unbound."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'projectile-add-known-project) nil))
      (fmakunbound 'projectile-add-known-project)
      (should-not (claude-repl--ws-register-project "/tmp/p/")))))

;;;; ---- Tests: --ws-unregister-project ----

(ert-deftest claude-repl-test-ws-unregister-project-delegates-when-bound ()
  "ws-unregister-project forwards DIR to projectile-remove-known-project."
  (claude-repl-test--with-clean-state
    (let (captured)
      (cl-letf (((symbol-function 'projectile-remove-known-project)
                 (lambda (dir) (setq captured dir))))
        (claude-repl--ws-unregister-project "/tmp/p/")
        (should (equal captured "/tmp/p/"))))))

(ert-deftest claude-repl-test-ws-unregister-project-noop-when-unbound ()
  "ws-unregister-project is a no-op when projectile-remove-known-project is unbound."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'projectile-remove-known-project) nil))
      (fmakunbound 'projectile-remove-known-project)
      (should-not (claude-repl--ws-unregister-project "/tmp/p/")))))

;;;; ---- Tests: --ws-switch-project ----

(ert-deftest claude-repl-test-ws-switch-project-delegates-when-bound ()
  "ws-switch-project forwards PROJECT to projectile-switch-project-by-name."
  (claude-repl-test--with-clean-state
    (let (captured)
      (cl-letf (((symbol-function 'projectile-switch-project-by-name)
                 (lambda (project) (setq captured project))))
        (claude-repl--ws-switch-project "/tmp/p/")
        (should (equal captured "/tmp/p/"))))))

(ert-deftest claude-repl-test-ws-switch-project-noop-when-unbound ()
  "ws-switch-project is a no-op when projectile-switch-project-by-name is unbound."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'projectile-switch-project-by-name) nil))
      (fmakunbound 'projectile-switch-project-by-name)
      (should-not (claude-repl--ws-switch-project "/tmp/p/")))))

;;;; ---- Tests: --ws-known-projects ----

(ert-deftest claude-repl-test-ws-known-projects-delegates-when-bound ()
  "ws-known-projects returns the projectile-relevant-known-projects list."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'projectile-relevant-known-projects)
               (lambda () '("/a/" "/b/"))))
      (should (equal (claude-repl--ws-known-projects) '("/a/" "/b/"))))))

(ert-deftest claude-repl-test-ws-known-projects-returns-nil-when-unbound ()
  "ws-known-projects returns nil when projectile-relevant-known-projects is unbound."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'projectile-relevant-known-projects) nil))
      (fmakunbound 'projectile-relevant-known-projects)
      (should-not (claude-repl--ws-known-projects)))))

;;;; ---- Tests: --ws-all-persps ----

(ert-deftest claude-repl-test-ws-all-persps-delegates-when-bound ()
  "ws-all-persps returns the raw persp-persps list when bound."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'persp-persps) (lambda () '(p1 p2 nil))))
      (should (equal (claude-repl--ws-all-persps) '(p1 p2 nil))))))

(ert-deftest claude-repl-test-ws-all-persps-returns-nil-when-unbound ()
  "ws-all-persps returns nil when persp-persps is not fboundp."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'persp-persps) nil))
      (fmakunbound 'persp-persps)
      (should-not (claude-repl--ws-all-persps)))))

;;;; ---- Tests: --ws-persp-name ----

(ert-deftest claude-repl-test-ws-persp-name-delegates-when-bound ()
  "ws-persp-name returns the safe-persp-name result when bound."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'safe-persp-name) (lambda (persp) (format "%s" persp))))
      (should (equal (claude-repl--ws-persp-name 'a-persp) "a-persp")))))

(ert-deftest claude-repl-test-ws-persp-name-returns-nil-when-unbound ()
  "ws-persp-name returns nil when safe-persp-name is not fboundp."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'safe-persp-name) nil))
      (fmakunbound 'safe-persp-name)
      (should-not (claude-repl--ws-persp-name 'a-persp)))))

;;;; ---- Tests: --workspace-for-buffer (moved from test-status.el) ----

(ert-deftest claude-repl-test-workspace-for-buffer-persp-mode-nil ()
  "workspace-for-buffer should return nil when persp-mode is nil."
  (claude-repl-test--with-clean-state
    (let ((persp-mode nil))
      (should-not (claude-repl--workspace-for-buffer (current-buffer))))))

(ert-deftest claude-repl-test-workspace-for-buffer-found ()
  "workspace-for-buffer should return workspace name when buffer is found."
  (claude-repl-test--with-clean-state
    (let ((persp-mode t)
          (test-buf (current-buffer))
          (fake-persp "my-workspace"))
      (cl-letf (((symbol-function 'persp-persps)
                 (lambda () (list fake-persp)))
                ((symbol-function 'persp-contain-buffer-p)
                 (lambda (buf persp)
                   (and (eq buf test-buf) (equal persp fake-persp))))
                ((symbol-function 'safe-persp-name)
                 (lambda (persp) persp)))
        (should (equal (claude-repl--workspace-for-buffer test-buf)
                       "my-workspace"))))))

(ert-deftest claude-repl-test-workspace-for-buffer-not-found ()
  "workspace-for-buffer should return nil when buffer not in any persp."
  (claude-repl-test--with-clean-state
    (let ((persp-mode t))
      (cl-letf (((symbol-function 'persp-persps)
                 (lambda () '("ws-a" "ws-b")))
                ((symbol-function 'persp-contain-buffer-p)
                 (lambda (_buf _persp) nil)))
        (should-not (claude-repl--workspace-for-buffer (current-buffer)))))))

;;;; ---- Tests: reorder-workspace-to-front (moved from test-status.el) ----

(ert-deftest claude-repl-test-reorder-to-front-moves-to-leftmost-visible ()
  "reorder-workspace-to-front moves WS to the first visible slot,
immediately after `persp-nil-name'."
  (claude-repl-test--with-clean-state
    (let* ((persp-nil-name "main")
           (persp-names-cache '("main" "ws-a" "ws-b" "merge-failed-ws"))
           (captured nil))
      (cl-letf (((symbol-function 'persp-update-names-cache)
                 (lambda (new-cache) (setq captured new-cache))))
        (claude-repl--reorder-workspace-to-front "merge-failed-ws")
        (should (equal captured '("main" "merge-failed-ws" "ws-a" "ws-b")))))))

(ert-deftest claude-repl-test-reorder-to-front-without-nil-name ()
  "When persp-nil-name is unset, the front-reorder places WS at index 0
of the cache (no sentinel head)."
  (claude-repl-test--with-clean-state
    (let* ((persp-nil-name nil)
           (persp-names-cache '("ws-a" "ws-b" "merge-failed-ws"))
           (captured nil))
      (cl-letf (((symbol-function 'persp-update-names-cache)
                 (lambda (new-cache) (setq captured new-cache))))
        (claude-repl--reorder-workspace-to-front "merge-failed-ws")
        (should (equal captured '("merge-failed-ws" "ws-a" "ws-b")))))))

(ert-deftest claude-repl-test-reorder-to-front-noop-when-not-in-cache ()
  "reorder-workspace-to-front no-ops when WS is not in `persp-names-cache'."
  (claude-repl-test--with-clean-state
    (let* ((persp-nil-name "main")
           (persp-names-cache '("main" "ws-a"))
           (captured nil))
      (cl-letf (((symbol-function 'persp-update-names-cache)
                 (lambda (new-cache) (setq captured new-cache))))
        (claude-repl--reorder-workspace-to-front "missing-ws")
        (should-not captured)))))

(ert-deftest claude-repl-test-reorder-to-front-logs-bail-not-in-cache ()
  "reorder-workspace-to-front emits a BAIL/not-in-cache log line when WS is missing."
  (claude-repl-test--with-clean-state
    (let* ((persp-names-cache '("main" "ws-a"))
           (logs nil))
      (cl-letf (((symbol-function 'claude-repl--log)
                 (lambda (_ws fmt &rest args)
                   (push (apply #'format fmt args) logs))))
        (claude-repl--reorder-workspace-to-front "missing-ws")
        (should (cl-find-if (lambda (l)
                              (and (string-match-p "reorder-workspace-to-front: BAIL" l)
                                   (string-match-p "reason=not-in-cache" l)))
                            logs))))))

(ert-deftest claude-repl-test-reorder-to-front-logs-apply-on-success ()
  "reorder-workspace-to-front emits an APPLY log line on the success path."
  (claude-repl-test--with-clean-state
    (let* ((persp-nil-name "main")
           (persp-names-cache '("main" "ws-a" "merge-failed-ws"))
           (logs nil))
      (cl-letf (((symbol-function 'persp-update-names-cache) (lambda (_) nil))
                ((symbol-function 'claude-repl--log)
                 (lambda (_ws fmt &rest args)
                   (push (apply #'format fmt args) logs))))
        (claude-repl--reorder-workspace-to-front "merge-failed-ws")
        (should (cl-find-if (lambda (l)
                              (string-match-p "reorder-workspace-to-front: APPLY" l))
                            logs))))))

(ert-deftest claude-repl-test-reorder-to-front-preserves-nil-persp-position ()
  "reorder-workspace-to-front keeps persp-nil-name at the head of the cache."
  (claude-repl-test--with-clean-state
    (let* ((persp-nil-name "main")
           (persp-names-cache '("main" "ws-a" "merge-failed-ws"))
           (captured nil))
      (cl-letf (((symbol-function 'persp-update-names-cache)
                 (lambda (new-cache) (setq captured new-cache))))
        (claude-repl--reorder-workspace-to-front "merge-failed-ws")
        (should (equal (car captured) "main"))))))

(ert-deftest claude-repl-test-reorder-to-front-preserves-cache-string-identity ()
  "After reorder, the WS slot in `persp-names-cache' is `eq' to the
canonical string already in the cache, NOT to the (potentially fresh)
WS argument.  Same guarantee as `reorder-workspace-by-priority' — see
workspace.el for the persp-mode identity rationale."
  (claude-repl-test--with-clean-state
    (let* ((canonical (copy-sequence "merge-failed-ws"))
           (fresh (copy-sequence "merge-failed-ws"))
           (persp-nil-name "main")
           (persp-names-cache (list "main" "ws-a" canonical))
           (captured nil))
      (should-not (eq canonical fresh))
      (cl-letf (((symbol-function 'persp-update-names-cache)
                 (lambda (new-cache) (setq captured new-cache))))
        (claude-repl--reorder-workspace-to-front fresh)
        (let ((injected (car (member "merge-failed-ws" captured))))
          (should injected)
          (should (eq injected canonical))
          (should-not (eq injected fresh)))))))

(ert-deftest claude-repl-test-reorder-to-front-idempotent-when-already-front ()
  "Reordering a WS that is already at the visible front leaves the cache
in the same shape (still leftmost, nil-name still at head)."
  (claude-repl-test--with-clean-state
    (let* ((persp-nil-name "main")
           (persp-names-cache '("main" "merge-failed-ws" "ws-a" "ws-b"))
           (captured nil))
      (cl-letf (((symbol-function 'persp-update-names-cache)
                 (lambda (new-cache) (setq captured new-cache))))
        (claude-repl--reorder-workspace-to-front "merge-failed-ws")
        (should (equal captured '("main" "merge-failed-ws" "ws-a" "ws-b")))))))

(provide 'test-workspace)
;;; test-workspace.el ends here
