;;; test-workspace.el --- ERT tests for agent-repl workspace.el -*- lexical-binding: t; -*-

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

;; `defvar' with no value marks a symbol special only within the file that
;; carries it, so workspace.el's declaration does not reach this file — a
;; `let' here would bind lexically and never meet the dynamic binding
;; `agent-repl--ws-remove-buffer' establishes.  Redeclare it to test that.
(defvar persp-autokill-buffer-on-remove)

;;;; ---- Tests: ws-get / ws-put (moved from test-core.el) ----

(ert-deftest agent-repl-test-ws-get-nonexistent-workspace ()
  "ws-get on non-existent workspace should return nil."
  (agent-repl-test--with-clean-state
    (should-not (agent-repl--ws-get "nonexistent" :status))))

(ert-deftest agent-repl-test-ws-get-nonexistent-key ()
  "ws-get for non-existent key on existing workspace should return nil."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :status "active")
    (should-not (agent-repl--ws-get "ws1" :nonexistent-key))))

(ert-deftest agent-repl-test-ws-get-zero-value ()
  "ws-get should return 0 when key is set to 0 (not confuse with nil)."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :counter 0)
    (should (equal (agent-repl--ws-get "ws1" :counter) 0))))

(ert-deftest agent-repl-test-ws-get-empty-string-value ()
  "ws-get should return empty string when key is set to empty string."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :name "")
    (should (equal (agent-repl--ws-get "ws1" :name) ""))))

;;;; ---- Tests: ws-plist --------------------------------------------------

(ert-deftest agent-repl-test-ws-plist-returns-complete-copy ()
  "ws-plist returns every field without exposing the owned top-level plist."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
    (agent-repl--ws-put "ws1" :priority :p1)
    (let ((snapshot (agent-repl--ws-plist "ws1")))
      (should (equal (plist-get snapshot :project-dir) "/tmp/ws1"))
      (should (eq (plist-get snapshot :priority) :p1))
      (setf (plist-get snapshot :priority) :p9)
      (should (eq (agent-repl--ws-get "ws1" :priority) :p1)))))

(ert-deftest agent-repl-test-ws-plist-allows-known-tombstone ()
  "ws-plist keeps identity state queryable after a workspace tombstones."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
    (agent-repl--ws-del "ws1")
    (let ((snapshot (agent-repl--ws-plist "ws1")))
      (should (equal (plist-get snapshot :project-dir) "/tmp/ws1"))
      (should (plist-get snapshot :nuked-at)))))

(ert-deftest agent-repl-test-ws-plist-rejects-unknown-workspace ()
  "ws-plist makes an invalid serialization target fail loudly."
  (agent-repl-test--with-clean-state
    (should-error (agent-repl--ws-plist "missing") :type 'user-error)))

(ert-deftest agent-repl-test-ws-put-new-workspace ()
  "ws-put to a brand new workspace should create the entry."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "new-ws" :status "ready")
    (should (equal (agent-repl--ws-get "new-ws" :status) "ready"))))

(ert-deftest agent-repl-test-ws-put-overwrite ()
  "ws-put should overwrite an existing key."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :status "old")
    (agent-repl--ws-put "ws1" :status "new")
    (should (equal (agent-repl--ws-get "ws1" :status) "new"))))

(ert-deftest agent-repl-test-ws-put-nil-value ()
  "ws-put with nil value should set key to nil."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :status "active")
    (agent-repl--ws-put "ws1" :status nil)
    (should-not (agent-repl--ws-get "ws1" :status))))

(ert-deftest agent-repl-test-ws-put-multiple-keys ()
  "ws-put should support multiple keys on the same workspace."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :status "ready")
    (agent-repl--ws-put "ws1" :priority "p1")
    (agent-repl--ws-put "ws1" :counter 42)
    (should (equal (agent-repl--ws-get "ws1" :status) "ready"))
    (should (equal (agent-repl--ws-get "ws1" :priority) "p1"))
    (should (equal (agent-repl--ws-get "ws1" :counter) 42))))

(ert-deftest agent-repl-test-ws-put-stub-create-emits-noisy-log ()
  "ws-put that creates a fresh entry with a non-:project-dir key should
emit a noisy unconditional log via `agent-repl--do-log'."
  (agent-repl-test--with-clean-state
    (let ((log-calls nil))
      (cl-letf (((symbol-function 'agent-repl--do-log)
                 (lambda (ws fmt args &optional _err)
                   (push (list ws fmt args) log-calls))))
        (agent-repl--ws-put "stub-ws" :priority "p1"))
      (should (= 1 (length log-calls)))
      (should (string-match-p "STUB-CREATE" (nth 1 (car log-calls)))))))

(ert-deftest agent-repl-test-ws-put-project-dir-first-no-log ()
  "ws-put that creates an entry by setting :project-dir as the first key
should not emit the stub-create log."
  (agent-repl-test--with-clean-state
    (let ((log-calls nil))
      (cl-letf (((symbol-function 'agent-repl--do-log)
                 (lambda (ws fmt args &optional _err)
                   (push (list ws fmt args) log-calls))))
        (agent-repl--ws-put "good-ws" :project-dir "/some/dir"))
      (should (null log-calls)))))

(ert-deftest agent-repl-test-ws-put-existing-entry-no-log ()
  "ws-put on an existing entry should not emit the stub-create log
even when writing a non-:project-dir key on an entry that itself
has no :project-dir (no new entry is being created)."
  (agent-repl-test--with-clean-state
    ;; Seed an entry via :project-dir first so it exists.
    (agent-repl--ws-put "ws1" :project-dir "/some/dir")
    (let ((log-calls nil))
      (cl-letf (((symbol-function 'agent-repl--do-log)
                 (lambda (ws fmt args &optional _err)
                   (push (list ws fmt args) log-calls))))
        (agent-repl--ws-put "ws1" :priority "p1"))
      (should (null log-calls)))))

(ert-deftest agent-repl-test-ws-put-stub-log-includes-caller-trace ()
  "Stub-create log payload should include a caller-trace string so the
producer of the leak can be identified from the message alone."
  (agent-repl-test--with-clean-state
    (let ((log-calls nil))
      (cl-letf (((symbol-function 'agent-repl--do-log)
                 (lambda (ws fmt args &optional _err)
                   (push (list ws fmt args) log-calls))))
        (agent-repl--ws-put "stub-ws" :priority "p1"))
      (should (= 1 (length log-calls)))
      (let* ((args (nth 2 (car log-calls)))
             (trace (car (last args))))
        (should (stringp trace))
        (should (> (length trace) 0))))))

;;;; ---- Tests: ws-del (tombstone semantics; moved from test-core.el) ----

(ert-deftest agent-repl-test-ws-del-clears-runtime-key ()
  "ws-del clears every key listed in `agent-repl--ws-runtime-keys'.
Asserts a representative runtime key (`:flashing') is reset to nil so
post-nuke render passes don't paint a stale flash on a tombstoned tab."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
    (agent-repl--ws-put "ws1" :flashing t)
    (agent-repl--ws-del "ws1")
    (should-not (agent-repl--ws-get "ws1" :flashing))))

(ert-deftest agent-repl-test-ws-del-clears-pushed-render-state ()
  "ws-del clears `:pushed-render-state' — a daemon-pushed runtime key tied
to the session, so it must not outlive the tombstone.
\(The former :queued-messages runtime key was removed in the S9 queue-plane
endgame; this covers the same tombstone-clearing contract on a live key.)"
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
    (agent-repl--ws-put "ws1" :pushed-render-state :thinking)
    (agent-repl--ws-del "ws1")
    (should-not (agent-repl--ws-get "ws1" :pushed-render-state))))

(ert-deftest agent-repl-test-ws-del-clears-incoming-session-id ()
  "ws-del clears `:incoming-session-id' — a staged id belongs to the
killed session and must never survive into a revived workspace, where
a later activity event could promote a dead session as the resume
target."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
    (agent-repl--ws-put "ws1" :incoming-session-id "staged-uuid")
    (agent-repl--ws-del "ws1")
    (should-not (agent-repl--ws-get "ws1" :incoming-session-id))))

(ert-deftest agent-repl-test-ws-del-nonexistent ()
  "ws-del on a non-existent workspace should be a no-op."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-del "nonexistent")
    ;; Should not error and should not synthesize an entry.
    (should-not (gethash "nonexistent" agent-repl--workspaces))))

(ert-deftest agent-repl-test-ws-del-preserves-project-dir ()
  "ws-del preserves `:project-dir' across the tombstone — the entire
point of the tombstone model.  Without this guarantee, `--ws-dir'
callers would resume firing `no :project-dir for workspace X' errors
on persps that outlive their agent-repl session."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
    (agent-repl--ws-del "ws1")
    (should (equal (agent-repl--ws-get "ws1" :project-dir) "/tmp/ws1"))))

(ert-deftest agent-repl-test-ws-del-preserves-priority ()
  "ws-del preserves `:priority' — identity/historical key, not runtime.
Re-creating a workspace with the same name should resume at its prior
priority badge without the user having to re-rank it."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
    (agent-repl--ws-put "ws1" :priority :p1)
    (agent-repl--ws-del "ws1")
    (should (eq (agent-repl--ws-get "ws1" :priority) :p1))))

(ert-deftest agent-repl-test-ws-del-stamps-nuked-at ()
  "ws-del stamps `:nuked-at' with a non-nil time value — the marker
read by `--ws-live-p' and the snapshot persistence layer to distinguish
tombstones from live entries."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
    (agent-repl--ws-del "ws1")
    (should (agent-repl--ws-get "ws1" :nuked-at))))

(ert-deftest agent-repl-test-ws-del-bumps-last-killed-at ()
  "ws-del bumps `:last-killed-at' so the picker's sort-by-last-killed
sees the tombstone immediately rather than waiting for an external
caller to stamp the timestamp."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
    (agent-repl--ws-del "ws1")
    (should (agent-repl--ws-get "ws1" :last-killed-at))))

(ert-deftest agent-repl-test-ws-del-hook-runs-before-runtime-key-clear ()
  "`agent-repl-ws-del-hook' fires while runtime keys are still readable.
The frontend session/webview release handlers depend on reading
`:frontend-session-id' / `:frontend-buffer' pre-clear; a regression
that moves the hook after the clear loop would silently strand daemon
sessions and WKWebViews on every nuke."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
    (agent-repl--ws-put "ws1" :frontend-session-id "s_live")
    (let ((seen 'unset)
          (agent-repl-ws-del-hook nil))
      (add-hook 'agent-repl-ws-del-hook
                (lambda (ws)
                  (setq seen (agent-repl--ws-get ws :frontend-session-id))))
      ;; Act
      (agent-repl--ws-del "ws1")
      ;; Assert — the hook observed the pre-clear value, and the
      ;; tombstone cleared it afterwards.
      (should (equal seen "s_live"))
      (should (null (agent-repl--ws-get "ws1" :frontend-session-id))))))

(ert-deftest agent-repl-test-ws-del-keeps-entry-in-hash ()
  "ws-del leaves the hash entry in place (tombstone, not remhash).
This is the structural inverse of the pre-tombstone behavior — pinning
so a regression that brings remhash back is caught immediately."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
    (agent-repl--ws-del "ws1")
    (should (gethash "ws1" agent-repl--workspaces))))

(ert-deftest agent-repl-test-ws-del-logs-had-entry-true ()
  "ws-del logs `had-entry=t' when the workspace was registered."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
    (let ((logged nil))
      (cl-letf (((symbol-function 'agent-repl--log)
                 (lambda (_ws fmt &rest args)
                   (setq logged (apply #'format fmt args)))))
        (agent-repl--ws-del "ws1")
        (should (string-match-p "ws-del:" logged))
        (should (string-match-p "had-entry=t" logged))))))

(ert-deftest agent-repl-test-ws-del-logs-had-entry-nil ()
  "ws-del logs `had-entry=nil' when the workspace was not registered."
  (agent-repl-test--with-clean-state
    (let ((logged nil))
      (cl-letf (((symbol-function 'agent-repl--log)
                 (lambda (_ws fmt &rest args)
                   (setq logged (apply #'format fmt args)))))
        (agent-repl--ws-del "nonexistent")
        (should (string-match-p "ws-del:" logged))
        (should (string-match-p "had-entry=nil" logged))))))

(ert-deftest agent-repl-test-ws-del-clears-peer-source-ws-name-cache ()
  "`--ws-del' must clear `:source-ws-name' on peers that cached the
deleted ws as their resolved source.  Without the sweep, a future
workspace re-using the deleted name would be returned as a parent it
isn't (different `:project-dir').  Asserts the sweep targets exactly
the affected peers."
  (agent-repl-test--with-clean-state
    (puthash "parent" '(:project-dir "/tmp/parent")
             agent-repl--workspaces)
    (puthash "child"  '(:project-dir "/tmp/child"
                                     :source-ws-dir "/tmp/parent"
                                     :source-ws-name "parent")
             agent-repl--workspaces)
    (puthash "unrelated" '(:project-dir "/tmp/u"
                                        :source-ws-name "someone-else")
             agent-repl--workspaces)
    (agent-repl--ws-del "parent")
    (should-not (agent-repl--ws-get "child" :source-ws-name))
    (should (equal (agent-repl--ws-get "unrelated" :source-ws-name)
                   "someone-else"))))

(ert-deftest agent-repl-test-ws-del-tombstones-entry-not-removes ()
  "`--ws-del' tombstones the target's own entry rather than removing it —
the post-tombstone-refactor invariant.  The peer-cache sweep above still
fires; this test pins that the same call also leaves the target entry
intact (just with `:nuked-at' stamped)."
  (agent-repl-test--with-clean-state
    (puthash "doomed" '(:project-dir "/tmp/x") agent-repl--workspaces)
    (agent-repl--ws-del "doomed")
    (should (gethash "doomed" agent-repl--workspaces))
    (should (agent-repl--ws-get "doomed" :nuked-at))
    (should (equal (agent-repl--ws-get "doomed" :project-dir) "/tmp/x"))))

;;;; ---- Tests: ws-live-p (moved from test-core.el) ----

(ert-deftest agent-repl-test-ws-live-p-returns-t-for-live-entry ()
  "ws-live-p returns non-nil for a fresh hash entry with no tombstone."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
    (should (agent-repl--ws-live-p "ws1"))))

(ert-deftest agent-repl-test-ws-live-p-returns-nil-for-tombstone ()
  "ws-live-p returns nil for a tombstoned entry — the predicate that
keeps tab-bar/picker/state-updater from surfacing nuked workspaces."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
    (agent-repl--ws-del "ws1")
    (should-not (agent-repl--ws-live-p "ws1"))))

(ert-deftest agent-repl-test-ws-live-p-returns-nil-for-unknown ()
  "ws-live-p returns nil when no hash entry exists at all."
  (agent-repl-test--with-clean-state
    (should-not (agent-repl--ws-live-p "never-seen"))))

;;;; ---- Tests: live-ws-names (moved from test-core.el) ----

(ert-deftest agent-repl-test-live-ws-names-excludes-tombstones ()
  "live-ws-names returns only non-tombstoned hash keys, regardless of
insertion order — the single helper every hash iterator routes through
to avoid surfacing nuked workspaces."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "alive" :project-dir "/tmp/alive")
    (agent-repl--ws-put "dead" :project-dir "/tmp/dead")
    (agent-repl--ws-del "dead")
    (let ((names (agent-repl--live-ws-names)))
      (should (member "alive" names))
      (should-not (member "dead" names)))))

(ert-deftest agent-repl-test-live-ws-names-empty-hash ()
  "live-ws-names returns nil (not an error) when the hash has no entries."
  (agent-repl-test--with-clean-state
    (should-not (agent-repl--live-ws-names))))

;;;; ---- Tests: project-pollable workspace partition ----

(ert-deftest agent-repl-test-ws-project-pollable-p-requires-live-project ()
  "Only a live entry with `:project-dir' is eligible for project polling."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "project" :project-dir "/tmp/project")
    (agent-repl--ws-put "placeholder" :agent-state :idle)
    (agent-repl--ws-put "dead" :project-dir "/tmp/dead")
    (agent-repl--ws-del "dead")
    (should (equal (agent-repl--ws-project-pollable-p "project")
                   "/tmp/project"))
    (should-not (agent-repl--ws-project-pollable-p "placeholder"))
    (should-not (agent-repl--ws-project-pollable-p "dead"))
    (should-not (agent-repl--ws-project-pollable-p "unknown"))))

(ert-deftest agent-repl-test-ws-project-poll-partition-separates-placeholders ()
  "Project poll partition excludes tombstones and reports live placeholders."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "project" :project-dir "/tmp/project")
    (agent-repl--ws-put "main" :agent-state :idle)
    (agent-repl--ws-put "none" :repl-state :inactive)
    (agent-repl--ws-put "dead" :project-dir "/tmp/dead")
    (agent-repl--ws-del "dead")
    (pcase-let ((`(,pollable . ,placeholders)
                 (agent-repl--ws-project-poll-partition)))
      (should (equal pollable '("project")))
      (should (equal (sort placeholders #'string<) '("main" "none"))))))

;;;; ---- Tests: --ws-dir-owner ----

(ert-deftest agent-repl-test-ws-dir-owner-finds-live-owner ()
  "ws-dir-owner returns a live workspace owning the canonical dir."
  (agent-repl-test--with-clean-state
    (let ((dir (agent-repl--path-canonical "/home/user/proj")))
      (agent-repl--ws-put "owner" :project-dir dir)
      (should (equal (agent-repl--ws-dir-owner dir) "owner")))))

(ert-deftest agent-repl-test-ws-dir-owner-excludes-self ()
  "ws-dir-owner excludes the EXCEPT workspace, so re-init of the owner finds
no OTHER owner."
  (agent-repl-test--with-clean-state
    (let ((dir (agent-repl--path-canonical "/home/user/proj")))
      (agent-repl--ws-put "owner" :project-dir dir)
      (should-not (agent-repl--ws-dir-owner dir "owner")))))

(ert-deftest agent-repl-test-ws-dir-owner-ignores-tombstoned ()
  "ws-dir-owner ignores a tombstoned (`:nuked-at') entry owning the dir, so a
dead shadow never counts as the owner."
  (agent-repl-test--with-clean-state
    (let ((dir (agent-repl--path-canonical "/home/user/proj")))
      (agent-repl--ws-put "dead" :project-dir dir)
      (agent-repl--ws-put "dead" :nuked-at '(1 2 3 4))
      (should-not (agent-repl--ws-dir-owner dir)))))

;;;; ---- Tests: --ws-known-p ----

(ert-deftest agent-repl-test-ws-known-p-returns-t-for-live-entry ()
  "A workspace with a hash entry and no :nuked-at is known."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/tmp/x")
    (should (agent-repl--ws-known-p "ws1"))))

(ert-deftest agent-repl-test-ws-known-p-returns-t-for-tombstoned-entry ()
  "A tombstoned workspace (entry + :nuked-at set) is still known."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/tmp/x")
    (agent-repl--ws-del "ws1")
    (should (agent-repl--ws-known-p "ws1"))))

(ert-deftest agent-repl-test-ws-known-p-returns-nil-for-unknown ()
  "A workspace name that has never been registered is not known."
  (agent-repl-test--with-clean-state
    (should-not (agent-repl--ws-known-p "never-registered"))))

(ert-deftest agent-repl-test-ws-known-p-returns-t-for-empty-plist ()
  "A workspace whose plist is the empty list is still present."
  (agent-repl-test--with-clean-state
    (puthash "ws1" nil agent-repl--workspaces)
    (should (agent-repl--ws-known-p "ws1"))))

;;;; ---- Tests: --ws-require-known ----

(ert-deftest agent-repl-test-ws-require-known-passes-for-known ()
  "--ws-require-known returns nil (no error) when ws is known."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/tmp/x")
    (should-not (agent-repl--ws-require-known "ws1" "ctx"))))

(ert-deftest agent-repl-test-ws-require-known-errors-for-unknown ()
  "--ws-require-known signals user-error when ws is not known."
  (agent-repl-test--with-clean-state
    (should-error (agent-repl--ws-require-known "missing" "ctx")
                  :type 'user-error)))

(ert-deftest agent-repl-test-ws-require-known-includes-context-in-message ()
  "The error message mentions the CONTEXT argument so callers identify themselves."
  (agent-repl-test--with-clean-state
    (condition-case err
        (progn (agent-repl--ws-require-known "missing" "render-status")
               (ert-fail "expected user-error"))
      (user-error
       (should (string-match-p "render-status" (error-message-string err)))))))

;; The old --ws-render-status derivation tests (idle-async from
;; :async-live, the :agent-state / :repl-state / :merging precedence
;; ladder) were replaced in the agent-shim cutover (design §10): the
;; function is now a pure lookup of the daemon-pushed :pushed-render-state
;; key.  See the ";;;; ---- Tests: --ws-render-status (daemon-pushed
;; lookup)" section below for the new coverage.

;;;; ---- Tests: --ws-tombstoned-p ----

(ert-deftest agent-repl-test-ws-tombstoned-p-returns-t-after-ws-del ()
  "A workspace returns t for tombstoned after --ws-del runs on it."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/tmp/x")
    (agent-repl--ws-del "ws1")
    (should (agent-repl--ws-tombstoned-p "ws1"))))

(ert-deftest agent-repl-test-ws-tombstoned-p-returns-nil-for-live-entry ()
  "A live workspace (no :nuked-at) is not tombstoned."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/tmp/x")
    (should-not (agent-repl--ws-tombstoned-p "ws1"))))

(ert-deftest agent-repl-test-ws-tombstoned-p-returns-nil-for-unknown ()
  "An unknown workspace is not tombstoned (it is neither live nor tombstoned)."
  (agent-repl-test--with-clean-state
    (should-not (agent-repl--ws-tombstoned-p "missing"))))

(ert-deftest agent-repl-test-ws-tombstoned-p-partition-with-live-p ()
  "live and tombstoned are mutually exclusive over known workspaces."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/tmp/x")
    ;; Arrange: live ws.
    (should (agent-repl--ws-live-p "ws1"))
    (should-not (agent-repl--ws-tombstoned-p "ws1"))
    ;; Act: tombstone it.
    (agent-repl--ws-del "ws1")
    ;; Assert: now the inverse.
    (should-not (agent-repl--ws-live-p "ws1"))
    (should (agent-repl--ws-tombstoned-p "ws1"))))

;;;; ---- Tests: --ws-hide-tombstoned-p ----

(ert-deftest agent-repl-test-ws-hide-tombstoned-p-returns-t-when-both-set ()
  "A workspace tombstoned by the hide flow returns t."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "hidden-ws" :project-dir "/tmp/x")
    (agent-repl--ws-put "hidden-ws" :hidden-project-dir t)
    (agent-repl--ws-del "hidden-ws")
    (should (agent-repl--ws-hide-tombstoned-p "hidden-ws"))))

(ert-deftest agent-repl-test-ws-hide-tombstoned-p-returns-nil-for-nuke-tombstoned ()
  "A workspace tombstoned without the hide marker returns nil even though it is tombstoned."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "nuked-ws" :project-dir "/tmp/x")
    (agent-repl--ws-del "nuked-ws")
    (should (agent-repl--ws-tombstoned-p "nuked-ws"))
    (should-not (agent-repl--ws-hide-tombstoned-p "nuked-ws"))))

(ert-deftest agent-repl-test-ws-hide-tombstoned-p-returns-nil-for-live-marker ()
  "A live workspace carrying the marker but no :nuked-at returns nil.
Predicate is a conjunction of tombstone state AND reason marker."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "live-ws" :project-dir "/tmp/x")
    (agent-repl--ws-put "live-ws" :hidden-project-dir t)
    (should-not (agent-repl--ws-hide-tombstoned-p "live-ws"))))

(ert-deftest agent-repl-test-ws-hide-tombstoned-p-returns-nil-for-unknown ()
  "An unregistered workspace returns nil."
  (agent-repl-test--with-clean-state
    (should-not (agent-repl--ws-hide-tombstoned-p "never-seen"))))

;;;; ---- Tests: --ws-hide-tombstoned-names ----

(ert-deftest agent-repl-test-ws-hide-tombstoned-names-returns-hide-tombstones-only ()
  "Enumerator returns hide-tombstoned ws but excludes nuke-tombstoned and live ws."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "hidden1" :project-dir "/tmp/a")
    (agent-repl--ws-put "hidden1" :hidden-project-dir t)
    (agent-repl--ws-del "hidden1")
    (agent-repl--ws-put "nuked"   :project-dir "/tmp/b")
    (agent-repl--ws-del "nuked")
    (agent-repl--ws-put "live"    :project-dir "/tmp/c")
    (let ((names (agent-repl--ws-hide-tombstoned-names)))
      (should (equal names '("hidden1")))
      (should-not (member "nuked" names))
      (should-not (member "live" names)))))

(ert-deftest agent-repl-test-ws-hide-tombstoned-names-sorted-by-name ()
  "Returned names are sorted lexicographically for deterministic restore order."
  (agent-repl-test--with-clean-state
    (dolist (n '("c" "a" "b"))
      (agent-repl--ws-put n :project-dir (concat "/tmp/" n))
      (agent-repl--ws-put n :hidden-project-dir t)
      (agent-repl--ws-del n))
    (should (equal (agent-repl--ws-hide-tombstoned-names) '("a" "b" "c")))))

(ert-deftest agent-repl-test-ws-hide-tombstoned-names-empty-when-no-matches ()
  "Returns nil when no workspace carries the marker."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "live" :project-dir "/tmp/x")
    (should-not (agent-repl--ws-hide-tombstoned-names))))

;;;; ---- Tests: --ws-render-status nil for hide-tombstoned ----

(ert-deftest agent-repl-test-ws-render-status-nil-for-hide-tombstoned ()
  "Render-status returns nil for hide-tombstoned ws, collapsed with nuke-tombstoned."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "hidden" :project-dir "/tmp/x")
    (agent-repl--ws-put "hidden" :agent-state :thinking)
    (agent-repl--ws-put "hidden" :hidden-project-dir t)
    (agent-repl--ws-del "hidden")
    (should-not (agent-repl--ws-render-status "hidden"))))

;;;; ---- Tests: --ws-open-p ----

(ert-deftest agent-repl-test-ws-open-p-returns-t-when-in-persp-cache ()
  "A known workspace present in persp-names-cache is open."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/tmp/x")
    (let ((persp-names-cache '("ws1" "other")))
      (should (agent-repl--ws-open-p "ws1")))))

(ert-deftest agent-repl-test-ws-open-p-returns-nil-when-not-in-persp-cache ()
  "A known workspace NOT present in persp-names-cache is not open."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/tmp/x")
    (let ((persp-names-cache '("other")))
      (should-not (agent-repl--ws-open-p "ws1")))))

(ert-deftest agent-repl-test-ws-open-p-errors-for-unknown ()
  "An unknown workspace name signals user-error rather than returning nil."
  (agent-repl-test--with-clean-state
    (let ((persp-names-cache '("missing")))
      (should-error (agent-repl--ws-open-p "missing") :type 'user-error))))

(ert-deftest agent-repl-test-ws-open-p-decouples-from-tombstone ()
  "A tombstoned ws can still be `open' if persp-names-cache still lists it."
  ;; This documents the legitimate divergence between the two data
  ;; sources: tab-bar membership (persp-names-cache) and hash liveness
  ;; (--ws-live-p) are NOT the same thing.  See `--ws-open-p' docstring.
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/tmp/x")
    (agent-repl--ws-del "ws1")
    (let ((persp-names-cache '("ws1")))
      (should (agent-repl--ws-tombstoned-p "ws1"))
      (should (agent-repl--ws-open-p "ws1")))))

(ert-deftest agent-repl-test-ws-open-p-returns-nil-when-persp-cache-unbound ()
  "--ws-open-p returns nil rather than erroring when persp-names-cache is unbound."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/tmp/x")
    (let (persp-names-cache)
      ;; Unbind the symbol entirely for the duration of this test.
      (makunbound 'persp-names-cache)
      (unwind-protect
          (should-not (agent-repl--ws-open-p "ws1"))
        ;; Restore: rebind to an empty list so other tests don't trip
        ;; on the unbound state.
        (setq persp-names-cache nil)))))

;;;; ---- Tests: --ws-list-names ------------------------------------------

(ert-deftest agent-repl-test-ws-list-names-intersects-cache-and-known ()
  "Returns names that are BOTH in persp-names-cache AND --ws-known-p."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "known-and-open" :project-dir "/tmp/a")
    (agent-repl--ws-put "known-not-open" :project-dir "/tmp/b")
    (let ((persp-names-cache '("known-and-open" "unknown-in-cache")))
      (let ((result (agent-repl--ws-list-names)))
        (should (member "known-and-open" result))
        (should-not (member "known-not-open" result))
        (should-not (member "unknown-in-cache" result))))))

(ert-deftest agent-repl-test-ws-list-names-excludes-persp-nil-name ()
  "The persp-nil-name sentinel is filtered out even when it appears in cache and would be known."
  (agent-repl-test--with-clean-state
    ;; Arrange a ws whose name equals the nil sentinel (pathological but
    ;; documented elsewhere as a guard pattern).
    (let ((persp-nil-name "none"))
      (agent-repl--ws-put "none" :project-dir "/tmp/x")
      (let ((persp-names-cache '("none" "real-ws")))
        (agent-repl--ws-put "real-ws" :project-dir "/tmp/y")
        (let ((result (agent-repl--ws-list-names)))
          (should-not (member "none" result))
          (should (member "real-ws" result)))))))

(ert-deftest agent-repl-test-ws-list-names-preserves-cache-order ()
  "Order of results follows persp-names-cache order so tab-bar order is stable."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "a" :project-dir "/tmp/a")
    (agent-repl--ws-put "b" :project-dir "/tmp/b")
    (agent-repl--ws-put "c" :project-dir "/tmp/c")
    (let ((persp-names-cache '("c" "a" "b")))
      (should (equal '("c" "a" "b") (agent-repl--ws-list-names))))))

(ert-deftest agent-repl-test-ws-list-names-returns-nil-when-cache-unbound ()
  "Returns nil rather than erroring when persp-names-cache is unbound."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/tmp/x")
    (let (persp-names-cache)
      (makunbound 'persp-names-cache)
      (unwind-protect
          (should-not (agent-repl--ws-list-names))
        (setq persp-names-cache nil)))))

(ert-deftest agent-repl-test-ws-list-names-includes-tombstoned-if-in-cache ()
  "A tombstoned ws that still appears in persp-names-cache is listed.
This case is rare in production (the nuke path removes from cache
before tombstoning), but the predicate is `--ws-known-p' which is
true for tombstoned, so the list includes it.  Documents the
contract explicitly so a renderer relying on it stays predictable."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/tmp/x")
    (agent-repl--ws-del "ws1")
    (let ((persp-names-cache '("ws1")))
      (should (member "ws1" (agent-repl--ws-list-names))))))

;;;; ---- Tests: --ws-all-names -------------------------------------------

(ert-deftest agent-repl-test-ws-all-names-delegates-when-bound ()
  "--ws-all-names returns the raw +workspace-list-names value when bound."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-list-names)
               (lambda () '("a" "b" "c"))))
      (should (equal (agent-repl--ws-all-names) '("a" "b" "c"))))))

(ert-deftest agent-repl-test-ws-all-names-unfiltered-by-known ()
  "--ws-all-names returns names even when agent-repl never registered them."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-list-names)
               (lambda () '("stray-persp"))))
      (should (equal (agent-repl--ws-all-names) '("stray-persp"))))))

(ert-deftest agent-repl-test-ws-all-names-returns-nil-when-unbound ()
  "--ws-all-names returns nil when +workspace-list-names is not fboundp."
  (agent-repl-test--with-clean-state
    (fmakunbound '+workspace-list-names)
    (should-not (agent-repl--ws-all-names))))

;;;; ---- Tests: --ws-tombstoned-names ------------------------------------

(ert-deftest agent-repl-test-ws-tombstoned-names-returns-tombstones ()
  "--ws-tombstoned-names returns all tombstoned workspace names."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "live-ws"  :project-dir "/tmp/live")
    (agent-repl--ws-put "dead-ws"  :project-dir "/tmp/dead")
    (agent-repl--ws-del "dead-ws")
    (let ((result (agent-repl--ws-tombstoned-names)))
      (should (equal result '("dead-ws")))
      (should-not (member "live-ws" result)))))

(ert-deftest agent-repl-test-ws-tombstoned-names-empty-when-none ()
  "--ws-tombstoned-names returns nil when no workspace is tombstoned."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
    (should-not (agent-repl--ws-tombstoned-names))))

(ert-deftest agent-repl-test-ws-tombstoned-names-sorted ()
  "--ws-tombstoned-names returns names in alphabetical order."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "charlie" :project-dir "/tmp/c")
    (agent-repl--ws-put "alpha"   :project-dir "/tmp/a")
    (agent-repl--ws-put "bravo"   :project-dir "/tmp/b")
    (agent-repl--ws-del "charlie")
    (agent-repl--ws-del "alpha")
    (agent-repl--ws-del "bravo")
    (should (equal (agent-repl--ws-tombstoned-names) '("alpha" "bravo" "charlie")))))

;;;; ---- Tests: --ws-names-cache-usable-p --------------------------------

(ert-deftest agent-repl-test-ws-names-cache-usable-p-returns-t-when-non-nil ()
  "--ws-names-cache-usable-p returns non-nil when persp-names-cache is a
non-nil list."
  (let ((persp-names-cache '("ws1")))
    (should (agent-repl--ws-names-cache-usable-p))))

(ert-deftest agent-repl-test-ws-names-cache-usable-p-returns-nil-when-nil ()
  "--ws-names-cache-usable-p returns nil when persp-names-cache is nil
even if bound — a nil cache is not a usable tab-bar signal."
  (let ((persp-names-cache nil))
    (should-not (agent-repl--ws-names-cache-usable-p))))

;;;; ---- Tests: --ws-render-status (daemon-pushed lookup) ----------------
;;
;; Post-cutover (design §10) --ws-render-status is a pure lookup of the
;; daemon-pushed :pushed-render-state key (set by frontend-state.el); it no
;; longer derives from :agent-state / :repl-state / :merging.  These tests
;; pin the lookup, the :init unpushed case, the tombstone/closed-workspace
;; guard, and that legacy derivation keys are ignored.

(ert-deftest agent-repl-test-ws-render-status-errors-for-unknown ()
  "Unknown ws signals user-error via --ws-require-known."
  (agent-repl-test--with-clean-state
    (should-error (agent-repl--ws-render-status "missing") :type 'user-error)))

(ert-deftest agent-repl-test-ws-render-status-returns-pushed-keyword ()
  "The pushed :pushed-render-state keyword is returned verbatim."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/tmp/x")
    (agent-repl--ws-put "ws1" :pushed-render-state :thinking)
    (should (eq :thinking (agent-repl--ws-render-status "ws1")))))

(ert-deftest agent-repl-test-ws-render-status-returns-pushed-merge-conflict ()
  "A pushed :merge-conflict is returned — the reactive conflict UX re-keys to it."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/tmp/x")
    (agent-repl--ws-put "ws1" :pushed-render-state :merge-conflict)
    (should (eq :merge-conflict (agent-repl--ws-render-status "ws1")))))

(ert-deftest agent-repl-test-ws-render-status-init-for-unpushed-known-live ()
  "A known, live ws with no pushed state yet returns :init (never nil).
A just-created workspace legitimately predates its first daemon push."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/tmp/x")
    (should (eq :init (agent-repl--ws-render-status "ws1")))))

(ert-deftest agent-repl-test-ws-render-status-nil-for-tombstoned ()
  "Tombstoned (locally-closed) ws returns nil — the guard dominates."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/tmp/x")
    (agent-repl--ws-del "ws1")
    (should-not (agent-repl--ws-render-status "ws1"))))

(ert-deftest agent-repl-test-ws-render-status-tombstone-beats-pushed-state ()
  "The closed-workspace guard suppresses even a pushed state.
Rendering a tombstone's pushed state would resurrect a closed
workspace's badge."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/tmp/x")
    (agent-repl--ws-put "ws1" :pushed-render-state :thinking)
    (agent-repl--ws-del "ws1")
    (should-not (agent-repl--ws-render-status "ws1"))))

(ert-deftest agent-repl-test-ws-render-status-ignores-legacy-agent-state ()
  "The function no longer derives from :agent-state / :repl-state.
A legacy :agent-state is ignored; the pushed key alone decides."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/tmp/x")
    (agent-repl--ws-put "ws1" :agent-state :thinking)
    (agent-repl--ws-put "ws1" :repl-state :dead)
    (agent-repl--ws-put "ws1" :pushed-render-state :idle)
    (should (eq :idle (agent-repl--ws-render-status "ws1")))))

;;;; ---- Tests: reorder-workspace-by-priority (moved from test-status.el) ----

(ert-deftest agent-repl-test-reorder-priority-noop-when-priority-nil ()
  "reorder-workspace-by-priority leaves cache untouched when ws has no priority."
  (agent-repl-test--with-clean-state
    (let* ((persp-nil-name "main")
           (persp-names-cache '("main" "ws-a" "new-ws"))
           (captured nil))
      (cl-letf (((symbol-function 'persp-update-names-cache)
                 (lambda (new-cache) (setq captured new-cache))))
        (agent-repl--reorder-workspace-by-priority "new-ws")
        (should-not captured)))))

(ert-deftest agent-repl-test-reorder-priority-noop-when-not-in-cache ()
  "reorder-workspace-by-priority no-ops when ws is not registered in cache."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "missing-ws" :priority "p1")
    (let* ((persp-nil-name "main")
           (persp-names-cache '("main" "ws-a"))
           (captured nil))
      (cl-letf (((symbol-function 'persp-update-names-cache)
                 (lambda (new-cache) (setq captured new-cache))))
        (agent-repl--reorder-workspace-by-priority "missing-ws")
        (should-not captured)))))

(ert-deftest agent-repl-test-reorder-priority-logs-bail-no-priority ()
  "reorder-workspace-by-priority emits a BAIL/no-priority log line when ws has no :priority."
  (agent-repl-test--with-clean-state
    (let* ((persp-names-cache '("main" "ws-a" "new-ws"))
           (logs nil))
      (cl-letf (((symbol-function 'agent-repl--log)
                 (lambda (_ws fmt &rest args)
                   (push (apply #'format fmt args) logs))))
        (agent-repl--reorder-workspace-by-priority "new-ws")
        (should (cl-find-if (lambda (l)
                              (and (string-match-p "reorder-workspace-by-priority: BAIL" l)
                                   (string-match-p "reason=no-priority" l)))
                            logs))))))

(ert-deftest agent-repl-test-reorder-priority-logs-bail-not-in-cache ()
  "reorder-workspace-by-priority emits a BAIL/not-in-cache log line when ws is missing from the cache."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "missing-ws" :priority "p1")
    (let* ((persp-names-cache '("main" "ws-a"))
           (logs nil))
      (cl-letf (((symbol-function 'agent-repl--log)
                 (lambda (_ws fmt &rest args)
                   (push (apply #'format fmt args) logs))))
        (agent-repl--reorder-workspace-by-priority "missing-ws")
        (should (cl-find-if (lambda (l)
                              (and (string-match-p "reorder-workspace-by-priority: BAIL" l)
                                   (string-match-p "reason=not-in-cache" l)))
                            logs))))))

(ert-deftest agent-repl-test-reorder-priority-preserves-cache-string-identity ()
  "After reorder, the WS slot in `persp-names-cache' is `eq' to the
canonical string already in the cache, NOT to the (potentially fresh)
WS argument.  Regression for the persp-kill failure where `cl-delete'
with default `:test #'eql' could not match a content-equal but
identity-distinct string injected by `agent-repl-set-priority' from
`completing-read'.  See workspace.el for the full explanation."
  (agent-repl-test--with-clean-state
    (let* ((canonical (copy-sequence "new-p1"))
           (fresh (copy-sequence "new-p1"))
           (persp-nil-name "main")
           (persp-names-cache (list "main" "ws-a" canonical))
           (captured nil))
      (should-not (eq canonical fresh))
      (agent-repl--ws-put fresh :priority "p1")
      (cl-letf (((symbol-function 'persp-update-names-cache)
                 (lambda (new-cache) (setq captured new-cache))))
        (agent-repl--reorder-workspace-by-priority fresh)
        (let ((injected (car (member "new-p1" captured))))
          (should injected)
          (should (eq injected canonical))
          (should-not (eq injected fresh)))))))

(ert-deftest agent-repl-test-reorder-priority-logs-apply-on-success ()
  "reorder-workspace-by-priority emits an APPLY log line on the success path."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "new-p1" :priority "p1")
    (let* ((persp-nil-name "main")
           (persp-names-cache '("main" "ws-a" "new-p1"))
           (logs nil))
      (cl-letf (((symbol-function 'persp-update-names-cache) (lambda (_) nil))
                ((symbol-function 'agent-repl--log)
                 (lambda (_ws fmt &rest args)
                   (push (apply #'format fmt args) logs))))
        (agent-repl--reorder-workspace-by-priority "new-p1")
        (should (cl-find-if (lambda (l)
                              (string-match-p "reorder-workspace-by-priority: APPLY" l))
                            logs))))))

(ert-deftest agent-repl-test-reorder-priority-p1-moves-before-unprioritized ()
  "A new p1 workspace is moved ahead of unprioritized workspaces."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "new-p1" :priority "p1")
    (let* ((persp-nil-name "main")
           (persp-names-cache '("main" "ws-a" "ws-b" "new-p1"))
           (captured nil))
      (cl-letf (((symbol-function 'persp-update-names-cache)
                 (lambda (new-cache) (setq captured new-cache))))
        (agent-repl--reorder-workspace-by-priority "new-p1")
        (should (equal captured '("main" "new-p1" "ws-a" "ws-b")))))))

(ert-deftest agent-repl-test-reorder-priority-p2-after-existing-p1 ()
  "A new p2 workspace lands after an existing p1 but before unprioritized."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws-p1" :priority "p1")
    (agent-repl--ws-put "new-p2" :priority "p2")
    (let* ((persp-nil-name "main")
           (persp-names-cache '("main" "ws-p1" "ws-a" "new-p2"))
           (captured nil))
      (cl-letf (((symbol-function 'persp-update-names-cache)
                 (lambda (new-cache) (setq captured new-cache))))
        (agent-repl--reorder-workspace-by-priority "new-p2")
        (should (equal captured '("main" "ws-p1" "new-p2" "ws-a")))))))

(ert-deftest agent-repl-test-reorder-priority-equal-priority-after-existing ()
  "A new p1 workspace lands after an existing p1 (does not displace peers)."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws-p1-old" :priority "p1")
    (agent-repl--ws-put "new-p1" :priority "p1")
    (let* ((persp-nil-name "main")
           (persp-names-cache '("main" "ws-p1-old" "ws-a" "new-p1"))
           (captured nil))
      (cl-letf (((symbol-function 'persp-update-names-cache)
                 (lambda (new-cache) (setq captured new-cache))))
        (agent-repl--reorder-workspace-by-priority "new-p1")
        (should (equal captured '("main" "ws-p1-old" "new-p1" "ws-a")))))))

(ert-deftest agent-repl-test-reorder-priority-p05-goes-to-very-front ()
  "A new p05 workspace lands ahead of every other priority and unprioritized."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws-p1" :priority "p1")
    (agent-repl--ws-put "ws-p2" :priority "p2")
    (agent-repl--ws-put "new-p05" :priority "p05")
    (let* ((persp-nil-name "main")
           (persp-names-cache '("main" "ws-p1" "ws-p2" "ws-a" "new-p05"))
           (captured nil))
      (cl-letf (((symbol-function 'persp-update-names-cache)
                 (lambda (new-cache) (setq captured new-cache))))
        (agent-repl--reorder-workspace-by-priority "new-p05")
        (should (equal captured '("main" "new-p05" "ws-p1" "ws-p2" "ws-a")))))))

(ert-deftest agent-repl-test-reorder-priority-p3-after-all-priorities ()
  "A new p3 workspace lands after p05/p1/p2 and before unprioritized."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws-p05" :priority "p05")
    (agent-repl--ws-put "ws-p1" :priority "p1")
    (agent-repl--ws-put "ws-p2" :priority "p2")
    (agent-repl--ws-put "new-p3" :priority "p3")
    (let* ((persp-nil-name "main")
           (persp-names-cache '("main" "ws-p05" "ws-p1" "ws-p2" "ws-a" "new-p3"))
           (captured nil))
      (cl-letf (((symbol-function 'persp-update-names-cache)
                 (lambda (new-cache) (setq captured new-cache))))
        (agent-repl--reorder-workspace-by-priority "new-p3")
        (should (equal captured '("main" "ws-p05" "ws-p1" "ws-p2" "new-p3" "ws-a")))))))

(ert-deftest agent-repl-test-reorder-priority-preserves-nil-persp-position ()
  "reorder-workspace-by-priority keeps persp-nil-name at the head of the cache."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "new-p1" :priority "p1")
    (let* ((persp-nil-name "main")
           (persp-names-cache '("main" "ws-a" "new-p1"))
           (captured nil))
      (cl-letf (((symbol-function 'persp-update-names-cache)
                 (lambda (new-cache) (setq captured new-cache))))
        (agent-repl--reorder-workspace-by-priority "new-p1")
        (should (equal (car captured) "main"))))))

;;;; ---- Tests: --reorder-workspace-next-to ----

(ert-deftest agent-repl-test-reorder-next-to-inserts-immediately-after-anchor ()
  "reorder-workspace-next-to splices ws directly after the anchor entry."
  (agent-repl-test--with-clean-state
    (let* ((persp-nil-name "main")
           (persp-names-cache '("main" "parent" "other" "child"))
           (captured nil))
      (cl-letf (((symbol-function 'persp-update-names-cache)
                 (lambda (new-cache) (setq captured new-cache))))
        (agent-repl--reorder-workspace-next-to "child" "parent")
        (should (equal captured '("main" "parent" "child" "other")))))))

(ert-deftest agent-repl-test-reorder-next-to-anchor-is-last-visible ()
  "reorder-workspace-next-to keeps ws right after an anchor that is the last visible tab."
  (agent-repl-test--with-clean-state
    (let* ((persp-nil-name "main")
           (persp-names-cache '("main" "other" "parent" "child"))
           (captured nil))
      (cl-letf (((symbol-function 'persp-update-names-cache)
                 (lambda (new-cache) (setq captured new-cache))))
        (agent-repl--reorder-workspace-next-to "child" "parent")
        (should (equal captured '("main" "other" "parent" "child")))))))

(ert-deftest agent-repl-test-reorder-next-to-preserves-nil-persp-position ()
  "reorder-workspace-next-to keeps persp-nil-name at the head of the cache."
  (agent-repl-test--with-clean-state
    (let* ((persp-nil-name "main")
           (persp-names-cache '("main" "parent" "child"))
           (captured nil))
      (cl-letf (((symbol-function 'persp-update-names-cache)
                 (lambda (new-cache) (setq captured new-cache))))
        (agent-repl--reorder-workspace-next-to "child" "parent")
        (should (equal (car captured) "main"))))))

(ert-deftest agent-repl-test-reorder-next-to-anchor-is-nil-name-lands-at-front ()
  "reorder-workspace-next-to puts ws right after the persp-nil-name sentinel when the anchor is that sentinel."
  (agent-repl-test--with-clean-state
    (let* ((persp-nil-name "main")
           (persp-names-cache '("main" "other" "child"))
           (captured nil))
      (cl-letf (((symbol-function 'persp-update-names-cache)
                 (lambda (new-cache) (setq captured new-cache))))
        (agent-repl--reorder-workspace-next-to "child" "main")
        (should (equal captured '("main" "child" "other")))))))

(ert-deftest agent-repl-test-reorder-next-to-preserves-cache-string-identity ()
  "After next-to reorder, the ws slot is `eq' to the canonical cache string, not the ws argument."
  (agent-repl-test--with-clean-state
    (let* ((canonical (copy-sequence "child"))
           (fresh (copy-sequence "child"))
           (persp-nil-name "main")
           (persp-names-cache (list "main" "parent" canonical))
           (captured nil))
      (should-not (eq canonical fresh))
      (cl-letf (((symbol-function 'persp-update-names-cache)
                 (lambda (new-cache) (setq captured new-cache))))
        (agent-repl--reorder-workspace-next-to fresh "parent")
        (let ((injected (car (member "child" captured))))
          (should injected)
          (should (eq injected canonical))
          (should-not (eq injected fresh)))))))

(ert-deftest agent-repl-test-reorder-next-to-noop-when-ws-not-in-cache ()
  "reorder-workspace-next-to no-ops when ws is not registered in the cache."
  (agent-repl-test--with-clean-state
    (let* ((persp-nil-name "main")
           (persp-names-cache '("main" "parent"))
           (captured nil))
      (cl-letf (((symbol-function 'persp-update-names-cache)
                 (lambda (new-cache) (setq captured new-cache))))
        (agent-repl--reorder-workspace-next-to "child" "parent")
        (should-not captured)))))

(ert-deftest agent-repl-test-reorder-next-to-noop-when-anchor-not-in-cache ()
  "reorder-workspace-next-to no-ops when the anchor is absent from the cache."
  (agent-repl-test--with-clean-state
    (let* ((persp-nil-name "main")
           (persp-names-cache '("main" "child"))
           (captured nil))
      (cl-letf (((symbol-function 'persp-update-names-cache)
                 (lambda (new-cache) (setq captured new-cache))))
        (agent-repl--reorder-workspace-next-to "child" "parent")
        (should-not captured)))))

(ert-deftest agent-repl-test-reorder-next-to-noop-when-anchor-nil ()
  "reorder-workspace-next-to no-ops when the anchor is nil."
  (agent-repl-test--with-clean-state
    (let* ((persp-nil-name "main")
           (persp-names-cache '("main" "child"))
           (captured nil))
      (cl-letf (((symbol-function 'persp-update-names-cache)
                 (lambda (new-cache) (setq captured new-cache))))
        (agent-repl--reorder-workspace-next-to "child" nil)
        (should-not captured)))))

(ert-deftest agent-repl-test-reorder-next-to-noop-when-anchor-is-self ()
  "reorder-workspace-next-to no-ops when the anchor names ws itself."
  (agent-repl-test--with-clean-state
    (let* ((persp-nil-name "main")
           (persp-names-cache '("main" "child"))
           (captured nil))
      (cl-letf (((symbol-function 'persp-update-names-cache)
                 (lambda (new-cache) (setq captured new-cache))))
        (agent-repl--reorder-workspace-next-to "child" "child")
        (should-not captured)))))

;;;; ---- Tests: --ws-resolve-persp ----

(ert-deftest agent-repl-test-ws-resolve-persp-returns-persp-when-found ()
  "ws-resolve-persp returns the persp object when one exists for the name."
  (agent-repl-test--with-clean-state
    (let ((fake-persp (list :a-persp-object)))
      (cl-letf (((symbol-function 'persp-get-by-name)
                 (lambda (_ws) fake-persp)))
        (should (eq (agent-repl--ws-resolve-persp "my-ws") fake-persp))))))

(ert-deftest agent-repl-test-ws-resolve-persp-returns-nil-for-not-persp-sentinel ()
  "ws-resolve-persp returns nil when persp-get-by-name returns the persp-not-persp keyword."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'persp-get-by-name)
               ;; persp-not-persp is :nil — a keyword — which keywordp catches
               (lambda (_ws) :nil)))
      (should-not (agent-repl--ws-resolve-persp "missing-ws")))))

(ert-deftest agent-repl-test-ws-resolve-persp-returns-nil-when-unbound ()
  "ws-resolve-persp returns nil when persp-get-by-name is not fboundp."
  (agent-repl-test--with-clean-state
    (fmakunbound 'persp-get-by-name)
    (should-not (agent-repl--ws-resolve-persp "my-ws"))))

(ert-deftest agent-repl-test-ws-resolve-persp-returns-nil-for-nil-result ()
  "ws-resolve-persp returns nil when persp-get-by-name returns nil."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'persp-get-by-name)
               (lambda (_ws) nil)))
      (should-not (agent-repl--ws-resolve-persp "my-ws")))))

;;;; ---- Tests: --ws-system-available-p ----

(ert-deftest agent-repl-test-ws-system-available-p-returns-t-when-persp-mode-on ()
  "ws-system-available-p returns t when persp-mode is non-nil."
  (agent-repl-test--with-clean-state
    (let ((persp-mode t))
      (should (agent-repl--ws-system-available-p)))))

(ert-deftest agent-repl-test-ws-system-available-p-returns-nil-when-persp-mode-off ()
  "ws-system-available-p returns nil when persp-mode is nil."
  (agent-repl-test--with-clean-state
    (let ((persp-mode nil))
      (should-not (agent-repl--ws-system-available-p)))))

(ert-deftest agent-repl-test-ws-system-available-p-returns-nil-when-persp-mode-unbound ()
  "ws-system-available-p returns nil when persp-mode variable is unbound."
  (agent-repl-test--with-clean-state
    ;; bound-and-true-p returns nil for unbound vars, same as nil.
    ;; We test with persp-mode=nil (the test-helpers default).
    (should-not (agent-repl--ws-system-available-p))))

;;;; ---- Tests: --ws-switch ----

(ert-deftest agent-repl-test-ws-switch-delegates-when-bound ()
  "ws-switch calls +workspace-switch with the given ws name."
  (agent-repl-test--with-clean-state
    (let (called-with)
      (cl-letf (((symbol-function '+workspace-switch)
                 (lambda (ws &rest _args) (setq called-with ws))))
        (agent-repl--ws-switch "my-ws")
        (should (equal called-with "my-ws"))))))

(ert-deftest agent-repl-test-ws-switch-passes-extra-args ()
  "ws-switch forwards additional args to +workspace-switch."
  (agent-repl-test--with-clean-state
    (let (captured-args)
      (cl-letf (((symbol-function '+workspace-switch)
                 (lambda (&rest args) (setq captured-args args))))
        (agent-repl--ws-switch "my-ws" t)
        (should (equal captured-args '("my-ws" t)))))))

(ert-deftest agent-repl-test-ws-switch-noop-when-unbound ()
  "ws-switch is a no-op when +workspace-switch is not fboundp."
  (agent-repl-test--with-clean-state
    (fmakunbound '+workspace-switch)
    (should-not (agent-repl--ws-switch "my-ws"))))

;;;; ---- Tests: --ws-current-name ----

(ert-deftest agent-repl-test-ws-current-name-delegates-to-wrapper ()
  "ws-current-name returns value from +workspace-current-name when bound."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "my-ws")))
      (should (equal (agent-repl--ws-current-name) "my-ws")))))

(ert-deftest agent-repl-test-ws-current-name-returns-nil-when-unbound ()
  "ws-current-name returns nil when +workspace-current-name is not fboundp."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) nil))
      ;; Unbind by fmakunbound so fboundp returns nil.
      (fmakunbound '+workspace-current-name)
      (should-not (agent-repl--ws-current-name)))))

(ert-deftest agent-repl-test-ws-current-name-returns-nil-when-no-persp ()
  "ws-current-name returns nil when the workspace system returns nil."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () nil)))
      (should-not (agent-repl--ws-current-name)))))

;;;; ---- Tests: --ws-exists-p ----

(ert-deftest agent-repl-test-ws-exists-p-delegates-when-bound ()
  "ws-exists-p returns the +workspace-exists-p result when bound."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-exists-p) (lambda (ws) (equal ws "live"))))
      (should (agent-repl--ws-exists-p "live"))
      (should-not (agent-repl--ws-exists-p "gone")))))

(ert-deftest agent-repl-test-ws-exists-p-returns-nil-when-unbound ()
  "ws-exists-p returns nil when +workspace-exists-p is not fboundp."
  (agent-repl-test--with-clean-state
    (fmakunbound '+workspace-exists-p)
    (should-not (agent-repl--ws-exists-p "any"))))

;;;; ---- Tests: --ws-kill ----

(ert-deftest agent-repl-test-ws-kill-delegates-when-bound ()
  "ws-kill calls +workspace/kill with the given ws name."
  (agent-repl-test--with-clean-state
    (let (killed)
      (cl-letf (((symbol-function '+workspace/kill) (lambda (ws) (setq killed ws))))
        (agent-repl--ws-kill "doomed")
        (should (equal killed "doomed"))))))

(ert-deftest agent-repl-test-ws-kill-noop-when-unbound ()
  "ws-kill is a no-op when +workspace/kill is not fboundp."
  (agent-repl-test--with-clean-state
    (fmakunbound '+workspace/kill)
    (should-not (agent-repl--ws-kill "doomed"))))

;;;; ---- Tests: --ws-main-name ----

(ert-deftest agent-repl-test-ws-main-name-returns-value ()
  "ws-main-name returns the +workspaces-main value when bound and non-nil."
  (agent-repl-test--with-clean-state
    (let ((+workspaces-main "custom-main"))
      (should (equal (agent-repl--ws-main-name) "custom-main")))))

(ert-deftest agent-repl-test-ws-main-name-returns-nil-when-nil ()
  "ws-main-name returns nil when +workspaces-main is nil."
  (agent-repl-test--with-clean-state
    (let ((+workspaces-main nil))
      (should-not (agent-repl--ws-main-name)))))

;;;; ---- Tests: --ws-frame-switch ----

(ert-deftest agent-repl-test-ws-frame-switch-delegates-when-bound ()
  "ws-frame-switch calls persp-frame-switch with the given ws name."
  (agent-repl-test--with-clean-state
    (let (switched)
      (cl-letf (((symbol-function 'persp-frame-switch) (lambda (ws) (setq switched ws))))
        (agent-repl--ws-frame-switch "target")
        (should (equal switched "target"))))))

(ert-deftest agent-repl-test-ws-frame-switch-noop-when-unbound ()
  "ws-frame-switch is a no-op when persp-frame-switch is not fboundp."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'persp-frame-switch) nil))
      (fmakunbound 'persp-frame-switch)
      (should-not (agent-repl--ws-frame-switch "target")))))

;;;; ---- Tests: --ws-frame-save-state ----

(ert-deftest agent-repl-test-ws-frame-save-state-delegates-when-bound ()
  "ws-frame-save-state calls persp-frame-save-state when bound."
  (agent-repl-test--with-clean-state
    (let (saved)
      (cl-letf (((symbol-function 'persp-frame-save-state) (lambda () (setq saved t))))
        (agent-repl--ws-frame-save-state)
        (should saved)))))

(ert-deftest agent-repl-test-ws-frame-save-state-noop-when-unbound ()
  "ws-frame-save-state is a no-op when persp-frame-save-state is not fboundp."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'persp-frame-save-state) nil))
      (fmakunbound 'persp-frame-save-state)
      (should-not (agent-repl--ws-frame-save-state)))))

;;;; ---- Tests: --ws-create ----

(ert-deftest agent-repl-test-ws-create-returns-persp-and-tags-project ()
  "ws-create calls persp-add-new and sets +workspace-project on a real persp."
  (agent-repl-test--with-clean-state
    (let (added param-call)
      (cl-letf (((symbol-function 'persp-add-new) (lambda (ws) (setq added ws) 'a-persp))
                ((symbol-function 'set-persp-parameter)
                 (lambda (key val persp) (setq param-call (list key val persp)))))
        (should (eq (agent-repl--ws-create "ws1" "/tmp/p") 'a-persp))
        (should (equal added "ws1"))
        (should (equal param-call '(+workspace-project "/tmp/p" a-persp)))))))

(ert-deftest agent-repl-test-ws-create-skips-param-when-keyword-sentinel ()
  "ws-create does not set the project param when persp-add-new returns a keyword."
  (agent-repl-test--with-clean-state
    (let (param-called)
      (cl-letf (((symbol-function 'persp-add-new) (lambda (_ws) :nil))
                ((symbol-function 'set-persp-parameter)
                 (lambda (&rest _) (setq param-called t))))
        (agent-repl--ws-create "ws1" "/tmp/p")
        (should-not param-called)))))

(ert-deftest agent-repl-test-ws-create-skips-param-when-no-dir ()
  "ws-create does not set the project param when PROJECT-DIR is nil."
  (agent-repl-test--with-clean-state
    (let (param-called)
      (cl-letf (((symbol-function 'persp-add-new) (lambda (_ws) 'a-persp))
                ((symbol-function 'set-persp-parameter)
                 (lambda (&rest _) (setq param-called t))))
        (agent-repl--ws-create "ws1")
        (should-not param-called)))))

(ert-deftest agent-repl-test-ws-create-noop-when-unbound ()
  "ws-create returns nil when persp-add-new is not fboundp."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'persp-add-new) nil))
      (fmakunbound 'persp-add-new)
      (should-not (agent-repl--ws-create "ws1" "/tmp/p")))))

(ert-deftest agent-repl-test-ws-create-seeds-project-dir ()
  "ws-create seeds :project-dir into the hash for a real persp + non-nil dir."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'persp-add-new) (lambda (_ws) 'a-persp))
              ((symbol-function 'set-persp-parameter) (lambda (&rest _) nil)))
      (agent-repl--ws-create "ws1" "/tmp/p")
      (should (equal (agent-repl--ws-get "ws1" :project-dir) "/tmp/p")))))

(ert-deftest agent-repl-test-ws-create-no-seed-when-no-dir ()
  "ws-create does not seed :project-dir when PROJECT-DIR is nil."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'persp-add-new) (lambda (_ws) 'a-persp))
              ((symbol-function 'set-persp-parameter) (lambda (&rest _) nil)))
      (agent-repl--ws-create "ws1")
      (should-not (agent-repl--ws-get "ws1" :project-dir)))))

(ert-deftest agent-repl-test-ws-create-no-seed-when-keyword-sentinel ()
  "ws-create does not seed :project-dir when persp-add-new returns a keyword."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'persp-add-new) (lambda (_ws) :nil))
              ((symbol-function 'set-persp-parameter) (lambda (&rest _) nil)))
      (agent-repl--ws-create "ws1" "/tmp/p")
      (should-not (agent-repl--ws-get "ws1" :project-dir)))))

;;;; ---- Tests: --ws-protected-p ----

(ert-deftest agent-repl-test-ws-protected-p-delegates-when-bound ()
  "ws-protected-p returns the +workspace--protected-p result when bound."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace--protected-p) (lambda (ws) (equal ws "main"))))
      (should (agent-repl--ws-protected-p "main"))
      (should-not (agent-repl--ws-protected-p "feature")))))

(ert-deftest agent-repl-test-ws-protected-p-returns-nil-when-unbound ()
  "ws-protected-p returns nil when +workspace--protected-p is not fboundp."
  (agent-repl-test--with-clean-state
    (fmakunbound '+workspace--protected-p)
    (should-not (agent-repl--ws-protected-p "main"))))

;;;; ---- Tests: --ws-error ----

(ert-deftest agent-repl-test-ws-error-delegates-when-bound ()
  "ws-error forwards message and noerror flag to +workspace-error."
  (agent-repl-test--with-clean-state
    (let (captured)
      (cl-letf (((symbol-function '+workspace-error)
                 (lambda (msg &optional noerror) (setq captured (list msg noerror)))))
        (agent-repl--ws-error "boom" t)
        (should (equal captured '("boom" t)))))))

(ert-deftest agent-repl-test-ws-error-noop-when-unbound ()
  "ws-error is a no-op when +workspace-error is not fboundp."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-error) nil))
      (fmakunbound '+workspace-error)
      (should-not (agent-repl--ws-error "boom" t)))))

;;;; ---- Tests: --ws-add-buffer ----

(ert-deftest agent-repl-test-ws-add-buffer-delegates-when-bound ()
  "ws-add-buffer forwards buffer, persp, and switch to persp-add-buffer."
  (agent-repl-test--with-clean-state
    (let (captured)
      (cl-letf (((symbol-function 'persp-add-buffer)
                 (lambda (buf persp switch) (setq captured (list buf persp switch)))))
        (agent-repl--ws-add-buffer 'buf 'persp t)
        (should (equal captured '(buf persp t)))))))

(ert-deftest agent-repl-test-ws-add-buffer-noop-when-unbound ()
  "ws-add-buffer is a no-op when persp-add-buffer is not fboundp."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'persp-add-buffer) nil))
      (fmakunbound 'persp-add-buffer)
      (should-not (agent-repl--ws-add-buffer 'buf 'persp nil)))))

;;;; ---- Tests: --ws-buffers ----

(ert-deftest agent-repl-test-ws-buffers-delegates-when-bound ()
  "ws-buffers returns the persp-buffers result for a non-nil persp."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'persp-buffers) (lambda (_persp) '(b1 b2))))
      (should (equal (agent-repl--ws-buffers 'persp) '(b1 b2))))))

(ert-deftest agent-repl-test-ws-buffers-returns-nil-for-nil-persp ()
  "ws-buffers returns nil when persp is nil, without calling persp-buffers."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'persp-buffers)
               (lambda (_persp) (error "should not be called"))))
      (should-not (agent-repl--ws-buffers nil)))))

(ert-deftest agent-repl-test-ws-buffers-returns-nil-when-unbound ()
  "ws-buffers returns nil when persp-buffers is not fboundp."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'persp-buffers) nil))
      (fmakunbound 'persp-buffers)
      (should-not (agent-repl--ws-buffers 'persp)))))

;;;; ---- Tests: --ws-rename-persp ----

(ert-deftest agent-repl-test-ws-rename-persp-renames-live-persp ()
  "ws-rename-persp renames the resolved persp and returns non-nil on success."
  (agent-repl-test--with-clean-state
    (let (captured)
      (cl-letf (((symbol-function 'agent-repl--ws-resolve-persp) (lambda (_ws) 'a-persp))
                ((symbol-function 'persp-rename)
                 (lambda (new persp) (setq captured (list new persp)) t)))
        (should (agent-repl--ws-rename-persp "old" "new"))
        (should (equal captured '("new" a-persp)))))))

(ert-deftest agent-repl-test-ws-rename-persp-returns-nil-on-failure ()
  "ws-rename-persp returns nil when a live persp exists but persp-rename fails."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--ws-resolve-persp) (lambda (_ws) 'a-persp))
              ((symbol-function 'persp-rename) (lambda (_new _persp) nil)))
      (should-not (agent-repl--ws-rename-persp "old" "new")))))

(ert-deftest agent-repl-test-ws-rename-persp-noop-when-no-persp ()
  "ws-rename-persp returns non-nil and skips rename when OLD-WS has no live persp."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--ws-resolve-persp) (lambda (_ws) nil))
              ((symbol-function 'persp-rename)
               (lambda (&rest _) (error "should not be called"))))
      (should (agent-repl--ws-rename-persp "old" "new")))))

(ert-deftest agent-repl-test-ws-rename-persp-noop-when-unbound ()
  "ws-rename-persp returns non-nil when persp-rename is not fboundp."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'persp-rename) nil))
      (fmakunbound 'persp-rename)
      (should (agent-repl--ws-rename-persp "old" "new")))))

;;;; ---- Tests: --ws-frame-ordered-names ----

(ert-deftest agent-repl-test-ws-frame-ordered-names-delegates-when-bound ()
  "ws-frame-ordered-names returns the persp fast-ordered list when bound."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'persp-names-current-frame-fast-ordered)
               (lambda () '("a" "b" "c"))))
      (should (equal (agent-repl--ws-frame-ordered-names) '("a" "b" "c"))))))

(ert-deftest agent-repl-test-ws-frame-ordered-names-returns-nil-when-unbound ()
  "ws-frame-ordered-names returns nil when the persp helper is not fboundp."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'persp-names-current-frame-fast-ordered) nil))
      (fmakunbound 'persp-names-current-frame-fast-ordered)
      (should-not (agent-repl--ws-frame-ordered-names)))))

;;;; ---- Tests: --ws-update-names-cache ----

(ert-deftest agent-repl-test-ws-update-names-cache-delegates-when-bound ()
  "ws-update-names-cache forwards NAMES to persp-update-names-cache."
  (agent-repl-test--with-clean-state
    (let (captured)
      (cl-letf (((symbol-function 'persp-update-names-cache)
                 (lambda (names) (setq captured names))))
        (agent-repl--ws-update-names-cache '("a" "b"))
        (should (equal captured '("a" "b")))))))

(ert-deftest agent-repl-test-ws-update-names-cache-noop-when-unbound ()
  "ws-update-names-cache is a no-op when persp-update-names-cache is not fboundp."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'persp-update-names-cache) nil))
      (fmakunbound 'persp-update-names-cache)
      (should-not (agent-repl--ws-update-names-cache '("a"))))))

;;;; ---- Tests: --ws-window-conf ----

(ert-deftest agent-repl-test-ws-window-conf-delegates-when-bound ()
  "ws-window-conf returns the persp-window-conf result for a non-nil persp."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'persp-window-conf) (lambda (_persp) 'a-wconf)))
      (should (eq (agent-repl--ws-window-conf 'persp) 'a-wconf)))))

(ert-deftest agent-repl-test-ws-window-conf-returns-nil-for-nil-persp ()
  "ws-window-conf returns nil for a nil persp without calling persp-window-conf."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'persp-window-conf)
               (lambda (_persp) (error "should not be called"))))
      (should-not (agent-repl--ws-window-conf nil)))))

(ert-deftest agent-repl-test-ws-window-conf-returns-nil-when-unbound ()
  "ws-window-conf returns nil when persp-window-conf is not fboundp."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'persp-window-conf) nil))
      (fmakunbound 'persp-window-conf)
      (should-not (agent-repl--ws-window-conf 'persp)))))

;;;; ---- Tests: --ws-tab-face / --ws-tab-selected-face ----

(ert-deftest agent-repl-test-ws-tab-face-returns-doom-face-symbol ()
  "ws-tab-face returns the +workspace-tab-face symbol."
  (should (eq (agent-repl--ws-tab-face) '+workspace-tab-face)))

(ert-deftest agent-repl-test-ws-tab-selected-face-returns-doom-face-symbol ()
  "ws-tab-selected-face returns the +workspace-tab-selected-face symbol."
  (should (eq (agent-repl--ws-tab-selected-face) '+workspace-tab-selected-face)))

;;;; ---- Tests: --ws-register-project ----

(ert-deftest agent-repl-test-ws-register-project-delegates-when-bound ()
  "ws-register-project forwards DIR to projectile-add-known-project."
  (agent-repl-test--with-clean-state
    (let (captured)
      (cl-letf (((symbol-function 'projectile-add-known-project)
                 (lambda (dir) (setq captured dir))))
        (agent-repl--ws-register-project "/tmp/p/")
        (should (equal captured "/tmp/p/"))))))

(ert-deftest agent-repl-test-ws-register-project-noop-when-unbound ()
  "ws-register-project is a no-op when projectile-add-known-project is unbound."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'projectile-add-known-project) nil))
      (fmakunbound 'projectile-add-known-project)
      (should-not (agent-repl--ws-register-project "/tmp/p/")))))

;;;; ---- Tests: --ws-unregister-project ----

(ert-deftest agent-repl-test-ws-unregister-project-delegates-when-bound ()
  "ws-unregister-project forwards DIR to projectile-remove-known-project."
  (agent-repl-test--with-clean-state
    (let (captured)
      (cl-letf (((symbol-function 'projectile-remove-known-project)
                 (lambda (dir) (setq captured dir))))
        (agent-repl--ws-unregister-project "/tmp/p/")
        (should (equal captured "/tmp/p/"))))))

(ert-deftest agent-repl-test-ws-unregister-project-noop-when-unbound ()
  "ws-unregister-project is a no-op when projectile-remove-known-project is unbound."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'projectile-remove-known-project) nil))
      (fmakunbound 'projectile-remove-known-project)
      (should-not (agent-repl--ws-unregister-project "/tmp/p/")))))

;;;; ---- Tests: --ws-switch-project ----

(ert-deftest agent-repl-test-ws-switch-project-delegates-when-bound ()
  "ws-switch-project forwards PROJECT to projectile-switch-project-by-name."
  (agent-repl-test--with-clean-state
    (let (captured)
      (cl-letf (((symbol-function 'projectile-switch-project-by-name)
                 (lambda (project) (setq captured project))))
        (agent-repl--ws-switch-project "/tmp/p/")
        (should (equal captured "/tmp/p/"))))))

(ert-deftest agent-repl-test-ws-switch-project-noop-when-unbound ()
  "ws-switch-project is a no-op when projectile-switch-project-by-name is unbound."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'projectile-switch-project-by-name) nil))
      (fmakunbound 'projectile-switch-project-by-name)
      (should-not (agent-repl--ws-switch-project "/tmp/p/")))))

;;;; ---- Tests: --ws-known-projects ----

(ert-deftest agent-repl-test-ws-known-projects-delegates-when-bound ()
  "ws-known-projects returns the projectile-relevant-known-projects list."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'projectile-relevant-known-projects)
               (lambda () '("/a/" "/b/"))))
      (should (equal (agent-repl--ws-known-projects) '("/a/" "/b/"))))))

(ert-deftest agent-repl-test-ws-known-projects-returns-nil-when-unbound ()
  "ws-known-projects returns nil when projectile-relevant-known-projects is unbound."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'projectile-relevant-known-projects) nil))
      (fmakunbound 'projectile-relevant-known-projects)
      (should-not (agent-repl--ws-known-projects)))))

;;;; ---- Tests: --ws-all-persps ----

(ert-deftest agent-repl-test-ws-all-persps-delegates-when-bound ()
  "ws-all-persps returns the raw persp-persps list when bound."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'persp-persps) (lambda () '(p1 p2 nil))))
      (should (equal (agent-repl--ws-all-persps) '(p1 p2 nil))))))

(ert-deftest agent-repl-test-ws-all-persps-returns-nil-when-unbound ()
  "ws-all-persps returns nil when persp-persps is not fboundp."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'persp-persps) nil))
      (fmakunbound 'persp-persps)
      (should-not (agent-repl--ws-all-persps)))))

;;;; ---- Tests: --ws-persp-name ----

(ert-deftest agent-repl-test-ws-persp-name-delegates-when-bound ()
  "ws-persp-name returns the safe-persp-name result when bound."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'safe-persp-name) (lambda (persp) (format "%s" persp))))
      (should (equal (agent-repl--ws-persp-name 'a-persp) "a-persp")))))

(ert-deftest agent-repl-test-ws-persp-name-returns-nil-when-unbound ()
  "ws-persp-name returns nil when safe-persp-name is not fboundp."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'safe-persp-name) nil))
      (fmakunbound 'safe-persp-name)
      (should-not (agent-repl--ws-persp-name 'a-persp)))))

;;;; ---- Tests: --ws-run-switch-project-function ----

(ert-deftest agent-repl-test-ws-run-switch-project-function-invokes-when-set ()
  "ws-run-switch-project-function funcalls the configured function with DIR."
  (agent-repl-test--with-clean-state
    (let ((+workspaces-switch-project-function nil)
          (called-with nil))
      (setq +workspaces-switch-project-function
            (lambda (dir) (setq called-with dir)))
      (agent-repl--ws-run-switch-project-function "/tmp/proj")
      (should (equal called-with "/tmp/proj")))))

(ert-deftest agent-repl-test-ws-run-switch-project-function-noop-when-nil ()
  "ws-run-switch-project-function is a no-op when the function var is nil."
  (agent-repl-test--with-clean-state
    (let ((+workspaces-switch-project-function nil))
      (should-not (agent-repl--ws-run-switch-project-function "/tmp/proj")))))

;;;; ---- Tests: --record-workspace-history ----

(ert-deftest agent-repl-test-record-workspace-history-pushes-current ()
  "record-workspace-history pushes the current workspace to the front."
  (agent-repl-test--with-clean-state
    (let ((agent-repl--workspace-history nil))
      (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "a")))
        (agent-repl--record-workspace-history)
        (should (equal agent-repl--workspace-history '("a")))))))

(ert-deftest agent-repl-test-record-workspace-history-dedups-and-fronts ()
  "record-workspace-history moves an already-present name to the front."
  (agent-repl-test--with-clean-state
    (let ((agent-repl--workspace-history '("b" "a")))
      (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "a")))
        (agent-repl--record-workspace-history)
        (should (equal agent-repl--workspace-history '("a" "b")))))))

(ert-deftest agent-repl-test-record-workspace-history-noop-when-no-current ()
  "record-workspace-history leaves history unchanged when there is no current ws."
  (agent-repl-test--with-clean-state
    (let ((agent-repl--workspace-history '("a")))
      (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () nil)))
        (agent-repl--record-workspace-history)
        (should (equal agent-repl--workspace-history '("a")))))))

(ert-deftest agent-repl-test-record-workspace-history-stamps-last-viewed-at ()
  "record-workspace-history stamps :last-viewed-at on the activated known workspace."
  (agent-repl-test--with-clean-state
    (let ((agent-repl--workspace-history nil))
      (agent-repl--ws-put "a" :project-dir "/tmp/a")
      (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "a"))
                ((symbol-function 'current-time) (lambda () '(25000 0))))
        (agent-repl--record-workspace-history)
        (should (equal (agent-repl--ws-get "a" :last-viewed-at) '(25000 0)))))))

(ert-deftest agent-repl-test-record-workspace-history-skips-unknown-stamp ()
  "record-workspace-history does not stub-create an entry for a foreign persp."
  (agent-repl-test--with-clean-state
    (let ((agent-repl--workspace-history nil))
      (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "main")))
        (agent-repl--record-workspace-history)
        ;; History still records the name, but no hash entry was created.
        (should (equal agent-repl--workspace-history '("main")))
        (should-not (agent-repl--ws-known-p "main"))))))

(ert-deftest agent-repl-test-record-workspace-history-suppressed-during-eager-open ()
  "record-workspace-history does not record the transient visit while
`agent-repl--eager-open-in-progress' is set — the eager-open switch to a
just-generated background workspace is not a real visit, so `SPC b p'
must not treat it as the caller's previous workspace."
  (agent-repl-test--with-clean-state
    (let ((agent-repl--workspace-history '("caller"))
          (agent-repl--eager-open-in-progress t))
      (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "generated")))
        (agent-repl--record-workspace-history)
        (should (equal agent-repl--workspace-history '("caller")))))))

(ert-deftest agent-repl-test-record-workspace-history-eager-open-skips-last-viewed-stamp ()
  "record-workspace-history does not stamp :last-viewed-at on the
transiently activated workspace while eager-open is in progress."
  (agent-repl-test--with-clean-state
    (let ((agent-repl--workspace-history nil)
          (agent-repl--eager-open-in-progress t))
      (agent-repl--ws-put "generated" :project-dir "/tmp/g")
      (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "generated"))
                ((symbol-function 'current-time) (lambda () '(25000 0))))
        (agent-repl--record-workspace-history)
        (should-not (agent-repl--ws-get "generated" :last-viewed-at))))))

;;;; ---- Tests: --ws-new ----

(ert-deftest agent-repl-test-ws-new-with-name-delegates-to-workspace-new ()
  "ws-new with a NAME calls +workspace-new with that name."
  (agent-repl-test--with-clean-state
    (let (created)
      (cl-letf (((symbol-function '+workspace-new) (lambda (name) (setq created name))))
        (agent-repl--ws-new "ws1")
        (should (equal created "ws1"))))))

(ert-deftest agent-repl-test-ws-new-without-name-delegates-to-workspace/new ()
  "ws-new without a NAME calls the interactive +workspace/new."
  (agent-repl-test--with-clean-state
    (let (called)
      (cl-letf (((symbol-function '+workspace/new) (lambda (&rest _) (setq called t))))
        (agent-repl--ws-new)
        (should called)))))

(ert-deftest agent-repl-test-ws-new-noop-when-unbound ()
  "ws-new with a NAME is a no-op when +workspace-new is not fboundp."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-new) nil))
      (fmakunbound '+workspace-new)
      (should-not (agent-repl--ws-new "ws1")))))

;;;; ---- Tests: --ws-persp-kill ----

(ert-deftest agent-repl-test-ws-persp-kill-delegates-when-bound ()
  "ws-persp-kill calls persp-kill with the given ws name."
  (agent-repl-test--with-clean-state
    (let (killed)
      (cl-letf (((symbol-function 'persp-kill) (lambda (ws) (setq killed ws))))
        (agent-repl--ws-persp-kill "doomed")
        (should (equal killed "doomed"))))))

(ert-deftest agent-repl-test-ws-persp-kill-noop-when-unbound ()
  "ws-persp-kill is a no-op when persp-kill is not fboundp."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'persp-kill) nil))
      (fmakunbound 'persp-kill)
      (should-not (agent-repl--ws-persp-kill "doomed")))))

;;;; ---- Tests: --ws-remove-buffer ----

(ert-deftest agent-repl-test-ws-remove-buffer-delegates-when-bound ()
  "ws-remove-buffer calls persp-remove-buffer with the given buffer."
  (agent-repl-test--with-clean-state
    (let (removed)
      (cl-letf (((symbol-function 'persp-remove-buffer) (lambda (buf) (setq removed buf))))
        (agent-repl--ws-remove-buffer 'buf)
        (should (eq removed 'buf))))))

(ert-deftest agent-repl-test-ws-remove-buffer-noop-when-unbound ()
  "ws-remove-buffer is a no-op when persp-remove-buffer is not fboundp."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'persp-remove-buffer) nil))
      (fmakunbound 'persp-remove-buffer)
      (should-not (agent-repl--ws-remove-buffer 'buf)))))

(ert-deftest agent-repl-test-ws-remove-buffer-suppresses-autokill ()
  "ws-remove-buffer nils persp-autokill-buffer-on-remove for the removal.
Doom's `kill-weak' would otherwise let persp-mode kill the detached
buffer, taking the frontend webview (persp-free, xwidget-bearing) with it."
  (agent-repl-test--with-clean-state
    (let ((persp-autokill-buffer-on-remove 'kill-weak)
          observed)
      (cl-letf (((symbol-function 'persp-remove-buffer)
                 (lambda (_buf) (setq observed persp-autokill-buffer-on-remove))))
        (agent-repl--ws-remove-buffer 'buf)
        (should-not observed)))))

(ert-deftest agent-repl-test-ws-remove-buffer-restores-autokill ()
  "ws-remove-buffer leaves persp-autokill-buffer-on-remove untouched afterward."
  (agent-repl-test--with-clean-state
    (let ((persp-autokill-buffer-on-remove 'kill-weak))
      (cl-letf (((symbol-function 'persp-remove-buffer) #'ignore))
        (agent-repl--ws-remove-buffer 'buf)
        (should (eq persp-autokill-buffer-on-remove 'kill-weak))))))

;;;; ---- Tests: --ws-nil-name ----

(ert-deftest agent-repl-test-ws-nil-name-returns-value-when-bound ()
  "ws-nil-name returns the persp-nil-name value when bound and non-nil."
  (agent-repl-test--with-clean-state
    (let ((persp-nil-name "none"))
      (should (equal (agent-repl--ws-nil-name) "none")))))

(ert-deftest agent-repl-test-ws-nil-name-returns-nil-when-nil ()
  "ws-nil-name returns nil when persp-nil-name is nil."
  (agent-repl-test--with-clean-state
    (let ((persp-nil-name nil))
      (should-not (agent-repl--ws-nil-name)))))

;;;; ---- Tests: --ws-names-cache ----

(ert-deftest agent-repl-test-ws-names-cache-returns-cache-when-bound ()
  "ws-names-cache returns the persp-names-cache list when bound and non-empty."
  (agent-repl-test--with-clean-state
    (let ((persp-names-cache '("main" "ws-a")))
      (should (equal (agent-repl--ws-names-cache) '("main" "ws-a"))))))

(ert-deftest agent-repl-test-ws-names-cache-returns-nil-when-empty ()
  "ws-names-cache returns nil when persp-names-cache is empty."
  (agent-repl-test--with-clean-state
    (let ((persp-names-cache nil))
      (should-not (agent-repl--ws-names-cache)))))

;;;; ---- Tests: --workspace-for-buffer (moved from test-status.el) ----

(ert-deftest agent-repl-test-workspace-for-buffer-persp-mode-nil ()
  "workspace-for-buffer should return nil when persp-mode is nil."
  (agent-repl-test--with-clean-state
    (let ((persp-mode nil))
      (should-not (agent-repl--workspace-for-buffer (current-buffer))))))

(ert-deftest agent-repl-test-workspace-for-buffer-found ()
  "workspace-for-buffer should return workspace name when buffer is found."
  (agent-repl-test--with-clean-state
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
        (should (equal (agent-repl--workspace-for-buffer test-buf)
                       "my-workspace"))))))

(ert-deftest agent-repl-test-workspace-for-buffer-not-found ()
  "workspace-for-buffer should return nil when buffer not in any persp."
  (agent-repl-test--with-clean-state
    (let ((persp-mode t))
      (cl-letf (((symbol-function 'persp-persps)
                 (lambda () '("ws-a" "ws-b")))
                ((symbol-function 'persp-contain-buffer-p)
                 (lambda (_buf _persp) nil)))
        (should-not (agent-repl--workspace-for-buffer (current-buffer)))))))

;;;; ---- Tests: reorder-workspace-to-front (moved from test-status.el) ----

(ert-deftest agent-repl-test-reorder-to-front-moves-to-leftmost-visible ()
  "reorder-workspace-to-front moves WS to the first visible slot,
immediately after `persp-nil-name'."
  (agent-repl-test--with-clean-state
    (let* ((persp-nil-name "main")
           (persp-names-cache '("main" "ws-a" "ws-b" "merge-failed-ws"))
           (captured nil))
      (cl-letf (((symbol-function 'persp-update-names-cache)
                 (lambda (new-cache) (setq captured new-cache))))
        (agent-repl--reorder-workspace-to-front "merge-failed-ws")
        (should (equal captured '("main" "merge-failed-ws" "ws-a" "ws-b")))))))

(ert-deftest agent-repl-test-reorder-to-front-without-nil-name ()
  "When persp-nil-name is unset, the front-reorder places WS at index 0
of the cache (no sentinel head)."
  (agent-repl-test--with-clean-state
    (let* ((persp-nil-name nil)
           (persp-names-cache '("ws-a" "ws-b" "merge-failed-ws"))
           (captured nil))
      (cl-letf (((symbol-function 'persp-update-names-cache)
                 (lambda (new-cache) (setq captured new-cache))))
        (agent-repl--reorder-workspace-to-front "merge-failed-ws")
        (should (equal captured '("merge-failed-ws" "ws-a" "ws-b")))))))

(ert-deftest agent-repl-test-reorder-to-front-noop-when-not-in-cache ()
  "reorder-workspace-to-front no-ops when WS is not in `persp-names-cache'."
  (agent-repl-test--with-clean-state
    (let* ((persp-nil-name "main")
           (persp-names-cache '("main" "ws-a"))
           (captured nil))
      (cl-letf (((symbol-function 'persp-update-names-cache)
                 (lambda (new-cache) (setq captured new-cache))))
        (agent-repl--reorder-workspace-to-front "missing-ws")
        (should-not captured)))))

(ert-deftest agent-repl-test-reorder-to-front-logs-bail-not-in-cache ()
  "reorder-workspace-to-front emits a BAIL/not-in-cache log line when WS is missing."
  (agent-repl-test--with-clean-state
    (let* ((persp-names-cache '("main" "ws-a"))
           (logs nil))
      (cl-letf (((symbol-function 'agent-repl--log)
                 (lambda (_ws fmt &rest args)
                   (push (apply #'format fmt args) logs))))
        (agent-repl--reorder-workspace-to-front "missing-ws")
        (should (cl-find-if (lambda (l)
                              (and (string-match-p "reorder-workspace-to-front: BAIL" l)
                                   (string-match-p "reason=not-in-cache" l)))
                            logs))))))

(ert-deftest agent-repl-test-reorder-to-front-logs-apply-on-success ()
  "reorder-workspace-to-front emits an APPLY log line on the success path."
  (agent-repl-test--with-clean-state
    (let* ((persp-nil-name "main")
           (persp-names-cache '("main" "ws-a" "merge-failed-ws"))
           (logs nil))
      (cl-letf (((symbol-function 'persp-update-names-cache) (lambda (_) nil))
                ((symbol-function 'agent-repl--log)
                 (lambda (_ws fmt &rest args)
                   (push (apply #'format fmt args) logs))))
        (agent-repl--reorder-workspace-to-front "merge-failed-ws")
        (should (cl-find-if (lambda (l)
                              (string-match-p "reorder-workspace-to-front: APPLY" l))
                            logs))))))

(ert-deftest agent-repl-test-reorder-to-front-preserves-nil-persp-position ()
  "reorder-workspace-to-front keeps persp-nil-name at the head of the cache."
  (agent-repl-test--with-clean-state
    (let* ((persp-nil-name "main")
           (persp-names-cache '("main" "ws-a" "merge-failed-ws"))
           (captured nil))
      (cl-letf (((symbol-function 'persp-update-names-cache)
                 (lambda (new-cache) (setq captured new-cache))))
        (agent-repl--reorder-workspace-to-front "merge-failed-ws")
        (should (equal (car captured) "main"))))))

(ert-deftest agent-repl-test-reorder-to-front-preserves-cache-string-identity ()
  "After reorder, the WS slot in `persp-names-cache' is `eq' to the
canonical string already in the cache, NOT to the (potentially fresh)
WS argument.  Same guarantee as `reorder-workspace-by-priority' — see
workspace.el for the persp-mode identity rationale."
  (agent-repl-test--with-clean-state
    (let* ((canonical (copy-sequence "merge-failed-ws"))
           (fresh (copy-sequence "merge-failed-ws"))
           (persp-nil-name "main")
           (persp-names-cache (list "main" "ws-a" canonical))
           (captured nil))
      (should-not (eq canonical fresh))
      (cl-letf (((symbol-function 'persp-update-names-cache)
                 (lambda (new-cache) (setq captured new-cache))))
        (agent-repl--reorder-workspace-to-front fresh)
        (let ((injected (car (member "merge-failed-ws" captured))))
          (should injected)
          (should (eq injected canonical))
          (should-not (eq injected fresh)))))))

(ert-deftest agent-repl-test-reorder-to-front-idempotent-when-already-front ()
  "Reordering a WS that is already at the visible front leaves the cache
in the same shape (still leftmost, nil-name still at head)."
  (agent-repl-test--with-clean-state
    (let* ((persp-nil-name "main")
           (persp-names-cache '("main" "merge-failed-ws" "ws-a" "ws-b"))
           (captured nil))
      (cl-letf (((symbol-function 'persp-update-names-cache)
                 (lambda (new-cache) (setq captured new-cache))))
        (agent-repl--reorder-workspace-to-front "merge-failed-ws")
        (should (equal captured '("main" "merge-failed-ws" "ws-a" "ws-b")))))))

;;;; ---- Tests: repo grouping + folding ----------------------------------

(ert-deftest agent-repl-test-ws-repo-key-uses-cached-group-key ()
  "`--ws-repo-key' short-circuits on the cached `:group-key' (no git)."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws" :project-dir "/tmp/ws/")
    (agent-repl--ws-put "ws" :group-key "/repos/doom/.git")
    (should (equal (agent-repl--ws-repo-key "ws") "/repos/doom/.git"))))

(ert-deftest agent-repl-test-repo-key-for-dir-nil-dir ()
  "`--repo-key-for-dir' returns nil for a nil DIR without shelling out."
  (cl-letf (((symbol-function 'agent-repl--git-string-quiet)
             (lambda (&rest _) (error "must not shell out for nil dir"))))
    (should (null (agent-repl--repo-key-for-dir nil)))))

(ert-deftest agent-repl-test-repo-key-for-dir-absolute-output ()
  "`--repo-key-for-dir' canonicalizes an absolute git common-dir."
  (cl-letf (((symbol-function 'agent-repl--git-string-quiet)
             (lambda (&rest _) "/repos/doom/.git")))
    (should (equal (agent-repl--repo-key-for-dir "/tmp/ws/")
                   (agent-repl--path-canonical "/repos/doom/.git")))))

(ert-deftest agent-repl-test-repo-key-for-dir-relative-output ()
  "`--repo-key-for-dir' expands a relative common-dir against DIR."
  (cl-letf (((symbol-function 'agent-repl--git-string-quiet)
             (lambda (&rest _) ".git")))
    (should (equal (agent-repl--repo-key-for-dir "/repos/doom/")
                   (agent-repl--path-canonical "/repos/doom/.git")))))

(ert-deftest agent-repl-test-repo-key-for-dir-fatal-output ()
  "`--repo-key-for-dir' maps a git \"fatal...\" answer onto nil."
  (cl-letf (((symbol-function 'agent-repl--git-string-quiet)
             (lambda (&rest _) "fatal: not a git repository")))
    (should (null (agent-repl--repo-key-for-dir "/tmp/nowhere/")))))

(ert-deftest agent-repl-test-repo-key-for-dir-empty-output ()
  "`--repo-key-for-dir' maps an empty git answer onto nil."
  (cl-letf (((symbol-function 'agent-repl--git-string-quiet)
             (lambda (&rest _) "")))
    (should (null (agent-repl--repo-key-for-dir "/tmp/ws/")))))

(ert-deftest agent-repl-test-ws-repo-key-derives-and-caches-group-key ()
  "`--ws-repo-key' derives via `--repo-key-for-dir' and caches `:group-key'."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws" :project-dir "/tmp/ws/")
    (cl-letf (((symbol-function 'agent-repl--ws-dir)
               (lambda (_ws) "/tmp/ws/"))
              ((symbol-function 'agent-repl--git-string-quiet)
               (lambda (&rest _) "/repos/doom/.git")))
      (let ((key (agent-repl--ws-repo-key "ws")))
        (should (equal key (agent-repl--path-canonical "/repos/doom/.git")))
        (should (equal (agent-repl--ws-get "ws" :group-key) key))))))

(ert-deftest agent-repl-test-ws-repo-group-falls-back-to-unknown-sentinel ()
  "`--ws-repo-group' maps an unresolvable repo onto the `(no repo)' sentinel."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws" :project-dir "/tmp/ws/")
    (cl-letf (((symbol-function 'agent-repl--git-string-quiet)
               (lambda (&rest _) "")))
      (should (equal (agent-repl--ws-repo-group "ws")
                     agent-repl--repo-key-unknown)))))

(ert-deftest agent-repl-test-repo-label-from-key ()
  "`--repo-label' returns the basename of the parent of KEY."
  (should (equal (agent-repl--repo-label "/x/y/explanation-engine/.git")
                 "explanation-engine")))

(ert-deftest agent-repl-test-repo-label-of-unknown-sentinel-is-the-sentinel ()
  "`--repo-label' passes the `(no repo)' sentinel through as its own label."
  (should (equal (agent-repl--repo-label agent-repl--repo-key-unknown)
                 agent-repl--repo-key-unknown)))

(ert-deftest agent-repl-test-repo-label-nil-key ()
  "`--repo-label' returns nil for a nil KEY."
  (should (null (agent-repl--repo-label nil))))

(ert-deftest agent-repl-test-toggle-repo-fold-folds ()
  "`--toggle-repo-fold' on an unfolded repo folds it."
  (agent-repl-test--with-clean-state
    (should (agent-repl--toggle-repo-fold "/repos/doom/.git"))
    (should (agent-repl--repo-folded-p "/repos/doom/.git"))))

(ert-deftest agent-repl-test-toggle-repo-fold-unfolds ()
  "`--toggle-repo-fold' on a folded repo unfolds it."
  (agent-repl-test--with-clean-state
    (agent-repl--toggle-repo-fold "/repos/doom/.git")
    (should-not (agent-repl--toggle-repo-fold "/repos/doom/.git"))
    (should-not (agent-repl--repo-folded-p "/repos/doom/.git"))))

(ert-deftest agent-repl-test-toggle-repo-fold-errors-on-nil-group ()
  "`--toggle-repo-fold' fails hard on a nil repo group rather than folding nothing."
  (agent-repl-test--with-clean-state
    (should-error (agent-repl--toggle-repo-fold nil))))

(ert-deftest agent-repl-test-repo-folded-p-false-for-untouched-repo ()
  "`--repo-folded-p' is nil for a repo that was never folded."
  (agent-repl-test--with-clean-state
    (should-not (agent-repl--repo-folded-p "/repos/doom/.git"))))

(ert-deftest agent-repl-test-folded-repo-keys-sorted ()
  "`--folded-repo-keys' returns the folded keys in sorted order."
  (agent-repl-test--with-clean-state
    (agent-repl--toggle-repo-fold "/b/.git")
    (agent-repl--toggle-repo-fold "/a/.git")
    (should (equal (agent-repl--folded-repo-keys) '("/a/.git" "/b/.git")))))

(ert-deftest agent-repl-test-filter-folded-names-drops-folded-repo ()
  "`--filter-folded-names' drops the workspaces of a folded repo."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "doom-a" :group-key "/repos/doom/.git")
    (agent-repl--ws-put "ee-a"   :group-key "/repos/explanation-engine/.git")
    (agent-repl--toggle-repo-fold "/repos/explanation-engine/.git")
    (should (equal (agent-repl--filter-folded-names '("doom-a" "ee-a") "doom-a")
                   '("doom-a")))))

(ert-deftest agent-repl-test-filter-folded-names-retains-current ()
  "`--filter-folded-names' keeps CURRENT-NAME even when its repo is folded."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ee-a" :group-key "/repos/explanation-engine/.git")
    (agent-repl--ws-put "ee-b" :group-key "/repos/explanation-engine/.git")
    (agent-repl--toggle-repo-fold "/repos/explanation-engine/.git")
    (should (equal (agent-repl--filter-folded-names '("ee-a" "ee-b") "ee-b")
                   '("ee-b")))))

(ert-deftest agent-repl-test-filter-folded-names-identity-when-nothing-folded ()
  "`--filter-folded-names' returns NAMES untouched when no repo is folded."
  (agent-repl-test--with-clean-state
    (let ((names '("a" "b")))
      (should (eq (agent-repl--filter-folded-names names "a") names)))))

(ert-deftest agent-repl-test-ws-tabline-names-excludes-folded-repo ()
  "`--ws-tabline-names' is the tab-bar list minus folded repos."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "doom-a" :group-key "/repos/doom/.git")
    (agent-repl--ws-put "ee-a"   :group-key "/repos/explanation-engine/.git")
    (agent-repl--toggle-repo-fold "/repos/explanation-engine/.git")
    (let ((persp-names-cache '("doom-a" "ee-a")))
      (cl-letf (((symbol-function 'agent-repl--ws-current-name)
                 (lambda () "doom-a")))
        (should (equal (agent-repl--ws-tabline-names) '("doom-a")))))))

(provide 'test-workspace)
;;; test-workspace.el ends here
