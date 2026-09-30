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
      (should (plist-get snapshot :killed-at)))))

(ert-deftest agent-repl-test-ws-plist-rejects-unknown-workspace ()
  "ws-plist makes an invalid serialization target fail loudly."
  (agent-repl-test--with-clean-state
    (should-error (agent-repl--ws-plist "missing") :type 'user-error)))

;;;; ---- Tests: ws-rename-state -------------------------------------------

(ert-deftest agent-repl-test-ws-rename-state-moves-complete-live-state ()
  "State rename moves the full plist, rewrites its dir, and clears its dir hash."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "old" :project-dir "/old/path")
    (agent-repl--ws-put "old" :priority :p1)
    (agent-repl--ws-put "old" :ws-dir-hash "stale")
    (cl-letf (((symbol-function 'agent-repl--path-canonical)
               (lambda (path) (concat "CANON:" path))))
      (should (equal (agent-repl--ws-rename-state
                      "old" "new" "/new/path")
                     "new")))
    (should-not (agent-repl--ws-known-p "old"))
    (should (agent-repl--ws-live-p "new"))
    (should (eq (agent-repl--ws-get "new" :priority) :p1))
    (should (equal (agent-repl--ws-get "new" :project-dir)
                   "CANON:/new/path"))
    (should-not (agent-repl--ws-get "new" :ws-dir-hash))))

(ert-deftest agent-repl-test-ws-rename-state-rejects-unknown-source ()
  "State rename rejects an unregistered source without creating a target."
  (agent-repl-test--with-clean-state
    (should-error
     (agent-repl--ws-rename-state "missing" "new" "/new/path")
     :type 'user-error)
    (should-not (agent-repl--ws-known-p "new"))))

(ert-deftest agent-repl-test-ws-rename-state-rejects-tombstoned-source ()
  "State rename does not resurrect a tombstone under a new name."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "old" :project-dir "/old/path")
    (agent-repl--ws-del "old")
    (should-error
     (agent-repl--ws-rename-state "old" "new" "/new/path")
     :type 'user-error)
    (should (agent-repl--ws-tombstoned-p "old"))
    (should-not (agent-repl--ws-known-p "new"))))

(ert-deftest agent-repl-test-ws-rename-state-rejects-known-target-atomically ()
  "Target collision leaves both workspace records byte-for-byte unchanged."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "old" :project-dir "/old/path")
    (agent-repl--ws-put "new" :project-dir "/occupied/path")
    (let ((old-before (agent-repl--ws-plist "old"))
          (new-before (agent-repl--ws-plist "new")))
      (should-error
       (agent-repl--ws-rename-state "old" "new" "/new/path")
       :type 'user-error)
      (should (equal (agent-repl--ws-plist "old") old-before))
      (should (equal (agent-repl--ws-plist "new") new-before)))))

(ert-deftest agent-repl-test-ws-rename-state-canonicalizes-before-mutation ()
  "A project-dir resolution error leaves the source registered and target absent."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "old" :project-dir "/old/path")
    (cl-letf (((symbol-function 'agent-repl--path-canonical)
               (lambda (_path) (error "cannot canonicalize"))))
      (should-error
       (agent-repl--ws-rename-state "old" "new" "/new/path")
       :type 'error))
    (should (agent-repl--ws-live-p "old"))
    (should-not (agent-repl--ws-known-p "new"))))

(ert-deftest agent-repl-test-ws-rename-state-keeps-the-selection-history-place ()
  "A renamed workspace keeps its place in the selection history, so closing
the workspace selected after it still lands on it."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "old" :project-dir "/old/path")
    (let ((agent-repl--workspace-history '("current" "old" "first")))
      ;; Act
      (agent-repl--ws-rename-state "old" "new" "/new/path")
      ;; Assert
      (should (equal agent-repl--workspace-history '("current" "new" "first"))))))

(ert-deftest agent-repl-test-ws-rename-state-rejected-leaves-the-selection-history ()
  "A rejected rename leaves the selection history untouched."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "old" :project-dir "/old/path")
    (agent-repl--ws-put "new" :project-dir "/occupied/path")
    (let ((agent-repl--workspace-history '("current" "old")))
      ;; Act
      (should-error (agent-repl--ws-rename-state "old" "new" "/new/path")
                    :type 'user-error)
      ;; Assert
      (should (equal agent-repl--workspace-history '("current" "old"))))))

;;;; ---- Tests: ws-rewrite-source-back-refs -------------------------------

(ert-deftest agent-repl-test-ws-rewrite-source-back-refs-targets-matches-only ()
  "Back-ref rewrite updates matching peers, clears their cache, and returns count."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "match" :project-dir "/match")
    (agent-repl--ws-put "match" :source-ws-dir "/source/old")
    (agent-repl--ws-put "match" :source-ws-name "old")
    (agent-repl--ws-put "other" :project-dir "/other")
    (agent-repl--ws-put "other" :source-ws-dir "/source/other")
    (agent-repl--ws-put "other" :source-ws-name "other")
    (agent-repl--ws-put "root" :project-dir "/root")
    (cl-letf (((symbol-function 'agent-repl--path-canonical)
               (lambda (path) (concat "CANON:" path))))
      (should (= 1 (agent-repl--ws-rewrite-source-back-refs
                    "/source/old" "/source/new"))))
    (should (equal (agent-repl--ws-get "match" :source-ws-dir)
                   "CANON:/source/new"))
    (should-not (agent-repl--ws-get "match" :source-ws-name))
    (should (equal (agent-repl--ws-get "other" :source-ws-dir)
                   "/source/other"))
    (should (equal (agent-repl--ws-get "other" :source-ws-name)
                   "other"))
    (should-not (agent-repl--ws-get "root" :source-ws-dir))))

(ert-deftest agent-repl-test-ws-rewrite-source-back-refs-updates-tombstones ()
  "Historical source identity is corrected even on tombstoned peers."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "child" :project-dir "/child")
    (agent-repl--ws-put "child" :source-ws-dir "/source/old")
    (agent-repl--ws-del "child")
    (should (= 1 (agent-repl--ws-rewrite-source-back-refs
                  "/source/old" "/source/new")))
    (should (agent-repl--ws-tombstoned-p "child"))
    (should (equal (agent-repl--ws-get "child" :source-ws-dir)
                   (agent-repl--path-canonical "/source/new")))))

(ert-deftest agent-repl-test-ws-rewrite-source-back-refs-rejects-identical-dirs ()
  "Canonical no-change requests fail before clearing any cached source name."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "child" :project-dir "/child")
    (agent-repl--ws-put "child" :source-ws-dir "/source")
    (agent-repl--ws-put "child" :source-ws-name "parent")
    (cl-letf (((symbol-function 'agent-repl--path-canonical)
               (lambda (_path) "SAME")))
      (should-error
       (agent-repl--ws-rewrite-source-back-refs "/old" "/new")
       :type 'user-error))
    (should (equal (agent-repl--ws-get "child" :source-ws-dir)
                   "/source"))
    (should (equal (agent-repl--ws-get "child" :source-ws-name)
                   "parent"))))

(ert-deftest agent-repl-test-ws-rewrite-source-back-refs-logs-peer-workspace ()
  "Each rewrite log is scoped to the peer whose back-reference changed."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "child" :project-dir "/child")
    (agent-repl--ws-put "child" :source-ws-dir "/source/old")
    (let (calls)
      (cl-letf (((symbol-function 'agent-repl--log)
                 (lambda (ws fmt &rest args)
                   (push (list ws (apply #'format fmt args)) calls))))
        (agent-repl--ws-rewrite-source-back-refs
         "/source/old" "/source/new"))
      (should
       (cl-find-if
        (lambda (call)
          (and (equal (car call) "child")
               (string-match-p "REWROTE" (cadr call))))
        calls)))))

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

(ert-deftest agent-repl-test-ws-put-stub-create-log-routes-globally ()
  "The stub-create record is emitted with a nil workspace.
Its subject IS that the entry has no `:project-dir', which is exactly what
denies it a durable sink, so the global sink is the record's correct
destination and the name travels in the message text instead."
  (agent-repl-test--with-clean-state
    ;; Arrange
    (let ((log-calls nil))
      (cl-letf (((symbol-function 'agent-repl--do-log)
                 (lambda (ws fmt args &optional _err)
                   (push (list ws fmt args) log-calls))))
        ;; Act
        (agent-repl--ws-put "stub-ws" :priority "p1"))
      ;; Assert
      (should (null (nth 0 (car log-calls)))))))

(ert-deftest agent-repl-test-ws-put-stub-create-names-the-workspace ()
  "Routing the stub-create record globally must not lose the workspace name."
  (agent-repl-test--with-clean-state
    ;; Arrange
    (let ((log-calls nil))
      (cl-letf (((symbol-function 'agent-repl--do-log)
                 (lambda (ws fmt args &optional _err)
                   (push (list ws fmt args) log-calls))))
        ;; Act
        (agent-repl--ws-put "stub-ws" :priority "p1"))
      ;; Assert
      (should (member "stub-ws" (nth 2 (car log-calls)))))))


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

;;;; ---- Tests: ws-forget (hard removal of a tombstone) ----

(ert-deftest agent-repl-test-ws-forget-removes-tombstoned-entry ()
  "ws-forget hard-removes a tombstoned entry from the hash."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
    (agent-repl--ws-del "ws1")
    (agent-repl--ws-forget "ws1")
    (should-not (agent-repl--ws-known-p "ws1"))))

(ert-deftest agent-repl-test-ws-forget-refuses-live-workspace ()
  "ws-forget signals rather than removing a live workspace."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
    (should-error (agent-repl--ws-forget "ws1"))
    (should (agent-repl--ws-live-p "ws1"))))

(ert-deftest agent-repl-test-ws-forget-refuses-unknown-workspace ()
  "ws-forget signals on a name that was never registered."
  (agent-repl-test--with-clean-state
    (should-error (agent-repl--ws-forget "never-registered"))))

;;;; ---- Tests: ws-del (tombstone semantics; moved from test-core.el) ----

(ert-deftest agent-repl-test-ws-del-clears-runtime-key ()
  "ws-del clears every key listed in `agent-repl--ws-runtime-keys'.
Asserts a representative runtime key (`:pending-show-panels') is reset to
nil so post-kill passes don't act on stale runtime intent."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
    (agent-repl--ws-put "ws1" :pending-show-panels t)
    (agent-repl--ws-del "ws1")
    (should-not (agent-repl--ws-get "ws1" :pending-show-panels))))

(ert-deftest agent-repl-test-ws-del-forgets-emacs-log-target-without-deleting-history ()
  "Tombstoning forgets only in-memory target ownership for a workspace name."
  (agent-repl-test--with-clean-state
    (let* ((target (make-temp-file "agent-repl-ws-history-"))
           (canonical (make-temp-file "agent-repl-ws-link-"))
           (agent-repl--workspace-log-targets (make-hash-table :test #'equal)))
      (unwind-protect
          (progn
            (delete-file canonical)
            (make-symbolic-link target canonical)
            (agent-repl--ws-put "ws1" :project-dir temporary-file-directory)
            ;; Installed under the IDENTITY key the forget sweep matches on.
            ;; A hand-placed entry under the old name key would let the
            ;; assertion below pass vacuously.
            (let ((identity (agent-repl--workspace-log-identity "ws1")))
              (puthash (agent-repl--workspace-log-target-key identity)
                       (append (list :target target) identity)
                       agent-repl--workspace-log-targets))
            (should (agent-repl--workspace-log-target-entry "ws1"))
            (cl-letf (((symbol-function 'agent-repl--log) (lambda (&rest _) nil)))
              (agent-repl--ws-del "ws1"))
            (should-not (agent-repl--workspace-log-target-entry "ws1"))
            (should (file-exists-p target))
            (should (file-symlink-p canonical)))
        (when (file-symlink-p canonical) (delete-file canonical))
        (when (file-exists-p target) (delete-file target))))))

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

(ert-deftest agent-repl-test-ws-del-stamps-killed-at ()
  "ws-del stamps `:killed-at' with a non-nil time value — the marker
read by `--ws-live-p' and the snapshot persistence layer to distinguish
tombstones from live entries."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
    (agent-repl--ws-del "ws1")
    (should (agent-repl--ws-get "ws1" :killed-at))))

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
A release handler reads the key it releases pre-clear (`:frontend-buffer'
is the webview's); a regression that moves the hook after the clear loop
would silently strand every resource one of them owns."
  (agent-repl-test--with-clean-state
    (let ((buf (generate-new-buffer " *fake-webview*")))
      (unwind-protect
          (progn
            (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
            (agent-repl--ws-put "ws1" :frontend-buffer buf)
            (let ((seen 'unset)
                  (agent-repl-ws-del-hook nil))
              (add-hook 'agent-repl-ws-del-hook
                        (lambda (ws)
                          (setq seen (agent-repl--ws-get ws :frontend-buffer))))
              ;; Act
              (agent-repl--ws-del "ws1")
              ;; Assert — the hook observed the pre-clear value, and the
              ;; tombstone cleared it afterwards.
              (should (eq seen buf))
              (should (null (agent-repl--ws-get "ws1" :frontend-buffer)))))
        (kill-buffer buf)))))

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
intact (just with `:killed-at' stamped)."
  (agent-repl-test--with-clean-state
    (puthash "doomed" '(:project-dir "/tmp/x") agent-repl--workspaces)
    (agent-repl--ws-del "doomed")
    (should (gethash "doomed" agent-repl--workspaces))
    (should (agent-repl--ws-get "doomed" :killed-at))
    (should (equal (agent-repl--ws-get "doomed" :project-dir) "/tmp/x"))))

;;;; ---- Tests: ws-live-p (moved from test-core.el) ----

(ert-deftest agent-repl-test-ws-live-p-returns-t-for-live-entry ()
  "ws-live-p returns non-nil for a fresh hash entry with no tombstone."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
    (should (agent-repl--ws-live-p "ws1"))))

(ert-deftest agent-repl-test-ws-live-p-returns-nil-for-tombstone ()
  "ws-live-p returns nil for a tombstoned entry — the predicate that
keeps tab-bar/picker/state-updater from surfacing killed workspaces."
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
to avoid surfacing killed workspaces."
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

(ert-deftest agent-repl-test-live-ws-names-excludes-persp-nil-name ()
  "`persp-nil-name' (\"none\") is never a workspace candidate."
  (agent-repl-test--with-clean-state
    ;; Arrange
    (let ((persp-nil-name "none"))
      (agent-repl--ws-put "none" :ref "persp-own")
      (agent-repl--ws-put "real-ws" :project-dir "/tmp/real")
      ;; Act
      (let ((names (agent-repl--live-ws-names)))
        ;; Assert
        (should-not (member "none" names))
        (should (member "real-ws" names))))))

(ert-deftest agent-repl-test-live-ws-names-excludes-doom-main ()
  "Doom's startup perspective is never a workspace candidate."
  (agent-repl-test--with-clean-state
    ;; Arrange
    (let ((+workspaces-main "main"))
      (agent-repl--ws-put "main" :ref "doom-own")
      (agent-repl--ws-put "real-ws" :project-dir "/tmp/real")
      ;; Act
      (let ((names (agent-repl--live-ws-names)))
        ;; Assert
        (should-not (member "main" names))
        (should (member "real-ws" names))))))

;;;; ---- Tests: ws-registered-names ---------------------------------------

(ert-deftest agent-repl-test-ws-registered-names-includes-live-and-tombstoned ()
  "The complete registration view includes both lifecycle states."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "live" :project-dir "/tmp/live")
    (agent-repl--ws-put "tombstone" :project-dir "/tmp/tombstone")
    (agent-repl--ws-del "tombstone")
    (let ((names (agent-repl--ws-registered-names)))
      (should (member "live" names))
      (should (member "tombstone" names))
      (should (= 2 (length names))))))

(ert-deftest agent-repl-test-ws-registered-names-preserves-raw-hash-order ()
  "The wrapper returns the hash's native key traversal without sorting."
  (agent-repl-test--with-clean-state
    (dolist (name '("charlie" "alpha" "bravo"))
      (agent-repl--ws-put name :project-dir (concat "/tmp/" name)))
    (should (equal (agent-repl--ws-registered-names)
                   (hash-table-keys agent-repl--workspaces)))))

(ert-deftest agent-repl-test-ws-registered-names-empty-hash ()
  "An empty registration table returns nil."
  (agent-repl-test--with-clean-state
    (should-not (agent-repl--ws-registered-names))))

(ert-deftest agent-repl-test-ws-registered-names-logs-verbose-snapshot ()
  "Enumeration emits its complete key snapshot through verbose logging."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "live" :project-dir "/tmp/live")
    (let (captured)
      (cl-letf (((symbol-function 'agent-repl--log-verbose)
                 (lambda (ws fmt &rest args)
                   (setq captured
                         (list ws (apply #'format fmt args))))))
        (agent-repl--ws-registered-names))
      (should (agent-repl--central-log-scope-reason (car captured)))
      (should (string-match-p "count=1" (cadr captured)))
      (should (string-match-p "live" (cadr captured))))))

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
  "Project poll partition excludes tombstones and reports live placeholders.
The placeholders are registered workspaces that have not yet been given a
`:project-dir'.  persp-mode's own perspectives are NOT among them: they are
no longer workspace candidates anywhere, so the poller never sees them."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "project" :project-dir "/tmp/project")
    (agent-repl--ws-put "pending-a" :agent-state :idle)
    (agent-repl--ws-put "pending-b" :repl-state :inactive)
    (agent-repl--ws-put "dead" :project-dir "/tmp/dead")
    (agent-repl--ws-del "dead")
    (pcase-let ((`(,pollable . ,placeholders)
                 (agent-repl--ws-project-poll-partition)))
      (should (equal pollable '("project")))
      (should (equal (sort placeholders #'string<) '("pending-a" "pending-b"))))))

(ert-deftest agent-repl-test-ws-project-poll-partition-omits-pseudo-perspectives ()
  "A pseudo perspective is not even a placeholder: it is not a candidate."
  (agent-repl-test--with-clean-state
    ;; Arrange
    (let ((persp-nil-name "none")
          (+workspaces-main "main"))
      (agent-repl--ws-put "none" :repl-state :inactive)
      (agent-repl--ws-put "main" :agent-state :idle)
      ;; Act
      (pcase-let ((`(,pollable . ,placeholders)
                   (agent-repl--ws-project-poll-partition)))
        ;; Assert
        (should-not pollable)
        (should-not placeholders)))))

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
  "ws-dir-owner ignores a tombstoned (`:killed-at') entry owning the dir, so a
dead shadow never counts as the owner."
  (agent-repl-test--with-clean-state
    (let ((dir (agent-repl--path-canonical "/home/user/proj")))
      (agent-repl--ws-put "dead" :project-dir dir)
      (agent-repl--ws-put "dead" :killed-at '(1 2 3 4))
      (should-not (agent-repl--ws-dir-owner dir)))))

;;;; ---- Tests: --ws-known-p ----

(ert-deftest agent-repl-test-ws-known-p-returns-t-for-live-entry ()
  "A workspace with a hash entry and no :killed-at is known."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/tmp/x")
    (should (agent-repl--ws-known-p "ws1"))))

(ert-deftest agent-repl-test-ws-known-p-returns-t-for-tombstoned-entry ()
  "A tombstoned workspace (entry + :killed-at set) is still known."
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
    (let ((logged-workspace nil))
      (cl-letf (((symbol-function 'agent-repl--log)
                 (lambda (ws &rest _args) (setq logged-workspace ws))))
        (should-error (agent-repl--ws-require-known "missing" "ctx")
                      :type 'user-error))
      (should (agent-repl--central-log-scope-reason logged-workspace)))))

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
  "A live workspace (no :killed-at) is not tombstoned."
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


;;;; ---- Tests: --ws-revive ----

(ert-deftest agent-repl-test-ws-revive-clears-the-tombstone ()
  "Reviving a tombstoned workspace makes the name live again."
  ;; Arrange.
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/tmp/x")
    (agent-repl--ws-del "ws1")
    ;; Act.
    (agent-repl--ws-revive "ws1")
    ;; Assert.
    (should (agent-repl--ws-live-p "ws1"))))

(ert-deftest agent-repl-test-ws-revive-keeps-last-killed-at ()
  "A revive keeps `:last-killed-at': the previous close still happened."
  ;; Arrange.
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/tmp/x")
    (agent-repl--ws-del "ws1")
    ;; Act.
    (agent-repl--ws-revive "ws1")
    ;; Assert.
    (should (agent-repl--ws-get "ws1" :last-killed-at))))

(ert-deftest agent-repl-test-ws-revive-on-a-live-workspace-answers-nil ()
  "Reviving an already-live workspace moves nothing and answers nil."
  ;; Arrange.
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/tmp/x")
    ;; Act / Assert.
    (should-not (agent-repl--ws-revive "ws1"))))

(ert-deftest agent-repl-test-ws-revive-on-an-unknown-workspace-answers-nil ()
  "Reviving an unknown name creates no entry and answers nil."
  ;; Arrange.
  (agent-repl-test--with-clean-state
    (let ((logged-workspace nil))
      (cl-letf (((symbol-function 'agent-repl--log)
                 (lambda (ws &rest _args) (setq logged-workspace ws))))
        ;; Act.
        (should-not (agent-repl--ws-revive "missing")))
    ;; Assert.
      (should-not (agent-repl--ws-known-p "missing"))
      (should (agent-repl--central-log-scope-reason logged-workspace)))))

(ert-deftest agent-repl-test-ws-revive-restores-ref-id-reverse-lookup ()
  "After a revive the name answers `--ws-by-ref-id' again."
  ;; Arrange.
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/tmp/x")
    (agent-repl--ws-put "ws1" :ref '(:id "id-1" :dir "/tmp/x"))
    (agent-repl--ws-del "ws1")
    (should-not (agent-repl--ws-by-ref-id "id-1"))
    ;; Act.
    (agent-repl--ws-revive "ws1")
    (agent-repl--ws-put "ws1" :ref '(:id "id-1" :dir "/tmp/x"))
    ;; Assert.
    (should (equal (agent-repl--ws-by-ref-id "id-1") "ws1"))))

;;;; ---- Tests: --ws-hide-tombstoned-p ----

;;;; ---- Tests: --ws-hide-tombstoned-names ----

;;;; ---- Tests: --ws-render-status nil for hide-tombstoned ----

(ert-deftest agent-repl-test-ws-render-status-nil-for-hide-tombstoned ()
  "Render-status returns nil for hide-tombstoned ws, collapsed with kill-tombstoned."
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

(ert-deftest agent-repl-test-ws-list-names-excludes-doom-main ()
  "The tab-bar iteration source drops Doom's startup perspective."
  (agent-repl-test--with-clean-state
    ;; Arrange
    (let ((+workspaces-main "main")
          (persp-names-cache '("main" "real-ws")))
      (agent-repl--ws-put "main" :ref "doom-own")
      (agent-repl--ws-put "real-ws" :project-dir "/tmp/y")
      ;; Act
      (let ((result (agent-repl--ws-list-names)))
        ;; Assert
        (should-not (member "main" result))
        (should (member "real-ws" result))))))

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
This case is rare in production (the kill path removes from cache
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

;;;; ---- Tests: reorder-workspace-by-priority (moved from test-status.el) ----

;;;; ---- Tests: --reorder-workspace-next-to ----

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

;;;; ---- Tests: sidebar repaint on tab-bar departure ----
;;
;; A killed workspace loses its sidebar row, so the roster must be
;; re-pushed at the kill rather than at the next 1Hz signature tick.

(ert-deftest agent-repl-test-ws-persp-kill-forces-the-sidebar-repaint ()
  "`--ws-persp-kill' pushes a fresh roster after the persp is gone.
The push is FORCED past the sidebar's signature gate: the membership
cache the signature reads may not have registered the kill yet, and a
gated push would then drop the very repaint this exists for."
  (agent-repl-test--with-clean-state
    (let (pushed)
      (cl-letf (((symbol-function 'persp-kill) (lambda (_ws)))
                ((symbol-function 'agent-repl--sidebar-push)
                 (lambda (&optional force) (setq pushed (list :force force)))))
        (agent-repl--ws-persp-kill "doomed")
        (should (equal pushed '(:force t)))))))

(ert-deftest agent-repl-test-ws-persp-kill-repaints-after-the-persp-is-gone ()
  "The repaint runs AFTER `persp-kill', so the roster sees the removal."
  (agent-repl-test--with-clean-state
    (let ((order nil))
      (cl-letf (((symbol-function 'persp-kill)
                 (lambda (_ws) (push :killed order)))
                ((symbol-function 'agent-repl--sidebar-push)
                 (lambda (&optional _force) (push :pushed order))))
        (agent-repl--ws-persp-kill "doomed")
        (should (equal (nreverse order) '(:killed :pushed)))))))

(ert-deftest agent-repl-test-ws-persp-kill-repaints-the-sidebar ()
  "`--ws-persp-kill' repaints too — it is the other tab-bar exit."
  (agent-repl-test--with-clean-state
    (let (pushed)
      (cl-letf (((symbol-function 'persp-kill) (lambda (_ws)))
                ((symbol-function 'agent-repl--sidebar-push)
                 (lambda (&optional _force) (setq pushed t))))
        (agent-repl--ws-persp-kill "doomed")
        (should (eq pushed t))))))

(ert-deftest agent-repl-test-ws-repaint-sidebar-survives-a-push-error ()
  "A failing roster push never turns a teardown into an error."
  (agent-repl-test--with-clean-state
    (let (warned)
      (cl-letf (((symbol-function 'agent-repl--sidebar-push)
                 (lambda () (error "boom")))
                ;; The contained failure rides the warn rung, so the helper's
                ;; value is the log emitter's rather than a contract; a signal
                ;; escaping the containment still fails this test.
                ((symbol-function 'agent-repl--warn)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) warned))))
        (agent-repl--ws-repaint-sidebar "doomed" "test")
        (should (cl-some (lambda (l) (string-match-p "push error" l)) warned))))))

(ert-deftest agent-repl-test-ws-repaint-sidebar-noop-without-sidebar ()
  "The repaint is skipped when sidebar.el is not loaded."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--sidebar-push) nil))
      (should-not (agent-repl--ws-repaint-sidebar "doomed" "test")))))

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
    (let (captured logged-workspace)
      (cl-letf (((symbol-function 'agent-repl--ws-resolve-persp) (lambda (_ws) 'a-persp))
                ((symbol-function 'persp-rename)
                 (lambda (new persp) (setq captured (list new persp)) t))
                ((symbol-function 'agent-repl--log)
                 (lambda (ws &rest _args) (setq logged-workspace ws))))
        (should (agent-repl--ws-rename-persp "old" "new"))
        (should (equal captured '("new" a-persp)))
        (should (equal logged-workspace "new"))))))

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

(ert-deftest agent-repl-test-ws-persp-identity-is-bounded-and-opaque ()
  "Perspective diagnostics never print the recursive perspective payload."
  (let* ((secret "SECRET-PERSPECTIVE-PAYLOAD")
         (persp (list :name "ws" :window-state (list secret)))
         (identity (agent-repl--ws-persp-identity persp)))
    (should (equal identity (agent-repl--ws-persp-identity persp)))
    (should (string-match-p "\\`persp@[[:xdigit:]-]+\\'" identity))
    (should (< (string-bytes identity) 40))
    (should-not (string-match-p secret identity))))

(ert-deftest agent-repl-test-ws-persp-identity-rejects-nil-with-canonical-log ()
  "A missing perspective identity logs its cause before signalling."
  (let (record)
    (cl-letf (((symbol-function 'agent-repl--log)
               (lambda (ws fmt &rest args)
                 (setq record (list ws (apply #'format fmt args))))))
      (should-error (agent-repl--ws-persp-identity nil) :type 'error))
    (should (agent-repl--central-log-scope-reason (car record)))
    (should (equal (cadr record)
                   "ws-persp-identity: rejected reason=nil-perspective"))))

;;;; ---- Tests: --ws-switch-project-display ----

(ert-deftest agent-repl-test-switch-project-display-workspace-keeps-its-panel ()
  "A switch to a workspace dir opens NO magit: the panel owns that display."
  (agent-repl-test--with-clean-state
    ;; Arrange.
    (let (magit-dirs)
      (cl-letf (((symbol-function 'agent-repl--ws-name-for-dir) (lambda (_dir) "ws-one"))
                ((symbol-function 'doom-real-buffer-list) (lambda (&optional _b) nil))
                ((symbol-function 'agent-repl--magit-status-same-window)
                 (lambda (dir) (push dir magit-dirs))))
        ;; Act.
        (agent-repl--ws-switch-project-display "/tmp/ws-one/")
        ;; Assert.
        (should-not magit-dirs)))))

(ert-deftest agent-repl-test-switch-project-display-empty-project-shows-magit ()
  "A plain project with nothing open still lands on magit status."
  (agent-repl-test--with-clean-state
    ;; Arrange.
    (let (magit-dirs)
      (cl-letf (((symbol-function 'agent-repl--ws-name-for-dir) (lambda (_dir) nil))
                ((symbol-function 'doom-real-buffer-list) (lambda (&optional _b) nil))
                ((symbol-function 'agent-repl--magit-status-same-window)
                 (lambda (dir) (push dir magit-dirs))))
        ;; Act.
        (agent-repl--ws-switch-project-display "/tmp/plain/")
        ;; Assert.
        (should (equal magit-dirs '("/tmp/plain/")))))))

(ert-deftest agent-repl-test-switch-project-display-open-project-opens-nothing ()
  "A plain project that already has buffers open is left as it is."
  (agent-repl-test--with-clean-state
    ;; Arrange.
    (let (magit-dirs)
      (cl-letf (((symbol-function 'agent-repl--ws-name-for-dir) (lambda (_dir) nil))
                ((symbol-function 'doom-real-buffer-list)
                 (lambda (&optional _b) (list (current-buffer))))
                ((symbol-function 'agent-repl--magit-status-same-window)
                 (lambda (dir) (push dir magit-dirs))))
        ;; Act.
        (agent-repl--ws-switch-project-display "/tmp/plain/")
        ;; Assert.
        (should-not magit-dirs)))))

;;;; ---- Tests: --ws-install-persp-policy ----

(ert-deftest agent-repl-test-persp-policy-never-recycles-the-workspace-being-left ()
  "A project switch always makes its own workspace, never renaming the old one.

Doom's `+workspaces-switch-to-project-h\=' recycles the workspace being
LEFT -- `+workspace-rename\=' onto the entered project's name -- whenever
`+workspaces-on-switch-project-behavior\=' is `non-empty\=' and
`+workspace-buffer-list\=' is empty.  An agent-repl workspace showing its
agent IS empty by that test, because the panel is a webview buffer and
deliberately not a `doom-real-buffer-list\=' member, so the abandoned
workspace's persp left `persp-names-cache\=' under its old name while the
registry and the roster kept carrying it -- and its TAB vanished."
  (agent-repl-test--with-clean-state
    ;; Arrange.
    (let ((+workspaces-on-switch-project-behavior 'non-empty)
          (+workspaces-switch-project-function nil)
          (persp-auto-resume-time nil)
          (persp-auto-save-opt nil)
          (persp-kill-foreign-buffer-behaviour nil)
          (persp-set-frame-buffer-predicate nil))
      ;; Act.
      (agent-repl--ws-install-persp-policy)
      ;; Assert.
      (should (eq +workspaces-on-switch-project-behavior t)))))

(ert-deftest agent-repl-test-persp-policy-lands-a-project-switch-on-the-panel ()
  "The policy installs agent-repl's own switch-project display function."
  (agent-repl-test--with-clean-state
    ;; Arrange.
    (let ((+workspaces-on-switch-project-behavior nil)
          (+workspaces-switch-project-function nil)
          (persp-auto-resume-time nil)
          (persp-auto-save-opt nil)
          (persp-kill-foreign-buffer-behaviour nil)
          (persp-set-frame-buffer-predicate nil))
      ;; Act.
      (agent-repl--ws-install-persp-policy)
      ;; Assert.
      (should (eq +workspaces-switch-project-function
                  #'agent-repl--ws-switch-project-display)))))

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

(ert-deftest agent-repl-test-ws-persp-kill-retires-the-windows-first ()
  "The persp's windows are retired BEFORE `persp-kill' walks its buffers.
persp-mode retires a removed buffer from its window with
`set-window-buffer', which a strongly dedicated agent panel window
refuses -- so a kill that reached persp-mode first aborted mid-walk and
left the workspace's tab on the bar."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((order nil))
      (cl-letf (((symbol-function 'agent-repl--ws-resolve-persp) (lambda (_ws) 'persp))
                ((symbol-function 'agent-repl--ws-buffers)
                 (lambda (_persp) (list (current-buffer))))
                ((symbol-function 'agent-repl-window--delete-buffer-windows)
                 (lambda (&rest _) (push 'retire order)))
                ((symbol-function 'persp-kill) (lambda (_ws) (push 'kill order))))
        ;; Act
        (agent-repl--ws-persp-kill "doomed")
        ;; Assert
        (should (equal (nreverse order) '(retire kill)))))))

;;;; ---- Tests: --ws-retire-persp-windows ----

(ert-deftest agent-repl-test-ws-retire-persp-windows-retires-each-live-buffer ()
  "Every live buffer of the persp has its windows retired."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((buf-a (generate-new-buffer " *retire-a*"))
          (buf-b (generate-new-buffer " *retire-b*"))
          (retired nil))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-repl--ws-resolve-persp) (lambda (_ws) 'persp))
                    ((symbol-function 'agent-repl--ws-buffers)
                     (lambda (_persp) (list buf-a buf-b)))
                    ((symbol-function 'agent-repl-window--delete-buffer-windows)
                     (lambda (buf &rest _) (push buf retired))))
            ;; Act
            (agent-repl--ws-retire-persp-windows "doomed")
            ;; Assert
            (should (equal (nreverse retired) (list buf-a buf-b))))
        (kill-buffer buf-a)
        (kill-buffer buf-b)))))

(ert-deftest agent-repl-test-ws-retire-persp-windows-skips-a-dead-buffer ()
  "A buffer persp-mode still lists but that is already killed is skipped."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((dead (generate-new-buffer " *retire-dead*"))
          (retired nil))
      (kill-buffer dead)
      (cl-letf (((symbol-function 'agent-repl--ws-resolve-persp) (lambda (_ws) 'persp))
                ((symbol-function 'agent-repl--ws-buffers) (lambda (_persp) (list dead)))
                ((symbol-function 'agent-repl-window--delete-buffer-windows)
                 (lambda (buf &rest _) (push buf retired))))
        ;; Act
        (agent-repl--ws-retire-persp-windows "doomed")
        ;; Assert
        (should-not retired)))))

(ert-deftest agent-repl-test-ws-retire-persp-windows-noop-without-a-persp ()
  "A name with no live perspective retires nothing."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((retired nil))
      (cl-letf (((symbol-function 'agent-repl--ws-resolve-persp) (lambda (_ws) nil))
                ((symbol-function 'agent-repl-window--delete-buffer-windows)
                 (lambda (buf &rest _) (push buf retired))))
        ;; Act
        (agent-repl--ws-retire-persp-windows "doomed")
        ;; Assert
        (should-not retired)))))

(ert-deftest agent-repl-test-ws-retire-persp-windows-skips-a-foreign-buffer ()
  "A foreign-owned buffer drifted into the persp is not passed to window deletion."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((foreign (generate-new-buffer " *retire-foreign*"))
          (retired nil))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-repl--ws-resolve-persp) (lambda (_ws) 'persp))
                    ((symbol-function 'agent-repl--ws-buffers) (lambda (_persp) (list foreign)))
                    ((symbol-function 'agent-repl--foreign-owned-buffer-p)
                     (lambda (buf _ws) (eq buf foreign)))
                    ((symbol-function 'agent-repl--buffer-owner) (lambda (_buf) "neighbor"))
                    ((symbol-function 'agent-repl-window--delete-buffer-windows)
                     (lambda (buf &rest _) (push buf retired))))
            ;; Act
            (agent-repl--ws-retire-persp-windows "doomed")
            ;; Assert
            (should-not retired))
        (kill-buffer foreign)))))

(ert-deftest agent-repl-test-ws-retire-persp-windows-retires-an-owned-buffer ()
  "An owned buffer is passed to window deletion even when the foreign-skip check runs."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((owned (generate-new-buffer " *retire-owned*"))
          (retired nil))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-repl--ws-resolve-persp) (lambda (_ws) 'persp))
                    ((symbol-function 'agent-repl--ws-buffers) (lambda (_persp) (list owned)))
                    ((symbol-function 'agent-repl--foreign-owned-buffer-p) (lambda (_buf _ws) nil))
                    ((symbol-function 'agent-repl-window--delete-buffer-windows)
                     (lambda (buf &rest _) (push buf retired))))
            ;; Act
            (agent-repl--ws-retire-persp-windows "doomed")
            ;; Assert
            (should (equal retired (list owned))))
        (kill-buffer owned)))))

;;;; ---- Tests: --ws-shared-unowned-buffer-p ----

(ert-deftest agent-repl-test-ws-shared-unowned-buffer-p-answers-t-for-a-buffer-another-persp-holds ()
  "An unowned buffer that another live persp also holds is shared."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((buf (generate-new-buffer " *shared*")))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-repl--ws-resolve-persp) (lambda (_ws) 'persp))
                    ((symbol-function 'persp-other-persps-with-buffer-except-nil)
                     (lambda (&rest _) (list 'keeper-persp))))
            ;; Act / Assert
            (should (agent-repl--ws-shared-unowned-buffer-p buf "doomed")))
        (kill-buffer buf)))))

(ert-deftest agent-repl-test-ws-shared-unowned-buffer-p-answers-nil-for-a-buffer-only-this-persp-holds ()
  "An unowned buffer no other persp holds belongs to the dying workspace alone."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((buf (generate-new-buffer " *solo*")))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-repl--ws-resolve-persp) (lambda (_ws) 'persp))
                    ((symbol-function 'persp-other-persps-with-buffer-except-nil)
                     (lambda (&rest _) nil)))
            ;; Act / Assert
            (should-not (agent-repl--ws-shared-unowned-buffer-p buf "doomed")))
        (kill-buffer buf)))))

(ert-deftest agent-repl-test-ws-shared-unowned-buffer-p-answers-nil-for-an-owned-buffer ()
  "A buffer an agent-repl workspace owns is answered by ownership, not sharing."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((buf (generate-new-buffer " *owned*")))
      (unwind-protect
          (progn
            (with-current-buffer buf (setq-local agent-repl--owning-workspace "doomed"))
            (cl-letf (((symbol-function 'agent-repl--ws-resolve-persp) (lambda (_ws) 'persp))
                      ((symbol-function 'persp-other-persps-with-buffer-except-nil)
                       (lambda (&rest _) (list 'keeper-persp))))
              ;; Act / Assert
              (should-not (agent-repl--ws-shared-unowned-buffer-p buf "doomed"))))
        (kill-buffer buf)))))

(ert-deftest agent-repl-test-ws-retire-persp-windows-skips-a-shared-buffer ()
  "An unowned buffer another persp also holds keeps its windows: they may be
the landing workspace's."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((shared (generate-new-buffer " *retire-shared*"))
          (retired nil))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-repl--ws-resolve-persp) (lambda (_ws) 'persp))
                    ((symbol-function 'agent-repl--ws-buffers) (lambda (_persp) (list shared)))
                    ((symbol-function 'persp-other-persps-with-buffer-except-nil)
                     (lambda (&rest _) (list 'keeper-persp)))
                    ((symbol-function 'agent-repl-window--delete-buffer-windows)
                     (lambda (buf &rest _) (push buf retired))))
            ;; Act
            (agent-repl--ws-retire-persp-windows "doomed")
            ;; Assert
            (should-not retired))
        (kill-buffer shared)))))

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

;;;; ---- The two context cuts --------------------------------------------

(ert-deftest agent-repl-test-ws-state-icon-clearing ()
  ":clearing has a glyph of its own in `agent-repl-ws-state-icons'."
  ;; Act / Assert
  (should (equal (alist-get :clearing agent-repl-ws-state-icons) "🧹")))

(ert-deftest agent-repl-test-ws-state-icon-compacting ()
  ":compacting has a glyph of its own in `agent-repl-ws-state-icons'."
  ;; Act / Assert
  (should (equal (alist-get :compacting agent-repl-ws-state-icons) "🗜")))

(ert-deftest agent-repl-test-ws-state-icon-turn-failed ()
  ":turn-failed has a glyph of its own in `agent-repl-ws-state-icons'."
  ;; Act / Assert
  (should (equal (alist-get :turn-failed agent-repl-ws-state-icons) "⚠")))

(ert-deftest agent-repl-test-ws-severed-takes-a-glyph-of-its-own ()
  "`:severed\=' gets its OWN glyph, never the sleep one.
Color and glyph are the only two things a tab carries, so reusing 💤 for
a broken substrate would undo half the split at the glance that matters."
  ;; Act / Assert
  (let ((severed (alist-get :severed agent-repl-ws-state-icons)))
    (should (stringp severed))
    (should-not (equal severed (alist-get :hibernated agent-repl-ws-state-icons)))))

;;;; ---- Tests: log-scoped current-workspace resolution ----

(ert-deftest agent-repl-test-ws-current-log-name-rejects-persp-placeholder ()
  "Outside any workspace the current perspective owns no log sink."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "none")))
      (should-not (agent-repl--ws-current-log-name)))))

(ert-deftest agent-repl-test-ws-current-log-name-returns-registered-workspace ()
  "Inside a registered workspace the current name is returned unchanged."
  (agent-repl-test--with-clean-state
    (let ((project (make-temp-file "agent-repl-current-log-name-" t)))
      (unwind-protect
          (progn
            (agent-repl--ws-put "live-ws" :project-dir project)
            (cl-letf (((symbol-function '+workspace-current-name)
                       (lambda () "live-ws")))
              (should (equal (agent-repl--ws-current-log-name) "live-ws"))))
        (delete-directory project t)))))

(ert-deftest agent-repl-test-ws-current-log-name-leaves-behavioral-name-untouched ()
  "The unscreened persp identity boundary still reports the placeholder.
The 130 behavioral callers that switch, compare, and resolve directories
must keep seeing what persp-mode actually says."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "none")))
      (should (equal (agent-repl--ws-current-name) "none")))))

;;;; ---- Tests: the unfinished-merge teardown guard ----
;;
;; The ONE merge responsibility Emacs still carries.  Killing a workspace
;; mid-merge kills the session the daemon's merge lease drives conflict
;; resolution through, so the primitive refuses before it touches anything.

(ert-deftest agent-repl-test-merge-unfinished-p-nil-when-merged ()
  "`:merged' is terminal, so the workspace is free to be torn down."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws" :pushed-render-state :merged)
    (should-not (agent-repl--ws-merge-unfinished-p "ws"))))

(ert-deftest agent-repl-test-merge-unfinished-p-nil-when-merge-failed ()
  "`:merge-failed' is terminal too: the daemon reached a verdict."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws" :pushed-render-state :merge-failed)
    (should-not (agent-repl--ws-merge-unfinished-p "ws"))))

(ert-deftest agent-repl-test-merge-unfinished-p-nil-for-non-merge-state ()
  "An ordinary render state is not a merge at all."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws" :pushed-render-state :thinking)
    (should-not (agent-repl--ws-merge-unfinished-p "ws"))))

(ert-deftest agent-repl-test-merge-unfinished-p-nil-when-nothing-pushed ()
  "A workspace with no pushed state has no merge to be unfinished."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws" :project-dir "/tmp/ws")
    (should-not (agent-repl--ws-merge-unfinished-p "ws"))))

(ert-deftest agent-repl-test-teardown-guard-passes-on-terminal-merge ()
  "The assertion returns quietly once the merge has a verdict."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws" :pushed-render-state :merged)
    (should-not (agent-repl--assert-mergeable-teardown "ws"))))

;;;; ---- ws-log-name screens a caller-supplied name ----

(ert-deftest agent-repl-test-workspace-ws-log-name-demotes-a-persp-placeholder ()
  "A persp-mode placeholder name resolves to the global sink.
\"main\" and \"none\" are real perspectives that are not agent-repl
workspaces, so they own no `:project-dir' and no durable log sink."
  (agent-repl-test--with-clean-state
    ;; Arrange / Act
    (let ((resolved (agent-repl--ws-log-name "main")))
      ;; Assert
      (should (null resolved)))))

(ert-deftest agent-repl-test-workspace-ws-log-name-keeps-a-registered-workspace ()
  "A workspace that owns a sink is returned unchanged.
The screen must only demote names that could not be routed at all."
  (agent-repl-test--with-clean-state
    ;; Arrange
    (let ((project (make-temp-file "agent-repl-log-name-" t)))
      (unwind-protect
          (progn
            (agent-repl--ws-put "ws1" :project-dir project)
            ;; Act
            (let ((resolved (agent-repl--ws-log-name "ws1")))
              ;; Assert
              (should (equal resolved "ws1"))))
        (delete-directory project t)))))


;;;; ---- The ref id is the identity ---------------------------------------

(ert-deftest agent-repl-test-ws-by-ref-id-finds-the-workspace ()
  "A workspace is found by the daemon-minted id it echoes."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "alpha" :ref '(:id "ws-1" :dir "/w/1"))
    ;; Act / Assert
    (should (equal (agent-repl--ws-by-ref-id "ws-1") "alpha"))))

(ert-deftest agent-repl-test-ws-by-ref-id-answers-nil-for-an-unknown-id ()
  "An id no workspace carries resolves to nothing."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "alpha" :ref '(:id "ws-1" :dir "/w/1"))
    ;; Act / Assert
    (should (null (agent-repl--ws-by-ref-id "ws-2")))))

(ert-deftest agent-repl-test-ws-by-ref-id-ignores-a-tombstone ()
  "A tombstone has no tab, so answering with one would resurrect it."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "alpha" :ref '(:id "ws-1" :dir "/w/1"))
    (agent-repl--ws-put "alpha" :killed-at (current-time))
    ;; Act / Assert
    (should (null (agent-repl--ws-by-ref-id "ws-1")))))

(ert-deftest agent-repl-test-ws-by-ref-id-does-not-match-on-the-dir ()
  "The dir is display, never a key: paths have many spellings."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "alpha" :ref '(:id "ws-1" :dir "/w/1"))
    ;; Act / Assert
    (should (null (agent-repl--ws-by-ref-id "/w/1")))))

;;;; ---- Render status is the roster's arm --------------------------------

(ert-deftest agent-repl-test-ws-render-status-answers-a-merge-failed-arm ()
  "The status renderers read is the roster row's status arm, verbatim."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "alpha" :project-dir "/w/1")
    (cl-letf (((symbol-function 'agent-repl-status-tab-state)
               (lambda (_ws) :merge-failed)))
      ;; Act / Assert
      (should (eq (agent-repl--ws-render-status "alpha") :merge-failed)))))

(ert-deftest agent-repl-test-ws-render-status-is-nil-before-the-first-push ()
  "A workspace the roster has not spoken about draws no colour."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "alpha" :project-dir "/w/1")
    (cl-letf (((symbol-function 'agent-repl-status-tab-state) (lambda (_ws) nil)))
      ;; Act / Assert
      (should (null (agent-repl--ws-render-status "alpha"))))))

(ert-deftest agent-repl-test-ws-render-status-is-nil-for-a-tombstone ()
  "A workspace closed locally has no state to draw."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "alpha" :project-dir "/w/1")
    (agent-repl--ws-put "alpha" :killed-at (current-time))
    (cl-letf (((symbol-function 'agent-repl-status-tab-state) (lambda (_ws) :ready)))
      ;; Act / Assert
      (should (null (agent-repl--ws-render-status "alpha"))))))

(ert-deftest agent-repl-test-ws-render-status-refuses-an-unknown-workspace ()
  "An unregistered name is a caller bug, not a default state."
  ;; Arrange
  (agent-repl-test--with-clean-state
    ;; Act / Assert
    (should-error (agent-repl--ws-render-status "never-registered")
                  :type 'user-error)))

;;;; ---- The tab bar follows the roster -----------------------------------

(ert-deftest agent-repl-test-ws-tabline-names-follow-the-roster-order ()
  "Tab order is the roster's walk order strictly."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl-roster-tab-order)
               (lambda () '("two" "one")))
              ((symbol-function 'agent-repl--ws-list-names)
               (lambda () '("one" "two"))))
      ;; Act / Assert
      (should (equal (agent-repl--ws-tabline-names) '("two" "one"))))))

(ert-deftest agent-repl-test-ws-tabline-names-drop-a-name-with-no-perspective ()
  "The tab bar can only render tabs that exist."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl-roster-tab-order)
               (lambda () '("one" "ghost")))
              ((symbol-function 'agent-repl--ws-list-names)
               (lambda () '("one"))))
      ;; Act / Assert
      (should (equal (agent-repl--ws-tabline-names) '("one"))))))

(ert-deftest agent-repl-test-ws-tabline-names-fall-back-before-the-first-push ()
  "Before the roster speaks, the workspaces Emacs knows are drawn as they are."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl-roster-tab-order) (lambda () nil))
              ((symbol-function 'agent-repl--ws-list-names)
               (lambda () '("one" "two"))))
      ;; Act / Assert
      (should (equal (agent-repl--ws-tabline-names) '("one" "two"))))))

;;;; ---- The merge teardown guard -----------------------------------------

(ert-deftest agent-repl-test-ws-merge-unfinished-p-is-true-while-merging ()
  "A merge with no verdict yet is unfinished."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl-status-tab-state) (lambda (_ws) :merging)))
      ;; Act / Assert
      (should (agent-repl--ws-merge-unfinished-p "alpha")))))

(ert-deftest agent-repl-test-ws-merge-unfinished-p-is-false-once-merged ()
  "A settled merge is finished and the workspace is free to tear down."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl-status-tab-state) (lambda (_ws) :merged)))
      ;; Act / Assert
      (should-not (agent-repl--ws-merge-unfinished-p "alpha")))))

(ert-deftest agent-repl-test-ws-teardown-guard-refuses-a-queued-merge ()
  "Tearing down a workspace whose merge is queued loses the merge."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl-status-tab-state) (lambda (_ws) :merge-queued)))
      ;; Act / Assert
      (should-error (agent-repl--assert-mergeable-teardown "alpha")
                    :type 'user-error))))

(ert-deftest agent-repl-test-ws-teardown-guard-refuses-a-running-merge ()
  "Tearing a merging workspace down kills the session the merge is driving."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl-status-tab-state) (lambda (_ws) :merging)))
      ;; Act / Assert
      (should-error (agent-repl--assert-mergeable-teardown "alpha")
                    :type 'user-error))))

(ert-deftest agent-repl-test-ws-teardown-guard-allows-a-settled-workspace ()
  "A workspace with no merge in flight tears down normally."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl-status-tab-state) (lambda (_ws) :ready)))
      ;; Act / Assert
      (should (null (agent-repl--assert-mergeable-teardown "alpha"))))))

(provide 'test-workspace)
;;; test-workspace.el ends here

;;;; ---- Tests: the one teardown order -- land first, then kill ----

(defvar agent-repl-test-ws--events nil
  "Boundary calls a teardown test observed, most recent first.")

(defvar agent-repl-test-ws--current nil
  "The perspective a teardown test's fake frame is standing on.")

(defmacro agent-repl-test-ws--with-teardown (current names &rest body)
  "Run BODY on a faked persp-mode: standing on CURRENT with NAMES live.
NAMES serves as both the agent-repl workspace list and the full persp
list.  `agent-repl--ws-switch' and `agent-repl--ws-persp-kill' are mocked
and recorded, in call order, into `agent-repl-test-ws--events'; the kill
records which perspective was current when it ran."
  (declare (indent 2))
  (let ((names-var (make-symbol "names")))
    `(let ((,names-var ,names)
           (agent-repl-test-ws--events nil)
           (agent-repl-test-ws--current ,current))
       (cl-letf (((symbol-function 'agent-repl--ws-system-available-p) (lambda () t))
                 ((symbol-function 'agent-repl--ws-current-name)
                  (lambda () agent-repl-test-ws--current))
                 ((symbol-function 'agent-repl--ws-all-names) (lambda () ,names-var))
                 ((symbol-function 'agent-repl--ws-list-names) (lambda () ,names-var))
                 ((symbol-function 'agent-repl--ws-persp-exists-p)
                  (lambda (ws) (and (member ws ,names-var) t)))
                 ((symbol-function 'agent-repl--ws-switch)
                  (lambda (ws &rest _)
                    (push (list :switch ws) agent-repl-test-ws--events)
                    (setq agent-repl-test-ws--current ws)))
                 ((symbol-function 'agent-repl--ws-persp-kill)
                  (lambda (ws)
                    (push (list :kill ws :current agent-repl-test-ws--current)
                          agent-repl-test-ws--events)
                    t)))
         ,@body))))

(defun agent-repl-test-ws--recorder ()
  "Return (FN . CELL): FN records formatted log lines into CELL's car.
FN takes the arguments `agent-repl--info' and `agent-repl--warn' take;
the lines are kept most recent first."
  (let ((cell (list nil)))
    (cons (lambda (_ws fmt &rest args) (push (apply #'format fmt args) (car cell)))
          cell)))

;;; --- The landing target rule

(ert-deftest agent-repl-test-teardown-landing-target-prefers-an-agent-repl-workspace ()
  "A surviving agent-repl workspace is chosen over a persp agent-repl does not own."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--ws-list-names) (lambda () '("keeper")))
              ((symbol-function 'agent-repl--ws-all-names) (lambda () '("foreign" "keeper"))))
      ;; Act / Assert
      (should (equal (agent-repl--teardown-landing-target "gone") "keeper")))))

(ert-deftest agent-repl-test-teardown-landing-target-falls-back-to-a-real-persp ()
  "With no agent-repl workspace left, the first non-built-in persp is chosen."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((persp-nil-name "none"))
      (cl-letf (((symbol-function 'agent-repl--ws-main-name) (lambda () "main"))
                ((symbol-function 'agent-repl--ws-list-names) (lambda () nil))
                ((symbol-function 'agent-repl--ws-all-names)
                 (lambda () '("none" "main" "foreign"))))
        ;; Act / Assert
        (should (equal (agent-repl--teardown-landing-target "gone") "foreign"))))))

(ert-deftest agent-repl-test-teardown-landing-target-never-names-the-departing-workspace ()
  "The workspace being torn down is never its own landing."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--ws-list-names) (lambda () '("gone")))
              ((symbol-function 'agent-repl--ws-all-names) (lambda () '("gone"))))
      ;; Act / Assert
      (should-not (agent-repl--teardown-landing-target "gone")))))

;;; --- The landing target is the workspace selected before the departing one

(ert-deftest agent-repl-test-teardown-landing-target-is-the-previously-selected-workspace ()
  "Closing the workspace the user stands on lands on the one selected before
it, not the first tab (owner ruling, 2026-09-30)."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((agent-repl--workspace-history '("gone" "second" "first")))
      (agent-repl-test-ws--with-teardown "gone" '("first" "second" "gone")
        ;; Act / Assert
        (should (equal (agent-repl--teardown-landing-target "gone") "second"))))))

(ert-deftest agent-repl-test-teardown-landing-target-skips-a-previous-workspace-since-closed ()
  "A previously selected workspace that has itself closed is skipped: the next
most recent one still open is the landing."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((agent-repl--workspace-history '("gone" "closed-earlier" "third" "first")))
      (agent-repl-test-ws--with-teardown "gone" '("first" "gone" "third")
        ;; Act / Assert
        (should (equal (agent-repl--teardown-landing-target "gone") "third"))))))

(ert-deftest agent-repl-test-teardown-landing-target-skips-a-built-in-in-the-history ()
  "A built-in perspective the user once stood in is never a history landing."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((persp-nil-name "none")
          (agent-repl--workspace-history '("gone" "main" "second")))
      (cl-letf (((symbol-function 'agent-repl--ws-main-name) (lambda () "main")))
        (agent-repl-test-ws--with-teardown "gone" '("main" "first" "second" "gone")
          ;; Act / Assert
          (should (equal (agent-repl--teardown-landing-target "gone") "second")))))))

(ert-deftest agent-repl-test-teardown-landing-target-falls-back-to-tab-order-on-empty-history ()
  "With an empty history and no durable instant -- no workspace ever
selected -- the first open workspace in tab order is the landing."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((agent-repl--workspace-history nil))
      (agent-repl-test-ws--with-teardown "gone" '("first" "second" "gone")
        ;; Act / Assert
        (should (equal (agent-repl--teardown-landing-target "gone") "first"))))))

(ert-deftest agent-repl-test-teardown-landing-target-falls-back-when-no-history-entry-is-open ()
  "A history naming only the departing workspace and closed ones, with no
durable instant either, is the same expected condition as an empty one:
the first open tab is the landing."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((agent-repl--workspace-history '("gone" "closed-earlier")))
      (agent-repl-test-ws--with-teardown "gone" '("first" "second" "gone")
        ;; Act / Assert
        (should (equal (agent-repl--teardown-landing-target "gone") "first"))))))

(ert-deftest agent-repl-test-teardown-landing-target-records-a-history-decision-at-info ()
  "Which source chose the landing is recorded at INFO: here, the history."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((agent-repl--workspace-history '("gone" "second"))
          (info (agent-repl-test-ws--recorder)))
      (cl-letf (((symbol-function 'agent-repl--info) (car info)))
        (agent-repl-test-ws--with-teardown "gone" '("first" "second" "gone")
          ;; Act
          (agent-repl--teardown-landing-target "gone")
          ;; Assert
          (should (seq-some
                   (lambda (l)
                     (string-match-p
                      "teardown-landing-target: ws=gone target=second source=history" l))
                   (cadr info))))))))

(ert-deftest agent-repl-test-teardown-landing-target-records-a-tab-order-fallback-at-info ()
  "Which source chose the landing is recorded at INFO: here, the tab order."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((agent-repl--workspace-history nil)
          (info (agent-repl-test-ws--recorder)))
      (cl-letf (((symbol-function 'agent-repl--info) (car info)))
        (agent-repl-test-ws--with-teardown "gone" '("first" "gone")
          ;; Act
          (agent-repl--teardown-landing-target "gone")
          ;; Assert
          (should (seq-some
                   (lambda (l)
                     (string-match-p
                      "teardown-landing-target: ws=gone target=first source=tab-order" l))
                   (cadr info))))))))

(ert-deftest agent-repl-test-teardown-landing-target-uses-the-durable-instant-without-history ()
  "With no session history naming a survivor (a fresh Emacs), the landing is
the survivor the roster's durable instant says was selected most recently."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((agent-repl--workspace-history '("gone")))
      (cl-letf (((symbol-function 'agent-repl-roster-last-selected-ms)
                 (lambda (ws) (cdr (assoc ws '(("first" . 100) ("second" . 900)))))))
        (agent-repl-test-ws--with-teardown "gone" '("first" "second" "gone")
          ;; Act / Assert
          (should (equal (agent-repl--teardown-landing-target "gone") "second")))))))

(ert-deftest agent-repl-test-teardown-landing-target-records-a-roster-decision-at-info ()
  "Which source chose the landing is recorded at INFO: here, the roster."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((agent-repl--workspace-history nil)
          (info (agent-repl-test-ws--recorder)))
      (cl-letf (((symbol-function 'agent-repl--info) (car info))
                ((symbol-function 'agent-repl-roster-last-selected-ms)
                 (lambda (ws) (and (equal ws "second") 900))))
        (agent-repl-test-ws--with-teardown "gone" '("first" "second" "gone")
          ;; Act
          (agent-repl--teardown-landing-target "gone")
          ;; Assert
          (should (seq-some
                   (lambda (l)
                     (string-match-p
                      "teardown-landing-target: ws=gone target=second source=roster" l))
                   (cadr info))))))))

(ert-deftest agent-repl-test-teardown-landing-target-records-a-foreign-persp-fallback-at-info ()
  "Which source chose the landing is recorded at INFO: here, a foreign persp."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((agent-repl--workspace-history nil)
          (info (agent-repl-test-ws--recorder)))
      (cl-letf (((symbol-function 'agent-repl--info) (car info))
                ((symbol-function 'agent-repl--ws-list-names) (lambda () nil))
                ((symbol-function 'agent-repl--ws-all-names) (lambda () '("foreign"))))
        ;; Act
        (agent-repl--teardown-landing-target "gone")
        ;; Assert
        (should (seq-some
                 (lambda (l)
                   (string-match-p
                    "teardown-landing-target: ws=gone target=foreign source=foreign-persp" l))
                 (cadr info)))))))

(ert-deftest agent-repl-test-land-before-teardown-lands-on-the-previously-selected-workspace ()
  "Standing on the departing workspace, the user lands where they were before."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((agent-repl--workspace-history '("gone" "second" "first")))
      (agent-repl-test-ws--with-teardown "gone" '("first" "second" "gone")
        ;; Act
        (should (equal (agent-repl--land-before-teardown "gone") "second"))
        ;; Assert
        (should (equal agent-repl-test-ws--events '((:switch "second"))))))))

(ert-deftest agent-repl-test-land-before-teardown-ignores-the-history-when-standing-elsewhere ()
  "Closing a workspace the user is NOT on moves nothing, whatever the history."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((agent-repl--workspace-history '("mine" "other" "first")))
      (agent-repl-test-ws--with-teardown "mine" '("first" "mine" "other")
        ;; Act
        (should-not (agent-repl--land-before-teardown "other"))
        ;; Assert
        (should-not agent-repl-test-ws--events)))))

;;; --- Landing before the kill

(ert-deftest agent-repl-test-land-before-teardown-switches-off-the-departing-workspace ()
  "Standing on the workspace being torn down, the user lands on a survivor."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test-ws--with-teardown "gone" '("gone" "keeper")
      ;; Act
      (should (equal (agent-repl--land-before-teardown "gone") "keeper"))
      ;; Assert
      (should (equal agent-repl-test-ws--events '((:switch "keeper")))))))

(ert-deftest agent-repl-test-land-before-teardown-skips-a-built-in-perspective ()
  "Doom's startup `main' is not a landing: the user goes to a REAL workspace.
`main' is auto-vivified into the registry by a persp hook, so it can lead
`agent-repl--ws-list-names' and be picked ahead of every workspace this
module owns -- and it has no panels, so the frame came up on the
fallback buffer."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((persp-nil-name "none"))
      (cl-letf (((symbol-function 'agent-repl--ws-main-name) (lambda () "main")))
        (agent-repl-test-ws--with-teardown "gone" '("main" "gone" "keeper")
          ;; Act
          (agent-repl--land-before-teardown "gone")
          ;; Assert
          (should (equal agent-repl-test-ws--events '((:switch "keeper")))))))))

(ert-deftest agent-repl-test-land-before-teardown-warns-when-only-built-ins-survive ()
  "Built-in perspectives alone are NO survivor: there is nowhere to land, and
that is reported rather than taken for a landing."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((persp-nil-name "none")
          (warn (agent-repl-test-ws--recorder)))
      (cl-letf (((symbol-function 'agent-repl--ws-main-name) (lambda () "main"))
                ((symbol-function 'agent-repl--warn) (car warn)))
        (agent-repl-test-ws--with-teardown "gone" '("none" "main" "gone")
          ;; Act
          (should-not (agent-repl--land-before-teardown "gone"))
          ;; Assert
          (should-not agent-repl-test-ws--events)
          (should (seq-some (lambda (text)
                              (string-search "NO surviving workspace to land in" text))
                            (cadr warn))))))))

(ert-deftest agent-repl-test-land-before-teardown-does-not-count-a-built-in-as-standing-somewhere ()
  "Standing in `main' is standing in no workspace: the landing is still owed."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((persp-nil-name "none"))
      (cl-letf (((symbol-function 'agent-repl--ws-main-name) (lambda () "main")))
        (agent-repl-test-ws--with-teardown "main" '("main" "gone" "keeper")
          ;; Act
          (agent-repl--land-before-teardown "gone")
          ;; Assert
          (should (equal agent-repl-test-ws--events '((:switch "keeper")))))))))

(ert-deftest agent-repl-test-land-before-teardown-arms-nothing-on-the-landing ()
  "The landing workspace is reached by the plain user switch and is NOT armed.
Arrival re-shows panels by default; arming would force them open over an
explicit close, and tearing one workspace down must change nothing about
another."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test-ws--with-teardown "gone" '("gone" "keeper")
      ;; Act
      (agent-repl--land-before-teardown "gone")
      ;; Assert
      (should-not (agent-repl--ws-get "keeper" :pending-show-panels)))))

(ert-deftest agent-repl-test-land-before-teardown-leaves-a-live-persp-alone ()
  "A teardown of some OTHER workspace does not move the user off theirs."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test-ws--with-teardown "mine" '("mine" "other")
      ;; Act
      (should-not (agent-repl--land-before-teardown "other"))
      ;; Assert
      (should-not agent-repl-test-ws--events))))

(ert-deftest agent-repl-test-land-before-teardown-switches-off-a-vanished-persp ()
  "A current perspective no longer in the tab bar is landed off as well."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test-ws--with-teardown "stale" '("gone" "keeper")
      ;; Act
      (agent-repl--land-before-teardown "gone")
      ;; Assert
      (should (equal agent-repl-test-ws--events '((:switch "keeper")))))))

(ert-deftest agent-repl-test-land-before-teardown-warns-when-nothing-survives ()
  "With no surviving workspace the absence of a landing is surfaced, not hidden."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((warn (agent-repl-test-ws--recorder)))
      (cl-letf (((symbol-function 'agent-repl--warn) (car warn)))
        (agent-repl-test-ws--with-teardown "gone" '("gone")
          ;; Act
          (should-not (agent-repl--land-before-teardown "gone"))
          ;; Assert
          (should (seq-some (lambda (l) (string-match-p "NO surviving workspace" l))
                            (cadr warn))))))))

(ert-deftest agent-repl-test-land-before-teardown-signals-a-failed-switch ()
  "A switch that signals propagates: a landing that did not happen must not
read as one that did, and the caller must not go on to kill the workspace
the user still stands on."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test-ws--with-teardown "gone" '("gone" "keeper")
      (cl-letf (((symbol-function 'agent-repl--ws-switch)
                 (lambda (&rest _) (error "persp gone"))))
        ;; Act / Assert
        (should-error (agent-repl--land-before-teardown "gone"))))))

(ert-deftest agent-repl-test-land-before-teardown-is-a-noop-without-persp-mode ()
  "With no workspace system there is no perspective to land in."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test-ws--with-teardown "gone" '("gone" "keeper")
      (cl-letf (((symbol-function 'agent-repl--ws-system-available-p) (lambda () nil)))
        ;; Act
        (should-not (agent-repl--land-before-teardown "gone"))
        ;; Assert
        (should-not agent-repl-test-ws--events)))))

(ert-deftest agent-repl-test-land-before-teardown-records-the-target-at-info ()
  "Where a teardown lands the user is recorded at INFO, visible by default."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((info (agent-repl-test-ws--recorder)))
      (cl-letf (((symbol-function 'agent-repl--info) (car info)))
        (agent-repl-test-ws--with-teardown "gone" '("gone" "keeper")
          ;; Act
          (agent-repl--land-before-teardown "gone")
          ;; Assert
          (should (seq-some
                   (lambda (l)
                     (string-match-p
                      "elisp\\.workspace\\.teardown-landing: ws=gone decision=land current=gone target=keeper"
                      l))
                   (cadr info))))))))

(ert-deftest agent-repl-test-land-before-teardown-records-a-landing-not-owed-at-info ()
  "A teardown that owes no landing says so at INFO rather than staying silent."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((info (agent-repl-test-ws--recorder)))
      (cl-letf (((symbol-function 'agent-repl--info) (car info)))
        (agent-repl-test-ws--with-teardown "mine" '("mine" "other")
          ;; Act
          (agent-repl--land-before-teardown "other")
          ;; Assert
          (should (seq-some
                   (lambda (l)
                     (string-match-p "teardown-landing: ws=other decision=not-owed current=mine" l))
                   (cadr info))))))))

;;; --- Refusals

(ert-deftest agent-repl-test-ws-persp-kill-refusal-allows-an-ordinary-workspace ()
  "A perspective neither protected nor shown in another frame may be killed."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace--protected-p) (lambda (_ws) nil))
              ((symbol-function 'agent-repl--ws-resolve-persp) (lambda (_ws) 'persp))
              ((symbol-function 'persp-frames-with-persp)
               (lambda (&optional _persp) (list (selected-frame)))))
      ;; Act / Assert
      (should-not (agent-repl--ws-persp-kill-refusal "gone")))))

;;; --- Land, then kill

(ert-deftest agent-repl-test-land-then-kill-lands-before-it-kills-the-current-workspace ()
  "Tearing down the CURRENT workspace switches to the landing, THEN kills."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test-ws--with-teardown "gone" '("gone" "keeper")
      ;; Act
      (agent-repl--ws-land-then-kill "gone")
      ;; Assert
      (should (equal (mapcar #'car (reverse agent-repl-test-ws--events))
                     '(:switch :kill))))))

(ert-deftest agent-repl-test-land-then-kill-kills-a-workspace-that-is-no-longer-current ()
  "By the time the kill runs the landing workspace is current, so persp-mode
never drops the frame into its nil perspective and Doom never lays a
fallback buffer into the landing workspace's window."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test-ws--with-teardown "gone" '("gone" "keeper")
      ;; Act
      (agent-repl--ws-land-then-kill "gone")
      ;; Assert
      (should (equal (car agent-repl-test-ws--events)
                     '(:kill "gone" :current "keeper"))))))

(ert-deftest agent-repl-test-land-then-kill-displays-no-fallback-buffer ()
  "No fallback buffer is ever put on screen by a teardown."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let (fallback-asked switched-to)
      (cl-letf (((symbol-function 'doom-fallback-buffer)
                 (lambda () (setq fallback-asked t) (get-buffer-create " *test-fallback*")))
                ((symbol-function 'switch-to-buffer)
                 (lambda (buf &rest _) (setq switched-to buf))))
        (agent-repl-test-ws--with-teardown "gone" '("gone" "keeper")
          ;; Act
          (agent-repl--ws-land-then-kill "gone")
          ;; Assert
          (should-not fallback-asked)
          (should-not switched-to))))))

(ert-deftest agent-repl-test-land-then-kill-never-touches-the-landing-panel-window ()
  "The landing workspace's panel window survives the real persp kill, still
showing its panel, even when persp-mode drifted that panel into the dying
persp; the dying workspace's own window is the one retired."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((real-persp-kill (symbol-function 'agent-repl--ws-persp-kill))
          (keeper-panel (generate-new-buffer "*agent-panel-input-keeper*"))
          (gone-file (generate-new-buffer "gone-file")))
      (unwind-protect
          (save-window-excursion
            (delete-other-windows)
            (with-current-buffer keeper-panel
              (setq-local agent-repl--owning-workspace "keeper"))
            (let ((keeper-win (selected-window))
                  (gone-win (split-window)))
              (set-window-buffer keeper-win keeper-panel)
              (set-window-buffer gone-win gone-file)
              (agent-repl-test-ws--with-teardown "gone" '("gone" "keeper")
                (cl-letf (((symbol-function 'agent-repl--ws-persp-kill) real-persp-kill)
                          ((symbol-function 'persp-kill) (lambda (_ws) t))
                          ((symbol-function 'agent-repl--ws-resolve-persp)
                           (lambda (_ws) 'persp))
                          ((symbol-function 'agent-repl--ws-buffers)
                           (lambda (_persp) (list keeper-panel gone-file)))
                          ((symbol-function 'agent-repl--ws-repaint-sidebar) #'ignore))
                  ;; Act
                  (agent-repl--ws-land-then-kill "gone")))
              ;; Assert
              (should (eq (window-buffer keeper-win) keeper-panel))))
        (kill-buffer keeper-panel)
        (kill-buffer gone-file)))))

(ert-deftest agent-repl-test-land-then-kill-does-not-switch-for-a-non-current-workspace ()
  "Tearing down a workspace the user is NOT standing on switches nothing."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test-ws--with-teardown "mine" '("mine" "other")
      ;; Act
      (agent-repl--ws-land-then-kill "other")
      ;; Assert
      (should (equal agent-repl-test-ws--events '((:kill "other" :current "mine")))))))

(ert-deftest agent-repl-test-land-then-kill-skips-a-nonexistent-workspace ()
  "A workspace already gone -- the merge flow's second close -- is not killed,
and the skip is recorded at INFO."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((info (agent-repl-test-ws--recorder)))
      (cl-letf (((symbol-function 'agent-repl--info) (car info)))
        (agent-repl-test-ws--with-teardown "mine" '("mine")
          ;; Act
          (should-not (agent-repl--ws-land-then-kill "gone"))
          ;; Assert
          (should-not agent-repl-test-ws--events)
          (should (seq-some
                   (lambda (l)
                     (string-match-p
                      "teardown-kill: ws=gone decision=skip reason=not-a-workspace" l))
                   (cadr info))))))))

(ert-deftest agent-repl-test-land-then-kill-refuses-a-workspace-visible-in-another-frame ()
  "A perspective shown in another frame is refused -- logged and signalled --
before anything moves."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((info (agent-repl-test-ws--recorder))
          (other-frame 'other-frame))
      (cl-letf (((symbol-function 'agent-repl--info) (car info))
                ((symbol-function 'agent-repl--ws-resolve-persp) (lambda (_ws) 'persp))
                ((symbol-function 'persp-frames-with-persp)
                 (lambda (&optional _persp) (list (selected-frame) other-frame))))
        (agent-repl-test-ws--with-teardown "gone" '("gone" "keeper")
          ;; Act
          (should-error (agent-repl--ws-land-then-kill "gone") :type 'user-error)
          ;; Assert
          (should-not agent-repl-test-ws--events)
          (should (seq-some
                   (lambda (l)
                     (string-match-p
                      "teardown-refused: ws=gone reason=it is visible in another frame" l))
                   (cadr info))))))))

(ert-deftest agent-repl-test-land-then-kill-refuses-the-protected-perspective ()
  "persp-mode's protected nil perspective is refused -- logged and signalled."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((info (agent-repl-test-ws--recorder)))
      (cl-letf (((symbol-function 'agent-repl--info) (car info))
                ((symbol-function '+workspace--protected-p)
                 (lambda (ws) (equal ws "none"))))
        (agent-repl-test-ws--with-teardown "keeper" '("none" "keeper")
          ;; Act
          (should-error (agent-repl--ws-land-then-kill "none") :type 'user-error)
          ;; Assert
          (should-not agent-repl-test-ws--events)
          (should (seq-some
                   (lambda (l)
                     (string-match-p
                      "teardown-refused: ws=none reason=it is persp-mode's protected nil perspective"
                      l))
                   (cadr info))))))))

(ert-deftest agent-repl-test-land-then-kill-still-kills-when-nothing-survives ()
  "With nowhere to land, the absence is recorded at WARN and the kill still
runs, leaving the frame as persp-mode arranges it."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((warn (agent-repl-test-ws--recorder)))
      (cl-letf (((symbol-function 'agent-repl--warn) (car warn)))
        (agent-repl-test-ws--with-teardown "gone" '("gone")
          ;; Act
          (agent-repl--ws-land-then-kill "gone")
          ;; Assert
          (should (equal agent-repl-test-ws--events '((:kill "gone" :current "gone"))))
          (should (seq-some (lambda (l) (string-match-p "NO surviving workspace" l))
                            (cadr warn))))))))

(ert-deftest agent-repl-test-land-then-kill-does-not-kill-after-a-failed-landing ()
  "A landing that signals aborts the kill: the user still stands on the
workspace, and killing it now is exactly the hazard the order exists for."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test-ws--with-teardown "gone" '("gone" "keeper")
      (cl-letf (((symbol-function 'agent-repl--ws-switch)
                 (lambda (&rest _) (error "persp gone"))))
        ;; Act
        (should-error (agent-repl--ws-land-then-kill "gone"))
        ;; Assert
        (should-not agent-repl-test-ws--events)))))

(ert-deftest agent-repl-test-land-then-kill-records-the-kill-at-info ()
  "The kill is recorded at INFO, visible by default."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((info (agent-repl-test-ws--recorder)))
      (cl-letf (((symbol-function 'agent-repl--info) (car info)))
        (agent-repl-test-ws--with-teardown "mine" '("mine" "other")
          ;; Act
          (agent-repl--ws-land-then-kill "other")
          ;; Assert
          (should (seq-some
                   (lambda (l)
                     (string-match-p "teardown-kill: ws=other decision=kill current=mine" l))
                   (cadr info))))))))

;;; --- kill-one-workspace goes through the one order

(ert-deftest agent-repl-test-kill-one-workspace-tears-the-persp-down-through-land-then-kill ()
  "Tearing a workspace down ends in the one teardown order."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws" :project-dir "/tmp/ws")
    (let (torn-down)
      (cl-letf (((symbol-function 'agent-repl--state-save) #'ignore)
                ((symbol-function 'agent-repl--kill-workspace-buffers) #'ignore)
                ((symbol-function 'agent-repl--ws-repaint-sidebar) #'ignore)
                ((symbol-function 'agent-repl--ws-land-then-kill)
                 (lambda (ws) (setq torn-down ws))))
        ;; Act
        (agent-repl--kill-one-workspace "ws")
        ;; Assert
        (should (equal torn-down "ws"))))))

(ert-deftest agent-repl-test-kill-one-workspace-of-the-current-lands-on-the-previously-selected ()
  "An EXPLICIT close, kill or nuke tears the tab down through
`agent-repl--kill-one-workspace', and closing the workspace the user stands
on lands them on the one selected before it, not the first tab."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws" :project-dir "/tmp/ws")
    (let ((agent-repl--workspace-history '("ws" "second" "first")))
      (agent-repl-test-ws--with-teardown "ws" '("first" "second" "ws")
        (cl-letf (((symbol-function 'agent-repl--state-save) #'ignore)
                  ((symbol-function 'agent-repl--kill-workspace-buffers) #'ignore)
                  ((symbol-function 'agent-repl--ws-repaint-sidebar) #'ignore)
                  ((symbol-function 'agent-repl--ws-persp-kill-refusal) #'ignore))
          ;; Act
          (agent-repl--kill-one-workspace "ws")
          ;; Assert
          (should (equal (car (last agent-repl-test-ws--events)) '(:switch "second"))))))))

(ert-deftest agent-repl-test-kill-one-workspace-declares-the-departure ()
  "A teardown under way is what explains its own workspace's missing sink."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((agent-repl--departed-log-workspaces (make-hash-table :test #'equal)))
      (agent-repl--ws-put "ws" :project-dir "/tmp/ws")
      (cl-letf (((symbol-function 'agent-repl--state-save) #'ignore)
                ((symbol-function 'agent-repl--kill-workspace-buffers) #'ignore)
                ((symbol-function 'agent-repl--ws-repaint-sidebar) #'ignore)
                ((symbol-function 'agent-repl--ws-land-then-kill) #'ignore))
        ;; Act
        (agent-repl--kill-one-workspace "ws")
        ;; Assert
        (should (agent-repl--log-workspace-departing-p "ws"))))))

(ert-deftest agent-repl-test-a-refused-teardown-declares-no-departure ()
  "A teardown refused before it began leaves the workspace standing."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((agent-repl--departed-log-workspaces (make-hash-table :test #'equal)))
      (agent-repl--ws-put "ws" :project-dir "/tmp/ws")
      (cl-letf (((symbol-function 'agent-repl--assert-mergeable-teardown)
                 (lambda (_ws) (error "merge not finished"))))
        ;; Act
        (should-error (agent-repl--kill-one-workspace "ws"))
        ;; Assert
        (should-not (agent-repl--log-workspace-departing-p "ws"))))))

(ert-deftest agent-repl-test-kill-one-workspace-refuses-an-unkillable-persp-before-teardown ()
  "A perspective the kill would refuse is refused BEFORE any teardown step,
so no half-torn-down workspace is left behind its surviving tab."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws" :project-dir "/tmp/ws")
    (let (saved)
      (cl-letf (((symbol-function 'agent-repl--ws-persp-kill-refusal)
                 (lambda (_ws) "it is visible in another frame"))
                ((symbol-function 'agent-repl--state-save)
                 (lambda (_ws) (setq saved t))))
        ;; Act
        (should-error (agent-repl--kill-one-workspace "ws") :type 'user-error)
        ;; Assert
        (should-not saved)))))

(ert-deftest agent-repl-test-kill-one-workspace-survives-a-failing-land-then-kill ()
  "A land-then-kill that signals is warned about and never aborts the teardown."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws" :project-dir "/tmp/ws")
    (let (warned repainted)
      (cl-letf (((symbol-function 'agent-repl--state-save) #'ignore)
                ((symbol-function 'agent-repl--kill-workspace-buffers) #'ignore)
                ((symbol-function 'agent-repl--ws-repaint-sidebar)
                 (lambda (&rest _) (setq repainted t)))
                ((symbol-function 'agent-repl--ws-land-then-kill)
                 (lambda (_ws) (error "no frame")))
                ((symbol-function 'agent-repl--warn)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) warned))))
        ;; Act
        (agent-repl--kill-one-workspace "ws")
        ;; Assert — the failure is surfaced and the teardown still finishes.
        (should (seq-find (lambda (l) (string-match-p "land-then-kill error" l))
                          warned))
        (should repainted)))))

;;; --- No agent-repl source calls Doom's kill

(defconst agent-repl-test-ws--lisp-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "The `lisp/' directory this suite lives in, captured at LOAD time.")

(ert-deftest agent-repl-test-no-source-calls-doom-workspace-kill ()
  "No agent-repl source under `lisp/' calls or references Doom's `+workspace/kill'
as code.  Its current-workspace branch puts `doom-fallback-buffer' into
the landing workspace's panel window; teardown goes through
`agent-repl--ws-land-then-kill' instead.  Mentions in docstrings and
comments, written `+workspace/kill' with a leading backquote, are allowed."
  ;; Arrange
  (let ((offenders nil))
    (dolist (file (directory-files agent-repl-test-ws--lisp-dir t "\\`[^.].*\\.el\\'"))
      (unless (string-prefix-p "test-" (file-name-nondirectory file))
        (with-temp-buffer
          (insert-file-contents file)
          ;; Act
          (goto-char (point-min))
          (while (re-search-forward "\\(?:(\\|'\\|declare-function \\)\\+workspace/kill\\_>" nil t)
            (push (format "%s:%d" (file-name-nondirectory file)
                          (line-number-at-pos (match-beginning 0)))
                  offenders)))))
    ;; Assert
    (should-not offenders)))

;;;; ---- Tests: a persp built-in may never claim a workspace directory ----

(ert-deftest agent-repl-test-ws-put-refuses-project-dir-on-pseudo-perspective ()
  "\"main\" and \"none\" are persp-mode's own; the write never commits.
Measured 2026-08-11: the live registry held `main' -> a real worktree, which
made the perspective log-routable and gave it that workspace's durable sink."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((+workspaces-main "main"))
      (cl-letf (((symbol-function 'agent-repl--do-log) #'ignore))
        ;; Act
        (agent-repl--ws-put "main" :project-dir "/tmp/some-real-worktree")
        ;; Assert
        (should-not (agent-repl--ws-get "main" :project-dir))))))

(ert-deftest agent-repl-test-ws-put-refusal-is-announced ()
  "The refusal is loud: a producer bug must not be silently absorbed."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((+workspaces-main "main")
          (log-calls nil))
      (cl-letf (((symbol-function 'agent-repl--do-log)
                 (lambda (ws fmt args &optional _err)
                   (push (list ws fmt args) log-calls))))
        ;; Act
        (agent-repl--ws-put "main" :project-dir "/tmp/some-real-worktree"))
      ;; Assert
      (should (= 1 (length log-calls)))
      (should (string-match-p "REFUSED :project-dir" (nth 1 (car log-calls)))))))

(ert-deftest agent-repl-test-ws-put-allows-non-project-dir-keys-on-pseudo ()
  "Only `:project-dir' is refused; a pseudo entry is otherwise untouched."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((+workspaces-main "main"))
      (cl-letf (((symbol-function 'agent-repl--do-log) #'ignore))
        ;; Act
        (agent-repl--ws-put "main" :priority "p1")
        ;; Assert
        (should (equal "p1" (agent-repl--ws-get "main" :priority)))))))

(ert-deftest agent-repl-test-ws-put-still-writes-project-dir-for-a-real-workspace ()
  "The refusal is scoped to the pseudo names and nothing else."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((+workspaces-main "main"))
      ;; Act
      (agent-repl--ws-put "real-ws" :project-dir "/tmp/real-ws")
      ;; Assert
      (should (equal "/tmp/real-ws" (agent-repl--ws-get "real-ws" :project-dir))))))

(ert-deftest agent-repl-test-ws-forget-log-target-tolerates-a-deleted-worktree ()
  "A workspace whose worktree is gone must not make the teardown signal.
`agent-repl--ws-del' runs this, and a diagnostic sink may never abort a
teardown — so the sweep matches on the registered directory rather than
resolving an identity that would raise."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((agent-repl--workspace-log-targets (make-hash-table :test #'equal)))
      (agent-repl--ws-put "gone-ws" :project-dir "/no/such/worktree/at/all")
      ;; Act / Assert
      (cl-letf (((symbol-function 'agent-repl--log) (lambda (&rest _) nil)))
        (should-not (agent-repl--ws-forget-emacs-log-target "gone-ws" "probe"))))))

(ert-deftest agent-repl-test-ws-forget-log-target-drops-every-name-for-the-dir ()
  "One directory's target is forgotten once, however many names reached it."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let* ((project (make-temp-file "agent-repl-forget-shared-" t))
           (agent-repl--workspace-log-targets (make-hash-table :test #'equal)))
      (unwind-protect
          (progn
            (agent-repl--ws-put "shared-a" :project-dir project)
            (puthash "shared-b" (gethash "shared-a" agent-repl--workspaces)
                     agent-repl--workspaces)
            (agent-repl--workspace-emacs-log-target "shared-a")
            ;; Act
            (cl-letf (((symbol-function 'agent-repl--log) (lambda (&rest _) nil)))
              (agent-repl--ws-forget-emacs-log-target "shared-b" "probe"))
            ;; Assert — forgetting through EITHER name clears the one entry.
            (should (= 0 (hash-table-count agent-repl--workspace-log-targets))))
        (delete-directory project t)))))

;;;; ---- R-LOGFORGET: the teardown forgets its sink LAST ----
;;
;; `agent-repl--ws-del' used to forget WS's owned log target FIRST, then go
;; on logging the teardown it was in the middle of: the `ws-del-hook'
;; records, every runtime-key clear, and the closing `ws-del:' line.  The
;; first of those minted a FRESH durable target and re-pointed the canonical
;; `<ws>/.claude/emacs/emacs.log' symlink at it, so the path every reader
;; follows no longer reached one line of that workspace's history.  The
;; forget is now the last act of the teardown.

(ert-deftest agent-repl-test-ws-del-leaves-the-canonical-link-on-the-old-target ()
  "After a teardown the canonical link still names the pre-teardown target."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let* ((project (make-temp-file "agent-repl-del-link-" t))
           (agent-repl--workspace-log-targets (make-hash-table :test #'equal))
           (agent-repl-log-to-file t))
      (unwind-protect
          (progn
            (agent-repl--ws-put "del-link-ws" :project-dir project)
            (let ((target (agent-repl--workspace-emacs-log-target "del-link-ws")))
              ;; Act
              (agent-repl--ws-del "del-link-ws")
              ;; Assert
              (should (equal target
                             (file-symlink-p
                              (agent-repl--workspace-emacs-log-path project))))))
        (delete-directory project t)))))

(ert-deftest agent-repl-test-ws-del-writes-its-own-record-into-the-old-target ()
  "The teardown's own closing record lands in the pre-teardown target."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let* ((project (make-temp-file "agent-repl-del-record-" t))
           (agent-repl--workspace-log-targets (make-hash-table :test #'equal))
           (agent-repl-log-to-file t))
      (unwind-protect
          (progn
            (agent-repl--ws-put "del-record-ws" :project-dir project)
            (let ((target (agent-repl--workspace-emacs-log-target "del-record-ws")))
              ;; Act
              (agent-repl--ws-del "del-record-ws")
              ;; Assert
              (should (string-match-p
                       "ws-del: ws=del-record-ws"
                       (with-temp-buffer
                         (insert-file-contents target)
                         (buffer-string))))))
        (delete-directory project t)))))

(ert-deftest agent-repl-test-a-record-after-ws-del-rejoins-the-standing-target ()
  "Ownership is released and re-resolved, and re-resolving REJOINS the file.
The assertion inverted on 2026-09-11 because the contract it tests did.
This test used to require a fresh target after a teardown; the
standing-target rule says a runtime APPENDS to the target the canonical
link names, precisely so a workspace\='s history is not split into a new
file every time ownership is re-resolved.  What the teardown must still do
-- drop the registry entry so the sink is resolved again rather than served
from memory -- is what this now pins, plus the rejoin that follows it."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let* ((project (make-temp-file "agent-repl-del-fresh-" t))
           (agent-repl--workspace-log-targets (make-hash-table :test #'equal))
           (agent-repl-log-to-file t))
      (unwind-protect
          (progn
            (agent-repl--ws-put "del-fresh-ws" :project-dir project)
            (let ((target (agent-repl--workspace-emacs-log-target "del-fresh-ws")))
              (agent-repl--ws-del "del-fresh-ws")
              (should-not (agent-repl--workspace-log-target-entry "del-fresh-ws"))
              (agent-repl--ws-put "del-fresh-ws" :project-dir project)
              ;; Act
              (let ((next (agent-repl--workspace-emacs-log-target "del-fresh-ws")))
                ;; Assert
                (should (equal target next)))))
        (delete-directory project t)))))

;;;; ---- agent-repl--call-in-background-workspace ----

(ert-deftest agent-repl-test-call-in-background-switches-in-before-fn-runs ()
  "FN runs with the target workspace activated, not the caller's."
  (agent-repl-test--with-clean-state
    ;; Arrange
    (let ((current "caller-ws")
          (seen nil))
      (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () current))
                ((symbol-function 'agent-repl--ws-switch)
                 (lambda (ws &rest _) (setq current ws)))
                ((symbol-function 'agent-repl--clean-frame-foreign-windows) #'ignore))
        ;; Act
        (agent-repl--call-in-background-workspace
         "target-ws" (lambda () (setq seen current)))
        ;; Assert
        (should (equal seen "target-ws"))))))

(ert-deftest agent-repl-test-call-in-background-restores-the-callers-workspace ()
  "The caller's workspace is selected again once FN returns."
  (agent-repl-test--with-clean-state
    ;; Arrange
    (let ((current "caller-ws"))
      (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () current))
                ((symbol-function 'agent-repl--ws-switch)
                 (lambda (ws &rest _) (setq current ws)))
                ((symbol-function 'agent-repl--clean-frame-foreign-windows) #'ignore))
        ;; Act
        (agent-repl--call-in-background-workspace "target-ws" #'ignore)
        ;; Assert
        (should (equal current "caller-ws"))))))

(ert-deftest agent-repl-test-call-in-background-restores-focus-when-fn-signals ()
  "A signalling FN still leaves the caller's workspace selected."
  (agent-repl-test--with-clean-state
    ;; Arrange
    (let ((current "caller-ws"))
      (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () current))
                ((symbol-function 'agent-repl--ws-switch)
                 (lambda (ws &rest _) (setq current ws)))
                ((symbol-function 'agent-repl--clean-frame-foreign-windows) #'ignore))
        ;; Act
        (should-error (agent-repl--call-in-background-workspace
                       "target-ws" (lambda () (error "boom"))))
        ;; Assert
        (should (equal current "caller-ws"))))))

(ert-deftest agent-repl-test-call-in-background-skips-the-switch-when-already-current ()
  "No perspective traffic at all when the target is already the current workspace."
  (agent-repl-test--with-clean-state
    ;; Arrange
    (let ((switches 0))
      (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "target-ws"))
                ((symbol-function 'agent-repl--ws-switch)
                 (lambda (&rest _) (cl-incf switches)))
                ((symbol-function 'agent-repl--clean-frame-foreign-windows) #'ignore))
        ;; Act
        (agent-repl--call-in-background-workspace "target-ws" #'ignore)
        ;; Assert
        (should (zerop switches))))))

(ert-deftest agent-repl-test-call-in-background-cleans-foreign-windows-before-fn ()
  "The frame is cleared of other workspaces' windows before FN builds into it."
  (agent-repl-test--with-clean-state
    ;; Arrange
    (let ((order nil))
      (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "target-ws"))
                ((symbol-function 'agent-repl--ws-switch) #'ignore)
                ((symbol-function 'agent-repl--clean-frame-foreign-windows)
                 (lambda (_ws) (push 'clean order))))
        ;; Act
        (agent-repl--call-in-background-workspace
         "target-ws" (lambda () (push 'fn order)))
        ;; Assert
        (should (equal (nreverse order) '(clean fn)))))))

(ert-deftest agent-repl-test-call-in-background-binds-the-eager-open-flag ()
  "The activation-reactive hooks are suppressed for the duration of FN."
  (agent-repl-test--with-clean-state
    ;; Arrange
    (let ((seen nil))
      (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "target-ws"))
                ((symbol-function 'agent-repl--ws-switch) #'ignore)
                ((symbol-function 'agent-repl--clean-frame-foreign-windows) #'ignore))
        ;; Act
        (agent-repl--call-in-background-workspace
         "target-ws" (lambda () (setq seen agent-repl--eager-open-in-progress)))
        ;; Assert
        (should seen)
        (should-not agent-repl--eager-open-in-progress)))))

(ert-deftest agent-repl-test-restore-focus-reselects-the-original-window ()
  "A live original window is selected again even when the body moved away."
  (agent-repl-test--with-clean-state
    ;; Arrange
    (let ((conf (current-window-configuration)))
      (unwind-protect
          (let (orig other)
            (delete-other-windows)
            (setq orig (selected-window))
            (setq other (split-window orig))
            (select-window other)
            ;; Act
            (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () nil)))
              (agent-repl--restore-focus nil orig (window-buffer orig)))
            ;; Assert
            (should (eq (selected-window) orig)))
        (set-window-configuration conf)))))

(ert-deftest agent-repl-test-restore-focus-survives-a-failing-switch-back ()
  "A switch-back that signals is recorded, not re-signaled."
  (agent-repl-test--with-clean-state
    ;; Arrange
    (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "elsewhere"))
              ((symbol-function 'agent-repl--ws-switch)
               (lambda (&rest _) (error "persp gone"))))
      ;; Act / Assert
      (should-not (agent-repl--restore-focus "caller-ws" nil nil)))))

;;;; ---- Tests: --pseudo-perspective-killable-p ----

(ert-deftest agent-repl-test-pseudo-killable-main-is-killable ()
  "Doom's initial \"main\" is a normal perspective and IS killable."
  (agent-repl-test--with-clean-state
    (let ((persp-nil-name "none")
          (+workspaces-main "main"))
      (should (agent-repl--pseudo-perspective-killable-p "main")))))

(ert-deftest agent-repl-test-pseudo-killable-none-is-not-killable ()
  "persp-mode's nil perspective \"none\" is NOT killable."
  (agent-repl-test--with-clean-state
    (let ((persp-nil-name "none")
          (+workspaces-main "main"))
      (should-not (agent-repl--pseudo-perspective-killable-p "none")))))

(ert-deftest agent-repl-test-pseudo-killable-real-workspace-is-not-a-pseudo ()
  "A real workspace name is not a pseudo, so it is not a kill candidate here."
  (agent-repl-test--with-clean-state
    (let ((persp-nil-name "none")
          (+workspaces-main "main"))
      (should-not (agent-repl--pseudo-perspective-killable-p "doom")))))

;;;; ---- Tests: --delete-pseudo-perspectives ----

(ert-deftest agent-repl-test-delete-pseudos-kills-main-once-a-real-exists ()
  "With a real workspace present, the killable pseudo \"main\" is killed."
  (agent-repl-test--with-clean-state
    ;; Arrange
    (let ((persp-mode t)
          (persp-nil-name "none")
          (+workspaces-main "main")
          (persp-names-cache '("none" "main" "doom"))
          (agent-repl--pseudo-perspectives-deleted nil)
          (killed nil))
      (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "doom"))
                ((symbol-function 'agent-repl--ws-switch)
                 (lambda (&rest _) (error "should not switch: already in a real ws")))
                ((symbol-function 'agent-repl--ws-persp-kill)
                 (lambda (ws) (push ws killed)))
                ((symbol-function 'agent-repl--info) (lambda (&rest _) nil)))
        ;; Act
        (agent-repl--delete-pseudo-perspectives)
        ;; Assert
        (should (equal killed '("main")))))))

(ert-deftest agent-repl-test-delete-pseudos-never-kills-none ()
  "persp-nil \"none\" is skipped, never handed to the kill wrapper."
  (agent-repl-test--with-clean-state
    ;; Arrange
    (let ((persp-mode t)
          (persp-nil-name "none")
          (+workspaces-main "main")
          (persp-names-cache '("none" "main" "doom"))
          (agent-repl--pseudo-perspectives-deleted nil)
          (killed nil))
      (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "doom"))
                ((symbol-function 'agent-repl--ws-switch) (lambda (&rest _) nil))
                ((symbol-function 'agent-repl--ws-persp-kill)
                 (lambda (ws) (push ws killed)))
                ((symbol-function 'agent-repl--info) (lambda (&rest _) nil)))
        ;; Act
        (agent-repl--delete-pseudo-perspectives)
        ;; Assert
        (should-not (member "none" killed))))))

(ert-deftest agent-repl-test-delete-pseudos-noop-when-no-real-workspace ()
  "It does NOT fire before a real workspace perspective exists."
  (agent-repl-test--with-clean-state
    ;; Arrange -- only the pseudos are in the cache
    (let ((persp-mode t)
          (persp-nil-name "none")
          (+workspaces-main "main")
          (persp-names-cache '("none" "main"))
          (agent-repl--pseudo-perspectives-deleted nil)
          (killed nil))
      (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "main"))
                ((symbol-function 'agent-repl--ws-switch) (lambda (&rest _) nil))
                ((symbol-function 'agent-repl--ws-persp-kill)
                 (lambda (ws) (push ws killed)))
                ((symbol-function 'agent-repl--info) (lambda (&rest _) nil)))
        ;; Act
        (agent-repl--delete-pseudo-perspectives)
        ;; Assert -- nothing killed, one-shot not consumed
        (should-not killed)
        (should-not agent-repl--pseudo-perspectives-deleted)))))

(ert-deftest agent-repl-test-delete-pseudos-is-idempotent ()
  "It does not fire twice: a set one-shot flag suppresses the kill."
  (agent-repl-test--with-clean-state
    ;; Arrange -- flag already set from a prior pass
    (let ((persp-mode t)
          (persp-nil-name "none")
          (+workspaces-main "main")
          (persp-names-cache '("none" "main" "doom"))
          (agent-repl--pseudo-perspectives-deleted t)
          (killed nil))
      (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "doom"))
                ((symbol-function 'agent-repl--ws-switch) (lambda (&rest _) nil))
                ((symbol-function 'agent-repl--ws-persp-kill)
                 (lambda (ws) (push ws killed)))
                ((symbol-function 'agent-repl--info) (lambda (&rest _) nil)))
        ;; Act
        (agent-repl--delete-pseudo-perspectives)
        ;; Assert
        (should-not killed)))))

(ert-deftest agent-repl-test-delete-pseudos-sets-the-one-shot-flag ()
  "A successful deletion consumes the one-shot so a later pass is a no-op."
  (agent-repl-test--with-clean-state
    ;; Arrange
    (let ((persp-mode t)
          (persp-nil-name "none")
          (+workspaces-main "main")
          (persp-names-cache '("none" "main" "doom"))
          (agent-repl--pseudo-perspectives-deleted nil))
      (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "doom"))
                ((symbol-function 'agent-repl--ws-switch) (lambda (&rest _) nil))
                ((symbol-function 'agent-repl--ws-persp-kill) (lambda (_ws) nil))
                ((symbol-function 'agent-repl--info) (lambda (&rest _) nil)))
        ;; Act
        (agent-repl--delete-pseudo-perspectives)
        ;; Assert
        (should agent-repl--pseudo-perspectives-deleted)))))

(ert-deftest agent-repl-test-delete-pseudos-vacates-a-pseudo-current-first ()
  "Standing IN a pseudo, the frame is switched to a real workspace first."
  (agent-repl-test--with-clean-state
    ;; Arrange -- current is the pseudo "main"
    (let ((persp-mode t)
          (persp-nil-name "none")
          (+workspaces-main "main")
          (persp-names-cache '("none" "main" "doom"))
          (agent-repl--pseudo-perspectives-deleted nil)
          (events nil))
      (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "main"))
                ((symbol-function 'agent-repl--ws-switch)
                 (lambda (ws &rest _) (push (cons :switch ws) events)))
                ((symbol-function 'agent-repl--ws-persp-kill)
                 (lambda (ws) (push (cons :kill ws) events)))
                ((symbol-function 'agent-repl--info) (lambda (&rest _) nil)))
        ;; Act
        (agent-repl--delete-pseudo-perspectives)
        ;; Assert -- switch to the real "doom" precedes any kill
        (setq events (nreverse events))
        (should (equal (car events) '(:switch . "doom")))
        (should (member '(:kill . "main") events))))))

(ert-deftest agent-repl-test-delete-pseudos-noop-when-persp-mode-off ()
  "With persp-mode unavailable the deletion never touches the kill wrapper."
  (agent-repl-test--with-clean-state
    ;; Arrange
    (let ((persp-mode nil)
          (persp-nil-name "none")
          (+workspaces-main "main")
          (persp-names-cache '("none" "main" "doom"))
          (agent-repl--pseudo-perspectives-deleted nil)
          (killed nil))
      (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "doom"))
                ((symbol-function 'agent-repl--ws-switch) (lambda (&rest _) nil))
                ((symbol-function 'agent-repl--ws-persp-kill)
                 (lambda (ws) (push ws killed)))
                ((symbol-function 'agent-repl--info) (lambda (&rest _) nil)))
        ;; Act
        (agent-repl--delete-pseudo-perspectives)
        ;; Assert
        (should-not killed)))))

;;;; ---- Tests: --delete-pseudos-on-bringup (the seam) ----

(ert-deftest agent-repl-test-delete-pseudos-on-bringup-fires-on-finished ()
  "A finished pass that opened a tab drives the deletion."
  (agent-repl-test--with-clean-state
    (let ((called nil))
      (cl-letf (((symbol-function 'agent-repl--delete-pseudo-perspectives)
                 (lambda () (setq called t))))
        ;; Act -- finished pass, one tab opened
        (agent-repl--delete-pseudos-on-bringup 1 1 t)
        ;; Assert
        (should called)))))

(ert-deftest agent-repl-test-delete-pseudos-on-bringup-skips-unfinished ()
  "A mid-pass (FINISHED nil) call does not drive the deletion."
  (agent-repl-test--with-clean-state
    (let ((called nil))
      (cl-letf (((symbol-function 'agent-repl--delete-pseudo-perspectives)
                 (lambda () (setq called t))))
        ;; Act -- not finished
        (agent-repl--delete-pseudos-on-bringup 1 2 nil)
        ;; Assert
        (should-not called)))))

(ert-deftest agent-repl-test-delete-pseudos-on-bringup-skips-zero-opened ()
  "A finished pass that opened no tab does not drive the deletion."
  (agent-repl-test--with-clean-state
    (let ((called nil))
      (cl-letf (((symbol-function 'agent-repl--delete-pseudo-perspectives)
                 (lambda () (setq called t))))
        ;; Act -- finished but opened=0
        (agent-repl--delete-pseudos-on-bringup 0 0 t)
        ;; Assert
        (should-not called)))))
