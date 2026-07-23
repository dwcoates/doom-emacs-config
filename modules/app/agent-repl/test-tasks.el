;;; test-tasks.el --- ERT tests for tasks.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the user-defined task model (tasks.el): the id-keyed hash,
;; its on-disk persistence, per-task org notes files, and workspace
;; membership (direct + inherited).  Persistence and org files run REAL
;; against an isolated temp state dir (`AGENT_REPL_STATE_DIR'), so no
;; test touches the developer's `~/.claude-emacs'.
;;
;; Run with:
;;   emacs -batch -Q -l ert -l test-tasks.el -f ert-run-tests-batch-and-exit

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

(require 'cl-lib)

;;;; ---- Helpers -----------------------------------------------------------

(defmacro agent-repl-test--with-tasks-state (&rest body)
  "Execute BODY with a fresh task hash and an isolated on-disk state dir.
Binds `agent-repl--tasks' / `agent-repl--tasks-loaded' fresh, points
`AGENT_REPL_STATE_DIR' at a throwaway temp dir so `tasks.el's real disk
writes never reach the user's `~/.claude-emacs', and also rebinds the
workspace hash so membership tests seed live workspaces cleanly."
  (declare (indent 0))
  (let ((dir-sym (make-symbol "dir")))
    `(let* ((,dir-sym (make-temp-file "agent-repl-tasks-" t))
            (process-environment
             (cons (format "AGENT_REPL_STATE_DIR=%s" ,dir-sym) process-environment))
            (agent-repl--tasks (make-hash-table :test 'equal))
            (agent-repl--tasks-loaded nil)
            (agent-repl--workspaces (make-hash-table :test 'equal)))
       (unwind-protect
           (progn ,@body)
         (when (file-directory-p ,dir-sym)
           (delete-directory ,dir-sym t))))))

;;;; ---- Create ------------------------------------------------------------

(ert-deftest agent-repl-test-tasks-create-stores-task ()
  "`--task-create' registers a task retrievable by its returned id."
  (agent-repl-test--with-tasks-state
    ;; Act
    (let ((id (agent-repl--task-create "Ship the view selector")))
      ;; Assert
      (let ((task (agent-repl--task-get id)))
        (should (equal (plist-get task :title) "Ship the view selector"))
        (should (null (plist-get task :done)))
        (should (numberp (plist-get task :created-at)))))))

(ert-deftest agent-repl-test-tasks-create-trims-title ()
  "`--task-create' trims surrounding whitespace from the title."
  (agent-repl-test--with-tasks-state
    (let ((id (agent-repl--task-create "  padded  ")))
      (should (equal (plist-get (agent-repl--task-get id) :title) "padded")))))

(ert-deftest agent-repl-test-tasks-create-empty-title-errors ()
  "`--task-create' signals on a whitespace-only title."
  (agent-repl-test--with-tasks-state
    (should-error (agent-repl--task-create "   "))))

(ert-deftest agent-repl-test-tasks-create-writes-org-file ()
  "`--task-create' seeds the task's org notes file with a TITLE header."
  (agent-repl-test--with-tasks-state
    (let* ((id (agent-repl--task-create "Notes task"))
           (file (agent-repl--task-org-file id)))
      (should (file-exists-p file))
      (with-temp-buffer
        (insert-file-contents file)
        (should (string-match-p "#\\+TITLE: Notes task" (buffer-string)))))))

(ert-deftest agent-repl-test-tasks-create-ids-unique ()
  "Two `--task-create' calls yield distinct ids."
  (agent-repl-test--with-tasks-state
    (let ((a (agent-repl--task-create "one"))
          (b (agent-repl--task-create "two")))
      (should-not (equal a b)))))

;;;; ---- Toggle done -------------------------------------------------------

(ert-deftest agent-repl-test-tasks-toggle-done-flips ()
  "`--task-toggle-done' flips the done flag and returns the new state."
  (agent-repl-test--with-tasks-state
    (let ((id (agent-repl--task-create "toggle me")))
      (should (eq (agent-repl--task-toggle-done id) t))
      (should (plist-get (agent-repl--task-get id) :done))
      (should (eq (agent-repl--task-toggle-done id) nil))
      (should-not (plist-get (agent-repl--task-get id) :done)))))

(ert-deftest agent-repl-test-tasks-toggle-done-unknown-errors ()
  "`--task-toggle-done' signals on an unknown id."
  (agent-repl-test--with-tasks-state
    (should-error (agent-repl--task-toggle-done "nope"))))

;;;; ---- Org file ----------------------------------------------------------

(ert-deftest agent-repl-test-tasks-org-ensure-idempotent ()
  "`--task-org-ensure' leaves an existing file's contents untouched."
  (agent-repl-test--with-tasks-state
    (let* ((id (agent-repl--task-create "keep"))
           (file (agent-repl--task-org-file id)))
      (with-temp-file file (insert "user notes here"))
      (agent-repl--task-org-ensure id "keep")
      (with-temp-buffer
        (insert-file-contents file)
        (should (equal (buffer-string) "user notes here"))))))

(ert-deftest agent-repl-test-tasks-org-file-uses-prefix ()
  "The org notes file name carries the `task-notes-' prefix and id."
  (agent-repl-test--with-tasks-state
    (should (string-match-p "task-notes-abc123\\.org\\'"
                            (agent-repl--task-org-file "abc123")))))

;;;; ---- Opening the org popup ---------------------------------------------

(ert-deftest agent-repl-test-tasks-open-unknown-errors ()
  "`--task-open' signals on an unknown id."
  (agent-repl-test--with-tasks-state
    (should-error (agent-repl--task-open "nope"))))

(ert-deftest agent-repl-test-tasks-open-pops-notes-buffer ()
  "`--task-open' pops to the task's org notes buffer."
  (agent-repl-test--with-tasks-state
    (let ((id (agent-repl--task-create "open me"))
          (popped nil))
      (cl-letf (((symbol-function 'pop-to-buffer)
                 (lambda (buf &rest _) (setq popped buf) buf)))
        (agent-repl--task-open id))
      (should (bufferp popped))
      (should (equal (buffer-file-name popped) (agent-repl--task-org-file id)))
      (kill-buffer popped))))

(ert-deftest agent-repl-test-tasks-open-installs-save-on-kill-hook ()
  "`--task-open' installs a buffer-local save-on-kill hook on the notes buffer."
  (agent-repl-test--with-tasks-state
    (let ((id (agent-repl--task-create "hooked")))
      (cl-letf (((symbol-function 'pop-to-buffer) (lambda (buf &rest _) buf)))
        (let ((buf (agent-repl--task-open id)))
          (with-current-buffer buf
            (should (local-variable-p 'kill-buffer-hook)))
          (kill-buffer buf))))))

;;;; ---- Persistence -------------------------------------------------------

(ert-deftest agent-repl-test-tasks-persist-round-trip ()
  "A created task survives a hash-clear + reload from disk."
  (agent-repl-test--with-tasks-state
    (let ((id (agent-repl--task-create "durable")))
      ;; Act: simulate a fresh session by dropping the in-memory hash.
      (clrhash agent-repl--tasks)
      (setq agent-repl--tasks-loaded nil)
      ;; Assert: the reload hydrates the same task.
      (should (equal (plist-get (agent-repl--task-get id) :title) "durable")))))

(ert-deftest agent-repl-test-tasks-sorted-by-created-at ()
  "`--tasks-sorted' orders tasks oldest-created first."
  (agent-repl-test--with-tasks-state
    (let ((older (list :id "b" :title "B" :done nil :created-at 100.0))
          (newer (list :id "a" :title "A" :done nil :created-at 200.0)))
      (puthash "b" older agent-repl--tasks)
      (puthash "a" newer agent-repl--tasks)
      (setq agent-repl--tasks-loaded t)
      (should (equal (mapcar (lambda (task) (plist-get task :id))
                             (agent-repl--tasks-sorted))
                     '("b" "a"))))))

;;;; ---- Membership --------------------------------------------------------

(ert-deftest agent-repl-test-tasks-effective-id-direct ()
  "A workspace's own `:task-id' is its effective task."
  (agent-repl-test--with-tasks-state
    (let ((id (agent-repl--task-create "direct")))
      (agent-repl--ws-put "ws" :project-dir "/tmp/ws")
      (agent-repl--task-assign-workspace "ws" id)
      (should (equal (agent-repl--ws-effective-task-id "ws") id)))))

(ert-deftest agent-repl-test-tasks-effective-id-inherited ()
  "A child workspace inherits its parent's task via `:source-ws-dir'."
  (agent-repl-test--with-tasks-state
    (let ((id (agent-repl--task-create "family")))
      (agent-repl--ws-put "parent" :project-dir "/tmp/parent")
      (agent-repl--task-assign-workspace "parent" id)
      (agent-repl--ws-put "child" :project-dir "/tmp/child")
      (agent-repl--ws-put "child" :source-ws-dir "/tmp/parent")
      (should (equal (agent-repl--ws-effective-task-id "child") id)))))

(ert-deftest agent-repl-test-tasks-effective-id-none ()
  "A workspace outside every task has no effective task."
  (agent-repl-test--with-tasks-state
    (agent-repl--ws-put "loner" :project-dir "/tmp/loner")
    (should (null (agent-repl--ws-effective-task-id "loner")))))

(ert-deftest agent-repl-test-tasks-effective-id-dangling-ignored ()
  "A `:task-id' naming a deleted task is ignored, not stranded."
  (agent-repl-test--with-tasks-state
    (agent-repl--ws-put "ws" :project-dir "/tmp/ws")
    (agent-repl--ws-put "ws" :task-id "ghost")
    (should (null (agent-repl--ws-effective-task-id "ws")))))

(ert-deftest agent-repl-test-tasks-assign-unknown-errors ()
  "`--task-assign-workspace' signals on an unknown task id."
  (agent-repl-test--with-tasks-state
    (agent-repl--ws-put "ws" :project-dir "/tmp/ws")
    (should-error (agent-repl--task-assign-workspace "ws" "nope"))))

;;;; ---- Signature ---------------------------------------------------------

(ert-deftest agent-repl-test-tasks-signature-tracks-done ()
  "`--tasks-signature' changes when a task's done flag flips."
  (agent-repl-test--with-tasks-state
    (let* ((id (agent-repl--task-create "sig"))
           (before (agent-repl--tasks-signature)))
      (agent-repl--task-toggle-done id)
      (should-not (equal before (agent-repl--tasks-signature))))))

(provide 'test-tasks)
;;; test-tasks.el ends here
