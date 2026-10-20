;;; test-worktree.el --- ERT tests for agent-repl worktree.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;   AGENT_REPL_FORBID_VENDOR_CALLS=1 emacs -batch -Q -l ert \
;;     -l lisp/test-worktree.el -f ert-run-tests-batch-and-exit
;;
;; WHAT IS LEFT TO TEST is what is left of the file: the read-only branch
;; readers, the async-git boundary, and the workspace-directory reverse
;; lookup.  Everything else worktree.el used to hold -- every creation
;; flavor, the headless name generation, the one-shot suffix composition,
;; the merge detection and cherry-pick machinery -- is the daemon's now, so
;; its coverage went with it rather than being adapted to functions that
;; should not exist.
;;
;; Both git wrappers are external boundaries and are stubbed everywhere they
;; are reached; no test shells out.

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                           (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Branch readers ----

(ert-deftest agent-repl-test-worktree-branch-of-dir-answers-the-branch ()
  "The reader answers the abbreviated branch git reports."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'file-directory-p) (lambda (_d) t))
              ((symbol-function 'agent-repl--git-string) (lambda (&rest _) "feature/x")))
      (should (equal (agent-repl--git-branch-of-dir "/tmp/wt") "feature/x")))))

(ert-deftest agent-repl-test-worktree-branch-of-dir-rejects-detached-head ()
  "A detached HEAD is not a branch name, so the reader answers nil."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'file-directory-p) (lambda (_d) t))
              ((symbol-function 'agent-repl--git-string) (lambda (&rest _) "HEAD")))
      (should-not (agent-repl--git-branch-of-dir "/tmp/wt")))))

(ert-deftest agent-repl-test-worktree-branch-of-dir-rejects-a-fatal ()
  "A `fatal' line is git's error text, not a branch."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'file-directory-p) (lambda (_d) t))
              ((symbol-function 'agent-repl--git-string)
               (lambda (&rest _) "fatal: not a git repository")))
      (should-not (agent-repl--git-branch-of-dir "/tmp/wt")))))

(ert-deftest agent-repl-test-worktree-branch-of-dir-rejects-empty-output ()
  "Empty output is no answer."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'file-directory-p) (lambda (_d) t))
              ((symbol-function 'agent-repl--git-string) (lambda (&rest _) "")))
      (should-not (agent-repl--git-branch-of-dir "/tmp/wt")))))

(ert-deftest agent-repl-test-worktree-branch-of-dir-nil-for-a-missing-dir ()
  "A directory that is not there has no branch."
  (agent-repl-test--with-clean-state
    (should-not (agent-repl--git-branch-of-dir "/tmp/definitely-not-here-12345"))))

(ert-deftest agent-repl-test-worktree-workspace-branch-answers-the-branch ()
  "The workspace reader resolves through the workspace's own directory."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/tmp/wt")
    (cl-letf (((symbol-function 'agent-repl--git-string) (lambda (&rest _) "ABC/fix-login")))
      (should (equal (agent-repl--workspace-branch "ws1") "ABC/fix-login")))))

(ert-deftest agent-repl-test-worktree-workspace-branch-answers-the-sha-when-detached ()
  "A detached HEAD answers the SHA: `HEAD' would not answer the question."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/tmp/wt")
    (cl-letf (((symbol-function 'agent-repl--git-string)
               (lambda (&rest args)
                 (if (member "--abbrev-ref" args) "HEAD" "abc1234"))))
      (should (equal (agent-repl--workspace-branch "ws1") "abc1234")))))

(ert-deftest agent-repl-test-worktree-workspace-branch-nil-without-a-dir ()
  "A workspace with no directory has no branch to read."
  (agent-repl-test--with-clean-state
    (should-not (agent-repl--workspace-branch "ws1"))))

;;;; ---- The workspace-directory reverse lookup ----

(ert-deftest agent-repl-test-worktree-ws-name-for-dir-finds-the-workspace ()
  "The reverse lookup answers the live workspace registered at that path."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/tmp/wt-one/")
    (should (equal (agent-repl--ws-name-for-dir "/tmp/wt-one/") "ws1"))))

(ert-deftest agent-repl-test-worktree-ws-name-for-dir-nil-for-nil ()
  "No directory, no answer."
  (agent-repl-test--with-clean-state
    (should-not (agent-repl--ws-name-for-dir nil))))

(ert-deftest agent-repl-test-worktree-ws-name-for-dir-nil-for-an-unknown-dir ()
  "A path no workspace is registered at answers nil."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/tmp/wt-one/")
    (should-not (agent-repl--ws-name-for-dir "/tmp/somewhere-else/"))))

(ert-deftest agent-repl-test-worktree-ws-name-for-dir-skips-a-tombstone ()
  "A killed workspace's preserved path must not shadow a live one at it."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "dead" :project-dir "/tmp/wt-one/")
    (agent-repl--ws-put "dead" :killed-at (current-time))
    (agent-repl--ws-put "live" :project-dir "/tmp/wt-one/")
    (should (equal (agent-repl--ws-name-for-dir "/tmp/wt-one/") "live"))))

;;;; ---- The async git boundary ----

(ert-deftest agent-repl-test-worktree-async-git-is-a-registered-boundary ()
  "The async git wrapper is a registered external boundary."
  (should (memq 'agent-repl--async-git agent-repl--external-boundary-functions)))

(ert-deftest agent-repl-test-worktree-async-git-settle-reports-success ()
  "A zero exit is delivered to the callback as success with the output."
  (let ((seen nil)
        (buf (generate-new-buffer " *agent-repl-test-git*")))
    (unwind-protect
        (progn
          (with-current-buffer buf (insert "  fetched  "))
          (cl-letf (((symbol-function 'process-exit-status) (lambda (_p) 0))
                    ((symbol-function 'process-status) (lambda (_p) 'exit))
                    ((symbol-function 'process-name) (lambda (_p) "test"))
                    ((symbol-function 'process-buffer) (lambda (_p) buf))
                    ((symbol-function 'process-get)
                     (lambda (_p key)
                       (pcase key
                         ('agent-repl-callback
                          (lambda (ok out) (setq seen (list ok out))))
                         ('agent-repl-log-workspace "git-ws"))))
                    ((symbol-function 'agent-repl--kill-buffer-safely) (lambda (_b) nil)))
            (agent-repl--async-git-settle 'fake-proc)
            (should (equal seen '(t "fetched")))))
      (kill-buffer buf))))

(ert-deftest agent-repl-test-worktree-async-git-settle-reports-failure ()
  "A non-zero exit is delivered as a failure, never swallowed."
  (let ((seen nil)
        (buf (generate-new-buffer " *agent-repl-test-git*")))
    (unwind-protect
        (progn
          (with-current-buffer buf (insert "fatal: no remote"))
          (cl-letf (((symbol-function 'process-exit-status) (lambda (_p) 128))
                    ((symbol-function 'process-status) (lambda (_p) 'exit))
                    ((symbol-function 'process-name) (lambda (_p) "test"))
                    ((symbol-function 'process-buffer) (lambda (_p) buf))
                    ((symbol-function 'process-get)
                     (lambda (_p key)
                       (pcase key
                         ('agent-repl-callback
                          (lambda (ok out) (setq seen (list ok out))))
                         ('agent-repl-log-workspace "git-ws"))))
                    ((symbol-function 'agent-repl--kill-buffer-safely) (lambda (_b) nil)))
            (agent-repl--async-git-settle 'fake-proc)
            (should (equal seen '(nil "fatal: no remote")))))
      (kill-buffer buf))))

(ert-deftest agent-repl-test-worktree-async-git-settle-rejects-a-missing-log-scope ()
  "A malformed async process records and signals its missing workspace scope."
  ;; Arrange.
  (let ((logged-workspace nil)
        (buf (generate-new-buffer " *agent-repl-test-git*")))
    (unwind-protect
        (cl-letf (((symbol-function 'process-exit-status) (lambda (_p) 0))
                  ((symbol-function 'process-name) (lambda (_p) "test"))
                  ((symbol-function 'process-buffer) (lambda (_p) buf))
                  ((symbol-function 'process-get) (lambda (_p _key) nil))
                  ((symbol-function 'agent-repl--error)
                   (lambda (ws &rest _args) (setq logged-workspace ws))))
          ;; Act / Assert.
          (should-error (agent-repl--async-git-settle 'fake-proc)
                        :type 'error)
          (should (agent-repl--central-log-scope-reason logged-workspace)))
      (kill-buffer buf))))

;;;; ---- Initial buffers ----

(ert-deftest agent-repl-test-worktree-initial-buffers-unmatched-pattern-opens-nothing ()
  "A configuration naming another repo's files opens nothing here."
  (agent-repl-test--with-clean-state
    (let ((opened nil)
          (agent-repl-workspace-initial-buffers '(("/other/repo" . ("README.md")))))
      (cl-letf (((symbol-function 'agent-repl--ws-resolve-persp) (lambda (_ws) 'persp))
                ((symbol-function 'find-file-noselect) (lambda (f) (push f opened) nil)))
        (agent-repl--open-initial-buffers "ws1" "/tmp/wt")
        (should-not opened)))))

(ert-deftest agent-repl-test-worktree-initial-buffers-missing-file-opens-nothing ()
  "A configured file that is not on disk is logged, not opened."
  (agent-repl-test--with-clean-state
    (let ((opened nil)
          (agent-repl-workspace-initial-buffers '(("/tmp" . ("nope-12345.md")))))
      (cl-letf (((symbol-function 'agent-repl--ws-resolve-persp) (lambda (_ws) 'persp))
                ((symbol-function 'find-file-noselect) (lambda (f) (push f opened) nil)))
        (agent-repl--open-initial-buffers "ws1" "/tmp")
        (should-not opened)))))

(ert-deftest agent-repl-test-worktree-initial-buffers-no-persp-opens-nothing ()
  "With no perspective to add buffers to there is nothing to do."
  (agent-repl-test--with-clean-state
    (let ((opened nil)
          (agent-repl-workspace-initial-buffers '(("/tmp" . ("x.md")))))
      (cl-letf (((symbol-function 'agent-repl--ws-resolve-persp) (lambda (_ws) nil))
                ((symbol-function 'find-file-noselect) (lambda (f) (push f opened) nil)))
        (agent-repl--open-initial-buffers "ws1" "/tmp")
        (should-not opened)))))

;;;; ---- What is deliberately gone ----

(ert-deftest agent-repl-test-worktree-the-old-evaluator-is-gone ()
  "The editor evaluates an agent's elisp in ONE handler, evaluate.el; the
emacsclient-reached evaluator it replaced is gone."
  (dolist (sym '(agent-repl--eval-code-string agent-repl--eval-format-prompt
                 agent-repl--eval-truncate agent-repl--eval-snippet))
    (should-not (fboundp sym)))
  (should-not (boundp 'agent-repl-eval-output-max-chars)))

(ert-deftest agent-repl-test-worktree-defines-no-creation-command ()
  "Workspace CREATION is the daemon's: no creation flavor survives here.
Asserted rather than merely deleted, because a re-introduced local
creation path is exactly the regression the overhaul removed -- Emacs
runs no `git worktree add' and names no branch."
  (dolist (sym '(agent-repl-create-worktree-workspace
                 agent-repl-create-worktree-workspace-from-origin-master
                 agent-repl-fork-worktree-workspace
                 agent-repl-create-doom-oneshot-workspace
                 agent-repl-create-explanation-engine-oneshot-workspace
                 agent-repl--remove-git-worktree
                 agent-repl--workspace-merge-async
                 agent-repl-workspace-merge-current-into-source))
    (should-not (fboundp sym))))

(provide 'test-worktree)

;;; test-worktree.el ends here

;;;; ---- kill-buffer-safely detaches the buffer's process ----

(ert-deftest agent-repl-test-worktree-kill-buffer-safely-detaches-the-sentinel ()
  "The buffer's process loses its sentinel BEFORE the buffer is killed.
`kill-buffer' deletes the buffer's process, which delivers a fresh
terminal status to that sentinel from inside the kill; detaching first is
what stops a sentinel that kills its own buffer from re-entering itself."
  ;; Arrange
  (let ((buf (generate-new-buffer " *agent-repl-test-kbs*"))
        (order nil))
    (unwind-protect
        (cl-letf (((symbol-function 'get-buffer-process) (lambda (_b) 'fake-proc))
                  ((symbol-function 'set-process-query-on-exit-flag) #'ignore)
                  ((symbol-function 'set-process-filter) #'ignore)
                  ((symbol-function 'set-process-sentinel)
                   (lambda (_p s) (push (cons 'sentinel s) order)))
                  ((symbol-function 'kill-buffer)
                   (lambda (_b) (push 'kill order))))
          ;; Act
          (agent-repl--kill-buffer-safely buf)
          ;; Assert
          (should (equal (nreverse order) '((sentinel . ignore) kill))))
      (kill-buffer buf))))

(ert-deftest agent-repl-test-worktree-kill-buffer-safely-detaches-the-filter ()
  "The buffer's process also loses its filter, so no output lands mid-kill."
  ;; Arrange
  (let ((buf (generate-new-buffer " *agent-repl-test-kbs-filter*"))
        (filter 'unset))
    (unwind-protect
        (cl-letf (((symbol-function 'get-buffer-process) (lambda (_b) 'fake-proc))
                  ((symbol-function 'set-process-query-on-exit-flag) #'ignore)
                  ((symbol-function 'set-process-sentinel) #'ignore)
                  ((symbol-function 'set-process-filter)
                   (lambda (_p f) (setq filter f)))
                  ((symbol-function 'kill-buffer) #'ignore))
          ;; Act
          (agent-repl--kill-buffer-safely buf)
          ;; Assert
          (should (eq filter #'ignore)))
      (kill-buffer buf))))

(ert-deftest agent-repl-test-worktree-kill-buffer-safely-terminates-a-self-killing-sentinel ()
  "A sentinel that kills its own process buffer runs ONCE, not forever.
Drives the exact recursion the hung Emacs sampled: killing the buffer
re-delivers a terminal status to the process's sentinel, which kills the
same buffer again.  With the detachment in place the re-delivery reaches
`ignore' and the stack unwinds."
  ;; Arrange
  (let* ((buf (generate-new-buffer " *agent-repl-test-kbs-loop*"))
         (installed nil)
         (runs 0)
         (sentinel nil))
    (setq sentinel (lambda (&rest _)
                     (cl-incf runs)
                     (when (< runs 100)   ; a runaway is bounded, not hung
                       (agent-repl--kill-buffer-safely buf))))
    (setq installed sentinel)
    (unwind-protect
        (cl-letf (((symbol-function 'get-buffer-process)
                   (lambda (_b) (and (buffer-live-p buf) 'fake-proc)))
                  ((symbol-function 'set-process-query-on-exit-flag) #'ignore)
                  ((symbol-function 'set-process-filter) #'ignore)
                  ((symbol-function 'set-process-sentinel)
                   (lambda (_p s) (setq installed s)))
                  ;; `kill_buffer_processes': the kill deletes the process,
                  ;; which delivers a new terminal status to whatever
                  ;; sentinel is installed at that moment.
                  ((symbol-function 'kill-buffer)
                   (lambda (_b) (funcall installed 'fake-proc "finished\n"))))
          ;; Act
          (funcall sentinel 'fake-proc "finished\n")
          ;; Assert
          (should (= runs 1)))
      (kill-buffer buf))))
