;;; test-worktree.el --- ERT tests for agent-repl worktree.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;   AGENT_REPL_FORBID_VENDOR_CALLS=1 emacs -batch -Q -l ert \
;;     -l lisp/test-worktree.el -f ert-run-tests-batch-and-exit
;;
;; WHAT IS LEFT TO TEST is what is left of the file: the eval helpers the
;; /runtime-eval-code skill reaches over emacsclient, the read-only branch
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

;;;; ---- The eval helpers ----

(ert-deftest agent-repl-test-worktree-eval-returns-the-last-value ()
  "The value is that of the LAST form: the caller asked for one answer."
  (should (equal (plist-get (agent-repl--eval-code-string "(+ 1 1) (* 3 4)")
                            :value-string)
                 "12")))

(ert-deftest agent-repl-test-worktree-eval-captures-princ-output ()
  "`princ' output is captured rather than vanishing."
  (should (equal (plist-get (agent-repl--eval-code-string "(princ \"hello\")") :printed)
                 "hello")))

(ert-deftest agent-repl-test-worktree-eval-traps-an-error ()
  "A trapped error populates `:error' instead of escaping to the caller."
  (should (string-match-p "arith-error\\|Arithmetic"
                          (plist-get (agent-repl--eval-code-string "(/ 1 0)") :error))))

(ert-deftest agent-repl-test-worktree-eval-error-leaves-no-value ()
  "An error means there is no return value to report."
  (should-not (plist-get (agent-repl--eval-code-string "(/ 1 0)") :value-string)))

(ert-deftest agent-repl-test-worktree-eval-reports-partial-output-on-error ()
  "Side effects from earlier forms are reported: pretending otherwise misleads."
  (should (equal (plist-get (agent-repl--eval-code-string "(princ \"before\") (/ 1 0)")
                            :printed)
                 "before")))

(ert-deftest agent-repl-test-worktree-eval-whitespace-only-yields-nil ()
  "A whitespace-only code string is not an error; it evaluates to nil."
  (let ((result (agent-repl--eval-code-string "   \n  ")))
    (should (equal (plist-get result :value-string) "nil"))
    (should-not (plist-get result :error))))

(ert-deftest agent-repl-test-worktree-eval-empty-string-yields-nil ()
  "Neither is an empty code string."
  (should (equal (plist-get (agent-repl--eval-code-string "") :value-string) "nil")))

(ert-deftest agent-repl-test-worktree-eval-truncate-keeps-short-text ()
  "Text inside the cap is returned unchanged."
  (let ((agent-repl-eval-output-max-chars 100))
    (should (equal (agent-repl--eval-truncate "short") "short"))))

(ert-deftest agent-repl-test-worktree-eval-truncate-clips-long-text ()
  "Text over the cap is clipped and SAYS it was clipped."
  (let ((agent-repl-eval-output-max-chars 10))
    (let ((out (agent-repl--eval-truncate (make-string 50 ?x))))
      (should (string-prefix-p (make-string 10 ?x) out))
      (should (string-match-p "truncated to 10 chars" out)))))

(ert-deftest agent-repl-test-worktree-eval-truncate-zero-cap-disables-clipping ()
  "A zero cap disables truncation entirely."
  (let ((agent-repl-eval-output-max-chars 0))
    (should (equal (length (agent-repl--eval-truncate (make-string 50 ?x))) 50))))

(ert-deftest agent-repl-test-worktree-eval-truncate-tolerates-nil ()
  "Nil text is the empty string, not a crash."
  (should (equal (agent-repl--eval-truncate nil) "")))

(ert-deftest agent-repl-test-worktree-eval-format-labels-the-result ()
  "A successful eval is labeled `result' so a reader can match on it."
  (let ((out (agent-repl--eval-format-prompt "(+ 1 1)" nil nil "2" nil)))
    (should (string-match-p ";; result:" out))
    (should (string-match-p "Elisp eval result" out))))

(ert-deftest agent-repl-test-worktree-eval-format-labels-the-error ()
  "A failed eval is labeled `error' and headed as an ERROR."
  (let ((out (agent-repl--eval-format-prompt "(/ 1 0)" nil nil nil "boom")))
    (should (string-match-p ";; error:" out))
    (should (string-match-p "Elisp eval ERROR" out))))

(ert-deftest agent-repl-test-worktree-eval-format-omits-empty-printed ()
  "An empty `printed' section is omitted rather than drawn blank."
  (should-not (string-match-p ";; printed:"
                             (agent-repl--eval-format-prompt "(+ 1 1)" nil "" "2" nil))))

(ert-deftest agent-repl-test-worktree-eval-format-includes-printed-output ()
  "Captured output is included when there is any."
  (should (string-match-p ";; printed:"
                          (agent-repl--eval-format-prompt "(princ 1)" nil "1" "1" nil))))

(ert-deftest agent-repl-test-worktree-eval-format-carries-the-note ()
  "A note is echoed in the header so the reader knows what was asked."
  (should (string-match-p "note: dump the roster"
                          (agent-repl--eval-format-prompt
                           "(+ 1 1)" "dump the roster" nil "2" nil))))

(ert-deftest agent-repl-test-worktree-eval-format-includes-the-code ()
  "The source is embedded so the answer is self-contained."
  (should (string-match-p ";; code:"
                          (agent-repl--eval-format-prompt "(+ 1 1)" nil nil "2" nil))))

(ert-deftest agent-repl-test-worktree-eval-snippet-labels-its-block ()
  "A snippet is its label followed by the text."
  (should (equal (agent-repl--eval-snippet "code" "(+ 1 1)") ";; code:\n(+ 1 1)\n")))

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
