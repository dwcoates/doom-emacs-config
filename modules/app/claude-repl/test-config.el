;;; test-config.el --- Tests for claude-repl config.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the early-orphan-cherry-pick recovery defined in
;; `config.el'.  The recovery runs at the top of config.el (before any
;; module file is `require'd) and must therefore not depend on any
;; other claude-repl module having loaded successfully — its only
;; dependencies are built-in Elisp and the persisted snapshot file at
;; `~/.claude/emacs/workspaces.el'.
;;
;; The tests stub `call-process' so no real `git' is invoked, redirect
;; the snapshot file to a temp path, and seed each fixture with a
;; specific `:in-flight-merges' shape to exercise one decision branch
;; per test.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load shared stubs first so `config.el' can be loaded in -Q.
(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "test-helpers.el" dir) nil t))

;; Load only `config.el' — it defines the early-recovery functions.
;; Avoid loading the full module to keep the test scope tight; the
;; load!-wrapped `claude-repl--load-module' would otherwise pull in the
;; rest of the module.  We define `load!' as a no-op so the load calls
;; in `config.el' are skipped while the function definitions outside
;; those calls (including the early-recovery helpers + invocation) take
;; effect normally.
(unless (fboundp 'load!)
  (defmacro load! (&rest _args)
    "Test-only no-op stub: skip the sub-module loads in config.el."
    nil))

;; Loading config.el also invokes `claude-repl--early-recover-orphan-cherry-picks'
;; against the host's real `~/.claude/emacs/workspaces.el', which is
;; undesirable in a test run.  Suppress that invocation by binding the
;; function to a no-op for the duration of the load.
(let ((claude-repl--config-file-load-suppressed t))
  (cl-letf (((symbol-function 'message) #'ignore))
    (let ((dir (file-name-directory (or load-file-name buffer-file-name))))
      ;; Redefine `claude-repl--early-recover-orphan-cherry-picks' to a
      ;; no-op BEFORE config.el load, then restore the real one after
      ;; — config.el calls it at top-level, and we don't want that to
      ;; touch the host's real snapshot file.
      (defun claude-repl--early-recover-orphan-cherry-picks () nil)
      (load (expand-file-name "config.el" dir) nil t))))

;;;; ---- Test helpers ----

(defmacro claude-repl-test--with-snapshot-fixture (path content &rest body)
  "Write CONTENT (a sexp) to PATH, run BODY, then remove PATH on exit."
  (declare (indent 2))
  `(progn
     (let ((coding-system-for-write 'utf-8))
       (with-temp-file ,path
         (let ((print-length nil) (print-level nil))
           (prin1 ,content (current-buffer)))))
     (unwind-protect (progn ,@body)
       (when (file-exists-p ,path) (delete-file ,path)))))

(defmacro claude-repl-test--with-stub-git (responses &rest body)
  "Run BODY with `call-process' replaced by a stub.
RESPONSES is an alist mapping (CMD-MATCHER) → (EXIT-CODE STDOUT).
Each call is matched by passing the joined argv to CMD-MATCHER (a
function returning non-nil for a hit).  Recorded calls are
appended to `claude-repl-test--git-calls'."
  (declare (indent 1))
  `(let ((claude-repl-test--git-calls nil))
     (cl-letf (((symbol-function 'call-process)
                (lambda (program &optional _infile destination _display &rest args)
                  (push (cons program args) claude-repl-test--git-calls)
                  (let ((joined (mapconcat #'identity (cons program args) " ")))
                    (cl-loop for (matcher exit stdout) in ,responses
                             when (funcall matcher joined)
                             do (progn
                                  ;; `call-process' DESTINATION can be:
                                  ;; - a buffer object → insert into it
                                  ;; - t / `(t nil)' → insert into current buffer
                                  ;; - nil / 0 → discard stdout
                                  (when stdout
                                    (cond
                                     ((bufferp destination)
                                      (with-current-buffer destination
                                        (insert stdout)))
                                     ((or (eq destination t)
                                          (and (listp destination)
                                               (eq (car destination) t)))
                                      (insert stdout))))
                                  (cl-return exit))
                             finally (cl-return 0))))))
       ,@body)))

;;;; ---- Tests ----

(ert-deftest claude-repl-config-test-early-recovery/empty-in-flight-is-noop ()
  "Empty `:in-flight-merges' in the snapshot is a no-op — no git calls,
no rewrite."
  (let ((snap (make-temp-file "claude-snap-")))
    (claude-repl-test--with-snapshot-fixture snap
        '(:workspaces (("ws-a" :project-dir "/tmp/a"))
          :merge-queue nil
          :in-flight-merges nil)
      (cl-letf (((symbol-function 'expand-file-name)
                 (lambda (name &optional dir)
                   (if (equal name "~/.claude/emacs/workspaces.el")
                       snap
                     (funcall (symbol-function 'file-name-absolute-p)
                              name)
                     (concat (or dir "") name)))))
        (claude-repl-test--with-stub-git
            '(((lambda (_) t) 0 ""))
          (cl-letf (((symbol-function 'message) #'ignore))
            (claude-repl--early-recover-orphan-cherry-picks))
          (should (null claude-repl-test--git-calls)))))))

(ert-deftest claude-repl-config-test-early-recovery/missing-file-is-noop ()
  "Snapshot file absent → recovery is a silent no-op."
  (cl-letf (((symbol-function 'expand-file-name)
             (lambda (name &optional _dir)
               (if (equal name "~/.claude/emacs/workspaces.el")
                   "/definitely/not/a/real/path-claude-snap.el"
                 name))))
    (let ((claude-repl-test--git-calls nil))
      (cl-letf (((symbol-function 'call-process)
                 (lambda (&rest args)
                   (push args claude-repl-test--git-calls) 0))
                ((symbol-function 'message) #'ignore))
        (claude-repl--early-recover-orphan-cherry-picks)
        (should (null claude-repl-test--git-calls))))))

(ert-deftest claude-repl-config-test-early-recovery/aborts-when-cherry-pick-head-exists ()
  "An in-flight entry whose target-dir has a live CHERRY_PICK_HEAD must
trigger `git cherry-pick --abort' and re-enqueue the source ws onto
:merge-queue at the back, then rewrite the snapshot with empty
:in-flight-merges."
  (let* ((snap (make-temp-file "claude-snap-"))
         (target-dir (make-temp-file "claude-target-" t))
         (git-dir (expand-file-name ".git" target-dir)))
    (unwind-protect
        (progn
          (make-directory git-dir t)
          ;; Seed CHERRY_PICK_HEAD inside the simulated git dir.  The
          ;; stub `git rev-parse --absolute-git-dir' returns this path,
          ;; and `claude-repl--early-cherry-pick-head-at' checks for
          ;; CHERRY_PICK_HEAD existence under it.
          (with-temp-file (expand-file-name "CHERRY_PICK_HEAD" git-dir)
            (insert "deadbeef"))
          (claude-repl-test--with-snapshot-fixture snap
              `(:workspaces (("ws-a" :project-dir ,target-dir))
                :merge-queue nil
                :in-flight-merges ((:source-ws "ws-a" :target-dir ,target-dir :started-at 1.0)))
            (cl-letf (((symbol-function 'expand-file-name)
                       (let ((orig (symbol-function 'expand-file-name)))
                         (lambda (name &optional dir)
                           (if (equal name "~/.claude/emacs/workspaces.el")
                               snap
                             (funcall orig name dir))))))
              (claude-repl-test--with-stub-git
                  `(((lambda (s) (string-match-p "rev-parse --absolute-git-dir" s))
                     0 ,git-dir)
                    ((lambda (s) (string-match-p "cherry-pick --abort" s))
                     0 ""))
                (cl-letf (((symbol-function 'message) #'ignore))
                  (claude-repl--early-recover-orphan-cherry-picks))
                ;; Saw both git calls: rev-parse then abort.
                (should (cl-some (lambda (c)
                                   (string-match-p "cherry-pick --abort"
                                                   (mapconcat #'identity (cons (car c) (cdr c)) " ")))
                                 claude-repl-test--git-calls)))
              ;; Snapshot must be rewritten: :in-flight-merges empty,
              ;; :merge-queue has ws-a at the back.
              (let* ((raw (with-temp-buffer
                            (insert-file-contents snap)
                            (goto-char (point-min))
                            (read (current-buffer))))
                     (mq (plist-get raw :merge-queue))
                     (ifm (plist-get raw :in-flight-merges)))
                (should (null ifm))
                (should (= 1 (length mq)))
                (should (equal (plist-get (car mq) :source-ws) "ws-a"))
                (should-not (plist-get (car mq) :halt-until-human))))))
      (when (file-exists-p snap) (delete-file snap))
      (when (file-directory-p target-dir)
        (delete-directory target-dir t)))))

(ert-deftest claude-repl-config-test-early-recovery/no-cherry-pick-head-just-clears-bookkeeping ()
  "Entry whose target-dir has NO CHERRY_PICK_HEAD must NOT trigger an
abort (calling abort with nothing in flight would error) — it just
clears the bookkeeping entry from :in-flight-merges."
  (let* ((snap (make-temp-file "claude-snap-"))
         (target-dir (make-temp-file "claude-target-" t))
         (git-dir (expand-file-name ".git" target-dir)))
    (unwind-protect
        (progn
          (make-directory git-dir t)
          ;; No CHERRY_PICK_HEAD seeded — merge completed before crash.
          (claude-repl-test--with-snapshot-fixture snap
              `(:workspaces (("ws-a" :project-dir ,target-dir))
                :merge-queue nil
                :in-flight-merges ((:source-ws "ws-a" :target-dir ,target-dir :started-at 1.0)))
            (cl-letf (((symbol-function 'expand-file-name)
                       (let ((orig (symbol-function 'expand-file-name)))
                         (lambda (name &optional dir)
                           (if (equal name "~/.claude/emacs/workspaces.el")
                               snap
                             (funcall orig name dir))))))
              (claude-repl-test--with-stub-git
                  `(((lambda (s) (string-match-p "rev-parse --absolute-git-dir" s))
                     0 ,git-dir))
                (cl-letf (((symbol-function 'message) #'ignore))
                  (claude-repl--early-recover-orphan-cherry-picks))
                ;; No abort call recorded.
                (should-not (cl-some (lambda (c)
                                       (string-match-p "cherry-pick --abort"
                                                       (mapconcat #'identity (cons (car c) (cdr c)) " ")))
                                     claude-repl-test--git-calls)))
              ;; Snapshot rewritten: :in-flight-merges empty,
              ;; :merge-queue unchanged (no re-enqueue without an orphan).
              (let* ((raw (with-temp-buffer
                            (insert-file-contents snap)
                            (goto-char (point-min))
                            (read (current-buffer))))
                     (mq (plist-get raw :merge-queue))
                     (ifm (plist-get raw :in-flight-merges)))
                (should (null ifm))
                (should (null mq))))))
      (when (file-exists-p snap) (delete-file snap))
      (when (file-directory-p target-dir)
        (delete-directory target-dir t)))))

(ert-deftest claude-repl-config-test-early-recovery/skips-malformed-entries ()
  "Entries missing :source-ws or :target-dir are skipped — recovery
must not crash on a partial entry from a botched prior write."
  (let ((snap (make-temp-file "claude-snap-")))
    (claude-repl-test--with-snapshot-fixture snap
        '(:workspaces nil
          :merge-queue nil
          :in-flight-merges ((:source-ws nil :target-dir "/tmp/x" :started-at 1.0)
                             (:source-ws "ws-b" :target-dir nil :started-at 2.0)))
      (cl-letf (((symbol-function 'expand-file-name)
                 (let ((orig (symbol-function 'expand-file-name)))
                   (lambda (name &optional dir)
                     (if (equal name "~/.claude/emacs/workspaces.el")
                         snap
                       (funcall orig name dir))))))
        (claude-repl-test--with-stub-git '()
          (cl-letf (((symbol-function 'message) #'ignore))
            (claude-repl--early-recover-orphan-cherry-picks))
          (should (null claude-repl-test--git-calls)))
        (let* ((raw (with-temp-buffer
                      (insert-file-contents snap)
                      (goto-char (point-min))
                      (read (current-buffer))))
               (mq (plist-get raw :merge-queue))
               (ifm (plist-get raw :in-flight-merges)))
          (should (null ifm))
          (should (null mq)))))))

(provide 'test-config)

;;; test-config.el ends here
