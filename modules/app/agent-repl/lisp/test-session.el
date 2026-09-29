;;; test-session.el --- Tests for session.el -*- lexical-binding: t; -*-

;;; Commentary:

;; What is left of sessions in Emacs: the two finish-edge reactions whose
;; knowledge only this process has (desktop focus, magit buffers) and the
;; model preferences a creation carries onto the wire.  The command
;; assembly, config-dir and permission-flag computation, display-state
;; save/load and session-id bookkeeping are the daemon's now, and their
;; tests went with them.
;;
;; Run with:
;;   AGENT_REPL_FORBID_VENDOR_CALLS=1 emacs -batch -Q -l ert \
;;     -l lisp/test-session.el -f ert-run-tests-batch-and-exit

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Model preferences ----

(ert-deftest agent-repl-test-session-oneshot-candidates-are-a-non-empty-list ()
  "The one-shot picker has something to offer."
  ;; Act / Assert
  (should (and (listp agent-repl-oneshot-model-candidates)
               agent-repl-oneshot-model-candidates
               (cl-every #'stringp agent-repl-oneshot-model-candidates))))

;;;; ---- Reaction (2): the magit refresh ----

(defmacro agent-repl-test-session--with-magit (var &rest body)
  "Run BODY with `magit-refresh' counted onto VAR."
  (declare (indent 1))
  `(let ((,var 0))
     (cl-letf (((symbol-function 'magit-refresh) (lambda () (cl-incf ,var))))
       ,@body)))

(ert-deftest agent-repl-test-session-magit-refresh-hits-a-matching-buffer ()
  "A magit-status buffer on the workspace's directory is refreshed."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((dir (make-temp-file "agent-repl-session-" t)))
      (unwind-protect
          (agent-repl-test-session--with-magit refreshed
            (with-temp-buffer
              (setq-local default-directory (file-name-as-directory dir))
              (setq-local major-mode 'magit-status-mode)
              ;; Act
              (agent-repl--refresh-magit-status-for-dir dir nil)
              ;; Assert
              (should (equal refreshed 1))))
        (delete-directory dir t)))))

(ert-deftest agent-repl-test-session-magit-refresh-skips-a-foreign-buffer ()
  "A status buffer on another repository is left alone."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((dir (make-temp-file "agent-repl-session-" t))
          (other (make-temp-file "agent-repl-other-" t)))
      (unwind-protect
          (agent-repl-test-session--with-magit refreshed
            (with-temp-buffer
              (setq-local default-directory (file-name-as-directory other))
              (setq-local major-mode 'magit-status-mode)
              ;; Act
              (agent-repl--refresh-magit-status-for-dir dir nil)
              ;; Assert
              (should (equal refreshed 0))))
        (delete-directory dir t)
        (delete-directory other t)))))

(ert-deftest agent-repl-test-session-magit-refresh-skips-a-non-magit-buffer ()
  "An ordinary buffer in the worktree is not a status buffer."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((dir (make-temp-file "agent-repl-session-" t)))
      (unwind-protect
          (agent-repl-test-session--with-magit refreshed
            (with-temp-buffer
              (setq-local default-directory (file-name-as-directory dir))
              ;; Act
              (agent-repl--refresh-magit-status-for-dir dir nil)
              ;; Assert
              (should (equal refreshed 0))))
        (delete-directory dir t)))))

(ert-deftest agent-repl-test-session-magit-refresh-without-a-directory-is-a-noop ()
  "No directory means nothing to refresh — not an error."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test-session--with-magit refreshed
      ;; Act
      (agent-repl--refresh-magit-status-for-dir nil nil)
      ;; Assert
      (should (equal refreshed 0)))))

