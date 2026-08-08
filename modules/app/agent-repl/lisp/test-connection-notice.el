;;; test-connection-notice.el --- Tests for connection-notice.el -*- lexical-binding: t; -*-

;;; Commentary:

;; The retractable daemon-connection notice: raising one, taking every one
;; back when the link returns, and leaving alone what this module did not
;; write.  One edge case per test (AAA).

;;; Code:

(require 'ert)
(require 'cl-lib)

(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "test-helpers.el" dir) nil t))

(defmacro agent-repl-test--with-notices (&rest body)
  "Run BODY over a private notice registry and a private notice buffer."
  (declare (indent 0))
  `(let* ((agent-repl--connection-notices nil)
          (agent-repl--connection-notice-echo nil)
          (agent-repl-connection-notice-buffer " *agent-repl-test-warnings*"))
     (unwind-protect
         (progn ,@body)
       (when (get-buffer agent-repl-connection-notice-buffer)
         (kill-buffer agent-repl-connection-notice-buffer)))))

(defun agent-repl-test--notice-buffer-string ()
  "Return the notice buffer's contents, or the empty string when it is gone."
  (let ((buffer (get-buffer agent-repl-connection-notice-buffer)))
    (if buffer (with-current-buffer buffer (buffer-string)) "")))

;;;; ---- Raising ---------------------------------------------------------

(ert-deftest agent-repl-test-connection-notice-warn-displays-its-text ()
  "A raised notice reaches the notice buffer."
  ;; Arrange / Act
  (agent-repl-test--with-notices
    (agent-repl-connection-notice-warn "the daemon is unreachable")
    ;; Assert
    (should (string-match-p "the daemon is unreachable"
                            (agent-repl-test--notice-buffer-string)))))

(ert-deftest agent-repl-test-connection-notice-warn-records-it-as-standing ()
  "A raised notice is registered so it can be retracted later."
  ;; Arrange / Act
  (agent-repl-test--with-notices
    (agent-repl-connection-notice-warn "the daemon is unreachable")
    ;; Assert
    (should (= 1 (length agent-repl--connection-notices)))))

;;;; ---- Retraction ------------------------------------------------------

(ert-deftest agent-repl-test-connection-notice-retract-removes-the-text ()
  "Retraction takes the notice's own text out of the buffer."
  ;; Arrange
  (agent-repl-test--with-notices
    (agent-repl-connection-notice-warn "the daemon is unreachable")
    ;; Act
    (agent-repl-connection-notices-retract "reconnected")
    ;; Assert
    (should-not (string-match-p "the daemon is unreachable"
                                (agent-repl-test--notice-buffer-string)))))

(ert-deftest agent-repl-test-connection-notice-retract-reports-how-many ()
  "Retraction returns the number of notices that were standing."
  ;; Arrange
  (agent-repl-test--with-notices
    (agent-repl-connection-notice-warn "one")
    (agent-repl-connection-notice-warn "two")
    ;; Act / Assert
    (should (= 2 (agent-repl-connection-notices-retract "reconnected")))))

(ert-deftest agent-repl-test-connection-notice-retract-empties-the-registry ()
  "Nothing stands after a retraction, so a second one has nothing to do."
  ;; Arrange
  (agent-repl-test--with-notices
    (agent-repl-connection-notice-warn "the daemon is unreachable")
    (agent-repl-connection-notices-retract "reconnected")
    ;; Act / Assert
    (should (= 0 (agent-repl-connection-notices-retract "reconnected")))))

(ert-deftest agent-repl-test-connection-notice-retract-of-nothing-is-zero ()
  "A reconnect after no outage was ever reported retracts nothing."
  ;; Arrange / Act / Assert
  (agent-repl-test--with-notices
    (should (= 0 (agent-repl-connection-notices-retract "reconnected")))))

(ert-deftest agent-repl-test-connection-notice-retract-spares-a-foreign-warning ()
  "A warning this module did not write survives the retraction.
The retraction is bounded by the region the notice actually wrote, so a
compiler warning sharing the buffer is not collateral."
  ;; Arrange
  (agent-repl-test--with-notices
    (display-warning 'some-other-package "an unrelated warning"
                     :warning agent-repl-connection-notice-buffer)
    (agent-repl-connection-notice-warn "the daemon is unreachable")
    ;; Act
    (agent-repl-connection-notices-retract "reconnected")
    ;; Assert
    (should (string-match-p "an unrelated warning"
                            (agent-repl-test--notice-buffer-string)))))

(ert-deftest agent-repl-test-connection-notice-retract-keeps-a-shared-buffer ()
  "The buffer is NOT dismissed while it still holds a foreign warning."
  ;; Arrange
  (agent-repl-test--with-notices
    (display-warning 'some-other-package "an unrelated warning"
                     :warning agent-repl-connection-notice-buffer)
    (agent-repl-connection-notice-warn "the daemon is unreachable")
    ;; Act
    (agent-repl-connection-notices-retract "reconnected")
    ;; Assert
    (should (get-buffer agent-repl-connection-notice-buffer))))

(ert-deftest agent-repl-test-connection-notice-retract-dismisses-an-empty-buffer ()
  "The last notice out takes the (now empty) notice buffer with it.
An empty `*Warnings*' window left standing reads as \"something is still
wrong\" just as loudly as the text it used to hold."
  ;; Arrange
  (agent-repl-test--with-notices
    (agent-repl-connection-notice-warn "the daemon is unreachable")
    ;; Act
    (agent-repl-connection-notices-retract "reconnected")
    ;; Assert
    (should-not (get-buffer agent-repl-connection-notice-buffer))))

(ert-deftest agent-repl-test-connection-notice-retract-survives-a-killed-buffer ()
  "A notice whose buffer the user killed retracts without erroring.
It is already gone, which is the outcome the retraction wanted."
  ;; Arrange
  (agent-repl-test--with-notices
    (agent-repl-connection-notice-warn "the daemon is unreachable")
    (kill-buffer agent-repl-connection-notice-buffer)
    ;; Act / Assert
    (should (= 1 (agent-repl-connection-notices-retract "reconnected")))))

;;;; ---- The echoed notice -----------------------------------------------

(ert-deftest agent-repl-test-connection-notice-echo-is-recorded ()
  "An echoed notice is registered even though nothing was written."
  ;; Arrange / Act
  (agent-repl-test--with-notices
    (agent-repl-connection-notice-echo "the daemon is unreachable")
    ;; Assert
    (should (equal "the daemon is unreachable" agent-repl--connection-notice-echo))))

(ert-deftest agent-repl-test-connection-notice-retract-clears-the-echo-record ()
  "Retraction forgets the echoed notice, so a later one is not double-cleared."
  ;; Arrange
  (agent-repl-test--with-notices
    (agent-repl-connection-notice-echo "the daemon is unreachable")
    ;; Act
    (agent-repl-connection-notices-retract "reconnected")
    ;; Assert
    (should-not agent-repl--connection-notice-echo)))

(ert-deftest agent-repl-test-connection-notice-retract-blanks-a-matching-echo ()
  "The echo area is cleared when it still shows the notice."
  ;; Arrange
  (agent-repl-test--with-notices
    (let ((cleared nil))
      (agent-repl-connection-notice-echo "the daemon is unreachable")
      (cl-letf (((symbol-function 'current-message)
                 (lambda () "agent-repl: the daemon is unreachable"))
                ((symbol-function 'message)
                 (lambda (fmt &rest _) (when (null fmt) (setq cleared t)))))
        ;; Act
        (agent-repl-connection-notices-retract "reconnected"))
      ;; Assert
      (should cleared))))

(ert-deftest agent-repl-test-connection-notice-retract-spares-a-replaced-echo ()
  "An echo the user has since replaced is theirs and is left alone."
  ;; Arrange
  (agent-repl-test--with-notices
    (let ((cleared nil))
      (agent-repl-connection-notice-echo "the daemon is unreachable")
      (cl-letf (((symbol-function 'current-message)
                 (lambda () "Saved /tmp/file.el"))
                ((symbol-function 'message)
                 (lambda (fmt &rest _) (when (null fmt) (setq cleared t)))))
        ;; Act
        (agent-repl-connection-notices-retract "reconnected"))
      ;; Assert
      (should-not cleared))))

(provide 'test-connection-notice)
;;; test-connection-notice.el ends here
