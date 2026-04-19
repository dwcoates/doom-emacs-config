;;; test-config.el --- ERT tests for config.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the claude-repl config loader and reload functionality.
;;
;; Run with:
;;   emacs -batch -Q -l ert -l test-config.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'cl-lib)

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Tests: claude-repl-reload ----

(ert-deftest claude-repl-test-reload-loads-all-module-files ()
  "claude-repl-reload calls load-file for every module in claude-repl--module-files."
  (let ((loaded-files nil))
    (cl-letf (((symbol-function 'load-file)
               (lambda (path) (push (file-name-base path) loaded-files))))
      (claude-repl-reload)
      (should (equal (nreverse loaded-files) claude-repl--module-files)))))

(ert-deftest claude-repl-test-reload-records-errors ()
  "claude-repl-reload records errors but continues loading remaining modules."
  (let ((loaded-files nil))
    (cl-letf (((symbol-function 'load-file)
               (lambda (path)
                 (let ((base (file-name-base path)))
                   (if (string= base "sentinel")
                       (error "Deliberate test error")
                     (push base loaded-files))))))
      (claude-repl-reload)
      ;; sentinel should not appear in loaded-files
      (should-not (member "sentinel" loaded-files))
      ;; but everything else should
      (should (= (length loaded-files) (1- (length claude-repl--module-files))))
      ;; error should be recorded
      (should (= (length claude-repl--load-errors) 1))
      (should (string= (car (car claude-repl--load-errors)) "sentinel")))))

(ert-deftest claude-repl-test-reload-clears-previous-errors ()
  "claude-repl-reload resets claude-repl--load-errors before loading."
  (setq claude-repl--load-errors '(("fake" . "old error")))
  (cl-letf (((symbol-function 'load-file) #'ignore))
    (claude-repl-reload)
    (should (null claude-repl--load-errors))))

;;; test-config.el ends here
