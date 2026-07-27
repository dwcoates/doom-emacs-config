;;; test-dir-watcher.el --- Tests for legacy watcher teardown -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)

(ert-deftest agent-repl-test-dir-watcher-removes-valid-legacy-descriptor ()
  (let (removed)
    (cl-letf (((symbol-function 'file-notify-valid-p)
               (lambda (descriptor)
                 (eq descriptor 'legacy-descriptor)))
              ((symbol-function 'file-notify-rm-watch)
               (lambda (descriptor)
                 (setq removed descriptor))))
      (should-not
       (agent-repl--dir-watcher-remove-legacy
        'legacy-descriptor "workspace_commands"))
      (should (eq removed 'legacy-descriptor)))))

(ert-deftest agent-repl-test-dir-watcher-skips-absent-or-invalid-descriptor ()
  (let ((remove-calls 0))
    (cl-letf (((symbol-function 'file-notify-valid-p)
               (lambda (_descriptor) nil))
              ((symbol-function 'file-notify-rm-watch)
               (lambda (_descriptor)
                 (cl-incf remove-calls))))
      (should-not
       (agent-repl--dir-watcher-remove-legacy nil "workspace_commands"))
      (should-not
       (agent-repl--dir-watcher-remove-legacy 'invalid "workspace_commands"))
      (should (zerop remove-calls)))))

(provide 'agent-repl-test-dir-watcher)
;;; test-dir-watcher.el ends here
