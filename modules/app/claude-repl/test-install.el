;;; test-install.el --- ERT tests for install.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the Emacs wrapper around .claude/install.sh.  Covers the
;; sandbox-detection predicate, the installed-state predicate against
;; synthetic settings.json fixtures, and the dispatch of interactive
;; commands through to the bash script (mocked).
;;
;; Bash-script integration coverage (fresh install, foreign keys, etc.)
;; lives in a later commit; here we stick to unit-level behavior of the
;; elisp surface.
;;
;; Run with:
;;   emacs -batch -Q -l ert -l test-install.el -f ert-run-tests-batch-and-exit

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Fixtures ----

(defun test-install--json-with-all-hooks ()
  "Return parsed JSON shape with all four managed hooks registered."
  '((hooks
     (Stop . (((hooks . (((type . "command")
                          (command . "~/.claude/hooks/stop-notify.sh")))))))
     (UserPromptSubmit . (((hooks . (((type . "command")
                                      (command . "~/.claude/hooks/prompt-submit-notify.sh")))))))
     (SessionStart . (((hooks . (((type . "command")
                                  (command . "~/.claude/hooks/session-start-notify.sh")))))))
     (Notification . (((matcher . "permission_prompt")
                       (hooks . (((type . "command")
                                  (command . "~/.claude/hooks/permission-notify.sh"))))))))))

(defun test-install--json-missing-one (event-to-drop)
  "Like the all-hooks fixture but with EVENT-TO-DROP removed."
  (let ((json (copy-tree (test-install--json-with-all-hooks))))
    (setcdr (assq 'hooks json)
            (assq-delete-all event-to-drop (cdr (assq 'hooks json))))
    json))

(defun test-install--json-with-foreign-stop ()
  "All-hooks fixture plus a foreign entry under Stop before ours."
  (let ((json (copy-tree (test-install--json-with-all-hooks))))
    (setcdr (assq 'Stop (cdr (assq 'hooks json)))
            (cons '((hooks . (((type . "command")
                               (command . "/some/foreign.sh")))))
                  (cdr (assq 'Stop (cdr (assq 'hooks json))))))
    json))

;;;; ---- sandbox-p ----

(ert-deftest claude-repl-test-in-sandbox-dockerenv ()
  "in-sandbox-p returns t when /.dockerenv exists."
  (cl-letf (((symbol-function 'file-exists-p)
             (lambda (path) (equal path "/.dockerenv")))
            ((symbol-function 'getenv) (lambda (_) nil)))
    (should (claude-repl--in-sandbox-p))))

(ert-deftest claude-repl-test-in-sandbox-env-var ()
  "in-sandbox-p returns t when DOOM_SANDBOX=1."
  (cl-letf (((symbol-function 'file-exists-p) (lambda (_) nil))
            ((symbol-function 'getenv)
             (lambda (k) (and (equal k "DOOM_SANDBOX") "1"))))
    (should (claude-repl--in-sandbox-p))))

(ert-deftest claude-repl-test-in-sandbox-neither ()
  "in-sandbox-p returns nil when neither signal is present."
  (cl-letf (((symbol-function 'file-exists-p) (lambda (_) nil))
            ((symbol-function 'getenv) (lambda (_) nil)))
    (should-not (claude-repl--in-sandbox-p))))

(ert-deftest claude-repl-test-in-sandbox-env-other-value ()
  "DOOM_SANDBOX set to a non-1 value does NOT trigger sandbox mode."
  (cl-letf (((symbol-function 'file-exists-p) (lambda (_) nil))
            ((symbol-function 'getenv)
             (lambda (k) (and (equal k "DOOM_SANDBOX") "0"))))
    (should-not (claude-repl--in-sandbox-p))))

;;;; ---- event-has-command-p ----

(ert-deftest claude-repl-test-event-has-command-match ()
  "Returns non-nil when CMD appears in an entry's inner .hooks[].command."
  (let ((hooks (cdr (assq 'hooks (test-install--json-with-all-hooks)))))
    (should (claude-repl--event-has-command-p
             hooks 'Stop "~/.claude/hooks/stop-notify.sh"))))

(ert-deftest claude-repl-test-event-has-command-mismatch ()
  "Returns nil when CMD differs from any registered entry."
  (let ((hooks (cdr (assq 'hooks (test-install--json-with-all-hooks)))))
    (should-not (claude-repl--event-has-command-p
                 hooks 'Stop "/nowhere.sh"))))

(ert-deftest claude-repl-test-event-has-command-event-absent ()
  "Returns nil when the event is not present in the hooks alist."
  (let ((hooks (cdr (assq 'hooks (test-install--json-missing-one 'Stop)))))
    (should-not (claude-repl--event-has-command-p
                 hooks 'Stop "~/.claude/hooks/stop-notify.sh"))))

(ert-deftest claude-repl-test-event-has-command-foreign-coresident ()
  "Matches even when foreign entries co-exist in the event array."
  (let ((hooks (cdr (assq 'hooks (test-install--json-with-foreign-stop)))))
    (should (claude-repl--event-has-command-p
             hooks 'Stop "~/.claude/hooks/stop-notify.sh"))
    (should (claude-repl--event-has-command-p
             hooks 'Stop "/some/foreign.sh"))))

;;;; ---- hooks-installed-p ----

(ert-deftest claude-repl-test-hooks-installed-all-present ()
  "Returns t when all four managed hooks are registered."
  (cl-letf (((symbol-function 'claude-repl--settings-json)
             #'test-install--json-with-all-hooks))
    (should (claude-repl--hooks-installed-p))))

(ert-deftest claude-repl-test-hooks-installed-missing-stop ()
  "Returns nil when Stop is absent."
  (cl-letf (((symbol-function 'claude-repl--settings-json)
             (lambda () (test-install--json-missing-one 'Stop))))
    (should-not (claude-repl--hooks-installed-p))))

(ert-deftest claude-repl-test-hooks-installed-missing-session-start ()
  "Returns nil when SessionStart is absent (post-e3d7451 dealbreaker)."
  (cl-letf (((symbol-function 'claude-repl--settings-json)
             (lambda () (test-install--json-missing-one 'SessionStart))))
    (should-not (claude-repl--hooks-installed-p))))

(ert-deftest claude-repl-test-hooks-installed-missing-notification ()
  "Returns nil when the permission Notification hook is absent."
  (cl-letf (((symbol-function 'claude-repl--settings-json)
             (lambda () (test-install--json-missing-one 'Notification))))
    (should-not (claude-repl--hooks-installed-p))))

(ert-deftest claude-repl-test-hooks-installed-settings-absent ()
  "Returns nil when settings.json cannot be read."
  (cl-letf (((symbol-function 'claude-repl--settings-json) (lambda () nil)))
    (should-not (claude-repl--hooks-installed-p))))

(ert-deftest claude-repl-test-hooks-installed-foreign-coresident ()
  "Returns t even when foreign hooks co-exist under our event keys."
  (cl-letf (((symbol-function 'claude-repl--settings-json)
             #'test-install--json-with-foreign-stop))
    (should (claude-repl--hooks-installed-p))))

;;;; ---- run-install-action dispatch ----

(ert-deftest claude-repl-test-install-action-sandbox-noop ()
  "install/uninstall/reinstall no-op when sandbox is detected."
  (let ((called nil))
    (cl-letf (((symbol-function 'claude-repl--in-sandbox-p) (lambda () t))
              ((symbol-function 'claude-repl--run-install-script)
               (lambda (&rest _) (setq called t) '(0 ""))))
      (claude-repl--run-install-action "install")
      (should-not called))))

(ert-deftest claude-repl-test-install-action-passes-through ()
  "When not in sandbox, the action arg is forwarded to the script."
  (let ((received-arg nil))
    (cl-letf (((symbol-function 'claude-repl--in-sandbox-p) (lambda () nil))
              ((symbol-function 'claude-repl--run-install-script)
               (lambda (action) (setq received-arg action) '(0 "ok\n")))
              ((symbol-function 'claude-repl--surface-install-output)
               (lambda (_) nil)))
      (claude-repl--run-install-action "reinstall")
      (should (equal received-arg "reinstall")))))

(ert-deftest claude-repl-test-install-action-nonzero-exit-errors ()
  "Non-zero exit surfaces the output buffer and signals an error."
  (let ((buffer-shown nil))
    (cl-letf (((symbol-function 'claude-repl--in-sandbox-p) (lambda () nil))
              ((symbol-function 'claude-repl--run-install-script)
               (lambda (_) '(2 "boom\n")))
              ((symbol-function 'claude-repl--surface-install-output)
               (lambda (_) nil))
              ((symbol-function 'display-buffer)
               (lambda (_) (setq buffer-shown t))))
      (should-error (claude-repl--run-install-action "install"))
      (should buffer-shown))))

(ert-deftest claude-repl-test-install-action-surface-output ()
  "Zero-exit run pipes script output to the output buffer."
  (cl-letf (((symbol-function 'claude-repl--in-sandbox-p) (lambda () nil))
            ((symbol-function 'claude-repl--run-install-script)
             (lambda (_) '(0 "hello from script\n"))))
    (claude-repl--run-install-action "install")
    (with-current-buffer claude-repl--install-output-buffer
      (should (string-match-p "hello from script"
                              (buffer-substring-no-properties
                               (point-min) (point-max)))))))

;;;; ---- Interactive commands ----

(ert-deftest claude-repl-test-install-hooks-calls-install ()
  "`claude-repl-install-hooks' dispatches the `install' action."
  (let ((action nil))
    (cl-letf (((symbol-function 'claude-repl--run-install-action)
               (lambda (a) (setq action a))))
      (claude-repl-install-hooks)
      (should (equal action "install")))))

(ert-deftest claude-repl-test-uninstall-hooks-calls-uninstall ()
  "`claude-repl-uninstall-hooks' dispatches the `uninstall' action."
  (let ((action nil))
    (cl-letf (((symbol-function 'claude-repl--run-install-action)
               (lambda (a) (setq action a))))
      (claude-repl-uninstall-hooks)
      (should (equal action "uninstall")))))

(ert-deftest claude-repl-test-reinstall-hooks-calls-reinstall ()
  "`claude-repl-reinstall-hooks' dispatches the `reinstall' action."
  (let ((action nil))
    (cl-letf (((symbol-function 'claude-repl--run-install-action)
               (lambda (a) (setq action a))))
      (claude-repl-reinstall-hooks)
      (should (equal action "reinstall")))))

;;;; ---- Script location sanity ----

(ert-deftest claude-repl-test-install-script-path-exists ()
  "The resolved install-script path points at the checked-in .claude/install.sh."
  (should (file-exists-p claude-repl--install-script))
  (should (string-match-p "/\\.claude/install\\.sh\\'"
                          claude-repl--install-script)))

;;; test-install.el ends here
