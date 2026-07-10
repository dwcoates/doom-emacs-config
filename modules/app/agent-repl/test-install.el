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
  "Return parsed JSON shape with all managed hooks registered."
  '((hooks
     (Stop . (((hooks . (((type . "command")
                          (command . "~/.claude/hooks/stop-notify.sh")))))))
     (StopFailure . (((hooks . (((type . "command")
                                 (command . "~/.claude/hooks/stop-failure-notify.sh")))))))
     (SubagentStart . (((hooks . (((type . "command")
                                   (command . "~/.claude/hooks/subagent-start-notify.sh")))))))
     (SubagentStop . (((hooks . (((type . "command")
                                  (command . "~/.claude/hooks/subagent-stop-notify.sh")))))))
     (UserPromptSubmit . (((hooks . (((type . "command")
                                      (command . "~/.claude/hooks/prompt-submit-notify.sh")))))))
     (SessionStart . (((hooks . (((type . "command")
                                  (command . "~/.claude/hooks/session-start-notify.sh")))))))
     (Notification . (((matcher . "permission_prompt")
                       (hooks . (((type . "command")
                                  (command . "~/.claude/hooks/permission-notify.sh")))))))
     (PermissionRequest . (((hooks . (((type . "command")
                                       (command . "~/.claude/hooks/permission-request-notify.sh"))))))))))

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

(ert-deftest agent-repl-test-in-sandbox-dockerenv ()
  "in-sandbox-p returns t when /.dockerenv exists."
  (cl-letf (((symbol-function 'file-exists-p)
             (lambda (path) (equal path "/.dockerenv")))
            ((symbol-function 'getenv) (lambda (_) nil)))
    (should (agent-repl--in-sandbox-p))))

(ert-deftest agent-repl-test-in-sandbox-env-var ()
  "in-sandbox-p returns t when DOOM_SANDBOX=1."
  (cl-letf (((symbol-function 'file-exists-p) (lambda (_) nil))
            ((symbol-function 'getenv)
             (lambda (k) (and (equal k "DOOM_SANDBOX") "1"))))
    (should (agent-repl--in-sandbox-p))))

(ert-deftest agent-repl-test-in-sandbox-neither ()
  "in-sandbox-p returns nil when neither signal is present."
  (cl-letf (((symbol-function 'file-exists-p) (lambda (_) nil))
            ((symbol-function 'getenv) (lambda (_) nil)))
    (should-not (agent-repl--in-sandbox-p))))

(ert-deftest agent-repl-test-in-sandbox-env-other-value ()
  "DOOM_SANDBOX set to a non-1 value does NOT trigger sandbox mode."
  (cl-letf (((symbol-function 'file-exists-p) (lambda (_) nil))
            ((symbol-function 'getenv)
             (lambda (k) (and (equal k "DOOM_SANDBOX") "0"))))
    (should-not (agent-repl--in-sandbox-p))))

;;;; ---- event-has-command-p ----

(ert-deftest agent-repl-test-event-has-command-match ()
  "Returns non-nil when CMD appears in an entry's inner .hooks[].command."
  (let ((hooks (cdr (assq 'hooks (test-install--json-with-all-hooks)))))
    (should (agent-repl--event-has-command-p
             hooks 'Stop "~/.claude/hooks/stop-notify.sh"))))

(ert-deftest agent-repl-test-event-has-command-mismatch ()
  "Returns nil when CMD differs from any registered entry."
  (let ((hooks (cdr (assq 'hooks (test-install--json-with-all-hooks)))))
    (should-not (agent-repl--event-has-command-p
                 hooks 'Stop "/nowhere.sh"))))

(ert-deftest agent-repl-test-event-has-command-event-absent ()
  "Returns nil when the event is not present in the hooks alist."
  (let ((hooks (cdr (assq 'hooks (test-install--json-missing-one 'Stop)))))
    (should-not (agent-repl--event-has-command-p
                 hooks 'Stop "~/.claude/hooks/stop-notify.sh"))))

(ert-deftest agent-repl-test-event-has-command-foreign-coresident ()
  "Matches even when foreign entries co-exist in the event array."
  (let ((hooks (cdr (assq 'hooks (test-install--json-with-foreign-stop)))))
    (should (agent-repl--event-has-command-p
             hooks 'Stop "~/.claude/hooks/stop-notify.sh"))
    (should (agent-repl--event-has-command-p
             hooks 'Stop "/some/foreign.sh"))))

;;;; ---- hooks-installed-p ----

(ert-deftest agent-repl-test-hooks-installed-all-present ()
  "Returns t when all four managed hooks are registered."
  (cl-letf (((symbol-function 'agent-repl--settings-json)
             #'test-install--json-with-all-hooks))
    (should (agent-repl--hooks-installed-p))))

(ert-deftest agent-repl-test-hooks-installed-missing-stop ()
  "Returns nil when Stop is absent."
  (cl-letf (((symbol-function 'agent-repl--settings-json)
             (lambda () (test-install--json-missing-one 'Stop))))
    (should-not (agent-repl--hooks-installed-p))))

(ert-deftest agent-repl-test-hooks-installed-missing-session-start ()
  "Returns nil when SessionStart is absent (post-e3d7451 dealbreaker)."
  (cl-letf (((symbol-function 'agent-repl--settings-json)
             (lambda () (test-install--json-missing-one 'SessionStart))))
    (should-not (agent-repl--hooks-installed-p))))

(ert-deftest agent-repl-test-hooks-installed-missing-notification ()
  "Returns nil when the permission Notification hook is absent."
  (cl-letf (((symbol-function 'agent-repl--settings-json)
             (lambda () (test-install--json-missing-one 'Notification))))
    (should-not (agent-repl--hooks-installed-p))))

(ert-deftest agent-repl-test-hooks-installed-missing-permission-request ()
  "Returns nil when the PermissionRequest hook is absent.
PermissionRequest is the real-time permission-dialog signal that drives
the tab to `:permission' WHILE the agent is waiting on the user.  Its
absence falls back to the lagging Notification permission_prompt path,
so doctor must surface the missing registration."
  (cl-letf (((symbol-function 'agent-repl--settings-json)
             (lambda () (test-install--json-missing-one 'PermissionRequest))))
    (should-not (agent-repl--hooks-installed-p))))

(ert-deftest agent-repl-test-managed-hooks-includes-permission-request ()
  "The managed-hooks alist must include PermissionRequest pointing at our script.
This is the real-time signal that flips the tab to `:permission' the
moment the permission dialog appears, so a regression that drops the
entry would silently revert to the lagging Notification path."
  (should (equal (cdr (assq 'PermissionRequest agent-repl--managed-hooks))
                 "~/.claude/hooks/permission-request-notify.sh")))

(ert-deftest agent-repl-test-hooks-installed-settings-absent ()
  "Returns nil when settings.json cannot be read."
  (cl-letf (((symbol-function 'agent-repl--settings-json) (lambda () nil)))
    (should-not (agent-repl--hooks-installed-p))))

(ert-deftest agent-repl-test-hooks-installed-foreign-coresident ()
  "Returns t even when foreign hooks co-exist under our event keys."
  (cl-letf (((symbol-function 'agent-repl--settings-json)
             #'test-install--json-with-foreign-stop))
    (should (agent-repl--hooks-installed-p))))

;;;; ---- run-install-action dispatch ----

(ert-deftest agent-repl-test-install-action-sandbox-noop ()
  "install/uninstall/reinstall no-op when sandbox is detected."
  (let ((called nil))
    (cl-letf (((symbol-function 'agent-repl--in-sandbox-p) (lambda () t))
              ((symbol-function 'agent-repl--run-install-script)
               (lambda (&rest _) (setq called t) '(0 ""))))
      (agent-repl--run-install-action "install")
      (should-not called))))

(ert-deftest agent-repl-test-install-action-passes-through ()
  "When not in sandbox, the action arg is forwarded to the script."
  (let ((received-arg nil))
    (cl-letf (((symbol-function 'agent-repl--in-sandbox-p) (lambda () nil))
              ((symbol-function 'agent-repl--run-install-script)
               (lambda (action) (setq received-arg action) '(0 "ok\n")))
              ((symbol-function 'agent-repl--surface-install-output)
               (lambda (_) nil)))
      (agent-repl--run-install-action "reinstall")
      (should (equal received-arg "reinstall")))))

(ert-deftest agent-repl-test-install-action-nonzero-exit-errors ()
  "Non-zero exit surfaces the output buffer and signals an error."
  (let ((buffer-shown nil))
    (cl-letf (((symbol-function 'agent-repl--in-sandbox-p) (lambda () nil))
              ((symbol-function 'agent-repl--run-install-script)
               (lambda (_) '(2 "boom\n")))
              ((symbol-function 'agent-repl--surface-install-output)
               (lambda (_) nil))
              ((symbol-function 'display-buffer)
               (lambda (_) (setq buffer-shown t))))
      (should-error (agent-repl--run-install-action "install"))
      (should buffer-shown))))

(ert-deftest agent-repl-test-install-action-quiet-nonzero-no-window ()
  "Quiet non-zero exit does NOT pop the output window."
  (let ((buffer-shown nil))
    (cl-letf (((symbol-function 'agent-repl--in-sandbox-p) (lambda () nil))
              ((symbol-function 'agent-repl--run-install-script)
               (lambda (_) '(2 "boom\n")))
              ((symbol-function 'agent-repl--surface-install-output)
               (lambda (_) nil))
              ((symbol-function 'agent-repl--log) (lambda (&rest _) nil))
              ((symbol-function 'display-buffer)
               (lambda (&rest _) (setq buffer-shown t))))
      (should-error (agent-repl--run-install-action "install" t))
      (should-not buffer-shown))))

(ert-deftest agent-repl-test-install-action-quiet-nonzero-logs ()
  "Quiet non-zero exit routes the script output to `agent-repl--log'."
  (let ((logged nil))
    (cl-letf (((symbol-function 'agent-repl--in-sandbox-p) (lambda () nil))
              ((symbol-function 'agent-repl--run-install-script)
               (lambda (_) '(2 "boom\n")))
              ((symbol-function 'agent-repl--surface-install-output)
               (lambda (_) nil))
              ((symbol-function 'agent-repl--log)
               (lambda (&rest args) (setq logged args))))
      (should-error (agent-repl--run-install-action "install" t))
      (should logged))))

(ert-deftest agent-repl-test-install-action-quiet-nonzero-still-errors ()
  "Quiet non-zero exit still signals an error so surfacing is preserved."
  (cl-letf (((symbol-function 'agent-repl--in-sandbox-p) (lambda () nil))
            ((symbol-function 'agent-repl--run-install-script)
             (lambda (_) '(2 "boom\n")))
            ((symbol-function 'agent-repl--surface-install-output)
             (lambda (_) nil))
            ((symbol-function 'agent-repl--log) (lambda (&rest _) nil)))
    (should-error (agent-repl--run-install-action "install" t))))

(ert-deftest agent-repl-test-install-action-surface-output ()
  "Zero-exit run pipes script output to the output buffer."
  (cl-letf (((symbol-function 'agent-repl--in-sandbox-p) (lambda () nil))
            ((symbol-function 'agent-repl--run-install-script)
             (lambda (_) '(0 "hello from script\n"))))
    (agent-repl--run-install-action "install")
    (with-current-buffer agent-repl--install-output-buffer
      (should (string-match-p "hello from script"
                              (buffer-substring-no-properties
                               (point-min) (point-max)))))))

;;;; ---- Interactive commands ----

(ert-deftest agent-repl-test-install-hooks-calls-install ()
  "`agent-repl-install-hooks' dispatches the `install' action."
  (let ((action nil))
    (cl-letf (((symbol-function 'agent-repl--run-install-action)
               (lambda (a) (setq action a))))
      (agent-repl-install-hooks)
      (should (equal action "install")))))

(ert-deftest agent-repl-test-uninstall-hooks-calls-uninstall ()
  "`agent-repl-uninstall-hooks' dispatches the `uninstall' action."
  (let ((action nil))
    (cl-letf (((symbol-function 'agent-repl--run-install-action)
               (lambda (a) (setq action a))))
      (agent-repl-uninstall-hooks)
      (should (equal action "uninstall")))))

(ert-deftest agent-repl-test-reinstall-hooks-calls-reinstall ()
  "`agent-repl-reinstall-hooks' dispatches the `reinstall' action."
  (let ((action nil))
    (cl-letf (((symbol-function 'agent-repl--run-install-action)
               (lambda (a) (setq action a))))
      (agent-repl-reinstall-hooks)
      (should (equal action "reinstall")))))

;;;; ---- Script location sanity ----

(ert-deftest agent-repl-test-install-script-path-exists ()
  "The resolved install-script path points at the checked-in .claude/install.sh."
  (should (file-exists-p agent-repl--install-script))
  (should (string-match-p "/\\.claude/install\\.sh\\'"
                          agent-repl--install-script)))

;;;; ---- doctor-issues ----

(defun test-install--doctor-level-count (issues level)
  "Return the number of entries in ISSUES whose level is LEVEL."
  (cl-count-if (lambda (i) (eq (car i) level)) issues))

(defun test-install--doctor-find (issues substring)
  "Return the first issue whose message contains SUBSTRING, else nil."
  (cl-find-if (lambda (i) (string-match-p substring (cdr i))) issues))

(ert-deftest agent-repl-test-doctor-sandbox-returns-nil ()
  "In sandbox, doctor-issues is a no-op."
  (cl-letf (((symbol-function 'agent-repl--in-sandbox-p) (lambda () t)))
    (should-not (agent-repl--doctor-issues))))

(ert-deftest agent-repl-test-doctor-missing-settings ()
  "Settings missing or unreadable produces a single top-level error."
  (cl-letf (((symbol-function 'agent-repl--in-sandbox-p) (lambda () nil))
            ((symbol-function 'agent-repl--settings-json) (lambda () nil))
            ((symbol-function 'agent-repl--check-skill-links)
             (lambda (_issues) nil))
            ((symbol-function 'agent-repl--check-unmanaged-broken-links)
             (lambda (_issues) nil)))
    (let ((issues (agent-repl--doctor-issues)))
      (should (= 1 (length issues)))
      (should (eq 'error (car (car issues))))
      (should (string-match-p "missing or unreadable"
                              (cdr (car issues)))))))

(ert-deftest agent-repl-test-doctor-all-present ()
  "With all hooks registered and scripts on disk matching source, no issues."
  (cl-letf (((symbol-function 'agent-repl--in-sandbox-p) (lambda () nil))
            ((symbol-function 'agent-repl--settings-json)
             #'test-install--json-with-all-hooks)
            ((symbol-function 'agent-repl--check-script-files)
             (lambda (_issues) nil))
            ((symbol-function 'agent-repl--check-skill-links)
             (lambda (_issues) nil))
            ((symbol-function 'agent-repl--check-unmanaged-broken-links)
             (lambda (_issues) nil)))
    (should-not (agent-repl--doctor-issues))))

(ert-deftest agent-repl-test-doctor-missing-stop-errors ()
  "Missing Stop registration is error-level (workspaces stuck in :thinking)."
  (cl-letf (((symbol-function 'agent-repl--in-sandbox-p) (lambda () nil))
            ((symbol-function 'agent-repl--settings-json)
             (lambda () (test-install--json-missing-one 'Stop)))
            ((symbol-function 'agent-repl--check-script-files)
             (lambda (_issues) nil)))
    (let ((issues (agent-repl--doctor-issues)))
      (should (test-install--doctor-find issues "Stop hook not registered"))
      (should (eq 'error
                  (car (test-install--doctor-find
                        issues "Stop hook not registered")))))))

(ert-deftest agent-repl-test-doctor-missing-session-start-errors ()
  "Missing SessionStart is error-level (readiness never fires, placeholder stuck)."
  (cl-letf (((symbol-function 'agent-repl--in-sandbox-p) (lambda () nil))
            ((symbol-function 'agent-repl--settings-json)
             (lambda () (test-install--json-missing-one 'SessionStart)))
            ((symbol-function 'agent-repl--check-script-files)
             (lambda (_issues) nil)))
    (let ((issues (agent-repl--doctor-issues)))
      (should (test-install--doctor-find issues "SessionStart"))
      (should (eq 'error
                  (car (test-install--doctor-find issues "SessionStart")))))))

(ert-deftest agent-repl-test-doctor-missing-user-prompt-submit-warns ()
  "Missing UserPromptSubmit is warn-level (Emacs do-send still sets :thinking)."
  (cl-letf (((symbol-function 'agent-repl--in-sandbox-p) (lambda () nil))
            ((symbol-function 'agent-repl--settings-json)
             (lambda () (test-install--json-missing-one 'UserPromptSubmit)))
            ((symbol-function 'agent-repl--check-script-files)
             (lambda (_issues) nil)))
    (let ((issues (agent-repl--doctor-issues)))
      (should (test-install--doctor-find issues "UserPromptSubmit"))
      (should (eq 'warn
                  (car (test-install--doctor-find issues "UserPromptSubmit")))))))

(ert-deftest agent-repl-test-doctor-missing-notification-warns ()
  "Missing Notification is warn-level (❓ badge absent but module works)."
  (cl-letf (((symbol-function 'agent-repl--in-sandbox-p) (lambda () nil))
            ((symbol-function 'agent-repl--settings-json)
             (lambda () (test-install--json-missing-one 'Notification)))
            ((symbol-function 'agent-repl--check-script-files)
             (lambda (_issues) nil)))
    (let ((issues (agent-repl--doctor-issues)))
      (should (test-install--doctor-find issues "Notification"))
      (should (eq 'warn
                  (car (test-install--doctor-find issues "Notification")))))))

(ert-deftest agent-repl-test-doctor-missing-stop-failure-warns ()
  "Missing StopFailure is warn-level (the :stop-failed state never appears,
but the core REPL loop still works)."
  (cl-letf (((symbol-function 'agent-repl--in-sandbox-p) (lambda () nil))
            ((symbol-function 'agent-repl--settings-json)
             (lambda () (test-install--json-missing-one 'StopFailure)))
            ((symbol-function 'agent-repl--check-script-files)
             (lambda (_issues) nil)))
    (let ((issues (agent-repl--doctor-issues)))
      (should (test-install--doctor-find issues "StopFailure"))
      (should (eq 'warn
                  (car (test-install--doctor-find issues "StopFailure")))))))

(ert-deftest agent-repl-test-doctor-missing-subagent-start-warns ()
  "Missing SubagentStart is warn-level (Stop gating reduces to legacy
behavior — Stop transitions immediately even with subagents in flight)."
  (cl-letf (((symbol-function 'agent-repl--in-sandbox-p) (lambda () nil))
            ((symbol-function 'agent-repl--settings-json)
             (lambda () (test-install--json-missing-one 'SubagentStart)))
            ((symbol-function 'agent-repl--check-script-files)
             (lambda (_issues) nil)))
    (let ((issues (agent-repl--doctor-issues)))
      (should (test-install--doctor-find issues "SubagentStart"))
      (should (eq 'warn
                  (car (test-install--doctor-find issues "SubagentStart")))))))

(ert-deftest agent-repl-test-doctor-missing-subagent-stop-warns ()
  "Missing SubagentStop is warn-level (counter would never decrement, so
in practice Stop would still drive transitions correctly when there are
no SubagentStart fires either; degraded but not broken)."
  (cl-letf (((symbol-function 'agent-repl--in-sandbox-p) (lambda () nil))
            ((symbol-function 'agent-repl--settings-json)
             (lambda () (test-install--json-missing-one 'SubagentStop)))
            ((symbol-function 'agent-repl--check-script-files)
             (lambda (_issues) nil)))
    (let ((issues (agent-repl--doctor-issues)))
      (should (test-install--doctor-find issues "SubagentStop"))
      (should (eq 'warn
                  (car (test-install--doctor-find issues "SubagentStop")))))))

(ert-deftest agent-repl-test-doctor-settings-skip-short-circuits-script-checks ()
  "When settings.json is missing, script-file checks are not performed."
  (let ((script-checks-called nil))
    (cl-letf (((symbol-function 'agent-repl--in-sandbox-p) (lambda () nil))
              ((symbol-function 'agent-repl--settings-json) (lambda () nil))
              ((symbol-function 'agent-repl--check-script-files)
               (lambda (_) (setq script-checks-called t))))
      (agent-repl--doctor-issues)
      (should-not script-checks-called))))

;; Script-file checks — use tmpfile fixtures so the real file predicates work.

(defun test-install--with-fake-hooks-dir (contents body-fn)
  "Run BODY-FN with agent-repl--hooks-dest-dir rebound to a tmpdir.
CONTENTS is an alist ((SCRIPT-NAME . (MODE . TEXT)) ...): writes each
script with the given unix MODE permissions and TEXT content.  A
SCRIPT-NAME with `:missing' means leave that script absent."
  (let ((tmpdir (file-name-as-directory
                 (make-temp-file "agent-repl-doctor-" t))))
    (unwind-protect
        (progn
          (dolist (entry contents)
            (let ((name (car entry))
                  (mode-content (cdr entry)))
              (unless (eq mode-content :missing)
                (let ((path (expand-file-name name tmpdir)))
                  (with-temp-file path
                    (insert (cdr mode-content)))
                  (set-file-modes path (car mode-content))))))
          (cl-letf (((symbol-function 'agent-repl--installed-script-path)
                     (lambda (cmd)
                       (expand-file-name
                        (file-name-nondirectory cmd) tmpdir))))
            (funcall body-fn tmpdir)))
      (delete-directory tmpdir t))))

(ert-deftest agent-repl-test-doctor-script-missing-errors ()
  "A missing installed script emits an error-level issue."
  (test-install--with-fake-hooks-dir
   `(("stop-notify.sh" . :missing)
     ("stop-failure-notify.sh" . (#o755 . "x"))
     ("subagent-start-notify.sh" . (#o755 . "x"))
     ("subagent-stop-notify.sh" . (#o755 . "x"))
     ("prompt-submit-notify.sh" . (#o755 . "x"))
     ("session-start-notify.sh" . (#o755 . "x"))
     ("permission-notify.sh" . (#o755 . "x"))
     ("permission-request-notify.sh" . (#o755 . "x")))
   (lambda (_tmp)
     (cl-letf (((symbol-function 'agent-repl--in-sandbox-p) (lambda () nil))
               ((symbol-function 'agent-repl--settings-json)
                #'test-install--json-with-all-hooks)
               ((symbol-function 'agent-repl--script-drift-p)
                (lambda (_) nil)))
       (let* ((issues (agent-repl--doctor-issues))
              (found (test-install--doctor-find
                      issues "missing:.*stop-notify\\.sh")))
         (should found)
         (should (eq 'error (car found))))))))

(ert-deftest agent-repl-test-doctor-script-not-executable-errors ()
  "A non-executable installed script emits an error-level issue."
  (test-install--with-fake-hooks-dir
   `(("stop-notify.sh" . (#o644 . "x"))
     ("stop-failure-notify.sh" . (#o755 . "x"))
     ("subagent-start-notify.sh" . (#o755 . "x"))
     ("subagent-stop-notify.sh" . (#o755 . "x"))
     ("prompt-submit-notify.sh" . (#o755 . "x"))
     ("session-start-notify.sh" . (#o755 . "x"))
     ("permission-notify.sh" . (#o755 . "x"))
     ("permission-request-notify.sh" . (#o755 . "x")))
   (lambda (_tmp)
     (cl-letf (((symbol-function 'agent-repl--in-sandbox-p) (lambda () nil))
               ((symbol-function 'agent-repl--settings-json)
                #'test-install--json-with-all-hooks)
               ((symbol-function 'agent-repl--script-drift-p)
                (lambda (_) nil)))
       (let* ((issues (agent-repl--doctor-issues))
              (found (test-install--doctor-find issues "not executable")))
         (should found)
         (should (eq 'error (car found))))))))

(ert-deftest agent-repl-test-doctor-script-drift-warns ()
  "A drifted installed script emits a warn-level issue."
  (test-install--with-fake-hooks-dir
   `(("stop-notify.sh" . (#o755 . "x"))
     ("stop-failure-notify.sh" . (#o755 . "x"))
     ("subagent-start-notify.sh" . (#o755 . "x"))
     ("subagent-stop-notify.sh" . (#o755 . "x"))
     ("prompt-submit-notify.sh" . (#o755 . "x"))
     ("session-start-notify.sh" . (#o755 . "x"))
     ("permission-notify.sh" . (#o755 . "x"))
     ("permission-request-notify.sh" . (#o755 . "x")))
   (lambda (_tmp)
     (cl-letf (((symbol-function 'agent-repl--in-sandbox-p) (lambda () nil))
               ((symbol-function 'agent-repl--settings-json)
                #'test-install--json-with-all-hooks)
               ((symbol-function 'agent-repl--script-drift-p)
                (lambda (cmd)
                  (equal (file-name-nondirectory cmd)
                         "stop-notify.sh"))))
       (let* ((issues (agent-repl--doctor-issues))
              (found (test-install--doctor-find issues "drift")))
         (should found)
         (should (eq 'warn (car found))))))))

(ert-deftest agent-repl-test-script-drift-p-match ()
  "script-drift-p is nil when installed bytes match source."
  (let* ((src (make-temp-file "crd-src-")) (dest (make-temp-file "crd-dst-")))
    (unwind-protect
        (progn
          (with-temp-file src (insert "identical"))
          (with-temp-file dest (insert "identical"))
          (cl-letf (((symbol-function 'agent-repl--installed-script-path)
                     (lambda (_) dest))
                    ((symbol-function 'agent-repl--source-script-path)
                     (lambda (_) src)))
            (should-not
             (agent-repl--script-drift-p "~/.claude/hooks/anything.sh"))))
      (delete-file src) (delete-file dest))))

(ert-deftest agent-repl-test-script-drift-p-mismatch ()
  "script-drift-p is non-nil when installed bytes differ from source."
  (let* ((src (make-temp-file "crd-src-")) (dest (make-temp-file "crd-dst-")))
    (unwind-protect
        (progn
          (with-temp-file src (insert "source-content"))
          (with-temp-file dest (insert "tampered-content"))
          (cl-letf (((symbol-function 'agent-repl--installed-script-path)
                     (lambda (_) dest))
                    ((symbol-function 'agent-repl--source-script-path)
                     (lambda (_) src)))
            (should (agent-repl--script-drift-p
                     "~/.claude/hooks/anything.sh"))))
      (delete-file src) (delete-file dest))))

;;;; ---- Bash-integration tests: intentionally not present ----
;;
;; Earlier revisions of this file invoked `bash .claude/install.sh' via
;; `call-process' under an isolated HOME tmpdir to assert end-to-end
;; installer behavior (fresh install, idempotency, foreign-entry
;; preservation, uninstall, sandbox short-circuit, etc.).
;;
;; Those tests have been removed because per AGENTS.md "No External
;; Processes or External State in Tests" the ERT suite covers ELISP
;; code only.  When the production artifact under test is a non-elisp
;; binary (a shell script, here), there is nothing meaningful for an
;; elisp test to assert without invoking that binary — at which point
;; the test is an integration test of an external program, not a unit
;; test of lisp code.  AGENTS.md explicitly disallows integration
;; tests inside the ERT suite.
;;
;; The bash installer is still covered, but outside ERT: install.sh's
;; behavior is exercised manually and by the install/uninstall flow
;; in any project that consumes the hook.  If automated coverage is
;; needed later, it lives in a dedicated CI job invoked separately
;; from the ERT batch run.

;;;; ---- maybe-install-hooks ----

(ert-deftest agent-repl-test-maybe-install-runs-when-issues ()
  "maybe-install-hooks invokes install when doctor reports issues."
  (let ((called nil))
    (cl-letf (((symbol-function 'agent-repl--in-sandbox-p) (lambda () nil))
              ((symbol-function 'agent-repl--doctor-issues)
               (lambda () '((error . "missing"))))
              ((symbol-function 'agent-repl--run-install-action)
               (lambda (&rest _) (setq called t)))
              (noninteractive nil)
              (agent-repl-auto-install-hooks t))
      (agent-repl--maybe-install-hooks)
      (should called))))

(ert-deftest agent-repl-test-maybe-install-passes-quiet ()
  "maybe-install-hooks dispatches the install action with QUIET non-nil.
The quiet flag is what routes a failed auto-install to the log instead of
popping the `*agent-repl-install*' window on every reload."
  (let ((received-args nil))
    (cl-letf (((symbol-function 'agent-repl--in-sandbox-p) (lambda () nil))
              ((symbol-function 'agent-repl--doctor-issues)
               (lambda () '((error . "missing"))))
              ((symbol-function 'agent-repl--run-install-action)
               (lambda (&rest args) (setq received-args args)))
              (noninteractive nil)
              (agent-repl-auto-install-hooks t))
      (agent-repl--maybe-install-hooks)
      (should (equal (nth 0 received-args) "install"))
      (should (nth 1 received-args)))))

(ert-deftest agent-repl-test-maybe-install-skips-when-clean ()
  "maybe-install-hooks no-ops when doctor reports no issues."
  (let ((called nil))
    (cl-letf (((symbol-function 'agent-repl--in-sandbox-p) (lambda () nil))
              ((symbol-function 'agent-repl--doctor-issues) (lambda () nil))
              ((symbol-function 'agent-repl--run-install-action)
               (lambda (&rest _) (setq called t)))
              (noninteractive nil)
              (agent-repl-auto-install-hooks t))
      (agent-repl--maybe-install-hooks)
      (should-not called))))

(ert-deftest agent-repl-test-maybe-install-skips-in-sandbox ()
  "maybe-install-hooks no-ops in sandbox even when issues would be reported."
  (let ((called nil))
    (cl-letf (((symbol-function 'agent-repl--in-sandbox-p) (lambda () t))
              ((symbol-function 'agent-repl--doctor-issues)
               (lambda () '((error . "missing"))))
              ((symbol-function 'agent-repl--run-install-action)
               (lambda (&rest _) (setq called t)))
              (noninteractive nil)
              (agent-repl-auto-install-hooks t))
      (agent-repl--maybe-install-hooks)
      (should-not called))))

(ert-deftest agent-repl-test-maybe-install-skips-when-disabled ()
  "maybe-install-hooks no-ops when the custom flag is nil."
  (let ((called nil))
    (cl-letf (((symbol-function 'agent-repl--in-sandbox-p) (lambda () nil))
              ((symbol-function 'agent-repl--doctor-issues)
               (lambda () '((error . "missing"))))
              ((symbol-function 'agent-repl--run-install-action)
               (lambda (&rest _) (setq called t)))
              (noninteractive nil)
              (agent-repl-auto-install-hooks nil))
      (agent-repl--maybe-install-hooks)
      (should-not called))))

(ert-deftest agent-repl-test-maybe-install-swallows-error ()
  "Errors from the install action are caught, not propagated to startup."
  (cl-letf (((symbol-function 'agent-repl--in-sandbox-p) (lambda () nil))
            ((symbol-function 'agent-repl--doctor-issues)
             (lambda () '((error . "missing"))))
            ((symbol-function 'agent-repl--run-install-action)
             (lambda (&rest _) (error "boom")))
            ((symbol-function 'agent-repl--log) (lambda (&rest _) nil))
            (noninteractive nil)
            (agent-repl-auto-install-hooks t))
    ;; Must not propagate the error — return value is irrelevant.
    (agent-repl--maybe-install-hooks)
    (should t)))

(ert-deftest agent-repl-test-maybe-install-logs-caught-error ()
  "A caught install-action error is surfaced via `agent-repl--log'."
  (let ((logged nil))
    (cl-letf (((symbol-function 'agent-repl--in-sandbox-p) (lambda () nil))
              ((symbol-function 'agent-repl--doctor-issues)
               (lambda () '((error . "missing"))))
              ((symbol-function 'agent-repl--run-install-action)
               (lambda (&rest _) (error "boom")))
              ((symbol-function 'agent-repl--log)
               (lambda (&rest _) (setq logged t)))
              (noninteractive nil)
              (agent-repl-auto-install-hooks t))
      (agent-repl--maybe-install-hooks)
      (should logged))))

(ert-deftest agent-repl-test-maybe-install-not-on-startup-hook ()
  "Auto-install must NOT be deferred to `emacs-startup-hook' — later
agent-repl sub-modules depend on hooks being registered at load time."
  (should-not (memq 'agent-repl--maybe-install-hooks emacs-startup-hook)))

;;;; ---- skill-link helpers ----

(defun test-install--make-skills-tmp ()
  "Build a temp-dir trio (SRC LOCAL-SRC . DEST) for skill-link tests.
Creates SRC/<managed-skill-names> and LOCAL-SRC/<managed-local-skill-names>
as real directories so the symlink targets resolve, and DEST as an empty
directory ready for links."
  (let* ((root (make-temp-file "agent-repl-skill-test-" t))
         (src (expand-file-name "src/" root))
         (local-src (expand-file-name "local-src/" root))
         (dest (expand-file-name "dest/" root)))
    (make-directory src t)
    (make-directory local-src t)
    (make-directory dest t)
    (dolist (name agent-repl--managed-skills)
      (make-directory (expand-file-name name src) t))
    (dolist (name agent-repl--managed-local-skills)
      (make-directory (expand-file-name name local-src) t))
    (list src local-src dest)))

(defmacro test-install--with-skill-dirs (bindings &rest body)
  "Run BODY with SRC/LOCAL-SRC/DEST dirs materialized and defcustoms pointed at them.
BINDINGS is ignored — provided so future test helpers can extend."
  (declare (indent 1))
  (ignore bindings)
  `(let* ((trio (test-install--make-skills-tmp))
          (src (nth 0 trio))
          (local-src (nth 1 trio))
          (dest (nth 2 trio)))
     (let ((agent-repl-skills-src-dir src)
           (agent-repl-local-skills-src-dir local-src)
           (agent-repl--skills-dest-dir dest))
       (unwind-protect (progn ,@body)
         (delete-directory (file-name-directory (directory-file-name src)) t)))))

(defun test-install--link-all-skills ()
  "Create symlinks at the test DEST for every managed (external + local) skill.
Use after `test-install--with-skill-dirs' has set up the temp dirs and
the defcustoms.  Returns the list of dests created."
  (let (created)
    (dolist (name agent-repl--managed-skills)
      (let ((d (agent-repl--skill-dest-path name)))
        (make-symbolic-link (agent-repl--skill-src-path name) d)
        (push d created)))
    (dolist (name agent-repl--managed-local-skills)
      (let ((d (agent-repl--skill-dest-path name)))
        (make-symbolic-link
         (agent-repl--skill-src-path name agent-repl-local-skills-src-dir)
         d)
        (push d created)))
    (nreverse created)))

(ert-deftest agent-repl-test-skill-link-ok-correct ()
  "skill-link-ok-p returns t when dest is a symlink to the expected src."
  (test-install--with-skill-dirs ()
    (let ((name (car agent-repl--managed-skills)))
      (make-symbolic-link (agent-repl--skill-src-path name)
                          (agent-repl--skill-dest-path name))
      (should (agent-repl--skill-link-ok-p name)))))

(ert-deftest agent-repl-test-skill-link-ok-missing ()
  "skill-link-ok-p returns nil when dest does not exist at all."
  (test-install--with-skill-dirs ()
    (should-not
     (agent-repl--skill-link-ok-p (car agent-repl--managed-skills)))))

(ert-deftest agent-repl-test-skill-link-ok-foreign-target ()
  "skill-link-ok-p returns nil when dest points at something other than our src."
  (test-install--with-skill-dirs ()
    (let* ((name (car agent-repl--managed-skills))
           (dest (agent-repl--skill-dest-path name)))
      (make-symbolic-link "/tmp/elsewhere" dest)
      (should-not (agent-repl--skill-link-ok-p name)))))

(ert-deftest agent-repl-test-check-skill-links-missing ()
  "Missing symlinks produce one warn per managed skill (external + local)."
  (test-install--with-skill-dirs ()
    (let ((issues (list nil))
          (expected (+ (length agent-repl--managed-skills)
                       (length agent-repl--managed-local-skills))))
      (agent-repl--check-skill-links issues)
      (should (= (length (car issues)) expected))
      (should (cl-every (lambda (i) (eq (car i) 'warn)) (car issues))))))

(ert-deftest agent-repl-test-check-skill-links-all-ok ()
  "All skills linked correctly (external + local) produces no issues."
  (test-install--with-skill-dirs ()
    (test-install--link-all-skills)
    (let ((issues (list nil)))
      (agent-repl--check-skill-links issues)
      (should (null (car issues))))))

(ert-deftest agent-repl-test-check-skill-links-foreign ()
  "A foreign file at one dest path is flagged as `points elsewhere'."
  (test-install--with-skill-dirs ()
    (let ((name (car agent-repl--managed-skills)))
      (write-region "" nil (agent-repl--skill-dest-path name))
      (let ((issues (list nil)))
        (agent-repl--check-skill-links issues)
        (should (= 1 (length
                      (cl-remove-if-not
                       (lambda (i) (string-match-p "points elsewhere"
                                                    (cdr i)))
                       (car issues)))))))))

;;;; ---- local-skill specific tests ----

(ert-deftest agent-repl-test-managed-local-skills-nonempty ()
  "Repo-local skills list must include `debug-logs' (regression guard)."
  (should (member "debug-logs" agent-repl--managed-local-skills)))

(ert-deftest agent-repl-test-local-skills-src-dir-default ()
  "Default `agent-repl-local-skills-src-dir' points at this module's
checked-in `skills/' directory when install.el is loaded from a file."
  ;; The defcustom default is computed from `load-file-name'; when
  ;; install.el was loaded normally, the path should end in the
  ;; module-local `skills/' segment.
  (when agent-repl-local-skills-src-dir
    (should (string-match-p
             "modules/app/agent-repl/skills/?$"
             (directory-file-name
              (expand-file-name agent-repl-local-skills-src-dir))))))

(ert-deftest agent-repl-test-skill-link-ok-uses-local-src ()
  "skill-link-ok-p honors the SRC-DIR argument for local skills."
  (test-install--with-skill-dirs ()
    (let ((name (car agent-repl--managed-local-skills)))
      (make-symbolic-link
       (agent-repl--skill-src-path name agent-repl-local-skills-src-dir)
       (agent-repl--skill-dest-path name))
      ;; Correct when called with the local src-dir.
      (should (agent-repl--skill-link-ok-p
               name agent-repl-local-skills-src-dir))
      ;; Wrong when called with the external src-dir (different target).
      (should-not (agent-repl--skill-link-ok-p name)))))

(ert-deftest agent-repl-test-check-skill-links-local-missing ()
  "A missing local-skill symlink contributes its own warn entry."
  (test-install--with-skill-dirs ()
    ;; Link only the external skills; leave local-skill dests missing.
    (dolist (name agent-repl--managed-skills)
      (make-symbolic-link (agent-repl--skill-src-path name)
                          (agent-repl--skill-dest-path name)))
    (let ((issues (list nil)))
      (agent-repl--check-skill-links issues)
      (should (= (length (car issues))
                 (length agent-repl--managed-local-skills)))
      (should (cl-every (lambda (i)
                          (string-match-p "Skill symlink missing" (cdr i)))
                        (car issues))))))

(ert-deftest agent-repl-test-debug-logs-skill-file-exists ()
  "The checked-in debug-logs SKILL.md must exist with required frontmatter.
Regression guard so the file is not silently moved or deleted —
`/debug-logs' depends on it being discoverable at install time."
  (let* ((src-dir (expand-file-name
                   (or agent-repl-local-skills-src-dir
                       (error "agent-repl-local-skills-src-dir is unset"))))
         (skill-md (expand-file-name "debug-logs/SKILL.md" src-dir)))
    (should (file-exists-p skill-md))
    (with-temp-buffer
      (insert-file-contents skill-md)
      (goto-char (point-min))
      (should (looking-at "^---\n"))
      (should (re-search-forward "^name: debug-logs$" nil t))
      (should (re-search-forward "^description: " nil t)))))

(ert-deftest agent-repl-test-managed-local-skills-includes-runtime-eval-code ()
  "Repo-local skills list must include `runtime-eval-code' (regression guard).
`/runtime-eval-code' replaces the prior `/workspace-eval' skill and owns
the JSON contract that the editor's `\"eval\"' handler dispatches against."
  (should (member "runtime-eval-code" agent-repl--managed-local-skills)))

(ert-deftest agent-repl-test-managed-local-skills-includes-emit-workspace-commands ()
  "Repo-local skills list must include `emit-workspace-commands.sh' (regression guard).
explanation-engine folded the original into `workspace/run.sh --emit-commands'
(python3-dependent, absent from the doom-sandbox image), so the doom repo
now owns the trivial uuidgen-only emitter as a repo-local skill."
  (should (member "emit-workspace-commands.sh" agent-repl--managed-local-skills)))

(ert-deftest agent-repl-test-managed-skills-excludes-emit-workspace-commands ()
  "External managed-skills list must NOT include `emit-workspace-commands.sh'.
It moved to the repo-local list, so leaving it here would re-introduce the
dangling explanation-engine canonical-impl path that broke install."
  (should-not (member "emit-workspace-commands.sh" agent-repl--managed-skills)))

(ert-deftest agent-repl-test-managed-local-skills-includes-workspace-open ()
  "Repo-local skills list must include `workspace-open' (regression guard).
`/workspace-open' owns the JSON contract that the editor's `\"open\"'
handler dispatches against to re-establish a closed/nuked workspace."
  (should (member "workspace-open" agent-repl--managed-local-skills)))

(ert-deftest agent-repl-test-emit-workspace-commands-skill-file-exists ()
  "The checked-in repo-local emit-workspace-commands.sh must exist and be executable.
Regression guard so the dispatch emitter the skill run.sh wrappers exec
is not silently moved or deleted."
  (let ((emit (expand-file-name
               "emit-workspace-commands.sh"
               (or agent-repl-local-skills-src-dir
                   (error "agent-repl-local-skills-src-dir is unset")))))
    (should (file-exists-p emit))
    (should (file-executable-p emit))))

(ert-deftest agent-repl-test-runtime-eval-code-skill-file-exists ()
  "The checked-in runtime-eval-code SKILL.md must exist with required frontmatter.
Regression guard so the file is not silently moved or deleted —
`/runtime-eval-code' depends on it being discoverable at install time."
  (let* ((src-dir (expand-file-name
                   (or agent-repl-local-skills-src-dir
                       (error "agent-repl-local-skills-src-dir is unset"))))
         (skill-md (expand-file-name "runtime-eval-code/SKILL.md" src-dir)))
    (should (file-exists-p skill-md))
    (with-temp-buffer
      (insert-file-contents skill-md)
      (goto-char (point-min))
      (should (looking-at "^---\n"))
      (should (re-search-forward "^name: runtime-eval-code$" nil t))
      (should (re-search-forward "^description: " nil t)))))

(ert-deftest agent-repl-test-runtime-eval-code-run-sh-executable ()
  "The checked-in runtime-eval-code run.sh must exist and be executable.
Regression guard: SKILL.md Step 0 dispatches `run.sh resolve-ws' to obtain
the workspace routing key, and the dispatch step pipes JSON into the same
script. A non-executable or missing file breaks every dispatch."
  (let* ((src-dir (expand-file-name
                   (or agent-repl-local-skills-src-dir
                       (error "agent-repl-local-skills-src-dir is unset"))))
         (run-sh (expand-file-name "runtime-eval-code/run.sh" src-dir)))
    (should (file-exists-p run-sh))
    (should (file-executable-p run-sh))))

(ert-deftest agent-repl-test-workspace-open-skill-file-exists ()
  "The checked-in workspace-open SKILL.md must exist with required frontmatter.
Regression guard so the file is not silently moved or deleted —
`/workspace-open' depends on it being discoverable at install time."
  (let* ((src-dir (expand-file-name
                   (or agent-repl-local-skills-src-dir
                       (error "agent-repl-local-skills-src-dir is unset"))))
         (skill-md (expand-file-name "workspace-open/SKILL.md" src-dir)))
    (should (file-exists-p skill-md))
    (with-temp-buffer
      (insert-file-contents skill-md)
      (goto-char (point-min))
      (should (looking-at "^---\n"))
      (should (re-search-forward "^name: workspace-open$" nil t))
      (should (re-search-forward "^description: " nil t)))))

(ert-deftest agent-repl-test-workspace-open-run-sh-executable ()
  "The checked-in workspace-open run.sh must exist and be executable.
Regression guard: the SKILL.md dispatch step pipes JSON into run.sh; a
non-executable or missing file breaks every `/workspace-open' dispatch."
  (let* ((src-dir (expand-file-name
                   (or agent-repl-local-skills-src-dir
                       (error "agent-repl-local-skills-src-dir is unset"))))
         (run-sh (expand-file-name "workspace-open/run.sh" src-dir)))
    (should (file-exists-p run-sh))
    (should (file-executable-p run-sh))))

(ert-deftest agent-repl-test-runtime-eval-code-dispatch-resolves-sibling-through-symlinked-dir ()
  "`run.sh dispatch' must find `emit-workspace-commands.sh' even when its
install dir is a symlink into a worktree.
Regression guard for the exit-127 bug: the dispatch path used a literal
`..', which the kernel resolved THROUGH the symlinked skill dir into the
worktree (no sibling there).  This builds that exact layout and asserts
dispatch exits 0 (resolves the install-dir sibling), which fails under
the old `..' form and passes under the string-only `dirname' form."
  (skip-unless (executable-find "bash"))
  (skip-unless (executable-find "uuidgen"))
  (let* ((src-dir (expand-file-name
                   (or agent-repl-local-skills-src-dir
                       (error "agent-repl-local-skills-src-dir is unset"))))
         (checked-in-run-sh (expand-file-name "runtime-eval-code/run.sh" src-dir))
         (root (make-temp-file "agent-repl-dispatch-" t))
         ;; The "worktree" the skill dir really lives in; crucially its
         ;; PARENT has no emit-workspace-commands.sh, mirroring production.
         (worktree-rec (expand-file-name "worktree/runtime-eval-code" root))
         ;; The install layout where the sibling DOES exist.
         (install (expand-file-name "install" root))
         (install-link (expand-file-name "runtime-eval-code" install))
         (install-emit (expand-file-name "emit-workspace-commands.sh" install)))
    (unwind-protect
        (progn
          (make-directory worktree-rec t)
          (make-directory install t)
          (copy-file checked-in-run-sh
                     (expand-file-name "run.sh" worktree-rec) t nil nil t)
          (with-temp-file install-emit
            (insert "#!/usr/bin/env bash\nexit 0\n"))
          (set-file-modes install-emit #o755)
          (make-symbolic-link worktree-rec install-link t)
          (should (= 0 (call-process
                        "bash" nil nil nil
                        (expand-file-name "run.sh" install-link)
                        "dispatch"))))
      (delete-directory root t))))

(ert-deftest agent-repl-test-managed-local-skills-no-workspace-eval ()
  "The legacy `workspace-eval' skill name must NOT be present.
Regression guard: it was renamed/absorbed into `runtime-eval-code'.
Leaving the old name in the managed list would create a broken symlink
on install (no source directory matches it any more)."
  (should-not (member "workspace-eval" agent-repl--managed-local-skills)))

(ert-deftest agent-repl-test-managed-skills-includes-build-skill ()
  "External managed-skills list must include `build-skill' (regression guard).
The skill lives at `agent-repl-skills-src-dir'/build-skill on the host;
the doctor uses this list to verify the host symlink points at it."
  (should (member "build-skill" agent-repl--managed-skills)))

(ert-deftest agent-repl-test-managed-skills-includes-workspace ()
  "External managed-skills list must include the collapsed `workspace' skill.
The per-command `workspace-*' skills were folded into `/workspace', so
the doctor verifies the single `workspace' host symlink now."
  (should (member "workspace" agent-repl--managed-skills)))

(ert-deftest agent-repl-test-managed-skills-excludes-folded-workspace-skills ()
  "The folded per-command workspace skills must NOT be managed any more.
They were absorbed into `/workspace'; leaving any in the managed list
would make the doctor demand a host symlink whose impl no longer exists."
  (dolist (name '("workspace-merge" "workspace-status"
                  "workspace-update" "generate-workspace"))
    (should-not (member name agent-repl--managed-skills))))

;;;; ---- unmanaged broken symlink detection ----

(ert-deftest agent-repl-test-check-unmanaged-broken-link-detected ()
  "A broken symlink not in the managed set produces a warn-level issue."
  (test-install--with-skill-dirs ()
    ;; Create a broken symlink for a name we don't manage.
    (make-symbolic-link "/nonexistent/workspace-eval"
                        (expand-file-name "workspace-eval" dest))
    (let ((issues (list nil)))
      (agent-repl--check-unmanaged-broken-links issues)
      (should (= 1 (length (car issues))))
      (should (eq 'warn (caar (car issues))))
      (should (string-match-p "Unmanaged broken symlink"
                              (cdar (car issues)))))))

(ert-deftest agent-repl-test-check-unmanaged-broken-link-ignores-managed ()
  "A broken symlink for a managed skill name is NOT flagged by this check.
That case is already covered by `agent-repl--check-skill-links'."
  (test-install--with-skill-dirs ()
    ;; Create a broken symlink for a managed name.
    (make-symbolic-link "/nonexistent/target"
                        (expand-file-name (car agent-repl--managed-local-skills) dest))
    (let ((issues (list nil)))
      (agent-repl--check-unmanaged-broken-links issues)
      (should (null (car issues))))))

(ert-deftest agent-repl-test-check-unmanaged-broken-link-ignores-valid ()
  "A non-broken unmanaged symlink produces no issue."
  (test-install--with-skill-dirs ()
    ;; Create a valid symlink to a real directory.
    (let ((target (make-temp-file "real-target-" t)))
      (unwind-protect
          (progn
            (make-symbolic-link target
                                (expand-file-name "some-foreign-skill" dest))
            (let ((issues (list nil)))
              (agent-repl--check-unmanaged-broken-links issues)
              (should (null (car issues)))))
        (delete-directory target t)))))

(ert-deftest agent-repl-test-check-unmanaged-broken-link-empty-dir ()
  "No issues when the skills dest dir is empty."
  (test-install--with-skill-dirs ()
    (let ((issues (list nil)))
      (agent-repl--check-unmanaged-broken-links issues)
      (should (null (car issues))))))

(ert-deftest agent-repl-test-check-unmanaged-broken-link-nonexistent-dir ()
  "No issues when the skills dest dir does not exist."
  (let ((agent-repl--skills-dest-dir "/tmp/this-dir-should-not-exist-12345/"))
    (let ((issues (list nil)))
      (agent-repl--check-unmanaged-broken-links issues)
      (should (null (car issues))))))

(ert-deftest agent-repl-test-doctor-includes-unmanaged-broken-links ()
  "doctor-issues surfaces unmanaged broken symlinks alongside managed checks."
  (test-install--with-skill-dirs ()
    ;; Link all managed skills correctly so those checks pass.
    (test-install--link-all-skills)
    ;; Add an unmanaged broken symlink.
    (make-symbolic-link "/nonexistent/stale-skill"
                        (expand-file-name "stale-skill" dest))
    (cl-letf (((symbol-function 'agent-repl--in-sandbox-p) (lambda () nil))
              ((symbol-function 'agent-repl--settings-json)
               #'test-install--json-with-all-hooks)
              ((symbol-function 'agent-repl--check-script-files)
               (lambda (_issues) nil)))
      (let ((issues (agent-repl--doctor-issues)))
        (should (test-install--doctor-find issues "Unmanaged broken symlink"))))))

;;;; ---- Tests: alt-account config-dir provisioning ----

(defun test-install--all-hooks-registered-p (settings-file)
  "Return non-nil when every managed hook is registered in SETTINGS-FILE."
  (let* ((json (agent-repl--read-settings-alist (expand-file-name settings-file)))
         (hooks (cdr (assq 'hooks json))))
    (cl-every (lambda (pair)
                (agent-repl--event-has-command-p hooks (car pair) (cdr pair)))
              agent-repl--managed-hooks)))

(ert-deftest agent-repl-test-config-dirs-to-provision-excludes-default ()
  "config-dirs-to-provision excludes the default ~/.claude."
  (let ((agent-repl-multi-repo-config-dir "~/.claude-chesscom")
        (agent-repl-default-config-dir nil))
    (should-not (member (file-name-as-directory (expand-file-name "~/.claude"))
                        (agent-repl--config-dirs-to-provision)))))

(ert-deftest agent-repl-test-config-dirs-to-provision-includes-multi-repo ()
  "config-dirs-to-provision includes the multi-repo config dir."
  (let ((agent-repl-multi-repo-config-dir "~/.claude-chesscom")
        (agent-repl-default-config-dir nil))
    (should (member (file-name-as-directory (expand-file-name "~/.claude-chesscom"))
                    (agent-repl--config-dirs-to-provision)))))

(ert-deftest agent-repl-test-provision-registers-into-each-config-dir ()
  "provision-config-dirs registers the managed hooks into every selected dir's settings.json."
  (let* ((root (make-temp-file "claude-provision-" t))
         (multi (expand-file-name "chesscom" root))
         (other (expand-file-name "personal" root)))
    (unwind-protect
        (let ((agent-repl-multi-repo-config-dir multi)
              (agent-repl-default-config-dir other))
          (agent-repl--provision-config-dirs)
          (should (test-install--all-hooks-registered-p
                   (expand-file-name "settings.json" multi)))
          (should (test-install--all-hooks-registered-p
                   (expand-file-name "settings.json" other))))
      (delete-directory root t))))

(ert-deftest agent-repl-test-register-hooks-idempotent ()
  "register-hooks-in-settings is a no-op on a second run (returns nil, no double-register)."
  (let* ((dir (make-temp-file "agent-register-" t))
         (settings (expand-file-name "settings.json" dir)))
    (unwind-protect
        (progn
          (should (agent-repl--register-hooks-in-settings settings))
          (should-not (agent-repl--register-hooks-in-settings settings)))
      (delete-directory dir t))))

(ert-deftest agent-repl-test-register-hooks-custom-alist ()
  "register-hooks-in-settings registers exactly a caller-supplied alist.
Guards the parameterization the codex hooks.json installer relies on:
only the supplied events appear, with the supplied matcher."
  (let* ((dir (make-temp-file "agent-register-" t))
         (settings (expand-file-name "hooks.json" dir)))
    (unwind-protect
        (progn
          (agent-repl--register-hooks-in-settings
           settings
           '((Stop . "/x/stop.sh"))
           '((Stop . "stop-matcher")))
          (let* ((json (agent-repl--read-settings-alist settings))
                 (hooks (cdr (assq 'hooks json)))
                 (stop (cdr (assq 'Stop hooks)))
                 (entry (car stop)))
            (should (= (length hooks) 1))
            (should (equal (cdr (assq 'matcher entry)) "stop-matcher"))
            (should (agent-repl--event-has-command-p hooks 'Stop "/x/stop.sh"))))
      (delete-directory dir t))))

(ert-deftest agent-repl-test-register-hooks-preserves-foreign ()
  "register-hooks-in-settings preserves foreign top-level keys."
  (let* ((dir (make-temp-file "agent-register-" t))
         (settings (expand-file-name "settings.json" dir)))
    (unwind-protect
        (progn
          (with-temp-file settings (insert "{\"model\":\"opus\"}"))
          (agent-repl--register-hooks-in-settings settings)
          (let ((json (agent-repl--read-settings-alist settings)))
            (should (equal (cdr (assq 'model json)) "opus"))
            (should (cdr (assq 'hooks json)))))
      (delete-directory dir t))))

(ert-deftest agent-repl-test-register-hooks-notification-matcher ()
  "register-hooks-in-settings writes the permission_prompt matcher for Notification."
  (let* ((dir (make-temp-file "agent-register-" t))
         (settings (expand-file-name "settings.json" dir)))
    (unwind-protect
        (progn
          (agent-repl--register-hooks-in-settings settings)
          (let* ((json (agent-repl--read-settings-alist settings))
                 (entries (cdr (assq 'Notification (cdr (assq 'hooks json))))))
            (should (seq-some (lambda (e) (equal (cdr (assq 'matcher e)) "permission_prompt"))
                              entries))))
      (delete-directory dir t))))

(ert-deftest agent-repl-test-register-hooks-malformed-signals ()
  "register-hooks-in-settings signals on malformed existing JSON (never silently resets)."
  (let* ((dir (make-temp-file "agent-register-" t))
         (settings (expand-file-name "settings.json" dir)))
    (unwind-protect
        (progn
          (with-temp-file settings (insert "{not json"))
          (should-error (agent-repl--register-hooks-in-settings settings)))
      (delete-directory dir t))))

;;; test-install.el ends here
