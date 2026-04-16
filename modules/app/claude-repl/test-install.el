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

;;;; ---- doctor-issues ----

(defun test-install--doctor-level-count (issues level)
  "Return the number of entries in ISSUES whose level is LEVEL."
  (cl-count-if (lambda (i) (eq (car i) level)) issues))

(defun test-install--doctor-find (issues substring)
  "Return the first issue whose message contains SUBSTRING, else nil."
  (cl-find-if (lambda (i) (string-match-p substring (cdr i))) issues))

(ert-deftest claude-repl-test-doctor-sandbox-returns-nil ()
  "In sandbox, doctor-issues is a no-op."
  (cl-letf (((symbol-function 'claude-repl--in-sandbox-p) (lambda () t)))
    (should-not (claude-repl--doctor-issues))))

(ert-deftest claude-repl-test-doctor-missing-settings ()
  "Settings missing or unreadable produces a single top-level error."
  (cl-letf (((symbol-function 'claude-repl--in-sandbox-p) (lambda () nil))
            ((symbol-function 'claude-repl--settings-json) (lambda () nil)))
    (let ((issues (claude-repl--doctor-issues)))
      (should (= 1 (length issues)))
      (should (eq 'error (car (car issues))))
      (should (string-match-p "missing or unreadable"
                              (cdr (car issues)))))))

(ert-deftest claude-repl-test-doctor-all-present ()
  "With all hooks registered and scripts on disk matching source, no issues."
  (cl-letf (((symbol-function 'claude-repl--in-sandbox-p) (lambda () nil))
            ((symbol-function 'claude-repl--settings-json)
             #'test-install--json-with-all-hooks)
            ((symbol-function 'claude-repl--check-script-files)
             (lambda (_issues) nil)))
    (should-not (claude-repl--doctor-issues))))

(ert-deftest claude-repl-test-doctor-missing-stop-errors ()
  "Missing Stop registration is error-level (workspaces stuck in :thinking)."
  (cl-letf (((symbol-function 'claude-repl--in-sandbox-p) (lambda () nil))
            ((symbol-function 'claude-repl--settings-json)
             (lambda () (test-install--json-missing-one 'Stop)))
            ((symbol-function 'claude-repl--check-script-files)
             (lambda (_issues) nil)))
    (let ((issues (claude-repl--doctor-issues)))
      (should (test-install--doctor-find issues "Stop hook not registered"))
      (should (eq 'error
                  (car (test-install--doctor-find
                        issues "Stop hook not registered")))))))

(ert-deftest claude-repl-test-doctor-missing-session-start-errors ()
  "Missing SessionStart is error-level (readiness never fires, placeholder stuck)."
  (cl-letf (((symbol-function 'claude-repl--in-sandbox-p) (lambda () nil))
            ((symbol-function 'claude-repl--settings-json)
             (lambda () (test-install--json-missing-one 'SessionStart)))
            ((symbol-function 'claude-repl--check-script-files)
             (lambda (_issues) nil)))
    (let ((issues (claude-repl--doctor-issues)))
      (should (test-install--doctor-find issues "SessionStart"))
      (should (eq 'error
                  (car (test-install--doctor-find issues "SessionStart")))))))

(ert-deftest claude-repl-test-doctor-missing-user-prompt-submit-warns ()
  "Missing UserPromptSubmit is warn-level (Emacs do-send still sets :thinking)."
  (cl-letf (((symbol-function 'claude-repl--in-sandbox-p) (lambda () nil))
            ((symbol-function 'claude-repl--settings-json)
             (lambda () (test-install--json-missing-one 'UserPromptSubmit)))
            ((symbol-function 'claude-repl--check-script-files)
             (lambda (_issues) nil)))
    (let ((issues (claude-repl--doctor-issues)))
      (should (test-install--doctor-find issues "UserPromptSubmit"))
      (should (eq 'warn
                  (car (test-install--doctor-find issues "UserPromptSubmit")))))))

(ert-deftest claude-repl-test-doctor-missing-notification-warns ()
  "Missing Notification is warn-level (❓ badge absent but module works)."
  (cl-letf (((symbol-function 'claude-repl--in-sandbox-p) (lambda () nil))
            ((symbol-function 'claude-repl--settings-json)
             (lambda () (test-install--json-missing-one 'Notification)))
            ((symbol-function 'claude-repl--check-script-files)
             (lambda (_issues) nil)))
    (let ((issues (claude-repl--doctor-issues)))
      (should (test-install--doctor-find issues "Notification"))
      (should (eq 'warn
                  (car (test-install--doctor-find issues "Notification")))))))

(ert-deftest claude-repl-test-doctor-settings-skip-short-circuits-script-checks ()
  "When settings.json is missing, script-file checks are not performed."
  (let ((script-checks-called nil))
    (cl-letf (((symbol-function 'claude-repl--in-sandbox-p) (lambda () nil))
              ((symbol-function 'claude-repl--settings-json) (lambda () nil))
              ((symbol-function 'claude-repl--check-script-files)
               (lambda (_) (setq script-checks-called t))))
      (claude-repl--doctor-issues)
      (should-not script-checks-called))))

;; Script-file checks — use tmpfile fixtures so the real file predicates work.

(defun test-install--with-fake-hooks-dir (contents body-fn)
  "Run BODY-FN with claude-repl--hooks-dest-dir rebound to a tmpdir.
CONTENTS is an alist ((SCRIPT-NAME . (MODE . TEXT)) ...): writes each
script with the given unix MODE permissions and TEXT content.  A
SCRIPT-NAME with `:missing' means leave that script absent."
  (let ((tmpdir (file-name-as-directory
                 (make-temp-file "claude-repl-doctor-" t))))
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
          (cl-letf (((symbol-function 'claude-repl--installed-script-path)
                     (lambda (cmd)
                       (expand-file-name
                        (file-name-nondirectory cmd) tmpdir))))
            (funcall body-fn tmpdir)))
      (delete-directory tmpdir t))))

(ert-deftest claude-repl-test-doctor-script-missing-errors ()
  "A missing installed script emits an error-level issue."
  (test-install--with-fake-hooks-dir
   `(("stop-notify.sh" . :missing)
     ("prompt-submit-notify.sh" . (#o755 . "x"))
     ("session-start-notify.sh" . (#o755 . "x"))
     ("permission-notify.sh" . (#o755 . "x")))
   (lambda (_tmp)
     (cl-letf (((symbol-function 'claude-repl--in-sandbox-p) (lambda () nil))
               ((symbol-function 'claude-repl--settings-json)
                #'test-install--json-with-all-hooks)
               ((symbol-function 'claude-repl--script-drift-p)
                (lambda (_) nil)))
       (let* ((issues (claude-repl--doctor-issues))
              (found (test-install--doctor-find
                      issues "missing:.*stop-notify\\.sh")))
         (should found)
         (should (eq 'error (car found))))))))

(ert-deftest claude-repl-test-doctor-script-not-executable-errors ()
  "A non-executable installed script emits an error-level issue."
  (test-install--with-fake-hooks-dir
   `(("stop-notify.sh" . (#o644 . "x"))
     ("prompt-submit-notify.sh" . (#o755 . "x"))
     ("session-start-notify.sh" . (#o755 . "x"))
     ("permission-notify.sh" . (#o755 . "x")))
   (lambda (_tmp)
     (cl-letf (((symbol-function 'claude-repl--in-sandbox-p) (lambda () nil))
               ((symbol-function 'claude-repl--settings-json)
                #'test-install--json-with-all-hooks)
               ((symbol-function 'claude-repl--script-drift-p)
                (lambda (_) nil)))
       (let* ((issues (claude-repl--doctor-issues))
              (found (test-install--doctor-find issues "not executable")))
         (should found)
         (should (eq 'error (car found))))))))

(ert-deftest claude-repl-test-doctor-script-drift-warns ()
  "A drifted installed script emits a warn-level issue."
  (test-install--with-fake-hooks-dir
   `(("stop-notify.sh" . (#o755 . "x"))
     ("prompt-submit-notify.sh" . (#o755 . "x"))
     ("session-start-notify.sh" . (#o755 . "x"))
     ("permission-notify.sh" . (#o755 . "x")))
   (lambda (_tmp)
     (cl-letf (((symbol-function 'claude-repl--in-sandbox-p) (lambda () nil))
               ((symbol-function 'claude-repl--settings-json)
                #'test-install--json-with-all-hooks)
               ((symbol-function 'claude-repl--script-drift-p)
                (lambda (cmd)
                  (equal (file-name-nondirectory cmd)
                         "stop-notify.sh"))))
       (let* ((issues (claude-repl--doctor-issues))
              (found (test-install--doctor-find issues "drift")))
         (should found)
         (should (eq 'warn (car found))))))))

(ert-deftest claude-repl-test-script-drift-p-match ()
  "script-drift-p is nil when installed bytes match source."
  (let* ((src (make-temp-file "crd-src-")) (dest (make-temp-file "crd-dst-")))
    (unwind-protect
        (progn
          (with-temp-file src (insert "identical"))
          (with-temp-file dest (insert "identical"))
          (cl-letf (((symbol-function 'claude-repl--installed-script-path)
                     (lambda (_) dest))
                    ((symbol-function 'claude-repl--source-script-path)
                     (lambda (_) src)))
            (should-not
             (claude-repl--script-drift-p "~/.claude/hooks/anything.sh"))))
      (delete-file src) (delete-file dest))))

(ert-deftest claude-repl-test-script-drift-p-mismatch ()
  "script-drift-p is non-nil when installed bytes differ from source."
  (let* ((src (make-temp-file "crd-src-")) (dest (make-temp-file "crd-dst-")))
    (unwind-protect
        (progn
          (with-temp-file src (insert "source-content"))
          (with-temp-file dest (insert "tampered-content"))
          (cl-letf (((symbol-function 'claude-repl--installed-script-path)
                     (lambda (_) dest))
                    ((symbol-function 'claude-repl--source-script-path)
                     (lambda (_) src)))
            (should (claude-repl--script-drift-p
                     "~/.claude/hooks/anything.sh"))))
      (delete-file src) (delete-file dest))))

;;;; ---- Bash-integration tests --------------------------------------------
;;
;; These spawn `bash .claude/install.sh` with HOME redirected to a tmpdir.
;; They assert end-to-end behavior of the installer, including foreign-entry
;; preservation, idempotency, uninstall semantics, and sandbox short-circuit.
;; Skipped when `bash' or `jq' is not on PATH.

(defun test-install-bash--deps-available-p ()
  "Return non-nil when bash and jq are both on PATH."
  (and (executable-find "bash") (executable-find "jq")))

(defun test-install-bash--make-tmphome ()
  "Return an absolute path to a fresh tmpdir usable as HOME."
  (file-name-as-directory
   (make-temp-file "claude-repl-install-test-" t)))

(defun test-install-bash--cleanup (tmphome)
  "Recursively delete TMPHOME."
  (when (and tmphome (file-directory-p tmphome))
    (delete-directory tmphome t)))

(defun test-install-bash--write-settings (tmphome content)
  "Write CONTENT (string) to TMPHOME/.claude/settings.json.
Creates the .claude directory if needed."
  (let ((dir (expand-file-name ".claude" tmphome)))
    (make-directory dir t)
    (with-temp-file (expand-file-name "settings.json" dir)
      (insert content))))

(defun test-install-bash--run (tmphome action &optional extra-env)
  "Invoke the installer with HOME=TMPHOME and ACTION.
EXTRA-ENV is an alist of additional environment variables.
Returns a cons (EXIT-CODE . OUTPUT)."
  (with-temp-buffer
    ;; Pin default-directory to a known-existing path so call-process does
    ;; not choke when the test runner's inherited default-directory contains
    ;; an unexpanded tilde.
    (let* ((default-directory temporary-file-directory)
           (process-environment
            (append (mapcar (lambda (kv) (format "%s=%s" (car kv) (cdr kv)))
                            (cons (cons "HOME" (directory-file-name tmphome))
                                  extra-env))
                    process-environment))
           (exit (call-process "bash" nil t nil
                               claude-repl--install-script action)))
      (cons exit (buffer-string)))))

(defun test-install-bash--read-settings (tmphome)
  "Return parsed settings.json from TMPHOME, or nil if missing."
  (let ((path (expand-file-name ".claude/settings.json" tmphome)))
    (when (file-exists-p path)
      (json-read-file path))))

(defun test-install-bash--cmd-in-event-p (json event cmd)
  "Return non-nil if CMD is registered anywhere under EVENT in JSON."
  (let ((hooks (cdr (assq 'hooks json))))
    (claude-repl--event-has-command-p hooks event cmd)))

(defun test-install-bash--event-entry-count (json event)
  "Return the number of top-level entries under EVENT in JSON's .hooks."
  (let* ((hooks (cdr (assq 'hooks json)))
         (entries (cdr (assq event hooks))))
    (cond ((null entries) 0)
          ((vectorp entries) (length entries))
          (t (length entries)))))

;; --- Fresh install ------------------------------------------------------

(ert-deftest claude-repl-test-bash-install-fresh ()
  "Fresh install creates settings.json with all four events registered."
  (skip-unless (test-install-bash--deps-available-p))
  (let ((tmphome (test-install-bash--make-tmphome)))
    (unwind-protect
        (let* ((result (test-install-bash--run tmphome "install"))
               (json (test-install-bash--read-settings tmphome)))
          (should (= 0 (car result)))
          (should json)
          (should (test-install-bash--cmd-in-event-p
                   json 'Stop "~/.claude/hooks/stop-notify.sh"))
          (should (test-install-bash--cmd-in-event-p
                   json 'UserPromptSubmit
                   "~/.claude/hooks/prompt-submit-notify.sh"))
          (should (test-install-bash--cmd-in-event-p
                   json 'SessionStart
                   "~/.claude/hooks/session-start-notify.sh"))
          (should (test-install-bash--cmd-in-event-p
                   json 'Notification
                   "~/.claude/hooks/permission-notify.sh")))
      (test-install-bash--cleanup tmphome))))

(ert-deftest claude-repl-test-bash-install-no-backup-when-fresh ()
  "Fresh install (settings.json not pre-existing) does not create a backup."
  (skip-unless (test-install-bash--deps-available-p))
  (let ((tmphome (test-install-bash--make-tmphome)))
    (unwind-protect
        (progn
          (test-install-bash--run tmphome "install")
          (should-not
           (directory-files (expand-file-name ".claude" tmphome)
                            nil "settings\\.json\\.bak\\.")))
      (test-install-bash--cleanup tmphome))))

;; --- Idempotency --------------------------------------------------------

(ert-deftest claude-repl-test-bash-install-idempotent ()
  "Running install twice does not duplicate managed entries."
  (skip-unless (test-install-bash--deps-available-p))
  (let ((tmphome (test-install-bash--make-tmphome)))
    (unwind-protect
        (progn
          (test-install-bash--run tmphome "install")
          (test-install-bash--run tmphome "install")
          (let ((json (test-install-bash--read-settings tmphome)))
            (should (= 1 (test-install-bash--event-entry-count json 'Stop)))
            (should (= 1 (test-install-bash--event-entry-count
                          json 'UserPromptSubmit)))
            (should (= 1 (test-install-bash--event-entry-count
                          json 'SessionStart)))
            (should (= 1 (test-install-bash--event-entry-count
                          json 'Notification)))))
      (test-install-bash--cleanup tmphome))))

;; --- Foreign entries preserved ------------------------------------------

(ert-deftest claude-repl-test-bash-install-preserves-foreign-stop ()
  "Install appends our Stop entry alongside a pre-existing foreign one."
  (skip-unless (test-install-bash--deps-available-p))
  (let ((tmphome (test-install-bash--make-tmphome)))
    (unwind-protect
        (progn
          (test-install-bash--write-settings
           tmphome
           "{\"hooks\":{\"Stop\":[{\"hooks\":[{\"type\":\"command\",\"command\":\"/foreign.sh\"}]}]}}")
          (test-install-bash--run tmphome "install")
          (let ((json (test-install-bash--read-settings tmphome)))
            (should (= 2 (test-install-bash--event-entry-count json 'Stop)))
            (should (test-install-bash--cmd-in-event-p
                     json 'Stop "/foreign.sh"))
            (should (test-install-bash--cmd-in-event-p
                     json 'Stop "~/.claude/hooks/stop-notify.sh"))))
      (test-install-bash--cleanup tmphome))))

(ert-deftest claude-repl-test-bash-install-preserves-foreign-top-level ()
  "Install preserves unrelated top-level keys in settings.json."
  (skip-unless (test-install-bash--deps-available-p))
  (let ((tmphome (test-install-bash--make-tmphome)))
    (unwind-protect
        (progn
          (test-install-bash--write-settings
           tmphome "{\"enabledPlugins\":[\"foo\"],\"statusLine\":\"bar\"}")
          (test-install-bash--run tmphome "install")
          (let ((json (test-install-bash--read-settings tmphome)))
            (should (equal (cdr (assq 'enabledPlugins json)) ["foo"]))
            (should (equal (cdr (assq 'statusLine json)) "bar"))))
      (test-install-bash--cleanup tmphome))))

(ert-deftest claude-repl-test-bash-install-creates-backup-when-file-exists ()
  "Install creates a timestamped backup of a pre-existing settings.json."
  (skip-unless (test-install-bash--deps-available-p))
  (let ((tmphome (test-install-bash--make-tmphome)))
    (unwind-protect
        (progn
          (test-install-bash--write-settings tmphome "{\"enabledPlugins\":[]}")
          (test-install-bash--run tmphome "install")
          (let ((backups
                 (directory-files (expand-file-name ".claude" tmphome)
                                  nil "settings\\.json\\.bak\\.[0-9]+\\'")))
            (should (= 1 (length backups)))))
      (test-install-bash--cleanup tmphome))))

;; --- Malformed settings aborts ------------------------------------------

(ert-deftest claude-repl-test-bash-install-malformed-settings-aborts ()
  "Install aborts with non-zero exit when settings.json is malformed."
  (skip-unless (test-install-bash--deps-available-p))
  (let ((tmphome (test-install-bash--make-tmphome)))
    (unwind-protect
        (progn
          (test-install-bash--write-settings tmphome "{ not valid json")
          (let ((result (test-install-bash--run tmphome "install")))
            (should (/= 0 (car result)))))
      (test-install-bash--cleanup tmphome))))

;; --- Sandbox no-op ------------------------------------------------------

(ert-deftest claude-repl-test-bash-install-sandbox-noop ()
  "DOOM_SANDBOX=1 makes install short-circuit without writing."
  (skip-unless (test-install-bash--deps-available-p))
  (let ((tmphome (test-install-bash--make-tmphome)))
    (unwind-protect
        (let ((result (test-install-bash--run
                       tmphome "install" '(("DOOM_SANDBOX" . "1")))))
          (should (= 0 (car result)))
          (should (string-match-p "sandbox" (cdr result)))
          (should-not (file-exists-p
                       (expand-file-name ".claude/settings.json" tmphome))))
      (test-install-bash--cleanup tmphome))))

;; --- Uninstall ----------------------------------------------------------

(ert-deftest claude-repl-test-bash-uninstall-preserves-foreign ()
  "Uninstall removes our Stop entry but preserves the foreign one."
  (skip-unless (test-install-bash--deps-available-p))
  (let ((tmphome (test-install-bash--make-tmphome)))
    (unwind-protect
        (progn
          (test-install-bash--write-settings
           tmphome
           "{\"hooks\":{\"Stop\":[{\"hooks\":[{\"type\":\"command\",\"command\":\"/foreign.sh\"}]}]}}")
          (test-install-bash--run tmphome "install")
          (test-install-bash--run tmphome "uninstall")
          (let ((json (test-install-bash--read-settings tmphome)))
            (should (= 1 (test-install-bash--event-entry-count json 'Stop)))
            (should (test-install-bash--cmd-in-event-p
                     json 'Stop "/foreign.sh"))
            (should-not (test-install-bash--cmd-in-event-p
                         json 'Stop "~/.claude/hooks/stop-notify.sh"))))
      (test-install-bash--cleanup tmphome))))

(ert-deftest claude-repl-test-bash-uninstall-removes-empty-event-keys ()
  "Uninstall drops an event key whose array becomes empty."
  (skip-unless (test-install-bash--deps-available-p))
  (let ((tmphome (test-install-bash--make-tmphome)))
    (unwind-protect
        (progn
          (test-install-bash--run tmphome "install")
          (test-install-bash--run tmphome "uninstall")
          (let* ((json (test-install-bash--read-settings tmphome))
                 (hooks (cdr (assq 'hooks json))))
            ;; None of our event keys should survive when no foreign entries
            ;; co-exist.
            (should-not (assq 'UserPromptSubmit hooks))
            (should-not (assq 'SessionStart hooks))
            (should-not (assq 'Notification hooks))
            (should-not (assq 'Stop hooks))))
      (test-install-bash--cleanup tmphome))))

(ert-deftest claude-repl-test-bash-uninstall-deletes-scripts ()
  "Uninstall removes the managed script files from ~/.claude/hooks/."
  (skip-unless (test-install-bash--deps-available-p))
  (let ((tmphome (test-install-bash--make-tmphome)))
    (unwind-protect
        (progn
          (test-install-bash--run tmphome "install")
          (test-install-bash--run tmphome "uninstall")
          (let ((hooks-dir (expand-file-name ".claude/hooks" tmphome)))
            (should-not (file-exists-p
                         (expand-file-name "stop-notify.sh" hooks-dir)))
            (should-not (file-exists-p
                         (expand-file-name "prompt-submit-notify.sh" hooks-dir)))
            (should-not (file-exists-p
                         (expand-file-name "session-start-notify.sh" hooks-dir)))
            (should-not (file-exists-p
                         (expand-file-name "permission-notify.sh" hooks-dir)))))
      (test-install-bash--cleanup tmphome))))

;; --- Reinstall ----------------------------------------------------------

(ert-deftest claude-repl-test-bash-reinstall-round-trip ()
  "Reinstall leaves the foreign+ours layout identical to one install run."
  (skip-unless (test-install-bash--deps-available-p))
  (let ((tmphome (test-install-bash--make-tmphome)))
    (unwind-protect
        (progn
          (test-install-bash--write-settings
           tmphome
           "{\"hooks\":{\"Stop\":[{\"hooks\":[{\"type\":\"command\",\"command\":\"/foreign.sh\"}]}]}}")
          (test-install-bash--run tmphome "install")
          (test-install-bash--run tmphome "reinstall")
          (let ((json (test-install-bash--read-settings tmphome)))
            ;; Foreign plus our Stop = length 2 (still).
            (should (= 2 (test-install-bash--event-entry-count json 'Stop)))
            (should (test-install-bash--cmd-in-event-p
                     json 'Stop "/foreign.sh"))
            (should (test-install-bash--cmd-in-event-p
                     json 'Stop "~/.claude/hooks/stop-notify.sh"))))
      (test-install-bash--cleanup tmphome))))

;;; test-install.el ends here
