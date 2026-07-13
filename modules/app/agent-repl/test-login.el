;;; test-login.el --- ERT tests for login.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the gui login button's host end: the account the login
;; terminal targets, and the terminal itself.  vterm is stubbed by
;; test-helpers.el, so no real terminal is ever spawned.
;;
;; Run with:
;;   emacs -batch -Q -l ert -l test-login.el -f ert-run-tests-batch-and-exit

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

(require 'cl-lib)

;;;; ---- Tests: account selection (agent-repl--login-cmd) ---------------------

(ert-deftest agent-repl-test-login-cmd-multi-repo-targets-chesscom-account ()
  "A project under $MULTI_REPO_ROOT logs into ~/.claude-chesscom."
  ;; Arrange
  (let ((process-environment (cons "MULTI_REPO_ROOT=/home/user/multi" process-environment))
        (agent-repl-multi-repo-config-dir "~/.claude-chesscom")
        (agent-repl-login-command "claude /login"))
    ;; Act
    (let ((cmd (agent-repl--login-cmd "/home/user/multi/repoA")))
      ;; Assert
      (should (equal cmd (format "CLAUDE_CONFIG_DIR=%s claude /login"
                                 (shell-quote-argument
                                  (expand-file-name "~/.claude-chesscom"))))))))

(ert-deftest agent-repl-test-login-cmd-personal-project-has-no-config-dir-prefix ()
  "A project outside the multi-repo root logs into the CLI's default root."
  ;; Arrange
  (let ((process-environment (cons "MULTI_REPO_ROOT=/home/user/multi" process-environment))
        (agent-repl-default-config-dir nil)
        (agent-repl-doom-multi-repo-mode nil)
        (agent-repl-login-command "claude /login"))
    ;; Act
    (let ((cmd (agent-repl--login-cmd "/home/user/personal/proj")))
      ;; Assert — an empty CLAUDE_CONFIG_DIR= would name a root called "".
      (should (equal cmd "claude /login")))))

(ert-deftest agent-repl-test-login-cmd-honors-explicit-default-config-dir ()
  "An explicit default config dir is exported for personal projects too."
  ;; Arrange
  (let ((process-environment (cons "MULTI_REPO_ROOT=/home/user/multi" process-environment))
        (agent-repl-default-config-dir "~/.claude-personal")
        (agent-repl-doom-multi-repo-mode nil)
        (agent-repl-login-command "claude /login"))
    ;; Act
    (let ((cmd (agent-repl--login-cmd "/home/user/personal/proj")))
      ;; Assert
      (should (equal cmd (format "CLAUDE_CONFIG_DIR=%s claude /login"
                                 (shell-quote-argument
                                  (expand-file-name "~/.claude-personal"))))))))

(ert-deftest agent-repl-test-login-cmd-uses-the-configured-login-command ()
  "The claude invocation itself comes from `agent-repl-login-command'."
  ;; Arrange
  (let ((process-environment (cons "MULTI_REPO_ROOT=" process-environment))
        (agent-repl-default-config-dir nil)
        (agent-repl-doom-multi-repo-mode nil)
        (agent-repl-login-command "claude"))
    ;; Act
    (let ((cmd (agent-repl--login-cmd "/home/user/personal/proj")))
      ;; Assert
      (should (equal cmd "claude")))))

(ert-deftest agent-repl-test-login-cmd-errors-without-a-project-dir ()
  "No project dir means no account can be resolved, so the login fails loudly."
  ;; Act / Assert — `--compute-config-dir' refuses a nil dir by contract.
  (should-error (agent-repl--login-cmd nil)))

;;;; ---- Tests: buffer naming -------------------------------------------------

(ert-deftest agent-repl-test-login-buffer-name-is-workspace-scoped ()
  "Each workspace gets its own login terminal name."
  ;; Arrange / Act / Assert
  (should (equal (agent-repl--login-buffer-name "feat-x") "*agent-login-feat-x*")))

;;;; ---- Tests: terminal launch (agent-repl--login-open-terminal) -------------

(defmacro agent-repl-test--with-login-stubs (sent &rest body)
  "Run BODY with the vterm and buffer boundaries stubbed.
SENT is a symbol bound to the string sent to the terminal."
  (declare (indent 1))
  `(let ((,sent nil))
     (cl-letf (((symbol-function 'agent-repl--create-buffer)
                (lambda (_ws &optional _suffix) (generate-new-buffer " *login-test*")))
               ((symbol-function 'vterm-mode) (lambda () nil))
               ((symbol-function 'vterm-send-string) (lambda (s) (setq ,sent s)))
               ((symbol-function 'vterm-send-return) (lambda () nil))
               ((symbol-function 'pop-to-buffer) (lambda (b &rest _) b)))
       ,@body)))

(ert-deftest agent-repl-test-login-terminal-runs-the-account-scoped-command ()
  "The login terminal runs the command carrying the workspace's config dir."
  (agent-repl-test--with-clean-state
    (let ((process-environment (cons "MULTI_REPO_ROOT=/home/user/multi" process-environment))
          (agent-repl-multi-repo-config-dir "~/.claude-chesscom")
          (agent-repl-login-command "claude /login")
          (agent-repl-startup-prefix ""))
      (agent-repl-test--with-login-stubs sent
        ;; Act
        (let ((buf (agent-repl--login-open-terminal "ws1" "/home/user/multi/repoA")))
          (unwind-protect
              ;; Assert
              (should (string-match-p
                       (regexp-quote (expand-file-name "~/.claude-chesscom")) sent))
            (kill-buffer buf)))))))

(ert-deftest agent-repl-test-login-terminal-prepends-the-startup-prefix ()
  "The login command is sent behind `agent-repl-startup-prefix', like the vterm start."
  (agent-repl-test--with-clean-state
    (let ((process-environment (cons "MULTI_REPO_ROOT=" process-environment))
          (agent-repl-default-config-dir nil)
          (agent-repl-doom-multi-repo-mode nil)
          (agent-repl-login-command "claude /login")
          (agent-repl-startup-prefix "clear && "))
      (agent-repl-test--with-login-stubs sent
        ;; Act
        (let ((buf (agent-repl--login-open-terminal "ws1" "/home/user/personal/proj")))
          (unwind-protect
              ;; Assert
              (should (equal sent "clear && claude /login"))
            (kill-buffer buf)))))))

(ert-deftest agent-repl-test-login-terminal-runs-outside-the-workspace-dir ()
  "The login terminal runs from the temp dir, never the workspace's cwd.
The login CLI fires the global hooks, which are keyed on cwd — running it
in the workspace would attribute a login prompt's session_start/stop
sentinels to that workspace and walk its tab through bogus agent states."
  (agent-repl-test--with-clean-state
    (let ((process-environment (cons "MULTI_REPO_ROOT=/home/user/multi" process-environment))
          (agent-repl-multi-repo-config-dir "~/.claude-chesscom")
          (agent-repl-startup-prefix "")
          (ran-in nil))
      (cl-letf (((symbol-function 'agent-repl--create-buffer)
                 (lambda (_ws &optional _suffix)
                   (setq ran-in default-directory)
                   (generate-new-buffer " *login-test*")))
                ((symbol-function 'vterm-mode) (lambda () nil))
                ((symbol-function 'vterm-send-string) (lambda (_s) nil))
                ((symbol-function 'vterm-send-return) (lambda () nil))
                ((symbol-function 'pop-to-buffer) (lambda (b &rest _) b)))
        ;; Act
        (let ((buf (agent-repl--login-open-terminal "ws1" "/home/user/multi/repoA")))
          (unwind-protect
              ;; Assert
              (should (equal ran-in temporary-file-directory))
            (kill-buffer buf)))))))

(ert-deftest agent-repl-test-login-terminal-names-the-buffer-for-the-workspace ()
  "The login terminal is renamed onto the workspace-scoped login name."
  (agent-repl-test--with-clean-state
    (let ((process-environment (cons "MULTI_REPO_ROOT=" process-environment))
          (agent-repl-default-config-dir nil)
          (agent-repl-doom-multi-repo-mode nil)
          (agent-repl-startup-prefix ""))
      (agent-repl-test--with-login-stubs sent
        (ignore sent)
        ;; Act
        (let ((buf (agent-repl--login-open-terminal "ws1" "/home/user/personal/proj")))
          (unwind-protect
              ;; Assert
              (should (equal (buffer-name buf) "*agent-login-ws1*"))
            (kill-buffer buf)))))))

;;;; ---- Tests: sentinel callback (agent-repl--on-login-request-event) --------

(ert-deftest agent-repl-test-login-event-prefers-the-registered-project-dir ()
  "The account is resolved from the workspace's :project-dir, not the sentinel cwd.
The sentinel cwd is the daemon's view; the registered project dir is what
every other account decision in the module is made from, so the two must
not be allowed to disagree."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/home/user/multi/repoA")
    (let ((process-environment (cons "MULTI_REPO_ROOT=/home/user/multi" process-environment))
          (agent-repl-multi-repo-config-dir "~/.claude-chesscom")
          (agent-repl-login-command "claude /login")
          (agent-repl-startup-prefix "")
          (opened nil))
      (cl-letf (((symbol-function 'agent-repl--login-open-terminal)
                 (lambda (_ws dir) (setq opened dir))))
        ;; Act — the sentinel reports a DIFFERENT (container-shaped) cwd.
        (agent-repl--on-login-request-event "ws1" "/repoA")
        ;; Assert
        (should (equal opened "/home/user/multi/repoA"))))))

(ert-deftest agent-repl-test-login-event-falls-back-to-the-sentinel-dir ()
  "An unregistered workspace logs in against the cwd the sentinel named."
  (agent-repl-test--with-clean-state
    (let ((agent-repl-startup-prefix "")
          (opened nil))
      (cl-letf (((symbol-function 'agent-repl--login-open-terminal)
                 (lambda (_ws dir) (setq opened dir))))
        ;; Act — no :project-dir was ever registered for this ws.
        (agent-repl--on-login-request-event "ws-unregistered" "/home/user/plain/proj")
        ;; Assert
        (should (equal opened "/home/user/plain/proj"))))))

;;;; ---- Tests: sentinel dispatch wiring --------------------------------------

(ert-deftest agent-repl-test-login-request-sentinel-dispatches-to-the-login-handler ()
  "A `login_request_' sentinel filename routes to the login callback.
The daemon writes exactly this prefix, so a rename on either side silently
breaks the button."
  ;; Arrange / Act
  (let ((handler (cl-loop for (prefix . plist) in agent-repl--sentinel-dispatch-alist
                          when (string-prefix-p prefix "login_request_sid-1")
                          return plist)))
    ;; Assert
    (should (eq (plist-get handler :callback) 'agent-repl--on-login-request-event))))

(provide 'test-login)
;;; test-login.el ends here
