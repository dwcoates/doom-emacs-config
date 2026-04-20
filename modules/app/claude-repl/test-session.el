;;; test-session.el --- Tests for session.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for session lifecycle management: sandbox config, command building,
;; session startup, completion handling, session ID management, readiness
;; handling, process state predicates, and the ready timer.

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Migrated tests ----

(ert-deftest claude-repl-test-finished-from-hook-hidden-sets-done ()
  "handle-claude-finished sets claude-state :done when vterm buffer is not visible."
  (claude-repl-test--with-clean-state
    (let ((done-set nil)
          (fake-buf (generate-new-buffer " *test-hook-hidden*")))
      (unwind-protect
          (progn
            (claude-repl--ws-put "ws1" :vterm-buffer fake-buf)
            (cl-letf (((symbol-function 'get-buffer-window)
                       (lambda (_buf &rest _) nil))
                      ((symbol-function 'claude-repl--do-refresh) #'ignore)
                      ((symbol-function 'claude-repl--update-hide-overlay) #'ignore)
                      ((symbol-function 'claude-repl--maybe-notify-finished) #'ignore)
                      ((symbol-function 'claude-repl--ws-set-claude-state)
                       (lambda (ws state)
                         (when (eq state :done) (setq done-set ws)))))
              (claude-repl--handle-claude-finished "ws1")
              (should (equal done-set "ws1"))))
        (kill-buffer fake-buf)))))

(ert-deftest claude-repl-test-finished-from-hook-visible-also-sets-done ()
  "handle-claude-finished sets :done regardless of vterm visibility.
Visibility is no longer a gate; the renderer decides the display via
the composed-state rule with :repl-state."
  (claude-repl-test--with-clean-state
    (let ((done-set nil)
          (fake-buf (generate-new-buffer " *test-hook-visible*")))
      (unwind-protect
          (progn
            (claude-repl--ws-put "ws1" :vterm-buffer fake-buf)
            (cl-letf (((symbol-function 'get-buffer-window)
                       (lambda (_buf &rest _) (selected-window)))
                      ((symbol-function 'claude-repl--do-refresh) #'ignore)
                      ((symbol-function 'claude-repl--update-hide-overlay) #'ignore)
                      ((symbol-function 'claude-repl--fix-vterm-scroll) #'ignore)
                      ((symbol-function 'claude-repl--maybe-notify-finished) #'ignore)
                      ((symbol-function 'claude-repl--ws-set-claude-state)
                       (lambda (ws state)
                         (when (eq state :done) (setq done-set ws)))))
              (claude-repl--handle-claude-finished "ws1")
              (should (equal done-set "ws1"))))
        (kill-buffer fake-buf)))))

(ert-deftest claude-repl-test-maybe-notify-debounce ()
  "maybe-notify-finished should debounce within 2 seconds."
  (claude-repl-test--with-clean-state
    (let ((notify-count 0))
      (cl-letf (((symbol-function 'frame-focus-state) (lambda () nil))
                ((symbol-function 'run-at-time)
                 (lambda (_delay _repeat fn &rest args)
                   (cl-incf notify-count))))
        ;; First call should notify
        (claude-repl--maybe-notify-finished "ws1")
        (should (= notify-count 1))
        ;; Second call within 2s window should be suppressed
        (claude-repl--maybe-notify-finished "ws1")
        (should (= notify-count 1))
        ;; Simulate time passing beyond debounce window
        (claude-repl--ws-put "ws1" :last-notify-time (- (float-time) 3.0))
        (claude-repl--maybe-notify-finished "ws1")
        (should (= notify-count 2))))))

(ert-deftest claude-repl-test-maybe-notify-skips-when-focused ()
  "maybe-notify-finished should NOT send desktop notification when frame is focused."
  (claude-repl-test--with-clean-state
    (let ((notify-count 0))
      (cl-letf (((symbol-function 'frame-focus-state) (lambda () t))
                ((symbol-function 'run-at-time)
                 (lambda (_delay _repeat fn &rest args)
                   (cl-incf notify-count))))
        (claude-repl--maybe-notify-finished "ws1")
        (should (= notify-count 0))))))

(ert-deftest claude-repl-test-finished-from-hook-nil-vterm-sets-done ()
  "handle-claude-finished still sets :done when vterm-buf is nil.
The Stop signal's intent is \"Claude finished\" — unrelated to whether
the vterm buffer is still around.  Refresh-vterm-after-finish is guarded
by vterm-buf presence; the :done write is not."
  (claude-repl-test--with-clean-state
    (let ((done-set nil))
      ;; Register ws1 (required by handle-claude-finished guard) but
      ;; do NOT set :vterm-buffer.
      (claude-repl--ws-put "ws1" :project-dir "/tmp/fake")
      (cl-letf (((symbol-function 'claude-repl--do-refresh) #'ignore)
                ((symbol-function 'claude-repl--update-hide-overlay) #'ignore)
                ((symbol-function 'claude-repl--maybe-notify-finished) #'ignore)
                ((symbol-function '+workspace-current-name) (lambda () "ws1"))
                ((symbol-function 'claude-repl--ws-set-claude-state)
                 (lambda (ws state)
                   (when (eq state :done) (setq done-set ws)))))
        (claude-repl--handle-claude-finished "ws1")
        (should (equal done-set "ws1"))))))

;;;; ---- Tests: Sandbox configuration ----

(ert-deftest claude-repl-test-find-sandbox-script-prefers-path ()
  "find-sandbox-script should prefer executable on PATH over repo script."
  (cl-letf (((symbol-function 'executable-find)
             (lambda (name)
               (when (equal name "claude-sandbox") "/usr/bin/claude-sandbox")))
            ((symbol-function 'file-executable-p) (lambda (_f) t)))
    (should (equal (claude-repl--find-sandbox-script "/repo")
                   "/usr/bin/claude-sandbox"))))

(ert-deftest claude-repl-test-find-sandbox-script-falls-back-to-repo ()
  "find-sandbox-script should fall back to .agents-sandbox/sandbox in repo."
  (cl-letf (((symbol-function 'executable-find) (lambda (_n) nil))
            ((symbol-function 'file-executable-p)
             (lambda (f)
               (string-suffix-p ".agents-sandbox/sandbox" f))))
    (should (equal (claude-repl--find-sandbox-script "/my/repo")
                   "/my/repo/.agents-sandbox/sandbox"))))

(ert-deftest claude-repl-test-find-sandbox-script-returns-nil ()
  "find-sandbox-script should return nil when no launcher exists."
  (cl-letf (((symbol-function 'executable-find) (lambda (_n) nil))
            ((symbol-function 'file-executable-p) (lambda (_f) nil)))
    (should-not (claude-repl--find-sandbox-script "/my/repo"))))

(ert-deftest claude-repl-test-query-sandbox-image-returns-trimmed ()
  "query-sandbox-image should return trimmed output from script."
  (cl-letf (((symbol-function 'call-process)
             (lambda (_script _infile destination _display &rest args)
               ;; Real call-process with destination=t inserts into current buffer
               (when (and (eq destination t) (equal args '("--image-name")))
                 (insert "  my-image:latest  "))
               0)))
    (should (equal (claude-repl--query-sandbox-image "/path/to/script")
                   "my-image:latest"))))

(ert-deftest claude-repl-test-query-sandbox-image-empty-returns-nil ()
  "query-sandbox-image should return nil when script outputs empty string."
  (cl-letf (((symbol-function 'call-process)
             (lambda (_script _infile _dest _display &rest _args) 0)))
    (should-not (claude-repl--query-sandbox-image "/path/to/script"))))

(ert-deftest claude-repl-test-find-install-script-exists ()
  "find-install-script should return path when install script is executable."
  (cl-letf (((symbol-function 'file-executable-p)
             (lambda (f) (string-suffix-p "install-claude.sh" f))))
    (should (equal (claude-repl--find-install-script "/repo")
                   "/repo/.agents-sandbox/install-claude.sh"))))

(ert-deftest claude-repl-test-find-install-script-missing ()
  "find-install-script should return nil when install script not found."
  (cl-letf (((symbol-function 'file-executable-p) (lambda (_f) nil)))
    (should-not (claude-repl--find-install-script "/repo"))))

(ert-deftest claude-repl-test-docker-image-exists-p-true ()
  "docker-image-exists-p should return non-nil when docker inspect returns 0."
  (cl-letf (((symbol-function 'call-process)
             (lambda (_prog _infile _dest _display &rest _args) 0)))
    (should (claude-repl--docker-image-exists-p "my-image:latest"))))

(ert-deftest claude-repl-test-docker-image-exists-p-false ()
  "docker-image-exists-p should return nil when docker inspect returns non-zero."
  (cl-letf (((symbol-function 'call-process)
             (lambda (_prog _infile _dest _display &rest _args) 1)))
    (should-not (claude-repl--docker-image-exists-p "no-such-image"))))

(ert-deftest claude-repl-test-resolve-sandbox-config-no-launcher ()
  "resolve-sandbox-config should return nil when no launcher script is found."
  (cl-letf (((symbol-function 'claude-repl--find-sandbox-script) (lambda (_r) nil)))
    (should-not (claude-repl--resolve-sandbox-config "/repo"))))

(ert-deftest claude-repl-test-resolve-sandbox-config-image-ready ()
  "resolve-sandbox-config should return (:image IMAGE :script SCRIPT) when image exists."
  (cl-letf (((symbol-function 'claude-repl--find-sandbox-script)
             (lambda (_r) "/usr/bin/claude-sandbox"))
            ((symbol-function 'claude-repl--query-sandbox-image)
             (lambda (_s) "my-image:latest"))
            ((symbol-function 'claude-repl--docker-image-exists-p)
             (lambda (_i) t)))
    (let ((result (claude-repl--resolve-sandbox-config "/repo")))
      (should (equal (plist-get result :image) "my-image:latest"))
      (should (equal (plist-get result :script) "/usr/bin/claude-sandbox"))
      (should-not (plist-get result :needs-build)))))

(ert-deftest claude-repl-test-resolve-sandbox-config-needs-build ()
  "resolve-sandbox-config should return :needs-build when image does not exist."
  (cl-letf (((symbol-function 'claude-repl--find-sandbox-script)
             (lambda (_r) "/usr/bin/claude-sandbox"))
            ((symbol-function 'claude-repl--query-sandbox-image)
             (lambda (_s) "my-image:latest"))
            ((symbol-function 'claude-repl--docker-image-exists-p)
             (lambda (_i) nil))
            ((symbol-function 'claude-repl--find-install-script)
             (lambda (_r) "/repo/.agents-sandbox/install-claude.sh")))
    (let ((result (claude-repl--resolve-sandbox-config "/repo")))
      (should (plist-get result :needs-build))
      (should (equal (plist-get result :image) "my-image:latest"))
      (should (equal (plist-get result :install-script)
                     "/repo/.agents-sandbox/install-claude.sh")))))

(ert-deftest claude-repl-test-resolve-sandbox-config-empty-image ()
  "resolve-sandbox-config should return nil when launcher outputs empty image."
  (cl-letf (((symbol-function 'claude-repl--find-sandbox-script)
             (lambda (_r) "/usr/bin/claude-sandbox"))
            ((symbol-function 'claude-repl--query-sandbox-image)
             (lambda (_s) nil)))
    (should-not (claude-repl--resolve-sandbox-config "/repo"))))

;;;; ---- Tests: Command building ----

(ert-deftest claude-repl-test-compute-claude-flags-continue ()
  "compute-claude-flags should emit --continue when session-id is set and no fork."
  (should (equal (claude-repl--compute-claude-flags "abc123" nil nil)
                 "--continue")))

(ert-deftest claude-repl-test-compute-claude-flags-no-continue-without-session-id ()
  "compute-claude-flags should not emit --continue when session-id is nil."
  (should (equal (claude-repl--compute-claude-flags nil nil nil) "")))

(ert-deftest claude-repl-test-compute-claude-flags-fork ()
  "compute-claude-flags should emit --resume <id> --fork-session for forks."
  (should (equal (claude-repl--compute-claude-flags "current" "fork-id" nil)
                 "--resume fork-id --fork-session")))

(ert-deftest claude-repl-test-compute-claude-flags-fork-ignores-session ()
  "compute-claude-flags with fork should not also emit --continue for session-id."
  (let ((result (claude-repl--compute-claude-flags "current" "fork-id" nil)))
    (should (string-match-p "--resume fork-id --fork-session" result))
    (should-not (string-match-p "--continue" result))))

(ert-deftest claude-repl-test-compute-claude-flags-perm-flag ()
  "compute-claude-flags should include permission flag when provided."
  (should (equal (claude-repl--compute-claude-flags nil nil "--permission-mode auto")
                 "--permission-mode auto")))

(ert-deftest claude-repl-test-compute-claude-flags-all-nil ()
  "compute-claude-flags should return empty string when all args are nil."
  (should (equal (claude-repl--compute-claude-flags nil nil nil) "")))

(ert-deftest claude-repl-test-compute-claude-flags-continue-plus-perm ()
  "compute-claude-flags should combine --continue and perm flag."
  (should (equal (claude-repl--compute-claude-flags "sess1" nil "--dangerously-skip-permissions")
                 "--continue --dangerously-skip-permissions")))

(ert-deftest claude-repl-test-compute-perm-flag-sandboxed ()
  "compute-perm-flag should return nil when sandboxed."
  (should-not (claude-repl--compute-perm-flag t "/some/dir")))

(ert-deftest claude-repl-test-compute-perm-flag-chesscom ()
  "compute-perm-flag should return --permission-mode auto for ChessCom repos."
  (should (equal (claude-repl--compute-perm-flag nil "/home/user/ChessCom/project")
                 "--permission-mode auto")))

(ert-deftest claude-repl-test-compute-perm-flag-personal ()
  "compute-perm-flag should return --dangerously-skip-permissions for personal repos."
  (should (equal (claude-repl--compute-perm-flag nil "/home/user/personal/project")
                 "--dangerously-skip-permissions")))

(ert-deftest claude-repl-test-compute-perm-flag-nil-dir ()
  "compute-perm-flag with nil project-dir should signal an error."
  (should-error (claude-repl--compute-perm-flag nil nil) :type 'error))

(ert-deftest claude-repl-test-compute-perm-flag-chesscom-dir ()
  "compute-perm-flag with ChessCom project-dir returns auto permissions."
  (should (equal (claude-repl--compute-perm-flag nil "/tmp/ChessCom/test")
                 "--permission-mode auto")))

(ert-deftest claude-repl-test-assemble-cmd-sandboxed ()
  "assemble-cmd should use sandbox script when sandboxed."
  (let ((config (list :script "/usr/bin/claude-sandbox" :image "img")))
    (should (equal (claude-repl--assemble-cmd config t "--resume abc")
                   "/usr/bin/claude-sandbox --resume abc"))))

(ert-deftest claude-repl-test-assemble-cmd-bare-metal ()
  "assemble-cmd should use 'claude' when not sandboxed."
  (should (equal (claude-repl--assemble-cmd nil nil "--resume abc")
                 "claude --resume abc")))

(ert-deftest claude-repl-test-assemble-cmd-no-flags ()
  "assemble-cmd with empty flags should produce clean command."
  (should (equal (claude-repl--assemble-cmd nil nil "") "claude")))

;;;; ---- Tests: Sandbox mode-line ----

(ert-deftest claude-repl-test-sandbox-mode-line-sandboxed ()
  "sandbox-mode-line should include DOCKER SANDBOX when sandboxed."
  (let ((result (claude-repl--sandbox-mode-line t "my-image:latest")))
    (should (listp result))
    (should (string-match-p "DOCKER SANDBOX" (car result)))
    (should (string-match-p "my-image:latest" (car result)))))

(ert-deftest claude-repl-test-sandbox-mode-line-bare-metal ()
  "sandbox-mode-line should include BARE METAL when not sandboxed."
  (let ((result (claude-repl--sandbox-mode-line nil nil)))
    (should (listp result))
    (should (string-match-p "BARE METAL" (car result)))))

;;;; ---- Tests: Session completion handling ----

(ert-deftest claude-repl-test-mark-claude-done-sets-done ()
  "mark-claude-done sets :claude-state :done unconditionally."
  (claude-repl-test--with-clean-state
    (let ((done-set nil))
      (cl-letf (((symbol-function 'claude-repl--ws-set-claude-state)
                 (lambda (ws state)
                   (when (eq state :done) (setq done-set ws)))))
        (claude-repl--mark-claude-done "ws1")
        (should (equal done-set "ws1"))))))

(ert-deftest claude-repl-test-mark-claude-done-regardless-of-visibility ()
  "mark-claude-done no longer branches on vterm visibility.
The previous mark-done-if-hidden used the vterm window as a \"user is
already looking\" gate. Post-axis-split that gate is the renderer's job."
  (claude-repl-test--with-clean-state
    (let ((done-set nil))
      ;; Any hypothetical visibility — mark-claude-done does not read it.
      (cl-letf (((symbol-function 'get-buffer-window)
                 (lambda (&rest _) 'some-window))
                ((symbol-function 'claude-repl--ws-set-claude-state)
                 (lambda (ws state)
                   (when (eq state :done) (setq done-set ws)))))
        (claude-repl--mark-claude-done "ws1")
        (should (equal done-set "ws1"))))))

(ert-deftest claude-repl-test-refresh-vterm-after-finish-live ()
  "refresh-vterm-after-finish should call do-refresh and update-hide-overlay for live buffer."
  (let ((refreshed nil)
        (overlay-updated nil)
        (scroll-fixed nil)
        (fake-buf (generate-new-buffer " *test-refresh*")))
    (unwind-protect
        (cl-letf (((symbol-function 'claude-repl--do-refresh)
                   (lambda () (setq refreshed t)))
                  ((symbol-function 'claude-repl--update-hide-overlay)
                   (lambda () (setq overlay-updated t)))
                  ((symbol-function 'claude-repl--fix-vterm-scroll)
                   (lambda (_buf) (setq scroll-fixed t))))
          (claude-repl--refresh-vterm-after-finish fake-buf)
          (should refreshed)
          (should overlay-updated)
          (should scroll-fixed))
      (kill-buffer fake-buf))))

(ert-deftest claude-repl-test-refresh-vterm-after-finish-dead ()
  "refresh-vterm-after-finish should be a no-op for a dead buffer."
  (let ((refreshed nil)
        (fake-buf (generate-new-buffer " *test-refresh-dead*")))
    (kill-buffer fake-buf)
    (cl-letf (((symbol-function 'claude-repl--do-refresh)
               (lambda () (setq refreshed t)))
              ((symbol-function 'claude-repl--fix-vterm-scroll) #'ignore))
      (claude-repl--refresh-vterm-after-finish fake-buf)
      (should-not refreshed))))

(ert-deftest claude-repl-test-handle-claude-finished-notifies-other-ws ()
  "handle-claude-finished should message when WS is not the current workspace."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :project-dir "/tmp/fake")
    (let ((messaged nil))
      (cl-letf (((symbol-function 'claude-repl--maybe-notify-finished) #'ignore)
                ((symbol-function '+workspace-current-name) (lambda () "other-ws"))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (when (string-match-p "Claude finished" fmt)
                     (setq messaged t)))))
        (claude-repl--handle-claude-finished "ws1")
        (should messaged)))))

(ert-deftest claude-repl-test-handle-claude-finished-no-message-current-ws ()
  "handle-claude-finished should NOT message when WS is the current workspace."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :project-dir "/tmp/fake")
    (let ((messaged nil))
      (cl-letf (((symbol-function 'claude-repl--maybe-notify-finished) #'ignore)
                ((symbol-function '+workspace-current-name) (lambda () "ws1"))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (when (string-match-p "Claude finished" fmt)
                     (setq messaged t)))))
        (claude-repl--handle-claude-finished "ws1")
        (should-not messaged)))))

(ert-deftest claude-repl-test-handle-claude-finished-errors-on-unregistered-ws ()
  "handle-claude-finished errors hard when WS is not registered — guards
against stop events arriving after kill."
  (claude-repl-test--with-clean-state
    (should-error (claude-repl--handle-claude-finished "not-a-ws"))))

;;;; ---- Tests: maybe-notify-finished edge cases ----

(ert-deftest claude-repl-test-maybe-notify-first-call-no-last-time ()
  "maybe-notify-finished should notify on first call (no :last-notify-time set)."
  (claude-repl-test--with-clean-state
    (let ((notify-count 0))
      (cl-letf (((symbol-function 'frame-focus-state) (lambda () nil))
                ((symbol-function 'run-at-time)
                 (lambda (_delay _repeat _fn &rest _args)
                   (cl-incf notify-count))))
        (claude-repl--maybe-notify-finished "ws1")
        (should (= notify-count 1))))))

(ert-deftest claude-repl-test-maybe-notify-stores-time ()
  "maybe-notify-finished should store :last-notify-time after notifying."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'frame-focus-state) (lambda () nil))
              ((symbol-function 'run-at-time) #'ignore))
      (claude-repl--maybe-notify-finished "ws1")
      (should (numberp (claude-repl--ws-get "ws1" :last-notify-time))))))

;;;; ---- Tests: Readiness handling ----

(ert-deftest claude-repl-test-drain-pending-prompts-empty ()
  "drain-pending-prompts should return nil when no pending prompts."
  (claude-repl-test--with-clean-state
    (should-not (claude-repl--drain-pending-prompts "ws1"))))

(ert-deftest claude-repl-test-drain-pending-prompts-sends ()
  "drain-pending-prompts should clear prompts and schedule delivery."
  (claude-repl-test--with-clean-state
    (let ((timer-scheduled nil)
          (fake-buf (generate-new-buffer " *test-drain*")))
      (unwind-protect
          (progn
            (claude-repl--ws-put "ws1" :pending-prompts '("prompt1" "prompt2"))
            (claude-repl--ws-put "ws1" :vterm-buffer fake-buf)
            (cl-letf (((symbol-function 'run-at-time)
                       (lambda (_delay _repeat fn &rest args)
                         (setq timer-scheduled t))))
              (should (claude-repl--drain-pending-prompts "ws1"))
              (should timer-scheduled)
              (should-not (claude-repl--ws-get "ws1" :pending-prompts))))
        (kill-buffer fake-buf)))))

(ert-deftest claude-repl-test-deliver-pending-prompts-live-buf ()
  "deliver-pending-prompts should call send for each prompt when buffer is live."
  (let ((sent nil)
        (fake-buf (generate-new-buffer " *test-deliver*")))
    (unwind-protect
        (cl-letf (((symbol-function 'claude-repl--send)
                   (lambda (p _ws) (push p sent))))
          (claude-repl--deliver-pending-prompts fake-buf '("a" "b") "ws1")
          (should (equal (reverse sent) '("a" "b"))))
      (kill-buffer fake-buf))))

(ert-deftest claude-repl-test-deliver-pending-prompts-dead-buf ()
  "deliver-pending-prompts should signal an error when buffer is dead."
  (let ((fake-buf (generate-new-buffer " *test-deliver-dead*")))
    (kill-buffer fake-buf)
    (should-error (claude-repl--deliver-pending-prompts fake-buf '("a") "ws1")
                  :type 'error)))

(ert-deftest claude-repl-test-show-panels-or-defer-current-ws ()
  "show-panels-or-defer should call claude-repl when WS is current."
  (claude-repl-test--with-clean-state
    (let ((panels-opened nil))
      (cl-letf (((symbol-function 'claude-repl--current-ws-p) (lambda (_ws) t))
                ((symbol-function 'claude-repl)
                 (lambda () (setq panels-opened t))))
        (claude-repl--show-panels-or-defer "ws1")
        (should panels-opened)))))

(ert-deftest claude-repl-test-show-panels-or-defer-other-ws ()
  "show-panels-or-defer should set :pending-show-panels when WS is not current."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'claude-repl--current-ws-p) (lambda (_ws) nil)))
      (claude-repl--show-panels-or-defer "ws1")
      (should (claude-repl--ws-get "ws1" :pending-show-panels)))))

(ert-deftest claude-repl-test-open-panels-after-ready-with-pending ()
  "open-panels-after-ready should show panels when there are pending prompts."
  (claude-repl-test--with-clean-state
    (let ((shown nil))
      (cl-letf (((symbol-function 'claude-repl--drain-pending-prompts)
                 (lambda (_ws) '("prompt1")))
                ((symbol-function 'claude-repl--show-panels-or-defer)
                 (lambda (_ws) (setq shown t))))
        (claude-repl--open-panels-after-ready "ws1")
        (should shown)))))

(ert-deftest claude-repl-test-open-panels-after-ready-no-pending-current ()
  "open-panels-after-ready should open panels when no pending prompts and WS is current."
  (claude-repl-test--with-clean-state
    (let ((panels-opened nil))
      (cl-letf (((symbol-function 'claude-repl--drain-pending-prompts)
                 (lambda (_ws) nil))
                ((symbol-function 'claude-repl--current-ws-p) (lambda (_ws) t))
                ((symbol-function 'claude-repl)
                 (lambda () (setq panels-opened t))))
        (claude-repl--open-panels-after-ready "ws1")
        (should panels-opened)))))

(ert-deftest claude-repl-test-open-panels-after-ready-no-pending-other ()
  "open-panels-after-ready should NOT open panels when no pending prompts and WS is other."
  (claude-repl-test--with-clean-state
    (let ((panels-opened nil))
      (cl-letf (((symbol-function 'claude-repl--drain-pending-prompts)
                 (lambda (_ws) nil))
                ((symbol-function 'claude-repl--current-ws-p) (lambda (_ws) nil))
                ((symbol-function 'claude-repl)
                 (lambda () (setq panels-opened t))))
        (claude-repl--open-panels-after-ready "ws1")
        (should-not panels-opened)))))

;;;; ---- Tests: Loading placeholder ----

(ert-deftest claude-repl-test-swap-placeholder-into-windows-live ()
  "swap-placeholder-into-windows should replace placeholder and kill it."
  (let ((real-buf (generate-new-buffer " *test-real-buf*"))
        (placeholder (generate-new-buffer " *test-placeholder*")))
    (unwind-protect
        (progn
          ;; No windows in batch mode, so just verify placeholder is killed
          (cl-letf (((symbol-function 'get-buffer-window-list)
                     (lambda (_buf _minibuf _all-frames) nil)))
            (claude-repl--swap-placeholder-into-windows real-buf placeholder)
            (should-not (buffer-live-p placeholder))))
      (when (buffer-live-p real-buf) (kill-buffer real-buf))
      (when (buffer-live-p placeholder) (kill-buffer placeholder)))))

(ert-deftest claude-repl-test-swap-placeholder-into-windows-dead-buf ()
  "swap-placeholder-into-windows should not kill placeholder when buf is dead."
  (let ((real-buf (generate-new-buffer " *test-dead-real*"))
        (placeholder (generate-new-buffer " *test-placeholder-2*")))
    (kill-buffer real-buf)
    (unwind-protect
        (progn
          (claude-repl--swap-placeholder-into-windows real-buf placeholder)
          (should (buffer-live-p placeholder)))
      (when (buffer-live-p placeholder) (kill-buffer placeholder)))))

;;;; ---- Tests: Process state predicates ----

(ert-deftest claude-repl-test-vterm-process-alive-no-buffer ()
  "vterm-process-alive-p should return nil when no vterm buffer is set."
  (claude-repl-test--with-clean-state
    (should-not (claude-repl--vterm-process-alive-p "ws1"))))

(ert-deftest claude-repl-test-vterm-process-alive-dead-buffer ()
  "vterm-process-alive-p should return nil when buffer has been killed."
  (claude-repl-test--with-clean-state
    (let ((buf (generate-new-buffer " *test-dead-vterm*")))
      (claude-repl--ws-put "ws1" :vterm-buffer buf)
      (kill-buffer buf)
      (should-not (claude-repl--vterm-process-alive-p "ws1")))))

(ert-deftest claude-repl-test-vterm-process-alive-no-process ()
  "vterm-process-alive-p should return nil when buffer has no process."
  (claude-repl-test--with-clean-state
    (let ((buf (generate-new-buffer " *test-no-process*")))
      (unwind-protect
          (progn
            (claude-repl--ws-put "ws1" :vterm-buffer buf)
            (should-not (claude-repl--vterm-process-alive-p "ws1")))
        (kill-buffer buf)))))

(ert-deftest claude-repl-test-session-starting-p-not-running ()
  "session-starting-p should return nil when vterm is not running."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'claude-repl--vterm-process-alive-p)
               (lambda (_ws) nil)))
      (should-not (claude-repl--session-starting-p "ws1")))))

(ert-deftest claude-repl-test-session-starting-p-ready ()
  "session-starting-p should return nil when process is alive but already ready."
  (claude-repl-test--with-clean-state
    (let ((buf (generate-new-buffer " *test-starting-ready*")))
      (unwind-protect
          (progn
            (with-current-buffer buf (setq-local claude-repl--ready t))
            (claude-repl--ws-put "ws1" :vterm-buffer buf)
            (cl-letf (((symbol-function 'claude-repl--vterm-process-alive-p)
                       (lambda (_ws) t)))
              (should-not (claude-repl--session-starting-p "ws1"))))
        (kill-buffer buf)))))

(ert-deftest claude-repl-test-session-starting-p-true ()
  "session-starting-p should return t when process alive but not ready."
  (claude-repl-test--with-clean-state
    (let ((buf (generate-new-buffer " *test-starting*")))
      (unwind-protect
          (progn
            (with-current-buffer buf (setq-local claude-repl--ready nil))
            (claude-repl--ws-put "ws1" :vterm-buffer buf)
            (cl-letf (((symbol-function 'claude-repl--vterm-process-alive-p)
                       (lambda (_ws) t)))
              (should (claude-repl--session-starting-p "ws1"))))
        (kill-buffer buf)))))

;;;; ---- Tests: Readiness timer ----

(ert-deftest claude-repl-test-cancel-ready-timer-no-timer ()
  "cancel-ready-timer should be a no-op when no timer is set."
  (claude-repl-test--with-clean-state
    (claude-repl--cancel-ready-timer "ws1")
    (should-not (claude-repl--ws-get "ws1" :ready-timer))))

(ert-deftest claude-repl-test-cancel-ready-timer-with-timer ()
  "cancel-ready-timer should cancel and clear the timer."
  (claude-repl-test--with-clean-state
    (let ((cancelled nil)
          (fake-timer (run-at-time 999 nil #'ignore)))
      (claude-repl--ws-put "ws1" :ready-timer fake-timer)
      (claude-repl--cancel-ready-timer "ws1")
      (should-not (claude-repl--ws-get "ws1" :ready-timer)))))

(ert-deftest claude-repl-test-ready-timer-tick-timeout ()
  "ready-timer-tick should cancel timer after 30s timeout."
  (claude-repl-test--with-clean-state
    (let ((cancelled nil))
      (cl-letf (((symbol-function 'claude-repl--cancel-ready-timer)
                 (lambda (_ws) (setq cancelled t))))
        (claude-repl--ready-timer-tick "ws1" (- (float-time) 31.0))
        (should cancelled)))))

(ert-deftest claude-repl-test-ready-timer-tick-still-starting ()
  "ready-timer-tick should do nothing when session is still starting."
  (claude-repl-test--with-clean-state
    (let ((cancelled nil))
      (cl-letf (((symbol-function 'claude-repl--cancel-ready-timer)
                 (lambda (_ws) (setq cancelled t)))
                ((symbol-function 'claude-repl--session-starting-p)
                 (lambda (_ws) t)))
        (claude-repl--ready-timer-tick "ws1" (float-time))
        (should-not cancelled)))))

(ert-deftest claude-repl-test-ready-timer-tick-ready-current-ws ()
  "ready-timer-tick should cancel timer and open panels when ready and current."
  (claude-repl-test--with-clean-state
    (let ((cancelled nil)
          (panels-opened nil))
      (cl-letf (((symbol-function 'claude-repl--cancel-ready-timer)
                 (lambda (_ws) (setq cancelled t)))
                ((symbol-function 'claude-repl--session-starting-p)
                 (lambda (_ws) nil))
                ((symbol-function 'claude-repl--current-ws-p) (lambda (_ws) t))
                ((symbol-function 'claude-repl)
                 (lambda () (setq panels-opened t))))
        (claude-repl--ready-timer-tick "ws1" (float-time))
        (should cancelled)
        (should panels-opened)))))

(ert-deftest claude-repl-test-ready-timer-tick-ready-other-ws ()
  "ready-timer-tick should cancel timer but not open panels when ready and not current."
  (claude-repl-test--with-clean-state
    (let ((cancelled nil)
          (panels-opened nil))
      (cl-letf (((symbol-function 'claude-repl--cancel-ready-timer)
                 (lambda (_ws) (setq cancelled t)))
                ((symbol-function 'claude-repl--session-starting-p)
                 (lambda (_ws) nil))
                ((symbol-function 'claude-repl--current-ws-p) (lambda (_ws) nil))
                ((symbol-function 'claude-repl)
                 (lambda () (setq panels-opened t))))
        (claude-repl--ready-timer-tick "ws1" (float-time))
        (should cancelled)
        (should-not panels-opened)))))

(ert-deftest claude-repl-test-schedule-ready-timer ()
  "schedule-ready-timer should cancel existing timer and schedule new one."
  (claude-repl-test--with-clean-state
    (let ((cancel-count 0))
      (cl-letf (((symbol-function 'claude-repl--cancel-ready-timer)
                 (lambda (_ws) (cl-incf cancel-count)))
                ((symbol-function 'run-at-time)
                 (lambda (_delay _repeat _fn &rest _args) 'fake-timer)))
        (claude-repl--schedule-ready-timer "ws1")
        (should (= cancel-count 1))
        (should (eq (claude-repl--ws-get "ws1" :ready-timer) 'fake-timer))))))

;;;; ---- Tests: Workspace environment initialization ----

(ert-deftest claude-repl-test-initialize-ws-env-initializes-fresh ()
  "initialize-ws-env on a fresh workspace with no state file should set up
default `:active-env' and instantiation structs for each environment."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'claude-repl--read-state-file)
               (lambda (_ws) nil)))
      (claude-repl--initialize-ws-env "ws1")
      (should (eq (claude-repl--ws-get "ws1" :active-env) :bare-metal))
      (should (claude-repl-instantiation-p (claude-repl--ws-get "ws1" :sandbox)))
      (should (claude-repl-instantiation-p (claude-repl--ws-get "ws1" :bare-metal))))))

(ert-deftest claude-repl-test-initialize-ws-env-errors-when-already-initialized ()
  "initialize-ws-env signals an error when the workspace already has
`:active-env' set.  The function is a one-time initializer; a second call
indicates a caller-side contract violation, so failing loudly is preferable
to silently re-running and clobbering session-ids on the instantiation structs."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :active-env :sandbox)
    (should-error (claude-repl--initialize-ws-env "ws1") :type 'error)))

;;;; ---- Tests: prompt-sandbox-build ----

(ert-deftest claude-repl-test-prompt-sandbox-build-yes-compiles-and-errors ()
  "prompt-sandbox-build should call compile then signal user-error when user says yes."
  (let ((compiled-cmd nil))
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) t))
              ((symbol-function 'compile)
               (lambda (cmd) (setq compiled-cmd cmd))))
      (should-error
       (claude-repl--prompt-sandbox-build
        '(:image "my-img:latest" :install-script "/repo/.agents-sandbox/install-claude.sh"))
       :type 'user-error)
      (should (string-match-p "install-claude.sh" compiled-cmd)))))

(ert-deftest claude-repl-test-prompt-sandbox-build-no-falls-through ()
  "prompt-sandbox-build should return nil (no error) when user answers no."
  (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) nil))
            ((symbol-function 'compile) #'ignore))
    ;; Should NOT signal user-error -- just returns nil
    (should-not
     (claude-repl--prompt-sandbox-build
      '(:image "my-img:latest" :install-script "/repo/.agents-sandbox/install-claude.sh")))))

(ert-deftest claude-repl-test-prompt-sandbox-build-no-install-script ()
  "prompt-sandbox-build should signal user-error with manual instruction when no install-script."
  (should-error
   (claude-repl--prompt-sandbox-build '(:image "my-img:latest" :install-script nil))
   :type 'user-error))

(ert-deftest claude-repl-test-prompt-sandbox-build-empty-image ()
  "prompt-sandbox-build with empty image should still signal user-error when no install-script."
  (should-error
   (claude-repl--prompt-sandbox-build '(:image "" :install-script nil))
   :type 'user-error))

;;;; ---- Tests: ensure-sandbox-image ----

(ert-deftest claude-repl-test-ensure-sandbox-image-non-worktree ()
  "ensure-sandbox-image should return nil for non-worktree workspaces."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :worktree-p nil)
    (claude-repl--ws-put "ws1" :active-env :bare-metal)
    (let ((resolve-called nil))
      (cl-letf (((symbol-function 'claude-repl--resolve-sandbox-config)
                 (lambda (_r) (setq resolve-called t) nil)))
        (should-not (claude-repl--ensure-sandbox-image "ws1"))
        (should-not resolve-called)))))

(ert-deftest claude-repl-test-ensure-sandbox-image-not-sandbox-env ()
  "ensure-sandbox-image should return nil when :active-env is not :sandbox."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :worktree-p t)
    (claude-repl--ws-put "ws1" :active-env :bare-metal)
    (let ((resolve-called nil))
      (cl-letf (((symbol-function 'claude-repl--resolve-sandbox-config)
                 (lambda (_r) (setq resolve-called t) nil)))
        (should-not (claude-repl--ensure-sandbox-image "ws1"))
        (should-not resolve-called)))))

(ert-deftest claude-repl-test-ensure-sandbox-image-exists ()
  "ensure-sandbox-image should return config plist when image is ready."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :worktree-p t)
    (claude-repl--ws-put "ws1" :active-env :sandbox)
    (claude-repl--ws-put "ws1" :project-dir "/my/project")
    (cl-letf (((symbol-function 'claude-repl--git-root) (lambda (_d) "/my/project"))
              ((symbol-function 'claude-repl--resolve-sandbox-config)
               (lambda (_r) '(:image "img:latest" :script "/usr/bin/claude-sandbox"))))
      (let ((result (claude-repl--ensure-sandbox-image "ws1")))
        (should (equal (plist-get result :image) "img:latest"))
        (should (equal (plist-get result :script) "/usr/bin/claude-sandbox"))))))

(ert-deftest claude-repl-test-ensure-sandbox-image-needs-build ()
  "ensure-sandbox-image should call prompt-sandbox-build when image needs building."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :worktree-p t)
    (claude-repl--ws-put "ws1" :active-env :sandbox)
    (claude-repl--ws-put "ws1" :project-dir "/my/project")
    (let ((prompt-called nil))
      (cl-letf (((symbol-function 'claude-repl--git-root) (lambda (_d) "/my/project"))
                ((symbol-function 'claude-repl--resolve-sandbox-config)
                 (lambda (_r) '(:needs-build t :image "img:latest"
                                :install-script "/repo/install.sh")))
                ((symbol-function 'claude-repl--prompt-sandbox-build)
                 (lambda (_cfg) (setq prompt-called t))))
        (claude-repl--ensure-sandbox-image "ws1")
        (should prompt-called)))))

(ert-deftest claude-repl-test-ensure-sandbox-image-nil-project-dir ()
  "ensure-sandbox-image should return nil when :project-dir is nil."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :worktree-p t)
    (claude-repl--ws-put "ws1" :active-env :sandbox)
    (claude-repl--ws-put "ws1" :project-dir nil)
    (cl-letf (((symbol-function 'claude-repl--git-root) (lambda (_d) nil))
              ((symbol-function 'claude-repl--resolve-sandbox-config)
               (lambda (_r) nil)))
      (should-not (claude-repl--ensure-sandbox-image "ws1")))))

;;;; ---- Tests: build-start-cmd ----

(ert-deftest claude-repl-test-build-start-cmd-bare-metal-no-session ()
  "build-start-cmd for bare-metal with no session should produce claude --dangerously-skip-permissions."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :active-env :bare-metal)
    (claude-repl--ws-put "ws1" :bare-metal (make-claude-repl-instantiation))
    (claude-repl--ws-put "ws1" :worktree-p nil)
    (claude-repl--ws-put "ws1" :project-dir "/home/user/personal/project")
    (cl-letf (((symbol-function 'claude-repl--ensure-sandbox-image)
               (lambda (_ws) nil)))
      (let ((result (claude-repl--build-start-cmd "ws1")))
        (should (equal (plist-get result :cmd) "claude --dangerously-skip-permissions"))
        (should-not (plist-get result :sandboxed-p))))))

(ert-deftest claude-repl-test-build-start-cmd-sandbox-with-session ()
  "build-start-cmd for worktree sandbox with session should use sandbox script + --continue."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :active-env :sandbox)
    (claude-repl--ws-put "ws1" :sandbox
                         (make-claude-repl-instantiation :session-id "sess-123"))
    (claude-repl--ws-put "ws1" :worktree-p t)
    (claude-repl--ws-put "ws1" :project-dir "/home/user/project")
    (cl-letf (((symbol-function 'claude-repl--ensure-sandbox-image)
               (lambda (_ws) '(:image "img:latest" :script "/usr/bin/claude-sandbox"))))
      (let ((result (claude-repl--build-start-cmd "ws1")))
        (should (string-match-p "/usr/bin/claude-sandbox" (plist-get result :cmd)))
        (should (string-match-p "--continue" (plist-get result :cmd)))
        (should-not (string-match-p "--resume sess-123" (plist-get result :cmd)))
        (should (plist-get result :sandboxed-p))
        (should (equal (plist-get result :docker-image) "img:latest"))))))

(ert-deftest claude-repl-test-build-start-cmd-fork-session ()
  "build-start-cmd with fork-session-id should include --fork-session flag."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :active-env :bare-metal)
    (claude-repl--ws-put "ws1" :bare-metal (make-claude-repl-instantiation))
    (claude-repl--ws-put "ws1" :worktree-p nil)
    (claude-repl--ws-put "ws1" :project-dir "/home/user/personal/project")
    (claude-repl--ws-put "ws1" :fork-session-id "fork-abc")
    (cl-letf (((symbol-function 'claude-repl--ensure-sandbox-image)
               (lambda (_ws) nil)))
      (let ((result (claude-repl--build-start-cmd "ws1")))
        (should (string-match-p "--resume fork-abc --fork-session" (plist-get result :cmd)))
        (should (equal (plist-get result :fork-session-id) "fork-abc"))))))

(ert-deftest claude-repl-test-build-start-cmd-needs-build ()
  "build-start-cmd when sandbox needs-build should have nil docker-image and sandboxed-p false."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :active-env :sandbox)
    (claude-repl--ws-put "ws1" :sandbox (make-claude-repl-instantiation))
    (claude-repl--ws-put "ws1" :worktree-p t)
    (claude-repl--ws-put "ws1" :project-dir "/home/user/project")
    (cl-letf (((symbol-function 'claude-repl--ensure-sandbox-image)
               (lambda (_ws) '(:needs-build t :image "img:latest"))))
      (let ((result (claude-repl--build-start-cmd "ws1")))
        (should-not (plist-get result :docker-image))
        (should-not (plist-get result :sandboxed-p))))))

(ert-deftest claude-repl-test-build-start-cmd-nil-project-dir ()
  "build-start-cmd with personal :project-dir should produce a command."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :active-env :bare-metal)
    (claude-repl--ws-put "ws1" :bare-metal (make-claude-repl-instantiation))
    (claude-repl--ws-put "ws1" :worktree-p nil)
    (claude-repl--ws-put "ws1" :project-dir "/tmp/personal/project")
    (cl-letf (((symbol-function 'claude-repl--ensure-sandbox-image)
               (lambda (_ws) nil)))
      (let* ((result (claude-repl--build-start-cmd "ws1")))
        (should (stringp (plist-get result :cmd)))
        (should (string-match-p "claude" (plist-get result :cmd)))))))

(ert-deftest claude-repl-test-build-start-cmd-empty-session-id ()
  "build-start-cmd with nil session-id should not include --resume or --continue."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :active-env :bare-metal)
    (claude-repl--ws-put "ws1" :bare-metal (make-claude-repl-instantiation :session-id nil))
    (claude-repl--ws-put "ws1" :worktree-p nil)
    (claude-repl--ws-put "ws1" :project-dir "/home/user/personal/project")
    (cl-letf (((symbol-function 'claude-repl--ensure-sandbox-image)
               (lambda (_ws) nil)))
      (let ((result (claude-repl--build-start-cmd "ws1")))
        (should-not (string-match-p "--resume" (plist-get result :cmd)))
        (should-not (string-match-p "--continue" (plist-get result :cmd)))))))

;;;; ---- Tests: start-claude ----

(ert-deftest claude-repl-test-start-claude-normal-startup ()
  "start-claude should send clear && cmd and schedule ready timer."
  (claude-repl-test--with-temp-buffer " *test-start-claude*"
    (setq-local claude-repl--owning-workspace "ws1")
    (claude-repl-test--with-clean-state
      (let ((sent-string nil)
            (return-sent nil)
            (timer-scheduled nil))
        (cl-letf (((symbol-function 'claude-repl--initialize-ws-env) #'ignore)
                  ((symbol-function 'claude-repl--build-start-cmd)
                   (lambda (_ws) (list :cmd "claude --dangerously-skip-permissions"
                                       :sandboxed-p nil
                                       :docker-image nil
                                       :session-id nil
                                       :fork-session-id nil
                                       :worktree-p nil
                                       :active-env :bare-metal
                                       :inst (make-claude-repl-instantiation))))
                  ((symbol-function 'vterm-send-string)
                   (lambda (s) (setq sent-string s)))
                  ((symbol-function 'vterm-send-return)
                   (lambda () (setq return-sent t)))
                  ((symbol-function 'claude-repl--schedule-ready-timer)
                   (lambda (_ws) (setq timer-scheduled t)))
                  ((symbol-function 'claude-repl--log-session-start) #'ignore)
                  ((symbol-function 'claude-repl--sandbox-mode-line)
                   (lambda (_s _d) '("test"))))
          (claude-repl--start-claude "ws1")
          (should (string-match-p "clear && claude" sent-string))
          (should return-sent)
          (should timer-scheduled))))))

(ert-deftest claude-repl-test-start-claude-clears-fork-session-id ()
  "start-claude should clear :fork-session-id after building cmd with fork."
  (claude-repl-test--with-temp-buffer " *test-start-claude-fork*"
    (setq-local claude-repl--owning-workspace "ws1")
    (claude-repl-test--with-clean-state
      (claude-repl--ws-put "ws1" :fork-session-id "fork-abc")
      (cl-letf (((symbol-function 'claude-repl--initialize-ws-env) #'ignore)
                ((symbol-function 'claude-repl--build-start-cmd)
                 (lambda (_ws) (list :cmd "claude --resume fork-abc --fork-session"
                                     :sandboxed-p nil
                                     :docker-image nil
                                     :session-id nil
                                     :fork-session-id "fork-abc"
                                     :worktree-p nil
                                     :active-env :bare-metal
                                     :inst (make-claude-repl-instantiation))))
                ((symbol-function 'vterm-send-string) #'ignore)
                ((symbol-function 'vterm-send-return) #'ignore)
                ((symbol-function 'claude-repl--schedule-ready-timer) #'ignore)
                ((symbol-function 'claude-repl--log-session-start) #'ignore)
                ((symbol-function 'claude-repl--sandbox-mode-line)
                 (lambda (_s _d) '("test"))))
        (claude-repl--start-claude "ws1")
        (should-not (claude-repl--ws-get "ws1" :fork-session-id))))))

(ert-deftest claude-repl-test-start-claude-sets-mode-line-sandboxed ()
  "start-claude should set mode-line to sandboxed format when sandboxed."
  (claude-repl-test--with-temp-buffer " *test-start-claude-modeline*"
    (setq-local claude-repl--owning-workspace "ws1")
    (claude-repl-test--with-clean-state
      (cl-letf (((symbol-function 'claude-repl--initialize-ws-env) #'ignore)
                ((symbol-function 'claude-repl--build-start-cmd)
                 (lambda (_ws) (list :cmd "claude-sandbox"
                                     :sandboxed-p t
                                     :docker-image "img:latest"
                                     :session-id nil
                                     :fork-session-id nil
                                     :worktree-p t
                                     :active-env :sandbox
                                     :inst (make-claude-repl-instantiation))))
                ((symbol-function 'vterm-send-string) #'ignore)
                ((symbol-function 'vterm-send-return) #'ignore)
                ((symbol-function 'claude-repl--schedule-ready-timer) #'ignore)
                ((symbol-function 'claude-repl--log-session-start) #'ignore))
        (claude-repl--start-claude "ws1")
        (should (string-match-p "DOCKER SANDBOX" (car mode-line-format)))))))

(ert-deftest claude-repl-test-start-claude-sets-ready-nil ()
  "start-claude should set claude-repl--ready to nil before sending command."
  (claude-repl-test--with-temp-buffer " *test-start-ready-nil*"
    (setq-local claude-repl--owning-workspace "ws1")
    (setq-local claude-repl--ready t)
    (claude-repl-test--with-clean-state
      (let ((ready-at-send nil))
        (cl-letf (((symbol-function 'claude-repl--initialize-ws-env) #'ignore)
                  ((symbol-function 'claude-repl--build-start-cmd)
                   (lambda (_ws) (list :cmd "claude"
                                       :sandboxed-p nil
                                       :docker-image nil
                                       :session-id nil
                                       :fork-session-id nil
                                       :worktree-p nil
                                       :active-env :bare-metal
                                       :inst (make-claude-repl-instantiation))))
                  ((symbol-function 'vterm-send-string)
                   (lambda (_s) (setq ready-at-send claude-repl--ready)))
                  ((symbol-function 'vterm-send-return) #'ignore)
                  ((symbol-function 'claude-repl--schedule-ready-timer) #'ignore)
                  ((symbol-function 'claude-repl--log-session-start) #'ignore)
                  ((symbol-function 'claude-repl--sandbox-mode-line)
                   (lambda (_s _d) '("test"))))
          (claude-repl--start-claude "ws1")
          (should-not ready-at-send))))))

(ert-deftest claude-repl-test-start-claude-uses-explicit-ws-arg ()
  "start-claude should use its WS argument verbatim, ignoring buffer-local
`claude-repl--owning-workspace' and `+workspace-current-name'.  This is
load-bearing: when a worktree is created from another persp, the
creating persp's current-name is wrong, and the owning-workspace
buffer-local may not be pinned yet."
  (claude-repl-test--with-temp-buffer " *test-start-explicit-ws*"
    (setq-local claude-repl--owning-workspace "buffer-local-ws")
    (claude-repl-test--with-clean-state
      (let ((ws-used nil))
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "persp-current-ws"))
                  ((symbol-function 'claude-repl--initialize-ws-env)
                   (lambda (ws) (setq ws-used ws)))
                  ((symbol-function 'claude-repl--build-start-cmd)
                   (lambda (_ws) (list :cmd "claude"
                                       :sandboxed-p nil
                                       :docker-image nil
                                       :session-id nil
                                       :fork-session-id nil
                                       :worktree-p nil
                                       :active-env :bare-metal
                                       :inst (make-claude-repl-instantiation))))
                  ((symbol-function 'vterm-send-string) #'ignore)
                  ((symbol-function 'vterm-send-return) #'ignore)
                  ((symbol-function 'claude-repl--schedule-ready-timer) #'ignore)
                  ((symbol-function 'claude-repl--log-session-start) #'ignore)
                  ((symbol-function 'claude-repl--sandbox-mode-line)
                   (lambda (_s _d) '("test"))))
          (claude-repl--start-claude "explicit-arg-ws")
          (should (equal ws-used "explicit-arg-ws")))))))

(ert-deftest claude-repl-test-start-claude-skips-init-when-env-set ()
  "start-claude should skip initialize-ws-env when :active-env is already set.
This is the kill-then-restart path: the environment state survives kill
so re-initialization must not be attempted."
  (claude-repl-test--with-temp-buffer " *test-start-skip-init*"
    (setq-local claude-repl--owning-workspace "ws1")
    (claude-repl-test--with-clean-state
      (claude-repl--ws-put "ws1" :active-env :bare-metal)
      (claude-repl--ws-put "ws1" :bare-metal (make-claude-repl-instantiation))
      (claude-repl--ws-put "ws1" :sandbox (make-claude-repl-instantiation))
      (let ((init-called nil))
        (cl-letf (((symbol-function 'claude-repl--initialize-ws-env)
                   (lambda (_ws) (setq init-called t)))
                  ((symbol-function 'claude-repl--build-start-cmd)
                   (lambda (_ws) (list :cmd "claude"
                                       :sandboxed-p nil
                                       :docker-image nil
                                       :session-id nil
                                       :fork-session-id nil
                                       :worktree-p nil
                                       :active-env :bare-metal
                                       :inst (make-claude-repl-instantiation))))
                  ((symbol-function 'vterm-send-string) #'ignore)
                  ((symbol-function 'vterm-send-return) #'ignore)
                  ((symbol-function 'claude-repl--schedule-ready-timer) #'ignore)
                  ((symbol-function 'claude-repl--log-session-start) #'ignore)
                  ((symbol-function 'claude-repl--sandbox-mode-line)
                   (lambda (_s _d) '("test"))))
          (claude-repl--start-claude "ws1")
          (should-not init-called))))))

;;;; ---- Tests: log-session-start ----

(ert-deftest claude-repl-test-log-session-start-all-fields ()
  "log-session-start should format message with all fields present."
  (let ((logged-msg nil))
    (cl-letf (((symbol-function 'claude-repl--log)
               (lambda (_ws fmt &rest args) (setq logged-msg (apply #'format fmt args)))))
      (claude-repl--log-session-start "ws1"
                                      '(:cmd "claude --resume abc"
                                        :session-id "abc"
                                        :fork-session-id "fork-1"
                                        :worktree-p t
                                        :active-env :sandbox))
      (should (string-match-p "ws=ws1" logged-msg))
      (should (string-match-p "session-id=abc" logged-msg))
      (should (string-match-p "fork-session-id=fork-1" logged-msg))
      (should (string-match-p "worktree=yes" logged-msg))
      (should (string-match-p "env=:sandbox" logged-msg)))))

(ert-deftest claude-repl-test-log-session-start-nil-optional-fields ()
  "log-session-start should handle nil session-id and fork-session-id."
  (let ((logged-msg nil))
    (cl-letf (((symbol-function 'claude-repl--log)
               (lambda (_ws fmt &rest args) (setq logged-msg (apply #'format fmt args)))))
      (claude-repl--log-session-start "ws1"
                                      '(:cmd "claude"
                                        :session-id nil
                                        :fork-session-id nil
                                        :worktree-p nil
                                        :active-env :bare-metal))
      (should (string-match-p "session-id=nil" logged-msg))
      (should (string-match-p "fork-session-id=nil" logged-msg)))))

(ert-deftest claude-repl-test-log-session-start-worktree-formatting ()
  "log-session-start should format worktree-p as yes/no strings."
  (let ((msg-yes nil)
        (msg-no nil))
    (cl-letf (((symbol-function 'claude-repl--log)
               (lambda (_ws fmt &rest args) (setq msg-yes (apply #'format fmt args)))))
      (claude-repl--log-session-start "ws1"
                                      '(:cmd "claude" :worktree-p t :active-env :sandbox
                                        :session-id nil :fork-session-id nil)))
    (cl-letf (((symbol-function 'claude-repl--log)
               (lambda (_ws fmt &rest args) (setq msg-no (apply #'format fmt args)))))
      (claude-repl--log-session-start "ws1"
                                      '(:cmd "claude" :worktree-p nil :active-env :bare-metal
                                        :session-id nil :fork-session-id nil)))
    (should (string-match-p "worktree=yes" msg-yes))
    (should (string-match-p "worktree=no" msg-no))))

;;;; ---- Tests: claude-running-p ----

(ert-deftest claude-repl-test-claude-running-p-explicit-ws ()
  "claude-running-p with explicit ws should delegate to vterm-process-alive-p."
  (claude-repl-test--with-clean-state
    (let ((checked-ws nil))
      (cl-letf (((symbol-function 'claude-repl--vterm-process-alive-p)
                 (lambda (ws) (setq checked-ws ws) t)))
        (should (claude-repl--claude-running-p "my-ws"))
        (should (equal checked-ws "my-ws"))))))

(ert-deftest claude-repl-test-claude-running-p-nil-ws-uses-current ()
  "claude-running-p with nil ws should fall back to +workspace-current-name."
  (claude-repl-test--with-clean-state
    (let ((checked-ws nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "current-ws"))
                ((symbol-function 'claude-repl--vterm-process-alive-p)
                 (lambda (ws) (setq checked-ws ws) nil)))
        (should-not (claude-repl--claude-running-p))
        (should (equal checked-ws "current-ws"))))))

;;;; ---- Tests: session edge cases (status transitions .md) ----

(ert-deftest claude-repl-test-handle-claude-finished-second-notify-debounced ()
  "Two calls to handle-claude-finished within 2s should only produce one notification."
  (claude-repl-test--with-clean-state
    (let ((notify-count 0))
      (cl-letf (((symbol-function 'claude-repl--notify)
                 (lambda (&rest _) (cl-incf notify-count)))
                ((symbol-function 'claude-repl--do-refresh) #'ignore)
                ((symbol-function 'claude-repl--update-hide-overlay) #'ignore)
                ((symbol-function 'claude-repl--fix-vterm-scroll) #'ignore)
                ((symbol-function 'frame-focus-state) (lambda () nil))
                ((symbol-function 'run-at-time)
                 (lambda (_delay _repeat fn &rest args)
                   (apply fn args)))
                ((symbol-function '+workspace-current-name) (lambda () "other-ws"))
                ((symbol-function 'claude-repl--current-ws-p) (lambda (_ws) nil)))
        ;; Create a hidden vterm buffer for ws1
        (let ((vterm-buf (generate-new-buffer "*claude-panel-testfinish*")))
          (unwind-protect
              (progn
                (claude-repl--ws-put "ws1" :vterm-buffer vterm-buf)
                ;; First call — should notify
                (claude-repl--handle-claude-finished "ws1")
                (should (= notify-count 1))
                ;; Second call within 2s window — should be debounced
                (claude-repl--handle-claude-finished "ws1")
                (should (= notify-count 1)))
            (when (buffer-live-p vterm-buf)
              (kill-buffer vterm-buf))))))))

;;; test-session.el ends here
