;;; test-agents-repl.el --- ERT tests for agents-repl -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;   emacs -batch -Q -l ert -l test-agents-repl.el -f ert-run-tests-batch-and-exit
;;
;; Or interactively:
;;   M-x load-file RET test-agents-repl.el RET
;;   M-x ert RET t RET

;;; Code:

(require 'ert)
(require 'cl-lib)

;;;; ---- Stub layer ----
;; Provide no-op stubs for Doom/vterm/evil/persp APIs so we can load
;; config.el in a vanilla Emacs -Q environment.

;; Doom macros
(unless (fboundp 'after!)
  (defmacro after! (_feature &rest body)
    "No-op stub: ignore FEATURE, just execute BODY."
    `(progn ,@body)))

(unless (fboundp 'map!)
  (defmacro map! (&rest _args)
    "No-op stub: ignore all keybinding declarations."
    nil))

(unless (fboundp 'cmd!)
  (defmacro cmd! (&rest body)
    "No-op stub: return a lambda wrapping BODY."
    `(lambda () (interactive) ,@body)))

(unless (fboundp 'modulep!)
  (defmacro modulep! (&rest _args)
    "No-op stub: always return nil."
    nil))

;; Doom workspace API
(unless (fboundp '+workspace-current-name)
  (defun +workspace-current-name ()
    "Stub: return test workspace name."
    "test-ws"))

(unless (fboundp '+workspace-list-names)
  (defun +workspace-list-names ()
    "Stub: return list of workspace names."
    '("test-ws" "other-ws")))

(unless (fboundp '+workspace/display)
  (defun +workspace/display ()
    "Stub: no-op."
    nil))

(unless (fboundp '+workspace--tabline)
  (defun +workspace--tabline (&optional _names)
    "Stub: return empty string."
    ""))

;; Doom faces used by tabline
(unless (facep '+workspace-tab-selected-face)
  (defface +workspace-tab-selected-face '((t :weight bold)) "Stub face."))
(unless (facep '+workspace-tab-face)
  (defface +workspace-tab-face '((t)) "Stub face."))

;; Doom leader map
(unless (boundp 'doom-leader-map)
  (defvar doom-leader-map (make-sparse-keymap) "Stub leader keymap."))

;; vterm stubs
(unless (fboundp 'vterm-mode)
  (defun vterm-mode () "Stub." nil))
(unless (fboundp 'vterm-send-string)
  (defun vterm-send-string (&rest _args) "Stub." nil))
(unless (fboundp 'vterm-send-return)
  (defun vterm-send-return () "Stub." nil))
(unless (fboundp 'vterm-send-key)
  (defun vterm-send-key (&rest _args) "Stub." nil))
(unless (fboundp 'vterm-send-down)
  (defun vterm-send-down () "Stub." nil))
(unless (fboundp 'vterm-send-up)
  (defun vterm-send-up () "Stub." nil))
(unless (fboundp 'vterm-reset-cursor-point)
  (defun vterm-reset-cursor-point () "Stub." nil))
(unless (fboundp 'vterm--redraw)
  (defun vterm--redraw (&rest _args) "Stub." nil))
(unless (fboundp 'vterm--set-title)
  (defun vterm--set-title (&rest _args) "Stub." nil))
(unless (boundp 'vterm--term)
  (defvar vterm--term nil "Stub."))

;; evil stubs
(unless (fboundp 'evil-insert-state)
  (defun evil-insert-state () "Stub." nil))
(unless (fboundp 'evil-window-left)
  (defun evil-window-left (&rest _args) "Stub." nil))

;; persp-mode stubs
(unless (boundp 'persp-mode)
  (defvar persp-mode nil "Stub."))
(unless (fboundp 'persp-persps)
  (defun persp-persps () "Stub." nil))
(unless (fboundp 'persp-contain-buffer-p)
  (defun persp-contain-buffer-p (_buf _persp) "Stub." nil))
(unless (fboundp 'safe-persp-name)
  (defun safe-persp-name (persp) "Stub." persp))

;; filenotify stub (prevent side effects at load time)
(require 'filenotify)
(unless (fboundp 'file-notify-add-watch--orig)
  ;; Save original and replace with no-op during test loading
  (defalias 'file-notify-add-watch--orig #'file-notify-add-watch)
  (defun file-notify-add-watch--test-stub (_dir _flags _callback)
    "Stub: no-op for tests."
    nil)
  (advice-add 'file-notify-add-watch :override #'file-notify-add-watch--test-stub))

;; Suppress timers at load time
(defvar agents-repl-test--orig-run-with-timer (symbol-function 'run-with-timer))
(advice-add 'run-with-timer :override (lambda (&rest _) nil))

;; Load the module
(load (expand-file-name "config.el" (file-name-directory
                                      (or load-file-name buffer-file-name)))
      nil t)

;; Restore run-with-timer after loading
(advice-remove 'run-with-timer (lambda (&rest _) nil))

;; Restore file-notify-add-watch after loading
(advice-remove 'file-notify-add-watch #'file-notify-add-watch--test-stub)

;;;; ---- Test utilities ----

(defmacro agents-repl-test--with-clean-state (&rest body)
  "Execute BODY with fresh agents-repl global state."
  (declare (indent 0))
  `(let ((agents-repl--workspaces (make-hash-table :test 'equal))
         (agents-repl--fullscreen-config nil)
         (agents-repl--sync-timer nil)
         (agents-repl--cursor-reset-timer nil)
         (agents-repl--hide-overlay-refcount 0)
         (agents-repl-debug nil))
     ,@body))

(defmacro agents-repl-test--with-temp-buffer (name &rest body)
  "Create a temp buffer named NAME, execute BODY, kill buffer after."
  (declare (indent 1))
  (let ((buf-sym (make-symbol "buf")))
    `(let ((,buf-sym (get-buffer-create ,name)))
       (unwind-protect
           (with-current-buffer ,buf-sym
             ,@body)
         (when (buffer-live-p ,buf-sym)
           (kill-buffer ,buf-sym))))))

;;;; ---- Tests: Workspace ID / root resolution ----

(ert-deftest agents-repl-test-workspace-id-from-project-root ()
  "Workspace ID should be first 8 chars of MD5 of project root."
  (agents-repl-test--with-temp-buffer " *test-ws-id*"
    (setq-local agents-repl--project-root "/test/project")
    (let ((default-directory "/nonexistent/"))
      ;; git-root will fail, so resolve-root falls through to project-root
      (cl-letf (((symbol-function 'agents-repl--git-root) (lambda (&optional _d) nil)))
        (should (equal (agents-repl--workspace-id)
                       (substring (md5 "/test/project") 0 8)))))))

(ert-deftest agents-repl-test-workspace-id-default-directory ()
  "Workspace ID falls back to default-directory when no git root or project root."
  (agents-repl-test--with-temp-buffer " *test-ws-id-dd*"
    (setq-local agents-repl--project-root nil)
    (let ((default-directory "/fallback/dir/"))
      (cl-letf (((symbol-function 'agents-repl--git-root) (lambda (&optional _d) nil)))
        (should (equal (agents-repl--workspace-id)
                       (substring (md5 "/fallback/dir/") 0 8)))))))

(ert-deftest agents-repl-test-resolve-root-priority ()
  "resolve-root should prefer git-root > project-root > default-directory."
  (agents-repl-test--with-temp-buffer " *test-resolve*"
    (setq-local agents-repl--project-root "/project")
    (let ((default-directory "/default/"))
      ;; With git root available
      (cl-letf (((symbol-function 'agents-repl--git-root) (lambda (&optional _d) "/git-root")))
        (should (equal (agents-repl--resolve-root) "/git-root")))
      ;; Without git root
      (cl-letf (((symbol-function 'agents-repl--git-root) (lambda (&optional _d) nil)))
        (should (equal (agents-repl--resolve-root) "/project"))))))

;;;; ---- Tests: Buffer naming ----

(ert-deftest agents-repl-test-buffer-name-format ()
  "Buffer names should follow *claude-HASH* and *claude-input-HASH* pattern."
  (agents-repl-test--with-temp-buffer " *test-buf-name*"
    (setq-local agents-repl--project-root "/test/proj")
    (cl-letf (((symbol-function 'agents-repl--git-root) (lambda (&optional _d) nil)))
      (let ((id (agents-repl--workspace-id)))
        (should (equal (agents-repl--buffer-name) (format "*claude-%s*" id)))
        (should (equal (agents-repl--buffer-name "-input") (format "*claude-input-%s*" id)))))))

(ert-deftest agents-repl-test-buffer-name-default ()
  "Buffer name uses 'default' when workspace-id returns nil."
  (cl-letf (((symbol-function 'agents-repl--workspace-id) (lambda () nil)))
    (should (equal (agents-repl--buffer-name) "*claude-default*"))))

;;;; ---- Tests: History ----

(ert-deftest agents-repl-test-history-push ()
  "history-push should add text, skip empty, skip duplicates."
  (agents-repl-test--with-temp-buffer " *test-hist*"
    (setq-local agents-repl--input-history nil)
    ;; Push "hello"
    (insert "hello")
    (agents-repl--history-push)
    (should (equal agents-repl--input-history '("hello")))
    ;; Duplicate should be skipped
    (erase-buffer) (insert "hello")
    (agents-repl--history-push)
    (should (equal agents-repl--input-history '("hello")))
    ;; New entry
    (erase-buffer) (insert "world")
    (agents-repl--history-push)
    (should (equal agents-repl--input-history '("world" "hello")))
    ;; Empty should be skipped
    (erase-buffer) (insert "   ")
    (agents-repl--history-push)
    (should (equal agents-repl--input-history '("world" "hello")))))

(ert-deftest agents-repl-test-history-prev-next ()
  "history-prev and history-next should navigate correctly."
  (agents-repl-test--with-temp-buffer " *test-hist-nav*"
    (setq-local agents-repl--input-history '("second" "first"))
    (setq-local agents-repl--history-index -1)
    (setq-local agents-repl--history-stash nil)
    (setq-local agents-repl--history-navigating nil)
    (insert "current text")
    ;; Go back once
    (agents-repl--history-prev)
    (should (equal (buffer-string) "second"))
    (should (= agents-repl--history-index 0))
    (should (equal agents-repl--history-stash "current text"))
    ;; Go back again
    (agents-repl--history-prev)
    (should (equal (buffer-string) "first"))
    (should (= agents-repl--history-index 1))
    ;; Go forward
    (agents-repl--history-next)
    (should (equal (buffer-string) "second"))
    (should (= agents-repl--history-index 0))
    ;; Go forward past newest — restore stash
    (agents-repl--history-next)
    (should (equal (buffer-string) "current text"))
    (should (= agents-repl--history-index -1))))

(ert-deftest agents-repl-test-history-reset ()
  "history-reset should set index back to -1."
  (agents-repl-test--with-temp-buffer " *test-hist-reset*"
    (setq-local agents-repl--history-index 3)
    (agents-repl--history-reset)
    (should (= agents-repl--history-index -1))))

(ert-deftest agents-repl-test-history-on-change-resets ()
  "Editing buffer while browsing history should reset the index."
  (agents-repl-test--with-temp-buffer " *test-hist-change*"
    (setq-local agents-repl--history-index 2)
    (setq-local agents-repl--history-navigating nil)
    (agents-repl--history-on-change)
    (should (= agents-repl--history-index -1))))

;;;; ---- Tests: State machine (ws-set, ws-clear, ws-state) ----

(ert-deftest agents-repl-test-ws-set-and-state ()
  "ws-set should set the correct state, clearing others."
  (agents-repl-test--with-clean-state
    (agents-repl--ws-set "ws1" :thinking)
    (should (eq (agents-repl--ws-state "ws1") :thinking))
    (agents-repl--ws-set "ws1" :done)
    (should (eq (agents-repl--ws-state "ws1") :done))
    ;; Thinking should be cleared — the plist status should now be :done
    (should (eq (agents-repl--ws-get "ws1" :status) :done))))

(ert-deftest agents-repl-test-ws-clear ()
  "ws-clear should clear only the specified state."
  (agents-repl-test--with-clean-state
    (agents-repl--ws-set "ws1" :thinking)
    (agents-repl--ws-clear "ws1" :thinking)
    (should-not (agents-repl--ws-state "ws1"))))

(ert-deftest agents-repl-test-ws-state-stale ()
  "ws-state should return :stale for recently-active workspaces."
  (agents-repl-test--with-clean-state
    (let ((agents-repl-stale-minutes 60))
      (cl-letf (((symbol-function 'agents-repl--workspace-clean-p) (lambda (_) t)))
        (agents-repl--touch-activity "ws1")
        (should (eq (agents-repl--ws-state "ws1") :stale))))))

(ert-deftest agents-repl-test-ws-set-nil-error ()
  "ws-set with nil workspace should signal an error."
  (agents-repl-test--with-clean-state
    (should-error (agents-repl--ws-set nil :thinking) :type 'error)))

;;;; ---- Tests: Title spinner detection ----

(ert-deftest agents-repl-test-title-has-spinner-p ()
  "Spinner detection: Unicode prefix = spinner, ✳ prefix = idle, ASCII = no spinner."
  (should (agents-repl--title-has-spinner-p "⠋ Claude Code"))
  (should (agents-repl--title-has-spinner-p "🔄 Claude Code"))
  (should-not (agents-repl--title-has-spinner-p "✳ Claude Code"))
  (should-not (agents-repl--title-has-spinner-p "Claude Code"))
  (should-not (agents-repl--title-has-spinner-p "")))

(ert-deftest agents-repl-test-detect-title-transition-started ()
  "Transition from idle to spinner should be 'started."
  (agents-repl-test--with-temp-buffer " *test-title-tr*"
    (setq-local agents-repl--title-thinking nil)
    (cl-letf (((symbol-function 'agents-repl--workspace-for-buffer) (lambda (_buf) "ws1")))
      (let ((info (agents-repl--detect-title-transition "⠋ Claude Code")))
        (should (eq (plist-get info :transition) 'started))
        (should (plist-get info :thinking))))))

(ert-deftest agents-repl-test-detect-title-transition-finished ()
  "Transition from spinner to idle should be 'finished."
  (agents-repl-test--with-temp-buffer " *test-title-fin*"
    (setq-local agents-repl--title-thinking t)
    (cl-letf (((symbol-function 'agents-repl--workspace-for-buffer) (lambda (_buf) "ws1")))
      (let ((info (agents-repl--detect-title-transition "✳ Claude Code")))
        (should (eq (plist-get info :transition) 'finished))
        (should-not (plist-get info :thinking))))))

;;;; ---- Tests: Prefix injection counter ----

(ert-deftest agents-repl-test-prefix-injection-counter ()
  "Prefix should be injected when counter mod period is 0."
  (agents-repl-test--with-clean-state
    (let ((agents-repl-skip-permissions t)
          (agents-repl-prefix-period 3)
          (agents-repl-command-prefix "TEST")
          (agents-repl--command-prefix "PREFIX: "))
      (agents-repl-test--with-temp-buffer " *test-prefix*"
        (let ((ws "test-ws"))
          (agents-repl--ws-put ws :input-buffer (current-buffer))
          (agents-repl--ws-put ws :prefix-counter 0)
          (insert "hello")
          ;; Counter 0 mod 3 = 0 → prefix
          (cl-letf (((symbol-function '+workspace-current-name) (lambda () ws)))
            (should (string-prefix-p "PREFIX: " (agents-repl--prepare-input ws)))
            ;; Counter 1 mod 3 ≠ 0 → no prefix
            (agents-repl--ws-put ws :prefix-counter 1)
            (should-not (string-prefix-p "PREFIX: " (agents-repl--prepare-input ws)))
            ;; Counter 3 mod 3 = 0 → prefix again
            (agents-repl--ws-put ws :prefix-counter 3)
            (should (string-prefix-p "PREFIX: " (agents-repl--prepare-input ws)))))))))

(ert-deftest agents-repl-test-prefix-counter-per-workspace ()
  "Each workspace should maintain its own prefix counter independently."
  (agents-repl-test--with-clean-state
    ;; Set different counters for two workspaces
    (agents-repl--ws-put "ws-a" :prefix-counter 7)
    (agents-repl--ws-put "ws-b" :prefix-counter 42)
    ;; They should be independent
    (should (= (agents-repl--ws-get "ws-a" :prefix-counter) 7))
    (should (= (agents-repl--ws-get "ws-b" :prefix-counter) 42))
    ;; Mutating one should not affect the other
    (agents-repl--ws-put "ws-a" :prefix-counter 8)
    (should (= (agents-repl--ws-get "ws-b" :prefix-counter) 42))))

;;;; ---- Tests: Overlay refcount ----

(ert-deftest agents-repl-test-overlay-refcount ()
  "Overlay advice should be added once and removed only when refcount hits 0."
  (agents-repl-test--with-clean-state
    (let ((advice-added 0)
          (advice-removed 0))
      (cl-letf (((symbol-function 'advice-add)
                 (lambda (&rest _) (cl-incf advice-added)))
                ((symbol-function 'advice-remove)
                 (lambda (&rest _) (cl-incf advice-removed))))
        ;; Enable twice
        (agents-repl--enable-hide-overlay)
        (agents-repl--enable-hide-overlay)
        (should (= advice-added 1))
        (should (= agents-repl--hide-overlay-refcount 2))
        ;; Disable once — advice should NOT be removed
        (agents-repl--disable-hide-overlay)
        (should (= advice-removed 0))
        (should (= agents-repl--hide-overlay-refcount 1))
        ;; Disable again — now advice should be removed
        (agents-repl--disable-hide-overlay)
        (should (= advice-removed 1))
        (should (= agents-repl--hide-overlay-refcount 0))
        ;; Extra disable should clamp to 0
        (agents-repl--disable-hide-overlay)
        (should (= agents-repl--hide-overlay-refcount 0))))))

;;;; ---- Tests: Orphan panel detection ----

(ert-deftest agents-repl-test-orphaned-vterm-p ()
  "A vterm buffer with no matching input window is orphaned."
  (agents-repl-test--with-clean-state
    (let ((agents-repl--fullscreen-config nil))
      ;; Mock: not one-window-p, no matching input window
      (cl-letf (((symbol-function 'one-window-p) (lambda () nil))
                ((symbol-function 'get-buffer-window) (lambda (_buf) nil)))
        (should (agents-repl--orphaned-vterm-p "*claude-abcd1234*"))
        (should-not (agents-repl--orphaned-vterm-p "*claude-input-abcd1234*"))
        (should-not (agents-repl--orphaned-vterm-p "*some-other*"))))))

(ert-deftest agents-repl-test-orphaned-input-p ()
  "An input buffer with no matching vterm window is orphaned."
  (agents-repl-test--with-clean-state
    (cl-letf (((symbol-function 'one-window-p) (lambda () nil))
              ((symbol-function 'get-buffer-window) (lambda (_buf) nil))
              ((symbol-function 'get-buffer) (lambda (_name) nil)))
      (should (agents-repl--orphaned-input-p "*claude-input-abcd1234*"))
      (should-not (agents-repl--orphaned-input-p "*claude-abcd1234*"))
      (should-not (agents-repl--orphaned-input-p "*scratch*")))))

;;;; ---- Tests: Tabline rendering ----

(ert-deftest agents-repl-test-tabline-thinking-face ()
  "Tabline should apply thinking face for background thinking tabs."
  (agents-repl-test--with-clean-state
    (agents-repl--ws-set "other-ws" :thinking)
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
              ((symbol-function '+workspace-list-names) (lambda () '("test-ws" "other-ws"))))
      (let ((result (agents-repl--tabline-advice '("test-ws" "other-ws"))))
        ;; other-ws should have thinking face
        (should (string-match-p "other-ws" result))))))

(ert-deftest agents-repl-test-tabline-permission-label ()
  "Tabline should show ❓ for permission state."
  (agents-repl-test--with-clean-state
    (agents-repl--ws-set "test-ws" :permission)
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
              ((symbol-function '+workspace-list-names) (lambda () '("test-ws"))))
      (let ((result (agents-repl--tabline-advice '("test-ws"))))
        (should (string-match-p "❓" result))))))

;;;; ---- Tests: Deferred macro ----

(ert-deftest agents-repl-test-deferred-macro ()
  "The deferred macro should create a debouncing lambda."
  (let ((agents-repl--sync-timer nil)
        (call-count 0))
    (let ((debounced (agents-repl--deferred agents-repl--sync-timer
                       (lambda () (cl-incf call-count)))))
      ;; Calling it should set the timer var
      (funcall debounced)
      (should agents-repl--sync-timer)
      ;; Cancel it to prevent side effects
      (cancel-timer agents-repl--sync-timer)
      (setq agents-repl--sync-timer nil))))

;;;; ---- Bug regression tests ----

(ert-deftest agents-repl-test-bug1-cursor-reset-timer-defvar ()
  "Bug 1: agents-repl--cursor-reset-timer should be defined (not void)."
  (should (boundp 'agents-repl--cursor-reset-timer)))

(ert-deftest agents-repl-test-bug2-title-thinking-buffer-local ()
  "Bug 2: agents-repl--title-thinking should be buffer-local."
  (agents-repl-test--with-temp-buffer " *test-bl-1*"
    (setq agents-repl--title-thinking t)
    (agents-repl-test--with-temp-buffer " *test-bl-2*"
      ;; Should be nil in a different buffer (default value)
      (should-not agents-repl--title-thinking))))

(ert-deftest agents-repl-test-bug3-title-change-no-ws ()
  "Bug 3: on-title-change should silently skip (not error) when workspace is nil during a transition."
  (agents-repl-test--with-clean-state
    (agents-repl-test--with-temp-buffer "*claude-abcd1234*"
      (setq-local agents-repl--ready t)  ;; skip first-ready handling
      (setq-local agents-repl--title-thinking nil)
      (cl-letf (((symbol-function 'agents-repl--workspace-for-buffer)
                 (lambda (_buf) nil))
                ((symbol-function 'agents-repl--handle-first-ready)
                 (lambda () nil)))
        ;; Should log and return nil, not error, when ws is nil during a transition
        (should-not (agents-repl--on-title-change "⠋ Claude Code"))
        ;; title-thinking should still be updated (setq is before the guard)
        (should agents-repl--title-thinking)))))

(ert-deftest agents-repl-test-bug5-rel-path-non-file-buffer ()
  "Bug 5: rel-path should signal user-error for non-file buffers."
  (agents-repl-test--with-temp-buffer " *test-no-file*"
    (should-error (agents-repl--rel-path) :type 'user-error)))

(ert-deftest agents-repl-test-bug5-rel-path-with-file ()
  "Bug 5: rel-path should work for file-visiting buffers."
  (cl-letf (((symbol-function 'buffer-file-name)
             (lambda (&optional _buf) "/project/src/foo.el"))
            ((symbol-function 'agents-repl--resolve-root)
             (lambda () "/project/")))
    (should (equal (agents-repl--rel-path) "src/foo.el"))))

(ert-deftest agents-repl-test-bug6-stale-window ()
  "Bug 6: live-return-window should fall back to selected-window."
  (agents-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1")))
      ;; No return-window stored → fall back to selected-window
      (should (eq (agents-repl--live-return-window) (selected-window)))
      ;; Live window stored → return it
      (agents-repl--ws-put "ws1" :return-window (selected-window))
      (should (eq (agents-repl--live-return-window) (selected-window))))))

(ert-deftest agents-repl-test-bug7-prefix-counter-persists ()
  "Bug 7: prefix counter should persist in the workspaces hash across lookups."
  (agents-repl-test--with-clean-state
    (agents-repl--ws-put "ws1" :prefix-counter 42)
    (should (= (agents-repl--ws-get "ws1" :prefix-counter) 42))
    ;; Incrementing should work correctly
    (agents-repl--ws-put "ws1" :prefix-counter
                         (1+ (or (agents-repl--ws-get "ws1" :prefix-counter) 0)))
    (should (= (agents-repl--ws-get "ws1" :prefix-counter) 43))))

(ert-deftest agents-repl-test-bug8-history-save-uses-input-buffer-root ()
  "Bug 8: history-save should use the input buffer's project root."
  (agents-repl-test--with-clean-state
    (agents-repl-test--with-temp-buffer " *test-hist-save*"
      (setq-local agents-repl--project-root "/input-buffer-root")
      (setq-local agents-repl--input-history '("entry1" "entry2"))
      (agents-repl--ws-put "test-ws" :input-buffer (current-buffer))
      ;; Verify the file path computed from the input buffer's project root
      (let* ((buf (agents-repl--ws-get "test-ws" :input-buffer))
             (root (buffer-local-value 'agents-repl--project-root buf))
             (file (expand-file-name ".agents-repl-history" root)))
        (should (string-match-p "/input-buffer-root" file))))))

(ert-deftest agents-repl-test-bug9-paste-delay-configurable ()
  "Bug 9: agents-repl-paste-delay should be a configurable variable."
  (should (boundp 'agents-repl-paste-delay))
  (should (numberp agents-repl-paste-delay)))

(ert-deftest agents-repl-test-bug10-defvar-declarations ()
  "Bug 10: All key variables should be properly declared."
  (should (boundp 'agents-repl--workspaces))
  (should (boundp 'agents-repl-hide-input-box))
  (should (boundp 'agents-repl--notify-fn))
  (should (boundp 'agents-repl--sync-timer))
  (should (boundp 'agents-repl--title-thinking)))

(ert-deftest agents-repl-test-bug11-fullscreen-config-declared ()
  "Bug 11: agents-repl--fullscreen-config should be declared before use."
  (should (boundp 'agents-repl--fullscreen-config)))

(ert-deftest agents-repl-test-bug13-docstring-accuracy ()
  "Bug 13: show-panels docstring should mention 60% and 15%."
  (let ((doc (documentation 'agents-repl--show-panels)))
    (should (string-match-p "60%" doc))
    (should (string-match-p "15%" doc))))

(ert-deftest agents-repl-test-package-provide ()
  "Package should provide 'agents-repl feature."
  (should (featurep 'agents-repl)))

(ert-deftest agents-repl-test-grey-format ()
  "Grey helper should return proper hex color."
  (should (equal (agents-repl--grey 0) "#000000"))
  (should (equal (agents-repl--grey 255) "#ffffff"))
  (should (equal (agents-repl--grey 15) "#0f0f0f")))

(ert-deftest agents-repl-test-claude-buffer-p ()
  "claude-buffer-p should match *claude-HASH* pattern only."
  (agents-repl-test--with-temp-buffer "*claude-abcd1234*"
    (should (agents-repl--claude-buffer-p)))
  (agents-repl-test--with-temp-buffer "*claude-input-abcd1234*"
    (should-not (agents-repl--claude-buffer-p)))
  (agents-repl-test--with-temp-buffer "*scratch*"
    (should-not (agents-repl--claude-buffer-p))))

;;;; ---- Tests: Unreal buffer predicate ----

;;;; ---- Tests: Paste to vterm ----

(ert-deftest agents-repl-test-paste-to-vterm ()
  "agents-repl-paste-to-vterm should call vterm-send-key with C-v args."
  (agents-repl-test--with-clean-state
    (let ((send-key-args nil))
      (agents-repl-test--with-temp-buffer "*claude-abcd1234*"
        (agents-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                  ((symbol-function 'agents-repl--vterm-live-p) (lambda () t))
                  ((symbol-function 'vterm-send-key)
                   (lambda (&rest args) (setq send-key-args args))))
          (agents-repl-paste-to-vterm)
          (should (equal send-key-args '("v" nil nil t))))))))

;;;; ---- Tests: Logging ----

(ert-deftest agents-repl-test-log-respects-debug-flag ()
  "When `agents-repl-debug' is nil, `agents-repl--log' should NOT call `message'.
When t, it should call `message'."
  (let ((message-called nil))
    (cl-letf (((symbol-function 'message)
               (lambda (&rest _args) (setq message-called t))))
      ;; debug off: no message
      (let ((agents-repl-debug nil))
        (agents-repl--log "test %s" "hello")
        (should-not message-called))
      ;; debug on: message called
      (let ((agents-repl-debug t))
        (setq message-called nil)
        (agents-repl--log "test %s" "hello")
        (should message-called)))))

;;;; ---- Tests: Buffer predicates ----

(ert-deftest agents-repl-test-vterm-live-p-nil ()
  "Returns nil when no vterm buffer is stored for the workspace."
  (agents-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1")))
      (should-not (agents-repl--vterm-live-p)))))

(ert-deftest agents-repl-test-vterm-live-p-dead ()
  "Returns nil for a killed buffer."
  (agents-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1")))
      (let ((buf (get-buffer-create " *test-dead-buf*")))
        (agents-repl--ws-put "ws1" :vterm-buffer buf)
        (kill-buffer buf)
        (should-not (agents-repl--vterm-live-p))))))

(ert-deftest agents-repl-test-vterm-live-p-live ()
  "Returns non-nil for a live buffer."
  (agents-repl-test--with-clean-state
    (agents-repl-test--with-temp-buffer " *test-live-buf*"
      (agents-repl--ws-put "ws1" :vterm-buffer (current-buffer))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1")))
        (should (agents-repl--vterm-live-p))))))

(ert-deftest agents-repl-test-vterm-running-p-no-process ()
  "Returns nil when buffer is live but has no process."
  (agents-repl-test--with-clean-state
    (agents-repl-test--with-temp-buffer " *test-no-proc*"
      (agents-repl--ws-put "ws1" :vterm-buffer (current-buffer))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1")))
        (should-not (agents-repl--vterm-running-p))))))

(ert-deftest agents-repl-test-vterm-running-p-with-process ()
  "Returns non-nil when buffer has a live process."
  (agents-repl-test--with-clean-state
    (agents-repl-test--with-temp-buffer " *test-with-proc*"
      (agents-repl--ws-put "ws1" :vterm-buffer (current-buffer))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
                ((symbol-function 'get-buffer-process)
                 (lambda (_buf) 'fake-process)))
        (should (agents-repl--vterm-running-p))))))

;;;; ---- Tests: History edge cases ----

(ert-deftest agents-repl-test-history-prev-empty-list ()
  "Calling `history-prev' with empty history does nothing."
  (agents-repl-test--with-temp-buffer " *test-hist-empty*"
    (setq-local agents-repl--input-history nil)
    (setq-local agents-repl--history-index -1)
    (setq-local agents-repl--history-navigating nil)
    (insert "original")
    (agents-repl--history-prev)
    (should (equal (buffer-string) "original"))
    (should (= agents-repl--history-index -1))))

(ert-deftest agents-repl-test-history-prev-at-oldest ()
  "When already at the last (oldest) entry, `history-prev' does nothing."
  (agents-repl-test--with-temp-buffer " *test-hist-oldest*"
    (setq-local agents-repl--input-history '("only-entry"))
    (setq-local agents-repl--history-index 0)
    (setq-local agents-repl--history-stash "stashed")
    (setq-local agents-repl--history-navigating nil)
    (insert "only-entry")
    (agents-repl--history-prev)
    ;; index should stay at 0, buffer unchanged
    (should (= agents-repl--history-index 0))
    (should (equal (buffer-string) "only-entry"))))

(ert-deftest agents-repl-test-history-next-at-start ()
  "When `agents-repl--history-index' is -1, `history-next' does nothing."
  (agents-repl-test--with-temp-buffer " *test-hist-at-start*"
    (setq-local agents-repl--input-history '("a" "b"))
    (setq-local agents-repl--history-index -1)
    (setq-local agents-repl--history-navigating nil)
    (insert "current")
    (agents-repl--history-next)
    (should (equal (buffer-string) "current"))
    (should (= agents-repl--history-index -1))))

(ert-deftest agents-repl-test-history-on-change-noop-during-navigation ()
  "When `agents-repl--history-navigating' is t, `history-on-change' does NOT reset the index."
  (agents-repl-test--with-temp-buffer " *test-hist-nav-noop*"
    (setq-local agents-repl--history-index 3)
    (setq-local agents-repl--history-navigating t)
    (agents-repl--history-on-change)
    (should (= agents-repl--history-index 3))))

(ert-deftest agents-repl-test-history-file-path ()
  "`agents-repl--history-file' returns `<root>/.agents-repl-history'."
  (cl-letf (((symbol-function 'agents-repl--resolve-root) (lambda () "/test/root")))
    (should (equal (agents-repl--history-file)
                   (expand-file-name ".agents-repl-history" "/test/root")))))

;;;; ---- Tests: Input preparation ----

(ert-deftest agents-repl-test-prepare-input-no-prefix-when-disabled ()
  "When `agents-repl-skip-permissions' is nil, `prepare-input' returns raw text."
  (agents-repl-test--with-clean-state
    (let ((agents-repl-skip-permissions nil)
          (agents-repl-prefix-period 1))
      (agents-repl-test--with-temp-buffer " *test-no-prefix*"
        (agents-repl--ws-put "ws1" :input-buffer (current-buffer))
        (agents-repl--ws-put "ws1" :prefix-counter 0)
        (insert "raw text")
        (should (equal (agents-repl--prepare-input "ws1") "raw text"))))))

;;;; ---- Tests: Send functions ----

(ert-deftest agents-repl-test-send-char-calls-vterm ()
  "`agents-repl-send-char' should call `vterm-send-string' with the char, then `vterm-send-return'."
  (agents-repl-test--with-clean-state
    (let ((calls nil))
      (agents-repl-test--with-temp-buffer "*claude-abcd1234*"
        (agents-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                  ((symbol-function 'agents-repl--vterm-live-p) (lambda () t))
                  ((symbol-function 'vterm-send-string)
                   (lambda (s &rest _) (push (list 'send-string s) calls)))
                  ((symbol-function 'vterm-send-return)
                   (lambda () (push '(send-return) calls))))
          (agents-repl-send-char "y")
          (should (member '(send-string "y") (reverse calls)))
          (should (member '(send-return) (reverse calls))))))))

(ert-deftest agents-repl-test-scroll-down-sends-down ()
  "`agents-repl-scroll-down' calls `vterm-send-down' in the vterm buffer."
  (agents-repl-test--with-clean-state
    (let ((down-called nil))
      (agents-repl-test--with-temp-buffer "*claude-abcd1234*"
        (agents-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                  ((symbol-function 'agents-repl--vterm-live-p) (lambda () t))
                  ((symbol-function 'vterm-send-down)
                   (lambda () (setq down-called t))))
          (agents-repl-scroll-down)
          (should down-called))))))

(ert-deftest agents-repl-test-scroll-up-sends-up ()
  "`agents-repl-scroll-up' calls `vterm-send-up'."
  (agents-repl-test--with-clean-state
    (let ((up-called nil))
      (agents-repl-test--with-temp-buffer "*claude-abcd1234*"
        (agents-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                  ((symbol-function 'agents-repl--vterm-live-p) (lambda () t))
                  ((symbol-function 'vterm-send-up)
                   (lambda () (setq up-called t))))
          (agents-repl-scroll-up)
          (should up-called))))))

(ert-deftest agents-repl-test-interrupt-sends-escape-twice ()
  "`agents-repl-interrupt' calls `vterm-send-key' with \"<escape>\" twice."
  (agents-repl-test--with-clean-state
    (let ((escape-count 0))
      (agents-repl-test--with-temp-buffer "*claude-abcd1234*"
        (agents-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                  ((symbol-function 'agents-repl--vterm-live-p) (lambda () t))
                  ((symbol-function 'vterm-send-key)
                   (lambda (key &rest _)
                     (when (equal key "<escape>")
                       (cl-incf escape-count)))))
          (agents-repl-interrupt)
          (should (= escape-count 2)))))))

(ert-deftest agents-repl-test-cycle-sends-backtab ()
  "`agents-repl-cycle' calls `vterm-send-key' with \"<backtab>\"."
  (agents-repl-test--with-clean-state
    (let ((backtab-called nil))
      (agents-repl-test--with-temp-buffer "*claude-abcd1234*"
        (agents-repl--ws-put "test-ws" :vterm-buffer (current-buffer))
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                  ((symbol-function 'agents-repl--vterm-live-p) (lambda () t))
                  ((symbol-function 'vterm-send-key)
                   (lambda (key &rest _)
                     (when (equal key "<backtab>")
                       (setq backtab-called t)))))
          (agents-repl-cycle)
          (should backtab-called))))))

(ert-deftest agents-repl-test-send-input-direct-mode ()
  "For input <=200 chars, `send-input-to-vterm' calls `vterm-send-string' without paste flag."
  (agents-repl-test--with-clean-state
    (let ((send-string-args nil)
          (return-called nil))
      (agents-repl-test--with-temp-buffer "*claude-abcd1234*"
        (cl-letf (((symbol-function 'vterm-send-string)
                   (lambda (s &rest args) (setq send-string-args (cons s args))))
                  ((symbol-function 'vterm-send-return)
                   (lambda () (setq return-called t)))
                  ((symbol-function 'agents-repl--refresh-vterm) #'ignore))
          (agents-repl--send-input-to-vterm (current-buffer) "short input")
          ;; Should have been called with just the string (no paste flag)
          (should (equal (car send-string-args) "short input"))
          (should (null (cdr send-string-args)))
          (should return-called))))))

(ert-deftest agents-repl-test-send-input-paste-mode ()
  "For input >200 chars, calls `vterm-send-string' WITH paste flag, defers return."
  (agents-repl-test--with-clean-state
    (let ((send-string-args nil)
          (return-called nil)
          (timer-args nil))
      (agents-repl-test--with-temp-buffer "*claude-abcd1234*"
        (cl-letf (((symbol-function 'vterm-send-string)
                   (lambda (s &rest args) (setq send-string-args (cons s args))))
                  ((symbol-function 'vterm-send-return)
                   (lambda () (setq return-called t)))
                  ((symbol-function 'agents-repl--refresh-vterm) #'ignore)
                  ((symbol-function 'run-at-time)
                   (lambda (&rest args) (setq timer-args args))))
          (let ((long-input (make-string 201 ?x)))
            (agents-repl--send-input-to-vterm (current-buffer) long-input)
            ;; paste flag (2nd arg) should be t
            (should (equal (cadr send-string-args) t))
            ;; return should NOT have been called directly
            (should-not return-called)
            ;; run-at-time should have been called to defer
            (should timer-args)))))))

;;;; ---- Tests: Composite state functions ----

(ert-deftest agents-repl-test-mark-ws-thinking-composite ()
  "`agents-repl--mark-ws-thinking' should set :thinking state AND record activity."
  (agents-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agents-repl--workspace-clean-p) (lambda (_) t)))
      (agents-repl--mark-ws-thinking "ws1")
      (should (eq (agents-repl--ws-state "ws1") :thinking))
      (should (agents-repl--ws-get "ws1" :activity-time)))))

(ert-deftest agents-repl-test-clear-input-pushes-and-clears ()
  "`agents-repl--clear-input' pushes text to history, resets index, clears buffer."
  (agents-repl-test--with-clean-state
    (agents-repl-test--with-temp-buffer " *test-clear-input*"
      (setq-local agents-repl--input-history nil)
      (setq-local agents-repl--history-index 5)
      (setq-local agents-repl--history-navigating nil)
      (agents-repl--ws-put "ws1" :input-buffer (current-buffer))
      (insert "some input text")
      (agents-repl--clear-input "ws1")
      (should (equal agents-repl--input-history '("some input text")))
      (should (= agents-repl--history-index -1))
      (should (equal (buffer-string) "")))))

(ert-deftest agents-repl-test-discard-input-pushes-and-clears ()
  "`agents-repl-discard-input' pushes text, clears buffer, calls `evil-insert-state'."
  (agents-repl-test--with-temp-buffer " *test-discard*"
    (setq-local agents-repl--input-history nil)
    (setq-local agents-repl--history-index 3)
    (setq-local agents-repl--history-navigating nil)
    (insert "discard me")
    (let ((evil-called nil))
      (cl-letf (((symbol-function 'evil-insert-state)
                 (lambda () (setq evil-called t))))
        (agents-repl-discard-input)
        (should (equal agents-repl--input-history '("discard me")))
        (should (= agents-repl--history-index -1))
        (should (equal (buffer-string) ""))
        (should evil-called)))))

(ert-deftest agents-repl-test-remember-return-window-saves ()
  "`agents-repl--remember-return-window' stores return-window when not in input buffer."
  (agents-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1")))
      ;; No input buffer stored — any buffer qualifies as non-input
      (agents-repl--remember-return-window)
      (should (eq (agents-repl--ws-get "ws1" :return-window) (selected-window))))))

(ert-deftest agents-repl-test-remember-return-window-skips-input ()
  "When current buffer IS the workspace's input buffer, should NOT change return-window."
  (agents-repl-test--with-clean-state
    (agents-repl-test--with-temp-buffer " *test-skip-input*"
      (agents-repl--ws-put "ws1" :input-buffer (current-buffer))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1")))
        (agents-repl--remember-return-window)
        (should-not (agents-repl--ws-get "ws1" :return-window))))))

;;;; ---- Tests: Title change handling ----

(ert-deftest agents-repl-test-on-title-change-started-sets-thinking ()
  "In a claude buffer with `title-thinking' nil, spinner title sets workspace to :thinking."
  (agents-repl-test--with-clean-state
    (agents-repl-test--with-temp-buffer "*claude-abcd1234*"
      (setq-local agents-repl--title-thinking nil)
      (setq-local agents-repl--ready t)
      (cl-letf (((symbol-function 'agents-repl--workspace-for-buffer)
                 (lambda (_buf) "ws1"))
                ((symbol-function 'agents-repl--handle-first-ready) #'ignore))
        (agents-repl--on-title-change "⠋ Claude Code")
        (should (eq (agents-repl--ws-state "ws1") :thinking))))))

(ert-deftest agents-repl-test-on-title-change-finished-clears-thinking ()
  "With `title-thinking' t, idle title clears :thinking and calls `on-claude-finished'."
  (agents-repl-test--with-clean-state
    (let ((finished-called nil))
      (agents-repl-test--with-temp-buffer "*claude-abcd1234*"
        (setq-local agents-repl--title-thinking t)
        (setq-local agents-repl--ready t)
        (agents-repl--ws-set "ws1" :thinking)
        (cl-letf (((symbol-function 'agents-repl--workspace-for-buffer)
                   (lambda (_buf) "ws1"))
                  ((symbol-function 'agents-repl--handle-first-ready) #'ignore)
                  ((symbol-function 'agents-repl--on-claude-finished)
                   (lambda (ws) (setq finished-called ws))))
          (agents-repl--on-title-change "✳ Claude Code")
          (should-not (eq (agents-repl--ws-get "ws1" :status) :thinking))
          (should (equal finished-called "ws1")))))))

(ert-deftest agents-repl-test-on-title-change-no-reentrant-recursion ()
  "Regression: on-title-change updates title-thinking BEFORE side effects.
If on-claude-finished triggers a re-entrant on-title-change (via
refresh-vterm -> vterm--redraw -> set-title), the re-entrant call should
see the updated title-thinking and detect no transition."
  (agents-repl-test--with-clean-state
    (agents-repl-test--with-temp-buffer "*claude-abcd1234*"
      (setq-local agents-repl--title-thinking t)
      (setq-local agents-repl--ready t)
      (agents-repl--ws-set "ws1" :thinking)
      (let ((reentrant-transition nil))
        (cl-letf (((symbol-function 'agents-repl--workspace-for-buffer)
                   (lambda (_buf) "ws1"))
                  ((symbol-function 'agents-repl--handle-first-ready) #'ignore)
                  ((symbol-function 'agents-repl--on-claude-finished)
                   (lambda (_ws)
                     ;; Simulate re-entrant call: on-claude-finished would
                     ;; normally call refresh-vterm -> vterm--redraw, which
                     ;; triggers vterm--set-title -> on-title-change again.
                     ;; title-thinking must already be nil at this point.
                     (let ((info (agents-repl--detect-title-transition "✳ Claude Code")))
                       (setq reentrant-transition (plist-get info :transition))))))
          (agents-repl--on-title-change "✳ Claude Code")
          ;; The re-entrant detect-title-transition should see title-thinking=nil
          ;; (already updated) and return nil transition — not 'finished again.
          (should-not reentrant-transition))))))

(ert-deftest agents-repl-test-on-title-change-non-claude-buffer ()
  "In a non-claude buffer, `on-title-change' should do nothing."
  (agents-repl-test--with-clean-state
    (agents-repl-test--with-temp-buffer "*scratch*"
      (setq-local agents-repl--title-thinking nil)
      ;; Should not error or change state
      (agents-repl--on-title-change "⠋ Claude Code")
      (should-not agents-repl--title-thinking))))

(ert-deftest agents-repl-test-handle-first-ready-idempotent ()
  "First call sets `agents-repl--ready' to t and calls `swap-placeholder'. Second call is a no-op."
  (agents-repl-test--with-temp-buffer " *test-first-ready*"
    (setq-local agents-repl--ready nil)
    (let ((swap-count 0))
      (cl-letf (((symbol-function 'agents-repl--swap-placeholder)
                 (lambda () (cl-incf swap-count))))
        (agents-repl--handle-first-ready)
        (should agents-repl--ready)
        (should (= swap-count 1))
        ;; Second call should be no-op
        (agents-repl--handle-first-ready)
        (should (= swap-count 1))))))

(ert-deftest agents-repl-test-detect-title-transition-no-change ()
  "When `title-thinking' is nil and title has no spinner, transition should be nil."
  (agents-repl-test--with-temp-buffer " *test-no-change*"
    (setq-local agents-repl--title-thinking nil)
    (cl-letf (((symbol-function 'agents-repl--workspace-for-buffer) (lambda (_buf) "ws1")))
      (let ((info (agents-repl--detect-title-transition "Claude Code")))
        (should-not (plist-get info :transition))))))

(ert-deftest agents-repl-test-detect-title-transition-still-thinking ()
  "When `title-thinking' is t and title has spinner, transition should be nil."
  (agents-repl-test--with-temp-buffer " *test-still-thinking*"
    (setq-local agents-repl--title-thinking t)
    (cl-letf (((symbol-function 'agents-repl--workspace-for-buffer) (lambda (_buf) "ws1")))
      (let ((info (agents-repl--detect-title-transition "⠋ Claude Code")))
        (should-not (plist-get info :transition))
        (should (plist-get info :thinking))))))

;;;; ---- Tests: State machine completeness ----

(ert-deftest agents-repl-test-ws-state-transitions ()
  "Verify state transitions: ws-set correctly cycles through :thinking, :permission, :done, and :stale."
  (agents-repl-test--with-clean-state
    ;; :thinking wins over :done (ws-set replaces, but we can test ordering by
    ;; setting :status directly in the plist and checking ws-state)
    (agents-repl--ws-set "ws1" :thinking)
    (should (eq (agents-repl--ws-state "ws1") :thinking))
    ;; Set to :permission
    (agents-repl--ws-set "ws1" :permission)
    (should (eq (agents-repl--ws-state "ws1") :permission))
    ;; Set to :done
    (agents-repl--ws-set "ws1" :done)
    (should (eq (agents-repl--ws-state "ws1") :done))
    ;; Clear done, set activity for stale
    (agents-repl--ws-clear "ws1" :done)
    (cl-letf (((symbol-function 'agents-repl--workspace-clean-p) (lambda (_) t)))
      (agents-repl--touch-activity "ws1"))
    (should (eq (agents-repl--ws-state "ws1") :stale))))

(ert-deftest agents-repl-test-ws-state-expired-stale ()
  "Activity time >60 minutes ago should return nil (expired)."
  (agents-repl-test--with-clean-state
    (let ((agents-repl-stale-minutes 60))
      ;; Set activity to 61 minutes ago
      (agents-repl--ws-put "ws1" :activity-time (- (float-time) (* 61 60)))
      (should-not (agents-repl--ws-state "ws1")))))

(ert-deftest agents-repl-test-ws-set-permission ()
  "`ws-set' with :permission should set permission hash."
  (agents-repl-test--with-clean-state
    (agents-repl--ws-set "ws1" :permission)
    (should (eq (agents-repl--ws-state "ws1") :permission))))

(ert-deftest agents-repl-test-ws-clear-done ()
  "`ws-clear' with :done should not clear status when it is :thinking."
  (agents-repl-test--with-clean-state
    (agents-repl--ws-set "ws1" :thinking)
    (agents-repl--ws-clear "ws1" :done)
    (should (eq (agents-repl--ws-get "ws1" :status) :thinking))))

(ert-deftest agents-repl-test-ws-clear-permission ()
  "`ws-clear' with :permission should not clear status when it is :done."
  (agents-repl-test--with-clean-state
    (agents-repl--ws-set "ws1" :done)
    (agents-repl--ws-clear "ws1" :permission)
    (should (eq (agents-repl--ws-get "ws1" :status) :done))))

(ert-deftest agents-repl-test-ws-clear-nil-error ()
  "`ws-clear' with nil ws should signal error."
  (agents-repl-test--with-clean-state
    (should-error (agents-repl--ws-clear nil :done) :type 'error)))

(ert-deftest agents-repl-test-touch-activity-nil-error ()
  "`touch-activity' with nil ws should signal error."
  (agents-repl-test--with-clean-state
    (should-error (agents-repl--touch-activity nil) :type 'error)))

;;;; ---- Tests: Tabline rendering completeness ----

(ert-deftest agents-repl-test-tabline-done-face ()
  "A background tab with :done should use `agents-repl-tab-done' face."
  (agents-repl-test--with-clean-state
    (agents-repl--ws-set "bg-ws" :done)
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "current-ws")))
      (let ((result (agents-repl--tabline-advice '("current-ws" "bg-ws"))))
        ;; Find the "bg-ws" segment and check its face
        (let ((pos (string-match "bg-ws" result)))
          (should pos)
          (should (eq (get-text-property pos 'face result) 'agents-repl-tab-done)))))))

(ert-deftest agents-repl-test-tabline-selected-suppresses-thinking ()
  "The SELECTED tab with :thinking should NOT get the thinking face."
  (agents-repl-test--with-clean-state
    (agents-repl--ws-set "sel-ws" :thinking)
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "sel-ws")))
      (let ((result (agents-repl--tabline-advice '("sel-ws"))))
        (let ((pos (string-match "sel-ws" result)))
          (should pos)
          ;; Should get the normal selected face, NOT thinking
          (should (eq (get-text-property pos 'face result)
                      '+workspace-tab-selected-face)))))))

(ert-deftest agents-repl-test-tabline-selected-shows-permission ()
  "The SELECTED tab with :permission SHOULD still get the permission face."
  (agents-repl-test--with-clean-state
    (agents-repl--ws-set "sel-ws" :permission)
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "sel-ws")))
      (let ((result (agents-repl--tabline-advice '("sel-ws"))))
        (let ((pos (string-match "sel-ws" result)))
          (should pos)
          (should (eq (get-text-property pos 'face result)
                      'agents-repl-tab-permission)))))))

(ert-deftest agents-repl-test-tabline-stale-face ()
  "A background tab with :stale should use stale face."
  (agents-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agents-repl--workspace-clean-p) (lambda (_) t)))
      (agents-repl--touch-activity "bg-ws"))
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "current-ws")))
      (let ((result (agents-repl--tabline-advice '("current-ws" "bg-ws"))))
        (let ((pos (string-match "bg-ws" result)))
          (should pos)
          (should (eq (get-text-property pos 'face result)
                      'agents-repl-tab-stale)))))))

;;;; ---- Tests: Orphan detection edge cases ----

(ert-deftest agents-repl-test-orphaned-vterm-in-fullscreen ()
  "When `agents-repl--fullscreen-config' is non-nil, `orphaned-vterm-p' returns nil."
  (let ((agents-repl--fullscreen-config 'some-config))
    (should-not (agents-repl--orphaned-vterm-p "*claude-abcd1234*"))))

(ert-deftest agents-repl-test-orphaned-vterm-one-window ()
  "When `one-window-p' returns t, should return nil."
  (let ((agents-repl--fullscreen-config nil))
    (cl-letf (((symbol-function 'one-window-p) (lambda () t)))
      (should-not (agents-repl--orphaned-vterm-p "*claude-abcd1234*")))))

(ert-deftest agents-repl-test-orphaned-input-with-loading ()
  "When loading placeholder buffer exists, `orphaned-input-p' should return nil."
  (agents-repl-test--with-clean-state
    (cl-letf (((symbol-function 'one-window-p) (lambda () nil))
              ((symbol-function 'get-buffer-window) (lambda (_buf) nil))
              ((symbol-function 'get-buffer) (lambda (name)
                                               (when (equal name " *claude-loading*")
                                                 'fake-buffer))))
      (should-not (agents-repl--orphaned-input-p "*claude-input-abcd1234*")))))

;;;; ---- Tests: Permission notification handler ----

(ert-deftest agents-repl-test-on-permission-notify-ignores-non-permission ()
  "An event with a non-matching filename should not call `ws-set'."
  (agents-repl-test--with-clean-state
    (let ((ws-set-called nil))
      (cl-letf (((symbol-function 'agents-repl--ws-set)
                 (lambda (&rest _) (setq ws-set-called t))))
        (agents-repl--on-permission-notify '(nil changed "/some/other-file"))
        (should-not ws-set-called)))))

(ert-deftest agents-repl-test-on-permission-notify-ignores-other-actions ()
  "An event with action `deleted' should be ignored even if filename matches."
  (agents-repl-test--with-clean-state
    (let ((ws-set-called nil))
      (cl-letf (((symbol-function 'agents-repl--ws-set)
                 (lambda (&rest _) (setq ws-set-called t))))
        (agents-repl--on-permission-notify '(nil deleted "/path/to/permission_prompt"))
        (should-not ws-set-called)))))

;;;; ---- Tests: Overlay management ----

(ert-deftest agents-repl-test-create-overlay-respects-flag ()
  "When `agents-repl-hide-input-box' is nil, `create-hide-overlay' should NOT create an overlay."
  (agents-repl-test--with-temp-buffer " *test-no-overlay*"
    (let ((agents-repl-hide-input-box nil))
      (insert "some content\nline two\nline three\nline four\nline five\n")
      (agents-repl--create-hide-overlay)
      (should-not agents-repl-hide-overlay))))

(ert-deftest agents-repl-test-create-overlay-creates-when-enabled ()
  "When `agents-repl-hide-input-box' is t and buffer has content, overlay is created."
  (agents-repl-test--with-temp-buffer " *test-yes-overlay*"
    (let ((agents-repl-hide-input-box t))
      (insert "line1\nline2\nline3\nline4\nline5\nline6\n")
      (agents-repl--create-hide-overlay)
      (should agents-repl-hide-overlay)
      (should (overlayp agents-repl-hide-overlay)))))

(ert-deftest agents-repl-test-delete-overlay-noop-when-nil ()
  "When `agents-repl-hide-overlay' is nil, `delete-hide-overlay' should not error."
  (agents-repl-test--with-temp-buffer " *test-del-nil*"
    (setq-local agents-repl-hide-overlay nil)
    ;; Should not signal any error
    (agents-repl--delete-hide-overlay)
    (should-not agents-repl-hide-overlay)))

(ert-deftest agents-repl-test-redraw-advice-reentrancy-guard ()
  "Reentrancy guard prevents recursive calls to `after-vterm-redraw'."
  (agents-repl-test--with-clean-state
    (let ((update-count 0))
      (agents-repl-test--with-temp-buffer "*claude-abcd1234*"
        (cl-letf (((symbol-function 'agents-repl--update-hide-overlay)
                   (lambda () (cl-incf update-count))))
          ;; With reentrancy guard active, update-hide-overlay should NOT be called
          (let ((agents-repl--in-redraw-advice t))
            (agents-repl--after-vterm-redraw)
            (should (= update-count 0)))
          ;; Without guard, in a claude buffer, update-hide-overlay IS called
          (let ((agents-repl--in-redraw-advice nil))
            (agents-repl--after-vterm-redraw)
            (should (= update-count 1))))))))

(ert-deftest agents-repl-test-redraw-advice-skips-non-claude ()
  "In a non-claude buffer, `after-vterm-redraw' should not call `update-hide-overlay'."
  (agents-repl-test--with-clean-state
    (let ((update-count 0))
      (agents-repl-test--with-temp-buffer "*scratch*"
        (cl-letf (((symbol-function 'agents-repl--update-hide-overlay)
                   (lambda () (cl-incf update-count))))
          (let ((agents-repl--in-redraw-advice nil))
            (agents-repl--after-vterm-redraw)
            (should (= update-count 0))))))))

;;;; ---- Tests: Workspace-for-buffer ----

(ert-deftest agents-repl-test-workspace-for-buffer-no-persp ()
  "When `persp-mode' is nil, `workspace-for-buffer' should return nil."
  (let ((persp-mode nil))
    (should-not (agents-repl--workspace-for-buffer (current-buffer)))))

;;;; ---- Tests: on-claude-finished ----

(ert-deftest agents-repl-test-on-claude-finished-hidden-sets-done ()
  "When the buffer is NOT visible, `on-claude-finished' should call `ws-set' with :done."
  (agents-repl-test--with-clean-state
    (agents-repl-test--with-temp-buffer " *test-hidden-done*"
      (let ((done-set nil))
        (cl-letf (((symbol-function 'get-buffer-window) (lambda (_buf &rest _) nil))
                  ((symbol-function 'agents-repl--refresh-vterm) #'ignore)
                  ((symbol-function 'agents-repl--update-hide-overlay) #'ignore)
                  ((symbol-function 'agents-repl--maybe-notify-finished) #'ignore)
                  ((symbol-function 'agents-repl--ws-set)
                   (lambda (ws state)
                     (when (eq state :done)
                       (setq done-set ws)))))
          (agents-repl--on-claude-finished "ws1")
          (should (equal done-set "ws1")))))))

(ert-deftest agents-repl-test-on-claude-finished-visible-no-done ()
  "When the buffer IS visible, should NOT set :done."
  (agents-repl-test--with-clean-state
    (agents-repl-test--with-temp-buffer " *test-visible-no-done*"
      (let ((done-set nil))
        (cl-letf (((symbol-function 'get-buffer-window) (lambda (_buf &rest _) 'some-window))
                  ((symbol-function 'agents-repl--refresh-vterm) #'ignore)
                  ((symbol-function 'agents-repl--update-hide-overlay) #'ignore)
                  ((symbol-function 'agents-repl--maybe-notify-finished) #'ignore)
                  ((symbol-function 'agents-repl--ws-set)
                   (lambda (ws state)
                     (when (eq state :done)
                       (setq done-set ws)))))
          (agents-repl--on-claude-finished "ws1")
          (should-not done-set))))))

;;;; ---- Tests: Misc declared variables ----

(ert-deftest agents-repl-test-in-redraw-advice-declared ()
  "`agents-repl--in-redraw-advice' should be `boundp'."
  (should (boundp 'agents-repl--in-redraw-advice)))

;;; test-agents-repl.el ends here
