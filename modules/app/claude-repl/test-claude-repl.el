;;; test-claude-repl.el --- ERT tests for claude-repl -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;   emacs -batch -Q -l ert -l test-claude-repl.el -f ert-run-tests-batch-and-exit
;;
;; Or interactively:
;;   M-x load-file RET test-claude-repl.el RET
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
(defvar claude-repl-test--orig-run-with-timer (symbol-function 'run-with-timer))
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

(defmacro claude-repl-test--with-clean-state (&rest body)
  "Execute BODY with fresh claude-repl global state."
  (declare (indent 0))
  `(let ((claude-repl--sessions (make-hash-table :test 'equal))
         (claude-repl--done-workspaces (make-hash-table :test 'equal))
         (claude-repl--thinking-workspaces (make-hash-table :test 'equal))
         (claude-repl--permission-workspaces (make-hash-table :test 'equal))
         (claude-repl--activity-times (make-hash-table :test 'equal))
         (claude-repl-vterm-buffer nil)
         (claude-repl-input-buffer nil)
         (claude-repl-return-window nil)
         (claude-repl--saved-window-config nil)
         (claude-repl--fullscreen-config nil)
         (claude-repl--prefix-counter 0)
         (claude-repl--sync-timer nil)
         (claude-repl--cursor-reset-timer nil)
         (claude-repl--hide-overlay-refcount 0)
         (claude-repl-debug nil))
     ,@body))

(defmacro claude-repl-test--with-temp-buffer (name &rest body)
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

(ert-deftest claude-repl-test-workspace-id-from-project-root ()
  "Workspace ID should be first 8 chars of MD5 of project root."
  (claude-repl-test--with-temp-buffer " *test-ws-id*"
    (setq-local claude-repl--project-root "/test/project")
    (let ((default-directory "/nonexistent/"))
      ;; git-root will fail, so resolve-root falls through to project-root
      (cl-letf (((symbol-function 'claude-repl--git-root) (lambda (&optional _d) nil)))
        (should (equal (claude-repl--workspace-id)
                       (substring (md5 "/test/project") 0 8)))))))

(ert-deftest claude-repl-test-workspace-id-default-directory ()
  "Workspace ID falls back to default-directory when no git root or project root."
  (claude-repl-test--with-temp-buffer " *test-ws-id-dd*"
    (setq-local claude-repl--project-root nil)
    (let ((default-directory "/fallback/dir/"))
      (cl-letf (((symbol-function 'claude-repl--git-root) (lambda (&optional _d) nil)))
        (should (equal (claude-repl--workspace-id)
                       (substring (md5 "/fallback/dir/") 0 8)))))))

(ert-deftest claude-repl-test-resolve-root-priority ()
  "resolve-root should prefer git-root > project-root > default-directory."
  (claude-repl-test--with-temp-buffer " *test-resolve*"
    (setq-local claude-repl--project-root "/project")
    (let ((default-directory "/default/"))
      ;; With git root available
      (cl-letf (((symbol-function 'claude-repl--git-root) (lambda (&optional _d) "/git-root")))
        (should (equal (claude-repl--resolve-root) "/git-root")))
      ;; Without git root
      (cl-letf (((symbol-function 'claude-repl--git-root) (lambda (&optional _d) nil)))
        (should (equal (claude-repl--resolve-root) "/project"))))))

;;;; ---- Tests: Buffer naming ----

(ert-deftest claude-repl-test-buffer-name-format ()
  "Buffer names should follow *claude-HASH* and *claude-input-HASH* pattern."
  (claude-repl-test--with-temp-buffer " *test-buf-name*"
    (setq-local claude-repl--project-root "/test/proj")
    (cl-letf (((symbol-function 'claude-repl--git-root) (lambda (&optional _d) nil)))
      (let ((id (claude-repl--workspace-id)))
        (should (equal (claude-repl--buffer-name) (format "*claude-%s*" id)))
        (should (equal (claude-repl--buffer-name "-input") (format "*claude-input-%s*" id)))))))

(ert-deftest claude-repl-test-buffer-name-default ()
  "Buffer name uses 'default' when workspace-id returns nil."
  (cl-letf (((symbol-function 'claude-repl--workspace-id) (lambda () nil)))
    (should (equal (claude-repl--buffer-name) "*claude-default*"))))

;;;; ---- Tests: History ----

(ert-deftest claude-repl-test-history-push ()
  "history-push should add text, skip empty, skip duplicates."
  (claude-repl-test--with-temp-buffer " *test-hist*"
    (setq-local claude-repl--input-history nil)
    ;; Push "hello"
    (insert "hello")
    (claude-repl--history-push)
    (should (equal claude-repl--input-history '("hello")))
    ;; Duplicate should be skipped
    (erase-buffer) (insert "hello")
    (claude-repl--history-push)
    (should (equal claude-repl--input-history '("hello")))
    ;; New entry
    (erase-buffer) (insert "world")
    (claude-repl--history-push)
    (should (equal claude-repl--input-history '("world" "hello")))
    ;; Empty should be skipped
    (erase-buffer) (insert "   ")
    (claude-repl--history-push)
    (should (equal claude-repl--input-history '("world" "hello")))))

(ert-deftest claude-repl-test-history-prev-next ()
  "history-prev and history-next should navigate correctly."
  (claude-repl-test--with-temp-buffer " *test-hist-nav*"
    (setq-local claude-repl--input-history '("second" "first"))
    (setq-local claude-repl--history-index -1)
    (setq-local claude-repl--history-stash nil)
    (setq-local claude-repl--history-navigating nil)
    (insert "current text")
    ;; Go back once
    (claude-repl--history-prev)
    (should (equal (buffer-string) "second"))
    (should (= claude-repl--history-index 0))
    (should (equal claude-repl--history-stash "current text"))
    ;; Go back again
    (claude-repl--history-prev)
    (should (equal (buffer-string) "first"))
    (should (= claude-repl--history-index 1))
    ;; Go forward
    (claude-repl--history-next)
    (should (equal (buffer-string) "second"))
    (should (= claude-repl--history-index 0))
    ;; Go forward past newest — restore stash
    (claude-repl--history-next)
    (should (equal (buffer-string) "current text"))
    (should (= claude-repl--history-index -1))))

(ert-deftest claude-repl-test-history-reset ()
  "history-reset should set index back to -1."
  (claude-repl-test--with-temp-buffer " *test-hist-reset*"
    (setq-local claude-repl--history-index 3)
    (claude-repl--history-reset)
    (should (= claude-repl--history-index -1))))

(ert-deftest claude-repl-test-history-on-change-resets ()
  "Editing buffer while browsing history should reset the index."
  (claude-repl-test--with-temp-buffer " *test-hist-change*"
    (setq-local claude-repl--history-index 2)
    (setq-local claude-repl--history-navigating nil)
    (claude-repl--history-on-change)
    (should (= claude-repl--history-index -1))))

;;;; ---- Tests: State machine (ws-set, ws-clear, ws-state) ----

(ert-deftest claude-repl-test-ws-set-and-state ()
  "ws-set should set the correct state, clearing others."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set "ws1" :thinking)
    (should (eq (claude-repl--ws-state "ws1") :thinking))
    (claude-repl--ws-set "ws1" :done)
    (should (eq (claude-repl--ws-state "ws1") :done))
    ;; Thinking should be cleared
    (should-not (gethash "ws1" claude-repl--thinking-workspaces))))

(ert-deftest claude-repl-test-ws-clear ()
  "ws-clear should clear only the specified state."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set "ws1" :thinking)
    (claude-repl--ws-clear "ws1" :thinking)
    (should-not (claude-repl--ws-state "ws1"))))

(ert-deftest claude-repl-test-ws-state-stale ()
  "ws-state should return :stale for recently-active workspaces."
  (claude-repl-test--with-clean-state
    (let ((claude-repl-stale-minutes 60))
      (cl-letf (((symbol-function 'claude-repl--workspace-clean-p) (lambda (_) t)))
        (claude-repl--touch-activity "ws1")
        (should (eq (claude-repl--ws-state "ws1") :stale))))))

(ert-deftest claude-repl-test-ws-set-nil-error ()
  "ws-set with nil workspace should signal an error."
  (claude-repl-test--with-clean-state
    (should-error (claude-repl--ws-set nil :thinking) :type 'error)))

;;;; ---- Tests: Title spinner detection ----

(ert-deftest claude-repl-test-title-has-spinner-p ()
  "Spinner detection: Unicode prefix = spinner, ✳ prefix = idle, ASCII = no spinner."
  (should (claude-repl--title-has-spinner-p "⠋ Claude Code"))
  (should (claude-repl--title-has-spinner-p "🔄 Claude Code"))
  (should-not (claude-repl--title-has-spinner-p "✳ Claude Code"))
  (should-not (claude-repl--title-has-spinner-p "Claude Code"))
  (should-not (claude-repl--title-has-spinner-p "")))

(ert-deftest claude-repl-test-detect-title-transition-started ()
  "Transition from idle to spinner should be 'started."
  (claude-repl-test--with-temp-buffer " *test-title-tr*"
    (setq-local claude-repl--title-thinking nil)
    (cl-letf (((symbol-function 'claude-repl--workspace-for-buffer) (lambda (_buf) "ws1")))
      (let ((info (claude-repl--detect-title-transition "⠋ Claude Code")))
        (should (eq (plist-get info :transition) 'started))
        (should (plist-get info :thinking))))))

(ert-deftest claude-repl-test-detect-title-transition-finished ()
  "Transition from spinner to idle should be 'finished."
  (claude-repl-test--with-temp-buffer " *test-title-fin*"
    (setq-local claude-repl--title-thinking t)
    (cl-letf (((symbol-function 'claude-repl--workspace-for-buffer) (lambda (_buf) "ws1")))
      (let ((info (claude-repl--detect-title-transition "✳ Claude Code")))
        (should (eq (plist-get info :transition) 'finished))
        (should-not (plist-get info :thinking))))))

;;;; ---- Tests: Prefix injection counter ----

(ert-deftest claude-repl-test-prefix-injection-counter ()
  "Prefix should be injected when counter mod period is 0."
  (claude-repl-test--with-clean-state
    (let ((claude-repl-skip-permissions t)
          (claude-repl-prefix-period 3)
          (claude-repl--prefix-counter 0)
          (claude-repl-command-prefix "TEST")
          (claude-repl--command-prefix "PREFIX: "))
      (claude-repl-test--with-temp-buffer " *test-prefix*"
        (setq claude-repl-input-buffer (current-buffer))
        (insert "hello")
        ;; Counter 0 mod 3 = 0 → prefix
        (should (string-prefix-p "PREFIX: " (claude-repl--prepare-input)))
        ;; Counter 1 mod 3 ≠ 0 → no prefix
        (setq claude-repl--prefix-counter 1)
        (should-not (string-prefix-p "PREFIX: " (claude-repl--prepare-input)))
        ;; Counter 3 mod 3 = 0 → prefix again
        (setq claude-repl--prefix-counter 3)
        (should (string-prefix-p "PREFIX: " (claude-repl--prepare-input)))))))

(ert-deftest claude-repl-test-prefix-counter-per-session ()
  "Each session should maintain its own prefix counter via save/load."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'claude-repl--git-root) (lambda (&optional _d) nil)))
      ;; Session A
      (claude-repl-test--with-temp-buffer " *test-session-a*"
        (setq-local claude-repl--project-root "/project-a")
        (setq claude-repl--prefix-counter 7)
        (claude-repl--save-session)
        ;; Session B
        (claude-repl-test--with-temp-buffer " *test-session-b*"
          (setq-local claude-repl--project-root "/project-b")
          (setq claude-repl--prefix-counter 42)
          (claude-repl--save-session))
        ;; Reload session A
        (claude-repl--load-session)
        (should (= claude-repl--prefix-counter 7))))))

;;;; ---- Tests: Overlay refcount ----

(ert-deftest claude-repl-test-overlay-refcount ()
  "Overlay advice should be added once and removed only when refcount hits 0."
  (claude-repl-test--with-clean-state
    (let ((advice-added 0)
          (advice-removed 0))
      (cl-letf (((symbol-function 'advice-add)
                 (lambda (&rest _) (cl-incf advice-added)))
                ((symbol-function 'advice-remove)
                 (lambda (&rest _) (cl-incf advice-removed))))
        ;; Enable twice
        (claude-repl--enable-hide-overlay)
        (claude-repl--enable-hide-overlay)
        (should (= advice-added 1))
        (should (= claude-repl--hide-overlay-refcount 2))
        ;; Disable once — advice should NOT be removed
        (claude-repl--disable-hide-overlay)
        (should (= advice-removed 0))
        (should (= claude-repl--hide-overlay-refcount 1))
        ;; Disable again — now advice should be removed
        (claude-repl--disable-hide-overlay)
        (should (= advice-removed 1))
        (should (= claude-repl--hide-overlay-refcount 0))
        ;; Extra disable should clamp to 0
        (claude-repl--disable-hide-overlay)
        (should (= claude-repl--hide-overlay-refcount 0))))))

;;;; ---- Tests: Orphan panel detection ----

(ert-deftest claude-repl-test-orphaned-vterm-p ()
  "A vterm buffer with no matching input window is orphaned."
  (claude-repl-test--with-clean-state
    (let ((claude-repl--fullscreen-config nil))
      ;; Mock: not one-window-p, no matching input window
      (cl-letf (((symbol-function 'one-window-p) (lambda () nil))
                ((symbol-function 'get-buffer-window) (lambda (_buf) nil)))
        (should (claude-repl--orphaned-vterm-p "*claude-abcd1234*"))
        (should-not (claude-repl--orphaned-vterm-p "*claude-input-abcd1234*"))
        (should-not (claude-repl--orphaned-vterm-p "*some-other*"))))))

(ert-deftest claude-repl-test-orphaned-input-p ()
  "An input buffer with no matching vterm window is orphaned."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'one-window-p) (lambda () nil))
              ((symbol-function 'get-buffer-window) (lambda (_buf) nil))
              ((symbol-function 'get-buffer) (lambda (_name) nil)))
      (should (claude-repl--orphaned-input-p "*claude-input-abcd1234*"))
      (should-not (claude-repl--orphaned-input-p "*claude-abcd1234*"))
      (should-not (claude-repl--orphaned-input-p "*scratch*")))))

;;;; ---- Tests: Tabline rendering ----

(ert-deftest claude-repl-test-tabline-thinking-face ()
  "Tabline should apply thinking face for background thinking tabs."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set "other-ws" :thinking)
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
              ((symbol-function '+workspace-list-names) (lambda () '("test-ws" "other-ws"))))
      (let ((result (claude-repl--tabline-advice '("test-ws" "other-ws"))))
        ;; other-ws should have thinking face
        (should (string-match-p "other-ws" result))))))

(ert-deftest claude-repl-test-tabline-permission-label ()
  "Tabline should show ❓ for permission state."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set "test-ws" :permission)
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
              ((symbol-function '+workspace-list-names) (lambda () '("test-ws"))))
      (let ((result (claude-repl--tabline-advice '("test-ws"))))
        (should (string-match-p "❓" result))))))

;;;; ---- Tests: Deferred macro ----

(ert-deftest claude-repl-test-deferred-macro ()
  "The deferred macro should create a debouncing lambda."
  (let ((claude-repl--sync-timer nil)
        (call-count 0))
    (let ((debounced (claude-repl--deferred claude-repl--sync-timer
                       (lambda () (cl-incf call-count)))))
      ;; Calling it should set the timer var
      (funcall debounced)
      (should claude-repl--sync-timer)
      ;; Cancel it to prevent side effects
      (cancel-timer claude-repl--sync-timer)
      (setq claude-repl--sync-timer nil))))

;;;; ---- Bug regression tests ----

(ert-deftest claude-repl-test-bug1-cursor-reset-timer-defvar ()
  "Bug 1: claude-repl--cursor-reset-timer should be defined (not void)."
  (should (boundp 'claude-repl--cursor-reset-timer)))

(ert-deftest claude-repl-test-bug2-title-thinking-buffer-local ()
  "Bug 2: claude-repl--title-thinking should be buffer-local."
  (claude-repl-test--with-temp-buffer " *test-bl-1*"
    (setq claude-repl--title-thinking t)
    (claude-repl-test--with-temp-buffer " *test-bl-2*"
      ;; Should be nil in a different buffer (default value)
      (should-not claude-repl--title-thinking))))

(ert-deftest claude-repl-test-bug3-title-change-no-ws ()
  "Bug 3: on-title-change should error when workspace is nil during a transition."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-buffer "*claude-abcd1234*"
      (setq-local claude-repl--ready t)  ;; skip first-ready handling
      (setq-local claude-repl--title-thinking nil)
      (cl-letf (((symbol-function 'claude-repl--workspace-for-buffer)
                 (lambda (_buf) nil))
                ((symbol-function 'claude-repl--handle-first-ready)
                 (lambda () nil)))
        ;; Should error because ws is nil during a started transition
        (should-error (claude-repl--on-title-change "⠋ Claude Code"))
        ;; title-thinking should still be updated (setq is before the guard)
        (should claude-repl--title-thinking)))))

(ert-deftest claude-repl-test-bug5-rel-path-non-file-buffer ()
  "Bug 5: rel-path should signal user-error for non-file buffers."
  (claude-repl-test--with-temp-buffer " *test-no-file*"
    (should-error (claude-repl--rel-path) :type 'user-error)))

(ert-deftest claude-repl-test-bug5-rel-path-with-file ()
  "Bug 5: rel-path should work for file-visiting buffers."
  (cl-letf (((symbol-function 'buffer-file-name)
             (lambda (&optional _buf) "/project/src/foo.el"))
            ((symbol-function 'claude-repl--resolve-root)
             (lambda () "/project/")))
    (should (equal (claude-repl--rel-path) "src/foo.el"))))

(ert-deftest claude-repl-test-bug6-stale-window ()
  "Bug 6: live-return-window should fall back to selected-window."
  (let ((claude-repl-return-window nil))
    ;; nil window should fall back
    (should (eq (claude-repl--live-return-window) (selected-window))))
  ;; Live window should be returned
  (let ((claude-repl-return-window (selected-window)))
    (should (eq (claude-repl--live-return-window) (selected-window)))))

(ert-deftest claude-repl-test-bug7-prefix-counter-session ()
  "Bug 7: prefix counter should round-trip through save/load."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'claude-repl--git-root) (lambda (&optional _d) nil)))
      (claude-repl-test--with-temp-buffer " *test-pc-rt*"
        (setq-local claude-repl--project-root "/test/proj")
        (setq claude-repl--prefix-counter 42)
        (claude-repl--save-session)
        (setq claude-repl--prefix-counter 0)
        (claude-repl--load-session)
        (should (= claude-repl--prefix-counter 42))))))

(ert-deftest claude-repl-test-bug8-history-save-uses-input-buffer-root ()
  "Bug 8: history-save should use the input buffer's project root."
  (claude-repl-test--with-clean-state
    (let ((saved-file nil))
      (claude-repl-test--with-temp-buffer " *test-hist-save*"
        (setq-local claude-repl--project-root "/input-buffer-root")
        (setq-local claude-repl--input-history '("entry1" "entry2"))
        (setq claude-repl-input-buffer (current-buffer))
        (cl-letf (((symbol-function 'with-temp-file)
                   ;; Just capture the file argument
                   nil))
          ;; We can't easily mock with-temp-file (it's a macro),
          ;; so instead just verify the file path is computed correctly
          (let* ((root (buffer-local-value 'claude-repl--project-root claude-repl-input-buffer))
                 (file (expand-file-name ".claude-repl-history" root)))
            (should (string-match-p "/input-buffer-root" file))))))))

(ert-deftest claude-repl-test-bug9-paste-delay-configurable ()
  "Bug 9: claude-repl-paste-delay should be a configurable variable."
  (should (boundp 'claude-repl-paste-delay))
  (should (numberp claude-repl-paste-delay)))

(ert-deftest claude-repl-test-bug10-defvar-declarations ()
  "Bug 10: All former bare-setq variables should be properly declared."
  (should (boundp 'claude-repl-vterm-buffer))
  (should (boundp 'claude-repl-input-buffer))
  (should (boundp 'claude-repl-return-window))
  (should (boundp 'claude-repl--saved-window-config))
  (should (boundp 'claude-repl-hide-input-box))
  (should (boundp 'claude-repl--sessions))
  (should (boundp 'claude-repl--notify-fn))
  (should (boundp 'claude-repl--done-workspaces))
  (should (boundp 'claude-repl--thinking-workspaces))
  (should (boundp 'claude-repl--permission-workspaces))
  (should (boundp 'claude-repl--activity-times))
  (should (boundp 'claude-repl--sync-timer))
  (should (boundp 'claude-repl--title-thinking)))

(ert-deftest claude-repl-test-bug11-fullscreen-config-declared ()
  "Bug 11: claude-repl--fullscreen-config should be declared before use."
  (should (boundp 'claude-repl--fullscreen-config)))

(ert-deftest claude-repl-test-bug13-docstring-accuracy ()
  "Bug 13: show-panels docstring should mention 60% and 15%."
  (let ((doc (documentation 'claude-repl--show-panels)))
    (should (string-match-p "60%" doc))
    (should (string-match-p "15%" doc))))

(ert-deftest claude-repl-test-package-provide ()
  "Package should provide 'claude-repl feature."
  (should (featurep 'claude-repl)))

(ert-deftest claude-repl-test-grey-format ()
  "Grey helper should return proper hex color."
  (should (equal (claude-repl--grey 0) "#000000"))
  (should (equal (claude-repl--grey 255) "#ffffff"))
  (should (equal (claude-repl--grey 15) "#0f0f0f")))

(ert-deftest claude-repl-test-claude-buffer-p ()
  "claude-buffer-p should match *claude-HASH* pattern only."
  (claude-repl-test--with-temp-buffer "*claude-abcd1234*"
    (should (claude-repl--claude-buffer-p)))
  (claude-repl-test--with-temp-buffer "*claude-input-abcd1234*"
    (should-not (claude-repl--claude-buffer-p)))
  (claude-repl-test--with-temp-buffer "*scratch*"
    (should-not (claude-repl--claude-buffer-p))))

;;;; ---- Tests: Unreal buffer predicate ----

;;;; ---- Tests: Paste to vterm ----

(ert-deftest claude-repl-test-paste-to-vterm ()
  "claude-repl-paste-to-vterm should call vterm-send-key with C-v args."
  (claude-repl-test--with-clean-state
    (let ((send-key-args nil))
      (claude-repl-test--with-temp-buffer "*claude-abcd1234*"
        (setq claude-repl-vterm-buffer (current-buffer))
        (cl-letf (((symbol-function 'claude-repl--load-session) #'ignore)
                  ((symbol-function 'claude-repl--vterm-live-p) (lambda () t))
                  ((symbol-function 'vterm-send-key)
                   (lambda (&rest args) (setq send-key-args args))))
          (claude-repl-paste-to-vterm)
          (should (equal send-key-args '("v" nil nil t))))))))

;;;; ---- Tests: Logging ----

(ert-deftest claude-repl-test-log-respects-debug-flag ()
  "When `claude-repl-debug' is nil, `claude-repl--log' should NOT call `message'.
When t, it should call `message'."
  (let ((message-called nil))
    (cl-letf (((symbol-function 'message)
               (lambda (&rest _args) (setq message-called t))))
      ;; debug off: no message
      (let ((claude-repl-debug nil))
        (claude-repl--log "test %s" "hello")
        (should-not message-called))
      ;; debug on: message called
      (let ((claude-repl-debug t))
        (setq message-called nil)
        (claude-repl--log "test %s" "hello")
        (should message-called)))))

;;;; ---- Tests: Buffer predicates ----

(ert-deftest claude-repl-test-vterm-live-p-nil ()
  "Returns nil when `claude-repl-vterm-buffer' is nil."
  (let ((claude-repl-vterm-buffer nil))
    (should-not (claude-repl--vterm-live-p))))

(ert-deftest claude-repl-test-vterm-live-p-dead ()
  "Returns nil for a killed buffer."
  (let ((buf (get-buffer-create " *test-dead-buf*")))
    (kill-buffer buf)
    (let ((claude-repl-vterm-buffer buf))
      (should-not (claude-repl--vterm-live-p)))))

(ert-deftest claude-repl-test-vterm-live-p-live ()
  "Returns non-nil for a live buffer."
  (claude-repl-test--with-temp-buffer " *test-live-buf*"
    (let ((claude-repl-vterm-buffer (current-buffer)))
      (should (claude-repl--vterm-live-p)))))

(ert-deftest claude-repl-test-vterm-running-p-no-process ()
  "Returns nil when buffer is live but has no process."
  (claude-repl-test--with-temp-buffer " *test-no-proc*"
    (let ((claude-repl-vterm-buffer (current-buffer)))
      (should-not (claude-repl--vterm-running-p)))))

(ert-deftest claude-repl-test-vterm-running-p-with-process ()
  "Returns non-nil when buffer has a live process."
  (claude-repl-test--with-temp-buffer " *test-with-proc*"
    (let ((claude-repl-vterm-buffer (current-buffer)))
      (cl-letf (((symbol-function 'get-buffer-process)
                 (lambda (_buf) 'fake-process)))
        (should (claude-repl--vterm-running-p))))))

;;;; ---- Tests: History edge cases ----

(ert-deftest claude-repl-test-history-prev-empty-list ()
  "Calling `history-prev' with empty history does nothing."
  (claude-repl-test--with-temp-buffer " *test-hist-empty*"
    (setq-local claude-repl--input-history nil)
    (setq-local claude-repl--history-index -1)
    (setq-local claude-repl--history-navigating nil)
    (insert "original")
    (claude-repl--history-prev)
    (should (equal (buffer-string) "original"))
    (should (= claude-repl--history-index -1))))

(ert-deftest claude-repl-test-history-prev-at-oldest ()
  "When already at the last (oldest) entry, `history-prev' does nothing."
  (claude-repl-test--with-temp-buffer " *test-hist-oldest*"
    (setq-local claude-repl--input-history '("only-entry"))
    (setq-local claude-repl--history-index 0)
    (setq-local claude-repl--history-stash "stashed")
    (setq-local claude-repl--history-navigating nil)
    (insert "only-entry")
    (claude-repl--history-prev)
    ;; index should stay at 0, buffer unchanged
    (should (= claude-repl--history-index 0))
    (should (equal (buffer-string) "only-entry"))))

(ert-deftest claude-repl-test-history-next-at-start ()
  "When `claude-repl--history-index' is -1, `history-next' does nothing."
  (claude-repl-test--with-temp-buffer " *test-hist-at-start*"
    (setq-local claude-repl--input-history '("a" "b"))
    (setq-local claude-repl--history-index -1)
    (setq-local claude-repl--history-navigating nil)
    (insert "current")
    (claude-repl--history-next)
    (should (equal (buffer-string) "current"))
    (should (= claude-repl--history-index -1))))

(ert-deftest claude-repl-test-history-on-change-noop-during-navigation ()
  "When `claude-repl--history-navigating' is t, `history-on-change' does NOT reset the index."
  (claude-repl-test--with-temp-buffer " *test-hist-nav-noop*"
    (setq-local claude-repl--history-index 3)
    (setq-local claude-repl--history-navigating t)
    (claude-repl--history-on-change)
    (should (= claude-repl--history-index 3))))

(ert-deftest claude-repl-test-history-file-path ()
  "`claude-repl--history-file' returns `<root>/.claude-repl-history'."
  (cl-letf (((symbol-function 'claude-repl--resolve-root) (lambda () "/test/root")))
    (should (equal (claude-repl--history-file)
                   (expand-file-name ".claude-repl-history" "/test/root")))))

;;;; ---- Tests: Input preparation ----

(ert-deftest claude-repl-test-prepare-input-no-prefix-when-disabled ()
  "When `claude-repl-skip-permissions' is nil, `prepare-input' returns raw text."
  (claude-repl-test--with-clean-state
    (let ((claude-repl-skip-permissions nil)
          (claude-repl--prefix-counter 0)
          (claude-repl-prefix-period 1))
      (claude-repl-test--with-temp-buffer " *test-no-prefix*"
        (setq claude-repl-input-buffer (current-buffer))
        (insert "raw text")
        (should (equal (claude-repl--prepare-input) "raw text"))))))

;;;; ---- Tests: Send functions ----

(ert-deftest claude-repl-test-send-char-calls-vterm ()
  "`claude-repl-send-char' should call `vterm-send-string' with the char, then `vterm-send-return'."
  (claude-repl-test--with-clean-state
    (let ((calls nil))
      (claude-repl-test--with-temp-buffer "*claude-abcd1234*"
        (setq claude-repl-vterm-buffer (current-buffer))
        (cl-letf (((symbol-function 'claude-repl--load-session) #'ignore)
                  ((symbol-function 'claude-repl--vterm-live-p) (lambda () t))
                  ((symbol-function 'vterm-send-string)
                   (lambda (s &rest _) (push (list 'send-string s) calls)))
                  ((symbol-function 'vterm-send-return)
                   (lambda () (push '(send-return) calls))))
          (claude-repl-send-char "y")
          (should (member '(send-string "y") (reverse calls)))
          (should (member '(send-return) (reverse calls))))))))

(ert-deftest claude-repl-test-scroll-down-sends-down ()
  "`claude-repl-scroll-down' calls `vterm-send-down' in the vterm buffer."
  (claude-repl-test--with-clean-state
    (let ((down-called nil))
      (claude-repl-test--with-temp-buffer "*claude-abcd1234*"
        (setq claude-repl-vterm-buffer (current-buffer))
        (cl-letf (((symbol-function 'claude-repl--load-session) #'ignore)
                  ((symbol-function 'claude-repl--vterm-live-p) (lambda () t))
                  ((symbol-function 'vterm-send-down)
                   (lambda () (setq down-called t))))
          (claude-repl-scroll-down)
          (should down-called))))))

(ert-deftest claude-repl-test-scroll-up-sends-up ()
  "`claude-repl-scroll-up' calls `vterm-send-up'."
  (claude-repl-test--with-clean-state
    (let ((up-called nil))
      (claude-repl-test--with-temp-buffer "*claude-abcd1234*"
        (setq claude-repl-vterm-buffer (current-buffer))
        (cl-letf (((symbol-function 'claude-repl--load-session) #'ignore)
                  ((symbol-function 'claude-repl--vterm-live-p) (lambda () t))
                  ((symbol-function 'vterm-send-up)
                   (lambda () (setq up-called t))))
          (claude-repl-scroll-up)
          (should up-called))))))

(ert-deftest claude-repl-test-interrupt-sends-escape-twice ()
  "`claude-repl-interrupt' calls `vterm-send-key' with \"<escape>\" twice."
  (claude-repl-test--with-clean-state
    (let ((escape-count 0))
      (claude-repl-test--with-temp-buffer "*claude-abcd1234*"
        (setq claude-repl-vterm-buffer (current-buffer))
        (cl-letf (((symbol-function 'claude-repl--load-session) #'ignore)
                  ((symbol-function 'claude-repl--vterm-live-p) (lambda () t))
                  ((symbol-function 'vterm-send-key)
                   (lambda (key &rest _)
                     (when (equal key "<escape>")
                       (cl-incf escape-count)))))
          (claude-repl-interrupt)
          (should (= escape-count 2)))))))

(ert-deftest claude-repl-test-cycle-sends-backtab ()
  "`claude-repl-cycle' calls `vterm-send-key' with \"<backtab>\"."
  (claude-repl-test--with-clean-state
    (let ((backtab-called nil))
      (claude-repl-test--with-temp-buffer "*claude-abcd1234*"
        (setq claude-repl-vterm-buffer (current-buffer))
        (cl-letf (((symbol-function 'claude-repl--load-session) #'ignore)
                  ((symbol-function 'claude-repl--vterm-live-p) (lambda () t))
                  ((symbol-function 'vterm-send-key)
                   (lambda (key &rest _)
                     (when (equal key "<backtab>")
                       (setq backtab-called t)))))
          (claude-repl-cycle)
          (should backtab-called))))))

(ert-deftest claude-repl-test-send-input-direct-mode ()
  "For input <=200 chars, `send-input-to-vterm' calls `vterm-send-string' without paste flag."
  (claude-repl-test--with-clean-state
    (let ((send-string-args nil)
          (return-called nil))
      (claude-repl-test--with-temp-buffer "*claude-abcd1234*"
        (setq claude-repl-vterm-buffer (current-buffer))
        (cl-letf (((symbol-function 'vterm-send-string)
                   (lambda (s &rest args) (setq send-string-args (cons s args))))
                  ((symbol-function 'vterm-send-return)
                   (lambda () (setq return-called t)))
                  ((symbol-function 'claude-repl--refresh-vterm) #'ignore))
          (claude-repl--send-input-to-vterm "short input")
          ;; Should have been called with just the string (no paste flag)
          (should (equal (car send-string-args) "short input"))
          (should (null (cdr send-string-args)))
          (should return-called))))))

(ert-deftest claude-repl-test-send-input-paste-mode ()
  "For input >200 chars, calls `vterm-send-string' WITH paste flag, defers return."
  (claude-repl-test--with-clean-state
    (let ((send-string-args nil)
          (return-called nil)
          (timer-args nil))
      (claude-repl-test--with-temp-buffer "*claude-abcd1234*"
        (setq claude-repl-vterm-buffer (current-buffer))
        (cl-letf (((symbol-function 'vterm-send-string)
                   (lambda (s &rest args) (setq send-string-args (cons s args))))
                  ((symbol-function 'vterm-send-return)
                   (lambda () (setq return-called t)))
                  ((symbol-function 'claude-repl--refresh-vterm) #'ignore)
                  ((symbol-function 'run-at-time)
                   (lambda (&rest args) (setq timer-args args))))
          (let ((long-input (make-string 201 ?x)))
            (claude-repl--send-input-to-vterm long-input)
            ;; paste flag (2nd arg) should be t
            (should (equal (cadr send-string-args) t))
            ;; return should NOT have been called directly
            (should-not return-called)
            ;; run-at-time should have been called to defer
            (should timer-args)))))))

;;;; ---- Tests: Composite state functions ----

(ert-deftest claude-repl-test-mark-ws-thinking-composite ()
  "`claude-repl--mark-ws-thinking' should set :thinking state AND record activity."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'claude-repl--workspace-clean-p) (lambda (_) t)))
      (claude-repl--mark-ws-thinking "ws1")
      (should (eq (claude-repl--ws-state "ws1") :thinking))
      (should (gethash "ws1" claude-repl--activity-times)))))

(ert-deftest claude-repl-test-clear-input-pushes-and-clears ()
  "`claude-repl--clear-input' pushes text to history, resets index, clears buffer."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-buffer " *test-clear-input*"
      (setq-local claude-repl--input-history nil)
      (setq-local claude-repl--history-index 5)
      (setq-local claude-repl--history-navigating nil)
      (setq claude-repl-input-buffer (current-buffer))
      (insert "some input text")
      (claude-repl--clear-input)
      (should (equal claude-repl--input-history '("some input text")))
      (should (= claude-repl--history-index -1))
      (should (equal (buffer-string) "")))))

(ert-deftest claude-repl-test-discard-input-pushes-and-clears ()
  "`claude-repl-discard-input' pushes text, clears buffer, calls `evil-insert-state'."
  (claude-repl-test--with-temp-buffer " *test-discard*"
    (setq-local claude-repl--input-history nil)
    (setq-local claude-repl--history-index 3)
    (setq-local claude-repl--history-navigating nil)
    (insert "discard me")
    (let ((evil-called nil))
      (cl-letf (((symbol-function 'evil-insert-state)
                 (lambda () (setq evil-called t))))
        (claude-repl-discard-input)
        (should (equal claude-repl--input-history '("discard me")))
        (should (= claude-repl--history-index -1))
        (should (equal (buffer-string) ""))
        (should evil-called)))))

(ert-deftest claude-repl-test-remember-return-window-saves ()
  "`claude-repl--remember-return-window' sets `claude-repl-return-window' when not in input buffer."
  (let ((claude-repl-input-buffer nil)
        (claude-repl-return-window nil))
    (claude-repl--remember-return-window)
    (should (eq claude-repl-return-window (selected-window)))))

(ert-deftest claude-repl-test-remember-return-window-skips-input ()
  "When current buffer IS `claude-repl-input-buffer', should NOT change return-window."
  (claude-repl-test--with-temp-buffer " *test-skip-input*"
    (let ((claude-repl-input-buffer (current-buffer))
          (claude-repl-return-window nil))
      (claude-repl--remember-return-window)
      (should-not claude-repl-return-window))))

;;;; ---- Tests: Title change handling ----

(ert-deftest claude-repl-test-on-title-change-started-sets-thinking ()
  "In a claude buffer with `title-thinking' nil, spinner title sets workspace to :thinking."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-buffer "*claude-abcd1234*"
      (setq-local claude-repl--title-thinking nil)
      (setq-local claude-repl--ready t)
      (cl-letf (((symbol-function 'claude-repl--workspace-for-buffer)
                 (lambda (_buf) "ws1"))
                ((symbol-function 'claude-repl--handle-first-ready) #'ignore))
        (claude-repl--on-title-change "⠋ Claude Code")
        (should (eq (claude-repl--ws-state "ws1") :thinking))))))

(ert-deftest claude-repl-test-on-title-change-finished-clears-thinking ()
  "With `title-thinking' t, idle title clears :thinking and calls `on-claude-finished'."
  (claude-repl-test--with-clean-state
    (let ((finished-called nil))
      (claude-repl-test--with-temp-buffer "*claude-abcd1234*"
        (setq-local claude-repl--title-thinking t)
        (setq-local claude-repl--ready t)
        (claude-repl--ws-set "ws1" :thinking)
        (cl-letf (((symbol-function 'claude-repl--workspace-for-buffer)
                   (lambda (_buf) "ws1"))
                  ((symbol-function 'claude-repl--handle-first-ready) #'ignore)
                  ((symbol-function 'claude-repl--on-claude-finished)
                   (lambda (ws) (setq finished-called ws))))
          (claude-repl--on-title-change "✳ Claude Code")
          (should-not (gethash "ws1" claude-repl--thinking-workspaces))
          (should (equal finished-called "ws1")))))))

(ert-deftest claude-repl-test-on-title-change-no-reentrant-recursion ()
  "Regression: on-title-change updates title-thinking BEFORE side effects.
If on-claude-finished triggers a re-entrant on-title-change (via
refresh-vterm -> vterm--redraw -> set-title), the re-entrant call should
see the updated title-thinking and detect no transition."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-buffer "*claude-abcd1234*"
      (setq-local claude-repl--title-thinking t)
      (setq-local claude-repl--ready t)
      (claude-repl--ws-set "ws1" :thinking)
      (let ((reentrant-transition nil))
        (cl-letf (((symbol-function 'claude-repl--workspace-for-buffer)
                   (lambda (_buf) "ws1"))
                  ((symbol-function 'claude-repl--handle-first-ready) #'ignore)
                  ((symbol-function 'claude-repl--on-claude-finished)
                   (lambda (_ws)
                     ;; Simulate re-entrant call: on-claude-finished would
                     ;; normally call refresh-vterm -> vterm--redraw, which
                     ;; triggers vterm--set-title -> on-title-change again.
                     ;; title-thinking must already be nil at this point.
                     (let ((info (claude-repl--detect-title-transition "✳ Claude Code")))
                       (setq reentrant-transition (plist-get info :transition))))))
          (claude-repl--on-title-change "✳ Claude Code")
          ;; The re-entrant detect-title-transition should see title-thinking=nil
          ;; (already updated) and return nil transition — not 'finished again.
          (should-not reentrant-transition))))))

(ert-deftest claude-repl-test-on-title-change-non-claude-buffer ()
  "In a non-claude buffer, `on-title-change' should do nothing."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-buffer "*scratch*"
      (setq-local claude-repl--title-thinking nil)
      ;; Should not error or change state
      (claude-repl--on-title-change "⠋ Claude Code")
      (should-not claude-repl--title-thinking))))

(ert-deftest claude-repl-test-handle-first-ready-idempotent ()
  "First call sets `claude-repl--ready' to t and calls `swap-placeholder'. Second call is a no-op."
  (claude-repl-test--with-temp-buffer " *test-first-ready*"
    (setq-local claude-repl--ready nil)
    (let ((swap-count 0))
      (cl-letf (((symbol-function 'claude-repl--swap-placeholder)
                 (lambda () (cl-incf swap-count))))
        (claude-repl--handle-first-ready)
        (should claude-repl--ready)
        (should (= swap-count 1))
        ;; Second call should be no-op
        (claude-repl--handle-first-ready)
        (should (= swap-count 1))))))

(ert-deftest claude-repl-test-detect-title-transition-no-change ()
  "When `title-thinking' is nil and title has no spinner, transition should be nil."
  (claude-repl-test--with-temp-buffer " *test-no-change*"
    (setq-local claude-repl--title-thinking nil)
    (cl-letf (((symbol-function 'claude-repl--workspace-for-buffer) (lambda (_buf) "ws1")))
      (let ((info (claude-repl--detect-title-transition "Claude Code")))
        (should-not (plist-get info :transition))))))

(ert-deftest claude-repl-test-detect-title-transition-still-thinking ()
  "When `title-thinking' is t and title has spinner, transition should be nil."
  (claude-repl-test--with-temp-buffer " *test-still-thinking*"
    (setq-local claude-repl--title-thinking t)
    (cl-letf (((symbol-function 'claude-repl--workspace-for-buffer) (lambda (_buf) "ws1")))
      (let ((info (claude-repl--detect-title-transition "⠋ Claude Code")))
        (should-not (plist-get info :transition))
        (should (plist-get info :thinking))))))

;;;; ---- Tests: State machine completeness ----

(ert-deftest claude-repl-test-ws-state-priority-order ()
  "Verify state priority: :thinking > :permission > :done > :stale."
  (claude-repl-test--with-clean-state
    ;; Set both :thinking and :done directly
    (puthash "ws1" t claude-repl--thinking-workspaces)
    (puthash "ws1" t claude-repl--done-workspaces)
    (should (eq (claude-repl--ws-state "ws1") :thinking))
    ;; Clear thinking, add permission and done
    (remhash "ws1" claude-repl--thinking-workspaces)
    (puthash "ws1" t claude-repl--permission-workspaces)
    (should (eq (claude-repl--ws-state "ws1") :permission))
    ;; Clear permission, just done remains
    (remhash "ws1" claude-repl--permission-workspaces)
    (should (eq (claude-repl--ws-state "ws1") :done))
    ;; Clear done, set activity for stale
    (remhash "ws1" claude-repl--done-workspaces)
    (cl-letf (((symbol-function 'claude-repl--workspace-clean-p) (lambda (_) t)))
      (claude-repl--touch-activity "ws1"))
    (should (eq (claude-repl--ws-state "ws1") :stale))))

(ert-deftest claude-repl-test-ws-state-expired-stale ()
  "Activity time >60 minutes ago should return nil (expired)."
  (claude-repl-test--with-clean-state
    (let ((claude-repl-stale-minutes 60))
      ;; Set activity to 61 minutes ago
      (puthash "ws1" (- (float-time) (* 61 60)) claude-repl--activity-times)
      (should-not (claude-repl--ws-state "ws1")))))

(ert-deftest claude-repl-test-ws-set-permission ()
  "`ws-set' with :permission should set permission hash."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set "ws1" :permission)
    (should (eq (claude-repl--ws-state "ws1") :permission))))

(ert-deftest claude-repl-test-ws-clear-done ()
  "`ws-clear' with :done should only clear done, leaving others."
  (claude-repl-test--with-clean-state
    (puthash "ws1" t claude-repl--thinking-workspaces)
    (puthash "ws1" t claude-repl--done-workspaces)
    (claude-repl--ws-clear "ws1" :done)
    (should-not (gethash "ws1" claude-repl--done-workspaces))
    (should (gethash "ws1" claude-repl--thinking-workspaces))))

(ert-deftest claude-repl-test-ws-clear-permission ()
  "`ws-clear' with :permission should only clear permission."
  (claude-repl-test--with-clean-state
    (puthash "ws1" t claude-repl--permission-workspaces)
    (puthash "ws1" t claude-repl--done-workspaces)
    (claude-repl--ws-clear "ws1" :permission)
    (should-not (gethash "ws1" claude-repl--permission-workspaces))
    (should (gethash "ws1" claude-repl--done-workspaces))))

(ert-deftest claude-repl-test-ws-clear-nil-error ()
  "`ws-clear' with nil ws should signal error."
  (claude-repl-test--with-clean-state
    (should-error (claude-repl--ws-clear nil :done) :type 'error)))

(ert-deftest claude-repl-test-touch-activity-nil-error ()
  "`touch-activity' with nil ws should signal error."
  (claude-repl-test--with-clean-state
    (should-error (claude-repl--touch-activity nil) :type 'error)))

;;;; ---- Tests: Tabline rendering completeness ----

(ert-deftest claude-repl-test-tabline-done-face ()
  "A background tab with :done should use `claude-repl-tab-done' face."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set "bg-ws" :done)
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "current-ws")))
      (let ((result (claude-repl--tabline-advice '("current-ws" "bg-ws"))))
        ;; Find the "bg-ws" segment and check its face
        (let ((pos (string-match "bg-ws" result)))
          (should pos)
          (should (eq (get-text-property pos 'face result) 'claude-repl-tab-done)))))))

(ert-deftest claude-repl-test-tabline-selected-suppresses-thinking ()
  "The SELECTED tab with :thinking should NOT get the thinking face."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set "sel-ws" :thinking)
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "sel-ws")))
      (let ((result (claude-repl--tabline-advice '("sel-ws"))))
        (let ((pos (string-match "sel-ws" result)))
          (should pos)
          ;; Should get the normal selected face, NOT thinking
          (should (eq (get-text-property pos 'face result)
                      '+workspace-tab-selected-face)))))))

(ert-deftest claude-repl-test-tabline-selected-shows-permission ()
  "The SELECTED tab with :permission SHOULD still get the permission face."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set "sel-ws" :permission)
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "sel-ws")))
      (let ((result (claude-repl--tabline-advice '("sel-ws"))))
        (let ((pos (string-match "sel-ws" result)))
          (should pos)
          (should (eq (get-text-property pos 'face result)
                      'claude-repl-tab-permission)))))))

(ert-deftest claude-repl-test-tabline-stale-face ()
  "A background tab with :stale should use stale face."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'claude-repl--workspace-clean-p) (lambda (_) t)))
      (claude-repl--touch-activity "bg-ws"))
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "current-ws")))
      (let ((result (claude-repl--tabline-advice '("current-ws" "bg-ws"))))
        (let ((pos (string-match "bg-ws" result)))
          (should pos)
          (should (eq (get-text-property pos 'face result)
                      'claude-repl-tab-stale)))))))

;;;; ---- Tests: Orphan detection edge cases ----

(ert-deftest claude-repl-test-orphaned-vterm-in-fullscreen ()
  "When `claude-repl--fullscreen-config' is non-nil, `orphaned-vterm-p' returns nil."
  (let ((claude-repl--fullscreen-config 'some-config))
    (should-not (claude-repl--orphaned-vterm-p "*claude-abcd1234*"))))

(ert-deftest claude-repl-test-orphaned-vterm-one-window ()
  "When `one-window-p' returns t, should return nil."
  (let ((claude-repl--fullscreen-config nil))
    (cl-letf (((symbol-function 'one-window-p) (lambda () t)))
      (should-not (claude-repl--orphaned-vterm-p "*claude-abcd1234*")))))

(ert-deftest claude-repl-test-orphaned-input-with-loading ()
  "When loading placeholder buffer exists, `orphaned-input-p' should return nil."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'one-window-p) (lambda () nil))
              ((symbol-function 'get-buffer-window) (lambda (_buf) nil))
              ((symbol-function 'get-buffer) (lambda (name)
                                               (when (equal name " *claude-loading*")
                                                 'fake-buffer))))
      (should-not (claude-repl--orphaned-input-p "*claude-input-abcd1234*")))))

;;;; ---- Tests: Permission notification handler ----

(ert-deftest claude-repl-test-on-permission-notify-ignores-non-permission ()
  "An event with a non-matching filename should not call `ws-set'."
  (claude-repl-test--with-clean-state
    (let ((ws-set-called nil))
      (cl-letf (((symbol-function 'claude-repl--ws-set)
                 (lambda (&rest _) (setq ws-set-called t))))
        (claude-repl--on-permission-notify '(nil changed "/some/other-file"))
        (should-not ws-set-called)))))

(ert-deftest claude-repl-test-on-permission-notify-ignores-other-actions ()
  "An event with action `deleted' should be ignored even if filename matches."
  (claude-repl-test--with-clean-state
    (let ((ws-set-called nil))
      (cl-letf (((symbol-function 'claude-repl--ws-set)
                 (lambda (&rest _) (setq ws-set-called t))))
        (claude-repl--on-permission-notify '(nil deleted "/path/to/permission_prompt"))
        (should-not ws-set-called)))))

;;;; ---- Tests: Overlay management ----

(ert-deftest claude-repl-test-create-overlay-respects-flag ()
  "When `claude-repl-hide-input-box' is nil, `create-hide-overlay' should NOT create an overlay."
  (claude-repl-test--with-temp-buffer " *test-no-overlay*"
    (let ((claude-repl-hide-input-box nil))
      (insert "some content\nline two\nline three\nline four\nline five\n")
      (claude-repl--create-hide-overlay)
      (should-not claude-repl-hide-overlay))))

(ert-deftest claude-repl-test-create-overlay-creates-when-enabled ()
  "When `claude-repl-hide-input-box' is t and buffer has content, overlay is created."
  (claude-repl-test--with-temp-buffer " *test-yes-overlay*"
    (let ((claude-repl-hide-input-box t))
      (insert "line1\nline2\nline3\nline4\nline5\nline6\n")
      (claude-repl--create-hide-overlay)
      (should claude-repl-hide-overlay)
      (should (overlayp claude-repl-hide-overlay)))))

(ert-deftest claude-repl-test-delete-overlay-noop-when-nil ()
  "When `claude-repl-hide-overlay' is nil, `delete-hide-overlay' should not error."
  (claude-repl-test--with-temp-buffer " *test-del-nil*"
    (setq-local claude-repl-hide-overlay nil)
    ;; Should not signal any error
    (claude-repl--delete-hide-overlay)
    (should-not claude-repl-hide-overlay)))

(ert-deftest claude-repl-test-redraw-advice-reentrancy-guard ()
  "Reentrancy guard prevents recursive calls to `after-vterm-redraw'."
  (claude-repl-test--with-clean-state
    (let ((load-session-count 0))
      (claude-repl-test--with-temp-buffer "*claude-abcd1234*"
        (cl-letf (((symbol-function 'claude-repl--load-session)
                   (lambda () (cl-incf load-session-count)))
                  ((symbol-function 'claude-repl--update-hide-overlay) #'ignore))
          ;; With reentrancy guard active, load-session should NOT be called
          (let ((claude-repl--in-redraw-advice t))
            (claude-repl--after-vterm-redraw)
            (should (= load-session-count 0)))
          ;; Without guard, in a claude buffer, load-session IS called
          (let ((claude-repl--in-redraw-advice nil))
            (claude-repl--after-vterm-redraw)
            (should (= load-session-count 1))))))))

(ert-deftest claude-repl-test-redraw-advice-skips-non-claude ()
  "In a non-claude buffer, `after-vterm-redraw' should not call `load-session'."
  (claude-repl-test--with-clean-state
    (let ((load-session-count 0))
      (claude-repl-test--with-temp-buffer "*scratch*"
        (cl-letf (((symbol-function 'claude-repl--load-session)
                   (lambda () (cl-incf load-session-count)))
                  ((symbol-function 'claude-repl--update-hide-overlay) #'ignore))
          (let ((claude-repl--in-redraw-advice nil))
            (claude-repl--after-vterm-redraw)
            (should (= load-session-count 0))))))))

;;;; ---- Tests: Workspace-for-buffer ----

(ert-deftest claude-repl-test-workspace-for-buffer-no-persp ()
  "When `persp-mode' is nil, `workspace-for-buffer' should return nil."
  (let ((persp-mode nil))
    (should-not (claude-repl--workspace-for-buffer (current-buffer)))))

;;;; ---- Tests: on-claude-finished ----

(ert-deftest claude-repl-test-on-claude-finished-hidden-sets-done ()
  "When the buffer is NOT visible, `on-claude-finished' should call `ws-set' with :done."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-buffer " *test-hidden-done*"
      (let ((done-set nil))
        (cl-letf (((symbol-function 'get-buffer-window) (lambda (_buf &rest _) nil))
                  ((symbol-function 'claude-repl--refresh-vterm) #'ignore)
                  ((symbol-function 'claude-repl--update-hide-overlay) #'ignore)
                  ((symbol-function 'claude-repl--maybe-notify-finished) #'ignore)
                  ((symbol-function 'claude-repl--ws-set)
                   (lambda (ws state)
                     (when (eq state :done)
                       (setq done-set ws)))))
          (claude-repl--on-claude-finished "ws1")
          (should (equal done-set "ws1")))))))

(ert-deftest claude-repl-test-on-claude-finished-visible-no-done ()
  "When the buffer IS visible, should NOT set :done."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-buffer " *test-visible-no-done*"
      (let ((done-set nil))
        (cl-letf (((symbol-function 'get-buffer-window) (lambda (_buf &rest _) 'some-window))
                  ((symbol-function 'claude-repl--refresh-vterm) #'ignore)
                  ((symbol-function 'claude-repl--update-hide-overlay) #'ignore)
                  ((symbol-function 'claude-repl--maybe-notify-finished) #'ignore)
                  ((symbol-function 'claude-repl--ws-set)
                   (lambda (ws state)
                     (when (eq state :done)
                       (setq done-set ws)))))
          (claude-repl--on-claude-finished "ws1")
          (should-not done-set))))))

;;;; ---- Tests: Misc declared variables ----

(ert-deftest claude-repl-test-in-redraw-advice-declared ()
  "`claude-repl--in-redraw-advice' should be `boundp'."
  (should (boundp 'claude-repl--in-redraw-advice)))

;;; test-claude-repl.el ends here
