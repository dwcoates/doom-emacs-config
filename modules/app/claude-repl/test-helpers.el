;;; test-helpers.el --- Shared test infrastructure for claude-repl -*- lexical-binding: t; -*-

;;; Commentary:

;; Shared stub layer and test utilities for all claude-repl test files.
;; Each per-module test file should load this before defining tests:
;;
;;   (load (expand-file-name "test-helpers.el" (file-name-directory
;;                                               (or load-file-name buffer-file-name)))
;;         nil t)

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)

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

(unless (fboundp 'load!)
  (defmacro load! (filename &optional path noerror)
    "Stub: load FILENAME relative to the current file's directory."
    `(load (expand-file-name ,filename
                             ,(or path '(file-name-directory
                                         (or load-file-name buffer-file-name))))
           nil ,(if noerror noerror t))))

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

(unless (fboundp '+workspace--message-body)
  (defun +workspace--message-body (message &optional _type)
    "Stub: mimic Doom by prefixing the (stubbed) tabline + separator.
Tests that exercise the override at
`claude-repl--workspace-message-body-advice' assert that the advice
strips this prefix, so the stub must include it (otherwise the
advice's effect would be invisible)."
    (concat (+workspace--tabline) " | " (format "%s" message))))

(unless (fboundp '+workspace--protected-p)
  (defun +workspace--protected-p (name)
    "Stub: protected when NAME equals `persp-nil-name'."
    (equal name (and (boundp 'persp-nil-name) persp-nil-name))))

(unless (fboundp '+workspace-switch)
  (defun +workspace-switch (_name &optional _auto-create-p)
    "Stub: no-op."
    nil))

(unless (fboundp '+workspace-error)
  (defun +workspace-error (message &optional _noerror)
    "Stub: signal `user-error' with MESSAGE so tests can observe it."
    (user-error "%s" message)))

(unless (boundp '+workspaces-main)
  (defvar +workspaces-main "main" "Stub: simulated main workspace name."))

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

;; vterm-mode-map stub — `(after! vterm ...)' in keybindings.el strips
;; conflicting `C-S-<letter>' bindings via `define-key' at load time
;; (the `after!' shim above is eager), so the variable must exist.
(unless (boundp 'vterm-mode-map)
  (defvar vterm-mode-map (make-sparse-keymap) "Stub."))

;; general-override-mode-map stub — keybindings.el installs scroll-output
;; override chords into this keymap at load time, so the variable must
;; exist before that load runs.
(unless (boundp 'general-override-mode-map)
  (defvar general-override-mode-map (make-sparse-keymap) "Stub."))
(unless (boundp 'vterm--term)
  (defvar vterm--term nil "Stub."))
(unless (boundp 'vterm--process)
  (defvar vterm--process nil "Stub."))

;; evil stubs
(unless (fboundp 'evil-insert-state)
  (defun evil-insert-state () "Stub." nil))
(unless (boundp 'evil-insert-state-exit-hook)
  (defvar evil-insert-state-exit-hook nil "Stub hook."))
(unless (boundp 'evil-escape-inhibit)
  (defvar-local evil-escape-inhibit nil "Stub."))
(unless (fboundp 'evil-window-left)
  (defun evil-window-left (&rest _args) "Stub." nil))
(unless (fboundp 'evil-define-key)
  (defun evil-define-key (&rest _args) "Stub." nil))

;; magit stubs
(unless (fboundp 'magit-current-section)
  (defun magit-current-section () "Stub." nil))
(unless (fboundp 'magit-file-at-point)
  (defun magit-file-at-point () "Stub." nil))
(unless (fboundp 'magit-toplevel)
  (defun magit-toplevel () "Stub." nil))
(unless (fboundp 'magit-section-match)
  (defun magit-section-match (_type) "Stub." nil))
(unless (fboundp 'magit-status)
  (defun magit-status (&rest _args) "Stub." nil))
(unless (fboundp 'magit-visit-thing)
  (defun magit-visit-thing (&rest _args) "Stub." nil))
(unless (fboundp 'magit-diff-visit-file)
  (defun magit-diff-visit-file (&rest _args) "Stub." nil))
(unless (fboundp 'magit-diff-visit-worktree-file)
  (defun magit-diff-visit-worktree-file (&rest _args) "Stub." nil))
(unless (fboundp 'magit-diff-visit-file-other-window)
  (defun magit-diff-visit-file-other-window (&rest _args) "Stub." nil))
(unless (fboundp 'magit-insert-tags-header)
  (defun magit-insert-tags-header (&rest _args) "Stub." nil))
(unless (fboundp 'magit-refresh)
  (defun magit-refresh (&rest _args) "Stub." nil))
(unless (fboundp 'magit-format-ref-labels)
  (defun magit-format-ref-labels (string) "Stub: identity." string))

;; magit variable stubs — `after!' is a no-op shim in tests, so the
;; `(after! magit ...)' body in magit.el executes and would reference
;; these at load time without them.
(unless (boundp 'magit-no-confirm)
  (defvar magit-no-confirm nil "Stub."))
(unless (boundp 'magit-diff-visit-previous-blob)
  (defvar magit-diff-visit-previous-blob nil "Stub."))
(unless (boundp 'magit-section-initial-visibility-alist)
  (defvar magit-section-initial-visibility-alist nil "Stub."))
(unless (boundp 'magit-status-headers-hook)
  (defvar magit-status-headers-hook nil "Stub: simulated magit headers hook."))
(unless (boundp 'magit-display-buffer-function)
  (defvar magit-display-buffer-function nil "Stub."))

;; magit keymap stubs — `define-key' in `(after! magit-diff ...)'
;; executes eagerly under the test shim, so these must exist.
(unless (boundp 'magit-unstaged-section-map)
  (defvar magit-unstaged-section-map (make-sparse-keymap) "Stub."))
(unless (boundp 'magit-staged-section-map)
  (defvar magit-staged-section-map (make-sparse-keymap) "Stub."))
(unless (boundp 'magit-untracked-section-map)
  (defvar magit-untracked-section-map (make-sparse-keymap) "Stub."))
(unless (boundp 'magit-mode-map)
  (defvar magit-mode-map (make-sparse-keymap) "Stub."))
(unless (boundp 'magit-file-section-map)
  (defvar magit-file-section-map (make-sparse-keymap) "Stub."))
(unless (boundp 'magit-hunk-section-map)
  (defvar magit-hunk-section-map (make-sparse-keymap) "Stub."))
(unless (boundp 'magit-status-mode-map)
  (defvar magit-status-mode-map (make-sparse-keymap) "Stub."))
(unless (boundp 'magit-diff-section-base-map)
  (defvar magit-diff-section-base-map (make-sparse-keymap) "Stub."))
(unless (boundp 'magit-diff-section-map)
  (defvar magit-diff-section-map (make-sparse-keymap) "Stub."))

;; eieio stubs — the source uses eieio-oref (the runtime function
;; underlying the `oref' macro) so it can be stubbed in tests.
(unless (fboundp 'eieio-oref)
  (defun eieio-oref (_obj _slot)
    "Stub: return nil."
    nil))

;; persp-mode stubs
(unless (boundp 'persp-mode)
  (defvar persp-mode nil "Stub."))
(unless (boundp 'persp-names-cache)
  (defvar persp-names-cache nil "Stub: simulated persp-mode names cache."))
;; Doom workspace existence helper.  Mirrors Doom's
;; `+workspace-exists-p' (which checks `persp-names-cache' membership)
;; so callers under test can use the production existence guard
;; without a separate fboundp escape hatch firing first.
(unless (fboundp '+workspace-exists-p)
  (defun +workspace-exists-p (name)
    "Stub: simulate Doom's `+workspace-exists-p' using `persp-names-cache'."
    (and (member name persp-names-cache) t)))
(unless (boundp 'persp-nil-name)
  (defvar persp-nil-name "main" "Stub: simulated persp-mode nil-persp name."))
(unless (fboundp 'persp-persps)
  (defun persp-persps () "Stub." nil))
(unless (fboundp 'persp-add-new)
  (defun persp-add-new (_name) "Stub." nil))
(unless (fboundp 'persp-frame-switch)
  (defun persp-frame-switch (_name) "Stub." nil))
(unless (fboundp 'projectile-add-known-project)
  (defun projectile-add-known-project (_dir) "Stub." nil))
(unless (fboundp 'doom-fallback-buffer)
  (defun doom-fallback-buffer () "Stub." (get-buffer-create " *test-fallback*")))
(unless (fboundp 'hack-dir-local-variables-non-file-buffer)
  (defun hack-dir-local-variables-non-file-buffer () "Stub." nil))
(unless (boundp '+workspaces-switch-project-function)
  (defvar +workspaces-switch-project-function nil "Stub."))
(unless (fboundp 'persp-contain-buffer-p)
  (defun persp-contain-buffer-p (_buf _persp) "Stub." nil))
(unless (fboundp 'persp-remove-buffer)
  (defun persp-remove-buffer (_buf &optional _persp) "Stub." nil))
(unless (fboundp 'safe-persp-name)
  (defun safe-persp-name (persp) "Stub." persp))

;; Doom dashboard stubs
(unless (boundp '+doom-dashboard-buffer-name)
  (defvar +doom-dashboard-buffer-name "*doom*" "Stub."))

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

;; Make `claude-repl--workspace-merge-async' synchronous in tests.  In
;; production the wrapper closes the workspace UI, spawns a worker thread
;; that runs `--dispatch-merge-handler', and posts a reopen on failure.
;; For most tests we want the dispatch to run inline so the test can
;; assert on the eventual cherry-pick/merge state directly.  Tests that
;; specifically verify the close-then-spawn-then-reopen lifecycle bypass
;; this stub via:
;;
;;   (cl-letf (((symbol-function 'claude-repl--workspace-merge-async)
;;              claude-repl-test--orig-workspace-merge-async))
;;     ...)
(defvar claude-repl-test--orig-workspace-merge-async
  (symbol-function 'claude-repl--workspace-merge-async)
  "Real `claude-repl--workspace-merge-async' captured before the fixture's
sync-stub advice.  Tests that need to exercise the actual async wrapper
behavior can rebind via `cl-letf'.")
(advice-add 'claude-repl--workspace-merge-async :override
            (lambda (ws repo-root)
              (claude-repl--dispatch-merge-handler ws repo-root)))

;; Make `claude-repl--defer-to-main-thread' synchronous in tests.  In
;; production the helper schedules its thunk via `run-at-time' so the work
;; lands on the main thread even when called from the worker thread spawned
;; by `claude-repl--workspace-merge-async'.  For tests, the deferral just
;; hides UI ops behind a timer that never fires (tests don't drain the
;; timer queue), so override to run THUNK immediately and let assertions
;; observe the resulting state.  Individual tests that specifically need to
;; verify deferral semantics (rather than the deferred work itself) can
;; rebind via `cl-letf' to capture the thunk without invoking it.
(advice-add 'claude-repl--defer-to-main-thread :override
            (lambda (thunk) (funcall thunk)))

;; Disable file-logging during tests so the unconditional file-write path
;; (always-on after the core.el log refactor) does not append every
;; test-emitted line to the user's real `~/.claude/emacs/doom-claude-repl.log'.
;; Tests that specifically exercise the file-write path bind this back
;; locally and redirect `claude-repl-log-file-name' to a temp path.
(when (boundp 'claude-repl-log-to-file)
  (setq claude-repl-log-to-file nil))

;; Redirect the events log to a throwaway temp path so any test that
;; exercises a code path which records workspace lifecycle events does
;; not clobber the user's real `~/.claude/emacs/events.el'.  Per-test
;; isolation still uses the `--with-clean-state' rebinding below.
(when (boundp 'claude-repl-events-file)
  (setq claude-repl-events-file
        (expand-file-name (format "claude-events-test-%d.el" (emacs-pid))
                          temporary-file-directory)))

;;;; ---- Test utilities ----

(defmacro claude-repl-test--with-clean-state (&rest body)
  "Execute BODY with fresh claude-repl global state.
Also redirects `claude-repl-workspace-snapshot-file' to a throwaway
temp path so the state-save snapshot piggyback can't clobber the
user's real snapshot during ERT runs."
  (declare (indent 0))
  `(let ((claude-repl--workspaces (make-hash-table :test 'equal))
         (claude-repl--snapshot-load-state nil)
         (claude-repl-after-ready-functions nil)
         (claude-repl--fullscreen-config nil)
         (claude-repl--sync-timer nil)
         (claude-repl--hide-overlay-refcount 0)
         (claude-repl-debug nil)
         (claude-repl-workspace-snapshot-file
          (expand-file-name (format "claude-snap-%s" (random)) temporary-file-directory))
         (claude-repl--snapshot-archived-this-run nil)
         (claude-repl--restored-workspaces nil)
         (claude-repl-events-file
          (expand-file-name (format "claude-events-%s.el" (random)) temporary-file-directory))
         (claude-repl--events-cache nil)
         (claude-repl--events-cache-loaded t))
     (unwind-protect
         (progn ,@body)
       (when (file-exists-p claude-repl-workspace-snapshot-file)
         (delete-file claude-repl-workspace-snapshot-file))
       (when (file-exists-p claude-repl-events-file)
         (delete-file claude-repl-events-file))
       (let ((archive-dir (claude-repl--workspace-snapshot-archive-dir)))
         (when (file-directory-p archive-dir)
           (delete-directory archive-dir t))))))

(defun claude-repl-test--seed-file (path content)
  "Write CONTENT (string) to PATH, creating any needed parent dirs.
Used by tests that need to seed a fixture file at a path whose parent
directory doesn't exist yet (e.g. the relocated `<root>/.claude/emacs/'
data dir).  Avoids `with-temp-file's bare write that would otherwise
fail with `file-missing'."
  (let ((dir (file-name-directory path)))
    (when (and dir (not (file-directory-p dir)))
      (make-directory dir t)))
  (with-temp-file path (insert content)))

(defmacro claude-repl-test--with-temp-buffer (name &rest body)
  "Create (or reuse) buffer NAME, execute BODY, kill buffer only if we created it.
If NAME already refers to a live buffer when the macro runs (e.g. `*scratch*',
which the test runner itself lives in during aggregate runs), the macro must
NOT kill it on exit — killing the pre-existing buffer swaps ert out of its
current buffer and leaves `default-directory' pointing somewhere arbitrary
(often the Emacs binary's directory on macOS), which breaks any later test
that shells out to git from the cwd."
  (declare (indent 1))
  (let ((buf-sym (make-symbol "buf"))
        (pre-sym (make-symbol "pre-existed")))
    `(let* ((,pre-sym (get-buffer ,name))
            (,buf-sym (or ,pre-sym (get-buffer-create ,name))))
       (unwind-protect
           (with-current-buffer ,buf-sym
             ,@body)
         (unless ,pre-sym
           (when (buffer-live-p ,buf-sym)
             (kill-buffer ,buf-sym)))))))

(provide 'test-helpers)

;;; test-helpers.el ends here
