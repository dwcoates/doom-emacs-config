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

;; magit variable stubs — `after!' is a no-op shim in tests, so the
;; `(after! magit ...)' body in magit.el executes and would reference
;; these at load time without them.
(unless (boundp 'magit-no-confirm)
  (defvar magit-no-confirm nil "Stub."))
(unless (boundp 'magit-diff-visit-previous-blob)
  (defvar magit-diff-visit-previous-blob nil "Stub."))
(unless (boundp 'magit-section-initial-visibility-alist)
  (defvar magit-section-initial-visibility-alist nil "Stub."))

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
(unless (fboundp 'persp-persps)
  (defun persp-persps () "Stub." nil))
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

;;;; ---- Test utilities ----

(defmacro claude-repl-test--with-clean-state (&rest body)
  "Execute BODY with fresh claude-repl global state."
  (declare (indent 0))
  `(let ((claude-repl--workspaces (make-hash-table :test 'equal))
         (claude-repl--fullscreen-config nil)
         (claude-repl--sync-timer nil)
         (claude-repl--cursor-reset-timer nil)
         (claude-repl--hide-overlay-refcount 0)
         (claude-repl-debug nil))
     ,@body))

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
