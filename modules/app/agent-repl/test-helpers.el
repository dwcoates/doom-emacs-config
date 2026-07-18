;;; test-helpers.el --- Shared test infrastructure for agent-repl -*- lexical-binding: t; -*-

;;; Commentary:

;; Shared stub layer and test utilities for all agent-repl test files.
;; Each per-module test file should load this before defining tests:
;;
;;   (load (expand-file-name "test-helpers.el" (file-name-directory
;;                                               (or load-file-name buffer-file-name)))
;;         nil t)

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)

;;;; ---- Batch-only contract ----
;;
;; This file is BATCH-ONLY scaffolding (`emacs -batch -Q -l ert ...').
;; Loaded into a live interactive Emacs — e.g. by a blanket hot-reload
;; of touched .el files — its load-time side effects would poison the
;; running session: every external-boundary wrapper replaced with an
;; error guard, `AGENT_REPL_STATE_DIR' pointed at a throwaway temp dir,
;; file logging disabled, the whole module reloaded under a stubbed
;; `file-notify-add-watch' (killing live watchers), and async merge
;; dispatch forced synchronous.  That exact incident occurred: the web
;; frontend died with "daemon never became ready: EXTERNAL BOUNDARY
;; UNMOCKED" after test-helpers.el was hot-loaded into the main Emacs.
;;
;; Every such side effect below is therefore gated on `noninteractive'.
;; In an interactive session this file loads as an inert no-op (the
;; fboundp/boundp-guarded stubs aside) and announces itself instead.
(unless noninteractive
  (display-warning
   'agent-repl-test
   (concat "test-helpers.el loaded in an INTERACTIVE session — "
           "batch-only test scaffolding skipped (boundary guards, "
           "state-dir redirect, module reload, log/merge/defer "
           "overrides).  Run the suite via `emacs -batch' instead.")
   :warning))

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
`agent-repl--workspace-message-body-advice' assert that the advice
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

;; No vterm stubs: agent-repl has no vterm frontend, so no production code
;; path reaches a `vterm-*' symbol.  A test that still calls one is testing
;; a frontend that no longer exists, and the resulting void-function is the
;; correct, loud answer.

;; general-override-mode-map stub — keybindings.el installs override chords
;; into this keymap at load time, so the variable must exist before that
;; load runs.
(unless (boundp 'general-override-mode-map)
  (defvar general-override-mode-map (make-sparse-keymap) "Stub."))

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

;; filenotify stub (prevent side effects at load time).  Batch-gated:
;; stubbing `file-notify-add-watch' in a live session would silently
;; kill every watcher registered while the stub is active.
(require 'filenotify)
(when noninteractive
  (unless (fboundp 'file-notify-add-watch--orig)
    ;; Save original and replace with no-op during test loading
    (defalias 'file-notify-add-watch--orig #'file-notify-add-watch)
    (defun file-notify-add-watch--test-stub (_dir _flags _callback)
      "Stub: no-op for tests."
      nil)
    (advice-add 'file-notify-add-watch :override #'file-notify-add-watch--test-stub)))

;; `string-search' is an Emacs 28.1 built-in; the batch Emacs used to run
;; the suite may be older (e.g. 27.1), where several test-status.el tabline
;; assertions call it and would otherwise abort with `void-function
;; string-search'.  Polyfill it with the same literal, case-sensitive
;; semantics (index of the first NEEDLE occurrence in HAYSTACK at or after
;; START, or nil) so the assertions actually run.  Guarded by `fboundp' so
;; the native builtin always wins on Emacs 28+.
(require 'cl-lib)
(unless (fboundp 'string-search)
  (defun string-search (needle haystack &optional start)
    "Polyfill for Emacs 28+ `string-search'.
Return the index of the first occurrence of NEEDLE in HAYSTACK, searching
from START (default 0), or nil when NEEDLE does not occur."
    (cl-search needle haystack :start2 (or start 0))))

;; In batch mode, Emacs uses a tiny terminal frame (typically 9 rows x 10 cols).
;; Tests that create real windows (split-window, display-buffer-in-side-window)
;; fail when window-min-height / window-min-width enforce larger minimum sizes
;; than the tiny frame allows.  Lowering both to 1 lets the frame be split into
;; as many sub-windows as needed for layout-level tests without changing any
;; behavioural logic (the minimums only gate whether a split is geometrically
;; possible).
(when noninteractive
  (setq-default window-min-height 1)
  (setq-default window-min-width  1))

;; Stub notification backend so notifications.el loads without error in
;; environments lacking terminal-notifier / osascript.  `defvar' only
;; initialises the variable when it is void; pre-binding it here means the
;; `(defvar agent-repl--notification-backend (agent-repl--select-notification-backend))'
;; form in notifications.el skips the init-form evaluation entirely, which
;; is the call that would otherwise signal the FATAL load error.
(unless (boundp 'agent-repl--notification-backend)
  (defvar agent-repl--notification-backend (lambda (_ws _title _msg) nil)
    "Stub: no-op notification backend for test environments."))

;; Isolate agent-repl's canonical state dir to a throwaway temp location
;; for the ENTIRE test session, BEFORE the module loads.  `core.el'
;; resolves `agent-repl--global-state-dir' (and the log path default it
;; bakes in at load) from the `AGENT_REPL_STATE_DIR' override, falling
;; back to `~/.claude-emacs'; pointing that env var at a temp dir here
;; ensures module load-time logging and any state writes never touch the
;; developer's real `~/.claude-emacs' tree — which, if created by a test
;; run, would otherwise block the one-time legacy migration on the next
;; interactive reload.  Individual tests that assert specific state-dir
;; paths rebind `process-environment' locally and are unaffected.
;; Batch-gated: in a live session this redirect would send every state
;; write (workspace snapshot, logs, status export) to the temp dir, and
;; the module reload below would re-bake load-time path constants from
;; it — both halves of the observed live-session poisoning.
(when noninteractive
  (setenv "AGENT_REPL_STATE_DIR"
          (expand-file-name (format "agent-repl-test-state-%d" (emacs-pid))
                            temporary-file-directory))

  ;; Suppress timers at load time.  Both the periodic (`run-with-timer')
  ;; and idle (`run-with-idle-timer') registrations that fire at module
  ;; load are overridden so no real timer leaks into the batch test
  ;; process.
  (defvar agent-repl-test--orig-run-with-timer (symbol-function 'run-with-timer))
  (defvar agent-repl-test--orig-run-with-idle-timer (symbol-function 'run-with-idle-timer))
  (advice-add 'run-with-timer :override (lambda (&rest _) nil))
  (advice-add 'run-with-idle-timer :override (lambda (&rest _) nil))

  ;; Load the module
  (load (expand-file-name "config.el" (file-name-directory
                                        (or load-file-name buffer-file-name)))
        nil t)

  ;; Restore run-with-timer / run-with-idle-timer after loading
  (advice-remove 'run-with-timer (lambda (&rest _) nil))
  (advice-remove 'run-with-idle-timer (lambda (&rest _) nil))

  ;; Restore file-notify-add-watch after loading
  (advice-remove 'file-notify-add-watch #'file-notify-add-watch--test-stub))

;; Make `agent-repl--workspace-merge-async' synchronous in tests.  In
;; production the wrapper closes the workspace UI, spawns a worker thread
;; that runs `--dispatch-merge-handler', and posts a reopen on failure.
;; For most tests we want the dispatch to run inline so the test can
;; assert on the eventual cherry-pick/merge state directly.  Tests that
;; specifically verify the close-then-spawn-then-reopen lifecycle bypass
;; this stub via:
;;
;;   (cl-letf (((symbol-function 'agent-repl--workspace-merge-async)
;;              agent-repl-test--orig-workspace-merge-async))
;;     ...)
;; Batch-gated: this advice persists across module reloads, so in a
;; live session it would permanently force merges synchronous.
(when noninteractive
  (defvar agent-repl-test--orig-workspace-merge-async
    (symbol-function 'agent-repl--workspace-merge-async)
    "Real `agent-repl--workspace-merge-async' captured before the fixture's
sync-stub advice.  Tests that need to exercise the actual async wrapper
behavior can rebind via `cl-letf'.")
  (advice-add 'agent-repl--workspace-merge-async :override
              (lambda (ws repo-root &optional onto-master)
                (agent-repl--dispatch-merge-handler ws repo-root onto-master))))

;; Make `agent-repl--defer-to-main-thread' synchronous in tests.  In
;; production the helper schedules its thunk via `run-at-time' so the work
;; lands on the main thread even when called from the worker thread spawned
;; by `agent-repl--workspace-merge-async'.  For tests, the deferral just
;; hides UI ops behind a timer that never fires (tests don't drain the
;; timer queue), so override to run THUNK immediately and let assertions
;; observe the resulting state.  Individual tests that specifically need to
;; verify deferral semantics (rather than the deferred work itself) can
;; rebind via `cl-letf' to capture the thunk without invoking it.
;; Batch-gated for the same reload-surviving-advice reason as the
;; merge-async override above.
(when noninteractive
  (advice-add 'agent-repl--defer-to-main-thread :override
              (lambda (thunk) (funcall thunk))))

;; Disable file-logging during tests so the unconditional file-write path
;; (always-on after the core.el log refactor) does not append every
;; test-emitted line to the user's real `~/.claude-emacs/doom-agent-repl.log'.
;; Tests that specifically exercise the file-write path bind this back
;; locally and redirect `agent-repl-log-file-name' to a temp path.
(when (and noninteractive (boundp 'agent-repl-log-to-file))
  (setq agent-repl-log-to-file nil))

;;;; ---- External-boundary guards ----
;;
;; The production module (loaded above) defines every external-process
;; wrapper in `agent-repl--external-boundary-functions'.  Replace each
;; with a guard that ERRORS if called during a test run — tests must
;; `cl-letf' over the wrapper before exercising any production path
;; that reaches it.  Without this, a test could silently shell out to
;; real `git'/`gh'/etc. and either pollute external state (the
;; original `branch-a` etc. incident) or pass for the wrong reason.
;;
;; This is the runtime half of the policy in AGENTS.md "No External
;; Processes or External State in Tests".  The static half is
;; `.claude/check-external-boundaries.sh' (pre-commit-installed lint).
;;
;; Idempotent — only installs once per Emacs session.
(defvar agent-repl-test--external-guards-installed nil
  "Non-nil after `agent-repl-test--install-external-guards' has fired.")

(defvar agent-repl-test--external-original-functions nil
  "Alist of (SYMBOL . ORIGINAL-FUNCTION) captured before guard install.
Read by `agent-repl-test--verify-external-guards-installed' to assert
each registered wrapper was actually reassigned away from its real
implementation, and by any test that genuinely needs the real wrapper
back (rare; usually a smell — see AGENTS.md).")

(defvar agent-repl-test--external-guard-functions nil
  "Alist of (SYMBOL . GUARD-FUNCTION) as installed over each registry entry.
Read by `agent-repl-test--reinstall-external-guards' to re-`fset' the
SAME guard a production-file re-load overwrote, without re-capturing
`agent-repl-test--external-original-functions' (whose entries must stay
the REAL implementations, not a previously-installed guard).")

(defun agent-repl-test--make-external-guard (fn-name)
  "Return the unmocked-call guard function installed over FN-NAME."
  (lambda (&rest args)
    ;; Fail-open OUTSIDE batch: a guard reaching a live interactive
    ;; session is a harness leak (install refuses outside batch, so
    ;; this should be impossible), and erroring there breaks the
    ;; user's editor.  Warn loudly and delegate to the captured
    ;; original so the session keeps working.  Inside batch the guard
    ;; errors exactly as before.
    (if (not noninteractive)
        (let ((orig (cdr (assq fn-name agent-repl-test--external-original-functions))))
          (unless orig
            (error "agent-repl test guard: `%s' invoked in an interactive session with no captured original to delegate to" fn-name))
          (display-warning
           'agent-repl-test
           (format "external-boundary guard for `%s' invoked in an interactive session — delegating to the real implementation (harness leak; report this)" fn-name)
           :warning)
          (apply orig args))
      (error
       (concat
        "EXTERNAL BOUNDARY UNMOCKED: `%s' called with %S during a test run.\n"
        "Per AGENTS.md \"No External Processes or External State in Tests\",\n"
        "every external-boundary wrapper MUST be stubbed via `cl-letf'\n"
        "before the production code under test reaches it.\n"
        "\n"
        "REQUIRED REMEDIATION (one of these two; no other option exists):\n"
        "  (a) Add the stub: `((symbol-function '%s)\n"
        "                      (lambda (&rest _args) <fixture>))`\n"
        "      to the failing test's `cl-letf' bindings.\n"
        "  (b) Delete the test if its only purpose is to exercise the\n"
        "      external boundary itself (per AGENTS.md \"We test lisp,\n"
        "      not external code\" — such tests do not belong in ERT).\n"
        "\n"
        "BYPASSING IS FORBIDDEN.  Do NOT route around this guard via\n"
        "`--no-verify', `ignore-errors', `condition-case' that swallows\n"
        "the signal, restoring the original `symbol-function' inside the\n"
        "test, advice that no-ops the guard, or any other technique.\n"
        "If you find yourself reasoning \"just this once\", stop and\n"
        "apply remediation (a) or (b) instead.")
       fn-name args fn-name))))

(defun agent-repl-test--install-external-guards ()
  "Replace every symbol in `agent-repl--external-boundary-functions'
with a guard that errors if invoked.  Captures the original function
of each symbol into `agent-repl-test--external-original-functions'
so the install can be verified after the fact by
`agent-repl-test--verify-external-guards-installed' (proves the
`fset' actually took for every registry entry), and the installed guard
into `agent-repl-test--external-guard-functions' so a later re-load of a
production file can be re-armed by
`agent-repl-test--reinstall-external-guards'.

Refuses to run outside batch: the `fset's would replace the LIVE
session's external wrappers, breaking every path that shells out or
talks HTTP (the \"daemon never became ready\" incident)."
  (unless noninteractive
    (error (concat "agent-repl-test--install-external-guards: refusing in an "
                   "interactive session — the guards would clobber the live "
                   "session's external-boundary wrappers")))
  (unless agent-repl-test--external-guards-installed
    (when (boundp 'agent-repl--external-boundary-functions)
      (dolist (sym agent-repl--external-boundary-functions)
        ;; Capture the real impl FIRST so we can verify replacement
        ;; afterwards (and so a rare test that needs the real impl
        ;; back has a documented escape hatch).
        (push (cons sym (and (fboundp sym) (symbol-function sym)))
              agent-repl-test--external-original-functions)
        (let ((guard (agent-repl-test--make-external-guard sym)))
          (push (cons sym guard) agent-repl-test--external-guard-functions)
          (fset sym guard))))
    (setq agent-repl-test--external-guards-installed t)))

(defun agent-repl-test--reinstall-external-guards ()
  "Re-arm the external-boundary guards after a production file was re-loaded.
`agent-repl-test--install-external-guards' installs once per Emacs
session, so ANY later re-load of a production file silently DISARMS the
guards for every wrapper that file `defun's: the re-`defun' overwrites
the guard with the real implementation, and the install will not fire
again.  A test file that re-loads a production file (e.g. `config.el',
to exercise a load-time code path) MUST call this afterwards — otherwise
every test loaded after it in the aggregate run executes UNGUARDED and
can silently reach the real `git' / `gh' / daemon.

Re-`fset's the guard recorded in `agent-repl-test--external-guard-functions'
for every registry entry, then verifies the result so a re-arm that fails
to take is loud rather than silent."
  (unless noninteractive
    (error (concat "agent-repl-test--reinstall-external-guards: refusing in an "
                   "interactive session — the guards would clobber the live "
                   "session's external-boundary wrappers")))
  (unless agent-repl-test--external-guards-installed
    (error (concat "agent-repl-test--reinstall-external-guards: guards were never "
                   "installed — nothing to re-arm (install runs at test-helpers load)")))
  (dolist (cell agent-repl-test--external-guard-functions)
    (fset (car cell) (cdr cell)))
  (agent-repl-test--verify-external-guards-installed))

(defun agent-repl-test--verify-external-guards-installed ()
  "Sanity-check that every symbol in `agent-repl--external-boundary-functions'
has actually been reassigned away from its captured original.
Signals `error' loudly listing every symbol whose `symbol-function'
still matches the captured pre-guard implementation — that condition
means the install loop missed an entry (registry/install bug), and
running tests under that state would silently let production code
shell out to the real binary."
  (let ((missed nil))
    (when (boundp 'agent-repl--external-boundary-functions)
      (dolist (sym agent-repl--external-boundary-functions)
        (let* ((cell (assq sym agent-repl-test--external-original-functions))
               (orig (cdr cell))
               (current (and (fboundp sym) (symbol-function sym))))
          (cond
           ((null cell)
            ;; Symbol was registered but never seen by the install loop.
            (push (cons sym 'never-captured) missed))
           ((eq orig current)
            ;; Symbol still bound to its real implementation — guard `fset` did not take.
            (push (cons sym 'guard-not-installed) missed))))))
    (when missed
      (error
       (concat
        "External-boundary guards INVARIANT VIOLATED.  The test harness "
        "tried to replace every wrapper in `agent-repl--external-boundary-functions' "
        "with an unmocked-call guard, but the following entries are still bound "
        "to their original implementation (or were skipped entirely): %S.  "
        "Refusing to run tests in this state because production code would "
        "silently shell out to real external binaries.")
       missed))))

;; Batch-gated: installing the guards is the single most destructive
;; side effect this file has when leaked into a live session.
;;
;; RE-ARM on every load, not just the first.  Every test file `load's this
;; file, and this file's body re-`load's `config.el' (and with it every
;; production module) — which re-`defun's the external-boundary wrappers and
;; overwrites the guards.  Since the install is once-per-session, the first
;; test file used to be the ONLY guarded one: from the second file onward the
;; whole aggregate ran UNGUARDED, free to shell out to the real `git' / `gh'
;; or HTTP the developer's live daemon (which is exactly what
;; `agent-repl-test-daemon-stop-deletes-and-clears' started doing).
(when noninteractive
  (if agent-repl-test--external-guards-installed
      (agent-repl-test--reinstall-external-guards)
    (agent-repl-test--install-external-guards))
  ;; Eager verification: if the install missed anything, abort BEFORE any
  ;; test gets a chance to silently shell out.  Failure here means a
  ;; bug in the install loop, the registry, or someone clobbered the
  ;; guards between install and now.
  (agent-repl-test--verify-external-guards-installed))

;;;; ---- Test utilities ----

(defun agent-repl-test--fake-webview-factory (log-sym)
  "Return a boundary mock for `agent-repl--frontend-make-webview-buffer'.
Records each mounted URL onto the (special) variable LOG-SYM and hands
back an ordinary buffer — batch Emacs has no xwidget support, so no
webview can actually be created.  The buffer carries the \"WebKit: \"
header-line that `xwidget-webkit-mode' installs, so any mount path's
clearing of it is observable.

Shared by every webview consumer's tests (the workspace frontend and
the explain-config popup mount the same wrapper), so the two cannot
drift apart in what they pretend a webview is."
  (lambda (url)
    (push url (symbol-value log-sym))
    (let ((buf (generate-new-buffer "*fake-webview*")))
      (with-current-buffer buf
        (setq-local header-line-format (list "WebKit: " "claude-repl")))
      buf)))

(defmacro agent-repl-test--with-clean-state (&rest body)
  "Execute BODY with fresh agent-repl global state.
Also redirects `agent-repl-workspace-snapshot-file' to a throwaway
temp path so the state-save snapshot piggyback can't clobber the
user's real snapshot during ERT runs.

`agent-repl-default-frontend' is scratch-bound here, at the choke point
every test already passes through, because the selection commands ADOPT
their choice as the default new workspaces are born with
\(`agent-repl--frontend-adopt-default') — a plain `setq' on a global.
A BODY that drives `agent-repl-select-frontend' /
`agent-repl-switch-frontend' therefore leaks its choice into every later
test in load order, and every workspace carrying no `:frontend' of its
own resolves through that global.  Binding it per-test closes the whole
class: leaking a scratch frontend name makes later tests ERROR
\(unregistered), and leaking a real one (`gui') is worse — it silently
re-routes their frontend resolution instead."
  (declare (indent 0))
  `(let ((agent-repl--workspaces (make-hash-table :test 'equal))
         ;; Repo-fold set: global UI state, so a test that folds a repo
         ;; would otherwise leak that fold into every later test's
         ;; tab-bar render.
         (agent-repl--folded-repos (make-hash-table :test 'equal))
         (agent-repl--snapshot-load-state nil)
         (agent-repl-after-ready-functions nil)
         (agent-repl--fullscreen-config nil)
         (agent-repl--sync-timer nil)
         (agent-repl--hide-overlay-refcount 0)
         (agent-repl-debug nil)
         (agent-repl-default-frontend agent-repl-default-frontend)
         (agent-repl-workspace-snapshot-file
          (expand-file-name (format "agent-snap-%s" (random)) temporary-file-directory))
         (agent-repl--snapshot-archived-this-run nil)
         (agent-repl--restored-workspaces nil)
         ;; Reset workspace-state update timer state so each test starts
         ;; from a clean slate: counter at 0, no chain in flight, async
         ;; spread disabled (tests want synchronous iteration so they
         ;; can read state immediately after the call).
         (agent-repl--update-tick-counter 0)
         (agent-repl--update-in-flight nil)
         (agent-repl--update-spread-sync t))
     (unwind-protect
         (progn ,@body)
       (when (file-exists-p agent-repl-workspace-snapshot-file)
         (delete-file agent-repl-workspace-snapshot-file))
       (let ((archive-dir (agent-repl--workspace-snapshot-archive-dir)))
         (when (file-directory-p archive-dir)
           (delete-directory archive-dir t))))))

(defmacro agent-repl-test--with-merge-state (&rest body)
  "Execute BODY with fresh merge queue / in-flight / progress / lookahead state.
Used by the worktree tests, which exercise the cherry-pick progress
filter against exactly these globals."
  (declare (indent 0))
  `(let ((agent-repl--merge-queue nil)
         (agent-repl--in-flight-merges nil)
         (agent-repl--merge-progress (make-hash-table :test 'equal))
         (agent-repl--merge-progress-seq 0)
         (agent-repl--merge-lookahead (make-hash-table :test 'equal)))
     ,@body))

(defmacro agent-repl-test--with-mocked-git-probes (&rest body)
  "Execute BODY with the cherry-pick probe's git wrappers stubbed.

`agent-repl--cherry-pick-in-progress-p' probes a target worktree for
`CHERRY_PICK_HEAD' via two external-boundary wrappers
\(`agent-repl--git-string' for `rev-parse --absolute-git-dir',
`agent-repl--git-string-quiet' for other rev-parse variants), and the
merge-queue drain additionally reads HEAD SHAs via
`agent-repl--current-head-sha' (also `agent-repl--git-string').  Any
test whose subject reaches one of these — including every
`--drain-merge-queue' test, every `--workspace-merge-into-source' /
`--workspace-merge-current-into-source' test, and any future call site
that probes a worktree's git dir — must mock both wrappers or trip the
runtime boundary guard.

This macro stubs both wrappers to return the empty string, which
causes the probe's downstream `expand-file-name CHERRY_PICK_HEAD' +
`file-exists-p' to resolve nil, so the probe reports `no cherry-pick
in flight' deterministically without shelling out.  Nest an inner
`cl-letf' inside BODY for any test-specific stubs.

Extracted to consolidate the (formerly duplicated) 4-line `cl-letf'
block that previously appeared verbatim at every site needing this
mock pair."
  (declare (indent 0) (debug t))
  `(cl-letf (((symbol-function 'agent-repl--git-string)
              (lambda (&rest _args) ""))
             ((symbol-function 'agent-repl--git-string-quiet)
              (lambda (&rest _args) "")))
     ,@body))

(defun agent-repl-test--seed-file (path content)
  "Write CONTENT (string) to PATH, creating any needed parent dirs.
Used by tests that need to seed a fixture file at a path whose parent
directory doesn't exist yet (e.g. the relocated `<root>/.claude/emacs/'
data dir).  Avoids `with-temp-file's bare write that would otherwise
fail with `file-missing'."
  (let ((dir (file-name-directory path)))
    (when (and dir (not (file-directory-p dir)))
      (make-directory dir t)))
  (with-temp-file path (insert content)))

(defmacro agent-repl-test--with-temp-buffer (name &rest body)
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

;;;; ---- Visible-in-report sanity test for the registry+guard scheme ----
;;
;; The eager call to `agent-repl-test--verify-external-guards-installed'
;; above already aborts test-helpers load if the invariant is violated.
;; This `ert-deftest' duplicates that check inside the ert harness so
;; the assertion shows up in the test report as a discrete green/red
;; line — operators reading a CI failure log see immediately whether
;; the guard machinery is intact.
;;
;; Naming: leading `AAA-' makes the test sort BEFORE every other
;; `agent-repl-test-*' under default ert alphabetical ordering (`A' < `a'
;; in ASCII), so a broken install fails the very first line of the report.

;; Guard registration: the aggregator loads test-helpers.el once per
;; test file; modern ert errors on `ert-deftest' re-definition.  Set a
;; flag the first time we run and skip the definition on subsequent
;; loads.
(defvar agent-repl-test--AAA-test-registered nil
  "Non-nil once the AAA sanity ert-deftest below has been registered.")

;; Batch-gated alongside the install itself: registering this test in a
;; live session would only plant a false alarm in a later `M-x ert' run
;; (the guards are — correctly — not installed there).
(when (and noninteractive (not agent-repl-test--AAA-test-registered))
  (setq agent-repl-test--AAA-test-registered t)
  (ert-deftest agent-repl-test-AAA-external-guards-installed-globally ()
    "Every symbol in `agent-repl--external-boundary-functions' must be
reassigned to the unmocked-call guard before any test runs.  Fails
the report explicitly if the install loop missed an entry."
    (should agent-repl-test--external-guards-installed)
    (should agent-repl--external-boundary-functions)
    (dolist (sym agent-repl--external-boundary-functions)
      (let* ((cell (assq sym agent-repl-test--external-original-functions))
             (orig (cdr cell))
             (current (and (fboundp sym) (symbol-function sym))))
        ;; A pre-install original was captured for this symbol.
        (should cell)
        ;; The current binding is NOT the captured original — i.e. the
        ;; guard actually took.
        (should-not (eq orig current))))))

(provide 'test-helpers)

;;; test-helpers.el ends here
