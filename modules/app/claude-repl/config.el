;;; config.el --- claude repl for doom emacs -*- lexical-binding: t; -*-

;; Author: Dodge Coates
;; URL: https://github.com/dodgecoates
;; Version: 0.1.0

;;; Commentary:
;; Main loader for the claude-repl module. Sub-files are loaded in
;; dependency order; Elisp's call-time function resolution means
;; forward references between defuns are safe.

;;; Code:

(message "[claude-repl] Loading Claude-Repl package...")

(defvar claude-repl--config-file
  (or load-file-name buffer-file-name)
  "Absolute path to this config.el, captured at load time for reloading.")

(defvar claude-repl--load-errors nil
  "List of (FILE . ERROR) pairs for sub-files that failed to load.")

;; Reset the accumulator on every load — `defvar' only initializes once,
;; so without this `M-x doom/reload' would re-report stale errors from a
;; prior failed load even after the next load succeeded, masking actual
;; status.
(setq claude-repl--load-errors nil)

(defmacro claude-repl--load-module (file)
  "Load FILE via `load!', recording any error for collective reporting."
  `(condition-case err
       (progn
         (load! ,file)
         (message "[claude-repl] %s.el loaded." ,file))
     (error
      (push (cons ,file err) claude-repl--load-errors)
      (message "[claude-repl] FAILED to load %s.el: %S" ,file err))))

;; ---- Early orphan cherry-pick recovery ----
;;
;; When Emacs is hard-killed mid-cherry-pick (the synchronous headless
;; `claude -p' auto-resolve is the canonical blocker — the worker thread
;; blocks via `claude-repl--wait-for-process-exit' and can't run its
;; `condition-case' cleanup if the whole process dies), the in-flight
;; cherry-pick is left orphaned: CHERRY_PICK_HEAD lingers in the target
;; worktree's git dir and conflict markers may remain in working-tree
;; files.  When the target IS this Emacs's master worktree and the
;; marker-bearing files are `.el' under `modules/app/claude-repl/', the
;; next Emacs start can't load the claude-repl module at all because
;; the elisp reader rejects `<<<<<<<' markers.
;;
;; This block runs BEFORE any module file is `require'd — its only
;; dependencies are built-in Elisp (`read', `call-process', `with-temp-file')
;; and the persisted workspace snapshot at `~/.claude/emacs/workspaces.el'.
;; That keeps it loadable even when every other claude-repl module file
;; has conflict markers.  Each in-flight entry persisted by
;; `claude-repl--push-in-flight-merge' is processed:
;;
;;   1. If CHERRY_PICK_HEAD is still present at the recorded `:target-dir',
;;      run `git -C target-dir cherry-pick --abort' to clear conflict
;;      markers from working-tree files.  The source workspace is moved
;;      onto `:merge-queue' (BACK, no halt) so the normal drain retries
;;      it once the rest of the config has loaded.
;;   2. If CHERRY_PICK_HEAD is absent: the prior merge actually completed
;;      between the push and the crash — nothing to abort, just clear
;;      the bookkeeping.
;;
;; The snapshot file is rewritten in place: `:in-flight-merges' becomes
;; nil and recovered entries are appended to `:merge-queue'.  All errors
;; are caught and surfaced via `message' so a broken snapshot or missing
;; `git' binary cannot block Emacs startup.

(defun claude-repl--early-git-string (&rest args)
  "Run `git ARGS' synchronously and return its trimmed stdout.
Returns the empty string on non-zero exit — early recovery callers
need a tolerant probe, not a hard fail, mirroring the
`claude-repl--git-string-quiet' contract.

This IS the external-boundary wrapper for the early-recovery code
path.  Defined locally because `config.el's early recovery executes
at module-loader top level — BEFORE `core.el' loads and the regular
`claude-repl--git-*' family becomes available.  Same role,
separately defined so the recovery is self-contained.  Registered in
`claude-repl--external-boundary-functions' (core.el) so the
test-time runtime guards see it and tests cannot accidentally shell
out to real `git'."
  (with-temp-buffer
    (let ((exit-code (apply #'call-process "git" nil t nil args))) ;; ALLOW-EXTERNAL-BOUNDARY
      (if (zerop exit-code)
          (string-trim (buffer-string))
        ""))))

(defun claude-repl--early-git-exit-code (&rest args)
  "Run `git ARGS' synchronously and return its exit code (stdout discarded).
Sibling to `claude-repl--early-git-string'; see that function's
docstring for the architectural context.  Registered in
`claude-repl--external-boundary-functions' (core.el)."
  (apply #'call-process "git" nil nil nil args)) ;; ALLOW-EXTERNAL-BOUNDARY

(defun claude-repl--early-cherry-pick-head-at (target-dir)
  "Return the path to CHERRY_PICK_HEAD for TARGET-DIR's repo, or nil.
Resolves the git dir via `git rev-parse --absolute-git-dir' so a linked
worktree (whose `.git' is a file pointing into the parent
`.git/worktrees/<name>') is handled correctly."
  (when (and target-dir (file-directory-p target-dir))
    (let ((git-dir (claude-repl--early-git-string
                    "-C" target-dir "rev-parse" "--absolute-git-dir")))
      (and (not (string-empty-p git-dir))
           (let ((cp-head (expand-file-name "CHERRY_PICK_HEAD" git-dir)))
             (and (file-exists-p cp-head) cp-head))))))

(defun claude-repl--early-abort-cherry-pick (target-dir)
  "Run `git -C TARGET-DIR cherry-pick --abort'; return the exit code."
  (claude-repl--early-git-exit-code
   "-C" target-dir "cherry-pick" "--abort"))

(defun claude-repl--early-recover-orphan-cherry-picks ()
  "Process every in-flight-merge entry in the on-disk workspace snapshot.
See the commentary at the top of `config.el' for the full rationale.
Reads `~/.claude/emacs/workspaces.el', iterates `:in-flight-merges',
aborts each entry whose `:target-dir' still has a `CHERRY_PICK_HEAD',
and rewrites the snapshot with `:in-flight-merges' cleared and the
recovered source workspaces appended to `:merge-queue' for retry."
  (let ((snap-file (expand-file-name "~/.claude/emacs/workspaces.el")))
    (when (file-exists-p snap-file)
      (condition-case err
          (let* ((raw (with-temp-buffer
                        (insert-file-contents snap-file)
                        (goto-char (point-min))
                        (read (current-buffer))))
                 (in-flight (and (consp raw) (keywordp (car raw))
                                 (plist-get raw :in-flight-merges))))
            (when in-flight
              (message "[claude-repl] early-recovery: scanning %d in-flight merge entries"
                       (length in-flight))
              (let ((recovered nil))
                (dolist (entry in-flight)
                  (let* ((source-ws (plist-get entry :source-ws))
                         (target-dir (plist-get entry :target-dir))
                         (recovered-entry (list source-ws target-dir)))
                    ;; Short-circuit malformed entries BEFORE probing
                    ;; the target dir — a botched prior write must not
                    ;; spawn `git' subprocesses against the partial
                    ;; entry's stale path.
                    (cond
                     ((or (null source-ws) (null target-dir))
                      (message "[claude-repl] early-recovery: malformed entry %S — skipping"
                               entry))
                     (t
                      (let ((cp-head (claude-repl--early-cherry-pick-head-at target-dir)))
                        (cond
                         (cp-head
                          (let ((exit-code (claude-repl--early-abort-cherry-pick target-dir)))
                            (message "[claude-repl] early-recovery: ws=%s aborted cherry-pick in %s (exit=%d) — re-enqueueing"
                                     source-ws target-dir exit-code)
                            (push recovered-entry recovered)))
                         (t
                          (message "[claude-repl] early-recovery: ws=%s no orphan CHERRY_PICK_HEAD at %s — clearing bookkeeping"
                                   source-ws target-dir))))))))
                ;; Rewrite the snapshot: clear :in-flight-merges and
                ;; append recovered entries to :merge-queue.
                (let* ((workspaces (plist-get raw :workspaces))
                       (merge-queue (plist-get raw :merge-queue))
                       ;; RECOVERED holds (SOURCE-WS TARGET-DIR) pairs.
                       ;; Carry TARGET-DIR onto the re-enqueued entry as
                       ;; `:target-dir' so the recovered merge rejoins its
                       ;; own per-target+repo sub-queue (the orphan's
                       ;; destination is exactly the in-flight target dir).
                       (new-entries
                        (mapcar (lambda (pair)
                                  (list :source-ws (nth 0 pair)
                                        :silent t
                                        :auto-resolve t
                                        :target-dir (nth 1 pair)
                                        :last-attempt-target-head nil
                                        :halt-until-human nil))
                                (reverse recovered)))
                       (new-queue (append merge-queue new-entries))
                       (new-raw (list :workspaces workspaces
                                      :merge-queue new-queue
                                      :in-flight-merges nil)))
                  (with-temp-file snap-file
                    (let ((print-length nil)
                          (print-level nil))
                      (prin1 new-raw (current-buffer))))
                  (message "[claude-repl] early-recovery: snapshot rewritten — merge-queue=%d in-flight=0 recovered=%d"
                           (length new-queue) (length recovered))))))
        (error
         (message "[claude-repl] early-recovery: failed err=%S" err))))))

(claude-repl--early-recover-orphan-cherry-picks)

;; ---- Loaded-version SHA ----
;;
;; `claude-repl--version' caches the git SHA of the doom config that this
;; module was loaded from.  It is refreshed via `setq' (NOT `defvar') on
;; every load below, so `M-x doom/reload' updates it to the freshly
;; checked-out SHA instead of keeping the value captured at first startup.
;; `claude-repl-version' surfaces it interactively.

(defvar claude-repl--version nil
  "Git SHA of the doom config this claude-repl module was last loaded from.
Refreshed on every load (including `M-x doom/reload') by the
`noninteractive'-gated `setq' below, so it always reflects the version
actually running rather than a stale first-startup value.  nil when the
SHA could not be determined.")

(defun claude-repl--compute-version ()
  "Return the git SHA of the doom repo this module was loaded from, or nil.
Resolves the repo from `claude-repl--config-file's directory so a linked
worktree reports its own checked-out SHA rather than the primary
worktree's.  Returns nil when the config-file path is unknown or git
cannot resolve a SHA (for example outside a repository).

Uses the early-boundary `claude-repl--early-git-string' wrapper so this
helper has no dependency on `core.el' having loaded — it runs at the
config-loader top level alongside the early-recovery code."
  (when claude-repl--config-file
    (let ((sha (claude-repl--early-git-string
                "-C" (file-name-directory claude-repl--config-file)
                "rev-parse" "HEAD")))
      (and (not (string-empty-p sha)) sha))))

;; Refresh on EVERY load so reloads pick up the new SHA.  Gated against
;; `noninteractive' (mirroring core.el's startup log rotate) so batch ERT
;; runs neither shell out to real `git' nor depend on the repo state.
(unless noninteractive
  (setq claude-repl--version (claude-repl--compute-version)))

(defun claude-repl-version ()
  "Display the git SHA of the loaded doom config in the echo area.
Reads the cached `claude-repl--version', refreshed on every load, and
returns the SHA string (or the sentinel \"unknown\" when undetermined)."
  (interactive)
  (let ((version (or claude-repl--version "unknown")))
    (message "claude-repl version: %s" version)
    version))

(claude-repl--load-module "core")
;; WHY: workspace.el owns `claude-repl--workspaces' and the hash
;; accessors that nearly every other module uses.  Must load right
;; after core.el (which provides the logging primitives workspace.el
;; calls) and before everything else.
(claude-repl--load-module "workspace")
(claude-repl--load-module "install")
(claude-repl--load-module "notifications")
(claude-repl--load-module "history")
(claude-repl--load-module "memory-state")
(claude-repl--load-module "overlay")
(claude-repl--load-module "status")
(claude-repl--load-module "workspace-status-export")
(claude-repl--load-module "events")
(claude-repl--load-module "autosave")
(claude-repl--load-module "sentinel")
(claude-repl--load-module "vterm-freeze")
(claude-repl--load-module "input")
(claude-repl--load-module "backoff-retry")
(claude-repl--load-module "commands")
(claude-repl--load-module "session")
(claude-repl--load-module "prompt-summary")
(claude-repl--load-module "ai-title")
(claude-repl--load-module "window")
(claude-repl--load-module "sibling-popup")
(claude-repl--load-module "panels")
(claude-repl--load-module "merge-handlers")
(claude-repl--load-module "worktree")
(claude-repl--load-module "rename")
(claude-repl--load-module "drawer")
(claude-repl--load-module "hide-project-dirs")
(claude-repl--load-module "keybindings")
(claude-repl--load-module "magit")
(claude-repl--load-module "emoji")
(claude-repl--load-module "prevent-select")
(claude-repl--load-module "close-panels-on-open")
(claude-repl--load-module "caffeinate")

(if claude-repl--load-errors
    (progn
      (message "[claude-repl] Loaded with %d ERROR(S):" (length claude-repl--load-errors))
      (dolist (pair (nreverse claude-repl--load-errors))
        (message "[claude-repl]   %s.el: %S" (car pair) (cdr pair)))
      (error "[claude-repl] FATAL: %d module(s) failed to load — see messages above"
             (length claude-repl--load-errors)))
  (message "[claude-repl] Loaded Claude-Repl package."))

;; Snapshot restore is wired to `emacs-startup-hook' through an idle
;; timer (`claude-repl-snapshot-startup-load-delay' seconds).  The
;; deferral exists only to let persp-mode finish its own initialization
;; before our loader iterates entries — once the timer fires, restore
;; runs fully synchronously: each entry is created, activated,
;; project-aligned (default-directory, dir-locals, magit lambda,
;; find-file recent), and has its claude session started before the
;; loader moves to the next entry.  The loader returns to whichever
;; workspace was active when it began.
;;
;; Companion save-guard (`claude-repl--snapshot-loaded-p') prevents
;; `--state-save' from clobbering the on-disk roster if a state-
;; mutation fires before the idle timer resolves.
;;
;; Snapshot save is paired with `claude-repl--state-save' (history.el) so
;; the roster is updated on every workspace mutation rather than only at
;; Emacs quit — that way a crash before quit doesn't lose the roster.
(defcustom claude-repl-snapshot-startup-load-delay 2.0
  "Idle seconds to wait after `emacs-startup-hook' before restoring snapshot.
Tuned to let persp-mode finish initialization (so `safe-persp-name'
and friends are bound) before the loader iterates entries.  Set to nil
to disable startup-time restore entirely."
  :type '(choice (const :tag "Disabled" nil) number)
  :group 'claude-repl)

(defun claude-repl--schedule-snapshot-startup-load ()
  "Schedule `--load-workspace-snapshot-on-startup' on an idle timer.
Honours `claude-repl-snapshot-startup-load-delay'; a nil delay disables
the auto-load entirely.  Intended to run from `emacs-startup-hook'."
  (when claude-repl-snapshot-startup-load-delay
    (run-with-idle-timer claude-repl-snapshot-startup-load-delay
                         nil
                         #'claude-repl--load-workspace-snapshot-on-startup)))

(add-hook 'emacs-startup-hook #'claude-repl--schedule-snapshot-startup-load)

(provide 'claude-repl)
;;; config.el ends here
