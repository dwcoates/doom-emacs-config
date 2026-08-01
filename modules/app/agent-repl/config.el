;;; config.el --- claude repl for doom emacs -*- lexical-binding: t; -*-

;; Author: Dodge Coates
;; URL: https://github.com/dodgecoates
;; Version: 0.1.0

;;; Commentary:
;; Main loader for the agent-repl module. Sub-files are loaded in
;; dependency order; Elisp's call-time function resolution means
;; forward references between defuns are safe.

;;; Code:

;;;; ---- Bootstrap-phase emission ----
;;
;; config.el is the one file that cannot lean on core.el's log-severity
;; ladder (`agent-repl--info' / `agent-repl--warn'): it runs BEFORE core.el
;; is loaded, and it is also the code that REPORTS core.el failing to load.
;; So it carries its own bootstrap-phase stand-ins:
;;
;;   `--boot-info'  quiet notice  -> *Messages* only, never the echo area
;;   `--boot-warn'  warning       -> loud ONLY when core.el is absent (below)
;;
;; Once core.el has loaded, both helpers delegate to the real ladder, so
;; lines emitted after that point still reach the log FILE.  Post-core,
;; `agent-repl--warn' is the QUIET sink — the modeline is reserved for
;; genuine fatal `error's alone — so a delegated boot-warning does NOT flash
;; in the modeline.  Before core.el exists, though, the ladder is undefined
;; and `--boot-warn' degrades to a bare `message' that DOES reach the echo
;; area, on purpose: a failure to load core.el breaks the whole logging
;; system, which is exactly the genuine fatal condition the user must see.

(defun agent-repl--boot-info (fmt &rest args)
  "Emit a bootstrap-phase notice for FMT and ARGS to the QUIET sink.
Delegates to `agent-repl--info' once core.el has defined it.  Before then,
falls back to a `message' quieted with `inhibit-message', so the line still
lands in *Messages* but never flashes in the echo area."
  (if (fboundp 'agent-repl--info)
      (apply #'agent-repl--info nil fmt args)
    (let ((inhibit-message t))
      (apply #'message (concat "[agent-repl] " fmt) args))))

(defun agent-repl--boot-warn (fmt &rest args)
  "Emit a bootstrap-phase WARNING for FMT and ARGS.
Delegates to `agent-repl--warn' once core.el has defined it — post-core
that is the QUIET sink (log file + *Messages*, no modeline flash), so a
delegated boot-warning does not interrupt.  Before core.el exists,
`agent-repl--warn' is unbound and this falls back to a bare `message'
that DOES reach the echo area: core.el is the one module whose load
failure leaves the ladder undefined, and a broken logging system is a
genuine fatal condition the user must see immediately."
  (if (fboundp 'agent-repl--warn)
      (apply #'agent-repl--warn nil fmt args)
    (apply #'message (concat "[agent-repl] WARNING: " fmt) args)))

(agent-repl--boot-info "Loading Agent-Repl package...")

(defvar agent-repl--config-file
  (or load-file-name buffer-file-name)
  "Absolute path to this config.el, captured at load time for reloading.")

(defvar agent-repl--load-errors nil
  "List of (FILE . ERROR) pairs for sub-files that failed to load.")

;; Reset the accumulator on every load — `defvar' only initializes once,
;; so without this `M-x doom/reload' would re-report stale errors from a
;; prior failed load even after the next load succeeded, masking actual
;; status.
(setq agent-repl--load-errors nil)

(defmacro agent-repl--load-module (file)
  "Load FILE via `load!', recording any error for collective reporting.
The per-file success line is background chatter and goes to the quiet sink
via `agent-repl--boot-info'; a load FAILURE is loud, via
`agent-repl--boot-warn'.  Both use the bootstrap helpers rather than the
core.el ladder directly because the very first expansion of this macro is
what loads core.el."
  `(condition-case err
       (progn
         (load! ,file)
         (agent-repl--boot-info "%s.el loaded." ,file))
     (error
      (push (cons ,file err) agent-repl--load-errors)
      (agent-repl--boot-warn "FAILED to load %s.el: %S" ,file err))))

(defun agent-repl--early-git-string (&rest args)
  "Run `git ARGS' synchronously and return its trimmed stdout.
Returns the empty string on non-zero exit — the early callers need a
tolerant probe, not a hard fail, mirroring the
`agent-repl--git-string-quiet' contract.

This IS the external-boundary wrapper for the pre-`core.el' code path.
Defined locally because it runs at module-loader top level — BEFORE
`core.el' loads and the regular `agent-repl--git-*' family becomes
available.  Same role, separately defined.  Registered in
`agent-repl--external-boundary-functions' (core.el) so the
test-time runtime guards see it and tests cannot accidentally shell
out to real `git'."
  (with-temp-buffer
    (agent-repl--boot-info "early-git-string: invoking args=%S" args)
    (let* ((exit-code (apply #'call-process "git" nil t nil args)) ;; ALLOW-EXTERNAL-BOUNDARY
           (stdout (string-trim (buffer-string)))
           (success-p (zerop exit-code)))
      (agent-repl--boot-info
       "early-git-string: args=%S exit=%d success=%s stdout=%S"
       args exit-code success-p stdout)
      (if success-p stdout ""))))

;; ---- Loaded-version SHA ----
;;
;; `agent-repl--version' caches the git SHA of the doom config that this
;; module was loaded from.  It is refreshed via `setq' (NOT `defvar') on
;; every load below, so `M-x doom/reload' updates it to the freshly
;; checked-out SHA instead of keeping the value captured at first startup.
;; `agent-repl-version' surfaces it interactively.

(defvar agent-repl--version nil
  "Git SHA of the doom config this agent-repl module was last loaded from.
Refreshed on every load (including `M-x doom/reload') by the
`noninteractive'-gated `setq' below, so it always reflects the version
actually running rather than a stale first-startup value.  nil when the
SHA could not be determined.")

(defun agent-repl--compute-version ()
  "Return the git SHA of the doom repo this module was loaded from, or nil.
Resolves the repo from `agent-repl--config-file's directory so a linked
worktree reports its own checked-out SHA rather than the primary
worktree's.  Returns nil when the config-file path is unknown or git
cannot resolve a SHA (for example outside a repository).

Uses the early-boundary `agent-repl--early-git-string' wrapper so this
helper has no dependency on `core.el' having loaded — it runs at the
config-loader top level, before `core.el' has loaded."
  (when agent-repl--config-file
    (let ((sha (agent-repl--early-git-string
                "-C" (file-name-directory agent-repl--config-file)
                "rev-parse" "HEAD")))
      (and (not (string-empty-p sha)) sha))))

;; Refresh on EVERY load so reloads pick up the new SHA.  Gated against
;; `noninteractive' (mirroring core.el's startup log rotate) so batch ERT
;; runs neither shell out to real `git' nor depend on the repo state.
(unless noninteractive
  (setq agent-repl--version (agent-repl--compute-version))
  (agent-repl--boot-info "version: refreshed config-file=%S sha=%S"
                         agent-repl--config-file agent-repl--version))

(defun agent-repl-version ()
  "Display the git SHA of the loaded doom config in the echo area.
Reads the cached `agent-repl--version', refreshed on every load, and
returns the SHA string (or the sentinel \"unknown\" when undetermined)."
  (interactive)
  (let ((version (or agent-repl--version "unknown")))
    (agent-repl--log nil "version command: cached-sha=%S display=%S"
                      agent-repl--version version)
    (message "agent-repl version: %s" version)
    version))

(agent-repl--load-module "core")
;; WHY: workspace.el owns `agent-repl--workspaces' and the hash
;; accessors that nearly every other module uses.  Must load right
;; after core.el (which provides the logging primitives workspace.el
;; calls) and before everything else.
(agent-repl--load-module "workspace")
;; WHY: backend.el defines the pluggable agent-CLI backend registry and
;; registers the `claude' backend.  It must load after workspace.el
;; (its selection helpers call the `agent-repl--ws-*' accessors) and
;; before session.el (whose command assembly resolves the backend).
(agent-repl--load-module "backend")
;; WHY: frontends.el defines the presentation-frontend registry that
;; frontend.el (gui) registers into at load time.
(agent-repl--load-module "frontends")
(agent-repl--load-module "install")
;; WHY: codex.el registers the codex backend (backend.el must precede
;; it) and reuses install.el's hook-registration writer for its
;; hooks.json installer.
(agent-repl--load-module "codex")
(agent-repl--load-module "notifications")
(agent-repl--load-module "history")
(agent-repl--load-module "memory-state")
(agent-repl--load-module "status")
(agent-repl--load-module "workspace-status-export")
(agent-repl--load-module "autosave")
(agent-repl--load-module "sentinel")
;; WHY: output-nav.el defines the feed-cycling commands that
;; `agent-repl-input-mode-map' (input.el) binds, and drives them through
;; the webview script boundary in frontend.el -- both sides already loaded
;; above.  Ahead of input.el so the commands exist before the map cites
;; them, though `map!' stores symbols and would resolve them either way.
(agent-repl--load-module "output-nav")
(agent-repl--load-module "input")
;; WHY: clipboard-image.el binds `agent-repl-attach-clipboard-image' into
;; `agent-repl-input-mode-map' (input.el) and writes captured images under
;; the workspace dir via `agent-repl--ws-dir' (status.el) -- both already
;; loaded above.  Named `clipboard-image', NOT `image', so its `provide'
;; never shadows Emacs's built-in `image' feature.
(agent-repl--load-module "clipboard-image")
(agent-repl--load-module "commands")
(agent-repl--load-module "session")
(agent-repl--load-module "daemon")
(agent-repl--load-module "frontend-client")
;; The classified-failure vocabulary (F4): the ONE place Emacs turns a
;; failure into something a human reads, and the closed `client.'-prefixed
;; set it is allowed to classify for itself. Loaded BEFORE the transport,
;; which surfaces a refused command's classified ack through it.
(agent-repl--load-module "failure")
;; The agent-shim frontend UDS transport + state application (design §10,
;; G10).  Loaded right after frontend-client: `frontend-uds' owns the
;; connection/framing/dispatch, `frontend-state' registers the state-bearing
;; handlers on it (order matters — `frontend-state' calls
;; `agent-repl--uds-register-handler', defined in `frontend-uds').  These
;; supersede the sentinel/hook-derived render-status inputs the cutover deletes.
(agent-repl--load-module "frontend-uds")
(agent-repl--load-module "frontend-state")
;; WHY: services.el owns launchd lifecycle for shim-store/sidecar and the
;; coordinated runtime bounce.  It needs the daemon/client plus the pushed
;; state stores loaded so its preflight can reject every active turn before
;; changing any process.
(agent-repl--load-module "services")
;; WHY: permission.el registers the `conversationDelta' frame handler on the
;; transport (frontend-uds), so it must load after it; it drives the
;; permission UX (prompt bookkeeping + desktop notification + answer
;; round-trip) entirely from pushed `frontend.v1' state, replacing the
;; deleted permission sentinels/hooks (S8/S9).  Needs notifications.el and
;; workspace.el (both loaded above) for `--notify' and the ws accessors.
(agent-repl--load-module "permission")
(agent-repl--load-module "frontend")
;; WHY: tasks.el owns the user-defined task model the sidebar's "Task"
;; view groups workspaces under, persisting via history.el's sexp-file
;; helpers and reading the `agent-repl--ws-*' accessors — history.el and
;; workspace.el both load above.  Must precede sidebar.el, which builds
;; the task-view roster and hosts the task command handlers.
(agent-repl--load-module "tasks")
(agent-repl--load-module "sidebar")
;; There is no login module here any more.  The gui login ran in an Emacs
;; vterm because the OAuth flow needs a TTY and Emacs was believed to be the
;; only TTY host in the system.  The daemon can open a pty of its own, so it
;; now runs the login and streams the terminal to the webapp — Emacs is not
;; in that path at all.  See daemon/internal/login.
;; WHY: readiness.el decorates the webview modeline off frontend.el's
;; `agent-repl-frontend-webview-adopt-hook' and faces its cells with the
;; color constants status.el defines — both load above.  It also starts a
;; timer, so it must come after core.el's `--cancel-all-timers'.
(agent-repl--load-module "readiness")
(agent-repl--load-module "prompt-summary")
(agent-repl--load-module "ai-title")
(agent-repl--load-module "context")
(agent-repl--load-module "window")
(agent-repl--load-module "sibling-popup")
(agent-repl--load-module "panels")
;; WHY: explain-config.el mounts the SAME webkit GUI the workspace
;; frontend uses, so it needs frontend.el (the webview boundary wrapper)
;; and frontend-client.el (session CRUD + message injection) already
;; registered, plus window.el's `--delete-buffer-windows' for the popup's
;; hide path.  It registers a persp-activated hook at load time, so
;; workspace.el must precede it too — every one of those is loaded above.
(agent-repl--load-module "explain-config")
(agent-repl--load-module "merge-handlers")
(agent-repl--load-module "worktree")
;; The daemon owns workspace creation.  This thin-client bridge loads after
;; worktree.el/sidebar.el so inbound HostAction can reuse the existing UI-only
;; handlers, and after frontend-uds.el so it can register the new frame arms.
(agent-repl--load-module "workspace-create-client")
(agent-repl--load-module "rename")
(agent-repl--load-module "hide-project-dirs")
(agent-repl--load-module "keybindings")
(agent-repl--load-module "magit")
(agent-repl--load-module "emoji")
(agent-repl--load-module "prevent-select")
(agent-repl--load-module "close-panels-on-open")

;; Task notes popup: the sidebar's Task view opens each task's org notes
;; file (`agent-repl--task-open', tasks.el) in a right-side popup that
;; leaves the agent-repl panels the left two thirds of the frame.
;; `:autosave t' persists the notes when the popup is dismissed, matching
;; the buffer-local save-on-kill hook the opener also installs.  Guarded
;; because the Doom popup module (and its `set-popup-rule!' macro) is
;; absent under `emacs -Q' — the batch ERT suite loads this file but has
;; no popup system to configure.
(if (fboundp 'set-popup-rule!)
    (progn
      (set-popup-rule! "^task-notes-.*\\.org\\'"
        :side 'right :size 0.33 :select t :quit t :autosave t)
      (agent-repl--boot-info "task-notes popup rule installed side=right size=0.33 autosave=t"))
  (agent-repl--boot-info "task-notes popup rule skipped; set-popup-rule! is unavailable"))

(if agent-repl--load-errors
    (progn
      ;; `agent-repl--boot-warn', not `agent-repl--warn': this branch is
      ;; reached precisely when a module failed, and core.el is a module —
      ;; so the real ladder may not exist to report its own absence.
      (agent-repl--boot-warn "Loaded with %d ERROR(S):" (length agent-repl--load-errors))
      (dolist (pair (nreverse agent-repl--load-errors))
        (agent-repl--boot-warn "  %s.el: %S" (car pair) (cdr pair)))
      (error "[agent-repl] FATAL: %d module(s) failed to load — see messages above"
             (length agent-repl--load-errors)))
  (agent-repl--info nil "Loaded Agent-Repl package."))

;; Snapshot restore is wired to `emacs-startup-hook' through an idle
;; timer (`agent-repl-snapshot-startup-load-delay' seconds).  The
;; deferral exists only to let persp-mode finish its own initialization
;; before our loader iterates entries — once the timer fires, restore
;; runs fully synchronously: each entry is created, activated,
;; project-aligned (default-directory, dir-locals, magit lambda,
;; find-file recent), and has its claude session started before the
;; loader moves to the next entry.  The loader returns to whichever
;; workspace was active when it began.
;;
;; Companion save-guard (`agent-repl--snapshot-loaded-p') prevents
;; `--state-save' from clobbering the on-disk roster if a state-
;; mutation fires before the idle timer resolves.
;;
;; Snapshot save is paired with `agent-repl--state-save' (history.el) so
;; the roster is updated on every workspace mutation rather than only at
;; Emacs quit — that way a crash before quit doesn't lose the roster.
(defcustom agent-repl-snapshot-startup-load-delay 2.0
  "Idle seconds to wait after `emacs-startup-hook' before restoring snapshot.
Tuned to let persp-mode finish initialization (so `safe-persp-name'
and friends are bound) before the loader iterates entries.  Set to nil
to disable startup-time restore entirely."
  :type '(choice (const :tag "Disabled" nil) number)
  :group 'agent-repl)

(defun agent-repl--schedule-snapshot-startup-load ()
  "Schedule `--load-workspace-snapshot-on-startup' on an idle timer.
Honours `agent-repl-snapshot-startup-load-delay'; a nil delay disables
the auto-load entirely.  Intended to run from `emacs-startup-hook'."
  (if agent-repl-snapshot-startup-load-delay
      (let ((timer (run-with-idle-timer agent-repl-snapshot-startup-load-delay
                                         nil
                                         #'agent-repl--load-workspace-snapshot-on-startup)))
        (agent-repl--boot-info "snapshot-startup: scheduled delay=%S timer=%S callback=%S"
                               agent-repl-snapshot-startup-load-delay timer
                               #'agent-repl--load-workspace-snapshot-on-startup)
        timer)
    (agent-repl--boot-info "snapshot-startup: disabled delay=nil")))

(add-hook 'emacs-startup-hook #'agent-repl--schedule-snapshot-startup-load)
(agent-repl--boot-info "snapshot-startup: registered scheduler on emacs-startup-hook")

(provide 'agent-repl)
;;; config.el ends here
