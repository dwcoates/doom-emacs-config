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

(defvar agent-repl--global-log-scope)

(defun agent-repl--boot-info (fmt &rest args)
  "Emit a bootstrap-phase notice for FMT and ARGS to the QUIET sink.
Delegates to `agent-repl--info' once core.el has defined it.  Before then,
falls back to a `message' quieted with `inhibit-message', so the line still
lands in *Messages* but never flashes in the echo area."
  (if (fboundp 'agent-repl--info)
      (apply #'agent-repl--info agent-repl--global-log-scope fmt args)
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
      (apply #'agent-repl--warn agent-repl--global-log-scope fmt args)
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

;; ---- The elisp build this Emacs loaded ----
;;
;; Every process reports its build when it connects, and Emacs's is the
;; content hash of the elisp it LOADED (WatchDaemonEmacs.elisp_build).  The
;; loader records, in load order, each module's name and the SHA-256 of the
;; `lisp/<module>.el' bytes it loaded; `lisp/elisp-build.el' turns that list
;; into the build by the proto's algorithm.  The list is RESET on every load
;; of this file, exactly like `agent-repl--load-errors', so a reload reports
;; what the reload loaded rather than an accumulation of every load since
;; startup.

(defvar agent-repl--elisp-module-builds nil
  "The elisp this Emacs loaded, as ((MODULE . SHA256) ...) in load order.
MODULE is the name `agent-repl--load-module' was given (\"core\"), and
SHA256 the lowercase hex SHA-256 of the bytes of `lisp/MODULE.el' as they
were when it was loaded.  A module whose file is absent records nothing.")

(setq agent-repl--elisp-module-builds nil)

(defun agent-repl--elisp-module-file (root module)
  "Return the absolute path of MODULE's source, `lisp/MODULE.el' under ROOT.
Always the `.el' source, even where a `.elc' exists: the build is the
content hash of the source, which is what the daemon hashes too."
  (expand-file-name (concat "lisp/" module ".el") root))

(defun agent-repl--elisp-file-sha256 (file)
  "Return the lowercase hex SHA-256 of FILE's bytes, or nil when FILE is absent.
The file is read LITERALLY into a unibyte buffer, so the digest is of the
bytes on disk — no decoding, no end-of-line conversion — which is what the
daemon's Go side hashes from the checkout."
  (when (file-exists-p file)
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents-literally file)
      (secure-hash 'sha256 (current-buffer)))))

(defun agent-repl--elisp-record-module-build (root module)
  "Record MODULE's source under ROOT in `agent-repl--elisp-module-builds'.
Appends (MODULE . SHA256) so the list stays in load order.  An absent file
records nothing: it contributes no line to the build, and its load fails
and is reported on the loader's own path."
  (let ((sha (agent-repl--elisp-file-sha256 (agent-repl--elisp-module-file root module))))
    (if sha
        (setq agent-repl--elisp-module-builds
              (append agent-repl--elisp-module-builds (list (cons module sha))))
      (agent-repl--boot-info "elisp-build: %s.el is absent under %s; it records nothing"
                             module root))))

(defmacro agent-repl--load-module (file)
  "Load FILE via `load!', recording any error for collective reporting.
Before loading, FILE's source bytes are recorded in
`agent-repl--elisp-module-builds' (the build this Emacs reports).
FILE names a module source WITHOUT its `lisp/' prefix and `.el' suffix:
every source lives in `lisp/' beside this file, while config.el itself
stays at the module root because that is the path Doom's module loader
resolves.
The per-file success line is background chatter and goes to the quiet sink
via `agent-repl--boot-info'; a load FAILURE is loud, via
`agent-repl--boot-warn'.  Both use the bootstrap helpers rather than the
core.el ladder directly because the very first expansion of this macro is
what loads core.el."
  `(condition-case err
       (progn
         (agent-repl--elisp-record-module-build
          (file-name-directory (or load-file-name buffer-file-name)) ,file)
         (load! (concat "lisp/" ,file))
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
;; module was loaded from.  It is INVALIDATED via `setq' (NOT `defvar') on
;; every load below, so `M-x doom/reload' recomputes it from the freshly
;; checked-out worktree instead of keeping the value captured at first
;; startup, and it is COMPUTED lazily on first use so no startup pays a
;; synchronous `git rev-parse' before the frame appears.
;; `agent-repl-version' surfaces it interactively.

(defvar agent-repl--version nil
  "Git SHA of the doom config this agent-repl module was last loaded from.
Invalidated on every load (including `M-x doom/reload') by the `setq'
below, so it always reflects the version actually running rather than a
stale first-startup value.  nil when the SHA has not been computed yet,
or could not be determined.")

(defvar agent-repl--version-computed nil
  "Non-nil once this load has tried to compute `agent-repl--version'.
Distinguishes \"not asked yet\" from \"asked, and git had no answer\", so a
repo that cannot resolve a SHA is not re-probed on every call.")

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

;; INVALIDATE on every load so a reload picks up the new SHA — but do NOT
;; compute it here.  This runs at module-load time, which on startup is
;; before the first frame is painted, and `git rev-parse' is a synchronous
;; subprocess: the SHA is wanted by an interactive command and nothing
;; else, so it is computed on first use instead.
(setq agent-repl--version nil
      agent-repl--version-computed nil)

(defun agent-repl--version-string ()
  "Return the loaded config's git SHA, computing it once per load.
LAZY BY DESIGN: the git probe is a synchronous subprocess, and paying it
at load time cost every startup a `git rev-parse' before the frame
appeared."
  (unless agent-repl--version-computed
    (setq agent-repl--version-computed t
          agent-repl--version (agent-repl--compute-version))
    (agent-repl--boot-info "version: computed config-file=%S sha=%S"
                           agent-repl--config-file agent-repl--version))
  agent-repl--version)

(defun agent-repl-version ()
  "Display the git SHA of the loaded doom config in the echo area.
Computes the SHA on first use (and caches it for the rest of this load),
returning the SHA string (or the sentinel \"unknown\" when undetermined)."
  (interactive)
  (let ((version (or (agent-repl--version-string) "unknown")))
    (agent-repl--log '(:agent-repl-central "the loaded module version is process-wide")
                     "version command: cached-sha=%S display=%S"
                     agent-repl--version version)
    (message "agent-repl version: %s" version)
    version))

(agent-repl--load-module "core")
;; WHY: the wire-*.el codec is the protojson layer every agentrepl.v1 caller
;; sits on, so it loads directly after core.el — its only dependency is
;; core.el's logging ladder — and before anything that speaks to the daemon.
;; Order within the group is the dependency order: wire-common.el carries the
;; error, the shared primitives and the leaf vocabularies the other three
;; build on; wire-verbs.el is the workspace and daemon-admin verbs.
(agent-repl--load-module "wire-common")
(agent-repl--load-module "wire-host")
(agent-repl--load-module "wire-roster")
(agent-repl--load-module "wire-verbs")
;; WHY: connect.el is the Connect-over-HTTP/1.1 transport every daemon
;; exchange rides.  It loads right after the codec, whose error it never
;; needs but whose consumers all sit above it, and before any consumer of
;; the daemon.
(agent-repl--load-module "connect")
;; WHY: rpc.el is the one function per `agentrepl.v1' rpc Emacs calls, and
;; every daemon-facing module calls it rather than the transport directly.
;; It sits above both the codec it encodes through and the transport it
;; sends over.
(agent-repl--load-module "rpc")
;; WHY: popup.el is the ONE shared editor-popup subroutine ("open path[:line]
;; in a doom popup, right side, half width"); notes.el, commands.el and the
;; host stream's `open_in_editor' arm all call it, so it loads above all of
;; them.  It depends on core.el's logging ladder and nothing else.
(agent-repl--load-module "popup")
;; WHY: daemon-link.el owns the daemon connection's whole life — discovery,
;; the one WatchDaemon stream, the reconnect loop and the blue-green
;; handover — and publishes the hooks every daemon-facing module hangs off.
;; It needs core.el, connect.el and rpc.el and nothing else, so it loads
;; immediately after the transport and before its first consumer.
(agent-repl--load-module "daemon-link")
;; WHY: mutation-progress.el is the correlation seat the daemon-link's push
;; dispatch hands workspace-mutation progress to, and the create verb registers
;; its callbacks with; it needs only core.el, so it loads beside daemon-link.
(agent-repl--load-module "mutation-progress")
;; WHY: elisp-build.el answers the elisp build every WatchDaemon reports and
;; carries out a deploy's pushed `reload_elisp'.  daemon-link.el dispatches
;; that push to it and rpc.el asks it for the build, both at call time; its
;; own needs are core.el's logging ladder and heartbeat assertion.
(agent-repl--load-module "elisp-build")
;; WHY: external-browser.el pins `browse-url-browser-function' so every
;; hyperlink lands in the external Chrome profile instead of an Emacs
;; xwidget buffer.  It needs only core.el's logging ladder, and it loads
;; this early so no later module can visit a URL before the handler is in
;; place.
;; WHY: workspace.el owns `agent-repl--workspaces' and the hash
;; accessors that nearly every other module uses.  Must load right
;; after core.el (which provides the logging primitives workspace.el
;; calls) and before everything else.
(agent-repl--load-module "workspace")
;; WHY: host.el is the agentrepl.v1 HOST section (register / select /
;; WatchHostWorkspace / adopt).  It registers on daemon-link.el's up/down
;; hooks AND on workspace.el's perspective-activation boundary at load
;; time, so it loads after BOTH.  The W2-B surfaces it calls (the tab
;; blink, the webview reload, the editor popup) resolve at call time, long
;; after every module is loaded.
(agent-repl--load-module "host")
;; WHY: verbs.el is the workspace and daemon-admin verbs as thin wrappers.
;; It sits directly on rpc.el, host.el (for the ref and the per-workspace
;; connection) and daemon-link.el (for the primary connection), all loaded
;; above.  The roster it reads for its open/create pickers and the tab
;; teardown it calls on a close resolve at call time, long after every
;; module is loaded.
(agent-repl--load-module "verbs")
;; WHY: roster.el is the WatchWorkspaceRoster consumer — the one source of
;; Emacs's tabs, their order and their paint.  It sits above rpc.el (it
;; subscribes through it) and calls workspace.el and status.el at runtime
;; only, so it may load before either.
;; It loads after host.el so the host accessors and hooks it reacts through
;; are defined before its own hook registrations run.
(agent-repl--load-module "roster")
;; WHY: frontends.el defines the presentation-frontend registry that
;; frontend.el (gui) registers into at load time.
(agent-repl--load-module "frontends")
(agent-repl--load-module "notifications")
(agent-repl--load-module "history")
(agent-repl--load-module "status")
(agent-repl--load-module "autosave")
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
;; The held-prompt queue: what a prompt sent across a daemon bounce becomes
;; instead of a dropped submission.  It needs only core.el and workspace.el at
;; load time; its defaults call back into the transport from lambdas that run
;; long after every module is loaded.
(agent-repl--load-module "prompt-queue")
;; WHY: services.el owns launchd lifecycle for shim-store/sidecar and the
;; coordinated runtime bounce.  It needs the daemon/client plus the pushed
;; state stores loaded so its preflight can reject every active turn before
;; changing any process.
(agent-repl--load-module "services")
(agent-repl--load-module "frontend")
;; WHY: webview-recovery.el drives the webapp's recovery hook through
;; frontend.el's execute-script chokepoint, so it must load after it.  It
;; exists because a hidden xwidget webview's own timers are suspended by the
;; embedder — see the file's commentary.
(agent-repl--load-module "webview-recovery")
;; WHY: notes.el owns the per-workspace org notes file, all that survives
;; of tasks.el.  It reads core.el's state-dir resolver, workspace.el's
;; `--ws-current-name' and autosave.el's save-on-kill helper — all loaded
;; above.
(agent-repl--load-module "notes")
(agent-repl--load-module "prompt-summary")
(agent-repl--load-module "window")
(agent-repl--load-module "sibling-popup")
(agent-repl--load-module "panels")
;; WHY: open-progress.el shows its placeholder in the SAME main-area window
;; the webview mount claims, so it needs frontend.el's host resolution.  It
;; subscribes to the pushed-state hook at load time; `add-hook' auto-vivifies
;; that variable, so the producer's position above is not load-critical.
(agent-repl--load-module "open-progress")
(agent-repl--load-module "worktree")
;; merge-handlers.el and workspace-create-client.el are DELETED.  Both were
;; written against the removed frontend-state/frontend-uds transport, and
;; both did work that is now the daemon's outright: verbs.el replaces them
;; with thin wrappers over MergeWorkspace and CreateWorkspace.
;; WHY: conversations.el is the `SPC j c' binding over
;; ListWorkspaceTranscripts and BindWorkspaceSession.  It sits on rpc.el,
;; verbs.el (the ref, the connection and the one send dispatcher) and
;; mutation-progress.el (the bind's stages), all loaded above, and it must
;; load before keybindings.el names its command.
(agent-repl--load-module "conversations")
(agent-repl--load-module "keybindings")
(agent-repl--load-module "magit")
(agent-repl--load-module "emoji")
(agent-repl--load-module "prevent-select")
(agent-repl--load-module "close-panels-on-open")
;; WHY: find-file-workspace.el advises the DISPLAY primitives so a visited
;; file lands in the workspace owning its git root.  It reads window.el's
;; side-window predicate, worktree.el's dir→workspace reverse lookup,
;; verbs.el's open verb and commands.el's register verb, and it hangs its
;; pending-placement handler on roster.el's post-reconcile hook, so it loads
;; after
;; all of them.  Loading it AFTER close-panels-on-open.el also makes its
;; :around advice the OUTER one on the two shared primitives, so a routed
;; file never triggers that module's panel close: the panels belong to the
;; workspace the file is being routed INTO.
(agent-repl--load-module "find-file-workspace")
;; WHY: interaction-record.el depends on core.el alone (the logging
;; ladder and the state-dir resolver) and is loaded LAST so its
;; startup env check (`AGENT_REPL_RECORD_INTERACTIONS') arms
;; `pre-command-hook' only once every other module's own hooks and
;; advice are installed — a recording that started mid-load would
;; capture the tail of the module load rather than the user's session.
(agent-repl--load-module "interaction-record")

;; Workspace notes popup: `agent-repl-notes-open' (notes.el) opens the
;; current workspace's org notes file in a right-side popup that leaves the
;; agent-repl panels the left two thirds of the frame.  `:autosave t'
;; persists the notes when the popup is dismissed, matching the
;; buffer-local save-on-kill hook the opener also installs.
;;
;; The rule matches by PREDICATE, not by buffer name: a notes buffer is
;; named `<workspace>.org', which no name pattern can tell apart from any
;; other org file the user opens, while its residence under the notes
;; directory identifies it exactly.
;;
;; Guarded because the Doom popup module (and its `set-popup-rule!' macro)
;; is absent under `emacs -Q' — the batch ERT suite loads this file but has
;; no popup system to configure.
(defun agent-repl--notes-buffer-p (buffer-name &optional _action)
  "Return non-nil when BUFFER-NAME names a buffer visiting a notes file.
The popup predicate for `agent-repl-notes-open': a notes buffer is
identified by the file it visits living under `agent-repl--notes-dir',
never by its name."
  (let* ((buf (get-buffer buffer-name))
         (file (and (buffer-live-p buf) (buffer-file-name buf)))
         (match (and file
                     (string-prefix-p (expand-file-name (agent-repl--notes-dir))
                                      (expand-file-name file)))))
    (agent-repl--log '(:agent-repl-central "popup classification is process-wide")
                     "elisp.notes.popup-predicate: buffer=%S file=%S match=%s"
                     buffer-name file (if match t nil))
    match))

(if (fboundp 'set-popup-rule!)
    (progn
      (set-popup-rule! #'agent-repl--notes-buffer-p
        :side 'right :size 0.33 :select t :quit t :autosave t)
      (agent-repl--boot-info "workspace-notes popup rule installed side=right size=0.33 autosave=t"))
  (agent-repl--boot-info "workspace-notes popup rule skipped; set-popup-rule! is unavailable"))

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
  (agent-repl--info '(:agent-repl-central "module loading is process-wide")
                    "Loaded Agent-Repl package."))

;; NO SNAPSHOT RESTORE.  The durable Emacs workspace-roster snapshot is
;; gone: THE DAEMON is the source of which workspaces exist, and on connect
;; Emacs opens tabs from the roster stream (roster.el's reconciliation).  A
;; second, Emacs-authored roster on disk could only ever disagree with the
;; pushed one, and reconciling two sources of truth was the machinery this
;; overhaul removed rather than repaired.

;; COLD START.  Emacs owns bringing a daemon up and nothing after that:
;; `agent-repl-daemon-ensure' adopts any daemon that answers, and only
;; builds and starts one when `daemon.addr' names nobody.  Registered on
;; `emacs-startup-hook' rather than run here — and what the hook registers
;; is `agent-repl-daemon-schedule-ensure', which only ARMS an idle timer:
;; `emacs-startup-hook' runs before Doom's UI init and before the first
;; redisplay, so an ensure run directly from it holds the frame back until
;; the whole stack build finishes.  GATED ON `noninteractive' too, because
;; a batch run must never build or spawn anything.
(if (and agent-repl-frontend-auto-start (not noninteractive))
    (progn
      (add-hook 'emacs-startup-hook #'agent-repl-daemon-schedule-ensure)
      (agent-repl--boot-info "cold-start: registered daemon ensure on emacs-startup-hook"))
  (agent-repl--boot-info "cold-start: daemon ensure NOT registered auto-start=%s batch=%s"
                         agent-repl-frontend-auto-start noninteractive))

;; ORPHANED LOG TARGETS.  Every Emacs instance before 2026-09-11 minted its
;; own temporary-root log target, and 22,112 of them had accumulated by the
;; time the standing-target rule landed.  The sweep that removes the
;; unreferenced, day-old ones is armed on an IDLE timer, never run from the
;; startup hook: unlinking files before the first redisplay would hold the
;; frame off screen to tidy a directory.  Batch runs arm nothing.
(if noninteractive
    (agent-repl--boot-info "cold-start: orphan log sweep NOT registered batch=%s"
                           noninteractive)
  (add-hook 'emacs-startup-hook #'agent-repl-schedule-orphan-log-sweep)
  (agent-repl--boot-info "cold-start: registered orphan log sweep on emacs-startup-hook"))

(provide 'agent-repl)
;;; config.el ends here
