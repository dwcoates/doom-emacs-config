;;; sentinel.el --- file-notify watcher for Claude Code hooks -*- lexical-binding: t; -*-

;;; Code:

(require 'filenotify)

(declare-function agent-repl--do-log "core")

;; ---------------------------------------------------------------------------
;; Constants
;; ---------------------------------------------------------------------------

(defconst agent-repl--sentinel-dir
  (agent-repl--global-state-file "workspace-notifications")
  "Directory where Claude Code hooks write sentinel files.
Lives at `~/.claude-emacs/workspace-notifications' (under
`agent-repl--global-state-dir'); the managed hook scripts compute the
identical path as `$HOME/.claude-emacs/workspace-notifications'.")

(defcustom agent-repl-sentinel-debug-log-filename "hook-debug.log"
  "Filename of the hook debug log to ignore in sentinel file processing."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-sentinel-dispatch-actions '(created changed)
  "File-notify actions that trigger sentinel dispatch."
  :type '(repeat symbol)
  :group 'agent-repl)

(defcustom agent-repl-sentinel-poll-file-regexp "\\`[^.]"
  "Regexp for filtering sentinel directory files during polling.
Only files matching this pattern are enumerated."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-sentinel-watch-events '(change)
  "File-notify event types to watch on the sentinel directory."
  :type '(repeat symbol)
  :group 'agent-repl)

;;; Workspace resolution

;; Workspace event detection via file-notify watcher.
;; Claude Code hooks write the CWD to sentinel files in the sentinel
;; directory; we watch it and dispatch by filename: permission_prompt,
;; stop_*, prompt_submit_*.

(defun agent-repl--ws-for-dir-fast (dir)
  "Fast path for DIR: iterate registered workspaces and match :project-dir.
Returns the workspace name whose canonicalized :project-dir equals the
canonicalized git-root of DIR, or nil.

Unlike the previous dir -> git-root -> md5 -> buffer -> persp-lookup chain,
this function does not touch persp-mode or buffer names.  Bypassing the
persp lookup closes the last known leak path that fed \"none\" into
`agent-repl--workspaces' when persp-mode routed an agent buffer to the
wrong perspective."
  (agent-repl--log-verbose nil "ws-for-dir-fast: ENTER dir=%S" dir)
  (let* ((target-root (and dir (agent-repl--git-root dir)))
         (canonical-target (when target-root
                             (agent-repl--path-canonical target-root)))
         (match nil))
    (agent-repl--log-verbose nil "ws-for-dir-fast: git-root=%S canonical=%S"
                              target-root canonical-target)
    (when canonical-target
      (let ((candidates nil))
        (dolist (ws (agent-repl--ws-registered-names))
          (let* ((proj (agent-repl--ws-get ws :project-dir))
                 (canonical-proj (when proj
                                   (agent-repl--path-canonical proj))))
            (agent-repl--log-verbose ws "ws-for-dir-fast: check ws=%s proj=%S canonical=%S match=%s"
                                      ws proj canonical-proj
                                      (if (and canonical-proj
                                               (string= canonical-target canonical-proj))
                                          "YES" "no"))
            (when (and canonical-proj
                       (string= canonical-target canonical-proj))
              (push ws candidates))))
        ;; Multiple entries can share one :project-dir — e.g. a no-name
        ;; `SPC TAB n' stub shadowing the real workspace.  Prefer a
        ;; fully-initialized, non-tombstoned workspace (one that ran
        ;; `initialize-ws-env', so it carries :active-env) over a stub;
        ;; matching the stub makes `active-inst' throw on its missing
        ;; :active-env.  Fall back to any match so a genuine single-entry
        ;; lookup is unchanged.
        (setq match (or (cl-find-if
                         (lambda (ws)
                           (and (agent-repl--ws-get ws :active-env)
                                (not (agent-repl--ws-get ws :nuked-at))))
                         candidates)
                        (car candidates)))
        (when (> (length candidates) 1)
          (agent-repl--log nil "ws-for-dir-fast: %d entries share dir=%S — chose ws=%s over candidates=%S"
                            (length candidates) canonical-target match candidates))))
    (if match
        (agent-repl--log-verbose match "ws-for-dir-fast: HIT dir=%S root=%S ws=%s"
                                  dir canonical-target match)
      (agent-repl--log-verbose nil "ws-for-dir-fast: MISS dir=%S root=%S"
                                dir canonical-target))
    match))

(defun agent-repl--ws-for-dir (dir)
  "Return the workspace name for an agent session rooted at DIR, or nil.
Resolves via the fast path: git-root -> hash -> buffer -> workspace."
  (agent-repl--log-verbose nil "ws-for-dir: ENTER dir=%S" dir)
  (let ((ws (agent-repl--ws-for-dir-fast dir)))
    (agent-repl--log-verbose nil "ws-for-dir: EXIT dir=%S ws=%S (via %s)"
                      dir ws (if ws "fast-path" "NONE"))
    ws))

(defun agent-repl--delete-sentinel-file (file ws)
  "Delete sentinel FILE, surfacing any failure loudly for workspace WS.
Both the normal dispatch path and the deprecated-prefix drain must remove a
sentinel file after acting on it, otherwise the poll fallback re-detects it
every cycle.  This runs from the file-notify / poll async context, where a
hard error would kill the sentinel watcher, so a failed delete is surfaced
via `agent-repl--warn' rather than rethrown."
  (condition-case err
      (progn
        (delete-file file)
        (agent-repl--log ws "delete-sentinel-file: deleted file=%s" file))
    (error
     (agent-repl--warn ws "could not delete sentinel file %s: %S"
                       (file-name-nondirectory file) err))))

(defvar agent-repl-ws-fully-loaded-functions nil
  "Hook run when a Agent REPL workspace is fully loaded.
A workspace is fully loaded when BOTH:
  1. the `:agent-ready' bit is set, AND
  2. `--on-workspace-switch' has completed (`:ws-loaded' bit set).

CUTOVER GAP (design §10): the `:agent-ready' bit was previously set by
`--on-session-start-event', deleted in the agent-shim cutover along with
the SessionStart managed hook.  The daemon now owns session-ready
reporting (it pushes state and owns prompt submission), so the
`:agent-ready' half of this latch has no producer in Emacs until a
daemon-pushed frame is wired to set it.  `--latch-and-maybe-fire-loaded'
and the `:ws-loaded' callers (commands.el / panels.el) are retained.

Each handler is called with two arguments: (WS &optional MARKER).
WS is the workspace name string.  MARKER is `:timed-out' when the
snapshot loader's watchdog fired the hook synthetically because the
ws never reached both ready+settled within the timeout; otherwise
MARKER is nil (happy path).  Handlers that don't care about the
distinction may accept `(ws)' alone — the second arg is optional.

This hook is the LOAD BARRIER.  At fire time, no further
agent-repl-managed automatic code is queued to run for this
workspace's load lifecycle.

HANDLERS MUST BE SYNCHRONOUS AND SHORT-RUNNING.  The barrier guarantee
breaks the moment a handler schedules async work (`run-at-time',
`file-notify-add-watch', a process sentinel, `make-thread', etc.) —
that work will interleave with subsequent workspace loads and
resurrect every race this hook was designed to eliminate.  If you
need async behavior keyed off load completion, drive it from a
separate mechanism with its own state, not from this hook.

Handlers run via `run-hook-wrapped' wrapped in `condition-case', so a
broken handler cannot prevent later handlers from completing.

Fires exactly once per load cycle.  Latch state is cleared after
firing; a subsequent kill/restart that resets the ws plist also
implicitly resets the latch bits.")

(defun agent-repl--latch-and-maybe-fire-loaded (ws key &optional marker)
  "Set KEY to t on WS's plist; fire `ws-fully-loaded-functions' if both bits set.
KEY is `:agent-ready' or `:ws-loaded'.  When both bits are now t,
clears them and runs `agent-repl-ws-fully-loaded-functions' with
WS (and MARKER, when non-nil — typically `:timed-out' from the
watchdog path).  Both bits are explicitly cleared after firing so a
later kill-and-relaunch cycle starts clean (kill clears the whole
plist; explicit clear here covers the case where the ws keeps
running but its load cycle has logically ended).

One caller is `agent-repl--on-workspace-switch', which is driven by the
persp activation hook and so hands over whatever perspective persp-mode
activated — including persp-mode's own \"none\" and Doom's initial \"main\".
Those perspectives own no `:project-dir' and therefore no durable log sink,
so the latch bookkeeping uses the unscreened WS while the records go through
`agent-repl--ws-log-name' and name WS in their text.  A workspace that does
own a sink keeps its attribution."
  (agent-repl--ws-put ws key t)
  (let ((agent-ready (agent-repl--ws-get ws :agent-ready))
        (ws-loaded (agent-repl--ws-get ws :ws-loaded))
        (log-ws (agent-repl--ws-log-name ws)))
    (agent-repl--log log-ws "ws-fully-loaded: latch-set ws=%s key=%s marker=%S agent-ready=%S ws-loaded=%S"
                      ws key marker agent-ready ws-loaded)
    (if (and agent-ready ws-loaded)
        (progn
          (agent-repl--ws-put ws :agent-ready nil)
          (agent-repl--ws-put ws :ws-loaded nil)
          (agent-repl--log log-ws "ws-fully-loaded: firing ws=%s marker=%S hook-count=%d"
                            ws marker (length agent-repl-ws-fully-loaded-functions))
          (run-hook-wrapped 'agent-repl-ws-fully-loaded-functions
                            (lambda (fn ws marker)
                              (let ((log-ws (agent-repl--ws-log-name ws)))
                                (condition-case err
                                    (progn
                                      (agent-repl--log log-ws "ws-fully-loaded-hook: ws=%s invoke fn=%S marker=%S"
                                                        ws fn marker)
                                      (funcall fn ws marker)
                                      (agent-repl--log log-ws "ws-fully-loaded-hook: ws=%s completed fn=%S"
                                                        ws fn))
                                  (error
                                   (agent-repl--warn log-ws
                                                     "ws-fully-loaded-hook: ws=%s failed fn=%S err=%S"
                                                     ws fn err))))
                              nil)
                            ws marker))
      (agent-repl--log log-ws "ws-fully-loaded: waiting ws=%s key=%s agent-ready=%S ws-loaded=%S"
                        ws key agent-ready ws-loaded))))

;;; Event dispatch

(defconst agent-repl--deprecated-sentinel-prefixes
  '("login_request_"
    ;; Retired STATUS channels (agent-shim cutover, design §10).  Their
    ;; managed Claude Code hooks are removed from `install.el', so these
    ;; sentinels should never be written again — but a stale `~/.claude'
    ;; hook install or an older shim could still emit them, so drain them
    ;; rather than let the dispatch path re-warn "no handler" every poll.
    ;; `stop_' covers `stop_failure_'; `subagent_' covers both subagent
    ;; variants.
    "stop_"
    "subagent_"
    "prompt_submit_"
    "session_start_"
    ;; Retired NON-status daemon channels (S8/S9 sentinel endgame).  The
    ;; permission UX, session death, and account identity are all driven by
    ;; pushed `frontend.v1' state now, so these are drained too.  Each of
    ;; the three permission_* prefixes is listed explicitly (rather than a
    ;; bare `permission_') so the retirement of each channel is auditable.
    "permission_request"
    "permission_prompt"
    "permission_resolved"
    "session_dead_"
    "account_changed_")
  "Filename prefixes for sentinel channels Emacs no longer acts on.
A file matching one of these is DRAINED (deleted and dropped) rather than
warned about: there is no live handler and no side effect to run, only
cleanup.

The reason draining matters: an unrecognized sentinel file is never deleted
by the dispatch path, so `agent-repl--poll-workspace-notifications' re-detects
it every poll cycle and re-warns indefinitely.  A retired channel that a
stale daemon binary or older shim can still emit would therefore spam the
log forever.  Draining deletes the file on first sight so the poll fallback
stops seeing it.

`login_request_' was retired by the commit \"Emacs is out of the login
path\": the daemon now owns its own pty and runs `claude /login' itself,
streaming the terminal to the webapp, so Emacs has no reason to act on the
request.  Truly-unknown prefixes are NOT listed here — they still warn and
are left on disk, since a not-yet-reloaded handler may be able to process
them (forward compatibility).")

(defun agent-repl--dispatch-sentinel-file (file)
  "Drain FILE when it is a retired sentinel channel, else warn.
There are NO sentinel handlers left: Emacs acts on no sentinel file at
all, so this is a pure drain.  A retired prefix (see
`agent-repl--deprecated-sentinel-prefixes') is deleted on sight so the
poll fallback stops re-detecting it; a truly unknown prefix warns and is
left on disk for forward compatibility.  Returns non-nil when the file
was drained."
  (let ((name (file-name-nondirectory file)))
    (cond
     ((cl-some (lambda (prefix) (string-prefix-p prefix name))
               agent-repl--deprecated-sentinel-prefixes)
      (agent-repl--delete-sentinel-file file nil)
      (agent-repl--log nil "dispatch-sentinel-file: drained deprecated sentinel file=%s" name)
      t)
     (t
      (agent-repl--warn nil "no handler for sentinel file %s" name)
      (agent-repl--log nil "dispatch-sentinel-file: NO HANDLER for file=%s" name)
      nil))))

(defun agent-repl--dispatch-sentinel-event (event)
  "Handle file-notify EVENT for workspace notification sentinel files.
Drains by filename via `agent-repl--dispatch-sentinel-file'.
Skips files that no longer exist (file-notify often fires multiple events
for a single file creation; the first handler deletes the file).
Ignores events whose file is nil (e.g. `stopped' events fired when a
watch is removed) and events on the hook debug log, which is pure
noise — same filter as the poll path."
  (let* ((descriptor (nth 0 event))
         (action     (nth 1 event))
         (file       (nth 2 event))
         (fname      (and (stringp file) (file-name-nondirectory file))))
    (cond
     ((not (stringp file))
      (agent-repl--log-verbose nil ">>> SENTINEL EVENT SKIPPED: action=%s no file in event=%S"
                        action event))
     ((string= fname agent-repl-sentinel-debug-log-filename)
      (agent-repl--log-verbose nil ">>> SENTINEL EVENT SKIPPED: action=%s hook-debug-log=%s descriptor=%S"
                                action fname descriptor))
     (t
      (let ((exists (file-exists-p file)))
        (agent-repl--log nil ">>> SENTINEL EVENT: action=%s file=%s exists=%s descriptor=%S event=%S"
                          action fname exists descriptor event)
        (if (and (memq action agent-repl-sentinel-dispatch-actions) exists)
            (let ((result (agent-repl--dispatch-sentinel-file file)))
              (agent-repl--log nil ">>> SENTINEL EVENT DONE: file=%s dispatched=%s" fname result))
          (agent-repl--log-verbose nil ">>> SENTINEL EVENT SKIPPED: action=%s file=%s exists=%s (need created/changed + exists)"
                            action fname exists)))))))

;;; Polling fallback

(defun agent-repl--poll-workspace-notifications ()
  "Scan the sentinel directory for files that file-notify may have missed.
Called periodically as a fallback; any file still present was not picked up
by the file-notify watcher and needs processing."
  (let* ((dir-exists (file-directory-p agent-repl--sentinel-dir))
         (files (if dir-exists
                    (directory-files agent-repl--sentinel-dir t agent-repl-sentinel-poll-file-regexp t)
                  nil))
         ;; Filter out the debug log itself
         (files (cl-remove-if (lambda (f) (string= (file-name-nondirectory f) agent-repl-sentinel-debug-log-filename)) files)))
    ;; This runs on the 1Hz timer path, so routine scans stay verbose-only.
    (agent-repl--log-verbose nil "poll-notifications: scan dir=%s exists=%S candidate-count=%d"
                              agent-repl--sentinel-dir dir-exists (length files))
    (when files
      (agent-repl--log nil "poll-notifications: found %d orphaned file(s): %s"
                        (length files)
                        (mapconcat #'file-name-nondirectory files ", "))
      (dolist (file files)
        (if (file-exists-p file)
            (progn
              (agent-repl--log nil "poll-notifications: processing orphan file=%s" (file-name-nondirectory file))
              (unless (agent-repl--dispatch-sentinel-file file)
                (agent-repl--log nil "poll-notifications: ignoring unknown file %s"
                                  (file-name-nondirectory file))))
          (agent-repl--log nil "poll-notifications: file disappeared before processing: %s"
                            (file-name-nondirectory file)))))))

;; ---------------------------------------------------------------------------
;; File-notify watcher registration (top-level side effect)
;; ---------------------------------------------------------------------------

(defvar agent-repl--sentinel-watch-descriptor nil
  "File-notify descriptor for the sentinel directory watcher.
Stored so we can remove the old watcher before registering a new one
when sentinel.el is reloaded.")

(defun agent-repl--reap-sentinel-watchers ()
  "Remove every file-notify watcher on `agent-repl--sentinel-dir'.
Returns the count removed.  Iterates `file-notify-descriptors' rather
than relying on `agent-repl--sentinel-watch-descriptor', so it also
reclaims descriptors leaked across module reloads where that variable
lost track of the old descriptor."
  (let ((target (file-truename agent-repl--sentinel-dir))
        (removed 0))
    (agent-repl--log nil "reap-sentinel-watchers: begin target=%s" target)
    (maphash
     (lambda (desc watch)
       (let ((watch-dir (cond
                         ((and (fboundp 'file-notify--watch-p)
                               (file-notify--watch-p watch))
                          (file-notify--watch-directory watch))
                         ((consp watch) (car watch)))))
         (agent-repl--log-verbose nil "reap-sentinel-watchers: inspect descriptor=%S watch-dir=%S target=%S"
                                   desc watch-dir target)
         (when (and watch-dir (string= (file-truename watch-dir) target))
           (file-notify-rm-watch desc)
           (cl-incf removed)
           (agent-repl--log nil "reap-sentinel-watchers: removed descriptor=%S watch-dir=%S"
                             desc watch-dir))))
     file-notify-descriptors)
    (agent-repl--log nil "reap-sentinel-watchers: complete target=%s removed=%d" target removed)
    removed))

(defun agent-repl-reset-sentinel-watchers ()
  "Remove all file-notify watchers on the sentinel dir and re-register one.
Interactive recovery for the reload-accumulated duplicate-watcher case."
  (interactive)
  (let ((removed (agent-repl--reap-sentinel-watchers)))
    (setq agent-repl--sentinel-watch-descriptor
          (file-notify-add-watch agent-repl--sentinel-dir agent-repl-sentinel-watch-events
                                 #'agent-repl--dispatch-sentinel-event))
    (agent-repl--log nil "reset-sentinel-watchers: removed=%d dir=%s events=%S descriptor=%S"
                      removed agent-repl--sentinel-dir agent-repl-sentinel-watch-events
                      agent-repl--sentinel-watch-descriptor)
    (message "agent-repl: removed %d stale watcher(s); new descriptor=%S"
             removed agent-repl--sentinel-watch-descriptor)))

(defun agent-repl-nuke-sentinel-watchers ()
  "Remove every file-notify watcher on the sentinel dir WITHOUT re-registering.
Intended for testing: after nuking, re-eval sentinel.el (or just the
top-level init block) and confirm that exactly one watcher is created.
Useful to verify the init-time reap logic works without restarting Emacs."
  (interactive)
  (let ((removed (agent-repl--reap-sentinel-watchers)))
    (setq agent-repl--sentinel-watch-descriptor nil)
    (agent-repl--log nil "nuke-sentinel-watchers: removed=%d dir=%s descriptor-cleared=t"
                      removed agent-repl--sentinel-dir)
    (message "agent-repl: nuked %d sentinel watcher(s) — no replacement registered"
             removed)))

(agent-repl--log nil "sentinel-init: ensure-directory dir=%s" agent-repl--sentinel-dir)
(make-directory agent-repl--sentinel-dir t)
(agent-repl--log nil "sentinel-init: directory-ready dir=%s exists=%S"
                  agent-repl--sentinel-dir (file-directory-p agent-repl--sentinel-dir))
(let ((reaped (agent-repl--reap-sentinel-watchers)))
  (when (> reaped 0)
    (agent-repl--log nil "sentinel-init: reaped %d stale watcher(s)" reaped)))
(setq agent-repl--sentinel-watch-descriptor
      (file-notify-add-watch agent-repl--sentinel-dir agent-repl-sentinel-watch-events
                             #'agent-repl--dispatch-sentinel-event))
(agent-repl--log nil "sentinel-init: registered watcher descriptor=%S"
                  agent-repl--sentinel-watch-descriptor)
