;;; app/agent-repl/backend.el -*- lexical-binding: t; -*-
;;
;; Pluggable agent-CLI backend registry.
;;
;; agent-repl drives an interactive coding-agent CLI as a session's
;; agent backend.  Historically that CLI was hardwired to Claude Code
;; and driven inside a vterm panel; this file is the seam that makes
;; the CLI pluggable.  A backend bundles every CLI-specific decision
;; behind a small struct:
;;
;;   - how to build the interactive start command for a workspace
;;     (binary, resume/fork flags, permission flags, config-dir env)
;;
;; Further slots (headless one-shot runner, transcript locator, hook
;; registration) are added as the corresponding subsystems are threaded
;; through the seam.
;;
;; The claude backend is defined here and registered at load time; its
;; implementation functions live in session.el (they predate the seam).
;; A codex backend follows the same shape.
;;
;; Selection: `agent-repl-default-backend' names the default; a
;; workspace may override it via the `:backend' workspace property
;; (set at workspace creation).

(require 'cl-lib)

;;;; ---- Struct -------------------------------------------------------------

(cl-defstruct (agent-repl-backend
               (:constructor agent-repl-backend-create)
               (:copier nil))
  "One agent-CLI backend.

NAME is the identifying symbol (e.g. `claude', `codex').
BINARY is the CLI executable name (e.g. \"claude\").
START-CMD-FN builds the interactive start command: called with one
plist argument carrying `:session-id', `:fork-session-id',
`:project-dir' and `:model' (any of which may be nil except
`:project-dir'), it returns the full shell command string used to
launch the agent's interactive CLI session.
HEADLESS-CMD-FN builds the argv for a one-shot (non-interactive) run:
called with (MODEL EXTRA-ARGS), it returns the process command list
(the prompt is delivered on stdin by the caller).  Optional — a
backend with no headless mode leaves it nil, and
`agent-repl--backend-headless-cmd' errors if such a backend is asked
for a headless command.

The transcript slots feed the mode-line session segments (title /
model / context).  Each is optional: a nil slot means the backend
lacks that capability and the corresponding segment renders empty.
TRANSCRIPT-PATH-FN: (WS) -> absolute path of WS's live session
transcript, or nil when unresolvable (no session yet, file missing).
TRANSCRIPT-TITLE-FN: (PATH) -> most recent conversation title, or nil.
TRANSCRIPT-MODEL-FN: (PATH) -> most recent raw model id, or nil.
TRANSCRIPT-CONTEXT-FN: (PATH) -> most recent context-token total, or
nil."
  name
  binary
  start-cmd-fn
  headless-cmd-fn
  transcript-path-fn
  transcript-title-fn
  transcript-context-fn)

;;;; ---- Registry -----------------------------------------------------------

(defvar agent-repl--backends (make-hash-table :test #'eq)
  "Registry of known backends, keyed by `agent-repl-backend-name' symbol.")

(defun agent-repl-register-backend (backend)
  "Register BACKEND in `agent-repl--backends', replacing any same-named one.
Signals an error when BACKEND is not an `agent-repl-backend' struct or
when any required slot is missing — a partially-defined backend is a
bug, not a configuration to cope with."
  (unless (agent-repl-backend-p backend)
    (agent-repl--log nil "register-backend: rejected non-struct backend=%S" backend)
    (error "agent-repl-register-backend: not a backend struct: %S" backend))
  (dolist (slot '(name binary start-cmd-fn))
    (unless (funcall (intern (format "agent-repl-backend-%s" slot)) backend)
      (agent-repl--log nil "register-backend: rejected name=%S missing-required-slot=%s"
                       (agent-repl-backend-name backend) slot)
      (error "agent-repl-register-backend: backend %S is missing slot %s"
             (agent-repl-backend-name backend) slot)))
  (let* ((name (agent-repl-backend-name backend))
         (replaced (gethash name agent-repl--backends)))
    (puthash name backend agent-repl--backends)
    (agent-repl--log nil
                     "register-backend: name=%s binary=%s replaced=%s headless=%s transcript=%s"
                     name (agent-repl-backend-binary backend)
                     (if replaced "yes" "no")
                     (if (agent-repl-backend-headless-cmd-fn backend) "yes" "no")
                     (if (agent-repl-backend-transcript-path-fn backend) "yes" "no"))))

(defun agent-repl-backend-get (name &optional ws)
  "Return the registered backend named NAME (a symbol).
Signals an error when no such backend is registered — callers must
never silently fall back to a different CLI.  WS is optional diagnostic
context for a workspace-scoped lookup."
  (let ((backend (gethash name agent-repl--backends)))
    (if backend
        (progn
          ;; Backend lookup is reached from mode-line transcript readers.
          (agent-repl--log-verbose ws "backend-get: name=%s result=registered" name)
          backend)
      (agent-repl--log ws "backend-get: name=%s result=unregistered" name)
      (error "agent-repl-backend-get: no backend registered under `%s'" name))))

;;;; ---- Selection ----------------------------------------------------------

(defcustom agent-repl-default-backend 'claude
  "Name of the backend used for workspaces without a `:backend' override."
  :type 'symbol
  :group 'agent-repl)

(defun agent-repl--ws-backend-name (ws)
  "Return the backend name symbol for workspace WS.
The workspace's `:backend' property wins; otherwise
`agent-repl-default-backend'."
  (let ((override (agent-repl--ws-get ws :backend)))
    ;; This can be called on every mode-line redraw, so keep it verbose-only.
    (agent-repl--log-verbose ws "ws-backend-name: override=%s default=%s selected=%s"
                             (or override "none") agent-repl-default-backend
                             (or override agent-repl-default-backend))
    (or override agent-repl-default-backend)))

(defun agent-repl--ws-backend (ws)
  "Return the resolved backend struct for workspace WS.
Signals via `agent-repl-backend-get' when the named backend is not
registered."
  (let* ((name (agent-repl--ws-backend-name ws))
         (backend (agent-repl-backend-get name ws)))
    ;; This inherits the mode-line call frequency of `--ws-backend-name'.
    (agent-repl--log-verbose ws "ws-backend: name=%s binary=%s" name
                             (agent-repl-backend-binary backend))
    backend))

(defun agent-repl--default-backend ()
  "Return the resolved default backend struct.
Used by headless call sites with no workspace in scope (e.g. new-
workspace name generation, the config-explainer)."
  (let ((backend (agent-repl-backend-get agent-repl-default-backend)))
    (agent-repl--log nil "default-backend: name=%s binary=%s"
                     agent-repl-default-backend
                     (agent-repl-backend-binary backend))
    backend))

(defun agent-repl--backend-names ()
  "Return the list of registered backend name symbols."
  (let (names)
    (maphash (lambda (name _b) (push name names)) agent-repl--backends)
    (setq names (nreverse names))
    (agent-repl--log nil "backend-names: count=%d names=%S" (length names) names)
    names))

(defun agent-repl--capture-backend-session-ids (ws)
  "Return a plist snapshot of WS's current per-env session ids and fork pointer.
The plist carries one entry per `agent-repl--environment-keys' env (its
session id, or nil) plus `:fork-session-id'.  These live per-env ids
always belong to WS's CURRENT backend, so the snapshot is what gets
stashed under the outgoing backend on a switch.  Round-trips through
`agent-repl--apply-backend-session-ids'."
  (let (snapshot)
    (dolist (env agent-repl--environment-keys)
      (let ((inst (agent-repl--ws-get ws env)))
        (setq snapshot
              (plist-put snapshot env
                         (and (agent-repl-instantiation-p inst)
                              (agent-repl-instantiation-session-id inst))))))
    (setq snapshot (plist-put snapshot :fork-session-id
                              (agent-repl--ws-get ws :fork-session-id)))
    (agent-repl--log ws "capture-backend-session-ids: envs-with-session=%S fork-present=%s"
                     (cl-loop for env in agent-repl--environment-keys
                              when (plist-get snapshot env)
                              collect env)
                     (if (plist-get snapshot :fork-session-id) "yes" "no"))
    snapshot))

(defun agent-repl--apply-backend-session-ids (ws saved)
  "Restore per-env session ids and the fork pointer on WS from SAVED plist.
SAVED is a plist as produced by `agent-repl--capture-backend-session-ids',
or nil to clear every env's session id and the fork pointer (the
fresh-backend case, where the incoming backend has no stash yet).  Each
env's instantiation struct is updated in place; an env with no struct is
skipped."
  (dolist (env agent-repl--environment-keys)
    (let ((inst (agent-repl--ws-get ws env)))
      (when (agent-repl-instantiation-p inst)
        (setf (agent-repl-instantiation-session-id inst)
              (plist-get saved env)))))
  (agent-repl--ws-put ws :fork-session-id (plist-get saved :fork-session-id))
  (agent-repl--log ws "apply-backend-session-ids: saved=%s envs-with-session=%S fork-present=%s"
                   (if saved "present" "nil")
                   (cl-loop for env in agent-repl--environment-keys
                            when (plist-get saved env)
                            collect env)
                   (if (plist-get saved :fork-session-id) "yes" "no")))

(defun agent-repl--backend-session-ids-present-p (saved &optional ws)
  "Return non-nil when SAVED carries any non-empty session id or fork pointer.
SAVED is a captured/stashed session-id plist.  Used to decide whether a
backend switch restored resumable state (so the next start will
`--continue'/`resume') versus started fresh."
  (let ((present
         (cl-some (lambda (v) (and (stringp v) (> (length v) 0)))
                  (cons (plist-get saved :fork-session-id)
                        (mapcar (lambda (env) (plist-get saved env))
                                agent-repl--environment-keys)))))
    (agent-repl--log ws "backend-session-ids-present-p: saved=%s result=%s"
                     (if saved "present" "nil") (if present "present" "empty"))
    present))

(defun agent-repl--ws-switch-backend-session-ids (ws old-backend new-backend)
  "Stash OLD-BACKEND's session ids and restore NEW-BACKEND's for WS.
Session ids are BACKEND-SCOPED: a claude session UUID means nothing to
codex and vice versa, so the per-env ids and the fork pointer always
belong to whichever backend is currently selected.  On a switch this:

  - snapshots WS's live per-env session ids plus `:fork-session-id' and
    stashes them under OLD-BACKEND in the `:backend-session-stash'
    workspace plist, and
  - restores NEW-BACKEND's previously-stashed ids (or clears everything
    when NEW-BACKEND was never used), so switching BACK to a prior
    backend resumes its conversation instead of starting fresh.

Returns non-nil when NEW-BACKEND's restored ids were present (i.e. the
next start will resume rather than start a new session)."
  (let* ((stash (agent-repl--ws-get ws :backend-session-stash))
         (outgoing (agent-repl--capture-backend-session-ids ws))
         (incoming (plist-get stash new-backend)))
    (setq stash (plist-put stash old-backend outgoing))
    (agent-repl--ws-put ws :backend-session-stash stash)
    (agent-repl--apply-backend-session-ids ws incoming)
    (let ((restored (agent-repl--backend-session-ids-present-p incoming ws)))
      (agent-repl--log ws "ws-switch-backend-session-ids: ws=%s old=%s new=%s restored=%s"
                       ws old-backend new-backend (if restored "yes" "no"))
      restored)))

(defun agent-repl-select-backend (set-default)
  "Select the agent backend for the current workspace via completion.
With prefix argument SET-DEFAULT, set `agent-repl-default-backend' (for
subsequently created workspaces this session) instead of the current
workspace's `:backend' property.

Refuses to change a workspace whose agent is currently running — the
in-flight session belongs to the old CLI, and the change only takes
effect at the next agent start anyway.  The choice is persisted with
the workspace state, so a codex workspace resumes through codex after
an Emacs restart.

An actual backend CHANGE stashes the outgoing backend's session ids and
restores the incoming backend's previously-stashed ids (see
`agent-repl--ws-switch-backend-session-ids'): session ids are scoped to
the CLI that minted them, so the active per-env ids always belong to the
current backend, and switching BACK to a prior backend resumes its
conversation via `--continue'/`resume' instead of starting fresh."
  (interactive "P")
  (let* ((names (agent-repl--backend-names))
         (choice (intern (completing-read
                          (if set-default "Default backend: " "Workspace backend: ")
                          (mapcar #'symbol-name names) nil t))))
    (if set-default
        (let ((old-default agent-repl-default-backend))
          (setq agent-repl-default-backend choice)
          (agent-repl--log nil "select-backend: target=default old=%s new=%s changed=%s available=%S"
                           old-default choice (if (eq old-default choice) "no" "yes") names)
          (message "agent-repl: default backend -> %s" choice))
      (let ((ws (agent-repl--ws-current-name)))
        (unless ws
          (agent-repl--log nil "select-backend: target=workspace choice=%s result=no-current-workspace"
                           choice)
          (user-error "agent-repl-select-backend: no current workspace"))
        (when (agent-repl--agent-running-p ws)
          (agent-repl--log ws "select-backend: choice=%s result=refused-agent-running" choice)
          (user-error "agent-repl-select-backend: %s has a running agent — kill it first (the backend applies at the next start)" ws))
        (let* ((old (agent-repl--ws-backend-name ws))
               (changed (not (eq old choice)))
               (restored (when changed
                           (agent-repl--ws-switch-backend-session-ids ws old choice))))
          (agent-repl--ws-put ws :backend choice)
          (agent-repl--state-save ws)
          (agent-repl--log ws "select-backend: old=%s new=%s changed=%s restored=%s state-saved=yes"
                           old choice (if changed "yes" "no")
                           (if restored "yes" "no"))
          (message "agent-repl: %s backend -> %s%s" ws choice
                   (cond ((not changed) "")
                         (restored " (resumed prior session)")
                         (t " (session ids reset — new CLI, new session)"))))))))

;;;; ---- Headless command construction --------------------------------------

(defun agent-repl--backend-headless-cmd (backend model extra-args)
  "Return the argv for a one-shot headless run under BACKEND.
MODEL is the model alias to pin; EXTRA-ARGS is a list of additional
flags.  Delegates to BACKEND's HEADLESS-CMD-FN.  Signals an error when
BACKEND declares no headless command builder — headless mode is a
capability, and asking for it from a backend that lacks it is a bug,
not a condition to paper over."
  (let ((fn (agent-repl-backend-headless-cmd-fn backend)))
    (unless fn
      (agent-repl--log nil "backend-headless-cmd: backend=%s model=%S extra-args=%S result=unsupported"
                       (agent-repl-backend-name backend) model extra-args)
      (error "agent-repl--backend-headless-cmd: backend `%s' has no headless-cmd-fn"
             (agent-repl-backend-name backend)))
    (let ((command (funcall fn model extra-args)))
      (agent-repl--log nil "backend-headless-cmd: backend=%s model=%S extra-args=%S argv=%S"
                       (agent-repl-backend-name backend) model extra-args command)
      command)))

;;;; ---- Transcript access (mode-line session segments) ----------------------

;; Defined in ai-title.el (loaded after this file; resolved at call time).
(declare-function agent-repl--ai-title-mtime "ai-title" (path))

(defun agent-repl--ws-transcript-path (ws)
  "Return the absolute path of WS's live session transcript, or nil.
Resolves WS's backend and delegates to its TRANSCRIPT-PATH-FN.  Returns
nil when the backend has no transcript support or the path cannot be
resolved (no session yet, file missing)."
  (let ((fn (agent-repl-backend-transcript-path-fn (agent-repl--ws-backend ws))))
    (if fn
        (let ((path (funcall fn ws)))
          ;; Mode-line segments can ask on every redisplay.
          (agent-repl--log-verbose ws "ws-transcript-path: capability=yes path=%S" path)
          path)
      (agent-repl--log-verbose ws "ws-transcript-path: capability=no path=nil")
      nil)))

(defun agent-repl--transcript-read-tail (path scan-bytes)
  "Return the trailing SCAN-BYTES bytes of PATH as a string, or nil.
Reading only the tail keeps per-redraw transcript scans cheap on
multi-MB files.  Returns nil for a nil PATH, an unreadable file, or an
empty file — the segment readers all treat that as \"no value yet\"."
  ;; Transcript segments invoke this on redisplay; all diagnostics are verbose-only.
  (if (and path (file-readable-p path))
      (let* ((size (or (file-attribute-size (file-attributes path)) 0))
             (start (max 0 (- size scan-bytes))))
        (if (> size 0)
            (let ((tail (with-temp-buffer
                          (insert-file-contents path nil start size)
                          (buffer-string))))
              (agent-repl--log-verbose nil
                                       "transcript-read-tail: path=%S scan-bytes=%d size=%d start=%d result-bytes=%d"
                                       path scan-bytes size start (length tail))
              tail)
          (agent-repl--log-verbose nil
                                   "transcript-read-tail: path=%S scan-bytes=%d size=0 result=empty"
                                   path scan-bytes)
          nil))
    (agent-repl--log-verbose nil "transcript-read-tail: path=%S scan-bytes=%d result=unreadable"
                             path scan-bytes)
    nil))

(defun agent-repl--transcript-cached (ws cache-key slot-accessor)
  "Return a transcript-derived value for WS via an (mtime-keyed) cache.
SLOT-ACCESSOR is the backend-struct accessor for the reader slot (e.g.
`agent-repl-backend-transcript-title-fn'); the resolved reader is
called with the transcript path and its result cached on WS's plist
under CACHE-KEY as (PATH MTIME VALUE).  Returns nil (without caching)
when WS's backend lacks the capability, the path is unresolvable, or
the file has no mtime."
  (let* ((backend (agent-repl--ws-backend ws))
         (read-fn (funcall slot-accessor backend))
         (path (agent-repl--ws-transcript-path ws))
         (mtime (and path (agent-repl--ai-title-mtime path)))
         (cache (agent-repl--ws-get ws cache-key)))
    (cond
     ((null read-fn)
      ;; This helper runs in mode-line redisplay; retain the branches only in verbose traces.
      (agent-repl--log-verbose ws "transcript-cached: cache-key=%s backend=%s result=no-reader"
                               cache-key (agent-repl-backend-name backend))
      nil)
     ((null path)
      (agent-repl--log-verbose ws "transcript-cached: cache-key=%s backend=%s result=no-path"
                               cache-key (agent-repl-backend-name backend))
      nil)
     ((null mtime)
      (agent-repl--log-verbose ws "transcript-cached: cache-key=%s path=%S result=no-mtime"
                               cache-key path)
      nil)
     ((and (consp cache)
           (equal (nth 0 cache) path)
           (equal (nth 1 cache) mtime))
      (let ((value (nth 2 cache)))
        (agent-repl--log-verbose ws
                                 "transcript-cached: cache-key=%s path=%S mtime=%S result=cache-hit value-present=%s"
                                 cache-key path mtime (if value "yes" "no"))
        value))
     (t
      (let ((value (funcall read-fn path)))
        (agent-repl--ws-put ws cache-key (list path mtime value))
        (agent-repl--log-verbose ws
                                 "transcript-cached: cache-key=%s path=%S mtime=%S result=cache-miss value-present=%s"
                                 cache-key path mtime (if value "yes" "no"))
        value)))))

;;;; ---- Claude backend -----------------------------------------------------

;; Implementation functions live in session.el; symbols resolve at call
;; time, so load order between backend.el and session.el is free.

(agent-repl-register-backend
 (agent-repl-backend-create
  :name 'claude
  :binary "claude"
  :start-cmd-fn #'agent-repl--claude-start-cmd
  :headless-cmd-fn #'agent-repl--claude-headless-cmd
  ;; Transcript readers live in ai-title.el / context.el
  ;; (the mode-line segment files, which predate the backend seam).
  :transcript-path-fn #'agent-repl--ai-title-jsonl-path
  :transcript-title-fn #'agent-repl--ai-title-read-from-jsonl
  :transcript-context-fn #'agent-repl--context-read-from-jsonl))

(provide 'agent-repl-backend)
;;; backend.el ends here
