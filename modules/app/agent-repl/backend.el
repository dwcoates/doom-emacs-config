;;; app/agent-repl/backend.el -*- lexical-binding: t; -*-
;;
;; Pluggable agent-CLI backend registry.
;;
;; agent-repl drives an interactive coding-agent CLI inside a vterm
;; panel.  Historically that CLI was hardwired to Claude Code; this file
;; is the seam that makes the CLI pluggable.  A backend bundles every
;; CLI-specific decision behind a small struct:
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
launch the agent inside the workspace vterm.
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
  transcript-model-fn
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
    (error "agent-repl-register-backend: not a backend struct: %S" backend))
  (dolist (slot '(name binary start-cmd-fn))
    (unless (funcall (intern (format "agent-repl-backend-%s" slot)) backend)
      (error "agent-repl-register-backend: backend %S is missing slot %s"
             (agent-repl-backend-name backend) slot)))
  (puthash (agent-repl-backend-name backend) backend agent-repl--backends))

(defun agent-repl-backend-get (name)
  "Return the registered backend named NAME (a symbol).
Signals an error when no such backend is registered — callers must
never silently fall back to a different CLI."
  (or (gethash name agent-repl--backends)
      (error "agent-repl-backend-get: no backend registered under `%s'" name)))

;;;; ---- Selection ----------------------------------------------------------

(defcustom agent-repl-default-backend 'claude
  "Name of the backend used for workspaces without a `:backend' override."
  :type 'symbol
  :group 'agent-repl)

(defun agent-repl--ws-backend-name (ws)
  "Return the backend name symbol for workspace WS.
The workspace's `:backend' property wins; otherwise
`agent-repl-default-backend'."
  (or (agent-repl--ws-get ws :backend) agent-repl-default-backend))

(defun agent-repl--ws-backend (ws)
  "Return the resolved backend struct for workspace WS.
Signals via `agent-repl-backend-get' when the named backend is not
registered."
  (agent-repl-backend-get (agent-repl--ws-backend-name ws)))

(defun agent-repl--default-backend ()
  "Return the resolved default backend struct.
Used by headless call sites with no workspace in scope (e.g. new-
workspace name generation, the config-explainer)."
  (agent-repl-backend-get agent-repl-default-backend))

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
      (error "agent-repl--backend-headless-cmd: backend `%s' has no headless-cmd-fn"
             (agent-repl-backend-name backend)))
    (funcall fn model extra-args)))

;;;; ---- Transcript access (mode-line session segments) ----------------------

;; Defined in ai-title.el (loaded after this file; resolved at call time).
(declare-function agent-repl--ai-title-mtime "ai-title" (path))

(defun agent-repl--ws-transcript-path (ws)
  "Return the absolute path of WS's live session transcript, or nil.
Resolves WS's backend and delegates to its TRANSCRIPT-PATH-FN.  Returns
nil when the backend has no transcript support or the path cannot be
resolved (no session yet, file missing)."
  (let ((fn (agent-repl-backend-transcript-path-fn (agent-repl--ws-backend ws))))
    (and fn (funcall fn ws))))

(defun agent-repl--transcript-read-tail (path scan-bytes)
  "Return the trailing SCAN-BYTES bytes of PATH as a string, or nil.
Reading only the tail keeps per-redraw transcript scans cheap on
multi-MB files.  Returns nil for a nil PATH, an unreadable file, or an
empty file — the segment readers all treat that as \"no value yet\"."
  (when (and path (file-readable-p path))
    (let* ((size (or (file-attribute-size (file-attributes path)) 0))
           (start (max 0 (- size scan-bytes))))
      (when (> size 0)
        (with-temp-buffer
          (insert-file-contents path nil start size)
          (buffer-string))))))

(defun agent-repl--transcript-cached (ws cache-key slot-accessor)
  "Return a transcript-derived value for WS via an (mtime-keyed) cache.
SLOT-ACCESSOR is the backend-struct accessor for the reader slot (e.g.
`agent-repl-backend-transcript-model-fn'); the resolved reader is
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
     ((null read-fn) nil)
     ((null path) nil)
     ((null mtime) nil)
     ((and (consp cache)
           (equal (nth 0 cache) path)
           (equal (nth 1 cache) mtime))
      (nth 2 cache))
     (t
      (let ((value (funcall read-fn path)))
        (agent-repl--ws-put ws cache-key (list path mtime value))
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
  ;; Transcript readers live in ai-title.el / model.el / context.el
  ;; (the mode-line segment files, which predate the backend seam).
  :transcript-path-fn #'agent-repl--ai-title-jsonl-path
  :transcript-title-fn #'agent-repl--ai-title-read-from-jsonl
  :transcript-model-fn #'agent-repl--model-read-from-jsonl
  :transcript-context-fn #'agent-repl--context-read-from-jsonl))

(provide 'agent-repl-backend)
;;; backend.el ends here
