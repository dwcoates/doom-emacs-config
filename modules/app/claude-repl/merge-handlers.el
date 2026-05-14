;;; merge-handlers.el --- Repo-routed /workspace-merge dispatch -*- lexical-binding: t; -*-

;;; Commentary:

;; Pluggable post-processing for `/workspace-merge' command-file
;; dispatch.  The skill's JSON contract is uniform — `{"type":"merge",
;; "workspace":"..."}` — but what the editor does on receipt is keyed
;; by the target workspace's repo via a small registry of named
;; handlers.
;;
;; Lookup order at dispatch time:
;;   1. `<repo-root>/.claude-repl/workspace-merge.eld' — repo-checked-in
;;      data file naming a registered handler symbol plus optional args.
;;      Read with `read', NEVER evaluated, so repo content cannot inject
;;      arbitrary code into Emacs.
;;   2. `claude-repl-workspace-merge-handler-overrides' — user-side
;;      defcustom alist keyed by canonical repo-root path.  Acts as the
;;      fallback for repos that haven't opted in via the .eld file.
;;   3. `cherry-pick' — the default handler, preserving the pre-routing
;;      behaviour of `claude-repl--handle-merge-command'.
;;
;; Each handler function has signature `(TARGET-WS &optional ARGS)' and
;; is responsible for performing the post-merge work and recording
;; terminal state via the shared helpers in worktree.el
;; (`--mark-merge-failed', `--close-workspace', drawer refresh, etc.).

;;; Code:

(require 'cl-lib)

(defvar claude-repl--merge-handler-registry nil
  "Alist mapping handler symbol → handler function.
Each function is called with `(TARGET-WS &optional ARGS)' where
TARGET-WS is the bare workspace name and ARGS is the optional plist
read from the repo's `.claude-repl/workspace-merge.eld' file (or
declared in `claude-repl-workspace-merge-handler-overrides').

Entries are registered with
`claude-repl--register-merge-handler'.  The `cherry-pick' handler is
seeded by this file; additional handlers (e.g. `create-pr', `noop')
may register themselves in their own files.")

(defun claude-repl--register-merge-handler (symbol fn)
  "Register FN under SYMBOL in `claude-repl--merge-handler-registry'.
Replaces any prior binding for SYMBOL so reloads pick up the new
definition without leaving stale function references behind."
  (setq claude-repl--merge-handler-registry
        (cons (cons symbol fn)
              (assq-delete-all symbol claude-repl--merge-handler-registry))))

(defcustom claude-repl-workspace-merge-handler-overrides nil
  "User-side fallback merge handler config, keyed by repo root path.
Each entry is `(REPO-ROOT . CONFIG)' where CONFIG is an alist with
keys `handler' (registered symbol) and optional `args' (plist passed
to the handler).  Path matching is canonical (`file-truename' +
`directory-file-name'), so trailing slashes and tilde expansion are
normalised.

Consulted only when the repo itself does not provide
`.claude-repl/workspace-merge.eld' — the repo-local file always wins."
  :type '(alist :key-type directory
                :value-type (alist :key-type symbol :value-type sexp))
  :group 'claude-repl)

(defconst claude-repl--merge-handler-config-file
  ".claude-repl/workspace-merge.eld"
  "Repo-relative path that declares a workspace-merge handler.
Format: a single alist sexp like
  ((handler . create-pr)
   (args . (:add-to-merge-queue t :skip-tests t))).
Read with `read', never `eval'.")

(defun claude-repl--read-merge-handler-config-file (repo-root)
  "Return the parsed handler-config alist from REPO-ROOT, or nil.
REPO-ROOT is a directory; the file
`claude-repl--merge-handler-config-file' is read from inside it.

Safe by construction: uses `read', not `eval'.  Returns nil on
missing file, IO error, or non-alist content (each case logged so
misconfigurations are debuggable)."
  (when (and repo-root (file-directory-p repo-root))
    (let ((path (expand-file-name claude-repl--merge-handler-config-file
                                  repo-root)))
      (when (file-readable-p path)
        (condition-case err
            (with-temp-buffer
              (insert-file-contents path)
              (goto-char (point-min))
              (let ((data (read (current-buffer))))
                (cond
                 ((consp data) data)
                 (t
                  (when (fboundp 'claude-repl--log)
                    (claude-repl--log
                     nil
                     "merge-handler-config: %s: not an alist (%S), ignoring"
                     path data))
                  nil))))
          (error
           (when (fboundp 'claude-repl--log)
             (claude-repl--log
              nil
              "merge-handler-config: failed to read %s: %S"
              path err))
           nil))))))

(defun claude-repl--lookup-merge-handler-override (repo-root)
  "Return the override config alist for REPO-ROOT, or nil.
Walks `claude-repl-workspace-merge-handler-overrides' and matches
entries by canonical path (`claude-repl--path-canonical')."
  (when repo-root
    (let ((canon (claude-repl--path-canonical repo-root)))
      (cl-loop for (root . config)
               in claude-repl-workspace-merge-handler-overrides
               when (and root (stringp root)
                         (string= canon
                                  (claude-repl--path-canonical root)))
               return config))))

(defun claude-repl--resolve-merge-handler (repo-root)
  "Resolve REPO-ROOT to a `(SYMBOL . ARGS)' handler descriptor.

Lookup order:
  1. REPO-ROOT's `.claude-repl/workspace-merge.eld' file.
  2. `claude-repl-workspace-merge-handler-overrides' user alist.
  3. Fallback to `cherry-pick'.

If a config names an unknown handler symbol, falls back to
`cherry-pick' with a logged warning so a typo in the data file
cannot wedge merge dispatch."
  (let* ((config (or (claude-repl--read-merge-handler-config-file repo-root)
                     (claude-repl--lookup-merge-handler-override repo-root)))
         (raw-symbol (and (consp config) (alist-get 'handler config)))
         (args (and (consp config) (alist-get 'args config)))
         (known (and raw-symbol
                     (assq raw-symbol
                           claude-repl--merge-handler-registry)))
         (symbol (cond
                  (known raw-symbol)
                  (raw-symbol
                   (when (fboundp 'claude-repl--log)
                     (claude-repl--log
                      nil
                      "resolve-merge-handler: unknown handler %S for repo=%s — falling back to cherry-pick"
                      raw-symbol repo-root))
                   'cherry-pick)
                  (t 'cherry-pick))))
    (cons symbol args)))

(defun claude-repl--dispatch-merge-handler (target-ws repo-root)
  "Resolve and invoke the merge handler for TARGET-WS.
REPO-ROOT is the directory used to locate the repo-local handler
config; it is the workspace's `:source-ws-dir' when recorded, else
its `:project-dir'.  Both point at the same repo (different
worktrees), so the .eld content is consistent either way.

Logs the resolved handler + args before invoking so failure modes
are easy to trace.  Signals `user-error' if the resolved symbol has
no entry in the registry — defensive: the resolver guarantees a
valid symbol, but an unloaded handler file could leave the registry
short an entry."
  (let* ((descriptor (claude-repl--resolve-merge-handler repo-root))
         (symbol (car descriptor))
         (args (cdr descriptor))
         (entry (assq symbol claude-repl--merge-handler-registry))
         (fn (and entry (cdr entry))))
    (when (fboundp 'claude-repl--log)
      (claude-repl--log target-ws
                        "dispatch-merge-handler: ws=%s repo-root=%s handler=%S args=%S"
                        target-ws (or repo-root "nil") symbol args))
    (unless fn
      (user-error "No merge handler registered for symbol '%s'" symbol))
    (funcall fn target-ws args)))

;;; Built-in handlers

(defun claude-repl--merge-handler-cherry-pick (target-ws &optional _args)
  "Cherry-pick TARGET-WS's commits into its source workspace.
Default handler — wraps `claude-repl--workspace-merge-into-source'
with the same silent + auto-resolve semantics that
`claude-repl--handle-merge-command' has always used for skill-invoked
merges.  Ignores ARGS (none defined for this handler)."
  (claude-repl--workspace-merge-into-source target-ws t t))

(claude-repl--register-merge-handler 'cherry-pick
                                     #'claude-repl--merge-handler-cherry-pick)

(provide 'merge-handlers)

;;; merge-handlers.el ends here
