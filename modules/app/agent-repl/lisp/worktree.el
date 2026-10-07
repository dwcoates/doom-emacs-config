;;; worktree.el --- the editor-local remnants of worktree handling -*- lexical-binding: t; -*-

;;; Commentary:

;; WHAT IS LEFT, AND WHY SO LITTLE.  Workspace creation, naming, branch and
;; worktree management, close, finish and the whole merge machinery are the
;; DAEMON's now: `CreateWorkspace' names and creates everything,
;; `CloseWorkspace' / `KillWorkspace' / `NukeWorkspace' / `MergeWorkspace'
;; own the rest, and `verbs.el' is the thin wrapper that calls them.  Emacs
;; runs no `git worktree add', names no branch, preflights no collision and
;; holds no merge state.
;;
;; This file therefore keeps only what has NO wire successor and is
;; deliberately blessed as editor-local:
;;
;;   THE EVAL HELPERS -- `agent-repl--eval-code-string' and its formatter,
;;   reached over `emacsclient' by the /runtime-eval-code skill.  Debugging
;;   the editor depends on being able to evaluate inside it, and no rpc
;;   replaces that.
;;
;;   THE BRANCH READERS -- `agent-repl--workspace-branch' and
;;   `agent-repl--git-branch-of-dir', which answer "what branch is this
;;   worktree on" for local display.  Read-only, and never an identity: a
;;   workspace's name is not its branch name.
;;
;;   THE ASYNC GIT BOUNDARY -- `agent-repl--async-git', the one wrapper the
;;   surviving `git fetch origin' before a rebase prompt goes through, and
;;   an external boundary registered in core.el.
;;
;;   THE WORKSPACE-DIRECTORY REVERSE LOOKUP and the two buffer helpers
;;   panels.el and core.el call into.
;;
;; Everything else that used to live here -- every creation flavor, the
;; headless name-generation spawn, the one-shot suffix composition, the
;; gns-sockets close round-trip, the per-workspace clipboard, the PGN board,
;; the profiler dump, the main-thread heartbeat, the merge detection and
;; cherry-pick machinery, and the JSON inbox command handlers -- is deleted
;; rather than adapted: each was a workaround for a contract that no longer
;; exists.

;;; Code:

;; Cross-file forward declarations.  These sources load in the dependency
;; order config.el establishes and resolve each other's calls at call time,
;; so the declarations below exist for the byte-compiler alone.
(declare-function agent-repl--with-deferred-quit "core")
(declare-function agent-repl--deferred-quit-arm-audit "agent-repl-core" (context))
(declare-function agent-repl--deferred-quit-hand-off "agent-repl-core" (context))

(require 'cl-lib)
(require 'subr-x)

(declare-function agent-repl--log "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--info "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--warn "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--error "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--git-string "agent-repl-core" (&rest args))
(declare-function agent-repl--path-canonical "agent-repl-core" (path))
(declare-function agent-repl--ws-get "agent-repl-workspace" (ws key))
(declare-function agent-repl--ws-add-buffer "agent-repl-workspace" (buf persp &optional no-display))
(declare-function agent-repl--ws-remove-buffer "agent-repl-workspace" (buffer))
(declare-function agent-repl--ws-resolve-persp "agent-repl-workspace" (ws))
(declare-function agent-repl--ws-current-log-name "agent-repl-workspace" ())
(declare-function agent-repl--live-ws-names "agent-repl-workspace" ())

;;;; ---- Initial buffers and the dashboard --------------------------------
;;
;; Both are editor-local presentation, called by panels.el when a workspace
;; opens; neither has anything to do with creating a worktree.

(defcustom agent-repl-workspace-initial-buffers nil
  "Alist mapping repo path patterns to files opened when a workspace opens.
Each entry is (PATTERN . FILES) where PATTERN is a regexp matched against
the worktree path with `string-match-p', and FILES is a list of paths
relative to the worktree root.  Files are added to the workspace's
perspective without being displayed.  A missing file is logged and does
not abort anything -- the configuration naming it may simply be for a
sibling repo."
  :type '(alist :key-type regexp :value-type (repeat string))
  :group 'agent-repl)

(defun agent-repl--open-initial-buffers (ws path)
  "Open the configured initial buffers for workspace WS rooted at PATH."
  (agent-repl--log ws "elisp.worktree.initial-buffers ws=%s path=%s" ws path)
  (if-let ((persp (agent-repl--ws-resolve-persp ws)))
      (let ((matched nil))
        (dolist (entry agent-repl-workspace-initial-buffers)
          (when (string-match-p (car entry) path)
            (setq matched t)
            (dolist (relpath (cdr entry))
              (let ((fullpath (expand-file-name relpath path)))
                (if (file-exists-p fullpath)
                    (progn
                      (agent-repl--log ws "elisp.worktree.initial-buffer-open file=%s" fullpath)
                      (agent-repl--ws-add-buffer (find-file-noselect fullpath) persp t))
                  (agent-repl--log ws "elisp.worktree.initial-buffer-missing file=%s" fullpath))))))
        (unless matched
          (agent-repl--log ws "elisp.worktree.initial-buffers-unmatched path=%s" path)))
    (agent-repl--log ws "elisp.worktree.initial-buffers-no-persp ws=%s path=%s" ws path)))

(defun agent-repl--remove-doom-dashboard ()
  "Remove the Doom dashboard buffer from the current workspace.
Called after `magit-status' opens so magit is the sole main buffer in a
new workspace, rather than the splash screen lingering in the buffer
list."
  (when (boundp '+doom-dashboard-buffer-name)
    (when-let ((dash (get-buffer +doom-dashboard-buffer-name)))
      (agent-repl--log (agent-repl--ws-current-log-name)
                       "elisp.worktree.remove-dashboard buffer=%s" (buffer-name dash))
      (ignore-errors (agent-repl--ws-remove-buffer dash)))))

;;;; ---- Thread-safe buffer teardown --------------------------------------

(defun agent-repl--kill-buffer-safely (buf)
  "Kill BUF, tolerating a buffer already dead or never created.
Called from core.el's own teardown paths as well as from the async-git
settle below, which is why it lives at this layer rather than inside
either caller.

DETACHES BUF's PROCESS FIRST, and that detachment is the whole point.
`kill-buffer' runs `kill_buffer_processes', which `delete-process's every
process owning BUF, which delivers a NEW terminal status to that
process's SENTINEL — from inside the kill.  A sentinel that reaches this
helper (every one of ours does: the async-git settle, the async-gh
completion, the prompt-summary settle) therefore re-enters itself, kills
the same still-live buffer again, and recurses until Emacs is wedged with
no Lisp error and no log line, because the recursion happens BEFORE the
sentinel's own logging.  That is the `SPC .' hang: an unbounded
Fkill_buffer -> Fdelete_process -> exec_sentinel -> Fkill_buffer stack.

Clearing the sentinel and the filter makes that recursion UNREPRESENTABLE
rather than merely guarded against: the re-delivered status has nowhere
to go, for every caller of this helper at once, so no individual sentinel
has to remember to defend itself.  The query flag is cleared for the same
reason the detachment happens — the kill must not stop to ask."
  (when (and buf (buffer-live-p buf))
    (when-let ((proc (get-buffer-process buf)))
      (set-process-query-on-exit-flag proc nil)
      (set-process-sentinel proc #'ignore)
      (set-process-filter proc #'ignore))
    (let ((kill-buffer-query-functions nil))
      (kill-buffer buf))))

;;;; ---- The async git boundary -------------------------------------------

(defun agent-repl--async-git-settle (proc)
  "Collect PROC's result, kill its buffer and deliver it to the callback.
The quit-inhibited critical section of `agent-repl--async-git-sentinel',
named separately so the guard has a body to wrap and tests can drive a
settle directly."
  (let ((ok (zerop (process-exit-status proc)))
        (output (with-current-buffer (process-buffer proc)
                  (string-trim (buffer-string))))
        (callback (process-get proc 'agent-repl-callback))
        (log-ws (process-get proc 'agent-repl-log-workspace)))
    (unless log-ws
      (agent-repl--error
       '(:agent-repl-central "a malformed async git process has no captured workspace")
       "elisp.worktree.async-git-missing-log-scope proc=%s"
       (process-name proc))
      (error "agent-repl async git process %S has no log workspace"
             (process-name proc)))
    (agent-repl--log log-ws "elisp.worktree.async-git-settle proc=%s status=%s exit=%s"
                     (process-name proc) (process-status proc) (process-exit-status proc))
    (agent-repl--kill-buffer-safely (process-buffer proc))
    (funcall callback ok output)))

(defun agent-repl--async-git-sentinel (proc _event)
  "Process sentinel for `agent-repl--async-git'.

Runs under `agent-repl--with-deferred-quit'.  A `C-g' between reading the
output out of the process buffer, killing that buffer and invoking the
callback would leak the buffer and drop the only delivery of the git
result its caller is waiting on.  The quit is deferred to the command
loop instead -- deferred, never swallowed."
  (when (memq (process-status proc) '(exit signal))
    (agent-repl--with-deferred-quit "async-git-sentinel"
      (agent-repl--async-git-settle proc))))

(defun agent-repl--async-git (label git-root args callback)
  "Run git -C GIT-ROOT with ARGS asynchronously.
LABEL names the process and temp buffer.  CALLBACK is called with
\(SUCCESS-P OUTPUT) when the process exits.  This IS the
external-boundary wrapper -- tests mock it via `cl-letf' (see
`agent-repl--external-boundary-functions' in core.el)."
  (let* ((workspace (agent-repl--ws-name-for-dir git-root))
         (log-ws (if workspace
                     workspace
                   '(:agent-repl-central
                     "a git directory outside the roster has no workspace")))
         (buf (generate-new-buffer (format " *agent-repl-%s*" label))))
    (agent-repl--log log-ws "elisp.worktree.async-git label=%s git-root=%s args=%S"
                     label git-root args)
    (let ((proc (apply #'start-process ;; ALLOW-EXTERNAL-BOUNDARY
                       (format "agent-repl-%s" label)
                       buf
                       "git" "-C" git-root
                       args)))
      (process-put proc 'agent-repl-callback callback)
      (process-put proc 'agent-repl-log-workspace log-ws)
      (set-process-sentinel proc #'agent-repl--async-git-sentinel))))

;;;; ---- Branch readers ----------------------------------------------------
;;
;; READ-ONLY, and never an identity.  A workspace's name is not its branch
;; name (persp "fix-login" may sit on branch "ABC/fix-login"), and the
;; roster row's detail lines are the daemon's own account of the branch --
;; these exist for local display and for the print-branch commands.

(defun agent-repl--git-branch-of-dir (dir)
  "Return the abbreviated git branch checked out in DIR, or nil.
Thin wrapper over `git -C DIR rev-parse --abbrev-ref HEAD' that filters
the empty, `fatal' and detached-`HEAD' degenerate outputs down to nil."
  (when (and dir (file-directory-p dir))
    (let ((branch (agent-repl--git-string
                   "-C" dir "rev-parse" "--abbrev-ref" "HEAD")))
      (and branch
           (not (string-empty-p branch))
           (not (string-prefix-p "fatal" branch))
           (not (string= branch "HEAD"))
           branch))))

(defun agent-repl--workspace-branch (ws)
  "Return the git branch checked out in workspace WS's worktree, or nil.
A detached HEAD answers with the SHA instead: the caller asked what this
worktree is on, and \"HEAD\" would not answer it."
  (when-let* ((path (agent-repl--ws-get ws :project-dir))
              (branch (agent-repl--git-string
                       "-C" path "rev-parse" "--abbrev-ref" "HEAD"))
              (valid (not (or (string-empty-p branch)
                              (string-prefix-p "fatal" branch)))))
    (agent-repl--log ws "elisp.worktree.branch ws=%s path=%s branch=%s" ws path branch)
    (if (string= branch "HEAD")
        (let ((sha (agent-repl--git-string "-C" path "rev-parse" "HEAD")))
          (agent-repl--log ws "elisp.worktree.branch-detached ws=%s sha=%s" ws sha)
          sha)
      branch)))

;;;; ---- Workspace directory reverse lookup --------------------------------

(defun agent-repl--ws-name-for-dir (dir)
  "Return the LIVE workspace name whose `:project-dir' canonicalizes to DIR.
Reverse lookup over the workspace hash; first match wins, because
canonical paths are unique per live workspace by construction.
Tombstoned entries are skipped so a killed workspace's preserved
`:project-dir' cannot shadow a live workspace at the same path."
  (if (not dir)
      (progn
        (agent-repl--log '(:agent-repl-central "the lookup input names no workspace")
                         "elisp.worktree.ws-name-for-dir dir=nil result=nil")
        nil)
    (let* ((canon (agent-repl--path-canonical dir))
           (live-names (agent-repl--live-ws-names))
           (result
            (cl-find-if
             (lambda (ws)
               (let ((wd (agent-repl--ws-get ws :project-dir)))
                 (and wd (string= (agent-repl--path-canonical wd) canon))))
             live-names)))
      (agent-repl--log result
                       "elisp.worktree.ws-name-for-dir dir=%s canonical=%s live-count=%d result=%s"
                       dir canon (length live-names) (or result "nil"))
      result)))

;;;; ---- The eval helpers --------------------------------------------------
;;
;; REACHED OVER `emacsclient' by the /runtime-eval-code skill, which is the
;; only way to inspect or mutate live editor state without rewriting source.
;; It survives the overhaul deliberately: debugging depends on it, and no
;; rpc replaces it.  There is no longer a JSON inbox command that sends the
;; result back into a session -- the caller reads the returned plist.

(defcustom agent-repl-eval-output-max-chars 8000
  "Maximum characters of eval output a formatted result carries.
Anything longer is truncated with a `[truncated to N chars]' marker
appended, so a reader knows the output was clipped rather than silently
short.  Set to 0 to disable truncation entirely (not recommended -- a
runaway loop can otherwise dump megabytes)."
  :type 'integer
  :group 'agent-repl)

(defun agent-repl--eval-snippet (label code-string)
  "Return CODE-STRING under a `;; LABEL:' header line."
  (concat ";; " label ":\n" code-string "\n"))

(defun agent-repl--eval-truncate (text)
  "Truncate TEXT to `agent-repl-eval-output-max-chars'.
Returns TEXT unmodified when the cap is 0 or TEXT fits within it."
  (cond
   ((or (null text) (not (stringp text))) (or text ""))
   ((<= agent-repl-eval-output-max-chars 0) text)
   ((<= (length text) agent-repl-eval-output-max-chars) text)
   (t (concat (substring text 0 agent-repl-eval-output-max-chars)
              (format "\n;; [truncated to %d chars]"
                      agent-repl-eval-output-max-chars)))))

(defun agent-repl--eval-format-prompt (code-string note printed value-string error-string)
  "Format an eval result for a reader.
CODE-STRING is the raw source, NOTE an optional one-line label, PRINTED
the captured `standard-output', VALUE-STRING the printed return value (or
nil when an error fired) and ERROR-STRING the trapped error (or nil).

The format is deliberately enumerated and labeled so a reader can
pattern-match on `;; code:', `;; printed:', `;; result:' and `;; error:'
without ambiguity."
  (let* ((header (if error-string "Elisp eval ERROR" "Elisp eval result"))
         (note-suffix (if (and note (stringp note) (not (string-empty-p note)))
                          (format " (note: %s)" note)
                        ""))
         (sections (list (agent-repl--eval-snippet "code" code-string))))
    (when (and printed (not (string-empty-p printed)))
      (push (agent-repl--eval-snippet "printed" printed) sections))
    (if error-string
        (push (agent-repl--eval-snippet "error" error-string) sections)
      (push (agent-repl--eval-snippet "result" (or value-string "nil")) sections))
    (concat header note-suffix ":\n\n"
            "```elisp\n"
            (agent-repl--eval-truncate
             (mapconcat #'identity (nreverse sections) "\n"))
            "\n```")))

(defun agent-repl--eval-code-string (code-string)
  "Read every top-level form from CODE-STRING and evaluate them in order.
Returns the plist (:printed STRING :value-string STRING-OR-NIL :error
STRING-OR-NIL).

Captures `princ' / `print' output through a buffer-bound
`standard-output', so a `(princ ...)' side effect lands in `:printed'
rather than vanishing.  `message' writes to `*Messages*' directly and is
NOT captured -- callers that need to round-trip messages use `princ'.

The value is that of the LAST form.  A trapped error short-circuits the
remaining forms and populates `:error'; partial side effects from earlier
forms are still reported through `:printed', because pretending they did
not happen would be the more misleading answer."
  (let ((printed-buf (generate-new-buffer " *agent-repl-eval-output*"))
        (value-string nil)
        (error-string nil)
        (printed ""))
    (unwind-protect
        (progn
          (let ((standard-output printed-buf))
            (condition-case err
                (let ((pos 0)
                      (last-value nil)
                      (len (length code-string)))
                  (while (< pos len)
                    (let ((parsed (read-from-string code-string pos)))
                      (setq last-value (eval (car parsed) t))
                      (setq pos (cdr parsed))
                      ;; Skip whitespace between forms so the next read
                      ;; starts at the next form (or at EOF).
                      (while (and (< pos len)
                                  (memq (aref code-string pos) '(?\s ?\t ?\n ?\r)))
                        (setq pos (1+ pos)))))
                  (setq value-string (prin1-to-string last-value)))
              (end-of-file
               ;; Only fatal when nothing was successfully read -- a
               ;; whitespace-only code string yields nil with no error.
               (when (null value-string)
                 (setq value-string "nil")))
              (error
               (setq error-string (error-message-string err)))))
          (setq printed (with-current-buffer printed-buf (buffer-string))))
      (kill-buffer printed-buf))
    (agent-repl--log '(:agent-repl-central "runtime evaluation is process-wide")
                     "elisp.worktree.eval printed-len=%d errored=%s"
                     (length printed) (and error-string t))
    (list :printed printed
          :value-string value-string
          :error error-string)))

(provide 'worktree)

;;; worktree.el ends here
