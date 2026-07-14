;;; history.el --- input history and session state persistence -*- lexical-binding: t; -*-

;;; Code:

;;;; Constants

(defconst agent-repl--environment-keys '(:bare-metal)
  "List of per-workspace environment keys.
Each workspace has one `agent-repl-instantiation' struct per environment.

The `:sandbox' environment was retired: it never actually sandboxed
anything (`agent-repl--build-start-cmd' always returned a hardcoded
`:sandboxed-p nil'), so it was a label rather than a container, and it
survives only as the migration in `agent-repl--migrate-saved-state'.
The axis is kept — rather than collapsed away — because it is still the
seam a real containerized environment would land on.")

;; These are defconsts (not defcustoms) so they update on every load — a
;; defcustom does NOT reset already-bound values, which leaves a running
;; Emacs writing to stale paths after a path-rename refactor.  They're
;; layout conventions, not user-tuning knobs; users wanting custom paths
;; can `setq' after load.

(defconst agent-repl-emacs-data-subdir ".claude/emacs"
  "Per-project subdirectory (relative to project root) for agent-repl data.
Files inside use plain (non-hidden) names since the parent directory
is already hidden via `.claude/'.  Auto-created on first write.")

(defconst agent-repl-history-filename "history.el"
  "Name of the per-project input history file (under
`agent-repl-emacs-data-subdir').")

(defconst agent-repl-state-filename "state.el"
  "Name of the per-project session state file (under
`agent-repl-emacs-data-subdir').")

(defconst agent-repl--legacy-history-filename ".agent-repl-history"
  "Pre-relocation history filename at project root.
Read-only fallback: when no file exists at the new location but a
legacy file exists at the project root, the reader uses it.  The
writer never targets this path — first save naturally migrates
to the new location.")

(defconst agent-repl--legacy-state-filename ".agent-repl-state"
  "Pre-relocation state filename at project root.
Same fallback semantics as `agent-repl--legacy-history-filename'.")

;;;; Error handling

(defmacro agent-repl--with-error-logging (label &rest body)
  "Execute BODY, logging any error with LABEL prefix.
Catches errors and displays a user-visible warning via
`agent-repl--warn', then logs the full error via `agent-repl--log'."
  (declare (indent 1) (debug (stringp body)))
  `(condition-case err
       (progn ,@body)
     (error
      (agent-repl--warn nil "%s error: %S" ,label err)
      (agent-repl--log nil (concat ,label " error: %S") err))))

;;;; File I/O helpers

(defun agent-repl--read-sexp-file (file)
  "Read and return the first sexp from FILE."
  (agent-repl--log nil "read-sexp-file: file=%s" file)
  (with-temp-buffer
    (insert-file-contents file)
    (read (current-buffer))))

(defun agent-repl--write-sexp-file (file data)
  "Write DATA as a sexp to FILE.
Creates FILE's parent directory if missing so the relocated `.claude/emacs/'
data dir is auto-provisioned on first save."
  (agent-repl--log nil "write-sexp-file: file=%s" file)
  (let ((dir (file-name-directory file)))
    (when (and dir (not (file-directory-p dir)))
      (make-directory dir t)))
  (with-temp-file file
    (prin1 data (current-buffer))))

(defun agent-repl--read-sexp-file-if-exists (file)
  "Read and return the first sexp from FILE, or nil if FILE does not exist."
  (if (file-exists-p file)
      (agent-repl--read-sexp-file file)
    (agent-repl--log nil "read-sexp-file-if-exists: file not found file=%s" file)
    nil))

;;;; Instantiation serialization

(defun agent-repl--instantiation-to-plist (inst)
  "Serialize instantiation INST to a plist for persistence.
Returns nil when INST is nil."
  (if inst
      `(:session-id ,(agent-repl-instantiation-session-id inst))
    (agent-repl--log nil "instantiation-to-plist: inst is nil, returning nil")
    nil))

(defun agent-repl--make-instantiation-from-plist (saved)
  "Create a new `agent-repl-instantiation' from SAVED plist.
Returns a fresh empty instantiation when SAVED is nil."
  (if saved
      (progn
        (agent-repl--log nil "make-instantiation-from-plist: session-id=%s"
                          (plist-get saved :session-id))
        (make-agent-repl-instantiation
         :session-id (plist-get saved :session-id)))
    (agent-repl--log nil "make-instantiation-from-plist: nil saved, creating empty")
    (make-agent-repl-instantiation)))

;;;; State migration

(defun agent-repl--migrate-saved-state (saved)
  "Return the SAVED state plist with retired values migrated forward.
Returns SAVED untouched when it is nil or carries neither retired value.
Never mutates SAVED.

Two persisted values were retired, and each one HARD-ERRORS on hydration
if it survives into the current schema:

`:active-env :sandbox' — the environment axis collapsed to `:bare-metal'
alone (`agent-repl--environment-keys'), so a saved `:sandbox' fails
`agent-repl--validate-ws-env'.  Relabeling it is NOT sufficient: the
workspace's claude session id lives in its `:sandbox' instantiation while
`:bare-metal' sits empty beside it, so the `:sandbox' instantiation is
PROMOTED into `:bare-metal' rather than dropped.  Relabeling without
promoting would strand every sandbox-era conversation on a nil session id,
silently destroying the one thing the state file exists to protect.

Promoting is sound because `:sandbox' never actually sandboxed anything:
`agent-repl--build-start-cmd' always returned a hardcoded `:sandboxed-p
nil' and `agent-repl--assemble-cmd' always launched plain `claude', so the
promoted session is the same host session it has been all along.

`:frontend vterm' — the vterm frontend is no longer registered, and
`agent-repl-frontend-get' signals on an unregistered name.  Dropping the
key (and its `:frontend-explicit' marker) lets the workspace re-resolve
through `agent-repl-default-frontend'."
  (if (null saved)
      saved
    (let ((out (copy-sequence saved)))
      (when (eq (plist-get out :active-env) :sandbox)
        (agent-repl--log nil "migrate-saved-state: :active-env :sandbox -> :bare-metal, promoting instantiation session-id=%s"
                         (plist-get (plist-get out :sandbox) :session-id))
        (setq out (plist-put out :bare-metal (plist-get out :sandbox)))
        (setq out (plist-put out :active-env :bare-metal)))
      (when (eq (plist-get out :frontend) 'vterm)
        (agent-repl--log nil "migrate-saved-state: dropping retired :frontend vterm")
        (setq out (plist-put out :frontend nil))
        (setq out (plist-put out :frontend-explicit nil)))
      out)))

;;;; Persistence file paths

(defun agent-repl--data-dir (root)
  "Return the absolute path to the agent-repl data directory under ROOT.
Used by writers to compose the destination path; readers use the
`-for-read' resolvers below to also consider the legacy project-root
locations."
  (when root
    (expand-file-name agent-repl-emacs-data-subdir root)))

(defun agent-repl--history-file (root)
  "Return the writer path for the history file under ROOT.
This is always the new location (`<root>/<emacs-data-subdir>/<history>'),
never the legacy file at the project root.  Use
`agent-repl--history-file-for-read' when reading."
  (when root
    (expand-file-name agent-repl-history-filename
                      (agent-repl--data-dir root))))

(defun agent-repl--state-file (root)
  "Return the writer path for the state file under ROOT, or nil if ROOT is nil.
Always the new location; use `agent-repl--state-file-for-read' for
reads (which falls back to the legacy project-root path)."
  (when root
    (expand-file-name agent-repl-state-filename
                      (agent-repl--data-dir root))))

(defun agent-repl--history-file-for-read (root)
  "Return the read path for the history file under ROOT.
Prefers the new location; falls back to the legacy project-root path
when the new file does not exist (so existing on-disk histories keep
working through the relocation transition)."
  (when root
    (let ((new (agent-repl--history-file root))
          (legacy (expand-file-name agent-repl--legacy-history-filename root)))
      (cond ((file-exists-p new) new)
            ((file-exists-p legacy) legacy)
            (t new)))))

(defun agent-repl--state-file-for-read (root)
  "Return the read path for the state file under ROOT.
Same fallback semantics as `agent-repl--history-file-for-read'."
  (when root
    (let ((new (agent-repl--state-file root))
          (legacy (expand-file-name agent-repl--legacy-state-filename root)))
      (cond ((file-exists-p new) new)
            ((file-exists-p legacy) legacy)
            (t new)))))

;;;; History persistence

(defun agent-repl--ws-live-input-buffer (ws)
  "Return the live input buffer for workspace WS, or nil.
WS defaults to the current workspace name."
  (let* ((ws (or ws (agent-repl--ws-current-name)))
         (buf (and ws (agent-repl--ws-get ws :input-buffer))))
    (if (and buf (buffer-live-p buf))
        buf
      (agent-repl--log ws "ws-live-input-buffer: no live input buffer ws=%s buf=%s" ws buf)
      nil)))

(defun agent-repl--history-save (&optional ws)
  "Write input history to disk for workspace WS.
Resolves the project root via `agent-repl--ws-dir' so the location
follows the workspace record rather than whatever buffer is current.
WS defaults to the current workspace name."
  (let ((ws (or ws (agent-repl--ws-current-name))))
    (agent-repl--log ws "history-save ws=%s" ws)
    (if-let ((buf (agent-repl--ws-live-input-buffer ws)))
        (let* ((root (agent-repl--ws-dir ws))
               (history (buffer-local-value 'agent-repl--input-history buf))
               (file (when root (agent-repl--history-file root))))
          (if history
              (agent-repl--write-sexp-file file history)
            (agent-repl--log ws "history-save: no history to save ws=%s" ws)))
      (agent-repl--log ws "history-save: no live input buffer ws=%s" ws))))

(defun agent-repl--history-restore (ws)
  "Load input history from disk into the current buffer for workspace WS.
Resolves the history file location via `agent-repl--ws-dir', preferring
the new `<root>/.claude/emacs/' location and falling back to the legacy
project-root path when only the legacy file exists."
  (agent-repl--log ws "history-restore ws=%s" ws)
  (let* ((root (ignore-errors (agent-repl--ws-dir ws)))
         (data (when root
                 (agent-repl--read-sexp-file-if-exists
                  (agent-repl--history-file-for-read root)))))
    (if data
        (setq agent-repl--input-history data)
      (agent-repl--log ws "history-restore: no data found, history unchanged"))))

;;;; Session state persistence

(defun agent-repl--collect-env-state (ws)
  "Return a plist of serialized instantiation state for each environment in WS.
Iterates over `agent-repl--environment-keys', converting each
environment's instantiation struct to a plist."
  (agent-repl--log ws "collect-env-state: ws=%s keys=%S" ws agent-repl--environment-keys)
  (cl-loop for key in agent-repl--environment-keys
           nconc (list key (agent-repl--instantiation-to-plist
                            (agent-repl--ws-get ws key)))))

(defun agent-repl--state-save (ws)
  "Persist session state for workspace WS to disk.
Saves session-id for each environment so it survives Emacs restarts.
Written to the per-project data dir (`<root>/.claude/emacs/state.el');
the input history goes alongside as `history.el'.

Also rewrites the workspace roster snapshot
(`agent-repl-workspace-snapshot-file') so the snapshot reflects the
current `:project-dir' / `:priority' for this and all live workspaces.
The snapshot used to save only at Emacs quit, which lost the roster on
crash; pairing it with `state-save' makes the roster crash-safe at the
same granularity as per-project state.

`:created-at' is set once — preferring the value already on the ws plist,
then the value persisted in the existing on-disk state file, then
`current-time' as a final fallback — so the first state-save for a
project stamps a creation date that subsequent saves preserve.
`:last-killed-at' is written through unchanged when set on the ws plist
\(nuke flows populate it before calling state-save), and falls back to
the previously-persisted value to avoid clobbering on stray saves that
do not represent a kill.

`:model' records the session's current model, resolved from the
workspace's Claude config dir via `agent-repl--model-persist-value' so
a mid-session `/model' switch (e.g. `opus' to `fable') survives restart;
`agent-repl--apply-display-state' restores it so the re-booted session
launches under the same model."
  (let* ((root (agent-repl--ws-get ws :project-dir))
         (file (agent-repl--state-file root)))
    (agent-repl--log ws "state-save ws=%s file=%s" ws file)
    (if (null file)
        (agent-repl--log ws "state-save: no :project-dir for ws=%s, skipping" ws)
      (let* ((existing (when (file-exists-p file)
                         (condition-case err
                             (agent-repl--read-sexp-file file)
                           (error
                            (agent-repl--log ws "state-save: existing read err err=%S" err)
                            nil))))
             (created-at (or (agent-repl--ws-get ws :created-at)
                             (plist-get existing :created-at)
                             (current-time)))
             (last-killed-at (or (agent-repl--ws-get ws :last-killed-at)
                                 (plist-get existing :last-killed-at)))
             ;; The session's *current* model, resolved from the workspace's
             ;; Claude config dir (its session jsonl) so a mid-session
             ;; `/model' switch survives restart; falls back to the
             ;; workspace-generation `:model' when no session model is
             ;; available yet.  `agent-repl--apply-display-state' restores
             ;; this onto `:model' so `agent-repl--build-start-cmd' passes
             ;; `--model' when re-booting the session.
             (model (if (fboundp 'agent-repl--model-persist-value)
                        (agent-repl--model-persist-value ws)
                      (agent-repl--ws-get ws :model))))
        (agent-repl--ws-put ws :created-at created-at)
        (when last-killed-at
          (agent-repl--ws-put ws :last-killed-at last-killed-at))
        (let ((state (append `(:project-dir ,root
                                :created-at ,created-at
                                :last-killed-at ,last-killed-at
                                :model ,model
                                :active-env ,(agent-repl--ws-get ws :active-env)
                                :priority ,(agent-repl--ws-get ws :priority)
                                :source-ws-dir ,(agent-repl--ws-get ws :source-ws-dir)
                                :repl-state ,(agent-repl--ws-get ws :repl-state)
                                :saved-tab-index ,(agent-repl--ws-get ws :saved-tab-index)
                                :backend ,(agent-repl--ws-get ws :backend)
                                :backend-session-stash ,(agent-repl--ws-get ws :backend-session-stash)
                                :config-dir-override ,(agent-repl--ws-get ws :config-dir-override)
                                :frontend ,(agent-repl--ws-get ws :frontend)
                                :frontend-explicit ,(agent-repl--ws-get ws :frontend-explicit)
                                :fork-session-id ,(agent-repl--ws-get ws :fork-session-id)
                                :last-prompt-summary ,(agent-repl--ws-get ws :last-prompt-summary)
                                :last-prompt-summary-at ,(agent-repl--ws-get ws :last-prompt-summary-at)
                                :last-prompt-time ,(agent-repl--ws-get ws :last-prompt-time)
                                :worktree-p ,(agent-repl--ws-get ws :worktree-p)
                                :merge-completed ,(agent-repl--ws-get ws :merge-completed)
                                :merge-completed-at ,(agent-repl--ws-get ws :merge-completed-at)
                                :merge-failed ,(agent-repl--ws-get ws :merge-failed))
                             (agent-repl--collect-env-state ws))))
          (agent-repl--with-error-logging "state-save"
            (agent-repl--write-sexp-file file state)
            (agent-repl--log ws "state-save: write complete ws=%s file=%s" ws file)))))
    (agent-repl--with-error-logging "state-save: snapshot"
      (when (fboundp 'agent-repl-save-workspace-snapshot)
        (agent-repl-save-workspace-snapshot)))))

(defun agent-repl--validate-ws-env (ws)
  "Validate that workspace WS has well-formed environment state.
Signals an error with a descriptive message when validation fails."
  (let ((active-env (agent-repl--ws-get ws :active-env)))
    (unless (memq active-env agent-repl--environment-keys)
      (error "agent-repl: ws %s has invalid :active-env %S (expected one of %S)"
             ws active-env agent-repl--environment-keys))
    (dolist (key agent-repl--environment-keys)
      (let ((inst (agent-repl--ws-get ws key)))
        (unless (agent-repl-instantiation-p inst)
          (error "agent-repl: ws %s missing instantiation struct for %s (got %S)"
                 ws key inst))
        (let ((sid (agent-repl-instantiation-session-id inst)))
          (unless (or (null sid) (stringp sid))
            (error "agent-repl: ws %s env %s has invalid session-id %S (expected string or nil)"
                   ws key sid))))))
  (agent-repl--log ws "validate-ws-env: ws=%s passed" ws))

;;;; Input history

(defvar-local agent-repl--input-history nil
  "List of previous inputs, most recent first.")

(defvar-local agent-repl--history-index -1
  "Current position in history. -1 means not browsing.")

(defvar-local agent-repl--history-stash nil
  "Stashed in-progress text saved when history browsing begins.")

(defvar-local agent-repl--history-navigating nil
  "Non-nil while history navigation is replacing buffer text.")

(defun agent-repl--history-push (&optional text)
  "Save TEXT (or current buffer text) to history.
Skips empty strings and duplicates of the most recent entry."
  (let ((text (string-trim (or text (buffer-string))))
        (ws (agent-repl--ws-current-name)))
    (cond
     ((string-empty-p text)
      (agent-repl--log ws "history-push: skipped empty text"))
     ((equal text (car agent-repl--input-history))
      (agent-repl--log ws "history-push: skipped duplicate text=%s" text))
     (t
      (agent-repl--log ws "history-push: pushed text=%s" text)
      (push text agent-repl--input-history)))))

(defun agent-repl--history-reset ()
  "Reset history browsing index to the default (not browsing) state."
  (setq agent-repl--history-index -1))

(defun agent-repl--history-replace-buffer-text (text)
  "Erase the current buffer and insert TEXT.
Binds `agent-repl--history-navigating' to suppress `history-on-change'."
  (let ((agent-repl--history-navigating t))
    (erase-buffer)
    (insert text)))

(defun agent-repl--history-show-entry (index)
  "Display the history entry at INDEX, or the stash when INDEX is negative.
Updates `agent-repl--history-index' and replaces the buffer contents."
  (agent-repl--log (agent-repl--ws-current-name) "history-show-entry: index=%d" index)
  (setq agent-repl--history-index index)
  (agent-repl--history-replace-buffer-text
   (if (< index 0)
       (or agent-repl--history-stash "")
     (nth index agent-repl--input-history))))

(defun agent-repl--history-prev ()
  "Navigate to the previous (older) history entry."
  (interactive)
  (agent-repl--log (agent-repl--ws-current-name) "history-prev index=%d" agent-repl--history-index)
  (when agent-repl--input-history
    (let ((next-index (1+ agent-repl--history-index)))
      (when (< next-index (length agent-repl--input-history))
        (when (= agent-repl--history-index -1)
          (setq agent-repl--history-stash (buffer-string)))
        (agent-repl--history-show-entry next-index)))))

(defun agent-repl--history-next ()
  "Navigate to the next (newer) history entry, or restore stashed text."
  (interactive)
  (agent-repl--log (agent-repl--ws-current-name) "history-next index=%d" agent-repl--history-index)
  (when (>= agent-repl--history-index 0)
    (agent-repl--history-show-entry (1- agent-repl--history-index))))

(defun agent-repl--history-on-change (&rest _)
  "Reset history browsing when the user edits the buffer directly."
  (when (and (not agent-repl--history-navigating)
             (>= agent-repl--history-index 0))
    (agent-repl--log (agent-repl--ws-current-name) "history-on-change resetting from index=%d" agent-repl--history-index)
    (agent-repl--history-reset)))

;;;; History search (completing-read)

(defun agent-repl--history-format-candidate (entry index)
  "Return a single-line label for ENTRY at INDEX.
Collapses internal newlines to a visible glyph so multi-line inputs stay
on one line in the completing-read frame.  The INDEX prefix keeps
otherwise-identical entries unique candidates."
  (let ((collapsed (replace-regexp-in-string "[\n\r]+" " ⏎ " entry)))
    (format "%4d  %s" index collapsed)))

(defun agent-repl--history-search-candidates ()
  "Return an alist of (LABEL . INDEX) candidates for the current buffer's history.
Index is the position into `agent-repl--input-history' (0 = most recent)."
  (cl-loop for entry in agent-repl--input-history
           for i from 0
           collect (cons (agent-repl--history-format-candidate entry i) i)))

(defun agent-repl-history-search ()
  "Search the input buffer's history via `completing-read'.
Picks any entry (not just adjacent ones) and replaces the buffer with
the selection.  Stashes the in-progress text on first navigation so
`history-next' past the newest entry restores it, matching the
arrow-key navigation flow.  No-op (with a message) when history is
empty."
  (interactive)
  (agent-repl--log (agent-repl--ws-current-name) "history-search: entries=%d"
                    (length agent-repl--input-history))
  (if (null agent-repl--input-history)
      (message "[agent-repl] input history is empty")
    (let* ((candidates (agent-repl--history-search-candidates))
           (choice (completing-read "Claude history: "
                                    (mapcar #'car candidates) nil t))
           (index (cdr (assoc choice candidates))))
      (when index
        (when (= agent-repl--history-index -1)
          (setq agent-repl--history-stash (buffer-string)))
        (agent-repl--history-show-entry index)))))
