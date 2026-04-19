;;; history.el --- input history and session state persistence -*- lexical-binding: t; -*-

;;; Code:

;;;; Constants

(defconst claude-repl--environment-keys '(:bare-metal :sandbox)
  "List of per-workspace environment keys.
Each workspace has one `claude-repl-instantiation' struct per environment.")

(defcustom claude-repl-history-filename ".claude-repl-history"
  "Name of the per-project input history file."
  :type 'string
  :group 'claude-repl)

(defcustom claude-repl-state-filename ".claude-repl-state"
  "Name of the per-project session state file."
  :type 'string
  :group 'claude-repl)

;;;; Error handling

(defmacro claude-repl--with-error-logging (label &rest body)
  "Execute BODY, logging any error with LABEL prefix.
Catches errors and logs them via `claude-repl--log' rather than propagating."
  (declare (indent 1) (debug (stringp body)))
  `(condition-case err
       (progn ,@body)
     (error (claude-repl--log nil (concat ,label " error: %S") err))))

;;;; File I/O helpers

(defun claude-repl--read-sexp-file (file)
  "Read and return the first sexp from FILE."
  (claude-repl--log nil "read-sexp-file: file=%s" file)
  (with-temp-buffer
    (insert-file-contents file)
    (read (current-buffer))))

(defun claude-repl--write-sexp-file (file data)
  "Write DATA as a sexp to FILE."
  (claude-repl--log nil "write-sexp-file: file=%s" file)
  (with-temp-file file
    (prin1 data (current-buffer))))

(defun claude-repl--read-sexp-file-if-exists (file)
  "Read and return the first sexp from FILE, or nil if FILE does not exist."
  (if (file-exists-p file)
      (claude-repl--read-sexp-file file)
    (claude-repl--log nil "read-sexp-file-if-exists: file not found file=%s" file)
    nil))

;;;; Instantiation serialization

(defun claude-repl--instantiation-to-plist (inst)
  "Serialize instantiation INST to a plist for persistence.
Returns nil when INST is nil."
  (if inst
      `(:session-id ,(claude-repl-instantiation-session-id inst)
        :had-session ,(claude-repl-instantiation-had-session inst))
    (claude-repl--log nil "instantiation-to-plist: inst is nil, returning nil")
    nil))

(defun claude-repl--instantiation-from-plist (inst saved)
  "Restore instantiation INST fields from SAVED plist."
  (when (and inst saved)
    (claude-repl--log nil "instantiation-from-plist: restoring session-id=%s had-session=%s"
                      (plist-get saved :session-id)
                      (plist-get saved :had-session))
    (setf (claude-repl-instantiation-session-id inst)
          (plist-get saved :session-id))
    (setf (claude-repl-instantiation-had-session inst)
          (plist-get saved :had-session))))

;;;; Persistence file paths

(defun claude-repl--history-file ()
  "Return the path to the history file for the current project."
  (expand-file-name claude-repl-history-filename (claude-repl--resolve-root)))

(defun claude-repl--state-file (root)
  "Return the path to the state file under ROOT, or nil if ROOT is nil."
  (when root
    (expand-file-name claude-repl-state-filename root)))

(defvar claude-repl--per-project-files
  (list claude-repl-state-filename claude-repl-history-filename)
  "Filenames this module persists at each workspace's project root.
Listed so `claude-repl--state-purge' can unlink them without having to
hunt them down individually.")

(defun claude-repl--state-purge (root)
  "Delete every per-project persistence file under ROOT.
Iterates `claude-repl--per-project-files' and unlinks each when
present.  No-op when ROOT is nil or a file is missing.  Called by the
nuke paths so a freshly-destroyed workspace cannot resurrect its
stale session-id via `state-restore' on the next
`claude-repl--register-worktree-ws' at the same project root."
  (when root
    (dolist (filename claude-repl--per-project-files)
      (let ((path (expand-file-name filename root)))
        (when (file-exists-p path)
          (claude-repl--log nil "state-purge: deleting %s" path)
          (ignore-errors (delete-file path)))))))

;;;; History persistence

(defun claude-repl--ws-live-input-buffer (ws)
  "Return the live input buffer for workspace WS, or nil.
WS defaults to the current workspace name."
  (let* ((ws (or ws (+workspace-current-name)))
         (buf (and ws (claude-repl--ws-get ws :input-buffer))))
    (if (and buf (buffer-live-p buf))
        buf
      (claude-repl--log ws "ws-live-input-buffer: no live input buffer ws=%s buf=%s" ws buf)
      nil)))

(defun claude-repl--history-save (&optional ws)
  "Write input history to disk.
Resolves the project root from the input buffer's local variable
rather than `default-directory' of whatever buffer is current.
WS defaults to the current workspace name."
  (claude-repl--log ws "history-save ws=%s" ws)
  (if-let ((buf (claude-repl--ws-live-input-buffer ws)))
      (let ((history (buffer-local-value 'claude-repl--input-history buf))
            (file (with-current-buffer buf (claude-repl--history-file))))
        (if history
            (claude-repl--write-sexp-file file history)
          (claude-repl--log ws "history-save: no history to save ws=%s" ws)))
    (claude-repl--log ws "history-save: no live input buffer ws=%s" ws)))

(defun claude-repl--history-restore ()
  "Load input history from disk into the current buffer."
  (claude-repl--log nil "history-restore")
  (let ((data (claude-repl--read-sexp-file-if-exists (claude-repl--history-file))))
    (if data
        (setq claude-repl--input-history data)
      (claude-repl--log nil "history-restore: no data found, history unchanged"))))

;;;; Session state persistence

(defun claude-repl--collect-env-state (ws)
  "Return a plist of serialized instantiation state for each environment in WS.
Iterates over `claude-repl--environment-keys', converting each
environment's instantiation struct to a plist."
  (claude-repl--log ws "collect-env-state: ws=%s keys=%S" ws claude-repl--environment-keys)
  (cl-loop for key in claude-repl--environment-keys
           nconc (list key (claude-repl--instantiation-to-plist
                            (claude-repl--ws-get ws key)))))

(defun claude-repl--state-save (ws)
  "Persist session state for workspace WS to disk.
Saves session-id and had-session for each environment so they
survive Emacs restarts.  Written to .claude-repl-state in the
project root (alongside .claude-repl-history)."
  (let* ((root (claude-repl--ws-get ws :project-dir))
         (file (claude-repl--state-file root)))
    (claude-repl--log ws "state-save ws=%s file=%s" ws file)
    (if (null file)
        (claude-repl--log ws "state-save: no :project-dir for ws=%s, skipping" ws)
      (let ((state (append `(:project-dir ,root
                              :active-env ,(claude-repl--ws-get ws :active-env))
                           (claude-repl--collect-env-state ws))))
        (claude-repl--with-error-logging "state-save"
          (claude-repl--write-sexp-file file state)
          (claude-repl--log ws "state-save: write complete ws=%s file=%s" ws file))))))

(defun claude-repl--restore-env-state (ws state)
  "Restore instantiation fields for each environment in WS from STATE plist.
Iterates over `claude-repl--environment-keys' and applies the saved
plist to each environment's instantiation struct."
  (dolist (key claude-repl--environment-keys)
    (claude-repl--log ws "restore-env-state: ws=%s key=%s" ws key)
    (claude-repl--instantiation-from-plist
     (claude-repl--ws-get ws key) (plist-get state key))))

(defun claude-repl--apply-restored-state (ws state)
  "Apply persisted STATE plist to workspace WS.
Sets :project-dir, :active-env, and restores instantiation fields for
all environments."
  (let ((saved-dir (plist-get state :project-dir))
        (saved-env (plist-get state :active-env)))
    (when saved-dir
      (claude-repl--ws-put ws :project-dir (claude-repl--path-canonical saved-dir)))
    (when saved-env
      (claude-repl--ws-put ws :active-env saved-env))
    (claude-repl--restore-env-state ws state)
    (claude-repl--log ws "state-restore ws=%s project-dir=%s active-env=%s envs=%S"
                      ws saved-dir saved-env
                      (cl-loop for key in claude-repl--environment-keys
                               collect (cons key (plist-get state key))))))

(defun claude-repl--state-restore (ws)
  "Restore persisted session state for workspace WS from disk.
Populates had-session, session-id, and :project-dir on the workspace
from .claude-repl-state.  After Emacs restart the in-memory hash table
has no :project-dir yet, so we fall back to `default-directory' (which
persp-mode restores per workspace) to locate the state file."
  (let* ((root (or (claude-repl--ws-get ws :project-dir)
                   (claude-repl--git-root default-directory)))
         (file (claude-repl--state-file root))
         (data (when file (claude-repl--read-sexp-file-if-exists file))))
    (cond
     ((null root)
      (claude-repl--log ws "state-restore: no root found for ws=%s, skipping" ws))
     ((null file)
      (claude-repl--log ws "state-restore: no state file for ws=%s root=%s, skipping" ws root))
     ((null data)
      (claude-repl--log ws "state-restore: no data in file ws=%s file=%s, skipping" ws file))
     (t
      (claude-repl--with-error-logging "state-restore"
        (claude-repl--apply-restored-state ws data))))))

;;;; Input history

(defvar-local claude-repl--input-history nil
  "List of previous inputs, most recent first.")

(defvar-local claude-repl--history-index -1
  "Current position in history. -1 means not browsing.")

(defvar-local claude-repl--history-stash nil
  "Stashed in-progress text saved when history browsing begins.")

(defvar-local claude-repl--history-navigating nil
  "Non-nil while history navigation is replacing buffer text.")

(defun claude-repl--history-push (&optional text)
  "Save TEXT (or current buffer text) to history.
Skips empty strings and duplicates of the most recent entry."
  (let ((text (string-trim (or text (buffer-string)))))
    (cond
     ((string-empty-p text)
      (claude-repl--log nil "history-push: skipped empty text"))
     ((equal text (car claude-repl--input-history))
      (claude-repl--log nil "history-push: skipped duplicate text=%s" text))
     (t
      (claude-repl--log nil "history-push: pushed text=%s" text)
      (push text claude-repl--input-history)))))

(defun claude-repl--history-reset ()
  "Reset history browsing index to the default (not browsing) state."
  (setq claude-repl--history-index -1))

(defun claude-repl--history-replace-buffer-text (text)
  "Erase the current buffer and insert TEXT.
Binds `claude-repl--history-navigating' to suppress `history-on-change'."
  (let ((claude-repl--history-navigating t))
    (erase-buffer)
    (insert text)))

(defun claude-repl--history-show-entry (index)
  "Display the history entry at INDEX, or the stash when INDEX is negative.
Updates `claude-repl--history-index' and replaces the buffer contents."
  (claude-repl--log nil "history-show-entry: index=%d" index)
  (setq claude-repl--history-index index)
  (claude-repl--history-replace-buffer-text
   (if (< index 0)
       (or claude-repl--history-stash "")
     (nth index claude-repl--input-history))))

(defun claude-repl--history-prev ()
  "Navigate to the previous (older) history entry."
  (interactive)
  (claude-repl--log nil "history-prev index=%d" claude-repl--history-index)
  (when claude-repl--input-history
    (let ((next-index (1+ claude-repl--history-index)))
      (when (< next-index (length claude-repl--input-history))
        (when (= claude-repl--history-index -1)
          (setq claude-repl--history-stash (buffer-string)))
        (claude-repl--history-show-entry next-index)))))

(defun claude-repl--history-next ()
  "Navigate to the next (newer) history entry, or restore stashed text."
  (interactive)
  (claude-repl--log nil "history-next index=%d" claude-repl--history-index)
  (when (>= claude-repl--history-index 0)
    (claude-repl--history-show-entry (1- claude-repl--history-index))))

(defun claude-repl--history-on-change (&rest _)
  "Reset history browsing when the user edits the buffer directly."
  (when (and (not claude-repl--history-navigating)
             (>= claude-repl--history-index 0))
    (claude-repl--log nil "history-on-change resetting from index=%d" claude-repl--history-index)
    (claude-repl--history-reset)))
