;;; autosave.el --- periodic buffer autosave for agent-repl workspaces -*- lexical-binding: t; -*-

;;; Code:

;; Every log call is a log-site macro (log-site.el), needed where this
;; file is expanded; config.el loads it first.
(require 'log-site)

(require 'cl-lib)

;; Cross-file forward declarations.  These sources load in the dependency
;; order config.el establishes and resolve each other's calls at call time,
;; so the declarations below exist for the byte-compiler alone.
(declare-function agent-repl--register-timer "core")
(declare-function agent-repl--ws-all-persps "workspace")
(declare-function agent-repl--ws-buffers "workspace")
(declare-function agent-repl--ws-persp-identity "workspace")
(declare-function agent-repl--ws-persp-name "workspace")
(declare-function agent-repl--ws-system-available-p "workspace")

(defun agent-repl--autosave-buffer-state (buf)
  "Return minimized autosave predicate state for BUF.
This helper runs once per buffer during a sweep.  Per-buffer logging would
recreate the burst this helper exists to aggregate, so the workspace summary
in `agent-repl--autosave-workspace-buffers' owns its diagnostics."
  (let* ((live (buffer-live-p buf))
         (name (and live (buffer-name buf)))
         (file (and live (buffer-file-name buf)))
         (modified (and file (buffer-modified-p buf))))
    (list :live (and live t)
          :name name
          :file file
          :modified (and modified t)
          :candidate (and live file modified t))))

(defun agent-repl--autosave-buffer-modified-p (buf)
  "Return t when BUF is live, file-visiting, and modified."
  (plist-get (agent-repl--autosave-buffer-state buf) :candidate))

(defun agent-repl--save-buffer-if-modified (buf &optional ws aggregate-p)
  "Save BUF silently if it is a live, modified, file-visiting buffer.
Return non-nil if the buffer was saved.  Optional WS is threaded through
for diagnostic logging context.  When AGGREGATE-P is non-nil, omit expected
per-buffer saved/skipped records because the owning sweep emits one bounded
workspace summary; save failures are always recorded before they propagate."
  (let* ((state (agent-repl--autosave-buffer-state buf))
         (live (plist-get state :live))
         (buffer-name (plist-get state :name))
         (file (plist-get state :file))
         (modified (plist-get state :modified)))
    (if (plist-get state :candidate)
        (progn
          ;; WHY: a failed save must be traceable without converting the
          ;; autosave failure into a silently skipped buffer.
          (condition-case err
              (with-current-buffer buf
                (let ((inhibit-message t))
                  (save-buffer)))
            (error
             (agent-repl--log
              ws
              "save-buffer-if-modified: outcome=save-failed buffer={buffer} name={name} file={file} live={live} modified={modified} error={error}"
              :buffer (prin1-to-string buf) :name (prin1-to-string buffer-name) :file (prin1-to-string file) :live live :modified modified :error (prin1-to-string err))
             (signal (car err) (cdr err))))
          (unless aggregate-p
            (agent-repl--log-verbose
             ws
             "save-buffer-if-modified: outcome=saved buffer={buffer} name={name} file={file} live={live} modified={modified}"
             :buffer (prin1-to-string buf) :name (prin1-to-string buffer-name) :file (prin1-to-string file) :live live :modified modified))
          t)
      (unless aggregate-p
        (agent-repl--log-verbose
         ws
         "save-buffer-if-modified: outcome=skipped buffer={buffer} name={name} file={file} live={live} modified={modified}"
         :buffer (prin1-to-string buf) :name (prin1-to-string buffer-name) :file (prin1-to-string file) :live live :modified modified))
      nil)))

(defun agent-repl--autosave-workspace-buffers ()
  "Save all modified file-visiting buffers across all workspaces.
Runs silently every 5 minutes to prevent data loss."
  (if (not (agent-repl--ws-system-available-p))
      (agent-repl--log-verbose
       '(:agent-repl-central "the autosave sweep spans workspaces")
       "autosave-workspace-buffers: outcome=skipped workspace-system-available=nil")
    (let* ((persps (agent-repl--ws-all-persps))
           (persp-count (length persps))
           (saved 0))
      (agent-repl--log-verbose
       '(:agent-repl-central "the autosave sweep spans workspaces")
       "autosave-workspace-buffers: outcome=scanning workspace-system-available=t perspective-count={perspective_count}"
       :perspective_count persp-count)
      (dolist (persp persps)
        (cond
         ;; nil is persp-mode's "no perspective" container — expected, skip silently.
         ((null persp)
          (agent-repl--log-verbose
           '(:agent-repl-central "the autosave sweep spans workspaces")
           "autosave-workspace-buffers: outcome=skipped-nil-perspective entry={entry}" :entry (prin1-to-string persp)))
         ((not (symbolp persp))
          (let* ((ws (agent-repl--ws-persp-name persp))
                 (perspective-identity (agent-repl--ws-persp-identity persp)))
            (unless (and (stringp ws) (not (string-empty-p ws)))
              (agent-repl--log
               '(:agent-repl-central "the autosave sweep spans workspaces")
               "autosave-workspace-buffers: rejected perspective-identity={perspective_identity} workspace-name={workspace_name} reason=invalid-workspace-name"
               :perspective_identity perspective-identity :workspace_name (prin1-to-string ws))
              (error "agent-repl--autosave-workspace-buffers: perspective has no workspace name"))
            (let* ((buffers (agent-repl--ws-buffers persp))
                   (buffer-count (length buffers))
                   (modified-count (cl-count-if
                                    #'agent-repl--autosave-buffer-modified-p
                                    buffers))
                   (workspace-saved 0))
              (dolist (buf buffers)
                (when (agent-repl--save-buffer-if-modified buf ws t)
                  (cl-incf saved)
                  (cl-incf workspace-saved)))
              (agent-repl--log-verbose
               ws
               "autosave-workspace-buffers: outcome=workspace-complete workspace-name={workspace_name} perspective-identity={perspective_identity} buffer-count={buffer_count} modified-count={modified_count} saved-count={saved_count}"
               :workspace_name ws :perspective_identity perspective-identity :buffer_count buffer-count :modified_count modified-count :saved_count workspace-saved))))
         (t
          (agent-repl--log '(:agent-repl-central "the autosave sweep spans workspaces") "WARN: autosave encountered non-perspective entry: {entry}" :entry (prin1-to-string persp)))))
      (if (> saved 0)
          (agent-repl--log '(:agent-repl-central "the autosave sweep spans workspaces") "autosave: saved {saved} buffer(s) outcome=saved perspective-count={perspective_count}" :saved saved :perspective_count persp-count)
        (agent-repl--log-verbose
         '(:agent-repl-central "the autosave sweep spans workspaces")
         "autosave-workspace-buffers: outcome=no-buffers-saved perspective-count={perspective_count}" :perspective_count persp-count)))))

(defcustom agent-repl-autosave-initial-delay 300
  "Seconds before the first autosave sweep fires after load."
  :type 'integer
  :group 'agent-repl)

(defcustom agent-repl-autosave-interval 300
  "Seconds between autosave sweeps."
  :type 'integer
  :group 'agent-repl)

(defun agent-repl--arm-autosave-timer ()
  "Arm the periodic autosave sweep under the `:autosave' key.
Idempotent: `agent-repl--register-timer' cancels and replaces any timer
already held under the key, so re-loading this file leaves exactly one
sweep scheduled instead of stacking a second.  Returns the timer."
  (let ((timer (agent-repl--register-timer
                :autosave
                (run-with-timer agent-repl-autosave-initial-delay
                                agent-repl-autosave-interval
                                #'agent-repl--autosave-workspace-buffers))))
    (agent-repl--log
     '(:agent-repl-central "the autosave sweep spans workspaces")
     "autosave: timer-scheduled initial-delay={initial_delay} interval={interval} timer={timer}"
     :initial_delay (prin1-to-string agent-repl-autosave-initial-delay) :interval (prin1-to-string agent-repl-autosave-interval) :timer (prin1-to-string timer))
    timer))

(agent-repl--arm-autosave-timer)
