;;; autosave.el --- periodic buffer autosave for agent-repl workspaces -*- lexical-binding: t; -*-

;;; Code:

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
              "save-buffer-if-modified: outcome=save-failed buffer=%S name=%S file=%S live=%s modified=%s error=%S"
              buf buffer-name file live modified err)
             (signal (car err) (cdr err))))
          (unless aggregate-p
            (agent-repl--log-verbose
             ws
             "save-buffer-if-modified: outcome=saved buffer=%S name=%S file=%S live=%s modified=%s"
             buf buffer-name file live modified))
          t)
      (unless aggregate-p
        (agent-repl--log-verbose
         ws
         "save-buffer-if-modified: outcome=skipped buffer=%S name=%S file=%S live=%s modified=%s"
         buf buffer-name file live modified))
      nil)))

(defun agent-repl--autosave-workspace-buffers ()
  "Save all modified file-visiting buffers across all workspaces.
Runs silently every 5 minutes to prevent data loss."
  (if (not (agent-repl--ws-system-available-p))
      (agent-repl--log-verbose
       nil "autosave-workspace-buffers: outcome=skipped workspace-system-available=nil")
    (let* ((persps (agent-repl--ws-all-persps))
           (persp-count (length persps))
           (saved 0))
      (agent-repl--log-verbose
       nil "autosave-workspace-buffers: outcome=scanning workspace-system-available=t perspective-count=%d"
       persp-count)
      (dolist (persp persps)
        (cond
         ;; nil is persp-mode's "no perspective" container — expected, skip silently.
         ((null persp)
          (agent-repl--log-verbose
           nil "autosave-workspace-buffers: outcome=skipped-nil-perspective entry=%S" persp))
         ((not (symbolp persp))
          (let* ((ws (agent-repl--ws-persp-name persp))
                 (perspective-identity (agent-repl--ws-persp-identity persp)))
            (unless (and (stringp ws) (not (string-empty-p ws)))
              (agent-repl--log
               nil
               "autosave-workspace-buffers: rejected perspective-identity=%s workspace-name=%S reason=invalid-workspace-name"
               perspective-identity ws)
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
               "autosave-workspace-buffers: outcome=workspace-complete workspace-name=%s perspective-identity=%s buffer-count=%d modified-count=%d saved-count=%d"
               ws perspective-identity buffer-count modified-count workspace-saved))))
         (t
          (agent-repl--log nil "WARN: autosave encountered non-perspective entry: %S" persp))))
      (if (> saved 0)
          (agent-repl--log nil "autosave: saved %d buffer(s) outcome=saved perspective-count=%d" saved persp-count)
        (agent-repl--log-verbose
         nil "autosave-workspace-buffers: outcome=no-buffers-saved perspective-count=%d" persp-count)))))

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
     nil "autosave: timer-scheduled initial-delay=%S interval=%S timer=%S"
     agent-repl-autosave-initial-delay agent-repl-autosave-interval timer)
    timer))

(agent-repl--arm-autosave-timer)
