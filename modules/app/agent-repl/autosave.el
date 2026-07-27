;;; autosave.el --- periodic buffer autosave for agent-repl workspaces -*- lexical-binding: t; -*-

;;; Code:

(defun agent-repl--save-buffer-if-modified (buf &optional ws)
  "Save BUF silently if it is a live, modified, file-visiting buffer.
Return non-nil if the buffer was saved.  Optional WS is threaded through
for diagnostic logging context."
  (let* ((live (buffer-live-p buf))
         (buffer-name (and live (buffer-name buf)))
         (file (and live (buffer-file-name buf)))
         (modified (and file (buffer-modified-p buf))))
    (if (and live file modified)
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
          (agent-repl--log-verbose
           ws
           "save-buffer-if-modified: outcome=saved buffer=%S name=%S file=%S live=%s modified=%s"
           buf buffer-name file live modified)
          t)
      (agent-repl--log-verbose
       ws
       "save-buffer-if-modified: outcome=skipped buffer=%S name=%S file=%S live=%s modified=%s"
       buf buffer-name file live modified)
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
                 (buffers (agent-repl--ws-buffers persp))
                 (buffer-count (length buffers))
                 (workspace-saved 0))
            (agent-repl--log-verbose
             ws
             "autosave-workspace-buffers: outcome=scanning-workspace perspective=%S buffer-count=%d"
             persp buffer-count)
            (dolist (buf buffers)
              (when (agent-repl--save-buffer-if-modified buf ws)
                (cl-incf saved)
                (cl-incf workspace-saved)))
            (agent-repl--log-verbose
             ws
             "autosave-workspace-buffers: outcome=workspace-complete perspective=%S buffer-count=%d saved-count=%d"
             persp buffer-count workspace-saved)))
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

(let ((timer (run-with-timer agent-repl-autosave-initial-delay agent-repl-autosave-interval
                             #'agent-repl--autosave-workspace-buffers)))
  (push timer agent-repl--timers)
  (agent-repl--log
   nil "autosave: timer-scheduled initial-delay=%S interval=%S timer=%S"
   agent-repl-autosave-initial-delay agent-repl-autosave-interval timer))
