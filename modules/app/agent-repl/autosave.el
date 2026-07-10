;;; autosave.el --- periodic buffer autosave for agent-repl workspaces -*- lexical-binding: t; -*-

;;; Code:

(defun agent-repl--save-buffer-if-modified (buf &optional ws)
  "Save BUF silently if it is a live, modified, file-visiting buffer.
Return non-nil if the buffer was saved.  Optional WS is threaded through
for diagnostic logging context."
  (when (and (buffer-live-p buf)
             (buffer-file-name buf)
             (buffer-modified-p buf))
    (with-current-buffer buf
      (let ((inhibit-message t))
        (save-buffer)))
    (agent-repl--log-verbose ws "save-buffer-if-modified: saved buffer=%s" (buffer-name buf))
    t))

(defun agent-repl--autosave-workspace-buffers ()
  "Save all modified file-visiting buffers across all workspaces.
Runs silently every 5 minutes to prevent data loss."
  (when (agent-repl--ws-system-available-p)
    (agent-repl--log-verbose nil "autosave-workspace-buffers: scanning all workspace buffers")
    (let ((saved 0))
      (dolist (persp (agent-repl--ws-all-persps))
        (cond
         ;; nil is persp-mode's "no perspective" container — expected, skip silently.
         ((null persp) nil)
         ((not (symbolp persp))
          (let ((ws (agent-repl--ws-persp-name persp)))
            (dolist (buf (agent-repl--ws-buffers persp))
              (when (agent-repl--save-buffer-if-modified buf ws)
                (cl-incf saved)))))
         (t
          (agent-repl--log nil "WARN: autosave encountered non-perspective entry: %S" persp))))
      (if (> saved 0)
          (agent-repl--log nil "autosave: saved %d buffer(s)" saved)
        (agent-repl--log-verbose nil "autosave-workspace-buffers: no buffers needed saving")))))

(defcustom agent-repl-autosave-initial-delay 300
  "Seconds before the first autosave sweep fires after load."
  :type 'integer
  :group 'agent-repl)

(defcustom agent-repl-autosave-interval 300
  "Seconds between autosave sweeps."
  :type 'integer
  :group 'agent-repl)

(push (run-with-timer agent-repl-autosave-initial-delay agent-repl-autosave-interval #'agent-repl--autosave-workspace-buffers)
      agent-repl--timers)
