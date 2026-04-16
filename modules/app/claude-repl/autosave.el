;;; autosave.el --- periodic buffer autosave for claude-repl workspaces -*- lexical-binding: t; -*-

;;; Code:

(defun claude-repl--save-buffer-if-modified (buf)
  "Save BUF silently if it is a live, modified, file-visiting buffer.
Return non-nil if the buffer was saved."
  (when (and (buffer-live-p buf)
             (buffer-file-name buf)
             (buffer-modified-p buf))
    (with-current-buffer buf
      (let ((inhibit-message t))
        (save-buffer)))
    (claude-repl--log-verbose nil "save-buffer-if-modified: saved buffer=%s" (buffer-name buf))
    t))

(defun claude-repl--autosave-workspace-buffers ()
  "Save all modified file-visiting buffers across all workspaces.
Runs silently every 5 minutes to prevent data loss."
  (when (bound-and-true-p persp-mode)
    (claude-repl--log-verbose nil "autosave-workspace-buffers: scanning all workspace buffers")
    (let ((saved 0))
      (dolist (persp (persp-persps))
        (if (and persp (not (symbolp persp)))
            (dolist (buf (persp-buffers persp))
              (when (claude-repl--save-buffer-if-modified buf)
                (cl-incf saved)))
          (claude-repl--log nil "WARN: autosave encountered non-perspective entry: %S" persp)))
      (if (> saved 0)
          (claude-repl--log nil "autosave: saved %d buffer(s)" saved)
        (claude-repl--log-verbose nil "autosave-workspace-buffers: no buffers needed saving")))))

(push (run-with-timer 300 300 #'claude-repl--autosave-workspace-buffers)
      claude-repl--timers)
