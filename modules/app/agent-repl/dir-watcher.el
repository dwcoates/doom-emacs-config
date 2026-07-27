;;; dir-watcher.el --- Legacy file-notify teardown boundary -*- lexical-binding: t; -*-

;;; Commentary:

;; The daemon is the sole owner of workspace_commands files.  This file
;; remains only as the integration boundary needed to remove a file-notify
;; descriptor installed by an older, already-running module version during
;; hot reload.  It deliberately exposes no registration, drain, dispatch, or
;; file-processing API.

;;; Code:

(defun agent-repl--dir-watcher-remove-legacy (descriptor label)
  "Remove legacy file-notify DESCRIPTOR identified by LABEL.
Return nil after removal.  A nil or already-invalid descriptor is expected
on a clean start and requires no external mutation.  Errors removing a valid
descriptor propagate so module reload cannot silently leave competing
workspace-command ownership active."
  (let ((valid (and descriptor (file-notify-valid-p descriptor))))
    (agent-repl--log nil
                     "dir-watcher legacy teardown: label=%s descriptor=%S valid=%s"
                     label descriptor (if valid "yes" "no"))
    (cond
     ((null descriptor)
      (agent-repl--log nil
                       "dir-watcher legacy teardown: label=%s outcome=absent"
                       label))
     ((not valid)
      (agent-repl--log nil
                       "dir-watcher legacy teardown: label=%s outcome=already-invalid"
                       label))
     (t
      (condition-case err
          (progn
            (file-notify-rm-watch descriptor)
            (agent-repl--log nil
                             "dir-watcher legacy teardown: label=%s outcome=removed"
                             label))
        (error
         (agent-repl--log nil
                          "dir-watcher legacy teardown: label=%s descriptor=%S outcome=remove-failed error=%S"
                          label descriptor err)
         (signal (car err) (cdr err))))))
    nil))

(provide 'agent-repl-dir-watcher)
;;; dir-watcher.el ends here
