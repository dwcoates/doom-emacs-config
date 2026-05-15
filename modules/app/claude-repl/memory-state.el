;;; memory-state.el --- on-disk dump of the in-memory workspace plist -*- lexical-binding: t; -*-

;;; Commentary:

;; Persists each workspace's full `claude-repl--workspaces' plist to
;; `<root>/.claude/emacs/memory-state.el' whenever its `:claude-state'
;; or `:repl-state' changes.  Mirrors what `SPC j h p'
;; (`claude-repl-debug/dump-workspace') would show interactively, so an
;; out-of-process reader (the debug-logs skill) can inspect live
;; workspace state without an Emacs session.
;;
;; Format: a single readable plist sexp.  Values that don't survive
;; `read' round-trip (buffers, processes, timers, cl-structs) are
;; replaced with readable strings using the same rendering as the
;; interactive dump.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defconst claude-repl-memory-state-filename "memory-state.el"
  "Filename (relative to `claude-repl-emacs-data-subdir') for the workspace
plist dump.  Auto-written from the state setters in status.el.")

(defun claude-repl--memory-state-file (root)
  "Return the absolute path to the memory-state file under ROOT.
Returns nil when ROOT is nil so callers can short-circuit on stub
workspaces that have no `:project-dir' yet."
  (when root
    (expand-file-name claude-repl-memory-state-filename
                      (claude-repl--data-dir root))))

(defun claude-repl--memory-state-format-value (val)
  "Render VAL for the memory-state file.
Buffers, processes, timers, and cl-structs become readable strings
matching the interactive `claude-repl-debug/dump-workspace' output.
Every other value passes through unchanged so the file round-trips
through `read'."
  (cond
   ((bufferp val)
    (format "#<buffer %s %s>"
            (buffer-name val)
            (if (buffer-live-p val) "live" "dead")))
   ((processp val)
    (format "#<process %s %s>"
            (process-name val)
            (if (process-live-p val) "running" "exited")))
   ((timerp val)
    (format "#<timer %s>"
            (if (timer--triggered val) "triggered" "pending")))
   ((cl-struct-p val)
    (string-trim (pp-to-string val)))
   (t val)))

(defun claude-repl--memory-state-render (ws plist)
  "Return the memory-state plist for WS.
Prepends `:ws' and `:written-at' header keys, then walks PLIST
running every value through `claude-repl--memory-state-format-value'."
  (let ((out (list :ws ws
                   :written-at (format-time-string "%Y-%m-%dT%H:%M:%S%z")))
        (pl plist))
    (while pl
      (let ((k (pop pl))
            (v (pop pl)))
        (setq out (nconc out (list k (claude-repl--memory-state-format-value v))))))
    out))

(defun claude-repl--memory-state-write-file (file data)
  "Write DATA (a plist) to FILE one `:key value' pair per line.
Creates FILE's parent directory if missing."
  (let ((dir (file-name-directory file)))
    (when (and dir (not (file-directory-p dir)))
      (make-directory dir t)))
  (with-temp-file file
    (insert ";;; -*- lexical-binding: t; -*-\n")
    (insert ";;; claude-repl memory-state dump — auto-written by Emacs.\n")
    (insert ";;; Mirrors `SPC j h p' / `claude-repl-debug/dump-workspace'.\n")
    (insert ";;; Read with: (with-temp-buffer (insert-file-contents FILE) (read (current-buffer)))\n\n")
    (insert "(")
    (let ((first t)
          (pl data))
      (while pl
        (let ((k (pop pl))
              (v (pop pl)))
          (if first
              (setq first nil)
            (insert "\n "))
          (prin1 k (current-buffer))
          (insert " ")
          (prin1 v (current-buffer)))))
    (insert ")\n")))

(defun claude-repl--memory-state-save (ws)
  "Persist the full plist of WS to its `<root>/.claude/emacs/memory-state.el'.
No-op when WS is nil, absent from `claude-repl--workspaces', or lacking
a `:project-dir'.  Wrapped in `claude-repl--with-error-logging' so a
write failure is logged but does not propagate into the state-setter
caller (matching `claude-repl--state-save')."
  (when (and ws (boundp 'claude-repl--workspaces))
    (let* ((plist (gethash ws claude-repl--workspaces))
           (root (plist-get plist :project-dir))
           (file (and root (claude-repl--memory-state-file root))))
      (cond
       ((null plist)
        (claude-repl--log
         ws "memory-state-save: ws=%s missing from --workspaces, skipping" ws))
       ((null file)
        (claude-repl--log
         ws "memory-state-save: ws=%s no :project-dir, skipping" ws))
       (t
        (claude-repl--with-error-logging "memory-state-save"
          (claude-repl--memory-state-write-file
           file (claude-repl--memory-state-render ws plist))
          (claude-repl--log
           ws "memory-state-save: wrote ws=%s file=%s" ws file)))))))

(provide 'claude-repl-memory-state)
;;; memory-state.el ends here
