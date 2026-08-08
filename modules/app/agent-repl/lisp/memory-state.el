;;; memory-state.el --- on-disk dump of the in-memory workspace plist -*- lexical-binding: t; -*-

;;; Commentary:

;; Persists each workspace's full `agent-repl--workspaces' plist to
;; `<root>/.claude/emacs/memory-state.el' whenever its `:agent-state'
;; or `:repl-state' changes.  Mirrors what `SPC j h p'
;; (`agent-repl-debug/dump-workspace') would show interactively, so an
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

(defconst agent-repl-memory-state-filename "memory-state.el"
  "Filename (relative to `agent-repl-emacs-data-subdir') for the workspace
plist dump.  Auto-written from the state setters in status.el.")

(defun agent-repl--memory-state-file (root)
  "Return the absolute path to the memory-state file under ROOT.
Returns nil when ROOT is nil so callers can short-circuit on stub
workspaces that have no `:project-dir' yet."
  (when root
    (expand-file-name agent-repl-memory-state-filename
                      (agent-repl--data-dir root))))

(defun agent-repl--memory-state-format-value (val &optional ws)
  "Render VAL for the memory-state file.
Buffers, processes, timers, and cl-structs become readable strings
matching the interactive `agent-repl-debug/dump-workspace' output.
Every other value passes through unchanged so the file round-trips
through `read'.  WS scopes verbose serialization diagnostics."
  (cond
   ((bufferp val)
    (agent-repl--log-verbose ws "memory-state-format-value: type=buffer live=%s"
                             (buffer-live-p val))
    (format "#<buffer %s %s>"
            (buffer-name val)
            (if (buffer-live-p val) "live" "dead")))
   ((processp val)
    (agent-repl--log-verbose ws "memory-state-format-value: type=process live=%s"
                             (process-live-p val))
    (format "#<process %s %s>"
            (process-name val)
            (if (process-live-p val) "running" "exited")))
   ((timerp val)
    (agent-repl--log-verbose ws "memory-state-format-value: type=timer triggered=%s"
                             (timer--triggered val))
    (format "#<timer %s>"
            (if (timer--triggered val) "triggered" "pending")))
   ((cl-struct-p val)
    (agent-repl--log-verbose ws "memory-state-format-value: type=cl-struct")
    (string-trim (pp-to-string val)))
   (t
    (agent-repl--log-verbose ws "memory-state-format-value: type=%s preserved=t"
                             (type-of val))
    val)))

(defun agent-repl--memory-state-render (ws plist)
  "Return the memory-state plist for WS.
Prepends `:ws' and `:written-at' header keys, then walks PLIST
running every value through `agent-repl--memory-state-format-value'."
  (let ((out (list :ws ws
                   :written-at (format-time-string "%Y-%m-%dT%H:%M:%S%z")))
        (pl plist))
    (agent-repl--log ws "memory-state-render: start ws=%s plist-pairs=%d"
                      ws (/ (length plist) 2))
    (while pl
      (let ((k (pop pl))
            (v (pop pl)))
        (agent-repl--log-verbose ws "memory-state-render: key=%s value-type=%s"
                                 k (type-of v))
        (setq out (nconc out (list k (agent-repl--memory-state-format-value v ws))))))
    (agent-repl--log ws "memory-state-render: complete ws=%s output-pairs=%d"
                      ws (/ (length out) 2))
    out))

(defun agent-repl--memory-state-write-file (file data &optional ws)
  "Write DATA (a plist) to FILE one `:key value' pair per line.
Creates FILE's parent directory if missing.  WS scopes diagnostics."
  (let ((dir (file-name-directory file)))
    (agent-repl--log ws "memory-state-write-file: start file=%s pairs=%d parent-exists=%s"
                      file (/ (length data) 2) (file-directory-p dir))
    (when (and dir (not (file-directory-p dir)))
      (agent-repl--log ws "memory-state-write-file: creating parent=%s" dir)
      (make-directory dir t)))
  (with-temp-file file
    (insert ";;; -*- lexical-binding: t; -*-\n")
    (insert ";;; agent-repl memory-state dump — auto-written by Emacs.\n")
    (insert ";;; Mirrors `SPC j h p' / `agent-repl-debug/dump-workspace'.\n")
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
          (agent-repl--log-verbose ws "memory-state-write-file: serializing key=%s value-type=%s"
                                   k (type-of v))
          (prin1 k (current-buffer))
          (insert " ")
          (prin1 v (current-buffer)))))
    (insert ")\n"))
  (agent-repl--log ws "memory-state-write-file: complete file=%s pairs=%d"
                    file (/ (length data) 2)))

(defun agent-repl--memory-state-save (ws)
  "Persist the full plist of WS to its `<root>/.claude/emacs/memory-state.el'.
No-op when WS is nil, absent from `agent-repl--workspaces', or lacking
a `:project-dir'.  Wrapped in `agent-repl--with-error-logging' so a
write failure is logged but does not propagate into the state-setter
caller (matching `agent-repl--state-save')."
  (cond
   ((null ws)
    (agent-repl--log nil "memory-state-save: ws=nil, skipping"))
   ((not (boundp 'agent-repl--workspaces))
    (agent-repl--log ws "memory-state-save: ws=%s --workspaces unbound, skipping" ws))
   ((not (agent-repl--ws-known-p ws))
    (agent-repl--log
     ws "memory-state-save: ws=%s missing from --workspaces, skipping" ws))
   (t
    (let* ((plist (agent-repl--ws-plist ws))
           (root (plist-get plist :project-dir))
           (file (and root (agent-repl--memory-state-file root))))
      (cond
       ((null file)
        (agent-repl--log
         ws "memory-state-save: ws=%s no :project-dir, skipping" ws))
       (t
        (agent-repl--with-error-logging "memory-state-save"
          (agent-repl--log ws "memory-state-save: begin ws=%s root=%s file=%s plist-pairs=%d"
                            ws root file (/ (length plist) 2))
          (agent-repl--memory-state-write-file
           file (agent-repl--memory-state-render ws plist) ws)
          (agent-repl--log
           ws "memory-state-save: wrote ws=%s file=%s" ws file))))))))

(provide 'agent-repl-memory-state)
;;; memory-state.el ends here
