;;; sentinel.el --- file-notify watcher for Claude Code hooks -*- lexical-binding: t; -*-

;;; Code:

(require 'filenotify)

;; ---------------------------------------------------------------------------
;; Constants
;; ---------------------------------------------------------------------------

(defconst claude-repl--sentinel-dir
  (expand-file-name "~/.claude/workspace-notifications")
  "Directory where Claude Code hooks write sentinel files.")

;;; Workspace resolution

;; Workspace event detection via file-notify watcher.
;; Claude Code hooks write the CWD to sentinel files in the sentinel
;; directory; we watch it and dispatch by filename: permission_prompt,
;; stop_*, prompt_submit_*.

(defun claude-repl--ws-for-dir-fast (dir)
  "Try the fast path for DIR: git-root -> hash -> buffer -> workspace.
Return the workspace name or nil."
  (let* ((root (claude-repl--git-root dir))
         (hash (when root (substring (md5 (claude-repl--path-canonical root)) 0 8)))
         (buf-name (when hash (format "*claude-%s*" hash)))
         (buf (when buf-name (get-buffer buf-name)))
         (ws (when buf (claude-repl--workspace-for-buffer buf))))
    (if ws
        (claude-repl--log ws "ws-for-dir fast-path HIT: dir=%S root=%S hash=%s buf=%S ws=%s"
                          dir root hash buf-name ws)
      (claude-repl--log nil "ws-for-dir fast-path MISS: dir=%S root=%S hash=%s buf-name=%S buf-exists=%s ws=%s"
                        dir root hash buf-name (if buf "yes" "no") ws))
    ws))

(defun claude-repl--ws-for-dir-container (dir)
  "Try container-path matching for DIR.
Docker sandboxes mount worktrees at /<dirname>, so the sentinel CWD
won't match any host path.  Extract the first path component after /
as the container root name and match against workspace project dirs.
Return the workspace name or nil."
  (unless (bound-and-true-p persp-mode)
    (claude-repl--log nil "ws-for-dir-container: persp-mode not bound, skipping container path for dir=%S" dir))
  (when (bound-and-true-p persp-mode)
    (let* ((container-root (car (split-string (substring dir 1) "/")))
           (all-ws (+workspace-list-names))
           (ws-dirs (mapcar (lambda (ws)
                              (cons ws (claude-repl--ws-get ws :project-dir)))
                            all-ws))
           (match (cl-loop for (ws . proj-dir) in ws-dirs
                           when (and proj-dir
                                     (string= container-root
                                              (file-name-nondirectory
                                               (directory-file-name proj-dir))))
                           return ws)))
      (if match
          (claude-repl--log match "ws-for-dir container-path match: dir=%S root=%S ws=%s"
                            dir container-root match)
        (claude-repl--log nil "ws-for-dir fallback FAILED: dir=%S container-root=%S workspaces=%S"
                          dir container-root ws-dirs))
      match)))

(defun claude-repl--ws-for-dir (dir)
  "Return the workspace name for a Claude session rooted at DIR, or nil.
First tries the fast path: git-root -> hash -> buffer -> workspace.
Falls back to container-path matching for Docker sandbox workspaces."
  (let ((ws (or (let ((fast (claude-repl--ws-for-dir-fast dir)))
                  (when fast
                    (claude-repl--log fast "ws-for-dir: fast path succeeded: dir=%S ws=%s" dir fast))
                  fast)
                (let ((container (claude-repl--ws-for-dir-container dir)))
                  (when container
                    (claude-repl--log container "ws-for-dir: container path succeeded: dir=%S ws=%s" dir container))
                  container))))
    (unless ws
      (claude-repl--log nil "ws-for-dir: both paths failed: dir=%S" dir))
    ws))

;;; Sentinel file reading

(defun claude-repl--read-sentinel-file (file)
  "Read and trim the contents of sentinel FILE, or nil on error."
  (let ((fname (file-name-nondirectory file)))
    (condition-case err
        (let ((content (string-trim (with-temp-buffer
                                      (insert-file-contents file)
                                      (buffer-string)))))
          (claude-repl--log nil "read-sentinel-file: file=%s len=%d" fname (length content))
          content)
      (file-missing
       ;; Race: file-notify fired but file was deleted between exists-p and read.
       (claude-repl--log nil "read-sentinel-file race: %s gone" fname)
       nil)
      (error
       (message "[claude-repl] WARNING: failed to read sentinel %s: %S"
                fname err)
       nil))))

;;; Event handlers

(defun claude-repl--process-sentinel-file (file handler)
  "Read sentinel FILE, resolve its workspace, invoke HANDLER's callback, then delete.
HANDLER is an entry from `claude-repl--sentinel-dispatch-alist' with keys
:warning, :callback, and :name.

Reads the directory from FILE via `claude-repl--read-sentinel-file',
resolves the workspace via `claude-repl--ws-for-dir'.  If the directory
is nil (read failed), does nothing.  If the workspace is nil, logs the
:warning message with the directory interpolated via %s.  Otherwise logs a
standard entry using :name, then calls :callback with two arguments: the
workspace name and the directory.  Always deletes FILE at the end."
  (let* ((dir (claude-repl--read-sentinel-file file))
         (ws  (when dir (claude-repl--ws-for-dir dir))))
    (cond
     ((null dir)) ; read-sentinel-file already warned
     ((null ws)
      (message (plist-get handler :warning) dir))
     (t
      (claude-repl--log ws "%s: file=%s dir=%s ws=%s status=%s"
                        (plist-get handler :name)
                        (file-name-nondirectory file) dir ws
                        (claude-repl--ws-get ws :status))
      (funcall (plist-get handler :callback) ws dir)))
    (ignore-errors (delete-file file))))

(defun claude-repl--on-permission-event (ws _dir)
  "Set :permission status on workspace WS.
Callback for the permission_prompt sentinel handler."
  (claude-repl--log ws "on-permission-event: ws=%s status=%s" ws (claude-repl--ws-get ws :status))
  (claude-repl--ws-set ws :permission))

(defun claude-repl--on-stop-event (ws dir)
  "Handle a stop event for workspace WS with directory DIR.
Logs the resolution, clears :thinking, and runs the finished handler."
  (message "[claude-repl] stop resolved: dir=%s → ws=%s (was %s)" dir ws
           (claude-repl--ws-get ws :status))
  (claude-repl--ws-clear ws :thinking)
  (claude-repl--handle-claude-finished ws))

(defun claude-repl--on-prompt-submit-event (ws _dir)
  "Mark workspace WS as thinking after a prompt submission."
  (claude-repl--log ws "on-prompt-submit-event: ws=%s status=%s" ws (claude-repl--ws-get ws :status))
  (claude-repl--mark-ws-thinking ws))

;;; Event dispatch

(defconst claude-repl--sentinel-dispatch-alist
  '(("permission_prompt" . (:callback claude-repl--on-permission-event
                            :warning  "[claude-repl] WARNING: permission dir=%s matched no workspace"
                            :name     "handle-permission"))
    ("stop_"             . (:callback claude-repl--on-stop-event
                            :warning  "[claude-repl] WARNING: stop sentinel dir=%s matched no workspace (tab may be stuck red)"
                            :name     "handle-stop"))
    ("prompt_submit_"    . (:callback claude-repl--on-prompt-submit-event
                            :warning  "[claude-repl] WARNING: prompt-submit dir=%s matched no workspace"
                            :name     "handle-prompt-submit")))
  "Alist mapping filename prefixes to handler plists.
Each entry is (PREFIX . PLIST) where PLIST has keys:
  :callback  - function called with (WS DIR) on match
  :warning   - format string logged when no workspace matches (interpolates DIR via %s)
  :name      - handler name for debug logging")

(defun claude-repl--dispatch-sentinel-file (file)
  "Dispatch FILE to the appropriate sentinel handler.
Matches the filename against `claude-repl--sentinel-dispatch-alist'.
Returns non-nil if a handler was found and called."
  (let* ((name (file-name-nondirectory file))
         (handler (cl-loop for (prefix . plist) in claude-repl--sentinel-dispatch-alist
                           when (string-prefix-p prefix name)
                           return plist)))
    (if handler
        (progn
          (claude-repl--process-sentinel-file file handler)
          t)
      (claude-repl--log nil "dispatch-sentinel-file: no handler matched: file=%s" name)
      nil)))

(defun claude-repl--dispatch-sentinel-event (event)
  "Handle file-notify EVENT for workspace notification sentinel files.
Dispatches by filename via `claude-repl--sentinel-dispatch-alist'.
Skips files that no longer exist (file-notify often fires multiple events
for a single file creation; the first handler deletes the file)."
  (let* ((action (nth 1 event))
         (file   (nth 2 event))
         (fname  (file-name-nondirectory file)))
    (claude-repl--log nil "notify-event: action=%s file=%s exists=%s"
                      action fname (file-exists-p file))
    (if (and (memq action '(created changed))
             (file-exists-p file))
        (claude-repl--dispatch-sentinel-file file)
      (claude-repl--log-verbose nil "notify-event: skipping action=%s file=%s exists=%s"
                                action fname (file-exists-p file)))))

;;; Polling fallback

(defun claude-repl--poll-workspace-notifications ()
  "Scan the sentinel directory for files that file-notify may have missed.
Called periodically as a fallback; any file still present was not picked up
by the file-notify watcher and needs processing."
  (let ((files (if (file-directory-p claude-repl--sentinel-dir)
                   (directory-files claude-repl--sentinel-dir t "\\`[^.]" t)
                 (progn
                   (claude-repl--log nil "poll-notifications: sentinel dir does not exist: %s"
                                     claude-repl--sentinel-dir)
                   nil))))
    (if files
        (progn
          (claude-repl--log nil "poll-notifications: found %d orphaned file(s): %s"
                            (length files)
                            (mapconcat #'file-name-nondirectory files ", "))
          (dolist (file files)
            (when (file-exists-p file)
              (unless (claude-repl--dispatch-sentinel-file file)
                (claude-repl--log nil "poll-notifications: ignoring unknown file %s"
                                  (file-name-nondirectory file))))))
      (claude-repl--log-verbose nil "poll-notifications: no files found in %s"
                                claude-repl--sentinel-dir))))

;; ---------------------------------------------------------------------------
;; File-notify watcher registration (top-level side effect)
;; ---------------------------------------------------------------------------

(make-directory claude-repl--sentinel-dir t)
(file-notify-add-watch claude-repl--sentinel-dir '(change)
                       #'claude-repl--dispatch-sentinel-event)
