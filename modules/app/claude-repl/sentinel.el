;;; sentinel.el --- file-notify watcher for Claude Code hooks -*- lexical-binding: t; -*-

;;; Code:

(require 'filenotify)

;; ---------------------------------------------------------------------------
;; Constants
;; ---------------------------------------------------------------------------

(defconst claude-repl--sentinel-dir
  (expand-file-name "~/.claude/workspace-notifications")
  "Directory where Claude Code hooks write sentinel files.")

(defcustom claude-repl-sentinel-debug-log-filename "hook-debug.log"
  "Filename of the hook debug log to ignore in sentinel file processing."
  :type 'string
  :group 'claude-repl)

(defcustom claude-repl-sentinel-dispatch-actions '(created changed)
  "File-notify actions that trigger sentinel dispatch."
  :type '(repeat symbol)
  :group 'claude-repl)

(defcustom claude-repl-sentinel-poll-file-regexp "\\`[^.]"
  "Regexp for filtering sentinel directory files during polling.
Only files matching this pattern are enumerated."
  :type 'string
  :group 'claude-repl)

(defcustom claude-repl-sentinel-watch-events '(change)
  "File-notify event types to watch on the sentinel directory."
  :type '(repeat symbol)
  :group 'claude-repl)

;;; Workspace resolution

;; Workspace event detection via file-notify watcher.
;; Claude Code hooks write the CWD to sentinel files in the sentinel
;; directory; we watch it and dispatch by filename: permission_prompt,
;; stop_*, prompt_submit_*.

(defun claude-repl--ws-for-dir-fast (dir)
  "Fast path for DIR: iterate `claude-repl--workspaces' and match :project-dir.
Returns the workspace name whose canonicalized :project-dir equals the
canonicalized git-root of DIR, or nil.

Unlike the previous dir -> git-root -> md5 -> buffer -> persp-lookup chain,
this function does not touch persp-mode or buffer names.  Bypassing the
persp lookup closes the last known leak path that fed \"none\" into
`claude-repl--workspaces' when persp-mode routed a Claude buffer to the
wrong perspective."
  (claude-repl--log-verbose nil "ws-for-dir-fast: ENTER dir=%S" dir)
  (let* ((target-root (and dir (claude-repl--git-root dir)))
         (canonical-target (when target-root
                             (claude-repl--path-canonical target-root)))
         (match nil))
    (claude-repl--log-verbose nil "ws-for-dir-fast: git-root=%S canonical=%S"
                              target-root canonical-target)
    (when canonical-target
      (maphash
       (lambda (ws _plist)
         (unless match
           (let* ((proj (claude-repl--ws-get ws :project-dir))
                  (canonical-proj (when proj
                                    (claude-repl--path-canonical proj))))
             (claude-repl--log-verbose nil "ws-for-dir-fast: check ws=%s proj=%S canonical=%S match=%s"
                                       ws proj canonical-proj
                                       (if (and canonical-proj
                                                (string= canonical-target canonical-proj))
                                           "YES" "no"))
             (when (and canonical-proj
                        (string= canonical-target canonical-proj))
               (setq match ws)))))
       claude-repl--workspaces))
    (if match
        (claude-repl--log-verbose match "ws-for-dir-fast: HIT dir=%S root=%S ws=%s"
                                  dir canonical-target match)
      (claude-repl--log-verbose nil "ws-for-dir-fast: MISS dir=%S root=%S"
                                dir canonical-target))
    match))

(defun claude-repl--ws-for-dir-container (dir)
  "Try container-path matching for DIR.
Docker sandboxes mount worktrees at /<dirname>, so the sentinel CWD
won't match any host path.  Extract the first path component after /
as the container root name and match against workspace project dirs.
Return the workspace name or nil."
  (claude-repl--log-verbose nil "ws-for-dir-container: ENTER dir=%S persp-mode=%s"
                    dir (if (bound-and-true-p persp-mode) "yes" "no"))
  (unless (bound-and-true-p persp-mode)
    (claude-repl--log nil "ws-for-dir-container: persp-mode not bound, aborting"))
  (when (bound-and-true-p persp-mode)
    (let* ((container-root (car (split-string (substring dir 1) "/")))
           (all-ws (+workspace-list-names))
           (ws-dirs (mapcar (lambda (ws)
                              (cons ws (claude-repl--ws-get ws :project-dir)))
                            all-ws)))
      (claude-repl--log-verbose nil "ws-for-dir-container: container-root=%S all-ws=%S ws-dirs=%S"
                        container-root all-ws ws-dirs)
      (let ((match (cl-loop for (ws . proj-dir) in ws-dirs
                            for canonical = (when proj-dir
                                              (claude-repl--path-canonical proj-dir))
                            for basename = (when canonical
                                             (file-name-nondirectory canonical))
                            do (claude-repl--log-verbose nil "ws-for-dir-container: checking ws=%s proj-dir=%S canonical=%S basename=%S vs container-root=%S match=%s"
                                                 ws proj-dir canonical basename container-root
                                                 (if (and basename (string= container-root basename)) "YES" "no"))
                            when (and basename (string= container-root basename))
                            return ws)))
        (if match
            (claude-repl--log-verbose match "ws-for-dir-container: HIT dir=%S container-root=%S ws=%s"
                              dir container-root match)
          (claude-repl--log nil "ws-for-dir-container: MISS dir=%S container-root=%S"
                            dir container-root))
        match))))

(defun claude-repl--ws-for-dir (dir)
  "Return the workspace name for a Claude session rooted at DIR, or nil.
First tries the fast path: git-root -> hash -> buffer -> workspace.
Falls back to container-path matching for Docker sandbox workspaces."
  (claude-repl--log-verbose nil "ws-for-dir: ENTER dir=%S" dir)
  (let* ((fast (claude-repl--ws-for-dir-fast dir))
         (_ (claude-repl--log-verbose nil "ws-for-dir: fast-path returned %S" fast))
         (container (unless fast (claude-repl--ws-for-dir-container dir)))
         (_ (unless fast (claude-repl--log-verbose nil "ws-for-dir: container-path returned %S" container)))
         (ws (or fast container)))
    (claude-repl--log-verbose nil "ws-for-dir: EXIT dir=%S ws=%S (via %s)"
                      dir ws (cond (fast "fast-path") (container "container-path") (t "NONE")))
    ws))

;;; Sentinel file reading

(defun claude-repl--read-sentinel-file (file)
  "Read sentinel FILE and return a plist (:dir DIR :session-id SID), or nil on error.
The file format is two lines: CWD on line 1, session_id on line 2.
For backward compatibility, a single-line file (CWD only) returns :session-id nil."
  (let ((fname (file-name-nondirectory file)))
    (claude-repl--log-verbose nil "read-sentinel-file: ENTER file=%s exists=%s readable=%s"
                      fname (file-exists-p file) (file-readable-p file))
    (condition-case err
        (let* ((raw (with-temp-buffer
                      (insert-file-contents file)
                      (buffer-string)))
               (lines (split-string (string-trim raw) "\n" t))
               (dir (string-trim (or (nth 0 lines) "")))
               (session-id (when (nth 1 lines) (string-trim (nth 1 lines)))))
          (claude-repl--log-verbose nil "read-sentinel-file: file=%s dir=%S session-id=%S"
                            fname dir session-id)
          (if (or (string= dir "null") (string= dir ""))
              (progn
                (claude-repl--log nil "read-sentinel-file: REJECTING file=%s with bogus dir=%S (hook may not have received cwd)"
                                  fname dir)
                nil)
            (list :dir dir :session-id session-id)))
      (file-missing
       (claude-repl--log nil "read-sentinel-file: RACE file=%s gone between exists-p and read" fname)
       nil)
      (error
       (claude-repl--log nil "read-sentinel-file: ERROR file=%s err=%S" fname err)
       nil))))

;;; Event handlers

(defun claude-repl--update-session-id-from-sentinel (ws session-id)
  "Update the session ID for workspace WS from sentinel data if non-nil.
Only updates when WS is already registered in `claude-repl--workspaces'
and SESSION-ID is a non-empty string that differs from the stored value.
Skipping unregistered workspaces is load-bearing: `claude-repl--active-inst'
auto-creates an instantiation in the hash, which would otherwise cause
sentinel fires for workspaces this module doesn't manage (e.g. the
default persp matched via `workspace-for-buffer') to leak entries into
`claude-repl--workspaces'."
  (when (and session-id
             (not (string-empty-p session-id))
             (gethash ws claude-repl--workspaces))
    (let* ((inst (claude-repl--active-inst ws))
           (current (claude-repl-instantiation-session-id inst)))
      (unless (equal current session-id)
        (claude-repl--log ws "update-session-id-from-sentinel: ws=%s old=%s new=%s" ws current session-id)
        (claude-repl--set-session-id ws session-id)))))

(defun claude-repl--process-sentinel-file (file handler)
  "Read sentinel FILE, delete it, resolve its workspace, then invoke HANDLER's callback.
HANDLER is an entry from `claude-repl--sentinel-dispatch-alist' with keys
:warning, :callback, and :name.

Reads the directory and session-id from FILE via `claude-repl--read-sentinel-file',
then deletes FILE immediately so the poll fallback cannot race a slow handler
and re-dispatch the same file.  Resolves the workspace via `claude-repl--ws-for-dir'.
If the read failed \(nil return), does nothing further.  If the workspace is
nil, logs the :warning message with the directory interpolated via %s.
Otherwise updates the workspace's session ID (if provided), logs a standard
entry using :name, then calls :callback with two arguments: the workspace
name and the directory."
  (let* ((sentinel-data (claude-repl--read-sentinel-file file))
         (dir (plist-get sentinel-data :dir))
         (session-id (plist-get sentinel-data :session-id))
         (ws  (when dir (claude-repl--ws-for-dir dir))))
    ;; Delete the file immediately after reading so the poll fallback can't
    ;; re-dispatch it while a slow handler (e.g. panel setup) is still running.
    (ignore-errors (delete-file file))
    (claude-repl--log nil "process-sentinel-file: handler=%s file=%s dir=%S session-id=%S ws=%s"
                      (plist-get handler :name) (file-name-nondirectory file) dir session-id ws)
    (cond
     ((null dir)
      (claude-repl--log nil "process-sentinel-file: dir is nil (read failed) for %s"
                        (file-name-nondirectory file)))
     ((null ws)
      (message (plist-get handler :warning) dir))
     (t
      (claude-repl--update-session-id-from-sentinel ws session-id)
      (claude-repl--log ws "%s: file=%s dir=%s ws=%s status=%s"
                        (plist-get handler :name)
                        (file-name-nondirectory file) dir ws
                        (claude-repl--ws-get ws :claude-state))
      (funcall (plist-get handler :callback) ws dir)))))

(defun claude-repl--on-permission-event (ws _dir)
  "Set :permission status on workspace WS.
Callback for the permission_prompt sentinel handler."
  (let ((before (claude-repl--ws-get ws :claude-state)))
    (claude-repl--log-verbose ws "on-permission-event: ws=%s status-BEFORE=%s" ws before)
    (claude-repl--ws-set-claude-state ws :permission)
    (claude-repl--log-verbose ws "on-permission-event: ws=%s status-AFTER=%s" ws (claude-repl--ws-get ws :claude-state))))

(defun claude-repl--on-stop-event (ws dir)
  "Handle a stop event for workspace WS with directory DIR.
Logs the resolution, clears :thinking, and runs the finished handler."
  (let ((before (claude-repl--ws-get ws :claude-state))
        (vterm-buf (claude-repl--ws-get ws :vterm-buffer)))
    (claude-repl--log ws "on-stop-event: ENTER ws=%s dir=%S status-BEFORE=%s vterm-buf=%S vterm-live=%s"
                      ws dir before
                      (when vterm-buf (buffer-name vterm-buf))
                      (if (and vterm-buf (buffer-live-p vterm-buf)) "yes" "no"))
    (claude-repl--ws-claude-state-clear-if ws :thinking)
    (let ((after-clear (claude-repl--ws-get ws :claude-state)))
      (claude-repl--log ws "on-stop-event: after ws-clear status=%s (was %s, expected nil)" after-clear before)
      (claude-repl--handle-claude-finished ws)
      (claude-repl--log ws "on-stop-event: EXIT ws=%s status-AFTER=%s" ws (claude-repl--ws-get ws :claude-state)))))

(defun claude-repl--on-prompt-submit-event (ws _dir)
  "Mark workspace WS as thinking after a prompt submission."
  (let ((before (claude-repl--ws-get ws :claude-state)))
    (claude-repl--log-verbose ws "on-prompt-submit-event: ws=%s status-BEFORE=%s" ws before)
    (claude-repl--mark-ws-thinking ws)
    (claude-repl--log-verbose ws "on-prompt-submit-event: ws=%s status-AFTER=%s" ws (claude-repl--ws-get ws :claude-state))))

(defun claude-repl--on-session-start-event (ws _dir)
  "Handle a session_start event for workspace WS.
When WS has a live vterm buffer, this is Claude becoming ready:
transitions `:claude-state' from `:init' to `:idle', sets
`claude-repl--ready' on the vterm buffer, cancels the ready timer,
and opens panels (via `open-panels-after-ready').

When WS is registered but its vterm buffer is null or dead, that is a
structural inconsistency — by the time this callback runs, the caller
chain has confirmed WS is non-nil and registered in
`claude-repl--workspaces'.  We emit a loud `message' (but do not
`error', since we run from a file-notify callback and a hard error
would kill the watcher) so the condition is visible.

Idempotent on repeated fires for the same workspace."
  (let ((vterm-buf (claude-repl--ws-get ws :vterm-buffer)))
    (claude-repl--log ws "on-session-start-event: ENTER ws=%s vterm-buf=%S vterm-live=%s"
                      ws (when vterm-buf (buffer-name vterm-buf))
                      (if (and vterm-buf (buffer-live-p vterm-buf)) "yes" "no"))
    (cond
     ((or (null vterm-buf) (not (buffer-live-p vterm-buf)))
      (message "[claude-repl] ERROR: session_start for ws=%s but vterm buffer is null/dead" ws)
      (claude-repl--log ws
                        "on-session-start-event: ERROR ws=%s vterm buffer is null/dead — structural inconsistency"
                        ws))
     ((buffer-local-value 'claude-repl--ready vterm-buf)
      (claude-repl--log ws "on-session-start-event: already ready ws=%s — no-op" ws))
     (t
      (claude-repl--log ws "on-session-start-event: marking ready ws=%s" ws)
      (claude-repl--ws-set-claude-state ws :idle)
      (with-current-buffer vterm-buf
        (setq claude-repl--ready t))
      (claude-repl--cancel-ready-timer ws)
      (claude-repl--open-panels-after-ready ws)))))

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
                            :name     "handle-prompt-submit"))
    ("session_start_"    . (:callback claude-repl--on-session-start-event
                            :warning  "[claude-repl] WARNING: session-start dir=%s matched no workspace"
                            :name     "handle-session-start")))
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
         (matched-prefix nil)
         (handler (cl-loop for (prefix . plist) in claude-repl--sentinel-dispatch-alist
                           when (string-prefix-p prefix name)
                           do (setq matched-prefix prefix)
                           and return plist)))
    (claude-repl--log nil "dispatch-sentinel-file: file=%s matched-prefix=%S handler=%s"
                      name matched-prefix (if handler (plist-get handler :name) "NONE"))
    (if handler
        (progn
          (claude-repl--process-sentinel-file file handler)
          t)
      (claude-repl--log nil "dispatch-sentinel-file: NO HANDLER for file=%s (tried prefixes: %S)"
                        name (mapcar #'car claude-repl--sentinel-dispatch-alist))
      nil)))

(defun claude-repl--dispatch-sentinel-event (event)
  "Handle file-notify EVENT for workspace notification sentinel files.
Dispatches by filename via `claude-repl--sentinel-dispatch-alist'.
Skips files that no longer exist (file-notify often fires multiple events
for a single file creation; the first handler deletes the file).
Ignores events whose file is nil (e.g. `stopped' events fired when a
watch is removed) and events on the hook debug log, which is pure
noise — same filter as the poll path."
  (let* ((descriptor (nth 0 event))
         (action     (nth 1 event))
         (file       (nth 2 event))
         (fname      (and (stringp file) (file-name-nondirectory file))))
    (cond
     ((not (stringp file))
      (claude-repl--log-verbose nil ">>> SENTINEL EVENT SKIPPED: action=%s no file in event=%S"
                        action event))
     ((string= fname claude-repl-sentinel-debug-log-filename)
      nil)
     (t
      (let ((exists (file-exists-p file)))
        (claude-repl--log nil ">>> SENTINEL EVENT: action=%s file=%s exists=%s descriptor=%S event=%S"
                          action fname exists descriptor event)
        (if (and (memq action claude-repl-sentinel-dispatch-actions) exists)
            (let ((result (claude-repl--dispatch-sentinel-file file)))
              (claude-repl--log nil ">>> SENTINEL EVENT DONE: file=%s dispatched=%s" fname result))
          (claude-repl--log-verbose nil ">>> SENTINEL EVENT SKIPPED: action=%s file=%s exists=%s (need created/changed + exists)"
                            action fname exists)))))))

;;; Polling fallback

(defun claude-repl--poll-workspace-notifications ()
  "Scan the sentinel directory for files that file-notify may have missed.
Called periodically as a fallback; any file still present was not picked up
by the file-notify watcher and needs processing."
  (let* ((dir-exists (file-directory-p claude-repl--sentinel-dir))
         (files (if dir-exists
                    (directory-files claude-repl--sentinel-dir t claude-repl-sentinel-poll-file-regexp t)
                  nil))
         ;; Filter out the debug log itself
         (files (cl-remove-if (lambda (f) (string= (file-name-nondirectory f) claude-repl-sentinel-debug-log-filename)) files)))
    (when files
      (claude-repl--log nil "poll-notifications: found %d orphaned file(s): %s"
                        (length files)
                        (mapconcat #'file-name-nondirectory files ", "))
      (dolist (file files)
        (if (file-exists-p file)
            (progn
              (claude-repl--log nil "poll-notifications: processing orphan file=%s" (file-name-nondirectory file))
              (unless (claude-repl--dispatch-sentinel-file file)
                (claude-repl--log nil "poll-notifications: ignoring unknown file %s"
                                  (file-name-nondirectory file))))
          (claude-repl--log nil "poll-notifications: file disappeared before processing: %s"
                            (file-name-nondirectory file)))))))

;; ---------------------------------------------------------------------------
;; File-notify watcher registration (top-level side effect)
;; ---------------------------------------------------------------------------

(defvar claude-repl--sentinel-watch-descriptor nil
  "File-notify descriptor for the sentinel directory watcher.
Stored so we can remove the old watcher before registering a new one
when sentinel.el is reloaded.")

(defun claude-repl--reap-sentinel-watchers ()
  "Remove every file-notify watcher on `claude-repl--sentinel-dir'.
Returns the count removed.  Iterates `file-notify-descriptors' rather
than relying on `claude-repl--sentinel-watch-descriptor', so it also
reclaims descriptors leaked across module reloads where that variable
lost track of the old descriptor."
  (let ((target (file-truename claude-repl--sentinel-dir))
        (removed 0))
    (maphash
     (lambda (desc watch)
       (let ((watch-dir (cond
                         ((and (fboundp 'file-notify--watch-p)
                               (file-notify--watch-p watch))
                          (file-notify--watch-directory watch))
                         ((consp watch) (car watch)))))
         (when (and watch-dir (string= (file-truename watch-dir) target))
           (file-notify-rm-watch desc)
           (cl-incf removed))))
     file-notify-descriptors)
    removed))

(defun claude-repl-reset-sentinel-watchers ()
  "Remove all file-notify watchers on the sentinel dir and re-register one.
Interactive recovery for the reload-accumulated duplicate-watcher case."
  (interactive)
  (let ((removed (claude-repl--reap-sentinel-watchers)))
    (setq claude-repl--sentinel-watch-descriptor
          (file-notify-add-watch claude-repl--sentinel-dir claude-repl-sentinel-watch-events
                                 #'claude-repl--dispatch-sentinel-event))
    (message "claude-repl: removed %d stale watcher(s); new descriptor=%S"
             removed claude-repl--sentinel-watch-descriptor)))

(defun claude-repl-nuke-sentinel-watchers ()
  "Remove every file-notify watcher on the sentinel dir WITHOUT re-registering.
Intended for testing: after nuking, re-eval sentinel.el (or just the
top-level init block) and confirm that exactly one watcher is created.
Useful to verify the init-time reap logic works without restarting Emacs."
  (interactive)
  (let ((removed (claude-repl--reap-sentinel-watchers)))
    (setq claude-repl--sentinel-watch-descriptor nil)
    (message "claude-repl: nuked %d sentinel watcher(s) — no replacement registered"
             removed)))

(make-directory claude-repl--sentinel-dir t)
(let ((reaped (claude-repl--reap-sentinel-watchers)))
  (when (> reaped 0)
    (claude-repl--log nil "sentinel-init: reaped %d stale watcher(s)" reaped)))
(setq claude-repl--sentinel-watch-descriptor
      (file-notify-add-watch claude-repl--sentinel-dir claude-repl-sentinel-watch-events
                             #'claude-repl--dispatch-sentinel-event))
(claude-repl--log nil "sentinel-init: registered watcher descriptor=%S"
                  claude-repl--sentinel-watch-descriptor)
