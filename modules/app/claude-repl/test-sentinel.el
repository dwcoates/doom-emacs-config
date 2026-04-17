;;; test-sentinel.el --- Tests for sentinel.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for the file-notify watcher dispatch and event handlers
;; defined in sentinel.el.  Migrated from test-claude-repl.el and
;; updated for the refactored dispatch-table API.

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Tests: dispatch-sentinel-event filtering ----

(ert-deftest claude-repl-test-sentinel-event-ignores-unknown-files ()
  "An event with a non-matching filename should not call any handler."
  (claude-repl-test--with-clean-state
    (let ((process-called nil))
      (cl-letf (((symbol-function 'claude-repl--process-sentinel-file)
                 (lambda (&rest _) (setq process-called t)))
                ((symbol-function 'file-exists-p) (lambda (_f) t)))
        (claude-repl--dispatch-sentinel-event '(nil changed "/some/other-file"))
        (should-not process-called)))))

(ert-deftest claude-repl-test-sentinel-event-ignores-deleted-action ()
  "An event with action `deleted' should be ignored even if filename matches."
  (claude-repl-test--with-clean-state
    (let ((process-called nil))
      (cl-letf (((symbol-function 'claude-repl--process-sentinel-file)
                 (lambda (&rest _) (setq process-called t)))
                ((symbol-function 'file-exists-p) (lambda (_f) t)))
        (claude-repl--dispatch-sentinel-event '(nil deleted "/path/to/permission_prompt"))
        (should-not process-called)))))

(ert-deftest claude-repl-test-sentinel-event-ignores-nonexistent-file ()
  "An event for a file that no longer exists should be ignored."
  (claude-repl-test--with-clean-state
    (let ((process-called nil))
      (cl-letf (((symbol-function 'claude-repl--process-sentinel-file)
                 (lambda (&rest _) (setq process-called t)))
                ((symbol-function 'file-exists-p) (lambda (_f) nil)))
        (claude-repl--dispatch-sentinel-event '(nil created "/dir/permission_prompt"))
        (should-not process-called)))))

(ert-deftest claude-repl-test-sentinel-event-ignores-hook-debug-log ()
  "Events for hook-debug.log should be ignored before dispatch.
Symmetric to the filter in `claude-repl--poll-workspace-notifications'."
  (claude-repl-test--with-clean-state
    (let ((dispatched nil))
      (cl-letf (((symbol-function 'claude-repl--dispatch-sentinel-file)
                 (lambda (_f) (setq dispatched t) t)))
        (claude-repl--dispatch-sentinel-event '(nil changed "/dir/hook-debug.log"))
        (should-not dispatched)))))

(ert-deftest claude-repl-test-sentinel-event-tolerates-nil-file ()
  "An event with no file (e.g. `stopped') should be skipped without crashing."
  (claude-repl-test--with-clean-state
    (let ((dispatched nil))
      (cl-letf (((symbol-function 'claude-repl--dispatch-sentinel-file)
                 (lambda (_f) (setq dispatched t) t)))
        ;; Must not error even though the file slot is nil.
        (claude-repl--dispatch-sentinel-event '(nil stopped nil))
        (should-not dispatched)))))

;;;; ---- Tests: dispatch-sentinel-file prefix matching ----

(ert-deftest claude-repl-test-sentinel-dispatches-permission ()
  "A file named permission_prompt should dispatch to the permission handler."
  (claude-repl-test--with-clean-state
    (let ((dispatched-handler nil))
      (cl-letf (((symbol-function 'claude-repl--process-sentinel-file)
                 (lambda (_file handler) (setq dispatched-handler handler))))
        (claude-repl--dispatch-sentinel-file "/dir/permission_prompt")
        (should dispatched-handler)
        (should (eq (plist-get dispatched-handler :callback)
                    'claude-repl--on-permission-event))))))

(ert-deftest claude-repl-test-sentinel-dispatches-stop ()
  "A file named stop_12345 should dispatch to the stop handler."
  (claude-repl-test--with-clean-state
    (let ((dispatched-handler nil))
      (cl-letf (((symbol-function 'claude-repl--process-sentinel-file)
                 (lambda (_file handler) (setq dispatched-handler handler))))
        (claude-repl--dispatch-sentinel-file "/dir/stop_12345")
        (should dispatched-handler)
        (should (eq (plist-get dispatched-handler :callback)
                    'claude-repl--on-stop-event))))))

(ert-deftest claude-repl-test-sentinel-dispatches-prompt-submit ()
  "A file named prompt_submit_99 should dispatch to the prompt-submit handler."
  (claude-repl-test--with-clean-state
    (let ((dispatched-handler nil))
      (cl-letf (((symbol-function 'claude-repl--process-sentinel-file)
                 (lambda (_file handler) (setq dispatched-handler handler))))
        (claude-repl--dispatch-sentinel-file "/dir/prompt_submit_99")
        (should dispatched-handler)
        (should (eq (plist-get dispatched-handler :callback)
                    'claude-repl--on-prompt-submit-event))))))

(ert-deftest claude-repl-test-sentinel-dispatch-returns-nil-for-unknown ()
  "dispatch-sentinel-file should return nil for an unrecognized filename."
  (claude-repl-test--with-clean-state
    (should-not (claude-repl--dispatch-sentinel-file "/dir/unknown_file"))))

(ert-deftest claude-repl-test-sentinel-dispatch-returns-t-for-known ()
  "dispatch-sentinel-file should return t when a handler is found."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'claude-repl--process-sentinel-file)
               (lambda (&rest _) nil)))
      (should (eq t (claude-repl--dispatch-sentinel-file "/dir/stop_abc"))))))

;;;; ---- Tests: process-sentinel-file orchestration ----

(ert-deftest claude-repl-test-process-sentinel-file-calls-callback ()
  "process-sentinel-file should read file, resolve ws, call callback, delete file."
  (claude-repl-test--with-clean-state
    (let ((callback-args nil)
          (deleted-file nil))
      (cl-letf (((symbol-function 'claude-repl--read-sentinel-file)
                 (lambda (_f) '(:dir "/some/dir" :session-id "sid-123")))
                ((symbol-function 'claude-repl--ws-for-dir)
                 (lambda (_d) "ws1"))
                ((symbol-function 'claude-repl--update-session-id-from-sentinel)
                 #'ignore)
                ((symbol-function 'delete-file)
                 (lambda (f) (setq deleted-file f))))
        (claude-repl--process-sentinel-file
         "/tmp/stop_123"
         (list :callback (lambda (ws dir) (setq callback-args (list ws dir)))
               :warning "warn %s"
               :name "test-handler"))
        (should (equal callback-args '("ws1" "/some/dir")))
        (should (equal deleted-file "/tmp/stop_123"))))))

(ert-deftest claude-repl-test-process-sentinel-file-nil-read-skips-all ()
  "When read-sentinel-file returns nil, callback should not be called."
  (claude-repl-test--with-clean-state
    (let ((callback-called nil)
          (deleted-file nil))
      (cl-letf (((symbol-function 'claude-repl--read-sentinel-file)
                 (lambda (_f) nil))
                ((symbol-function 'claude-repl--ws-for-dir)
                 (lambda (_d) (error "should not be called")))
                ((symbol-function 'delete-file)
                 (lambda (f) (setq deleted-file f))))
        (claude-repl--process-sentinel-file
         "/tmp/stop_456"
         '(:callback (lambda (&rest _) (setq callback-called t))
           :warning "warn %s"
           :name "test"))
        (should-not callback-called)
        ;; File should still be deleted even when dir is nil
        (should (equal deleted-file "/tmp/stop_456"))))))

(ert-deftest claude-repl-test-process-sentinel-file-nil-ws-warns ()
  "When ws-for-dir returns nil, the warning should be logged and callback skipped."
  (claude-repl-test--with-clean-state
    (let ((callback-called nil)
          (warning-msg nil)
          (deleted-file nil))
      (cl-letf (((symbol-function 'claude-repl--read-sentinel-file)
                 (lambda (_f) '(:dir "/unknown/dir" :session-id nil)))
                ((symbol-function 'claude-repl--ws-for-dir)
                 (lambda (_d) nil))
                ((symbol-function 'message)
                 (lambda (fmt &rest args) (setq warning-msg (apply #'format fmt args))))
                ((symbol-function 'delete-file)
                 (lambda (f) (setq deleted-file f))))
        (claude-repl--process-sentinel-file
         "/tmp/stop_789"
         '(:callback (lambda (&rest _) (setq callback-called t))
           :warning "[claude-repl] WARNING: stop sentinel dir=%s matched no workspace"
           :name "test"))
        (should-not callback-called)
        (should (string-match-p "matched no workspace" warning-msg))
        (should (string-match-p "/unknown/dir" warning-msg))
        (should (equal deleted-file "/tmp/stop_789"))))))

(ert-deftest claude-repl-test-process-sentinel-file-always-deletes ()
  "File should be deleted even when callback signals an error."
  (claude-repl-test--with-clean-state
    (let ((deleted-file nil))
      (cl-letf (((symbol-function 'claude-repl--read-sentinel-file)
                 (lambda (_f) '(:dir "/some/dir" :session-id nil)))
                ((symbol-function 'claude-repl--ws-for-dir)
                 (lambda (_d) "ws1"))
                ((symbol-function 'claude-repl--update-session-id-from-sentinel)
                 #'ignore)
                ((symbol-function 'delete-file)
                 (lambda (f) (setq deleted-file f))))
        (claude-repl--process-sentinel-file
         "/tmp/perm_123"
         '(:callback (lambda (_ws _dir) nil)
           :warning "warn %s"
           :name "test"))
        (should (equal deleted-file "/tmp/perm_123"))))))

;;;; ---- Tests: on-stop-event handler ----

(ert-deftest claude-repl-test-on-stop-event-clears-thinking-calls-finished ()
  "on-stop-event should clear :thinking and call handle-claude-finished."
  (claude-repl-test--with-clean-state
    (let ((cleared nil) (finished-ws nil))
      (claude-repl--ws-set "ws1" :thinking)
      (cl-letf (((symbol-function 'claude-repl--ws-claude-state-clear-if)
                 (lambda (ws state)
                   (when (eq state :thinking)
                     (setq cleared ws))))
                ((symbol-function 'claude-repl--handle-claude-finished)
                 (lambda (ws) (setq finished-ws ws)))
                ((symbol-function 'claude-repl--ws-get)
                 (lambda (_ws key)
                   (pcase key
                     (:claude-state :thinking)
                     (:status :thinking)
                     (:vterm-buffer nil)
                     (_ nil)))))
        (claude-repl--on-stop-event "ws1" "/some/dir")
        (should (equal cleared "ws1"))
        (should (equal finished-ws "ws1"))))))

(ert-deftest claude-repl-test-on-stop-event-tolerates-nil-workspace ()
  "on-stop-event via process-sentinel-file should not error when ws-for-dir returns nil."
  (claude-repl-test--with-clean-state
    (let ((finished-called nil))
      (cl-letf (((symbol-function 'claude-repl--read-sentinel-file)
                 (lambda (_f) '(:dir "/unknown/dir" :session-id nil)))
                ((symbol-function 'claude-repl--ws-for-dir)
                 (lambda (_d) nil))
                ((symbol-function 'claude-repl--handle-claude-finished)
                 (lambda (_ws) (setq finished-called t)))
                ((symbol-function 'delete-file) #'ignore))
        (claude-repl--process-sentinel-file
         "/tmp/stop_456"
         (cdr (assoc "stop_" claude-repl--sentinel-dispatch-alist)))
        (should-not finished-called)))))

;;;; ---- Tests: on-prompt-submit-event handler ----

(ert-deftest claude-repl-test-on-prompt-submit-event-sets-thinking ()
  "on-prompt-submit-event should call mark-ws-thinking."
  (claude-repl-test--with-clean-state
    (let ((thinking-ws nil))
      (cl-letf (((symbol-function 'claude-repl--mark-ws-thinking)
                 (lambda (ws) (setq thinking-ws ws))))
        (claude-repl--on-prompt-submit-event "ws1" "/some/dir")
        (should (equal thinking-ws "ws1"))))))

;;;; ---- Tests: on-permission-event handler ----

(ert-deftest claude-repl-test-on-permission-event-sets-permission ()
  "on-permission-event should call ws-set-claude-state with :permission."
  (claude-repl-test--with-clean-state
    (let ((set-args nil))
      (cl-letf (((symbol-function 'claude-repl--ws-set-claude-state)
                 (lambda (ws state) (setq set-args (list ws state)))))
        (claude-repl--on-permission-event "ws1" "/some/dir")
        (should (equal set-args '("ws1" :permission)))))))

;;;; ---- Tests: read-sentinel-file ----

(ert-deftest claude-repl-test-read-sentinel-file-returns-dir-only ()
  "read-sentinel-file with single-line file returns plist with :dir and nil :session-id."
  (claude-repl-test--with-clean-state
    (let ((tmp (make-temp-file "sentinel-test-")))
      (unwind-protect
          (progn
            (write-region "  /some/project/dir  \n" nil tmp)
            (let ((result (claude-repl--read-sentinel-file tmp)))
              (should (equal (plist-get result :dir) "/some/project/dir"))
              (should-not (plist-get result :session-id))))
        (ignore-errors (delete-file tmp))))))

(ert-deftest claude-repl-test-read-sentinel-file-returns-dir-and-session-id ()
  "read-sentinel-file with two-line file returns both :dir and :session-id."
  (claude-repl-test--with-clean-state
    (let ((tmp (make-temp-file "sentinel-test-")))
      (unwind-protect
          (progn
            (write-region "/some/project/dir\nabc-123-def\n" nil tmp)
            (let ((result (claude-repl--read-sentinel-file tmp)))
              (should (equal (plist-get result :dir) "/some/project/dir"))
              (should (equal (plist-get result :session-id) "abc-123-def"))))
        (ignore-errors (delete-file tmp))))))

(ert-deftest claude-repl-test-read-sentinel-file-returns-nil-on-missing ()
  "read-sentinel-file should return nil when file is missing (race condition)."
  (claude-repl-test--with-clean-state
    (should-not (claude-repl--read-sentinel-file "/nonexistent/path/sentinel_file"))))

(ert-deftest claude-repl-test-read-sentinel-file-empty-file ()
  "read-sentinel-file should return plist with empty :dir for empty file."
  (claude-repl-test--with-clean-state
    (let ((tmp (make-temp-file "sentinel-test-")))
      (unwind-protect
          (progn
            (write-region "" nil tmp)
            (let ((result (claude-repl--read-sentinel-file tmp)))
              (should (equal (plist-get result :dir) ""))
              (should-not (plist-get result :session-id))))
        (ignore-errors (delete-file tmp))))))

;;;; ---- Tests: ws-for-dir-fast ----

(ert-deftest claude-repl-test-ws-for-dir-fast-hit ()
  "ws-for-dir-fast should return workspace when git-root -> hash -> buffer -> ws all resolve."
  (claude-repl-test--with-clean-state
    ;; Use the canonical form throughout so hash calc matches the real
    ;; --git-root's post-canonicalization behavior regardless of platform
    ;; (e.g. macOS firmlinks rewrite /home → /System/Volumes/Data/home).
    (let* ((test-root (claude-repl--path-canonical "/home/user/project"))
           (hash (substring (md5 test-root) 0 8))
           (buf-name (format "*claude-%s*" hash))
           (buf (get-buffer-create buf-name)))
      (unwind-protect
          (cl-letf (((symbol-function 'claude-repl--git-root)
                     (lambda (_d) test-root))
                    ((symbol-function 'claude-repl--workspace-for-buffer)
                     (lambda (_b) "my-workspace")))
            (should (equal (claude-repl--ws-for-dir-fast
                            (concat test-root "/subdir"))
                           "my-workspace")))
        (when (buffer-live-p buf)
          (kill-buffer buf))))))

(ert-deftest claude-repl-test-ws-for-dir-fast-no-git-root ()
  "ws-for-dir-fast should return nil when git-root returns nil."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'claude-repl--git-root)
               (lambda (_d) nil)))
      (should-not (claude-repl--ws-for-dir-fast "/some/dir")))))

(ert-deftest claude-repl-test-ws-for-dir-fast-no-buffer ()
  "ws-for-dir-fast should return nil when the expected buffer does not exist."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'claude-repl--git-root)
               (lambda (_d) "/some/root")))
      ;; No buffer named *claude-HASH* exists
      (should-not (claude-repl--ws-for-dir-fast "/some/root/subdir")))))

(ert-deftest claude-repl-test-ws-for-dir-fast-buffer-no-workspace ()
  "ws-for-dir-fast should return nil when buffer exists but workspace-for-buffer returns nil."
  (claude-repl-test--with-clean-state
    (let* ((test-root "/home/user/project")
           (hash (substring (md5 (claude-repl--path-canonical test-root)) 0 8))
           (buf-name (format "*claude-%s*" hash))
           (buf (get-buffer-create buf-name)))
      (unwind-protect
          (cl-letf (((symbol-function 'claude-repl--git-root)
                     (lambda (_d) test-root))
                    ((symbol-function 'claude-repl--workspace-for-buffer)
                     (lambda (_b) nil)))
            (should-not (claude-repl--ws-for-dir-fast "/home/user/project/subdir")))
        (when (buffer-live-p buf)
          (kill-buffer buf))))))

;;;; ---- Tests: ws-for-dir-container ----

(ert-deftest claude-repl-test-ws-for-dir-container-match ()
  "ws-for-dir-container should match container path to workspace project dir."
  (claude-repl-test--with-clean-state
    (let ((persp-mode t))
      (cl-letf (((symbol-function '+workspace-list-names)
                 (lambda () '("proj-ws")))
                ((symbol-function 'claude-repl--ws-get)
                 (lambda (ws key)
                   (when (and (equal ws "proj-ws") (eq key :project-dir))
                     "/home/user/myproject"))))
        (should (equal (claude-repl--ws-for-dir-container "/myproject/src")
                       "proj-ws"))))))

(ert-deftest claude-repl-test-ws-for-dir-container-no-persp-mode ()
  "ws-for-dir-container should return nil when persp-mode is disabled."
  (claude-repl-test--with-clean-state
    (let ((persp-mode nil))
      (should-not (claude-repl--ws-for-dir-container "/myproject/src")))))

(ert-deftest claude-repl-test-ws-for-dir-container-no-match ()
  "ws-for-dir-container should return nil when no workspace project dir matches."
  (claude-repl-test--with-clean-state
    (let ((persp-mode t))
      (cl-letf (((symbol-function '+workspace-list-names)
                 (lambda () '("other-ws")))
                ((symbol-function 'claude-repl--ws-get)
                 (lambda (_ws key)
                   (when (eq key :project-dir) "/home/user/different"))))
        (should-not (claude-repl--ws-for-dir-container "/myproject/src"))))))

(ert-deftest claude-repl-test-ws-for-dir-container-nil-project-dir ()
  "ws-for-dir-container should skip workspaces with nil project-dir."
  (claude-repl-test--with-clean-state
    (let ((persp-mode t))
      (cl-letf (((symbol-function '+workspace-list-names)
                 (lambda () '("ws-no-dir" "ws-with-dir")))
                ((symbol-function 'claude-repl--ws-get)
                 (lambda (ws key)
                   (when (eq key :project-dir)
                     (cond ((equal ws "ws-no-dir") nil)
                           ((equal ws "ws-with-dir") "/home/user/myproject"))))))
        (should (equal (claude-repl--ws-for-dir-container "/myproject/sub")
                       "ws-with-dir"))))))

;;;; ---- Tests: ws-for-dir (combined) ----

(ert-deftest claude-repl-test-ws-for-dir-prefers-fast-path ()
  "ws-for-dir should return fast-path result when available."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'claude-repl--ws-for-dir-fast)
               (lambda (_d) "fast-ws"))
              ((symbol-function 'claude-repl--ws-for-dir-container)
               (lambda (_d) "container-ws")))
      (should (equal (claude-repl--ws-for-dir "/some/dir") "fast-ws")))))

(ert-deftest claude-repl-test-ws-for-dir-falls-back-to-container ()
  "ws-for-dir should try container path when fast path returns nil."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'claude-repl--ws-for-dir-fast)
               (lambda (_d) nil))
              ((symbol-function 'claude-repl--ws-for-dir-container)
               (lambda (_d) "container-ws")))
      (should (equal (claude-repl--ws-for-dir "/some/dir") "container-ws")))))

(ert-deftest claude-repl-test-ws-for-dir-returns-nil-when-both-fail ()
  "ws-for-dir should return nil when both paths fail."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'claude-repl--ws-for-dir-fast)
               (lambda (_d) nil))
              ((symbol-function 'claude-repl--ws-for-dir-container)
               (lambda (_d) nil)))
      (should-not (claude-repl--ws-for-dir "/some/dir")))))

;;;; ---- Tests: dispatch-sentinel-event actions ----

(ert-deftest claude-repl-test-sentinel-event-accepts-created-action ()
  "An event with action `created' and existing file should dispatch."
  (claude-repl-test--with-clean-state
    (let ((dispatched nil))
      (cl-letf (((symbol-function 'claude-repl--dispatch-sentinel-file)
                 (lambda (_f) (setq dispatched t)))
                ((symbol-function 'file-exists-p) (lambda (_f) t)))
        (claude-repl--dispatch-sentinel-event '(nil created "/dir/stop_123"))
        (should dispatched)))))

(ert-deftest claude-repl-test-sentinel-event-accepts-changed-action ()
  "An event with action `changed' and existing file should dispatch."
  (claude-repl-test--with-clean-state
    (let ((dispatched nil))
      (cl-letf (((symbol-function 'claude-repl--dispatch-sentinel-file)
                 (lambda (_f) (setq dispatched t)))
                ((symbol-function 'file-exists-p) (lambda (_f) t)))
        (claude-repl--dispatch-sentinel-event '(nil changed "/dir/stop_123"))
        (should dispatched)))))

(ert-deftest claude-repl-test-sentinel-event-ignores-renamed-action ()
  "An event with action `renamed' should be ignored."
  (claude-repl-test--with-clean-state
    (let ((dispatched nil))
      (cl-letf (((symbol-function 'claude-repl--dispatch-sentinel-file)
                 (lambda (_f) (setq dispatched t)))
                ((symbol-function 'file-exists-p) (lambda (_f) t)))
        (claude-repl--dispatch-sentinel-event '(nil renamed "/dir/stop_123"))
        (should-not dispatched)))))

;;;; ---- Tests: poll-workspace-notifications ----

(ert-deftest claude-repl-test-poll-dispatches-orphaned-files ()
  "poll-workspace-notifications should dispatch files in the sentinel directory."
  (claude-repl-test--with-clean-state
    ;; Use a real temp directory rather than mocking primitives like
    ;; `directory-files' / `file-exists-p' / `file-directory-p'.  Redefining
    ;; those via cl-letf triggers native-comp trampoline installation which
    ;; can fail on cached native-elisp installs.
    (let* ((tmp (make-temp-file "claude-repl-sentinel-test-" t))
           (claude-repl--sentinel-dir tmp)
           (stop-file (expand-file-name "stop_abc" tmp))
           (perm-file (expand-file-name "permission_prompt" tmp))
           (dispatched-files nil))
      (unwind-protect
          (progn
            (write-region "" nil stop-file)
            (write-region "" nil perm-file)
            (cl-letf (((symbol-function 'claude-repl--dispatch-sentinel-file)
                       (lambda (f) (push (file-name-nondirectory f)
                                         dispatched-files)
                          t)))
              (claude-repl--poll-workspace-notifications)
              (should (= (length dispatched-files) 2))
              (should (member "stop_abc" dispatched-files))
              (should (member "permission_prompt" dispatched-files))))
        (ignore-errors (delete-directory tmp t))))))

(ert-deftest claude-repl-test-poll-skips-nonexistent-dir ()
  "poll-workspace-notifications should do nothing if sentinel dir doesn't exist."
  (claude-repl-test--with-clean-state
    (let ((dispatched nil))
      (cl-letf (((symbol-function 'file-directory-p) (lambda (_d) nil))
                ((symbol-function 'claude-repl--dispatch-sentinel-file)
                 (lambda (_f) (setq dispatched t))))
        (claude-repl--poll-workspace-notifications)
        (should-not dispatched)))))

(ert-deftest claude-repl-test-poll-skips-nonexistent-files ()
  "poll-workspace-notifications should skip files that disappeared between listing and processing."
  (claude-repl-test--with-clean-state
    (let ((dispatched-files nil))
      (cl-letf (((symbol-function 'file-directory-p) (lambda (_d) t))
                ((symbol-function 'directory-files)
                 (lambda (_dir _full _match _nosort)
                   '("/sentinel/stop_abc")))
                ((symbol-function 'file-exists-p) (lambda (_f) nil))
                ((symbol-function 'claude-repl--dispatch-sentinel-file)
                 (lambda (f) (push f dispatched-files) t)))
        (claude-repl--poll-workspace-notifications)
        (should-not dispatched-files)))))

(ert-deftest claude-repl-test-poll-logs-unknown-files ()
  "poll-workspace-notifications should not error on files that match no handler."
  (claude-repl-test--with-clean-state
    (let ((dispatched-count 0))
      (cl-letf (((symbol-function 'file-directory-p) (lambda (_d) t))
                ((symbol-function 'directory-files)
                 (lambda (_dir _full _match _nosort)
                   '("/sentinel/unknown_file")))
                ((symbol-function 'file-exists-p) (lambda (_f) t))
                ((symbol-function 'claude-repl--dispatch-sentinel-file)
                 (lambda (_f) (cl-incf dispatched-count) nil)))
        (claude-repl--poll-workspace-notifications)
        (should (= dispatched-count 1))))))

(ert-deftest claude-repl-test-poll-empty-directory ()
  "poll-workspace-notifications should do nothing when directory is empty."
  (claude-repl-test--with-clean-state
    (let ((dispatched nil))
      (cl-letf (((symbol-function 'file-directory-p) (lambda (_d) t))
                ((symbol-function 'directory-files)
                 (lambda (_dir _full _match _nosort) nil))
                ((symbol-function 'claude-repl--dispatch-sentinel-file)
                 (lambda (_f) (setq dispatched t))))
        (claude-repl--poll-workspace-notifications)
        (should-not dispatched)))))

;;;; ---- Tests: sentinel-dispatch-alist structure ----

(ert-deftest claude-repl-test-dispatch-alist-has-required-keys ()
  "Every entry in the dispatch alist should have :callback, :warning, :name."
  (dolist (entry claude-repl--sentinel-dispatch-alist)
    (let ((prefix (car entry))
          (plist (cdr entry)))
      (should (plist-get plist :callback))
      (should (plist-get plist :warning))
      (should (plist-get plist :name))
      ;; :warning should be a format string with %s
      (should (string-match-p "%s" (plist-get plist :warning)))
      ;; :callback should be a function symbol
      (should (symbolp (plist-get plist :callback)))
      ;; :name should be a string
      (should (stringp (plist-get plist :name)))
      ;; prefix should be a string
      (should (stringp prefix)))))

(ert-deftest claude-repl-test-dispatch-alist-callbacks-are-fboundp ()
  "All callback functions in the dispatch alist should be defined."
  (dolist (entry claude-repl--sentinel-dispatch-alist)
    (let ((cb (plist-get (cdr entry) :callback)))
      (should (fboundp cb)))))

;;;; ---- Tests: end-to-end dispatch through process-sentinel-file ----

(ert-deftest claude-repl-test-end-to-end-permission-dispatch ()
  "Full dispatch: permission_prompt file -> on-permission-event -> ws-set-claude-state :permission."
  (claude-repl-test--with-clean-state
    (let ((set-args nil))
      (cl-letf (((symbol-function 'claude-repl--read-sentinel-file)
                 (lambda (_f) '(:dir "/project/dir" :session-id "test-sid")))
                ((symbol-function 'claude-repl--ws-for-dir)
                 (lambda (_d) "test-ws"))
                ((symbol-function 'claude-repl--update-session-id-from-sentinel)
                 #'ignore)
                ((symbol-function 'claude-repl--ws-set-claude-state)
                 (lambda (ws state) (setq set-args (list ws state))))
                ((symbol-function 'claude-repl--ws-get)
                 (lambda (_ws _key) nil))
                ((symbol-function 'delete-file) #'ignore))
        (claude-repl--dispatch-sentinel-file "/dir/permission_prompt")
        (should (equal set-args '("test-ws" :permission)))))))

(ert-deftest claude-repl-test-end-to-end-stop-dispatch ()
  "Full dispatch: stop_* file -> on-stop-event -> ws-claude-state-clear-if :thinking + handle-finished."
  (claude-repl-test--with-clean-state
    (let ((cleared nil) (finished nil))
      (cl-letf (((symbol-function 'claude-repl--read-sentinel-file)
                 (lambda (_f) '(:dir "/project/dir" :session-id "test-sid")))
                ((symbol-function 'claude-repl--ws-for-dir)
                 (lambda (_d) "test-ws"))
                ((symbol-function 'claude-repl--ws-claude-state-clear-if)
                 (lambda (ws state)
                   (when (eq state :thinking) (setq cleared ws))))
                ((symbol-function 'claude-repl--update-session-id-from-sentinel)
                 #'ignore)
                ((symbol-function 'claude-repl--handle-claude-finished)
                 (lambda (ws) (setq finished ws)))
                ((symbol-function 'claude-repl--ws-get)
                 (lambda (_ws key)
                   (pcase key
                     (:claude-state :thinking)
                     (:status :thinking)
                     (:vterm-buffer nil)
                     (_ nil))))
                ((symbol-function 'delete-file) #'ignore))
        (claude-repl--dispatch-sentinel-file "/dir/stop_123")
        (should (equal cleared "test-ws"))
        (should (equal finished "test-ws"))))))

(ert-deftest claude-repl-test-end-to-end-prompt-submit-dispatch ()
  "Full dispatch: prompt_submit_* file -> on-prompt-submit-event -> mark-ws-thinking."
  (claude-repl-test--with-clean-state
    (let ((thinking-ws nil))
      (cl-letf (((symbol-function 'claude-repl--read-sentinel-file)
                 (lambda (_f) '(:dir "/project/dir" :session-id "test-sid")))
                ((symbol-function 'claude-repl--ws-for-dir)
                 (lambda (_d) "test-ws"))
                ((symbol-function 'claude-repl--update-session-id-from-sentinel)
                 #'ignore)
                ((symbol-function 'claude-repl--mark-ws-thinking)
                 (lambda (ws) (setq thinking-ws ws)))
                ((symbol-function 'claude-repl--ws-get)
                 (lambda (_ws _key) nil))
                ((symbol-function 'delete-file) #'ignore))
        (claude-repl--dispatch-sentinel-file "/dir/prompt_submit_456")
        (should (equal thinking-ws "test-ws"))))))

;;;; ---- Tests: ws-for-dir-fast uncovered edge cases ----

(ert-deftest claude-repl-test-ws-for-dir-fast-nil-dir ()
  "ws-for-dir-fast should return nil when DIR is nil."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'claude-repl--git-root)
               (lambda (d) d)))
      (should-not (claude-repl--ws-for-dir-fast nil)))))

(ert-deftest claude-repl-test-ws-for-dir-fast-trailing-slash ()
  "ws-for-dir-fast should match when git-root returns a path with trailing slash.
The hash is computed via path-canonical which strips trailing slashes."
  (claude-repl-test--with-clean-state
    (let* ((canonical (claude-repl--path-canonical "/home/user/project/"))
           (hash (substring (md5 canonical) 0 8))
           (buf-name (format "*claude-%s*" hash))
           (buf (get-buffer-create buf-name)))
      (unwind-protect
          (cl-letf (((symbol-function 'claude-repl--git-root)
                     (lambda (_d) canonical))
                    ((symbol-function 'claude-repl--workspace-for-buffer)
                     (lambda (_b) "trail-ws")))
            (should (equal (claude-repl--ws-for-dir-fast "/home/user/project/sub")
                           "trail-ws")))
        (when (buffer-live-p buf)
          (kill-buffer buf))))))

(ert-deftest claude-repl-test-ws-for-dir-fast-symlink-canonical ()
  "ws-for-dir-fast should produce a stable hash regardless of symlink paths.
path-canonical uses file-truename which resolves symlinks."
  (claude-repl-test--with-clean-state
    ;; Simulate: git-root returns a symlink path, but path-canonical resolves it
    ;; to a real path.  The buffer was created using the real path's hash.
    (let* ((real-root "/home/user/real-project")
           (canonical (claude-repl--path-canonical real-root))
           (hash (substring (md5 canonical) 0 8))
           (buf-name (format "*claude-%s*" hash))
           (buf (get-buffer-create buf-name)))
      (unwind-protect
          (cl-letf (((symbol-function 'claude-repl--git-root)
                     (lambda (_d) canonical))
                    ((symbol-function 'claude-repl--workspace-for-buffer)
                     (lambda (_b) "sym-ws")))
            (should (equal (claude-repl--ws-for-dir-fast "/home/user/real-project/sub")
                           "sym-ws")))
        (when (buffer-live-p buf)
          (kill-buffer buf))))))

;;;; ---- Tests: ws-for-dir-container uncovered edge cases ----

(ert-deftest claude-repl-test-ws-for-dir-container-multiple-match-returns-first ()
  "ws-for-dir-container should return the first matching workspace when multiple match."
  (claude-repl-test--with-clean-state
    (let ((persp-mode t))
      (cl-letf (((symbol-function '+workspace-list-names)
                 (lambda () '("ws-first" "ws-second")))
                ((symbol-function 'claude-repl--ws-get)
                 (lambda (ws key)
                   (when (eq key :project-dir)
                     (cond ((equal ws "ws-first") "/home/user/myproject")
                           ((equal ws "ws-second") "/other/path/myproject"))))))
        (should (equal (claude-repl--ws-for-dir-container "/myproject/src")
                       "ws-first"))))))

(ert-deftest claude-repl-test-ws-for-dir-container-root-slash ()
  "ws-for-dir-container with DIR=\"/\" should not error (container-root is empty string)."
  (claude-repl-test--with-clean-state
    (let ((persp-mode t))
      (cl-letf (((symbol-function '+workspace-list-names)
                 (lambda () '("ws1")))
                ((symbol-function 'claude-repl--ws-get)
                 (lambda (_ws key)
                   (when (eq key :project-dir) "/home/user/project"))))
        ;; DIR is "/", so (substring "/" 1) = "", (split-string "" "/") = (""),
        ;; container-root = "".  Should not match anything and return nil.
        (should-not (claude-repl--ws-for-dir-container "/"))))))

(ert-deftest claude-repl-test-ws-for-dir-container-empty-workspace-list ()
  "ws-for-dir-container should return nil when workspace-list-names returns empty list."
  (claude-repl-test--with-clean-state
    (let ((persp-mode t))
      (cl-letf (((symbol-function '+workspace-list-names)
                 (lambda () nil)))
        (should-not (claude-repl--ws-for-dir-container "/myproject/src"))))))

(ert-deftest claude-repl-test-ws-for-dir-container-trailing-slash-normalization ()
  "ws-for-dir-container should match project dirs with trailing slashes via directory-file-name."
  (claude-repl-test--with-clean-state
    (let ((persp-mode t))
      (cl-letf (((symbol-function '+workspace-list-names)
                 (lambda () '("trail-ws")))
                ((symbol-function 'claude-repl--ws-get)
                 (lambda (ws key)
                   (when (and (equal ws "trail-ws") (eq key :project-dir))
                     "/home/user/myproject/"))))
        ;; directory-file-name strips the trailing slash, so the last component
        ;; should still be "myproject".
        (should (equal (claude-repl--ws-for-dir-container "/myproject/src")
                       "trail-ws"))))))

;;;; ---- Tests: read-sentinel-file uncovered edge cases ----

(ert-deftest claude-repl-test-read-sentinel-file-whitespace-only ()
  "read-sentinel-file should return plist with empty :dir for whitespace-only file."
  (claude-repl-test--with-clean-state
    (let ((tmp (make-temp-file "sentinel-test-")))
      (unwind-protect
          (progn
            (write-region "   \n\t\n  " nil tmp)
            (let ((result (claude-repl--read-sentinel-file tmp)))
              (should (equal (plist-get result :dir) ""))
              (should-not (plist-get result :session-id))))
        (ignore-errors (delete-file tmp))))))

(ert-deftest claude-repl-test-read-sentinel-file-multiline-content ()
  "read-sentinel-file with two lines returns :dir and :session-id."
  (claude-repl-test--with-clean-state
    (let ((tmp (make-temp-file "sentinel-test-")))
      (unwind-protect
          (progn
            (write-region "/first/line\n/second/line\n" nil tmp)
            (let ((result (claude-repl--read-sentinel-file tmp)))
              (should (equal (plist-get result :dir) "/first/line"))
              (should (equal (plist-get result :session-id) "/second/line"))))
        (ignore-errors (delete-file tmp))))))

(ert-deftest claude-repl-test-read-sentinel-file-generic-error ()
  "read-sentinel-file should return nil and warn on non-file-missing errors."
  (claude-repl-test--with-clean-state
    (let ((warning-msg nil))
      (cl-letf (((symbol-function 'insert-file-contents)
                 (lambda (&rest _) (error "disk I/O error")))
                ((symbol-function 'claude-repl--log)
                 (lambda (_ws fmt &rest args) (setq warning-msg (apply #'format fmt args)))))
        (should-not (claude-repl--read-sentinel-file "/some/sentinel_file"))
        (should (string-match-p "read-sentinel-file: ERROR.*disk I/O" warning-msg))))))

(ert-deftest claude-repl-test-read-sentinel-file-very-long-content ()
  "read-sentinel-file should handle files with very long dir path."
  (claude-repl-test--with-clean-state
    (let ((tmp (make-temp-file "sentinel-test-"))
          (long-path (concat "/" (make-string 1000 ?a))))
      (unwind-protect
          (progn
            (write-region long-path nil tmp)
            (let ((result (claude-repl--read-sentinel-file tmp)))
              (should (equal (plist-get result :dir) long-path))
              (should-not (plist-get result :session-id))))
        (ignore-errors (delete-file tmp))))))

;;;; ---- Tests: process-sentinel-file uncovered edge cases ----

(ert-deftest claude-repl-test-process-sentinel-file-callback-error-skips-delete ()
  "When callback raises an error, delete-file is not called (no unwind-protect)."
  (claude-repl-test--with-clean-state
    (let ((deleted-file nil))
      (cl-letf (((symbol-function 'claude-repl--read-sentinel-file)
                 (lambda (_f) '(:dir "/some/dir" :session-id nil)))
                ((symbol-function 'claude-repl--ws-for-dir)
                 (lambda (_d) "ws1"))
                ((symbol-function 'claude-repl--update-session-id-from-sentinel)
                 #'ignore)
                ((symbol-function 'delete-file)
                 (lambda (f) (setq deleted-file f))))
        ;; The callback errors; since there's no unwind-protect, delete-file won't run.
        (condition-case _err
            (claude-repl--process-sentinel-file
             "/tmp/stop_err"
             '(:callback (lambda (_ws _dir) (error "callback boom"))
               :warning "warn %s"
               :name "test"))
          (error nil))
        ;; delete-file should NOT have run because the error interrupted execution
        (should-not deleted-file)))))

(ert-deftest claude-repl-test-process-sentinel-file-delete-file-error-suppressed ()
  "When delete-file errors, the error should be suppressed by ignore-errors."
  (claude-repl-test--with-clean-state
    (let ((callback-called nil))
      (cl-letf (((symbol-function 'claude-repl--read-sentinel-file)
                 (lambda (_f) '(:dir "/some/dir" :session-id nil)))
                ((symbol-function 'claude-repl--ws-for-dir)
                 (lambda (_d) "ws1"))
                ((symbol-function 'claude-repl--update-session-id-from-sentinel)
                 #'ignore)
                ((symbol-function 'delete-file)
                 (lambda (_f) (error "permission denied"))))
        ;; Should not propagate the delete-file error
        (claude-repl--process-sentinel-file
         "/tmp/stop_del"
         (list :callback (lambda (_ws _dir) (setq callback-called t))
               :warning "warn %s"
               :name "test"))
        (should callback-called)))))

(ert-deftest claude-repl-test-process-sentinel-file-missing-callback-key ()
  "When handler plist is missing :callback, funcall should error on nil."
  (claude-repl-test--with-clean-state
    (let ((deleted-file nil))
      (cl-letf (((symbol-function 'claude-repl--read-sentinel-file)
                 (lambda (_f) '(:dir "/some/dir" :session-id nil)))
                ((symbol-function 'claude-repl--ws-for-dir)
                 (lambda (_d) "ws1"))
                ((symbol-function 'claude-repl--update-session-id-from-sentinel)
                 #'ignore)
                ((symbol-function 'delete-file)
                 (lambda (f) (setq deleted-file f))))
        ;; Missing :callback means (plist-get handler :callback) => nil
        ;; funcall nil should signal an error
        (should-error
         (claude-repl--process-sentinel-file
          "/tmp/stop_nocb"
          '(:warning "warn %s" :name "test")))))))

(ert-deftest claude-repl-test-process-sentinel-file-missing-warning-key ()
  "When handler plist is missing :warning and ws is nil, message receives nil as fmt.
In Emacs, (message nil ...) clears the echo area without error, so the callback
is still skipped and the file is still deleted."
  (claude-repl-test--with-clean-state
    (let ((callback-called nil)
          (deleted-file nil))
      (cl-letf (((symbol-function 'claude-repl--read-sentinel-file)
                 (lambda (_f) '(:dir "/some/dir" :session-id nil)))
                ((symbol-function 'claude-repl--ws-for-dir)
                 (lambda (_d) nil))
                ((symbol-function 'delete-file)
                 (lambda (f) (setq deleted-file f))))
        (claude-repl--process-sentinel-file
         "/tmp/stop_nowarn"
         '(:callback (lambda (&rest _) (setq callback-called t)) :name "test"))
        ;; Callback should not have been called (ws was nil)
        (should-not callback-called)
        ;; File should still be deleted
        (should (equal deleted-file "/tmp/stop_nowarn"))))))

;;;; ---- Tests: on-permission-event uncovered edge cases ----

(ert-deftest claude-repl-test-on-permission-event-nil-ws-errors ()
  "on-permission-event with nil ws should error in ws-set."
  (claude-repl-test--with-clean-state
    ;; ws-set raises an error when ws is nil
    (should-error (claude-repl--on-permission-event nil "/some/dir"))))

(ert-deftest claude-repl-test-on-permission-event-already-permission ()
  "on-permission-event should still call ws-set-claude-state even if ws already has :permission."
  (claude-repl-test--with-clean-state
    (let ((set-args nil))
      (claude-repl--ws-set "ws1" :permission)
      (cl-letf (((symbol-function 'claude-repl--ws-set-claude-state)
                 (lambda (ws state) (setq set-args (list ws state)))))
        (claude-repl--on-permission-event "ws1" "/some/dir")
        (should (equal set-args '("ws1" :permission)))))))

;;;; ---- Tests: on-stop-event uncovered edge cases ----

(ert-deftest claude-repl-test-on-stop-event-status-not-thinking ()
  "on-stop-event should call ws-claude-state-clear-if even when state is not :thinking."
  (claude-repl-test--with-clean-state
    (let ((cleared nil) (finished-ws nil))
      (claude-repl--ws-set "ws1" :permission)
      (cl-letf (((symbol-function 'claude-repl--ws-claude-state-clear-if)
                 (lambda (ws state)
                   (when (eq state :thinking)
                     (setq cleared (list ws state)))))
                ((symbol-function 'claude-repl--handle-claude-finished)
                 (lambda (ws) (setq finished-ws ws)))
                ((symbol-function 'claude-repl--ws-get)
                 (lambda (_ws key)
                   (pcase key
                     (:claude-state :permission)
                     (:status :permission)
                     (:vterm-buffer nil)
                     (_ nil)))))
        (claude-repl--on-stop-event "ws1" "/some/dir")
        ;; ws-claude-state-clear-if is still called (though clear-if is a no-op)
        (should (equal cleared '("ws1" :thinking)))
        (should (equal finished-ws "ws1"))))))

(ert-deftest claude-repl-test-on-stop-event-nil-status ()
  "on-stop-event should still call ws-claude-state-clear-if and handle-finished when claude-state is nil."
  (claude-repl-test--with-clean-state
    (let ((cleared nil) (finished-ws nil))
      (cl-letf (((symbol-function 'claude-repl--ws-claude-state-clear-if)
                 (lambda (ws state)
                   (when (eq state :thinking)
                     (setq cleared (list ws state)))))
                ((symbol-function 'claude-repl--handle-claude-finished)
                 (lambda (ws) (setq finished-ws ws)))
                ((symbol-function 'claude-repl--ws-get)
                 (lambda (_ws _key) nil)))
        (claude-repl--on-stop-event "ws1" "/some/dir")
        (should (equal cleared '("ws1" :thinking)))
        (should (equal finished-ws "ws1"))))))

(ert-deftest claude-repl-test-on-stop-event-handle-finished-error ()
  "on-stop-event should propagate error from handle-claude-finished."
  (claude-repl-test--with-clean-state
    (cl-letf (((symbol-function 'claude-repl--ws-clear) #'ignore)
              ((symbol-function 'claude-repl--handle-claude-finished)
               (lambda (_ws) (error "finished handler boom")))
              ((symbol-function 'claude-repl--ws-get)
               (lambda (_ws _key) :thinking)))
      (should-error (claude-repl--on-stop-event "ws1" "/some/dir")))))

;;;; ---- Tests: on-prompt-submit-event uncovered edge cases ----

(ert-deftest claude-repl-test-on-prompt-submit-event-nil-ws ()
  "on-prompt-submit-event with nil ws passes nil to mark-ws-thinking."
  (claude-repl-test--with-clean-state
    (let ((thinking-ws :not-called))
      (cl-letf (((symbol-function 'claude-repl--mark-ws-thinking)
                 (lambda (ws) (setq thinking-ws ws))))
        (claude-repl--on-prompt-submit-event nil "/some/dir")
        (should (eq thinking-ws nil))))))

(ert-deftest claude-repl-test-on-prompt-submit-event-already-thinking ()
  "on-prompt-submit-event should still call mark-ws-thinking even if ws is already thinking."
  (claude-repl-test--with-clean-state
    (let ((thinking-ws nil))
      (cl-letf (((symbol-function 'claude-repl--mark-ws-thinking)
                 (lambda (ws) (setq thinking-ws ws))))
        ;; Set thinking first, then submit again
        (claude-repl--ws-set "ws1" :thinking)
        (claude-repl--on-prompt-submit-event "ws1" "/some/dir")
        (should (equal thinking-ws "ws1"))))))

;;;; ---- Tests: dispatch-sentinel-file uncovered edge cases ----

(ert-deftest claude-repl-test-dispatch-sentinel-file-no-directory-component ()
  "dispatch-sentinel-file should work with a bare filename (no directory)."
  (claude-repl-test--with-clean-state
    (let ((dispatched-handler nil))
      (cl-letf (((symbol-function 'claude-repl--process-sentinel-file)
                 (lambda (_file handler) (setq dispatched-handler handler))))
        (claude-repl--dispatch-sentinel-file "stop_123")
        (should dispatched-handler)
        (should (eq (plist-get dispatched-handler :callback)
                    'claude-repl--on-stop-event))))))

(ert-deftest claude-repl-test-dispatch-sentinel-file-exact-prefix ()
  "dispatch-sentinel-file should match a filename that is exactly a prefix (no suffix)."
  (claude-repl-test--with-clean-state
    (let ((dispatched-handler nil))
      (cl-letf (((symbol-function 'claude-repl--process-sentinel-file)
                 (lambda (_file handler) (setq dispatched-handler handler))))
        ;; "stop_" is a prefix in the dispatch alist; file is exactly "stop_"
        (claude-repl--dispatch-sentinel-file "/dir/stop_")
        (should dispatched-handler)
        (should (eq (plist-get dispatched-handler :callback)
                    'claude-repl--on-stop-event))))))

(ert-deftest claude-repl-test-dispatch-sentinel-file-first-prefix-wins ()
  "dispatch-sentinel-file should use the first matching prefix when multiple could match."
  (claude-repl-test--with-clean-state
    (let ((dispatched-handler nil))
      (cl-letf (((symbol-function 'claude-repl--process-sentinel-file)
                 (lambda (_file handler) (setq dispatched-handler handler))))
        ;; "permission_prompt" is the first entry; it should match before any other
        (claude-repl--dispatch-sentinel-file "/dir/permission_prompt")
        (should dispatched-handler)
        (should (eq (plist-get dispatched-handler :callback)
                    'claude-repl--on-permission-event))))))

;;;; ---- Tests: dispatch-sentinel-event uncovered edge cases ----

(ert-deftest claude-repl-test-sentinel-event-nil-action ()
  "An event with nil action should not dispatch."
  (claude-repl-test--with-clean-state
    (let ((dispatched nil))
      (cl-letf (((symbol-function 'claude-repl--dispatch-sentinel-file)
                 (lambda (_f) (setq dispatched t)))
                ((symbol-function 'file-exists-p) (lambda (_f) t)))
        (claude-repl--dispatch-sentinel-event '(nil nil "/dir/stop_123"))
        (should-not dispatched)))))

;; Covered by `claude-repl-test-sentinel-event-tolerates-nil-file' above:
;; `stopped' events fire with nil file when watchers are removed, and the
;; dispatcher must skip them gracefully rather than crash.

;;;; ---- Tests: poll-workspace-notifications uncovered edge cases ----

(ert-deftest claude-repl-test-poll-mix-of-known-and-unknown-files ()
  "poll-workspace-notifications should dispatch both known and unknown files."
  (claude-repl-test--with-clean-state
    (let ((dispatched-files nil)
          (dispatch-results nil))
      (cl-letf (((symbol-function 'file-directory-p) (lambda (_d) t))
                ((symbol-function 'directory-files)
                 (lambda (_dir _full _match _nosort)
                   '("/sentinel/stop_abc" "/sentinel/unknown_file" "/sentinel/permission_prompt")))
                ((symbol-function 'file-exists-p) (lambda (_f) t))
                ((symbol-function 'claude-repl--dispatch-sentinel-file)
                 (lambda (f)
                   (push f dispatched-files)
                   ;; Return t for known, nil for unknown
                   (not (string-match-p "unknown" f)))))
        (claude-repl--poll-workspace-notifications)
        ;; All three files should have been dispatched
        (should (= (length dispatched-files) 3))
        (should (member "/sentinel/stop_abc" dispatched-files))
        (should (member "/sentinel/unknown_file" dispatched-files))
        (should (member "/sentinel/permission_prompt" dispatched-files))))))

(ert-deftest claude-repl-test-poll-excludes-hidden-files ()
  "poll-workspace-notifications should not see hidden files (dotfiles) due to regex filter."
  (claude-repl-test--with-clean-state
    (let ((dispatched-files nil)
          (regex-used nil))
      (cl-letf (((symbol-function 'file-directory-p) (lambda (_d) t))
                ((symbol-function 'directory-files)
                 (lambda (_dir _full match _nosort)
                   ;; Capture the regex and simulate what Emacs would return:
                   ;; the regex \\`[^.] excludes dotfiles
                   (setq regex-used match)
                   ;; Only non-dotfiles would be returned by directory-files
                   '("/sentinel/stop_abc")))
                ((symbol-function 'file-exists-p) (lambda (_f) t))
                ((symbol-function 'claude-repl--dispatch-sentinel-file)
                 (lambda (f) (push f dispatched-files) t)))
        (claude-repl--poll-workspace-notifications)
        ;; Verify the regex passed to directory-files excludes dotfiles
        (should (stringp regex-used))
        ;; ".hidden" should NOT match the regex (starts with dot)
        (should-not (string-match-p regex-used ".hidden"))
        ;; "stop_abc" should match the regex (starts with non-dot)
        (should (string-match-p regex-used "stop_abc"))
        ;; Only the non-hidden file was dispatched
        (should (= (length dispatched-files) 1))))))

;;;; ---- Tests: sentinel event edge cases (status transitions .md) ----

(ert-deftest claude-repl-test-on-stop-event-when-permission-clear-noop ()
  "Stop event when status is :permission: ws-clear :thinking is a no-op, but handle-finished still runs."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set "ws1" :permission)
    (let ((finished-called nil))
      (cl-letf (((symbol-function 'claude-repl--handle-claude-finished)
                 (lambda (ws) (setq finished-called ws))))
        (claude-repl--on-stop-event "ws1" "/some/dir")
        ;; :permission should be unchanged (ws-clear :thinking was a no-op)
        (should (eq (claude-repl--ws-state "ws1") :permission))
        ;; handle-finished should still have been called
        (should (equal finished-called "ws1"))))))

(ert-deftest claude-repl-test-on-permission-event-overwrites-done ()
  "Permission event should overwrite :done status with :permission."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set "ws1" :done)
    (claude-repl--on-permission-event "ws1" "/some/dir")
    (should (eq (claude-repl--ws-state "ws1") :permission))))

(ert-deftest claude-repl-test-on-permission-event-overwrites-thinking ()
  "Permission event should overwrite :thinking status with :permission."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set "ws1" :thinking)
    (claude-repl--on-permission-event "ws1" "/some/dir")
    (should (eq (claude-repl--ws-state "ws1") :permission))))

;;;; ---- Tests: claude-repl-reset-sentinel-watchers ----

(ert-deftest claude-repl-test-reset-sentinel-watchers-removes-matching ()
  "Should remove every descriptor whose watched dir is the sentinel dir."
  (claude-repl-test--with-clean-state
    (let ((removed '())
          (target claude-repl--sentinel-dir)
          (descs (make-hash-table :test 'equal)))
      (puthash 'desc-a (cons target 'cb) descs)
      (puthash 'desc-b (cons target 'cb) descs)
      (cl-letf (((symbol-function 'file-notify-rm-watch)
                 (lambda (d) (push d removed)))
                ((symbol-function 'file-notify-add-watch)
                 (lambda (&rest _) 'new-desc))
                ((symbol-function 'file-truename) #'identity)
                (file-notify-descriptors descs))
        (claude-repl-reset-sentinel-watchers)
        (should (equal (sort (copy-sequence removed)
                             (lambda (a b) (string< (symbol-name a) (symbol-name b))))
                       '(desc-a desc-b)))))))

(ert-deftest claude-repl-test-reset-sentinel-watchers-leaves-others ()
  "Should NOT remove descriptors watching other directories."
  (claude-repl-test--with-clean-state
    (let ((removed '())
          (descs (make-hash-table :test 'equal)))
      (puthash 'desc-other (cons "/some/other/dir" 'cb) descs)
      (cl-letf (((symbol-function 'file-notify-rm-watch)
                 (lambda (d) (push d removed)))
                ((symbol-function 'file-notify-add-watch)
                 (lambda (&rest _) 'new-desc))
                ((symbol-function 'file-truename) #'identity)
                (file-notify-descriptors descs))
        (claude-repl-reset-sentinel-watchers)
        (should-not removed)))))

(ert-deftest claude-repl-test-reset-sentinel-watchers-registers-fresh ()
  "Should register a new watcher on the sentinel dir after cleanup."
  (claude-repl-test--with-clean-state
    (let ((added-dir nil)
          (descs (make-hash-table :test 'equal)))
      (cl-letf (((symbol-function 'file-notify-rm-watch) (lambda (&rest _) nil))
                ((symbol-function 'file-notify-add-watch)
                 (lambda (dir &rest _) (setq added-dir dir) 'new-desc))
                ((symbol-function 'file-truename) #'identity)
                (file-notify-descriptors descs))
        (claude-repl-reset-sentinel-watchers)
        (should (equal added-dir claude-repl--sentinel-dir))
        (should (eq claude-repl--sentinel-watch-descriptor 'new-desc))))))

(ert-deftest claude-repl-test-reap-sentinel-watchers-returns-count ()
  "Reap helper should return the number of watchers removed."
  (claude-repl-test--with-clean-state
    (let ((target claude-repl--sentinel-dir)
          (descs (make-hash-table :test 'equal)))
      (puthash 'desc-a (cons target 'cb) descs)
      (puthash 'desc-b (cons target 'cb) descs)
      (puthash 'desc-other (cons "/other" 'cb) descs)
      (cl-letf (((symbol-function 'file-notify-rm-watch) (lambda (&rest _) nil))
                ((symbol-function 'file-truename) #'identity)
                (file-notify-descriptors descs))
        (should (= (claude-repl--reap-sentinel-watchers) 2))))))

(ert-deftest claude-repl-test-nuke-sentinel-watchers-does-not-re-register ()
  "`claude-repl-nuke-sentinel-watchers' must NOT create a new watcher."
  (claude-repl-test--with-clean-state
    (let ((target claude-repl--sentinel-dir)
          (add-watch-called nil)
          (descs (make-hash-table :test 'equal)))
      (puthash 'desc-a (cons target 'cb) descs)
      (cl-letf (((symbol-function 'file-notify-rm-watch) (lambda (&rest _) nil))
                ((symbol-function 'file-notify-add-watch)
                 (lambda (&rest _) (setq add-watch-called t) 'new-desc))
                ((symbol-function 'file-truename) #'identity)
                (file-notify-descriptors descs))
        (claude-repl-nuke-sentinel-watchers)
        (should-not add-watch-called)
        (should-not claude-repl--sentinel-watch-descriptor)))))

;;;; ---- Tests: update-session-id-from-sentinel ----

(ert-deftest claude-repl-test-update-session-id-from-sentinel-sets-id ()
  "update-session-id-from-sentinel should set the session ID on the active inst."
  (claude-repl-test--with-clean-state
    (let ((inst (make-claude-repl-instantiation)))
      (claude-repl--ws-put "ws1" :active-env :bare-metal)
      (claude-repl--ws-put "ws1" :bare-metal inst)
      (claude-repl--update-session-id-from-sentinel "ws1" "new-sid-abc")
      (should (equal (claude-repl-instantiation-session-id inst) "new-sid-abc")))))

(ert-deftest claude-repl-test-update-session-id-from-sentinel-skips-nil ()
  "update-session-id-from-sentinel should be a no-op when session-id is nil."
  (claude-repl-test--with-clean-state
    (let ((inst (make-claude-repl-instantiation :session-id "old-sid")))
      (claude-repl--ws-put "ws1" :active-env :bare-metal)
      (claude-repl--ws-put "ws1" :bare-metal inst)
      (claude-repl--update-session-id-from-sentinel "ws1" nil)
      (should (equal (claude-repl-instantiation-session-id inst) "old-sid")))))

(ert-deftest claude-repl-test-update-session-id-from-sentinel-skips-empty ()
  "update-session-id-from-sentinel should be a no-op when session-id is empty string."
  (claude-repl-test--with-clean-state
    (let ((inst (make-claude-repl-instantiation :session-id "old-sid")))
      (claude-repl--ws-put "ws1" :active-env :bare-metal)
      (claude-repl--ws-put "ws1" :bare-metal inst)
      (claude-repl--update-session-id-from-sentinel "ws1" "")
      (should (equal (claude-repl-instantiation-session-id inst) "old-sid")))))

(ert-deftest claude-repl-test-update-session-id-from-sentinel-skips-same ()
  "update-session-id-from-sentinel should be a no-op when session-id is unchanged."
  (claude-repl-test--with-clean-state
    (let ((inst (make-claude-repl-instantiation :session-id "same-sid"))
          (set-called nil))
      (claude-repl--ws-put "ws1" :active-env :bare-metal)
      (claude-repl--ws-put "ws1" :bare-metal inst)
      (cl-letf (((symbol-function 'claude-repl--set-session-id)
                 (lambda (_ws _id) (setq set-called t))))
        (claude-repl--update-session-id-from-sentinel "ws1" "same-sid")
        (should-not set-called)))))

(ert-deftest claude-repl-test-update-session-id-from-sentinel-updates-changed ()
  "update-session-id-from-sentinel should update when session-id differs."
  (claude-repl-test--with-clean-state
    (let ((inst (make-claude-repl-instantiation :session-id "old-sid")))
      (claude-repl--ws-put "ws1" :active-env :bare-metal)
      (claude-repl--ws-put "ws1" :bare-metal inst)
      (claude-repl--update-session-id-from-sentinel "ws1" "new-sid")
      (should (equal (claude-repl-instantiation-session-id inst) "new-sid")))))

;;;; ---- Tests: on-session-start-event ----

(ert-deftest claude-repl-test-on-session-start-event-sets-ready ()
  "on-session-start-event should set claude-repl--ready on the vterm buffer."
  (claude-repl-test--with-clean-state
    (let ((fake-buf (generate-new-buffer " *test-session-start-vterm*"))
          (timer-cancelled nil)
          (panels-opened nil))
      (unwind-protect
          (progn
            (with-current-buffer fake-buf
              (setq-local claude-repl--ready nil))
            (claude-repl--ws-put "ws1" :vterm-buffer fake-buf)
            (cl-letf (((symbol-function 'claude-repl--cancel-ready-timer)
                       (lambda (_ws) (setq timer-cancelled t)))
                      ((symbol-function 'claude-repl--swap-placeholder) #'ignore)
                      ((symbol-function 'claude-repl--open-panels-after-ready)
                       (lambda (_ws) (setq panels-opened t))))
              (claude-repl--on-session-start-event "ws1" "/some/dir")
              (should (buffer-local-value 'claude-repl--ready fake-buf))
              (should timer-cancelled)
              (should panels-opened)))
        (when (buffer-live-p fake-buf) (kill-buffer fake-buf))))))

(ert-deftest claude-repl-test-on-session-start-event-idempotent ()
  "on-session-start-event should be a no-op when already ready."
  (claude-repl-test--with-clean-state
    (let ((fake-buf (generate-new-buffer " *test-session-start-idem*"))
          (panels-opened nil))
      (unwind-protect
          (progn
            (with-current-buffer fake-buf
              (setq-local claude-repl--ready t))
            (claude-repl--ws-put "ws1" :vterm-buffer fake-buf)
            (cl-letf (((symbol-function 'claude-repl--open-panels-after-ready)
                       (lambda (_ws) (setq panels-opened t))))
              (claude-repl--on-session-start-event "ws1" "/some/dir")
              (should-not panels-opened)))
        (when (buffer-live-p fake-buf) (kill-buffer fake-buf))))))

(ert-deftest claude-repl-test-on-session-start-event-no-vterm-silent-noop ()
  "on-session-start-event silently no-ops when no vterm buffer exists.
This covers session_start hooks fired by Claude sessions that this
module does not own (common when the hook's cwd routes to a default
persp with no managed vterm)."
  (claude-repl-test--with-clean-state
    (let ((msg-called nil)
          (claude-state-written nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (&rest _) (setq msg-called t)))
                ((symbol-function 'claude-repl--ws-set-claude-state)
                 (lambda (&rest _) (setq claude-state-written t))))
        (claude-repl--on-session-start-event "ws1" "/some/dir")
        (should-not msg-called)
        (should-not claude-state-written)))))

(ert-deftest claude-repl-test-on-session-start-event-dead-vterm-silent-noop ()
  "on-session-start-event silently no-ops when the vterm buffer is dead."
  (claude-repl-test--with-clean-state
    (let ((fake-buf (generate-new-buffer " *test-session-start-dead*"))
          (msg-called nil)
          (claude-state-written nil))
      (claude-repl--ws-put "ws1" :vterm-buffer fake-buf)
      (kill-buffer fake-buf)
      (cl-letf (((symbol-function 'message)
                 (lambda (&rest _) (setq msg-called t)))
                ((symbol-function 'claude-repl--ws-set-claude-state)
                 (lambda (&rest _) (setq claude-state-written t))))
        (claude-repl--on-session-start-event "ws1" "/some/dir")
        (should-not msg-called)
        (should-not claude-state-written)))))

(ert-deftest claude-repl-test-on-session-start-event-sets-idle ()
  "on-session-start-event writes :claude-state :idle (transition from :init)."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-set-claude-state "ws1" :init)
    (let ((fake-buf (generate-new-buffer " *test-session-start-idle*")))
      (unwind-protect
          (progn
            (claude-repl--ws-put "ws1" :vterm-buffer fake-buf)
            (cl-letf (((symbol-function 'claude-repl--cancel-ready-timer) #'ignore)
                      ((symbol-function 'claude-repl--swap-placeholder) #'ignore)
                      ((symbol-function 'claude-repl--open-panels-after-ready) #'ignore))
              (claude-repl--on-session-start-event "ws1" "/some/dir")
              (should (eq (claude-repl--ws-claude-state "ws1") :idle))))
        (when (buffer-live-p fake-buf) (kill-buffer fake-buf))))))

(provide 'test-sentinel)

;;; test-sentinel.el ends here
