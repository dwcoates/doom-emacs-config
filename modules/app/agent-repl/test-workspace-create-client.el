;;; test-workspace-create-client.el --- Tests for daemon workspace thin client -*- lexical-binding: t; -*-

;;; Code:

(load (expand-file-name "test-helpers.el"
                        (file-name-directory
                         (or load-file-name buffer-file-name)))
      nil t)

(ert-deftest agent-repl-test-workspace-create-keybindings-are-tab-n-and-N ()
  "The only declared create chords dispatch the required commands."
  (should
   (equal agent-repl--workspace-create-keybindings
          '(("n" . agent-repl-create-worktree-workspace)
            ("N" . agent-repl-create-worktree-workspace-from-origin-master)))))

(ert-deftest agent-repl-test-workspace-create-send-promptless-still-daemon-owned ()
  "A blank initial prompt sends createWorkspace; it never selects no-agent."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "source" :project-dir "/tmp/source/")
    (let ((sent nil)
          (tracked nil)
          (reads '("requested-name" "")))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) (pop reads)))
                ((symbol-function 'agent-repl--ws-current-name)
                 (lambda () "source"))
                ((symbol-function 'agent-repl--uds-send-command)
                 (lambda (field payload &optional workspace _process)
                   (setq sent (list field payload workspace))
                   "req-1"))
                ((symbol-function 'agent-repl--uds-track-command)
                 (lambda (&rest args) (setq tracked args)))
                ((symbol-function 'agent-repl--do-create-worktree-workspace)
                 (lambda (&rest _) (error "local worktree creation forbidden")))
                ((symbol-function 'agent-repl--setup-worktree-session)
                 (lambda (&rest _) (error "local session creation forbidden")))
                ((symbol-function 'agent-repl--spawn-workspace-generation)
                 (lambda (&rest _) (error "headless name generation forbidden"))))
        (should (equal (agent-repl-create-worktree-workspace 'head) "req-1"))
        (should (equal (car sent) "createWorkspace"))
        (should (equal (plist-get (cadr sent) :requestedName)
                       "requested-name"))
        (should-not (plist-member (cadr sent) :initialPrompt))
        (should (equal (caddr sent) "source"))
        (should (equal tracked
                       '("req-1" "createWorkspace" "source")))))))

(ert-deftest agent-repl-test-workspace-create-master-wrapper-dispatches-master ()
  "`SPC TAB N' delegates to the same thin client with BASE `master'."
  (let ((seen nil))
    (cl-letf (((symbol-function 'agent-repl-create-worktree-workspace)
               (lambda (base &optional source)
                 (setq seen (list base source)))))
      (agent-repl-create-worktree-workspace-from-origin-master "source")
      (should (equal seen '(master "source"))))))

(ert-deftest agent-repl-test-workspace-create-send-includes-nonblank-prompt ()
  "Creation carries prompt, account, permission posture, and consent."
  (let ((sent nil))
    (let ((agent-repl-frontend-permission-mode "auto")
          (agent-repl-frontend-allow-ungated t))
      (cl-letf (((symbol-function 'agent-repl--compute-config-dir)
                 (lambda (dir)
                   (should (equal dir "/tmp/source"))
                   "/tmp/account"))
                ((symbol-function 'agent-repl--uds-send-command)
               (lambda (field payload &optional workspace _process)
                 (setq sent (list field payload workspace))
                 "req-2"))
                ((symbol-function 'agent-repl--uds-track-command)
                 (lambda (&rest _) nil)))
        (agent-repl--workspace-create-send
         "new" "/tmp/source" "master" "source" "/tmp/source"
         "  investigate this  " "p1" "opus")
        (should (equal (plist-get (cadr sent) :initialPrompt)
                       "investigate this"))
        (should (equal (plist-get (cadr sent) :baseCommit) "master"))
        (should (equal (plist-get (cadr sent) :priority) "p1"))
        (should (equal (plist-get (cadr sent) :model) "opus"))
        (should (equal (plist-get (cadr sent) :configDir) "/tmp/account"))
        (should (equal (plist-get (cadr sent) :permissionMode) "auto"))
        (should (eq (plist-get (cadr sent) :allowUngated) t))))))

(ert-deftest agent-repl-test-workspace-create-send-explicit-default-posture ()
  "Nil account/mode become explicit empty wire values without consent."
  (let ((sent nil)
        (agent-repl-frontend-permission-mode nil)
        (agent-repl-frontend-allow-ungated nil))
    (cl-letf (((symbol-function 'agent-repl--compute-config-dir)
               (lambda (_dir) nil))
              ((symbol-function 'agent-repl--uds-send-command)
               (lambda (_field payload &rest _)
                 (setq sent payload)
                 "req-default"))
              ((symbol-function 'agent-repl--uds-track-command)
               (lambda (&rest _) nil)))
      (agent-repl--workspace-create-send
       "new" "/tmp/source" "master" "source" "/tmp/source" nil)
      (should (equal (plist-get sent :configDir) ""))
      (should (equal (plist-get sent :permissionMode) ""))
      (should-not (plist-member sent :allowUngated)))))

(ert-deftest agent-repl-test-workspace-available-validates-before-mutation ()
  "A missing authoritative field causes no materialization and no ACK."
  (let ((materialized nil)
        (sent nil))
    (cl-letf (((symbol-function 'agent-repl--ws-materialize-daemon-workspace)
               (lambda (&rest _) (setq materialized t)))
              ((symbol-function 'agent-repl--uds-send-command)
               (lambda (&rest _) (setq sent t))))
      (should-error
       (agent-repl--workspace-create-handle-available
        '(:jobId "job-1" :finalName "new" :worktreePath "/tmp/new")))
      (should-not materialized)
      (should-not sent))))

(ert-deftest agent-repl-test-workspace-available-replay-is-idempotent-and-acks ()
  "An exact reconnect replay creates one perspective and sends another ACK."
  (agent-repl-test--with-clean-state
    (let ((creates 0)
          (sets 0)
          (acks nil)
          (tracks nil)
          (available
           '(:jobId "job-1" :finalName "new"
             :worktreePath "/tmp/new" :sessionId "session-1"
             :branch "DWC/new" :gitRoot "/tmp/source"
             :baseCommit "HEAD" :sourceWorkspace "source"
             :sourceDir "/tmp/source" :initialPromptQueued t
             :configDir "/tmp/account" :permissionMode "auto"
             :allowUngated t)))
      (cl-letf (((symbol-function 'file-directory-p) (lambda (_path) t))
                ((symbol-function 'agent-repl--ws-dir-owner)
                 (lambda (&rest _) nil))
                ((symbol-function 'agent-repl--ws-resolve-persp)
                 (lambda (_ws) nil))
                ((symbol-function 'persp-add-new)
                 (lambda (_ws) (cl-incf creates) 'persp))
                ((symbol-function 'set-persp-parameter)
                 (lambda (&rest _) (cl-incf sets)))
                ((symbol-function 'persp-kill)
                 (lambda (&rest _) (error "rollback must not run")))
                ((symbol-function 'agent-repl--uds-send-command)
                 (lambda (field payload &optional workspace _process)
                   (push (list field payload workspace) acks)
                   (format "ack-%d" (length acks))))
                ((symbol-function 'agent-repl--uds-track-command)
                 (lambda (&rest args) (push args tracks))))
        (should (eq (agent-repl--workspace-create-handle-available available)
                    'created))
        (should (eq (agent-repl--workspace-create-handle-available available)
                    'existing))
        (should (= creates 1))
        (should (= sets 1))
        (should (= (length acks) 2))
        (dolist (ack acks)
          (should (equal (car ack) "workspaceMaterialized"))
          (should (equal (plist-get (cadr ack) :jobId) "job-1"))
          (should (equal (caddr ack) "new")))
        (should (= (length tracks) 2))
        (should (equal (agent-repl--ws-get "new" :frontend-session-id)
                       "session-1"))
        (should (equal (agent-repl--ws-get "new" :config-dir-override)
                       "/tmp/account"))
        (should (equal (agent-repl--ws-get "new" :permission-mode) "auto"))
        (should (eq (agent-repl--ws-get "new" :allow-ungated) t))
        (should (eq (agent-repl--ws-get "new" :initial-prompt-queued) t))))))

(ert-deftest agent-repl-test-workspace-available-never-creates-git-session-or-shim ()
  "Materialization touches only perspective/bookkeeping and its ACK."
  (agent-repl-test--with-clean-state
    (let ((available
           '(:jobId "job-2" :finalName "plain"
             :worktreePath "/tmp/plain" :sessionId "session-2"
             :initialPromptQueued nil)))
      (cl-letf (((symbol-function 'file-directory-p) (lambda (_path) t))
                ((symbol-function 'agent-repl--ws-dir-owner)
                 (lambda (&rest _) nil))
                ((symbol-function 'agent-repl--ws-resolve-persp)
                 (lambda (_ws) nil))
                ((symbol-function 'persp-add-new) (lambda (_ws) 'persp))
                ((symbol-function 'set-persp-parameter) (lambda (&rest _) nil))
                ((symbol-function 'persp-kill) (lambda (&rest _) nil))
                ((symbol-function 'agent-repl--uds-send-command)
                 (lambda (&rest _) "ack"))
                ((symbol-function 'agent-repl--uds-track-command)
                 (lambda (&rest _) nil))
                ((symbol-function 'agent-repl--async-worktree-add)
                 (lambda (&rest _) (error "git worktree forbidden")))
                ((symbol-function 'agent-repl--frontend-create-session)
                 (lambda (&rest _) (error "session creation forbidden")))
                ((symbol-function 'agent-repl--setup-worktree-session)
                 (lambda (&rest _) (error "session setup forbidden")))
                ((symbol-function 'agent-repl--spawn-agent-shim)
                 (lambda (&rest _) (error "shim startup forbidden")))
                ((symbol-function 'agent-repl--send)
                 (lambda (&rest _) (error "local prompt delivery forbidden"))))
        (should
         (eq (agent-repl--workspace-create-handle-available available)
             'created))
        (should (agent-repl--ws-live-p "plain"))
        (should-not (agent-repl--ws-get "plain" :initial-prompt-queued))))))

(ert-deftest agent-repl-test-workspace-available-rolls-back-on-perspective-setup-error ()
  "Failure after perspective creation leaves no hash entry or perspective."
  (agent-repl-test--with-clean-state
    (let ((killed nil)
          (acked nil)
          (available
           '(:jobId "job-3" :finalName "broken"
             :worktreePath "/tmp/broken" :sessionId "session-3")))
      (cl-letf (((symbol-function 'file-directory-p) (lambda (_path) t))
                ((symbol-function 'agent-repl--ws-dir-owner)
                 (lambda (&rest _) nil))
                ((symbol-function 'agent-repl--ws-resolve-persp)
                 (lambda (_ws) nil))
                ((symbol-function 'persp-add-new) (lambda (_ws) 'persp))
                ((symbol-function 'set-persp-parameter)
                 (lambda (&rest _) (error "persp property failed")))
                ((symbol-function 'persp-kill)
                 (lambda (ws) (setq killed ws)))
                ((symbol-function 'agent-repl--uds-send-command)
                 (lambda (&rest _) (setq acked t))))
        (should-error
         (agent-repl--workspace-create-handle-available available))
        (should (equal killed "broken"))
        (should-not (agent-repl--ws-known-p "broken"))
        (should-not acked)))))

(ert-deftest agent-repl-test-workspace-command-files-are-not-emacs-owned ()
  "Emacs exposes no watcher, drain, or file claimant for daemon-owned JSON."
  (dolist (fn '(agent-repl--register-workspace-commands-watch
                agent-repl--drain-workspace-commands-files
                agent-repl--process-workspace-commands-file))
    (should-not (fboundp fn))))

(ert-deftest agent-repl-test-daemon-materialization-envelope-clears-on-tombstone ()
  "The replay envelope does not retain a dead workspace's session id."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "dead" :project-dir "/tmp/dead/")
    (agent-repl--ws-put
     "dead" :daemon-workspace-metadata
     '(:frontend-session-id "session-dead"))
    (agent-repl--ws-del "dead")
    (should-not
     (agent-repl--ws-get "dead" :daemon-workspace-metadata))))

(ert-deftest agent-repl-test-host-action-dispatches-ui-handler-and-completes ()
  "HostAction preserves UI-only behavior through its completion command."
  (let ((handled nil)
        (completion nil))
    (cl-letf (((symbol-function 'agent-repl--handle-switch-command)
               (lambda (cmd) (setq handled cmd)))
              ((symbol-function 'agent-repl--uds-send-command)
               (lambda (field payload &optional workspace _process)
                 (setq completion (list field payload workspace))
                 "host-ack"))
              ((symbol-function 'agent-repl--uds-track-command)
               (lambda (&rest _) nil)))
      (should
       (agent-repl--workspace-create-handle-host-action
        '(:actionId "action-1" :switchWorkspace (:dir "/tmp/repo"))))
      (should (equal handled '((dir . "/tmp/repo"))))
      (should (equal (car completion) "hostActionCompleted"))
      (should (equal (plist-get (cadr completion) :actionId) "action-1"))
      (should (eq (plist-get (cadr completion) :ok) t)))))

(ert-deftest agent-repl-test-host-action-workspace-create-failure-is-announced ()
  "A failed creation job is logged and echoed, then completed ok."
  (let ((logged nil)
        (echoed nil)
        (completion nil))
    (cl-letf (((symbol-function 'agent-repl--log)
               (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logged)))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (setq echoed (apply #'format fmt args))))
              ((symbol-function 'agent-repl--uds-send-command)
               (lambda (field payload &rest _)
                 (setq completion (list field payload))
                 "failure-ack"))
              ((symbol-function 'agent-repl--uds-track-command)
               (lambda (&rest _) nil)))
      (should
       (agent-repl--workspace-create-handle-host-action
        '(:actionId "job-1:failed"
          :workspaceCreateFailed
          (:jobId "job-1" :requestedName "DWC/feature"
           :error "plan worktree: exit=128"))))
      (should (string-match-p "DWC/feature" echoed))
      (should (string-match-p "plan worktree: exit=128" echoed))
      (should (cl-some (lambda (line)
                         (string-match-p "JOB FAILED job-id=job-1" line))
                       logged))
      (should (equal (car completion) "hostActionCompleted"))
      (should (eq (plist-get (cadr completion) :ok) t)))))

(ert-deftest agent-repl-test-host-action-legacy-command-translates-struct ()
  "legacyCommand converts its recursive Struct and ACKs handler completion."
  (let ((handled nil)
        (completion nil)
        (tracked nil))
    (cl-letf (((symbol-function 'agent-repl--handle-send-command)
               (lambda (cmd) (setq handled cmd)))
              ((symbol-function 'agent-repl--uds-send-command)
               (lambda (field payload &optional workspace _process)
                 (setq completion (list field payload workspace))
                 "host-legacy-ack"))
              ((symbol-function 'agent-repl--uds-track-command)
               (lambda (&rest args) (setq tracked args))))
      (should
       (agent-repl--workspace-create-handle-host-action
        '(:actionId "legacy-1"
          :legacyCommand
          (:type "send"
           :payload (:workspace "ws1"
                     :data (:pgn "1. e4" :labels ("a" "b")))))))
      (should
       (equal handled
              '((workspace . "ws1")
                (data . ((pgn . "1. e4") (labels . ("a" "b")))))))
      (should (equal (car completion) "hostActionCompleted"))
      (should (equal (plist-get (cadr completion) :actionId) "legacy-1"))
      (should (eq (plist-get (cadr completion) :ok) t))
      (should (equal tracked
                     '("host-legacy-ack" "hostActionCompleted" "ws1"))))))

(ert-deftest agent-repl-test-host-action-legacy-types-are-exact ()
  "Only the eight daemon legacy-command types resolve to host handlers."
  (should
   (equal
    (mapcar #'car agent-repl--legacy-host-action-handlers)
    '("prompt" "finish" "close" "open"
      "clipboard" "send" "merge" "eval")))
  (should-not (assoc "create" agent-repl--legacy-host-action-handlers)))

(ert-deftest agent-repl-test-host-action-unknown-legacy-type-nacks ()
  "Unknown legacy types fail loudly and send an unsuccessful completion."
  (let ((completion nil))
    (cl-letf (((symbol-function 'agent-repl--uds-send-command)
               (lambda (field payload &rest _)
                 (setq completion (list field payload))
                 "host-failure-ack"))
              ((symbol-function 'agent-repl--uds-track-command)
               (lambda (&rest _) nil)))
      (should-error
       (agent-repl--workspace-create-handle-host-action
        '(:actionId "legacy-bad"
          :legacyCommand (:type "create" :payload (:name "forbidden")))))
      (should (equal (car completion) "hostActionCompleted"))
      (should (equal (plist-get (cadr completion) :actionId) "legacy-bad"))
      (should (eq (plist-get (cadr completion) :ok) json-false))
      (should (string-match-p
               "unsupported HostAction legacyCommand type create"
               (plist-get (cadr completion) :error))))))

(ert-deftest agent-repl-test-host-action-handler-error-nacks-and-resignals ()
  "A legacy handler error is ACKed false before the error escapes."
  (let ((completion nil))
    (cl-letf (((symbol-function 'agent-repl--handle-prompt-command)
               (lambda (_cmd) (error "prompt handler exploded")))
              ((symbol-function 'agent-repl--uds-send-command)
               (lambda (_field payload &rest _)
                 (setq completion payload)
                 "host-failure-ack"))
              ((symbol-function 'agent-repl--uds-track-command)
               (lambda (&rest _) nil)))
      (should-error
       (agent-repl--workspace-create-handle-host-action
        '(:actionId "legacy-handler-failure"
          :legacyCommand
          (:type "prompt" :payload (:workspace "ws1" :prompt "hi")))))
      (should (eq (plist-get completion :ok) json-false))
      (should (equal (plist-get completion :error)
                     "prompt handler exploded")))))

(ert-deftest agent-repl-test-host-action-overlap-runs-handler-once-and-resends ()
  "In-flight and completed duplicates never repeat a non-idempotent handler."
  (let ((agent-repl--host-action-outcomes (make-hash-table :test 'equal))
        (agent-repl--host-action-success-order nil)
        (action
         '(:actionId "overlap-1"
           :legacyCommand
           (:type "clipboard" :payload (:text "one copy"))))
        (handler-calls 0)
        completions)
    (cl-letf (((symbol-function 'agent-repl--handle-clipboard-command)
               (lambda (_cmd)
                 (cl-incf handler-calls)
                 ;; Simulate a live delivery re-entering while the snapshot
                 ;; copy is still executing.
                 (agent-repl--workspace-create-handle-host-action action)))
              ((symbol-function 'agent-repl--uds-send-command)
               (lambda (_field payload &rest _)
                 (push payload completions)
                 (format "completion-%d" (length completions))))
              ((symbol-function 'agent-repl--uds-track-command)
               (lambda (&rest _) nil)))
      (should
       (agent-repl--workspace-create-handle-host-action action))
      ;; One original completion plus one replay for the suppressed overlap.
      (should (= (length completions) 2))
      (should (= handler-calls 1))
      ;; A later completed duplicate resends the same successful outcome.
      (should
       (eq (agent-repl--workspace-create-handle-host-action action)
           :duplicate))
      (should (= (length completions) 3))
      (should (= handler-calls 1))
      (dolist (payload completions)
        (should (equal (plist-get payload :actionId) "overlap-1"))
        (should (eq (plist-get payload :ok) t))))))

(ert-deftest agent-repl-test-host-action-failure-completion-makes-redelivery-retryable ()
  "After failure completion, redelivery executes the handler again."
  (let ((agent-repl--host-action-outcomes (make-hash-table :test 'equal))
        (agent-repl--host-action-success-order nil)
        (action
         '(:actionId "retry-1"
           :legacyCommand
           (:type "prompt" :payload (:workspace "ws1" :prompt "retry"))))
        (handler-calls 0)
        completions)
    (cl-letf (((symbol-function 'agent-repl--handle-prompt-command)
               (lambda (_cmd)
                 (cl-incf handler-calls)
                 (when (= handler-calls 1)
                   (error "first attempt failed"))))
              ((symbol-function 'agent-repl--uds-send-command)
               (lambda (_field payload &rest _)
                 (push payload completions)
                 (format "completion-%d" (length completions))))
              ((symbol-function 'agent-repl--uds-track-command)
               (lambda (&rest _) nil)))
      (should-error
       (agent-repl--workspace-create-handle-host-action action))
      (should-not
       (gethash "retry-1" agent-repl--host-action-outcomes))
      (should
       (agent-repl--workspace-create-handle-host-action action))
      (should (= handler-calls 2))
      (should (= (length completions) 2))
      (should (eq (plist-get (cadr completions) :ok) json-false))
      (should (equal (plist-get (cadr completions) :error)
                     "first attempt failed"))
      (should (eq (plist-get (car completions) :ok) t)))))

(ert-deftest agent-repl-test-snapshot-materializes-before-render-state ()
  "Reconnect snapshots never render a created workspace before Available."
  (let ((events nil))
    (cl-letf (((symbol-function 'agent-repl--workspace-create-handle-available)
               (lambda (_item) (push 'available events)))
              ((symbol-function 'agent-repl--frontend-apply-workspace-state)
               (lambda (_item) (push 'workspace-state events)))
              ((symbol-function 'agent-repl--frontend-apply-session-view)
               (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--frontend-apply-session-init)
               (lambda (&rest _) nil)))
      (agent-repl--frontend-apply-snapshot
       '(:workspaceAvailable ((:jobId "job"))
         :workspaces ((:workspace "/tmp/new"))))
      (should (equal (nreverse events) '(available workspace-state))))))

(provide 'test-workspace-create-client)

;;; test-workspace-create-client.el ends here
