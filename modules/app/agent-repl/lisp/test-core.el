;;; test-core.el --- ERT tests for agent-repl core.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;   emacs -batch -Q -l ert -l test-core.el -f ert-run-tests-batch-and-exit
;;
;; Or interactively:
;;   M-x load-file RET test-core.el RET
;;   M-x ert RET t RET

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Tests: Workspace ID / root resolution ----

;;;; ---- Tests: bounded warning deduplication ----

(ert-deftest agent-repl-test-warn-once-emits-first-observation-only ()
  "A repeated causal fingerprint records one identity-complete warning."
  (let ((agent-repl--warn-once-fingerprints (make-hash-table :test 'equal))
        (agent-repl--warn-once-order nil)
        (warnings nil))
    (cl-letf (((symbol-function 'agent-repl--emit-log-record)
               (lambda (ws _level _verbosity fmt args &rest _)
                 (push (list ws (apply #'format fmt args)) warnings))))
      (should (agent-repl--warn-once "ws" "api-message=m payload=abc"
                                     "response warning api_message_id=%s" "m"))
      (should-not (agent-repl--warn-once "ws" "api-message=m payload=abc"
                                         "response warning api_message_id=%s" "m"))
      (should (equal warnings '(("ws" "WARNING: response warning api_message_id=m")))))))

(ert-deftest agent-repl-test-warn-once-fifo-bound-makes-evicted-key-observable ()
  "A full warning cache evicts FIFO state without retaining unbounded entries."
  (let ((agent-repl--warn-once-capacity 2)
        (agent-repl--warn-once-fingerprints (make-hash-table :test 'equal))
        (agent-repl--warn-once-order nil)
        (warnings nil))
    (cl-letf (((symbol-function 'agent-repl--emit-log-record)
               (lambda (_ws _level _verbosity fmt args &rest _)
                 (push (apply #'format fmt args) warnings))))
      (dolist (fingerprint '("first" "second" "third" "first"))
        (should (agent-repl--warn-once "ws" fingerprint "warning=%s" fingerprint)))
      (should (= (hash-table-count agent-repl--warn-once-fingerprints) 2))
      (should (equal (nreverse warnings)
                     '("WARNING: warning=first" "WARNING: warning=second"
                       "WARNING: warning=third" "WARNING: warning=first"))))))

(ert-deftest agent-repl-test-warn-once-rejects-empty-causal-fingerprint ()
  "A missing causal identity signals before warning-state mutation."
  (let ((agent-repl--warn-once-fingerprints (make-hash-table :test 'equal))
        (agent-repl--warn-once-order nil))
    (cl-letf (((symbol-function 'agent-repl--log) (lambda (&rest _) nil)))
      (should-error (agent-repl--warn-once "ws" "" "impossible")))
    (should (= (hash-table-count agent-repl--warn-once-fingerprints) 0))))

(ert-deftest agent-repl-test-workspace-dir-hash-from-project-root ()
  "The workspace dir hash is the first 8 chars of MD5 of the canonical ws-dir."
  (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
            ((symbol-function 'agent-repl--ws-dir) (lambda (_ws) "/test/project")))
    (should (equal (agent-repl--workspace-dir-hash)
                   (substring (md5 (agent-repl--path-canonical "/test/project")) 0 8)))))

;;;; ---- Tests: Buffer naming ----

(ert-deftest agent-repl-test-buffer-name-format ()
  "Buffer names should follow *agent-panel-WS* and *agent-panel-input-WS* pattern."
  (cl-letf (((symbol-function '+workspace-current-name) (lambda () "my-ws")))
    (should (equal (agent-repl--buffer-name) "*agent-panel-my-ws*"))
    (should (equal (agent-repl--buffer-name "-input") "*agent-panel-input-my-ws*"))))

(ert-deftest agent-repl-test-buffer-name-default ()
  "Buffer name signals an error when no workspace name is available."
  (cl-letf (((symbol-function '+workspace-current-name) (lambda () nil)))
    (should-error (agent-repl--buffer-name) :type 'error)))

(ert-deftest agent-repl-test-buffer-name-empty-ws ()
  "Buffer name signals an error when the resolved workspace name is empty."
  (cl-letf (((symbol-function '+workspace-current-name) (lambda () "")))
    (should-error (agent-repl--buffer-name) :type 'error))
  (should-error (agent-repl--buffer-name nil "") :type 'error))

(ert-deftest agent-repl-test-buffer-name-uses-explicit-ws ()
  "Buffer name should prefer the explicit WS argument over the current workspace."
  (cl-letf (((symbol-function '+workspace-current-name) (lambda () "current-ws")))
    (should (equal (agent-repl--buffer-name nil "other-ws")
                   "*agent-panel-other-ws*"))))

(ert-deftest agent-repl-test-buffer-name-sanitizes-unsafe-chars ()
  "Workspace names with unsafe characters should be sanitized to underscores."
  (should (equal (agent-repl--buffer-name nil "feat/login")
                 "*agent-panel-feat_login*"))
  (should (equal (agent-repl--buffer-name nil "ws with space")
                 "*agent-panel-ws_with_space*"))
  (should (equal (agent-repl--buffer-name "-input" "a*b")
                 "*agent-panel-input-a_b*")))

(ert-deftest agent-repl-test-sanitize-ws-name ()
  "sanitize-ws-name keeps alphanumerics, hyphens, and underscores."
  (should (equal (agent-repl--sanitize-ws-name "abc-123_xyz") "abc-123_xyz"))
  (should (equal (agent-repl--sanitize-ws-name "feat/login") "feat_login"))
  (should (equal (agent-repl--sanitize-ws-name "a b*c") "a_b_c"))
  (should-not (agent-repl--sanitize-ws-name nil)))

;;;; ---- Tests: Buffer predicates ----

;;;; ---- Tests: agent-view-buffer-name-p (the RENDERING predicate) ----

(ert-deftest agent-repl-test-agent-view-name-matches-the-gui-webview ()
  "A gui workspace shows its agent in the webview.
This is the whole point of the predicate: the tab-bar asks \"is the agent
view open\", and answering nil for every gui workspace is what left their
tabs permanently drawn as though the panels were closed."
  (should (agent-repl--agent-view-buffer-name-p "*agent-frontend-my-ws*")))

(ert-deftest agent-repl-test-agent-view-name-excludes-the-input-panel ()
  "The input panel alone is not a workspace showing its agent."
  (should-not (agent-repl--agent-view-buffer-name-p "*agent-panel-input-my-ws*")))

(ert-deftest agent-repl-test-agent-view-name-excludes-the-explain-config-popup ()
  "`SPC j h c' mounts a webview too, but it is not any workspace's agent view."
  (should-not (agent-repl--agent-view-buffer-name-p "*agent-explain-config*")))

(ert-deftest agent-repl-test-agent-view-name-excludes-an-unrelated-buffer ()
  "An ordinary buffer is not an agent view."
  (should-not (agent-repl--agent-view-buffer-name-p "*repl-test-non-agent-buf*")))

(ert-deftest agent-repl-test-agent-view-name-tolerates-a-non-string ()
  "A `window-state-get' tree can hold a non-string where a buffer name went."
  (should-not (agent-repl--agent-view-buffer-name-p nil)))

(ert-deftest agent-repl-test-agent-view-buffer-p-reads-the-current-buffer ()
  "The buffer-shaped form defers to the name-shaped one."
  (agent-repl-test--with-temp-buffer "*agent-frontend-my-ws*"
    (should (agent-repl--agent-view-buffer-p))))

;;;; ---- Tests: Logging ----

(defun agent-repl-test--production-lisp-files ()
  "Return the hand-written production Elisp files subject to source lints."
  (seq-filter
   (lambda (path)
     (not (string-prefix-p "test-" (file-name-nondirectory path))))
   (directory-files agent-repl-test--module-dir t "\\.el\\'")))

(defun agent-repl-test--walk-executable-form (form file owner visit)
  "Call VISIT for executable list forms within FORM from FILE and OWNER.
Quoted data, function argument lists, and variable binding names are not
executable forms and are deliberately excluded."
  (cond
   ((atom form))
   ((memq (car form) '(quote function declare-function)))
   ((memq (car form) '(defun defmacro cl-defun cl-defmacro))
    (dolist (body-form (cdddr form))
      (agent-repl-test--walk-executable-form
       body-form file (cadr form) visit)))
   ((eq (car form) 'lambda)
    (dolist (body-form (cddr form))
      (agent-repl-test--walk-executable-form body-form file owner visit)))
   ((memq (car form) '(let let*))
    (dolist (binding (cadr form))
      (when (consp binding)
        (dolist (initializer (cdr binding))
          (agent-repl-test--walk-executable-form initializer file owner visit))))
    (dolist (body-form (cddr form))
      (agent-repl-test--walk-executable-form body-form file owner visit)))
   (t
    (funcall visit file owner form)
    (let ((tail form))
      (while (consp tail)
        (agent-repl-test--walk-executable-form (car tail) file owner visit)
        (setq tail (cdr tail)))
      (when tail
        (agent-repl-test--walk-executable-form tail file owner visit))))))

(defun agent-repl-test--production-source-calls (callees)
  "Return production calls whose head is one of CALLEES.
Each result is `(RELATIVE-FILE OWNER FORM)'."
  (let (calls)
    (dolist (file (agent-repl-test--production-lisp-files))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (condition-case nil
            (while t
              (agent-repl-test--walk-executable-form
               (read (current-buffer)) file nil
               (lambda (source owner form)
                 (when (memq (car form) callees)
                   (push (list (file-relative-name source agent-repl-test--module-dir)
                               owner form)
                         calls)))))
          (end-of-file nil))))
    (nreverse calls)))

(defun agent-repl-test--source-format-prefix (form)
  "Return FORM's statically knowable leading format text, or nil."
  (cond
   ((stringp form) form)
   ((and (consp form) (eq (car form) 'concat))
    (let ((parts (cdr form))
          (prefix ""))
      (while (and parts (stringp (car parts)))
        (setq prefix (concat prefix (pop parts))))
      (unless (string-empty-p prefix) prefix)))))

(ert-deftest agent-repl-test-logging-has-one-record-builder-and-writer ()
  "Only the canonical emitter may build a record or call the file writer."
  ;; Arrange / Act
  (let ((calls (agent-repl-test--production-source-calls
                '(agent-repl--log-record agent-repl--do-log-to-file
                  write-region))))
    ;; Assert
    (should
     (equal
      (mapcar (lambda (call) (list (car call) (cadr call) (car (nth 2 call))))
              calls)
      ;; The first pair is the once-per-workspace central-fallback record: a
      ;; workspace that cannot host a durable sink is announced from inside
      ;; the emitter precisely so the one builder and the one writer stay one.
      '(("core.el" agent-repl--do-log-to-file write-region)
        ("core.el" agent-repl--emit-log-record agent-repl--do-log-to-file)
        ("core.el" agent-repl--emit-log-record agent-repl--log-record)
        ("core.el" agent-repl--emit-log-record agent-repl--log-record)
        ("core.el" agent-repl--emit-log-record agent-repl--log-record)
        ("core.el" agent-repl--emit-log-record agent-repl--do-log-to-file)
        ("core.el" agent-repl--emit-log-record agent-repl--do-log-to-file)
        ("core.el" agent-repl--emit-log-record agent-repl--log-record)
        ("core.el" agent-repl--emit-log-record agent-repl--do-log-to-file))))))

(ert-deftest agent-repl-test-every-logging-rung-calls-the-canonical-emitter ()
  "Every public severity wrapper is a thin call into one emitter."
  ;; Arrange / Act
  (let* ((rungs '(agent-repl--log agent-repl--log-verbose agent-repl--info
                  agent-repl--warn agent-repl--warn-once agent-repl--error
                  agent-repl--fatal))
         (calls (agent-repl-test--production-source-calls
                 '(agent-repl--emit-log-record)))
         (owners (delete-dups (mapcar #'cadr calls))))
    ;; Assert
    (dolist (rung rungs)
      (should (memq rung owners)))))

(defun agent-repl-test--reasoned-log-scope (form tag)
  "Return FORM's reason when FORM is a quoted `(TAG REASON)' scope marker."
  (when (and (consp form)
             (eq (car form) 'quote)
             (consp (cadr form))
             (eq (car (cadr form)) tag)
             (consp (cdr (cadr form)))
             (null (cddr (cadr form)))
             (stringp (cadr (cadr form)))
             (not (string-empty-p (cadr (cadr form)))))
    (cadr (cadr form))))

(ert-deftest agent-repl-test-log-sites-have-explicit-attribution ()
  "Every log site names a workspace expression or a reasoned scope marker."
  ;; Arrange
  (let ((print-length nil)
        (print-level nil)
        (rungs '(agent-repl--log agent-repl--log-verbose agent-repl--info
                 agent-repl--warn agent-repl--warn-once agent-repl--error
                 agent-repl--fatal agent-repl--backend-phase))
        bare-nil
        malformed-markers
        central-sites)
    ;; Act
    (dolist (call (agent-repl-test--production-source-calls rungs))
      (let* ((form (nth 2 call))
             (scope (nth 1 form))
             (quoted (and (consp scope) (eq (car scope) 'quote)
                          (cadr scope)))
             (tag (and (consp quoted) (car quoted))))
        (when (null scope)
          (push (list (car call) (cadr call) form) bare-nil))
        (when (memq tag '(:agent-repl-central :agent-repl-context))
          (let ((reason (agent-repl-test--reasoned-log-scope scope tag)))
            (if reason
                (when (eq tag :agent-repl-central)
                  (push (list (car call) (cadr call) reason) central-sites))
              (push (list (car call) (cadr call) scope) malformed-markers))))))
    ;; Assert
    (should (null (nreverse bare-nil)))
    (should (null (nreverse malformed-markers)))
    ;; Keep the complete reasoned inventory in the assertion value so any
    ;; malformed central marker reports every reviewed central site beside it.
    (should (cl-every (lambda (site) (stringp (nth 2 site)))
                      (nreverse central-sites)))))

(defconst agent-repl-test--user-facing-message-sites
  '(("clipboard-image.el" agent-repl-attach-clipboard-image
     "agent-repl: attached image %s"
     "confirms the interactive attachment command")
    ("commands.el" agent-repl-copy-reference "Copied: %s"
     "confirms an interactive clipboard write")
    ("commands.el" agent-repl-copy-workspace-name "Copied workspace name: %s"
     "confirms an interactive clipboard write")
    ("commands.el" agent-repl-open-most-recent-workspace
     "All workspaces visited -- cycle reset"
     "reports the interactive workspace-cycle boundary")
    ("commands.el" agent-repl-switch-to-workspace
     "[agent-repl] No workspace tabs on the bar"
     "explains why the requested interactive switch did nothing")
    ("commands.el" agent-repl-switch-to-workspace
     "[agent-repl] No workspace tab %d -- the bar draws %d"
     "explains why the requested interactive switch did nothing")
    ("core.el" agent-repl--migrate-legacy-state
     "[agent-repl] migrated state %s -> %s"
     "announces an on-disk state migration")
    ("core.el" agent-repl--migrate-legacy-state
     "[agent-repl] WARNING: state migration %s -> %s failed: %S"
     "warns that on-disk state still needs user attention")
    ("core.el" agent-repl--do-log-to-file
     "[agent-repl] LOG SINK FAILURE path=%s error=%S"
     "the failed sink cannot persist its own emergency")
    ("core.el" agent-repl--emit-message "%s"
     "the canonical presentation boundary intentionally owns all log echoes")
    ("core.el" agent-repl-toggle-debug "[agent-repl] debug logging: %s"
     "confirms the interactive visibility toggle")
    ("core.el" agent-repl-set-log-file-level
     "[agent-repl] durable log level: %s"
     "confirms the interactive durability threshold")
    ("core.el" agent-repl-toggle-verbose-to-disk
     "[agent-repl] verbose logging to disk: %s%s"
     "confirms the interactive verbose-record toggle")
    ("core.el" agent-repl-print-git-branch
     "agent-repl loaded on branch: %s"
     "answers the interactive branch-report command")
    ("daemon.el" agent-repl-frontend-daemon-ensure "agent-repl: %s"
     "surfaces a daemon readiness failure to the requesting user")
    ("daemon.el" agent-repl-frontend-daemon-stop
     "agent-repl: no daemon link to stop"
     "explains why the interactive stop did nothing")
    ("daemon.el" agent-repl-frontend-daemon-stop
     "agent-repl: daemon shutting down"
     "confirms acceptance of the interactive stop")
    ("daemon.el" agent-repl-frontend-daemon-stop
     "agent-repl: daemon refused the shutdown"
     "reports refusal of the interactive stop")
    ("daemon.el" agent-repl-frontend-daemon-stop
     "agent-repl: could not reach the daemon to stop it"
     "reports failure of the interactive stop")
    ("daemon.el" agent-repl-frontend-daemon-restart
     "agent-repl: not restarted: %s"
     "reports the daemon-provided reason why interactive restart aborted")
    ("daemon.el" agent-repl-frontend-daemon-restart
     "agent-repl: not restarted: the accepted daemon stop never completed"
     "reports that interactive restart timed out before daemon departure")
    ("emoji.el" agent-repl-install-commit-emoji-hook
     "Backed up existing hook to %s"
     "reports the backup made by the interactive installer")
    ("emoji.el" agent-repl-install-commit-emoji-hook
     "Installed prepare-commit-msg hook to %s"
     "confirms the interactive installer")
    ("find-file-workspace.el" agent-repl-find-file-workspace-reset
     "agent-repl: find-file routing reset"
     "confirms the interactive routing reset")
    ("find-file-workspace.el" agent-repl--ffw-acquire
     "agent-repl: could not open the workspace for %s; opening the file here"
     "warns the user that file placement differs from the request")
    ("history.el" agent-repl-history-search
     "[agent-repl] input history is empty"
     "answers the interactive history search")
    ("input.el" agent-repl-input-response-selection-escape
     "%s"
     "warns, in the minibuffer, that a second consecutive command-mode escape clears the reply-to-a-past-response selection")
    ("input.el" agent-repl--input-on-bubble-refusal
     "agent-repl: the prompt was refused for this agent (%S)"
     "reports a prompt refusal requiring user action")
    ("input.el" agent-repl--input-on-bubble-refusal
     "agent-repl: refused -- %s%s"
     "reports a prompt refusal requiring user action")
    ("input.el" agent-repl--input-on-error
     "agent-repl: refused -- a merge is in flight for this workspace"
     "reports why the user's prompt was not accepted")
    ("input.el" agent-repl--input-on-error
     "agent-repl: this submission's key was already accepted; the earlier submission stands"
     "reports idempotent acceptance to the submitting user")
    ("input.el" agent-repl--input-on-error
     "agent-repl: %s%s"
     "tells the user their prompt met the cold gate and where to answer it")
    ("input.el" agent-repl--input-on-error
     "agent-repl: model change refused%s"
     "tells the user their model change was refused, in the refusal's own words")
    ("input.el" agent-repl--input-on-error
     "agent-repl: %s"
     "tells the user the workspace has no session yet and the daemon is starting one")
    ("input.el" agent-repl--input-on-error
     "agent-repl: submission refused (%S)"
     "reports why the user's prompt was not accepted")
    ("input.el" agent-repl--input-on-failure
     "agent-repl: the daemon did not answer; the prompt is held"
     "tells the user their submitted text remains queued")
    ("keybindings.el" agent-repl-reload-config "[agent-repl] Reloaded %s"
     "confirms the interactive source reload")
    ("magit.el" +dwc/magit-toggle-tags-in-log "magit commit-list tags %s"
     "confirms the interactive Magit display toggle")
    ("magit.el" +dwc/magit-copy-commit-link
     "GitHub commit link copied to clipboard: %s"
     "confirms an interactive clipboard write")
    ("magit.el" +dwc/open-workspace-pr-in-browser "Opened PR: %s"
     "confirms the interactive browser action")
    ("panels.el" agent-repl-workspace-push-to-back
     "Pushed '%s' to the back; switched to '%s'."
     "confirms the interactive tab reorder and selection")
    ("panels.el" agent-repl-workspace-push-to-back "Pushed '%s' to the back."
     "confirms the interactive tab reorder")
    ("prompt-queue.el" agent-repl-queue-deferred-prompt
     "agent-repl: no input to queue"
     "explains why the interactive queue command did nothing")
    ("prompt-queue.el" agent-repl-queue-deferred-prompt
     "agent-repl: queued prompt #%d for %s (fires when the turn settles)"
     "confirms the interactive queue command")
    ("roster.el" agent-repl-roster-echo-finished
     "Agent finished in workspace: %s"
     "is the configured user-facing completion notification")
    ("status.el" agent-repl-tabbar-apply-row-count
     "agent-repl: tab-bar set to %d line%s on this frame"
     "confirms the interactive tab-bar layout command")
    ("verbs.el" agent-repl-verbs--on-refusal "%s refused: %s%s"
     "reports a workspace command refusal to its user")
    ("verbs.el" agent-repl-verbs--send
     "agent-repl: %s failed -- the daemon did not answer"
     "reports failure of the user's workspace command")
    ("verbs.el" agent-repl-verb-close "close blocked -- %s"
     "reports why the user's close command was refused")
    ("verbs.el" agent-repl-verb-merge "merge enqueued"
     "confirms the user's merge command")
    ("verbs.el" agent-repl-verb-restart "agent-repl: restart %s"
     "confirms the user's restart command")
    ("verbs.el" agent-repl-verb-interrupt "agent-repl: turn stopped"
     "confirms the user's interrupt stopped the running turn")
    ("verbs.el" agent-repl-verb-interrupt "agent-repl: nothing to interrupt"
     "tells the user there was no running turn to interrupt")
    ("verbs.el" agent-repl-verb-interrupt "agent-repl: turn stopped, %s agent%s also ended"
     "confirms the interrupt stopped the turn and how many agents it also ended")
    ("verbs.el" agent-repl-verb-interrupt "agent-repl: the interrupt answer could not be read"
     "tells the user the interrupt outcome could not be read")
    ("verbs.el" agent-repl-verb-interrupt "agent-repl: turn left running"
     "tells the user the interrupt left the turn running")
    ("verbs.el" agent-repl-verb-set-priority "agent-repl: priority %s"
     "confirms the user's priority command")
    ("verbs.el" agent-repl-verbs--create-naming-refusal
     "create refused: the workspace could not be named (%s, %s attempt%s)%s"
     "tells the user why the workspace could not be named and what the model last said")
    ("verbs.el" agent-repl-verbs--create-policy-refusal
     "create refused: %s states no one-shot policy -- write %s"
     "tells the user which repository states no one-shot policy and what to write")
    ("verbs.el" agent-repl-verbs-select-minted
     "agent-repl: the new workspace carries no directory to switch to"
     "reports why the requested new workspace cannot be selected")
    ("verbs.el" agent-repl-verb-shutdown-schedule "agent-repl: shutdown %s"
     "confirms the user's shutdown-schedule command")
    ("verbs.el" agent-repl-verbs--merge-queue-on-error
     "merge-queue refused: the daemon's registry does not hold repository %S"
     "reports why the user's merge-queue command was refused")
    ("verbs.el" agent-repl-verb-merge-queue "agent-repl: merge queue %s"
     "confirms the user's merge-queue command"))
  "Every permitted production `message' call and its user-facing reason.")

(ert-deftest agent-repl-test-message-sites-are-explicitly-user-facing ()
  "No diagnostic may use `message' unless this audit names its user purpose."
  ;; Arrange / Act
  (let ((actual
         (mapcar (lambda (call)
                   (list (car call) (cadr call) (nth 1 (nth 2 call))))
                 (agent-repl-test--production-source-calls '(message))))
        (allowed
         (mapcar (lambda (entry)
                   (should (and (stringp (nth 3 entry))
                                (not (string-empty-p (nth 3 entry)))))
                   (cl-subseq entry 0 3))
                 agent-repl-test--user-facing-message-sites)))
    ;; Assert
    (should (equal actual allowed))))

(ert-deftest agent-repl-test-log-respects-debug-flag ()
  "When `agent-repl-debug' is nil, `agent-repl--log' should NOT call `message'.
When t, it should call `message'."
  (let ((message-called nil))
    (cl-letf (((symbol-function 'message)
               (lambda (&rest _args) (setq message-called t))))
      ;; debug off: no message
      (let ((agent-repl-debug nil))
        (agent-repl--log nil "test %s" "hello")
        (should-not message-called))
      ;; debug on: message called
      (let ((agent-repl-debug t))
        (setq message-called nil)
        (agent-repl--log nil "test %s" "hello")
        (should message-called)))))

;;;; ---- Tests: Deferred macro ----

(ert-deftest agent-repl-test-deferred-macro ()
  "The deferred macro should create a debouncing lambda."
  (let ((agent-repl--sync-timer nil)
        (call-count 0))
    (let ((debounced (agent-repl--deferred agent-repl--sync-timer
                       (lambda () (cl-incf call-count)))))
      ;; Calling it should set the timer var
      (funcall debounced)
      (should agent-repl--sync-timer)
      ;; Cancel it to prevent side effects
      (cancel-timer agent-repl--sync-timer)
      (setq agent-repl--sync-timer nil))))

;;;; ---- Bug regression tests ----

(ert-deftest agent-repl-test-bug10-defvar-declarations ()
  "Bug 10: All key variables should be properly declared.
Note: agent-repl--notification-backend may not be bound in headless
environments without notification tools (terminal-notifier or osascript)."
  (should (boundp 'agent-repl--workspaces))
  ;; notification-backend requires osascript or terminal-notifier at load time
  (when (or (executable-find "terminal-notifier") (executable-find "osascript"))
    (should (boundp 'agent-repl--notification-backend)))
  (should (boundp 'agent-repl--sync-timer)))

(ert-deftest agent-repl-test-bug11-fullscreen-config-stored-in-plist ()
  "Bug 11: fullscreen-config is per-workspace plist storage, not a global defvar."
  ;; The original test checked (boundp 'agent-repl--fullscreen-config), but that
  ;; symbol is never defvar'd.  The real storage is the :fullscreen-config plist
  ;; key accessed via ws-get/ws-put in panels.el.
  (agent-repl-test--with-clean-state
    (let ((ws-id "test-fc"))
      (puthash ws-id (list :status nil) agent-repl--workspaces)
      (agent-repl--ws-put ws-id :fullscreen-config 'some-config)
      (should (equal (agent-repl--ws-get ws-id :fullscreen-config) 'some-config)))))

(ert-deftest agent-repl-test-package-provide ()
  "Package should provide 'agent-repl feature."
  (should (featurep 'agent-repl)))

;;;; ---- Tests: cancel-all-timers ----

(ert-deftest agent-repl-test-cancel-all-timers-empty-list ()
  "Cancelling timers with an empty list should be a no-op."
  (let ((agent-repl--timers nil))
    (agent-repl--cancel-all-timers)
    (should (null agent-repl--timers))))

(ert-deftest agent-repl-test-cancel-all-timers-mix-valid-and-nil ()
  "Cancelling timers with mix of valid timers and nil entries should not error."
  (let* ((timer1 (run-with-timer 9999 nil #'ignore))
         (agent-repl--timers (list timer1 nil nil)))
    (agent-repl--cancel-all-timers)
    (should (null agent-repl--timers))))

(ert-deftest agent-repl-test-cancel-all-timers-already-cancelled ()
  "Cancelling already-cancelled timers should not error."
  (let* ((timer1 (run-with-timer 9999 nil #'ignore)))
    (cancel-timer timer1)
    (let ((agent-repl--timers (list timer1)))
      (agent-repl--cancel-all-timers)
      (should (null agent-repl--timers)))))

(ert-deftest agent-repl-test-cancel-all-timers-sets-nil ()
  "After cancellation, `agent-repl--timers' should be nil."
  (let* ((timer1 (run-with-timer 9999 nil #'ignore))
         (timer2 (run-with-timer 9999 nil #'ignore))
         (agent-repl--timers (list timer1 timer2)))
    (agent-repl--cancel-all-timers)
    (should (null agent-repl--timers))))

(ert-deftest agent-repl-test-cancel-all-timers-idempotent ()
  "Calling cancel-all-timers twice should be safe."
  (let* ((timer1 (run-with-timer 9999 nil #'ignore))
         (agent-repl--timers (list timer1)))
    (agent-repl--cancel-all-timers)
    (should (null agent-repl--timers))
    (agent-repl--cancel-all-timers)
    (should (null agent-repl--timers))))

;;;; ---- Tests: log-format ----

(ert-deftest agent-repl-test-log-format-empty-string ()
  "log-format with empty string should still include timestamp and tag."
  (let ((result (agent-repl--log-format nil "")))
    (should (string-match-p "\\[agent-repl\\]" result))
    (should (string-match-p "^[0-9][0-9]:[0-9][0-9]:[0-9][0-9]\\." result))))

(ert-deftest agent-repl-test-log-format-with-format-specifiers ()
  "log-format should pass format specifiers through literally, not expand them."
  (let ((result (agent-repl--log-format nil "hello %s %d")))
    (should (string-match-p "%s" result))
    (should (string-match-p "%d" result))))

(ert-deftest agent-repl-test-log-format-contains-timestamp-and-tag ()
  "log-format output should contain timestamp and [agent-repl] tag."
  (let ((result (agent-repl--log-format nil "test message")))
    (should (string-match-p "^[0-9][0-9]:[0-9][0-9]:[0-9][0-9]\\.[0-9]+" result))
    (should (string-match-p "\\[agent-repl\\]" result))
    (should (string-match-p "test message$" result))))

;;;; ---- Tests: do-log ----

(ert-deftest agent-repl-test-do-log-fmt-no-args ()
  "do-log with fmt and no args should call message with formatted prefix."
  (let ((captured-msg nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (setq captured-msg (apply #'format fmt args)))))
      (agent-repl--do-log nil "simple message" nil)
      (should (string-match-p "\\[agent-repl\\] simple message" captured-msg)))))

(ert-deftest agent-repl-test-do-log-fmt-multiple-args ()
  "do-log with fmt and multiple args should expand them."
  (let ((captured-msg nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (setq captured-msg (apply #'format fmt args)))))
      (agent-repl--do-log nil "hello %s and %d" '("world" 42))
      (should (string-match-p "hello world and 42" captured-msg)))))

(ert-deftest agent-repl-test-do-log-nil-args ()
  "do-log with nil args should work like no args."
  (let ((captured-msg nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (setq captured-msg (apply #'format fmt args)))))
      (agent-repl--do-log nil "no args here" nil)
      (should (string-match-p "no args here" captured-msg)))))

(ert-deftest agent-repl-test-do-log-message-has-prefix ()
  "do-log should emit message with timestamp prefix."
  (let ((captured-msg nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (setq captured-msg (apply #'format fmt args)))))
      (agent-repl--do-log nil "test" nil)
      (should (string-match-p "^[0-9][0-9]:[0-9][0-9]:[0-9][0-9]\\." captured-msg)))))

(ert-deftest agent-repl-test-do-log-error-p-signals-error ()
  "do-log with ERROR-P non-nil should signal `error' instead of calling `message'."
  (let ((message-called nil))
    (cl-letf (((symbol-function 'message)
               (lambda (&rest _) (setq message-called t)))
              ((symbol-function 'agent-repl--do-log-to-file) #'ignore))
      (should-error (agent-repl--do-log nil "boom %s" '("reason") t) :type 'error)
      (should-not message-called))))

(ert-deftest agent-repl-test-do-log-error-p-includes-formatted-body ()
  "do-log with ERROR-P should include FMT expansion in the error's data."
  (cl-letf (((symbol-function 'agent-repl--do-log-to-file) #'ignore))
    (condition-case err
        (progn
          (agent-repl--do-log nil "thing failed: %s" '("why") t)
          (should nil))
      (error
       (should (string-match-p "thing failed: why" (error-message-string err)))))))

(ert-deftest agent-repl-test-do-log-error-p-writes-to-file-before-signalling ()
  "do-log with ERROR-P should write to the logfile before the error unwinds execution."
  (let ((file-write-called nil)
        (agent-repl-log-to-file t))
    (cl-letf (((symbol-function 'agent-repl--do-log-to-file)
               (lambda (&rest _args) (setq file-write-called t))))
      (ignore-errors
        (agent-repl--do-log nil "boom" nil t))
      (should file-write-called))))

;;;; ---- Tests: log ----

(ert-deftest agent-repl-test-log-verbose-symbol-still-logs ()
  "`agent-repl--log' should log when debug is set to 'verbose (non-nil)."
  (let ((message-called nil))
    (cl-letf (((symbol-function 'message)
               (lambda (&rest _args) (setq message-called t))))
      (let ((agent-repl-debug 'verbose))
        (agent-repl--log nil "test %s" "verbose")
        (should message-called)))))

(ert-deftest agent-repl-test-log-multiple-format-args ()
  "`agent-repl--log' should correctly expand multiple format arguments."
  (let ((captured-msg nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (setq captured-msg (apply #'format fmt args)))))
      (let ((agent-repl-debug t))
        (agent-repl--log nil "a=%s b=%d c=%s" "x" 1 "z")
        (should (string-match-p "a=x b=1 c=z" captured-msg))))))

(ert-deftest agent-repl-test-log-bare-string ()
  "`agent-repl--log' with bare string (no format args) should work."
  (let ((captured-msg nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (setq captured-msg (apply #'format fmt args)))))
      (let ((agent-repl-debug t))
        (agent-repl--log nil "bare message")
        (should (string-match-p "bare message" captured-msg))))))

(ert-deftest agent-repl-test-log-includes-timestamp ()
  "`agent-repl--log' output should include timestamp prefix."
  (let ((captured-msg nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (setq captured-msg (apply #'format fmt args)))))
      (let ((agent-repl-debug t))
        (agent-repl--log nil "ts check")
        (should (string-match-p "^[0-9][0-9]:[0-9][0-9]:[0-9][0-9]\\." captured-msg))))))

;;;; ---- Tests: log-verbose ----

(ert-deftest agent-repl-test-log-verbose-nil-no-message ()
  "`agent-repl--log-verbose' should not emit to *Messages* when debug is nil."
  (let ((message-called nil))
    (cl-letf (((symbol-function 'message)
               (lambda (&rest _args) (setq message-called t))))
      (let ((agent-repl-debug nil))
        (agent-repl--log-verbose nil "test")
        (should-not message-called)))))

(ert-deftest agent-repl-test-log-verbose-t-no-message ()
  "`agent-repl--log-verbose' should not emit to *Messages* when debug is t."
  (let ((message-called nil))
    (cl-letf (((symbol-function 'message)
               (lambda (&rest _args) (setq message-called t))))
      (let ((agent-repl-debug t))
        (agent-repl--log-verbose nil "test")
        (should-not message-called)))))

(ert-deftest agent-repl-test-log-verbose-verbose-emits-message ()
  "`agent-repl--log-verbose' should emit to *Messages* when debug is `verbose'."
  (let ((message-called nil))
    (cl-letf (((symbol-function 'message)
               (lambda (&rest _args) (setq message-called t))))
      (let ((agent-repl-debug 'verbose))
        (agent-repl--log-verbose nil "test")
        (should message-called)))))

;;;; ---- Tests: workspace-owned live log buffers ----

(ert-deftest agent-repl-test-workspace-log-buffer-is-owned-and-attached ()
  "The live log buffer is created through the workspace attachment boundary."
  (let ((buf nil)
        (attached nil)
        (agent-repl--workspace-log-buffer-enabled t))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-repl--ws-resolve-persp)
                   (lambda (_ws) 'fake-persp))
                  ((symbol-function 'agent-repl--ws-add-buffer)
                   (lambda (buffer persp switch)
                     (setq attached (list buffer persp switch)))))
          (setq buf (agent-repl--workspace-log-buffer "ws-live-log"))
          (should (equal (buffer-name buf) "*agent-panel-log-ws-live-log*"))
          (should (equal (agent-repl--buffer-owner buf) "ws-live-log"))
          (should (equal attached (list buf 'fake-persp nil))))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

(ert-deftest agent-repl-test-append-workspace-log-keeps-workspaces-isolated ()
  "Each live log buffer contains its exact lines and no other workspace's lines."
  (let ((first nil)
        (second nil)
        (agent-repl--workspace-log-buffer-enabled t))
    (unwind-protect
        (progn
          (agent-repl--append-workspace-log "first-log-ws" "first exact line")
          (agent-repl--append-workspace-log "second-log-ws" "second exact line")
          (setq first (agent-repl--workspace-log-buffer "first-log-ws")
                second (agent-repl--workspace-log-buffer "second-log-ws"))
          (should (equal (with-current-buffer first (buffer-string))
                         "first exact line\n"))
          (should (equal (with-current-buffer second (buffer-string))
                         "second exact line\n")))
      (dolist (buf (list first second))
        (when (buffer-live-p buf)
          (kill-buffer buf))))))

(ert-deftest agent-repl-test-log-ladder-routes-workspace-lines-to-live-log-buffer ()
  "Every emitted workspace log-ladder line reaches its live buffer once."
  (agent-repl-test--with-clean-state
    (let ((ws "ladder-log-ws")
          (project (make-temp-file "agent-repl-live-ladder-" t))
          (buf nil)
          (agent-repl-debug nil)
          (agent-repl-log-to-file nil)
          ;; The ladder's verbose rung is one of the lines this asserts on, so
          ;; the BUFFER threshold is opened to admit it.  Its default excludes
          ;; verbose; that exclusion is the sibling test's subject.
          (agent-repl-log-buffer-level 'verbose)
          (agent-repl--workspace-log-buffer-enabled nil))
      (unwind-protect
          (progn
            (agent-repl--ws-put ws :project-dir project)
            (let ((agent-repl--workspace-log-buffer-enabled t))
              (agent-repl--log ws "normal-entry")
              (agent-repl--info ws "info-entry")
              (agent-repl--warn ws "warn-entry")
              (agent-repl--error ws "error-entry")
              ;; Verbose records persist even when terminal visibility is off.
              (agent-repl--log-verbose ws "terminal-hidden-verbose-entry")
              (let ((agent-repl-debug 'verbose))
                (agent-repl--log-verbose ws "terminal-visible-verbose-entry")))
            (setq buf (agent-repl--workspace-log-buffer ws))
            (let* ((contents (with-current-buffer buf (buffer-string)))
                   (records (mapcar
                             (lambda (line)
                               (json-parse-string line :object-type 'alist))
                             (split-string contents "\n" t)))
                   (messages (mapcar
                              (lambda (record) (alist-get 'message record))
                              records)))
              (should (equal messages
                             '("normal-entry" "info-entry"
                               "WARNING: warn-entry" "ERROR: error-entry"
                               "terminal-hidden-verbose-entry"
                               "terminal-visible-verbose-entry")))))
        (when (buffer-live-p buf)
          (kill-buffer buf))
        (delete-directory project t)))))

;;;; ---- Tests: echo-area (modeline) severity gate ----
;;
;; The invariant under test: the log file and *Messages* are the QUIET sink
;; and take everything; the echo area / modeline is the LOUD sink and is
;; reserved for GENUINE FATAL conditions alone, which reach it by SIGNALLING
;; an `error' (`agent-repl--do-log' with ERROR-P), not through the gate
;; below.  EVERY ladder level — `agent-repl--warn' and `agent-repl--error'
;; alike — emits quietly.
;; `inhibit-message' is what separates the quiet emits — when it is non-nil
;; at the moment `message' runs, the line reaches *Messages* but never the
;; echo area.  So "did this quiet line reach the modeline?" is exactly "was
;; `inhibit-message' nil inside the `message' call?".

(defun agent-repl-test--capture-emission (thunk)
  "Run THUNK with `message' stubbed and report what it emitted, and how loudly.
Returns a plist (:text TEXT :echoed BOOL :called BOOL), where :echoed is
non-nil only when `inhibit-message' was nil at `message' time — i.e. only
when the line actually reached the echo area / modeline.  File writes are
suppressed so the suite never touches the real logfile."
  (let ((text nil) (echoed nil) (called nil))
    (cl-letf (((symbol-function 'agent-repl--do-log-to-file) #'ignore)
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq called t
                       text (apply #'format fmt args)
                       echoed (not inhibit-message)))))
      (funcall thunk))
    (list :text text :echoed echoed :called called)))

(ert-deftest agent-repl-test-emit-message-echo-nil-suppresses-echo-area ()
  "`agent-repl--emit-message' with ECHO nil binds `inhibit-message'."
  (let ((res (agent-repl-test--capture-emission
              (lambda () (agent-repl--emit-message "quiet line" nil)))))
    (should-not (plist-get res :echoed))))

(ert-deftest agent-repl-test-emit-message-echo-nil-still-reaches-messages ()
  "`agent-repl--emit-message' with ECHO nil still calls `message' (so *Messages* gets it)."
  (let ((res (agent-repl-test--capture-emission
              (lambda () (agent-repl--emit-message "quiet line" nil)))))
    (should (plist-get res :called))
    (should (equal "quiet line" (plist-get res :text)))))

(ert-deftest agent-repl-test-emit-message-echo-t-reaches-echo-area ()
  "`agent-repl--emit-message' with ECHO non-nil leaves `inhibit-message' unbound."
  (let ((res (agent-repl-test--capture-emission
              (lambda () (agent-repl--emit-message "loud line" t)))))
    (should (plist-get res :echoed))))

(ert-deftest agent-repl-test-info-never-reaches-echo-area ()
  "`agent-repl--info' is the quiet sink: it must NOT reach the echo area."
  (let ((res (agent-repl-test--capture-emission
              (lambda () (agent-repl--info nil "background chatter")))))
    (should (plist-get res :called))
    (should-not (plist-get res :echoed))))

(ert-deftest agent-repl-test-info-emits-when-debug-off ()
  "`agent-repl--info' is ungated by `agent-repl-debug' — it always records."
  (let* ((agent-repl-debug nil)
         (res (agent-repl-test--capture-emission
               (lambda () (agent-repl--info nil "recorded anyway")))))
    (should (plist-get res :called))
    (should (string-match-p "recorded anyway" (plist-get res :text)))))

(ert-deftest agent-repl-test-info-writes-to-file ()
  "`agent-repl--info' always writes to the logfile."
  (let ((written nil)
        (agent-repl-log-to-file t))
    (cl-letf (((symbol-function 'agent-repl--do-log-to-file)
               (lambda (text &rest _args) (setq written text)))
              ((symbol-function 'message) #'ignore))
      (agent-repl--info nil "on the record")
      (should (string-match-p "on the record" written)))))

(ert-deftest agent-repl-test-warn-never-reaches-echo-area ()
  "`agent-repl--warn' is now quiet: a non-fatal warning must NOT reach the modeline."
  (let ((res (agent-repl-test--capture-emission
              (lambda () (agent-repl--warn nil "something broke")))))
    (should-not (plist-get res :echoed))))

(ert-deftest agent-repl-test-warn-still-emits-to-messages ()
  "`agent-repl--warn' still emits (into *Messages*) even though it is quiet."
  (let ((res (agent-repl-test--capture-emission
              (lambda () (agent-repl--warn nil "something broke")))))
    (should (plist-get res :called))
    (should (string-match-p "something broke" (plist-get res :text)))))

(ert-deftest agent-repl-test-warn-prepends-severity-tag ()
  "`agent-repl--warn' prepends a `WARNING: ' tag so call sites need not."
  (let ((res (agent-repl-test--capture-emission
              (lambda () (agent-repl--warn nil "disk on fire")))))
    (should (string-match-p "WARNING: disk on fire" (plist-get res :text)))))

(ert-deftest agent-repl-test-warn-expands-format-args ()
  "`agent-repl--warn' takes &rest ARGS and expands them into FMT."
  (let ((res (agent-repl-test--capture-emission
              (lambda () (agent-repl--warn nil "failed %s after %d tries" "sync" 3)))))
    (should (string-match-p "WARNING: failed sync after 3 tries" (plist-get res :text)))))

(ert-deftest agent-repl-test-warn-non-string-fmt-preserves-args ()
  "A non-string FMT is a caller bug: `agent-repl--warn' must route it to the
existing bug-capture path WITHOUT dropping ARGS on the floor."
  (let ((res (agent-repl-test--capture-emission
              (lambda ()
                (cl-letf (((symbol-function 'agent-repl--log-format-capture-bug) #'ignore))
                  (agent-repl--warn nil 'not-a-string "kept"))))))
    (should (string-match-p "BUG non-string-fmt" (plist-get res :text)))))

(ert-deftest agent-repl-test-warn-writes-to-file ()
  "`agent-repl--warn' always writes to the logfile."
  (let ((written nil)
        (agent-repl-log-to-file t))
    (cl-letf (((symbol-function 'agent-repl--do-log-to-file)
               (lambda (text &rest _args) (setq written text)))
              ((symbol-function 'message) #'ignore))
      (agent-repl--warn nil "wrote this")
      (should (string-match-p "WARNING: wrote this" written)))))

(defun agent-repl-test--recorded-log-field (field thunk)
  "Return FIELD of the single JSONL record THUNK writes to the global sink."
  (let ((written nil)
        (agent-repl-log-to-file t))
    (cl-letf (((symbol-function 'agent-repl--do-log-to-file)
               (lambda (text &rest _args) (setq written text)))
              ((symbol-function 'message) #'ignore))
      (funcall thunk))
    (alist-get (intern field)
               (json-parse-string written :object-type 'alist))))

(ert-deftest agent-repl-test-warn-operation-drops-the-severity-tag ()
  "A warned operation is recorded under its BARE slug, not a warning- one.
logging-contract.md reserves `level' for severity and requires
`operation' to be stable, so the same logical operation must not change
name because the warn rung logged it."
  (should (equal (agent-repl-test--recorded-log-field
                  "operation"
                  (lambda () (agent-repl--warn nil "elisp.daemon.stale-addr")))
                 "agent-repl.elisp-daemon-stale-addr")))

(ert-deftest agent-repl-test-warn-records-the-warn-level ()
  "The severity a warn dropped from `operation' is carried by `level'."
  (should (equal (agent-repl-test--recorded-log-field
                  "level"
                  (lambda () (agent-repl--warn nil "elisp.daemon.stale-addr")))
                 "warn")))

(ert-deftest agent-repl-test-warn-record-keeps-the-displayed-tag ()
  "Dropping the tag from `operation' does not drop it from the message."
  (should (equal (agent-repl-test--recorded-log-field
                  "message"
                  (lambda () (agent-repl--warn nil "elisp.daemon.stale-addr")))
                 "WARNING: elisp.daemon.stale-addr")))

(ert-deftest agent-repl-test-error-operation-drops-the-severity-tag ()
  "An errored operation is recorded under its BARE slug too."
  (should (equal (agent-repl-test--recorded-log-field
                  "operation"
                  (lambda () (agent-repl--error nil "elisp.rpc.push-invalid")))
                 "agent-repl.elisp-rpc-push-invalid")))

(ert-deftest agent-repl-test-error-records-the-error-level ()
  "The error rung's severity lives in `level', as the contract says."
  (should (equal (agent-repl-test--recorded-log-field
                  "level"
                  (lambda () (agent-repl--error nil "elisp.rpc.push-invalid")))
                 "error")))

(ert-deftest agent-repl-test-error-record-keeps-the-displayed-tag ()
  "The `ERROR: ' tag still reaches the recorded message."
  (should (equal (agent-repl-test--recorded-log-field
                  "message"
                  (lambda () (agent-repl--error nil "elisp.rpc.push-invalid")))
                 "ERROR: elisp.rpc.push-invalid")))

(ert-deftest agent-repl-test-debug-operation-is-unchanged ()
  "The debug rung tags nothing, so its operation is untouched by the fix."
  (should (equal (agent-repl-test--recorded-log-field
                  "operation"
                  (lambda () (agent-repl--log nil "elisp.daemon.stale-addr")))
                 "agent-repl.elisp-daemon-stale-addr")))

(ert-deftest agent-repl-test-info-operation-is-unchanged ()
  "The info rung tags nothing either."
  (should (equal (agent-repl-test--recorded-log-field
                  "operation"
                  (lambda () (agent-repl--info nil "elisp.daemon.stale-addr")))
                 "agent-repl.elisp-daemon-stale-addr")))

(ert-deftest agent-repl-test-log-never-reaches-echo-area-when-debug-on ()
  "Turning debug logging on must not turn the modeline into a firehose."
  (let* ((agent-repl-debug t)
         (res (agent-repl-test--capture-emission
               (lambda () (agent-repl--log nil "debug chatter")))))
    (should (plist-get res :called))
    (should-not (plist-get res :echoed))))

(ert-deftest agent-repl-test-log-verbose-never-reaches-echo-area ()
  "Verbose hot-path chatter must not reach the echo area either."
  (let* ((agent-repl-debug 'verbose)
         (res (agent-repl-test--capture-emission
               (lambda () (agent-repl--log-verbose nil "hot path")))))
    (should (plist-get res :called))
    (should-not (plist-get res :echoed))))

(ert-deftest agent-repl-test-do-log-non-error-never-reaches-echo-area ()
  "`agent-repl--do-log' non-error path is quiet — it must NOT reach the modeline."
  (let ((res (agent-repl-test--capture-emission
              (lambda () (agent-repl--do-log nil "invariant violated" nil)))))
    (should (plist-get res :called))
    (should-not (plist-get res :echoed))))

(ert-deftest agent-repl-test-do-log-error-p-bypasses-quiet-gate ()
  "`agent-repl--do-log' with ERROR-P signals directly, never routing through the
quiet `agent-repl--emit-message' gate, so a fatal line always reaches the modeline."
  (let ((emitted nil))
    (cl-letf (((symbol-function 'agent-repl--do-log-to-file) #'ignore)
              ((symbol-function 'agent-repl--emit-message)
               (lambda (&rest _) (setq emitted t))))
      (should-error (agent-repl--do-log nil "fatal boom" nil t) :type 'error)
      (should-not emitted))))

(ert-deftest agent-repl-test-log-verbose-includes-timestamp ()
  "`agent-repl--log-verbose' output should include timestamp prefix."
  (let ((captured-msg nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (setq captured-msg (apply #'format fmt args)))))
      (let ((agent-repl-debug 'verbose))
        (agent-repl--log-verbose nil "ts check")
        (should (string-match-p "^[0-9][0-9]:[0-9][0-9]:[0-9][0-9]\\." captured-msg))))))

;;;; ---- Tests: error ----

(ert-deftest agent-repl-test-fatal-signals ()
  "`agent-repl--fatal' aborts the caller: it signals, unlike `--error'."
  (cl-letf (((symbol-function 'agent-repl--do-log-to-file) #'ignore))
    (should-error (agent-repl--fatal nil "bad %s" "thing") :type 'error)))

(ert-deftest agent-repl-test-fatal-formats-fmt-and-args ()
  "`agent-repl--fatal' expands FMT with ARGS in the signalled message."
  (cl-letf (((symbol-function 'agent-repl--do-log-to-file) #'ignore))
    (let ((err (should-error (agent-repl--fatal nil "ws=%s code=%d" "foo" 7))))
      (should (string-match-p "ws=foo code=7" (error-message-string err))))))

(ert-deftest agent-repl-test-fatal-includes-agent-repl-tag ()
  "`agent-repl--fatal' output includes the [agent-repl] tag."
  (cl-letf (((symbol-function 'agent-repl--do-log-to-file) #'ignore))
    (let ((err (should-error (agent-repl--fatal nil "something"))))
      (should (string-match-p "\\[agent-repl\\] something"
                              (error-message-string err))))))

(ert-deftest agent-repl-test-fatal-persists-record-at-level-error ()
  "`agent-repl--fatal' records at level \"error\" before it signals."
  (let ((captured nil))
    (cl-letf (((symbol-function 'agent-repl--emit-log-record)
               (lambda (_ws level _verbosity _fmt _args &rest _options)
                 (setq captured level)
                 (error "boom"))))
      (should-error (agent-repl--fatal nil "boom"))
      (should (equal captured "error")))))

(ert-deftest agent-repl-test-fatal-persists-before-signalling ()
  "The record is written BEFORE the signal, so the failure is never lost."
  (let ((order nil))
    (cl-letf (((symbol-function 'agent-repl--emit-log-record)
               (lambda (&rest _)
                 (push 'persisted order)
                 (error "boom"))))
      (ignore-errors (agent-repl--fatal nil "boom"))
      (push 'unwound order))
    (should (equal (nreverse order) '(persisted unwound)))))

(ert-deftest agent-repl-test-error-does-not-signal ()
  "`agent-repl--error' RECORDS a failure; it never unwinds the caller.
The error rung exists so a branch can log its own failure and still return
one to its caller.  A caller that must abort signals for itself."
  (cl-letf (((symbol-function 'agent-repl--do-log-to-file) #'ignore)
            ((symbol-function 'agent-repl--emit-message) #'ignore))
    (should (equal (progn (agent-repl--error nil "bad %s" "thing") 'returned)
                   'returned))))

(ert-deftest agent-repl-test-error-emits-regardless-of-debug ()
  "`agent-repl--error' emits with `agent-repl-debug' nil — it is ungated."
  (let ((agent-repl-debug nil))
    (let ((res (agent-repl-test--capture-emission
                (lambda () (agent-repl--error nil "bad %s" "thing")))))
      (should (plist-get res :called)))))

(ert-deftest agent-repl-test-error-emits-quietly ()
  "`agent-repl--error' stays on the QUIET sink: it never reaches the echo area."
  (let ((res (agent-repl-test--capture-emission
              (lambda () (agent-repl--error nil "bad thing")))))
    (should-not (plist-get res :echoed))))

(ert-deftest agent-repl-test-error-formats-fmt-and-args ()
  "`agent-repl--error' expands FMT with ARGS in the emitted line."
  (let ((res (agent-repl-test--capture-emission
              (lambda () (agent-repl--error nil "ws=%s code=%d" "foo" 7)))))
    (should (string-match-p "ws=foo code=7" (plist-get res :text)))))

(ert-deftest agent-repl-test-error-includes-agent-repl-tag ()
  "`agent-repl--error' output includes the [agent-repl] tag."
  (let ((res (agent-repl-test--capture-emission
              (lambda () (agent-repl--error nil "something")))))
    (should (string-match-p "\\[agent-repl\\] ERROR: something" (plist-get res :text)))))

(ert-deftest agent-repl-test-error-prepends-severity-tag ()
  "`agent-repl--error' prepends the `ERROR: ' severity tag, as `--warn' does."
  (let ((res (agent-repl-test--capture-emission
              (lambda () (agent-repl--error nil "boom")))))
    (should (string-match-p "ERROR: boom" (plist-get res :text)))))

(ert-deftest agent-repl-test-error-passes-non-string-fmt-through-untouched ()
  "A non-string FMT is handed through rather than `concat'-ed into a type error."
  (cl-letf (((symbol-function 'agent-repl--do-log-to-file) #'ignore)
            ((symbol-function 'agent-repl--emit-message) #'ignore))
    (should (equal (progn (agent-repl--error nil 'not-a-string) 'returned)
                   'returned))))

(ert-deftest agent-repl-test-error-persists-record-at-level-error ()
  "The persisted JSONL record carries `level' = \"error\" per logging-contract.md."
  (let ((captured nil))
    (cl-letf (((symbol-function 'agent-repl--emit-message) #'ignore)
              ((symbol-function 'agent-repl--emit-log-record)
               (lambda (_ws level _verbosity _fmt _args &rest _options)
                 (setq captured level))))
      (agent-repl--error nil "boom")
      (should (equal captured "error")))))

(ert-deftest agent-repl-test-error-persists-record-at-normal-verbosity ()
  "The persisted record is `normal' verbosity, so the durable sink keeps it."
  (let ((captured nil))
    (cl-letf (((symbol-function 'agent-repl--emit-message) #'ignore)
              ((symbol-function 'agent-repl--emit-log-record)
               (lambda (_ws _level verbosity _fmt _args &rest _options)
                 (setq captured verbosity))))
      (agent-repl--error nil "boom")
      (should (equal captured "normal")))))

(ert-deftest agent-repl-test-error-record-clears-default-file-threshold ()
  "An error record persists under the default `agent-repl-log-file-level'."
  (should (agent-repl--log-record-persists-p "error" "normal")))

(ert-deftest agent-repl-test-error-record-clears-default-buffer-threshold ()
  "An error record displays under the default `agent-repl-log-buffer-level'."
  (should (agent-repl--log-record-displays-p "error" "normal")))

;;;; ---- Tests: runtime log-verbosity controls ----

(ert-deftest agent-repl-test-log-level-environment-accepts-the-shared-vocabulary ()
  "The process switch accepts every contract level and defaults to info."
  (dolist (case '((nil . info)
                  ("debug" . debug)
                  ("info" . info)
                  ("warn" . warn)
                  ("error" . error)))
    ;; Arrange
    (let ((process-environment (copy-sequence process-environment)))
      (setenv "AGENT_REPL_LOG_LEVEL" (car case))
      ;; Act / Assert
      (should (eq (agent-repl--log-level-from-environment) (cdr case))))))

(ert-deftest agent-repl-test-log-level-environment-rejects-an-unknown-value ()
  "A misspelled process threshold aborts instead of changing log volume."
  ;; Arrange
  (let ((process-environment (copy-sequence process-environment)))
    (setenv "AGENT_REPL_LOG_LEVEL" "verbose")
    ;; Act / Assert
    (should-error (agent-repl--log-level-from-environment) :type 'error)))

(ert-deftest agent-repl-test-toggle-debug-turns-visibility-on ()
  "`agent-repl-toggle-debug' turns *Messages* visibility on from nil."
  (cl-letf (((symbol-function 'message) #'ignore))
    (let ((agent-repl-debug nil))
      (agent-repl-toggle-debug)
      (should (eq agent-repl-debug t)))))

(ert-deftest agent-repl-test-toggle-debug-turns-visibility-off ()
  "`agent-repl-toggle-debug' turns visibility back off from t."
  (cl-letf (((symbol-function 'message) #'ignore))
    (let ((agent-repl-debug t))
      (agent-repl-toggle-debug)
      (should (eq agent-repl-debug nil)))))

(ert-deftest agent-repl-test-toggle-debug-prefix-selects-verbose ()
  "With a prefix argument the toggle selects the verbose rung."
  (cl-letf (((symbol-function 'message) #'ignore))
    (let ((agent-repl-debug nil))
      (agent-repl-toggle-debug t)
      (should (eq agent-repl-debug 'verbose)))))

(ert-deftest agent-repl-test-toggle-debug-prefix-clears-verbose ()
  "With a prefix argument the toggle clears an existing verbose setting."
  (cl-letf (((symbol-function 'message) #'ignore))
    (let ((agent-repl-debug 'verbose))
      (agent-repl-toggle-debug t)
      (should (eq agent-repl-debug nil)))))

(ert-deftest agent-repl-test-toggle-debug-leaves-the-file-level-alone ()
  "The visibility toggle does not touch durable log volume."
  (cl-letf (((symbol-function 'message) #'ignore))
    (let ((agent-repl-debug nil)
          (agent-repl-log-file-level 'debug))
      (agent-repl-toggle-debug)
      (should (eq agent-repl-log-file-level 'debug)))))

(ert-deftest agent-repl-test-toggle-debug-is-interactive ()
  "`agent-repl-toggle-debug' is a user-facing command."
  (should (commandp 'agent-repl-toggle-debug)))

(ert-deftest agent-repl-test-set-log-file-level-sets-the-threshold ()
  "`agent-repl-set-log-file-level' sets the durable threshold."
  (cl-letf (((symbol-function 'message) #'ignore))
    (let ((agent-repl-log-file-level 'debug))
      (agent-repl-set-log-file-level 'warn)
      (should (eq agent-repl-log-file-level 'warn)))))

(ert-deftest agent-repl-test-set-log-file-level-rejects-an-unknown-level ()
  "An unknown level is refused rather than silently installed."
  (cl-letf (((symbol-function 'message) #'ignore))
    (let ((agent-repl-log-file-level 'debug))
      (should-error (agent-repl-set-log-file-level 'chatty) :type 'error)
      (should (eq agent-repl-log-file-level 'debug)))))

(ert-deftest agent-repl-test-set-log-file-level-rejects-verbose-as-a-level ()
  "Verbose is a record class, not a value in the shared level vocabulary."
  (cl-letf (((symbol-function 'message) #'ignore))
    ;; Arrange
    (let ((agent-repl-log-file-level 'info))
      ;; Act / Assert
      (should-error (agent-repl-set-log-file-level 'verbose) :type 'error)
      (should (eq agent-repl-log-file-level 'info)))))

(ert-deftest agent-repl-test-set-log-file-level-leaves-debug-visibility-alone ()
  "Changing durable volume does not change *Messages* visibility."
  (cl-letf (((symbol-function 'message) #'ignore))
    (let ((agent-repl-log-file-level 'debug)
          (agent-repl-debug nil))
      (agent-repl-set-log-file-level 'error)
      (should (eq agent-repl-debug nil)))))

(ert-deftest agent-repl-test-set-log-file-level-is-interactive ()
  "`agent-repl-set-log-file-level' is a user-facing command."
  (should (commandp 'agent-repl-set-log-file-level)))

(ert-deftest agent-repl-test-toggle-verbose-to-disk-turns-it-on ()
  "The verbose-to-disk toggle lowers the durable threshold to debug."
  (cl-letf (((symbol-function 'message) #'ignore))
    (let ((agent-repl-log-file-level 'info))
      (agent-repl-toggle-verbose-to-disk)
      (should (eq agent-repl-log-file-level 'debug)))))

(ert-deftest agent-repl-test-toggle-verbose-to-disk-turns-it-off ()
  "The verbose-to-disk toggle restores info from debug."
  (cl-letf (((symbol-function 'message) #'ignore))
    (let ((agent-repl-log-file-level 'debug))
      (agent-repl-toggle-verbose-to-disk)
      (should (eq agent-repl-log-file-level 'info)))))

(ert-deftest agent-repl-test-toggle-verbose-to-disk-from-a-quieter-rung-goes-debug ()
  "From a rung above debug the toggle still enables debug persistence."
  (cl-letf (((symbol-function 'message) #'ignore))
    (let ((agent-repl-log-file-level 'warn))
      (agent-repl-toggle-verbose-to-disk)
      (should (eq agent-repl-log-file-level 'debug)))))

(ert-deftest agent-repl-test-toggle-verbose-to-disk-leaves-the-buffer-level-alone ()
  "The verbose-to-disk toggle affects the FILE only, not the log buffers."
  (cl-letf (((symbol-function 'message) #'ignore))
    (let ((agent-repl-log-file-level 'debug)
          (agent-repl-log-buffer-level 'info))
      (agent-repl-toggle-verbose-to-disk)
      (should (eq agent-repl-log-buffer-level 'info)))))

(ert-deftest agent-repl-test-toggle-verbose-to-disk-is-interactive ()
  "`agent-repl-toggle-verbose-to-disk' is a user-facing command."
  (should (commandp 'agent-repl-toggle-verbose-to-disk)))

;;;; ---- Tests: state-dir / state-file ----

(ert-deftest agent-repl-test-state-dir-uses-env-override ()
  "state-dir honors the AGENT_REPL_STATE_DIR override when set."
  (let ((process-environment (cons "AGENT_REPL_STATE_DIR=/tmp/statetest" process-environment)))
    (should (equal (agent-repl--global-state-dir)
                   (file-name-as-directory (expand-file-name "/tmp/statetest"))))))

(ert-deftest agent-repl-test-state-dir-falls-back-to-claude-emacs ()
  "state-dir falls back to ~/.claude-emacs/ when the override is unset."
  ;; A bare \"AGENT_REPL_STATE_DIR\" entry (no =) makes getenv return nil.
  (let ((process-environment (cons "AGENT_REPL_STATE_DIR" process-environment)))
    (should (equal (agent-repl--global-state-dir)
                   (file-name-as-directory (expand-file-name "~/.claude-emacs"))))))

(ert-deftest agent-repl-test-state-file-joins-under-state-dir ()
  "state-file returns RELATIVE joined under state-dir."
  (let ((process-environment (cons "AGENT_REPL_STATE_DIR=/tmp/statetest" process-environment)))
    (should (equal (agent-repl--global-state-file "x.el")
                   (expand-file-name "/tmp/statetest/x.el")))))

(ert-deftest agent-repl-test-state-file-empty-relative-yields-state-dir ()
  "An empty RELATIVE yields the state dir itself (used by the flatten migration)."
  (let ((process-environment (cons "AGENT_REPL_STATE_DIR=/tmp/statetest" process-environment)))
    (should (equal (agent-repl--global-state-file "")
                   (expand-file-name "/tmp/statetest")))))

;;;; ---- Tests: legacy-state migration ----

(ert-deftest agent-repl-test-migrate-moves-legacy-when-new-absent ()
  "Migration moves a legacy dir to its new location when new is absent."
  (let* ((tmp (make-temp-file "migtest-" t))
         (legacy (expand-file-name "legacy" tmp))
         (process-environment (cons (format "AGENT_REPL_STATE_DIR=%s" (expand-file-name "state" tmp))
                                    process-environment))
         (agent-repl--legacy-state-migrations (list (cons legacy "emacs"))))
    (unwind-protect
        (progn
          (make-directory legacy t)
          (with-temp-file (expand-file-name "f.el" legacy) (insert "data"))
          (agent-repl--migrate-legacy-state)
          (should-not (file-exists-p legacy))
          (should (file-exists-p (agent-repl--global-state-file "emacs/f.el"))))
      (delete-directory tmp t))))

(ert-deftest agent-repl-test-migrate-skips-when-new-exists ()
  "Migration never overwrites an existing new-location path."
  (let* ((tmp (make-temp-file "migtest2-" t))
         (legacy (expand-file-name "legacy" tmp))
         (process-environment (cons (format "AGENT_REPL_STATE_DIR=%s" (expand-file-name "state" tmp))
                                    process-environment))
         (agent-repl--legacy-state-migrations (list (cons legacy "emacs"))))
    (unwind-protect
        (progn
          (make-directory legacy t)
          (with-temp-file (expand-file-name "old.el" legacy) (insert "old"))
          (make-directory (agent-repl--global-state-file "emacs") t)
          (with-temp-file (agent-repl--global-state-file "emacs/new.el") (insert "new"))
          (agent-repl--migrate-legacy-state)
          (should (file-exists-p (expand-file-name "old.el" legacy)))
          (should-not (file-exists-p (agent-repl--global-state-file "emacs/old.el")))
          (should (file-exists-p (agent-repl--global-state-file "emacs/new.el"))))
      (delete-directory tmp t))))

(ert-deftest agent-repl-test-migrate-noop-when-legacy-absent ()
  "Migration is a no-op when the legacy path does not exist."
  (let* ((tmp (make-temp-file "migtest3-" t))
         (legacy (expand-file-name "nonexistent" tmp))
         (process-environment (cons (format "AGENT_REPL_STATE_DIR=%s" (expand-file-name "state" tmp))
                                    process-environment))
         (agent-repl--legacy-state-migrations (list (cons legacy "emacs"))))
    (unwind-protect
        (progn
          (agent-repl--migrate-legacy-state)
          (should-not (file-exists-p (agent-repl--global-state-file "emacs"))))
      (delete-directory tmp t))))

;;;; ---- Tests: log-to-file ----

(ert-deftest agent-repl-test-default-log-directory-uses-os-temp-root-and-uid ()
  "The default log directory should be UID-qualified under the OS temp root."
  (let* ((tmpdir (make-temp-file "test-log-root-" t))
         (temporary-file-directory (file-name-as-directory tmpdir)))
    (unwind-protect
        (should
         (equal (agent-repl--default-log-directory)
                (file-name-as-directory
                 (expand-file-name (format "doom-agent-repl-%d" (user-uid))
                                   temporary-file-directory))))
      (delete-directory tmpdir t))))

(ert-deftest agent-repl-test-default-log-directory-rejects-invalid-temp-root ()
  "The default log resolver should fail loudly without an absolute temp root."
  (dolist (bad-root '(nil "" "relative/tmp"))
    (let ((temporary-file-directory bad-root))
      (should-error (agent-repl--default-log-directory) :type 'error))))

(ert-deftest agent-repl-test-normalize-log-file-name-redirects-retired-default ()
  "A reload should redirect the retired default without moving its file."
  (should
   (equal (agent-repl--normalize-log-file-name
           agent-repl--retired-state-log-file-name)
          agent-repl--default-log-file-name)))

(ert-deftest agent-repl-test-normalize-log-file-name-preserves-explicit-path ()
  "An explicitly different logfile path should remain untouched."
  (let ((custom "/tmp/agent-repl-explicit-test.log"))
    (should (equal (agent-repl--normalize-log-file-name custom) custom))))

(ert-deftest agent-repl-test-logfile-path-returns-private-temp-path ()
  "`agent-repl--logfile-path' should create the private default temp directory."
  (let* ((tmpdir (make-temp-file "test-log-root-" t))
         (temporary-file-directory (file-name-as-directory tmpdir))
         (expected-dir (agent-repl--default-log-directory))
         (agent-repl-log-file-name
          (expand-file-name "doom-agent-repl.log" expected-dir))
         (agent-repl--validated-private-log-directories
          (make-hash-table :test #'equal)))
    (unwind-protect
        (progn
          (should (equal (agent-repl--logfile-path)
                         (expand-file-name "doom-agent-repl.log" expected-dir)))
          (should (file-directory-p expected-dir))
          (should (= (logand (file-modes expected-dir) #o777) #o700))
          (should (gethash expected-dir
                           agent-repl--validated-private-log-directories)))
      (delete-directory tmpdir t))))

(ert-deftest agent-repl-test-logfile-path-revalidates-recreated-temp-directory ()
  "Deleting the private directory should evict and rebuild its validation."
  (let* ((tmpdir (make-temp-file "test-log-root-" t))
         (temporary-file-directory (file-name-as-directory tmpdir))
         (expected-dir (agent-repl--default-log-directory))
         (agent-repl-log-file-name
          (expand-file-name "doom-agent-repl.log" expected-dir))
         (agent-repl--validated-private-log-directories
          (make-hash-table :test #'equal)))
    (unwind-protect
        (progn
          (agent-repl--logfile-path)
          (delete-directory expected-dir)
          (should (gethash expected-dir
                           agent-repl--validated-private-log-directories))
          (agent-repl--logfile-path)
          (should (file-directory-p expected-dir))
          (should (= (logand (file-modes expected-dir) #o777) #o700))
          (should (gethash expected-dir
                           agent-repl--validated-private-log-directories)))
      (delete-directory tmpdir t))))

(ert-deftest agent-repl-test-logfile-path-rejects-symlinked-temp-directory ()
  "The private temporary log directory must not be a symlink."
  (let* ((tmpdir (make-temp-file "test-log-root-" t))
         (temporary-file-directory (file-name-as-directory tmpdir))
         (target (expand-file-name "target" tmpdir))
         (expected-dir (agent-repl--default-log-directory))
         (agent-repl-log-file-name
          (expand-file-name "doom-agent-repl.log" expected-dir))
         (agent-repl--validated-private-log-directories
          (make-hash-table :test #'equal)))
    (unwind-protect
        (progn
          (make-directory target)
          (make-symbolic-link target (directory-file-name expected-dir))
          (should-error (agent-repl--logfile-path) :type 'error))
      (delete-directory tmpdir t))))

(ert-deftest agent-repl-test-logfile-path-honors-defcustom ()
  "`agent-repl--logfile-path' should expand `agent-repl-log-file-name'."
  (let* ((tmpdir (make-temp-file "test-logpath-" t))
         (custom-path (expand-file-name "sub/custom.log" tmpdir))
         (agent-repl-log-file-name custom-path))
    (unwind-protect
        (should (equal (agent-repl--logfile-path) custom-path))
      (delete-directory tmpdir t))))

(ert-deftest agent-repl-test-do-log-to-file-writes-when-enabled ()
  "`agent-repl--do-log-to-file' should append text to the logfile."
  (let* ((tmpdir (make-temp-file "test-log-" t))
         (logpath (expand-file-name ".agent-repl.log" tmpdir))
         (agent-repl-log-to-file t))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-repl--logfile-path) (lambda () logpath)))
          (agent-repl--do-log-to-file "first line")
          (agent-repl--do-log-to-file "second line")
          (should (= (logand (file-modes logpath) #o777) #o600))
          (let ((contents (with-temp-buffer
                            (insert-file-contents logpath)
                            (buffer-string))))
            (should (string-match-p "first line" contents))
            (should (string-match-p "second line" contents))))
      (delete-directory tmpdir t))))

(ert-deftest agent-repl-test-do-log-to-file-skips-when-disabled ()
  "`agent-repl--do-log-to-file' should not write when `agent-repl-log-to-file' is nil."
  (let* ((tmpdir (make-temp-file "test-log-" t))
         (logpath (expand-file-name ".agent-repl.log" tmpdir))
         (agent-repl-log-to-file nil))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-repl--logfile-path) (lambda () logpath)))
          (agent-repl--do-log-to-file "should not appear")
          (should-not (file-exists-p logpath)))
      (delete-directory tmpdir t))))

(ert-deftest agent-repl-test-do-log-writes-jsonl-to-global-file ()
  "A nil-WS log record uses the global sink and is valid JSONL."
  (let* ((tmpdir (make-temp-file "test-log-" t))
         (logpath (expand-file-name ".agent-repl.log" tmpdir))
         (agent-repl-log-to-file t)
         (agent-repl-debug t))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-repl--logfile-path) (lambda () logpath))
                  ((symbol-function 'message) (lambda (&rest _args) nil)))
          (agent-repl--log nil "hello %s" "world")
          (let ((contents (with-temp-buffer
                            (insert-file-contents logpath)
                            (buffer-string))))
            (let ((record (json-parse-string contents :object-type 'alist)))
              (should (equal (alist-get 'runtime record) "emacs"))
              (should (equal (alist-get 'message record) "hello world"))
              (should (numberp (alist-get 'pid record)))
              (should (equal (alist-get 'verbosity record) "normal")))))
      (delete-directory tmpdir t))))

(ert-deftest agent-repl-test-log-operation-normalizes-format-template ()
  "Operation names are stable slugs derived from format templates alone."
  (should (equal (agent-repl--log-operation "Started request %s: %d")
                 "agent-repl.started-request-s-d"))
  (should (equal (agent-repl--log-operation "punctuation ///")
                 "agent-repl.punctuation"))
  (should (equal (agent-repl--log-operation "") "agent-repl.log")))

(ert-deftest agent-repl-test-do-log-to-file-signals-write-error ()
  "A sink failure is loud because persistence cannot silently degrade."
  (let ((agent-repl-log-to-file t))
    (cl-letf (((symbol-function 'agent-repl--logfile-path)
               (lambda () "/nonexistent/dir/impossible.log")))
      (should-error (agent-repl--do-log-to-file "test")))))

;;;; ---- Tests: JSONL workspace logging contract ----

(defun agent-repl-test--workspace-log-record (ws)
  "Return the LAST JSONL record written to WS's durable Emacs log sink."
  (let ((target (plist-get (agent-repl--workspace-log-target-entry ws) :target)))
    (should target)
    (with-temp-buffer
      (insert-file-contents target)
      (goto-char (point-max))
      (skip-chars-backward "\n")
      (json-parse-string
       (buffer-substring (line-beginning-position) (point))
       :object-type 'alist))))

(ert-deftest agent-repl-test-log-workspace-record-is-jsonl-and-uses-external-target ()
  "Workspace logging writes the complete Emacs JSONL schema through a symlink."
  (agent-repl-test--with-clean-state
    (let* ((project (make-temp-file "agent-repl-workspace-log-" t))
           (ws "json-ws")
           (agent-repl-log-to-file nil)
           (agent-repl--workspace-log-targets (make-hash-table :test #'equal)))
      (unwind-protect
          (progn
            (agent-repl--ws-put ws :project-dir project)
            (agent-repl--ws-put ws :ref (list :id "0123456789abcdef" :dir project))
            (let ((agent-repl-log-to-file t))
              (cl-letf (((symbol-function 'message) #'ignore))
                (agent-repl--log ws "started request %s" "r-1")))
            (let* ((canonical (expand-file-name ".claude/emacs/emacs.log" project))
                   (target (plist-get (agent-repl--workspace-log-target-entry ws) :target))
                   (record (with-temp-buffer
                             (insert-file-contents target)
                             (json-parse-string (buffer-string) :object-type 'alist))))
              (should (equal (file-symlink-p canonical) target))
              (should (string-prefix-p (file-truename temporary-file-directory)
                                       (file-truename target)))
              (dolist (field '("timestamp" "runtime" "pid" "level" "verbosity"
                               "operation" "message" "context" "workspace_dir" "workspace_id"))
                (should (assoc (intern field) record)))
              (should (equal (alist-get 'runtime record) "emacs"))
              (should (equal (alist-get 'message record) "started request r-1"))
              (should (equal (alist-get 'workspace_dir record)
                             (directory-file-name (file-truename project))))))
        (delete-directory project t)))))

(ert-deftest agent-repl-test-log-workspace-id-is-the-daemon-minted-id ()
  "After the roster push, `workspace_id' is the id the daemon minted."
  (agent-repl-test--with-clean-state
    (let* ((project (make-temp-file "agent-repl-wsid-daemon-" t))
           (ws "daemon-id-ws")
           (agent-repl-log-to-file nil)
           (agent-repl--workspace-log-targets (make-hash-table :test #'equal)))
      (unwind-protect
          (progn
            ;; Arrange: the roster push writes the row's ref, which carries
            ;; the daemon's 16-hex workspace id.
            (agent-repl--ws-put ws :project-dir project)
            (agent-repl--ws-put ws :ref (list :id "fedcba9876543210" :dir project))
            ;; Act.
            (let ((agent-repl-log-to-file t))
              (cl-letf (((symbol-function 'message) #'ignore))
                (agent-repl--log ws "elisp.test.daemon-id probe")))
            ;; Assert.
            (let ((record (agent-repl-test--workspace-log-record ws)))
              (should (equal (alist-get 'workspace_id record) "fedcba9876543210"))))
        (delete-directory project t)))))

(ert-deftest agent-repl-test-log-workspace-id-absent-before-the-roster-push ()
  "Before the roster push the id is unknown, so the field is omitted.
Stamping the directory hash in its place would split one workspace into two
groups in a harvest grouped by `workspace_id'."
  (agent-repl-test--with-clean-state
    (let* ((project (make-temp-file "agent-repl-wsid-preroster-" t))
           (ws "pre-roster-ws")
           (agent-repl-log-to-file nil)
           (agent-repl--workspace-log-targets (make-hash-table :test #'equal)))
      (unwind-protect
          (progn
            ;; Arrange: a registered workspace that no roster row has reached.
            (agent-repl--ws-put ws :project-dir project)
            ;; Act.
            (let ((agent-repl-log-to-file t))
              (cl-letf (((symbol-function 'message) #'ignore))
                (agent-repl--log ws "elisp.test.pre-roster probe")))
            ;; Assert.
            (let ((record (agent-repl-test--workspace-log-record ws)))
              (should-not (assoc 'workspace_id record))
              (should (equal (alist-get 'workspace_dir record)
                             (directory-file-name (file-truename project))))))
        (delete-directory project t)))))

(ert-deftest agent-repl-test-log-workspace-dir-hash-present-before-the-roster-push ()
  "The directory hash is derivable here, so it is stamped with no roster row."
  (agent-repl-test--with-clean-state
    (let* ((project (make-temp-file "agent-repl-dirhash-preroster-" t))
           (ws "pre-roster-hash-ws")
           (agent-repl-log-to-file nil)
           (agent-repl--workspace-log-targets (make-hash-table :test #'equal)))
      (unwind-protect
          (progn
            ;; Arrange.
            (agent-repl--ws-put ws :project-dir project)
            ;; Act.
            (let ((agent-repl-log-to-file t))
              (cl-letf (((symbol-function 'message) #'ignore))
                (agent-repl--log ws "elisp.test.dirhash-pre probe")))
            ;; Assert.
            (let ((record (agent-repl-test--workspace-log-record ws)))
              (should (equal (alist-get 'workspace_dir_hash
                                        (alist-get 'context record))
                             (substring (md5 (directory-file-name
                                              (file-truename project)))
                                        0 agent-repl-workspace-dir-hash-length)))))
        (delete-directory project t)))))

(ert-deftest agent-repl-test-log-workspace-dir-hash-present-after-the-roster-push ()
  "The directory hash stays on the record once the daemon id is also known."
  (agent-repl-test--with-clean-state
    (let* ((project (make-temp-file "agent-repl-dirhash-postroster-" t))
           (ws "post-roster-hash-ws")
           (agent-repl-log-to-file nil)
           (agent-repl--workspace-log-targets (make-hash-table :test #'equal)))
      (unwind-protect
          (progn
            ;; Arrange.
            (agent-repl--ws-put ws :project-dir project)
            (agent-repl--ws-put ws :ref (list :id "00112233445566aa" :dir project))
            ;; Act.
            (let ((agent-repl-log-to-file t))
              (cl-letf (((symbol-function 'message) #'ignore))
                (agent-repl--log ws "elisp.test.dirhash-post probe")))
            ;; Assert.
            (let ((record (agent-repl-test--workspace-log-record ws)))
              (should (equal (alist-get 'workspace_dir_hash
                                        (alist-get 'context record))
                             (substring (md5 (directory-file-name
                                              (file-truename project)))
                                        0 agent-repl-workspace-dir-hash-length)))))
        (delete-directory project t)))))

(ert-deftest agent-repl-test-log-sink-survives-the-roster-push ()
  "The roster push does not rebind the sink: one directory keeps one target.
The durable target is keyed by the DIRECTORY HASH precisely so a workspace's
pre-registration records and the rest of its records land in one file."
  (agent-repl-test--with-clean-state
    (let* ((project (make-temp-file "agent-repl-sink-stable-" t))
           (ws "sink-stable-ws")
           (agent-repl-log-to-file nil)
           (agent-repl--workspace-log-targets (make-hash-table :test #'equal)))
      (unwind-protect
          (progn
            ;; Arrange: one record before the roster push.
            (agent-repl--ws-put ws :project-dir project)
            (let ((agent-repl-log-to-file t))
              (cl-letf (((symbol-function 'message) #'ignore))
                (agent-repl--log ws "elisp.test.sink-before probe")))
            (let ((before (plist-get (agent-repl--workspace-log-target-entry ws)
                                     :target)))
              ;; Act: the roster push arrives and a second record is written.
              (agent-repl--ws-put ws :ref (list :id "aabbccddeeff0011" :dir project))
              (let ((agent-repl-log-to-file t))
                (cl-letf (((symbol-function 'message) #'ignore))
                  (agent-repl--log ws "elisp.test.sink-after probe")))
              ;; Assert.
              (should (equal (plist-get (agent-repl--workspace-log-target-entry ws)
                                        :target)
                             before))
              (with-temp-buffer
                (insert-file-contents before)
                (should (string-match-p "elisp.test.sink-before probe" (buffer-string)))
                (should (string-match-p "elisp.test.sink-after probe" (buffer-string))))))
        (delete-directory project t)))))

(ert-deftest agent-repl-test-log-workspace-record-attributes-known-sessions ()
  "Workspace JSONL records expose both known session identifiers."
  (agent-repl-test--with-clean-state
    (let* ((project (make-temp-file "agent-repl-identity-log-" t))
           (ws "identity-ws")
           (agent-repl-log-to-file nil)
           (agent-repl--workspace-log-targets (make-hash-table :test #'equal)))
      (unwind-protect
          (progn
            (agent-repl--ws-put ws :project-dir project)
            (let ((agent-repl-log-to-file t))
              (cl-letf (((symbol-function 'message) #'ignore)
                        ((symbol-function 'agent-repl-host-session-id)
                         (lambda (_) "agent-session-1"))
                        ((symbol-function 'agent-repl-host--live)
                         (lambda (_) '(:vendor-info (:arm :claude :value (:session-id "claude-session-1"))))))
                (agent-repl--log ws "identity test")))
            (let* ((target (plist-get (agent-repl--workspace-log-target-entry ws) :target))
                   (record (with-temp-buffer
                             (insert-file-contents target)
                             (json-parse-string (buffer-string) :object-type 'alist))))
              (should (equal (alist-get 'agent_repl_session_id record)
                             "agent-session-1"))
              (should (equal (alist-get 'claude_session_id record) "claude-session-1"))))
        (delete-directory project t)))))

(ert-deftest agent-repl-test-log-record-carries-the-edge-request-id ()
  "A request edge's dynamic identity is serialized into its nested records."
  (agent-repl-test--with-temp-logfile path
    ;; Arrange
    (cl-letf (((symbol-function 'message) #'ignore))
      ;; Act
      (agent-repl--with-log-context
       agent-repl--global-log-scope "request-1"
       (lambda () (agent-repl--info nil "elisp.roster.push: correlated")))
      ;; Assert
      (let ((record (json-parse-string (agent-repl-test--read-file path)
                                       :object-type 'alist)))
        (should (equal (alist-get 'request_id record) "request-1"))))))

(ert-deftest agent-repl-test-log-session-identity-does-not-reenter-persistence ()
  "Resolving a session identity while logging produces exactly one record."
  (agent-repl-test--with-clean-state
    (let* ((project (make-temp-file "agent-repl-nonrecursive-log-" t))
           (ws "nonrecursive-ws")
           (agent-repl-log-to-file nil)
           (agent-repl--workspace-log-targets (make-hash-table :test #'equal))
           (original-builder (symbol-function 'agent-repl--log-record))
           (builder-calls 0))
      (unwind-protect
          (progn
            (agent-repl--ws-put ws :project-dir project)
            (agent-repl--ws-put ws :active-env :bare-metal)
            (agent-repl--ws-put ws :bare-metal
                                (make-agent-repl-instantiation :session-id "claude-session-1"))
            (let ((agent-repl-log-to-file t))
              (cl-letf (((symbol-function 'agent-repl--log-record)
                         (lambda (&rest args)
                           (cl-incf builder-calls)
                           (apply original-builder args))))
                (agent-repl--log ws "one record")
                (should (= builder-calls 1)))))
        (delete-directory project t)))))

(ert-deftest agent-repl-test-log-workspace-record-omits-missing-session-identities ()
  "An absent durable conversation id is not serialized as a null field."
  (agent-repl-test--with-clean-state
    (let* ((project (make-temp-file "agent-repl-missing-identity-" t))
           (ws "missing-identity-ws")
           (agent-repl-log-to-file nil)
           (agent-repl--workspace-log-targets (make-hash-table :test #'equal)))
      (unwind-protect
          (progn
            (agent-repl--ws-put ws :project-dir project)
            (let ((agent-repl-log-to-file t))
              (agent-repl--log ws "missing identities"))
            (let* ((target (plist-get (agent-repl--workspace-log-target-entry ws) :target))
                   (record (with-temp-buffer
                             (insert-file-contents target)
                             (json-parse-string (buffer-string) :object-type 'alist))))
              (should-not (assoc 'claude_session_id record))
              (should-not (assoc 'agent_repl_session_id record))))
        (delete-directory project t)))))

(ert-deftest agent-repl-test-log-workspace-invalid-session-identity-fails-before-write ()
  "Malformed workspace identity aborts logging without creating a target."
  (agent-repl-test--with-clean-state
    (let* ((project (make-temp-file "agent-repl-invalid-identity-" t))
           (ws "invalid-identity-ws")
           (agent-repl-log-to-file nil)
           (agent-repl--workspace-log-targets (make-hash-table :test #'equal)))
      (unwind-protect
          (progn
            (agent-repl--ws-put ws :project-dir project)
            (let ((agent-repl-log-to-file t))
              (cl-letf (((symbol-function 'agent-repl-host--live)
                         (lambda (_) '(:vendor-info (:arm :claude :value (:session-id 99))))))
                (should-error (agent-repl--log ws "invalid claude identity"))))
            (should-not (agent-repl--workspace-log-target-entry ws)))
        (delete-directory project t)))))

(ert-deftest agent-repl-test-log-verbose-persists-with-terminal-visibility-disabled ()
  "Verbose records persist even when verbose terminal visibility is disabled."
  (let* ((dir (make-temp-file "agent-repl-verbose-log-" t))
         (path (expand-file-name "global.log" dir))
         (agent-repl-log-to-file t)
         (agent-repl-log-file-name path)
         (agent-repl-debug nil)
         ;; Opened so the assertion is about verbosity persisting independently
         ;; of `agent-repl-debug', not about the durable threshold's gate.
         (agent-repl-log-file-level 'debug)
         (message-called nil))
    (unwind-protect
        (cl-letf (((symbol-function 'message) (lambda (&rest _) (setq message-called t))))
          (agent-repl--log-verbose nil "timer tick")
          (let ((record (with-temp-buffer
                          (insert-file-contents path)
                          (json-parse-string (buffer-string) :object-type 'alist))))
            (should-not message-called)
            (should (equal (alist-get 'verbosity record) "verbose"))
            (should (equal (alist-get 'message record) "timer tick"))))
      (delete-directory dir t))))

(ert-deftest agent-repl-test-log-workspace-without-directory-records-a-central-fallback ()
  "A workspace that owns no sink announces the fallback, not a routing error."
  (agent-repl-test--with-clean-state
    (let* ((dir (make-temp-file "agent-repl-central-fallback-" t))
           (global (expand-file-name "global.log" dir))
           (agent-repl-log-to-file t)
           (agent-repl-log-file-name global)
           (agent-repl-log-file-level 'debug)
           (agent-repl--unroutable-log-workspaces (make-hash-table :test #'equal)))
      (unwind-protect
          (cl-letf (((symbol-function 'display-warning) #'ignore))
            (agent-repl--log "missing-ws" "routed centrally")
            (should (file-exists-p global))
            (with-temp-buffer
              (insert-file-contents global)
              (should (string-match-p "log-central-fallback" (buffer-string)))))
        (delete-directory dir t)))))

(ert-deftest agent-repl-test-log-workspace-without-directory-records-no-routing-error ()
  "An unavailable directory is an ordinary outcome, never an error flood."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-logfile path
      (let ((agent-repl--unroutable-log-workspaces (make-hash-table :test #'equal)))
        ;; Act
        (cl-letf (((symbol-function 'display-warning) #'ignore))
          (dotimes (_ 5)
            (agent-repl--log "missing-ws" "routed centrally")))
        ;; Assert
        (with-temp-buffer
          (insert-file-contents path)
          (should-not (string-match-p "log-routing-error" (buffer-string))))))))

(ert-deftest agent-repl-test-log-workspace-without-directory-announces-the-fallback-once ()
  "The condition belongs to the workspace, so it is announced once."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-logfile path
      (let ((agent-repl--unroutable-log-workspaces (make-hash-table :test #'equal)))
        ;; Act
        (cl-letf (((symbol-function 'display-warning) #'ignore))
          (dotimes (_ 5)
            (agent-repl--log "missing-ws" "routed centrally")))
        ;; Assert
        (should (= 1 (seq-count
                      (lambda (record)
                        (string-match-p "log-central-fallback"
                                        (or (alist-get 'operation record) "")))
                      (agent-repl-test--log-records path))))))))


(ert-deftest agent-repl-test-workspace-log-replaces-hostile-canonical-symlink ()
  "A workspace-provided symlink is replaced without writing its target."
  (agent-repl-test--with-clean-state
    (let* ((project (make-temp-file "agent-repl-hostile-link-" t))
           (foreign (make-temp-file "agent-repl-foreign-log-"))
           (canonical (expand-file-name ".claude/emacs/emacs.log" project))
           (ws "hostile-link")
           (agent-repl-log-to-file nil)
           (agent-repl--workspace-log-targets (make-hash-table :test #'equal)))
      (unwind-protect
          (progn
            (with-temp-file foreign (insert "foreign content\n"))
            (make-directory (file-name-directory canonical) t)
            (make-symbolic-link foreign canonical)
            (agent-repl--ws-put ws :project-dir project)
            (let ((agent-repl-log-to-file t))
              (cl-letf (((symbol-function 'message) #'ignore))
                (agent-repl--log ws "safe write")))
            (should (equal (with-temp-buffer (insert-file-contents foreign) (buffer-string))
                           "foreign content\n"))
            (should-not (equal (file-symlink-p canonical) foreign)))
        (delete-file foreign)
        (delete-directory project t)))))

(ert-deftest agent-repl-test-workspace-log-replaces-hostile-regular-file ()
  "A workspace-provided regular file is replaced rather than opened as a sink."
  (agent-repl-test--with-clean-state
    (let* ((project (make-temp-file "agent-repl-hostile-file-" t))
           (canonical (expand-file-name ".claude/emacs/emacs.log" project))
           (ws "hostile-file")
           (agent-repl-log-to-file nil)
           (agent-repl--workspace-log-targets (make-hash-table :test #'equal)))
      (unwind-protect
          (progn
            (make-directory (file-name-directory canonical) t)
            (with-temp-file canonical (insert "hostile content\n"))
            (agent-repl--ws-put ws :project-dir project)
            (let ((agent-repl-log-to-file t))
              (cl-letf (((symbol-function 'message) #'ignore))
                (agent-repl--log ws "safe write")))
            (should (file-symlink-p canonical)))
        (delete-directory project t)))))

(ert-deftest agent-repl-test-workspace-log-rejects-symlinked-claude-directory ()
  "A `.claude' symlink aborts before any external directory is modified."
  (agent-repl-test--with-clean-state
    (let* ((project (make-temp-file "agent-repl-hostile-claude-" t))
           (external (make-temp-file "agent-repl-external-claude-" t))
           (ws "hostile-claude")
           (agent-repl-log-to-file nil)
           (agent-repl--workspace-log-targets (make-hash-table :test #'equal)))
      (unwind-protect
          (progn
            (make-symbolic-link external (expand-file-name ".claude" project))
            (agent-repl--ws-put ws :project-dir project)
            (let ((agent-repl-log-to-file t))
              (should-error (agent-repl--log ws "must fail")))
            (should-not (file-exists-p (expand-file-name "emacs/emacs.log" external)))
            (should-not (agent-repl--workspace-log-target-entry ws)))
        (delete-directory project t)
        (delete-directory external t)))))

(ert-deftest agent-repl-test-workspace-log-rejects-symlinked-emacs-directory ()
  "A `.claude/emacs' symlink aborts before any external directory is modified."
  (agent-repl-test--with-clean-state
    (let* ((project (make-temp-file "agent-repl-hostile-emacs-" t))
           (external (make-temp-file "agent-repl-external-emacs-" t))
           (claude (expand-file-name ".claude" project))
           (ws "hostile-emacs")
           (agent-repl-log-to-file nil)
           (agent-repl--workspace-log-targets (make-hash-table :test #'equal)))
      (unwind-protect
          (progn
            (make-directory claude)
            (make-symbolic-link external (expand-file-name "emacs" claude))
            (agent-repl--ws-put ws :project-dir project)
            (let ((agent-repl-log-to-file t))
              (should-error (agent-repl--log ws "must fail")))
            (should-not (file-exists-p (expand-file-name "emacs.log" external)))
            (should-not (agent-repl--workspace-log-target-entry ws)))
        (delete-directory project t)
        (delete-directory external t)))))

(ert-deftest agent-repl-test-workspace-log-unsafe-parent-creates-no-target ()
  "Unsafe workspace components fail before creating an external target file."
  (agent-repl-test--with-clean-state
    (let* ((sandbox (make-temp-file "agent-repl-unsafe-target-sandbox-" t))
           ;; This assertion inventories every matching target in the temp
           ;; directory, so isolate it from concurrent test-all processes.
           (temporary-file-directory (file-name-as-directory sandbox))
           (project (make-temp-file "agent-repl-unsafe-target-" t))
           (external (make-temp-file "agent-repl-unsafe-external-" t))
           (ws "unsafe-no-target")
           (before (directory-files temporary-file-directory t "\\`agent-repl-emacs-"))
           (agent-repl-log-to-file nil)
           (agent-repl--workspace-log-targets (make-hash-table :test #'equal)))
      (unwind-protect
          (progn
            (make-symbolic-link external (expand-file-name ".claude" project))
            (agent-repl--ws-put ws :project-dir project)
            (let ((agent-repl-log-to-file t))
              (should-error (agent-repl--log ws "must fail")))
            (should (equal before (directory-files temporary-file-directory t "\\`agent-repl-emacs-")))
            (should-not (file-exists-p (expand-file-name "emacs/emacs.log" external))))
        (delete-directory project t)
        (delete-directory external t)
        (delete-directory sandbox t)))))

(ert-deftest agent-repl-test-workspace-log-rebinds-to-a-fresh-target ()
  "Rebinding a WS forgets in-memory ownership and creates a fresh target."
  (agent-repl-test--with-clean-state
    (let* ((first (make-temp-file "agent-repl-first-project-" t))
           (second (make-temp-file "agent-repl-second-project-" t))
           (ws "rebound-ws")
           (agent-repl-log-to-file nil)
           (agent-repl--workspace-log-targets (make-hash-table :test #'equal)))
      (unwind-protect
          (progn
            (agent-repl--ws-put ws :project-dir first)
            (let ((agent-repl-log-to-file t))
              (agent-repl--log ws "first"))
            (let ((target (plist-get (agent-repl--workspace-log-target-entry ws) :target)))
              (agent-repl--ws-put ws :project-dir second)
              (should-not (agent-repl--workspace-log-target-entry ws))
              (let ((agent-repl-log-to-file t))
                (agent-repl--log ws "second"))
              (let ((rebound (plist-get (agent-repl--workspace-log-target-entry ws) :target)))
                (should-not (equal target rebound))
                (should (file-symlink-p (expand-file-name ".claude/emacs/emacs.log" second))))))
        (delete-directory first t)
        (delete-directory second t)))))

(ert-deftest agent-repl-test-workspace-log-cleans-reserved-staging-path-on-link-failure ()
  "A failed staging symlink leaves neither a cache entry nor a staging file."
  (agent-repl-test--with-clean-state
    (let* ((project (make-temp-file "agent-repl-staging-failure-" t))
           (ws "staging-failure")
           (agent-repl-log-to-file nil)
           (agent-repl--workspace-log-targets (make-hash-table :test #'equal)))
      (unwind-protect
          (progn
            (agent-repl--ws-put ws :project-dir project)
            (cl-letf (((symbol-function 'make-symbolic-link)
                       (lambda (&rest _) (error "simulated staging failure"))))
              (let ((agent-repl-log-to-file t))
                (should-error (agent-repl--log ws "must fail"))))
            (should-not (agent-repl--workspace-log-target-entry ws))
            (should-not (directory-files-recursively project "\\.emacs\\.log-link-")))
        (delete-directory project t)))))

(ert-deftest agent-repl-test-workspace-log-rename-failure-cleans-target-and-staging ()
  "A failed atomic install leaves no target or staging artifact behind."
  (agent-repl-test--with-clean-state
    (let* ((sandbox (make-temp-file "agent-repl-rename-sandbox-" t))
           (temporary-file-directory (file-name-as-directory sandbox))
           (project (make-temp-file "agent-repl-rename-failure-" t))
           (ws "rename-failure")
           (before (directory-files temporary-file-directory t "\\`agent-repl-emacs-"))
           (agent-repl-log-to-file nil)
           (agent-repl--workspace-log-targets (make-hash-table :test #'equal)))
      (unwind-protect
          (progn
            (agent-repl--ws-put ws :project-dir project)
            (cl-letf (((symbol-function 'rename-file)
                       (lambda (&rest _) (error "simulated rename failure"))))
              (let ((agent-repl-log-to-file t))
                (should-error (agent-repl--log ws "must fail"))))
            (should-not (agent-repl--workspace-log-target-entry ws))
            (should (equal before (directory-files temporary-file-directory t "\\`agent-repl-emacs-")))
            (should-not (directory-files-recursively project "\\.emacs\\.log-link-"))
            (should-not (file-exists-p (expand-file-name ".claude/emacs/emacs.log" project))))
        (delete-directory sandbox t)))))

(ert-deftest agent-repl-test-log-rotation-failure-is-visible-and-fatal ()
  "A generation-rotation failure emits the sink emergency and signals."
  ;; Arrange
  (let ((emergency nil))
    (agent-repl-test--with-temp-logfile path
      (let ((agent-repl-log-size-cap-bytes 1))
        (write-region "x" nil path)
        (cl-letf (((symbol-function 'agent-repl--log-rotate-generations)
                   (lambda (&rest _) (error "simulated rotation failure")))
                  ((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (setq emergency (apply #'format fmt args)))))
          ;; Act / Assert
          (should-error (agent-repl--do-log-to-file "record"))
          (should (string-match-p "LOG SINK FAILURE" emergency)))))))

;;;; ---- Tests: dir-has-git-p ----

(ert-deftest agent-repl-test-dir-has-git-p-with-git-dir ()
  "dir-has-git-p should return non-nil for directory with .git subdirectory."
  (let ((tmpdir (make-temp-file "test-git-" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" tmpdir) t)
          (should (agent-repl--dir-has-git-p tmpdir)))
      (delete-directory tmpdir t))))

(ert-deftest agent-repl-test-dir-has-git-p-with-git-file ()
  "dir-has-git-p should return non-nil for directory with .git file (worktree)."
  (let ((tmpdir (make-temp-file "test-git-" t)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name ".git" tmpdir)
            (insert "gitdir: /some/other/path"))
          (should (agent-repl--dir-has-git-p tmpdir)))
      (delete-directory tmpdir t))))

(ert-deftest agent-repl-test-dir-has-git-p-no-git ()
  "dir-has-git-p should return nil for directory without .git."
  (let ((tmpdir (make-temp-file "test-git-" t)))
    (unwind-protect
        (should-not (agent-repl--dir-has-git-p tmpdir))
      (delete-directory tmpdir t))))

(ert-deftest agent-repl-test-dir-has-git-p-nonexistent ()
  "dir-has-git-p should return nil for non-existent directory."
  (should-not (agent-repl--dir-has-git-p "/nonexistent/path/does/not/exist")))

(ert-deftest agent-repl-test-dir-has-git-p-nil ()
  "dir-has-git-p with nil should not error (expand-file-name handles nil)."
  ;; nil becomes default-directory; verify it doesn't crash.
  ;; No ignore-errors: if it signals, ERT correctly fails the test.
  (should (or (agent-repl--dir-has-git-p nil) t)))

(ert-deftest agent-repl-test-dir-has-git-p-empty-string ()
  "dir-has-git-p with empty string should not error."
  ;; No ignore-errors: if it signals, ERT correctly fails the test.
  (should (or (agent-repl--dir-has-git-p "") t)))

;;;; ---- Tests: git-string / git-string-quiet ----
;;
;; Intentionally none for the wrappers themselves.
;; `agent-repl--git-string' and `agent-repl--git-string-quiet' are
;; the external-boundary wrappers described by AGENTS.md "No External
;; Processes or External State in Tests".  Per that policy they are
;; not tested in isolation — testing them would require either
;; invoking real git (forbidden) or stubbing
;; `agent-repl--capture-process-output' (tautological).  Their
;; behavior is covered indirectly by the many callers that stub these
;; wrappers via `cl-letf'.
;;
;; The shared implementation, `agent-repl--capture-process-output',
;; IS tested below — it carries the worker-thread safety contract
;; (routes through `agent-repl--wait-for-process-exit' instead of
;; `shell-command-to-string') and the silent-on-timeout contract that
;; the quiet variants rely on, both of which are worth pinning.

;;;; ---- Tests: capture-process-output ----

(ert-deftest agent-repl-test-capture-process-output-returns-trimmed-buffer ()
  "On clean exit, `--capture-process-output' returns the stdout buffer's
contents with leading/trailing whitespace trimmed."
  (let ((captured-buf nil))
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest args)
                 (let ((buf (plist-get args :buffer)))
                   (setq captured-buf buf)
                   (with-current-buffer buf
                     (insert "  trimmed result  \n"))
                   (list :fake-proc buf))))
              ((symbol-function 'set-process-query-on-exit-flag)
               (lambda (&rest _) nil))
              ((symbol-function 'set-process-sentinel)
               (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--wait-for-process-exit)
               (lambda (&rest _) 0)))
      (should (equal (agent-repl--capture-process-output
                      "git" '("rev-parse" "HEAD"))
                     "trimmed result"))
      (should-not (buffer-live-p captured-buf)))))

(ert-deftest agent-repl-test-capture-process-output-returns-empty-on-timeout ()
  "On timeout, `--capture-process-output' returns an empty string.
This is load-bearing for the silent-failure contract of the `--quiet'
wrappers: init-time callers that may run outside a git repository
must not explode when git hangs or doesn't terminate in time."
  (cl-letf (((symbol-function 'make-process)
             (lambda (&rest args)
               (let ((buf (plist-get args :buffer)))
                 (with-current-buffer buf
                   (insert "partial output that should not be returned\n"))
                 (list :fake-proc buf))))
            ((symbol-function 'set-process-query-on-exit-flag)
             (lambda (&rest _) nil))
            ((symbol-function 'set-process-sentinel)
             (lambda (&rest _) nil))
            ((symbol-function 'agent-repl--wait-for-process-exit)
             (lambda (&rest _) 'timeout)))
    (should (equal (agent-repl--capture-process-output
                    "git" '("rev-parse" "HEAD"))
                   ""))))

(ert-deftest agent-repl-test-capture-process-output-logs-on-timeout ()
  "On timeout, `--capture-process-output' emits a log line naming the
stalled program and args so the otherwise-silent \"\" return leaves a
post-mortem breadcrumb."
  (let ((logged nil))
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest args) (list :fake-proc (plist-get args :buffer))))
              ((symbol-function 'set-process-query-on-exit-flag)
               (lambda (&rest _) nil))
              ((symbol-function 'set-process-sentinel)
               (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--wait-for-process-exit)
               (lambda (&rest _) 'timeout))
              ((symbol-function 'agent-repl--log)
               (lambda (_ws fmt &rest args)
                 (setq logged (apply #'format fmt args)))))
      (agent-repl--capture-process-output "git" '("fetch" "origin"))
      (should (string-match-p "TIMEOUT" logged))
      (should (string-match-p "git" logged))
      (should (string-match-p "fetch" logged)))))

;;;; ---- Tests: async-gh terminal outcomes ----

(ert-deftest agent-repl-test-async-gh-calls-callback-after-abnormal-exit ()
  "An abnormal gh exit is a completed command, not a silently dropped callback."
  (let ((buf nil)
        (callback-result nil)
        (agent-repl-log-to-file nil))
    (unwind-protect
        (progn
          (setq buf (generate-new-buffer " *agent-repl-test-async-gh*"))
          (cl-letf (((symbol-function 'process-live-p) (lambda (_proc) nil))
                  ((symbol-function 'process-buffer) (lambda (_proc) buf))
                  ((symbol-function 'process-exit-status) (lambda (_proc) 1))
                  ((symbol-function 'agent-repl--kill-buffer-safely)
                   (lambda (buffer) (kill-buffer buffer))))
            (with-current-buffer buf (insert "network unavailable"))
            (agent-repl--async-gh-handle-completion
             "failed-pr-poll"
             (lambda (ok output) (setq callback-result (list ok output)))
             'fake-gh-process "exited abnormally with code 1\n"))
          (should (equal callback-result '(nil "network unavailable"))))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

(ert-deftest agent-repl-test-capture-process-output-uses-make-process-when-suppress-stderr ()
  "When SUPPRESS-STDERR is non-nil, `--capture-process-output' uses
`make-process' with `:stderr' set to a separate buffer so stderr is
discarded — matches the `2>/dev/null' contract the quiet wrappers
depend on."
  (let ((stderr-buf-arg :unset))
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest args)
                 (setq stderr-buf-arg (plist-get args :stderr))
                 (list :fake-proc (plist-get args :buffer))))
              ((symbol-function 'set-process-query-on-exit-flag)
               (lambda (&rest _) nil))
              ((symbol-function 'set-process-sentinel)
               (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--wait-for-process-exit)
               (lambda (&rest _) 0)))
      (agent-repl--capture-process-output "git" '("status") t))
    (should (bufferp stderr-buf-arg))))

(ert-deftest agent-repl-test-capture-process-output-merges-stderr-when-no-suppress ()
  "When SUPPRESS-STDERR is nil (default), no separate `:stderr' buffer is
passed, so stderr merges into the stdout buffer (matches
`shell-command-to-string''s default and the existing `--git-string'
contract that includes stderr in the returned text)."
  (let ((stderr-buf-arg :unset))
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest args)
                 (setq stderr-buf-arg (plist-get args :stderr))
                 (list :fake-proc (plist-get args :buffer))))
              ((symbol-function 'set-process-query-on-exit-flag)
               (lambda (&rest _) nil))
              ((symbol-function 'set-process-sentinel)
               (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--wait-for-process-exit)
               (lambda (&rest _) 0)))
      (agent-repl--capture-process-output "git" '("status") nil))
    (should-not stderr-buf-arg)))

(ert-deftest agent-repl-test-capture-process-output-spawns-on-pipe-when-no-suppress ()
  "The merged-stderr branch spawns with `:connection-type' `pipe'.
A pty spawn makes `git log' see a terminal and launch its pager, which
hangs until the timeout on every call — the 2026-07-18 freeze trigger."
  (let ((conn-type :unset))
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest args)
                 (setq conn-type (plist-get args :connection-type))
                 (list :fake-proc (plist-get args :buffer))))
              ((symbol-function 'set-process-query-on-exit-flag)
               (lambda (&rest _) nil))
              ((symbol-function 'set-process-sentinel)
               (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--wait-for-process-exit)
               (lambda (&rest _) 0)))
      (agent-repl--capture-process-output "git" '("log" "-1") nil))
    (should (eq conn-type 'pipe))))

(ert-deftest agent-repl-test-capture-process-output-spawns-on-pipe-when-suppress ()
  "The suppress-stderr branch spawns with `:connection-type' `pipe' too."
  (let ((conn-type :unset))
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest args)
                 (setq conn-type (plist-get args :connection-type))
                 (list :fake-proc (plist-get args :buffer))))
              ((symbol-function 'set-process-query-on-exit-flag)
               (lambda (&rest _) nil))
              ((symbol-function 'set-process-sentinel)
               (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--wait-for-process-exit)
               (lambda (&rest _) 0)))
      (agent-repl--capture-process-output "git" '("log" "-1") t))
    (should (eq conn-type 'pipe))))

(ert-deftest agent-repl-test-capture-process-output-cleanup-uses-safe-buffer-kill ()
  "Buffer cleanup routes through `agent-repl--kill-buffer-safely' when it
is bound: on the timeout path the child can still be alive, and a bare
`kill-buffer' on its buffer from the merge worker is the AppKit
off-main teardown deadlock (2026-07-18)."
  (let ((safe-killed nil))
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest args)
                 (list :fake-proc (plist-get args :buffer))))
              ((symbol-function 'set-process-query-on-exit-flag)
               (lambda (&rest _) nil))
              ((symbol-function 'set-process-sentinel)
               (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--wait-for-process-exit)
               (lambda (&rest _) 'timeout))
              ((symbol-function 'agent-repl--kill-buffer-safely)
               (lambda (buf) (push (buffer-name buf) safe-killed)
                 (kill-buffer buf) t)))
      (agent-repl--capture-process-output "git" '("log" "-1") nil)
      (should (= 1 (length safe-killed))))))

(ert-deftest agent-repl-test-capture-process-output-kills-stderr-buffer ()
  "The temporary stderr buffer (when SUPPRESS-STDERR is non-nil) is killed
before return so we don't accumulate hidden buffers across many git calls."
  (let ((stderr-buf-arg nil))
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest args)
                 (setq stderr-buf-arg (plist-get args :stderr))
                 (list :fake-proc (plist-get args :buffer))))
              ((symbol-function 'set-process-query-on-exit-flag)
               (lambda (&rest _) nil))
              ((symbol-function 'set-process-sentinel)
               (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--wait-for-process-exit)
               (lambda (&rest _) 0)))
      (agent-repl--capture-process-output "git" '("status") t))
    (should stderr-buf-arg)
    (should-not (buffer-live-p stderr-buf-arg))))

(ert-deftest agent-repl-test-capture-process-output-installs-noop-sentinel ()
  "`--capture-process-output' installs `#'ignore' as the capture process's
sentinel.  This displaces Emacs's `internal-default-process-sentinel',
whose `Process NAME finished' status insertion into the shared buffer
would otherwise be read back as command output and corrupt the result."
  (let ((sentinel-arg 'unset))
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest args)
                 (let ((buf (plist-get args :buffer)))
                   (with-current-buffer buf (insert "clean-output\n"))
                   (list :fake-proc buf))))
              ((symbol-function 'set-process-query-on-exit-flag)
               (lambda (&rest _) nil))
              ((symbol-function 'set-process-sentinel)
               (lambda (_proc sentinel) (setq sentinel-arg sentinel)))
              ((symbol-function 'agent-repl--wait-for-process-exit)
               (lambda (&rest _) 0)))
      (should (equal (agent-repl--capture-process-output
                      "git" '("rev-parse" "HEAD"))
                     "clean-output"))
      (should (eq sentinel-arg #'ignore)))))

(ert-deftest agent-repl-test-capture-process-output-installs-sentinel-before-waiting ()
  "The no-op sentinel is installed BEFORE the wait, not after.  Installing
it after `--wait-for-process-exit' would be useless on the main-thread
path, where the default sentinel fires during the wait's
`accept-process-output' loop."
  (let ((waited nil)
        (installed-before-wait nil))
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest args)
                 (let ((buf (plist-get args :buffer)))
                   (with-current-buffer buf (insert "out\n"))
                   (list :fake-proc buf))))
              ((symbol-function 'set-process-query-on-exit-flag)
               (lambda (&rest _) nil))
              ((symbol-function 'set-process-sentinel)
               (lambda (&rest _) (setq installed-before-wait (not waited))))
              ((symbol-function 'agent-repl--wait-for-process-exit)
               (lambda (&rest _) (setq waited t) 0)))
      (agent-repl--capture-process-output "git" '("rev-parse" "HEAD"))
      (should installed-before-wait))))

;;;; ---- Tests: print-git-branch ----

(ert-deftest agent-repl-test-print-git-branch-message ()
  "print-git-branch should include the git branch value in its message."
  ;; Reset the lazy cache so the cl-letf'd boundary wrapper actually fires
  ;; (otherwise a populated `agent-repl-git-branch' from a prior session
  ;; or earlier test short-circuits the only call this test exercises).
  (let ((agent-repl-git-branch nil)
        (captured-msg nil))
    (cl-letf (((symbol-function 'agent-repl--git-string-quiet)
               (lambda (&rest _args) "test-branch"))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (setq captured-msg (apply #'format fmt args)))))
      (agent-repl-print-git-branch)
      (should (stringp captured-msg))
      (should (string-match-p (regexp-quote agent-repl-git-branch) captured-msg)))))

;;;; ---- Tests: path-canonical ----

(ert-deftest agent-repl-test-path-canonical-trailing-slash ()
  "path-canonical should strip trailing slash."
  (let ((result (agent-repl--path-canonical "/tmp/foo/")))
    (should-not (string-suffix-p "/" result))))

(ert-deftest agent-repl-test-path-canonical-no-trailing-slash ()
  "path-canonical without trailing slash should remain unchanged (modulo truename)."
  (let ((result (agent-repl--path-canonical "/tmp")))
    (should-not (string-suffix-p "/" result))))

(ert-deftest agent-repl-test-path-canonical-tilde ()
  "path-canonical should expand tilde."
  (let ((result (agent-repl--path-canonical "~")))
    (should-not (string-prefix-p "~" result))
    (should (string-prefix-p "/" result))))

(ert-deftest agent-repl-test-path-canonical-unbound-cache-asks-the-filesystem ()
  "With no cache bound, every call resolves afresh — no hidden memo."
  ;; Arrange
  (let ((agent-repl--path-canonical-cache nil)
        (calls 0))
    (cl-letf (((symbol-function 'file-truename)
               ;; Stubbed rather than wrapped: the real `file-truename'
               ;; recurses on itself, so a counting wrapper would count
               ;; components instead of calls.
               (lambda (p &rest _) (setq calls (1+ calls)) p)))
      ;; Act
      (agent-repl--path-canonical "/tmp")
      (agent-repl--path-canonical "/tmp")
      ;; Assert
      (should (= calls 2)))))

(ert-deftest agent-repl-test-path-canonical-bound-cache-resolves-a-path-once ()
  "A bound cache resolves each distinct path exactly once."
  ;; Arrange
  (let ((agent-repl--path-canonical-cache (make-hash-table :test 'equal))
        (calls 0))
    (cl-letf (((symbol-function 'file-truename)
               ;; Stubbed rather than wrapped: the real `file-truename'
               ;; recurses on itself, so a counting wrapper would count
               ;; components instead of calls.
               (lambda (p &rest _) (setq calls (1+ calls)) p)))
      ;; Act
      (agent-repl--path-canonical "/tmp")
      (agent-repl--path-canonical "/tmp")
      ;; Assert
      (should (= calls 1)))))

(ert-deftest agent-repl-test-path-canonical-cache-returns-the-same-answer ()
  "A memoized answer is the answer the uncached call would have given."
  ;; Arrange
  (let ((uncached (let ((agent-repl--path-canonical-cache nil))
                    (agent-repl--path-canonical "/tmp/foo/")))
        (agent-repl--path-canonical-cache (make-hash-table :test 'equal)))
    ;; Act
    (agent-repl--path-canonical "/tmp/foo/")
    ;; Assert
    (should (equal uncached (agent-repl--path-canonical "/tmp/foo/")))))

(ert-deftest agent-repl-test-path-canonical-relative-path ()
  "path-canonical should expand relative paths."
  (let ((result (agent-repl--path-canonical ".")))
    (should (string-prefix-p "/" result))))

(ert-deftest agent-repl-test-path-canonical-root ()
  "path-canonical for / should return /."
  ;; directory-file-name of "/" is "" on some systems, but file-truename "/" is "/"
  ;; and directory-file-name "/" is "/"; this just verifies no crash.
  (let ((result (agent-repl--path-canonical "/")))
    (should (stringp result))))

(ert-deftest agent-repl-test-path-canonical-symlink ()
  "path-canonical should resolve symlinks to true path."
  (let ((tmpdir (make-temp-file "test-sym-" t)))
    (unwind-protect
        (let* ((real-dir (expand-file-name "real" tmpdir))
               (link-dir (expand-file-name "link" tmpdir)))
          (make-directory real-dir t)
          (make-symbolic-link real-dir link-dir)
          (let ((result (agent-repl--path-canonical link-dir)))
            ;; Should resolve to the real path
            (should (string-match-p "real" result))
            (should-not (string-match-p "link" result))))
      (delete-directory tmpdir t))))

(ert-deftest agent-repl-test-path-canonical-empty-string ()
  "path-canonical with empty string should not error."
  (let ((result (agent-repl--path-canonical "")))
    (should (stringp result))))

;;;; ---- Tests: workspace-dir-hash ----

(ert-deftest agent-repl-test-workspace-dir-hash-nil-when-no-ws-dir ()
  "workspace-dir-hash should return nil when no workspace has a :project-dir."
  (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
            ((symbol-function 'agent-repl--ws-dir)
             (lambda (_ws) (error "no dir"))))
    (should-not (agent-repl--workspace-dir-hash))))

(ert-deftest agent-repl-test-workspace-dir-hash-hash-length ()
  "workspace-dir-hash should return exactly 8 characters."
  (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
            ((symbol-function 'agent-repl--ws-dir) (lambda (_ws) "/test/project")))
    (let ((id (agent-repl--workspace-dir-hash)))
      (should (= (length id) 8)))))

(ert-deftest agent-repl-test-workspace-dir-hash-different-roots ()
  "Two different roots should produce different hashes."
  (let (id1 id2)
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
              ((symbol-function 'agent-repl--ws-dir) (lambda (_ws) "/path/one")))
      (setq id1 (agent-repl--workspace-dir-hash)))
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws2"))
              ((symbol-function 'agent-repl--ws-dir) (lambda (_ws) "/path/two")))
      (setq id2 (agent-repl--workspace-dir-hash)))
    (should-not (equal id1 id2))))

(ert-deftest agent-repl-test-workspace-dir-hash-deterministic ()
  "Same root should always produce the same hash."
  (let (id1 id2)
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
              ((symbol-function 'agent-repl--ws-dir) (lambda (_ws) "/stable/path")))
      (setq id1 (agent-repl--workspace-dir-hash))
      (setq id2 (agent-repl--workspace-dir-hash)))
    (should (equal id1 id2))))

;;;; ---- Tests: create-buffer ----

(ert-deftest agent-repl-test-create-buffer-bare-name ()
  "create-buffer with no suffix produces the bare *agent-panel-WS* name.
No current production caller creates a buffer through this path (the
input buffer, via \"-input\", is the only one that does), but the
capability is still part of the function's contract."
  (let ((buf nil))
    (unwind-protect
        (progn
          (setq buf (agent-repl--create-buffer "ws1"))
          (should (buffer-live-p buf))
          (should (equal (buffer-name buf) "*agent-panel-ws1*")))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest agent-repl-test-create-buffer-input-suffix ()
  "create-buffer with \"-input\" suffix produces the input buffer name."
  (let ((buf nil))
    (unwind-protect
        (progn
          (setq buf (agent-repl--create-buffer "ws1" "-input"))
          (should (equal (buffer-name buf) "*agent-panel-input-ws1*")))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest agent-repl-test-create-buffer-sets-owning-workspace ()
  "create-buffer sets `agent-repl--owning-workspace' buffer-locally."
  (let ((buf nil))
    (unwind-protect
        (progn
          (setq buf (agent-repl--create-buffer "ws1"))
          (should (equal (buffer-local-value 'agent-repl--owning-workspace buf)
                         "ws1")))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest agent-repl-test-create-buffer-owning-workspace-survives-mode ()
  "`agent-repl--owning-workspace' survives `kill-all-local-variables'.
Major-mode activation (vterm-mode, agent-repl-input-mode) wipes buffer-local
bindings; the permanent-local property on this variable is what keeps
ownership intact across that transition."
  (let ((buf nil))
    (unwind-protect
        (progn
          (setq buf (agent-repl--create-buffer "ws1"))
          (with-current-buffer buf
            (kill-all-local-variables))
          (should (equal (buffer-local-value 'agent-repl--owning-workspace buf)
                         "ws1")))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest agent-repl-test-create-buffer-attaches-to-persp ()
  "create-buffer adds the buffer to WS's perspective when it exists."
  (let ((buf nil)
        (added nil))
    (unwind-protect
        (cl-letf (((symbol-function 'persp-get-by-name)
                   (lambda (_name) 'fake-persp))
                  ((symbol-function 'persp-add-buffer)
                   (lambda (b persp &rest _)
                     (setq added (list b persp)))))
          (setq buf (agent-repl--create-buffer "ws1"))
          (should (equal added (list buf 'fake-persp))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest agent-repl-test-create-buffer-skips-persp-when-not-found ()
  "create-buffer does not error when no perspective named WS exists."
  (let ((buf nil)
        (add-called nil))
    (unwind-protect
        (cl-letf (((symbol-function 'persp-get-by-name) (lambda (_name) nil))
                  ((symbol-function 'persp-add-buffer)
                   (lambda (&rest _) (setq add-called t))))
          (setq buf (agent-repl--create-buffer "ws1"))
          (should-not add-called))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest agent-repl-test-create-buffer-errors-when-ws-nil ()
  "create-buffer signals an error when WS is nil and no current workspace."
  (cl-letf (((symbol-function '+workspace-current-name) (lambda () nil)))
    (should-error (agent-repl--create-buffer nil) :type 'error)))

(ert-deftest agent-repl-test-create-buffer-idempotent ()
  "Calling create-buffer twice with the same args reuses the buffer."
  (let ((first nil))
    (unwind-protect
        (let* ((_ (setq first (agent-repl--create-buffer "ws1")))
               (second (agent-repl--create-buffer "ws1")))
          (should (eq first second)))
      (when (buffer-live-p first) (kill-buffer first)))))

;;;; ---- Tests: ws-observed-claude-session-id ----
;;
;; Emacs holds NO durable copy of a vendor conversation uuid. It reads the
;; current one off the daemon-pushed `SessionView\=' store, purely to attribute
;; its own log records. The accessor this replaced read a PERSISTED uuid and
;; handed it back as a resume pointer, which made Emacs a second authority on
;; which conversation a workspace owns.

(ert-deftest agent-repl-test-observed-session-id-reads-the-pushed-host-frame ()
  "ws-observed-claude-session-id reads the pushed HostWorkspace's vendor arm."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/w")
    (cl-letf (((symbol-function 'agent-repl-host--live)
               (lambda (ws) (when (equal ws "ws1")
                              '(:vendor-info (:arm :claude :value (:session-id "cli-uuid-1")))))))
      (should (equal (agent-repl--ws-observed-claude-session-id "ws1") "cli-uuid-1")))))

(ert-deftest agent-repl-test-observed-session-id-ignores-the-in-memory-instantiation ()
  "An instantiation carrying a uuid is NOT a source for attribution.
Reading it back would be the persisted-pointer path returning by another
name; the daemon-pushed host frame is the only source."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :active-env :bare-metal)
    (agent-repl--ws-put "ws1" :bare-metal
                        (make-agent-repl-instantiation :session-id "stale-uuid"))
    (cl-letf (((symbol-function 'agent-repl-host--live)
               (lambda (_) nil)))
      (should-not (agent-repl--ws-observed-claude-session-id "ws1")))))

(ert-deftest agent-repl-test-observed-session-id-nil-while-no-vendor-conversation-exists ()
  "The vendor oneof is legally UNSET until a conversation exists."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl-host--live)
               (lambda (_) '(:vendor-info nil))))
      (should-not (agent-repl--ws-observed-claude-session-id "ws1")))))

(ert-deftest agent-repl-test-observed-session-id-nil-before-the-first-push ()
  "Nil before the first pushed frame is a NORMAL answer, not a failure.
An unattributed log record is accepted by the daemon; a misattributed one is
what gets rejected, so guessing would be strictly worse than saying nothing."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl-host--live)
               (lambda (_) nil)))
      (should-not (agent-repl--ws-observed-claude-session-id "ws1")))))

(ert-deftest agent-repl-test-observed-session-id-nil-for-an-empty-vendor-id ()
  "An empty vendor id is the vendor naming nothing, not an identity."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl-host--live)
               (lambda (_) '(:vendor-info (:arm :claude :value (:session-id ""))))))
      (should-not (agent-repl--ws-observed-claude-session-id "ws1")))))

;;;; ---- Tests: buffer-name edge cases ----

(ert-deftest agent-repl-test-buffer-name-empty-suffix ()
  "Buffer name with empty string suffix should work like no suffix."
  (cl-letf (((symbol-function '+workspace-current-name) (lambda () "abcd1234")))
    (should (equal (agent-repl--buffer-name "") "*agent-panel-abcd1234*"))))

(ert-deftest agent-repl-test-buffer-name-various-suffixes ()
  "Buffer name with various suffix values should include them."
  (cl-letf (((symbol-function '+workspace-current-name) (lambda () "abcd1234")))
    (should (equal (agent-repl--buffer-name "-debug") "*agent-panel-debug-abcd1234*"))
    (should (equal (agent-repl--buffer-name "-log") "*agent-panel-log-abcd1234*"))))

;;;; ---- Tests: agent-panel-buffer-p ----

(ert-deftest agent-repl-test-agent-panel-buffer-p-frontend ()
  "agent-panel-buffer-p should match the gui webview buffer name too.
The predicate widened to cover the two buffers a workspace actually has
now that vterm is gone: the input composer and the webview."
  (agent-repl-test--with-temp-buffer "*agent-frontend-abcd1234*"
    (should (agent-repl--agent-panel-buffer-p))))

(ert-deftest agent-repl-test-agent-panel-buffer-p-input ()
  "agent-panel-buffer-p should match input buffer names."
  (agent-repl-test--with-temp-buffer "*agent-panel-input-abcd1234*"
    (should (agent-repl--agent-panel-buffer-p))))

(ert-deftest agent-repl-test-agent-panel-buffer-p-workspace-log ()
  "The workspace-owned live log buffer is an agent-repl buffer."
  (agent-repl-test--with-temp-buffer "*agent-panel-log-abcd1234*"
    (should (agent-repl--agent-panel-buffer-p))))

(ert-deftest agent-repl-test-agent-panel-buffer-p-regular ()
  "agent-panel-buffer-p should not match regular buffer names."
  (agent-repl-test--with-temp-buffer "*scratch*"
    (should-not (agent-repl--agent-panel-buffer-p))))

(ert-deftest agent-repl-test-agent-panel-buffer-p-nil ()
  "agent-panel-buffer-p with nil should use current buffer."
  (agent-repl-test--with-temp-buffer "*agent-panel-input-abcd1234*"
    (should (agent-repl--agent-panel-buffer-p nil))))

;;;; ---- Tests: non-user-buffer-p ----

(ert-deftest agent-repl-test-non-user-buffer-p-agent-panel ()
  "non-user-buffer-p should return non-nil for the agent's own panel buffers."
  (agent-repl-test--with-temp-buffer "*agent-frontend-abcd1234*"
    (should (agent-repl--non-user-buffer-p (current-buffer)))))

(ert-deftest agent-repl-test-non-user-buffer-p-minibuffer ()
  "non-user-buffer-p should return non-nil for minibuffer-like names."
  (agent-repl-test--with-temp-buffer " *Minibuf-0*"
    (should (agent-repl--non-user-buffer-p (current-buffer)))))

(ert-deftest agent-repl-test-non-user-buffer-p-dead-buffer ()
  "non-user-buffer-p should return non-nil for dead/killed buffer."
  (let ((buf (get-buffer-create " *test-dead*")))
    (kill-buffer buf)
    (should (agent-repl--non-user-buffer-p buf))))

(ert-deftest agent-repl-test-non-user-buffer-p-nil ()
  "non-user-buffer-p should return non-nil for nil input."
  (should (agent-repl--non-user-buffer-p nil)))

(ert-deftest agent-repl-test-non-user-buffer-p-string-nonexistent ()
  "non-user-buffer-p should return non-nil for string name of non-existent buffer."
  (should (agent-repl--non-user-buffer-p "nonexistent-buffer-name-99999")))

(ert-deftest agent-repl-test-non-user-buffer-p-string-existing ()
  "non-user-buffer-p should return nil for string name of existing normal buffer."
  (agent-repl-test--with-temp-buffer "*normal-test-buf*"
    (should-not (agent-repl--non-user-buffer-p "*normal-test-buf*"))))

(ert-deftest agent-repl-test-non-user-buffer-p-normal-buffer ()
  "non-user-buffer-p should return nil for a normal live buffer."
  (agent-repl-test--with-temp-buffer "*normal-test-buf2*"
    (should-not (agent-repl--non-user-buffer-p (current-buffer)))))

;;;; ---- Tests: current-ws-p ----

(ert-deftest agent-repl-test-current-ws-p-match ()
  "current-ws-p should return non-nil when WS matches current workspace."
  (cl-letf (((symbol-function '+workspace-current-name) (lambda () "my-ws")))
    (should (agent-repl--current-ws-p "my-ws"))))

(ert-deftest agent-repl-test-current-ws-p-no-match ()
  "current-ws-p should return nil when WS does not match."
  (cl-letf (((symbol-function '+workspace-current-name) (lambda () "my-ws")))
    (should-not (agent-repl--current-ws-p "other-ws"))))

(ert-deftest agent-repl-test-current-ws-p-empty-string ()
  "current-ws-p with empty string should not match non-empty workspace."
  (cl-letf (((symbol-function '+workspace-current-name) (lambda () "my-ws")))
    (should-not (agent-repl--current-ws-p ""))))

(ert-deftest agent-repl-test-current-ws-p-no-current-workspace-is-nil-not-an-error ()
  "No workspace selected answers nil rather than signalling.
`agent-repl--ws-current-name' documents nil as legal, and the finish-edge
reactions read this predicate: a signal there would abort the reactions
still queued behind it for that edge."
  (cl-letf (((symbol-function '+workspace-current-name) (lambda () nil)))
    (should-not (agent-repl--current-ws-p "my-ws"))))

(ert-deftest agent-repl-test-current-ws-p-nil-ws-is-nil-not-an-error ()
  "A nil WS is not the current workspace, and asking is not an error."
  (cl-letf (((symbol-function '+workspace-current-name) (lambda () "my-ws")))
    (should-not (agent-repl--current-ws-p nil))))

;;;; ---- Tests: instantiation struct ----

(ert-deftest agent-repl-test-instantiation-create ()
  "make-agent-repl-instantiation should create a struct."
  (let ((inst (make-agent-repl-instantiation)))
    (should (agent-repl-instantiation-p inst))))

(ert-deftest agent-repl-test-instantiation-default-fields ()
  "Instantiation struct fields should default to nil."
  (let ((inst (make-agent-repl-instantiation)))
    (should-not (agent-repl-instantiation-session-id inst))
    (should-not (agent-repl-instantiation-start-cmd inst))))

(ert-deftest agent-repl-test-instantiation-setf ()
  "Instantiation struct fields should be modifiable with setf."
  (let ((inst (make-agent-repl-instantiation)))
    (setf (agent-repl-instantiation-session-id inst) "sess-123")
    (setf (agent-repl-instantiation-start-cmd inst) "claude --resume")
    (should (equal (agent-repl-instantiation-session-id inst) "sess-123"))
    (should (equal (agent-repl-instantiation-start-cmd inst) "claude --resume"))))

;;;; ---- Tests: defvar/defcustom declarations ----

(ert-deftest agent-repl-test-timers-var-bound ()
  "`agent-repl--timers' should be bound."
  (should (boundp 'agent-repl--timers)))

(ert-deftest agent-repl-test-debug-default-nil ()
  "`agent-repl-debug' should default to nil."
  (should (boundp 'agent-repl-debug))
  ;; Note: default-value because tests may let-bind it
  (should-not (default-value 'agent-repl-debug)))

(ert-deftest agent-repl-test-input-buffer-re-matches ()
  "`agent-repl--input-buffer-re' should match expected input buffer patterns."
  (should (string-match-p agent-repl--input-buffer-re "*agent-panel-input-abcd1234*"))
  (should (string-match-p agent-repl--input-buffer-re "*agent-panel-input-my-workspace*"))
  (should-not (string-match-p agent-repl--input-buffer-re "*agent-panel-abcd1234*"))
  (should-not (string-match-p agent-repl--input-buffer-re "*scratch*")))

;;;; ---- Tests: log-format hardening against non-string fmt ----

(ert-deftest agent-repl-test-log-format-tolerates-symbol-fmt ()
  "`agent-repl--log-format' must not crash when FMT is a symbol.
Regression guard for callers that pass a file-notify action symbol by mistake."
  (let ((agent-repl--log-format-bug-captured t)) ; suppress capture side-effect
    (let ((result (agent-repl--log-format nil 'stopped)))
      (should (stringp result))
      (should (string-match-p "BUG non-string-fmt=stopped" result)))))

(ert-deftest agent-repl-test-log-format-captures-backtrace-once ()
  "Non-string FMT should capture a backtrace into *agent-repl-log-bug* only once."
  (let ((agent-repl--log-format-bug-captured nil))
    (unwind-protect
        (progn
          (when (get-buffer "*agent-repl-log-bug*")
            (kill-buffer "*agent-repl-log-bug*"))
          (agent-repl--log-format nil 'stopped)
          (should (get-buffer "*agent-repl-log-bug*"))
          (should agent-repl--log-format-bug-captured)
          (let ((size (buffer-size (get-buffer "*agent-repl-log-bug*"))))
            (agent-repl--log-format nil 'changed)
            ;; Second call should NOT add more content.
            (should (= size (buffer-size (get-buffer "*agent-repl-log-bug*"))))))
      (when (get-buffer "*agent-repl-log-bug*")
        (kill-buffer "*agent-repl-log-bug*")))))

(ert-deftest agent-repl-test-do-log-survives-percent-in-metadata ()
  "`agent-repl--do-log' must not raise arity errors when workspace metadata
contains a literal `%' character.  Regression for \"Not enough arguments for
format string\" seen when running `agent-repl-reset-sentinel-watchers'."
  (let ((agent-repl-debug t))
    (cl-letf (((symbol-function 'agent-repl--format-ws-metadata)
               (lambda (_ws) " {dir=/path/with/%s/literal}"))
              ((symbol-function 'message) #'ignore))
      ;; Should complete without signaling a format-string arity error.
      (agent-repl--log nil "plain message, no specifiers")
      (agent-repl--log nil "one-specifier=%s" "value")
      ;; Non-string fmt path should also be safe.
      (agent-repl--log nil 'stopped)
      (should t))))

;;;; ---- Tests: file-write decoupled from debug gate ----

(defmacro agent-repl-test--with-temp-logfile (sym &rest body)
  "Bind a fresh per-test logfile path to SYM and route agent-repl writes to it.
The temp file is deleted after BODY runs.  `agent-repl-log-to-file' is
forced on inside BODY (test-helpers globally turns it off to keep other
tests pollution-free).  Every retained generation is removed afterwards."
  (declare (indent 1))
  `(let ((,sym (make-temp-file "agent-repl-test-log-")))
     (unwind-protect
         (let ((agent-repl-log-to-file t)
               (agent-repl-log-file-name ,sym)
               (agent-repl-log-file-level 'debug))
           ,@body)
       (when (file-exists-p ,sym) (delete-file ,sym))
       (dotimes (index agent-repl-log-generation-count)
         (let ((generation (agent-repl--log-generation-path ,sym (1+ index))))
           (when (file-exists-p generation)
             (delete-file generation)))))))

(defun agent-repl-test--read-file (path)
  "Return PATH's complete contents for logging assertions."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun agent-repl-test--log-records (path)
  "Return every JSONL record written to PATH, parsed, oldest first."
  (mapcar (lambda (line) (json-parse-string line :object-type 'alist))
          (split-string (agent-repl-test--read-file path) "\n" t)))

(defun agent-repl-test--log-record-for (path operation)
  "Return the first record in PATH whose `operation' contains OPERATION."
  (seq-find (lambda (record)
              (string-match-p operation (or (alist-get 'operation record) "")))
            (agent-repl-test--log-records path)))

(ert-deftest agent-repl-test-log-always-writes-file-when-debug-off ()
  "`agent-repl--log' must write to file even when `agent-repl-debug' is nil.
This is the core decoupling guarantee — the file is the canonical
record; `agent-repl-debug' now only gates the *Messages* emit."
  (agent-repl-test--with-temp-logfile path
    (cl-letf (((symbol-function 'message) #'ignore))
      (let ((agent-repl-debug nil))
        (agent-repl--log nil "debug-off line")
        (should (file-exists-p path))
        (with-temp-buffer
          (insert-file-contents path)
          (should (string-match-p "debug-off line" (buffer-string))))))))

(ert-deftest agent-repl-test-log-still-suppresses-message-when-debug-off ()
  "`agent-repl--log' must NOT call `message' when `agent-repl-debug' is nil."
  (agent-repl-test--with-temp-logfile path
    (let ((message-called nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (&rest _) (setq message-called t))))
        (let ((agent-repl-debug nil))
          (agent-repl--log nil "test")
          (should-not message-called))))))

(ert-deftest agent-repl-test-log-verbose-persists-when-debug-off ()
  "`agent-repl--log-verbose' persists even when terminal visibility is off.
The durable THRESHOLD is opened here so the subject under test is the
decoupling from `agent-repl-debug' alone; `agent-repl-log-file-level' is
the separate knob that decides whether this rung is written at all."
  (agent-repl-test--with-temp-logfile path
    (cl-letf (((symbol-function 'message) #'ignore))
      (let ((agent-repl-debug nil)
            (agent-repl-log-file-level 'debug))
        (agent-repl--log-verbose nil "verbose-line")
        (should (> (nth 7 (file-attributes path)) 0))))))

(ert-deftest agent-repl-test-log-verbose-persists-when-debug-t ()
  "`agent-repl--log-verbose' persists when normal debug visibility is on.
See the sibling test: the durable threshold is opened so this asserts the
decoupling from `agent-repl-debug' rather than the threshold's own gate."
  (agent-repl-test--with-temp-logfile path
    (cl-letf (((symbol-function 'message) #'ignore))
      (let ((agent-repl-debug t)
            (agent-repl-log-file-level 'debug))
        (agent-repl--log-verbose nil "verbose-line")
        (should (> (nth 7 (file-attributes path)) 0))))))

(ert-deftest agent-repl-test-log-verbose-writes-file-when-debug-verbose ()
  "`agent-repl--log-verbose' writes to file when debug is `verbose'.
The durable threshold is opened alongside it: both knobs must be at
verbose for this rung to reach the file, and this test is about the
second one not blocking the first."
  (agent-repl-test--with-temp-logfile path
    (cl-letf (((symbol-function 'message) #'ignore))
      (let ((agent-repl-debug 'verbose)
            (agent-repl-log-file-level 'debug))
        (agent-repl--log-verbose nil "verbose-line")
        (should (file-exists-p path))
        (with-temp-buffer
          (insert-file-contents path)
          (should (string-match-p "verbose-line" (buffer-string))))))))

(ert-deftest agent-repl-test-log-verbose-still-suppresses-message-when-debug-t ()
  "`agent-repl--log-verbose' must NOT call `message' when debug is t (only verbose).
Regression guard: the file-write decoupling must not collapse the
verbose-vs-standard distinction at the message-emit layer."
  (agent-repl-test--with-temp-logfile path
    (let ((message-called nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (&rest _) (setq message-called t))))
        (let ((agent-repl-debug t))
          (agent-repl--log-verbose nil "test")
          (should-not message-called))))))

(ert-deftest agent-repl-test-log-no-file-write-when-log-to-file-nil ()
  "`agent-repl--log' must not touch the disk when `agent-repl-log-to-file' is nil.
The master kill-switch overrides the always-on file-write decoupling."
  (let ((path (make-temp-file "agent-repl-test-log-")))
    (unwind-protect
        (let ((agent-repl-log-to-file nil)
              (agent-repl-log-file-name path)
              (initial-size (nth 7 (file-attributes path))))
          (cl-letf (((symbol-function 'message) #'ignore))
            (let ((agent-repl-debug nil))
              (agent-repl--log nil "should-not-land"))
            (should (= initial-size (nth 7 (file-attributes path))))))
      (when (file-exists-p path) (delete-file path)))))

;;;; ---- Tests: size-capped generations ----

(ert-deftest agent-repl-test-log-write-rotates-before-crossing-cap ()
  "The record crossing the cap starts a fresh active generation whole."
  ;; Arrange
  (agent-repl-test--with-temp-logfile path
    (let ((agent-repl-log-size-cap-bytes 8))
      (write-region "1234567" nil path)
      ;; Act
      (agent-repl--do-log-to-file "new")
      ;; Assert
      (should (equal (agent-repl-test--read-file path) "new\n"))
      (should (equal (agent-repl-test--read-file (concat path ".1")) "1234567")))))

(ert-deftest agent-repl-test-log-generations-retain-five-newest ()
  "Generation shifting retains `.1' through `.5' and deletes the oldest."
  ;; Arrange
  (agent-repl-test--with-temp-logfile path
    (let ((agent-repl-log-generation-count 5))
      (write-region "active" nil path)
      (dotimes (index 5)
        (write-region (format "generation-%d" (1+ index)) nil
                      (agent-repl--log-generation-path path (1+ index))))
      ;; Act
      (agent-repl--log-rotate-generations path)
      ;; Assert
      (should (equal (agent-repl-test--read-file (concat path ".1")) "active"))
      (should (equal (agent-repl-test--read-file (concat path ".5")) "generation-4"))
      (should-not (file-exists-p (concat path ".6"))))))

(ert-deftest agent-repl-test-log-write-keeps-an-oversize-record-whole ()
  "One record larger than the cap remains whole in the active file."
  ;; Arrange / Act
  (agent-repl-test--with-temp-logfile path
    (delete-file path)
    (let ((agent-repl-log-size-cap-bytes 4))
      (agent-repl--do-log-to-file "oversize")
      ;; Assert
      (should (equal (agent-repl-test--read-file path) "oversize\n"))
      (should-not (file-exists-p (concat path ".1"))))))

;;;; ---- Tests: buffer-owner accessor ----

(ert-deftest agent-repl-test-core-buffer-owner-returns-owner ()
  "buffer-owner returns the buffer-local owning workspace."
  (let ((buf (get-buffer-create "*bo-owned*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (setq-local agent-repl--owning-workspace "owner-ws"))
          (should (equal "owner-ws" (agent-repl--buffer-owner buf))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest agent-repl-test-core-buffer-owner-nil-when-unset ()
  "buffer-owner returns nil for a buffer with no owner set."
  (let ((buf (get-buffer-create "*bo-unset*")))
    (unwind-protect
        (should-not (agent-repl--buffer-owner buf))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest agent-repl-test-core-buffer-owner-nil-when-dead ()
  "buffer-owner returns nil for a dead buffer instead of erroring."
  (let ((buf (get-buffer-create "*bo-dead*")))
    (with-current-buffer buf
      (setq-local agent-repl--owning-workspace "owner-ws"))
    (kill-buffer buf)
    (should-not (agent-repl--buffer-owner buf))))

(ert-deftest agent-repl-test-core-buffer-owner-nil-when-nil-buffer ()
  "buffer-owner returns nil for a nil buffer argument."
  (should-not (agent-repl--buffer-owner nil)))

;;;; ---- Tests: foreign-owned-buffer-p ----

(ert-deftest agent-repl-test-core-foreign-owned-buffer-p/foreign-owner ()
  "foreign-owned-buffer-p is non-nil for a buffer owned by another workspace."
  (let ((buf (get-buffer-create "*fo-foreign*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (setq-local agent-repl--owning-workspace "other-ws"))
          (should (agent-repl--foreign-owned-buffer-p buf "this-ws")))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest agent-repl-test-core-foreign-owned-buffer-p/same-owner ()
  "foreign-owned-buffer-p is nil for a buffer owned by the same workspace."
  (let ((buf (get-buffer-create "*fo-own*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (setq-local agent-repl--owning-workspace "this-ws"))
          (should-not (agent-repl--foreign-owned-buffer-p buf "this-ws")))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest agent-repl-test-core-foreign-owned-buffer-p/no-owner ()
  "foreign-owned-buffer-p is nil for an unowned buffer (e.g. magit/file)."
  (let ((buf (get-buffer-create "*fo-unowned*")))
    (unwind-protect
        (should-not (agent-repl--foreign-owned-buffer-p buf "this-ws"))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest agent-repl-test-core-foreign-owned-buffer-p/dead-buffer ()
  "foreign-owned-buffer-p is nil for a dead buffer."
  (let ((buf (get-buffer-create "*fo-dead*")))
    (with-current-buffer buf
      (setq-local agent-repl--owning-workspace "other-ws"))
    (kill-buffer buf)
    (should-not (agent-repl--foreign-owned-buffer-p buf "this-ws"))))

;;;; ---- Tests: workspace-name prefix ----

(ert-deftest agent-repl-test-workspace-prefix-env-set ()
  "workspace-prefix falls back to the legacy CLAUDE_WORKSPACE_PREFIX."
  (cl-letf (((symbol-function 'getenv)
             (lambda (k) (and (equal k "CLAUDE_WORKSPACE_PREFIX") "DWC"))))
    (should (equal (agent-repl--workspace-prefix) "DWC"))))

(ert-deftest agent-repl-test-workspace-prefix-new-env-set ()
  "workspace-prefix returns the AGENT_WORKSPACE_PREFIX value when set."
  (cl-letf (((symbol-function 'getenv)
             (lambda (k) (and (equal k "AGENT_WORKSPACE_PREFIX") "AWP"))))
    (should (equal (agent-repl--workspace-prefix) "AWP"))))

(ert-deftest agent-repl-test-workspace-prefix-new-env-wins-over-legacy ()
  "workspace-prefix prefers AGENT_WORKSPACE_PREFIX over the legacy var."
  (cl-letf (((symbol-function 'getenv)
             (lambda (k) (cond ((equal k "AGENT_WORKSPACE_PREFIX") "AWP")
                               ((equal k "CLAUDE_WORKSPACE_PREFIX") "DWC")))))
    (should (equal (agent-repl--workspace-prefix) "AWP"))))

(ert-deftest agent-repl-test-workspace-prefix-empty-new-env-falls-back ()
  "workspace-prefix treats an empty AGENT_WORKSPACE_PREFIX as unset."
  (cl-letf (((symbol-function 'getenv)
             (lambda (k) (cond ((equal k "AGENT_WORKSPACE_PREFIX") "")
                               ((equal k "CLAUDE_WORKSPACE_PREFIX") "DWC")))))
    (should (equal (agent-repl--workspace-prefix) "DWC"))))

(ert-deftest agent-repl-test-workspace-prefix-env-unset ()
  "workspace-prefix returns the empty string when the env var is unset."
  (cl-letf (((symbol-function 'getenv) (lambda (_) nil)))
    (should (equal (agent-repl--workspace-prefix) ""))))

;;;; ---- Tests: agent-repl--output-dir constant ----

(ert-deftest agent-repl-test-output-dir-is-absolute ()
  "output-dir should be an absolute path under ~/.claude-emacs/output/."
  (should (file-name-absolute-p agent-repl--output-dir))
  (should (string-match-p "output/$" agent-repl--output-dir)))

;;;; ---- Tests: Buffer background color (moved from overlay.el) ----

(ert-deftest agent-repl-test-rgb-hex-format ()
  "rgb-hex should format R, G, B independently into a #rrggbb string."
  (should (equal (agent-repl--rgb-hex 0 0 0) "#000000"))
  (should (equal (agent-repl--rgb-hex 255 255 255) "#ffffff"))
  (should (equal (agent-repl--rgb-hex 20 20 26) "#14141a")))

(ert-deftest agent-repl-test-rgb-hex-channels-independent ()
  "rgb-hex keeps each channel distinct rather than collapsing to grey."
  (should (equal (agent-repl--rgb-hex 1 2 3) "#010203")))

(ert-deftest agent-repl-test-set-buffer-background-remaps-default-and-fringe ()
  "set-buffer-background remaps both the default and fringe faces."
  (agent-repl-test--with-temp-buffer " *test-bg*"
    (let ((remapped-faces nil))
      (cl-letf (((symbol-function 'face-remap-add-relative)
                 (lambda (face &rest props)
                   (push (list face props) remapped-faces))))
        (agent-repl--set-buffer-background "#1e1e1e")
        (should (= (length remapped-faces) 2))
        (should (assq 'default remapped-faces))
        (should (assq 'fringe remapped-faces))))))

(ert-deftest agent-repl-test-set-buffer-background-applies-passed-color ()
  "set-buffer-background passes its COLOR argument through as the :background."
  (agent-repl-test--with-temp-buffer " *test-bg-hex*"
    (let ((hex-used nil))
      (cl-letf (((symbol-function 'face-remap-add-relative)
                 (lambda (_face &rest props)
                   (setq hex-used (plist-get props :background)))))
        (agent-repl--set-buffer-background "#0f0f0f")
        (should (equal hex-used "#0f0f0f"))))))

(ert-deftest agent-repl-test-set-buffer-background-different-colors-differ ()
  "Different colors passed to set-buffer-background produce different :background values."
  (let (hex-a hex-b)
    (agent-repl-test--with-temp-buffer " *test-bg-diff-a*"
      (cl-letf (((symbol-function 'face-remap-add-relative)
                 (lambda (_face &rest props)
                   (setq hex-a (plist-get props :background)))))
        (agent-repl--set-buffer-background "#0f0f0f")))
    (agent-repl-test--with-temp-buffer " *test-bg-diff-b*"
      (cl-letf (((symbol-function 'face-remap-add-relative)
                 (lambda (_face &rest props)
                   (setq hex-b (plist-get props :background)))))
        (agent-repl--set-buffer-background "#1e1e1e")))
    (should-not (equal hex-a hex-b))))

;;;; ---- Tests: harness-injected (meta) prompt spans ----

(ert-deftest agent-repl-test-meta-wrap-brackets-the-text ()
  "`agent-repl--meta-wrap' brackets TEXT with the open/close markers."
  (should (equal (agent-repl--meta-wrap "injected")
                 (concat agent-repl--meta-open "injected" agent-repl--meta-close))))

(ert-deftest agent-repl-test-meta-wrap-keeps-the-text-verbatim ()
  "The wrapped span still carries its text intact — the agent reads it."
  (should (string-match-p (regexp-quote "read the file at /x/metaprompt.md")
                          (agent-repl--meta-wrap "read the file at /x/metaprompt.md"))))

(ert-deftest agent-repl-test-meta-markers-are-html-comments ()
  "Both markers are inert HTML comments, so no renderer treats them as content."
  (should (string-prefix-p "<!--" agent-repl--meta-open))
  (should (string-suffix-p "-->" agent-repl--meta-open))
  (should (string-prefix-p "<!--" agent-repl--meta-close))
  (should (string-suffix-p "-->" agent-repl--meta-close)))

;;;; ---- Tests: kill-cause attribution ----

(ert-deftest agent-repl-test-kill-cause-str-unbound-is-loud-bug-marker ()
  "An unbound kill-cause renders as a self-documenting BUG marker, so an
unattributed teardown is visible in the log rather than silently blank."
  (let ((agent-repl--kill-cause nil))
    (should (equal (agent-repl--kill-cause-str)
                   "unattributed(BUG: bind agent-repl--kill-cause)"))))

(ert-deftest agent-repl-test-kill-cause-str-returns-bound-cause ()
  "A let-bound kill-cause is returned verbatim for the log line."
  (let ((agent-repl--kill-cause "interactive kill command (test)"))
    (should (equal (agent-repl--kill-cause-str)
                   "interactive kill command (test)"))))

;;;; ---- Tests: assert-main-thread ----

(ert-deftest agent-repl-test-assert-main-thread-passes-on-main ()
  "assert-main-thread is a nil-returning no-op on the main thread."
  (should-not (agent-repl--assert-main-thread "op-x")))

(ert-deftest agent-repl-test-assert-main-thread-signals-off-main ()
  "assert-main-thread signals on a worker thread, naming the operation."
  ;; Arrange — capture the outcome of a genuine non-main-thread call.
  (let ((outcome nil))
    ;; Act.
    (thread-join
     (make-thread
      (lambda ()
        (setq outcome
              (condition-case err
                  (agent-repl--assert-main-thread "op-x")
                (error (error-message-string err)))))))
    ;; Assert — the guard fired and the message names the operation.
    (should (stringp outcome))
    (should (string-match-p "REFUSING op-x off the main thread" outcome))))

;;;; ---- Tests: log-sink routability ----

(ert-deftest agent-repl-test-log-routable-rejects-nil-workspace ()
  "nil is the ladder's global-sink value, never a routable workspace."
  (agent-repl-test--with-clean-state
    (should-not (agent-repl--ws-log-routable-p nil))))

(ert-deftest agent-repl-test-log-routable-rejects-unregistered-name ()
  "A name absent from the workspace hash owns no durable sink."
  (agent-repl-test--with-clean-state
    (should-not (agent-repl--ws-log-routable-p "never-registered"))))

(ert-deftest agent-repl-test-log-routable-rejects-persp-placeholder ()
  "A registered entry without `:project-dir' is a placeholder, not a sink."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "none" :repl-state :inactive)
    (should-not (agent-repl--ws-log-routable-p "none"))))

(ert-deftest agent-repl-test-log-routable-rejects-vanished-project-dir ()
  "A registered `:project-dir' that no longer exists owns no sink."
  (agent-repl-test--with-clean-state
    (let ((project (make-temp-file "agent-repl-routable-gone-" t)))
      (agent-repl--ws-put "gone-ws" :project-dir project)
      (delete-directory project t)
      (should-not (agent-repl--ws-log-routable-p "gone-ws")))))

(ert-deftest agent-repl-test-log-routable-accepts-registered-workspace ()
  "A registered workspace with an existing project directory owns a sink."
  (agent-repl-test--with-clean-state
    (let ((project (make-temp-file "agent-repl-routable-ok-" t)))
      (unwind-protect
          (should (agent-repl--ws-log-routable-p
                   (progn (agent-repl--ws-put "real-ws" :project-dir project)
                          "real-ws")))
        (delete-directory project t)))))

(ert-deftest agent-repl-test-log-routable-refusal-matches-identity-signal ()
  "A non-nil ws the predicate refuses is one the identity resolver signals on.
Pins the two against drift: the predicate exists so callers can avoid
violating the invariant, which only holds while they agree."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "placeholder-ws" :repl-state :inactive)
    (should-not (agent-repl--ws-log-routable-p "placeholder-ws"))
    (should-error (agent-repl--workspace-log-identity "placeholder-ws"))))

(ert-deftest agent-repl-test-log-routable-acceptance-matches-identity-success ()
  "A ws the predicate accepts never makes the identity resolver signal."
  (agent-repl-test--with-clean-state
    (let ((project (make-temp-file "agent-repl-routable-agree-" t)))
      (unwind-protect
          (progn
            (agent-repl--ws-put "agree-ws" :project-dir project)
            (should (agent-repl--ws-log-routable-p "agree-ws"))
            (should (plist-get (agent-repl--workspace-log-identity "agree-ws")
                               :workspace-dir-hash)))
        (delete-directory project t)))))

(ert-deftest agent-repl-test-log-from-persp-placeholder-reaches-global-sink ()
  "A log line emitted while a persp placeholder is current must not signal.
Regression for the boot failure: `agent-repl--ws-current-name' answers
persp-mode's \"none\" outside any workspace, and routing that name into the
ladder made a debug line abort `doom-init-ui-hook'."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-logfile path
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "none"))
                ((symbol-function 'message) #'ignore))
        (agent-repl--log (agent-repl--ws-current-log-name) "placeholder probe")
        (with-temp-buffer
          (insert-file-contents path)
          (should (string-match-p "placeholder probe" (buffer-string))))))))

(ert-deftest agent-repl-test-frame-lifecycle-formats-have-central-reasons ()
  "Workspace-free frame lifecycle records have documented central ownership."
  ;; Arrange.
  (let ((cases
         '(("before-persp-deactivate: entry ws=nil cache=nil"
            . "perspective deactivation can precede workspace selection")
           ("persp-frame-save-state failed for ws=nil: boom"
            . "perspective teardown can run without an agent workspace")
           ("on-window-change"
            . "frame-wide window reconciliation can run without an agent workspace")
           ("sync-panels: entry windows=1"
            . "frame-wide orphan-panel reconciliation spans workspaces")
           ("elisp.notes.popup-predicate: buffer=\"notes\" file=nil match=nil"
            . "popup classification is process-wide"))))
    ;; Act / Assert.
    (dolist (case cases)
      (should (equal (agent-repl--central-log-reason (car case))
                     (cdr case))))))

;;;; ---- Tests: unroutable workspaces fail loudly ----

(ert-deftest agent-repl-test-context-log-scope-resolves-the-buffer-owner ()
  "A context-scoped site inherits its owning composer or panel workspace."
  ;; Arrange
  (let ((agent-repl--log-context-workspace nil)
        (agent-repl-log-to-file t))
    (with-temp-buffer
      (setq-local agent-repl--owning-workspace "buffer-ws")
      (cl-letf (((symbol-function 'agent-repl--ws-log-routable-p)
                 (lambda (ws) (equal ws "buffer-ws"))))
        ;; Act / Assert
        (should (equal (agent-repl--capture-log-scope
                        '(:agent-repl-context "transport can be process-wide"))
                       "buffer-ws"))))))

(ert-deftest agent-repl-test-context-log-scope-resolves-the-current-workspace ()
  "A context-scoped site outside an owned buffer inherits the active workspace."
  ;; Arrange
  (let ((agent-repl--log-context-workspace nil)
        (agent-repl-log-to-file t)
        (agent-repl--owning-workspace nil))
    (cl-letf (((symbol-function 'agent-repl--ws-current-log-name)
               (lambda () "current-ws"))
              ((symbol-function 'agent-repl--ws-log-routable-p)
               (lambda (ws) (equal ws "current-ws"))))
      ;; Act / Assert
      (should (equal (agent-repl--capture-log-scope
                      '(:agent-repl-context "transport can be process-wide"))
                     "current-ws")))))

(ert-deftest agent-repl-test-explicit-log-workspace-wins-over-the-buffer-owner ()
  "An explicit workspace is never replaced by the current buffer's owner."
  ;; Arrange
  (let ((agent-repl--log-context-workspace nil)
        (agent-repl-log-to-file t))
    (with-temp-buffer
      (setq-local agent-repl--owning-workspace "buffer-ws")
      (cl-letf (((symbol-function 'agent-repl--ws-log-routable-p)
                 (lambda (ws) (member ws '("explicit-ws" "buffer-ws")))))
        ;; Act / Assert
        (should (equal (agent-repl--capture-log-scope "explicit-ws")
                       "explicit-ws"))))))

(ert-deftest agent-repl-test-explicit-central-log-scope-uses-the-global-sink ()
  "A reasoned central marker routes globally even from a workspace buffer."
  ;; Arrange
  (let ((agent-repl-log-to-file t)
        (sink-workspace :unset))
    (with-temp-buffer
      (setq-local agent-repl--owning-workspace "buffer-ws")
      (cl-letf (((symbol-function 'agent-repl--do-log-to-file)
                 (lambda (_record ws) (setq sink-workspace ws)))
                ((symbol-function 'message) #'ignore))
        ;; Act
        (agent-repl--info
         '(:agent-repl-central "daemon lifecycle spans workspaces")
         "central event")
        ;; Assert
        (should (null sink-workspace))))))

(ert-deftest agent-repl-test-deleted-worktree-does-not-abort-its-caller ()
  "A workspace whose sink vanished must not take its logging caller down."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-logfile path
      (let ((project (make-temp-file "agent-repl-vanished-" t))
            (agent-repl--unroutable-log-workspaces (make-hash-table :test #'equal)))
        (agent-repl--ws-put "vanished-ws" :project-dir project)
        (delete-directory project t)
        (cl-letf (((symbol-function 'display-warning) #'ignore))
          ;; Act / Assert: a returned record, not a signal.
          (should (agent-repl--log "vanished-ws" "line after the worktree went away")))))))

(ert-deftest agent-repl-test-deleted-worktree-records-the-central-fallback-at-warn ()
  "A REGISTERED directory that is gone is a stale row, announced at WARN."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-logfile path
      ;; Arrange
      (let ((project (make-temp-file "agent-repl-vanished-record-" t))
            (agent-repl--unroutable-log-workspaces (make-hash-table :test #'equal)))
        (agent-repl--ws-put "vanished-record-ws" :project-dir project)
        (delete-directory project t)
        (cl-letf (((symbol-function 'display-warning) #'ignore))
          ;; Act
          (agent-repl--log "vanished-record-ws" "line after the worktree went away"))
        ;; Assert
        (let ((record (agent-repl-test--log-record-for path "log-central-fallback")))
          (should (equal (alist-get 'level record) "warn")))))))

(ert-deftest agent-repl-test-no-durable-home-records-the-central-fallback-at-info ()
  "A workspace with no durable home of its own is ordinary, announced at INFO."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-logfile path
      ;; Arrange
      (let ((agent-repl--unroutable-log-workspaces (make-hash-table :test #'equal)))
        (cl-letf (((symbol-function 'display-warning) #'ignore))
          ;; Act
          (agent-repl--log "unregistered-record-ws" "line from a homeless workspace"))
        ;; Assert
        (let ((record (agent-repl-test--log-record-for path "log-central-fallback")))
          (should (equal (alist-get 'level record) "info")))))))

(ert-deftest agent-repl-test-stale-registration-is-classified-apart ()
  "A registered directory that is GONE classifies as a stale registry row."
  (agent-repl-test--with-clean-state
    ;; Arrange
    (let ((project (make-temp-file "agent-repl-class-stale-" t)))
      (agent-repl--ws-put "class-stale-ws" :project-dir project)
      (delete-directory project t)
      ;; Act / Assert
      (should (eq (agent-repl--central-log-fallback-class "class-stale-ws")
                  'stale-registration)))))

(ert-deftest agent-repl-test-unregistered-workspace-is-classified-as-homeless ()
  "A name with no registration at all owns no durable home, and that is all."
  (agent-repl-test--with-clean-state
    ;; Arrange / Act / Assert
    (should (eq (agent-repl--central-log-fallback-class "class-unregistered-ws")
                'no-durable-home))))

(ert-deftest agent-repl-test-present-directory-is-classified-as-homeless ()
  "A registered directory that still EXISTS is never a stale registry row."
  (agent-repl-test--with-clean-state
    ;; Arrange
    (let ((project (make-temp-file "agent-repl-class-present-" t)))
      (unwind-protect
          (progn
            (agent-repl--ws-put "class-present-ws" :project-dir project)
            ;; Act / Assert
            (should (eq (agent-repl--central-log-fallback-class "class-present-ws")
                        'no-durable-home)))
        (delete-directory project t)))))

(ert-deftest agent-repl-test-ordered-departure-is-classified-apart ()
  "A directory that went because the editor asked is not a stale registry row."
  (agent-repl-test--with-clean-state
    ;; Arrange
    (let ((project (make-temp-file "agent-repl-class-departing-" t))
          (agent-repl--departed-log-workspaces (make-hash-table :test #'equal)))
      (agent-repl--ws-put "class-departing-ws" :project-dir project)
      (delete-directory project t)
      (agent-repl--log-note-workspace-departing "class-departing-ws")
      ;; Act / Assert
      (should (eq (agent-repl--central-log-fallback-class "class-departing-ws")
                  'ordered-departure)))))

(ert-deftest agent-repl-test-ordered-departure-records-the-central-fallback-at-info ()
  "The trailing records of a departure the editor ordered are ordinary."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-logfile path
      ;; Arrange
      (let ((project (make-temp-file "agent-repl-departing-record-" t))
            (agent-repl--unroutable-log-workspaces (make-hash-table :test #'equal))
            (agent-repl--departed-log-workspaces (make-hash-table :test #'equal)))
        (agent-repl--ws-put "departing-record-ws" :project-dir project)
        (delete-directory project t)
        (agent-repl--log-note-workspace-departing "departing-record-ws")
        (cl-letf (((symbol-function 'display-warning) #'ignore))
          ;; Act
          (agent-repl--log "departing-record-ws" "line from a departing workspace"))
        ;; Assert
        (let ((record (agent-repl-test--log-record-for path "log-central-fallback")))
          (should (equal (alist-get 'level record) "info")))))))

(ert-deftest agent-repl-test-ordered-departure-raises-no-popup ()
  "No workspace the user already asked to destroy interrupts them about it."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-logfile path
      ;; Arrange
      (let ((project (make-temp-file "agent-repl-departing-popup-" t))
            (agent-repl--unroutable-log-workspaces (make-hash-table :test #'equal))
            (agent-repl--departed-log-workspaces (make-hash-table :test #'equal))
            (warned nil))
        (agent-repl--ws-put "departing-popup-ws" :project-dir project)
        (delete-directory project t)
        (agent-repl--log-note-workspace-departing "departing-popup-ws")
        (cl-letf (((symbol-function 'display-warning)
                   (lambda (&rest _) (setq warned t))))
          ;; Act
          (agent-repl--log "departing-popup-ws" "line from a departing workspace"))
        ;; Assert
        (should-not warned)))))

(ert-deftest agent-repl-test-a-refused-departure-is-not-an-excuse ()
  "A destructive verb the daemon refused withdraws the order it recorded."
  (agent-repl-test--with-clean-state
    ;; Arrange
    (let ((project (make-temp-file "agent-repl-class-refused-" t))
          (agent-repl--departed-log-workspaces (make-hash-table :test #'equal)))
      (agent-repl--ws-put "class-refused-ws" :project-dir project)
      (delete-directory project t)
      (agent-repl--log-note-workspace-departing "class-refused-ws")
      ;; Act
      (agent-repl--log-forget-workspace-departure "class-refused-ws")
      ;; Assert
      (should (eq (agent-repl--central-log-fallback-class "class-refused-ws")
                  'stale-registration)))))

(ert-deftest agent-repl-test-registering-a-directory-forgets-an-earlier-departure ()
  "A name is reusable, and the new tenant inherits none of the old one's exit."
  (agent-repl-test--with-clean-state
    ;; Arrange
    (let ((first (make-temp-file "agent-repl-class-reused-first-" t))
          (second (make-temp-file "agent-repl-class-reused-second-" t))
          (agent-repl--departed-log-workspaces (make-hash-table :test #'equal)))
      (unwind-protect
          (progn
            (agent-repl--ws-put "class-reused-ws" :project-dir first)
            (delete-directory first t)
            (agent-repl--log-note-workspace-departing "class-reused-ws")
            ;; Act: a workspace arrives under the same name, then loses its own
            ;; directory with nothing having asked for it to go.
            (agent-repl--ws-put "class-reused-ws" :project-dir second)
            (delete-directory second t)
            ;; Assert
            (should (eq (agent-repl--central-log-fallback-class "class-reused-ws")
                        'stale-registration)))
        (when (file-directory-p second) (delete-directory second t))))))

(ert-deftest agent-repl-test-registering-a-directory-forgets-the-spent-claim ()
  "The one announcement a name is owed is owed again to its next tenant."
  (agent-repl-test--with-clean-state
    ;; Arrange
    (let ((project (make-temp-file "agent-repl-claim-reused-" t))
          (agent-repl--unroutable-log-workspaces (make-hash-table :test #'equal))
          (agent-repl--departed-log-workspaces (make-hash-table :test #'equal)))
      (unwind-protect
          (progn
            (puthash "claim-reused-ws" t agent-repl--unroutable-log-workspaces)
            ;; Act
            (agent-repl--ws-put "claim-reused-ws" :project-dir project)
            ;; Assert
            (should (agent-repl--claim-central-log-fallback "claim-reused-ws")))
        (delete-directory project t)))))

(ert-deftest agent-repl-test-deleted-worktree-fallback-names-the-workspace ()
  "The announcement says WHICH workspace lost its sink."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-logfile path
      (let ((project (make-temp-file "agent-repl-vanished-named-" t))
            (agent-repl--unroutable-log-workspaces (make-hash-table :test #'equal)))
        (agent-repl--ws-put "vanished-named-ws" :project-dir project)
        (delete-directory project t)
        (cl-letf (((symbol-function 'display-warning) #'ignore))
          ;; Act
          (agent-repl--log "vanished-named-ws" "line after the worktree went away"))
        ;; Assert
        (let ((record (agent-repl-test--log-record-for path "log-central-fallback")))
          (should (equal (alist-get 'unroutable_workspace record)
                         "vanished-named-ws")))))))

(ert-deftest agent-repl-test-unroutable-workspace-original-reaches-the-global-sink ()
  "The workspace-owned record lands centrally rather than being dropped."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-logfile path
      (let ((agent-repl--unroutable-log-workspaces (make-hash-table :test #'equal)))
        (cl-letf (((symbol-function 'display-warning) #'ignore))
          ;; Act
          (agent-repl--log "no-such-ws" "owned payload"))
        ;; Assert
        (with-temp-buffer
          (insert-file-contents path)
          (should (string-match-p "owned payload" (buffer-string))))))))

(ert-deftest agent-repl-test-unroutable-workspace-original-names-the-workspace ()
  "The rerouted original still says which workspace it is about."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-logfile path
      (let ((agent-repl--unroutable-log-workspaces (make-hash-table :test #'equal)))
        (cl-letf (((symbol-function 'display-warning) #'ignore))
          ;; Act
          (agent-repl--log "no-such-ws" "elisp.test.marked payload"))
        ;; Assert
        (let ((record (agent-repl-test--log-record-for path "elisp-test-marked")))
          (should (equal (alist-get 'unroutable_workspace record) "no-such-ws")))))))

(ert-deftest agent-repl-test-unroutable-workspace-original-claims-no-sink-identity ()
  "The rerouted original is central, so it carries no workspace identity."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-logfile path
      (let ((agent-repl--unroutable-log-workspaces (make-hash-table :test #'equal)))
        (cl-letf (((symbol-function 'display-warning) #'ignore))
          ;; Act
          (agent-repl--log "no-such-ws" "elisp.test.identity payload"))
        ;; Assert
        (let ((record (agent-repl-test--log-record-for path "elisp-test-identity")))
          (should-not (alist-get 'workspace_id record)))))))

(ert-deftest agent-repl-test-unroutable-workspace-record-names-the-operation ()
  "The routing-error record identifies the logger site that failed."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-logfile path
      ;; Arrange
      (let ((agent-repl--log-context-workspace nil)
            (agent-repl--unroutable-log-workspaces (make-hash-table :test #'equal)))
        (cl-letf (((symbol-function 'agent-repl--buffer-owner) (lambda (_buffer) nil))
                  ((symbol-function 'agent-repl--ws-current-log-name) (lambda () nil))
                  ((symbol-function 'display-warning) #'ignore))
          ;; Act
          (agent-repl--log nil "elisp.test.unattributed value=%s" "x"))
        ;; Assert
        (let ((record (agent-repl-test--log-record-for path "log-routing-error")))
          (should (string-match-p
                   "original-operation=agent-repl.elisp-test-unattributed-value-s"
                   (alist-get 'message record))))))))

(ert-deftest agent-repl-test-central-fallback-keeps-the-request-id ()
  "The announcement remains correlated to the request edge that exposed it."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-logfile path
      ;; Arrange
      (let ((agent-repl--log-context-request-id "request-1")
            (agent-repl--unroutable-log-workspaces (make-hash-table :test #'equal)))
        (cl-letf (((symbol-function 'display-warning) #'ignore))
          ;; Act
          (agent-repl--log "no-such-ws" "owned event"))
        ;; Assert
        (let ((record (agent-repl-test--log-record-for path "log-central-fallback")))
          (should (equal (alist-get 'request_id record) "request-1")))))))

(ert-deftest agent-repl-test-unattributed-record-still-records-the-routing-error ()
  "A record naming NO workspace is missing attribution, and still says so.
No directory can supply what the call site never named, so this fact keeps
its ERROR while an unavailable directory does not."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-logfile path
      ;; Arrange
      (let ((agent-repl--log-context-workspace nil)
            (agent-repl--unroutable-log-workspaces (make-hash-table :test #'equal)))
        (cl-letf (((symbol-function 'agent-repl--buffer-owner) (lambda (_buffer) nil))
                  ((symbol-function 'agent-repl--ws-current-log-name) (lambda () nil))
                  ((symbol-function 'display-warning) #'ignore))
          ;; Act
          (agent-repl--log nil "elisp.test.unattributed-still-errors"))
        ;; Assert
        (let ((record (agent-repl-test--log-record-for path "log-routing-error")))
          (should (equal (alist-get 'level record) "error")))))))

(ert-deftest agent-repl-test-stale-registration-is-named-in-a-user-warning ()
  "A registered directory that is GONE is visible and names the workspace."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-logfile path
      ;; Arrange
      (let ((project (make-temp-file "agent-repl-stale-named-" t))
            (agent-repl--unroutable-log-workspaces (make-hash-table :test #'equal))
            (warning nil))
        (agent-repl--ws-put "stale-named-ws" :project-dir project)
        (delete-directory project t)
        (cl-letf (((symbol-function 'display-warning)
                   (lambda (_type text &rest _) (setq warning text))))
          ;; Act
          (agent-repl--log "stale-named-ws" "probe"))
        ;; Assert
        (should (string-match-p "stale-named-ws" warning))))))

(ert-deftest agent-repl-test-stale-registration-warning-keeps-its-wording ()
  "The one warning the user still sees is worded exactly as it always was."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-logfile path
      ;; Arrange
      (let ((project (make-temp-file "agent-repl-stale-wording-" t))
            (agent-repl--unroutable-log-workspaces (make-hash-table :test #'equal))
            (warning nil))
        (agent-repl--ws-put "stale-wording-ws" :project-dir project)
        (delete-directory project t)
        (cl-letf (((symbol-function 'display-warning)
                   (lambda (_type text &rest _) (setq warning text))))
          ;; Act
          (agent-repl--log "stale-wording-ws" "probe"))
        ;; Assert
        (should (string-match-p "cannot host a durable log sink" warning))
        (should (string-match-p "\\[MISSING\\]" warning))))))

(ert-deftest agent-repl-test-stale-registration-warning-keeps-its-level ()
  "The stale registry row keeps the `:warning' level it has always had."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-logfile path
      ;; Arrange
      (let ((project (make-temp-file "agent-repl-stale-level-" t))
            (agent-repl--unroutable-log-workspaces (make-hash-table :test #'equal))
            (level :unset))
        (agent-repl--ws-put "stale-level-ws" :project-dir project)
        (delete-directory project t)
        (cl-letf (((symbol-function 'display-warning)
                   (lambda (_type _text &optional warning-level &rest _)
                     (setq level warning-level))))
          ;; Act
          (agent-repl--log "stale-level-ws" "probe"))
        ;; Assert
        (should (eq level :warning))))))

(ert-deftest agent-repl-test-stale-registration-warns-only-once ()
  "Repeated records about one stale row record every time but warn once."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-logfile path
      ;; Arrange
      (let ((project (make-temp-file "agent-repl-stale-once-" t))
            (agent-repl--unroutable-log-workspaces (make-hash-table :test #'equal))
            (warnings 0))
        (agent-repl--ws-put "stale-once-ws" :project-dir project)
        (delete-directory project t)
        (cl-letf (((symbol-function 'display-warning)
                   (lambda (&rest _) (cl-incf warnings))))
          ;; Act
          (dotimes (_ 5)
            (agent-repl--log "stale-once-ws" "repeated probe")))
        ;; Assert
        (should (= warnings 1))))))

(ert-deftest agent-repl-test-unregistered-workspace-raises-no-user-warning ()
  "A name that owns no registration at all must not interrupt the user."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-logfile path
      ;; Arrange
      (let ((agent-repl--unroutable-log-workspaces (make-hash-table :test #'equal))
            (warnings 0))
        (cl-letf (((symbol-function 'display-warning)
                   (lambda (&rest _) (cl-incf warnings))))
          ;; Act
          (agent-repl--log "no-such-ws" "probe"))
        ;; Assert
        (should (= warnings 0))))))

(ert-deftest agent-repl-test-present-directory-without-a-sink-raises-no-warning ()
  "A registered directory that still exists is ordinary, popup or not."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-logfile path
      ;; Arrange
      (let ((project (make-temp-file "agent-repl-present-nosink-" t))
            (agent-repl--unroutable-log-workspaces (make-hash-table :test #'equal))
            (warnings 0))
        (unwind-protect
            (progn
              (agent-repl--ws-put "present-nosink-ws" :project-dir project)
              (cl-letf (((symbol-function 'agent-repl--ws-dir-hash-cached)
                         (lambda (_ws) nil))
                        ((symbol-function 'display-warning)
                         (lambda (&rest _) (cl-incf warnings))))
                ;; Act
                (agent-repl--log "present-nosink-ws" "probe"))
              ;; Assert
              (should (= warnings 0)))
          (delete-directory project t))))))

(ert-deftest agent-repl-test-routable-workspace-still-uses-its-own-target ()
  "The hard routing invariant does not disturb a routable workspace."
  (agent-repl-test--with-clean-state
    (let* ((project (make-temp-file "agent-repl-still-routed-" t))
           (agent-repl-log-to-file nil)
           (agent-repl--workspace-log-targets (make-hash-table :test #'equal)))
      (unwind-protect
          (progn
            (agent-repl--ws-put "routed-ws" :project-dir project)
            (let ((agent-repl-log-to-file t))
              (cl-letf (((symbol-function 'message) #'ignore))
                (agent-repl--log "routed-ws" "owned line")))
            (should (plist-get (agent-repl--workspace-log-target-entry "routed-ws")
                               :target)))
        (delete-directory project t)))))

;;;; ---- Tests: pseudo-perspectives are classified, not warned about ----
;;
;; persp-mode's own perspectives are not agent-repl workspaces, so a record
;; attributed to one is routed globally by `agent-repl--log-sink-workspace'
;; without a warning.  Screening at the router (rather than at each producer)
;; is what stops an unscreened producer from putting the warning back.

(ert-deftest agent-repl-test-pseudo-workspace-record-emits-no-warning ()
  "Doom's startup perspective owns no sink and that is not an anomaly."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-logfile path
      (let ((+workspaces-main "main")
            (agent-repl--unroutable-log-workspaces (make-hash-table :test #'equal)))
        ;; Act
        (cl-letf (((symbol-function 'message) #'ignore))
          (agent-repl--log "main" "startup perspective probe"))
        ;; Assert
        (with-temp-buffer
          (insert-file-contents path)
          (should-not (string-match-p "unroutable log workspace" (buffer-string))))))))

(ert-deftest agent-repl-test-persp-nil-name-record-emits-no-warning ()
  "persp-mode's nil perspective is the other built-in and warns no more than main."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-logfile path
      (let ((persp-nil-name "none")
            (agent-repl--unroutable-log-workspaces (make-hash-table :test #'equal)))
        ;; Act
        (cl-letf (((symbol-function 'message) #'ignore))
          (agent-repl--log "none" "nil perspective probe"))
        ;; Assert
        (with-temp-buffer
          (insert-file-contents path)
          (should-not (string-match-p "unroutable log workspace" (buffer-string))))))))

(ert-deftest agent-repl-test-pseudo-workspace-record-reaches-global-sink ()
  "Demoting the attribution must not drop the record."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-logfile path
      (let ((+workspaces-main "main")
            (agent-repl--unroutable-log-workspaces (make-hash-table :test #'equal)))
        ;; Act
        (cl-letf (((symbol-function 'message) #'ignore))
          (agent-repl--log "main" "still recorded globally"))
        ;; Assert
        (with-temp-buffer
          (insert-file-contents path)
          (should (string-match-p "still recorded globally" (buffer-string))))))))

(ert-deftest agent-repl-test-pseudo-workspace-name-is-preserved-on-the-record ()
  "The routed record still says which perspective it was about."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-logfile path
      (let ((+workspaces-main "main")
            (agent-repl--unroutable-log-workspaces (make-hash-table :test #'equal)))
        ;; Act
        (cl-letf (((symbol-function 'message) #'ignore))
          (agent-repl--log "main" "named probe"))
        ;; Assert
        (with-temp-buffer
          (insert-file-contents path)
          (should (string-match-p "\"pseudo_workspace\":\"main\"" (buffer-string))))))))

(ert-deftest agent-repl-test-pseudo-workspace-record-carries-no-workspace-identity ()
  "A globally-routed pseudo record must not claim a workspace sink identity."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-logfile path
      (let ((+workspaces-main "main")
            (agent-repl--unroutable-log-workspaces (make-hash-table :test #'equal)))
        ;; Act
        (cl-letf (((symbol-function 'message) #'ignore))
          (agent-repl--log "main" "identity probe"))
        ;; Assert
        (with-temp-buffer
          (insert-file-contents path)
          (should-not (string-match-p "workspace_id" (buffer-string))))))))

(ert-deftest agent-repl-test-pseudo-name-owns-no-sink-even-when-registered ()
  "A persp built-in never owns a sink, whatever got registered under its name.

THIS REVERSES A PRIOR RULE, deliberately.  The test that stood here asserted
`the pseudo screen never outranks a real registration of the same name', and
that rule is what let the defect through: on 2026-08-11 the live registry
held `main' -> \".../marcos-pr-remediation/\" and
`none' -> \".../slack-cee-ceac-integration-shj/\", so both built-ins were
routable and 60 of 60 `recovery-slo:' records in each of those two
workspaces' durable logs named the perspective instead of the workspace.
The reversal costs nothing real: persp-mode owns \"none\" and Doom owns
\"main\", so an agent-repl workspace cannot hold either name without
colliding with the perspective itself."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let* ((project (make-temp-file "agent-repl-real-main-" t))
           (+workspaces-main "main")
           (agent-repl--workspace-log-targets (make-hash-table :test #'equal)))
      (unwind-protect
          (progn
            (puthash "main" (list :project-dir project)
                     agent-repl--workspaces)
            ;; Act / Assert
            (should-not (agent-repl--ws-log-routable-p "main"))
            (should-not (agent-repl--log-sink-workspace "main")))
        (delete-directory project t)))))

(ert-deftest agent-repl-test-pseudo-name-cannot-shadow-a-real-workspace-sink ()
  "A registered pseudo sharing a real workspace's dir never wins its sink.
The shadowing this pins is the one that was measured: the reverse lookup in
`agent-repl--log-canonical-workspace' walks the registry for a dir match, and
with the pseudo routable it could return the perspective for a path that
belongs to a workspace."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let* ((project (make-temp-file "agent-repl-shadowed-" t))
           (+workspaces-main "main"))
      (unwind-protect
          (progn
            (agent-repl--ws-put "real-ws" :project-dir project)
            (puthash "main" (list :project-dir (file-name-as-directory project))
                     agent-repl--workspaces)
            ;; Act
            (let ((resolved (agent-repl--log-canonical-workspace project)))
              ;; Assert
              (should (equal "real-ws" resolved))))
        (delete-directory project t)))))

(ert-deftest agent-repl-test-log-identity-resolver-still-signals-when-called-directly ()
  "The invariant itself is unchanged; only the ladder screens before calling it.
Keeping this coverage means a direct caller that skips the screen is still
caught loudly."
  (agent-repl-test--with-clean-state
    (should-error (agent-repl--workspace-log-identity "never-registered"))))

;;;; ---- Tests: the log sink does not instrument itself ----

(ert-deftest agent-repl-test-workspace-scoped-record-emits-no-buffer-name-record ()
  "Writing one workspace-scoped record must not produce a second record.
`agent-repl--append-workspace-log' documents that it does not instrument its
own buffer resolution, but the buffer is NAMED by `agent-repl--buffer-name',
which logged — so every workspace-scoped line emitted a `buffer-name:' line
too.  In production that doubling put `buffer-name: suffix=-log' third in the
log at 71,425 records."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-logfile path
      (let ((project (make-temp-file "agent-repl-no-amplify-" t))
            (agent-repl--workspace-log-buffer-enabled t)
            (agent-repl--workspace-log-targets (make-hash-table :test #'equal)))
        (unwind-protect
            (progn
              (agent-repl--ws-put "amp-ws" :project-dir project)
              (cl-letf (((symbol-function 'message) #'ignore))
                (agent-repl--log "amp-ws" "single line"))
              (with-temp-buffer
                (insert-file-contents path)
                (should (= 0 (cl-count-if
                              (lambda (l) (string-match-p "buffer-name: suffix=-log" l))
                              (split-string (buffer-string) "\n" t))))))
          (delete-directory project t))))))

(ert-deftest agent-repl-test-buffer-name-still-logs-for-ordinary-callers ()
  "Silencing the sink's own path must not silence genuine callers."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-logfile path
      ;; `agent-repl--buffer-name' reports on the verbose rung, so the durable
      ;; threshold is opened: the subject is the sink's self-silencing, not the
      ;; level gate.
      (let ((agent-repl-log-file-level 'debug)
            (project (make-temp-file "agent-repl-still-logs-" t)))
        (unwind-protect
            (progn
              (agent-repl--ws-put "named-ws" :project-dir project)
              (cl-letf (((symbol-function 'message) #'ignore))
                (agent-repl--buffer-name "-view" "named-ws"))
              (let ((workspace-path
                     (plist-get (agent-repl--workspace-log-target-entry "named-ws")
                                :target)))
                (with-temp-buffer
                  (insert-file-contents workspace-path)
                  (should (string-match-p "buffer-name: suffix=-view"
                                          (buffer-string))))))
          (delete-directory project t))))))

;;;; ---- Tests: keyed timer registry ----

(defmacro agent-repl-test--with-timer-registry (&rest body)
  "Run BODY against fresh, isolated timer registries.
Any timer BODY armed is cancelled on the way out, so no real timer
survives into the rest of the batch run."
  (declare (indent 0))
  `(let ((agent-repl--timers nil)
         (agent-repl--keyed-timers nil))
     (unwind-protect (progn ,@body)
       (agent-repl--cancel-all-timers))))

(defun agent-repl-test--armed-timer ()
  "Return a genuinely scheduled timer that will not fire during the run."
  (run-with-timer 3600 nil #'ignore))

(defvar agent-repl-test--fake-heartbeat-arm-count 0
  "Number of times `agent-repl-test--arm-fake-heartbeat' has been called.")

(defun agent-repl-test--arm-fake-heartbeat ()
  "Arm a stand-in heartbeat under `:test-heartbeat', counting the call."
  (setq agent-repl-test--fake-heartbeat-arm-count
        (1+ agent-repl-test--fake-heartbeat-arm-count))
  (agent-repl--register-timer :test-heartbeat (agent-repl-test--armed-timer)))

(defun agent-repl-test--arm-fn-that-arms-nothing ()
  "Stand in for an arm function that runs but leaves its key un-armed."
  nil)

(ert-deftest agent-repl-test-register-timer-replaces-rather-than-stacks ()
  "Re-arming a key cancels the prior timer instead of stacking a duplicate."
  ;; Arrange
  (agent-repl-test--with-timer-registry
    (let ((first (agent-repl--register-timer :test-key (agent-repl-test--armed-timer))))
      ;; Act
      (let ((second (agent-repl--register-timer :test-key (agent-repl-test--armed-timer))))
        ;; Assert
        (should (equal 1 (length agent-repl--timers)))
        (should (equal 1 (length agent-repl--keyed-timers)))
        (should (eq second (cdr (assq :test-key agent-repl--keyed-timers))))
        (should-not (memq first timer-list))))))

(ert-deftest agent-repl-test-cancel-all-then-one-arm-per-key-yields-one-each ()
  "Cancel-all followed by a single arm per key leaves exactly one timer per key."
  ;; Arrange
  (agent-repl-test--with-timer-registry
    (agent-repl--register-timer :key-a (agent-repl-test--armed-timer))
    (agent-repl--register-timer :key-b (agent-repl-test--armed-timer))
    (agent-repl--cancel-all-timers)
    ;; Act
    (agent-repl--register-timer :key-a (agent-repl-test--armed-timer))
    (agent-repl--register-timer :key-b (agent-repl-test--armed-timer))
    ;; Assert
    (should (equal 2 (length agent-repl--timers)))
    (should (equal 2 (length agent-repl--keyed-timers)))
    (should (agent-repl--timer-armed-p :key-a))
    (should (agent-repl--timer-armed-p :key-b))))

(ert-deftest agent-repl-test-cancel-all-timers-clears-the-keyed-registry ()
  "Cancel-all empties the keyed registry, not just the flat timer list."
  ;; Arrange
  (agent-repl-test--with-timer-registry
    (agent-repl--register-timer :test-key (agent-repl-test--armed-timer))
    ;; Act
    (agent-repl--cancel-all-timers)
    ;; Assert
    (should (null agent-repl--keyed-timers))))

(ert-deftest agent-repl-test-cancel-timer-key-deregisters-only-that-key ()
  "Cancelling one key leaves every other key's timer armed."
  ;; Arrange
  (agent-repl-test--with-timer-registry
    (agent-repl--register-timer :key-a (agent-repl-test--armed-timer))
    (agent-repl--register-timer :key-b (agent-repl-test--armed-timer))
    ;; Act
    (should (agent-repl--cancel-timer-key :key-a))
    ;; Assert
    (should-not (agent-repl--timer-armed-p :key-a))
    (should (agent-repl--timer-armed-p :key-b))))

(ert-deftest agent-repl-test-register-timer-signals-on-a-non-timer ()
  "A caller handing something that is not a timer is a bug and must signal."
  ;; Arrange
  (agent-repl-test--with-timer-registry
    ;; Act / Assert
    (should-error (agent-repl--register-timer :test-key "not-a-timer") :type 'error)
    (should (null agent-repl--keyed-timers))))

(ert-deftest agent-repl-test-register-timer-signals-on-a-nil-key ()
  "A nil KEY would make the registry unaddressable, so it must signal."
  ;; Arrange
  (agent-repl-test--with-timer-registry
    (let ((timer (agent-repl-test--armed-timer)))
      (unwind-protect
          ;; Act / Assert
          (should-error (agent-repl--register-timer nil timer) :type 'error)
        (cancel-timer timer)))))

;;;; ---- Tests: heartbeat assertion ----

(ert-deftest agent-repl-test-assert-heartbeat-rearms-a-stranded-key ()
  "A key with no live timer is re-armed through its owner's arm function."
  ;; Arrange
  (agent-repl-test--with-timer-registry
    (let ((agent-repl--required-timer-keys
           '((:test-heartbeat . agent-repl-test--arm-fake-heartbeat)))
          (agent-repl-test--fake-heartbeat-arm-count 0))
      (cl-letf (((symbol-function 'agent-repl--warn) (lambda (&rest _) nil))
                ((symbol-function 'agent-repl--info) (lambda (&rest _) nil)))
        ;; Act
        (let ((result (agent-repl--assert-heartbeat-armed)))
          ;; Assert
          (should (equal '(:test-heartbeat) (plist-get result :rearmed)))
          (should (equal 1 agent-repl-test--fake-heartbeat-arm-count))
          (should (agent-repl--timer-armed-p :test-heartbeat)))))))

(ert-deftest agent-repl-test-assert-heartbeat-records-the-stranded-key-at-info ()
  "The strand is still recorded naming the key, at info rather than warn."
  ;; Arrange
  (agent-repl-test--with-timer-registry
    (let ((infos nil)
          (agent-repl--required-timer-keys
           '((:test-heartbeat . agent-repl-test--arm-fake-heartbeat)))
          (agent-repl-test--fake-heartbeat-arm-count 0))
      (cl-letf (((symbol-function 'agent-repl--warn) (lambda (&rest _) nil))
                ((symbol-function 'agent-repl--info)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) infos))))
        ;; Act
        (agent-repl--assert-heartbeat-armed)
        ;; Assert
        (should (cl-some (lambda (i)
                           (string-match-p
                            "key=:test-heartbeat outcome=stranded arm-fn=agent-repl-test--arm-fake-heartbeat action=re-arming"
                            i))
                         infos))))))

(ert-deftest agent-repl-test-assert-heartbeat-records-the-rearm-at-info ()
  "A strand that heals reports `outcome=rearmed' at info, naming the key."
  ;; Arrange
  (agent-repl-test--with-timer-registry
    (let ((infos nil)
          (agent-repl--required-timer-keys
           '((:test-heartbeat . agent-repl-test--arm-fake-heartbeat)))
          (agent-repl-test--fake-heartbeat-arm-count 0))
      (cl-letf (((symbol-function 'agent-repl--warn) (lambda (&rest _) nil))
                ((symbol-function 'agent-repl--info)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) infos))))
        ;; Act
        (agent-repl--assert-heartbeat-armed)
        ;; Assert
        (should (cl-some (lambda (i)
                           (string-match-p
                            "key=:test-heartbeat outcome=rearmed arm-fn=agent-repl-test--arm-fake-heartbeat"
                            i))
                         infos))))))

(ert-deftest agent-repl-test-assert-heartbeat-does-not-warn-on-a-healed-strand ()
  "Strand-then-rearm is self-healing, so it must raise no WARNING at all."
  ;; Arrange
  (agent-repl-test--with-timer-registry
    (let ((warnings nil)
          (agent-repl--required-timer-keys
           '((:test-heartbeat . agent-repl-test--arm-fake-heartbeat)))
          (agent-repl-test--fake-heartbeat-arm-count 0))
      (cl-letf (((symbol-function 'agent-repl--warn)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) warnings)))
                ((symbol-function 'agent-repl--info) (lambda (&rest _) nil)))
        ;; Act
        (agent-repl--assert-heartbeat-armed)
        ;; Assert
        (should (null warnings))))))

(ert-deftest agent-repl-test-assert-heartbeat-warns-when-the-rearm-fails ()
  "A key still un-armed after its arm function ran is a persisted fault: warn."
  ;; Arrange
  (agent-repl-test--with-timer-registry
    (let ((warnings nil)
          (agent-repl--required-timer-keys
           '((:test-heartbeat . agent-repl-test--arm-fn-that-arms-nothing))))
      (cl-letf (((symbol-function 'agent-repl--warn)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) warnings)))
                ((symbol-function 'agent-repl--info) (lambda (&rest _) nil)))
        ;; Act
        (let ((result (agent-repl--assert-heartbeat-armed)))
          ;; Assert
          (should (equal '(:test-heartbeat) (plist-get result :failed)))
          (should (cl-some (lambda (w)
                             (string-match-p "key=:test-heartbeat outcome=rearm-failed" w))
                           warnings)))))))

(ert-deftest agent-repl-test-assert-heartbeat-warns-on-an-unloaded-owner ()
  "An owner that cannot re-arm at all is a persisted fault: warn."
  ;; Arrange
  (agent-repl-test--with-timer-registry
    (let ((warnings nil)
          (agent-repl--required-timer-keys
           '((:test-heartbeat . agent-repl-test--arm-fn-that-does-not-exist))))
      (cl-letf (((symbol-function 'agent-repl--warn)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) warnings)))
                ((symbol-function 'agent-repl--info) (lambda (&rest _) nil)))
        ;; Act
        (agent-repl--assert-heartbeat-armed)
        ;; Assert
        (should (cl-some (lambda (w)
                           (string-match-p
                            "key=:test-heartbeat outcome=unavailable arm-fn=agent-repl-test--arm-fn-that-does-not-exist reason=owner-not-loaded"
                            w))
                         warnings))))))

(ert-deftest agent-repl-test-assert-heartbeat-is-quiet-when-all-armed ()
  "An already-armed contract produces no warning and no re-arm."
  ;; Arrange
  (agent-repl-test--with-timer-registry
    (let ((warnings nil)
          (agent-repl--required-timer-keys
           '((:test-heartbeat . agent-repl-test--arm-fake-heartbeat)))
          (agent-repl-test--fake-heartbeat-arm-count 0))
      (agent-repl--register-timer :test-heartbeat (agent-repl-test--armed-timer))
      (cl-letf (((symbol-function 'agent-repl--warn)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) warnings))))
        ;; Act
        (let ((result (agent-repl--assert-heartbeat-armed)))
          ;; Assert
          (should (equal '(:test-heartbeat) (plist-get result :armed)))
          (should (null (plist-get result :rearmed)))
          (should (equal 0 agent-repl-test--fake-heartbeat-arm-count))
          (should (null warnings)))))))

(ert-deftest agent-repl-test-assert-heartbeat-reports-an-unloaded-owner ()
  "A key whose owner file is not loaded is reported, never silently passed."
  ;; Arrange
  (agent-repl-test--with-timer-registry
    (let ((agent-repl--required-timer-keys
           '((:test-heartbeat . agent-repl-test--arm-fn-that-does-not-exist))))
      (cl-letf (((symbol-function 'agent-repl--warn) (lambda (&rest _) nil)))
        ;; Act
        (let ((result (agent-repl--assert-heartbeat-armed)))
          ;; Assert
          (should (equal '(:test-heartbeat) (plist-get result :unavailable))))))))

(ert-deftest agent-repl-test-heartbeat-assertion-defers-on-a-cold-load ()
  "A cold load (owners not yet defined) defers instead of erroring or stranding."
  ;; Arrange — this is exactly core.el's own load-time state in a fresh
  ;; batch process: core.el is evaluated before status.el and autosave.el
  ;; define the arm functions.
  (agent-repl-test--with-timer-registry
    (let ((agent-repl--required-timer-keys
           '((:test-heartbeat . agent-repl-test--arm-fn-that-does-not-exist)))
          (agent-repl--heartbeat-assert-deferral-timer nil)
          (scheduled nil))
      (cl-letf (((symbol-function 'run-with-idle-timer)
                 (lambda (&rest _) (setq scheduled t) (timer-create))))
        ;; Act
        (let ((outcome (agent-repl--assert-heartbeat-armed-when-owners-load)))
          ;; Assert
          (should (eq outcome :deferred))
          (should scheduled)
          (should (timerp agent-repl--heartbeat-assert-deferral-timer)))))))

(ert-deftest agent-repl-test-heartbeat-assertion-runs-immediately-when-owners-exist ()
  "With every arm function defined the check runs inline rather than deferring."
  ;; Arrange — a bare core.el hot-load into a RUNNING Emacs looks like this.
  (agent-repl-test--with-timer-registry
    (let ((agent-repl--required-timer-keys
           '((:test-heartbeat . agent-repl-test--arm-fake-heartbeat)))
          (agent-repl-test--fake-heartbeat-arm-count 0)
          (agent-repl--heartbeat-assert-deferral-timer nil))
      (cl-letf (((symbol-function 'agent-repl--warn) (lambda (&rest _) nil)))
        ;; Act
        (let ((outcome (agent-repl--assert-heartbeat-armed-when-owners-load)))
          ;; Assert
          (should (eq :checked (car outcome)))
          (should (equal '(:test-heartbeat) (plist-get (cdr outcome) :rearmed)))
          (should (null agent-repl--heartbeat-assert-deferral-timer)))))))

(ert-deftest agent-repl-test-every-required-timer-key-names-a-loadable-owner ()
  "A required key whose owner file does not exist can never be satisfied.
`:readiness-poll' and `:workspace-status-export' outlived readiness.el
and workspace-status-export.el, so every cold start warned
`outcome=unavailable reason=owner-not-loaded' twice, a second after the
module loaded, about jobs nothing owns any more."
  ;; Arrange: the arm functions the production sources actually define.
  (let ((defined nil))
    (dolist (file (agent-repl-test--production-lisp-files))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (re-search-forward "^(defun \\([^ \t\n()]+\\)" nil t)
          (push (intern (match-string 1)) defined))))
    ;; Act / Assert: every required key's owner is one of them.  Read off
    ;; the SOURCES, never `fboundp', because the suite's own process has
    ;; loaded files a cold start would not have reached yet.
    (dolist (entry agent-repl--required-timer-keys)
      (should (memq (cdr entry) defined)))))

(ert-deftest agent-repl-test-core-input-buffer-name-carries-the-title ()
  "The title rides after the identity segment, inside the name form."
  (should (equal (agent-repl--input-buffer-name "ws-1" "Refactor the codec")
                 "*agent-panel-input-ws-1 Refactor the codec*")))

(ert-deftest agent-repl-test-core-input-buffer-name-without-a-title-is-canonical ()
  "The row-name fallback adds nothing, so the name stays the bare one."
  (should (equal (agent-repl--input-buffer-name "ws-1" "ws-1")
                 "*agent-panel-input-ws-1*")))

(ert-deftest agent-repl-test-core-input-buffer-name-drops-asterisks-from-the-title ()
  "The asterisk is the name form's own delimiter and cannot ride a title."
  (should (equal (agent-repl--input-buffer-name "ws-1" "a*b")
                 "*agent-panel-input-ws-1 ab*")))

(ert-deftest agent-repl-test-core-titled-input-buffer-name-still-matches ()
  "A titled composer must still match the input buffer regexp."
  (should (string-match-p agent-repl--input-buffer-re
                          "*agent-panel-input-ws-1 Refactor the codec*")))


;;;; ---- Tests: the agent INPUT buffer name predicate ----
;;
;; The companion of the view predicate: together they are the two panels
;; the tab bar's full-vs-partial extent asks about (owner ruling 5).

(ert-deftest agent-repl-test-agent-input-buffer-name-p-matches-the-composer ()
  "An input composer buffer name is recognized."
  ;; Arrange / Act / Assert
  (should (agent-repl--agent-input-buffer-name-p "*agent-panel-input-my-ws*")))

(ert-deftest agent-repl-test-agent-input-buffer-name-p-matches-a-titled-composer ()
  "A composer whose name carries the daemon's display title is recognized."
  ;; Arrange / Act / Assert
  (should (agent-repl--agent-input-buffer-name-p
           "*agent-panel-input-my-ws Fix the tab bar*")))

(ert-deftest agent-repl-test-agent-input-buffer-name-p-rejects-the-webview ()
  "The webapp panel is not the input window."
  ;; Arrange / Act / Assert
  (should-not (agent-repl--agent-input-buffer-name-p "*agent-frontend-my-ws*")))

(ert-deftest agent-repl-test-agent-input-buffer-name-p-rejects-a-plain-buffer ()
  "An ordinary buffer name is not an input composer."
  ;; Arrange / Act / Assert
  (should-not (agent-repl--agent-input-buffer-name-p "*scratch*")))

(ert-deftest agent-repl-test-agent-input-buffer-name-p-rejects-a-non-string ()
  "A non-string name answers nil rather than signalling."
  ;; Arrange / Act / Assert
  (should-not (agent-repl--agent-input-buffer-name-p nil)))

(ert-deftest agent-repl-test-agent-input-buffer-p-is-the-buffer-shaped-form ()
  "The buffer-shaped form reads the buffer's own name."
  ;; Arrange
  (let ((buf (generate-new-buffer "*agent-panel-input-bufform*")))
    (unwind-protect
        ;; Act / Assert
        (should (agent-repl--agent-input-buffer-p buf))
      (kill-buffer buf))))

(provide 'test-core)

;;; test-core.el ends here

;;;; ---- Tests: durable log level threshold ----
;;
;; The knob exists because `agent-repl-debug' governs *Messages* only: the
;; verbose rung was reaching the file unconditionally, and nothing short of
;; the all-or-nothing kill-switch would stop it.  One edge per test.

(defmacro agent-repl-test--with-captured-file-writes (level &rest body)
  "Run BODY with `agent-repl-log-file-level' at LEVEL, collecting file writes."
  (declare (indent 2))
  `(let ((writes 0)
         (agent-repl-log-to-file t)
         (agent-repl-log-file-level ,level))
     (cl-letf (((symbol-function 'agent-repl--do-log-to-file)
                (lambda (&rest _args) (setq writes (1+ writes)))))
       ,@body)
     writes))

(ert-deftest agent-repl-test-log-file-level-info-drops-verbose ()
  "The info threshold drops debug-level verbose records."
  ;; Arrange / Act
  (let ((writes (agent-repl-test--with-captured-file-writes 'info
                  (agent-repl--log-verbose nil "chatter %s" "x"))))
    ;; Assert
    (should (= writes 0))))

(ert-deftest agent-repl-test-log-file-level-debug-keeps-debug ()
  "The debug threshold records ordinary debug lines."
  ;; Arrange / Act
  (let ((writes (agent-repl-test--with-captured-file-writes 'debug
                  (agent-repl--log nil "ordinary %s" "x"))))
    ;; Assert
    (should (= writes 1))))

(ert-deftest agent-repl-test-log-file-level-debug-persists-verbose ()
  "Verbose records persist with every other debug record."
  ;; Arrange / Act
  (let ((writes (agent-repl-test--with-captured-file-writes 'debug
                  (agent-repl--log-verbose nil "chatter %s" "x"))))
    ;; Assert
    (should (= writes 1))))

(ert-deftest agent-repl-test-log-file-level-warn-drops-info ()
  "A warn threshold drops the info rung beneath it."
  ;; Arrange / Act
  (let ((writes (agent-repl-test--with-captured-file-writes 'warn
                  (agent-repl--info nil "notice %s" "x"))))
    ;; Assert
    (should (= writes 0))))

(ert-deftest agent-repl-test-log-file-level-never-drops-errors ()
  "The most severe threshold still records errors."
  ;; Arrange / Act
  (let ((writes (agent-repl-test--with-captured-file-writes 'error
                  (ignore-errors (agent-repl--do-log nil "boom" nil t)))))
    ;; Assert
    (should (= writes 1))))

(ert-deftest agent-repl-test-log-file-level-rejects-an-unknown-record-level ()
  "An unrecognized record level fails instead of acquiring an implicit rank."
  ;; Arrange / Act / Assert
  (should-error
   (agent-repl--log-record-persists-p "no-such-level" "normal") :type 'error))

(ert-deftest agent-repl-test-log-file-level-verbose-follows-its-carried-level ()
  "Durable filtering uses level and never drops a debug verbose record."
  (let ((agent-repl-log-file-level 'debug))
    (should (agent-repl--log-record-persists-p "debug" "verbose"))))

(ert-deftest agent-repl-test-log-file-level-leaves-messages-visibility-alone ()
  "The durable threshold does not change what reaches *Messages*."
  ;; Arrange
  (let ((messaged nil)
        (agent-repl-log-to-file t)
        (agent-repl-log-file-level 'error)
        (agent-repl-debug 'verbose))
    (cl-letf (((symbol-function 'agent-repl--do-log-to-file) #'ignore)
              ((symbol-function 'message)
               (lambda (&rest _args) (setq messaged t))))
      ;; Act — the record is far below the durable threshold.
      (agent-repl--log-verbose nil "chatter %s" "x")
      ;; Assert — visibility is the other knob's business, and it still fired.
      (should messaged))))

;;;; ---- Tests: workspace log BUFFER threshold ----
;;
;; The buffers are read live by a human while debugging; the file is a forensic
;; record nobody reads top to bottom.  They are gated separately so a quiet
;; buffer never costs a complete file.

(defmacro agent-repl-test--with-log-buffer (ws &rest body)
  "Run BODY with WS's live log buffer enabled, returning its contents."
  (declare (indent 1))
  `(agent-repl-test--with-clean-state
     (let ((project (make-temp-file "agent-repl-buffer-level-" t))
           (buf nil))
       (unwind-protect
           (let ((agent-repl-log-to-file nil)
                 (agent-repl-debug nil))
             (agent-repl--ws-put ,ws :project-dir project)
             (let ((agent-repl--workspace-log-buffer-enabled t))
               ,@body)
             (setq buf (agent-repl--workspace-log-buffer ,ws))
             (with-current-buffer buf (buffer-string)))
         (when (buffer-live-p buf) (kill-buffer buf))
         (delete-directory project t)))))

(ert-deftest agent-repl-test-log-buffer-level-drops-verbose-by-default ()
  "Hot-path chatter stays out of the buffer a human is reading."
  ;; Arrange / Act
  (let ((contents (agent-repl-test--with-log-buffer "buf-level-ws"
                    (agent-repl--log-verbose "buf-level-ws" "chatter"))))
    ;; Assert
    (should (equal contents ""))))

(ert-deftest agent-repl-test-log-buffer-level-drops-ordinary-debug-by-default ()
  "The buffers are stricter than the file: they are for what went WRONG."
  ;; Arrange / Act
  (let ((contents (agent-repl-test--with-log-buffer "buf-level-ws"
                    (agent-repl--log "buf-level-ws" "ordinary"))))
    ;; Assert
    (should (equal contents ""))))

(ert-deftest agent-repl-test-log-buffer-level-keeps-warnings-by-default ()
  "A warning is exactly what the default threshold exists to surface."
  ;; Arrange / Act
  (let ((contents (agent-repl-test--with-log-buffer "buf-level-ws"
                    (agent-repl--warn "buf-level-ws" "something wrong"))))
    ;; Assert
    (should (string-match-p "something wrong" contents))))

(ert-deftest agent-repl-test-log-buffer-level-debug-follows-ordinary-activity ()
  "Lowering the buffer threshold puts the ordinary lines back."
  ;; Arrange / Act
  (let ((contents (let ((agent-repl-log-buffer-level 'debug))
                    (agent-repl-test--with-log-buffer "buf-level-ws"
                      (agent-repl--log "buf-level-ws" "ordinary")))))
    ;; Assert
    (should (string-match-p "ordinary" contents))))

(ert-deftest agent-repl-test-log-buffer-level-verbose-admits-chatter ()
  "Opening the buffer threshold puts the chatter back."
  ;; Arrange / Act
  (let ((contents (let ((agent-repl-log-buffer-level 'verbose))
                    (agent-repl-test--with-log-buffer "buf-level-ws"
                      (agent-repl--log-verbose "buf-level-ws" "chatter")))))
    ;; Assert
    (should (string-match-p "chatter" contents))))

(ert-deftest agent-repl-test-log-buffer-level-independent-of-the-file-level ()
  "A quiet buffer does not cost a complete file."
  ;; Arrange — the file admits a rung the buffer does not.
  (let ((writes 0)
        (agent-repl-log-to-file t)
        (agent-repl-log-file-level 'debug)
        (agent-repl-log-buffer-level 'warn))
    (cl-letf (((symbol-function 'agent-repl--do-log-to-file)
               (lambda (&rest _args) (setq writes (1+ writes)))))
      ;; Act
      (agent-repl--log nil "ordinary")
      ;; Assert — the file recorded it even though no buffer would show it.
      (should (= writes 1)))))

;;;; ---- Tests: backend-initiation phase surfacing ----
;;
;; The backend-initiation ladder (artifact builds, launchd kickstarts, the
;; daemon spawn and the runtime bounce) is the ONE background flow allowed to
;; reach the echo area, and `agent-repl--backend-phase' is its only door.  A
;; regression here is invisible by construction: the user sees nothing at all
;; while a synchronous build blocks the frame.

(ert-deftest agent-repl-test-backend-output-tail-names-an-empty-capture ()
  "An empty capture is reported as empty rather than as an absent field."
  ;; Arrange / Act
  (let ((tail (agent-repl--backend-output-tail "")))
    ;; Assert
    (should (equal tail "<no output>"))))

(ert-deftest agent-repl-test-backend-output-tail-keeps-only-the-last-lines ()
  "Only the trailing nonblank lines survive into the echo fragment."
  ;; Arrange
  (let ((output (mapconcat #'number-to-string (number-sequence 1 20) "\n")))
    ;; Act
    (let ((tail (agent-repl--backend-output-tail output 3)))
      ;; Assert
      (should (equal tail "18 / 19 / 20")))))

(ert-deftest agent-repl-test-backend-output-tail-truncates-an-overlong-line ()
  "A single enormous line is truncated so one echo line cannot be flooded."
  ;; Arrange
  (let ((output (make-string 2000 ?x)))
    ;; Act
    (let ((tail (agent-repl--backend-output-tail output)))
      ;; Assert
      (should (= (length tail) (1+ agent-repl--backend-output-tail-limit)))
      (should (string-prefix-p "…" tail)))))

(ert-deftest agent-repl-test-backend-output-tail-suffix-keeps-a-short-string-whole ()
  "A capture already inside the scan bound is passed through untouched."
  ;; Arrange
  (let ((output "one\ntwo\n"))
    ;; Act
    (let ((suffix (agent-repl--backend-output-tail-suffix output)))
      ;; Assert
      (should (eq suffix output)))))

(ert-deftest agent-repl-test-backend-output-tail-suffix-bounds-a-huge-string ()
  "An oversized capture is cut down to exactly the scan bound."
  ;; Arrange
  (let ((output (make-string (* 4 agent-repl--backend-output-tail-scan-limit) ?x)))
    ;; Act
    (let ((suffix (agent-repl--backend-output-tail-suffix output)))
      ;; Assert
      (should (= (length suffix) agent-repl--backend-output-tail-scan-limit)))))

(ert-deftest agent-repl-test-backend-output-tail-matches-its-bounded-suffix ()
  "A multi-megabyte multiline capture yields the same tail as its suffix.
The bound is what keeps the helper off the quadratic `split-string' path
that froze Emacs; this pins the equivalence the bound relies on."
  ;; Arrange — multibyte content, since char-to-byte indexing is the cost.
  (let* ((filler (mapconcat (lambda (i) (format "récord %d ————" i))
                            (number-sequence 1 120000) "\n"))
         (output (concat filler "\nlast-one\nlast-two")))
    (should (> (length output) (* 2 1024 1024)))
    ;; Act
    (let ((tail (agent-repl--backend-output-tail output))
          (suffix-tail (agent-repl--backend-output-tail
                        (agent-repl--backend-output-tail-suffix output))))
      ;; Assert
      (should (equal tail suffix-tail)))))

(ert-deftest agent-repl-test-backend-output-tail-keeps-the-final-lines-of-a-huge-capture ()
  "The trailing lines of a multi-megabyte capture still reach the echo tail."
  ;; Arrange
  (let ((output (concat (make-string (* 3 1024 1024) ?x) "\nfinal-line")))
    ;; Act
    (let ((tail (agent-repl--backend-output-tail output 1)))
      ;; Assert
      (should (equal tail "final-line")))))

(ert-deftest agent-repl-test-backend-phase-persists-a-structured-record ()
  "A phase transition reaches the durable sink at the info rung."
  ;; Arrange
  (let (records)
    (cl-letf (((symbol-function 'agent-repl--emit-log-record)
               (lambda (ws level verbosity fmt args &rest _options)
                 (push (list ws level verbosity fmt args) records)))
              ((symbol-function 'agent-repl--emit-message) #'ignore))
      ;; Act
      (agent-repl--backend-phase nil "daemon up (pid %s)" 41)
      ;; Assert
      (should (equal (car records)
                     (list nil "info" "normal" "daemon up (pid %s)" '(41)))))))

(ert-deftest agent-repl-test-backend-phase-reaches-the-echo-area ()
  "A phase transition is echoed loudly, not filed quietly like other chatter."
  ;; Arrange
  (let (emitted)
    (cl-letf (((symbol-function 'agent-repl--do-log-to-file) #'ignore)
              ((symbol-function 'agent-repl--emit-message)
               (lambda (text &optional echo) (setq emitted (cons text echo)))))
      ;; Act
      (agent-repl--backend-phase nil "daemon up (pid %s)" 41)
      ;; Assert
      (should (equal emitted (cons "agent-repl: daemon up (pid 41)" t))))))

;;;; ---- Tests: registration does not bypass sink ownership ----

(ert-deftest agent-repl-test-preregistration-record-fails-routing ()
  "A declared creation window cannot reroute a workspace-owned record.
The record is still written -- centrally, stamped with the unroutable
name -- but it never claims the unregistered workspace as its sink."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-logfile path
      (let ((agent-repl--log-preregistration-workspace "being-created")
            (agent-repl--log-context-workspace nil)
            (agent-repl--workspace-log-buffer-enabled t)
            (sinks nil))
        (cl-letf (((symbol-function 'display-warning) #'ignore)
                  ((symbol-function 'agent-repl--do-log-to-file)
                   (lambda (_record ws) (push ws sinks))))
          ;; Act
          (agent-repl--log "being-created" "creation prologue probe"))
        ;; Assert
        (should (equal sinks '(nil nil)))))))

;;;; ---- Tests: user-facing minibuffer copy ----
;;
;; The policy these cover: the echo area carries ONE short sentence, the log
;; carries that sentence AND the verbose counterpart, and no raw daemon error
;; chain ever reaches the minibuffer.

(ert-deftest agent-repl-test-user-message-echoes-prefixed-user-copy ()
  "`agent-repl--user-message' echoes the user copy under the module prefix."
  ;; Arrange
  (let ((echoed nil))
    (cl-letf (((symbol-function 'agent-repl--emit-message)
               (lambda (text &optional _echo) (setq echoed text)))
              ((symbol-function 'agent-repl--log) #'ignore))
      ;; Act
      (agent-repl--user-message nil "prompt refused — %s" '("queued for a merge"))
      ;; Assert
      (should (equal echoed
                     "agent-repl: prompt refused — queued for a merge")))))

(ert-deftest agent-repl-test-user-message-reaches-the-echo-area ()
  "The user line is the LOUD sink: it is emitted with ECHO non-nil."
  ;; Arrange
  (let ((echo-flag 'unset))
    (cl-letf (((symbol-function 'agent-repl--emit-message)
               (lambda (_text &optional echo) (setq echo-flag echo)))
              ((symbol-function 'agent-repl--log) #'ignore))
      ;; Act
      (agent-repl--user-message nil "something happened" nil)
      ;; Assert
      (should echo-flag))))

(ert-deftest agent-repl-test-user-message-does-not-double-prefix ()
  "Copy that already carries the prefix is echoed unchanged."
  ;; Arrange
  (let ((echoed nil))
    (cl-letf (((symbol-function 'agent-repl--emit-message)
               (lambda (text &optional _echo) (setq echoed text)))
              ((symbol-function 'agent-repl--log) #'ignore))
      ;; Act
      (agent-repl--user-message nil "agent-repl: already prefixed" nil)
      ;; Assert
      (should (equal echoed "agent-repl: already prefixed")))))

(ert-deftest agent-repl-test-user-message-files-both-lines-globally ()
  "A nil-workspace call files the user line AND the detail to the global sink."
  ;; Arrange
  (let* ((tmpdir (make-temp-file "agent-repl-user-message-" t))
         (logpath (expand-file-name ".agent-repl.log" tmpdir))
         (agent-repl-log-to-file t))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-repl--logfile-path) (lambda () logpath))
                  ((symbol-function 'agent-repl--emit-message) #'ignore))
          ;; Act
          (agent-repl--user-message nil "prompt refused — queued for a merge" nil
                                    :detail "ssm: invariant failed state=RENDER_STATE_MERGE_QUEUED")
          ;; Assert
          (let ((contents (with-temp-buffer
                            (insert-file-contents logpath)
                            (buffer-string))))
            (should (string-match-p "prompt refused — queued for a merge" contents))
            (should (string-match-p "RENDER_STATE_MERGE_QUEUED" contents))))
      (delete-directory tmpdir t))))

(ert-deftest agent-repl-test-user-message-files-both-lines-for-a-workspace ()
  "A workspace-scoped call files both lines through that workspace's own sink."
  (agent-repl-test--with-clean-state
    ;; Arrange
    (let* ((project (make-temp-file "agent-repl-user-message-ws-" t))
           (ws "user-copy-ws")
           (agent-repl-log-to-file nil)
           (agent-repl--workspace-log-targets (make-hash-table :test #'equal)))
      (unwind-protect
          (progn
            (agent-repl--ws-put ws :project-dir project)
            ;; Act
            (let ((agent-repl-log-to-file t))
              (cl-letf (((symbol-function 'agent-repl--emit-message) #'ignore))
                (agent-repl--user-message ws "prompt refused — queued for a merge" nil
                                          :detail "state=RENDER_STATE_MERGE_QUEUED turn_active=true")))
            ;; Assert
            (let* ((target (plist-get (agent-repl--workspace-log-target-entry ws) :target))
                   (contents (with-temp-buffer
                               (insert-file-contents target)
                               (buffer-string))))
              (should (string-match-p "prompt refused — queued for a merge" contents))
              (should (string-match-p "state=RENDER_STATE_MERGE_QUEUED turn_active=true"
                                      contents))))
        (delete-directory project t)))))

(ert-deftest agent-repl-test-user-message-nil-detail-files-only-the-user-line ()
  "With no DETAIL there is no second record to write."
  ;; Arrange
  (let ((logged nil))
    (cl-letf (((symbol-function 'agent-repl--emit-message) #'ignore)
              ((symbol-function 'agent-repl--log)
               (lambda (ws fmt &rest args) (push (list ws (apply #'format fmt args)) logged))))
      ;; Act
      (agent-repl--user-message "ws" "nothing verbose here" nil)
      ;; Assert
      (should (equal logged '(("ws" "user-message: agent-repl: nothing verbose here")))))))

(ert-deftest agent-repl-test-user-message-empty-detail-files-only-the-user-line ()
  "An empty DETAIL string is treated as no detail, not as a blank record."
  ;; Arrange
  (let ((logged nil))
    (cl-letf (((symbol-function 'agent-repl--emit-message) #'ignore)
              ((symbol-function 'agent-repl--log)
               (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logged))))
      ;; Act
      (agent-repl--user-message nil "terse" nil :detail "")
      ;; Assert
      (should (= 1 (length logged))))))

(ert-deftest agent-repl-test-user-message-non-string-format-is-captured-not-signalled ()
  "A caller bug is captured the way the logging ladder captures it, not signalled."
  ;; Arrange
  (let ((captured nil)
        (echoed nil))
    (cl-letf (((symbol-function 'agent-repl--log-format-capture-bug)
               (lambda (fmt) (setq captured fmt)))
              ((symbol-function 'agent-repl--log) #'ignore)
              ((symbol-function 'agent-repl--emit-message)
               (lambda (text &optional _echo) (setq echoed text))))
      ;; Act
      (agent-repl--user-message nil '(not a string) nil)
      ;; Assert
      (should (equal captured '(not a string)))
      (should (string-prefix-p "agent-repl: " echoed)))))

;;;; ---- Tests: daemon-error translation ----

(ert-deftest agent-repl-test-user-copy-translates-a-merge-queued-refusal ()
  "The observed merge-queued nack becomes the queued-for-a-merge sentence."
  ;; Arrange
  (let ((raw (concat "session-controller: synchronous state publication failed before "
                     "submitting for workspace \"/tmp/ws\" session \"s_ea\" request \"fe-79\": "
                     "ssm: synchronous prompt state invariant failed for workspace \"/tmp/ws\" "
                     "session \"s_ea\" request \"fe-79\": "
                     "state=RENDER_STATE_MERGE_QUEUED turn_active=true")))
    ;; Act
    (let ((copy (agent-repl--user-copy-for-error raw "prompt")))
      ;; Assert
      (should (equal copy
                     (concat "prompt refused — this workspace is queued for a merge; "
                             "wait for the merge to finish or interrupt it"))))))

(ert-deftest agent-repl-test-user-copy-never-quotes-the-raw-chain ()
  "The translated sentence carries none of the raw chain's evidence."
  ;; Arrange
  (let ((raw "ssm: invariant failed for workspace \"/tmp/ws\": state=RENDER_STATE_MERGE_QUEUED"))
    ;; Act
    (let ((copy (agent-repl--user-copy-for-error raw "prompt")))
      ;; Assert
      (should-not (string-match-p "RENDER_STATE" copy))
      (should-not (string-match-p "ssm:" copy)))))

(ert-deftest agent-repl-test-user-copy-translates-a-merge-lease-refusal ()
  "A merge-lease refusal says a merge run owns the session."
  ;; Arrange
  (let ((raw (concat "session-controller: the workspace's session is held by a merge "
                     "exclusivity lease and cannot be hibernated: workspace \"/tmp/ws\"")))
    ;; Act
    (let ((copy (agent-repl--user-copy-for-error raw "hibernate")))
      ;; Assert
      (should (equal copy
                     (concat "hibernate refused — a merge run owns this session; "
                             "wait for the merge to finish or interrupt it"))))))

(ert-deftest agent-repl-test-user-copy-translates-a-not-live-refusal ()
  "A workspace with no live session gets the not-live sentence."
  ;; Act
  (let ((copy (agent-repl--user-copy-for-error
               "shimclient: request nacked: workspace \"doom\" has no live session to drive"
               "prompt")))
    ;; Assert
    (should (equal copy
                   "prompt refused — this workspace is not live; start or wake it first"))))

(ert-deftest agent-repl-test-user-copy-translates-a-reconnect-supersession ()
  "A superseded reconnect promises a resync rather than reporting a fault."
  ;; Act
  (let ((copy (agent-repl--user-copy-for-error
               "session-controller: command superseded by the current workspace generation"
               "reconnect")))
    ;; Assert
    (should (equal copy
                   (concat "reconnect superseded — a newer connection owns this "
                           "workspace; the view will resync")))))

(ert-deftest agent-repl-test-user-copy-translates-a-missing-transcript-refusal ()
  "A resume target with no transcript names the transcript, not the daemon."
  ;; Act
  (let ((copy (agent-repl--user-copy-for-error
               "createSession: resume target s_1 has no transcript" "resume")))
    ;; Assert
    (should (equal copy
                   (concat "resume refused — the conversation being resumed has no "
                           "transcript on disk")))))

(ert-deftest agent-repl-test-user-copy-falls-back-to-the-generic-sentence ()
  "An unrecognized error names the verb and points at the log."
  ;; Act
  (let ((copy (agent-repl--user-copy-for-error
               "shimclient: dial unix /tmp/x.sock: connect: connection refused"
               "hibernate")))
    ;; Assert
    (should (equal copy "hibernate failed — see the workspace log for detail"))))

(ert-deftest agent-repl-test-user-copy-generic-sentence-omits-the-raw-chain ()
  "The generic sentence never quotes the error it could not classify."
  ;; Arrange
  (let ((raw "shimclient: dial unix /tmp/x.sock: connect: connection refused"))
    ;; Act
    (let ((copy (agent-repl--user-copy-for-error raw "hibernate")))
      ;; Assert
      (should-not (string-match-p "shimclient" copy)))))

(ert-deftest agent-repl-test-user-copy-degrades-a-missing-verb ()
  "A nil VERB yields a whole sentence rather than one with a hole in it."
  ;; Act
  (let ((copy (agent-repl--user-copy-for-error "unclassifiable" nil)))
    ;; Assert
    (should (equal copy "the command failed — see the workspace log for detail"))))

(ert-deftest agent-repl-test-user-copy-classifies-nil-error-text-generically ()
  "A refusal with no error text at all still produces the generic sentence."
  ;; Act
  (let ((copy (agent-repl--user-copy-for-error nil "prompt")))
    ;; Assert
    (should (equal copy "prompt failed — see the workspace log for detail"))))

(ert-deftest agent-repl-test-user-copy-matches-case-insensitively ()
  "Pattern matching does not depend on the daemon's capitalization."
  ;; Act
  (let ((copy (agent-repl--user-copy-for-error "Workspace has NO LIVE SESSION" "prompt")))
    ;; Assert
    (should (equal copy
                   "prompt refused — this workspace is not live; start or wake it first"))))

;;;; ---- Tests: one-shot settle latch ----
;;
;; The latch is what makes an asynchronous operation's settle atomic with
;; respect to `C-g'.  Each test below pins one half of that: exactly one
;; claimant, no timer outliving the operation, and the whole claim running
;; with quit held off.

(ert-deftest agent-repl-test-latch-claim-succeeds-once ()
  "Exactly one caller may claim a latch."
  ;; Arrange
  (let ((latch (agent-repl--make-latch)))
    ;; Act / Assert
    (should (agent-repl--latch-claim latch))
    (should-not (agent-repl--latch-claim latch))))

(ert-deftest agent-repl-test-latch-claim-cancels-held-timers ()
  "Claiming a latch cancels every timer it holds.
A deadline surviving its own operation is exactly the stranded timer the
heartbeat assertion reports."
  ;; Arrange
  (let* ((latch (agent-repl--make-latch))
         (timer (run-with-timer 3600 nil #'ignore)))
    (agent-repl--latch-set-timer latch 'deadline timer)
    (should (memq timer timer-list))
    ;; Act
    (agent-repl--latch-claim latch)
    ;; Assert
    (should-not (memq timer timer-list))))

(ert-deftest agent-repl-test-latch-claim-runs-cleanup-once ()
  "The winning claim runs CLEANUP; later claims run nothing."
  ;; Arrange
  (let* ((runs 0)
         (latch (agent-repl--make-latch (lambda () (setq runs (1+ runs))))))
    ;; Act
    (agent-repl--latch-claim latch)
    (agent-repl--latch-claim latch)
    ;; Assert
    (should (equal runs 1))))

(ert-deftest agent-repl-test-latch-claim-inhibits-quit-across-cleanup ()
  "CLEANUP runs with quit inhibited, so a `C-g' cannot split the settle."
  ;; Arrange
  (let (observed)
    (let ((latch (agent-repl--make-latch (lambda () (setq observed inhibit-quit)))))
      ;; Act
      (agent-repl--latch-claim latch))
    ;; Assert
    (should observed)))

(ert-deftest agent-repl-test-latch-set-timer-replaces-same-key ()
  "Re-arming a key cancels its predecessor rather than accumulating beside it.
The create's 20Hz view poll re-arms every tick, so a list would grow one
entry per tick for the whole bring-up."
  ;; Arrange
  (let* ((latch (agent-repl--make-latch))
         (first (run-with-timer 3600 nil #'ignore))
         (second (run-with-timer 3600 nil #'ignore)))
    (agent-repl--latch-set-timer latch 'poll first)
    ;; Act
    (agent-repl--latch-set-timer latch 'poll second)
    ;; Assert
    (should-not (memq first timer-list))
    (should (memq second timer-list))
    (should (equal (length (agent-repl--latch-timers latch)) 1))
    ;; Cleanup
    (agent-repl--latch-claim latch)))

(ert-deftest agent-repl-test-latch-set-timer-drops-a-late-timer ()
  "A timer armed after the settle is cancelled instead of recorded."
  ;; Arrange
  (let* ((latch (agent-repl--make-latch))
         (late (run-with-timer 3600 nil #'ignore)))
    (agent-repl--latch-claim latch)
    ;; Act
    (should-not (agent-repl--latch-set-timer latch 'poll late))
    ;; Assert
    (should-not (memq late timer-list))
    (should-not (agent-repl--latch-timers latch))))

(ert-deftest agent-repl-test-latch-set-timer-rejects-a-non-latch ()
  "A non-latch first argument is a programming error, not a coped-with case."
  (should-error (agent-repl--latch-set-timer 'not-a-latch 'poll nil)))

(ert-deftest agent-repl-test-latch-claim-rejects-a-non-latch ()
  "Claiming something that is not a latch signals."
  (should-error (agent-repl--latch-claim 'not-a-latch)))
;;;; ---- Tests: deferred quit around asynchronous critical sections ----

(ert-deftest agent-repl-test-deferred-quit-inhibits-quitting-inside-the-body ()
  "The guarded body observes `inhibit-quit' bound, so a C-g cannot land in it."
  ;; Arrange
  (let ((observed 'unset))
    ;; Act
    (agent-repl--with-deferred-quit "test"
      (setq observed inhibit-quit))
    ;; Assert
    (should (eq observed t))))

(ert-deftest agent-repl-test-deferred-quit-returns-the-body-value ()
  "The guard is transparent to the body's value."
  ;; Act
  (let ((value (agent-repl--with-deferred-quit "test" 41 (1+ 41))))
    ;; Assert
    (should (equal value 42))))

(ert-deftest agent-repl-test-deferred-quit-re-arms-a-quit-for-the-command-loop ()
  "A C-g raised inside the body survives in `quit-flag' after the guard exits."
  ;; Act / Assert -- the helper makes the observation the command loop would.
  (should (agent-repl-test--quit-deferred-p
            (agent-repl--with-deferred-quit "test"
              ;; What Emacs itself does when C-g arrives under `inhibit-quit'.
              (setq quit-flag t)))))

(ert-deftest agent-repl-test-deferred-quit-records-the-deferral ()
  "A deferred quit is explainable from the canonical log alone."
  ;; Arrange
  (let ((logged nil))
    (cl-letf (((symbol-function 'agent-repl--log)
               (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logged))))
      ;; Act
      (agent-repl-test--quit-deferred-p
        (agent-repl--with-deferred-quit "uds-filter"
          (setq quit-flag t))))
    ;; Assert
    (should (seq-find (lambda (line)
                        (and (string-match-p "deferred-quit" line)
                             (string-match-p "uds-filter" line)))
                      logged))))

(ert-deftest agent-repl-test-deferred-quit-stays-silent-without-a-quit ()
  "No quit means no deferral record — the guard is quiet on the ordinary path."
  ;; Arrange
  (let ((quit-flag nil)
        (logged nil))
    (cl-letf (((symbol-function 'agent-repl--log)
               (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logged))))
      ;; Act
      (agent-repl--with-deferred-quit "uds-filter" t))
    ;; Assert
    (should-not (seq-find (lambda (line) (string-match-p "deferred-quit" line))
                          logged))))

(ert-deftest agent-repl-test-deferred-quit-propagates-an-error-from-the-body ()
  "The guard defers quits only; a signalled error still reaches the caller."
  ;; Act / Assert
  (should-error (agent-repl--with-deferred-quit "test"
                  (error "boom"))
                :type 'error))

;;;; ---- Tests: a deferred quit is AUDITED, and NEVER taken down ----------
;;
;; Arming `quit-flag' IS the deferral, whole.  The failure these cover is the
;; 2026-09-13 one: a zero-delay timer that cleared the flag and then tried to
;; abort the standing minibuffer itself, whose `abort-minibuffers' signalled
;; `Not in a minibuffer' from the timer's own current buffer and whose error
;; `timer-event-handler' reduced to a message.  The quit was dropped.  The
;; invariant is that nothing armed out of a guarded section may write the
;; flag at all.

(ert-deftest agent-repl-test-deferred-quit-arms-an-audit-for-the-command-loop ()
  "A quit deferred out of a guarded section arms an audit naming it."
  ;; Arrange
  (let ((armed nil))
    (cl-letf (((symbol-function 'agent-repl--deferred-quit-arm-audit)
               (lambda (context) (push context armed))))
      ;; Act
      (agent-repl-test--quit-deferred-p
        (agent-repl--with-deferred-quit "uds-filter"
          (setq quit-flag t))))
    ;; Assert
    (should (equal armed '("uds-filter")))))

(ert-deftest agent-repl-test-deferred-quit-arms-no-audit-without-a-quit ()
  "No quit means no audit -- the guard is inert on the ordinary path."
  ;; Arrange
  (let ((quit-flag nil)
        (armed nil))
    (cl-letf (((symbol-function 'agent-repl--deferred-quit-arm-audit)
               (lambda (context) (push context armed))))
      ;; Act
      (agent-repl--with-deferred-quit "uds-filter" t))
    ;; Assert
    (should-not armed)))

(ert-deftest agent-repl-test-deferred-quit-audit-is-armed-on-a-zero-delay-timer ()
  "The audit goes through a timer: the first Lisp context after the section."
  ;; Arrange
  (let ((scheduled nil))
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (delay repeat function &rest args)
                 (setq scheduled (list delay repeat function args)))))
      ;; Act
      (agent-repl--deferred-quit-arm-audit "uds-filter")
      ;; Assert
      (should (equal scheduled
                     (list 0 nil #'agent-repl--deferred-quit-audit
                           '("uds-filter")))))))

(ert-deftest agent-repl-test-deferred-quit-audit-leaves-an-armed-quit-armed ()
  "THE INVARIANT: a quit still owed when the audit runs is still owed after."
  ;; Act / Assert -- the shared recipe stands in for the C-g already pressed.
  (should (eq t (agent-repl-test--with-pending-quit
                  (agent-repl--deferred-quit-audit "webview-precreate-drain")
                  quit-flag))))

(ert-deftest agent-repl-test-deferred-quit-audit-leaves-an-unarmed-flag-down ()
  "A quit already honoured before the audit is not re-armed by it."
  ;; Arrange
  (let ((quit-flag nil))
    (cl-letf (((symbol-function 'agent-repl--log) #'ignore))
      ;; Act
      (agent-repl--deferred-quit-audit "webview-precreate-drain"))
    ;; Assert
    (should-not quit-flag)))

(ert-deftest agent-repl-test-deferred-quit-audit-records-a-quit-still-owed ()
  "A still-armed quit is explainable from the canonical log alone."
  ;; Arrange
  (let ((logged nil))
    (cl-letf (((symbol-function 'agent-repl--log)
               (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logged))))
      ;; Act
      (agent-repl-test--with-pending-quit
        (agent-repl--deferred-quit-audit "webview-precreate-drain")))
    ;; Assert
    (should (seq-find (lambda (line)
                        (string-match-p "still armed for the command loop" line))
                      logged))))

(ert-deftest agent-repl-test-deferred-quit-audit-records-a-quit-already-honoured ()
  "An unarmed flag at audit time is recorded as honoured, not as silence."
  ;; Arrange
  (let ((quit-flag nil)
        (logged nil))
    (cl-letf (((symbol-function 'agent-repl--log)
               (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logged))))
      ;; Act
      (agent-repl--deferred-quit-audit "webview-precreate-drain"))
    ;; Assert
    (should (seq-find (lambda (line)
                        (string-match-p "honoured before the audit ran" line))
                      logged))))

(ert-deftest agent-repl-test-deferred-quit-requeue-clears-the-flag ()
  "A prompt standing means the flag comes DOWN -- the quit is now an event."
  ;; Arrange
  (let ((unread-command-events nil))
    (cl-letf (((symbol-function 'active-minibuffer-window) (lambda () 'window))
              ((symbol-function 'agent-repl--log) #'ignore))
      ;; Act / Assert -- nil is "no quit left armed" from the shared recipe.
      (should-not (agent-repl-test--quit-deferred-p
                    (agent-repl--with-deferred-quit "webview-precreate-drain"
                      (setq quit-flag t)))))))

(ert-deftest agent-repl-test-deferred-quit-requeue-queues-the-quit-character ()
  "The quit goes back into the input stream as the key the user pressed."
  ;; Arrange
  (let ((unread-command-events nil))
    (cl-letf (((symbol-function 'active-minibuffer-window) (lambda () 'window))
              ((symbol-function 'agent-repl--log) #'ignore))
      ;; Act
      (agent-repl-test--quit-deferred-p
        (agent-repl--with-deferred-quit "webview-precreate-drain"
          (setq quit-flag t))))
    ;; Assert
    (should (equal unread-command-events (list ?\C-g)))))

(ert-deftest agent-repl-test-deferred-quit-requeue-uses-the-terminals-quit-char ()
  "A user who moved their quit character gets THAT key back, not `C-g'."
  ;; Arrange
  (let ((unread-command-events nil))
    (cl-letf (((symbol-function 'active-minibuffer-window) (lambda () 'window))
              ((symbol-function 'current-input-mode)
               (lambda () (list nil nil nil ?\C-x)))
              ((symbol-function 'agent-repl--log) #'ignore))
      ;; Act
      (agent-repl-test--quit-deferred-p
        (agent-repl--with-deferred-quit "webview-precreate-drain"
          (setq quit-flag t))))
    ;; Assert
    (should (equal unread-command-events (list ?\C-x)))))

(ert-deftest agent-repl-test-deferred-quit-requeue-records-the-requeue ()
  "Which hand-off was taken is answerable from the canonical log alone."
  ;; Arrange
  (let ((unread-command-events nil)
        (logged nil))
    (cl-letf (((symbol-function 'active-minibuffer-window) (lambda () 'window))
              ((symbol-function 'agent-repl--log)
               (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logged))))
      ;; Act
      (agent-repl-test--quit-deferred-p
        (agent-repl--with-deferred-quit "webview-precreate-drain"
          (setq quit-flag t))))
    ;; Assert
    (should (seq-find (lambda (line) (string-match-p "requeued the quit" line))
                      logged))))

(ert-deftest agent-repl-test-deferred-quit-no-minibuffer-keeps-the-flag-armed ()
  "With no prompt standing the command loop is the right taker, so nothing moves."
  ;; Arrange
  (let ((unread-command-events nil))
    (cl-letf (((symbol-function 'active-minibuffer-window) (lambda () nil))
              ((symbol-function 'agent-repl--log) #'ignore))
      ;; Act / Assert
      (should (agent-repl-test--quit-deferred-p
                (agent-repl--with-deferred-quit "uds-filter"
                  (setq quit-flag t)))))))

(ert-deftest agent-repl-test-deferred-quit-no-minibuffer-queues-nothing ()
  "A `C-g' aimed at no prompt has no keymap to be requeued into."
  ;; Arrange
  (let ((unread-command-events nil))
    (cl-letf (((symbol-function 'active-minibuffer-window) (lambda () nil))
              ((symbol-function 'agent-repl--log) #'ignore))
      ;; Act
      (agent-repl-test--quit-deferred-p
        (agent-repl--with-deferred-quit "uds-filter"
          (setq quit-flag t))))
    ;; Assert
    (should-not unread-command-events)))

(ert-deftest agent-repl-test-deferred-quit-without-a-quit-queues-nothing ()
  "No quit means nothing is put back -- the guard is inert on the ordinary path."
  ;; Arrange
  (let ((quit-flag nil)
        (unread-command-events nil))
    (cl-letf (((symbol-function 'active-minibuffer-window) (lambda () 'window))
              ((symbol-function 'agent-repl--log) #'ignore))
      ;; Act
      (agent-repl--with-deferred-quit "webview-precreate-drain" t))
    ;; Assert
    (should-not unread-command-events)))

(ert-deftest agent-repl-test-deferred-quit-audit-requeues-a-prompt-raised-since ()
  "A prompt that went up between the section and the timer still gets the key."
  ;; Arrange -- the flag survived the section because no prompt stood then.
  (let ((unread-command-events nil))
    (cl-letf (((symbol-function 'active-minibuffer-window) (lambda () 'window))
              ((symbol-function 'agent-repl--log) #'ignore))
      ;; Act
      (agent-repl-test--with-pending-quit
        (agent-repl--deferred-quit-audit "webview-precreate-drain")))
    ;; Assert
    (should (equal unread-command-events (list ?\C-g)))))

(ert-deftest agent-repl-test-deferred-quit-audit-does-not-escape-as-an-error ()
  "From a timer the audit must not signal: `timer-event-handler' eats errors."
  ;; Arrange -- exactly how a timer runs it: current buffer is NOT a minibuffer.
  (let ((raised nil))
    (cl-letf (((symbol-function 'agent-repl--log) #'ignore))
      ;; Act
      (setq raised (agent-repl-test--with-pending-quit
                     (condition-case err
                         (progn (save-current-buffer
                                  (agent-repl--deferred-quit-audit "uds-filter"))
                                nil)
                       (error err)))))
    ;; Assert
    (should-not raised)))

;;;; ---- Tests: a workspace's directory is canonicalized to its registry key ----

(ert-deftest agent-repl-test-log-workspace-dir-routes-to-its-registered-name ()
  "A record naming a live workspace by its DIRECTORY routes to that workspace.
Regression: a caller holding the worktree path rather than the workspace
name made a live, registered workspace look unroutable."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((project (make-temp-file "agent-repl-dirkey-" t)))
      (unwind-protect
          (progn
            (agent-repl--ws-put "dirkey-ws" :project-dir project)
            ;; Act / Assert
            (should (equal "dirkey-ws" (agent-repl--log-sink-workspace project))))
        (delete-directory project t)))))

(ert-deftest agent-repl-test-log-workspace-dir-does-not-warn-as-unroutable ()
  "Routing by directory must not emit the unroutable-workspace warning."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-logfile path
      (let ((project (make-temp-file "agent-repl-dirkey-quiet-" t))
            (agent-repl--unroutable-log-workspaces (make-hash-table :test #'equal)))
        (unwind-protect
            (progn
              (agent-repl--ws-put "dirkey-quiet-ws" :project-dir project)
              ;; Act
              (cl-letf (((symbol-function 'message) #'ignore))
                (agent-repl--log project "probe by directory"))
              ;; Assert
              (with-temp-buffer
                (insert-file-contents path)
                (should-not (string-match-p "unroutable log workspace"
                                            (buffer-string)))))
          (delete-directory project t))))))

(ert-deftest agent-repl-test-log-workspace-dir-with-trailing-slash-resolves ()
  "The registry key is reached through the shared path canonicalizer.
A trailing slash is the same directory, so it must resolve to the same
workspace name rather than to a second, unroutable spelling."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((project (make-temp-file "agent-repl-dirkey-slash-" t)))
      (unwind-protect
          (progn
            (agent-repl--ws-put "dirkey-slash-ws" :project-dir project)
            ;; Act / Assert
            (should (equal "dirkey-slash-ws"
                           (agent-repl--log-sink-workspace
                            (file-name-as-directory project)))))
        (delete-directory project t)))))

(ert-deftest agent-repl-test-unknown-directory-is-still-not-adopted-as-a-sink ()
  "Canonicalization must not accept a genuinely unknown directory.
It owns no sink, so its records go centrally under its own name rather than
into some other workspace's log."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-logfile path
      (let ((agent-repl--unroutable-log-workspaces (make-hash-table :test #'equal)))
        ;; Act
        (cl-letf (((symbol-function 'display-warning) #'ignore))
          (agent-repl--log "/no/such/worktree/anywhere" "probe"))
        ;; Assert
        (let ((record (agent-repl-test--log-record-for path "log-central-fallback")))
          (should (equal (alist-get 'unroutable_workspace record)
                         "/no/such/worktree/anywhere")))))))

(ert-deftest agent-repl-test-tombstoned-workspace-dir-is-not-adopted ()
  "A dead workspace's preserved directory must not claim the record.
`agent-repl--ws-log-routable-p' is the gate, so an entry whose worktree is
gone cannot silently swallow a directory-spelled record."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((project (make-temp-file "agent-repl-dirkey-tomb-" t)))
      (agent-repl--ws-put "dirkey-tomb-ws" :project-dir project)
      (delete-directory project t)
      ;; Act / Assert
      (should-error (agent-repl--log-sink-workspace project)))))

;;;; ---- Tests: one directory owns one durable target and one canonical link ----
;;
;; The registry used to be keyed by workspace NAME, so two names resolving to
;; one directory each installed their own external target and each overwrote
;; the other's `emacs.log' symlink.  The loser then wrote, forever, to a temp
;; file nothing on disk pointed at.  Measured 2026-08-11: `main' and
;; `marcos-pr-remediation' held two different targets for the same
;; `:project-dir' and the same `workspace_id', the link pointed at the
;; pseudo's, and a day of the real workspace's records were invisible in the
;; file every reader opens.

(ert-deftest agent-repl-test-two-names-for-one-directory-share-one-target ()
  "A second name for the same directory adopts the target already installed."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let* ((project (make-temp-file "agent-repl-one-target-" t))
           (agent-repl--workspace-log-targets (make-hash-table :test #'equal)))
      (unwind-protect
          (progn
            (agent-repl--ws-put "name-a" :project-dir project)
            (puthash "name-b" (gethash "name-a" agent-repl--workspaces)
                     agent-repl--workspaces)
            ;; Act
            (let ((first (agent-repl--workspace-emacs-log-target "name-a"))
                  (second (agent-repl--workspace-emacs-log-target "name-b")))
              ;; Assert
              (should (equal first second))))
        (delete-directory project t)))))

(ert-deftest agent-repl-test-one-directory-installs-exactly-one-registry-entry ()
  "Two names for one directory must not produce two registry entries.
Two entries is what let one of them own the symlink while the other wrote to
an orphan."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let* ((project (make-temp-file "agent-repl-one-entry-" t))
           (agent-repl--workspace-log-targets (make-hash-table :test #'equal)))
      (unwind-protect
          (progn
            (agent-repl--ws-put "name-a" :project-dir project)
            (puthash "name-b" (gethash "name-a" agent-repl--workspaces)
                     agent-repl--workspaces)
            ;; Act
            (agent-repl--workspace-emacs-log-target "name-a")
            (agent-repl--workspace-emacs-log-target "name-b")
            ;; Assert
            (should (= 1 (hash-table-count agent-repl--workspace-log-targets))))
        (delete-directory project t)))))

(ert-deftest agent-repl-test-second-name-does-not-repoint-the-canonical-link ()
  "The canonical `emacs.log' link keeps pointing at the one shared target."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let* ((project (make-temp-file "agent-repl-one-link-" t))
           (agent-repl--workspace-log-targets (make-hash-table :test #'equal)))
      (unwind-protect
          (progn
            (agent-repl--ws-put "name-a" :project-dir project)
            (puthash "name-b" (gethash "name-a" agent-repl--workspaces)
                     agent-repl--workspaces)
            (let ((first (agent-repl--workspace-emacs-log-target "name-a")))
              ;; Act
              (agent-repl--workspace-emacs-log-target "name-b")
              ;; Assert
              (should (equal first
                             (file-symlink-p
                              (agent-repl--workspace-emacs-log-path project))))))
        (delete-directory project t)))))

(ert-deftest agent-repl-test-reuse-repoints-a-stolen-canonical-link ()
  "A reuse of the owned target re-establishes a canonical link that was stolen.
Another runtime registering the same directory replaces the symlink; the
registry keeps returning the owned target, so every record written after
the theft would land in a file no reader opens."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let* ((project (make-temp-file "agent-repl-stolen-link-" t))
           (agent-repl--workspace-log-targets (make-hash-table :test #'equal)))
      (unwind-protect
          (let (target interloper canonical)
            (agent-repl--ws-put "ws" :project-dir project)
            (setq target (agent-repl--workspace-emacs-log-target "ws")
                  canonical (agent-repl--workspace-emacs-log-path project)
                  interloper (make-temp-file "agent-repl-interloper-" nil ".log"))
            (make-symbolic-link interloper canonical t)
            ;; Act
            (agent-repl--workspace-emacs-log-target "ws")
            ;; Assert
            (should (equal target (file-symlink-p canonical)))
            (delete-file interloper))
        (delete-directory project t)))))

(ert-deftest agent-repl-test-reuse-restores-a-deleted-canonical-link ()
  "A reuse of the owned target re-creates a canonical link that was removed."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let* ((project (make-temp-file "agent-repl-deleted-link-" t))
           (agent-repl--workspace-log-targets (make-hash-table :test #'equal)))
      (unwind-protect
          (let (target canonical)
            (agent-repl--ws-put "ws" :project-dir project)
            (setq target (agent-repl--workspace-emacs-log-target "ws")
                  canonical (agent-repl--workspace-emacs-log-path project))
            (delete-file canonical)
            ;; Act
            (agent-repl--workspace-emacs-log-target "ws")
            ;; Assert
            (should (equal target (file-symlink-p canonical))))
        (delete-directory project t)))))

(ert-deftest agent-repl-test-reuse-keeps-an-intact-canonical-link-untouched ()
  "A reuse leaves an already-correct canonical link exactly as it stands."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let* ((project (make-temp-file "agent-repl-intact-link-" t))
           (agent-repl--workspace-log-targets (make-hash-table :test #'equal)))
      (unwind-protect
          (let (target canonical attrs)
            (agent-repl--ws-put "ws" :project-dir project)
            (setq target (agent-repl--workspace-emacs-log-target "ws")
                  canonical (agent-repl--workspace-emacs-log-path project)
                  attrs (file-attributes canonical))
            ;; Act
            (agent-repl--workspace-emacs-log-target "ws")
            ;; Assert
            (should (equal target (file-symlink-p canonical)))
            (should (equal (file-attribute-inode-number attrs)
                           (file-attribute-inode-number
                            (file-attributes canonical)))))
        (delete-directory project t)))))

(ert-deftest agent-repl-test-log-link-current-p-rejects-a-plain-file ()
  "A canonical path that is a regular file is not a current link."
  ;; Arrange
  (let ((plain (make-temp-file "agent-repl-plain-canonical-" nil ".log")))
    (unwind-protect
        ;; Act / Assert
        (should-not (agent-repl--workspace-log-link-current-p plain plain))
      (delete-file plain))))

(ert-deftest agent-repl-test-a-different-directory-gets-its-own-target ()
  "The sharing is scoped to one identity; two directories stay independent."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let* ((one (make-temp-file "agent-repl-dir-one-" t))
           (two (make-temp-file "agent-repl-dir-two-" t))
           (agent-repl--workspace-log-targets (make-hash-table :test #'equal)))
      (unwind-protect
          (progn
            (agent-repl--ws-put "ws-one" :project-dir one)
            (agent-repl--ws-put "ws-two" :project-dir two)
            ;; Act
            (let ((a (agent-repl--workspace-emacs-log-target "ws-one"))
                  (b (agent-repl--workspace-emacs-log-target "ws-two")))
              ;; Assert
              (should-not (equal a b))))
        (delete-directory one t)
        (delete-directory two t)))))

(ert-deftest agent-repl-test-target-key-separates-identities-that-concatenate ()
  "The key's NUL join makes two identities unable to collide by concatenation."
  ;; Arrange / Act
  (let ((a (agent-repl--workspace-log-target-key
            (list :workspace-dir-hash "ab" :project-dir "/c")))
        (b (agent-repl--workspace-log-target-key
            (list :workspace-dir-hash "a" :project-dir "b/c"))))
    ;; Assert
    (should-not (equal a b))))

(ert-deftest agent-repl-test-target-key-changes-when-the-dir-hash-rebinds ()
  "A different workspace at the same path is a different sink."
  ;; Arrange / Act
  (let ((a (agent-repl--workspace-log-target-key
            (list :workspace-dir-hash "one" :project-dir "/p")))
        (b (agent-repl--workspace-log-target-key
            (list :workspace-dir-hash "two" :project-dir "/p"))))
    ;; Assert
    (should-not (equal a b))))

(ert-deftest agent-repl-test-target-entry-is-nil-for-a-name-owning-no-sink ()
  "The name-shaped accessor answers nil rather than reaching a signalling resolver."
  ;; Arrange / Act / Assert
  (agent-repl-test--with-clean-state
    (should-not (agent-repl--workspace-log-target-entry "never-registered"))))

;;;; ---- Tests: wait-for-process-exit ----
;;
;; `agent-repl--capture-process-output' — the shared implementation behind
;; every `agent-repl--git-*' / `--gh-*' wrapper — calls
;; `agent-repl--wait-for-process-exit'.  That definition was deleted as
;; collateral when worktree.el was slimmed, leaving every real capture path
;; signalling `void-function'; the whole suite stayed green because every
;; capture test stubs the wait.  The first test below pins the definition's
;; existence so the seam cannot silently reopen.
;;
;; No real process is spawned anywhere here: the process primitives are
;; stubbed, per AGENTS.md "No External Processes or External State in Tests".

(ert-deftest agent-repl-test-wait-for-process-exit-is-defined ()
  "`agent-repl--wait-for-process-exit' exists for its production callers."
  ;; Arrange / Act / Assert
  (should (fboundp 'agent-repl--wait-for-process-exit)))

(ert-deftest agent-repl-test-wait-for-process-exit-dispatches-to-main-on-main-thread ()
  "On the main thread the wait routes to the `accept-process-output' path."
  ;; Arrange
  (let ((routed nil))
    (cl-letf (((symbol-function 'agent-repl--wait-for-process-exit--main)
               (lambda (&rest _) (setq routed 'main) 0))
              ((symbol-function 'agent-repl--wait-for-process-exit--worker)
               (lambda (&rest _) (setq routed 'worker) 0)))
      ;; Act
      (agent-repl--wait-for-process-exit nil 1 nil nil)
      ;; Assert
      (should (eq routed 'main)))))

(ert-deftest agent-repl-test-wait-for-process-exit-dispatches-to-worker-off-main-thread ()
  "Off the main thread the wait routes to the condition-variable path."
  ;; Arrange
  (let ((routed nil))
    (cl-letf (((symbol-function 'current-thread) (lambda () 'some-worker))
              ((symbol-function 'agent-repl--wait-for-process-exit--main)
               (lambda (&rest _) (setq routed 'main) 0))
              ((symbol-function 'agent-repl--wait-for-process-exit--worker)
               (lambda (&rest _) (setq routed 'worker) 0)))
      ;; Act
      (agent-repl--wait-for-process-exit nil 1 nil nil)
      ;; Assert
      (should (eq routed 'worker)))))

(ert-deftest agent-repl-test-wait-for-process-exit-main-returns-exit-status ()
  "A process already exited yields its exit status, not `timeout'."
  ;; Arrange
  (cl-letf (((symbol-function 'process-live-p) (lambda (_) nil))
            ((symbol-function 'process-exit-status) (lambda (_) 7)))
    ;; Act / Assert
    (should (equal (agent-repl--wait-for-process-exit--main 'proc 1 nil nil) 7))))

(ert-deftest agent-repl-test-wait-for-process-exit-main-returns-timeout-past-deadline ()
  "A process still live past the deadline yields the symbol `timeout'."
  ;; Arrange
  (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
            ((symbol-function 'accept-process-output) (lambda (&rest _) nil))
            ((symbol-function 'agent-repl--kill-process-safely) (lambda (_) t)))
    ;; Act / Assert — a zero-second budget is already expired on first check
    (should (eq (agent-repl--wait-for-process-exit--main 'proc 0 nil nil)
                'timeout))))

(ert-deftest agent-repl-test-wait-for-process-exit-main-kills-the-process-on-timeout ()
  "The timeout path tears the overrunning process down."
  ;; Arrange
  (let ((killed nil))
    (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
              ((symbol-function 'accept-process-output) (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--kill-process-safely)
               (lambda (p) (setq killed p) t)))
      ;; Act
      (agent-repl--wait-for-process-exit--main 'proc 0 nil nil)
      ;; Assert
      (should (eq killed 'proc)))))

;;;; ---- Tests: thread-safe process teardown ----

(ert-deftest agent-repl-test-kill-process-safely-deletes-on-the-main-thread ()
  "On the main thread the live process is deleted directly."
  ;; Arrange
  (let ((deleted nil))
    (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
              ((symbol-function 'delete-process) (lambda (p) (setq deleted p))))
      ;; Act
      (should (agent-repl--kill-process-safely 'proc))
      ;; Assert
      (should (eq deleted 'proc)))))

(ert-deftest agent-repl-test-kill-process-safely-is-a-no-op-for-a-dead-process ()
  "A process that is no longer live is not deleted again."
  ;; Arrange
  (let ((deleted nil))
    (cl-letf (((symbol-function 'process-live-p) (lambda (_) nil))
              ((symbol-function 'delete-process) (lambda (p) (setq deleted p))))
      ;; Act
      (should-not (agent-repl--kill-process-safely 'proc))
      ;; Assert
      (should-not deleted))))

(ert-deftest agent-repl-test-kill-process-safely-defers-off-the-main-thread ()
  "Off the main thread the delete is handed to the main thread, not run here."
  ;; Arrange
  (let ((deleted nil)
        (deferred nil))
    (cl-letf (((symbol-function 'current-thread) (lambda () 'some-worker))
              ((symbol-function 'process-live-p) (lambda (_) t))
              ((symbol-function 'process-name) (lambda (_) "proc"))
              ((symbol-function 'delete-process) (lambda (p) (setq deleted p)))
              ((symbol-function 'agent-repl--defer-to-main-thread)
               (lambda (thunk) (setq deferred thunk))))
      ;; Act
      (should (agent-repl--kill-process-safely 'proc))
      ;; Assert
      (should (functionp deferred))
      (should-not deleted))))

(ert-deftest agent-repl-test-defer-to-main-thread-schedules-on-the-timer-queue ()
  "The deferral hands the thunk to `run-at-time' rather than calling it.
The harness overrides `agent-repl--defer-to-main-thread' to run its thunk
synchronously (see test-helpers.el), so this test lifts every advice off
the symbol for its duration to reach the production body underneath, and
restores them afterwards."
  ;; Arrange
  (let ((scheduled nil)
        (called nil)
        (advices nil))
    (advice-mapc (lambda (fn props) (push (cons fn props) advices))
                 'agent-repl--defer-to-main-thread)
    (dolist (entry advices)
      (advice-remove 'agent-repl--defer-to-main-thread (car entry)))
    (unwind-protect
        (cl-letf (((symbol-function 'run-at-time)
                   (lambda (secs repeat thunk)
                     (setq scheduled (list secs repeat thunk)))))
          ;; Act
          (agent-repl--defer-to-main-thread (lambda () (setq called t))))
      (dolist (entry (reverse advices))
        (advice-add 'agent-repl--defer-to-main-thread :override (car entry)
                    (cdr entry))))
    ;; Assert
    (should (equal (list (nth 0 scheduled) (nth 1 scheduled)) '(0 nil)))
    (should-not called)))

(ert-deftest agent-repl-test-workspace-log-joins-the-standing-target ()
  "A second runtime appends to the target the canonical link already names."
  (agent-repl-test--with-clean-state
    ;; Arrange
    (let* ((project (make-temp-file "agent-repl-standing-project-" t))
           (ws "standing-ws")
           (agent-repl-log-to-file nil)
           (agent-repl--workspace-log-targets (make-hash-table :test #'equal))
           (first nil))
      (unwind-protect
          (progn
            (agent-repl--ws-put ws :project-dir project)
            (let ((agent-repl-log-to-file t))
              (agent-repl--log ws "first instance"))
            (setq first (plist-get (agent-repl--workspace-log-target-entry ws) :target))
            ;; Act: a fresh runtime's registry knows nothing.
            (let ((agent-repl--workspace-log-targets (make-hash-table :test #'equal))
                  (agent-repl-log-to-file t))
              (agent-repl--log ws "second instance")
              ;; Assert
              (should (equal (plist-get (agent-repl--workspace-log-target-entry ws) :target)
                             first))
              (should (equal (file-symlink-p
                              (expand-file-name ".claude/emacs/emacs.log" project))
                             first))
              (should (string-match-p
                       "second instance"
                       (with-temp-buffer (insert-file-contents first) (buffer-string))))))
        (when (and first (file-exists-p first)) (delete-file first))
        (delete-directory project t)))))

(ert-deftest agent-repl-test-workspace-log-mints-under-the-state-logs-directory ()
  "A minted target lives in ~/.claude-emacs/logs and carries the workspace identity."
  (agent-repl-test--with-clean-state
    ;; Arrange
    (let* ((project (make-temp-file "agent-repl-mint-project-" t))
           (ws "mint-ws")
           (agent-repl-log-to-file nil)
           (agent-repl--workspace-log-targets (make-hash-table :test #'equal)))
      (unwind-protect
          ;; Act
          (progn
            (agent-repl--ws-put ws :project-dir project)
            (let ((agent-repl-log-to-file t))
              (agent-repl--log ws "mint"))
            ;; Assert
            (let ((target (plist-get (agent-repl--workspace-log-target-entry ws) :target)))
              (should (equal (directory-file-name (file-name-directory target))
                             (directory-file-name (agent-repl--emacs-log-target-directory))))
              (should (string-prefix-p
                       (format "agent-repl-%s-emacs-"
                               (agent-repl--ws-dir-hash-cached ws))
                       (file-name-nondirectory target)))))
        (delete-directory project t)))))

(ert-deftest agent-repl-test-workspace-log-mints-past-an-over-cap-standing-target ()
  "A standing target at the cap is rotated and a fresh target is minted."
  (agent-repl-test--with-clean-state
    ;; Arrange
    (let* ((project (make-temp-file "agent-repl-overcap-project-" t))
           (ws "overcap-ws")
           (agent-repl-log-to-file nil)
           (agent-repl-log-size-cap-bytes 64)
           (agent-repl--workspace-log-targets (make-hash-table :test #'equal))
           (first nil))
      (unwind-protect
          (progn
            (agent-repl--ws-put ws :project-dir project)
            (let ((agent-repl-log-to-file t))
              (agent-repl--log ws "first instance"))
            (setq first (plist-get (agent-repl--workspace-log-target-entry ws) :target))
            (write-region (make-string 200 ?x) nil first nil 'silent)
            ;; Act
            (let ((agent-repl--workspace-log-targets (make-hash-table :test #'equal))
                  (agent-repl-log-to-file t))
              (agent-repl--log ws "second instance")
              ;; Assert
              (let ((second (plist-get (agent-repl--workspace-log-target-entry ws) :target)))
                (should-not (equal second first))
                (should (equal (file-symlink-p
                                (expand-file-name ".claude/emacs/emacs.log" project))
                               second))
                (should (file-regular-p first)))))
        (when (and first (file-exists-p first)) (delete-file first))
        (delete-directory project t)))))

(ert-deftest agent-repl-test-workspace-log-remints-past-a-dangling-link ()
  "A canonical link naming a deleted target is displaced, never followed."
  (agent-repl-test--with-clean-state
    ;; Arrange
    (let* ((project (make-temp-file "agent-repl-dangling-project-" t))
           (ws "dangling-ws")
           (agent-repl-log-to-file nil)
           (agent-repl--workspace-log-targets (make-hash-table :test #'equal))
           (first nil))
      (unwind-protect
          (progn
            (agent-repl--ws-put ws :project-dir project)
            (let ((agent-repl-log-to-file t))
              (agent-repl--log ws "first instance"))
            (setq first (plist-get (agent-repl--workspace-log-target-entry ws) :target))
            (delete-file first)
            ;; Act
            (let ((agent-repl--workspace-log-targets (make-hash-table :test #'equal))
                  (agent-repl-log-to-file t))
              (agent-repl--log ws "second instance")
              ;; Assert
              (should-not (equal (plist-get (agent-repl--workspace-log-target-entry ws) :target)
                                 first))))
        (when (and first (file-exists-p first)) (delete-file first))
        (delete-directory project t)))))

(ert-deftest agent-repl-test-workspace-log-standing-target-accepts-a-legacy-temp-target ()
  "A target an older instance minted into the temporary root is joined, not renamed."
  ;; Arrange
  (let* ((project (make-temp-file "agent-repl-legacy-project-" t))
         (canonical (expand-file-name ".claude/emacs/emacs.log" project))
         (legacy (make-temp-file agent-repl--emacs-log-target-prefix nil ".log")))
    (unwind-protect
        (progn
          (make-directory (file-name-directory canonical) t)
          (make-symbolic-link legacy canonical)
          ;; Act / Assert
          (should (equal (agent-repl--emacs-log-standing-target canonical) legacy)))
      (delete-file legacy)
      (delete-directory project t))))

(ert-deftest agent-repl-test-workspace-log-standing-target-refuses-a-foreign-target ()
  "A link naming a file outside this module's naming is not joined."
  ;; Arrange
  (let* ((project (make-temp-file "agent-repl-foreign-project-" t))
         (canonical (expand-file-name ".claude/emacs/emacs.log" project))
         (foreign (make-temp-file "somebody-elses-" nil ".log")))
    (unwind-protect
        (progn
          (make-directory (file-name-directory canonical) t)
          (make-symbolic-link foreign canonical)
          ;; Act / Assert
          (should-not (agent-repl--emacs-log-standing-target canonical)))
      (delete-file foreign)
      (delete-directory project t))))

(ert-deftest agent-repl-test-log-sweep-deletes-only-unreferenced-old-targets ()
  "The sweep deletes an old orphan and spares a referenced one and a young one."
  (agent-repl-test--with-clean-state
    ;; Arrange
    (let* ((dir (make-temp-file "agent-repl-sweep-" t))
           (orphan (expand-file-name "agent-repl-emacs-orphan.log" dir))
           (referenced (expand-file-name "agent-repl-emacs-referenced.log" dir))
           (young (expand-file-name "agent-repl-emacs-young.log" dir))
           (unrelated (expand-file-name "somebody-elses.log" dir))
           (old (time-subtract (current-time) (* 3 86400)))
           (agent-repl--workspace-log-targets (make-hash-table :test #'equal)))
      (unwind-protect
          (progn
            (dolist (path (list orphan referenced young unrelated))
              (write-region "x" nil path nil 'silent))
            (dolist (path (list orphan referenced unrelated))
              (set-file-times path old))
            (puthash "key" (list :target referenced) agent-repl--workspace-log-targets)
            ;; Act
            (let ((result (agent-repl--sweep-orphan-log-targets dir)))
              ;; Assert
              (should (equal (plist-get result :deleted) 1))
              (should-not (file-exists-p orphan))
              (should (file-exists-p referenced))
              (should (file-exists-p young))
              (should (file-exists-p unrelated))))
        (delete-directory dir t)))))

(ert-deftest agent-repl-test-log-sweep-is-bounded-per-tick ()
  "One sweep tick deletes at most `agent-repl-log-sweep-max-files' targets."
  (agent-repl-test--with-clean-state
    ;; Arrange
    (let* ((dir (make-temp-file "agent-repl-sweep-bound-" t))
           (old (time-subtract (current-time) (* 3 86400)))
           (agent-repl-log-sweep-max-files 2)
           (agent-repl--workspace-log-targets (make-hash-table :test #'equal)))
      (unwind-protect
          (progn
            (dotimes (i 5)
              (let ((path (expand-file-name (format "agent-repl-emacs-%d.log" i) dir)))
                (write-region "x" nil path nil 'silent)
                (set-file-times path old)))
            ;; Act
            (let ((result (agent-repl--sweep-orphan-log-targets dir)))
              ;; Assert
              (should (equal (plist-get result :deleted) 2))
              (should (equal (plist-get result :remaining) 3))
              (should (equal (length (directory-files dir nil "\\.log\\'")) 3))))
        (delete-directory dir t)))))

(ert-deftest agent-repl-test-log-sweep-deletes-a-swept-target-s-generations ()
  "A swept target's `.N' generations are orphans too and go with it."
  (agent-repl-test--with-clean-state
    ;; Arrange
    (let* ((dir (make-temp-file "agent-repl-sweep-gen-" t))
           (base (expand-file-name "agent-repl-emacs-gen.log" dir))
           (generation (concat base ".1"))
           (old (time-subtract (current-time) (* 3 86400)))
           (agent-repl--workspace-log-targets (make-hash-table :test #'equal)))
      (unwind-protect
          (progn
            (dolist (path (list base generation))
              (write-region "x" nil path nil 'silent)
              (set-file-times path old))
            ;; Act
            (agent-repl--sweep-orphan-log-targets dir)
            ;; Assert
            (should-not (file-exists-p generation)))
        (delete-directory dir t)))))

(ert-deftest agent-repl-test-log-sweep-spares-a-referenced-target-s-generations ()
  "A live target's retained generations are not swept out from under a reader."
  (agent-repl-test--with-clean-state
    ;; Arrange
    (let* ((dir (make-temp-file "agent-repl-sweep-live-gen-" t))
           (base (expand-file-name "agent-repl-emacs-live.log" dir))
           (generation (concat base ".1"))
           (old (time-subtract (current-time) (* 3 86400)))
           (agent-repl--workspace-log-targets (make-hash-table :test #'equal)))
      (unwind-protect
          (progn
            (dolist (path (list base generation))
              (write-region "x" nil path nil 'silent)
              (set-file-times path old))
            (puthash "key" (list :target base) agent-repl--workspace-log-targets)
            ;; Act
            (agent-repl--sweep-orphan-log-targets dir)
            ;; Assert
            (should (file-exists-p generation))
            (should (file-exists-p base)))
        (delete-directory dir t)))))
