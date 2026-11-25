;;; test-rules-logging.el --- Rule tests: the logging boundary -*- lexical-binding: t; -*-

;;; Commentary:

;; Logging-boundary rules over every elisp source: the one record builder and
;; writer, every rung through the canonical emitter, every log site's
;; attribution, every `message' a user-facing one, the one excerpt cut, and the
;; log-site scan (every call's literal template and its key).
;;
;; A rule test reads the module's elisp sources as data and holds them to a
;; structural rule.  Rule tests are no one module's tests, so they live by
;; rule area in `test-rules-*.el', the `ert-rules' suite every branch runs
;; before its merge is admitted (lisp/AGENTS.md, "Rule tests").

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'log-site)

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- core.el ----

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
        ("core.el" agent-repl--emit-log-record agent-repl--do-log-to-file)
        ;; The last pair is the same record rerouted centrally when the
        ;; workspace's directory went between routing and writing.
        ("core.el" agent-repl--emit-log-record agent-repl--log-record)
        ("core.el" agent-repl--emit-log-record agent-repl--do-log-to-file))))))

(ert-deftest agent-repl-test-every-logging-rung-calls-the-canonical-emitter ()
  "Every public severity rung's emitter is a thin call into one emitter."
  ;; Arrange / Act
  (let* ((rungs '(agent-repl--emit-log agent-repl--emit-log-verbose agent-repl--emit-info
                  agent-repl--emit-warn agent-repl--emit-warn-once agent-repl--emit-error
                  agent-repl--emit-fatal agent-repl--emit-backend-phase
                  agent-repl--emit-phase-echo))
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
  '(("commands.el" agent-repl--push-release-callback "agent-repl: %s"
     "tells the user the release push they asked for landed, in the script's own words")
    ("commands.el" agent-repl--push-release-callback "agent-repl: release push failed: %s"
     "tells the user the release push they asked for failed, in the script's own words")
    ("commands.el" agent-repl-push-release "agent-repl: pushing master to origin as a release..."
     "acknowledges the interactive release push while it runs")
    ("commands.el" agent-repl-copy-reference "Copied: %s"
     "confirms an interactive clipboard write")
    ("commands.el" agent-repl-copy-workspace-name "Copied workspace name: %s"
     "confirms an interactive clipboard write")
    ("commands.el" agent-repl-open-most-recent-workspace
     "All workspaces visited -- cycle reset"
     "reports the interactive workspace-cycle boundary")
    ("commands.el" agent-repl-switch-to-previous-workspace
     "[agent-repl] No previous workspace or thread to switch to"
     "tells the user SPC TAB TAB found no other workspace or thread to go back to")
    ("commands.el" agent-repl-switch-sidebar-app
     "[agent-repl] The sidebar shows no app yet to switch from"
     "tells the user SPC SPC pressed before the first roster had nothing to switch from")
    ("commands.el" agent-repl-switch-sidebar-app
     "[agent-repl] No workspace to switch to"
     "tells the user SPC SPC from Chat found no workspace to go back to")
    ("commands.el" agent-repl--jump-to-ranked-workspace "[agent-repl] %s"
     "tells the user SPC TAB g or M-0 or M-s-0 or M-) found no other workspace to jump to, in the gesture's own words")
    ("commands.el" agent-repl-switch-to-workspace
     "[agent-repl] No workspace tabs on the bar"
     "explains why the requested interactive switch did nothing")
    ("commands.el" agent-repl-switch-to-workspace
     "[agent-repl] No workspace tab %d -- the bar's last is %S"
     "explains why the requested interactive switch did nothing")
    ("commands.el" agent-repl-switch-to-workspace
     "[agent-repl] Tab %d (%s) is still opening"
     "explains why the requested interactive switch did nothing")
    ("commands.el" agent-repl--act-on-tab-slot
     "[agent-repl] The current workspace has no tab to %s"
     "explains why the requested interactive swap or move did nothing")
    ("commands.el" agent-repl--act-on-tab-slot
     "[agent-repl] No workspace tab %d -- the bar's last is %S"
     "explains why the requested interactive swap or move did nothing")
    ("commands.el" agent-repl--act-on-tab-slot
     "[agent-repl] %s is already tab %d"
     "explains why the requested interactive swap or move did nothing")
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
    ("daemon-link.el" agent-repl-link--faults-standing "agent-repl: %s"
     "surfaces a failed deploy (the daemon's loud faults) however it was started")
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
    ("elisp-build.el" agent-repl-elisp-reload-handle
     "agent-repl: elisp reload REFUSED -- the deploy is for %s, this Emacs runs %s"
     "tells the user a deploy's reload was refused because it names another checkout")
    ("elisp-build.el" agent-repl--elisp-reload-report-heartbeat
     "agent-repl: elisp reload left required timers unarmed: %s"
     "tells the user the reload left the editor's heartbeat broken")
    ("elisp-build.el" agent-repl--elisp-reload-run
     "agent-repl: elisp reload FAILED -- %s names no module"
     "tells the user the reload loaded nothing and why")
    ("elisp-build.el" agent-repl--elisp-reload-run
     "agent-repl: elisp reload FAILED -- %s could not be read: %s"
     "tells the user the reload loaded nothing and why")
    ("elisp-build.el" agent-repl--elisp-reload-finish
     "agent-repl: elisp reload: %d module(s) failed to load: %s"
     "tells the user which modules the reload could not load")
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
    ("held-edit.el" agent-repl--held-edit-enter
     "agent-repl: no composer is open to edit the held prompt in; the edit is cancelled"
     "tells the user why the held-prompt edit they began did not reach a composer")
    ("held-edit.el" agent-repl--held-edit-enter
     "agent-repl: the held prompt carries content the composer cannot hold; the edit is cancelled"
     "tells the user why the held-prompt edit they began was cancelled")
    ("held-edit.el" agent-repl--held-edit-on-refusal
     "agent-repl: editing the held prompt was refused -- %s"
     "reports why the user's held-prompt edit step was not taken")
    ("history.el" agent-repl-history-search
     "[agent-repl] input history is empty"
     "answers the interactive history search")
    ("host.el" agent-repl-host--unheld-select-refused "[agent-repl] The switch to %s did not happen (%s): pick it again"
     "tells the user the workspace they picked could not be switched to, and that picking it again retries")
    ("host.el" agent-repl-host--request-switch-held
     "[agent-repl] Cannot switch to %s: the daemon has not registered it"
     "explains why the user's switch request went nowhere")
    ("host.el" agent-repl-host--request-switch-held
     "[agent-repl] Cannot switch to %s: no daemon connection"
     "explains why the user's switch request went nowhere")
    ("input.el" agent-repl--input-resent
     "rollback: done, the prompt was sent again"
     "tells the user the rollback they confirmed is done and their words were sent in the prompt's place")
    ("input.el" agent-repl--input-refilled
     "%s"
     "tells the user the rollback they confirmed is done, with the files it restored")
    ("input.el" agent-repl--input-rollback
     "rollback: cancelled"
     "confirms the user declined the rollback confirmation, so nothing was rolled back")
    ("input.el" agent-repl-input-selection-escape
     "%s"
     "warns, in the minibuffer, that a second consecutive command-mode escape clears the feed selection")
    ("input.el" agent-repl--input-on-success
     "[agent-repl] Asked %s…"
     "tells the user their question about a card was handed to an intake workspace")
    ("input.el" agent-repl--input-on-bubble-refusal
     "agent-repl: the prompt was refused for this agent (%S)"
     "reports a prompt refusal requiring user action")
    ("input.el" agent-repl--input-on-bubble-refusal
     "agent-repl: refused -- %s%s"
     "reports a prompt refusal requiring user action")
    ("input.el" agent-repl--input-say-refusal
     "agent-repl: %s%s"
     "tells the user why their prompt was refused: the cold gate, no session yet, or an ask that settled first")
    ("input.el" agent-repl--input-on-error
     "agent-repl: refused -- a merge is in flight for this workspace"
     "reports why the user's prompt was not accepted")
    ("input.el" agent-repl--input-on-error
     "agent-repl: this submission's key was already accepted; the earlier submission stands"
     "reports idempotent acceptance to the submitting user")
    ("input.el" agent-repl--input-on-error
     "agent-repl: model change refused%s"
     "tells the user their model change was refused, in the refusal's own words")
    ("input.el" agent-repl--input-on-error
     "agent-repl: submission refused (%S)"
     "reports why the user's prompt was not accepted")
    ("input.el" agent-repl--input-on-failure
     "agent-repl: the daemon did not answer; the prompt is held"
     "tells the user their submitted text is held for the daemon")
    ("input.el" agent-repl--input-hold
     "agent-repl: the prompt could not be saved for the daemon (%s); it is in the input history: %s"
     "tells the user their prompt could not be held, with its words")
    ("input.el" agent-repl-send-held-now
     "agent-repl: no prompt sent from this composer to send now"
     "tells the user the send-now chord had no sent prompt to release")
    ("input.el" agent-repl-send-held-now
     "agent-repl: send now: %s"
     "tells the user why the daemon refused to send their held prompt now")
    ("keybindings.el" agent-repl-reload-config "[agent-repl] Reloaded %s"
     "confirms the interactive source reload")
    ("magit.el" agent-repl-magit-copy-commit-link
     "GitHub commit link copied to clipboard: %s"
     "confirms an interactive clipboard write")
    ("magit.el" agent-repl-open-workspace-pr-in-browser "Opened PR: %s"
     "confirms the interactive browser action")
    ("magit.el" agent-repl-magit-log-commit
     "agent-repl: %s is not among the commits %s's log shows"
     "tells the user the commit they clicked is not in the log that opened")
    ("persistent-wifi.el" agent-repl-persistent-wifi-handle "agent-repl: %s"
     "tells the user the machine's persistent-wifi standing changed")
    ("persistent-wifi.el" agent-repl-persistent-wifi--on-success "agent-repl: %s · %s · %s · %s"
     "reports what the user's persistent-wifi toggle left, step by step")
    ("prompt-queue.el" agent-repl-queue-deferred-prompt
     "agent-repl: no input to queue"
     "explains why the interactive queue command did nothing")
    ("prompt-queue.el" agent-repl-queue-deferred-prompt
     "agent-repl: deferred the prompt for %s (it runs as its own turn once the current one ends)"
     "confirms the interactive queue command")
    ("roster.el" agent-repl-roster-echo-finished
     "Agent finished in workspace: %s"
     "is the configured user-facing completion notification")
    ("spell.el" agent-repl--spell-guarded-word
     "Spellcheck: %s did not answer for %S (%s); the spell process was reset"
     "tells the user the spell checker stopped answering and was reset")
    ("status.el" agent-repl-tabbar-apply-row-count
     "agent-repl: tab-bar set to %s on this frame"
     "confirms the interactive tab-bar layout command")
    ("terminal.el" agent-repl-terminal-finished
     "agent-repl: the command exited with status %s; SPC o t shows its output"
     "tells the user the shell code block they ran failed, since its terminal popup closes on exit")
    ("threads.el" agent-repl-threads--create "[agent-repl] Starting a thread…"
     "tells the user their new-thread question was sent")
    ("threads.el" agent-repl-threads--create "[agent-repl] The thread was not started: the daemon could not be reached"
     "reports why the user's new thread was not started")
    ("threads.el" agent-repl-threads--created "[agent-repl] Thread %s started"
     "confirms the user's new thread, by its name")
    ("threads.el" agent-repl-threads--created "[agent-repl] The thread was not started: %s"
     "reports why the daemon refused the user's new thread")
    ("threads.el" agent-repl-threads--created "[agent-repl] The daemon answered the new thread with %S"
     "reports an answer to the user's new thread this build cannot read")
    ("threads.el" agent-repl-thread-send-background "[agent-repl] Asked %s"
     "confirms the question sent to the last thread without switching to it, by the thread's name")
    ("threads.el" agent-repl-threads--open-recent-launched
     "[agent-repl] There is no thread yet; SPC TAB t starts one"
     "tells the user who opened Chats from outside Emacs that there is no thread to show")
    ("verbs.el" agent-repl-verbs--on-refusal "%s refused: %s"
     "tells the user why a workspace with nothing left to restore it from cannot be opened")
    ("verbs.el" agent-repl-verbs--on-refusal "agent-repl: %s"
     "tells the user which workspace of the family stands in the way of their command")
    ("verbs.el" agent-repl-verbs--on-refusal
     "%s refused: the shim's lock helper %s %s; no other process owns this conversation"
     "tells the user the shim's lock helper failed, with its binary and how it failed")
    ("verbs.el" agent-repl-verbs--on-refusal "%s refused: %s%s"
     "reports a workspace command refusal to its user")
    ("verbs.el" agent-repl-verbs--send
     "agent-repl: %s failed -- the daemon did not answer"
     "reports failure of the user's workspace command")
    ("verbs.el" agent-repl-verb-close "close blocked -- %s"
     "reports why the user's close command was refused")
    ("verbs.el" agent-repl-verbs--send-teardown
     "agent-repl: %s of %s FAILED after its tab closed: %s"
     "tells the user a kill or nuke they ordered failed after its tab had already closed")
    ("verbs.el" agent-repl-verb-finish "deploy enqueued"
     "confirms the user's finish command")
    ("verbs.el" agent-repl-verb-restart "agent-repl: the restart failed: %s"
     "tells the user their restart failed")
    ("verbs.el" agent-repl-verb-interrupt "agent-repl: turn stopped"
     "confirms the user's interrupt stopped the running turn")
    ("verbs.el" agent-repl-verb-interrupt "agent-repl: turn stopped, prompt kept (%s)"
     "tells the user their interrupt stopped the turn but could not roll its prompt back, and why")
    ("verbs.el" agent-repl-verb-interrupt "agent-repl: nothing to interrupt"
     "tells the user there was no running turn to interrupt")
    ("verbs.el" agent-repl-verb-interrupt "agent-repl: %s stopped"
     "confirms the user's interrupt stopped the response updates and how many")
    ("verbs.el" agent-repl-verb-interrupt "agent-repl: turn stopped, %s also ended"
     "confirms the interrupt stopped the turn and how many agents it also ended")
    ("verbs.el" agent-repl-verb-interrupt "agent-repl: the interrupt answer could not be read"
     "tells the user the interrupt outcome could not be read")
    ("verbs.el" agent-repl-verb-interrupt "agent-repl: turn left running"
     "tells the user the interrupt left the turn running")
    ("verbs.el" agent-repl-verbs--focus-merge-queue-offer
     "agent-repl: the merge is queued -- Evict, Prioritize or Cancel in its bubble"
     "tells the user their interrupt raised the merge-queue question and where to answer it")
    ("verbs.el" agent-repl-verb-set-priority "agent-repl: priority %s"
     "confirms the user's priority command")
    ("verbs.el" agent-repl-verb-set-backlogged "agent-repl: %s %s"
     "confirms the user's hide or show of a workspace")
    ("verbs.el" agent-repl-verb-show-ref "agent-repl: %s shown"
     "confirms the hidden workspace the user asked to show again is shown")
    ("verbs.el" agent-repl-verb-retry-vendor-error "%s"
     "reports what the user's RET in an empty composer did to the scheduled retry")
    ("verbs.el" agent-repl-remote-control-toggle "agent-repl: remote control %s for %s"
     "reports the remote-control mode the user's toggle left")
    ("verbs.el" agent-repl-remote-control-toggle "agent-repl: %s"
     "reports why the user's remote-control toggle was refused")
    ("verbs.el" agent-repl-send-backlogged-keepalives "agent-repl: %s"
     "reports how the user's keep-alive to the hidden workspaces went")
    ("verbs.el" agent-repl-verb-swap "agent-repl: swapped %s with %s"
     "confirms the user's swap command")
    ("verbs.el" agent-repl-verbs--no-tab-to-take
     "agent-repl: %s and %s are in different repositories, so %s cannot take that tab"
     "explains why the daemon refused the user's swap or move command")
    ("verbs.el" agent-repl-verbs--no-tab-to-take
     "agent-repl: %s and %s are not siblings, so %s cannot take that tab"
     "explains why the daemon refused the user's swap or move command")
    ("verbs.el" agent-repl-verb-move "agent-repl: moved %s onto %s's tab"
     "confirms the user's move command")
    ("verbs.el" agent-repl-verbs-select-minted
     "agent-repl: the new workspace carries no directory to switch to"
     "reports why the requested new workspace cannot be selected")
    ("verbs.el" agent-repl-verb-shutdown-schedule "agent-repl: shutdown %s"
     "confirms the user's shutdown-schedule command")
    ("verbs.el" agent-repl-verbs--merge-queue-on-error
     "merge-queue refused: the daemon's registry does not hold repository %S"
     "reports why the user's merge-queue command was refused")
    ("verbs.el" agent-repl-verb-merge-queue "agent-repl: merge queue %s"
     "confirms the user's merge-queue command")
    ("verbs.el" agent-repl-verbs--deploy-on-success "agent-repl: deploy%s: %s"
     "reports every component's decision for the user's deploy command")
    ("verbs.el" agent-repl-verbs--deploy-on-error "agent-repl: deploy refused: %s"
     "reports why the user's deploy command was refused, with its detail"))
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

(ert-deftest agent-repl-test-log-excerpt-is-every-excerpt-s-one-cut ()
  "No agent-repl source but core.el writes the excerpt marker by hand."
  ;; Arrange
  (let ((dir (file-name-directory (symbol-file 'agent-repl--log-excerpt)))
        (hand-rolled nil))
    (dolist (file (directory-files dir t "\\`[a-z-]+\\.el\\'"))
      (unless (or (string-prefix-p "test-" (file-name-nondirectory file))
                  (equal (file-name-nondirectory file) "core.el"))
        (with-temp-buffer
          (insert-file-contents file)
          ;; Act
          (when (search-forward "more characters not logged]" nil t)
            (push (file-name-nondirectory file) hand-rolled)))))
    ;; Assert
    (should (null hand-rolled))))

;;;; ---- log-site.el ----

(defconst agent-repl-test-log-site--macros
  '((agent-repl--log . 1) (agent-repl--log-verbose . 1) (agent-repl--info . 1)
    (agent-repl--warn . 1) (agent-repl--warn-once . 2) (agent-repl--error . 1)
    (agent-repl--fatal . 1) (agent-repl--log-decision . 3)
    (agent-repl--backend-phase . 1) (agent-repl--phase-echo . 1)
    (agent-repl--log-at . 2) (agent-repl--log-msg . 0)
    (agent-repl--boot-info . 0) (agent-repl--boot-warn . 0)
    (agent-repl-open-tabs--say . 0))
  "Every log macro, with how many arguments precede its template.
`agent-repl--boot-info', `agent-repl--boot-warn' (config.el) and
`agent-repl-open-tabs--say' (open-tabs.el) are log macros of their own
files, passing their template on to a rung.")

(defun agent-repl-test-log-site--source-files ()
  "Every Elisp source of the module: `lisp/*.el' and config.el."
  (cons (expand-file-name "config.el" agent-repl-test--module-root)
        (directory-files agent-repl-test--module-dir t "\\.el\\'")))

(defun agent-repl-test-log-site--scan-form (form file enclosing quasi visit)
  "Call VISIT on every log macro call in FORM, read from FILE.
ENCLOSING is the innermost definition's name around FORM, or nil; QUASI
is non-nil inside a backquote.  VISIT gets (FILE FUNCTION TEMPLATE QUASI
CALL).  Quoted data holds no calls and is not walked."
  (when (and (consp form) (not (eq (car form) 'quote)))
    (let ((enclosing (or (agent-repl-log-site-definition-name form) enclosing))
          (quasi (or quasi (eq (car form) '\`)))
          (lead (and (symbolp (car form))
                     (cdr (assq (car form) agent-repl-test-log-site--macros)))))
      (when (and lead (proper-list-p form) (> (length form) lead))
        (funcall visit file
                 (or enclosing (file-name-nondirectory file))
                 (nth (1+ lead) form) quasi form))
      (while (consp form)
        (agent-repl-test-log-site--scan-form (car form) file enclosing quasi visit)
        (setq form (cdr form))))))

(defun agent-repl-test-log-site--scan ()
  "Scan every source; answer (:sites ((KEY FUNCTION TEMPLATE FILE) ...) :faults (TEXT ...))."
  (let ((sites nil) (faults nil))
    (dolist (file (agent-repl-test-log-site--source-files))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (condition-case nil
            (while t
              (agent-repl-test-log-site--scan-form
               (read (current-buffer)) file nil nil
               (lambda (file function template quasi call)
                 (cond
                  ;; A forwarding macro hands its own caller's template on.
                  ((and quasi (consp template) (eq (car template) '\,)))
                  (quasi
                   (push (format "%s: %S is written inside a backquote, so its site would be keyed by wherever the macro expands; move it into a function"
                                 (file-name-nondirectory file) call)
                         faults))
                  ((not (stringp template))
                   (push (format "%s: %S has no literal template"
                                 (file-name-nondirectory file) call)
                         faults))
                  (t
                   (push (list (agent-repl-log-site-key agent-repl-log-site-runtime
                                                        function template)
                               function template (file-name-nondirectory file))
                         sites))))))
          (end-of-file nil))))
    (list :sites (nreverse sites) :faults (nreverse faults))))

(ert-deftest agent-repl-test-log-site-scan-finds-every-call-literal ()
  "Every log call in every source has a literal template outside any backquote."
  ;; Act
  (let ((faults (plist-get (agent-repl-test-log-site--scan) :faults)))
    ;; Assert
    (should-not faults)))

(ert-deftest agent-repl-test-log-site-scan-finds-no-template-twice-in-one-function ()
  "No two calls in one function write one template: each gets its own wording."
  ;; Arrange
  (let ((seen (make-hash-table :test #'equal))
        (twice nil))
    ;; Act
    (dolist (site (plist-get (agent-repl-test-log-site--scan) :sites))
      (let ((where (list (nth 1 site) (nth 2 site))))
        (when (gethash where seen)
          (push (format "%s: %s writes %S twice" (nth 3 site) (nth 1 site) (nth 2 site))
                twice))
        (puthash where t seen)))
    ;; Assert
    (should-not (delete-dups twice))))

(ert-deftest agent-repl-test-log-site-scan-finds-no-colliding-keys ()
  "No two sites, of different functions or templates, share one key."
  ;; Arrange
  (let ((owners (make-hash-table :test #'equal))
        (collisions nil))
    ;; Act
    (dolist (site (plist-get (agent-repl-test-log-site--scan) :sites))
      (let ((owner (gethash (car site) owners))
            (this (list (nth 1 site) (nth 2 site))))
        (if (and owner (not (equal owner this)))
            (push (format "%s: %S and %S" (car site) owner this) collisions)
          (puthash (car site) this owners))))
    ;; Assert
    (should-not collisions)))

(ert-deftest agent-repl-test-log-site-scan-agrees-with-every-loaded-expansion ()
  "Every production site the scan finds was expanded, at load, under its own key."
  ;; Arrange
  (let ((missing nil))
    ;; Act
    (dolist (site (plist-get (agent-repl-test-log-site--scan) :sites))
      (unless (or (string-prefix-p "test-" (nth 3 site))
                  (equal (nth 3 site) "config.el")
                  (gethash (car site) agent-repl-log-site--expanded))
        (push (format "%s: %s %S" (nth 3 site) (nth 1 site) (nth 2 site)) missing)))
    ;; Assert
    (should-not missing)))

;;; test-rules-logging.el ends here
