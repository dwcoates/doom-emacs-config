;;; test-host.el --- ERT tests for agent-repl host.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;   AGENT_REPL_FORBID_VENDOR_CALLS=1 emacs -batch -Q -l ert \
;;     -l lisp/test-host.el -f ert-run-tests-batch-and-exit
;;
;; The rpc layer is stubbed with functions that invoke their callbacks
;; SYNCHRONOUSLY, so every arm — success, daemon-authored error, transport
;; failure — is exercised deterministically with no process anywhere.  The
;; surfaces host.el calls in other modules (`agent-repl--notification-activate',
;; `agent-repl-frontend-reload-webview', `agent-repl-popup-open') are
;; recorded here rather than invoked for real.

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                           (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Fixtures ----

(defun agent-repl-test-host--ref (&optional id dir)
  "Return a decoded `WorkspaceRef' plist, echoed verbatim by production."
  (list :id (or id "ws-id-1") :dir (or dir "/tmp/agent-repl-test/ws-1")))

(defun agent-repl-test-host--live (&rest overrides)
  "Return a `HostWorkspace' with a LIVE session, with OVERRIDES on the live arm."
  (let ((live (list :generation (list :value "gen-1")
                    :shim-attached t
                    :vendor-info (list :arm :claude
                                       :value (list :session-id "vendor-1"
                                                    :config-dir "/tmp/cfg"))
                    :backfill (list :arm :done :value nil)
                    :composer (list :arm :open :value nil)
                    :faults nil)))
    (while overrides
      (setq live (plist-put live (pop overrides) (pop overrides))))
    (list :session (list :arm :existing
                         :value (list :id (list :value "session-1")
                                      :standing (list :arm :live :value live)))
          :naming (list :slug nil :title nil))))

(defun agent-repl-test-host--composer (arm)
  "Return a `HostWorkspace' whose live composer oneof is ARM."
  (agent-repl-test-host--live :composer (list :arm arm :value nil)))

(defun agent-repl-test-host--terminal (&optional rehydratable)
  "Return a `HostWorkspace' whose session standing is terminal."
  (list :session (list :arm :existing
                       :value (list :id (list :value "session-1")
                                    :standing (list :arm :terminal
                                                    :value (list :rehydratable
                                                                 (and rehydratable t)))))
        :naming (list :slug nil :title nil)))

(defun agent-repl-test-host--none ()
  "Return a `HostWorkspace' that has never had a session."
  (list :session (list :arm :none :value nil)
        :naming (list :slug nil :title nil)))

;;;; ---- Harness ----

(defvar agent-repl-test-host--streams nil
  "Stubbed host streams, newest first: `(:conn C :ref R :on-push F :on-close F)'.")

(defvar agent-repl-test-host--cancelled nil
  "Stream records cancelled through the transport, newest first.")

(defvar agent-repl-test-host--calls nil
  "Recorded rpc calls, newest first: `(METHOD CONN REQUEST)'.")

(defvar agent-repl-test-host--register-answer nil
  "What the stubbed RegisterWorkspace answers; see the harness.")

(defvar agent-repl-test-host--adopt-answer nil
  "What the stubbed AdoptHostWorkspace answers; see the harness.")

(defvar agent-repl-test-host--evaluation-answer nil
  "What the stubbed AnswerEditorEvaluation answers; see the harness.")

(defvar agent-repl-test-host--select-answer nil
  "What the stubbed SelectWorkspace answers; see the harness.")

(defvar agent-repl-test-host--mark-viewed-answer nil
  "What the stubbed MarkWorkspaceViewed answers; see the harness.")

(defvar agent-repl-test-host--unloading-answer nil
  "What the stubbed ReportPageUnloading answers; see the harness.")

(defvar agent-repl-test-host--effects nil
  "W2-B surface calls, newest first: `(NAME . ARGS)'.")

(defvar agent-repl-test-host--current-ws nil
  "What the stubbed `agent-repl--ws-current-name' answers.")

(defvar agent-repl-test-host--logs nil
  "Captured `(LEVEL . TEXT)' log entries, newest first.")

(defvar agent-repl-test-host--successor nil
  "What the stubbed `agent-repl-link-successor' answers.")

(defvar agent-repl-test-host--successor-pending nil
  "What the stubbed `agent-repl-link-successor-pending-p' answers.
Non-nil models the real window the outgoing daemon\='s `transferred'
push routinely lands in: the successor was announced and dialed, but its
`WatchDaemon' has not been accepted yet, so
`agent-repl-link-successor' still answers nil.")

(defvar agent-repl-test-host--walk nil
  "Steps of the adoption walk, newest first: :adopt :reload :cancel :subscribe.")

(defvar agent-repl-test-host--dialled nil
  "Addresses handed to the stubbed dial, newest first.")

(defvar agent-repl-test-host--timers nil
  "Timers this harness intercepted, newest first: (DELAY FN . ARGS).

NO SCHEDULED WORK ESCAPES A SCENARIO.  Two of host.el\='s handover paths
answer a refusal or a transport failure by SCHEDULING the retry rather
than issuing it (`agent-repl-host--on-refused\=',
`agent-repl-host--adopt-onto\='), and a real `run-at-time\=' from a batch
scenario outlives it: the timer fires during whatever LATER test is
inside an `accept-process-output\=', and re-walks an adopt against
globals that scenario has rebound.  Measured: with the adopt\='s transport
failure retrying for real, the full `test-agent-repl.el\=' run failed
`agent-repl-itest-host-fault-kind-name-reaches-the-health-buffer\=' about
one run in three, at an assertion rather than a bound and in 14ms, while
the host suite alone passed every time.  So the harness records the
schedule instead of arming it, and a test that wants the retry walked
calls it itself.")

(defvar agent-repl-test-host--link-up t
  "What the stubbed `agent-repl-link-up-p' answers: an up link by default.")

(defvar agent-repl-test-host--live nil
  "What the stubbed `agent-repl-link-live' answers: the live daemon, or nil.")

(defvar agent-repl-test-host--ending nil
  "The connection the stubbed `agent-repl-link-ending-p' answers as ending, or nil.")

(defvar agent-repl-test-host--dial-accepts t
  "When non-nil the stubbed dial is ACCEPTED at once and answers a conn.
Nil models the real gate: the dial stands but is not accepted yet, so
`agent-repl-link-successor' keeps answering nil.")

(defun agent-repl-test-host--logged-p (level substring)
  "Return non-nil when a LEVEL entry containing SUBSTRING was recorded."
  (seq-some (lambda (entry)
              (and (eq (car entry) level)
                   (string-search substring (cdr entry))))
            agent-repl-test-host--logs))

(defun agent-repl-test-host--answer (answer on-response on-failure)
  "Deliver ANSWER through ON-RESPONSE or ON-FAILURE, synchronously.
ANSWER is `(:response PLIST)' or `(:failure DETAIL)' — the two facts a
unary rpc can produce, which the contract never collapses into one."
  (pcase (car answer)
    (:response (when on-response (funcall on-response (cadr answer))))
    (:failure (when on-failure (funcall on-failure (cadr answer))))
    ;; HELD: the call stays unanswered until the test answers it, in the
    ;; order it chooses (`agent-repl-test-host--answer-held').
    (:held (setq agent-repl-test-host--held
                 (append agent-repl-test-host--held
                         (list (cons on-response on-failure)))))))

(defvar agent-repl-test-host--held nil
  "Unanswered calls under a `(:held)' answer, oldest first, as (ON-RESPONSE . ON-FAILURE).")

(defvar agent-repl-test-host--reapplied 0
  "How many times host.el asked roster.el to re-apply `current'.")

(defun agent-repl-test-host--answer-held (response)
  "Answer the OLDEST held call with RESPONSE (a response plist)."
  (let ((call (pop agent-repl-test-host--held)))
    (funcall (car call) response)))

(defun agent-repl-test-host--select-requests ()
  "The request plists of every SelectWorkspace sent, oldest first."
  (mapcar (lambda (call) (nth 2 call))
          (reverse (cl-remove-if-not (lambda (call) (equal (car call) "SelectWorkspace"))
                                     agent-repl-test-host--calls))))

(defun agent-repl-test-host--selected-ids ()
  "The ref ids SelectWorkspace was sent for, oldest first."
  (mapcar (lambda (request) (plist-get (plist-get request :workspace) :id))
          (agent-repl-test-host--select-requests)))

(defmacro agent-repl-test-host--with-harness (&rest body)
  "Run BODY with host.el's whole world faked and its state reset."
  (declare (indent 0))
  `(let ((agent-repl-host--by-name (make-hash-table :test 'equal))
         (agent-repl-host--select-in-flight nil)
         (agent-repl-host--select-queued nil)
         (agent-repl-host--confirming nil)
         (agent-repl--recency-walk-landing nil)
         (agent-repl-host-last-request nil)
         (agent-repl-host--editor-switches 0)
         (agent-repl-host-automatic-switch nil)
         (agent-repl-roster-following nil)
         (agent-repl-test-host--held nil)
         (agent-repl-test-host--reapplied 0)
         (agent-repl-host-last-selected-id nil)
         (agent-repl-host-reselect-pending nil)
         (agent-repl--eager-open-in-progress nil)
         (agent-repl-host-update-functions nil)
         (agent-repl-test-host--streams nil)
         (agent-repl-test-host--cancelled nil)
         (agent-repl-test-host--calls nil)
         (agent-repl-test-host--effects nil)
         (agent-repl-test-host--logs nil)
         (agent-repl-test-host--current-ws nil)
         (agent-repl-test-host--successor nil)
         (agent-repl-test-host--successor-pending nil)
         (agent-repl-test-host--walk nil)
         (agent-repl-test-host--dialled nil)
         (agent-repl-test-host--timers nil)
         (agent-repl-test-host--dial-accepts t)
         (agent-repl-test-host--live nil)
         (agent-repl-test-host--link-up t)
         (agent-repl-test-host--ending nil)
         (agent-repl-link-down-planned nil)
         (agent-repl-host--reattach-tokens 0)
         (agent-repl-link-handover-functions nil)
         (agent-repl-test-host--register-answer
          (list :response (list :arm :success
                                :value (list :workspace (agent-repl-test-host--ref)))))
         (agent-repl-test-host--select-answer
          (list :response (list :arm :success :value (list :roster-revision 0))))
         (agent-repl-test-host--mark-viewed-answer
          (list :response (list :arm :success :value nil)))
         (agent-repl-test-host--unloading-answer
          (list :response (list :arm :success :value nil)))
         (agent-repl-test-host--adopt-answer
          (list :response (list :arm :success :value nil)))
         (agent-repl-test-host--evaluation-answer
          (list :response (list :arm :success :value nil))))
     (cl-letf (((symbol-function 'agent-repl-link-up-p)
                (lambda () agent-repl-test-host--link-up))
               ((symbol-function 'agent-repl-rpc-register-workspace)
                (lambda (conn request &rest keys)
                  (push (list "RegisterWorkspace" conn request) agent-repl-test-host--calls)
                  (agent-repl-test-host--answer agent-repl-test-host--register-answer
                                                (plist-get keys :on-response)
                                                (plist-get keys :on-failure))))
               ((symbol-function 'agent-repl-rpc-select-workspace)
                (lambda (conn request &rest keys)
                  (push (list "SelectWorkspace" conn request) agent-repl-test-host--calls)
                  (agent-repl-test-host--answer agent-repl-test-host--select-answer
                                                (plist-get keys :on-response)
                                                (plist-get keys :on-failure))))
               ((symbol-function 'agent-repl-rpc-mark-workspace-viewed)
                (lambda (conn request &rest keys)
                  (push (list "MarkWorkspaceViewed" conn request) agent-repl-test-host--calls)
                  (agent-repl-test-host--answer agent-repl-test-host--mark-viewed-answer
                                                (plist-get keys :on-response)
                                                (plist-get keys :on-failure))))
               ((symbol-function 'agent-repl-rpc-report-page-unloading)
                (lambda (conn request &rest keys)
                  (push (list "ReportPageUnloading" conn request) agent-repl-test-host--calls)
                  (agent-repl-test-host--answer agent-repl-test-host--unloading-answer
                                                (plist-get keys :on-response)
                                                (plist-get keys :on-failure))))
               ((symbol-function 'agent-repl-rpc-adopt-host-workspace)
                (lambda (conn request &rest keys)
                  (push (list "AdoptHostWorkspace" conn request) agent-repl-test-host--calls)
                  (push :adopt agent-repl-test-host--walk)
                  (agent-repl-test-host--answer agent-repl-test-host--adopt-answer
                                                (plist-get keys :on-response)
                                                (plist-get keys :on-failure))))
               ((symbol-function 'agent-repl-rpc-answer-editor-evaluation)
                (lambda (conn request &rest keys)
                  (push (list "AnswerEditorEvaluation" conn request) agent-repl-test-host--calls)
                  (agent-repl-test-host--answer agent-repl-test-host--evaluation-answer
                                                (plist-get keys :on-response)
                                                (plist-get keys :on-failure))))
               ((symbol-function 'agent-repl-rpc-watch-host-workspace)
                (lambda (conn ref on-push on-close &optional on-open)
                  (push :subscribe agent-repl-test-host--walk)
                  (let ((record (list :conn conn :ref ref
                                      :on-push on-push :on-close on-close
                                      :on-open on-open)))
                    (push record agent-repl-test-host--streams)
                    record)))
               ((symbol-function 'agent-repl-connect-stream-cancel)
                (lambda (stream)
                  (push stream agent-repl-test-host--cancelled)
                  (push :cancel agent-repl-test-host--walk)))
               ((symbol-function 'run-at-time)
                (lambda (delay _repeat fn &rest args)
                  (push (cons delay (cons fn args)) agent-repl-test-host--timers)))
               ((symbol-function 'agent-repl-link-successor)
                (lambda () agent-repl-test-host--successor))
               ((symbol-function 'agent-repl-link-successor-pending-p)
                (lambda () agent-repl-test-host--successor-pending))
               ((symbol-function 'agent-repl-link-primary) (lambda () nil))
               ((symbol-function 'agent-repl-link-live)
                (lambda () agent-repl-test-host--live))
               ((symbol-function 'agent-repl-link-ending-p)
                (lambda (conn) (and conn (eq conn agent-repl-test-host--ending))))
               ((symbol-function 'agent-repl-link-dial-successor)
                (lambda (address)
                  (push address agent-repl-test-host--dialled)
                  (when agent-repl-test-host--dial-accepts
                    (setq agent-repl-test-host--successor
                          (agent-repl-connect-open address)))
                  agent-repl-test-host--successor))
               ((symbol-function 'agent-repl--ws-put) (lambda (&rest _) nil))
               ((symbol-function 'agent-repl--ws-current-name)
                (lambda () agent-repl-test-host--current-ws))
               ((symbol-function 'agent-repl--notification-activate)
                (lambda (ws) (push (cons :activate ws) agent-repl-test-host--effects)))
               ((symbol-function 'agent-repl-input-focus-for-reply)
                (lambda (ws) (push (cons :focus-for-reply ws) agent-repl-test-host--effects)))
               ((symbol-function 'agent-repl-input-edit-entry)
                (lambda (ws row label text)
                  (push (list :edit-entry ws (plist-get row :value) label text) agent-repl-test-host--effects)))
               ((symbol-function 'agent-repl-roster-apply-current)
                (lambda () (cl-incf agent-repl-test-host--reapplied)))
               ((symbol-function 'agent-repl-frontend-reload-webapp)
                (lambda (ws build) (push (list :reload-webapp ws build) agent-repl-test-host--effects)))
               ((symbol-function 'agent-repl-frontend-reload-webview)
                (lambda (ws)
                  ;; The conn is captured AS THE RELOAD SEES IT: frontend.el
                  ;; derives the page URL from it, so the order is the fact
                  ;; under test, not an incidental detail.
                  (push (cons :reload-conn (agent-repl-host-conn ws))
                        agent-repl-test-host--effects)
                  (push (cons :reload ws) agent-repl-test-host--effects)
                  (push :reload agent-repl-test-host--walk)))
               ((symbol-function 'agent-repl-popup-open)
                (lambda (path &optional line)
                  (push (list :popup path line) agent-repl-test-host--effects)))
               ((symbol-function 'agent-repl--log)
                (lambda (_ws fmt &rest args)
                  (push (cons :log (apply #'format fmt args)) agent-repl-test-host--logs)))
               ((symbol-function 'agent-repl--info)
                (lambda (_ws fmt &rest args)
                  (push (cons :info (apply #'format fmt args)) agent-repl-test-host--logs)))
               ((symbol-function 'agent-repl--warn)
                (lambda (_ws fmt &rest args)
                  (push (cons :warn (apply #'format fmt args)) agent-repl-test-host--logs)))
               ((symbol-function 'agent-repl--error)
                (lambda (_ws fmt &rest args)
                  (push (cons :error (apply #'format fmt args)) agent-repl-test-host--logs))))
       ,@body)))

(defun agent-repl-test-host--subscribe (ws &optional conn ref)
  "Subscribe WS on CONN with REF and return the stubbed stream record."
  (agent-repl-host-subscribe (or conn (agent-repl-connect-open "127.0.0.1:9001"))
                             ws (or ref (agent-repl-test-host--ref))))

(defun agent-repl-test-host--push (ws push)
  "Deliver PUSH to WS's stubbed host stream."
  (funcall (plist-get (agent-repl-host-stream ws) :on-push) push))

;;;; ---- Register ----

(ert-deftest agent-repl-test-host-register-sends-the-dir ()
  "The host hands the daemon a PATH; the daemon mints the identity."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (let ((conn (agent-repl-connect-open "127.0.0.1:9001")))
      ;; Act
      (agent-repl-host-register conn "/tmp/agent-repl-test/ws-1" #'ignore)
      ;; Assert
      (should (equal (nth 2 (car agent-repl-test-host--calls))
                     '(:dir "/tmp/agent-repl-test/ws-1"))))))

(ert-deftest agent-repl-test-host-register-hands-the-minted-ref-to-the-continuation ()
  "The success arm's `workspace' IS the identity everything later echoes."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (let ((conn (agent-repl-connect-open "127.0.0.1:9001"))
          received)
      ;; Act
      (agent-repl-host-register conn "/tmp/ws" (lambda (ref) (setq received ref)))
      ;; Assert
      (should (equal received (agent-repl-test-host--ref))))))

(ert-deftest agent-repl-test-host-register-error-arm-answers-nil ()
  "A daemon-authored refusal is an ERROR and answers no ref."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (let ((conn (agent-repl-connect-open "127.0.0.1:9001"))
          (received :unset))
      (setq agent-repl-test-host--register-answer
            (list :response (list :arm :error :value nil)))
      ;; Act
      (agent-repl-host-register conn "/tmp/ws" (lambda (ref) (setq received ref)))
      ;; Assert
      (should (null received)))))

(ert-deftest agent-repl-test-host-register-error-arm-is-logged-at-error ()
  "A refusal is never swallowed."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (let ((conn (agent-repl-connect-open "127.0.0.1:9001")))
      (setq agent-repl-test-host--register-answer
            (list :response (list :arm :error :value nil)))
      ;; Act
      (agent-repl-host-register conn "/tmp/ws" #'ignore)
      ;; Assert
      (should (agent-repl-test-host--logged-p :error "elisp.host.register-refused")))))

(ert-deftest agent-repl-test-host-register-refusal-goes-to-its-owner ()
  "A caller that owns refusals is handed the decoded error, not a nil ref."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (let ((conn (agent-repl-connect-open "127.0.0.1:9001"))
          (refusal '(:cause (:arm :not-a-worktree :value nil)))
          (done :unset)
          received)
      (setq agent-repl-test-host--register-answer
            (list :response (list :arm :error :value refusal)))
      ;; Act
      (agent-repl-host-register conn "/tmp/ws" (lambda (ref) (setq done ref)) nil
                                (lambda (value) (setq received value)))
      ;; Assert
      (should (equal received refusal))
      (should (eq done :unset)))))

(ert-deftest agent-repl-test-host-register-owned-refusal-is-an-answer-at-info ()
  "A refusal its caller reports is an answer: INFO, never ERROR."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (let ((conn (agent-repl-connect-open "127.0.0.1:9001")))
      (setq agent-repl-test-host--register-answer
            (list :response (list :arm :error :value nil)))
      ;; Act
      (agent-repl-host-register conn "/tmp/ws" #'ignore nil #'ignore)
      ;; Assert
      (should (agent-repl-test-host--logged-p :info "elisp.host.register-refused"))
      (should-not (agent-repl-test-host--logged-p :error "elisp.host.register-refused")))))

(ert-deftest agent-repl-test-host-register-transport-failure-answers-nil ()
  "A transport failure and a daemon error arm are different facts, same outcome."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (let ((conn (agent-repl-connect-open "127.0.0.1:9001"))
          (received :unset))
      (setq agent-repl-test-host--register-answer
            (list :failure (list :kind :transport :message "no route")))
      ;; Act
      (agent-repl-host-register conn "/tmp/ws" (lambda (ref) (setq received ref)))
      ;; Assert
      (should (null received)))))

;;;; ---- Select ----

(ert-deftest agent-repl-test-host-select-echoes-the-ref ()
  "SelectWorkspace carries the daemon-minted ref, never a path."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    (setq agent-repl-test-host--calls nil)
    ;; Act
    (agent-repl-host-select "ws-1")
    ;; Assert
    (should (equal (nth 2 (car agent-repl-test-host--calls))
                   (list :workspace (agent-repl-test-host--ref))))))

(ert-deftest agent-repl-test-host-select-records-the-last-selected-id ()
  "roster.el compares a daemon-originated `current' change against this."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act
    (agent-repl-host-select "ws-1")
    ;; Assert
    (should (equal agent-repl-host-last-selected-id "ws-id-1"))))

(ert-deftest agent-repl-test-host-select-without-a-ref-sends-nothing ()
  "An unregistered workspace has no identity to select."
  (agent-repl-test-host--with-harness
    ;; Arrange / Act
    (agent-repl-host-select "ws-unknown")
    ;; Assert
    (should (null agent-repl-test-host--calls))))

(ert-deftest agent-repl-test-host-a-background-activation-selects-nothing ()
  "A transiently activated BACKGROUND workspace is not a tab switch.
`agent-repl--call-in-background-workspace' makes a background workspace's
perspective active for the length of a mount; reporting that as a
SelectWorkspace told the daemon the user had chosen a workspace they
never looked at, and the roster push carrying that stamp took the frame
off the one they were standing in."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    (setq agent-repl-test-host--current-ws "ws-1"
          agent-repl-test-host--calls nil)
    ;; Act
    (let ((agent-repl--eager-open-in-progress t))
      (agent-repl-host--on-workspace-activated))
    ;; Assert
    (should (null agent-repl-test-host--calls))))

(ert-deftest agent-repl-test-host-an-ordinary-activation-still-selects ()
  "A switch Emacs made on its own is still told to the daemon."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    (setq agent-repl-test-host--current-ws "ws-1"
          agent-repl-test-host--calls nil)
    ;; Act
    (let ((agent-repl--eager-open-in-progress nil))
      (agent-repl-host--on-workspace-activated))
    ;; Assert
    (should (equal (car (car agent-repl-test-host--calls)) "SelectWorkspace"))))

(ert-deftest agent-repl-test-host-attaching-inside-a-background-activation-selects-nothing ()
  "The attach's own selection asks the same question and needs the same answer.
`--ws-current-name' answers the perspective active right now, which
during a background mount is the BACKGROUND workspace's."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (setq agent-repl-test-host--current-ws "ws-1")
    ;; Act
    (let ((agent-repl--eager-open-in-progress t))
      (agent-repl-test-host--subscribe "ws-1"))
    ;; Assert
    (should (null (seq-find (lambda (call) (equal (car call) "SelectWorkspace"))
                            agent-repl-test-host--calls)))))

(ert-deftest agent-repl-test-host-attaching-a-ref-selects-the-current-workspace ()
  "A newly registered workspace activates its perspective BEFORE the daemon
mints its ref, so the activation hook skips for want of one.  Attaching
the ref is when that condition stops holding, and the selection the hook
could not make is made then -- without it the daemon never stamps
`current' and no sidebar row is ever drawn as the selected one."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (setq agent-repl-test-host--current-ws "ws-1")
    ;; Act
    (agent-repl-test-host--subscribe "ws-1")
    ;; Assert
    (let ((call (assoc "SelectWorkspace" agent-repl-test-host--calls)))
      (should call)
      (should (equal (nth 2 call) (list :workspace (agent-repl-test-host--ref)))))))

(ert-deftest agent-repl-test-host-attaching-a-ref-selects-nothing-in-the-background ()
  "A workspace registered while the user stands somewhere else is NOT the
selection.  Selecting it would move the daemon\='s `current' to a workspace
nobody switched to, and would clear an attention marker the user has not
looked at."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (setq agent-repl-test-host--current-ws "ws-elsewhere")
    ;; Act
    (agent-repl-test-host--subscribe "ws-1")
    ;; Assert
    (should (null (assoc "SelectWorkspace" agent-repl-test-host--calls)))))

;;;; ---- Mark viewed ----

(ert-deftest agent-repl-test-host-mark-viewed-echoes-the-ref ()
  "MarkWorkspaceViewed carries the daemon-minted ref, never a path."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    (setq agent-repl-test-host--calls nil)
    ;; Act
    (agent-repl-host-mark-viewed "ws-1")
    ;; Assert
    (should (equal (car agent-repl-test-host--calls)
                   (list "MarkWorkspaceViewed"
                         (nth 1 (car agent-repl-test-host--calls))
                         (list :workspace (agent-repl-test-host--ref)))))))

(ert-deftest agent-repl-test-host-mark-viewed-without-a-ref-sends-nothing ()
  "An unregistered workspace has no row to mark."
  (agent-repl-test-host--with-harness
    ;; Arrange / Act
    (agent-repl-host-mark-viewed "ws-unknown")
    ;; Assert
    (should (null agent-repl-test-host--calls))))

(ert-deftest agent-repl-test-host-mark-viewed-refusal-is-logged-at-error ()
  "A refused report is surfaced, never swallowed."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    (setq agent-repl-test-host--mark-viewed-answer
          (list :response (list :arm :error :value nil)))
    ;; Act
    (agent-repl-host-mark-viewed "ws-1")
    ;; Assert
    (should (agent-repl-test-host--logged-p :error "elisp.host.mark-viewed-refused"))))

(ert-deftest agent-repl-test-host-mark-viewed-transport-failure-is-logged-at-error ()
  "A report that never lands is recorded; there is no local state to undo."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    (setq agent-repl-test-host--mark-viewed-answer
          (list :failure (list :kind :transport :message "no route")))
    ;; Act
    (agent-repl-host-mark-viewed "ws-1")
    ;; Assert
    (should (agent-repl-test-host--logged-p :error "elisp.host.mark-viewed-failed"))))

;;;; ---- The workspace-subject verbs' one sender ----

(ert-deftest agent-repl-test-host-subject-verb-skip-records-its-reason ()
  "A workspace with no ref is not sent, records why, and runs ON-NOT-SENT."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (let (not-sent)
      ;; Act
      (let ((sent (agent-repl-host--send-subject-verb
                   "ws-unknown" "some-verb" #'agent-repl-rpc-mark-workspace-viewed
                   #'ignore (lambda () (setq not-sent t)))))
        ;; Assert
        (should (equal (list sent not-sent) '(nil t)))
        (should (null agent-repl-test-host--calls))))))

(ert-deftest agent-repl-test-host-subject-verb-success-runs-on-success-alone ()
  "A success answer runs ON-SUCCESS and never ON-NOT-SENT."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    (let (ran)
      ;; Act
      (agent-repl-host--send-subject-verb
       "ws-1" "some-verb" #'agent-repl-rpc-mark-workspace-viewed
       (lambda () (push 'success ran)) (lambda () (push 'not-sent ran)))
      ;; Assert
      (should (equal ran '(success))))))

(ert-deftest agent-repl-test-host-every-subject-verb-shares-the-one-sender ()
  "MarkWorkspaceViewed and ReportPageUnloading both send through the one sender.
A hand-rolled copy of either would not be recorded here."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (let (ops)
      (cl-letf (((symbol-function 'agent-repl-host--send-subject-verb)
                 (lambda (_ws op &rest _) (push op ops) t)))
        ;; Act
        (agent-repl-host-mark-viewed "ws-1")
        (agent-repl-host-report-page-unloading "ws-1" #'ignore #'ignore))
      ;; Assert
      (should (equal (reverse ops) '("mark-viewed" "page-unloading"))))))

;;;; ---- Report page unloading ----

(defun agent-repl-test-host--report-unloading (ws)
  "Report WS\='s page unloading; answer which callback ran: `reported' or `kept'."
  (let (outcome)
    (agent-repl-host-report-page-unloading
     ws (lambda () (setq outcome 'reported)) (lambda () (setq outcome 'kept)))
    outcome))

(ert-deftest agent-repl-test-host-page-unloading-echoes-the-ref ()
  "ReportPageUnloading carries the daemon-minted ref, and success lets the unload run."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    (setq agent-repl-test-host--calls nil)
    ;; Act
    (let ((outcome (agent-repl-test-host--report-unloading "ws-1")))
      ;; Assert
      (should (eq outcome 'reported))
      (should (equal (car agent-repl-test-host--calls)
                     (list "ReportPageUnloading"
                           (nth 1 (car agent-repl-test-host--calls))
                           (list :workspace (agent-repl-test-host--ref))))))))

(ert-deftest agent-repl-test-host-page-unloading-without-a-ref-keeps-the-page ()
  "An unregistered workspace is not reported, and its page stays loaded."
  (agent-repl-test-host--with-harness
    ;; Arrange / Act
    (let ((outcome (agent-repl-test-host--report-unloading "ws-unknown")))
      ;; Assert
      (should (eq outcome 'kept))
      (should (null agent-repl-test-host--calls)))))

(ert-deftest agent-repl-test-host-page-unloading-refusal-keeps-the-page ()
  "A refused report is surfaced, never swallowed, and keeps the page loaded."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    (setq agent-repl-test-host--unloading-answer
          (list :response (list :arm :error :value nil)))
    ;; Act
    (let ((outcome (agent-repl-test-host--report-unloading "ws-1")))
      ;; Assert
      (should (eq outcome 'kept))
      (should (agent-repl-test-host--logged-p :error "elisp.host.page-unloading-refused")))))

(ert-deftest agent-repl-test-host-page-unloading-transport-failure-keeps-the-page ()
  "A report that never lands is recorded at ERROR and keeps the page loaded."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    (setq agent-repl-test-host--unloading-answer
          (list :failure (list :kind :transport :message "no route")))
    ;; Act
    (let ((outcome (agent-repl-test-host--report-unloading "ws-1")))
      ;; Assert
      (should (eq outcome 'kept))
      (should (agent-repl-test-host--logged-p :error "elisp.host.page-unloading-failed")))))

(ert-deftest agent-repl-test-host-page-unloading-unknown-arm-keeps-the-page ()
  "An arm the verb does not know is an ERROR and keeps the page loaded."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    (setq agent-repl-test-host--unloading-answer
          (list :response (list :arm :mystery :value nil)))
    ;; Act
    (let ((outcome (agent-repl-test-host--report-unloading "ws-1")))
      ;; Assert
      (should (eq outcome 'kept))
      (should (agent-repl-test-host--logged-p :error "elisp.host.page-unloading-unknown-arm")))))

(defun agent-repl-test-host--standing-down-answer ()
  "The daemon's answer to a select while it is standing down."
  (list :response (list :arm :error
                        :value (list :cause (list :arm :standing-down :value nil)))))

(ert-deftest agent-repl-test-host-select-standing-down-is-logged-at-info ()
  "A leaving daemon's answer is an expected one, recorded at INFO."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    (setq agent-repl-test-host--select-answer (agent-repl-test-host--standing-down-answer))
    ;; Act
    (agent-repl-host-select "ws-1")
    ;; Assert
    (should (agent-repl-test-host--logged-p :info "elisp.host.select-standing-down ws=ws-1"))))

(ert-deftest agent-repl-test-host-select-standing-down-reports-no-refusal ()
  "No ERROR is recorded for a select a leaving daemon answered."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    (setq agent-repl-test-host--select-answer (agent-repl-test-host--standing-down-answer))
    ;; Act
    (agent-repl-host-select "ws-1")
    ;; Assert
    (should-not (cl-find-if (lambda (entry) (eq (car entry) :error))
                            agent-repl-test-host--logs))))

(ert-deftest agent-repl-test-host-select-standing-down-records-the-selection ()
  "The leaving daemon stamped the selection, so Emacs records it as its own."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    (setq agent-repl-test-host--select-answer (agent-repl-test-host--standing-down-answer))
    ;; Act
    (agent-repl-host-select "ws-1")
    ;; Assert
    (should (equal agent-repl-host-last-selected-id "ws-id-1"))))

(ert-deftest agent-repl-test-host-select-standing-down-settles-as-standing-down ()
  "The caller holding state on the selection hears the leaving daemon's answer."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    (setq agent-repl-test-host--select-answer (agent-repl-test-host--standing-down-answer))
    (let ((outcome nil))
      ;; Act
      (agent-repl-host-select "ws-1" (lambda (o) (setq outcome o)))
      ;; Assert
      (should (eq outcome :standing-down)))))

(ert-deftest agent-repl-test-host-select-refusal-is-logged-at-error ()
  "A refused selection is surfaced, never swallowed."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    (setq agent-repl-test-host--select-answer
          (list :response (list :arm :error :value nil)))
    ;; Act
    (agent-repl-host-select "ws-1")
    ;; Assert
    (should (agent-repl-test-host--logged-p :error "elisp.host.select-refused"))))

(ert-deftest agent-repl-test-host-select-refusal-does-not-record-the-id ()
  "A refusal is the daemon saying it stamped nothing as `current'."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    (setq agent-repl-test-host--select-answer
          (list :response (list :arm :error
                                :value (list :cause (list :arm :unknown-workspace
                                                          :value nil)))))
    ;; Act
    (agent-repl-host-select "ws-1")
    ;; Assert
    (should (null agent-repl-host-last-selected-id))))

(ert-deftest agent-repl-test-host-select-transport-failure-records-the-selection-to-reassert ()
  "Nobody answering leaves the user's choice standing, even with the link
still up as the failure lands: the next link-up re-asserts it, so the
order of the failure and the link-down edge decides nothing (2026-10-07)."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    (setq agent-repl-test-host--select-answer
          (list :failure (list :kind :transport :message "no route")))
    ;; Act
    (agent-repl-host-select "ws-1")
    ;; Assert
    (should (equal agent-repl-host-last-selected-id "ws-id-1"))))

(ert-deftest agent-repl-test-host-select-transport-failure-with-the-link-up-is-logged-at-error ()
  "With the link up, a select nobody answered is the error it is."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    (setq agent-repl-test-host--select-answer
          (list :failure (list :kind :transport :message "no route")))
    ;; Act
    (agent-repl-host-select "ws-1")
    ;; Assert
    (should (agent-repl-test-host--logged-p :error "elisp.host.select-failed ws=ws-1"))))

(defun agent-repl-test-host--select-failing-as-the-daemon-goes ()
  "Arrange a select whose daemon goes away under it: the link is down as
the transport failure lands."
  (agent-repl-test-host--subscribe "ws-1")
  (setq agent-repl-test-host--link-up nil)
  (setq agent-repl-test-host--select-answer
        (list :failure (list :kind :http :code "unavailable" :status 503
                             :message "the daemon is shutting down"))))

(ert-deftest agent-repl-test-host-select-failing-as-the-daemon-goes-records-the-selection ()
  "The user's choice is carried to the next daemon, as a standing-down
answer's is: the link-up re-asserts it."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--select-failing-as-the-daemon-goes)
    ;; Act
    (agent-repl-host-select "ws-1")
    ;; Assert
    (should (equal agent-repl-host-last-selected-id "ws-id-1"))))

(ert-deftest agent-repl-test-host-select-failing-as-the-daemon-goes-settles-as-standing-down ()
  "Its caller hears the same outcome a leaving daemon's answer gives."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--select-failing-as-the-daemon-goes)
    (let ((outcome nil))
      ;; Act
      (agent-repl-host-select "ws-1" (lambda (o) (setq outcome o)))
      ;; Assert
      (should (eq outcome :standing-down)))))

(ert-deftest agent-repl-test-host-select-failing-as-the-daemon-goes-is-logged-at-info ()
  "An expected loss, recorded at INFO with the failure's detail, and no ERROR."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--select-failing-as-the-daemon-goes)
    ;; Act
    (agent-repl-host-select "ws-1")
    ;; Assert
    (should (agent-repl-test-host--logged-p :info "elisp.host.select-daemon-gone ws=ws-1"))
    (should-not (cl-find-if (lambda (entry) (eq (car entry) :error))
                            agent-repl-test-host--logs))))

(defun agent-repl-test-host--drop-every-connection ()
  "Leave every subscribed workspace with no connection, as a link-down does."
  (maphash (lambda (ws _) (agent-repl-host--put ws :conn nil)) agent-repl-host--by-name))

(ert-deftest agent-repl-test-host-select-with-no-connection-records-the-selection ()
  "A select with no daemon to send it to is still the user's: the next
link-up re-asserts it."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    (agent-repl-test-host--drop-every-connection)
    ;; Act
    (agent-repl-host-select "ws-1")
    ;; Assert
    (should (equal agent-repl-host-last-selected-id "ws-id-1"))))

(ert-deftest agent-repl-test-host-select-with-no-connection-sends-nothing ()
  "No SelectWorkspace goes out with no connection to carry it."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    (agent-repl-test-host--drop-every-connection)
    (setq agent-repl-test-host--calls nil)
    ;; Act
    (agent-repl-host-select "ws-1")
    ;; Assert
    (should (null (agent-repl-test-host--selected-ids)))))

(ert-deftest agent-repl-test-host-select-with-no-connection-settles-as-standing-down ()
  "Its caller is settled as a leaving daemon's answer settles it, so a
re-assertion waiting on it lets go."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    (agent-repl-test-host--drop-every-connection)
    (let ((outcome nil))
      ;; Act
      (agent-repl-host-select "ws-1" (lambda (o) (setq outcome o)))
      ;; Assert
      (should (eq outcome :standing-down)))))

(ert-deftest agent-repl-test-host-select-with-no-connection-is-logged-at-info ()
  "An expected deferral, recorded at INFO with the id, and no WARN."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    (agent-repl-test-host--drop-every-connection)
    ;; Act
    (agent-repl-host-select "ws-1")
    ;; Assert
    (should (agent-repl-test-host--logged-p
             :info "elisp.host.select-deferred ws=ws-1 id=\"ws-id-1\" reason=no-connection reassert=next-link-up"))
    (should-not (cl-find-if (lambda (entry) (eq (car entry) :warn))
                            agent-repl-test-host--logs))))

(ert-deftest agent-repl-test-host-a-queued-selection-outlives-the-link-going-down ()
  "The user's newest choice is the one re-asserted: a selection queued
behind one a leaving daemon answers is recorded once the queue drains onto
a dead link, not lost to the older one (2026-10-14)."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe-three)
    (agent-repl-host-select "ws-1")
    (agent-repl-host-select "ws-2")
    (agent-repl-test-host--drop-every-connection)
    ;; Act
    (agent-repl-test-host--answer-held (plist-get (agent-repl-test-host--standing-down-answer) :response))
    ;; Assert
    (should (equal agent-repl-host-last-selected-id "ws-id-2"))))

;;;; ---- Select: one at a time, the last press wins ----

(defconst agent-repl-test-host--success '(:arm :success :value (:roster-revision 0))
  "A SelectWorkspace success answer.")

(defun agent-repl-test-host--subscribe-three ()
  "Subscribe ws-1, ws-2 and ws-3, each with its own ref id."
  (dolist (n '(1 2 3))
    (agent-repl-test-host--subscribe
     (format "ws-%d" n) nil
     (agent-repl-test-host--ref (format "ws-id-%d" n) (format "/tmp/ws-%d" n))))
  (setq agent-repl-test-host--calls nil
        agent-repl-test-host--select-answer '(:held)))

(ert-deftest agent-repl-test-host-rapid-requests-send-only-the-first-at-once ()
  "Three presses before any answer put ONE SelectWorkspace on the wire."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe-three)
    ;; Act
    (agent-repl-host-request-switch "ws-1" 'cycle)
    (agent-repl-host-request-switch "ws-2" 'cycle)
    (agent-repl-host-request-switch "ws-3" 'cycle)
    ;; Assert
    (should (equal (agent-repl-test-host--selected-ids) '("ws-id-1")))))

(ert-deftest agent-repl-test-host-rapid-requests-end-on-the-last-press ()
  "Rapid presses reach the daemon in order and the LAST press is the last
selection sent; a press already passed over is never sent at all."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe-three)
    (agent-repl-host-request-switch "ws-1" 'cycle)
    (agent-repl-host-request-switch "ws-2" 'cycle)
    (agent-repl-host-request-switch "ws-3" 'cycle)
    ;; Act
    (agent-repl-test-host--answer-held agent-repl-test-host--success)
    (agent-repl-test-host--answer-held agent-repl-test-host--success)
    ;; Assert
    (should (equal (agent-repl-test-host--selected-ids) '("ws-id-1" "ws-id-3")))))

(ert-deftest agent-repl-test-host-a-replaced-queued-selection-settles-superseded ()
  "The caller of a selection replaced before it was sent is told so."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (let ((outcome nil))
      (agent-repl-test-host--subscribe-three)
      (agent-repl-host-select "ws-1")
      (agent-repl-host-select "ws-2" (lambda (o) (setq outcome o)))
      ;; Act
      (agent-repl-host-select "ws-3")
      ;; Assert
      (should (eq outcome :superseded)))))

(ert-deftest agent-repl-test-host-pending-selection-is-the-newest-queued ()
  "With one in flight and one queued, the queued one is the newest request."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe-three)
    ;; Act
    (agent-repl-host-select "ws-1")
    (agent-repl-host-select "ws-2")
    ;; Assert
    (should (equal (agent-repl-host-pending-selection) "ws-2"))))

(ert-deftest agent-repl-test-host-pending-selection-is-the-one-in-flight ()
  "With nothing queued, the selection in flight is the pending one."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe-three)
    ;; Act
    (agent-repl-host-select "ws-1")
    ;; Assert
    (should (equal (agent-repl-host-pending-selection) "ws-1"))))

(ert-deftest agent-repl-test-host-pending-selection-clears-once-the-answered-roster-is-held ()
  "Once every selection is answered AND the roster carrying it is held,
nothing is pending; between the two the answered one still is."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe-three)
    (agent-repl-host-select "ws-1")
    (agent-repl-test-host--answer-held agent-repl-test-host--success)
    (should (equal (agent-repl-host-pending-selection) "ws-1"))
    ;; Act
    (agent-repl-host-release-confirmed 0)
    ;; Assert
    (should (null (agent-repl-host-pending-selection)))))

(ert-deftest agent-repl-test-host-the-last-answer-re-applies-the-roster ()
  "When nothing is left pending the roster's `current' is judged again,
so a push deferred behind the request is obeyed: the last push wins."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe-three)
    (agent-repl-host-select "ws-1")
    ;; Act
    (agent-repl-test-host--answer-held agent-repl-test-host--success)
    ;; Assert
    (should (= agent-repl-test-host--reapplied 1))))

(ert-deftest agent-repl-test-host-an-answer-with-a-queued-selection-does-not-re-apply ()
  "While a queued selection is still to be sent, nothing is re-applied yet."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe-three)
    (agent-repl-host-select "ws-1")
    (agent-repl-host-select "ws-2")
    ;; Act
    (agent-repl-test-host--answer-held agent-repl-test-host--success)
    ;; Assert
    (should (= agent-repl-test-host--reapplied 0))))

(ert-deftest agent-repl-test-host-a-refused-selection-re-applies-the-roster ()
  "A refusal stamped nothing, so the frame must follow whatever the roster
says now rather than wait for a push that will never come."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe-three)
    (agent-repl-host-select "ws-1")
    ;; Act
    (agent-repl-test-host--answer-held '(:arm :error :value nil))
    ;; Assert
    (should (= agent-repl-test-host--reapplied 1))))

(ert-deftest agent-repl-test-host-a-failed-selection-releases-the-slot ()
  "A transport failure answers the call too, so the next one is sent."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe-three)
    (setq agent-repl-test-host--select-answer
          (list :failure (list :kind :transport :message "no route")))
    (agent-repl-host-select "ws-1")
    ;; Act
    (agent-repl-host-select "ws-2")
    ;; Assert
    (should (equal (agent-repl-test-host--selected-ids) '("ws-id-1" "ws-id-2")))))

(ert-deftest agent-repl-test-host-an-unsent-selection-releases-the-slot ()
  "A call that signals before going out leaves no selection in flight."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe-three)
    (cl-letf (((symbol-function 'agent-repl-rpc-select-workspace)
               (lambda (&rest _) (error "socket refused"))))
      ;; Act
      (ignore-errors (agent-repl-host-select "ws-1")))
    ;; Assert
    (should (null agent-repl-host--select-in-flight))))

(ert-deftest agent-repl-test-host-an-unsent-selection-signals ()
  "The error of a call that never went out reaches the caller."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe-three)
    (cl-letf (((symbol-function 'agent-repl-rpc-select-workspace)
               (lambda (&rest _) (error "socket refused"))))
      ;; Act / Assert
      (should-error (agent-repl-host-select "ws-1")))))

;;;; ---- Request a switch ----

(defun agent-repl-test-host--selected-keeps ()
  "The `:keep-selection-history' of each SelectWorkspace sent, oldest first."
  (mapcar (lambda (request) (plist-get request :keep-selection-history))
          (agent-repl-test-host--select-requests)))

(ert-deftest agent-repl-test-host-select-requests-are-oldest-first-and-selects-only ()
  "The shared reader keeps only SelectWorkspace calls, in the order sent."
  ;; Arrange
  (let ((agent-repl-test-host--calls
         (list (list "SelectWorkspace" nil '(:workspace (:id "b")))
               (list "MarkWorkspaceViewed" nil '(:workspace (:id "x")))
               (list "SelectWorkspace" nil '(:workspace (:id "a"))))))
    ;; Act / Assert
    (should (equal (agent-repl-test-host--select-requests)
                   '((:workspace (:id "a")) (:workspace (:id "b")))))
    (should (equal (agent-repl-test-host--selected-ids) '("a" "b")))))

(ert-deftest agent-repl-test-host-a-recency-walk-request-keeps-the-selection-history ()
  "A `recency-walk' step asks the daemon to leave the last-selected instant."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe-three)
    ;; Act
    (agent-repl-host-request-switch "ws-2" 'recency-walk)
    ;; Assert
    (should (equal (agent-repl-test-host--selected-keeps) '(t)))))

(ert-deftest agent-repl-test-host-a-chosen-request-records-the-selection-history ()
  "Every other trigger is a choice, so the daemon stamps the instant."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe-three)
    ;; Act
    (agent-repl-host-request-switch "ws-2" 'cycle)
    ;; Assert
    (should (equal (agent-repl-test-host--selected-keeps) '(nil)))))

(ert-deftest agent-repl-test-host-a-recency-walk-request-marks-its-landing ()
  "The walk's target is marked before the frame moves, so its activation is no visit."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe-three)
    (let ((seen :unset))
      (cl-letf (((symbol-function 'agent-repl-host--move-on-request)
                 (lambda (_ws _shown) (setq seen agent-repl--recency-walk-landing))))
        ;; Act
        (agent-repl-host-request-switch "ws-2" 'recency-walk))
      ;; Assert
      (should (equal seen "ws-2"))
      (should (equal agent-repl--recency-walk-landing "ws-2")))))

(ert-deftest agent-repl-test-host-a-chosen-request-ends-the-walk ()
  "Any other trigger clears the walk's landing mark."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe-three)
    (setq agent-repl--recency-walk-landing "ws-1")
    ;; Act
    (agent-repl-host-request-switch "ws-2" 'slot)
    ;; Assert
    (should-not agent-repl--recency-walk-landing)))

(ert-deftest agent-repl-test-host-a-queued-walk-step-keeps-the-history-when-sent ()
  "A walk step queued behind one in flight is sent with its own keep flag."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe-three)
    (agent-repl-host-request-switch "ws-1" 'cycle)
    (agent-repl-host-request-switch "ws-3" 'recency-walk)
    ;; Act
    (agent-repl-test-host--answer-held agent-repl-test-host--success)
    ;; Assert
    (should (equal (agent-repl-test-host--selected-ids) '("ws-id-1" "ws-id-3")))
    (should (equal (agent-repl-test-host--selected-keeps) '(nil t)))))

(ert-deftest agent-repl-test-host-select-records-its-keep-flag ()
  "The send record names whether the select keeps the history."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe-three)
    (setq agent-repl-test-host--logs nil)
    ;; Act
    (agent-repl-host-select "ws-2" nil t)
    ;; Assert
    (should (cl-some (lambda (line) (string-match-p "elisp.host.select ws=ws-2 .*keep-history=t" line))
                     (mapcar (lambda (l) (if (stringp l) l (format "%S" l))) agent-repl-test-host--logs)))))


(ert-deftest agent-repl-test-host-a-request-sends-select-workspace ()
  "A switch request is a SelectWorkspace for the workspace's ref."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe-three)
    ;; Act
    (agent-repl-host-request-switch "ws-2" 'slot)
    ;; Assert
    (should (equal (agent-repl-test-host--selected-ids) '("ws-id-2")))))

(ert-deftest agent-repl-test-host-a-request-is-remembered-for-the-follow-record ()
  "The newest request names its workspace, for the follow's latency field."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe-three)
    ;; Act
    (agent-repl-host-request-switch "ws-2" 'slot)
    ;; Assert
    (should (equal (car agent-repl-host-last-request) "ws-2"))))

(ert-deftest agent-repl-test-host-a-request-is-recorded-at-info ()
  "The user's request is durable, naming its target and its trigger."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe-three)
    (setq agent-repl-test-host--current-ws "ws-1")
    ;; Act
    (agent-repl-host-request-switch "ws-2" 'slot)
    ;; Assert
    (should (agent-repl-test-host--logged-p
             :info "elisp.host.switch-requested ws=ws-2 trigger=slot shown=ws-1 pending=none"))))

(ert-deftest agent-repl-test-host-a-request-moves-no-frame ()
  "A request never switches the frame itself."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (let ((switched nil))
      (agent-repl-test-host--subscribe-three)
      (cl-letf (((symbol-function 'agent-repl--ws-switch)
                 (lambda (ws &rest _) (setq switched ws))))
        ;; Act
        (agent-repl-host-request-switch "ws-2" 'slot))
      ;; Assert
      (should (null switched)))))

(ert-deftest agent-repl-test-host-a-request-without-a-ref-warns ()
  "A switch the user asked for that cannot be sent is reported, not dropped."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (cl-letf (((symbol-function 'message) (lambda (&rest _) nil)))
      ;; Act
      (agent-repl-host-request-switch "ws-unknown" 'slot))
    ;; Assert
    (should (agent-repl-test-host--logged-p
             :warn "elisp.host.switch-request-unsent ws=ws-unknown trigger=slot reason=no-ref"))))

(ert-deftest agent-repl-test-host-a-request-without-a-connection-warns ()
  "No daemon connection is reported, not dropped."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe-three)
    (agent-repl-host--put "ws-2" :conn nil)
    (cl-letf (((symbol-function 'message) (lambda (&rest _) nil)))
      ;; Act
      (agent-repl-host-request-switch "ws-2" 'slot))
    ;; Assert
    (should (agent-repl-test-host--logged-p
             :warn "elisp.host.switch-request-unsent ws=ws-2 trigger=slot reason=no-connection"))))

(ert-deftest agent-repl-test-host-a-request-that-cannot-be-sent-tells-the-user ()
  "The echo area says why the switch went nowhere."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (let ((said nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (setq said (apply #'format fmt args)))))
        ;; Act
        (agent-repl-host-request-switch "ws-unknown" 'slot))
      ;; Assert
      (should (string-match-p "Cannot switch to ws-unknown" said)))))

(ert-deftest agent-repl-test-host-an-activation-following-the-roster-selects-nothing ()
  "The roster moved the frame, so the daemon already holds the selection;
an echo would race a newer request and take the frame back."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    (setq agent-repl-test-host--current-ws "ws-1"
          agent-repl-test-host--calls nil)
    ;; Act
    (let ((agent-repl-roster-following t))
      (agent-repl-host--on-workspace-activated))
    ;; Assert
    (should (null agent-repl-test-host--calls))))

(ert-deftest agent-repl-test-host-a-local-activation-is-recorded-at-info ()
  "A switch Emacs made on its own is told to the daemon, and recorded."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    (setq agent-repl-test-host--current-ws "ws-1")
    ;; Act
    (let ((this-command 'some-command))
      (agent-repl-host--on-workspace-activated))
    ;; Assert
    (should (agent-repl-test-host--logged-p
             :info "elisp.host.select-local-activation ws=ws-1 command=some-command"))))

;;;; ---- Counting the switches the user makes in Emacs ----

(ert-deftest agent-repl-test-host-a-switch-request-is-counted ()
  "A switch the user asked for in Emacs is counted."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act
    (agent-repl-host-request-switch "ws-1" 'cycle)
    ;; Assert
    (should (= (agent-repl-host-editor-switch-count) 1))))

(ert-deftest agent-repl-test-host-a-switch-request-is-counted-even-unsent ()
  "A request that could not be sent is still the user's move in Emacs."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (cl-letf (((symbol-function 'message) #'ignore))
      ;; Act
      (agent-repl-host-request-switch "ws-unknown" 'slot))
    ;; Assert
    (should (= (agent-repl-host-editor-switch-count) 1))))

(ert-deftest agent-repl-test-host-a-counted-switch-notes-its-instant-for-the-page ()
  "A counted switch notes its start with the webhost, for the page to measure."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    (let ((noted nil))
      (cl-letf (((symbol-function 'agent-repl-webhost-note-switch)
                 (lambda (ws) (push ws noted))))
        ;; Act
        (agent-repl-host-request-switch "ws-1" 'picker))
      ;; Assert
      (should (equal noted '("ws-1"))))))

(ert-deftest agent-repl-test-host-the-startup-selection-notes-no-switch ()
  "The startup's own selection is no switch the user made, so nothing is noted."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    (let ((noted nil))
      (cl-letf (((symbol-function 'agent-repl-webhost-note-switch)
                 (lambda (ws) (push ws noted))))
        ;; Act
        (agent-repl-host-request-switch "ws-1" 'startup))
      ;; Assert
      (should-not noted))))

(ert-deftest agent-repl-test-host-the-startup-selection-is-not-counted ()
  "The startup selecting its first tab is Emacs's own, not the user's."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act
    (agent-repl-host-request-switch "ws-1" 'startup)
    ;; Assert
    (should (= (agent-repl-host-editor-switch-count) 0))))

(ert-deftest agent-repl-test-host-a-counted-switch-is-recorded-at-info ()
  "Every counted switch is in the log with its origin and the new count."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act
    (agent-repl-host-request-switch "ws-1" 'picker)
    ;; Assert
    (should (agent-repl-test-host--logged-p
             :info "elisp.host.editor-switch ws=ws-1 origin=picker count=1"))))

(ert-deftest agent-repl-test-host-a-local-activation-is-counted ()
  "A perspective activation an Emacs command made is the user's switch."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    (setq agent-repl-test-host--current-ws "ws-1")
    ;; Act
    (agent-repl-host--on-workspace-activated)
    ;; Assert
    (should (= (agent-repl-host-editor-switch-count) 1))))

(ert-deftest agent-repl-test-host-an-activation-onto-a-plain-perspective-is-counted ()
  "Switching to a perspective that is no workspace still moves the user."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (setq agent-repl-test-host--current-ws "main")
    ;; Act
    (agent-repl-host--on-workspace-activated)
    ;; Assert
    (should (= (agent-repl-host-editor-switch-count) 1))))

(ert-deftest agent-repl-test-host-an-activation-following-the-roster-is-not-counted ()
  "A follow of the roster is a sidebar's or an agent's switch, never Emacs's."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    (setq agent-repl-test-host--current-ws "ws-1")
    ;; Act
    (let ((agent-repl-roster-following t))
      (agent-repl-host--on-workspace-activated))
    ;; Assert
    (should (= (agent-repl-host-editor-switch-count) 0))))

(ert-deftest agent-repl-test-host-a-background-activation-is-not-counted ()
  "A transient background activation moves the user nowhere."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    (setq agent-repl-test-host--current-ws "ws-1")
    ;; Act
    (let ((agent-repl--eager-open-in-progress t))
      (agent-repl-host--on-workspace-activated))
    ;; Assert
    (should (= (agent-repl-host-editor-switch-count) 0))))

(ert-deftest agent-repl-test-host-an-automatic-activation-is-not-counted ()
  "A move Emacs made on its own account is not the user's switch."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    (setq agent-repl-test-host--current-ws "ws-1")
    ;; Act
    (let ((agent-repl-host-automatic-switch t))
      (agent-repl-host--on-workspace-activated))
    ;; Assert
    (should (= (agent-repl-host-editor-switch-count) 0))))

(ert-deftest agent-repl-test-host-an-automatic-activation-still-selects ()
  "Not counting an automatic move does not keep it from the daemon."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    (setq agent-repl-test-host--current-ws "ws-1"
          agent-repl-test-host--calls nil)
    ;; Act
    (let ((agent-repl-host-automatic-switch t))
      (agent-repl-host--on-workspace-activated))
    ;; Assert
    (should (equal (car (car agent-repl-test-host--calls)) "SelectWorkspace"))))

(ert-deftest agent-repl-test-host-an-uncounted-automatic-activation-is-recorded ()
  "An activation left uncounted says why."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    (setq agent-repl-test-host--current-ws "ws-1")
    ;; Act
    (let ((agent-repl-host-automatic-switch t))
      (agent-repl-host--on-workspace-activated))
    ;; Assert
    (should (agent-repl-test-host--logged-p
             :log "elisp.host.editor-switch-uncounted ws=ws-1 reason=automatic"))))

(ert-deftest agent-repl-test-host-select-transferring-away-goes-to-the-handover ()
  "The refusal is where a lagging client learns the workspace moved."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    (setq agent-repl-test-host--select-answer
          (list :response
                (list :arm :error
                      :value (list :cause (list :arm :transferring-away
                                                :value (list :address "127.0.0.1:9100"))))))
    (let ((seen nil))
      (cl-letf (((symbol-function 'agent-repl-host-handle-refusal)
                 (lambda (_ws arm) (setq seen arm))))
        ;; Act
        (agent-repl-host-select "ws-1")
        ;; Assert
        (should (eq (plist-get seen :arm) :transferring-away))))))

(ert-deftest agent-repl-test-host-select-transferring-away-is-not-an-error ()
  "A handover is news about where the workspace went, not a failed verb."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    (setq agent-repl-test-host--select-answer
          (list :response
                (list :arm :error
                      :value (list :cause (list :arm :transferring-away
                                                :value (list :address "127.0.0.1:9100"))))))
    (cl-letf (((symbol-function 'agent-repl-host-handle-refusal) #'ignore))
      ;; Act
      (agent-repl-host-select "ws-1")
      ;; Assert
      (should-not (agent-repl-test-host--logged-p :error "elisp.host.select-refused")))))

(ert-deftest agent-repl-test-host-select-non-handover-refusal-stays-an-error ()
  "Every arm that is not a handover is still a refusal of the verb."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    (setq agent-repl-test-host--select-answer
          (list :response
                (list :arm :error
                      :value (list :cause (list :arm :workspace-ref-mismatch
                                                :value (list :registry-dir "/x"))))))
    ;; Act
    (agent-repl-host-select "ws-1")
    ;; Assert
    (should (agent-repl-test-host--logged-p :error "elisp.host.select-refused"))))

;;;; ---- Subscribe ----

(ert-deftest agent-repl-test-host-subscribe-echoes-the-ref-on-the-stream ()
  "One subscription per open workspace, addressed by the minted identity."
  (agent-repl-test-host--with-harness
    ;; Arrange / Act
    (let ((stream (agent-repl-test-host--subscribe "ws-1")))
      ;; Assert
      (should (equal (plist-get stream :ref) (agent-repl-test-host--ref))))))

(ert-deftest agent-repl-test-host-subscribe-records-the-owning-connection ()
  "The conn that serves a workspace is what its verbs are sent over."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (let ((conn (agent-repl-connect-open "127.0.0.1:9001")))
      ;; Act
      (agent-repl-test-host--subscribe "ws-1" conn)
      ;; Assert
      (should (eq (agent-repl-host-conn "ws-1") conn)))))

(ert-deftest agent-repl-test-host-subscribe-stores-the-ref-on-the-workspace-plist ()
  "The ref is readable from workspace.el's plist too, never rebuilt from a path."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (let (put)
      (cl-letf (((symbol-function 'agent-repl--ws-put)
                 (lambda (ws key val) (push (list ws key val) put))))
        ;; Act
        (agent-repl-test-host--subscribe "ws-1"))
      ;; Assert
      (should (member (list "ws-1" :ref (agent-repl-test-host--ref)) put)))))

(ert-deftest agent-repl-test-host-unsubscribe-cancels-the-stream ()
  "Cancelling IS the graceful close; no CloseXConnection verb exists."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (let ((stream (agent-repl-test-host--subscribe "ws-1")))
      ;; Act
      (agent-repl-host-unsubscribe "ws-1")
      ;; Assert
      (should (equal agent-repl-test-host--cancelled (list stream))))))

(ert-deftest agent-repl-test-host-unsubscribe-is-idempotent ()
  "A workspace with no standing stream is already unsubscribed."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    (agent-repl-host-unsubscribe "ws-1")
    ;; Act
    (agent-repl-host-unsubscribe "ws-1")
    ;; Assert
    (should (= (length agent-repl-test-host--cancelled) 1))))

(ert-deftest agent-repl-test-host-producer-close-without-cancel-is-an-error ()
  "A standing stream the producer drops is a transport failure."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (let ((stream (agent-repl-test-host--subscribe "ws-1")))
      ;; Act
      (funcall (plist-get stream :on-close) '(:ended))
      ;; Assert
      (should (agent-repl-test-host--logged-p :error "elisp.host.stream-lost")))))

;;;; ---- The composer gate ----

(ert-deftest agent-repl-test-host-gate-without-a-push-is-unknown ()
  "No host push yet is `:unknown' — and it still sends."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act / Assert
    (should (eq (agent-repl-host-composer-gate "ws-1") :unknown))))

(ert-deftest agent-repl-test-host-gate-open-arm ()
  "The resolved composer arm IS the gate."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    (agent-repl-test-host--push
     "ws-1" (list :arm :host :value (agent-repl-test-host--composer :open)))
    ;; Act / Assert
    (should (eq (agent-repl-host-composer-gate "ws-1") :open))))

(ert-deftest agent-repl-test-host-gate-merging-arm ()
  "A merge lease owns the session, so the composer is closed."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    (agent-repl-test-host--push
     "ws-1" (list :arm :host :value (agent-repl-test-host--composer :merging)))
    ;; Act / Assert
    (should (eq (agent-repl-host-composer-gate "ws-1") :merging))))

(ert-deftest agent-repl-test-host-gate-draining-arm ()
  "A scheduled shutdown closes the composer."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    (agent-repl-test-host--push
     "ws-1" (list :arm :host :value (agent-repl-test-host--composer :draining)))
    ;; Act / Assert
    (should (eq (agent-repl-host-composer-gate "ws-1") :draining))))

(ert-deftest agent-repl-test-host-gate-restarting-arm ()
  "A graceful RestartWorkspace closes the composer."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    (agent-repl-test-host--push
     "ws-1" (list :arm :host :value (agent-repl-test-host--composer :restarting)))
    ;; Act / Assert
    (should (eq (agent-repl-host-composer-gate "ws-1") :restarting))))

(ert-deftest agent-repl-test-host-gate-retired-merge-parked-arm-is-an-error ()
  "The retired `:merge-parked' arm is outside the vocabulary: a breach.
A merge never parks (merge-landing.md, Landed change 1), so the gate
answers `:unknown' and records the breach rather than opening."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    (agent-repl-test-host--push
     "ws-1" (list :arm :host :value (agent-repl-test-host--composer :merge-parked)))
    ;; Act
    (let ((gate (agent-repl-host-composer-gate "ws-1")))
      ;; Assert
      (should (eq gate :unknown))
      (should (agent-repl-test-host--logged-p
               :error "elisp.host.gate-unknown-composer-arm")))))

(ert-deftest agent-repl-test-host-gate-no-session-arm ()
  "A workspace that never had a session is `:no-session' — and still sends."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    (agent-repl-test-host--push
     "ws-1" (list :arm :host :value (agent-repl-test-host--none)))
    ;; Act / Assert
    (should (eq (agent-repl-host-composer-gate "ws-1") :no-session))))

(ert-deftest agent-repl-test-host-gate-terminal-standing ()
  "A terminal standing is blocked by its own nature; the daemon revives."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    (agent-repl-test-host--push
     "ws-1" (list :arm :host :value (agent-repl-test-host--terminal t)))
    ;; Act / Assert
    (should (eq (agent-repl-host-composer-gate "ws-1") :terminal))))

(ert-deftest agent-repl-test-host-gate-unknown-composer-arm-is-an-error ()
  "An arm outside the fixed vocabulary is a contract breach."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    (agent-repl-test-host--push
     "ws-1" (list :arm :host :value (agent-repl-test-host--composer :teleported)))
    ;; Act
    (agent-repl-host-composer-gate "ws-1")
    ;; Assert
    (should (agent-repl-test-host--logged-p
             :error "elisp.host.gate-unknown-composer-arm"))))

(ert-deftest agent-repl-test-host-restart-hold-closes-the-gate-before-any-push ()
  "The requested forced restart closes the gate the daemon still calls open."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    (agent-repl-test-host--push
     "ws-1" (list :arm :host :value (agent-repl-test-host--composer :open)))
    ;; Act
    (agent-repl-host-take-restart-hold "ws-1")
    ;; Assert
    (should (eq (agent-repl-host-composer-gate "ws-1") :restarting))))

(ert-deftest agent-repl-test-host-restart-hold-survives-a-stale-open-push ()
  "A push from the DYING generation cannot reopen a gate the restart shut."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    (agent-repl-test-host--push
     "ws-1" (list :arm :host :value (agent-repl-test-host--composer :open)))
    (agent-repl-host-take-restart-hold "ws-1")
    ;; Act
    (agent-repl-test-host--push
     "ws-1" (list :arm :host :value (agent-repl-test-host--composer :open)))
    ;; Assert
    (should (eq (agent-repl-host-composer-gate "ws-1") :restarting))))

(ert-deftest agent-repl-test-host-restart-hold-ends-when-the-daemon-says-restarting ()
  "The daemon publishing `restarting' takes the fact back off the hold."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    (agent-repl-test-host--push
     "ws-1" (list :arm :host :value (agent-repl-test-host--composer :open)))
    (agent-repl-host-take-restart-hold "ws-1")
    ;; Act
    (agent-repl-test-host--push
     "ws-1" (list :arm :host :value (agent-repl-test-host--composer :restarting)))
    ;; Assert
    (should (null (plist-get (agent-repl-host--entry "ws-1") :restart-hold)))))

(ert-deftest agent-repl-test-host-restart-hold-ends-when-the-generation-rolls ()
  "The relaunched shim reporting a new generation reopens the composer."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    (agent-repl-test-host--push
     "ws-1" (list :arm :host :value (agent-repl-test-host--composer :open)))
    (agent-repl-host-take-restart-hold "ws-1")
    ;; Act
    (agent-repl-test-host--push
     "ws-1" (list :arm :host
                  :value (agent-repl-test-host--live
                          :generation (list :value "gen-2"))))
    ;; Assert
    (should (eq (agent-repl-host-composer-gate "ws-1") :open))))

(ert-deftest agent-repl-test-host-restart-hold-ends-when-the-stream-is-lost ()
  "No push can settle the hold once the standing stream drops, so it ends."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (let ((stream (agent-repl-test-host--subscribe "ws-1")))
      (agent-repl-test-host--push
       "ws-1" (list :arm :host :value (agent-repl-test-host--composer :open)))
      (agent-repl-host-take-restart-hold "ws-1")
      ;; Act
      (funcall (plist-get stream :on-close) '(:ended))
      ;; Assert
      (should (eq (agent-repl-host-composer-gate "ws-1") :open)))))

(ert-deftest agent-repl-test-host-parked-session-still-gates-open ()
  "`shim_attached' false has NO treatment: parked is invisible by design."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    (agent-repl-test-host--push
     "ws-1" (list :arm :host
                  :value (agent-repl-test-host--live :shim-attached :false)))
    ;; Act / Assert
    (should (eq (agent-repl-host-composer-gate "ws-1") :open))))

;;;; ---- Naming, backfill, faults ----

(ert-deftest agent-repl-test-host-display-title-prefers-the-title ()
  "Precedence, fixed: title beats slug beats the roster row name."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    (agent-repl-test-host--push
     "ws-1" (list :arm :host
                  :value (plist-put (agent-repl-test-host--live)
                                    :naming (list :slug "fix-reconnect"
                                                  :title "Fix the flaky reconnect"))))
    ;; Act / Assert
    (should (equal (agent-repl-host-display-title "ws-1") "Fix the flaky reconnect"))))

(ert-deftest agent-repl-test-host-display-title-falls-back-to-the-slug ()
  "An undelivered vendor title leaves the daemon-derived slug."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    (agent-repl-test-host--push
     "ws-1" (list :arm :host
                  :value (plist-put (agent-repl-test-host--live)
                                    :naming (list :slug "fix-reconnect" :title nil))))
    ;; Act / Assert
    (should (equal (agent-repl-host-display-title "ws-1") "fix-reconnect"))))

(ert-deftest agent-repl-test-host-display-title-falls-back-to-the-row-name ()
  "Both naming fields unset is the ordinary early state, not a failure."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    (agent-repl-test-host--push
     "ws-1" (list :arm :host :value (agent-repl-test-host--live)))
    ;; Act / Assert
    (should (equal (agent-repl-host-display-title "ws-1") "ws-1"))))

(ert-deftest agent-repl-test-host-backfill-reports-the-arm ()
  "The never-blue signal is read as its arm keyword."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    (agent-repl-test-host--push
     "ws-1" (list :arm :host
                  :value (agent-repl-test-host--live
                          :backfill (list :arm :pending :value nil))))
    ;; Act / Assert
    (should (eq (agent-repl-host-backfill "ws-1") :pending))))

(ert-deftest agent-repl-test-host-backfill-is-nil-without-a-live-session ()
  "Backfill lives on the LIVE arm and nowhere else."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    (agent-repl-test-host--push
     "ws-1" (list :arm :host :value (agent-repl-test-host--terminal nil)))
    ;; Act / Assert
    (should (null (agent-repl-host-backfill "ws-1")))))

(ert-deftest agent-repl-test-host-faults-are-read-off-the-live-arm ()
  "Standing faults are generation-scoped and live with the live session."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (let ((faults (list (list :detail "shim restarted twice" :opened-at-ms 1))))
      (agent-repl-test-host--subscribe "ws-1")
      (agent-repl-test-host--push
       "ws-1" (list :arm :host
                    :value (agent-repl-test-host--live :faults faults)))
      ;; Act / Assert
      (should (equal (agent-repl-host-faults "ws-1") faults)))))

(ert-deftest agent-repl-test-host-session-id-is-read-off-the-existing-arm ()
  "The logging accessor returns the daemon-minted session echo token."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    (agent-repl-test-host--push
     "ws-1" (list :arm :host :value (agent-repl-test-host--live)))
    ;; Act / Assert
    (should (equal (agent-repl-host-session-id "ws-1") "session-1"))))

(ert-deftest agent-repl-test-host-session-id-is-nil-without-a-session ()
  "A workspace with no session has no agent-repl session identifier."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    (agent-repl-test-host--push
     "ws-1" (list :arm :host :value (agent-repl-test-host--none)))
    ;; Act / Assert
    (should (null (agent-repl-host-session-id "ws-1")))))

(ert-deftest agent-repl-test-host-session-id-is-nil-before-the-first-push ()
  "The ordinary pre-push state carries no session identifier."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act / Assert
    (should (null (agent-repl-host-session-id "ws-1")))))

(ert-deftest agent-repl-test-host-vendor-session-id-is-read-off-the-claude-arm ()
  "The durable id is the vendor conversation's, not the daemon's echo token."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    (agent-repl-test-host--push
     "ws-1" (list :arm :host :value (agent-repl-test-host--live)))
    ;; Act / Assert
    (should (equal (agent-repl-host-vendor-session-id "ws-1") "vendor-1"))))

(ert-deftest agent-repl-test-host-vendor-session-id-is-nil-while-the-oneof-is-unset ()
  "No vendor conversation yet is the ordinary early state, not a failure."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    (agent-repl-test-host--push
     "ws-1" (list :arm :host
                  :value (agent-repl-test-host--live :vendor-info nil)))
    ;; Act / Assert
    (should (null (agent-repl-host-vendor-session-id "ws-1")))))

(ert-deftest agent-repl-test-host-vendor-session-id-is-nil-without-a-live-session ()
  "`vendor_info' lives on the LIVE arm and nowhere else."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    (agent-repl-test-host--push
     "ws-1" (list :arm :host :value (agent-repl-test-host--terminal t)))
    ;; Act / Assert
    (should (null (agent-repl-host-vendor-session-id "ws-1")))))

(ert-deftest agent-repl-test-host-a-push-that-names-the-vendor-conversation-is-recorded-at-info ()
  "The moment Emacs learns the durable id is visible in the logs."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    (agent-repl-test-host--push
     "ws-1" (list :arm :host
                  :value (agent-repl-test-host--live :vendor-info nil)))
    (setq agent-repl-test-host--logs nil)
    ;; Act
    (agent-repl-test-host--push
     "ws-1" (list :arm :host :value (agent-repl-test-host--live)))
    ;; Assert
    (should (member '(:info . "elisp.host.vendor-session ws=ws-1 from=nil to=\"vendor-1\"")
                    agent-repl-test-host--logs))))

(ert-deftest agent-repl-test-host-a-push-that-keeps-the-vendor-conversation-records-no-change ()
  "An unchanged vendor id is not a change and writes no record."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    (agent-repl-test-host--push
     "ws-1" (list :arm :host :value (agent-repl-test-host--live)))
    (setq agent-repl-test-host--logs nil)
    ;; Act
    (agent-repl-test-host--push
     "ws-1" (list :arm :host :value (agent-repl-test-host--live)))
    ;; Assert
    (should-not (cl-find-if (lambda (entry)
                              (string-prefix-p "elisp.host.vendor-session" (cdr entry)))
                            agent-repl-test-host--logs))))

(ert-deftest agent-repl-test-host-a-push-after-the-workspace-is-forgotten-is-dropped ()
  "A frame that arrives after `agent-repl-host-forget' re-creates nothing."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (let ((stream (agent-repl-test-host--subscribe "ws-1")))
      (agent-repl-host-forget "ws-1")
      ;; Act
      (funcall (plist-get stream :on-push)
               (list :arm :host :value (agent-repl-test-host--live)))
      ;; Assert
      (should-not (agent-repl-host--entry "ws-1")))))

(ert-deftest agent-repl-test-host-a-push-after-the-workspace-is-forgotten-is-recorded ()
  "The dropped frame is said at INFO, naming why."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (let ((stream (agent-repl-test-host--subscribe "ws-1")))
      (agent-repl-host-forget "ws-1")
      ;; Act
      (funcall (plist-get stream :on-push)
               (list :arm :host :value (agent-repl-test-host--live)))
      ;; Assert
      (should (agent-repl-test-host--logged-p
               :info "elisp.host.push-dropped ws=ws-1 arm=:host reason=forgotten")))))

(ert-deftest agent-repl-test-host-a-push-on-a-replaced-stream-is-dropped ()
  "A frame from a stream that no longer stands for WS does not touch its state."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (let ((old (agent-repl-test-host--subscribe "ws-1")))
      (agent-repl-test-host--subscribe "ws-1")
      ;; Act
      (funcall (plist-get old :on-push)
               (list :arm :host :value (agent-repl-test-host--live)))
      ;; Assert
      (should-not (agent-repl-host-state "ws-1")))))

(ert-deftest agent-repl-test-host-a-second-subscribe-cancels-the-standing-stream ()
  "No workspace ever holds two streams: the standing one is cancelled first."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (let ((old (agent-repl-test-host--subscribe "ws-1")))
      ;; Act
      (agent-repl-test-host--subscribe "ws-1")
      ;; Assert
      (should (memq old agent-repl-test-host--cancelled)))))

(ert-deftest agent-repl-test-host-state-push-runs-the-update-hook ()
  "Every host push is whole-replace, and consumers hear about it."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (let ((seen nil)
          (host (agent-repl-test-host--live)))
      (add-hook 'agent-repl-host-update-functions
                (lambda (ws state) (setq seen (list ws state))))
      (agent-repl-test-host--subscribe "ws-1")
      ;; Act
      (agent-repl-test-host--push "ws-1" (list :arm :host :value host))
      ;; Assert
      (should (equal seen (list "ws-1" host))))))

(ert-deftest agent-repl-test-host-naming-title-renames-the-input-buffer ()
  "Titles NAME THE BUFFERS: the composer's own name carries the title."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (let ((buffer (generate-new-buffer " *test-host-input*")))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-repl--ws-get)
                     (lambda (_ws key) (and (eq key :input-buffer) buffer))))
            (agent-repl-test-host--subscribe "ws-1")
            ;; Act
            (agent-repl-test-host--push
             "ws-1" (list :arm :host
                          :value (plist-put (agent-repl-test-host--live)
                                            :naming (list :slug "slug-1"
                                                          :title "Refactor the codec"))))
            ;; Assert
            (should (string-match-p (regexp-quote "Refactor the codec")
                                    (buffer-name buffer))))
        (kill-buffer buffer)))))

(ert-deftest agent-repl-test-host-naming-slug-names-the-input-buffer ()
  "With no vendor title the daemon-derived slug is what the buffer shows."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (let ((buffer (generate-new-buffer " *test-host-input*")))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-repl--ws-get)
                     (lambda (_ws key) (and (eq key :input-buffer) buffer))))
            (agent-repl-test-host--subscribe "ws-1")
            ;; Act
            (agent-repl-test-host--push
             "ws-1" (list :arm :host
                          :value (plist-put (agent-repl-test-host--live)
                                            :naming (list :slug "slug-1" :title nil))))
            ;; Assert
            (should (string-match-p (regexp-quote "slug-1") (buffer-name buffer))))
        (kill-buffer buffer)))))

(ert-deftest agent-repl-test-host-renamed-input-buffer-is-still-an-agent-panel ()
  "A titled composer must stay an agent panel to every name predicate."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (let ((buffer (generate-new-buffer " *test-host-input*")))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-repl--ws-get)
                     (lambda (_ws key) (and (eq key :input-buffer) buffer))))
            (agent-repl-test-host--subscribe "ws-1")
            ;; Act
            (agent-repl-test-host--push
             "ws-1" (list :arm :host
                          :value (plist-put (agent-repl-test-host--live)
                                            :naming (list :slug nil
                                                          :title "Refactor the codec"))))
            ;; Assert
            (should (agent-repl--agent-panel-buffer-p buffer)))
        (kill-buffer buffer)))))

(ert-deftest agent-repl-test-host-renamed-input-buffer-keeps-its-identity-segment ()
  "The workspace is still recoverable from the titled name."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (let ((buffer (generate-new-buffer " *test-host-input*")))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-repl--ws-get)
                     (lambda (_ws key) (and (eq key :input-buffer) buffer))))
            (agent-repl-test-host--subscribe "ws-1")
            ;; Act
            (agent-repl-test-host--push
             "ws-1" (list :arm :host
                          :value (plist-put (agent-repl-test-host--live)
                                            :naming (list :slug nil
                                                          :title "Refactor the codec"))))
            ;; Assert
            (should (equal (agent-repl--extract-panel-id (buffer-name buffer)) "ws-1")))
        (kill-buffer buffer)))))

(ert-deftest agent-repl-test-host-naming-without-a-title-or-slug-leaves-the-name-bare ()
  "Un-derived naming is the ordinary early state, not a title to write down."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (let ((buffer (generate-new-buffer " *test-host-input*")))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-repl--ws-get)
                     (lambda (_ws key) (and (eq key :input-buffer) buffer))))
            (agent-repl-test-host--subscribe "ws-1")
            ;; Act
            (agent-repl-test-host--push
             "ws-1" (list :arm :host :value (agent-repl-test-host--live)))
            ;; Assert
            (should (equal (buffer-name buffer) "*agent-panel-input-ws-1*")))
        (kill-buffer buffer)))))

;;;; ---- Routing a verb's handover refusal ----

(ert-deftest agent-repl-test-host-route-handover-routes-transferring-away ()
  "A `transferring_away' refusal goes to the handover walk."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (let (handed)
      (cl-letf (((symbol-function 'agent-repl-host-handle-refusal)
                 (lambda (ws arm) (push (list ws arm) handed))))
        ;; Act
        (agent-repl-host-route-handover
         "ws-1" '(:arm :transferring-away :value (:address "127.0.0.1:9")) "elisp.x.verb-handover"))
      ;; Assert
      (should (equal handed '(("ws-1" (:arm :transferring-away :value (:address "127.0.0.1:9")))))))))

(ert-deftest agent-repl-test-host-route-handover-routes-not-yet-adopted ()
  "A `not_yet_adopted' refusal goes to the handover walk."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (let (handed)
      (cl-letf (((symbol-function 'agent-repl-host-handle-refusal)
                 (lambda (_ws arm) (push (plist-get arm :arm) handed))))
        ;; Act
        (agent-repl-host-route-handover "ws-1" '(:arm :not-yet-adopted :value nil) "elisp.x.v"))
      ;; Assert
      (should (equal handed '(:not-yet-adopted))))))

(ert-deftest agent-repl-test-host-route-handover-answers-t-when-routed ()
  "A routed handover answers non-nil, so the caller reports nothing more."
  (agent-repl-test-host--with-harness
    (cl-letf (((symbol-function 'agent-repl-host-handle-refusal) #'ignore))
      ;; Act / Assert
      (should (agent-repl-host-route-handover "ws-1" '(:arm :not-yet-adopted :value nil) "elisp.x.v")))))

(ert-deftest agent-repl-test-host-route-handover-leaves-other-arms ()
  "Any other arm is the caller's: nil, and nothing is handed over."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (let (handed answer)
      (cl-letf (((symbol-function 'agent-repl-host-handle-refusal)
                 (lambda (&rest args) (push args handed))))
        ;; Act
        (setq answer (agent-repl-host-route-handover "ws-1" '(:arm :plan-stale :value nil) "elisp.x.v")))
      ;; Assert
      (should (equal (list answer handed) '(nil nil))))))

(ert-deftest agent-repl-test-host-route-handover-logs-under-the-slug ()
  "A routed handover is logged at INFO under the caller's slug."
  (agent-repl-test-host--with-harness
    (cl-letf (((symbol-function 'agent-repl-host-handle-refusal) #'ignore))
      ;; Act
      (agent-repl-host-route-handover "ws-1" '(:arm :not-yet-adopted :value nil)
                                      "elisp.input.rollback-plan-handover-refusal"))
    ;; Assert
    (should (agent-repl-test-host--logged-p
             :info "elisp.input.rollback-plan-handover-refusal ws=ws-1 arm=:not-yet-adopted"))))

;;;; ---- The feed selection ----

(ert-deftest agent-repl-test-host-selection-push-records-the-kind ()
  "A `selection' push records the kind of row the feed has selected."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act
    (agent-repl-test-host--push "ws-1" (list :arm :selection :value '(:kind :prompt :markdown "q")))
    ;; Assert
    (should (eq (agent-repl-host-selection "ws-1") :prompt))))

(ert-deftest agent-repl-test-host-selection-push-records-the-text ()
  "A `selection' push records the selected bubble's text for the search."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act
    (agent-repl-test-host--push "ws-1" (list :arm :selection :value '(:kind :bubble :markdown "meanwhile")))
    ;; Assert
    (should (equal (agent-repl-host-selection-markdown "ws-1") "meanwhile"))))

(ert-deftest agent-repl-test-host-selection-push-tells-the-composer ()
  "Every `selection' push reaches the composer, which follows it."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    (let (told)
      (cl-letf (((symbol-function 'agent-repl--input-selection-changed)
                 (lambda (ws) (push ws told))))
        ;; Act
        (agent-repl-test-host--push "ws-1" (list :arm :selection :value '(:kind :none :markdown nil))))
      ;; Assert
      (should (equal told '("ws-1"))))))

(ert-deftest agent-repl-test-host-selection-push-replaces-the-kind ()
  "A later `selection' push replaces the kind in force."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    (agent-repl-test-host--push "ws-1" (list :arm :selection :value '(:kind :response :markdown "a")))
    ;; Act
    (agent-repl-test-host--push "ws-1" (list :arm :selection :value '(:kind :none :markdown nil)))
    ;; Assert
    (should (eq (agent-repl-host-selection "ws-1") :none))))

(ert-deftest agent-repl-test-host-selection-is-nil-before-any-push ()
  "Before any `selection' push the kind is nil: nothing is selected."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act / Assert
    (should (null (agent-repl-host-selection "ws-1")))))

(ert-deftest agent-repl-test-host-selection-push-is-logged ()
  "The selection reaching Emacs leaves an INFO record naming the kind."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act
    (agent-repl-test-host--push "ws-1" (list :arm :selection :value '(:kind :response :markdown "abc")))
    ;; Assert
    (should (agent-repl-test-host--logged-p :info "elisp.host.selection ws=ws-1 kind=:response text-len=3"))))

;;;; ---- The notification click ----

(ert-deftest agent-repl-test-host-notification-click-selects-this-workspace ()
  "A `notification_clicked' push selects the workspace its stream carries."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act
    (agent-repl-test-host--push "ws-1" (list :arm :notification-clicked :value nil))
    ;; Assert
    (should (equal (car agent-repl-test-host--effects) '(:activate . "ws-1")))))

(ert-deftest agent-repl-test-host-notification-click-is-logged ()
  "The click reaching Emacs leaves an INFO record naming the workspace."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act
    (agent-repl-test-host--push "ws-1" (list :arm :notification-clicked :value nil))
    ;; Assert
    (should (agent-repl-test-host--logged-p :info "elisp.host.notification-clicked ws=ws-1"))))

;;;; ---- The rail's row click ----

(ert-deftest agent-repl-test-host-select-requested-sends-select-workspace ()
  "A `select_requested' push requests the switch to the workspace its stream carries."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe-three)
    ;; Act
    (agent-repl-test-host--push "ws-2" (list :arm :select-requested :value nil))
    ;; Assert
    (should (equal (agent-repl-test-host--selected-ids) '("ws-id-2")))))

(ert-deftest agent-repl-test-host-select-requested-is-a-sidebar-switch-request ()
  "The click takes the key's own path, recorded with the `sidebar' trigger."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe-three)
    (setq agent-repl-test-host--current-ws "ws-1")
    ;; Act
    (agent-repl-test-host--push "ws-2" (list :arm :select-requested :value nil))
    ;; Assert
    (should (agent-repl-test-host--logged-p
             :info "elisp.host.switch-requested ws=ws-2 trigger=sidebar shown=ws-1 pending=none"))))

(ert-deftest agent-repl-test-host-select-requested-is-counted ()
  "A sidebar row's click is the user's switch, counted as a key's is."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act
    (agent-repl-test-host--push "ws-1" (list :arm :select-requested :value nil))
    ;; Assert
    (should (= (agent-repl-host-editor-switch-count) 1))))

(ert-deftest agent-repl-test-host-select-requested-is-logged ()
  "The click reaching Emacs leaves an INFO record naming the workspace."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act
    (agent-repl-test-host--push "ws-1" (list :arm :select-requested :value nil))
    ;; Assert
    (should (agent-repl-test-host--logged-p :info "elisp.host.select-requested ws=ws-1"))))

;;;; ---- The final-response click ----

(ert-deftest agent-repl-test-host-final-response-click-focuses-this-workspaces-composer ()
  "A `final_response_clicked' push focuses the composer of the workspace its stream carries."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act
    (agent-repl-test-host--push "ws-1" (list :arm :final-response-clicked :value nil))
    ;; Assert
    (should (equal (car agent-repl-test-host--effects) '(:focus-for-reply . "ws-1")))))

(ert-deftest agent-repl-test-host-final-response-click-is-logged ()
  "The click reaching Emacs leaves an INFO record naming the workspace."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act
    (agent-repl-test-host--push "ws-1" (list :arm :final-response-clicked :value nil))
    ;; Assert
    (should (agent-repl-test-host--logged-p :info "elisp.host.final-response-clicked ws=ws-1"))))

;;;; ---- The prompt-entry click ----

(ert-deftest agent-repl-test-host-prompt-entry-click-fills-this-workspaces-composer ()
  "A `prompt_entry_clicked' push fills the composer of its stream's workspace with the entry."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act
    (agent-repl-test-host--push "ws-1" (list :arm :prompt-entry-clicked
                                              :value '(:row (:value "p1") :label "3.1" :text "yes")))
    ;; Assert
    (should (equal (car agent-repl-test-host--effects) '(:edit-entry "ws-1" "p1" "3.1" "yes")))))

(ert-deftest agent-repl-test-host-prompt-entry-click-is-logged ()
  "The entry click reaching Emacs leaves an INFO record naming the row and the label."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act
    (agent-repl-test-host--push "ws-1" (list :arm :prompt-entry-clicked
                                              :value '(:row (:value "p1") :label "3.1" :text "yes")))
    ;; Assert
    (should (agent-repl-test-host--logged-p :info "elisp.host.prompt-entry-clicked ws=ws-1 row=\"p1\" label=3.1"))))

;;;; ---- The handover ----

(ert-deftest agent-repl-test-host-transferred-adopts-on-the-successor ()
  "The adopt goes to the NEW daemon, echoing the same ref."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (let ((successor (agent-repl-connect-open "127.0.0.1:9100")))
      (setq agent-repl-test-host--successor successor)
      (agent-repl-test-host--subscribe "ws-1")
      (setq agent-repl-test-host--calls nil)
      ;; Act
      (agent-repl-test-host--push "ws-1" (list :arm :transferred :value nil))
      ;; Assert
      (should (equal (car agent-repl-test-host--calls)
                     (list "AdoptHostWorkspace" successor
                           (list :workspace (agent-repl-test-host--ref))))))))

(ert-deftest agent-repl-test-host-transferred-cancels-the-old-stream-after-adopting ()
  "Order is the contract's: adopt, THEN cancel, then re-subscribe."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (setq agent-repl-test-host--successor (agent-repl-connect-open "127.0.0.1:9100"))
    (let ((old (agent-repl-test-host--subscribe "ws-1")))
      ;; Act
      (agent-repl-test-host--push "ws-1" (list :arm :transferred :value nil))
      ;; Assert
      (should (equal agent-repl-test-host--cancelled (list old))))))

(ert-deftest agent-repl-test-host-transferred-resubscribes-on-the-successor ()
  "The workspace's stream moves to the daemon that now owns it."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (let ((successor (agent-repl-connect-open "127.0.0.1:9100")))
      (setq agent-repl-test-host--successor successor)
      (agent-repl-test-host--subscribe "ws-1")
      ;; Act
      (agent-repl-test-host--push "ws-1" (list :arm :transferred :value nil))
      ;; Assert
      (should (eq (agent-repl-host-conn "ws-1") successor)))))

(ert-deftest agent-repl-test-host-transferred-before-the-announcement-is-not-an-error ()
  "A notice that overtook its own announcement is an ordering, not a breach.
The daemon announces the stand-down and THEN transfers each free
workspace, and the two pushes ride different streams — so which one this
Emacs decodes first is a coin toss.  Measured in the e2e sandbox: the two
`WatchHostWorkspaceResponse\=' transfer pushes were decoded at
16:43:37.321 and the `DaemonShutdownAnnounced\=' at .322."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (setq agent-repl-test-host--successor nil
          agent-repl-test-host--successor-pending nil)
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act
    (agent-repl-test-host--push "ws-1" (list :arm :transferred :value nil))
    ;; Assert
    (should-not (agent-repl-test-host--logged-p
                 :error "elisp.host.transferred-without-successor"))))

(ert-deftest agent-repl-test-host-transferred-before-the-announcement-records-the-order ()
  "The overtaking order is recorded at INFO: it is ordinary, never a warning."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (setq agent-repl-test-host--successor nil
          agent-repl-test-host--successor-pending nil)
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act
    (agent-repl-test-host--push "ws-1" (list :arm :transferred :value nil))
    ;; Assert
    (should (agent-repl-test-host--logged-p
             :info "elisp.host.transferred-before-the-announcement"))
    (should-not (agent-repl-test-host--logged-p
                 :warn "elisp.host.transferred-before-the-announcement"))))

(ert-deftest agent-repl-test-host-transferred-before-the-announcement-adopts-on-acceptance ()
  "THE DEFECT: this adopt was dropped, so the workspace was never handed over.
The outgoing daemon then sat out its whole adoption window and the
successor was promoted only by the old stream dying underneath it, which
is `TestEmacsHandoverTransfersAtFreeness\=' waiting out 21s for a
promotion healthy runs make in about two."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (setq agent-repl-test-host--successor nil
          agent-repl-test-host--successor-pending nil)
    (agent-repl-test-host--subscribe "ws-1")
    (agent-repl-test-host--push "ws-1" (list :arm :transferred :value nil))
    (let ((successor (agent-repl-connect-open "127.0.0.1:9100")))
      ;; Act — daemon-link's acceptance seam, once the announcement lands
      (run-hook-with-args 'agent-repl-link-handover-functions nil successor)
      ;; Assert
      (should (equal (car agent-repl-test-host--calls)
                     (list "AdoptHostWorkspace" successor
                           (list :workspace (agent-repl-test-host--ref))))))))

(ert-deftest agent-repl-test-host-transferred-before-the-announcement-sends-no-adopt-yet ()
  "Nothing is sent to a daemon this Emacs has not even dialed."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (setq agent-repl-test-host--successor nil
          agent-repl-test-host--successor-pending nil)
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act
    (agent-repl-test-host--push "ws-1" (list :arm :transferred :value nil))
    ;; Assert
    (should (null (assoc "AdoptHostWorkspace" agent-repl-test-host--calls)))))

(ert-deftest agent-repl-test-host-transferred-before-the-announcement-keeps-the-stream ()
  "A workspace whose old stream was dropped would be served by nobody."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (setq agent-repl-test-host--successor nil)
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act
    (agent-repl-test-host--push "ws-1" (list :arm :transferred :value nil))
    ;; Assert
    (should (null agent-repl-test-host--cancelled))))

(ert-deftest agent-repl-test-host-transferred-while-the-successor-is-pending-is-not-an-error ()
  "The announced successor is dialed but unaccepted; that is a wait, not a breach."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (setq agent-repl-test-host--successor nil
          agent-repl-test-host--successor-pending t)
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act
    (agent-repl-test-host--push "ws-1" (list :arm :transferred :value nil))
    ;; Assert
    (should-not (agent-repl-test-host--logged-p
                 :error "elisp.host.transferred-without-successor"))))

(ert-deftest agent-repl-test-host-transferred-while-the-successor-is-pending-records-the-wait ()
  "\"Where did this transfer go\" is a real question and this line is its answer."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (setq agent-repl-test-host--successor nil
          agent-repl-test-host--successor-pending t)
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act
    (agent-repl-test-host--push "ws-1" (list :arm :transferred :value nil))
    ;; Assert
    (should (agent-repl-test-host--logged-p
             :info "elisp.host.transferred-awaiting-successor"))))

(ert-deftest agent-repl-test-host-transferred-while-the-successor-is-pending-sends-no-adopt-yet ()
  "An unaccepted daemon has not proven it is listening, so nothing is sent to it."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (setq agent-repl-test-host--successor nil
          agent-repl-test-host--successor-pending t)
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act
    (agent-repl-test-host--push "ws-1" (list :arm :transferred :value nil))
    ;; Assert
    (should (null (assoc "AdoptHostWorkspace" agent-repl-test-host--calls)))))

(ert-deftest agent-repl-test-host-transferred-while-the-successor-is-pending-keeps-the-old-stream ()
  "Until the adopt lands the old daemon is still the only one serving the workspace."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (setq agent-repl-test-host--successor nil
          agent-repl-test-host--successor-pending t)
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act
    (agent-repl-test-host--push "ws-1" (list :arm :transferred :value nil))
    ;; Assert
    (should (null agent-repl-test-host--cancelled))))

(ert-deftest agent-repl-test-host-transferred-adopts-once-the-pending-successor-is-accepted ()
  "The defect: this adopt was never sent, so the workspace was never handed over."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (setq agent-repl-test-host--successor nil
          agent-repl-test-host--successor-pending t)
    (agent-repl-test-host--subscribe "ws-1")
    (agent-repl-test-host--push "ws-1" (list :arm :transferred :value nil))
    (let ((successor (agent-repl-connect-open "127.0.0.1:9100")))
      ;; Act — daemon-link's acceptance seam
      (run-hook-with-args 'agent-repl-link-handover-functions nil successor)
      ;; Assert
      (should (equal (car agent-repl-test-host--calls)
                     (list "AdoptHostWorkspace" successor
                           (list :workspace (agent-repl-test-host--ref))))))))

(ert-deftest agent-repl-test-host-transferred-adopts-only-once-on-acceptance ()
  "The latch is self-removing; a second acceptance must not re-adopt."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (setq agent-repl-test-host--successor nil
          agent-repl-test-host--successor-pending t)
    (agent-repl-test-host--subscribe "ws-1")
    (agent-repl-test-host--push "ws-1" (list :arm :transferred :value nil))
    (let ((successor (agent-repl-connect-open "127.0.0.1:9100")))
      ;; Act
      (run-hook-with-args 'agent-repl-link-handover-functions nil successor)
      (run-hook-with-args 'agent-repl-link-handover-functions nil successor)
      ;; Assert
      (should (= 1 (seq-count (lambda (call) (equal (car call) "AdoptHostWorkspace"))
                              agent-repl-test-host--calls))))))

(ert-deftest agent-repl-test-host-transferred-latches-every-waiting-workspace ()
  "A stand-down transfers EVERY free workspace, so every one must latch.
Each latch is its own self-removing closure on the shared acceptance
hook, and `add-hook' compares candidates against what already hangs
there — so two workspaces waiting at once is the arrangement that proves
neither the dedup nor the self-reference swallows the second one."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (setq agent-repl-test-host--successor nil
          agent-repl-test-host--successor-pending t)
    (agent-repl-test-host--subscribe "ws-1")
    (agent-repl-test-host--subscribe
     "ws-2" nil (agent-repl-test-host--ref "id-2" "/tmp/ws-2"))
    (agent-repl-test-host--push "ws-1" (list :arm :transferred :value nil))
    (agent-repl-test-host--push "ws-2" (list :arm :transferred :value nil))
    (let ((successor (agent-repl-connect-open "127.0.0.1:9100")))
      ;; Act
      (run-hook-with-args 'agent-repl-link-handover-functions nil successor)
      ;; Assert
      (should (= 2 (seq-count (lambda (call) (equal (car call) "AdoptHostWorkspace"))
                              agent-repl-test-host--calls))))))

(ert-deftest agent-repl-test-host-transferred-prefers-a-standing-successor-over-the-wait ()
  "An accepted successor is adopted onto AT ONCE; the wait is only for the pending case."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (setq agent-repl-test-host--successor (agent-repl-connect-open "127.0.0.1:9100")
          agent-repl-test-host--successor-pending t)
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act
    (agent-repl-test-host--push "ws-1" (list :arm :transferred :value nil))
    ;; Assert
    (should (agent-repl-test-host--logged-p :info "elisp.host.transferred ws=ws-1"))))

(ert-deftest agent-repl-test-host-adopt-error-arm-keeps-the-old-stream ()
  "A refused adoption leaves the old daemon serving the workspace."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (setq agent-repl-test-host--successor (agent-repl-connect-open "127.0.0.1:9100")
          agent-repl-test-host--adopt-answer
          (list :response (list :arm :error :value nil)))
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act
    (agent-repl-test-host--push "ws-1" (list :arm :transferred :value nil))
    ;; Assert
    (should (null agent-repl-test-host--cancelled))))

(ert-deftest agent-repl-test-host-adopt-error-arm-is-logged-at-error ()
  "A refused adoption is surfaced, never swallowed."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (setq agent-repl-test-host--successor (agent-repl-connect-open "127.0.0.1:9100")
          agent-repl-test-host--adopt-answer
          (list :response (list :arm :error :value nil)))
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act
    (agent-repl-test-host--push "ws-1" (list :arm :transferred :value nil))
    ;; Assert
    (should (agent-repl-test-host--logged-p :error "elisp.host.adopt-refused"))))

(ert-deftest agent-repl-test-host-adopt-transport-failure-retries-off-a-timer ()
  "THE DEFECT: a lost adopt cost the outgoing daemon its whole adoption window.
The adopt rides a 10s unary timeout while the rendezvous it joins ends
only when every participant has called, so a loaded box expires this call
on a handover that is merely still waiting — and with no retry the
workspace was adopted by nobody, the outgoing daemon sat out its 30s
window, and the promotion Emacs makes on the primary stream's close came
far too late."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (setq agent-repl-test-host--successor (agent-repl-connect-open "127.0.0.1:9100")
          agent-repl-test-host--adopt-answer
          (list :failure (list :kind :timeout :message "no answer")))
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act
    (agent-repl-test-host--push "ws-1" (list :arm :transferred :value nil))
    ;; Assert
    (should (eq (nth 1 (car agent-repl-test-host--timers))
                #'agent-repl-host--adopt-onto))))

(ert-deftest agent-repl-test-host-adopt-transport-failure-does-not-retry-a-gone-successor ()
  "A successor that is no longer standing has nothing left to adopt onto.
The promotion clears the successor slot, so a retry armed against it
would adopt onto a connection the link has let go of."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (setq agent-repl-test-host--successor (agent-repl-connect-open "127.0.0.1:9100")
          agent-repl-test-host--adopt-answer
          (list :failure (list :kind :transport :message "no route")))
    (agent-repl-test-host--subscribe "ws-1")
    (cl-letf (((symbol-function 'agent-repl-rpc-adopt-host-workspace)
               (lambda (_conn _request &rest keys)
                 ;; The successor is let go of BEFORE the answer lands, which
                 ;; is the order a promotion makes.
                 (setq agent-repl-test-host--successor nil)
                 (agent-repl-test-host--answer agent-repl-test-host--adopt-answer
                                               (plist-get keys :on-response)
                                               (plist-get keys :on-failure)))))
      ;; Act
      (agent-repl-test-host--push "ws-1" (list :arm :transferred :value nil))
      ;; Assert
      (should (null agent-repl-test-host--timers)))))

(ert-deftest agent-repl-test-host-adopt-transport-failure-keeps-the-old-stream ()
  "A successor that cannot be reached is not a reason to strand a workspace."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (setq agent-repl-test-host--successor (agent-repl-connect-open "127.0.0.1:9100")
          agent-repl-test-host--adopt-answer
          (list :failure (list :kind :transport :message "no route")))
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act
    (agent-repl-test-host--push "ws-1" (list :arm :transferred :value nil))
    ;; Assert
    (should (null agent-repl-test-host--cancelled))))

;;;; ---- The two relayed acts ----

(ert-deftest agent-repl-test-host-reload-webapp-reloads-that-workspace ()
  "A reload push reloads against the SAME daemon, handing on the build it names."
  (dolist (build '("Web2" nil))
    (agent-repl-test-host--with-harness
      ;; Arrange
      (agent-repl-test-host--subscribe "ws-1")
      ;; Act
      (agent-repl-test-host--push "ws-1" (list :arm :reload-webapp
                                               :value (list :webapp-build build)))
      ;; Assert
      (should (equal (assq :reload-webapp agent-repl-test-host--effects)
                     (list :reload-webapp "ws-1" build))))))

(ert-deftest agent-repl-test-host-open-in-editor-passes-the-line ()
  "The 1-indexed line rides through to the one shared popup subroutine."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act
    (agent-repl-test-host--push
     "ws-1" (list :arm :open-in-editor
                  :value (list :path "/tmp/plan.md" :line 42)))
    ;; Assert
    (should (equal (car agent-repl-test-host--effects)
                   (list :popup "/tmp/plan.md" 42)))))

(ert-deftest agent-repl-test-host-open-in-editor-without-a-line-passes-nil ()
  "An UNSET line means the file's top (or a directory) — absence, not zero."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act
    (agent-repl-test-host--push
     "ws-1" (list :arm :open-in-editor
                  :value (list :path "/tmp/findings" :line nil)))
    ;; Assert
    (should (equal (car agent-repl-test-host--effects)
                   (list :popup "/tmp/findings" nil)))))

(ert-deftest agent-repl-test-host-open-commit-log-opens-the-branch-log ()
  "The commit click opens its branch's log at the commit, and is logged."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    (let ((opened nil))
      (cl-letf (((symbol-function 'agent-repl-magit-log-commit)
                 (lambda (&rest args) (setq opened args))))
        ;; Act
        (agent-repl-test-host--push
         "ws-1" (list :arm :open-commit-log
                      :value (list :repository-dir "/repo" :branch "master" :commit "abc123")))
        ;; Assert
        (should (equal opened '("ws-1" "/repo" "master" "abc123")))
        (should (agent-repl-test-host--logged-p :info "elisp.host.open-commit-log"))))))

;;;; ---- The evaluation push ----

(defun agent-repl-test-host--evaluation-call ()
  "Answer the request of the one AnswerEditorEvaluation call made."
  (let ((calls (cl-remove-if-not (lambda (call) (equal (car call) "AnswerEditorEvaluation"))
                                 agent-repl-test-host--calls)))
    (should (= (length calls) 1))
    (nth 2 (car calls))))

(defun agent-repl-test-host--evaluate (ws text)
  "Push an evaluation of TEXT to WS's host stream with the token `t-1'."
  (agent-repl-test-host--push
   ws (list :arm :evaluate :value (list :token '(:value "t-1") :text text))))

(ert-deftest agent-repl-test-host-evaluate-answers-with-the-token-and-the-outcome ()
  "An evaluation push is answered on WS's connection with its token unchanged."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act
    (agent-repl-test-host--evaluate "ws-1" "(+ 1 2)")
    ;; Assert
    (let ((request (agent-repl-test-host--evaluation-call)))
      (should (equal (plist-get request :workspace) (agent-repl-test-host--ref)))
      (should (equal (plist-get request :token) '(:value "t-1")))
      (should (equal (plist-get (plist-get request :outcome) :outcome)
                     '(:arm :value :value (:printed "3" :readable t)))))))

(ert-deftest agent-repl-test-host-evaluate-answers-on-the-workspaces-connection ()
  "The answer goes to the daemon whose stream pushed the evaluation."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (let ((conn (agent-repl-connect-open "127.0.0.1:9007")))
      (agent-repl-test-host--subscribe "ws-1" conn)
      ;; Act
      (agent-repl-test-host--evaluate "ws-1" "1")
      ;; Assert
      (should (eq (nth 1 (car (cl-remove-if-not
                               (lambda (call) (equal (car call) "AnswerEditorEvaluation"))
                               agent-repl-test-host--calls)))
                  conn)))))

(ert-deftest agent-repl-test-host-evaluate-runs-in-the-composer ()
  "A live composer is the buffer current while the text is evaluated."
  (agent-repl-test-host--with-harness
    (with-temp-buffer
      (rename-buffer "agent-repl-test-host-composer" t)
      (let ((composer (current-buffer)))
        (with-temp-buffer
          (cl-letf (((symbol-function 'agent-repl--ws-get)
                     (lambda (_ws key) (and (eq key :input-buffer) composer))))
            ;; Arrange
            (agent-repl-test-host--subscribe "ws-1")
            ;; Act
            (agent-repl-test-host--evaluate "ws-1" "(buffer-name)")
            ;; Assert
            (should (equal (plist-get (plist-get (plist-get (plist-get (agent-repl-test-host--evaluation-call)
                                                                       :outcome)
                                                            :outcome)
                                                 :value)
                                      :printed)
                           (prin1-to-string (buffer-name composer))))
            (should (agent-repl-test-host--logged-p
                     :info "composer=t outcome=:value"))))))))

(ert-deftest agent-repl-test-host-evaluate-without-a-composer-says-so ()
  "A workspace with no live composer evaluates in the current buffer, recorded."
  (agent-repl-test-host--with-harness
    (cl-letf (((symbol-function 'agent-repl--ws-get) (lambda (&rest _) nil)))
      ;; Arrange
      (agent-repl-test-host--subscribe "ws-1")
      ;; Act
      (agent-repl-test-host--evaluate "ws-1" "1")
      ;; Assert
      (agent-repl-test-host--evaluation-call)
      (should (agent-repl-test-host--logged-p
               :info "composer=nil outcome=:value")))))

(ert-deftest agent-repl-test-host-evaluate-answers-a-read-error ()
  "Text that does not read is answered, never dropped."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act
    (agent-repl-test-host--evaluate "ws-1" "(+ 1")
    ;; Assert
    (should (eq (plist-get (plist-get (plist-get (agent-repl-test-host--evaluation-call) :outcome)
                                      :outcome)
                           :arm)
                :read-error))))

(ert-deftest agent-repl-test-host-evaluate-refused-answer-is-an-error ()
  "A refused answer is recorded at ERROR with the token and the cause."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (setq agent-repl-test-host--evaluation-answer
          (list :response (list :arm :error
                                :value (list :cause (list :arm :unknown-token :value nil)))))
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act
    (agent-repl-test-host--evaluate "ws-1" "1")
    ;; Assert
    (should (agent-repl-test-host--logged-p
             :error "elisp.host.evaluation-answer-refused ws=ws-1 token=t-1 cause=:unknown-token"))))

(ert-deftest agent-repl-test-host-evaluate-failed-answer-is-an-error ()
  "An answer the transport could not deliver is recorded at ERROR."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (setq agent-repl-test-host--evaluation-answer
          (list :failure (list :detail "connection refused")))
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act
    (agent-repl-test-host--evaluate "ws-1" "1")
    ;; Assert
    (should (agent-repl-test-host--logged-p
             :error "elisp.host.evaluation-answer-failed ws=ws-1 token=t-1"))))

(ert-deftest agent-repl-test-host-evaluate-without-a-connection-signals ()
  "A push on a stream whose workspace has no connection is a broken invariant."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    (cl-letf (((symbol-function 'agent-repl-host-conn) (lambda (_ws) nil))
              ((symbol-function 'agent-repl--fatal)
               (lambda (_ws fmt &rest args) (error (apply #'format fmt args)))))
      ;; Act / Assert
      (should (string-match-p
               "elisp.host.evaluate-no-connection ws=ws-1 token=t-1"
               (cadr (should-error (agent-repl-test-host--evaluate "ws-1" "1")))))
      (should-not (cl-find "AnswerEditorEvaluation" agent-repl-test-host--calls
                           :key #'car :test #'equal)))))

(ert-deftest agent-repl-test-host-unknown-push-arm-is-an-error ()
  "An arm nobody knows is a contract breach, never a silent drop."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act
    (agent-repl-test-host--push "ws-1" (list :arm :teleported :value nil))
    ;; Assert
    (should (agent-repl-test-host--logged-p :error "elisp.host.unknown-push"))))

;;;; ---- Link lifecycle ----

(ert-deftest agent-repl-test-host-link-up-registers-every-live-workspace ()
  "Re-registration after a reconnect is the NORMAL path, idempotent by dir."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (let ((conn (agent-repl-connect-open "127.0.0.1:9001")))
      (cl-letf (((symbol-function 'agent-repl--live-ws-names) (lambda () '("ws-1")))
                ((symbol-function 'agent-repl--ws-get) (lambda (&rest _) "/tmp/ws-1")))
        ;; Act
        (agent-repl-host-on-link-up conn))
      ;; Assert
      (should (member (list "RegisterWorkspace" conn '(:dir "/tmp/ws-1"))
                      agent-repl-test-host--calls)))))

(ert-deftest agent-repl-test-host-link-up-does-not-warn-about-a-pseudo-perspective ()
  "persp-mode's own `none' is not a workspace, so skipping it is not a fault.
It warned `link-up-skipped ws=none reason=no-dir' on every single
link-up, which teaches a reader to ignore the warnings that matter.  The
filter now lives in `agent-repl--live-ws-names', so this drives the REAL
registry rather than a stub: a pseudo perspective cannot reach the walk."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (let ((conn (agent-repl-connect-open "127.0.0.1:9001"))
          (persp-nil-name "none")
          (agent-repl--workspaces (make-hash-table :test #'equal)))
      (puthash "none" '(:repl-state :inactive) agent-repl--workspaces)
      (puthash "ws-1" '(:project-dir "/tmp/ws-1") agent-repl--workspaces)
      ;; Act
      (agent-repl-host-on-link-up conn)
      ;; Assert
      (should-not (agent-repl-test-host--logged-p
                   :warn "elisp.host.link-up-skipped")))))

(ert-deftest agent-repl-test-host-link-up-still-registers-the-real-workspace ()
  "Filtering the pseudo names must not cost the workspace beside them."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (let ((conn (agent-repl-connect-open "127.0.0.1:9001"))
          (persp-nil-name "none")
          (agent-repl--workspaces (make-hash-table :test #'equal)))
      (puthash "none" '(:repl-state :inactive) agent-repl--workspaces)
      (puthash "ws-1" '(:project-dir "/tmp/ws-1") agent-repl--workspaces)
      ;; Act
      (agent-repl-host-on-link-up conn)
      ;; Assert
      (should (member (list "RegisterWorkspace" conn '(:dir "/tmp/ws-1"))
                      agent-repl-test-host--calls)))))

(ert-deftest agent-repl-test-host-link-up-still-warns-about-a-real-workspace-with-no-dir ()
  "A REAL workspace missing its directory is an anomaly and keeps warning."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (let ((conn (agent-repl-connect-open "127.0.0.1:9001"))
          (persp-nil-name "none"))
      (cl-letf (((symbol-function 'agent-repl--live-ws-names)
                 (lambda () '("none" "ws-1")))
                ((symbol-function 'agent-repl--ws-get) (lambda (&rest _) nil)))
        ;; Act
        (agent-repl-host-on-link-up conn))
      ;; Assert
      (should (agent-repl-test-host--logged-p
               :warn "elisp.host.link-up-skipped ws=ws-1")))))

(ert-deftest agent-repl-test-host-link-up-subscribes-after-registering ()
  "Register then subscribe: the subscription is addressed by the minted ref."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (let ((conn (agent-repl-connect-open "127.0.0.1:9001")))
      (cl-letf (((symbol-function 'agent-repl--live-ws-names) (lambda () '("ws-1")))
                ((symbol-function 'agent-repl--ws-get) (lambda (&rest _) "/tmp/ws-1")))
        ;; Act
        (agent-repl-host-on-link-up conn))
      ;; Assert
      (should (equal (plist-get (agent-repl-host-stream "ws-1") :ref)
                     (agent-repl-test-host--ref))))))

(ert-deftest agent-repl-test-host-link-up-repoints-the-webview-at-the-new-daemon ()
  "A daemon Emacs relaunches listens on a FRESH PORT, so the page must move.

Without this the tab bar healed, the roster came back and the composer
worked while the webview went on dialing a daemon that had exited, and
the page drew a permanent `daemonUnreachable\=' card over its last feed.
The reload is what re-points it, and it must see the NEW connection —
frontend.el derives the page url from `agent-repl-host-conn\='."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (let ((conn (agent-repl-connect-open "127.0.0.1:9001")))
      (cl-letf (((symbol-function 'agent-repl--live-ws-names) (lambda () '("ws-1")))
                ((symbol-function 'agent-repl--ws-get) (lambda (&rest _) "/tmp/ws-1")))
        ;; Act
        (agent-repl-host-on-link-up conn))
      ;; Assert
      (should (equal (assq :reload agent-repl-test-host--effects) '(:reload . "ws-1")))
      (should (eq (cdr (assq :reload-conn agent-repl-test-host--effects)) conn)))))

(ert-deftest agent-repl-test-host-link-up-repoints-after-subscribing ()
  "The reload comes AFTER the subscribe, which is what moved `:conn'.
Reloading first would navigate the page at the daemon that just died."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (let ((conn (agent-repl-connect-open "127.0.0.1:9001")))
      (cl-letf (((symbol-function 'agent-repl--live-ws-names) (lambda () '("ws-1")))
                ((symbol-function 'agent-repl--ws-get) (lambda (&rest _) "/tmp/ws-1")))
        (setq agent-repl-test-host--walk nil)
        ;; Act
        (agent-repl-host-on-link-up conn))
      ;; Assert
      (let ((walk (reverse agent-repl-test-host--walk)))
        (should (< (seq-position walk :subscribe) (seq-position walk :reload)))))))

(ert-deftest agent-repl-test-host-link-up-register-failure-repoints-nothing ()
  "A workspace the new daemon would not register is not re-pointed at it."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (let ((conn (agent-repl-connect-open "127.0.0.1:9001")))
      (setq agent-repl-test-host--register-answer
            (list :response (list :arm :error :value (list :message "no"))))
      (cl-letf (((symbol-function 'agent-repl--live-ws-names) (lambda () '("ws-1")))
                ((symbol-function 'agent-repl--ws-get) (lambda (&rest _) "/tmp/ws-1")))
        ;; Act
        (agent-repl-host-on-link-up conn))
      ;; Assert
      (should (null (assq :reload agent-repl-test-host--effects))))))

(ert-deftest agent-repl-test-host-link-up-skips-a-workspace-with-no-dir ()
  "A workspace with no directory has nothing to register."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (let ((conn (agent-repl-connect-open "127.0.0.1:9001"))
          (logged-workspace nil))
      (cl-letf (((symbol-function 'agent-repl--live-ws-names) (lambda () '("ws-1")))
                ((symbol-function 'agent-repl--ws-get) (lambda (&rest _) nil))
                ((symbol-function 'agent-repl--warn)
                 (lambda (ws &rest _args) (setq logged-workspace ws))))
        ;; Act
        (agent-repl-host-on-link-up conn))
      ;; Assert
      (should (null agent-repl-test-host--calls))
      (should (agent-repl--central-log-scope-reason logged-workspace)))))

(ert-deftest agent-repl-test-host-link-up-reselects-the-dir-the-user-stood-in ()
  "Emacs owns the user's selection across a daemon restart.
A relaunched daemon stamps `current' on whichever workspace re-registered
first — here ws-2, walked first — so Emacs says again what the user
chose, addressed by the DIR, since the ids were re-minted."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (let ((conn (agent-repl-connect-open "127.0.0.1:9001")))
      (setq agent-repl-host-last-selected-id "id-before-the-outage"
            agent-repl-test-host--current-ws "ws-1")
      (cl-letf (((symbol-function 'agent-repl--live-ws-names)
                 (lambda () '("ws-2" "ws-1")))
                ((symbol-function 'agent-repl--ws-get)
                 (lambda (ws _key) (concat "/tmp/" ws)))
                ((symbol-function 'agent-repl--ws-by-ref-id)
                 (lambda (id) (and (equal id "id-before-the-outage") "ws-1")))
                ((symbol-function 'agent-repl--ws-switch) (lambda (&rest _) nil)))
        ;; Act
        (agent-repl-host-on-link-up conn))
      ;; Assert
      (should (agent-repl-test-host--logged-p
               :info "elisp.host.link-up-reselect ws=ws-1")))))

(ert-deftest agent-repl-test-host-link-up-reasserts-a-selection-a-leaving-daemon-answered ()
  "A select the leaving daemon answered `standing_down' is asked of the next one."
  (agent-repl-test-host--with-harness
    ;; Arrange: the frame shows ws-2; the user's select of ws-1 reached a
    ;; daemon that was standing down.
    (agent-repl-test-host--subscribe "ws-1")
    (setq agent-repl-test-host--current-ws "ws-2"
          agent-repl-test-host--select-answer (agent-repl-test-host--standing-down-answer))
    (agent-repl-host-select "ws-1")
    (setq agent-repl-test-host--select-answer
          (list :response (list :arm :success :value (list :roster-revision 0))))
    (let ((conn (agent-repl-connect-open "127.0.0.1:9002")))
      (cl-letf (((symbol-function 'agent-repl--live-ws-names)
                 (lambda () '("ws-2" "ws-1")))
                ((symbol-function 'agent-repl--ws-get)
                 (lambda (ws _key) (concat "/tmp/" ws)))
                ((symbol-function 'agent-repl--ws-by-ref-id)
                 (lambda (id) (and (equal id "ws-id-1") "ws-1")))
                ((symbol-function 'agent-repl--ws-switch) (lambda (&rest _) nil)))
        ;; Act: the next daemon's link comes up.
        (agent-repl-host-on-link-up conn)))
    ;; Assert
    (should (agent-repl-test-host--logged-p
             :info "elisp.host.link-up-reselect ws=ws-1"))))

(ert-deftest agent-repl-test-host-link-up-reasserts-a-selection-still-in-flight ()
  "A select still unanswered when the link comes up is the one re-asserted.
Its failure can land after the link-up, so the id recorded before it
names the workspace the user already left."
  (agent-repl-test-host--with-harness
    ;; Arrange: ws-2's select was acknowledged; the user's select of ws-1
    ;; is still in flight as the next daemon's link comes up.
    (let ((conn (agent-repl-connect-open "127.0.0.1:9001")))
      (setq agent-repl-host-last-selected-id "ws-id-2"
            agent-repl-host--select-in-flight "ws-1"
            agent-repl-test-host--current-ws "ws-1")
      (cl-letf (((symbol-function 'agent-repl--live-ws-names)
                 (lambda () '("ws-2" "ws-1")))
                ((symbol-function 'agent-repl--ws-get)
                 (lambda (ws _key) (concat "/tmp/" ws)))
                ((symbol-function 'agent-repl--ws-by-ref-id)
                 (lambda (id) (and (equal id "ws-id-2") "ws-2")))
                ((symbol-function 'agent-repl--ws-switch) (lambda (&rest _) nil)))
        ;; Act
        (agent-repl-host-on-link-up conn))
      ;; Assert
      (should (agent-repl-test-host--logged-p
               :info "elisp.host.link-up-reselect ws=ws-1")))))

(ert-deftest agent-repl-test-host-selected-dir-prefers-a-queued-selection ()
  "A selection queued behind the one in flight is the user's newest choice."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (setq agent-repl-host-last-selected-id "ws-id-1"
          agent-repl-host--select-in-flight "ws-2"
          agent-repl-host--select-queued (cons "ws-3" nil))
    (cl-letf (((symbol-function 'agent-repl--ws-get)
               (lambda (ws _key) (concat "/tmp/" ws)))
              ((symbol-function 'agent-repl--ws-by-ref-id)
               (lambda (_id) "ws-1")))
      ;; Act / Assert
      (should (equal (agent-repl-host--selected-dir) "/tmp/ws-3")))))

(ert-deftest agent-repl-test-host-selected-dir-reads-the-recorded-id-with-nothing-pending ()
  "With no selection pending, the id Emacs last recorded decides."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (setq agent-repl-host-last-selected-id "ws-id-1"
          agent-repl-test-host--current-ws "ws-2")
    (cl-letf (((symbol-function 'agent-repl--ws-get)
               (lambda (ws _key) (concat "/tmp/" ws)))
              ((symbol-function 'agent-repl--ws-by-ref-id)
               (lambda (id) (and (equal id "ws-id-1") "ws-1"))))
      ;; Act / Assert
      (should (equal (agent-repl-host--selected-dir) "/tmp/ws-1")))))

(ert-deftest agent-repl-test-host-link-up-reselect-waits-for-every-registration ()
  "The re-assertion comes AFTER the last workspace re-registered.
Asserting it earlier would let the workspaces still registering behind it
stamp `current' again and undo the user's selection."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (let ((conn (agent-repl-connect-open "127.0.0.1:9001")))
      (setq agent-repl-host-last-selected-id "id-before-the-outage"
            agent-repl-test-host--current-ws "ws-1")
      (cl-letf (((symbol-function 'agent-repl--live-ws-names)
                 (lambda () '("ws-1" "ws-2")))
                ((symbol-function 'agent-repl--ws-get)
                 (lambda (ws _key) (concat "/tmp/" ws)))
                ((symbol-function 'agent-repl--ws-by-ref-id)
                 (lambda (id) (and (equal id "id-before-the-outage") "ws-1")))
                ((symbol-function 'agent-repl--ws-switch) (lambda (&rest _) nil)))
        ;; Act
        (agent-repl-host-on-link-up conn))
      ;; Assert
      (let ((entries (mapcar #'cdr (reverse agent-repl-test-host--logs))))
        (should (< (seq-position entries "elisp.host.link-up-webview-repointed ws=ws-2 address=\"127.0.0.1:9001\"")
                   (seq-position entries "elisp.host.link-up-reselect ws=ws-1 dir=\"/tmp/ws-1\"")))))))

(ert-deftest agent-repl-test-host-link-up-reselect-holds-the-frame-until-acknowledged ()
  "The suppression roster.el reads is released on the re-select's ack.
A suppression that outlived its re-select would deafen Emacs to the
user's next sidebar click."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (let ((conn (agent-repl-connect-open "127.0.0.1:9001")))
      (setq agent-repl-host-last-selected-id "id-before-the-outage"
            agent-repl-test-host--current-ws "ws-1")
      (cl-letf (((symbol-function 'agent-repl--live-ws-names) (lambda () '("ws-1")))
                ((symbol-function 'agent-repl--ws-get)
                 (lambda (ws _key) (concat "/tmp/" ws)))
                ((symbol-function 'agent-repl--ws-by-ref-id)
                 (lambda (id) (and (equal id "id-before-the-outage") "ws-1")))
                ((symbol-function 'agent-repl--ws-switch) (lambda (&rest _) nil)))
        ;; Act
        (agent-repl-host-on-link-up conn))
      ;; Assert
      (should (null agent-repl-host-reselect-pending)))))

(ert-deftest agent-repl-test-host-link-up-reselect-releases-the-frame-on-a-refusal ()
  "A refused re-select still releases the frame, and says so."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (let ((conn (agent-repl-connect-open "127.0.0.1:9001")))
      (setq agent-repl-host-last-selected-id "id-before-the-outage"
            agent-repl-test-host--current-ws "ws-1"
            agent-repl-test-host--select-answer
            (list :response (list :arm :error :value (list :message "no"))))
      (cl-letf (((symbol-function 'agent-repl--live-ws-names) (lambda () '("ws-1")))
                ((symbol-function 'agent-repl--ws-get)
                 (lambda (ws _key) (concat "/tmp/" ws)))
                ((symbol-function 'agent-repl--ws-by-ref-id)
                 (lambda (id) (and (equal id "id-before-the-outage") "ws-1")))
                ((symbol-function 'agent-repl--ws-switch) (lambda (&rest _) nil)))
        ;; Act
        (agent-repl-host-on-link-up conn))
      ;; Assert
      (should (null agent-repl-host-reselect-pending)))))

(ert-deftest agent-repl-test-host-link-up-reselect-brings-the-frame-to-the-selection ()
  "The frame follows the user's own selection, not the walk order."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (let ((conn (agent-repl-connect-open "127.0.0.1:9001"))
          (switched nil))
      (setq agent-repl-host-last-selected-id "id-before-the-outage"
            agent-repl-test-host--current-ws "ws-2")
      (cl-letf (((symbol-function 'agent-repl--live-ws-names)
                 (lambda () '("ws-2" "ws-1")))
                ((symbol-function 'agent-repl--ws-get)
                 (lambda (ws _key) (concat "/tmp/" ws)))
                ((symbol-function 'agent-repl--ws-by-ref-id)
                 (lambda (id) (and (equal id "id-before-the-outage") "ws-1")))
                ((symbol-function 'agent-repl--ws-switch)
                 (lambda (ws &rest _) (push ws switched))))
        ;; Act
        (agent-repl-host-on-link-up conn))
      ;; Assert
      (should (equal switched '("ws-1"))))))

(ert-deftest agent-repl-test-host-link-up-reselect-moves-the-frame-as-an-automatic-switch ()
  "Restoring the user's own selection is not a new switch by the user."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (let ((conn (agent-repl-connect-open "127.0.0.1:9001"))
          (automatic nil))
      (setq agent-repl-host-last-selected-id "id-before-the-outage"
            agent-repl-test-host--current-ws "ws-2")
      (cl-letf (((symbol-function 'agent-repl--live-ws-names)
                 (lambda () '("ws-2" "ws-1")))
                ((symbol-function 'agent-repl--ws-get)
                 (lambda (ws _key) (concat "/tmp/" ws)))
                ((symbol-function 'agent-repl--ws-by-ref-id)
                 (lambda (id) (and (equal id "id-before-the-outage") "ws-1")))
                ((symbol-function 'agent-repl--ws-switch)
                 (lambda (&rest _) (setq automatic agent-repl-host-automatic-switch))))
        ;; Act
        (agent-repl-host-on-link-up conn))
      ;; Assert
      (should automatic))))

(ert-deftest agent-repl-test-host-link-up-reselect-reports-a-workspace-that-did-not-come-back ()
  "A selection whose workspace the new daemon refused is reported, not moved.
The frame stays on the live workspace it is already standing on rather
than being handed an arbitrary substitute."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (let ((conn (agent-repl-connect-open "127.0.0.1:9001"))
          (switched nil))
      (setq agent-repl-host-last-selected-id "id-before-the-outage"
            agent-repl-test-host--current-ws "ws-2"
            agent-repl-test-host--register-answer
            (list :response (list :arm :error :value (list :message "gone"))))
      (cl-letf (((symbol-function 'agent-repl--live-ws-names)
                 (lambda () '("ws-2" "ws-1")))
                ((symbol-function 'agent-repl--ws-get)
                 (lambda (ws _key) (concat "/tmp/" ws)))
                ((symbol-function 'agent-repl--ws-by-ref-id)
                 (lambda (id) (and (equal id "id-before-the-outage") "ws-1")))
                ((symbol-function 'agent-repl--ws-switch)
                 (lambda (ws &rest _) (push ws switched))))
        ;; Act
        (agent-repl-host-on-link-up conn))
      ;; Assert
      (should (agent-repl-test-host--logged-p :warn "elisp.host.link-up-reselect-lost"))
      (should (null switched)))))

(ert-deftest agent-repl-test-host-link-up-with-no-selection-reselects-nothing ()
  "A session that never selected anything has nothing to re-assert."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (let ((conn (agent-repl-connect-open "127.0.0.1:9001")))
      (cl-letf (((symbol-function 'agent-repl--live-ws-names) (lambda () '("ws-1")))
                ((symbol-function 'agent-repl--ws-get)
                 (lambda (ws _key) (concat "/tmp/" ws)))
                ((symbol-function 'agent-repl--ws-by-ref-id) (lambda (_id) nil))
                ((symbol-function 'agent-repl--ws-switch) (lambda (&rest _) nil)))
        ;; Act
        (agent-repl-host-on-link-up conn))
      ;; Assert
      (should (agent-repl-test-host--logged-p
               :log "elisp.host.link-up-reselect-skipped")))))

(ert-deftest agent-repl-test-host-link-down-drops-the-stream ()
  "A dead connection carries no subscription."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (let ((conn (agent-repl-connect-open "127.0.0.1:9001")))
      (agent-repl-test-host--subscribe "ws-1" conn)
      ;; Act
      (agent-repl-host-on-link-down conn)
      ;; Assert
      (should (null (agent-repl-host-stream "ws-1"))))))

(ert-deftest agent-repl-test-host-planned-link-down-is-recorded-at-info ()
  "A link the daemon announced the end of goes down at INFO."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (let ((conn (agent-repl-connect-open "127.0.0.1:9001")))
      (agent-repl-test-host--subscribe "ws-1" conn)
      ;; Act
      (let ((agent-repl-link-down-planned t))
        (agent-repl-host-on-link-down conn))
      ;; Assert
      (should (agent-repl-test-host--logged-p
               :info "elisp.host.link-down workspaces=1 planned=t")))))

(ert-deftest agent-repl-test-host-planned-link-down-writes-no-warning ()
  "A planned link-down never writes the WARNING of a lost link."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (let ((conn (agent-repl-connect-open "127.0.0.1:9001")))
      (agent-repl-test-host--subscribe "ws-1" conn)
      ;; Act
      (let ((agent-repl-link-down-planned t))
        (agent-repl-host-on-link-down conn))
      ;; Assert
      (should-not (agent-repl-test-host--logged-p :warn "elisp.host.link-down")))))

(ert-deftest agent-repl-test-host-unannounced-link-down-is-a-warning ()
  "A link that died with no word from its daemon stays a WARNING."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (let ((conn (agent-repl-connect-open "127.0.0.1:9001")))
      (agent-repl-test-host--subscribe "ws-1" conn)
      ;; Act
      (agent-repl-host-on-link-down conn)
      ;; Assert
      (should (agent-repl-test-host--logged-p
               :warn "elisp.host.link-down workspaces=1 planned=nil")))))

(ert-deftest agent-repl-test-host-link-down-keeps-the-last-state ()
  "The pushed state is the newest fact Emacs has; an outage must not blank it."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (let ((conn (agent-repl-connect-open "127.0.0.1:9001")))
      (agent-repl-test-host--subscribe "ws-1" conn)
      (agent-repl-test-host--push
       "ws-1" (list :arm :host :value (agent-repl-test-host--composer :open)))
      ;; Act
      (agent-repl-host-on-link-down conn)
      ;; Assert
      (should (eq (agent-repl-host-composer-gate "ws-1") :open)))))

(ert-deftest agent-repl-test-host-link-down-leaves-other-connections-alone ()
  "Only the streams the dead connection carried are dropped."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (let ((dead (agent-repl-connect-open "127.0.0.1:9001"))
          (other (agent-repl-connect-open "127.0.0.1:9002")))
      (agent-repl-test-host--subscribe "ws-1" dead)
      (agent-repl-test-host--subscribe "ws-2" other)
      ;; Act
      (agent-repl-host-on-link-down dead)
      ;; Assert
      (should (agent-repl-host-stream "ws-2")))))

(ert-deftest agent-repl-test-host-forget-drops-the-workspace-entirely ()
  "Tearing a tab down is a VIEW act; the entry goes with it."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act
    (agent-repl-host-forget "ws-1")
    ;; Assert
    (should (null (agent-repl-host-ref "ws-1")))))


;;;; ---- Handover refusals answered by a per-workspace rpc ----

(ert-deftest agent-repl-test-host-transferring-away-adopts-on-the-named-daemon ()
  "A verb's `transferring_away' is the same fact as a `transferred' push."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (let ((successor (agent-repl-connect-open "127.0.0.1:9100")))
      (setq agent-repl-test-host--successor successor)
      (agent-repl-test-host--subscribe "ws-1")
      ;; Act
      (agent-repl-host-handle-refusal
       "ws-1" '(:arm :transferring-away :value (:address "127.0.0.1:9100")))
      ;; Assert
      (should (equal (car agent-repl-test-host--calls)
                     (list "AdoptHostWorkspace" successor
                           (list :workspace (agent-repl-test-host--ref))))))))

(ert-deftest agent-repl-test-host-transferring-away-resubscribes-on-the-successor ()
  "The adopt is followed by cancel-then-subscribe, the one contract order."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (setq agent-repl-test-host--successor (agent-repl-connect-open "127.0.0.1:9100"))
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act
    (agent-repl-host-handle-refusal
     "ws-1" '(:arm :transferring-away :value (:address "127.0.0.1:9100")))
    ;; Assert
    (should (eq (plist-get (car agent-repl-test-host--streams) :conn)
                agent-repl-test-host--successor))))

(ert-deftest agent-repl-test-host-transferring-away-does-not-redial-the-standing-successor ()
  "A successor already standing at the named address is dialed again by nobody."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (setq agent-repl-test-host--successor (agent-repl-connect-open "127.0.0.1:9100"))
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act
    (agent-repl-host-handle-refusal
     "ws-1" '(:arm :transferring-away :value (:address "127.0.0.1:9100")))
    ;; Assert
    (should (null agent-repl-test-host--dialled))))

(ert-deftest agent-repl-test-host-transferring-away-dials-when-no-successor-stands ()
  "The refusal can be the FIRST news of a handover: Emacs dials the address."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act
    (agent-repl-host-handle-refusal
     "ws-1" '(:arm :transferring-away :value (:address "127.0.0.1:9100")))
    ;; Assert
    (should (equal agent-repl-test-host--dialled '("127.0.0.1:9100")))))

(ert-deftest agent-repl-test-host-transferring-away-redials-a-stale-successor ()
  "A successor standing at a DIFFERENT address is stale; the refusal wins."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (setq agent-repl-test-host--successor (agent-repl-connect-open "127.0.0.1:9999"))
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act
    (agent-repl-host-handle-refusal
     "ws-1" '(:arm :transferring-away :value (:address "127.0.0.1:9100")))
    ;; Assert
    (should (equal agent-repl-test-host--dialled '("127.0.0.1:9100")))))

(ert-deftest agent-repl-test-host-transferring-away-logs-the-redial ()
  "The dial is on the record as `elisp.host.redial'."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act
    (agent-repl-host-handle-refusal
     "ws-1" '(:arm :transferring-away :value (:address "127.0.0.1:9100")))
    ;; Assert
    (should (agent-repl-test-host--logged-p :info "elisp.host.redial"))))

(ert-deftest agent-repl-test-host-transferring-away-waits-for-an-unaccepted-dial ()
  "Adopting onto a daemon that has not answered is what the gate forbids."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (setq agent-repl-test-host--dial-accepts nil)
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act
    (agent-repl-host-handle-refusal
     "ws-1" '(:arm :transferring-away :value (:address "127.0.0.1:9100")))
    ;; Assert
    (should (null (assoc "AdoptHostWorkspace" agent-repl-test-host--calls)))))

(ert-deftest agent-repl-test-host-transferring-away-adopts-once-the-dial-is-accepted ()
  "Acceptance itself wakes the adopt; nothing polls."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (setq agent-repl-test-host--dial-accepts nil)
    (agent-repl-test-host--subscribe "ws-1")
    (agent-repl-host-handle-refusal
     "ws-1" '(:arm :transferring-away :value (:address "127.0.0.1:9100")))
    (let ((successor (agent-repl-connect-open "127.0.0.1:9100")))
      ;; Act — daemon-link's acceptance seam
      (run-hook-with-args 'agent-repl-link-handover-functions nil successor)
      ;; Assert
      (should (equal (car agent-repl-test-host--calls)
                     (list "AdoptHostWorkspace" successor
                           (list :workspace (agent-repl-test-host--ref))))))))

(ert-deftest agent-repl-test-host-transferring-away-without-an-address-is-an-error ()
  "The address is the whole content of the arm; without it nothing can act."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act
    (agent-repl-host-handle-refusal "ws-1" '(:arm :transferring-away :value nil))
    ;; Assert
    (should (agent-repl-test-host--logged-p
             :error "elisp.host.transferring-away-without-address"))))

(ert-deftest agent-repl-test-host-transferring-away-with-a-blank-address-is-an-error ()
  "`address' is a plain string, so the daemon's zero value is the empty one."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act
    (agent-repl-host-handle-refusal "ws-1" '(:arm :transferring-away :value (:address "")))
    ;; Assert
    (should (agent-repl-test-host--logged-p
             :error "elisp.host.transferring-away-without-address"))))

(ert-deftest agent-repl-test-host-transferring-away-with-a-blank-address-dials-nothing ()
  "Dialing the empty address would be inventing a daemon."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act
    (agent-repl-host-handle-refusal "ws-1" '(:arm :transferring-away :value (:address "")))
    ;; Assert
    (should (null agent-repl-test-host--dialled))))

(ert-deftest agent-repl-test-host-adopt-transferring-away-goes-to-the-handover ()
  "AdoptHostWorkspace is a per-workspace rpc and carries the handover arms too."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (setq agent-repl-test-host--successor (agent-repl-connect-open "127.0.0.1:9100")
          agent-repl-test-host--adopt-answer
          (list :response
                (list :arm :error
                      :value (list :cause (list :arm :transferring-away
                                                :value (list :address "127.0.0.1:9200"))))))
    (agent-repl-test-host--subscribe "ws-1")
    (let ((seen nil))
      (cl-letf (((symbol-function 'agent-repl-host-handle-refusal)
                 (lambda (_ws arm) (setq seen arm))))
        ;; Act
        (agent-repl-test-host--push "ws-1" (list :arm :transferred :value nil))
        ;; Assert
        (should (equal (plist-get seen :value) (list :address "127.0.0.1:9200")))))))

(ert-deftest agent-repl-test-host-adopt-not-yet-adopted-is-not-an-error ()
  "The successor is still finishing its takeover; nothing has failed."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (setq agent-repl-test-host--successor (agent-repl-connect-open "127.0.0.1:9100")
          agent-repl-test-host--adopt-answer
          (list :response
                (list :arm :error
                      :value (list :cause (list :arm :not-yet-adopted :value nil)))))
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act
    (agent-repl-test-host--push "ws-1" (list :arm :transferred :value nil))
    ;; Assert
    (should-not (agent-repl-test-host--logged-p :error "elisp.host.adopt-refused"))))

(ert-deftest agent-repl-test-host-adopt-not-yet-adopted-retries-off-a-timer ()
  "The retry is SCHEDULED: re-adopting from inside the answer would spin."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (setq agent-repl-test-host--successor (agent-repl-connect-open "127.0.0.1:9100")
          agent-repl-test-host--adopt-answer
          (list :response
                (list :arm :error
                      :value (list :cause (list :arm :not-yet-adopted :value nil)))))
    (agent-repl-test-host--subscribe "ws-1")
    (let ((scheduled nil))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (delay _repeat fn &rest args)
                   (setq scheduled (cons delay (cons fn args))))))
        ;; Act
        (agent-repl-test-host--push "ws-1" (list :arm :transferred :value nil))
        ;; Assert
        (should (eq (nth 1 scheduled) #'agent-repl-host-handle-refusal))))))

(ert-deftest agent-repl-test-host-adopt-transfer-deferred-is-info-not-an-error ()
  "The incumbent took the workspace back; that is news, not a failure."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (setq agent-repl-test-host--successor (agent-repl-connect-open "127.0.0.1:9100")
          agent-repl-test-host--adopt-answer
          (list :response
                (list :arm :error
                      :value (list :cause (list :arm :transfer-deferred :value nil)))))
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act
    (agent-repl-test-host--push "ws-1" (list :arm :transferred :value nil))
    ;; Assert
    (should-not (agent-repl-test-host--logged-p :error "elisp.host.adopt-refused"))
    (should (agent-repl-test-host--logged-p :info "elisp.host.adopt-transfer-deferred"))))

(ert-deftest agent-repl-test-host-adopt-transfer-deferred-schedules-no-retry ()
  "Nothing changes until the incumbent's next `transferred' push, so no
retry is scheduled: retrying flipped the webview between the daemons."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (setq agent-repl-test-host--successor (agent-repl-connect-open "127.0.0.1:9100")
          agent-repl-test-host--adopt-answer
          (list :response
                (list :arm :error
                      :value (list :cause (list :arm :transfer-deferred :value nil)))))
    (agent-repl-test-host--subscribe "ws-1")
    (let ((scheduled nil))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (delay _repeat fn &rest args)
                   (push (cons delay (cons fn args)) scheduled))))
        ;; Act
        (agent-repl-test-host--push "ws-1" (list :arm :transferred :value nil))
        ;; Assert
        (should-not scheduled)))))

(ert-deftest agent-repl-test-host-not-yet-adopted-is-info-not-an-error ()
  "The successor simply has not taken the workspace over yet."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act
    (agent-repl-host-handle-refusal "ws-1" '(:arm :not-yet-adopted :value nil))
    ;; Assert
    (should (agent-repl-test-host--logged-p :info "elisp.host.not-yet-adopted"))))

(ert-deftest agent-repl-test-host-not-yet-adopted-retries-on-the-standing-successor ()
  "With the successor already accepted the adopt is simply walked again."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (let ((successor (agent-repl-connect-open "127.0.0.1:9100")))
      (setq agent-repl-test-host--successor successor)
      (agent-repl-test-host--subscribe "ws-1")
      ;; Act
      (agent-repl-host-handle-refusal "ws-1" '(:arm :not-yet-adopted :value nil))
      ;; Assert
      (should (equal (car agent-repl-test-host--calls)
                     (list "AdoptHostWorkspace" successor
                           (list :workspace (agent-repl-test-host--ref))))))))

(ert-deftest agent-repl-test-host-not-yet-adopted-waits-when-no-successor-stands ()
  "Nothing is adopted onto a successor that has not been accepted."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act
    (agent-repl-host-handle-refusal "ws-1" '(:arm :not-yet-adopted :value nil))
    ;; Assert
    (should (null (assoc "AdoptHostWorkspace" agent-repl-test-host--calls)))))

(ert-deftest agent-repl-test-host-not-yet-adopted-retries-on-acceptance ()
  "Acceptance wakes the retry — one hook, no poll and no busy loop."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    (agent-repl-host-handle-refusal "ws-1" '(:arm :not-yet-adopted :value nil))
    (let ((successor (agent-repl-connect-open "127.0.0.1:9100")))
      ;; Act
      (run-hook-with-args 'agent-repl-link-handover-functions nil successor)
      ;; Assert
      (should (agent-repl-test-host--logged-p :info "elisp.host.adopt-retry")))))

(ert-deftest agent-repl-test-host-adopt-retry-hook-removes-itself ()
  "The retry is ONE-SHOT: a later handover must not re-adopt out of nowhere."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    (agent-repl-host-handle-refusal "ws-1" '(:arm :not-yet-adopted :value nil))
    (let ((successor (agent-repl-connect-open "127.0.0.1:9100")))
      (run-hook-with-args 'agent-repl-link-handover-functions nil successor)
      (setq agent-repl-test-host--calls nil)
      ;; Act
      (run-hook-with-args 'agent-repl-link-handover-functions nil successor)
      ;; Assert
      (should (null agent-repl-test-host--calls)))))

(ert-deftest agent-repl-test-host-unknown-refusal-arm-is-an-error ()
  "An arm that is not a handover refusal is a contract breach."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act
    (agent-repl-host-handle-refusal "ws-1" '(:arm :budget-exceeded :value nil))
    ;; Assert
    (should (agent-repl-test-host--logged-p :error "elisp.host.unknown-refusal-arm"))))

;;;; ---- The handover's webview redial ----

(ert-deftest agent-repl-test-host-adoption-walks-conn-reload-adopt-cancel-subscribe ()
  "THE ORDER IS THE CONTRACT, and the whole of it is asserted here.

THE RELOAD COMES FIRST, and it is a fix rather than a preference.  The
successor\='s adopt is a RENDEZVOUS that completes only when every
participant snapshotted at announcement has called, and for an OPEN
workspace those are the host AND the reloaded page.  Reloading only after
the adopt ANSWERED made the host\='s call wait for the very page its own
return was supposed to create: measured in the sandbox,
`AdoptHostWorkspace timed out after 10s\=' and the outgoing daemon then
sat out its whole 30s adoption window."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (setq agent-repl-test-host--successor (agent-repl-connect-open "127.0.0.1:9100"))
    (agent-repl-test-host--subscribe "ws-1")
    (setq agent-repl-test-host--walk nil)
    ;; Act
    (agent-repl-test-host--push "ws-1" '(:arm :transferred :value nil))
    ;; Assert
    (should (equal (reverse agent-repl-test-host--walk)
                   '(:reload :adopt :cancel :subscribe)))))

(ert-deftest agent-repl-test-host-adoption-reloads-before-the-adopt-is-issued ()
  "The web participant exists BEFORE the host waits on it, or neither does.

This is the deadlock\='s own regression test, stated as the one ordering
that breaks it: the reload is recorded before the AdoptHostWorkspace call
is even issued, so the reloaded page can complete the rendezvous the
host\='s call is parked in."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (setq agent-repl-test-host--successor (agent-repl-connect-open "127.0.0.1:9100"))
    (agent-repl-test-host--subscribe "ws-1")
    (setq agent-repl-test-host--walk nil)
    ;; Act
    (agent-repl-test-host--push "ws-1" '(:arm :transferred :value nil))
    ;; Assert
    (let ((walk (reverse agent-repl-test-host--walk)))
      (should (< (seq-position walk :reload) (seq-position walk :adopt))))))

(ert-deftest agent-repl-test-host-adoption-moves-the-conn-before-the-reload ()
  "frontend.el reads the URL off `agent-repl-host-conn': reloading first
would navigate the page straight back at the daemon that released it."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (let ((successor (agent-repl-connect-open "127.0.0.1:9100")))
      (setq agent-repl-test-host--successor successor)
      (agent-repl-test-host--subscribe "ws-1")
      ;; Act
      (agent-repl-test-host--push "ws-1" '(:arm :transferred :value nil))
      ;; Assert
      (should (eq (cdr (assq :reload-conn agent-repl-test-host--effects))
                  successor)))))

(ert-deftest agent-repl-test-host-adoption-reloads-the-webview-once ()
  "One adoption is one navigation, never two."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (setq agent-repl-test-host--successor (agent-repl-connect-open "127.0.0.1:9100"))
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act
    (agent-repl-test-host--push "ws-1" '(:arm :transferred :value nil))
    ;; Assert
    (should (= 1 (length (seq-filter (lambda (e) (eq (car-safe e) :reload))
                                     agent-repl-test-host--effects))))))

(ert-deftest agent-repl-test-host-adoption-logs-the-webview-redial ()
  "The page's move is on the record with the address it moved to."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (setq agent-repl-test-host--successor (agent-repl-connect-open "127.0.0.1:9100"))
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act
    (agent-repl-test-host--push "ws-1" '(:arm :transferred :value nil))
    ;; Assert
    (should (agent-repl-test-host--logged-p
             :info "elisp.host.webview-redialed ws=ws-1 address=\"127.0.0.1:9100\""))))

(ert-deftest agent-repl-test-host-refused-adopt-puts-the-page-back-on-the-old-daemon ()
  "An adopt that was REFUSED moved nothing, so the page ends where it began.

AMENDED, and the reason is the deadlock above: the page is redialed
BEFORE the adopt is issued, because it is the participant the adopt waits
on.  So a refusal can no longer be answered by never having moved it —
it is answered by moving it back, which is the same guarantee stated
about the END state instead of about the absence of an act."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (let ((old (agent-repl-connect-open "127.0.0.1:9001")))
      (setq agent-repl-test-host--successor (agent-repl-connect-open "127.0.0.1:9100")
            agent-repl-test-host--adopt-answer
            (list :response (list :arm :error :value (list :message "no"))))
      (agent-repl-test-host--subscribe "ws-1" old)
      ;; Act
      (agent-repl-test-host--push "ws-1" '(:arm :transferred :value nil))
      ;; Assert: the LAST reload the page was given saw the OLD connection,
      ;; which is the URL frontend.el would have navigated it to. `assq'
      ;; answers the most recent record, because effects are pushed.
      (should (eq (cdr (assq :reload-conn agent-repl-test-host--effects)) old)))))

(ert-deftest agent-repl-test-host-failed-adopt-puts-the-page-back-on-the-old-daemon ()
  "A TRANSPORT failure restores the page too — not only a refusal arm."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (let ((old (agent-repl-connect-open "127.0.0.1:9001")))
      (setq agent-repl-test-host--successor (agent-repl-connect-open "127.0.0.1:9100")
            agent-repl-test-host--adopt-answer
            (list :failure (list :kind :timeout :message "AdoptHostWorkspace timed out")))
      (agent-repl-test-host--subscribe "ws-1" old)
      ;; Act
      (agent-repl-test-host--push "ws-1" '(:arm :transferred :value nil))
      ;; Assert
      (should (eq (agent-repl-host-conn "ws-1") old))
      (should (eq (cdr (assq :reload-conn agent-repl-test-host--effects)) old)))))

(ert-deftest agent-repl-test-host-refused-adopt-leaves-the-conn-on-the-old-daemon ()
  "Moving `:conn' on a refusal would point the page at a daemon that said no."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (let ((old (agent-repl-connect-open "127.0.0.1:9001")))
      (setq agent-repl-test-host--successor (agent-repl-connect-open "127.0.0.1:9100")
            agent-repl-test-host--adopt-answer
            (list :response (list :arm :error :value (list :message "no"))))
      (agent-repl-test-host--subscribe "ws-1" old)
      ;; Act
      (agent-repl-test-host--push "ws-1" '(:arm :transferred :value nil))
      ;; Assert
      (should (eq (agent-repl-host-conn "ws-1") old)))))

(ert-deftest agent-repl-test-host-transferring-away-reloads-the-webview ()
  "The refusal path is the same walk, so the page moves there too."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (setq agent-repl-test-host--successor (agent-repl-connect-open "127.0.0.1:9100"))
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act
    (agent-repl-host-handle-refusal
     "ws-1" '(:arm :transferring-away :value (:address "127.0.0.1:9100")))
    ;; Assert
    (should (= 1 (length (seq-filter (lambda (e) (eq (car-safe e) :reload))
                                     agent-repl-test-host--effects))))))

(ert-deftest agent-repl-test-host-transferring-away-moves-the-conn-before-the-reload ()
  "Same order on the refusal path, for the same reason."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (let ((successor (agent-repl-connect-open "127.0.0.1:9100")))
      (setq agent-repl-test-host--successor successor)
      (agent-repl-test-host--subscribe "ws-1")
      ;; Act
      (agent-repl-host-handle-refusal
       "ws-1" '(:arm :transferring-away :value (:address "127.0.0.1:9100")))
      ;; Assert
      (should (eq (cdr (assq :reload-conn agent-repl-test-host--effects))
                  successor)))))

(ert-deftest agent-repl-test-host-transferred-adopts-the-recorded-successor ()
  "A `transferred' push adopts onto the link\='s RECORDED successor.
`HostWorkspaceTransferred' is EMPTY on the wire, so the push names no
daemon: the successor is the one `shutdown_announced{address}' already
made the link dial and accept.  Nothing is dialed here."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (let ((successor (agent-repl-connect-open "127.0.0.1:9100")))
      (setq agent-repl-test-host--successor successor)
      (agent-repl-test-host--subscribe "ws-1")
      ;; Act
      (agent-repl-test-host--push "ws-1" '(:arm :transferred :value nil))
      ;; Assert
      (should (eq (nth 1 (assoc "AdoptHostWorkspace" agent-repl-test-host--calls))
                  successor)))))

(ert-deftest agent-repl-test-host-transferred-dials-nobody ()
  "The `transferred' push dials NO daemon of its own.
The message is empty, so there is no address to dial toward: a dial here
could only be invented, and the link already holds the accepted successor."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (setq agent-repl-test-host--successor (agent-repl-connect-open "127.0.0.1:9100"))
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act
    (agent-repl-test-host--push "ws-1" '(:arm :transferred :value nil))
    ;; Assert
    (should (null agent-repl-test-host--dialled))))

;;;; ---- Subscription acceptance ----

(ert-deftest agent-repl-test-host-subscribe-alone-is-not-subscribed ()
  "A spawn is not an acceptance: nothing may claim the watch stands yet."
  (agent-repl-test-host--with-harness
    ;; Arrange / Act
    (agent-repl-test-host--subscribe "ws-1")
    ;; Assert
    (should (null (agent-repl-test-host--logged-p :info "elisp.host.subscribed")))))

(ert-deftest agent-repl-test-host-acceptance-logs-subscribed ()
  "The daemon accepting the watch is what puts `subscribed' on the record."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (let ((record (agent-repl-test-host--subscribe "ws-1")))
      ;; Act
      (funcall (plist-get record :on-open))
      ;; Assert
      (should (agent-repl-test-host--logged-p :info "elisp.host.subscribed")))))

(ert-deftest agent-repl-test-host-acceptance-names-the-method ()
  "The subscribed record says WHICH subscription was accepted."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (let ((record (agent-repl-test-host--subscribe "ws-1")))
      ;; Act
      (funcall (plist-get record :on-open))
      ;; Assert
      (should (agent-repl-test-host--logged-p :info "method=\"WatchHostWorkspace\"")))))

;;;; ---- Teardown ----

(ert-deftest agent-repl-test-host-a-tombstoned-workspace-is-forgotten ()
  "The tombstone hook drops WS's host entry: nothing answers for it after."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act
    (run-hook-with-args 'agent-repl-ws-del-hook "ws-1")
    ;; Assert
    (should (null (gethash "ws-1" agent-repl-host--by-name)))))

(ert-deftest agent-repl-test-host-a-tombstoned-workspace-s-stream-is-cancelled ()
  "Forgetting WS cancels its standing host stream."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act
    (run-hook-with-args 'agent-repl-ws-del-hook "ws-1")
    ;; Assert
    (should (equal (length agent-repl-test-host--cancelled) 1))))

(ert-deftest agent-repl-test-host-a-promotion-re-registers-no-torn-down-workspace ()
  "A workspace torn down before a promotion is not walked onto the successor.
MEASURED 2026-09-29: two nuked workspaces were re-registered on a deploy's
promotion and refused with `not-a-worktree'."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (let ((old (agent-repl-connect-open "127.0.0.1:9001"))
          (new (agent-repl-connect-open "127.0.0.1:9002")))
      (agent-repl-test-host--subscribe "ws-1" old)
      (run-hook-with-args 'agent-repl-ws-del-hook "ws-1")
      (setq agent-repl-test-host--calls nil)
      ;; Act
      (agent-repl-host-on-link-promote old new)
      ;; Assert
      (should-not (assoc "RegisterWorkspace" agent-repl-test-host--calls)))))

;;;; ---- Rename ----

(ert-deftest agent-repl-test-host-rename-re-keys-the-entry ()
  "A renamed workspace answers under the NEW name."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act
    (agent-repl-host-rename "ws-1" "ws-2")
    ;; Assert
    (should (equal (agent-repl-host-ref "ws-2") (agent-repl-test-host--ref)))))

(ert-deftest agent-repl-test-host-rename-drops-the-old-name ()
  "The old key is gone, so nothing keeps answering for a name that ended."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act
    (agent-repl-host-rename "ws-1" "ws-2")
    ;; Assert
    (should (null (agent-repl-host-ref "ws-1")))))

(ert-deftest agent-repl-test-host-rename-carries-the-stream ()
  "The standing stream moves with the name; a teardown by NEW cancels it."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act
    (agent-repl-host-rename "ws-1" "ws-2")
    (agent-repl-host-unsubscribe "ws-2")
    ;; Assert
    (should (equal (length agent-repl-test-host--cancelled) 1))))

(ert-deftest agent-repl-test-host-rename-carries-the-conn ()
  "The owning connection moves with the name."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (let ((conn (agent-repl-connect-open "127.0.0.1:9001")))
      (agent-repl-test-host--subscribe "ws-1" conn)
      ;; Act
      (agent-repl-host-rename "ws-1" "ws-2")
      ;; Assert
      (should (eq (agent-repl-host-conn "ws-2") conn)))))

(ert-deftest agent-repl-test-host-a-push-after-a-rename-updates-the-new-name ()
  "The stream's own pushes reach the NEW name's gate, not a dead key's."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    (agent-repl-host-rename "ws-1" "ws-2")
    ;; Act
    (agent-repl-host--handle-push
     "ws-2" (list :arm :host :value (agent-repl-test-host--composer :merging)))
    ;; Assert
    (should (eq (agent-repl-host-composer-gate "ws-2") :merging))))

(ert-deftest agent-repl-test-host-a-stream-callback-after-a-rename-resolves-the-new-name ()
  "The STREAM'S OWN push, not a hand-named one, must reach the NEW name's gate.
The stream outlives the rename, so a callback closed over the
subscribe-time name would keep updating the old key and the renamed tab's
gate would never advance again.  The ref id is the tab identity, so the
name is resolved from it at call time."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    (agent-repl-host-rename "ws-1" "ws-2")
    (let ((agent-repl--workspaces (make-hash-table :test 'equal)))
      (puthash "ws-2" (list :ref (agent-repl-test-host--ref)) agent-repl--workspaces)
      ;; Act
      (agent-repl-test-host--push
       "ws-2" (list :arm :host :value (agent-repl-test-host--composer :merging)))
      ;; Assert
      (should (eq (agent-repl-host-composer-gate "ws-2") :merging)))))

(ert-deftest agent-repl-test-host-a-stream-callback-after-a-rename-leaves-the-old-name ()
  "The old name is left with nothing: the push landed on ONE gate, not two.
`:unknown' is the answer for a name that holds no host state at all, so
this is the assertion that the push did not resurrect the dead key."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    (agent-repl-host-rename "ws-1" "ws-2")
    (let ((agent-repl--workspaces (make-hash-table :test 'equal)))
      (puthash "ws-2" (list :ref (agent-repl-test-host--ref)) agent-repl--workspaces)
      ;; Act
      (agent-repl-test-host--push
       "ws-2" (list :arm :host :value (agent-repl-test-host--composer :merging)))
      ;; Assert
      (should (eq (agent-repl-host-composer-gate "ws-1") :unknown)))))

(ert-deftest agent-repl-test-host-a-stream-callback-falls-back-when-the-id-is-gone ()
  "A ref id that no longer resolves falls back to the subscribe-time name.
A closed or tombstoned workspace has no live entry to find, and the name
the subscription was opened under is then the best the record has -- the
callback must still run rather than resolve to nil."
  (agent-repl-test-host--with-harness
    ;; Arrange: no live workspace carries this ref id.
    (agent-repl-test-host--subscribe "ws-1")
    (let ((agent-repl--workspaces (make-hash-table :test 'equal)))
      ;; Act
      (agent-repl-test-host--push
       "ws-1" (list :arm :host :value (agent-repl-test-host--composer :merging)))
      ;; Assert
      (should (eq (agent-repl-host-composer-gate "ws-1") :merging)))))

(ert-deftest agent-repl-test-host-rename-logs-the-move ()
  "Every branch records; the move is INFO with both names."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act
    (agent-repl-host-rename "ws-1" "ws-2")
    ;; Assert
    (should (agent-repl-test-host--logged-p :info "elisp.host.renamed"))))

(ert-deftest agent-repl-test-host-rename-of-an-unknown-workspace-is-a-noop ()
  "A rename before the first subscribe has nothing to move and is not a failure."
  (agent-repl-test-host--with-harness
    ;; Arrange / Act
    (let ((moved (agent-repl-host-rename "ws-1" "ws-2")))
      ;; Assert
      (should (null moved)))))

(ert-deftest agent-repl-test-host-rename-onto-an-occupied-name-is-refused ()
  "Two workspaces must never share one entry: the target is refused loudly."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    (agent-repl-test-host--subscribe "ws-2" nil (agent-repl-test-host--ref "ws-id-2"))
    ;; Act
    (agent-repl-host-rename "ws-1" "ws-2")
    ;; Assert
    (should (agent-repl-test-host--logged-p :error "elisp.host.rename-target-occupied"))))

(ert-deftest agent-repl-test-host-a-refused-rename-leaves-both-entries ()
  "A refused rename moves nothing: the old name still answers."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    (agent-repl-test-host--subscribe "ws-2" nil (agent-repl-test-host--ref "ws-id-2"))
    ;; Act
    (agent-repl-host-rename "ws-1" "ws-2")
    ;; Assert
    (should (equal (plist-get (agent-repl-host-ref "ws-1") :id) "ws-id-1"))))

;;;; ---- A workspace's link follows the live daemon ----
;;
;; Regression, 2026-09-27: a daemon exited without transferring a workspace,
;; Emacs's link was promoted onto its successor, and the workspace kept its
;; dead connection -- every SelectWorkspace and MarkWorkspaceViewed went to
;; the dead address and its webview stayed blank on the dead origin.

(defmacro agent-repl-test-host--with-dir (&rest body)
  "Run BODY with every workspace's project dir answering /tmp/ws-1."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'agent-repl--ws-get)
              (lambda (_ws key) (and (eq key :project-dir) "/tmp/ws-1"))))
     ,@body))

(defun agent-repl-test-host--lose (ws)
  "Drop WS's standing host stream the way a daemon's exit does."
  (funcall (plist-get (agent-repl-host-stream ws) :on-close)
           (list :error (list :kind :no-end-frame
                              :message "WatchHostWorkspace: producer closed without an end frame"))))

(defun agent-repl-test-host--calls-to (method conn)
  "Return the recorded METHOD calls sent on CONN."
  (seq-filter (lambda (call) (and (equal (car call) method) (eq (nth 1 call) conn)))
              agent-repl-test-host--calls))

;;;; ---- A planned ending: the daemon's last frame says the end is expected ----

(defun agent-repl-test-host--end-planned (ws)
  "Deliver the planned ending on WS's host stream, then its clean end."
  (let ((stream (agent-repl-host-stream ws)))
    (funcall (plist-get stream :on-push) '(:arm :ending :value nil))
    (funcall (plist-get stream :on-close) '(:ended))))

(ert-deftest agent-repl-test-host-ending-push-is-recorded-at-info ()
  "The planned-ending frame itself is on the record, at INFO."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act
    (agent-repl-test-host--push "ws-1" '(:arm :ending :value nil))
    ;; Assert
    (should (agent-repl-test-host--logged-p :info "elisp.host.stream-ending ws=ws-1"))))

(ert-deftest agent-repl-test-host-planned-end-is-recorded-at-info ()
  "A clean end after the planned ending is INFO, never the ERROR of a loss."
  (agent-repl-test-host--with-harness
    (agent-repl-test-host--with-dir
      ;; Arrange
      (agent-repl-test-host--subscribe "ws-1")
      ;; Act
      (agent-repl-test-host--end-planned "ws-1")
      ;; Assert
      (should (agent-repl-test-host--logged-p :info "elisp.host.stream-ended-planned ws=ws-1")))))

(ert-deftest agent-repl-test-host-planned-end-writes-no-stream-lost ()
  "A planned end never writes the stream-lost ERROR."
  (agent-repl-test-host--with-harness
    (agent-repl-test-host--with-dir
      ;; Arrange
      (agent-repl-test-host--subscribe "ws-1")
      ;; Act
      (agent-repl-test-host--end-planned "ws-1")
      ;; Assert
      (should-not (agent-repl-test-host--logged-p :error "elisp.host.stream-lost")))))

(ert-deftest agent-repl-test-host-planned-end-reattaches-on-the-live-daemon ()
  "A planned end takes the single reattach walk, with trigger planned-ending."
  (agent-repl-test-host--with-harness
    (agent-repl-test-host--with-dir
      ;; Arrange
      (let ((old (agent-repl-connect-open "127.0.0.1:61043"))
            (live (agent-repl-connect-open "127.0.0.1:58175")))
        (agent-repl-test-host--subscribe "ws-1" old)
        (setq agent-repl-test-host--live live)
        ;; Act
        (agent-repl-test-host--end-planned "ws-1")
        ;; Assert
        (should (agent-repl-test-host--logged-p
                 :info "elisp.host.reattached ws=ws-1 address=\"127.0.0.1:58175\" trigger=planned-ending"))))))

(ert-deftest agent-repl-test-host-clean-end-without-ending-is-still-lost ()
  "A clean end the daemon did NOT announce stays the stream-lost ERROR."
  (agent-repl-test-host--with-harness
    (agent-repl-test-host--with-dir
      ;; Arrange
      (agent-repl-test-host--subscribe "ws-1")
      ;; Act
      (funcall (plist-get (agent-repl-host-stream "ws-1") :on-close) '(:ended))
      ;; Assert
      (should (agent-repl-test-host--logged-p :error "elisp.host.stream-lost ws=ws-1")))))

(ert-deftest agent-repl-test-host-error-after-ending-is-still-lost ()
  "A transport error after the ending is a loss: only a CLEAN end is planned."
  (agent-repl-test-host--with-harness
    (agent-repl-test-host--with-dir
      ;; Arrange
      (agent-repl-test-host--subscribe "ws-1")
      (agent-repl-test-host--push "ws-1" '(:arm :ending :value nil))
      ;; Act
      (agent-repl-test-host--lose "ws-1")
      ;; Assert
      (should (agent-repl-test-host--logged-p :error "elisp.host.stream-lost ws=ws-1")))))

(ert-deftest agent-repl-test-host-ending-on-an-old-stream-does-not-plan-the-new-one ()
  "The ending marks only the stream that carried it, never its replacement."
  (agent-repl-test-host--with-harness
    (agent-repl-test-host--with-dir
      ;; Arrange
      (let ((old (agent-repl-test-host--subscribe "ws-1")))
        (funcall (plist-get old :on-push) '(:arm :ending :value nil))
        (agent-repl-test-host--subscribe "ws-1")
        ;; Act
        (funcall (plist-get (agent-repl-host-stream "ws-1") :on-close) '(:ended))
        ;; Assert
        (should (agent-repl-test-host--logged-p :error "elisp.host.stream-lost ws=ws-1"))))))

(ert-deftest agent-repl-test-host-stream-lost-registers-on-the-live-daemon ()
  "A lost stream re-registers the workspace on the daemon the link now names."
  (agent-repl-test-host--with-harness
    (agent-repl-test-host--with-dir
      ;; Arrange
      (let ((dead (agent-repl-connect-open "127.0.0.1:61043"))
            (live (agent-repl-connect-open "127.0.0.1:58175")))
        (agent-repl-test-host--subscribe "ws-1" dead)
        (setq agent-repl-test-host--live live
              agent-repl-test-host--calls nil)
        ;; Act
        (agent-repl-test-host--lose "ws-1")
        ;; Assert
        (should (equal (agent-repl-test-host--calls-to "RegisterWorkspace" live)
                       (list (list "RegisterWorkspace" live '(:dir "/tmp/ws-1")))))))))

(ert-deftest agent-repl-test-host-stream-lost-resubscribes-on-the-live-daemon ()
  "The workspace's stream and connection move to the live daemon."
  (agent-repl-test-host--with-harness
    (agent-repl-test-host--with-dir
      ;; Arrange
      (let ((dead (agent-repl-connect-open "127.0.0.1:61043"))
            (live (agent-repl-connect-open "127.0.0.1:58175")))
        (agent-repl-test-host--subscribe "ws-1" dead)
        (setq agent-repl-test-host--live live)
        ;; Act
        (agent-repl-test-host--lose "ws-1")
        ;; Assert
        (should (eq (plist-get (agent-repl-host-stream "ws-1") :conn) live))))))

(ert-deftest agent-repl-test-host-stream-lost-reloads-the-webview-onto-the-live-daemon ()
  "The page is navigated to the LIVE daemon's origin, never the dead one's."
  (agent-repl-test-host--with-harness
    (agent-repl-test-host--with-dir
      ;; Arrange
      (let ((dead (agent-repl-connect-open "127.0.0.1:61043"))
            (live (agent-repl-connect-open "127.0.0.1:58175")))
        (agent-repl-test-host--subscribe "ws-1" dead)
        (setq agent-repl-test-host--live live)
        ;; Act
        (agent-repl-test-host--lose "ws-1")
        ;; Assert
        (should (eq (cdr (assq :reload-conn agent-repl-test-host--effects)) live))))))

(ert-deftest agent-repl-test-host-stream-lost-records-the-reattach-at-info ()
  "The loss is ERROR already; the reattach that answers it is INFO."
  (agent-repl-test-host--with-harness
    (agent-repl-test-host--with-dir
      ;; Arrange
      (let ((dead (agent-repl-connect-open "127.0.0.1:61043"))
            (live (agent-repl-connect-open "127.0.0.1:58175")))
        (agent-repl-test-host--subscribe "ws-1" dead)
        (setq agent-repl-test-host--live live)
        ;; Act
        (agent-repl-test-host--lose "ws-1")
        ;; Assert
        (should (agent-repl-test-host--logged-p
                 :info "elisp.host.reattached ws=ws-1 address=\"127.0.0.1:58175\" trigger=stream-lost"))))))

(ert-deftest agent-repl-test-host-stream-lost-with-no-link-sends-nothing ()
  "With no daemon reachable nothing is dialed and nothing polls."
  (agent-repl-test-host--with-harness
    (agent-repl-test-host--with-dir
      ;; Arrange
      (agent-repl-test-host--subscribe "ws-1")
      (setq agent-repl-test-host--calls nil)
      ;; Act
      (agent-repl-test-host--lose "ws-1")
      ;; Assert
      (should (and (null agent-repl-test-host--calls)
                   (null agent-repl-test-host--timers))))))

(ert-deftest agent-repl-test-host-stream-lost-with-no-link-records-the-wait ()
  "The wait for the link is on the record, at INFO."
  (agent-repl-test-host--with-harness
    (agent-repl-test-host--with-dir
      ;; Arrange
      (agent-repl-test-host--subscribe "ws-1")
      ;; Act
      (agent-repl-test-host--lose "ws-1")
      ;; Assert
      (should (agent-repl-test-host--logged-p
               :info "elisp.host.reattach-awaiting-link ws=ws-1 trigger=stream-lost")))))

(ert-deftest agent-repl-test-host-stream-lost-with-no-link-reattaches-on-link-up ()
  "The link-up edge is what re-attaches a workspace no daemon could take."
  (agent-repl-test-host--with-harness
    (agent-repl-test-host--with-dir
      ;; Arrange
      (let ((live (agent-repl-connect-open "127.0.0.1:58175")))
        (agent-repl-test-host--subscribe "ws-1")
        (agent-repl-test-host--lose "ws-1")
        (cl-letf (((symbol-function 'agent-repl--live-ws-names) (lambda () '("ws-1"))))
          ;; Act
          (agent-repl-host-on-link-up live))
        ;; Assert
        (should (eq (plist-get (agent-repl-host-stream "ws-1") :conn) live))))))

(ert-deftest agent-repl-test-host-stream-lost-on-a-daemon-handing-over-waits-for-the-promotion ()
  "The daemon that lost the stream is leaving: its successor's promotion re-attaches.
Registering on a daemon that is handing over would only fail."
  (agent-repl-test-host--with-harness
    (agent-repl-test-host--with-dir
      ;; Arrange
      (let ((leaving (agent-repl-connect-open "127.0.0.1:61043")))
        (agent-repl-test-host--subscribe "ws-1" leaving)
        (setq agent-repl-test-host--live leaving
              agent-repl-test-host--successor (agent-repl-connect-open "127.0.0.1:58175")
              agent-repl-test-host--calls nil)
        ;; Act
        (agent-repl-test-host--lose "ws-1")
        ;; Assert
        (should (and (null agent-repl-test-host--calls)
                     (agent-repl-test-host--logged-p
                      :info "elisp.host.reattach-awaiting-promotion ws=ws-1")))))))

;; Regression, 2026-10-03: a bounce's announced stand-down ended every
;; workspace's host stream before the link's own stream, and each workspace
;; was registered on the daemon that had just said it was leaving.

(ert-deftest agent-repl-test-host-planned-end-on-the-departing-live-daemon-registers-nothing ()
  "A planned end while the link still names the departing daemon calls nothing."
  (agent-repl-test-host--with-harness
    (agent-repl-test-host--with-dir
      ;; Arrange
      (let ((leaving (agent-repl-connect-open "127.0.0.1:61043")))
        (agent-repl-test-host--subscribe "ws-1" leaving)
        (setq agent-repl-test-host--live leaving
              agent-repl-test-host--calls nil)
        ;; Act
        (agent-repl-test-host--end-planned "ws-1")
        ;; Assert
        (should (null (agent-repl-test-host--calls-to "RegisterWorkspace" leaving)))))))

(ert-deftest agent-repl-test-host-planned-end-on-the-departing-live-daemon-records-the-wait ()
  "The wait for the link edge is on the record, at INFO."
  (agent-repl-test-host--with-harness
    (agent-repl-test-host--with-dir
      ;; Arrange
      (let ((leaving (agent-repl-connect-open "127.0.0.1:61043")))
        (agent-repl-test-host--subscribe "ws-1" leaving)
        (setq agent-repl-test-host--live leaving)
        ;; Act
        (agent-repl-test-host--end-planned "ws-1")
        ;; Assert
        (should (agent-repl-test-host--logged-p
                 :info "elisp.host.reattach-awaiting-link-edge ws=ws-1 trigger=planned-ending"))))))

(ert-deftest agent-repl-test-host-planned-end-on-the-departing-live-daemon-reattaches-on-link-up ()
  "The link-up edge onto the next daemon re-attaches the waiting workspace."
  (agent-repl-test-host--with-harness
    (agent-repl-test-host--with-dir
      ;; Arrange
      (let ((leaving (agent-repl-connect-open "127.0.0.1:61043"))
            (next (agent-repl-connect-open "127.0.0.1:58175")))
        (agent-repl-test-host--subscribe "ws-1" leaving)
        (setq agent-repl-test-host--live leaving)
        (agent-repl-test-host--end-planned "ws-1")
        (setq agent-repl-test-host--live next)
        (cl-letf (((symbol-function 'agent-repl--live-ws-names) (lambda () '("ws-1"))))
          ;; Act
          (agent-repl-host-on-link-up next))
        ;; Assert
        (should (eq (plist-get (agent-repl-host-stream "ws-1") :conn) next))))))

(ert-deftest agent-repl-test-host-stream-lost-on-a-live-daemon-that-announced-its-ending-waits ()
  "A loss on a daemon whose link carried the ending waits for the link edge too."
  (agent-repl-test-host--with-harness
    (agent-repl-test-host--with-dir
      ;; Arrange
      (let ((leaving (agent-repl-connect-open "127.0.0.1:61043")))
        (agent-repl-test-host--subscribe "ws-1" leaving)
        (setq agent-repl-test-host--live leaving
              agent-repl-test-host--ending leaving
              agent-repl-test-host--calls nil)
        ;; Act
        (agent-repl-test-host--lose "ws-1")
        ;; Assert
        (should (null (agent-repl-test-host--calls-to "RegisterWorkspace" leaving)))))))

(ert-deftest agent-repl-test-host-stream-lost-on-a-live-daemon-that-said-nothing-still-registers ()
  "A loss on a live daemon that announced no ending is walked onto it at once."
  (agent-repl-test-host--with-harness
    (agent-repl-test-host--with-dir
      ;; Arrange
      (let ((live (agent-repl-connect-open "127.0.0.1:61043")))
        (agent-repl-test-host--subscribe "ws-1" live)
        (setq agent-repl-test-host--live live
              agent-repl-test-host--calls nil)
        ;; Act
        (agent-repl-test-host--lose "ws-1")
        ;; Assert
        (should (agent-repl-test-host--calls-to "RegisterWorkspace" live))))))

(ert-deftest agent-repl-test-host-stale-stream-close-keeps-the-standing-stream ()
  "A close of a stream the workspace already left does not drop the one standing."
  (agent-repl-test-host--with-harness
    (agent-repl-test-host--with-dir
      ;; Arrange
      (let* ((old (agent-repl-test-host--subscribe "ws-1"))
             (standing (agent-repl-test-host--subscribe
                        "ws-1" (agent-repl-connect-open "127.0.0.1:58175"))))
        ;; Act
        (funcall (plist-get old :on-close) '(:ended))
        ;; Assert
        (should (eq (agent-repl-host-stream "ws-1") standing))))))

(ert-deftest agent-repl-test-host-call-on-a-dead-connection-goes-to-the-live-daemon ()
  "A call whose connection is closed goes to the daemon that serves Emacs now."
  (agent-repl-test-host--with-harness
    (agent-repl-test-host--with-dir
      ;; Arrange
      (let ((dead (agent-repl-connect-open "127.0.0.1:61043"))
            (live (agent-repl-connect-open "127.0.0.1:58175")))
        (agent-repl-test-host--subscribe "ws-1" dead)
        (agent-repl-connect-close dead)
        (setq agent-repl-test-host--live live
              agent-repl-test-host--calls nil)
        ;; Act
        (agent-repl-host-select "ws-1")
        ;; Assert
        (should (agent-repl-test-host--calls-to "SelectWorkspace" live))))))

(ert-deftest agent-repl-test-host-call-on-a-dead-connection-never-reaches-the-dead-address ()
  "Nothing more is sent to an address whose connection is known dead."
  (agent-repl-test-host--with-harness
    (agent-repl-test-host--with-dir
      ;; Arrange
      (let ((dead (agent-repl-connect-open "127.0.0.1:61043"))
            (live (agent-repl-connect-open "127.0.0.1:58175")))
        (agent-repl-test-host--subscribe "ws-1" dead)
        (agent-repl-connect-close dead)
        (setq agent-repl-test-host--live live
              agent-repl-test-host--calls nil)
        ;; Act
        (agent-repl-host-mark-viewed "ws-1")
        ;; Assert
        (should (null (seq-filter (lambda (call) (eq (nth 1 call) dead))
                                  agent-repl-test-host--calls)))))))

(ert-deftest agent-repl-test-host-call-on-a-dead-connection-reattaches-the-workspace ()
  "Finding the connection dead re-attaches the workspace, stream and all."
  (agent-repl-test-host--with-harness
    (agent-repl-test-host--with-dir
      ;; Arrange
      (let ((dead (agent-repl-connect-open "127.0.0.1:61043"))
            (live (agent-repl-connect-open "127.0.0.1:58175")))
        (agent-repl-test-host--subscribe "ws-1" dead)
        (agent-repl-connect-close dead)
        (setq agent-repl-test-host--live live)
        ;; Act
        (agent-repl-host-select "ws-1")
        ;; Assert
        (should (agent-repl-test-host--logged-p
                 :info "elisp.host.reattached ws=ws-1 address=\"127.0.0.1:58175\" trigger=dead-connection"))))))

(ert-deftest agent-repl-test-host-call-on-a-dead-connection-with-no-link-sends-nothing ()
  "With no daemon reachable the call has no connection and is not sent."
  (agent-repl-test-host--with-harness
    (agent-repl-test-host--with-dir
      ;; Arrange
      (let ((dead (agent-repl-connect-open "127.0.0.1:61043")))
        (agent-repl-test-host--subscribe "ws-1" dead)
        (agent-repl-connect-close dead)
        (setq agent-repl-test-host--calls nil)
        ;; Act
        (agent-repl-host-select "ws-1")
        ;; Assert
        (should (null agent-repl-test-host--calls))))))

(ert-deftest agent-repl-test-host-promotion-reattaches-a-workspace-left-on-the-old-daemon ()
  "A workspace the old daemon never transferred is walked onto the successor."
  (agent-repl-test-host--with-harness
    (agent-repl-test-host--with-dir
      ;; Arrange
      (let ((old (agent-repl-connect-open "127.0.0.1:61043"))
            (new (agent-repl-connect-open "127.0.0.1:58175")))
        (agent-repl-test-host--subscribe "ws-1" old)
        ;; Act
        (agent-repl-host-on-link-promote old new)
        ;; Assert
        (should (eq (plist-get (agent-repl-host-stream "ws-1") :conn) new))))))

(ert-deftest agent-repl-test-host-promotion-reattaches-a-detached-workspace ()
  "A workspace that lost its daemon while waiting for the promotion is walked too."
  (agent-repl-test-host--with-harness
    (agent-repl-test-host--with-dir
      ;; Arrange
      (let ((old (agent-repl-connect-open "127.0.0.1:61043"))
            (new (agent-repl-connect-open "127.0.0.1:58175")))
        (agent-repl-test-host--subscribe "ws-1" (agent-repl-connect-open "127.0.0.1:50000"))
        (agent-repl-test-host--lose "ws-1")
        ;; Act
        (agent-repl-host-on-link-promote old new)
        ;; Assert
        (should (eq (plist-get (agent-repl-host-stream "ws-1") :conn) new))))))

(ert-deftest agent-repl-test-host-promotion-leaves-an-adopted-workspace-alone ()
  "A workspace already adopted onto the successor has nothing to rebuild."
  (agent-repl-test-host--with-harness
    (agent-repl-test-host--with-dir
      ;; Arrange
      (let ((old (agent-repl-connect-open "127.0.0.1:61043"))
            (new (agent-repl-connect-open "127.0.0.1:58175")))
        (agent-repl-test-host--subscribe "ws-1" new)
        (setq agent-repl-test-host--calls nil)
        ;; Act
        (agent-repl-host-on-link-promote old new)
        ;; Assert
        (should (null agent-repl-test-host--calls))))))

(ert-deftest agent-repl-test-host-promotion-is-on-the-promote-hook ()
  "The promotion edge reaches host.el through daemon-link's promote seam."
  ;; Arrange / Act / Assert
  (should (memq #'agent-repl-host-on-link-promote
                (default-value 'agent-repl-link-promote-functions))))

(ert-deftest agent-repl-test-host-transfer-notice-and-stream-loss-share-one-reattach-path ()
  "The transfer notice and a lost stream walk the SAME function, by claim."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (let ((successor (agent-repl-connect-open "127.0.0.1:9100"))
          (walks nil))
      (agent-repl-test-host--subscribe "ws-1")
      (setq agent-repl-test-host--successor successor
            agent-repl-test-host--live successor)
      (cl-letf (((symbol-function 'agent-repl-host--reattach)
                 (lambda (_ws _new claim trigger &rest _)
                   (push (list claim trigger) walks))))
        ;; Act
        (agent-repl-test-host--push "ws-1" '(:arm :transferred :value nil))
        (setq agent-repl-test-host--successor nil)
        (agent-repl-test-host--lose "ws-1"))
      ;; Assert
      (should (equal (reverse walks)
                     '((:adopt "transferred") (:register "stream-lost")))))))

(ert-deftest agent-repl-test-host-superseded-reattach-answer-is-dropped ()
  "A walk's late answer never moves the workspace back behind a newer walk."
  (agent-repl-test-host--with-harness
    (agent-repl-test-host--with-dir
      ;; Arrange
      (let ((first (agent-repl-connect-open "127.0.0.1:61043"))
            (second (agent-repl-connect-open "127.0.0.1:58175"))
            (answers nil)
            (success (list :arm :success
                           :value (list :workspace (agent-repl-test-host--ref)))))
        (cl-letf (((symbol-function 'agent-repl-rpc-register-workspace)
                   (lambda (_conn _request &rest keys)
                     (push (plist-get keys :on-response) answers))))
          (agent-repl-host--reattach "ws-1" first :register "stream-lost")
          (agent-repl-host--reattach "ws-1" second :register "promotion"))
        ;; Act
        (funcall (car answers) success)
        (funcall (cadr answers) success)
        ;; Assert
        (should (eq (agent-repl-host--recorded-conn "ws-1") second))))))

(ert-deftest agent-repl-test-host-reattach-register-refusal-leaves-the-workspace-detached ()
  "A daemon that will not register the workspace leaves it for the next edge."
  (agent-repl-test-host--with-harness
    (agent-repl-test-host--with-dir
      ;; Arrange
      (let ((dead (agent-repl-connect-open "127.0.0.1:61043"))
            (live (agent-repl-connect-open "127.0.0.1:58175")))
        (agent-repl-test-host--subscribe "ws-1" dead)
        (setq agent-repl-test-host--live live
              agent-repl-test-host--register-answer
              (list :response (list :arm :error :value (list :message "no"))))
        ;; Act
        (agent-repl-test-host--lose "ws-1")
        ;; Assert
        (should (and (plist-get (agent-repl-host--entry "ws-1") :detached)
                     (agent-repl-test-host--logged-p
                      :error "elisp.host.stream-lost-register-failed ws=ws-1")))))))

(ert-deftest agent-repl-test-host-reattach-runs-the-reattached-hook ()
  "The end of the walk tells the consumers that held work for the workspace."
  (agent-repl-test-host--with-harness
    (agent-repl-test-host--with-dir
      ;; Arrange
      (let ((live (agent-repl-connect-open "127.0.0.1:58175"))
            (told nil))
        (agent-repl-test-host--subscribe "ws-1")
        (setq agent-repl-test-host--live live)
        (let ((agent-repl-host-reattached-functions
               (list (lambda (ws) (push ws told)))))
          ;; Act
          (agent-repl-test-host--lose "ws-1"))
        ;; Assert
        (should (equal told '("ws-1")))))))

;; Regression, 2026-10-22: a handover's adopt had already moved the
;; workspace onto the successor when the old daemon's stream ended planned,
;; and the ending registered it back on the daemon that was leaving.

(ert-deftest agent-repl-test-host-adopting-p-answers-by-the-walk-in-flight ()
  "Only an adopt walk still waiting on its answer counts as adopting."
  (dolist (case '((:adopt . t) (:register . nil) (nil . nil)))
    (agent-repl-test-host--with-harness
      (cl-letf (((symbol-function 'agent-repl-host--adopt-onto) #'ignore)
                ((symbol-function 'agent-repl-host--register-onto) #'ignore))
        ;; Arrange
        (let ((successor (agent-repl-connect-open "127.0.0.1:58175")))
          (when (car case)
            (agent-repl-host--reattach "ws-1" successor (car case) "transferred"))
          ;; Act / Assert
          (should (eq (cdr case) (and (agent-repl-host-adopting-p "ws-1") t))))))))

(ert-deftest agent-repl-test-host-adopting-p-ends-with-the-reattach ()
  "A finished reattach leaves the workspace adopting no longer."
  (agent-repl-test-host--with-harness
    (agent-repl-test-host--with-dir
      ;; Arrange
      (let ((leaving (agent-repl-connect-open "127.0.0.1:61043"))
            (successor (agent-repl-connect-open "127.0.0.1:58175")))
        (agent-repl-test-host--subscribe "ws-1" leaving)
        (cl-letf (((symbol-function 'agent-repl-host--adopt-onto) #'ignore))
          (agent-repl-host--reattach "ws-1" successor :adopt "transferred"))
        ;; Act
        (agent-repl-host--reattached "ws-1" successor (agent-repl-host-ref "ws-1") "transferred")
        ;; Assert
        (should-not (agent-repl-host-adopting-p "ws-1"))))))

(ert-deftest agent-repl-test-host-planned-end-during-an-adopt-registers-nothing ()
  "A planned end while an adopt onto the successor is in flight calls nothing."
  (agent-repl-test-host--with-harness
    (agent-repl-test-host--with-dir
      ;; Arrange
      (let ((leaving (agent-repl-connect-open "127.0.0.1:61043"))
            (successor (agent-repl-connect-open "127.0.0.1:58175")))
        (agent-repl-test-host--subscribe "ws-1" leaving)
        (setq agent-repl-test-host--live leaving
              agent-repl-test-host--successor successor)
        (agent-repl-host--put "ws-1" :conn successor)
        (agent-repl-host--put "ws-1" :reattach (list :token 'adopt :conn successor :trigger "transferred"))
        (setq agent-repl-test-host--calls nil)
        ;; Act
        (agent-repl-test-host--end-planned "ws-1")
        ;; Assert
        (should (null (agent-repl-test-host--calls-to "RegisterWorkspace" leaving)))))))

(ert-deftest agent-repl-test-host-planned-end-during-an-adopt-records-the-walk-in-flight ()
  "The ending is answered by the walk already in flight, on the record."
  (agent-repl-test-host--with-harness
    (agent-repl-test-host--with-dir
      ;; Arrange
      (let ((leaving (agent-repl-connect-open "127.0.0.1:61043"))
            (successor (agent-repl-connect-open "127.0.0.1:58175")))
        (agent-repl-test-host--subscribe "ws-1" leaving)
        (setq agent-repl-test-host--live leaving
              agent-repl-test-host--successor successor)
        (agent-repl-host--put "ws-1" :conn successor)
        (agent-repl-host--put "ws-1" :reattach (list :token 'adopt :conn successor :trigger "transferred"))
        ;; Act
        (agent-repl-test-host--end-planned "ws-1")
        ;; Assert
        (should (agent-repl-test-host--logged-p
                 :log "elisp.host.reattach-in-flight ws=ws-1 trigger=planned-ending"))))))

(ert-deftest agent-repl-test-host-reattach-in-flight-is-not-walked-twice ()
  "A second loss found while a walk onto the live daemon is in flight sends nothing."
  (agent-repl-test-host--with-harness
    (agent-repl-test-host--with-dir
      ;; Arrange
      (let ((dead (agent-repl-connect-open "127.0.0.1:61043"))
            (live (agent-repl-connect-open "127.0.0.1:58175"))
            (registers 0))
        (agent-repl-test-host--subscribe "ws-1" dead)
        (agent-repl-connect-close dead)
        (setq agent-repl-test-host--live live)
        (cl-letf (((symbol-function 'agent-repl-rpc-register-workspace)
                   (lambda (&rest _) (setq registers (1+ registers)))))
          ;; Act
          (agent-repl-host-select "ws-1")
          (agent-repl-host-mark-viewed "ws-1"))
        ;; Assert
        (should (= registers 1))))))

(provide 'test-host)

;;; test-host.el ends here

;;;; ---- agent-repl-host-conn-or-primary ----

(ert-deftest agent-repl-test-host-conn-or-primary-prefers-the-workspace-conn ()
  "A workspace with its own serving connection is addressed on it."
  (cl-letf (((symbol-function 'agent-repl-host-conn) (lambda (_ws) 'ws-conn))
            ((symbol-function 'agent-repl-link-primary) (lambda () 'primary)))
    (should (eq (agent-repl-host-conn-or-primary "ws-1") 'ws-conn))))

(ert-deftest agent-repl-test-host-conn-or-primary-falls-to-the-primary-link ()
  "A workspace with no serving connection yet is addressed on the primary link."
  (cl-letf (((symbol-function 'agent-repl-host-conn) (lambda (_ws) nil))
            ((symbol-function 'agent-repl-link-primary) (lambda () 'primary)))
    (should (eq (agent-repl-host-conn-or-primary "ws-1") 'primary))))

(ert-deftest agent-repl-test-host-conn-or-primary-addresses-no-workspace-on-the-primary ()
  "A nil workspace never asks for a workspace connection."
  (cl-letf (((symbol-function 'agent-repl-host-conn)
             (lambda (ws) (error "asked for the connection of %S" ws)))
            ((symbol-function 'agent-repl-link-primary) (lambda () 'primary)))
    (should (eq (agent-repl-host-conn-or-primary nil) 'primary))))

(ert-deftest agent-repl-test-host-conn-or-primary-is-nil-with-no-link ()
  "Nil when neither a workspace connection nor a primary link stands."
  (cl-letf (((symbol-function 'agent-repl-host-conn) (lambda (_ws) nil))
            ((symbol-function 'agent-repl-link-primary) (lambda () nil)))
    (should-not (agent-repl-host-conn-or-primary "ws-1"))))

(ert-deftest agent-repl-test-host-conn-or-primary-is-the-only-spelling ()
  "No source hand-rolls the workspace-conn-else-primary fallback.
Every per-workspace call resolves its connection through
`agent-repl-host-conn-or-primary', so a site that spells the fallback
itself is a second copy free to drift from it."
  (let ((pattern "(or (\\(?:and ws (\\)?agent-repl-host-conn ws))?[[:space:]\n]*(agent-repl-link-primary)")
        (offenders nil))
    (dolist (file (directory-files agent-repl-test--module-dir t "\\`[^.].*\\.el\\'"))
      (unless (string-prefix-p "test-" (file-name-nondirectory file))
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (while (re-search-forward pattern nil t)
            (unless (and (string= (file-name-nondirectory file) "host.el")
                         (save-excursion
                           (beginning-of-defun)
                           (looking-at "(defun agent-repl-host-conn-or-primary ")))
              (push (format "%s:%d" (file-name-nondirectory file) (line-number-at-pos))
                    offenders))))))
    (should-not offenders)))

;;;; ---- A switch request moves the frame at once ----

(defmacro agent-repl-test-host--moving (shown tabbed &rest body)
  "Run BODY with SHOWN on screen, the workspaces in TABBED having tabs, and
every frame move recorded in `moved'."
  (declare (indent 2))
  `(let ((moved nil))
     (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () ,shown))
               ((symbol-function 'agent-repl-open-tabs-holds-p) (lambda (_id) nil))
               ((symbol-function 'agent-repl--ws-by-ref-id)
                (lambda (id) (cl-find-if (lambda (ws) (equal (format "ws-id-%s" (substring ws 3)) id)) ,tabbed)))
               ((symbol-function 'agent-repl-roster-move-frame) (lambda (ws) (push ws moved)))
               ((symbol-function 'agent-repl-webhost-has-page-p) (lambda (_ws) t))
               ((symbol-function 'agent-repl-webhost-show) (lambda (ws) (push (list :shown ws) moved)))
               ((symbol-function 'agent-repl--frontend-fit-webviews) (lambda (&rest _) (push :fitted moved))))
       ,@body)))

(ert-deftest agent-repl-test-host-a-switch-request-moves-the-frame-at-once ()
  "The frame moves on the request, before the daemon answers: the roster's
follow came some 50ms later (2026-10-15 switch-bench)."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe-three)
    (agent-repl-test-host--moving "ws-1" '("ws-1" "ws-2")
      ;; Act
      (agent-repl-host-request-switch "ws-2" 'cycle)
      ;; Assert: the frame moves, the webview takes the size its window now
      ;; has, and only then is the page shown, so WebKit paints it once at
      ;; that size (2026-10-16: the page painted at the old height first).
      (should (equal (reverse moved) '("ws-2" :fitted (:shown "ws-2"))))
      (should (equal (agent-repl-test-host--selected-ids) '("ws-id-2"))))))

(ert-deftest agent-repl-test-host-a-switch-request-with-no-page-moves-the-frame-alone ()
  "A workspace whose page the host does not hold yet moves the frame; the
switch pass shows its page once it is made."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe-three)
    (agent-repl-test-host--moving "ws-1" '("ws-1" "ws-2")
      (cl-letf (((symbol-function 'agent-repl-webhost-has-page-p) (lambda (_ws) nil)))
        ;; Act
        (agent-repl-host-request-switch "ws-2" 'cycle)
        ;; Assert
        (should (equal (reverse moved) '("ws-2" :fitted)))
        (should (agent-repl-test-host--logged-p
                 :info "elisp.host.move-on-request ws=ws-2 decision=moved from=ws-1 page=none"))))))

(ert-deftest agent-repl-test-host-a-moved-request-records-its-timing-and-collections ()
  "The moved record carries the time since the request, the move's own time,
and the collections the move ran, so a slow switch names what spent it."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe-three)
    (agent-repl-test-host--moving "ws-1" '("ws-1" "ws-2")
      (cl-letf (((symbol-function 'agent-repl-roster-move-frame)
                 (lambda (ws) (push ws moved) (garbage-collect))))
        ;; Act
        (agent-repl-host-request-switch "ws-2" 'cycle)
        ;; Assert
        (should (seq-some
                 (lambda (entry)
                   (and (eq (car entry) :info)
                        (string-match
                         "decision=moved from=ws-1 page=shown since-request-ms=\\([0-9]+\\) move-ms=\\([0-9]+\\) gcs=\\([0-9]+\\) gc-ms=[0-9]+\\'"
                         (cdr entry))
                        (>= (string-to-number (match-string 1 (cdr entry)))
                            (string-to-number (match-string 2 (cdr entry))))
                        (>= (string-to-number (match-string 3 (cdr entry))) 1)))
                 agent-repl-test-host--logs))))))

(ert-deftest agent-repl-test-host-a-switch-request-moves-on-every-press ()
  "Every press moves the frame, though only the first select is on the wire."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe-three)
    (agent-repl-test-host--moving "ws-1" '("ws-1" "ws-2" "ws-3")
      ;; Act
      (agent-repl-host-request-switch "ws-2" 'cycle)
      (agent-repl-host-request-switch "ws-3" 'cycle)
      ;; Assert
      (should (equal (reverse moved) '("ws-2" :fitted (:shown "ws-2") "ws-3" :fitted (:shown "ws-3")))))))

(ert-deftest agent-repl-test-host-a-switch-request-to-a-workspace-with-no-tab-moves-nothing ()
  "A workspace with no tab yet is landed on by the roster's follow once its
tab opens, never here."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe-three)
    (agent-repl-test-host--moving "ws-1" '("ws-1")
      ;; Act
      (agent-repl-host-request-switch "ws-2" 'cycle)
      ;; Assert
      (should (null moved))
      (should (agent-repl-test-host--logged-p
               :info "elisp.host.move-on-request ws=ws-2 decision=no-tab-yet")))))

(ert-deftest agent-repl-test-host-a-switch-request-to-the-shown-workspace-moves-nothing ()
  "Asking for the workspace on screen moves nothing."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe-three)
    (agent-repl-test-host--moving "ws-2" '("ws-1" "ws-2")
      ;; Act
      (agent-repl-host-request-switch "ws-2" 'cycle)
      ;; Assert
      (should (null moved)))))


;;;; ---- An answered selection awaits the roster that carries it ----

(ert-deftest agent-repl-test-host-an-answered-selection-stays-pending-until-its-roster ()
  "The answer names the roster revision the selection is in; until Emacs
holds that roster the selection stays pending, so a roster from before it
is deferred rather than followed back (2026-10-16)."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe-three)
    (agent-repl-host-select "ws-2")
    ;; Act
    (agent-repl-test-host--answer-held '(:arm :success :value (:roster-revision 7)))
    ;; Assert
    (should (equal agent-repl-host--confirming '("ws-2" . 7)))
    (should (equal (agent-repl-host-pending-selection) "ws-2"))))

(ert-deftest agent-repl-test-host-release-confirmed-waits-for-the-revision ()
  "A roster before the answered revision keeps the selection pending; one
at it or after releases it."
  (dolist (case '((6 . ("ws-2" . 7)) (7 . nil) (9 . nil)))
    (agent-repl-test-host--with-harness
      ;; Arrange
      (setq agent-repl-host--confirming (cons "ws-2" 7))
      ;; Act
      (agent-repl-host-release-confirmed (car case))
      ;; Assert
      (should (equal agent-repl-host--confirming (cdr case))))))

(ert-deftest agent-repl-test-host-forget-confirming-ends-the-wait-and-says-why ()
  "A closed roster stream ends the wait: the next daemon's revisions start
afresh."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (setq agent-repl-host--confirming (cons "ws-2" 7))
    ;; Act
    (agent-repl-host-forget-confirming "the roster stream closed")
    ;; Assert
    (should (null agent-repl-host--confirming))
    (should (agent-repl-test-host--logged-p
             :info "elisp.host.select-unconfirmed ws=ws-2 awaited=7 reason=the roster stream closed"))))

(ert-deftest agent-repl-test-host-standing-ask-is-read-off-the-push ()
  "The standing ask is the last push's `ask', nil when none stands."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    (let ((ask '(:row (:value "row-p") :kind (:arm :permission :value (:standing-offered nil)))))
      (agent-repl-test-host--push
       "ws-1" (list :arm :host :value (append (agent-repl-test-host--live) (list :ask ask))))
      ;; Act / Assert
      (should (equal (agent-repl-host-standing-ask "ws-1") ask)))
    (agent-repl-test-host--push "ws-1" (list :arm :host :value (agent-repl-test-host--live)))
    (should-not (agent-repl-host-standing-ask "ws-1"))))

(ert-deftest agent-repl-test-host-standing-ask-is-nil-before-any-push ()
  "Before the first push no ask is known to stand."
  (agent-repl-test-host--with-harness
    (agent-repl-test-host--subscribe "ws-1")
    (should-not (agent-repl-host-standing-ask "ws-1"))))
