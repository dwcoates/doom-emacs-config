;;; test-frontend-client.el --- ERT tests for frontend-client.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the daemon session client.  Emacs speaks no HTTP to the
;; daemon: session CRUD travels as UDS commands and every read comes off
;; pushed frames, so the boundaries shadowed here are
;; `agent-repl--uds-send-command' / `--uds-connected-p' — no real socket
;; ever opens.  The send is also where a command's ack callbacks are
;; registered, so shadowing it shadows tracking too.
;;
;; Run with:
;;   emacs -batch -Q -l ert -l test-frontend-client.el -f ert-run-tests-batch-and-exit

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

(require 'cl-lib)

;;;; ---- UDS command capture helpers -------------------------------------------
;;
;; The session-CRUD/prompt/interrupt paths were migrated off HTTP onto the
;; frontend.v1 UDS command channel (S7).  These shadow that boundary
;; (`agent-repl--uds-send-command') so no real socket
;; fires; commands accumulate in the anaphoric `uds-commands' as
;; \(FIELD PAYLOAD WORKSPACE) lists, newest last.

(defmacro agent-repl-test--with-uds (&rest body)
  "Run BODY with the UDS command boundary shadowed (capture only).
Suitable for submitPrompt/interrupt/deleteSession, which do not await."
  (declare (indent 0))
  `(let ((uds-commands '()) (uds-counter 0))
     (ignore uds-commands)
     (cl-letf (((symbol-function 'agent-repl--uds-send-command)
                (lambda (field payload &optional workspace &rest _)
                  (setq uds-commands
                        (append uds-commands (list (list field payload workspace))))
                  (format "req-%d" (cl-incf uds-counter))))
               )
       ,@body)))

(defmacro agent-repl-test--with-ws (ws plist &rest body)
  "Register workspace WS with PLIST for BODY in isolated workspace state."
  (declare (indent 2))
  `(agent-repl-test--with-clean-state
     (let ((agent-repl-test--plist (copy-sequence ,plist)))
       (while agent-repl-test--plist
         (agent-repl--ws-put ,ws (pop agent-repl-test--plist)
                             (pop agent-repl-test--plist))))
     ,@body))

(defmacro agent-repl-test--with-views (views &rest body)
  "Clear the SessionView store, install VIEWS, then run BODY."
  (declare (indent 1))
  `(progn
     (clrhash agent-repl--frontend-session-views)
     (dolist (v ,views) (agent-repl--frontend-store-session-view v))
     ,@body))

(defmacro agent-repl-test--with-interrupt-acks (answer &rest body)
  "Run BODY with interrupt commands captured and confirmation returning ANSWER."
  (declare (indent 1))
  `(let ((uds-commands '()) (uds-counter 0) (asked '()) (echoed nil))
     (ignore uds-commands asked echoed)
     (clrhash agent-repl--uds-pending-commands)
     (cl-letf (;; Shadows ONLY the socket write: the stub still registers the
               ;; pending command the way the real send does, so the ack
               ;; handler under test matches a genuinely tracked request.
               ((symbol-function 'agent-repl--uds-send-command)
                (lambda (field payload &optional workspace _process &rest keys)
                  (setq uds-commands
                        (append uds-commands (list (list field payload workspace))))
                  (let ((request-id (format "req-%d" (cl-incf uds-counter))))
                    (agent-repl--uds-register-pending-command
                     request-id field workspace
                     (plist-get keys :on-failure)
                     (plist-get keys :on-success)
                     (plist-get keys :on-challenge))
                    (when-let ((registered (plist-get keys :on-registered)))
                      (funcall registered request-id))
                    request-id)))
               ((symbol-function 'agent-repl--uds-run-timer) (lambda (&rest _) 'timer))
               ((symbol-function 'y-or-n-p)
                (lambda (question)
                  (setq asked (append asked (list question)))
                  ,answer))
               ((symbol-function 'message)
                (lambda (fmt &rest args) (setq echoed (apply #'format fmt args)))))
       ,@body)))

;;;; ---- asynchronous client continuations -----------------------------------

(ert-deftest agent-repl-test-frontend-after-ready-returns-before-its-callback ()
  "Readiness schedules its retry and returns before a later ready callback."
  (let (tick events)
    (cl-letf (((symbol-function 'agent-repl--frontend-daemon-ready-p) (lambda () nil))
              ((symbol-function 'agent-repl--uds-connected-p) (lambda () t))
              ((symbol-function 'agent-repl--uds-run-timer)
               (lambda (_delay fn) (setq tick fn) 'timer)))
      (should (eq :pending (agent-repl--frontend-after-ready
                            (lambda () (push 'ready events))
                            (lambda (_detail) (push 'failed events)))))
      (should-not events)
      (cl-letf (((symbol-function 'agent-repl--frontend-daemon-ready-p) (lambda () t)))
        (funcall tick))
      (should (equal events '(ready))))))

(ert-deftest agent-repl-test-frontend-after-ready-times-out-on-its-owned-timer ()
  "Readiness invokes only failure after its bounded dial budget is exhausted."
  (let ((agent-repl-frontend-ready-attempts 1) ticks events)
    (cl-letf (((symbol-function 'agent-repl--frontend-daemon-ready-p) (lambda () nil))
              ((symbol-function 'agent-repl--uds-connected-p) (lambda () t))
              ((symbol-function 'agent-repl--uds-run-timer)
               (lambda (_delay fn) (push fn ticks) 'timer)))
      (agent-repl--frontend-after-ready
       (lambda () (push 'ready events)) (lambda (_detail) (push 'failed events)))
      (funcall (pop ticks))
      (should (equal events '(failed))))))

(ert-deftest agent-repl-test-frontend-after-open-workspace-preserves-continuation-order ()
  "openWorkspace returns before its ack and completes only through success."
  (let (success failure ack)
    (agent-repl-test--with-ws "ws1" '(:project-dir "/w")
      (cl-letf (((symbol-function 'agent-repl--frontend-after-ready) (lambda (ok _fail &optional _ws) (funcall ok) :ready))
                ((symbol-function 'agent-repl--uds-send-command)
                 (agent-repl-test--send-command-stub
                  "open-1" (lambda (c) (setq ack (plist-get c :on-success)))))
                ((symbol-function 'agent-repl--uds-run-timer) (lambda (&rest _) 'timer)))
        (should (eq :pending (agent-repl--frontend-after-open-workspace
                              "ws1" (lambda () (setq success t)) (lambda (_e) (setq failure t)))))
        (should-not success)
        (should-not failure)
        (funcall ack)
        (should success)
        (should-not failure)))))

(ert-deftest agent-repl-test-frontend-after-create-awaits-ack-and-session-view ()
  "createSession completes only after its ack and matching pushed SessionView."
  (let (success failure ack poll)
    (clrhash agent-repl--frontend-session-views)
    (cl-letf (((symbol-function 'agent-repl--frontend-after-ready) (lambda (ok _fail &optional _ws) (funcall ok) :ready))
              ((symbol-function 'agent-repl--uds-send-command)
               (agent-repl-test--send-command-stub
                "create-1" (lambda (c) (setq ack (plist-get c :on-success)))))
              ((symbol-function 'agent-repl--uds-run-timer)
               (lambda (delay fn) (when (= delay 0.05) (setq poll fn)) 'timer)))
      (should (eq :pending (agent-repl--frontend-after-create-session
                            "/w" nil 'continue nil
                            (lambda (id) (setq success id))
                            (lambda (e) (setq failure e)))))
      (should-not success)
      (funcall ack)
      (should-not success)
      (agent-repl--frontend-store-session-view '(:sessionId "s-new" :workspace "/w"))
      (funcall poll)
      (should (equal success "s-new"))
      (should-not failure))))

(ert-deftest agent-repl-test-frontend-after-create-canonicalizes-every-routing-key ()
  "createSession and its bookkeeping share one canonical cwd string."
  (let (sent ack poll canonicalized)
    (clrhash agent-repl--frontend-session-views)
    (remhash "/w" agent-repl--frontend-creates-in-flight)
    (cl-letf (((symbol-function 'agent-repl--path-canonical)
              (lambda (path)
                 (push path canonicalized)
                 "/w"))
              ((symbol-function 'agent-repl--frontend-after-ready)
               (lambda (ok _fail &optional _ws) (funcall ok) :ready))
              ((symbol-function 'agent-repl--uds-send-command)
               (lambda (&rest args)
                 (setq sent args)
                 (funcall (plist-get (nthcdr 4 args) :on-registered) "create-canonical")
                 (setq ack (plist-get (nthcdr 4 args) :on-success))
                 "create-canonical"))
              ((symbol-function 'agent-repl--uds-run-timer)
               (lambda (delay fn) (when (= delay 0.05) (setq poll fn)) 'timer)))
      (agent-repl--frontend-after-create-session
       "/w/" nil 'continue nil #'ignore #'error)
      (should (equal (car sent) "createSession"))
      (should (equal (plist-get (cadr sent) :cwd) "/w"))
      (should (equal (nth 2 sent) "/w"))
      (setq canonicalized (nreverse canonicalized))
      (should (equal (car canonicalized) "/w/"))
      (should (cl-every (lambda (path) (equal path "/w")) (cdr canonicalized)))
      (should (gethash "/w" agent-repl--frontend-creates-in-flight))
      (should-not (gethash "/w/" agent-repl--frontend-creates-in-flight))
      (funcall ack)
      (agent-repl--frontend-store-session-view '(:sessionId "s-new" :workspace "/w"))
      (funcall poll)
      (should-not (gethash "/w" agent-repl--frontend-creates-in-flight)))))

(ert-deftest agent-repl-test-frontend-after-create-readiness-failure-cleans-reservation ()
  "A readiness failure removes the cwd reservation before reporting failure."
  (let (failure)
    (remhash "/w-fail" agent-repl--frontend-creates-in-flight)
    (cl-letf (((symbol-function 'agent-repl--frontend-after-ready)
               (lambda (_ok fail &optional _ws)
                 (funcall fail "no daemon")
                 :pending)))
      (agent-repl--frontend-after-create-session
       "/w-fail" nil 'continue nil #'ignore
       (lambda (detail) (setq failure detail)))
      (should (equal failure "no daemon"))
      (should-not (gethash "/w-fail" agent-repl--frontend-creates-in-flight)))))

(ert-deftest agent-repl-test-frontend-after-health-rejection-cleans-up-and-fails ()
  "A rejected health command invokes failure and retires both registrations."
  (let (failure untracked-command untracked-health rejection)
    (cl-letf (((symbol-function 'agent-repl--frontend-after-ready) (lambda (ok _fail &optional _ws) (funcall ok) :ready))
              ((symbol-function 'agent-repl--uds-send-command)
               (agent-repl-test--send-command-stub
                "health-1" (lambda (c) (setq rejection (plist-get c :on-failure)))))
              ((symbol-function 'agent-repl--uds-track-health-response) (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--uds-run-timer) (lambda (&rest _) 'timer))
              ((symbol-function 'agent-repl--uds-untrack-command) (lambda (&rest args) (setq untracked-command args)))
              ((symbol-function 'agent-repl--uds-untrack-health-response) (lambda (&rest args) (setq untracked-health args))))
      (agent-repl--frontend-after-health-command "daemonHealth" nil nil nil "daemon"
                                                  (lambda () (error "unexpected success"))
                                                  (lambda (detail) (setq failure detail)))
      (funcall rejection "not ready")
      (should (string-match-p "rejected" failure))
      (should (equal untracked-command '("health-1" nil "health-settled")))
      (should (equal untracked-health '("health-1" nil "health-settled"))))))

(ert-deftest agent-repl-test-frontend-after-health-success-continuation-failure-is-terminal ()
  "A healthy response still reports a failing continuation to its owner."
  (let (failure response-callback)
    (cl-letf (((symbol-function 'agent-repl--frontend-after-ready)
               (lambda (ok _fail &optional _ws) (funcall ok) :ready))
              ((symbol-function 'agent-repl--uds-send-command)
               (agent-repl-test--send-command-stub "health-2"))
              ((symbol-function 'agent-repl--uds-track-health-response)
               (lambda (_id _field _ws _session-id callback)
                 (setq response-callback callback)))
              ((symbol-function 'agent-repl--uds-run-timer)
               (lambda (&rest _) 'timer))
              ((symbol-function 'agent-repl--uds-untrack-command) #'ignore)
              ((symbol-function 'agent-repl--uds-untrack-health-response) #'ignore))
      (agent-repl--frontend-after-health-command
       "daemonHealth" nil nil nil "daemon"
       (lambda () (error "rebind invariant"))
       (lambda (detail) (setq failure detail)))
      (funcall response-callback '(:healthy t))
      (should (equal failure
                     "healthy continuation failed: rebind invariant")))))

(ert-deftest agent-repl-test-frontend-after-ensure-session-reuses-a-live-binding ()
  "ensure-session reopens the workspace, then reports establishment.
The continuation takes no arguments: what the workspace's session IS
belongs to the daemon."
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w")
    (let (success failure)
      (cl-letf (((symbol-function 'agent-repl--frontend-after-daemon-ensured)
                 (lambda (ok _fail &optional _force) (funcall ok) :pending))
                ((symbol-function 'agent-repl--frontend-after-ready) (lambda (ok _fail &optional _ws) (funcall ok) :ready))
                ((symbol-function 'agent-repl--frontend-workspace-session-live-p) (lambda (_key) t))
                ((symbol-function 'agent-repl--frontend-after-open-workspace)
                 (lambda (_ws ok _fail) (funcall ok) :pending)))
        (should (eq :pending (agent-repl--frontend-after-ensure-session
                              "ws1" (lambda () (setq success t))
                              (lambda (detail) (setq failure detail)))))
        (should success)
        (should-not failure)))))

(ert-deftest agent-repl-test-frontend-after-ensure-never-creates-a-session ()
  "Establishment is `openWorkspace' and nothing else.
Emacs creating on its own is the duplicate-session generator: it asked
whether a session was live, got a stale answer off a durable record, and
minted a rival for a workspace that already had one.  The daemon owns the
question and creates on open when there is nothing to reattach to."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w")
    (let (opened created)
      (cl-letf (((symbol-function 'agent-repl--frontend-after-daemon-ensured)
                 (lambda (ok _fail &optional _force) (funcall ok) :pending))
                ((symbol-function 'agent-repl--frontend-after-ready)
                 (lambda (ok _fail &optional _ws) (funcall ok) :ready))
                ((symbol-function 'agent-repl--frontend-after-open-workspace)
                 (lambda (ws ok _fail) (setq opened ws) (funcall ok) :pending))
                ((symbol-function 'agent-repl--frontend-after-create-session)
                 (lambda (&rest _) (setq created t) :pending)))
        ;; Act — the workspace has no live session, which is exactly the case
        ;; that used to create one here.
        (agent-repl--frontend-after-ensure-session "ws1" #'ignore #'error)
        ;; Assert
        (should (equal opened "ws1"))
        (should-not created)))))

(ert-deftest agent-repl-test-frontend-open-workspace-carries-the-run-preferences ()
  "openWorkspace sends the posture a session the daemon starts runs under.
It is the SAME posture an explicit create sends, so the two entry points
cannot offer the daemon different answers for one workspace."
  ;; Arrange
  (cl-letf (((symbol-function 'agent-repl--frontend-session-posture)
             (lambda (_dir) '(:config-dir "/cfg" :permission-mode "acceptEdits"
                              :allow-ungated t))))
    ;; Act
    (let ((payload (agent-repl--frontend-open-workspace-payload "/w")))
      ;; Assert
      (should (equal (plist-get payload :configDir) "/cfg"))
      (should (equal (plist-get payload :permissionMode) "acceptEdits"))
      (should (eq (plist-get payload :allowUngated) t))
      ;; No session identity: which session a workspace owns is the daemon's.
      (should-not (plist-member payload :sessionId)))))

(ert-deftest agent-repl-test-frontend-after-ensure-send-skips-presentation-gate ()
  "Send-purpose ensure reports establishment without opening the workspace."
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w")
    (let (success opened)
      (cl-letf (((symbol-function 'agent-repl--frontend-after-daemon-ensured)
                 (lambda (ok _fail &optional _force) (funcall ok) :pending))
                ((symbol-function 'agent-repl--frontend-after-ready)
                 (lambda (ok _fail &optional _ws) (funcall ok) :ready))
                ((symbol-function 'agent-repl--frontend-workspace-session-live-p) (lambda (_key) t))
                ((symbol-function 'agent-repl--frontend-after-open-workspace)
                 (lambda (&rest _) (setq opened t))))
        (agent-repl--frontend-after-ensure-session
         "ws1" (lambda () (setq success t)) #'error 'send)
        (should success)
        (should-not opened)))))

(ert-deftest agent-repl-test-frontend-async-send-mutates-only-after-ensure-success ()
  "Prompt dispatch and origin clearing follow ensure success."
  (agent-repl-test--with-ws "ws1"
      '(:project-dir "/w" :next-send-origin merge)
    (let (ensure-success sent request)
      (cl-letf (((symbol-function 'agent-repl--frontend-after-ensure-session)
                 (lambda (_ws ok _fail &optional purpose)
                   (should (eq purpose 'send))
                   (setq ensure-success ok)
                   :pending))
                ((symbol-function 'agent-repl--uds-send-command)
                 (lambda (field payload key)
                   (setq sent (list field payload key)) "req-1"))
                )
        (agent-repl--frontend-send-user-message
         "ws1" "hello" "PROMPT_ORIGIN_USER_SENT" (lambda (id) (setq request id)) #'error)
        (should-not sent)
        (should (eq (agent-repl--ws-get "ws1" :next-send-origin) 'merge))
        (funcall ensure-success)
        (should (equal sent '("submitPrompt" (:text "hello" :promptOrigin "PROMPT_ORIGIN_USER_SENT") "/w")))
        (should (equal request "req-1"))
        (should-not (agent-repl--ws-get "ws1" :next-send-origin))))))

(ert-deftest agent-repl-test-gui-send-turn-does-not-mark-thinking-on-ensure-failure ()
  "A failed asynchronous ensure leaves all sent-turn state untouched."
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w")
    (let (failure settled)
      (cl-letf (((symbol-function 'agent-repl--frontend-send-user-message)
                 (lambda (_ws _text _origin _ok fail) (setq failure fail) :pending))
                ((symbol-function 'agent-repl--frontend-snap-webview-to-tail)
                 (lambda (&rest _) (error "must not present"))))
        (agent-repl--gui-send-turn "ws1" "prepared" "raw" "PROMPT_ORIGIN_USER_SENT"
                                   (lambda () (setq settled t)))
        (funcall failure "no daemon")
        (should settled)
        (should-not (agent-repl--ws-get "ws1" :sent-turn))
        (should-not (agent-repl--ws-get "ws1" :thinking))))))

;;;; ---- webview URL ---------------------------------------------------------

(ert-deftest agent-repl-test-frontend-resume-mode-fresh-is-not-expressible ()
  "There is no fresh resume mode to send.
The proto retired RESUME_MODE_FRESH, so a caller reaching for it must hit
the wire-map's own refusal rather than silently landing on `continue' —
which would resume a conversation the caller believed it was replacing."
  ;; Arrange / Act / Assert
  (should-error (agent-repl--frontend-resume-mode-wire 'fresh)))

(ert-deftest agent-repl-test-frontend-base-url-is-the-webview-address ()
  "The one surviving URL builder addresses the daemon's served webapp.
Emacs itself issues no HTTP; this URL is handed to the embedded browser."
  ;; Arrange
  (let ((agent-repl-frontend-daemon-addr "127.0.0.1:9999"))
    ;; Act / Assert
    (should (equal (agent-repl--frontend-base-url) "http://127.0.0.1:9999"))))

;;;; ---- create (UDS `createSession') -----------------------------------------

(defmacro agent-repl-test--created-payload ()
  "Return the payload plist of the single captured `createSession' command.
A macro (not a defun) so it captures the lexical `uds-commands' at the call
site inside `agent-repl-test--with-uds-create'."
  '(nth 1 (car uds-commands)))

(ert-deftest agent-repl-test-frontend-ungated-mode-p-flags-bypass-permissions ()
  "`bypassPermissions' is the mode under which no permission gate exists."
  ;; Arrange + Act + Assert
  (should (agent-repl-frontend-ungated-permission-mode-p "bypassPermissions")))

(ert-deftest agent-repl-test-frontend-ungated-mode-p-clears-dont-ask ()
  "`dontAsk' bypasses canUseTool fail-CLOSED, so it is not ungated."
  ;; Arrange + Act + Assert
  (should-not (agent-repl-frontend-ungated-permission-mode-p "dontAsk")))

(ert-deftest agent-repl-test-frontend-ungated-mode-p-clears-auto ()
  "`auto' still reaches canUseTool for the ask path."
  ;; Arrange + Act + Assert
  (should-not (agent-repl-frontend-ungated-permission-mode-p "auto")))

(ert-deftest agent-repl-test-frontend-ungated-mode-p-clears-nil ()
  "A nil mode is not a claim of ungatedness."
  ;; Arrange + Act + Assert
  (should-not (agent-repl-frontend-ungated-permission-mode-p nil)))

(ert-deftest agent-repl-test-frontend-session-posture-is-shared-explicit-state ()
  "The shared posture helper returns account, mode, and deliberate consent."
  (let ((agent-repl-frontend-permission-mode "bypassPermissions")
        (agent-repl-frontend-allow-ungated t))
    (cl-letf (((symbol-function 'agent-repl--compute-config-dir)
               (lambda (cwd)
                 (should (equal cwd "/work"))
                 "/account"))
              ((symbol-function 'agent-repl--ws-name-for-dir)
               (lambda (_cwd) "ws")))
      (should
       (equal (agent-repl--frontend-session-posture "/work")
              '(:config-dir "/account"
                :permission-mode "bypassPermissions"
                :allow-ungated t))))))

(ert-deftest agent-repl-test-frontend-create-timeout-outlives-the-daemon-bound ()
  "The client waits longer than the daemon's own establishment bound.
Whichever bound fires first is the one the user reads, and only the daemon's
nack can name the link that is still pending."
  ;; Arrange / Act / Assert
  (should (> agent-repl-frontend-create-timeout 20)))

;;;; ---- session health as a diagnostic ----------------------------------------

(ert-deftest agent-repl-test-frontend-session-live-p-true-for-listed ()
  "A workspace whose stored SessionView is non-terminal is live."
  ;; Arrange
  (agent-repl-test--with-views '((:sessionId "s_1" :workspace "/w"))
    ;; Act / Assert
    (should (agent-repl--frontend-workspace-session-live-p "/w"))))

(ert-deftest agent-repl-test-frontend-session-live-p-nil-for-terminal ()
  "A workspace whose stored SessionView is terminal is not live."
  ;; Arrange
  (agent-repl-test--with-views '((:sessionId "s_1" :workspace "/w" :terminal t))
    ;; Act / Assert
    (should-not (agent-repl--frontend-workspace-session-live-p "/w"))))

(ert-deftest agent-repl-test-frontend-session-live-p-nil-for-unlisted ()
  "A workspace with no stored SessionView is not live."
  ;; Arrange
  (agent-repl-test--with-views '()
    ;; Act / Assert
    (should-not (agent-repl--frontend-workspace-session-live-p "/ghost"))))

;;;; ---- wait-ready -------------------------------------------------------------

(defmacro agent-repl-test--with-readiness (connected-fn &rest body)
  "Run BODY with the UDS readiness inputs shadowed.
CONNECTED-FN is the nullary stand-in for `agent-repl--uds-connected-p'.
`agent-repl-uds-connect' dials are counted into the anaphoric `dials',
`accept-process-output' pumps into `pumps', and blocking `sleep-for'
waits into `sleeps' (newest last).  The DaemonView store starts EMPTY so
each test drives it explicitly."
  (declare (indent 1))
  `(let ((dials 0) (pumps 0) (sleeps '())
         (agent-repl--frontend-last-daemon-view nil))
     (ignore dials pumps sleeps)
     (cl-letf (((symbol-function 'agent-repl--uds-connected-p) ,connected-fn)
               ((symbol-function 'agent-repl-uds-connect)
                (lambda (&optional _p _readiness-p) (cl-incf dials) nil))
               ((symbol-function 'accept-process-output)
                (lambda (&rest _) (cl-incf pumps) nil))
               ((symbol-function 'sleep-for)
                (lambda (secs) (setq sleeps (append sleeps (list secs))) nil)))
       ,@body)))

(ert-deftest agent-repl-test-frontend-ready-p-false-without-a-daemon-view ()
  "A live link with no pushed `DaemonView' is NOT ready.
The connection can be accepted a beat before the snapshot is composed."
  ;; Arrange
  (agent-repl-test--with-readiness (lambda () t)
    ;; Act / Assert
    (should-not (agent-repl--frontend-daemon-ready-p))))

(ert-deftest agent-repl-test-frontend-ready-p-false-when-link-is-down ()
  "A stored `DaemonView' with a DEAD link is NOT ready.
The view outlives the connection that delivered it, so it describes a
daemon that may be gone."
  ;; Arrange
  (agent-repl-test--with-readiness (lambda () nil)
    (setq agent-repl--frontend-last-daemon-view '(:bootId "b_1"))
    ;; Act / Assert
    (should-not (agent-repl--frontend-daemon-ready-p))))

(ert-deftest agent-repl-test-frontend-ready-p-true-with-link-and-view ()
  "A live link plus a pushed `DaemonView' is ready."
  ;; Arrange
  (agent-repl-test--with-readiness (lambda () t)
    (setq agent-repl--frontend-last-daemon-view '(:bootId "b_1"))
    ;; Act / Assert
    (should (agent-repl--frontend-daemon-ready-p))))

(ert-deftest agent-repl-test-gui-running-p-tracks-the-pushed-view ()
  "The gui liveness capability reads the daemon's pushed view for the workspace."
  ;; Arrange
  (agent-repl-test--with-views '((:sessionId "s_1" :workspace "/w1")
                                 (:sessionId "s_2" :workspace "/w2" :terminal t))
    ;; Act / Assert
    (agent-repl-test--with-ws "ws1" '(:project-dir "/w1")
      (should (agent-repl--gui-running-p "ws1")))
    (agent-repl-test--with-ws "ws2" '(:project-dir "/w2")
      (should-not (agent-repl--gui-running-p "ws2")))
    (agent-repl-test--with-ws "ws3" '(:project-dir "/unknown")
      (should-not (agent-repl--gui-running-p "ws3")))
    ;; A workspace with no project dir has no wire key to ask about.
    (agent-repl-test--with-ws "ws4" '()
      (should-not (agent-repl--gui-running-p "ws4")))))

(ert-deftest agent-repl-test-frontend-turn-active-sessions-extracts-busy-ids ()
  "An active workspace with a live session is reported busy, by path."
  (let ((agent-repl--frontend-workspace-state-views
         (make-hash-table :test 'equal)))
    (puthash "/w1"
             '(:workspace "/w1" :sessionId "vendor-uuid" :turnActive t)
             agent-repl--frontend-workspace-state-views)
    (puthash "/w2"
             '(:workspace "/w2" :sessionId "vendor-idle" :turnActive nil)
             agent-repl--frontend-workspace-state-views)
    (agent-repl-test--with-views '((:sessionId "s_busy" :workspace "/w1")
                                   (:sessionId "s_idle" :workspace "/w2"))
      (should (equal (agent-repl--frontend-turn-active-sessions) '("/w1"))))))

(ert-deftest agent-repl-test-frontend-turn-active-sessions-skips-terminal ()
  "A terminal session is never counted busy for its active workspace."
  (let ((agent-repl--frontend-workspace-state-views
         (make-hash-table :test 'equal)))
    (puthash "/w1" '(:workspace "/w1" :turnActive t)
             agent-repl--frontend-workspace-state-views)
    (puthash "/w2" '(:workspace "/w2" :turnActive t)
             agent-repl--frontend-workspace-state-views)
    (agent-repl-test--with-views '((:sessionId "s_zombie" :workspace "/w1" :terminal t)
                                   (:sessionId "s_live" :workspace "/w2"))
      (should (equal (agent-repl--frontend-turn-active-sessions) '("/w2"))))))

(ert-deftest agent-repl-test-frontend-turn-active-sessions-ignores-stale-local-binding ()
  "A stale local binding cannot turn an idle authoritative workspace busy."
  (let ((agent-repl--frontend-workspace-state-views
         (make-hash-table :test 'equal)))
    (puthash "/w1" '(:workspace "/w1" :turnActive nil)
             agent-repl--frontend-workspace-state-views)
    (agent-repl-test--with-views '((:sessionId "s_current" :workspace "/w1"))
      (agent-repl-test--with-ws "ws1"
          '(:project-dir "/w1"
            :pushed-render-state-meta (:turn-active t))
        (should-not (agent-repl--frontend-turn-active-sessions))))))

(ert-deftest agent-repl-test-frontend-turn-active-sessions-nil-when-none-active ()
  "No authoritative workspace reporting turn-active means nothing to protect."
  (let ((agent-repl--frontend-workspace-state-views
         (make-hash-table :test 'equal)))
    (puthash "/w1" '(:workspace "/w1" :turnActive nil)
             agent-repl--frontend-workspace-state-views)
    (agent-repl-test--with-views '((:sessionId "s_1" :workspace "/w1"))
      (should-not (agent-repl--frontend-turn-active-sessions)))))

(ert-deftest agent-repl-test-frontend-all-turn-active-includes-unrestored-workspace ()
  "Startup safety sees an active daemon path before Emacs restores its workspace."
  (let ((agent-repl--frontend-workspace-state-views
         (make-hash-table :test 'equal)))
    (puthash "/unrestored"
             '(:workspace "/unrestored" :sessionId "vendor-uuid" :turnActive t)
             agent-repl--frontend-workspace-state-views)
    (agent-repl-test--with-views
        '((:sessionId "s_busy" :workspace "/unrestored"))
      (should (equal (agent-repl--frontend-all-turn-active-workspaces)
                     '("/unrestored"))))))

(ert-deftest agent-repl-test-frontend-all-turn-active-skips-terminal-session ()
  "A terminal session cannot block the coordinated startup restart."
  (let ((agent-repl--frontend-workspace-state-views
         (make-hash-table :test 'equal)))
    (puthash "/old"
             '(:workspace "/old" :sessionId "vendor-uuid" :turnActive t)
             agent-repl--frontend-workspace-state-views)
    (agent-repl-test--with-views
        '((:sessionId "s_dead" :workspace "/old" :terminal t))
      (should-not (agent-repl--frontend-all-turn-active-workspaces)))))

;;;; ---- reattach loop -----------------------------------------------------------

(ert-deftest agent-repl-test-frontend-reattach-check-ensures-every-live-workspace ()
  "The sweep ensures every live workspace, whatever the roster says.
There is one question — is this workspace wired to the daemon answering
right now — and the ensure both asks it and acts on it."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w1")
    (agent-repl-test--with-ws "ws2" '(:project-dir "/w2")
      (agent-repl-test--with-views '((:sessionId "s_1" :workspace "/w1"))
        (let ((ensured nil))
          (cl-letf (((symbol-function 'agent-repl--uds-connected-p) (lambda () t))
                    ((symbol-function 'agent-repl--live-ws-names)
                     (lambda () '("ws1" "ws2")))
                    ((symbol-function 'agent-repl--frontend-ensure-workspace)
                     (lambda (ws) (push ws ensured))))
            ;; Act
            (agent-repl--frontend-reattach-check)
            ;; Assert — the workspace with a live view and the one without are
            ;; both ensured; the ensure's own skip reason decides what to send.
            (should (equal (sort ensured #'string<) '("ws1" "ws2")))))))))

(ert-deftest agent-repl-test-frontend-reattach-check-names-no-session ()
  "The sweep reads no session identity at all.
It once compared a recorded id against the pushed roster to decide
between reattaching and ensuring; which session a workspace owns is the
daemon's ruling, and asking it here is what let a stale local answer mint
a rival session."
  ;; Arrange / Act / Assert
  (let ((source (with-temp-buffer
                  (insert-file-contents
                   (expand-file-name "frontend-client.el" agent-repl-test--module-dir))
                  (buffer-string))))
    (should-not (string-match-p ":frontend-session-id" source))))

(ert-deftest agent-repl-test-frontend-reattach-check-ensures-daemon-when-link-down ()
  "A down UDS link with live workspaces triggers a daemon ensure."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w")
    (let ((ensured nil))
      (cl-letf (((symbol-function 'agent-repl--uds-connected-p) (lambda () nil))
                ((symbol-function 'agent-repl--frontend-after-daemon-ensured)
                 (lambda (_ok _fail &optional _f) (setq ensured t))))
        ;; Act
        (agent-repl--frontend-reattach-check)
        ;; Assert
        (should ensured)))))

;;;; ---- the sweep is the RECOVERY sweep, not only the reattach sweep ------
;;
;; A binding the roster still lists says nothing about whether THIS daemon
;; instance is driving it: `live' and `backfill' are read back off the
;; durable registry record and survive a bounce.  The sweep used to stop at
;; "the binding is still listed", so a workspace the new instance had never
;; brought up recovered only when the user switched to it.

(ert-deftest agent-repl-test-frontend-reattach-check-ensures-a-retained-binding ()
  "A binding the roster still lists is ENSURED, not merely retained.
The bounce case: the record is durable and still listed, but no session
controller is attached, so the workspace needs bringing up."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w")
    (agent-repl-test--with-views '((:sessionId "s_1" :workspace "/w"))
      (let ((ensured nil))
        (cl-letf (((symbol-function 'agent-repl--uds-connected-p) (lambda () t))
                  ((symbol-function 'agent-repl--frontend-ensure-workspace)
                   (lambda (ws) (push ws ensured))))
          ;; Act
          (agent-repl--frontend-reattach-check)
          ;; Assert
          (should (equal ensured '("ws1"))))))))

(ert-deftest agent-repl-test-frontend-reattach-check-ensures-an-unbound-workspace ()
  "A workspace with NO binding is ensured rather than passed over.
It has no stale binding to notice, which is exactly why the sweep used to
leave its bring-up waiting for a switch."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w")
    (agent-repl-test--with-views '()
      (let ((ensured nil))
        (cl-letf (((symbol-function 'agent-repl--uds-connected-p) (lambda () t))
                  ((symbol-function 'agent-repl--frontend-ensure-workspace)
                   (lambda (ws) (push ws ensured))))
          ;; Act
          (agent-repl--frontend-reattach-check)
          ;; Assert
          (should (equal ensured '("ws1"))))))))

;;;; ---- recovery is driven by the RECONNECT, not by a switch --------------

(ert-deftest agent-repl-test-frontend-recovery-is-subscribed-to-the-snapshot ()
  "The recovery runs off the snapshot-applied edge, not off socket-open.
A socket with no state behind it has not finished reconnecting, and the
roster the sweep reads there is still empty."
  ;; Act / Assert
  (should (memq #'agent-repl--frontend-recover-after-reconnect
                agent-repl-uds-snapshot-applied-functions))
  (should-not (memq #'agent-repl--frontend-recover-after-reconnect
                    agent-repl-uds-connected-functions)))

(ert-deftest agent-repl-test-frontend-recovery-sweeps-before-retracting ()
  "Recovery is driven BEFORE the notices come down.
Retracting first would take the outage notices down over a recovery this
end had merely decided to attempt."
  ;; Arrange
  (let ((calls nil))
    (cl-letf (((symbol-function 'agent-repl--frontend-reattach-check)
               (lambda () (push 'sweep calls)))
              ((symbol-function 'agent-repl-connection-notices-retract)
               (lambda (_reason) (push 'retract calls) 0)))
      ;; Act
      (agent-repl--frontend-recover-after-reconnect)
      ;; Assert
      (should (equal (reverse calls) '(sweep retract))))))

(ert-deftest agent-repl-test-frontend-recovery-names-its-retraction-reason ()
  "The retraction records WHAT cleared the notices, for the log."
  ;; Arrange
  (let ((reason nil))
    (cl-letf (((symbol-function 'agent-repl--frontend-reattach-check) #'ignore)
              ((symbol-function 'agent-repl-connection-notices-retract)
               (lambda (r) (setq reason r) 0)))
      ;; Act
      (agent-repl--frontend-recover-after-reconnect)
      ;; Assert
      (should (equal reason "daemon-reconnected")))))

(ert-deftest agent-repl-test-frontend-note-boot-id-first-observation-sets ()
  "The first boot id observation records without resetting anything."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:reattach-failed t
                                    :project-dir "/w")
    (let ((agent-repl--frontend-last-boot-id nil))
      ;; Act
      (agent-repl--frontend-note-boot-id "b_first")
      ;; Assert — recorded, but no give-up reset on first sight.
      (should (equal agent-repl--frontend-last-boot-id "b_first"))
      (should (agent-repl--ws-get "ws1" :reattach-failed)))))

(ert-deftest agent-repl-test-frontend-note-boot-id-change-resets-give-ups ()
  "A boot id change resets :reattach-failed give-ups across workspaces."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:reattach-failed t
                                    :reattach-failures 3 :project-dir "/w")
    (let ((agent-repl--frontend-last-boot-id "b_old"))
      ;; Act
      (agent-repl--frontend-note-boot-id "b_new")
      ;; Assert
      (should (equal agent-repl--frontend-last-boot-id "b_new"))
      (should-not (agent-repl--ws-get "ws1" :reattach-failed))
      (should-not (agent-repl--ws-get "ws1" :reattach-failures)))))

(ert-deftest agent-repl-test-frontend-note-boot-id-nil-never-resets ()
  "A pre-boot-id daemon (nil boot id) neither records nor resets."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:reattach-failed t
                                    :project-dir "/w")
    (let ((agent-repl--frontend-last-boot-id "b_old"))
      ;; Act
      (agent-repl--frontend-note-boot-id nil)
      ;; Assert
      (should (equal agent-repl--frontend-last-boot-id "b_old"))
      (should (agent-repl--ws-get "ws1" :reattach-failed)))))

(ert-deftest agent-repl-test-frontend-ensure-give-up-is-a-retractable-notice ()
  "The ensure give-up goes out as a notice the reconnect can take back."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:ensure-failures 2 :project-dir "/w")
    (let ((notices nil))
      (cl-letf (((symbol-function 'agent-repl-connection-notice-warn)
                 (lambda (text &optional level) (push (list text level) notices)))
                ((symbol-function 'display-warning)
                 (lambda (&rest _) (error "the give-up must not bypass the notice"))))
        ;; Act
        (agent-repl--frontend-note-ensure-failure "ws1" "boom")
        ;; Assert
        (should (= 1 (length notices)))
        (should (string-match-p "could not be opened" (caar notices)))))))

(ert-deftest agent-repl-test-frontend-reattach-timer-inhibited-in-batch ()
  "The sweep timer does not start when init is inhibited."
  ;; Arrange
  (let ((agent-repl--frontend-reattach-timer nil)
        (armed nil))
    (cl-letf (((symbol-function 'agent-repl--frontend-init-inhibited-p) (lambda () t))
              ((symbol-function 'run-with-timer)
               (lambda (&rest _args) (setq armed t) 'fake-timer)))
      ;; Act
      (agent-repl--frontend-reattach-timer-start)
      ;; Assert
      (should-not armed)
      (should-not agent-repl--frontend-reattach-timer))))

(ert-deftest agent-repl-test-frontend-reattach-timer-starts-once ()
  "The sweep timer arms exactly once across repeated starts."
  ;; Arrange
  (let ((agent-repl--frontend-reattach-timer nil)
        (armed 0))
    (cl-letf (((symbol-function 'agent-repl--frontend-init-inhibited-p) (lambda () nil))
              ((symbol-function 'run-with-timer)
               (lambda (&rest _args) (cl-incf armed) 'fake-timer)))
      ;; Act
      (agent-repl--frontend-reattach-timer-start)
      (agent-repl--frontend-reattach-timer-start)
      ;; Assert
      (should (= armed 1))
      (should (eq agent-repl--frontend-reattach-timer 'fake-timer)))))

;;;; ---- rebind after restart -------------------------------------------------------

(ert-deftest agent-repl-test-frontend-rebind-waits-ready-before-remount ()
  "The rebind waits for the daemon snapshot before remounting webviews."
  ;; Arrange
  (let ((calls nil))
    (cl-letf (((symbol-function 'agent-repl--frontend-after-ready)
              (lambda (ok _fail &optional _ws) (push 'ready calls) (funcall ok) :ready))
              ((symbol-function 'agent-repl--live-ws-names) (lambda () nil))
              ((symbol-function 'agent-repl--frontend-remount-all-webviews)
               (lambda () (push 'remount-all calls))))
      ;; Act
      (agent-repl--frontend-rebind-workspaces-after-restart)
      ;; Assert
      (should (equal (reverse calls) '(ready remount-all))))))

(ert-deftest agent-repl-test-frontend-rebind-remounts-after-snapshot-recovery ()
  "The rebind force-remounts after snapshot-owned recovery was driven."
  ;; Arrange
  (let ((calls nil))
    (cl-letf (((symbol-function 'agent-repl--frontend-after-ready)
               (lambda (ok _fail &optional _ws) (funcall ok) :ready))
              ((symbol-function 'agent-repl--live-ws-names) (lambda () nil))
              ((symbol-function 'agent-repl--frontend-remount-all-webviews)
               (lambda () (push 'remount-all calls) 0)))
      ;; Act
      (agent-repl--frontend-rebind-workspaces-after-restart)
      ;; Assert
      (should (equal calls '(remount-all))))))

(ert-deftest agent-repl-test-frontend-rebind-does-not-duplicate-snapshot-recovery ()
  "The rebind does not race the reconnect snapshot's recovery sweep."
  ;; Arrange
  (let ((reattached nil))
    (cl-letf (((symbol-function 'agent-repl--frontend-after-ready) (lambda (ok _fail &optional _ws) (funcall ok) :ready))
              ((symbol-function 'agent-repl--live-ws-names) (lambda () nil))
              ((symbol-function 'agent-repl--frontend-reattach-check)
               (lambda () (setq reattached t))))
      ;; Act
      (agent-repl--frontend-rebind-workspaces-after-restart)
      ;; Assert
      (should-not reattached))))

(ert-deftest agent-repl-test-frontend-rebind-returns-bound-workspace-count ()
  "The rebind returns how many open workspaces carried a session binding."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w")
    (agent-repl-test--with-ws "ws2" '(:project-dir "/w2")
      (agent-repl-test--with-ws "ws3" '(:project-dir "/w3")
        (cl-letf (((symbol-function 'agent-repl--frontend-after-ready) (lambda (ok _fail &optional _ws) (funcall ok) :ready))
                  ((symbol-function 'agent-repl--frontend-reattach-check) #'ignore))
          ;; Act / Assert — ws1 and ws2 are bound, ws3 is not.
          (should (eq :pending (agent-repl--frontend-rebind-workspaces-after-restart))))))))

(ert-deftest agent-repl-test-frontend-rebind-returns-zero-without-bindings ()
  "The rebind returns 0 when no open workspace carries a session binding."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w")
    (cl-letf (((symbol-function 'agent-repl--frontend-after-ready) (lambda (ok _fail &optional _ws) (funcall ok) :ready))
              ((symbol-function 'agent-repl--frontend-reattach-check) #'ignore))
      ;; Act / Assert
      (should (eq :pending (agent-repl--frontend-rebind-workspaces-after-restart))))))

;;;; ---- release on nuke ------------------------------------------------------------

;;;; ---- never-blue: the workspace-switch ensure ---------------------------
;;
;; The switch half of the never-blue requirement: a persp activation sends
;; `openWorkspace' so the daemon binds any on-disk transcript and brings the
;; shim up.  These pin the SKIPS (which is most of the behavior) as hard as
;; the send, because an unskipped send costs the daemon a projects-directory
;; rescan on every switch.

(defmacro agent-repl-test--with-switch-ensure (&rest body)
  "Run BODY with the UDS boundary captured and the link reported UP."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'agent-repl--uds-connected-p) (lambda () t)))
     (agent-repl-test--with-uds ,@body)))

(ert-deftest agent-repl-test-frontend-switch-ensure-sends-open-workspace ()
  "A switch to a session-less workspace sends `openWorkspace' keyed by its cwd."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w")
    (agent-repl-test--with-switch-ensure
      ;; Act
      (agent-repl--frontend-ensure-workspace "ws1")
      ;; Assert — the daemon routes purely by cwd, never the persp name.
      (pcase-let ((`(,field ,payload ,ws) (car uds-commands)))
        (should (equal field "openWorkspace"))
        (should (null payload))
        (should (equal ws "/w"))))))

(ert-deftest agent-repl-test-frontend-switch-ensure-skips-when-session-live ()
  "A workspace this daemon is already DRIVING has nothing to ensure."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w")
    (cl-letf (((symbol-function 'agent-repl--frontend-session-live-p) (lambda (_id) t))
              ((symbol-function 'agent-repl--frontend-session-controller-live-p) (lambda (_id) t)))
      (agent-repl-test--with-switch-ensure
        ;; Act
        (agent-repl--frontend-ensure-workspace "ws1")
        ;; Assert — this is THE common case; a send here is pure daemon rescan.
        (should (null uds-commands))))))

;;;; ---- never-blue: the DAEMON-RESTART shape ------------------------------
;;
;; The dead perspective switch.  Every fact the skip used to consult is read
;; back off the durable registry record, so after a daemon restart a workspace
;; with no session controller at all still looked live-and-backfilled — every switch
;; skipped, nothing ever bootstrapped, and the workspace sat blue until the
;; user typed.  `SessionView.shim_attached' is the non-durable half.

(ert-deftest agent-repl-test-frontend-session-controller-live-reads-shim-attached ()
  "The session controller-liveness read is the pushed `shim_attached', nothing derived."
  (cl-letf (((symbol-function 'agent-repl--frontend-session-view)
             (lambda (_id) (list :sessionId "s_1" :shimAttached t))))
    (should (agent-repl--frontend-session-controller-live-p "s_1"))))

(ert-deftest agent-repl-test-frontend-session-controller-live-nil-without-the-field ()
  "A daemon that sends no field reads as NOT live, so the switch ensures."
  (cl-letf (((symbol-function 'agent-repl--frontend-session-view)
             (lambda (_id) (list :sessionId "s_1"))))
    (should-not (agent-repl--frontend-session-controller-live-p "s_1"))))

(ert-deftest agent-repl-test-frontend-switch-ensure-sends-after-a-daemon-restart ()
  "THE restart shape: a durable live+backfilled record with NO live session controller.
Every durable fact says the workspace is up; only `shim_attached' knows the
daemon has never brought it up.  A missing session controller must ALWAYS ensure."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w")
    (cl-letf (((symbol-function 'agent-repl--uds-connected-p) (lambda () t))
              ((symbol-function 'agent-repl--frontend-session-live-p) (lambda (_id) t))
              ((symbol-function 'agent-repl--frontend-session-view)
               (lambda (_id) (list :sessionId "s_1"
                                   :backfill "BACKFILL_STATE_DONE"))))
      (agent-repl-test--with-uds
        ;; Act
        (agent-repl--frontend-ensure-workspace "ws1")
        ;; Assert
        (should (equal (car (car uds-commands)) "openWorkspace"))))))

;;;; ---- never-blue: the backfill-completion gate (F2) --------------------
;;
;; A LIVE session whose history never arrived is live and blue at once. These
;; pin that liveness alone no longer earns the skip.

(defmacro agent-repl-test--with-backfill (state &rest body)
  "Run BODY with ws1's bound session live and its `backfill' reading STATE.
STATE nil stands for a pre-F2 daemon that sends no field at all."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'agent-repl--uds-connected-p) (lambda () t))
             ((symbol-function 'agent-repl--frontend-session-live-p) (lambda (_id) t))
             ((symbol-function 'agent-repl--frontend-session-view)
              (lambda (_id) (and ,state (list :sessionId "s_1" :backfill ,state
                                              ;; The daemon is DRIVING this
                                              ;; workspace: these cases isolate
                                              ;; the backfill axis.
                                              :shimAttached t)))))
     (agent-repl-test--with-uds ,@body)))

(ert-deftest agent-repl-test-frontend-backfill-settled-when-done ()
  "A delivered transcript is settled."
  (agent-repl-test--with-backfill "BACKFILL_STATE_DONE"
    (should (agent-repl--frontend-backfill-settled-p "s_1"))))

(ert-deftest agent-repl-test-frontend-backfill-settled-when-nothing-to-backfill ()
  "A workspace with no transcript is settled: an empty feed is CORRECT there."
  (agent-repl-test--with-backfill "BACKFILL_STATE_UNSPECIFIED"
    (should (agent-repl--frontend-backfill-settled-p "s_1"))))

(ert-deftest agent-repl-test-frontend-backfill-unsettled-while-pending ()
  "History that has not landed yet is NOT settled."
  (agent-repl-test--with-backfill "BACKFILL_STATE_PENDING"
    (should-not (agent-repl--frontend-backfill-settled-p "s_1"))))

(ert-deftest agent-repl-test-frontend-backfill-unsettled-when-failed ()
  "A failed sidecar read is NOT settled, and must never read as merely not-yet."
  (agent-repl-test--with-backfill "BACKFILL_STATE_FAILED"
    (should-not (agent-repl--frontend-backfill-settled-p "s_1"))))

(ert-deftest agent-repl-test-frontend-backfill-settled-on-a-pre-f2-daemon ()
  "A daemon that sends no field reads as settled.
It cannot backfill on switch either, so retrying would loop for nothing."
  (agent-repl-test--with-backfill nil
    (should (agent-repl--frontend-backfill-settled-p "s_1"))))

(ert-deftest agent-repl-test-frontend-switch-ensure-skips-a-live-backfilled-session ()
  "The steady state: live AND backfilled earns the skip."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w")
    (agent-repl-test--with-backfill "BACKFILL_STATE_DONE"
      ;; Act
      (agent-repl--frontend-ensure-workspace "ws1")
      ;; Assert
      (should (null uds-commands)))))

(ert-deftest agent-repl-test-frontend-switch-ensure-sends-for-a-live-but-unbackfilled-session ()
  "THE residual this closes: live but blue must re-ensure, not skip."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w")
    (agent-repl-test--with-backfill "BACKFILL_STATE_PENDING"
      ;; Act
      (agent-repl--frontend-ensure-workspace "ws1")
      ;; Assert
      (should (equal (car (car uds-commands)) "openWorkspace")))))

(ert-deftest agent-repl-test-frontend-switch-ensure-sends-for-a-failed-backfill ()
  "A failed sidecar read re-ensures rather than being mistaken for done."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w")
    (agent-repl-test--with-backfill "BACKFILL_STATE_FAILED"
      ;; Act
      (agent-repl--frontend-ensure-workspace "ws1")
      ;; Assert
      (should (equal (car (car uds-commands)) "openWorkspace")))))

(ert-deftest agent-repl-test-frontend-switch-ensure-failed-backfill-still-gives-up ()
  "A permanently failing sidecar cannot retry-loop.
The give-up latch is what bounds it: the unsettled backfill would otherwise
re-send on every single switch forever."
  ;; Arrange — unsettled AND already given up.
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w"
                                    :ensure-failed t)
    (agent-repl-test--with-backfill "BACKFILL_STATE_FAILED"
      ;; Act
      (agent-repl--frontend-ensure-workspace "ws1")
      ;; Assert
      (should (null uds-commands)))))

(ert-deftest agent-repl-test-frontend-switch-ensure-failed-backfill-respects-cooldown ()
  "An unsettled backfill still debounces within the cooldown."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w")
    (agent-repl-test--with-backfill "BACKFILL_STATE_PENDING"
      (agent-repl--frontend-ensure-workspace "ws1")
      ;; Act — a rapid re-switch.
      (agent-repl--frontend-ensure-workspace "ws1")
      ;; Assert
      (should (= 1 (length uds-commands))))))

(ert-deftest agent-repl-test-frontend-switch-ensure-sends-when-bound-session-dead ()
  "A bound-but-dead session still needs the ensure (that is the blue case)."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w")
    (cl-letf (((symbol-function 'agent-repl--frontend-session-live-p) (lambda (_id) nil)))
      (agent-repl-test--with-switch-ensure
        ;; Act
        (agent-repl--frontend-ensure-workspace "ws1")
        ;; Assert
        (should (equal (car (car uds-commands)) "openWorkspace"))))))

(ert-deftest agent-repl-test-frontend-switch-ensure-skips-when-link-down ()
  "With the UDS link down the switch sends nothing.
The reattach sweep owns daemon revival; a switch must not race it."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w")
    (cl-letf (((symbol-function 'agent-repl--uds-connected-p) (lambda () nil)))
      (agent-repl-test--with-uds
        ;; Act
        (agent-repl--frontend-ensure-workspace "ws1")
        ;; Assert
        (should (null uds-commands))))))

(ert-deftest agent-repl-test-frontend-switch-ensure-skips-without-project-dir ()
  "No cwd means no routable wire key, so the switch sends nothing."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:agent-state :idle)
    (agent-repl-test--with-switch-ensure
      ;; Act — must not signal either; this runs on EVERY switch.
      (agent-repl--frontend-ensure-workspace "ws1")
      ;; Assert
      (should (null uds-commands)))))

(ert-deftest agent-repl-test-frontend-switch-ensure-debounces-within-cooldown ()
  "A second switch inside the cooldown does not re-send."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w")
    (agent-repl-test--with-switch-ensure
      (agent-repl--frontend-ensure-workspace "ws1")
      ;; Act — rapid re-switch.
      (agent-repl--frontend-ensure-workspace "ws1")
      ;; Assert — exactly one command, not two.
      (should (= 1 (length uds-commands))))))

(ert-deftest agent-repl-test-frontend-switch-ensure-resends-after-cooldown ()
  "Once the cooldown has elapsed a switch may ensure again."
  ;; Arrange — a stamp older than the cooldown.
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w")
    (agent-repl--ws-put "ws1" :ensure-at
                        (- (float-time) agent-repl-frontend-ensure-cooldown 1))
    (agent-repl-test--with-switch-ensure
      ;; Act
      (agent-repl--frontend-ensure-workspace "ws1")
      ;; Assert
      (should (= 1 (length uds-commands))))))

(ert-deftest agent-repl-test-frontend-switch-ensure-skips-after-give-up ()
  "A workspace that gave up stops sending entirely."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w" :ensure-failed t)
    (agent-repl-test--with-switch-ensure
      ;; Act
      (agent-repl--frontend-ensure-workspace "ws1")
      ;; Assert
      (should (null uds-commands)))))

(ert-deftest agent-repl-test-frontend-switch-ensure-counts-a-failed-ack ()
  "A rejected ack increments the workspace's failure tally."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w")
    ;; Act
    (agent-repl--frontend-note-ensure-failure "ws1" "no live session")
    ;; Assert
    (should (= 1 (agent-repl--ws-get "ws1" :ensure-failures)))))

(ert-deftest agent-repl-test-frontend-switch-ensure-gives-up-at-the-cap ()
  "At the failure cap the workspace latches `:ensure-failed'."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w")
    (let ((display-warning-minimum-level :emergency))
      ;; Act — one short of the cap, then the one that trips it.
      (dotimes (_ agent-repl-frontend-ensure-max-failures)
        (agent-repl--frontend-note-ensure-failure "ws1" "boom"))
      ;; Assert — the retry-loop guard the directive asks for.
      (should (agent-repl--ws-get "ws1" :ensure-failed)))))

(ert-deftest agent-repl-test-frontend-switch-ensure-does-not-give-up-early ()
  "Below the cap the workspace keeps trying."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w")
    ;; Act
    (agent-repl--frontend-note-ensure-failure "ws1" "boom")
    ;; Assert
    (should-not (agent-repl--ws-get "ws1" :ensure-failed))))

(ert-deftest agent-repl-test-frontend-switch-ensure-boot-change-clears-give-up ()
  "A new daemon instance earns a workspace fresh switch-ensure attempts."
  ;; Arrange — a give-up that belonged to the PREVIOUS instance.
  (agent-repl-test--with-ws "ws1" '(:ensure-failed t :ensure-failures 3)
    (cl-letf (((symbol-function 'agent-repl--live-ws-names) (lambda () '("ws1"))))
      (let ((agent-repl--frontend-last-boot-id "boot-old"))
        ;; Act
        (agent-repl--frontend-note-boot-id "boot-new")
        ;; Assert
        (should-not (agent-repl--ws-get "ws1" :ensure-failed))
        (should-not (agent-repl--ws-get "ws1" :ensure-failures))))))

(ert-deftest agent-repl-test-frontend-switch-ensure-boot-change-clears-cooldown ()
  "A daemon bounce also clears the cooldown stamp.
Otherwise the first switch after a restart would be swallowed by a timer
belonging to the instance that is already gone."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:ensure-at 12345.0)
    (cl-letf (((symbol-function 'agent-repl--live-ws-names) (lambda () '("ws1"))))
      (let ((agent-repl--frontend-last-boot-id "boot-old"))
        ;; Act
        (agent-repl--frontend-note-boot-id "boot-new")
        ;; Assert
        (should-not (agent-repl--ws-get "ws1" :ensure-at))))))

(ert-deftest agent-repl-test-frontend-switch-ensure-never-signals-on-send-failure ()
  "A send that signals is logged, never raised.
This runs on the persp-activation path, so a signal would strand the
switch before the tail that flips `:ws-loaded'.  The link can die between
the connected-p check and the send, so the skip guards cannot be the only
protection."
  ;; Arrange — connected, then the send blows up anyway.
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w")
    (cl-letf (((symbol-function 'agent-repl--uds-connected-p) (lambda () t))
              ((symbol-function 'agent-repl--uds-send-command)
               (lambda (&rest _) (user-error "not connected"))))
      ;; Act / Assert — must return nil rather than signalling.
      (should (null (agent-repl--frontend-ensure-workspace "ws1"))))))

(ert-deftest agent-repl-test-frontend-switch-ensure-never-signals-without-cwd ()
  "A workspace whose cwd lookup signals is skipped, not raised."
  ;; Arrange — no :project-dir at all, which `--ws-dir' signals on.
  (agent-repl-test--with-ws "ws1" '(:agent-state :idle)
    (cl-letf (((symbol-function 'agent-repl--uds-connected-p) (lambda () t)))
      ;; Act / Assert
      (should (null (agent-repl--frontend-ensure-workspace "ws1"))))))

(ert-deftest agent-repl-test-frontend-switch-ensure-stamps-before-sending ()
  "The cooldown stamp is written on the send, which is what debounces it."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w")
    (agent-repl-test--with-switch-ensure
      ;; Act
      (agent-repl--frontend-ensure-workspace "ws1")
      ;; Assert
      (should (agent-repl--ws-get "ws1" :ensure-at)))))

;;;; ---- gui-send-turn ------------------------------------------------------------

(ert-deftest agent-repl-test-gui-send-turn-keeps-meta-markers ()
  "gui-send-turn posts the marked text VERBATIM to the daemon.
The webapp hides the bracketed spans at render time, so stripping them on
the wire would deprive the agent of the directive it must read."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w")
    (let ((sent nil)
          (input (concat (agent-repl--meta-wrap "READ-DIRECTIVE") "\n\nhello")))
      (cl-letf (((symbol-function 'agent-repl--frontend-send-user-message)
                 (lambda (_ws text _origin ok _fail)
                   (setq sent text)
                   (funcall ok "r_1")
                   :pending))
                ((symbol-function 'agent-repl--run-send-posthooks) #'ignore)
                ((symbol-function 'agent-repl--kickoff-prompt-summary) #'ignore))
        ;; Act
        (agent-repl--gui-send-turn "ws1" input "hello" "PROMPT_ORIGIN_USER_SENT")
        ;; Assert
        (should (equal sent input))))))

;;;; ---- webview URLs --------------------------------------------------------------

(ert-deftest agent-repl-test-frontend-session-url-shape ()
  "The webapp attach URL carries the session query param."
  ;; Arrange
  (let ((agent-repl-frontend-daemon-addr "127.0.0.1:9999"))
    ;; Act / Assert
    (should (equal (agent-repl--frontend-session-url "s_9")
                   "http://127.0.0.1:9999/?session=s_9"))))

(ert-deftest agent-repl-test-frontend-workspace-url-shape ()
  "The workspace attach URL carries the directory path, URL-encoded.
Every case is a path the daemon must read back byte-for-byte: the
scoped connection compares it against the workspace key on each frame,
so a path that survives the round trip only for plain ASCII would
silently serve nothing for the rest."
  ;; Arrange
  (let ((agent-repl-frontend-daemon-addr "127.0.0.1:9999")
        (cases '(("/repos/proj"
                  . "http://127.0.0.1:9999/?workspace=%2Frepos%2Fproj&build=bid1")
                 ("/repos/My Projects/agent repl"
                  . "http://127.0.0.1:9999/?workspace=%2Frepos%2FMy%20Projects%2Fagent%20repl&build=bid1")
                 ("/repos/prosjekt/æøå"
                  . "http://127.0.0.1:9999/?workspace=%2Frepos%2Fprosjekt%2F%C3%A6%C3%B8%C3%A5&build=bid1")
                 ("/repos/a&b=c/d?e#f"
                  . "http://127.0.0.1:9999/?workspace=%2Frepos%2Fa%26b%3Dc%2Fd%3Fe%23f&build=bid1"))))
    (cl-letf (((symbol-function 'agent-repl--frontend-build-id) (lambda () "bid1")))
      (dolist (case cases)
        ;; Act / Assert
        (should (equal (agent-repl--frontend-workspace-url (car case))
                       (cdr case)))))))

(ert-deftest agent-repl-test-frontend-build-id-refuses-a-missing-stamp ()
  "An unbuilt webapp is named, not addressed without its identity.
A URL built with no build id is a stable cache key, which is the stale
bundle this stamp exists to prevent."
  ;; Arrange — a webapp directory the build script has never stamped.
  (let ((agent-repl--frontend-webapp-dir (make-temp-file "agent-repl-webapp" t)))
    ;; Act / Assert
    (should-error (agent-repl--frontend-build-id))))

(ert-deftest agent-repl-test-frontend-build-id-needs-no-built-webapp-in-batch ()
  "The batch harness resolves a build id without a built webapp.
`webapp/dist' is gitignored and built by hand, so reading the real stamp
made every webview-URL test pass or fail on whether whoever ran the suite
had built the webapp — which is why the frontend URL tests failed in a
fresh checkout and in the merge gate's temporary rebase worktree.
test-helpers.el redirects `agent-repl--frontend-webapp-dir' to a stamped
temp dir for the whole batch session, and this pins that redirect."
  ;; Act / Assert — no rebinding: the harness's own directory is the subject.
  (should (equal (agent-repl--frontend-build-id) "test-build-id")))

(ert-deftest agent-repl-test-frontend-build-id-reads-the-stamp ()
  "The build id is the stamp's content, whitespace trimmed."
  ;; Arrange
  (let* ((agent-repl--frontend-webapp-dir (make-temp-file "agent-repl-webapp" t))
         (stamp (expand-file-name ".build-id" agent-repl--frontend-webapp-dir)))
    (with-temp-file stamp (insert "q56e9Rm0\n"))
    ;; Act / Assert
    (should (equal (agent-repl--frontend-build-id) "q56e9Rm0"))))

;; The GET /commands fetch + POST /commands/refresh tests were deleted in
;; the S9 slash-menu cutover: those HTTP calls are gone.  The slash-command
;; menu is now the pushed `SessionInitView' (covered in test-frontend-state.el
;; for the store and test-input.el for the completion source).

;;;; ---- origin-tagged sends (merge-remediation) -----------------------------
;;
;; frontend.v1's `SubmitPromptCmd' has no `origin' field, so the merge
;; status-card origin stamp is no longer forwarded (it was already dead
;; server-side — the retired HTTP /message route never read it into the
;; session controller).  Send-user-message still CONSUMES and clears the one-shot
;; `:next-send-origin' so it never lingers; it just does not reach the wire.

(ert-deftest agent-repl-test-gui-interrupt-sends-command-keyed-by-workspace ()
  "Interrupt dispatches the UDS `interrupt' command keyed by the workspace CWD."
  ;; Arrange
  (agent-repl-test--with-ws "ws1"
      '(:project-dir "/w" :sent-turn (:request-id "r_9" :raw "draft"))
    (agent-repl-test--with-uds
      ;; Act
      (agent-repl--gui-interrupt "ws1" 'escape)
      ;; Assert
      (pcase-let ((`(,field ,_payload ,ws) (car uds-commands)))
        (should (equal field "interrupt"))
        (should (equal ws "/w"))))))

(ert-deftest agent-repl-test-gui-interrupt-returns-t ()
  "Both gestures return t (dispatched); the retract verdict is gone."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w")
    (agent-repl-test--with-uds
      ;; Act / Assert
      (should (eq (agent-repl--gui-interrupt "ws1" 'escape) t))
      (should (eq (agent-repl--gui-interrupt "ws1" 'ctrl-c) t)))))


;;;; ---- the detached-agent cancel ---------------------------------------
;;
;; The command an interrupt structurally cannot make.  `:idle-async' means
;; the main turn has ENDED, so a turn interrupt sent into it is answered
;; ALREADY_COMPLETE and the detached agents it was meant to stop keep
;; running.

(ert-deftest agent-repl-test-gui-cancel-detached-sends-command-keyed-by-workspace ()
  "The cancel dispatches `cancelDetachedAgents' keyed by the workspace CWD."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w")
    (agent-repl-test--with-uds
      ;; Act
      (agent-repl--gui-cancel-detached-agents "ws1")
      ;; Assert
      (pcase-let ((`(,field ,_payload ,ws) (car uds-commands)))
        (should (equal field "cancelDetachedAgents"))
        (should (equal ws "/w"))))))

(ert-deftest agent-repl-test-gui-cancel-detached-carries-no-confirmation-flag ()
  "The cancel sends an EMPTY payload: there is nothing here to confirm.
The question was already put to the user before it was called, and sending
the command IS the deliberate act the interrupt's `confirm_agents' gate
exists to require."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w")
    (agent-repl-test--with-uds
      ;; Act
      (agent-repl--gui-cancel-detached-agents "ws1")
      ;; Assert
      (pcase-let ((`(,_field ,payload ,_ws) (car uds-commands)))
        (should-not payload)))))

(ert-deftest agent-repl-test-gui-cancel-detached-returns-t ()
  "The cancel reports DISPATCH, like the interrupt does."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w")
    (agent-repl-test--with-uds
      ;; Act / Assert
      (should (eq (agent-repl--gui-cancel-detached-agents "ws1") t)))))

(ert-deftest agent-repl-test-gui-cancel-detached-sends-no-interrupt ()
  "The cancel sends ONLY itself — never an interrupt alongside it.
An interrupt in this state provably does nothing, so putting one on the
wire would be exactly the no-op this command replaces."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w")
    (agent-repl-test--with-uds
      ;; Act
      (agent-repl--gui-cancel-detached-agents "ws1")
      ;; Assert
      (should (equal (length uds-commands) 1)))))

;;;; ---- the interrupt confirmation challenge -----------------------------
;;
;; The daemon refuses an interrupt that would stop live SUBAGENTS with
;; `CommandAck.interrupt_confirm_required' — a CHALLENGE, not an error: the
;; command was understood and deliberately not performed.  These drive the
;; REAL ack handler (only the socket write is shadowed) so the routing from
;; ack arm to minibuffer question to resend is covered end to end.

(ert-deftest agent-repl-test-gui-interrupt-challenge-yes-resends-confirmed ()
  "A yes to the challenge resends the interrupt carrying `confirmAgents'."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w")
    (agent-repl-test--with-interrupt-acks t
      (agent-repl--gui-interrupt "ws1" 'escape)
      ;; Act — the daemon challenges the first (unconfirmed) send
      (agent-repl--uds-handle-command-ack
       '(:requestId "req-1" :interruptConfirmRequired (:liveTasks "3")))
      ;; Assert — a second interrupt goes out, confirmed, on the same key
      (should (equal (length uds-commands) 2))
      (should (equal (nth 1 uds-commands) '("interrupt" (:confirmAgents t) "/w"))))))

(ert-deftest agent-repl-test-gui-interrupt-challenge-no-sends-nothing ()
  "A no to the challenge sends nothing further — the subagents keep running."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w")
    (agent-repl-test--with-interrupt-acks nil
      (agent-repl--gui-interrupt "ws1" 'escape)
      ;; Act
      (agent-repl--uds-handle-command-ack
       '(:requestId "req-1" :interruptConfirmRequired (:liveTasks "3")))
      ;; Assert
      (should (equal (length uds-commands) 1)))))

(ert-deftest agent-repl-test-gui-interrupt-challenge-question-names-the-stakes ()
  "The question counts the subagents it would stop, in the wire's own shapes."
  ;; Arrange — (liveTasks . expected question)
  (dolist (case '(("3" . "Interrupt 3 running subagents? ")
                  ("1" . "Interrupt 1 running subagent? ")
                  (2 . "Interrupt 2 running subagents? ")
                  (nil . "Interrupt the running subagents? ")))
    (agent-repl-test--with-ws "ws1" '(:project-dir "/w")
      (agent-repl-test--with-interrupt-acks nil
        (agent-repl--gui-interrupt "ws1" 'escape)
        ;; Act
        (agent-repl--uds-handle-command-ack
         (list :requestId "req-1"
               :interruptConfirmRequired (list :liveTasks (car case))))
        ;; Assert
        (should (equal asked (list (cdr case))))))))

(ert-deftest agent-repl-test-gui-interrupt-ok-ack-never-prompts ()
  "An accepted interrupt is done: no question, no resend."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w")
    (agent-repl-test--with-interrupt-acks t
      (agent-repl--gui-interrupt "ws1" 'escape)
      ;; Act
      (agent-repl--uds-handle-command-ack '(:requestId "req-1" :ok t))
      ;; Assert
      (should-not asked)
      (should (equal (length uds-commands) 1)))))

(ert-deftest agent-repl-test-gui-interrupt-error-ack-still-surfaces ()
  "A genuine error ack keeps the old failure path: echoed, never a question."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w")
    (agent-repl-test--with-interrupt-acks t
      (agent-repl--gui-interrupt "ws1" 'escape)
      ;; Act — protojson omits ok=false, so a rejection arrives with no :ok
      (agent-repl--uds-handle-command-ack
       '(:requestId "req-1" :error "no live session to drive"))
      ;; Assert
      (should-not asked)
      (should (equal (length uds-commands) 1))
      (should (string-match-p "interrupt" echoed))
      ;; The refusal is still loud, but the daemon's own text does not reach
      ;; the echo area — it is translated into the not-live sentence.
      (should-not (string-match-p "no live session to drive" echoed))
      (should (string-match-p "this workspace is not live" echoed)))))

(ert-deftest agent-repl-test-gui-interrupt-confirmed-resend-is-not-rechallenged ()
  "The confirmed resend carries no challenge handler — a re-challenge cannot loop."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w")
    (agent-repl-test--with-interrupt-acks t
      (agent-repl--gui-interrupt "ws1" 'escape)
      (agent-repl--uds-handle-command-ack
       '(:requestId "req-1" :interruptConfirmRequired (:liveTasks "3")))
      ;; Act — the daemon contradicts itself and challenges the confirmed send
      (agent-repl--uds-handle-command-ack
       '(:requestId "req-2" :interruptConfirmRequired (:liveTasks "3")))
      ;; Assert — asked once, sent twice: no third command, no second question
      (should (equal (length asked) 1))
      (should (equal (length uds-commands) 2)))))

(ert-deftest agent-repl-test-gui-send-turn-records-the-sent-turn ()
  "The send records what an undo of it would need: the id and the RAW text."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '()
    (cl-letf (((symbol-function 'agent-repl--frontend-send-user-message)
               (lambda (_ws _text _origin ok _fail)
                 (funcall ok "r_9")
                 :pending))
              ((symbol-function 'agent-repl--mark-ws-thinking) #'ignore)
              ((symbol-function 'agent-repl--run-send-posthooks) #'ignore)
              ((symbol-function 'agent-repl--kickoff-prompt-summary) #'ignore))
      ;; Act — the prepared text carries decoration the user never typed.
      (agent-repl--gui-send-turn "ws1" "META\n\nwrite a test" "write a test" "PROMPT_ORIGIN_USER_SENT")
      ;; Assert — RAW is recorded, since the decoration is not the user's to revise.
      (should (equal (agent-repl--ws-get "ws1" :sent-turn)
                     '(:request-id "r_9" :raw "write a test"))))))

;;;; ---- the workspace wire key -------------------------------------------
;;
;; The daemon routes ws-keyed commands by matching the `workspace' field
;; against the session's CWD.  Emacs keys everything by the persp NAME, so the
;; resolver is what keeps the name off the wire — the UDS command cutover
;; shipped without it and every prompt NACKed as "no live session to drive"
;; (2026-07-25).

(ert-deftest agent-repl-test-frontend-ws-command-key-is-the-project-dir ()
  "The wire key is WS's `:project-dir', NEVER the persp name."
  ;; Arrange
  (agent-repl-test--with-ws "doom" '(:project-dir "/Users/x/.config/doom")
    ;; Act / Assert
    (should (equal (agent-repl--frontend-ws-command-key "doom")
                   "/Users/x/.config/doom"))))

(ert-deftest agent-repl-test-frontend-ws-command-key-signals-without-project-dir ()
  "A workspace with no `:project-dir' fails loudly rather than sending its name."
  ;; Arrange
  (agent-repl-test--with-ws "doom" '()
    ;; Act / Assert
    (should-error (agent-repl--frontend-ws-command-key "doom"))))

;;;; ---- SPC o C-c: the HARD SESSION RESTART -------------------------------
;;
;; A restart keeps the conversation and replaces only the process serving it.
;; The daemon does the work; this side only has to ask, key the request by the
;; cwd the daemon routes on, and surface a rejection loudly -- a restart that
;; failed must never read as a session that came back.

(ert-deftest agent-repl-test-frontend-restart-session-sends-the-command ()
  "The restart sends `restartSession' keyed by the workspace's cwd."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w")
    (agent-repl-test--with-uds
      ;; Act
      (agent-repl--frontend-restart-session "ws1")
      ;; Assert — the daemon routes by cwd, never the persp name.
      (pcase-let ((`(,field ,payload ,ws) (car uds-commands)))
        (should (equal field "restartSession"))
        (should (null payload))
        (should (equal ws "/w"))))))

(ert-deftest agent-repl-test-frontend-restart-session-is-a-known-command ()
  "`restartSession' is in the sendable command vocabulary.
An arm missing from the allow-list fails loudly at the send rather than
reaching the daemon, so this is the guard that the wiring exists at all."
  (should (member "restartSession" agent-repl--uds-known-command-fields)))

(ert-deftest agent-repl-test-frontend-restart-session-surfaces-a-rejection ()
  "A REJECTED restart is logged, never read as a session that came back."
  ;; Arrange
  (let (logged call)
    (cl-letf (((symbol-function 'agent-repl--uds-send-command)
               (agent-repl-test--send-command-stub
                "req-1" (lambda (c) (setq call c))))
              ;; A refused restart is a UX regression, so the rejection rides
              ;; the warn rung.
              ((symbol-function 'agent-repl--warn)
               (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logged))))
      (agent-repl-test--with-ws "ws1" '(:project-dir "/w")
        ;; Act — the daemon nacks the command the dispatch registered.
        (agent-repl--frontend-restart-session "ws1")
        (funcall (plist-get call :on-failure) "no live session")
        ;; Assert
        (should (cl-some (lambda (l) (string-match-p "REJECTED" l)) logged))))))

;;;; ---- SPC o z: DELIBERATE HIBERNATION -----------------------------------
;;
;; The user asking for the ~500MB back now instead of waiting out the idle
;; sweeper.  The daemon refuses mid-turn, so the nack path carries as much
;; weight as the send does: a refused hibernate that read as success would
;; leave the user believing they had reclaimed memory they still hold.

(ert-deftest agent-repl-test-frontend-hibernate-workspace-sends-the-command ()
  "The hibernate sends `hibernateWorkspace' keyed by the workspace's cwd."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w")
    (agent-repl-test--with-uds
      ;; Act
      (agent-repl--frontend-hibernate-workspace "ws1")
      ;; Assert — the daemon routes by cwd, never the persp name.
      (pcase-let ((`(,field ,payload ,ws) (car uds-commands)))
        (should (equal field "hibernateWorkspace"))
        (should (null payload))
        (should (equal ws "/w"))))))

(ert-deftest agent-repl-test-frontend-hibernate-workspace-is-a-known-command ()
  "`hibernateWorkspace' is in the sendable command vocabulary.
An arm missing from the allow-list fails loudly at the send rather than
reaching the daemon, so this is the guard that the wiring exists at all."
  (should (member "hibernateWorkspace" agent-repl--uds-known-command-fields)))

(ert-deftest agent-repl-test-frontend-hibernate-workspace-surfaces-a-rejection ()
  "A REJECTED hibernate is logged, never read as a session that went to sleep."
  ;; Arrange
  (let (logged call)
    (cl-letf (((symbol-function 'agent-repl--uds-send-command)
               (agent-repl-test--send-command-stub
                "req-1" (lambda (c) (setq call c))))
              ;; A refused hibernate is a UX regression, so the rejection rides
              ;; the warn rung.
              ((symbol-function 'agent-repl--warn)
               (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logged)))
              ((symbol-function 'message) (lambda (&rest _) nil)))
      (agent-repl-test--with-ws "ws1" '(:project-dir "/w")
        ;; Act
        (agent-repl--frontend-hibernate-workspace "ws1")
        (funcall (plist-get call :on-failure) "a turn is live")
        ;; Assert
        (should (cl-some (lambda (l) (string-match-p "REJECTED" l)) logged))))))

(ert-deftest agent-repl-test-frontend-hibernate-workspace-echoes-the-rejection ()
  "A REJECTED hibernate reaches the USER, not only the log.
The log is where the reason is diagnosed; the echo area is the only place
the person who pressed the key learns the memory was never freed."
  ;; Arrange
  (let (echoed call)
    (cl-letf (((symbol-function 'agent-repl--uds-send-command)
               (agent-repl-test--send-command-stub
                "req-1" (lambda (c) (setq call c))))
              ((symbol-function 'agent-repl--log) (lambda (&rest _) nil))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (push (apply #'format fmt args) echoed))))
      (agent-repl-test--with-ws "ws1" '(:project-dir "/w")
        ;; Act — the daemon's own `ErrNotSettled' text, verbatim.
        (agent-repl--frontend-hibernate-workspace "ws1")
        (funcall (plist-get call :on-failure)
                 "session-controller: the workspace has not settled; refusing to hibernate it")
        ;; Assert
        (should (cl-some (lambda (l) (string-match-p "hibernate refused" l)) echoed))))))

(ert-deftest agent-repl-test-frontend-hibernate-rejection-keeps-the-chain-out-of-the-echo ()
  "The daemon's wrapped text is filed, not echoed: the user reads one sentence."
  ;; Arrange
  (let (echoed call)
    (cl-letf (((symbol-function 'agent-repl--uds-send-command)
               (agent-repl-test--send-command-stub
                "req-1" (lambda (c) (setq call c))))
              ((symbol-function 'agent-repl--log) (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--warn) (lambda (&rest _) nil))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (push (apply #'format fmt args) echoed))))
      (agent-repl-test--with-ws "ws1" '(:project-dir "/w")
        ;; Act
        (agent-repl--frontend-hibernate-workspace "ws1")
        (funcall (plist-get call :on-failure)
                 "session-controller: the workspace has not settled; refusing to hibernate it")
        ;; Assert
        (should-not (cl-some (lambda (l) (string-match-p "session-controller" l)) echoed))))))

(ert-deftest agent-repl-test-frontend-hibernate-rejection-files-the-chain ()
  "The wrapped text the echo dropped still reaches the canonical log."
  ;; Arrange
  (let (logged call)
    (cl-letf (((symbol-function 'agent-repl--uds-send-command)
               (agent-repl-test--send-command-stub
                "req-1" (lambda (c) (setq call c))))
              ((symbol-function 'agent-repl--log)
               (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logged)))
              ((symbol-function 'agent-repl--warn) (lambda (&rest _) nil))
              ((symbol-function 'message) (lambda (&rest _) nil)))
      (agent-repl-test--with-ws "ws1" '(:project-dir "/w")
        ;; Act
        (agent-repl--frontend-hibernate-workspace "ws1")
        (funcall (plist-get call :on-failure)
                 "session-controller: the workspace has not settled; refusing to hibernate it")
        ;; Assert
        (should (cl-some (lambda (l) (string-match-p "session-controller" l)) logged))))))

;;;; ---- Async operation verbs ----

(ert-deftest agent-repl-test-frontend-operation-verb-maps-a-known-label ()
  "An async operation label becomes the English verb the echo line is built on."
  ;; Act / Assert
  (should (equal (agent-repl--frontend-operation-verb "openWorkspace") "open")))

(ert-deftest agent-repl-test-frontend-operation-verb-strips-session-health-identifiers ()
  "The session health label carries a workspace and a session id; neither is copy."
  ;; Act / Assert
  (should (equal (agent-repl--frontend-operation-verb "session ws=ws1 id=s_1")
                 "session health check")))

(ert-deftest agent-repl-test-frontend-operation-verb-degrades-an-unknown-label ()
  "An unmapped label yields nil rather than being printed as a verb."
  ;; Act / Assert
  (should-not (agent-repl--frontend-operation-verb "somethingElse")))

;;;; ---- ensure-workspace skip-record routing ----

(ert-deftest agent-repl-test-frontend-ensure-skip-for-a-placeholder-logs-globally ()
  "Skipping a persp PLACEHOLDER records against the global sink.
The persp-activation driver reaches ensure with persp-mode's own \"main\"
and \"none\", which own no `:project-dir' and so no durable log sink."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((logged 'no-record))
      (cl-letf (((symbol-function 'agent-repl--log-verbose)
                 (lambda (ws &rest _)
                   (when (eq logged 'no-record) (setq logged ws)))))
        ;; Act
        (agent-repl--frontend-ensure-workspace "main"))
      ;; Assert
      (should (null logged)))))

(ert-deftest agent-repl-test-frontend-ensure-skip-for-a-routable-ws-keeps-attribution ()
  "A REAL workspace's skip record stays attributed to that workspace."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((project (make-temp-file "agent-repl-ensure-route-" t))
          (logged 'no-record))
      (unwind-protect
          (progn
            (agent-repl--ws-put "ws1" :project-dir project)
            (cl-letf (((symbol-function 'agent-repl--uds-connected-p) (lambda () nil))
                      ((symbol-function 'agent-repl--log-verbose)
                       (lambda (ws &rest _)
                         (when (eq logged 'no-record) (setq logged ws)))))
              ;; Act
              (agent-repl--frontend-ensure-workspace "ws1")))
        (delete-directory project t))
      ;; Assert
      (should (equal logged "ws1")))))

;;;; ---- Establishment waits for the daemon, and survives a quit -------------
;;
;; The ensure used to be read as a yes/no gate: readiness polling started
;; while the stack was still deploying, so a first open on a stale checkout
;; burned its whole poll budget against a daemon that had not been launched
;; yet.  The continuation makes that overlap unrepresentable.

(ert-deftest agent-repl-test-frontend-ensure-waits-for-the-daemon ()
  "Readiness polling starts only once the daemon ensure reports success."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w")
    (let (ensured polled)
      (cl-letf (((symbol-function 'agent-repl--frontend-after-daemon-ensured)
                 (lambda (ok _fail &optional _force) (setq ensured ok) :pending))
                ((symbol-function 'agent-repl--frontend-after-ready)
                 (lambda (_ok _fail &optional _ws) (setq polled t) :pending)))
        ;; Act
        (agent-repl--frontend-after-ensure-session "ws1" #'ignore #'ignore)
        ;; Assert — nothing polls while the deploy is still running.
        (should-not polled)
        (funcall ensured)
        (should polled)))))

(ert-deftest agent-repl-test-frontend-ensure-reports-a-declined-daemon ()
  "A declined ensure reaches ON-FAILURE and returns `:failed'."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w")
    (let (failure)
      (cl-letf (((symbol-function 'agent-repl--frontend-after-daemon-ensured)
                 (lambda (_ok fail &optional _force) (funcall fail "declined") nil))
                ((symbol-function 'agent-repl--frontend-after-ready)
                 (lambda (&rest _) (error "readiness must not be armed"))))
        ;; Act
        (should (eq :failed (agent-repl--frontend-after-ensure-session
                             "ws1" #'ignore (lambda (d) (setq failure d)))))
        ;; Assert
        (should (equal failure "declined"))))))

(ert-deftest agent-repl-test-frontend-hydrate-writes-both-keys-under-inhibit-quit ()
  "The project dir and the environment are written as one uninterruptible fact.
Split by a `C-g', the workspace keeps a project dir and no environment —
and nothing would ever fill the environment in, because only the branch
that finds a MISSING project dir initializes it."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '()
    (let (observed)
      (cl-letf (((symbol-function 'agent-repl--resolve-current-git-root)
                 (lambda () "/w"))
                ((symbol-function 'agent-repl--initialize-ws-env)
                 (lambda (ws _dir)
                   (setq observed inhibit-quit)
                   (agent-repl--ws-put ws :active-env :bare-metal))))
        ;; Act
        (agent-repl--frontend-hydrate-ws-environment "ws1")
        ;; Assert
        (should observed)
        (should (equal (agent-repl--ws-get "ws1" :project-dir) "/w"))
        (should (eq (agent-repl--ws-get "ws1" :active-env) :bare-metal))))))

(ert-deftest agent-repl-test-frontend-hydrate-leaves-a-hydrated-workspace-alone ()
  "A workspace that already has a project dir is not re-hydrated."
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w")
    (cl-letf (((symbol-function 'agent-repl--resolve-current-git-root)
               (lambda () (error "must not re-resolve"))))
      (agent-repl--frontend-hydrate-ws-environment "ws1")
      (should (equal (agent-repl--ws-get "ws1" :project-dir) "/w")))))

(ert-deftest agent-repl-test-frontend-create-reserves-under-inhibit-quit ()
  "The create's cwd reservation and the dispatch that releases it are atomic.
A quit between them would leave the cwd reserved with nothing armed to
release it, and every later create for that workspace refused outright."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w")
    (let ((agent-repl--frontend-creates-in-flight (make-hash-table :test 'equal))
          observed)
      (cl-letf (((symbol-function 'agent-repl--frontend-after-ready)
                 (lambda (_ok _fail &optional _ws) (setq observed inhibit-quit) :pending))
                ((symbol-function 'agent-repl--uds-run-timer)
                 (lambda (&rest _) nil)))
        ;; Act
        (agent-repl--frontend-after-create-session
         "/w" "sonnet" 'continue nil #'ignore #'ignore "ws1")
        ;; Assert
        (should observed)))))

(ert-deftest agent-repl-test-frontend-create-releases-its-reservation-on-failure ()
  "A settled create clears its cwd reservation, so the next create is allowed."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w")
    (let ((agent-repl--frontend-creates-in-flight (make-hash-table :test 'equal)))
      (cl-letf (((symbol-function 'agent-repl--frontend-after-ready)
                 (lambda (_ok fail &optional _ws) (funcall fail "no daemon") :pending))
                ((symbol-function 'agent-repl--frontend-async-fail) #'ignore)
                ((symbol-function 'agent-repl--uds-run-timer)
                 (lambda (&rest _) nil)))
        ;; Act
        (agent-repl--frontend-after-create-session
         "/w" "sonnet" 'continue nil #'ignore #'ignore "ws1")
        ;; Assert
        (should (equal (hash-table-count agent-repl--frontend-creates-in-flight) 0))))))
