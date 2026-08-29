;;; test-host.el --- ERT tests for agent-repl host.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;   AGENT_REPL_FORBID_VENDOR_CALLS=1 emacs -batch -Q -l ert \
;;     -l lisp/test-host.el -f ert-run-tests-batch-and-exit
;;
;; The rpc layer is stubbed with functions that invoke their callbacks
;; SYNCHRONOUSLY, so every arm — success, daemon-authored error, transport
;; failure — is exercised deterministically with no process anywhere.  The
;; three W2-B surfaces host.el calls (`agent-repl-status-blink-tab',
;; `agent-repl-frontend-reload-webview', `agent-repl-popup-open') are
;; NAMED by host.el and defined by W2-B, so they are recorded here rather
;; than invoked for real.

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

(defun agent-repl-test-host--notification (&rest overrides)
  "Return a `HostWorkspaceNotification' plist, with OVERRIDES applied."
  (let ((base (list :text "the agent has a question"
                    :at-ms 1700000000000
                    :kind (list :arm :agent-addressed :value nil))))
    (while overrides
      (setq base (plist-put base (pop overrides) (pop overrides))))
    base))

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

(defvar agent-repl-test-host--select-answer nil
  "What the stubbed SelectWorkspace answers; see the harness.")

(defvar agent-repl-test-host--effects nil
  "W2-B surface calls, newest first: `(NAME . ARGS)'.")

(defvar agent-repl-test-host--notifications nil
  "Desktop notifications posted, newest first: `(WS TITLE MESSAGE)'.")

(defvar agent-repl-test-host--focused nil
  "What the stubbed `agent-repl--emacs-focused-p' answers.")

(defvar agent-repl-test-host--current-ws nil
  "What the stubbed `agent-repl--ws-current-name' answers.")

(defvar agent-repl-test-host--logs nil
  "Captured `(LEVEL . TEXT)' log entries, newest first.")

(defvar agent-repl-test-host--successor nil
  "What the stubbed `agent-repl-link-successor' answers.")

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
    (:failure (when on-failure (funcall on-failure (cadr answer))))))

(defmacro agent-repl-test-host--with-harness (&rest body)
  "Run BODY with host.el's whole world faked and its state reset."
  (declare (indent 0))
  `(let ((agent-repl-host--by-name (make-hash-table :test 'equal))
         (agent-repl-host-last-selected-id nil)
         (agent-repl-host-update-functions nil)
         (agent-repl-test-host--streams nil)
         (agent-repl-test-host--cancelled nil)
         (agent-repl-test-host--calls nil)
         (agent-repl-test-host--effects nil)
         (agent-repl-test-host--notifications nil)
         (agent-repl-test-host--logs nil)
         (agent-repl-test-host--focused nil)
         (agent-repl-test-host--current-ws nil)
         (agent-repl-test-host--successor nil)
         (agent-repl-test-host--register-answer
          (list :response (list :arm :success
                                :value (list :workspace (agent-repl-test-host--ref)))))
         (agent-repl-test-host--select-answer
          (list :response (list :arm :success :value nil)))
         (agent-repl-test-host--adopt-answer
          (list :response (list :arm :success :value nil))))
     (cl-letf (((symbol-function 'agent-repl-rpc-register-workspace)
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
               ((symbol-function 'agent-repl-rpc-adopt-host-workspace)
                (lambda (conn request &rest keys)
                  (push (list "AdoptHostWorkspace" conn request) agent-repl-test-host--calls)
                  (agent-repl-test-host--answer agent-repl-test-host--adopt-answer
                                                (plist-get keys :on-response)
                                                (plist-get keys :on-failure))))
               ((symbol-function 'agent-repl-rpc-watch-host-workspace)
                (lambda (conn ref on-push on-close)
                  (let ((record (list :conn conn :ref ref
                                      :on-push on-push :on-close on-close)))
                    (push record agent-repl-test-host--streams)
                    record)))
               ((symbol-function 'agent-repl-connect-stream-cancel)
                (lambda (stream) (push stream agent-repl-test-host--cancelled)))
               ((symbol-function 'agent-repl-link-successor)
                (lambda () agent-repl-test-host--successor))
               ((symbol-function 'agent-repl-link-primary) (lambda () nil))
               ((symbol-function 'agent-repl--ws-put) (lambda (&rest _) nil))
               ((symbol-function 'agent-repl--ws-current-name)
                (lambda () agent-repl-test-host--current-ws))
               ((symbol-function 'agent-repl--emacs-focused-p)
                (lambda (&optional _ws) agent-repl-test-host--focused))
               ((symbol-function 'agent-repl--notify)
                (lambda (ws title message)
                  (push (list ws title message) agent-repl-test-host--notifications)))
               ((symbol-function 'agent-repl-status-blink-tab)
                (lambda (ws) (push (cons :blink ws) agent-repl-test-host--effects)))
               ((symbol-function 'agent-repl-frontend-reload-webview)
                (lambda (ws) (push (cons :reload ws) agent-repl-test-host--effects)))
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

(ert-deftest agent-repl-test-host-gate-merge-parked-arm ()
  "A parked merge leaves the composer OPEN WITH CONTEXT, never refused."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    (agent-repl-test-host--push
     "ws-1" (list :arm :host :value (agent-repl-test-host--composer :merge-parked)))
    ;; Act / Assert
    (should (eq (agent-repl-host-composer-gate "ws-1") :merge-parked))))

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

(ert-deftest agent-repl-test-host-session-id-is-the-correlation-token ()
  "The session id is what transcripts and health probes are joined on."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    (agent-repl-test-host--push
     "ws-1" (list :arm :host :value (agent-repl-test-host--live)))
    ;; Act / Assert
    (should (equal (agent-repl-host-session-id "ws-1") "session-1"))))

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

;;;; ---- The notification policy ----

(ert-deftest agent-repl-test-host-notification-unfocused-posts-a-desktop-banner ()
  "Emacs unfocused: the OS banner is the whole reaction."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (setq agent-repl-test-host--focused nil)
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act
    (agent-repl-test-host--push
     "ws-1" (list :arm :notification :value (agent-repl-test-host--notification)))
    ;; Assert
    (should (equal (car agent-repl-test-host--notifications)
                   (list "ws-1" "ws-1" "the agent has a question")))))

(ert-deftest agent-repl-test-host-notification-unfocused-does-not-blink ()
  "The three cases are exclusive: an unfocused Emacs blinks nothing."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (setq agent-repl-test-host--focused nil)
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act
    (agent-repl-test-host--push
     "ws-1" (list :arm :notification :value (agent-repl-test-host--notification)))
    ;; Assert
    (should (null (assq :blink agent-repl-test-host--effects)))))

(ert-deftest agent-repl-test-host-notification-focused-unselected-blinks-the-tab ()
  "Focused with the tab elsewhere: the canonical blink cadence."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (setq agent-repl-test-host--focused t
          agent-repl-test-host--current-ws "ws-other")
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act
    (agent-repl-test-host--push
     "ws-1" (list :arm :notification :value (agent-repl-test-host--notification)))
    ;; Assert
    (should (equal (assq :blink agent-repl-test-host--effects) '(:blink . "ws-1")))))

(ert-deftest agent-repl-test-host-notification-focused-unselected-posts-no-banner ()
  "A banner while the user is looking at Emacs would be noise."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (setq agent-repl-test-host--focused t
          agent-repl-test-host--current-ws "ws-other")
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act
    (agent-repl-test-host--push
     "ws-1" (list :arm :notification :value (agent-repl-test-host--notification)))
    ;; Assert
    (should (null agent-repl-test-host--notifications))))

(ert-deftest agent-repl-test-host-notification-on-the-selected-tab-does-nothing ()
  "The footer's activity line already shows it."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (setq agent-repl-test-host--focused t
          agent-repl-test-host--current-ws "ws-1")
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act
    (agent-repl-test-host--push
     "ws-1" (list :arm :notification :value (agent-repl-test-host--notification)))
    ;; Assert
    (should (null agent-repl-test-host--effects))))

(ert-deftest agent-repl-test-host-notification-on-the-selected-tab-is-still-logged ()
  "Doing nothing is a decision, and it is on the record."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (setq agent-repl-test-host--focused t
          agent-repl-test-host--current-ws "ws-1")
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act
    (agent-repl-test-host--push
     "ws-1" (list :arm :notification :value (agent-repl-test-host--notification)))
    ;; Assert
    (should (agent-repl-test-host--logged-p :info "elisp.host.notification-selected"))))

(ert-deftest agent-repl-test-host-permission-request-follows-the-same-policy ()
  "A permission ask is a notification kind, not a separate reaction."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (setq agent-repl-test-host--focused t
          agent-repl-test-host--current-ws "ws-other")
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act
    (agent-repl-test-host--push
     "ws-1" (list :arm :notification
                  :value (agent-repl-test-host--notification
                          :kind (list :arm :permission-requested
                                      :value (list :tool-name "Bash")))))
    ;; Assert
    (should (equal (assq :blink agent-repl-test-host--effects) '(:blink . "ws-1")))))

(ert-deftest agent-repl-test-host-permission-request-logs-the-tool-name ()
  "The gated tool belongs in the log context, not in a line Emacs composes."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (setq agent-repl-test-host--focused nil)
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act
    (agent-repl-test-host--push
     "ws-1" (list :arm :notification
                  :value (agent-repl-test-host--notification
                          :kind (list :arm :permission-requested
                                      :value (list :tool-name "Bash")))))
    ;; Assert
    (should (agent-repl-test-host--logged-p :info "tool=\"Bash\""))))

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

(ert-deftest agent-repl-test-host-transferred-without-a-successor-is-an-error ()
  "No successor means the client relay broke; that is loud."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (setq agent-repl-test-host--successor nil)
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act
    (agent-repl-test-host--push "ws-1" (list :arm :transferred :value nil))
    ;; Assert
    (should (agent-repl-test-host--logged-p
             :error "elisp.host.transferred-without-successor"))))

(ert-deftest agent-repl-test-host-transferred-without-a-successor-keeps-the-stream ()
  "A workspace whose old stream was dropped would be served by nobody."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (setq agent-repl-test-host--successor nil)
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act
    (agent-repl-test-host--push "ws-1" (list :arm :transferred :value nil))
    ;; Assert
    (should (null agent-repl-test-host--cancelled))))

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
  "A webapp-only rollout bounces the webview against the SAME daemon."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act
    (agent-repl-test-host--push "ws-1" (list :arm :reload-webapp :value nil))
    ;; Assert
    (should (equal (assq :reload agent-repl-test-host--effects) '(:reload . "ws-1")))))

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

(ert-deftest agent-repl-test-host-link-up-skips-a-workspace-with-no-dir ()
  "A workspace with no directory has nothing to register."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (let ((conn (agent-repl-connect-open "127.0.0.1:9001")))
      (cl-letf (((symbol-function 'agent-repl--live-ws-names) (lambda () '("ws-1")))
                ((symbol-function 'agent-repl--ws-get) (lambda (&rest _) nil)))
        ;; Act
        (agent-repl-host-on-link-up conn))
      ;; Assert
      (should (null agent-repl-test-host--calls)))))

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

(provide 'test-host)

;;; test-host.el ends here
