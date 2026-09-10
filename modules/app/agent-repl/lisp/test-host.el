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
  "Desktop notifications posted, newest first: `(WS TITLE MESSAGE ACTIVATE)'.")

(defvar agent-repl-test-host--focused nil
  "What the stubbed `agent-repl--emacs-focused-p' answers.")

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
         (agent-repl-test-host--successor-pending nil)
         (agent-repl-test-host--walk nil)
         (agent-repl-test-host--dialled nil)
         (agent-repl-test-host--dial-accepts t)
         (agent-repl-link-handover-functions nil)
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
                  (push :adopt agent-repl-test-host--walk)
                  (agent-repl-test-host--answer agent-repl-test-host--adopt-answer
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
               ((symbol-function 'agent-repl-link-successor)
                (lambda () agent-repl-test-host--successor))
               ((symbol-function 'agent-repl-link-successor-pending-p)
                (lambda () agent-repl-test-host--successor-pending))
               ((symbol-function 'agent-repl-link-primary) (lambda () nil))
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
               ((symbol-function 'agent-repl--emacs-focused-p)
                (lambda (&optional _ws) agent-repl-test-host--focused))
               ((symbol-function 'agent-repl--notify)
                (lambda (ws title message &optional activate)
                  (push (list ws title message activate)
                        agent-repl-test-host--notifications)))
               ((symbol-function 'agent-repl-status-blink-tab)
                (lambda (ws) (push (cons :blink ws) agent-repl-test-host--effects)))
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

(ert-deftest agent-repl-test-host-select-transport-failure-does-not-record-the-id ()
  "Nobody answering is not the daemon stamping the workspace either."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (agent-repl-test-host--subscribe "ws-1")
    (setq agent-repl-test-host--select-answer
          (list :failure (list :kind :transport :message "no route")))
    ;; Act
    (agent-repl-host-select "ws-1")
    ;; Assert
    (should (null agent-repl-host-last-selected-id))))

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
    (should (equal (seq-take (car agent-repl-test-host--notifications) 3)
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

(ert-deftest agent-repl-test-host-notification-carries-a-click-activation ()
  "R-CLICK: the unfocused banner carries an activation, not a bare line."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (setq agent-repl-test-host--focused nil)
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act
    (agent-repl-test-host--push
     "ws-1" (list :arm :notification :value (agent-repl-test-host--notification)))
    ;; Assert
    (should (functionp (nth 3 (car agent-repl-test-host--notifications))))))

(ert-deftest agent-repl-test-host-notification-activation-selects-this-workspace ()
  "Running the activation selects the workspace the banner came from."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (setq agent-repl-test-host--focused nil)
    (agent-repl-test-host--subscribe "ws-1")
    (agent-repl-test-host--push
     "ws-1" (list :arm :notification :value (agent-repl-test-host--notification)))
    (let ((activated nil))
      (cl-letf (((symbol-function 'agent-repl--notification-activate)
                 (lambda (ws) (setq activated ws))))
        ;; Act
        (funcall (nth 3 (car agent-repl-test-host--notifications)))
        ;; Assert
        (should (equal activated "ws-1"))))))

(ert-deftest agent-repl-test-host-question-asked-unfocused-posts-a-banner ()
  "A question batch blocks the agent: unfocused, it earns the OS banner."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (setq agent-repl-test-host--focused nil)
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act
    (agent-repl-test-host--push
     "ws-1" (list :arm :notification
                  :value (agent-repl-test-host--notification
                          :kind (list :arm :question-asked
                                      :value (list :header "Which branch?")))))
    ;; Assert
    ;; R-CLICK appended an activation closure as the record's 4th element;
    ;; the banner facts are the first three.
    (should (equal (seq-take (car agent-repl-test-host--notifications) 3)
                   (list "ws-1" "ws-1" "the agent has a question")))))

(ert-deftest agent-repl-test-host-question-asked-focused-unselected-blinks ()
  "Focused with the tab elsewhere: the same blink a permission ask gets."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (setq agent-repl-test-host--focused t
          agent-repl-test-host--current-ws "ws-other")
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act
    (agent-repl-test-host--push
     "ws-1" (list :arm :notification
                  :value (agent-repl-test-host--notification
                          :kind (list :arm :question-asked
                                      :value (list :header "Which branch?")))))
    ;; Assert
    (should (equal (assq :blink agent-repl-test-host--effects) '(:blink . "ws-1")))))

(ert-deftest agent-repl-test-host-question-asked-on-the-selected-tab-is-logged-only ()
  "Selected: the footer already shows it, so the log is the whole reaction."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (setq agent-repl-test-host--focused t
          agent-repl-test-host--current-ws "ws-1")
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act
    (agent-repl-test-host--push
     "ws-1" (list :arm :notification
                  :value (agent-repl-test-host--notification
                          :kind (list :arm :question-asked
                                      :value (list :header "Which branch?")))))
    ;; Assert
    (should (and (null agent-repl-test-host--effects)
                 (agent-repl-test-host--logged-p :info "elisp.host.notification-selected")))))

(ert-deftest agent-repl-test-host-question-asked-logs-the-header ()
  "The chip header belongs in the log context, like a gated tool's name."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (setq agent-repl-test-host--focused nil)
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act
    (agent-repl-test-host--push
     "ws-1" (list :arm :notification
                  :value (agent-repl-test-host--notification
                          :kind (list :arm :question-asked
                                      :value (list :header "Which branch?")))))
    ;; Assert
    (should (agent-repl-test-host--logged-p :info "header=\"Which branch?\""))))

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
  "The overtaking order is unusual, so it is seen; it is not an error."
  (agent-repl-test-host--with-harness
    ;; Arrange
    (setq agent-repl-test-host--successor nil
          agent-repl-test-host--successor-pending nil)
    (agent-repl-test-host--subscribe "ws-1")
    ;; Act
    (agent-repl-test-host--push "ws-1" (list :arm :transferred :value nil))
    ;; Assert
    (should (agent-repl-test-host--logged-p
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

(provide 'test-host)

;;; test-host.el ends here
