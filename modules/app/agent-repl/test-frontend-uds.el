;;; test-frontend-uds.el --- ERT tests for frontend-uds.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Pure-elisp tests for the frontend UDS transport.  The ONLY external
;; boundary (`agent-repl--uds-connect', the sole `make-network-process'
;; call) and the timer seam (`agent-repl--uds-run-timer') are shadowed via
;; `cl-letf' in every test that reaches them, so no real socket is opened
;; and no real timer is armed.  `process-send-string' / `process-live-p'
;; are likewise shadowed for the outbound-send and connect tests.
;;
;; One edge case per test, AAA structure.
;;
;; Run with:
;;   emacs -batch -Q -l ert -l test-frontend-uds.el -f ert-run-tests-batch-and-exit

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;; test-helpers.el loads the full module (core.el, workspace.el, ...) via
;; config.el; the G10 files are not yet registered there (stitch phase),
;; so load them explicitly, transport before state (state registers its
;; handlers into the transport at load).
(load (expand-file-name "frontend-uds.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

(require 'cl-lib)

;;;; ---- Fixtures --------------------------------------------------------

(defmacro agent-repl-test--with-uds (&rest body)
  "Run BODY with every frontend-uds module global reset to a clean slate.
Isolates the process, read accumulator, reconnect timer, handler
registry, and request-id counter so tests never leak into each other."
  (declare (indent 0))
  `(let ((agent-repl--uds-process nil)
         (agent-repl--uds-read-accumulator "")
         (agent-repl--uds-reconnect-timer nil)
         (agent-repl--uds-frame-handlers nil)
         (agent-repl--uds-request-id-counter 0)
         (agent-repl--uds-pending-commands (make-hash-table :test 'equal))
         (agent-repl--uds-pending-health-responses
          (make-hash-table :test 'equal))
         (agent-repl-uds-reconnect-delay 2.0)
         (agent-repl-debug nil))
     ,@body))

(defun agent-repl-test--uds-frame (field-json)
  "Return a single-arm FrontendFrame protojson string wrapping FIELD-JSON.
FIELD-JSON is the already-serialized `\"field\": {...}' body."
  (concat "{" field-json "}"))

;;;; ---- decode: well-formed frames --------------------------------------

(ert-deftest agent-repl-test-uds-decode-workspace-state-plist ()
  "A workspaceState frame decodes into a one-key plist keyed by the oneof arm."
  ;; Arrange
  (agent-repl-test--with-uds
    (let ((line "{\"workspaceState\":{\"workspace\":\"ws1\",\"state\":\"RENDER_STATE_IDLE\"}}"))
      ;; Act
      (let ((frame (agent-repl--uds-decode-frame line)))
        ;; Assert
        (should (equal (plist-get frame :workspaceState)
                       '(:workspace "ws1" :state "RENDER_STATE_IDLE")))))))

(ert-deftest agent-repl-test-uds-decode-bool-true-is-t ()
  "A JSON true decodes to elisp t (not the :false sentinel)."
  ;; Arrange
  (agent-repl-test--with-uds
    (let ((line "{\"workspaceState\":{\"turnActive\":true}}"))
      ;; Act
      (let ((frame (agent-repl--uds-decode-frame line)))
        ;; Assert
        (should (eq (plist-get (plist-get frame :workspaceState) :turnActive) t))))))

(ert-deftest agent-repl-test-uds-decode-bool-false-is-nil ()
  "A JSON false decodes to nil."
  ;; Arrange
  (agent-repl-test--with-uds
    (let ((line "{\"workspaceState\":{\"turnActive\":false}}"))
      ;; Act
      (let ((frame (agent-repl--uds-decode-frame line)))
        ;; Assert
        (should (eq (plist-get (plist-get frame :workspaceState) :turnActive) nil))))))

;;;; ---- decode: malformed frames (loud error, never skipped) ------------

(ert-deftest agent-repl-test-uds-decode-malformed-json-signals ()
  "Undecodable JSON signals `agent-repl-uds-malformed-frame' — never skipped."
  ;; Arrange
  (agent-repl-test--with-uds
    ;; Act / Assert
    (should-error (agent-repl--uds-decode-frame "{not valid json")
                  :type 'agent-repl-uds-malformed-frame)))

(ert-deftest agent-repl-test-uds-decode-malformed-carries-raw-line ()
  "The malformed-frame signal carries the offending line for diagnosis."
  ;; Arrange
  (agent-repl-test--with-uds
    ;; Act
    (let ((err (should-error (agent-repl--uds-decode-frame "{bad")
                             :type 'agent-repl-uds-malformed-frame)))
      ;; Assert
      (should (member "{bad" (cdr err))))))

;;;; ---- dispatch --------------------------------------------------------

(ert-deftest agent-repl-test-uds-dispatch-calls-registered-handler ()
  "Dispatch routes a known frame to its registered handler with the payload."
  ;; Arrange
  (agent-repl-test--with-uds
    (let (captured)
      (agent-repl--uds-register-handler
       "workspaceState" (lambda (p) (setq captured p)))
      ;; Act
      (agent-repl--uds-dispatch-frame '(:workspaceState (:workspace "ws1")))
      ;; Assert
      (should (equal captured '(:workspace "ws1"))))))

(ert-deftest agent-repl-test-uds-dispatch-returns-handler-value ()
  "Dispatch returns the handler's return value."
  ;; Arrange
  (agent-repl-test--with-uds
    (agent-repl--uds-register-handler "workspaceState" (lambda (_p) :applied))
    ;; Act / Assert
    (should (eq (agent-repl--uds-dispatch-frame '(:workspaceState (:x 1)))
                :applied))))

(ert-deftest agent-repl-test-uds-dispatch-no-oneof-key-signals ()
  "A frame with no oneof key is malformed and signals loudly."
  ;; Arrange
  (agent-repl-test--with-uds
    ;; Act / Assert
    (should-error (agent-repl--uds-dispatch-frame nil)
                  :type 'agent-repl-uds-malformed-frame)))

(ert-deftest agent-repl-test-uds-dispatch-multiple-oneof-keys-signals ()
  "A frame carrying more than one oneof arm is malformed and signals loudly."
  ;; Arrange
  (agent-repl-test--with-uds
    ;; Act / Assert
    (should-error
     (agent-repl--uds-dispatch-frame
      '(:workspaceState (:workspace "ws1") :snapshot (:workspace "ws1")))
     :type 'agent-repl-uds-malformed-frame)))

(ert-deftest agent-repl-test-uds-dispatch-unknown-field-signals ()
  "A frame with an unknown oneof field is malformed and signals loudly."
  ;; Arrange
  (agent-repl-test--with-uds
    ;; Act / Assert
    (should-error (agent-repl--uds-dispatch-frame '(:bogusField (:x 1)))
                  :type 'agent-repl-uds-malformed-frame)))

(ert-deftest agent-repl-test-uds-dispatch-known-field-no-handler-no-signal ()
  "A known field with no registered handler is logged, not signalled."
  ;; Arrange
  (agent-repl-test--with-uds
    ;; Act / Assert — conversationDelta is a known arm; no handler registered
    (should-not (agent-repl--uds-dispatch-frame '(:conversationDelta (:x 1))))))

(ert-deftest agent-repl-test-uds-dispatch-known-field-no-handler-does-not-call-others ()
  "Missing handler for one field does not invoke another field's handler."
  ;; Arrange
  (agent-repl-test--with-uds
    (let (called)
      (agent-repl--uds-register-handler "workspaceState" (lambda (_p) (setq called t)))
      ;; Act — dispatch a DIFFERENT known-but-unregistered arm
      (agent-repl--uds-dispatch-frame '(:conversationDelta (:x 1)))
      ;; Assert
      (should-not called))))

;;;; ---- register-handler ------------------------------------------------

(ert-deftest agent-repl-test-uds-register-unknown-field-errors ()
  "Registering a handler for an unknown frame field errors (no silent skip)."
  ;; Arrange
  (agent-repl-test--with-uds
    ;; Act / Assert
    (should-error (agent-repl--uds-register-handler "nope" #'ignore))))

(ert-deftest agent-repl-test-uds-register-replaces-existing ()
  "Re-registering a field replaces the prior handler (no duplicate entries)."
  ;; Arrange
  (agent-repl-test--with-uds
    (agent-repl--uds-register-handler "snapshot" (lambda (_p) :first))
    (agent-repl--uds-register-handler "snapshot" (lambda (_p) :second))
    ;; Act
    (let ((entries (cl-count "snapshot" agent-repl--uds-frame-handlers
                             :key #'car :test #'equal)))
      ;; Assert
      (should (= entries 1))
      (should (eq (agent-repl--uds-dispatch-frame '(:snapshot nil)) :second)))))

;;;; ---- filter: framing -------------------------------------------------

(ert-deftest agent-repl-test-uds-filter-dispatches-single-frame ()
  "The filter decodes and dispatches one newline-terminated frame."
  ;; Arrange
  (agent-repl-test--with-uds
    (let (captured)
      (agent-repl--uds-register-handler "workspaceState" (lambda (p) (setq captured p)))
      ;; Act
      (agent-repl--uds-filter
       nil "{\"workspaceState\":{\"workspace\":\"ws1\"}}\n")
      ;; Assert
      (should (equal captured '(:workspace "ws1")))
      (should (string-empty-p agent-repl--uds-read-accumulator)))))

(ert-deftest agent-repl-test-uds-filter-dispatches-two-frames-in-one-chunk ()
  "The filter dispatches every complete frame in a multi-frame chunk."
  ;; Arrange
  (agent-repl-test--with-uds
    (let (workspaces)
      (agent-repl--uds-register-handler
       "workspaceState"
       (lambda (p) (push (plist-get p :workspace) workspaces)))
      ;; Act
      (agent-repl--uds-filter
       nil (concat "{\"workspaceState\":{\"workspace\":\"a\"}}\n"
                   "{\"workspaceState\":{\"workspace\":\"b\"}}\n"))
      ;; Assert
      (should (equal (nreverse workspaces) '("a" "b"))))))

(ert-deftest agent-repl-test-uds-filter-retains-partial-frame ()
  "A partial trailing frame is retained until its newline arrives."
  ;; Arrange
  (agent-repl-test--with-uds
    (let (captured)
      (agent-repl--uds-register-handler "workspaceState" (lambda (p) (setq captured p)))
      ;; Act — first chunk has no newline; nothing dispatched yet
      (agent-repl--uds-filter nil "{\"workspaceState\":{\"workspa")
      (should-not captured)
      ;; ... second chunk completes the frame
      (agent-repl--uds-filter nil "ce\":\"ws1\"}}\n")
      ;; Assert
      (should (equal captured '(:workspace "ws1")))
      (should (string-empty-p agent-repl--uds-read-accumulator)))))

(ert-deftest agent-repl-test-uds-filter-skips-blank-framing-line ()
  "A blank framing line is skipped (not decoded, no error)."
  ;; Arrange
  (agent-repl-test--with-uds
    (let ((calls 0))
      (agent-repl--uds-register-handler "workspaceState" (lambda (_p) (cl-incf calls)))
      ;; Act — blank line between real frames
      (agent-repl--uds-filter
       nil (concat "\n{\"workspaceState\":{\"workspace\":\"a\"}}\n"))
      ;; Assert — only the real frame dispatched
      (should (= calls 1)))))

;;;; ---- connect (external boundary mocked) ------------------------------

(ert-deftest agent-repl-test-uds-connect-invokes-boundary-with-path ()
  "Connect calls the boundary wrapper with the socket path + filter + sentinel."
  ;; Arrange
  (agent-repl-test--with-uds
    (let (args)
      (cl-letf (((symbol-function 'agent-repl--uds-connect)
                 (lambda (path name filter sentinel)
                   (setq args (list path name filter sentinel))
                   'fake-proc))
                ((symbol-function 'process-live-p) (lambda (p) (eq p 'fake-proc)))
                ((symbol-function 'process-name) (lambda (_p) "fake"))
                ((symbol-function 'process-status) (lambda (_p) 'open)))
        ;; Act
        (agent-repl-uds-connect "/tmp/test.sock")
        ;; Assert
        (should (equal (nth 0 args) "/tmp/test.sock"))
        (should (eq (nth 2 args) #'agent-repl--uds-filter))
        (should (eq (nth 3 args) #'agent-repl--uds-sentinel))
        (should (eq agent-repl--uds-process 'fake-proc))))))

(ert-deftest agent-repl-test-uds-connect-defaults-to-configured-path ()
  "Connect with no PATH dials `agent-repl-uds-socket-path'."
  ;; Arrange
  (agent-repl-test--with-uds
    (let ((agent-repl-uds-socket-path "/tmp/configured.sock")
          dialed)
      (cl-letf (((symbol-function 'agent-repl--uds-connect)
                 (lambda (path &rest _) (setq dialed path) 'fake-proc))
                ((symbol-function 'process-live-p) (lambda (p) (eq p 'fake-proc)))
                ((symbol-function 'process-name) (lambda (_p) "fake"))
                ((symbol-function 'process-status) (lambda (_p) 'open)))
        ;; Act
        (agent-repl-uds-connect)
        ;; Assert
        (should (equal dialed "/tmp/configured.sock"))))))

(ert-deftest agent-repl-test-uds-connect-already-connected-is-noop ()
  "Connect while already connected returns the existing process, no re-dial."
  ;; Arrange
  (agent-repl-test--with-uds
    (setq agent-repl--uds-process 'live-proc)
    (let ((dials 0))
      (cl-letf (((symbol-function 'process-live-p) (lambda (p) (eq p 'live-proc)))
                ((symbol-function 'process-name) (lambda (_p) "live"))
                ((symbol-function 'agent-repl--uds-connect)
                 (lambda (&rest _) (cl-incf dials) 'new-proc)))
        ;; Act
        (let ((result (agent-repl-uds-connect "/tmp/x.sock")))
          ;; Assert
          (should (eq result 'live-proc))
          (should (= dials 0)))))))

(ert-deftest agent-repl-test-uds-connect-failure-schedules-reconnect ()
  "A failed dial schedules a reconnect, leaves the process nil, returns nil."
  ;; Arrange
  (agent-repl-test--with-uds
    (let (scheduled)
      (cl-letf (((symbol-function 'process-live-p) (lambda (_p) nil))
                ((symbol-function 'agent-repl--uds-connect)
                 (lambda (&rest _) (error "connection refused")))
                ((symbol-function 'agent-repl--uds-run-timer)
                 (lambda (delay fn) (setq scheduled (list delay fn)) 'fake-timer)))
        ;; Act
        (let ((result (agent-repl-uds-connect "/tmp/x.sock")))
          ;; Assert
          (should-not result)
          (should-not agent-repl--uds-process)
          (should (equal (nth 0 scheduled) 2.0))
          (should (eq (nth 1 scheduled) #'agent-repl-uds-connect)))))))

(ert-deftest agent-repl-test-uds-connect-readiness-failure-stays-with-owner ()
  "A cold-start dial logs but leaves retry and final failure to readiness."
  ;; Arrange
  (agent-repl-test--with-uds
    (let (scheduled surfaced)
      (cl-letf (((symbol-function 'process-live-p) (lambda (_p) nil))
                ((symbol-function 'agent-repl--uds-connect)
                 (lambda (&rest _) (error "connection refused")))
                ((symbol-function 'agent-repl--uds-run-timer)
                 (lambda (&rest args) (setq scheduled args) 'fake-timer))
                ((symbol-function 'agent-repl-failure-surface)
                 (lambda (&rest args) (setq surfaced args))))
        ;; Act
        (let ((result (agent-repl-uds-connect "/tmp/x.sock" t)))
          ;; Assert — the synchronous readiness loop retains sole ownership.
          (should-not result)
          (should-not agent-repl--uds-process)
          (should-not scheduled)
          (should-not surfaced)
          (should-not agent-repl--uds-reconnect-timer))))))

;;;; ---- socket liveness probe (adopted-daemon detection) ----------------

(ert-deftest agent-repl-test-uds-socket-live-p-true-when-already-connected ()
  "A live link is proof enough; no throwaway probe is dialed."
  ;; Arrange
  (agent-repl-test--with-uds
    (setq agent-repl--uds-process 'live-proc)
    (let ((probes 0))
      (cl-letf (((symbol-function 'process-live-p) (lambda (p) (eq p 'live-proc)))
                ((symbol-function 'agent-repl--uds-probe)
                 (lambda (&rest _) (cl-incf probes) t)))
        ;; Act / Assert
        (should (agent-repl--uds-socket-live-p))
        (should (= probes 0))))))

(ert-deftest agent-repl-test-uds-socket-live-p-probes-when-disconnected ()
  "With no link, a throwaway probe answers the question."
  ;; Arrange
  (agent-repl-test--with-uds
    (let (probed)
      (cl-letf (((symbol-function 'process-live-p) (lambda (_p) nil))
                ((symbol-function 'agent-repl--uds-probe)
                 (lambda (path) (setq probed path) t)))
        ;; Act / Assert
        (should (agent-repl--uds-socket-live-p "/tmp/probe.sock"))
        (should (equal probed "/tmp/probe.sock"))))))

(ert-deftest agent-repl-test-uds-socket-live-p-defaults-to-configured-path ()
  "The probe with no PATH targets `agent-repl-uds-socket-path'."
  ;; Arrange
  (agent-repl-test--with-uds
    (let ((agent-repl-uds-socket-path "/tmp/configured.sock")
          probed)
      (cl-letf (((symbol-function 'process-live-p) (lambda (_p) nil))
                ((symbol-function 'agent-repl--uds-probe)
                 (lambda (path) (setq probed path) t)))
        ;; Act
        (agent-repl--uds-socket-live-p)
        ;; Assert
        (should (equal probed "/tmp/configured.sock"))))))

(ert-deftest agent-repl-test-uds-socket-live-p-nil-on-refused-dial ()
  "A refused probe (no listener, or a stale socket file) reads as absent."
  ;; Arrange
  (agent-repl-test--with-uds
    (cl-letf (((symbol-function 'process-live-p) (lambda (_p) nil))
              ((symbol-function 'agent-repl--uds-probe)
               (lambda (&rest _) (error "connection refused"))))
      ;; Act / Assert
      (should-not (agent-repl--uds-socket-live-p "/tmp/gone.sock")))))

(ert-deftest agent-repl-test-uds-socket-live-p-never-adopts-the-probe-process ()
  "The probe must not become the tracked connection."
  ;; Arrange
  (agent-repl-test--with-uds
    (cl-letf (((symbol-function 'process-live-p) (lambda (_p) nil))
              ((symbol-function 'agent-repl--uds-probe) (lambda (&rest _) t)))
      ;; Act
      (agent-repl--uds-socket-live-p "/tmp/probe.sock")
      ;; Assert
      (should-not agent-repl--uds-process))))

(ert-deftest agent-repl-test-uds-probe-is-a-registered-external-boundary ()
  "The throwaway probe opens a real socket, so it MUST carry a test guard."
  ;; Arrange / Act / Assert
  (should (memq 'agent-repl--uds-probe agent-repl--external-boundary-functions)))

;;;; ---- reconnect scheduling --------------------------------------------

(ert-deftest agent-repl-test-uds-schedule-reconnect-uses-timer-seam ()
  "Scheduling a reconnect calls the timer seam with the delay + connect fn."
  ;; Arrange
  (agent-repl-test--with-uds
    (let ((agent-repl-uds-reconnect-delay 5.0)
          scheduled)
      (cl-letf (((symbol-function 'agent-repl--uds-run-timer)
                 (lambda (delay fn) (setq scheduled (list delay fn)) 'fake-timer)))
        ;; Act
        (agent-repl--uds-schedule-reconnect)
        ;; Assert
        (should (equal (nth 0 scheduled) 5.0))
        (should (eq (nth 1 scheduled) #'agent-repl-uds-connect))
        (should (eq agent-repl--uds-reconnect-timer 'fake-timer))))))

(ert-deftest agent-repl-test-uds-sentinel-dead-link-schedules-reconnect ()
  "A dead-link sentinel clears the process/accumulator and reconnects."
  ;; Arrange
  (agent-repl-test--with-uds
    (setq agent-repl--uds-process 'dead-proc
          agent-repl--uds-read-accumulator "leftover")
    (let (scheduled)
      (cl-letf (((symbol-function 'process-live-p) (lambda (_p) nil))
                ((symbol-function 'process-name) (lambda (_p) "dead"))
                ((symbol-function 'agent-repl--uds-run-timer)
                 (lambda (delay fn) (setq scheduled (list delay fn)) 'fake-timer)))
        ;; Act
        (agent-repl--uds-sentinel 'dead-proc "connection broken by remote peer\n")
        ;; Assert
        (should-not agent-repl--uds-process)
        (should (string-empty-p agent-repl--uds-read-accumulator))
        (should (eq (nth 1 scheduled) #'agent-repl-uds-connect))))))

(ert-deftest agent-repl-test-uds-sentinel-live-link-does-not-reconnect ()
  "A sentinel event on a still-live process does not schedule a reconnect."
  ;; Arrange
  (agent-repl-test--with-uds
    (let ((scheduled nil))
      (cl-letf (((symbol-function 'process-live-p) (lambda (_p) t))
                ((symbol-function 'process-name) (lambda (_p) "live"))
                ((symbol-function 'agent-repl--uds-run-timer)
                 (lambda (&rest _) (setq scheduled t) 'fake-timer)))
        ;; Act
        (agent-repl--uds-sentinel 'live-proc "open\n")
        ;; Assert
        (should-not scheduled)))))

;;;; ---- outbound command send -------------------------------------------

(defmacro agent-repl-test--capturing-send (sent-var &rest body)
  "Run BODY with `process-send-string' shadowed to push into SENT-VAR.
The request-id generator is stubbed deterministic (\"req-fixed\")."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'process-live-p) (lambda (p) (eq p 'fake-proc)))
             ((symbol-function 'agent-repl--uds-generate-request-id)
              (lambda () "req-fixed"))
             ((symbol-function 'process-send-string)
              (lambda (_proc s) (setq ,sent-var s))))
     ,@body))

(ert-deftest agent-repl-test-uds-send-command-shapes-frame ()
  "Send wraps the payload under the oneof field with requestId + workspace."
  ;; Arrange
  (agent-repl-test--with-uds
    (let (sent)
      (agent-repl-test--capturing-send sent
        ;; Act
        (agent-repl--uds-send-command
         "submitPrompt" '(:text "hi" :permissionMode "") "ws1" 'fake-proc)
        ;; Assert — parse the wire bytes back and inspect the shape
        (let ((frame (json-parse-string (string-trim-right sent)
                                        :object-type 'plist :array-type 'list)))
          (should (equal (plist-get frame :requestId) "req-fixed"))
          (should (equal (plist-get frame :workspace) "ws1"))
          (should (equal (plist-get (plist-get frame :submitPrompt) :text) "hi")))))))

(ert-deftest agent-repl-test-uds-send-command-newline-terminated ()
  "The sent frame is newline-terminated (the wire delimiter)."
  ;; Arrange
  (agent-repl-test--with-uds
    (let (sent)
      (agent-repl-test--capturing-send sent
        ;; Act
        (agent-repl--uds-send-command "interrupt" '(:hard t) "ws1" 'fake-proc)
        ;; Assert
        (should (string-suffix-p "\n" sent))))))

(ert-deftest agent-repl-test-uds-send-command-returns-request-id ()
  "Send returns the generated request-id."
  ;; Arrange
  (agent-repl-test--with-uds
    (let (sent)
      (agent-repl-test--capturing-send sent
        ;; Act / Assert
        (should (equal (agent-repl--uds-send-command
                        "interrupt" '(:hard t) "ws1" 'fake-proc)
                       "req-fixed"))))))

(ert-deftest agent-repl-test-uds-send-command-accepts-create-session ()
  "The S7 `createSession' command field is accepted and shaped."
  ;; Arrange
  (agent-repl-test--with-uds
    (let (sent)
      (agent-repl-test--capturing-send sent
        ;; Act
        (agent-repl--uds-send-command
         "createSession" '(:cwd "/w" :model "haiku") "/w" 'fake-proc)
        ;; Assert
        (let ((frame (json-parse-string (string-trim-right sent)
                                        :object-type 'plist :array-type 'list)))
          (should (equal (plist-get (plist-get frame :createSession) :cwd) "/w")))))))

(ert-deftest agent-repl-test-uds-send-command-accepts-delete-session ()
  "The S7 `deleteSession' command field is accepted and shaped."
  ;; Arrange
  (agent-repl-test--with-uds
    (let (sent)
      (agent-repl-test--capturing-send sent
        ;; Act
        (agent-repl--uds-send-command "deleteSession" '(:sessionId "s_9") nil 'fake-proc)
        ;; Assert
        (let ((frame (json-parse-string (string-trim-right sent)
                                        :object-type 'plist :array-type 'list)))
          (should (equal (plist-get (plist-get frame :deleteSession) :sessionId) "s_9")))))))

(ert-deftest agent-repl-test-uds-daemon-view-is-a-known-frame ()
  "The S7 `daemonView' oneof arm is a recognized frame field (not malformed)."
  ;; Act / Assert
  (should (member "daemonView" agent-repl--uds-known-frame-fields)))

(ert-deftest agent-repl-test-uds-session-init-is-a-known-frame ()
  "The S9 `sessionInit' oneof arm is a recognized frame field (not malformed)."
  ;; Act / Assert
  (should (member "sessionInit" agent-repl--uds-known-frame-fields)))

(ert-deftest agent-repl-test-uds-heartbeat-is-a-known-frame ()
  "The E4 `heartbeat' oneof arm is a recognized frame field (not malformed)."
  ;; Act / Assert
  (should (member "heartbeat" agent-repl--uds-known-frame-fields)))

(ert-deftest agent-repl-test-uds-heartbeat-is-a-deliberately-ignored-frame ()
  "`heartbeat' is declared ignored, so its silence reads as design not a gap."
  ;; Act / Assert
  (should (member "heartbeat" agent-repl--uds-ignored-frame-fields)))

(ert-deftest agent-repl-test-uds-task-catalog-is-a-deliberately-ignored-frame ()
  "`taskCatalog' is webapp-only, so Emacs declares rather than warns about it."
  ;; Act / Assert
  (should (member "taskCatalog" agent-repl--uds-ignored-frame-fields)))

(ert-deftest agent-repl-test-uds-ignored-frames-are-all-known-frames ()
  "Every deliberately-ignored arm must also be a known arm, or dispatch signals."
  ;; Act / Assert
  (dolist (field agent-repl--uds-ignored-frame-fields)
    (should (member field agent-repl--uds-known-frame-fields))))

(ert-deftest agent-repl-test-uds-dispatch-ignored-field-returns-nil ()
  "Dispatching a deliberately-ignored arm is a no-op, not a signal."
  ;; Arrange
  (agent-repl-test--with-uds
    ;; Act / Assert
    (should-not (agent-repl--uds-dispatch-frame
                 '(:heartbeat (:workspace "ws1" :progress (:toolUseId "tu1")))))))

(ert-deftest agent-repl-test-uds-dispatch-ignored-field-skips-unwired-warning ()
  "An ignored arm must NOT log the unfinished-wiring message a real gap logs."
  ;; Arrange
  (agent-repl-test--with-uds
    (let (logged)
      (cl-letf (((symbol-function 'agent-repl--log)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logged))))
        ;; Act
        (agent-repl--uds-dispatch-frame '(:heartbeat (:workspace "ws1")))
        ;; Assert
        (should-not (seq-find (lambda (m) (string-match-p "no handler registered" m))
                              logged))))))

(ert-deftest agent-repl-test-uds-dispatch-task-catalog-logs-scoped-shape-in-verbose-mode ()
  "TaskCatalog diagnostics identify their workspace, session, and roster size.
The frame names its workspace by cwd; the SINK is scoped by the resolved
persp name, and the raw cwd stays in the message text."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-uds
      (agent-repl--ws-put "ws1" :project-dir temporary-file-directory)
      (let (logged-ws logged-text)
        (cl-letf (((symbol-function 'agent-repl--log-verbose)
                   (lambda (ws fmt &rest args)
                     (setq logged-ws ws
                           logged-text (apply #'format fmt args)))))
          ;; Act
          (agent-repl--uds-dispatch-frame
           (list :taskCatalog
                 (list :workspace temporary-file-directory :sessionId "s1"
                       :tasks '((:taskId "t1") (:taskId "t2")))))
          ;; Assert
          (should (equal logged-ws "ws1"))
          (should (string-match-p "field=taskCatalog" logged-text))
          (should (string-match-p (regexp-quote temporary-file-directory)
                                  logged-text))
          (should (string-match-p "session=s1" logged-text))
          (should (string-match-p "task-count=2" logged-text)))))))

(ert-deftest agent-repl-test-uds-dispatch-unignored-gap-still-warns ()
  "A known arm that is NOT declared ignored still logs the wiring gap loudly."
  ;; Arrange
  (agent-repl-test--with-uds
    (let (logged)
      (cl-letf (((symbol-function 'agent-repl--log)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logged))))
        ;; Act
        (agent-repl--uds-dispatch-frame '(:conversationDelta (:x 1)))
        ;; Assert
        (should (seq-find (lambda (m) (string-match-p "no handler registered" m))
                          logged))))))

(ert-deftest agent-repl-test-uds-queue-is-a-known-frame ()
  "The E4 `queue' oneof arm is a recognized frame field (not malformed)."
  ;; Act / Assert
  (should (member "queue" agent-repl--uds-known-frame-fields)))

(ert-deftest agent-repl-test-uds-queue-is-a-deliberately-ignored-frame ()
  "`queue' is declared ignored: the webapp owns the chips and their controls."
  ;; Act / Assert
  (should (member "queue" agent-repl--uds-ignored-frame-fields)))

(ert-deftest agent-repl-test-uds-dispatch-queue-frame-returns-nil ()
  "Dispatching a queue frame is a no-op, not a signal."
  ;; Arrange
  (agent-repl-test--with-uds
    ;; Act / Assert
    (should-not (agent-repl--uds-dispatch-frame
                 '(:queue (:workspace "ws1" :sessionId "s1" :entries nil))))))

(ert-deftest agent-repl-test-uds-progress-is-a-known-frame ()
  "The F1 `progress' oneof arm is a recognized frame field (not malformed).
The daemon pushes a `ProgressView' per workspace; this vocabulary
predated the frame, so every push signalled
`agent-repl-uds-malformed-frame' and surfaced as a user-visible error on
workspace open."
  ;; Act / Assert
  (should (member "progress" agent-repl--uds-known-frame-fields)))

(ert-deftest agent-repl-test-uds-progress-is-a-deliberately-ignored-frame ()
  "`progress' is declared ignored: the footer is webapp-only by design."
  ;; Act / Assert
  (should (member "progress" agent-repl--uds-ignored-frame-fields)))

(ert-deftest agent-repl-test-uds-dispatch-progress-frame-returns-nil ()
  "Dispatching a progress frame is a no-op, not a signal."
  ;; Arrange
  (agent-repl-test--with-uds
    ;; Act / Assert
    (should-not (agent-repl--uds-dispatch-frame
                 '(:progress (:workspace "ws1" :sessionId "s1" :liveTaskCount 0))))))

(ert-deftest agent-repl-test-uds-dispatch-progress-frame-skips-unwired-warning ()
  "A progress frame must NOT log the unfinished-wiring message a real gap logs."
  ;; Arrange
  (agent-repl-test--with-uds
    (let (logged)
      (cl-letf (((symbol-function 'agent-repl--log)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logged))))
        ;; Act
        (agent-repl--uds-dispatch-frame '(:progress (:workspace "ws1")))
        ;; Assert
        (should-not (seq-find (lambda (m) (string-match-p "no handler registered" m))
                              logged))))))

(ert-deftest agent-repl-test-uds-queue-commands-are-known ()
  "The three E4 queue control arms are in the command mirror."
  ;; Act / Assert
  (dolist (field '("queueForce" "queueAccept" "queueCancel"))
    (should (member field agent-repl--uds-known-command-fields))))

(ert-deftest agent-repl-test-uds-client-log-is-a-known-command ()
  "The E4 `clientLog' arm is in the command mirror, though Emacs never sends it."
  ;; Act / Assert
  (should (member "clientLog" agent-repl--uds-known-command-fields)))

(ert-deftest agent-repl-test-uds-shutdown-is-a-known-command ()
  "The S9 `shutdown' command arm is an accepted outbound command field."
  ;; Act / Assert
  (should (member "shutdown" agent-repl--uds-known-command-fields)))

(ert-deftest agent-repl-test-uds-send-command-accepts-shutdown ()
  "The S9 `shutdown' command field is accepted and shapes an empty object."
  ;; Arrange
  (agent-repl-test--with-uds
    (let (sent)
      (agent-repl-test--capturing-send sent
        ;; Act
        (agent-repl--uds-send-command "shutdown" nil nil 'fake-proc)
        ;; Assert — ShutdownCmd is empty; the arm must serialize as `{}'
        (should (string-match-p "\"shutdown\":{}" sent))))))

(ert-deftest agent-repl-test-uds-send-command-empty-payload-is-object ()
  "A nil payload serializes as an empty object `{}', never JSON null."
  ;; Arrange
  (agent-repl-test--with-uds
    (let (sent)
      (agent-repl-test--capturing-send sent
        ;; Act
        (agent-repl--uds-send-command "closeWorkspace" nil "ws1" 'fake-proc)
        ;; Assert — the arm must be present as an object for oneof detection
        (should (string-match-p "\"closeWorkspace\":{}" sent))))))

(ert-deftest agent-repl-test-uds-send-command-no-connection-errors ()
  "Sending with no live connection fails loudly (no queue, no silent drop)."
  ;; Arrange
  (agent-repl-test--with-uds
    (cl-letf (((symbol-function 'process-live-p) (lambda (_p) nil)))
      ;; Act / Assert
      (should-error (agent-repl--uds-send-command "interrupt" '(:hard t) "ws1" nil)
                    :type 'user-error))))

(ert-deftest agent-repl-test-uds-send-command-unknown-field-errors ()
  "Sending an unknown command field fails loudly."
  ;; Arrange
  (agent-repl-test--with-uds
    (cl-letf (((symbol-function 'process-live-p) (lambda (p) (eq p 'fake-proc))))
      ;; Act / Assert
      (should-error (agent-repl--uds-send-command "bogusCmd" nil "ws1" 'fake-proc)
                    :type 'user-error))))

(ert-deftest agent-repl-test-uds-generate-request-id-increments ()
  "Successive request-ids differ (the monotonic counter advances)."
  ;; Arrange
  (agent-repl-test--with-uds
    ;; Act
    (let ((a (agent-repl--uds-generate-request-id))
          (b (agent-repl--uds-generate-request-id)))
      ;; Assert
      (should-not (equal a b)))))

;;;; ---- command-ack tracking --------------------------------------------

(ert-deftest agent-repl-test-uds-track-command-records-entry ()
  "Tracking a command records its field + workspace under the request-id."
  ;; Arrange
  (agent-repl-test--with-uds
    ;; Act
    (agent-repl--uds-track-command "req-1" "mergeWorkspace" "ws1")
    ;; Assert
    (let ((entry (gethash "req-1" agent-repl--uds-pending-commands)))
      (should (equal (plist-get entry :field) "mergeWorkspace"))
      (should (equal (plist-get entry :workspace) "ws1")))))

(ert-deftest agent-repl-test-uds-command-ack-ok-drops-entry ()
  "An ok=t ack drops the pending entry and returns t."
  ;; Arrange
  (agent-repl-test--with-uds
    (agent-repl--uds-track-command "req-1" "mergeWorkspace" "ws1")
    ;; Act / Assert
    (should (eq (agent-repl--uds-handle-command-ack '(:requestId "req-1" :ok t)) t))
    (should-not (gethash "req-1" agent-repl--uds-pending-commands))))

(ert-deftest agent-repl-test-uds-command-ack-failure-surfaces-loudly ()
  "A failed ack (ok omitted) surfaces an echo-area message and returns nil."
  ;; Arrange
  (agent-repl-test--with-uds
    (agent-repl--uds-track-command "req-1" "mergeWorkspace" "ws1")
    (let (echoed)
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (setq echoed (apply #'format fmt args)))))
        ;; Act — protojson omits ok=false, so a failure ack has no :ok
        (let ((ret (agent-repl--uds-handle-command-ack
                    '(:requestId "req-1" :error "branch not found"))))
          ;; Assert
          (should-not ret)
          (should (string-match-p "mergeWorkspace" echoed))
          (should (string-match-p "branch not found" echoed)))))))

(ert-deftest agent-repl-test-uds-command-ack-failure-runs-on-failure ()
  "A failed ack runs the tracked :on-failure callback with the error string."
  ;; Arrange
  (agent-repl-test--with-uds
    (let (cb-arg)
      (agent-repl--uds-track-command
       "req-1" "mergeWorkspace" "ws1"
       (lambda (err) (setq cb-arg err)))
      (cl-letf (((symbol-function 'message) (lambda (&rest _) nil)))
        ;; Act
        (agent-repl--uds-handle-command-ack '(:requestId "req-1" :error "boom"))
        ;; Assert
        (should (equal cb-arg "boom"))))))

(ert-deftest agent-repl-test-uds-command-ack-challenge-runs-on-challenge ()
  "A challenge ack routes its payload to the tracked :on-challenge callback."
  ;; Arrange
  (agent-repl-test--with-uds
    (let (cb-arg)
      (agent-repl--uds-track-command
       "req-1" "interrupt" "ws1" nil nil
       (lambda (challenge) (setq cb-arg challenge)))
      ;; Act
      (agent-repl--uds-handle-command-ack
       '(:requestId "req-1" :interruptConfirmRequired (:liveTasks "3")))
      ;; Assert
      (should (equal cb-arg '(:liveTasks "3"))))))

(ert-deftest agent-repl-test-uds-command-ack-challenge-skips-the-failure-path ()
  "A challenge is not a failure: no echo, and :on-failure never runs."
  ;; Arrange
  (agent-repl-test--with-uds
    (let (echoed failed)
      (agent-repl--uds-track-command
       "req-1" "interrupt" "ws1"
       (lambda (_err) (setq failed t)) nil #'ignore)
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (setq echoed (apply #'format fmt args)))))
        ;; Act
        (agent-repl--uds-handle-command-ack
         '(:requestId "req-1" :interruptConfirmRequired (:liveTasks "3")))
        ;; Assert
        (should-not failed)
        (should-not echoed)))))

(ert-deftest agent-repl-test-uds-command-ack-unhandled-challenge-surfaces ()
  "A challenge nobody can answer surfaces loudly rather than dying quietly."
  ;; Arrange
  (agent-repl-test--with-uds
    (let (echoed)
      (agent-repl--uds-track-command "req-1" "interrupt" "ws1")
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (setq echoed (apply #'format fmt args)))))
        ;; Act
        (agent-repl--uds-handle-command-ack
         '(:requestId "req-1" :interruptConfirmRequired (:liveTasks "3")))
        ;; Assert
        (should (string-match-p "confirmation" echoed))))))

(ert-deftest agent-repl-test-uds-command-ack-ok-never-runs-on-challenge ()
  "An accepted ack runs :on-success only; the challenge handler stays idle."
  ;; Arrange
  (agent-repl-test--with-uds
    (let (challenged succeeded)
      (agent-repl--uds-track-command
       "req-1" "interrupt" "ws1" nil
       (lambda () (setq succeeded t))
       (lambda (_challenge) (setq challenged t)))
      ;; Act
      (agent-repl--uds-handle-command-ack '(:requestId "req-1" :ok t))
      ;; Assert
      (should succeeded)
      (should-not challenged))))

(ert-deftest agent-repl-test-uds-command-ack-error-never-runs-on-challenge ()
  "A genuine error ack keeps the failure path and never asks a question."
  ;; Arrange
  (agent-repl-test--with-uds
    (let (challenged failed)
      (agent-repl--uds-track-command
       "req-1" "interrupt" "ws1"
       (lambda (err) (setq failed err)) nil
       (lambda (_challenge) (setq challenged t)))
      (cl-letf (((symbol-function 'message) (lambda (&rest _) nil)))
        ;; Act
        (agent-repl--uds-handle-command-ack
         '(:requestId "req-1" :error "boom"))
        ;; Assert
        (should (equal failed "boom"))
        (should-not challenged)))))

(ert-deftest agent-repl-test-uds-command-ack-untracked-does-not-surface ()
  "An ack for an untracked request-id logs only — no echo-area surfacing."
  ;; Arrange
  (agent-repl-test--with-uds
    (let (echoed)
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (setq echoed (apply #'format fmt args)))))
        ;; Act — a failure-shaped ack for a request we never tracked
        (agent-repl--uds-handle-command-ack
         '(:requestId "ghost" :error "boom"))
        ;; Assert — untracked acks are not surfaced (no pending caller cares)
        (should-not echoed)))))

(ert-deftest agent-repl-test-uds-command-ack-without-request-id-is-malformed ()
  "A command acknowledgement without correlation identity fails loudly."
  ;; Arrange
  (agent-repl-test--with-uds
    ;; Act / Assert
    (should-error
     (agent-repl--uds-handle-command-ack '(:ok t :workspace "ws1"))
     :type 'agent-repl-uds-malformed-frame)))

;;;; ---- correlated health responses ------------------------------------

(ert-deftest agent-repl-test-uds-health-frame-vocabulary-and-handlers ()
  "Both health result arms are known and registered at module load."
  (dolist (pair '(("daemonHealth" . agent-repl--uds-handle-daemon-health)
                  ("sessionHealth" . agent-repl--uds-handle-session-health)))
    (should (member (car pair) agent-repl--uds-known-frame-fields))
    (should (eq (cdr (assoc (car pair) agent-repl--uds-frame-handlers))
                (cdr pair)))))

(ert-deftest agent-repl-test-uds-daemon-health-correlates-by-request-id ()
  "A daemon health view reaches only the callback tracked under its id."
  (agent-repl-test--with-uds
    (let (received)
      (agent-repl--uds-track-health-response
       "health-1" "daemonHealth" nil nil
       (lambda (view) (setq received view)))
      (agent-repl--uds-handle-daemon-health
       '(:requestId "health-1" :healthy t))
      (should (equal received '(:requestId "health-1" :healthy t)))
      (should-not
       (gethash "health-1" agent-repl--uds-pending-health-responses)))))

(ert-deftest agent-repl-test-uds-session-health-requires-exact-identities ()
  "A stale session result cannot satisfy a newer workspace binding."
  (agent-repl-test--with-uds
    (let (received)
      (agent-repl--uds-track-health-response
       "health-2" "sessionHealth" "/w/tree" "s_expected"
       (lambda (view) (setq received view)))
      (should-error
       (agent-repl--uds-handle-session-health
        '(:requestId "health-2"
          :workspace "/w/tree"
          :sessionId "s_stale"
          :healthy t))
       :type 'agent-repl-uds-malformed-frame)
      (should-not received)
      (should-not
       (gethash "health-2" agent-repl--uds-pending-health-responses)))))

(ert-deftest agent-repl-test-uds-untracked-health-never-mutates-caller-state ()
  "A late result after timeout is logged but cannot call an abandoned waiter."
  (agent-repl-test--with-uds
    (let (called)
      (agent-repl--uds-track-health-response
       "health-3" "daemonHealth" nil nil
       (lambda (_view) (setq called t)))
      (agent-repl--uds-untrack-health-response
       "health-3" nil "test-timeout")
      (should-not
       (agent-repl--uds-handle-daemon-health
        '(:requestId "health-3" :healthy t)))
      (should-not called))))

(ert-deftest agent-repl-test-uds-health-response-without-request-id-is-malformed ()
  "A health assertion without correlation identity fails loudly."
  (agent-repl-test--with-uds
    (should-error
     (agent-repl--uds-handle-daemon-health '(:healthy t))
     :type 'agent-repl-uds-malformed-frame)))

(ert-deftest agent-repl-test-uds-command-ack-handler-registered ()
  "Loading frontend-uds.el registers the commandAck handler."
  (should (eq (cdr (assoc "commandAck" agent-repl--uds-frame-handlers))
              #'agent-repl--uds-handle-command-ack)))

;;;; ---- Wire CWDs never reach the workspace log sink --------------------
;;
;; Every `workspace' field on this transport — inbound frame, outbound
;; command, tracking registry — is a session CWD, never a persp name.  A CWD
;; cannot index `agent-repl--workspaces', so handing one to the logging
;; ladder makes `agent-repl--workspace-log-identity' signal.  Inbound frames
;; are dispatched from the connection's process filter, where that signal
;; kills the filter.
;;
;; These tests are meaningful ONLY with the durable sink enabled: with it off
;; `agent-repl--persist-log-record' skips identity resolution entirely, which
;; is why this file passed for the whole life of the defect.  Hence
;; `agent-repl-test--with-log-sink-on' (test-helpers.el) around each one.

(defmacro agent-repl-test--with-uds-log-sink (&rest body)
  "Run BODY under a clean workspace hash, a clean transport, and a live sink."
  (declare (indent 0))
  `(agent-repl-test--with-clean-state
     (agent-repl-test--with-uds
       (agent-repl-test--with-log-sink-on
         ,@body))))

(ert-deftest agent-repl-test-uds-dispatch-unowned-cwd-does-not-signal ()
  "Dispatching a frame whose workspace no live workspace owns must not signal."
  ;; Arrange
  (agent-repl-test--with-uds-log-sink
    ;; Act / Assert — a known arm with no handler still logs, loudly
    (should-not (agent-repl--uds-dispatch-frame
                 '(:conversationDelta (:workspace "/nowhere/unowned"))))))

(ert-deftest agent-repl-test-uds-dispatch-resolves-owned-cwd-to-its-name ()
  "A frame naming an OWNED cwd routes its log line to that workspace's NAME."
  ;; Arrange
  (agent-repl-test--with-uds-log-sink
    (agent-repl--ws-put "ws1" :project-dir temporary-file-directory)
    (let (logged-ws)
      (cl-letf (((symbol-function 'agent-repl--log)
                 (lambda (ws &rest _) (setq logged-ws ws))))
        ;; Act
        (agent-repl--uds-dispatch-frame
         (list :conversationDelta (list :workspace temporary-file-directory)))
        ;; Assert
        (should (equal logged-ws "ws1"))))))

(ert-deftest agent-repl-test-uds-send-command-keeps-the-raw-cwd-on-the-wire ()
  "Resolving for the log must NOT rewrite the cwd the daemon routes by."
  ;; Arrange
  (agent-repl-test--with-uds-log-sink
    (agent-repl--ws-put "ws1" :project-dir temporary-file-directory)
    (let (sent)
      (agent-repl-test--capturing-send sent
        ;; Act
        (agent-repl--uds-send-command
         "submitPrompt" '(:text "hi") temporary-file-directory 'fake-proc)
        ;; Assert
        (should (equal (plist-get (json-parse-string (string-trim-right sent)
                                                     :object-type 'plist
                                                     :array-type 'list)
                                  :workspace)
                       temporary-file-directory))))))

(ert-deftest agent-repl-test-uds-send-command-unowned-cwd-does-not-signal ()
  "Sending for a cwd nothing owns logs globally rather than aborting the send."
  ;; Arrange
  (agent-repl-test--with-uds-log-sink
    (let (sent)
      (agent-repl-test--capturing-send sent
        ;; Act / Assert
        (should (equal (agent-repl--uds-send-command
                        "submitPrompt" '(:text "hi") "/nowhere/unowned" 'fake-proc)
                       "req-fixed"))))))

(ert-deftest agent-repl-test-uds-track-command-unowned-cwd-does-not-signal ()
  "Tracking a command issued for an unowned cwd must not signal."
  ;; Arrange
  (agent-repl-test--with-uds-log-sink
    ;; Act / Assert
    (should (equal (agent-repl--uds-track-command
                    "req-1" "submitPrompt" "/nowhere/unowned")
                   "req-1"))))

(ert-deftest agent-repl-test-uds-track-command-retains-the-raw-cwd ()
  "The retained `:workspace' stays raw — the ack path correlates against it."
  ;; Arrange
  (agent-repl-test--with-uds-log-sink
    (agent-repl--ws-put "ws1" :project-dir temporary-file-directory)
    ;; Act
    (agent-repl--uds-track-command "req-1" "submitPrompt" temporary-file-directory)
    ;; Assert
    (should (equal (plist-get (gethash "req-1" agent-repl--uds-pending-commands)
                              :workspace)
                   temporary-file-directory))))

(ert-deftest agent-repl-test-uds-untrack-command-unowned-cwd-does-not-signal ()
  "Untracking after a local timeout must not signal on an unowned cwd."
  ;; Arrange
  (agent-repl-test--with-uds-log-sink
    (agent-repl--uds-track-command "req-1" "submitPrompt" "/nowhere/unowned")
    ;; Act / Assert
    (should-not (agent-repl--uds-untrack-command
                 "req-1" "/nowhere/unowned" "health-timeout"))))

(ert-deftest agent-repl-test-uds-track-health-unowned-cwd-does-not-signal ()
  "Registering a health waiter for an unowned cwd must not signal."
  ;; Arrange
  (agent-repl-test--with-uds-log-sink
    ;; Act / Assert
    (should (equal (agent-repl--uds-track-health-response
                    "req-1" "sessionHealth" "/nowhere/unowned" "s1" #'ignore)
                   "req-1"))))

(ert-deftest agent-repl-test-uds-track-health-retains-the-raw-cwd ()
  "The retained health `:workspace' stays raw — the reply is compared to it."
  ;; Arrange
  (agent-repl-test--with-uds-log-sink
    (agent-repl--ws-put "ws1" :project-dir temporary-file-directory)
    ;; Act
    (agent-repl--uds-track-health-response
     "req-1" "sessionHealth" temporary-file-directory "s1" #'ignore)
    ;; Assert
    (should (equal (plist-get (gethash "req-1"
                                       agent-repl--uds-pending-health-responses)
                              :workspace)
                   temporary-file-directory))))

(ert-deftest agent-repl-test-uds-untrack-health-unowned-cwd-does-not-signal ()
  "Dropping a health waiter for an unowned cwd must not signal."
  ;; Arrange
  (agent-repl-test--with-uds-log-sink
    (agent-repl--uds-track-health-response
     "req-1" "sessionHealth" "/nowhere/unowned" "s1" #'ignore)
    ;; Act / Assert
    (should-not (agent-repl--uds-untrack-health-response
                 "req-1" "/nowhere/unowned" "health-timeout"))))

(ert-deftest agent-repl-test-uds-health-response-untracked-cwd-does-not-signal ()
  "A health reply arriving after its waiter timed out must not signal."
  ;; Arrange
  (agent-repl-test--with-uds-log-sink
    ;; Act / Assert
    (should-not (agent-repl--uds-handle-health-response
                 "sessionHealth"
                 '(:requestId "req-gone" :workspace "/nowhere/unowned"
                   :sessionId "s1" :healthy t)))))

(ert-deftest agent-repl-test-uds-health-response-correlated-cwd-does-not-signal ()
  "A correlated health reply logs under the resolved name, not the cwd."
  ;; Arrange
  (agent-repl-test--with-uds-log-sink
    (agent-repl--uds-track-health-response
     "req-1" "sessionHealth" "/nowhere/unowned" "s1" #'ignore)
    ;; Act / Assert
    (should (agent-repl--uds-handle-health-response
             "sessionHealth"
             '(:requestId "req-1" :workspace "/nowhere/unowned"
               :sessionId "s1" :healthy t)))))

(ert-deftest agent-repl-test-uds-command-ack-untracked-cwd-does-not-signal ()
  "An ack for a request nobody tracked carries the ack's own wire cwd."
  ;; Arrange
  (agent-repl-test--with-uds-log-sink
    ;; Act / Assert
    (should-not (agent-repl--uds-handle-command-ack
                 '(:requestId "req-gone" :workspace "/nowhere/unowned")))))

(ert-deftest agent-repl-test-uds-command-ack-accepted-cwd-does-not-signal ()
  "An ACCEPTED ack logs under the tracked cwd and must not signal."
  ;; Arrange
  (agent-repl-test--with-uds-log-sink
    (agent-repl--uds-track-command "req-1" "submitPrompt" "/nowhere/unowned")
    ;; Act / Assert
    (should (agent-repl--uds-handle-command-ack
             '(:requestId "req-1" :ok t)))))

(ert-deftest agent-repl-test-uds-command-ack-rejected-cwd-does-not-signal ()
  "A REJECTED ack also routes its classified failure through the log sink."
  ;; Arrange
  (agent-repl-test--with-uds-log-sink
    (agent-repl--uds-track-command "req-1" "submitPrompt" "/nowhere/unowned")
    ;; Act / Assert
    (should-not (agent-repl--uds-handle-command-ack
                 '(:requestId "req-1" :error "nope"
                   :failure (:errorClass "ERROR_CLASS_INTERNAL"
                             :errorType "shim.nack"
                             :message "the daemon refused"))))))

(ert-deftest agent-repl-test-uds-command-ack-challenge-cwd-does-not-signal ()
  "The interrupt-confirmation CHALLENGE branch logs with the same cwd."
  ;; Arrange
  (agent-repl-test--with-uds-log-sink
    (agent-repl--uds-track-command "req-1" "interrupt" "/nowhere/unowned")
    ;; Act / Assert
    (should-not (agent-repl--uds-handle-command-ack
                 '(:requestId "req-1"
                   :interruptConfirmRequired (:liveTasks 2))))))

(provide 'test-frontend-uds)

;;; test-frontend-uds.el ends here
