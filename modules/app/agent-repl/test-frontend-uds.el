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
registry, request-id counter, ack-aging tables, and command-link health so
tests never leak into each other.

The timer seam `agent-repl--uds-run-timer' is stubbed to a sentinel for
the whole body, so tracking a command never arms a real ack-deadline
alarm.  Tests that assert ON the seam shadow it again with their own
`cl-letf', which wins."
  (declare (indent 0))
  `(let ((agent-repl--uds-process nil)
         (agent-repl--uds-read-accumulator "")
         (agent-repl--uds-reconnect-timer nil)
         (agent-repl--uds-frame-handlers nil)
         (agent-repl--uds-request-id-counter 0)
         (agent-repl--uds-pending-commands (make-hash-table :test 'equal))
         (agent-repl--uds-timed-out-commands (make-hash-table :test 'equal))
         (agent-repl--uds-pending-health-responses
          (make-hash-table :test 'equal))
         (agent-repl--uds-link-health :healthy)
         (agent-repl-uds-reconnect-delay 2.0)
         (agent-repl-uds-command-ack-deadline 10.0)
         (agent-repl-debug nil))
     (cl-letf (((symbol-function 'agent-repl--uds-run-timer)
                (lambda (&rest _) 'fake-timer)))
       ,@body)))

(defmacro agent-repl-test--with-captured-deadline (thunk-var &rest body)
  "Run BODY capturing the ack-deadline alarm thunk into THUNK-VAR.
The timer seam is shadowed so no real timer is armed; calling THUNK-VAR is
what \"the deadline expired\" means in these tests."
  (declare (indent 1))
  `(let (,thunk-var)
     (cl-letf (((symbol-function 'agent-repl--uds-run-timer)
                (lambda (_delay fn) (setq ,thunk-var fn) 'fake-timer)))
       ,@body)))

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

(ert-deftest agent-repl-test-uds-new-dial-invalidates-prior-daemon-view ()
  "A new socket cannot satisfy readiness with the previous connection's view."
  (agent-repl-test--with-uds
    (let ((agent-repl--frontend-last-daemon-view '(:bootId "old-boot")))
      (cl-letf (((symbol-function 'agent-repl--uds-connect)
                 (lambda (&rest _) 'fake-proc))
                ((symbol-function 'process-live-p)
                 (lambda (p) (eq p 'fake-proc)))
                ((symbol-function 'process-name) (lambda (_p) "fake"))
                ((symbol-function 'process-status) (lambda (_p) 'open)))
        (agent-repl-uds-connect "/tmp/test.sock")
        (should-not agent-repl--frontend-last-daemon-view)))))

(ert-deftest agent-repl-test-uds-connect-already-connected-is-noop ()
  "Connect while already connected returns the existing process, no re-dial."
  ;; Arrange
  (agent-repl-test--with-uds
    (setq agent-repl--uds-process 'live-proc
          agent-repl--uds-connection-state 'open)
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

(ert-deftest agent-repl-test-uds-socket-live-p-reports-absent-when-probe-fails ()
  "Legacy liveness callers retain a boolean result until their migration."
  (agent-repl-test--with-uds
    (cl-letf (((symbol-function 'process-live-p) (lambda (_p) nil)))
      (should-not (agent-repl--uds-socket-live-p "/tmp/probe.sock")))))

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
     (let ((agent-repl--uds-connection-state 'open)
           (agent-repl--uds-outbound-queue nil))
       ,@body)))

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

(ert-deftest agent-repl-test-uds-send-command-withholds-frames-while-dialing ()
  "A dialing socket never receives frames before the `open' sentinel event."
  (agent-repl-test--with-uds
    (let (sent)
      (cl-letf (((symbol-function 'process-live-p) (lambda (_p) t))
                ((symbol-function 'process-name) (lambda (_p) "fake"))
                ((symbol-function 'process-send-string)
                 (lambda (_proc frame) (setq sent frame)))
                ((symbol-function 'agent-repl--uds-generate-request-id)
                 (lambda () "queued-1")))
        (let ((agent-repl--uds-process 'fake-proc)
              (agent-repl--uds-connection-state 'dialing)
              (agent-repl--uds-connect-started-at (float-time))
              (agent-repl--uds-outbound-queue nil))
          (agent-repl--uds-send-command "interrupt" '(:hard t) "ws1")
          (should-not sent)
          (should (= (length agent-repl--uds-outbound-queue) 1))
          (agent-repl--uds-sentinel 'fake-proc "open\n")
          (should (stringp sent)))))))

(ert-deftest agent-repl-test-uds-sentinel-open-runs-the-connected-hook ()
  "An `open' transition runs `agent-repl-uds-connected-functions'."
  ;; Arrange
  (agent-repl-test--with-uds
    (let ((ran 0))
      (cl-letf (((symbol-function 'process-live-p) (lambda (_p) t))
                ((symbol-function 'process-name) (lambda (_p) "fake")))
        (let ((agent-repl--uds-process 'fake-proc)
              (agent-repl--uds-connection-state 'dialing)
              (agent-repl--uds-connect-started-at (float-time))
              (agent-repl--uds-outbound-queue nil)
              (agent-repl-uds-connected-functions
               (list (lambda () (cl-incf ran)))))
          ;; Act
          (agent-repl--uds-sentinel 'fake-proc "open\n")
          ;; Assert
          (should (= ran 1)))))))

(ert-deftest agent-repl-test-uds-connected-hook-survives-a-failing-subscriber ()
  "One subscriber's failure is logged and the remaining subscribers still run.
Dropping the rest of a reconnect's recovery because its first step failed
would compound one outage into several."
  ;; Arrange
  (let ((ran nil)
        (logged nil))
    (cl-letf (((symbol-function 'agent-repl--log)
               (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logged))))
      (let ((agent-repl-uds-connected-functions
             (list (lambda () (error "subscriber blew up"))
                   (lambda () (setq ran t)))))
        ;; Act
        (agent-repl--uds-run-connected-hook)))
    ;; Assert
    (should ran)
    (should (cl-find-if (lambda (l) (string-match-p "subscriber blew up" l)) logged))))

(ert-deftest agent-repl-test-uds-snapshot-applied-hook-runs-its-subscribers ()
  "The snapshot-applied hook runs every subscriber."
  ;; Arrange
  (let ((ran 0))
    (let ((agent-repl-uds-snapshot-applied-functions
           (list (lambda () (cl-incf ran)) (lambda () (cl-incf ran)))))
      ;; Act
      (agent-repl--uds-run-snapshot-applied-hook))
    ;; Assert
    (should (= ran 2))))

(ert-deftest agent-repl-test-uds-snapshot-applied-hook-survives-a-failure ()
  "One subscriber's failure is logged and the remaining subscribers still run.
The same containment the connected hook keeps, from the same shared runner."
  ;; Arrange
  (let ((ran nil)
        (logged nil))
    (cl-letf (((symbol-function 'agent-repl--log)
               (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logged))))
      (let ((agent-repl-uds-snapshot-applied-functions
             (list (lambda () (error "subscriber blew up"))
                   (lambda () (setq ran t)))))
        ;; Act
        (agent-repl--uds-run-snapshot-applied-hook)))
    ;; Assert
    (should ran)
    (should (cl-find-if (lambda (l) (string-match-p "uds-snapshot-applied-hook" l)) logged))))

(ert-deftest agent-repl-test-uds-sentinel-open-never-runs-the-snapshot-hook ()
  "A socket that merely OPENED has not finished reconnecting.
The roster a snapshot subscriber reads is still empty there, which is why
the two lifecycle edges are two hooks."
  ;; Arrange
  (agent-repl-test--with-uds
    (let ((ran 0))
      (cl-letf (((symbol-function 'process-live-p) (lambda (_p) t))
                ((symbol-function 'process-name) (lambda (_p) "fake")))
        (let ((agent-repl--uds-process 'fake-proc)
              (agent-repl--uds-connection-state 'dialing)
              (agent-repl--uds-connect-started-at (float-time))
              (agent-repl--uds-outbound-queue nil)
              (agent-repl-uds-snapshot-applied-functions
               (list (lambda () (cl-incf ran)))))
          ;; Act
          (agent-repl--uds-sentinel 'fake-proc "open\n")
          ;; Assert
          (should (= ran 0)))))))

(ert-deftest agent-repl-test-uds-failed-dial-records-a-retractable-notice ()
  "A refused dial records its echoed notice so the reconnect can take it back."
  ;; Arrange
  (let ((echoed nil))
    (cl-letf (((symbol-function 'agent-repl--uds-connected-p) (lambda () nil))
              ((symbol-function 'agent-repl--frontend-invalidate-daemon-view) #'ignore)
              ((symbol-function 'agent-repl--uds-connect)
               (lambda (&rest _) (error "connection refused")))
              ((symbol-function 'agent-repl--uds-schedule-reconnect) #'ignore)
              ((symbol-function 'agent-repl-connection-notice-echo)
               (lambda (text) (setq echoed text))))
      ;; Act
      (agent-repl-uds-connect "/tmp/agent-repl-test.sock")
      ;; Assert
      (should (stringp echoed))
      (should (string-match-p "unreachable" echoed)))))

(ert-deftest agent-repl-test-uds-readiness-dial-records-no-notice ()
  "A cold-start dial under the readiness loop raises no outage alarm.
The readiness continuation owns retry pacing and the final hard error, so
a notice here would be an alarm about a startup that has not failed yet."
  ;; Arrange
  (let ((echoed nil))
    (cl-letf (((symbol-function 'agent-repl--uds-connected-p) (lambda () nil))
              ((symbol-function 'agent-repl--frontend-invalidate-daemon-view) #'ignore)
              ((symbol-function 'agent-repl--uds-connect)
               (lambda (&rest _) (error "connection refused")))
              ((symbol-function 'agent-repl-connection-notice-echo)
               (lambda (text) (setq echoed text))))
      ;; Act
      (agent-repl-uds-connect "/tmp/agent-repl-test.sock" t)
      ;; Assert
      (should-not echoed))))

(ert-deftest agent-repl-test-uds-publish-workspace-roster-is-sendable ()
  "The roster publish arm is a known, sendable command field."
  (should (member "publishWorkspaceRoster" agent-repl--uds-known-command-fields)))

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

(ert-deftest agent-repl-test-uds-workspace-roster-is-a-known-frame ()
  "The `workspaceRoster' broadcast echo is a recognized frame field.
The daemon broadcasts the roster to every connected client, including
the Emacs host client that published it, so an unregistered arm surfaced
as a user-visible malformed-frame error on every roster publish."
  ;; Act / Assert
  (should (member "workspaceRoster" agent-repl--uds-known-frame-fields)))

(ert-deftest agent-repl-test-uds-workspace-roster-is-a-deliberately-ignored-frame ()
  "`workspaceRoster' is declared ignored: Emacs is the roster's sole author."
  ;; Act / Assert
  (should (member "workspaceRoster" agent-repl--uds-ignored-frame-fields)))

(ert-deftest agent-repl-test-uds-dispatch-workspace-roster-frame-returns-nil ()
  "Dispatching a workspaceRoster frame is a no-op, not a signal."
  ;; Arrange
  (agent-repl-test--with-uds
    ;; Act / Assert
    (should-not (agent-repl--uds-dispatch-frame
                 '(:workspaceRoster (:workspaces [(:workspace "ws1")]))))))

(ert-deftest agent-repl-test-uds-dispatch-workspace-roster-invokes-no-handler ()
  "A workspaceRoster frame must reach no handler — the echo renders nothing."
  ;; Arrange
  (agent-repl-test--with-uds
    ;; Act
    (agent-repl--uds-dispatch-frame '(:workspaceRoster (:workspaces [])))
    ;; Assert
    (should-not (assoc "workspaceRoster" agent-repl--uds-frame-handlers))))

(ert-deftest agent-repl-test-uds-dispatch-workspace-roster-skips-unwired-warning ()
  "A workspaceRoster frame must NOT log the unfinished-wiring message."
  ;; Arrange
  (agent-repl-test--with-uds
    (let (logged)
      (cl-letf (((symbol-function 'agent-repl--log)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logged))))
        ;; Act
        (agent-repl--uds-dispatch-frame '(:workspaceRoster (:workspaces [])))
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

;;;; ---- Drain lease: frame vocabulary and command mirror ---------------

(ert-deftest agent-repl-test-uds-shutdown-schedule-is-a-known-frame ()
  "The drain lease's `shutdownSchedule' arm is a recognized frame field."
  ;; Act / Assert
  (should (member "shutdownSchedule" agent-repl--uds-known-frame-fields)))

(ert-deftest agent-repl-test-uds-shutdown-schedule-is-not-an-ignored-frame ()
  "`shutdownSchedule' is recorded, not ignored: the cancel needs its id."
  ;; Act / Assert
  (should-not (member "shutdownSchedule" agent-repl--uds-ignored-frame-fields)))

(ert-deftest agent-repl-test-uds-decode-shutdown-schedule-idle-arm ()
  "An `idle' lease decodes: the empty message is a present arm with a nil body."
  ;; Arrange
  (agent-repl-test--with-uds
    (let ((line "{\"shutdownSchedule\":{\"idle\":{}}}"))
      ;; Act
      (let ((frame (agent-repl--uds-decode-frame line)))
        ;; Assert
        (should (plist-member (plist-get frame :shutdownSchedule) :idle))))))

(ert-deftest agent-repl-test-uds-decode-shutdown-schedule-draining-arm ()
  "A `draining' lease decodes with its schedule id intact."
  ;; Arrange
  (agent-repl-test--with-uds
    (let ((line (concat "{\"shutdownSchedule\":{\"draining\":{"
                        "\"scheduleId\":\"sch-1\",\"cause\":\"merge\"}}}")))
      ;; Act
      (let ((frame (agent-repl--uds-decode-frame line)))
        ;; Assert
        (should (equal (plist-get (plist-get (plist-get frame :shutdownSchedule)
                                             :draining)
                                  :scheduleId)
                       "sch-1"))))))

(ert-deftest agent-repl-test-uds-dispatch-shutdown-schedule-reaches-its-handler ()
  "A shutdownSchedule frame dispatches to its registered handler, not a signal."
  ;; Arrange
  (agent-repl-test--with-uds
    (let (seen)
      (agent-repl--uds-register-handler "shutdownSchedule"
                                        (lambda (view) (setq seen view) :ok))
      ;; Act
      (let ((result (agent-repl--uds-dispatch-frame '(:shutdownSchedule (:idle nil)))))
        ;; Assert
        (should (eq result :ok))
        (should (equal seen '(:idle nil)))))))

(ert-deftest agent-repl-test-uds-dispatch-queue-frame-with-shutdown-hold-is-a-no-op ()
  "A queue entry held by the drain lease still decodes and is still ignored."
  ;; Arrange
  (agent-repl-test--with-uds
    ;; Act / Assert
    (should-not (agent-repl--uds-dispatch-frame
                 '(:queue (:workspace "ws1" :sessionId "s1"
                           :entries ((:id "q1" :text "hi"
                                      :shutdownHold (:scheduleId "sch-1")))))))))

(ert-deftest agent-repl-test-uds-schedule-shutdown-is-a-known-command ()
  "The drain lease's `scheduleShutdown' arm is an accepted outbound command."
  ;; Act / Assert
  (should (member "scheduleShutdown" agent-repl--uds-known-command-fields)))

(ert-deftest agent-repl-test-uds-cancel-scheduled-shutdown-is-a-known-command ()
  "The drain lease's `cancelScheduledShutdown' arm is an accepted command."
  ;; Act / Assert
  (should (member "cancelScheduledShutdown" agent-repl--uds-known-command-fields)))

(ert-deftest agent-repl-test-uds-send-command-accepts-schedule-shutdown ()
  "`scheduleShutdown' serializes its cause under the protojson field name."
  ;; Arrange
  (agent-repl-test--with-uds
    (let (sent)
      (agent-repl-test--capturing-send sent
        ;; Act
        (agent-repl--uds-send-command "scheduleShutdown"
                                      '(:cause "manual restart") nil 'fake-proc)
        ;; Assert
        (should (string-match-p "\"scheduleShutdown\":{\"cause\":\"manual restart\"}"
                                sent))))))

(ert-deftest agent-repl-test-uds-send-command-accepts-cancel-scheduled-shutdown ()
  "`cancelScheduledShutdown' serializes the schedule id it must match."
  ;; Arrange
  (agent-repl-test--with-uds
    (let (sent)
      (agent-repl-test--capturing-send sent
        ;; Act
        (agent-repl--uds-send-command "cancelScheduledShutdown"
                                      '(:scheduleId "sch-9") nil 'fake-proc)
        ;; Assert
        (should (string-match-p "\"cancelScheduledShutdown\":{\"scheduleId\":\"sch-9\"}"
                                sent))))))

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

;;;; ---- Ack aging + command-link health ---------------------------------

(ert-deftest agent-repl-test-uds-link-health-starts-healthy ()
  "The command link's initial state is `:healthy'."
  ;; Arrange / Act / Assert
  (agent-repl-test--with-uds
    (should (eq (agent-repl-uds-link-health) :healthy))))

(ert-deftest agent-repl-test-uds-link-health-reports-degraded ()
  "The health reader reports `:degraded' once the link has been degraded."
  ;; Arrange
  (agent-repl-test--with-uds
    ;; Act
    (agent-repl--uds-link-degrade "req-1" "mergeWorkspace" "ws1")
    ;; Assert
    (should (eq (agent-repl-uds-link-health) :degraded))))

(ert-deftest agent-repl-test-uds-track-command-arms-deadline-alarm ()
  "Tracking a command arms the ack alarm through the injectable timer seam."
  ;; Arrange
  (agent-repl-test--with-uds
    (let ((agent-repl-uds-command-ack-deadline 7.5)
          scheduled)
      (cl-letf (((symbol-function 'agent-repl--uds-run-timer)
                 (lambda (delay fn) (setq scheduled (list delay fn)) 'fake-timer)))
        ;; Act
        (agent-repl--uds-track-command "req-1" "mergeWorkspace" "ws1")
        ;; Assert
        (should (equal (nth 0 scheduled) 7.5))
        (should (functionp (nth 1 scheduled)))))))

(ert-deftest agent-repl-test-uds-deadline-expiry-warns-user ()
  "An unacked command past its deadline surfaces a user-visible warning."
  ;; Arrange
  (agent-repl-test--with-uds
    (agent-repl-test--with-captured-deadline expire
      (agent-repl--uds-track-command "req-1" "mergeWorkspace" "ws1")
      (let (echoed)
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args) (setq echoed (apply #'format fmt args)))))
          ;; Act
          (funcall expire)
          ;; Assert
          (should (string-match-p "mergeWorkspace" echoed))
          (should (string-match-p "never acknowledged" echoed)))))))

(ert-deftest agent-repl-test-uds-deadline-expiry-names-request-id ()
  "The deadline warning carries the request-id so the log can be correlated."
  ;; Arrange
  (agent-repl-test--with-uds
    (agent-repl-test--with-captured-deadline expire
      (agent-repl--uds-track-command "req-1" "mergeWorkspace" "ws1")
      (let (echoed)
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args) (setq echoed (apply #'format fmt args)))))
          ;; Act
          (funcall expire)
          ;; Assert
          (should (string-match-p "req-1" echoed)))))))

(ert-deftest agent-repl-test-uds-deadline-expiry-logs-field-and-workspace ()
  "The canonical log line names the request-id, field, and wire workspace."
  ;; Arrange
  (agent-repl-test--with-uds
    (agent-repl-test--with-captured-deadline expire
      (agent-repl--uds-track-command "req-1" "mergeWorkspace" "ws1")
      (let (logged)
        (cl-letf (((symbol-function 'message) (lambda (&rest _) nil))
                  ((symbol-function 'agent-repl--log)
                   (lambda (_ws fmt &rest args)
                     (push (apply #'format fmt args) logged))))
          ;; Act
          (funcall expire)
          ;; Assert
          (should (cl-find-if
                   (lambda (line)
                     (and (string-match-p "uds-ack-deadline: UNACKED" line)
                          (string-match-p "request-id=req-1" line)
                          (string-match-p "field=mergeWorkspace" line)
                          (string-match-p "ws=ws1" line)))
                   logged)))))))

(ert-deftest agent-repl-test-uds-deadline-expiry-degrades-link ()
  "An unacked command past its deadline flips the command link to degraded."
  ;; Arrange
  (agent-repl-test--with-uds
    (agent-repl-test--with-captured-deadline expire
      (agent-repl--uds-track-command "req-1" "mergeWorkspace" "ws1")
      (cl-letf (((symbol-function 'message) (lambda (&rest _) nil)))
        ;; Act
        (funcall expire)
        ;; Assert
        (should (eq (agent-repl-uds-link-health) :degraded))))))

(ert-deftest agent-repl-test-uds-deadline-expiry-marks-not-drops ()
  "A timed-out command is MARKED timed-out, never silently forgotten."
  ;; Arrange
  (agent-repl-test--with-uds
    (agent-repl-test--with-captured-deadline expire
      (agent-repl--uds-track-command "req-1" "mergeWorkspace" "ws1")
      (cl-letf (((symbol-function 'message) (lambda (&rest _) nil)))
        ;; Act
        (funcall expire)
        ;; Assert
        (should-not (gethash "req-1" agent-repl--uds-pending-commands))
        (should (equal (plist-get (gethash "req-1"
                                           agent-repl--uds-timed-out-commands)
                                  :field)
                       "mergeWorkspace"))))))

(ert-deftest agent-repl-test-uds-deadline-expiry-skips-on-failure-callback ()
  "A timeout never runs :on-failure — nothing was rejected, only unanswered."
  ;; Arrange
  (agent-repl-test--with-uds
    (agent-repl-test--with-captured-deadline expire
      (let (failed)
        (agent-repl--uds-track-command
         "req-1" "mergeWorkspace" "ws1" (lambda (_err) (setq failed t)))
        (cl-letf (((symbol-function 'message) (lambda (&rest _) nil)))
          ;; Act
          (funcall expire)
          ;; Assert
          (should-not failed))))))

(ert-deftest agent-repl-test-uds-deadline-after-ack-is-a-no-op ()
  "A deadline thunk that runs after its ack landed reports nothing."
  ;; Arrange
  (agent-repl-test--with-uds
    (agent-repl-test--with-captured-deadline expire
      (agent-repl--uds-track-command "req-1" "mergeWorkspace" "ws1")
      (agent-repl--uds-handle-command-ack '(:requestId "req-1" :ok t))
      (let (echoed)
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args) (setq echoed (apply #'format fmt args)))))
          ;; Act
          (funcall expire)
          ;; Assert
          (should-not echoed)
          (should (eq (agent-repl-uds-link-health) :healthy)))))))

(ert-deftest agent-repl-test-uds-ack-before-deadline-cancels-alarm ()
  "An ack inside the deadline disarms the alarm rather than leaving it live."
  ;; Arrange
  (agent-repl-test--with-uds
    (let (cancelled)
      (cl-letf (((symbol-function 'agent-repl--uds-run-timer)
                 (lambda (&rest _) 'armed-timer))
                ((symbol-function 'timerp) (lambda (tm) (eq tm 'armed-timer)))
                ((symbol-function 'cancel-timer)
                 (lambda (tm) (setq cancelled tm))))
        (agent-repl--uds-track-command "req-1" "mergeWorkspace" "ws1")
        ;; Act
        (agent-repl--uds-handle-command-ack '(:requestId "req-1" :ok t))
        ;; Assert
        (should (eq cancelled 'armed-timer))))))

(ert-deftest agent-repl-test-uds-ack-before-deadline-leaves-health-clean ()
  "An ack inside the deadline leaves the command link healthy."
  ;; Arrange
  (agent-repl-test--with-uds
    (agent-repl--uds-track-command "req-1" "mergeWorkspace" "ws1")
    ;; Act
    (agent-repl--uds-handle-command-ack '(:requestId "req-1" :ok t))
    ;; Assert
    (should (eq (agent-repl-uds-link-health) :healthy))))

(ert-deftest agent-repl-test-uds-untrack-command-cancels-alarm ()
  "Untracking a command after a local wait aborts also disarms its alarm."
  ;; Arrange
  (agent-repl-test--with-uds
    (let (cancelled)
      (cl-letf (((symbol-function 'agent-repl--uds-run-timer)
                 (lambda (&rest _) 'armed-timer))
                ((symbol-function 'timerp) (lambda (tm) (eq tm 'armed-timer)))
                ((symbol-function 'cancel-timer)
                 (lambda (tm) (setq cancelled tm))))
        (agent-repl--uds-track-command "req-1" "mergeWorkspace" "ws1")
        ;; Act
        (agent-repl--uds-untrack-command "req-1" "ws1" "local-wait-aborted")
        ;; Assert
        (should (eq cancelled 'armed-timer))))))

(ert-deftest agent-repl-test-uds-late-ack-does-not-error ()
  "An ack arriving after its timeout is tolerated, never an error."
  ;; Arrange
  (agent-repl-test--with-uds
    (agent-repl-test--with-captured-deadline expire
      (agent-repl--uds-track-command "req-1" "mergeWorkspace" "ws1")
      (cl-letf (((symbol-function 'message) (lambda (&rest _) nil)))
        (funcall expire)
        ;; Act / Assert
        (should (eq (agent-repl--uds-handle-command-ack
                     '(:requestId "req-1" :ok t))
                    t))))))

(ert-deftest agent-repl-test-uds-late-ack-logs-late-ack-after-timeout ()
  "A late ack is logged as `late ack after timeout', not as untracked."
  ;; Arrange
  (agent-repl-test--with-uds
    (agent-repl-test--with-captured-deadline expire
      (agent-repl--uds-track-command "req-1" "mergeWorkspace" "ws1")
      (cl-letf (((symbol-function 'message) (lambda (&rest _) nil)))
        (funcall expire))
      (let (logged)
        (cl-letf (((symbol-function 'agent-repl--log)
                   (lambda (_ws fmt &rest args)
                     (push (apply #'format fmt args) logged))))
          ;; Act
          (agent-repl--uds-handle-command-ack '(:requestId "req-1" :ok t))
          ;; Assert
          (should (cl-find-if
                   (lambda (line)
                     (and (string-match-p "late ack after timeout" line)
                          (string-match-p "field=mergeWorkspace" line)))
                   logged)))))))

(ert-deftest agent-repl-test-uds-late-ack-clears-the-timed-out-record ()
  "A late ack consumes its timed-out record so it cannot be reported twice."
  ;; Arrange
  (agent-repl-test--with-uds
    (agent-repl-test--with-captured-deadline expire
      (agent-repl--uds-track-command "req-1" "mergeWorkspace" "ws1")
      (cl-letf (((symbol-function 'message) (lambda (&rest _) nil)))
        (funcall expire))
      ;; Act
      (agent-repl--uds-handle-command-ack '(:requestId "req-1" :ok t))
      ;; Assert
      (should-not (gethash "req-1" agent-repl--uds-timed-out-commands)))))

(ert-deftest agent-repl-test-uds-late-ack-leaves-link-degraded ()
  "A late ack does not clear degradation: its deadline really was missed."
  ;; Arrange
  (agent-repl-test--with-uds
    (agent-repl-test--with-captured-deadline expire
      (agent-repl--uds-track-command "req-1" "mergeWorkspace" "ws1")
      (cl-letf (((symbol-function 'message) (lambda (&rest _) nil)))
        (funcall expire))
      ;; Act
      (agent-repl--uds-handle-command-ack '(:requestId "req-1" :ok t))
      ;; Assert
      (should (eq (agent-repl-uds-link-health) :degraded)))))

(ert-deftest agent-repl-test-uds-next-successful-ack-restores-health ()
  "The next in-deadline ack after a timeout restores the link to healthy."
  ;; Arrange
  (agent-repl-test--with-uds
    (agent-repl-test--with-captured-deadline expire
      (agent-repl--uds-track-command "req-1" "mergeWorkspace" "ws1")
      (cl-letf (((symbol-function 'message) (lambda (&rest _) nil)))
        (funcall expire))
      (agent-repl--uds-track-command "req-2" "mergeWorkspace" "ws1")
      ;; Act
      (agent-repl--uds-handle-command-ack '(:requestId "req-2" :ok t))
      ;; Assert
      (should (eq (agent-repl-uds-link-health) :healthy)))))

(ert-deftest agent-repl-test-uds-rejected-ack-restores-health ()
  "A REJECTED ack still proves the link carried traffic, so health returns."
  ;; Arrange
  (agent-repl-test--with-uds
    (agent-repl-test--with-captured-deadline expire
      (agent-repl--uds-track-command "req-1" "mergeWorkspace" "ws1")
      (cl-letf (((symbol-function 'message) (lambda (&rest _) nil)))
        (funcall expire)
        (agent-repl--uds-track-command "req-2" "mergeWorkspace" "ws1")
        ;; Act
        (agent-repl--uds-handle-command-ack
         '(:requestId "req-2" :error "branch not found"))
        ;; Assert
        (should (eq (agent-repl-uds-link-health) :healthy))))))

(ert-deftest agent-repl-test-uds-untracked-ack-leaves-link-degraded ()
  "An ack for a request nobody tracked is no proof about the command plane."
  ;; Arrange
  (agent-repl-test--with-uds
    (agent-repl--uds-link-degrade "req-1" "mergeWorkspace" "ws1")
    ;; Act
    (agent-repl--uds-handle-command-ack '(:requestId "req-9" :ok t))
    ;; Assert
    (should (eq (agent-repl-uds-link-health) :degraded))))

(ert-deftest agent-repl-test-uds-reconnect-restores-health ()
  "A successful reconnect restores the command link to healthy."
  ;; Arrange
  (agent-repl-test--with-uds
    (agent-repl--uds-link-degrade "req-1" "mergeWorkspace" "ws1")
    (cl-letf (((symbol-function 'agent-repl--uds-connect)
               (lambda (&rest _) 'fake-proc))
              ((symbol-function 'process-name) (lambda (_p) "fake"))
              ((symbol-function 'process-status) (lambda (_p) 'open)))
      ;; Act
      (agent-repl-uds-connect)
      ;; Assert
      (should (eq (agent-repl-uds-link-health) :healthy)))))

(ert-deftest agent-repl-test-uds-command-unacked-is-a-local-failure-type ()
  "The timeout's failure type belongs to the closed local vocabulary."
  ;; Arrange / Act / Assert
  (should (member "client.command_unacked" agent-repl-failure-local-types)))

(provide 'test-frontend-uds)

;;; test-frontend-uds.el ends here

(ert-deftest agent-repl-test-uds-send-reconciles-state-desync-over-open-process ()
  "A live `open' process with a stale non-open state variable sends anyway.
The state variable is bookkeeping and the process is reality: refusing to
talk over a working connection because the variable went stale (observed
after a daemon bounce) is the dishonest option.  The reconciliation is
loud and flips the variable back to `open'."
  (agent-repl-test--with-uds
    (let (sent)
      (cl-letf (((symbol-function 'process-live-p)
                 (lambda (p) (eq p 'fake-proc)))
                ((symbol-function 'process-status)
                 (lambda (_p) 'open))
                ((symbol-function 'agent-repl--uds-generate-request-id)
                 (lambda () "req-fixed"))
                ((symbol-function 'process-send-string)
                 (lambda (_proc s) (setq sent s))))
        (let ((agent-repl--uds-connection-state 'failed)
              (agent-repl--uds-outbound-queue nil))
          ;; Act
          (agent-repl--uds-send-command "interrupt" '(:hard t) "ws1" 'fake-proc)
          ;; Assert — the frame went out and the state reconciled.
          (should sent)
          (should (eq agent-repl--uds-connection-state 'open)))))))

(ert-deftest agent-repl-test-uds-send-still-refuses-a-dead-process ()
  "Reconciliation never resurrects a genuinely dead connection."
  (agent-repl-test--with-uds
    (cl-letf (((symbol-function 'process-live-p) (lambda (_p) nil)))
      (let ((agent-repl--uds-connection-state 'failed))
        ;; Act + Assert
        (should-error
         (agent-repl--uds-send-command "interrupt" '(:hard t) "ws1" 'dead-proc)
         :type 'user-error)))))

(ert-deftest agent-repl-test-uds-sentinel-open-survives-nil-started-at ()
  "A duplicate/racing `open' sentinel event with no start timestamp must not
error inside the sentinel — that silent skip is what stranded the
connection state over an open socket."
  (agent-repl-test--with-uds
    (cl-letf (((symbol-function 'process-live-p) (lambda (_p) t))
              ((symbol-function 'process-name) (lambda (_p) "fake")))
      (let ((agent-repl--uds-process 'fake-proc)
            (agent-repl--uds-connection-state 'dialing)
            (agent-repl--uds-connect-started-at nil)
            (agent-repl--uds-outbound-queue nil))
        ;; Act — must not signal.
        (agent-repl--uds-sentinel 'fake-proc "open\n")
        ;; Assert
        (should (eq agent-repl--uds-connection-state 'open))))))
