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

(defun agent-repl-test--uds-send-command-literal-fields ()
  "Return every literal FIELD name passed to `agent-repl--uds-send-command'.
Scans the module's non-test `lisp/*.el' sources for the call form and
collects its first argument whenever that argument is a string literal.
Computed fields (a variable, a `format' call) are skipped: only a literal
can be checked against the allowlist without running the caller."
  (let ((re (concat "(agent-repl--uds-send-command[ \t\n]+"
                    "\"\\([A-Za-z][A-Za-z0-9]*\\)\""))
        fields)
    (dolist (file (directory-files
                   (expand-file-name "lisp" agent-repl-test--module-root)
                   t "\\`[^.].*\\.el\\'"))
      (unless (string-prefix-p "test-" (file-name-nondirectory file))
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (while (re-search-forward re nil t)
            (push (match-string 1) fields)))))
    (delete-dups fields)))

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
         (agent-repl-uds-reconnect-max-delay 30.0)
         (agent-repl--uds-reconnect-delay-current nil)
         (agent-repl--uds-reconnect-attempts 0)
         (agent-repl-uds-command-ack-deadline 10.0)
         (agent-repl-debug nil))
     (cl-letf (((symbol-function 'agent-repl--uds-run-timer)
                (lambda (&rest _) 'fake-timer))
               ;; The filesystem check the dial is gated on is an external
               ;; boundary; the default arrangement is \"the socket file is
               ;; there\", which is what every pre-gate dial test assumed.
               ;; Gate tests shadow it again with their own `cl-letf', which
               ;; wins.
               ((symbol-function 'agent-repl--uds-socket-file-present-p)
                (lambda (&rest _) t)))
       ,@body)))

(defmacro agent-repl-test--with-connected-link (&rest body)
  "Run BODY with the command link ARRANGED as connected on `fake-proc'.

`agent-repl-uds-link-health' reports `:degraded' over a link whose process
is not connected, so an assertion about the ACK latch is only about the
latch when a live link is arranged first — without this the latch
assertions would pass on the link-down verdict instead.

`fake-proc' is the same stub the send/connect tests already use, so a test
that shadows `process-live-p' again for `fake-proc' stays consistent with
this arrangement rather than undoing it.  Link-DOWN tests simply omit this
wrapper."
  (declare (indent 0))
  `(let ((agent-repl--uds-process 'fake-proc)
         (agent-repl--uds-connection-state 'open))
     (cl-letf (((symbol-function 'process-live-p)
                (lambda (proc) (eq proc 'fake-proc))))
       ,@body)))

(defun agent-repl-test--pend (request-id field workspace
                                         &optional on-failure on-success
                                         on-challenge on-timeout)
  "ARRANGE a pending command entry the way a real send would.

`agent-repl--uds-register-pending-command' is private to
`agent-repl--uds-send-command' — there is deliberately no public tracker —
so the ack-machinery tests below reach it through this one helper rather
than reproducing the send-then-track shape the transport now forbids.
Tests of the SEND path drive `agent-repl--uds-send-command' itself."
  (agent-repl--uds-register-pending-command
   request-id field workspace on-failure on-success on-challenge on-timeout))

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

;;;; ---- dispatch: the recovery SLO's wire evidence ----------------------
;;
;; THE PIN.  The wire stamp used to fire for EVERY arm that carried a
;; `workspace', so the evidence was whichever frame happened to arrive first —
;; in practice a `typingDelta', because a workspace mid-turn emits hundreds a
;; second.  A workspace that simply was not typing had no wire evidence at all,
;; and the day a daemon-side fold silenced typing inside async windows, every
;; affected workspace started reporting `wire_ms=-1'.  These two tests are the
;; before/after of that rule at the dispatch point.

(defmacro agent-repl-test--capturing-wire-stamps (var &rest body)
  "Run BODY with wire stamps captured into VAR instead of reaching the SLO."
  (declare (indent 1))
  `(let ((,var nil))
     (cl-letf (((symbol-function 'agent-repl--recovery-slo-note)
                (lambda (ws signal) (push (cons signal ws) ,var)))
               ((symbol-function 'agent-repl--frontend-ws-name) #'identity))
       ,@body)))

(ert-deftest agent-repl-test-uds-dispatch-typing-delta-is-not-wire-evidence ()
  "An incidental `typingDelta' does not stand in for the guaranteed carrier."
  ;; Arrange
  (agent-repl-test--with-uds
    (agent-repl-test--capturing-wire-stamps stamps
      ;; Act
      (agent-repl--uds-dispatch-frame '(:typingDelta (:workspace "ws1")))
      ;; Assert
      (should (null stamps)))))

(ert-deftest agent-repl-test-uds-dispatch-workspace-state-is-wire-evidence ()
  "A `workspaceState' — which the daemon owes every workspace — stamps wire."
  ;; Arrange
  (agent-repl-test--with-uds
    (agent-repl-test--capturing-wire-stamps stamps
      ;; Act
      (agent-repl--uds-dispatch-frame '(:workspaceState (:workspace "ws1")))
      ;; Assert
      (should (equal stamps '((wire . "ws1")))))))

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

;;;; ---- filter: a failing handler cannot stall the batch -----------------
;;
;; One chunk routinely carries frames for several workspaces.  A handler that
;; signals must not abandon the drain: the complete frames behind it are
;; already whole in the accumulator and would otherwise wait for the next
;; chunk.  The batch drains, then the first failure is re-signalled — the
;; error is contained for the length of the batch, never swallowed.

(defconst agent-repl-test--uds-failing-chunk
  (concat "{\"workspaceState\":{\"workspace\":\"bad\"}}\n"
          "{\"snapshot\":{\"workspace\":\"good\"}}\n")
  "A two-frame chunk whose FIRST frame's handler is made to signal.")

(ert-deftest agent-repl-test-uds-filter-dispatches-frames-behind-a-failing-handler ()
  "A signalling handler does not stop the frames behind it from dispatching."
  ;; Arrange
  (agent-repl-test--with-uds
    (let (reached)
      (agent-repl--uds-register-handler
       "workspaceState" (lambda (_p) (error "handler blew up")))
      (agent-repl--uds-register-handler "snapshot" (lambda (_p) (setq reached t)))
      (cl-letf (((symbol-function 'agent-repl--log) (lambda (&rest _) nil)))
        ;; Act
        (ignore-errors
          (agent-repl--uds-filter nil agent-repl-test--uds-failing-chunk))
        ;; Assert
        (should reached)))))

(ert-deftest agent-repl-test-uds-filter-re-signals-a-handler-failure ()
  "The contained failure is raised again once the drain completes."
  ;; Arrange
  (agent-repl-test--with-uds
    (agent-repl--uds-register-handler
     "workspaceState" (lambda (_p) (error "handler blew up")))
    (agent-repl--uds-register-handler "snapshot" #'ignore)
    (cl-letf (((symbol-function 'agent-repl--log) (lambda (&rest _) nil)))
      ;; Act / Assert
      (should-error
       (agent-repl--uds-filter nil agent-repl-test--uds-failing-chunk)
       :type 'error))))

(ert-deftest agent-repl-test-uds-filter-re-signals-the-first-failure ()
  "When two handlers fail, the FIRST failure is the one raised."
  ;; Arrange
  (agent-repl-test--with-uds
    (agent-repl--uds-register-handler
     "workspaceState" (lambda (_p) (error "first")))
    (agent-repl--uds-register-handler
     "snapshot" (lambda (_p) (error "second")))
    (cl-letf (((symbol-function 'agent-repl--log) (lambda (&rest _) nil)))
      (let ((err (should-error
                  (agent-repl--uds-filter nil agent-repl-test--uds-failing-chunk))))
        ;; Assert
        (should (equal (cadr err) "first"))))))

(ert-deftest agent-repl-test-uds-filter-drains-the-accumulator-past-a-failure ()
  "The batch is consumed to the end, so no complete frame is left buffered."
  ;; Arrange
  (agent-repl-test--with-uds
    (agent-repl--uds-register-handler
     "workspaceState" (lambda (_p) (error "handler blew up")))
    (agent-repl--uds-register-handler "snapshot" #'ignore)
    (cl-letf (((symbol-function 'agent-repl--log) (lambda (&rest _) nil)))
      ;; Act
      (ignore-errors
        (agent-repl--uds-filter nil agent-repl-test--uds-failing-chunk))
      ;; Assert
      (should (string-empty-p agent-repl--uds-read-accumulator)))))

(ert-deftest agent-repl-test-uds-filter-logs-the-failing-frame ()
  "The contained failure is loud-logged with the arm and workspace that caused it."
  ;; Arrange
  (agent-repl-test--with-uds
    (let (logged)
      (agent-repl--uds-register-handler
       "workspaceState" (lambda (_p) (error "handler blew up")))
      (agent-repl--uds-register-handler "snapshot" #'ignore)
      (cl-letf (((symbol-function 'agent-repl--log)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logged))))
        ;; Act
        (ignore-errors
          (agent-repl--uds-filter nil agent-repl-test--uds-failing-chunk))
        ;; Assert
        (should (cl-some
                 (lambda (l)
                   (and (string-match-p "HANDLER FAILED field=workspaceState" l)
                        (string-match-p "workspace=\"bad\"" l)))
                 logged))))))

(ert-deftest agent-repl-test-uds-dispatch-outside-a-drain-propagates-the-failure ()
  "A dispatch with no drain to re-signal for it lets the handler error escape.
Containment is scoped to the batch; outside one there is nobody to raise
the recorded error, so containing it there would be swallowing it."
  ;; Arrange
  (agent-repl-test--with-uds
    (agent-repl--uds-register-handler
     "workspaceState" (lambda (_p) (error "handler blew up")))
    (cl-letf (((symbol-function 'agent-repl--log) (lambda (&rest _) nil)))
      ;; Act / Assert
      (should-error
       (agent-repl--uds-dispatch-frame '(:workspaceState (:workspace "bad")))
       :type 'error))))

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

(ert-deftest agent-repl-test-uds-born-dead-dial-schedules-another-reconnect ()
  "A dial whose process comes back already `failed' arms the next rung.
No sentinel transition follows a process that was never live, so without
this the ladder would end at its first attempt and the link would stay
down for the whole daemon outage."
  ;; Arrange
  (agent-repl-test--with-uds
    (let (scheduled)
      (cl-letf (((symbol-function 'processp) (lambda (p) (eq p 'dead-dial)))
                ((symbol-function 'process-status) (lambda (_p) 'failed))
                ((symbol-function 'process-name) (lambda (_p) "dead-dial"))
                ((symbol-function 'process-live-p) (lambda (_p) nil))
                ((symbol-function 'delete-process) #'ignore)
                ((symbol-function 'agent-repl--uds-connect)
                 (lambda (&rest _) 'dead-dial))
                ((symbol-function 'agent-repl--uds-run-timer)
                 (lambda (delay fn) (setq scheduled (list delay fn)) 'fake-timer)))
        ;; Act
        (let ((result (agent-repl-uds-connect "/tmp/x.sock")))
          ;; Assert
          (should-not result)
          (should-not agent-repl--uds-process)
          (should (equal (nth 0 scheduled) 2.0))
          (should (eq (nth 1 scheduled) #'agent-repl-uds-connect)))))))

;;;; ---- socket-existence gate -------------------------------------------

(ert-deftest agent-repl-test-uds-absent-socket-skips-the-doomed-spawn ()
  "No socket FILE means no dial: the doomed process is never spawned."
  ;; Arrange
  (agent-repl-test--with-uds
    (let ((dials 0))
      (cl-letf (((symbol-function 'agent-repl--uds-socket-file-present-p)
                 (lambda (&rest _) nil))
                ((symbol-function 'agent-repl--uds-connect)
                 (lambda (&rest _) (cl-incf dials) 'fake-proc))
                ((symbol-function 'agent-repl--uds-run-timer)
                 (lambda (&rest _) 'fake-timer)))
        ;; Act
        (let ((result (agent-repl-uds-connect "/tmp/absent.sock")))
          ;; Assert
          (should-not result)
          (should (= dials 0))
          (should-not agent-repl--uds-process))))))

(ert-deftest agent-repl-test-uds-absent-socket-retries-through-the-ladder ()
  "The absent-socket retry is the EXISTING ladder, not a second mechanism."
  ;; Arrange
  (agent-repl-test--with-uds
    (let (scheduled)
      (cl-letf (((symbol-function 'agent-repl--uds-socket-file-present-p)
                 (lambda (&rest _) nil))
                ((symbol-function 'agent-repl--uds-connect)
                 (lambda (&rest _) (error "must not dial")))
                ((symbol-function 'agent-repl--uds-run-timer)
                 (lambda (delay fn) (setq scheduled (list delay fn)) 'fake-timer)))
        ;; Act
        (agent-repl-uds-connect "/tmp/absent.sock")
        ;; Assert
        (should (equal (nth 0 scheduled) 2.0))
        (should (eq (nth 1 scheduled) #'agent-repl-uds-connect))
        (should (eq agent-repl--uds-reconnect-timer 'fake-timer))))))

(ert-deftest agent-repl-test-uds-absent-socket-does-not-warn ()
  "A daemon that has not finished coming up is not a warning-worthy fact."
  ;; Arrange
  (agent-repl-test--with-uds
    (let (warned)
      (cl-letf (((symbol-function 'agent-repl--uds-socket-file-present-p)
                 (lambda (&rest _) nil))
                ((symbol-function 'agent-repl--uds-connect)
                 (lambda (&rest _) (error "must not dial")))
                ((symbol-function 'agent-repl--warn)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) warned)))
                ((symbol-function 'agent-repl--uds-run-timer)
                 (lambda (&rest _) 'fake-timer)))
        ;; Act
        (agent-repl-uds-connect "/tmp/absent.sock")
        ;; Assert
        (should-not warned)))))

(ert-deftest agent-repl-test-uds-absent-socket-under-readiness-arms-no-timer ()
  "The readiness loop keeps sole ownership of retry pacing across the gate."
  ;; Arrange
  (agent-repl-test--with-uds
    (let (scheduled)
      (cl-letf (((symbol-function 'agent-repl--uds-socket-file-present-p)
                 (lambda (&rest _) nil))
                ((symbol-function 'agent-repl--uds-connect)
                 (lambda (&rest _) (error "must not dial")))
                ((symbol-function 'agent-repl--uds-run-timer)
                 (lambda (&rest args) (setq scheduled args) 'fake-timer)))
        ;; Act
        (should-not (agent-repl-uds-connect "/tmp/absent.sock" t))
        ;; Assert
        (should-not scheduled)
        (should-not agent-repl--uds-reconnect-timer)))))

(ert-deftest agent-repl-test-uds-present-socket-dials ()
  "An existing socket FILE dials exactly as before the gate."
  ;; Arrange
  (agent-repl-test--with-uds
    (let (dialed)
      (cl-letf (((symbol-function 'agent-repl--uds-socket-file-present-p)
                 (lambda (&rest _) t))
                ((symbol-function 'agent-repl--uds-connect)
                 (lambda (path &rest _) (setq dialed path) 'fake-proc))
                ((symbol-function 'process-live-p) (lambda (p) (eq p 'fake-proc)))
                ((symbol-function 'process-name) (lambda (_p) "fake"))
                ((symbol-function 'process-status) (lambda (_p) 'open)))
        ;; Act
        (let ((result (agent-repl-uds-connect "/tmp/present.sock")))
          ;; Assert
          (should (eq result 'fake-proc))
          (should (equal dialed "/tmp/present.sock")))))))

(ert-deftest agent-repl-test-uds-absent-socket-still-escalates-the-ladder ()
  "A socket that NEVER appears keeps climbing the ladder's backoff."
  ;; Arrange
  (agent-repl-test--with-uds
    (let (delays)
      (cl-letf (((symbol-function 'agent-repl--uds-socket-file-present-p)
                 (lambda (&rest _) nil))
                ((symbol-function 'agent-repl--uds-connect)
                 (lambda (&rest _) (error "must not dial")))
                ((symbol-function 'agent-repl--uds-run-timer)
                 (lambda (delay _fn) (push delay delays) 'fake-timer)))
        ;; Act
        (agent-repl-uds-connect "/tmp/absent.sock")
        (agent-repl-uds-connect "/tmp/absent.sock")
        (agent-repl-uds-connect "/tmp/absent.sock")
        ;; Assert
        (should (equal (nreverse delays) '(2.0 4.0 8.0)))
        (should (= agent-repl--uds-reconnect-attempts 3))))))

(ert-deftest agent-repl-test-uds-born-dead-warns-when-the-socket-existed ()
  "The born-dead path survives the gate for the check-then-dial race."
  ;; Arrange — the file was there at the check and gone by the dial.
  (agent-repl-test--with-uds
    (let (warned)
      (cl-letf (((symbol-function 'agent-repl--uds-socket-file-present-p)
                 (lambda (&rest _) t))
                ((symbol-function 'processp) (lambda (p) (eq p 'dead-dial)))
                ((symbol-function 'process-status) (lambda (_p) 'failed))
                ((symbol-function 'process-name) (lambda (_p) "dead-dial"))
                ((symbol-function 'process-live-p) (lambda (_p) nil))
                ((symbol-function 'delete-process) #'ignore)
                ((symbol-function 'agent-repl--uds-connect)
                 (lambda (&rest _) 'dead-dial))
                ((symbol-function 'agent-repl--warn)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) warned)))
                ((symbol-function 'agent-repl--uds-run-timer)
                 (lambda (&rest _) 'fake-timer)))
        ;; Act
        (should-not (agent-repl-uds-connect "/tmp/present.sock"))
        ;; Assert
        (should (string-match-p "dial born dead" (car warned)))))))

(ert-deftest agent-repl-test-uds-reconnect-backoff-doubles ()
  "Each successive scheduled reconnect waits twice as long as the last."
  ;; Arrange
  (agent-repl-test--with-uds
    (let (delays)
      (cl-letf (((symbol-function 'agent-repl--uds-run-timer)
                 (lambda (delay _fn) (push delay delays) 'fake-timer)))
        ;; Act
        (agent-repl--uds-schedule-reconnect "t1")
        (agent-repl--uds-schedule-reconnect "t2")
        (agent-repl--uds-schedule-reconnect "t3")
        ;; Assert
        (should (equal (nreverse delays) '(2.0 4.0 8.0)))))))

(ert-deftest agent-repl-test-uds-reconnect-backoff-stops-at-the-cap ()
  "The doubling ladder never schedules beyond the configured maximum."
  ;; Arrange
  (agent-repl-test--with-uds
    (let ((agent-repl-uds-reconnect-max-delay 8.0)
          delays)
      (cl-letf (((symbol-function 'agent-repl--uds-run-timer)
                 (lambda (delay _fn) (push delay delays) 'fake-timer)))
        ;; Act
        (dotimes (_ 6) (agent-repl--uds-schedule-reconnect "cap"))
        ;; Assert
        (should (equal (nreverse delays) '(2.0 4.0 8.0 8.0 8.0 8.0)))))))

(ert-deftest agent-repl-test-uds-open-link-resets-reconnect-backoff ()
  "A connection reaching `open' returns the ladder to its base delay."
  ;; Arrange — climb the ladder, then open the link.
  (agent-repl-test--with-uds
    (let (delays)
      (cl-letf (((symbol-function 'agent-repl--uds-run-timer)
                 (lambda (delay _fn) (push delay delays) 'fake-timer))
                ((symbol-function 'process-live-p) (lambda (_p) t))
                ((symbol-function 'process-name) (lambda (_p) "uds<1>"))
                ((symbol-function 'agent-repl--uds-run-connected-hook) #'ignore))
        (agent-repl--uds-schedule-reconnect "t1")
        (agent-repl--uds-schedule-reconnect "t2")
        (setq agent-repl--uds-process 'live-proc
              agent-repl--uds-connect-started-at (float-time))
        ;; Act
        (agent-repl--uds-sentinel 'live-proc "open from /tmp/x.sock\n")
        (setq delays nil)
        (agent-repl--uds-schedule-reconnect "after-open")
        ;; Assert
        (should (equal delays '(2.0)))))))

(ert-deftest agent-repl-test-uds-disconnect-cancels-the-pending-chain ()
  "A deliberate disconnect cancels the pending reconnect, leaving no zombie."
  ;; Arrange
  (agent-repl-test--with-uds
    (let ((cancelled 0))
      (cl-letf (((symbol-function 'agent-repl--uds-run-timer)
                 (lambda (&rest _) 'pending-timer))
                ((symbol-function 'timerp) (lambda (tm) (eq tm 'pending-timer)))
                ((symbol-function 'cancel-timer)
                 (lambda (_tm) (cl-incf cancelled)))
                ((symbol-function 'process-live-p) (lambda (_p) nil)))
        (agent-repl--uds-schedule-reconnect "outage")
        ;; Act
        (agent-repl-uds-disconnect)
        ;; Assert
        (should (= cancelled 1))
        (should-not agent-repl--uds-reconnect-timer)))))

(ert-deftest agent-repl-test-uds-disconnect-resets-reconnect-backoff ()
  "A deliberate disconnect ends the outage, so the ladder returns to base."
  ;; Arrange
  (agent-repl-test--with-uds
    (let (delays)
      (cl-letf (((symbol-function 'agent-repl--uds-run-timer)
                 (lambda (delay _fn) (push delay delays) 'fake-timer))
                ((symbol-function 'process-live-p) (lambda (_p) nil)))
        (agent-repl--uds-schedule-reconnect "t1")
        (agent-repl--uds-schedule-reconnect "t2")
        ;; Act
        (agent-repl-uds-disconnect)
        (setq delays nil)
        (agent-repl--uds-schedule-reconnect "after-disconnect")
        ;; Assert
        (should (equal delays '(2.0)))))))

(ert-deftest agent-repl-test-uds-schedule-reconnect-replaces-a-pending-timer ()
  "Scheduling over a pending reconnect cancels it rather than stacking one."
  ;; Arrange
  (agent-repl-test--with-uds
    (let ((cancelled 0)
          (armed 0))
      (cl-letf (((symbol-function 'agent-repl--uds-run-timer)
                 (lambda (&rest _) (cl-incf armed) 'pending-timer))
                ((symbol-function 'timerp) (lambda (tm) (eq tm 'pending-timer)))
                ((symbol-function 'cancel-timer)
                 (lambda (_tm) (cl-incf cancelled))))
        (agent-repl--uds-schedule-reconnect "first")
        ;; Act
        (agent-repl--uds-schedule-reconnect "second")
        ;; Assert — two arms, but the first was cancelled, so one is pending.
        (should (= armed 2))
        (should (= cancelled 1))))))

(ert-deftest agent-repl-test-uds-connect-cancels-the-timer-it-supersedes ()
  "A dial starting clears the pending-reconnect flag other drivers test."
  ;; Arrange
  (agent-repl-test--with-uds
    (setq agent-repl--uds-reconnect-timer 'pending-timer)
    (cl-letf (((symbol-function 'timerp) (lambda (tm) (eq tm 'pending-timer)))
              ((symbol-function 'cancel-timer) #'ignore)
              ((symbol-function 'process-live-p) (lambda (_p) nil))
              ((symbol-function 'process-status) (lambda (_p) 'connect))
              ((symbol-function 'process-name) (lambda (_p) "uds<1>"))
              ((symbol-function 'agent-repl--uds-connect)
               (lambda (&rest _) 'fake-proc)))
      ;; Act
      (agent-repl-uds-connect "/tmp/x.sock")
      ;; Assert
      (should-not agent-repl--uds-reconnect-timer))))

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

(ert-deftest agent-repl-test-uds-sentinel-established-link-loss-says-link-down ()
  "Losing an OPEN link is reported as a link loss, not as a dial failure."
  ;; Arrange — a cleared started-at is what an `open' transition leaves behind.
  (agent-repl-test--with-uds
    (setq agent-repl--uds-process 'dead-proc
          agent-repl--uds-connect-started-at nil)
    (let (warned)
      (cl-letf (((symbol-function 'process-live-p) (lambda (_p) nil))
                ((symbol-function 'process-name) (lambda (_p) "uds<8>"))
                ((symbol-function 'agent-repl--warn)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) warned)))
                ((symbol-function 'agent-repl--uds-run-timer)
                 (lambda (&rest _) 'fake-timer)))
        ;; Act
        (agent-repl--uds-sentinel 'dead-proc "connection broken by remote peer\n")
        ;; Assert
        (should (string-match-p "uds-link: DOWN" (car warned)))))))

(ert-deftest agent-repl-test-uds-sentinel-established-link-loss-omits-nil-elapsed ()
  "A link loss never reports `elapsed=nil', which read as an instant dial failure."
  ;; Arrange
  (agent-repl-test--with-uds
    (setq agent-repl--uds-process 'dead-proc
          agent-repl--uds-connect-started-at nil)
    (let (warned)
      (cl-letf (((symbol-function 'process-live-p) (lambda (_p) nil))
                ((symbol-function 'process-name) (lambda (_p) "uds<8>"))
                ((symbol-function 'agent-repl--warn)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) warned)))
                ((symbol-function 'agent-repl--uds-run-timer)
                 (lambda (&rest _) 'fake-timer)))
        ;; Act
        (agent-repl--uds-sentinel 'dead-proc "connection broken by remote peer\n")
        ;; Assert
        (should-not (string-match-p "elapsed=nil" (car warned)))))))

(ert-deftest agent-repl-test-uds-sentinel-failed-dial-reports-its-elapsed ()
  "A dial that never opened is reported as a dial failure carrying its elapsed."
  ;; Arrange — a live started-at is what a dial still in flight leaves behind.
  (agent-repl-test--with-uds
    (setq agent-repl--uds-process 'dead-proc
          agent-repl--uds-connect-started-at (- (float-time) 1.5))
    (let (warned)
      (cl-letf (((symbol-function 'process-live-p) (lambda (_p) nil))
                ((symbol-function 'process-name) (lambda (_p) "uds<8>"))
                ((symbol-function 'agent-repl--warn)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) warned)))
                ((symbol-function 'agent-repl--uds-run-timer)
                 (lambda (&rest _) 'fake-timer)))
        ;; Act
        (agent-repl--uds-sentinel 'dead-proc "connection broken by remote peer\n")
        ;; Assert
        (should (string-match-p "uds-connect: dial FAILED" (car warned)))))))

(ert-deftest agent-repl-test-uds-sentinel-link-loss-still-warns ()
  "A link loss stays LOUD: this ladder retries forever and never gives up."
  ;; Arrange
  (agent-repl-test--with-uds
    (setq agent-repl--uds-process 'dead-proc
          agent-repl--uds-connect-started-at nil)
    (let ((warned 0))
      (cl-letf (((symbol-function 'process-live-p) (lambda (_p) nil))
                ((symbol-function 'process-name) (lambda (_p) "uds<8>"))
                ((symbol-function 'agent-repl--warn)
                 (lambda (&rest _) (cl-incf warned)))
                ((symbol-function 'agent-repl--uds-run-timer)
                 (lambda (&rest _) 'fake-timer)))
        ;; Act
        (agent-repl--uds-sentinel 'dead-proc "connection broken by remote peer\n")
        ;; Assert
        (should (= warned 1))))))

;;;; ---- Tests: link loss inside an expected-restart window ----

(defmacro agent-repl-test--with-link-down (initiator warned informed &rest body)
  "Drive an ESTABLISHED link down, collecting warn/info text and running BODY.
INITIATOR nil leaves the expected-restart window disarmed; a string arms a
window on its behalf first.  WARNED and INFORMED are bound to the lists of
messages each level received, newest first."
  (declare (indent 3))
  `(agent-repl-test--with-uds
     (let ((agent-repl--frontend-expected-restart nil)
           (agent-repl--frontend-expected-restart-last-close nil)
           (agent-repl-frontend-expected-restart-window-seconds 180.0)
           (,warned nil)
           (,informed nil))
       (setq agent-repl--uds-process 'dead-proc
             agent-repl--uds-connect-started-at nil)
       (cl-letf (((symbol-function 'process-live-p) (lambda (_p) nil))
                 ((symbol-function 'process-name) (lambda (_p) "uds<9>"))
                 ((symbol-function 'agent-repl--warn)
                  (lambda (_ws fmt &rest args) (push (apply #'format fmt args) ,warned)))
                 ((symbol-function 'agent-repl--info)
                  (lambda (_ws fmt &rest args) (push (apply #'format fmt args) ,informed))))
         (when ,initiator
           (agent-repl--frontend-arm-expected-restart ,initiator))
         ,@body))))

(ert-deftest agent-repl-test-uds-link-down-inside-a-restart-window-logs-info ()
  "A link that drops inside an expected-restart window is info, naming who ordered it."
  ;; Arrange
  (agent-repl-test--with-link-down "deploy (emacsclient)" warned informed
    ;; Act
    (agent-repl--uds-sentinel 'dead-proc "deleted\n")
    ;; Assert
    (should (string-match-p "uds-link: down for the deploy (emacsclient) restart"
                            (car informed)))
    (ignore warned)))

(ert-deftest agent-repl-test-uds-link-down-inside-a-restart-window-emits-no-warn ()
  "The deliberate drop spends no warning: the restart already explained it."
  ;; Arrange
  (agent-repl-test--with-link-down "deploy (emacsclient)" warned informed
    ;; Act
    (agent-repl--uds-sentinel 'dead-proc "deleted\n")
    ;; Assert
    (should-not warned)
    (ignore informed)))

(ert-deftest agent-repl-test-uds-link-down-outside-a-restart-window-warns-verbatim ()
  "With no window armed the link-loss warning is byte-identical to before."
  ;; Arrange
  (agent-repl-test--with-link-down nil warned informed
    ;; Act
    (agent-repl--uds-sentinel 'dead-proc "connection broken by remote peer\n")
    ;; Assert
    (should (equal (car warned)
                   "uds-link: DOWN proc=uds<9> (link was established) event=connection broken by remote peer"))
    (ignore informed)))

(ert-deftest agent-repl-test-uds-link-down-classifies-by-window-not-by-event ()
  "A graceful peer-close inside the window classifies exactly as a `deleted' kill.
The same deliberate bounce produces either event string, so the event can
never be the discriminator."
  ;; Arrange
  (agent-repl-test--with-link-down "deploy (emacsclient)" warned informed
    ;; Act
    (agent-repl--uds-sentinel 'dead-proc "connection broken by remote peer\n")
    ;; Assert
    (should-not warned)
    (should (string-match-p "uds-link: down for the" (car informed)))))

(ert-deftest agent-repl-test-uds-link-down-inside-a-restart-window-still-reconnects ()
  "Classification changes what is SAID about the drop, never the reconnect ladder."
  ;; Arrange
  (agent-repl-test--with-link-down "deploy (emacsclient)" warned informed
    (let (scheduled)
      (cl-letf (((symbol-function 'agent-repl--uds-run-timer)
                 (lambda (delay fn) (setq scheduled (list delay fn)) 'fake-timer)))
        ;; Act
        (agent-repl--uds-sentinel 'dead-proc "deleted\n")
        ;; Assert
        (should (eq (nth 1 scheduled) #'agent-repl-uds-connect))))
    (ignore warned informed)))

(ert-deftest agent-repl-test-uds-dial-failure-inside-a-restart-window-still-warns ()
  "The window explains an ESTABLISHED link dropping, never a dial that never opened."
  ;; Arrange
  (agent-repl-test--with-link-down "deploy (emacsclient)" warned informed
    (setq agent-repl--uds-connect-started-at (- (float-time) 1.5))
    ;; Act
    (agent-repl--uds-sentinel 'dead-proc "connection broken by remote peer\n")
    ;; Assert
    (should (string-match-p "uds-connect: dial FAILED" (car warned)))
    (ignore informed)))

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

;;;; ---- a write that loses the link inside a restart window --------------
;;
;; Live evidence: five seconds after a deploy TERMed the daemon, the periodic
;; roster export tried to write and Emacs warned "uds-send-command: FAILED
;; field=publishWorkspaceRoster ... error=process ... no longer connected".
;; The daemon was draining inside an open expected-restart window, and the
;; next reconnect publish carries a fresher roster than the frame that could
;; not go out — so that line is a phase of the restart, not news.

(defun agent-repl-test--uds-failed-write (initiator field error-message)
  "Fail one write of FIELD with ERROR-MESSAGE and report what was said.
INITIATOR, when non-nil, arms an expected-restart window on its behalf
first.  Returns a plist: `:warned' / `:informed' are the messages each
level received (newest first), and `:ack' is the CommandAck the transport
handed to the caller-settling path."
  (let ((agent-repl--frontend-expected-restart nil)
        (agent-repl--frontend-expected-restart-last-close nil)
        (agent-repl-frontend-expected-restart-window-seconds 180.0)
        (warned nil)
        (informed nil)
        (ack 'never-settled))
    (cl-letf (((symbol-function 'agent-repl--frontend-ws-name) (lambda (_w) nil))
              ((symbol-function 'agent-repl--log) (lambda (&rest _) nil))
              ((symbol-function 'process-send-string)
               (lambda (&rest _) (error "%s" error-message)))
              ((symbol-function 'agent-repl--warn)
               (lambda (_ws fmt &rest args) (push (apply #'format fmt args) warned)))
              ((symbol-function 'agent-repl--info)
               (lambda (_ws fmt &rest args) (push (apply #'format fmt args) informed)))
              ((symbol-function 'agent-repl--uds-handle-command-ack)
               (lambda (a) (setq ack a))))
      (when initiator
        (agent-repl--frontend-arm-expected-restart initiator))
      (unwind-protect
          (agent-repl--uds-write-frame
           'fake-proc (list :field field :request-id "fe-348-b00a"
                            :frame "{}\n" :workspace nil
                            :enqueued-at (float-time)))
        (agent-repl--frontend-expected-restart-cancel-timer)))
    (list :warned warned :informed informed :ack ack)))

(ert-deftest agent-repl-test-uds-write-lost-link-in-window-logs-info ()
  "A republished field losing the link inside the window is info, naming the initiator."
  ;; Arrange
  (agent-repl-test--with-uds
    ;; Act
    (let ((result (agent-repl-test--uds-failed-write
                   "deploy (emacsclient)" "publishWorkspaceRoster"
                   "process agent-repl-frontend-uds<13> no longer connected")))
      ;; Assert
      (should (equal (car (plist-get result :informed))
                     (concat "uds-send-command: field=publishWorkspaceRoster "
                             "request-id=fe-348-b00a not sent — link down for the "
                             "deploy (emacsclient) restart; the reconnect publish "
                             "supersedes it"))))))

(ert-deftest agent-repl-test-uds-write-lost-link-in-window-emits-no-warn ()
  "The classified write spends no warning: the restart already explained it."
  ;; Arrange
  (agent-repl-test--with-uds
    ;; Act
    (let ((result (agent-repl-test--uds-failed-write
                   "deploy (emacsclient)" "publishWorkspaceRoster"
                   "process agent-repl-frontend-uds<13> no longer connected")))
      ;; Assert
      (should-not (plist-get result :warned)))))

(ert-deftest agent-repl-test-uds-write-lost-link-outside-a-window-warns-verbatim ()
  "With no window armed the send-failure warning is byte-identical to before."
  ;; Arrange
  (agent-repl-test--with-uds
    ;; Act
    (let ((result (agent-repl-test--uds-failed-write
                   nil "publishWorkspaceRoster"
                   "process agent-repl-frontend-uds<13> no longer connected")))
      ;; Assert
      (should (equal (car (plist-get result :warned))
                     (concat "uds-send-command: FAILED field=publishWorkspaceRoster "
                             "request-id=fe-348-b00a error=process "
                             "agent-repl-frontend-uds<13> no longer connected"))))))

(ert-deftest agent-repl-test-uds-write-non-link-failure-in-window-still-warns ()
  "The window explains a lost link, never an unrecognized write failure."
  ;; Arrange
  (agent-repl-test--with-uds
    ;; Act
    (let ((result (agent-repl-test--uds-failed-write
                   "deploy (emacsclient)" "publishWorkspaceRoster"
                   "Args out of range: 4096")))
      ;; Assert
      (should (string-match-p "uds-send-command: FAILED"
                              (car (plist-get result :warned)))))))

(ert-deftest agent-repl-test-uds-write-one-shot-command-in-window-still-warns ()
  "A one-shot command nothing re-sends stays loud: its loss is real either way."
  ;; Arrange
  (agent-repl-test--with-uds
    ;; Act
    (let ((result (agent-repl-test--uds-failed-write
                   "deploy (emacsclient)" "submitPrompt"
                   "process agent-repl-frontend-uds<13> no longer connected")))
      ;; Assert
      (should (string-match-p "uds-send-command: FAILED field=submitPrompt"
                              (car (plist-get result :warned)))))))

(ert-deftest agent-repl-test-uds-write-classified-failure-still-nacks-the-caller ()
  "Classification picks a log level; the caller still sees the send fail."
  ;; Arrange
  (agent-repl-test--with-uds
    ;; Act
    (let ((result (agent-repl-test--uds-failed-write
                   "deploy (emacsclient)" "publishWorkspaceRoster"
                   "process agent-repl-frontend-uds<13> no longer connected")))
      ;; Assert
      (should (equal (plist-get result :ack)
                     (list :requestId "fe-348-b00a" :ok nil
                           :error "process agent-repl-frontend-uds<13> no longer connected"))))))

(ert-deftest agent-repl-test-uds-write-unclassified-failure-nacks-identically ()
  "The unclassified branch hands the caller exactly the same CommandAck."
  ;; Arrange
  (agent-repl-test--with-uds
    ;; Act
    (let ((result (agent-repl-test--uds-failed-write
                   nil "publishWorkspaceRoster"
                   "process agent-repl-frontend-uds<13> no longer connected")))
      ;; Assert
      (should (equal (plist-get result :ack)
                     (list :requestId "fe-348-b00a" :ok nil
                           :error "process agent-repl-frontend-uds<13> no longer connected"))))))

(ert-deftest agent-repl-test-uds-write-failure-link-down-p-uses-process-status ()
  "A process whose status is no longer live is link-down whatever the message says."
  ;; Arrange
  (cl-letf (((symbol-function 'processp) (lambda (_p) t))
            ((symbol-function 'process-status) (lambda (_p) 'closed)))
    ;; Act / Assert
    (should (agent-repl--uds-write-failure-link-down-p
             'fake-proc '(error "Args out of range: 4096")))))

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

(ert-deftest agent-repl-test-uds-send-command-strips-trailing-slash-workspace ()
  "A workspace key with a trailing slash reaches the wire normalized."
  ;; Arrange
  (agent-repl-test--with-uds
    (let (sent)
      (agent-repl-test--capturing-send sent
        ;; Act
        (agent-repl--uds-send-command
         "restartSession" nil "/Users/u/doom-worktrees/ws/" 'fake-proc)
        ;; Assert
        (let ((frame (json-parse-string (string-trim-right sent)
                                        :object-type 'plist :array-type 'list)))
          (should (equal (plist-get frame :workspace)
                         "/Users/u/doom-worktrees/ws")))))))

(ert-deftest agent-repl-test-uds-normalize-workspace-key-keeps-plain-path ()
  "A key with no trailing slash is left exactly as it is."
  ;; Arrange
  (let ((key "/Users/u/doom-worktrees/ws"))
    ;; Act / Assert
    (should (equal (agent-repl--uds-normalize-workspace-key key) key))))

(ert-deftest agent-repl-test-uds-normalize-workspace-key-passes-nil-through ()
  "A nil key stays nil — it means \"no workspace field\", not a path."
  ;; Arrange / Act / Assert
  (should (null (agent-repl--uds-normalize-workspace-key nil))))

(ert-deftest agent-repl-test-uds-normalize-workspace-key-passes-empty-through ()
  "An empty key is not a path and is not rewritten."
  ;; Arrange / Act / Assert
  (should (equal (agent-repl--uds-normalize-workspace-key "") "")))

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
    ;; The contained subscriber failure rides the warn rung: the recovery step
    ;; it owned never ran, which is a UX regression even though the rest of the
    ;; hook continued.
    (cl-letf (((symbol-function 'agent-repl--warn)
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
    ;; Same warn rung as the connected hook, from the same shared runner.
    (cl-letf (((symbol-function 'agent-repl--warn)
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
              ;; A refused dial is a STALE socket file: present, no listener.
              ((symbol-function 'agent-repl--uds-socket-file-present-p)
               (lambda (&rest _) t))
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
              ;; A refused dial is a STALE socket file: present, no listener.
              ((symbol-function 'agent-repl--uds-socket-file-present-p)
               (lambda (&rest _) t))
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

(ert-deftest agent-repl-test-uds-async-bubble-delta-is-a-deliberately-ignored-frame ()
  "`asyncBubbleDelta' is a webapp bubble surface Emacs does not draw."
  ;; Act / Assert
  (should (member "asyncBubbleDelta" agent-repl--uds-ignored-frame-fields)))

(ert-deftest agent-repl-test-uds-topbar-is-a-deliberately-ignored-frame ()
  "`topbar' is the webapp's resolved chrome; Emacs draws its own tab bar."
  ;; Act / Assert
  (should (member "topbar" agent-repl--uds-ignored-frame-fields)))

(ert-deftest agent-repl-test-uds-token-breakdown-is-a-deliberately-ignored-frame ()
  "`tokenBreakdown' is the webapp's menu; Emacs has no token menu."
  ;; Act / Assert
  (should (member "tokenBreakdown" agent-repl--uds-ignored-frame-fields)))

(ert-deftest agent-repl-test-uds-workspace-gate-is-a-deliberately-ignored-frame ()
  "`workspaceGate' is the webapp's revival gate; Emacs offers no gate."
  ;; Act / Assert
  (should (member "workspaceGate" agent-repl--uds-ignored-frame-fields)))

(ert-deftest agent-repl-test-uds-conversation-page-is-a-deliberately-ignored-frame ()
  "`conversationPage' is the webapp's history paging; Emacs renders no feed."
  ;; Act / Assert
  (should (member "conversationPage" agent-repl--uds-ignored-frame-fields)))

(ert-deftest agent-repl-test-uds-dispatch-conversation-page-does-not-signal ()
  "A pushed `conversationPage' arm decodes instead of taking the drain down.
Emacs never asks for a page, so one should not reach it — but the frame
vocabulary mirrors the proto oneof rather than the subset Emacs expects,
and an unlisted arm SIGNALS rather than being dropped."
  ;; Arrange / Act / Assert
  (should-not (agent-repl--uds-dispatch-frame
               '(:conversationPage (:workspace "ws1" :requestId "r-1" :fence "sess-a|gen-b")))))

(ert-deftest agent-repl-test-uds-dispatch-topbar-does-not-signal ()
  "A pushed `topbar' arm decodes instead of reading as an unknown wire field.
The daemon broadcasts the resolved component views to every client, host
included, so an unlisted arm would take the whole drain down."
  ;; Arrange
  (agent-repl-test--with-uds
    ;; Act / Assert
    (should-not (agent-repl--uds-dispatch-frame
                 '(:topbar (:workspace "ws1" :fence "f1"))))))

(ert-deftest agent-repl-test-uds-dispatch-logs-the-fence-of-a-fenced-push ()
  "A fenced push reserves `session_id', so the trace must name its fence.
Without this the hottest diagnostic line in the transport prints
`session-id=nil' for every conversation, typing and progress frame on the
wire and says nothing about which generation produced them."
  ;; Arrange
  (agent-repl-test--with-uds
    (let (logged)
      (agent-repl--uds-register-handler "conversationDelta" (lambda (_p) nil))
      (cl-letf (((symbol-function 'agent-repl--log)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logged))))
        ;; Act
        (agent-repl--uds-dispatch-frame
         '(:conversationDelta (:workspace "ws1" :fence "sess-a|gen-b")))
        ;; Assert
        (should (seq-find (lambda (m) (string-match-p "fence=\"sess-a|gen-b\"" m))
                          logged))))))

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

(ert-deftest agent-repl-test-uds-progress-is-no-longer-an-ignored-frame ()
  "`progress' left the ignored list when it acquired a handler.
The consolidated footer is still webapp-only, but the proto assigns ONE
of the message's fields — `expensive_turn' — to Emacs, and
`context-cost.el' registers a reader for it.  An arm on the ignored list
AND carrying a handler would state two incompatible things about the same
frame."
  ;; Act / Assert
  (should-not (member "progress" agent-repl--uds-ignored-frame-fields)))

(ert-deftest agent-repl-test-uds-dispatch-progress-frame-returns-nil ()
  "Dispatching a progress frame is quiet, not a signal.
Nothing is registered inside this fixture's cleared handler registry, and
a known arm with no handler is a logged gap rather than an error."
  ;; Arrange
  (agent-repl-test--with-uds
    ;; Act / Assert
    (should-not (agent-repl--uds-dispatch-frame
                 '(:progress (:workspace "ws1" :fence "f1" :liveTaskCount 0))))))

(ert-deftest agent-repl-test-uds-progress-has-a-registered-handler ()
  "A progress frame reaches a handler, so it never logs unfinished wiring.
This is the same guarantee the old ignored-list membership gave, now held
the other way round: the arm is dispatched because `context-cost.el'
registered a reader at load, not skipped because nothing wanted it."
  ;; Act / Assert
  (should (assoc "progress" agent-repl--uds-frame-handlers)))

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

(ert-deftest agent-repl-test-uds-every-known-command-has-a-user-verb ()
  "No sendable command can nack into an echo line with a hole where its name goes."
  ;; Act / Assert
  (dolist (field agent-repl--uds-known-command-fields)
    (should (agent-repl--uds-command-verb field))))

(ert-deftest agent-repl-test-uds-command-verb-translates-a-wire-field ()
  "The verb is English, not the protojson identifier."
  ;; Act / Assert
  (should (equal (agent-repl--uds-command-verb "submitPrompt") "prompt")))

(ert-deftest agent-repl-test-uds-command-verb-degrades-an-unknown-field ()
  "An unmapped field yields nil rather than leaking the identifier as copy."
  ;; Act / Assert
  (should-not (agent-repl--uds-command-verb "notACommand")))

(ert-deftest agent-repl-test-uds-command-verb-degrades-a-nil-field ()
  "A nil field is not a verb, and asking is not an error."
  ;; Act / Assert
  (should-not (agent-repl--uds-command-verb nil)))

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
                 '(:queue (:workspace "ws1" :fence "f1"
                           :entries ((:id "q1" :text "hi"
                                      :pending ()
                                      :shutdown (:scheduleId "sch-1")))))))))

(ert-deftest agent-repl-test-uds-schedule-shutdown-is-a-known-command ()
  "The drain lease's `scheduleShutdown' arm is an accepted outbound command."
  ;; Act / Assert
  (should (member "scheduleShutdown" agent-repl--uds-known-command-fields)))

(ert-deftest agent-repl-test-uds-cancel-scheduled-shutdown-is-a-known-command ()
  "The drain lease's `cancelScheduledShutdown' arm is an accepted command."
  ;; Act / Assert
  (should (member "cancelScheduledShutdown" agent-repl--uds-known-command-fields)))

(ert-deftest agent-repl-test-uds-cancel-detached-agents-is-a-known-command ()
  "The workspace-close `cancelDetachedAgents' arm is an accepted command."
  ;; Act / Assert
  (should (member "cancelDetachedAgents" agent-repl--uds-known-command-fields)))

(ert-deftest agent-repl-test-uds-every-send-site-field-is-allowlisted ()
  "Every literal field name any lisp/ send site passes is on the allowlist.
The allowlist is what `agent-repl--uds-send-command' checks, and an arm
added at a call site but not here fails LOUDLY at runtime — which is how
`cancelDetachedAgents' broke every workspace close.  Scanning the sources
turns that into a suite failure at the moment the send site lands."
  ;; Arrange
  (let ((fields (agent-repl-test--uds-send-command-literal-fields)))
    ;; Act / Assert -- the scan must find sites, or it asserts nothing.
    (should fields)
    (should-not (cl-remove-if
                 (lambda (field)
                   (member field agent-repl--uds-known-command-fields))
                 fields))))

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

(ert-deftest agent-repl-test-uds-register-pending-command-records-entry ()
  "Tracking a command records its field + workspace under the request-id."
  ;; Arrange
  (agent-repl-test--with-uds
    ;; Act
    (agent-repl-test--pend "req-1" "mergeWorkspace" "ws1")
    ;; Assert
    (let ((entry (gethash "req-1" agent-repl--uds-pending-commands)))
      (should (equal (plist-get entry :field) "mergeWorkspace"))
      (should (equal (plist-get entry :workspace) "ws1")))))

(ert-deftest agent-repl-test-uds-command-ack-ok-drops-entry ()
  "An ok=t ack drops the pending entry and returns t."
  ;; Arrange
  (agent-repl-test--with-uds
    (agent-repl-test--pend "req-1" "mergeWorkspace" "ws1")
    ;; Act / Assert
    (should (eq (agent-repl--uds-handle-command-ack '(:requestId "req-1" :ok t)) t))
    (should-not (gethash "req-1" agent-repl--uds-pending-commands))))

(ert-deftest agent-repl-test-uds-command-ack-failure-surfaces-loudly ()
  "A failed ack (ok omitted) surfaces echo-area USER COPY and returns nil.
The refusal is still loud; only its presentation changed — the echo names
the command in English rather than reprinting the daemon's error text."
  ;; Arrange
  (agent-repl-test--with-uds
    (agent-repl-test--pend "req-1" "mergeWorkspace" "ws1")
    (let (echoed)
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (setq echoed (apply #'format fmt args)))))
        ;; Act — protojson omits ok=false, so a failure ack has no :ok
        (let ((ret (agent-repl--uds-handle-command-ack
                    '(:requestId "req-1" :error "branch not found"))))
          ;; Assert
          (should-not ret)
          (should (equal echoed
                         "agent-repl: merge failed — see the workspace log for detail")))))))

(ert-deftest agent-repl-test-uds-command-ack-failure-keeps-the-error-out-of-the-echo ()
  "The daemon's error text never reaches the minibuffer from an unclassified nack."
  ;; Arrange
  (agent-repl-test--with-uds
    (agent-repl-test--pend "req-1" "mergeWorkspace" "ws1")
    (let (echoed)
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (setq echoed (apply #'format fmt args)))))
        ;; Act
        (agent-repl--uds-handle-command-ack
         '(:requestId "req-1" :error "branch not found"))
        ;; Assert
        (should-not (string-match-p "branch not found" echoed))))))

(ert-deftest agent-repl-test-uds-command-ack-failure-files-the-error-text ()
  "The error text the echo dropped is still on the canonical log."
  ;; Arrange
  (agent-repl-test--with-uds
    (agent-repl-test--pend "req-1" "mergeWorkspace" "ws1")
    (let (logged)
      (cl-letf (((symbol-function 'message) #'ignore)
                ((symbol-function 'agent-repl--log)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logged))))
        ;; Act
        (agent-repl--uds-handle-command-ack
         '(:requestId "req-1" :error "branch not found"))
        ;; Assert
        (should (cl-find-if (lambda (line) (string-match-p "branch not found" line))
                            logged))))))

(ert-deftest agent-repl-test-uds-command-ack-classified-refusal-uses-the-failure-path ()
  "A CLASSIFIED refusal surfaces through the failure path, not the raw fallback.
`CommandAck.failure' is a bare `FailureKind' now, so the ack has to be read
as a kind rather than as a whole card; misreading it would drop every
refusal onto the unclassified branch.  An ack carries no daemon-composed
sentence, so the raw error it does carry is TRANSLATED into user copy."
  ;; Arrange
  (agent-repl-test--with-uds
    (agent-repl-test--pend "req-1" "submitPrompt" "ws1")
    (let (echoed)
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (setq echoed (apply #'format fmt args))))
                ((symbol-function 'agent-repl--warn) (lambda (&rest _) nil)))
        ;; Act
        (agent-repl--uds-handle-command-ack
         '(:requestId "req-1" :error "the workspace has no live session"
           :failure (:workspaceNotLive ())))
        ;; Assert
        (should (equal echoed
                       (concat "agent-repl: prompt refused — this workspace is not "
                               "live; start or wake it first")))))))

(ert-deftest agent-repl-test-uds-command-ack-unclassified-refusal-still-surfaces ()
  "An ack with no classified kind still surfaces, as the generic sentence.
A refusal a mixed-build daemon could not classify must never be invisible;
what it must also never be is a Go error chain in the echo area."
  ;; Arrange
  (agent-repl-test--with-uds
    (agent-repl-test--pend "req-1" "submitPrompt" "ws1")
    (let (echoed)
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (setq echoed (apply #'format fmt args)))))
        ;; Act
        (agent-repl--uds-handle-command-ack
         '(:requestId "req-1" :error "unexplained"))
        ;; Assert
        (should (equal echoed
                       "agent-repl: prompt failed — see the workspace log for detail"))))))

(ert-deftest agent-repl-test-uds-command-ack-merge-queued-nack-reads-as-a-refusal ()
  "The observed merge-queued refusal reads as by-design, not as a broken workspace."
  ;; Arrange
  (agent-repl-test--with-uds
    (agent-repl-test--pend "req-1" "submitPrompt" "ws1")
    (let (echoed)
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (setq echoed (apply #'format fmt args)))))
        ;; Act
        (agent-repl--uds-handle-command-ack
         '(:requestId "req-1"
           :error "ssm: prompt state invariant failed: state=RENDER_STATE_MERGE_QUEUED turn_active=true"))
        ;; Assert
        (should (equal echoed
                       (concat "agent-repl: prompt refused — this workspace is queued "
                               "for a merge; wait for the merge to finish or interrupt it")))))))

(ert-deftest agent-repl-test-uds-command-ack-failure-runs-on-failure ()
  "A failed ack runs the tracked :on-failure callback with the error string."
  ;; Arrange
  (agent-repl-test--with-uds
    (let (cb-arg)
      (agent-repl-test--pend
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
      (agent-repl-test--pend
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
      (agent-repl-test--pend
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
      (agent-repl-test--pend "req-1" "interrupt" "ws1")
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
      (agent-repl-test--pend
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
      (agent-repl-test--pend
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
  "Resolving for the log must NOT rewrite the cwd the daemon routes by.
Trailing-slash normalization is the ONE rewrite the wire key gets (see
`agent-repl--uds-normalize-workspace-key'), so the expectation is the
normalized cwd, not the log sink's workspace name."
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
                       (directory-file-name temporary-file-directory)))))))

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

(ert-deftest agent-repl-test-uds-register-pending-command-unowned-cwd-does-not-signal ()
  "Tracking a command issued for an unowned cwd must not signal."
  ;; Arrange
  (agent-repl-test--with-uds-log-sink
    ;; Act / Assert
    (should (equal (agent-repl-test--pend
                    "req-1" "submitPrompt" "/nowhere/unowned")
                   "req-1"))))

(ert-deftest agent-repl-test-uds-register-pending-command-retains-the-raw-cwd ()
  "The retained `:workspace' stays raw — the ack path correlates against it."
  ;; Arrange
  (agent-repl-test--with-uds-log-sink
    (agent-repl--ws-put "ws1" :project-dir temporary-file-directory)
    ;; Act
    (agent-repl-test--pend "req-1" "submitPrompt" temporary-file-directory)
    ;; Assert
    (should (equal (plist-get (gethash "req-1" agent-repl--uds-pending-commands)
                              :workspace)
                   temporary-file-directory))))

(ert-deftest agent-repl-test-uds-untrack-command-unowned-cwd-does-not-signal ()
  "Untracking after a local timeout must not signal on an unowned cwd."
  ;; Arrange
  (agent-repl-test--with-uds-log-sink
    (agent-repl-test--pend "req-1" "submitPrompt" "/nowhere/unowned")
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
    (agent-repl-test--pend "req-1" "submitPrompt" "/nowhere/unowned")
    ;; Act / Assert
    (should (agent-repl--uds-handle-command-ack
             '(:requestId "req-1" :ok t)))))

(ert-deftest agent-repl-test-uds-command-ack-rejected-cwd-does-not-signal ()
  "A REJECTED ack also routes its classified failure through the log sink."
  ;; Arrange
  (agent-repl-test--with-uds-log-sink
    (agent-repl-test--pend "req-1" "submitPrompt" "/nowhere/unowned")
    ;; Act / Assert
    (should-not (agent-repl--uds-handle-command-ack
                 '(:requestId "req-1" :error "nope"
                   :failure (:shimRejected ()))))))

(ert-deftest agent-repl-test-uds-command-ack-challenge-cwd-does-not-signal ()
  "The interrupt-confirmation CHALLENGE branch logs with the same cwd."
  ;; Arrange
  (agent-repl-test--with-uds-log-sink
    (agent-repl-test--pend "req-1" "interrupt" "/nowhere/unowned")
    ;; Act / Assert
    (should-not (agent-repl--uds-handle-command-ack
                 '(:requestId "req-1"
                   :interruptConfirmRequired (:liveTasks 2))))))

;;;; ---- Ack aging + command-link health ---------------------------------

(ert-deftest agent-repl-test-uds-link-health-starts-healthy ()
  "A connected link with nothing lost reports `:healthy'."
  ;; Arrange / Act / Assert
  (agent-repl-test--with-uds
    (agent-repl-test--with-connected-link
      (should (eq (agent-repl-uds-link-health) :healthy)))))

(ert-deftest agent-repl-test-uds-link-health-down-link-is-degraded ()
  "A link whose process is not connected reports `:degraded'.
Nothing is in flight to go unacknowledged, which is exactly how a total
link loss used to report itself healthy."
  ;; Arrange
  (agent-repl-test--with-uds
    ;; Act / Assert — no connected link is arranged: the link is DOWN.
    (should (eq (agent-repl-uds-link-health) :degraded))))

(ert-deftest agent-repl-test-uds-link-health-reconnect-pending-is-degraded ()
  "A down link with a reconnect scheduled is still `:degraded'."
  ;; Arrange
  (agent-repl-test--with-uds
    ;; Act
    (agent-repl--uds-schedule-reconnect)
    ;; Assert
    (should (eq (agent-repl-uds-link-health) :degraded))))

(ert-deftest agent-repl-test-uds-link-health-dialing-is-degraded ()
  "A link still dialing has not carried anything yet, so it is `:degraded'."
  ;; Arrange
  (agent-repl-test--with-uds
    (let ((agent-repl--uds-process 'fake-proc)
          (agent-repl--uds-connection-state 'dialing))
      (cl-letf (((symbol-function 'process-live-p)
                 (lambda (proc) (eq proc 'fake-proc))))
        ;; Act / Assert
        (should (eq (agent-repl-uds-link-health) :degraded))))))

(ert-deftest agent-repl-test-uds-link-health-open-sentinel-restores-healthy ()
  "The sentinel's `open' transition brings the reader back to `:healthy'."
  ;; Arrange
  (agent-repl-test--with-uds
    (let ((agent-repl--uds-process 'fake-proc)
          (agent-repl--uds-connection-state 'dialing)
          (agent-repl--uds-connect-started-at (float-time)))
      (cl-letf (((symbol-function 'process-live-p)
                 (lambda (proc) (eq proc 'fake-proc)))
                ((symbol-function 'process-name) (lambda (_p) "fake"))
                ((symbol-function 'agent-repl--force-tab-bar-redraw)
                 (lambda () nil)))
        ;; Act
        (agent-repl--uds-sentinel-transition 'fake-proc "open\n")
        ;; Assert
        (should (eq (agent-repl-uds-link-health) :healthy))))))

(ert-deftest agent-repl-test-uds-link-down-repaints-the-tab-bar ()
  "The link's DOWN edge drives the shared tab-bar repaint."
  ;; Arrange
  (agent-repl-test--with-uds
    (let ((agent-repl--uds-process 'fake-proc)
          (agent-repl--uds-connection-state 'open)
          (repainted 0))
      (cl-letf (((symbol-function 'process-live-p) (lambda (_p) nil))
                ((symbol-function 'process-name) (lambda (_p) "fake"))
                ((symbol-function 'agent-repl--frontend-expected-restart-initiator)
                 (lambda () nil))
                ((symbol-function 'agent-repl--force-tab-bar-redraw)
                 (lambda () (setq repainted (1+ repainted)))))
        ;; Act
        (agent-repl--uds-sentinel-transition 'fake-proc "connection broken\n")
        ;; Assert
        (should (= repainted 1))))))

(ert-deftest agent-repl-test-uds-link-up-repaints-the-tab-bar ()
  "The link's UP edge drives the same shared tab-bar repaint."
  ;; Arrange
  (agent-repl-test--with-uds
    (let ((agent-repl--uds-process 'fake-proc)
          (agent-repl--uds-connection-state 'dialing)
          (agent-repl--uds-connect-started-at (float-time))
          (repainted 0))
      (cl-letf (((symbol-function 'process-live-p)
                 (lambda (proc) (eq proc 'fake-proc)))
                ((symbol-function 'process-name) (lambda (_p) "fake"))
                ((symbol-function 'agent-repl--force-tab-bar-redraw)
                 (lambda () (setq repainted (1+ repainted)))))
        ;; Act
        (agent-repl--uds-sentinel-transition 'fake-proc "open\n")
        ;; Assert
        (should (= repainted 1))))))

(ert-deftest agent-repl-test-uds-established-link-down-arms-the-recovery-slo ()
  "An established link dropping is the SLO's down-edge evidence."
  ;; Arrange
  (agent-repl-test--with-uds
    (let ((agent-repl--uds-process 'fake-proc)
          (agent-repl--uds-connection-state 'open)
          (armed 0))
      (cl-letf (((symbol-function 'process-live-p) (lambda (_p) nil))
                ((symbol-function 'process-name) (lambda (_p) "fake"))
                ((symbol-function 'agent-repl--frontend-expected-restart-initiator)
                 (lambda () nil))
                ((symbol-function 'agent-repl--force-tab-bar-redraw) (lambda () nil))
                ((symbol-function 'agent-repl--recovery-slo-on-link-down)
                 (lambda () (setq armed (1+ armed)))))
        ;; Act
        (agent-repl--uds-sentinel-transition 'fake-proc "connection broken\n")
        ;; Assert
        (should (= armed 1))))))

(ert-deftest agent-repl-test-uds-failed-dial-does-not-arm-the-recovery-slo ()
  "A dial that never completed is a RETRY of an outage, not fresh evidence."
  ;; Arrange
  (agent-repl-test--with-uds
    (let ((agent-repl--uds-process 'fake-proc)
          (agent-repl--uds-connection-state 'dialing)
          (agent-repl--uds-connect-started-at (float-time))
          (armed 0))
      (cl-letf (((symbol-function 'process-live-p) (lambda (_p) nil))
                ((symbol-function 'process-name) (lambda (_p) "fake"))
                ((symbol-function 'agent-repl--force-tab-bar-redraw) (lambda () nil))
                ((symbol-function 'agent-repl--recovery-slo-on-link-down)
                 (lambda () (setq armed (1+ armed)))))
        ;; Act
        (agent-repl--uds-sentinel-transition 'fake-proc "failed\n")
        ;; Assert
        (should (= armed 0))))))

(ert-deftest agent-repl-test-uds-announced-link-down-still-arms-the-recovery-slo ()
  "A drop demoted to INFO by the restart window is STILL armed on."
  ;; Arrange
  (agent-repl-test--with-uds
    (let ((agent-repl--uds-process 'fake-proc)
          (agent-repl--uds-connection-state 'open)
          (armed 0))
      (cl-letf (((symbol-function 'process-live-p) (lambda (_p) nil))
                ((symbol-function 'process-name) (lambda (_p) "fake"))
                ((symbol-function 'agent-repl--frontend-expected-restart-initiator)
                 (lambda () "daemon-announced:deploy"))
                ((symbol-function 'agent-repl--force-tab-bar-redraw) (lambda () nil))
                ((symbol-function 'agent-repl--recovery-slo-on-link-down)
                 (lambda () (setq armed (1+ armed)))))
        ;; Act
        (agent-repl--uds-sentinel-transition 'fake-proc "connection broken\n")
        ;; Assert
        (should (= armed 1))))))

(ert-deftest agent-repl-test-uds-link-down-logs-the-health-transition ()
  "The down edge logs the health transition where it is DECIDED."
  ;; Arrange
  (agent-repl-test--with-uds
    (let ((agent-repl--uds-process 'fake-proc)
          (agent-repl--uds-connection-state 'open)
          logged)
      (cl-letf (((symbol-function 'process-live-p) (lambda (_p) nil))
                ((symbol-function 'process-name) (lambda (_p) "fake"))
                ((symbol-function 'agent-repl--frontend-expected-restart-initiator)
                 (lambda () nil))
                ((symbol-function 'agent-repl--force-tab-bar-redraw) (lambda () nil))
                ((symbol-function 'agent-repl--log)
                 (lambda (_ws fmt &rest args)
                   (push (apply #'format fmt args) logged))))
        ;; Act
        (agent-repl--uds-sentinel-transition 'fake-proc "connection broken\n")
        ;; Assert
        (should (cl-find-if
                 (lambda (line)
                   (string-match-p
                    "uds-link-health: -> :degraded cause=link-down" line))
                 logged))))))

(ert-deftest agent-repl-test-uds-link-health-unacked-degrades-a-live-link ()
  "The unacked-deadline condition still degrades a link that is CONNECTED."
  ;; Arrange
  (agent-repl-test--with-uds
    (agent-repl-test--with-connected-link
      (agent-repl-test--with-captured-deadline expire
        (agent-repl-test--pend "req-1" "mergeWorkspace" "ws1")
        (cl-letf (((symbol-function 'message) (lambda (&rest _) nil))
                  ((symbol-function 'agent-repl--force-tab-bar-redraw)
                   (lambda () nil)))
          ;; Act
          (funcall expire)
          ;; Assert
          (should (eq (agent-repl-uds-link-health) :degraded)))))))

(ert-deftest agent-repl-test-uds-link-health-reports-degraded ()
  "The health reader reports `:degraded' once the link has been degraded."
  ;; Arrange
  (agent-repl-test--with-uds
    ;; Act
    (agent-repl--uds-link-degrade "req-1" "mergeWorkspace" "ws1")
    ;; Assert
    (should (eq (agent-repl-uds-link-health) :degraded))))

(ert-deftest agent-repl-test-uds-register-pending-command-arms-deadline-alarm ()
  "Tracking a command arms the ack alarm through the injectable timer seam."
  ;; Arrange
  (agent-repl-test--with-uds
    (let ((agent-repl-uds-command-ack-deadline 7.5)
          scheduled)
      (cl-letf (((symbol-function 'agent-repl--uds-run-timer)
                 (lambda (delay fn) (setq scheduled (list delay fn)) 'fake-timer)))
        ;; Act
        (agent-repl-test--pend "req-1" "mergeWorkspace" "ws1")
        ;; Assert
        (should (equal (nth 0 scheduled) 7.5))
        (should (functionp (nth 1 scheduled)))))))

(ert-deftest agent-repl-test-uds-deadline-expiry-warns-user ()
  "An unacked command past its deadline surfaces a user-visible warning."
  ;; Arrange
  (agent-repl-test--with-uds
    (agent-repl-test--with-captured-deadline expire
      (agent-repl-test--pend "req-1" "mergeWorkspace" "ws1")
      (let (echoed)
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args) (setq echoed (apply #'format fmt args)))))
          ;; Act
          (funcall expire)
          ;; Assert
          (should (string-match-p "merge" echoed))
          (should (string-match-p "never acknowledged" echoed)))))))

(ert-deftest agent-repl-test-uds-deadline-expiry-keeps-the-wire-field-out-of-the-echo ()
  "The echo names the command in English; `mergeWorkspace' is a wire identifier."
  ;; Arrange
  (agent-repl-test--with-uds
    (agent-repl-test--with-captured-deadline expire
      (agent-repl-test--pend "req-1" "mergeWorkspace" "ws1")
      (let (echoed)
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args) (setq echoed (apply #'format fmt args)))))
          ;; Act
          (funcall expire)
          ;; Assert
          (should-not (string-match-p "mergeWorkspace" echoed)))))))

(ert-deftest agent-repl-test-uds-deadline-expiry-files-the-request-id ()
  "The request-id reaches the LOG, where correlation happens — not the echo area."
  ;; Arrange
  (agent-repl-test--with-uds
    (agent-repl-test--with-captured-deadline expire
      (agent-repl-test--pend "req-1" "mergeWorkspace" "ws1")
      (let (logged)
        (cl-letf (((symbol-function 'message) #'ignore)
                  ((symbol-function 'agent-repl--log)
                   (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logged))))
          ;; Act
          (funcall expire)
          ;; Assert
          (should (cl-find-if (lambda (line) (string-match-p "req-1" line)) logged)))))))

(ert-deftest agent-repl-test-uds-deadline-expiry-omits-the-request-id-from-the-echo ()
  "An identifier the user cannot act on stays off the one line they read."
  ;; Arrange
  (agent-repl-test--with-uds
    (agent-repl-test--with-captured-deadline expire
      (agent-repl-test--pend "req-1" "mergeWorkspace" "ws1")
      (let (echoed)
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args) (setq echoed (apply #'format fmt args)))))
          ;; Act
          (funcall expire)
          ;; Assert
          (should-not (string-match-p "req-1" echoed)))))))

(ert-deftest agent-repl-test-uds-deadline-expiry-logs-field-and-workspace ()
  "The canonical log line names the request-id, field, and wire workspace."
  ;; Arrange
  (agent-repl-test--with-uds
    (agent-repl-test--with-captured-deadline expire
      (agent-repl-test--pend "req-1" "mergeWorkspace" "ws1")
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
      (agent-repl-test--pend "req-1" "mergeWorkspace" "ws1")
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
      (agent-repl-test--pend "req-1" "mergeWorkspace" "ws1")
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
        (agent-repl-test--pend
         "req-1" "mergeWorkspace" "ws1" (lambda (_err) (setq failed t)))
        (cl-letf (((symbol-function 'message) (lambda (&rest _) nil)))
          ;; Act
          (funcall expire)
          ;; Assert
          (should-not failed))))))

(ert-deftest agent-repl-test-uds-deadline-superseded-opens-no-failure ()
  "A lost command its sender calls SUPERSEDED opens no failure card."
  ;; Arrange
  (agent-repl-test--with-uds
    (agent-repl-test--with-captured-deadline expire
      (agent-repl-test--pend "req-1" "publishWorkspaceRoster" nil
                             nil nil nil (lambda (_id) t))
      (let (echoed)
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (push (apply #'format fmt args) echoed))))
          ;; Act
          (funcall expire)
          ;; Assert
          (should-not echoed))))))

(ert-deftest agent-repl-test-uds-deadline-superseded-leaves-link-healthy ()
  "A superseded loss leaves the command link healthy: nothing diverged."
  ;; Arrange
  (agent-repl-test--with-uds
   (agent-repl-test--with-connected-link
    (agent-repl-test--with-captured-deadline expire
      (agent-repl-test--pend "req-1" "publishWorkspaceRoster" nil
                             nil nil nil (lambda (_id) t))
      (cl-letf (((symbol-function 'message) (lambda (&rest _) nil)))
        ;; Act
        (funcall expire)
        ;; Assert
        (should (eq (agent-repl-uds-link-health) :healthy)))))))

(ert-deftest agent-repl-test-uds-deadline-superseded-is-still-marked ()
  "A superseded loss is still recorded, so a late ack is never unexplained."
  ;; Arrange
  (agent-repl-test--with-uds
    (agent-repl-test--with-captured-deadline expire
      (agent-repl-test--pend "req-1" "publishWorkspaceRoster" nil
                             nil nil nil (lambda (_id) t))
      (cl-letf (((symbol-function 'message) (lambda (&rest _) nil)))
        ;; Act
        (funcall expire)
        ;; Assert
        (should (gethash "req-1" agent-repl--uds-timed-out-commands))))))

(ert-deftest agent-repl-test-uds-deadline-unsuperseded-still-degrades ()
  "An `on-timeout' answering NO leaves the loud loss path exactly as it was."
  ;; Arrange
  (agent-repl-test--with-uds
    (agent-repl-test--with-captured-deadline expire
      (agent-repl-test--pend "req-1" "publishWorkspaceRoster" nil
                             nil nil nil (lambda (_id) nil))
      (cl-letf (((symbol-function 'message) (lambda (&rest _) nil)))
        ;; Act
        (funcall expire)
        ;; Assert
        (should (eq (agent-repl-uds-link-health) :degraded))))))

(ert-deftest agent-repl-test-uds-deadline-on-timeout-error-still-surfaces ()
  "A supersede callback that signals is not consent: the loss still surfaces."
  ;; Arrange
  (agent-repl-test--with-uds
    (agent-repl-test--with-captured-deadline expire
      (agent-repl-test--pend "req-1" "publishWorkspaceRoster" nil
                             nil nil nil (lambda (_id) (error "callback broke")))
      (let (echoed)
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (push (apply #'format fmt args) echoed))))
          ;; Act
          (funcall expire)
          ;; Assert
          (should (cl-find-if (lambda (line)
                                (string-match-p "never acknowledged" line))
                              echoed)))))))

(ert-deftest agent-repl-test-uds-deadline-without-on-timeout-surfaces ()
  "A command carrying no supersede callback keeps the unchanged loud path."
  ;; Arrange
  (agent-repl-test--with-uds
    (agent-repl-test--with-captured-deadline expire
      (agent-repl-test--pend "req-1" "mergeWorkspace" "ws1")
      (let (echoed)
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (push (apply #'format fmt args) echoed))))
          ;; Act
          (funcall expire)
          ;; Assert
          (should (cl-find-if (lambda (line)
                                (string-match-p "never acknowledged" line))
                              echoed)))))))

(ert-deftest agent-repl-test-uds-deadline-supersede-question-names-request ()
  "The supersede question hands the sender the request-id that was lost."
  ;; Arrange
  (agent-repl-test--with-uds
    (agent-repl-test--with-captured-deadline expire
      (let (asked)
        (agent-repl-test--pend "req-1" "publishWorkspaceRoster" nil
                               nil nil nil (lambda (id) (setq asked id) t))
        (cl-letf (((symbol-function 'message) (lambda (&rest _) nil)))
          ;; Act
          (funcall expire)
          ;; Assert
          (should (equal asked "req-1")))))))

(ert-deftest agent-repl-test-uds-command-pending-p-while-unacked ()
  "A registered command reads as pending until its ack lands."
  ;; Arrange
  (agent-repl-test--with-uds
    (agent-repl-test--pend "req-1" "mergeWorkspace" "ws1")
    ;; Act / Assert
    (should (agent-repl--uds-command-pending-p "req-1"))))

(ert-deftest agent-repl-test-uds-command-pending-p-after-ack ()
  "An acked command no longer reads as pending."
  ;; Arrange
  (agent-repl-test--with-uds
    (agent-repl-test--pend "req-1" "mergeWorkspace" "ws1")
    ;; Act
    (agent-repl--uds-handle-command-ack '(:requestId "req-1" :ok t))
    ;; Assert
    (should-not (agent-repl--uds-command-pending-p "req-1"))))

(ert-deftest agent-repl-test-uds-command-pending-p-unknown-request ()
  "A request-id this Emacs never sent is not pending."
  ;; Arrange
  (agent-repl-test--with-uds
    ;; Act / Assert
    (should-not (agent-repl--uds-command-pending-p "req-never-sent"))))

(ert-deftest agent-repl-test-uds-deadline-after-ack-is-a-no-op ()
  "A deadline thunk that runs after its ack landed reports nothing."
  ;; Arrange
  (agent-repl-test--with-uds
   (agent-repl-test--with-connected-link
    (agent-repl-test--with-captured-deadline expire
      (agent-repl-test--pend "req-1" "mergeWorkspace" "ws1")
      (agent-repl--uds-handle-command-ack '(:requestId "req-1" :ok t))
      (let (echoed)
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args) (setq echoed (apply #'format fmt args)))))
          ;; Act
          (funcall expire)
          ;; Assert
          (should-not echoed)
          (should (eq (agent-repl-uds-link-health) :healthy))))))))

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
        (agent-repl-test--pend "req-1" "mergeWorkspace" "ws1")
        ;; Act
        (agent-repl--uds-handle-command-ack '(:requestId "req-1" :ok t))
        ;; Assert
        (should (eq cancelled 'armed-timer))))))

(ert-deftest agent-repl-test-uds-ack-before-deadline-leaves-health-clean ()
  "An ack inside the deadline leaves the command link healthy."
  ;; Arrange
  (agent-repl-test--with-uds
   (agent-repl-test--with-connected-link
    (agent-repl-test--pend "req-1" "mergeWorkspace" "ws1")
    ;; Act
    (agent-repl--uds-handle-command-ack '(:requestId "req-1" :ok t))
    ;; Assert
    (should (eq (agent-repl-uds-link-health) :healthy)))))

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
        (agent-repl-test--pend "req-1" "mergeWorkspace" "ws1")
        ;; Act
        (agent-repl--uds-untrack-command "req-1" "ws1" "local-wait-aborted")
        ;; Assert
        (should (eq cancelled 'armed-timer))))))

(ert-deftest agent-repl-test-uds-late-ack-does-not-error ()
  "An ack arriving after its timeout is tolerated, never an error."
  ;; Arrange
  (agent-repl-test--with-uds
    (agent-repl-test--with-captured-deadline expire
      (agent-repl-test--pend "req-1" "mergeWorkspace" "ws1")
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
      (agent-repl-test--pend "req-1" "mergeWorkspace" "ws1")
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
      (agent-repl-test--pend "req-1" "mergeWorkspace" "ws1")
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
      (agent-repl-test--pend "req-1" "mergeWorkspace" "ws1")
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
   (agent-repl-test--with-connected-link
    (agent-repl-test--with-captured-deadline expire
      (agent-repl-test--pend "req-1" "mergeWorkspace" "ws1")
      (cl-letf (((symbol-function 'message) (lambda (&rest _) nil))
                ((symbol-function 'agent-repl--force-tab-bar-redraw)
                 (lambda () nil)))
        (funcall expire)
        (agent-repl-test--pend "req-2" "mergeWorkspace" "ws1")
        ;; Act
        (agent-repl--uds-handle-command-ack '(:requestId "req-2" :ok t))
        ;; Assert
        (should (eq (agent-repl-uds-link-health) :healthy)))))))

(ert-deftest agent-repl-test-uds-rejected-ack-restores-health ()
  "A REJECTED ack still proves the link carried traffic, so health returns."
  ;; Arrange
  (agent-repl-test--with-uds
   (agent-repl-test--with-connected-link
    (agent-repl-test--with-captured-deadline expire
      (agent-repl-test--pend "req-1" "mergeWorkspace" "ws1")
      (cl-letf (((symbol-function 'message) (lambda (&rest _) nil))
                ((symbol-function 'agent-repl--force-tab-bar-redraw)
                 (lambda () nil)))
        (funcall expire)
        (agent-repl-test--pend "req-2" "mergeWorkspace" "ws1")
        ;; Act
        (agent-repl--uds-handle-command-ack
         '(:requestId "req-2" :error "branch not found"))
        ;; Assert
        (should (eq (agent-repl-uds-link-health) :healthy)))))))

(ert-deftest agent-repl-test-uds-untracked-ack-leaves-link-degraded ()
  "An ack for a request nobody tracked is no proof about the command plane."
  ;; Arrange
  (agent-repl-test--with-uds
    (agent-repl--uds-link-degrade "req-1" "mergeWorkspace" "ws1")
    ;; Act
    (agent-repl--uds-handle-command-ack '(:requestId "req-9" :ok t))
    ;; Assert
    (should (eq (agent-repl-uds-link-health) :degraded))))

(ert-deftest agent-repl-test-uds-reconnect-clears-the-ack-latch ()
  "A fresh dial clears the ack latch: the lost command belonged to the old link."
  ;; Arrange
  (agent-repl-test--with-uds
    (cl-letf (((symbol-function 'agent-repl--force-tab-bar-redraw) (lambda () nil)))
      (agent-repl--uds-link-degrade "req-1" "mergeWorkspace" "ws1"))
    (cl-letf (((symbol-function 'agent-repl--uds-connect)
               (lambda (&rest _) 'fake-proc))
              ((symbol-function 'process-name) (lambda (_p) "fake"))
              ((symbol-function 'agent-repl--force-tab-bar-redraw) (lambda () nil))
              ((symbol-function 'process-status) (lambda (_p) 'open)))
      ;; Act
      (agent-repl-uds-connect)
      ;; Assert
      (should (eq agent-repl--uds-link-health :healthy)))))

(ert-deftest agent-repl-test-uds-dial-alone-does-not-report-healthy ()
  "A dial in progress is not a carried command: the reader stays `:degraded'.
The latch is cleared by the dial, so only the reader's connectedness half
keeps this honest until the sentinel reports `open'."
  ;; Arrange
  (agent-repl-test--with-uds
    (cl-letf (((symbol-function 'agent-repl--uds-connect)
               (lambda (&rest _) 'fake-proc))
              ((symbol-function 'process-name) (lambda (_p) "fake"))
              ((symbol-function 'agent-repl--force-tab-bar-redraw) (lambda () nil))
              ((symbol-function 'process-status) (lambda (_p) 'open)))
      ;; Act
      (agent-repl-uds-connect)
      ;; Assert
      (should (eq (agent-repl-uds-link-health) :degraded)))))

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

;;;; ---- The vocabulary matches the generated proto ------------------------
;;
;; This file hand-declares the frame and command oneof spellings the protos
;; already own.  Until the frontends consume a generated constants module,
;; drift is caught HERE, against the checked-in generated bindings: a
;; misspelled arm then fails the suite instead of surfacing as a refused
;; frame or a daemon-side unknown-command NACK at runtime.

(ert-deftest agent-repl-test-uds-known-frame-fields-match-the-proto ()
  "Every declared frame arm is spelled exactly as a `FrontendFrame' oneof arm."
  ;; Arrange
  (let ((generated (agent-repl-test--generated-oneof-arms
                    "agentshim/frontend/v1/frame.pb.go" "FrontendFrame")))
    ;; Act / Assert
    (should generated)
    (should-not (cl-remove-if (lambda (field) (member field generated))
                              agent-repl--uds-known-frame-fields))))

(ert-deftest agent-repl-test-uds-known-command-fields-match-the-proto ()
  "Every declared command arm is spelled exactly as a `FrontendCommand' oneof arm.
The reverse containment is deliberately NOT asserted: `createWorkspace'
exists in the proto and is deliberately absent here."
  ;; Arrange
  (let ((generated (agent-repl-test--generated-oneof-arms
                    "agentshim/frontend/v1/frame.pb.go" "FrontendCommand")))
    ;; Act / Assert
    (should generated)
    (should-not (cl-remove-if (lambda (field) (member field generated))
                              agent-repl--uds-known-command-fields))))

(ert-deftest agent-repl-test-uds-ignored-frame-fields-match-the-proto ()
  "Every deliberately-ignored arm is spelled as a real `FrontendFrame' arm.
A typo here would silently move an arm from the ignored list into the
unfinished-wiring log, which is the opposite of what the list means."
  ;; Arrange
  (let ((generated (agent-repl-test--generated-oneof-arms
                    "agentshim/frontend/v1/frame.pb.go" "FrontendFrame")))
    ;; Act / Assert
    (should generated)
    (should-not (cl-remove-if (lambda (field) (member field generated))
                              agent-repl--uds-ignored-frame-fields))))

;;;; ---- registration precedes the write ---------------------------------
;;
;; `process-send-string' YIELDS TO THE EVENT LOOP on a frame large enough
;; to block, and the connection's own filter then runs the daemon's reply
;; REENTRANTLY inside that yield — before the send has returned to its
;; caller.  Two `publishWorkspaceRoster' commands took exactly that path
;; live: both acks were received and logged as "UNTRACKED ... — ignoring",
;; and ten seconds later the ack-aging alarm surfaced two
;; `client.command_unacked' failure cards for commands the daemon had
;; already retained.  These tests drive that exact interleaving.

(defmacro agent-repl-test--with-reentrant-ack (ack &rest body)
  "Run BODY with the write seam delivering ACK reentrantly, before it returns.
Stands in for the real `process-send-string' yield in which the connection
filter runs the daemon's answer while the send is still on the stack."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'process-live-p) (lambda (p) (eq p 'fake-proc)))
             ((symbol-function 'agent-repl--uds-generate-request-id)
              (lambda () "req-reentrant"))
             ((symbol-function 'process-send-string)
              (lambda (_proc _s) (agent-repl--uds-handle-command-ack ,ack))))
     (let ((agent-repl--uds-connection-state 'open)
           (agent-repl--uds-outbound-queue nil))
       ,@body)))

(ert-deftest agent-repl-test-uds-reentrant-ack-is-matched ()
  "An ack delivered inside the write is matched, not logged as UNTRACKED."
  ;; Arrange
  (agent-repl-test--with-uds
    (let (acked)
      (agent-repl-test--with-reentrant-ack '(:requestId "req-reentrant" :ok t)
        ;; Act
        (agent-repl--uds-send-command
         "publishWorkspaceRoster" '(:roster nil) nil 'fake-proc
         :on-success (lambda () (setq acked t)))
        ;; Assert
        (should acked)))))

(ert-deftest agent-repl-test-uds-reentrant-ack-disarms-the-deadline ()
  "The reentrant ack cancels the alarm that would have declared it lost."
  ;; Arrange
  (agent-repl-test--with-uds
    (let (cancelled)
      (cl-letf (((symbol-function 'agent-repl--uds-run-timer)
                 (lambda (&rest _) 'deadline-timer))
                ((symbol-function 'agent-repl--uds-cancel-ack-deadline)
                 (lambda (pending)
                   (setq cancelled (plist-get pending :deadline-timer)))))
        (agent-repl-test--with-reentrant-ack '(:requestId "req-reentrant" :ok t)
          ;; Act
          (agent-repl--uds-send-command
           "publishWorkspaceRoster" '(:roster nil) nil 'fake-proc)))
      ;; Assert
      (should (eq cancelled 'deadline-timer)))))

(ert-deftest agent-repl-test-uds-reentrant-ack-drops-the-pending-entry ()
  "The reentrant ack settles the command: nothing is left pending."
  ;; Arrange
  (agent-repl-test--with-uds
    (agent-repl-test--with-reentrant-ack '(:requestId "req-reentrant" :ok t)
      ;; Act
      (agent-repl--uds-send-command
       "publishWorkspaceRoster" '(:roster nil) nil 'fake-proc))
    ;; Assert
    (should-not (gethash "req-reentrant" agent-repl--uds-pending-commands))))

(ert-deftest agent-repl-test-uds-reentrant-ack-leaves-the-link-healthy ()
  "A command answered inside its own write never degrades the link."
  ;; Arrange
  (agent-repl-test--with-uds
   (agent-repl-test--with-connected-link
    (agent-repl-test--with-reentrant-ack '(:requestId "req-reentrant" :ok t)
      ;; Act
      (agent-repl--uds-send-command
       "publishWorkspaceRoster" '(:roster nil) nil 'fake-proc))
    ;; Assert
    (should (eq (agent-repl-uds-link-health) :healthy)))))

(ert-deftest agent-repl-test-uds-reentrant-ack-surfaces-no-failure ()
  "No failure is surfaced for a command the daemon demonstrably answered."
  ;; Arrange
  (agent-repl-test--with-uds
    (let (surfaced)
      (cl-letf (((symbol-function 'agent-repl-failure-surface)
                 (lambda (&rest args) (push args surfaced))))
        (agent-repl-test--with-reentrant-ack '(:requestId "req-reentrant" :ok t)
          ;; Act
          (agent-repl--uds-send-command
           "publishWorkspaceRoster" '(:roster nil) nil 'fake-proc)))
      ;; Assert
      (should-not surfaced))))

(ert-deftest agent-repl-test-uds-two-interleaved-sends-are-both-matched ()
  "Two sends whose acks arrive inside their own writes both settle.
The live failure interleaved two roster publishes and unwound them in
reverse order; neither was tracked when its ack landed."
  ;; Arrange
  (agent-repl-test--with-uds
    (let ((ids '("fe-108" "fe-109")) settled)
      (cl-letf (((symbol-function 'process-live-p) (lambda (p) (eq p 'fake-proc)))
                ((symbol-function 'agent-repl--uds-generate-request-id)
                 (lambda () (pop ids)))
                ((symbol-function 'process-send-string)
                 (lambda (_proc _s) nil)))
        (let ((agent-repl--uds-connection-state 'open)
              (agent-repl--uds-outbound-queue nil))
          ;; Act — both acks arrive after both sends, out of order.
          (agent-repl--uds-send-command
           "publishWorkspaceRoster" '(:roster nil) nil 'fake-proc
           :on-success (lambda () (push "fe-108" settled)))
          (agent-repl--uds-send-command
           "publishWorkspaceRoster" '(:roster nil) nil 'fake-proc
           :on-success (lambda () (push "fe-109" settled)))
          (agent-repl--uds-handle-command-ack '(:requestId "fe-109" :ok t))
          (agent-repl--uds-handle-command-ack '(:requestId "fe-108" :ok t))))
      ;; Assert
      (should (equal (sort settled #'string<) '("fe-108" "fe-109"))))))

(ert-deftest agent-repl-test-uds-registration-precedes-the-write ()
  "The pending entry EXISTS by the time the first byte reaches the socket.
This is the structural guarantee: no schedule can put an ack ahead of its
own tracking, because the tracking is already done when the write starts."
  ;; Arrange
  (agent-repl-test--with-uds
    (let (pending-at-write)
      (cl-letf (((symbol-function 'process-live-p) (lambda (p) (eq p 'fake-proc)))
                ((symbol-function 'agent-repl--uds-generate-request-id)
                 (lambda () "req-order"))
                ((symbol-function 'process-send-string)
                 (lambda (_proc _s)
                   (setq pending-at-write
                         (gethash "req-order" agent-repl--uds-pending-commands)))))
        (let ((agent-repl--uds-connection-state 'open)
              (agent-repl--uds-outbound-queue nil))
          ;; Act
          (agent-repl--uds-send-command "interrupt" nil "ws1" 'fake-proc)))
      ;; Assert
      (should (equal (plist-get pending-at-write :field) "interrupt")))))

(ert-deftest agent-repl-test-uds-queued-send-registers-before-queueing ()
  "A frame withheld while `dialing' is tracked the moment it is queued.
The sentinel writes it much later, so an entry registered only after the
send returned would be no safer here than on the immediate path."
  ;; Arrange
  (agent-repl-test--with-uds
    (cl-letf (((symbol-function 'process-live-p) (lambda (_p) t))
              ((symbol-function 'process-name) (lambda (_p) "fake"))
              ((symbol-function 'process-send-string) (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--uds-generate-request-id)
               (lambda () "queued-track-1")))
      (let ((agent-repl--uds-process 'fake-proc)
            (agent-repl--uds-connection-state 'dialing)
            (agent-repl--uds-connect-started-at (float-time))
            (agent-repl--uds-outbound-queue nil))
        ;; Act
        (agent-repl--uds-send-command "interrupt" '(:hard t) "ws1")
        ;; Assert
        (should (gethash "queued-track-1" agent-repl--uds-pending-commands))))))

(ert-deftest agent-repl-test-uds-queued-send-ack-after-flush-settles ()
  "The queued frame's ack, arriving after the sentinel flush, still settles."
  ;; Arrange
  (agent-repl-test--with-uds
    (let (acked)
      (cl-letf (((symbol-function 'process-live-p) (lambda (_p) t))
                ((symbol-function 'process-name) (lambda (_p) "fake"))
                ((symbol-function 'process-send-string) (lambda (&rest _) nil))
                ((symbol-function 'agent-repl--uds-generate-request-id)
                 (lambda () "queued-track-2")))
        (let ((agent-repl--uds-process 'fake-proc)
              (agent-repl--uds-connection-state 'dialing)
              (agent-repl--uds-connect-started-at (float-time))
              (agent-repl--uds-outbound-queue nil)
              ;; The connected hook would send commands of its own through
              ;; the stubbed id generator, colliding with this request-id.
              (agent-repl-uds-connected-functions nil))
          ;; Act
          (agent-repl--uds-send-command
           "interrupt" '(:hard t) "ws1" nil
           :on-success (lambda () (setq acked t)))
          (agent-repl--uds-sentinel 'fake-proc "open\n")
          (agent-repl--uds-handle-command-ack '(:requestId "queued-track-2" :ok t))))
      ;; Assert
      (should acked))))

(ert-deftest agent-repl-test-uds-queued-send-dial-failure-runs-on-failure ()
  "A dial that dies before delivery settles the queued command as failed.
The command is tracked from the moment it is queued, so the synthesized
failure ack reaches the caller instead of aging out ten seconds later."
  ;; Arrange
  (agent-repl-test--with-uds
    (let ((live t) failure)
      (cl-letf (((symbol-function 'process-live-p) (lambda (_p) live))
                ((symbol-function 'process-name) (lambda (_p) "fake"))
                ((symbol-function 'agent-repl--uds-generate-request-id)
                 (lambda () "queued-dead-1")))
        (let ((agent-repl--uds-process 'fake-proc)
              (agent-repl--uds-connection-state 'dialing)
              (agent-repl--uds-connect-started-at (float-time))
              (agent-repl--uds-outbound-queue nil))
          ;; Act
          (agent-repl--uds-send-command
           "interrupt" '(:hard t) "ws1" nil
           :on-failure (lambda (err) (setq failure err)))
          (setq live nil)
          (agent-repl--uds-sentinel 'fake-proc "connection broken\n")))
      ;; Assert
      (should (equal failure "UDS dial failed before command delivery")))))

(ert-deftest agent-repl-test-uds-on-registered-runs-before-the-write ()
  "`:on-registered' fires after registration and before the first byte."
  ;; Arrange
  (agent-repl-test--with-uds
    (let (order)
      (cl-letf (((symbol-function 'process-live-p) (lambda (p) (eq p 'fake-proc)))
                ((symbol-function 'agent-repl--uds-generate-request-id)
                 (lambda () "req-hook"))
                ((symbol-function 'process-send-string)
                 (lambda (_proc _s) (push 'written order))))
        (let ((agent-repl--uds-connection-state 'open)
              (agent-repl--uds-outbound-queue nil))
          ;; Act
          (agent-repl--uds-send-command
           "interrupt" nil "ws1" 'fake-proc
           :on-registered (lambda (_id) (push 'registered order)))))
      ;; Assert
      (should (equal (nreverse order) '(registered written))))))

(ert-deftest agent-repl-test-uds-on-registered-failure-untracks-and-signals ()
  "A signalling `:on-registered' unregisters the command and propagates.
Nothing was written, so leaving the entry armed would age out a command
that never went anywhere."
  ;; Arrange
  (agent-repl-test--with-uds
    (let (written)
      (cl-letf (((symbol-function 'process-live-p) (lambda (p) (eq p 'fake-proc)))
                ((symbol-function 'agent-repl--uds-generate-request-id)
                 (lambda () "req-hook-bad"))
                ((symbol-function 'process-send-string)
                 (lambda (_proc _s) (setq written t))))
        (let ((agent-repl--uds-connection-state 'open)
              (agent-repl--uds-outbound-queue nil))
          ;; Act / Assert
          (should-error
           (agent-repl--uds-send-command
            "interrupt" nil "ws1" 'fake-proc
            :on-registered (lambda (_id) (error "hook is broken"))))
          (should-not written)
          (should-not (gethash "req-hook-bad"
                               agent-repl--uds-pending-commands)))))))

(ert-deftest agent-repl-test-uds-unanswered-send-still-ages-out ()
  "A command the daemon never answers still degrades the link and surfaces.
The fix must not buy a quiet reentrant ack at the price of the aging that
caught three vanished `mergeWorkspace' commands."
  ;; Arrange
  (agent-repl-test--with-uds
    (let (surfaced)
      (agent-repl-test--with-captured-deadline expire
        (cl-letf (((symbol-function 'process-live-p) (lambda (p) (eq p 'fake-proc)))
                  ((symbol-function 'agent-repl--uds-generate-request-id)
                   (lambda () "req-lost"))
                  ((symbol-function 'process-send-string) (lambda (&rest _) nil))
                  ((symbol-function 'agent-repl-failure-surface)
                   (lambda (_ws failure &optional _verb) (push failure surfaced))))
          (let ((agent-repl--uds-connection-state 'open)
                (agent-repl--uds-outbound-queue nil))
            (agent-repl--uds-send-command
             "mergeWorkspace" '(:workspaceName "ws1") "ws1" 'fake-proc)
            ;; Act — no ack ever arrives; the alarm fires.
            (funcall expire))))
      ;; Assert
      (should (eq (agent-repl-uds-link-health) :degraded))
      (should (= (length surfaced) 1))
      (should (equal (plist-get (car surfaced) :type) "client.command_unacked")))))

(ert-deftest agent-repl-test-uds-unanswered-send-logs-its-loss ()
  "The aged-out command is recorded through the canonical log helper."
  ;; Arrange
  (agent-repl-test--with-uds
    (let (logged)
      (agent-repl-test--with-captured-deadline expire
        (cl-letf (((symbol-function 'process-live-p) (lambda (p) (eq p 'fake-proc)))
                  ((symbol-function 'agent-repl--uds-generate-request-id)
                   (lambda () "req-lost-2"))
                  ((symbol-function 'process-send-string) (lambda (&rest _) nil))
                  ((symbol-function 'agent-repl-failure-surface) (lambda (&rest _) nil))
                  ((symbol-function 'agent-repl--log)
                   (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logged))))
          (let ((agent-repl--uds-connection-state 'open)
                (agent-repl--uds-outbound-queue nil))
            (agent-repl--uds-send-command
             "mergeWorkspace" '(:workspaceName "ws1") "ws1" 'fake-proc)
            ;; Act
            (funcall expire))))
      ;; Assert
      (should (cl-some (lambda (line)
                         (and (string-match-p "UNACKED" line)
                              (string-match-p "req-lost-2" line)
                              (string-match-p "mergeWorkspace" line)))
                       logged)))))

;; An Emacs that blocked its own event loop through a workspace restore reads
;; nothing, so the daemon's writer blocks mid-write and every ack it produced
;; is delivered in one burst the instant Emacs becomes responsive.  At that
;; instant the due alarms and the arrived bytes are both ready and nothing
;; orders them, so an alarm could card a command whose successful ack it was
;; about to read.  The alarm now looks at the link first.

(ert-deftest agent-repl-test-uds-deadline-drain-settles-an-arrived-ack ()
  "An ack already on the link at alarm time opens no failure card."
  ;; Arrange
  (agent-repl-test--with-uds
    (let (surfaced)
      (agent-repl-test--with-captured-deadline expire
        (cl-letf (((symbol-function 'agent-repl-failure-surface)
                   (lambda (_ws failure &optional _verb) (push failure surfaced)))
                  ((symbol-function 'agent-repl--uds-drain-input)
                   (lambda ()
                     (agent-repl--uds-handle-command-ack
                      '(:requestId "req-arrived" :ok t))
                     t)))
          (agent-repl-test--pend "req-arrived" "openWorkspace" "ws1")
          ;; Act — the alarm fires on bytes Emacs had not yet looked at.
          (funcall expire)))
      ;; Assert
      (should-not surfaced))))

(ert-deftest agent-repl-test-uds-deadline-drain-keeps-the-link-healthy ()
  "A command settled by the pre-verdict drain never degrades the link."
  ;; Arrange
  (agent-repl-test--with-uds
   (agent-repl-test--with-connected-link
    (agent-repl-test--with-captured-deadline expire
      (cl-letf (((symbol-function 'agent-repl-failure-surface) (lambda (&rest _) nil))
                ((symbol-function 'agent-repl--uds-drain-input)
                 (lambda ()
                   (agent-repl--uds-handle-command-ack
                    '(:requestId "req-healthy" :ok t))
                   t)))
        (agent-repl-test--pend "req-healthy" "openWorkspace" "ws1")
        ;; Act
        (funcall expire)))
    ;; Assert
    (should (eq (agent-repl-uds-link-health) :healthy)))))

(ert-deftest agent-repl-test-uds-deadline-drain-runs-the-success-callback ()
  "The drained ack runs `:on-success' rather than abandoning it.
The presentation gate resolves openWorkspace on that callback, so an
abandoned one strands the mount until its own separate timeout."
  ;; Arrange
  (agent-repl-test--with-uds
    (let (ran)
      (agent-repl-test--with-captured-deadline expire
        (cl-letf (((symbol-function 'agent-repl-failure-surface) (lambda (&rest _) nil))
                  ((symbol-function 'agent-repl--uds-drain-input)
                   (lambda ()
                     (agent-repl--uds-handle-command-ack
                      '(:requestId "req-cb" :ok t))
                     t)))
          (agent-repl-test--pend "req-cb" "openWorkspace" "ws1"
                                 nil (lambda () (setq ran t)))
          ;; Act
          (funcall expire)))
      ;; Assert
      (should ran))))

(ert-deftest agent-repl-test-uds-deadline-drain-empty-still-declares-the-loss ()
  "A drain that finds nothing leaves the loud loss path exactly as it was."
  ;; Arrange
  (agent-repl-test--with-uds
    (let (surfaced)
      (agent-repl-test--with-captured-deadline expire
        (cl-letf (((symbol-function 'agent-repl-failure-surface)
                   (lambda (_ws failure &optional _verb) (push failure surfaced)))
                  ((symbol-function 'agent-repl--uds-drain-input) (lambda () nil)))
          (agent-repl-test--pend "req-really-lost" "mergeWorkspace" "ws1")
          ;; Act
          (funcall expire)))
      ;; Assert
      (should (= (length surfaced) 1))
      (should (equal (plist-get (car surfaced) :type) "client.command_unacked")))))

(ert-deftest agent-repl-test-uds-deadline-drain-error-still-declares-the-loss ()
  "A drain that SIGNALS is never read as evidence the command survived."
  ;; Arrange
  (agent-repl-test--with-uds
    (let (surfaced)
      (agent-repl-test--with-captured-deadline expire
        (cl-letf (((symbol-function 'agent-repl-failure-surface)
                   (lambda (_ws failure &optional _verb) (push failure surfaced)))
                  ((symbol-function 'agent-repl--uds-drain-input)
                   (lambda () (error "filter handler blew up"))))
          (agent-repl-test--pend "req-drain-err" "mergeWorkspace" "ws1")
          ;; Act
          (funcall expire)))
      ;; Assert
      (should (= (length surfaced) 1))
      (should (equal (plist-get (car surfaced) :type) "client.command_unacked")))))

(ert-deftest agent-repl-test-uds-deadline-drain-error-is-logged ()
  "The failed drain is recorded, never silently passed over."
  ;; Arrange
  (agent-repl-test--with-uds
    (let (logged)
      (agent-repl-test--with-captured-deadline expire
        (cl-letf (((symbol-function 'agent-repl-failure-surface) (lambda (&rest _) nil))
                  ((symbol-function 'agent-repl--log)
                   (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logged)))
                  ((symbol-function 'agent-repl--uds-drain-input)
                   (lambda () (error "filter handler blew up"))))
          (agent-repl-test--pend "req-drain-log" "mergeWorkspace" "ws1")
          ;; Act
          (funcall expire)))
      ;; Assert
      (should (cl-some (lambda (line)
                         (and (string-match-p "pre-verdict drain ERROR" line)
                              (string-match-p "req-drain-log" line)))
                       logged)))))

(ert-deftest agent-repl-test-uds-deadline-drain-does-not-nest ()
  "An alarm reached from inside a drain does not start a second read."
  ;; Arrange
  (agent-repl-test--with-uds
    (let ((reads 0))
      (agent-repl-test--with-captured-deadline expire
        (cl-letf (((symbol-function 'agent-repl-failure-surface) (lambda (&rest _) nil))
                  ((symbol-function 'agent-repl--uds-drain-input)
                   (lambda ()
                     (setq reads (1+ reads))
                     ;; The filter dispatched a frame, and a second due alarm
                     ;; ran reentrantly off the same read.
                     (agent-repl--uds-command-deadline-expired "req-inner")
                     nil)))
          (agent-repl-test--pend "req-outer" "mergeWorkspace" "ws1")
          (agent-repl-test--pend "req-inner" "mergeWorkspace" "ws2")
          ;; Act
          (funcall expire)))
      ;; Assert
      (should (= reads 1)))))

(ert-deftest agent-repl-test-uds-drain-input-skips-a-dead-link ()
  "A disconnected link has nothing to drain and is not an error."
  ;; Arrange
  (agent-repl-test--with-uds
    (let ((agent-repl--uds-process nil))
      ;; Act / Assert
      (should-not (agent-repl--uds-drain-input)))))

(ert-deftest agent-repl-test-uds-drain-input-reads-a-live-link ()
  "A live link is read with a ZERO timeout, so no deadline is extended."
  ;; Arrange
  (agent-repl-test--with-uds
    (let ((agent-repl--uds-process 'fake-proc)
          timeout)
      (cl-letf (((symbol-function 'process-live-p) (lambda (p) (eq p 'fake-proc)))
                ((symbol-function 'accept-process-output)
                 (lambda (_proc secs) (setq timeout secs) t)))
        ;; Act
        (should (agent-repl--uds-drain-input))
        ;; Assert
        (should (equal timeout 0))))))

(ert-deftest agent-repl-test-uds-untrack-still-cleans-an-aborted-wait ()
  "An aborted synchronous wait can still retire its own registration."
  ;; Arrange
  (agent-repl-test--with-uds
    (agent-repl-test--with-captured-deadline expire
      (cl-letf (((symbol-function 'process-live-p) (lambda (p) (eq p 'fake-proc)))
                ((symbol-function 'agent-repl--uds-generate-request-id)
                 (lambda () "req-abandoned"))
                ((symbol-function 'process-send-string) (lambda (&rest _) nil)))
        (let ((agent-repl--uds-connection-state 'open)
              ;; The link the command goes out on is LIVE: the reader
              ;; reports a disconnected link as degraded, and this test is
              ;; about the abandoned registration, not about link loss.
              (agent-repl--uds-process 'fake-proc)
              (agent-repl--uds-outbound-queue nil))
          (agent-repl--uds-send-command "interrupt" nil "ws1" 'fake-proc)
          ;; Act
          (agent-repl--uds-untrack-command "req-abandoned" "ws1" "caller-gave-up")
          ;; Assert
          (should-not (gethash "req-abandoned" agent-repl--uds-pending-commands))
          (funcall expire)
          (should (eq (agent-repl-uds-link-health) :healthy)))))))

;;;; ---- filter/sentinel: quit deferral ----------------------------------

(ert-deftest agent-repl-test-uds-filter-defers-a-quit-raised-mid-drain ()
  "A C-g inside a handler does not abandon the frames buffered behind it."
  ;; Arrange
  (agent-repl-test--with-uds
    (let ((workspaces nil))
      (agent-repl--uds-register-handler
       "workspaceState"
       (lambda (p)
         (push (plist-get p :workspace) workspaces)
         ;; What Emacs does when C-g arrives while `inhibit-quit' is bound.
         (when (equal (plist-get p :workspace) "a") (setq quit-flag t))))
      ;; Act
      (agent-repl-test--quit-deferred-p
        (agent-repl--uds-filter
         nil (concat "{\"workspaceState\":{\"workspace\":\"a\"}}\n"
                     "{\"workspaceState\":{\"workspace\":\"b\"}}\n")))
      ;; Assert
      (should (equal (nreverse workspaces) '("a" "b"))))))

(ert-deftest agent-repl-test-uds-filter-leaves-the-deferred-quit-armed ()
  "The deferred quit is re-signalled by the command loop, never dropped."
  ;; Arrange
  (agent-repl-test--with-uds
    (agent-repl--uds-register-handler
     "workspaceState" (lambda (_p) (setq quit-flag t)))
    ;; Act / Assert
    (should (agent-repl-test--quit-deferred-p
              (agent-repl--uds-filter
               nil "{\"workspaceState\":{\"workspace\":\"a\"}}\n")))))

(ert-deftest agent-repl-test-uds-filter-still-re-signals-a-handler-error ()
  "Quit deferral does not soften the drain's own error re-signalling."
  ;; Arrange
  (agent-repl-test--with-uds
    (agent-repl--uds-register-handler
     "workspaceState" (lambda (_p) (error "handler boom")))
    ;; Act / Assert
    (should-error
     (agent-repl--uds-filter nil "{\"workspaceState\":{\"workspace\":\"a\"}}\n")
     :type 'error)))

;;;; ---- the session restart: no waiting, no half-registered commands -----

(ert-deftest agent-repl-test-uds-restart-timeout-surfaces-the-failure-card ()
  "A restart the daemon never acknowledges opens the unacked-command card."
  ;; Arrange
  (agent-repl-test--with-uds
    (agent-repl-test--with-captured-deadline expire
      (agent-repl-test--pend "req-1" "restartSession" "/w")
      (let (surfaced)
        (cl-letf (((symbol-function 'agent-repl-failure-surface)
                   (lambda (_ws failure &rest _) (push failure surfaced)))
                  ((symbol-function 'message) (lambda (&rest _) nil)))
          ;; Act
          (funcall expire)
          ;; Assert
          (should (equal (plist-get (car surfaced) :type)
                         "client.command_unacked")))))))

(ert-deftest agent-repl-test-uds-restart-timeout-names-the-verb-in-english ()
  "The unacked restart card says `session restart', never the wire field."
  ;; Arrange
  (agent-repl-test--with-uds
    (agent-repl-test--with-captured-deadline expire
      (agent-repl-test--pend "req-1" "restartSession" "/w")
      (let (echoed)
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args) (setq echoed (apply #'format fmt args)))))
          ;; Act
          (funcall expire)
          ;; Assert
          (should (string-match-p "session restart" echoed))
          (should-not (string-match-p "restartSession" echoed)))))))

(ert-deftest agent-repl-test-uds-send-command-defers-a-quit-raised-mid-write ()
  "A C-g during a congested write is deferred rather than taken mid-send."
  ;; Arrange
  (agent-repl-test--with-uds
    (let (sent)
      (agent-repl-test--capturing-send sent
        (cl-letf (((symbol-function 'process-send-string)
                   ;; What a C-g held during a congested write amounts to.
                   (lambda (_proc s) (setq sent s quit-flag t))))
          ;; Act / Assert
          (should (agent-repl-test--quit-deferred-p
                    (agent-repl--uds-send-command
                     "restartSession" nil "/w" 'fake-proc))))))))

(ert-deftest agent-repl-test-uds-send-command-quit-leaves-the-registry-consistent ()
  "A quit mid-send never leaves a tracked command whose frame was not delivered."
  ;; Arrange
  (agent-repl-test--with-uds
    (let ((request-id nil)
          sent)
      (agent-repl-test--capturing-send sent
        (cl-letf (((symbol-function 'process-send-string)
                   (lambda (_proc s) (setq sent s quit-flag t))))
          ;; Act
          (agent-repl-test--quit-deferred-p
            (setq request-id
                  (agent-repl--uds-send-command
                   "restartSession" nil "/w" 'fake-proc)))))
      ;; Assert — the command is tracked AND its frame reached the wire, so
      ;; the ack-aging alarm can never declare lost a send that happened.
      (should (agent-repl--uds-command-pending-p request-id))
      (should (string-match-p request-id (or sent ""))))))

;;;; ---- the sentinel's own quit deferral ---------------------------------

(ert-deftest agent-repl-test-uds-sentinel-defers-a-quit-raised-mid-transition ()
  "A C-g during a link transition is deferred rather than taken mid-rewrite.
The transition rewrites the connection state, drains the outbound queue
and re-arms the reconnect ladder; a quit landing between those steps
leaves the link recorded as neither up nor down."
  ;; Arrange
  (agent-repl-test--with-uds
    (cl-letf (((symbol-function 'agent-repl--uds-sentinel-transition)
               ;; What Emacs does when C-g arrives while `inhibit-quit' is
               ;; bound: the request is recorded, not taken.
               (lambda (&rest _) (setq quit-flag t))))
      ;; Act / Assert
      (should (agent-repl-test--quit-deferred-p
                (agent-repl--uds-sentinel
                 'dead-proc "connection broken by remote peer\n"))))))

(ert-deftest agent-repl-test-uds-sentinel-completes-its-transition-under-a-quit ()
  "A quit mid-transition never leaves the link recorded as neither up nor down."
  ;; Arrange
  (agent-repl-test--with-uds
    (setq agent-repl--uds-process 'dead-proc
          agent-repl--uds-read-accumulator "leftover")
    (let (scheduled)
      (cl-letf (((symbol-function 'process-live-p) (lambda (_p) nil))
                ((symbol-function 'process-name) (lambda (_p) "dead"))
                ((symbol-function 'agent-repl--uds-run-timer)
                 (lambda (delay fn) (setq scheduled (list delay fn)) 'fake-timer)))
        ;; Act -- the C-g lands while the transition is running.
        (agent-repl-test--with-pending-quit
          (agent-repl--uds-sentinel
           'dead-proc "connection broken by remote peer\n")))
      ;; Assert -- the whole transition ran despite the quit.
      (should-not agent-repl--uds-process)
      (should (string-empty-p agent-repl--uds-read-accumulator))
      (should (eq (nth 1 scheduled) #'agent-repl-uds-connect)))))

;;;; ---- restartPending, the intentional-restart announcement -------------

(ert-deftest agent-repl-test-uds-restart-pending-is-a-known-frame-field ()
  "The arm is on the allowlist, so a pushed announcement decodes rather than signals."
  ;; Arrange / Act / Assert
  (should (member "restartPending" agent-repl--uds-known-frame-fields)))

(ert-deftest agent-repl-test-uds-restart-pending-is-not-an-ignored-frame-field ()
  "The arm has a real handler, so it is not a deliberately-unrendered arm."
  ;; Arrange / Act / Assert
  (should-not (member "restartPending" agent-repl--uds-ignored-frame-fields)))

(ert-deftest agent-repl-test-uds-restart-pending-dispatches-to-its-handler ()
  "A pushed announcement reaches its handler with the view intact."
  ;; Arrange
  (agent-repl-test--with-uds
    (let (captured)
      (agent-repl--uds-register-handler
       "restartPending" (lambda (v) (setq captured v)))
      ;; Act
      (agent-repl--uds-dispatch-frame
       '(:restartPending (:cause "deploy-all" :expectedOutageSeconds 60
                          :stopShims t :announcedAtMs 1)))
      ;; Assert
      (should (equal (plist-get captured :cause) "deploy-all")))))
