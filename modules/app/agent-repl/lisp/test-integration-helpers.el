;;; test-integration-helpers.el --- Fake-daemon harness for the integration suite -*- lexical-binding: t; -*-

;;; Commentary:

;; Infrastructure for `test-integration-*.el': a REAL agentrepl.v1 server
;; (lisp/testsupport/fakedaemon, Go) that Emacs's production transport talks
;; to over a real loopback socket, driven from the test through the fake's
;; /_fake/ control plane.
;;
;; What this is NOT: an end-to-end harness.  The fake daemon mocks Emacs's
;; one NEIGHBOR and composes no other system — no shim, no store, no webapp,
;; no vendor.  `AGENT_REPL_FORBID_VENDOR_CALLS=1' is exported into every
;; process this file starts.
;;
;; What it buys over stubbing the transport in elisp: every request Emacs
;; sends is unmarshalled into the GENERATED Go types and every answer is
;; marshalled out of them, so the suite pins elisp's encoders and decoders
;; against the frozen schema itself rather than against another elisp
;; opinion of it.  The fake refuses unknown fields (protojson's default
;; strictness, restored) and enforces the validation invariant.
;;
;; BATCH-ONLY, exactly like `test-helpers.el', whose gating this file
;; inherits by loading it first: it starts subprocesses, redirects the state
;; dir, and restores one external-boundary wrapper.  Never load it into a
;; live Emacs.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'subr-x)

(load (expand-file-name "test-helpers.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Locations ----

(defconst agent-repl-itest--lisp-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Absolute path of the `lisp/' directory holding this harness.")

(defconst agent-repl-itest--fakedaemon-source-dir
  (expand-file-name "testsupport/fakedaemon" agent-repl-itest--lisp-dir)
  "Absolute path of the fake daemon's Go module.")

;; `test-helpers.el' is loaded above at RUNTIME, so the byte-compiler does not
;; see its definitions; declare the one variable this file reads out of it.
(defvar agent-repl-test--external-original-functions)

(defvar agent-repl-itest--binary nil
  "Absolute path of the fake daemon binary built for this Emacs process.
Built once per run by `agent-repl-itest--ensure-binary'.")

;;;; ---- Building the fake ----

(defun agent-repl-itest--go-program ()
  "Return the absolute path of the `go' toolchain, or signal.
A missing toolchain FAILS the suite loudly rather than skipping it: a
skipped integration suite is a suite everyone believes is running and
nothing is."
  (or (executable-find "go")
      (error (concat "agent-repl-itest: no `go' toolchain on PATH — the integration "
                     "suite needs it to build lisp/testsupport/fakedaemon.  This is a "
                     "FAILURE, not a skip: the suite cannot be believed without it"))))

(defun agent-repl-itest--ensure-binary ()
  "Build the fake daemon once per Emacs process and return its path.
Builds OFFLINE (`GOPROXY=off') against the module cache, so a run never
depends on the network."
  (or (and agent-repl-itest--binary (file-executable-p agent-repl-itest--binary)
           agent-repl-itest--binary)
      (let* ((go (agent-repl-itest--go-program))
             (output (expand-file-name (format "agent-repl-fakedaemon-%d" (emacs-pid))
                                       temporary-file-directory))
             (default-directory agent-repl-itest--fakedaemon-source-dir)
             (process-environment (append '("GOFLAGS=-mod=mod" "GOPROXY=off")
                                          process-environment))
             (log (generate-new-buffer "*agent-repl-itest-go-build*"))
             (status (call-process go nil log nil "build" "-o" output ".")))
        (unless (eq status 0)
          (let ((text (with-current-buffer log (buffer-string))))
            (kill-buffer log)
            (error "agent-repl-itest: building the fake daemon failed (exit %s):\n%s"
                   status text)))
        (kill-buffer log)
        (setq agent-repl-itest--binary output))))

;;;; ---- The running fake ----

(cl-defstruct (agent-repl-itest-daemon (:constructor agent-repl-itest--make-daemon))
  "One running fake daemon instance.
PROCESS is its Emacs process object, STATE-DIR the private
`AGENT_REPL_STATE_DIR' it was started with, ADDRESS the `127.0.0.1:PORT'
it wrote to that dir's `daemon.addr', and STDERR-BUFFER its structured
JSON log."
  process state-dir address stderr-buffer)

(defconst agent-repl-itest-default-timeout 15
  "Seconds `agent-repl-itest--wait-until' waits before failing.")

(defun agent-repl-itest--wait-until (pred &optional timeout description)
  "Block until PRED returns non-nil, or fail after TIMEOUT seconds.
Waits through `accept-process-output' so subprocess output and process
sentinels are actually served while waiting — a `sleep-for' would block
the very I/O the predicate is waiting on.  DESCRIPTION names the
condition in the failure message; without one the predicate itself is
printed, because a timeout that does not say what it was waiting for is
almost useless."
  (let* ((limit (or timeout agent-repl-itest-default-timeout))
         (deadline (+ (float-time) limit)))
    (catch 'agent-repl-itest--satisfied
      (while t
        (when (funcall pred)
          (throw 'agent-repl-itest--satisfied t))
        (when (> (float-time) deadline)
          (error "agent-repl-itest: timed out after %ss waiting for %s"
                 limit (or description (prin1-to-string pred))))
        ;; A nil PROCESS argument serves every process's output and runs any
        ;; pending sentinel, then returns after the interval at the latest.
        (accept-process-output nil 0.02)))))

(defun agent-repl-itest--private-state-dir ()
  "Create and return a fresh private `AGENT_REPL_STATE_DIR'."
  (file-name-as-directory (make-temp-file "agent-repl-itest-state-" t)))

(defun agent-repl-itest--addr-file (state-dir)
  "Return the `daemon.addr' path under STATE-DIR."
  (expand-file-name "daemon.addr" state-dir))

(defun agent-repl-itest--read-addr-file (state-dir)
  "Return the address in STATE-DIR's `daemon.addr', or nil when unwritten."
  (let ((path (agent-repl-itest--addr-file state-dir)))
    (when (and (file-exists-p path) (> (file-attribute-size (file-attributes path)) 0))
      (with-temp-buffer
        (insert-file-contents path)
        (string-trim (buffer-string))))))

(defun agent-repl-itest--start-daemon (&optional state-dir)
  "Start one fake daemon in STATE-DIR (a fresh private dir by default).
Waits for it to publish `daemon.addr' and returns the
`agent-repl-itest-daemon' describing it.  The process environment carries
`AGENT_REPL_STATE_DIR' and `AGENT_REPL_FORBID_VENDOR_CALLS=1'; nothing
this process can reach is a vendor, and the variable says so anyway."
  (let* ((binary (agent-repl-itest--ensure-binary))
         (dir (or state-dir (agent-repl-itest--private-state-dir)))
         (stderr (generate-new-buffer (format " *agent-repl-itest-fakedaemon-log %s*"
                                              (file-name-nondirectory
                                               (directory-file-name dir)))))
         (process
          ;; `make-process' has no environment keyword: the child inherits
          ;; `process-environment', so the two variables are bound around the
          ;; spawn.  AGENT_REPL_FORBID_VENDOR_CALLS rides along even though
          ;; nothing this process can reach is a vendor.
          (let ((process-environment
                 (append (list (concat "AGENT_REPL_STATE_DIR=" dir)
                               "AGENT_REPL_FORBID_VENDOR_CALLS=1")
                         process-environment)))
            (make-process
             :name "agent-repl-itest-fakedaemon"
             :command (list binary)
             :connection-type 'pipe
             :coding 'utf-8
             :noquery t
             :stderr stderr))))
    (agent-repl-itest--wait-until
     (lambda () (or (agent-repl-itest--read-addr-file dir)
                    (not (process-live-p process))))
     agent-repl-itest-default-timeout
     (format "the fake daemon to publish %s" (agent-repl-itest--addr-file dir)))
    (unless (process-live-p process)
      (let ((text (with-current-buffer stderr (buffer-string))))
        (error "agent-repl-itest: the fake daemon exited before publishing daemon.addr:\n%s"
               text)))
    (agent-repl-itest--make-daemon
     :process process
     :state-dir dir
     :address (agent-repl-itest--read-addr-file dir)
     :stderr-buffer stderr)))

(defun agent-repl-itest--stop-daemon (daemon &optional keep-state-dir)
  "Stop DAEMON, orderly if it still answers, and reap its buffers.
Unless KEEP-STATE-DIR, deletes its private state dir."
  (let ((process (agent-repl-itest-daemon-process daemon)))
    (when (process-live-p process)
      ;; Ask for the orderly exit first, so daemon.addr removal is exercised
      ;; on the same path production takes.
      (ignore-errors (agent-repl-itest--exit daemon))
      (agent-repl-itest--wait-until
       (lambda () (not (process-live-p process)))
       5 "the fake daemon to exit")
      (when (process-live-p process)
        (delete-process process)))
    (let ((stderr (agent-repl-itest-daemon-stderr-buffer daemon)))
      (when (buffer-live-p stderr) (kill-buffer stderr))))
  (unless keep-state-dir
    (ignore-errors (delete-directory (agent-repl-itest-daemon-state-dir daemon) t))))

;;;; ---- The control plane ----

(defun agent-repl-itest--curl-program ()
  "Return `curl', which the control plane rides.
This is the TEST's own use of curl, not production's: production reaches
curl only through `agent-repl-connect--spawn-curl'."
  (or (executable-find "curl")
      (error "agent-repl-itest: no `curl' on PATH — the control plane needs it")))

(defun agent-repl-itest--control (daemon path &optional body)
  "Call DAEMON's control-plane PATH and return (STATUS . PARSED).
With BODY (a string of JSON) the call is a POST, otherwise a GET.  STATUS
is the HTTP status as an integer and PARSED the decoded JSON body (an
alist with symbol keys, `list' arrays), or the raw string when the body
is not JSON.  A non-2xx status is returned rather than signalled: several
scenarios assert on a deliberate 400."
  (let* ((curl (agent-repl-itest--curl-program))
         (url (format "http://%s%s" (agent-repl-itest-daemon-address daemon) path))
         (args (append (list "-sS" "--http1.1" "-w" "\n%{http_code}")
                       (when body (list "-X" "POST" "--data-binary" body))
                       (list url))))
    (with-temp-buffer
      (let ((status (apply #'call-process curl nil t nil args)))
        (unless (eq status 0)
          (error "agent-repl-itest: curl failed (exit %s) for %s: %s"
                 status path (buffer-string))))
      (goto-char (point-max))
      (forward-line -1)
      (let* ((http-status (string-to-number (string-trim
                                             (buffer-substring (point) (point-max)))))
             (payload (string-trim (buffer-substring (point-min) (point)))))
        (cons http-status
              (if (string-empty-p payload)
                  nil
                (condition-case nil
                    (json-parse-string payload :object-type 'alist :array-type 'list
                                       :null-object :null :false-object :false)
                  (error payload))))))))

(defun agent-repl-itest--control-ok (daemon path &optional body)
  "Call DAEMON's control-plane PATH and return the parsed body, or signal.
Refuses a non-200: a control call the fake rejected means the TEST is
malformed, and letting it pass silently would make the scenario assert
against a fake that never received its instruction."
  (let ((result (agent-repl-itest--control daemon path body)))
    (unless (eq (car result) 200)
      (error "agent-repl-itest: %s answered %s: %S" path (car result) (cdr result)))
    (cdr result)))

(defun agent-repl-itest--script (daemon method response)
  "Script DAEMON's answer to unary METHOD as RESPONSE.
RESPONSE is an alist (or nil for `{}') serialized as protojson and
validated by the fake against the GENERATED response type, so a
misspelled arm fails here rather than as a puzzling wire error later."
  (agent-repl-itest--control-ok
   daemon "/_fake/script"
   (json-serialize `((method . ,method) (response . ,(or response '()))))))

(defun agent-repl-itest--push (daemon stream message &optional workspace-id snapshot)
  "Push MESSAGE on DAEMON's STREAM (\"host\", \"daemon\" or \"roster\").
WORKSPACE-ID is required for the host stream and refused for the other
two.  With SNAPSHOT non-nil the message is also stored and replayed to
later subscribers, which is how a standing state is staged before Emacs
subscribes."
  (agent-repl-itest--control-ok
   daemon "/_fake/push"
   (json-serialize (append `((stream . ,stream) (message . ,message))
                           (when workspace-id `((workspace_id . ,workspace-id)))
                           (when snapshot '((snapshot . t)))))))

(defun agent-repl-itest--end (daemon stream &optional workspace-id error abort)
  "End DAEMON's STREAM subscribers.
ERROR is (CODE . MESSAGE) for an end frame carrying a Connect error.
ABORT drops the TCP connection with NO end frame at all — the
producer-side end that is a transport failure rather than a close."
  (agent-repl-itest--control-ok
   daemon "/_fake/end"
   (json-serialize (append `((stream . ,stream))
                           (when workspace-id `((workspace_id . ,workspace-id)))
                           (when error `((error . ((code . ,(car error))
                                                   (message . ,(cdr error))))))
                           (when abort '((abort . t)))))))

(defun agent-repl-itest--calls (daemon &optional method)
  "Return DAEMON's recorded requests in order, optionally only METHOD's.
Each entry is an alist with `method' and `body' (the request's protojson,
already parsed) — so an assertion reads the wire as the fake saw it."
  (let ((calls (agent-repl-itest--control-ok daemon "/_fake/calls")))
    (if method
        (seq-filter (lambda (call) (equal (alist-get 'method call) method)) calls)
      calls)))

(defun agent-repl-itest--call-bodies (daemon method)
  "Return the recorded request bodies for METHOD on DAEMON, in order."
  (mapcar (lambda (call) (alist-get 'body call))
          (agent-repl-itest--calls daemon method)))

(defun agent-repl-itest--subscribers (daemon &optional stream workspace-id)
  "Return DAEMON's open stream subscribers, optionally filtered.
STREAM filters by stream name and WORKSPACE-ID by workspace."
  (let ((subs (agent-repl-itest--control-ok daemon "/_fake/subscribers")))
    (seq-filter
     (lambda (sub)
       (and (or (null stream) (equal (alist-get 'stream sub) stream))
            (or (null workspace-id) (equal (alist-get 'workspace_id sub) workspace-id))))
     subs)))

(defun agent-repl-itest--exit (daemon)
  "Ask DAEMON to exit orderly (removing its `daemon.addr')."
  (agent-repl-itest--control-ok daemon "/_fake/exit" "{}"))

(defun agent-repl-itest--await-subscriber (daemon stream &optional workspace-id count)
  "Block until DAEMON has COUNT (default 1) subscribers of STREAM.
A push before the subscription is registered is delivered to nobody, so
every scenario that pushes after subscribing must pass through here."
  (let ((wanted (or count 1)))
    (agent-repl-itest--wait-until
     (lambda () (>= (length (agent-repl-itest--subscribers daemon stream workspace-id)) wanted))
     agent-repl-itest-default-timeout
     (format "%s subscriber(s) on the %s stream%s" wanted stream
             (if workspace-id (format " for %s" workspace-id) "")))))

(defun agent-repl-itest--await-call (daemon method &optional count)
  "Block until DAEMON has recorded COUNT (default 1) calls of METHOD."
  (let ((wanted (or count 1)))
    (agent-repl-itest--wait-until
     (lambda () (>= (length (agent-repl-itest--calls daemon method)) wanted))
     agent-repl-itest-default-timeout
     (format "%s recorded %s call(s)" wanted method))))

;;;; ---- Production logs ----
;;
;; The integration runs are read through the production code's own structured
;; logs (the teamlead's remediation loop leans on this), so the harness points
;; the JSONL sink into the private state dir and hands the suite a reader.

(defun agent-repl-itest--log-file (daemon)
  "Return the production JSONL log path for DAEMON's state dir."
  (expand-file-name "emacs.jsonl" (agent-repl-itest-daemon-state-dir daemon)))

(defun agent-repl-itest--log-records (daemon)
  "Return the production log records written during DAEMON's run.
Each record is the parsed JSONL object; a malformed line is skipped
rather than aborting the read, because a truncated final line is a normal
consequence of reading a sink that is still being appended to."
  (let ((path (agent-repl-itest--log-file daemon))
        (records nil))
    (when (file-exists-p path)
      (with-temp-buffer
        (insert-file-contents path)
        (goto-char (point-min))
        (while (not (eobp))
          (let ((line (string-trim (buffer-substring (line-beginning-position)
                                                     (line-end-position)))))
            (unless (string-empty-p line)
              (condition-case nil
                  (push (json-parse-string line :object-type 'alist :array-type 'list
                                           :null-object :null :false-object :false)
                        records)
                (error nil))))
          (forward-line 1))))
    (nreverse records)))

(defun agent-repl-itest--operation-slug (name)
  "Return the slug core.el derives from the log NAME's format string.
`agent-repl--log-operation' normalizes the FORMAT STRING into
`agent-repl.<slug>', so a logical operation name like
\"elisp.rpc.push-invalid\" becomes a PREFIX of the recorded slug (the
format's trailing `key=%S' fragments extend it)."
  (replace-regexp-in-string
   "\\`-+\\|-+\\'" ""
   (replace-regexp-in-string "[^[:alnum:]]+" "-" (downcase name))))

(defun agent-repl-itest--operation-prefix (name)
  "Return the `operation' prefix core.el derives for the log NAME.
Kept for callers that want the plain `agent-repl.<slug>' form; the
matcher below is what tolerates core.el's SEVERITY prefix."
  (concat "agent-repl." (agent-repl-itest--operation-slug name)))

(defconst agent-repl-itest--operation-severity-slugs '("" "warning-" "error-")
  "Slug fragments core.el's severity tags contribute to `operation'.
`agent-repl--warn' and `agent-repl--error' prepend \"WARNING: \" and
\"ERROR: \" to the FORMAT STRING itself, and `agent-repl--log-operation'
normalizes that whole string — so the recorded operation for a warned
`elisp.daemon.stale-addr' is `agent-repl.warning-elisp-daemon-stale-addr...'.
The LEVEL field already carries the severity, so a reader asking for a
logical operation name must accept it with or without the tag rather than
making every caller spell the tag it cannot see from the call site.")

(defun agent-repl-itest--operation-matches-p (operation name)
  "Return non-nil when the recorded OPERATION names the logical NAME.
Matches `agent-repl.<slug>...' with or without a severity tag between the
namespace and the slug (see
`agent-repl-itest--operation-severity-slugs')."
  (let ((slug (agent-repl-itest--operation-slug name)))
    (seq-some (lambda (severity)
                (string-prefix-p (concat "agent-repl." severity slug) operation))
              agent-repl-itest--operation-severity-slugs)))

(defun agent-repl-itest--log-entries (daemon operation &optional level)
  "Return DAEMON's log records for OPERATION, optionally at LEVEL.
OPERATION is the logical `elisp.<module>.<operation>' name the production
code logs; LEVEL is core.el's level string (\"debug\", \"info\", \"warn\"
or \"error\")."
  (seq-filter
   (lambda (record)
     (and (agent-repl-itest--operation-matches-p
           (or (alist-get 'operation record) "") operation)
          (or (null level) (equal (alist-get 'level record) level))))
   (agent-repl-itest--log-records daemon)))

(defun agent-repl-itest--logged-p (daemon operation &optional level)
  "Return non-nil when DAEMON's run logged OPERATION (at LEVEL)."
  (consp (agent-repl-itest--log-entries daemon operation level)))

(defun agent-repl-itest--await-log (daemon operation &optional level)
  "Block until DAEMON's run has logged OPERATION (at LEVEL).
Production logging is a side effect of asynchronous callbacks, so a
suite asserting on a log line waits for it rather than racing it."
  (agent-repl-itest--wait-until
   (lambda () (agent-repl-itest--logged-p daemon operation level))
   agent-repl-itest-default-timeout
   (format "the production log to carry %s%s" operation
           (if level (format " at %s" level) ""))))

;;;; ---- Boundary mocks the suite installs ----

(defun agent-repl-itest--real-spawn-curl ()
  "Return the REAL `agent-repl-connect--spawn-curl' captured before guarding.
The batch harness replaces every entry of
`agent-repl--external-boundary-functions' with a guard that errors, which
is exactly right for every other boundary — but the transport's ONE spawn
point is the boundary this suite exists to exercise, against a fake daemon
on loopback.  Every OTHER guard stays armed."
  (let ((cell (assq 'agent-repl-connect--spawn-curl
                    agent-repl-test--external-original-functions)))
    (unless cell
      (error (concat "agent-repl-itest: `agent-repl-connect--spawn-curl' is not in "
                     "`agent-repl--external-boundary-functions' — the integration suite "
                     "cannot reach the fake daemon without the real spawn point")))
    (or (cdr cell)
        (error "agent-repl-itest: no original captured for `agent-repl-connect--spawn-curl'"))))

(defvar agent-repl-itest-notifications nil
  "Desktop notifications the fake notifier backend recorded, newest first.
Each entry is the plist (:workspace WS :title TITLE :message MESSAGE).")

(defun agent-repl-itest--fake-notifier ()
  "Return a notifier backend that records instead of notifying.
Emacs's notification POLICY is the whole reaction to a `notification'
push, so the suite must observe the call rather than an OS side effect."
  (lambda (ws title message)
    (push (list :workspace ws :title title :message message)
          agent-repl-itest-notifications)
    t))

(defvar agent-repl-itest-webview-urls nil
  "URLs the fake webview factory was asked to mount, newest first.")

(defmacro agent-repl-itest--with-fake-daemon (var &rest body)
  "Run BODY with VAR bound to a freshly started fake daemon.
Inside BODY:

- `AGENT_REPL_STATE_DIR' names VAR's PRIVATE state dir, so production's
  own `daemon.addr' discovery finds the fake and nothing touches the
  developer's real state tree;
- `AGENT_REPL_FORBID_VENDOR_CALLS' is set, in this process too;
- the production JSONL log sink is ON and writes into that private dir,
  which is what `agent-repl-itest--log-entries' reads;
- the ONE external boundary the transport spawns through is restored to
  its real implementation, and every other guard stays armed;
- the notifier backend and the webview factory record into
  `agent-repl-itest-notifications' and `agent-repl-itest-webview-urls'.

The daemon is always stopped and its state dir removed, even when BODY
signals."
  (declare (indent 1) (debug (symbolp body)))
  `(let ((,var (agent-repl-itest--start-daemon)))
     (unwind-protect
         (let* ((agent-repl-itest-notifications nil)
                (agent-repl-itest-webview-urls nil)
                (process-environment
                 (append (list (concat "AGENT_REPL_STATE_DIR="
                                       (agent-repl-itest-daemon-state-dir ,var))
                               "AGENT_REPL_FORBID_VENDOR_CALLS=1")
                         process-environment))
                (agent-repl-log-to-file t)
                (agent-repl-log-file-name (agent-repl-itest--log-file ,var))
                (agent-repl--log-write-counter 0))
           (cl-letf (((symbol-function 'agent-repl-connect--spawn-curl)
                      (agent-repl-itest--real-spawn-curl))
                     ((symbol-function 'agent-repl--frontend-make-webview-buffer)
                      (agent-repl-test--fake-webview-factory 'agent-repl-itest-webview-urls))
                     (agent-repl--notification-backend (agent-repl-itest--fake-notifier)))
             ,@body))
       (agent-repl-itest--stop-daemon ,var))))

(defmacro agent-repl-itest--with-second-daemon (primary var &rest body)
  "Run BODY with VAR bound to a SECOND fake daemon sharing PRIMARY's state dir.
This stages a handover: the successor binds a fresh port and rewrites
`daemon.addr' in the one state root, exactly as a joining blue-green
daemon does, so Emacs's reconnect and adopt paths see two real servers.
Ordering note: the successor is started INSIDE this macro, so PRIMARY's
address is already published and recorded before it is replaced."
  (declare (indent 2) (debug (form symbolp body)))
  `(let ((,var (agent-repl-itest--start-daemon
                (agent-repl-itest-daemon-state-dir ,primary))))
     (unwind-protect
         (progn ,@body)
       ;; The state dir belongs to PRIMARY's `with-fake-daemon'; only the
       ;; successor process is reaped here.
       (agent-repl-itest--stop-daemon ,var t))))

(defalias 'agent-repl-itest--start-second-daemon #'agent-repl-itest--start-daemon
  "Start a second fake daemon; pass the primary's state dir to stage a handover.")

;;;; ---- Small assertion helpers ----

(defun agent-repl-itest--body-field (body &rest path)
  "Return the value at PATH inside a recorded request BODY.
PATH is a list of protojson keys as SYMBOLS, spelled lowerCamel exactly
as the wire spells them — which is itself part of what the suite pins."
  (let ((value body))
    (dolist (key path value)
      (setq value (and (consp value) (alist-get key value))))))

(provide 'test-integration-helpers)

;;; test-integration-helpers.el ends here
