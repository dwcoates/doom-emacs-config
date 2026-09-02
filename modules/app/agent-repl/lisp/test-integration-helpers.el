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
this process can reach is a vendor, and the variable says so anyway.

A HANDOVER STAGES TWO DAEMONS IN ONE STATE ROOT, so the wait is for the
address to CHANGE, not merely to exist: an already-published address is
the INCUMBENT'S, and returning it would hand the caller a struct whose
`address' names the wrong process — every rpc aimed at the successor
(including the orderly exit) would land on the daemon it is replacing."
  (let* ((binary (agent-repl-itest--ensure-binary))
         (dir (or state-dir (agent-repl-itest--private-state-dir)))
         (incumbent (agent-repl-itest--read-addr-file dir))
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
     (lambda ()
       (let ((published (agent-repl-itest--read-addr-file dir)))
         (or (and published (not (equal published incumbent)))
             (not (process-live-p process)))))
     agent-repl-itest-default-timeout
     (format "the fake daemon to publish %s%s" (agent-repl-itest--addr-file dir)
             (if incumbent (format " over the incumbent %s" incumbent) "")))
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

(defun agent-repl-itest--gate (daemon method)
  "Make DAEMON withhold METHOD\='s ANSWER until released.
The request is still recorded and validated; only the response waits.
This is how a scenario pins an ORDER — the handover\='s \"call
AdoptHostWorkspace ... THEN cancel the old stream and re-subscribe\" is
only observable while the adopt is in flight."
  (agent-repl-itest--control-ok
   daemon "/_fake/gate" (json-serialize `((method . ,method)))))

(defun agent-repl-itest--release-gate (daemon method)
  "Let every held call of METHOD on DAEMON answer, and disarm the gate.
Releasing a gate nobody armed is a control-plane 400, so a scenario that
lost track of what it held fails loudly."
  (agent-repl-itest--control-ok
   daemon "/_fake/gate" (json-serialize `((method . ,method) (release . t)))))

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

(defun agent-repl-itest--call-raw-bodies (daemon method)
  "Return the RAW request bodies METHOD sent to DAEMON, in order.
Each is the request string EXACTLY as the transport wrote it.  The
parsed `body\=' is a re-marshal of the decoded message and so drops every
zero-valued scalar — so an explicitly encoded `false\=' (which the
contract requires for `force\=', `self_certified\=' and
`add_to_merge_queue\=') can ONLY be asserted here."
  (mapcar (lambda (call) (alist-get 'raw call))
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
  "Return the production GLOBAL JSONL log path for DAEMON's state dir."
  (expand-file-name "emacs.jsonl" (agent-repl-itest-daemon-state-dir daemon)))

(defvar agent-repl--workspaces)
(defvar agent-repl--workspace-log-targets)
;; Declared here so `agent-repl-itest--with-fake-daemon''s per-scenario
;; scratch bindings are DYNAMIC ones over the production tables rather than
;; lexical shadows this file alone would see.
(defvar agent-repl-host--by-name)
(defvar agent-repl--prompt-queue)
(defvar agent-repl--prompt-queue-draining)

(defvar agent-repl-itest--orphaned-log-targets nil
  "Durable workspace log targets production has stopped owning this scenario.

A WORKSPACE TEARDOWN ORPHANS THE HISTORY THE CANONICAL LINK USED TO NAME.
`agent-repl--ws-del\=' forgets the workspace\='s target
(`agent-repl--ws-forget-emacs-log-target\='), and the very next
workspace-owned record mints a fresh target and re-points
`<workspace>/.claude/emacs/emacs.log\=' at it -- deliberately, so a future
workspace reusing the name gets its own runtime-owned file.  The records
written BEFORE the teardown are still durable, but the canonical link no
longer names them, so a reader that followed only the link would conclude
a verb never logged its success ack.

The fixture therefore remembers each target at the ONE moment it is
orphaned, and `agent-repl-itest--workspace-log-files\=' reads those too.")

(defun agent-repl-itest--note-orphaned-log-targets (ws)
  "Remember the durable log targets WS owns, before production forgets them.
Called from the fixture\='s wrapper around
`agent-repl--ws-forget-emacs-log-target\='; the registry is swept the same
way that function sweeps it, by WS\='s registered directory, because a WS
mid-teardown may no longer resolve to a log identity."
  (let* ((dir (ignore-errors (agent-repl--ws-get ws :project-dir)))
         (canonical (and (stringp dir) (agent-repl--path-canonical dir))))
    (when (and canonical (boundp 'agent-repl--workspace-log-targets))
      (maphash (lambda (_key entry)
                 (let ((owned (plist-get entry :project-dir))
                       (target (plist-get entry :target)))
                   (when (and (stringp owned) (stringp target)
                              (equal canonical (agent-repl--path-canonical owned))
                              (not (member target agent-repl-itest--orphaned-log-targets)))
                     (push target agent-repl-itest--orphaned-log-targets))))
               agent-repl--workspace-log-targets))))

(defun agent-repl-itest--workspace-log-files ()
  "Return the workspace `emacs.log' sinks of every registered workspace.
THE SINK FOLLOWS THE RECORD'S OWNER.  logging-contract.md routes a record
that owns a workspace into that workspace\='s own
`<workspace>/.claude/emacs/emacs.log\=' -- writing it globally would be
the routing invariant violation the contract forbids -- so a reader that
looked only at the global sink could not see a workspace-owned line at
all.  The canonical path is a SYMLINK to the runtime-owned target;
`insert-file-contents\=' follows it, which is the whole of what reading it
takes."
  (let (paths)
    (when (boundp 'agent-repl--workspaces)
      (maphash
       (lambda (_ws plist)
         (let ((dir (plist-get plist :project-dir)))
           (when (stringp dir)
             (let ((path (expand-file-name ".claude/emacs/emacs.log" dir)))
               (when (and (file-exists-p path) (not (member path paths)))
                 (push path paths))))))
       agent-repl--workspaces))
    ;; The targets a teardown orphaned come FIRST, because they carry the
    ;; older records and `agent-repl-itest--log-records\=' returns its
    ;; sinks in the order it is given them.
    (append (seq-filter #'file-exists-p
                        (reverse agent-repl-itest--orphaned-log-targets))
            (nreverse paths))))

(defun agent-repl-itest--log-records-in (path)
  "Return the JSONL records in PATH, oldest first.
A malformed line is skipped rather than aborting the read, because a
truncated final line is a normal consequence of reading a sink that is
still being appended to."
  (let ((records nil))
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

(defun agent-repl-itest--log-records (daemon)
  "Return the production log records written during DAEMON's run.
BOTH SINKS ARE READ: the global one for records that genuinely have no
workspace, and every registered workspace\='s own `emacs.log\=' for the
records that own one.  A scenario asserting on a workspace-owned line
must find it where the contract puts it."
  (apply #'append
         (agent-repl-itest--log-records-in (agent-repl-itest--log-file daemon))
         (mapcar #'agent-repl-itest--log-records-in
                 (agent-repl-itest--workspace-log-files))))

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
  "Return the `operation' prefix core.el derives for the log NAME."
  (concat "agent-repl." (agent-repl-itest--operation-slug name)))

(defun agent-repl-itest--operation-matches-p (operation name)
  "Return non-nil when the recorded OPERATION names the logical NAME.
core.el derives `operation' from the BARE format string on every rung —
the severity rungs\' display tags never reach it, and `level' carries the
severity instead — so a plain prefix match is the whole rule."
  (string-prefix-p (agent-repl-itest--operation-prefix name) operation))

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

;;;; ---- Link teardown ----

(defun agent-repl-itest--teardown-link ()
  "Close any standing daemon link and cancel its reconnect timer.
`agent-repl-link-teardown\=' closes the connections and forgets the link
state; the reconnect timer is armed separately by the close handler, so
it is cancelled here too -- a timer that survives the scenario reconnects
into the NEXT one\='s daemon."
  (ignore-errors (agent-repl-link-teardown))
  (ignore-errors (agent-repl-link--cancel-reconnect)))

;;;; ---- Boundary mocks the suite installs ----

(defun agent-repl-itest--real-boundary (symbol)
  "Return the REAL implementation of boundary SYMBOL, captured before guarding.
The batch harness replaces every entry of
`agent-repl--external-boundary-functions' with a guard that errors, which
is exactly right for every boundary a scenario only has to STUB — but a
handful of boundaries are the very thing an integration scenario exists to
exercise against real, harmless, test-owned targets (the fake daemon on
loopback; stub shell scripts in the test's own temp dir).  Restoring one
is deliberate and NARROW: it happens per scenario, by name, and every
other guard stays armed.

Signals when SYMBOL is not registered — a boundary that left the registry
must not be reachable by accident."
  (let ((cell (assq symbol agent-repl-test--external-original-functions)))
    (unless cell
      (error (concat "agent-repl-itest: `%s' is not in "
                     "`agent-repl--external-boundary-functions' — the integration "
                     "suite cannot restore a boundary it does not know about")
             symbol))
    (or (cdr cell)
        (error "agent-repl-itest: no original captured for `%s'" symbol))))

(defun agent-repl-itest--real-spawn-curl ()
  "Return the REAL `agent-repl-connect--spawn-curl' captured before guarding.
The transport's ONE spawn point is the boundary the integration suite
exists to exercise, against a fake daemon on loopback."
  (agent-repl-itest--real-boundary 'agent-repl-connect--spawn-curl))

(defvar agent-repl-itest-notifications nil
  "Desktop notifications the fake notifier backend recorded, newest first.
Each entry is the list (WS TITLE MESSAGE ACTIVATE) — the shape
`agent-repl-notify-make-fake-backend' records, because that is the seam
this harness installs.")

(defun agent-repl-itest--fake-notifier ()
  "Return a notifier backend that records instead of notifying.
Emacs's notification POLICY is the whole reaction to a `notification'
push, so the suite must observe the call rather than an OS side effect.

THE SEAM IS PRODUCTION'S OWN.  `agent-repl--notify' calls its backend
with FOUR arguments (the per-notification click action is the fourth), so
a hand-rolled three-argument recorder here signals inside the delayed
notification timer and records nothing at all.  Using
`agent-repl-notify-make-fake-backend' keeps the arity and the recorded
shape defined in ONE place, beside the caller that fixes them."
  (agent-repl-notify-make-fake-backend 'agent-repl-itest-notifications))

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
                (agent-repl-itest--orphaned-log-targets nil)
                ;; SCRATCH REGISTRIES, one set per scenario.  Each of these is
                ;; a process-global production table, and a scenario that
                ;; leaves an entry in one does not merely litter it: on the
                ;; NEXT scenario's link-up edge production WALKS them.  A
                ;; leftover prompt-queue entry whose workspace still carried a
                ;; dead connection made `agent-repl--prompt-queue-on-link-up'
                ;; signal `agent-repl-connect-error' straight out of
                ;; `run-hook-with-args', so the up hooks behind it --
                ;; roster.el's and host.el's -- never ran, and the next
                ;; scenario waited out its whole deadline for a register
                ;; nothing was ever going to issue.  Binding the tables here
                ;; means a scenario cannot leak one, however it exits.
                (agent-repl--workspaces (make-hash-table :test 'equal))
                (agent-repl-host--by-name (make-hash-table :test 'equal))
                (agent-repl--prompt-queue (make-hash-table :test 'equal))
                (agent-repl--prompt-queue-draining (make-hash-table :test 'equal))
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
                     ((symbol-function 'agent-repl--ws-forget-emacs-log-target)
                      (let ((real (symbol-function
                                   'agent-repl--ws-forget-emacs-log-target)))
                        (lambda (ws reason)
                          (agent-repl-itest--note-orphaned-log-targets ws)
                          (funcall real ws reason))))
                     ((symbol-function 'agent-repl--frontend-make-webview-buffer)
                      (agent-repl-test--fake-webview-factory 'agent-repl-itest-webview-urls))
                     (agent-repl--notification-backend (agent-repl-itest--fake-notifier)))
             ,@body))
       ;; A verb may STAND THE LINK on its own (a daemon-admin verb can be
       ;; the first thing that needs one), and a link outliving its daemon
       ;; leaves an armed reconnect timer that fires into the next
       ;; scenario.  Tearing it down is the only thing that actually ends
       ;; it -- the same reason the cold-start reset exists.
       (agent-repl-itest--teardown-link)
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

;;;; ---- Cold start ----
;;
;; Scenario 14 (§14) drives daemon.el's cold start with STUB SHELL SCRIPTS in
;; the test's own temp dir: "build script (stub) invoked → daemon command (stub
;; that starts the fake) → link up".  That contract is exactly "run this
;; script, then wait for daemon.addr", so an elisp stub over the boundary would
;; assert nothing — and the state-root export can only be observed by a child
;; that really ran.  These three boundaries are therefore RESTORED for a
;; cold-start scenario, per-scenario and by name, the same way the transport's
;; spawn point is; every other guard stays armed.

(defconst agent-repl-itest--cold-start-boundaries
  '(agent-repl--frontend-run-build-script
    agent-repl--frontend-spawn-daemon
    agent-repl--frontend-artifact-exists-p)
  "The external boundaries a cold-start scenario exercises for real.
Their targets are all test-owned: a stub script and a stub argv written
into the scenario's own temp dir, and `file-exists-p' on those paths.")

(defvar agent-repl-daemon--ensure-in-flight)
(defvar agent-repl-daemon--boot-timer)
(defvar agent-repl-daemon--boot-deadline)
(defvar agent-repl-daemon--boot-continuation)
(defvar agent-repl-daemon-build-failure)
(defvar agent-repl-daemon-mode-line-segment)
(defvar agent-repl--frontend-daemon-process)
(declare-function agent-repl-daemon--cancel-boot-wait "daemon")
(declare-function agent-repl-link-teardown "daemon-link")
(declare-function agent-repl-link--cancel-reconnect "daemon-link")

(defun agent-repl-itest--reset-cold-start ()
  "Take down everything a cold-start scenario can leave running.
A scenario that asserts as soon as its stub script ran leaves a POLL
TIMER armed and, on the paths that got that far, a spawned daemon and a
standing link.  All three outlive the `let' that reset daemon.el's state,
so the next scenario's ensure would see an in-flight boot or a live link
and no-op — which is how one broken scenario silently disables the rest of
the suite.  Cancelling and reaping is the only thing that actually ends
them."
  (agent-repl-daemon--cancel-boot-wait)
  (when (process-live-p agent-repl--frontend-daemon-process)
    (delete-process agent-repl--frontend-daemon-process))
  (agent-repl-link-teardown)
  (dolist (name '("*agent-repl-health*" "*agent-repl-build-frontend*"))
    (when (get-buffer name) (kill-buffer name))))

(defmacro agent-repl-itest--with-cold-start (&rest body)
  "Run BODY with daemon.el's cold start reachable and its state isolated.
Inside BODY the three `agent-repl-itest--cold-start-boundaries' are the
real implementations, and every piece of daemon.el's cold-start state
starts fresh.  On the way out the boot poll is cancelled, any spawned
daemon is reaped and the link is torn down, so nothing leaks into the next
scenario."
  (declare (indent 0) (debug body))
  `(let ((agent-repl-daemon--ensure-in-flight nil)
         (agent-repl-daemon--boot-timer nil)
         (agent-repl-daemon--boot-deadline nil)
         (agent-repl-daemon--boot-continuation nil)
         (agent-repl-daemon-build-failure nil)
         (agent-repl-daemon-mode-line-segment nil)
         (agent-repl--frontend-daemon-process nil))
     (cl-letf ,(mapcar (lambda (boundary)
                         `((symbol-function ',boundary)
                           (agent-repl-itest--real-boundary ',boundary)))
                       agent-repl-itest--cold-start-boundaries)
       (unwind-protect (progn ,@body)
         (agent-repl-itest--reset-cold-start)))))

;;;; ---- Small assertion helpers ----

(defun agent-repl-itest--body-field (body &rest path)
  "Return the value at PATH inside a recorded request BODY.
PATH is a list of protojson keys as SYMBOLS, spelled lowerCamel exactly
as the wire spells them — which is itself part of what the suite pins."
  (let ((value body))
    (dolist (key path value)
      (setq value (and (consp value) (alist-get key value))))))

;;;; ---- Audit-2 additions (R-SUITE-2) ----
;;
;; Helpers findings 43-46 of docs/overhaul/reports/elisp-suite-audit-2.md
;; need.  Kept in their own section so they merge cleanly beside concurrent
;; edits to the sections above.

(defun agent-repl-itest--call-headers (daemon method &optional index)
  "Return the request HEADERS METHOD's INDEXth call carried to DAEMON.
INDEX defaults to 0.  The Connect protocol fixes headers as well as
bodies (fanout §3: `Content-Type' plus `Connect-Protocol-Version: 1' on a
unary call, `application/connect+json' on a stream), and neither the
parsed `body\=' nor the verbatim `raw\=' can see them — so the fake\='s mux
records them beside both, exactly the way it records `raw\=', and this
reads them.  Header names are CANONICAL (`Content-Type\='), which is how
Go\='s `http.Header\=' keys them.

Answers nil when the call was recorded by a fake too old to carry
headers, which a caller must not paper over."
  (let ((call (nth (or index 0) (agent-repl-itest--calls daemon method))))
    (alist-get 'headers call)))

(defun agent-repl-itest--call-header (daemon method name &optional index)
  "Return the value of request header NAME on METHOD's INDEXth call to DAEMON.
NAME is the canonical spelling, e.g. \"Content-Type\"."
  (alist-get (intern name) (agent-repl-itest--call-headers daemon method index)))

(provide 'test-integration-helpers)

;;; test-integration-helpers.el ends here
