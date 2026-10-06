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
(defvar agent-repl--timers)
(defvar agent-repl-status--marker-on)

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

(defconst agent-repl-itest-prebuilt-env "AGENT_REPL_ITEST_FAKEDAEMON"
  "Environment variable naming an already-built fake daemon.
The test runner (testrun) builds the fake ONCE and hands its path to every
ERT chunk through this variable, so N chunks do not pay N builds.  When it
is set the binary must exist: a chunk told to use a binary that was never
built is a broken run, never a reason to build one quietly.")

(defun agent-repl-itest--ensure-binary ()
  "Return the fake daemon's path, building it once per Emacs process.
A path in `agent-repl-itest-prebuilt-env' is used as-is and never built.
Otherwise builds OFFLINE (`GOPROXY=off') against the module cache, so a
run never depends on the network."
  (or (and agent-repl-itest--binary (file-executable-p agent-repl-itest--binary)
           agent-repl-itest--binary)
      (agent-repl-itest--prebuilt-binary (getenv agent-repl-itest-prebuilt-env))
      (let* ((go (agent-repl-itest--go-program))
             (output (expand-file-name (format "agent-repl-fakedaemon-%d" (emacs-pid))
                                       temporary-file-directory))
             (default-directory agent-repl-itest--fakedaemon-source-dir)
             (process-environment (append '("GOFLAGS=-mod=mod" "GOPROXY=off")
                                          process-environment))
             (log (generate-new-buffer "*agent-repl-itest-go-build*"))
             ;; -buildvcs=false: a test build never asks git to stamp the
             ;; binary (no test runs real git, owner rule).
             (status (call-process go nil log nil "build" "-buildvcs=false" "-o" output ".")))
        (unless (eq status 0)
          (let ((text (with-current-buffer log (buffer-string))))
            (kill-buffer log)
            (error "agent-repl-itest: building the fake daemon failed (exit %s):\n%s"
                   status text)))
        (kill-buffer log)
        (setq agent-repl-itest--binary output))))

(defun agent-repl-itest--prebuilt-binary (path)
  "Adopt the prebuilt fake daemon at PATH, or return nil when PATH is unset.
Signals when PATH is set but names no executable."
  (when (and path (not (string-empty-p path)))
    (unless (file-executable-p path)
      (error "agent-repl-itest: %s names %s, which is not an executable fake daemon"
             agent-repl-itest-prebuilt-env path))
    (setq agent-repl-itest--binary path)))

;;;; ---- The running fake ----

(cl-defstruct (agent-repl-itest-daemon (:constructor agent-repl-itest--make-daemon))
  "One running fake daemon instance.
PROCESS is its Emacs process object, STATE-DIR the private
`AGENT_REPL_STATE_DIR' it was started with, ADDRESS the `127.0.0.1:PORT'
it wrote to that dir's `daemon.addr', and STDERR-BUFFER its structured
JSON log."
  process state-dir address stderr-buffer)

(defconst agent-repl-itest-default-timeout 14
  "Seconds `agent-repl-itest--wait-until' waits before failing.
Sized at ~3x the slowest healthy wait observed across a full run of every
`test-integration-*.el' suite (the link suite's own handover/reconnect
scenarios top out around 4.7s) -- see the bounds table in
`modules/app/agent-repl/AGENTS.md's test section.  Do not raise this back
toward its old, unmeasured 15s without re-measuring: a bound this close to
the slowest suite's own \"needs longer\" cases (composer's ~2.4s outage
drain, host's ~2.7s not-yet-adopted retry) is deliberate, not slack.")

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
        ;; pending sentinel, and RETURNS THE MOMENT ANY ARRIVES -- so for a
        ;; predicate over Emacs's own state this loop is already woken by the
        ;; event, and the interval is only the floor under a predicate whose
        ;; fact lives on the DAEMON (a subscriber count, a recorded call) and
        ;; so cannot announce itself.
        ;;
        ;; 2ms, not the 20ms it was: at 20ms nearly every wait in the suite
        ;; slept a whole slot, which was the single largest cost in the
        ;; integration run (454 waits, 10.0s of an 11.5s host run).  A
        ;; daemon-state predicate now costs one loopback round trip (~0.3ms
        ;; since the control plane stopped spawning curl), so 2ms samples an
        ;; order of magnitude finer at a duty cycle the predicate can carry.
        (accept-process-output nil 0.002)))))

(defconst agent-repl-itest--fixture-root
  (expand-file-name (format "agent-repl-itest-fixtures-%d" (emacs-pid))
                    temporary-file-directory)
  "Root of this Emacs process's PRIVATE fixture directories.

A FIXTURE DIRECTORY IS A WORKSPACE'S `:project-dir', AND PRODUCTION OWNS
WHAT IT WRITES THERE.  A registered workspace's records route into
`<project-dir>/.claude/emacs/emacs.log', a canonical SYMLINK
`agent-repl--workspace-emacs-log-target' re-points at its own
runtime-owned target whenever it finds the link naming someone else's --
deliberately, because \"another Emacs runtime registering the same
directory\" is exactly the case that rule exists for.

Two suite runs on one machine are two such runtimes.  With a fixed
`/tmp/itest-<suite>-ws' they share one canonical link and each re-points
it under the other, so a scenario's `--await-log' reads a sink holding
the OTHER process's records and waits out its whole deadline for a line
that was written, findably, somewhere else.  That is a roaming timeout in
whichever composer or verbs scenario happened to be running, which is
indistinguishable from a flake and is not one.

Keying the root by pid makes the sharing impossible rather than
unlikely.")

(defun agent-repl-itest--fixture-dir (name)
  "Return this process's private fixture directory called NAME.
The directory itself is NOT created: production creates a workspace's
`.claude' tree the first time it routes a record there, and a scenario
that needs the directory earlier makes it itself."
  (expand-file-name name agent-repl-itest--fixture-root))

(defun agent-repl-itest--ws-put-with-real-project-dir
    (original ws key value)
  "Call ORIGINAL for WS / KEY / VALUE after materializing fixture project dirs.
Only `:project-dir' values below the process-private fixture root are created.
Production logging requires a registered project directory to exist before it
can own a sink; integration scenarios model that precondition here rather than
letting a path-only fixture silently route as a workspace."
  (when (and (eq key :project-dir)
             (stringp value)
             (string-prefix-p (file-name-as-directory agent-repl-itest--fixture-root)
                              (file-name-as-directory value)))
    (make-directory value t))
  (funcall original ws key value))

(defun agent-repl-itest--remove-tree (dir)
  "Remove DIR and everything under it, if it exists.
An absent DIR is already the state asked for.  ANY OTHER FAILURE SIGNALS:
a cleanup that could not remove what a scenario made leaves the next one
reading it, which the run must say rather than swallow."
  (when (file-exists-p dir)
    (delete-directory dir t)))

(defun agent-repl-itest--at-exit (what fn)
  "Run FN, a cleanup of WHAT, from `kill-emacs-hook'; print a failure.
An error escaping `kill-emacs-hook' is reported by Emacs with no context
and the exit status left untouched, so a failed exit-time cleanup is
printed to stderr here, naming WHAT, in the batch temp root's form."
  (condition-case err
      (funcall fn)
    (error
     (princ (format "agent-repl-itest: ERROR: could not %s: %S\n" what err)
            #'external-debugging-output))))

(defun agent-repl-itest--delete-fixture-root ()
  "Delete this process's fixture root.  Runs on `kill-emacs-hook'.
A failure is printed to stderr rather than swallowed."
  (agent-repl-itest--at-exit
   (format "remove the fixture root %s" agent-repl-itest--fixture-root)
   (lambda () (agent-repl-itest--remove-tree agent-repl-itest--fixture-root))))

(add-hook 'kill-emacs-hook #'agent-repl-itest--delete-fixture-root)

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

(defvar agent-repl-itest--shared-state-dir nil
  "State root of the ONE fake daemon this Emacs process shares.
Created on first use and never deleted while the process lives; see
`agent-repl-itest--shared-daemon'.")

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
      ;; Kept at 5s rather than tightened to ~3x the observed teardown time
      ;; (well under 1s in every measured run): this runs after EVERY
      ;; scenario in every suite, tearing down a REAL OS process via its own
      ;; graceful-shutdown path, and a slow CI host reclaiming that process
      ;; is exactly the "genuinely needs longer" case -- a spurious failure
      ;; here just falls through to `delete-process' below anyway.
      (agent-repl-itest--wait-until
       (lambda () (not (process-live-p process)))
       5 "the fake daemon to exit")
      (when (process-live-p process)
        (delete-process process)))
    (let ((stderr (agent-repl-itest-daemon-stderr-buffer daemon)))
      (when (buffer-live-p stderr) (kill-buffer stderr))))
  ;; THE SHARED STATE ROOT OUTLIVES EVERY DAEMON THAT EVER BOUND IT.  A
  ;; scenario that stops the shared daemon (cold start's "no daemon.addr"
  ;; cases do exactly that) must not take the root with it: the next
  ;; scenario respawns into the same root, and a deleted one would leave
  ;; production's discovery pointed at a directory that no longer exists.
  (unless (or keep-state-dir
              (equal (agent-repl-itest-daemon-state-dir daemon)
                     agent-repl-itest--shared-state-dir))
    (agent-repl-itest--remove-tree (agent-repl-itest-daemon-state-dir daemon))))

;;;; ---- The control plane ----
;;
;; The control plane rides a NATIVE loopback socket rather than a `curl'
;; child.  It is called several times by every scenario -- twice by the reset
;; alone, then once per turn of every `--await-*' poll -- and a process spawn
;; per call was measurably the largest single cost in the integration run
;; (471 calls, 3.3s of a 9.4s roster run).  A `make-network-process' to
;; 127.0.0.1 costs microseconds and pulls one more external binary out of the
;; harness besides.  Production's transport now dials the same way, through
;; `agent-repl-connect--open-socket', and that is the boundary the suite
;; deliberately runs for real.
;;
;; THE RESPONSE DECODING IS PRODUCTION'S.  `agent-repl-connect--reader' is
;; the one HTTP/1.1 response reader in this repository -- status line, then a
;; body under whichever framing the Go server picked -- and the harness calls
;; it rather than carrying a second copy that can drift from the one the
;; editor actually uses.

(defun agent-repl-itest--http-read-body (raw path)
  "Return the status and payload of RAW, one whole HTTP/1.1 response.
PATH names the request in any failure message.  Decoding is
`agent-repl-connect--reader''s, so a `Content-Length' body and a
`Transfer-Encoding: chunked' one are both handled by the same code
production reads the daemon with."
  (let* ((reader (agent-repl-connect--reader-create))
         (payload (agent-repl-connect--reader-feed reader raw))
         (breach (agent-repl-connect--reader-breach reader))
         (status (agent-repl-connect--reader-status reader)))
    (when breach
      (error "agent-repl-itest: %s answered with unreadable framing: %s" path breach))
    (unless status
      (error "agent-repl-itest: %s answered with no complete status line: %S" path raw))
    (cons status payload)))

(defun agent-repl-itest--http-request (address path body)
  "Send one HTTP/1.1 request for PATH at ADDRESS and return (STATUS . PAYLOAD).
With BODY (a string) the request is a POST, otherwise a GET.  The request
asks for `Connection: close', so an answer with no declared framing still
ends at the end-of-file."
  (let* ((host-port (agent-repl-connect--split-address address))
         (reader (agent-repl-connect--reader-create))
         (payload "")
         (proc (make-network-process
                :name "agent-repl-itest-control"
                :host (car host-port) :service (cdr host-port)
                :coding 'binary :noquery t
                :filter (lambda (_p text)
                          (setq payload
                                (concat payload
                                        (agent-repl-connect--reader-feed reader text)))))))
    (unwind-protect
        (progn
          (process-send-string
           proc
           (concat (if body "POST " "GET ") path " HTTP/1.1\r\n"
                   "Host: " address "\r\n"
                   "Connection: close\r\n"
                   (if body
                       (concat "Content-Type: application/json\r\n"
                               "Content-Length: "
                               (number-to-string (string-bytes body)) "\r\n")
                     "")
                   "\r\n"
                   (or body "")))
          ;; The reader knows when a framed body is whole; a body delimited by
          ;; the close is whole when the socket is.  Neither is a sleep.
          (while (and (process-live-p proc)
                      (not (agent-repl-connect--reader-complete-p reader)))
            (accept-process-output proc 1))
          (let ((breach (agent-repl-connect--reader-breach reader))
                (status (agent-repl-connect--reader-status reader)))
            (when breach
              (error "agent-repl-itest: %s answered with unreadable framing: %s" path breach))
            (unless status
              (error "agent-repl-itest: %s answered with no complete header block" path))
            (cons status (decode-coding-string payload 'utf-8))))
      (when (process-live-p proc) (delete-process proc)))))

(defun agent-repl-itest--control (daemon path &optional body)
  "Call DAEMON's control-plane PATH and return (STATUS . PARSED).
With BODY (a string of JSON) the call is a POST, otherwise a GET.  STATUS
is the HTTP status as an integer and PARSED the decoded JSON body (an
alist with symbol keys, `list' arrays), or the raw string when the body
is not JSON.  A non-2xx status is returned rather than signalled: several
scenarios assert on a deliberate 400."
  (let* ((answer (agent-repl-itest--http-request
                  (agent-repl-itest-daemon-address daemon) path body))
         (payload (string-trim (cdr answer))))
    (cons (car answer)
          (if (string-empty-p payload)
              nil
            (condition-case nil
                (json-parse-string payload :object-type 'alist :array-type 'list
                                   :null-object :null :false-object :false)
              (error payload))))))

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
The request is still recorded and validated; only the answer waits.
This is how a scenario pins an ORDER — the handover\='s \"call
AdoptHostWorkspace ... THEN cancel the old stream and re-subscribe\" is
only observable while the adopt is in flight.

METHOD may also be one of Emacs\='s three STREAMS
(`WatchHostWorkspace\=', `WatchDaemon\=', `WatchWorkspaceRoster\='), and
then the withheld answer is the stream\='s ACCEPTANCE: a gated stream is
DIALLED but not accepted, flushes no headers and lists no subscriber
(fanout §3 STANDING-STREAM ACCEPTANCE).  That is the only way to stage a
client that must wait for acceptance before it acts."
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

(defun agent-repl-itest--reset (daemon)
  "Return DAEMON to its start-of-process state and report what was cleared.
Drops the recording, the scripted table, the stored snapshots and every
armed gate, and ends every standing stream with a clean end frame."
  (agent-repl-itest--control-ok daemon "/_fake/reset" "{}"))

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

(defvar agent-repl-itest--orphaned-log-targets nil
  "Durable workspace log targets production has stopped owning this scenario.

A WORKSPACE TEARDOWN RELEASES THE TARGET THE CANONICAL LINK NAMES.
`agent-repl--ws-del\=' forgets the workspace\='s target
(`agent-repl--ws-forget-emacs-log-target\='), so the next workspace-owned
record re-resolves the sink from scratch.  Since the standing-target rule
landed that usually REJOINS the same file, but it need not: a target at the
cap, a link a scenario replaced, or a directory rebinding all put the older
records somewhere the link no longer names, and a reader that followed only
the link would conclude a verb never logged its success ack.

The fixture therefore remembers each target at the ONE moment it is
released, and `agent-repl-itest--workspace-log-files\=' reads those too --
DEDUPED against the link, because a rejoined target is one file and reading
it twice would count every record in it twice.")

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
    ;; The targets a teardown released come FIRST, because they carry the
    ;; older records and `agent-repl-itest--log-records\=' returns its
    ;; sinks in the order it is given them.
    ;;
    ;; DEDUPED BY THE FILE ITSELF, not by the path that reaches it.  A
    ;; released target the workspace's link still names -- the ordinary case
    ;; under the standing-target rule -- is reachable both ways, and reading
    ;; one file through two names counted every record in it twice.
    (agent-repl-itest--distinct-log-files
     (append (seq-filter #'file-exists-p
                         (reverse agent-repl-itest--orphaned-log-targets))
             (nreverse paths)))))

(defun agent-repl-itest--distinct-log-files (paths)
  "Return PATHS with every path naming an already-listed FILE removed.
Order is preserved, and the first name a file is reached by is the one
kept, so the released-target-first ordering above survives deduplication."
  (let ((seen (make-hash-table :test #'equal))
        (kept nil))
    (dolist (path paths (nreverse kept))
      (let ((identity (file-truename path)))
        (unless (gethash identity seen)
          (puthash identity t seen)
          (push path kept))))))

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

(defconst agent-repl-itest--transport-process-prefix "agent-repl-connect-"
  "Name prefix every socket `agent-repl-connect--open-socket' opens carries.
`connect.el' names a unary exchange `agent-repl-connect-<method>' and a
stream `agent-repl-connect-stream-<method>'; both are matched by this.")

(defun agent-repl-itest--reap-transport-children ()
  "Reap every transport child still running, sentinel dropped first.

A SCENARIO OWNS EVERY PROCESS IT STARTED, AND AN EXCHANGE'S ANSWER IS A
CONTINUATION.  connect.el delivers a Connect answer -- including a
transport FAILURE, which is what an unanswered probe eventually is --
from its curl child's sentinel, and Emacs runs a sentinel from the event
loop rather than at the moment the process dies.  An exchange still in
flight when the scenario returns therefore resumes AFTER `cl-letf' has
put the external-boundary guards back and after the scenario's own
scratch bindings have unwound: cold start's probe answers `nothing is
listening', builds with the DEFAULT build script, spawns the DEFAULT
binary, and errors out of a sentinel -- which aborts the whole batch run
and names whichever test happened to be running.

Dropping the sentinel before the reap is what makes the resumption
impossible rather than unlikely; killing without it would simply deliver
the same answer one line earlier."
  (dolist (process (process-list))
    (when (string-prefix-p agent-repl-itest--transport-process-prefix
                           (process-name process))
      (set-process-sentinel process #'ignore)
      (set-process-filter process #'ignore)
      (when (process-live-p process)
        (delete-process process)))))

(defun agent-repl-itest--teardown-link ()
  "Close any standing daemon link, cancel its reconnect, reap its children.
`agent-repl-link-teardown\=' closes the connections and forgets the link
state; the reconnect timer is armed separately by the close handler, so
it is cancelled here too -- a timer that survives the scenario reconnects
into the NEXT one\='s daemon.  The transport children are reaped last,
because a close is not a reap: an exchange the teardown did not know
about is still holding a curl child whose sentinel would answer into the
next scenario."
  (ignore-errors (agent-repl-link-teardown))
  (ignore-errors (agent-repl-link--cancel-reconnect))
  (agent-repl-itest--reap-transport-children))

;;;; ---- The ONE fake daemon a suite run shares ----
;;
;; A fake-daemon process costs seconds to boot, and the integration suites run
;; hundreds of scenarios.  ONE daemon therefore serves the whole batch Emacs
;; process: it is started lazily on the first scenario that needs it, RESET
;; between scenarios, and reaped at process exit.
;;
;; A RESET IS NOT A WEAKER TEARDOWN.  What a fresh process used to guarantee
;; was that no scenario could see another\='s state, and that comes from three
;; things, all of which the reset does: the fake\='s own tables are cleared and
;; its standing streams ended (`/_fake/reset\='), the shared state root is swept
;; back to nothing but `daemon.addr\=', and `daemon.addr\=' is re-published so a
;; scenario that pointed it at a stub daemon cannot misdirect the next one.

(defvar agent-repl-itest--shared-daemon nil
  "The one fake daemon this Emacs process shares, or nil before the first.
A scenario may legitimately STOP it (cold start\='s absent-address cases
do), so the accessor treats a dead process as an absent one and respawns
into the same state root.")

(defun agent-repl-itest--shutdown-shared-daemon ()
  "Reap the shared daemon and its state root.  Runs on `kill-emacs-hook'."
  (when agent-repl-itest--shared-daemon
    (let ((daemon agent-repl-itest--shared-daemon))
      (setq agent-repl-itest--shared-daemon nil)
      (agent-repl-itest--at-exit
       "stop the shared fake daemon"
       (lambda () (agent-repl-itest--stop-daemon daemon t)))))
  (when agent-repl-itest--shared-state-dir
    (let ((dir agent-repl-itest--shared-state-dir))
      (setq agent-repl-itest--shared-state-dir nil)
      (agent-repl-itest--at-exit
       (format "remove the shared state root %s" dir)
       (lambda () (agent-repl-itest--remove-tree dir))))))

(defun agent-repl-itest--shared-daemon ()
  "Return the shared fake daemon, starting or restarting it when needed."
  (unless agent-repl-itest--shared-state-dir
    (setq agent-repl-itest--shared-state-dir (agent-repl-itest--private-state-dir))
    (add-hook 'kill-emacs-hook #'agent-repl-itest--shutdown-shared-daemon))
  (unless (and agent-repl-itest--shared-daemon
               (process-live-p
                (agent-repl-itest-daemon-process agent-repl-itest--shared-daemon)))
    (setq agent-repl-itest--shared-daemon
          (agent-repl-itest--start-daemon agent-repl-itest--shared-state-dir)))
  agent-repl-itest--shared-daemon)

(defun agent-repl-itest--sweep-state-dir (daemon)
  "Empty DAEMON\='s state root of everything but `daemon.addr'.
Production writes into the state root (the JSONL log, the notes state
file), and a scenario that read another scenario\='s leftovers there would
be exactly the leak a fresh process used to prevent."
  (let ((dir (agent-repl-itest-daemon-state-dir daemon)))
    (dolist (entry (directory-files dir t directory-files-no-dot-files-regexp t))
      (unless (equal (file-name-nondirectory entry) "daemon.addr")
        ;; A failed sweep SIGNALS: a leftover is exactly the cross-scenario
        ;; leak the sweep exists to prevent.
        (if (file-directory-p entry)
            (agent-repl-itest--remove-tree entry)
          (delete-file entry))))))

(defun agent-repl-itest--sweep-fixture-root ()
  "Empty this process\='s fixture root, so no scenario inherits a log sink.

A FIXTURE WORKSPACE\='S RECORDS OUTLIVE THE SCENARIO THAT WROTE THEM.  The
suites reuse one directory per suite, and a registered workspace\='s
records reach a reader only through that directory\='s canonical
`.claude/emacs/emacs.log\=' symlink.  The log-target registry is scratch-
bound per scenario, so each scenario mints a FRESH target -- but the link
still names the PREVIOUS scenario\='s target until this scenario writes
its first workspace-owned record.

In that window `agent-repl-itest--await-log\=' is satisfied instantly by
the previous scenario\='s record for the same operation, and the assertion
behind it then reads THAT record\='s arguments -- the refusal fields, the
arm keyword -- and fails on a scenario whose own answer had not arrived
yet.  Sweeping the root is what makes a record found a record this
scenario wrote; production recreates the `.claude\=' tree the moment it
routes one.

A sweep that fails SIGNALS, failing the scenario it begins: the leftover
it could not remove is the very cross-scenario leak described above."
  (agent-repl-itest--remove-tree agent-repl-itest--fixture-root))

(defun agent-repl-itest--republish-addr (daemon)
  "Point DAEMON\='s state root at DAEMON, whatever last wrote `daemon.addr'.
A cold-start scenario spawns a STUB daemon that publishes its own address
into the shared root; left there, production\='s discovery in the next
scenario would dial a process that no longer exists."
  (let ((path (agent-repl-itest--addr-file (agent-repl-itest-daemon-state-dir daemon)))
        (address (agent-repl-itest-daemon-address daemon)))
    (unless (equal (agent-repl-itest--read-addr-file
                    (agent-repl-itest-daemon-state-dir daemon))
                   address)
      (with-temp-file path (insert address "\n")))))

(defun agent-repl-itest--begin-scenario ()
  "Hand the next scenario a shared daemon indistinguishable from a fresh one.
Returns it.  Everything the previous scenario could have left behind — the
fake\='s tables, its standing streams, the state root\='s contents and the
published address — is cleared here rather than at the end of the scenario
that made it, so a scenario that dies mid-way cannot poison its successor."
  (let ((daemon (agent-repl-itest--shared-daemon)))
    (agent-repl-itest--reset daemon)
    (agent-repl-itest--wait-until
     (lambda () (null (agent-repl-itest--subscribers daemon)))
     5 "the previous scenario's stream subscribers to drain")
    ;; A SECOND reset, after the drain.  The previous scenario's transport
    ;; children are killed asynchronously, so one of them can still land a
    ;; request on the shared daemon between the first reset and its own death
    ;; -- a call the next scenario would then read as its own.  A per-test
    ;; process could not leak that because it died with the scenario; the
    ;; shared one closes the window by clearing again once nothing is left
    ;; subscribed.
    (agent-repl-itest--reset daemon)
    (agent-repl-itest--sweep-state-dir daemon)
    (agent-repl-itest--sweep-fixture-root)
    (agent-repl-itest--republish-addr daemon)
    daemon))

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

(defun agent-repl-itest--real-open-socket ()
  "Return the REAL `agent-repl-connect--open-socket' captured before guarding.
The transport's ONE dial point is the boundary the integration suite
exists to exercise, against a fake daemon on loopback."
  (agent-repl-itest--real-boundary 'agent-repl-connect--open-socket))

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
- the ONE external boundary the transport dials through is restored to
  its real implementation, and every other guard stays armed;
- the webview factory records into `agent-repl-itest-webview-urls'.

The daemon is always stopped and its state dir removed, even when BODY
signals."
  (declare (indent 1) (debug (symbolp body)))
  `(let ((,var (agent-repl-itest--begin-scenario)))
     (let* ((agent-repl-itest-webview-urls nil)
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
                ;; Real blink-cadence scenarios arm keyed timers.  Give the
                ;; scenario its own registry and marker table so their later
                ;; steps cannot fire after the workspace registry unwinds.
                (agent-repl--timers nil)
                (agent-repl-status--marker-on (make-hash-table :test 'equal))
                ;; Preserve the production handover consumers but isolate
                ;; scenario-owned, self-removing continuations.  A transfer
                ;; that intentionally waits for a later announcement leaves
                ;; one such continuation armed; it must not fire during a
                ;; different scenario's promotion with the old workspace name.
                (agent-repl-link-handover-functions
                 (copy-sequence agent-repl-link-handover-functions))
                ;; The log-target registry belongs in this list for the SAME
                ;; reason as the others, and for one more: it is what decides
                ;; where a scenario's workspace-owned records LAND.  Left
                ;; process-global, every scenario using one fixture directory
                ;; appends to the one target the first scenario minted, so a
                ;; later scenario's `--await-log' is satisfied by an EARLIER
                ;; scenario's record for the same operation and the assertion
                ;; behind it races the reply that was supposed to produce one.
                ;; Binding it here gives each scenario its own target and its
                ;; own canonical link, so a record found is a record this
                ;; scenario wrote.
                (agent-repl--workspace-log-targets (make-hash-table :test #'equal))
                ;; The arrival gate (panels.el) is a SCRATCH REGISTRY for the
                ;; same reason as the tables above: it is armed by the first
                ;; roster reconcile of a session, and left process-global the
                ;; first scenario's reconcile would make every later
                ;; scenario's first roster push open panels -- a background
                ;; perspective switch that scenario never asked for.
                (agent-repl--panels-arrivals-armed nil)
                (agent-repl--panels-arrival-reasons (make-hash-table :test 'equal))
                ;; The editor's startup is process-wide (startup.el): each
                ;; scenario runs with it over unless it stages one itself.
                (agent-repl-startup--phase 'done)
                (agent-repl-startup--go-aheads nil)
                (agent-repl-startup--next 0)
                (agent-repl-startup--finished nil)
                (agent-repl-startup--released (make-hash-table :test 'equal))
                (agent-repl-startup--loaded (make-hash-table :test 'equal))
                (agent-repl-startup--loading-said (make-hash-table :test 'equal))
                (agent-repl-itest--real-ws-put
                 (symbol-function 'agent-repl--ws-put))
                (process-environment
                 (append (list (concat "AGENT_REPL_STATE_DIR="
                                       (agent-repl-itest-daemon-state-dir ,var))
                               "AGENT_REPL_FORBID_VENDOR_CALLS=1")
                         process-environment))
                (agent-repl-log-to-file t)
                (agent-repl-log-file-name (agent-repl-itest--log-file ,var))
                ;; Integration assertions cover debug request boundaries; the
                ;; production default is info, so this scenario-local switch
                ;; opens the sink without changing the process environment.
                (agent-repl-log-file-level 'debug))
       (cl-letf (((symbol-function 'agent-repl-connect--open-socket)
                      (agent-repl-itest--real-open-socket))
                     ((symbol-function 'agent-repl--ws-put)
                      (lambda (ws key value)
                        (agent-repl-itest--ws-put-with-real-project-dir
                         agent-repl-itest--real-ws-put ws key value)))
                     ((symbol-function 'agent-repl--ws-forget-emacs-log-target)
                      (let ((real (symbol-function
                                   'agent-repl--ws-forget-emacs-log-target)))
                        (lambda (ws reason)
                          (agent-repl-itest--note-orphaned-log-targets ws)
                          (funcall real ws reason))))
                     ((symbol-function 'agent-repl--frontend-make-webview-buffer)
                      (agent-repl-test--fake-webview-factory 'agent-repl-itest-webview-urls)))
         (unwind-protect
             (progn ,@body)
           ;; A verb may STAND THE LINK on its own (a daemon-admin verb can be
           ;; the first thing that needs one), and a link outliving its daemon
           ;; leaves an armed reconnect timer that fires into the next
           ;; scenario.  Tearing it down is the only thing that actually ends
           ;; it -- the same reason the cold-start reset exists.
           ;;
           ;; Keep teardown INSIDE the scenario's scratch workspace and host
           ;; registries.  Closing a promoted stream runs sentinels immediately;
           ;; after these bindings unwind, those records would resolve against
           ;; unrelated process-global test state and report a false unroutable
           ;; workspace.
           ;;
           ;; The daemon is NOT stopped: it is the one this whole run shares,
           ;; and `agent-repl-itest--begin-scenario' is what hands the next
           ;; scenario a clean one.  Doing the cleaning on the way IN rather
           ;; than on the way out is deliberate — a scenario that dies mid-way
           ;; still cannot poison its successor.
           (agent-repl-itest--teardown-link)
           (agent-repl--cancel-all-timers))))))

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
       ;; The state dir belongs to the SHARED daemon; only the successor
       ;; process is reaped here.  Its orderly exit removes `daemon.addr',
       ;; which is PRIMARY's discovery file too, so the primary re-publishes
       ;; its own address afterwards — a scenario after this one must find the
       ;; daemon that is still running, not the file the successor took away.
       (agent-repl-itest--stop-daemon ,var t)
       (when (process-live-p (agent-repl-itest-daemon-process ,primary))
         (agent-repl-itest--republish-addr ,primary)))))

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
    agent-repl--frontend-artifact-exists-p
    agent-repl--frontend-run-log-tail
    agent-repl--frontend-stdio-log-tail)
  "The external boundaries a cold-start scenario exercises for real.
Their targets are all test-owned: a stub script and a stub argv written
into the scenario's own temp dir, `file-exists-p' on those paths, and the
run log under the scenario's own private `AGENT_REPL_STATE_DIR'.

`--run-log-tail' is on the list because THE STUB DAEMON EXITS.  Every
cold-start stub here is a shell script that does its one job and returns,
and `agent-repl-daemon--await-address' polls once INLINE before arming
its timer -- so whenever the stub has already exited by that first tick,
production takes its `boot-exited' branch and reads the run log to say
WHY, which is the whole point of that branch.  Which side of the tick the
exit lands on is a race with the scheduler, so leaving this boundary
guarded made the guard fire on a slow host and not on a fast one: a flake
whose cause is the harness's own list, not the scenario.")

(defvar agent-repl-daemon--ensure-in-flight)
(defvar agent-repl-daemon--build-in-flight)
(defvar agent-repl-daemon--build-process)
(defvar agent-repl-daemon--build-continuations)
(defvar agent-repl-daemon--build-started)
(defvar agent-repl-daemon--build-labels)
(defvar agent-repl-daemon--build-target-names)
(defvar agent-repl-daemon--build-status-timer)
(defvar agent-repl-daemon--boot-timer)
(defvar agent-repl-daemon--boot-process)
(defvar agent-repl-daemon--boot-deadline)
(defvar agent-repl-daemon--departure-timer)
(defvar agent-repl-daemon--departure-deadline)
(defvar agent-repl-daemon--departure-continuation)
(defvar agent-repl-daemon--boot-continuation)
(defvar agent-repl-daemon-build-failure)
(defvar agent-repl-daemon-mode-line-segment)
(defvar agent-repl--frontend-daemon-process)
(declare-function agent-repl-daemon--cancel-boot-wait "daemon")
(declare-function agent-repl-daemon--cancel-departure-wait "daemon")
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
  ;; The departure wait is armed by a restart and is NOT reached by
  ;; `--cancel-boot-wait'; its continuation runs the ensure behind it, so a
  ;; tick that survives the scenario reaches the guarded boundaries too.
  (agent-repl-daemon--cancel-departure-wait)
  ;; A BUILD SCRIPT'S SENTINEL OUTLIVES THE COLD-START WINDOW.  The build
  ;; boundary is asynchronous, so a scenario that asserts as soon as its stub
  ;; script has DONE ITS WORK returns before the exit behind it is delivered
  ;; -- and Emacs runs a sentinel from the event loop, never at the moment
  ;; the process dies, so it fires after `cl-letf' has put the
  ;; external-boundary guards back.  `agent-repl-daemon--build-finished' then
  ;; runs the continuation, which starts the daemon and polls once inline
  ;; (`agent-repl-daemon--await-address' ends with a `--boot-tick'), reaching
  ;; `agent-repl--frontend-run-log-tail' and `--artifact-exists-p' with the
  ;; guards armed.  That is a hard error out of a sentinel, which aborts the
  ;; whole batch run and names whichever test happened to be running.
  ;;
  ;; The sentinel is therefore dropped whenever the process OBJECT exists --
  ;; not only while it is live, because a queued-but-unrun sentinel belongs to
  ;; a process that is already dead, which is exactly the case that escaped.
  (when (processp agent-repl-daemon--build-process)
    (set-process-sentinel agent-repl-daemon--build-process #'ignore)
    (when (process-live-p agent-repl-daemon--build-process)
      (delete-process agent-repl-daemon--build-process)))
  (when (timerp agent-repl-daemon--build-status-timer)
    (cancel-timer agent-repl-daemon--build-status-timer))
  (when (processp agent-repl--frontend-daemon-process)
    (set-process-sentinel agent-repl--frontend-daemon-process #'ignore)
    (when (process-live-p agent-repl--frontend-daemon-process)
      (delete-process agent-repl--frontend-daemon-process)))
  ;; THE RECONNECT TIMER IS A COLD-START TRIGGER, not just a link detail.
  ;; `agent-repl-daemon-ensure' is registered on
  ;; `agent-repl-link-no-daemon-functions' at load time, so a reconnect that
  ;; fires after this window and finds no `daemon.addr' starts a WHOLE cold
  ;; start -- default build script, default binary, a fresh boot poll -- with
  ;; the external-boundary guards armed again, and the boot tick then errors
  ;; out of a timer and aborts the batch run.  A bare
  ;; `agent-repl-link-teardown' does not cancel that timer; the harness's own
  ;; teardown does, and reaps the transport children the same way (the
  ;; cold-start probe is asynchronous, so its answer can still be in flight).
  (agent-repl-itest--teardown-link)
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
         ;; The build's own state, scratch-bound for the same reason the boot
         ;; state is: a scenario that leaves a build in flight would otherwise
         ;; make the NEXT scenario's ensure coalesce onto it and spawn nothing.
         (agent-repl-daemon--build-in-flight nil)
         (agent-repl-daemon--build-process nil)
         (agent-repl-daemon--build-continuations nil)
         (agent-repl-daemon--build-started nil)
         (agent-repl-daemon--build-labels nil)
         (agent-repl-daemon--build-target-names nil)
         (agent-repl-daemon--build-status-timer nil)
         (agent-repl-daemon--boot-timer nil)
         (agent-repl-daemon--boot-process nil)
         (agent-repl-daemon--boot-deadline nil)
         ;; The DEPARTURE wait is the boot wait's mirror image and leaks the
         ;; same way: `agent-repl-frontend-daemon-restart' arms it, its
         ;; continuation runs the ensure behind it, and an ensure reached
         ;; from a tick after the scenario returned finds the guards armed.
         (agent-repl-daemon--departure-timer nil)
         (agent-repl-daemon--departure-deadline nil)
         (agent-repl-daemon--departure-continuation nil)
         (agent-repl-daemon--boot-continuation nil)
         (agent-repl-daemon-build-failure nil)
         (agent-repl-daemon-mode-line-segment nil)
         (agent-repl--frontend-daemon-process nil))
     (cl-letf (,@(mapcar (lambda (boundary)
                           `((symbol-function ',boundary)
                             (agent-repl-itest--real-boundary ',boundary)))
                         agent-repl-itest--cold-start-boundaries)
               ;; The elisp staleness pre-check is stubbed to "everything is
               ;; stale" rather than restored: its artifacts are the REAL
               ;; module's, so a real probe would make whether the stub build
               ;; script runs at all depend on the developer's working tree.
               ((symbol-function 'agent-repl--frontend-file-mtime)
                (lambda (_path) nil))
               ((symbol-function 'agent-repl--frontend-source-files)
                (lambda (_dir _regexp) nil)))
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
