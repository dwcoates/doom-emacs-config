;;; connect.el --- Connect-over-HTTP/1.1 transport for agent-repl -*- lexical-binding: t; -*-

;;; Commentary:

;; The elisp side of the Connect protocol (connectrpc.com), spoken against
;; the agent-repl daemon's single loopback listener.  This file owns the
;; TRANSPORT and nothing else: it moves JSON strings, it does not know a
;; single field name of `agentrepl.v1'.  The codec (wire-*.el) turns plists
;; into JSON strings and back; `rpc.el' pairs a codec call with a transport
;; call.
;;
;; THE PROTOCOL, as implemented here (verified against a real Connect
;; server):
;;
;;   UNARY.  POST http://<addr>/agentrepl.v1.AgentRepl/<Method> with
;;   `Content-Type: application/json' and `Connect-Protocol-Version: 1';
;;   the body is the protojson request.  HTTP 200 carries the protojson
;;   response.  ANY non-200 carries a Connect error object
;;   `{"code": "...", "message": "..."}' as its body.
;;
;;   SERVER-STREAMING.  Same path, `Content-Type: application/connect+json'.
;;   The request body is exactly ONE envelope; the response body is a
;;   sequence of envelopes.  An envelope is a 1-byte flag, a 4-byte
;;   big-endian payload length, then that many payload bytes.  Flag bit
;;   0x02 marks the terminal EndStreamResponse, whose payload is `{}' for a
;;   clean end or `{"error": {"code": ..., "message": ...}}' for a failure.
;;   Bit 0x01 (compression) is never negotiated and never expected.
;;
;; HTTP STATUS MECHANISM (the implementer's documented choice, per the
;; fanout spec §3): `curl -D -' dumps the response header block onto
;; stdout ahead of the body, and ONE reader state machine
;; (`agent-repl-connect--reader') consumes that block before handing the
;; rest through as body bytes.  The same mechanism serves unary and
;; streaming, so there is one status path rather than two: `-w' would only
;; work for unary (it prints at completion), and a stream must know its
;; status the moment the headers land, long before the process exits.
;; `-H "Expect:"' suppresses curl's 100-continue negotiation so exactly one
;; header block ever arrives, and no `-L' is passed so no redirect can add
;; another.
;;
;; STANDING-STREAM ACCEPTANCE.  The daemon flushes its response headers on
;; accept, so the HTTP 200 header block IS the acceptance of a
;; subscription — before any frame, and long before a first push that a
;; standing stream may never send at all.  `agent-repl-connect-stream'
;; exposes that instant as its optional ON-OPEN, and daemon-link, host and
;; roster key "subscribed" on it rather than on a spawn or a first frame.
;;
;; EVERY process spawn in this file goes through the single boundary
;; wrapper `agent-repl-connect--spawn-curl', registered in
;; `agent-repl--external-boundary-functions' (core.el) so batch tests are
;; guarded against reaching a real `curl'.  The argv it is handed is built
;; by the pure `agent-repl-connect--curl-argv'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function agent-repl--log "core" (ws fmt &rest args))
(declare-function agent-repl--info "core" (ws fmt &rest args))
(declare-function agent-repl--warn "core" (ws fmt &rest args))
(declare-function agent-repl--error "core" (ws fmt &rest args))
(declare-function agent-repl--global-state-file "core" (relative))

;;;; ---- Errors and customization ----

(define-error 'agent-repl-connect-error
  "agent-repl Connect transport failure")

(defcustom agent-repl-connect-curl-program "curl"
  "Program used to speak HTTP/1.1 to the daemon.
The default resolves `curl' on `exec-path'.  Set to an absolute path when
the ambient `curl' is not the one that should be used."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-connect-unary-timeout-seconds 10
  "Seconds a unary Connect call may run before it is failed as timed out.
Applies to `agent-repl-connect-unary' and `agent-repl-connect-unary-sync'
whenever the caller does not pass its own timeout.  Streams are standing
and are never timed out here."
  :type 'number
  :group 'agent-repl)

(defconst agent-repl-connect--service-path "/agentrepl.v1.AgentRepl/"
  "Path prefix of every `agentrepl.v1.AgentRepl' rpc.
The Connect URL of a method is this prefix plus the bare method name.")

(defconst agent-repl-connect--unary-content-type "application/json"
  "Request `Content-Type' of a unary Connect call (protojson body).")

(defconst agent-repl-connect--stream-content-type "application/connect+json"
  "Request `Content-Type' of a Connect server-streaming call (envelopes).")

(defconst agent-repl-connect--end-stream-flag #x02
  "Connect envelope flag bit marking the terminal EndStreamResponse frame.")

;;;; ---- daemon.addr discovery ----

(defconst agent-repl-connect--daemon-addr-basename "daemon.addr"
  "Basename of the daemon's address file under the state dir.")

(defconst agent-repl-connect--address-regexp
  "\\`\\([0-9]+\\(?:\\.[0-9]+\\)\\{3\\}\\|\\[[0-9A-Fa-f:]+\\]\\):\\([0-9]+\\)\\'"
  "Regexp a `daemon.addr' payload must match.
The daemon writes the literal `127.0.0.1:<port>'; a bracketed IPv6
literal is accepted so a future loopback spelling is not a parse crash.")

(defun agent-repl-connect-daemon-addr-file ()
  "Return the absolute path of the daemon's address file.
Lives at `daemon.addr' under agent-repl's canonical state dir
\(`$AGENT_REPL_STATE_DIR', default `~/.claude-emacs'), which is the one
state root shared by the daemon, Emacs, the skills and the tests."
  (agent-repl--global-state-file agent-repl-connect--daemon-addr-basename))

(defun agent-repl-connect-read-daemon-addr ()
  "Return the daemon's `HOST:PORT' address string, or nil when no daemon.
An ABSENT file is the legal no-daemon state and answers nil.  MALFORMED
content is a loud misconfiguration, never a silent fallback: it is logged
at ERROR and signals `agent-repl-connect-error'."
  (let ((file (agent-repl-connect-daemon-addr-file)))
    (if (not (file-readable-p file))
        (progn
          (agent-repl--log nil "elisp.connect.daemon-addr-absent file=%S" file)
          nil)
      (let ((raw (with-temp-buffer
                   (insert-file-contents file)
                   (buffer-string))))
        (let ((address (string-trim raw)))
          (if (string-match-p agent-repl-connect--address-regexp address)
              (progn
                (agent-repl--log nil "elisp.connect.daemon-addr-read file=%S address=%S"
                                 file address)
                address)
            (agent-repl--error nil "elisp.connect.daemon-addr-malformed file=%S content=%S"
                               file raw)
            (signal 'agent-repl-connect-error
                    (list (agent-repl-connect--failure
                           :malformed-addr
                           (format "malformed daemon.addr content: %S" raw))))))))))

;;;; ---- Failure detail ----

(cl-defun agent-repl-connect--failure (kind message &key code status)
  "Return the failure plist handed to ON-FAILURE / ON-CLOSE `(:error D)'.
KIND is the transport-level classification keyword: `:http' (the daemon
answered with a non-200 and a Connect error body), `:transport' (curl
could not complete the exchange), `:timeout', `:malformed' (a 200 whose
body is not the JSON it must be), `:malformed-addr', or `:no-end-frame'
\(the producer closed a stream without its terminal envelope).  MESSAGE is
the human-facing text.  CODE is the Connect error code string when the
daemon supplied one; STATUS is the HTTP status when one was read."
  (list :kind kind :code code :status status :message message))

(defun agent-repl-connect-failure-message (detail)
  "Return the human-facing text of failure plist DETAIL."
  (plist-get detail :message))

;;;; ---- JSON ----

(defun agent-repl-connect--parse-json (string)
  "Parse STRING as protojson using the transport's fixed options.
Returns an alist keyed by symbols spelled exactly as on the wire.  The
options are fixed repository-wide (fanout spec §2) so every consumer sees
one representation.  Signals `json-parse-error' on invalid input; callers
classify."
  (json-parse-string string
                     :object-type 'alist
                     :array-type 'list
                     :null-object :null
                     :false-object :false))

(defun agent-repl-connect--connect-error-detail (status body)
  "Return a failure plist for a non-200 answer with BODY at HTTP STATUS.
A Connect error body is `{\"code\": ..., \"message\": ...}'.  A body that
does not parse still yields a failure — the status alone is the fact, and
the raw body rides along as the message so nothing is swallowed."
  (condition-case nil
      (let* ((parsed (agent-repl-connect--parse-json body))
             (code (alist-get 'code parsed))
             (message (alist-get 'message parsed)))
        (if (stringp code)
            (agent-repl-connect--failure
             :http (or (and (stringp message) message) code)
             :code code :status status)
          (agent-repl-connect--failure
           :http (format "HTTP %s: %s" status (string-trim body))
           :status status)))
    (error
     (agent-repl-connect--failure
      :http (format "HTTP %s: %s" status (string-trim body))
      :status status))))

;;;; ---- Envelope codec (pure) ----

(cl-defstruct (agent-repl-connect-envelope-parser
               (:constructor agent-repl-connect-envelope-parser-create)
               (:copier nil))
  "Incremental Connect envelope parser.
BUFFER holds the bytes received so far that do not yet form a whole
envelope; a frame split across any number of process-filter chunks is
reassembled here."
  (buffer "" :type string))

(defun agent-repl-connect-envelope-encode (flags payload)
  "Return the Connect envelope carrying PAYLOAD under FLAGS as a unibyte string.
FLAGS is the 1-byte flag field (0 for a message, `#x02' for the terminal
frame).  PAYLOAD is the JSON text; it is UTF-8 encoded and its BYTE length
is what the 4-byte big-endian length field carries."
  (let* ((bytes (encode-coding-string payload 'utf-8 t))
         (len (length bytes)))
    (concat (unibyte-string (logand flags #xff)
                            (logand (ash len -24) #xff)
                            (logand (ash len -16) #xff)
                            (logand (ash len -8) #xff)
                            (logand len #xff))
            bytes)))

(defun agent-repl-connect--envelope-length (buffer)
  "Return the big-endian payload length encoded at bytes 1..4 of BUFFER."
  (logior (ash (aref buffer 1) 24)
          (ash (aref buffer 2) 16)
          (ash (aref buffer 3) 8)
          (aref buffer 4)))

(defun agent-repl-connect-envelope-feed (parser bytes)
  "Feed BYTES into PARSER and return every WHOLE frame it now yields.
Each frame is a cons `(FLAGS . PAYLOAD-STRING)', PAYLOAD-STRING decoded
from UTF-8.  Partial trailing input stays in PARSER for the next call, so
frames split across chunks and several frames merged into one chunk are
both handled.  Frames come back in arrival order."
  (setf (agent-repl-connect-envelope-parser-buffer parser)
        (concat (agent-repl-connect-envelope-parser-buffer parser) bytes))
  (let ((frames nil)
        (done nil))
    (while (not done)
      (let ((buffer (agent-repl-connect-envelope-parser-buffer parser)))
        (if (< (length buffer) 5)
            (setq done t)
          (let ((len (agent-repl-connect--envelope-length buffer)))
            (if (< (length buffer) (+ 5 len))
                (setq done t)
              (push (cons (aref buffer 0)
                          (decode-coding-string (substring buffer 5 (+ 5 len)) 'utf-8))
                    frames)
              (setf (agent-repl-connect-envelope-parser-buffer parser)
                    (substring buffer (+ 5 len))))))))
    (nreverse frames)))

(defun agent-repl-connect-envelope-end-frame-p (flags)
  "Return non-nil when FLAGS marks the terminal EndStreamResponse frame."
  (/= 0 (logand flags agent-repl-connect--end-stream-flag)))

;;;; ---- HTTP response reader (pure) ----

(cl-defstruct (agent-repl-connect--reader
               (:constructor agent-repl-connect--reader-create)
               (:copier nil))
  "State machine consuming curl's `-D -' header block ahead of the body.
PHASE is `:headers' until the blank line separating the header block from
the body has been seen, `:body' thereafter.  PENDING holds the header
bytes accumulated so far.  STATUS is the parsed HTTP status integer, nil
while it is still unknown."
  (phase :headers)
  (pending "")
  (status nil))

(defun agent-repl-connect--parse-status-line (block)
  "Return the HTTP status integer of header BLOCK, or nil when unparsable."
  (when (string-match "\\`HTTP/[0-9.]+ +\\([0-9]\\{3\\}\\)" block)
    (string-to-number (match-string 1 block))))

(defun agent-repl-connect--reader-feed (reader chunk)
  "Feed CHUNK into READER and return the BODY bytes it newly exposes.
While the header block is still arriving this returns the empty string and
records nothing but progress; the call that completes the block parses the
status line and returns whatever body bytes trailed it in the same chunk."
  (if (eq (agent-repl-connect--reader-phase reader) :body)
      chunk
    (let ((pending (concat (agent-repl-connect--reader-pending reader) chunk)))
      (setf (agent-repl-connect--reader-pending reader) pending)
      (let ((separator (cond ((string-match "\r\n\r\n" pending) (match-end 0))
                             ((string-match "\n\n" pending) (match-end 0)))))
        (if (not separator)
            ""
          (let ((block (substring pending 0 separator)))
            (setf (agent-repl-connect--reader-status reader)
                  (agent-repl-connect--parse-status-line block))
            (setf (agent-repl-connect--reader-phase reader) :body)
            (setf (agent-repl-connect--reader-pending reader) "")
            (substring pending separator)))))))

;;;; ---- argv (pure) and the ONE spawn boundary ----

(defun agent-repl-connect--method-url (address method)
  "Return the Connect URL of METHOD on the daemon at ADDRESS."
  (concat "http://" address agent-repl-connect--service-path method))

(defun agent-repl-connect--curl-argv (address method content-type body-file)
  "Return curl's argv for a Connect POST of BODY-FILE to METHOD at ADDRESS.
CONTENT-TYPE selects unary protojson or the streaming envelope framing.
Pure: it composes strings and touches nothing.  `--http1.1' pins the
transport the daemon's listener shares with the webapp assets, `-sS'
silences progress while keeping errors on stderr, `--no-buffer' is what
makes a server stream arrive frame by frame instead of at exit, and
`-D -' puts the response header block on stdout so the status is readable
before any body byte."
  (list "--http1.1" "-sS" "--no-buffer" "-D" "-"
        "-X" "POST"
        "-H" (concat "Content-Type: " content-type)
        "-H" "Connect-Protocol-Version: 1"
        "-H" "Expect:"
        "--data-binary" (concat "@" body-file)
        (agent-repl-connect--method-url address method)))

(defun agent-repl-connect--spawn-curl (name args filter sentinel stderr-buffer)
  "Start `agent-repl-connect-curl-program' with ARGS as process NAME.
FILTER receives raw stdout bytes, SENTINEL the exit event, STDERR-BUFFER
curl's diagnostics.

This IS the one external-boundary wrapper of connect.el: every Connect
exchange, unary or streaming, is spawned here and nowhere else.
Registered in `agent-repl--external-boundary-functions' (core.el) so the
batch harness's guard fails any test that reaches a real `curl'."
  (make-process :name name
                :command (cons agent-repl-connect-curl-program args) ;; ALLOW-EXTERNAL-BOUNDARY
                :connection-type 'pipe
                :coding 'no-conversion
                :noquery t
                :filter filter
                :sentinel sentinel
                :stderr stderr-buffer))

(defun agent-repl-connect--write-body-file (bytes)
  "Write unibyte BYTES to a fresh temp file and return its path.
The request body is handed to curl as `--data-binary @FILE' rather than on
the command line so no request size or byte value can be mangled by argv
quoting.  BYTES must already be encoded (UTF-8 for a unary protojson body,
the framed envelope for a stream); the file is written verbatim.  The
caller deletes the file when its process exits."
  (let ((file (make-temp-file "agent-repl-connect-"))
        (coding-system-for-write 'no-conversion))
    (with-temp-file file
      (set-buffer-multibyte nil)
      (insert bytes))
    file))

(defun agent-repl-connect--cleanup (body-file stderr-buffer)
  "Delete BODY-FILE and kill STDERR-BUFFER, tolerating either being gone."
  (when (and body-file (file-exists-p body-file))
    (condition-case err
        (delete-file body-file)
      (error (agent-repl--warn nil "elisp.connect.body-file-cleanup-failed file=%S error=%S"
                               body-file err))))
  (when (buffer-live-p stderr-buffer)
    (kill-buffer stderr-buffer)))

(defun agent-repl-connect--stderr-text (stderr-buffer)
  "Return the trimmed contents of STDERR-BUFFER, or the empty string."
  (if (buffer-live-p stderr-buffer)
      (string-trim (with-current-buffer stderr-buffer (buffer-string)))
    ""))

;;;; ---- Connection ----

(cl-defstruct (agent-repl-connect-connection
               (:constructor agent-repl-connect-connection-create)
               (:copier nil))
  "A logical connection to one daemon listener.
Connect is stateless per exchange, so this object owns no socket: it owns
the ADDRESS every exchange dials, the open STREAMS started on it (so a
link teardown can cancel them all), and the ALIVE-P flag that refuses new
work after teardown."
  (address nil :type string)
  (streams nil :type list)
  (alive-p t))

(defun agent-repl-connect-open (address)
  "Return a connection object for the daemon listening at ADDRESS.
ADDRESS is the `HOST:PORT' string read from `daemon.addr'.  Opens no
socket: Connect exchanges each dial their own, and this object is the
address plus the bookkeeping of the streams standing on it."
  (unless (and (stringp address)
               (string-match-p agent-repl-connect--address-regexp address))
    (agent-repl--error nil "elisp.connect.open-invalid-address address=%S" address)
    (signal 'agent-repl-connect-error
            (list (agent-repl-connect--failure
                   :malformed-addr (format "invalid daemon address: %S" address)))))
  (agent-repl--info nil "elisp.connect.open address=%S" address)
  (agent-repl-connect-connection-create :address address))

(defun agent-repl-connect-close (conn)
  "Mark CONN dead and cancel every stream still standing on it.
Each cancelled stream's ON-CLOSE runs with `(:cancelled)' — a client-side
close is never an error."
  (agent-repl--info nil "elisp.connect.close address=%S streams=%d"
                    (agent-repl-connect-connection-address conn)
                    (length (agent-repl-connect-connection-streams conn)))
  (setf (agent-repl-connect-connection-alive-p conn) nil)
  (dolist (stream (copy-sequence (agent-repl-connect-connection-streams conn)))
    (agent-repl-connect-stream-cancel stream))
  (setf (agent-repl-connect-connection-streams conn) nil))

(defun agent-repl-connect--check-alive (conn method)
  "Signal `agent-repl-connect-error' when CONN is closed, naming METHOD."
  (unless (agent-repl-connect-connection-alive-p conn)
    (agent-repl--error nil "elisp.connect.call-on-closed-connection method=%S address=%S"
                       method (agent-repl-connect-connection-address conn))
    (signal 'agent-repl-connect-error
            (list (agent-repl-connect--failure
                   :transport
                   (format "connection to %s is closed"
                           (agent-repl-connect-connection-address conn)))))))

;;;; ---- Unary ----

(cl-defun agent-repl-connect-unary (conn method json-string
                                         &key on-response on-failure timeout)
  "POST JSON-STRING to METHOD on CONN and answer asynchronously.
METHOD is the bare rpc name, e.g. \"RegisterWorkspace\".  ON-RESPONSE
receives the parsed response alist on HTTP 200.  ON-FAILURE receives a
failure plist (see `agent-repl-connect--failure') for a Connect error, a
transport failure, a timeout, or a 200 whose body will not parse.  Exactly
one of the two runs, exactly once.  TIMEOUT defaults to
`agent-repl-connect-unary-timeout-seconds'.  Nothing is ever retried here;
retry policy belongs to the caller that knows whether the call is
idempotent.  Returns the curl process."
  (agent-repl-connect--check-alive conn method)
  (let* ((address (agent-repl-connect-connection-address conn))
         (body-file (agent-repl-connect--write-body-file
                     (encode-coding-string json-string 'utf-8 t)))
         (stderr-buffer (generate-new-buffer
                         (format " *agent-repl-connect-stderr %s*" method)))
         (reader (agent-repl-connect--reader-create))
         (body "")
         (settled nil)
         (timed-out nil)
         (timer nil)
         (process nil))
    (cl-labels
        ((settle (kind value)
           (unless settled
             (setq settled t)
             (when timer (cancel-timer timer) (setq timer nil))
             (pcase kind
               (:response
                (agent-repl--log nil "elisp.connect.unary-response method=%S address=%S"
                                 method address)
                (when on-response (funcall on-response value)))
               (:failure
                (agent-repl--warn nil "elisp.connect.unary-failure method=%S address=%S detail=%S"
                                  method address value)
                (when on-failure (funcall on-failure value)))))))
      (agent-repl--log nil "elisp.connect.unary-send method=%S address=%S bytes=%d"
                       method address (string-bytes json-string))
      (setq process
            (agent-repl-connect--spawn-curl
             (format "agent-repl-connect-%s" method)
             (agent-repl-connect--curl-argv
              address method agent-repl-connect--unary-content-type body-file)
             (lambda (_proc chunk)
               (setq body (concat body (agent-repl-connect--reader-feed reader chunk))))
             (lambda (proc _event)
               (unless (process-live-p proc)
                 (let ((status (agent-repl-connect--reader-status reader))
                       (stderr (agent-repl-connect--stderr-text stderr-buffer)))
                   (agent-repl-connect--cleanup body-file stderr-buffer)
                   (cond
                    (timed-out
                     (settle :failure
                             (agent-repl-connect--failure
                              :timeout
                              (format "%s timed out after %ss" method
                                      (or timeout agent-repl-connect-unary-timeout-seconds)))))
                    ((null status)
                     (settle :failure
                             (agent-repl-connect--failure
                              :transport
                              (if (string-empty-p stderr)
                                  (format "%s: no HTTP response from %s" method address)
                                stderr))))
                    ((= status 200)
                     (let ((parsed (condition-case err
                                       (list :ok (agent-repl-connect--parse-json body))
                                     (error (list :bad err)))))
                       (if (eq (car parsed) :ok)
                           (settle :response (cadr parsed))
                         (settle :failure
                                 (agent-repl-connect--failure
                                  :malformed
                                  (format "%s: unparsable 200 body (%S)" method (cadr parsed))
                                  :status status)))))
                    (t
                     (settle :failure
                             (agent-repl-connect--connect-error-detail status body)))))))
             stderr-buffer))
      (setq timer
            (run-at-time (or timeout agent-repl-connect-unary-timeout-seconds) nil
                         (lambda ()
                           (when (process-live-p process)
                             (setq timed-out t)
                             (agent-repl--warn nil "elisp.connect.unary-timeout method=%S address=%S"
                                               method address)
                             (delete-process process)))))
      process)))

(defun agent-repl-connect-unary-sync (conn method json-string &optional timeout)
  "Call METHOD on CONN with JSON-STRING and block for the answer.
Returns the parsed response alist, or signals `agent-repl-connect-error'
with the failure plist as its data.  Waits on process output rather than
sleeping, so timers (including the call's own timeout) keep running."
  (let ((outcome nil))
    (agent-repl-connect-unary
     conn method json-string
     :timeout timeout
     :on-response (lambda (alist) (setq outcome (cons :response alist)))
     :on-failure (lambda (detail) (setq outcome (cons :failure detail))))
    (let ((deadline (+ (float-time)
                       (* 2 (or timeout agent-repl-connect-unary-timeout-seconds))
                       1)))
      (while (and (null outcome) (< (float-time) deadline))
        (accept-process-output nil 0.05)))
    (cond
     ((null outcome)
      (agent-repl--error nil "elisp.connect.unary-sync-abandoned method=%S" method)
      (signal 'agent-repl-connect-error
              (list (agent-repl-connect--failure
                     :timeout (format "%s: no answer before the sync deadline" method)))))
     ((eq (car outcome) :response) (cdr outcome))
     (t (signal 'agent-repl-connect-error (list (cdr outcome)))))))

;;;; ---- Server streaming ----

(cl-defstruct (agent-repl-connect-stream
               (:constructor agent-repl-connect-stream-create)
               (:copier nil))
  "One standing server-streaming Connect call.
PROCESS is the curl child, METHOD the bare rpc name, CONN the connection
it stands on.  CANCELLED-P records a client-side cancel so the exit is
reported as `(:cancelled)' and not as a failure.  CLOSE-REASON holds the
outcome decided by a terminal envelope, so the sentinel reports what the
producer said rather than re-deriving it.  CLOSED-P makes ON-CLOSE run
exactly once, and OPENED-P makes ON-OPEN run exactly once."
  (process nil)
  (method nil :type string)
  (conn nil)
  (on-push nil)
  (on-close nil)
  (on-open nil)
  (opened-p nil)
  (cancelled-p nil)
  (close-reason nil)
  (closed-p nil))

(defun agent-repl-connect--end-frame-reason (method payload)
  "Return the ON-CLOSE outcome carried by terminal PAYLOAD of METHOD.
`{}' is `(:ended)'; an `error' object is `(:error DETAIL)'.  A payload
that will not parse is itself a transport failure — a producer that ends a
stream unintelligibly has not ended it cleanly."
  (condition-case err
      (let* ((parsed (if (string-empty-p (string-trim payload))
                         nil
                       (agent-repl-connect--parse-json payload)))
             (error-object (alist-get 'error parsed)))
        (if (null error-object)
            (list :ended)
          (list :error
                (agent-repl-connect--failure
                 :http (or (alist-get 'message error-object)
                           (alist-get 'code error-object)
                           "stream ended with an error")
                 :code (alist-get 'code error-object)))))
    (error
     (list :error
           (agent-repl-connect--failure
            :malformed
            (format "%s: unparsable end frame (%S)" method err))))))

(defun agent-repl-connect--dispatch-push (stream payload)
  "Parse PAYLOAD and hand it to STREAM's ON-PUSH, containing any error.
A push whose JSON will not parse is a transport-level breach: logged at
ERROR with the raw payload and dropped.  An ON-PUSH that itself signals is
caught HERE, at the filter boundary: the exception is logged at ERROR with
the payload in context and THE STREAM STAYS OPEN, because one bad reaction
must not tear down a standing subscription."
  (let ((method (agent-repl-connect-stream-method stream)))
    (condition-case err
        (let ((parsed (agent-repl-connect--parse-json payload)))
          (condition-case push-err
              (funcall (agent-repl-connect-stream-on-push stream) parsed)
            (error
             (agent-repl--error nil "elisp.connect.push-handler-error method=%S error=%S payload=%S"
                                method push-err payload))))
      (error
       (agent-repl--error nil "elisp.connect.push-unparsable method=%S error=%S payload=%S"
                          method err payload)))))

(defun agent-repl-connect--dispatch-open (stream)
  "Run STREAM's ON-OPEN once, the instant its HTTP 200 headers landed.
ACCEPTANCE IS THE HEADER BLOCK.  The daemon flushes response headers on
accept, so a 200 status is proof the subscription stands — before any
frame, and long before the first push a standing stream may never send.
A non-200 status and a death before any header block never reach here, so
a caller that keys its subscribed record on this is never told a refused
stream was accepted.  An ON-OPEN that itself signals is contained here, like
every other consumer callback at this boundary: it is logged at ERROR and
the stream stays open."
  (unless (agent-repl-connect-stream-opened-p stream)
    (setf (agent-repl-connect-stream-opened-p stream) t)
    (let ((method (agent-repl-connect-stream-method stream)))
      (agent-repl--info nil "elisp.connect.stream-accepted method=%S" method)
      (when (agent-repl-connect-stream-on-open stream)
        (condition-case err
            (funcall (agent-repl-connect-stream-on-open stream))
          (error
           (agent-repl--error nil "elisp.connect.open-handler-error method=%S error=%S"
                              method err)))))))

(defun agent-repl-connect--close-stream (stream outcome)
  "Run STREAM's ON-CLOSE with OUTCOME once, and forget the stream.
OUTCOME is `(:cancelled)', `(:ended)', or `(:error DETAIL)'."
  (unless (agent-repl-connect-stream-closed-p stream)
    (setf (agent-repl-connect-stream-closed-p stream) t)
    (let ((conn (agent-repl-connect-stream-conn stream))
          (method (agent-repl-connect-stream-method stream)))
      (when conn
        (setf (agent-repl-connect-connection-streams conn)
              (delq stream (agent-repl-connect-connection-streams conn))))
      (pcase (car outcome)
        (:cancelled (agent-repl--info nil "elisp.connect.stream-cancelled method=%S" method))
        (:ended (agent-repl--info nil "elisp.connect.stream-ended method=%S" method))
        (:error (agent-repl--error nil "elisp.connect.stream-error method=%S detail=%S"
                                   method (cadr outcome))))
      (when (agent-repl-connect-stream-on-close stream)
        (condition-case err
            (funcall (agent-repl-connect-stream-on-close stream) outcome)
          (error
           (agent-repl--error nil "elisp.connect.close-handler-error method=%S error=%S outcome=%S"
                              method err outcome)))))))

(defun agent-repl-connect-stream (conn method json-string on-push on-close
                                      &optional on-open)
  "Open server-streaming METHOD on CONN with request JSON-STRING.
ON-OPEN, when given, is called with no arguments exactly once, the moment
the response header block parses as HTTP 200 — the instant the stream was
ACCEPTED, before any frame.  It never runs for a non-200 status and never
for a transport death that happened before any headers arrived.
ON-PUSH receives each pushed message as a parsed alist.  ON-CLOSE receives
`(:cancelled)' when the client cancelled, `(:ended)' when the producer
sent a clean terminal envelope, or `(:error DETAIL)' when the terminal
envelope carried an error, the HTTP exchange failed, or the producer died
without a terminal envelope at all.  A standing stream's `(:ended)' is a
failure to ITS caller; the transport reports what happened and judges
nothing.  Returns the stream object."
  (agent-repl-connect--check-alive conn method)
  (let* ((address (agent-repl-connect-connection-address conn))
         (body-file (agent-repl-connect--write-body-file
                     (agent-repl-connect-envelope-encode 0 json-string)))
         (stderr-buffer (generate-new-buffer
                         (format " *agent-repl-connect-stderr %s*" method)))
         (reader (agent-repl-connect--reader-create))
         (parser (agent-repl-connect-envelope-parser-create))
         (error-body "")
         (stream (agent-repl-connect-stream-create
                  :method method :conn conn :on-push on-push :on-close on-close
                  :on-open on-open)))
    (agent-repl--info nil "elisp.connect.stream-open method=%S address=%S" method address)
    (setf (agent-repl-connect-stream-process stream)
          (agent-repl-connect--spawn-curl
           (format "agent-repl-connect-stream-%s" method)
           (agent-repl-connect--curl-argv
            address method agent-repl-connect--stream-content-type body-file)
           (lambda (_proc chunk)
             (let ((bytes (agent-repl-connect--reader-feed reader chunk))
                   (status (agent-repl-connect--reader-status reader)))
               (cond
                ((null status) nil)
                ((/= status 200)
                 (setq error-body (concat error-body bytes)))
                (t
                 (agent-repl-connect--dispatch-open stream)
                 (dolist (frame (agent-repl-connect-envelope-feed parser bytes))
                   (if (agent-repl-connect-envelope-end-frame-p (car frame))
                       (unless (agent-repl-connect-stream-close-reason stream)
                         (setf (agent-repl-connect-stream-close-reason stream)
                               (agent-repl-connect--end-frame-reason method (cdr frame)))
                         (let ((proc (agent-repl-connect-stream-process stream)))
                           (when (process-live-p proc) (delete-process proc))))
                     (agent-repl-connect--dispatch-push stream (cdr frame))))))))
           (lambda (proc _event)
             (unless (process-live-p proc)
               (let ((status (agent-repl-connect--reader-status reader))
                     (stderr (agent-repl-connect--stderr-text stderr-buffer))
                     (reason (agent-repl-connect-stream-close-reason stream)))
                 (agent-repl-connect--cleanup body-file stderr-buffer)
                 (agent-repl-connect--close-stream
                  stream
                  (cond
                   (reason reason)
                   ((agent-repl-connect-stream-cancelled-p stream) (list :cancelled))
                   ((null status)
                    (list :error
                          (agent-repl-connect--failure
                           :transport
                           (if (string-empty-p stderr)
                               (format "%s: no HTTP response from %s" method address)
                             stderr))))
                   ((/= status 200)
                    (list :error
                          (agent-repl-connect--connect-error-detail status error-body)))
                   (t
                    (list :error
                          (agent-repl-connect--failure
                           :no-end-frame
                           (format "%s: producer closed without an end frame" method)))))))))
           stderr-buffer))
    (push stream (agent-repl-connect-connection-streams conn))
    stream))

(defun agent-repl-connect-stream-cancel (stream)
  "Cancel STREAM.  Cancelling IS the graceful close of a Connect stream.
Kills the curl child; the sentinel then runs ON-CLOSE with `(:cancelled)'.
Never an error, and idempotent — a stream already closed is left alone."
  (unless (agent-repl-connect-stream-closed-p stream)
    (setf (agent-repl-connect-stream-cancelled-p stream) t)
    (agent-repl--log nil "elisp.connect.stream-cancel method=%S"
                     (agent-repl-connect-stream-method stream))
    (let ((proc (agent-repl-connect-stream-process stream)))
      (if (process-live-p proc)
          (delete-process proc)
        (agent-repl-connect--close-stream stream (list :cancelled))))))

(provide 'connect)

;;; connect.el ends here
