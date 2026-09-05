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
;; THE SOCKET, NOT A CHILD PROCESS.  Every exchange here rides a native
;; `make-network-process' to the daemon's loopback listener.  It used to
;; spawn a `curl' child per call, which cost ~14ms of process setup on
;; every daemon round trip — user-visible latency in the editor, and the
;; floor under every Emacs integration suite.  A loopback socket costs
;; about a millisecond, pulls one more external binary out of the
;; dependency set, and puts the HTTP/1.1 response framing under this
;; file's own tests rather than under curl's behavior.
;;
;; HTTP RESPONSE READING.  ONE reader state machine
;; (`agent-repl-connect--reader') consumes the response header block, then
;; decodes the body under whichever framing the server chose — a
;; `Content-Length', `Transfer-Encoding: chunked', or a body delimited by
;; the close.  curl used to do that decoding on the transport's behalf; it
;; is production code now, and the test harness's own control plane shares
;; it rather than carrying a second copy that can drift.  The same reader
;; serves unary and streaming, so there is one status path rather than
;; two, and a stream knows its status the moment the headers land.
;;
;; Every request asks for `Connection: close', so an answer is always
;; terminated by an end-of-file the sentinel sees, whatever framing the
;; server picked.
;;
;; CLOSING, EXACTLY ONCE, IN ONE OF THREE WAYS.  A terminal envelope runs
;; ON-CLOSE from the FILTER, the instant the producer said how the stream
;; ended, and closes the socket afterwards — a consumer is never made to
;; wait on a socket teardown for a fact it has already been sent.  A
;; client cancel deletes the socket and the sentinel reports
;; `(:cancelled)'.  A death with no terminal envelope is reported by the
;; sentinel as an error.  CLOSED-P makes the three mutually exclusive and
;; each of them singular, and a frame arriving after the end frame is
;; logged at WARNING, never delivered.
;;
;; STANDING-STREAM ACCEPTANCE.  The daemon flushes its response headers on
;; accept, so the HTTP 200 header block IS the acceptance of a
;; subscription — before any frame, and long before a first push that a
;; standing stream may never send at all.  `agent-repl-connect-stream'
;; exposes that instant as its optional ON-OPEN, and daemon-link, host and
;; roster key "subscribed" on it rather than on a spawn or a first frame.
;;
;; EVERY socket in this file is opened through the single boundary wrapper
;; `agent-repl-connect--open-socket', registered in
;; `agent-repl--external-boundary-functions' (core.el) so batch tests are
;; guarded against reaching a real listener.  The request bytes it is
;; handed are built by the pure `agent-repl-connect--request-bytes'.

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
answered with a non-200 and a Connect error body), `:transport' (the
socket could not carry the exchange through), `:timeout', `:malformed'
\(a 200 whose body is not the JSON it must be), `:malformed-addr', or
`:no-end-frame'
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
  "State machine consuming one HTTP/1.1 response off the wire.
PHASE is `:headers' until the blank line separating the header block from
the body has been seen, `:body' while the body is arriving, and `:done'
once the body is whole.  PENDING holds the bytes received that do not yet
form anything deliverable.  STATUS is the parsed HTTP status integer, nil
while it is still unknown.

FRAMING is how the server said the body ends — `:length' (a
`Content-Length'), `:chunked' (a `Transfer-Encoding: chunked'), or
`:until-close' (neither, so the close IS the end).  REMAINING counts the
`:length' bytes still owed.  COMPLETE-P is set the moment the body is
whole, which is what lets a caller answer without waiting on the socket
teardown.  BREACH holds the text of a framing violation — a chunk header
that is not a hexadecimal length — so the exchange fails loudly with its
own reason instead of decoding garbage."
  (phase :headers)
  (pending "")
  (status nil)
  (framing nil)
  (remaining nil)
  (complete-p nil)
  (breach nil))

(defun agent-repl-connect--parse-status-line (block)
  "Return the HTTP status integer of header BLOCK, or nil when unparsable."
  (when (string-match "\\`HTTP/[0-9.]+ +\\([0-9]\\{3\\}\\)" block)
    (string-to-number (match-string 1 block))))

(defun agent-repl-connect--reader-read-head (reader block)
  "Record the status line and the body framing named by header BLOCK.
`Transfer-Encoding: chunked' outranks a `Content-Length', per RFC 9112 —
a server that sends both is chunking.  Neither header means the body is
delimited by the close, which is what a `Connection: close' answer with
no length looks like."
  (setf (agent-repl-connect--reader-status reader)
        (agent-repl-connect--parse-status-line block))
  (cond
   ((string-match-p "^[Tt]ransfer-[Ee]ncoding:[ \t]*chunked" block)
    (setf (agent-repl-connect--reader-framing reader) :chunked))
   ((string-match "^[Cc]ontent-[Ll]ength:[ \t]*\\([0-9]+\\)" block)
    (setf (agent-repl-connect--reader-framing reader) :length
          (agent-repl-connect--reader-remaining reader)
          (string-to-number (match-string 1 block)))
    (when (zerop (agent-repl-connect--reader-remaining reader))
      (setf (agent-repl-connect--reader-complete-p reader) t
            (agent-repl-connect--reader-phase reader) :done)))
   (t (setf (agent-repl-connect--reader-framing reader) :until-close))))

(defun agent-repl-connect--reader-dechunk (reader)
  "Consume every whole chunk in READER's pending bytes and return their payload.
A partial chunk stays pending for the next feed.  The terminal `0' chunk
ends the body; its trailers, which this protocol never sends, are
discarded with it.  A chunk header that is not a hexadecimal length is
recorded as a BREACH rather than guessed at."
  (let ((out "")
        (done nil))
    (while (not done)
      (let* ((pending (agent-repl-connect--reader-pending reader))
             (eol (string-search "\r\n" pending)))
        (if (null eol)
            (setq done t)
          (let* ((header (substring pending 0 eol))
                 (semicolon (string-search ";" header))
                 (size-text (if semicolon (substring header 0 semicolon) header)))
            (cond
             ((not (string-match-p "\\`[0-9A-Fa-f]+\\'" size-text))
              (setf (agent-repl-connect--reader-breach reader)
                    (format "unreadable chunk header %S" size-text)
                    (agent-repl-connect--reader-phase reader) :done
                    (agent-repl-connect--reader-pending reader) "")
              (setq done t))
             ((zerop (string-to-number size-text 16))
              (setf (agent-repl-connect--reader-pending reader) ""
                    (agent-repl-connect--reader-complete-p reader) t
                    (agent-repl-connect--reader-phase reader) :done)
              (setq done t))
             (t
              (let ((size (string-to-number size-text 16)))
                (if (< (length pending) (+ eol 2 size 2))
                    (setq done t)
                  (setq out (concat out (substring pending (+ eol 2) (+ eol 2 size))))
                  (setf (agent-repl-connect--reader-pending reader)
                        (substring pending (+ eol 2 size 2)))))))))))
    out))

(defun agent-repl-connect--reader-decode (reader)
  "Return the body bytes READER's framing lets it release from PENDING now."
  (pcase (agent-repl-connect--reader-framing reader)
    (:chunked (agent-repl-connect--reader-dechunk reader))
    (:length
     (let* ((pending (agent-repl-connect--reader-pending reader))
            (take (min (length pending) (agent-repl-connect--reader-remaining reader)))
            (out (substring pending 0 take)))
       (setf (agent-repl-connect--reader-pending reader) (substring pending take)
             (agent-repl-connect--reader-remaining reader)
             (- (agent-repl-connect--reader-remaining reader) take))
       (when (zerop (agent-repl-connect--reader-remaining reader))
         (setf (agent-repl-connect--reader-complete-p reader) t
               (agent-repl-connect--reader-phase reader) :done))
       out))
    (_
     (let ((out (agent-repl-connect--reader-pending reader)))
       (setf (agent-repl-connect--reader-pending reader) "")
       out))))

(defun agent-repl-connect--reader-truncated-p (reader)
  "Return non-nil when READER's body was cut short by an end-of-file.
Only a body whose framing DECLARED its length can be found short: a body
delimited by the close ends exactly when the close arrives."
  (and (memq (agent-repl-connect--reader-framing reader) '(:length :chunked))
       (not (agent-repl-connect--reader-complete-p reader))))

(defun agent-repl-connect--reader-feed (reader chunk)
  "Feed CHUNK into READER and return the BODY bytes it newly exposes.
While the header block is still arriving this returns the empty string and
records nothing but progress; the call that completes the block reads the
status line and the framing, and returns whatever body bytes the framing
lets it release from the same chunk.  Bytes arriving after a whole body
are not part of it and are dropped."
  (pcase (agent-repl-connect--reader-phase reader)
    (:done "")
    (:body
     (setf (agent-repl-connect--reader-pending reader)
           (concat (agent-repl-connect--reader-pending reader) chunk))
     (agent-repl-connect--reader-decode reader))
    (_
     (let ((pending (concat (agent-repl-connect--reader-pending reader) chunk)))
       (setf (agent-repl-connect--reader-pending reader) pending)
       (let ((separator (cond ((string-match "\r\n\r\n" pending) (match-end 0))
                              ((string-match "\n\n" pending) (match-end 0)))))
         (if (not separator)
             ""
           (let ((block (substring pending 0 separator)))
             (setf (agent-repl-connect--reader-pending reader)
                   (substring pending separator)
                   (agent-repl-connect--reader-phase reader) :body)
             (agent-repl-connect--reader-read-head reader block)
             (if (eq (agent-repl-connect--reader-phase reader) :done)
                 ""
               (agent-repl-connect--reader-decode reader)))))))))

;;;; ---- The request (pure) and the ONE socket boundary ----

(defun agent-repl-connect--method-path (method)
  "Return the request-target path of METHOD on the daemon."
  (concat agent-repl-connect--service-path method))

(defun agent-repl-connect--split-address (address)
  "Return the cons `(HOST . PORT)' of the `HOST:PORT' string ADDRESS.
A bracketed IPv6 literal loses its brackets, which is the spelling
`make-network-process' wants.  ADDRESS has already been validated by
`agent-repl-connect-open'; an unsplittable one signals rather than
dialing something else."
  (cond
   ((and (stringp address) (string-match "\\`\\[\\([^]]+\\)\\]:\\([0-9]+\\)\\'" address))
    (cons (match-string 1 address) (string-to-number (match-string 2 address))))
   ((and (stringp address) (string-match "\\`\\(.+\\):\\([0-9]+\\)\\'" address))
    (cons (match-string 1 address) (string-to-number (match-string 2 address))))
   (t
    (agent-repl--error nil "elisp.connect.unsplittable-address address=%S" address)
    (signal 'agent-repl-connect-error
            (list (agent-repl-connect--failure
                   :malformed-addr
                   (format "unsplittable daemon address: %S" address)))))))

(defun agent-repl-connect--request-bytes (address method content-type body)
  "Return the whole HTTP/1.1 POST of BODY to METHOD at ADDRESS as unibyte bytes.
CONTENT-TYPE selects unary protojson or the streaming envelope framing.
BODY is ALREADY ENCODED — UTF-8 for a unary protojson body, the framed
envelope for a stream — and its byte length is what `Content-Length'
carries, so no request size or byte value can be mangled.  Pure: it
composes bytes and touches nothing.

`Connection: close' is asked for on every exchange so an answer always
ends in an end-of-file the sentinel sees, whichever body framing the
server picks."
  (concat
   (encode-coding-string
    (concat "POST " (agent-repl-connect--method-path method) " HTTP/1.1\r\n"
            "Host: " address "\r\n"
            "Content-Type: " content-type "\r\n"
            "Connect-Protocol-Version: 1\r\n"
            "Content-Length: " (number-to-string (length body)) "\r\n"
            "Connection: close\r\n"
            "\r\n")
    'utf-8 t)
   body))

(defun agent-repl-connect--transport-message (method address event)
  "Return the text of a socket that ended METHOD without an HTTP answer.
EVENT is the process sentinel's own event string — `failed with code 61
\(Connection refused)' for a connection that never landed, `connection
broken by remote peer' for a peer that hung up before answering.  It is
the socket's counterpart of curl's stderr and rides along verbatim, so a
refused daemon still names its own reason rather than being flattened
into a generic silence."
  (let ((detail (string-trim (or event ""))))
    (if (string-empty-p detail)
        (format "%s: no HTTP response from %s" method address)
      (format "%s: no HTTP response from %s (%s)" method address detail))))

(defun agent-repl-connect--close-socket (process)
  "Delete PROCESS when it is still live, tolerating one already gone."
  (when (process-live-p process)
    (delete-process process)))

(defun agent-repl-connect--open-socket (name host port request filter sentinel)
  "Open a socket to HOST:PORT as process NAME and send REQUEST on it.
FILTER receives raw response bytes and SENTINEL every status change.
Returns the process, or nil when the connect never happened at all.

This IS the one external-boundary wrapper of connect.el: every Connect
exchange, unary or streaming, dials here and nowhere else.  Registered in
`agent-repl--external-boundary-functions' (core.el) so the batch
harness's guard fails any test that reaches a real listener.

THE CONNECT IS BLOCKING, AND THAT IS THE POINT.  The peer is the daemon's
own loopback listener: connecting to it costs a fraction of a
millisecond, and it either lands or is refused on the spot -- there is no
network in between to stall on.  What a non-blocking connect costs
instead is not latency but RE-ENTRANCY, and that is a correctness
problem, not a tuning one.  A `:nowait' dial has to wait for the
connection somewhere before it can write, and every way of waiting --
`accept-process-output', or the 20ms retry Emacs itself does when the
write gets EAGAIN -- runs the event loop.  These dials happen INSIDE
PROCESS FILTERS: daemon-link attaches a successor from the `WatchDaemon'
filter.  Running the event loop from inside a filter cost the handover
its `transferred' pushes outright -- the outgoing daemon pushed them, the
sockets were open, and Emacs never delivered a byte of them, so no
workspace was ever adopted and the successor was never promoted.

THE REQUEST IS WRITTEN IN CALLER ORDER, before this returns, so two
exchanges started one after the other reach the daemon in that order --
which is what a prompt queue replaying its held prompts oldest first
depends on.

A connect the kernel refuses, and a write that cannot be issued, are NOT
swallowed: each is logged at ERROR here and handed to SENTINEL as the
death it is.

THEY ARE HANDED OVER ASYNCHRONOUSLY, THOUGH, and deliberately.  A caller
told its exchange had already failed before this function returned would
be answered before the stream it is opening exists to be closed, and out
of the middle of its own constructor.  A death is delivered on the next
turn of the event loop instead, exactly where every other death arrives
from."
  (let ((process nil)
        (refusal nil))
    (condition-case err
        (setq process
              (make-network-process ;; ALLOW-EXTERNAL-BOUNDARY
               :name name
               :host host
               :service port
               :coding 'binary
               :noquery t
               :filter filter
               :sentinel #'ignore))
      (error
       (setq refusal (error-message-string err))
       (agent-repl--error nil "elisp.connect.dial-failed name=%S host=%S port=%S error=%S"
                          name host port err)))
    (when process
      (condition-case err
          (process-send-string process request)
        (error
         (setq refusal (error-message-string err))
         (agent-repl--error nil "elisp.connect.request-send-failed name=%S error=%S"
                            name err)
         (agent-repl-connect--close-socket process))))
    (if (process-live-p process)
        (set-process-sentinel process sentinel)
      (let ((event (or refusal (format "%s" (and process (process-status process))))))
        (run-at-time 0 nil (lambda () (funcall sentinel process event)))))
    process))

(defun agent-repl-connect--detach-sentinel (process)
  "Detach PROCESS's sentinel so a teardown cannot re-deliver into it.

CALLED FIRST BY EVERY TERMINAL SENTINEL BRANCH HERE, and that ordering is
the whole point.  A terminal branch that tears its exchange down can move
the very process whose sentinel is running RIGHT NOW, and `status_notify'
then runs that sentinel again: the branch sees a dead process a second
time, tears down a second time, and recurses — an unbounded stack with no
Lisp error and no log record, because the recursion happens before the
branch reaches any logging.  That is the `SPC .' hang.

Detaching before the teardown makes the re-entry UNREPRESENTABLE rather
than merely unlikely: the re-delivered status reaches `ignore'.  It is
also correct on its own terms — a terminal branch has, by definition,
nothing left to hear from its process."
  (when (processp process)
    (set-process-sentinel process #'ignore)))


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
idempotent.  Returns the socket process."
  (agent-repl-connect--check-alive conn method)
  (let* ((address (agent-repl-connect-connection-address conn))
         (host-port (agent-repl-connect--split-address address))
         (request (agent-repl-connect--request-bytes
                   address method agent-repl-connect--unary-content-type
                   (encode-coding-string json-string 'utf-8 t)))
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
            (agent-repl-connect--open-socket
             (format "agent-repl-connect-%s" method)
             (car host-port) (cdr host-port) request
             (lambda (proc chunk)
               (setq body (concat body (agent-repl-connect--reader-feed reader chunk)))
               ;; A WHOLE BODY IS THE ANSWER.  The framing said how long the
               ;; body is, so the exchange need not wait on the peer's close
               ;; to hand it over; a framing breach ends it here just as
               ;; promptly, and the sentinel below names it.
               (when (or (agent-repl-connect--reader-breach reader)
                         (agent-repl-connect--reader-complete-p reader))
                 (agent-repl-connect--close-socket proc)))
             (lambda (proc event)
               (unless (process-live-p proc)
                 (agent-repl-connect--detach-sentinel proc)
                 (let ((status (agent-repl-connect--reader-status reader))
                       (breach (agent-repl-connect--reader-breach reader)))
                   (cond
                    (timed-out
                     (settle :failure
                             (agent-repl-connect--failure
                              :timeout
                              (format "%s timed out after %ss" method
                                      (or timeout agent-repl-connect-unary-timeout-seconds)))))
                    (breach
                     (settle :failure
                             (agent-repl-connect--failure
                              :transport (format "%s: %s" method breach)
                              :status status)))
                    ((null status)
                     (settle :failure
                             (agent-repl-connect--failure
                              :transport
                              (agent-repl-connect--transport-message method address event))))
                    ;; A DECLARED LENGTH THE PEER DID NOT DELIVER IS A DROP,
                    ;; not a body.  Parsing what arrived would turn a severed
                    ;; connection into an unrelated `malformed' complaint
                    ;; about JSON, and a truncation that happened to parse
                    ;; into a silent partial answer.
                    ((agent-repl-connect--reader-truncated-p reader)
                     (settle :failure
                             (agent-repl-connect--failure
                              :transport
                              (format "%s: connection dropped before the body completed (%s)"
                                      method (string-trim (or event "")))
                              :status status)))
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
                             (agent-repl-connect--connect-error-detail status body)))))))))
      ;; A REFUSED CONNECT CAN ANSWER BEFORE THE DIAL RETURNS -- the kernel
      ;; refuses on the spot and the sentinel runs from inside the write.  An
      ;; exchange already settled must not arm a timer nothing will cancel.
      (unless settled
        (setq timer
              (run-at-time (or timeout agent-repl-connect-unary-timeout-seconds) nil
                           (lambda ()
                             (when (process-live-p process)
                               (setq timed-out t)
                               (agent-repl--warn nil "elisp.connect.unary-timeout method=%S address=%S"
                                                 method address)
                               (delete-process process))))))
      process)))

(defun agent-repl-connect-unary-sync (conn method json-string &optional timeout)
  "Call METHOD on CONN with JSON-STRING and block for the answer.
Returns the parsed response alist, or signals `agent-repl-connect-error'
with the failure plist as its data.  Waits on process output rather than
sleeping, so timers (including the call's own timeout) keep running.

The wait samples at 2ms.  `accept-process-output' returns the instant any
process delivers, so the interval is only the floor under an answer that
arrives while nothing else is talking; at the 50ms it used to be, a
loopback round trip that takes about a millisecond still cost a whole
slot, and every synchronous call in the editor paid it."
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
        (accept-process-output nil 0.002)))
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
PROCESS is the socket, METHOD the bare rpc name, CONN the connection
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
         (host-port (agent-repl-connect--split-address address))
         (request (agent-repl-connect--request-bytes
                   address method agent-repl-connect--stream-content-type
                   (agent-repl-connect-envelope-encode 0 json-string)))
         (reader (agent-repl-connect--reader-create))
         (parser (agent-repl-connect-envelope-parser-create))
         (error-body "")
         (stream (agent-repl-connect-stream-create
                  :method method :conn conn :on-push on-push :on-close on-close
                  :on-open on-open)))
    (agent-repl--info nil "elisp.connect.stream-open method=%S address=%S" method address)
    ;; REGISTERED BEFORE IT IS DIALED.  A refused connect answers from inside
    ;; the dial, so a stream registered afterwards would be pushed onto the
    ;; connection by a call whose own close had already taken it off.
    (push stream (agent-repl-connect-connection-streams conn))
    (setf (agent-repl-connect-stream-process stream)
          (agent-repl-connect--open-socket
           (format "agent-repl-connect-stream-%s" method)
           (car host-port) (cdr host-port) request
           (lambda (proc chunk)
             (let ((bytes (agent-repl-connect--reader-feed reader chunk))
                   (status (agent-repl-connect--reader-status reader)))
               (cond
                ;; A FRAMING BREACH IS NOT A STREAM.  Decoding past an
                ;; unreadable chunk header would invent frames; the socket
                ;; goes and the sentinel names the breach.
                ((agent-repl-connect--reader-breach reader)
                 (agent-repl-connect--close-socket proc))
                ((null status) nil)
                ((/= status 200)
                 (setq error-body (concat error-body bytes))
                 (when (agent-repl-connect--reader-complete-p reader)
                   (agent-repl-connect--close-socket proc)))
                (t
                 (agent-repl-connect--dispatch-open stream)
                 (dolist (frame (agent-repl-connect-envelope-feed parser bytes))
                   (cond
                    ;; THE END FRAME IS THE END.  Anything after it is the
                    ;; producer contradicting itself, never a push to deliver.
                    ((agent-repl-connect-stream-closed-p stream)
                     (agent-repl--warn nil "elisp.connect.frame-after-end method=%S flags=%S"
                                       method (car frame)))
                    ((agent-repl-connect-envelope-end-frame-p (car frame))
                     ;; ON-CLOSE runs HERE, from the frame that ended the
                     ;; stream, and not from the sentinel: the producer has
                     ;; already said what happened, so a consumer must not
                     ;; wait on a socket teardown to be told.  The socket is
                     ;; closed straight after; the sentinel then finds the
                     ;; stream closed and reports nothing again.
                     (let ((reason (agent-repl-connect--end-frame-reason
                                    method (cdr frame))))
                       (setf (agent-repl-connect-stream-close-reason stream) reason)
                       (agent-repl-connect--close-stream stream reason)
                       (agent-repl-connect--close-socket
                        (agent-repl-connect-stream-process stream))))
                    (t
                     (agent-repl-connect--dispatch-push stream (cdr frame)))))
                 ;; A body the framing declared WHOLE ends the response even
                 ;; when no end frame came with it; the sentinel below calls
                 ;; that what it is.
                 (when (agent-repl-connect--reader-complete-p reader)
                   (agent-repl-connect--close-socket proc))))))
           (lambda (proc event)
             (unless (process-live-p proc)
               (agent-repl-connect--detach-sentinel proc)
               (let ((status (agent-repl-connect--reader-status reader))
                     (breach (agent-repl-connect--reader-breach reader))
                     (reason (agent-repl-connect-stream-close-reason stream)))
                 (agent-repl-connect--close-stream
                  stream
                  (cond
                   (reason reason)
                   ((agent-repl-connect-stream-cancelled-p stream) (list :cancelled))
                   (breach
                    (list :error
                          (agent-repl-connect--failure
                           :transport (format "%s: %s" method breach)
                           :status status)))
                   ((null status)
                    (list :error
                          (agent-repl-connect--failure
                           :transport
                           (agent-repl-connect--transport-message method address event))))
                   ((/= status 200)
                    (list :error
                          (agent-repl-connect--connect-error-detail status error-body)))
                   (t
                    (list :error
                          (agent-repl-connect--failure
                           :no-end-frame
                           (format "%s: producer closed without an end frame" method)))))))))))
    stream))

(defun agent-repl-connect-stream-cancel (stream)
  "Cancel STREAM.  Cancelling IS the graceful close of a Connect stream.
Closes the socket; the sentinel then runs ON-CLOSE with `(:cancelled)'.
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
