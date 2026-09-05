;;; test-connect.el --- ERT tests for agent-repl connect.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;   AGENT_REPL_FORBID_VENDOR_CALLS=1 emacs -batch -Q -l ert \
;;     -l lisp/test-connect.el -f ert-run-tests-batch-and-exit
;;
;; NO SOCKET IS EVER OPENED HERE.  `agent-repl-connect--open-socket' is
;; connect.el's ONE external boundary and is registered in
;; `agent-repl--external-boundary-functions', so the harness's guard already
;; fails any test that reaches it unstubbed.  Every test below stubs it with
;; `agent-repl-test-connect--dial-stub', which hands back a real but
;; CONNECTION-LESS `make-pipe-process' object — process-live-p,
;; delete-process and the sentinel all behave as production expects, while
;; nothing is dialed — and records the request bytes, the filter and the
;; sentinel so a test can drive the exchange byte by byte and
;; deterministically, with no sleeps and no scheduler dependence.
;;
;; The real loopback round trip belongs to the integration suite, not here.

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                           (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Harness ----

(defvar agent-repl-test-connect--spawned nil
  "List of dial records, newest first, captured by the dial stub.
Each is a plist `(:name :host :port :request :filter :sentinel :process)'.")

(defvar agent-repl-test-connect--logs nil
  "List of `(LEVEL . TEXT)' entries, newest first, captured from the ladder.")

(defvar agent-repl-test-connect--script nil
  "Canned response bytes the dial stub replays inline, or nil to stay silent.
When non-nil the stub feeds this string to the filter and then runs the
sentinel BEFORE returning, which is what makes the synchronous entry
points testable without waiting on anything.")

(defun agent-repl-test-connect--record-log (level fmt args)
  "Push a `(LEVEL . TEXT)' entry built from FMT and ARGS onto the log capture."
  (push (cons level (condition-case nil (apply #'format fmt args)
                      (error (format "%S %S" fmt args))))
        agent-repl-test-connect--logs))

(defun agent-repl-test-connect--logs-matching (level regexp)
  "Return every captured LEVEL entry whose text matches REGEXP."
  (seq-filter (lambda (entry)
                (and (eq (car entry) level)
                     (string-match-p regexp (cdr entry))))
              agent-repl-test-connect--logs))

(defun agent-repl-test-connect--dial-stub (name host port request filter sentinel)
  "Stand in for `agent-repl-connect--open-socket' without dialing anything.
Creates a pipe process (a real process object bound to no connection),
records the exchange, and — when `agent-repl-test-connect--script' is set
— replays those response bytes and closes inline."
  ;; The pipe process carries INERT callbacks: the production filter and
  ;; sentinel are kept in the record and driven by the test directly, so the
  ;; whole exchange is synchronous.  Attaching the real ones would let Emacs
  ;; re-run the sentinel asynchronously after the test's `cl-letf' unwound,
  ;; firing the production `agent-repl--error' from outside the test.
  (let* ((process (make-pipe-process :name (concat "test-" name)
                                     :noquery t
                                     :coding 'no-conversion
                                     :filter #'ignore
                                     :sentinel #'ignore))
         (record (list :name name :host host :port port :request request
                       :filter filter :sentinel sentinel :process process)))
    (push record agent-repl-test-connect--spawned)
    (when agent-repl-test-connect--script
      (funcall filter process agent-repl-test-connect--script)
      (delete-process process)
      (funcall sentinel process "connection broken by remote peer\n"))
    process))

(defun agent-repl-test-connect--last-spawn ()
  "Return the most recent dial record."
  (car agent-repl-test-connect--spawned))

(defun agent-repl-test-connect--feed (record chunk)
  "Hand CHUNK to RECORD's process filter, as the socket's bytes would."
  (funcall (plist-get record :filter) (plist-get record :process) chunk))

(defun agent-repl-test-connect--exit (record &optional event)
  "End RECORD's process and run its sentinel with EVENT.
EVENT defaults to the peer hanging up, which is what a `Connection: close'
answer ends with; a test that means a connect refusal or a mid-body drop
passes the event string the real socket would carry."
  (let ((process (plist-get record :process)))
    (when (process-live-p process)
      (delete-process process))
    (funcall (plist-get record :sentinel)
             process (or event "connection broken by remote peer\n"))))

(defun agent-repl-test-connect--cleanup ()
  "Delete every process the dial stub created during a test."
  (dolist (record agent-repl-test-connect--spawned)
    (let ((process (plist-get record :process)))
      (when (process-live-p process)
        (delete-process process))))
  (setq agent-repl-test-connect--spawned nil))

(defmacro agent-repl-test-connect--with-transport (&rest body)
  "Run BODY with the spawn boundary stubbed and the logging ladder captured.
The ladder is captured rather than exercised because a test asserting a
transport branch must not also depend on the log sink, and because
`agent-repl--error' signals in production — a branch under test would
abort before its own assertions ran."
  (declare (indent 0))
  `(let ((agent-repl-test-connect--spawned nil)
         (agent-repl-test-connect--logs nil)
         (agent-repl-test-connect--script nil))
     (unwind-protect
         (cl-letf (((symbol-function 'agent-repl-connect--open-socket)
                    #'agent-repl-test-connect--dial-stub)
                   ((symbol-function 'agent-repl--log)
                    (lambda (_ws fmt &rest args)
                      (agent-repl-test-connect--record-log 'debug fmt args)))
                   ((symbol-function 'agent-repl--info)
                    (lambda (_ws fmt &rest args)
                      (agent-repl-test-connect--record-log 'info fmt args)))
                   ((symbol-function 'agent-repl--warn)
                    (lambda (_ws fmt &rest args)
                      (agent-repl-test-connect--record-log 'warn fmt args)))
                   ((symbol-function 'agent-repl--error)
                    (lambda (_ws fmt &rest args)
                      (agent-repl-test-connect--record-log 'error fmt args))))
           ,@body)
       (agent-repl-test-connect--cleanup))))

(defun agent-repl-test-connect--http (status body &optional extra-headers)
  "Return response bytes: STATUS line, EXTRA-HEADERS, then BODY.
No `Content-Length' and no `Transfer-Encoding', so the body is delimited
by the close — the framing a Connect stream's flushed answer uses, and
the one that lets a test hand over bytes chunk by chunk."
  (concat (format "HTTP/1.1 %d %s\r\n" status (if (= status 200) "OK" "Error"))
          "Content-Type: application/json\r\n"
          (or extra-headers "")
          "\r\n"
          body))

(defun agent-repl-test-connect--conn ()
  "Return a connection object for a fixed loopback address."
  (agent-repl-connect-open "127.0.0.1:41234"))

;;;; ---- Tests: envelope encoding ----

(ert-deftest agent-repl-test-connect-envelope-encode-frames-flag-length-and-payload ()
  "A message envelope is the flag byte, a 4-byte big-endian length, the payload."
  ;; Arrange
  (let ((payload "{\"a\":1}"))
    ;; Act
    (let ((envelope (agent-repl-connect-envelope-encode 0 payload)))
      ;; Assert
      (should (equal (append (substring envelope 0 5) nil) '(0 0 0 0 7)))
      (should (equal (substring envelope 5) payload)))))

(ert-deftest agent-repl-test-connect-envelope-encode-length-counts-utf-8-bytes ()
  "A multibyte payload's length field counts encoded BYTES, not characters."
  ;; Arrange
  (let ((payload "{\"n\":\"é\"}"))
    ;; Act
    (let ((envelope (agent-repl-connect-envelope-encode 0 payload)))
      ;; Assert
      (should (= (agent-repl-connect--envelope-length envelope)
                 (string-bytes payload)))
      (should (> (agent-repl-connect--envelope-length envelope)
                 (length payload))))))

(ert-deftest agent-repl-test-connect-envelope-encode-writes-the-end-stream-flag ()
  "The terminal frame's flag byte carries bit 0x02."
  ;; Arrange / Act
  (let ((envelope (agent-repl-connect-envelope-encode #x02 "{}")))
    ;; Assert
    (should (agent-repl-connect-envelope-end-frame-p (aref envelope 0)))))

(ert-deftest agent-repl-test-connect-envelope-message-flag-is-not-an-end-frame ()
  "A flag byte of 0 does not mark the terminal frame."
  ;; Arrange / Act / Assert
  (should-not (agent-repl-connect-envelope-end-frame-p 0)))

;;;; ---- Tests: envelope parsing ----

(ert-deftest agent-repl-test-connect-envelope-feed-yields-a-whole-frame ()
  "One complete envelope in one chunk yields one frame."
  ;; Arrange
  (let ((parser (agent-repl-connect-envelope-parser-create))
        (bytes (agent-repl-connect-envelope-encode 0 "{\"x\":1}")))
    ;; Act
    (let ((frames (agent-repl-connect-envelope-feed parser bytes)))
      ;; Assert
      (should (equal frames '((0 . "{\"x\":1}")))))))

(ert-deftest agent-repl-test-connect-envelope-feed-reassembles-a-split-frame ()
  "A frame split across two chunks is delivered whole on the second chunk."
  ;; Arrange
  (let* ((parser (agent-repl-connect-envelope-parser-create))
         (bytes (agent-repl-connect-envelope-encode 0 "{\"x\":1}"))
         (cut 8))
    ;; Act
    (let ((first (agent-repl-connect-envelope-feed parser (substring bytes 0 cut)))
          (second (agent-repl-connect-envelope-feed parser (substring bytes cut))))
      ;; Assert
      (should (null first))
      (should (equal second '((0 . "{\"x\":1}")))))))

(ert-deftest agent-repl-test-connect-envelope-feed-reassembles-a-split-length-prefix ()
  "A chunk cutting through the 4-byte length prefix holds nothing back wrongly."
  ;; Arrange
  (let* ((parser (agent-repl-connect-envelope-parser-create))
         (bytes (agent-repl-connect-envelope-encode 0 "{\"x\":1}")))
    ;; Act
    (let ((first (agent-repl-connect-envelope-feed parser (substring bytes 0 3)))
          (second (agent-repl-connect-envelope-feed parser (substring bytes 3))))
      ;; Assert
      (should (null first))
      (should (equal second '((0 . "{\"x\":1}")))))))

(ert-deftest agent-repl-test-connect-envelope-feed-splits-merged-frames ()
  "Several envelopes arriving in one chunk come back as separate frames in order."
  ;; Arrange
  (let ((parser (agent-repl-connect-envelope-parser-create))
        (bytes (concat (agent-repl-connect-envelope-encode 0 "{\"n\":1}")
                       (agent-repl-connect-envelope-encode 0 "{\"n\":2}")
                       (agent-repl-connect-envelope-encode #x02 "{}"))))
    ;; Act
    (let ((frames (agent-repl-connect-envelope-feed parser bytes)))
      ;; Assert
      (should (equal frames '((0 . "{\"n\":1}") (0 . "{\"n\":2}") (2 . "{}")))))))

(ert-deftest agent-repl-test-connect-envelope-feed-retains-a-trailing-partial-frame ()
  "A whole frame followed by a partial one yields the whole frame only."
  ;; Arrange
  (let* ((parser (agent-repl-connect-envelope-parser-create))
         (whole (agent-repl-connect-envelope-encode 0 "{\"n\":1}"))
         (partial (substring (agent-repl-connect-envelope-encode 0 "{\"n\":2}") 0 6)))
    ;; Act
    (let ((frames (agent-repl-connect-envelope-feed parser (concat whole partial))))
      ;; Assert
      (should (equal frames '((0 . "{\"n\":1}"))))
      (should (= (length (agent-repl-connect-envelope-parser-buffer parser))
                 (length partial))))))

(ert-deftest agent-repl-test-connect-envelope-feed-decodes-utf-8-payloads ()
  "A multibyte payload survives the byte-length framing intact."
  ;; Arrange
  (let ((parser (agent-repl-connect-envelope-parser-create))
        (payload "{\"n\":\"héllo\"}"))
    ;; Act
    (let ((frames (agent-repl-connect-envelope-feed
                   parser (agent-repl-connect-envelope-encode 0 payload))))
      ;; Assert
      (should (equal frames (list (cons 0 payload)))))))

;;;; ---- Tests: the HTTP response reader ----

(ert-deftest agent-repl-test-connect-reader-parses-status-and-returns-the-body ()
  "A header block and body in one chunk yield the status and the body bytes."
  ;; Arrange
  (let ((reader (agent-repl-connect--reader-create)))
    ;; Act
    (let ((body (agent-repl-connect--reader-feed
                 reader (agent-repl-test-connect--http 200 "{\"ok\":true}"))))
      ;; Assert
      (should (= (agent-repl-connect--reader-status reader) 200))
      (should (equal body "{\"ok\":true}")))))

(ert-deftest agent-repl-test-connect-reader-holds-the-body-until-headers-complete ()
  "A header block split across chunks yields no body until the blank line lands."
  ;; Arrange
  (let ((reader (agent-repl-connect--reader-create)))
    ;; Act
    (let ((first (agent-repl-connect--reader-feed reader "HTTP/1.1 200 OK\r\nContent-"))
          (second (agent-repl-connect--reader-feed reader "Type: application/json\r\n\r\nBODY")))
      ;; Assert
      (should (equal first ""))
      (should (= (agent-repl-connect--reader-status reader) 200))
      (should (equal second "BODY")))))

(ert-deftest agent-repl-test-connect-reader-parses-a-non-200-status ()
  "A Connect failure's HTTP status is read exactly as the success path's is."
  ;; Arrange
  (let ((reader (agent-repl-connect--reader-create)))
    ;; Act
    (agent-repl-connect--reader-feed reader (agent-repl-test-connect--http 503 "{}"))
    ;; Assert
    (should (= (agent-repl-connect--reader-status reader) 503))))

(ert-deftest agent-repl-test-connect-reader-accepts-a-bare-lf-header-terminator ()
  "A header block terminated by LF LF rather than CRLF CRLF still parses."
  ;; Arrange
  (let ((reader (agent-repl-connect--reader-create)))
    ;; Act
    (let ((body (agent-repl-connect--reader-feed reader "HTTP/1.1 200 OK\nX: y\n\nBODY")))
      ;; Assert
      (should (= (agent-repl-connect--reader-status reader) 200))
      (should (equal body "BODY")))))

(ert-deftest agent-repl-test-connect-reader-passes-post-header-chunks-through ()
  "Once the header block is consumed, later chunks are body bytes verbatim."
  ;; Arrange
  (let ((reader (agent-repl-connect--reader-create)))
    (agent-repl-connect--reader-feed reader (agent-repl-test-connect--http 200 ""))
    ;; Act
    (let ((body (agent-repl-connect--reader-feed reader "\x00\x01raw")))
      ;; Assert
      (should (equal body "\x00\x01raw")))))

(ert-deftest agent-repl-test-connect-reader-reports-no-status-for-an-unparsable-block ()
  "A header block with no recognizable status line leaves the status unknown."
  ;; Arrange
  (let ((reader (agent-repl-connect--reader-create)))
    ;; Act
    (agent-repl-connect--reader-feed reader "not an http answer at all\r\n\r\n")
    ;; Assert
    (should (null (agent-repl-connect--reader-status reader)))))

(ert-deftest agent-repl-test-connect-reader-completes-a-content-length-body ()
  "A `Content-Length\' body is whole the moment that many bytes have arrived."
  ;; Arrange
  (let ((reader (agent-repl-connect--reader-create)))
    ;; Act
    (let ((body (agent-repl-connect--reader-feed
                 reader "HTTP/1.1 200 OK\r\nContent-Length: 2\r\n\r\n{}")))
      ;; Assert
      (should (equal body "{}"))
      (should (agent-repl-connect--reader-complete-p reader)))))

(ert-deftest agent-repl-test-connect-reader-holds-a-short-content-length-body-open ()
  "A `Content-Length\' body that is still arriving is not yet complete."
  ;; Arrange
  (let ((reader (agent-repl-connect--reader-create)))
    ;; Act
    (agent-repl-connect--reader-feed
     reader "HTTP/1.1 200 OK\r\nContent-Length: 8\r\n\r\n{}")
    ;; Assert
    (should-not (agent-repl-connect--reader-complete-p reader))
    (should (agent-repl-connect--reader-truncated-p reader))))

(ert-deftest agent-repl-test-connect-reader-stops-at-the-declared-length ()
  "Bytes past a `Content-Length\' belong to no body and are not returned."
  ;; Arrange
  (let ((reader (agent-repl-connect--reader-create)))
    ;; Act
    (let ((body (agent-repl-connect--reader-feed
                 reader "HTTP/1.1 200 OK\r\nContent-Length: 2\r\n\r\n{}trailing")))
      ;; Assert
      (should (equal body "{}")))))

(ert-deftest agent-repl-test-connect-reader-completes-an-empty-content-length-body ()
  "A `Content-Length: 0\' answer is whole with no body bytes at all."
  ;; Arrange
  (let ((reader (agent-repl-connect--reader-create)))
    ;; Act
    (let ((body (agent-repl-connect--reader-feed
                 reader "HTTP/1.1 204 No Content\r\nContent-Length: 0\r\n\r\n")))
      ;; Assert
      (should (equal body ""))
      (should (agent-repl-connect--reader-complete-p reader)))))

(ert-deftest agent-repl-test-connect-reader-dechunks-a-chunked-body ()
  "A `Transfer-Encoding: chunked\' body comes back with its framing removed."
  ;; Arrange
  (let ((reader (agent-repl-connect--reader-create)))
    ;; Act
    (let ((body (agent-repl-connect--reader-feed
                 reader (concat "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n"
                                "4\r\n{\"ok\r\n" "4\r\n\":1}\r\n" "0\r\n\r\n"))))
      ;; Assert
      (should (equal body "{\"ok\":1}"))
      (should (agent-repl-connect--reader-complete-p reader)))))

(ert-deftest agent-repl-test-connect-reader-holds-a-partial-chunk ()
  "A chunk still arriving is held back until all of it has landed."
  ;; Arrange
  (let ((reader (agent-repl-connect--reader-create)))
    (agent-repl-connect--reader-feed
     reader "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n")
    ;; Act
    (let ((first (agent-repl-connect--reader-feed reader "8\r\n{\"ok"))
          (second (agent-repl-connect--reader-feed reader "\":1}\r\n")))
      ;; Assert
      (should (equal first ""))
      (should (equal second "{\"ok\":1}")))))

(ert-deftest agent-repl-test-connect-reader-ignores-a-chunk-extension ()
  "A chunk header\'s extension after `;\' is not part of its length."
  ;; Arrange
  (let ((reader (agent-repl-connect--reader-create)))
    (agent-repl-connect--reader-feed
     reader "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n")
    ;; Act
    (let ((body (agent-repl-connect--reader-feed reader "2;x=y\r\n{}\r\n")))
      ;; Assert
      (should (equal body "{}")))))

(ert-deftest agent-repl-test-connect-reader-records-an-unreadable-chunk-header ()
  "A chunk header that is not a hexadecimal length is a breach, never a guess."
  ;; Arrange
  (let ((reader (agent-repl-connect--reader-create)))
    (agent-repl-connect--reader-feed
     reader "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n")
    ;; Act
    (agent-repl-connect--reader-feed reader "zz\r\nnope\r\n")
    ;; Assert
    (should (string-match-p "unreadable chunk header"
                            (agent-repl-connect--reader-breach reader)))))

(ert-deftest agent-repl-test-connect-reader-chunked-outranks-a-content-length ()
  "A server sending both headers is chunking, per RFC 9112."
  ;; Arrange
  (let ((reader (agent-repl-connect--reader-create)))
    ;; Act
    (agent-repl-connect--reader-feed
     reader (concat "HTTP/1.1 200 OK\r\nContent-Length: 99\r\n"
                    "Transfer-Encoding: chunked\r\n\r\n"))
    ;; Assert
    (should (eq (agent-repl-connect--reader-framing reader) :chunked))))

(ert-deftest agent-repl-test-connect-reader-close-delimited-body-is-never-truncated ()
  "A body the close delimits ends when the close does, so it is never short."
  ;; Arrange
  (let ((reader (agent-repl-connect--reader-create)))
    ;; Act
    (agent-repl-connect--reader-feed reader (agent-repl-test-connect--http 200 "BODY"))
    ;; Assert
    (should (eq (agent-repl-connect--reader-framing reader) :until-close))
    (should-not (agent-repl-connect--reader-truncated-p reader))))

;;;; ---- Tests: daemon.addr discovery ----

(defmacro agent-repl-test-connect--with-addr-file (contents &rest body)
  "Run BODY with the state dir's `daemon.addr' holding CONTENTS.
A CONTENTS of nil means the file is absent — the legal no-daemon state."
  (declare (indent 1))
  `(let ((file (agent-repl-connect-daemon-addr-file)))
     (make-directory (file-name-directory file) t)
     (unwind-protect
         (progn
           (if ,contents
               (with-temp-file file (insert ,contents))
             (when (file-exists-p file) (delete-file file)))
           ,@body)
       (when (file-exists-p file) (delete-file file)))))

(ert-deftest agent-repl-test-connect-daemon-addr-file-lives-under-the-state-dir ()
  "The address file is `daemon.addr' under agent-repl's one state root."
  ;; Arrange / Act
  (let ((file (agent-repl-connect-daemon-addr-file)))
    ;; Assert
    (should (equal (file-name-nondirectory file) "daemon.addr"))
    (should (equal (file-name-as-directory (file-name-directory file))
                   (agent-repl--global-state-dir)))))

(ert-deftest agent-repl-test-connect-read-daemon-addr-returns-the-address ()
  "A well-formed address file yields its `HOST:PORT' string."
  ;; Arrange
  (agent-repl-test-connect--with-transport
    (agent-repl-test-connect--with-addr-file "127.0.0.1:41234\n"
      ;; Act / Assert
      (should (equal (agent-repl-connect-read-daemon-addr) "127.0.0.1:41234")))))

(ert-deftest agent-repl-test-connect-read-daemon-addr-answers-nil-when-absent ()
  "An absent address file is the legal no-daemon state, not a failure."
  ;; Arrange
  (agent-repl-test-connect--with-transport
    (agent-repl-test-connect--with-addr-file nil
      ;; Act / Assert
      (should (null (agent-repl-connect-read-daemon-addr))))))

(ert-deftest agent-repl-test-connect-read-daemon-addr-tolerates-no-trailing-newline ()
  "The address is trimmed, so a file written without its newline still parses."
  ;; Arrange
  (agent-repl-test-connect--with-transport
    (agent-repl-test-connect--with-addr-file "127.0.0.1:9"
      ;; Act / Assert
      (should (equal (agent-repl-connect-read-daemon-addr) "127.0.0.1:9")))))

(ert-deftest agent-repl-test-connect-read-daemon-addr-signals-on-malformed-content ()
  "Malformed address content is a loud misconfiguration, never a silent nil."
  ;; Arrange
  (agent-repl-test-connect--with-transport
    (agent-repl-test-connect--with-addr-file "not-an-address\n"
      ;; Act / Assert
      (should-error (agent-repl-connect-read-daemon-addr)
                    :type 'agent-repl-connect-error))))

(ert-deftest agent-repl-test-connect-read-daemon-addr-logs-error-on-malformed-content ()
  "The malformed-address branch reaches the ERROR rung before it signals."
  ;; Arrange
  (agent-repl-test-connect--with-transport
    (agent-repl-test-connect--with-addr-file "127.0.0.1\n"
      ;; Act
      (ignore-errors (agent-repl-connect-read-daemon-addr))
      ;; Assert
      (should (agent-repl-test-connect--logs-matching
               'error "elisp\\.connect\\.daemon-addr-malformed")))))

;;;; ---- Tests: address splitting ----

(ert-deftest agent-repl-test-connect-split-address-separates-host-and-port ()
  "A `HOST:PORT' string splits into the host and the port integer."
  ;; Arrange / Act / Assert
  (should (equal (agent-repl-connect--split-address "127.0.0.1:41234")
                 (cons "127.0.0.1" 41234))))

(ert-deftest agent-repl-test-connect-split-address-unwraps-an-ipv6-literal ()
  "A bracketed IPv6 literal loses its brackets, which is what the socket wants."
  ;; Arrange / Act / Assert
  (should (equal (agent-repl-connect--split-address "[::1]:9")
                 (cons "::1" 9))))

(ert-deftest agent-repl-test-connect-split-address-signals-on-a-portless-address ()
  "An address with no port signals rather than dialing something else."
  ;; Arrange
  (agent-repl-test-connect--with-transport
    ;; Act / Assert
    (should-error (agent-repl-connect--split-address "127.0.0.1")
                  :type 'agent-repl-connect-error)))

;;;; ---- Tests: request composition ----

(ert-deftest agent-repl-test-connect-request-targets-the-connect-method-path ()
  "The request line targets the service path plus the bare method name."
  ;; Arrange / Act
  (let ((request (agent-repl-connect--request-bytes
                  "127.0.0.1:41234" "RegisterWorkspace" "application/json" "{}")))
    ;; Assert
    (should (string-prefix-p
             "POST /agentrepl.v1.AgentRepl/RegisterWorkspace HTTP/1.1\r\n" request))))

(ert-deftest agent-repl-test-connect-request-names-the-daemon-as-its-host ()
  "The `Host' header is the address dialed, as HTTP/1.1 requires."
  ;; Arrange / Act
  (let ((request (agent-repl-connect--request-bytes
                  "127.0.0.1:41234" "DaemonHealth" "application/json" "{}")))
    ;; Assert
    (should (string-match-p "\r\nHost: 127\\.0\\.0\\.1:41234\r\n" request))))

(ert-deftest agent-repl-test-connect-request-sends-the-connect-protocol-version ()
  "Every Connect request carries `Connect-Protocol-Version: 1'."
  ;; Arrange / Act
  (let ((request (agent-repl-connect--request-bytes
                  "127.0.0.1:1" "DaemonHealth" "application/json" "{}")))
    ;; Assert
    (should (string-match-p "\r\nConnect-Protocol-Version: 1\r\n" request))))

(ert-deftest agent-repl-test-connect-request-carries-the-requested-content-type ()
  "The content type is the caller's, so unary and streaming differ only there."
  ;; Arrange / Act
  (let ((unary (agent-repl-connect--request-bytes
                "127.0.0.1:1" "DaemonHealth" "application/json" "{}"))
        (stream (agent-repl-connect--request-bytes
                 "127.0.0.1:1" "WatchDaemon" "application/connect+json" "{}")))
    ;; Assert
    (should (string-match-p "\r\nContent-Type: application/json\r\n" unary))
    (should (string-match-p "\r\nContent-Type: application/connect\\+json\r\n" stream))))

(ert-deftest agent-repl-test-connect-request-asks-the-daemon-to-close ()
  "`Connection: close' is asked for, so every answer ends in an end-of-file."
  ;; Arrange / Act
  (let ((request (agent-repl-connect--request-bytes
                  "127.0.0.1:1" "DaemonHealth" "application/json" "{}")))
    ;; Assert
    (should (string-match-p "\r\nConnection: close\r\n" request))))

(ert-deftest agent-repl-test-connect-request-body-follows-the-blank-line ()
  "The body rides the request verbatim, after the header block\'s blank line."
  ;; Arrange / Act
  (let* ((request (agent-repl-connect--request-bytes
                   "127.0.0.1:1" "SubmitPrompt" "application/json" "{\"a\":1}"))
         (split (string-search "\r\n\r\n" request)))
    ;; Assert
    (should (equal (substring request (+ split 4)) "{\"a\":1}"))))

(ert-deftest agent-repl-test-connect-request-content-length-counts-bytes ()
  "`Content-Length' counts the body\'s BYTES, never its characters."
  ;; Arrange
  (let ((body (encode-coding-string "{\"n\":\"é\"}" 'utf-8 t)))
    ;; Act
    (let ((request (agent-repl-connect--request-bytes
                    "127.0.0.1:1" "SubmitPrompt" "application/json" body)))
      ;; Assert
      (should (string-match "\r\nContent-Length: \\([0-9]+\\)\r\n" request))
      (should (= (string-to-number (match-string 1 request)) (length body))))))


;;;; ---- Tests: connection lifecycle ----

(ert-deftest agent-repl-test-connect-open-records-the-address ()
  "Opening a connection records the address it will dial."
  ;; Arrange
  (agent-repl-test-connect--with-transport
    ;; Act
    (let ((conn (agent-repl-test-connect--conn)))
      ;; Assert
      (should (equal (agent-repl-connect-connection-address conn) "127.0.0.1:41234"))
      (should (agent-repl-connect-connection-alive-p conn)))))

(ert-deftest agent-repl-test-connect-open-signals-on-a-malformed-address ()
  "An address that is not `HOST:PORT' is refused before anything is spawned."
  ;; Arrange
  (agent-repl-test-connect--with-transport
    ;; Act / Assert
    (should-error (agent-repl-connect-open "localhost") :type 'agent-repl-connect-error)))

(ert-deftest agent-repl-test-connect-unary-on-a-closed-connection-signals ()
  "A call on a closed connection fails immediately rather than dialing."
  ;; Arrange
  (agent-repl-test-connect--with-transport
    (let ((conn (agent-repl-test-connect--conn)))
      (agent-repl-connect-close conn)
      ;; Act / Assert
      (should-error (agent-repl-connect-unary conn "DaemonHealth" "{}")
                    :type 'agent-repl-connect-error)
      (should (null agent-repl-test-connect--spawned)))))

;;;; ---- Tests: unary ----

(ert-deftest agent-repl-test-connect-unary-parses-a-200-body ()
  "HTTP 200 hands the parsed protojson alist to ON-RESPONSE."
  ;; Arrange
  (agent-repl-test-connect--with-transport
    (let ((conn (agent-repl-test-connect--conn))
          (answer :none))
      (agent-repl-connect-unary conn "RegisterWorkspace" "{\"dir\":\"/w\"}"
                                :on-response (lambda (alist) (setq answer alist))
                                :on-failure (lambda (d) (setq answer (list :failed d))))
      ;; Act
      (let ((record (agent-repl-test-connect--last-spawn)))
        (agent-repl-test-connect--feed
         record (agent-repl-test-connect--http 200 "{\"success\":{\"workspace\":{\"id\":\"w1\"}}}"))
        (agent-repl-test-connect--exit record))
      ;; Assert
      (should (equal answer '((success . ((workspace . ((id . "w1")))))))))))

(ert-deftest agent-repl-test-connect-unary-reports-a-connect-error-body ()
  "A non-200 answer's Connect error `code' and `message' reach ON-FAILURE."
  ;; Arrange
  (agent-repl-test-connect--with-transport
    (let ((conn (agent-repl-test-connect--conn))
          (failure nil))
      (agent-repl-connect-unary conn "SelectWorkspace" "{}"
                                :on-response (lambda (_a) (setq failure :unexpected-success))
                                :on-failure (lambda (d) (setq failure d)))
      ;; Act
      (let ((record (agent-repl-test-connect--last-spawn)))
        (agent-repl-test-connect--feed
         record (agent-repl-test-connect--http
                 404 "{\"code\":\"not_found\",\"message\":\"no such workspace\"}"))
        (agent-repl-test-connect--exit record))
      ;; Assert
      (should (eq (plist-get failure :kind) :http))
      (should (equal (plist-get failure :code) "not_found"))
      (should (equal (plist-get failure :message) "no such workspace"))
      (should (= (plist-get failure :status) 404)))))

(ert-deftest agent-repl-test-connect-unary-reports-a-non-json-error-body ()
  "A non-200 whose body is not a Connect error still fails loudly with its status."
  ;; Arrange
  (agent-repl-test-connect--with-transport
    (let ((conn (agent-repl-test-connect--conn))
          (failure nil))
      (agent-repl-connect-unary conn "DaemonHealth" "{}"
                                :on-failure (lambda (d) (setq failure d)))
      ;; Act
      (let ((record (agent-repl-test-connect--last-spawn)))
        (agent-repl-test-connect--feed record (agent-repl-test-connect--http 502 "bad gateway"))
        (agent-repl-test-connect--exit record))
      ;; Assert
      (should (eq (plist-get failure :kind) :http))
      (should (= (plist-get failure :status) 502))
      (should (string-match-p "bad gateway" (plist-get failure :message))))))

(ert-deftest agent-repl-test-connect-unary-reports-an-unparsable-200-body ()
  "A 200 whose body is not JSON is a malformed answer, never a silent success."
  ;; Arrange
  (agent-repl-test-connect--with-transport
    (let ((conn (agent-repl-test-connect--conn))
          (failure nil))
      (agent-repl-connect-unary conn "DaemonHealth" "{}"
                                :on-response (lambda (_a) (setq failure :unexpected-success))
                                :on-failure (lambda (d) (setq failure d)))
      ;; Act
      (let ((record (agent-repl-test-connect--last-spawn)))
        (agent-repl-test-connect--feed record (agent-repl-test-connect--http 200 "not json"))
        (agent-repl-test-connect--exit record))
      ;; Assert
      (should (eq (plist-get failure :kind) :malformed)))))

(ert-deftest agent-repl-test-connect-unary-reports-a-transport-failure ()
  "A close with no HTTP status at all is a transport failure."
  ;; Arrange
  (agent-repl-test-connect--with-transport
    (let ((conn (agent-repl-test-connect--conn))
          (failure nil))
      (agent-repl-connect-unary conn "DaemonHealth" "{}"
                                :on-failure (lambda (d) (setq failure d)))
      ;; Act
      (agent-repl-test-connect--exit (agent-repl-test-connect--last-spawn))
      ;; Assert
      (should (eq (plist-get failure :kind) :transport))
      (should (string-match-p "no HTTP response" (plist-get failure :message))))))

(ert-deftest agent-repl-test-connect-unary-names-a-refused-connection ()
  "A connection that never landed fails naming the socket\'s own reason.
`curl' put `Failed to connect\' on its stderr; the sentinel event carries
the same fact now, and it must reach the caller rather than being
flattened into a generic silence."
  ;; Arrange
  (agent-repl-test-connect--with-transport
    (let ((conn (agent-repl-test-connect--conn))
          (failure nil))
      (agent-repl-connect-unary conn "DaemonHealth" "{}"
                                :on-failure (lambda (d) (setq failure d)))
      ;; Act — no bytes ever arrive; the connect itself failed
      (agent-repl-test-connect--exit (agent-repl-test-connect--last-spawn)
                                     "failed with code 61\n")
      ;; Assert
      (should (eq (plist-get failure :kind) :transport))
      (should (string-match-p "failed with code 61" (plist-get failure :message))))))

(ert-deftest agent-repl-test-connect-unary-reports-a-drop-mid-body ()
  "A peer that hangs up inside a declared body fails as a DROP, not as JSON.
Parsing what arrived would turn a severed connection into an unrelated
complaint about the payload, and a truncation that happened to parse into
a silent partial answer."
  ;; Arrange
  (agent-repl-test-connect--with-transport
    (let ((conn (agent-repl-test-connect--conn))
          (failure nil))
      (agent-repl-connect-unary conn "DaemonHealth" "{}"
                                :on-response (lambda (_a) (setq failure :unexpected-success))
                                :on-failure (lambda (d) (setq failure d)))
      ;; Act
      (let ((record (agent-repl-test-connect--last-spawn)))
        (agent-repl-test-connect--feed
         record (concat "HTTP/1.1 200 OK\r\nContent-Length: 40\r\n\r\n{\"success\":"))
        (agent-repl-test-connect--exit record))
      ;; Assert
      (should (eq (plist-get failure :kind) :transport))
      (should (string-match-p "dropped before the body completed"
                              (plist-get failure :message))))))

(ert-deftest agent-repl-test-connect-unary-parses-a-body-arriving-in-chunks ()
  "A 200 whose body is spread over several reads is reassembled before parsing."
  ;; Arrange
  (agent-repl-test-connect--with-transport
    (let ((conn (agent-repl-test-connect--conn))
          (answer :none))
      (agent-repl-connect-unary conn "DaemonHealth" "{}"
                                :on-response (lambda (alist) (setq answer alist))
                                :on-failure (lambda (d) (setq answer (list :failed d))))
      ;; Act
      (let ((record (agent-repl-test-connect--last-spawn)))
        (agent-repl-test-connect--feed record "HTTP/1.1 200 OK\r\nContent-Length: 19\r\n\r\n")
        (agent-repl-test-connect--feed record "{\"success\":")
        (agent-repl-test-connect--feed record "{\"n\":1}}")
        (agent-repl-test-connect--exit record))
      ;; Assert
      (should (equal answer '((success . ((n . 1)))))))))

(ert-deftest agent-repl-test-connect-unary-answers-a-whole-framed-body-without-a-close ()
  "A `Content-Length\' body is answered the moment it is whole.
Waiting on the peer\'s close for a body the framing already declared
complete would put a round trip of latency on every editor command."
  ;; Arrange
  (agent-repl-test-connect--with-transport
    (let ((conn (agent-repl-test-connect--conn))
          (answer :none))
      (agent-repl-connect-unary conn "DaemonHealth" "{}"
                                :on-response (lambda (alist) (setq answer alist)))
      ;; Act — no `--exit'; only the bytes
      (let ((record (agent-repl-test-connect--last-spawn)))
        (agent-repl-test-connect--feed
         record "HTTP/1.1 200 OK\r\nContent-Length: 2\r\n\r\n{}")
        ;; Assert — the socket is gone, and only the sentinel is still owed
        (should-not (process-live-p (plist-get record :process)))
        (agent-repl-test-connect--exit record)
        (should (equal answer nil))))))

(ert-deftest agent-repl-test-connect-unary-reports-an-unreadable-chunk-header ()
  "A chunked answer whose framing will not parse fails loudly as transport.
Decoding past it would invent a body out of the framing bytes."
  ;; Arrange
  (agent-repl-test-connect--with-transport
    (let ((conn (agent-repl-test-connect--conn))
          (failure nil))
      (agent-repl-connect-unary conn "DaemonHealth" "{}"
                                :on-response (lambda (_a) (setq failure :unexpected-success))
                                :on-failure (lambda (d) (setq failure d)))
      ;; Act
      (let ((record (agent-repl-test-connect--last-spawn)))
        (agent-repl-test-connect--feed
         record (concat "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n"
                        "zz\r\nnope\r\n"))
        (agent-repl-test-connect--exit record))
      ;; Assert
      (should (eq (plist-get failure :kind) :transport))
      (should (string-match-p "unreadable chunk header" (plist-get failure :message))))))

(ert-deftest agent-repl-test-connect-unary-answers-exactly-once ()
  "A second sentinel run after settling does not re-answer the caller."
  ;; Arrange
  (agent-repl-test-connect--with-transport
    (let ((conn (agent-repl-test-connect--conn))
          (answers 0))
      (agent-repl-connect-unary conn "DaemonHealth" "{}"
                                :on-response (lambda (_a) (cl-incf answers))
                                :on-failure (lambda (_d) (cl-incf answers)))
      ;; Act
      (let ((record (agent-repl-test-connect--last-spawn)))
        (agent-repl-test-connect--feed record (agent-repl-test-connect--http 200 "{}"))
        (agent-repl-test-connect--exit record)
        (agent-repl-test-connect--exit record))
      ;; Assert
      (should (= answers 1)))))

(ert-deftest agent-repl-test-connect-unary-sends-the-json-as-its-request-body ()
  "The caller\'s JSON is what goes on the wire, byte for byte."
  ;; Arrange
  (agent-repl-test-connect--with-transport
    (let ((conn (agent-repl-test-connect--conn)))
      ;; Act
      (agent-repl-connect-unary conn "RegisterWorkspace" "{\"dir\":\"/w\"}")
      ;; Assert
      (let* ((request (plist-get (agent-repl-test-connect--last-spawn) :request))
             (split (string-search "\r\n\r\n" request)))
        (should (equal (substring request (+ split 4)) "{\"dir\":\"/w\"}"))))))

(ert-deftest agent-repl-test-connect-unary-dials-the-connection-address ()
  "The socket is opened on the host and port the connection was opened with."
  ;; Arrange
  (agent-repl-test-connect--with-transport
    (let ((conn (agent-repl-test-connect--conn)))
      ;; Act
      (agent-repl-connect-unary conn "DaemonHealth" "{}")
      ;; Assert
      (let ((record (agent-repl-test-connect--last-spawn)))
        (should (equal (plist-get record :host) "127.0.0.1"))
        (should (equal (plist-get record :port) 41234))))))

(ert-deftest agent-repl-test-connect-unary-leaves-no-socket-behind ()
  "The exchange\'s socket does not outlive the call that opened it."
  ;; Arrange
  (agent-repl-test-connect--with-transport
    (let ((conn (agent-repl-test-connect--conn)))
      (agent-repl-connect-unary conn "DaemonHealth" "{}")
      (let ((record (agent-repl-test-connect--last-spawn)))
        ;; Act
        (agent-repl-test-connect--feed record (agent-repl-test-connect--http 200 "{}"))
        (agent-repl-test-connect--exit record)
        ;; Assert
        (should-not (process-live-p (plist-get record :process)))))))

(ert-deftest agent-repl-test-connect-unary-sync-returns-the-parsed-response ()
  "The blocking form answers the parsed alist of a 200."
  ;; Arrange
  (agent-repl-test-connect--with-transport
    (let ((agent-repl-test-connect--script
           (agent-repl-test-connect--http 200 "{\"success\":{}}"))
          (conn (agent-repl-test-connect--conn)))
      ;; Act
      (let ((response (agent-repl-connect-unary-sync conn "DaemonHealth" "{}")))
        ;; Assert
        (should (equal response '((success . nil))))))))

(ert-deftest agent-repl-test-connect-unary-sync-signals-the-failure-detail ()
  "The blocking form raises the failure plist rather than returning it."
  ;; Arrange
  (agent-repl-test-connect--with-transport
    (let ((agent-repl-test-connect--script
           (agent-repl-test-connect--http 503 "{\"code\":\"unavailable\",\"message\":\"down\"}"))
          (conn (agent-repl-test-connect--conn)))
      ;; Act
      (let ((data (should-error (agent-repl-connect-unary-sync conn "DaemonHealth" "{}")
                                :type 'agent-repl-connect-error)))
        ;; Assert
        (should (equal (plist-get (cadr data) :code) "unavailable"))))))

;;;; ---- Tests: streaming ----

(defun agent-repl-test-connect--open-stream (conn pushes closes)
  "Open a test stream on CONN recording pushes into PUSHES and closes into CLOSES.
PUSHES and CLOSES are symbols of special variables the callbacks push onto."
  (agent-repl-connect-stream
   conn "WatchDaemon" "{}"
   (lambda (alist) (push alist (symbol-value pushes)))
   (lambda (outcome) (push outcome (symbol-value closes)))))

(defvar agent-repl-test-connect--pushes nil "Pushes captured by a stream test.")
(defvar agent-repl-test-connect--closes nil "Close outcomes captured by a stream test.")

(defmacro agent-repl-test-connect--with-stream (&rest body)
  "Run BODY with CONN, STREAM and RECORD bound to a freshly opened test stream.
The stream's headers are already consumed with a 200, so BODY starts where
frames begin."
  (declare (indent 0))
  `(agent-repl-test-connect--with-transport
     (let* ((agent-repl-test-connect--pushes nil)
            (agent-repl-test-connect--closes nil)
            (conn (agent-repl-test-connect--conn))
            (stream (agent-repl-test-connect--open-stream
                     conn 'agent-repl-test-connect--pushes 'agent-repl-test-connect--closes))
            (record (agent-repl-test-connect--last-spawn)))
       (ignore stream)
       (agent-repl-test-connect--feed record (agent-repl-test-connect--http 200 ""))
       ,@body)))

(ert-deftest agent-repl-test-connect-stream-request-body-is-one-envelope ()
  "A streaming request body is exactly one message envelope."
  ;; Arrange
  (agent-repl-test-connect--with-transport
    (let ((conn (agent-repl-test-connect--conn)))
      (agent-repl-connect-stream conn "WatchDaemon" "{}" #'ignore #'ignore)
      ;; Act
      (let* ((request (plist-get (agent-repl-test-connect--last-spawn) :request))
             (staged (substring request (+ (string-search "\r\n\r\n" request) 4))))
        ;; Assert
        (should (equal (append (substring staged 0 5) nil) '(0 0 0 0 2)))
        (should (equal (substring staged 5) "{}"))))))

(ert-deftest agent-repl-test-connect-stream-delivers-each-push ()
  "Every message frame reaches ON-PUSH parsed, in arrival order."
  ;; Arrange / Act
  (agent-repl-test-connect--with-stream
    (agent-repl-test-connect--feed
     record (concat (agent-repl-connect-envelope-encode 0 "{\"n\":1}")
                    (agent-repl-connect-envelope-encode 0 "{\"n\":2}")))
    ;; Assert
    (should (equal (nreverse agent-repl-test-connect--pushes)
                   '(((n . 1)) ((n . 2)))))))

(ert-deftest agent-repl-test-connect-stream-clean-end-frame-closes-as-ended ()
  "A terminal envelope carrying `{}' closes the stream as `(:ended)'."
  ;; Arrange / Act
  (agent-repl-test-connect--with-stream
    (agent-repl-test-connect--feed record (agent-repl-connect-envelope-encode #x02 "{}"))
    (agent-repl-test-connect--exit record)
    ;; Assert
    (should (equal agent-repl-test-connect--closes '((:ended))))))

(ert-deftest agent-repl-test-connect-stream-error-end-frame-closes-as-error ()
  "A terminal envelope carrying an error closes the stream as `(:error DETAIL)'."
  ;; Arrange / Act
  (agent-repl-test-connect--with-stream
    (agent-repl-test-connect--feed
     record (agent-repl-connect-envelope-encode
             #x02 "{\"error\":{\"code\":\"internal\",\"message\":\"boom\"}}"))
    (agent-repl-test-connect--exit record)
    ;; Assert
    (let ((outcome (car agent-repl-test-connect--closes)))
      (should (eq (car outcome) :error))
      (should (equal (plist-get (cadr outcome) :code) "internal"))
      (should (equal (plist-get (cadr outcome) :message) "boom")))))

(ert-deftest agent-repl-test-connect-stream-death-without-an-end-frame-is-an-error ()
  "A producer that closes without a terminal envelope is a transport failure."
  ;; Arrange / Act
  (agent-repl-test-connect--with-stream
    (agent-repl-test-connect--feed record (agent-repl-connect-envelope-encode 0 "{\"n\":1}"))
    (agent-repl-test-connect--exit record)
    ;; Assert
    (let ((outcome (car agent-repl-test-connect--closes)))
      (should (eq (car outcome) :error))
      (should (eq (plist-get (cadr outcome) :kind) :no-end-frame)))))

(ert-deftest agent-repl-test-connect-stream-death-without-an-end-frame-logs-error ()
  "The missing-end-frame branch reaches the ERROR rung."
  ;; Arrange / Act
  (agent-repl-test-connect--with-stream
    (agent-repl-test-connect--exit record)
    ;; Assert
    (should (agent-repl-test-connect--logs-matching 'error "elisp\\.connect\\.stream-error"))))

(ert-deftest agent-repl-test-connect-stream-cancel-closes-as-cancelled ()
  "A client cancel is `(:cancelled)' — the graceful close, never a failure."
  ;; Arrange / Act
  (agent-repl-test-connect--with-stream
    (agent-repl-connect-stream-cancel stream)
    (agent-repl-test-connect--exit record)
    ;; Assert
    (should (equal agent-repl-test-connect--closes '((:cancelled))))))

(ert-deftest agent-repl-test-connect-stream-cancel-logs-no-error ()
  "Cancelling records no ERROR: a client-side close is not a failure."
  ;; Arrange / Act
  (agent-repl-test-connect--with-stream
    (agent-repl-connect-stream-cancel stream)
    (agent-repl-test-connect--exit record)
    ;; Assert
    (should (null (agent-repl-test-connect--logs-matching 'error "elisp\\.connect")))))

(ert-deftest agent-repl-test-connect-stream-closes-exactly-once ()
  "An end frame followed by the process exit runs ON-CLOSE a single time."
  ;; Arrange / Act
  (agent-repl-test-connect--with-stream
    (agent-repl-test-connect--feed record (agent-repl-connect-envelope-encode #x02 "{}"))
    (agent-repl-test-connect--exit record)
    (agent-repl-test-connect--exit record)
    ;; Assert
    (should (= (length agent-repl-test-connect--closes) 1))))

(ert-deftest agent-repl-test-connect-stream-end-frame-closes-before-the-exit ()
  "ON-CLOSE runs from the END FRAME, not from the process exit.
The producer has already said how the stream ended; making a consumer
wait on a socket teardown for that fact is what left a real stream hanging."
  ;; Arrange / Act
  (agent-repl-test-connect--with-stream
    (agent-repl-test-connect--feed record (agent-repl-connect-envelope-encode #x02 "{}"))
    ;; Assert — no `--exit' has run yet
    (should (equal agent-repl-test-connect--closes '((:ended))))))

(ert-deftest agent-repl-test-connect-stream-end-frame-kills-the-process ()
  "The end frame is followed by closing the socket: nothing is left open."
  ;; Arrange / Act
  (agent-repl-test-connect--with-stream
    (agent-repl-test-connect--feed record (agent-repl-connect-envelope-encode #x02 "{}"))
    ;; Assert
    (should-not (process-live-p (plist-get record :process)))))

(ert-deftest agent-repl-test-connect-stream-error-end-frame-closes-before-the-exit ()
  "An end frame CARRYING an error closes on the frame too, not on the exit."
  ;; Arrange / Act
  (agent-repl-test-connect--with-stream
    (agent-repl-test-connect--feed
     record (agent-repl-connect-envelope-encode
             #x02 "{\"error\":{\"code\":\"unavailable\",\"message\":\"going away\"}}"))
    ;; Assert
    (should (eq (car (car agent-repl-test-connect--closes)) :error))))

(ert-deftest agent-repl-test-connect-stream-frame-after-the-end-is-not-delivered ()
  "The end frame is the end: a later message frame is never a push."
  ;; Arrange
  (agent-repl-test-connect--with-stream
    ;; Act
    (agent-repl-test-connect--feed
     record (concat (agent-repl-connect-envelope-encode #x02 "{}")
                    (agent-repl-connect-envelope-encode 0 "{\"n\":1}")))
    ;; Assert
    (should (null agent-repl-test-connect--pushes))))

(ert-deftest agent-repl-test-connect-stream-frame-after-the-end-is-logged ()
  "A producer contradicting its own end frame is named at WARNING."
  ;; Arrange
  (agent-repl-test-connect--with-stream
    ;; Act
    (agent-repl-test-connect--feed
     record (concat (agent-repl-connect-envelope-encode #x02 "{}")
                    (agent-repl-connect-envelope-encode 0 "{\"n\":1}")))
    ;; Assert
    (should (agent-repl-test-connect--logs-matching
             'warn "elisp\\.connect\\.frame-after-end"))))

(ert-deftest agent-repl-test-connect-stream-end-frame-then-exit-closes-once ()
  "The sentinel finds the stream already closed and reports nothing again."
  ;; Arrange
  (agent-repl-test-connect--with-stream
    ;; Act
    (agent-repl-test-connect--feed record (agent-repl-connect-envelope-encode #x02 "{}"))
    (agent-repl-test-connect--exit record)
    ;; Assert
    (should (= (length agent-repl-test-connect--closes) 1))))

(ert-deftest agent-repl-test-connect-stream-drop-mid-stream-is-an-error ()
  "A peer that hangs up mid-stream closes as an error, never as a clean end.
A standing subscription that vanished has not ENDED; the consumer must be
told the difference so it can reconnect."
  ;; Arrange
  (agent-repl-test-connect--with-stream
    ;; Act — one whole push, then the connection goes
    (agent-repl-test-connect--feed record (agent-repl-connect-envelope-encode 0 "{\"n\":1}"))
    (agent-repl-test-connect--exit record "connection broken by remote peer\n")
    ;; Assert
    (let ((outcome (car agent-repl-test-connect--closes)))
      (should (eq (car outcome) :error))
      (should (eq (plist-get (cadr outcome) :kind) :no-end-frame)))))

(ert-deftest agent-repl-test-connect-stream-refused-connection-closes-as-error ()
  "A stream whose connection never landed closes naming the socket\'s reason."
  ;; Arrange
  (agent-repl-test-connect--with-transport
    (let* ((agent-repl-test-connect--closes nil)
           (conn (agent-repl-test-connect--conn)))
      (agent-repl-test-connect--open-stream
       conn 'agent-repl-test-connect--pushes 'agent-repl-test-connect--closes)
      ;; Act
      (agent-repl-test-connect--exit (agent-repl-test-connect--last-spawn)
                                     "failed with code 61\n")
      ;; Assert
      (let ((outcome (car agent-repl-test-connect--closes)))
        (should (eq (car outcome) :error))
        (should (eq (plist-get (cadr outcome) :kind) :transport))
        (should (string-match-p "failed with code 61"
                                (plist-get (cadr outcome) :message)))))))

(ert-deftest agent-repl-test-connect-stream-reassembles-a-chunked-frame ()
  "A frame split across HTTP chunk boundaries is delivered once, whole."
  ;; Arrange
  (agent-repl-test-connect--with-transport
    (let* ((agent-repl-test-connect--pushes nil)
           (agent-repl-test-connect--closes nil)
           (conn (agent-repl-test-connect--conn))
           (envelope (agent-repl-connect-envelope-encode 0 "{\"n\":1}")))
      (agent-repl-test-connect--open-stream
       conn 'agent-repl-test-connect--pushes 'agent-repl-test-connect--closes)
      (let ((record (agent-repl-test-connect--last-spawn)))
        (agent-repl-test-connect--feed
         record "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n")
        ;; Act — the envelope arrives as two HTTP chunks
        (agent-repl-test-connect--feed
         record (format "%x\r\n%s\r\n" 6 (substring envelope 0 6)))
        (agent-repl-test-connect--feed
         record (format "%x\r\n%s\r\n" (- (length envelope) 6) (substring envelope 6))))
      ;; Assert
      (should (equal agent-repl-test-connect--pushes '(((n . 1))))))))

(ert-deftest agent-repl-test-connect-stream-contains-an-on-push-exception ()
  "An ON-PUSH that signals is contained: the stream stays open for the next push."
  ;; Arrange
  (agent-repl-test-connect--with-transport
    (let* ((conn (agent-repl-test-connect--conn))
           (seen nil)
           (closes nil)
           (stream (agent-repl-connect-stream
                    conn "WatchDaemon" "{}"
                    (lambda (alist)
                      (push alist seen)
                      (when (= 1 (alist-get 'n alist)) (error "handler exploded")))
                    (lambda (outcome) (push outcome closes))))
           (record (agent-repl-test-connect--last-spawn)))
      (agent-repl-test-connect--feed record (agent-repl-test-connect--http 200 ""))
      ;; Act
      (agent-repl-test-connect--feed record (agent-repl-connect-envelope-encode 0 "{\"n\":1}"))
      (agent-repl-test-connect--feed record (agent-repl-connect-envelope-encode 0 "{\"n\":2}"))
      ;; Assert
      (should (equal (nreverse seen) '(((n . 1)) ((n . 2)))))
      (should (null closes))
      (should-not (agent-repl-connect-stream-closed-p stream)))))

(ert-deftest agent-repl-test-connect-stream-logs-an-on-push-exception ()
  "A contained ON-PUSH exception is recorded at ERROR with its payload."
  ;; Arrange
  (agent-repl-test-connect--with-transport
    (let* ((conn (agent-repl-test-connect--conn)))
      (agent-repl-connect-stream conn "WatchDaemon" "{}"
                                 (lambda (_alist) (error "handler exploded"))
                                 #'ignore)
      (let ((record (agent-repl-test-connect--last-spawn)))
        (agent-repl-test-connect--feed record (agent-repl-test-connect--http 200 ""))
        ;; Act
        (agent-repl-test-connect--feed record (agent-repl-connect-envelope-encode 0 "{\"n\":1}"))
        ;; Assert
        (should (agent-repl-test-connect--logs-matching
                 'error "elisp\\.connect\\.push-handler-error"))))))

(ert-deftest agent-repl-test-connect-stream-drops-an-unparsable-push ()
  "A push whose JSON will not parse is logged and dropped; the stream continues."
  ;; Arrange / Act
  (agent-repl-test-connect--with-stream
    (agent-repl-test-connect--feed record (agent-repl-connect-envelope-encode 0 "not json"))
    (agent-repl-test-connect--feed record (agent-repl-connect-envelope-encode 0 "{\"n\":2}"))
    ;; Assert
    (should (equal agent-repl-test-connect--pushes '(((n . 2)))))
    (should (agent-repl-test-connect--logs-matching 'error "elisp\\.connect\\.push-unparsable"))
    (should (null agent-repl-test-connect--closes))))

(ert-deftest agent-repl-test-connect-stream-non-200-closes-as-error ()
  "A stream refused with a non-200 closes as an error carrying the Connect code."
  ;; Arrange
  (agent-repl-test-connect--with-transport
    (let* ((agent-repl-test-connect--closes nil)
           (conn (agent-repl-test-connect--conn)))
      (agent-repl-test-connect--open-stream
       conn 'agent-repl-test-connect--pushes 'agent-repl-test-connect--closes)
      ;; Act
      (let ((record (agent-repl-test-connect--last-spawn)))
        (agent-repl-test-connect--feed
         record (agent-repl-test-connect--http
                 403 "{\"code\":\"permission_denied\",\"message\":\"nope\"}"))
        (agent-repl-test-connect--exit record))
      ;; Assert
      (let ((outcome (car agent-repl-test-connect--closes)))
        (should (eq (car outcome) :error))
        (should (equal (plist-get (cadr outcome) :code) "permission_denied"))))))

(ert-deftest agent-repl-test-connect-stream-is-tracked-on-its-connection ()
  "An open stream is registered on its connection and dropped when it closes."
  ;; Arrange
  (agent-repl-test-connect--with-stream
    ;; Act / Assert
    (should (equal (agent-repl-connect-connection-streams conn) (list stream)))
    (agent-repl-test-connect--feed record (agent-repl-connect-envelope-encode #x02 "{}"))
    (agent-repl-test-connect--exit record)
    (should (null (agent-repl-connect-connection-streams conn)))))

(ert-deftest agent-repl-test-connect-close-cancels-standing-streams ()
  "Closing a connection cancels every stream standing on it."
  ;; Arrange
  (agent-repl-test-connect--with-stream
    ;; Act
    (agent-repl-connect-close conn)
    (agent-repl-test-connect--exit record)
    ;; Assert
    (should (equal agent-repl-test-connect--closes '((:cancelled))))
    (should-not (agent-repl-connect-connection-alive-p conn))))


;;;; ---- Tests: stream acceptance (ON-OPEN) ----

(defvar agent-repl-test-connect--opens 0
  "How many times a stream test's ON-OPEN was called.")

(defvar agent-repl-test-connect--order nil
  "Callback order captured by an acceptance test, oldest first.")

(defun agent-repl-test-connect--open-accepting-stream (conn)
  "Open a stream on CONN counting ON-OPEN calls and recording callback order."
  (agent-repl-connect-stream
   conn "WatchDaemon" "{}"
   (lambda (_alist) (setq agent-repl-test-connect--order
                          (append agent-repl-test-connect--order '(:push))))
   (lambda (_outcome) (setq agent-repl-test-connect--order
                            (append agent-repl-test-connect--order '(:close))))
   (lambda ()
     (setq agent-repl-test-connect--opens (1+ agent-repl-test-connect--opens)
           agent-repl-test-connect--order
           (append agent-repl-test-connect--order '(:open))))))

(ert-deftest agent-repl-test-connect-stream-on-open-fires-on-a-200 ()
  "ACCEPTANCE IS THE HEADER BLOCK: a 200 status calls ON-OPEN."
  ;; Arrange
  (agent-repl-test-connect--with-transport
    (let ((agent-repl-test-connect--opens 0)
          (agent-repl-test-connect--order nil)
          (conn (agent-repl-test-connect--conn)))
      (agent-repl-test-connect--open-accepting-stream conn)
      ;; Act
      (agent-repl-test-connect--feed (agent-repl-test-connect--last-spawn)
                                     (agent-repl-test-connect--http 200 ""))
      ;; Assert
      (should (= agent-repl-test-connect--opens 1)))))

(ert-deftest agent-repl-test-connect-stream-on-open-fires-exactly-once ()
  "Further body chunks are not further acceptances."
  ;; Arrange
  (agent-repl-test-connect--with-transport
    (let ((agent-repl-test-connect--opens 0)
          (agent-repl-test-connect--order nil)
          (conn (agent-repl-test-connect--conn)))
      (agent-repl-test-connect--open-accepting-stream conn)
      (agent-repl-test-connect--feed (agent-repl-test-connect--last-spawn)
                                     (agent-repl-test-connect--http 200 ""))
      ;; Act
      (agent-repl-test-connect--feed
       (agent-repl-test-connect--last-spawn)
       (agent-repl-connect-envelope-encode 0 "{\"n\":1}"))
      ;; Assert
      (should (= agent-repl-test-connect--opens 1)))))

(ert-deftest agent-repl-test-connect-stream-on-open-precedes-the-first-push ()
  "A subscriber is told it is subscribed BEFORE it is told anything else."
  ;; Arrange
  (agent-repl-test-connect--with-transport
    (let ((agent-repl-test-connect--opens 0)
          (agent-repl-test-connect--order nil)
          (conn (agent-repl-test-connect--conn)))
      (agent-repl-test-connect--open-accepting-stream conn)
      ;; Act — headers and the first frame in ONE chunk, the tightest race
      (agent-repl-test-connect--feed
       (agent-repl-test-connect--last-spawn)
       (concat (agent-repl-test-connect--http 200 "")
               (agent-repl-connect-envelope-encode 0 "{\"n\":1}")))
      ;; Assert
      (should (equal agent-repl-test-connect--order '(:open :push))))))

(ert-deftest agent-repl-test-connect-stream-on-open-does-not-fire-on-a-non-200 ()
  "A refused stream was never accepted, so nothing may claim it was."
  ;; Arrange
  (agent-repl-test-connect--with-transport
    (let ((agent-repl-test-connect--opens 0)
          (agent-repl-test-connect--order nil)
          (conn (agent-repl-test-connect--conn)))
      (agent-repl-test-connect--open-accepting-stream conn)
      ;; Act
      (let ((record (agent-repl-test-connect--last-spawn)))
        (agent-repl-test-connect--feed
         record (agent-repl-test-connect--http
                 503 "{\"code\":\"unavailable\",\"message\":\"down\"}"))
        (agent-repl-test-connect--exit record))
      ;; Assert
      (should (= agent-repl-test-connect--opens 0)))))

(ert-deftest agent-repl-test-connect-stream-on-open-does-not-fire-on-death-before-headers ()
  "A socket that dies before any header block never accepted anything."
  ;; Arrange
  (agent-repl-test-connect--with-transport
    (let ((agent-repl-test-connect--opens 0)
          (agent-repl-test-connect--order nil)
          (conn (agent-repl-test-connect--conn)))
      (agent-repl-test-connect--open-accepting-stream conn)
      ;; Act
      (agent-repl-test-connect--exit (agent-repl-test-connect--last-spawn))
      ;; Assert
      (should (= agent-repl-test-connect--opens 0)))))

(ert-deftest agent-repl-test-connect-stream-contains-an-on-open-exception ()
  "A subscriber whose acceptance reaction signals must not kill the stream."
  ;; Arrange
  (agent-repl-test-connect--with-transport
    (let* ((agent-repl-test-connect--pushes nil)
           (agent-repl-test-connect--closes nil)
           (conn (agent-repl-test-connect--conn)))
      (agent-repl-connect-stream
       conn "WatchDaemon" "{}"
       (lambda (alist) (push alist agent-repl-test-connect--pushes))
       (lambda (outcome) (push outcome agent-repl-test-connect--closes))
       (lambda () (error "acceptance reaction blew up")))
      ;; Act
      (let ((record (agent-repl-test-connect--last-spawn)))
        (agent-repl-test-connect--feed record (agent-repl-test-connect--http 200 ""))
        (agent-repl-test-connect--feed
         record (agent-repl-connect-envelope-encode 0 "{\"n\":1}")))
      ;; Assert
      (should (equal agent-repl-test-connect--pushes '(((n . 1))))))))

(ert-deftest agent-repl-test-connect-stream-logs-an-on-open-exception ()
  "The contained acceptance exception is on the record at ERROR."
  ;; Arrange
  (agent-repl-test-connect--with-transport
    (let ((conn (agent-repl-test-connect--conn)))
      (agent-repl-connect-stream
       conn "WatchDaemon" "{}" #'ignore #'ignore
       (lambda () (error "acceptance reaction blew up")))
      ;; Act
      (agent-repl-test-connect--feed (agent-repl-test-connect--last-spawn)
                                     (agent-repl-test-connect--http 200 ""))
      ;; Assert
      (should (agent-repl-test-connect--logs-matching
               'error "elisp\\.connect\\.open-handler-error")))))

(ert-deftest agent-repl-test-connect-stream-without-an-on-open-still-accepts ()
  "ON-OPEN is optional; a caller that does not want it is not an error."
  ;; Arrange
  (agent-repl-test-connect--with-transport
    (let* ((agent-repl-test-connect--pushes nil)
           (agent-repl-test-connect--closes nil)
           (conn (agent-repl-test-connect--conn))
           (stream (agent-repl-test-connect--open-stream
                    conn 'agent-repl-test-connect--pushes
                    'agent-repl-test-connect--closes)))
      ;; Act
      (agent-repl-test-connect--feed (agent-repl-test-connect--last-spawn)
                                     (agent-repl-test-connect--http 200 ""))
      ;; Assert
      (should (agent-repl-connect-stream-opened-p stream)))))

;;;; ---- Terminal sentinels cannot re-enter themselves ----

(ert-deftest agent-repl-test-connect-detach-sentinel-installs-ignore ()
  "A terminal branch detaches its process, so nothing can be delivered again."
  ;; Arrange
  (let ((installed 'unset))
    (cl-letf (((symbol-function 'processp) (lambda (_p) t))
              ((symbol-function 'set-process-sentinel)
               (lambda (_p s) (setq installed s))))
      ;; Act
      (agent-repl-connect--detach-sentinel 'fake-proc)
      ;; Assert
      (should (eq installed #'ignore)))))

(ert-deftest agent-repl-test-connect-detach-sentinel-tolerates-a-non-process ()
  "A nil or non-process argument is not an error."
  ;; Arrange / Act / Assert
  (should-not (agent-repl-connect--detach-sentinel nil)))

(ert-deftest agent-repl-test-connect-terminal-sentinel-runs-once-under-re-delivery ()
  "A terminal branch that tears down runs ONCE even when the teardown
re-delivers its process's status.  This is the sampled hang, driven
directly: the teardown kills a buffer, the kill deletes a process, and the
deletion re-runs whatever sentinel is installed at that moment."
  ;; Arrange
  (let* ((installed nil)
         (runs 0)
         (branch nil))
    (setq branch (lambda (proc)
                   (cl-incf runs)
                   (agent-repl-connect--detach-sentinel proc)
                   (when (< runs 100)      ; a runaway is bounded, not hung
                     ;; The teardown's kill re-delivers the terminal status.
                     (funcall installed proc))))
    (setq installed branch)
    (cl-letf (((symbol-function 'processp) (lambda (_p) t))
              ((symbol-function 'set-process-sentinel)
               (lambda (_p s) (setq installed s))))
      ;; Act
      (funcall branch 'fake-proc)
      ;; Assert
      (should (= runs 1)))))

(provide 'test-connect)

;;; test-connect.el ends here
