;;; test-connect.el --- ERT tests for agent-repl connect.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;   AGENT_REPL_FORBID_VENDOR_CALLS=1 emacs -batch -Q -l ert \
;;     -l lisp/test-connect.el -f ert-run-tests-batch-and-exit
;;
;; NO EXTERNAL PROCESS IS EVER SPAWNED HERE.  `agent-repl-connect--spawn-curl'
;; is connect.el's ONE external boundary and is registered in
;; `agent-repl--external-boundary-functions', so the harness's guard already
;; fails any test that reaches it unstubbed.  Every test below stubs it with
;; `agent-repl-test-connect--spawn-stub', which hands back a real but
;; PROCESS-LESS `make-pipe-process' object — process-live-p, delete-process
;; and the sentinel all behave as production expects, while nothing is
;; executed — and records the filter and sentinel so a test can drive the
;; exchange byte by byte and deterministically, with no sleeps and no
;; scheduler dependence.
;;
;; The real curl round trip belongs to the integration suite, not here.

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                           (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Harness ----

(defvar agent-repl-test-connect--spawned nil
  "List of spawn records, newest first, captured by the spawn stub.
Each is a plist `(:name :args :filter :sentinel :stderr :process)'.")

(defvar agent-repl-test-connect--logs nil
  "List of `(LEVEL . TEXT)' entries, newest first, captured from the ladder.")

(defvar agent-repl-test-connect--script nil
  "Canned stdout the spawn stub replays inline, or nil to stay silent.
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

(defun agent-repl-test-connect--spawn-stub (name args filter sentinel stderr-buffer)
  "Stand in for `agent-repl-connect--spawn-curl' without running anything.
Creates a pipe process (a real process object bound to no program), records
the exchange, and — when `agent-repl-test-connect--script' is set — replays
that stdout and exits inline."
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
         (record (list :name name :args args :filter filter :sentinel sentinel
                       :stderr stderr-buffer :process process)))
    (push record agent-repl-test-connect--spawned)
    (when agent-repl-test-connect--script
      (funcall filter process agent-repl-test-connect--script)
      (delete-process process)
      (funcall sentinel process "finished\n"))
    process))

(defun agent-repl-test-connect--last-spawn ()
  "Return the most recent spawn record."
  (car agent-repl-test-connect--spawned))

(defun agent-repl-test-connect--feed (record chunk)
  "Hand CHUNK to RECORD's process filter, as curl's stdout would."
  (funcall (plist-get record :filter) (plist-get record :process) chunk))

(defun agent-repl-test-connect--exit (record)
  "End RECORD's process and run its sentinel, as an exiting curl would."
  (let ((process (plist-get record :process)))
    (when (process-live-p process)
      (delete-process process))
    (funcall (plist-get record :sentinel) process "finished\n")))

(defun agent-repl-test-connect--cleanup ()
  "Delete every process the stub created during a test."
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
         (cl-letf (((symbol-function 'agent-repl-connect--spawn-curl)
                    #'agent-repl-test-connect--spawn-stub)
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
  "Return a curl `-D -' stdout blob: STATUS line, EXTRA-HEADERS, then BODY."
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
    (agent-repl-connect--reader-feed reader "curl: (7) Failed to connect\r\n\r\n")
    ;; Assert
    (should (null (agent-repl-connect--reader-status reader)))))

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

;;;; ---- Tests: argv composition ----

(ert-deftest agent-repl-test-connect-curl-argv-targets-the-connect-method-url ()
  "The request URL is the service path plus the bare method name."
  ;; Arrange / Act
  (let ((argv (agent-repl-connect--curl-argv
               "127.0.0.1:41234" "RegisterWorkspace" "application/json" "/tmp/body")))
    ;; Assert
    (should (equal (car (last argv))
                   "http://127.0.0.1:41234/agentrepl.v1.AgentRepl/RegisterWorkspace"))))

(ert-deftest agent-repl-test-connect-curl-argv-sends-the-connect-protocol-version ()
  "Every Connect request carries `Connect-Protocol-Version: 1'."
  ;; Arrange / Act
  (let ((argv (agent-repl-connect--curl-argv
               "127.0.0.1:1" "DaemonHealth" "application/json" "/tmp/body")))
    ;; Assert
    (should (member "Connect-Protocol-Version: 1" argv))))

(ert-deftest agent-repl-test-connect-curl-argv-carries-the-requested-content-type ()
  "The content type is the caller's, so unary and streaming differ only there."
  ;; Arrange / Act
  (let ((unary (agent-repl-connect--curl-argv
                "127.0.0.1:1" "DaemonHealth" "application/json" "/tmp/b"))
        (stream (agent-repl-connect--curl-argv
                 "127.0.0.1:1" "WatchDaemon" "application/connect+json" "/tmp/b")))
    ;; Assert
    (should (member "Content-Type: application/json" unary))
    (should (member "Content-Type: application/connect+json" stream))))

(ert-deftest agent-repl-test-connect-curl-argv-reads-the-body-from-a-file ()
  "The request body rides `--data-binary @FILE', never argv."
  ;; Arrange / Act
  (let ((argv (agent-repl-connect--curl-argv
               "127.0.0.1:1" "SubmitPrompt" "application/json" "/tmp/body-file")))
    ;; Assert
    (should (member "--data-binary" argv))
    (should (member "@/tmp/body-file" argv))))

(ert-deftest agent-repl-test-connect-curl-argv-pins-http-1-1-and-dumps-headers ()
  "The argv pins HTTP/1.1, disables buffering, and dumps headers onto stdout."
  ;; Arrange / Act
  (let ((argv (agent-repl-connect--curl-argv
               "127.0.0.1:1" "WatchDaemon" "application/connect+json" "/tmp/b")))
    ;; Assert
    (should (member "--http1.1" argv))
    (should (member "--no-buffer" argv))
    (should (equal (member "-D" argv) (append '("-D" "-") (cdr (member "-" (member "-D" argv))))))
    (should (member "Expect:" argv))))

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
  "An exit with no HTTP status at all is a transport failure carrying stderr."
  ;; Arrange
  (agent-repl-test-connect--with-transport
    (let ((conn (agent-repl-test-connect--conn))
          (failure nil))
      (agent-repl-connect-unary conn "DaemonHealth" "{}"
                                :on-failure (lambda (d) (setq failure d)))
      ;; Act
      (let ((record (agent-repl-test-connect--last-spawn)))
        (with-current-buffer (plist-get record :stderr)
          (insert "curl: (7) Failed to connect to 127.0.0.1 port 41234"))
        (agent-repl-test-connect--exit record))
      ;; Assert
      (should (eq (plist-get failure :kind) :transport))
      (should (string-match-p "Failed to connect" (plist-get failure :message))))))

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

(ert-deftest agent-repl-test-connect-unary-writes-then-deletes-the-body-file ()
  "The request body is staged in a temp file that the exit removes."
  ;; Arrange
  (agent-repl-test-connect--with-transport
    (let* ((conn (agent-repl-test-connect--conn)))
      (agent-repl-connect-unary conn "RegisterWorkspace" "{\"dir\":\"/w\"}")
      (let* ((record (agent-repl-test-connect--last-spawn))
             (arg (car (seq-filter (lambda (a) (string-prefix-p "@" a))
                                   (plist-get record :args))))
             (file (substring arg 1)))
        ;; Act
        (let ((staged (with-temp-buffer (insert-file-contents file) (buffer-string))))
          (agent-repl-test-connect--feed record (agent-repl-test-connect--http 200 "{}"))
          (agent-repl-test-connect--exit record)
          ;; Assert
          (should (equal staged "{\"dir\":\"/w\"}"))
          (should-not (file-exists-p file)))))))

(ert-deftest agent-repl-test-connect-unary-kills-its-stderr-buffer-on-exit ()
  "The per-call stderr buffer does not outlive the call that created it."
  ;; Arrange
  (agent-repl-test-connect--with-transport
    (let ((conn (agent-repl-test-connect--conn)))
      (agent-repl-connect-unary conn "DaemonHealth" "{}")
      (let ((record (agent-repl-test-connect--last-spawn)))
        ;; Act
        (agent-repl-test-connect--feed record (agent-repl-test-connect--http 200 "{}"))
        (agent-repl-test-connect--exit record)
        ;; Assert
        (should-not (buffer-live-p (plist-get record :stderr)))))))

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
      (let* ((record (agent-repl-test-connect--last-spawn))
             (arg (car (seq-filter (lambda (a) (string-prefix-p "@" a))
                                   (plist-get record :args))))
             (staged (let ((coding-system-for-read 'no-conversion))
                       (with-temp-buffer
                         (set-buffer-multibyte nil)
                         (insert-file-contents (substring arg 1))
                         (buffer-string)))))
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
wait on a curl exit for that fact is what left a real stream hanging."
  ;; Arrange / Act
  (agent-repl-test-connect--with-stream
    (agent-repl-test-connect--feed record (agent-repl-connect-envelope-encode #x02 "{}"))
    ;; Assert — no `--exit' has run yet
    (should (equal agent-repl-test-connect--closes '((:ended))))))

(ert-deftest agent-repl-test-connect-stream-end-frame-kills-the-process ()
  "The end frame is followed by killing curl: nothing is left running."
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

(ert-deftest agent-repl-test-connect-stream-end-frame-still-frees-the-temporaries ()
  "Closing on the frame must not leak the body file or the stderr buffer."
  ;; Arrange
  (agent-repl-test-connect--with-stream
    ;; Act
    (agent-repl-test-connect--feed record (agent-repl-connect-envelope-encode #x02 "{}"))
    (agent-repl-test-connect--exit record)
    ;; Assert
    (should-not (buffer-live-p (plist-get record :stderr)))))

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
  "A curl that dies before any header block never accepted anything."
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

(provide 'test-connect)

;;; test-connect.el ends here
