;;; test-frontend-client.el --- ERT tests for frontend-client.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the daemon HTTP session client.  The single external
;; boundary (`agent-repl--frontend-http-request') is shadowed via
;; `cl-letf' in every test that reaches it, so no real HTTP ever fires.
;;
;; Run with:
;;   emacs -batch -Q -l ert -l test-frontend-client.el -f ert-run-tests-batch-and-exit

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

(require 'cl-lib)

;;;; ---- Request capture helper ------------------------------------------------

(defmacro agent-repl-test--with-http (responder &rest body)
  "Run BODY with the HTTP boundary shadowed by RESPONDER.
RESPONDER is called with (METHOD URL PAYLOAD) and returns (STATUS . BODY).
Captured requests accumulate in the anaphoric variable `requests' as
\(METHOD URL PAYLOAD) lists, newest last."
  (declare (indent 1))
  `(let ((requests '()))
     (ignore requests)
     (cl-letf (((symbol-function 'agent-repl--frontend-http-request)
                (lambda (method url &optional payload)
                  (setq requests (append requests (list (list method url payload))))
                  (funcall ,responder method url payload))))
       ,@body)))

(defun agent-repl-test--json-ok (obj)
  "Return a (200 . BODY) response carrying OBJ as JSON."
  (cons 200 (json-encode obj)))

;;;; ---- api core -----------------------------------------------------------

(ert-deftest agent-repl-test-frontend-api-parses-json-body ()
  "A 2xx JSON body parses into an alist."
  ;; Arrange
  (agent-repl-test--with-http
      (lambda (&rest _) (agent-repl-test--json-ok '((hello . "world"))))
    ;; Act
    (let ((resp (agent-repl--frontend-api "GET" "/sessions")))
      ;; Assert
      (should (equal (alist-get 'hello resp) "world")))))

(ert-deftest agent-repl-test-frontend-api-errors-on-non-2xx ()
  "A non-2xx status signals with the daemon's error text included."
  ;; Arrange
  (agent-repl-test--with-http
      (lambda (&rest _) (cons 400 "invalid permission_mode"))
    ;; Act / Assert
    (let ((err (should-error (agent-repl--frontend-api "POST" "/sessions"))))
      (should (string-match-p "invalid permission_mode"
                              (error-message-string err))))))

(ert-deftest agent-repl-test-frontend-api-errors-on-bad-json ()
  "An undecodable 2xx body signals rather than returning garbage."
  ;; Arrange
  (agent-repl-test--with-http
      (lambda (&rest _) (cons 200 "{not json"))
    ;; Act / Assert
    (should-error (agent-repl--frontend-api "GET" "/sessions"))))

(ert-deftest agent-repl-test-frontend-api-nil-for-empty-body ()
  "An empty body (DELETE's 204) returns nil rather than erroring."
  ;; Arrange
  (agent-repl-test--with-http
      (lambda (&rest _) (cons 204 ""))
    ;; Act / Assert
    (should (null (agent-repl--frontend-api "DELETE" "/sessions/s1")))))

;;;; ---- create ---------------------------------------------------------------

(ert-deftest agent-repl-test-frontend-create-posts-cwd-payload ()
  "Create POSTs to /sessions with the cwd in the JSON payload."
  ;; Arrange
  (agent-repl-test--with-http
      (lambda (&rest _) (agent-repl-test--json-ok '((session_id . "s_1"))))
    ;; Act
    (let ((id (agent-repl--frontend-create-session "/w/tree")))
      ;; Assert
      (should (equal id "s_1"))
      (pcase-let ((`(,method ,url ,payload) (car requests)))
        (should (equal method "POST"))
        (should (string-suffix-p "/sessions" url))
        (should (string-match-p "\"cwd\":\"/w/tree\"" payload))))))

(ert-deftest agent-repl-test-frontend-create-passes-model-and-resume ()
  "Optional model/resume land in the payload when given."
  ;; Arrange
  (agent-repl-test--with-http
      (lambda (&rest _) (agent-repl-test--json-ok '((session_id . "s_1"))))
    ;; Act
    (agent-repl--frontend-create-session "/w" "haiku" "cli-uuid-9")
    ;; Assert
    (let ((payload (nth 2 (car requests))))
      (should (string-match-p "\"model\":\"haiku\"" payload))
      (should (string-match-p "\"resume\":\"cli-uuid-9\"" payload)))))

(ert-deftest agent-repl-test-frontend-create-sends-permission-mode ()
  "Create carries the configured permission mode (vterm-parity default auto)."
  ;; Arrange
  (agent-repl-test--with-http
      (lambda (&rest _) (agent-repl-test--json-ok '((session_id . "s_1"))))
    (let ((agent-repl-frontend-permission-mode "auto"))
      ;; Act
      (agent-repl--frontend-create-session "/w")
      ;; Assert
      (should (string-match-p "\"permission_mode\":\"auto\""
                              (nth 2 (car requests)))))))

(ert-deftest agent-repl-test-frontend-create-omits-permission-mode-when-nil ()
  "A nil mode customization omits the field (SDK default)."
  ;; Arrange
  (agent-repl-test--with-http
      (lambda (&rest _) (agent-repl-test--json-ok '((session_id . "s_1"))))
    (let ((agent-repl-frontend-permission-mode nil))
      ;; Act
      (agent-repl--frontend-create-session "/w")
      ;; Assert
      (should-not (string-match-p "permission_mode" (nth 2 (car requests)))))))

(ert-deftest agent-repl-test-frontend-create-requires-cwd ()
  "Create without a cwd signals instead of minting a cwd-less session."
  ;; Act / Assert — no HTTP boundary shadow needed: must fail before I/O.
  (should-error (agent-repl--frontend-create-session nil)))

(ert-deftest agent-repl-test-frontend-create-errors-without-session-id ()
  "A create response missing session_id signals loudly."
  ;; Arrange
  (agent-repl-test--with-http
      (lambda (&rest _) (agent-repl-test--json-ok '((unexpected . "shape"))))
    ;; Act / Assert
    (should-error (agent-repl--frontend-create-session "/w"))))

;;;; ---- liveness -------------------------------------------------------------

(ert-deftest agent-repl-test-frontend-session-live-p-true-for-listed ()
  "A listed, non-terminal session is live."
  ;; Arrange
  (agent-repl-test--with-http
      (lambda (&rest _)
        (agent-repl-test--json-ok
         `((sessions . [,(list '(session_id . "s_1") '(terminal . :json-false))]))))
    ;; Act / Assert
    (should (agent-repl--frontend-session-live-p "s_1"))))

(ert-deftest agent-repl-test-frontend-session-live-p-nil-for-terminal ()
  "A terminal session is not live."
  ;; Arrange
  (agent-repl-test--with-http
      (lambda (&rest _)
        (agent-repl-test--json-ok
         `((sessions . [,(list '(session_id . "s_1") '(terminal . t))]))))
    ;; Act / Assert
    (should-not (agent-repl--frontend-session-live-p "s_1"))))

(ert-deftest agent-repl-test-frontend-session-live-p-nil-for-unlisted ()
  "An unlisted id is not live."
  ;; Arrange
  (agent-repl-test--with-http
      (lambda (&rest _) (agent-repl-test--json-ok '((sessions . []))))
    ;; Act / Assert
    (should-not (agent-repl--frontend-session-live-p "ghost"))))

;;;; ---- wait-ready -------------------------------------------------------------

(ert-deftest agent-repl-test-frontend-wait-ready-retries-then-succeeds ()
  "Readiness polling retries failed probes until one succeeds."
  ;; Arrange — first two probes fail, third answers.
  (let ((calls 0)
        (sleeps '()))
    (cl-letf (((symbol-function 'sleep-for)
               (lambda (secs) (push secs sleeps))))
      (agent-repl-test--with-http
          (lambda (&rest _)
            (setq calls (1+ calls))
            (if (< calls 3)
                (error "connection refused")
              (agent-repl-test--json-ok '((sessions . [])))))
        ;; Act / Assert
        (should (agent-repl--frontend-wait-ready))
        (should (= calls 3))
        ;; Pacing: one 0.2s blocking sleep per FAILED probe, none after
        ;; the success.
        (should (equal sleeps '(0.2 0.2)))))))

(ert-deftest agent-repl-test-frontend-ensure-session-fails-fast-without-daemon ()
  "A nil daemon-ensure (auto-start off/inhibited) errors immediately.
Polling readiness against a daemon that was never started would burn
the retry budget on a misleading error."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w")
    (let ((polled nil))
      (cl-letf (((symbol-function 'agent-repl--ensure-frontend-daemon)
                 (lambda (&optional _f) nil))
                ((symbol-function 'agent-repl--frontend-wait-ready)
                 (lambda () (setq polled t))))
        ;; Act
        (let ((err (should-error (agent-repl--frontend-ensure-session "ws1"))))
          ;; Assert — loud, immediate, and readiness never polled.
          (should (string-match-p "not started" (error-message-string err)))
          (should-not polled))))))

(ert-deftest agent-repl-test-frontend-wait-ready-errors-after-attempts ()
  "Readiness polling gives up loudly after the attempt budget."
  ;; Arrange
  (let ((agent-repl-frontend-ready-attempts 3))
    (cl-letf (((symbol-function 'sleep-for) #'ignore))
      (agent-repl-test--with-http
          (lambda (&rest _) (error "connection refused"))
        ;; Act / Assert
        (should-error (agent-repl--frontend-wait-ready))))))

;;;; ---- ensure-session ----------------------------------------------------------

(defmacro agent-repl-test--with-ws (ws plist &rest body)
  "Register workspace WS with PLIST for BODY, cleaning up after."
  (declare (indent 2))
  `(unwind-protect
       (progn
         (puthash ,ws (copy-sequence ,plist) agent-repl--workspaces)
         ,@body)
     (remhash ,ws agent-repl--workspaces)))

(ert-deftest agent-repl-test-frontend-ensure-session-reuses-live-id ()
  "A recorded id still listed live is reused without a POST."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:frontend-session-id "s_live" :project-dir "/w")
    (cl-letf (((symbol-function 'agent-repl--ensure-frontend-daemon) (lambda (&optional _f) t))
              ((symbol-function 'agent-repl--frontend-wait-ready) (lambda () t)))
      (agent-repl-test--with-http
          (lambda (&rest _)
            (agent-repl-test--json-ok
             `((sessions . [,(list '(session_id . "s_live") '(terminal . :json-false))]))))
        ;; Act
        (let ((id (agent-repl--frontend-ensure-session "ws1")))
          ;; Assert — only the liveness GET fired, no POST.
          (should (equal id "s_live"))
          (should (cl-every (lambda (r) (equal (car r) "GET")) requests)))))))

(ert-deftest agent-repl-test-frontend-ensure-session-creates-when-stale ()
  "A recorded id the daemon no longer lists is replaced via POST."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:frontend-session-id "s_stale" :project-dir "/w/tree")
    (cl-letf (((symbol-function 'agent-repl--ensure-frontend-daemon) (lambda (&optional _f) t))
              ((symbol-function 'agent-repl--frontend-wait-ready) (lambda () t)))
      (agent-repl-test--with-http
          (lambda (method &rest _)
            (if (equal method "GET")
                (agent-repl-test--json-ok '((sessions . [])))
              (agent-repl-test--json-ok '((session_id . "s_fresh")))))
        ;; Act
        (let ((id (agent-repl--frontend-ensure-session "ws1")))
          ;; Assert
          (should (equal id "s_fresh"))
          (should (equal (agent-repl--ws-get "ws1" :frontend-session-id) "s_fresh")))))))

(ert-deftest agent-repl-test-frontend-ensure-session-adopts-git-root ()
  "An unregistered perspective adopts the current git root as its project dir.
Plain project persps (doom itself, projectile switches) never pass
through agent-repl workspace creation, so :project-dir is absent — the
resolver supplies it exactly as creation would."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:frontend-session-id nil)
    (cl-letf (((symbol-function 'agent-repl--ensure-frontend-daemon) (lambda (&optional _f) t))
              ((symbol-function 'agent-repl--frontend-wait-ready) (lambda () t))
              ((symbol-function 'agent-repl--resolve-current-git-root)
               (lambda () "/repo/root/")))
      (agent-repl-test--with-http
          (lambda (method &rest _)
            (if (equal method "GET")
                (agent-repl-test--json-ok '((sessions . [])))
              (agent-repl-test--json-ok '((session_id . "s_new")))))
        ;; Act
        (let ((id (agent-repl--frontend-ensure-session "ws1")))
          ;; Assert — session rooted at the adopted dir, dir recorded.
          (should (equal id "s_new"))
          (should (equal (agent-repl--ws-get "ws1" :project-dir) "/repo/root/"))
          (let ((post (car (last requests))))
            (should (string-match-p "\"cwd\":\"/repo/root/\"" (nth 2 post)))))))))

(ert-deftest agent-repl-test-frontend-ensure-session-resumes-durable-id ()
  "A fresh POST resumes the workspace's durable claude session uuid.
The uuid lives in the active instantiation (hook-captured; shared with
the vterm frontend), so a recreated daemon binding continues the
conversation instead of starting over."
  ;; Arrange
  (agent-repl-test--with-ws "ws1"
      (list :frontend-session-id nil :project-dir "/w"
            :active-env :bare-metal
            :bare-metal (make-agent-repl-instantiation :session-id "cli-uuid-7"))
    (cl-letf (((symbol-function 'agent-repl--ensure-frontend-daemon) (lambda (&optional _f) t))
              ((symbol-function 'agent-repl--frontend-wait-ready) (lambda () t)))
      (agent-repl-test--with-http
          (lambda (method &rest _)
            (if (equal method "GET")
                (agent-repl-test--json-ok '((sessions . [])))
              (agent-repl-test--json-ok '((session_id . "s_resumed")))))
        ;; Act
        (agent-repl--frontend-ensure-session "ws1")
        ;; Assert — the POST payload carries the resume uuid.
        (let ((post (car (last requests))))
          (should (string-match-p "\"resume\":\"cli-uuid-7\"" (nth 2 post))))))))

(ert-deftest agent-repl-test-frontend-ensure-session-fresh-without-durable-id ()
  "No recorded durable session id means a fresh session with no resume field."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:frontend-session-id nil :project-dir "/w")
    (cl-letf (((symbol-function 'agent-repl--ensure-frontend-daemon) (lambda (&optional _f) t))
              ((symbol-function 'agent-repl--frontend-wait-ready) (lambda () t)))
      (agent-repl-test--with-http
          (lambda (method &rest _)
            (if (equal method "GET")
                (agent-repl-test--json-ok '((sessions . [])))
              (agent-repl-test--json-ok '((session_id . "s_new")))))
        ;; Act
        (agent-repl--frontend-ensure-session "ws1")
        ;; Assert
        (let ((post (car (last requests))))
          (should-not (string-match-p "resume" (or (nth 2 post) ""))))))))

(ert-deftest agent-repl-test-frontend-ensure-session-propagates-non-git-error ()
  "Outside a git repository, the resolver's user-error surfaces unchanged."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:frontend-session-id nil)
    (cl-letf (((symbol-function 'agent-repl--ensure-frontend-daemon) (lambda (&optional _f) t))
              ((symbol-function 'agent-repl--frontend-wait-ready) (lambda () t))
              ((symbol-function 'agent-repl--resolve-current-git-root)
               (lambda () (user-error "not inside a git repository"))))
      (agent-repl-test--with-http
          (lambda (&rest _) (agent-repl-test--json-ok '((sessions . []))))
        ;; Act / Assert
        (should-error (agent-repl--frontend-ensure-session "ws1") :type 'user-error)))))

;;;; ---- message / interrupt injection -----------------------------------------

(ert-deftest agent-repl-test-frontend-send-message-posts-content ()
  "Send-message POSTs the content and returns the daemon's request id."
  ;; Arrange
  (agent-repl-test--with-http
      (lambda (&rest _) (agent-repl-test--json-ok '((request_id . "r_9"))))
    ;; Act
    (let ((rid (agent-repl--frontend-send-message "s_1" "hello there")))
      ;; Assert
      (should (equal rid "r_9"))
      (pcase-let ((`(,method ,url ,payload) (car requests)))
        (should (equal method "POST"))
        (should (string-suffix-p "/sessions/s_1/message" url))
        (should (string-match-p "\"content\":\"hello there\"" payload))))))

(ert-deftest agent-repl-test-frontend-send-message-errors-without-request-id ()
  "A malformed injection response signals instead of returning nil."
  ;; Arrange
  (agent-repl-test--with-http
      (lambda (&rest _) (agent-repl-test--json-ok '((unexpected . "shape"))))
    ;; Act / Assert
    (should-error (agent-repl--frontend-send-message "s_1" "hi"))))

(ert-deftest agent-repl-test-frontend-interrupt-session-posts-route ()
  "Interrupt POSTs the session's interrupt route."
  ;; Arrange
  (agent-repl-test--with-http
      (lambda (&rest _) (cons 202 ""))
    ;; Act
    (agent-repl--frontend-interrupt-session "s_1")
    ;; Assert
    (pcase-let ((`(,method ,url ,_) (car requests)))
      (should (equal method "POST"))
      (should (string-suffix-p "/sessions/s_1/interrupt" url)))))

(ert-deftest agent-repl-test-gui-running-p-tracks-session-binding ()
  "The gui liveness capability is exactly the presence of :frontend-session-id."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:frontend-session-id "s_1")
    ;; Act / Assert
    (should (agent-repl--gui-running-p "ws1")))
  (agent-repl-test--with-ws "ws2" '(:project-dir "/w")
    (should-not (agent-repl--gui-running-p "ws2"))))

(ert-deftest agent-repl-test-frontend-send-user-message-heals-via-ensure ()
  "Workspace sends resolve the session through ensure (healing staleness)."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w")
    (let ((sent nil))
      (cl-letf (((symbol-function 'agent-repl--frontend-ensure-session)
                 (lambda (_ws) "s_fresh"))
                ((symbol-function 'agent-repl--frontend-send-message)
                 (lambda (id text) (setq sent (list id text)) "r_1")))
        ;; Act
        (agent-repl--frontend-send-user-message "ws1" "hi")
        ;; Assert
        (should (equal sent '("s_fresh" "hi")))))))

;;;; ---- release on nuke ------------------------------------------------------------

(ert-deftest agent-repl-test-frontend-release-deletes-and-clears ()
  "Release DELETEs the recorded session and clears the plist key."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:frontend-session-id "s_1")
    (agent-repl-test--with-http
        (lambda (&rest _) (cons 204 ""))
      ;; Act
      (agent-repl--frontend-release-workspace-session "ws1")
      ;; Assert
      (pcase-let ((`(,method ,url ,_) (car requests)))
        (should (equal method "DELETE"))
        (should (string-suffix-p "/sessions/s_1" url)))
      (should (null (agent-repl--ws-get "ws1" :frontend-session-id))))))

(ert-deftest agent-repl-test-frontend-release-logs-but-never-signals ()
  "A dead daemon must not abort the nuke: release logs and proceeds."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:frontend-session-id "s_1")
    (agent-repl-test--with-http
        (lambda (&rest _) (error "connection refused"))
      ;; Act — must not signal.
      (agent-repl--frontend-release-workspace-session "ws1")
      ;; Assert — the key is still cleared.
      (should (null (agent-repl--ws-get "ws1" :frontend-session-id))))))

(ert-deftest agent-repl-test-frontend-release-noop-without-id ()
  "Release without a recorded id performs no HTTP at all."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w")
    (agent-repl-test--with-http
        (lambda (&rest _) (error "must not be called"))
      ;; Act
      (agent-repl--frontend-release-workspace-session "ws1")
      ;; Assert
      (should (null requests)))))

(ert-deftest agent-repl-test-frontend-release-registered-on-ws-del-hook ()
  "The release fn is registered on the pre-tombstone hook."
  ;; Assert
  (should (memq #'agent-repl--frontend-release-workspace-session
                agent-repl-ws-del-hook)))

;;;; ---- gui-send-turn ------------------------------------------------------------

(ert-deftest agent-repl-test-gui-send-turn-sets-thinking-before-send ()
  "gui-send-turn sets :thinking optimistically BEFORE the HTTP send.
The prompt_submit hook remains the authoritative confirmation, but a
permission request can beat a lagging hook and on-permission-event
gates on :thinking, so the optimistic write must precede the wire."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:frontend-session-id "s_1" :project-dir "/w")
    (let ((state-at-send nil))
      (cl-letf (((symbol-function 'agent-repl--frontend-send-user-message)
                 (lambda (ws _input)
                   (setq state-at-send (agent-repl--ws-get ws :agent-state))))
                ((symbol-function 'agent-repl--increment-prefix-counter) #'ignore)
                ((symbol-function 'agent-repl--run-send-posthooks) #'ignore)
                ((symbol-function 'agent-repl--kickoff-prompt-summary) #'ignore))
        ;; Act
        (agent-repl--gui-send-turn "ws1" "prepared input" "raw input")
        ;; Assert
        (should (eq state-at-send :thinking))))))

;;;; ---- session-url ---------------------------------------------------------------

(ert-deftest agent-repl-test-frontend-session-url-shape ()
  "The webapp attach URL carries the session query param."
  ;; Arrange
  (let ((agent-repl-frontend-daemon-addr "127.0.0.1:9999"))
    ;; Act / Assert
    (should (equal (agent-repl--frontend-session-url "s_9")
                   "http://127.0.0.1:9999/?session=s_9"))))

(provide 'test-frontend-client)

;;; test-frontend-client.el ends here
