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

(ert-deftest agent-repl-test-frontend-create-defaults-model-to-interactive ()
  "A create with no model sends `agent-repl-interactive-model' so the
daemon's hello carries a concrete model from the first frame."
  ;; Arrange
  (agent-repl-test--with-http
      (lambda (&rest _) (agent-repl-test--json-ok '((session_id . "s_1"))))
    (let ((agent-repl-interactive-model "opus"))
      ;; Act
      (agent-repl--frontend-create-session "/w")
      ;; Assert
      (should (string-match-p "\"model\":\"opus\"" (nth 2 (car requests)))))))

(ert-deftest agent-repl-test-frontend-create-omits-model-when-interactive-nil ()
  "A nil `agent-repl-interactive-model' is respected as \"let the CLI
choose\": no model flag is sent."
  ;; Arrange
  (agent-repl-test--with-http
      (lambda (&rest _) (agent-repl-test--json-ok '((session_id . "s_1"))))
    (let ((agent-repl-interactive-model nil))
      ;; Act
      (agent-repl--frontend-create-session "/w")
      ;; Assert
      (should-not (string-match-p "\"model\"" (nth 2 (car requests)))))))

(ert-deftest agent-repl-test-frontend-create-explicit-model-overrides-interactive ()
  "An explicit MODEL wins over `agent-repl-interactive-model'."
  ;; Arrange
  (agent-repl-test--with-http
      (lambda (&rest _) (agent-repl-test--json-ok '((session_id . "s_1"))))
    (let ((agent-repl-interactive-model "opus"))
      ;; Act
      (agent-repl--frontend-create-session "/w" "haiku")
      ;; Assert
      (should (string-match-p "\"model\":\"haiku\"" (nth 2 (car requests)))))))

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

(ert-deftest agent-repl-test-frontend-create-sends-multi-repo-config-dir ()
  "A cwd under the multi-repo root carries that account's CLAUDE_CONFIG_DIR."
  ;; Arrange
  (agent-repl-test--with-http
      (lambda (&rest _) (agent-repl-test--json-ok '((session_id . "s_1"))))
    (let ((process-environment (cons "MULTI_REPO_ROOT=/home/user/multi" process-environment))
          (agent-repl-multi-repo-config-dir "~/.claude-chesscom"))
      ;; Act
      (agent-repl--frontend-create-session "/home/user/multi/repoA")
      ;; Assert — the gui session must run as the SAME account the vterm
      ;; start command would select for this project.
      (should (string-match-p (format "\"config_dir\":\"%s\""
                                      (expand-file-name "~/.claude-chesscom"))
                              (nth 2 (car requests)))))))

(ert-deftest agent-repl-test-frontend-create-omits-config-dir-outside-multi-repo ()
  "A personal project omits config_dir so the CLI uses its own default root."
  ;; Arrange
  (agent-repl-test--with-http
      (lambda (&rest _) (agent-repl-test--json-ok '((session_id . "s_1"))))
    (let ((process-environment (cons "MULTI_REPO_ROOT=/home/user/multi" process-environment))
          (agent-repl-default-config-dir nil)
          (agent-repl-doom-multi-repo-mode nil))
      ;; Act
      (agent-repl--frontend-create-session "/home/user/personal/proj")
      ;; Assert
      (should-not (string-match-p "config_dir" (nth 2 (car requests)))))))

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

;;;; ---- resume_transcript_missing hard-fail ----------------------------------

(defun agent-repl-test--resume-missing-body (&optional resume-id searched)
  "Return a (422 . BODY) response mimicking the daemon's hard-fail.
RESUME-ID defaults to \"uuid-gone\"; SEARCHED defaults to one path."
  (cons 422 (json-encode
             `((code . "resume_transcript_missing")
               (resume_id . ,(or resume-id "uuid-gone"))
               (searched_paths . ,(vconcat (or searched '("/cfg/projects/-w/uuid-gone.jsonl"))))
               (error . "loud message")))))

(ert-deftest agent-repl-test-frontend-create-hard-fails-on-missing-transcript ()
  "A resume_transcript_missing hard-fail signals the distinct
`agent-repl-resume-transcript-missing' rather than starting fresh."
  ;; Arrange
  (cl-letf (((symbol-function 'agent-repl--dispatch-resume-investigation)
             (lambda (&rest _) "resume-investigate-uuid-gon")))
    (agent-repl-test--with-http
        (lambda (&rest _) (agent-repl-test--resume-missing-body))
      ;; Act / Assert
      (should-error (agent-repl--frontend-create-session "/w" nil "uuid-gone")
                    :type 'agent-repl-resume-transcript-missing))))

(ert-deftest agent-repl-test-frontend-create-hard-fail-names-investigation-ws ()
  "The surfaced hard-fail error names the investigation workspace."
  ;; Arrange
  (cl-letf (((symbol-function 'agent-repl--dispatch-resume-investigation)
             (lambda (&rest _) "resume-investigate-uuid-gon")))
    (agent-repl-test--with-http
        (lambda (&rest _) (agent-repl-test--resume-missing-body))
      ;; Act
      (let ((err (should-error (agent-repl--frontend-create-session "/w" nil "uuid-gone"))))
        ;; Assert
        (should (string-match-p "resume-investigate-uuid-gon"
                                (error-message-string err)))))))

(ert-deftest agent-repl-test-frontend-create-hard-fail-dispatches-investigation ()
  "The hard-fail opens an investigation workspace instead of a fresh session."
  ;; Arrange
  (let (captured)
    (cl-letf (((symbol-function 'agent-repl--dispatch-resume-investigation)
               (lambda (resume-id searched cwd)
                 (setq captured (list resume-id searched cwd)) "ws")))
      (agent-repl-test--with-http
          (lambda (&rest _)
            (agent-repl-test--resume-missing-body "uuid-gone" '("/p/uuid-gone.jsonl")))
        ;; Act
        (ignore-errors (agent-repl--frontend-create-session "/w/tree" nil "uuid-gone"))
        ;; Assert — the lost session, its searched paths, and the cwd flow through.
        (should (equal captured '("uuid-gone" ("/p/uuid-gone.jsonl") "/w/tree")))))))

(ert-deftest agent-repl-test-frontend-create-hard-fail-falls-back-to-requested-resume ()
  "When the body omits resume_id, the requested resume id is used."
  ;; Arrange
  (let (captured)
    (cl-letf (((symbol-function 'agent-repl--dispatch-resume-investigation)
               (lambda (resume-id &rest _) (setq captured resume-id) "ws")))
      (agent-repl-test--with-http
          (lambda (&rest _)
            (cons 422 (json-encode '((code . "resume_transcript_missing")))))
        ;; Act
        (ignore-errors (agent-repl--frontend-create-session "/w" nil "requested-uuid"))
        ;; Assert
        (should (equal captured "requested-uuid"))))))

(ert-deftest agent-repl-test-frontend-create-other-errors-stay-generic ()
  "A non-2xx WITHOUT the resume_transcript_missing code stays a plain error."
  ;; Arrange
  (agent-repl-test--with-http
      (lambda (&rest _) (cons 400 "invalid permission_mode"))
    ;; Act
    (let ((err (should-error (agent-repl--frontend-create-session "/w"))))
      ;; Assert — not misclassified as the resume hard-fail.
      (should-not (eq (car err) 'agent-repl-resume-transcript-missing))
      (should (string-match-p "invalid permission_mode" (error-message-string err))))))

(ert-deftest agent-repl-test-frontend-resume-missing-predicate-matches ()
  "The predicate returns the parsed body on the hard-fail code."
  (should (agent-repl--frontend-resume-transcript-missing
           (json-encode '((code . "resume_transcript_missing") (resume_id . "x"))))))

(ert-deftest agent-repl-test-frontend-resume-missing-predicate-ignores-other-code ()
  "The predicate returns nil for an unrelated error code."
  (should-not (agent-repl--frontend-resume-transcript-missing
               (json-encode '((code . "internal"))))))

(ert-deftest agent-repl-test-frontend-resume-missing-predicate-ignores-non-json ()
  "The predicate returns nil (never signals) on a non-JSON body."
  (should-not (agent-repl--frontend-resume-transcript-missing "not json at all")))

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
              ((symbol-function 'agent-repl--frontend-wait-ready) (lambda () t))
              ((symbol-function 'agent-repl--initialize-ws-env) (lambda (&rest _args) nil)))
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
              ((symbol-function 'agent-repl--initialize-ws-env) (lambda (&rest _args) nil))
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
              ((symbol-function 'agent-repl--frontend-wait-ready) (lambda () t))
              ((symbol-function 'agent-repl--initialize-ws-env) (lambda (&rest _args) nil)))
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

(ert-deftest agent-repl-test-frontend-ensure-session-passes-ws-model ()
  "A fresh POST carries the workspace's `:model', not a hardcoded nil.

Regression test: `agent-repl--frontend-ensure-session' once passed a
literal nil for the model argument regardless of WS, so a gui workspace
generated with an explicit model silently ran on the daemon/SDK default
model instead.  Fails against that hardcoded nil, since `:model' would
then never reach the POST body at all
\(`agent-repl--frontend-create-session' omits the field entirely when
model is nil)."
  ;; Arrange
  (agent-repl-test--with-ws "ws1"
      '(:frontend-session-id nil :project-dir "/w" :model "opus")
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
          (should (string-match-p "\"model\":\"opus\"" (nth 2 post))))))))

(ert-deftest agent-repl-test-frontend-ensure-session-restores-env-when-missing ()
  "A nil :active-env triggers env restore, and the restored durable id resumes.
After an Emacs restart the workspace plist carries no :active-env; the
gui open must restore it from the persisted state (as the vterm boot
does) BEFORE resolving the resume uuid, or the created session starts a
blank conversation."
  ;; Arrange: the stubbed initializer simulates a state-file restore.
  (agent-repl-test--with-ws "ws1" '(:frontend-session-id nil :project-dir "/w")
    (let ((init-calls nil))
      (cl-letf (((symbol-function 'agent-repl--ensure-frontend-daemon) (lambda (&optional _f) t))
                ((symbol-function 'agent-repl--frontend-wait-ready) (lambda () t))
                ((symbol-function 'agent-repl--initialize-ws-env)
                 (lambda (ws &optional dir env-hint)
                   (push (list ws dir env-hint) init-calls)
                   (agent-repl--ws-put ws :active-env :bare-metal)
                   (agent-repl--ws-put ws :bare-metal
                                       (make-agent-repl-instantiation :session-id "restored-uuid")))))
        (agent-repl-test--with-http
            (lambda (method &rest _)
              (if (equal method "GET")
                  (agent-repl-test--json-ok '((sessions . [])))
                (agent-repl-test--json-ok '((session_id . "s_restored")))))
          ;; Act
          (agent-repl--frontend-ensure-session "ws1")
          ;; Assert — restore ran for ws1 with its dir, and the POST
          ;; resumes the uuid the restore installed.
          (should (equal init-calls '(("ws1" "/w" nil))))
          (let ((post (car (last requests))))
            (should (string-match-p "\"resume\":\"restored-uuid\"" (nth 2 post)))))))))

(ert-deftest agent-repl-test-frontend-ensure-session-skips-env-restore-when-present ()
  "An already-initialized workspace must not be re-initialized.
Re-running the initializer while a session is live would clobber the
in-memory instantiation with staler on-disk state."
  ;; Arrange
  (agent-repl-test--with-ws "ws1"
      (list :frontend-session-id nil :project-dir "/w"
            :active-env :bare-metal
            :bare-metal (make-agent-repl-instantiation :session-id "live-uuid"))
    (let ((init-calls 0))
      (cl-letf (((symbol-function 'agent-repl--ensure-frontend-daemon) (lambda (&optional _f) t))
                ((symbol-function 'agent-repl--frontend-wait-ready) (lambda () t))
                ((symbol-function 'agent-repl--initialize-ws-env)
                 (lambda (&rest _args) (cl-incf init-calls))))
        (agent-repl-test--with-http
            (lambda (method &rest _)
              (if (equal method "GET")
                  (agent-repl-test--json-ok '((sessions . [])))
                (agent-repl-test--json-ok '((session_id . "s_new")))))
          ;; Act
          (agent-repl--frontend-ensure-session "ws1")
          ;; Assert
          (should (= init-calls 0)))))))

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

;;;; ---- gui-adopt-session ----------------------------------------------------------

(ert-deftest agent-repl-test-gui-adopt-session-passes-ws-model ()
  "Adopting a durable session carries the workspace's `:model', not a
hardcoded nil.

Regression test for the same bug fixed in
`agent-repl--frontend-ensure-session': `agent-repl--gui-adopt-session'
once passed a literal nil for the model argument regardless of WS, so a
frontend switch that adopted a durable claude session silently dropped
the workspace's model.  Fails against that hardcoded nil, since
`:model' would then never reach the POST body at all."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w" :model "opus")
    (cl-letf (((symbol-function 'agent-repl--ensure-frontend-daemon) (lambda (&optional _f) t))
              ((symbol-function 'agent-repl--frontend-wait-ready) (lambda () t)))
      (agent-repl-test--with-http
          (lambda (&rest _) (agent-repl-test--json-ok '((session_id . "s_new"))))
        ;; Act
        (agent-repl--gui-adopt-session "ws1" "cli-uuid-1")
        ;; Assert
        (let ((post (car (last requests))))
          (should (string-match-p "\"model\":\"opus\"" (nth 2 post))))))))

(ert-deftest agent-repl-test-gui-adopt-session-binds-the-new-session-id ()
  "Adopt binds WS's `:frontend-session-id' to the newly created session."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w")
    (cl-letf (((symbol-function 'agent-repl--ensure-frontend-daemon) (lambda (&optional _f) t))
              ((symbol-function 'agent-repl--frontend-wait-ready) (lambda () t)))
      (agent-repl-test--with-http
          (lambda (&rest _) (agent-repl-test--json-ok '((session_id . "s_adopted"))))
        ;; Act
        (let ((id (agent-repl--gui-adopt-session "ws1" "cli-uuid-1")))
          ;; Assert
          (should (equal id "s_adopted"))
          (should (equal (agent-repl--ws-get "ws1" :frontend-session-id) "s_adopted")))))))

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

(ert-deftest agent-repl-test-frontend-interrupt-session-sends-no-retract-target-by-default ()
  "A plain interrupt names no turn, so the daemon retracts nothing."
  ;; Arrange
  (agent-repl-test--with-http
      (lambda (&rest _) (agent-repl-test--json-ok '((retracted . :false))))
    ;; Act
    (agent-repl--frontend-interrupt-session "s_1")
    ;; Assert
    (pcase-let ((`(,_ ,_ ,payload) (car requests)))
      (should (null payload)))))

(ert-deftest agent-repl-test-frontend-interrupt-session-carries-the-retract-target ()
  "A retracting interrupt names the turn it means to withdraw."
  ;; Arrange
  (agent-repl-test--with-http
      (lambda (&rest _) (agent-repl-test--json-ok '((retracted . t))))
    ;; Act
    (agent-repl--frontend-interrupt-session "s_1" "r_9")
    ;; Assert
    (pcase-let ((`(,_ ,_ ,payload) (car requests)))
      (should (string-match-p "\"retract_request_id\":\"r_9\"" payload)))))

(ert-deftest agent-repl-test-frontend-interrupt-session-reports-a-retraction ()
  "The daemon's `retracted' verdict is what the caller acts on."
  ;; Arrange
  (agent-repl-test--with-http
      (lambda (&rest _) (agent-repl-test--json-ok '((retracted . t))))
    ;; Act / Assert
    (should (agent-repl--frontend-interrupt-session "s_1" "r_9"))))

(ert-deftest agent-repl-test-frontend-interrupt-session-reports-a-refused-retraction ()
  "A turn the daemon declined to retract must not read as retracted."
  ;; Arrange — the agent already answered, so the prompt stays on the feed.
  (agent-repl-test--with-http
      (lambda (&rest _) (agent-repl-test--json-ok '((retracted . :false))))
    ;; Act / Assert
    (should-not (agent-repl--frontend-interrupt-session "s_1" "r_9"))))

(ert-deftest agent-repl-test-frontend-interrupt-session-reports-no-retraction-from-an-old-daemon ()
  "A body with no `retracted' field reads as no retraction, never as one."
  ;; Arrange — a daemon predating the retract route answers 202 with no body.
  (agent-repl-test--with-http
      (lambda (&rest _) (cons 202 ""))
    ;; Act / Assert
    (should-not (agent-repl--frontend-interrupt-session "s_1" "r_9"))))

;;;; ---- in-flight message queue (§2.13) ----------------------------------------

(ert-deftest agent-repl-test-frontend-queue-run-now-posts-route ()
  "Run-now POSTs the session/queue run-now override route."
  ;; Arrange
  (agent-repl-test--with-http
      (lambda (&rest _) (cons 200 ""))
    ;; Act
    (agent-repl--frontend-queue-run-now "s_1" "q_9")
    ;; Assert
    (pcase-let ((`(,method ,url ,_) (car requests)))
      (should (equal method "POST"))
      (should (string-suffix-p "/sessions/s_1/queue/q_9/run-now" url)))))

(ert-deftest agent-repl-test-frontend-queue-cancel-posts-route ()
  "Cancel POSTs the session/queue cancel override route."
  ;; Arrange
  (agent-repl-test--with-http
      (lambda (&rest _) (cons 200 ""))
    ;; Act
    (agent-repl--frontend-queue-cancel "s_1" "q_9")
    ;; Assert
    (pcase-let ((`(,method ,url ,_) (car requests)))
      (should (equal method "POST"))
      (should (string-suffix-p "/sessions/s_1/queue/q_9/cancel" url)))))

(ert-deftest agent-repl-test-frontend-queue-preview-joins-text-blocks ()
  "The content preview concatenates the text of text blocks."
  ;; Arrange / Act / Assert
  (should (equal (agent-repl--frontend-queue-content-preview
                  '(((type . "text") (text . "hello"))
                    ((type . "text") (text . "world"))))
                 "hello world")))

(ert-deftest agent-repl-test-frontend-queue-preview-truncates ()
  "A preview beyond the length cap is truncated with an ellipsis."
  ;; Arrange
  (let ((agent-repl-queue-preview-length 5))
    ;; Act / Assert
    (should (equal (agent-repl--frontend-queue-content-preview
                    '(((type . "text") (text . "abcdefgh"))))
                   "abcde…"))))

(ert-deftest agent-repl-test-frontend-queue-preview-ignores-non-text ()
  "Non-text content blocks contribute nothing to the preview."
  ;; Arrange / Act / Assert
  (should (equal (agent-repl--frontend-queue-content-preview
                  '(((type . "tool_use") (name . "Bash") (input . "ls"))))
                 "")))

(ert-deftest agent-repl-test-frontend-session-queue-extracts-items ()
  "The parser lifts each queue item into a plist with the §2.13 fields."
  ;; Arrange
  (let* ((entry '((session_id . "s_1")
                  (queue . (((queue_id . "q_1") (request_id . "r_1")
                             (status . "waiting") (verdict . "wait")
                             (content . (((type . "text") (text . "first"))))))))))
    ;; Act
    (let ((items (agent-repl--frontend-session-queue entry)))
      ;; Assert
      (should (= 1 (length items)))
      (let ((it (car items)))
        (should (equal (plist-get it :queue-id) "q_1"))
        (should (equal (plist-get it :status) "waiting"))
        (should (equal (plist-get it :verdict) "wait"))
        (should (equal (plist-get it :content-preview) "first"))))))

(ert-deftest agent-repl-test-frontend-session-queue-nil-without-queue ()
  "An entry carrying no `queue' array parses to an empty queue."
  ;; Arrange / Act / Assert
  (should (null (agent-repl--frontend-session-queue '((session_id . "s_1"))))))

(ert-deftest agent-repl-test-frontend-capture-queues-stores-for-bound-ws ()
  "Capture stores the parsed queue under a bound workspace's :queued-messages."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:frontend-session-id "s_1")
    (let ((sessions '(((session_id . "s_1")
                       (queue . (((queue_id . "q_1") (status . "waiting")
                                  (content . (((type . "text") (text . "hi")))))))))))
      ;; Act
      (agent-repl--frontend-capture-queues sessions)
      ;; Assert
      (let ((q (agent-repl--ws-get "ws1" :queued-messages)))
        (should (= 1 (length q)))
        (should (equal (plist-get (car q) :queue-id) "q_1"))))))

(ert-deftest agent-repl-test-frontend-capture-queues-stores-async-live ()
  "Capture stores the entry's async_live count under :async-live."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:frontend-session-id "s_1")
    (let ((sessions '(((session_id . "s_1") (async_live . 3)))))
      ;; Act
      (agent-repl--frontend-capture-queues sessions)
      ;; Assert
      (should (= 3 (agent-repl--ws-get "ws1" :async-live))))))

(ert-deftest agent-repl-test-frontend-capture-queues-clears-when-absent ()
  "Capture clears :queued-messages when the bound session is not listed."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:frontend-session-id "s_gone"
                                    :queued-messages ((:queue-id "q_old")))
    ;; Act
    (agent-repl--frontend-capture-queues '(((session_id . "s_other"))))
    ;; Assert
    (should (null (agent-repl--ws-get "ws1" :queued-messages)))))

(ert-deftest agent-repl-test-ws-queued-messages-reads-plist ()
  "The messages accessor returns the stored queue list verbatim."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:queued-messages ((:queue-id "q_1")))
    ;; Act / Assert
    (should (equal (agent-repl--ws-queued-messages "ws1") '((:queue-id "q_1"))))))

(ert-deftest agent-repl-test-ws-queued-count-counts-items ()
  "The count accessor returns the number of queued items."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:queued-messages ((:queue-id "q_1") (:queue-id "q_2")))
    ;; Act / Assert
    (should (= 2 (agent-repl--ws-queued-count "ws1")))))

(ert-deftest agent-repl-test-ws-queued-count-zero-when-unset ()
  "The count accessor returns 0 when no queue has been captured."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w")
    ;; Act / Assert
    (should (= 0 (agent-repl--ws-queued-count "ws1")))))

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

(ert-deftest agent-repl-test-frontend-send-user-message-syncs-webview ()
  "The send path remounts the webview onto the ensured (possibly healed) session."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w")
    (let ((synced nil))
      (cl-letf (((symbol-function 'agent-repl--frontend-ensure-session)
                 (lambda (_ws) "s_healed"))
                ((symbol-function 'agent-repl--frontend-sync-webview)
                 (lambda (ws id) (setq synced (list ws id))))
                ((symbol-function 'agent-repl--frontend-send-message)
                 (lambda (_id _text) "r_1")))
        ;; Act
        (agent-repl--frontend-send-user-message "ws1" "hi")
        ;; Assert
        (should (equal synced '("ws1" "s_healed")))))))

;;;; ---- turn-active probe ------------------------------------------------------

(ert-deftest agent-repl-test-frontend-bound-session-ids-collects-live-workspaces ()
  "The bound-id set gathers every live workspace's :frontend-session-id.
A workspace without a binding contributes nothing."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:frontend-session-id "s_1" :project-dir "/w")
    (agent-repl-test--with-ws "ws2" '(:frontend-session-id "s_2" :project-dir "/w2")
      (agent-repl-test--with-ws "ws3" '(:project-dir "/w3")
        ;; Act / Assert
        (should (equal (sort (copy-sequence (agent-repl--frontend-bound-session-ids))
                             #'string<)
                       '("s_1" "s_2")))))))

(ert-deftest agent-repl-test-frontend-turn-active-sessions-extracts-busy-ids ()
  "A bound session with turn_active true is returned, an idle one skipped."
  ;; Arrange
  (cl-letf (((symbol-function 'agent-repl--frontend-bound-session-ids)
             (lambda () '("s_busy" "s_idle"))))
    (agent-repl-test--with-http
        (lambda (&rest _)
          (agent-repl-test--json-ok
           `((sessions . [,(list '(session_id . "s_busy") '(turn_active . t))
                          ,(list '(session_id . "s_idle") '(turn_active . :json-false))]))))
      ;; Act / Assert
      (should (equal (agent-repl--frontend-turn-active-sessions) '("s_busy"))))))

(ert-deftest agent-repl-test-frontend-turn-active-sessions-skips-terminal ()
  "A terminal session is never counted busy, even bound and turn_active true."
  ;; Arrange
  (cl-letf (((symbol-function 'agent-repl--frontend-bound-session-ids)
             (lambda () '("s_zombie" "s_live"))))
    (agent-repl-test--with-http
        (lambda (&rest _)
          (agent-repl-test--json-ok
           `((sessions . [,(list '(session_id . "s_zombie") '(turn_active . t) '(terminal . t))
                          ,(list '(session_id . "s_live") '(turn_active . t) '(terminal . :false))]))))
      ;; Act / Assert
      (should (equal (agent-repl--frontend-turn-active-sessions) '("s_live"))))))

(ert-deftest agent-repl-test-frontend-turn-active-sessions-skips-unbound-orphan ()
  "A turn_active session no live workspace is bound to is never counted.
This is the orphan a prior bounce leaves behind with `turn_active' stuck
true — it must not block a future bounce."
  ;; Arrange
  (cl-letf (((symbol-function 'agent-repl--frontend-bound-session-ids)
             (lambda () '("s_bound"))))
    (agent-repl-test--with-http
        (lambda (&rest _)
          (agent-repl-test--json-ok
           `((sessions . [,(list '(session_id . "s_bound") '(turn_active . t) '(terminal . :false))
                          ,(list '(session_id . "s_orphan") '(turn_active . t) '(terminal . :false))]))))
      ;; Act / Assert
      (should (equal (agent-repl--frontend-turn-active-sessions) '("s_bound"))))))

(ert-deftest agent-repl-test-frontend-turn-active-sessions-nil-when-unreachable ()
  "An unreachable daemon reads as no turns (nothing to protect)."
  ;; Arrange
  (agent-repl-test--with-http
      (lambda (&rest _) (error "connection refused"))
    ;; Act / Assert
    (should (null (agent-repl--frontend-turn-active-sessions)))))

;;;; ---- orphan-session reaper --------------------------------------------------

(ert-deftest agent-repl-test-frontend-orphan-ids-selects-superseded-duplicate ()
  "A live-shim session bound nowhere, duplicating a bound conversation, is a target."
  ;; Arrange — s_new is bound; s_old is the same claude id, live, unbound.
  (cl-letf (((symbol-function 'agent-repl--frontend-bound-session-ids)
             (lambda () '("s_new"))))
    (let ((sessions
           `(,(list '(session_id . "s_new") '(claude_session_id . "c1"))
             ,(list '(session_id . "s_old") '(claude_session_id . "c1")))))
      ;; Act / Assert
      (should (equal (agent-repl--frontend-orphan-session-ids sessions) '("s_old"))))))

(ert-deftest agent-repl-test-frontend-orphan-ids-spares-rehydratable ()
  "A rehydratable (cold) duplicate is spared: it has no shim to leak."
  ;; Arrange
  (cl-letf (((symbol-function 'agent-repl--frontend-bound-session-ids)
             (lambda () '("s_new"))))
    (let ((sessions
           `(,(list '(session_id . "s_new") '(claude_session_id . "c1"))
             ,(list '(session_id . "s_cold") '(claude_session_id . "c1") '(rehydratable . t)))))
      ;; Act / Assert
      (should (null (agent-repl--frontend-orphan-session-ids sessions))))))

(ert-deftest agent-repl-test-frontend-orphan-ids-spares-hibernated ()
  "A hibernated duplicate is spared: its shim is already freed."
  ;; Arrange
  (cl-letf (((symbol-function 'agent-repl--frontend-bound-session-ids)
             (lambda () '("s_new"))))
    (let ((sessions
           `(,(list '(session_id . "s_new") '(claude_session_id . "c1"))
             ,(list '(session_id . "s_hib") '(claude_session_id . "c1") '(hibernated . t)))))
      ;; Act / Assert
      (should (null (agent-repl--frontend-orphan-session-ids sessions))))))

(ert-deftest agent-repl-test-frontend-orphan-ids-spares-unique-unbound ()
  "An unbound live session duplicating NO bound conversation is left alone."
  ;; Arrange
  (cl-letf (((symbol-function 'agent-repl--frontend-bound-session-ids)
             (lambda () '("s_new"))))
    (let ((sessions
           `(,(list '(session_id . "s_new") '(claude_session_id . "c1"))
             ,(list '(session_id . "s_solo") '(claude_session_id . "c2")))))
      ;; Act / Assert
      (should (null (agent-repl--frontend-orphan-session-ids sessions))))))

(ert-deftest agent-repl-test-frontend-reap-deletes-orphans ()
  "The reaper issues a DELETE for each orphan id and returns them."
  ;; Arrange
  (let ((deleted nil))
    (cl-letf (((symbol-function 'agent-repl--frontend-orphan-session-ids)
               (lambda (_sessions) '("s_old")))
              ((symbol-function 'agent-repl--frontend-delete-session)
               (lambda (id) (push id deleted) t)))
      ;; Act
      (let ((reaped (agent-repl--frontend-reap-orphan-sessions '())))
        ;; Assert
        (should (equal reaped '("s_old")))
        (should (equal deleted '("s_old")))))))

(ert-deftest agent-repl-test-frontend-reap-skips-failed-delete ()
  "A failed DELETE is skipped rather than fatal, and excluded from the reaped list."
  ;; Arrange
  (cl-letf (((symbol-function 'agent-repl--frontend-orphan-session-ids)
             (lambda (_sessions) '("s_bad" "s_ok")))
            ((symbol-function 'agent-repl--frontend-delete-session)
             (lambda (id) (if (equal id "s_bad") (error "boom") t))))
    ;; Act
    (let ((reaped (agent-repl--frontend-reap-orphan-sessions '())))
      ;; Assert
      (should (equal reaped '("s_ok"))))))

;;;; ---- reattach loop -----------------------------------------------------------

(ert-deftest agent-repl-test-frontend-reattach-check-no-op-when-listed ()
  "A binding the daemon still lists is left alone and its markers clear."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:frontend-session-id "s_1" :reattach-failed t
                                    :reattach-failures 2 :project-dir "/w")
    (let ((reattached nil)
          (agent-repl--frontend-last-boot-id nil))
      (cl-letf (((symbol-function 'agent-repl--frontend-api)
                 (lambda (&rest _) '((boot_id . "b_1") (sessions . (((session_id . "s_1")))))))
                ((symbol-function 'agent-repl--frontend-reattach-ws)
                 (lambda (&rest args) (push args reattached))))
        ;; Act
        (agent-repl--frontend-reattach-check)
        ;; Assert
        (should (null reattached))
        (should-not (agent-repl--ws-get "ws1" :reattach-failed))
        (should-not (agent-repl--ws-get "ws1" :reattach-failures))))))

(ert-deftest agent-repl-test-frontend-reattach-check-reattaches-vanished ()
  "A binding missing from the daemon's list triggers a reattach."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:frontend-session-id "s_gone" :project-dir "/w")
    (let ((reattached nil)
          (agent-repl--frontend-last-boot-id nil))
      (cl-letf (((symbol-function 'agent-repl--frontend-api)
                 (lambda (&rest _) '((boot_id . "b_1") (sessions . (((session_id . "s_other")))))))
                ((symbol-function 'agent-repl--frontend-reattach-ws)
                 (lambda (ws stale) (push (list ws stale) reattached))))
        ;; Act
        (agent-repl--frontend-reattach-check)
        ;; Assert
        (should (equal reattached '(("ws1" "s_gone"))))))))

(ert-deftest agent-repl-test-frontend-reattach-check-skips-given-up-workspaces ()
  "A workspace marked :reattach-failed is not retried."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:frontend-session-id "s_gone" :reattach-failed t
                                    :project-dir "/w")
    (let ((reattached nil)
          (agent-repl--frontend-last-boot-id nil))
      (cl-letf (((symbol-function 'agent-repl--frontend-api)
                 (lambda (&rest _) '((boot_id . "b_1") (sessions . ()))))
                ((symbol-function 'agent-repl--frontend-reattach-ws)
                 (lambda (&rest args) (push args reattached))))
        ;; Act
        (agent-repl--frontend-reattach-check)
        ;; Assert
        (should (null reattached))))))

(ert-deftest agent-repl-test-frontend-reattach-check-ensures-daemon-when-unreachable ()
  "Unreachable daemon with live bindings triggers a daemon ensure."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:frontend-session-id "s_1" :project-dir "/w")
    (let ((ensured nil))
      (cl-letf (((symbol-function 'agent-repl--frontend-api)
                 (lambda (&rest _) (error "connection refused")))
                ((symbol-function 'agent-repl--ensure-frontend-daemon)
                 (lambda (&optional _f) (setq ensured t))))
        ;; Act
        (agent-repl--frontend-reattach-check)
        ;; Assert
        (should ensured)))))

(ert-deftest agent-repl-test-frontend-note-boot-id-first-observation-sets ()
  "The first boot id observation records without resetting anything."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:frontend-session-id "s_1" :reattach-failed t
                                    :project-dir "/w")
    (let ((agent-repl--frontend-last-boot-id nil))
      ;; Act
      (agent-repl--frontend-note-boot-id "b_first")
      ;; Assert — recorded, but no give-up reset on first sight.
      (should (equal agent-repl--frontend-last-boot-id "b_first"))
      (should (agent-repl--ws-get "ws1" :reattach-failed)))))

(ert-deftest agent-repl-test-frontend-note-boot-id-change-resets-give-ups ()
  "A boot id change resets :reattach-failed give-ups across workspaces."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:frontend-session-id "s_1" :reattach-failed t
                                    :reattach-failures 3 :project-dir "/w")
    (let ((agent-repl--frontend-last-boot-id "b_old"))
      ;; Act
      (agent-repl--frontend-note-boot-id "b_new")
      ;; Assert
      (should (equal agent-repl--frontend-last-boot-id "b_new"))
      (should-not (agent-repl--ws-get "ws1" :reattach-failed))
      (should-not (agent-repl--ws-get "ws1" :reattach-failures)))))

(ert-deftest agent-repl-test-frontend-note-boot-id-nil-never-resets ()
  "A pre-boot-id daemon (nil boot id) neither records nor resets."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:frontend-session-id "s_1" :reattach-failed t
                                    :project-dir "/w")
    (let ((agent-repl--frontend-last-boot-id "b_old"))
      ;; Act
      (agent-repl--frontend-note-boot-id nil)
      ;; Assert
      (should (equal agent-repl--frontend-last-boot-id "b_old"))
      (should (agent-repl--ws-get "ws1" :reattach-failed)))))

(ert-deftest agent-repl-test-frontend-reattach-ws-success-remounts ()
  "A successful reattach re-ensures, remounts, and clears counters."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:frontend-session-id "s_gone"
                                    :reattach-failures 2 :project-dir "/w")
    (let ((synced nil))
      (cl-letf (((symbol-function 'agent-repl--frontend-ensure-session)
                 (lambda (_ws) "s_new"))
                ((symbol-function 'agent-repl--frontend-sync-webview)
                 (lambda (ws id) (setq synced (list ws id)))))
        ;; Act
        (agent-repl--frontend-reattach-ws "ws1" "s_gone")
        ;; Assert
        (should (equal synced '("ws1" "s_new")))
        (should-not (agent-repl--ws-get "ws1" :reattach-failures))))))

(ert-deftest agent-repl-test-frontend-reattach-ws-failure-restores-binding ()
  "A failed reattach restores the stale binding and counts the failure."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:frontend-session-id "s_gone" :project-dir "/w")
    (cl-letf (((symbol-function 'agent-repl--frontend-ensure-session)
               (lambda (_ws) (error "boom"))))
      ;; Act
      (agent-repl--frontend-reattach-ws "ws1" "s_gone")
      ;; Assert — binding restored so the next sweep retries.
      (should (equal (agent-repl--ws-get "ws1" :frontend-session-id) "s_gone"))
      (should (= (agent-repl--ws-get "ws1" :reattach-failures) 1))
      (should-not (agent-repl--ws-get "ws1" :reattach-failed)))))

(ert-deftest agent-repl-test-frontend-reattach-ws-gives-up-at-cap ()
  "Reaching the failure cap sets :reattach-failed and warns."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" (list :frontend-session-id "s_gone"
                                        :reattach-failures
                                        (1- agent-repl-frontend-reattach-max-failures)
                                        :project-dir "/w")
    (let ((warned nil))
      (cl-letf (((symbol-function 'agent-repl--frontend-ensure-session)
                 (lambda (_ws) (error "boom")))
                ((symbol-function 'display-warning)
                 (lambda (type msg &rest _) (setq warned (cons type msg)))))
        ;; Act
        (agent-repl--frontend-reattach-ws "ws1" "s_gone")
        ;; Assert
        (should (agent-repl--ws-get "ws1" :reattach-failed))
        (should (eq (car warned) 'agent-repl))
        (should (string-match-p "ws1" (cdr warned)))))))

(ert-deftest agent-repl-test-frontend-reattach-timer-inhibited-in-batch ()
  "The sweep timer does not start when init is inhibited."
  ;; Arrange
  (let ((agent-repl--frontend-reattach-timer nil)
        (armed nil))
    (cl-letf (((symbol-function 'agent-repl--frontend-init-inhibited-p) (lambda () t))
              ((symbol-function 'run-with-timer)
               (lambda (&rest _args) (setq armed t) 'fake-timer)))
      ;; Act
      (agent-repl--frontend-reattach-timer-start)
      ;; Assert
      (should-not armed)
      (should-not agent-repl--frontend-reattach-timer))))

(ert-deftest agent-repl-test-frontend-reattach-timer-starts-once ()
  "The sweep timer arms exactly once across repeated starts."
  ;; Arrange
  (let ((agent-repl--frontend-reattach-timer nil)
        (armed 0))
    (cl-letf (((symbol-function 'agent-repl--frontend-init-inhibited-p) (lambda () nil))
              ((symbol-function 'run-with-timer)
               (lambda (&rest _args) (cl-incf armed) 'fake-timer)))
      ;; Act
      (agent-repl--frontend-reattach-timer-start)
      (agent-repl--frontend-reattach-timer-start)
      ;; Assert
      (should (= armed 1))
      (should (eq agent-repl--frontend-reattach-timer 'fake-timer)))))

;;;; ---- rebind after restart -------------------------------------------------------

(ert-deftest agent-repl-test-frontend-rebind-waits-ready-before-reattach ()
  "The rebind waits for the daemon to answer BEFORE driving the reattach.
Order matters: `agent-repl--frontend-reattach-check' treats an unreachable
daemon as \"nothing to reattach\", so probing before readiness would skip
every workspace."
  ;; Arrange
  (let ((calls nil))
    (cl-letf (((symbol-function 'agent-repl--frontend-wait-ready)
               (lambda () (push 'wait calls) t))
              ((symbol-function 'agent-repl--live-ws-names) (lambda () nil))
              ((symbol-function 'agent-repl--frontend-reattach-check)
               (lambda () (push 'reattach calls))))
      ;; Act
      (agent-repl--frontend-rebind-workspaces-after-restart)
      ;; Assert
      (should (equal (reverse calls) '(wait reattach))))))

(ert-deftest agent-repl-test-frontend-rebind-remounts-all-after-reattach ()
  "The rebind force-remounts every open webview, and only after the reattach.
The reattach must rebind sessions first; the unconditional remount then
guarantees each webview reloads the freshly built bundle."
  ;; Arrange
  (let ((calls nil))
    (cl-letf (((symbol-function 'agent-repl--frontend-wait-ready) (lambda () t))
              ((symbol-function 'agent-repl--live-ws-names) (lambda () nil))
              ((symbol-function 'agent-repl--frontend-reattach-check)
               (lambda () (push 'reattach calls)))
              ((symbol-function 'agent-repl--frontend-remount-all-webviews)
               (lambda () (push 'remount-all calls) 0)))
      ;; Act
      (agent-repl--frontend-rebind-workspaces-after-restart)
      ;; Assert
      (should (equal (reverse calls) '(reattach remount-all))))))

(ert-deftest agent-repl-test-frontend-rebind-delegates-to-reattach-check ()
  "The rebind drives the same sweep machinery that bounces and remounts."
  ;; Arrange
  (let ((reattached nil))
    (cl-letf (((symbol-function 'agent-repl--frontend-wait-ready) (lambda () t))
              ((symbol-function 'agent-repl--live-ws-names) (lambda () nil))
              ((symbol-function 'agent-repl--frontend-reattach-check)
               (lambda () (setq reattached t))))
      ;; Act
      (agent-repl--frontend-rebind-workspaces-after-restart)
      ;; Assert
      (should reattached))))

(ert-deftest agent-repl-test-frontend-rebind-returns-bound-workspace-count ()
  "The rebind returns how many open workspaces carried a session binding."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:frontend-session-id "s_1" :project-dir "/w")
    (agent-repl-test--with-ws "ws2" '(:frontend-session-id "s_2" :project-dir "/w2")
      (agent-repl-test--with-ws "ws3" '(:project-dir "/w3")
        (cl-letf (((symbol-function 'agent-repl--frontend-wait-ready) (lambda () t))
                  ((symbol-function 'agent-repl--frontend-reattach-check) #'ignore))
          ;; Act / Assert — ws1 and ws2 are bound, ws3 is not.
          (should (= 2 (agent-repl--frontend-rebind-workspaces-after-restart))))))))

(ert-deftest agent-repl-test-frontend-rebind-returns-zero-without-bindings ()
  "The rebind returns 0 when no open workspace carries a session binding."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w")
    (cl-letf (((symbol-function 'agent-repl--frontend-wait-ready) (lambda () t))
              ((symbol-function 'agent-repl--frontend-reattach-check) #'ignore))
      ;; Act / Assert
      (should (= 0 (agent-repl--frontend-rebind-workspaces-after-restart))))))

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

(ert-deftest agent-repl-test-gui-send-turn-keeps-meta-markers ()
  "gui-send-turn posts the marked text VERBATIM to the daemon.
The webapp hides the bracketed spans at render time, so stripping them on
the wire would deprive the agent of the directive it must read."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:frontend-session-id "s_1" :project-dir "/w")
    (let ((sent nil)
          (input (concat (agent-repl--meta-wrap "READ-DIRECTIVE") "\n\nhello")))
      (cl-letf (((symbol-function 'agent-repl--frontend-send-user-message)
                 (lambda (_ws text) (setq sent text)))
                ((symbol-function 'agent-repl--increment-prefix-counter) #'ignore)
                ((symbol-function 'agent-repl--run-send-posthooks) #'ignore)
                ((symbol-function 'agent-repl--kickoff-prompt-summary) #'ignore))
        ;; Act
        (agent-repl--gui-send-turn "ws1" input "hello")
        ;; Assert
        (should (equal sent input))))))

;;;; ---- session-url ---------------------------------------------------------------

(ert-deftest agent-repl-test-frontend-session-url-shape ()
  "The webapp attach URL carries the session query param."
  ;; Arrange
  (let ((agent-repl-frontend-daemon-addr "127.0.0.1:9999"))
    ;; Act / Assert
    (should (equal (agent-repl--frontend-session-url "s_9")
                   "http://127.0.0.1:9999/?session=s_9"))))

;;;; ---- slash commands -------------------------------------------------------

(ert-deftest agent-repl-test-frontend-fetch-commands-gets-the-menu ()
  "Fetch GETs the session's commands endpoint and returns the list."
  ;; Arrange
  (agent-repl-test--with-http
      (lambda (&rest _)
        (agent-repl-test--json-ok
         '((commands . (((name . "debug-logs") (description . "d") (argumentHint . "")))))))
    ;; Act
    (let ((cmds (agent-repl--frontend-fetch-commands "s1")))
      ;; Assert
      (should (equal (alist-get 'name (car cmds)) "debug-logs"))
      (pcase-let ((`(,method ,url ,_payload) (car requests)))
        (should (equal method "GET"))
        (should (string-suffix-p "/sessions/s1/commands" url))))))

(ert-deftest agent-repl-test-frontend-fetch-commands-empty-menu ()
  "An unresolved menu (the daemon's `{\"commands\":[]}') is returned as nil,
not an error."
  ;; Arrange — feed the daemon's literal empty-array body, since that is
  ;; exactly what an unresolved menu serializes to (never JSON null).
  (agent-repl-test--with-http
      (lambda (&rest _) (cons 200 "{\"commands\":[]}"))
    ;; Act / Assert
    (should (null (agent-repl--frontend-fetch-commands "s1")))))

(ert-deftest agent-repl-test-frontend-refresh-commands-posts-to-refresh ()
  "Refresh POSTs to the session's commands/refresh endpoint."
  ;; Arrange
  (agent-repl-test--with-http
      (lambda (&rest _) (cons 202 ""))
    ;; Act
    (agent-repl--frontend-refresh-commands "s1")
    ;; Assert
    (pcase-let ((`(,method ,url ,_payload) (car requests)))
      (should (equal method "POST"))
      (should (string-suffix-p "/sessions/s1/commands/refresh" url)))))

(ert-deftest agent-repl-test-frontend-refresh-commands-errors-on-non-2xx ()
  "A refresh that the daemon rejects signals rather than reporting success."
  ;; Arrange
  (agent-repl-test--with-http
      (lambda (&rest _) (cons 404 "no such session"))
    ;; Act / Assert
    (should-error (agent-repl--frontend-refresh-commands "s1"))))

(provide 'test-frontend-client)

;;; test-frontend-client.el ends here

;;;; ---- gui interrupt: gesture intent and retraction ---------------------------

(defmacro agent-repl-test--with-interrupt (ws-plist retracted &rest body)
  "Run BODY with workspace \"ws1\" carrying WS-PLIST and the interrupt route
answering RETRACTED.  Binds `sent-to' to the (SESSION-ID . RETRACT-ID) the
frontend asked for."
  (declare (indent 2))
  `(let ((sent-to nil))
     (ignore sent-to)
     (agent-repl-test--with-ws "ws1" ,ws-plist
       (cl-letf (((symbol-function 'agent-repl--frontend-interrupt-session)
                  (lambda (id &optional retract-id)
                    (setq sent-to (cons id retract-id))
                    ,retracted)))
         ,@body))))

(ert-deftest agent-repl-test-gui-interrupt-escape-asks-to-retract-the-sent-turn ()
  "C-c C-k means undo, so it names the turn it wants withdrawn."
  ;; Arrange
  (agent-repl-test--with-interrupt
      '(:frontend-session-id "s_1" :sent-turn (:request-id "r_9" :raw "draft")) t
    ;; Act
    (agent-repl--gui-interrupt "ws1" 'escape)
    ;; Assert
    (should (equal sent-to '("s_1" . "r_9")))))

(ert-deftest agent-repl-test-gui-interrupt-escape-reports-a-retraction ()
  "A withdrawn prompt is reported so the caller knows to restore it."
  ;; Arrange
  (agent-repl-test--with-interrupt
      '(:frontend-session-id "s_1" :sent-turn (:request-id "r_9" :raw "draft")) t
    ;; Act / Assert
    (should (eq (agent-repl--gui-interrupt "ws1" 'escape) 'retracted))))

(ert-deftest agent-repl-test-gui-interrupt-escape-reports-a-plain-stop ()
  "A turn the daemon kept is an ordinary interrupt, not an undo."
  ;; Arrange — the agent already answered.
  (agent-repl-test--with-interrupt
      '(:frontend-session-id "s_1" :sent-turn (:request-id "r_9" :raw "draft")) nil
    ;; Act / Assert — non-nil (delivered) but never `retracted'.
    (should (eq (agent-repl--gui-interrupt "ws1" 'escape) t))))

(ert-deftest agent-repl-test-gui-interrupt-ctrl-c-never-retracts ()
  "C-c C-c has just discarded the draft, so it must not hand a prompt back."
  ;; Arrange
  (agent-repl-test--with-interrupt
      '(:frontend-session-id "s_1" :sent-turn (:request-id "r_9" :raw "draft")) t
    ;; Act
    (agent-repl--gui-interrupt "ws1" 'ctrl-c)
    ;; Assert — the gesture names no turn, so the daemon withdraws nothing.
    (should (equal sent-to '("s_1" . nil)))))

(ert-deftest agent-repl-test-gui-interrupt-escape-without-a-sent-turn-names-none ()
  "With no send on record there is nothing to undo."
  ;; Arrange
  (agent-repl-test--with-interrupt '(:frontend-session-id "s_1") nil
    ;; Act
    (agent-repl--gui-interrupt "ws1" 'escape)
    ;; Assert
    (should (equal sent-to '("s_1" . nil)))))

(ert-deftest agent-repl-test-gui-send-turn-records-the-sent-turn ()
  "The send records what an undo of it would need: the id and the RAW text."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:frontend-session-id "s_1")
    (cl-letf (((symbol-function 'agent-repl--frontend-send-user-message)
               (lambda (&rest _) "r_9"))
              ((symbol-function 'agent-repl--mark-ws-thinking) #'ignore)
              ((symbol-function 'agent-repl--increment-prefix-counter) #'ignore)
              ((symbol-function 'agent-repl--run-send-posthooks) #'ignore)
              ((symbol-function 'agent-repl--kickoff-prompt-summary) #'ignore))
      ;; Act — the prepared text carries decoration the user never typed.
      (agent-repl--gui-send-turn "ws1" "META\n\nwrite a test" "write a test")
      ;; Assert — RAW is recorded, since the decoration is not the user's to revise.
      (should (equal (agent-repl--ws-get "ws1" :sent-turn)
                     '(:request-id "r_9" :raw "write a test"))))))
