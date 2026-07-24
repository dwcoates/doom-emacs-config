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

;;;; ---- UDS command capture helpers -------------------------------------------
;;
;; The session-CRUD/prompt/interrupt paths were migrated off HTTP onto the
;; frontend.v1 UDS command channel (S7).  These shadow that boundary
;; (`agent-repl--uds-send-command'/`--uds-track-command') so no real socket
;; fires; commands accumulate in the anaphoric `uds-commands' as
;; \(FIELD PAYLOAD WORKSPACE) lists, newest last.

(defmacro agent-repl-test--with-uds (&rest body)
  "Run BODY with the UDS command boundary shadowed (capture only).
Suitable for submitPrompt/interrupt/deleteSession, which do not await."
  (declare (indent 0))
  `(let ((uds-commands '()) (uds-counter 0))
     (ignore uds-commands)
     (cl-letf (((symbol-function 'agent-repl--uds-send-command)
                (lambda (field payload &optional workspace &rest _)
                  (setq uds-commands
                        (append uds-commands (list (list field payload workspace))))
                  (format "req-%d" (cl-incf uds-counter))))
               ((symbol-function 'agent-repl--uds-track-command)
                (lambda (request-id &rest _) request-id)))
       ,@body)))

(defmacro agent-repl-test--with-uds-create (outcome &rest body)
  "Run BODY with the UDS boundary mocked so `createSession' resolves via OUTCOME.
OUTCOME evaluates to a plist: (:id STRING) simulates the daemon storing a
SessionView for the command's cwd and succeeding the ack (create returns
that id); (:error STRING) fails the ack with that error.  Other commands
are captured into `uds-commands'.  `agent-repl--frontend-await-uds' is
reduced to a single predicate evaluation (the mock resolves synchronously),
and the SessionView store is cleared first so tests do not contaminate."
  (declare (indent 1))
  `(let ((uds-commands '()) (uds-counter 0) (uds-outcome ,outcome))
     (ignore uds-commands)
     (clrhash agent-repl--frontend-session-views)
     (cl-letf (((symbol-function 'agent-repl--uds-send-command)
                (lambda (field payload &optional workspace &rest _)
                  (setq uds-commands
                        (append uds-commands (list (list field payload workspace))))
                  (format "req-%d" (cl-incf uds-counter))))
               ((symbol-function 'agent-repl--uds-track-command)
                (lambda (request-id field workspace &optional on-failure on-success)
                  (when (equal field "createSession")
                    (if (plist-get uds-outcome :error)
                        (when on-failure (funcall on-failure (plist-get uds-outcome :error)))
                      (agent-repl--frontend-store-session-view
                       (list :sessionId (plist-get uds-outcome :id) :workspace workspace))
                      (when on-success (funcall on-success))))
                  request-id))
               ((symbol-function 'agent-repl--frontend-await-uds)
                (lambda (predicate &rest _) (funcall predicate))))
       ,@body)))

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

;;;; ---- create (UDS `createSession') -----------------------------------------

(defmacro agent-repl-test--created-payload ()
  "Return the payload plist of the single captured `createSession' command.
A macro (not a defun) so it captures the lexical `uds-commands' at the call
site inside `agent-repl-test--with-uds-create'."
  '(nth 1 (car uds-commands)))

(ert-deftest agent-repl-test-frontend-create-sends-cwd-payload ()
  "Create sends a `createSession' command carrying the cwd, and returns the
id the daemon delivers on the pushed SessionView."
  ;; Arrange
  (agent-repl-test--with-uds-create '(:id "s_1")
    ;; Act
    (let ((id (agent-repl--frontend-create-session "/w/tree")))
      ;; Assert
      (should (equal id "s_1"))
      (pcase-let ((`(,field ,payload ,ws) (car uds-commands)))
        (should (equal field "createSession"))
        (should (equal (plist-get payload :cwd) "/w/tree"))
        (should (equal ws "/w/tree"))))))

(ert-deftest agent-repl-test-frontend-create-passes-model-and-resume ()
  "Optional model/resume land in the command payload when given."
  ;; Arrange
  (agent-repl-test--with-uds-create '(:id "s_1")
    ;; Act
    (agent-repl--frontend-create-session "/w" "haiku" "cli-uuid-9")
    ;; Assert
    (let ((payload (agent-repl-test--created-payload)))
      (should (equal (plist-get payload :model) "haiku"))
      (should (equal (plist-get payload :resumeClaudeSessionId) "cli-uuid-9")))))

(ert-deftest agent-repl-test-frontend-create-defaults-model-to-interactive ()
  "A create with no model sends `agent-repl-interactive-model'."
  ;; Arrange
  (agent-repl-test--with-uds-create '(:id "s_1")
    (let ((agent-repl-interactive-model "opus"))
      ;; Act
      (agent-repl--frontend-create-session "/w")
      ;; Assert
      (should (equal (plist-get (agent-repl-test--created-payload) :model) "opus")))))

(ert-deftest agent-repl-test-frontend-create-omits-model-when-interactive-nil ()
  "A nil `agent-repl-interactive-model' sends no model field."
  ;; Arrange
  (agent-repl-test--with-uds-create '(:id "s_1")
    (let ((agent-repl-interactive-model nil))
      ;; Act
      (agent-repl--frontend-create-session "/w")
      ;; Assert
      (should-not (plist-member (agent-repl-test--created-payload) :model)))))

(ert-deftest agent-repl-test-frontend-create-explicit-model-overrides-interactive ()
  "An explicit MODEL wins over `agent-repl-interactive-model'."
  ;; Arrange
  (agent-repl-test--with-uds-create '(:id "s_1")
    (let ((agent-repl-interactive-model "opus"))
      ;; Act
      (agent-repl--frontend-create-session "/w" "haiku")
      ;; Assert
      (should (equal (plist-get (agent-repl-test--created-payload) :model) "haiku")))))

(ert-deftest agent-repl-test-frontend-create-sends-permission-mode ()
  "Create carries the configured permission mode (default auto)."
  ;; Arrange
  (agent-repl-test--with-uds-create '(:id "s_1")
    (let ((agent-repl-frontend-permission-mode "auto"))
      ;; Act
      (agent-repl--frontend-create-session "/w")
      ;; Assert
      (should (equal (plist-get (agent-repl-test--created-payload) :permissionMode) "auto")))))

(ert-deftest agent-repl-test-frontend-create-omits-permission-mode-when-nil ()
  "A nil mode customization omits the field (SDK default)."
  ;; Arrange
  (agent-repl-test--with-uds-create '(:id "s_1")
    (let ((agent-repl-frontend-permission-mode nil))
      ;; Act
      (agent-repl--frontend-create-session "/w")
      ;; Assert
      (should-not (plist-member (agent-repl-test--created-payload) :permissionMode)))))

(ert-deftest agent-repl-test-frontend-create-sends-multi-repo-config-dir ()
  "A cwd under the multi-repo root carries that account's CLAUDE_CONFIG_DIR."
  ;; Arrange
  (agent-repl-test--with-uds-create '(:id "s_1")
    (let ((process-environment (cons "MULTI_REPO_ROOT=/home/user/multi" process-environment))
          (agent-repl-multi-repo-config-dir "~/.claude-chesscom"))
      ;; Act
      (agent-repl--frontend-create-session "/home/user/multi/repoA")
      ;; Assert
      (should (equal (plist-get (agent-repl-test--created-payload) :configDir)
                     (expand-file-name "~/.claude-chesscom"))))))

(ert-deftest agent-repl-test-frontend-create-omits-config-dir-outside-multi-repo ()
  "A personal project omits configDir so the CLI uses its own default root."
  ;; Arrange
  (agent-repl-test--with-uds-create '(:id "s_1")
    (let ((process-environment (cons "MULTI_REPO_ROOT=/home/user/multi" process-environment))
          (agent-repl-default-config-dir nil)
          (agent-repl-doom-multi-repo-mode nil))
      ;; Act
      (agent-repl--frontend-create-session "/home/user/personal/proj")
      ;; Assert
      (should-not (plist-member (agent-repl-test--created-payload) :configDir)))))

(ert-deftest agent-repl-test-frontend-create-requires-cwd ()
  "Create without a cwd signals instead of minting a cwd-less session."
  ;; Act / Assert — must fail before touching the UDS boundary.
  (should-error (agent-repl--frontend-create-session nil)))

(ert-deftest agent-repl-test-frontend-create-errors-without-session-view ()
  "An accepted ack with no matching SessionView signals loudly (never a silent nil)."
  ;; Arrange — ack succeeds but the store gets no view for the cwd.
  (let ((uds-commands '()))
    (ignore uds-commands)
    (clrhash agent-repl--frontend-session-views)
    (cl-letf (((symbol-function 'agent-repl--uds-send-command)
               (lambda (&rest _) "req-1"))
              ((symbol-function 'agent-repl--uds-track-command)
               (lambda (_r _f _w &optional _of on-success) (when on-success (funcall on-success)) "req-1"))
              ((symbol-function 'agent-repl--frontend-await-uds)
               (lambda (predicate &rest _) (funcall predicate))))
      ;; Act / Assert
      (should-error (agent-repl--frontend-create-session "/w")))))

;;;; ---- resume-viability hard-fail (ack error) --------------------------------

(defconst agent-repl-test--resume-missing-error
  "resume target uuid-gone has no transcript in this daemon's config dir"
  "The `createSession' CommandAck error text the resume-viability gate returns.")

(ert-deftest agent-repl-test-frontend-create-hard-fails-on-missing-transcript ()
  "A resume-missing ack error signals the distinct
`agent-repl-resume-transcript-missing' rather than starting fresh."
  ;; Arrange
  (cl-letf (((symbol-function 'agent-repl--dispatch-resume-investigation)
             (lambda (&rest _) "resume-investigate-uuid-gon")))
    (agent-repl-test--with-uds-create (list :error agent-repl-test--resume-missing-error)
      ;; Act / Assert
      (should-error (agent-repl--frontend-create-session "/w" nil "uuid-gone")
                    :type 'agent-repl-resume-transcript-missing))))

(ert-deftest agent-repl-test-frontend-create-hard-fail-names-investigation-ws ()
  "The surfaced hard-fail error names the investigation workspace."
  ;; Arrange
  (cl-letf (((symbol-function 'agent-repl--dispatch-resume-investigation)
             (lambda (&rest _) "resume-investigate-uuid-gon")))
    (agent-repl-test--with-uds-create (list :error agent-repl-test--resume-missing-error)
      ;; Act
      (let ((err (should-error (agent-repl--frontend-create-session "/w" nil "uuid-gone"))))
        ;; Assert
        (should (string-match-p "resume-investigate-uuid-gon"
                                (error-message-string err)))))))

(ert-deftest agent-repl-test-frontend-create-hard-fail-dispatches-investigation ()
  "The hard-fail opens an investigation workspace with the requested resume id.
The CommandAck carries no structured `searched_paths', so the investigation
is dispatched with the resume id and nil paths (a documented S7 constraint)."
  ;; Arrange
  (let (captured)
    (cl-letf (((symbol-function 'agent-repl--dispatch-resume-investigation)
               (lambda (resume-id searched cwd)
                 (setq captured (list resume-id searched cwd)) "ws")))
      (agent-repl-test--with-uds-create (list :error agent-repl-test--resume-missing-error)
        ;; Act
        (ignore-errors (agent-repl--frontend-create-session "/w/tree" nil "uuid-gone"))
        ;; Assert
        (should (equal captured '("uuid-gone" nil "/w/tree")))))))

(ert-deftest agent-repl-test-frontend-create-other-errors-stay-generic ()
  "An ack error WITHOUT the resume-missing signature stays a plain error."
  ;; Arrange
  (agent-repl-test--with-uds-create '(:error "no live shim for workspace")
    ;; Act
    (let ((err (should-error (agent-repl--frontend-create-session "/w" nil "uuid-x"))))
      ;; Assert — not misclassified as the resume hard-fail.
      (should-not (eq (car err) 'agent-repl-resume-transcript-missing))
      (should (string-match-p "no live shim" (error-message-string err))))))

;;;; ---- force-fresh override of the lost-transcript hard-fail ----------------

(ert-deftest agent-repl-test-frontend-create-force-fresh-var-degrades-to-fresh ()
  "With the override var set, a lost-transcript resume recreates fresh.
The mock fails the resumed create with the resume-missing ack, then the
force-fresh recreate (no resume) succeeds with a fresh id."
  ;; Arrange — a stateful mock: first createSession (with resume) fails, the
  ;; second (no resume) succeeds.
  (let ((agent-repl--force-fresh-conversation t)
        (calls 0) (uds-commands '()))
    (ignore uds-commands)
    (clrhash agent-repl--frontend-session-views)
    (cl-letf (((symbol-function 'agent-repl--dispatch-resume-investigation)
               (lambda (&rest _) (error "investigation must not run")))
              ((symbol-function 'agent-repl--uds-send-command)
               (lambda (field payload &optional ws &rest _)
                 (setq uds-commands (append uds-commands (list (list field payload ws)))) "req"))
              ((symbol-function 'agent-repl--uds-track-command)
               (lambda (_r field ws &optional on-failure on-success)
                 (when (equal field "createSession")
                   (if (= (cl-incf calls) 1)
                       (funcall on-failure agent-repl-test--resume-missing-error)
                     (agent-repl--frontend-store-session-view
                      (list :sessionId "fresh-sid" :workspace ws))
                     (funcall on-success)))
                 "req"))
              ((symbol-function 'agent-repl--frontend-await-uds)
               (lambda (predicate &rest _) (funcall predicate))))
      ;; Act / Assert
      (should (equal (agent-repl--frontend-create-session "/w" nil "uuid-gone") "fresh-sid"))
      ;; The last createSession carries no resume.
      (should-not (plist-member (nth 1 (car (last uds-commands))) :resumeClaudeSessionId)))))

;;;; ---- force-fresh session recreate ----------------------------------------

(ert-deftest agent-repl-test-frontend-force-fresh-session-omits-resume ()
  "`agent-repl--frontend-force-fresh-session' creates its session with no resume."
  ;; Arrange
  (let ((captured-resume 'unset))
    (cl-letf (((symbol-function 'agent-repl--ensure-frontend-daemon) (lambda () t))
              ((symbol-function 'agent-repl--frontend-wait-ready) (lambda () t))
              ((symbol-function 'agent-repl--frontend-create-session)
               (lambda (_dir &optional _model resume &rest _)
                 (setq captured-resume resume) "fresh-sid")))
      (unwind-protect
          (progn
            (puthash "ws1" (list :project-dir "/w") agent-repl--workspaces)
            ;; Act
            (agent-repl--frontend-force-fresh-session "ws1")
            ;; Assert
            (should (null captured-resume)))
        (remhash "ws1" agent-repl--workspaces)))))

(ert-deftest agent-repl-test-frontend-force-fresh-session-binds-fresh-id ()
  "The fresh session id is bound as the workspace's frontend session."
  ;; Arrange
  (cl-letf (((symbol-function 'agent-repl--ensure-frontend-daemon) (lambda () t))
            ((symbol-function 'agent-repl--frontend-wait-ready) (lambda () t))
            ((symbol-function 'agent-repl--frontend-create-session)
             (lambda (&rest _) "fresh-sid")))
    (unwind-protect
        (progn
          (puthash "ws1" (list :project-dir "/w") agent-repl--workspaces)
          ;; Act / Assert
          (should (equal (agent-repl--frontend-force-fresh-session "ws1") "fresh-sid"))
          (should (equal (agent-repl--ws-get "ws1" :frontend-session-id) "fresh-sid")))
      (remhash "ws1" agent-repl--workspaces))))

(ert-deftest agent-repl-test-frontend-force-fresh-session-clears-reattach-markers ()
  "The recreate resets the workspace's reattach failure markers."
  ;; Arrange
  (cl-letf (((symbol-function 'agent-repl--ensure-frontend-daemon) (lambda () t))
            ((symbol-function 'agent-repl--frontend-wait-ready) (lambda () t))
            ((symbol-function 'agent-repl--frontend-create-session)
             (lambda (&rest _) "fresh-sid")))
    (unwind-protect
        (progn
          (puthash "ws1" (list :project-dir "/w" :reattach-failed t :reattach-failures 3)
                   agent-repl--workspaces)
          ;; Act
          (agent-repl--frontend-force-fresh-session "ws1")
          ;; Assert
          (should-not (agent-repl--ws-get "ws1" :reattach-failed))
          (should-not (agent-repl--ws-get "ws1" :reattach-failures)))
      (remhash "ws1" agent-repl--workspaces))))

;;;; ---- liveness (pushed SessionView store) ----------------------------------

(defmacro agent-repl-test--with-views (views &rest body)
  "Clear the SessionView store, install VIEWS (a list of plists), run BODY.
Isolates the module-global `agent-repl--frontend-session-views' per test."
  (declare (indent 1))
  `(progn
     (clrhash agent-repl--frontend-session-views)
     (dolist (v ,views) (agent-repl--frontend-store-session-view v))
     ,@body))

(ert-deftest agent-repl-test-frontend-session-live-p-true-for-listed ()
  "A stored, non-terminal SessionView is live."
  ;; Arrange
  (agent-repl-test--with-views '((:sessionId "s_1" :workspace "/w"))
    ;; Act / Assert
    (should (agent-repl--frontend-session-live-p "s_1"))))

(ert-deftest agent-repl-test-frontend-session-live-p-nil-for-terminal ()
  "A terminal SessionView is not live."
  ;; Arrange
  (agent-repl-test--with-views '((:sessionId "s_1" :workspace "/w" :terminal t))
    ;; Act / Assert
    (should-not (agent-repl--frontend-session-live-p "s_1"))))

(ert-deftest agent-repl-test-frontend-session-live-p-nil-for-unlisted ()
  "An id with no stored SessionView is not live."
  ;; Arrange
  (agent-repl-test--with-views '()
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

(defmacro agent-repl-test--with-ws (ws plist &rest body)
  "Register workspace WS with PLIST for BODY, cleaning up after."
  (declare (indent 2))
  `(unwind-protect
       (progn
         (puthash ,ws (copy-sequence ,plist) agent-repl--workspaces)
         ,@body)
     (remhash ,ws agent-repl--workspaces)))

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

(ert-deftest agent-repl-test-frontend-ensure-session-reuses-live-id ()
  "A recorded id whose pushed SessionView is still live is reused; no create."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:frontend-session-id "s_live" :project-dir "/w")
    (cl-letf (((symbol-function 'agent-repl--ensure-frontend-daemon) (lambda (&optional _f) t))
              ((symbol-function 'agent-repl--frontend-wait-ready) (lambda () t)))
      (agent-repl-test--with-uds-create '(:id "unused")
        (agent-repl--frontend-store-session-view '(:sessionId "s_live" :workspace "/w"))
        ;; Act
        (let ((id (agent-repl--frontend-ensure-session "ws1")))
          ;; Assert — the live binding is reused, no createSession sent.
          (should (equal id "s_live"))
          (should (null uds-commands)))))))

(ert-deftest agent-repl-test-frontend-ensure-session-creates-when-stale ()
  "A recorded id with no live pushed SessionView is replaced via createSession."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:frontend-session-id "s_stale" :project-dir "/w/tree")
    (cl-letf (((symbol-function 'agent-repl--ensure-frontend-daemon) (lambda (&optional _f) t))
              ((symbol-function 'agent-repl--frontend-wait-ready) (lambda () t))
              ((symbol-function 'agent-repl--initialize-ws-env) (lambda (&rest _args) nil)))
      (agent-repl-test--with-uds-create '(:id "s_fresh")
        ;; Act — "s_stale" is absent from the (cleared) store, so it is stale.
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
      (agent-repl-test--with-uds-create '(:id "s_new")
        ;; Act
        (let ((id (agent-repl--frontend-ensure-session "ws1")))
          ;; Assert — session rooted at the adopted dir, dir recorded.
          (should (equal id "s_new"))
          (should (equal (agent-repl--ws-get "ws1" :project-dir) "/repo/root/"))
          (should (equal (plist-get (nth 1 (car uds-commands)) :cwd) "/repo/root/")))))))

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
      (agent-repl-test--with-uds-create '(:id "s_resumed")
        ;; Act
        (agent-repl--frontend-ensure-session "ws1")
        ;; Assert — the createSession payload carries the resume uuid.
        (should (equal (plist-get (nth 1 (car uds-commands)) :resumeClaudeSessionId)
                       "cli-uuid-7"))))))

(ert-deftest agent-repl-test-frontend-ensure-session-fresh-without-durable-id ()
  "No recorded durable session id means a fresh session with no resume field."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:frontend-session-id nil :project-dir "/w")
    (cl-letf (((symbol-function 'agent-repl--ensure-frontend-daemon) (lambda (&optional _f) t))
              ((symbol-function 'agent-repl--frontend-wait-ready) (lambda () t))
              ((symbol-function 'agent-repl--initialize-ws-env) (lambda (&rest _args) nil)))
      (agent-repl-test--with-uds-create '(:id "s_new")
        ;; Act
        (agent-repl--frontend-ensure-session "ws1")
        ;; Assert
        (should-not (plist-member (nth 1 (car uds-commands)) :resumeClaudeSessionId))))))

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
      (agent-repl-test--with-uds-create '(:id "s_new")
        ;; Act
        (agent-repl--frontend-ensure-session "ws1")
        ;; Assert
        (should (equal (plist-get (nth 1 (car uds-commands)) :model) "opus"))))))

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
        (agent-repl-test--with-uds-create '(:id "s_restored")
          ;; Act
          (agent-repl--frontend-ensure-session "ws1")
          ;; Assert — restore ran for ws1 with its dir, and the createSession
          ;; resumes the uuid the restore installed.
          (should (equal init-calls '(("ws1" "/w" nil))))
          (should (equal (plist-get (nth 1 (car uds-commands)) :resumeClaudeSessionId)
                         "restored-uuid")))))))

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
        (agent-repl-test--with-uds-create '(:id "s_new")
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
      (agent-repl-test--with-uds-create '(:id "s_new")
        ;; Act
        (agent-repl--gui-adopt-session "ws1" "cli-uuid-1")
        ;; Assert
        (should (equal (plist-get (nth 1 (car uds-commands)) :model) "opus"))))))

(ert-deftest agent-repl-test-gui-adopt-session-binds-the-new-session-id ()
  "Adopt binds WS's `:frontend-session-id' to the newly created session."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w")
    (cl-letf (((symbol-function 'agent-repl--ensure-frontend-daemon) (lambda (&optional _f) t))
              ((symbol-function 'agent-repl--frontend-wait-ready) (lambda () t)))
      (agent-repl-test--with-uds-create '(:id "s_adopted")
        ;; Act
        (let ((id (agent-repl--gui-adopt-session "ws1" "cli-uuid-1")))
          ;; Assert
          (should (equal id "s_adopted"))
          (should (equal (agent-repl--ws-get "ws1" :frontend-session-id) "s_adopted")))))))

;;;; ---- prompt submission (UDS `submitPrompt') --------------------------------
;;
;; The send/interrupt paths were migrated off HTTP (POST /message, /interrupt)
;; onto the UDS `submitPrompt'/`interrupt' commands keyed by workspace.  The
;; old session-id-keyed HTTP senders (`--frontend-send-message',
;; `--frontend-interrupt-session') and their retract half are gone (frontend.v1
;; carries no origin or retract), so their tests are gone with them; the send
;; is now covered by `agent-repl-test-frontend-send-user-message-*' and the
;; interrupt by `agent-repl-test-gui-interrupt-*'.

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

;; The queue SNAPSHOT readers (`--frontend-queue-content-preview',
;; `--frontend-session-queue', `--frontend-capture-queues') were removed in the
;; S7 cutover: the daemon no longer carries a `queue' array on GET /sessions and
;; the frontend.v1 SessionView has no queue field, so nothing populates
;; `:queued-messages' anymore.  The accessors below remain (they now always read
;; the empty queue), covering the retired plane's read side.

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
  "Workspace sends ensure the session (healing staleness) then send `submitPrompt'
keyed by the workspace (no session id on the wire)."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w")
    (cl-letf (((symbol-function 'agent-repl--frontend-ensure-session)
               (lambda (_ws) "s_fresh"))
              ((symbol-function 'agent-repl--frontend-sync-webview) (lambda (&rest _) nil)))
      (agent-repl-test--with-uds
        ;; Act
        (agent-repl--frontend-send-user-message "ws1" "hi")
        ;; Assert — submitPrompt carrying the text, keyed by the workspace.
        (pcase-let ((`(,field ,payload ,ws) (car uds-commands)))
          (should (equal field "submitPrompt"))
          (should (equal (plist-get payload :text) "hi"))
          (should (equal ws "ws1")))))))

(ert-deftest agent-repl-test-frontend-send-user-message-syncs-webview ()
  "The send path remounts the webview onto the ensured (possibly healed) session."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w")
    (let ((synced nil))
      (cl-letf (((symbol-function 'agent-repl--frontend-ensure-session)
                 (lambda (_ws) "s_healed"))
                ((symbol-function 'agent-repl--frontend-sync-webview)
                 (lambda (ws id) (setq synced (list ws id)))))
        (agent-repl-test--with-uds
          ;; Act
          (agent-repl--frontend-send-user-message "ws1" "hi")
          ;; Assert
          (should (equal synced '("ws1" "s_healed"))))))))

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
  "A bound session whose pushed WorkspaceState is turn-active is returned; idle skipped."
  ;; Arrange — ws1's pushed state is turn-active, ws2's is not.
  (agent-repl-test--with-views '((:sessionId "s_busy" :workspace "/w1")
                                 (:sessionId "s_idle" :workspace "/w2"))
    (agent-repl-test--with-ws "ws1"
        '(:frontend-session-id "s_busy" :project-dir "/w1"
          :pushed-render-state-meta (:turn-active t))
      (agent-repl-test--with-ws "ws2"
          '(:frontend-session-id "s_idle" :project-dir "/w2"
            :pushed-render-state-meta (:turn-active nil))
        ;; Act / Assert
        (should (equal (agent-repl--frontend-turn-active-sessions) '("s_busy")))))))

(ert-deftest agent-repl-test-frontend-turn-active-sessions-skips-terminal ()
  "A terminal session is never counted busy, even bound and turn-active."
  ;; Arrange
  (agent-repl-test--with-views '((:sessionId "s_zombie" :workspace "/w1" :terminal t)
                                 (:sessionId "s_live" :workspace "/w2"))
    (agent-repl-test--with-ws "ws1"
        '(:frontend-session-id "s_zombie" :project-dir "/w1"
          :pushed-render-state-meta (:turn-active t))
      (agent-repl-test--with-ws "ws2"
          '(:frontend-session-id "s_live" :project-dir "/w2"
            :pushed-render-state-meta (:turn-active t))
        ;; Act / Assert
        (should (equal (agent-repl--frontend-turn-active-sessions) '("s_live")))))))

(ert-deftest agent-repl-test-frontend-turn-active-sessions-skips-unbound-orphan ()
  "A turn-active session in the store but bound to NO live workspace is never counted.
Iterating only live workspaces' bindings intrinsically excludes the orphan a
prior bounce leaves behind — it must not block a future bounce."
  ;; Arrange — s_orphan is live in the store but no workspace is bound to it.
  (agent-repl-test--with-views '((:sessionId "s_bound" :workspace "/w1")
                                 (:sessionId "s_orphan" :workspace "/w2"))
    (agent-repl-test--with-ws "ws1"
        '(:frontend-session-id "s_bound" :project-dir "/w1"
          :pushed-render-state-meta (:turn-active t))
      ;; Act / Assert
      (should (equal (agent-repl--frontend-turn-active-sessions) '("s_bound"))))))

(ert-deftest agent-repl-test-frontend-turn-active-sessions-nil-when-none-active ()
  "No bound workspace reporting turn-active reads as no turns (nothing to protect)."
  ;; Arrange — a bound, live session whose pushed state is idle.
  (agent-repl-test--with-views '((:sessionId "s_1" :workspace "/w1"))
    (agent-repl-test--with-ws "ws1"
        '(:frontend-session-id "s_1" :project-dir "/w1"
          :pushed-render-state-meta (:turn-active nil))
      ;; Act / Assert
      (should (null (agent-repl--frontend-turn-active-sessions))))))

;;;; ---- orphan-session reaper --------------------------------------------------

(ert-deftest agent-repl-test-frontend-orphan-ids-selects-superseded-duplicate ()
  "A live pushed SessionView bound nowhere, duplicating a bound conversation, is a target."
  ;; Arrange — s_new is bound; s_old shares its claude id, is live and unbound.
  (cl-letf (((symbol-function 'agent-repl--frontend-bound-session-ids)
             (lambda () '("s_new"))))
    (agent-repl-test--with-views '((:sessionId "s_new" :claudeSessionId "c1")
                                   (:sessionId "s_old" :claudeSessionId "c1"))
      ;; Act / Assert
      (should (equal (agent-repl--frontend-orphan-session-ids) '("s_old"))))))

(ert-deftest agent-repl-test-frontend-orphan-ids-spares-terminal-duplicate ()
  "A TERMINAL duplicate is spared: a dead record leaks no shim."
  ;; Arrange (post-cutover a session is either live-shim or terminal; the old
  ;; hibernated/rehydratable exclusions are gone — those fields are hard-false).
  (cl-letf (((symbol-function 'agent-repl--frontend-bound-session-ids)
             (lambda () '("s_new"))))
    (agent-repl-test--with-views '((:sessionId "s_new" :claudeSessionId "c1")
                                   (:sessionId "s_dead" :claudeSessionId "c1" :terminal t))
      ;; Act / Assert
      (should (null (agent-repl--frontend-orphan-session-ids))))))

(ert-deftest agent-repl-test-frontend-orphan-ids-spares-unique-unbound ()
  "An unbound live session duplicating NO bound conversation is left alone."
  ;; Arrange
  (cl-letf (((symbol-function 'agent-repl--frontend-bound-session-ids)
             (lambda () '("s_new"))))
    (agent-repl-test--with-views '((:sessionId "s_new" :claudeSessionId "c1")
                                   (:sessionId "s_solo" :claudeSessionId "c2"))
      ;; Act / Assert
      (should (null (agent-repl--frontend-orphan-session-ids))))))

(ert-deftest agent-repl-test-frontend-reap-deletes-orphans ()
  "The reaper issues a deleteSession for each orphan id and returns them."
  ;; Arrange
  (let ((deleted nil))
    (cl-letf (((symbol-function 'agent-repl--frontend-orphan-session-ids)
               (lambda () '("s_old")))
              ((symbol-function 'agent-repl--frontend-delete-session)
               (lambda (id &optional _ws) (push id deleted) "req")))
      ;; Act
      (let ((reaped (agent-repl--frontend-reap-orphan-sessions)))
        ;; Assert
        (should (equal reaped '("s_old")))
        (should (equal deleted '("s_old")))))))

(ert-deftest agent-repl-test-frontend-reap-skips-failed-delete ()
  "A failed delete is skipped rather than fatal, and excluded from the reaped list."
  ;; Arrange
  (cl-letf (((symbol-function 'agent-repl--frontend-orphan-session-ids)
             (lambda () '("s_bad" "s_ok")))
            ((symbol-function 'agent-repl--frontend-delete-session)
             (lambda (id &optional _ws) (if (equal id "s_bad") (error "boom") "req"))))
    ;; Act
    (let ((reaped (agent-repl--frontend-reap-orphan-sessions)))
      ;; Assert
      (should (equal reaped '("s_ok"))))))

;;;; ---- reattach loop -----------------------------------------------------------

(ert-deftest agent-repl-test-frontend-reattach-check-no-op-when-listed ()
  "A binding present (non-terminal) in the pushed roster is left alone; markers clear."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:frontend-session-id "s_1" :reattach-failed t
                                    :reattach-failures 2 :project-dir "/w")
    (agent-repl-test--with-views '((:sessionId "s_1" :workspace "/w"))
      (let ((reattached nil))
        (cl-letf (((symbol-function 'agent-repl--uds-connected-p) (lambda () t))
                  ((symbol-function 'agent-repl--frontend-reattach-ws)
                   (lambda (&rest args) (push args reattached)))
                  ((symbol-function 'agent-repl--frontend-reap-orphan-sessions) #'ignore))
          ;; Act
          (agent-repl--frontend-reattach-check)
          ;; Assert
          (should (null reattached))
          (should-not (agent-repl--ws-get "ws1" :reattach-failed))
          (should-not (agent-repl--ws-get "ws1" :reattach-failures)))))))

(ert-deftest agent-repl-test-frontend-reattach-check-reattaches-vanished ()
  "A binding missing from the pushed roster triggers a reattach."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:frontend-session-id "s_gone" :project-dir "/w")
    (agent-repl-test--with-views '((:sessionId "s_other" :workspace "/w2"))
      (let ((reattached nil))
        (cl-letf (((symbol-function 'agent-repl--uds-connected-p) (lambda () t))
                  ((symbol-function 'agent-repl--frontend-reattach-ws)
                   (lambda (ws stale) (push (list ws stale) reattached)))
                  ((symbol-function 'agent-repl--frontend-reap-orphan-sessions) #'ignore))
          ;; Act
          (agent-repl--frontend-reattach-check)
          ;; Assert
          (should (equal reattached '(("ws1" "s_gone")))))))))

(ert-deftest agent-repl-test-frontend-reattach-check-skips-given-up-workspaces ()
  "A workspace marked :reattach-failed is not retried."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:frontend-session-id "s_gone" :reattach-failed t
                                    :project-dir "/w")
    (agent-repl-test--with-views '()
      (let ((reattached nil))
        (cl-letf (((symbol-function 'agent-repl--uds-connected-p) (lambda () t))
                  ((symbol-function 'agent-repl--frontend-reattach-ws)
                   (lambda (&rest args) (push args reattached)))
                  ((symbol-function 'agent-repl--frontend-reap-orphan-sessions) #'ignore))
          ;; Act
          (agent-repl--frontend-reattach-check)
          ;; Assert
          (should (null reattached)))))))

(ert-deftest agent-repl-test-frontend-reattach-check-ensures-daemon-when-link-down ()
  "A down UDS link with live bindings triggers a daemon ensure."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:frontend-session-id "s_1" :project-dir "/w")
    (let ((ensured nil))
      (cl-letf (((symbol-function 'agent-repl--uds-connected-p) (lambda () nil))
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
  "Release sends a `deleteSession' for the recorded session and clears the key."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:frontend-session-id "s_1")
    (agent-repl-test--with-uds
      ;; Act
      (agent-repl--frontend-release-workspace-session "ws1")
      ;; Assert
      (pcase-let ((`(,field ,payload ,_ws) (car uds-commands)))
        (should (equal field "deleteSession"))
        (should (equal (plist-get payload :sessionId) "s_1")))
      (should (null (agent-repl--ws-get "ws1" :frontend-session-id))))))

(ert-deftest agent-repl-test-frontend-release-logs-but-never-signals ()
  "A down link must not abort the nuke: release logs and proceeds."
  ;; Arrange — the UDS send signals (not connected); release must catch it.
  (agent-repl-test--with-ws "ws1" '(:frontend-session-id "s_1")
    (cl-letf (((symbol-function 'agent-repl--uds-send-command)
               (lambda (&rest _) (user-error "not connected"))))
      ;; Act — must not signal.
      (agent-repl--frontend-release-workspace-session "ws1")
      ;; Assert — the key is still cleared.
      (should (null (agent-repl--ws-get "ws1" :frontend-session-id))))))

(ert-deftest agent-repl-test-frontend-release-noop-without-id ()
  "Release without a recorded id sends no command at all."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w")
    (agent-repl-test--with-uds
      ;; Act
      (agent-repl--frontend-release-workspace-session "ws1")
      ;; Assert
      (should (null uds-commands)))))

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

(ert-deftest agent-repl-test-gui-send-turn-snaps-webview-to-tail-before-send ()
  "gui-send-turn snaps the webview to its tail BEFORE the HTTP send.
A prompt sent from a scrolled-up feed must jump to the bottom the instant
it leaves, not wait for the daemon to echo the turn back, so the snap
precedes the wire."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:frontend-session-id "s_1" :project-dir "/w")
    (let ((events '()))
      (cl-letf (((symbol-function 'agent-repl--frontend-snap-webview-to-tail)
                 (lambda (_ws) (push 'snap events)))
                ((symbol-function 'agent-repl--frontend-send-user-message)
                 (lambda (&rest _) (push 'send events) "r_1"))
                ((symbol-function 'agent-repl--mark-ws-thinking) #'ignore)
                ((symbol-function 'agent-repl--increment-prefix-counter) #'ignore)
                ((symbol-function 'agent-repl--run-send-posthooks) #'ignore)
                ((symbol-function 'agent-repl--kickoff-prompt-summary) #'ignore))
        ;; Act
        (agent-repl--gui-send-turn "ws1" "prepared input" "raw input")
        ;; Assert -- snap was recorded, and it landed before the send.
        (should (equal (reverse events) '(snap send)))))))

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

;;;; ---- origin-tagged sends (merge-remediation) -----------------------------
;;
;; frontend.v1's `SubmitPromptCmd' has no `origin' field, so the merge
;; status-card origin stamp is no longer forwarded (it was already dead
;; server-side — the retired HTTP /message route never read it into the
;; driver).  Send-user-message still CONSUMES and clears the one-shot
;; `:next-send-origin' so it never lingers; it just does not reach the wire.

(ert-deftest agent-repl-test-frontend-send-user-message-clears-next-send-origin ()
  "Send-user-message consumes and clears the one-shot `:next-send-origin'."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:frontend-session-id "s1" :next-send-origin "merge")
    (cl-letf (((symbol-function 'agent-repl--frontend-ensure-session) (lambda (_ws) "s1"))
              ((symbol-function 'agent-repl--frontend-sync-webview) (lambda (&rest _) nil)))
      (agent-repl-test--with-uds
        ;; Act
        (agent-repl--frontend-send-user-message "ws1" "rebase")
        ;; Assert — the one-shot flag is cleared (not forwarded on the wire).
        (should (null (agent-repl--ws-get "ws1" :next-send-origin)))))))

(ert-deftest agent-repl-test-frontend-send-user-message-submits-text ()
  "Send-user-message sends `submitPrompt' with the text, keyed by the workspace."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:frontend-session-id "s1")
    (cl-letf (((symbol-function 'agent-repl--frontend-ensure-session) (lambda (_ws) "s1"))
              ((symbol-function 'agent-repl--frontend-sync-webview) (lambda (&rest _) nil)))
      (agent-repl-test--with-uds
        ;; Act
        (agent-repl--frontend-send-user-message "ws1" "a normal prompt")
        ;; Assert
        (pcase-let ((`(,field ,payload ,ws) (car uds-commands)))
          (should (equal field "submitPrompt"))
          (should (equal (plist-get payload :text) "a normal prompt"))
          (should (equal ws "ws1")))))))

(provide 'test-frontend-client)

;;; test-frontend-client.el ends here

;;;; ---- gui interrupt (UDS `interrupt', keyed by workspace) --------------------
;;
;; The retract half of `C-c C-k' is gone: frontend.v1's `InterruptCmd' carries
;; only `hard' (no retract id), and the daemon's HTTP interrupt already reported
;; retracted=false post-cutover.  So gui-interrupt now just dispatches an
;; `interrupt' command keyed by the workspace and always returns t.

(ert-deftest agent-repl-test-gui-interrupt-sends-command-keyed-by-workspace ()
  "Interrupt dispatches the UDS `interrupt' command keyed by the workspace."
  ;; Arrange
  (agent-repl-test--with-ws "ws1"
      '(:frontend-session-id "s_1" :sent-turn (:request-id "r_9" :raw "draft"))
    (agent-repl-test--with-uds
      ;; Act
      (agent-repl--gui-interrupt "ws1" 'escape)
      ;; Assert
      (pcase-let ((`(,field ,_payload ,ws) (car uds-commands)))
        (should (equal field "interrupt"))
        (should (equal ws "ws1"))))))

(ert-deftest agent-repl-test-gui-interrupt-returns-t ()
  "Both gestures return t (dispatched); the retract verdict is gone."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:frontend-session-id "s_1")
    (agent-repl-test--with-uds
      ;; Act / Assert
      (should (eq (agent-repl--gui-interrupt "ws1" 'escape) t))
      (should (eq (agent-repl--gui-interrupt "ws1" 'ctrl-c) t)))))

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
