;;; test-frontend-client.el --- ERT tests for frontend-client.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the daemon session client.  Emacs speaks no HTTP to the
;; daemon: session CRUD travels as UDS commands and every read comes off
;; pushed frames, so the boundaries shadowed here are
;; `agent-repl--uds-send-command' / `--uds-track-command' /
;; `--uds-connected-p' — no real socket ever opens.
;;
;; Run with:
;;   emacs -batch -Q -l ert -l test-frontend-client.el -f ert-run-tests-batch-and-exit

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

(require 'cl-lib)

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

;;;; ---- webview URL ---------------------------------------------------------

(ert-deftest agent-repl-test-frontend-base-url-is-the-webview-address ()
  "The one surviving URL builder addresses the daemon's served webapp.
Emacs itself issues no HTTP; this URL is handed to the embedded browser."
  ;; Arrange
  (let ((agent-repl-frontend-daemon-addr "127.0.0.1:9999"))
    ;; Act / Assert
    (should (equal (agent-repl--frontend-base-url) "http://127.0.0.1:9999"))))

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

(ert-deftest agent-repl-test-frontend-create-omits-ungated-consent-by-default ()
  "An ordinary create never carries the ungated-session consent."
  ;; Arrange
  (agent-repl-test--with-uds-create '(:id "s_1")
    (let ((agent-repl-frontend-allow-ungated nil))
      ;; Act
      (agent-repl--frontend-create-session "/w")
      ;; Assert
      (should-not (plist-member (agent-repl-test--created-payload) :allowUngated)))))

(ert-deftest agent-repl-test-frontend-create-sends-bound-ungated-consent ()
  "A caller that binds the consent gets it on the wire as `allowUngated'."
  ;; Arrange
  (agent-repl-test--with-uds-create '(:id "s_1")
    (let ((agent-repl-frontend-allow-ungated t)
          (agent-repl-frontend-permission-mode "bypassPermissions"))
      ;; Act
      (agent-repl--frontend-create-session "/w")
      ;; Assert
      (should (eq (plist-get (agent-repl-test--created-payload) :allowUngated) t)))))

(ert-deftest agent-repl-test-frontend-ungated-mode-p-flags-bypass-permissions ()
  "`bypassPermissions' is the mode under which no permission gate exists."
  ;; Arrange + Act + Assert
  (should (agent-repl-frontend-ungated-permission-mode-p "bypassPermissions")))

(ert-deftest agent-repl-test-frontend-ungated-mode-p-clears-dont-ask ()
  "`dontAsk' bypasses canUseTool fail-CLOSED, so it is not ungated."
  ;; Arrange + Act + Assert
  (should-not (agent-repl-frontend-ungated-permission-mode-p "dontAsk")))

(ert-deftest agent-repl-test-frontend-ungated-mode-p-clears-auto ()
  "`auto' still reaches canUseTool for the ask path."
  ;; Arrange + Act + Assert
  (should-not (agent-repl-frontend-ungated-permission-mode-p "auto")))

(ert-deftest agent-repl-test-frontend-ungated-mode-p-clears-nil ()
  "A nil mode is not a claim of ungatedness."
  ;; Arrange + Act + Assert
  (should-not (agent-repl-frontend-ungated-permission-mode-p nil)))

(ert-deftest agent-repl-test-frontend-session-posture-is-shared-explicit-state ()
  "The shared posture helper returns account, mode, and deliberate consent."
  (let ((agent-repl-frontend-permission-mode "bypassPermissions")
        (agent-repl-frontend-allow-ungated t))
    (cl-letf (((symbol-function 'agent-repl--compute-config-dir)
               (lambda (cwd)
                 (should (equal cwd "/work"))
                 "/account"))
              ((symbol-function 'agent-repl--ws-name-for-dir)
               (lambda (_cwd) "ws")))
      (should
       (equal (agent-repl--frontend-session-posture "/work")
              '(:config-dir "/account"
                :permission-mode "bypassPermissions"
                :allow-ungated t))))))

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

(defmacro agent-repl-test--with-readiness (connected-fn &rest body)
  "Run BODY with the UDS readiness inputs shadowed.
CONNECTED-FN is the nullary stand-in for `agent-repl--uds-connected-p'.
`agent-repl-uds-connect' dials are counted into the anaphoric `dials',
`accept-process-output' pumps into `pumps', and blocking `sleep-for'
waits into `sleeps' (newest last).  The DaemonView store starts EMPTY so
each test drives it explicitly."
  (declare (indent 1))
  `(let ((dials 0) (pumps 0) (sleeps '())
         (agent-repl--frontend-last-daemon-view nil))
     (ignore dials pumps sleeps)
     (cl-letf (((symbol-function 'agent-repl--uds-connected-p) ,connected-fn)
               ((symbol-function 'agent-repl-uds-connect)
                (lambda (&optional _p _readiness-p) (cl-incf dials) nil))
               ((symbol-function 'accept-process-output)
                (lambda (&rest _) (cl-incf pumps) nil))
               ((symbol-function 'sleep-for)
                (lambda (secs) (setq sleeps (append sleeps (list secs))) nil)))
       ,@body)))

(ert-deftest agent-repl-test-frontend-ready-p-false-without-a-daemon-view ()
  "A live link with no pushed `DaemonView' is NOT ready.
The connection can be accepted a beat before the snapshot is composed."
  ;; Arrange
  (agent-repl-test--with-readiness (lambda () t)
    ;; Act / Assert
    (should-not (agent-repl--frontend-daemon-ready-p))))

(ert-deftest agent-repl-test-frontend-ready-p-false-when-link-is-down ()
  "A stored `DaemonView' with a DEAD link is NOT ready.
The view outlives the connection that delivered it, so it describes a
daemon that may be gone."
  ;; Arrange
  (agent-repl-test--with-readiness (lambda () nil)
    (setq agent-repl--frontend-last-daemon-view '(:bootId "b_1"))
    ;; Act / Assert
    (should-not (agent-repl--frontend-daemon-ready-p))))

(ert-deftest agent-repl-test-frontend-ready-p-true-with-link-and-view ()
  "A live link plus a pushed `DaemonView' is ready."
  ;; Arrange
  (agent-repl-test--with-readiness (lambda () t)
    (setq agent-repl--frontend-last-daemon-view '(:bootId "b_1"))
    ;; Act / Assert
    (should (agent-repl--frontend-daemon-ready-p))))

(ert-deftest agent-repl-test-frontend-wait-ready-returns-immediately-when-ready ()
  "An already-ready link neither dials nor pumps."
  ;; Arrange
  (agent-repl-test--with-readiness (lambda () t)
    (setq agent-repl--frontend-last-daemon-view '(:bootId "b_1"))
    ;; Act
    (should (agent-repl--frontend-wait-ready))
    ;; Assert
    (should (= dials 0))
    (should (= pumps 0))))

(ert-deftest agent-repl-test-frontend-wait-ready-dials-while-the-link-is-down ()
  "Each DOWN-link attempt is readiness-owned and paces with a blocking sleep."
  ;; Arrange — the first two dials fail; the third connects with a view.
  (let ((attempts 0)
        readiness-args)
    (agent-repl-test--with-readiness (lambda () (>= attempts 3))
      (cl-letf (((symbol-function 'agent-repl-uds-connect)
                 (lambda (&optional _p readiness-p)
                   (cl-incf attempts)
                   (setq readiness-args
                         (append readiness-args (list readiness-p)))
                   (when (>= attempts 3)
                     (setq agent-repl--frontend-last-daemon-view '(:bootId "b_1")))
                   nil)))
        ;; Act
        (should (agent-repl--frontend-wait-ready))
        ;; Assert — one dial per attempt, one 0.2s pacing sleep per FAILED dial.
        (should (= attempts 3))
        (should (equal readiness-args '(t t t)))
        (should (equal sleeps '(0.2 0.2)))))))

(ert-deftest agent-repl-test-frontend-wait-ready-pumps-a-live-link-for-the-view ()
  "A live link with no view yet is PUMPED until the snapshot lands."
  ;; Arrange — connected throughout; the view arrives on the 2nd pump.
  (let ((pumped 0))
    (agent-repl-test--with-readiness (lambda () t)
      (cl-letf (((symbol-function 'accept-process-output)
                 (lambda (&rest _)
                   (cl-incf pumped)
                   (when (>= pumped 2)
                     (setq agent-repl--frontend-last-daemon-view '(:bootId "b_1")))
                   nil)))
        ;; Act
        (should (agent-repl--frontend-wait-ready))
        ;; Assert — pumped, never slept (a live link needs no pacing sleep).
        (should (= pumped 2))
        (should (null sleeps))))))

(ert-deftest agent-repl-test-frontend-wait-ready-never-dials-a-live-link ()
  "A live link is never re-dialed while waiting for its snapshot."
  ;; Arrange
  (let ((pumped 0))
    (agent-repl-test--with-readiness (lambda () t)
      (cl-letf (((symbol-function 'accept-process-output)
                 (lambda (&rest _)
                   (cl-incf pumped)
                   (setq agent-repl--frontend-last-daemon-view '(:bootId "b_1"))
                   nil)))
        ;; Act
        (agent-repl--frontend-wait-ready)
        ;; Assert
        (should (= dials 0))))))

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

(ert-deftest agent-repl-test-frontend-wait-ready-errors-when-never-connected ()
  "A link that never comes up gives up loudly after the attempt budget."
  ;; Arrange
  (let ((agent-repl-frontend-ready-attempts 3))
    (agent-repl-test--with-readiness (lambda () nil)
      ;; Act / Assert
      (let ((err (should-error (agent-repl--frontend-wait-ready))))
        (should (string-match-p "connected=no" (error-message-string err)))))))

(ert-deftest agent-repl-test-frontend-wait-ready-errors-when-no-view-arrives ()
  "A live link that never pushes a `DaemonView' gives up loudly too.
Distinct failure mode from an unreachable socket, and the error says so."
  ;; Arrange
  (let ((agent-repl-frontend-ready-attempts 3))
    (agent-repl-test--with-readiness (lambda () t)
      ;; Act / Assert
      (let ((err (should-error (agent-repl--frontend-wait-ready))))
        (should (string-match-p "daemon-view=no" (error-message-string err)))))))

(ert-deftest agent-repl-test-frontend-wait-ready-honors-the-attempt-budget ()
  "The give-up happens after exactly `agent-repl-frontend-ready-attempts' tries."
  ;; Arrange
  (let ((agent-repl-frontend-ready-attempts 3))
    (agent-repl-test--with-readiness (lambda () nil)
      ;; Act
      (ignore-errors (agent-repl--frontend-wait-ready))
      ;; Assert — one dial per attempt, no more.
      (should (= dials 3)))))

;;;; ---- UDS health commands ---------------------------------------------------

(ert-deftest agent-repl-test-frontend-daemon-health-awaits-correlated-view ()
  "Daemon health succeeds only from its correlated healthy result frame."
  (let (sent tracked-command tracked-health)
    (cl-letf (((symbol-function 'agent-repl--frontend-wait-ready) (lambda () t))
              ((symbol-function 'agent-repl--uds-send-command)
               (lambda (field payload workspace &rest _)
                 (setq sent (list field payload workspace)) "health-1"))
              ((symbol-function 'agent-repl--uds-track-health-response)
               (lambda (id field workspace session-id callback)
                 (setq tracked-health
                       (list id field workspace session-id))
                 (funcall callback '(:requestId "health-1" :healthy t))))
              ((symbol-function 'agent-repl--uds-track-command)
               (lambda (id field workspace &rest _)
                 (setq tracked-command (list id field workspace))))
              ((symbol-function 'agent-repl--frontend-await-uds)
               (lambda (predicate &rest _) (funcall predicate))))
      (should (agent-repl--frontend-wait-daemon-healthy))
      (should (equal sent '("daemonHealth" nil nil)))
      (should (equal tracked-command
                     '("health-1" "daemonHealth" nil)))
      (should (equal tracked-health
                     '("health-1" "daemonHealth" nil nil))))))

(ert-deftest agent-repl-test-frontend-session-health-sends-session-id-and-cwd ()
  "Session health binds the daemon's verdict to the rendered session id."
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w/tree")
    (let (sent tracked-health)
      (cl-letf (((symbol-function 'agent-repl--frontend-wait-ready) (lambda () t))
                ((symbol-function 'agent-repl--uds-send-command)
                 (lambda (field payload workspace &rest _)
                   (setq sent (list field payload workspace)) "health-2"))
                ((symbol-function 'agent-repl--uds-track-health-response)
                 (lambda (id field workspace session-id callback)
                   (setq tracked-health
                         (list id field workspace session-id))
                   (funcall callback
                            '(:requestId "health-2"
                              :workspace "/w/tree"
                              :sessionId "s_expected"
                              :healthy t))))
                ((symbol-function 'agent-repl--uds-track-command)
                 (lambda (&rest _) "health-2"))
                ((symbol-function 'agent-repl--frontend-await-uds)
                 (lambda (predicate &rest _) (funcall predicate))))
        (should (agent-repl--frontend-wait-session-healthy "ws1" "s_expected"))
        (should (equal sent
                       '("sessionHealth" (:sessionId "s_expected") "/w/tree")))
        (should (equal tracked-health
                       '("health-2" "sessionHealth" "/w/tree" "s_expected")))))))

(ert-deftest agent-repl-test-frontend-health-accepted-ack-alone-times-out ()
  "A successful command receipt cannot satisfy a health wait."
  (let (command-tracked)
    (cl-letf (((symbol-function 'agent-repl--frontend-wait-ready) (lambda () t))
              ((symbol-function 'agent-repl--uds-send-command)
               (lambda (&rest _) "health-ack-only"))
              ((symbol-function 'agent-repl--uds-track-health-response)
               (lambda (&rest _) "health-ack-only"))
              ((symbol-function 'agent-repl--uds-track-command)
               (lambda (&rest _)
                 (setq command-tracked t)
                 "health-ack-only"))
              ((symbol-function 'agent-repl--frontend-await-uds)
               (lambda (predicate &rest _)
                 (should-not (funcall predicate))
                 nil))
              ((symbol-function 'agent-repl--uds-untrack-command)
               (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--uds-untrack-health-response)
               (lambda (&rest _) nil)))
      (should-error (agent-repl--frontend-wait-daemon-healthy))
      (should command-tracked))))

(ert-deftest agent-repl-test-frontend-unhealthy-result-fails-loudly ()
  "A correlated `healthy=false' result aborts and includes its reason."
  (cl-letf (((symbol-function 'agent-repl--frontend-wait-ready) (lambda () t))
            ((symbol-function 'agent-repl--uds-send-command)
             (lambda (&rest _) "health-bad"))
            ((symbol-function 'agent-repl--uds-track-health-response)
             (lambda (_id _field _workspace _session-id callback)
               (funcall callback
                        '(:requestId "health-bad"
                          :reason "store link down"))))
            ((symbol-function 'agent-repl--uds-track-command)
             (lambda (&rest _) "health-bad"))
            ((symbol-function 'agent-repl--frontend-await-uds)
             (lambda (predicate &rest _) (funcall predicate))))
    (let ((err (should-error
                (agent-repl--frontend-wait-daemon-healthy))))
      (should (string-match-p "store link down"
                              (error-message-string err))))))

(ert-deftest agent-repl-test-frontend-health-timeout-untracks-and-fails-loudly ()
  "A health timeout removes its delayed callback and aborts rendering/startup."
  (let (command-untracked health-untracked)
    (cl-letf (((symbol-function 'agent-repl--frontend-wait-ready) (lambda () t))
              ((symbol-function 'agent-repl--uds-send-command) (lambda (&rest _) "health-3"))
              ((symbol-function 'agent-repl--uds-track-health-response)
               (lambda (&rest _) "health-3"))
              ((symbol-function 'agent-repl--uds-track-command) (lambda (&rest _) "health-3"))
              ((symbol-function 'agent-repl--frontend-await-uds) (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--uds-untrack-command)
               (lambda (id workspace reason)
                 (setq command-untracked (list id workspace reason))))
              ((symbol-function 'agent-repl--uds-untrack-health-response)
               (lambda (id workspace reason)
                 (setq health-untracked (list id workspace reason)))))
      (should-error (agent-repl--frontend-wait-daemon-healthy))
      (should (equal command-untracked
                     '("health-3" nil "health-timeout")))
      (should (equal health-untracked
                     '("health-3" nil "health-timeout"))))))

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
      ;; Act / Assert
      (should-error (agent-repl--frontend-ensure-session "ws1") :type 'user-error))))

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

;; The §2.13 in-flight-message-queue tests (the queue-run-now / queue-cancel
;; POST routes and the :queued-messages / queued-count accessors) were deleted
;; in the S9 endgame: the queue plane is retired daemon-side and the
;; perpetually-empty accessors are gone.

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
keyed by the workspace CWD (no session id on the wire)."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w")
    (cl-letf (((symbol-function 'agent-repl--frontend-ensure-session)
               (lambda (_ws) "s_fresh"))
              ((symbol-function 'agent-repl--frontend-sync-webview) (lambda (&rest _) nil)))
      (agent-repl-test--with-uds
        ;; Act
        (agent-repl--frontend-send-user-message "ws1" "hi")
        ;; Assert — submitPrompt carrying the text, keyed by the workspace CWD.
        (pcase-let ((`(,field ,payload ,ws) (car uds-commands)))
          (should (equal field "submitPrompt"))
          (should (equal (plist-get payload :text) "hi"))
          (should (equal ws "/w")))))))

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

(ert-deftest agent-repl-test-frontend-turn-active-sessions-skips-unbound-stale ()
  "A turn-active session in the store but bound to NO live workspace is never counted.
Iterating only live workspaces' bindings intrinsically excludes the stale
unbound session a prior bounce leaves behind — it must not block a future
bounce."
  ;; Arrange — s_stale is live in the store but no workspace is bound to it.
  (agent-repl-test--with-views '((:sessionId "s_bound" :workspace "/w1")
                                 (:sessionId "s_stale" :workspace "/w2"))
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

(ert-deftest agent-repl-test-frontend-all-turn-active-includes-unrestored-workspace ()
  "Startup safety sees an active daemon path before Emacs restores its workspace."
  (let ((agent-repl--frontend-workspace-state-views
         (make-hash-table :test 'equal)))
    (puthash "/unrestored"
             '(:workspace "/unrestored" :sessionId "s_busy" :turnActive t)
             agent-repl--frontend-workspace-state-views)
    (agent-repl-test--with-views
        '((:sessionId "s_busy" :workspace "/unrestored"))
      (should (equal (agent-repl--frontend-all-turn-active-session-ids)
                     '("s_busy"))))))

(ert-deftest agent-repl-test-frontend-all-turn-active-skips-terminal-session ()
  "A terminal session cannot block the coordinated startup restart."
  (let ((agent-repl--frontend-workspace-state-views
         (make-hash-table :test 'equal)))
    (puthash "/old"
             '(:workspace "/old" :sessionId "s_dead" :turnActive t)
             agent-repl--frontend-workspace-state-views)
    (agent-repl-test--with-views
        '((:sessionId "s_dead" :workspace "/old" :terminal t))
      (should-not (agent-repl--frontend-all-turn-active-session-ids)))))

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
                   (lambda (&rest args) (push args reattached))))
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
                   (lambda (ws stale) (push (list ws stale) reattached))))
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
                   (lambda (&rest args) (push args reattached))))
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

;;;; ---- never-blue: the workspace-switch ensure ---------------------------
;;
;; The switch half of the never-blue requirement: a persp activation sends
;; `openWorkspace' so the daemon binds any on-disk transcript and brings the
;; shim up.  These pin the SKIPS (which is most of the behavior) as hard as
;; the send, because an unskipped send costs the daemon a projects-directory
;; rescan on every switch.

(defmacro agent-repl-test--with-switch-ensure (&rest body)
  "Run BODY with the UDS boundary captured and the link reported UP."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'agent-repl--uds-connected-p) (lambda () t)))
     (agent-repl-test--with-uds ,@body)))

(ert-deftest agent-repl-test-frontend-switch-ensure-sends-open-workspace ()
  "A switch to a session-less workspace sends `openWorkspace' keyed by its cwd."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w")
    (agent-repl-test--with-switch-ensure
      ;; Act
      (agent-repl--frontend-notify-workspace-switch "ws1")
      ;; Assert — the daemon routes purely by cwd, never the persp name.
      (pcase-let ((`(,field ,payload ,ws) (car uds-commands)))
        (should (equal field "openWorkspace"))
        (should (null payload))
        (should (equal ws "/w"))))))

(ert-deftest agent-repl-test-frontend-switch-ensure-skips-when-session-live ()
  "A workspace already driving a live session has nothing to ensure."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w" :frontend-session-id "s_1")
    (cl-letf (((symbol-function 'agent-repl--frontend-session-live-p) (lambda (_id) t)))
      (agent-repl-test--with-switch-ensure
        ;; Act
        (agent-repl--frontend-notify-workspace-switch "ws1")
        ;; Assert — this is THE common case; a send here is pure daemon rescan.
        (should (null uds-commands))))))

;;;; ---- never-blue: the backfill-completion gate (F2) --------------------
;;
;; A LIVE session whose history never arrived is live and blue at once. These
;; pin that liveness alone no longer earns the skip.

(defmacro agent-repl-test--with-backfill (state &rest body)
  "Run BODY with ws1's bound session live and its `backfill' reading STATE.
STATE nil stands for a pre-F2 daemon that sends no field at all."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'agent-repl--uds-connected-p) (lambda () t))
             ((symbol-function 'agent-repl--frontend-session-live-p) (lambda (_id) t))
             ((symbol-function 'agent-repl--frontend-session-view)
              (lambda (_id) (and ,state (list :sessionId "s_1" :backfill ,state)))))
     (agent-repl-test--with-uds ,@body)))

(ert-deftest agent-repl-test-frontend-backfill-settled-when-done ()
  "A delivered transcript is settled."
  (agent-repl-test--with-backfill "BACKFILL_STATE_DONE"
    (should (agent-repl--frontend-backfill-settled-p "s_1"))))

(ert-deftest agent-repl-test-frontend-backfill-settled-when-nothing-to-backfill ()
  "A workspace with no transcript is settled: an empty feed is CORRECT there."
  (agent-repl-test--with-backfill "BACKFILL_STATE_UNSPECIFIED"
    (should (agent-repl--frontend-backfill-settled-p "s_1"))))

(ert-deftest agent-repl-test-frontend-backfill-unsettled-while-pending ()
  "History that has not landed yet is NOT settled."
  (agent-repl-test--with-backfill "BACKFILL_STATE_PENDING"
    (should-not (agent-repl--frontend-backfill-settled-p "s_1"))))

(ert-deftest agent-repl-test-frontend-backfill-unsettled-when-failed ()
  "A failed sidecar read is NOT settled, and must never read as merely not-yet."
  (agent-repl-test--with-backfill "BACKFILL_STATE_FAILED"
    (should-not (agent-repl--frontend-backfill-settled-p "s_1"))))

(ert-deftest agent-repl-test-frontend-backfill-settled-on-a-pre-f2-daemon ()
  "A daemon that sends no field reads as settled.
It cannot backfill on switch either, so retrying would loop for nothing."
  (agent-repl-test--with-backfill nil
    (should (agent-repl--frontend-backfill-settled-p "s_1"))))

(ert-deftest agent-repl-test-frontend-switch-ensure-skips-a-live-backfilled-session ()
  "The steady state: live AND backfilled earns the skip."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w" :frontend-session-id "s_1")
    (agent-repl-test--with-backfill "BACKFILL_STATE_DONE"
      ;; Act
      (agent-repl--frontend-notify-workspace-switch "ws1")
      ;; Assert
      (should (null uds-commands)))))

(ert-deftest agent-repl-test-frontend-switch-ensure-sends-for-a-live-but-unbackfilled-session ()
  "THE residual this closes: live but blue must re-ensure, not skip."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w" :frontend-session-id "s_1")
    (agent-repl-test--with-backfill "BACKFILL_STATE_PENDING"
      ;; Act
      (agent-repl--frontend-notify-workspace-switch "ws1")
      ;; Assert
      (should (equal (car (car uds-commands)) "openWorkspace")))))

(ert-deftest agent-repl-test-frontend-switch-ensure-sends-for-a-failed-backfill ()
  "A failed sidecar read re-ensures rather than being mistaken for done."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w" :frontend-session-id "s_1")
    (agent-repl-test--with-backfill "BACKFILL_STATE_FAILED"
      ;; Act
      (agent-repl--frontend-notify-workspace-switch "ws1")
      ;; Assert
      (should (equal (car (car uds-commands)) "openWorkspace")))))

(ert-deftest agent-repl-test-frontend-switch-ensure-failed-backfill-still-gives-up ()
  "A permanently failing sidecar cannot retry-loop.
The give-up latch is what bounds it: the unsettled backfill would otherwise
re-send on every single switch forever."
  ;; Arrange — unsettled AND already given up.
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w" :frontend-session-id "s_1"
                                    :switch-ensure-failed t)
    (agent-repl-test--with-backfill "BACKFILL_STATE_FAILED"
      ;; Act
      (agent-repl--frontend-notify-workspace-switch "ws1")
      ;; Assert
      (should (null uds-commands)))))

(ert-deftest agent-repl-test-frontend-switch-ensure-failed-backfill-respects-cooldown ()
  "An unsettled backfill still debounces within the cooldown."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w" :frontend-session-id "s_1")
    (agent-repl-test--with-backfill "BACKFILL_STATE_PENDING"
      (agent-repl--frontend-notify-workspace-switch "ws1")
      ;; Act — a rapid re-switch.
      (agent-repl--frontend-notify-workspace-switch "ws1")
      ;; Assert
      (should (= 1 (length uds-commands))))))

(ert-deftest agent-repl-test-frontend-switch-ensure-sends-when-bound-session-dead ()
  "A bound-but-dead session still needs the ensure (that is the blue case)."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w" :frontend-session-id "s_gone")
    (cl-letf (((symbol-function 'agent-repl--frontend-session-live-p) (lambda (_id) nil)))
      (agent-repl-test--with-switch-ensure
        ;; Act
        (agent-repl--frontend-notify-workspace-switch "ws1")
        ;; Assert
        (should (equal (car (car uds-commands)) "openWorkspace"))))))

(ert-deftest agent-repl-test-frontend-switch-ensure-skips-when-link-down ()
  "With the UDS link down the switch sends nothing.
The reattach sweep owns daemon revival; a switch must not race it."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w")
    (cl-letf (((symbol-function 'agent-repl--uds-connected-p) (lambda () nil)))
      (agent-repl-test--with-uds
        ;; Act
        (agent-repl--frontend-notify-workspace-switch "ws1")
        ;; Assert
        (should (null uds-commands))))))

(ert-deftest agent-repl-test-frontend-switch-ensure-skips-without-project-dir ()
  "No cwd means no routable wire key, so the switch sends nothing."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:agent-state :idle)
    (agent-repl-test--with-switch-ensure
      ;; Act — must not signal either; this runs on EVERY switch.
      (agent-repl--frontend-notify-workspace-switch "ws1")
      ;; Assert
      (should (null uds-commands)))))

(ert-deftest agent-repl-test-frontend-switch-ensure-debounces-within-cooldown ()
  "A second switch inside the cooldown does not re-send."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w")
    (agent-repl-test--with-switch-ensure
      (agent-repl--frontend-notify-workspace-switch "ws1")
      ;; Act — rapid re-switch.
      (agent-repl--frontend-notify-workspace-switch "ws1")
      ;; Assert — exactly one command, not two.
      (should (= 1 (length uds-commands))))))

(ert-deftest agent-repl-test-frontend-switch-ensure-resends-after-cooldown ()
  "Once the cooldown has elapsed a switch may ensure again."
  ;; Arrange — a stamp older than the cooldown.
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w")
    (agent-repl--ws-put "ws1" :switch-ensure-at
                        (- (float-time) agent-repl-frontend-switch-ensure-cooldown 1))
    (agent-repl-test--with-switch-ensure
      ;; Act
      (agent-repl--frontend-notify-workspace-switch "ws1")
      ;; Assert
      (should (= 1 (length uds-commands))))))

(ert-deftest agent-repl-test-frontend-switch-ensure-skips-after-give-up ()
  "A workspace that gave up stops sending entirely."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w" :switch-ensure-failed t)
    (agent-repl-test--with-switch-ensure
      ;; Act
      (agent-repl--frontend-notify-workspace-switch "ws1")
      ;; Assert
      (should (null uds-commands)))))

(ert-deftest agent-repl-test-frontend-switch-ensure-counts-a-failed-ack ()
  "A rejected ack increments the workspace's failure tally."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w")
    ;; Act
    (agent-repl--frontend-note-switch-ensure-failure "ws1" "no live session")
    ;; Assert
    (should (= 1 (agent-repl--ws-get "ws1" :switch-ensure-failures)))))

(ert-deftest agent-repl-test-frontend-switch-ensure-gives-up-at-the-cap ()
  "At the failure cap the workspace latches `:switch-ensure-failed'."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w")
    (let ((display-warning-minimum-level :emergency))
      ;; Act — one short of the cap, then the one that trips it.
      (dotimes (_ agent-repl-frontend-switch-ensure-max-failures)
        (agent-repl--frontend-note-switch-ensure-failure "ws1" "boom"))
      ;; Assert — the retry-loop guard the directive asks for.
      (should (agent-repl--ws-get "ws1" :switch-ensure-failed)))))

(ert-deftest agent-repl-test-frontend-switch-ensure-does-not-give-up-early ()
  "Below the cap the workspace keeps trying."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w")
    ;; Act
    (agent-repl--frontend-note-switch-ensure-failure "ws1" "boom")
    ;; Assert
    (should-not (agent-repl--ws-get "ws1" :switch-ensure-failed))))

(ert-deftest agent-repl-test-frontend-switch-ensure-boot-change-clears-give-up ()
  "A new daemon instance earns a workspace fresh switch-ensure attempts."
  ;; Arrange — a give-up that belonged to the PREVIOUS instance.
  (agent-repl-test--with-ws "ws1" '(:switch-ensure-failed t :switch-ensure-failures 3)
    (cl-letf (((symbol-function 'agent-repl--live-ws-names) (lambda () '("ws1"))))
      (let ((agent-repl--frontend-last-boot-id "boot-old"))
        ;; Act
        (agent-repl--frontend-note-boot-id "boot-new")
        ;; Assert
        (should-not (agent-repl--ws-get "ws1" :switch-ensure-failed))
        (should-not (agent-repl--ws-get "ws1" :switch-ensure-failures))))))

(ert-deftest agent-repl-test-frontend-switch-ensure-boot-change-clears-cooldown ()
  "A daemon bounce also clears the cooldown stamp.
Otherwise the first switch after a restart would be swallowed by a timer
belonging to the instance that is already gone."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:switch-ensure-at 12345.0)
    (cl-letf (((symbol-function 'agent-repl--live-ws-names) (lambda () '("ws1"))))
      (let ((agent-repl--frontend-last-boot-id "boot-old"))
        ;; Act
        (agent-repl--frontend-note-boot-id "boot-new")
        ;; Assert
        (should-not (agent-repl--ws-get "ws1" :switch-ensure-at))))))

(ert-deftest agent-repl-test-frontend-switch-ensure-never-signals-on-send-failure ()
  "A send that signals is logged, never raised.
This runs on the persp-activation path, so a signal would strand the
switch before the tail that flips `:ws-loaded'.  The link can die between
the connected-p check and the send, so the skip guards cannot be the only
protection."
  ;; Arrange — connected, then the send blows up anyway.
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w")
    (cl-letf (((symbol-function 'agent-repl--uds-connected-p) (lambda () t))
              ((symbol-function 'agent-repl--uds-send-command)
               (lambda (&rest _) (user-error "not connected"))))
      ;; Act / Assert — must return nil rather than signalling.
      (should (null (agent-repl--frontend-notify-workspace-switch "ws1"))))))

(ert-deftest agent-repl-test-frontend-switch-ensure-never-signals-without-cwd ()
  "A workspace whose cwd lookup signals is skipped, not raised."
  ;; Arrange — no :project-dir at all, which `--ws-dir' signals on.
  (agent-repl-test--with-ws "ws1" '(:agent-state :idle)
    (cl-letf (((symbol-function 'agent-repl--uds-connected-p) (lambda () t)))
      ;; Act / Assert
      (should (null (agent-repl--frontend-notify-workspace-switch "ws1"))))))

(ert-deftest agent-repl-test-frontend-switch-ensure-stamps-before-sending ()
  "The cooldown stamp is written on the send, which is what debounces it."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w")
    (agent-repl-test--with-switch-ensure
      ;; Act
      (agent-repl--frontend-notify-workspace-switch "ws1")
      ;; Assert
      (should (agent-repl--ws-get "ws1" :switch-ensure-at)))))

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

;; The GET /commands fetch + POST /commands/refresh tests were deleted in
;; the S9 slash-menu cutover: those HTTP calls are gone.  The slash-command
;; menu is now the pushed `SessionInitView' (covered in test-frontend-state.el
;; for the store and test-input.el for the completion source).

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
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w" :frontend-session-id "s1" :next-send-origin "merge")
    (cl-letf (((symbol-function 'agent-repl--frontend-ensure-session) (lambda (_ws) "s1"))
              ((symbol-function 'agent-repl--frontend-sync-webview) (lambda (&rest _) nil)))
      (agent-repl-test--with-uds
        ;; Act
        (agent-repl--frontend-send-user-message "ws1" "rebase")
        ;; Assert — the one-shot flag is cleared (not forwarded on the wire).
        (should (null (agent-repl--ws-get "ws1" :next-send-origin)))))))

(ert-deftest agent-repl-test-frontend-send-user-message-submits-text ()
  "Send-user-message sends `submitPrompt' with the text, keyed by the workspace CWD."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w" :frontend-session-id "s1")
    (cl-letf (((symbol-function 'agent-repl--frontend-ensure-session) (lambda (_ws) "s1"))
              ((symbol-function 'agent-repl--frontend-sync-webview) (lambda (&rest _) nil)))
      (agent-repl-test--with-uds
        ;; Act
        (agent-repl--frontend-send-user-message "ws1" "a normal prompt")
        ;; Assert
        (pcase-let ((`(,field ,payload ,ws) (car uds-commands)))
          (should (equal field "submitPrompt"))
          (should (equal (plist-get payload :text) "a normal prompt"))
          (should (equal ws "/w")))))))

(provide 'test-frontend-client)

;;; test-frontend-client.el ends here

;;;; ---- gui interrupt (UDS `interrupt', keyed by workspace) --------------------
;;
;; The retract half of `C-c C-k' is gone: frontend.v1's `InterruptCmd' carries
;; only `hard' (no retract id), and the daemon's HTTP interrupt already reported
;; retracted=false post-cutover.  So gui-interrupt now just dispatches an
;; `interrupt' command keyed by the workspace and always returns t.

(ert-deftest agent-repl-test-gui-interrupt-sends-command-keyed-by-workspace ()
  "Interrupt dispatches the UDS `interrupt' command keyed by the workspace CWD."
  ;; Arrange
  (agent-repl-test--with-ws "ws1"
      '(:project-dir "/w" :frontend-session-id "s_1" :sent-turn (:request-id "r_9" :raw "draft"))
    (agent-repl-test--with-uds
      ;; Act
      (agent-repl--gui-interrupt "ws1" 'escape)
      ;; Assert
      (pcase-let ((`(,field ,_payload ,ws) (car uds-commands)))
        (should (equal field "interrupt"))
        (should (equal ws "/w"))))))

(ert-deftest agent-repl-test-gui-interrupt-returns-t ()
  "Both gestures return t (dispatched); the retract verdict is gone."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w" :frontend-session-id "s_1")
    (agent-repl-test--with-uds
      ;; Act / Assert
      (should (eq (agent-repl--gui-interrupt "ws1" 'escape) t))
      (should (eq (agent-repl--gui-interrupt "ws1" 'ctrl-c) t)))))

;;;; ---- the interrupt confirmation challenge -----------------------------
;;
;; The daemon refuses an interrupt that would stop live SUBAGENTS with
;; `CommandAck.interrupt_confirm_required' — a CHALLENGE, not an error: the
;; command was understood and deliberately not performed.  These drive the
;; REAL ack handler (only the socket write is shadowed) so the routing from
;; ack arm to minibuffer question to resend is covered end to end.

(defmacro agent-repl-test--with-interrupt-acks (answer &rest body)
  "Run BODY with the interrupt round trip observable and `y-or-n-p' stubbed.
ANSWER is what the stubbed prompt returns; the questions it was asked
accumulate in `asked' (newest last) and the sent commands in
`uds-commands' as (FIELD PAYLOAD WORKSPACE), both anaphoric.  The command
tracking table is real (and cleared first), so acks route exactly as they
do live; `message' is silenced but captured in `echoed'."
  (declare (indent 1))
  `(let ((uds-commands '()) (uds-counter 0) (asked '()) (echoed nil))
     (ignore uds-commands asked echoed)
     (clrhash agent-repl--uds-pending-commands)
     (cl-letf (((symbol-function 'agent-repl--uds-send-command)
                (lambda (field payload &optional workspace &rest _)
                  (setq uds-commands
                        (append uds-commands (list (list field payload workspace))))
                  (format "req-%d" (cl-incf uds-counter))))
               ((symbol-function 'y-or-n-p)
                (lambda (question)
                  (setq asked (append asked (list question)))
                  ,answer))
               ((symbol-function 'message)
                (lambda (fmt &rest args) (setq echoed (apply #'format fmt args)))))
       ,@body)))

(ert-deftest agent-repl-test-gui-interrupt-challenge-yes-resends-confirmed ()
  "A yes to the challenge resends the interrupt carrying `confirmAgents'."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w" :frontend-session-id "s_1")
    (agent-repl-test--with-interrupt-acks t
      (agent-repl--gui-interrupt "ws1" 'escape)
      ;; Act — the daemon challenges the first (unconfirmed) send
      (agent-repl--uds-handle-command-ack
       '(:requestId "req-1" :interruptConfirmRequired (:liveTasks "3")))
      ;; Assert — a second interrupt goes out, confirmed, on the same key
      (should (equal (length uds-commands) 2))
      (should (equal (nth 1 uds-commands) '("interrupt" (:confirmAgents t) "/w"))))))

(ert-deftest agent-repl-test-gui-interrupt-challenge-no-sends-nothing ()
  "A no to the challenge sends nothing further — the subagents keep running."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w" :frontend-session-id "s_1")
    (agent-repl-test--with-interrupt-acks nil
      (agent-repl--gui-interrupt "ws1" 'escape)
      ;; Act
      (agent-repl--uds-handle-command-ack
       '(:requestId "req-1" :interruptConfirmRequired (:liveTasks "3")))
      ;; Assert
      (should (equal (length uds-commands) 1)))))

(ert-deftest agent-repl-test-gui-interrupt-challenge-question-names-the-stakes ()
  "The question counts the subagents it would stop, in the wire's own shapes."
  ;; Arrange — (liveTasks . expected question)
  (dolist (case '(("3" . "Interrupt 3 running subagents? ")
                  ("1" . "Interrupt 1 running subagent? ")
                  (2 . "Interrupt 2 running subagents? ")
                  (nil . "Interrupt the running subagents? ")))
    (agent-repl-test--with-ws "ws1" '(:project-dir "/w" :frontend-session-id "s_1")
      (agent-repl-test--with-interrupt-acks nil
        (agent-repl--gui-interrupt "ws1" 'escape)
        ;; Act
        (agent-repl--uds-handle-command-ack
         (list :requestId "req-1"
               :interruptConfirmRequired (list :liveTasks (car case))))
        ;; Assert
        (should (equal asked (list (cdr case))))))))

(ert-deftest agent-repl-test-gui-interrupt-ok-ack-never-prompts ()
  "An accepted interrupt is done: no question, no resend."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w" :frontend-session-id "s_1")
    (agent-repl-test--with-interrupt-acks t
      (agent-repl--gui-interrupt "ws1" 'escape)
      ;; Act
      (agent-repl--uds-handle-command-ack '(:requestId "req-1" :ok t))
      ;; Assert
      (should-not asked)
      (should (equal (length uds-commands) 1)))))

(ert-deftest agent-repl-test-gui-interrupt-error-ack-still-surfaces ()
  "A genuine error ack keeps the old failure path: echoed, never a question."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w" :frontend-session-id "s_1")
    (agent-repl-test--with-interrupt-acks t
      (agent-repl--gui-interrupt "ws1" 'escape)
      ;; Act — protojson omits ok=false, so a rejection arrives with no :ok
      (agent-repl--uds-handle-command-ack
       '(:requestId "req-1" :error "no live session to drive"))
      ;; Assert
      (should-not asked)
      (should (equal (length uds-commands) 1))
      (should (string-match-p "interrupt" echoed))
      (should (string-match-p "no live session to drive" echoed)))))

(ert-deftest agent-repl-test-gui-interrupt-confirmed-resend-is-not-rechallenged ()
  "The confirmed resend carries no challenge handler — a re-challenge cannot loop."
  ;; Arrange
  (agent-repl-test--with-ws "ws1" '(:project-dir "/w" :frontend-session-id "s_1")
    (agent-repl-test--with-interrupt-acks t
      (agent-repl--gui-interrupt "ws1" 'escape)
      (agent-repl--uds-handle-command-ack
       '(:requestId "req-1" :interruptConfirmRequired (:liveTasks "3")))
      ;; Act — the daemon contradicts itself and challenges the confirmed send
      (agent-repl--uds-handle-command-ack
       '(:requestId "req-2" :interruptConfirmRequired (:liveTasks "3")))
      ;; Assert — asked once, sent twice: no third command, no second question
      (should (equal (length asked) 1))
      (should (equal (length uds-commands) 2)))))

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

;;;; ---- the workspace wire key -------------------------------------------
;;
;; The daemon routes ws-keyed commands by matching the `workspace' field
;; against the session's CWD.  Emacs keys everything by the persp NAME, so the
;; resolver is what keeps the name off the wire — the UDS command cutover
;; shipped without it and every prompt NACKed as "no live session to drive"
;; (2026-07-25).

(ert-deftest agent-repl-test-frontend-ws-command-key-is-the-project-dir ()
  "The wire key is WS's `:project-dir', NEVER the persp name."
  ;; Arrange
  (agent-repl-test--with-ws "doom" '(:project-dir "/Users/x/.config/doom")
    ;; Act / Assert
    (should (equal (agent-repl--frontend-ws-command-key "doom")
                   "/Users/x/.config/doom"))))

(ert-deftest agent-repl-test-frontend-ws-command-key-signals-without-project-dir ()
  "A workspace with no `:project-dir' fails loudly rather than sending its name."
  ;; Arrange
  (agent-repl-test--with-ws "doom" '(:frontend-session-id "s1")
    ;; Act / Assert
    (should-error (agent-repl--frontend-ws-command-key "doom"))))
