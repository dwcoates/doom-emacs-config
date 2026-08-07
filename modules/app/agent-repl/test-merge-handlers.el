;;; test-merge-handlers.el --- Tests for merge-handlers.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for Emacs's ENTIRE merge surface: two bare `mergeWorkspace'
;; requests keyed by workspace.  Covers:
;;   - `agent-repl--merge-command-payload' (shape + the unkeyed-request error)
;;   - `agent-repl--merge-dispatch-over-uds' (routing, tracking, host action)
;;   - `agent-repl--merge-resume-over-uds' (the resolve-and-continue handoff)
;;
;; The absence tests matter as much as the presence tests: geometry,
;; handler selection, and dispatch bookkeeping all left Emacs when the
;; daemon took ownership of merging, and a payload that quietly regrows a
;; `sourceDir' is a second owner of the workspace->worktree map.

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Fixture helpers ----

(defmacro agent-repl-test--with-captured-merge-command (sent-var &rest body)
  "Run BODY with `--uds-send-command' capturing into SENT-VAR.
SENT-VAR is bound to `(:field F :payload P :ws W)' for the last command
sent.  The host-action helpers are stubbed inert so a test that does not
care about them is not forced to mock them."
  (declare (indent 1))
  `(let (,sent-var)
     (cl-letf (((symbol-function 'agent-repl--uds-send-command)
                (agent-repl-test--send-command-stub
                 "req-1"
                 (lambda (call)
                   (setq ,sent-var (list :field (plist-get call :field)
                                         :payload (plist-get call :payload)
                                         :ws (plist-get call :workspace))))))
               ((symbol-function 'agent-repl--host-action-defer)
                (lambda (&rest _) nil))
               ((symbol-function 'agent-repl--host-action-settle)
                (lambda (&rest _) nil)))
       ,@body)))

;;;; ---- Tests: merge-command-payload ----

(ert-deftest agent-repl-test-merge-payload-carries-only-the-workspace-name ()
  "A merge request names its workspace and nothing else."
  (should (equal (agent-repl--merge-command-payload "DWC/foo")
                 '(:workspaceName "DWC/foo"))))

(ert-deftest agent-repl-test-merge-payload-resume-sets-continue-flag ()
  "The resume payload adds `conflictResolvedContinue' and nothing else."
  (should (equal (agent-repl--merge-command-payload "DWC/foo" 'resume)
                 '(:conflictResolvedContinue t :workspaceName "DWC/foo"))))

(ert-deftest agent-repl-test-merge-payload-carries-no-geometry ()
  "The payload NEVER carries geometry — the daemon owns that map alone."
  (let ((payload (agent-repl--merge-command-payload "DWC/foo")))
    (dolist (key '(:sourceBranch :sourceDir :targetDir))
      (should-not (plist-member payload key)))))

(ert-deftest agent-repl-test-merge-payload-carries-no-handler ()
  "The payload NEVER names a handler — every merge is the same request."
  (should-not (plist-member (agent-repl--merge-command-payload "DWC/foo")
                            :handler)))

(ert-deftest agent-repl-test-merge-payload-refuses-nil-workspace ()
  "An unkeyed merge request signals rather than filing state under nothing."
  (should-error (agent-repl--merge-command-payload nil) :type 'user-error))

(ert-deftest agent-repl-test-merge-payload-refuses-empty-workspace ()
  "A blank workspace name is refused for the same reason nil is."
  (should-error (agent-repl--merge-command-payload "") :type 'user-error))

(ert-deftest agent-repl-test-merge-payload-logs-the-refusal ()
  "The refused request is recorded through the canonical log helper."
  (let (logged)
    (cl-letf (((symbol-function 'agent-repl--log)
               (lambda (_ws fmt &rest args)
                 (push (apply #'format fmt args) logged))))
      (ignore-errors (agent-repl--merge-command-payload nil)))
    (should (cl-some (lambda (line)
                       (string-match-p "refusing merge request" line))
                     logged))))

;;;; ---- Tests: merge-dispatch-over-uds ----

(ert-deftest agent-repl-test-merge-dispatch-sends-the-merge-field ()
  "The dispatch sends the `mergeWorkspace' command field."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "DWC/foo" :project-dir "/src")
    (agent-repl-test--with-captured-merge-command sent
      (agent-repl--merge-dispatch-over-uds "DWC/foo")
      (should (equal (plist-get sent :field) "mergeWorkspace")))))

(ert-deftest agent-repl-test-merge-dispatch-routes-by-the-command-key ()
  "The envelope routes on the workspace COMMAND KEY, not the display name.
Sending the bare name here filed the daemon's merge state rows under a
workspace key nothing else used, so their WorkspaceState carried no
connectivity verdict and Emacs refused the frame."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "DWC/foo" :project-dir "/src")
    (agent-repl-test--with-captured-merge-command sent
      (agent-repl--merge-dispatch-over-uds "DWC/foo")
      (should (equal (plist-get sent :ws) "/src")))))

(ert-deftest agent-repl-test-merge-dispatch-names-the-workspace-in-the-payload ()
  "The DISPLAY name rides `:workspaceName' beside the routing key."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "DWC/foo" :project-dir "/src")
    (agent-repl-test--with-captured-merge-command sent
      (agent-repl--merge-dispatch-over-uds "DWC/foo")
      (should (equal (plist-get (plist-get sent :payload) :workspaceName)
                     "DWC/foo")))))

(ert-deftest agent-repl-test-merge-dispatch-sends-no-geometry ()
  "The dispatch reads no `:project-dir'-derived geometry onto the wire."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "DWC/foo" :project-dir "/src")
    (agent-repl-test--with-captured-merge-command sent
      (agent-repl--merge-dispatch-over-uds "DWC/foo")
      (should (equal (plist-get sent :payload) '(:workspaceName "DWC/foo"))))))

(ert-deftest agent-repl-test-merge-dispatch-records-no-merge-state ()
  "The dispatch leaves no merge bookkeeping on the workspace plist."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "DWC/foo" :project-dir "/src")
    (agent-repl-test--with-captured-merge-command _sent
      (agent-repl--merge-dispatch-over-uds "DWC/foo")
      (dolist (key '(:daemon-merge-dispatched :resolved-target-dir
                     :merge-target-branch :merge-base))
        (should-not (agent-repl--ws-get "DWC/foo" key))))))

(ert-deftest agent-repl-test-merge-dispatch-tracks-the-request ()
  "The dispatch hands the send its ack callbacks so a rejection surfaces."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "DWC/foo" :project-dir "/src")
    (let (call)
      (cl-letf (((symbol-function 'agent-repl--uds-send-command)
                 (agent-repl-test--send-command-stub
                  "req-9" (lambda (c) (setq call c))))
                ((symbol-function 'agent-repl--host-action-defer)
                 (lambda (&rest _) nil)))
        (agent-repl--merge-dispatch-over-uds "DWC/foo")
        (should (equal (plist-get call :field) "mergeWorkspace"))
        (should (functionp (plist-get call :on-failure)))
        (should (functionp (plist-get call :on-success)))))))

(ert-deftest agent-repl-test-merge-dispatch-returns-the-request-id ()
  "The dispatch returns the request-id its caller correlates on."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "DWC/foo" :project-dir "/src")
    (agent-repl-test--with-captured-merge-command _sent
      (should (equal (agent-repl--merge-dispatch-over-uds "DWC/foo") "req-1")))))

;;;; ---- Tests: the host action waits for the merge's own outcome ----
;;
;; The dispatch returns long before the daemon answers, so a host action
;; completed on the dispatch reports a merge that has not happened yet.

(ert-deftest agent-repl-test-merge-dispatch-defers-its-host-action ()
  "The dispatch declares its outcome unknown rather than letting it read OK."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "DWC/foo" :project-dir "/src")
    (let (deferred)
      (cl-letf (((symbol-function 'agent-repl--uds-send-command)
                 (agent-repl-test--send-command-stub "req-9"))
                ((symbol-function 'agent-repl--host-action-defer)
                 (lambda (token) (setq deferred token))))
        (agent-repl--merge-dispatch-over-uds "DWC/foo")
        (should (equal deferred "req-9"))))))

(ert-deftest agent-repl-test-merge-dispatch-defers-before-the-frame-is-written ()
  "The host-action deferral is declared BEFORE the command reaches the socket.
An ack the socket delivers reentrantly inside `process-send-string' would
otherwise settle an action nothing had yet deferred."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "DWC/foo" :project-dir "/src")
    (let (order)
      (cl-letf (((symbol-function 'agent-repl--uds-send-command)
                 (lambda (_field _payload &optional _ws _proc &rest keys)
                   (funcall (plist-get keys :on-registered) "req-9")
                   (push 'written order)
                   "req-9"))
                ((symbol-function 'agent-repl--host-action-defer)
                 (lambda (_token) (push 'deferred order))))
        (agent-repl--merge-dispatch-over-uds "DWC/foo")
        (should (equal (nreverse order) '(deferred written)))))))

(ert-deftest agent-repl-test-merge-ack-failure-settles-the-host-action ()
  "A rejected merge ack completes the deferred host action as FAILED."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "DWC/foo" :project-dir "/src")
    (let (call settled)
      (cl-letf (((symbol-function 'agent-repl--uds-send-command)
                 (agent-repl-test--send-command-stub
                  "req-1" (lambda (c) (setq call c))))
                ((symbol-function 'agent-repl--host-action-defer)
                 (lambda (_token) nil))
                ((symbol-function 'agent-repl--host-action-settle)
                 (lambda (token ok text) (setq settled (list token ok text)))))
        (agent-repl--merge-dispatch-over-uds "DWC/foo")
        (funcall (plist-get call :on-failure) "resolve dirs: not wired")
        (should (equal settled '("req-1" nil "resolve dirs: not wired")))))))

(ert-deftest agent-repl-test-merge-ack-success-settles-the-host-action ()
  "An accepted merge ack completes the deferred host action as OK."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "DWC/foo" :project-dir "/src")
    (let (call settled)
      (cl-letf (((symbol-function 'agent-repl--uds-send-command)
                 (agent-repl-test--send-command-stub
                  "req-1" (lambda (c) (setq call c))))
                ((symbol-function 'agent-repl--host-action-defer)
                 (lambda (_token) nil))
                ((symbol-function 'agent-repl--host-action-settle)
                 (lambda (token ok text) (setq settled (list token ok text)))))
        (agent-repl--merge-dispatch-over-uds "DWC/foo")
        (funcall (plist-get call :on-success))
        (should (equal settled '("req-1" t nil)))))))

(ert-deftest agent-repl-test-merge-ack-failure-logs-the-rejection ()
  "A rejected ack is recorded through the canonical log helper."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "DWC/foo" :project-dir "/src")
    (let (call logged)
      (cl-letf (((symbol-function 'agent-repl--uds-send-command)
                 (agent-repl-test--send-command-stub
                  "req-1" (lambda (c) (setq call c))))
                ((symbol-function 'agent-repl--host-action-defer)
                 (lambda (_token) nil))
                ((symbol-function 'agent-repl--host-action-settle)
                 (lambda (&rest _) nil))
                ((symbol-function 'agent-repl--log)
                 (lambda (_ws fmt &rest args)
                   (push (apply #'format fmt args) logged)))
                ;; A refused merge is a UX regression, so the rejection rides
                ;; the warn rung; capture both so this test keeps asserting
                ;; the CONTENT rather than the severity.
                ((symbol-function 'agent-repl--warn)
                 (lambda (_ws fmt &rest args)
                   (push (apply #'format fmt args) logged))))
        (agent-repl--merge-dispatch-over-uds "DWC/foo")
        (funcall (plist-get call :on-failure) "branch not found")
        (should (cl-some (lambda (line)
                           (and (string-match-p "REJECTED" line)
                                (string-match-p "branch not found" line)))
                         logged))))))

(ert-deftest agent-repl-test-merge-dispatch-propagates-the-payload-error ()
  "An unkeyed dispatch aborts BEFORE any frame is written."
  (agent-repl-test--with-clean-state
    (let (sent)
      (cl-letf (((symbol-function 'agent-repl--uds-send-command)
                 (lambda (&rest _) (setq sent t) "req-1"))
                ((symbol-function 'agent-repl--host-action-defer)
                 (lambda (&rest _) nil)))
        (should-error (agent-repl--merge-dispatch-over-uds nil)
                      :type 'user-error)
        (should-not sent)))))

;;;; ---- Tests: resolve-and-continue over UDS ----

(ert-deftest agent-repl-test-resume-sets-the-continue-flag ()
  "Resume sends `mergeWorkspace' with `conflictResolvedContinue'."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "DWC/foo" :project-dir "/src")
    (agent-repl-test--with-captured-merge-command sent
      (agent-repl--merge-resume-over-uds "DWC/foo")
      (should (equal (plist-get sent :field) "mergeWorkspace"))
      (should (eq (plist-get (plist-get sent :payload)
                             :conflictResolvedContinue)
                  t)))))

(ert-deftest agent-repl-test-resume-is-routed-by-the-command-key-too ()
  "The resume path routes on the command key and names the workspace."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "DWC/foo" :project-dir "/src")
    (agent-repl-test--with-captured-merge-command sent
      (agent-repl--merge-resume-over-uds "DWC/foo")
      (should (equal (plist-get sent :ws) "/src"))
      (should (equal (plist-get (plist-get sent :payload) :workspaceName)
                     "DWC/foo")))))

(ert-deftest agent-repl-test-resume-sends-no-geometry ()
  "Resume re-supplies nothing: the daemon still holds the geometry."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "DWC/foo" :project-dir "/src")
    (agent-repl-test--with-captured-merge-command sent
      (agent-repl--merge-resume-over-uds "DWC/foo")
      (let ((payload (plist-get sent :payload)))
        (dolist (key '(:sourceBranch :sourceDir :targetDir :handler))
          (should-not (plist-member payload key)))))))

(ert-deftest agent-repl-test-resume-tracks-the-request ()
  "Resume returns the request-id the transport registered before writing."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "DWC/foo" :project-dir "/src")
    (let (call)
      (cl-letf (((symbol-function 'agent-repl--uds-send-command)
                 (agent-repl-test--send-command-stub
                  "req-7" (lambda (c) (setq call c)))))
        (should (equal (agent-repl--merge-resume-over-uds "DWC/foo") "req-7"))
        (should (equal (plist-get call :field) "mergeWorkspace"))))))

(ert-deftest agent-repl-test-resume-propagates-the-payload-error ()
  "An unkeyed resume aborts BEFORE any frame is written."
  (agent-repl-test--with-clean-state
    (let (sent)
      (cl-letf (((symbol-function 'agent-repl--uds-send-command)
                 (lambda (&rest _) (setq sent t) "req-1")))
        (should-error (agent-repl--merge-resume-over-uds "") :type 'user-error)
        (should-not sent)))))

;;;; ---- Tests: minibuffer feedback ----

(defmacro agent-repl-test--with-captured-messages (messages-var &rest body)
  "Run BODY with `message' capturing formatted strings into MESSAGES-VAR."
  (declare (indent 1))
  `(let (,messages-var)
     (cl-letf (((symbol-function 'message)
                (lambda (fmt &rest args)
                  (push (apply #'format fmt args) ,messages-var))))
       ,@body)))

(ert-deftest agent-repl-test-merge-dispatch-echoes-the-request ()
  "Issuing a merge immediately says so in the minibuffer."
  (agent-repl-test--with-captured-messages msgs
    (agent-repl-test--with-captured-merge-command sent
      (cl-letf (((symbol-function 'agent-repl--frontend-ws-command-key)
                 (lambda (ws) ws)))
        (agent-repl--merge-dispatch-over-uds "ws1")))
    (should (cl-some (lambda (m) (string-match-p "merge of ws1 requested" m))
                     msgs))))

(ert-deftest agent-repl-test-merge-dispatch-echoes-a-rejection ()
  "A rejected merge command's error reaches the minibuffer."
  (agent-repl-test--with-captured-messages msgs
    (let (call)
      (cl-letf (((symbol-function 'agent-repl--uds-send-command)
                 (agent-repl-test--send-command-stub
                  "req-1" (lambda (c) (setq call c))))
                ((symbol-function 'agent-repl--frontend-ws-command-key)
                 (lambda (ws) ws))
                ((symbol-function 'agent-repl--host-action-defer)
                 (lambda (&rest _) nil))
                ((symbol-function 'agent-repl--host-action-settle)
                 (lambda (&rest _) nil)))
        (agent-repl--merge-dispatch-over-uds "ws1")
        (funcall (plist-get call :on-failure) "lease unavailable")))
    (should (cl-some (lambda (m)
                       (string-match-p "merge of ws1 refused: lease unavailable" m))
                     msgs))))

(defmacro agent-repl-test--with-merge-echo (status last &rest body)
  "Run BODY with the narrator's workspace reads stubbed.
STATUS is what `:pushed-merge-status' answers, LAST what
`:merge-echo-last' answers; `:pushed-render-state-meta' answers nil.
`agent-repl--ws-put' is stubbed inert so narrating does not stub-create a
workspace entry."
  (declare (indent 2))
  `(cl-letf (((symbol-function 'agent-repl--ws-get)
              (lambda (_ws key)
                (pcase key
                  (:pushed-merge-status ,status)
                  (:merge-echo-last ,last))))
             ((symbol-function 'agent-repl--ws-put) (lambda (&rest _) nil)))
     ,@body))

(ert-deftest agent-repl-test-merge-echo-narrates-a-merge-phase ()
  "A pushed merge-phase transition is echoed in the minibuffer."
  (agent-repl-test--with-captured-messages msgs
    (agent-repl-test--with-merge-echo nil nil
      (agent-repl--merge-echo-pushed-state "ws1" :merging :merge-queued))
    (should (equal msgs '("agent-repl: ws1 merging")))))

(ert-deftest agent-repl-test-merge-echo-carries-the-failure-cause ()
  "A `:merge-failed' echo includes the daemon's cause, prefix stripped."
  (agent-repl-test--with-captured-messages msgs
    (cl-letf (((symbol-function 'agent-repl--ws-get)
               (lambda (_ws key)
                 (when (eq key :pushed-render-state-meta)
                   '(:cause-kind "merge_transition:shim lease unavailable"))))
              ((symbol-function 'agent-repl--ws-put) (lambda (&rest _) nil)))
      (agent-repl--merge-echo-pushed-state "ws1" :merge-failed :merge-enqueuing))
    (should (equal msgs
                   '("agent-repl: ws1 merge failed — shim lease unavailable")))))

(ert-deftest agent-repl-test-merge-echo-skips-a-same-state-repush ()
  "Re-pushing the same merge state does not spam the minibuffer."
  (agent-repl-test--with-captured-messages msgs
    (agent-repl-test--with-merge-echo nil nil
      (agent-repl--merge-echo-pushed-state "ws1" :merging :merging))
    (should-not msgs)))

(ert-deftest agent-repl-test-merge-echo-ignores-non-merge-states ()
  "Ordinary render states are not the merge narrator's business."
  (agent-repl-test--with-captured-messages msgs
    (agent-repl-test--with-merge-echo nil nil
      (agent-repl--merge-echo-pushed-state "ws1" :thinking :ready))
    (should-not msgs)))

(ert-deftest agent-repl-test-merge-echo-states-are-the-render-maps-merge-arm ()
  "The narrated set IS the render map's merge arm, not a copy of it.
Derived rather than restated so a merge state the daemon adds narrates
itself instead of silently going unmentioned."
  (should (equal (sort (copy-sequence agent-repl--merge-echo-states) #'string<)
                 (sort (list :merge-enqueuing :merge-queued :merging
                             :merge-conflict :merge-failed :merged)
                       #'string<))))

;;;; ---- Tests: the narration reads MergeStatus ----

(ert-deftest agent-repl-test-merge-echo-narrates-the-queue-position ()
  "An enqueued status reports where in its repository's queue it sits."
  (agent-repl-test--with-captured-messages msgs
    (agent-repl-test--with-merge-echo
        '(:phase :enqueued :position 2 :depth 5) nil
      (agent-repl--merge-echo-pushed-state "ws1" :merge-queued :merge-enqueuing))
    (should (equal msgs '("agent-repl: ws1 merge queued — position 2 of 5")))))

(ert-deftest agent-repl-test-merge-echo-narrates-the-pick-progress ()
  "A cherry-picking status reports landed/total and the commit on the table."
  (agent-repl-test--with-captured-messages msgs
    (agent-repl-test--with-merge-echo
        '(:phase :cherry-picking :commits-total 4 :commits-landed 1
          :current-subject "fix the widget")
        nil
      (agent-repl--merge-echo-pushed-state "ws1" :merging :merge-queued))
    (should (equal msgs
                   '("agent-repl: ws1 cherry-picking — 1/4 commits, fix the widget")))))

(ert-deftest agent-repl-test-merge-echo-narrates-the-testing-phase ()
  "A testing status is narrated as testing, not as another pick."
  (agent-repl-test--with-captured-messages msgs
    (agent-repl-test--with-merge-echo
        '(:phase :testing :commits-total 4 :commits-landed 2) nil
      (agent-repl--merge-echo-pushed-state "ws1" :merging :merge-queued))
    (should (equal msgs '("agent-repl: ws1 testing its merge — 2/4 commits")))))

(ert-deftest agent-repl-test-merge-echo-narrates-the-pre-merge-action ()
  "A before-action status quotes the action the daemon is running."
  (agent-repl-test--with-captured-messages msgs
    (agent-repl-test--with-merge-echo
        '(:phase :before-action :prompt "bump the version") nil
      (agent-repl--merge-echo-pushed-state "ws1" :merging :merge-queued))
    (should (equal msgs
                   '("agent-repl: ws1 running its pre-merge action — bump the version")))))

(ert-deftest agent-repl-test-merge-echo-narrates-the-post-merge-action ()
  "An after-action status quotes the action the daemon is running."
  (agent-repl-test--with-captured-messages msgs
    (agent-repl-test--with-merge-echo
        '(:phase :after-action :prompt "run the suite") nil
      (agent-repl--merge-echo-pushed-state "ws1" :merging :merge-queued))
    (should (equal msgs
                   '("agent-repl: ws1 running its post-merge action — run the suite")))))

(ert-deftest agent-repl-test-merge-echo-narrates-the-conflicted-subject ()
  "A conflict names the commit that conflicted."
  (agent-repl-test--with-captured-messages msgs
    (agent-repl-test--with-merge-echo
        '(:phase :conflict :conflicted-subject "rename the widget"
          :commits-total 4 :commits-landed 2)
        nil
      (agent-repl--merge-echo-pushed-state "ws1" :merge-conflict :merging))
    (should (equal msgs
                   '("agent-repl: ws1 merge conflicted — rename the widget, 2/4 commits")))))

(ert-deftest agent-repl-test-merge-echo-narrates-the-structured-failure-cause ()
  "A failed status reports the daemon's own cause, not the routing prefix."
  (agent-repl-test--with-captured-messages msgs
    (agent-repl-test--with-merge-echo
        '(:phase :failed :cause "lease unavailable") nil
      (agent-repl--merge-echo-pushed-state "ws1" :merge-failed :merging))
    (should (equal msgs '("agent-repl: ws1 merge failed — lease unavailable")))))

(ert-deftest agent-repl-test-merge-echo-reports-the-failure-record ()
  "A failed status reports the arm's whole JSON record as its own field.
The prose clauses quote the cause and the failing subject; the record is
what carries the counts and the sha a diagnosis needs."
  (agent-repl-test--with-captured-messages msgs
    (agent-repl-test--with-merge-echo
        '(:phase :failed :cause "lease unavailable"
          :failed-json "{\"cause\":\"lease unavailable\",\"commitsTotal\":4}")
        nil
      (agent-repl--merge-echo-pushed-state "ws1" :merge-failed :merging))
    (should (equal msgs
                   '("agent-repl: ws1 merge failed — lease unavailable, record {\"cause\":\"lease unavailable\",\"commitsTotal\":4}")))))

(ert-deftest agent-repl-test-merge-echo-does-not-clip-the-failure-record ()
  "The record rides UNCLIPPED — a JSON cut at 60 characters cannot be read.
Every other clause is clipped so a long subject cannot push the counts
off the line; the record is the exception because a fragment of it is
worth nothing to the reader who came for it."
  (agent-repl-test--with-captured-messages msgs
    (let ((record (format "{\"cause\":\"%s\"}" (make-string 100 ?x))))
      (agent-repl-test--with-merge-echo
          (list :phase :failed :cause "broke" :failed-json record)
          nil
        (agent-repl--merge-echo-pushed-state "ws1" :merge-failed :merging))
      (should (string-suffix-p (concat "record " record) (car msgs))))))

(ert-deftest agent-repl-test-merge-echo-omits-an-empty-failure-record ()
  "An empty record is no record — the clause is dropped, never narrated blank."
  (agent-repl-test--with-captured-messages msgs
    (agent-repl-test--with-merge-echo
        '(:phase :failed :cause "lease unavailable" :failed-json "") nil
      (agent-repl--merge-echo-pushed-state "ws1" :merge-failed :merging))
    (should (equal msgs '("agent-repl: ws1 merge failed — lease unavailable")))))

(ert-deftest agent-repl-test-merge-echo-narrates-a-failed-after-action ()
  "A merge that landed with a failed post-merge action says so."
  (agent-repl-test--with-captured-messages msgs
    (agent-repl-test--with-merge-echo
        '(:phase :merged :commits-total 3 :after-action-error "tests failed") nil
      (agent-repl--merge-echo-pushed-state "ws1" :merged :merging))
    (should (equal msgs
                   '("agent-repl: ws1 merged — 3 commits, post-merge action failed: tests failed")))))

(ert-deftest agent-repl-test-merge-echo-omits-an-empty-after-action-error ()
  "An empty after-action error is no error — the clause is dropped."
  (agent-repl-test--with-captured-messages msgs
    (agent-repl-test--with-merge-echo
        '(:phase :merged :commits-total 3 :after-action-error "") nil
      (agent-repl--merge-echo-pushed-state "ws1" :merged :merging))
    (should (equal msgs '("agent-repl: ws1 merged — 3 commits")))))

(ert-deftest agent-repl-test-merge-echo-clips-a-long-subject ()
  "A commit subject longer than the cap is clipped so the counts survive."
  (agent-repl-test--with-captured-messages msgs
    (agent-repl-test--with-merge-echo
        (list :phase :cherry-picking :commits-total 1 :commits-landed 0
              :current-subject (make-string 100 ?x))
        nil
      (agent-repl--merge-echo-pushed-state "ws1" :merging :merge-queued))
    (should (string-suffix-p (concat (make-string 60 ?x) "…") (car msgs)))
    (should-not (string-match-p (make-string 61 ?x) (car msgs)))))

(ert-deftest agent-repl-test-merge-echo-narrates-a-phase-change-under-one-state ()
  "A phase change with NO render-state change still narrates.
The daemon runs the pre-merge action, the picks, the tests and the
post-merge action all under one pushed `:merging'."
  (agent-repl-test--with-captured-messages msgs
    (agent-repl-test--with-merge-echo
        '(:phase :testing :commits-total 2 :commits-landed 2)
        '(:phase :cherry-picking :commits-landed 2)
      (agent-repl--merge-echo-pushed-state "ws1" :merging :merging))
    (should (equal msgs '("agent-repl: ws1 testing its merge — 2/2 commits")))))

;;;; ---- Tests: the per-pick tick fires only on a changed count ----

(ert-deftest agent-repl-test-merge-echo-ticks-when-a-commit-lands ()
  "A landed commit inside one phase is echoed."
  (agent-repl-test--with-captured-messages msgs
    (agent-repl-test--with-merge-echo
        '(:phase :cherry-picking :commits-total 4 :commits-landed 2)
        '(:phase :cherry-picking :commits-landed 1)
      (agent-repl--merge-echo-pushed-state "ws1" :merging :merging))
    (should (equal msgs '("agent-repl: ws1 cherry-picking — 2/4 commits")))))

(ert-deftest agent-repl-test-merge-echo-skips-a-tickless-revision ()
  "A within-phase revision that lands no commit is silent."
  (agent-repl-test--with-captured-messages msgs
    (agent-repl-test--with-merge-echo
        '(:phase :cherry-picking :commits-total 4 :commits-landed 2)
        '(:phase :cherry-picking :commits-landed 2)
      (agent-repl--merge-echo-pushed-state "ws1" :merging :merging))
    (should-not msgs)))

(ert-deftest agent-repl-test-merge-echo-records-what-it-narrated ()
  "The narration records its phase and count so the next push can compare."
  (let (puts)
    (agent-repl-test--with-captured-messages _msgs
      (cl-letf (((symbol-function 'agent-repl--ws-get)
                 (lambda (_ws key)
                   (when (eq key :pushed-merge-status)
                     '(:phase :cherry-picking :commits-total 4 :commits-landed 2))))
                ((symbol-function 'agent-repl--ws-put)
                 (lambda (ws key value) (push (list ws key value) puts))))
        (agent-repl--merge-echo-pushed-state "ws1" :merging :merge-queued)))
    (should (equal puts
                   '(("ws1" :merge-echo-last
                      (:phase :cherry-picking :commits-landed 2)))))))

(ert-deftest agent-repl-test-merge-echo-is-subscribed-to-the-transition-hook ()
  "The narrator is registered on `agent-repl-ws-state-transition-functions'."
  (should (memq #'agent-repl--merge-echo-pushed-state
                agent-repl-ws-state-transition-functions)))

;;;; ---- Tests: merge-kill-on-merged (a landed merge kills the tab) ----

(defmacro agent-repl-test--with-kill-capture (open-p closes puts &rest body)
  "Run BODY with the kill subscriber's collaborators stubbed.
OPEN-P is the value `agent-repl--ws-open-p' returns.  CLOSES collects
`(WS . PRESERVE)' per `agent-repl--close-workspace' call; PUTS collects
`(WS KEY VALUE)' per `agent-repl--ws-put' call."
  (declare (indent 3))
  `(let (,closes ,puts)
     (cl-letf (((symbol-function 'agent-repl--ws-open-p)
                (lambda (_ws) ,open-p))
               ((symbol-function 'agent-repl--close-workspace)
                (lambda (ws &optional preserve)
                  (push (cons ws preserve) ,closes)))
               ((symbol-function 'agent-repl--ws-put)
                (lambda (ws key value)
                  (push (list ws key value) ,puts)))
               ((symbol-function 'agent-repl--log)
                (lambda (&rest _) nil)))
       ,@body)))

(ert-deftest agent-repl-test-merge-kill-closes-an-open-workspace-on-merged ()
  "A pushed `:merged' against an open tab kills the workspace."
  (agent-repl-test--with-kill-capture t closes _puts
    (agent-repl--merge-kill-on-merged "ws-a" :merged :merging)
    (should (equal closes '(("ws-a" . preserve-entry))))))

(ert-deftest agent-repl-test-merge-kill-preserves-the-workspaces-entry ()
  "The kill closes with `preserve-entry' — the data-only entry survives."
  (agent-repl-test--with-kill-capture t closes _puts
    (agent-repl--merge-kill-on-merged "ws-a" :merged :merging)
    (should (eq (cdar closes) 'preserve-entry))))

(ert-deftest agent-repl-test-merge-kill-stamps-merge-completed-first ()
  "`:merge-completed' is recorded so the closed entry classifies as merged."
  (agent-repl-test--with-kill-capture t _closes puts
    (agent-repl--merge-kill-on-merged "ws-a" :merged :merging)
    (should (equal puts '(("ws-a" :merge-completed t))))))

(ert-deftest agent-repl-test-merge-kill-ignores-a-closed-workspace ()
  "A `:merged' push for an already-closed tab is a no-op."
  (agent-repl-test--with-kill-capture nil closes _puts
    (agent-repl--merge-kill-on-merged "ws-a" :merged :merging)
    (should-not closes)))

(ert-deftest agent-repl-test-merge-kill-ignores-non-merged-states ()
  "No other pushed state kills a workspace."
  (agent-repl-test--with-kill-capture t closes _puts
    (dolist (state '(:merging :merge-queued :merge-conflict :merge-failed
                     :ready :init :hibernated))
      (agent-repl--merge-kill-on-merged "ws-a" state :merging))
    (should-not closes)))

(ert-deftest agent-repl-test-merge-kill-fires-on-a-same-state-re-push ()
  "A `:merged' -> `:merged' re-push against an open tab still kills.
The convergence case: a daemon bounce re-pushes `:merged' for a tab
that was open while the subscriber was absent or the state flapped."
  (agent-repl-test--with-kill-capture t closes _puts
    (agent-repl--merge-kill-on-merged "ws-a" :merged :merged)
    (should (equal closes '(("ws-a" . preserve-entry))))))

(ert-deftest agent-repl-test-merge-kill-ignores-a-merged-merge-phase ()
  "A `:merged' MergeStatus PHASE under a non-merged render state does not kill.
The tab dies on the daemon's resolved render state, never on the merge
axis's own report, so the two can never disagree about whether the
workspace is gone."
  (agent-repl-test--with-kill-capture t closes _puts
    (cl-letf (((symbol-function 'agent-repl--ws-get)
               (lambda (_ws key)
                 (when (eq key :pushed-merge-status) '(:phase :merged)))))
      (agent-repl--merge-kill-on-merged "ws-a" :merging :merging))
    (should-not closes)))

(ert-deftest agent-repl-test-merge-kill-is-subscribed-to-the-transition-hook ()
  "The kill subscriber is registered on the transition hook."
  (should (memq #'agent-repl--merge-kill-on-merged
                agent-repl-ws-state-transition-functions)))

;;;; ---- Tests: the removed surface stays removed ----

(ert-deftest agent-repl-test-no-merge-handler-resolution-remains ()
  "Emacs resolves no merge handler: every merge is one daemon request."
  (dolist (sym '(agent-repl--resolve-merge-handler
                 agent-repl--resolve-merge-handler-symbol
                 agent-repl--dispatch-merge-handler
                 agent-repl--register-merge-handler
                 agent-repl--read-merge-handler-config-file
                 agent-repl--lookup-merge-handler-override))
    (should-not (fboundp sym))))

(ert-deftest agent-repl-test-no-merge-geometry-resolution-remains ()
  "Emacs computes no workspace->worktree merge geometry."
  (dolist (sym '(agent-repl--merge-cherry-pick-geometry
                 agent-repl--merge-target-dir-for-ws
                 agent-repl--cherry-pick-base))
    (should-not (fboundp sym))))

(ert-deftest agent-repl-test-no-merge-queue-remains ()
  "Emacs keeps no merge queue: position and depth are pushed, not derived."
  (dolist (sym '(agent-repl--merge-queue
                 agent-repl--in-flight-merges
                 agent-repl--merge-progress))
    (should-not (boundp sym)))
  (dolist (sym '(agent-repl--enqueue-merge
                 agent-repl--dequeue-merge
                 agent-repl--drain-merge-queue
                 agent-repl--persist-merge-queue))
    (should-not (fboundp sym))))

(ert-deftest agent-repl-test-no-merge-host-action-remains ()
  "Emacs runs no \"merge\" host action: a merge is a daemon COMMAND.
The handler carried the whole name-resolution chain, whose miss branch
logged and returned — a merge the user asked for producing nothing but a
log line is the silent degradation the payload's `user-error' replaced."
  (dolist (sym '(agent-repl--handle-merge-command
                 agent-repl--resolve-merge-workspace-name))
    (should-not (fboundp sym)))
  (should-not (assoc "merge" agent-repl--legacy-host-action-handlers)))

(ert-deftest agent-repl-test-no-merge-action-policy-remains ()
  "Emacs neither defers a merge behind a pre-merge action nor runs one.
The daemon executes both ends of the pipeline and reports them as
`MergeStatus' phases; a second implementation here would be a second
owner of the merge's ordering."
  (dolist (sym '(agent-repl--maybe-run-before-ws-merge-prompt
                 agent-repl--before-ws-merge-turn
                 agent-repl--maybe-run-postprocessing-prompt))
    (should-not (fboundp sym)))
  (should-not (boundp 'agent-repl--before-ws-merge-reinvoke-instruction)))

(provide 'test-merge-handlers)

;;; test-merge-handlers.el ends here
