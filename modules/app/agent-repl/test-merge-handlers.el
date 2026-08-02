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
sent.  Tracking and the host-action helpers are stubbed inert so a test
that does not care about them is not forced to mock them."
  (declare (indent 1))
  `(let (,sent-var)
     (cl-letf (((symbol-function 'agent-repl--uds-send-command)
                (lambda (field payload &optional ws &rest _)
                  (setq ,sent-var (list :field field :payload payload :ws ws))
                  "req-1"))
               ((symbol-function 'agent-repl--uds-track-command)
                (lambda (&rest _) "req-1"))
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
  "The dispatch tracks the sent request-id so a rejected ack surfaces."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "DWC/foo" :project-dir "/src")
    (let (tracked)
      (cl-letf (((symbol-function 'agent-repl--uds-send-command)
                 (lambda (&rest _) "req-9"))
                ((symbol-function 'agent-repl--uds-track-command)
                 (lambda (req field ws &optional _fail _ok _challenge)
                   (setq tracked (list req field ws))))
                ((symbol-function 'agent-repl--host-action-defer)
                 (lambda (&rest _) nil)))
        (agent-repl--merge-dispatch-over-uds "DWC/foo")
        (should (equal tracked '("req-9" "mergeWorkspace" "DWC/foo")))))))

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
                 (lambda (&rest _) "req-9"))
                ((symbol-function 'agent-repl--uds-track-command)
                 (lambda (&rest _) nil))
                ((symbol-function 'agent-repl--host-action-defer)
                 (lambda (token) (setq deferred token))))
        (agent-repl--merge-dispatch-over-uds "DWC/foo")
        (should (equal deferred "req-9"))))))

(ert-deftest agent-repl-test-merge-ack-failure-settles-the-host-action ()
  "A rejected merge ack completes the deferred host action as FAILED."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "DWC/foo" :project-dir "/src")
    (let (on-failure settled)
      (cl-letf (((symbol-function 'agent-repl--uds-send-command)
                 (lambda (&rest _) "req-1"))
                ((symbol-function 'agent-repl--uds-track-command)
                 (lambda (_req _field _ws &optional cb &rest _)
                   (setq on-failure cb)))
                ((symbol-function 'agent-repl--host-action-defer)
                 (lambda (_token) nil))
                ((symbol-function 'agent-repl--host-action-settle)
                 (lambda (token ok text) (setq settled (list token ok text)))))
        (agent-repl--merge-dispatch-over-uds "DWC/foo")
        (funcall on-failure "resolve dirs: not wired")
        (should (equal settled '("req-1" nil "resolve dirs: not wired")))))))

(ert-deftest agent-repl-test-merge-ack-success-settles-the-host-action ()
  "An accepted merge ack completes the deferred host action as OK."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "DWC/foo" :project-dir "/src")
    (let (on-success settled)
      (cl-letf (((symbol-function 'agent-repl--uds-send-command)
                 (lambda (&rest _) "req-1"))
                ((symbol-function 'agent-repl--uds-track-command)
                 (lambda (_req _field _ws &optional _cb ok-cb &rest _)
                   (setq on-success ok-cb)))
                ((symbol-function 'agent-repl--host-action-defer)
                 (lambda (_token) nil))
                ((symbol-function 'agent-repl--host-action-settle)
                 (lambda (token ok text) (setq settled (list token ok text)))))
        (agent-repl--merge-dispatch-over-uds "DWC/foo")
        (funcall on-success)
        (should (equal settled '("req-1" t nil)))))))

(ert-deftest agent-repl-test-merge-ack-failure-logs-the-rejection ()
  "A rejected ack is recorded through the canonical log helper."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "DWC/foo" :project-dir "/src")
    (let (on-failure logged)
      (cl-letf (((symbol-function 'agent-repl--uds-send-command)
                 (lambda (&rest _) "req-1"))
                ((symbol-function 'agent-repl--uds-track-command)
                 (lambda (_req _field _ws &optional cb &rest _)
                   (setq on-failure cb)))
                ((symbol-function 'agent-repl--host-action-defer)
                 (lambda (_token) nil))
                ((symbol-function 'agent-repl--host-action-settle)
                 (lambda (&rest _) nil))
                ((symbol-function 'agent-repl--log)
                 (lambda (_ws fmt &rest args)
                   (push (apply #'format fmt args) logged))))
        (agent-repl--merge-dispatch-over-uds "DWC/foo")
        (funcall on-failure "branch not found")
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
                ((symbol-function 'agent-repl--uds-track-command)
                 (lambda (&rest _) nil))
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
  "Resume tracks its request-id so a rejected ack surfaces loudly."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "DWC/foo" :project-dir "/src")
    (let (tracked)
      (cl-letf (((symbol-function 'agent-repl--uds-send-command)
                 (lambda (&rest _) "req-7"))
                ((symbol-function 'agent-repl--uds-track-command)
                 (lambda (req field ws &rest _)
                   (setq tracked (list req field ws)))))
        (agent-repl--merge-resume-over-uds "DWC/foo")
        (should (equal tracked '("req-7" "mergeWorkspace" "DWC/foo")))))))

(ert-deftest agent-repl-test-resume-propagates-the-payload-error ()
  "An unkeyed resume aborts BEFORE any frame is written."
  (agent-repl-test--with-clean-state
    (let (sent)
      (cl-letf (((symbol-function 'agent-repl--uds-send-command)
                 (lambda (&rest _) (setq sent t) "req-1"))
                ((symbol-function 'agent-repl--uds-track-command)
                 (lambda (&rest _) nil)))
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
    (let (on-failure)
      (cl-letf (((symbol-function 'agent-repl--uds-send-command)
                 (lambda (&rest _) "req-1"))
                ((symbol-function 'agent-repl--frontend-ws-command-key)
                 (lambda (ws) ws))
                ((symbol-function 'agent-repl--host-action-defer)
                 (lambda (&rest _) nil))
                ((symbol-function 'agent-repl--host-action-settle)
                 (lambda (&rest _) nil))
                ((symbol-function 'agent-repl--uds-track-command)
                 (lambda (_req _field _ws &optional fail _ok _challenge)
                   (setq on-failure fail))))
        (agent-repl--merge-dispatch-over-uds "ws1")
        (funcall on-failure "lease unavailable")))
    (should (cl-some (lambda (m)
                       (string-match-p "merge of ws1 refused: lease unavailable" m))
                     msgs))))

(ert-deftest agent-repl-test-merge-echo-narrates-a-merge-phase ()
  "A pushed merge-phase transition is echoed in the minibuffer."
  (agent-repl-test--with-captured-messages msgs
    (cl-letf (((symbol-function 'agent-repl--ws-get)
               (lambda (_ws _key) nil)))
      (agent-repl--merge-echo-pushed-state "ws1" :merging :merge-queued))
    (should (equal msgs '("agent-repl: ws1 merging")))))

(ert-deftest agent-repl-test-merge-echo-carries-the-failure-cause ()
  "A `:merge-failed' echo includes the daemon's cause, prefix stripped."
  (agent-repl-test--with-captured-messages msgs
    (cl-letf (((symbol-function 'agent-repl--ws-get)
               (lambda (_ws key)
                 (when (eq key :pushed-render-state-meta)
                   '(:cause-kind "merge_transition:shim lease unavailable")))))
      (agent-repl--merge-echo-pushed-state "ws1" :merge-failed :merge-enqueuing))
    (should (equal msgs
                   '("agent-repl: ws1 merge failed — shim lease unavailable")))))

(ert-deftest agent-repl-test-merge-echo-skips-a-same-state-repush ()
  "Re-pushing the same merge state does not spam the minibuffer."
  (agent-repl-test--with-captured-messages msgs
    (cl-letf (((symbol-function 'agent-repl--ws-get)
               (lambda (_ws _key) nil)))
      (agent-repl--merge-echo-pushed-state "ws1" :merging :merging))
    (should-not msgs)))

(ert-deftest agent-repl-test-merge-echo-ignores-non-merge-states ()
  "Ordinary render states are not the merge narrator's business."
  (agent-repl-test--with-captured-messages msgs
    (cl-letf (((symbol-function 'agent-repl--ws-get)
               (lambda (_ws _key) nil)))
      (agent-repl--merge-echo-pushed-state "ws1" :thinking :ready))
    (should-not msgs)))

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

(provide 'test-merge-handlers)

;;; test-merge-handlers.el ends here
