;;; test-permission.el --- ERT tests for permission.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Pure-elisp tests for the pushed-state permission UX: the
;; `conversationDelta' permission-item handler (present / clear), the
;; desktop-notification firing, the per-uuid idempotency guard, and the
;; `PermissionAnswerCmd' answer round-trip.  No processes, no network —
;; frame plists are constructed directly and the UDS send/notify boundaries
;; are shadowed via `cl-letf'.
;;
;; One edge case per test, AAA structure.
;;
;; Run with:
;;   emacs -batch -Q -l ert -l test-permission.el -f ert-run-tests-batch-and-exit

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;; Transport before permission: permission.el registers its handler into
;; frontend-uds.el at load, so load the transport first.
(load (expand-file-name "frontend-uds.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)
(load (expand-file-name "permission.el" (file-name-directory
                                         (or load-file-name buffer-file-name)))
      nil t)

(require 'cl-lib)

;;;; ---- Fixtures --------------------------------------------------------

(defun agent-repl-test--perm-item (uuid tool resolution &optional deny-message input)
  "Return a permission `ConversationItem' plist for UUID/TOOL/RESOLUTION."
  (list :uuid uuid
        :permission (list :request (list :requestId uuid
                                         :toolName tool
                                         :input (or input '(:cmd "ls")))
                          :resolution resolution
                          :denyMessage (or deny-message ""))))

;;;; ---- permission-item-p -----------------------------------------------

(ert-deftest agent-repl-test-permission-item-p-true-for-permission-arm ()
  "An item carrying the :permission oneof arm is recognized."
  ;; Arrange / Act / Assert
  (should (agent-repl--permission-item-p
           (agent-repl-test--perm-item "r1" "Bash" "RESOLUTION_PENDING"))))

(ert-deftest agent-repl-test-permission-item-p-false-for-other-arm ()
  "An agent-emitted conversation item is not a permission arm.
Everything the agent produced folded onto the single `agent' arm with the
component reshape; the permission arm stayed its own, which is the whole
reason this reader can ask for one key."
  ;; Arrange / Act / Assert
  (should-not (agent-repl--permission-item-p
               '(:uuid "m1" :agent (:assistantMessage (:content "hi"))))))

;;;; ---- present: bookkeeping + notification -----------------------------

(ert-deftest agent-repl-test-permission-pending-records-active-prompt ()
  "A PENDING permission item stores :permission-prompt-active with its request id."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--notify) (lambda (&rest _) nil)))
      ;; Act
      (agent-repl--frontend-apply-conversation-delta
       (list :workspace "ws1"
             :items (list (agent-repl-test--perm-item "r1" "Bash" "RESOLUTION_PENDING"))))
      ;; Assert
      (should (equal (plist-get (agent-repl--ws-get "ws1" :permission-prompt-active)
                                :request-id)
                     "r1")))))

(ert-deftest agent-repl-test-permission-pending-records-tool-name ()
  "A PENDING permission item records the requested tool name in the active prompt."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--notify) (lambda (&rest _) nil)))
      ;; Act
      (agent-repl--frontend-apply-conversation-delta
       (list :workspace "ws1"
             :items (list (agent-repl-test--perm-item "r1" "Bash" "RESOLUTION_PENDING"))))
      ;; Assert
      (should (equal (plist-get (agent-repl--ws-get "ws1" :permission-prompt-active)
                                :tool-name)
                     "Bash")))))

(ert-deftest agent-repl-test-permission-pending-fires-notification ()
  "A PENDING permission item fires Emacs's own desktop notification."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let (notified)
      (cl-letf (((symbol-function 'agent-repl--notify)
                 (lambda (ws _title msg) (setq notified (list ws msg)))))
        ;; Act
        (agent-repl--frontend-apply-conversation-delta
         (list :workspace "ws1"
               :items (list (agent-repl-test--perm-item "r1" "Bash" "RESOLUTION_PENDING"))))
        ;; Assert
        (should (equal (nth 0 notified) "ws1"))
        (should (string-match-p "permission requested" (nth 1 notified)))
        (should (string-match-p "Bash" (nth 1 notified)))))))

(ert-deftest agent-repl-test-permission-pending-idempotent-same-uuid ()
  "Re-applying the SAME pending uuid does not re-notify (replay after reconnect)."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((count 0))
      (cl-letf (((symbol-function 'agent-repl--notify)
                 (lambda (&rest _) (cl-incf count))))
        (agent-repl--frontend-apply-conversation-delta
         (list :workspace "ws1"
               :items (list (agent-repl-test--perm-item "r1" "Bash" "RESOLUTION_PENDING"))))
        ;; Act — replay the identical pending item
        (agent-repl--frontend-apply-conversation-delta
         (list :workspace "ws1"
               :items (list (agent-repl-test--perm-item "r1" "Bash" "RESOLUTION_PENDING"))))
        ;; Assert — notified exactly once
        (should (= count 1))))))

(ert-deftest agent-repl-test-permission-pending-different-uuid-renotifies ()
  "A pending prompt with a NEW uuid re-presents and re-notifies."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((count 0))
      (cl-letf (((symbol-function 'agent-repl--notify)
                 (lambda (&rest _) (cl-incf count))))
        (agent-repl--frontend-apply-conversation-delta
         (list :workspace "ws1"
               :items (list (agent-repl-test--perm-item "r1" "Bash" "RESOLUTION_PENDING"))))
        ;; Act — a different request id arrives
        (agent-repl--frontend-apply-conversation-delta
         (list :workspace "ws1"
               :items (list (agent-repl-test--perm-item "r2" "Edit" "RESOLUTION_PENDING"))))
        ;; Assert
        (should (= count 2))
        (should (equal (plist-get (agent-repl--ws-get "ws1" :permission-prompt-active)
                                  :request-id)
                       "r2"))))))

;;;; ---- clear: resolution updates ---------------------------------------

(ert-deftest agent-repl-test-permission-allowed-clears-prompt ()
  "A RESOLUTION_ALLOWED update clears the matching active prompt."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--notify) (lambda (&rest _) nil)))
      (agent-repl--ws-put "ws1" :permission-prompt-active '(:request-id "r1" :tool-name "Bash"))
      ;; Act
      (agent-repl--frontend-apply-conversation-delta
       (list :workspace "ws1"
             :items (list (agent-repl-test--perm-item "r1" "Bash" "RESOLUTION_ALLOWED"))))
      ;; Assert
      (should-not (agent-repl--ws-get "ws1" :permission-prompt-active)))))

(ert-deftest agent-repl-test-permission-denied-echoes-deny-message ()
  "A RESOLUTION_DENIED update echoes the daemon's deny message."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :permission-prompt-active '(:request-id "r1" :tool-name "Bash"))
    (let (echoed)
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (setq echoed (apply #'format fmt args)))))
        ;; Act
        (agent-repl--frontend-apply-conversation-delta
         (list :workspace "ws1"
               :items (list (agent-repl-test--perm-item "r1" "Bash" "RESOLUTION_DENIED" "not allowed"))))
        ;; Assert
        (should (string-match-p "not allowed" (or echoed "")))))))

(ert-deftest agent-repl-test-permission-denied-files-the-request-identity ()
  "The denial's request id is filed rather than echoed: the reason is the copy."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :permission-prompt-active '(:request-id "r1" :tool-name "Bash"))
    (let (logged)
      (cl-letf (((symbol-function 'agent-repl--emit-message) #'ignore)
                ((symbol-function 'agent-repl--log)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logged))))
        ;; Act
        (agent-repl--frontend-apply-conversation-delta
         (list :workspace "ws1"
               :items (list (agent-repl-test--perm-item "r1" "Bash" "RESOLUTION_DENIED" "not allowed"))))
        ;; Assert
        (should (cl-find-if (lambda (line)
                              (string-match-p "permission-clear ws=ws1 request=r1" line))
                            logged))))))

(ert-deftest agent-repl-test-permission-abandoned-clears-silently ()
  "A RESOLUTION_ABANDONED update clears the prompt WITHOUT echoing a message."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :permission-prompt-active '(:request-id "r1" :tool-name "Bash"))
    (let ((echoed nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (&rest _) (setq echoed t))))
        ;; Act
        (agent-repl--frontend-apply-conversation-delta
         (list :workspace "ws1"
               :items (list (agent-repl-test--perm-item "r1" "Bash" "RESOLUTION_ABANDONED"))))
        ;; Assert — cleared, and no echo-area message
        (should-not (agent-repl--ws-get "ws1" :permission-prompt-active))
        (should-not echoed)))))

(ert-deftest agent-repl-test-permission-resolution-nonmatching-uuid-preserves ()
  "A resolution for a DIFFERENT uuid leaves the active prompt untouched."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'message) (lambda (&rest _) nil)))
      (agent-repl--ws-put "ws1" :permission-prompt-active '(:request-id "r1" :tool-name "Bash"))
      ;; Act — a resolution for an unrelated request id
      (agent-repl--frontend-apply-conversation-delta
       (list :workspace "ws1"
             :items (list (agent-repl-test--perm-item "r2" "Edit" "RESOLUTION_ALLOWED"))))
      ;; Assert
      (should (equal (plist-get (agent-repl--ws-get "ws1" :permission-prompt-active)
                                :request-id)
                     "r1")))))

;;;; ---- delta iteration --------------------------------------------------

(ert-deftest agent-repl-test-permission-delta-counts-only-permission-items ()
  "The handler acts on permission items only and returns their count."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--notify) (lambda (&rest _) nil)))
      ;; Act — a delta mixing a text item, a tool item, and one permission
      (let ((n (agent-repl--frontend-apply-conversation-delta
                (list :workspace "ws1"
                      :items (list '(:uuid "m1" :agent (:assistantMessage (:content "hi")))
                                   '(:uuid "t1" :agent (:toolUse (:name "Bash")))
                                   (agent-repl-test--perm-item "r1" "Bash" "RESOLUTION_PENDING"))))))
        ;; Assert
        (should (= n 1))))))

(ert-deftest agent-repl-test-permission-delta-ignores-non-permission-items ()
  "A delta with no permission items handles nothing and returns 0."
  ;; Arrange
  (agent-repl-test--with-clean-state
    ;; Act
    (let ((n (agent-repl--frontend-apply-conversation-delta
              (list :workspace "ws1"
                    :items (list '(:uuid "m1" :agent (:assistantMessage (:content "hi"))))))))
      ;; Assert
      (should (= n 0))
      (should-not (agent-repl--ws-get "ws1" :permission-prompt-active)))))

(ert-deftest agent-repl-test-permission-delta-logs-nonpermission-frame-verbosely ()
  "A chatty frame with no permission item records only verbose frame telemetry."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let (ordinary verbose)
      (cl-letf (((symbol-function 'agent-repl--log)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) ordinary)))
                ((symbol-function 'agent-repl--log-verbose)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) verbose))))
        ;; Act
        (agent-repl--frontend-apply-conversation-delta
         (list :workspace "ws1"
               :items (list '(:uuid "m1" :agent (:assistantMessage (:content "private message"))))))
        ;; Assert
        (should-not ordinary)
        (should (seq-some (lambda (line) (string-match-p "item-count=1" line)) verbose))
        (should-not (seq-some (lambda (line) (string-match-p "private message" line)) verbose))))))

;;;; ---- No-Silent-Fallbacks ---------------------------------------------

(ert-deftest agent-repl-test-permission-missing-workspace-errors ()
  "A permission item with no workspace fails loudly."
  ;; Arrange / Act / Assert
  (should-error
   (agent-repl--frontend-apply-conversation-delta
    (list :workspace ""
          :items (list (agent-repl-test--perm-item "r1" "Bash" "RESOLUTION_PENDING"))))))

(ert-deftest agent-repl-test-permission-unknown-resolution-errors ()
  "A permission item with an unknown/absent resolution fails loudly."
  ;; Arrange / Act / Assert
  (should-error
   (agent-repl--frontend-apply-conversation-delta
   (list :workspace "ws1"
         :items (list (agent-repl-test--perm-item "r1" "Bash" "RESOLUTION_BOGUS"))))))

(ert-deftest agent-repl-test-permission-missing-workspace-log-excludes-tool-input ()
  "The missing-workspace diagnostic retains the request id but not sensitive input."
  ;; Arrange
  (let (logs)
    (cl-letf (((symbol-function 'agent-repl--log)
               (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logs)))
              ((symbol-function 'agent-repl--log-verbose) (lambda (&rest _) nil)))
      ;; Act / Assert
      (should-error
       (agent-repl--frontend-apply-conversation-delta
        (list :workspace ""
              :items (list (agent-repl-test--perm-item
                            "r1" "Bash" "RESOLUTION_PENDING" nil
                            '(:command "TOP-SECRET-TOOL-INPUT"))))))
      (should (seq-some (lambda (line) (string-match-p "request=r1 workspace=missing" line)) logs))
      (should-not (seq-some (lambda (line) (string-match-p "TOP-SECRET-TOOL-INPUT" line)) logs)))))

(ert-deftest agent-repl-test-permission-notify-failure-logs-and-does-not-record-prompt ()
  "A notification failure is logged and leaves no unsurfaced active prompt."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let (logs)
      (cl-letf (((symbol-function 'agent-repl--log)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logs)))
                ;; A dropped permission notification is a UX regression, so the
                ;; diagnostic rides the warn rung; capture both so this test
                ;; keeps asserting the CONTENT rather than the severity.
                ((symbol-function 'agent-repl--warn)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logs)))
                ((symbol-function 'agent-repl--notify)
                 (lambda (&rest _) (error "desktop unavailable"))))
        ;; Act / Assert
        (should-error (agent-repl--permission-present "ws1" "r1" "Bash" '(:command "TOP-SECRET")))
        (should-not (agent-repl--ws-get "ws1" :permission-prompt-active))
        (should (seq-some (lambda (line) (string-match-p "request=r1 failed error-type=error" line)) logs))
        (should-not (seq-some (lambda (line) (string-match-p "TOP-SECRET" line)) logs))))))

;;;; ---- answer round-trip -----------------------------------------------

(defmacro agent-repl-test--with-answer-ws (&rest body)
  "Register workspace \"ws1\" (cwd /w) for BODY, cleaning up after.
The answer path resolves its WIRE KEY from `:project-dir\=', so a bare-name
workspace cannot route an answer at all."
  (declare (indent 0))
  `(unwind-protect
       (progn
         (puthash "ws1" (list :project-dir "/w") agent-repl--workspaces)
         ,@body)
     (remhash "ws1" agent-repl--workspaces)))

(ert-deftest agent-repl-test-permission-answer-allow-shapes-payload ()
  "An allow answer sends allow:t with the permission request id."
  ;; Arrange
  (agent-repl-test--with-answer-ws
    (let (captured)
      (cl-letf (((symbol-function 'agent-repl--uds-send-command)
                 (lambda (field payload &rest _) (setq captured (list field payload)) "req-1")))
        ;; Act
        (agent-repl--send-permission-answer "ws1" "r1" t)
        ;; Assert
        (should (equal (nth 0 captured) "permissionAnswer"))
        (should (equal (plist-get (nth 1 captured) :permissionRequestId) "r1"))
        (should (eq (plist-get (nth 1 captured) :allow) t))))))

(ert-deftest agent-repl-test-permission-answer-deny-omits-allow ()
  "A deny answer omits the allow field and carries the deny message."
  ;; Arrange
  (agent-repl-test--with-answer-ws
    (let (captured)
      (cl-letf (((symbol-function 'agent-repl--uds-send-command)
                 (lambda (field payload &rest _) (setq captured (list field payload)) "req-1")))
        ;; Act
        (agent-repl--send-permission-answer "ws1" "r1" nil nil "nope")
        ;; Assert — allow omitted (protojson false-omission), denyMessage present
        (should-not (plist-member (nth 1 captured) :allow))
        (should (equal (plist-get (nth 1 captured) :denyMessage) "nope"))))))


(ert-deftest agent-repl-test-permission-answer-is-keyed-by-the-workspace-cwd ()
  "The answer goes on the wire keyed by WS\='s cwd, never the persp name.
The daemon matches the `workspace\=' field against the session CWD; a bare
name matches nothing and the answer is NACKed as \"no live session\"."
  ;; Arrange
  (agent-repl-test--with-answer-ws
    (let (key)
      (cl-letf (((symbol-function 'agent-repl--uds-send-command)
                 (lambda (_field _payload &optional ws &rest _) (setq key ws) "req-1")))
        ;; Act
        (agent-repl--send-permission-answer "ws1" "r1" t)
        ;; Assert
        (should (equal key "/w"))))))

(ert-deftest agent-repl-test-permission-answer-uds-failure-logs-without-payload-content ()
  "A UDS send error logs request identity and error type, never edited input."
  ;; Arrange
  (agent-repl-test--with-answer-ws
    (let (logs)
      (cl-letf (((symbol-function 'agent-repl--log)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logs)))
                ((symbol-function 'agent-repl--uds-send-command)
                 (lambda (&rest _) (error "uds unavailable"))))
        ;; Act / Assert
        (should-error
         (agent-repl--send-permission-answer "ws1" "r1" t
                                             '(:command "TOP-SECRET-EDITED-INPUT")))
        (should (seq-some (lambda (line) (string-match-p "request=r1 uds-send-failed error-type=error" line)) logs))
        (should-not (seq-some (lambda (line) (string-match-p "TOP-SECRET-EDITED-INPUT" line)) logs))))))

;;;; ---- interactive answer command --------------------------------------

(ert-deftest agent-repl-test-answer-permission-no-active-prompt-errors ()
  "Answering with no active permission prompt signals user-error."
  ;; Arrange
  (agent-repl-test--with-clean-state
    ;; Act / Assert
    (should-error (agent-repl-answer-permission "ws1") :type 'user-error)))

(ert-deftest agent-repl-test-answer-permission-allow-sends-allow ()
  "Answering allow on the active prompt sends an allow answer for its request id."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :permission-prompt-active '(:request-id "r1" :tool-name "Bash"))
    (let (sent)
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                ((symbol-function 'message) (lambda (&rest _) nil))
                ((symbol-function 'agent-repl--send-permission-answer)
                 (lambda (ws rid allow &rest _) (setq sent (list ws rid allow)))))
        ;; Act
        (agent-repl-answer-permission "ws1")
        ;; Assert
        (should (equal sent '("ws1" "r1" t)))))))

(ert-deftest agent-repl-test-answer-permission-deny-sends-deny ()
  "Answering deny sends allow=nil and the typed deny message."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :permission-prompt-active '(:request-id "r1" :tool-name "Bash"))
    (let (sent)
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil))
                ((symbol-function 'read-string) (lambda (&rest _) "too risky"))
                ((symbol-function 'message) (lambda (&rest _) nil))
                ((symbol-function 'agent-repl--send-permission-answer)
                 (lambda (ws rid allow _ui deny) (setq sent (list ws rid allow deny)))))
        ;; Act
        (agent-repl-answer-permission "ws1")
        ;; Assert
        (should (equal sent '("ws1" "r1" nil "too risky")))))))

(ert-deftest agent-repl-test-answer-permission-deny-prompts-for-a-recorded-reason ()
  "The deny prompt asks for a RECORDED reason, not a message to the agent.
A decline stops the turn that asked (daemon permdecline.go), so the text
typed here reaches no agent, and a prompt that implied otherwise would be
asking the user to write for a reader that no longer exists."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :permission-prompt-active '(:request-id "r1" :tool-name "Bash"))
    (let (asked)
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil))
                ((symbol-function 'read-string) (lambda (prompt &rest _) (setq asked prompt) ""))
                ((symbol-function 'message) (lambda (&rest _) nil))
                ((symbol-function 'agent-repl--send-permission-answer) (lambda (&rest _) nil)))
        ;; Act
        (agent-repl-answer-permission "ws1")
        ;; Assert
        (should (string-match-p "recorded" (or asked "")))))))

;;;; ---- Wire CWD routing into the workspace log sink --------------------
;;
;; A `ConversationDelta' names its workspace by session CWD.  Dispatch keeps
;; the raw value (so `agent-repl--permission-handle-item' can still refuse a
;; blank one loudly), but the LOG sink is scoped by the resolved persp name:
;; a CWD cannot index `agent-repl--workspaces', so logging under one loses
;; the workspace attribution entirely.  The durable sink must be ENABLED for
;; these to test anything — with it off the ladder skips identity resolution.
;;
;; The unowned-cwd cases still reach one log call no call site can fix:
;; `agent-repl--ws-put' warns about the stub it just created and names that
;; stub as its own workspace, by construction.  That one relies on the sink
;; degrading an unroutable name to the global record.

(ert-deftest agent-repl-test-permission-delta-attributes-an-owned-cwd-to-its-ws ()
  "A delta naming an OWNED cwd routes its log record to that workspace's NAME."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir temporary-file-directory)
    (let (logged-ws)
      (cl-letf (((symbol-function 'agent-repl--notify) (lambda (&rest _) nil))
                ((symbol-function 'agent-repl--log)
                 (lambda (ws &rest _) (setq logged-ws ws))))
        ;; Act
        (agent-repl--frontend-apply-conversation-delta
         (list :workspace temporary-file-directory
               :items (list (agent-repl-test--perm-item
                             "r1" "Bash" "RESOLUTION_PENDING"))))
        ;; Assert
        (should (equal logged-ws "ws1"))))))

(ert-deftest agent-repl-test-permission-delta-unowned-cwd-routes-globally ()
  "A delta no live workspace owns still logs — globally — and still dispatches."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-log-sink-on
      (cl-letf (((symbol-function 'agent-repl--notify) (lambda (&rest _) nil)))
        ;; Act / Assert
        (should (= 1 (agent-repl--frontend-apply-conversation-delta
                      (list :workspace "/nowhere/unowned"
                            :items (list (agent-repl-test--perm-item
                                          "r1" "Bash" "RESOLUTION_PENDING"))))))))))

(ert-deftest agent-repl-test-permission-delta-unowned-cwd-still-dispatches-raw ()
  "The dispatch fallback is untouched: an unowned cwd still keys the prompt.
Degrading the DISPATCH value to nil would turn `--permission-handle-item''s
loud missing-workspace refusal into a silent one; only the log sink resolves."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-log-sink-on
      (cl-letf (((symbol-function 'agent-repl--notify) (lambda (&rest _) nil)))
        ;; Act
        (agent-repl--frontend-apply-conversation-delta
         (list :workspace "/nowhere/unowned"
               :items (list (agent-repl-test--perm-item
                             "r1" "Bash" "RESOLUTION_PENDING"))))
        ;; Assert
        (should (equal (plist-get (agent-repl--ws-get "/nowhere/unowned"
                                                      :permission-prompt-active)
                                  :request-id)
                       "r1"))))))

(ert-deftest agent-repl-test-permission-unknown-resolution-errors-on-an-unowned-cwd ()
  "An unknown resolution still fails with ITS OWN error, not a routing one."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-log-sink-on
      ;; Act
      (let ((err (should-error
                  (agent-repl--permission-handle-item
                   "/nowhere/unowned"
                   (agent-repl-test--perm-item "r1" "Bash" "RESOLUTION_WAT")))))
        ;; Assert
        (should (string-match-p "unknown resolution"
                                (error-message-string err)))))))


;;; test-permission.el ends here
