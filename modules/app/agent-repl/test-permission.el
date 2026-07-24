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
  "A non-permission conversation item (assistant message) is not a permission arm."
  ;; Arrange / Act / Assert
  (should-not (agent-repl--permission-item-p
               '(:uuid "m1" :assistantMessage (:content "hi")))))

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
                      :items (list '(:uuid "m1" :assistantMessage (:content "hi"))
                                   '(:uuid "t1" :toolUse (:name "Bash"))
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
                    :items (list '(:uuid "m1" :assistantMessage (:content "hi")))))))
      ;; Assert
      (should (= n 0))
      (should-not (agent-repl--ws-get "ws1" :permission-prompt-active)))))

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

;;;; ---- answer round-trip -----------------------------------------------

(ert-deftest agent-repl-test-permission-answer-allow-shapes-payload ()
  "An allow answer sends allow:t with the permission request id."
  ;; Arrange
  (let (captured)
    (cl-letf (((symbol-function 'agent-repl--uds-send-command)
               (lambda (field payload &rest _) (setq captured (list field payload)) "req-1"))
              ((symbol-function 'agent-repl--uds-track-command)
               (lambda (&rest _) nil)))
      ;; Act
      (agent-repl--send-permission-answer "ws1" "r1" t)
      ;; Assert
      (should (equal (nth 0 captured) "permissionAnswer"))
      (should (equal (plist-get (nth 1 captured) :permissionRequestId) "r1"))
      (should (eq (plist-get (nth 1 captured) :allow) t)))))

(ert-deftest agent-repl-test-permission-answer-deny-omits-allow ()
  "A deny answer omits the allow field and carries the deny message."
  ;; Arrange
  (let (captured)
    (cl-letf (((symbol-function 'agent-repl--uds-send-command)
               (lambda (field payload &rest _) (setq captured (list field payload)) "req-1"))
              ((symbol-function 'agent-repl--uds-track-command)
               (lambda (&rest _) nil)))
      ;; Act
      (agent-repl--send-permission-answer "ws1" "r1" nil nil "nope")
      ;; Assert — allow omitted (protojson false-omission), denyMessage present
      (should-not (plist-member (nth 1 captured) :allow))
      (should (equal (plist-get (nth 1 captured) :denyMessage) "nope")))))

(ert-deftest agent-repl-test-permission-answer-tracks-command ()
  "The answer round-trip tracks its command for ack surfacing."
  ;; Arrange
  (let (tracked)
    (cl-letf (((symbol-function 'agent-repl--uds-send-command)
               (lambda (&rest _) "req-9"))
              ((symbol-function 'agent-repl--uds-track-command)
               (lambda (rid field &rest _) (setq tracked (list rid field)))))
      ;; Act
      (agent-repl--send-permission-answer "ws1" "r1" t)
      ;; Assert
      (should (equal tracked '("req-9" "permissionAnswer"))))))

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

;;; test-permission.el ends here
