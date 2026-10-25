;;; wire-chat.el --- the chat verbs' codec -*- lexical-binding: t; -*-

;;; Commentary:

;; The codec of the two chat verbs Emacs sends: CreateThread (`SPC TAB t')
;; and the daemon's push asking for a new thread's prompt
;; (`DaemonNewThreadRequested', decoded in wire-host.el's WatchDaemon arm
;; list).  The rest of the chat (the view, its gestures) is the webapp's.

;;; Code:

(require 'cl-lib)

(declare-function agent-repl-wire--fail "wire-common" (message-name field reason))
(declare-function agent-repl-wire--encoded "wire-common" (message-name value))
(declare-function agent-repl-wire--object "wire-common" (message-name value))
(declare-function agent-repl-wire--check-keys "wire-common" (message-name object keys))
(declare-function agent-repl-wire--decoded "wire-common" (message-name value))
(declare-function agent-repl-wire--decode-empty "wire-common" (message-name value))
(declare-function agent-repl-wire--decode-message "wire-common" (message-name field object decoder))
(declare-function agent-repl-wire--decode-string "wire-common" (message-name field object))
(declare-function agent-repl-wire--decode-oneof "wire-common" (message-name oneof object arms &optional unset-legal))
(declare-function agent-repl-wire-decode-workspace-ref "wire-common" (value))

;;;; ---- CreateThread ----

(defun agent-repl-wire-encode-create-thread-request (value)
  "Encode the CreateThreadRequest plist VALUE `(:question TEXT)'.
TEXT is the thread's first question, as typed; a blank one is refused here,
before the send: a thread starts from a question."
  (let ((text (plist-get value :question)))
    (unless (and (stringp text) (not (string-empty-p (string-trim text))))
      (agent-repl-wire--fail "CreateThreadRequest" 'question "a thread starts from a question"))
    (agent-repl-wire--encoded
     "CreateThreadRequest"
     (list (cons 'question (list (cons 'text text)))))))

(defun agent-repl-wire-decode-create-thread-success (value)
  "Decode VALUE as `CreateThreadSuccess', a plist `(:thread REF)'."
  (let ((object (agent-repl-wire--object "CreateThreadSuccess" value)))
    (agent-repl-wire--check-keys "CreateThreadSuccess" object '(thread))
    (agent-repl-wire--decoded
     "CreateThreadSuccess"
     (list :thread (agent-repl-wire--decode-message
                    "CreateThreadSuccess" 'thread object #'agent-repl-wire-decode-workspace-ref)))))

(defun agent-repl-wire-decode-create-thread-creation-failed (value)
  "Decode VALUE as `CreateThreadCreationFailed', a plist `(:detail TEXT)'."
  (let ((object (agent-repl-wire--object "CreateThreadCreationFailed" value)))
    (agent-repl-wire--check-keys "CreateThreadCreationFailed" object '(detail))
    (agent-repl-wire--decoded
     "CreateThreadCreationFailed"
     (list :detail (agent-repl-wire--decode-string "CreateThreadCreationFailed" 'detail object)))))

(defun agent-repl-wire-decode-create-thread-error (value)
  "Decode VALUE as `CreateThreadError', a plist `(:cause ONEOF)'."
  (let ((object (agent-repl-wire--object "CreateThreadError" value)))
    (agent-repl-wire--check-keys "CreateThreadError" object '(creationFailed))
    (agent-repl-wire--decoded
     "CreateThreadError"
     (list :cause (agent-repl-wire--decode-oneof
                   "CreateThreadError" 'cause object
                   '((creationFailed :creation-failed agent-repl-wire-decode-create-thread-creation-failed)))))))

(defun agent-repl-wire-decode-create-thread-response (value)
  "Decode VALUE as `CreateThreadResponse'.  THE ARM IS THE OUTCOME."
  (let ((object (agent-repl-wire--object "CreateThreadResponse" value)))
    (agent-repl-wire--check-keys "CreateThreadResponse" object '(success error))
    (agent-repl-wire--decoded
     "CreateThreadResponse"
     (agent-repl-wire--decode-oneof
      "CreateThreadResponse" 'result object
      '((success :success agent-repl-wire-decode-create-thread-success)
        (error :error agent-repl-wire-decode-create-thread-error))))))

;;;; ---- The daemon's new-thread push ----

(defun agent-repl-wire-decode-daemon-new-thread-requested (value)
  "Decode VALUE as the empty `DaemonNewThreadRequested'.
The sidebar's new-thread button was clicked: open the new-thread prompt."
  (agent-repl-wire--decode-empty "DaemonNewThreadRequested" value))

(provide 'wire-chat)

;;; wire-chat.el ends here
