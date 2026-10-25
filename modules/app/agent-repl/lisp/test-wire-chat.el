;;; test-wire-chat.el --- ERT tests for agent-repl wire-chat.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;   AGENT_REPL_FORBID_VENDOR_CALLS=1 emacs -batch -Q -l ert \
;;     -l lisp/test-wire-chat.el -f ert-run-tests-batch-and-exit
;;
;; Fixtures are hand-written from `agentrepl/v1/endpoint_create_thread.proto'
;; in Go's protojson shape.

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

(defmacro agent-repl-test-wire-chat--quiet (&rest body)
  "Run BODY with the logging ladder stubbed out."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'agent-repl--error) (lambda (&rest _) nil))
             ((symbol-function 'agent-repl--log) (lambda (&rest _) nil)))
     ,@body))

(defun agent-repl-test-wire-chat--decode (decoder json)
  "Decode JSON with DECODER, quietly."
  (agent-repl-test-wire-chat--quiet
    (funcall decoder (json-parse-string json :object-type 'alist :array-type 'list
                                        :null-object :null :false-object :false))))

(ert-deftest agent-repl-test-wire-chat-encodes-the-first-question ()
  "A thread's request is its first question, as typed."
  (should (equal (agent-repl-test-wire-chat--quiet
                   (agent-repl-wire-encode-create-thread-request '(:question "why is the sky blue?")))
                 '((question . ((text . "why is the sky blue?")))))))

(ert-deftest agent-repl-test-wire-chat-refuses-a-blank-question ()
  "A thread starts from a question; a blank one is refused before the send."
  (dolist (question '("" "  \n" nil))
    (should-error (agent-repl-test-wire-chat--quiet
                    (agent-repl-wire-encode-create-thread-request (list :question question)))
                  :type 'agent-repl-wire-error)))

(ert-deftest agent-repl-test-wire-chat-decodes-the-answers ()
  "The success names the thread; the refusal carries its detail."
  (should (equal (agent-repl-test-wire-chat--decode
                  #'agent-repl-wire-decode-create-thread-response
                  "{\"success\":{\"thread\":{\"id\":\"t1\",\"dir\":\"/z/why\"}}}")
                 '(:arm :success :value (:thread (:id "t1" :dir "/z/why")))))
  (should (equal (agent-repl-test-wire-chat--decode
                  #'agent-repl-wire-decode-create-thread-response
                  "{\"error\":{\"creationFailed\":{\"detail\":\"no name\"}}}")
                 '(:arm :error :value (:cause (:arm :creation-failed :value (:detail "no name")))))))

(ert-deftest agent-repl-test-wire-chat-an-unknown-refusal-is-a-breach ()
  "A refusal arm this codec does not know is refused loudly."
  (should-error (agent-repl-test-wire-chat--decode
                 #'agent-repl-wire-decode-create-thread-response
                 "{\"error\":{\"mystery\":{}}}")
                :type 'agent-repl-wire-error))

(ert-deftest agent-repl-test-wire-chat-the-create-thread-arms-are-pinned ()
  "CreateThreadError's cause oneof has exactly the arms this codec decodes."
  (should (equal (agent-repl-test--generated-oneof-arms
                  "agentrepl/v1/endpoint_create_thread.pb.go" "CreateThreadError")
                 '("creationFailed"))))

(provide 'test-wire-chat)

;;; test-wire-chat.el ends here
