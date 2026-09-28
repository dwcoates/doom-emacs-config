;;; test-prompt-queue.el --- ERT tests for agent-repl prompt-queue.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;   bin/background.sh emacs -batch -Q -l ert \
;;     -l lisp/test-prompt-queue.el -f ert-run-tests-batch-and-exit
;;
;; The submit is stubbed and records what it was handed.  Nothing here uses
;; a timer or a sleep.
;;
;; The assertions that matter most: a deferral is SUBMITTED AT ONCE, deferred
;; (`SubmitPromptRequest.delivery = SUBMIT_PROMPT_DELIVERY_DEFERRED'), under
;; `:deferred-prompt' whatever origin it was composed under, so the DAEMON
;; holds it durably; and Emacs holds no prompt in memory at all -- neither
;; the retired outage queue nor the retired in-memory deferral queue, with
;; its finish-edge release and liveness gate.

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                           (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Fixtures ----

(defvar agent-repl-test-pq--submitted nil
  "Submissions the stubbed composer received, as plists, newest first.")

(defmacro agent-repl-test-pq--with-composer (text &rest body)
  "Run BODY with a live composer for ws-one holding TEXT and the submit stubbed."
  (declare (indent 1))
  `(let ((agent-repl-test-pq--submitted nil)
         (buf (generate-new-buffer " *agent-repl-test-composer*")))
     (unwind-protect
         (progn
           (with-current-buffer buf (agent-repl-input-mode) (insert ,text))
           (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "ws-one"))
                     ((symbol-function 'agent-repl--ws-get)
                      (lambda (_ws key) (when (eq key :input-buffer) buf)))
                     ((symbol-function 'agent-repl--history-push) (lambda (&optional _t) nil))
                     ((symbol-function 'agent-repl--history-reset) (lambda () nil))
                     ((symbol-function 'agent-repl--history-save) (lambda (_ws) nil))
                     ((symbol-function 'agent-repl--input-submit)
                      (lambda (ws said origin raw &optional key from-buffer snapshot delivery)
                        (push (list :ws ws :said said :origin origin :raw raw :key key
                                    :from-buffer from-buffer :snapshot snapshot
                                    :delivery delivery)
                              agent-repl-test-pq--submitted)
                        "key-1"))
                     ((symbol-function 'message) (lambda (&rest _) nil)))
             ,@body))
       (kill-buffer buf))))

(defun agent-repl-test-pq--said-text (submission)
  "Return the text of SUBMISSION's said."
  (plist-get (plist-get (car (plist-get (plist-get (plist-get submission :said) :content)
                                        :blocks))
                        :value)
             :text))

;;;; ---- The deferral command ----

(ert-deftest agent-repl-pq-defer-submits-once-at-once ()
  "Deferring submits the prompt NOW: the daemon, not Emacs, holds it."
  (agent-repl-test-pq--with-composer "later please"
    ;; Act
    (agent-repl-queue-deferred-prompt)
    ;; Assert
    (should (equal (length agent-repl-test-pq--submitted) 1))))

(ert-deftest agent-repl-pq-defer-asks-for-the-deferred-delivery ()
  "The submission asks to run as its own turn after the current one."
  (agent-repl-test-pq--with-composer "later please"
    ;; Act
    (agent-repl-queue-deferred-prompt)
    ;; Assert
    (should (eq (plist-get (car agent-repl-test-pq--submitted) :delivery) :deferred))))

(ert-deftest agent-repl-pq-defer-carries-the-deferred-origin ()
  "This command is `:deferred-prompt''s one production send site."
  (agent-repl-test-pq--with-composer "later please"
    ;; Act
    (agent-repl-queue-deferred-prompt)
    ;; Assert
    (should (eq (plist-get (car agent-repl-test-pq--submitted) :origin) :deferred-prompt))))

(ert-deftest agent-repl-pq-defer-mints-a-fresh-key ()
  "A deferral is a first attempt, so it passes no key and the composer mints one."
  (agent-repl-test-pq--with-composer "later please"
    ;; Act
    (agent-repl-queue-deferred-prompt)
    ;; Assert
    (should (null (plist-get (car agent-repl-test-pq--submitted) :key)))))

(ert-deftest agent-repl-pq-defer-submits-the-composed-text ()
  "What the user wrote is what is submitted."
  (agent-repl-test-pq--with-composer "later please"
    ;; Act
    (agent-repl-queue-deferred-prompt)
    ;; Assert
    (should (string-match-p "later please"
                            (agent-repl-test-pq--said-text (car agent-repl-test-pq--submitted))))))

(ert-deftest agent-repl-pq-defer-clears-the-composer ()
  "Deferring captures the text and clears the composer."
  (agent-repl-test-pq--with-composer "later please"
    ;; Act
    (agent-repl-queue-deferred-prompt)
    ;; Assert
    (should (equal (with-current-buffer buf (buffer-string)) ""))))

(ert-deftest agent-repl-pq-defer-on-empty-input-submits-nothing ()
  "There is nothing to defer when nothing was written."
  (agent-repl-test-pq--with-composer ""
    ;; Act
    (agent-repl-queue-deferred-prompt)
    ;; Assert
    (should-not agent-repl-test-pq--submitted)))

;;;; ---- ONE path: Emacs holds no prompt in memory ----

(defconst agent-repl-test-pq--retired-in-memory-names
  '(;; The retired outage queue and its release edges.
    "agent-repl-prompt-queue-offer"
    "agent-repl-prompt-queue-drain"
    "agent-repl--prompt-queue-draining"
    "agent-repl--prompt-queue-on-link-up"
    "agent-repl--prompt-queue-on-link-promote"
    "agent-repl--prompt-queue-on-reattached"
    ":outage"
    ;; The retired in-memory deferral queue, its finish-edge release and its
    ;; liveness gate.
    "agent-repl--prompt-queue-enqueue"
    "agent-repl--prompt-queue-on-finish"
    "agent-repl-prompt-queue-pending"
    "agent-repl-prompt-queue-deliverable-p"
    "agent-repl--prompt-queue-blocked-gates"
    ":deferred-prompts")
  "Names of the retired in-memory prompt holding and its release edges.")

(ert-deftest agent-repl-pq-no-production-source-names-a-retired-in-memory-queue ()
  "No production source holds a prompt in Emacs memory.
Owner ruling, 2026-09-28: held prompts survive outages and restarts, so a
prompt the daemon did not take goes to the durable held-prompt ingress
\(held-ingress.el), and a deferral is held by the daemon itself.  A source
naming a retired queue or one of its release edges has reopened a second
path."
  ;; Arrange
  (let* ((dir (file-name-directory (symbol-file 'agent-repl-queue-deferred-prompt)))
         (sources (cl-remove-if (lambda (f) (string-prefix-p "test-" (file-name-nondirectory f)))
                                (directory-files dir t "\\.el\\'")))
         (offenders nil))
    ;; Act
    (dolist (file sources)
      (with-temp-buffer
        (insert-file-contents file)
        (dolist (name agent-repl-test-pq--retired-in-memory-names)
          (goto-char (point-min))
          (when (search-forward name nil t)
            (push (format "%s: %s" (file-name-nondirectory file) name) offenders)))))
    ;; Assert
    (should (> (length sources) 10))
    (should-not offenders)))

(ert-deftest agent-repl-pq-nothing-rides-the-finish-edge-to-release-a-prompt ()
  "The roster's finish edge releases no prompt: the daemon delivers deferrals."
  (should-not (seq-some (lambda (fn) (and (symbolp fn)
                                          (string-prefix-p "agent-repl--prompt-queue"
                                                           (symbol-name fn))))
                        (default-value 'agent-repl-roster-finish-functions))))

(ert-deftest agent-repl-pq-no-release-edge-hangs-off-the-link-or-a-reattach ()
  "Nothing from this file is wired to link-up, a promotion or a reattach."
  (dolist (hook '(agent-repl-link-up-functions
                  agent-repl-link-promote-functions
                  agent-repl-host-reattached-functions))
    (should-not (seq-some (lambda (fn) (and (symbolp fn)
                                            (string-prefix-p "agent-repl--prompt-queue"
                                                             (symbol-name fn))))
                          (default-value hook)))))

(provide 'test-prompt-queue)

;;; test-prompt-queue.el ends here
