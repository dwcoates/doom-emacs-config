;;; test-input.el --- ERT tests for agent-repl input.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;   AGENT_REPL_FORBID_VENDOR_CALLS=1 emacs -batch -Q -l ert \
;;     -l lisp/test-input.el -f ert-run-tests-batch-and-exit
;;
;; SubmitPrompt is stubbed with a function that records its request and
;; invokes the caller's callback SYNCHRONOUSLY, so each answer arm -- a
;; minted turn, a resolved command panel, a recognized-but-unsupported
;; command, the merging refusal, and a transport failure -- is exercised
;; deterministically with no daemon anywhere.
;;
;; WHAT THESE TESTS ARE REALLY ASSERTING, in the two places it matters:
;; that a REFUSAL never costs the user their text, and that every send site
;; sends its OWN origin -- the vocabulary is closed and durable precisely so
;; a stored turn traces back to the editor situation that caused it, and a
;; site sending the wrong value silently corrupts that record.

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                           (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Fixtures ----

(defvar agent-repl-test-input--submitted nil
  "SubmitPrompt requests received, oldest first.")

(defvar agent-repl-test-input--refusals nil
  "Handover refusals handed to host.el, as (WS ARM-PLIST).")

(defvar agent-repl-test-input--queued nil
  "Prompts written to the held-prompt ingress, as (WS SAID ORIGIN KEY).")

(defvar agent-repl-test-input--hold-error nil
  "When non-nil, the stubbed ingress write signals this error.")

(defvar agent-repl-test-input--messages nil
  "Strings passed to `message' during a test.")

(defvar agent-repl-test-input--answer nil
  "The scripted answer: `(:response PLIST)' or `(:failure PLIST)'.")

(defvar agent-repl-test-input--gate :open
  "The composer gate the stubbed host reports.")

(defvar agent-repl-test-input--buffer nil
  "The composer buffer for the workspace under test.")

(defconst agent-repl-test-input--ref '(:id "ws-id-1" :dir "/tmp/agent-repl-test/ws-1")
  "The decoded `WorkspaceRef' production must echo verbatim.")

(defun agent-repl-test-input--turn-answer (&optional id)
  "Return a scripted minted-turn success."
  (list :response (list :arm :success
                        :value (list :arm :turn
                                     :value (list :turn (list :value (or id "turn-1")))))))

(defmacro agent-repl-test-input--with (&rest body)
  "Run BODY against a live composer buffer with the wire stubbed.
The buffer is a real `agent-repl-input-mode' buffer, because the
attachment list and the mode-line notice are buffer-local state and a
fake would not exercise them."
  (declare (indent 0))
  `(let ((agent-repl-test-input--submitted nil)
         (agent-repl-test-input--queued nil)
         (agent-repl-test-input--hold-error nil)
         (agent-repl-test-input--refusals nil)
         (agent-repl-test-input--messages nil)
         (agent-repl-test-input--answer (agent-repl-test-input--turn-answer))
         (agent-repl-test-input--gate :open)
         (agent-repl-test-input--buffer nil))
     (unwind-protect
         (progn
           (setq agent-repl-test-input--buffer
                 (generate-new-buffer " *agent-repl-test-composer*"))
           (with-current-buffer agent-repl-test-input--buffer
             (agent-repl-input-mode)
             (setq-local agent-repl--owning-workspace "ws-one"))
           (cl-letf* (((symbol-function 'agent-repl--ws-current-name) (lambda () "ws-one"))
                      ((symbol-function 'agent-repl--ws-current-log-name) (lambda () "ws-one"))
                      ((symbol-function 'agent-repl--ws-get)
                       (lambda (_ws key)
                         (pcase key
                           (:input-buffer agent-repl-test-input--buffer)
                           (:project-dir "/tmp/agent-repl-test/ws-1")
                           (_ nil))))
                      ((symbol-function 'agent-repl--ws-log-routable-p)
                       (lambda (ws) (and (stringp ws)
                                         (not (member ws '("main" "none"))))))
                      ((symbol-function 'agent-repl--workspace-log-identity)
                       (lambda (ws)
                         (list :project-dir (format "/tmp/agent-repl-test/%s" ws)
                               :workspace-id (format "id-%s" ws))))
                      ((symbol-function 'agent-repl-host-ref)
                       (lambda (_ws) agent-repl-test-input--ref))
                      ((symbol-function 'agent-repl-host-conn) (lambda (_ws) 'test-conn))
                      ((symbol-function 'agent-repl-link-primary) (lambda () 'test-conn))
                      ((symbol-function 'agent-repl-host-composer-gate)
                       (lambda (_ws) agent-repl-test-input--gate))
                      ((symbol-function 'agent-repl--history-push) (lambda (&optional _t) nil))
                      ((symbol-function 'agent-repl--history-reset) (lambda () nil))
                      ((symbol-function 'agent-repl--history-save) (lambda (_ws) nil))
                      ((symbol-function 'agent-repl--kickoff-prompt-summary)
                       (lambda (_ws _raw) nil))
                      ((symbol-function 'agent-repl-host-handle-refusal)
                       (lambda (ws arm)
                         (push (list ws arm) agent-repl-test-input--refusals)))
                      ((symbol-function 'agent-repl-held-ingress-write)
                       (lambda (ws said origin key)
                         (when agent-repl-test-input--hold-error
                           (signal (car agent-repl-test-input--hold-error)
                                   (cdr agent-repl-test-input--hold-error)))
                         (push (list ws said origin key)
                               agent-repl-test-input--queued)
                         (format "/state/held-prompts/held_x_%s.json" key)))
                      ((symbol-function 'run-at-time) (lambda (&rest _) nil))
                      ((symbol-function 'agent-repl--register-timer)
                       (lambda (_key timer) timer))
                      ((symbol-function 'message)
                       (lambda (fmt &rest args)
                         (push (if args (apply #'format fmt args) fmt)
                               agent-repl-test-input--messages)
                         nil))
                      ((symbol-function 'agent-repl-rpc-submit-prompt)
                       (lambda (_conn request &rest keys)
                         (push request agent-repl-test-input--submitted)
                         (let ((failure (plist-get agent-repl-test-input--answer :failure)))
                           (if failure
                               (funcall (plist-get keys :on-failure) failure)
                             (funcall (plist-get keys :on-response)
                                      (plist-get agent-repl-test-input--answer
                                                 :response)))))))
             ,@body))
       (when (buffer-live-p agent-repl-test-input--buffer)
         (kill-buffer agent-repl-test-input--buffer)))))

(defun agent-repl-test-input--type (text)
  "Put TEXT into the composer buffer."
  (with-current-buffer agent-repl-test-input--buffer
    (erase-buffer)
    (insert text)))

(defun agent-repl-test-input--composer-text ()
  "Return the composer buffer's current contents."
  (with-current-buffer agent-repl-test-input--buffer (buffer-string)))

(defun agent-repl-test-input--request ()
  "Return the single recorded SubmitPrompt request."
  (car agent-repl-test-input--submitted))

(defun agent-repl-test-input--blocks ()
  "Return the recorded request's `UserSaid' content blocks."
  (plist-get (plist-get (plist-get (agent-repl-test-input--request) :said) :content)
             :blocks))

;;;; ---- Whose text was sent (audit-3 #51) ----

(ert-deftest agent-repl-input-a-composer-send-clears-the-composer ()
  "The words that went out came out of here, so here is emptied."
  (agent-repl-test-input--with
    ;; Arrange
    (agent-repl-test-input--type "my draft")
    ;; Act
    (agent-repl--send :user-sent)
    ;; Assert
    (should (equal (agent-repl-test-input--composer-text) ""))))

(ert-deftest agent-repl-input-an-explicit-text-send-keeps-the-draft ()
  "A canned command composed its own words and must not eat an unrelated draft."
  (agent-repl-test-input--with
    ;; Arrange
    (agent-repl-test-input--type "my draft")
    ;; Act
    (agent-repl--send :command-update-pr "update the pr")
    ;; Assert
    (should (equal (agent-repl-test-input--composer-text) "my draft"))))

(ert-deftest agent-repl-input-an-explicit-text-send-still-sends-its-own-text ()
  "The draft is untouched precisely because it was never what went out."
  (agent-repl-test-input--with
    ;; Arrange
    (agent-repl-test-input--type "my draft")
    ;; Act
    (agent-repl--send :command-update-pr "update the pr")
    ;; Assert
    (should (equal (plist-get (plist-get (car (agent-repl-test-input--blocks)) :value) :text)
                   "update the pr"))))

(ert-deftest agent-repl-input-an-explicit-text-send-still-pushes-history ()
  "The ring records what was SENT, and a canned prompt was sent."
  (agent-repl-test-input--with
    ;; Arrange
    (agent-repl-test-input--type "my draft")
    (let ((pushed nil))
      (cl-letf (((symbol-function 'agent-repl--history-push)
                 (lambda (&optional text) (push text pushed))))
        ;; Act
        (agent-repl--send :command-update-pr "update the pr"))
      ;; Assert
      (should (equal pushed '("update the pr"))))))

(ert-deftest agent-repl-input-an-explicit-text-send-does-not-reset-history-position ()
  "A surviving draft keeps whatever navigation state it had."
  (agent-repl-test-input--with
    ;; Arrange
    (agent-repl-test-input--type "my draft")
    (let ((reset 0))
      (cl-letf (((symbol-function 'agent-repl--history-reset)
                 (lambda () (cl-incf reset))))
        ;; Act
        (agent-repl--send :command-update-pr "update the pr"))
      ;; Assert
      (should (equal reset 0)))))

(ert-deftest agent-repl-input-an-explicit-text-send-still-clears-attachments ()
  "The submission carried them, whatever composed its text."
  (agent-repl-test-input--with
    ;; Arrange
    (agent-repl-test-input--type "my draft")
    (let ((cleared nil))
      (cl-letf (((symbol-function 'agent-repl-input-clear-attachments)
                 (lambda (ws) (push ws cleared))))
        ;; Act
        (agent-repl--send :command-update-pr "update the pr"))
      ;; Assert
      (should (equal cleared '("ws-one"))))))

(ert-deftest agent-repl-input-a-command-panel-answer-to-an-explicit-send-keeps-the-draft ()
  "Every accepted arm follows the same rule, not just the minted turn."
  (agent-repl-test-input--with
    ;; Arrange
    (agent-repl-test-input--type "my draft")
    (setq agent-repl-test-input--answer
          (list :response (list :arm :success
                                :value (list :arm :command-panel :value nil))))
    ;; Act
    (agent-repl--send :command-update-pr "update the pr")
    ;; Assert
    (should (equal (agent-repl-test-input--composer-text) "my draft"))))

;;;; ---- The optimistic clear (owner ruling) ----

(ert-deftest agent-repl-input-a-composer-send-clears-before-the-daemon-answers ()
  "A from-buffer submit erases the composer at dispatch, not on the ack."
  (agent-repl-test-input--with
    ;; Arrange -- the wire records the request but NEVER answers.
    (agent-repl-test-input--type "my draft")
    (cl-letf (((symbol-function 'agent-repl-rpc-submit-prompt)
               (lambda (_conn request &rest _keys)
                 (push request agent-repl-test-input--submitted))))
      ;; Act
      (agent-repl--send :user-sent)
      ;; Assert -- cleared even though no callback ever fired.
      (should (equal (agent-repl-test-input--composer-text) "")))))

(ert-deftest agent-repl-input-a-composer-send-pushes-history-before-the-daemon-answers ()
  "A from-buffer submit records RAW at dispatch, not on the ack."
  (agent-repl-test-input--with
    ;; Arrange -- capture pushes; the wire never answers.
    (agent-repl-test-input--type "my draft")
    (let ((pushed nil))
      (cl-letf (((symbol-function 'agent-repl--history-push)
                 (lambda (&optional text) (push text pushed)))
                ((symbol-function 'agent-repl-rpc-submit-prompt)
                 (lambda (_conn request &rest _keys)
                   (push request agent-repl-test-input--submitted))))
        ;; Act
        (agent-repl--send :user-sent))
      ;; Assert -- RAW is on the ring before any response.
      (should (equal pushed '("my draft"))))))

(ert-deftest agent-repl-input-a-composer-send-success-ack-does-not-double-push-history ()
  "The optimistic clear already recorded RAW, so the success ack must not re-push."
  (agent-repl-test-input--with
    ;; Arrange -- the default answer is a minted-turn success.
    (agent-repl-test-input--type "my draft")
    (let ((pushed nil))
      (cl-letf (((symbol-function 'agent-repl--history-push)
                 (lambda (&optional text) (push text pushed))))
        ;; Act -- dispatch AND the synchronous success ack both run.
        (agent-repl--send :user-sent))
      ;; Assert -- exactly one push across the whole submission.
      (should (equal pushed '("my draft"))))))

(ert-deftest agent-repl-input-a-composer-send-success-ack-does-not-error-on-empty-composer ()
  "The success ack runs against an already-emptied composer without erroring."
  (agent-repl-test-input--with
    ;; Arrange
    (agent-repl-test-input--type "my draft")
    ;; Act -- dispatch clears; the synchronous success ack follows.
    (agent-repl--send :user-sent)
    ;; Assert -- still empty, no error was raised reaching this point.
    (should (equal (agent-repl-test-input--composer-text) ""))))

(ert-deftest agent-repl-input-a-composer-send-transport-failure-holds-the-said ()
  "The erased draft is not lost: the held-prompt ingress carries the said."
  (agent-repl-test-input--with
    ;; Arrange -- the wire fails.
    (agent-repl-test-input--type "my draft")
    (setq agent-repl-test-input--answer
          (list :failure (list :kind :transport :message "no daemon")))
    ;; Act
    (agent-repl--send :user-sent)
    ;; Assert -- one held entry carrying the full said.
    (should (equal (length agent-repl-test-input--queued) 1))
    (pcase-let ((`(,_ws ,said ,_origin ,_key)
                 (car agent-repl-test-input--queued)))
      (should (equal (plist-get (plist-get (car (plist-get (plist-get said :content) :blocks))
                                           :value)
                                :text)
                     "my draft")))))

(ert-deftest agent-repl-input-a-canned-send-does-not-erase-the-draft-before-the-ack ()
  "A canned command never clears the composer optimistically (audit-3 #51 guard)."
  (agent-repl-test-input--with
    ;; Arrange -- an unrelated draft; the wire never answers.
    (agent-repl-test-input--type "my draft")
    (cl-letf (((symbol-function 'agent-repl-rpc-submit-prompt)
               (lambda (_conn request &rest _keys)
                 (push request agent-repl-test-input--submitted))))
      ;; Act
      (agent-repl--send :command-update-pr "update the pr")
      ;; Assert -- the draft survives dispatch untouched.
      (should (equal (agent-repl-test-input--composer-text) "my draft")))))

(ert-deftest agent-repl-input-a-whitespace-only-send-does-not-clear-or-record ()
  "An empty submission clears nothing and records nothing -- there was no prompt."
  (agent-repl-test-input--with
    ;; Arrange
    (agent-repl-test-input--type "   \n\t ")
    (let ((pushed nil))
      (cl-letf (((symbol-function 'agent-repl--history-push)
                 (lambda (&optional text) (push text pushed))))
        ;; Act
        (should-not (agent-repl--send :user-sent)))
      ;; Assert -- composer intact, nothing pushed.
      (should (equal (agent-repl-test-input--composer-text) "   \n\t "))
      (should (null pushed)))))

;;;; ---- The uuid ----

(ert-deftest agent-repl-input-uuid-has-the-rfc-4122-shape ()
  "The idempotency key is a canonical 8-4-4-4-12 hex UUID."
  (should (string-match-p
           "\\`[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{12\\}\\'"
           (agent-repl--uuid))))

(ert-deftest agent-repl-input-uuid-states-version-4 ()
  "The version nibble is 4: the key is a random UUID, and says so."
  (should (eq (aref (agent-repl--uuid) 14) ?4)))

(ert-deftest agent-repl-input-uuid-states-the-rfc-variant ()
  "The variant nibble is one of 8/9/a/b, as RFC 4122 requires."
  (should (memq (aref (agent-repl--uuid) 19) '(?8 ?9 ?a ?b))))

(ert-deftest agent-repl-input-uuid-is-fresh-each-call ()
  "A retried request must not reuse a key, so each call mints a new one."
  (should-not (equal (agent-repl--uuid) (agent-repl--uuid))))

;;;; ---- The gate: one test per arm of the fixed vocabulary ----

(ert-deftest agent-repl-input-gate-open-sends ()
  "An open composer submits."
  (agent-repl-test-input--with
    (setq agent-repl-test-input--gate :open)
    (agent-repl-test-input--type "hello")
    (agent-repl--send :user-sent)
    (should (agent-repl-test-input--request))))

(ert-deftest agent-repl-input-gate-merging-refuses ()
  "A merge owns the session: the submission is refused and no rpc is sent."
  (agent-repl-test-input--with
    (setq agent-repl-test-input--gate :merging)
    (agent-repl-test-input--type "hello")
    (should-error (agent-repl--send :user-sent) :type 'user-error)
    (should-not agent-repl-test-input--submitted)))

(ert-deftest agent-repl-input-gate-merging-keeps-the-text ()
  "A gate refusal costs the user nothing they wrote."
  (agent-repl-test-input--with
    (setq agent-repl-test-input--gate :merging)
    (agent-repl-test-input--type "hello")
    (ignore-errors (agent-repl--send :user-sent))
    (should (equal (agent-repl-test-input--composer-text) "hello"))))

(ert-deftest agent-repl-input-gate-merging-draws-its-own-message ()
  "The merging refusal draws the arm's fixed message, not a composed one."
  (agent-repl-test-input--with
    (setq agent-repl-test-input--gate :merging)
    (agent-repl-test-input--type "hello")
    (should (equal (cdr (should-error (agent-repl--send :user-sent) :type 'user-error))
                   '("composer closed: a merge owns this session")))))

(ert-deftest agent-repl-input-gate-draining-refuses ()
  "A draining daemon closes the composer."
  (agent-repl-test-input--with
    (setq agent-repl-test-input--gate :draining)
    (agent-repl-test-input--type "hello")
    (should (equal (cdr (should-error (agent-repl--send :user-sent) :type 'user-error))
                   '("composer closed: daemon draining")))
    (should-not agent-repl-test-input--submitted)))

(ert-deftest agent-repl-input-gate-restarting-refuses ()
  "A restarting session closes the composer."
  (agent-repl-test-input--with
    (setq agent-repl-test-input--gate :restarting)
    (agent-repl-test-input--type "hello")
    (should (equal (cdr (should-error (agent-repl--send :user-sent) :type 'user-error))
                   '("composer closed: restarting")))
    (should-not agent-repl-test-input--submitted)))

(ert-deftest agent-repl-input-gate-merge-parked-sends ()
  "A parked merge leaves the composer OPEN WITH CONTEXT: it submits."
  (agent-repl-test-input--with
    (setq agent-repl-test-input--gate :merge-parked)
    (agent-repl-test-input--type "hello")
    (agent-repl--send :user-sent)
    (should (agent-repl-test-input--request))))

(ert-deftest agent-repl-input-gate-merge-parked-badges-the-composer ()
  "The parked gate draws its badge so the user knows who receives the prompt."
  (agent-repl-test-input--with
    (setq agent-repl-test-input--gate :merge-parked)
    (agent-repl-test-input--type "hello")
    (agent-repl--send :user-sent)
    (should (equal (buffer-local-value 'agent-repl-input-notice
                                       agent-repl-test-input--buffer)
                   agent-repl--input-merge-parked-badge))))

(ert-deftest agent-repl-input-gate-no-session-sends ()
  "SubmitPrompt has no precondition: the daemon starts the session."
  (agent-repl-test-input--with
    (setq agent-repl-test-input--gate :no-session)
    (agent-repl-test-input--type "hello")
    (agent-repl--send :user-sent)
    (should (agent-repl-test-input--request))))

(ert-deftest agent-repl-input-gate-terminal-sends ()
  "A terminal session simply submits: the daemon revives it implicitly."
  (agent-repl-test-input--with
    (setq agent-repl-test-input--gate :terminal)
    (agent-repl-test-input--type "hello")
    (agent-repl--send :user-sent)
    (should (agent-repl-test-input--request))))

(ert-deftest agent-repl-input-gate-unknown-sends ()
  "With no host push yet the daemon is still the authority: it submits."
  (agent-repl-test-input--with
    (setq agent-repl-test-input--gate :unknown)
    (agent-repl-test-input--type "hello")
    (agent-repl--send :user-sent)
    (should (agent-repl-test-input--request))))

;;;; ---- The request's required fields ----

(ert-deftest agent-repl-input-submit-echoes-the-workspace-ref ()
  "The workspace ref is REQUIRED and travels verbatim, never rebuilt."
  (agent-repl-test-input--with
    (agent-repl-test-input--type "hello")
    (agent-repl--send :user-sent)
    (should (equal (plist-get (agent-repl-test-input--request) :workspace)
                   agent-repl-test-input--ref))))

(ert-deftest agent-repl-input-submit-carries-an-idempotency-key ()
  "Every submission carries a key so a retry is not a second turn."
  (agent-repl-test-input--with
    (agent-repl-test-input--type "hello")
    (agent-repl--send :user-sent)
    (should (stringp (plist-get (agent-repl-test-input--request) :idempotency-key)))))

(ert-deftest agent-repl-input-submit-correlates-the-request-and-response ()
  "SubmitPrompt logs use the wire idempotency key as their request id."
  (agent-repl-test-input--with
    ;; Arrange
    (let (seen)
      (cl-letf (((symbol-function 'agent-repl--emit-log-record)
                 (lambda (_ws _level _verbosity fmt _args &rest _)
                   (push (list fmt
                               agent-repl--log-context-workspace
                               agent-repl--log-context-request-id)
                         seen))))
        ;; Act
        (agent-repl--input-submit
         "ws-one" (list :content (list :blocks nil))
         :user-sent "hello" "request-1" t)
        ;; Assert
        (should (> (length seen) 1))
        (should (cl-every (lambda (entry)
                            (and (equal (nth 1 entry) "ws-one")
                                 (equal (nth 2 entry) "request-1")))
                          seen))))))

(ert-deftest agent-repl-input-submit-carries-the-text-block ()
  "The composed text travels as one TextBlock."
  (agent-repl-test-input--with
    (agent-repl-test-input--type "hello")
    (agent-repl--send :user-sent)
    (should (equal (agent-repl-test-input--blocks)
                   (list (list :arm :text :value (list :text "hello")))))))

(ert-deftest agent-repl-input-submit-refuses-without-a-ref ()
  "A workspace with no daemon identity cannot be submitted for at all."
  (agent-repl-test-input--with
    (cl-letf (((symbol-function 'agent-repl-host-ref) (lambda (_ws) nil)))
      (agent-repl-test-input--type "hello")
      (should-error (agent-repl--send :user-sent))
      (should-not agent-repl-test-input--submitted))))

(ert-deftest agent-repl-input-refuses-an-origin-outside-the-vocabulary ()
  "The origin vocabulary is closed: an unknown value never reaches the wire."
  (agent-repl-test-input--with
    (agent-repl-test-input--type "hello")
    (should-error (agent-repl--send :not-a-real-origin))
    (should-not agent-repl-test-input--submitted)))

(ert-deftest agent-repl-input-refuses-the-unspecified-origin ()
  "UNSPECIFIED has no elisp spelling to reach for by accident."
  (agent-repl-test-input--with
    (agent-repl-test-input--type "hello")
    (should-error (agent-repl--send :unspecified))
    (should-not agent-repl-test-input--submitted)))

;;;; ---- Empty input ----

(ert-deftest agent-repl-input-empty-buffer-sends-nothing ()
  "RET on an empty composer dispatches nothing."
  (agent-repl-test-input--with
    (agent-repl-test-input--type "")
    (should-not (agent-repl--send :user-sent))
    (should-not agent-repl-test-input--submitted)))

(ert-deftest agent-repl-input-whitespace-only-sends-nothing ()
  "Whitespace is not something to say."
  (agent-repl-test-input--with
    (agent-repl-test-input--type "   \n\t ")
    (should-not (agent-repl--send :user-sent))
    (should-not agent-repl-test-input--submitted)))

;;;; ---- Every send site's own origin ----

(ert-deftest agent-repl-input-origin-user-sent ()
  "`agent-repl-send' sends `:user-sent'."
  (agent-repl-test-input--with
    (agent-repl-test-input--type "hello")
    (agent-repl-send)
    (should (eq (plist-get (agent-repl-test-input--request) :origin) :user-sent))))

(ert-deftest agent-repl-input-origin-user-sent-and-hide ()
  "`agent-repl-send-and-hide' sends `:user-sent-and-hide'."
  (agent-repl-test-input--with
    (cl-letf (((symbol-function 'agent-repl--on-close) (lambda () nil)))
      (agent-repl-test-input--type "hello")
      (agent-repl-send-and-hide)
      (should (eq (plist-get (agent-repl-test-input--request) :origin)
                  :user-sent-and-hide)))))

(ert-deftest agent-repl-input-origin-user-sent-with-metaprompt ()
  "`agent-repl-send-with-metaprompt' sends `:user-sent-with-metaprompt'."
  (agent-repl-test-input--with
    (agent-repl-test-input--type "hello")
    (agent-repl-send-with-metaprompt)
    (should (eq (plist-get (agent-repl-test-input--request) :origin)
                :user-sent-with-metaprompt))))

(ert-deftest agent-repl-input-origin-user-sent-with-postfix ()
  "`agent-repl-send-with-postfix' sends `:user-sent-with-postfix'."
  (agent-repl-test-input--with
    (agent-repl-test-input--type "hello")
    (agent-repl-send-with-postfix)
    (should (eq (plist-get (agent-repl-test-input--request) :origin)
                :user-sent-with-postfix))))

(ert-deftest agent-repl-input-origin-user-sent-with-prefix ()
  "`agent-repl-send-with-prefix' sends `:user-sent-with-prefix'."
  (agent-repl-test-input--with
    (agent-repl-test-input--type "hello")
    (agent-repl-send-with-prefix)
    (should (eq (plist-get (agent-repl-test-input--request) :origin)
                :user-sent-with-prefix))))

(ert-deftest agent-repl-input-origin-metaprompt-read ()
  "`agent-repl--fire-metaprompt-read' sends `:metaprompt-read'."
  (agent-repl-test-input--with
    (agent-repl--fire-metaprompt-read "ws-one")
    (should (eq (plist-get (agent-repl-test-input--request) :origin) :metaprompt-read))))

(ert-deftest agent-repl-input-every-origin-has-one-site ()
  "The accepted vocabulary is exactly the thirteen Emacs send sites."
  (should (equal agent-repl--input-origins
                 '(:user-sent
                   :user-sent-and-hide
                   :user-sent-with-metaprompt
                   :user-sent-with-postfix
                   :user-sent-with-prefix
                   :metaprompt-read
                   :command-diff-analysis
                   :command-explain-context
                   :command-explain-prompt
                   :command-update-pr
                   :command-rebase
                   :command-create-or-update-pr
                   :deferred-prompt))))

;;;; ---- Send-variant composition ----

(ert-deftest agent-repl-input-postfix-variant-appends-its-text ()
  "The postfix variant composes BEFORE submission, which is free."
  (agent-repl-test-input--with
    (agent-repl-test-input--type "hello")
    (agent-repl-send-with-postfix)
    (should (string-suffix-p agent-repl-send-postfix
                             (plist-get (plist-get (car (agent-repl-test-input--blocks))
                                                   :value)
                                        :text)))))

(ert-deftest agent-repl-input-prefix-variant-prepends-its-text ()
  "The prefix variant prepends its text to what was typed."
  (agent-repl-test-input--with
    (agent-repl-test-input--type "hello")
    (agent-repl-send-with-prefix)
    (should (string-prefix-p agent-repl-send-prefix
                             (plist-get (plist-get (car (agent-repl-test-input--blocks))
                                                   :value)
                                        :text)))))

(ert-deftest agent-repl-input-metaprompt-rides-inside-the-sentinel-markers ()
  "The read-directive is bracketed so the daemon strips it from the DRAWN row."
  (agent-repl-test-input--with
    (agent-repl-test-input--type "hello")
    (agent-repl-send-with-metaprompt)
    (let ((text (plist-get (plist-get (car (agent-repl-test-input--blocks)) :value) :text)))
      (should (string-prefix-p agent-repl--meta-open text))
      (should (string-match-p (regexp-quote agent-repl--meta-close) text))
      (should (string-suffix-p "hello" text)))))

(ert-deftest agent-repl-input-ordinary-send-carries-no-metaprompt ()
  "An ordinary send prepends nothing: the metaprompt is the system prompt."
  (agent-repl-test-input--with
    (agent-repl-test-input--type "hello")
    (agent-repl-send)
    (should (equal (plist-get (plist-get (car (agent-repl-test-input--blocks)) :value) :text)
                   "hello"))))

(ert-deftest agent-repl-input-metaprompt-skipped-for-a-slash-command ()
  "A slash command owns its own behavior, so the directive is not prepended."
  (agent-repl-test-input--with
    (agent-repl-test-input--type "/clear")
    (agent-repl-send-with-metaprompt)
    (should (equal (plist-get (plist-get (car (agent-repl-test-input--blocks)) :value) :text)
                   "/clear"))))

;;;; ---- The answer arms ----

(ert-deftest agent-repl-input-turn-arm-clears-the-composer ()
  "A minted turn means the words are the daemon's now: the composer clears."
  (agent-repl-test-input--with
    (agent-repl-test-input--type "hello")
    (agent-repl--send :user-sent)
    (should (equal (agent-repl-test-input--composer-text) ""))))

(ert-deftest agent-repl-input-turn-arm-runs-the-posthooks ()
  "The posthooks run on the minted-turn arm."
  (agent-repl-test-input--with
    (let* ((seen nil)
           (agent-repl-send-posthooks
            (list (cons "^hello$" (lambda (_ws raw) (setq seen raw))))))
      (agent-repl-test-input--type "hello")
      (agent-repl--send :user-sent)
      (should (equal seen "hello")))))

(ert-deftest agent-repl-input-command-panel-arm-clears-the-composer ()
  "A resolved panel is an ANSWER: nothing is awaited and the input clears."
  (agent-repl-test-input--with
    (setq agent-repl-test-input--answer
          '(:response (:arm :success
                       :value (:arm :command-panel :value (:arm :status :value nil)))))
    (agent-repl-test-input--type "/status")
    (agent-repl--send :user-sent)
    (should (equal (agent-repl-test-input--composer-text) ""))))

(ert-deftest agent-repl-input-command-panel-arm-runs-no-posthooks ()
  "No turn was minted, so there is nothing for a posthook to post-process."
  (agent-repl-test-input--with
    (setq agent-repl-test-input--answer
          '(:response (:arm :success
                       :value (:arm :command-panel :value (:arm :status :value nil)))))
    (let* ((ran nil)
           (agent-repl-send-posthooks
            (list (cons "" (lambda (_ws _raw) (setq ran t))))))
      (agent-repl-test-input--type "/status")
      (agent-repl--send :user-sent)
      (should-not ran))))

(ert-deftest agent-repl-input-command-refused-arm-clears-the-composer ()
  "A recognized-but-unsupported command is answered, so the input clears."
  (agent-repl-test-input--with
    (setq agent-repl-test-input--answer
          '(:response (:arm :success
                       :value (:arm :command-refused :value (:command "/agents")))))
    (agent-repl-test-input--type "/agents")
    (agent-repl--send :user-sent)
    (should (equal (agent-repl-test-input--composer-text) ""))))

(ert-deftest agent-repl-input-command-acted-arm-clears-the-composer ()
  "A session-acting command was acted on: answered, nothing awaited, so the
input clears."
  (agent-repl-test-input--with
    (setq agent-repl-test-input--answer
          '(:response (:arm :success :value (:arm :command-acted :value nil))))
    (agent-repl-test-input--type "/model opus")
    (agent-repl--send :user-sent)
    (should (equal (agent-repl-test-input--composer-text) ""))))

(ert-deftest agent-repl-input-command-acted-arm-runs-no-posthooks ()
  "No turn was minted, so there is nothing for a posthook to post-process."
  (agent-repl-test-input--with
    (setq agent-repl-test-input--answer
          '(:response (:arm :success :value (:arm :command-acted :value nil))))
    (let* ((ran nil)
           (agent-repl-send-posthooks
            (list (cons "" (lambda (_ws _raw) (setq ran t))))))
      (agent-repl-test-input--type "/model opus")
      (agent-repl--send :user-sent)
      (should-not ran))))

(ert-deftest agent-repl-input-duplicate-submission-restores-the-composer ()
  "A `duplicate_submission\\=' refusal restores the composer text.
The key was already accepted, so THIS submission was refused and its text
is owed back to the composer."
  ;; Arrange.
  (agent-repl-test-input--with
    (setq agent-repl-test-input--answer
          '(:response (:arm :error :value (:reason (:arm :duplicate-submission :value nil)))))
    (agent-repl-test-input--type "hello")
    ;; Act.
    (agent-repl--send :user-sent)
    ;; Assert.
    (should (equal (agent-repl-test-input--composer-text) "hello"))))

(ert-deftest agent-repl-input-duplicate-submission-holds-nothing ()
  "A duplicate key is an ANSWER, not an outage: nothing is queued for resend."
  (agent-repl-test-input--with
    (setq agent-repl-test-input--answer
          '(:response (:arm :error :value (:reason (:arm :duplicate-submission :value nil)))))
    (agent-repl-test-input--type "hello")
    (agent-repl--send :user-sent)
    (should-not agent-repl-test-input--queued)))

(ert-deftest agent-repl-input-duplicate-submission-states-the-key-was-accepted ()
  "The user is told plainly that the earlier submission stands."
  (agent-repl-test-input--with
    (setq agent-repl-test-input--answer
          '(:response (:arm :error :value (:reason (:arm :duplicate-submission :value nil)))))
    (agent-repl-test-input--type "hello")
    (agent-repl--send :user-sent)
    (should (seq-some (lambda (text) (string-match-p "already accepted" text))
                      agent-repl-test-input--messages))))

(ert-deftest agent-repl-input-duplicate-submission-routes-to-no-handover ()
  "It is a refusal about identity, not a handover: host.el is not involved."
  (agent-repl-test-input--with
    (setq agent-repl-test-input--answer
          '(:response (:arm :error :value (:reason (:arm :duplicate-submission :value nil)))))
    (agent-repl-test-input--type "hello")
    (agent-repl--send :user-sent)
    (should-not agent-repl-test-input--refusals)))

(ert-deftest agent-repl-input-bubble-refused-restores-the-composer ()
  "A `bubble_refused\\=' refusal restores the composer text.
The shim refused the bubble-addressed prompt, so nothing landed and the
user is left looking at exactly what they wrote."
  ;; Arrange.
  (agent-repl-test-input--with
    (setq agent-repl-test-input--answer
          '(:response (:arm :error :value (:reason (:arm :bubble-refused :value (:detail "" :kind (:arm :not-deliverable :value nil)))))))
    (agent-repl-test-input--type "hello")
    ;; Act.
    (agent-repl--send :user-sent)
    ;; Assert.
    (should (equal (agent-repl-test-input--composer-text) "hello"))))

(ert-deftest agent-repl-input-bubble-refused-holds-nothing ()
  "A re-drive would meet the same refusal, so nothing is queued."
  (agent-repl-test-input--with
    (setq agent-repl-test-input--answer
          '(:response (:arm :error :value (:reason (:arm :bubble-refused :value (:detail "" :kind (:arm :agent-busy :value nil)))))))
    (agent-repl-test-input--type "hello")
    (agent-repl--send :user-sent)
    (should-not agent-repl-test-input--queued)))

(ert-deftest agent-repl-input-bubble-refused-routes-to-no-handover ()
  "It is the shim's answer about an agent, not a handover: host.el is out."
  (agent-repl-test-input--with
    (setq agent-repl-test-input--answer
          '(:response (:arm :error :value (:reason (:arm :bubble-refused :value (:detail "" :kind (:arm :agent-busy :value nil)))))))
    (agent-repl-test-input--type "hello")
    (agent-repl--send :user-sent)
    (should-not agent-repl-test-input--refusals)))

(ert-deftest agent-repl-input-bubble-refused-not-deliverable-names-the-kind ()
  "The `not_deliverable' kind says there is no route to that agent."
  (agent-repl-test-input--with
    (setq agent-repl-test-input--answer
          '(:response (:arm :error :value (:reason (:arm :bubble-refused :value (:detail "" :kind (:arm :not-deliverable :value nil)))))))
    (agent-repl-test-input--type "hello")
    (agent-repl--send :user-sent)
    (should (seq-some (lambda (text) (string-match-p "no route to that agent" text))
                      agent-repl-test-input--messages))))

(ert-deftest agent-repl-input-bubble-refused-agent-busy-names-the-kind ()
  "The `agent_busy' kind says the addressed agent's own turn is running."
  (agent-repl-test-input--with
    (setq agent-repl-test-input--answer
          '(:response (:arm :error :value (:reason (:arm :bubble-refused :value (:detail "" :kind (:arm :agent-busy :value nil)))))))
    (agent-repl-test-input--type "hello")
    (agent-repl--send :user-sent)
    (should (seq-some (lambda (text)
                        (string-match-p "that agent's own turn is running" text))
                      agent-repl-test-input--messages))))

(ert-deftest agent-repl-input-bubble-refused-echoes-the-detail ()
  "The shim's own account rides into the echo area verbatim."
  (agent-repl-test-input--with
    (setq agent-repl-test-input--answer
          '(:response (:arm :error :value (:reason (:arm :bubble-refused :value (:detail "no such agent kind" :kind (:arm :not-deliverable :value nil)))))))
    (agent-repl-test-input--type "hello")
    (agent-repl--send :user-sent)
    (should (seq-some (lambda (text) (string-match-p "(no such agent kind)" text))
                      agent-repl-test-input--messages))))

(ert-deftest agent-repl-input-bubble-refused-omits-an-empty-detail ()
  "An empty detail adds no empty parenthetical to the sentence."
  (agent-repl-test-input--with
    (setq agent-repl-test-input--answer
          '(:response (:arm :error :value (:reason (:arm :bubble-refused :value (:detail "" :kind (:arm :agent-busy :value nil)))))))
    (agent-repl-test-input--type "hello")
    (agent-repl--send :user-sent)
    (should-not (seq-some (lambda (text) (string-match-p "()" text))
                          agent-repl-test-input--messages))))

(ert-deftest agent-repl-input-merging-error-restores-the-composer ()
  "A `merging\\=' refusal restores the composer text.
The prompt would have been orphaned by the merge, so the user resubmits it
once the merge resolves -- from the composer, where they left it."
  ;; Arrange.
  (agent-repl-test-input--with
    (setq agent-repl-test-input--answer
          '(:response (:arm :error :value (:reason (:arm :merging :value nil)))))
    (agent-repl-test-input--type "hello")
    ;; Act.
    (agent-repl--send :user-sent)
    ;; Assert.
    (should (equal (agent-repl-test-input--composer-text) "hello"))))

(ert-deftest agent-repl-input-merging-error-flashes-the-refusal ()
  "The merging refusal draws its own mode-line flash."
  (agent-repl-test-input--with
    (setq agent-repl-test-input--answer
          '(:response (:arm :error :value (:reason (:arm :merging :value nil)))))
    (agent-repl-test-input--type "hello")
    (agent-repl--send :user-sent)
    (should (equal (buffer-local-value 'agent-repl-input-notice
                                       agent-repl-test-input--buffer)
                   "refused: merge in flight"))))

(ert-deftest agent-repl-input-merging-error-does-not-queue ()
  "A REFUSAL is an answer, so it is not an outage to hold the prompt for."
  (agent-repl-test-input--with
    (setq agent-repl-test-input--answer
          '(:response (:arm :error :value (:reason (:arm :merging :value nil)))))
    (agent-repl-test-input--type "hello")
    (agent-repl--send :user-sent)
    (should-not agent-repl-test-input--queued)))

(ert-deftest agent-repl-input-transferring-away-routes-to-the-host ()
  "The handover ordering is enforced BY REFUSAL on every per-workspace rpc."
  (agent-repl-test-input--with
    ;; Arrange
    (setq agent-repl-test-input--answer
          '(:response (:arm :error :value (:reason (:arm :transferring-away
                                                   :value (:address "127.0.0.1:9100"))))))
    (agent-repl-test-input--type "hello")
    ;; Act
    (agent-repl--send :user-sent)
    ;; Assert
    (should (equal (nth 1 (car agent-repl-test-input--refusals))
                   '(:arm :transferring-away :value (:address "127.0.0.1:9100"))))))

(ert-deftest agent-repl-input-transferring-away-restores-the-composer ()
  "A `transferring_away\\=' handover refusal restores the composer text.
Same handover contract as `not_yet_adopted\\=': the prompt is held under its
own key and the words stay visible."
  ;; Arrange.
  (agent-repl-test-input--with
    (setq agent-repl-test-input--answer
          '(:response (:arm :error :value (:reason (:arm :transferring-away
                                                   :value (:address "127.0.0.1:9100"))))))
    (agent-repl-test-input--type "hello")
    ;; Act.
    (agent-repl--send :user-sent)
    ;; Assert.
    (should (equal (agent-repl-test-input--composer-text) "hello"))))

(ert-deftest agent-repl-input-transferring-away-draws-no-refusal-message ()
  "The one rollout that is supposed to be invisible must stay invisible."
  (agent-repl-test-input--with
    ;; Arrange
    (setq agent-repl-test-input--answer
          '(:response (:arm :error :value (:reason (:arm :transferring-away
                                                   :value (:address "127.0.0.1:9100"))))))
    (agent-repl-test-input--type "hello")
    ;; Act
    (agent-repl--send :user-sent)
    ;; Assert
    (should-not (seq-some (lambda (text) (string-match-p "refused" text))
                          agent-repl-test-input--messages))))

(ert-deftest agent-repl-input-transferring-away-holds-the-prompt ()
  "The prompt was refused, not consumed: it is held for the retry."
  (agent-repl-test-input--with
    ;; Arrange
    (setq agent-repl-test-input--answer
          '(:response (:arm :error :value (:reason (:arm :transferring-away
                                                   :value (:address "127.0.0.1:9100"))))))
    (agent-repl-test-input--type "hello")
    ;; Act
    (agent-repl--send :user-sent)
    ;; Assert
    (should (equal (length agent-repl-test-input--queued) 1))))

(ert-deftest agent-repl-input-transferring-away-holds-under-the-same-key ()
  "A re-drive is a RETRY of THIS submission, so the idempotency key rides."
  (agent-repl-test-input--with
    ;; Arrange
    (setq agent-repl-test-input--answer
          '(:response (:arm :error :value (:reason (:arm :transferring-away
                                                   :value (:address "127.0.0.1:9100"))))))
    (agent-repl-test-input--type "hello")
    ;; Act
    (agent-repl--send :user-sent)
    ;; Assert
    (should (equal (nth 3 (car agent-repl-test-input--queued))
                   (plist-get (agent-repl-test-input--request) :idempotency-key)))))

(ert-deftest agent-repl-input-not-yet-adopted-routes-to-the-host ()
  "The successor has not taken the workspace over yet: host.el retries the adopt."
  (agent-repl-test-input--with
    ;; Arrange
    (setq agent-repl-test-input--answer
          '(:response (:arm :error :value (:reason (:arm :not-yet-adopted :value nil)))))
    (agent-repl-test-input--type "hello")
    ;; Act
    (agent-repl--send :user-sent)
    ;; Assert
    (should (equal (nth 1 (car agent-repl-test-input--refusals))
                   '(:arm :not-yet-adopted :value nil)))))

(ert-deftest agent-repl-input-not-yet-adopted-restores-the-composer ()
  "A `not_yet_adopted\\=' handover refusal restores the composer text.
The queue holds the prompt for the re-drive; the composer still shows what
was written, because the handover is supposed to be invisible."
  ;; Arrange.
  (agent-repl-test-input--with
    (setq agent-repl-test-input--answer
          '(:response (:arm :error :value (:reason (:arm :not-yet-adopted :value nil)))))
    (agent-repl-test-input--type "hello")
    ;; Act.
    (agent-repl--send :user-sent)
    ;; Assert.
    (should (equal (agent-repl-test-input--composer-text) "hello"))))

(ert-deftest agent-repl-input-not-yet-adopted-holds-the-prompt ()
  "Held until the successor owns the workspace, then re-driven."
  (agent-repl-test-input--with
    ;; Arrange
    (setq agent-repl-test-input--answer
          '(:response (:arm :error :value (:reason (:arm :not-yet-adopted :value nil)))))
    (agent-repl-test-input--type "hello")
    ;; Act
    (agent-repl--send :user-sent)
    ;; Assert
    (should (equal (length agent-repl-test-input--queued) 1))))

(ert-deftest agent-repl-input-not-yet-adopted-draws-no-refusal-message ()
  "INFO, not a report: the user is not told a rollout refused their prompt."
  (agent-repl-test-input--with
    ;; Arrange
    (setq agent-repl-test-input--answer
          '(:response (:arm :error :value (:reason (:arm :not-yet-adopted :value nil)))))
    (agent-repl-test-input--type "hello")
    ;; Act
    (agent-repl--send :user-sent)
    ;; Assert
    (should-not (seq-some (lambda (text) (string-match-p "refused" text))
                          agent-repl-test-input--messages))))

(ert-deftest agent-repl-input-no-session-error-restores-the-composer ()
  "A `no_session\\=' refusal restores the composer text.
The daemon brings the session up itself; nothing is owed by the user but
their words, which come straight back."
  ;; Arrange.
  (agent-repl-test-input--with
    (setq agent-repl-test-input--answer
          '(:response (:arm :error :value (:reason (:arm :no-session :value nil)))))
    (agent-repl-test-input--type "hello")
    ;; Act.
    (agent-repl--send :user-sent)
    ;; Assert.
    (should (equal (agent-repl-test-input--composer-text) "hello"))))

(ert-deftest agent-repl-input-no-session-error-routes-to-no-handover ()
  "Only the two handover arms reach host.el; every other arm is a refusal."
  (agent-repl-test-input--with
    ;; Arrange
    (setq agent-repl-test-input--answer
          '(:response (:arm :error :value (:reason (:arm :no-session :value nil)))))
    (agent-repl-test-input--type "hello")
    ;; Act
    (agent-repl--send :user-sent)
    ;; Assert
    (should-not agent-repl-test-input--refusals)))

(ert-deftest agent-repl-input-feed-undecodable-error-holds-nothing ()
  "A refusal is an ANSWER, so a non-handover arm is not an outage to hold for."
  (agent-repl-test-input--with
    ;; Arrange
    (setq agent-repl-test-input--answer
          '(:response (:arm :error :value (:reason (:arm :feed-undecodable :value nil)))))
    (agent-repl-test-input--type "hello")
    ;; Act
    (agent-repl--send :user-sent)
    ;; Assert
    (should-not agent-repl-test-input--queued)))

(ert-deftest agent-repl-input-feed-undecodable-error-names-the-arm ()
  "An arm this composer has no treatment for is drawn naming the arm."
  (agent-repl-test-input--with
    ;; Arrange
    (setq agent-repl-test-input--answer
          '(:response (:arm :error :value (:reason (:arm :feed-undecodable :value nil)))))
    (agent-repl-test-input--type "hello")
    ;; Act
    (agent-repl--send :user-sent)
    ;; Assert
    (should (seq-some (lambda (text) (string-match-p "feed-undecodable" text))
                      agent-repl-test-input--messages))))

(ert-deftest agent-repl-input-transport-failure-clears-the-composer ()
  "A from-buffer submit was cleared optimistically at dispatch; nothing is lost
because the held-prompt ingress holds the said AND the RAW text sits in the history
ring (owner ruling overrides the old keep-the-text contract)."
  (agent-repl-test-input--with
    (setq agent-repl-test-input--answer '(:failure (:kind :transport :message "gone")))
    (agent-repl-test-input--type "hello")
    (agent-repl--send :user-sent)
    (should (equal (agent-repl-test-input--composer-text) ""))))

(ert-deftest agent-repl-input-transport-failure-writes-the-held-prompt-ingress ()
  "A transport failure writes the prompt to the durable held-prompt ingress."
  (agent-repl-test-input--with
    (setq agent-repl-test-input--answer '(:failure (:kind :transport :message "gone")))
    (agent-repl-test-input--type "hello")
    (agent-repl--send :user-sent)
    (should (equal (length agent-repl-test-input--queued) 1))
    (should (equal (nth 2 (car agent-repl-test-input--queued)) :user-sent))))

(ert-deftest agent-repl-input-transport-failure-holds-under-this-attempts-key ()
  "The entry carries THIS attempt\='s key: the re-drive is a retry of it."
  (agent-repl-test-input--with
    (setq agent-repl-test-input--answer '(:failure (:kind :transport :message "gone")))
    (agent-repl-test-input--type "hello")
    (let ((key (agent-repl--send :user-sent)))
      (should (equal (nth 3 (car agent-repl-test-input--queued)) key)))))

(ert-deftest agent-repl-input-a-hold-that-cannot-be-written-is-an-error-naming-the-words ()
  "An ingress write that fails is an ERROR and a message carrying the text."
  (agent-repl-test-input--with
    ;; Arrange
    (setq agent-repl-test-input--answer '(:failure (:kind :transport :message "gone"))
          agent-repl-test-input--hold-error '(file-error "Disk full"))
    (agent-repl-test-input--type "hello")
    ;; Act
    (agent-repl--send :user-sent)
    ;; Assert
    (should (seq-some (lambda (text) (string-match-p "could not be saved.*hello" text))
                      agent-repl-test-input--messages))))

(ert-deftest agent-repl-input-a-hold-that-cannot-be-written-does-not-claim-it-is-held ()
  "A failed ingress write never tells the user the prompt is held."
  (agent-repl-test-input--with
    ;; Arrange
    (setq agent-repl-test-input--answer '(:failure (:kind :transport :message "gone"))
          agent-repl-test-input--hold-error '(file-error "Disk full"))
    (agent-repl-test-input--type "hello")
    ;; Act
    (agent-repl--send :user-sent)
    ;; Assert
    (should-not (seq-some (lambda (text) (string-match-p "the prompt is held" text))
                          agent-repl-test-input--messages))))

(ert-deftest agent-repl-input-submit-reuses-a-supplied-key ()
  "A supplied key rides the wire instead of a freshly minted one."
  (agent-repl-test-input--with
    (agent-repl--input-submit "ws-1" (list :content (list :blocks nil))
                              :deferred-prompt "hello" "key-failed")
    (should (equal (plist-get (car agent-repl-test-input--submitted) :idempotency-key)
                   "key-failed"))))

(ert-deftest agent-repl-input-submit-mints-a-key-when-none-is-supplied ()
  "With no key supplied the composer mints one, as a first attempt must."
  (agent-repl-test-input--with
    (agent-repl--input-submit "ws-1" (list :content (list :blocks nil))
                              :user-sent "hello")
    (should (stringp (plist-get (car agent-repl-test-input--submitted)
                                :idempotency-key)))))

(ert-deftest agent-repl-input-no-connection-writes-the-held-prompt-ingress ()
  "No connection at all is the same fact as a transport failure."
  (agent-repl-test-input--with
    (cl-letf (((symbol-function 'agent-repl-host-conn) (lambda (_ws) nil))
              ((symbol-function 'agent-repl-link-primary) (lambda () nil)))
      (agent-repl-test-input--type "hello")
      (agent-repl--send :user-sent)
      (should-not agent-repl-test-input--submitted)
      (should (equal (length agent-repl-test-input--queued) 1)))))

;;;; ---- Attachments ----

(ert-deftest agent-repl-input-attach-records-path-and-media-type ()
  "The attach entry point records the reference and its MIME type."
  (agent-repl-test-input--with
    (with-current-buffer agent-repl-test-input--buffer
      (agent-repl-input-attach-image "/tmp/a.png" "image/png")
      (should (equal agent-repl-input-attachments
                     '((:path "/tmp/a.png" :media-type "image/png")))))))

(ert-deftest agent-repl-input-attach-attributes-the-record-to-the-composer-owner ()
  "Attachment diagnostics carry the composer buffer's workspace explicitly."
  ;; Arrange.
  (agent-repl-test-input--with
    (let (logged-workspace)
      (cl-letf (((symbol-function 'agent-repl--info)
                 (lambda (ws &rest _args) (setq logged-workspace ws))))
        ;; Act.
        (with-current-buffer agent-repl-test-input--buffer
          (agent-repl-input-attach-image "/tmp/a.png" "image/png"))
        ;; Assert.
        (should (equal logged-workspace "ws-one"))))))

(ert-deftest agent-repl-input-attach-refuses-outside-a-composer ()
  "Attaching is a composer act: there is no other buffer it could mean."
  (agent-repl-test-input--with
    (with-temp-buffer
      (should-error (agent-repl-input-attach-image "/tmp/a.png" "image/png")))))

(ert-deftest agent-repl-input-attachments-keep-attach-order ()
  "Blocks travel in the order the person composed them."
  (agent-repl-test-input--with
    (with-current-buffer agent-repl-test-input--buffer
      (agent-repl-input-attach-image "/tmp/a.png" "image/png")
      (agent-repl-input-attach-image "/tmp/b.png" "image/png"))
    (should (equal (mapcar (lambda (a) (plist-get a :path))
                           (agent-repl-input-attachments "ws-one"))
                   '("/tmp/a.png" "/tmp/b.png")))))

(ert-deftest agent-repl-input-attachment-becomes-an-image-block ()
  "An attachment travels as ImageBlock{path, media_type}, stated by ARM."
  (agent-repl-test-input--with
    (with-current-buffer agent-repl-test-input--buffer
      (agent-repl-input-attach-image "/tmp/a.png" "image/png"))
    (agent-repl-test-input--type "look")
    (agent-repl--send :user-sent)
    (should (equal (agent-repl-test-input--blocks)
                   (list (list :arm :text :value (list :text "look"))
                         (list :arm :image
                               :value (list :location
                                            (list :arm :path
                                                  :value (list :path "/tmp/a.png"))
                                            :media-type "image/png")))))))

(ert-deftest agent-repl-input-an-image-marker-never-rides-the-text-block ()
  "The composer's attachment marker is DRAWN, never submitted as words."
  (agent-repl-test-input--with
    ;; Arrange: the words, then the marker clipboard-image.el draws.
    (with-current-buffer agent-repl-test-input--buffer
      (agent-repl-input-attach-image "/tmp/a.png" "image/png")
      (erase-buffer)
      (insert "what is in this picture?")
      (agent-repl--image-insert-marker "/tmp/a.png" "ws-one"))
    ;; Act.
    (agent-repl--send :user-sent)
    ;; Assert.
    (should (equal (plist-get (plist-get (car (agent-repl-test-input--blocks)) :value) :text)
                   "what is in this picture?"))))

(ert-deftest agent-repl-input-history-records-the-stripped-text ()
  "The ring is the record of what was SENT, so it holds no marker either."
  (agent-repl-test-input--with
    ;; Arrange.
    (let (pushed)
      (cl-letf (((symbol-function 'agent-repl--history-push)
                 (lambda (&optional text) (push text pushed))))
        (with-current-buffer agent-repl-test-input--buffer
          (agent-repl-input-attach-image "/tmp/a.png" "image/png")
          (erase-buffer)
          (insert "remember only my words")
          (agent-repl--image-insert-marker "/tmp/a.png" "ws-one"))
        ;; Act.
        (agent-repl--send :user-sent)
        ;; Assert.
        (should (equal pushed (list "remember only my words")))))))

(ert-deftest agent-repl-input-attachments-cleared-after-a-turn ()
  "Attachments clear once the daemon has accepted the submission."
  (agent-repl-test-input--with
    (with-current-buffer agent-repl-test-input--buffer
      (agent-repl-input-attach-image "/tmp/a.png" "image/png"))
    (agent-repl-test-input--type "look")
    (agent-repl--send :user-sent)
    (should-not (agent-repl-input-attachments "ws-one"))))

(ert-deftest agent-repl-input-a-from-buffer-refusal-restores-attachments ()
  "A REFUSAL puts the attachments back: the submission never landed.
The optimistic clear drops them at dispatch because an accepted submission
carried them, but a refused one carried nothing -- an attachment left
cleared would be silently lost user intent."
  ;; Arrange.
  (agent-repl-test-input--with
    (setq agent-repl-test-input--answer
          '(:response (:arm :error :value (:reason (:arm :merging :value nil)))))
    (with-current-buffer agent-repl-test-input--buffer
      (agent-repl-input-attach-image "/tmp/a.png" "image/png"))
    (agent-repl-test-input--type "look")
    ;; Act.
    (agent-repl--send :user-sent)
    ;; Assert.
    (should (equal (agent-repl-input-attachments "ws-one")
                   (list (list :path "/tmp/a.png" :media-type "image/png"))))))

(ert-deftest agent-repl-input-image-only-submission-carries-no-text-block ()
  "An image alone is a legitimate thing to say; an empty TextBlock is not."
  (agent-repl-test-input--with
    (with-current-buffer agent-repl-test-input--buffer
      (agent-repl-input-attach-image "/tmp/a.png" "image/png"))
    (agent-repl-test-input--type "")
    (agent-repl--send :user-sent)
    (should (equal (length (agent-repl-test-input--blocks)) 1))
    (should (eq (plist-get (car (agent-repl-test-input--blocks)) :arm) :image))))

(ert-deftest agent-repl-input-discard-drops-attachments ()
  "Discarding the composer discards what was attached to it."
  (agent-repl-test-input--with
    (with-current-buffer agent-repl-test-input--buffer
      (agent-repl-input-attach-image "/tmp/a.png" "image/png")
      (insert "hello")
      (agent-repl-discard-input)
      (should-not agent-repl-input-attachments))))

;;;; ---- Input preparation ----

(ert-deftest agent-repl-input-wor-command-gets-a-source-tag ()
  "A /wor command is tagged with the workspace that initiated it."
  (agent-repl-test-input--with
    (should (string-suffix-p " [source-ws:ws-one path:/tmp/agent-repl-test/ws-1]"
                             (agent-repl--prepare-input "ws-one" "/workspace create")))))

(ert-deftest agent-repl-input-non-wor-command-gets-no-tag ()
  "An ordinary prompt is not tagged."
  (agent-repl-test-input--with
    (should (equal (agent-repl--prepare-input "ws-one" "hello") "hello"))))

(ert-deftest agent-repl-input-skip-metaprompt-for-a-bare-numeral ()
  "A bare numeral is an answer to a prompt, not free-form work."
  (should (agent-repl--skip-metaprompt-p "2")))

(ert-deftest agent-repl-input-skip-metaprompt-for-an-exempt-string ()
  "An explicitly exempt input never carries the directive."
  (should (agent-repl--skip-metaprompt-p "/login")))

(ert-deftest agent-repl-input-does-not-skip-metaprompt-for-prose ()
  "Ordinary prose is exactly what the directive is for."
  (should-not (agent-repl--skip-metaprompt-p "please refactor this")))

(ert-deftest agent-repl-input-slash-command-p-rejects-a-path ()
  "A Unix path merely starting with `/' is not a slash command."
  (should-not (agent-repl--slash-command-p "/Users/foo/bar")))

(ert-deftest agent-repl-input-slash-command-p-accepts-a-bare-command ()
  "A lone `/name' is a slash command."
  (should (agent-repl--slash-command-p "/clear")))


;;;; ---- The flash dwell -------------------------------------------------
;;
;; `agent-repl--input-flash' arms a timer that clears the notice after a
;; dwell.  The timer fires seconds later, long after the flash's own
;; scenario, so what it is allowed to clear is the whole contract:
;; `agent-repl--input-expire-flash' is called directly here rather than
;; through a real timer, which is what makes these tests deterministic.

(ert-deftest agent-repl-input-expire-flash-clears-its-own-text ()
  "The dwell clears the notice it flashed."
  ;; Arrange.
  (let ((buffer (generate-new-buffer " *agent-repl-test-input-flash*")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local agent-repl-input-notice "refused: merge in flight"))
          ;; Act.
          (cl-letf (((symbol-function 'agent-repl--log) #'ignore))
            (agent-repl--input-expire-flash "ws" buffer "refused: merge in flight"))
          ;; Assert.
          (should (null (buffer-local-value 'agent-repl-input-notice buffer))))
      (kill-buffer buffer))))

(ert-deftest agent-repl-input-expire-flash-leaves-a-different-notice-standing ()
  "The dwell NEVER clears a notice it did not set.
A refusal flashed within the dwell of a parked send would otherwise erase
the merge-parked badge -- the one line telling the user their prompts go
to the resolution agent."
  ;; Arrange.
  (let ((buffer (generate-new-buffer " *agent-repl-test-input-flash*")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local agent-repl-input-notice agent-repl--input-merge-parked-badge))
          ;; Act.
          (cl-letf (((symbol-function 'agent-repl--log) #'ignore))
            (agent-repl--input-expire-flash "ws" buffer "refused: merge in flight"))
          ;; Assert.
          (should (equal (buffer-local-value 'agent-repl-input-notice buffer)
                         agent-repl--input-merge-parked-badge)))
      (kill-buffer buffer))))

(ert-deftest agent-repl-input-expire-flash-tolerates-a-dead-buffer ()
  "A composer killed before the dwell elapses is not an error."
  ;; Arrange.
  (let ((buffer (generate-new-buffer " *agent-repl-test-input-flash*")))
    (kill-buffer buffer)
    ;; Act / Assert.
    (cl-letf (((symbol-function 'agent-repl--log) #'ignore))
      (should-not (agent-repl--input-expire-flash
                   "ws" buffer "refused: merge in flight")))))

(ert-deftest agent-repl-input-flash-arms-the-dwell-on-the-buffer-it-wrote ()
  "The dwell is armed with the buffer and text of THIS flash.
Resolving the composer by NAME when the timer fires is what let one
workspace's dwell clear another composer's badge."
  ;; Arrange.
  (agent-repl-test-input--with
    (let ((armed nil)
          (registered nil))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (_secs _repeat fn &rest args)
                   (setq armed (cons fn args))
                   'fake-timer))
                ((symbol-function 'agent-repl--register-timer)
                 (lambda (key timer)
                   (setq registered (list key timer))
                   timer)))
        ;; Act.
        (agent-repl--input-flash "ws-one" "refused: merge in flight"))
      ;; Assert.
      (should (eq (car armed) #'agent-repl--input-expire-flash))
      (should (equal (cdr armed)
                     (list "ws-one"
                           agent-repl-test-input--buffer
                           "refused: merge in flight")))
      (should (equal (cadr registered) 'fake-timer)))))

(ert-deftest agent-repl-input-mode-cancels-its-flash-timer-on-kill ()
  "Killing a composer prevents its delayed diagnostic from outliving the sink."
  ;; Arrange.
  (let ((buffer (generate-new-buffer " *agent-repl-test-flash-cleanup*"))
        (cancelled nil))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-repl--cancel-timer-key)
                   (lambda (key) (setq cancelled key))))
          (with-current-buffer buffer
            (agent-repl-input-mode)
            (setq-local agent-repl--input-flash-timer-key 'composer-flash))
          ;; Act.
          (kill-buffer buffer)
          ;; Assert.
          (should (eq cancelled 'composer-flash)))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

;;;; ---- The send edge is durable ----

;; A user's send is a once-per-action edge, so its record has to survive the
;; deployment's `info' threshold.  On the debug rung the send that produced a
;; turn could not be read back at all afterwards.

(defvar agent-repl-test-input--info nil
  "Messages the stubbed `agent-repl--info' rung received, newest first.")

(defvar agent-repl-test-input--debug nil
  "Messages the stubbed `agent-repl--log' (debug) rung received, newest first.")

(defvar agent-repl-test-input--warn nil
  "Messages the stubbed `agent-repl--warn' rung received, newest first.")

(defvar agent-repl-test-input--error nil
  "Messages the stubbed `agent-repl--error' rung received, newest first.")

(defmacro agent-repl-test-input--capturing-rungs (&rest body)
  "Run BODY with each logging rung captured separately.
The `error' and `warn' rungs are captured alongside `info' and debug
because a refusal the composer HAS a treatment for must be recorded at
WARN and must never reach the `unknown-error-arm' ERROR branch."
  (declare (indent 0))
  `(let ((agent-repl-test-input--info nil)
         (agent-repl-test-input--debug nil)
         (agent-repl-test-input--warn nil)
         (agent-repl-test-input--error nil))
     (cl-letf (((symbol-function 'agent-repl--info)
                (lambda (_ws fmt &rest args)
                  (push (apply #'format fmt args) agent-repl-test-input--info)))
               ((symbol-function 'agent-repl--log)
                (lambda (_ws fmt &rest args)
                  (push (apply #'format fmt args) agent-repl-test-input--debug)))
               ((symbol-function 'agent-repl--warn)
                (lambda (_ws fmt &rest args)
                  (push (apply #'format fmt args) agent-repl-test-input--warn)))
               ((symbol-function 'agent-repl--error)
                (lambda (_ws fmt &rest args)
                  (push (apply #'format fmt args) agent-repl-test-input--error))))
       ,@body)))

(defun agent-repl-test-input--rung-has-p (messages prefix)
  "Return non-nil when some message in MESSAGES starts with PREFIX."
  (and (cl-find-if (lambda (m) (string-prefix-p prefix m)) messages) t))

(ert-deftest agent-repl-input-send-records-the-edge-at-info ()
  "The send a person made is recorded on the `info' rung."
  ;; Arrange.
  (agent-repl-test-input--with
    (agent-repl-test-input--type "hello")
    ;; Act.
    (agent-repl-test-input--capturing-rungs
      (agent-repl--send :user-sent)
      ;; Assert.
      (should (agent-repl-test-input--rung-has-p
               agent-repl-test-input--info "elisp.input.send ws=ws-one")))))

(ert-deftest agent-repl-input-send-is-never-recorded-on-the-debug-rung ()
  "The send record must not sit below the durable threshold."
  ;; Arrange.
  (agent-repl-test-input--with
    (agent-repl-test-input--type "hello")
    ;; Act.
    (agent-repl-test-input--capturing-rungs
      (agent-repl--send :user-sent)
      ;; Assert.
      (should-not (agent-repl-test-input--rung-has-p
                   agent-repl-test-input--debug "elisp.input.send ws=")))))

(ert-deftest agent-repl-input-send-empty-records-the-edge-at-info ()
  "A send with nothing to say is still an action the person took."
  ;; Arrange.
  (agent-repl-test-input--with
    ;; Act.
    (agent-repl-test-input--capturing-rungs
      (agent-repl--send :user-sent)
      ;; Assert.
      (should (agent-repl-test-input--rung-has-p
               agent-repl-test-input--info "elisp.input.send-empty ws=ws-one")))))

(ert-deftest agent-repl-input-send-empty-is-never-recorded-on-the-debug-rung ()
  "The empty-send record must not sit below the durable threshold."
  ;; Arrange.
  (agent-repl-test-input--with
    ;; Act.
    (agent-repl-test-input--capturing-rungs
      (agent-repl--send :user-sent)
      ;; Assert.
      (should-not (agent-repl-test-input--rung-has-p
                   agent-repl-test-input--debug "elisp.input.send-empty ws=")))))

;;;; ---- The cold gate, and the session that is simply absent ----

;; Owner's report, 2026-09-14: three prompts to a workspace parked at its cold
;; gate were answered `no_session' and fell through this composer's catch-all,
;; which recorded `elisp.input.unknown-error-arm' at ERROR and echoed
;; "submission refused (:no-session)".  Both arms are ANSWERS with a place to
;; go -- the panel, and the daemon's own bring-up -- so both are handled here,
;; at WARN, without queueing and without costing the user their text.

(defconst agent-repl-test-input--cold-gate-detail
  "context cold -- 412k tokens would be re-read"
  "A cold gate's own account, as the daemon states it.")

(defun agent-repl-test-input--cold-gate-answer (detail)
  "Return a scripted `cold_gate' refusal carrying DETAIL."
  (list :response (list :arm :error
                        :value (list :reason (list :arm :cold-gate
                                                   :value (list :detail detail))))))

(defun agent-repl-test-input--model-refusal-answer (arm detail)
  "Return a scripted refused-`/model' answer under ARM carrying DETAIL."
  (list :response (list :arm :error
                        :value (list :reason (list :arm arm
                                                   :value (list :detail detail))))))

(ert-deftest agent-repl-input-model-refusal-shows-the-refusals-own-sentence ()
  "A refused `/model' act says why, in the refusal's own words."
  (agent-repl-test-input--with
    ;; Arrange
    (setq agent-repl-test-input--answer
          (agent-repl-test-input--model-refusal-answer
           :model-not-in-catalog "\"opus\" is not in this session's model catalog"))
    (agent-repl-test-input--type "/model opus")
    ;; Act
    (agent-repl--send :user-sent)
    ;; Assert
    (should (seq-some (lambda (text)
                        (string-match-p "model change refused -- \"opus\" is not in this session's model catalog"
                                        text))
                      agent-repl-test-input--messages))))

(ert-deftest agent-repl-input-model-refusal-is-not-queued-as-an-outage ()
  "A refused `/model' act is an answer: nothing is held for a re-drive."
  (agent-repl-test-input--with
    ;; Arrange
    (setq agent-repl-test-input--answer
          (agent-repl-test-input--model-refusal-answer :model-refused "refused"))
    (agent-repl-test-input--type "/model opus")
    ;; Act
    (agent-repl--send :user-sent)
    ;; Assert
    (should-not agent-repl-test-input--queued)))

(ert-deftest agent-repl-input-model-refusal-never-reaches-the-unknown-error-arm ()
  "Both model arms are HANDLED, so the catch-all's ERROR record must not fire."
  (dolist (arm '(:model-not-in-catalog :model-refused))
    (agent-repl-test-input--with
      ;; Arrange
      (setq agent-repl-test-input--answer
            (agent-repl-test-input--model-refusal-answer arm "refused"))
      (agent-repl-test-input--type "/model opus")
      ;; Act
      (agent-repl-test-input--capturing-rungs
        (agent-repl--send :user-sent)
        ;; Assert
        (should-not (agent-repl-test-input--rung-has-p
                     agent-repl-test-input--error
                     "elisp.input.unknown-error-arm"))))))

(defconst agent-repl-test-input--no-session-answer
  '(:response (:arm :error :value (:reason (:arm :no-session :value nil))))
  "A scripted `no_session' refusal: a workspace with no session at all.")

(ert-deftest agent-repl-input-cold-gate-flashes-the-panel-sentence ()
  "The gate is answered in the panel, so the flash names the panel."
  (agent-repl-test-input--with
    ;; Arrange
    (setq agent-repl-test-input--answer
          (agent-repl-test-input--cold-gate-answer
           agent-repl-test-input--cold-gate-detail))
    (agent-repl-test-input--type "hello")
    ;; Act
    (agent-repl--send :user-sent)
    ;; Assert
    (should (equal (buffer-local-value 'agent-repl-input-notice
                                       agent-repl-test-input--buffer)
                   "cold gate: answer it in the panel (clear / compact / resume)"))))

(ert-deftest agent-repl-input-cold-gate-echoes-the-daemons-detail ()
  "The gate's own account rides into the echo area verbatim."
  (agent-repl-test-input--with
    ;; Arrange
    (setq agent-repl-test-input--answer
          (agent-repl-test-input--cold-gate-answer
           agent-repl-test-input--cold-gate-detail))
    (agent-repl-test-input--type "hello")
    ;; Act
    (agent-repl--send :user-sent)
    ;; Assert
    (should (seq-some (lambda (text)
                        (string-match-p
                         (regexp-quote
                          (format "(%s)" agent-repl-test-input--cold-gate-detail))
                         text))
                      agent-repl-test-input--messages))))

(ert-deftest agent-repl-input-cold-gate-omits-an-empty-detail ()
  "An empty detail leaves the flash sentence alone, with no dangling parens."
  (agent-repl-test-input--with
    ;; Arrange
    (setq agent-repl-test-input--answer (agent-repl-test-input--cold-gate-answer ""))
    (agent-repl-test-input--type "hello")
    ;; Act
    (agent-repl--send :user-sent)
    ;; Assert
    (should-not (seq-some (lambda (text) (string-match-p "()" text))
                          agent-repl-test-input--messages))))

(ert-deftest agent-repl-input-cold-gate-records-at-warn ()
  "A standing the user answers is a WARN, under its own operation token."
  (agent-repl-test-input--with
    ;; Arrange
    (setq agent-repl-test-input--answer
          (agent-repl-test-input--cold-gate-answer
           agent-repl-test-input--cold-gate-detail))
    (agent-repl-test-input--type "hello")
    ;; Act
    (agent-repl-test-input--capturing-rungs
      (agent-repl--send :user-sent)
      ;; Assert
      (should (agent-repl-test-input--rung-has-p
               agent-repl-test-input--warn
               "elisp.input.refused-cold-gate ws=ws-one")))))

(ert-deftest agent-repl-input-cold-gate-never-reaches-the-unknown-error-arm ()
  "The arm is HANDLED, so the catch-all's ERROR record must not fire."
  (agent-repl-test-input--with
    ;; Arrange
    (setq agent-repl-test-input--answer
          (agent-repl-test-input--cold-gate-answer
           agent-repl-test-input--cold-gate-detail))
    (agent-repl-test-input--type "hello")
    ;; Act
    (agent-repl-test-input--capturing-rungs
      (agent-repl--send :user-sent)
      ;; Assert
      (should-not (agent-repl-test-input--rung-has-p
                   agent-repl-test-input--error
                   "elisp.input.unknown-error-arm")))))

(ert-deftest agent-repl-input-cold-gate-does-not-queue-the-prompt ()
  "A re-drive would meet the same gate, so nothing is held."
  (agent-repl-test-input--with
    ;; Arrange
    (setq agent-repl-test-input--answer
          (agent-repl-test-input--cold-gate-answer
           agent-repl-test-input--cold-gate-detail))
    (agent-repl-test-input--type "hello")
    ;; Act
    (agent-repl--send :user-sent)
    ;; Assert
    (should-not agent-repl-test-input--queued)))

(ert-deftest agent-repl-input-cold-gate-restores-the-composer ()
  "A `cold_gate\\=' refusal restores the composer text.
The gate is answered in the panel and the prompt is resubmitted after, so
the words stay in front of the user rather than only in the ring."
  ;; Arrange.
  (agent-repl-test-input--with
    (setq agent-repl-test-input--answer
          (agent-repl-test-input--cold-gate-answer
           agent-repl-test-input--cold-gate-detail))
    (agent-repl-test-input--type "hello")
    ;; Act.
    (agent-repl--send :user-sent)
    ;; Assert.
    (should (equal (agent-repl-test-input--composer-text) "hello"))))

(ert-deftest agent-repl-input-no-session-flashes-the-bring-up-sentence ()
  "The daemon owns the bring-up, so the flash says so rather than refusing."
  (agent-repl-test-input--with
    ;; Arrange
    (setq agent-repl-test-input--answer agent-repl-test-input--no-session-answer)
    (agent-repl-test-input--type "hello")
    ;; Act
    (agent-repl--send :user-sent)
    ;; Assert
    (should (equal (buffer-local-value 'agent-repl-input-notice
                                       agent-repl-test-input--buffer)
                   "no session; the daemon is starting it"))))

(ert-deftest agent-repl-input-no-session-echoes-the-sentence-plainly ()
  "The echo says the same thing the flash does, with nothing appended."
  (agent-repl-test-input--with
    ;; Arrange
    (setq agent-repl-test-input--answer agent-repl-test-input--no-session-answer)
    (agent-repl-test-input--type "hello")
    ;; Act
    (agent-repl--send :user-sent)
    ;; Assert
    (should (member "agent-repl: no session; the daemon is starting it"
                    agent-repl-test-input--messages))))

(ert-deftest agent-repl-input-no-session-records-at-warn ()
  "A session the daemon is bringing up is a WARN, under its own token."
  (agent-repl-test-input--with
    ;; Arrange
    (setq agent-repl-test-input--answer agent-repl-test-input--no-session-answer)
    (agent-repl-test-input--type "hello")
    ;; Act
    (agent-repl-test-input--capturing-rungs
      (agent-repl--send :user-sent)
      ;; Assert
      (should (agent-repl-test-input--rung-has-p
               agent-repl-test-input--warn
               "elisp.input.refused-no-session ws=ws-one")))))

(ert-deftest agent-repl-input-no-session-never-reaches-the-unknown-error-arm ()
  "The owner's report was exactly this ERROR record; it must not fire."
  (agent-repl-test-input--with
    ;; Arrange
    (setq agent-repl-test-input--answer agent-repl-test-input--no-session-answer)
    (agent-repl-test-input--type "hello")
    ;; Act
    (agent-repl-test-input--capturing-rungs
      (agent-repl--send :user-sent)
      ;; Assert
      (should-not (agent-repl-test-input--rung-has-p
                   agent-repl-test-input--error
                   "elisp.input.unknown-error-arm")))))

(ert-deftest agent-repl-input-no-session-does-not-queue-the-prompt ()
  "It is an ANSWER, not an outage, so the hold queue is not involved."
  (agent-repl-test-input--with
    ;; Arrange
    (setq agent-repl-test-input--answer agent-repl-test-input--no-session-answer)
    (agent-repl-test-input--type "hello")
    ;; Act
    (agent-repl--send :user-sent)
    ;; Assert
    (should-not agent-repl-test-input--queued)))

;;;; ---- Keybindings -----------------------------------------------------

(ert-deftest agent-repl-test-input-ck-interrupts-the-turn ()
  "`C-c C-k' in the composer is bound to the turn interrupt.
Bound with `define-key' (not the `map!' form, a no-op under `emacs -Q'),
so the binding itself is observable here -- restored on the very chord that
used to interrupt before the overhaul."
  (should (eq (lookup-key agent-repl-input-mode-map (kbd "C-c C-k"))
              #'agent-repl-interrupt-turn)))

(ert-deftest agent-repl-test-input-interrupt-turn-is-a-command ()
  "The chord's target is a real interactive command, not a dead symbol."
  (should (commandp #'agent-repl-interrupt-turn)))

;;;; ---- Feed text zoom (C-+ / C--) --------------------------------------

(ert-deftest agent-repl-test-input-feed-text-scale-increase-is-a-command ()
  "The zoom-in target is a real interactive command (so auto-repeat works)."
  (should (commandp #'agent-repl-feed-text-scale-increase)))

(ert-deftest agent-repl-test-input-feed-text-scale-decrease-is-a-command ()
  "The zoom-out target is a real interactive command (so auto-repeat works)."
  (should (commandp #'agent-repl-feed-text-scale-decrease)))

(ert-deftest agent-repl-test-input-c-plus-zooms-feed-in ()
  "`C-+' in the composer is bound to the feed zoom-in command.
Bound with `define-key' (not the `map!' no-op under `emacs -Q'), so the
override of Doom's global text-scale binding is observable here."
  (should (eq (lookup-key agent-repl-input-mode-map (kbd "C-+"))
              #'agent-repl-feed-text-scale-increase)))

(ert-deftest agent-repl-test-input-c-minus-zooms-feed-out ()
  "`C--' in the composer is bound to the feed zoom-out command."
  (should (eq (lookup-key agent-repl-input-mode-map (kbd "C--"))
              #'agent-repl-feed-text-scale-decrease)))

(ert-deftest agent-repl-test-input-feed-text-scale-increase-sends-increase ()
  "Zoom-in asks the daemon for one INCREASE step."
  (agent-repl-test-input--with
    ;; Arrange
    (let (requests)
      (cl-letf (((symbol-function 'agent-repl-rpc-adjust-feed-text-scale)
                 (lambda (_conn request &rest keys)
                   (push request requests)
                   (funcall (plist-get keys :on-response) '(:scale 1.02)))))
        ;; Act
        (agent-repl-feed-text-scale-increase))
      ;; Assert
      (should (eq (plist-get (car requests) :direction) :increase)))))

(ert-deftest agent-repl-test-input-feed-text-scale-decrease-sends-decrease ()
  "Zoom-out asks the daemon for one DECREASE step."
  (agent-repl-test-input--with
    ;; Arrange
    (let (requests)
      (cl-letf (((symbol-function 'agent-repl-rpc-adjust-feed-text-scale)
                 (lambda (_conn request &rest keys)
                   (push request requests)
                   (funcall (plist-get keys :on-response) '(:scale 0.98)))))
        ;; Act
        (agent-repl-feed-text-scale-decrease))
      ;; Assert
      (should (eq (plist-get (car requests) :direction) :decrease)))))

;;;; ---- Response selection (reply to a past response) -------------------

(defun agent-repl-test-input--selection ()
  "Return the composer's buffer-local reply-to-a-past-response selection."
  (buffer-local-value 'agent-repl--input-response-selection
                      agent-repl-test-input--buffer))

(defun agent-repl-test-input--set-selection (feedid)
  "Force FEEDID as the composer's active reply target."
  (with-current-buffer agent-repl-test-input--buffer
    (setq agent-repl--input-response-selection feedid)))

(ert-deftest agent-repl-test-input-response-select-prev-is-a-command ()
  "The `C-p' nav target is a real interactive command."
  (should (commandp #'agent-repl-response-select-prev)))

(ert-deftest agent-repl-test-input-response-select-next-is-a-command ()
  "The `C-n' nav target is a real interactive command."
  (should (commandp #'agent-repl-response-select-next)))

(ert-deftest agent-repl-test-input-response-selection-escape-is-a-command ()
  "The command-mode escape target is a real interactive command."
  (should (commandp #'agent-repl-input-response-selection-escape)))

(ert-deftest agent-repl-test-input-response-select-prev-sends-prev ()
  "`C-p' asks the daemon for the PREVIOUS final-response row."
  (agent-repl-test-input--with
    ;; Arrange
    (let (requests)
      (cl-letf (((symbol-function 'agent-repl-rpc-select-response)
                 (lambda (_conn request &rest keys)
                   (push request requests)
                   (funcall (plist-get keys :on-response)
                            '(:arm :success :value (:selected (:value "feed-9")))))))
        ;; Act
        (agent-repl-response-select-prev))
      ;; Assert
      (should (eq (plist-get (car requests) :direction) :prev)))))

(ert-deftest agent-repl-test-input-response-select-next-sends-next ()
  "`C-n' asks the daemon for the NEXT final-response row."
  (agent-repl-test-input--with
    ;; Arrange
    (let (requests)
      (cl-letf (((symbol-function 'agent-repl-rpc-select-response)
                 (lambda (_conn request &rest keys)
                   (push request requests)
                   (funcall (plist-get keys :on-response)
                            '(:arm :success :value (:selected (:value "feed-9")))))))
        ;; Act
        (agent-repl-response-select-next))
      ;; Assert
      (should (eq (plist-get (car requests) :direction) :next)))))

(ert-deftest agent-repl-test-input-response-select-records-the-acked-feedid ()
  "The daemon's acked selected feedid becomes the buffer-local selection."
  (agent-repl-test-input--with
    ;; Arrange
    (cl-letf (((symbol-function 'agent-repl-rpc-select-response)
               (lambda (_conn _request &rest keys)
                 (funcall (plist-get keys :on-response)
                          '(:arm :success :value (:selected (:value "feed-9")))))))
      ;; Act
      (agent-repl-response-select-prev))
    ;; Assert
    (should (equal (agent-repl-test-input--selection) '(:value "feed-9")))))

(ert-deftest agent-repl-test-input-response-select-none-leaves-no-selection ()
  "A nav that finds no selectable row stores NONE, so nothing is active."
  (agent-repl-test-input--with
    ;; Arrange
    (cl-letf (((symbol-function 'agent-repl-rpc-select-response)
               (lambda (_conn _request &rest keys)
                 (funcall (plist-get keys :on-response)
                          '(:arm :success :value (:selected nil))))))
      ;; Act
      (agent-repl-response-select-next))
    ;; Assert
    (should-not (agent-repl-test-input--selection))))

(ert-deftest agent-repl-test-input-response-select-refusal-keeps-prior-selection ()
  "A refused nav never clobbers the selection already held.
Error-surfacing coverage is not dropped just because the daemon is the
authority: a refusal leaves the prior reply target intact."
  (agent-repl-test-input--with
    ;; Arrange
    (agent-repl-test-input--set-selection '(:value "feed-1"))
    (cl-letf (((symbol-function 'agent-repl-rpc-select-response)
               (lambda (_conn _request &rest keys)
                 (funcall (plist-get keys :on-response)
                          '(:arm :error
                            :value (:cause (:arm :unknown-workspace :value nil)))))))
      ;; Act
      (agent-repl-response-select-prev))
    ;; Assert
    (should (equal (agent-repl-test-input--selection) '(:value "feed-1")))))

(ert-deftest agent-repl-test-input-submit-carries-the-selected-feedid ()
  "With a selection active, the submit rides it as the reply target."
  (agent-repl-test-input--with
    ;; Arrange
    (agent-repl-test-input--set-selection '(:value "feed-9"))
    (agent-repl-test-input--type "reply text")
    ;; Act
    (agent-repl--send :user-sent)
    ;; Assert
    (should (equal (plist-get (agent-repl-test-input--request)
                              :reference-response-feedid)
                   '(:value "feed-9")))))

(ert-deftest agent-repl-test-input-submit-omits-reference-without-selection ()
  "An ordinary prompt carries no reply target: absence is the fact."
  (agent-repl-test-input--with
    ;; Arrange
    (agent-repl-test-input--type "plain")
    ;; Act
    (agent-repl--send :user-sent)
    ;; Assert
    (should-not (plist-member (agent-repl-test-input--request)
                             :reference-response-feedid))))

(ert-deftest agent-repl-test-input-successful-submit-clears-the-selection ()
  "An accepted submit drops the reply target so the next prompt is ordinary."
  (agent-repl-test-input--with
    ;; Arrange
    (agent-repl-test-input--set-selection '(:value "feed-9"))
    (agent-repl-test-input--type "reply")
    ;; Act (harness answers with a minted turn)
    (agent-repl--send :user-sent)
    ;; Assert
    (should-not (agent-repl-test-input--selection))))

(ert-deftest agent-repl-test-input-escape-without-selection-delegates ()
  "Escape keeps its ordinary meaning when nothing is selected."
  (agent-repl-test-input--with
    ;; Arrange
    (let ((defaulted 0) (selects 0))
      (cl-letf (((symbol-function 'agent-repl--input-escape-default)
                 (lambda () (cl-incf defaulted)))
                ((symbol-function 'agent-repl-rpc-select-response)
                 (lambda (&rest _) (cl-incf selects))))
        ;; Act
        (agent-repl-input-response-selection-escape))
      ;; Assert
      (should (= defaulted 1))
      (should (= selects 0)))))

(ert-deftest agent-repl-test-input-first-escape-warns-and-keeps-selection ()
  "The first command-mode escape only WARNS; the selection stays."
  (agent-repl-test-input--with
    ;; Arrange
    (agent-repl-test-input--set-selection '(:value "feed-9"))
    (let (selects)
      (cl-letf (((symbol-function 'agent-repl-rpc-select-response)
                 (lambda (_conn request &rest _) (push request selects))))
        ;; Act -- the preceding command was a nav, not another escape.
        (let ((last-command 'agent-repl-response-select-prev))
          (agent-repl-input-response-selection-escape)))
      ;; Assert
      (should (null selects))
      (should (equal (agent-repl-test-input--selection) '(:value "feed-9")))
      (should (cl-some (lambda (m) (string-match-p "escape again" m))
                       agent-repl-test-input--messages)))))

(ert-deftest agent-repl-test-input-second-consecutive-escape-clears ()
  "Two consecutive escapes send CLEAR and drop the local selection."
  (agent-repl-test-input--with
    ;; Arrange
    (agent-repl-test-input--set-selection '(:value "feed-9"))
    (let (selects)
      (cl-letf (((symbol-function 'agent-repl-rpc-select-response)
                 (lambda (_conn request &rest keys)
                   (push request selects)
                   (funcall (plist-get keys :on-response)
                            '(:arm :success :value (:selected nil))))))
        ;; Act -- the preceding command was the escape itself.
        (let ((last-command 'agent-repl-input-response-selection-escape))
          (agent-repl-input-response-selection-escape)))
      ;; Assert
      (should (eq (plist-get (car selects) :direction) :clear))
      (should-not (agent-repl-test-input--selection)))))

(ert-deftest agent-repl-test-input-non-consecutive-escape-rearms ()
  "A command-mode key between two escapes resets the consecutive count.
The second escape is treated as a FIRST again -- it warns, never clears."
  (agent-repl-test-input--with
    ;; Arrange
    (agent-repl-test-input--set-selection '(:value "feed-9"))
    (let (selects)
      (cl-letf (((symbol-function 'agent-repl-rpc-select-response)
                 (lambda (_conn request &rest _) (push request selects))))
        ;; Act -- an intervening command ran as `last-command'.
        (let ((last-command 'agent-repl-response-select-next))
          (agent-repl-input-response-selection-escape)))
      ;; Assert
      (should (null selects))
      (should (equal (agent-repl-test-input--selection) '(:value "feed-9"))))))

(provide 'test-input)


;;;; ---- The refusal restore (owner ruling) ----
;;
;; The optimistic clear erases the composer at dispatch.  A REFUSAL says the
;; prompt did not land, so every trace of that clear is undone.  One edge
;; case per test.

(ert-deftest agent-repl-input-unknown-refusal-arm-restores-the-composer ()
  "An arm the composer has no treatment for still restores the text.
The restore is keyed on the response being an ERROR at all, not on which
arm it carries, so a refusal nobody anticipated cannot cost the user their
words."
  ;; Arrange.
  (agent-repl-test-input--with
    (setq agent-repl-test-input--answer
          '(:response (:arm :error :value (:reason (:arm :unknown-workspace :value nil)))))
    (agent-repl-test-input--type "hello")
    ;; Act.
    (agent-repl--send :user-sent)
    ;; Assert.
    (should (equal (agent-repl-test-input--composer-text) "hello"))))

(ert-deftest agent-repl-input-refusal-un-pushes-the-history-entry ()
  "A refused prompt is never left in the history ring.
The optimistic clear pushes RAW at dispatch; the restore puts the ring back
to exactly what it held before, so history-prev never recalls a prompt the
daemon rejected as though it had been delivered."
  ;; Arrange.
  (agent-repl-test-input--with
    (cl-letf (((symbol-function 'agent-repl--history-push)
               (lambda (&optional text) (push text agent-repl--input-history))))
      (setq agent-repl-test-input--answer
            '(:response (:arm :error :value (:reason (:arm :merging :value nil)))))
      (with-current-buffer agent-repl-test-input--buffer
        (setq agent-repl--input-history (list "sentinel-untouched")))
      (agent-repl-test-input--type "hello")
      ;; Act.
      (agent-repl--send :user-sent)
      ;; Assert.
      (should (equal (buffer-local-value 'agent-repl--input-history
                                         agent-repl-test-input--buffer)
                     (list "sentinel-untouched"))))))

(ert-deftest agent-repl-input-refusal-restores-point ()
  "The restore puts point back where the user had it, not at the buffer end."
  ;; Arrange.
  (agent-repl-test-input--with
    (setq agent-repl-test-input--answer
          '(:response (:arm :error :value (:reason (:arm :merging :value nil)))))
    (agent-repl-test-input--type "hello")
    (with-current-buffer agent-repl-test-input--buffer (goto-char 3))
    ;; Act.
    (agent-repl--send :user-sent)
    ;; Assert.
    (should (equal (with-current-buffer agent-repl-test-input--buffer (point)) 3))))

(ert-deftest agent-repl-input-refusal-of-a-canned-send-leaves-the-draft-alone ()
  "A refused CANNED send restores nothing: it never cleared anything.
A canned command composes its own text and leaves the user\\='s half-written
draft in place, so there is no snapshot and the draft must survive the
refusal untouched rather than being overwritten by the canned prompt."
  ;; Arrange.
  (agent-repl-test-input--with
    (setq agent-repl-test-input--answer
          '(:response (:arm :error :value (:reason (:arm :merging :value nil)))))
    (agent-repl-test-input--type "my draft")
    ;; Act.
    (agent-repl--send :command-rebase "canned text")
    ;; Assert.
    (should (equal (agent-repl-test-input--composer-text) "my draft"))))


;;;; ---- A held-prompt edit ----

(defmacro agent-repl-test-input--editing (&rest body)
  "Run BODY with the composer standing in a held-prompt edit.
The edit's own module is recorded rather than run: this suite pins only
that the send and the discard branch to it."
  (declare (indent 0))
  `(let ((commits nil) (cancels nil))
     (cl-letf (((symbol-function 'agent-repl-held-edit-active-p) (lambda (_ws) t))
               ((symbol-function 'agent-repl-held-edit-commit)
                (lambda (ws said snapshot) (push (list ws said snapshot) commits)))
               ((symbol-function 'agent-repl-held-edit-cancel)
                (lambda (ws) (push ws cancels))))
       ,@body)))

(ert-deftest agent-repl-input-an-edit-mode-send-commits-the-edit ()
  "While a held prompt is edited, the composer's send is the edit's commit."
  (agent-repl-test-input--with
    (agent-repl-test-input--editing
      ;; Arrange
      (agent-repl-test-input--type "the revision")
      ;; Act
      (agent-repl--send :user-sent)
      ;; Assert
      (should (equal (plist-get (plist-get (car (plist-get (plist-get (nth 1 (car commits)) :content) :blocks))
                                           :value)
                                :text)
                     "the revision")))))

(ert-deftest agent-repl-input-an-edit-mode-send-submits-no-new-prompt ()
  "The commit replaces the held prompt; nothing new is submitted."
  (agent-repl-test-input--with
    (agent-repl-test-input--editing
      ;; Arrange
      (agent-repl-test-input--type "the revision")
      ;; Act
      (agent-repl--send :user-sent)
      ;; Assert
      (should (null agent-repl-test-input--submitted)))))

(ert-deftest agent-repl-input-an-edit-mode-send-clears-the-composer ()
  "The commit clears and records exactly as a send does."
  (agent-repl-test-input--with
    (agent-repl-test-input--editing
      ;; Arrange
      (agent-repl-test-input--type "the revision")
      ;; Act
      (agent-repl--send :user-sent)
      ;; Assert
      (should (equal (agent-repl-test-input--composer-text) "")))))

(ert-deftest agent-repl-input-an-edit-mode-canned-send-still-submits ()
  "A caller-composed send is not the composer's, so it is not a commit."
  (agent-repl-test-input--with
    (agent-repl-test-input--editing
      ;; Act
      (agent-repl--send :command-rebase "rebase please")
      ;; Assert
      (should (and agent-repl-test-input--submitted (null commits))))))

(ert-deftest agent-repl-input-an-edit-mode-discard-cancels-the-edit ()
  "`C-c C-c' while a held prompt is edited cancels the edit."
  (agent-repl-test-input--with
    (agent-repl-test-input--editing
      ;; Arrange
      (agent-repl-test-input--type "the revision")
      ;; Act
      (with-current-buffer agent-repl-test-input--buffer
        (agent-repl-discard-input))
      ;; Assert
      (should (equal cancels '("ws-one"))))))

(ert-deftest agent-repl-input-a-discard-with-no-edit-cancels-nothing ()
  "An ordinary discard is only a discard."
  (agent-repl-test-input--with
    (let ((cancels nil))
      (cl-letf (((symbol-function 'agent-repl-held-edit-active-p) (lambda (_ws) nil))
                ((symbol-function 'agent-repl-held-edit-cancel) (lambda (ws) (push ws cancels))))
        ;; Act
        (with-current-buffer agent-repl-test-input--buffer
          (agent-repl-discard-input))
        ;; Assert
        (should (null cancels))))))

;;; test-input.el ends here
