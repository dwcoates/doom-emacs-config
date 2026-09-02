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
  "Prompts offered to the hold queue, as (WS SAID ORIGIN RAW KEY).")

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
         (agent-repl-test-input--refusals nil)
         (agent-repl-test-input--messages nil)
         (agent-repl-test-input--answer (agent-repl-test-input--turn-answer))
         (agent-repl-test-input--gate :open)
         (agent-repl-test-input--buffer nil))
     (unwind-protect
         (progn
           (setq agent-repl-test-input--buffer
                 (generate-new-buffer " *agent-repl-test-composer*"))
           (with-current-buffer agent-repl-test-input--buffer (agent-repl-input-mode))
           (cl-letf* (((symbol-function 'agent-repl--ws-current-name) (lambda () "ws-one"))
                      ((symbol-function 'agent-repl--ws-current-log-name) (lambda () "ws-one"))
                      ((symbol-function 'agent-repl--ws-get)
                       (lambda (_ws key)
                         (pcase key
                           (:input-buffer agent-repl-test-input--buffer)
                           (:project-dir "/tmp/agent-repl-test/ws-1")
                           (_ nil))))
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
                      ((symbol-function 'agent-repl-prompt-queue-offer)
                       (lambda (ws said origin raw &optional key)
                         (push (list ws said origin raw key)
                               agent-repl-test-input--queued)))
                      ((symbol-function 'run-at-time) (lambda (&rest _) nil))
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

(ert-deftest agent-repl-input-duplicate-submission-keeps-the-text ()
  "The key was already accepted, but nothing here landed: the text stays."
  (agent-repl-test-input--with
    (setq agent-repl-test-input--answer
          '(:response (:arm :error :value (:reason (:arm :duplicate-submission :value nil)))))
    (agent-repl-test-input--type "hello")
    (agent-repl--send :user-sent)
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

(ert-deftest agent-repl-input-merging-error-keeps-the-text ()
  "The merging refusal KEEPS the text: the user resubmits after the merge."
  (agent-repl-test-input--with
    (setq agent-repl-test-input--answer
          '(:response (:arm :error :value (:reason (:arm :merging :value nil)))))
    (agent-repl-test-input--type "hello")
    (agent-repl--send :user-sent)
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

(ert-deftest agent-repl-input-transferring-away-keeps-the-text ()
  "Nothing says the prompt landed, so the user keeps seeing what they wrote."
  (agent-repl-test-input--with
    ;; Arrange
    (setq agent-repl-test-input--answer
          '(:response (:arm :error :value (:reason (:arm :transferring-away
                                                   :value (:address "127.0.0.1:9100"))))))
    (agent-repl-test-input--type "hello")
    ;; Act
    (agent-repl--send :user-sent)
    ;; Assert
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
    (should (equal (nth 4 (car agent-repl-test-input--queued))
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

(ert-deftest agent-repl-input-not-yet-adopted-keeps-the-text ()
  "Nothing is wrong and nothing landed: the text stays in the composer."
  (agent-repl-test-input--with
    ;; Arrange
    (setq agent-repl-test-input--answer
          '(:response (:arm :error :value (:reason (:arm :not-yet-adopted :value nil)))))
    (agent-repl-test-input--type "hello")
    ;; Act
    (agent-repl--send :user-sent)
    ;; Assert
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

(ert-deftest agent-repl-input-no-session-error-keeps-the-text ()
  "A non-handover refusal keeps its current treatment: the text is kept."
  (agent-repl-test-input--with
    ;; Arrange
    (setq agent-repl-test-input--answer
          '(:response (:arm :error :value (:reason (:arm :no-session :value nil)))))
    (agent-repl-test-input--type "hello")
    ;; Act
    (agent-repl--send :user-sent)
    ;; Assert
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

(ert-deftest agent-repl-input-transport-failure-keeps-the-text ()
  "Nobody answered, so nothing is known: the composer keeps its text."
  (agent-repl-test-input--with
    (setq agent-repl-test-input--answer '(:failure (:kind :transport :message "gone")))
    (agent-repl-test-input--type "hello")
    (agent-repl--send :user-sent)
    (should (equal (agent-repl-test-input--composer-text) "hello"))))

(ert-deftest agent-repl-input-transport-failure-offers-to-the-queue ()
  "A transport failure hands the prompt to the hold queue for link-up."
  (agent-repl-test-input--with
    (setq agent-repl-test-input--answer '(:failure (:kind :transport :message "gone")))
    (agent-repl-test-input--type "hello")
    (agent-repl--send :user-sent)
    (should (equal (length agent-repl-test-input--queued) 1))
    (should (equal (nth 3 (car agent-repl-test-input--queued)) "hello"))))

(ert-deftest agent-repl-input-transport-failure-offers-this-attempts-key ()
  "The offer carries THIS attempt\='s key: the re-drive is a retry of it."
  (agent-repl-test-input--with
    (setq agent-repl-test-input--answer '(:failure (:kind :transport :message "gone")))
    (agent-repl-test-input--type "hello")
    (let ((key (agent-repl--send :user-sent)))
      (should (equal (nth 4 (car agent-repl-test-input--queued)) key)))))

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

(ert-deftest agent-repl-input-no-connection-offers-to-the-queue ()
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

(ert-deftest agent-repl-input-attachments-cleared-after-a-turn ()
  "Attachments clear once the daemon has accepted the submission."
  (agent-repl-test-input--with
    (with-current-buffer agent-repl-test-input--buffer
      (agent-repl-input-attach-image "/tmp/a.png" "image/png"))
    (agent-repl-test-input--type "look")
    (agent-repl--send :user-sent)
    (should-not (agent-repl-input-attachments "ws-one"))))

(ert-deftest agent-repl-input-attachments-survive-a-refusal ()
  "A refused submission may not silently discard an attachment."
  (agent-repl-test-input--with
    (setq agent-repl-test-input--answer
          '(:response (:arm :error :value (:reason (:arm :merging :value nil)))))
    (with-current-buffer agent-repl-test-input--buffer
      (agent-repl-input-attach-image "/tmp/a.png" "image/png"))
    (agent-repl-test-input--type "look")
    (agent-repl--send :user-sent)
    (should (equal (length (agent-repl-input-attachments "ws-one")) 1))))

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

(provide 'test-input)

;;; test-input.el ends here
