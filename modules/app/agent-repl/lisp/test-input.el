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

(defvar agent-repl-test-input--held-deliveries nil
  "The delivery each held-prompt ingress write carried, newest first.")

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
         (agent-repl-test-input--held-deliveries nil)
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
                       (lambda (ws said origin key &optional delivery)
                         (when agent-repl-test-input--hold-error
                           (signal (car agent-repl-test-input--hold-error)
                                   (cdr agent-repl-test-input--hold-error)))
                         (push (list ws said origin key)
                               agent-repl-test-input--queued)
                         (push delivery agent-repl-test-input--held-deliveries)
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

(ert-deftest agent-repl-input-a-first-send-closes-the-news-digest ()
  "Sending a prompt closes a standing news digest on the workspace's page."
  (agent-repl-test-input--with
    ;; Arrange
    (let ((dismissed nil))
      (cl-letf (((symbol-function 'agent-repl--input-dismiss-digest)
                 (lambda (ws) (push ws dismissed))))
        ;; Act
        (agent-repl--input-submit "ws-1" (list :content (list :blocks nil))
                                  :user-sent "hello"))
      ;; Assert
      (should (equal dismissed '("ws-1"))))))

(ert-deftest agent-repl-input-a-retry-leaves-the-news-digest-alone ()
  "A retry re-drives an earlier send, so it closes nothing."
  (agent-repl-test-input--with
    ;; Arrange
    (let ((dismissed nil))
      (cl-letf (((symbol-function 'agent-repl--input-dismiss-digest)
                 (lambda (ws) (push ws dismissed))))
        ;; Act
        (agent-repl--input-submit "ws-1" (list :content (list :blocks nil))
                                  :deferred-prompt "hello" "key-failed"))
      ;; Assert
      (should-not dismissed))))

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

(ert-deftest agent-repl-input-discard-saves-as-a-discard ()
  "`agent-repl-discard-input' saves through the discard-aware history push."
  (agent-repl-test-input--with
    (let ((calls nil))
      (cl-letf (((symbol-function 'agent-repl--history-push-discard)
                 (lambda () (push :discard calls))))
        (with-current-buffer agent-repl-test-input--buffer
          (insert "hello")
          (agent-repl-discard-input)))
      (should (equal calls '(:discard))))))

(ert-deftest agent-repl-input-discard-drops-attachments ()
  "Discarding the composer discards what was attached to it."
  (agent-repl-test-input--with
    (with-current-buffer agent-repl-test-input--buffer
      (agent-repl-input-attach-image "/tmp/a.png" "image/png")
      (insert "hello")
      (agent-repl-discard-input)
      (should-not agent-repl-input-attachments))))

(ert-deftest agent-repl-input-discard-empties-the-composer ()
  "Discarding leaves the composer empty."
  (agent-repl-test-input--with
    ;; Arrange
    (agent-repl-test-input--type "hello")
    ;; Act
    (with-current-buffer agent-repl-test-input--buffer
      (agent-repl-discard-input))
    ;; Assert
    (should (equal (agent-repl-test-input--composer-text) ""))))

(ert-deftest agent-repl-input-discard-submits-nothing ()
  "Discarding sends nothing to the daemon and holds nothing for it."
  (agent-repl-test-input--with
    ;; Arrange
    (agent-repl-test-input--type "hello")
    ;; Act
    (with-current-buffer agent-repl-test-input--buffer
      (agent-repl-discard-input))
    ;; Assert
    (should-not agent-repl-test-input--submitted)
    (should-not agent-repl-test-input--queued)))

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

(ert-deftest agent-repl-input-expire-flash-leaves-a-later-flash-standing ()
  "The dwell NEVER clears a notice it did not set.
A second refusal flashed within the first one's dwell would otherwise have
its own dwell cut short by the first flash's timer."
  ;; Arrange.
  (let ((buffer (generate-new-buffer " *agent-repl-test-input-flash*")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local agent-repl-input-notice "composer closed: restarting"))
          ;; Act.
          (cl-letf (((symbol-function 'agent-repl--log) #'ignore))
            (agent-repl--input-expire-flash "ws" buffer "refused: merge in flight"))
          ;; Assert.
          (should (equal (buffer-local-value 'agent-repl-input-notice buffer)
                         "composer closed: restarting")))
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

(ert-deftest agent-repl-input-mode-enables-visual-line-mode ()
  "The composer turns on `visual-line-mode' so Evil moves by screen line."
  ;; Arrange.
  (with-temp-buffer
    ;; Act.
    (agent-repl-input-mode)
    ;; Assert.
    (should visual-line-mode)))

(ert-deftest agent-repl-input-mode-paints-the-composer-background ()
  "The composer paints `agent-repl-input-background' behind its text."
  ;; Arrange.
  (let ((agent-repl-input-background "#123456")
        (remapped nil))
    (cl-letf (((symbol-function 'agent-repl--set-buffer-background)
               (lambda (color) (setq remapped color))))
      (with-temp-buffer
        ;; Act.
        (agent-repl-input-mode)
        ;; Assert.
        (should (equal remapped "#123456"))))))

(ert-deftest agent-repl-input-background-is-the-webapp-composer-color ()
  "The composer's default background is the webapp's dark `--composer-bg'.
The ert suite may not read webapp/src/styles.css, so the shared value is
pinned on both sides: here, and in webapp/test/styles.test.ts."
  ;; Arrange / Act.
  (let ((background (eval (car (get 'agent-repl-input-background 'standard-value)))))
    ;; Assert.
    (should (equal background "#1a1a19"))))

(ert-deftest agent-repl-input-mode-wraps-at-word-boundaries ()
  "The composer soft-wraps long lines at word boundaries."
  ;; Arrange.
  (with-temp-buffer
    ;; Act.
    (agent-repl-input-mode)
    ;; Assert.
    (should word-wrap)
    (should-not truncate-lines)))

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

(defconst agent-repl-test-input--ask-answered-answer
  '(:response (:arm :success :value (:arm :ask-answered :value nil)))
  "A scripted `ask_answered' success: the send answered a standing ask.")

(ert-deftest agent-repl-input-ask-answered-arm-clears-the-composer ()
  "A typed answer to a standing ask landed: nothing awaited, so the input
clears."
  (agent-repl-test-input--with
    (setq agent-repl-test-input--answer agent-repl-test-input--ask-answered-answer)
    (agent-repl-test-input--type "use OAuth")
    (agent-repl--send :user-sent)
    (should (equal (agent-repl-test-input--composer-text) ""))))

(ert-deftest agent-repl-input-ask-answered-arm-runs-no-posthooks ()
  "No turn was minted, so there is nothing for a posthook to post-process."
  (agent-repl-test-input--with
    (setq agent-repl-test-input--answer agent-repl-test-input--ask-answered-answer)
    (let* ((ran nil)
           (agent-repl-send-posthooks
            (list (cons "" (lambda (_ws _raw) (setq ran t))))))
      (agent-repl-test-input--type "use OAuth")
      (agent-repl--send :user-sent)
      (should-not ran))))

(ert-deftest agent-repl-input-ask-answered-arm-is-recorded ()
  "The typed answer is recorded at INFO, never as an unknown success arm."
  (agent-repl-test-input--with
    (setq agent-repl-test-input--answer agent-repl-test-input--ask-answered-answer)
    (agent-repl-test-input--type "use OAuth")
    (agent-repl-test-input--capturing-rungs
      (agent-repl--send :user-sent)
      (should (agent-repl-test-input--rung-has-p
               agent-repl-test-input--info "elisp.input.ask-answered"))
      (should-not (agent-repl-test-input--rung-has-p
                   agent-repl-test-input--error "elisp.input.unknown-success-arm")))))

(defconst agent-repl-test-input--ask-not-standing-answer
  '(:response (:arm :error :value (:reason (:arm :ask-not-standing :value nil))))
  "A scripted `ask_not_standing' refusal: the ask settled first.")

(ert-deftest agent-repl-input-ask-not-standing-keeps-the-text ()
  "The typed answer reached nobody, so the composer keeps the text."
  (agent-repl-test-input--with
    (setq agent-repl-test-input--answer agent-repl-test-input--ask-not-standing-answer)
    (agent-repl-test-input--type "use OAuth")
    (agent-repl--send :user-sent)
    (should (equal (agent-repl-test-input--composer-text) "use OAuth"))))

(ert-deftest agent-repl-input-ask-not-standing-is-not-queued ()
  "A re-drive would reach the session as a prompt the user never meant, so
nothing is held."
  (agent-repl-test-input--with
    (setq agent-repl-test-input--answer agent-repl-test-input--ask-not-standing-answer)
    (agent-repl-test-input--type "use OAuth")
    (agent-repl--send :user-sent)
    (should-not agent-repl-test-input--queued)))

(ert-deftest agent-repl-input-ask-not-standing-flashes-and-records-it ()
  "The refusal says what happened and is recorded at INFO, not as an
unknown arm."
  (agent-repl-test-input--with
    (setq agent-repl-test-input--answer agent-repl-test-input--ask-not-standing-answer)
    (agent-repl-test-input--type "use OAuth")
    (agent-repl-test-input--capturing-rungs
      (agent-repl--send :user-sent)
      (should (equal (buffer-local-value 'agent-repl-input-notice
                                         agent-repl-test-input--buffer)
                     agent-repl--input-ask-not-standing-flash))
      (should (agent-repl-test-input--rung-has-p
               agent-repl-test-input--info "elisp.input.refused-ask-not-standing"))
      (should-not (agent-repl-test-input--rung-has-p
                   agent-repl-test-input--error "elisp.input.unknown-error-arm")))))

(ert-deftest agent-repl-input-say-refusal-flashes-and-echoes-the-sentence ()
  "The helper flashes the sentence in the composer and echoes it alone."
  (agent-repl-test-input--with
    (agent-repl--input-say-refusal "ws-one" "refused for a reason")
    (should (equal (buffer-local-value 'agent-repl-input-notice agent-repl-test-input--buffer)
                   "refused for a reason"))
    (should (member "agent-repl: refused for a reason" agent-repl-test-input--messages))))

(ert-deftest agent-repl-input-say-refusal-appends-a-detail-verbatim ()
  "A non-empty detail rides the echo in parentheses, verbatim."
  (agent-repl-test-input--with
    (agent-repl--input-say-refusal "ws-one" "refused" "the daemon's words")
    (should (member "agent-repl: refused (the daemon's words)" agent-repl-test-input--messages))))

(ert-deftest agent-repl-input-say-refusal-drops-an-empty-detail ()
  "An empty detail adds nothing, not an empty pair of parentheses."
  (agent-repl-test-input--with
    (agent-repl--input-say-refusal "ws-one" "refused" "")
    (should (member "agent-repl: refused" agent-repl-test-input--messages))))

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

(ert-deftest agent-repl-test-input-ret-sends-in-the-plain-map ()
  "`RET' in the composer's plain mode map sends, so emacs state can send.
The `map!' `:ni' entry reaches evil's normal and insert maps only."
  (should (eq (lookup-key agent-repl-input-mode-map (kbd "RET"))
              #'agent-repl-send)))

(ert-deftest agent-repl-test-input-s-ret-inserts-newline-in-the-plain-map ()
  "`S-RET' in the composer's plain mode map inserts a newline, without sending."
  (should (eq (lookup-key agent-repl-input-mode-map (kbd "S-RET"))
              #'newline)))

(ert-deftest agent-repl-test-input-interrupt-turn-is-a-command ()
  "The chord's target is a real interactive command, not a dead symbol."
  (should (commandp #'agent-repl-interrupt-turn)))

;;;; ---- Feed text zoom (C-+ / C--) --------------------------------------

(ert-deftest agent-repl-test-input-zoom-keys-are-planted-in-every-evil-state ()
  "Both zoom chords go into each Evil state map, where Doom's own would win."
  ;; Arrange
  (let ((planted nil)
        (map (make-sparse-keymap)))
    (cl-letf (((symbol-function 'evil-define-key*)
               (lambda (state _map key def)
                 (push (list state (key-description key) def) planted))))
      ;; Act
      (agent-repl-bind-feed-zoom-keys map))
    ;; Assert
    (should (= (length planted) (* 5 3)))
    (should (member '(normal "C-+" agent-repl-feed-text-scale-increase) planted))
    (should (member '(insert "C--" agent-repl-feed-text-scale-decrease) planted))))

(ert-deftest agent-repl-test-input-zoom-keys-bind-the-map-itself ()
  "The chords bind in the map itself too, observable under -Q."
  ;; Arrange
  (let ((map (make-sparse-keymap)))
    (cl-letf (((symbol-function 'evil-define-key*) #'ignore))
      ;; Act
      (agent-repl-bind-feed-zoom-keys map))
    ;; Assert
    (should (eq (lookup-key map (kbd "C-+")) #'agent-repl-feed-text-scale-increase))
    (should (eq (lookup-key map (kbd "C--")) #'agent-repl-feed-text-scale-decrease))))

(ert-deftest agent-repl-test-input-zoom-keys-plant-c-equal-as-increase ()
  "`C-=' goes into each Evil state map as the zoom-in command."
  ;; Arrange
  (let ((planted nil)
        (map (make-sparse-keymap)))
    (cl-letf (((symbol-function 'evil-define-key*)
               (lambda (state _map key def)
                 (push (list state (key-description key) def) planted))))
      ;; Act
      (agent-repl-bind-feed-zoom-keys map))
    ;; Assert
    (should (seq-every-p
             (lambda (state)
               (member (list state "C-=" #'agent-repl-feed-text-scale-increase) planted))
             '(normal motion visual insert emacs)))))

(ert-deftest agent-repl-test-input-zoom-keys-bind-c-equal-in-the-map-itself ()
  "`C-=' binds the zoom-in command in the map itself, observable under -Q."
  ;; Arrange
  (let ((map (make-sparse-keymap)))
    (cl-letf (((symbol-function 'evil-define-key*) #'ignore))
      ;; Act
      (agent-repl-bind-feed-zoom-keys map))
    ;; Assert
    (should (eq (lookup-key map (kbd "C-=")) #'agent-repl-feed-text-scale-increase))))

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

(ert-deftest agent-repl-test-input-c-equal-zooms-feed-in ()
  "`C-=' in the composer is bound to the feed zoom-in command, as `C-+' is."
  (should (eq (lookup-key agent-repl-input-mode-map (kbd "C-="))
              #'agent-repl-feed-text-scale-increase)))

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

;;;; ---- Feed selection (reply to a response, roll back to a prompt) -------

(defvar agent-repl-test-input--selection-kind nil
  "The selection kind the stubbed host watch reports for the composer's workspace.")

(defvar agent-repl-test-input--select-requests nil
  "Every `SelectFeedRow' request the stubbed rpc received, newest first.")

(defmacro agent-repl-test-input--with-select (answer &rest body)
  "Run BODY in a composer whose `SelectFeedRow' answers ANSWER.
ANSWER is `(:response PLIST)' or `(:failure DETAIL)'; nil answers nothing.
The host watch's selection kind is `agent-repl-test-input--selection-kind'."
  (declare (indent 1))
  `(agent-repl-test-input--with
     (let ((agent-repl-test-input--select-requests nil)
           (agent-repl-test-input--selection-kind nil)
           (answer ,answer))
       (cl-letf (((symbol-function 'agent-repl-host-selection)
                  (lambda (_ws) agent-repl-test-input--selection-kind))
                 ((symbol-function 'agent-repl-rpc-select-feed-row)
                  (lambda (_conn request &rest keys)
                    (push request agent-repl-test-input--select-requests)
                    (pcase (car answer)
                      (:response (funcall (plist-get keys :on-response) (cadr answer)))
                      (:failure (funcall (plist-get keys :on-failure) (cadr answer)))))))
         ,@body))))

(defun agent-repl-test-input--select-outcome (arm)
  "A `SelectFeedRow' success answer whose outcome is ARM."
  (list :response
        (list :arm :success
              :value (list :outcome (list :arm arm :value nil)))))

(defconst agent-repl-test-input--select-refused
  '(:response (:arm :error :value (:cause (:arm :unknown-workspace :value nil))))
  "A `SelectFeedRow' refusal answer.")

(defun agent-repl-test-input--notice ()
  "Return the composer's mode-line notice."
  (buffer-local-value 'agent-repl-input-notice agent-repl-test-input--buffer))

(defun agent-repl-test-input--last-move ()
  "Return the move of the last `SelectFeedRow' request sent."
  (plist-get (car agent-repl-test-input--select-requests) :move))

(ert-deftest agent-repl-test-input-turn-boundary-select-prev-is-a-command ()
  "The `M-s-k' target is a real interactive command."
  (should (commandp #'agent-repl-turn-boundary-select-prev)))

(ert-deftest agent-repl-test-input-turn-boundary-select-next-is-a-command ()
  "The `M-s-j' target is a real interactive command."
  (should (commandp #'agent-repl-turn-boundary-select-next)))

(ert-deftest agent-repl-test-input-selection-escape-is-a-command ()
  "The command-mode escape target is a real interactive command."
  (should (commandp #'agent-repl-input-selection-escape)))

(ert-deftest agent-repl-test-input-meta-super-k-steps-turn-boundaries-older ()
  "`M-s-k' in the composer is bound to the older (upward) step, in both states."
  (should (eq (lookup-key agent-repl-input-mode-map (kbd "M-s-k"))
              #'agent-repl-turn-boundary-select-prev)))

(ert-deftest agent-repl-test-input-meta-super-j-steps-turn-boundaries-newer ()
  "`M-s-j' in the composer is bound to the newer (downward) step, in both states."
  (should (eq (lookup-key agent-repl-input-mode-map (kbd "M-s-j"))
              #'agent-repl-turn-boundary-select-next)))

(ert-deftest agent-repl-test-input-old-selection-chords-are-unbound ()
  "The retired selection chords no longer step the selection in the composer."
  (dolist (key '("C-S-p" "C-S-n"))
    (should-not (lookup-key agent-repl-input-mode-map (kbd key)))))

(ert-deftest agent-repl-test-input-turn-boundary-select-prev-steps-older ()
  "`M-s-k' asks the daemon to step the turn boundaries OLDER."
  (agent-repl-test-input--with-select (agent-repl-test-input--select-outcome :selected)
    ;; Act
    (agent-repl-turn-boundary-select-prev)
    ;; Assert
    (should (equal (agent-repl-test-input--last-move)
                   '(:arm :turn-boundary :value (:direction :older))))))

(ert-deftest agent-repl-test-input-turn-boundary-select-next-steps-newer ()
  "`M-s-j' asks the daemon to step the turn boundaries NEWER."
  (agent-repl-test-input--with-select (agent-repl-test-input--select-outcome :selected)
    ;; Act
    (agent-repl-turn-boundary-select-next)
    ;; Assert
    (should (equal (agent-repl-test-input--last-move)
                   '(:arm :turn-boundary :value (:direction :newer))))))

(ert-deftest agent-repl-test-input-select-addresses-this-workspace ()
  "A step names its workspace by the daemon-minted ref."
  (agent-repl-test-input--with-select (agent-repl-test-input--select-outcome :selected)
    ;; Act
    (agent-repl-turn-boundary-select-prev)
    ;; Assert
    (should (equal (plist-get (car agent-repl-test-input--select-requests) :workspace)
                   agent-repl-test-input--ref))))

(ert-deftest agent-repl-test-input-step-with-nothing-selectable-flashes ()
  "A step that finds no prompt or final response says so."
  (agent-repl-test-input--with-select
      (agent-repl-test-input--select-outcome :nothing-selectable)
    ;; Act
    (agent-repl-turn-boundary-select-prev)
    ;; Assert
    (should (equal (agent-repl-test-input--notice)
                   "selection: no prompt or final response to select"))))

(ert-deftest agent-repl-test-input-selected-step-flashes-nothing ()
  "A step that lands on a row leaves the composer quiet."
  (agent-repl-test-input--with-select (agent-repl-test-input--select-outcome :selected)
    ;; Act
    (agent-repl-turn-boundary-select-prev)
    ;; Assert
    (should-not (agent-repl-test-input--notice))))

(ert-deftest agent-repl-test-input-refused-step-flashes ()
  "A refused step flashes the composer under the selection prefix."
  (agent-repl-test-input--with-select agent-repl-test-input--select-refused
    ;; Act
    (agent-repl-turn-boundary-select-next)
    ;; Assert
    (should (equal (agent-repl-test-input--notice) "selection: selection refused"))))

(ert-deftest agent-repl-test-input-refused-step-is-logged ()
  "A refused step is logged at WARN with its cause."
  (agent-repl-test-input--with-select agent-repl-test-input--select-refused
    ;; Arrange
    (let ((warned nil))
      (cl-letf (((symbol-function 'agent-repl--warn)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) warned))))
        ;; Act
        (agent-repl-turn-boundary-select-prev))
      ;; Assert
      (should (cl-some (lambda (m) (string-match-p "select-feed-row-refused.*unknown-workspace" m))
                       warned)))))

(ert-deftest agent-repl-test-input-failed-step-flashes ()
  "A step the daemon never answered flashes the composer."
  (agent-repl-test-input--with-select '(:failure (:kind :transport :message "down"))
    ;; Act
    (agent-repl-turn-boundary-select-prev)
    ;; Assert
    (should (equal (agent-repl-test-input--notice) "selection: the daemon did not answer"))))

(ert-deftest agent-repl-test-input-failed-step-is-logged ()
  "A step the daemon never answered is logged at WARN with the detail."
  (agent-repl-test-input--with-select '(:failure (:kind :transport :message "down"))
    ;; Arrange
    (let ((warned nil))
      (cl-letf (((symbol-function 'agent-repl--warn)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) warned))))
        ;; Act
        (agent-repl-turn-boundary-select-next))
      ;; Assert
      (should (cl-some (lambda (m) (string-match-p "select-feed-row-failure.*down" m))
                       warned)))))

(ert-deftest agent-repl-test-input-unknown-answer-arm-is-logged-as-error ()
  "An answer arm the composer does not know is an ERROR, never silence."
  (agent-repl-test-input--with-select '(:response (:arm :accepted :value nil))
    ;; Arrange
    (let ((errors nil))
      (cl-letf (((symbol-function 'agent-repl--error)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) errors))))
        ;; Act
        (agent-repl-turn-boundary-select-next))
      ;; Assert
      (should (cl-some (lambda (m) (string-match-p "select-feed-row-unknown-arm" m))
                       errors)))))

(ert-deftest agent-repl-test-input-step-without-a-ref-sends-nothing ()
  "A workspace not registered yet sends no step."
  (agent-repl-test-input--with-select (agent-repl-test-input--select-outcome :selected)
    ;; Arrange
    (cl-letf (((symbol-function 'agent-repl-host-ref) (lambda (_ws) nil)))
      ;; Act
      (agent-repl-turn-boundary-select-prev))
    ;; Assert
    (should-not agent-repl-test-input--select-requests)))

(ert-deftest agent-repl-test-input-step-without-a-ref-flashes ()
  "A workspace not registered yet says so under the step's prefix."
  (agent-repl-test-input--with-select (agent-repl-test-input--select-outcome :selected)
    ;; Arrange
    (cl-letf (((symbol-function 'agent-repl-host-ref) (lambda (_ws) nil)))
      ;; Act
      (agent-repl-turn-boundary-select-prev))
    ;; Assert
    (should (equal (agent-repl-test-input--notice) "selection: workspace not ready"))))

(ert-deftest agent-repl-test-input-step-without-a-connection-flashes ()
  "No daemon connection says so under the step's prefix."
  (agent-repl-test-input--with-select (agent-repl-test-input--select-outcome :selected)
    ;; Arrange
    (cl-letf (((symbol-function 'agent-repl-host-conn) (lambda (_ws) nil))
              ((symbol-function 'agent-repl-link-primary) (lambda () nil)))
      ;; Act
      (agent-repl-turn-boundary-select-prev))
    ;; Assert
    (should (equal (agent-repl-test-input--notice) "selection: no daemon connection"))))

(ert-deftest agent-repl-test-input-submit-carries-no-reply-target ()
  "A submit names no reply target: the daemon applies its own selection."
  (agent-repl-test-input--with-select nil
    ;; Arrange
    (setq agent-repl-test-input--selection-kind :response)
    (agent-repl-test-input--type "reply text")
    ;; Act
    (agent-repl--send :user-sent)
    ;; Assert
    (should-not (plist-member (agent-repl-test-input--request)
                              :reference-response-feedid))))

(ert-deftest agent-repl-test-input-successful-submit-sends-no-clear ()
  "An accepted submit leaves ending the selection to the daemon."
  (agent-repl-test-input--with-select nil
    ;; Arrange
    (setq agent-repl-test-input--selection-kind :response)
    (agent-repl-test-input--type "reply")
    ;; Act (harness answers with a minted turn)
    (agent-repl--send :user-sent)
    ;; Assert
    (should-not agent-repl-test-input--select-requests)))

(ert-deftest agent-repl-test-input-escape-without-selection-delegates ()
  "Escape keeps its ordinary meaning when nothing is selected."
  (agent-repl-test-input--with-select nil
    ;; Arrange
    (setq agent-repl-test-input--selection-kind :none)
    (let ((defaulted 0))
      (cl-letf (((symbol-function 'agent-repl--input-escape-default)
                 (lambda () (cl-incf defaulted))))
        ;; Act
        (let ((last-command 'agent-repl-input-selection-escape))
          (agent-repl-input-selection-escape)))
      ;; Assert
      (should (= defaulted 1)))))

(ert-deftest agent-repl-test-input-escape-before-any-push-delegates ()
  "With no selection push yet, escape keeps its ordinary meaning."
  (agent-repl-test-input--with-select nil
    ;; Arrange
    (setq agent-repl-test-input--selection-kind nil)
    (let ((defaulted 0))
      (cl-letf (((symbol-function 'agent-repl--input-escape-default)
                 (lambda () (cl-incf defaulted))))
        ;; Act
        (agent-repl-input-selection-escape))
      ;; Assert
      (should (= defaulted 1)))))

(ert-deftest agent-repl-test-input-first-escape-warns ()
  "The first command-mode escape over a selection only WARNS."
  (agent-repl-test-input--with-select nil
    ;; Arrange
    (setq agent-repl-test-input--selection-kind :prompt)
    ;; Act -- the preceding command was a step, not another escape.
    (let ((last-command 'agent-repl-turn-boundary-select-prev))
      (agent-repl-input-selection-escape))
    ;; Assert
    (should (member "selection: press escape again to clear the selection"
                    agent-repl-test-input--messages))))

(ert-deftest agent-repl-test-input-first-escape-is-recorded-at-info ()
  "The armed first escape is a user's act: recorded at INFO, never WARN."
  (agent-repl-test-input--with-select nil
    ;; Arrange
    (setq agent-repl-test-input--selection-kind :prompt)
    (agent-repl-test-input--capturing-rungs
      ;; Act
      (let ((last-command 'agent-repl-turn-boundary-select-prev))
        (agent-repl-input-selection-escape))
      ;; Assert
      (should (cl-some (lambda (m) (string-prefix-p "elisp.input.selection-escape-armed" m))
                       agent-repl-test-input--info))
      (should-not agent-repl-test-input--warn))))

(ert-deftest agent-repl-test-input-first-escape-sends-nothing ()
  "The first command-mode escape over a selection clears nothing."
  (agent-repl-test-input--with-select nil
    ;; Arrange
    (setq agent-repl-test-input--selection-kind :response)
    ;; Act
    (let ((last-command 'agent-repl-turn-boundary-select-prev))
      (agent-repl-input-selection-escape))
    ;; Assert
    (should-not agent-repl-test-input--select-requests)))

(ert-deftest agent-repl-test-input-second-consecutive-escape-sends-clear ()
  "Two consecutive escapes over a selection send CLEAR."
  (agent-repl-test-input--with-select (agent-repl-test-input--select-outcome :none)
    ;; Arrange
    (setq agent-repl-test-input--selection-kind :prompt)
    ;; Act -- the preceding command was the escape itself.
    (let ((last-command 'agent-repl-input-selection-escape))
      (agent-repl-input-selection-escape))
    ;; Assert
    (should (equal (agent-repl-test-input--last-move) '(:arm :clear :value nil)))))

(ert-deftest agent-repl-test-input-refused-clear-flashes ()
  "A refused clear flashes the composer under the selection prefix."
  (agent-repl-test-input--with-select agent-repl-test-input--select-refused
    ;; Arrange
    (setq agent-repl-test-input--selection-kind :response)
    ;; Act
    (let ((last-command 'agent-repl-input-selection-escape))
      (agent-repl-input-selection-escape))
    ;; Assert
    (should (equal (agent-repl-test-input--notice) "selection: selection refused"))))

(ert-deftest agent-repl-test-input-non-consecutive-escape-rearms ()
  "A command-mode key between two escapes resets the consecutive count.
The second escape is treated as a FIRST again -- it warns, never clears."
  (agent-repl-test-input--with-select nil
    ;; Arrange
    (setq agent-repl-test-input--selection-kind :response)
    ;; Act -- an intervening command ran as `last-command'.
    (let ((last-command 'agent-repl-turn-boundary-select-next))
      (agent-repl-input-selection-escape))
    ;; Assert
    (should-not agent-repl-test-input--select-requests)))

;;;; ---- The shared history save and composer fill -----------------------

(ert-deftest agent-repl-test-input-save-to-history-pushes-resets-and-saves ()
  "The shared save pushes the composer's text, resets browsing, and persists."
  (agent-repl-test-input--with
    ;; Arrange
    (agent-repl-test-input--type "draft")
    (let (calls)
      (cl-letf (((symbol-function 'agent-repl--history-push)
                 (lambda (&optional _text) (push (list :push (buffer-string)) calls)))
                ((symbol-function 'agent-repl--history-reset) (lambda () (push :reset calls)))
                ((symbol-function 'agent-repl--history-save) (lambda (ws) (push (list :save ws) calls))))
        ;; Act
        (with-current-buffer agent-repl-test-input--buffer
          (agent-repl--input-save-to-history "ws-one")))
      ;; Assert
      (should (equal (nreverse calls) '((:push "draft") :reset (:save "ws-one")))))))

(ert-deftest agent-repl-test-input-discard-saves-through-the-shared-save ()
  "The discard saves the composer through the one shared save."
  (agent-repl-test-input--with
    ;; Arrange
    (let (saved)
      (cl-letf (((symbol-function 'agent-repl--input-save-to-history)
                 (lambda (ws &optional discard) (push (list ws discard) saved))))
        ;; Act
        (with-current-buffer agent-repl-test-input--buffer
          (agent-repl-discard-input)))
      ;; Assert: saved once, as a discard.
      (should (equal saved '(("ws-one" t)))))))

(defun agent-repl-test-input--said (&rest blocks)
  "A decoded UserSaid holding BLOCKS."
  (list :content (list :blocks blocks)))

(defun agent-repl-test-input--text-block (text)
  "A decoded text block saying TEXT."
  (list :arm :text :value (list :text text)))

(defun agent-repl-test-input--path-image (path)
  "A decoded image block at host PATH."
  (list :arm :image :value (list :location (list :arm :path :value (list :path path))
                                 :media-type "image/png")))

(defun agent-repl-test-input--url-image (url)
  "A decoded image block by URL."
  (list :arm :image :value (list :location (list :arm :url :value (list :url url))
                                 :media-type "image/png")))

(ert-deftest agent-repl-test-input-said-text-joins-text-blocks ()
  "The words of what was said are its text blocks, joined by newlines."
  (should (equal (agent-repl--input-said-text
                  (agent-repl-test-input--said (agent-repl-test-input--text-block "one")
                                               (agent-repl-test-input--path-image "/a.png")
                                               (agent-repl-test-input--text-block "two")))
                 "one\ntwo")))

(ert-deftest agent-repl-test-input-said-attachments-keeps-path-images ()
  "A path image becomes a composer attachment."
  (should (equal (agent-repl--input-said-attachments
                  (agent-repl-test-input--said (agent-repl-test-input--path-image "/a.png")))
                 '(:attachments ((:path "/a.png" :media-type "image/png")) :dropped 0))))

(ert-deftest agent-repl-test-input-said-attachments-counts-url-images-dropped ()
  "An image by URL cannot be an attachment: it is counted as dropped."
  (should (equal (agent-repl--input-said-attachments
                  (agent-repl-test-input--said (agent-repl-test-input--url-image "https://x/i.png")))
                 '(:attachments nil :dropped 1))))

(ert-deftest agent-repl-test-input-said-attachments-counts-unsupported-dropped ()
  "A block the schema does not model is counted as dropped."
  (should (equal (plist-get (agent-repl--input-said-attachments
                             (agent-repl-test-input--said '(:arm :unsupported :value (:kind "x"))))
                            :dropped)
                 1)))

;;;; ---- Rollback --------------------------------------------------------

(defvar agent-repl-test-input--plan-answer nil
  "What the stubbed PlanRollback answers: (:response R), (:failure DETAIL).")

(defvar agent-repl-test-input--roll-back-answer nil
  "What the stubbed RollBack answers: (:response R) or (:failure DETAIL).")

(defvar agent-repl-test-input--rollback-calls nil
  "Every rollback rpc the stubs received, newest first: (VERB REQUEST).")

(defvar agent-repl-test-input--list-answer nil
  "What the stubbed ListRollbackPrompts answers: (:response R) or (:failure DETAIL).")

(defvar agent-repl-test-input--asked nil
  "The y/n question the stubbed `y-or-n-p' was asked.")

(defvar agent-repl-test-input--warned nil
  "Messages the stubbed `agent-repl--warn' received, newest first.")

(defmacro agent-repl-test-input--with-rollback (plan confirm roll-back &rest body)
  "Run BODY in a composer whose PlanRollback answers PLAN.
CONFIRM is what the y/n question answers; ROLL-BACK is RollBack's answer."
  (declare (indent 3))
  `(agent-repl-test-input--with
     (let ((agent-repl-test-input--plan-answer ,plan)
           (agent-repl-test-input--roll-back-answer ,roll-back)
           (agent-repl-test-input--rollback-calls nil)
           (agent-repl-test-input--asked nil)
           (agent-repl-test-input--warned nil)
           (confirm ,confirm))
       (cl-letf (((symbol-function 'agent-repl-rpc-plan-rollback-sync)
                  (lambda (_conn request &optional _timeout)
                    (push (list :plan request) agent-repl-test-input--rollback-calls)
                    (pcase (car agent-repl-test-input--plan-answer)
                      (:response (cadr agent-repl-test-input--plan-answer))
                      (:failure (signal 'agent-repl-connect-error
                                        (cadr agent-repl-test-input--plan-answer))))))
                 ((symbol-function 'agent-repl-rpc-list-rollback-prompts-sync)
                  (lambda (_conn request &optional _timeout)
                    (push (list :list request) agent-repl-test-input--rollback-calls)
                    (pcase (car agent-repl-test-input--list-answer)
                      (:response (cadr agent-repl-test-input--list-answer))
                      (:failure (signal 'agent-repl-connect-error
                                        (cadr agent-repl-test-input--list-answer))))))
                 ((symbol-function 'agent-repl-rpc-roll-back)
                  (lambda (_conn request &rest keys)
                    (push (list :roll-back request) agent-repl-test-input--rollback-calls)
                    (pcase (car agent-repl-test-input--roll-back-answer)
                      (:response (funcall (plist-get keys :on-response)
                                          (cadr agent-repl-test-input--roll-back-answer)))
                      (:failure (funcall (plist-get keys :on-failure)
                                         (cadr agent-repl-test-input--roll-back-answer))))))
                 ((symbol-function 'y-or-n-p)
                  (lambda (prompt) (setq agent-repl-test-input--asked prompt) confirm))
                 ((symbol-function 'agent-repl--image-insert-marker) (lambda (&rest _) nil))
                 ((symbol-function 'agent-repl--warn)
                  (lambda (_ws fmt &rest args)
                    (push (apply #'format fmt args) agent-repl-test-input--warned))))
         ,@body))))

(defun agent-repl-test-input--plan (&rest overrides)
  "A decoded plan, with OVERRIDES (a plist) replacing its fields."
  (let ((plan (list :token '(:value "tok-1")
                    :target '(:chosen :latest :excerpt "fix the bug" :prompts-dropped 1)
                    :files '(:arm :kept :value nil)
                    :interrupt nil
                    :drop-queued nil)))
    (while overrides
      (setq plan (plist-put plan (pop overrides) (pop overrides))))
    plan))

(defun agent-repl-test-input--plan-answer (plan)
  "A PlanRollback success answering PLAN."
  (list :response (list :arm :success :value (list :outcome (list :arm :plan :value plan)))))

(defun agent-repl-test-input--done (said &optional files)
  "A RollBack success handing back SAID, FILES restored when non-nil."
  (list :response (list :arm :success
                        :value (list :prompt said
                                     :files-restored (and files (list :files files))))))

(defun agent-repl-test-input--refusal (arm &optional value)
  "A refusal answer of ARM carrying VALUE."
  (list :response (list :arm :error :value (list :cause (list :arm arm :value value)))))

(defun agent-repl-test-input--roll-back-sent ()
  "The RollBack request sent, or nil."
  (cadr (assq :roll-back agent-repl-test-input--rollback-calls)))

(defun agent-repl-test-input--face-of (text sub)
  "The face TEXT wears where SUB begins in it."
  (let ((at (string-search sub text)))
    (and at (get-text-property at 'face text))))

(ert-deftest agent-repl-test-input-rollback-keep-files-is-a-command ()
  "The `C-c C-RET' target is a real interactive command."
  (should (commandp #'agent-repl-rollback-keep-files)))

(ert-deftest agent-repl-test-input-rollback-restore-files-is-a-command ()
  "The `C-c M-RET' target is a real interactive command."
  (should (commandp #'agent-repl-rollback-restore-files)))

(ert-deftest agent-repl-test-input-c-c-c-return-rolls-back-keeping-files ()
  "`C-c C-RET' in the composer rolls back keeping files."
  (should (eq (lookup-key agent-repl-input-mode-map (kbd "C-c C-<return>"))
              #'agent-repl-rollback-keep-files)))

(ert-deftest agent-repl-test-input-c-c-m-return-rolls-back-restoring-files ()
  "`C-c M-RET' in the composer rolls back restoring files."
  (should (eq (lookup-key agent-repl-input-mode-map (kbd "C-c M-<return>"))
              #'agent-repl-rollback-restore-files)))

(ert-deftest agent-repl-test-input-rollback-leaves-c-c-c-k-alone ()
  "`C-c C-k' still only interrupts."
  (should (eq (lookup-key agent-repl-input-mode-map (kbd "C-c C-k"))
              #'agent-repl-interrupt-turn)))

(ert-deftest agent-repl-test-input-rollback-keep-plans-keep-files ()
  "`C-c C-RET' asks for a plan that keeps files."
  (agent-repl-test-input--with-rollback
      (agent-repl-test-input--plan-answer (agent-repl-test-input--plan)) nil nil
    ;; Act
    (agent-repl-rollback-keep-files)
    ;; Assert
    (should (equal (plist-get (cadr (assq :plan agent-repl-test-input--rollback-calls)) :files)
                   '(:arm :keep-files :value nil)))))

(ert-deftest agent-repl-test-input-rollback-restore-plans-restore-files ()
  "`C-c M-RET' asks for a plan that restores files."
  (agent-repl-test-input--with-rollback
      (agent-repl-test-input--plan-answer (agent-repl-test-input--plan)) nil nil
    ;; Act
    (agent-repl-rollback-restore-files)
    ;; Assert
    (should (equal (plist-get (cadr (assq :plan agent-repl-test-input--rollback-calls)) :files)
                   '(:arm :restore-files :value nil)))))

(ert-deftest agent-repl-test-input-rollback-nothing-flashes ()
  "Nothing to roll back says so and asks nothing."
  (agent-repl-test-input--with-rollback
      '(:response (:arm :success :value (:outcome (:arm :nothing-to-roll-back :value nil))))
      t nil
    ;; Act
    (agent-repl-rollback-keep-files)
    ;; Assert
    (should (equal (list (agent-repl-test-input--notice) agent-repl-test-input--asked)
                   '("rollback: no prompt to roll back to" nil)))))

(ert-deftest agent-repl-test-input-rollback-confirm-names-the-latest-prompt ()
  "With nothing selected the question cancels the latest prompt, by excerpt."
  (agent-repl-test-input--with-rollback
      (agent-repl-test-input--plan-answer (agent-repl-test-input--plan)) nil nil
    ;; Act
    (agent-repl-rollback-keep-files)
    ;; Assert
    (should (string-prefix-p "Cancel the latest prompt \"fix the bug\"."
                             agent-repl-test-input--asked))))

(ert-deftest agent-repl-test-input-rollback-confirm-names-the-selected-prompt ()
  "With a prompt selected the question rolls back to before it."
  (agent-repl-test-input--with-rollback
      (agent-repl-test-input--plan-answer
       (agent-repl-test-input--plan
        :target '(:chosen :selected :excerpt "add tests" :prompts-dropped 1)))
      nil nil
    ;; Act
    (agent-repl-rollback-keep-files)
    ;; Assert
    (should (string-prefix-p "Roll back to before the selected prompt \"add tests\"."
                             agent-repl-test-input--asked))))

(ert-deftest agent-repl-test-input-rollback-confirm-counts-prompts-dropped ()
  "More than one prompt dropped is counted in the question."
  (agent-repl-test-input--with-rollback
      (agent-repl-test-input--plan-answer
       (agent-repl-test-input--plan
        :target '(:chosen :selected :excerpt "add tests" :prompts-dropped 3)))
      nil nil
    ;; Act
    (agent-repl-rollback-keep-files)
    ;; Assert
    (should (string-search "(3 prompts are dropped)" agent-repl-test-input--asked))))

(ert-deftest agent-repl-test-input-rollback-confirm-keep-names-the-other-key ()
  "Keeping files says so and names the key that restores them."
  (agent-repl-test-input--with-rollback
      (agent-repl-test-input--plan-answer (agent-repl-test-input--plan)) nil nil
    ;; Act
    (agent-repl-rollback-keep-files)
    ;; Assert
    (should (string-search "Files are not restored (C-c M-RET restores them)."
                           agent-repl-test-input--asked))))

(ert-deftest agent-repl-test-input-rollback-confirm-restore-names-the-other-key ()
  "Restoring files states its limits and names the key that leaves them."
  (agent-repl-test-input--with-rollback
      (agent-repl-test-input--plan-answer
       (agent-repl-test-input--plan :files '(:arm :restored :value (:cancel-detached nil))))
      nil nil
    ;; Act
    (agent-repl-rollback-restore-files)
    ;; Assert
    (should (string-search "Files the agent's edit tools changed are restored; shell changes and git commits are not (C-c C-RET leaves files alone)."
                           agent-repl-test-input--asked))))

(ert-deftest agent-repl-test-input-rollback-confirm-interrupt-in-red ()
  "An interrupt is stated, in the `error' face."
  (agent-repl-test-input--with-rollback
      (agent-repl-test-input--plan-answer (agent-repl-test-input--plan :interrupt t)) nil nil
    ;; Act
    (agent-repl-rollback-keep-files)
    ;; Assert
    (should (eq (agent-repl-test-input--face-of
                 agent-repl-test-input--asked "The running turn will be interrupted.")
                'error))))

(ert-deftest agent-repl-test-input-rollback-confirm-drop-queued-in-red ()
  "Queued prompts dropped are counted, in the `error' face."
  (agent-repl-test-input--with-rollback
      (agent-repl-test-input--plan-answer (agent-repl-test-input--plan :drop-queued '(:prompts 2)))
      nil nil
    ;; Act
    (agent-repl-rollback-keep-files)
    ;; Assert
    (should (eq (agent-repl-test-input--face-of
                 agent-repl-test-input--asked "2 queued prompts will be dropped.")
                'error))))

(ert-deftest agent-repl-test-input-rollback-confirm-cancel-detached-in-red ()
  "Detached work stopped by a restore is counted, in the `error' face."
  (agent-repl-test-input--with-rollback
      (agent-repl-test-input--plan-answer
       (agent-repl-test-input--plan :files '(:arm :restored :value (:cancel-detached (:items 1)))))
      nil nil
    ;; Act
    (agent-repl-rollback-restore-files)
    ;; Assert
    (should (eq (agent-repl-test-input--face-of
                 agent-repl-test-input--asked
                 "1 background agent or shell started since then will be stopped.")
                'error))))

(ert-deftest agent-repl-test-input-rollback-confirm-files-sentence-not-red ()
  "The files sentence is not a side effect and wears no red."
  (agent-repl-test-input--with-rollback
      (agent-repl-test-input--plan-answer (agent-repl-test-input--plan :interrupt t)) nil nil
    ;; Act
    (agent-repl-rollback-keep-files)
    ;; Assert
    (should-not (agent-repl-test-input--face-of agent-repl-test-input--asked "Files are not"))))

(ert-deftest agent-repl-test-input-rollback-confirm-without-side-effects-has-no-red ()
  "A plan with no side effects lists none."
  (agent-repl-test-input--with-rollback
      (agent-repl-test-input--plan-answer (agent-repl-test-input--plan)) nil nil
    ;; Act
    (agent-repl-rollback-keep-files)
    ;; Assert
    (should-not (text-property-any 0 (length agent-repl-test-input--asked)
                                   'face 'error agent-repl-test-input--asked))))

(ert-deftest agent-repl-test-input-rollback-declined-sends-nothing ()
  "A no sends no RollBack."
  (agent-repl-test-input--with-rollback
      (agent-repl-test-input--plan-answer (agent-repl-test-input--plan)) nil nil
    ;; Act
    (agent-repl-rollback-keep-files)
    ;; Assert
    (should-not (agent-repl-test-input--roll-back-sent))))

(ert-deftest agent-repl-test-input-rollback-declined-says-cancelled ()
  "A no says the rollback was cancelled."
  (agent-repl-test-input--with-rollback
      (agent-repl-test-input--plan-answer (agent-repl-test-input--plan)) nil nil
    ;; Act
    (agent-repl-rollback-keep-files)
    ;; Assert
    (should (member "rollback: cancelled" agent-repl-test-input--messages))))

(ert-deftest agent-repl-test-input-rollback-confirmed-echoes-the-token ()
  "A yes sends RollBack with the plan's token, verbatim."
  (agent-repl-test-input--with-rollback
      (agent-repl-test-input--plan-answer (agent-repl-test-input--plan)) t
      (agent-repl-test-input--done (agent-repl-test-input--said))
    ;; Act
    (agent-repl-rollback-keep-files)
    ;; Assert
    (should (equal (agent-repl-test-input--roll-back-sent)
                   (list :workspace agent-repl-test-input--ref :token '(:value "tok-1"))))))

(ert-deftest agent-repl-test-input-rollback-done-saves-the-draft-to-history ()
  "The composer's contents are saved to its history before the refill."
  (agent-repl-test-input--with-rollback
      (agent-repl-test-input--plan-answer (agent-repl-test-input--plan)) t
      (agent-repl-test-input--done
       (agent-repl-test-input--said (agent-repl-test-input--text-block "old prompt")))
    ;; Arrange -- the harness stubs the history ring itself, so the shared
    ;; save is observed with what the composer held when it ran.
    (agent-repl-test-input--type "my draft")
    (let (saved)
      (cl-letf (((symbol-function 'agent-repl--input-save-to-history)
                 (lambda (ws) (push (list ws (buffer-string)) saved))))
        ;; Act
        (agent-repl-rollback-keep-files))
      ;; Assert
      (should (equal saved '(("ws-one" "my draft")))))))

(ert-deftest agent-repl-test-input-rollback-done-refills-the-composer ()
  "The composer holds the rolled-back prompt's words."
  (agent-repl-test-input--with-rollback
      (agent-repl-test-input--plan-answer (agent-repl-test-input--plan)) t
      (agent-repl-test-input--done
       (agent-repl-test-input--said (agent-repl-test-input--text-block "line one")
                                    (agent-repl-test-input--text-block "line two")))
    ;; Arrange
    (agent-repl-test-input--type "my draft")
    ;; Act
    (agent-repl-rollback-keep-files)
    ;; Assert
    (should (equal (agent-repl-test-input--composer-text) "line one\nline two"))))

(ert-deftest agent-repl-test-input-rollback-done-sets-path-attachments ()
  "The rolled-back prompt's path images become the composer's attachments."
  (agent-repl-test-input--with-rollback
      (agent-repl-test-input--plan-answer (agent-repl-test-input--plan)) t
      (agent-repl-test-input--done
       (agent-repl-test-input--said (agent-repl-test-input--text-block "see")
                                    (agent-repl-test-input--path-image "/tmp/a.png")))
    ;; Act
    (agent-repl-rollback-keep-files)
    ;; Assert
    (should (equal (buffer-local-value 'agent-repl-input-attachments
                                       agent-repl-test-input--buffer)
                   '((:path "/tmp/a.png" :media-type "image/png"))))))

(ert-deftest agent-repl-test-input-rollback-done-says-done ()
  "A rollback that restored no files says it is done."
  (agent-repl-test-input--with-rollback
      (agent-repl-test-input--plan-answer (agent-repl-test-input--plan)) t
      (agent-repl-test-input--done (agent-repl-test-input--said))
    ;; Act
    (agent-repl-rollback-keep-files)
    ;; Assert
    (should (member "rollback: done" agent-repl-test-input--messages))))

(ert-deftest agent-repl-test-input-rollback-done-counts-files-restored ()
  "A restore says how many files it changed back."
  (agent-repl-test-input--with-rollback
      (agent-repl-test-input--plan-answer
       (agent-repl-test-input--plan :files '(:arm :restored :value (:cancel-detached nil))))
      t (agent-repl-test-input--done (agent-repl-test-input--said) 3)
    ;; Act
    (agent-repl-rollback-restore-files)
    ;; Assert
    (should (member "rollback: done, 3 files restored" agent-repl-test-input--messages))))

(ert-deftest agent-repl-test-input-rollback-done-drops-a-url-image-with-a-flash ()
  "An image by URL cannot be attached: the composer says it was dropped."
  (agent-repl-test-input--with-rollback
      (agent-repl-test-input--plan-answer (agent-repl-test-input--plan)) t
      (agent-repl-test-input--done
       (agent-repl-test-input--said (agent-repl-test-input--url-image "https://x/i.png")))
    ;; Act
    (agent-repl-rollback-keep-files)
    ;; Assert
    (should (equal (agent-repl-test-input--notice)
                   "rollback: 1 image could not be attached and was dropped"))))

(ert-deftest agent-repl-test-input-rollback-done-logs-a-dropped-url-image ()
  "A dropped URL image is logged at WARN."
  (agent-repl-test-input--with-rollback
      (agent-repl-test-input--plan-answer (agent-repl-test-input--plan)) t
      (agent-repl-test-input--done
       (agent-repl-test-input--said (agent-repl-test-input--url-image "https://x/i.png")))
    ;; Act
    (agent-repl-rollback-keep-files)
    ;; Assert
    (should (cl-some (lambda (m) (string-match-p "rollback-attachments-dropped.*count=1" m))
                     agent-repl-test-input--warned))))

(defconst agent-repl-test-input--rollback-refusal-flashes
  '((:unknown-workspace nil "rollback: the daemon does not know this workspace")
    (:workspace-ref-mismatch (:registry-dir "/w")
     "rollback: the daemon holds this workspace under another directory")
    (:transferring-away (:address "127.0.0.1:9")
     "rollback: the workspace is moving to another daemon; press the key again")
    (:not-yet-adopted nil "rollback: the daemon is still taking this workspace over; press the key again")
    (:plan-stale nil "rollback: the conversation changed; press the key again")
    (:no-session nil "rollback: there is no running session to roll back")
    (:prompt-not-recorded nil "rollback: the conversation holds no record of that prompt")
    (:first-prompt nil "rollback: the first prompt can't be rolled back; /clear starts over")
    (:unseen-prompt nil "rollback: a later prompt the feed never showed would be dropped")
    (:files-not-restorable (:vendor-message "no checkpoint")
     "rollback: files can't be restored to that prompt: no checkpoint"))
  "Each RollBack refusal arm, its payload, and the flash it must draw.")

(ert-deftest agent-repl-test-input-rollback-each-roll-back-refusal-flashes-its-arm ()
  "Every RollBack refusal flashes the composer with its arm in plain words."
  (dolist (case agent-repl-test-input--rollback-refusal-flashes)
    (agent-repl-test-input--with-rollback
        (agent-repl-test-input--plan-answer (agent-repl-test-input--plan)) t
        (agent-repl-test-input--refusal (nth 0 case) (nth 1 case))
      ;; Act
      (agent-repl-rollback-keep-files)
      ;; Assert
      (should (equal (agent-repl-test-input--notice) (nth 2 case))))))

(ert-deftest agent-repl-test-input-rollback-roll-back-refusal-is-logged ()
  "A RollBack refusal is logged at WARN with its cause."
  (agent-repl-test-input--with-rollback
      (agent-repl-test-input--plan-answer (agent-repl-test-input--plan)) t
      (agent-repl-test-input--refusal :plan-stale)
    ;; Act
    (agent-repl-rollback-keep-files)
    ;; Assert
    (should (cl-some (lambda (m) (string-match-p "rollback-refused.*verb=roll-back.*plan-stale" m))
                     agent-repl-test-input--warned))))

(ert-deftest agent-repl-test-input-rollback-refused-leaves-the-composer ()
  "A refused rollback leaves the composer as it was."
  (agent-repl-test-input--with-rollback
      (agent-repl-test-input--plan-answer (agent-repl-test-input--plan)) t
      (agent-repl-test-input--refusal :first-prompt)
    ;; Arrange
    (agent-repl-test-input--type "my draft")
    ;; Act
    (agent-repl-rollback-keep-files)
    ;; Assert
    (should (equal (agent-repl-test-input--composer-text) "my draft"))))

(ert-deftest agent-repl-test-input-rollback-plan-refusal-flashes ()
  "A PlanRollback refusal flashes its arm in plain words."
  (agent-repl-test-input--with-rollback
      (agent-repl-test-input--refusal :not-yet-adopted) t nil
    ;; Act
    (agent-repl-rollback-keep-files)
    ;; Assert
    (should (equal (agent-repl-test-input--notice)
                   "rollback: the daemon is still taking this workspace over; press the key again"))))

(ert-deftest agent-repl-test-input-rollback-plan-refusal-asks-nothing ()
  "A refused plan asks no question."
  (agent-repl-test-input--with-rollback
      (agent-repl-test-input--refusal :unknown-workspace) t nil
    ;; Act
    (agent-repl-rollback-keep-files)
    ;; Assert
    (should-not agent-repl-test-input--asked)))

(ert-deftest agent-repl-test-input-rollback-plan-transport-failure-flashes ()
  "A plan the daemon never answered flashes the composer."
  (agent-repl-test-input--with-rollback
      '(:failure (:kind :transport :message "down")) t nil
    ;; Act
    (agent-repl-rollback-keep-files)
    ;; Assert
    (should (equal (agent-repl-test-input--notice) "rollback: the daemon did not answer"))))

(ert-deftest agent-repl-test-input-rollback-plan-transport-failure-is-logged ()
  "A plan the daemon never answered is logged at WARN with the detail."
  (agent-repl-test-input--with-rollback
      '(:failure (:kind :transport :message "down")) t nil
    ;; Act
    (agent-repl-rollback-keep-files)
    ;; Assert
    (should (cl-some (lambda (m) (string-match-p "rollback-failure.*verb=plan.*down" m))
                     agent-repl-test-input--warned))))

(ert-deftest agent-repl-test-input-rollback-roll-back-transport-failure-flashes ()
  "A RollBack the daemon never answered flashes the composer."
  (agent-repl-test-input--with-rollback
      (agent-repl-test-input--plan-answer (agent-repl-test-input--plan)) t
      '(:failure (:kind :transport :message "down"))
    ;; Act
    (agent-repl-rollback-keep-files)
    ;; Assert
    (should (equal (agent-repl-test-input--notice) "rollback: the daemon did not answer"))))

(ert-deftest agent-repl-test-input-rollback-without-a-ref-asks-nothing-of-the-daemon ()
  "A workspace not registered yet plans nothing, and says so."
  (agent-repl-test-input--with-rollback
      (agent-repl-test-input--plan-answer (agent-repl-test-input--plan)) t nil
    ;; Arrange
    (cl-letf (((symbol-function 'agent-repl-host-ref) (lambda (_ws) nil)))
      ;; Act
      (agent-repl-rollback-keep-files))
    ;; Assert
    (should (equal (list (agent-repl-test-input--notice) agent-repl-test-input--rollback-calls)
                   '("rollback: workspace not ready" nil)))))

(ert-deftest agent-repl-test-input-rollback-without-a-connection-says-so ()
  "No daemon connection plans nothing, and says so."
  (agent-repl-test-input--with-rollback
      (agent-repl-test-input--plan-answer (agent-repl-test-input--plan)) t nil
    ;; Arrange
    (cl-letf (((symbol-function 'agent-repl-host-conn) (lambda (_ws) nil))
              ((symbol-function 'agent-repl-link-primary) (lambda () nil)))
      ;; Act
      (agent-repl-rollback-keep-files))
    ;; Assert
    (should (equal (agent-repl-test-input--notice) "rollback: no daemon connection"))))

(ert-deftest agent-repl-test-input-rollback-unknown-plan-arm-is-an-error ()
  "A PlanRollback answer arm the composer does not know is logged at ERROR."
  (agent-repl-test-input--with-rollback
      '(:response (:arm :accepted :value nil)) t nil
    ;; Arrange
    (let ((errors nil))
      (cl-letf (((symbol-function 'agent-repl--error)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) errors))))
        ;; Act
        (agent-repl-rollback-keep-files))
      ;; Assert
      (should (cl-some (lambda (m) (string-match-p "rollback-unknown-arm.*verb=plan" m))
                       errors)))))

;;;; ---- The stop key's second press: the rollback offer -----------------

(defun agent-repl-test-input--entry (row excerpt &optional sent-at-ms tokens)
  "A decoded RollbackPromptEntry for ROW."
  (list :prompt (list :value row) :excerpt excerpt
        :sent-at-ms sent-at-ms :context-tokens tokens))

(defun agent-repl-test-input--listed (&rest entries)
  "A ListRollbackPrompts success answering ENTRIES."
  (list :response (list :arm :success :value (list :prompts entries))))

(defmacro agent-repl-test-input--picking (line &rest body)
  "Run BODY with `completing-read' answering the candidate starting with LINE.
The candidates offered are kept in `agent-repl-test-input--offered'."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'completing-read)
              (lambda (_prompt table &rest _)
                (setq agent-repl-test-input--offered (all-completions "" table))
                (cl-find-if (lambda (c) (string-prefix-p ,line c))
                            agent-repl-test-input--offered))))
     ,@body))

(defvar agent-repl-test-input--offered nil
  "The candidates the stubbed `completing-read' was offered.")

(ert-deftest agent-repl-test-input-rollback-offer-immediate-rolls-back-at-once ()
  "An immediate offer sends RollBack with its token and asks nothing."
  (agent-repl-test-input--with-rollback nil nil
      (agent-repl-test-input--done (agent-repl-test-input--said))
    ;; Act
    (agent-repl--input-rollback-offer "ws-one" '(:arm :immediate :value (:token (:value "tok-9"))))
    ;; Assert
    (should (equal (agent-repl-test-input--roll-back-sent)
                   (list :workspace agent-repl-test-input--ref :token '(:value "tok-9"))))
    (should-not agent-repl-test-input--asked)
    (should-not (assq :plan agent-repl-test-input--rollback-calls))))

(ert-deftest agent-repl-test-input-rollback-offer-immediate-refills-the-composer ()
  "The immediately rolled-back prompt lands in the composer."
  (agent-repl-test-input--with-rollback nil nil
      (agent-repl-test-input--done
       (agent-repl-test-input--said (agent-repl-test-input--text-block "fix it")))
    ;; Act
    (agent-repl--input-rollback-offer "ws-one" '(:arm :immediate :value (:token (:value "tok-9"))))
    ;; Assert
    (should (equal (agent-repl-test-input--composer-text) "fix it"))))

(ert-deftest agent-repl-test-input-rollback-offer-immediate-stale-flashes ()
  "A token the daemon finds stale flashes its refusal."
  (agent-repl-test-input--with-rollback nil nil
      (agent-repl-test-input--refusal :plan-stale)
    ;; Act
    (agent-repl--input-rollback-offer "ws-one" '(:arm :immediate :value (:token (:value "tok-9"))))
    ;; Assert
    (should (equal (agent-repl-test-input--notice)
                   "rollback: the conversation changed; press the key again"))))

(ert-deftest agent-repl-test-input-rollback-offer-immediate-without-a-ref-sends-nothing ()
  "A workspace not registered yet rolls nothing back, and says so."
  (agent-repl-test-input--with-rollback nil nil nil
    ;; Arrange
    (cl-letf (((symbol-function 'agent-repl-host-ref) (lambda (_ws) nil)))
      ;; Act
      (agent-repl--input-rollback-offer "ws-one" '(:arm :immediate :value (:token (:value "t")))))
    ;; Assert
    (should (equal (list (agent-repl-test-input--notice) agent-repl-test-input--rollback-calls)
                   '("rollback: workspace not ready" nil)))))

(ert-deftest agent-repl-test-input-rollback-offer-unknown-arm-is-an-error ()
  "An offer arm the composer does not know is logged at ERROR."
  (agent-repl-test-input--with-rollback nil nil nil
    (let ((errors nil))
      (cl-letf (((symbol-function 'agent-repl--error)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) errors))))
        ;; Act
        (agent-repl--input-rollback-offer "ws-one" '(:arm :later :value nil)))
      ;; Assert
      (should (cl-some (lambda (m) (string-match-p "rollback-unknown-offer.*:later" m)) errors))
      (should-not agent-repl-test-input--rollback-calls))))

(ert-deftest agent-repl-test-input-rollback-offer-choose-plans-the-pick-by-name ()
  "Choosing lists the prompts, and the pick is planned by naming its row."
  (agent-repl-test-input--with-rollback
      (agent-repl-test-input--plan-answer
       (agent-repl-test-input--plan :target '(:chosen :named :excerpt "older" :prompts-dropped 2)))
      t (agent-repl-test-input--done (agent-repl-test-input--said))
    (let ((agent-repl-test-input--list-answer
           (agent-repl-test-input--listed (agent-repl-test-input--entry "p2" "newer")
                                          (agent-repl-test-input--entry "p1" "older"))))
      (agent-repl-test-input--picking " 2"
        ;; Act
        (agent-repl--input-rollback-offer "ws-one" '(:arm :choose :value nil))))
    ;; Assert
    (should (equal (plist-get (cadr (assq :plan agent-repl-test-input--rollback-calls)) :prompt)
                   '(:value "p1")))
    (should (string-prefix-p "Roll back to before the chosen prompt \"older\""
                             agent-repl-test-input--asked))
    (should (agent-repl-test-input--roll-back-sent))))

(ert-deftest agent-repl-test-input-rollback-choose-offers-newest-first ()
  "The picker lists the prompts in the daemon's order, newest at the top."
  (agent-repl-test-input--with-rollback
      (agent-repl-test-input--plan-answer (agent-repl-test-input--plan)) nil nil
    (let ((agent-repl-test-input--list-answer
           (agent-repl-test-input--listed (agent-repl-test-input--entry "p2" "newer")
                                          (agent-repl-test-input--entry "p1" "older"))))
      (agent-repl-test-input--picking " 1"
        ;; Act
        (agent-repl--input-rollback-choose "ws-one")))
    ;; Assert
    (should (equal (mapcar (lambda (c) (car (last (split-string c "  +"))))
                           agent-repl-test-input--offered)
                   '("newer" "older")))))

(ert-deftest agent-repl-test-input-rollback-choose-with-nothing-listed-flashes ()
  "An empty list reads nothing and says there is no prompt."
  (agent-repl-test-input--with-rollback nil nil nil
    (let ((agent-repl-test-input--list-answer (agent-repl-test-input--listed))
          (read nil))
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) (setq read t) nil)))
        ;; Act
        (agent-repl--input-rollback-choose "ws-one"))
      ;; Assert
      (should-not read)
      (should (equal (agent-repl-test-input--notice) "rollback: no prompt to roll back to")))))

(ert-deftest agent-repl-test-input-rollback-choose-refusal-flashes-and-warns ()
  "A ListRollbackPrompts refusal flashes and is logged at WARN."
  (agent-repl-test-input--with-rollback nil nil nil
    (let ((agent-repl-test-input--list-answer
           (agent-repl-test-input--refusal :unknown-workspace)))
      ;; Act
      (agent-repl--input-rollback-choose "ws-one"))
    ;; Assert
    (should (equal (agent-repl-test-input--notice)
                   "rollback: the daemon does not know this workspace"))
    (should (cl-some (lambda (m) (string-match-p "rollback-refused.*verb=list" m))
                     agent-repl-test-input--warned))))

(ert-deftest agent-repl-test-input-rollback-choose-transport-failure-flashes ()
  "A ListRollbackPrompts the daemon never answers flashes and warns."
  (agent-repl-test-input--with-rollback nil nil nil
    (let ((agent-repl-test-input--list-answer '(:failure (:kind :transport :message "down"))))
      ;; Act
      (agent-repl--input-rollback-choose "ws-one"))
    ;; Assert
    (should (equal (agent-repl-test-input--notice) "rollback: the daemon did not answer"))
    (should (cl-some (lambda (m) (string-match-p "rollback-failure.*verb=list" m))
                     agent-repl-test-input--warned))))

(ert-deftest agent-repl-test-input-rollback-choose-unknown-arm-is-an-error ()
  "A ListRollbackPrompts answer arm the composer does not know is logged at ERROR."
  (agent-repl-test-input--with-rollback nil nil nil
    (let ((agent-repl-test-input--list-answer '(:response (:arm :accepted :value nil)))
          (errors nil))
      (cl-letf (((symbol-function 'agent-repl--error)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) errors))))
        ;; Act
        (agent-repl--input-rollback-choose "ws-one"))
      ;; Assert
      (should (cl-some (lambda (m) (string-match-p "rollback-unknown-arm.*verb=list" m)) errors)))))

(ert-deftest agent-repl-test-input-rollback-prompt-not-reachable-reads-plainly ()
  "A named prompt no longer reachable is said in plain words."
  (should (equal (agent-repl--input-rollback-refusal-text
                  '(:arm :prompt-not-reachable :value (:prompt (:value "p1"))))
                 "rollback: that prompt can no longer be rolled back to; press the key again")))

(ert-deftest agent-repl-test-input-rollback-tokens-text ()
  "The context column is short, and a missing figure is a dash."
  (dolist (case '((nil "-") (0 "0") (999 "999") (1000 "1k") (42400 "42k")
                  (999400 "999k") (1250000 "1.2M")))
    (should (equal (agent-repl--input-rollback-tokens-text (car case)) (cadr case)))))

(ert-deftest agent-repl-test-input-rollback-candidate-columns ()
  "A picker line is index, time since sent, context and excerpt."
  (let ((now 1000.0))
    (should (equal (agent-repl--input-rollback-candidate
                    3 (agent-repl-test-input--entry "p" "fix it" (* 1000 (- 1000 600)) 42000) now)
                   " 3  10m ago          42k  fix it"))))

(ert-deftest agent-repl-test-input-rollback-candidate-without-figures-shows-dashes ()
  "A prompt with no send time and no context figure shows dashes."
  (should (equal (agent-repl--input-rollback-candidate
                  1 (agent-repl-test-input--entry "p" "adopted") 1000.0)
                 " 1  -                  -  adopted")))

(ert-deftest agent-repl-test-input-rollback-read-entry-keeps-the-daemon-order ()
  "The completion table tells every backend not to re-sort it."
  (let (metadata)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt table &rest _)
                 (setq metadata (funcall table "" nil 'metadata))
                 (car (all-completions "" table)))))
      ;; Act
      (let ((entry (agent-repl--input-rollback-read-entry
                    (list (agent-repl-test-input--entry "p2" "b")
                          (agent-repl-test-input--entry "p1" "a")))))
        ;; Assert
        (should (equal (plist-get entry :prompt) '(:value "p2")))
        (should (eq (alist-get 'display-sort-function (cdr metadata)) 'identity))
        (should (eq (alist-get 'cycle-sort-function (cdr metadata)) 'identity))))))

(ert-deftest agent-repl-test-input-rollback-endpoints-answer-ref-and-conn ()
  "With a ref and a connection the guard answers both and flashes nothing."
  (agent-repl-test-input--with-rollback nil nil nil
    ;; Act
    (let ((endpoints (agent-repl--input-rollback-endpoints "ws-one" nil)))
      ;; Assert
      (should (equal (car endpoints) agent-repl-test-input--ref))
      (should (cdr endpoints))
      (should-not (agent-repl-test-input--notice)))))

(ert-deftest agent-repl-test-input-rollback-endpoints-without-a-connection-warn ()
  "No daemon connection is answered nil, flashed and logged at WARN."
  (agent-repl-test-input--with-rollback nil nil nil
    (cl-letf (((symbol-function 'agent-repl-host-conn) (lambda (_ws) nil))
              ((symbol-function 'agent-repl-link-primary) (lambda () nil)))
      ;; Act
      (should-not (agent-repl--input-rollback-endpoints "ws-one" '(:offer :choose))))
    ;; Assert
    (should (equal (agent-repl-test-input--notice) "rollback: no daemon connection"))
    (should (cl-some (lambda (m) (string-match-p "rollback-no-conn.*:choose" m))
                     agent-repl-test-input--warned))))

(ert-deftest agent-repl-test-input-rollback-every-entry-point-shares-the-guard ()
  "The planned, immediate and chosen rollbacks all send behind one guard."
  (agent-repl-test-input--with-rollback nil nil nil
    (let ((guarded nil))
      (cl-letf (((symbol-function 'agent-repl--input-rollback-endpoints)
                 (lambda (_ws context) (push context guarded) nil)))
        ;; Act
        (agent-repl-rollback-keep-files)
        (agent-repl--input-rollback-offer "ws-one" '(:arm :immediate :value (:token (:value "t"))))
        (agent-repl--input-rollback-choose "ws-one"))
      ;; Assert
      (should (= (length guarded) 3))
      (should-not agent-repl-test-input--rollback-calls))))

;;;; ---- Handover refusals route to host.el ------------------------------

(ert-deftest agent-repl-test-input-select-transferring-away-routes-the-handover ()
  "A SelectFeedRow `transferring_away' goes to host.el's handover walk."
  (agent-repl-test-input--with-select
      (agent-repl-test-input--refusal :transferring-away '(:address "127.0.0.1:9"))
    ;; Act
    (agent-repl-turn-boundary-select-prev)
    ;; Assert
    (should (equal agent-repl-test-input--refusals
                   '(("ws-one" (:arm :transferring-away :value (:address "127.0.0.1:9"))))))))

(ert-deftest agent-repl-test-input-select-handover-still-flashes ()
  "The routed move still did not happen, so the composer still says so."
  (agent-repl-test-input--with-select
      (agent-repl-test-input--refusal :not-yet-adopted)
    ;; Act
    (agent-repl-turn-boundary-select-prev)
    ;; Assert
    (should (equal (agent-repl-test-input--notice) "selection: selection refused"))))

(ert-deftest agent-repl-test-input-select-handover-is-not-a-warning ()
  "A handover refusal is news, never logged as a refused move."
  (agent-repl-test-input--with-select
      (agent-repl-test-input--refusal :not-yet-adopted)
    ;; Arrange
    (let ((warned nil))
      (cl-letf (((symbol-function 'agent-repl--warn)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) warned))))
        ;; Act
        (agent-repl-turn-boundary-select-prev))
      ;; Assert
      (should-not warned))))

(ert-deftest agent-repl-test-input-select-other-refusal-routes-nothing ()
  "A refusal that is not a handover is not handed to host.el."
  (agent-repl-test-input--with-select agent-repl-test-input--select-refused
    ;; Act
    (agent-repl-turn-boundary-select-prev)
    ;; Assert
    (should-not agent-repl-test-input--refusals)))

(ert-deftest agent-repl-test-input-plan-transferring-away-routes-the-handover ()
  "A PlanRollback `transferring_away' goes to host.el's handover walk."
  (agent-repl-test-input--with-rollback
      (agent-repl-test-input--refusal :transferring-away '(:address "127.0.0.1:9")) t nil
    ;; Act
    (agent-repl-rollback-keep-files)
    ;; Assert
    (should (equal agent-repl-test-input--refusals
                   '(("ws-one" (:arm :transferring-away :value (:address "127.0.0.1:9"))))))))

(ert-deftest agent-repl-test-input-plan-handover-is-not-a-warning ()
  "A PlanRollback handover refusal is not logged as a refused rollback."
  (agent-repl-test-input--with-rollback
      (agent-repl-test-input--refusal :not-yet-adopted) t nil
    ;; Act
    (agent-repl-rollback-keep-files)
    ;; Assert
    (should-not agent-repl-test-input--warned)))

(ert-deftest agent-repl-test-input-roll-back-not-yet-adopted-routes-the-handover ()
  "A RollBack `not_yet_adopted' goes to host.el's handover walk."
  (agent-repl-test-input--with-rollback
      (agent-repl-test-input--plan-answer (agent-repl-test-input--plan)) t
      (agent-repl-test-input--refusal :not-yet-adopted)
    ;; Act
    (agent-repl-rollback-keep-files)
    ;; Assert
    (should (equal agent-repl-test-input--refusals
                   '(("ws-one" (:arm :not-yet-adopted :value nil)))))))

(ert-deftest agent-repl-test-input-roll-back-handover-still-flashes ()
  "The routed rollback did not happen, so the composer still says so."
  (agent-repl-test-input--with-rollback
      (agent-repl-test-input--plan-answer (agent-repl-test-input--plan)) t
      (agent-repl-test-input--refusal :transferring-away '(:address "127.0.0.1:9"))
    ;; Act
    (agent-repl-rollback-keep-files)
    ;; Assert
    (should (equal (agent-repl-test-input--notice)
                   "rollback: the workspace is moving to another daemon; press the key again"))))

(ert-deftest agent-repl-test-input-roll-back-other-refusal-routes-nothing ()
  "A RollBack refusal that is not a handover is not handed to host.el."
  (agent-repl-test-input--with-rollback
      (agent-repl-test-input--plan-answer (agent-repl-test-input--plan)) t
      (agent-repl-test-input--refusal :plan-stale)
    ;; Act
    (agent-repl-rollback-keep-files)
    ;; Assert
    (should-not agent-repl-test-input--refusals)))

;;;; ---- The selection mode and the response view ----

(defvar agent-repl-test-input--host-kind nil
  "The selection kind the stubbed host watch reports.")

(defvar agent-repl-test-input--host-text nil
  "The selected bubble's text the stubbed host watch reports.")

(defvar agent-repl-test-input--searches nil
  "Which Evil searches a view test ran, newest first.")

(defmacro agent-repl-test-input--with-host-selection (kind text &rest body)
  "Run BODY in a composer whose host watch reports KIND selected with TEXT.
`markdown-mode' is stubbed to set the major mode, since `emacs -Q' has no
markdown package; the Evil searches record which one ran; the popup is
recorded; the response view is killed afterwards."
  (declare (indent 2))
  `(agent-repl-test-input--with
     (let ((agent-repl-test-input--host-kind ,kind)
           (agent-repl-test-input--host-text ,text)
           (agent-repl-test-input--searches nil))
       (unwind-protect
           (cl-letf (((symbol-function 'agent-repl-host-selection)
                      (lambda (_ws) agent-repl-test-input--host-kind))
                     ((symbol-function 'agent-repl-host-selection-markdown)
                      (lambda (_ws) agent-repl-test-input--host-text))
                     ((symbol-function 'markdown-mode)
                      (lambda () (setq major-mode 'markdown-mode)))
                     ((symbol-function 'evil-ex-search-forward)
                      (lambda () (interactive) (push :forward agent-repl-test-input--searches)))
                     ((symbol-function 'evil-ex-search-backward)
                      (lambda () (interactive) (push :backward agent-repl-test-input--searches))))
             (agent-repl-test--recording-popups ,@body))
         (when-let ((view (get-buffer (agent-repl--response-view-name "ws-one"))))
           (kill-buffer view))))))

(defun agent-repl-test-input--mode-on-p ()
  "Whether the composer's selection mode stands."
  (buffer-local-value 'agent-repl-input-selection-mode agent-repl-test-input--buffer))

(ert-deftest agent-repl-test-input-selection-changed-turns-the-mode-on ()
  "A selected bubble turns the composer's selection mode on."
  (agent-repl-test-input--with-host-selection :bubble "meanwhile"
    ;; Act
    (agent-repl--input-selection-changed "ws-one")
    ;; Assert
    (should (agent-repl-test-input--mode-on-p))))

(ert-deftest agent-repl-test-input-selection-changed-turns-the-mode-off ()
  "Nothing selected turns the composer's selection mode off."
  (agent-repl-test-input--with-host-selection :bubble "meanwhile"
    ;; Arrange
    (agent-repl--input-selection-changed "ws-one")
    (setq agent-repl-test-input--host-kind :none agent-repl-test-input--host-text nil)
    ;; Act
    (agent-repl--input-selection-changed "ws-one")
    ;; Assert
    (should-not (agent-repl-test-input--mode-on-p))))

(ert-deftest agent-repl-test-input-selection-changed-without-a-composer-is-logged ()
  "A workspace with no composer has nothing to follow, and says so."
  (agent-repl-test-input--with-host-selection :response "the answer"
    ;; Arrange
    (let (logged)
      (cl-letf (((symbol-function 'agent-repl--input-buffer) (lambda (_ws) nil))
                ((symbol-function 'agent-repl--log)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logged))))
        ;; Act
        (agent-repl--input-selection-changed "ws-one"))
      ;; Assert
      (should (cl-some (lambda (m) (string-match-p "selection-changed.*composer=nil" m)) logged)))))

(ert-deftest agent-repl-test-input-search-forward-shows-the-view-in-the-popup ()
  "`/' shows the selected bubble's view through the shared popup."
  (agent-repl-test-input--with-host-selection :response "# The answer"
    ;; Act
    (agent-repl-response-search-forward)
    ;; Assert
    (should (equal (mapcar #'buffer-name agent-repl-test--popups-shown)
                   (list (agent-repl--response-view-name "ws-one"))))))

(ert-deftest agent-repl-test-input-search-fills-the-view-with-the-markdown ()
  "The view holds the selected bubble's markdown source verbatim."
  (agent-repl-test-input--with-host-selection :prompt "what is\n\nthe capital?"
    ;; Act
    (agent-repl-response-search-forward)
    ;; Assert
    (should (equal (with-current-buffer (agent-repl--response-view-name "ws-one") (buffer-string))
                   "what is\n\nthe capital?"))))

(ert-deftest agent-repl-test-input-search-view-is-read-only-markdown ()
  "The view is read-only and in `markdown-mode'."
  (agent-repl-test-input--with-host-selection :bubble "meanwhile"
    ;; Act
    (agent-repl-response-search-forward)
    ;; Assert
    (with-current-buffer (agent-repl--response-view-name "ws-one")
      (should (equal (list buffer-read-only major-mode) '(t markdown-mode))))))

(ert-deftest agent-repl-test-input-search-forward-runs-evil-search-forward ()
  "`/' runs Evil's forward search in the view."
  (agent-repl-test-input--with-host-selection :bubble "meanwhile"
    ;; Act
    (agent-repl-response-search-forward)
    ;; Assert
    (should (equal agent-repl-test-input--searches '(:forward)))))

(ert-deftest agent-repl-test-input-search-backward-runs-evil-search-backward ()
  "`?' runs Evil's backward search in the view."
  (agent-repl-test-input--with-host-selection :bubble "meanwhile"
    ;; Act
    (agent-repl-response-search-backward)
    ;; Assert
    (should (equal agent-repl-test-input--searches '(:backward)))))

(ert-deftest agent-repl-test-input-a-new-selection-refills-an-open-view ()
  "A selection change refills an open view, so it never shows a stale bubble."
  (agent-repl-test-input--with-host-selection :bubble "meanwhile"
    ;; Arrange
    (agent-repl-response-search-forward)
    (setq agent-repl-test-input--host-kind :prompt agent-repl-test-input--host-text "a prompt")
    ;; Act
    (agent-repl--input-selection-changed "ws-one")
    ;; Assert
    (should (equal (with-current-buffer (agent-repl--response-view-name "ws-one") (buffer-string))
                   "a prompt"))))

(ert-deftest agent-repl-test-input-search-without-text-signals-and-logs ()
  "A search with no selected text is a broken invariant: it signals and logs."
  (agent-repl-test-input--with-host-selection :none nil
    ;; Arrange
    (let (errors)
      (cl-letf (((symbol-function 'agent-repl--error)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) errors))))
        ;; Act
        (should-error (agent-repl-response-search-forward))
        ;; Assert
        (should (cl-some (lambda (m) (string-match-p "response-view-no-text" m)) errors))))))

(defun agent-repl-test-input--evil-bindings ()
  "Run the selection mode's key binder; return every `evil-define-key*' call."
  (let (bound)
    (cl-letf (((symbol-function 'evil-define-key*)
               (lambda (state keymap &rest bindings)
                 (push (list state keymap bindings) bound))))
      (agent-repl--input-selection-bind-keys))
    (nreverse bound)))

(ert-deftest agent-repl-test-input-selection-mode-binds-slash-and-question-mark ()
  "`/' and `?' are bound in normal state on the selection mode's map."
  ;; Act
  (let ((bound (agent-repl-test-input--evil-bindings)))
    ;; Assert
    (should (member (list 'normal agent-repl-input-selection-mode-map
                          (list "/" #'agent-repl-response-search-forward
                                "?" #'agent-repl-response-search-backward))
                    bound))))

(ert-deftest agent-repl-test-input-selection-mode-binds-return-and-c-s-in-evil ()
  "`RET' and `C-s' are bound in normal and insert state on the selection mode's map."
  ;; Act
  (let ((bound (agent-repl-test-input--evil-bindings)))
    ;; Assert
    (should (member (list '(normal insert) agent-repl-input-selection-mode-map
                          (list (kbd "RET") #'agent-repl-input-selection-return
                                (kbd "C-s") #'agent-repl-response-search-buffer))
                    bound))))

(ert-deftest agent-repl-test-input-selection-mode-binds-return-in-emacs-state ()
  "`RET' acts on the selection in the plain map, which emacs state reads."
  (should (eq (lookup-key agent-repl-input-selection-mode-map (kbd "RET"))
              #'agent-repl-input-selection-return)))

(ert-deftest agent-repl-test-input-selection-mode-binds-c-s-in-emacs-state ()
  "`C-s' searches the selection in the plain map, which emacs state reads."
  (should (eq (lookup-key agent-repl-input-selection-mode-map (kbd "C-s"))
              #'agent-repl-response-search-buffer)))

(defmacro agent-repl-test-input--with-return-stubs (&rest body)
  "Run BODY with the rollback and send commands recording which one ran.
The record is bound as `ran'."
  (declare (indent 0))
  `(let (ran)
     (cl-letf (((symbol-function 'agent-repl-rollback-keep-files)
                (lambda () (interactive) (push :rollback-keep-files ran)))
               ((symbol-function 'agent-repl-send)
                (lambda () (interactive) (push :send ran))))
       ,@body)))

(ert-deftest agent-repl-test-input-selection-return-acts-on-the-selected-kind ()
  "`RET' rolls back to a selected prompt and sends a reply to anything else."
  (dolist (case '((:prompt :rollback-keep-files)
                  (:response :send)
                  (:bubble :send)))
    (agent-repl-test-input--with-host-selection (car case) "text"
      (agent-repl-test-input--with-return-stubs
        ;; Act
        (agent-repl-input-selection-return)
        ;; Assert
        (should (equal ran (cdr case)))))))

(ert-deftest agent-repl-test-input-selection-return-is-recorded-at-info ()
  "`RET' over a selection records which kind it acted on."
  (agent-repl-test-input--with-host-selection :prompt "a prompt"
    (agent-repl-test-input--with-return-stubs
      (agent-repl-test-input--capturing-rungs
        ;; Act
        (agent-repl-input-selection-return)
        ;; Assert
        (should (cl-some (lambda (m) (string-match-p "elisp.input.selection-return.*:prompt" m))
                         agent-repl-test-input--info))))))

(ert-deftest agent-repl-test-input-search-buffer-shows-the-view-in-the-popup ()
  "`C-s' shows the selected bubble's view through the shared popup."
  (agent-repl-test-input--with-host-selection :response "# The answer"
    (cl-letf (((symbol-function 'key-binding)
               (lambda (_key &rest _) (lambda () (interactive) nil))))
      ;; Act
      (agent-repl-response-search-buffer))
    ;; Assert
    (should (equal (mapcar #'buffer-name agent-repl-test--popups-shown)
                   (list (agent-repl--response-view-name "ws-one"))))))

(ert-deftest agent-repl-test-input-search-buffer-runs-c-s-in-the-view ()
  "`C-s' runs, in the view buffer, the command `C-s' is bound to there."
  (agent-repl-test-input--with-host-selection :prompt "a prompt"
    ;; Arrange
    (let (ran-in)
      (cl-letf (((symbol-function 'key-binding)
                 (lambda (_key &rest _)
                   (lambda () (interactive) (push (buffer-name) ran-in)))))
        ;; Act
        (agent-repl-response-search-buffer))
      ;; Assert
      (should (equal ran-in (list (agent-repl--response-view-name "ws-one")))))))

(ert-deftest agent-repl-test-input-search-buffer-with-c-s-unbound-signals-and-logs ()
  "An unbound `C-s' in the view is a broken invariant: it signals and logs."
  (agent-repl-test-input--with-host-selection :response "the answer"
    ;; Arrange
    (let (errors)
      (cl-letf (((symbol-function 'key-binding) (lambda (_key &rest _) nil))
                ((symbol-function 'agent-repl--error)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) errors))))
        ;; Act
        (should-error (agent-repl-response-search-buffer))
        ;; Assert
        (should (cl-some (lambda (m) (string-match-p "response-view-search-key-unbound" m))
                         errors))))))

(ert-deftest agent-repl-test-input-a-bubble-selection-is-active ()
  "A selected `:bubble' counts as a selection for escape and the mode."
  (agent-repl-test-input--with-host-selection :bubble "meanwhile"
    (should (agent-repl--input-selection-active-p "ws-one"))))



;;;; ---- Tests: escape closes the news digest ----

(ert-deftest agent-repl-test-input-escape-asks-the-page-to-close-its-digest ()
  "Command-state escape asks the host to close the workspace page's digest."
  (agent-repl-test--with-clean-state
    ;; Arrange
    (let ((asked nil))
      (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "ws"))
                ((symbol-function 'agent-repl-webhost-dismiss-digest) (lambda (ws) (push ws asked)))
                ((symbol-function 'agent-repl--input-selection-active-p) (lambda (_) nil))
                ((symbol-function 'agent-repl--input-escape-default) #'ignore))
        ;; Act
        (agent-repl-input-selection-escape)
        ;; Assert
        (should (equal asked '("ws")))))))

(ert-deftest agent-repl-test-input-insert-escape-closes-the-digest-then-leaves-insert ()
  "Insert-state escape asks for the close, then leaves insert state as ever."
  (agent-repl-test--with-clean-state
    ;; Arrange
    (let ((events nil))
      (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "ws"))
                ((symbol-function 'agent-repl-webhost-dismiss-digest) (lambda (&rest _) (push 'dismiss events)))
                ((symbol-function 'evil-normal-state) (lambda (&rest _) (push 'normal events))))
        ;; Act
        (agent-repl-input-insert-escape)
        ;; Assert
        (should (equal (reverse events) '(dismiss normal)))))))

(ert-deftest agent-repl-test-input-escape-without-a-workspace-asks-nothing ()
  "Standing in no workspace, there is no page to ask."
  (agent-repl-test--with-clean-state
    ;; Arrange
    (let ((asked nil))
      (cl-letf (((symbol-function 'agent-repl-webhost-dismiss-digest) (lambda (ws) (push ws asked))))
        ;; Act
        (agent-repl--input-dismiss-digest nil)
        ;; Assert
        (should-not asked)))))

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

;;;; ---- A deferred submission (SubmitPromptDelivery.DEFERRED) ----------

(defun agent-repl-test-input--submit-deferred ()
  "Submit one deferred prompt through the composer's ONE submit path."
  (agent-repl--input-submit "ws-one" (list :content (list :blocks nil))
                            :deferred-prompt "later" nil nil nil :deferred))

(ert-deftest agent-repl-input-a-deferred-submission-asks-for-its-delivery ()
  "The request carries `:delivery :deferred' for the codec to spell."
  (agent-repl-test-input--with
    ;; Act
    (agent-repl-test-input--submit-deferred)
    ;; Assert
    (should (eq (plist-get (car agent-repl-test-input--submitted) :delivery) :deferred))))

(ert-deftest agent-repl-input-an-ordinary-submission-names-no-delivery ()
  "An ordinary submission leaves the field absent: absence is ordinary."
  (agent-repl-test-input--with
    ;; Act
    (agent-repl--input-submit "ws-one" (list :content (list :blocks nil)) :user-sent "now")
    ;; Assert
    (should-not (plist-member (car agent-repl-test-input--submitted) :delivery))))

(ert-deftest agent-repl-input-a-deferred-submission-is-held-through-a-standing-refusal ()
  "A merge, a cold gate or a session coming up HOLD a deferral on disk.
An ordinary prompt is refused by each; a deferred one already asked for
later, so it goes to the ingress with its delivery and the daemon's retry
holds it until the condition clears."
  (dolist (arm '(:merging :cold-gate :no-session))
    (agent-repl-test-input--with
      ;; Arrange
      (setq agent-repl-test-input--answer
            `(:response (:arm :error :value (:reason (:arm ,arm :value nil)))))
      ;; Act
      (agent-repl-test-input--submit-deferred)
      ;; Assert
      (should (equal (length agent-repl-test-input--queued) 1))
      (should (eq (car agent-repl-test-input--held-deliveries) :deferred)))))

(ert-deftest agent-repl-input-a-deferred-hold-through-a-refusal-is-info ()
  "Holding a deferral through a standing condition is an answer, never a warning."
  (agent-repl-test-input--with
    ;; Arrange
    (setq agent-repl-test-input--answer
          '(:response (:arm :error :value (:reason (:arm :merging :value nil)))))
    (let ((warned nil))
      (cl-letf (((symbol-function 'agent-repl--warn)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) warned))))
        ;; Act
        (agent-repl-test-input--submit-deferred))
      ;; Assert
      (should-not warned))))

(ert-deftest agent-repl-input-a-deferred-duplicate-is-not-held ()
  "Only a standing condition holds a deferral: a duplicate is still the answer it is."
  (agent-repl-test-input--with
    ;; Arrange
    (setq agent-repl-test-input--answer
          '(:response (:arm :error :value (:reason (:arm :duplicate-submission :value nil)))))
    ;; Act
    (agent-repl-test-input--submit-deferred)
    ;; Assert
    (should-not agent-repl-test-input--queued)))

(ert-deftest agent-repl-input-a-deferred-transport-failure-is-held-deferred ()
  "A deferral the daemon never answered is held on disk still deferred."
  (agent-repl-test-input--with
    ;; Arrange
    (setq agent-repl-test-input--answer '(:failure (:kind :transport :message "gone")))
    ;; Act
    (agent-repl-test-input--submit-deferred)
    ;; Assert
    (should (eq (car agent-repl-test-input--held-deliveries) :deferred))))

(ert-deftest agent-repl-input-a-deferred-handover-refusal-is-held-deferred ()
  "A deferral refused by a handover is held on disk still deferred."
  (agent-repl-test-input--with
    ;; Arrange
    (setq agent-repl-test-input--answer
          '(:response (:arm :error :value (:reason (:arm :not-yet-adopted :value nil)))))
    ;; Act
    (agent-repl-test-input--submit-deferred)
    ;; Assert
    (should (eq (car agent-repl-test-input--held-deliveries) :deferred))))

;;; test-input.el ends here

;;;; ---- Unsent input across an Emacs restart ---------------------------

(defmacro agent-repl-test-input--with-composers (names &rest body)
  "Run BODY with a real composer and history directory for each of NAMES.
Inside BODY, `composers' is an alist (NAME . BUFFER).  The history
functions are the real ones, writing under a fresh temp directory per
workspace, so a save and a later restore meet on disk."
  (declare (indent 1))
  `(agent-repl-test--with-clean-state
     (let ((composers nil)
           (dirs nil))
       (unwind-protect
           (progn
             (dolist (name ,names)
               (let ((dir (make-temp-file "test-unsent-" t))
                     (buf (generate-new-buffer (format " *test-unsent-%s*" name))))
                 (push dir dirs)
                 (with-current-buffer buf
                   (agent-repl-input-mode)
                   (setq-local agent-repl--owning-workspace name))
                 (agent-repl--ws-put name :project-dir dir)
                 (agent-repl--ws-put name :input-buffer buf)
                 (push (cons name buf) composers)))
             (setq composers (nreverse composers))
             ,@body)
         (dolist (pair composers)
           (when (buffer-live-p (cdr pair)) (kill-buffer (cdr pair))))
         (dolist (dir dirs) (delete-directory dir t))))))

(defun agent-repl-test-input--reborn-composer (name)
  "Replace NAME's composer with a fresh one whose history is read from disk.
This is what an Emacs restart does to it.  Returns the new buffer."
  (let ((old (agent-repl--ws-get name :input-buffer))
        (buf (generate-new-buffer (format " *test-unsent-reborn-%s*" name))))
    (kill-buffer old)
    (with-current-buffer buf
      (agent-repl-input-mode)
      (setq-local agent-repl--owning-workspace name)
      (agent-repl--ws-put name :input-buffer buf)
      (agent-repl--history-restore name))
    buf))

(ert-deftest agent-repl-input-shutdown-saves-every-workspaces-composer ()
  "The shutdown save records each workspace's composer as its unsent head."
  (agent-repl-test-input--with-composers '("ws-a" "ws-b")
    (agent-repl-test-input--capturing-rungs
      ;; Arrange
      (with-current-buffer (alist-get "ws-a" composers nil nil #'equal) (insert "draft a"))
      (with-current-buffer (alist-get "ws-b" composers nil nil #'equal) (insert "draft b"))
      ;; Act
      (agent-repl-input-save-all-on-shutdown)
      ;; Assert
      (should (equal (with-current-buffer (alist-get "ws-a" composers nil nil #'equal)
                       (agent-repl--history-unsent-head))
                     "draft a"))
      (should (equal (with-current-buffer (alist-get "ws-b" composers nil nil #'equal)
                       (agent-repl--history-unsent-head))
                     "draft b")))))

(ert-deftest agent-repl-input-shutdown-save-goes-through-the-discard-save ()
  "The shutdown save is the discard's own save, not a copy of it."
  (agent-repl-test-input--with-composers '("ws-a")
    (agent-repl-test-input--capturing-rungs
      (let ((calls nil))
        ;; Arrange
        (with-current-buffer (cdar composers) (insert "draft"))
        (cl-letf (((symbol-function 'agent-repl--input-save-to-history)
                   (lambda (ws discard) (push (list ws discard) calls))))
          ;; Act
          (agent-repl-input-save-all-on-shutdown))
        ;; Assert
        (should (equal calls '(("ws-a" t))))))))

(ert-deftest agent-repl-input-shutdown-save-skips-an-empty-composer ()
  "An empty composer records nothing at shutdown."
  (agent-repl-test-input--with-composers '("ws-a")
    (agent-repl-test-input--capturing-rungs
      ;; Act
      (should (eq (agent-repl--input-save-on-shutdown "ws-a") 'empty))
      ;; Assert
      (should (null (buffer-local-value 'agent-repl--input-history (cdar composers))))
      (should (agent-repl-test-input--rung-has-p
               agent-repl-test-input--debug "elisp.input.shutdown-save ws=ws-a outcome=empty")))))

(ert-deftest agent-repl-input-shutdown-save-skips-a-whitespace-only-composer ()
  "A composer holding only whitespace counts as empty."
  (agent-repl-test-input--with-composers '("ws-a")
    (agent-repl-test-input--capturing-rungs
      ;; Arrange
      (with-current-buffer (cdar composers) (insert "  \n "))
      ;; Act, Assert
      (should (eq (agent-repl--input-save-on-shutdown "ws-a") 'empty)))))

(ert-deftest agent-repl-input-shutdown-save-skips-a-workspace-with-no-composer ()
  "A workspace whose composer is gone has nothing to save."
  (agent-repl-test-input--with-composers '("ws-a")
    (agent-repl-test-input--capturing-rungs
      ;; Arrange
      (kill-buffer (cdar composers))
      ;; Act, Assert
      (should (eq (agent-repl--input-save-on-shutdown "ws-a") 'no-composer)))))

(ert-deftest agent-repl-input-shutdown-save-failure-is-logged-and-the-rest-saved ()
  "One workspace failing to save is an error record; the next still saves."
  (agent-repl-test-input--with-composers '("ws-a" "ws-b")
    (agent-repl-test-input--capturing-rungs
      ;; Arrange: ws-a has lost its project directory, so its save errors.
      (with-current-buffer (alist-get "ws-a" composers nil nil #'equal) (insert "draft a"))
      (with-current-buffer (alist-get "ws-b" composers nil nil #'equal) (insert "draft b"))
      (agent-repl--ws-put "ws-a" :project-dir nil)
      ;; Act
      (agent-repl-input-save-all-on-shutdown)
      ;; Assert
      (should (agent-repl-test-input--rung-has-p
               agent-repl-test-input--error "elisp.input.shutdown-save-failed ws=ws-a"))
      (should (equal (with-current-buffer (alist-get "ws-b" composers nil nil #'equal)
                       (agent-repl--history-unsent-head))
                     "draft b")))))

(ert-deftest agent-repl-input-restore-unsent-fills-the-reborn-composer ()
  "What the composer held at shutdown is in it again after a restart."
  (agent-repl-test-input--with-composers '("ws-a")
    (agent-repl-test-input--capturing-rungs
      ;; Arrange
      (with-current-buffer (cdar composers) (insert "half a thought"))
      (agent-repl-input-save-all-on-shutdown)
      (let ((buf (agent-repl-test-input--reborn-composer "ws-a")))
        ;; Act
        (should (eq (agent-repl--input-restore-unsent "ws-a") 'restored))
        ;; Assert
        (should (equal (with-current-buffer buf (buffer-string)) "half a thought"))))))

(ert-deftest agent-repl-input-restore-unsent-sends-nothing ()
  "Putting the unsent words back never submits them."
  (agent-repl-test-input--with-composers '("ws-a")
    (agent-repl-test-input--capturing-rungs
      (let ((submits nil))
        ;; Arrange
        (with-current-buffer (cdar composers) (insert "half a thought"))
        (agent-repl-input-save-all-on-shutdown)
        (agent-repl-test-input--reborn-composer "ws-a")
        (cl-letf (((symbol-function 'agent-repl--input-submit)
                   (lambda (&rest args) (push args submits))))
          ;; Act
          (agent-repl--input-restore-unsent "ws-a"))
        ;; Assert
        (should (null submits))))))

(ert-deftest agent-repl-input-restore-unsent-leaves-standing-text-alone ()
  "A composer that already holds text is not overwritten, and that is logged."
  (agent-repl-test-input--with-composers '("ws-a")
    (agent-repl-test-input--capturing-rungs
      ;; Arrange
      (with-current-buffer (cdar composers) (insert "the old draft"))
      (agent-repl-input-save-all-on-shutdown)
      (let ((buf (agent-repl-test-input--reborn-composer "ws-a")))
        (with-current-buffer buf (insert "typed since"))
        ;; Act
        (should (eq (agent-repl--input-restore-unsent "ws-a") 'occupied))
        ;; Assert
        (should (equal (with-current-buffer buf (buffer-string)) "typed since"))
        (should (agent-repl-test-input--rung-has-p
                 agent-repl-test-input--info "elisp.input.restore-unsent ws=ws-a outcome=occupied"))))))

(ert-deftest agent-repl-input-restore-unsent-never-restores-a-sent-prompt ()
  "A discard later sent word for word is recorded, so it is never put back."
  (agent-repl-test-input--with-composers '("ws-a")
    (agent-repl-test-input--capturing-rungs
      ;; Arrange: discard, then send the same words (the send's history push).
      (with-current-buffer (cdar composers)
        (insert "do the thing")
        (agent-repl-discard-input)
        (agent-repl--history-push "do the thing")
        (agent-repl--history-save "ws-a"))
      (let ((buf (agent-repl-test-input--reborn-composer "ws-a")))
        ;; Act
        (should (eq (agent-repl--input-restore-unsent "ws-a") 'none))
        ;; Assert
        (should (equal (with-current-buffer buf (buffer-string)) ""))))))

(ert-deftest agent-repl-input-restore-unsent-skips-a-discard-buried-by-a-send ()
  "A discard followed by a different sent prompt is no longer the unsent head."
  (agent-repl-test-input--with-composers '("ws-a")
    (agent-repl-test-input--capturing-rungs
      ;; Arrange
      (with-current-buffer (cdar composers)
        (insert "abandoned")
        (agent-repl-discard-input)
        (agent-repl--history-push "something else")
        (agent-repl--history-save "ws-a"))
      (agent-repl-test-input--reborn-composer "ws-a")
      ;; Act, Assert
      (should (eq (agent-repl--input-restore-unsent "ws-a") 'none)))))

(ert-deftest agent-repl-input-restore-unsent-with-no-composer-warns ()
  "Asked to restore into a composer that is gone: a warning, nothing else."
  (agent-repl-test-input--with-composers '("ws-a")
    (agent-repl-test-input--capturing-rungs
      ;; Arrange
      (kill-buffer (cdar composers))
      ;; Act
      (should (eq (agent-repl--input-restore-unsent "ws-a") 'no-composer))
      ;; Assert
      (should (agent-repl-test-input--rung-has-p
               agent-repl-test-input--warn "elisp.input.restore-unsent ws=ws-a outcome=no-composer")))))

(ert-deftest agent-repl-input-refusal-restore-puts-the-unsent-marker-back ()
  "A refused send of the unsent words leaves them unsent, as before the send."
  (agent-repl-test-input--with-composers '("ws-a")
    (agent-repl-test-input--capturing-rungs
      (let ((buf (cdar composers)))
        ;; Arrange
        (cl-letf (((symbol-function 'agent-repl-input-clear-attachments) #'ignore))
          (with-current-buffer buf
            (insert "retry me")
            (agent-repl-discard-input)
            (insert "retry me"))
          (let ((snapshot (agent-repl--input-optimistic-clear "ws-a" "retry me")))
            ;; Act
            (agent-repl--input-restore "ws-a" snapshot)))
        ;; Assert
        (should (equal (with-current-buffer buf (agent-repl--history-unsent-head))
                       "retry me"))))))

(ert-deftest agent-repl-test-input-mode-installs-slash-command-completion ()
  "The composer's mode puts slash-command completion on its own capf list."
  (with-temp-buffer
    (agent-repl-input-mode)
    (should (memq #'agent-repl-command-completion-at-point completion-at-point-functions))
    (should (local-variable-p 'completion-at-point-functions))))

;;;; ---- The composer verb call (`agent-repl--input-call') -----------------

(defvar agent-repl-test-input--call-log nil
  "Every (LEVEL . MESSAGE) the stubbed logging ladder received, newest first.")

(defvar agent-repl-test-input--verb-answer nil
  "What the stubbed verb answers: `(:response PLIST)' or `(:failure DETAIL)'.")

(defmacro agent-repl-test-input--with-call (&rest body)
  "Run BODY in a composer with the logging ladder recorded.
The stubbed verb answers `agent-repl-test-input--verb-answer' and pushes
its requests onto `agent-repl-test-input--select-requests'."
  (declare (indent 0))
  `(agent-repl-test-input--with
     (let ((agent-repl-test-input--select-requests nil)
           (agent-repl-test-input--call-log nil))
       (cl-letf (((symbol-function 'agent-repl--warn)
                  (lambda (_ws fmt &rest args)
                    (push (cons :warn (apply #'format fmt args)) agent-repl-test-input--call-log)))
                 ((symbol-function 'agent-repl--error)
                  (lambda (_ws fmt &rest args)
                    (push (cons :error (apply #'format fmt args)) agent-repl-test-input--call-log)))
                 ((symbol-function 'agent-repl--info)
                  (lambda (_ws fmt &rest args)
                    (push (cons :info (apply #'format fmt args)) agent-repl-test-input--call-log))))
         ,@body))))

(defun agent-repl-test-input--stub-verb (conn request &rest keys)
  "The stubbed verb: record CONN's REQUEST and answer per the bound `answer'."
  (ignore conn)
  (push request agent-repl-test-input--select-requests)
  (pcase (car agent-repl-test-input--verb-answer)
    (:response (funcall (plist-get keys :on-response) (cadr agent-repl-test-input--verb-answer)))
    (:failure (funcall (plist-get keys :on-failure) (cadr agent-repl-test-input--verb-answer)))))

(defun agent-repl-test-input--call (&optional on-success)
  "Call the stubbed verb `probe' through `agent-repl--input-call'.
ON-SUCCESS receives the success value; it defaults to ignoring it."
  (agent-repl--input-call
   "ws-one" :op "probe" :prefix "probe" :detail-name "what" :detail 'thing
   :rpc #'agent-repl-test-input--stub-verb
   :request (lambda (ref) (list :workspace ref))
   :on-success (or on-success #'ignore)
   :on-refused (lambda (cause) (format "refused %s" (plist-get cause :arm)))))

(defun agent-repl-test-input--call-logged-p (level regexp)
  "Return non-nil when a LEVEL record matching REGEXP was logged."
  (cl-some (lambda (entry) (and (eq (car entry) level) (string-match-p regexp (cdr entry))))
           agent-repl-test-input--call-log))

(ert-deftest agent-repl-test-input-call-sends-the-request-for-the-ref ()
  "The request is built from the daemon-minted ref and the send is logged."
  (agent-repl-test-input--with-call
    (let ((agent-repl-test-input--verb-answer '(:response (:arm :success :value nil))))
      (agent-repl-test-input--call))
    (should (equal agent-repl-test-input--select-requests
                   (list (list :workspace agent-repl-test-input--ref))))
    (should (agent-repl-test-input--call-logged-p :info "elisp.input.probe ws=ws-one what=thing"))))

(ert-deftest agent-repl-test-input-call-hands-the-success-value-on ()
  "A success's value reaches ON-SUCCESS and the composer stays quiet."
  (agent-repl-test-input--with-call
    (let ((got nil))
      (let ((agent-repl-test-input--verb-answer '(:response (:arm :success :value (:outcome 7)))))
        (agent-repl-test-input--call (lambda (value) (setq got value))))
      (should (equal got '(:outcome 7)))
      (should-not (agent-repl-test-input--notice)))))

(ert-deftest agent-repl-test-input-call-refusal-is-logged-and-flashed ()
  "A refusal is logged at WARN with its cause and flashes ON-REFUSED's text."
  (agent-repl-test-input--with-call
    (let ((agent-repl-test-input--verb-answer '(:response (:arm :error :value (:cause (:arm :ask-not-standing :value nil))))))
      (agent-repl-test-input--call))
    (should (agent-repl-test-input--call-logged-p :warn "elisp.input.probe-refused .*what=thing.*ask-not-standing"))
    (should (equal (agent-repl-test-input--notice) "probe: refused :ask-not-standing"))))

(ert-deftest agent-repl-test-input-call-handover-refusal-is-routed-not-warned ()
  "A handover arm goes to the handover walk, never logged as a refusal."
  (agent-repl-test-input--with-call
    (let ((routed nil))
      (cl-letf (((symbol-function 'agent-repl-host-route-handover)
                 (lambda (_ws cause slug) (setq routed (list cause slug)) t)))
        (let ((agent-repl-test-input--verb-answer '(:response (:arm :error :value (:cause (:arm :transferring-away :value nil))))))
          (agent-repl-test-input--call)))
      (should (equal routed '((:arm :transferring-away :value nil) "elisp.input.probe-handover-refusal")))
      (should-not (agent-repl-test-input--call-logged-p :warn "probe-refused")))))

(ert-deftest agent-repl-test-input-call-unknown-arm-is-an-error ()
  "An answer arm the composer does not know is logged at ERROR."
  (agent-repl-test-input--with-call
    (let ((agent-repl-test-input--verb-answer '(:response (:arm :accepted :value nil))))
      (agent-repl-test-input--call))
    (should (agent-repl-test-input--call-logged-p :error "elisp.input.probe-unknown-arm .*:accepted"))))

(ert-deftest agent-repl-test-input-call-failure-is-logged-and-flashed ()
  "A transport failure is logged at WARN with the detail and flashed."
  (agent-repl-test-input--with-call
    (let ((agent-repl-test-input--verb-answer '(:failure (:kind :transport :message "down"))))
      (agent-repl-test-input--call))
    (should (agent-repl-test-input--call-logged-p :warn "elisp.input.probe-failure .*down"))
    (should (equal (agent-repl-test-input--notice) "probe: the daemon did not answer"))))

(ert-deftest agent-repl-test-input-call-without-a-ref-sends-nothing ()
  "No daemon-minted ref: nothing is sent, the miss is logged and flashed."
  (agent-repl-test-input--with-call
    (cl-letf (((symbol-function 'agent-repl-host-ref) (lambda (_ws) nil)))
      (let ((agent-repl-test-input--verb-answer '(:response (:arm :success :value nil))))
        (agent-repl-test-input--call)))
    (should-not agent-repl-test-input--select-requests)
    (should (agent-repl-test-input--call-logged-p :warn "elisp.input.probe-no-ref"))
    (should (equal (agent-repl-test-input--notice) "probe: workspace not ready"))))

(ert-deftest agent-repl-test-input-call-without-a-connection-sends-nothing ()
  "No connection: nothing is sent, the miss is logged and flashed."
  (agent-repl-test-input--with-call
    (cl-letf (((symbol-function 'agent-repl-host-conn-or-primary) (lambda (_ws) nil)))
      (let ((agent-repl-test-input--verb-answer '(:response (:arm :success :value nil))))
        (agent-repl-test-input--call)))
    (should-not agent-repl-test-input--select-requests)
    (should (agent-repl-test-input--call-logged-p :warn "elisp.input.probe-no-conn"))
    (should (equal (agent-repl-test-input--notice) "probe: no daemon connection"))))

(ert-deftest agent-repl-test-input-every-composer-verb-goes-through-the-one-call ()
  "No send site in input.el hand-rolls the verb call's handover routing."
  (let ((source (with-temp-buffer
                  (insert-file-contents (expand-file-name "input.el" agent-repl-test--module-dir))
                  (buffer-string)))
        (count 0) (start 0))
    (while (string-match "(agent-repl-host-route-handover" source start)
      (setq count (1+ count) start (match-end 0)))
    (should (= count 1))))

;;;; ---- Numbered answers ---------------------------------------------------

(defvar agent-repl-test-input--standing-ask nil
  "The `HostAsk' plist the stubbed host view reports for the composer's workspace.")

(defvar agent-repl-test-input--answer-requests nil
  "Every (VERB . REQUEST) a stubbed answer rpc received, newest first.")

(defun agent-repl-test-input--question-ask (selection labels)
  "A standing question ask whose shown question is SELECTION over LABELS."
  (list :row '(:value "row-q")
        :kind (list :arm :question
                    :value (list :question-text "Which?"
                                 :selection (list :arm selection
                                                  :value (list :options
                                                               (mapcar (lambda (l) (list :label l)) labels)))))))

(defun agent-repl-test-input--permission-ask (standing)
  "A standing permission ask, offering a standing form when STANDING."
  (list :row '(:value "row-p")
        :kind (list :arm :permission :value (list :standing-offered standing))))

(defmacro agent-repl-test-input--with-ask (ask &rest body)
  "Run BODY in a composer whose standing ask is ASK and whose answer rpcs succeed."
  (declare (indent 1))
  `(agent-repl-test-input--with
     (let ((agent-repl-test-input--standing-ask ,ask)
           (agent-repl-test-input--answer-requests nil)
           (agent-repl-test-input--verb-answer '(:response (:arm :success :value nil))))
       (cl-letf (((symbol-function 'agent-repl-host-standing-ask)
                  (lambda (_ws) agent-repl-test-input--standing-ask))
                 ((symbol-function 'agent-repl-rpc-update-question-draft)
                  (lambda (conn request &rest keys)
                    (push (cons :update-question-draft request) agent-repl-test-input--answer-requests)
                    (apply #'agent-repl-test-input--stub-verb conn request keys)))
                 ((symbol-function 'agent-repl-rpc-answer-permission)
                  (lambda (conn request &rest keys)
                    (push (cons :answer-permission request) agent-repl-test-input--answer-requests)
                    (apply #'agent-repl-test-input--stub-verb conn request keys))))
         (with-current-buffer agent-repl-test-input--buffer
           ,@body)))))

(defun agent-repl-test-input--type-digit (char)
  "Type CHAR into the current composer through the digit command."
  (let ((last-command-event char))
    (agent-repl-input-digit)))

(ert-deftest agent-repl-test-input-permission-buttons-follow-the-drawn-order ()
  "Allow once, always allow only when offered, then deny."
  (should (equal (agent-repl--input-permission-buttons '(:standing-offered nil))
                 '(:allow-once :deny)))
  (should (equal (agent-repl--input-permission-buttons '(:standing-offered t))
                 '(:allow-once :allow-standing :deny))))

(ert-deftest agent-repl-test-input-question-choices-are-its-options-in-order ()
  "A question's choices are picks of its options, in the served order."
  (should (equal (agent-repl--input-ask-choices
                  (agent-repl-test-input--question-ask :single-select '("a" "b")))
                 '((:verb :pick :row (:value "row-q") :question-text "Which?" :label "a")
                   (:verb :pick :row (:value "row-q") :question-text "Which?" :label "b")))))

(ert-deftest agent-repl-test-input-permission-choices-are-its-buttons ()
  "A permission's choices are its buttons, naming the card's row."
  (should (equal (agent-repl--input-ask-choices (agent-repl-test-input--permission-ask t))
                 '((:verb :permission :row (:value "row-p") :answer :allow-once)
                   (:verb :permission :row (:value "row-p") :answer :allow-standing)
                   (:verb :permission :row (:value "row-p") :answer :deny)))))

(ert-deftest agent-repl-test-input-an-unknown-ask-kind-fails-loudly ()
  "An ask kind the composer cannot number is an error, never silence."
  (should-error (agent-repl--input-ask-choices '(:row (:value "r") :kind (:arm :survey :value nil)))))

(ert-deftest agent-repl-test-input-numbered-choice-cases ()
  "A digit names a choice only from a blank composer, in range, with an ask standing."
  (dolist (case `(("no ask" nil "" ?1 nil)
                  ("in range" ,(agent-repl-test-input--question-ask :single-select '("a" "b")) "" ?2 "b")
                  ("after whitespace" ,(agent-repl-test-input--question-ask :single-select '("a" "b")) "  \n " ?1 "a")
                  ("after text" ,(agent-repl-test-input--question-ask :single-select '("a" "b")) "x" ?1 nil)
                  ("out of range" ,(agent-repl-test-input--question-ask :single-select '("a" "b")) "" ?3 nil)
                  ("zero" ,(agent-repl-test-input--question-ask :single-select '("a" "b")) "" ?0 nil)
                  ("not a digit" ,(agent-repl-test-input--question-ask :single-select '("a" "b")) "" ?a nil)))
    (pcase-let ((`(,name ,ask ,text ,char ,want) case))
      (agent-repl-test-input--with-ask ask
        (insert text)
        (should (equal (list name (plist-get (agent-repl--input-numbered-choice "ws-one" char) :label))
                       (list name want)))))))

(ert-deftest agent-repl-test-input-a-digit-answers-a-single-select-at-once ()
  "A digit in an empty composer picks the option, sends nothing else, inserts nothing."
  (agent-repl-test-input--with-ask (agent-repl-test-input--question-ask :single-select '("a" "b"))
    (agent-repl-test-input--type-digit ?2)
    (should (equal agent-repl-test-input--answer-requests
                   `((:update-question-draft
                      :workspace ,agent-repl-test-input--ref :question (:value "row-q")
                      :edit (:arm :pick :value (:question-text "Which?" :label "b"))))))
    (should (equal (agent-repl-test-input--composer-text) ""))
    (should-not agent-repl-test-input--submitted)))

(ert-deftest agent-repl-test-input-a-digit-toggles-a-multi-select-option ()
  "On a multi-select the digit is the same pick, which the daemon reads as a toggle."
  (agent-repl-test-input--with-ask (agent-repl-test-input--question-ask :multi-select '("a" "b"))
    (agent-repl-test-input--type-digit ?1)
    (should (equal (plist-get (cdar agent-repl-test-input--answer-requests) :edit)
                   '(:arm :pick :value (:question-text "Which?" :label "a"))))))

(ert-deftest agent-repl-test-input-a-digit-presses-a-permission-button ()
  "On a permission the digit presses the button in drawn order."
  (dolist (case '((nil ?2 :deny) (t ?2 :allow-standing) (t ?3 :deny) (nil ?1 :allow-once)))
    (pcase-let ((`(,standing ,char ,want) case))
      (agent-repl-test-input--with-ask (agent-repl-test-input--permission-ask standing)
        (agent-repl-test-input--type-digit char)
        (should (equal agent-repl-test-input--answer-requests
                       `((:answer-permission
                          :workspace ,agent-repl-test-input--ref :permission (:value "row-p")
                          :answer ,want))))))))

(ert-deftest agent-repl-test-input-a-digit-with-no-card-is-typed ()
  "With no card standing the digit is inserted and nothing is sent."
  (agent-repl-test-input--with-ask nil
    (agent-repl-test-input--type-digit ?1)
    (should (equal (agent-repl-test-input--composer-text) "1"))
    (should-not agent-repl-test-input--answer-requests)))

(ert-deftest agent-repl-test-input-a-digit-after-text-is-typed ()
  "A digit after other text is part of the text, even with a card standing."
  (agent-repl-test-input--with-ask (agent-repl-test-input--question-ask :single-select '("a" "b"))
    (insert "x")
    (agent-repl-test-input--type-digit ?1)
    (should (equal (agent-repl-test-input--composer-text) "x1"))
    (should-not agent-repl-test-input--answer-requests)))

(ert-deftest agent-repl-test-input-a-digit-out-of-range-is-typed ()
  "A digit naming no choice of the card is typed."
  (agent-repl-test-input--with-ask (agent-repl-test-input--permission-ask nil)
    (agent-repl-test-input--type-digit ?3)
    (should (equal (agent-repl-test-input--composer-text) "3"))
    (should-not agent-repl-test-input--answer-requests)))

(ert-deftest agent-repl-test-input-control-return-confirms-a-multi-select ()
  "`C-RET' from an empty composer confirms the shown multi-select."
  (agent-repl-test-input--with-ask (agent-repl-test-input--question-ask :multi-select '("a" "b"))
    (agent-repl-input-control-return)
    (should (equal agent-repl-test-input--answer-requests
                   `((:update-question-draft
                      :workspace ,agent-repl-test-input--ref :question (:value "row-q")
                      :edit (:arm :confirm :value (:question-text "Which?"))))))
    (should-not agent-repl-test-input--submitted)))

(ert-deftest agent-repl-test-input-control-return-otherwise-sends-with-postfix ()
  "`C-RET' with no multi-select standing, or with text, is the postfix send."
  (dolist (case `((,(agent-repl-test-input--question-ask :single-select '("a")) "")
                  (,(agent-repl-test-input--permission-ask nil) "")
                  (,(agent-repl-test-input--question-ask :multi-select '("a")) "words")
                  (nil "")))
    (pcase-let ((`(,ask ,text) case))
      (agent-repl-test-input--with-ask ask
        (insert text)
        (let ((sent nil))
          (cl-letf (((symbol-function 'agent-repl-send-with-postfix) (lambda () (setq sent t))))
            (agent-repl-input-control-return))
          (should sent)
          (should-not agent-repl-test-input--answer-requests))))))

(ert-deftest agent-repl-test-input-a-settled-card-refusal-says-so ()
  "A card that settled before the answer is refused, logged and flashed."
  (agent-repl-test-input--with-ask (agent-repl-test-input--question-ask :single-select '("a"))
    (let ((agent-repl-test-input--verb-answer
           '(:response (:arm :error :value (:cause (:arm :ask-not-standing :value nil)))))
          (warned nil))
      (cl-letf (((symbol-function 'agent-repl--warn)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) warned))))
        (agent-repl-test-input--type-digit ?1))
      (should (cl-some (lambda (m) (string-match-p "elisp.input.answer-question-refused .*:label \"a\".*ask-not-standing" m))
                       warned))
      (should (equal (agent-repl-test-input--notice) "answer: the card was already answered")))))

(ert-deftest agent-repl-test-input-answer-refusal-texts ()
  "Each refusal a numbered answer can meet has its own sentence."
  (should (equal (mapcar (lambda (arm) (agent-repl--input-answer-refusal-text (list :arm arm :value nil)))
                         '(:ask-not-standing :unserved-value :no-standing-offer :no-session :other))
                 '("the card was already answered" "the card no longer offers that choice"
                   "the card offers no always allow" "the workspace has no session"
                   "answer refused"))))

(ert-deftest agent-repl-test-input-an-answer-of-no-verb-fails-loudly ()
  "A numbered answer with a verb the composer has no rpc for is an error."
  (should-error (agent-repl--input-answer-choice "ws-one" '(:verb :survey :row (:value "r")))))

(ert-deftest agent-repl-test-input-digits-are-bound-to-the-numbered-answer ()
  "1-9 run the numbered answer in the composer; 0 stays an ordinary key."
  (dolist (digit (number-sequence ?1 ?9))
    (should (eq (lookup-key agent-repl-input-mode-map (string digit)) #'agent-repl-input-digit)))
  (should-not (eq (lookup-key agent-repl-input-mode-map "0") #'agent-repl-input-digit)))

(ert-deftest agent-repl-test-input-control-return-is-bound ()
  "`C-RET' runs the confirm-or-postfix command in the composer."
  (should (eq (lookup-key agent-repl-input-mode-map (kbd "C-<return>"))
              #'agent-repl-input-control-return)))
