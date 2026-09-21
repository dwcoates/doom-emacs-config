;;; test-conversations.el --- ERT tests for agent-repl conversations.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;   AGENT_REPL_FORBID_VENDOR_CALLS=1 emacs -batch -Q -l ert \
;;     -l lisp/test-conversations.el -f ert-run-tests-batch-and-exit
;;
;; Two halves, tested apart because they fail apart:
;;
;;   - the CANDIDATE RENDERING is pure.  It takes decoded transcripts and an
;;     instant and returns completion lines, so every case -- the opening
;;     words, the age, the size, the three markers -- is one call with no rpc
;;     anywhere.
;;   - the COMMAND is the rpc path.  The list verb is stubbed synchronously and
;;     the bind verb with the same record-and-answer stub the other verb suites
;;     use, so each refusal arm is exercised deterministically with no daemon.

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                           (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Fixtures ----

(defconst agent-repl-test-conversations--now (* 1000 1700000000)
  "The instant every rendering case is read at, in epoch milliseconds.")

(defun agent-repl-test-conversations--transcript (&rest overrides)
  "Return a decoded `WorkspaceTranscript' plist with OVERRIDES applied."
  (let ((transcript (list :vendor-session-id "conversation-a"
                          :last-request-at-ms agent-repl-test-conversations--now
                          :context-tokens 42000
                          :last-model (list :name "claude-opus-5")
                          :opening "explain hash tables"
                          :prompts 3
                          :current nil
                          :cleared nil
                          :active nil
                          :held nil)))
    (while overrides
      (setq transcript (plist-put transcript (pop overrides) (pop overrides))))
    transcript))

(defun agent-repl-test-conversations--line (&rest overrides)
  "Render one transcript with OVERRIDES as its completion line."
  (agent-repl-conversations--line
   (apply #'agent-repl-test-conversations--transcript overrides)
   agent-repl-test-conversations--now))

;;;; ---- Rendering: the opening words lead ----

(ert-deftest agent-repl-test-conversations-line-leads-with-the-opening-words ()
  "The line a person reads starts with what they asked, never with an id."
  ;; Arrange, Act.
  (let ((line (agent-repl-test-conversations--line)))
    ;; Assert.
    (should (string-prefix-p "explain hash tables" line))))

(ert-deftest agent-repl-test-conversations-line-never-shows-the-vendor-id ()
  "The id is the completion's VALUE, not its label."
  ;; Arrange, Act.
  (let ((line (agent-repl-test-conversations--line)))
    ;; Assert.
    (should-not (string-match-p "conversation-a" line))))

(ert-deftest agent-repl-test-conversations-line-says-so-when-there-is-no-prompt ()
  "A conversation holding no user prompt says so rather than showing an id."
  ;; Arrange, Act.
  (let ((line (agent-repl-test-conversations--line :opening nil)))
    ;; Assert.
    (should (string-prefix-p "(no prompt yet)" line))))

(ert-deftest agent-repl-test-conversations-line-states-the-prompt-count ()
  "How many times the person spoke is what tells a long conversation from an
abandoned one at a glance."
  ;; Arrange, Act.
  (let ((line (agent-repl-test-conversations--line :prompts 17)))
    ;; Assert.
    (should (string-match-p "17 prompts" line))))

;;;; ---- Rendering: the age ----

(ert-deftest agent-repl-test-conversations-age-reads-seconds ()
  "Arrange, Act, Assert: a span under a minute reads in seconds."
  (should (equal (agent-repl-conversations--age 1000 31000) "30s ago")))

(ert-deftest agent-repl-test-conversations-age-reads-minutes ()
  "Arrange, Act, Assert."
  (should (equal (agent-repl-conversations--age 0 (* 1000 600)) "10m ago")))

(ert-deftest agent-repl-test-conversations-age-reads-hours ()
  "Arrange, Act, Assert."
  (should (equal (agent-repl-conversations--age 0 (* 1000 7200)) "2h ago")))

(ert-deftest agent-repl-test-conversations-age-reads-days ()
  "Arrange, Act, Assert."
  (should (equal (agent-repl-conversations--age 0 (* 1000 86400 3)) "3d ago")))

(ert-deftest agent-repl-test-conversations-age-of-an-unstated-instant-is-never ()
  "A conversation that never reached the model is not one from 1970."
  ;; Arrange, Act, Assert.
  (should (equal (agent-repl-conversations--age nil agent-repl-test-conversations--now)
                 "never")))

;;;; ---- Rendering: the size ----

(ert-deftest agent-repl-test-conversations-size-reads-thousands-of-tokens ()
  "Arrange, Act, Assert."
  (should (equal (agent-repl-conversations--size
                  (agent-repl-test-conversations--transcript :context-tokens 42000))
                 "42k ctx")))

(ert-deftest agent-repl-test-conversations-size-reads-a-small-count-exactly ()
  "Arrange, Act, Assert: rounding a small conversation to 0k would erase it."
  (should (equal (agent-repl-conversations--size
                  (agent-repl-test-conversations--transcript :context-tokens 512))
                 "512 ctx")))

(ert-deftest agent-repl-test-conversations-size-of-an-unstated-usage-is-unread ()
  "An absence is not a zero: a conversation nobody read is not the cheapest one."
  ;; Arrange, Act, Assert.
  (should (equal (agent-repl-conversations--size
                  (agent-repl-test-conversations--transcript :context-tokens nil))
                 "unread")))

(ert-deftest agent-repl-test-conversations-size-of-a-cleared-conversation-says-cleared ()
  "A cleared conversation resumes EMPTY however large its last request was."
  ;; Arrange, Act, Assert.
  (should (equal (agent-repl-conversations--size
                  (agent-repl-test-conversations--transcript
                   :context-tokens 200000 :cleared (list :at-ms 1)))
                 "cleared")))

;;;; ---- Rendering: the three markers ----

(ert-deftest agent-repl-test-conversations-marker-names-the-current-conversation ()
  "Arrange, Act, Assert."
  (should (equal (agent-repl-conversations--marker
                  (agent-repl-test-conversations--transcript :current t))
                 "current")))

(ert-deftest agent-repl-test-conversations-marker-names-the-holding-workspace ()
  "The marker NAMES the holder rather than saying only that something holds it."
  ;; Arrange, Act, Assert.
  (should (equal (agent-repl-conversations--marker
                  (agent-repl-test-conversations--transcript
                   :held (list :workspace (list :id "ws-2" :dir "/tmp/ws-2"))))
                 "held by /tmp/ws-2")))

(ert-deftest agent-repl-test-conversations-marker-names-a-transcript-in-use ()
  "Something is writing to it right now, which the daemon refuses a bind on."
  ;; Arrange, Act, Assert.
  (should (equal (agent-repl-conversations--marker
                  (agent-repl-test-conversations--transcript :active (list :at-ms 1)))
                 "in use")))

(ert-deftest agent-repl-test-conversations-marker-is-absent-for-a-plain-conversation ()
  "Arrange, Act, Assert: a conversation nothing is wrong with carries no marker."
  (should-not (agent-repl-conversations--marker
               (agent-repl-test-conversations--transcript))))

(ert-deftest agent-repl-test-conversations-line-carries-the-marker-in-brackets ()
  "Arrange, Act, Assert."
  (should (string-suffix-p "[current]"
                           (agent-repl-test-conversations--line :current t))))

(ert-deftest agent-repl-test-conversations-unavailable-rows-are-SHOWN-not-hidden ()
  "A person looking for the conversation they just left in a terminal needs to
SEE it and be told why it is not available, not find it missing."
  ;; Arrange.
  (let ((transcripts (list (agent-repl-test-conversations--transcript
                            :vendor-session-id "a" :active (list :at-ms 1))
                           (agent-repl-test-conversations--transcript
                            :vendor-session-id "b"))))
    ;; Act.
    (let ((candidates (agent-repl-conversations--candidates
                       transcripts agent-repl-test-conversations--now)))
      ;; Assert.
      (should (equal (mapcar #'cdr candidates) '("a" "b"))))))

;;;; ---- Rendering: the candidate alist ----

(ert-deftest agent-repl-test-conversations-candidate-value-is-the-vendor-id ()
  "`completing-read' answers the label; the daemon wants the id it served."
  ;; Arrange, Act.
  (let ((candidates (agent-repl-conversations--candidates
                     (list (agent-repl-test-conversations--transcript
                            :vendor-session-id "the-id"))
                     agent-repl-test-conversations--now)))
    ;; Assert.
    (should (equal (cdr (car candidates)) "the-id"))))

(ert-deftest agent-repl-test-conversations-duplicate-lines-stay-reachable ()
  "Two conversations opened with the same words are ordinary, and an alist that
collapsed them would make one of them unreachable."
  ;; Arrange: two transcripts that render identically.
  (let* ((one (agent-repl-test-conversations--transcript :vendor-session-id "aaaaaaaa1111"))
         (two (agent-repl-test-conversations--transcript :vendor-session-id "bbbbbbbb2222"))
         ;; Act.
         (candidates (agent-repl-conversations--candidates
                      (list one two) agent-repl-test-conversations--now)))
    ;; Assert.
    (should (equal (length (delete-dups (mapcar #'car candidates))) 2))))

(ert-deftest agent-repl-test-conversations-an-empty-listing-yields-no-candidates ()
  "Arrange, Act, Assert."
  (should-not (agent-repl-conversations--candidates nil agent-repl-test-conversations--now)))

;;;; ---- The command: fixtures ----

(defvar agent-repl-test-conversations--sent nil
  "Bind requests the stubbed rpc received, oldest first.")

(defvar agent-repl-test-conversations--messages nil
  "Strings passed to `message' during a test.")

(defvar agent-repl-test-conversations--progress nil
  "Workspace-progress reports, oldest first, as (KIND PHASE . DETAILS).")

(defvar agent-repl-test-conversations--forgotten nil
  "Op ids retired through `agent-repl-mutation-progress-forget'.")

(defun agent-repl-test-conversations--list-answer (transcripts)
  "Return the decoded ListWorkspaceTranscripts SUCCESS carrying TRANSCRIPTS."
  (list :arm :success :value (list :transcripts transcripts)))

(defun agent-repl-test-conversations--list-refusal (arm &optional fields)
  "Return the decoded ListWorkspaceTranscripts refusal of ARM carrying FIELDS."
  (list :arm :error :value (list :cause (list :arm arm :value fields))))

(defun agent-repl-test-conversations--bind-refusal (arm &optional fields)
  "Return the decoded BindWorkspaceSession refusal of ARM carrying FIELDS."
  (list :arm :error :value (list :cause (list :arm arm :value fields))))

(defmacro agent-repl-test-conversations--with (list-answer bind-answer &rest body)
  "Run BODY with the two rpcs answering LIST-ANSWER and BIND-ANSWER.
LIST-ANSWER is the decoded response the SYNCHRONOUS list verb returns.
BIND-ANSWER is the decoded response the bind verb delivers to
`:on-response', or `(:failure PLIST)' to drive the transport-failure path.
The completion picks the FIRST candidate offered, so a test states what
is in the list and never how the minibuffer behaves."
  (declare (indent 2))
  `(let ((agent-repl-test-conversations--sent nil)
         (agent-repl-test-conversations--messages nil)
         (agent-repl-test-conversations--progress nil)
         (agent-repl-test-conversations--forgotten nil)
         (agent-repl-test-conversations--timeouts nil))
     (cl-letf* (;; A STRING, as production's `agent-repl--ws-current-name' answers.
                ;; A symbol here let `symbol-name' past the suite and into the
                ;; user's hands, where the first stage signalled on it.
                ((symbol-function 'agent-repl--ws-current-name) (lambda () "ws-one"))
                ((symbol-function 'agent-repl--ws-require-known) (lambda (&rest _) nil))
                ((symbol-function 'agent-repl-verbs--ref)
                 (lambda (&rest _) (list :id "ws-id-1" :dir "/tmp/ws-1")))
                ((symbol-function 'agent-repl-verbs--conn) (lambda (&rest _) 'test-conn))
                ((symbol-function 'agent-repl-host-ref)
                 (lambda (_ws) (list :id "ws-id-1" :dir "/tmp/ws-1")))
                ((symbol-function 'agent-repl-host-conn) (lambda (_ws) 'test-conn))
                ((symbol-function 'agent-repl-host-handle-refusal) (lambda (&rest _) nil))
                ((symbol-function 'agent-repl-link-primary) (lambda () 'test-conn))
                ((symbol-function 'agent-repl-mutation-progress-new-op-id)
                 (lambda () "op-test-1"))
                ((symbol-function 'agent-repl-mutation-progress-register)
                 (lambda (&rest _) nil))
                ((symbol-function 'agent-repl-mutation-progress-forget)
                 (lambda (op-id) (push op-id agent-repl-test-conversations--forgotten)))
                ((symbol-function 'agent-repl-workspace-progress-report)
                 (lambda (kind phase &rest details)
                   (push (cons kind (cons phase details))
                         agent-repl-test-conversations--progress)
                   nil))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (push (if args (apply #'format fmt args) fmt)
                         agent-repl-test-conversations--messages)
                   nil))
                ((symbol-function 'completing-read)
                 (lambda (_prompt collection &rest _) (car collection)))
                ((symbol-function 'agent-repl-rpc-list-workspace-transcripts-sync)
                 (lambda (&rest _) ,list-answer))
                ((symbol-function 'agent-repl-rpc-bind-workspace-session)
                 (lambda (_conn request &rest keys)
                   (push (plist-get keys :timeout)
                         agent-repl-test-conversations--timeouts)
                   (push request agent-repl-test-conversations--sent)
                   (let ((answer ,bind-answer))
                     (if (plist-get answer :failure)
                         (funcall (plist-get keys :on-failure) (plist-get answer :failure))
                       (funcall (plist-get keys :on-response) answer))))))
       ,@body)))

;;;; ---- The command: the happy path ----

(ert-deftest agent-repl-test-conversations-binds-the-id-of-the-chosen-line ()
  "The rpc echoes the id the listing served, never a label the user read."
  ;; Arrange.
  (agent-repl-test-conversations--with
      (agent-repl-test-conversations--list-answer
       (list (agent-repl-test-conversations--transcript :vendor-session-id "chosen-id")))
      (list :arm :success :value nil)
    ;; Act.
    (agent-repl-bind-conversation)
    ;; Assert.
    (should (equal (plist-get (car agent-repl-test-conversations--sent) :vendor-session-id)
                   "chosen-id"))))

(ert-deftest agent-repl-test-conversations-carries-an-op-id-so-the-wait-is-reported ()
  "A bind is a session bring-up, and the wait inside it is what the op id buys."
  ;; Arrange.
  (agent-repl-test-conversations--with
      (agent-repl-test-conversations--list-answer
       (list (agent-repl-test-conversations--transcript)))
      (list :arm :success :value nil)
    ;; Act.
    (agent-repl-bind-conversation)
    ;; Assert.
    (should (equal (plist-get (car agent-repl-test-conversations--sent) :op-id) "op-test-1"))))

(ert-deftest agent-repl-test-conversations-reports-the-request-before-it-sends ()
  "Arrange, Act, Assert: the user learns the bind started, not only that it ended."
  (agent-repl-test-conversations--with
      (agent-repl-test-conversations--list-answer
       (list (agent-repl-test-conversations--transcript)))
      (list :arm :success :value nil)
    (agent-repl-bind-conversation)
    (should (memq :requested
                  (mapcar #'cadr (reverse agent-repl-test-conversations--progress))))))

(ert-deftest agent-repl-test-conversations-reports-completion-on-success ()
  "Arrange, Act, Assert."
  (agent-repl-test-conversations--with
      (agent-repl-test-conversations--list-answer
       (list (agent-repl-test-conversations--transcript)))
      (list :arm :success :value nil)
    (agent-repl-bind-conversation)
    (should (memq :completed
                  (mapcar #'cadr agent-repl-test-conversations--progress)))))

(ert-deftest agent-repl-test-conversations-retires-the-op-on-success ()
  "The bind's outcome arrives on ITS OWN rpc, so nothing on the progress stream
would ever retire the registration."
  ;; Arrange.
  (agent-repl-test-conversations--with
      (agent-repl-test-conversations--list-answer
       (list (agent-repl-test-conversations--transcript)))
      (list :arm :success :value nil)
    ;; Act.
    (agent-repl-bind-conversation)
    ;; Assert.
    (should (member "op-test-1" agent-repl-test-conversations--forgotten))))

;;;; ---- The command: nothing to choose from ----

(ert-deftest agent-repl-test-conversations-refuses-an-empty-listing ()
  "An empty list is a successful answer, and there is nothing to bind to."
  ;; Arrange.
  (agent-repl-test-conversations--with
      (agent-repl-test-conversations--list-answer nil)
      (list :arm :success :value nil)
    ;; Act, Assert.
    (should-error (agent-repl-bind-conversation) :type 'user-error)))

(ert-deftest agent-repl-test-conversations-sends-nothing-for-an-empty-listing ()
  "Arrange, Act, Assert."
  (agent-repl-test-conversations--with
      (agent-repl-test-conversations--list-answer nil)
      (list :arm :success :value nil)
    (ignore-errors (agent-repl-bind-conversation))
    (should-not agent-repl-test-conversations--sent)))

(ert-deftest agent-repl-test-conversations-refuses-without-a-current-workspace ()
  "Arrange, Act, Assert."
  (agent-repl-test-conversations--with
      (agent-repl-test-conversations--list-answer
       (list (agent-repl-test-conversations--transcript)))
      (list :arm :success :value nil)
    (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () nil)))
      (should-error (agent-repl-bind-conversation) :type 'user-error))))

;;;; ---- The command: every LIST refusal, by name ----

(ert-deftest agent-repl-test-conversations-list-no-session-is-named ()
  "The shim is what reads the transcripts, so a workspace with none says so."
  ;; Arrange.
  (agent-repl-test-conversations--with
      (agent-repl-test-conversations--list-refusal :no-session)
      (list :arm :success :value nil)
    ;; Act, Assert.
    (should (string-match-p
             "no-session"
             (cadr (should-error (agent-repl-bind-conversation) :type 'user-error))))))

(ert-deftest agent-repl-test-conversations-list-unreadable-is-named ()
  "Arrange, Act, Assert."
  (agent-repl-test-conversations--with
      (agent-repl-test-conversations--list-refusal
       :unreadable (list :searched-path "/p" :detail "EACCES"))
      (list :arm :success :value nil)
    (should (string-match-p
             "unreadable"
             (cadr (should-error (agent-repl-bind-conversation) :type 'user-error))))))

(ert-deftest agent-repl-test-conversations-list-unknown-workspace-is-named ()
  "Arrange, Act, Assert."
  (agent-repl-test-conversations--with
      (agent-repl-test-conversations--list-refusal :unknown-workspace)
      (list :arm :success :value nil)
    (should (string-match-p
             "unknown-workspace"
             (cadr (should-error (agent-repl-bind-conversation) :type 'user-error))))))

(ert-deftest agent-repl-test-conversations-list-transferring-away-is-named ()
  "Arrange, Act, Assert."
  (agent-repl-test-conversations--with
      (agent-repl-test-conversations--list-refusal :transferring-away (list :address "127.0.0.1:1"))
      (list :arm :success :value nil)
    (should (string-match-p
             "transferring-away"
             (cadr (should-error (agent-repl-bind-conversation) :type 'user-error))))))

(ert-deftest agent-repl-test-conversations-list-workspace-ref-mismatch-is-named ()
  "Arrange, Act, Assert."
  (agent-repl-test-conversations--with
      (agent-repl-test-conversations--list-refusal :workspace-ref-mismatch (list :registry-dir "/d"))
      (list :arm :success :value nil)
    (should (string-match-p
             "workspace-ref-mismatch"
             (cadr (should-error (agent-repl-bind-conversation) :type 'user-error))))))

(ert-deftest agent-repl-test-conversations-list-sends-no-bind-after-a-refusal ()
  "Arrange, Act, Assert: a listing that refused offered nothing to bind."
  (agent-repl-test-conversations--with
      (agent-repl-test-conversations--list-refusal :no-session)
      (list :arm :success :value nil)
    (ignore-errors (agent-repl-bind-conversation))
    (should-not agent-repl-test-conversations--sent)))

;;;; ---- The command: every BIND refusal, by name ----
;;
;; THE COMMAND OWNS THE WORDING for its own refusals, because they are what a
;; person standing at the chooser has to act on -- so each case asserts the
;; sentence that reached the user's line, on the FAILED phase that retires the
;; progress the bind armed.  An arm this command has no sentence for still
;; reads as its own keyword, which is the dispatcher's behaviour kept.

(defmacro agent-repl-test-conversations--bind-refusal-names (arm fields expected)
  "Assert a bind refused with ARM carrying FIELDS names EXPECTED to the user."
  `(agent-repl-test-conversations--with
       (agent-repl-test-conversations--list-answer
        (list (agent-repl-test-conversations--transcript)))
       (agent-repl-test-conversations--bind-refusal ,arm ,fields)
     (agent-repl-bind-conversation)
     (should (cl-some (lambda (entry)
                       (and (eq (car entry) :bind)
                            (eq (cadr entry) :failed)
                            (cl-some (lambda (d) (and (stringp d)
                                                      (string-match-p ,expected d)))
                                     (cddr entry))))
                      agent-repl-test-conversations--progress))))

(ert-deftest agent-repl-test-conversations-bind-unknown-transcript-is-named ()
  "Arrange, Act, Assert."
  (agent-repl-test-conversations--bind-refusal-names
   :unknown-transcript (list :vendor-session-id "invented") "no longer on disk"))

(ert-deftest agent-repl-test-conversations-bind-already-bound-is-named ()
  "Arrange, Act, Assert."
  (agent-repl-test-conversations--bind-refusal-names
   :already-bound nil "already on that conversation"))

(ert-deftest agent-repl-test-conversations-bind-transcript-active-is-named ()
  "Arrange, Act, Assert."
  (agent-repl-test-conversations--bind-refusal-names
   :transcript-active (list :at-ms 1700000000000) "writing to that conversation right now"))

(ert-deftest agent-repl-test-conversations-bind-transcript-held-is-named ()
  "Arrange, Act, Assert."
  (agent-repl-test-conversations--bind-refusal-names
   :transcript-held (list :workspace (list :id "ws-2" :dir "/tmp/ws-2")) "/tmp/ws-2"))

(ert-deftest agent-repl-test-conversations-bind-turn-in-flight-is-named ()
  "Arrange, Act, Assert."
  (agent-repl-test-conversations--bind-refusal-names
   :turn-in-flight nil "a turn is in flight"))

(ert-deftest agent-repl-test-conversations-bind-stop-failed-is-named ()
  "Arrange, Act, Assert."
  (agent-repl-test-conversations--bind-refusal-names
   :stop-failed (list :detail "it would not die") "it would not die"))

(ert-deftest agent-repl-test-conversations-bind-start-failed-is-named ()
  "Arrange, Act, Assert."
  (agent-repl-test-conversations--bind-refusal-names
   :start-failed (list :detail "it would not come up") "it would not come up"))

(ert-deftest agent-repl-test-conversations-bind-unknown-workspace-is-named ()
  "Arrange, Act, Assert."
  (agent-repl-test-conversations--bind-refusal-names :unknown-workspace nil "unknown-workspace"))

(ert-deftest agent-repl-test-conversations-bind-start-failed-reports-no-completion ()
  "A refused bind must not tell the user the workspace came up."
  ;; Arrange.
  (agent-repl-test-conversations--with
      (agent-repl-test-conversations--list-answer
       (list (agent-repl-test-conversations--transcript)))
      (agent-repl-test-conversations--bind-refusal :start-failed (list :detail "no"))
    ;; Act.
    (agent-repl-bind-conversation)
    ;; Assert.
    (should-not (memq :completed
                      (mapcar #'cadr agent-repl-test-conversations--progress)))))

(ert-deftest agent-repl-test-conversations-retires-the-op-on-a-refusal ()
  "No further stage will ever arrive for a bind the daemon refused."
  ;; Arrange.
  (agent-repl-test-conversations--with
      (agent-repl-test-conversations--list-answer
       (list (agent-repl-test-conversations--transcript)))
      (agent-repl-test-conversations--bind-refusal :already-bound nil)
    ;; Act.
    (agent-repl-bind-conversation)
    ;; Assert.
    (should (member "op-test-1" agent-repl-test-conversations--forgotten))))

(ert-deftest agent-repl-test-conversations-retires-the-op-when-nobody-answered ()
  "A daemon that never answered will never push a stage either."
  ;; Arrange.
  (agent-repl-test-conversations--with
      (agent-repl-test-conversations--list-answer
       (list (agent-repl-test-conversations--transcript)))
      (list :failure (list :detail "no answer"))
    ;; Act.
    (agent-repl-bind-conversation)
    ;; Assert.
    (should (member "op-test-1" agent-repl-test-conversations--forgotten))))

(ert-deftest agent-repl-test-conversations-names-the-workspace-as-a-string ()
  "The requested stage names the workspace itself, never `symbol-name' of it."
  ;; Arrange.
  (agent-repl-test-conversations--with
      (agent-repl-test-conversations--list-answer
       (list (agent-repl-test-conversations--transcript)))
      (list :arm :success :value nil)
    ;; Act.
    (agent-repl-bind-conversation)
    ;; Assert.
    (should (member (list :bind :requested "ws-one")
                    agent-repl-test-conversations--progress))))

(ert-deftest agent-repl-test-conversations-outlasts-the-default-unary-deadline ()
  "A bind is a session bring-up, and the default deadline calls one a failure."
  ;; Arrange.
  (agent-repl-test-conversations--with
      (agent-repl-test-conversations--list-answer
       (list (agent-repl-test-conversations--transcript)))
      (list :arm :success :value nil)
    ;; Act.
    (agent-repl-bind-conversation)
    ;; Assert.
    (should (equal (car agent-repl-test-conversations--timeouts)
                   agent-repl-conversations-bind-timeout-seconds))))

(provide 'test-conversations)

;;; test-conversations.el ends here
