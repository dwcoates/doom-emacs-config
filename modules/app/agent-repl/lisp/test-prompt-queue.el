;;; test-prompt-queue.el --- ERT tests for agent-repl prompt-queue.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;   AGENT_REPL_FORBID_VENDOR_CALLS=1 emacs -batch -Q -l ert \
;;     -l lisp/test-prompt-queue.el -f ert-run-tests-batch-and-exit
;;
;; The submit is stubbed and records what it was handed, so THE RELEASE
;; EDGE -- the roster's finish edge -- is driven directly rather than
;; waited on.  Nothing here uses a timer or a sleep.
;;
;; The assertions that matter most: a released prompt carries
;; `:deferred-prompt' whatever origin it was composed under (this queue is
;; that origin's one send site), a prompt is never released into a composer
;; gate the daemon would refuse, and the in-memory OUTAGE queue is gone for
;; good -- a prompt the daemon did not take is held durably by
;; held-ingress.el, on ONE path.

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                           (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Fixtures ----

(defvar agent-repl-test-pq--submitted nil
  "Submissions the stubbed composer received, as (WS SAID ORIGIN RAW KEY).")

(defvar agent-repl-test-pq--link-up t
  "Whether the stubbed daemon link reports up.")

(defvar agent-repl-test-pq--gate :open
  "The composer gate the stubbed host reports.")

(defvar agent-repl-test-pq--conn nil
  "The REAL connection object the stubbed host hands the drain, or nil.
Real, not faked: aliveness is the struct's own flag, so the dead-connection
case is arranged by actually closing it.")

(defun agent-repl-test-pq--said (text)
  "Return a `UserSaid' carrying TEXT, as the composer would have built it."
  (list :content (list :blocks (list (list :arm :text :value (list :text text))))))

(defmacro agent-repl-test-pq--with (&rest body)
  "Run BODY with an empty queue and the wire and the gate stubbed."
  (declare (indent 0))
  `(let ((agent-repl--prompt-queue (make-hash-table :test 'equal))
         (agent-repl-test-pq--submitted nil)
         (agent-repl-test-pq--link-up t)
         (agent-repl-test-pq--gate :open)
         (agent-repl-test-pq--conn
          (agent-repl-connect-connection-create :address "127.0.0.1:9001")))
     (cl-letf (((symbol-function 'agent-repl-link-up-p)
                (lambda () agent-repl-test-pq--link-up))
               ((symbol-function 'agent-repl-host-conn)
                (lambda (_ws) agent-repl-test-pq--conn))
               ((symbol-function 'agent-repl-link-primary)
                (lambda () agent-repl-test-pq--conn))
               ((symbol-function 'agent-repl-host-composer-gate)
                (lambda (_ws) agent-repl-test-pq--gate))
               ((symbol-function 'agent-repl--input-submit)
                (lambda (ws said origin raw &optional key)
                  (push (list ws said origin raw key) agent-repl-test-pq--submitted)
                  (or key "key-1")))
               ((symbol-function 'message) (lambda (&rest _) nil)))
       ,@body)))

(defun agent-repl-test-pq--sent-texts ()
  "Return the text of every submission, oldest first."
  (mapcar (lambda (entry)
            (plist-get (plist-get (car (plist-get (plist-get (nth 1 entry) :content) :blocks))
                                  :value)
                       :text))
          (reverse agent-repl-test-pq--submitted)))

;;;; ---- Deferring ----

(defun agent-repl-test-pq--defer (ws text &optional origin)
  "Enqueue a deferred TEXT for WS, composed under ORIGIN (default the queue's)."
  (agent-repl--prompt-queue-enqueue ws :deferred (agent-repl-test-pq--said text)
                                    (or origin :deferred-prompt) text))

(ert-deftest agent-repl-pq-enqueue-keeps-order-per-workspace ()
  "Order is the user's typing order, per workspace."
  (agent-repl-test-pq--with
    ;; Act
    (agent-repl-test-pq--defer "ws-one" "a")
    (agent-repl-test-pq--defer "ws-one" "b")
    ;; Assert
    (should (equal (mapcar (lambda (e) (plist-get e :raw))
                           (agent-repl-prompt-queue-pending "ws-one"))
                   '("a" "b")))))

(ert-deftest agent-repl-pq-queues-are-per-workspace ()
  "One workspace's deferred prompts are not another's."
  (agent-repl-test-pq--with
    ;; Act
    (agent-repl-test-pq--defer "ws-one" "a")
    ;; Assert
    (should-not (agent-repl-prompt-queue-pending "ws-two"))))

(ert-deftest agent-repl-pq-enqueue-refuses-any-kind-but-a-deferral ()
  "The queue holds deferrals only; an outage entry has no place here."
  (agent-repl-test-pq--with
    ;; Act / Assert
    (should-error (agent-repl--prompt-queue-enqueue
                   "ws-one" :outage (agent-repl-test-pq--said "a") :user-sent "a"))))

;;;; ---- The liveness gate ----
(ert-deftest agent-repl-pq-not-deliverable-while-the-link-is-down ()
  "A drain needs the link up before anything else."
  (agent-repl-test-pq--with
    (setq agent-repl-test-pq--link-up nil)
    (should-not (agent-repl-prompt-queue-deliverable-p "ws-one"))))

(ert-deftest agent-repl-pq-not-deliverable-while-merging ()
  "A drain into a merging composer would be refused, so it does not happen."
  (agent-repl-test-pq--with
    (setq agent-repl-test-pq--gate :merging)
    (should-not (agent-repl-prompt-queue-deliverable-p "ws-one"))))

(ert-deftest agent-repl-pq-not-deliverable-while-draining ()
  "A draining daemon is not somewhere to release a held prompt."
  (agent-repl-test-pq--with
    (setq agent-repl-test-pq--gate :draining)
    (should-not (agent-repl-prompt-queue-deliverable-p "ws-one"))))

(ert-deftest agent-repl-pq-not-deliverable-while-restarting ()
  "A restarting session is not somewhere to release a held prompt."
  (agent-repl-test-pq--with
    (setq agent-repl-test-pq--gate :restarting)
    (should-not (agent-repl-prompt-queue-deliverable-p "ws-one"))))

(ert-deftest agent-repl-pq-deliverable-when-open ()
  "An open composer on a live link is deliverable."
  (agent-repl-test-pq--with
    (should (agent-repl-prompt-queue-deliverable-p "ws-one"))))

(ert-deftest agent-repl-pq-not-deliverable-on-a-dead-connection ()
  "A closed connection cannot carry a send, whatever the link reports."
  (agent-repl-test-pq--with
    ;; Arrange
    (agent-repl-connect-close agent-repl-test-pq--conn)
    ;; Act / Assert
    (should-not (agent-repl-prompt-queue-deliverable-p "ws-one"))))

(ert-deftest agent-repl-pq-not-deliverable-without-a-connection ()
  "No connection at all is the same refusal: there is nothing to send on."
  (agent-repl-test-pq--with
    ;; Arrange
    (setq agent-repl-test-pq--conn nil)
    ;; Act / Assert
    (should-not (agent-repl-prompt-queue-deliverable-p "ws-one"))))

(ert-deftest agent-repl-pq-dead-connection-is-warned ()
  "The refusal is not silent: a held prompt not going out is news."
  (agent-repl-test-pq--with
    ;; Arrange
    (agent-repl-connect-close agent-repl-test-pq--conn)
    (let ((warnings nil))
      (cl-letf (((symbol-function 'agent-repl--warn)
                 (lambda (_ws fmt &rest args)
                   (push (apply #'format fmt args) warnings))))
        ;; Act
        (agent-repl-prompt-queue-deliverable-p "ws-one")
        ;; Assert
        (should (cl-find-if (lambda (text)
                              (string-match-p "elisp.prompt-queue.dead-conn" text))
                            warnings))))))

(ert-deftest agent-repl-pq-deliverable-when-merge-parked ()
  "A parked merge leaves the composer open, so a held prompt may go."
  (agent-repl-test-pq--with
    (setq agent-repl-test-pq--gate :merge-parked)
    (should (agent-repl-prompt-queue-deliverable-p "ws-one"))))

(ert-deftest agent-repl-pq-deliverable-with-no-session ()
  "SubmitPrompt has no precondition, so a session-less workspace is fine."
  (agent-repl-test-pq--with
    (setq agent-repl-test-pq--gate :no-session)
    (should (agent-repl-prompt-queue-deliverable-p "ws-one"))))

;;;; ---- The release, on the finish edge ----

(ert-deftest agent-repl-pq-finish-edge-releases-one-deferred-prompt ()
  "Each finished turn releases ONE deferral: each is meant to be its own turn."
  (agent-repl-test-pq--with
    (agent-repl--prompt-queue-enqueue "ws-one" :deferred (agent-repl-test-pq--said "a")
                                      :deferred-prompt "a")
    (agent-repl--prompt-queue-enqueue "ws-one" :deferred (agent-repl-test-pq--said "b")
                                      :deferred-prompt "b")
    (agent-repl--prompt-queue-on-finish "ws-one")
    (should (equal (agent-repl-test-pq--sent-texts) '("a")))
    (should (equal (length (agent-repl-prompt-queue-pending "ws-one" :deferred)) 1))))

(ert-deftest agent-repl-pq-finish-edge-releases-the-oldest-first ()
  "The queue does not reorder itself: the head goes first."
  (agent-repl-test-pq--with
    (agent-repl--prompt-queue-enqueue "ws-one" :deferred (agent-repl-test-pq--said "first")
                                      :deferred-prompt "first")
    (agent-repl--prompt-queue-enqueue "ws-one" :deferred (agent-repl-test-pq--said "second")
                                      :deferred-prompt "second")
    (agent-repl--prompt-queue-on-finish "ws-one")
    (agent-repl--prompt-queue-on-finish "ws-one")
    (should (equal (agent-repl-test-pq--sent-texts) '("first" "second")))))

(ert-deftest agent-repl-pq-finish-edge-with-nothing-held-sends-nothing ()
  "The finish edge fires on every settled turn; usually nothing is held."
  (agent-repl-test-pq--with
    (agent-repl--prompt-queue-on-finish "ws-one")
    (should-not agent-repl-test-pq--submitted)))

(ert-deftest agent-repl-pq-finish-edge-holds-when-not-deliverable ()
  "A deferral is not released into a composer the daemon would refuse."
  (agent-repl-test-pq--with
    (agent-repl--prompt-queue-enqueue "ws-one" :deferred (agent-repl-test-pq--said "a")
                                      :deferred-prompt "a")
    (setq agent-repl-test-pq--gate :merging)
    (agent-repl--prompt-queue-on-finish "ws-one")
    (should-not agent-repl-test-pq--submitted)
    (should (equal (length (agent-repl-prompt-queue-pending "ws-one" :deferred)) 1))))

(ert-deftest agent-repl-pq-finish-edge-is-workspace-scoped ()
  "A finish edge on one workspace does not release another's deferral."
  (agent-repl-test-pq--with
    (agent-repl--prompt-queue-enqueue "ws-two" :deferred (agent-repl-test-pq--said "a")
                                      :deferred-prompt "a")
    (agent-repl--prompt-queue-on-finish "ws-one")
    (should-not agent-repl-test-pq--submitted)))

;;;; ---- Registration ----

(ert-deftest agent-repl-pq-registers-on-the-finish-edge ()
  "The deferral drain is wired to the roster's finish edge and nothing else."
  (should (memq #'agent-repl--prompt-queue-on-finish agent-repl-roster-finish-functions)))

;;;; ---- The deferral command ----

(ert-deftest agent-repl-pq-defer-command-enqueues-and-clears ()
  "Deferring captures the text, clears the composer and holds the prompt."
  (agent-repl-test-pq--with
    (let ((buf (generate-new-buffer " *agent-repl-test-composer*")))
      (unwind-protect
          (progn
            (with-current-buffer buf (agent-repl-input-mode) (insert "later please"))
            (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "ws-one"))
                      ((symbol-function 'agent-repl--ws-get)
                       (lambda (_ws key) (when (eq key :input-buffer) buf)))
                      ((symbol-function 'agent-repl--history-push) (lambda (&optional _t) nil))
                      ((symbol-function 'agent-repl--history-reset) (lambda () nil))
                      ((symbol-function 'agent-repl--history-save) (lambda (_ws) nil)))
              (agent-repl-queue-deferred-prompt)
              (should (equal (length (agent-repl-prompt-queue-pending "ws-one" :deferred)) 1))
              (should (equal (with-current-buffer buf (buffer-string)) ""))))
        (kill-buffer buf)))))

(ert-deftest agent-repl-pq-defer-command-on-empty-input-holds-nothing ()
  "There is nothing to defer when nothing was written."
  (agent-repl-test-pq--with
    (let ((buf (generate-new-buffer " *agent-repl-test-composer*")))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "ws-one"))
                    ((symbol-function 'agent-repl--ws-get)
                     (lambda (_ws key) (when (eq key :input-buffer) buf))))
            (with-current-buffer buf (agent-repl-input-mode))
            (agent-repl-queue-deferred-prompt)
            (should-not (agent-repl-prompt-queue-pending "ws-one")))
        (kill-buffer buf)))))

(ert-deftest agent-repl-pq-deferred-entry-carries-no-key ()
  "A deferral never attempted anything, so it holds no key to retry under."
  (agent-repl-test-pq--with
    (let ((entry (agent-repl--prompt-queue-enqueue
                  "ws-one" :deferred (agent-repl-test-pq--said "a") :deferred-prompt "a")))
      (should (null (plist-get entry :idempotency-key))))))

(ert-deftest agent-repl-pq-finish-edge-on-a-dead-connection-sends-nothing ()
  "A release onto a corpse would lose the prompt for a send that cannot happen."
  (agent-repl-test-pq--with
    ;; Arrange
    (agent-repl-test-pq--defer "ws-one" "a")
    (agent-repl-connect-close agent-repl-test-pq--conn)
    ;; Act
    (agent-repl--prompt-queue-on-finish "ws-one")
    ;; Assert
    (should-not agent-repl-test-pq--submitted)))

(ert-deftest agent-repl-pq-finish-edge-on-a-dead-connection-keeps-the-entry ()
  "What was not sent stays HELD: this queue never drops a prompt silently."
  (agent-repl-test-pq--with
    ;; Arrange
    (agent-repl-test-pq--defer "ws-one" "a")
    (agent-repl-connect-close agent-repl-test-pq--conn)
    ;; Act
    (agent-repl--prompt-queue-on-finish "ws-one")
    ;; Assert
    (should (equal (length (agent-repl-prompt-queue-pending "ws-one" :deferred)) 1))))

(ert-deftest agent-repl-pq-released-prompt-carries-the-queue-origin ()
  "A released prompt says it was HELD, not that it went out when typed."
  (agent-repl-test-pq--with
    ;; Arrange
    (agent-repl-test-pq--defer "ws-one" "a" :user-sent-with-prefix)
    ;; Act
    (agent-repl--prompt-queue-on-finish "ws-one")
    ;; Assert
    (should (eq (nth 2 (car agent-repl-test-pq--submitted)) :deferred-prompt))))

(ert-deftest agent-repl-pq-release-recomposes-nothing ()
  "The held `UserSaid' is submitted as composed: a second pass would double it."
  (agent-repl-test-pq--with
    ;; Arrange
    (let ((said (agent-repl-test-pq--said "already composed")))
      (agent-repl--prompt-queue-enqueue "ws-one" :deferred said :deferred-prompt "raw")
      ;; Act
      (agent-repl--prompt-queue-on-finish "ws-one")
      ;; Assert
      (should (equal (nth 1 (car agent-repl-test-pq--submitted)) said)))))

(ert-deftest agent-repl-pq-release-mints-a-fresh-key ()
  "A deferral never attempted anything, so its release passes no key."
  (agent-repl-test-pq--with
    ;; Arrange
    (agent-repl-test-pq--defer "ws-one" "a")
    ;; Act
    (agent-repl--prompt-queue-on-finish "ws-one")
    ;; Assert
    (should (null (nth 4 (car agent-repl-test-pq--submitted))))))

;;;; ---- ONE path: the in-memory outage queue is gone ----

(defconst agent-repl-test-pq--retired-outage-names
  '("agent-repl-prompt-queue-offer"
    "agent-repl-prompt-queue-drain"
    "agent-repl--prompt-queue-draining"
    "agent-repl--prompt-queue-on-link-up"
    "agent-repl--prompt-queue-on-link-promote"
    "agent-repl--prompt-queue-on-reattached"
    ":outage")
  "Names of the retired in-memory outage queue and its release edges.")

(ert-deftest agent-repl-pq-no-production-source-names-the-retired-outage-queue ()
  "No production source holds a prompt the daemon did not take in memory.
Owner ruling, 2026-09-28: such a prompt goes to the durable held-prompt
ingress (held-ingress.el), on ONE path, and survives restarts.  A source
that names the retired queue or one of its release edges has reopened the
second path."
  ;; Arrange
  (let* ((dir (file-name-directory (symbol-file 'agent-repl-queue-deferred-prompt)))
         (sources (cl-remove-if (lambda (f) (string-prefix-p "test-" (file-name-nondirectory f)))
                                (directory-files dir t "\\.el\\'")))
         (offenders nil))
    ;; Act
    (dolist (file sources)
      (with-temp-buffer
        (insert-file-contents file)
        (dolist (name agent-repl-test-pq--retired-outage-names)
          (goto-char (point-min))
          (when (search-forward name nil t)
            (push (format "%s: %s" (file-name-nondirectory file) name) offenders)))))
    ;; Assert
    (should (> (length sources) 10))
    (should-not offenders)))

(ert-deftest agent-repl-pq-no-release-edge-hangs-off-the-link-or-a-reattach ()
  "Nothing from this queue is wired to link-up, a promotion or a reattach."
  (dolist (hook '(agent-repl-link-up-functions
                  agent-repl-link-promote-functions
                  agent-repl-host-reattached-functions))
    (should-not (seq-some (lambda (fn) (and (symbolp fn)
                                            (string-prefix-p "agent-repl--prompt-queue"
                                                             (symbol-name fn))))
                          (default-value hook)))))

(provide 'test-prompt-queue)

;;; test-prompt-queue.el ends here
