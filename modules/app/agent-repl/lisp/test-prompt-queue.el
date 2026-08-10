;;; test-prompt-queue.el --- Tests for prompt-queue.el -*- lexical-binding: t; -*-

;;; Commentary:

;; The held-prompt queue: holding a prompt while the link is down, draining it
;; in order once the workspace revives, refusing to drain into an unwired
;; workspace, and surfacing a per-prompt failure for anything that never got
;; sent.  One edge case per test (AAA).

;;; Code:

(require 'ert)
(require 'cl-lib)

(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "test-helpers.el" dir) nil t))

(defvar agent-repl-test--pq-sent nil
  "Texts the queue handed to the send seam, in dispatch order.")
(defvar agent-repl-test--pq-pending nil
  "Texts surfaced as pending, in order.")
(defvar agent-repl-test--pq-failures nil
  "(TEXT . REASON) pairs surfaced as per-prompt failures, in order.")
(defvar agent-repl-test--pq-settled nil
  "Texts whose `:on-settle' ran, in order.")
(defvar agent-repl-test--pq-link-down t)
(defvar agent-repl-test--pq-revived nil)
(defvar agent-repl-test--pq-verdicts nil
  "Per-send verdicts consumed in order: `sent', `failed', or `hold'.")
(defvar agent-repl-test--pq-holds nil
  "Continuations parked by a `hold' verdict: (ON-SENT . ON-FAILED), oldest first.")

(defmacro agent-repl-test--with-prompt-queue (&rest body)
  "Run BODY over a private held-prompt queue with every seam stubbed.

The send seam consumes one verdict per dispatch: `sent' settles it as
dispatched, `failed' settles it as refused, and `hold' parks its
continuations so a test can settle it explicitly while the drain is
mid-flight.  An exhausted verdict list defaults to `sent'."
  (declare (indent 0))
  `(let* ((agent-repl--prompt-queue (make-hash-table :test 'equal))
          (agent-repl--prompt-queue-draining (make-hash-table :test 'equal))
          (agent-repl--prompt-queue-timers (make-hash-table :test 'equal))
          (agent-repl--prompt-queue-seq 0)
          (agent-repl-prompt-queue-revival-bound 60)
          (agent-repl-test--pq-sent nil)
          (agent-repl-test--pq-pending nil)
          (agent-repl-test--pq-failures nil)
          (agent-repl-test--pq-settled nil)
          (agent-repl-test--pq-link-down t)
          (agent-repl-test--pq-revived nil)
          (agent-repl-test--pq-verdicts nil)
          (agent-repl-test--pq-holds nil)
          (agent-repl-prompt-queue-link-down-function
           (lambda () agent-repl-test--pq-link-down))
          (agent-repl-prompt-queue-revived-function
           (lambda (_ws) agent-repl-test--pq-revived))
          (agent-repl-prompt-queue-pending-function
           (lambda (_ws entry)
             (setq agent-repl-test--pq-pending
                   (append agent-repl-test--pq-pending
                           (list (plist-get entry :text))))))
          (agent-repl-prompt-queue-failure-function
           (lambda (_ws entry reason)
             (setq agent-repl-test--pq-failures
                   (append agent-repl-test--pq-failures
                           (list (cons (plist-get entry :text) reason))))))
          (agent-repl-prompt-queue-send-function
           (lambda (_ws entry on-sent on-failed)
             (setq agent-repl-test--pq-sent
                   (append agent-repl-test--pq-sent
                           (list (plist-get entry :text))))
             (pcase (or (pop agent-repl-test--pq-verdicts) 'sent)
               ('sent (funcall on-sent "req-1"))
               ('failed (funcall on-failed "the daemon refused the prompt"))
               ('hold (setq agent-repl-test--pq-holds
                            (append agent-repl-test--pq-holds
                                    (list (cons on-sent on-failed)))))))))
     ;; No wall-clock deadline may fire during a test: the bound is exercised by
     ;; calling the deadline handler directly, never by waiting for a timer.
     (cl-letf (((symbol-function 'run-at-time) (lambda (&rest _) nil))
               ((symbol-function 'cancel-timer) (lambda (&rest _) nil)))
       ,@body)))

(defun agent-repl-test--pq-offer (ws text)
  "Offer TEXT for WS with a settle callback that records its text."
  (agent-repl-prompt-queue-offer
   ws text text "PROMPT_ORIGIN_EMACS_USER_SENT"
   (lambda () (setq agent-repl-test--pq-settled
                    (append agent-repl-test--pq-settled (list text))))))

(defun agent-repl-test--pq-texts (ws)
  "Return WS's held prompt texts, oldest first."
  (mapcar (lambda (e) (plist-get e :text))
          (agent-repl-prompt-queue-pending ws)))

;;;; ---- Holding while the link is down ----------------------------------

(ert-deftest agent-repl-test-prompt-queue-holds-while-the-link-is-down ()
  "A prompt offered while the link is down is taken by the queue."
  (agent-repl-test--with-prompt-queue
    ;; Arrange / Act
    (let ((held (agent-repl-test--pq-offer "/w" "first")))
      ;; Assert
      (should held))))

(ert-deftest agent-repl-test-prompt-queue-declines-while-the-link-is-up ()
  "A prompt offered on a live link with nothing held is NOT taken."
  (agent-repl-test--with-prompt-queue
    ;; Arrange
    (setq agent-repl-test--pq-link-down nil)
    ;; Act / Assert
    (should-not (agent-repl-test--pq-offer "/w" "first"))))

(ert-deftest agent-repl-test-prompt-queue-sends-nothing-while-holding ()
  "Holding a prompt does not put it on the wire."
  (agent-repl-test--with-prompt-queue
    ;; Arrange / Act
    (agent-repl-test--pq-offer "/w" "first")
    ;; Assert
    (should (null agent-repl-test--pq-sent))))

(ert-deftest agent-repl-test-prompt-queue-surfaces-the-held-prompt-as-pending ()
  "A held prompt is surfaced as pending the instant it is taken."
  (agent-repl-test--with-prompt-queue
    ;; Arrange / Act
    (agent-repl-test--pq-offer "/w" "first")
    ;; Assert
    (should (equal '("first") agent-repl-test--pq-pending))))

(ert-deftest agent-repl-test-prompt-queue-keeps-submission-order ()
  "Held prompts keep the order they were submitted in."
  (agent-repl-test--with-prompt-queue
    ;; Arrange / Act
    (agent-repl-test--pq-offer "/w" "first")
    (agent-repl-test--pq-offer "/w" "second")
    ;; Assert
    (should (equal '("first" "second") (agent-repl-test--pq-texts "/w")))))

(ert-deftest agent-repl-test-prompt-queue-holds-per-workspace ()
  "One workspace's held prompt is not held for another."
  (agent-repl-test--with-prompt-queue
    ;; Arrange / Act
    (agent-repl-test--pq-offer "/w" "first")
    ;; Assert
    (should (null (agent-repl-prompt-queue-pending "/other")))))

(ert-deftest agent-repl-test-prompt-queue-holds-behind-an-earlier-held-prompt ()
  "A prompt offered on a restored link still queues behind a held one."
  (agent-repl-test--with-prompt-queue
    ;; Arrange
    (agent-repl-test--pq-offer "/w" "first")
    (setq agent-repl-test--pq-link-down nil)
    ;; Act / Assert — sending directly would overtake the prompt ahead of it.
    (should (agent-repl-test--pq-offer "/w" "second"))))

;;;; ---- Draining on revival ---------------------------------------------

(ert-deftest agent-repl-test-prompt-queue-drains-in-order-on-revival ()
  "A revived workspace's held prompts go out in submission order."
  (agent-repl-test--with-prompt-queue
    ;; Arrange
    (agent-repl-test--pq-offer "/w" "first")
    (agent-repl-test--pq-offer "/w" "second")
    (setq agent-repl-test--pq-link-down nil
          agent-repl-test--pq-revived t)
    ;; Act
    (agent-repl-prompt-queue-drain "/w")
    ;; Assert
    (should (equal '("first" "second") agent-repl-test--pq-sent))))

(ert-deftest agent-repl-test-prompt-queue-empties-after-a-full-drain ()
  "Nothing is left held once every prompt has been sent."
  (agent-repl-test--with-prompt-queue
    ;; Arrange
    (agent-repl-test--pq-offer "/w" "first")
    (setq agent-repl-test--pq-revived t)
    ;; Act
    (agent-repl-prompt-queue-drain "/w")
    ;; Assert
    (should (null (agent-repl-prompt-queue-pending "/w")))))

(ert-deftest agent-repl-test-prompt-queue-settles-a-drained-prompt ()
  "A drained prompt's `:on-settle' runs, so its caller is not left waiting."
  (agent-repl-test--with-prompt-queue
    ;; Arrange
    (agent-repl-test--pq-offer "/w" "first")
    (setq agent-repl-test--pq-revived t)
    ;; Act
    (agent-repl-prompt-queue-drain "/w")
    ;; Assert
    (should (equal '("first") agent-repl-test--pq-settled))))

(ert-deftest agent-repl-test-prompt-queue-drain-all-covers-every-workspace ()
  "The snapshot-applied subscriber drains every workspace holding prompts."
  (agent-repl-test--with-prompt-queue
    ;; Arrange
    (agent-repl-test--pq-offer "/w" "first")
    (agent-repl-test--pq-offer "/other" "second")
    (setq agent-repl-test--pq-revived t)
    ;; Act
    (agent-repl-prompt-queue-drain-all)
    ;; Assert
    (should (equal 2 (length agent-repl-test--pq-sent)))))

;;;; ---- Never into an unwired workspace ---------------------------------

(ert-deftest agent-repl-test-prompt-queue-refuses-to-drain-into-an-unwired-workspace ()
  "A restored link is not enough: an unwired workspace receives nothing."
  (agent-repl-test--with-prompt-queue
    ;; Arrange
    (agent-repl-test--pq-offer "/w" "first")
    (setq agent-repl-test--pq-link-down nil)
    ;; Act — the link is back, but no session controller holds the workspace
    (agent-repl-prompt-queue-drain "/w")
    ;; Assert
    (should (null agent-repl-test--pq-sent))))

(ert-deftest agent-repl-test-prompt-queue-keeps-holding-an-unwired-workspace ()
  "A prompt not drained into an unwired workspace stays held."
  (agent-repl-test--with-prompt-queue
    ;; Arrange
    (agent-repl-test--pq-offer "/w" "first")
    (setq agent-repl-test--pq-link-down nil)
    ;; Act
    (agent-repl-prompt-queue-drain "/w")
    ;; Assert
    (should (equal '("first") (agent-repl-test--pq-texts "/w")))))

(ert-deftest agent-repl-test-prompt-queue-stops-when-the-workspace-leaves-mid-drain ()
  "A workspace that goes away mid-drain stops the drain."
  (agent-repl-test--with-prompt-queue
    ;; Arrange
    (agent-repl-test--pq-offer "/w" "first")
    (agent-repl-test--pq-offer "/w" "second")
    (setq agent-repl-test--pq-revived t
          agent-repl-test--pq-verdicts '(hold))
    (agent-repl-prompt-queue-drain "/w")
    ;; Act — the first send is in flight when the controller disappears
    (setq agent-repl-test--pq-revived nil)
    (funcall (car (pop agent-repl-test--pq-holds)) "req-1")
    ;; Assert
    (should (equal '("first") agent-repl-test--pq-sent))))

(ert-deftest agent-repl-test-prompt-queue-keeps-the-remainder-when-it-stops ()
  "The prompts behind a stopped drain stay held, in order."
  (agent-repl-test--with-prompt-queue
    ;; Arrange
    (agent-repl-test--pq-offer "/w" "first")
    (agent-repl-test--pq-offer "/w" "second")
    (setq agent-repl-test--pq-revived t
          agent-repl-test--pq-verdicts '(hold))
    (agent-repl-prompt-queue-drain "/w")
    ;; Act
    (setq agent-repl-test--pq-revived nil)
    (funcall (car (pop agent-repl-test--pq-holds)) "req-1")
    ;; Assert
    (should (equal '("second") (agent-repl-test--pq-texts "/w")))))

(ert-deftest agent-repl-test-prompt-queue-drain-is-a-no-op-with-nothing-held ()
  "Draining a workspace holding no prompts sends nothing."
  (agent-repl-test--with-prompt-queue
    ;; Arrange
    (setq agent-repl-test--pq-revived t)
    ;; Act
    (agent-repl-prompt-queue-drain "/w")
    ;; Assert
    (should (null agent-repl-test--pq-sent))))

(ert-deftest agent-repl-test-prompt-queue-second-drain-does-not-double-send ()
  "A drain edge arriving mid-drain does not re-dispatch what is in flight."
  (agent-repl-test--with-prompt-queue
    ;; Arrange
    (agent-repl-test--pq-offer "/w" "first")
    (setq agent-repl-test--pq-revived t
          agent-repl-test--pq-verdicts '(hold))
    (agent-repl-prompt-queue-drain "/w")
    ;; Act
    (agent-repl-prompt-queue-drain "/w")
    ;; Assert
    (should (equal '("first") agent-repl-test--pq-sent))))

;;;; ---- Failure honesty --------------------------------------------------

(ert-deftest agent-repl-test-prompt-queue-surfaces-a-refused-drain ()
  "A held prompt the daemon refuses is surfaced, not dropped."
  (agent-repl-test--with-prompt-queue
    ;; Arrange
    (agent-repl-test--pq-offer "/w" "first")
    (setq agent-repl-test--pq-revived t
          agent-repl-test--pq-verdicts '(failed))
    ;; Act
    (agent-repl-prompt-queue-drain "/w")
    ;; Assert
    (should (equal '(("first" . "the daemon refused the prompt"))
                   agent-repl-test--pq-failures))))

(ert-deftest agent-repl-test-prompt-queue-surfaces-one-failure-per-prompt ()
  "Two refused held prompts produce two reports, not one."
  (agent-repl-test--with-prompt-queue
    ;; Arrange
    (agent-repl-test--pq-offer "/w" "first")
    (agent-repl-test--pq-offer "/w" "second")
    (setq agent-repl-test--pq-revived t
          agent-repl-test--pq-verdicts '(failed failed))
    ;; Act
    (agent-repl-prompt-queue-drain "/w")
    ;; Assert
    (should (equal '("first" "second") (mapcar #'car agent-repl-test--pq-failures)))))

(ert-deftest agent-repl-test-prompt-queue-still-sends-behind-a-refusal ()
  "A refused held prompt does not take the prompts behind it down with it."
  (agent-repl-test--with-prompt-queue
    ;; Arrange
    (agent-repl-test--pq-offer "/w" "first")
    (agent-repl-test--pq-offer "/w" "second")
    (setq agent-repl-test--pq-revived t
          agent-repl-test--pq-verdicts '(failed sent))
    ;; Act
    (agent-repl-prompt-queue-drain "/w")
    ;; Assert
    (should (equal '("first" "second") agent-repl-test--pq-sent))))

(ert-deftest agent-repl-test-prompt-queue-settles-a-refused-prompt ()
  "A refused held prompt still settles its caller."
  (agent-repl-test--with-prompt-queue
    ;; Arrange
    (agent-repl-test--pq-offer "/w" "first")
    (setq agent-repl-test--pq-revived t
          agent-repl-test--pq-verdicts '(failed))
    ;; Act
    (agent-repl-prompt-queue-drain "/w")
    ;; Assert
    (should (equal '("first") agent-repl-test--pq-settled))))

;;;; ---- The revival bound ------------------------------------------------

(ert-deftest agent-repl-test-prompt-queue-fails-a-prompt-past-the-bound ()
  "A workspace that never comes back fails its held prompt."
  (agent-repl-test--with-prompt-queue
    ;; Arrange
    (agent-repl-test--pq-offer "/w" "first")
    (let ((entry (car (agent-repl-prompt-queue-pending "/w"))))
      ;; Act
      (agent-repl--prompt-queue-deadline "/w" entry))
    ;; Assert
    (should (equal '("first") (mapcar #'car agent-repl-test--pq-failures)))))

(ert-deftest agent-repl-test-prompt-queue-names-the-bound-it-enforced ()
  "The expiry failure says what the prompt waited for and for how long."
  (agent-repl-test--with-prompt-queue
    ;; Arrange
    (agent-repl-test--pq-offer "/w" "first")
    (let ((entry (car (agent-repl-prompt-queue-pending "/w"))))
      ;; Act
      (agent-repl--prompt-queue-deadline "/w" entry))
    ;; Assert
    (should (string-match-p "did not come back within 60s"
                            (cdr (car agent-repl-test--pq-failures))))))

(ert-deftest agent-repl-test-prompt-queue-releases-an-expired-prompt ()
  "An expired held prompt is no longer held."
  (agent-repl-test--with-prompt-queue
    ;; Arrange
    (agent-repl-test--pq-offer "/w" "first")
    (let ((entry (car (agent-repl-prompt-queue-pending "/w"))))
      ;; Act
      (agent-repl--prompt-queue-deadline "/w" entry))
    ;; Assert
    (should (null (agent-repl-prompt-queue-pending "/w")))))

(ert-deftest agent-repl-test-prompt-queue-expiry-spares-the-other-held-prompts ()
  "One prompt's bound expiring leaves the rest of the queue held."
  (agent-repl-test--with-prompt-queue
    ;; Arrange
    (agent-repl-test--pq-offer "/w" "first")
    (agent-repl-test--pq-offer "/w" "second")
    (let ((entry (car (agent-repl-prompt-queue-pending "/w"))))
      ;; Act
      (agent-repl--prompt-queue-deadline "/w" entry))
    ;; Assert
    (should (equal '("second") (agent-repl-test--pq-texts "/w")))))

(ert-deftest agent-repl-test-prompt-queue-deadline-drains-a-revived-workspace ()
  "A deadline reached on a revived workspace sends rather than fails."
  (agent-repl-test--with-prompt-queue
    ;; Arrange
    (agent-repl-test--pq-offer "/w" "first")
    (setq agent-repl-test--pq-revived t)
    (let ((entry (car (agent-repl-prompt-queue-pending "/w"))))
      ;; Act
      (agent-repl--prompt-queue-deadline "/w" entry))
    ;; Assert
    (should (null agent-repl-test--pq-failures))))

(ert-deftest agent-repl-test-prompt-queue-deadline-ignores-an-already-sent-prompt ()
  "A deadline firing after its prompt already went out reports nothing."
  (agent-repl-test--with-prompt-queue
    ;; Arrange
    (agent-repl-test--pq-offer "/w" "first")
    (let ((entry (car (agent-repl-prompt-queue-pending "/w"))))
      (setq agent-repl-test--pq-revived t)
      (agent-repl-prompt-queue-drain "/w")
      (setq agent-repl-test--pq-revived nil)
      ;; Act
      (agent-repl--prompt-queue-deadline "/w" entry))
    ;; Assert
    (should (null agent-repl-test--pq-failures))))

(ert-deftest agent-repl-test-prompt-queue-expiry-hands-back-the-raw-text ()
  "The default failure report carries the RAW prompt, not the decorated one."
  (agent-repl-test--with-prompt-queue
    ;; Arrange — the send path decorates; only what the user wrote may return.
    (let ((agent-repl-prompt-queue-failure-function
           #'agent-repl--prompt-queue-default-note-failure)
          (warned nil))
      (cl-letf (((symbol-function 'agent-repl--warn)
                 (lambda (_ws fmt &rest args) (setq warned (apply #'format fmt args)))))
        (agent-repl-prompt-queue-offer
         "/w" "DECORATED: what I wrote" "what I wrote"
         "PROMPT_ORIGIN_EMACS_USER_SENT" nil)
        (let ((entry (car (agent-repl-prompt-queue-pending "/w"))))
          ;; Act
          (agent-repl--prompt-queue-deadline "/w" entry)))
      ;; Assert
      (should (string-match-p "what I wrote" warned))
      (should-not (string-match-p "DECORATED" warned)))))

(provide 'test-prompt-queue)
;;; test-prompt-queue.el ends here
