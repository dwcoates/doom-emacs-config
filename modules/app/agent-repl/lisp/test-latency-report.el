;;; test-latency-report.el --- ERT tests for agent-repl latency-report.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;   bin/background.sh emacs -batch -Q -l ert -l lisp/test-latency-report.el \
;;     -f ert-run-tests-batch-and-exit

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

(defvar agent-repl-test-latency-report--sent nil "Every report sent, newest first, as its request plist.")
(defvar agent-repl-test-latency-report--answer nil "How the stubbed rpc answers: (:response R), (:failure D) or nil for never.")
(defvar agent-repl-test-latency-report--records nil "Every record, newest first, as (LEVEL TEXT).")
(defvar agent-repl-test-latency-report--link nil "What the stubbed `agent-repl-link-live' answers.")
(defvar agent-repl-test-latency-report--timers nil "Every timer armed, as (SECS FUNCTION).")

(defconst agent-repl-test-latency-report--recorded
  '(:response (:arm :success :value (:arm :recorded :value (:recorded 0))))
  "A daemon answer that recorded the batch.")

(defmacro agent-repl-test-latency-report--with (&rest body)
  "Run BODY with empty queues, a live link, a recording rpc and captured timers."
  (declare (indent 0))
  `(let ((agent-repl-latency-report--startup nil)
         (agent-repl-latency-report--editor nil)
         (agent-repl-latency-report--in-flight nil)
         (agent-repl-latency-report--timer nil)
         (agent-repl-latency-report--dropped 0)
         (agent-repl-test-latency-report--sent nil)
         (agent-repl-test-latency-report--records nil)
         (agent-repl-test-latency-report--timers nil)
         (agent-repl-test-latency-report--answer agent-repl-test-latency-report--recorded)
         (agent-repl-test-latency-report--link 'conn))
     (cl-letf (((symbol-function 'agent-repl-link-live) (lambda () agent-repl-test-latency-report--link))
               ((symbol-function 'run-at-time)
                (lambda (secs _repeat fn &rest _args)
                  (push (list secs fn) agent-repl-test-latency-report--timers)
                  'timer))
               ((symbol-function 'agent-repl--log) #'ignore)
               ((symbol-function 'agent-repl--info)
                (lambda (_ws fmt &rest args) (push (list 'info (apply #'format fmt args)) agent-repl-test-latency-report--records)))
               ((symbol-function 'agent-repl--warn)
                (lambda (_ws fmt &rest args) (push (list 'warn (apply #'format fmt args)) agent-repl-test-latency-report--records)))
               ((symbol-function 'agent-repl--error)
                (lambda (_ws fmt &rest args) (push (list 'error (apply #'format fmt args)) agent-repl-test-latency-report--records)))
               ((symbol-function 'agent-repl-rpc-report-latencies)
                (lambda (_conn request &rest keys)
                  (push request agent-repl-test-latency-report--sent)
                  (pcase agent-repl-test-latency-report--answer
                    (`(:response ,r) (funcall (plist-get keys :on-response) r))
                    (`(:failure ,d) (funcall (plist-get keys :on-failure) d))))))
       ,@body)))

(defun agent-repl-test-latency-report--recorded-p (level pattern)
  "Non-nil when a LEVEL record matching PATTERN was written."
  (cl-some (lambda (r) (and (eq (car r) level) (string-match-p pattern (cadr r))))
           agent-repl-test-latency-report--records))

(defconst agent-repl-test-latency-report--milestone
  '(:milestone :all-tabs :daemon :running :seconds 4.0)
  "One startup milestone as startup.el queues it.")

(ert-deftest agent-repl-test-latency-report-startup-milestones-are-sent-at-once ()
  "Queued startup milestones go out immediately, and the acknowledgment clears them."
  (agent-repl-test-latency-report--with
    ;; Act
    (agent-repl-latency-report-startup (list (copy-sequence agent-repl-test-latency-report--milestone)))
    ;; Assert
    (should (equal (plist-get (car agent-repl-test-latency-report--sent) :editor-startup)
                   (list agent-repl-test-latency-report--milestone)))
    (should-not agent-repl-latency-report--startup)
    (should (agent-repl-test-latency-report--recorded-p 'info "elisp.latency-report.ok milestones=1 latencies=0 outcome=:recorded"))))

(ert-deftest agent-repl-test-latency-report-an-editor-latency-waits-for-the-delay ()
  "An editor latency schedules one send after the delay, rather than sending now."
  (agent-repl-test-latency-report--with
    ;; Act
    (agent-repl-latency-report-editor :stall 0.2)
    (agent-repl-latency-report-editor :stall 0.3)
    ;; Assert
    (should-not agent-repl-test-latency-report--sent)
    (should (equal agent-repl-test-latency-report--timers
                   (list (list agent-repl-latency-report-delay #'agent-repl-latency-report--scheduled))))))

(ert-deftest agent-repl-test-latency-report-the-scheduled-send-carries-every-queued-latency ()
  "The delay's send carries every editor latency queued meanwhile, oldest first."
  (agent-repl-test-latency-report--with
    ;; Arrange
    (agent-repl-latency-report-editor :stall 0.2)
    (agent-repl-latency-report-editor :elisp-reload 0.3)
    ;; Act
    (agent-repl-latency-report--scheduled)
    ;; Assert
    (should (equal (plist-get (car agent-repl-test-latency-report--sent) :editor-latencies)
                   '((:kind :stall :seconds 0.2) (:kind :elisp-reload :seconds 0.3))))
    (should-not agent-repl-latency-report--editor)
    (should-not agent-repl-latency-report--timer)))

(ert-deftest agent-repl-test-latency-report-a-report-that-did-not-arrive-is-sent-at-the-next-link-up ()
  "A transport failure keeps the batch, and the link-up handler sends it again."
  (agent-repl-test-latency-report--with
    ;; Arrange
    (setq agent-repl-test-latency-report--answer '(:failure (:kind :transport)))
    (agent-repl-latency-report-editor :stall 0.2)
    (agent-repl-latency-report--scheduled)
    (should (= (length agent-repl-latency-report--editor) 1))
    ;; Act
    (setq agent-repl-test-latency-report--answer agent-repl-test-latency-report--recorded)
    (agent-repl-latency-report-flush 'conn)
    ;; Assert
    (should (= (length agent-repl-test-latency-report--sent) 2))
    (should-not agent-repl-latency-report--editor)
    (should (agent-repl-test-latency-report--recorded-p 'info "elisp.latency-report.deferred milestones=0 latencies=1"))))

(ert-deftest agent-repl-test-latency-report-a-refused-report-is-recorded-and-dropped ()
  "A refusal is a defect the same batch would repeat: recorded at ERROR, never resent."
  (agent-repl-test-latency-report--with
    ;; Arrange
    (setq agent-repl-test-latency-report--answer
          '(:response (:arm :error :value (:arm :workspace-ref-mismatch :value nil))))
    ;; Act
    (agent-repl-latency-report-startup (list (copy-sequence agent-repl-test-latency-report--milestone)))
    (agent-repl-latency-report-flush 'conn)
    ;; Assert
    (should (= (length agent-repl-test-latency-report--sent) 1))
    (should-not agent-repl-latency-report--startup)
    (should (agent-repl-test-latency-report--recorded-p 'error "elisp.latency-report.refused milestones=1 latencies=0"))))

(ert-deftest agent-repl-test-latency-report-an-unknown-answer-arm-is-an-error ()
  "An answer with neither arm is recorded at ERROR and the batch dropped."
  (agent-repl-test-latency-report--with
    ;; Arrange
    (setq agent-repl-test-latency-report--answer '(:response (:arm :other)))
    ;; Act
    (agent-repl-latency-report-startup (list (copy-sequence agent-repl-test-latency-report--milestone)))
    ;; Assert
    (should-not agent-repl-latency-report--startup)
    (should (agent-repl-test-latency-report--recorded-p 'error "elisp.latency-report.unknown-arm milestones=1 latencies=0 arm=:other"))))

(ert-deftest agent-repl-test-latency-report-one-report-at-a-time ()
  "A flush while a report is unanswered sends nothing more."
  (agent-repl-test-latency-report--with
    ;; Arrange
    (setq agent-repl-test-latency-report--answer nil)
    (agent-repl-latency-report-startup (list (copy-sequence agent-repl-test-latency-report--milestone)))
    ;; Act
    (agent-repl-latency-report-flush 'conn)
    ;; Assert
    (should (= (length agent-repl-test-latency-report--sent) 1))))

(ert-deftest agent-repl-test-latency-report-what-is-queued-meanwhile-goes-next ()
  "A latency queued while a report is out is kept by its answer and sent after it."
  (agent-repl-test-latency-report--with
    ;; Arrange
    (let (answer)
      (cl-letf (((symbol-function 'agent-repl-rpc-report-latencies)
                 (lambda (_conn request &rest keys)
                   (push request agent-repl-test-latency-report--sent)
                   (unless answer (setq answer (plist-get keys :on-response))))))
        (agent-repl-latency-report-startup (list (copy-sequence agent-repl-test-latency-report--milestone)))
        (agent-repl-latency-report-editor :stall 0.4)
        ;; Act
        (funcall answer (cadr agent-repl-test-latency-report--recorded))))
    ;; Assert
    (should (= (length agent-repl-test-latency-report--sent) 2))
    (should (equal (plist-get (car agent-repl-test-latency-report--sent) :editor-latencies)
                   '((:kind :stall :seconds 0.4))))
    (should-not (plist-get (car agent-repl-test-latency-report--sent) :editor-startup))))

(ert-deftest agent-repl-test-latency-report-no-link-keeps-the-queue ()
  "With no link standing nothing is sent, and the measurements wait."
  (agent-repl-test-latency-report--with
    ;; Arrange
    (setq agent-repl-test-latency-report--link nil)
    ;; Act
    (agent-repl-latency-report-startup (list (copy-sequence agent-repl-test-latency-report--milestone)))
    ;; Assert
    (should-not agent-repl-test-latency-report--sent)
    (should (= (length agent-repl-latency-report--startup) 1))))

(ert-deftest agent-repl-test-latency-report-nothing-queued-sends-nothing ()
  "A flush with empty queues makes no call."
  (agent-repl-test-latency-report--with
    ;; Act
    (agent-repl-latency-report-flush 'conn)
    ;; Assert
    (should-not agent-repl-test-latency-report--sent)))

(ert-deftest agent-repl-test-latency-report-the-queue-is-bounded-and-says-so ()
  "Past the bound the oldest editor latencies are dropped, at WARN, and counted."
  (agent-repl-test-latency-report--with
    ;; Arrange
    (setq agent-repl-test-latency-report--link nil)
    ;; Act
    (dotimes (i (+ agent-repl-latency-report-max-held 2))
      (agent-repl-latency-report-editor :stall (float i)))
    ;; Assert
    (should (= (length agent-repl-latency-report--editor) agent-repl-latency-report-max-held))
    (should (equal (car agent-repl-latency-report--editor) '(:kind :stall :seconds 2.0)))
    (should (= agent-repl-latency-report--dropped 2))
    (should (agent-repl-test-latency-report--recorded-p 'warn "elisp.latency-report.overflow dropped=2"))))

(ert-deftest agent-repl-test-latency-report-the-link-up-hook-flushes ()
  "A link that comes up sends what waited."
  (should (memq #'agent-repl-latency-report-flush agent-repl-link-up-functions)))

(provide 'test-latency-report)

;;; test-latency-report.el ends here
