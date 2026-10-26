;;; test-stall.el --- ERT tests for agent-repl stall.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;   bin/background.sh emacs -batch -Q -l ert -l lisp/test-stall.el \
;;     -f ert-run-tests-batch-and-exit

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

(defvar agent-repl-test-stall--reported nil "Every stall reported, newest first, as (KIND SECONDS).")
(defvar agent-repl-test-stall--records nil "Every info record, newest first.")
(defvar agent-repl-test-stall--clock 0.0 "What the stubbed wall clock answers.")
(defvar agent-repl-test-stall--pending nil "What the stubbed input check answers.")

(defmacro agent-repl-test-stall--with (&rest body)
  "Run BODY with a settable clock, a recording report and no prior run."
  (declare (indent 0))
  `(let ((agent-repl-stall--last nil)
         (agent-repl-stall--input nil)
         (agent-repl-test-stall--pending nil)
         (agent-repl-test-stall--reported nil)
         (agent-repl-test-stall--records nil)
         (agent-repl-test-stall--clock 100.0))
     (cl-letf (((symbol-function 'agent-repl--now) (lambda () agent-repl-test-stall--clock))
               ((symbol-function 'agent-repl-stall--input-pending-p)
                (lambda () agent-repl-test-stall--pending))
               ((symbol-function 'agent-repl-latency-report-editor)
                (lambda (kind seconds) (push (list kind seconds) agent-repl-test-stall--reported)))
               ((symbol-function 'agent-repl--info)
                (agent-repl-test--record-into agent-repl-test-stall--records)))
       ,@body)))

(defun agent-repl-test-stall--run-after (seconds)
  "Advance the clock by SECONDS and run the sampler."
  (setq agent-repl-test-stall--clock (+ agent-repl-test-stall--clock seconds))
  (agent-repl-stall-sample))

(ert-deftest agent-repl-test-stall-the-first-run-measures-nothing ()
  "The first run has no previous run to measure from."
  (agent-repl-test-stall--with
    ;; Act
    (agent-repl-stall-sample)
    ;; Assert
    (should-not agent-repl-test-stall--reported)
    (should (= agent-repl-stall--last 100.0))))

(ert-deftest agent-repl-test-stall-an-on-time-run-reports-nothing ()
  "A run one interval after the last is a healthy loop."
  (agent-repl-test-stall--with
    ;; Arrange
    (agent-repl-stall-sample)
    ;; Act
    (agent-repl-test-stall--run-after agent-repl-stall-interval)
    ;; Assert
    (should-not agent-repl-test-stall--reported)))

(ert-deftest agent-repl-test-stall-a-delay-under-the-floor-reports-nothing ()
  "A run late by less than the floor is a healthy loop."
  (agent-repl-test-stall--with
    ;; Arrange
    (agent-repl-stall-sample)
    ;; Act
    (agent-repl-test-stall--run-after (+ agent-repl-stall-interval (/ agent-repl-stall-floor 2)))
    ;; Assert
    (should-not agent-repl-test-stall--reported)))

(ert-deftest agent-repl-test-stall-a-delay-at-the-floor-is-a-stall ()
  "A run late by the floor exactly is reported."
  (agent-repl-test-stall--with
    ;; Arrange
    (agent-repl-stall-sample)
    ;; Act
    (agent-repl-test-stall--run-after (+ agent-repl-stall-interval agent-repl-stall-floor))
    ;; Assert
    (should (= (length agent-repl-test-stall--reported) 1))
    (should (eq (car (car agent-repl-test-stall--reported)) :stall))))

(ert-deftest agent-repl-test-stall-a-stall-is-the-gap-less-the-interval ()
  "A run 1.1s after the last reports a 1s stall."
  (agent-repl-test-stall--with
    ;; Arrange
    (agent-repl-stall-sample)
    ;; Act
    (agent-repl-test-stall--run-after (+ agent-repl-stall-interval 1.0))
    ;; Assert
    (should (< (abs (- (cadr (car agent-repl-test-stall--reported)) 1.0)) 1e-9))))

(ert-deftest agent-repl-test-stall-each-run-measures-from-the-one-before ()
  "Two stalls in a row are reported separately, each from its own previous run."
  (agent-repl-test-stall--with
    ;; Arrange
    (agent-repl-stall-sample)
    ;; Act
    (agent-repl-test-stall--run-after (+ agent-repl-stall-interval 0.5))
    (agent-repl-test-stall--run-after (+ agent-repl-stall-interval 0.2))
    ;; Assert
    (should (equal (mapcar (lambda (r) (/ (round (* 1000 (cadr r))) 1000.0)) agent-repl-test-stall--reported)
                   '(0.2 0.5)))))

(ert-deftest agent-repl-test-stall-a-gap-past-any-stall-is-recorded-not-reported ()
  "A gap longer than any stall (a host sleep) is recorded at INFO and not reported."
  (agent-repl-test-stall--with
    ;; Arrange
    (agent-repl-stall-sample)
    ;; Act
    (agent-repl-test-stall--run-after 3600.0)
    ;; Assert
    (should-not agent-repl-test-stall--reported)
    (should (string-match-p "elisp.stall.unclassified gap=3600.000" (car agent-repl-test-stall--records)))))

(ert-deftest agent-repl-test-stall-arming-registers-the-sampler-key-and-forgets-the-last-run ()
  "Arming goes through the timer registry under `:stall-sampler' and starts measuring afresh."
  (agent-repl-test-stall--with
    ;; Arrange
    (setq agent-repl-stall--last 5.0)
    (let (registered)
      (cl-letf (((symbol-function 'agent-repl--register-timer)
                 (lambda (key timer) (setq registered (list key timer)) timer))
                ((symbol-function 'run-at-time)
                 (lambda (secs repeat fn) (list 'timer secs repeat fn))))
        ;; Act
        (agent-repl--arm-stall-sampler))
      ;; Assert
      (should (equal registered
                     (list :stall-sampler
                           (list 'timer agent-repl-stall-interval agent-repl-stall-interval
                                 #'agent-repl-stall-sample))))
      (should-not agent-repl-stall--last))))

(ert-deftest agent-repl-test-stall-arming-forgets-an-input-wait ()
  "Arming starts the input measurement afresh too."
  (agent-repl-test-stall--with
    ;; Arrange
    (setq agent-repl-stall--input '(5.0 . 3))
    (cl-letf (((symbol-function 'agent-repl--register-timer) (lambda (_key timer) timer))
              ((symbol-function 'run-at-time) #'ignore))
      ;; Act
      (agent-repl--arm-stall-sampler))
    ;; Assert
    (should-not agent-repl-stall--input)))

;;;; ---- Input left waiting ----

(defun agent-repl-test-stall--runs-with-input (pending)
  "Run the sampler once per element of PENDING, one interval apart.
Each element is what the input check answers on that run."
  (dolist (answer pending)
    (setq agent-repl-test-stall--pending answer)
    (agent-repl-test-stall--run-after agent-repl-stall-interval)))

(ert-deftest agent-repl-test-stall-input-pending-across-runs-is-an-input-wait ()
  "Input found by three runs is reported from the first of them to the run that found none."
  (agent-repl-test-stall--with
    ;; Arrange
    (agent-repl-stall-sample)
    ;; Act
    (agent-repl-test-stall--runs-with-input '(t t t nil))
    ;; Assert
    (should (equal (mapcar (lambda (r) (list (car r) (/ (round (* 1000 (cadr r))) 1000.0)))
                           agent-repl-test-stall--reported)
                   '((:input-wait 0.3))))))

(ert-deftest agent-repl-test-stall-input-seen-by-one-run-is-no-wait ()
  "Input one run found and the next found read waited less than an interval."
  (agent-repl-test-stall--with
    ;; Arrange
    (agent-repl-stall-sample)
    ;; Act
    (agent-repl-test-stall--runs-with-input '(t nil))
    ;; Assert
    (should-not agent-repl-test-stall--reported)))

(ert-deftest agent-repl-test-stall-input-still-pending-reports-nothing-yet ()
  "A wait is reported only once its input is read."
  (agent-repl-test-stall--with
    ;; Arrange
    (agent-repl-stall-sample)
    ;; Act
    (agent-repl-test-stall--runs-with-input '(t t t))
    ;; Assert
    (should-not agent-repl-test-stall--reported)
    (should (equal (cdr agent-repl-stall--input) 3))))

(ert-deftest agent-repl-test-stall-each-input-wait-is-measured-on-its-own ()
  "Two waits with a read between them are reported separately."
  (agent-repl-test-stall--with
    ;; Arrange
    (agent-repl-stall-sample)
    ;; Act
    (agent-repl-test-stall--runs-with-input '(t t nil t t t nil))
    ;; Assert
    (should (equal (mapcar (lambda (r) (/ (round (* 1000 (cadr r))) 1000.0))
                           agent-repl-test-stall--reported)
                   '(0.3 0.2)))))

(ert-deftest agent-repl-test-stall-a-host-sleep-drops-an-input-wait ()
  "Input pending across a host sleep is not reported as a wait."
  (agent-repl-test-stall--with
    ;; Arrange
    (agent-repl-stall-sample)
    (agent-repl-test-stall--runs-with-input '(t t))
    ;; Act
    (setq agent-repl-test-stall--pending nil)
    (agent-repl-test-stall--run-after 3600.0)
    ;; Assert
    (should-not agent-repl-test-stall--reported)))

(ert-deftest agent-repl-test-stall-the-sampler-is-a-required-timer ()
  "The heartbeat assertion re-arms the sampler, through this file's arm function."
  (should (eq (cdr (assq :stall-sampler agent-repl--required-timer-keys))
              #'agent-repl--arm-stall-sampler)))

(defmacro agent-repl-test-stall--with-debug-on-quit (&rest body)
  "Run BODY with `debug-on-quit' nil as Lisp left it, and a recording log."
  (declare (indent 0))
  `(let ((debug-on-quit nil))
     (let ((agent-repl-stall--debug-on-quit-lisp nil)
           (agent-repl-test-stall--records nil))
       (cl-letf (((symbol-function 'agent-repl--info)
                  (agent-repl-test--record-into agent-repl-test-stall--records)))
         ,@body))))

(defun agent-repl-test-stall--write-from-c (value)
  "Set `debug-on-quit' to VALUE without its watcher, as Emacs's SIGUSR2 handler does."
  (remove-variable-watcher 'debug-on-quit #'agent-repl-stall--note-debug-on-quit)
  (unwind-protect (setq debug-on-quit value)
    (add-variable-watcher 'debug-on-quit #'agent-repl-stall--note-debug-on-quit)))

(ert-deftest agent-repl-test-stall-a-lisp-set-of-debug-on-quit-is-recorded ()
  "A Lisp `setq' of `debug-on-quit' is the value Lisp left."
  (agent-repl-test-stall--with-debug-on-quit
    ;; Act
    (setq debug-on-quit t)
    ;; Assert
    (should (eq agent-repl-stall--debug-on-quit-lisp t))))

(ert-deftest agent-repl-test-stall-a-let-of-debug-on-quit-is-recorded-while-it-holds ()
  "Inside a let, the let's value is the one Lisp left."
  (agent-repl-test-stall--with-debug-on-quit
    ;; Act
    (let ((debug-on-quit t))
      ;; Assert
      (should (eq agent-repl-stall--debug-on-quit-lisp t)))))

(ert-deftest agent-repl-test-stall-unbinding-debug-on-quit-records-the-outer-value ()
  "The end of a let leaves the outer value, and that is what is recorded."
  (agent-repl-test-stall--with-debug-on-quit
    ;; Arrange
    (setq debug-on-quit t)
    ;; Act
    (let ((debug-on-quit nil))
      (setq debug-on-quit nil))
    ;; Assert
    (should (eq agent-repl-stall--debug-on-quit-lisp t))))

(ert-deftest agent-repl-test-stall-a-buffer-local-debug-on-quit-is-not-recorded ()
  "A buffer-local set leaves the global value as it was."
  (agent-repl-test-stall--with-debug-on-quit
    ;; Act: the watcher is called directly, since making `debug-on-quit'
    ;; buffer-local for real changes how Emacs reports every later let.
    (agent-repl-stall--note-debug-on-quit 'debug-on-quit t 'set (current-buffer))
    ;; Assert
    (should-not agent-repl-stall--debug-on-quit-lisp)))

(ert-deftest agent-repl-test-stall-a-write-from-c-is-put-back-and-logged ()
  "SIGUSR2's write of `debug-on-quit' is undone as the debugger is entered."
  (agent-repl-test-stall--with-debug-on-quit
    ;; Arrange
    (agent-repl-test-stall--write-from-c t)
    ;; Act
    (agent-repl-stall--restore-debug-on-quit 'error '(quit))
    ;; Assert
    (should-not debug-on-quit)
    (should-not agent-repl-stall--debug-on-quit-lisp)
    (should (equal agent-repl-test-stall--records
                   (list (format "elisp.stall.debug-on-quit-restored from=t to=nil reason=set-outside-lisp event=%S"
                                 debug-on-event))))))

(ert-deftest agent-repl-test-stall-a-debug-on-quit-left-by-lisp-is-kept ()
  "A `debug-on-quit' turned on from Lisp survives entering the debugger, unlogged."
  (agent-repl-test-stall--with-debug-on-quit
    ;; Arrange
    (setq debug-on-quit t)
    ;; Act
    (agent-repl-stall--restore-debug-on-quit 'error '(quit))
    ;; Assert
    (should (eq debug-on-quit t))
    (should-not agent-repl-test-stall--records)))

(ert-deftest agent-repl-test-stall-the-debug-on-quit-guard-is-installed ()
  "Loading stall.el watches `debug-on-quit' and checks it as `debug' is entered."
  (should (memq #'agent-repl-stall--note-debug-on-quit (get-variable-watchers 'debug-on-quit)))
  (should (advice-member-p #'agent-repl-stall--restore-debug-on-quit 'debug)))

(provide 'test-stall)

;;; test-stall.el ends here
