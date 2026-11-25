;;; test-stall-culprit.el --- ERT tests for agent-repl stall-culprit.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Run through `agent-repld test run -ert lisp/test-stall-culprit.el'.

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

(defmacro agent-repl-test-stall-culprit--with (&rest body)
  "Run BODY with a settable clock and a fresh culprit window at 100.0."
  (declare (indent 0))
  `(let ((agent-repl-stall-culprit--current nil)
         (agent-repl-stall-culprit--since 100.0)
         (agent-repl-stall-culprit--window nil)
         (agent-repl-stall-culprit--gc-at gc-elapsed)
         (agent-repl-stall-culprit--suspended nil))
     (agent-repl-test--with-clock 100.0
       ,@body)))

(defun agent-repl-test-stall-culprit--advance (seconds)
  "Step the test clock by SECONDS."
  (setq agent-repl-test--clock (+ agent-repl-test--clock seconds)))

(defun agent-repl-test-stall-culprit--unit (key seconds &optional inner)
  "Run unit KEY for SECONDS, then INNER (a thunk) inside it."
  (agent-repl-stall-culprit-run
   key (lambda ()
         (agent-repl-test-stall-culprit--advance seconds)
         (when inner (funcall inner)))))

(ert-deftest agent-repl-test-stall-culprit-a-unit-is-charged-its-time ()
  "A unit's time lands in the window under its key."
  (agent-repl-test-stall-culprit--with
    ;; Act
    (agent-repl-test-stall-culprit--unit '(timer . agent-repl-a) 0.25)
    ;; Assert
    (should (equal agent-repl-stall-culprit--window '(((timer . agent-repl-a) . 0.25))))))

(ert-deftest agent-repl-test-stall-culprit-a-unit-answers-its-function-value ()
  "Timing a unit never changes what its function answers."
  (agent-repl-test-stall-culprit--with
    ;; Act / Assert
    (should (equal (agent-repl-stall-culprit-run '(timer . agent-repl-a) #'list 1 2) '(1 2)))))

(ert-deftest agent-repl-test-stall-culprit-repeated-runs-of-a-unit-add-up ()
  "Two runs of one unit in a window are one entry with their sum."
  (agent-repl-test-stall-culprit--with
    ;; Act
    (agent-repl-test-stall-culprit--unit '(timer . agent-repl-a) 0.25)
    (agent-repl-test-stall-culprit--unit '(timer . agent-repl-a) 0.5)
    ;; Assert
    (should (equal agent-repl-stall-culprit--window '(((timer . agent-repl-a) . 0.75))))))

(ert-deftest agent-repl-test-stall-culprit-a-nested-unit-pauses-its-parent ()
  "A unit run inside another is charged its own time; the parent only the rest."
  (agent-repl-test-stall-culprit--with
    ;; Act
    (agent-repl-test-stall-culprit--unit
     '(timer . agent-repl-outer) 0.25
     (lambda () (agent-repl-test-stall-culprit--unit '(process-filter . "WatchHostWorkspace") 0.5)))
    ;; Assert
    (should (equal (sort (copy-sequence agent-repl-stall-culprit--window)
                         (lambda (a b) (< (cdr a) (cdr b))))
                   '(((timer . agent-repl-outer) . 0.25)
                     ((process-filter . "WatchHostWorkspace") . 0.5))))))

(ert-deftest agent-repl-test-stall-culprit-a-unit-that-signals-restores-its-parent ()
  "A unit that exits non-locally is still charged, and its parent resumes."
  (agent-repl-test-stall-culprit--with
    ;; Act
    (agent-repl-stall-culprit-run
     '(command . agent-repl-outer)
     (lambda ()
       (ignore-errors
         (agent-repl-stall-culprit-run
          '(timer . agent-repl-fails)
          (lambda () (agent-repl-test-stall-culprit--advance 0.5) (error "Boom"))))
       ;; Assert
       (should (equal agent-repl-stall-culprit--current '(command . agent-repl-outer)))))
    ;; Assert
    (should (equal (cdr (assoc '(timer . agent-repl-fails) agent-repl-stall-culprit--window)) 0.5))
    (should-not agent-repl-stall-culprit--current)))

(ert-deftest agent-repl-test-stall-culprit-the-longest-unit-is-the-culprit ()
  "Of the timed units, the one that ran longest is named, with its time."
  (agent-repl-test-stall-culprit--with
    ;; Arrange
    (agent-repl-test-stall-culprit--unit '(timer . agent-repl-short) 0.125)
    (agent-repl-test-stall-culprit--unit '(command . agent-repl-long) 0.375)
    ;; Act
    (let ((taken (agent-repl-stall-culprit-take agent-repl-test--clock 0.5)))
      ;; Assert
      (should (equal (plist-get taken :culprit) '(:source command :function agent-repl-long)))
      (should (= (plist-get taken :ran) 0.375))
      (should (= (plist-get taken :timed) 0.5)))))

(ert-deftest agent-repl-test-stall-culprit-a-process-culprit-is-named-by-its-method ()
  "A filter or a sentinel culprit carries its rpc method, not a function."
  (agent-repl-test-stall-culprit--with
    ;; Arrange
    (agent-repl-test-stall-culprit--unit '(process-sentinel . "WatchHostWorkspace") 0.5)
    ;; Act / Assert
    (should (equal (plist-get (agent-repl-stall-culprit-take agent-repl-test--clock 0.5) :culprit)
                   '(:source process-sentinel :method "WatchHostWorkspace")))))

(ert-deftest agent-repl-test-stall-culprit-an-untimed-stall-is-unattributed ()
  "A stall that outlasts every timed unit by more than any of them ran is unattributed."
  (agent-repl-test-stall-culprit--with
    ;; Arrange
    (agent-repl-test-stall-culprit--unit '(timer . agent-repl-a) 0.125)
    ;; Act
    (let ((taken (agent-repl-stall-culprit-take agent-repl-test--clock 1.0)))
      ;; Assert
      (should (equal (plist-get taken :culprit) '(:source unattributed)))
      (should (= (plist-get taken :ran) 0.875)))))

(ert-deftest agent-repl-test-stall-culprit-a-window-with-nothing-timed-is-unattributed ()
  "With no unit timed, the whole stall is unattributed."
  (agent-repl-test-stall-culprit--with
    ;; Act / Assert
    (should (equal (plist-get (agent-repl-stall-culprit-take agent-repl-test--clock 0.25) :culprit)
                   '(:source unattributed)))))

(ert-deftest agent-repl-test-stall-culprit-garbage-collection-longer-than-any-unit-is-the-culprit ()
  "The collector's growth in the window beats a unit that ran less."
  (agent-repl-test-stall-culprit--with
    ;; Arrange
    (setq agent-repl-stall-culprit--gc-at (- gc-elapsed 0.4))
    (agent-repl-test-stall-culprit--unit '(timer . agent-repl-a) 0.25)
    ;; Act
    (let ((taken (agent-repl-stall-culprit-take agent-repl-test--clock 0.4)))
      ;; Assert
      (should (equal (plist-get taken :culprit) '(:source garbage-collection))))))

(ert-deftest agent-repl-test-stall-culprit-take-opens-a-new-window ()
  "Taking a culprit forgets the window, so the next stall starts empty."
  (agent-repl-test-stall-culprit--with
    ;; Arrange
    (agent-repl-test-stall-culprit--unit '(timer . agent-repl-a) 0.25)
    ;; Act
    (agent-repl-stall-culprit-take agent-repl-test--clock 0.25)
    ;; Assert
    (should-not agent-repl-stall-culprit--window)
    (should (= agent-repl-stall-culprit--since agent-repl-test--clock))))

(ert-deftest agent-repl-test-stall-culprit-a-running-unit-is-charged-up-to-the-take ()
  "The unit running when the culprit is taken is charged up to that instant and no more."
  (agent-repl-test-stall-culprit--with
    ;; Act
    (agent-repl-stall-culprit-run
     '(timer . agent-repl-outer)
     (lambda ()
       (agent-repl-test-stall-culprit--advance 0.5)
       ;; Assert
       (should (equal (plist-get (agent-repl-stall-culprit-take agent-repl-test--clock 0.5) :culprit)
                      '(:source timer :function agent-repl-outer)))
       (agent-repl-test-stall-culprit--advance 0.125)))
    ;; Assert
    (should (equal agent-repl-stall-culprit--window '(((timer . agent-repl-outer) . 0.125))))))

(ert-deftest agent-repl-test-stall-culprit-reset-forgets-the-window ()
  "A reset drops every charge and restarts the collector baseline."
  (agent-repl-test-stall-culprit--with
    ;; Arrange
    (agent-repl-test-stall-culprit--unit '(timer . agent-repl-a) 0.25)
    (setq agent-repl-stall-culprit--gc-at 0.0)
    ;; Act
    (agent-repl-stall-culprit-reset agent-repl-test--clock)
    ;; Assert
    (should-not agent-repl-stall-culprit--window)
    (should (= agent-repl-stall-culprit--gc-at gc-elapsed))))

(ert-deftest agent-repl-test-stall-culprit-a-minibuffer-wait-is-charged-to-no-unit ()
  "A command waiting in the minibuffer is not charged for the person's time."
  (agent-repl-test-stall-culprit--with
    ;; Act
    (agent-repl-stall-culprit-run
     '(command . execute-extended-command)
     (lambda ()
       (agent-repl-test-stall-culprit--advance 0.125)
       (agent-repl-stall-culprit--minibuffer-entered)
       (agent-repl-test-stall-culprit--advance 30.0)
       (agent-repl-stall-culprit--minibuffer-exited)
       (agent-repl-test-stall-culprit--advance 0.125)))
    ;; Assert
    (should (equal agent-repl-stall-culprit--window '(((command . execute-extended-command) . 0.25))))))

(ert-deftest agent-repl-test-stall-culprit-a-minibuffer-exit-resumes-the-suspended-unit ()
  "Leaving the minibuffer makes the unit it suspended current again."
  (agent-repl-test-stall-culprit--with
    ;; Arrange
    (setq agent-repl-stall-culprit--current '(command . agent-repl-a))
    (agent-repl-stall-culprit--minibuffer-entered)
    ;; Act
    (agent-repl-stall-culprit--minibuffer-exited)
    ;; Assert
    (should (equal agent-repl-stall-culprit--current '(command . agent-repl-a)))
    (should-not agent-repl-stall-culprit--suspended)))

(ert-deftest agent-repl-test-stall-culprit-a-timer-is-named-by-its-function ()
  "The timer advice times a timer under its function's symbol."
  (agent-repl-test-stall-culprit--with
    ;; Arrange
    (let ((timer (timer-create)))
      (timer-set-function timer #'agent-repl-test-stall-culprit--advance '(0.25))
      ;; Act
      (agent-repl-stall-culprit--around-timer (lambda (tm) (apply (timer--function tm) (timer--args tm))) timer))
    ;; Assert
    (should (equal agent-repl-stall-culprit--window
                   '(((timer . agent-repl-test-stall-culprit--advance) . 0.25))))))

(ert-deftest agent-repl-test-stall-culprit-a-closure-timer-is-anonymous ()
  "A timer whose function is a closure is timed as `:anonymous'."
  (agent-repl-test-stall-culprit--with
    ;; Arrange
    (let ((timer (timer-create)))
      (timer-set-function timer (lambda () (agent-repl-test-stall-culprit--advance 0.25)))
      ;; Act
      (agent-repl-stall-culprit--around-timer (lambda (tm) (funcall (timer--function tm))) timer))
    ;; Assert
    (should (equal agent-repl-stall-culprit--window '(((timer . :anonymous) . 0.25))))))

(ert-deftest agent-repl-test-stall-culprit-a-command-is-named-by-itself ()
  "The command advice times a command under its symbol."
  (agent-repl-test-stall-culprit--with
    ;; Act
    (agent-repl-stall-culprit--around-command
     (lambda (_cmd &rest _) (agent-repl-test-stall-culprit--advance 0.25)) 'agent-repl-switch)
    ;; Assert
    (should (equal agent-repl-stall-culprit--window '(((command . agent-repl-switch) . 0.25))))))

(ert-deftest agent-repl-test-stall-culprit-a-wrapped-handler-is-timed-under-its-method ()
  "A wrapped connection handler is timed under (SOURCE . METHOD) and passes its arguments on."
  (agent-repl-test-stall-culprit--with
    ;; Arrange
    (let ((handler (agent-repl-stall-culprit-wrap
                    'process-filter "WatchHostWorkspace"
                    (lambda (proc chunk) (agent-repl-test-stall-culprit--advance 0.25) (list proc chunk)))))
      ;; Act / Assert
      (should (equal (funcall handler 'p "bytes") '(p "bytes"))))
    ;; Assert
    (should (equal agent-repl-stall-culprit--window
                   '(((process-filter . "WatchHostWorkspace") . 0.25))))))

(ert-deftest agent-repl-test-stall-culprit-the-loop-entry-points-are-advised ()
  "Timers, commands and minibuffer waits all pass through the timing."
  ;; Assert
  (should (advice-member-p #'agent-repl-stall-culprit--around-timer 'timer-event-handler))
  (should (advice-member-p #'agent-repl-stall-culprit--around-command 'command-execute))
  (should (memq #'agent-repl-stall-culprit--minibuffer-entered minibuffer-setup-hook))
  (should (memq #'agent-repl-stall-culprit--minibuffer-exited minibuffer-exit-hook)))

(provide 'test-stall-culprit)

;;; test-stall-culprit.el ends here
