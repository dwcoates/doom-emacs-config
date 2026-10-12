;;; test-startup.el --- ERT tests for agent-repl startup.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;   bin/background.sh emacs -batch -Q -l ert -l lisp/test-startup.el \
;;     -f ert-run-tests-batch-and-exit
;;
;; Doom's one-time setup, run idle after the editor's first open.  The open
;; itself is test-open-tabs.el's.

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Doom's one-time setup at idle ----

(defvar agent-repl-test-startup--file-hook nil "Stands in for `doom-first-file-hook'.")
(defvar agent-repl-test-startup--buffer-hook nil "Stands in for `doom-first-buffer-hook'.")
(defvar agent-repl-test-startup--hook-runs nil "Every hook the stubbed `doom-run-hooks' ran, in order.")
(defvar agent-repl-test-startup--armed nil "Every timer armed, as (KIND SECS FUNCTION).")

(defun agent-repl-test-startup--first-hooks-timer-p (fn)
  "Return non-nil when FN is one of the idle run's own timer functions."
  (memq fn '(agent-repl-startup--first-hooks-run
             agent-repl-startup--first-hooks-arm-or-run)))

(defmacro agent-repl-test-startup--with-first-hooks (idle &rest body)
  "Run BODY with stand-in first hooks, a stubbed Doom runner, and timers recorded.
IDLE is what `current-idle-time' answers, in seconds, or nil for not idle.
The stand-in hooks start empty; nothing is really scheduled, and only the
idle run's own timers are recorded (Emacs arms others, such as undo's)."
  (declare (indent 1))
  `(let ((agent-repl-startup--first-hooks '(agent-repl-test-startup--file-hook
                                            agent-repl-test-startup--buffer-hook))
         (agent-repl-startup--first-hooks-timer nil)
         (agent-repl-test-startup--file-hook nil)
         (agent-repl-test-startup--buffer-hook nil)
         (agent-repl-test-startup--hook-runs nil)
         (agent-repl-test-startup--armed nil))
     (cl-letf (((symbol-function 'doom-run-hooks)
                (lambda (&rest hooks)
                  (dolist (hook hooks)
                    (setq agent-repl-test-startup--hook-runs
                          (append agent-repl-test-startup--hook-runs (list hook)))
                    (run-hooks hook))))
               ((symbol-function 'current-idle-time)
                (lambda () (and ,idle (seconds-to-time ,idle))))
               ((symbol-function 'run-at-time)
                (lambda (secs _repeat fn &rest _)
                  (when (agent-repl-test-startup--first-hooks-timer-p fn)
                    (push (list 'clock secs fn) agent-repl-test-startup--armed)
                    'clock-timer)))
               ((symbol-function 'run-with-idle-timer)
                (lambda (secs _repeat fn &rest _)
                  (when (agent-repl-test-startup--first-hooks-timer-p fn)
                    (push (list 'idle secs fn) agent-repl-test-startup--armed)
                    'idle-timer))))
       ,@body)))

(ert-deftest agent-repl-test-startup-first-hooks-run-a-pending-hook-once ()
  "A pending hook is run through Doom's runner exactly once."
  (agent-repl-test-startup--with-first-hooks nil
    ;; Arrange
    (let ((calls 0))
      (setq agent-repl-test-startup--buffer-hook (list (lambda () (cl-incf calls))))
      ;; Act
      (agent-repl-startup--first-hooks-run)
      (agent-repl-startup--first-hooks-run)
      ;; Assert
      (should (= calls 1)))))

(ert-deftest agent-repl-test-startup-first-hooks-run-clears-the-hook ()
  "A hook the idle run ran is set to nil, as Doom clears it."
  (agent-repl-test-startup--with-first-hooks nil
    ;; Arrange
    (setq agent-repl-test-startup--buffer-hook (list #'ignore))
    ;; Act
    (agent-repl-startup--first-hooks-run)
    ;; Assert
    (should (null agent-repl-test-startup--buffer-hook))))

(ert-deftest agent-repl-test-startup-first-hooks-run-leaves-a-run-hook-alone ()
  "A hook already run (nil) is never handed to Doom's runner."
  (agent-repl-test-startup--with-first-hooks nil
    ;; Arrange
    (setq agent-repl-test-startup--buffer-hook (list #'ignore))
    ;; Act
    (agent-repl-startup--first-hooks-run)
    ;; Assert
    (should (equal agent-repl-test-startup--hook-runs
                   '(agent-repl-test-startup--buffer-hook)))))

(ert-deftest agent-repl-test-startup-first-hooks-run-file-before-buffer ()
  "Both pending hooks run file first, the order Doom runs them at init."
  (agent-repl-test-startup--with-first-hooks nil
    ;; Arrange
    (setq agent-repl-test-startup--file-hook (list #'ignore)
          agent-repl-test-startup--buffer-hook (list #'ignore))
    ;; Act
    (agent-repl-startup--first-hooks-run)
    ;; Assert
    (should (equal agent-repl-test-startup--hook-runs
                   '(agent-repl-test-startup--file-hook
                     agent-repl-test-startup--buffer-hook)))))

(ert-deftest agent-repl-test-startup-first-hooks-run-is-a-no-op-after-a-switch ()
  "A switch that ran the hook first (Doom cleared it) leaves the timer nothing to do."
  (agent-repl-test-startup--with-first-hooks nil
    ;; Arrange: Doom's own switch path runs the hook and clears it.
    (setq agent-repl-test-startup--buffer-hook (list #'ignore))
    (agent-repl-startup-first-hooks-arm)
    (setq agent-repl-test-startup--buffer-hook nil)
    ;; Act: the armed timer fires.
    (funcall (nth 2 (car agent-repl-test-startup--armed)))
    ;; Assert
    (should (null agent-repl-test-startup--hook-runs))))

(ert-deftest agent-repl-test-startup-first-hooks-run-clears-a-hook-that-signals ()
  "A hook that signals is still cleared, so no half-run hook is left behind."
  (agent-repl-test-startup--with-first-hooks nil
    ;; Arrange
    (setq agent-repl-test-startup--buffer-hook (list (lambda () (error "Boom"))))
    (cl-letf (((symbol-function 'agent-repl--error) #'ignore))
      ;; Act
      (ignore-errors (agent-repl-startup--first-hooks-run)))
    ;; Assert
    (should (null agent-repl-test-startup--buffer-hook))))

(ert-deftest agent-repl-test-startup-first-hooks-run-resignals-a-hook-failure ()
  "A hook's failure is re-signalled, never swallowed."
  (agent-repl-test-startup--with-first-hooks nil
    ;; Arrange
    (setq agent-repl-test-startup--buffer-hook (list (lambda () (error "Boom"))))
    (cl-letf (((symbol-function 'agent-repl--error) #'ignore))
      ;; Act / Assert
      (should-error (agent-repl-startup--first-hooks-run)))))

(ert-deftest agent-repl-test-startup-first-hooks-run-records-a-hook-failure ()
  "A hook's failure is recorded at the error level, naming the hook."
  (agent-repl-test-startup--with-first-hooks nil
    ;; Arrange
    (setq agent-repl-test-startup--buffer-hook (list (lambda () (error "Boom"))))
    (let ((errors nil))
      (cl-letf (((symbol-function 'agent-repl--error)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) errors))))
        ;; Act
        (ignore-errors (agent-repl-startup--first-hooks-run)))
      ;; Assert
      (should (string-match-p "hook=agent-repl-test-startup--buffer-hook failed"
                              (car errors))))))

(ert-deftest agent-repl-test-startup-first-hooks-run-logs-one-info-record ()
  "A run records one INFO line naming the hooks it ran and the milliseconds."
  (agent-repl-test-startup--with-first-hooks nil
    ;; Arrange
    (setq agent-repl-test-startup--buffer-hook (list #'ignore))
    (let ((infos nil))
      (cl-letf (((symbol-function 'agent-repl--info)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) infos))))
        ;; Act
        (agent-repl-startup--first-hooks-run))
      ;; Assert
      (should (= (length infos) 1))
      (should (string-match-p
               "\\`elisp.startup.first-hooks-idle ran=agent-repl-test-startup--buffer-hook ms=[0-9]+\\'"
               (car infos))))))

(ert-deftest agent-repl-test-startup-first-hooks-arm-uses-an-idle-timer-when-busy ()
  "Armed while Emacs is not idle, the run waits on an idle timer for the delay."
  (agent-repl-test-startup--with-first-hooks nil
    ;; Arrange
    (setq agent-repl-test-startup--buffer-hook (list #'ignore))
    ;; Act
    (agent-repl-startup-first-hooks-arm)
    ;; Assert
    (should (equal agent-repl-test-startup--armed
                   (list (list 'idle agent-repl-startup--first-hooks-idle-delay
                               #'agent-repl-startup--first-hooks-run))))))

(ert-deftest agent-repl-test-startup-first-hooks-arm-uses-the-clock-deep-into-idle ()
  "Armed after Emacs sat idle past the delay, the run waits the delay on the clock.
An idle timer armed then would wait for the stretch after the next key."
  (agent-repl-test-startup--with-first-hooks 30
    ;; Arrange
    (setq agent-repl-test-startup--buffer-hook (list #'ignore))
    ;; Act
    (agent-repl-startup-first-hooks-arm)
    ;; Assert
    (should (equal agent-repl-test-startup--armed
                   (list (list 'clock agent-repl-startup--first-hooks-idle-delay
                               #'agent-repl-startup--first-hooks-arm-or-run))))))

(ert-deftest agent-repl-test-startup-first-hooks-arm-arms-nothing-with-nothing-pending ()
  "With every hook already run, nothing is armed."
  (agent-repl-test-startup--with-first-hooks nil
    ;; Act
    (agent-repl-startup-first-hooks-arm)
    ;; Assert
    (should (null agent-repl-test-startup--armed))))

(ert-deftest agent-repl-test-startup-first-hooks-arm-arms-once ()
  "A second arm while one timer is armed arms nothing more."
  (agent-repl-test-startup--with-first-hooks nil
    ;; Arrange
    (setq agent-repl-test-startup--buffer-hook (list #'ignore))
    (agent-repl-startup-first-hooks-arm)
    ;; Act
    (agent-repl-startup-first-hooks-arm)
    ;; Assert
    (should (= (length agent-repl-test-startup--armed) 1))))

(ert-deftest agent-repl-test-startup-first-hooks-arm-or-run-runs-when-idle-long-enough ()
  "The clock timer, firing while Emacs still sits idle past the delay, runs the hooks."
  (agent-repl-test-startup--with-first-hooks 30
    ;; Arrange
    (setq agent-repl-test-startup--buffer-hook (list #'ignore))
    ;; Act
    (agent-repl-startup--first-hooks-arm-or-run)
    ;; Assert
    (should (equal agent-repl-test-startup--hook-runs
                   '(agent-repl-test-startup--buffer-hook)))))

(ert-deftest agent-repl-test-startup-first-hooks-arm-or-run-waits-after-input ()
  "The clock timer, firing after input broke the idle stretch, arms an idle timer."
  (agent-repl-test-startup--with-first-hooks 0.1
    ;; Arrange
    (setq agent-repl-test-startup--buffer-hook (list #'ignore))
    ;; Act
    (agent-repl-startup--first-hooks-arm-or-run)
    ;; Assert
    (should (null agent-repl-test-startup--hook-runs))
    (should (equal (car (car agent-repl-test-startup--armed)) 'idle))))

(provide 'test-startup)
;;; test-startup.el ends here
