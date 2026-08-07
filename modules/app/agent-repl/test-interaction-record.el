;;; test-interaction-record.el --- ERT tests for interaction-record.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the user-interaction recorder and its replayer.
;;
;; Run with:
;;   emacs -batch -Q -l ert -l test-interaction-record.el \
;;     -f ert-run-tests-batch-and-exit

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

(defmacro agent-repl-test--with-interaction-recorder (&rest body)
  "Run BODY with recorder and replayer globals scratch-bound.
Every one of these is a process global that the recorder or replayer
mutates, so leaking any of them would let one test decide whether a
later test records, replays, or refuses."
  (declare (indent 0))
  `(let ((agent-repl--interaction-record-events nil)
         (agent-repl--interaction-record-count 0)
         (agent-repl--interaction-record-started-at nil)
         (agent-repl-interaction-record-capacity
          agent-repl-interaction-record-capacity)
         (agent-repl-interaction-record-mode nil)
         (agent-repl--interaction-replay-id nil)
         (agent-repl--interaction-replay-timers nil)
         (agent-repl--interaction-replay-failures 0)
         (agent-repl-interaction-recording nil))
     (unwind-protect
         (progn ,@body)
       (remove-hook 'pre-command-hook #'agent-repl--interaction-record-capture))))

(defun agent-repl-test--interaction-write-recording (events)
  "Write EVENTS as a saved recording file and return its path.
EVENTS is a list of recorder-shaped plists (`:time', `:keys',
`:command'), oldest first."
  (let ((path (make-temp-file "agent-repl-test-recording-" nil ".el")))
    (with-temp-file path
      (insert (agent-repl--interaction-record-serialize events)))
    path))

;;;; ---- Tests: recording ----

(ert-deftest agent-repl-test-interaction-record-captures-command ()
  "Capture records the command symbol of the command about to run."
  (agent-repl-test--with-interaction-recorder
    (let ((this-command 'agent-repl-test-fake-command))
      (agent-repl--interaction-record-capture)
      (should (equal (plist-get (car (agent-repl--interaction-record-events-in-order))
                                :command)
                     'agent-repl-test-fake-command)))))

(ert-deftest agent-repl-test-interaction-record-captures-keys ()
  "Capture records the key sequence, which is the replay currency."
  (agent-repl-test--with-interaction-recorder
    (cl-letf (((symbol-function 'this-command-keys-vector) (lambda () [3 97])))
      (agent-repl--interaction-record-capture)
      (should (equal (plist-get (car (agent-repl--interaction-record-events-in-order))
                                :keys)
                     [3 97])))))

(ert-deftest agent-repl-test-interaction-record-captures-time ()
  "Capture records a subsecond wall-clock time for the event."
  (agent-repl-test--with-interaction-recorder
    (cl-letf (((symbol-function 'float-time) (lambda (&optional _t) 1234.5)))
      (agent-repl--interaction-record-capture)
      (should (equal (plist-get (car (agent-repl--interaction-record-events-in-order))
                                :time)
                     1234.5)))))

(ert-deftest agent-repl-test-interaction-record-capture-error-is-contained ()
  "A capture failure never signals into the user's command loop."
  (agent-repl-test--with-interaction-recorder
    (cl-letf (((symbol-function 'this-command-keys-vector)
               (lambda () (error "recorder boom"))))
      (should-not (agent-repl--interaction-record-capture)))))

(ert-deftest agent-repl-test-interaction-record-capture-error-is-logged ()
  "A capture failure is logged loudly rather than swallowed."
  (agent-repl-test--with-interaction-recorder
    (let ((warnings nil))
      (cl-letf (((symbol-function 'this-command-keys-vector)
                 (lambda () (error "recorder boom")))
                ((symbol-function 'agent-repl--warn)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) warnings))))
        (agent-repl--interaction-record-capture)
        (should (string-match-p "capture failed" (car warnings)))))))

(ert-deftest agent-repl-test-interaction-record-trim-drops-oldest ()
  "Exceeding capacity drops the OLDEST events and keeps the newest."
  (agent-repl-test--with-interaction-recorder
    (let ((agent-repl-interaction-record-capacity 10))
      (cl-letf (((symbol-function 'agent-repl--warn) #'ignore))
        (dotimes (i 11)
          (let ((this-command (intern (format "cmd-%d" i))))
            (agent-repl--interaction-record-capture)))
        (should (equal (plist-get (car (agent-repl--interaction-record-events-in-order))
                                  :command)
                       'cmd-2))))))

(ert-deftest agent-repl-test-interaction-record-mode-installs-hook ()
  "Enabling the mode installs the capture function on `pre-command-hook'."
  (agent-repl-test--with-interaction-recorder
    (unwind-protect
        (progn
          (agent-repl-interaction-record-mode 1)
          (should (memq #'agent-repl--interaction-record-capture pre-command-hook)))
      (agent-repl-interaction-record-mode -1))))

(ert-deftest agent-repl-test-interaction-record-mode-off-keeps-events ()
  "Disabling the mode stops capture but retains what was recorded."
  (agent-repl-test--with-interaction-recorder
    (agent-repl-interaction-record-mode 1)
    (let ((this-command 'agent-repl-test-fake-command))
      (agent-repl--interaction-record-capture))
    (agent-repl-interaction-record-mode -1)
    (should (= (length (agent-repl--interaction-record-events-in-order)) 1))))

(ert-deftest agent-repl-test-interaction-record-env-enables ()
  "A non-empty env var enables recording at module load."
  (agent-repl-test--with-interaction-recorder
    (let ((prior (getenv agent-repl-interaction-record-env)))
      (unwind-protect
          (progn
            (setenv agent-repl-interaction-record-env "1")
            (should (agent-repl--interaction-record-enable-from-env)))
        (agent-repl-interaction-record-mode -1)
        (setenv agent-repl-interaction-record-env prior)))))

(ert-deftest agent-repl-test-interaction-record-env-unset-does-not-enable ()
  "An unset env var leaves recording off."
  (agent-repl-test--with-interaction-recorder
    (let ((prior (getenv agent-repl-interaction-record-env)))
      (unwind-protect
          (progn
            (setenv agent-repl-interaction-record-env nil)
            (should-not (agent-repl--interaction-record-enable-from-env)))
        (setenv agent-repl-interaction-record-env prior)))))

;;;; ---- Tests: saving ----

(ert-deftest agent-repl-test-interaction-record-save-returns-path ()
  "Save returns the absolute path of the file it wrote."
  (agent-repl-test--with-interaction-recorder
    (setq agent-repl--interaction-record-events
          (list (list :time 100.0 :keys [97] :command 'self-insert-command)))
    (let ((path (agent-repl-interaction-record-save
                 (make-temp-file "agent-repl-test-save-" nil ".el"))))
      (unwind-protect
          (should (file-readable-p path))
        (delete-file path)))))

(ert-deftest agent-repl-test-interaction-record-save-round-trips ()
  "A saved recording loads back with its commands in order."
  (agent-repl-test--with-interaction-recorder
    (let ((path (agent-repl-test--interaction-write-recording
                 (list (list :time 100.0 :keys [97] :command 'cmd-a)
                       (list :time 100.5 :keys [98] :command 'cmd-b)))))
      (unwind-protect
          (should (equal (mapcar (lambda (e) (plist-get e :command))
                                 (plist-get (agent-repl--interaction-replay-load path)
                                            :events))
                         '(cmd-a cmd-b)))
        (delete-file path)))))

(ert-deftest agent-repl-test-interaction-record-save-records-offsets ()
  "A saved event carries its offset from the first event."
  (agent-repl-test--with-interaction-recorder
    (let ((path (agent-repl-test--interaction-write-recording
                 (list (list :time 100.0 :keys [97] :command 'cmd-a)
                       (list :time 100.5 :keys [98] :command 'cmd-b)))))
      (unwind-protect
          (should (equal (mapcar (lambda (e) (plist-get e :offset))
                                 (plist-get (agent-repl--interaction-replay-load path)
                                            :events))
                         '(0.0 0.5)))
        (delete-file path)))))

(ert-deftest agent-repl-test-interaction-record-save-empty-errors ()
  "Saving an empty recording signals instead of writing a file."
  (agent-repl-test--with-interaction-recorder
    (should-error (agent-repl-interaction-record-save
                   (expand-file-name "never-written.el" temporary-file-directory)))))

;;;; ---- Tests: replay ----

(ert-deftest agent-repl-test-interaction-replay-schedules-recorded-spacing ()
  "Replay schedules each event at its recorded offset."
  (agent-repl-test--with-interaction-recorder
    (let ((path (agent-repl-test--interaction-write-recording
                 (list (list :time 100.0 :keys [97] :command 'cmd-a)
                       (list :time 100.25 :keys [98] :command 'cmd-b)
                       (list :time 100.75 :keys [99] :command 'cmd-c))))
          (delays nil))
      (unwind-protect
          (let ((agent-repl-interaction-replay-schedule-function
                 (lambda (delay _repeat _fn) (push delay delays) nil)))
            (agent-repl-interaction-replay path)
            ;; The trailing entry is the replay-end marker, scheduled at
            ;; the last event's offset.
            (should (equal (butlast (reverse delays)) '(0.0 0.25 0.75))))
        (delete-file path)))))

(ert-deftest agent-repl-test-interaction-replay-speed-scales-delays ()
  "SPEED divides the recorded delays rather than dropping them."
  (agent-repl-test--with-interaction-recorder
    (let ((path (agent-repl-test--interaction-write-recording
                 (list (list :time 100.0 :keys [97] :command 'cmd-a)
                       (list :time 100.5 :keys [98] :command 'cmd-b))))
          (delays nil))
      (unwind-protect
          (let ((agent-repl-interaction-replay-schedule-function
                 (lambda (delay _repeat _fn) (push delay delays) nil)))
            (agent-repl-interaction-replay path 2.0)
            (should (equal (butlast (reverse delays)) '(0.0 0.25))))
        (delete-file path)))))

(ert-deftest agent-repl-test-interaction-replay-executes-recorded-keys ()
  "A scheduled event feeds its recorded key vector to the executor."
  (agent-repl-test--with-interaction-recorder
    (let ((path (agent-repl-test--interaction-write-recording
                 (list (list :time 100.0 :keys [97] :command 'cmd-a))))
          (executed nil)
          (callbacks nil))
      (unwind-protect
          (let ((agent-repl-interaction-replay-schedule-function
                 (lambda (_delay _repeat fn) (push fn callbacks) nil))
                (agent-repl-interaction-replay-execute-function
                 (lambda (keys) (push keys executed))))
            (agent-repl-interaction-replay path)
            (funcall (car (last callbacks)))
            (should (equal executed '([97]))))
        (delete-file path)))))

(ert-deftest agent-repl-test-interaction-replay-event-failure-is-counted ()
  "A failing event is recorded as a failure rather than aborting replay."
  (agent-repl-test--with-interaction-recorder
    (let ((agent-repl-interaction-replay-execute-function
           (lambda (_keys) (error "replay boom"))))
      (cl-letf (((symbol-function 'agent-repl--warn) #'ignore))
        (agent-repl--interaction-replay-run-event
         "replay-test" (list :index 0 :keys [97] :command 'cmd-a))
        (should (= agent-repl--interaction-replay-failures 1))))))

(ert-deftest agent-repl-test-interaction-replay-refuses-while-recording ()
  "Replay refuses to start while recording is active."
  (agent-repl-test--with-interaction-recorder
    (let ((path (agent-repl-test--interaction-write-recording
                 (list (list :time 100.0 :keys [97] :command 'cmd-a)))))
      (unwind-protect
          (let ((agent-repl-interaction-record-mode t))
            (should-error (agent-repl-interaction-replay path)))
        (delete-file path)))))

(ert-deftest agent-repl-test-interaction-replay-refuses-concurrent-replay ()
  "Replay refuses while another replay is already in flight."
  (agent-repl-test--with-interaction-recorder
    (let ((path (agent-repl-test--interaction-write-recording
                 (list (list :time 100.0 :keys [97] :command 'cmd-a)))))
      (unwind-protect
          (let ((agent-repl--interaction-replay-id "replay-in-flight"))
            (should-error (agent-repl-interaction-replay path)))
        (delete-file path)))))

(ert-deftest agent-repl-test-interaction-replay-rejects-nonpositive-speed ()
  "A zero or negative SPEED is rejected rather than dividing by zero."
  (agent-repl-test--with-interaction-recorder
    (let ((path (agent-repl-test--interaction-write-recording
                 (list (list :time 100.0 :keys [97] :command 'cmd-a)))))
      (unwind-protect
          (should-error (agent-repl-interaction-replay path 0))
        (delete-file path)))))

(ert-deftest agent-repl-test-interaction-replay-rejects-unknown-version ()
  "A recording carrying an unknown schema version is refused."
  (agent-repl-test--with-interaction-recorder
    (let ((path (make-temp-file "agent-repl-test-recording-" nil ".el")))
      (unwind-protect
          (progn
            (with-temp-file path
              (insert "(setq agent-repl-interaction-recording '(:version 999 :events nil))"))
            (should-error (agent-repl--interaction-replay-load path)))
        (delete-file path)))))

(ert-deftest agent-repl-test-interaction-replay-abort-cancels-timers ()
  "Aborting cancels every timer the in-flight replay scheduled."
  (agent-repl-test--with-interaction-recorder
    (let* ((cancelled 0)
           (agent-repl--interaction-replay-id "replay-test")
           (agent-repl--interaction-replay-timers (list 'timer-a 'timer-b)))
      (cl-letf (((symbol-function 'timerp) (lambda (_t) t))
                ((symbol-function 'cancel-timer) (lambda (_t) (cl-incf cancelled))))
        (agent-repl-interaction-replay-abort)
        (should (= cancelled 2))))))

;;;; ---- Test: end-to-end timing fidelity ----

;; This test drives the PRODUCTION recorder through the real command
;; loop: `execute-kbd-macro' runs `pre-command-hook' in batch exactly as
;; an interactive session does, so the events below come from
;; `agent-repl--interaction-record-capture' reading the genuine
;; `this-command' and `this-command-keys-vector', not from a simulation.
;;
;; `sleep-for' here is the TEST SUBJECT, not a synchronization device:
;; the assertion is about whether the recorder's timestamps reproduce
;; deliberately introduced inter-command gaps, so real elapsed time must
;; actually pass between the driven commands.  The repo's no-sleep rule
;; is about waiting for another agent's work to land; nothing is being
;; waited on here.
;;
;; Tolerance is max(5% of the interval, 30ms).  The percentage is the
;; user-specified fidelity bar; the absolute floor exists because a batch
;; Emacs's timer and `sleep-for' resolution jitters by milliseconds
;; regardless of the interval, so 5% of a 300ms gap (15ms) is inside the
;; scheduler's own noise.

(defvar agent-repl-test--interaction-e2e-log nil
  "Commands executed by the end-to-end timing test, for identity assertions.")

(defun agent-repl-test--interaction-e2e-alpha ()
  "Test command A driven by the end-to-end timing test."
  (interactive)
  (push 'alpha agent-repl-test--interaction-e2e-log))

(defun agent-repl-test--interaction-e2e-beta ()
  "Test command B driven by the end-to-end timing test."
  (interactive)
  (push 'beta agent-repl-test--interaction-e2e-log))

(defun agent-repl-test--interaction-e2e-gamma ()
  "Test command C driven by the end-to-end timing test."
  (interactive)
  (push 'gamma agent-repl-test--interaction-e2e-log))

(defun agent-repl-test--interaction-e2e-tolerance (expected)
  "Return the allowed absolute error for an EXPECTED interval in seconds."
  (max (* 0.05 expected) 0.030))

(ert-deftest agent-repl-test-interaction-record-e2e-timing-fidelity ()
  "Recorded intervals match deliberately driven delays within tolerance."
  (agent-repl-test--with-interaction-recorder
    (let ((map (make-sparse-keymap))
          (agent-repl-test--interaction-e2e-log nil)
          (gap-a-b 0.3)
          (gap-b-c 0.5)
          (path nil))
      (define-key map (kbd "C-c C-v a") #'agent-repl-test--interaction-e2e-alpha)
      (define-key map (kbd "C-c C-v b") #'agent-repl-test--interaction-e2e-beta)
      (define-key map (kbd "C-c C-v g") #'agent-repl-test--interaction-e2e-gamma)
      (unwind-protect
          (let ((overriding-terminal-local-map map))
            ;; Arrange: real recording, driven through the real command loop.
            (agent-repl-interaction-record-mode 1)
            ;; Act.
            (execute-kbd-macro (kbd "C-c C-v a"))
            (sleep-for gap-a-b)
            (execute-kbd-macro (kbd "C-c C-v b"))
            (sleep-for gap-b-c)
            (execute-kbd-macro (kbd "C-c C-v g"))
            (agent-repl-interaction-record-mode -1)
            (setq path (agent-repl-interaction-record-save
                        (make-temp-file "agent-repl-test-e2e-" nil ".el")))
            ;; Assert: identities and order first, then interval fidelity.
            (let* ((events (plist-get (agent-repl--interaction-replay-load path) :events))
                   (commands (mapcar (lambda (e) (plist-get e :command)) events))
                   (offsets (mapcar (lambda (e) (plist-get e :offset)) events)))
              (should (equal commands
                             '(agent-repl-test--interaction-e2e-alpha
                               agent-repl-test--interaction-e2e-beta
                               agent-repl-test--interaction-e2e-gamma)))
              (should (equal (reverse agent-repl-test--interaction-e2e-log)
                             '(alpha beta gamma)))
              (should (< (abs (- (nth 1 offsets) gap-a-b))
                         (agent-repl-test--interaction-e2e-tolerance gap-a-b)))
              (should (< (abs (- (- (nth 2 offsets) (nth 1 offsets)) gap-b-c))
                         (agent-repl-test--interaction-e2e-tolerance gap-b-c)))))
        (agent-repl-interaction-record-mode -1)
        (when (and path (file-exists-p path)) (delete-file path))))))

(provide 'test-interaction-record)

;;; test-interaction-record.el ends here
