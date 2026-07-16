;;; test-notifications.el --- Tests for agent-repl notifications -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for notifications.el -- desktop notification backend selection,
;; dispatch, and the failure-surfacing spawn/sentinel layer.

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Tests: notification backend selection ----

(ert-deftest agent-repl-test-select-backend-prefers-alerter-when-all-available ()
  "When all are available, prefer alerter (delivers AND is clickable on macOS 26)."
  (cl-letf (((symbol-function 'executable-find)
             (lambda (_cmd) "/usr/bin/found")))
    (should (eq (agent-repl--select-notification-backend)
                #'agent-repl--notify-backend-alerter))))

(ert-deftest agent-repl-test-select-backend-uses-alerter-when-only-alerter ()
  "When only alerter is available, select alerter."
  (cl-letf (((symbol-function 'executable-find)
             (lambda (cmd) (when (equal cmd "alerter") "/usr/local/bin/alerter"))))
    (should (eq (agent-repl--select-notification-backend)
                #'agent-repl--notify-backend-alerter))))

(ert-deftest agent-repl-test-select-backend-falls-back-to-osascript-without-alerter ()
  "When alerter is absent but osascript is present, fall back to osascript."
  (cl-letf (((symbol-function 'executable-find)
             (lambda (cmd) (when (equal cmd "osascript") "/usr/bin/osascript"))))
    (should (eq (agent-repl--select-notification-backend)
                #'agent-repl--notify-backend-osascript))))

(ert-deftest agent-repl-test-select-backend-uses-osascript-when-only-osascript ()
  "When only osascript is available, select osascript."
  (cl-letf (((symbol-function 'executable-find)
             (lambda (cmd) (when (equal cmd "osascript") "/usr/bin/osascript"))))
    (should (eq (agent-repl--select-notification-backend)
                #'agent-repl--notify-backend-osascript))))

(ert-deftest agent-repl-test-select-backend-uses-terminal-notifier-when-only-terminal-notifier ()
  "When only terminal-notifier is available, select terminal-notifier."
  (cl-letf (((symbol-function 'executable-find)
             (lambda (cmd) (when (equal cmd "terminal-notifier") "/usr/local/bin/terminal-notifier"))))
    (should (eq (agent-repl--select-notification-backend)
                #'agent-repl--notify-backend-terminal-notifier))))

(ert-deftest agent-repl-test-select-backend-falls-back-to-terminal-notifier ()
  "When osascript is NOT available but terminal-notifier is, fall back to it."
  (cl-letf (((symbol-function 'executable-find)
             (lambda (cmd) (when (equal cmd "terminal-notifier") "/usr/local/bin/terminal-notifier"))))
    (should (eq (agent-repl--select-notification-backend)
                #'agent-repl--notify-backend-terminal-notifier))))

(ert-deftest agent-repl-test-select-backend-errors-when-none-available ()
  "When neither terminal-notifier nor osascript is available, signal an error."
  (cl-letf (((symbol-function 'executable-find) (lambda (_cmd) nil)))
    (should-error (agent-repl--select-notification-backend) :type 'error)))

;;;; ---- Tests: notification backend variable ----

(ert-deftest agent-repl-test-notification-backend-is-bound ()
  "The notification backend variable should be set at load time."
  (should (boundp 'agent-repl--notification-backend))
  (should (functionp agent-repl--notification-backend)))

;;;; ---- Tests: terminal-notifier backend ----

(ert-deftest agent-repl-test-terminal-notifier-spawns-with-args ()
  "terminal-notifier backend should spawn with -title/-message/-sound and a click -execute."
  (let (captured)
    (cl-letf (((symbol-function 'agent-repl--notify-spawn)
               (lambda (&rest args) (setq captured args) nil))
              ((symbol-function 'agent-repl--ensure-server) #'ignore))
      (agent-repl--notify-backend-terminal-notifier "ws-a" "My Title" "My Message")
      (should (equal captured
                     (list "ws-a" 'terminal-notifier "terminal-notifier"
                           (list "-title" "My Title"
                                 "-message" "My Message"
                                 "-sound" agent-repl-notification-sound
                                 "-execute" (agent-repl--notification-click-command "ws-a"))))))))

(ert-deftest agent-repl-test-terminal-notifier-nil-ws-omits-execute ()
  "With a nil WS the terminal-notifier backend must not add a -execute action."
  (let (captured)
    (cl-letf (((symbol-function 'agent-repl--notify-spawn)
               (lambda (&rest args) (setq captured args) nil))
              ((symbol-function 'agent-repl--ensure-server)
               (lambda () (error "ensure-server must not run for a non-clickable notification"))))
      (agent-repl--notify-backend-terminal-notifier nil "T" "M")
      (should-not (member "-execute" (nth 3 captured))))))

(ert-deftest agent-repl-test-terminal-notifier-ensures-server-when-clickable ()
  "The terminal-notifier backend must ensure the Emacs server for a clickable WS."
  (let (ensured)
    (cl-letf (((symbol-function 'agent-repl--notify-spawn) (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--ensure-server)
               (lambda () (setq ensured t))))
      (agent-repl--notify-backend-terminal-notifier "ws-a" "T" "M")
      (should ensured))))

(ert-deftest agent-repl-test-terminal-notifier-empty-strings ()
  "terminal-notifier backend should handle empty title and message."
  (let (captured)
    (cl-letf (((symbol-function 'agent-repl--notify-spawn)
               (lambda (&rest args) (setq captured args) nil)))
      (agent-repl--notify-backend-terminal-notifier "ws-a" "" "")
      ;; captured: (ws backend program ARGS); ARGS = (-title <title> -message <message> ...)
      (should (equal (nth 1 (nth 3 captured)) ""))
      (should (equal (nth 3 (nth 3 captured)) "")))))

(ert-deftest agent-repl-test-terminal-notifier-special-chars ()
  "terminal-notifier backend should pass through special characters."
  (let (captured)
    (cl-letf (((symbol-function 'agent-repl--notify-spawn)
               (lambda (&rest args) (setq captured args) nil)))
      (agent-repl--notify-backend-terminal-notifier "ws-a" "Title \"quoted\"" "Message with\nnewline")
      (should (equal (nth 1 (nth 3 captured)) "Title \"quoted\""))
      (should (equal (nth 3 (nth 3 captured)) "Message with\nnewline")))))

;;;; ---- Tests: osascript backend ----

(ert-deftest agent-repl-test-osascript-spawns-with-script ()
  "osascript backend should spawn osascript with a -e display-notification script."
  (let (captured)
    (cl-letf (((symbol-function 'agent-repl--notify-spawn)
               (lambda (&rest args) (setq captured args) nil)))
      (agent-repl--notify-backend-osascript "ws-a" "My Title" "My Message")
      ;; captured: (ws backend program ARGS); ARGS = (-e <script>)
      (should (equal (nth 0 captured) "ws-a"))
      (should (eq (nth 1 captured) 'osascript))
      (should (equal (nth 2 captured) "osascript"))
      (should (equal (nth 0 (nth 3 captured)) "-e"))
      (let ((script (nth 1 (nth 3 captured))))
        (should (string-match-p "display notification" script))
        (should (string-match-p "My Message" script))
        (should (string-match-p "My Title" script))
        (should (string-match-p "sound name" script))))))

(ert-deftest agent-repl-test-osascript-empty-strings ()
  "osascript backend should handle empty title and message."
  (let (captured)
    (cl-letf (((symbol-function 'agent-repl--notify-spawn)
               (lambda (&rest args) (setq captured args) nil)))
      (agent-repl--notify-backend-osascript "ws-a" "" "")
      (let ((script (nth 1 (nth 3 captured))))
        (should (string-match-p "display notification" script))))))

(ert-deftest agent-repl-test-osascript-special-chars ()
  "osascript backend should properly quote special characters in the script."
  (let (captured)
    (cl-letf (((symbol-function 'agent-repl--notify-spawn)
               (lambda (&rest args) (setq captured args) nil)))
      (agent-repl--notify-backend-osascript "ws-a" "Title \"quoted\"" "Line1\nLine2")
      (let ((script (nth 1 (nth 3 captured))))
        (should (stringp script))
        (should (string-match-p "display notification" script))))))

;;;; ---- Tests: alerter backend ----

(ert-deftest agent-repl-test-alerter-spawns-clickable-with-args ()
  "alerter backend should spawn with title/message/sound/sender/timeout and a ws group."
  (let (captured)
    (cl-letf (((symbol-function 'agent-repl--notify-spawn)
               (lambda (&rest args) (setq captured args) nil)))
      (agent-repl--notify-backend-alerter "ws-a" "My Title" "My Message")
      (should (equal (nth 0 captured) "ws-a"))
      (should (eq (nth 1 captured) 'alerter))
      (should (equal (nth 2 captured) agent-repl-alerter-executable))
      (should (equal (nth 3 captured)
                     (list "-title" "My Title"
                           "-message" "My Message"
                           "-sound" agent-repl-notification-sound
                           "-sender" agent-repl-notification-sender
                           "-timeout" (number-to-string
                                       agent-repl-notification-click-timeout-seconds)
                           "-group" "agent-repl:ws-a"))))))

(ert-deftest agent-repl-test-alerter-watchdog-outlives-click-timeout ()
  "The alerter watchdog must exceed alerter's own self-dismiss timeout."
  (let (captured)
    (cl-letf (((symbol-function 'agent-repl--notify-spawn)
               (lambda (&rest args) (setq captured args) nil)))
      (agent-repl--notify-backend-alerter "ws-a" "T" "M")
      ;; captured: (ws backend program args watchdog-seconds on-activate)
      (should (> (nth 4 captured) agent-repl-notification-click-timeout-seconds)))))

(ert-deftest agent-repl-test-alerter-nil-ws-omits-group ()
  "With a nil WS the alerter backend must not add a -group (no key to coalesce on)."
  (let (captured)
    (cl-letf (((symbol-function 'agent-repl--notify-spawn)
               (lambda (&rest args) (setq captured args) nil)))
      (agent-repl--notify-backend-alerter nil "T" "M")
      (should-not (member "-group" (nth 3 captured))))))

(ert-deftest agent-repl-test-alerter-on-activate-jumps-on-click ()
  "A click token from alerter must focus Emacs and switch to the workspace."
  (let (activated)
    (cl-letf (((symbol-function 'agent-repl--notify-spawn)
               (lambda (&rest args) (setq activated (nth 5 args)) nil))
              ((symbol-function 'agent-repl--notification-activate)
               (lambda (ws) (setq activated ws))))
      (agent-repl--notify-backend-alerter "ws-a" "T" "M")
      ;; activated now holds the on-activate lambda; drive it with a click token.
      (funcall activated "@CONTENTCLICKED")
      (should (equal activated "ws-a")))))

(ert-deftest agent-repl-test-alerter-on-activate-ignores-timeout ()
  "A timeout token from alerter must NOT focus Emacs or switch workspaces."
  (let (on-activate)
    (cl-letf (((symbol-function 'agent-repl--notify-spawn)
               (lambda (&rest args) (setq on-activate (nth 5 args)) nil))
              ((symbol-function 'agent-repl--notification-activate)
               (lambda (&rest _) (error "activate must not run on a non-click token"))))
      (agent-repl--notify-backend-alerter "ws-a" "T" "M")
      (funcall on-activate "@TIMEOUT"))))

;;;; ---- Tests: agent-repl--alerter-click-p ----

(ert-deftest agent-repl-test-alerter-click-p-recognizes-content-click ()
  "A `@CONTENTCLICKED' body click counts as a click."
  (should (agent-repl--alerter-click-p "@CONTENTCLICKED")))

(ert-deftest agent-repl-test-alerter-click-p-recognizes-action-click ()
  "A `@ACTIONCLICKED' action-button click counts as a click."
  (should (agent-repl--alerter-click-p "@ACTIONCLICKED")))

(ert-deftest agent-repl-test-alerter-click-p-rejects-dismissals ()
  "Timeout/close tokens and empty output are not clicks."
  (should-not (agent-repl--alerter-click-p "@TIMEOUT"))
  (should-not (agent-repl--alerter-click-p "@CLOSED"))
  (should-not (agent-repl--alerter-click-p ""))
  (should-not (agent-repl--alerter-click-p nil)))

(ert-deftest agent-repl-test-alerter-click-p-tolerates-surrounding-whitespace ()
  "A click token surrounded by whitespace still counts as a click."
  (should (agent-repl--alerter-click-p "  @CONTENTCLICKED\n")))

;;;; ---- Tests: agent-repl--notify-spawn ----

(ert-deftest agent-repl-test-notify-spawn-captures-output-into-buffer ()
  "spawn should pass a live capture buffer (not nil) to start-process."
  (let (captured-buffer)
    (cl-letf (((symbol-function 'start-process)
               (lambda (_name buffer &rest _args)
                 (setq captured-buffer buffer)
                 ;; Return a non-process so no real sentinel is attached.
                 nil)))
      (agent-repl--notify-spawn "ws-a" 'osascript "osascript" (list "-e" "noop"))
      (should (bufferp captured-buffer)))))

(ert-deftest agent-repl-test-notify-spawn-sets-sentinel-on-process ()
  "spawn should attach a sentinel to the spawned process."
  (let (sentinel-set)
    (cl-letf (((symbol-function 'start-process)
               (lambda (&rest _args) 'fake-proc))
              ((symbol-function 'processp) (lambda (obj) (eq obj 'fake-proc)))
              ((symbol-function 'run-at-time) (lambda (&rest _) 'fake-timer))
              ((symbol-function 'set-process-sentinel)
               (lambda (proc fn)
                 (setq sentinel-set (and (eq proc 'fake-proc) (functionp fn))))))
      (agent-repl--notify-spawn "ws-a" 'osascript "osascript" (list "-e" "noop"))
      (should sentinel-set))))

(ert-deftest agent-repl-test-notify-spawn-non-process-kills-buffer ()
  "When start-process returns a non-process, the capture buffer must be killed."
  (let (captured-buffer)
    (cl-letf (((symbol-function 'start-process)
               (lambda (_name buffer &rest _args)
                 (setq captured-buffer buffer)
                 nil)))
      (should (null (agent-repl--notify-spawn "ws-a" 'osascript "osascript" (list "-e" "noop"))))
      (should-not (buffer-live-p captured-buffer)))))

(ert-deftest agent-repl-test-notify-spawn-returns-process ()
  "spawn should return the spawned process when start-process yields one."
  (cl-letf (((symbol-function 'start-process)
             (lambda (&rest _args) 'fake-proc))
            ((symbol-function 'processp) (lambda (obj) (eq obj 'fake-proc)))
            ((symbol-function 'run-at-time) (lambda (&rest _) 'fake-timer))
            ((symbol-function 'set-process-sentinel) (lambda (&rest _) nil)))
    (should (eq (agent-repl--notify-spawn "ws-a" 'osascript "osascript" (list "-e" "noop"))
                'fake-proc))))

;;;; ---- Tests: hang watchdog ----

(ert-deftest agent-repl-test-notify-spawn-schedules-hang-watchdog ()
  "spawn should schedule the hang watchdog for the spawned process."
  (let (scheduled)
    (cl-letf (((symbol-function 'start-process)
               (lambda (&rest _args) 'fake-proc))
              ((symbol-function 'processp) (lambda (obj) (eq obj 'fake-proc)))
              ((symbol-function 'set-process-sentinel) (lambda (&rest _) nil))
              ((symbol-function 'run-at-time)
               (lambda (delay repeat fn &rest args)
                 (setq scheduled (list delay repeat fn args))
                 'fake-timer)))
      (agent-repl--notify-spawn "ws-a" 'osascript "osascript" (list "-e" "noop"))
      (should (equal (nth 0 scheduled) agent-repl-notify-timeout-seconds))
      (should (null (nth 1 scheduled)))
      (should (eq (nth 2 scheduled) #'agent-repl--notify-kill-hung))
      (should (equal (nth 3 scheduled) '(fake-proc "ws-a" osascript))))))

(ert-deftest agent-repl-test-notify-spawn-no-watchdog-for-non-process ()
  "spawn should not schedule a watchdog when start-process yields a non-process."
  (cl-letf (((symbol-function 'start-process) (lambda (&rest _args) nil))
            ((symbol-function 'run-at-time)
             (lambda (&rest _) (error "watchdog must not be scheduled without a process"))))
    (should (null (agent-repl--notify-spawn "ws-a" 'osascript "osascript" (list "-e" "noop"))))))

(ert-deftest agent-repl-test-kill-hung-deletes-live-process ()
  "The watchdog should delete a notification process still alive at timeout."
  (let (deleted)
    (cl-letf (((symbol-function 'processp) (lambda (obj) (eq obj 'fake-proc)))
              ((symbol-function 'process-live-p) (lambda (_p) t))
              ((symbol-function 'agent-repl--do-log) (lambda (&rest _) nil))
              ((symbol-function 'delete-process)
               (lambda (proc) (setq deleted proc))))
      (agent-repl--notify-kill-hung 'fake-proc "ws-a" 'terminal-notifier)
      (should (eq deleted 'fake-proc)))))

(ert-deftest agent-repl-test-kill-hung-logs-loudly ()
  "The watchdog should loudly log the hang via do-log, naming the backend."
  (let (do-log-args)
    (cl-letf (((symbol-function 'processp) (lambda (obj) (eq obj 'fake-proc)))
              ((symbol-function 'process-live-p) (lambda (_p) t))
              ((symbol-function 'delete-process) (lambda (_p) nil))
              ((symbol-function 'agent-repl--do-log)
               (lambda (_ws fmt args &optional _err) (setq do-log-args (cons fmt args)))))
      (agent-repl--notify-kill-hung 'fake-proc "ws-a" 'terminal-notifier)
      (should (string-match-p "HUNG" (car do-log-args)))
      (should (eq (nth 0 (cdr do-log-args)) 'terminal-notifier)))))

(ert-deftest agent-repl-test-kill-hung-noop-for-dead-process ()
  "The watchdog should not touch a process that already terminated."
  (cl-letf (((symbol-function 'processp) (lambda (obj) (eq obj 'fake-proc)))
            ((symbol-function 'process-live-p) (lambda (_p) nil))
            ((symbol-function 'agent-repl--do-log)
             (lambda (&rest _) (error "do-log must not fire for a terminated process")))
            ((symbol-function 'delete-process)
             (lambda (&rest _) (error "delete-process must not fire for a terminated process"))))
    (agent-repl--notify-kill-hung 'fake-proc "ws-a" 'osascript)))

(ert-deftest agent-repl-test-kill-hung-noop-for-non-process ()
  "The watchdog should not touch a non-process value (e.g. a test stub)."
  (cl-letf (((symbol-function 'delete-process)
             (lambda (&rest _) (error "delete-process must not fire for a non-process"))))
    (agent-repl--notify-kill-hung 'not-a-proc "ws-a" 'osascript)))

;;;; ---- Tests: agent-repl--notify-process-sentinel ----

(ert-deftest agent-repl-test-sentinel-logs-ok-on-zero-exit ()
  "Sentinel should log a gated `ok' line when the command exits zero."
  (let ((buffer (generate-new-buffer " *sentinel-ok*"))
        log-args)
    (unwind-protect
        (cl-letf (((symbol-function 'process-status) (lambda (_p) 'exit))
                  ((symbol-function 'process-exit-status) (lambda (_p) 0))
                  ((symbol-function 'agent-repl--log)
                   (lambda (_ws fmt &rest args) (setq log-args (cons fmt args))))
                  ((symbol-function 'agent-repl--do-log)
                   (lambda (&rest _) (error "do-log must not fire on success"))))
          (funcall (agent-repl--notify-process-sentinel "ws-a" 'osascript buffer)
                   'proc "finished\n")
          (should (string-match-p "notify-backend=%s ok" (car log-args)))
          (should (equal (cdr log-args) '(osascript))))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest agent-repl-test-sentinel-logs-failure-on-nonzero-exit ()
  "Sentinel should loudly log via do-log when the command exits non-zero."
  (let ((buffer (generate-new-buffer " *sentinel-fail*"))
        do-log-args)
    (unwind-protect
        (progn
          (with-current-buffer buffer (insert "boom: not allowed"))
          (cl-letf (((symbol-function 'process-status) (lambda (_p) 'exit))
                    ((symbol-function 'process-exit-status) (lambda (_p) 1))
                    ((symbol-function 'agent-repl--log)
                     (lambda (&rest _) (error "log must not fire on failure")))
                    ((symbol-function 'agent-repl--do-log)
                     (lambda (_ws fmt args &optional _err) (setq do-log-args (cons fmt args)))))
            (funcall (agent-repl--notify-process-sentinel "ws-a" 'osascript buffer)
                     'proc "exited abnormally with code 1\n")
            (should (string-match-p "FAILED" (car do-log-args)))
            ;; args list: (backend status event output)
            (should (eq (nth 0 (cdr do-log-args)) 'osascript))
            (should (equal (nth 1 (cdr do-log-args)) 1))
            (should (string-match-p "boom: not allowed" (nth 3 (cdr do-log-args))))))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest agent-repl-test-sentinel-kills-buffer-on-termination ()
  "Sentinel should kill the capture buffer once the process terminates."
  (let ((buffer (generate-new-buffer " *sentinel-kill*")))
    (cl-letf (((symbol-function 'process-status) (lambda (_p) 'exit))
              ((symbol-function 'process-exit-status) (lambda (_p) 0))
              ((symbol-function 'agent-repl--log) (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--do-log) (lambda (&rest _) nil)))
      (funcall (agent-repl--notify-process-sentinel "ws-a" 'osascript buffer)
               'proc "finished\n")
      (should-not (buffer-live-p buffer)))))

(ert-deftest agent-repl-test-sentinel-ignores-non-terminal-events ()
  "Sentinel should do nothing for non-terminal process states (e.g. `run')."
  (let ((buffer (generate-new-buffer " *sentinel-run*")))
    (unwind-protect
        (cl-letf (((symbol-function 'process-status) (lambda (_p) 'run))
                  ((symbol-function 'agent-repl--log)
                   (lambda (&rest _) (error "log must not fire while running")))
                  ((symbol-function 'agent-repl--do-log)
                   (lambda (&rest _) (error "do-log must not fire while running"))))
          (funcall (agent-repl--notify-process-sentinel "ws-a" 'osascript buffer)
                   'proc "run\n")
          ;; Buffer stays live because the process has not terminated.
          (should (buffer-live-p buffer)))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest agent-repl-test-sentinel-cancels-hang-watchdog ()
  "Sentinel should cancel the hang watchdog once the process terminates."
  (let ((buffer (generate-new-buffer " *sentinel-timer*"))
        (cell (list 'fake-timer))
        cancelled)
    (cl-letf (((symbol-function 'process-status) (lambda (_p) 'exit))
              ((symbol-function 'process-exit-status) (lambda (_p) 0))
              ((symbol-function 'agent-repl--log) (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--do-log) (lambda (&rest _) nil))
              ((symbol-function 'timerp) (lambda (obj) (eq obj 'fake-timer)))
              ((symbol-function 'cancel-timer) (lambda (tm) (setq cancelled tm))))
      (funcall (agent-repl--notify-process-sentinel "ws-a" 'osascript buffer cell)
               'proc "finished\n")
      (should (eq cancelled 'fake-timer)))))

(ert-deftest agent-repl-test-sentinel-clears-cancelled-watchdog-cell ()
  "Sentinel should clear the watchdog cell after cancelling the timer."
  (let ((buffer (generate-new-buffer " *sentinel-timer-cell*"))
        (cell (list 'fake-timer)))
    (cl-letf (((symbol-function 'process-status) (lambda (_p) 'exit))
              ((symbol-function 'process-exit-status) (lambda (_p) 0))
              ((symbol-function 'agent-repl--log) (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--do-log) (lambda (&rest _) nil))
              ((symbol-function 'timerp) (lambda (obj) (eq obj 'fake-timer)))
              ((symbol-function 'cancel-timer) (lambda (_tm) nil)))
      (funcall (agent-repl--notify-process-sentinel "ws-a" 'osascript buffer cell)
               'proc "finished\n")
      (should (null (car cell))))))

(ert-deftest agent-repl-test-sentinel-keeps-watchdog-armed-while-running ()
  "Sentinel must leave the watchdog armed for a non-terminal process state."
  (let ((buffer (generate-new-buffer " *sentinel-timer-run*"))
        (cell (list 'fake-timer)))
    (unwind-protect
        (cl-letf (((symbol-function 'process-status) (lambda (_p) 'run))
                  ((symbol-function 'timerp) (lambda (obj) (eq obj 'fake-timer)))
                  ((symbol-function 'cancel-timer)
                   (lambda (&rest _) (error "watchdog must stay armed while running"))))
          (funcall (agent-repl--notify-process-sentinel "ws-a" 'osascript buffer cell)
                   'proc "run\n")
          (should (eq (car cell) 'fake-timer)))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest agent-repl-test-sentinel-without-timer-cell-still-reports ()
  "Sentinel called without a watchdog cell should still log and kill the buffer."
  (let ((buffer (generate-new-buffer " *sentinel-no-cell*"))
        log-args)
    (cl-letf (((symbol-function 'process-status) (lambda (_p) 'exit))
              ((symbol-function 'process-exit-status) (lambda (_p) 0))
              ((symbol-function 'agent-repl--log)
               (lambda (_ws fmt &rest args) (setq log-args (cons fmt args))))
              ((symbol-function 'agent-repl--do-log) (lambda (&rest _) nil)))
      (funcall (agent-repl--notify-process-sentinel "ws-a" 'osascript buffer)
               'proc "finished\n")
      (should (string-match-p "notify-backend=%s ok" (car log-args)))
      (should-not (buffer-live-p buffer)))))

(ert-deftest agent-repl-test-sentinel-invokes-on-activate-with-output-on-zero-exit ()
  "Sentinel should call ON-ACTIVATE with the captured output when the command exits zero."
  (let ((buffer (generate-new-buffer " *sentinel-activate*"))
        got-output)
    (with-current-buffer buffer (insert "@CONTENTCLICKED"))
    (cl-letf (((symbol-function 'process-status) (lambda (_p) 'exit))
              ((symbol-function 'process-exit-status) (lambda (_p) 0))
              ((symbol-function 'agent-repl--log) (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--do-log) (lambda (&rest _) nil)))
      (funcall (agent-repl--notify-process-sentinel
                "ws-a" 'alerter buffer nil
                (lambda (output) (setq got-output output)))
               'proc "finished\n")
      (should (equal got-output "@CONTENTCLICKED")))))

(ert-deftest agent-repl-test-sentinel-skips-on-activate-on-nonzero-exit ()
  "Sentinel must NOT call ON-ACTIVATE when the command exits non-zero."
  (let ((buffer (generate-new-buffer " *sentinel-activate-fail*")))
    (unwind-protect
        (cl-letf (((symbol-function 'process-status) (lambda (_p) 'exit))
                  ((symbol-function 'process-exit-status) (lambda (_p) 1))
                  ((symbol-function 'agent-repl--log) (lambda (&rest _) nil))
                  ((symbol-function 'agent-repl--do-log) (lambda (&rest _) nil)))
          (funcall (agent-repl--notify-process-sentinel
                    "ws-a" 'alerter buffer nil
                    (lambda (&rest _) (error "on-activate must not run on failure")))
                   'proc "boom\n"))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest agent-repl-test-sentinel-logs-on-activate-error-without-signalling ()
  "An error signalled by ON-ACTIVATE is logged loudly via do-log, not propagated."
  (let ((buffer (generate-new-buffer " *sentinel-activate-err*"))
        do-log-args)
    (cl-letf (((symbol-function 'process-status) (lambda (_p) 'exit))
              ((symbol-function 'process-exit-status) (lambda (_p) 0))
              ((symbol-function 'agent-repl--log) (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--do-log)
               (lambda (_ws fmt args &optional _err) (setq do-log-args (cons fmt args)))))
      ;; Must return normally despite the on-activate error.
      (funcall (agent-repl--notify-process-sentinel
                "ws-a" 'alerter buffer nil
                (lambda (_output) (error "kaboom")))
               'proc "finished\n")
      (should (string-match-p "on-activate ERROR" (car do-log-args)))
      (should (eq (nth 0 (cdr do-log-args)) 'alerter))
      (should-not (buffer-live-p buffer)))))

;;;; ---- Tests: agent-repl--notify dispatch ----

(ert-deftest agent-repl-test-notify-dispatches-to-backend ()
  "agent-repl--notify should funcall the selected backend with ws/title/message."
  (let (called-with)
    (cl-letf ((agent-repl--notification-backend
               (lambda (ws title msg) (setq called-with (list ws title msg))))
              (agent-repl-debug nil)
              ((symbol-function 'agent-repl--log) (lambda (_ws _fmt &rest _args) nil)))
      (agent-repl--notify "ws-a" "Test Title" "Test Message")
      (should (equal called-with '("ws-a" "Test Title" "Test Message"))))))

(ert-deftest agent-repl-test-notify-logs-when-debug-enabled ()
  "agent-repl--notify should log when debug is enabled."
  (let (log-called)
    (cl-letf ((agent-repl--notification-backend (lambda (_ws _t _m) nil))
              (agent-repl-debug t)
              ((symbol-function 'agent-repl--log)
               (lambda (_ws _fmt &rest _args) (setq log-called t))))
      (agent-repl--notify nil "Title" "Message")
      (should log-called))))

(ert-deftest agent-repl-test-notify-logs-before-sending ()
  "agent-repl--notify should call log before dispatching to backend."
  (let (call-order)
    (cl-letf ((agent-repl--notification-backend
               (lambda (_ws _t _m) (push 'backend call-order)))
              (agent-repl-debug t)
              ((symbol-function 'agent-repl--log)
               (lambda (_ws _fmt &rest _args) (push 'log call-order))))
      (agent-repl--notify nil "Title" "Msg")
      ;; call-order is reversed because we push
      (should (equal call-order '(backend log))))))

(ert-deftest agent-repl-test-notify-does-not-log-when-debug-off ()
  "agent-repl--notify should not log when debug is nil."
  (cl-letf ((agent-repl--notification-backend (lambda (_ws _t _m) nil))
            (agent-repl-debug nil))
    ;; With debug nil, the real log function is a no-op, so we just
    ;; verify no error is raised.
    (agent-repl--notify nil "Title" "Message")))

;;;; ---- Tests: uncovered edge cases ----

(ert-deftest agent-repl-test-terminal-notifier-very-long-strings ()
  "terminal-notifier backend should handle very long title and message strings."
  (let* (captured
         (long-title (make-string 10000 ?T))
         (long-message (make-string 10000 ?M)))
    (cl-letf (((symbol-function 'agent-repl--notify-spawn)
               (lambda (&rest args) (setq captured args) nil)))
      (agent-repl--notify-backend-terminal-notifier "ws-a" long-title long-message)
      (should (equal (nth 1 (nth 3 captured)) long-title))
      (should (equal (nth 3 (nth 3 captured)) long-message)))))

(ert-deftest agent-repl-test-terminal-notifier-unicode-chars ()
  "terminal-notifier backend should pass through non-ASCII/Unicode characters."
  (let (captured)
    (cl-letf (((symbol-function 'agent-repl--notify-spawn)
               (lambda (&rest args) (setq captured args) nil)))
      (agent-repl--notify-backend-terminal-notifier "ws-a" "Héllo Wörld 你好" "Émoji 🎉 café")
      (should (equal (nth 1 (nth 3 captured)) "Héllo Wörld 你好"))
      (should (equal (nth 3 (nth 3 captured)) "Émoji 🎉 café")))))

(ert-deftest agent-repl-test-osascript-unicode-chars ()
  "osascript backend should include non-ASCII/Unicode characters in the script."
  (let (captured)
    (cl-letf (((symbol-function 'agent-repl--notify-spawn)
               (lambda (&rest args) (setq captured args) nil)))
      (agent-repl--notify-backend-osascript "ws-a" "Héllo 你好" "café 🎉")
      (let ((script (nth 1 (nth 3 captured))))
        (should (string-match-p "display notification" script))
        (should (string-match-p "café" script))
        (should (string-match-p "Héllo" script))))))

(ert-deftest agent-repl-test-osascript-start-process-returns-nil ()
  "osascript backend should not error when start-process returns nil."
  (cl-letf (((symbol-function 'start-process)
             (lambda (&rest _args) nil)))
    ;; Should complete without signaling an error (buffer cleanup path).
    (agent-repl--notify-backend-osascript "ws-a" "Title" "Message")))

(ert-deftest agent-repl-test-notify-backend-signals-error ()
  "agent-repl--notify should propagate errors from the backend function."
  (cl-letf ((agent-repl--notification-backend
             (lambda (_ws _title _msg) (error "Backend failed")))
            (agent-repl-debug nil)
            ((symbol-function 'agent-repl--log) (lambda (_ws _fmt &rest _args) nil)))
    (should-error (agent-repl--notify nil "Title" "Message")
                  :type 'error)))

;;;; ---- Tests: agent-repl--notification-click-command ----

(ert-deftest agent-repl-test-click-command-nil-ws-returns-nil ()
  "A nil WS produces no click command (notification stays non-clickable)."
  (should (null (agent-repl--notification-click-command nil))))

(ert-deftest agent-repl-test-click-command-empty-ws-returns-nil ()
  "An empty-string WS produces no click command."
  (should (null (agent-repl--notification-click-command ""))))

(ert-deftest agent-repl-test-click-command-invokes-emacsclient-eval ()
  "The click command should run emacsclient --eval on the configured binary."
  (let ((agent-repl-emacsclient-executable "/opt/emacs/bin/emacsclient"))
    (let ((cmd (agent-repl--notification-click-command "ws-a")))
      (should (string-match-p "/opt/emacs/bin/emacsclient" cmd))
      (should (string-match-p "--eval" cmd)))))

(ert-deftest agent-repl-test-click-command-embeds-activate-form-with-ws ()
  "The click command should eval `agent-repl--notification-activate' with WS."
  (let ((cmd (agent-repl--notification-click-command "DWC/foo")))
    (should (string-match-p "agent-repl--notification-activate" cmd))
    (should (string-match-p "DWC/foo" cmd))))

(ert-deftest agent-repl-test-click-command-shell-quotes-executable-with-spaces ()
  "An emacsclient path with spaces must be shell-quoted in the click command."
  (let ((agent-repl-emacsclient-executable "/Applications/Emacs.app/a b/emacsclient"))
    (let ((cmd (agent-repl--notification-click-command "ws-a")))
      ;; The raw space-containing path must be wrapped so the shell sees one arg.
      (should (string-match-p (regexp-quote (shell-quote-argument
                                             agent-repl-emacsclient-executable))
                              cmd)))))

;;;; ---- Tests: agent-repl--notification-activate ----

(ert-deftest agent-repl-test-activate-jumps-to-workspace ()
  "Activate should jump to the given workspace."
  (let (jumped)
    (cl-letf (((symbol-function 'agent-repl--log) (lambda (&rest _) nil))
              ((symbol-function 'agent-repl-jump-to-workspace)
               (lambda (ws &optional _no-flash) (setq jumped ws)))
              ((symbol-function 'select-frame-set-input-focus) (lambda (&rest _) nil))
              ((symbol-function 'selected-frame) (lambda () 'frame)))
      (agent-repl--notification-activate "ws-a")
      (should (equal jumped "ws-a")))))

(ert-deftest agent-repl-test-activate-focuses-selected-frame ()
  "Activate should focus the selected frame so Emacs comes forward."
  (let (focused)
    (cl-letf (((symbol-function 'agent-repl--log) (lambda (&rest _) nil))
              ((symbol-function 'agent-repl-jump-to-workspace) (lambda (&rest _) nil))
              ((symbol-function 'selected-frame) (lambda () 'the-frame))
              ((symbol-function 'select-frame-set-input-focus)
               (lambda (frame) (setq focused frame))))
      (agent-repl--notification-activate "ws-a")
      (should (eq focused 'the-frame)))))

(ert-deftest agent-repl-test-activate-nil-ws-focuses-without-jump ()
  "Activate with a nil WS should focus Emacs but not attempt a jump."
  (let ((jumped nil) (focused nil))
    (cl-letf (((symbol-function 'agent-repl--log) (lambda (&rest _) nil))
              ((symbol-function 'agent-repl-jump-to-workspace)
               (lambda (&rest _) (setq jumped t)))
              ((symbol-function 'selected-frame) (lambda () 'frame))
              ((symbol-function 'select-frame-set-input-focus)
               (lambda (&rest _) (setq focused t))))
      (agent-repl--notification-activate nil)
      (should-not jumped)
      (should focused))))

;;;; ---- Tests: agent-repl--ensure-server ----

(ert-deftest agent-repl-test-ensure-server-starts-when-not-running ()
  "ensure-server should start the server when none is running."
  (let (started)
    (cl-letf (((symbol-function 'require) (lambda (&rest _) t))
              ((symbol-function 'server-running-p) (lambda () nil))
              ((symbol-function 'server-start) (lambda (&rest _) (setq started t))))
      (let ((noninteractive nil))
        (agent-repl--ensure-server))
      (should started))))

(ert-deftest agent-repl-test-ensure-server-noop-when-already-running ()
  "ensure-server should not start a second server when one already runs."
  (cl-letf (((symbol-function 'require) (lambda (&rest _) t))
            ((symbol-function 'server-running-p) (lambda () t))
            ((symbol-function 'server-start)
             (lambda (&rest _) (error "server-start must not run when already running"))))
    (let ((noninteractive nil))
      (agent-repl--ensure-server))))

(ert-deftest agent-repl-test-ensure-server-noop-in-batch ()
  "ensure-server should be a no-op under `noninteractive' (batch/ERT)."
  (cl-letf (((symbol-function 'require)
             (lambda (&rest _) (error "require must not run in batch")))
            ((symbol-function 'server-start)
             (lambda (&rest _) (error "server-start must not run in batch"))))
    (let ((noninteractive t))
      (agent-repl--ensure-server))))

(provide 'test-notifications)

;;; test-notifications.el ends here
