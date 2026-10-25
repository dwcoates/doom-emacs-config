;;; test-profiling.el --- ERT tests for profiling.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for Emacs's corner of the runtime profiling session, asserted
;; against proto/vocab/profile-session.json like the Go and TypeScript
;; corners.
;;
;; Run with:
;;   emacs -batch -Q -l ert -l test-profiling.el -f ert-run-tests-batch-and-exit

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                           (or load-file-name buffer-file-name)))
      nil t)
(load (expand-file-name "profiling.el" (file-name-directory
                                         (or load-file-name buffer-file-name)))
      nil t)

(defconst agent-repl-test-profiling--fixture
  (expand-file-name "../proto/vocab/profile-session.json"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "The cross-runtime profiling session contract.")

(defun agent-repl-test-profiling--fixture ()
  "The parsed contract fixture."
  (json-parse-string (with-temp-buffer
                       (insert-file-contents agent-repl-test-profiling--fixture)
                       (buffer-string))
                     :object-type 'alist :array-type 'list))

(defmacro agent-repl-test-profiling--with-root (&rest body)
  "Run BODY with a fresh profiles root, no profile running and logs captured.
Binds `root' to the root and `logged' to the (LEVEL . MESSAGE) records."
  (declare (indent 0))
  `(let* ((root (make-temp-file "agent-repl-profiles" t))
          (process-environment (cons (concat agent-repl-profiling-root-env "=" root)
                                     process-environment))
          (agent-repl-profiling--session nil)
          (agent-repl-profiling--timer nil)
          (agent-repl-profiling--generation 0)
          (logged nil)
          (armed nil))
     (unwind-protect
         (cl-letf (((symbol-function 'agent-repl--info)
                    (lambda (_ws fmt &rest args) (push (cons 'info (apply #'format fmt args)) logged)))
                   ((symbol-function 'agent-repl--error)
                    (lambda (_ws fmt &rest args) (push (cons 'error (apply #'format fmt args)) logged)))
                   ((symbol-function 'run-at-time)
                    (lambda (_secs _repeat fn &rest args) (push (cons fn args) armed) (timer-create)))
                   ((symbol-function 'agent-repl--ws-list-names) (lambda () nil)))
           ,@body)
       (when (profiler-running-p)
         (profiler-memory-stop))
       (delete-directory root t))))

(defun agent-repl-test-profiling--open (root session)
  "Open SESSION under ROOT."
  (with-temp-file (expand-file-name "active" root) (insert session "\n")))

(defun agent-repl-test-profiling--close (root)
  "Close the open session under ROOT."
  (delete-file (expand-file-name "active" root)))

(ert-deftest agent-repl-test-profiling-contract-matches-the-fixture ()
  "The root variable, files and window are the fixture's."
  (let ((f (agent-repl-test-profiling--fixture)))
    (should (equal (alist-get 'root_env f) agent-repl-profiling-root-env))
    (should (equal (alist-get 'default_root f) (concat "~/" agent-repl-profiling-default-root)))
    (should (equal (alist-get 'active_file f) agent-repl-profiling-active-file))
    (should (equal (alist-get 'armed_dir f) agent-repl-profiling-armed-dir))
    (should (equal (alist-get 'max_seconds f) agent-repl-profiling-max-seconds))
    (should (equal (alist-get 'log_level_file f) agent-repl-profiling-log-level-file))))

(ert-deftest agent-repl-test-profiling-judges-every-fixture-session ()
  "Every session id in the fixture is judged as the fixture says."
  (dolist (case (alist-get 'sessions (agent-repl-test-profiling--fixture)))
    (should (eq (and (agent-repl-profiling-valid-session-p (alist-get 'id case)) t)
                (eq (alist-get 'valid case) t)))))

(ert-deftest agent-repl-test-profiling-root-prefers-the-override ()
  "The override variable names the root."
  (let ((process-environment (cons "AGENT_REPL_PROFILE_ROOT=/override" process-environment)))
    (should (equal (agent-repl-profiling-root) "/override"))))

(ert-deftest agent-repl-test-profiling-root-defaults-under-home ()
  "With no override the root is under the home directory."
  (let ((process-environment (cons "AGENT_REPL_PROFILE_ROOT" process-environment)))
    (should (equal (agent-repl-profiling-root)
                   (expand-file-name ".cache/agent-repl/profiles" "~")))))

(ert-deftest agent-repl-test-profiling-reconcile-with-no-session-is-unchanged ()
  "No active file and no profile: nothing happens."
  (agent-repl-test-profiling--with-root
    (should (equal (agent-repl-profiling-reconcile) "unchanged"))
    (should-not (profiler-running-p))))

(ert-deftest agent-repl-test-profiling-reconcile-starts-into-the-active-session ()
  "An open session starts the profiler and records it at info."
  (agent-repl-test-profiling--with-root
    (agent-repl-test-profiling--open root "s1")
    (should (equal (agent-repl-profiling-reconcile) "started s1 pages=0"))
    (should (profiler-running-p))
    (should (equal (car (car logged)) 'info))))

(ert-deftest agent-repl-test-profiling-never-starts-the-cpu-profiler ()
  "The SIGPROF CPU profiler stays off: its signals crash an Emacs hosting WebKit."
  (agent-repl-test-profiling--with-root
    (agent-repl-test-profiling--open root "s1")
    (agent-repl-profiling-reconcile)
    (should (profiler-memory-running-p))
    (should-not (profiler-cpu-running-p))))

(ert-deftest agent-repl-test-profiling-closing-the-session-writes-the-memory-profile ()
  "Closing the session stops the profiler and writes the memory profile and its tree."
  (agent-repl-test-profiling--with-root
    (agent-repl-test-profiling--open root "s1")
    (agent-repl-profiling-reconcile)
    (dotimes (_ 20000) (make-string 64 ?x))
    (agent-repl-test-profiling--close root)
    (let ((answer (agent-repl-profiling-reconcile)))
      (should (string-prefix-p "stopped s1 pages=0 " answer))
      (should-not (profiler-running-p))
      (should (equal (mapcar #'file-name-nondirectory (nthcdr 3 (split-string answer " ")))
                     (list (format "emacs-%d.mem.elprof" (emacs-pid))
                           (format "emacs-%d.mem.txt" (emacs-pid)))))
      (dolist (file (nthcdr 3 (split-string answer " ")))
        (should (file-exists-p file))))))

(ert-deftest agent-repl-test-profiling-a-changed-session-moves-the-profile ()
  "A different active session stops the old profile and starts the new."
  (agent-repl-test-profiling--with-root
    (agent-repl-test-profiling--open root "s1")
    (agent-repl-profiling-reconcile)
    (agent-repl-test-profiling--open root "s2")
    (let ((answer (agent-repl-profiling-reconcile)))
      (should (string-match-p "\\`stopped s1 .*; started s2 pages=0\\'" answer))
      (should (equal agent-repl-profiling--session "s2")))))

(ert-deftest agent-repl-test-profiling-reconcile-is-idempotent ()
  "Reconciling the running session again changes nothing."
  (agent-repl-test-profiling--with-root
    (agent-repl-test-profiling--open root "s1")
    (agent-repl-profiling-reconcile)
    (should (equal (agent-repl-profiling-reconcile) "unchanged"))))

(ert-deftest agent-repl-test-profiling-an-invalid-session-fails-loudly ()
  "An active file naming no valid session fails at error and starts nothing."
  (agent-repl-test-profiling--with-root
    (agent-repl-test-profiling--open root "../escape")
    (should (string-prefix-p "failed: " (agent-repl-profiling-reconcile)))
    (should-not (profiler-running-p))
    (should (equal (car (car logged)) 'error))))

(ert-deftest agent-repl-test-profiling-a-hand-started-profiler-is-not-clobbered ()
  "A profiler the user started by hand fails the reconcile instead."
  (agent-repl-test-profiling--with-root
    (profiler-memory-start)
    (agent-repl-test-profiling--open root "s1")
    (should (string-match-p "already running" (agent-repl-profiling-reconcile)))
    (should-not agent-repl-profiling--session)))

(ert-deftest agent-repl-test-profiling-the-max-duration-stops-the-profile ()
  "The armed max-duration stop ends the profile."
  (agent-repl-test-profiling--with-root
    (agent-repl-test-profiling--open root "s1")
    (agent-repl-profiling-reconcile)
    (let ((stop (car armed)))
      (apply (car stop) (cdr stop)))
    (should-not agent-repl-profiling--session)
    (should-not (profiler-running-p))))

(ert-deftest agent-repl-test-profiling-an-old-max-duration-leaves-a-later-profile ()
  "An earlier profile's max-duration stop does not end a later one."
  (agent-repl-test-profiling--with-root
    (agent-repl-test-profiling--open root "s1")
    (agent-repl-profiling-reconcile)
    (agent-repl-test-profiling--open root "s2")
    (agent-repl-profiling-reconcile)
    (let ((first (car (last armed))))
      (apply (car first) (cdr first)))
    (should (equal agent-repl-profiling--session "s2"))))

;;;; ---- the page sampler -------------------------------------------------

(ert-deftest agent-repl-test-profiling-pages-start-the-host ()
  "Starting a session injects the sampler into the host, which every page
shares a main thread with, by symbol."
  (agent-repl-test-profiling--with-root
    (let ((buf (generate-new-buffer " *host*"))
          (calls nil))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-repl-webhost-buffer) (lambda () buf))
                    ((symbol-function 'agent-repl--frontend-webview-read-script)
                     (lambda (b script cb) (push (list b script cb) calls) t)))
            (agent-repl-test-profiling--open root "s1")
            (should (equal (agent-repl-profiling-reconcile) "started s1 pages=1"))
            (should (equal (length calls) 1))
            (should (eq (nth 0 (car calls)) buf))
            (should (eq (nth 2 (car calls)) 'agent-repl-profiling--page-started)))
        (kill-buffer buf)))))

(ert-deftest agent-repl-test-profiling-pages-without-a-host-reach-none ()
  "With no host there is no page to sample."
  (agent-repl-test-profiling--with-root
    (cl-letf (((symbol-function 'agent-repl-webhost-buffer) (lambda () nil)))
      (agent-repl-test-profiling--open root "s1")
      (should (equal (agent-repl-profiling-reconcile) "started s1 pages=0")))))

(ert-deftest agent-repl-test-profiling-the-stop-script-carries-its-session-and-workspace ()
  "The stop script names the session and workspace its answer belongs to."
  (let ((script (agent-repl-profiling--page-stop-script "s1" "my ws")))
    (should (string-match-p "session:\"s1\"" script))
    (should (string-match-p "ws:\"my ws\"" script))))

(ert-deftest agent-repl-test-profiling-a-page-answer-is-written-into-its-session ()
  "A page's stop answer lands as webapp-<ws>.lag.json in its session."
  (agent-repl-test-profiling--with-root
    (make-directory (expand-file-name "s1" root))
    (agent-repl-profiling--page-stopped
     "{\"session\":\"s1\",\"ws\":\"my/ws\",\"ok\":true,\"stalls\":[[10,40]]}")
    (should (file-exists-p (expand-file-name "s1/webapp-my_ws.lag.json" root)))
    (should-not logged)))

(ert-deftest agent-repl-test-profiling-a-page-answer-naming-no-session-is-an-error ()
  "A page answer without a valid session writes nothing and is recorded."
  (agent-repl-test-profiling--with-root
    (agent-repl-profiling--page-stopped "{\"session\":\"../x\",\"ws\":\"a\"}")
    (should (equal (car (car logged)) 'error))))

(ert-deftest agent-repl-test-profiling-a-page-that-would-not-sample-is-an-error ()
  "A page whose sampler answered no is recorded at error."
  (agent-repl-test-profiling--with-root
    (agent-repl-profiling--page-started "{\"ok\":false,\"why\":\"already sampling\"}")
    (should (equal (car (car logged)) 'error))))

(ert-deftest agent-repl-test-profiling-a-started-page-records-nothing ()
  "A page whose sampler started records nothing."
  (agent-repl-test-profiling--with-root
    (agent-repl-profiling--page-started "{\"ok\":true}")
    (should-not logged)))

;;;; ---- a session's log level ---------------------------------------------

(defmacro agent-repl-test-profiling--with-levels (&rest body)
  "Run BODY in a profiles root with Emacs's and the webhost's level setters recorded.
Binds `levels' to the Emacs levels set and `hosted' to the (LEVEL . UNTIL-MS)
the webhost was told, newest first."
  (declare (indent 0))
  `(agent-repl-test-profiling--with-root
     (let ((levels nil) (hosted nil) (agent-repl-profiling--level-open nil))
       (cl-letf (((symbol-function 'agent-repl--set-log-level-window)
                  (lambda (level) (push level levels) (unless (eq level 'info) 1792000300.0)))
                 ((symbol-function 'agent-repl-webhost-set-log-level)
                  (lambda (level until-ms) (push (cons level until-ms) hosted) t)))
         ,@body))))

(defun agent-repl-test-profiling--ask-level (root session name)
  "Write NAME into SESSION's log level file under ROOT."
  (let ((dir (expand-file-name session root)))
    (make-directory dir t)
    (with-temp-file (expand-file-name "log-level" dir) (insert name "\n"))))

(ert-deftest agent-repl-test-profiling-a-session-level-raises-emacs ()
  (agent-repl-test-profiling--with-levels
    (agent-repl-test-profiling--ask-level root "s1" "debug")
    (agent-repl-test-profiling--open root "s1")
    (agent-repl-profiling-reconcile)
    (should (equal levels '(debug)))))

(ert-deftest agent-repl-test-profiling-a-session-level-raises-the-webhost-to-the-same-end ()
  (agent-repl-test-profiling--with-levels
    (agent-repl-test-profiling--ask-level root "s1" "debug")
    (agent-repl-test-profiling--open root "s1")
    (agent-repl-profiling-reconcile)
    (should (equal hosted '((debug . 1792000300000))))))

(ert-deftest agent-repl-test-profiling-a-session-level-is-answered-to-the-collector ()
  (agent-repl-test-profiling--with-levels
    (agent-repl-test-profiling--ask-level root "s1" "debug")
    (agent-repl-test-profiling--open root "s1")
    (should (equal (agent-repl-profiling-reconcile) "started s1 pages=0 level=debug"))))

(ert-deftest agent-repl-test-profiling-a-session-naming-no-level-raises-nothing ()
  (agent-repl-test-profiling--with-levels
    (agent-repl-test-profiling--open root "s1")
    (agent-repl-profiling-reconcile)
    (should (and (null levels) (null hosted)))))

(ert-deftest agent-repl-test-profiling-an-unknown-session-level-is-an-error-and-raises-nothing ()
  (agent-repl-test-profiling--with-levels
    (agent-repl-test-profiling--ask-level root "s1" "trace")
    (agent-repl-test-profiling--open root "s1")
    (agent-repl-profiling-reconcile)
    (should (and (null levels) (null hosted) (profiler-running-p)))
    (should (seq-some (lambda (r) (and (eq (car r) 'error) (string-search "names log level \"trace\"" (cdr r)))) logged))))

(ert-deftest agent-repl-test-profiling-the-stop-returns-every-level-to-info ()
  (agent-repl-test-profiling--with-levels
    (agent-repl-test-profiling--ask-level root "s1" "debug")
    (agent-repl-test-profiling--open root "s1")
    (agent-repl-profiling-reconcile)
    (agent-repl-test-profiling--close root)
    (agent-repl-profiling-reconcile)
    (should (equal levels '(info debug)))
    (should (equal (car hosted) '(info)))))

(ert-deftest agent-repl-test-profiling-a-stop-with-no-level-window-touches-no-level ()
  (agent-repl-test-profiling--with-levels
    (agent-repl-test-profiling--open root "s1")
    (agent-repl-profiling-reconcile)
    (agent-repl-test-profiling--close root)
    (agent-repl-profiling-reconcile)
    (should (and (null levels) (null hosted)))))

;;;; ---- the switch tour ---------------------------------------------------

(defmacro agent-repl-test-profiling--with-tour (&rest body)
  "Run BODY with no tour running, no key queued, and timers and logs captured.
Binds what `agent-repl-test-profiling--with-root' binds."
  (declare (indent 0))
  `(agent-repl-test-profiling--with-root
     (let ((agent-repl-profiling--tour nil)
           (unread-command-events nil))
       ,@body)))

(defun agent-repl-test-profiling--queued ()
  "The keys queued so far, as `kbd' strings."
  (mapcar (lambda (event) (key-description (vector event))) unread-command-events))

(defun agent-repl-test-profiling--run-step (armed)
  "Run the newest timer ARMED holds."
  (let ((step (car armed)))
    (apply (car step) (cdr step))))

(ert-deftest agent-repl-test-profiling-tour-queues-the-first-key-at-once ()
  (agent-repl-test-profiling--with-tour
    (agent-repl-profiling-switch-tour 2)
    (should (equal (agent-repl-test-profiling--queued) '("s-}")))))

(ert-deftest agent-repl-test-profiling-tour-answers-how-many-switches-it-runs ()
  (agent-repl-test-profiling--with-tour
    (should (equal (agent-repl-profiling-switch-tour 3) "touring 6 switches"))))

(ert-deftest agent-repl-test-profiling-tour-spaces-each-key-by-its-interval ()
  (agent-repl-test-profiling--with-tour
    (let (seconds)
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (secs _repeat _fn &rest _args) (push secs seconds) (timer-create))))
        (agent-repl-profiling-switch-tour 1))
      (should (equal seconds (list agent-repl-profiling-switch-tour-interval))))))

(ert-deftest agent-repl-test-profiling-tour-switches-right-then-back-left ()
  (agent-repl-test-profiling--with-tour
    (agent-repl-profiling-switch-tour 2)
    (dotimes (_ 3) (agent-repl-test-profiling--run-step armed))
    (should (equal (agent-repl-test-profiling--queued) '("s-}" "s-}" "s-{" "s-{")))))

(ert-deftest agent-repl-test-profiling-tour-ends-after-its-last-key ()
  (agent-repl-test-profiling--with-tour
    (agent-repl-profiling-switch-tour 1)
    (dotimes (_ 2) (agent-repl-test-profiling--run-step armed))
    (should (null agent-repl-profiling--tour))
    (should (member '(info . "profiling.switch-tour-done label=tour switches=2") logged))))

(ert-deftest agent-repl-test-profiling-tour-records-each-key-it-queues ()
  (agent-repl-test-profiling--with-tour
    (agent-repl-profiling-switch-tour 1)
    (should (member '(info . "profiling.switch-tour-key label=tour key=s-} remaining=1") logged))))

(ert-deftest agent-repl-test-profiling-tour-refuses-a-count-that-is-not-positive ()
  (agent-repl-test-profiling--with-tour
    (should-error (agent-repl-profiling-switch-tour 0))
    (should (null unread-command-events))
    (should (null agent-repl-profiling--tour))
    (should (member '(error . "profiling.switch-tour-refused: the count 0 is not a positive integer") logged))))

(ert-deftest agent-repl-test-profiling-tour-refuses-a-second-tour-while-one-runs ()
  (agent-repl-test-profiling--with-tour
    (agent-repl-profiling-switch-tour 2)
    (should-error (agent-repl-profiling-switch-tour 1))
    (should (equal (length (plist-get agent-repl-profiling--tour :keys)) 3))
    (should (member '(error . "profiling.switch-tour-refused: a tour is already running, 3 keys left") logged))))

(ert-deftest agent-repl-test-profiling-tour-cancel-stops-before-the-next-key ()
  (agent-repl-test-profiling--with-tour
    (agent-repl-profiling-switch-tour 2)
    (should (equal (agent-repl-profiling-switch-tour-cancel) "cancelled with 3 keys left"))
    (should (null agent-repl-profiling--tour))
    (should (member '(info . "profiling.switch-tour-cancelled keys-left=3") logged))))

(ert-deftest agent-repl-test-profiling-tour-spaces-keys-by-the-pace-it-names ()
  (agent-repl-test-profiling--with-tour
    (let (seconds)
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (secs _repeat _fn &rest _args) (push secs seconds) (timer-create))))
        (agent-repl-profiling-switch-tour 1 0.25 "fast"))
      (should (equal seconds '(0.25))))))

(ert-deftest agent-repl-test-profiling-tour-names-its-label-in-every-record ()
  (agent-repl-test-profiling--with-tour
    (agent-repl-profiling-switch-tour 1 0.25 "fast")
    (should (member '(info . "profiling.switch-tour-started label=fast count=1 interval=0.25") logged))
    (should (member '(info . "profiling.switch-tour-key label=fast key=s-} remaining=1") logged))))

(ert-deftest agent-repl-test-profiling-tour-refuses-a-pace-that-is-not-positive ()
  (agent-repl-test-profiling--with-tour
    (should-error (agent-repl-profiling-switch-tour 1 0 "fast"))
    (should (null agent-repl-profiling--tour))))

(ert-deftest agent-repl-test-profiling-tour-refuses-a-label-of-more-than-one-word ()
  (agent-repl-test-profiling--with-tour
    (should-error (agent-repl-profiling-switch-tour 1 1 "two words"))
    (should (null agent-repl-profiling--tour))))

(ert-deftest agent-repl-test-profiling-tour-state-names-a-running-tour ()
  (agent-repl-test-profiling--with-tour
    (agent-repl-profiling-switch-tour 2 1 "slow")
    (should (equal (agent-repl-profiling-switch-tour-state) "running slow 3"))))

(ert-deftest agent-repl-test-profiling-tour-state-is-idle-with-no-tour ()
  (agent-repl-test-profiling--with-tour
    (should (equal (agent-repl-profiling-switch-tour-state) "idle"))))

(ert-deftest agent-repl-test-profiling-tour-cancel-with-no-tour-says-so ()
  (agent-repl-test-profiling--with-tour
    (should (equal (agent-repl-profiling-switch-tour-cancel) "no tour running"))))

(provide 'test-profiling)
;;; test-profiling.el ends here
