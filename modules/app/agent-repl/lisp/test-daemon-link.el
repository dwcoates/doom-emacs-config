;;; test-daemon-link.el --- ERT tests for agent-repl daemon-link.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;   AGENT_REPL_FORBID_VENDOR_CALLS=1 emacs -batch -Q -l ert \
;;     -l lisp/test-daemon-link.el -f ert-run-tests-batch-and-exit
;;
;; NOTHING IS SPAWNED AND NOTHING SLEEPS.  The transport's own `struct' is
;; real (opening a connection opens no socket — Connect exchanges each dial
;; their own), so these tests use genuine connection objects and fake only
;; the two seams daemon-link.el actually depends on: the `daemon.addr' read
;; and the `WatchDaemon' stream.  The stream stub hands its ON-PUSH and
;; ON-CLOSE back to the test, which invokes them SYNCHRONOUSLY — that is
;; what makes every arm deterministic.
;;
;; ACCEPTANCE IS EXPLICIT.  The stream stub keeps the ON-OPEN it was handed
;; and never calls it; a test calls `agent-repl-test-link--accept' to say
;; the daemon accepted the subscription.  `agent-repl-test-link--connect'
;; opens AND accepts, because a standing link is most tests' arrangement;
;; `agent-repl-test-link--stand' opens WITHOUT accepting, which is how the
;; refused-before-acceptance cases are set up.
;;
;; Timers are captured, never scheduled: `run-with-timer' is stubbed to
;; record its callback and answer an un-armed timer object, and a test that
;; wants the next poll calls `agent-repl-link--reconnect-tick' itself.

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                           (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Harness ----

(defvar agent-repl-test-link--streams nil
  "Stream records, newest first: `(:conn C :on-push F :on-close F :id N)'.")

(defvar agent-repl-test-link--address nil
  "The address the stubbed `daemon.addr' read answers, or nil for absent.")

(defvar agent-repl-test-link--addr-reads 0
  "How many times the stubbed `daemon.addr' read was called.")

(defvar agent-repl-test-link--timers nil
  "Captured `run-with-timer' calls, newest first: `(SECONDS . FUNCTION)'.")

(defvar agent-repl-test-link--logs nil
  "Captured `(LEVEL . TEXT)' log entries, newest first.")

(defvar agent-repl-test-link--hooks nil
  "Hook firings, newest first: `(HOOK . ARGS)'.")

(defvar agent-repl-test-link--stream-counter 0
  "Monotonic id handed to each stubbed stream so tests can tell them apart.")

(defun agent-repl-test-link--record-hook (name)
  "Return a function recording its arguments against hook NAME."
  (lambda (&rest args)
    (push (cons name args) agent-repl-test-link--hooks)))

(defun agent-repl-test-link--logged-p (level substring)
  "Return non-nil when a LEVEL entry containing SUBSTRING was recorded."
  (seq-some (lambda (entry)
              (and (eq (car entry) level)
                   (string-search substring (cdr entry))))
            agent-repl-test-link--logs))

(defun agent-repl-test-link--stream-for (conn)
  "Return the newest stubbed stream record opened on CONN."
  (seq-find (lambda (record) (eq (plist-get record :conn) conn))
            agent-repl-test-link--streams))

(defmacro agent-repl-test-link--with-harness (&rest body)
  "Run BODY with daemon-link.el's world faked and its state reset."
  (declare (indent 0))
  `(let ((agent-repl-test-link--streams nil)
         (agent-repl-test-link--address nil)
         (agent-repl-test-link--addr-reads 0)
         (agent-repl-test-link--timers nil)
         (agent-repl-test-link--logs nil)
         (agent-repl-test-link--hooks nil)
         (agent-repl-test-link--stream-counter 0)
         (agent-repl-link--primary nil)
         (agent-repl-link--primary-stream nil)
         (agent-repl-link--successor nil)
         (agent-repl-link--successor-stream nil)
         (agent-repl-link--pending nil)
         (agent-repl-link--pending-stream nil)
         (agent-repl-link--pending-reconnect-p nil)
         (agent-repl-link--pending-successor nil)
         (agent-repl-link--pending-successor-stream nil)
         (agent-repl-link--reconnect-timer nil)
         (agent-repl-link--reconnect-interval nil)
         (agent-repl-link--quiet-until-ms nil)
         (agent-repl-link--bounce-cause nil)
         (agent-repl-link-drain nil)
         (agent-repl-link-drain-segment nil)
         (agent-repl-link-no-daemon-functions nil)
         (agent-repl-link-up-functions nil)
         (agent-repl-link-down-functions nil)
         (agent-repl-link-handover-functions nil)
         (agent-repl-link-drain-functions nil))
     (cl-letf (((symbol-function 'agent-repl-connect-read-daemon-addr)
                (lambda ()
                  (setq agent-repl-test-link--addr-reads
                        (1+ agent-repl-test-link--addr-reads))
                  agent-repl-test-link--address))
               ((symbol-function 'agent-repl-rpc-watch-daemon)
                (lambda (conn on-push on-close &optional on-open)
                  (setq agent-repl-test-link--stream-counter
                        (1+ agent-repl-test-link--stream-counter))
                  (let ((record (list :conn conn :on-push on-push :on-close on-close
                                      :on-open on-open
                                      :id agent-repl-test-link--stream-counter)))
                    (push record agent-repl-test-link--streams)
                    record)))
               ((symbol-function 'run-with-timer)
                (lambda (seconds _repeat function &rest _args)
                  (push (cons seconds function) agent-repl-test-link--timers)
                  (timer-create)))
               ((symbol-function 'agent-repl--log)
                (lambda (_ws fmt &rest args)
                  (push (cons :log (apply #'format fmt args)) agent-repl-test-link--logs)))
               ((symbol-function 'agent-repl--info)
                (lambda (_ws fmt &rest args)
                  (push (cons :info (apply #'format fmt args)) agent-repl-test-link--logs)))
               ((symbol-function 'agent-repl--warn)
                (lambda (_ws fmt &rest args)
                  (push (cons :warn (apply #'format fmt args)) agent-repl-test-link--logs)))
               ((symbol-function 'agent-repl--error)
                (lambda (_ws fmt &rest args)
                  (push (cons :error (apply #'format fmt args)) agent-repl-test-link--logs))))
       ,@body)))

(defun agent-repl-test-link--stand (address)
  "Open a link against ADDRESS WITHOUT the daemon accepting it.
Returns the pending connection, or nil when the open itself failed."
  (setq agent-repl-test-link--address address)
  (agent-repl-link-connect))

(defun agent-repl-test-link--accept (conn)
  "Say the daemon ACCEPTED the `WatchDaemon' subscription standing on CONN."
  (let ((on-open (plist-get (agent-repl-test-link--stream-for conn) :on-open)))
    (should on-open)
    (funcall on-open)))

(defun agent-repl-test-link--connect (address)
  "Stand a link against ADDRESS, accept it, and return the primary connection.
The two steps are separate in production — a spawn is not an acceptance —
so they are separate here, and this is the both-of-them convenience."
  (let ((conn (agent-repl-test-link--stand address)))
    (when conn (agent-repl-test-link--accept conn))
    agent-repl-link--primary))

(defun agent-repl-test-link--push (conn push)
  "Deliver PUSH to the stubbed `WatchDaemon' stream standing on CONN."
  (funcall (plist-get (agent-repl-test-link--stream-for conn) :on-push) push))

(defun agent-repl-test-link--close (conn outcome)
  "Close the stubbed `WatchDaemon' stream on CONN with OUTCOME."
  (funcall (plist-get (agent-repl-test-link--stream-for conn) :on-close) outcome))

(defun agent-repl-test-link--accept-pending ()
  "Accept whatever primary connection is currently pending acceptance."
  (should agent-repl-link--pending)
  (agent-repl-test-link--accept agent-repl-link--pending))

(defun agent-repl-test-link--announce-successor (conn address)
  "Announce a successor at ADDRESS on CONN and let it be ACCEPTED.
Returns the successor connection, which is nil until acceptance — that
gate is the point, so it is exercised here rather than bypassed."
  (agent-repl-test-link--push
   conn (list :arm :shutdown-announced
              :value (agent-repl-test-link--announcement :address address)))
  (when agent-repl-link--pending-successor
    (agent-repl-test-link--accept agent-repl-link--pending-successor))
  (agent-repl-link-successor))

(defun agent-repl-test-link--announcement (&rest overrides)
  "Return a `shutdown_announced' value plist, with OVERRIDES applied."
  (let ((base (list :address nil
                    :cause (list :arm :self-merge-rollout :value nil)
                    :expected-outage-ms 4000
                    :minted-at-ms 1000)))
    (while overrides
      (setq base (plist-put base (pop overrides) (pop overrides))))
    base))

;;;; ---- Connecting ----

(ert-deftest agent-repl-test-link-connect-absent-address-runs-no-daemon-hook ()
  "An absent `daemon.addr' is the legal no-daemon state: cold start is asked."
  (agent-repl-test-link--with-harness
    ;; Arrange
    (add-hook 'agent-repl-link-no-daemon-functions
              (agent-repl-test-link--record-hook :no-daemon))
    ;; Act
    (let ((conn (agent-repl-test-link--connect nil)))
      ;; Assert
      (should (null conn))
      (should (equal (assq :no-daemon agent-repl-test-link--hooks) '(:no-daemon))))))

(ert-deftest agent-repl-test-link-connect-absent-address-runs-no-up-hook ()
  "No daemon means no link, so the up hooks must not fire."
  (agent-repl-test-link--with-harness
    ;; Arrange
    (add-hook 'agent-repl-link-up-functions
              (agent-repl-test-link--record-hook :up))
    ;; Act
    (agent-repl-test-link--connect nil)
    ;; Assert
    (should (null (assq :up agent-repl-test-link--hooks)))))

(ert-deftest agent-repl-test-link-connect-runs-up-hook-with-the-connection ()
  "A standing `WatchDaemon' stream IS the link being up."
  (agent-repl-test-link--with-harness
    ;; Arrange
    (add-hook 'agent-repl-link-up-functions
              (agent-repl-test-link--record-hook :up))
    ;; Act
    (let ((conn (agent-repl-test-link--connect "127.0.0.1:9001")))
      ;; Assert
      (should (equal (cdr (assq :up agent-repl-test-link--hooks)) (list conn))))))

(ert-deftest agent-repl-test-link-connect-records-the-primary ()
  "The connection the link stands on is the primary."
  (agent-repl-test-link--with-harness
    ;; Arrange / Act
    (let ((conn (agent-repl-test-link--connect "127.0.0.1:9001")))
      ;; Assert
      (should (eq (agent-repl-link-primary) conn)))))

(ert-deftest agent-repl-test-link-connect-reports-up ()
  "`agent-repl-link-up-p' answers for a live primary."
  (agent-repl-test-link--with-harness
    ;; Arrange / Act
    (agent-repl-test-link--connect "127.0.0.1:9001")
    ;; Assert
    (should (agent-repl-link-up-p))))

(ert-deftest agent-repl-test-link-connect-is-idempotent-while-up ()
  "A second connect while the link stands opens no second stream."
  (agent-repl-test-link--with-harness
    ;; Arrange
    (agent-repl-test-link--connect "127.0.0.1:9001")
    ;; Act
    (agent-repl-link-connect)
    ;; Assert
    (should (= (length agent-repl-test-link--streams) 1))))

(ert-deftest agent-repl-test-link-connect-closes-the-connection-when-the-stream-refuses ()
  "A `WatchDaemon' that will not stand leaves no half-open connection."
  (agent-repl-test-link--with-harness
    ;; Arrange
    (setq agent-repl-test-link--address "127.0.0.1:9001")
    ;; Act
    (let (closed)
      (cl-letf (((symbol-function 'agent-repl-rpc-watch-daemon)
                 (lambda (&rest _) (error "no route to daemon")))
                ((symbol-function 'agent-repl-connect-close)
                 (lambda (conn) (setq closed conn))))
        (should (null (agent-repl-link-connect))))
      ;; Assert
      (should closed))))

(ert-deftest agent-repl-test-link-teardown-drops-the-primary ()
  "Teardown is the client-side close and leaves nothing standing."
  (agent-repl-test-link--with-harness
    ;; Arrange
    (agent-repl-test-link--connect "127.0.0.1:9001")
    ;; Act
    (agent-repl-link-teardown)
    ;; Assert
    (should (null (agent-repl-link-primary)))))

;;;; ---- The link dying ----

(ert-deftest agent-repl-test-link-cancelled-close-is-not-a-death ()
  "A client cancel is the graceful close and fires no down hooks."
  (agent-repl-test-link--with-harness
    ;; Arrange
    (let ((conn (agent-repl-test-link--connect "127.0.0.1:9001")))
      (add-hook 'agent-repl-link-down-functions
                (agent-repl-test-link--record-hook :down))
      ;; Act
      (agent-repl-test-link--close conn '(:cancelled))
      ;; Assert
      (should (null (assq :down agent-repl-test-link--hooks))))))

(ert-deftest agent-repl-test-link-error-close-runs-the-down-hook ()
  "A producer-side failure on the standing stream IS the link going down."
  (agent-repl-test-link--with-harness
    ;; Arrange
    (let ((conn (agent-repl-test-link--connect "127.0.0.1:9001")))
      (add-hook 'agent-repl-link-down-functions
                (agent-repl-test-link--record-hook :down))
      ;; Act
      (agent-repl-test-link--close conn '(:error (:kind :transport)))
      ;; Assert
      (should (equal (cdr (assq :down agent-repl-test-link--hooks)) (list conn))))))

(ert-deftest agent-repl-test-link-ended-close-is-a-failure-for-a-standing-stream ()
  "A standing stream the producer ENDS is a transport failure, not a close."
  (agent-repl-test-link--with-harness
    ;; Arrange
    (let ((conn (agent-repl-test-link--connect "127.0.0.1:9001")))
      (add-hook 'agent-repl-link-down-functions
                (agent-repl-test-link--record-hook :down))
      ;; Act
      (agent-repl-test-link--close conn '(:ended))
      ;; Assert
      (should (assq :down agent-repl-test-link--hooks)))))

(ert-deftest agent-repl-test-link-death-schedules-a-reconnect ()
  "The link going down arms the reconnect poll."
  (agent-repl-test-link--with-harness
    ;; Arrange
    (let ((conn (agent-repl-test-link--connect "127.0.0.1:9001")))
      (setq agent-repl-test-link--timers nil)
      ;; Act
      (agent-repl-test-link--close conn '(:error (:kind :transport)))
      ;; Assert
      (should (eq (cdar agent-repl-test-link--timers)
                  #'agent-repl-link--reconnect-tick)))))

(ert-deftest agent-repl-test-link-death-clears-the-primary ()
  "A dead link holds no connection: nothing may be sent over a corpse."
  (agent-repl-test-link--with-harness
    ;; Arrange
    (let ((conn (agent-repl-test-link--connect "127.0.0.1:9001")))
      ;; Act
      (agent-repl-test-link--close conn '(:error (:kind :transport)))
      ;; Assert
      (should (null (agent-repl-link-primary))))))

;;;; ---- The reconnect loop ----

(ert-deftest agent-repl-test-link-reconnect-tick-without-an-address-reschedules ()
  "No `daemon.addr' yet is not a failure: keep polling."
  (agent-repl-test-link--with-harness
    ;; Arrange
    (setq agent-repl-test-link--address nil)
    ;; Act
    (agent-repl-link--reconnect-tick)
    ;; Assert
    (should (eq (cdar agent-repl-test-link--timers)
                #'agent-repl-link--reconnect-tick))))

(ert-deftest agent-repl-test-link-reconnect-tick-restores-the-link ()
  "An address that appears is dialed and the up hooks run again."
  (agent-repl-test-link--with-harness
    ;; Arrange
    (add-hook 'agent-repl-link-up-functions
              (agent-repl-test-link--record-hook :up))
    (setq agent-repl-test-link--address "127.0.0.1:9002")
    ;; Act
    (agent-repl-link--reconnect-tick)
    (agent-repl-test-link--accept-pending)
    ;; Assert
    (should (equal (cdr (assq :up agent-repl-test-link--hooks))
                   (list (agent-repl-link-primary))))))

(ert-deftest agent-repl-test-link-reconnect-tick-honors-the-quiet-window ()
  "A plain bounce's announced outage is WAITED OUT, not polled through."
  (agent-repl-test-link--with-harness
    ;; Arrange
    (setq agent-repl-link--quiet-until-ms (+ (agent-repl-link--now-ms) 60000)
          agent-repl-test-link--address "127.0.0.1:9002"
          agent-repl-test-link--addr-reads 0)
    ;; Act
    (agent-repl-link--reconnect-tick)
    ;; Assert
    (should (= agent-repl-test-link--addr-reads 0))))

(ert-deftest agent-repl-test-link-reconnect-tick-polls-once-the-quiet-window-passed ()
  "An elapsed quiet window releases the poll."
  (agent-repl-test-link--with-harness
    ;; Arrange
    (setq agent-repl-link--quiet-until-ms (- (agent-repl-link--now-ms) 1)
          agent-repl-test-link--address "127.0.0.1:9002")
    ;; Act
    (agent-repl-link--reconnect-tick)
    (agent-repl-test-link--accept-pending)
    ;; Assert
    (should (agent-repl-link-up-p))))

(ert-deftest agent-repl-test-link-reconnect-backs-off-toward-the-ceiling ()
  "Repeated failed polls widen the interval instead of hammering at 1 Hz."
  (agent-repl-test-link--with-harness
    ;; Arrange
    (setq agent-repl-test-link--address nil)
    ;; Act
    (agent-repl-link--reconnect-tick)
    (agent-repl-link--reconnect-tick)
    ;; Assert
    (should (> (car (nth 0 agent-repl-test-link--timers))
               (car (nth 1 agent-repl-test-link--timers))))))

(ert-deftest agent-repl-test-link-reconnect-interval-never-exceeds-the-ceiling ()
  "The backoff is bounded by `agent-repl-link-reconnect-max-interval-seconds'."
  (agent-repl-test-link--with-harness
    ;; Arrange
    (setq agent-repl-test-link--address nil)
    ;; Act
    (dotimes (_ 10) (agent-repl-link--reconnect-tick))
    ;; Assert
    (should (<= (car (car agent-repl-test-link--timers))
                agent-repl-link-reconnect-max-interval-seconds))))

;;;; ---- The handover ----

(ert-deftest agent-repl-test-link-announcement-with-address-attaches-a-successor ()
  "An address on the announcement means a successor is already listening."
  (agent-repl-test-link--with-harness
    ;; Arrange
    (let ((conn (agent-repl-test-link--connect "127.0.0.1:9001")))
      ;; Act
      (agent-repl-test-link--announce-successor conn "127.0.0.1:9100")
      ;; Assert
      (should (equal (agent-repl-connect-connection-address (agent-repl-link-successor))
                     "127.0.0.1:9100")))))

(ert-deftest agent-repl-test-link-announcement-with-address-keeps-the-old-connection ()
  "DUAL ATTACH: the old daemon still serves every workspace it has not released."
  (agent-repl-test-link--with-harness
    ;; Arrange
    (let ((conn (agent-repl-test-link--connect "127.0.0.1:9001")))
      ;; Act
      (agent-repl-test-link--announce-successor conn "127.0.0.1:9100")
      ;; Assert
      (should (eq (agent-repl-link-primary) conn)))))

(ert-deftest agent-repl-test-link-announcement-with-address-runs-the-handover-hook ()
  "The handover hook receives BOTH connections, old first."
  (agent-repl-test-link--with-harness
    ;; Arrange
    (let ((conn (agent-repl-test-link--connect "127.0.0.1:9001")))
      (add-hook 'agent-repl-link-handover-functions
                (agent-repl-test-link--record-hook :handover))
      ;; Act
      (agent-repl-test-link--announce-successor conn "127.0.0.1:9100")
      ;; Assert
      (should (equal (cdr (assq :handover agent-repl-test-link--hooks))
                     (list conn (agent-repl-link-successor)))))))

(ert-deftest agent-repl-test-link-repeated-announcement-attaches-once ()
  "A re-announced handover is idempotent, never a second connection."
  (agent-repl-test-link--with-harness
    ;; Arrange
    (let* ((conn (agent-repl-test-link--connect "127.0.0.1:9001"))
           (push (list :arm :shutdown-announced
                       :value (agent-repl-test-link--announcement
                               :address "127.0.0.1:9100"))))
      (agent-repl-test-link--push conn push)
      ;; Act
      (agent-repl-test-link--push conn push)
      ;; Assert
      (should (= (length agent-repl-test-link--streams) 2)))))

(ert-deftest agent-repl-test-link-old-stream-closing-after-a-handover-promotes ()
  "The old daemon dropping its stream is the handover completing."
  (agent-repl-test-link--with-harness
    ;; Arrange
    (let ((conn (agent-repl-test-link--connect "127.0.0.1:9001")))
      (agent-repl-test-link--announce-successor conn "127.0.0.1:9100")
      (let ((successor (agent-repl-link-successor)))
        ;; Act
        (agent-repl-test-link--close conn '(:error (:kind :transport)))
        ;; Assert
        (should (eq (agent-repl-link-primary) successor))))))

(ert-deftest agent-repl-test-link-promotion-runs-no-down-hook ()
  "Nothing was lost in a promotion: the workspaces were already adopted."
  (agent-repl-test-link--with-harness
    ;; Arrange
    (let ((conn (agent-repl-test-link--connect "127.0.0.1:9001")))
      (agent-repl-test-link--announce-successor conn "127.0.0.1:9100")
      (add-hook 'agent-repl-link-down-functions
                (agent-repl-test-link--record-hook :down))
      ;; Act
      (agent-repl-test-link--close conn '(:error (:kind :transport)))
      ;; Assert
      (should (null (assq :down agent-repl-test-link--hooks))))))

(ert-deftest agent-repl-test-link-promotion-runs-no-up-hook ()
  "A promotion re-registers nothing: registration already happened on adopt."
  (agent-repl-test-link--with-harness
    ;; Arrange
    (let ((conn (agent-repl-test-link--connect "127.0.0.1:9001")))
      (agent-repl-test-link--announce-successor conn "127.0.0.1:9100")
      (add-hook 'agent-repl-link-up-functions
                (agent-repl-test-link--record-hook :up))
      ;; Act
      (agent-repl-test-link--close conn '(:error (:kind :transport)))
      ;; Assert
      (should (null (assq :up agent-repl-test-link--hooks))))))

(ert-deftest agent-repl-test-link-promotion-clears-the-successor ()
  "After a promotion there is no successor: the handover is over."
  (agent-repl-test-link--with-harness
    ;; Arrange
    (let ((conn (agent-repl-test-link--connect "127.0.0.1:9001")))
      (agent-repl-test-link--announce-successor conn "127.0.0.1:9100")
      ;; Act
      (agent-repl-test-link--close conn '(:error (:kind :transport)))
      ;; Assert
      (should (null (agent-repl-link-successor))))))

(ert-deftest agent-repl-test-link-successor-stream-loss-drops-the-successor ()
  "host.el must never adopt onto a successor that is already gone."
  (agent-repl-test-link--with-harness
    ;; Arrange
    (let ((conn (agent-repl-test-link--connect "127.0.0.1:9001")))
      (agent-repl-test-link--announce-successor conn "127.0.0.1:9100")
      ;; Act
      (agent-repl-test-link--close (agent-repl-link-successor)
                                   '(:error (:kind :transport)))
      ;; Assert
      (should (null (agent-repl-link-successor))))))

(ert-deftest agent-repl-test-link-successor-attach-failure-is-an-error ()
  "A successor that cannot be dialed is a loud failure, never a silent skip."
  (agent-repl-test-link--with-harness
    ;; Arrange
    (let ((conn (agent-repl-test-link--connect "127.0.0.1:9001")))
      ;; Act
      (cl-letf (((symbol-function 'agent-repl-rpc-watch-daemon)
                 (lambda (&rest _) (error "successor refused"))))
        (agent-repl-test-link--push
         conn (list :arm :shutdown-announced
                    :value (agent-repl-test-link--announcement
                            :address "127.0.0.1:9100"))))
      ;; Assert
      (should (agent-repl-test-link--logged-p
               :error "elisp.link.successor-attach-failed")))))

;;;; ---- The plain bounce ----

(ert-deftest agent-repl-test-link-plain-bounce-attaches-no-successor ()
  "No address means no successor exists to attach to."
  (agent-repl-test-link--with-harness
    ;; Arrange
    (let ((conn (agent-repl-test-link--connect "127.0.0.1:9001")))
      ;; Act
      (agent-repl-test-link--push
       conn (list :arm :shutdown-announced
                  :value (agent-repl-test-link--announcement)))
      ;; Assert
      (should (null (agent-repl-link-successor))))))

(ert-deftest agent-repl-test-link-plain-bounce-sizes-the-quiet-window-from-the-instants ()
  "The quiet window ends at `minted_at_ms + expected_outage_ms', an INSTANT."
  (agent-repl-test-link--with-harness
    ;; Arrange
    (let ((conn (agent-repl-test-link--connect "127.0.0.1:9001")))
      ;; Act
      (agent-repl-test-link--push
       conn (list :arm :shutdown-announced
                  :value (agent-repl-test-link--announcement
                          :minted-at-ms 1700000000000 :expected-outage-ms 7500)))
      ;; Assert
      (should (= agent-repl-link--quiet-until-ms 1700000007500)))))

(ert-deftest agent-repl-test-link-plain-bounce-draws-the-restarting-indicator ()
  "The indicator names the cause the daemon announced."
  (agent-repl-test-link--with-harness
    ;; Arrange
    (let ((conn (agent-repl-test-link--connect "127.0.0.1:9001")))
      ;; Act
      (agent-repl-test-link--push
       conn (list :arm :shutdown-announced
                  :value (agent-repl-test-link--announcement
                          :cause (list :arm :immediate :value nil)
                          :minted-at-ms (agent-repl-link--now-ms)
                          :expected-outage-ms 60000)))
      ;; Assert
      (should (equal agent-repl-link-drain-segment "daemon restarting (immediate)")))))

;;;; ---- The drain schedule ----

(ert-deftest agent-repl-test-link-drain-scheduled-records-the-schedule ()
  "The standing schedule is kept verbatim for every consumer."
  (agent-repl-test-link--with-harness
    ;; Arrange
    (let ((conn (agent-repl-test-link--connect "127.0.0.1:9001"))
          (schedule (list :at-ms 1700000000000
                          :reason (list :arm :deploy :value nil))))
      ;; Act
      (agent-repl-test-link--push conn (list :arm :drain-scheduled :value schedule))
      ;; Assert
      (should (equal agent-repl-link-drain schedule)))))

(ert-deftest agent-repl-test-link-drain-scheduled-runs-the-drain-hook ()
  "Consumers learn of the schedule through the hook, with its value."
  (agent-repl-test-link--with-harness
    ;; Arrange
    (let ((conn (agent-repl-test-link--connect "127.0.0.1:9001")))
      (add-hook 'agent-repl-link-drain-functions
                (agent-repl-test-link--record-hook :drain))
      ;; Act
      (agent-repl-test-link--push
       conn (list :arm :drain-scheduled
                  :value (list :at-ms 1700000000000
                               :reason (list :arm :deploy :value nil))))
      ;; Assert
      (should (equal (cdr (assq :drain agent-repl-test-link--hooks))
                     (list agent-repl-link-drain))))))

(ert-deftest agent-repl-test-link-drain-segment-names-a-deploy ()
  "The deploy arm draws its own word."
  (agent-repl-test-link--with-harness
    ;; Arrange
    (setq agent-repl-link-drain (list :at-ms 1700000000000
                                      :reason (list :arm :deploy :value nil)))
    ;; Act
    (let ((segment (agent-repl-link--compute-drain-segment)))
      ;; Assert
      (should (string-suffix-p " · deploy" segment)))))

(ert-deftest agent-repl-test-link-drain-segment-names-maintenance ()
  "The maintenance arm draws its own word."
  (agent-repl-test-link--with-harness
    ;; Arrange
    (setq agent-repl-link-drain (list :at-ms 1700000000000
                                      :reason (list :arm :maintenance :value nil)))
    ;; Act
    (let ((segment (agent-repl-link--compute-drain-segment)))
      ;; Assert
      (should (string-suffix-p " · maintenance" segment)))))

(ert-deftest agent-repl-test-link-drain-segment-draws-the-operator-note ()
  "The operator arm carries prose, and the prose is what the banner names."
  (agent-repl-test-link--with-harness
    ;; Arrange
    (setq agent-repl-link-drain
          (list :at-ms 1700000000000
                :reason (list :arm :operator :value (list :note "kernel patch"))))
    ;; Act
    (let ((segment (agent-repl-link--compute-drain-segment)))
      ;; Assert
      (should (string-suffix-p " · kernel patch" segment)))))

(ert-deftest agent-repl-test-link-drain-segment-draws-the-deadline-clock ()
  "The segment ticks off a SHIPPED INSTANT, rendered locally."
  (agent-repl-test-link--with-harness
    ;; Arrange
    (let* ((at-ms 1700000000000)
           (expected (format-time-string "%H:%M" (seconds-to-time (/ at-ms 1000.0)))))
      (setq agent-repl-link-drain (list :at-ms at-ms
                                        :reason (list :arm :deploy :value nil)))
      ;; Act
      (let ((segment (agent-repl-link--compute-drain-segment)))
        ;; Assert
        (should (string-prefix-p (format "drain %s" expected) segment))))))

(ert-deftest agent-repl-test-link-blank-operator-note-is-a-contract-breach ()
  "The note is REQUIRED non-blank at the request; a blank one is loud."
  (agent-repl-test-link--with-harness
    ;; Arrange
    (setq agent-repl-link-drain
          (list :at-ms 1700000000000
                :reason (list :arm :operator :value (list :note ""))))
    ;; Act
    (agent-repl-link--compute-drain-segment)
    ;; Assert
    (should (agent-repl-test-link--logged-p
             :error "elisp.link.drain-operator-note-blank"))))

(ert-deftest agent-repl-test-link-drain-cancelled-drops-the-schedule ()
  "Cancellation takes the banner down."
  (agent-repl-test-link--with-harness
    ;; Arrange
    (let ((conn (agent-repl-test-link--connect "127.0.0.1:9001")))
      (agent-repl-test-link--push
       conn (list :arm :drain-scheduled
                  :value (list :at-ms 1700000000000
                               :reason (list :arm :deploy :value nil))))
      ;; Act
      (agent-repl-test-link--push conn (list :arm :drain-cancelled :value nil))
      ;; Assert
      (should (null agent-repl-link-drain-segment)))))

(ert-deftest agent-repl-test-link-drain-cancelled-runs-the-hook-with-nil ()
  "Nil IS the cancellation, told to every consumer."
  (agent-repl-test-link--with-harness
    ;; Arrange
    (let ((conn (agent-repl-test-link--connect "127.0.0.1:9001")))
      (add-hook 'agent-repl-link-drain-functions
                (agent-repl-test-link--record-hook :drain))
      ;; Act
      (agent-repl-test-link--push conn (list :arm :drain-cancelled :value nil))
      ;; Assert
      (should (equal (cdr (assq :drain agent-repl-test-link--hooks)) (list nil))))))

;;;; ---- Refusals ----

(ert-deftest agent-repl-test-link-unknown-push-arm-is-an-error ()
  "An arm nobody knows is a contract breach, never a silent drop."
  (agent-repl-test-link--with-harness
    ;; Arrange
    (let ((conn (agent-repl-test-link--connect "127.0.0.1:9001")))
      ;; Act
      (agent-repl-test-link--push conn (list :arm :teleported :value nil))
      ;; Assert
      (should (agent-repl-test-link--logged-p
               :error "elisp.link.unknown-daemon-push")))))

(ert-deftest agent-repl-test-link-close-of-a-stale-connection-is-ignored ()
  "A connection this link no longer holds cannot take the link down."
  (agent-repl-test-link--with-harness
    ;; Arrange
    (let ((stale (agent-repl-connect-open "127.0.0.1:9500")))
      (agent-repl-test-link--connect "127.0.0.1:9001")
      (add-hook 'agent-repl-link-down-functions
                (agent-repl-test-link--record-hook :down))
      ;; Act
      (agent-repl-link--handle-close stale '(:error (:kind :transport)))
      ;; Assert
      (should (null (assq :down agent-repl-test-link--hooks))))))


;;;; ---- Acceptance: a spawn is not a link ----

(ert-deftest agent-repl-test-link-open-alone-does-not-report-up ()
  "A transport that the daemon has not accepted is not a link."
  (agent-repl-test-link--with-harness
    ;; Arrange / Act
    (agent-repl-test-link--stand "127.0.0.1:9001")
    ;; Assert
    (should-not (agent-repl-link-up-p))))

(ert-deftest agent-repl-test-link-open-alone-records-no-primary ()
  "The primary is set from acceptance and from nowhere else."
  (agent-repl-test-link--with-harness
    ;; Arrange / Act
    (agent-repl-test-link--stand "127.0.0.1:9001")
    ;; Assert
    (should (null (agent-repl-link-primary)))))

(ert-deftest agent-repl-test-link-open-alone-runs-no-up-hook ()
  "Registering a fleet against an unanswered connection would send it nowhere."
  (agent-repl-test-link--with-harness
    ;; Arrange
    (add-hook 'agent-repl-link-up-functions (agent-repl-test-link--record-hook :up))
    ;; Act
    (agent-repl-test-link--stand "127.0.0.1:9001")
    ;; Assert
    (should (null (assq :up agent-repl-test-link--hooks)))))

(ert-deftest agent-repl-test-link-acceptance-runs-the-up-hook ()
  "The daemon accepting the watch is what runs the up hooks."
  (agent-repl-test-link--with-harness
    ;; Arrange
    (add-hook 'agent-repl-link-up-functions (agent-repl-test-link--record-hook :up))
    (let ((conn (agent-repl-test-link--stand "127.0.0.1:9001")))
      ;; Act
      (agent-repl-test-link--accept conn)
      ;; Assert
      (should (equal (cdr (assq :up agent-repl-test-link--hooks)) (list conn))))))

(ert-deftest agent-repl-test-link-acceptance-reports-up ()
  "After acceptance the link stands."
  (agent-repl-test-link--with-harness
    ;; Arrange
    (let ((conn (agent-repl-test-link--stand "127.0.0.1:9001")))
      ;; Act
      (agent-repl-test-link--accept conn)
      ;; Assert
      (should (agent-repl-link-up-p)))))

(ert-deftest agent-repl-test-link-connect-while-pending-opens-no-second-transport ()
  "A second connect while one waits to be accepted would orphan a curl."
  (agent-repl-test-link--with-harness
    ;; Arrange
    (agent-repl-test-link--stand "127.0.0.1:9001")
    ;; Act
    (agent-repl-link-connect)
    ;; Assert
    (should (= (length agent-repl-test-link--streams) 1))))

(ert-deftest agent-repl-test-link-death-before-acceptance-runs-no-down-hook ()
  "A link that was never up cannot go down."
  (agent-repl-test-link--with-harness
    ;; Arrange
    (let ((conn (agent-repl-test-link--stand "127.0.0.1:9001")))
      (add-hook 'agent-repl-link-down-functions (agent-repl-test-link--record-hook :down))
      ;; Act
      (agent-repl-test-link--close conn '(:error (:kind :transport)))
      ;; Assert
      (should (null (assq :down agent-repl-test-link--hooks))))))

(ert-deftest agent-repl-test-link-death-before-acceptance-schedules-a-reconnect ()
  "A refused open goes straight back to the poll."
  (agent-repl-test-link--with-harness
    ;; Arrange
    (let ((conn (agent-repl-test-link--stand "127.0.0.1:9001")))
      ;; Act
      (agent-repl-test-link--close conn '(:error (:kind :transport)))
      ;; Assert
      (should (eq (cdar agent-repl-test-link--timers)
                  #'agent-repl-link--reconnect-tick)))))

(ert-deftest agent-repl-test-link-death-before-acceptance-logs-open-refused ()
  "The refusal is named on the record at WARNING."
  (agent-repl-test-link--with-harness
    ;; Arrange
    (let ((conn (agent-repl-test-link--stand "127.0.0.1:9001")))
      ;; Act
      (agent-repl-test-link--close conn '(:error (:kind :transport)))
      ;; Assert
      (should (agent-repl-test-link--logged-p :warn "elisp.link.open-refused")))))

(ert-deftest agent-repl-test-link-death-before-acceptance-clears-the-pending ()
  "A refused open leaves nothing behind for the next poll to trip over."
  (agent-repl-test-link--with-harness
    ;; Arrange
    (let ((conn (agent-repl-test-link--stand "127.0.0.1:9001")))
      ;; Act
      (agent-repl-test-link--close conn '(:error (:kind :transport)))
      ;; Assert
      (should (null agent-repl-link--pending)))))

(ert-deftest agent-repl-test-link-reconnect-open-alone-does-not-report-up ()
  "The reconnect loop is keyed on acceptance exactly as the first connect is."
  (agent-repl-test-link--with-harness
    ;; Arrange
    (setq agent-repl-test-link--address "127.0.0.1:9002")
    ;; Act
    (agent-repl-link--reconnect-tick)
    ;; Assert
    (should-not (agent-repl-link-up-p))))

(ert-deftest agent-repl-test-link-reconnect-acceptance-logs-a-reconnect ()
  "A recovery is still distinguishable from a cold start on the record."
  (agent-repl-test-link--with-harness
    ;; Arrange
    (setq agent-repl-test-link--address "127.0.0.1:9002")
    (agent-repl-link--reconnect-tick)
    ;; Act
    (agent-repl-test-link--accept-pending)
    ;; Assert
    (should (agent-repl-test-link--logged-p :info "elisp.link.reconnected"))))

(ert-deftest agent-repl-test-link-reconnect-tick-while-pending-opens-nothing ()
  "A poll that fires while a transport awaits acceptance must not race it."
  (agent-repl-test-link--with-harness
    ;; Arrange
    (setq agent-repl-test-link--address "127.0.0.1:9002")
    (agent-repl-link--reconnect-tick)
    ;; Act
    (agent-repl-link--reconnect-tick)
    ;; Assert
    (should (= (length agent-repl-test-link--streams) 1))))

;;;; ---- Acceptance: the successor gate ----

(ert-deftest agent-repl-test-link-unaccepted-successor-is-not-adoptable ()
  "host.el must never adopt onto a daemon that has not answered."
  (agent-repl-test-link--with-harness
    ;; Arrange
    (let ((conn (agent-repl-test-link--connect "127.0.0.1:9001")))
      ;; Act
      (agent-repl-test-link--push
       conn (list :arm :shutdown-announced
                  :value (agent-repl-test-link--announcement :address "127.0.0.1:9100")))
      ;; Assert
      (should (null (agent-repl-link-successor))))))

(ert-deftest agent-repl-test-link-unaccepted-successor-runs-no-handover-hook ()
  "The handover hook promises an adoptable NEW connection."
  (agent-repl-test-link--with-harness
    ;; Arrange
    (let ((conn (agent-repl-test-link--connect "127.0.0.1:9001")))
      (add-hook 'agent-repl-link-handover-functions
                (agent-repl-test-link--record-hook :handover))
      ;; Act
      (agent-repl-test-link--push
       conn (list :arm :shutdown-announced
                  :value (agent-repl-test-link--announcement :address "127.0.0.1:9100")))
      ;; Assert
      (should (null (assq :handover agent-repl-test-link--hooks))))))

(ert-deftest agent-repl-test-link-successor-acceptance-is-logged ()
  "The successor becoming adoptable is a fact worth naming."
  (agent-repl-test-link--with-harness
    ;; Arrange
    (let ((conn (agent-repl-test-link--connect "127.0.0.1:9001")))
      ;; Act
      (agent-repl-test-link--announce-successor conn "127.0.0.1:9100")
      ;; Assert
      (should (agent-repl-test-link--logged-p :info "elisp.link.successor-accepted")))))

(ert-deftest agent-repl-test-link-successor-death-before-acceptance-keeps-the-primary ()
  "The old daemon still owns everything it has not released."
  (agent-repl-test-link--with-harness
    ;; Arrange
    (let ((conn (agent-repl-test-link--connect "127.0.0.1:9001")))
      (agent-repl-test-link--push
       conn (list :arm :shutdown-announced
                  :value (agent-repl-test-link--announcement :address "127.0.0.1:9100")))
      ;; Act
      (agent-repl-test-link--close agent-repl-link--pending-successor
                                   '(:error (:kind :transport)))
      ;; Assert
      (should (eq (agent-repl-link-primary) conn)))))

(ert-deftest agent-repl-test-link-successor-death-before-acceptance-is-an-error ()
  "A handover that cannot complete is loud, never silent."
  (agent-repl-test-link--with-harness
    ;; Arrange
    (let ((conn (agent-repl-test-link--connect "127.0.0.1:9001")))
      (agent-repl-test-link--push
       conn (list :arm :shutdown-announced
                  :value (agent-repl-test-link--announcement :address "127.0.0.1:9100")))
      ;; Act
      (agent-repl-test-link--close agent-repl-link--pending-successor
                                   '(:error (:kind :transport)))
      ;; Assert
      (should (agent-repl-test-link--logged-p :error "elisp.link.successor-open-refused")))))

(ert-deftest agent-repl-test-link-teardown-closes-a-pending-connection ()
  "Teardown owes the same closure to a transport still awaiting acceptance."
  (agent-repl-test-link--with-harness
    ;; Arrange
    (agent-repl-test-link--stand "127.0.0.1:9001")
    ;; Act
    (agent-repl-link-teardown)
    ;; Assert
    (should (null agent-repl-link--pending))))

(provide 'test-daemon-link)

;;; test-daemon-link.el ends here
