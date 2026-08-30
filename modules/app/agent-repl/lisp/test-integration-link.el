;;; test-integration-link.el --- Integration: daemon-link.el against the fake daemon -*- lexical-binding: t; -*-

;;; Commentary:

;; Scenarios 2, 5 (link half), 6 and 8 of elisp-fanout.md §14.
;;
;; 2. Reconnect after a daemon restart: a second instance rewrites
;;    daemon.addr, Emacs re-registers (idempotent by dir) and re-subscribes.
;;    That is the NORMAL path, not an error.
;; 5. `shutdown_announced' CARRYING an address → DUAL ATTACH: the successor
;;    connection opens while the old one is KEPT, because each workspace's
;;    updates keep flowing from whichever daemon currently owns it.  When the
;;    old daemon stream later closes, the successor is promoted silently.
;; 6. A plain bounce (`shutdown_announced' with NO address) → no dual attach,
;;    the quiet window honored, reconnect once daemon.addr reappears.
;; 8. `drain_scheduled' / `drain_cancelled' → the standing indicator, and a
;;    LATE subscriber receiving the standing schedule.

;;; Code:

(require 'ert)
(require 'cl-lib)

(load (expand-file-name "test-integration-helpers.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

;; Production names this suite depends on (daemon-link.el, §6; host.el, §7).
(declare-function agent-repl-link-connect "daemon-link")
(declare-function agent-repl-link-primary "daemon-link")
(declare-function agent-repl-link-successor "daemon-link")
(declare-function agent-repl-link-up-p "daemon-link")
(declare-function agent-repl-link-teardown "daemon-link")
(declare-function agent-repl-connect-close "connect")
(declare-function agent-repl-host-register "host")
(declare-function agent-repl--ws-put "workspace")
(defvar agent-repl-link-up-functions)
(defvar agent-repl-link-down-functions)
(defvar agent-repl-link-handover-functions)
(defvar agent-repl-link-drain-functions)
(defvar agent-repl-link-no-daemon-functions)
(defvar agent-repl-link-drain)
(defvar agent-repl-link-drain-segment)
(defvar agent-repl-link-reconnect-interval-seconds)

;;;; ---- Fixtures ----

(defmacro agent-repl-itest-link--with-link (daemon &rest body)
  "Connect the link to DAEMON, run BODY, then tear the link down.
Every link hook is scratch-bound so a test observes only its own
registrations, and the reconnect interval is shortened so the reconnect
loop's own timing is not what a test spends its deadline on."
  (declare (indent 1) (debug (form body)))
  `(let ((agent-repl-link-up-functions nil)
         (agent-repl-link-down-functions nil)
         (agent-repl-link-handover-functions nil)
         (agent-repl-link-drain-functions nil)
         (agent-repl-link-no-daemon-functions nil)
         (agent-repl-link-drain nil)
         (agent-repl-link-drain-segment nil)
         (agent-repl-link-reconnect-interval-seconds 0.05))
     (unwind-protect
         (progn
           (agent-repl-link-connect)
           (agent-repl-itest--await-subscriber ,daemon "daemon")
           ,@body)
       (ignore-errors (agent-repl-link-teardown)))))

(defun agent-repl-itest-link--announce (daemon &optional address cause)
  "Push `shutdown_announced' on DAEMON, optionally naming ADDRESS.
CAUSE defaults to the self-merge rollout arm.  Without an address this is
a plain bounce: the same daemon is coming back, so nothing dual-attaches."
  (agent-repl-itest--push
   daemon "daemon"
   `((shutdownAnnounced
      . ,(append (when address `((address . ,address)))
                 `((cause . ,(or cause '((selfMergeRollout . ()))))
                   (expectedOutageMs . "1500")
                   (mintedAtMs . ,(format "%d" (truncate (* 1000 (float-time)))))))))))

;;;; ---- Scenario 2: reconnect after a daemon restart ----

(ert-deftest agent-repl-itest-link-connects-to-the-published-address ()
  "The link opens against the address in daemon.addr and stands its stream.
WatchDaemon is the ONE daemon-level stream; the link holds exactly it."
  ;; Arrange / Act.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-link--with-link daemon
      ;; Assert.
      (should (agent-repl-link-up-p))
      (should (equal 1 (length (agent-repl-itest--subscribers daemon "daemon")))))))

(ert-deftest agent-repl-itest-link-comes-up-on-acceptance-with-no-pushes ()
  "A WatchDaemon that is never pushed anything still brings the link UP.
THE STANDING-STREAM ACCEPTANCE RULE: the daemon flushes the response
headers the moment it accepts the subscription, and a client treats their
ARRIVAL as acceptance — before any frame exists.  A quiet daemon is the
normal case (nothing is draining, nothing is rolling out), so a link that
waited for a first push would sit down indefinitely against a perfectly
healthy daemon.

Nothing here pushes, scripts a snapshot, or waits on the daemon's own
subscriber registry first: the assertion is the CLIENT's, and its only
input is the header block."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (let ((up nil)
          (agent-repl-link-up-functions nil)
          (agent-repl-link-down-functions nil)
          (agent-repl-link-handover-functions nil)
          (agent-repl-link-drain-functions nil)
          (agent-repl-link-no-daemon-functions nil)
          (agent-repl-link-drain nil)
          (agent-repl-link-drain-segment nil)
          (agent-repl-link-reconnect-interval-seconds 0.05))
      (add-hook 'agent-repl-link-up-functions (lambda (&rest _) (setq up t)))
      (unwind-protect
          (progn
            ;; Act.
            (agent-repl-link-connect)
            ;; Assert: acceptance alone is enough.
            (agent-repl-itest--wait-until (lambda () (agent-repl-link-up-p)) nil
                                          "the link to come up on acceptance alone")
            (should (agent-repl-link-up-p))
            (should up)
            ;; And the stream is STANDING, not concluded — a standing stream
            ;; never ends of its own accord.
            (should (equal 1 (length (agent-repl-itest--subscribers daemon "daemon")))))
        (ignore-errors (agent-repl-link-teardown))))))

(ert-deftest agent-repl-itest-link-absent-daemon-runs-the-no-daemon-hooks ()
  "No daemon.addr → the no-daemon hooks run and the link stays down.
That absence is the legal no-daemon state; cold start hooks onto exactly
this, and it is NOT an error."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (delete-file (agent-repl-itest--addr-file
                  (agent-repl-itest-daemon-state-dir daemon)))
    (let* ((called nil)
           (agent-repl-link-no-daemon-functions (list (lambda () (setq called t))))
           (agent-repl-link-up-functions nil))
      ;; Act.
      (let ((conn (agent-repl-link-connect)))
        ;; Assert.
        (should (null conn))
        (should called)))))

(ert-deftest agent-repl-itest-link-reconnects-after-a-restart-on-a-new-port ()
  "A restarted daemon on a NEW port is reconnected to, from daemon.addr.
A daemon restart drops the streams and Emacs re-registers and
re-subscribes; staleness machinery (fences, boot ids) is gone."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon primary
    (agent-repl-itest-link--with-link primary
      (let ((state-dir (agent-repl-itest-daemon-state-dir primary)))
        ;; Act: the first daemon dies, a successor publishes a fresh port.
        (agent-repl-itest--stop-daemon primary t)
        (let ((successor (agent-repl-itest--start-daemon state-dir)))
          (unwind-protect
              (progn
                ;; Assert: the link stands its stream on the NEW instance.
                (agent-repl-itest--await-subscriber successor "daemon")
                (should (equal 1 (length (agent-repl-itest--subscribers
                                          successor "daemon"))))
                (should-not (equal (agent-repl-itest-daemon-address successor)
                                   (agent-repl-itest-daemon-address primary))))
            (agent-repl-itest--stop-daemon successor t)))))))

(ert-deftest agent-repl-itest-link-restart-runs-the-down-then-up-hooks ()
  "A dropped daemon stream runs the down hooks, then the up hooks on return.
host.el re-registers and re-subscribes on link-up and roster.el
re-subscribes; the hooks are the seam that makes that reactive."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon primary
    (let ((events nil))
      (agent-repl-itest-link--with-link primary
        (add-hook 'agent-repl-link-down-functions
                  (lambda (&rest _) (push 'down events)))
        (add-hook 'agent-repl-link-up-functions
                  (lambda (&rest _) (push 'up events)))
        (let ((state-dir (agent-repl-itest-daemon-state-dir primary)))
          ;; Act.
          (agent-repl-itest--stop-daemon primary t)
          (let ((successor (agent-repl-itest--start-daemon state-dir)))
            (unwind-protect
                (progn
                  (agent-repl-itest--await-subscriber successor "daemon")
                  ;; Assert: down first, then up — the order the reactive
                  ;; consumers depend on.
                  (agent-repl-itest--wait-until
                   (lambda () (and (memq 'down events) (memq 'up events)))
                   nil "the down and up hooks")
                  (should (equal (reverse events) '(down up))))
              (agent-repl-itest--stop-daemon successor t))))))))

(ert-deftest agent-repl-itest-link-re-registration-is-idempotent-by-dir ()
  "Re-registering the same dir after a restart reconciles to one ref.
RegisterWorkspace is IDEMPOTENT BY DIR, which is why re-registering is
the normal reconnect path rather than a duplicate-creation hazard."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-link--with-link daemon
      (let ((conn (agent-repl-link-primary))
            (first nil)
            (second nil))
        (agent-repl--ws-put "itest-link-ws" :project-dir "/tmp/itest-link-ws")
        ;; Act.
        (agent-repl-host-register conn "/tmp/itest-link-ws"
                                  (lambda (ref) (setq first ref)))
        (agent-repl-itest--wait-until (lambda () first) nil "the first registration")
        (agent-repl-host-register conn "/tmp/itest-link-ws"
                                  (lambda (ref) (setq second ref)))
        (agent-repl-itest--wait-until (lambda () second) nil "the second registration")
        ;; Assert.
        (should (equal (plist-get first :id) (plist-get second :id)))))))

;;;; ---- Scenario 5, link half: dual attach and promotion ----

(ert-deftest agent-repl-itest-link-announced-address-dual-attaches ()
  "`shutdown_announced{address}' opens the successor and KEEPS the old link.
Each workspace's updates keep flowing from the daemon that currently owns
it, so both connections must stand during the window."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon primary
    (agent-repl-itest-link--with-link primary
      (agent-repl-itest--with-second-daemon primary successor
        ;; Act.
        (agent-repl-itest-link--announce
         primary (agent-repl-itest-daemon-address successor))
        ;; Assert: a WatchDaemon stream stands on BOTH instances.
        (agent-repl-itest--await-subscriber successor "daemon")
        (should (agent-repl-link-successor))
        (should (equal 1 (length (agent-repl-itest--subscribers successor "daemon"))))
        (should (equal 1 (length (agent-repl-itest--subscribers primary "daemon"))))))))

(ert-deftest agent-repl-itest-link-announced-address-runs-the-handover-hooks ()
  "The handover hooks receive the OLD and NEW connections.
host.el's adopt-then-resubscribe walk is driven from exactly this seam;
there is no daemon-to-daemon channel, so the CLIENT is the relay."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon primary
    (agent-repl-itest-link--with-link primary
      (agent-repl-itest--with-second-daemon primary successor
        (let ((handovers nil))
          (add-hook 'agent-repl-link-handover-functions
                    (lambda (old new) (push (cons old new) handovers)))
          ;; Act.
          (agent-repl-itest-link--announce
           primary (agent-repl-itest-daemon-address successor))
          ;; Assert.
          (agent-repl-itest--wait-until (lambda () handovers) nil "the handover hooks")
          (should (eq (car (car handovers)) (agent-repl-link-primary)))
          (should (eq (cdr (car handovers)) (agent-repl-link-successor))))))))

(ert-deftest agent-repl-itest-link-old-stream-close-promotes-the-successor ()
  "When the old daemon's stream closes after a handover, the successor is
promoted SILENTLY — no down/up hooks, because the workspaces were adopted."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon primary
    (agent-repl-itest-link--with-link primary
      (agent-repl-itest--with-second-daemon primary successor
        (let ((hooks nil)
              (successor-address (agent-repl-itest-daemon-address successor)))
          (agent-repl-itest-link--announce primary successor-address)
          (agent-repl-itest--await-subscriber successor "daemon")
          (add-hook 'agent-repl-link-down-functions (lambda (&rest _) (push 'down hooks)))
          (add-hook 'agent-repl-link-up-functions (lambda (&rest _) (push 'up hooks)))
          ;; Act: the old daemon lets its stream go.
          (agent-repl-itest--end primary "daemon" nil nil t)
          ;; Assert.
          (agent-repl-itest--wait-until
           (lambda () (null (agent-repl-link-successor)))
           nil "the successor to be promoted to primary")
          (should (null hooks)))))))

;;;; ---- Scenario 6: the plain bounce ----

(ert-deftest agent-repl-itest-link-bounce-without-an-address-does-not-dual-attach ()
  "`shutdown_announced' with NO address is a plain bounce: nothing attaches.
The address is `optional' precisely so its ABSENCE says \"the same daemon
is coming back\" — PRESENCE, NEVER SENTINELS."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon primary
    (agent-repl-itest-link--with-link primary
      ;; Act.
      (agent-repl-itest-link--announce primary nil)
      (agent-repl-itest--await-call primary "WatchDaemon")
      ;; Assert.
      (should (null (agent-repl-link-successor))))))

(ert-deftest agent-repl-itest-link-bounce-reconnects-once-the-addr-returns ()
  "After a bounce the link reconnects when daemon.addr reappears.
The quiet window is `minted_at_ms + expected_outage_ms', an INSTANT on
the wire that the client ticks against its own clock."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon primary
    (agent-repl-itest-link--with-link primary
      (let ((state-dir (agent-repl-itest-daemon-state-dir primary)))
        (agent-repl-itest-link--announce primary nil)
        ;; Act: the bounce actually happens.
        (agent-repl-itest--stop-daemon primary t)
        (let ((successor (agent-repl-itest--start-daemon state-dir)))
          (unwind-protect
              (progn
                ;; Assert.
                (agent-repl-itest--await-subscriber successor "daemon")
                (should (agent-repl-link-up-p)))
            (agent-repl-itest--stop-daemon successor t)))))))

;;;; ---- Scenario 8: the drain indicator ----

(ert-deftest agent-repl-itest-link-drain-scheduled-records-the-schedule ()
  "`drain_scheduled' records the deadline instant and the reason.
A countdown ships its DEADLINE INSTANT; the client ticks locally, so the
wire carries no remaining-time value to go stale."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-link--with-link daemon
      ;; Act.
      (agent-repl-itest--push
       daemon "daemon"
       '((drainScheduled . ((atMs . "1735689600000") (reason . ((deploy . ())))))))
      ;; Assert.
      (agent-repl-itest--wait-until (lambda () agent-repl-link-drain) nil
                                    "the recorded drain schedule")
      (should (equal (plist-get agent-repl-link-drain :at-ms) 1735689600000)))))

(ert-deftest agent-repl-itest-link-drain-reason-arms-each-draw-a-segment ()
  "Each DrainReason arm draws its own indicator text.
Fixed treatment per ARM: deploy, maintenance and operator{note} are three
different facts, and the operator's note is dynamic detail."
  ;; Arrange.
  (let ((cases '((((deploy . ())) . "deploy")
                 (((maintenance . ())) . "maintenance")
                 (((operator . ((note . "cable work")))) . "cable work"))))
    (agent-repl-itest--with-fake-daemon daemon
      (agent-repl-itest-link--with-link daemon
        (dolist (case cases)
          (let ((reason (car case))
                (expected (cdr case)))
            ;; Act.
            (agent-repl-itest--push
             daemon "daemon"
             `((drainScheduled . ((atMs . "1735689600000") (reason . ,reason)))))
            ;; Assert.
            (agent-repl-itest--wait-until
             (lambda () (and agent-repl-link-drain-segment
                             (string-match-p (regexp-quote expected)
                                             agent-repl-link-drain-segment)))
             nil (format "the drain segment to name %S" expected))
            (should (string-match-p (regexp-quote expected)
                                    agent-repl-link-drain-segment))))))))

(ert-deftest agent-repl-itest-link-drain-cancelled-clears-the-indicator ()
  "`drain_cancelled' retracts the schedule and the indicator with it.
Whole-view pushes mean a retraction is a push too: the client never
expires a schedule on its own."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-link--with-link daemon
      (agent-repl-itest--push
       daemon "daemon"
       '((drainScheduled . ((atMs . "1735689600000") (reason . ((deploy . ())))))))
      (agent-repl-itest--wait-until (lambda () agent-repl-link-drain) nil
                                    "the recorded drain schedule")
      ;; Act.
      (agent-repl-itest--push daemon "daemon" '((drainCancelled . ())))
      ;; Assert.
      (agent-repl-itest--wait-until (lambda () (null agent-repl-link-drain)) nil
                                    "the drain schedule to be retracted")
      (should (null agent-repl-link-drain)))))

(ert-deftest agent-repl-itest-link-late-subscriber-gets-the-standing-schedule ()
  "A link connecting AFTER the schedule was set still learns it.
Streams are \"now\": the snapshot on subscribe is the only way a late
client can know a standing drain, and every client must draw the banner."
  ;; Arrange: the schedule is standing before the link exists.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest--push
     daemon "daemon"
     '((drainScheduled . ((atMs . "1735689600000") (reason . ((maintenance . ()))))))
     nil t)
    ;; Act.
    (agent-repl-itest-link--with-link daemon
      ;; Assert.
      (agent-repl-itest--wait-until (lambda () agent-repl-link-drain) nil
                                    "the standing schedule to be replayed")
      (should (equal (plist-get agent-repl-link-drain :at-ms) 1735689600000)))))

(ert-deftest agent-repl-itest-link-drain-runs-the-drain-hooks ()
  "A drain push runs the drain hooks, which is how surfaces react.
Other consequences ride existing surfaces (tray holds, footer status);
the link's own job is the standing indicator."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-link--with-link daemon
      (let ((ran nil))
        (add-hook 'agent-repl-link-drain-functions (lambda (&rest _) (push t ran)))
        ;; Act.
        (agent-repl-itest--push
         daemon "daemon"
         '((drainScheduled . ((atMs . "1735689600000") (reason . ((deploy . ())))))))
        ;; Assert.
        (agent-repl-itest--wait-until (lambda () ran) nil "the drain hooks")
        (should ran)))))

(ert-deftest agent-repl-itest-link-logs-the-stream-open ()
  "The link's WatchDaemon open is logged through the canonical ladder.
Every logical branch of production code logs; the remediation loop reads
these runs through exactly these lines."
  ;; Arrange / Act.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-link--with-link daemon
      ;; Assert.
      (agent-repl-itest--await-log daemon "elisp.rpc.stream-open" "info")
      (should (agent-repl-itest--logged-p daemon "elisp.rpc.stream-open" "info")))))

(provide 'test-integration-link)

;;; test-integration-link.el ends here
