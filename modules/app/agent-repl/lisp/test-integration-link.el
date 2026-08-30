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
(declare-function agent-repl-host-ref "host")
(declare-function agent-repl-host-conn "host")
(declare-function agent-repl-host-state "host")
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

(defmacro agent-repl-itest-link--with-real-hooks (daemon &rest body)
  "Connect the link to DAEMON with the PRODUCTION consumer hooks live.
Unlike `agent-repl-itest-link--with-link', the `agent-repl-link-*-functions'
variables are NOT scratch-bound here: host.el's and roster.el's own
`add-hook' registrations (installed once at load time, on
`agent-repl-link-up-functions' / `-down-functions') react for real.  This
is what findings 16-18 need — \"host.el's link-up hook doing the work, not
the test\" and \"roster.el re-subscribes\" mean production code reacting to
the seam, never the test driving it by hand."
  (declare (indent 1) (debug (form body)))
  `(let ((agent-repl-link-reconnect-interval-seconds 0.05))
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

(ert-deftest agent-repl-itest-link-restart-re-registers-and-resubscribes-via-host-hook ()
  "Re-registering the same dir across an ACTUAL daemon restart reconciles
to one ref — driven by host.el's OWN `agent-repl-link-up-functions' hook,
not by the test calling `agent-repl-host-register' by hand.
fanout §14 scenario 2: \"Re-register + re-subscribe after a daemon
restart ... Emacs re-registers (idempotent by dir) and re-subscribes.\"
RegisterWorkspace is IDEMPOTENT BY DIR, which is why re-registering is
the normal reconnect path rather than a duplicate-creation hazard."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon primary
    (let ((ws "itest-link-restart-ws")
          (dir "/tmp/itest-link-restart-ws"))
      (agent-repl--ws-put ws :project-dir dir)
      (agent-repl-itest-link--with-real-hooks primary
        ;; host.el's link-up hook registers and subscribes on the initial
        ;; connect, with no test-driven call at all.
        (agent-repl-itest--wait-until
         (lambda () (agent-repl-host-ref ws))
         nil "host.el's own initial register+subscribe")
        (let ((first-id (plist-get (agent-repl-host-ref ws) :id))
              (state-dir (agent-repl-itest-daemon-state-dir primary)))
          ;; Act: the primary dies, a successor takes its place.
          (agent-repl-itest--stop-daemon primary t)
          (let ((successor (agent-repl-itest--start-daemon state-dir)))
            (unwind-protect
                (progn
                  (agent-repl-itest--await-subscriber successor "daemon")
                  (agent-repl-itest--await-call successor "RegisterWorkspace")
                  (agent-repl-itest--await-call successor "WatchHostWorkspace")
                  ;; Assert: the SUCCESSOR itself recorded both calls, for
                  ;; the same dir — host.el's hook did the work.
                  (should (equal dir (agent-repl-itest--body-field
                                      (car (agent-repl-itest--call-bodies
                                            successor "RegisterWorkspace"))
                                      'dir)))
                  (should (equal dir (agent-repl-itest--body-field
                                      (car (agent-repl-itest--call-bodies
                                            successor "WatchHostWorkspace"))
                                      'workspace 'dir)))
                  ;; Re-registration is IDEMPOTENT BY DIR: the re-minted
                  ;; ref names the same workspace id as the first one.
                  (should (equal first-id (plist-get (agent-repl-host-ref ws) :id))))
              (agent-repl-itest--stop-daemon successor t))))))))

(ert-deftest agent-repl-itest-link-down-marks-streams-gone-but-keeps-host-state ()
  "On link down: mark streams gone; keep the last host state.
fanout §7: \"On link down: mark streams gone; keep the last host
state.\"  An outage the reconnect will cover must not blank the editor in
the meantime — `agent-repl-host-state' answers with the newest fact Emacs
has even once `agent-repl-host-conn' no longer names a live daemon.  This
is host.el's OWN `agent-repl-link-down-functions' hook, live via
`agent-repl-itest-link--with-real-hooks'."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (let ((ws "itest-link-down-ws")
          (dir "/tmp/itest-link-down-ws"))
      (agent-repl--ws-put ws :project-dir dir)
      (agent-repl-itest-link--with-real-hooks daemon
        (agent-repl-itest--wait-until
         (lambda () (agent-repl-host-ref ws))
         nil "host.el's own initial register+subscribe")
        (let ((id (plist-get (agent-repl-host-ref ws) :id)))
          (agent-repl-itest--push
           daemon "host" '((host . ((none . ()) (naming . ())))) id)
          (agent-repl-itest--wait-until
           (lambda () (agent-repl-host-state ws)) nil "the host push to apply")
          ;; Act: the daemon drops the link with no end frame at all.
          (agent-repl-itest--end daemon "daemon" nil nil t)
          (agent-repl-itest--wait-until
           (lambda () (null (agent-repl-host-conn ws)))
           nil "the link-down hook to mark the stream gone")
          ;; Assert.
          (should (null (agent-repl-host-conn ws)))
          (should (agent-repl-host-state ws)))))))

(ert-deftest agent-repl-itest-link-restart-triggers-rosters-own-resubscribe ()
  "roster.el re-subscribes on link-up BY ITSELF: after a restart the
successor lists one roster subscriber though nothing here ever calls
`agent-repl-roster-subscribe'.
fanout §8/§6: \"roster.el re-subscribes\" — reactively, off
`agent-repl-link-up-functions', the same seam host.el hangs off."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon primary
    (agent-repl-itest-link--with-real-hooks primary
      (agent-repl-itest--await-subscriber primary "roster")
      (let ((state-dir (agent-repl-itest-daemon-state-dir primary)))
        ;; Act: the primary dies, a successor takes its place.
        (agent-repl-itest--stop-daemon primary t)
        (let ((successor (agent-repl-itest--start-daemon state-dir)))
          (unwind-protect
              (progn
                (agent-repl-itest--await-subscriber successor "daemon")
                ;; Assert: nothing here called `agent-repl-roster-subscribe'.
                (agent-repl-itest--await-subscriber successor "roster")
                (should (equal 1 (length (agent-repl-itest--subscribers
                                          successor "roster")))))
            (agent-repl-itest--stop-daemon successor t)))))))

(ert-deftest agent-repl-itest-link-logs-elisp-link-down-up-and-handover ()
  "Each of the link's down/up/handover branches logs its own operation
name.
fanout §0: \"Operation names: `elisp.<module>.<operation>'.\"  The
remediation loop reads exactly these lines to tell one branch from
another: `elisp.link.up' (the initial connect), `elisp.link.handover-
announced' (a successor is announced), and `elisp.link.down' (an
unexpected, non-handover death)."
  ;; Arrange: connect the link — the `up' branch.
  (agent-repl-itest--with-fake-daemon primary
    (agent-repl-itest-link--with-link primary
      ;; Assert (up).
      (should (agent-repl-itest--logged-p primary "elisp.link.up" "info"))
      (agent-repl-itest--with-second-daemon primary successor
        ;; Act: announce the successor — the `handover' branch.
        (agent-repl-itest-link--announce
         primary (agent-repl-itest-daemon-address successor))
        (agent-repl-itest--await-subscriber successor "daemon")
        ;; Assert (handover).
        (agent-repl-itest--await-log primary "elisp.link.handover-announced" "info")
        (should (agent-repl-itest--logged-p primary "elisp.link.handover-announced" "info"))
        ;; Act: the successor dies before taking over, so the OLD daemon
        ;; dropping its own stream next is a genuine unexpected death, not
        ;; a promotion — the `down' branch.
        (agent-repl-itest--end successor "daemon" nil nil t)
        (agent-repl-itest--wait-until
         (lambda () (null (agent-repl-link-successor)))
         nil "the dead successor to be forgotten")
        (agent-repl-itest--end primary "daemon" nil nil t)
        ;; Assert (down).
        (agent-repl-itest--await-log primary "elisp.link.down" "warn")
        (should (agent-repl-itest--logged-p primary "elisp.link.down" "warn"))))))

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
promoted to PRIMARY silently — no down/up hooks, because the workspaces
were adopted.
fanout §6: \"PROMOTE the successor to primary silently.\"  Asserting only
that `agent-repl-link-successor' becomes nil is not enough — that is also
true if BOTH connections died; the successor becoming the live primary is
the actual claim."
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
          (let ((successor-conn (agent-repl-link-successor)))
            ;; Act: the old daemon lets its stream go.
            (agent-repl-itest--end primary "daemon" nil nil t)
            ;; Assert.
            (agent-repl-itest--wait-until
             (lambda () (null (agent-repl-link-successor)))
             nil "the successor to be promoted to primary")
            (should (null hooks))
            ;; The former successor is now the live PRIMARY.
            (should (eq (agent-repl-link-primary) successor-conn))
            (should (agent-repl-link-up-p))))))))

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

(ert-deftest agent-repl-itest-link-late-announcement-shortens-the-quiet-window ()
  "A LATE `shutdown_announced' shortens the quiet window by the elapsed
time instead of restarting a fresh one.
`endpoint_watch_daemon.proto' `DaemonShutdownAnnounced.minted_at_ms': \"A
late receiver SHORTENS its quiet window by the time already elapsed
rather than restarting it.\"  With 1400ms already elapsed out of a 1500ms
outage, the reconnect must reach the successor within roughly 100ms — a
client that restarted the whole 1500ms window from receipt would still be
waiting when this test's deadline is checked."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon primary
    (agent-repl-itest-link--with-link primary
      (let* ((state-dir (agent-repl-itest-daemon-state-dir primary))
             (minted-at-ms (- (truncate (* 1000 (float-time))) 1400)))
        ;; Act: minted 1400ms in the past, a 1500ms outage — ~100ms of
        ;; quiet remains.
        (agent-repl-itest--push
         primary "daemon"
         `((shutdownAnnounced
            . ((cause . ((selfMergeRollout . ())))
               (expectedOutageMs . "1500")
               (mintedAtMs . ,(format "%d" minted-at-ms))))))
        (agent-repl-itest--stop-daemon primary t)
        (let* ((start (float-time))
               (successor (agent-repl-itest--start-daemon state-dir)))
          (unwind-protect
              (progn
                (agent-repl-itest--await-call successor "WatchDaemon")
                ;; Assert: nowhere near the full 1500ms outage.
                (should (< (- (float-time) start) 1.0)))
            (agent-repl-itest--stop-daemon successor t)))))))

(ert-deftest agent-repl-itest-link-bounce-reconnect-does-not-poll-before-the-deadline ()
  "The reconnect loop waits UNTIL `minted_at_ms + expected_outage_ms'
before polling at all.
fanout §6: \"the reconnect loop waits until then before polling.\"  A
1.5s outage announced NOW, with the successor already up, must not see a
WatchDaemon call land before that instant — the timestamp at first
acceptance is compared against the announced deadline, not sampled with a
sleep."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon primary
    (agent-repl-itest-link--with-link primary
      (let* ((state-dir (agent-repl-itest-daemon-state-dir primary))
             (minted-at-ms (truncate (* 1000 (float-time))))
             (deadline-ms (+ minted-at-ms 1500)))
        ;; Act: an ordinary bounce; the successor is started immediately,
        ;; racing the quiet window rather than waiting it out first.
        (agent-repl-itest--push
         primary "daemon"
         `((shutdownAnnounced
            . ((cause . ((selfMergeRollout . ())))
               (expectedOutageMs . "1500")
               (mintedAtMs . ,(format "%d" minted-at-ms))))))
        (agent-repl-itest--stop-daemon primary t)
        (let ((successor (agent-repl-itest--start-daemon state-dir)))
          (unwind-protect
              (progn
                (agent-repl-itest--await-subscriber successor "daemon")
                ;; Assert: first acceptance is AT OR AFTER the announced
                ;; deadline (a small tolerance absorbs poll granularity),
                ;; never meaningfully before it.
                (should (>= (+ (truncate (* 1000 (float-time))) 50) deadline-ms)))
            (agent-repl-itest--stop-daemon successor t)))))))

(ert-deftest agent-repl-itest-link-bounce-indicator-names-the-cause ()
  "The bounce indicator reads \"daemon restarting (<cause>)\", naming the
cause arm.
fanout §6: \"the indicator reads 'daemon restarting (<cause>)'.\""
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon primary
    (agent-repl-itest-link--with-link primary
      ;; Act.
      (agent-repl-itest-link--announce primary nil)
      ;; Assert.
      (agent-repl-itest--wait-until
       (lambda () (equal agent-repl-link-drain-segment
                         "daemon restarting (self-merge rollout)"))
       nil "the bounce indicator to name the cause")
      (should (equal agent-repl-link-drain-segment
                     "daemon restarting (self-merge rollout)")))))

(ert-deftest agent-repl-itest-link-shutdown-cause-arms-each-decode-and-name-the-indicator ()
  "Each `DaemonShutdownCause' arm decodes, and for the REASON-BEARING arms
the bounce indicator names the carried `DrainReason' rather than a fixed
phrase for the cause alone.
`endpoint_watch_daemon.proto': `scheduled_drain{reason}' and
`immediate{reason}' both carry a `DrainReason' — THE ARM IS THE CAUSE, and
here the cause's own arm carries a further arm the indicator must
surface.  Every other test in this suite announces only
`selfMergeRollout'; an unknown or unset arm would never update the
indicator at all, which is how a decode failure would show up here."
  ;; Arrange.
  (let* ((self-merge '((selfMergeRollout . ())))
         (scheduled-drain-deploy
          `((scheduledDrain . ((reason . ((deploy . ())))))))
         (immediate-operator-note
          `((immediate . ((reason . ((operator . ((note . "cable work")))))))))
         (cases (list (cons self-merge "self-merge rollout")
                      (cons scheduled-drain-deploy "deploy")
                      (cons immediate-operator-note "cable work"))))
    (agent-repl-itest--with-fake-daemon daemon
      (agent-repl-itest-link--with-link daemon
        (dolist (case cases)
          (let ((cause (car case))
                (expected (cdr case)))
            ;; Act.
            (agent-repl-itest-link--announce daemon nil cause)
            ;; Assert.
            (agent-repl-itest--wait-until
             (lambda () (and agent-repl-link-drain-segment
                             (string-match-p (regexp-quote expected)
                                             agent-repl-link-drain-segment)))
             2 (format "the indicator to name %S" expected))
            (should (string-match-p (regexp-quote expected)
                                    agent-repl-link-drain-segment))))))))

(ert-deftest agent-repl-itest-link-invalid-shutdown-announced-is-dropped-not-fatal ()
  "A `shutdown_announced' push missing `cause' (a non-optional message) is
logged ERROR and dropped; the link stays up and a following legal push
still arrives.
fanout §0: \"a push ... missing a non-optional field ... is a contract
breach: signal `agent-repl-wire-error' and log ERROR\" — but for a PUSH,
unlike a request, the contract is that the stream survives the breach."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-link--with-link daemon
      ;; Act: `cause' is entirely absent.
      (agent-repl-itest--push
       daemon "daemon"
       '((shutdownAnnounced . ((expectedOutageMs . "1500") (mintedAtMs . "1000")))))
      ;; Assert: logged ERROR, and the link is untouched by the breach.
      (agent-repl-itest--await-log daemon "elisp.rpc.push-invalid" "error")
      (should (agent-repl-itest--logged-p daemon "elisp.rpc.push-invalid" "error"))
      (should (agent-repl-link-up-p))
      ;; Act: a following, legal push.
      (agent-repl-itest--push
       daemon "daemon"
       '((drainScheduled . ((atMs . "1735689600000") (reason . ((deploy . ())))))))
      ;; Assert: it still arrives — the stream was never taken down.
      (agent-repl-itest--wait-until (lambda () agent-repl-link-drain) nil
                                    "the following drainScheduled to still arrive")
      (should (equal (plist-get agent-repl-link-drain :at-ms) 1735689600000)))))

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
  "Each DrainReason arm draws its own indicator text, in the FULL \"drain
HH:MM · <reason>\" format — not merely a reason substring somewhere in it.
Fixed treatment per ARM: deploy, maintenance and operator{note} are three
different facts, and the operator's note is dynamic detail; the `HH:MM'
prefix is derived from `at_ms', independently computed here the same way
production does."
  ;; Arrange.
  (let* ((at-ms 1735689600000)
         (prefix (format "drain %s · " (format-time-string "%H:%M" (/ at-ms 1000))))
         (cases (list (cons '((deploy . ())) "deploy")
                      (cons '((maintenance . ())) "maintenance")
                      (cons '((operator . ((note . "cable work")))) "cable work"))))
    (agent-repl-itest--with-fake-daemon daemon
      (agent-repl-itest-link--with-link daemon
        (dolist (case cases)
          (let* ((reason (car case))
                 (expected (concat prefix (cdr case))))
            ;; Act.
            (agent-repl-itest--push
             daemon "daemon"
             `((drainScheduled . ((atMs . ,(format "%d" at-ms)) (reason . ,reason)))))
            ;; Assert: the FULL format, including the `drain HH:MM' prefix.
            (agent-repl-itest--wait-until
             (lambda () (equal agent-repl-link-drain-segment expected))
             nil (format "the drain segment to read %S" expected))
            (should (equal agent-repl-link-drain-segment expected))))))))

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
  "A link connecting AFTER the schedule was set still learns it — the
FULL schedule (deadline, reason arm) and the drawn indicator, not merely
the deadline instant.
Streams are \"now\": the snapshot on subscribe is the only way a late
client can know a standing drain, and every client must draw the banner.
`endpoint_watch_daemon.proto' `DaemonDrainScheduled': \"re-pushed to late
subscribers per the subscription invariant.\""
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
      (should (equal (plist-get agent-repl-link-drain :at-ms) 1735689600000))
      ;; The reason arm — a full re-push, not just the deadline instant.
      (should (eq (plist-get (plist-get agent-repl-link-drain :reason) :arm)
                  :maintenance))
      ;; The indicator is drawn from the replayed schedule too.
      (agent-repl-itest--wait-until (lambda () agent-repl-link-drain-segment) nil
                                    "the indicator to be drawn from the replay")
      (should agent-repl-link-drain-segment))))

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
