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
(declare-function agent-repl-connect-connection-alive-p "connect")
(declare-function agent-repl-host-register "host")
(declare-function agent-repl-host-ref "host")
(declare-function agent-repl-host-conn "host")
(declare-function agent-repl-host-state "host")
(declare-function agent-repl--ws-put "workspace")
(defvar agent-repl-link-up-functions)
(defvar agent-repl-link-down-functions)
(defvar agent-repl-link-handover-functions)
(defvar agent-repl-link-drain-functions)
(defvar agent-repl-link-promote-functions)
(defvar agent-repl-link-no-daemon-functions)
(defvar agent-repl-link-drain)
(defvar agent-repl-link-drain-segment)
(defvar agent-repl-link-reconnect-interval-seconds)
(defvar agent-repl-link-reconnect-max-interval-seconds)

;;;; ---- Fixtures ----

(defun agent-repl-itest-link--await-up (daemon)
  "Block until DAEMON has the watch AND the CLIENT has accepted it.
TWO WAITS, because they are two different facts and the second is the one
every assertion here rests on.  `agent-repl-itest--await-subscriber' reads
the DAEMON's registry: it goes true the instant the server accepts the
subscription.  `agent-repl-link-up-p' is the CLIENT's fact, and per the
standing-stream acceptance rule it flips only when connect.el sees the
HTTP 200 header block and runs ON-OPEN -- strictly after the server side,
by a whole network hop.  A fixture that returned on the server fact alone
handed BODY a link that was not yet up, which is exactly how
`agent-repl-itest-link-connects-to-the-published-address' failed once at
0.08s while passing everywhere else."
  (agent-repl-itest--await-subscriber daemon "daemon")
  (agent-repl-itest--wait-until
   (lambda () (agent-repl-link-up-p))
   nil "the client to accept the WatchDaemon stream (link-up)"))

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
         (agent-repl-link-reconnect-interval-seconds 0.05)
         (agent-repl-link-reconnect-max-interval-seconds 0.2))
     (unwind-protect
         (progn
           (agent-repl-link-connect)
           (agent-repl-itest-link--await-up ,daemon)
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
  `(let ((agent-repl-link-reconnect-interval-seconds 0.05)
         (agent-repl-link-reconnect-max-interval-seconds 0.2))
     (unwind-protect
         (progn
           (agent-repl-link-connect)
           (agent-repl-itest-link--await-up ,daemon)
           ,@body)
       (ignore-errors (agent-repl-link-teardown)))))

(defun agent-repl-itest-link--announce (daemon &optional address cause outage-ms)
  "Push `shutdown_announced' on DAEMON, optionally naming ADDRESS.
CAUSE defaults to the self-merge rollout arm.  Without an address this is
a plain bounce: the same daemon is coming back, so nothing dual-attaches.

THE OUTAGE IS THE SCENARIO'S OWN WALL-CLOCK COST: every bounce scenario
that reconnects has to let the announced window elapse first, and the
window's LENGTH is a parameter of the fixture, never of the contract.
500ms is an order of magnitude above the 50ms poll tolerance the one
scenario that asserts against the deadline allows itself, so a client
that ignored the window entirely still cannot pass -- and three
scenarios stop spending 1.5s each waiting to prove it.

OUTAGE-MS overrides that window for a scenario that asserts on what the
STANDING announcement draws: the indicator is computed only while the
window stands, so such a scenario passes a window longer than its own
wait bound, and the window cannot lapse before the assertion has had its
whole bound to hold (a 500ms window lapsed under a full-suite load before
the push was even read, and the indicator then drew nothing to assert on)."
  (agent-repl-itest--push
   daemon "daemon"
   `((shutdownAnnounced
      . ,(append (when address `((address . ,address)))
                 `((cause . ,(or cause '((selfMergeRollout . ()))))
                   (expectedOutageMs . ,(format "%d" (or outage-ms 500)))
                   (mintedAtMs . ,(format "%d" (truncate (* 1000 (float-time)))))))))))

(defun agent-repl-itest-link--boom-up-hook (&rest _)
  "A link-up consumer that always signals, for pinning hook containment.
Named (not a `lambda') so `agent-repl-link--run-hook''s ERROR record
names it by symbol, and so it can be `remove-hook'd by name."
  (error "agent-repl-itest: boom (up)"))

(defun agent-repl-itest-link--boom-down-hook (&rest _)
  "A link-down consumer that always signals, for pinning hook containment.
Named for the same reason as `agent-repl-itest-link--boom-up-hook'."
  (error "agent-repl-itest: boom (down)"))

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
          (agent-repl-link-reconnect-interval-seconds 0.05)
          (agent-repl-link-reconnect-max-interval-seconds 0.2))
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
          (dir (agent-repl-itest--fixture-dir "itest-link-restart-ws")))
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
          (dir (agent-repl-itest--fixture-dir "itest-link-down-ws")))
      (agent-repl--ws-put ws :project-dir dir)
      (agent-repl-itest-link--with-real-hooks daemon
        (agent-repl-itest--wait-until
         (lambda () (agent-repl-host-ref ws))
         nil "host.el's own initial register+subscribe")
        (let ((id (plist-get (agent-repl-host-ref ws) :id)))
          ;; THE REF IS THE CLIENT'S FACT, THE SUBSCRIBER IS THE DAEMON'S.
          ;; `agent-repl-host-ref' is minted when RegisterWorkspace answers,
          ;; strictly BEFORE the WatchHostWorkspace subscription it then
          ;; opens is registered daemon-side.  A push in that window reaches
          ;; nobody and this scenario waits out its whole deadline for a
          ;; state that was delivered to no one.
          (agent-repl-itest--await-subscriber daemon "host" id)
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
        ;; Assert: a WatchDaemon stream stands on BOTH instances.  The
        ;; successor is ADOPTABLE only from ACCEPTANCE, which reaches Emacs
        ;; strictly after the daemon registers the subscriber, so it is
        ;; waited for rather than sampled.
        (agent-repl-itest--await-subscriber successor "daemon")
        (agent-repl-itest--wait-until
         #'agent-repl-link-successor nil "the successor to be ACCEPTED")
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
          ;; ACCEPTANCE, NEVER A SPAWN: the connection only becomes the
          ;; successor once its own WatchDaemon is accepted, which lands
          ;; after the daemon-side subscriber appears.  Sampling here would
          ;; capture nil and assert nothing.
          (agent-repl-itest--wait-until
           #'agent-repl-link-successor nil "the successor to be ACCEPTED")
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
                ;; Assert.  The subscriber appearing daemon-side STRICTLY
                ;; PRECEDES acceptance reaching Emacs (the handler registers,
                ;; then flushes the 200 headers the link keys `up' on), so
                ;; `up-p' is waited for rather than sampled the instant the
                ;; daemon says it has a subscriber.
                (agent-repl-itest--await-subscriber successor "daemon")
                (agent-repl-itest--wait-until
                 #'agent-repl-link-up-p nil
                 "the reconnected link to be ACCEPTED on the successor")
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
             ;; 500ms, not 1500: the assertion's 50ms tolerance still has an
             ;; order of magnitude of room, and a client that did not wait at
             ;; all would accept ~450ms early -- nine times the tolerance.
             (deadline-ms (+ minted-at-ms 500)))
        ;; Act: an ordinary bounce; the successor is started immediately,
        ;; racing the quiet window rather than waiting it out first.
        (agent-repl-itest--push
         primary "daemon"
         `((shutdownAnnounced
            . ((cause . ((selfMergeRollout . ())))
               (expectedOutageMs . "500")
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
             ;; The whole multi-case loop this sits in runs in ~0.2s
             ;; end-to-end, so 1s is already >3x any one case's share.
             1 (format "the indicator to name %S" expected))
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

;;;; ---- Audit-2 additions (R-SUITE-2) ----
;;
;; Findings 1-5 of docs/overhaul/reports/elisp-suite-audit-2.md.  Kept in
;; their own section so they merge cleanly beside concurrent edits to the
;; sections above.

;; audit-2 #1
(ert-deftest agent-repl-itest-link-promotion-re-subscribes-the-roster ()
  "A PROMOTED successor carries the roster stream with it.
elisp.md FINAL HANDOVER SEQUENCE: \"Roster and daemon-link then follow the
successor's address as the current daemon (promotion)\"; fanout §6
\"PROMOTE the successor to primary silently (no down/up hooks)\".
Promotion closes the old connection, which cancels the roster stream
riding it, so unless the promotion itself re-subscribes, Emacs is left
with NO roster stream at all and tabs stop reconciling until the next
link bounce.  Nothing here calls `agent-repl-roster-subscribe'."
  ;; Arrange: the production hooks are live, so roster.el subscribes itself.
  (agent-repl-itest--with-fake-daemon primary
    (agent-repl-itest-link--with-real-hooks primary
      (agent-repl-itest--await-subscriber primary "roster")
      (agent-repl-itest--with-second-daemon primary successor
        (agent-repl-itest-link--announce
         primary (agent-repl-itest-daemon-address successor))
        (agent-repl-itest--await-subscriber successor "daemon")
        (agent-repl-itest--wait-until
         #'agent-repl-link-successor nil "the successor to be ACCEPTED")
        ;; Act: the old daemon finishes and drops its stream — promotion.
        (agent-repl-itest--end primary "daemon" nil nil t)
        (agent-repl-itest--wait-until
         (lambda () (null (agent-repl-link-successor)))
         nil "the successor to be promoted to primary")
        ;; Assert: the roster follows the promotion.
        (agent-repl-itest--await-subscriber successor "roster")
        (should (equal 1 (length (agent-repl-itest--subscribers
                                  successor "roster"))))))))

(ert-deftest agent-repl-itest-link-clean-end-after-the-planned-ending-is-info ()
  "A clean end after the daemon's planned ending takes the link down at INFO.
The daemon's last frame (`DaemonStreamEnding') says the end is PLANNED:
the same down walk as a loss, recorded at INFO instead of the WARN."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon primary
    (agent-repl-itest-link--with-link primary
      (let ((downs nil))
        (add-hook 'agent-repl-link-down-functions (lambda (&rest _) (push t downs)))
        ;; Act: the planned ending, then a clean end frame.
        (agent-repl-itest--push primary "daemon" '((ending . ())))
        (agent-repl-itest--await-log primary "elisp.link.stream-ending" "info")
        (agent-repl-itest--end primary "daemon")
        ;; Assert.
        (agent-repl-itest--await-log primary "elisp.link.down-planned" "info")
        (agent-repl-itest--wait-until (lambda () downs) nil "the down hooks")
        (should-not (agent-repl-itest--logged-p primary "elisp.link.down" "warn"))))))

;; audit-2 #2
(ert-deftest agent-repl-itest-link-clean-end-frame-is-link-down ()
  "A CLEAN end frame on the standing WatchDaemon is a failure, not a close.
fanout §3: `(:ended)' \"for a standing stream the caller treats it as a
failure\".  Only the abort path was pinned; an end frame carrying no
error at all must take the link down exactly the same way, run the down
hooks, and leave the reconnect loop able to stand a stream on the
restarted daemon."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon primary
    (agent-repl-itest-link--with-link primary
      (let ((downs nil)
            (state-dir (agent-repl-itest-daemon-state-dir primary)))
        (add-hook 'agent-repl-link-down-functions (lambda (&rest _) (push t downs)))
        ;; Act: a clean end frame, NO error.
        (agent-repl-itest--end primary "daemon")
        ;; Assert: WARN, down hooks, link down.
        (agent-repl-itest--await-log primary "elisp.link.down" "warn")
        (should (agent-repl-itest--logged-p primary "elisp.link.down" "warn"))
        (agent-repl-itest--wait-until (lambda () downs) nil "the down hooks")
        (should downs)
        (should-not (agent-repl-link-up-p))
        ;; Assert: and the reconnect stands a stream on a restarted daemon.
        (agent-repl-itest--stop-daemon primary t)
        (let ((successor (agent-repl-itest--start-daemon state-dir)))
          (unwind-protect
              (progn
                (agent-repl-itest--await-subscriber successor "daemon")
                (should (equal 1 (length (agent-repl-itest--subscribers
                                          successor "daemon")))))
            (agent-repl-itest--stop-daemon successor t)))))))

;; audit-2 #3
(ert-deftest agent-repl-itest-link-drain-cancelled-clears-the-segment ()
  "`drain_cancelled' takes the INDICATOR down, not merely the schedule.
fanout §14 scenario 8: \"drain_cancelled → removed\" names the indicator
as the removed thing, so the segment is asserted in its own right —
clearing `agent-repl-link-drain' while leaving a stale banner drawn would
otherwise pass."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-link--with-link daemon
      (agent-repl-itest--push
       daemon "daemon"
       '((drainScheduled . ((atMs . "1735689600000") (reason . ((deploy . ())))))))
      (agent-repl-itest--wait-until (lambda () agent-repl-link-drain-segment) nil
                                    "the drain indicator to be drawn")
      ;; Act.
      (agent-repl-itest--push daemon "daemon" '((drainCancelled . ())))
      ;; Assert.
      (agent-repl-itest--wait-until (lambda () (null agent-repl-link-drain-segment))
                                    nil "the drain indicator to be removed")
      (should (null agent-repl-link-drain-segment)))))

;; audit-2 #4
(ert-deftest agent-repl-itest-link-successor-death-keeps-the-old-link-up ()
  "A successor that dies BEFORE promotion is forgotten; the old link stands.
fanout §6 dual attach; daemon-link.el: \"the old daemon still owns
whatever it has not transferred, so the link is not down\".  Nothing was
adopted onto the successor by construction, so the only loss is the
handover: `elisp.link.successor-stream-lost' at ERROR, the successor
forgotten, and NO down hook — a link that went down here would blank the
editor over a daemon that is still perfectly alive."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon primary
    (agent-repl-itest-link--with-link primary
      (agent-repl-itest--with-second-daemon primary successor
        (let ((downs nil)
              (primary-conn nil))
          (agent-repl-itest-link--announce
           primary (agent-repl-itest-daemon-address successor))
          (agent-repl-itest--await-subscriber successor "daemon")
          (agent-repl-itest--wait-until
           #'agent-repl-link-successor nil "the successor to be ACCEPTED")
          (setq primary-conn (agent-repl-link-primary))
          (add-hook 'agent-repl-link-down-functions (lambda (&rest _) (push t downs)))
          ;; Act: the successor dies before taking anything over.
          (agent-repl-itest--end successor "daemon" nil nil t)
          ;; Assert.
          (agent-repl-itest--await-log primary "elisp.link.successor-stream-lost" "error")
          (agent-repl-itest--wait-until
           (lambda () (null (agent-repl-link-successor)))
           nil "the dead successor to be forgotten")
          (should (null (agent-repl-link-successor)))
          (should (agent-repl-link-up-p))
          (should (eq (agent-repl-link-primary) primary-conn))
          (should (null downs)))))))

;; audit-2 #5
(ert-deftest agent-repl-itest-link-scheduled-drain-maintenance-cause-is-announced ()
  "`shutdown_announced{scheduled_drain{maintenance}}' names maintenance.
`endpoint_watch_daemon.proto' `DaemonShutdownScheduledDrain' carries a
`DrainReason' with three arms; the cause table drives `deploy' (scheduled)
and `operator' (immediate) only, so the third arm on the scheduled path
has never decoded here."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-link--with-link daemon
      ;; Act.
      (agent-repl-itest-link--announce
       daemon nil '((scheduledDrain . ((reason . ((maintenance . ())))))))
      ;; Assert.
      (agent-repl-itest--wait-until
       (lambda () (equal agent-repl-link-drain-segment
                         "daemon restarting (scheduled drain: maintenance)"))
       nil "the bounce indicator to name the maintenance drain")
      (should (equal agent-repl-link-drain-segment
                     "daemon restarting (scheduled drain: maintenance)")))))

;;;; ---- Audit-3 additions (R-SUITE-3) ----
;;
;; Findings 4-11 of docs/overhaul/reports/elisp-suite-audit-3.md.  Kept in
;; their own section so they merge cleanly beside concurrent edits to the
;; sections above.

;; audit-3 #4
(ert-deftest agent-repl-itest-link-contained-hook-failure-does-not-block-host-or-roster-up ()
  "A link-up consumer that signals is CONTAINED: the remaining consumers —
host.el's register+subscribe, roster.el's re-subscribe — still run, and
the failure is recorded naming the hook and the consumer.
R-STABILITY ruling / fanout §6 `elisp.link.hook-consumer-failed'
(`agent-repl-link--run-hook'): \"a consumer that signals must not silently
take the consumers behind it down with it\".  Registered at DEPTH -100 so
it runs ahead of host.el's and roster.el's own `add-hook' registrations
regardless of load order — a regression that let one bad consumer cancel
the rest would leave WS unregistered and the roster unsubscribed here."
  (agent-repl-itest--with-fake-daemon daemon
    (let ((ws "itest-link-hook-up-ws")
          (dir (agent-repl-itest--fixture-dir "itest-link-hook-up-ws")))
      (agent-repl--ws-put ws :project-dir dir)
      (add-hook 'agent-repl-link-up-functions
                #'agent-repl-itest-link--boom-up-hook -100)
      (unwind-protect
          (agent-repl-itest-link--with-real-hooks daemon
            ;; Assert: the failure is recorded, naming the hook and the
            ;; consumer.
            (agent-repl-itest--await-log
             daemon "elisp.link.hook-consumer-failed" "error")
            (let ((entry (car (agent-repl-itest--log-entries
                                daemon "elisp.link.hook-consumer-failed" "error"))))
              (should (string-match-p "hook=agent-repl-link-up-functions"
                                      (alist-get 'message entry)))
              (should (string-match-p "consumer=agent-repl-itest-link--boom-up-hook"
                                      (alist-get 'message entry))))
            ;; Assert: host.el still registered and subscribed WS, despite
            ;; running behind the failing consumer.
            (agent-repl-itest--wait-until
             (lambda () (agent-repl-host-ref ws)) nil
             "host.el's register+subscribe despite the failing up hook")
            (should (agent-repl-host-ref ws))
            ;; Assert: roster.el's subscriber stands too.
            (agent-repl-itest--await-subscriber daemon "roster"))
        (remove-hook 'agent-repl-link-up-functions
                     #'agent-repl-itest-link--boom-up-hook)))))

;; audit-3 #4
(ert-deftest agent-repl-itest-link-contained-hook-failure-does-not-block-reconnect ()
  "A link-DOWN consumer that signals is CONTAINED too: the schedule-
reconnect call downstream of the hook loop still runs, and the link
stands a fresh stream on the daemon that returns.
`agent-repl-link--handle-close' runs the down hooks BEFORE
`agent-repl-link--schedule-reconnect', so a signalling consumer must not
swallow the poll behind it — a regression that let the failure propagate
out of the dolist would leave the reconnect loop never armed."
  (agent-repl-itest--with-fake-daemon primary
    (add-hook 'agent-repl-link-down-functions
              #'agent-repl-itest-link--boom-down-hook -100)
    (unwind-protect
        (agent-repl-itest-link--with-real-hooks primary
          (let ((state-dir (agent-repl-itest-daemon-state-dir primary)))
            ;; Act: an abrupt, non-handover death.
            (agent-repl-itest--end primary "daemon" nil nil t)
            ;; Assert: the failure is recorded, naming the hook and the
            ;; consumer.
            (agent-repl-itest--await-log
             primary "elisp.link.hook-consumer-failed" "error")
            (let ((entry (car (agent-repl-itest--log-entries
                                primary "elisp.link.hook-consumer-failed" "error"))))
              (should (string-match-p "hook=agent-repl-link-down-functions"
                                      (alist-get 'message entry)))
              (should (string-match-p "consumer=agent-repl-itest-link--boom-down-hook"
                                      (alist-get 'message entry))))
            ;; Assert: the reconnect still stands on the daemon that returns.
            ;; The abort drops only the stream: PRIMARY is still alive and
            ;; still owns `daemon.addr', so a reconnect tick landing before
            ;; the successor publishes would re-attach to PRIMARY.  Stop it
            ;; first so the restart is the only daemon the loop can find.
            (agent-repl-itest--stop-daemon primary t)
            (let ((successor (agent-repl-itest--start-daemon state-dir)))
              (unwind-protect
                  (progn
                    (agent-repl-itest--await-subscriber successor "daemon")
                    (should (equal 1 (length (agent-repl-itest--subscribers
                                              successor "daemon")))))
                (agent-repl-itest--stop-daemon successor t)))))
      (remove-hook 'agent-repl-link-down-functions
                   #'agent-repl-itest-link--boom-down-hook))))

;; audit-3 #5
(ert-deftest agent-repl-itest-link-promote-hook-receives-old-new-and-old-still-alive ()
  "The promote hook runs with (OLD NEW) and BEFORE OLD is closed: OLD is
still ALIVE at call time, naming the former primary and NEW the former
successor.
fanout §6 \"the one hook that fires on promotion\"; daemon-link.el's
`agent-repl-link--promote-successor' runs `agent-repl-link-promote-
functions' immediately before `(agent-repl-connect-close old)'.  Every
prior promotion test in this suite asserts only the consequence (roster's
resubscribe); none captures the hook's own arguments or the close order."
  (agent-repl-itest--with-fake-daemon primary
    (let ((agent-repl-link-promote-functions agent-repl-link-promote-functions))
      (agent-repl-itest-link--with-link primary
        (agent-repl-itest--with-second-daemon primary successor
          (let ((primary-conn (agent-repl-link-primary))
                (calls nil))
            (agent-repl-itest-link--announce
             primary (agent-repl-itest-daemon-address successor))
            (agent-repl-itest--await-subscriber successor "daemon")
            (agent-repl-itest--wait-until
             #'agent-repl-link-successor nil "the successor to be ACCEPTED")
            (let ((successor-conn (agent-repl-link-successor)))
              (add-hook 'agent-repl-link-promote-functions
                        (lambda (old new)
                          (push (list old new (agent-repl-connect-connection-alive-p old))
                                calls)))
              ;; Act: the old daemon lets its stream go — promotion.
              (agent-repl-itest--end primary "daemon" nil nil t)
              (agent-repl-itest--wait-until
               (lambda () (null (agent-repl-link-successor)))
               nil "the successor to be promoted to primary")
              ;; Assert: exactly one call, OLD the former primary, NEW the
              ;; former successor, and OLD still ALIVE when the hook ran.
              (should (equal (length calls) 1))
              (should (eq (nth 0 (car calls)) primary-conn))
              (should (eq (nth 1 (car calls)) successor-conn))
              (should (nth 2 (car calls))))))))))

;; audit-3 #5
(ert-deftest agent-repl-itest-link-promote-hook-does-not-run-on-plain-link-down ()
  "The promote hook fires ONLY on a handover promotion — an ordinary link
death with no successor standing must never call it.
fanout §6: promotion and a plain down are different facts (`elisp.link.
down' vs `elisp.link.handover-complete'); a promote hook that also ran on
a bare down would double-drive roster.el's and prompt-queue.el's
promotion-only resubscribe."
  (agent-repl-itest--with-fake-daemon primary
    (let ((agent-repl-link-promote-functions agent-repl-link-promote-functions))
      (agent-repl-itest-link--with-link primary
        (let ((calls nil))
          (add-hook 'agent-repl-link-promote-functions
                    (lambda (&rest args) (push args calls)))
          ;; Act: an ordinary, non-handover death.
          (agent-repl-itest--end primary "daemon" nil nil t)
          (agent-repl-itest--await-log primary "elisp.link.down" "warn")
          ;; Assert.
          (should (null calls)))))))

;; audit-3 #6
(ert-deftest agent-repl-itest-link-up-and-down-hooks-receive-the-connection ()
  "The up hook's argument IS `(agent-repl-link-primary)' once it runs, and
the down hook's argument is the EXACT connection that died — not merely
called with SOME argument, which every `(lambda (&rest _) ...)' consumer
elsewhere in this suite would pass regardless of what daemon-link.el
actually handed it.
fanout §6: \"run `agent-repl-link-up-functions' with CONN\"."
  (agent-repl-itest--with-fake-daemon primary
    (let ((up-args nil)
          (down-args nil))
      (agent-repl-itest-link--with-link primary
        (add-hook 'agent-repl-link-up-functions
                  (lambda (conn) (push conn up-args)))
        (add-hook 'agent-repl-link-down-functions
                  (lambda (conn) (push conn down-args)))
        (let ((primary-conn (agent-repl-link-primary))
              (state-dir (agent-repl-itest-daemon-state-dir primary)))
          ;; Act: kill the link.
          (agent-repl-itest--end primary "daemon" nil nil t)
          (agent-repl-itest--wait-until (lambda () down-args) nil "the down hook")
          ;; Assert: the down arg is the connection that died.
          (should (eq (car down-args) primary-conn))
          ;; Act: reconnect.  PRIMARY survives the abort and still owns
          ;; `daemon.addr'; stop it so the restart is the only daemon the
          ;; reconnect loop can find.
          (agent-repl-itest--stop-daemon primary t)
          (let ((successor (agent-repl-itest--start-daemon state-dir)))
            (unwind-protect
                (progn
                  (agent-repl-itest--await-subscriber successor "daemon")
                  (agent-repl-itest--wait-until
                   #'agent-repl-link-up-p nil "the reconnected link")
                  (agent-repl-itest--wait-until (lambda () up-args) nil "the up hook")
                  ;; Assert: the up arg IS the current primary.
                  (should (eq (car up-args) (agent-repl-link-primary))))
              (agent-repl-itest--stop-daemon successor t))))))))

;; audit-3 #7
(ert-deftest agent-repl-itest-link-clean-end-from-old-daemon-promotes-the-successor ()
  "A CLEAN end frame (no error, no abort) from the OLD daemon after a
handover promotes the successor exactly like the abort path does — no
down/up hooks, `elisp.link.handover-complete'.
fanout §6 + §3: \"`(:ended)' ... a failure\"; every existing promotion
test in this suite drops the old daemon's TCP connection with `abort',
never sends it a clean terminal envelope, so this path has never run."
  (agent-repl-itest--with-fake-daemon primary
    (agent-repl-itest-link--with-link primary
      (agent-repl-itest--with-second-daemon primary successor
        (let ((hooks nil))
          (agent-repl-itest-link--announce
           primary (agent-repl-itest-daemon-address successor))
          (agent-repl-itest--await-subscriber successor "daemon")
          (agent-repl-itest--wait-until
           #'agent-repl-link-successor nil "the successor to be ACCEPTED")
          (add-hook 'agent-repl-link-down-functions (lambda (&rest _) (push 'down hooks)))
          (add-hook 'agent-repl-link-up-functions (lambda (&rest _) (push 'up hooks)))
          (let ((successor-conn (agent-repl-link-successor)))
            ;; Act: a CLEAN end frame — no error, no abort.
            (agent-repl-itest--end primary "daemon")
            ;; Assert.
            (agent-repl-itest--await-log primary "elisp.link.handover-complete" "info")
            (agent-repl-itest--wait-until
             (lambda () (null (agent-repl-link-successor)))
             nil "the successor to be promoted to primary")
            (should (null hooks))
            (should (eq (agent-repl-link-primary) successor-conn))
            (should (agent-repl-link-up-p))))))))

;; audit-3 #8
(ert-deftest agent-repl-itest-link-re-announcing-the-same-successor-address-is-idempotent ()
  "A second `shutdown_announced{address}' naming the ALREADY-ATTACHED
successor is a no-op: exactly one connection stands, the handover hooks
fire only once, and the log names it `elisp.link.successor-already-
attached' rather than opening a second connection.
daemon-link.el: \"an already attached successor at the same address is a
no-op, never a second connection\"; elisp.md dual attach opens \"a second
connection\" (singular)."
  (agent-repl-itest--with-fake-daemon primary
    (agent-repl-itest-link--with-link primary
      (agent-repl-itest--with-second-daemon primary successor
        (let ((handovers nil)
              (address (agent-repl-itest-daemon-address successor)))
          (add-hook 'agent-repl-link-handover-functions
                    (lambda (old new) (push (cons old new) handovers)))
          ;; Act: announce, wait for acceptance, then announce AGAIN at the
          ;; SAME address.
          (agent-repl-itest-link--announce primary address)
          (agent-repl-itest--await-subscriber successor "daemon")
          (agent-repl-itest--wait-until
           #'agent-repl-link-successor nil "the successor to be ACCEPTED")
          (agent-repl-itest-link--announce primary address)
          ;; Assert.
          (agent-repl-itest--await-log
           primary "elisp.link.successor-already-attached" "debug")
          (should (equal 1 (length (agent-repl-itest--subscribers successor "daemon"))))
          (should (equal (length handovers) 1)))))))

;; audit-3 #8
(ert-deftest agent-repl-itest-link-re-announcing-a-different-successor-address-is-refused ()
  "A second `shutdown_announced{address}' naming a DIFFERENT address while
a successor already stands is refused loudly, and the standing successor
is left UNCHANGED — no attempt to open a second connection nor to
abandon the first."
  (agent-repl-itest--with-fake-daemon primary
    (agent-repl-itest-link--with-link primary
      (agent-repl-itest--with-second-daemon primary successor
        (let ((address (agent-repl-itest-daemon-address successor)))
          (agent-repl-itest-link--announce primary address)
          (agent-repl-itest--await-subscriber successor "daemon")
          (agent-repl-itest--wait-until
           #'agent-repl-link-successor nil "the successor to be ACCEPTED")
          (let ((successor-conn (agent-repl-link-successor)))
            ;; Act: announce a DIFFERENT address while the successor stands.
            (agent-repl-itest-link--announce primary "127.0.0.1:1")
            ;; Assert.
            (agent-repl-itest--await-log
             primary "elisp.link.successor-address-changed" "error")
            (should (eq (agent-repl-link-successor) successor-conn))))))))

;; audit-3 #9
(ert-deftest agent-repl-itest-link-successor-refused-before-acceptance ()
  "An announced successor address that refuses the connection outright —
nothing was ever listening — never becomes adoptable, never runs the
handover or down hooks, and leaves the old link standing.
daemon-link.el's pending-successor branch of `agent-repl-link--handle-
close'; audit-2 #4 covers a successor dying AFTER acceptance only."
  (agent-repl-itest--with-fake-daemon primary
    (agent-repl-itest-link--with-link primary
      (let ((handover-calls nil)
            (down-calls nil))
        (add-hook 'agent-repl-link-handover-functions
                  (lambda (old new) (push (cons old new) handover-calls)))
        (add-hook 'agent-repl-link-down-functions
                  (lambda (&rest _) (push t down-calls)))
        ;; Act: announce an address nothing is listening on.
        (agent-repl-itest-link--announce primary "127.0.0.1:1")
        ;; Assert.
        (agent-repl-itest--await-log
         primary "elisp.link.successor-open-refused" "error")
        (should (null (agent-repl-link-successor)))
        (should (agent-repl-link-up-p))
        (should (null handover-calls))
        (should (null down-calls))))))

;; audit-3 #10
(ert-deftest agent-repl-itest-link-bounce-indicator-clears-once-reconnected ()
  "The bounce indicator, drawn while the outage is waited out, is taken
DOWN once the reconnected link is accepted on the returning daemon.
fanout §6 \"daemon restarting (<cause>)\"; the existing
`agent-repl-itest-link-bounce-reconnects-once-the-addr-returns' asserts
only `agent-repl-link-up-p' — a regression that left the segment drawn
forever after a successful reconnect would still pass it."
  (agent-repl-itest--with-fake-daemon primary
    (agent-repl-itest-link--with-link primary
      (let ((state-dir (agent-repl-itest-daemon-state-dir primary)))
        (agent-repl-itest-link--announce primary nil)
        (agent-repl-itest--wait-until
         (lambda () agent-repl-link-drain-segment) nil
         "the bounce indicator to be drawn")
        (should agent-repl-link-drain-segment)
        ;; Act: the bounce actually happens.
        (agent-repl-itest--stop-daemon primary t)
        (let ((successor (agent-repl-itest--start-daemon state-dir)))
          (unwind-protect
              (progn
                (agent-repl-itest--await-subscriber successor "daemon")
                (agent-repl-itest--wait-until
                 #'agent-repl-link-up-p nil
                 "the reconnected link to be ACCEPTED on the successor")
                ;; Assert: the indicator is taken down.
                (should (null agent-repl-link-drain-segment)))
            (agent-repl-itest--stop-daemon successor t)))))))

;; audit-3 #11
(ert-deftest agent-repl-itest-link-drain-scheduled-with-a-blank-operator-note-is-a-breach ()
  "A `drain_scheduled' whose `DrainReason.operator.note' is the empty
string is a breach: production logs `elisp.link.drain-operator-note-
blank' ERROR, and the stream stands regardless — the breach is a logged
fact, never a reason to tear the link down.
drain_reason.proto: \"The note is REQUIRED non-blank\"; a plain proto3
string field cannot enforce that at the wire, so this is elisp's own
business-rule check inside `agent-repl-link--reason-text'."
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-link--with-link daemon
      ;; Act.
      (agent-repl-itest--push
       daemon "daemon"
       '((drainScheduled . ((atMs . "1735689600000")
                            (reason . ((operator . ((note . "")))))))))
      ;; Assert.
      (agent-repl-itest--await-log
       daemon "elisp.link.drain-operator-note-blank" "error")
      (should (agent-repl-itest--logged-p
               daemon "elisp.link.drain-operator-note-blank" "error"))
      (should (agent-repl-link-up-p)))))

;; audit-3 #11
(ert-deftest agent-repl-itest-link-shutdown-announced-immediate-with-a-blank-operator-note-is-a-breach ()
  "The same breach rides `shutdown_announced{cause:{immediate:{reason:
{operator:{}}}}}' — the operator arm present with `note' entirely absent,
which decodes to the SAME blank string as an explicit empty one, since a
plain proto3 string carries no presence bit.
drain_reason.proto REQUIRED non-blank; production logs this from
`agent-repl-link--cause-text''s call into `agent-repl-link--reason-text',
reached only via the bounce indicator's own recompute."
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-link--with-link daemon
      ;; Act.
      (agent-repl-itest-link--announce
       daemon nil '((immediate . ((reason . ((operator . ()))))))
       ;; The window outlasts the wait below; see the helper.
       (* 2 1000 agent-repl-itest-default-timeout))
      ;; Assert.
      (agent-repl-itest--await-log
       daemon "elisp.link.drain-operator-note-blank" "error")
      (should (agent-repl-itest--logged-p
               daemon "elisp.link.drain-operator-note-blank" "error"))
      (should (agent-repl-link-up-p)))))

;; audit-3 #11
(ert-deftest agent-repl-itest-link-drain-scheduled-without-a-reason-is-dropped-not-fatal ()
  "`drain_scheduled' missing `reason' (a non-optional message) is logged
`elisp.rpc.push-invalid' and dropped; the standing drain schedule is
UNCHANGED.
`endpoint_watch_daemon.proto' `DaemonDrainScheduled.reason' is non-
optional, mirroring the `shutdown_announced' / `cause' breach already
pinned for this stream (`agent-repl-itest-link-invalid-shutdown-
announced-is-dropped-not-fatal') — but nothing here pins the drain-
scheduled push's own required field."
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-link--with-link daemon
      ;; Act: `reason' is entirely absent.
      (agent-repl-itest--push
       daemon "daemon" '((drainScheduled . ((atMs . "1735689600000")))))
      ;; Assert.
      (agent-repl-itest--await-log daemon "elisp.rpc.push-invalid" "error")
      (should (agent-repl-itest--logged-p daemon "elisp.rpc.push-invalid" "error"))
      (should (null agent-repl-link-drain)))))

;;;; ---- The elisp build and the deploy's reload ----
;;
;; Every process reports its build when it connects: Emacs's WatchDaemon
;; names it as the `emacs' client with the elisp it loaded, and the fake
;; daemon refuses one that does not, as the daemon does.  A deploy that
;; finds that build stale pushes `reload_elisp' on the same stream.
;;
;; THE LOAD IS STUBBED, NOT RESTORED.  `agent-repl--elisp-reload-load-file'
;; is a registered boundary, but its real target is this very batch
;; process: loading the module set here would re-`defun' every production
;; boundary wrapper and disarm the harness's guards for the rest of the run.
;; So the scenario stubs that one call (and the heartbeat assertion, whose
;; timers the batch never arms) and drives everything else for real: the
;; wire, the decode, the dispatch, the root check and the timer.

(declare-function agent-repl-elisp-build "elisp-build")
(declare-function agent-repl-elisp-build-of "elisp-build")
(declare-function agent-repl-elisp-build-entries "elisp-build")
(defvar agent-repl--elisp-module-builds)
(defvar agent-repl--frontend-root)

(defun agent-repl-itest-link--write-bytes (file content)
  "Write CONTENT to FILE as its UTF-8 bytes, creating its directory."
  (make-directory (file-name-directory file) t)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert (encode-coding-string content 'utf-8))
    (let ((coding-system-for-write 'no-conversion))
      (write-region (point-min) (point-max) file nil 'silent))))

(ert-deftest agent-repl-itest-link-watch-daemon-reports-the-elisp-build ()
  "The WatchDaemon the link opens names Emacs and the elisp build it loaded."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-link--with-link daemon
      ;; Act.
      (let ((body (car (last (agent-repl-itest--call-bodies daemon "WatchDaemon")))))
        ;; Assert.
        (should (equal (alist-get 'elispBuild (alist-get 'emacs body))
                       (agent-repl-elisp-build)))))))

(ert-deftest agent-repl-itest-link-watch-daemon-states-emacs-focus ()
  "The WatchDaemon the link opens states whether Emacs is focused.
The daemon decides every desktop banner on that focus from the stream's
first instant.  A batch Emacs holds no window-system focus: unfocused."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-link--with-link daemon
      ;; Act.
      (let ((body (car (last (agent-repl-itest--call-bodies daemon "WatchDaemon")))))
        ;; Assert.
        (should (equal (alist-get 'focus (alist-get 'emacs body))
                       '((unfocused))))))))

(ert-deftest agent-repl-itest-link-up-reports-emacs-focus ()
  "Once the link stands, Emacs reports its focus to the daemon.
Focus that moved between building the WatchDaemon request and its
acceptance is told here, once a stream stands to own it."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-link--with-real-hooks daemon
      ;; Act: the link came up, which is the edge under test.
      (agent-repl-itest--await-call daemon "ReportEditorFocus")
      ;; Assert.
      (should (equal (alist-get 'focus (car (agent-repl-itest--call-bodies daemon "ReportEditorFocus")))
                     '((unfocused)))))))

(ert-deftest agent-repl-itest-link-reload-elisp-for-another-root-is-refused ()
  "A reload naming another checkout is refused at ERROR; the link stays up."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-link--with-link daemon
      (let ((loaded nil))
        (cl-letf (((symbol-function 'agent-repl--elisp-reload-load-file)
                   (lambda (file) (push file loaded))))
          ;; Act.
          (agent-repl-itest--push
           daemon "daemon"
           '((reloadElisp . ((moduleRoot . "/not/this/checkout/agent-repl/")
                             (build . "b-elsewhere")))))
          ;; Assert.
          (agent-repl-itest--await-log daemon "elisp.elisp-build.reload-refused" "error")
          (should (equal (list loaded (agent-repl-link-up-p)) '(nil t))))))))

(ert-deftest agent-repl-itest-link-reload-elisp-for-this-root-loads-the-set ()
  "A reload naming this Emacs's root loads config.el's modules in order."
  ;; Arrange.
  (let ((root (file-name-as-directory (make-temp-file "agent-repl-itest-root" t)))
        (loaded nil)
        (agent-repl--elisp-module-builds agent-repl--elisp-module-builds))
    (unwind-protect
        (progn
          (agent-repl-itest-link--write-bytes
           (expand-file-name "config.el" root)
           "(agent-repl--load-module \"core\")\n(agent-repl--load-module \"status\")\n")
          (agent-repl-itest-link--write-bytes (expand-file-name "lisp/core.el" root) ";; core\n")
          (agent-repl-itest-link--write-bytes (expand-file-name "lisp/status.el" root) ";; status\n")
          (let ((agent-repl--frontend-root root))
            (agent-repl-itest--with-fake-daemon daemon
              (agent-repl-itest-link--with-link daemon
                (cl-letf (((symbol-function 'agent-repl--elisp-reload-load-file)
                           (lambda (file) (push (file-name-base file) loaded)))
                          ((symbol-function 'agent-repl--assert-heartbeat-armed)
                           (lambda () '(:armed nil :rearmed nil :failed nil :unavailable nil))))
                  ;; Act.
                  (agent-repl-itest--push
                   daemon "daemon"
                   `((reloadElisp
                      . ((moduleRoot . ,root)
                         (build . ,(agent-repl-elisp-build-of
                                    (agent-repl-elisp-build-entries root '("core" "status"))))))))
                  (agent-repl-itest--await-log daemon "elisp.elisp-build.reload-complete" "info")
                  ;; Assert.
                  (should (equal (list (reverse loaded)
                                       (agent-repl-itest--logged-p daemon "elisp.elisp-build" "error"))
                                 '(("core" "status") nil))))))))
      (delete-directory root t))))

(provide 'test-integration-link)

;;; test-integration-link.el ends here
