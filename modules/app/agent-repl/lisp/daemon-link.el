;;; daemon-link.el --- the daemon connection's lifecycle -*- lexical-binding: t; -*-

;;; Commentary:

;; ONE PROCESS, ONE LINK.  Emacs is the daemon's singular client
;; multiplexer: it holds exactly one `WatchDaemon' stream, and every
;; per-workspace stream rides the same connection.  This file owns that
;; connection's whole life — opening it, watching it die, reconnecting,
;; and carrying the blue-green handover — and NOTHING else.  It never
;; issues Register, Select or any workspace verb; host.el, roster.el and
;; verbs.el do that, hanging off the hooks below.
;;
;; THE HOOKS ARE THE SEAM.  Consumers register on:
;;
;;   `agent-repl-link-no-daemon-functions'  no daemon.addr — cold start
;;   `agent-repl-link-up-functions' (CONN)  a link stands
;;   `agent-repl-link-down-functions' (CONN) the link died unexpectedly
;;   `agent-repl-link-handover-functions' (OLD NEW)  a successor is up
;;   `agent-repl-link-drain-functions' (DRAIN)  the drain schedule moved
;;
;; THE HANDOVER, from this file's seat.  `shutdown_announced' WITH an
;; address means a successor daemon is already listening: Emacs DUAL
;; ATTACHES — a second connection with its own `WatchDaemon', while the
;; old one is KEPT, because each workspace keeps being served by whichever
;; daemon still owns it.  host.el then adopts each workspace as the old
;; daemon releases it.  When the old daemon finally drops its own stream,
;; the successor is PROMOTED silently: no down hooks and no up hooks fire,
;; because nothing was lost — every workspace was already adopted.
;;
;; `shutdown_announced' WITHOUT an address is a PLAIN BOUNCE: no successor
;; exists, so there is nothing to attach to and the only correct behavior
;; is to wait the announced outage out.  The announcement carries
;; `minted_at_ms' and `expected_outage_ms' precisely so a LATE receiver
;; shortens its quiet window instead of restarting it; the reconnect loop
;; polls nothing until that instant passes.
;;
;; NO SLEEPS ANYWHERE.  Every wait in this file is a timer, and every
;; timer's function is a named `defun' a test can call directly.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function agent-repl--log "core" (ws fmt &rest args))
(declare-function agent-repl--info "core" (ws fmt &rest args))
(declare-function agent-repl--warn "core" (ws fmt &rest args))
(declare-function agent-repl--error "core" (ws fmt &rest args))

(declare-function agent-repl-connect-read-daemon-addr "connect" ())
(declare-function agent-repl-connect-open "connect" (address))
(declare-function agent-repl-connect-close "connect" (conn))
(declare-function agent-repl-connect-connection-address "connect" (conn))
(declare-function agent-repl-connect-connection-alive-p "connect" (conn))
(declare-function agent-repl-connect-failure-message "connect" (detail))
(declare-function agent-repl-rpc-watch-daemon "rpc" (conn on-push on-close))

;;;; ---- Customization ----

(defcustom agent-repl-link-reconnect-interval-seconds 1.0
  "Seconds between the first reconnect attempts after the link dies.
The poll is a `daemon.addr' read plus one connection attempt, so a short
interval costs nothing while the daemon is coming back; it backs off to
`agent-repl-link-reconnect-max-interval-seconds' when it does not."
  :type 'number
  :group 'agent-repl)

(defcustom agent-repl-link-reconnect-max-interval-seconds 5.0
  "Ceiling the reconnect poll backs off to.
A daemon that is gone for good must not be polled at 1 Hz forever, and a
daemon that comes back is discovered within this bound."
  :type 'number
  :group 'agent-repl)

;;;; ---- Hooks ----

(defvar agent-repl-link-no-daemon-functions nil
  "Functions run with no arguments when `daemon.addr' names no daemon.
An ABSENT address file is the legal no-daemon state, not an error: the
cold start (daemon.el) hangs here and builds and starts one.")

(defvar agent-repl-link-up-functions nil
  "Functions run with the CONNECTION once a `WatchDaemon' stream stands.
Runs on the first connect and after every reconnect — host.el
re-registers and re-subscribes its workspaces, roster.el re-subscribes.
It does NOT run for a handover promotion: nothing was lost there.")

(defvar agent-repl-link-down-functions nil
  "Functions run with the CONNECTION when the link dies unexpectedly.
A client-side cancel is not a death and never reaches here, nor does the
old daemon's stream closing after a handover (that is a promotion).")

(defvar agent-repl-link-handover-functions nil
  "Functions run with (OLD-CONN NEW-CONN) when a successor daemon is up.
Both connections are live at this point, by design: each workspace keeps
flowing from whichever daemon still owns it until it is transferred.")

(defvar agent-repl-link-drain-functions nil
  "Functions run with the current value of `agent-repl-link-drain'.
Nil means the standing schedule was cancelled.")

;;;; ---- State ----

(defvar agent-repl-link--primary nil
  "The connection to the daemon currently serving this Emacs, or nil.")

(defvar agent-repl-link--successor nil
  "The connection to a JOINING successor daemon during a handover, or nil.")

(defvar agent-repl-link--primary-stream nil
  "The `WatchDaemon' stream standing on `agent-repl-link--primary'.")

(defvar agent-repl-link--successor-stream nil
  "The `WatchDaemon' stream standing on `agent-repl-link--successor'.")

(defvar agent-repl-link-drain nil
  "The standing drain schedule, or nil when none is armed.
Shape: `(:at-ms N :reason (:arm KEYWORD :value VALUE))', decoded verbatim
from the `drain_scheduled' push.")

(defvar agent-repl-link-drain-segment nil
  "The `global-mode-string' segment drawn for a standing drain or bounce.
A string, or nil when there is nothing to say.  Recomputed by
`agent-repl-link--refresh-indicator'; never set by hand.")
(put 'agent-repl-link-drain-segment 'risky-local-variable t)

(defvar agent-repl-link--quiet-until-ms nil
  "Epoch ms before which the reconnect loop does not poll, or nil.
Set from a plain bounce's `minted_at_ms + expected_outage_ms' — the
announced end of the outage, not a duration, so a LATE receiver waits
what is left rather than the whole window again.")

(defvar agent-repl-link--bounce-cause nil
  "The cause arm keyword of the plain bounce being waited out, or nil.")

(defvar agent-repl-link--reconnect-timer nil
  "The pending reconnect timer, or nil when no reconnect is scheduled.")

(defvar agent-repl-link--reconnect-interval nil
  "Seconds the next reconnect poll waits; grows toward the ceiling.")

;;;; ---- Accessors ----

(defun agent-repl-link-primary ()
  "Return the connection to the daemon currently serving Emacs, or nil."
  agent-repl-link--primary)

(defun agent-repl-link-successor ()
  "Return the successor daemon's connection during a handover, or nil.
host.el reads this when a workspace's `transferred' push arrives: the
adopt call goes to the SUCCESSOR, and its absence is a contract breach
worth an ERROR rather than an improvised reconnect."
  agent-repl-link--successor)

(defun agent-repl-link-up-p ()
  "Return non-nil when a live primary connection stands."
  (and agent-repl-link--primary
       (agent-repl-connect-connection-alive-p agent-repl-link--primary)
       t))

(defun agent-repl-link--now-ms ()
  "Return the current instant as epoch milliseconds.
The wire carries instants, never durations-since-now, so every comparison
in this file is against this one clock reading."
  (truncate (* 1000 (float-time))))

;;;; ---- Connecting ----

(defun agent-repl-link--open (address)
  "Open a connection to ADDRESS and stand a `WatchDaemon' stream on it.
Returns the connection, or nil when either step fails — a failure here is
an ordinary absent-daemon outcome (the address file can name a daemon
that has already exited), so it is WARNED and answered with nil rather
than signalled."
  (condition-case err
      (let ((conn (agent-repl-connect-open address)))
        (condition-case stream-err
            (let ((stream (agent-repl-rpc-watch-daemon
                           conn
                           (lambda (push) (agent-repl-link--handle-push conn push))
                           (lambda (outcome) (agent-repl-link--handle-close conn outcome)))))
              (agent-repl--info nil "elisp.link.open address=%S" address)
              (cons conn stream))
          (error
           (agent-repl--warn nil "elisp.link.watch-daemon-failed address=%S error=%S"
                             address stream-err)
           (agent-repl-connect-close conn)
           nil)))
    (error
     (agent-repl--warn nil "elisp.link.open-failed address=%S error=%S" address err)
     nil)))

(defun agent-repl-link-connect ()
  "Discover the daemon and stand the link, or run the no-daemon hooks.
Returns the primary connection, or nil.  An ABSENT `daemon.addr' is the
legal no-daemon state and runs `agent-repl-link-no-daemon-functions' —
the cold start's entry point — rather than failing."
  (if (agent-repl-link-up-p)
      (progn
        (agent-repl--log nil "elisp.link.connect-noop address=%S"
                         (agent-repl-connect-connection-address agent-repl-link--primary))
        agent-repl-link--primary)
    (let ((address (agent-repl-connect-read-daemon-addr)))
      (if (null address)
          (progn
            (agent-repl--info nil "elisp.link.no-daemon")
            (run-hooks 'agent-repl-link-no-daemon-functions)
            nil)
        (let ((opened (agent-repl-link--open address)))
          (if (null opened)
              (progn
                (agent-repl--warn nil "elisp.link.connect-failed address=%S" address)
                nil)
            (setq agent-repl-link--primary (car opened)
                  agent-repl-link--primary-stream (cdr opened))
            (agent-repl-link--cancel-reconnect)
            (setq agent-repl-link--quiet-until-ms nil
                  agent-repl-link--bounce-cause nil)
            (agent-repl-link--refresh-indicator)
            (agent-repl--info nil "elisp.link.up address=%S" address)
            (run-hook-with-args 'agent-repl-link-up-functions agent-repl-link--primary)
            agent-repl-link--primary))))))

(defun agent-repl-link-teardown ()
  "Close every connection this link holds and forget all of its state.
The client-side close is the graceful one: each standing stream's
ON-CLOSE runs with `(:cancelled)', which this file treats as normal."
  (agent-repl--info nil "elisp.link.teardown primary=%s successor=%s"
                    (if agent-repl-link--primary "t" "nil")
                    (if agent-repl-link--successor "t" "nil"))
  (agent-repl-link--cancel-reconnect)
  (let ((primary agent-repl-link--primary)
        (successor agent-repl-link--successor))
    (setq agent-repl-link--primary nil
          agent-repl-link--primary-stream nil
          agent-repl-link--successor nil
          agent-repl-link--successor-stream nil
          agent-repl-link--quiet-until-ms nil
          agent-repl-link--bounce-cause nil
          agent-repl-link-drain nil)
    (when primary (agent-repl-connect-close primary))
    (when successor (agent-repl-connect-close successor)))
  (agent-repl-link--refresh-indicator))

;;;; ---- The reconnect loop ----

(defun agent-repl-link--cancel-reconnect ()
  "Cancel any pending reconnect timer and reset the backoff."
  (when (timerp agent-repl-link--reconnect-timer)
    (cancel-timer agent-repl-link--reconnect-timer))
  (setq agent-repl-link--reconnect-timer nil
        agent-repl-link--reconnect-interval nil))

(defun agent-repl-link--next-interval ()
  "Return the seconds the next reconnect poll waits, growing the backoff."
  (let ((interval (or agent-repl-link--reconnect-interval
                      agent-repl-link-reconnect-interval-seconds)))
    (setq agent-repl-link--reconnect-interval
          (min agent-repl-link-reconnect-max-interval-seconds (* 2 interval)))
    interval))

(defun agent-repl-link--schedule-reconnect ()
  "Arm the reconnect poll, replacing any timer already pending."
  (when (timerp agent-repl-link--reconnect-timer)
    (cancel-timer agent-repl-link--reconnect-timer))
  (let ((interval (agent-repl-link--next-interval)))
    (agent-repl--log nil "elisp.link.reconnect-scheduled interval=%.3f quiet-until=%S"
                     interval agent-repl-link--quiet-until-ms)
    (setq agent-repl-link--reconnect-timer
          (run-with-timer interval nil #'agent-repl-link--reconnect-tick))))

(defun agent-repl-link--quiet-p ()
  "Return non-nil while the announced plain-bounce outage has not elapsed."
  (and agent-repl-link--quiet-until-ms
       (< (agent-repl-link--now-ms) agent-repl-link--quiet-until-ms)))

(defun agent-repl-link--reconnect-tick ()
  "One poll of the reconnect loop: the timer's whole body.
Named and argument-free so a test drives the loop by calling it, with no
dependence on the scheduler and no sleep anywhere."
  (setq agent-repl-link--reconnect-timer nil)
  (cond
   ((agent-repl-link-up-p)
    (agent-repl--log nil "elisp.link.reconnect-already-up")
    (agent-repl-link--cancel-reconnect))
   ((agent-repl-link--quiet-p)
    (agent-repl--log nil "elisp.link.reconnect-quiet until=%S now=%S"
                     agent-repl-link--quiet-until-ms (agent-repl-link--now-ms))
    (agent-repl-link--schedule-reconnect))
   (t
    (let ((address (condition-case err
                       (agent-repl-connect-read-daemon-addr)
                     (error
                      (agent-repl--warn nil "elisp.link.reconnect-addr-unreadable error=%S" err)
                      nil))))
      (if (null address)
          (progn
            (agent-repl--log nil "elisp.link.reconnect-no-address")
            (agent-repl-link--schedule-reconnect))
        (let ((opened (agent-repl-link--open address)))
          (if (null opened)
              (progn
                (agent-repl--log nil "elisp.link.reconnect-refused address=%S" address)
                (agent-repl-link--schedule-reconnect))
            (setq agent-repl-link--primary (car opened)
                  agent-repl-link--primary-stream (cdr opened))
            (agent-repl-link--cancel-reconnect)
            (setq agent-repl-link--quiet-until-ms nil
                  agent-repl-link--bounce-cause nil)
            (agent-repl-link--refresh-indicator)
            (agent-repl--info nil "elisp.link.reconnected address=%S" address)
            (run-hook-with-args 'agent-repl-link-up-functions
                                agent-repl-link--primary))))))))

;;;; ---- Stream close: death, cancel, promotion ----

(defun agent-repl-link--promote-successor ()
  "Make the successor the primary, silently.
No down hooks and no up hooks run: every workspace was adopted onto the
successor as it was transferred, so nothing has to be rebuilt and firing
the reconnect hooks would re-register a fleet that is already registered."
  (let ((old agent-repl-link--primary))
    (setq agent-repl-link--primary agent-repl-link--successor
          agent-repl-link--primary-stream agent-repl-link--successor-stream
          agent-repl-link--successor nil
          agent-repl-link--successor-stream nil)
    (agent-repl--info nil "elisp.link.successor-promoted address=%S"
                      (agent-repl-connect-connection-address agent-repl-link--primary))
    (when old (agent-repl-connect-close old))))

(defun agent-repl-link--handle-close (conn outcome)
  "React to the `WatchDaemon' stream on CONN closing with OUTCOME.
OUTCOME is connect.el's vocabulary: `(:cancelled)' is the client's own
graceful close and means nothing here; `(:ended)' is a producer-side end
of a STANDING stream, which the contract calls a transport failure; and
`(:error DETAIL)' is one already."
  (let ((kind (car outcome)))
    (cond
     ((eq conn agent-repl-link--successor)
      ;; The successor died before it took over.  The old daemon still
      ;; owns whatever it has not transferred, so the link is not down —
      ;; but the handover cannot complete, and host.el must not adopt onto
      ;; a corpse.
      (if (eq kind :cancelled)
          (agent-repl--log nil "elisp.link.successor-stream-cancelled")
        (agent-repl--error nil "elisp.link.successor-stream-lost outcome=%S" outcome)
        (let ((successor agent-repl-link--successor))
          (setq agent-repl-link--successor nil
                agent-repl-link--successor-stream nil)
          (agent-repl-connect-close successor))))
     ((not (eq conn agent-repl-link--primary))
      (agent-repl--log nil "elisp.link.stale-stream-close outcome=%S" outcome))
     ((eq kind :cancelled)
      (agent-repl--log nil "elisp.link.primary-stream-cancelled"))
     (agent-repl-link--successor
      ;; The old daemon finished and dropped its stream after a handover.
      (agent-repl--info nil "elisp.link.handover-complete outcome=%S" outcome)
      (agent-repl-link--promote-successor))
     (t
      (agent-repl--warn nil "elisp.link.down outcome=%S address=%S" outcome
                        (agent-repl-connect-connection-address conn))
      (setq agent-repl-link--primary nil
            agent-repl-link--primary-stream nil)
      (run-hook-with-args 'agent-repl-link-down-functions conn)
      (agent-repl-connect-close conn)
      (agent-repl-link--schedule-reconnect)))))

;;;; ---- Pushes ----

(defun agent-repl-link--handle-push (conn push)
  "Dispatch one decoded `WatchDaemon' PUSH received on CONN."
  (let ((arm (plist-get push :arm))
        (value (plist-get push :value)))
    (pcase arm
      (:shutdown-announced (agent-repl-link--shutdown-announced conn value))
      (:drain-scheduled (agent-repl-link--drain-scheduled value))
      (:drain-cancelled (agent-repl-link--drain-cancelled))
      (_ (agent-repl--error nil "elisp.link.unknown-daemon-push arm=%S push=%S"
                            arm push)))))

(defun agent-repl-link--shutdown-announced (conn announcement)
  "React to a `shutdown_announced' ANNOUNCEMENT received on CONN.
With an address a successor is already listening and Emacs DUAL ATTACHES;
without one this is a plain bounce and the only act is to size the quiet
window from the announced instants."
  (let ((address (plist-get announcement :address))
        (cause (plist-get announcement :cause))
        (outage (or (plist-get announcement :expected-outage-ms) 0))
        (minted (or (plist-get announcement :minted-at-ms) 0)))
    (if address
        (agent-repl-link--attach-successor conn address cause)
      (setq agent-repl-link--quiet-until-ms (+ minted outage)
            agent-repl-link--bounce-cause (plist-get cause :arm))
      (agent-repl--info nil
                        "elisp.link.plain-bounce cause=%S minted-at-ms=%S expected-outage-ms=%S quiet-until-ms=%S"
                        (plist-get cause :arm) minted outage
                        agent-repl-link--quiet-until-ms)
      (agent-repl-link--refresh-indicator))))

(defun agent-repl-link--attach-successor (old address cause)
  "Dual-attach to the successor daemon at ADDRESS announced on OLD.
The old connection is KEPT: until each workspace is transferred it is the
daemon still serving it.  Re-announcements are idempotent — an already
attached successor at the same address is a no-op, never a second
connection."
  (cond
   ((and agent-repl-link--successor
         (equal address (agent-repl-connect-connection-address
                         agent-repl-link--successor)))
    (agent-repl--log nil "elisp.link.successor-already-attached address=%S" address))
   (agent-repl-link--successor
    (agent-repl--error nil "elisp.link.successor-address-changed old=%S new=%S"
                       (agent-repl-connect-connection-address agent-repl-link--successor)
                       address))
   (t
    (let ((opened (agent-repl-link--open address)))
      (if (null opened)
          (agent-repl--error nil "elisp.link.successor-attach-failed address=%S cause=%S"
                             address (plist-get cause :arm))
        (setq agent-repl-link--successor (car opened)
              agent-repl-link--successor-stream (cdr opened))
        (agent-repl--info nil "elisp.link.handover-announced address=%S cause=%S"
                          address (plist-get cause :arm))
        (run-hook-with-args 'agent-repl-link-handover-functions
                            old agent-repl-link--successor))))))

(defun agent-repl-link--drain-scheduled (schedule)
  "Record the standing drain SCHEDULE and redraw the indicator."
  (setq agent-repl-link-drain schedule)
  (agent-repl--info nil "elisp.link.drain-scheduled at-ms=%S reason=%S"
                    (plist-get schedule :at-ms)
                    (plist-get (plist-get schedule :reason) :arm))
  (agent-repl-link--refresh-indicator)
  (run-hook-with-args 'agent-repl-link-drain-functions agent-repl-link-drain))

(defun agent-repl-link--drain-cancelled ()
  "Drop the standing drain schedule and take the indicator down."
  (setq agent-repl-link-drain nil)
  (agent-repl--info nil "elisp.link.drain-cancelled")
  (agent-repl-link--refresh-indicator)
  (run-hook-with-args 'agent-repl-link-drain-functions nil))

;;;; ---- The indicator ----

(defun agent-repl-link--reason-text (reason)
  "Return the human phrase for a decoded `DrainReason' REASON.
The arm IS the reason; only the operator arm carries prose, and its note
is REQUIRED non-blank at the request, so an empty one is a breach worth
naming rather than an empty segment."
  (let ((arm (plist-get reason :arm))
        (value (plist-get reason :value)))
    (pcase arm
      (:deploy "deploy")
      (:maintenance "maintenance")
      (:operator
       (let ((note (plist-get value :note)))
         (if (and (stringp note) (not (string-blank-p note)))
             note
           (agent-repl--error nil "elisp.link.drain-operator-note-blank reason=%S" reason)
           "operator")))
      (_
       (agent-repl--error nil "elisp.link.drain-reason-unknown arm=%S" arm)
       "unknown"))))

(defun agent-repl-link--cause-text (arm)
  "Return the human phrase for a `DaemonShutdownCause' ARM keyword."
  (pcase arm
    (:self-merge-rollout "self-merge rollout")
    (:scheduled-drain "scheduled drain")
    (:immediate "immediate")
    (_ "unknown")))

(defun agent-repl-link--compute-drain-segment ()
  "Return the mode-line text for the standing drain or bounce, or nil.
A standing drain reads \"drain HH:MM · <reason>\"; a plain bounce being
waited out reads \"daemon restarting (<cause>)\".  Nil when neither
stands, which is what keeps the segment off the mode line entirely."
  (cond
   (agent-repl-link-drain
    (let ((at-ms (plist-get agent-repl-link-drain :at-ms)))
      (format "drain %s · %s"
              (format-time-string "%H:%M" (seconds-to-time (/ at-ms 1000.0)))
              (agent-repl-link--reason-text
               (plist-get agent-repl-link-drain :reason)))))
   ((agent-repl-link--quiet-p)
    (format "daemon restarting (%s)"
            (agent-repl-link--cause-text agent-repl-link--bounce-cause)))
   (t nil)))

(defun agent-repl-link--refresh-indicator ()
  "Recompute `agent-repl-link-drain-segment' and log what it now says."
  (setq agent-repl-link-drain-segment (agent-repl-link--compute-drain-segment))
  (agent-repl--log nil "elisp.link.indicator segment=%S" agent-repl-link-drain-segment)
  (force-mode-line-update t)
  agent-repl-link-drain-segment)

(defun agent-repl-link-install-indicator ()
  "Add the drain segment to `global-mode-string', once.
Idempotent: the segment is the variable itself, so a second install would
draw it twice."
  (let ((current (if (listp global-mode-string)
                     global-mode-string
                   (list global-mode-string))))
    (if (memq 'agent-repl-link-drain-segment current)
        (agent-repl--log nil "elisp.link.indicator-already-installed")
      (setq global-mode-string
            (append current '(agent-repl-link-drain-segment)))
      (agent-repl--log nil "elisp.link.indicator-installed"))))

(agent-repl-link-install-indicator)

(provide 'daemon-link)

;;; daemon-link.el ends here
