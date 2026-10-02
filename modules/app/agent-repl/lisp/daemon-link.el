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
;;   `agent-repl-link-promote-functions' (OLD NEW)  the successor took over
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
;; ACCEPTANCE, NEVER A SPAWN.  A link is up when the daemon ACCEPTED its
;; `WatchDaemon' subscription — the HTTP 200 header block, which the daemon
;; flushes on accept — and not a moment earlier.  Spawning a curl against
;; an address that names a corpse succeeds; only acceptance proves a daemon
;; is there.  So `agent-repl-link--open' returns as soon as the transport
;; exists, but the connection sits PENDING: the primary is marked up, the
;; indicator refreshed and `agent-repl-link-up-functions' run only from
;; connect.el's ON-OPEN.  A pending stream that dies goes straight to the
;; reconnect loop and fires NO down hooks, because it was never up.  The
;; successor is the same story: it becomes adoptable — `agent-repl-link-
;; successor' answers it, and the handover hooks run — only once ITS
;; `WatchDaemon' was accepted.
;;
;; NO SLEEPS ANYWHERE.  Every wait in this file is a timer, and every
;; timer's function is a named `defun' a test can call directly.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function agent-repl--log "core" (ws fmt &rest args))
(declare-function agent-repl-persistent-wifi-handle "persistent-wifi" (state))
(declare-function agent-repl--info "core" (ws fmt &rest args))
(declare-function agent-repl--warn "core" (ws fmt &rest args))
(declare-function agent-repl--error "core" (ws fmt &rest args))

(declare-function agent-repl-connect-read-daemon-addr "connect" ())
(declare-function agent-repl-connect-open "connect" (address))
(declare-function agent-repl-connect-close "connect" (conn))
(declare-function agent-repl-connect-connection-address "connect" (conn))
(declare-function agent-repl-connect-connection-alive-p "connect" (conn))
(declare-function agent-repl-connect-failure-message "connect" (detail))
(declare-function agent-repl-rpc-watch-daemon "rpc" (conn on-push on-close &optional on-open))
(declare-function agent-repl-startup-handle "startup" (event))
(declare-function agent-repl-mutation-progress-handle "mutation-progress" (progress))
(declare-function agent-repl-elisp-reload-handle "elisp-build" (reload))

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
  "Functions run with the CONNECTION once a `WatchDaemon' stream is ACCEPTED.
Runs on the first connect and after every reconnect — host.el
re-registers and re-subscribes its workspaces, roster.el re-subscribes.
Keyed on the stream's acceptance (its HTTP 200 header block), never on the
spawn: a connection to a dead address spawns fine and would send the whole
fleet's registrations into a void.  It does NOT run for a handover
promotion: nothing was lost there.")

(defvar agent-repl-link-down-functions nil
  "Functions run with the CONNECTION when the link dies unexpectedly.
A client-side cancel is not a death and never reaches here, nor does the
old daemon's stream closing after a handover (that is a promotion).")

(defvar agent-repl-link-handover-functions nil
  "Functions run with (OLD-CONN NEW-CONN) when a successor daemon is up.
Both connections are live at this point, by design: each workspace keeps
flowing from whichever daemon still owns it until it is transferred.  Run
from the successor's ACCEPTANCE, so NEW-CONN is already proven adoptable
when a hook sees it.")

(defvar agent-repl-link-drain-functions nil
  "Functions run with the current value of `agent-repl-link-drain'.
Nil means the standing schedule was cancelled.")

(defvar agent-repl-link-promote-functions nil
  "Functions run with (OLD-CONN NEW-CONN) when the successor is PROMOTED.
The old daemon dropped its stream after a handover, so NEW-CONN is now the
primary and OLD-CONN is about to be closed.  This is NOT a link-up: every
workspace was already adopted onto the successor and nothing has to be
rebuilt.  What it IS, is the moment every stream that rode the OLD
connection died with it — the roster's above all — so a consumer holding a
connection-scoped stream re-subscribes here and nowhere else.")

(defun agent-repl-link--run-hook (hook &rest args)
  "Run HOOK's consumers with ARGS, CONTAINED: one failure cannot cancel the rest.
These hooks are the seam between independent modules, so a consumer that
signals must not silently take the consumers behind it down with it — that
is how a roster stops reconciling because host.el errored.  Every consumer
runs under its own `condition-case'; an error is recorded at ERROR naming
the hook, the consumer and the datum, and the remaining consumers still
run.  Nothing is swallowed: the error is reported, only its propagation is
stopped at this boundary."
  (dolist (consumer (agent-repl-link--hook-consumers hook))
    (condition-case err
        (apply consumer args)
      (error
       (agent-repl--error '(:agent-repl-central "the resident daemon link spans workspaces")
                          "elisp.link.hook-consumer-failed hook=%S consumer=%S error=%S"
                          hook consumer err)))))

(defun agent-repl-link--hook-consumers (hook)
  "Return HOOK's consumers, resolving the buffer-local `t' marker.
`add-hook' with LOCAL appends `t' to the local value to mean \='and then
the global ones\='; a runner that applied `t' as a function would signal."
  (let ((value (and (boundp hook) (symbol-value hook))))
    (cond
     ((null value) nil)
     ((functionp value) (list value))
     (t (mapcan (lambda (entry)
                  (if (eq entry t)
                      (copy-sequence (default-value hook))
                    (list entry)))
                value)))))

;;;; ---- State ----

(defvar agent-repl-link--primary nil
  "The connection to the daemon currently serving this Emacs, or nil.")

(defvar agent-repl-link--successor nil
  "The connection to a JOINING successor daemon during a handover, or nil.")

(defvar agent-repl-link--primary-stream nil
  "The `WatchDaemon' stream standing on `agent-repl-link--primary'.")

(defvar agent-repl-link--successor-stream nil
  "The `WatchDaemon' stream standing on `agent-repl-link--successor'.")

(defvar agent-repl-link--pending nil
  "A connection whose `WatchDaemon' has not been ACCEPTED yet, or nil.
It becomes `agent-repl-link--primary' from connect.el's ON-OPEN and
nowhere else; until then Emacs holds a transport but no proven link.")

(defvar agent-repl-link--pending-stream nil
  "The unaccepted `WatchDaemon' stream on `agent-repl-link--pending'.")

(defvar agent-repl-link--pending-reconnect-p nil
  "Non-nil when `agent-repl-link--pending' came from the reconnect loop.
Only the log line differs; it is kept so the record still distinguishes a
cold connect from a recovery.")

(defvar agent-repl-link--pending-successor nil
  "A successor connection whose `WatchDaemon' is not ACCEPTED yet, or nil.
`agent-repl-link-successor' does NOT answer it: host.el must never adopt
onto a daemon that has not proven it is listening.")

(defvar agent-repl-link--pending-successor-stream nil
  "The unaccepted `WatchDaemon' stream on `agent-repl-link--pending-successor'.")

(defvar agent-repl-link-drain nil
  "The standing drain schedule, or nil when none is armed.
Shape: `(:at-ms N :reason (:arm KEYWORD :value VALUE))', decoded verbatim
from the `drain_scheduled' push.")

(defvar agent-repl-link--surfaced-faults (make-hash-table :test #'equal)
  "The ids of the daemon's loud faults this Emacs has already surfaced.
EACH FAULT ID IS SURFACED ONCE, however often it is re-told: the daemon
replays its standing set to every resubscribing stream (a reconnect, a
handover's successor, a restart of the link), and a failed deploy is one
line in the minibuffer, not one per stream.  It is deliberately NOT reset
by `agent-repl-link-teardown' or by a reload of this file: a fault id is
minted once and never reused, so remembering it across the link's
lifetimes costs nothing and forgetting it would echo it again.")

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
  "The decoded `DaemonShutdownCause' of the plain bounce being waited out.
Nil when no bounce stands.  THE WHOLE CAUSE, not just its arm keyword:
`scheduled_drain' and `immediate' each carry a `DrainReason', and the
indicator names THAT reason rather than a fixed phrase for the arm — the
arm alone would tell the user a drain is happening but never why.")

(defvar agent-repl-link--ending-conn nil
  "The connection whose `WatchDaemon' stream carried the planned ending, or nil.
The daemon on it is standing down in a PLANNED exit and the frame was
the stream\='s last (`DaemonStreamEnding'), so the clean end that follows
is recorded at INFO rather than as a link that went down unannounced.")

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

(defun agent-repl-link-successor-pending-p ()
  "Return non-nil while a successor dial stands but is NOT accepted yet.

`agent-repl-link-successor' answers nil throughout that window, and the
two nils mean opposite things: a PENDING successor is a daemon whose
address the announcement already named and whose `WatchDaemon' is on its
way up, while no pending successor at all is the contract breach the
announcement order forbids.  host.el needs the distinction because the
outgoing daemon pushes `transferred' as soon as a workspace falls free —
which is routinely BEFORE this Emacs has read the announcement, let alone
been accepted by the successor — so a transfer that lands in this window
has to WAIT for the acceptance rather than be reported as a breach."
  (and agent-repl-link--pending-successor t))

(defun agent-repl-link-up-p ()
  "Return non-nil when a live primary connection stands."
  (and agent-repl-link--primary
       (agent-repl-connect-connection-alive-p agent-repl-link--primary)
       t))

(defun agent-repl-link-live ()
  "Return the connection to the LIVE daemon, or nil when no link stands.
THE ONE ANSWER TO \"which daemon serves Emacs now\".  The primary is
resolved from `daemon.addr' (`agent-repl-link-connect', the reconnect
loop) or promoted from a successor the old daemon announced, and it is
the primary only once its `WatchDaemon' was ACCEPTED -- so a per-workspace
stream or call that lost its own daemon follows THIS, never an address it
remembers.  Nil while the link is down: the caller waits on
`agent-repl-link-up-functions' rather than polling.  A primary whose
connection is closed is never answered."
  (let ((primary agent-repl-link--primary))
    (and primary (agent-repl-connect-connection-alive-p primary) primary)))

(defun agent-repl-link--now-ms ()
  "Return the current instant as epoch milliseconds.
The wire carries instants, never durations-since-now, so every comparison
in this file is against this one clock reading."
  (truncate (* 1000 (float-time))))

;;;; ---- Connecting ----

(defun agent-repl-link--open (address on-open)
  "Open a connection to ADDRESS and stand a `WatchDaemon' stream on it.
Returns `(CONN . STREAM)' as soon as the TRANSPORT exists, or nil when
either step fails — a failure here is an ordinary absent-daemon outcome
(the address file can name a daemon that has already exited), so it is
WARNED and answered with nil rather than signalled.

A RETURN IS NOT AN ACCEPTANCE.  Spawning a curl at a dead address
succeeds; only the daemon's HTTP 200 header block proves it is listening.
ON-OPEN is called with no arguments from that instant and from nowhere
else, so every caller here keys its link-stands work on it."
  (condition-case err
      (let ((conn (agent-repl-connect-open address)))
        (condition-case stream-err
            (let ((stream (agent-repl-rpc-watch-daemon
                           conn
                           (lambda (push) (agent-repl-link--handle-push conn push))
                           (lambda (outcome) (agent-repl-link--handle-close conn outcome))
                           on-open)))
              (agent-repl--info '(:agent-repl-central "the resident daemon link spans workspaces") "elisp.link.open address=%S" address)
              (cons conn stream))
          (error
           (agent-repl--warn '(:agent-repl-central "the resident daemon link spans workspaces") "elisp.link.watch-daemon-failed address=%S error=%S"
                             address stream-err)
           (agent-repl-connect-close conn)
           nil)))
    (error
     (agent-repl--warn '(:agent-repl-central "the resident daemon link spans workspaces") "elisp.link.open-failed address=%S error=%S" address err)
     nil)))

(defun agent-repl-link-connect ()
  "Discover the daemon and stand the link, or run the no-daemon hooks.
Returns the primary connection, or nil.  An ABSENT `daemon.addr' is the
legal no-daemon state and runs `agent-repl-link-no-daemon-functions' —
the cold start's entry point — rather than failing."
  (cond
   ((agent-repl-link-up-p)
    (agent-repl--log '(:agent-repl-central "the resident daemon link spans workspaces") "elisp.link.connect-noop address=%S"
                     (agent-repl-connect-connection-address agent-repl-link--primary))
    agent-repl-link--primary)
   (agent-repl-link--pending
    (agent-repl--log '(:agent-repl-central "the resident daemon link spans workspaces") "elisp.link.connect-pending address=%S"
                     (agent-repl-connect-connection-address agent-repl-link--pending))
    agent-repl-link--pending)
   (t
    (let ((address (agent-repl-connect-read-daemon-addr)))
      (if (null address)
          (progn
            (agent-repl--info '(:agent-repl-central "the resident daemon link spans workspaces") "elisp.link.no-daemon")
            (agent-repl-link--run-hook 'agent-repl-link-no-daemon-functions)
            nil)
        (agent-repl-link--open-primary address nil))))))

(defun agent-repl-link--open-primary (address reconnect-p)
  "Stand a PENDING primary link on ADDRESS and return its connection, or nil.
RECONNECT-P records that the reconnect loop drove this, for the log line
acceptance later writes.  Nothing is marked up here: `agent-repl-link-up-p'
stays nil and no up hook runs until `agent-repl-link--accept-primary'."
  (let ((opened (agent-repl-link--open
                 address
                 (lambda () (agent-repl-link--accept-primary address)))))
    (if (null opened)
        (progn
          (agent-repl--warn '(:agent-repl-central "the resident daemon link spans workspaces") "elisp.link.connect-failed address=%S" address)
          nil)
      (setq agent-repl-link--pending (car opened)
            agent-repl-link--pending-stream (cdr opened)
            agent-repl-link--pending-reconnect-p reconnect-p)
      (agent-repl--info '(:agent-repl-central "the resident daemon link spans workspaces") "elisp.link.open-pending address=%S reconnect=%S"
                        address (and reconnect-p t))
      agent-repl-link--pending)))

(defun agent-repl-link--accept-primary (address)
  "Promote the pending connection to primary: the daemon ACCEPTED the watch.
The ONE place the link becomes up.  Everything a standing link owes its
consumers happens here and nowhere else: the reconnect loop is disarmed,
any bounce quiet window is dropped, the indicator is redrawn, and
`agent-repl-link-up-functions' run — so no consumer ever registers a
workspace against a connection the daemon never answered."
  (if (null agent-repl-link--pending)
      (agent-repl--warn '(:agent-repl-central "the resident daemon link spans workspaces") "elisp.link.accept-without-pending address=%S" address)
    (setq agent-repl-link--primary agent-repl-link--pending
          agent-repl-link--primary-stream agent-repl-link--pending-stream)
    (let ((reconnect-p agent-repl-link--pending-reconnect-p))
      (setq agent-repl-link--pending nil
            agent-repl-link--pending-stream nil
            agent-repl-link--pending-reconnect-p nil)
      (agent-repl-link--cancel-reconnect)
      (setq agent-repl-link--quiet-until-ms nil
            agent-repl-link--bounce-cause nil)
      (agent-repl-link--refresh-indicator)
      (if reconnect-p
          (agent-repl--info '(:agent-repl-central "the resident daemon link spans workspaces") "elisp.link.reconnected address=%S" address)
        (agent-repl--info '(:agent-repl-central "the resident daemon link spans workspaces") "elisp.link.up address=%S" address))
      (agent-repl-link--run-hook 'agent-repl-link-up-functions agent-repl-link--primary))))

(defun agent-repl-link-teardown ()
  "Close every connection this link holds and forget all of its state.
The client-side close is the graceful one: each standing stream's
ON-CLOSE runs with `(:cancelled)', which this file treats as normal."
  (agent-repl--info '(:agent-repl-central "the resident daemon link spans workspaces") "elisp.link.teardown primary=%s successor=%s"
                    (if agent-repl-link--primary "t" "nil")
                    (if agent-repl-link--successor "t" "nil"))
  (agent-repl-link--cancel-reconnect)
  (let ((conns (delq nil (list agent-repl-link--primary
                               agent-repl-link--pending
                               agent-repl-link--successor
                               agent-repl-link--pending-successor))))
    (setq agent-repl-link--primary nil
          agent-repl-link--primary-stream nil
          agent-repl-link--pending nil
          agent-repl-link--pending-stream nil
          agent-repl-link--pending-reconnect-p nil
          agent-repl-link--successor nil
          agent-repl-link--successor-stream nil
          agent-repl-link--pending-successor nil
          agent-repl-link--pending-successor-stream nil
          agent-repl-link--quiet-until-ms nil
          agent-repl-link--bounce-cause nil
          agent-repl-link--ending-conn nil
          agent-repl-link-drain nil)
    (dolist (conn conns) (agent-repl-connect-close conn)))
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
    (agent-repl--log '(:agent-repl-central "the resident daemon link spans workspaces") "elisp.link.reconnect-scheduled interval=%.3f quiet-until=%S"
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
    (agent-repl--log '(:agent-repl-central "the resident daemon link spans workspaces") "elisp.link.reconnect-already-up")
    (agent-repl-link--cancel-reconnect))
   (agent-repl-link--pending
    ;; A transport is already standing and waiting to be accepted; a second
    ;; one would race it and leave an orphan.
    (agent-repl--log '(:agent-repl-central "the resident daemon link spans workspaces") "elisp.link.reconnect-pending")
    (agent-repl-link--schedule-reconnect))
   ((agent-repl-link--quiet-p)
    (agent-repl--log '(:agent-repl-central "the resident daemon link spans workspaces") "elisp.link.reconnect-quiet until=%S now=%S"
                     agent-repl-link--quiet-until-ms (agent-repl-link--now-ms))
    (agent-repl-link--schedule-reconnect))
   (t
    (let ((address (condition-case err
                       (agent-repl-connect-read-daemon-addr)
                     (error
                      (agent-repl--warn '(:agent-repl-central "the resident daemon link spans workspaces") "elisp.link.reconnect-addr-unreadable error=%S" err)
                      nil))))
      (if (null address)
          ;; NO DAEMON AT ALL, after the link was up: the one that served it
          ;; stood down with no successor (a backend bounce stops it and
          ;; starts nothing).  Emacs owns bringing one up, so the cold
          ;; start's hook runs here exactly as at first connect -- polling
          ;; alone waited forever for a daemon nobody would start.  The poll
          ;; stays armed: the ensure links on its own, and the next tick
          ;; finds the link up and stops.
          (progn
            (agent-repl--info '(:agent-repl-central "the resident daemon link spans workspaces") "elisp.link.reconnect-no-daemon")
            (agent-repl-link--run-hook 'agent-repl-link-no-daemon-functions)
            (agent-repl-link--schedule-reconnect))
        (if (agent-repl-link--open-primary address t)
            (agent-repl--log '(:agent-repl-central "the resident daemon link spans workspaces") "elisp.link.reconnect-awaiting-acceptance address=%S"
                             address)
          (agent-repl--log '(:agent-repl-central "the resident daemon link spans workspaces") "elisp.link.reconnect-refused address=%S" address)
          (agent-repl-link--schedule-reconnect)))))))

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
    (when (eq agent-repl-link--ending-conn old)
      (setq agent-repl-link--ending-conn nil))
    (agent-repl--info '(:agent-repl-central "the resident daemon link spans workspaces") "elisp.link.successor-promoted address=%S"
                      (agent-repl-connect-connection-address agent-repl-link--primary))
    ;; BEFORE the close, and BEFORE any consumer sees the old connection
    ;; die: a promotion kills every stream that rode OLD, so the consumers
    ;; holding connection-scoped streams re-subscribe from here.  This is
    ;; not a link-up and does not pretend to be one -- nothing is
    ;; re-registered, only re-subscribed.
    (agent-repl-link--run-hook 'agent-repl-link-promote-functions
                               old agent-repl-link--primary)
    (when old (agent-repl-connect-close old))))

(defun agent-repl-link--handle-close (conn outcome)
  "React to the `WatchDaemon' stream on CONN closing with OUTCOME.
OUTCOME is connect.el's vocabulary: `(:cancelled)' is the client's own
graceful close and means nothing here; `(:ended)' is a producer-side end
of a STANDING stream, which the contract calls a transport failure unless
the stream\='s last frame was the planned ending
\(`agent-repl-link--ending-conn'), when the link goes down at INFO; and
`(:error DETAIL)' is one already."
  (let ((kind (car outcome)))
    (cond
     ((eq conn agent-repl-link--pending)
      ;; A stream that died before it was ACCEPTED was never up, so there is
      ;; nothing to take down: no down hooks, straight back to the poll.
      (setq agent-repl-link--pending nil
            agent-repl-link--pending-stream nil
            agent-repl-link--pending-reconnect-p nil)
      (if (eq kind :cancelled)
          (agent-repl--log '(:agent-repl-central "the resident daemon link spans workspaces") "elisp.link.pending-stream-cancelled")
        (agent-repl--warn '(:agent-repl-central "the resident daemon link spans workspaces") "elisp.link.open-refused address=%S outcome=%S"
                          (agent-repl-connect-connection-address conn) outcome)
        (agent-repl-connect-close conn)
        (agent-repl-link--schedule-reconnect)))
     ((eq conn agent-repl-link--pending-successor)
      ;; The successor died before proving it was listening.  Nothing was
      ;; adopted onto it, by construction, so the old daemon still serves
      ;; everything and the only loss is the handover itself.
      (setq agent-repl-link--pending-successor nil
            agent-repl-link--pending-successor-stream nil)
      (if (eq kind :cancelled)
          (agent-repl--log '(:agent-repl-central "the resident daemon link spans workspaces") "elisp.link.pending-successor-cancelled")
        (agent-repl--error '(:agent-repl-central "the resident daemon link spans workspaces") "elisp.link.successor-open-refused address=%S outcome=%S"
                           (agent-repl-connect-connection-address conn) outcome)
        (agent-repl-connect-close conn)))
     ((eq conn agent-repl-link--successor)
      ;; The successor died before it took over.  The old daemon still
      ;; owns whatever it has not transferred, so the link is not down —
      ;; but the handover cannot complete, and host.el must not adopt onto
      ;; a corpse.
      (if (eq kind :cancelled)
          (agent-repl--log '(:agent-repl-central "the resident daemon link spans workspaces") "elisp.link.successor-stream-cancelled")
        (agent-repl--error '(:agent-repl-central "the resident daemon link spans workspaces") "elisp.link.successor-stream-lost outcome=%S" outcome)
        (let ((successor agent-repl-link--successor))
          (setq agent-repl-link--successor nil
                agent-repl-link--successor-stream nil)
          (agent-repl-connect-close successor))))
     ((not (eq conn agent-repl-link--primary))
      (agent-repl--log '(:agent-repl-central "the resident daemon link spans workspaces") "elisp.link.stale-stream-close outcome=%S" outcome))
     ((eq kind :cancelled)
      (agent-repl--log '(:agent-repl-central "the resident daemon link spans workspaces") "elisp.link.primary-stream-cancelled"))
     (agent-repl-link--successor
      ;; The old daemon finished and dropped its stream after a handover.
      (agent-repl--info '(:agent-repl-central "the resident daemon link spans workspaces") "elisp.link.handover-complete outcome=%S" outcome)
      (agent-repl-link--promote-successor))
     ((and (eq kind :ended) (eq conn agent-repl-link--ending-conn))
      ;; THE DAEMON SAID SO FIRST: a planned stand-down with no successor to
      ;; promote.  The same walk as an unannounced loss -- down hooks, then
      ;; the reconnect loop that resolves the live daemon -- recorded at INFO.
      (agent-repl--info '(:agent-repl-central "the resident daemon link spans workspaces") "elisp.link.down-planned address=%S"
                        (agent-repl-connect-connection-address conn))
      (agent-repl-link--primary-down conn))
     (t
      (agent-repl--warn '(:agent-repl-central "the resident daemon link spans workspaces") "elisp.link.down outcome=%S address=%S" outcome
                        (agent-repl-connect-connection-address conn))
      (agent-repl-link--primary-down conn)))))

(defun agent-repl-link--primary-down (conn)
  "Take the link down after the primary CONN\='s stream ended.
The down hooks run, CONN is closed, and the reconnect loop resolves the
live daemon afresh.  The caller has already recorded why."
  (setq agent-repl-link--primary nil
        agent-repl-link--primary-stream nil
        agent-repl-link--ending-conn nil)
  (agent-repl-link--run-hook 'agent-repl-link-down-functions conn)
  (agent-repl-connect-close conn)
  (agent-repl-link--schedule-reconnect))

;;;; ---- Pushes ----

(defun agent-repl-link--handle-push (conn push)
  "Dispatch one decoded `WatchDaemon' PUSH received on CONN."
  (let ((arm (plist-get push :arm))
        (value (plist-get push :value)))
    (pcase arm
      (:shutdown-announced (agent-repl-link--shutdown-announced conn value))
      (:drain-scheduled (agent-repl-link--drain-scheduled value))
      (:drain-cancelled (agent-repl-link--drain-cancelled))
      ;; Workspace-mutation progress rides this daemon-level stream because it
      ;; PRECEDES the workspace's existence; the correlation seat matches it to
      ;; the command that minted its op id.
      (:mutation-progress (agent-repl-mutation-progress-handle value))
      ;; A deploy found this Emacs on older elisp.  elisp-build.el checks the
      ;; root and schedules the load OUT of this process filter.
      (:reload-elisp (agent-repl-elisp-reload-handle value))
      ;; The daemon's standing loud faults (a failed deploy, whoever started
      ;; it): each one not yet surfaced is echoed and recorded at ERROR.
      (:faults-standing (agent-repl-link--faults-standing value))
      ;; The machine's persistent-wifi standing: persistent-wifi.el keeps it
      ;; and echoes a change.
      (:persistent-wifi (agent-repl-persistent-wifi-handle value))
      ;; One step of this Emacs's startup, or a workspace's go-ahead to open
      ;; its tab (startup.el).  An EMACS stream's alone, never replayed.
      (:startup (agent-repl-startup-handle value))
      ;; The daemon is standing down on purpose and this was the stream's
      ;; last frame; `agent-repl-link--handle-close' reads the mark.
      (:ending
       (setq agent-repl-link--ending-conn conn)
       (agent-repl--info '(:agent-repl-central "the resident daemon link spans workspaces")
                         "elisp.link.stream-ending address=%S"
                         (agent-repl-connect-connection-address conn)))
      (_ (agent-repl--error '(:agent-repl-central "the resident daemon link spans workspaces") "elisp.link.unknown-daemon-push arm=%S push=%S"
                            arm push)))))

(defun agent-repl-link--faults-standing (standing)
  "Surface every fault in STANDING this Emacs has not surfaced before.
STANDING is a decoded `DaemonFaultsStanding': the daemon's WHOLE set of
standing loud faults, the ones every client surfaces (a failed deploy and
a failed deploy's rollback, whoever started the deploy -- this Emacs, the
CLI or a landing).  Each new fault is an ERROR record carrying its id, its
line and its typed fault, and the new faults' lines are echoed together
in ONE minibuffer line, the daemon's own words after `agent-repl: '.  A
fault already surfaced is recorded at debug and never echoed again, which
is what makes a resubscribe's replay silent."
  (let ((central '(:agent-repl-central "the resident daemon link spans workspaces"))
        (faults (plist-get standing :faults))
        fresh)
    (dolist (fault faults)
      (let ((id (plist-get fault :fault-id)))
        (if (gethash id agent-repl-link--surfaced-faults)
            (agent-repl--log central "elisp.link.fault-already-surfaced fault-id=%s" id)
          (puthash id t agent-repl-link--surfaced-faults)
          (push fault fresh))))
    (setq fresh (nreverse fresh))
    (dolist (fault fresh)
      (agent-repl--error central
                         "elisp.link.daemon-fault fault-id=%s line=%S opened-at-ms=%S fault=%S"
                         (plist-get fault :fault-id) (plist-get fault :line)
                         (plist-get fault :opened-at-ms) (plist-get fault :fault)))
    (when fresh
      (message "agent-repl: %s"
               (mapconcat (lambda (fault) (plist-get fault :line)) fresh "; ")))
    (agent-repl--log central "elisp.link.faults-standing standing=%d surfaced=%d"
                     (length faults) (length fresh))))

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
            agent-repl-link--bounce-cause cause)
      (agent-repl--info '(:agent-repl-central "the resident daemon link spans workspaces")
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
  (let ((attached (or agent-repl-link--successor agent-repl-link--pending-successor)))
    (cond
     ((and attached
           (equal address (agent-repl-connect-connection-address attached)))
      (agent-repl--log '(:agent-repl-central "the resident daemon link spans workspaces") "elisp.link.successor-already-attached address=%S" address))
     (attached
      (agent-repl--error '(:agent-repl-central "the resident daemon link spans workspaces") "elisp.link.successor-address-changed old=%S new=%S"
                         (agent-repl-connect-connection-address attached)
                         address))
     (t
      (let ((opened (agent-repl-link--open
                     address
                     (lambda () (agent-repl-link--accept-successor old address cause)))))
        (if (null opened)
            (agent-repl--error '(:agent-repl-central "the resident daemon link spans workspaces") "elisp.link.successor-attach-failed address=%S cause=%S"
                               address (plist-get cause :arm))
          (setq agent-repl-link--pending-successor (car opened)
                agent-repl-link--pending-successor-stream (cdr opened))
          (agent-repl--info '(:agent-repl-central "the resident daemon link spans workspaces") "elisp.link.successor-pending address=%S cause=%S"
                            address (plist-get cause :arm))))))))

(defun agent-repl-link-dial-successor (address)
  "Dial the successor daemon at ADDRESS outside a shutdown announcement.
A PER-WORKSPACE rpc can refuse with `transferring_away{address}' before
this Emacs has seen the daemon-scoped announcement at all — the refusal
is then the first news of the handover, and the address it carries is the
same fact the announcement would have carried.  Idempotent by address,
exactly like the announced path: an already attached or already pending
successor at ADDRESS is a no-op.  Returns `agent-repl-link-successor',
which is still nil while the dial awaits acceptance."
  (agent-repl--info '(:agent-repl-central "the resident daemon link spans workspaces") "elisp.link.dial-successor address=%S" address)
  (agent-repl-link--attach-successor (agent-repl-link-primary) address nil)
  (agent-repl-link-successor))

(defun agent-repl-link--accept-successor (old address cause)
  "Record the successor at ADDRESS as ADOPTABLE: it ACCEPTED its watch.
Announced on OLD with CAUSE.  Until this runs `agent-repl-link-successor'
answers nil, so host.el cannot send an adopt to a daemon that has not
proven it is listening; from here the handover hooks run with both live
connections."
  (if (null agent-repl-link--pending-successor)
      (agent-repl--warn '(:agent-repl-central "the resident daemon link spans workspaces") "elisp.link.successor-accept-without-pending address=%S"
                        address)
    (setq agent-repl-link--successor agent-repl-link--pending-successor
          agent-repl-link--successor-stream agent-repl-link--pending-successor-stream
          agent-repl-link--pending-successor nil
          agent-repl-link--pending-successor-stream nil)
    (agent-repl--info '(:agent-repl-central "the resident daemon link spans workspaces") "elisp.link.successor-accepted address=%S cause=%S"
                      address (plist-get cause :arm))
    (if (null agent-repl-link--primary)
        ;; The old daemon dropped its stream before the successor was
        ;; accepted, so there is nothing to hand over FROM: the successor is
        ;; simply the link now, and the fleet has to be rebuilt on it.
        (progn
          (agent-repl--warn '(:agent-repl-central "the resident daemon link spans workspaces") "elisp.link.successor-accepted-without-primary address=%S"
                            address)
          (setq agent-repl-link--primary agent-repl-link--successor
                agent-repl-link--primary-stream agent-repl-link--successor-stream
                agent-repl-link--successor nil
                agent-repl-link--successor-stream nil)
          (agent-repl-link--cancel-reconnect)
          (agent-repl-link--refresh-indicator)
          (agent-repl--info '(:agent-repl-central "the resident daemon link spans workspaces") "elisp.link.up address=%S" address)
          (agent-repl-link--run-hook 'agent-repl-link-up-functions agent-repl-link--primary))
      (agent-repl--info '(:agent-repl-central "the resident daemon link spans workspaces") "elisp.link.handover-announced address=%S cause=%S"
                        address (plist-get cause :arm))
      (agent-repl-link--run-hook 'agent-repl-link-handover-functions
                                 old agent-repl-link--successor))))

(defun agent-repl-link--drain-scheduled (schedule)
  "Record the standing drain SCHEDULE and redraw the indicator."
  (setq agent-repl-link-drain schedule)
  (agent-repl--info '(:agent-repl-central "the resident daemon link spans workspaces") "elisp.link.drain-scheduled at-ms=%S reason=%S"
                    (plist-get schedule :at-ms)
                    (plist-get (plist-get schedule :reason) :arm))
  (agent-repl-link--refresh-indicator)
  (agent-repl-link--run-hook 'agent-repl-link-drain-functions agent-repl-link-drain))

(defun agent-repl-link--drain-cancelled ()
  "Drop the standing drain schedule and take the indicator down."
  (setq agent-repl-link-drain nil)
  (agent-repl--info '(:agent-repl-central "the resident daemon link spans workspaces") "elisp.link.drain-cancelled")
  (agent-repl-link--refresh-indicator)
  (agent-repl-link--run-hook 'agent-repl-link-drain-functions nil))

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
           (agent-repl--error '(:agent-repl-central "the resident daemon link spans workspaces") "elisp.link.drain-operator-note-blank reason=%S" reason)
           "operator")))
      (_
       (agent-repl--error '(:agent-repl-central "the resident daemon link spans workspaces") "elisp.link.drain-reason-unknown arm=%S" arm)
       "unknown"))))

(defun agent-repl-link--cause-text (cause)
  "Return the human phrase for the decoded `DaemonShutdownCause' CAUSE.
THE ARM IS THE CAUSE, and two of the three arms carry a further arm the
user is owed: `scheduled_drain{reason}' and `immediate{reason}' each name
a `DrainReason', so the phrase names the REASON (deploy, maintenance, the
operator\='s own note) beside the arm instead of a fixed word that would
leave every drain reading alike.  `self_merge_rollout' carries nothing —
the arm is the whole fact there."
  (let ((arm (plist-get cause :arm))
        (value (plist-get cause :value)))
    (pcase arm
      (:self-merge-rollout "self-merge rollout")
      (:scheduled-drain
       (format "scheduled drain: %s"
               (agent-repl-link--reason-text (plist-get value :reason))))
      (:immediate
       (format "immediate: %s"
               (agent-repl-link--reason-text (plist-get value :reason))))
      (_
       (agent-repl--error '(:agent-repl-central "the resident daemon link spans workspaces") "elisp.link.shutdown-cause-unknown arm=%S" arm)
       "unknown"))))

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
  (agent-repl--log '(:agent-repl-central "the resident daemon link spans workspaces") "elisp.link.indicator segment=%S" agent-repl-link-drain-segment)
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
        (agent-repl--log '(:agent-repl-central "the resident daemon link spans workspaces") "elisp.link.indicator-already-installed")
      (setq global-mode-string
            (append current '(agent-repl-link-drain-segment)))
      (agent-repl--log '(:agent-repl-central "the resident daemon link spans workspaces") "elisp.link.indicator-installed"))))

(agent-repl-link-install-indicator)

(provide 'daemon-link)

;;; daemon-link.el ends here
