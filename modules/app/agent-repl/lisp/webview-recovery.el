;;; webview-recovery.el --- paced webview pre-creation -*- lexical-binding: t; -*-

;;; Commentary:

;; WEBVIEW PRE-CREATION, STAGGERED.  Mounting a WKWebView and loading the
;; webapp is the slow part of opening a workspace; doing it at first look
;; puts that cost in front of the user every time.  So every eligible
;; workspace's page is built ahead of the look, and the mounts are paced one
;; per tick so a link-up owing ten of them does not spawn ten WebKit content
;; processes in one command-loop turn.  Each webview is then BOUND TO ITS
;; WORKSPACE BUFFER FOR LIFE.
;;
;; THE STALE-WEBVIEW SWEEP IS GONE, and so is every script it drove.  A
;; rebuilt webapp reaches a live page through the daemon's own
;; `reload_webapp' push (frontend.el navigates the widget), and a daemon
;; handover re-attaches the page as a side effect — neither needs Emacs to
;; go looking for pages to repair, and neither needs a JavaScript hook.
;; There is no `window.agentRepl*' surface at all: the webview is purely
;; daemon-driven.

;;; Code:


(require 'cl-lib)
(require 'url-util)

(declare-function agent-repl--panels-on-view-created "panels" (ws))
(declare-function agent-repl--ws-current-name "workspace" ())
(declare-function agent-repl--with-deferred-quit "agent-repl-core")
(declare-function agent-repl--deferred-quit-arm-audit "agent-repl-core" (context))
(declare-function agent-repl--deferred-quit-hand-off "agent-repl-core" (context))
(declare-function agent-repl--log"agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--log-verbose "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--warn "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--live-ws-names "agent-repl-workspace" ())
(declare-function agent-repl--ws-get "agent-repl-workspace" (ws key))
(declare-function agent-repl--frontend-precreate-webview "agent-repl-frontend" (ws))
(declare-function agent-repl--frontend-precreate-refusal "agent-repl-frontend" (ws))
(declare-function agent-repl--ws-live-p "agent-repl-workspace" (ws))
(declare-function agent-repl--ws-gui-frontend-p "agent-repl-frontends" (ws))
(declare-function agent-repl--info "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--emacs-focused-p "agent-repl-notifications" (&optional ws))

(defvar agent-repl-link-up-functions)
(defvar agent-repl-roster-update-functions)


(defcustom agent-repl-webview-precreate-stagger-seconds 0.02
  "Seconds between two paced webview pre-creations.
Each pre-creation spawns a WebKit content process, and a startup sweep
can be owed ten of them at once.  The timer chain exists so eligibility
is re-checked between mounts and the command loop is handed back after
each one; the interval is deliberately near-zero because a full burst is
brief (each mount itself is milliseconds -- the heavy work happens in
WebKit's own processes) and a fast rollout is preferred over a stretched
one.  Raise this if a burst ever produces a visible hitch."
  :type 'number
  :group 'agent-repl)

(defvar agent-repl--webview-precreate-queue nil
  "Workspaces still owed a paced pre-creation, in creation order.")

(defvar agent-repl--webview-precreate-timer nil
  "The timer draining `agent-repl--webview-precreate-queue', or nil.")

(defvar agent-repl--webview-precreate-parked nil
  "Non-nil while the drain is HOLDING for Emacs to have desktop focus.
The queue is kept whole; nothing is dropped and no timer stands.")

(defvar agent-repl--webview-precreate-pass-open nil
  "Non-nil while a DRAIN PASS admitted by a clear hold is running.

THE HOLD ADMITS A PASS, NOT AN ITEM.  A pass opens on the tick that finds
the hold clear and closes on the tick that empties the queue; while it is
open the hold is not consulted again, so the whole queue that pass can
reach is drained without re-asking.

Why the hold is not re-asked per item: mounting is what perturbs the very
state the hold reads.  Instantiating a WKWebView hands key-window status
around, and an Emacs that was focused when the pass opened can read back
as visible-but-unfocused microseconds later -- so a per-item hold parks
the remainder after mounting exactly one page and waits for a fresh focus
edge to mount the next.  That is the twenty-second, two-focus-edge,
one-panel-per-edge cold start realtest 5 caught: the resume worked, it
just bought one item at a time.

Why relaxing it is safe: the hold exists so a mount cannot RAISE an Emacs
the user is not looking at.  A pass only opens when creating a view could
not do that -- Emacs hidden, or Emacs already frontmost -- and it spans
only the queue in hand at 20ms a mount, so the window in which a user
could tab away mid-pass is tens of milliseconds and its worst outcome is
returning them to the app they were in a moment ago.  The alternative,
measured, is a panel that stays blank until something focuses Emacs
again.  The hold is asked in full again for the NEXT pass.")

(defvar agent-repl--webview-precreate-pass-mounted 0
  "How many pages the open pass has mounted, for its completion record.")

(defun agent-repl--webview-precreate-arm ()
  "Arm the paced drain for the queue's next workspace, if one is owed."
  (when (and agent-repl--webview-precreate-queue
             (null agent-repl--webview-precreate-timer))
    (setq agent-repl--webview-precreate-timer
          (run-at-time agent-repl-webview-precreate-stagger-seconds nil
                       #'agent-repl--webview-precreate-drain)))
  agent-repl--webview-precreate-timer)

(defun agent-repl--emacs-visible-p ()
  "Return nil ONLY when we KNOW Emacs shows nothing on the desktop.

The one question this answers for the park guard is whether creating a
native view could raise a frame the user can see.  It cannot when the app
is HIDDEN: a hidden macOS app (an `open -gj' launch, or a `Cmd-H') has no
on-screen window, and instantiating a WKWebView inside it neither unhides
nor activates it -- hidden is a stronger state than merely unfocused.

`visible-frame-list' is the signal: it is empty exactly when no frame is
mapped on screen, which on ns is the hidden case.  We return nil ONLY on
that positive \"nothing is visible\" answer; anything else -- a mapped
frame, or a state we cannot read (no window system, `display-graphic-p'
nil) -- returns non-nil so the guard treats it as possibly-visible and
errs toward parking, matching the conservative \"suppress when possibly
foregrounding\" stance the focus gate already takes.  Under
`noninteractive' the whole hold short-circuits before this is reached, so
its batch answer here is moot."
  (cond
   ((not (display-graphic-p)) t)        ; cannot tell -> treat as visible
   ((visible-frame-list) t)
   (t nil)))                            ; positively hidden

(defun agent-repl--emacs-can-foreground-p ()
  "Return non-nil when creating a native view could bring Emacs to the front.

That is exactly the VISIBLE-BUT-UNFOCUSED state: a frame is on screen but
Emacs is not the focused app, so instantiating a WKWebView could raise the
frame and steal the desktop from whatever the user is looking at.  When
Emacs is HIDDEN (no visible frame) creating a view cannot foreground it,
and when Emacs is already FOCUSED there is no focus to steal -- both are
safe, so both return nil.  An unreadable visibility state counts as
visible (see `agent-repl--emacs-visible-p'), so it parks rather than
gambles; an `unknown' focus state counts as focused
\(`agent-repl--emacs-focused-p'), so it proceeds rather than parking a
frame that may already be frontmost."
  (and (agent-repl--emacs-visible-p)
       (fboundp 'agent-repl--emacs-focused-p)
       (not (agent-repl--emacs-focused-p))))

(defun agent-repl--webview-precreate-hold-p ()
  "Return non-nil while a pre-creation must not mount a webview yet.

MOUNTING A WEBVIEW CAN TAKE THE DESKTOP, but only from ONE state.
Creating a WKWebView instantiates a native view, and macOS activates the
owning process when doing so would bring a window forward -- a thing
`open -g' cannot suppress, because it is not the app launch doing it.  A
link-up pre-creates every eligible workspace's page seconds into every
cold start, so an Emacs that is on screen but not focused stole the
desktop from whatever the user was looking at.

That raise can happen only when a frame is ALREADY ON SCREEN and Emacs is
NOT the focused app: the visible-but-unfocused state.  A HIDDEN Emacs (an
`open -gj' launch) creating a view stays hidden -- hidden is a stronger
state than merely unfocused, and macOS does not unhide an app to mount a
native view inside it -- and a FOCUSED Emacs has no focus to steal.  So
both of those proceed and paint; only visible-but-unfocused holds.
Parking a hidden launch is exactly the collision realtest 1 caught: focus
was preserved but no panel painted.

Holding costs the warm page nothing it was for: pre-creation exists so
the mount is not paid at first LOOK, and a look is a focus.  The hold is
released by `agent-repl--webview-precreate-on-focus-change' the moment
Emacs is frontmost, which is at or before that first look.

Under `noninteractive' nothing holds -- a batch process has no
application to activate, and the suites drive this drain directly.  The
guard itself is `agent-repl--emacs-can-foreground-p'."
  (and (not noninteractive)
       ;; THE EDITOR'S STARTUP PRE-CREATES EVERY PAGE AT ONCE (owner
       ;; requirement 2026-10-02): a workspace's tab opens only once its page
       ;; has drawn, so a startup that waited for a focus edge would open no
       ;; tab until the user looked at Emacs.  Its tabs stay out of the bar
       ;; while the pages load, so nothing is switched to.
       (not (and (fboundp 'agent-repl-startup-active-p)
                 (agent-repl-startup-active-p)))
       (agent-repl--emacs-can-foreground-p)))

(defun agent-repl--webview-precreate-allow-reason ()
  "Name WHY the drain is allowed to mount right now, for the log line.

Mirrors `agent-repl--webview-precreate-hold-p': the hold is only the
visible-unfocused state, so a mount that actually happens is one of
created-while-hidden, created-while-focused, or (in batch) driven
directly by the suites.  Reported as a `reason=' tag on the created log."
  (cond
   (noninteractive "batch")
   ((and (fboundp 'agent-repl-startup-active-p) (agent-repl-startup-active-p)) "startup")
   ((not (agent-repl--emacs-visible-p)) "hidden")
   ((and (fboundp 'agent-repl--emacs-focused-p)
         (agent-repl--emacs-focused-p))
    "focused")
   (t "allowed")))

(defun agent-repl--webview-precreate-park ()
  "Hold the drain until creating a view can no longer foreground Emacs.
The only held state is visible-but-unfocused; the queue is kept whole,
nothing is dropped and no timer stands.  Reached only between passes: an
open pass does not re-ask the hold (see
`agent-repl--webview-precreate-pass-open')."
  (setq agent-repl--webview-precreate-parked t)
  (agent-repl--info '(:agent-repl-central "webview recovery spans workspaces")
                    "elisp.webview-recovery.precreate-parked queued=%d reason=visible-unfocused"
                    (length agent-repl--webview-precreate-queue))
  t)

(defun agent-repl--webview-precreate-open-pass ()
  "Open a drain pass, naming what admitted it, and clear any park.
Called by the tick that found the hold clear.  From here to the tick that
empties the queue the hold is not asked again."
  (setq agent-repl--webview-precreate-pass-open t
        agent-repl--webview-precreate-pass-mounted 0
        agent-repl--webview-precreate-parked nil)
  (agent-repl--info '(:agent-repl-central "webview recovery spans workspaces")
                    "elisp.webview-recovery.precreate-pass-opened queued=%d reason=%s"
                    (1+ (length agent-repl--webview-precreate-queue))
                    (agent-repl--webview-precreate-allow-reason)))

(defun agent-repl--webview-precreate-close-pass ()
  "Close the open pass once the queue is empty, recording what it mounted.
A no-op when no pass is open, which is every tick that finds an already
empty queue."
  (when agent-repl--webview-precreate-pass-open
    (agent-repl--info '(:agent-repl-central "webview recovery spans workspaces")
                      "elisp.webview-recovery.precreate-pass-completed mounted=%d"
                      agent-repl--webview-precreate-pass-mounted)
    (setq agent-repl--webview-precreate-pass-open nil
          agent-repl--webview-precreate-pass-mounted 0)))

(defun agent-repl--webview-precreate-on-focus-change ()
  "Resume a parked drain once Emacs actually holds desktop focus.
Registered on `after-focus-change-function', the same edge status.el
repaints the tab bar on.  A no-op when nothing is parked."
  (when (and agent-repl--webview-precreate-parked
             (not (agent-repl--webview-precreate-hold-p)))
    (setq agent-repl--webview-precreate-parked nil)
    ;; The tick this arms opens the pass; from there the hold is not
    ;; re-asked, so ONE focus edge drains the whole queue it can reach.
    (agent-repl--info '(:agent-repl-central "webview recovery spans workspaces")
                      "elisp.webview-recovery.precreate-drained-on-focus queued=%d reason=focus-edge"
                      (length agent-repl--webview-precreate-queue))
    (agent-repl--webview-precreate-arm)))

(defun agent-repl--webview-precreate-needed-p (ws)
  "Return non-nil when WS is owed a webview and has none.

Answered by `agent-repl--frontend-precreate-refusal' — the SAME
eligibility the mount itself applies — so the queue can never hold a
workspace the mount would then refuse, nor skip one it would accept.
It is re-asked here as well as at the mount because a workspace can be
closed, killed, merged or fenced during the seconds a paced queue takes
to drain, and a page for it must not appear afterwards."
  (null (agent-repl--frontend-precreate-refusal ws)))

(defun agent-repl--webview-precreate-drain ()
  "Pre-create the queue's next workspace, then re-arm for the one after.

One mount per tick.  ELIGIBILITY is RE-CHECKED at the tick rather than
trusted from when the workspace was queued, so a workspace closed mid
drain gets no page.  A mount that signals is warned about and the drain
continues: one workspace's failure must not strand the rest of the queue.

THE HOLD, unlike eligibility, is asked ONCE PER PASS rather than once per
item.  The first tick with a clear hold opens a pass; every tick after it
drains, and the tick that empties the queue closes it.  Re-asking the
hold per item is what stranded a cold start for twenty seconds, because
the mount itself perturbs the focus state the hold reads -- see
`agent-repl--webview-precreate-pass-open' for the whole argument.  A tick
that finds the hold set with NO pass open still parks, whole, and waits
for `agent-repl--webview-precreate-on-focus-change'.

A TICK IS A CRITICAL SECTION FOR QUITS.  This drain is set off by the
desktop FOCUS EDGE, which is the same instant a keypress arrives -- a key
event can only reach Emacs once Emacs is the active application, so
bringing it forward is what precedes one.  A `C-g' landing here found
Emacs BUSY rather than waiting in `read_char', so Emacs could only arm
`quit-flag'; the flag was then taken at a checkpoint inside this tick,
which sits inside a standing minibuffer's own recursive edit, so the echo
area said `Quit' and the prompt stayed up.  That is why the tick runs
under `agent-repl--with-deferred-quit\': the guard holds the quit off the
tick and LEAVES IT ARMED, so Emacs's own input wait turns it into the
`C-g' event the standing read aborts on.

That guard is also what made the 2026-09-13 realtest 5-8 failures land
HERE and nowhere else: only a run with a pending pre-creation runs a
guarded section on the focus edge, and the delivery timer the guard used
to arm was clearing the flag without honouring it.  See core.el's
commentary.

The mid-tick state this also protects is real on its own terms: the timer
is cleared at the top and re-armed at the bottom, so a quit between them
would leave the queue standing with no timer to drain it."
  (agent-repl--with-deferred-quit "webview-precreate-drain"
    (agent-repl--webview-precreate-tick)))

(defun agent-repl--webview-precreate-tick ()
  "Run one pre-creation tick: the quit-guarded body of the drain.
Named separately so `agent-repl--webview-precreate-drain\' has a body to
guard and so a tick is drivable from a test without a timer."
  (setq agent-repl--webview-precreate-timer nil)
  (if (and agent-repl--webview-precreate-queue
           (not agent-repl--webview-precreate-pass-open)
           (agent-repl--webview-precreate-hold-p))
      (agent-repl--webview-precreate-park)
    (let ((ws (agent-repl--webview-precreate-next)))
      (when ws
        (unless agent-repl--webview-precreate-pass-open
          (agent-repl--webview-precreate-open-pass))
        (condition-case err
            (when (agent-repl--webview-precreate-needed-p ws)
              (agent-repl--info '(:agent-repl-central "webview recovery spans workspaces")
                                "elisp.webview-recovery.precreate-created ws=%s reason=%s"
                                ws (agent-repl--webview-precreate-allow-reason))
              (agent-repl--frontend-precreate-webview ws)
              (setq agent-repl--webview-precreate-pass-mounted
                    (1+ agent-repl--webview-precreate-pass-mounted))
              ;; The workspace the user stands on may have declined its
              ;; panel restore for want of exactly this view.
              (agent-repl--panels-on-view-created ws))
          (error (agent-repl--warn ws "webview-precreate: ws=%s outcome=failed err=%S"
                                   ws err))))
      (unless agent-repl--webview-precreate-queue
        (agent-repl--webview-precreate-close-pass))
      (agent-repl--webview-precreate-arm))))

(defun agent-repl--webview-precreate-next ()
  "Take the next workspace off the pre-creation queue.
THE WORKSPACE THE USER STANDS ON GOES FIRST when it is queued: it is the
one whose panels are waiting on its view (`agent-repl--panels-on-view-created'),
and at startup it is the first thing the user sees.  Otherwise the queue
drains in order."
  (let ((current (agent-repl--ws-current-name)))
    (if (and current (member current agent-repl--webview-precreate-queue))
        (progn
          (setq agent-repl--webview-precreate-queue
                (delete current agent-repl--webview-precreate-queue))
          current)
      (pop agent-repl--webview-precreate-queue))))

(defun agent-repl--webview-precreate-schedule (workspaces)
  "Queue WORKSPACES for paced pre-creation, returning how many were queued.
Appends rather than replaces, so a sweep landing while an earlier queue
is draining cannot drop the workspaces that queue still owes.  A
workspace already queued is not queued twice."
  (let ((added 0))
    (dolist (ws workspaces)
      (unless (member ws agent-repl--webview-precreate-queue)
        (setq agent-repl--webview-precreate-queue
              (append agent-repl--webview-precreate-queue (list ws)))
        (setq added (1+ added))))
    (agent-repl--webview-precreate-arm)
    added))

(defun agent-repl--webview-precreate-missing ()
  "Return every live workspace owed a webview it does not have."
  (cl-remove-if-not #'agent-repl--webview-precreate-needed-p
                    (agent-repl--live-ws-names)))

(defun agent-repl-webview-precreate-all ()
  "Queue every live workspace owed a webview, returning how many were queued.
The whole of what a link-up needs: a workspace whose page is already
mounted is not queued again, and one that becomes ineligible while the
queue drains is skipped at its tick."
  (let ((queued (agent-repl--webview-precreate-schedule
                 (agent-repl--webview-precreate-missing))))
    (agent-repl--info '(:agent-repl-central "webview recovery spans workspaces") "elisp.webview-recovery.precreate-all: queued=%d" queued)
    queued))

(defun agent-repl--webview-precreate-on-link-up (&optional _conn)
  "Pre-create the pages the link-up made buildable.
Registered on `agent-repl-link-up-functions': before the link is up
there is no daemon address to build a URL from, so a workspace's page
cannot be mounted, and this edge is the first moment every open
workspace is buildable."
  (agent-repl--log '(:agent-repl-central "webview recovery spans workspaces") "elisp.webview-recovery.link-up: pre-creating")
  (agent-repl-webview-precreate-all))

(add-hook 'agent-repl-link-up-functions #'agent-repl--webview-precreate-on-link-up)

(defun agent-repl--webview-precreate-on-roster-update (&optional _roster)
  "Pre-create the pages a roster push made buildable.
Registered on `agent-repl-roster-update-functions', which runs after
every accepted push.  A COLD start has no workspaces registered at
link-up -- their `WorkspaceRef's arrive seconds later on the first
WatchWorkspaceRoster push, and until then every workspace refuses
pre-creation with `:no-ref', so the link-up edge queues nothing.  This
edge is the moment those refs exist, so it is the first moment a cold
workspace is buildable.  It shares `agent-repl-webview-precreate-all'
with the link-up edge, whose eligibility test skips a mounted page and
whose queue skips a duplicate, so running on both hooks never double
queues.  The roster argument is unused: the buildable set is read from
live state, not from the push."
  (agent-repl--log '(:agent-repl-central "webview recovery spans workspaces") "elisp.webview-recovery.roster-update: pre-creating")
  (agent-repl-webview-precreate-all))

(add-hook 'agent-repl-roster-update-functions #'agent-repl--webview-precreate-on-roster-update)
(add-function :after after-focus-change-function
              #'agent-repl--webview-precreate-on-focus-change)

(provide 'webview-recovery)
;;; webview-recovery.el ends here
