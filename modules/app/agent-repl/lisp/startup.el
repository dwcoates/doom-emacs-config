;;; startup.el --- the editor's startup: its milestones, and Doom's one-time setup -*- lexical-binding: t; -*-

;;; Commentary:

;; The editor's startup, timed and reported: each milestone from the Emacs
;; process's start to every tab open is recorded once per process and
;; reported to the daemon's metrics (`ReportLatencies', the `editor_start'
;; restart).
;;
;; The editor's first open (open-tabs.el) ends by arming Doom's one-time
;; first-file and first-buffer setup for the next idle stretch, so no tab
;; switch pays for it.

;;; Code:

(require 'cl-lib)

(declare-function agent-repl--info "core" (ws fmt &rest args))
(declare-function agent-repl--log "core" (ws fmt &rest args))
(declare-function agent-repl--error "core" (ws fmt &rest args))
(declare-function doom-run-hooks "doom-lib" (&rest hooks))
(declare-function agent-repl--fatal "core" (ws fmt &rest args))
(declare-function agent-repl-daemon--spawned-here-p "daemon" ())
(declare-function agent-repl-latency-report-startup "latency-report" (milestones))

(defconst agent-repl-startup--central
  '(:agent-repl-central "the editor's startup spans every workspace")
  "The log scope of Doom's one-time setup records.")

;;;; ---- The editor's startup milestones -----------------------------------

;; EACH MILESTONE IS REACHED ONCE PER EMACS PROCESS and timed from the
;; process's start (`before-init-time'), so it is the time since launch the
;; user waited, not the length of a step:
;;
;;   :init           Emacs's init files loaded (`after-init-time')
;;   :daemon-linked  the first WatchDaemon stream accepted
;;   :first-tab      the startup's first workspace tab opened
;;   :all-tabs       the startup's open finished: the editor is ready
;;
;; THE REPORT WAITS FOR THE STARTUP TO END, because every milestone carries
;; how this Emacs came to a daemon (spawned it, or found it running), and
;; the startup's milestones are one batch.  At the end they are handed to
;; latency-report.el, which keeps each until a report of it is acknowledged,
;; so a daemon handover during the startup loses nothing.  A milestone
;; reached after the end is handed over at once.  A hot reload of this file
;; keeps all of it: the variables are `defvar's, and a reload is no new
;; startup.

(defvar agent-repl-startup--milestones nil
  "The startup milestones this process reached, oldest first.
Each is (KEYWORD . SECONDS-SINCE-PROCESS-START).")

(defvar agent-repl-startup--daemon nil
  "How this Emacs came to a daemon: `:spawned' or `:running', or nil before.
Decided at the first link-up.")

(defvar agent-repl-startup--ended nil
  "Non-nil once the startup's open finished; the milestones are reported from then.")

(defun agent-repl-startup--since-process-start (&optional at)
  "Seconds from this Emacs process's start to AT, or to now when AT is nil."
  (float-time (time-subtract (or at (current-time)) before-init-time)))

(defun agent-repl-startup--record (milestone seconds)
  "Record MILESTONE as reached SECONDS after the process start.
After the startup's end it is handed to the report at once."
  (let ((cell (cons milestone seconds)))
    (setq agent-repl-startup--milestones (append agent-repl-startup--milestones (list cell)))
    (agent-repl--info agent-repl-startup--central
                      "elisp.startup.milestone milestone=%s seconds=%.3f" milestone seconds)
    (when agent-repl-startup--ended
      (agent-repl-startup--report (list cell)))))

(defun agent-repl-startup-reach (milestone)
  "Record that the editor's startup reached MILESTONE now, once per process.
The `:init' milestone is read off `after-init-time' the first time any
other is reached, so it is recorded at its own instant and in its order."
  (when (and after-init-time (not (assq :init agent-repl-startup--milestones)))
    (agent-repl-startup--record :init (agent-repl-startup--since-process-start after-init-time)))
  (unless (assq milestone agent-repl-startup--milestones)
    (agent-repl-startup--record milestone (agent-repl-startup--since-process-start))))

(defun agent-repl-startup-on-link-up (&rest _connections)
  "Reach `:daemon-linked' at the first link-up.
The first link-up also decides how this Emacs came to a daemon: a daemon
this process spawned and still runs is `:spawned', any other `:running'."
  (unless agent-repl-startup--daemon
    (setq agent-repl-startup--daemon
          (if (agent-repl-daemon--spawned-here-p) :spawned :running))
    (agent-repl-startup-reach :daemon-linked)))

(defun agent-repl-startup-ended ()
  "The startup's open finished: reach `:all-tabs' and report the startup."
  (agent-repl-startup-reach :all-tabs)
  (setq agent-repl-startup--ended t)
  (agent-repl-startup--report agent-repl-startup--milestones))

(defun agent-repl-startup--report (milestones)
  "Hand MILESTONES, a list of (KEYWORD . SECONDS), to the report.
Each carries how this Emacs came to a daemon.  A startup that ended
before any link-up named no daemon, which the open's order makes
impossible: it is an invariant violation."
  (unless agent-repl-startup--daemon
    (agent-repl--fatal agent-repl-startup--central
                       "elisp.startup.report no-daemon milestones=%S" milestones))
  (agent-repl--info agent-repl-startup--central
                    "elisp.startup.report queued milestones=%d daemon=%s"
                    (length milestones) agent-repl-startup--daemon)
  (agent-repl-latency-report-startup
   (mapcar (lambda (cell)
             (list :milestone (car cell) :daemon agent-repl-startup--daemon :seconds (cdr cell)))
           milestones)))

(add-hook 'agent-repl-link-up-functions #'agent-repl-startup-on-link-up)

;;;; ---- Doom's one-time first-file and first-buffer setup ------------------

;; DOOM'S ONE-TIME SETUP RUNS WHILE EMACS SITS IDLE AFTER THE STARTUP, so no
;; tab switch pays for it (owner ruling 2026-10-06).  Doom chains
;; `doom-first-buffer-hook' to the first buffer switch it counts and
;; `doom-first-file-hook' to the first file visit, runs each once and sets it
;; to nil (doom.el `doom-run-hook-on').  The editor's first open (open-tabs.el) never makes a
;; switch Doom counts, so the user's first tab switch ran ~450 ms of global
;; modes (flycheck, its popup tip, ...) inside its redisplay.  Both hooks run
;; here, file first, in the order Doom's own
;; `doom-run-first-hooks-if-files-open-h' runs them when Emacs starts with
;; files open.  A switch or visit that comes first runs its hook through
;; Doom's own path, and finds nil here.

(defvar agent-repl-startup--first-hooks '(doom-first-file-hook doom-first-buffer-hook)
  "Doom's one-time hooks the idle run takes, in the order it runs them.")

(defconst agent-repl-startup--first-hooks-idle-delay 0.5
  "Seconds Emacs must sit idle after the startup ends before Doom's setup runs.
Long enough that the burst of roster pushes and redraws that follows the
last tab's opening is over; shorter than a person takes to read \"all N
workspaces ready.\" and reach for another tab.")

(defvar agent-repl-startup--first-hooks-timer nil
  "The timer that will run Doom's pending one-time hooks, while one is armed.")

(defun agent-repl-startup--first-hooks-pending ()
  "Return Doom's one-time hooks that have not run yet, in run order.
Nil outside Doom (a -Q batch Emacs defines none of them)."
  (and (fboundp 'doom-run-hooks)
       (cl-remove-if-not (lambda (hook) (and (boundp hook) (symbol-value hook)))
                         agent-repl-startup--first-hooks)))

(defun agent-repl-startup-first-hooks-arm ()
  "Arm the run of Doom's pending one-time hooks for the next idle stretch.
The run comes once Emacs has sat idle
`agent-repl-startup--first-hooks-idle-delay' seconds.  An idle timer counts
from the start of the CURRENT idle stretch, and one armed for fewer seconds
than Emacs has already sat idle waits for the NEXT stretch -- after the
user's next key, which may be the very switch this run exists to spare.
The startup ends from a stream event, often deep into an idle stretch, so
that case waits the delay on the clock instead and then re-checks
\(`agent-repl-startup--first-hooks-arm-or-run')."
  (when (and (null agent-repl-startup--first-hooks-timer)
             (agent-repl-startup--first-hooks-pending))
    (let ((idle (current-idle-time))
          (delay agent-repl-startup--first-hooks-idle-delay))
      (setq agent-repl-startup--first-hooks-timer
            (if (and idle (>= (float-time idle) delay))
                (run-at-time delay nil #'agent-repl-startup--first-hooks-arm-or-run)
              (run-with-idle-timer delay nil #'agent-repl-startup--first-hooks-run))))))

(defun agent-repl-startup--first-hooks-arm-or-run ()
  "Run Doom's pending one-time hooks if Emacs has sat idle long enough, else arm.
Input since arming started a fresh idle stretch: the run waits for that
stretch to reach the delay."
  (setq agent-repl-startup--first-hooks-timer nil)
  (let ((idle (current-idle-time)))
    (if (and idle (>= (float-time idle) agent-repl-startup--first-hooks-idle-delay))
        (agent-repl-startup--first-hooks-run)
      (agent-repl-startup-first-hooks-arm))))

(defun agent-repl-startup--first-hooks-run ()
  "Run every one of Doom's one-time hooks still pending, exactly as Doom would.
Each runs through `doom-run-hooks' and is set to nil after, as
`doom-run-hook-on' does, so neither Doom's path nor this one runs it twice.
The run is NOT interruptible by input: Doom's own triggers run these hooks
whole, and a hook cut off mid-function leaves a mode half enabled with no
point to resume from.  Keys typed meanwhile wait for it, once.  A hook that
signals is still cleared -- a half-run hook is never left for Doom's path
to run again -- and the failure is recorded and re-signalled."
  (setq agent-repl-startup--first-hooks-timer nil)
  (let ((hooks (agent-repl-startup--first-hooks-pending)))
    (if (null hooks)
        (agent-repl--log agent-repl-startup--central
                         "elisp.startup.first-hooks-idle ran=none (Doom's own path ran them first)")
      (let ((start (float-time))
            ;; As Doom's switch path binds it around the same hooks.
            (gc-cons-threshold most-positive-fixnum))
        (dolist (hook hooks)
          (unwind-protect
              (condition-case err
                  (doom-run-hooks hook)
                (error
                 (agent-repl--error agent-repl-startup--central
                                    "elisp.startup.first-hooks-idle hook=%s failed: %S"
                                    hook err)
                 (signal (car err) (cdr err))))
            (set hook nil)))
        (agent-repl--info agent-repl-startup--central
                          "elisp.startup.first-hooks-idle ran=%s ms=%d"
                          (mapconcat #'symbol-name hooks ",")
                          (round (* 1000 (- (float-time) start))))))))

(provide 'agent-repl-startup)
;;; startup.el ends here
