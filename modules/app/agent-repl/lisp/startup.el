;;; startup.el --- Doom's one-time setup, run idle after the editor's first open -*- lexical-binding: t; -*-

;;; Commentary:

;; The editor's first open (open-tabs.el) ends by arming Doom's one-time
;; first-file and first-buffer setup for the next idle stretch, so no tab
;; switch pays for it.

;;; Code:

(require 'cl-lib)

(declare-function agent-repl--info "core" (ws fmt &rest args))
(declare-function agent-repl--log "core" (ws fmt &rest args))
(declare-function agent-repl--error "core" (ws fmt &rest args))
(declare-function doom-run-hooks "doom-lib" (&rest hooks))

(defconst agent-repl-startup--central
  '(:agent-repl-central "the editor's startup spans every workspace")
  "The log scope of Doom's one-time setup records.")

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
