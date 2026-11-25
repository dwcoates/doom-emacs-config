;;; stall-culprit.el --- which work held the editor's command loop -*- lexical-binding: t; -*-

;;; Commentary:

;; A STALL IS NAMED BY THE WORK THAT HELD THE LOOP.  stall.el's sampler only
;; sees that it ran late; it cannot see what ran instead.  So every unit of
;; work the command loop runs on agent-repl's behalf or the editor's is TIMED
;; here, and each sampler run asks which unit ran longest since the run
;; before it (`agent-repl-stall-culprit-take').
;;
;; THE UNITS:
;;
;;   timer            every timer, through `timer-event-handler' (the C
;;                    loop calls it by name for each due timer, idle ones
;;                    too), named by the timer's function
;;   command          every command, through `command-execute' (the
;;                    command loop's, and each special event's), named by
;;                    the command
;;   process-filter   a daemon connection's filter and sentinel, wrapped
;;   process-sentinel by connect.el (`agent-repl-stall-culprit-wrap'),
;;                    named by the rpc method the connection carries
;;
;; EACH UNIT IS CHARGED ONLY THE TIME IT RAN ITSELF.  A unit run inside
;; another (a timer `accept-process-output' let a filter run under, a
;; command that ran a timer) pauses its parent's clock: one current unit is
;; charged at a time, so nothing is counted twice and the sum of the charges
;; never exceeds the wall time.  A minibuffer wait inside a command is
;; charged to no unit (`:idle'), since the command is waiting for a person,
;; not holding the loop.
;;
;; TWO MORE CANDIDATES compete with the units: garbage collection, read as
;; the growth of `gc-elapsed' across the window, and the UNATTRIBUTED
;; remainder, the part of the stall that no timed unit covers (redisplay, a
;; page's widget, a process that is not a daemon connection).  The largest
;; candidate is the culprit.
;;
;; WHAT THE TIMING COSTS.  A unit costs two `agent-repl--now' reads, one
;; `assoc' over the window's few keys and one cons, all on the loop it
;; measures.  Measured 2026-11-25 in batch on the owner's host (Emacs
;; 30.2, `benchmark-run' of 200000 wrapped no-op calls against unwrapped
;; ones): 0.73 microseconds a unit byte-compiled, as bytecode.el loads it
;; (3.3 interpreted), so a burst of a thousand units a second costs the loop
;; under a millisecond of every second.  The sampler itself is a timer, so its own
;; time is charged to `agent-repl-stall-sample' like any other unit's, and a
;; sampler grown costly would name itself.
;;
;; EACH UNIT IS ALSO CHARGED WHAT IT ALLOCATED.  Garbage collection held the
;; loop for most of the stalls attributed so far, and a collection comes
;; after a fixed amount of allocation, whoever allocated it.  So every charge
;; also reads Emacs's allocation counters (`cons-cells-consed' and its
;; siblings, exact and read without allocating) and adds the bytes allocated
;; since the last charge to the current unit; allocation while no unit runs
;; (redisplay, a process that is not a daemon connection, the command loop
;; itself) is charged to `untimed'.  Once a minute the top allocators, the
;; untimed remainder, and the collections the minute paid for are recorded
;; at INFO (`elisp.alloc.minute'), so the allocation that drives the
;; collections is named from the shared log alone.  Reading the counters
;; costs about 0.5 microseconds a charge (measured 2026-11-25 in batch).

;;; Code:

;; Every log call is a log-site macro (log-site.el), needed where this
;; file is expanded; config.el loads it first.
(require 'log-site)

(declare-function agent-repl--now "core" ())
(declare-function agent-repl--register-timer "core" (key timer))
(declare-function timer--function "timer" (timer))

(defconst agent-repl-stall-culprit--central
  '(:agent-repl-central "the editor's allocation spans every workspace")
  "The log scope of every allocation record.")

(defconst agent-repl-stall-culprit-report-interval 60
  "Seconds between two records of the minute's allocation.")

(defconst agent-repl-stall-culprit-report-top 8
  "How many of the minute's allocators its record names.")

(defvar agent-repl-stall-culprit--current nil
  "The key of the unit the loop is running now, `:idle', or nil for none.
A key is (SOURCE . NAME): SOURCE `timer', `command', `process-filter' or
`process-sentinel'; NAME a function symbol, `:anonymous', or an rpc
method's name.")

(defvar agent-repl-stall-culprit--since 0.0
  "The `agent-repl--now' from which the current unit is next charged.")

(defvar agent-repl-stall-culprit--window nil
  "Each unit's time in the current window, an alist of (KEY . SECONDS).")

(defvar agent-repl-stall-culprit--gc-at gc-elapsed
  "`gc-elapsed' at the current window's start.")

(defvar agent-repl-stall-culprit--suspended nil
  "The units a minibuffer wait suspended, innermost first.")

(defvar agent-repl-stall-culprit--allocated-at 0
  "`agent-repl-stall-culprit--allocated' at the last charge.")

(defvar agent-repl-stall-culprit--allocations nil
  "Each unit's allocation since the last minute's record, an alist (KEY . BYTES).
Allocation while no unit runs is under the key `(untimed)'.")

(defvar agent-repl-stall-culprit--report-from nil
  "`(GCS-DONE . GC-ELAPSED)' when the current minute's record began.")

(defun agent-repl-stall-culprit--allocated ()
  "Answer the bytes Emacs has allocated since it started, by its counters.
Each counter is weighed by the size of what it counts on a 64-bit build:
a cons or a float 16 bytes, a vector cell 8, a symbol 48, a string header
32, an interval 56, a string character 1."
  (+ (* 16 cons-cells-consed) (* 16 floats-consed) (* 8 vector-cells-consed)
     (* 48 symbols-consed) (* 32 strings-consed) (* 56 intervals-consed)
     string-chars-consed))

(defun agent-repl-stall-culprit--charge (now)
  "Charge the current unit its time up to NOW and what it allocated meanwhile.
The allocation is the bytes since the last charge.  Restarts its clock
at NOW."
  (let ((key agent-repl-stall-culprit--current)
        (allocated (agent-repl-stall-culprit--allocated)))
    (when (consp key)
      (let ((ran (- now agent-repl-stall-culprit--since))
            (cell (assoc key agent-repl-stall-culprit--window)))
        (if cell
            (setcdr cell (+ (cdr cell) ran))
          (push (cons key ran) agent-repl-stall-culprit--window))))
    (let* ((owner (if (consp key) key '(untimed)))
           (bytes (- allocated agent-repl-stall-culprit--allocated-at))
           (cell (assoc owner agent-repl-stall-culprit--allocations)))
      (if cell
          (setcdr cell (+ (cdr cell) bytes))
        (push (cons owner bytes) agent-repl-stall-culprit--allocations)))
    (setq agent-repl-stall-culprit--since now
          agent-repl-stall-culprit--allocated-at allocated)))

(defun agent-repl-stall-culprit--key-name (key)
  "Answer unit KEY as it reads in a record: SOURCE:NAME, or `untimed'."
  (if (cdr key)
      (format "%s:%s" (car key) (cdr key))
    (symbol-name (car key))))

(defun agent-repl-stall-culprit-report-allocations ()
  "Record at INFO the allocation since the last record, by unit, and start afresh.
Names the `agent-repl-stall-culprit-report-top' largest allocators in
megabytes, the untimed remainder, the total, and the collections and their
seconds since the last record."
  (agent-repl-stall-culprit--charge (agent-repl--now))
  (let* ((from (or agent-repl-stall-culprit--report-from (cons gcs-done gc-elapsed)))
         (allocations (sort (copy-sequence agent-repl-stall-culprit--allocations)
                            (lambda (a b) (> (cdr a) (cdr b)))))
         (total (apply #'+ 0 (mapcar #'cdr allocations)))
         (mb (lambda (bytes) (format "%.1f" (/ bytes 1048576.0)))))
    (agent-repl--info agent-repl-stall-culprit--central
                      "elisp.alloc.minute allocated-mb={allocated} untimed-mb={untimed} gcs={gcs} gc-seconds={gc} threshold={threshold} top={top}"
                      :allocated (funcall mb total)
                      :untimed (funcall mb (or (cdr (assoc '(untimed) allocations)) 0))
                      :gcs (- gcs-done (car from))
                      :gc (format "%.3f" (- gc-elapsed (cdr from)))
                      :threshold gc-cons-threshold
                      :top (mapconcat (lambda (cell)
                                        (format "%s=%s" (agent-repl-stall-culprit--key-name (car cell))
                                                (funcall mb (cdr cell))))
                                      (seq-take allocations agent-repl-stall-culprit-report-top)
                                      ","))
    (setq agent-repl-stall-culprit--allocations nil
          agent-repl-stall-culprit--report-from (cons gcs-done gc-elapsed))))

(defun agent-repl-stall-culprit-run (key fn &rest args)
  "Run FN on ARGS as the unit KEY, timing it; answer what FN answers.
The unit running before it is paused for its length and resumed after,
however FN exits."
  (let ((outer agent-repl-stall-culprit--current))
    (agent-repl-stall-culprit--charge (agent-repl--now))
    (setq agent-repl-stall-culprit--current key)
    (unwind-protect (apply fn args)
      (agent-repl-stall-culprit--charge (agent-repl--now))
      (setq agent-repl-stall-culprit--current outer))))

(defun agent-repl-stall-culprit-wrap (source method fn)
  "Answer FN timed as a unit of SOURCE named by rpc METHOD.
SOURCE is `process-filter' or `process-sentinel': connect.el wraps each
daemon connection's filter and sentinel with it."
  (let ((key (cons source method)))
    (lambda (&rest args) (apply #'agent-repl-stall-culprit-run key fn args))))

(defun agent-repl-stall-culprit--function-name (fn)
  "Answer FN's name for a unit key: its symbol, or `:anonymous'."
  (if (and fn (symbolp fn)) fn :anonymous))

(defun agent-repl-stall-culprit--around-timer (handler timer)
  "Run HANDLER on TIMER as a unit named by the timer's function."
  (agent-repl-stall-culprit-run
   (cons 'timer (agent-repl-stall-culprit--function-name (timer--function timer)))
   handler timer))

(defun agent-repl-stall-culprit--around-command (execute cmd &rest args)
  "Run EXECUTE on CMD and ARGS as a unit named by the command."
  (apply #'agent-repl-stall-culprit-run
         (cons 'command (agent-repl-stall-culprit--function-name cmd))
         execute cmd args))

(defun agent-repl-stall-culprit--minibuffer-entered ()
  "Suspend the current unit while a minibuffer waits for a person."
  (agent-repl-stall-culprit--charge (agent-repl--now))
  (push agent-repl-stall-culprit--current agent-repl-stall-culprit--suspended)
  (setq agent-repl-stall-culprit--current :idle))

(defun agent-repl-stall-culprit--minibuffer-exited ()
  "Resume the unit the minibuffer suspended."
  (agent-repl-stall-culprit--charge (agent-repl--now))
  (setq agent-repl-stall-culprit--current (pop agent-repl-stall-culprit--suspended)))

(defun agent-repl-stall-culprit-reset (now)
  "Open a new window at NOW, forgetting the last one."
  (agent-repl-stall-culprit--charge now)
  (setq agent-repl-stall-culprit--window nil
        agent-repl-stall-culprit--gc-at gc-elapsed))

(defun agent-repl-stall-culprit--culprit (key)
  "Answer the culprit plist of candidate KEY."
  (pcase (car key)
    ((or 'timer 'command) (list :source (car key) :function (cdr key)))
    ((or 'process-filter 'process-sentinel) (list :source (car key) :method (cdr key)))
    (source (list :source source))))

(defun agent-repl-stall-culprit-take (now stall)
  "Answer the culprit of a STALL seconds long ending at NOW; open a new window.
The answer is a plist: `:culprit', the culprit for
`agent-repl-wire-encode-editor-stall-culprit'; `:ran', the seconds it ran;
`:timed', the seconds every timed unit ran together; `:gc', the seconds
garbage collection ran."
  (agent-repl-stall-culprit--charge now)
  (let* ((window agent-repl-stall-culprit--window)
         (timed (apply #'+ 0.0 (mapcar #'cdr window)))
         (gc (- gc-elapsed agent-repl-stall-culprit--gc-at))
         (best (cons '(unattributed) (max 0.0 (- stall timed)))))
    (dolist (candidate (cons (cons '(garbage-collection) gc) window))
      (when (> (cdr candidate) (cdr best))
        (setq best candidate)))
    (agent-repl-stall-culprit-reset now)
    (list :culprit (agent-repl-stall-culprit--culprit (car best))
          :ran (cdr best) :timed timed :gc gc)))

(setq agent-repl-stall-culprit--allocated-at (agent-repl-stall-culprit--allocated))
(agent-repl--register-timer
 :allocation-report
 (run-at-time agent-repl-stall-culprit-report-interval agent-repl-stall-culprit-report-interval
              #'agent-repl-stall-culprit-report-allocations))

(advice-add 'timer-event-handler :around #'agent-repl-stall-culprit--around-timer)
(advice-add 'command-execute :around #'agent-repl-stall-culprit--around-command)
(add-hook 'minibuffer-setup-hook #'agent-repl-stall-culprit--minibuffer-entered)
(add-hook 'minibuffer-exit-hook #'agent-repl-stall-culprit--minibuffer-exited)

(provide 'stall-culprit)

;;; stall-culprit.el ends here
