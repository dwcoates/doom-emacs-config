;;; interaction-record.el --- Record and replay user interaction sequences -*- lexical-binding: t; -*-

;;; Commentary:

;; A reproduction currency for agent-repl debugging: capture exactly what
;; the user did, then replay it hands-free into a freshly bounced stack
;; until the sequence runs clean.
;;
;; The problem this solves is that the expensive part of an agent-repl
;; investigation is not the fix, it is the REPRODUCTION.  A user hits a
;; stall or a wrong color after some particular sequence of commands, and
;; every fix-verify iteration then needs a human to perform that sequence
;; again by hand.  Recording the sequence once turns the loop autonomous:
;; the agent bounces the stack, replays the file, mines the logs, fixes,
;; and repeats without the user in the seat.
;;
;; Three parts:
;;
;;   1. RECORDING.  `agent-repl-interaction-record-mode' is a global minor
;;      mode that appends one event per command from `pre-command-hook'.
;;      Each event carries the wall-clock time (subsecond), the key
;;      sequence (`this-command-keys-vector'), and `this-command'.
;;
;;      The KEY SEQUENCE is the replay currency, not the command symbol.
;;      A command read through `interactive' — a file name, a completion
;;      pick, a `y-or-n-p' — cannot be re-run by `funcall'ing its symbol,
;;      because the arguments live in the keys the user went on to type.
;;      Feeding the recorded keys back through `execute-kbd-macro'
;;      reproduces the whole exchange.  The command symbol is recorded
;;      alongside so the saved file reads as a human-legible transcript,
;;      and so a replay failure can name the command that failed.
;;
;;      Recording must never cost the user anything, so the hook does no
;;      I/O — it pushes onto an in-memory list bounded by
;;      `agent-repl-interaction-record-capacity' — and it must never break
;;      a command, so the whole body is `condition-case'd.  That is error
;;      SURFACING, not swallowing: a capture failure is logged loudly
;;      through the canonical ladder, it just is not signalled into the
;;      user's command loop, where it would abort the very command the
;;      user was trying to reproduce a bug with.
;;
;;   2. SAVING.  `agent-repl-interaction-record-save' writes the sequence
;;      under `~/.claude-emacs/interaction-recordings/<timestamp>.el' and
;;      RETURNS the absolute path, so an agent can read the path straight
;;      off an `emacsclient -e' invocation.  The file is DATA, not code: a
;;      single `setq' of `agent-repl-interaction-recording' holding a
;;      plist the replayer reads.  Saving an empty recording is an error
;;      rather than an empty file, because an empty file replays clean and
;;      would report a false all-clear.
;;
;;   3. REPLAY.  `agent-repl-interaction-replay' loads a saved file and
;;      schedules each event at its recorded offset from the first event,
;;      divided by SPEED.  The recorded gaps are kept because they carry
;;      the timing the bug may depend on; SPEED exists to compress user
;;      think-time when the timing does not matter.
;;
;;      Replay refuses to start while recording is active.  A replay that
;;      recorded itself would append its own synthetic events to the live
;;      recording and, on the next save, produce a file that replays the
;;      replay — a loop with no fixed point.
;;
;; Replay start, end, and per-event failures are logged through the
;; canonical ladder with a REPLAY ID, so the structured-logs runbook can
;; bound the replay window in the log and attribute errors and slowdowns
;; to it.  Grep for `interaction-replay: begin' and `interaction-replay:
;; end' carrying the same `replay_id='.
;;
;; What is NOT captured, and must be stated in any report built on this:
;; interactions INSIDE the webview (the GUI runs in a webkit widget and
;; its clicks never reach `pre-command-hook'), events originating outside
;; Emacs entirely (daemon activity, launchd, the vendor), and any
;; dependence on starting workspace state.  A recording is faithful only
;; against a stack whose starting state resembles the one it was taken
;; against.
;;
;; Run the tests with:
;;   emacs -batch -Q -l ert -l test-interaction-record.el \
;;     -f ert-run-tests-batch-and-exit

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;;;; ---- Recording ----

(defconst agent-repl-interaction-record-env "AGENT_REPL_RECORD_INTERACTIONS"
  "Environment variable that enables interaction recording at module load.
Set to any non-empty value to start Emacs with recording already live,
which is how an agent hands a recording session to the user without
asking them to run a command first.")

(defconst agent-repl-interaction-record-format-version 1
  "Schema version stamped into every saved recording file.
The replayer refuses a file whose version it does not know rather than
guessing at an unfamiliar event shape.")

(defcustom agent-repl-interaction-record-capacity 20000
  "Maximum number of events retained by the in-memory recorder.
When the recording exceeds this, the OLDEST events are dropped in one
chunk (see `agent-repl--interaction-record-trim-fraction') so the trim
cost is amortized rather than paid on every command at the cap.  A drop
is logged as a warning, because a truncated recording no longer replays
the beginning of what the user did."
  :type 'integer
  :group 'agent-repl)

(defconst agent-repl--interaction-record-trim-fraction 0.1
  "Fraction of `agent-repl-interaction-record-capacity' dropped per trim.
Trimming in chunks keeps the amortized per-command cost constant; a
trim-to-exactly-capacity policy would walk the whole list on every
command once the cap is reached.")

(defcustom agent-repl-interaction-recordings-dir-name "interaction-recordings"
  "Directory under the agent-repl state dir holding saved recordings."
  :type 'string
  :group 'agent-repl)

(defvar agent-repl--interaction-record-events nil
  "Reversed list of recorded events; newest first.
Each event is a plist with `:time' (float seconds), `:keys' (the key
vector), and `:command' (the command symbol).  Reversed because the hot
path pushes; `agent-repl--interaction-record-events-in-order' is the
chronological view.")

(defvar agent-repl--interaction-record-count 0
  "Number of events currently held in `agent-repl--interaction-record-events'.
Tracked separately so the hot path never calls `length'.")

(defvar agent-repl--interaction-record-started-at nil
  "Float time at which the current recording session was enabled, or nil.")

(defun agent-repl--interaction-record-events-in-order ()
  "Return the recorded events oldest-first, as a fresh list."
  (reverse agent-repl--interaction-record-events))

(defun agent-repl--interaction-record-reset ()
  "Discard every recorded event and reset the recording counters."
  (let ((dropped agent-repl--interaction-record-count))
    (setq agent-repl--interaction-record-events nil
          agent-repl--interaction-record-count 0
          agent-repl--interaction-record-started-at nil)
    (agent-repl--log nil "interaction-record: reset dropped=%d" dropped)
    dropped))

(defun agent-repl--interaction-record-trim ()
  "Drop the oldest events when the recording exceeds its capacity.
Returns the number of events dropped (0 when under capacity)."
  (if (<= agent-repl--interaction-record-count
          agent-repl-interaction-record-capacity)
      0
    (let* ((chunk (max 1 (truncate (* agent-repl-interaction-record-capacity
                                      agent-repl--interaction-record-trim-fraction))))
           (target (max 1 (- agent-repl-interaction-record-capacity chunk)))
           (drop (- agent-repl--interaction-record-count target)))
      (setq agent-repl--interaction-record-events
            (nbutlast agent-repl--interaction-record-events drop))
      (setq agent-repl--interaction-record-count target)
      (agent-repl--warn nil
                        (concat "interaction-record: dropped oldest events "
                                "dropped=%d retained=%d capacity=%d")
                        drop target agent-repl-interaction-record-capacity)
      drop)))

(defun agent-repl--interaction-record-capture ()
  "Append the command about to run to the in-memory recording.
Installed on `pre-command-hook' by
`agent-repl-interaction-record-mode'.

Never signals.  A capture failure is logged loudly and the user's
command proceeds: aborting the command loop to report a recorder bug
would destroy the very interaction the recording exists to preserve."
  (condition-case err
      (progn
        (push (list :time (float-time)
                    :keys (this-command-keys-vector)
                    :command this-command)
              agent-repl--interaction-record-events)
        (setq agent-repl--interaction-record-count
              (1+ agent-repl--interaction-record-count))
        (agent-repl--interaction-record-trim))
    (error
     (agent-repl--warn nil
                       "interaction-record: capture failed command=%S err=%S"
                       this-command err)
     nil)))

;;;###autoload
(define-minor-mode agent-repl-interaction-record-mode
  "Record every command the user runs, for later replay.
Enabling starts a FRESH recording; the previously recorded events are
discarded, because a recording concatenated across two disjoint sessions
replays as a sequence the user never performed.  Disabling stops
capture but KEEPS the events, so
`agent-repl-interaction-record-save' still has something to write."
  :global t
  :group 'agent-repl
  :lighter " AR-Rec"
  (if agent-repl-interaction-record-mode
      (progn
        (agent-repl--interaction-record-reset)
        (setq agent-repl--interaction-record-started-at (float-time))
        (add-hook 'pre-command-hook #'agent-repl--interaction-record-capture)
        (agent-repl--info nil
                          "interaction-record: recording ENABLED started-at=%s capacity=%d"
                          (agent-repl--log-rfc3339-timestamp)
                          agent-repl-interaction-record-capacity))
    (remove-hook 'pre-command-hook #'agent-repl--interaction-record-capture)
    (agent-repl--info nil "interaction-record: recording DISABLED events=%d"
                      agent-repl--interaction-record-count)))

(defun agent-repl--interaction-record-enable-from-env ()
  "Enable recording when `agent-repl-interaction-record-env' is set non-empty.
Returns non-nil when recording was enabled.  This is the startup path an
agent uses to hand a live recording session to the user:

  AGENT_REPL_RECORD_INTERACTIONS=1 emacs"
  (let ((value (getenv agent-repl-interaction-record-env)))
    (if (and value (not (string-empty-p value)))
        (progn
          (agent-repl--info nil "interaction-record: enabling from env %s=%S"
                            agent-repl-interaction-record-env value)
          (agent-repl-interaction-record-mode 1)
          t)
      (agent-repl--log nil "interaction-record: env %s unset or empty value=%S"
                       agent-repl-interaction-record-env value)
      nil)))

;;;; ---- Saving ----

(defun agent-repl--interaction-recordings-dir ()
  "Return the directory holding saved recordings, with a trailing slash.
Creates nothing."
  (file-name-as-directory
   (agent-repl--global-state-file agent-repl-interaction-recordings-dir-name)))

(defun agent-repl--interaction-record-default-file ()
  "Return a fresh timestamped absolute path for a recording file."
  (expand-file-name (format-time-string "%Y%m%dT%H%M%S.el")
                    (agent-repl--interaction-recordings-dir)))

(defun agent-repl--interaction-record-serialize (events)
  "Return the loadable elisp text for EVENTS, oldest-first.
The result is DATA: one `setq' of `agent-repl-interaction-recording'
holding a plist.  Each event gains an `:offset' (seconds since the first
event) so the replayer never has to re-derive the schedule, and a
`:key-description' so the file reads as a transcript."
  (let* ((base (plist-get (car events) :time))
         (index -1)
         (records
          (mapcar
           (lambda (event)
             (setq index (1+ index))
             (list :index index
                   :offset (- (plist-get event :time) base)
                   :time (plist-get event :time)
                   :keys (plist-get event :keys)
                   :key-description (key-description (plist-get event :keys))
                   :command (plist-get event :command)))
           events))
         (print-level nil)
         (print-length nil)
         (print-quoted t))
    (concat
     ";;; agent-repl interaction recording -*- lexical-binding: t; -*-\n"
     ";;\n"
     ";; DATA, not code.  Replay with:\n"
     ";;   (agent-repl-interaction-replay \"<this-file>\")\n"
     ";;\n"
     ";; :keys is the replay currency; :command and :key-description are\n"
     ";; for humans reading this file.\n\n"
     (format "(setq agent-repl-interaction-recording\n      '%s)\n"
             (prin1-to-string
              (list :version agent-repl-interaction-record-format-version
                    :recorded-at (agent-repl--log-rfc3339-timestamp)
                    :emacs-version emacs-version
                    :event-count (length records)
                    :events records))))))

;;;###autoload
(defun agent-repl-interaction-record-save (&optional file)
  "Write the current recording to FILE and return its absolute path.
FILE defaults to a fresh timestamped path under
`agent-repl--interaction-recordings-dir'.  Returns the absolute path so
the agent-facing invocation prints it:

  emacsclient -e \\='(agent-repl-interaction-record-save)\\='

Signals when the recording is empty.  An empty file would load and
replay without error, which reads as a clean run of a sequence that was
never performed."
  (interactive)
  (let ((events (agent-repl--interaction-record-events-in-order)))
    (unless events
      (agent-repl--error
       nil
       (concat "interaction-record: refusing to save an EMPTY recording "
               "(mode=%s) — enable `agent-repl-interaction-record-mode' "
               "and perform the interaction first")
       (if agent-repl-interaction-record-mode "on" "off")))
    (let* ((path (expand-file-name
                  (or file (agent-repl--interaction-record-default-file))))
           (text (agent-repl--interaction-record-serialize events)))
      (make-directory (file-name-directory path) t)
      (with-temp-file path
        (insert text))
      (agent-repl--info nil
                        "interaction-record: saved file=%S events=%d span=%.3fs"
                        path (length events)
                        (- (plist-get (car (last events)) :time)
                           (plist-get (car events) :time)))
      path)))

;;;; ---- Replay ----

(defvar agent-repl-interaction-recording nil
  "Recording plist most recently loaded from a saved recording file.
Saved files `setq' this variable; the replayer reads it back.")

(defvar agent-repl--interaction-replay-id nil
  "Replay id of the replay currently in flight, or nil when idle.")

(defvar agent-repl--interaction-replay-timers nil
  "Timers scheduled by the replay in flight.
NOT registered through `agent-repl--register-timer': that registry keys
LONG-LIVED singleton jobs so re-loading an owner file is idempotent.
Replay timers are transient and there are one per recorded event, so
they are tracked here and cancelled together by
`agent-repl-interaction-replay-abort'.")

(defvar agent-repl--interaction-replay-failures 0
  "Number of events that failed during the replay in flight.")

(defvar agent-repl-interaction-replay-schedule-function #'run-at-time
  "Function used to schedule one replay event.
Called as (FN DELAY nil CALLBACK).  Injectable so tests can assert the
schedule the replayer builds without waiting on real time.")

(defvar agent-repl-interaction-replay-execute-function #'execute-kbd-macro
  "Function used to feed one recorded key vector back into Emacs.
Called as (FN KEYS).  Injectable so tests can drive replay without
executing real commands.")

(defun agent-repl--interaction-replay-load (file)
  "Load recording FILE and return its plist.
Signals when FILE is missing, holds no recording, or carries an
unknown schema version."
  (let ((path (expand-file-name file)))
    (unless (file-readable-p path)
      (agent-repl--error nil "interaction-replay: unreadable recording file=%S" path))
    (let ((agent-repl-interaction-recording nil))
      (load path nil t t)
      (let ((recording agent-repl-interaction-recording))
        (unless (plist-member recording :events)
          (agent-repl--error
           nil "interaction-replay: file carries no recording file=%S" path))
        (let ((version (plist-get recording :version)))
          (unless (equal version agent-repl-interaction-record-format-version)
            (agent-repl--error
             nil
             "interaction-replay: unsupported recording version=%S supported=%S file=%S"
             version agent-repl-interaction-record-format-version path)))
        recording))))

(defun agent-repl--interaction-replay-new-id ()
  "Return a fresh replay id, unique enough to bound a log window."
  (format "replay-%s-%04x" (format-time-string "%Y%m%dT%H%M%S") (random 65536)))

(defun agent-repl--interaction-replay-run-event (replay-id event)
  "Execute one recorded EVENT for REPLAY-ID.
A failing event is logged and the replay CONTINUES: the later events are
often what the investigation is after, and stopping at the first failure
would hide every subsequent one."
  (condition-case err
      (progn
        (agent-repl--log nil
                         "interaction-replay: event replay_id=%s index=%s keys=%S command=%S"
                         replay-id (plist-get event :index)
                         (plist-get event :key-description)
                         (plist-get event :command))
        (funcall agent-repl-interaction-replay-execute-function
                 (plist-get event :keys)))
    (error
     (setq agent-repl--interaction-replay-failures
           (1+ agent-repl--interaction-replay-failures))
     (agent-repl--warn nil
                       (concat "interaction-replay: event FAILED replay_id=%s "
                               "index=%s command=%S keys=%S err=%S")
                       replay-id (plist-get event :index)
                       (plist-get event :command)
                       (plist-get event :key-description) err))))

(defun agent-repl--interaction-replay-finish (replay-id event-count)
  "Close out REPLAY-ID after EVENT-COUNT events and log the window's end."
  (let ((failures agent-repl--interaction-replay-failures))
    (setq agent-repl--interaction-replay-id nil
          agent-repl--interaction-replay-timers nil)
    (agent-repl--info nil
                      "interaction-replay: end replay_id=%s events=%d failures=%d"
                      replay-id event-count failures)
    failures))

;;;###autoload
(defun agent-repl-interaction-replay (file &optional speed)
  "Replay the recording in FILE into this Emacs and return its replay id.
SPEED scales the recorded inter-command delays (default 1.0, meaning the
recorded pacing is reproduced exactly); 2.0 replays twice as fast.  The
recorded gaps are honored rather than collapsed because a bug that
depends on timing is invisible in a sequence fired back to back.

Agent-facing invocation:

  emacsclient -e \\='(agent-repl-interaction-replay \"/path/to/rec.el\")\\='

Refuses while `agent-repl-interaction-record-mode' is on — a replay that
records itself has no fixed point — and while another replay is in
flight.  Logs `interaction-replay: begin' and `interaction-replay: end'
lines carrying the returned replay id, which is how a log-mining runbook
bounds the replay window."
  (interactive "fRecording file: ")
  (when agent-repl-interaction-record-mode
    (agent-repl--error
     nil
     (concat "interaction-replay: refusing to replay while recording is ACTIVE "
             "— disable `agent-repl-interaction-record-mode' first")))
  (when agent-repl--interaction-replay-id
    (agent-repl--error
     nil "interaction-replay: refusing, replay already in flight replay_id=%s"
     agent-repl--interaction-replay-id))
  (let* ((scale (or speed 1.0)))
    (unless (and (numberp scale) (> scale 0))
      (agent-repl--error nil "interaction-replay: SPEED must be a positive number, got %S"
                         speed))
    (let* ((recording (agent-repl--interaction-replay-load file))
           (events (plist-get recording :events))
           (count (length events))
           (replay-id (agent-repl--interaction-replay-new-id))
           (last-delay 0.0))
      (unless events
        (agent-repl--error nil "interaction-replay: recording has no events file=%S" file))
      (setq agent-repl--interaction-replay-id replay-id
            agent-repl--interaction-replay-timers nil
            agent-repl--interaction-replay-failures 0)
      (agent-repl--info
       nil
       (concat "interaction-replay: begin replay_id=%s file=%S events=%d "
               "speed=%s recorded-at=%S")
       replay-id (expand-file-name file) count scale
       (plist-get recording :recorded-at))
      (dolist (event events)
        (let ((delay (/ (or (plist-get event :offset) 0.0) scale)))
          (setq last-delay (max last-delay delay))
          (push (funcall agent-repl-interaction-replay-schedule-function
                         delay nil
                         (lambda ()
                           (agent-repl--interaction-replay-run-event replay-id event)))
                agent-repl--interaction-replay-timers)))
      (push (funcall agent-repl-interaction-replay-schedule-function
                     last-delay nil
                     (lambda ()
                       (agent-repl--interaction-replay-finish replay-id count)))
            agent-repl--interaction-replay-timers)
      replay-id)))

;;;###autoload
(defun agent-repl-interaction-replay-abort ()
  "Cancel every timer of the replay in flight and return its replay id.
Returns nil when no replay is in flight."
  (interactive)
  (let ((replay-id agent-repl--interaction-replay-id)
        (pending (length agent-repl--interaction-replay-timers)))
    (dolist (timer agent-repl--interaction-replay-timers)
      (when (timerp timer)
        (cancel-timer timer)))
    (setq agent-repl--interaction-replay-timers nil
          agent-repl--interaction-replay-id nil)
    (agent-repl--info nil "interaction-replay: aborted replay_id=%S cancelled=%d"
                      replay-id pending)
    replay-id))

;; Startup enablement.  Gated on interactive use: the batch ERT suite
;; loads config.el, and a stray env var must not install a
;; `pre-command-hook' under the test harness.  Tests call
;; `agent-repl--interaction-record-enable-from-env' directly instead.
(unless noninteractive
  (agent-repl--interaction-record-enable-from-env))

(provide 'interaction-record)

;;; interaction-record.el ends here
