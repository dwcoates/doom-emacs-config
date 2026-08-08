;;; readiness.el --- deploy readiness in the output window's modeline -*- lexical-binding: t; -*-

;;; Commentary:

;; Polls `bin/readiness-report.sh' and paints its answer into the modeline of
;; every agent-repl OUTPUT (webview) window.
;;
;; The problem is the one AGENTS.md opens with: every component here is a built
;; artifact and every running process keeps serving the image it started with,
;; so a merged commit deploys nothing on its own.  A merged-but-undeployed fix
;; looks exactly like a fix that does not work — except the correct code
;; sitting in `git log' makes it harder to diagnose, not easier.  The
;; information that would end that confusion in one glance already exists on
;; disk; it just was not anywhere the user looks.  Now it is, in the one window
;; they are already reading.
;;
;; ELISP IS NOT REPORTED, deliberately.  Every other system's deployed revision
;; is recoverable from disk, but elisp is deployed by being LOADED into a live
;; Emacs, and only that Emacs knows which definitions its obarray holds.  The
;; script says so in its own header; this module inherits the omission rather
;; than papering over it with a file mtime that would be a guess.
;;
;; Three properties this must have, because it runs inside the editor:
;;
;;   1. NEVER BLOCKS.  The report shells out to git, and a slow disk or a busy
;;      repository would otherwise freeze Emacs on a timer.  The script runs
;;      through an asynchronous process; nothing here ever calls a synchronous
;;      process function.
;;
;;   2. NEVER OVERLAPS.  A run still in flight when the timer fires means the
;;      machine is slower than the poll interval.  The tick is skipped rather
;;      than queued, so a slow report degrades to a lower refresh rate instead
;;      of accumulating processes forever.
;;
;;   3. NEVER SIGNALS FROM THE TIMER.  An error out of a timer function is
;;      reported once and then Emacs cancels nothing, so a persistent failure
;;      would spam the user every interval with something they cannot act on.
;;      Failures are recorded in the report state (the segment renders them)
;;      and logged loudly to the agent-repl log, which is where a failing
;;      subsystem belongs.  Errors are surfaced, never swallowed — the segment
;;      showing "?" while the log carries the reason is the surfacing.
;;
;; Run tests with:
;;   emacs -batch -Q -l ert -l test-readiness.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function agent-repl--log "core" (ws fmt &rest args))
(declare-function agent-repl--log-verbose "core" (ws fmt &rest args))
(declare-function agent-repl--warn "core" (ws fmt &rest args))
(declare-function agent-repl--frontend-init-inhibited-p "frontend" ())

(defvar agent-repl--timers)
(defvar agent-repl-frontend-webview-adopt-hook)
(defvar agent-repl--color-done-green)
(defvar agent-repl--color-idle-async-yellow)
(defvar agent-repl--color-thinking-red)

;;;; Customization

(defgroup agent-repl-readiness nil
  "Deploy-readiness reporting for agent-repl."
  :group 'agent-repl)

(defcustom agent-repl-readiness-poll-interval 15
  "Seconds between deploy-readiness polls.
The report is git plumbing only and costs a fraction of a second, but it
is still a subprocess: raising this trades staleness for load, and
lowering it below the time one report takes just makes every other tick
skip (see `agent-repl--readiness-poll')."
  :type 'integer
  :group 'agent-repl-readiness)

(defcustom agent-repl-readiness-enabled t
  "When non-nil, poll deploy readiness and paint it in output modelines."
  :type 'boolean
  :group 'agent-repl-readiness)

(defcustom agent-repl-readiness-script
  (expand-file-name "../bin/readiness-report.sh"
                    (file-name-directory (or load-file-name buffer-file-name
                                             default-directory)))
  "Absolute path of the readiness report script.
Resolved from this file's own location so a worktree reports on ITSELF
rather than on whatever checkout happens to be first on PATH — the whole
point is to describe the tree the user is looking at.

`bin/' is a sibling of this file's `lisp/' directory, so the base is one
level up.  Climbing the wrong number of levels is the classic failure
here: a stale `modules/app/bin/' resolution once made every poll exit 127
and readiness never reported at all."
  :type 'string
  :group 'agent-repl-readiness)

;;;; State

(defvar agent-repl--readiness nil
  "The last deploy-readiness report, or nil before the first poll lands.

A plist:
  :generated-at  ISO8601 string from the report
  :systems       alist of NAME (string) -> alist of the system's fields
  :error         string when the last run failed; :systems is then stale

nil and a non-nil :error are different states and the modeline renders
them differently: nil means \"not yet known\", which is honest during
startup, while :error means \"asked and failed\", which is a problem.")

(defvar agent-repl--readiness-process nil
  "The in-flight report process, or nil.
Non-nil is the coalescing interlock: a tick that finds it live skips.")

(defvar agent-repl--readiness-timer nil
  "The repeating poll timer, or nil.
A convenience handle on the timer registered under the `:readiness-poll'
key; the registry (core.el) is what guarantees there is never more than
one.")

;;;; Faces
;;
;; Colors come from the same named constants status.el's tab palette uses, so
;; the readiness cells read as the same system as the workspace tabs rather
;; than as a second, unrelated color vocabulary.  Declared as `defface' forms
;; (not inline `:foreground' plists) because that is the module's Doom theming
;; hook: a user can `customize-face' these exactly like the tab faces.

(defface agent-repl-readiness-ready
  `((t :foreground ,(if (boundp 'agent-repl--color-done-green)
                        agent-repl--color-done-green
                      "#1a7a1a")
       :weight normal))
  "Face for a system whose deployed build carries every committed change."
  :group 'agent-repl-readiness)

(defface agent-repl-readiness-behind
  `((t :foreground ,(if (boundp 'agent-repl--color-idle-async-yellow)
                        agent-repl--color-idle-async-yellow
                      "#f59e0b")
       :weight normal))
  "Face for a system with committed changes its deployed build lacks."
  :group 'agent-repl-readiness)

(defface agent-repl-readiness-stale
  `((t :foreground ,(if (boundp 'agent-repl--color-thinking-red)
                        agent-repl--color-thinking-red
                      "#cc3333")
       :weight bold))
  "Face for a system whose RUNNING process serves an older binary.
Bold and red because this is the state that most convincingly imitates a
broken fix: the code is correct, built, and installed, and the process
the user is talking to has never seen it."
  :group 'agent-repl-readiness)

(defface agent-repl-readiness-unknown
  '((t :inherit shadow :weight normal))
  "Face for a system whose readiness could not be determined.
Deliberately dim: an unknown is not an alarm, and rendering it like one
would train the user to ignore the alarming colors that matter.")

;;;; Report parsing

(defun agent-repl--readiness-parse (output)
  "Parse OUTPUT, the report script's stdout, into the state plist.
Signals on malformed JSON; callers running from a timer must catch."
  (let* ((doc (json-parse-string output
                                 :object-type 'alist
                                 :array-type 'list
                                 :null-object nil
                                 :false-object nil))
         (systems (alist-get 'systems doc)))
    (list :generated-at (alist-get 'generated_at doc)
          :systems (mapcar (lambda (sys) (cons (alist-get 'name sys) sys))
                           systems))))

(defun agent-repl--readiness-field (system key)
  "Return KEY from SYSTEM, an entry of the parsed :systems alist."
  (alist-get key (cdr system)))

;;;; Polling

(defun agent-repl--readiness-run-script (callback)
  "Start the readiness script asynchronously; call CALLBACK when it exits.
CALLBACK receives (EXIT-CODE OUTPUT).  Returns the process.

This is the module's only external-process boundary and is registered in
`agent-repl--external-boundary-functions'; tests stub THIS, never the
process primitives underneath it."
  (let* ((buf (generate-new-buffer " *agent-repl-readiness*"))
         (proc (start-process "agent-repl-readiness" buf
                              "bash" agent-repl-readiness-script)))
    (set-process-query-on-exit-flag proc nil)
    (set-process-sentinel
     proc
     (lambda (p _event)
       (unless (process-live-p p)
         (let ((code (process-exit-status p))
               (out (when (buffer-live-p (process-buffer p))
                      (with-current-buffer (process-buffer p) (buffer-string)))))
           (when (buffer-live-p (process-buffer p))
             (kill-buffer (process-buffer p)))
           (funcall callback code (or out ""))))))
    proc))

(defun agent-repl--readiness-receive (code output)
  "Fold a finished report run into `agent-repl--readiness'.
CODE is the exit status and OUTPUT its stdout.  Never signals: this runs
off a process sentinel driven by a timer, and an error here would reach
the user as noise they cannot act on.  Both failure modes are recorded
in the state AND logged, so nothing is swallowed."
  (setq agent-repl--readiness-process nil)
  (if (not (eq code 0))
      (progn
        (setq agent-repl--readiness
              (list :error (format "readiness script exited %s" code)
                    :systems (plist-get agent-repl--readiness :systems)))
        (agent-repl--warn nil "readiness: script exited %s output=%S" code output))
    (condition-case err
        (progn
          (setq agent-repl--readiness (agent-repl--readiness-parse output))
          (agent-repl--log-verbose
           nil "readiness: report parsed systems=%d"
           (length (plist-get agent-repl--readiness :systems))))
      (error
       (setq agent-repl--readiness
             (list :error (format "unparseable report: %s"
                                  (error-message-string err))
                   :systems (plist-get agent-repl--readiness :systems)))
       (agent-repl--warn nil "readiness: unparseable report err=%S output=%S"
                         err output))))
  (force-mode-line-update t))

(defun agent-repl--readiness-poll ()
  "Poll the readiness report once, unless a run is already in flight.
The skip is the whole overlap policy: a machine slower than the interval
gets a lower refresh rate, not an unbounded pile of processes."
  (when agent-repl-readiness-enabled
    (if (process-live-p agent-repl--readiness-process)
        (agent-repl--log-verbose
         nil "readiness: poll skipped, a run is still in flight")
      (condition-case err
          (setq agent-repl--readiness-process
                (agent-repl--readiness-run-script
                 #'agent-repl--readiness-receive))
        (error
         (setq agent-repl--readiness-process nil)
         (setq agent-repl--readiness
               (list :error (format "could not start report: %s"
                                    (error-message-string err))
                     :systems (plist-get agent-repl--readiness :systems)))
         (agent-repl--warn nil "readiness: could not start report err=%S" err)
         (force-mode-line-update t))))))

;;;; Timer lifecycle

(defun agent-repl--readiness-cancel-timer ()
  "Cancel the readiness poll timer, if one is running.
Delegates the registry bookkeeping to `agent-repl--cancel-timer-key' and
clears this module's own handle."
  (prog1 (agent-repl--cancel-timer-key :readiness-poll)
    (setq agent-repl--readiness-timer nil)))

(defun agent-repl--readiness-start-timer ()
  "Start the readiness poll timer, replacing any existing one.
Arming through the `:readiness-poll' registry key is what makes \"never
more than one timer\" true across a re-eval of this file, which is how
the module is hot-reloaded.  This is also the arm function core.el's
`agent-repl--required-timer-keys' names for the key, so a stranded
readiness poll is re-armed through exactly this path."
  (setq agent-repl--readiness-timer
        (agent-repl--register-timer
         :readiness-poll
         (run-with-timer agent-repl-readiness-poll-interval
                         agent-repl-readiness-poll-interval
                         #'agent-repl--readiness-poll)))
  (agent-repl--log nil "readiness: poll timer started interval=%s script=%s"
                   agent-repl-readiness-poll-interval
                   agent-repl-readiness-script)
  agent-repl--readiness-timer)

;;;; Modeline segment

(defconst agent-repl--readiness-abbrevs
  '(("daemon"              . "D")
    ("shim"                . "S")
    ("webapp"              . "W")
    ("shim-store"          . "St")
    ("shim-claude-sidecar" . "Sc"))
  "System name to modeline abbreviation.
An unlisted system falls back to its first character, so a system added
to the script shows up as something rather than vanishing.")

(defun agent-repl--readiness-abbrev (name)
  "Return the modeline abbreviation for system NAME."
  (or (cdr (assoc name agent-repl--readiness-abbrevs))
      (substring name 0 1)))

(defun agent-repl--readiness-cell (system)
  "Return (TEXT . FACE) for SYSTEM, one entry of the :systems alist.

Precedence is deliberate.  A stale running process outranks everything
else because it is the state that imitates a broken fix most closely,
and it stays distinct even when the system is also behind (the count is
kept alongside the marker rather than replaced by it)."
  (let* ((name (car system))
         (abbrev (agent-repl--readiness-abbrev name))
         (behind (agent-repl--readiness-field system 'commits_behind))
         (running (agent-repl--readiness-field system 'running))
         (stale (and running (alist-get 'stale_binary running)))
         (ready (agent-repl--readiness-field system 'ready)))
    (cond
     (stale (cons (format "%s↯%s" abbrev
                          (if (and (numberp behind) (> behind 0))
                              (number-to-string behind)
                            ""))
                  'agent-repl-readiness-stale))
     (ready (cons (concat abbrev "✓") 'agent-repl-readiness-ready))
     ((and (numberp behind) (> behind 0))
      (cons (format "%s↓%d" abbrev behind) 'agent-repl-readiness-behind))
     (t (cons (concat abbrev "?") 'agent-repl-readiness-unknown)))))

(defun agent-repl--readiness-help-echo (system)
  "Return the tooltip text for SYSTEM."
  (let ((err (agent-repl--readiness-field system 'error))
        (behind (agent-repl--readiness-field system 'commits_behind))
        (mins (agent-repl--readiness-field system 'minutes_behind))
        (running (agent-repl--readiness-field system 'running)))
    (string-join
     (delq nil
           (list (car system)
                 (when (numberp behind) (format "%d commit(s) behind" behind))
                 (when (numberp mins) (format "%d minute(s) behind" mins))
                 (when (and running (alist-get 'stale_binary running))
                   (format "pid %s is running an older binary"
                           (alist-get 'pid running)))
                 err))
     "\n")))

(defun agent-repl--readiness-segment ()
  "Return the propertized readiness string for a modeline.

Renders \"…\" until the first poll lands: an empty string would be
indistinguishable from \"everything is fine\", and a fabricated verdict
before any report exists is exactly the guess this whole feature is
meant to replace."
  (cond
   ((not agent-repl-readiness-enabled) "")
   ((null agent-repl--readiness)
    (concat " " (propertize "…" 'face 'agent-repl-readiness-unknown
                            'help-echo "agent-repl readiness: awaiting first report")))
   ((and (plist-get agent-repl--readiness :error)
         (null (plist-get agent-repl--readiness :systems)))
    (concat " " (propertize "rdy?" 'face 'agent-repl-readiness-unknown
                            'help-echo (plist-get agent-repl--readiness :error))))
   (t
    (concat
     " "
     (string-join
      (mapcar (lambda (system)
                (let ((cell (agent-repl--readiness-cell system)))
                  (propertize (car cell) 'face (cdr cell)
                              'help-echo (agent-repl--readiness-help-echo system))))
              (plist-get agent-repl--readiness :systems))
      " ")
     ;; A stale report still shows its last-known cells — dropping them on a
     ;; transient script failure would flicker the whole segment away — but it
     ;; must not present them as current, hence the trailing marker.
     (if (plist-get agent-repl--readiness :error)
         (propertize "!" 'face 'agent-repl-readiness-unknown
                     'help-echo (plist-get agent-repl--readiness :error))
       "")))))

;;;; Modeline attachment

(defconst agent-repl--readiness-mode-line-spec
  '(:eval (agent-repl--readiness-segment))
  "Trailing `:eval' modeline segment painting deploy readiness.
Captured as a constant so the attach helper can detect (via `member')
whether a buffer's modeline already carries it.")

(defun agent-repl--readiness-attach-to-mode-line (buf)
  "Append the readiness segment to BUF's `mode-line-format' if missing.
Idempotent — does nothing when the segment is already present, the
buffer is dead, or the buffer's modeline is not a list."
  (if (not (buffer-live-p buf))
      (agent-repl--log nil "readiness attach: skipped dead buffer=%S" buf)
    (with-current-buffer buf
      (let* ((format-is-list (listp mode-line-format))
             (already (and format-is-list
                           (member agent-repl--readiness-mode-line-spec
                                   mode-line-format))))
        (if (and format-is-list (not already))
            (progn
              (setq-local mode-line-format
                          (append mode-line-format
                                  (list agent-repl--readiness-mode-line-spec)))
              (force-mode-line-update t)
              (agent-repl--log nil "readiness attach: attached buffer=%S"
                               (buffer-name buf)))
          (agent-repl--log nil
                           "readiness attach: skipped buffer=%S format-is-list=%s already=%s"
                           (buffer-name buf) format-is-list (not (null already))))))))

(defun agent-repl--readiness-attach-to-current-webview ()
  "Attach the readiness segment to the current buffer.
Registered on `agent-repl-frontend-webview-adopt-hook', which frontend.el
runs inside every freshly adopted webview buffer — the OUTPUT window.
The input buffer never passes through that hook, which is exactly why
the segment lands on one and not the other."
  (agent-repl--readiness-attach-to-mode-line (current-buffer)))

(add-hook 'agent-repl-frontend-webview-adopt-hook
          #'agent-repl--readiness-attach-to-current-webview)

;;;; Startup

;; The poll shells out, so it is started only where a runtime is expected.
;; Batch runs (the ERT harness) deliberately inhibit backend startup, and a
;; timer spawning `bash' underneath a test run would be exactly
;; the external-process leak the harness exists to prevent.
(unless (and (fboundp 'agent-repl--frontend-init-inhibited-p)
             (agent-repl--frontend-init-inhibited-p))
  (agent-repl--readiness-start-timer))

(provide 'agent-repl-readiness)
;;; readiness.el ends here
