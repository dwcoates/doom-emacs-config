;;; profiling.el --- Runtime CPU and memory profiling for the editor -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs's corner of the runtime profiling session every agent-repl runtime
;; answers identically (proto/vocab/profile-session.json).
;; bin/profile-agent-repl.sh opens a session by writing its id into
;; `<root>/active' and calls `agent-repl-profiling-reconcile' through
;; emacsclient (Emacs is called, where the Go runtimes and the shim are
;; signaled); closing the session and calling again stops the profile and
;; writes into `<root>/<id>/':
;;
;;   emacs-<pid>.mem.elprof
;;       loadable with `profiler-find-profile';
;;   emacs-<pid>.mem.txt
;;       the reversed call tree, hottest leaf functions first.
;;
;; NO CPU PROFILE IS TAKEN IN HERE.  Emacs's CPU profiler is driven by
;; SIGPROF, and a signal the kernel delivers to one of WebKit's threads is
;; forwarded by Emacs to its main thread with `pthread_kill'; when that
;; thread held a libpas lock, the forwarding took it again and aborted the
;; process (2026-10-25: the editor crashed, "Trying to recursively lock an
;; os_unfair_lock" on JavaScriptCore's scavenger thread).  Every agent-repl
;; Emacs hosts WebKit, so the collector samples Emacs's CPU from outside
;; with macOS `sample' instead (bin/profile-agent-repl.sh,
;; emacs-<pid>.cpu.sample).
;;
;;   webapp-<workspace>.lag.json
;;       every live page's main-thread stalls over the session: a
;;       timer-drift sampler the reconcile injects through the webview read
;;       channel (frontend.el), so the page needs no profiling code of its
;;       own and no wire change.  WebKit runs no JS CPU profiler without its
;;       inspector, and a HIDDEN webview's timers are suspended by the
;;       embedder, so the sampler measures the pages that were on screen;
;;       the WebKit processes' CPU and memory come from the collector's
;;       sampler.
;;
;; A profile stops by itself `agent-repl-profiling-max-seconds' after it
;; started.  OFF COSTS NOTHING: no timer, no hook, nothing sampled.  A
;; profiler the user started by hand is never clobbered; the reconcile fails
;; loudly instead.

;;; Code:

(require 'subr-x)
(require 'profiler)

(declare-function agent-repl--info "core" (ws fmt &rest args))
(declare-function agent-repl--error "core" (ws fmt &rest args))
(declare-function agent-repl--ws-get "workspace" (ws key))
(declare-function agent-repl--set-log-level-window "core" (level))
(declare-function agent-repl-webhost-set-log-level "webhost" (level until-ms))
(declare-function agent-repl--frontend-webview-read-script "frontend" (buf script callback))
(declare-function agent-repl-webhost-buffer "webhost" ())
(defvar agent-repl--global-log-scope)

(defconst agent-repl-profiling-root-env "AGENT_REPL_PROFILE_ROOT"
  "Environment variable overriding the profiles root.")

(defconst agent-repl-profiling-default-root ".cache/agent-repl/profiles"
  "The profiles root under the user's home.")

(defconst agent-repl-profiling-active-file "active"
  "The file under the root that names the open session.")

(defconst agent-repl-profiling-armed-dir "armed"
  "The directory under the root holding the signaled runtimes' markers.
Emacs is called rather than signaled, so it writes no marker of its own.")

(defconst agent-repl-profiling-max-seconds 1800
  "How long a profile runs before Emacs stops it by itself.")

(defvar agent-repl-profiling--session nil
  "The session being profiled into, nil when none is.")

(defvar agent-repl-profiling--timer nil
  "The max-duration stop of the running profile.")

(defconst agent-repl-profiling-log-level-file "log-level"
  "The file in a session's directory naming a log level every runtime raises
itself to while it profiles into the session (proto/vocab/profile-session.json).")

(defvar agent-repl-profiling--level-open nil
  "Non-nil while this profile's session holds Emacs and the webhost at a raised
log level, which the profile's stop closes.")

(defvar agent-repl-profiling--generation 0
  "Counts the profiles started, so an old max-duration stop ends no later one.")

(defun agent-repl-profiling-valid-session-p (id)
  "Non-nil when ID names a session Emacs may write into."
  (and (stringp id)
       (let ((case-fold-search nil))
         (string-match-p "\\`[A-Za-z0-9][A-Za-z0-9._-]\\{0,62\\}\\'" id))))

(defun agent-repl-profiling-root ()
  "The profiles root: the override when set, else under the home directory."
  (let ((override (getenv agent-repl-profiling-root-env)))
    (if (and override (not (string-empty-p override)))
        override
      (expand-file-name agent-repl-profiling-default-root "~"))))

(defun agent-repl-profiling--active-session ()
  "The session the active file names, nil when there is no active file.
Signals an error when the file names no valid session."
  (let ((file (expand-file-name agent-repl-profiling-active-file (agent-repl-profiling-root))))
    (when (file-exists-p file)
      (let ((id (string-trim (with-temp-buffer
                               (insert-file-contents file)
                               (buffer-string)))))
        (unless (agent-repl-profiling-valid-session-p id)
          (error "The active file names no valid session: %S" id))
        id))))

(defun agent-repl-profiling--start (session)
  "Start memory profiling into SESSION; never the SIGPROF CPU profiler."
  (when (profiler-running-p)
    (error "The profiler is already running outside this session; stop it first"))
  (make-directory (expand-file-name session (agent-repl-profiling-root)) t)
  (profiler-reset)
  (profiler-memory-start)
  (setq agent-repl-profiling--session session
        agent-repl-profiling--generation (1+ agent-repl-profiling--generation))
  (let ((armed agent-repl-profiling--generation))
    (setq agent-repl-profiling--timer
          (run-at-time agent-repl-profiling-max-seconds nil
                       #'agent-repl-profiling--expire armed)))
  (let ((pages (agent-repl-profiling--pages 'start session))
        (level (agent-repl-profiling--open-level session)))
    (agent-repl--info agent-repl--global-log-scope "profiling: started into session %s (%d pages sampled)" session pages)
    (format "started %s pages=%d%s" session pages (if level (format " level=%s" level) ""))))

(defun agent-repl-profiling--open-level (session)
  "Raise Emacs, the webhost and every page to the level SESSION names.
Answers the level, or nil when the session names none.  An unreadable or
unknown level is recorded at error and leaves every level as it was; the
profile still runs."
  (let ((file (expand-file-name agent-repl-profiling-log-level-file
                                (expand-file-name session (agent-repl-profiling-root)))))
    (when (file-exists-p file)
      (let* ((name (string-trim (with-temp-buffer (insert-file-contents file) (buffer-string))))
             (level (intern name)))
        (if (not (memq level '(debug warn error)))
            (progn
              (agent-repl--error agent-repl--global-log-scope
                                 "profiling: the session %s names log level %S, not one of debug warn error; levels left as they are"
                                 session name)
              nil)
          (let ((until (agent-repl--set-log-level-window level)))
            (setq agent-repl-profiling--level-open t)
            (agent-repl-webhost-set-log-level level (round (* 1000 until)))
            (agent-repl--info agent-repl--global-log-scope
                              "profiling: raised Emacs, the webhost and every page to %s for session %s until %s"
                              level session (format-time-string "%FT%T%z" (seconds-to-time until)))
            level))))))

(defun agent-repl-profiling--close-level (session)
  "Return Emacs, the webhost and every page to info after SESSION's window."
  (when agent-repl-profiling--level-open
    (setq agent-repl-profiling--level-open nil)
    (agent-repl--set-log-level-window 'info)
    (agent-repl-webhost-set-log-level 'info nil)
    (agent-repl--info agent-repl--global-log-scope
                      "profiling: closed session %s's log level window; Emacs, the webhost and every page are back at info"
                      session)))

(defun agent-repl-profiling--expire (armed)
  "Stop the profile of generation ARMED at its max duration."
  (when (and agent-repl-profiling--session
             (= armed agent-repl-profiling--generation))
    (agent-repl-profiling--stop "max_duration")))

(defun agent-repl-profiling--write-report (profile file)
  "Write PROFILE's reversed call tree as text to FILE."
  (let ((buf (profiler-report-setup-buffer-1 profile)))
    (unwind-protect
        (let ((text (with-current-buffer buf
                      (profiler-report-render-reversed-calltree)
                      (buffer-string))))
          (with-temp-file file (insert text)))
      (kill-buffer buf))))

(defun agent-repl-profiling--stop (reason)
  "Stop the running profile for REASON and write its files.
Answers the files written."
  (let* ((session agent-repl-profiling--session)
         (dir (expand-file-name session (agent-repl-profiling-root)))
         (base (format "emacs-%d" (emacs-pid)))
         (files nil))
    (when (timerp agent-repl-profiling--timer)
      (cancel-timer agent-repl-profiling--timer))
    (setq agent-repl-profiling--session nil
          agent-repl-profiling--timer nil)
    (when (profiler-memory-running-p)
      (setq profiler-memory-log (profiler-memory-log)))
    (profiler-memory-stop)
    (dolist (spec (list (list "mem" profiler-memory-log #'profiler-memory-profile)))
      (when (nth 1 spec)
        (let ((profile (funcall (nth 2 spec)))
              (data (expand-file-name (format "%s.%s.elprof" base (car spec)) dir))
              (text (expand-file-name (format "%s.%s.txt" base (car spec)) dir)))
          (with-temp-file data
            (let (print-level print-length)
              (print (profiler-fixup-profile profile) (current-buffer))))
          (agent-repl-profiling--write-report profile text)
          (push data files)
          (push text files))))
    (setq files (nreverse files))
    (agent-repl-profiling--close-level session)
    (let ((pages (agent-repl-profiling--pages 'stop session)))
      (agent-repl--info agent-repl--global-log-scope "profiling: stopped session %s (%s), wrote %s; %d pages answering"
                        session reason (string-join files ", ") pages)
      (format "stopped %s pages=%d %s" session pages (string-join files " ")))))

(defconst agent-repl-profiling--page-start-script
  (concat "(function(){var p=window.__agentReplLag;if(p&&p.timer){return JSON.stringify({ok:false,why:'already sampling'});}"
          "p=window.__agentReplLag={stalls:[],ticks:0,began:performance.now(),last:performance.now()};"
          "p.timer=setInterval(function(){var now=performance.now(),late=now-p.last-50;p.ticks++;"
          "if(late>=16){p.stalls.push([Math.round(p.last-p.began),Math.round(late)]);}p.last=now;},50);"
          "return JSON.stringify({ok:true});})()")
  "Starts the page's main-thread stall sampler: a 50ms timer whose lateness
past one frame (16ms) is a stall, recorded as [ms since start, ms late].")

(defun agent-repl-profiling--page-stop-script (session ws)
  "The script stopping the page sampler, answering its stalls.
The answer names SESSION and WS, so the callback needs no closure."
  (format (concat "(function(){var p=window.__agentReplLag;if(!p||!p.timer){"
                  "return JSON.stringify({session:%S,ws:%S,ok:false,why:'no sampler'});}"
                  "clearInterval(p.timer);p.timer=null;var total=0,longest=0;"
                  "p.stalls.forEach(function(s){total+=s[1];if(s[1]>longest)longest=s[1];});"
                  "return JSON.stringify({session:%S,ws:%S,ok:true,interval_ms:50,ticks:p.ticks,"
                  "wall_ms:Math.round(performance.now()-p.began),stalled_ms:total,longest_ms:longest,"
                  "stalls:p.stalls});})()")
          session ws session ws))

(defun agent-repl-profiling--page-buffers ()
  "Answers (NAME . BUFFER) for the web view whose main thread is sampled.
Every workspace's page lives in the one host web view (webhost.el) and
shares its main thread, so the host is sampled once, under the name
\"host\"; nil when there is no host."
  (when-let ((buf (agent-repl-webhost-buffer)))
    (list (cons "host" buf))))

(defun agent-repl-profiling--page-started (raw)
  "Records a page sampler's start answer RAW; a refusal is an error."
  (let ((answer (ignore-errors (json-parse-string raw :object-type 'alist))))
    (unless (eq (alist-get 'ok answer) t)
      (agent-repl--error agent-repl--global-log-scope "profiling: a page sampler did not start: %s" raw))))

(defun agent-repl-profiling--page-file-name (ws)
  "The lag file name for workspace WS, safe as one path component."
  (format "webapp-%s.lag.json" (replace-regexp-in-string "[^A-Za-z0-9._-]" "_" ws)))

(defun agent-repl-profiling--page-stopped (raw)
  "Writes a page sampler's stop answer RAW into its session directory."
  (let* ((answer (ignore-errors (json-parse-string raw :object-type 'alist)))
         (session (alist-get 'session answer))
         (ws (alist-get 'ws answer)))
    (if (not (and (stringp session) (agent-repl-profiling-valid-session-p session) (stringp ws)))
        (agent-repl--error agent-repl--global-log-scope "profiling: a page sampler answered no session: %s" raw)
      (let ((file (expand-file-name (agent-repl-profiling--page-file-name ws)
                                    (expand-file-name session (agent-repl-profiling-root)))))
        (with-temp-file file (insert raw))
        (unless (eq (alist-get 'ok answer) t)
          (agent-repl--error agent-repl--global-log-scope "profiling: the page sampler of %s answered %s" ws raw))))))

(defun agent-repl-profiling--pages (action session)
  "Starts (ACTION `start') or stops (`stop') every live page's sampler for SESSION.
Answers how many pages were reached."
  (let ((reached 0))
    (dolist (entry (agent-repl-profiling--page-buffers))
      (when (agent-repl--frontend-webview-read-script
             (cdr entry)
             (if (eq action 'start)
                 agent-repl-profiling--page-start-script
               (agent-repl-profiling--page-stop-script session (car entry)))
             (if (eq action 'start)
                 'agent-repl-profiling--page-started
               'agent-repl-profiling--page-stopped))
        (setq reached (1+ reached))))
    reached))

(defun agent-repl-profiling-reconcile ()
  "Make Emacs's profiling match the profiles root's active file.
A valid session named there is profiled into; no active file stops any
profile.  Answers what it did as a string for the collector:
\"started ID\", \"stopped ID FILES...\", \"unchanged\", or \"failed: WHY\"
(also recorded at error)."
  (interactive)
  (condition-case err
      (let ((want (agent-repl-profiling--active-session)))
        (cond
         ((equal want agent-repl-profiling--session) "unchanged")
         (t
          (let ((stopped (when agent-repl-profiling--session
                           (agent-repl-profiling--stop
                            (if want "session_changed" "session_closed")))))
            (if want
                (let ((started (agent-repl-profiling--start want)))
                  (if stopped (concat stopped "; " started) started))
              stopped)))))
    (error
     (let ((why (error-message-string err)))
       (agent-repl--error agent-repl--global-log-scope "profiling: the reconcile failed: %s" why)
       (concat "failed: " why)))))

;;;; ---- the switch tour ---------------------------------------------------

;; A MEASURED RUN OF REAL SWITCHES.  The tour queues the `s-}' key COUNT times
;; and then `s-{' COUNT times, one key per step, so it ends on the workspace it
;; started from.  Each key goes into `unread-command-events', so the command
;; loop reads it exactly as it reads a keystroke: the switch command, its
;; post-command hooks and its redisplay all run as they do for the user.  A
;; command called from the timer instead would skip that loop, which is part
;; of what a switch costs.  The steps are spaced as a person pauses between
;; switches; the measurement is the log's own edges (bin/profile-agent-repl.sh
;; report), never this timer.

(defconst agent-repl-profiling-switch-tour-interval 1.5
  "Seconds between two keys of a switch tour that names no pace of its own.")

(defvar agent-repl-profiling--tour nil
  "The running switch tour, or nil: a plist of the keys still to queue
\(:keys), how many it was asked for each way (:count), the seconds between
keys (:interval), its label (:label), and its pending step (:timer).")

(defun agent-repl-profiling--tour-refuse (fmt &rest args)
  "Record a refused switch tour at error, and signal it.  FMT and ARGS word it."
  (let ((why (apply #'format fmt args)))
    (agent-repl--error agent-repl--global-log-scope "profiling.switch-tour-refused: %s" why)
    (error "Switch tour refused: %s" why)))

(defun agent-repl-profiling-switch-tour (count &optional interval label)
  "Switch right COUNT times with `s-}', then back COUNT times with `s-{'.
Each key is queued as a keystroke INTERVAL seconds after the one before
\(`agent-repl-profiling-switch-tour-interval' when nil), and the tour ends on
the workspace it began on.  LABEL (default \"tour\") names the tour in every
record it writes, so a report can tell two tours of one session apart.
Answers at once, while the tour runs on its own timer."
  (interactive "nSwitches each way: ")
  (setq interval (or interval agent-repl-profiling-switch-tour-interval)
        label (or label "tour"))
  (unless (and (integerp count) (> count 0))
    (agent-repl-profiling--tour-refuse "the count %S is not a positive integer" count))
  (unless (and (numberp interval) (> interval 0))
    (agent-repl-profiling--tour-refuse "the interval %S is not a positive number of seconds" interval))
  (unless (and (stringp label) (string-match-p "\\`[A-Za-z0-9._-]+\\'" label))
    (agent-repl-profiling--tour-refuse "the label %S is not one word of letters, digits, dots, dashes or underscores" label))
  (when agent-repl-profiling--tour
    (agent-repl-profiling--tour-refuse "a tour is already running, %d keys left"
                                       (length (plist-get agent-repl-profiling--tour :keys))))
  (setq agent-repl-profiling--tour
        (list :keys (append (make-list count "s-}") (make-list count "s-{"))
              :count count
              :interval interval
              :label label
              :timer nil))
  (agent-repl--info agent-repl--global-log-scope
                    "profiling.switch-tour-started label=%s count=%d interval=%s"
                    label count interval)
  (agent-repl-profiling--tour-step)
  (format "touring %d switches" (* 2 count)))

(defun agent-repl-profiling--tour-step ()
  "Queue the running tour's next key and schedule the step after it.
With no key left, the tour ends."
  (let ((keys (plist-get agent-repl-profiling--tour :keys)))
    (if (null keys)
        (let ((count (plist-get agent-repl-profiling--tour :count))
              (label (plist-get agent-repl-profiling--tour :label)))
          (setq agent-repl-profiling--tour nil)
          (agent-repl--info agent-repl--global-log-scope
                            "profiling.switch-tour-done label=%s switches=%d" label (* 2 count)))
      (setq unread-command-events
            (append unread-command-events (listify-key-sequence (kbd (car keys)))))
      (agent-repl--info agent-repl--global-log-scope
                        "profiling.switch-tour-key label=%s key=%s remaining=%d"
                        (plist-get agent-repl-profiling--tour :label) (car keys) (length (cdr keys)))
      (setq agent-repl-profiling--tour
            (plist-put (plist-put agent-repl-profiling--tour :keys (cdr keys))
                       :timer (run-at-time (plist-get agent-repl-profiling--tour :interval) nil
                                           #'agent-repl-profiling--tour-step))))))

(defun agent-repl-profiling-switch-tour-state ()
  "Answers the running tour as \"running LABEL KEYS-LEFT\", or \"idle\".
What a driver outside Emacs waits on (bin/profile-agent-repl.sh switch-bench)."
  (if agent-repl-profiling--tour
      (format "running %s %d" (plist-get agent-repl-profiling--tour :label)
              (length (plist-get agent-repl-profiling--tour :keys)))
    "idle"))

(defun agent-repl-profiling-switch-tour-cancel ()
  "Stop the running switch tour before its next key; answers what it did."
  (interactive)
  (if (null agent-repl-profiling--tour)
      "no tour running"
    (let ((left (length (plist-get agent-repl-profiling--tour :keys)))
          (timer (plist-get agent-repl-profiling--tour :timer)))
      (when (timerp timer) (cancel-timer timer))
      (setq agent-repl-profiling--tour nil)
      (agent-repl--info agent-repl--global-log-scope
                        "profiling.switch-tour-cancelled keys-left=%d" left)
      (format "cancelled with %d keys left" left))))

(provide 'agent-repl-profiling)
;;; profiling.el ends here
