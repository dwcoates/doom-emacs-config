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
;;   emacs-<pid>.cpu.elprof, emacs-<pid>.mem.elprof
;;       loadable with `profiler-find-profile';
;;   emacs-<pid>.cpu.txt, emacs-<pid>.mem.txt
;;       the reversed call tree, hottest leaf functions first.
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
  "Start CPU and memory profiling into SESSION."
  (when (profiler-running-p)
    (error "The profiler is already running outside this session; stop it first"))
  (make-directory (expand-file-name session (agent-repl-profiling-root)) t)
  (profiler-reset)
  (when (fboundp 'profiler-cpu-start)
    (profiler-cpu-start profiler-sampling-interval))
  (profiler-memory-start)
  (setq agent-repl-profiling--session session
        agent-repl-profiling--generation (1+ agent-repl-profiling--generation))
  (let ((armed agent-repl-profiling--generation))
    (setq agent-repl-profiling--timer
          (run-at-time agent-repl-profiling-max-seconds nil
                       #'agent-repl-profiling--expire armed)))
  (agent-repl--info nil "profiling: started into session %s" session)
  (format "started %s" session))

(defun agent-repl-profiling--expire (armed)
  "Stop the profile of generation ARMED at its max duration."
  (when (and agent-repl-profiling--session
             (= armed agent-repl-profiling--generation))
    (agent-repl-profiling--stop "max_duration")))

(defun agent-repl-profiling--write-report (profile file)
  "Write PROFILE's reversed call tree as text to FILE."
  (let ((buf (profiler-report-setup-buffer-1 profile)))
    (unwind-protect
        (with-current-buffer buf
          (profiler-report-render-reversed-calltree)
          (write-region (point-min) (point-max) file nil 'silent))
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
    (when (and (fboundp 'profiler-cpu-running-p) (profiler-cpu-running-p))
      (setq profiler-cpu-log (profiler-cpu-log)))
    (when (profiler-memory-running-p)
      (setq profiler-memory-log (profiler-memory-log)))
    (when (fboundp 'profiler-cpu-stop) (profiler-cpu-stop))
    (profiler-memory-stop)
    (dolist (spec (list (list "cpu" profiler-cpu-log #'profiler-cpu-profile)
                        (list "mem" profiler-memory-log #'profiler-memory-profile)))
      (when (nth 1 spec)
        (let ((profile (funcall (nth 2 spec)))
              (data (expand-file-name (format "%s.%s.elprof" base (car spec)) dir))
              (text (expand-file-name (format "%s.%s.txt" base (car spec)) dir)))
          (with-temp-buffer
            (let (print-level print-length)
              (print (profiler-fixup-profile profile) (current-buffer)))
            (write-region (point-min) (point-max) data nil 'silent))
          (agent-repl-profiling--write-report profile text)
          (push data files)
          (push text files))))
    (setq files (nreverse files))
    (agent-repl--info nil "profiling: stopped session %s (%s), wrote %s"
                      session reason (string-join files ", "))
    (format "stopped %s %s" session (string-join files " "))))

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
       (agent-repl--error nil "profiling: the reconcile failed: %s" why)
       (concat "failed: " why)))))

(provide 'agent-repl-profiling)
;;; profiling.el ends here
